module Network.TCP.TCPServer
import Effects
import Network.TCP.TCPCommon

%access public
%link C "idris_net.o"
%include C "idris_net.h"

-- Raw acceptor socket info
--RawConnInfo : Type
--RawConnInfo = Ptr

data TCPServerStep = StateAllocated
                   | Listening

Client : Type
Client = Ptr

-- TODO: Parameterise resource with list of clients
-- Ideally, we want something like...
-- send : TCPSERVER (TCPServerRes Listening Clients) -> (c : RawConnInfo) -> (prf: Elem c Clients) -> TcpServer ...
data TCPServerRes : TCPServerStep -> Type where
  TCPServerStateAllocated : RawConnInfo -> TCPServerRes t
  TCPServerError : (Maybe RawConnInfo) -> TCPServerRes t

-- Not sure whether this is the best way to do this, but at the moment
-- we need to encapsulate that an error may have been thrown by the current
-- operation, or a prior one, and we may not have detailed information about
-- the previous failures. An example would be if we couldn't allocate a raw
-- connection state for the acceptor socket, and then attempt to listen.
-- We would, in this case encapsulate this with a SocketFailure Nothing.
data SocketOperationRes a = SocketSuccess a
                          | SocketFailure (Maybe ErrorCode)

data TcpServer : Effect where
  -- Allocation and freeing socket acceptor info
  -- TODO: Change Bool to something more informative
  AllocateState : TcpServer () (TCPServerRes StateAllocated) Bool
  FreeState : TcpServer (TCPServerRes StateAllocated) () ()
  -- Listen for connections
  Listen : (Maybe IPAddr) -> Port -> TcpServer (TCPServerRes StateAllocated) (TCPServerRes Listening) (SocketOperationRes ())
  -- Closes listening socket
  StopListening : TcpServer (TCPServerRes Listening) (TCPServerRes StateAllocated) ()
  -- Accepts a new connection
  -- Maybe we need extra states to prove that the result structure's been deallocated? Or maybe not.
  Accept : TcpServer (TCPServerRes Listening) (TCPServerRes Listening) (SocketOperationRes Client)
  -- Send (see note above for verification idea)
  -- Also, seems restrictive to have to be listening to send to clients, this might be something to look at
  SendData : Client -> String -> TcpServer (TCPServerRes Listening) (TCPServerRes Listening) (SocketOperationRes ())
  RecvData : Client -> TcpServer (TCPServerRes Listening) (TCPServerRes Listening) (SocketOperationRes String)


TCPSERVER : Type -> EFFECT
TCPSERVER t = MkEff t TcpServer

private 
allocateState : EffM IO [TCPSERVER ()] [TCPSERVER (TCPServerRes StateAllocated)] Bool
allocateState = AllocateState

private
freeState : EffM IO [TCPSERVER (TCPServerRes StateAllocated)] [TCPSERVER ()] ()
freeState = FreeState

sendData : Client -> String -> Eff IO [TCPSERVER (TCPServerRes Listening)] (SocketOperationRes ())
sendData c dat = (SendData c dat)

recvData : Client -> Eff IO [TCPSERVER (TCPServerRes Listening)] (SocketOperationRes String)
recvData c = (RecvData c)

private
stopListening' : EffM IO [TCPSERVER (TCPServerRes Listening)] [TCPSERVER (TCPServerRes StateAllocated)] ()
stopListening' = StopListening

listen' : Maybe IPAddr -> Port -> EffM IO [TCPSERVER (TCPServerRes StateAllocated)] [TCPSERVER (TCPServerRes Listening)] (SocketOperationRes ())
listen' m_ip p = (Listen m_ip p)

listen : Maybe IPAddr -> Port -> EffM IO [TCPSERVER ()] [TCPSERVER (TCPServerRes Listening)] (SocketOperationRes ())
listen m_ip p = do
  alloc_res <- allocateState
  listen' m_ip p

stopListening : EffM IO [TCPSERVER (TCPServerRes Listening)] [TCPSERVER ()] ()
stopListening = do
  stopListening'
  freeState

accept : Eff IO [TCPSERVER (TCPServerRes Listening)] (SocketOperationRes Client)
accept = Accept

instance Handler TcpServer IO where
  -- Allocation
  handle () (AllocateState) k = do
    ptr <- mkForeign (FFun "idrnet_allocate_conn_info" [] FPtr) 
    k (TCPServerStateAllocated (RawConn ptr)) True
  -- Deallocation
  handle (TCPServerError (Just (RawConn ptr))) (FreeState) k = do
    mkForeign (FFun "idrnet_deallocate_conn_info" [FPtr] FUnit) ptr
    k () ()

  handle (TCPServerError Nothing) (FreeState) k = k () ()

  handle (TCPServerStateAllocated (RawConn ptr)) (FreeState) k = do
    mkForeign (FFun "idrnet_deallocate_conn_info" [FPtr] FUnit) ptr
    k () ()

  -- Listening
  handle (TCPServerStateAllocated (RawConn ptr)) (Listen m_ip port) k = do
    let ip = maybe "" show m_ip 
    res <- mkForeign (FFun "idrnet_listen" [FPtr, FString, FString] FInt) ptr ip (show port)
    if (res /= 0) then do -- Error occurred
      err <- foreignGetLastError (RawConn ptr)
      k (TCPServerError (Just (RawConn ptr))) (SocketFailure (Just err))
    else k (TCPServerStateAllocated (RawConn ptr)) (SocketSuccess ())

  handle (TCPServerError a) (Listen _ _) k = k (TCPServerError a) (SocketFailure Nothing)

  -- Sending Data
  --sendData : Client -> String -> Eff IO [TCPSERVER (TCPServerRes Listening)] (SocketOperationRes ())
  handle (TCPServerStateAllocated (RawConn ptr)) (SendData c s) k = do
    res <- mkForeign (FFun "idrnet_send" [FPtr, FString] FInt) c s
    if (res /= 0) then do
      err <- foreignGetLastError (RawConn c) -- TODO: When it's not 1AM on Sunday, this should be a different type
      -- TODO: We don't want to invalidate the whole server state because of a client messing up,
      -- but ideally we'd have some way of recording that the client was in an erroneous state
      k (TCPServerStateAllocated (RawConn ptr)) (SocketFailure (Just err))
    else k (TCPServerStateAllocated (RawConn ptr)) (SocketSuccess ())

  handle (TCPServerError a) (SendData _ _) k = k (TCPServerError a) (SocketFailure Nothing)

  -- Receiving Data
  handle (TCPServerStateAllocated (RawConn ptr)) (RecvData c) k = do
    res <- mkForeign (FFun "idrnet_recv" [FPtr] FInt) c
    if (res <= 0) then do
      err <- foreignGetLastError (RawConn c)
      k (TCPServerStateAllocated (RawConn ptr)) (SocketFailure (Just err))
    else do
      recv_dat <- mkForeign (FFun "idrnet_get_fetched_data" [FPtr] FString) c
      k (TCPServerStateAllocated (RawConn ptr)) (SocketSuccess recv_dat)

  handle (TCPServerError a) (RecvData _) k = k (TCPServerError a) (SocketFailure Nothing)

  -- Accepting... Here goes...
  handle (TCPServerStateAllocated (RawConn ptr)) Accept k = do
    acc_res_struct <- mkForeign (FFun "idrnet_accept" [FPtr] FPtr) ptr
    acc_res <- mkForeign (FFun "idrnet_get_accept_res" [FPtr] FInt) ptr
    if (acc_res /= 0) then do
      acc_err <- mkForeign (FFun "idrnet_get_accept_err" [FPtr] FInt) ptr
      mkForeign (FFun "idrnet_free_accept_struct" [FPtr] FUnit) acc_res_struct
      k (TCPServerError (Just (RawConn ptr))) (SocketFailure (Just acc_err))
    else do
      acc_client <- mkForeign (FFun "idrnet_get_accept_client" [FPtr] FPtr) acc_res_struct
      mkForeign (FFun "idrnet_free_accept_struct" [FPtr] FUnit) acc_res_struct
      k (TCPServerStateAllocated (RawConn ptr)) (SocketSuccess acc_client)

  handle (TCPServerError a) Accept k = k (TCPServerError a) (SocketFailure Nothing)

  -- Stopping listening
  handle (TCPServerStateAllocated (RawConn ptr)) StopListening k = do
    res <- mkForeign (FFun "idrnet_close" [FPtr] FInt) ptr
    k (TCPServerStateAllocated (RawConn ptr)) ()

  handle (TCPServerError m_ptr) StopListening k = 
    k (TCPServerError m_ptr) ()
