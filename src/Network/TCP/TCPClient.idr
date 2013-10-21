module Network.TCP.TCPClient
import Effects
import Network.TCP.TCPCommon

-- D'OH
%access public
%link C "idris_net.o"
%include C "idris_net.h"

data TCPClientStep = StateAllocated
             | ConnectAttempted
             | NotConnected

data TCPClientRes : TCPClientStep -> Type where
  TCPConnected : RawConnInfo -> TCPClientRes a
  TCPStateAllocated : RawConnInfo -> TCPClientRes a
  TCPError : Maybe ErrorCode -> Maybe RawConnInfo -> TCPClientRes a

-- BIG TODO: Verification on things like packet structures, IP addresses and such.
-- Hacky for now!
-- Bools are there because we can do if..then's over them, but more detailed info can be retrieved from the resource.
--  ...though I might change this...
data TcpClient : Effect where
  -- Attempts to connect to a given server
  Connect : IPAddr -> Port -> TcpClient (TCPClientRes StateAllocated) (TCPClientRes ConnectAttempted) (Maybe ErrorCode)
  -- Disconnects from the current server
  Disconnect : TcpClient (TCPClientRes ConnectAttempted) (TCPClientRes NotConnected) ()
  -- Attempts to send data to the current server
  SendData : String -> TcpClient (TCPClientRes ConnectAttempted) (TCPClientRes ConnectAttempted) (Either (Maybe ErrorCode) BytesSent)
  -- Receives data from the server
  RecvData : TcpClient (TCPClientRes ConnectAttempted) (TCPClientRes ConnectAttempted) (Either (Maybe ErrorCode) String)
  -- Gets the last error thrown by the server, should one exist
  GetLastError : TcpClient (TCPClientRes ConnectAttempted) (TCPClientRes ConnectAttempted) (Maybe ErrorCode)
  -- Allocates the C-side connection state. Called internally as part of the Connect call.
  AllocateState : TcpClient () (TCPClientRes StateAllocated) Bool
  -- Frees the state. Must be called at the end, in order to prevent memory leaks.
  FreeState : TcpClient (TCPClientRes NotConnected) () ()

TCPCLIENT: Type -> EFFECT
TCPCLIENT t = MkEff t TcpClient

private
connect' : IPAddr -> Port -> EffM IO [TCPCLIENT (TCPClientRes StateAllocated)] [TCPCLIENT (TCPClientRes ConnectAttempted)] (Maybe ErrorCode)
connect' ip port = (Connect ip port)

private
disconnect' : EffM IO [TCPCLIENT (TCPClientRes ConnectAttempted)] [TCPCLIENT (TCPClientRes NotConnected)] ()
disconnect' = Disconnect

private 
allocateState : EffM IO [TCPCLIENT ()] [TCPCLIENT (TCPClientRes StateAllocated)] Bool
allocateState = AllocateState

freeState : EffM IO [TCPCLIENT (TCPClientRes NotConnected)] [TCPCLIENT ()] ()
freeState = FreeState

eitherSuccess : Either a b -> Bool
eitherSuccess (Left _) = False
eitherSuccess _ = True

maybeSuccess : Maybe a -> Bool
maybeSuccess (Just _) = True
maybeSuccess _ = False

connect : IPAddr -> Port -> EffM IO [TCPCLIENT ()] [TCPCLIENT (TCPClientRes ConnectAttempted)] (Maybe ErrorCode)
connect ip port = do alloc_res <- allocateState
                     if alloc_res then do
                       connect_res <- connect' ip port
                       return connect_res
                     else do -- Dubious. Will be easily sorted in IOExcept
                       connect' ip port
                       return Nothing

disconnect : EffM IO [TCPCLIENT (TCPClientRes ConnectAttempted)] [TCPCLIENT ()] ()
disconnect = do disconnect'
                freeState

sendData : String -> Eff IO [TCPCLIENT (TCPClientRes ConnectAttempted)] (Either (Maybe ErrorCode) BytesSent)
sendData dat = (SendData dat)

recvData : Eff IO [TCPCLIENT (TCPClientRes ConnectAttempted)] (Either (Maybe ErrorCode) String)
recvData = RecvData

getLastError : Eff IO [TCPCLIENT (TCPClientRes ConnectAttempted)] (Maybe ErrorCode)
getLastError = GetLastError


foreignFreeState : RawConnInfo -> IO ()
foreignFreeState (RawConn ptr) = mkForeign (FFun "idrnet_deallocate_conn_info" [FPtr] FUnit) ptr

  -- Connection handlers

instance Handler TcpClient IO where
  handle (TCPStateAllocated (RawConn ptr)) (Connect ip port) k = do
    conn_res <- mkForeign (FFun "idrnet_connect" [FPtr, FString, FString] FInt) ptr (show ip) (show port)
    if (conn_res /= 0) then -- Error occurred C-side
      k (TCPError (Just conn_res) (Just (RawConn ptr))) (Just conn_res) 
    else 
      k (TCPConnected (RawConn ptr)) Nothing
  -- If Connect is called with any other resource, it's a failure case.
  -- TODO: Increase granularity so the case Connect (TCPClientRes Connected) doesn't happen, by separating into a different resource
 -- handle (Connect _ _) (TCPError maybe_err maybe_ptr) k = k (TCPError maybe_err maybe_ptr) False
  --handle (Connect _ _) a k = k a False

  -- Disconnecting
  -- If we have a pointer to the connection state, use it.
  handle (TCPConnected (RawConn ptr)) (Disconnect) k = do
    conn_res <- mkForeign (FFun "idrnet_close" [FPtr] FInt) ptr
    k (TCPStateAllocated (RawConn ptr)) ()

  handle (TCPError err (Just (RawConn ptr))) (Disconnect) k = do
    mkForeign (FFun "idrnet_close" [FPtr] FInt) ptr
    k (TCPError err (Just (RawConn ptr))) ()

  handle (TCPError err Nothing) (Disconnect) k = 
    k (TCPError err Nothing) ()

  -- There *shouldn't* be a case where we're at TCPStateAllocated but not connected. 
  -- For this reason, just ignore this call, to avoid closing an uninitialised socket.
  handle (TCPStateAllocated (RawConn ptr)) (Disconnect) k = k (TCPError Nothing (Just (RawConn ptr))) ()

  -- Sending Data
  handle (TCPConnected (RawConn ptr)) (SendData dat) k = do
    send_res <- mkForeign (FFun "idrnet_send" [FPtr, FString] FInt) ptr dat
    if (send_res == -1) then do -- Error occurred
      -- Get last error
      err <- foreignGetLastError (RawConn ptr)
      -- *very* unsure about this. It'd be nicer, if a write fails, to have an exception function trigger...
      -- TCPCLIENTseems to be lending itself more to IOExcept than SQLite...
      k (TCPError (Just err) (Just (RawConn ptr))) (Left (Just err))
    else k (TCPConnected (RawConn ptr)) (Right send_res)

  -- Pass through
  -- FIXME: This is hacky -- it returns -1, since we don't know whether we have an error stored or not
  handle (TCPError maybe_err maybe_ptr) (SendData dat) k = 
    k (TCPError maybe_err maybe_ptr) (Left maybe_err)

  handle (TCPStateAllocated (RawConn ptr)) (SendData dat) k = 
    k (TCPStateAllocated (RawConn ptr)) (Left Nothing)

  -- Receiving data
  handle (TCPConnected (RawConn ptr)) (RecvData) k = do
    recv_res <- mkForeign (FFun "idrnet_recv" [FPtr] FInt) ptr 
    if (recv_res > 0) then do -- 0: connection closed, < 0: error
      recv_data <- mkForeign (FFun "idrnet_get_fetched_data" [FPtr] FString) ptr
      k (TCPConnected (RawConn ptr)) (Right recv_data) 
    else do -- TODO: take into account 0
      last_error <- mkForeign (FFun "idrnet_get_last_error" [FPtr] FInt) ptr
      k (TCPError (Just last_error) (Just (RawConn ptr))) (Left (Just last_error))

  handle (TCPError maybe_err maybe_ptr) (RecvData) k = 
    k (TCPError maybe_err maybe_ptr) (Left maybe_err)

  handle (TCPStateAllocated (RawConn ptr)) (RecvData) k = 
    k (TCPStateAllocated (RawConn ptr)) (Left Nothing)

  -- Retrieving last error
  handle (TCPConnected (RawConn ptr)) (GetLastError) k = do
    last_err <- foreignGetLastError (RawConn ptr)
    k (TCPConnected (RawConn ptr)) (Just last_err)
  
  handle (TCPStateAllocated (RawConn ptr)) (GetLastError) k = do
    last_err <- foreignGetLastError (RawConn ptr)
    k (TCPConnected (RawConn ptr)) (Just last_err)

  handle (TCPError m_err (Just (RawConn ptr))) (GetLastError) k = do
    last_err <- foreignGetLastError (RawConn ptr) 
    k (TCPError (Just last_err) (Just (RawConn ptr))) (Just last_err)

  handle (TCPError m_err Nothing) (GetLastError) k = k (TCPError (m_err) Nothing) Nothing

  -- Allocation of state... Only able to happen with no resource
  handle () (AllocateState) k = do
    ptr <- mkForeign (FFun "idrnet_allocate_conn_info" [] FPtr) 
    k (TCPStateAllocated (RawConn ptr)) True
  -- Once again, we need to increase granularity of resources so this isn't possible...
  handle (TCPConnected (RawConn ptr)) (FreeState) k = do
    foreignFreeState (RawConn ptr)
    k () ()
  handle (TCPStateAllocated (RawConn ptr)) (FreeState) k = do
    foreignFreeState (RawConn ptr)
    k () ()
  handle (TCPError m_err (Just (RawConn ptr))) (FreeState) k = do
    foreignFreeState (RawConn ptr)
    k () ()
  handle (TCPError m_err Nothing) (FreeState) k = k () ()

