module Network.TCP.TCP
import Effects

-- D'OH
%access public
%link C "idris_net.o"
%include C "idris_net.h"

-- Raw connection info pointer within the C library. We don't use this directly
-- in the Idris code, but it is a parameter in all stateful operations.
RawConnInfo : Type
RawConnInfo = Ptr

-- C Error Code
ErrorCode : Type
ErrorCode = Int

Port : Type
Port = Int

BytesSent : Type
BytesSent = Int

-- TODO: Validation
-- Also, no IPv6 support yet...
data IPAddr = IPv4Addr Int Int Int Int
            | IPv6Addr 

instance Show IPAddr where
  show (IPv4Addr i1 i2 i3 i4) = concat $ intersperse "." $ map show [i1, i2, i3, i4]

data TCPStep = StateAllocated
             | ConnectAttempted
             | NotConnected

data TCPRes : TCPStep -> Type where
  TCPConnected : RawConnInfo -> TCPRes a
  TCPStateAllocated : RawConnInfo -> TCPRes a
  TCPError : Maybe ErrorCode -> Maybe RawConnInfo -> TCPRes a

-- BIG TODO: Verification on things like packet structures, IP addresses and such.
-- Hacky for now!
-- Bools are there because we can do if..then's over them, but more detailed info can be retrieved from the resource.
--  ...though I might change this...
data Tcp : Effect where
  -- Attempts to connect to a given server
  Connect : IPAddr -> Port -> Tcp (TCPRes StateAllocated) (TCPRes ConnectAttempted) (Maybe ErrorCode)
  -- Disconnects from the current server
  Disconnect : Tcp (TCPRes ConnectAttempted) (TCPRes NotConnected) ()
  -- Attempts to send data to the current server
  SendData : String -> Tcp (TCPRes ConnectAttempted) (TCPRes ConnectAttempted) (Either (Maybe ErrorCode) BytesSent)
  -- Receives data from the server
  RecvData : Tcp (TCPRes ConnectAttempted) (TCPRes ConnectAttempted) (Either (Maybe ErrorCode) String)
  -- Gets the last error thrown by the server, should one exist
  GetLastError : Tcp (TCPRes ConnectAttempted) (TCPRes ConnectAttempted) (Maybe ErrorCode)
  -- Allocates the C-side connection state. Called internally as part of the Connect call.
  AllocateState : Tcp () (TCPRes StateAllocated) Bool
  -- Frees the state. Must be called at the end, in order to prevent memory leaks.
  FreeState : Tcp (TCPRes NotConnected) () ()

TCP : Type -> EFFECT
TCP t = MkEff t Tcp

private
connect' : IPAddr -> Port -> EffM IO [TCP (TCPRes StateAllocated)] [TCP (TCPRes ConnectAttempted)] (Maybe ErrorCode)
connect' ip port = (Connect ip port)

private
disconnect' : EffM IO [TCP (TCPRes ConnectAttempted)] [TCP (TCPRes NotConnected)] ()
disconnect' = Disconnect

private 
allocateState : EffM IO [TCP ()] [TCP (TCPRes StateAllocated)] Bool
allocateState = AllocateState

freeState : EffM IO [TCP (TCPRes NotConnected)] [TCP ()] ()
freeState = FreeState

eitherSuccess : Either a b -> Bool
eitherSuccess (Left _) = False
eitherSuccess _ = True

maybeSuccess : Maybe a -> Bool
maybeSuccess (Just _) = True
maybeSuccess _ = False

connect : IPAddr -> Port -> EffM IO [TCP ()] [TCP (TCPRes ConnectAttempted)] (Maybe ErrorCode)
connect ip port = do alloc_res <- allocateState
                     if alloc_res then do
                       connect_res <- connect' ip port
                       return connect_res
                     else do -- Dubious. Will be easily sorted in IOExcept
                       connect' ip port
                       return Nothing

disconnect : EffM IO [TCP (TCPRes ConnectAttempted)] [TCP ()] ()
disconnect = do disconnect'
                freeState

sendData : String -> Eff IO [TCP (TCPRes ConnectAttempted)] (Either (Maybe ErrorCode) BytesSent)
sendData dat = (SendData dat)

recvData : Eff IO [TCP (TCPRes ConnectAttempted)] (Either (Maybe ErrorCode) String)
recvData = RecvData

getLastError : Eff IO [TCP (TCPRes ConnectAttempted)] (Maybe ErrorCode)
getLastError = GetLastError

foreignGetLastError : RawConnInfo -> IO Int
foreignGetLastError ptr = mkForeign (FFun "idrnet_get_last_error" [FPtr] FInt) ptr

foreignFreeState : RawConnInfo -> IO ()
foreignFreeState ptr = mkForeign (FFun "idrnet_deallocate_conn_info" [FPtr] FUnit) ptr

  -- Connection handlers

instance Handler Tcp IO where
  handle (TCPStateAllocated ptr) (Connect ip port) k = do
    conn_res <- mkForeign (FFun "idrnet_connect" [FPtr, FString, FString] FInt) ptr (show ip) (show port)
    if (conn_res /= 0) then -- Error occurred C-side
      k (TCPError (Just conn_res) (Just ptr)) (Just conn_res) 
    else 
      k (TCPConnected ptr) Nothing
  -- If Connect is called with any other resource, it's a failure case.
  -- TODO: Increase granularity so the case Connect (TCPRes Connected) doesn't happen, by separating into a different resource
 -- handle (Connect _ _) (TCPError maybe_err maybe_ptr) k = k (TCPError maybe_err maybe_ptr) False
  --handle (Connect _ _) a k = k a False

  -- Disconnecting
  -- If we have a pointer to the connection state, use it.
  handle (TCPConnected ptr) (Disconnect) k = do
    conn_res <- mkForeign (FFun "idrnet_close" [FPtr] FInt) ptr
    k (TCPStateAllocated ptr) ()

  handle (TCPError err (Just ptr)) (Disconnect) k = do
    mkForeign (FFun "idrnet_close" [FPtr] FInt) ptr
    k (TCPError err (Just ptr)) ()

  handle (TCPError err Nothing) (Disconnect) k = 
    k (TCPError err Nothing) ()

  -- There *shouldn't* be a case where we're at TCPStateAllocated but not connected. 
  -- For this reason, just ignore this call, to avoid closing an uninitialised socket.
  handle (TCPStateAllocated ptr) (Disconnect) k = k (TCPError Nothing (Just ptr)) ()

  -- Sending Data
  handle (TCPConnected ptr) (SendData dat) k = do
    send_res <- mkForeign (FFun "idrnet_send" [FPtr, FString] FInt) ptr dat
    if (send_res == -1) then do -- Error occurred
      -- Get last error
      err <- foreignGetLastError ptr
      -- *very* unsure about this. It'd be nicer, if a write fails, to have an exception function trigger...
      -- TCP seems to be lending itself more to IOExcept than SQLite...
      k (TCPError (Just err) (Just ptr)) (Left (Just err))
    else k (TCPConnected ptr) (Right send_res)

  -- Pass through
  -- FIXME: This is hacky -- it returns -1, since we don't know whether we have an error stored or not
  handle (TCPError maybe_err maybe_ptr) (SendData dat) k = 
    k (TCPError maybe_err maybe_ptr) (Left maybe_err)

  handle (TCPStateAllocated ptr) (SendData dat) k = 
    k (TCPStateAllocated ptr) (Left Nothing)

  -- Receiving data
  handle (TCPConnected ptr) (RecvData) k = do
    recv_res <- mkForeign (FFun "idrnet_recv" [FPtr] FInt) ptr 
    if (recv_res > 0) then do -- 0: connection closed, < 0: error
      recv_data <- mkForeign (FFun "idrnet_get_fetched_data" [FPtr] FString) ptr
      k (TCPConnected ptr) (Right recv_data) 
    else do -- TODO: take into account 0
      last_error <- mkForeign (FFun "idrnet_get_last_error" [FPtr] FInt) ptr
      k (TCPError (Just last_error) (Just ptr)) (Left (Just last_error))

  handle (TCPError maybe_err maybe_ptr) (RecvData) k = 
    k (TCPError maybe_err maybe_ptr) (Left maybe_err)

  handle (TCPStateAllocated ptr) (RecvData) k = 
    k (TCPStateAllocated ptr) (Left Nothing)

  -- Retrieving last error
  handle (TCPConnected ptr) (GetLastError) k = do
    last_err <- foreignGetLastError ptr
    k (TCPConnected ptr) (Just last_err)
  
  handle (TCPStateAllocated ptr) (GetLastError) k = do
    last_err <- foreignGetLastError ptr
    k (TCPConnected ptr) (Just last_err)

  handle (TCPError m_err (Just ptr)) (GetLastError) k = do
    last_err <- foreignGetLastError ptr 
    k (TCPError (Just last_err) (Just ptr)) (Just last_err)

  handle (TCPError m_err Nothing) (GetLastError) k = k (TCPError (m_err) Nothing) Nothing

  -- Allocation of state... Only able to happen with no resource
  handle () (AllocateState) k = do
    ptr <- mkForeign (FFun "idrnet_allocate_conn_info" [] FPtr) 
    k (TCPStateAllocated ptr) True
  -- Once again, we need to increase granularity of resources so this isn't possible...
  handle (TCPConnected ptr) (FreeState) k = do
    foreignFreeState ptr
    k () ()
  handle (TCPStateAllocated ptr) (FreeState) k = do
    foreignFreeState ptr
    k () ()
  handle (TCPError m_err (Just ptr)) (FreeState) k = do
    foreignFreeState ptr
    k () ()
  handle (TCPError m_err Nothing) (FreeState) k = k () ()

