module Network.TCP 
import Effects

-- Raw connection info pointer within the C library. We don't use this directly
-- in the Idris code, but it is a parameter in all stateful operations.
RawConnInfo : Type
RawConnInfo = Ptr

-- C Error Code
ErrorCode : Type
ErrorCode = Int

-- TODO: Validation
-- Also, no IPv6 support yet...
data IPAddr = IPv4Addr Int Int Int Int
            | IPv6Addr 


data TCPStep = StateAllocated
             | ConnectAttempted
             | NotConnected

data TCPRes : TCPStep -> Type
  TCPConnected : Ptr -> TCPRes
  TCPStateAllocated : Ptr -> TCPRes
  TCPError : Maybe ErrorCode -> Maybe RawConnInfo -> TCPRes

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
  AllocateState : TCPRes () (TCPRes StateAllocated) Bool
  -- Frees the state. Must be called at the end, in order to prevent memory leaks.
  FreeState : TCPRes (TCPRes NotConnected) () ()

TCP : Type -> EFFECT
TCP t = MkEff t Tcp

private
connect' : IPAddr -> Port -> EffM IO [TCP (TCPRes StateAllocated)] [TCP (TCPRes TCPConnected)] (Maybe ErrorCode)
connect' ip port = (Connect ip port)

private
disconnect' : EffM IO [TCP (TCPRes NotConnected)] [TCP ()] ()
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
                     if alloc_res then
                       connect_res <- connect' ip port
                       if (eitherSuccess connect_res) then
                         return True
                       else
                         return False
                     else
                       return False

disconnect : EffM IO [TCP (TCPRes ConnectAttempted)] [TCP ()] ()
disconnect = do disconnect
                freeState

sendData : String -> Eff IO [TCP (TCPRes ConnectAttempted)] (Maybe ErrorCode)
sendData dat = (SendData dat)

recvData : Eff IO [TCP (TCPRes ConnectAttempted)] Bool
recvData = RecvData

getLastError : Eff IO [TCP (TCPRes ConnectAttempted)] (Maybe ErrorCode)
getLastError = GetLastError


disconnect : EffM IO [TCP (TCPRes ConnectAttempted)] [TCP (TCPRes NotConnected)] ()
disconnect = Disconnect

foreignGetLastError : RawConnInfo -> IO Int
foreignGetLastError ptr = mkForeign (FFun "idrnet_get_last_error" [] FUnit) ptr

instance Handler IO Tcp where
  -- Connection handlers
  handle (Connect ip port) (TCPStateAllocated ptr) = do
    conn_res <- mkForeign (FFun "idrnet_connect" [FPtr, FString, FString] FInt) ptr ip (show port)
    if (conn_res != 0) then -- Error occurred C-side
      k (TCPError (Just conn_res)) False
    else 
      k (TCPConnected ptr) True
  
  -- If Connect is called with any other resource, it's a failure case.
  -- TODO: Increase granularity so the case Connect (TCPRes Connected) doesn't happen, by separating into a different resource
  handle (Connect _ _) (TCPError maybe_err) k = k (TCPError maybe_err) False
  handle (Connect _ _ ) _ k = k (TCPError Nothing) False

  -- Disconnecting
  -- If we have a pointer to the connection state, use it.
  handle (Disconnect) (TCPConnected ptr) k = do
    conn_res <- mkForeign (FFun "idrnet_close" [FPtr] FInt) ptr
    k (TCPStateAllocated ptr) ()

  handle (Disconnect) (TCPError err (Just ptr)) k = do
    mkForeign (FFun "idrnet_close" [FPtr] FInt) ptr
    k (TCPError err (Just ptr)) ()

  handle (Disconnect) (TCPError err Nothing) k = do
    mkForeign (FFun "idrnet_close" [FPtr] FInt) ptr
    k (TCPErr err Nothing) ()

  -- There *shouldn't* be a case where we're at TCPStateAllocated but not connected. 
  -- For this reason, just ignore this call, to avoid closing an uninitialised socket.
  handle (Disconnect) (TCPStateAllocated ptr) k = k (TCPError err (Just ptr)) ()

  -- Sending Data
  handle (SendData dat) (TCPConnected ptr) k = do
    send_res <- mkForeign (FFun "idrnet_send" [FPtr, FString] FInt) ptr dat
    if (send_res == -1) then do -- Error occurred
      -- Get last error
      err <- foreignGetLastError ptr
      -- *very* unsure about this. It'd be nicer, if a write fails, to have an exception function trigger...
      -- TCP seems to be lending itself more to IOExcept than SQLite...
      k (TCPErr err (Just ptr)) (Left err)
    else k (TCPConnected ptr) (Right send_res)

  -- Pass through
  -- FIXME: This is hacky -- it returns -1, since we don't know whether we have an error stored or not
  handle (SendData dat) (TCPError maybe_err maybe_ptr) k = 
    k (TCPError maybe_err maybe_ptr) (Left maybe_err)

  handle (SendData dat) (TCPStateAllocated ptr) k = 
    k (TCPStateAllocated ptr) (Left Nothing)

  -- Receiving data
  handle (RecvData) (TCPConnected ptr) k = do
    recv_res <- mkForeign (FFun "idrnet_recv" [FPtr, FString] FPtr) ptr
    is_null <- isNull recv_res
    -- We still need to do the null check manually, before getting the string.
    if (not is_null) then do
      recv_data <- mkForeign (FFun "idrnet_recv" [FPtr, FString] FString) ptr
      k (TCPConnected ptr) (Right recv_data)
    else
            
 
