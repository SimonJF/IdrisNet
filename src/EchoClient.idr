module Main 
import Effects
import Effect.StdIO
import Network.TCP.TCPClient
import Network.TCP.TCPCommon

echoClientLoop' : Eff IO [STDIO, TCPCLIENT (TCPClientRes ConnectAttempted)] Bool
echoClientLoop' = do line <- (lift' getStr)
                     lift' (sendData line)
                     recv <- (lift' recvData)
                     case recv of
                       Left (Just err) => do lift' (putStr $ "Error: " ++ (show err))
                                             return False
                       Left Nothing => do lift' (putStr $ "Internal error")
                                          return False
                       Right str => do lift' (putStr $ "Server sent: " ++ str)
                                       return True

echoClientLoop : Nat -> Eff IO [TCPCLIENT (TCPClientRes ConnectAttempted)] ()
echoClientLoop Z = return ()
echoClientLoop (S k) = do
  res <- new () echoClientLoop'
  if res then echoClientLoop k
         else return ()

runEchoClient : IPAddr -> Port -> Eff IO [TCPCLIENT ()] ()
runEchoClient ip port = do
  conn_res <- connect ip port
  case conn_res of
    Nothing => do echoClientLoop 5 -- no error
                  disconnect
    Just err => disconnect

main : IO ()
main = run [()] (runEchoClient (IPv4Addr 127 0 0 1) (9001))


