module Main 
import Effects
import Effect.StdIO
import Network.TCP

echoClientLoop : Nat -> Eff IO [TCP (TCPRes ConnectAttempted)] ()
echoClientLoop Z = return ()
echoClientLoop (S k) = do
  new () (do line <- getStr
             sendData line
             recv <- recvData
             putStr $ "Server sent: " ++ recv)
  echoClientLoop k

runEchoClient : IPAddr -> Port -> Eff IO [TCP ()] ()
runEchoClient ip port = do
  conn_res <- connect ip port
  case conn_res of
    Nothing => do echoClientLoop 5 -- no error
                  disconnect
    Just err => disconnect

main : IO ()
main = run [()] (runEchoClient (IPv4Addr 127 0 0 1) (9001))


