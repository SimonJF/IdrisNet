module Main 

import Effects
import Effect.StdIO
import Network.TCP.TCPServer
import Network.TCP.TCPCommon

recvLoop' : Nat -> Client -> Eff IO [STDIO, TCPSERVER (TCPServerRes Listening)] ()
recvLoop' Z _ = return ()
recvLoop' (S k) c = do
  res <- lift' (recvData c)
  case res of
    SocketFailure i => do 
      lift' (putStr ("Error receiving data: "  ++ (show i) ++ "\n"))
      return ()
    SocketSuccess r_dat => do
      lift' (putStr $ "Received: " ++ r_dat ++ "\n")
      lift' (sendData c r_dat)
      recvLoop' k c

recvLoop : Nat -> Client -> Eff IO [TCPSERVER (TCPServerRes Listening)] ()
recvLoop n c = new () (recvLoop' n c)

acceptLoop : Nat -> Eff IO [TCPSERVER (TCPServerRes Listening)] ()
acceptLoop Z = return ()
acceptLoop (S k) = do
  m_client <- lift' accept
  case m_client of 
    SocketSuccess c => recvLoop 5 c
    SocketFailure _ => acceptLoop k

serverListen : (Maybe IPAddr) -> Port -> Eff IO [TCPSERVER ()] ()
serverListen m_ip p = do
  listen m_ip p
  acceptLoop 5
  stopListening

main : IO ()
main = run [()] (serverListen Nothing 9001)
