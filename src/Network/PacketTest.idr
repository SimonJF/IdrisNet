module Main
import Network.Packet
import Network.PacketLang
import Effects
import Effect.StdIO

stringList : PacketLang
stringList = do
  LISTN 3 cstring
  LISTN 3 (bits 32)

myPacket : (mkTy stringList)
myPacket = (hwd ** nvect)
  where boundedInt : Bounded 32
        boundedInt = BInt 10 oh
        hwd : Vect 3 String
        hwd = ["hello", "world", "dears"]
        nvect : Vect 3 (Bounded 32)
        nvect = [boundedInt, boundedInt, boundedInt]

makeAndDump : Eff IO [PACKET ()] String
makeAndDump = do
  createPacket 1024
  if_valid then do
    writePacket stringList myPacket
    if_valid then do
      dumpPacket
      destroyPacket
      return "Success!"
    else do 
      destroyFailedPacket
      return "Error writing packet to memory"
  else do
    destroyFailedPacket
    return "Error allocating memory for packet"

makeAndRetain : Eff IO [PACKET ()] (Maybe RawPacket)
makeAndRetain = do
  createPacket 1024
  if_valid then do
    writePacket stringList myPacket
    if_valid then do
      dumpPacket
      res <- unsafeExit
      Effects.return $ Just res
    else do
      destroyFailedPacket
      Effects.return Nothing
  else do 
    destroyFailedPacket
    Effects.return Nothing

processPacket : RawPacket ->
                Eff IO [PACKET (), STDIO] ()
processPacket pckt = do
  res <- readPacket stringList pckt 1024
  if_valid then do
    -- Won't need this when I rewrite it with dependent effects...
    case res of 
      Just (hwd ** nvect) => do
        putStr ("Decoding successful, packet: " ++ (show hwd) ++ 
                   ", " ++ (show nvect) ++ "\n")
        destroyPacket
      _ => do putStr "Decoding unsuccessful :( \n"
              destroyPacket
  else do
    putStr "Decoding unsuccessful :( \n"
    destroyFailedPacket


main : IO ()
main = do
--  result <- run [()] makeAndDump
  res <- run [()] makeAndRetain
  case res of
       Just pckt => 
         run [(), ()] (processPacket pckt)
       Nothing => putStrLn "Packet writing failed"
  --putStrLn result

