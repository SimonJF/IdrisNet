module Main
import Network.Packet
import Network.PacketLang
import Effects

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

main : IO ()
main = do
  result <- run [()] makeAndDump
  putStrLn result

