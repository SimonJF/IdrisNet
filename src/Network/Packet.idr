-- Effectual interface to the raw binary data library.
module Network.Packet
import Network.PacketLang
import Effects

%access public 

data RawPacket = RawPckt Ptr

BytePos : Type
BytePos = Int


data ActivePacket : Type where
  ActivePacketRes : RawPacket -> BytePos -> ActivePacket

data FailedPacket : Type where
  FailedPacketRes : (Maybe RawPacket) -> FailedPacket

data Packet : Effect where
  CreatePacket : Int -> Packet () 
                               (Either (FailedPacket) (ActivePacket)) ()
  DestroyPacket : Packet (ActivePacket) () ()

  -- Dumps packet to console. Not something we really want in the final thing...
  DumpPacket : Packet (ActivePacket) (ActivePacket) ()
  ReadPacket : (p : PacketLang) -> Packet (ActivePacket) 
                                          (Either (FailedPacket) (ActivePacket)) (Either () (mkTy p))
  WritePacket : (p : PacketLang) -> (mkTy p) -> Packet (ActivePacket)
                                                       (Either (FailedPacket) (ActivePacket)) ()

  -- Sets a byte at the given position to the given value
  RawSetByte : Int -> Int -> Packet (ActivePacket) (Either (FailedPacket) (ActivePacket)) ()
  -- Sets the bits between start and end positions to the given value
  RawSetBits : Int -> Int -> Int -> Packet (ActivePacket) (Either (FailedPacket) (ActivePacket)) ()
  
PACKET : Type -> EFFECT
PACKET t = MkEff t Packet


createPacket : Int -> EffM IO [PACKET ()] [PACKET (Either (FailedPacket) (ActivePacket))] ()
createPacket len = (CreatePacket len)

destroyPacket : EffM IO [PACKET (ActivePacket)] [PACKET ()] ()
destroyPacket = DestroyPacket

dumpPacket : Eff IO [PACKET (ActivePacket)] ()
dumpPacket = DumpPacket

readPacket : (p : PacketLang) -> EffM IO [PACKET (ActivePacket)] 
                                         [PACKET (Either (FailedPacket) (ActivePacket))]
                                         (Either () (mkTy p))
readPacket lang = (ReadPacket lang)

writePacket : (p : PacketLang) -> (mkTy p) -> EffM IO [PACKET (ActivePacket)] 
                                                      [PACKET (Either FailedPacket ActivePacket)] ()
writePacket pl dat = (WritePacket pl dat)

rawSetByte : Int -> Int -> EffM IO [PACKET (ActivePacket)] [PACKET (Either (FailedPacket) (ActivePacket))] ()
rawSetByte pos dat = (RawSetByte pos dat)

rawSetBits : Int -> Int -> Int -> EffM IO [PACKET (ActivePacket)] [PACKET (Either (FailedPacket) (ActivePacket))] ()
rawSetBits start end dat = (RawSetBits start end dat)


foreignCreatePacket : Int -> IO RawPacket
foreignCreatePacket len = map RawPckt $ mkForeign (FFun "newPacket" [FInt] FPtr) len



instance Handler Packet IO where
  handle () (CreatePacket len) k = do
    pckt <- foreignCreatePacket len
    k (Right $ ActivePacketRes pckt 0) ()
