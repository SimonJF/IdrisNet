-- Effectual interface to the raw binary data library.
module Network.Packet
import Network.PacketLang
import Effects

%access public

-- Pointer to the raw packet
data RawPacket = RawPckt Ptr

-- Type synonyms for different arguments to foreign functions
BytePos : Type
BytePos = Int

Position : Type
Position = Int

ByteData : Type
ByteData = Int

Length : Type
Length = Int


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

rawSetByte : Int -> Int -> EffM IO [PACKET (ActivePacket)] 
                                   [PACKET (Either (FailedPacket) (ActivePacket))] ()
rawSetByte pos dat = (RawSetByte pos dat)

rawSetBits : Int -> Int -> Int -> EffM IO [PACKET (ActivePacket)] 
                                          [PACKET (Either (FailedPacket) (ActivePacket))] ()
rawSetBits start end dat = (RawSetBits start end dat)


foreignCreatePacket : Int -> IO RawPacket
foreignCreatePacket len = map RawPckt $ mkForeign (FFun "newPacket" [FInt] FPtr) len

foreignSetByte : RawPacket -> Position -> ByteData -> IO ()
foreignSetByte (RawPckt pckt) dat pos = 
  mkForeign (FFun "setPacketByte" [FPtr, FInt, FInt] FUnit) pckt dat pos

foreignSetBits : RawPacket -> Position -> Position -> ByteData -> IO ()
foreignSetBits (RawPckt pckt) start end dat = 
  mkForeign (FFun "setPacketBits" [FPtr, FInt, FInt, FInt] FUnit) pckt start end dat

foreignSetString : RawPacket -> Position -> String -> Int -> Char -> IO ()
foreignSetString (RawPckt pckt) start dat len term =
  mkForeign (FFun "setPacketString" [FPtr, FInt, FString, FInt, FChar] FUnit) pckt start dat len term

foreignGetByte : RawPacket -> Position -> IO ByteData
foreignGetByte (RawPckt pckt) pos = 
  mkForeign (FFun "getPacketByte" [FPtr, FInt] FInt) pckt pos

foreignGetBits : RawPacket -> Position -> Position -> IO ByteData
foreignGetBits (RawPckt pckt) start end =
  mkForeign (FFun "getPacketBits" [FPtr, FInt, FInt] FInt) pckt start end

{- Chunk length in bits -}
chunkLength : (c : Chunk) -> chunkTy c -> Int
chunkLength (Bit w p) x1 = w
chunkLength CString str = 8 * (strLen . fst $ span (== '\0') str)
chunkLength (LString len) str = 8 * len 
chunkLength (Prop _) x1 = 0 -- Not written to the packet


{- Marshalling code -}

{- Marshal Chunks to ByteData -}

marshalChunk : ActivePacket -> (c : Chunk) -> (chunkTy c) -> IO Length
marshalChunk (ActivePacketRes pckt pos) (Bit w p) (BInt dat p2) = do
  let len = chunkLength (Bit w p) (BInt dat p2)
  foreignSetBits pckt pos (pos + w) dat
  return len
marshalChunk (ActivePacketRes pckt pos) CString str = do
  let len = chunkLength CString str
  foreignSetString pckt pos str len '\0'
  return len
marshalChunk (ActivePacketRes pckt pos) (LString n) str = do
  let len = chunkLength (LString n) str 
  foreignSetString pckt pos str len '\0'
  return len
marshalChunk (ActivePacketRes pckt pos) (Prop _) x2 = return 0 -- We're not doing anything
  


marshalList : ActivePacket -> (pl : PacketLang) -> mkTy pl -> IO Length
marshalList ap pl vals = map (\(ActivePacket pckt pos) -> pos) $ foldr (\a state -> marshalList' state pl) 

marshalList' : ActivePacket -> (pl : PacketLang) -> mkTy pl -> IO ActivePacket
marshalList' (ActivePacketRes pckt pos) pl val = do
  marshal_len <- marshal (ActivePacketRes pckt pos) pl val
  return (ActivePacket pckt (pos + marshal_len) + 1) 

{- Marshal PacketLang to ByteData -}
marshal : ActivePacket -> (pl : PacketLang) -> mkTy pl -> IO Length
marshal ap (CHUNK c) c_dat = marshalChunk ap c c_dat
marshal ap (IF True pl_t _) ite = marshal ap pl_t ite
marshal ap (IF False _ pl_f) ite = marshal ap pl_f ite
marshal ap (pl_1 // pl_2) x = either x (\x_l => marshal ap pl_1 x_l)
                                       (\x_r => marshal ap pl_2 x_r) 
marshal ap (LIST _) x2 = ?marshal_rhs_5
marshal ap (LISTN _ _) x2 = ?marshal_rhs_6
marshal ap (_ >>= _) x2 = ?marshal_rhs_7



instance Handler Packet IO where
  handle () (CreatePacket len) k = do
    pckt <- foreignCreatePacket len
    k (Right $ ActivePacketRes pckt 0) ()

  handle (ActivePacketRes pos pckt) (WritePacket lang dat) k = ?mv
