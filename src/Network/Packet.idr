-- Effectual interface to the raw binary data library.
module Network.Packet
import Network.PacketLang
import Effects

%access public

%include C "bindata.h"
%link C "bindata.o"
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
  -- TODO: Inelegant, would be nice to consolidate (much like w/ SQLite)
  DestroyFailedPacket : Packet (FailedPacket) () ()

  -- Dumps packet to console. Not something we really want in the final thing...
  DumpPacket : Packet (ActivePacket) (ActivePacket) ()
  ReadPacket : (p : PacketLang) -> Packet (ActivePacket) 
                                          (Either (FailedPacket) (ActivePacket)) 
                                          (Either () (mkTy p))
  WritePacket : (p : PacketLang) -> (mkTy p) -> Packet (ActivePacket)
                                                       (Either (FailedPacket) 
                                                               (ActivePacket)) ()

  -- Sets a byte at the given position to the given value
  RawSetByte : Int -> Int -> Packet (ActivePacket) 
                                    (Either (FailedPacket) 
                                            (ActivePacket)) ()

  -- Sets the bits between start and end positions to the given value
  RawSetBits : Int -> Int -> Int -> Packet (ActivePacket) 
                                           (Either (FailedPacket) 
                                                   (ActivePacket)) ()

  -- Returns a raw pointer to the current packet
  GetRawPtr : Packet (ActivePacket) (ActivePacket) RawPacket



PACKET : Type -> EFFECT
PACKET t = MkEff t Packet

getRawPacket : Eff IO [PACKET (ActivePacket)] RawPacket
getRawPacket = GetRawPtr

createPacket : Int -> EffM IO [PACKET ()] [PACKET (Either (FailedPacket) (ActivePacket))] ()
createPacket len = (CreatePacket len)

destroyPacket : EffM IO [PACKET (ActivePacket)] [PACKET ()] ()
destroyPacket = DestroyPacket

destroyFailedPacket : EffM IO [PACKET (FailedPacket)] [PACKET ()] ()
destroyFailedPacket = DestroyFailedPacket

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

foreignDestroyPacket : RawPacket -> IO ()
foreignDestroyPacket (RawPckt pckt) = mkForeign (FFun "freePacket" [FPtr] FUnit) pckt

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

foreignDumpPacket : RawPacket -> Position -> IO ()
foreignDumpPacket (RawPckt pckt) len =
  mkForeign (FFun "dumpPacket" [FPtr, FInt] FUnit) pckt len

{- Chunk length in bits -}
chunkLength : (c : Chunk) -> chunkTy c -> Int
chunkLength (Bit w p) x1 = w
-- TODO: This doesn't take into account if there's a null character
-- within the string itself. I had something nice using span earlier,
-- but it didn't work (probably due to a library bug)
chunkLength CString str = 8 * (strLen str)
chunkLength (LString len) str = 8 * len 
chunkLength (Prop _) x1 = 0 -- Not written to the packet

{-
vectBitLength : (p : PacketLang) -> mkTy p -> Int
vectBitLength pl [] = 0
vectBitLength pl (x::xs) = ?vbl_rhs
-}

bitLength : (p : PacketLang) -> (mkTy p) -> Int
bitLength (CHUNK c) x = chunkLength c x
bitLength (IF False yes no) x = bitLength no x
bitLength (IF True yes no) x = bitLength yes x
bitLength (y // z) x = ?bitLength_rhs_3
bitLength (LIST y) x = ?bitLength_rhs_4
bitLength (LISTN n y) x = ?bitLength_rhs_5
bitLength (p >>= f) x = ?bitLength_rhs_6

{- Marshalling code -}

{- Marshal Chunks to ByteData -}

marshalChunk : ActivePacket -> (c : Chunk) -> (chunkTy c) -> IO Length
marshalChunk (ActivePacketRes pckt pos) (Bit w p) (BInt dat p2) = do
  let len = chunkLength (Bit w p) (BInt dat p2)
  foreignSetBits pckt pos (pos + w) dat
  return len
marshalChunk (ActivePacketRes pckt pos) CString str = do
  let len = chunkLength CString str
  putStrLn $ "CStr length: " ++ (show len)
  foreignSetString pckt pos str len '\0'
  return len
-- TODO: This is wrong, need to set the length in there explicitly
marshalChunk (ActivePacketRes pckt pos) (LString n) str = do
  let len = chunkLength (LString n) str 
  foreignSetString pckt pos str len '\0'
  return len
marshalChunk (ActivePacketRes pckt pos) (Prop _) x2 = return 0 -- We're not doing anything
  
  
marshalList : ActivePacket -> (pl : PacketLang) -> List (mkTy pl) -> IO Length
marshalVect : ActivePacket -> (pl : PacketLang) -> Vect n (mkTy pl) -> IO Length

marshal : ActivePacket -> (pl : PacketLang) -> mkTy pl -> IO Length
marshal ap (CHUNK c) c_dat = marshalChunk ap c c_dat
marshal ap (IF True pl_t _) ite = marshal ap pl_t ite
marshal ap (IF False _ pl_f) ite = marshal ap pl_f ite
marshal ap (pl_1 // pl_2) x = either x (\x_l => marshal ap pl_1 x_l)
                                       (\x_r => marshal ap pl_2 x_r) 
marshal ap (LIST pl) xs = marshalList ap pl xs
marshal ap (LISTN n pl) xs = marshalVect ap pl xs
marshal ap (c >>= k) (x ** y) = do
  len <- marshal ap c x
  let (ActivePacketRes pckt pos) = ap
  let ap2 = (ActivePacketRes pckt (pos + len)) 
  len2 <- marshal ap2 (k x) y
  return $ len + len2


--marshalList : ActivePacket -> (pl : PacketLang) -> mkTy pl -> IO Length
--marshalList ap pl vals = ?mv --map (\(ActivePacket pckt pos) -> pos) $ foldr (\a state -> marshalList' state pl) 


{- Marshal PacketLang to ByteData -}
--marshalVect : ActivePacket -> (pl : PacketLang) -> Vect n (mkTy pl) -> IO Length
marshalVect ap pl [] = return 0
marshalVect (ActivePacketRes pckt pos) pl (x::xs) = do
  len <- marshal (ActivePacketRes pckt pos) pl x
  xs_len <- marshalVect (ActivePacketRes pckt (pos + len)) pl xs
  return $ len + xs_len

--marshalList : ActivePacket -> (pl : PacketLang) -> List (mkTy pl) -> IO Length
marshalList ap pl [] = return 0
marshalList (ActivePacketRes pckt pos) pl (x::xs) = do
  len <- marshal (ActivePacketRes pckt pos) pl x
  xs_len <- marshalList (ActivePacketRes pckt (pos + len)) pl xs
  return $ len + xs_len

{- Unmarshalling Code -}
unmarshal : ActivePacket -> (pl : PacketLang) -> IO (Maybe (mkTy pl))
unmarshal ap (CHUNK c) = ?unmarshal_rhs_1
unmarshal ap (IF False yes no) = unmarshal ap no
unmarshal ap (IF True yes no) = unmarshal ap yes
-- Attempt x, if correct then return x.
-- If not, try y. If correct, return y. 
-- If neither correct, return Nothing.
unmarshal ap (x // y) = do
-- TODO: There's likely a more idiomatic way to do this... IO makes it trickier
  x_res <- unmarshal ap x
  y_res <- unmarshal ap y
  --return Nothing
  return $ maybe (maybe Nothing (\y_res' => Just $ Right y_res') y_res)
                                (\x_res' => Just $ Left x_res') x_res
unmarshal ap (LIST x) = ?unmarshal_rhs_4
unmarshal ap (LISTN n x) = ?unmarshal_rhs_5
unmarshal ap (p >>= f) = ?unmarshal_rhs_6

instance Handler Packet IO where
  handle () (CreatePacket len) k = do
    pckt <- foreignCreatePacket len
    k (Right $ ActivePacketRes pckt 0) ()

  handle (ActivePacketRes pckt pos) (WritePacket lang dat) k = do
    len <- marshal (ActivePacketRes pckt pos) lang dat
    putStrLn $ "Len: " ++ (show len)
    k (Right $ ActivePacketRes pckt (pos + len)) ()

  handle (ActivePacketRes pckt pos) (DestroyPacket) k = do
    foreignDestroyPacket pckt
    k () ()

  handle (FailedPacketRes (Just pckt)) (DestroyFailedPacket) k = do
    foreignDestroyPacket pckt
    k () ()

  handle (FailedPacketRes Nothing) (DestroyFailedPacket) k = 
    k () ()

  handle (ActivePacketRes pckt p_pos) (RawSetByte pos val) k = do
    foreignSetByte pckt pos val
    k (Right $ ActivePacketRes pckt p_pos) ()

  handle (ActivePacketRes pckt p_pos) (RawSetBits start end val) k = do
    foreignSetBits pckt start end val
    k (Right $ ActivePacketRes pckt p_pos) ()

  handle (ActivePacketRes pckt p_pos) (GetRawPtr) k =
    k (ActivePacketRes pckt p_pos) pckt

  handle (ActivePacketRes pckt p_pos) (DumpPacket) k = do
    foreignDumpPacket pckt p_pos
    k (ActivePacketRes pckt p_pos) ()
