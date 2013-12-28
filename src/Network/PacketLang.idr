module Network.PacketLang 
import Language.Reflection
%access public

-- Start off with a direct translation of the data types from the 
-- paper and go from there

-- Propositions about data
data Proposition : Type where
  P_LT : Nat -> Nat -> Proposition
  P_EQ : Nat -> Nat -> Proposition
  P_BOOL : Bool -> Proposition
  P_AND : Proposition -> Proposition -> Proposition
  P_OR : Proposition -> Proposition -> Proposition

-- Bounded integers
data Bounded : Int -> Type where
-- TODO: The so proof should be a proof that x fits into i bits
  BInt : (x : Int) -> so (x < i) -> Bounded i

-- Primitive Binary Chunks
data Chunk : Type where
  -- Bits must be at least 1 wide
  Bit : (width : Int) -> so (width > 0) -> Chunk
  -- Native C String, null terminated
  CString : Chunk
  -- String with fixed bounded length
  LString : Int -> Chunk
  -- String with dynamic bounded length
  --LString : ((length s) ** (s : String)) -> Chunk
  -- Proposition about data
  Prop : (P : Proposition) -> Chunk

infixl 5 //

isThatSo : List (TTName, Binder TT) -> TT -> Tactic
isThatSo ctxt goal = (Refine "oh")


pines : so ((S Z) < (S (S Z)))
pines = oh


data Both : Proposition -> Proposition -> Type where
  MkBoth : (a : Proposition) -> (b : Proposition) -> (so (propTy a)) -> (so (propTy b)) -> Both a b

-- Decode propositions into Idris types.
propTy : Proposition -> Type
propTy (P_LT x y) = LT x y
propTy (P_EQ x y) = x=y
propTy (P_BOOL b) = so b
propTy (P_AND s t) = Both s t
propTy (P_OR s t) = Either (propTy s) (propTy t)

-- Decode chunks into Idris types
-- TODO <<
partial
chunkTy : Chunk -> Type
chunkTy (Bit w p) = Bounded w -- FIXME, take into account bit width
chunkTy CString = String
chunkTy (LString i) = String
chunkTy (Prop p) = propTy p



-- Packet Language
mutual
  data PacketLang : Type where
    CHUNK : (c : Chunk) -> PacketLang
    IF : (test : Bool) -> (yes : PacketLang) -> (no : PacketLang) -> PacketLang
    -- // : or
    (//) : PacketLang -> PacketLang -> PacketLang
    LIST : PacketLang -> PacketLang
    LISTN : (n : Nat) -> PacketLang -> PacketLang
    (>>=) : (p : PacketLang) -> (mkTy p -> PacketLang) -> PacketLang

  -- Packet language decoding
  mkTy : PacketLang -> Type
  mkTy (CHUNK c) = chunkTy c
  mkTy (IF x t e) = if x then (mkTy t) else (mkTy e)
  mkTy (l // r) = Either (mkTy l) (mkTy r)
  mkTy (LIST x) = List (mkTy x)
  mkTy (LISTN n a) = Vect n (mkTy a)
  mkTy (c >>= k) = (x ** mkTy (k x))


-- Syntax rules, so it's nicer to write these things...
-- Perhaps this would be better as an effectual EDSL? Well, can look into it anyway.
--bit_ : (i : Int) -> (w : Int) -> (so (i < w)) -> Chunk
--bit_ i w p = Bit i p

bit : (w : Int) -> {default tactics { refine oh; solve;} 
                     p : so (w > 0) } 
                -> Chunk
bit w {p} = Bit w p

-- syntax bit [x] = Bit x oh
syntax bits [n] = CHUNK (bit n)
--syntax bytes [n] = CHUNK (bit (n * 8))
--syntax bounded [x] = BInt x oh
syntax check [p] = CHUNK (Prop (P_BOOL p))
syntax lstring [n] = CHUNK (LString n)
syntax cstring = CHUNK (CString)

val : Bounded i -> Int
val (BInt i p) = i

-- grrrrr, hackity hack
natToInt : Nat -> Int
natToInt Z = 0
natToInt (S k) = 1 + (natToInt k)

intToNat : Int -> Nat
intToNat 0 = Z
intToNat i = S (intToNat (i - 1))

strLen : String -> Int
strLen s = natToInt $ length s


dlString : PacketLang
dlString = do len <- bits 8
              str <- lstring (val len)
              check ((natToInt . Prelude.Strings.length $ str) == (val len))

--bit : {w : Int} -> (i : Int) -> (p : (so (i < w))) -> Chunk
--bit i p = Bit i p

--bits : Int -> PacketLang
--bits n = CHUNK (bit n)

myBounded : Bounded 5
myBounded = BInt 0 oh


stringFormat : PacketLang
--stringFormat i = (bits i) >>= 
--                 (\len => (check (val len > 0)) >>= \_ => lstring (val len))
stringFormat = do len <- bits 5
                  check ((val len) > 0)
                  --let len = myBounded
                  lstring (val len)
-- ICMP Stuff
-- (Bit of a failed experiment, as we'll also need to implement all of IP...)
ICMPType : Type
ICMPType = Int

ICMPCode : Type
ICMPCode = Int

validCode : ICMPType -> ICMPCode -> Bool
-- Echo Reply
validCode 0 x = x == 0
-- 1 and 2 are reserved
-- 3: Destination Unreachable
validCode 3 x = x >=0 && x <= 15
-- 4: Source Quench: Congestion Control
validCode 4 x = x == 0
-- 5: Redirect Message
validCode 5 x = x >= 0 && x <= 3
-- 6 and 7 unused
-- 8: Echo Request
validCode 8 x = x == 0
-- 9: Router Advertisement
validCode 9 x = x == 0
-- Router Solicitation
validCode 10 x = x == 0
-- Time Exceeded
validCode 11 x = x == 1 || x == 2
-- Incorrect parameter
validCode 12 x = x >= 0 && x <= 2
-- Timestamp Request
validCode 13 x = x == 0
-- Timestamp Response
validCode 14 x = x == 0
-- Information Request
validCode 15 x = x == 0
-- Information Response
validCode 16 x = x == 0
-- Address Mask Request
validCode 17 x = x == 0
-- Address Mask Response
validCode 18 x = x == 0
-- 19 -> 29 reserved
-- 30: Traceroute information reques
validCode 30 x = x == 0
-- Lots of deprecated TP/IX fields, not really necessary
-- to implement right now, might do later for completeness
validCode _ _ = False

--icmpFormat : ICMPType -> ICMPCode -> PacketLang
--  icmpFormat t c = 
{-
ICMP : PacketLang
ICMP = do type <- bits 8
          code <- bits 8
          check (validCode (val type) (val code))
          checksum <- bits 16
-}


