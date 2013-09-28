module IdrisNet.PacketLang 
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
  BInt : (x : Int) -> so (x < i) -> Bounded i

-- Primitive Binary Chunks
data Chunk : Type where
  -- Bits must be at least 1 wide
  Bit : (width : Int) -> so (width > 0) -> Chunk
  -- Native C String, null terminated
  CString : Chunk
  -- String with bounded length
  LString : Int -> Chunk
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
--syntax bounded [x] = BInt x oh
syntax check [p] = CHUNK (Prop (P_BOOL p))
syntax lstring [n] = CHUNK (LString n)
syntax cstring = CHUNK (CString)


--bit : {w : Int} -> (i : Int) -> (p : (so (i < w))) -> Chunk
--bit i p = Bit i p



--bits : Int -> PacketLang
--bits n = CHUNK (bit n)

myBounded : Bounded 5
myBounded = BInt 0 oh

value : Bounded i -> Int
value (BInt i p) = i

stringFormat : PacketLang
--stringFormat i = (bits i) >>= 
--                 (\len => (check (value len > 0)) >>= \_ => lstring (value len))
stringFormat = do len <- bits 5
                  check ((value len) > 0)
                  --let len = myBounded
                  lstring (value len)
-- ICMP Stuff

--ICMP : PacketLang
--ICMP = ?mv
  

