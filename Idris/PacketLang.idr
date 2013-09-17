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


-- Proof search
{-
isThatSo : List (TTName, Binder TT) -> TT -> Tactic
isThatSo ctxt goal = (Refine "oh") `Seq` Solve


pines : so ((S Z) < (S (S Z)))
pines = ?mv
-}

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
-- chunkTy (Bit w p) = Bounded 
chunkTy CString = String
chunkTy (LString i) = String
chunkTy (Prop p) = propTy p


