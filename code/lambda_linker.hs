{-# LANGUAGE DataKinds, KindSignatures, TypeFamilies #-}
module Lambda_linker where
-- import qualified Lambda_compiler as LC
-- import qualified Lambdaman_types as LT
import Data.Int
import Data.Vector
-- import Control.Monad.ST
import Data.STRef

data Mutability a = Mutable a | Immutable

type family Array (m :: Mutability *) a
type instance Array ('Mutable s) a = MVector s a
type instance Array 'Immutable a = Vector a

type family Ref (m :: Mutability *) a
type instance Ref ('Mutable s) a = STRef s a
type instance Ref 'Immutable a = a

type LinkedProgram = Vector LInstruction
data LInstruction
	= L_LDC Int32
	| L_LD Int Int
	| L_ADD
	| L_SUB
	| L_MUL
	| L_DIV
	| L_CEQ
	| L_CGT
	| L_CGTE
	| L_ATOM
	| L_CONS
	| L_CAR
	| L_CDR
	| L_SEL Address Address
	| L_JOIN
	| L_LDF
	| L_AP Int
	| L_RTN
	| L_DUM Int
	| L_RAP Int
	| L_STOP
	| L_TSEL Address Address
	| L_TAP Int
	| L_TRAP Int
	| L_ST Int Int
	| L_DBUG
	| L_BRK
  deriving (Show, Eq)
type Address = Int

data LAtom =
	LInt Int32 
	| LCons LAtom LAtom
	-- | LClosure Address Environment

{-
data LMachine m = LMachine
	{ lmState :: Array m Int
-}
	
	