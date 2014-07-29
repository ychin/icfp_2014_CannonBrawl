module LmLanguage where
import LmUntypedExpr hiding (LProgram, LModule)
import qualified LmUntypedExpr
import Prelude (Num(..), ($), (.), String, Bool(..), Maybe(..), fromIntegral, Show(..), Monad(..), Int)
import qualified Prelude
import Control.Monad.Identity (Identity(..))
{-import Data.IORef (IORef)-}
import Data.List (foldr)

-- VERY SLOW IF ENABLED
kTraceFunctions :: Bool 
kTraceFunctions = False

maybeTrace :: String -> LExpr -> LExpr
maybeTrace funcName expr = 
	if kTraceFunctions 
		then ETrace (untype $ debugString funcName) expr
		else expr

-- Add function to module
infixl 8 ^^
-- Combine modules into bigger module
infixl 7 ><

-- Operators on typed expressions
infixl 7 /
infix 4 ==, /=, <=, <, >=, >
infixr 3 &&
infixr 2 ||

-- Typed representation
newtype TFunction a = UnsafeMkFunc LFunction deriving (Show)
newtype TExpr a = UnsafeMkExpr LExpr deriving (Show)
type ExprM a = Identity a
newtype Arg a = MkArgs { unArgs :: a }
data NoArgs = NoArgs
data Ref a = UnsafeMkRef String

-- silly getters
untypeFunc :: TFunction a -> LFunction
untypeFunc (UnsafeMkFunc func) = func
untype :: TExpr a -> LExpr
untype (UnsafeMkExpr expr) = expr
refName :: Ref a -> String
refName (UnsafeMkRef s) = s

global :: TFunction a -> TExpr a
global (UnsafeMkFunc lf) = UnsafeMkExpr $ EGlobal (lfName lf)

unsafeGlobalName :: String -> TExpr a
unsafeGlobalName name = UnsafeMkExpr $ EGlobal name

-- This is the type signature I want for let_
-- but ExprM needs an upgrade for that to work.
--
-- let_ :: String -> ExprM (TExpr a) -> ExprM (TExpr a)
-- letRec1 :: String -> (TExpr a -> ExprM (TExpr a)) -> ExprM (TExpr a)
-- 
-- (and we need a general form for letRec in more than 1 variable as well)
--
-- So we'll use "with" instead for now.
with :: String -> ExprM (TExpr a) -> (TExpr a -> ExprM (TExpr b)) -> ExprM (TExpr b)
with name val inner = do
	expVal <- val
	expInner <- inner (unsafeVar name)
	return $ UnsafeMkExpr $ ELet False [(name, untype expVal)] (untype expInner)

withRec :: String -> (TExpr a -> ExprM (TExpr a)) -> (TExpr a -> ExprM (TExpr b)) -> ExprM (TExpr b)
withRec name val inner = do
	let boundVar = unsafeVar name
	expVal <- val boundVar
	expInner <- inner boundVar
	return $ UnsafeMkExpr $ ELet True [(name, untype expVal)] (untype expInner)

withRec2 :: (String,String) -> ((TExpr a1,TExpr a2) -> ExprM (TExpr a1,TExpr a2)) -> ((TExpr a1, TExpr a2) -> ExprM (TExpr b)) -> ExprM (TExpr b)
withRec2 (name1,name2) val inner = do
	let boundVar1 = unsafeVar name1; boundVar2 = unsafeVar name2
	(expVal1, expVal2) <- val (boundVar1, boundVar2)
	expInner <- inner (boundVar1, boundVar2)
	return $ UnsafeMkExpr $ ELet True [(name1, untype expVal1), (name2, untype expVal2)] (untype expInner)

withMut :: String -> ExprM (TExpr a) -> (Ref a -> ExprM (TExpr b)) -> ExprM (TExpr b)
withMut name val inner = do
	initialVal <- val
	expInner <- inner (UnsafeMkRef name)
	return $ UnsafeMkExpr $ ELet False [(name, untype initialVal)] (untype expInner)

deref :: Ref a -> TExpr a
deref = unsafeVar . refName

assign :: Ref a -> TExpr a -> TExpr b -> TExpr b
assign ref newVal cont = UnsafeMkExpr $ EAssign (refName ref) (untype newVal) (untype cont)

lam0 :: ExprM (TExpr b) -> ExprM (TExpr (NoArgs -> b))
lam0 func = do
	expLam <- block $ func
	return $ UnsafeMkExpr $ ELambda [] (untype expLam)

lam1 :: String -> (TExpr a -> ExprM (TExpr b)) -> ExprM (TExpr (Arg a -> b))
lam1 name func = do
	expLam <- block $ func (unsafeVar name)
	return $ UnsafeMkExpr $ ELambda [name] (untype expLam)

lam2 :: (String, String) -> ((TExpr a,TExpr b) -> ExprM (TExpr c)) -> ExprM (TExpr ((Arg a, Arg b) -> c))
lam2 (name1,name2) func = do
	expLam <- block $ func (unsafeVar name1, unsafeVar name2)
	return $ UnsafeMkExpr $ ELambda [name1, name2] (untype expLam)

lam3 :: (String, String,String) -> ((TExpr a,TExpr b,TExpr c) -> ExprM (TExpr d)) -> ExprM (TExpr ((Arg a, Arg b,Arg c) -> d))
lam3 (name1,name2,name3) func = do
	expLam <- block $ func (unsafeVar name1, unsafeVar name2, unsafeVar name3)
	return $ UnsafeMkExpr $ ELambda [name1, name2, name3] (untype expLam)

-- TODO: Typeclass these somehow?

function0 :: String -> ExprM (TExpr a) -> TFunction (NoArgs -> a)
function0 funcName func = UnsafeMkFunc $ LF funcName [] (maybeTrace funcName expr)
	where expr = untype $ runIdentity $ func
ap0 :: TExpr (NoArgs -> a) -> TExpr a
ap0 f = UnsafeMkExpr $ EAp (untype f) []
bindAp0 :: TFunction (NoArgs -> a) -> TExpr a
bindAp0 f = ap0 (global f)

function1 :: String -> String -> (TExpr a -> ExprM (TExpr b)) -> TFunction (Arg a -> b)
function1 funcName argName func = UnsafeMkFunc $ LF funcName [argName] (maybeTrace funcName expr)
	where expr = untype $ runIdentity $ func (unsafeVar argName)
ap1 :: TExpr (Arg a -> b) -> TExpr a -> TExpr b
ap1 f x = UnsafeMkExpr $ EAp (untype f) [untype x]
bindAp1 :: TFunction (Arg a -> b) -> TExpr a -> TExpr b
bindAp1 f = ap1 (global f)

function2 :: String -> (String,String) -> ((TExpr a, TExpr b) -> ExprM (TExpr c)) -> TFunction ((Arg a,Arg b) -> c)
function2 funcName (argName1,argName2) func = UnsafeMkFunc $ LF funcName [argName1, argName2] (maybeTrace funcName expr)
	where expr = untype $ runIdentity $ func (unsafeVar argName1, unsafeVar argName2)
ap2 :: TExpr ((Arg a,Arg b) -> c) -> TExpr a -> TExpr b -> TExpr c
ap2 f x y = UnsafeMkExpr $ EAp (untype f) [untype x, untype y]
bindAp2 :: TFunction ((Arg a, Arg b) -> c) -> TExpr a -> TExpr b -> TExpr c
bindAp2 f = ap2 (global f)

function3 :: String -> (String,String,String) -> ((TExpr a, TExpr b, TExpr c) -> ExprM (TExpr d)) -> TFunction ((Arg a,Arg b,Arg c) -> d)
function3 funcName (argName1,argName2,argName3) func = UnsafeMkFunc $ LF funcName [argName1, argName2, argName3] (maybeTrace funcName expr)
	where expr = untype $ runIdentity $ func (unsafeVar argName1, unsafeVar argName2, unsafeVar argName3)
ap3 :: TExpr ((Arg a,Arg b,Arg c) -> d) -> TExpr a -> TExpr b -> TExpr c -> TExpr d
ap3 f x y z = UnsafeMkExpr $ EAp (untype f) [untype x, untype y, untype z]
bindAp3 :: TFunction ((Arg a, Arg b, Arg c) -> d) -> TExpr a -> TExpr b -> TExpr c -> TExpr d
bindAp3 f = ap3 (global f)

function4 :: String -> (String,String,String,String) -> ((TExpr a, TExpr b, TExpr c, TExpr d) -> ExprM (TExpr e)) -> TFunction ((Arg a,Arg b,Arg c,Arg d) -> e)
function4 funcName (argName1,argName2,argName3,argName4) func = UnsafeMkFunc $ LF funcName [argName1, argName2, argName3,argName4] (maybeTrace funcName expr)
	where expr = untype $ runIdentity $ func (unsafeVar argName1, unsafeVar argName2, unsafeVar argName3, unsafeVar argName4)
ap4 :: TExpr ((Arg a,Arg b,Arg c,Arg d) -> e) -> TExpr a -> TExpr b -> TExpr c -> TExpr d -> TExpr e
ap4 f x y z w = UnsafeMkExpr $ EAp (untype f) [untype x, untype y, untype z, untype w]
bindAp4 :: TFunction ((Arg a, Arg b, Arg c, Arg d) -> e) -> TExpr a -> TExpr b -> TExpr c -> TExpr d -> TExpr e
bindAp4 f = ap4 (global f)

--
-- Low level operations
--

function6 :: String -> (String,String,String,String,String,String) -> ((TExpr a, TExpr b, TExpr c, TExpr d, TExpr e, TExpr f) -> ExprM (TExpr g)) -> TFunction ((Arg a,Arg b,Arg c,Arg d,Arg e,Arg f) -> g)
function6 funcName (argName1,argName2,argName3,argName4,argName5,argName6) func = UnsafeMkFunc $ LF funcName [argName1, argName2, argName3,argName4,argName5,argName6] expr
	where expr = untype $ runIdentity $ func (unsafeVar argName1, unsafeVar argName2, unsafeVar argName3, unsafeVar argName4, unsafeVar argName5, unsafeVar argName6)
ap6 :: TExpr ((Arg a,Arg b,Arg c,Arg d,Arg e,Arg f) -> g) -> TExpr a -> TExpr b -> TExpr c -> TExpr d -> TExpr e -> TExpr f -> TExpr g
ap6 f x y z w a b = UnsafeMkExpr $ EAp (untype f) [untype x, untype y, untype z, untype w, untype a, untype b]
bindAp6 :: TFunction ((Arg a, Arg b, Arg c, Arg d, Arg e, Arg f) -> g) -> TExpr a -> TExpr b -> TExpr c -> TExpr d -> TExpr e -> TExpr f -> TExpr g
bindAp6 f = ap6 (global f)


-- give unsafe name?
unsafeCast :: TExpr a -> TExpr b
unsafeCast (UnsafeMkExpr expr) = (UnsafeMkExpr expr)

unsafeBinOp :: (LExpr -> LExpr -> LExpr) -> TExpr a -> TExpr b -> TExpr c
unsafeBinOp op l r = UnsafeMkExpr $ op (untype l) (untype r)

unsafeOp :: (LExpr -> LExpr) -> TExpr a -> TExpr b
unsafeOp op x = UnsafeMkExpr $ op (untype x)

unsafeVar :: String -> TExpr a
unsafeVar name = UnsafeMkExpr $ EVar name

error :: String -> TExpr a
error msg = UnsafeMkExpr $ EError msg

debugString :: String -> TExpr [Int]
debugString [] = nil
debugString (x:xs) = cons (int $ Prelude.fromEnum x) (debugString xs)
--debugString xs = cons (int $ fourcc (Prelude.take 4 xs)) (debugString $ Prelude.drop 4 xs) where
--	fourcc = Prelude.foldl (\acc c -> 255*acc + Prelude.fromEnum c) 0

class ExpRepr a where
	toExpr :: a -> TExpr a

--
-- Object
--

data Object = ObjInt Int | ObjPair Object Object

instance ExpRepr Object where
	toExpr (ObjInt n) = UnsafeMkExpr (EConst $ fromIntegral n)
	toExpr (ObjPair a b) = consObj (toExpr a) (toExpr b)

isAtomic :: TExpr Object -> TExpr Bool
isAtomic = unsafeOp (EUnOp OAtom)

isPair :: TExpr Object -> TExpr Bool
isPair = not . isAtomic

intObject :: TExpr Int -> TExpr Object
intObject = unsafeCast

pairObject :: TExpr (Object, Object) -> TExpr Object
pairObject = unsafeCast

consObj :: TExpr Object -> TExpr Object -> TExpr Object
consObj = unsafeBinOp (EBinOp OCons)

car :: TExpr Object -> TExpr Object
car = unsafeOp (EUnOp OCar)

cdr :: TExpr Object -> TExpr Object
cdr = unsafeOp (EUnOp OCdr)

atom :: TExpr Object -> TExpr Int
atom = unsafeCast

objPair :: TExpr Object -> TExpr (Object, Object)
objPair = unsafeCast

inlineCaseObject :: TExpr Object -> TExpr a -> (TExpr (Object,Object) -> TExpr a) -> TExpr a
inlineCaseObject obj fAtom fPair = ifv (isAtomic obj) fAtom (fPair $ objPair obj)

--
-- Int
--

instance ExpRepr Int where
	toExpr n = UnsafeMkExpr (EConst $ fromIntegral n)

instance Num a => Num (TExpr a) where
	(+) = unsafeBinOp (+)
	(-) = unsafeBinOp (-)
	(*) = unsafeBinOp (*)
	negate = unsafeOp negate -- compiles to (0 - x)
	abs = unsafeOp abs
	fromInteger = UnsafeMkExpr . fromInteger
	signum = unsafeOp signum

(/) :: TExpr Int -> TExpr Int -> TExpr Int
(/) = unsafeBinOp divE

debugTrace :: TExpr a -> TExpr b -> TExpr b
--debugTrace = unsafeBinOp $ ETrace 
debugTrace _a b = b

int :: Int -> TExpr Int
int = fromIntegral

(==), (/=), (>), (>=), (<), (<=) :: TExpr Int -> TExpr Int -> TExpr Bool
(==) = unsafeBinOp eqE
(/=) = unsafeBinOp neE
(>) = unsafeBinOp gtE
(>=) = unsafeBinOp gteE
(<) = unsafeBinOp ltE
(<=) = unsafeBinOp lteE

-- 
-- [a] = [] | a:[a]
--

instance ExpRepr a => ExpRepr [a] where
	toExpr [] = unsafeCast (0 :: TExpr Int)
	toExpr (x:xs) = cons (toExpr x) (toExpr xs)

head :: TExpr [a] -> TExpr a
head = unsafeOp (EUnOp OCar)

tail :: TExpr [a] -> TExpr [a]
tail = unsafeOp (EUnOp OCdr)

null :: TExpr [a] -> TExpr Bool
null = unsafeOp (EUnOp OAtom)

not :: TExpr Bool -> TExpr Bool
not = unsafeOp (EUnOp ONot)  -- compiles to (x == 0)

nil :: TExpr [a]
nil = unsafeCast (0 :: TExpr Int)

cons :: TExpr a -> TExpr [a] -> TExpr [a]
cons = unsafeBinOp (EBinOp OCons)

--
-- (a,b) = (a,b)
--

instance (ExpRepr a, ExpRepr b) => ExpRepr (a,b) where
	toExpr (a,b) = pair (toExpr a) (toExpr b)

pair :: TExpr a -> TExpr b -> TExpr (a,b)
pair = unsafeBinOp (EBinOp OCons)

fst :: TExpr (a,b) -> TExpr a
fst = unsafeOp (EUnOp OCar)

snd :: TExpr (a,b) -> TExpr b
snd = unsafeOp (EUnOp OCdr)

--
-- Maybe a = Nothing | Just a
-- 

-- TODO? optimize Maybe (a,b) to nil | (a,b)
instance ExpRepr a => ExpRepr (Maybe a) where
	toExpr Nothing  = unsafeCast nil
	toExpr (Just a) = unsafeCast $ toExpr (1 :: Int, a)

nothing :: TExpr (Maybe a)
nothing = unsafeCast (int 0)

just :: TExpr a -> TExpr (Maybe a)
just x = unsafeCast $ pair (int 1) x

fJust :: TFunction (Arg a -> Maybe a)
fJust = function1 "just" "a" $ \a -> return $ just a

isNothing :: TExpr (Maybe a) -> TExpr Bool
isNothing x = null $ unsafeCast x

isJust :: TExpr (Maybe a) -> TExpr Bool
isJust = not . isNothing

fromJust  :: TExpr (Maybe a) -> TExpr a
fromJust x = snd $ unsafeCast x

inlineCaseMaybe :: TExpr (Maybe a) -> TExpr (b) -> (TExpr a -> TExpr b) -> TExpr b
inlineCaseMaybe m b f = 
	ifv (isNothing m) 
		b
		(f $ fromJust m)

inlineFromMaybe :: TExpr (Maybe a) -> TExpr a -> TExpr a
inlineFromMaybe m a = inlineCaseMaybe m a Prelude.id

--
-- Bool = False | True
-- 

instance ExpRepr Bool where
	toExpr False = unsafeCast (0 :: TExpr Int)
	toExpr True = unsafeCast (1 :: TExpr Int)

and :: TExpr Bool -> TExpr Bool -> TExpr Bool
and cond1 cond2 = unsafeCast ((unsafeCast cond1 :: TExpr Int) * (unsafeCast cond2))

and3 :: TExpr Bool -> TExpr Bool -> TExpr Bool -> TExpr Bool
and3 cond1 cond2 cond3 = and (and cond1 cond2) cond3

or :: TExpr Bool -> TExpr Bool -> TExpr Bool
or cond1 cond2 = ((unsafeCast cond1) + (unsafeCast cond2)) >= 1

or3 :: TExpr Bool -> TExpr Bool -> TExpr Bool -> TExpr Bool
or3 cond1 cond2 cond3 = ((unsafeCast cond1) + (unsafeCast cond2) + (unsafeCast cond3)) >= 1

or9 :: TExpr Bool -> TExpr Bool -> TExpr Bool -> TExpr Bool -> TExpr Bool -> TExpr Bool -> TExpr Bool -> TExpr Bool -> TExpr Bool -> TExpr Bool
or9 cond0 cond1 cond2 cond3 cond4 cond5 cond6 cond7 cond8 = ((unsafeCast cond0) + (unsafeCast cond1) + (unsafeCast cond2) + (unsafeCast cond3) + (unsafeCast cond4) + (unsafeCast cond5) + (unsafeCast cond6) + (unsafeCast cond7) + (unsafeCast cond8)) >= 1

ifv :: TExpr Bool -> TExpr a -> TExpr a -> TExpr a
ifv pred t f = UnsafeMkExpr $ EIf (untype pred) (untype t) (untype f)

ifm :: TExpr Bool -> ExprM (TExpr a) -> ExprM (TExpr a) -> ExprM (TExpr a)
ifm pred mt mf = do
	t <- block mt
	f <- block mf
	return $ ifv pred t f

cond :: [(TExpr Bool, TExpr a)] -> TExpr a -> TExpr a
cond preds baseCase = foldr (\(pred,result) rest -> ifv pred result rest) baseCase preds

condM :: [(TExpr Bool, ExprM (TExpr a))] -> ExprM (TExpr a) -> ExprM (TExpr a)
condM preds baseCase = foldr (\(pred,result) rest -> ifm pred result rest) (block baseCase) preds

-- TODO: This should confine local variables and stuff declared inside the expression to 'before' the second expression executes
block :: ExprM (TExpr a) -> ExprM (TExpr a)
block exp = exp

-- short circuiting
(&&) :: TExpr Bool -> TExpr Bool -> TExpr Bool
a && b = ifv a b (toExpr False)

(||) :: TExpr Bool -> TExpr Bool -> TExpr Bool
a || b = ifv a (toExpr True) b


--
-- Prelude
--

fCaseMaybe :: TFunction ((Arg (Maybe a), Arg (NoArgs -> r), Arg (Arg a -> r)) -> r)
fCaseMaybe = function3 "caseMaybe" ("v", "def", "f") $ \(v,def,f) -> return $ inlineCaseMaybe v (ap0 def) (ap1 f)

caseMaybe :: TExpr (Maybe a) -> TExpr r -> TExpr (Arg a -> r) -> TExpr r
caseMaybe ma ifNothing ifJust = bindAp3 fCaseMaybe ma (lazy ifNothing) ifJust

lazy :: TExpr a -> TExpr (NoArgs -> a)
lazy exp = UnsafeMkExpr (ELambda [] (untype exp))

fFromMaybe :: TFunction ((Arg (Maybe a), Arg (NoArgs -> a)) -> a)
fFromMaybe = function2 "fromMaybe" ("v", "def") $ \(v, def) -> return $ inlineFromMaybe v (ap0 def)

fromMaybe :: TExpr (Maybe a) -> TExpr a -> TExpr a
fromMaybe ma a = bindAp2 fFromMaybe ma (lazy a)

fPAp2_1 :: TFunction ( (Arg ((Arg a, Arg b) -> c), Arg a) -> (Arg b -> c) )
fPAp2_1 = function2 "pAp2_1" ("f", "a") $ \(f, a) -> do
	lam <- block $ lam1 "b" $ \b -> do
		return $ ap2 f a b
	return lam
pAp2_1 :: TExpr ((Arg a, Arg b) -> c) -> TExpr a -> TExpr (Arg b -> c)
pAp2_1 = bindAp2 fPAp2_1

preludeModule :: LModule
preludeModule = newModule
	^^ fCaseMaybe
	^^ fFromMaybe
	^^ fPAp2_1

--
-- Module & Program creation
--

mkProgram :: String -> LModule -> LProgram
mkProgram main = LmUntypedExpr.LProgram main . LmUntypedExpr.lmFunctions

newModule :: LModule
newModule = LmUntypedExpr.LModule []

type LProgram = LmUntypedExpr.LProgram
type LModule = LmUntypedExpr.LModule

(^^) :: LModule -> TFunction a -> LModule
funcs ^^ (UnsafeMkFunc func) = LmUntypedExpr.LModule (func : LmUntypedExpr.lmFunctions funcs)

(><) :: LModule -> LModule -> LModule
mod1 >< mod2 = LmUntypedExpr.LModule (LmUntypedExpr.lmFunctions mod1 Prelude.++ LmUntypedExpr.lmFunctions mod2)

