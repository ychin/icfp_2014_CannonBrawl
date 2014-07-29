module LmUntypedExpr where
import Data.Int
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.ST
import Data.STRef

-- a program to execute
data LProgram = LProgram
    { lpMain :: String
	, lpFunctions :: [LFunction]
	}
	deriving (Show)

data LModule = LModule { lmFunctions :: [LFunction] }

-- a top-level function declaration
data LFunction = LF
	{ lfName :: String
	, lfArguments :: [String]
	, lfValue :: LExpr
	}
	deriving (Show)

-- expressions
data LExpr =
	-- constant values
	EConst Int32

	-- references to variables in the environment
	| EVar String
	| EGlobal String

	-- primitive operations including allocation and breaking down of cons cells
	| EUnOp LUnOp LExpr
	| EBinOp LBinOp LExpr LExpr

	-- evaluates the first expression,
	-- then if that is true (non-zero atom), evaluates the second expression,
	-- otherwise the third expression
	| EIf LExpr LExpr LExpr

	-- call a closure (either global or local)
	| EAp LExpr [LExpr]

	-- a closure in the current environment
	| ELambda [String] LExpr

	-- bool "is recursive" flag
	-- list of bindings of values to expressions
	-- then evaluate the inner expression in the new context
	| ELet Bool [(String,LExpr)] LExpr

	-- mutate the variable specified as the result of the first expression,
	-- then evaluate the second expression and return its result
	| EAssign String LExpr LExpr
	
	-- debug trace during evaluation
	-- print the first expression, then return the result of evaluating the second
	| ETrace LExpr LExpr

	-- runtime failure
	| EError String

	deriving (Show)

-- operations for EUnOp / EBinOp
data LBinOp = OAdd | OSub | OMul | ODiv | OEq | OGt | OGte | OCons deriving (Show)
data LUnOp = OAtom | OCar | OCdr | ONot | ONegate deriving (Show)

-- some syntax sugar
instance Num LExpr where
	(+) = EBinOp OAdd
	(-) = EBinOp OSub
	(*) = EBinOp OMul
	negate = EUnOp ONegate
	abs x = EIf (x `gteE` 0) x (negate x)  -- Note: duplicates evaluation of x
	-- alternative implementation if we could get a fresh variable?
	-- abs x = ELet False [(v, x)] $ EIf (EVar v `eLt` 0) (EVar v) (negate (EVar v))
	--     where v = "some_fresh_var"
	signum _ = error "LExpr signum: not implemented"
	fromInteger n = EConst (fromInteger n)

eqE, neE, ltE, gtE, lteE, gteE :: LExpr -> LExpr -> LExpr
eqE x y = EBinOp OEq x y
neE x y = EUnOp ONot (eqE x y)
gtE x y = EBinOp OGt x y
gteE x y = EBinOp OGte x y
ltE x y = EUnOp ONot (gteE x y)
lteE x y = EUnOp ONot (gtE x y)

divE :: LExpr -> LExpr -> LExpr
divE = EBinOp ODiv

consE :: LExpr -> LExpr -> LExpr
consE = EBinOp OCons

ifE :: LExpr -> LExpr -> LExpr -> LExpr
ifE = EIf

---
--- Evaluation
---

data LAtom s =
	ANum !Int32
	| ACons !(LAtom s) !(LAtom s)
	| AClosure (EvalEnvironment s) [String] LExpr
	| ABlackhole

data EvalError = 
	TypeError
	| UnboundLocal String
	| UnboundGlobal String
	| RecDependency
	| NumArgumentsError
	| DivByZero
	| SomeError String
	| RuntimeError String
	deriving Show

instance Error EvalError where
	noMsg = SomeError "unknown"
	strMsg = SomeError

data EvalEnvironment s = EEnv
	{ envLocals :: [(String, STRef s (LAtom s))]
	, envGlobals :: [(String, LAtom s)]
	}

type EvalM s a = ReaderT (EvalEnvironment s) (ErrorT EvalError (ST s)) a

readRef :: STRef s a-> EvalM s a
readRef ref = lift $ lift $ readSTRef ref

writeRef :: STRef s a -> a -> EvalM s ()
writeRef ref val = lift $ lift $ writeSTRef ref val

newRef :: a -> EvalM s (STRef s a)
newRef val = lift $ lift $ newSTRef val

lookupEnvironment :: String -> EvalM s (LAtom s)
lookupEnvironment name = do
	entry <- asks (lookup name . envLocals)
	maybe (throwError $ UnboundLocal name) readRef entry

lookupGlobal :: String -> EvalM s (LAtom s)
lookupGlobal name = do
	entry <- asks (lookup name . envGlobals)
	maybe (throwError $ UnboundGlobal name) return entry

writeEnvironment :: String -> LAtom s -> EvalM s ()
writeEnvironment name val = do
	entry <- asks (lookup name . envLocals)
	maybe (throwError $ UnboundLocal name) (\ref -> writeRef ref val) entry

-- Evaluate to result
evalProgram :: LProgram -> [LExpr] -> ST s (Either EvalError (LAtom s))
evalProgram program args = runErrorT $ runReaderT (eval expr) env where
	env = EEnv [] (map toEnvEntry $ lpFunctions program)
	expr = EAp (EGlobal $ lpMain program) args
	toEnvEntry lf = (lfName lf, AClosure env (lfArguments lf) (lfValue lf))

eval :: LExpr -> EvalM s (LAtom s)
eval (EConst n) = return (ANum n)
eval (EVar name) = do
	val <- lookupEnvironment name
	case val of ABlackhole -> throwError RecDependency
	            _ -> return val
eval (EGlobal name) = lookupGlobal name
eval (EUnOp op expr) = do
	atom <- eval expr
	evalUnOp op atom
eval (EBinOp op expr1 expr2) = do
	atom1 <- eval expr1
	atom2 <- eval expr2
	evalBinOp op atom1 atom2
eval (EIf cond ifT ifF) = do
	atom <- eval cond
	case atom of 
		ANum 0 -> eval ifT
		ANum _ -> eval ifF
		ABlackhole -> throwError RecDependency
		_      -> throwError TypeError
eval (ELambda args expr) = do
	env <- ask
	return (AClosure env args expr)
eval (EAp f vs) = do
	atomVs <- mapM eval vs
	atomRefs <- mapM newRef atomVs  -- allocate new frame

	atomF <- eval f
	(env,args,val) <- case atomF of
		AClosure env vars val
			| length vars == length vs -> return (env,vars,val)
			| otherwise                -> throwError NumArgumentsError
		ABlackhole -> throwError RecDependency
		_ -> throwError TypeError

	-- evaluate the subexpression in the new context
	local (\_ -> env { envLocals = zip args atomRefs ++ envLocals env } ) $ eval val
eval (ELet False bindings expr) = do
	-- eval bindings to make environment addition
	envExtra <- forM bindings $ \(name, val) -> do
		atomVal <- eval val
		atomRef <- newRef atomVal
		return (name, atomRef)

	-- eval expr with new bindings in scope
	local (\env -> env { envLocals = envExtra ++ envLocals env }) $ eval expr
eval (ELet True bindings expr) = do
	-- create new frame for bindings and blackhole all the entries
	let names = map fst bindings
	let exprs = map snd bindings
	atomRefs <- mapM (const $ newRef ABlackhole) bindings

	-- in this new context
	local (\env -> env { envLocals = zip names atomRefs ++ envLocals env }) $ do
		-- evaluate the bindings
		atomVs <- mapM eval exprs

		-- assign them to the targets
		mapM_ (uncurry writeRef) $ zip atomRefs atomVs

		-- then evaluate the subexpression
		eval expr
eval (EAssign name val expr) = do
	newVal <- eval val
	writeEnvironment name newVal
	eval expr

eval (ETrace traceExpr resultExpr) = do
	_ignored <- eval traceExpr
	-- TODO: print it?
	eval resultExpr

eval (EError s) = throwError (RuntimeError s)

evalUnOp :: LUnOp -> LAtom s -> EvalM s (LAtom s)
evalUnOp OAtom (ANum _) = return $ ANum 1
evalUnOp OAtom _        = return $ ANum 0
evalUnOp OCar (ACons v _) = return v
evalUnOp OCdr (ACons _ v) = return v
evalUnOp ONot (ANum n) = return $ ANum (if (n==0) then 1 else 0)
evalUnOp ONegate (ANum n) = return $ ANum (negate n)
evalUnOp _ ABlackhole = throwError RecDependency
evalUnOp _ _ = throwError TypeError

evalBinOp :: LBinOp -> LAtom s -> LAtom s -> EvalM s (LAtom s)
evalBinOp OAdd (ANum x) (ANum y) = return $ ANum (x+y)
evalBinOp OSub (ANum x) (ANum y) = return $ ANum (x-y)
evalBinOp OMul (ANum x) (ANum y) = return $ ANum (x*y)
evalBinOp ODiv (ANum x) (ANum y) = if (y == 0) then throwError DivByZero else return (ANum (div x y))
evalBinOp OEq (ANum x) (ANum y) = return $ ANum (if x==y then 1 else 0)
evalBinOp OGt (ANum x) (ANum y) = return $ ANum (if x>y then 1 else 0)
evalBinOp OGte (ANum x) (ANum y) = return $ ANum (if x>=y then 1 else 0)
evalBinOp OCons x y = return $ ACons x y
evalBinOp _ ABlackhole _ = throwError RecDependency
evalBinOp _ _ ABlackhole = throwError RecDependency
evalBinOp _ _ _ = throwError TypeError
