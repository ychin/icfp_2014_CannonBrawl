module Lmcompile where
import LmUntypedExpr(LBinOp(..), LUnOp(..), LExpr(..), LProgram(..), LFunction(..))
import Data.Int
import Control.Monad.State
-- import Control.Monad.Writer -- for old compiler
import qualified Data.Sequence as S
import Data.Sequence (ViewL(..), (|>), (<|))
import Data.Maybe (listToMaybe)
import Data.List (unfoldr, mapAccumL, foldl', sort)
import qualified Data.Map as M
-- For tests only
import qualified Lambda_compiler_2
import qualified LmLanguage

type Address = Int
data EnvAddr = EnvAddr { envName :: String, envFrameIdx :: Int, envVarIdx :: Int }
instance Show EnvAddr where
	show (EnvAddr n f v) = envStr ++ replicate (6 - length envStr) ' ' ++ " ; " ++ n
		where envStr = show f ++ " " ++ show v

data LCInstr env addr =
	LDC Int32
	| LD env
	| ADD | SUB | MUL | DIV
	| CEQ | CGT | CGTE
	| ATOM | CONS | CAR | CDR
	| SEL addr addr | JOIN
	| LDF addr | AP Int | RTN
	| DUM Int | RAP Int

	-- tail-call extensions
	| TSEL addr addr | TAP Int | TRAP Int

	-- pascal extensions
	| ST env

	-- debug extensions
	| DBUG | BRK

    -- not a real instruction, but another instruction with a comment
	| COMMENT String (LCInstr env addr)

instance (Show env, Show addr) => Show (LCInstr env addr) where
	show (LDC n) = "LDC " ++ show n
	show (LD env) = "LD " ++ show env
	show ADD = "ADD"
	show SUB = "SUB"
	show MUL = "MUL"
	show DIV = "DIV"
	show CEQ = "CEQ"
	show CGT = "CGT"
	show CGTE = "CGTE"
	show ATOM = "ATOM"
	show CONS = "CONS"
	show CAR = "CAR"
	show CDR = "CDR"
	show (SEL ta fa) = "SEL " ++ show ta ++ " " ++ show fa
	show JOIN = "JOIN"
	show (LDF a) = "LDF " ++ show a
	show (AP n) = "AP " ++ show n
	show RTN = "RTN"
	show (DUM n) = "DUM " ++ show n
	show (RAP n) = "RAP " ++ show n
	show (TSEL ta fa) = "TSEL " ++ show ta ++ " " ++ show fa
	show (TAP n) = "TAP " ++ show n
	show (TRAP n) = "TRAP " ++ show n
	show (ST env) = "ST " ++ show env
	show DBUG = "DBUG"
	show BRK = "BRK"
	show (COMMENT s i) = iStr ++ replicate (max 0 (20 - length iStr)) ' ' ++ " ; " ++ s
		where iStr = show i

newtype UniqueId = UniqueId Integer deriving (Show, Eq, Ord)
newtype UniqueIdGen = UGen Integer deriving Show

uniqGenNew :: UniqueIdGen
uniqGenNew = UGen 0

uniqNew :: UniqueIdGen -> (UniqueId, UniqueIdGen)
uniqNew (UGen n) = let next = n+1 in seq next $ (UniqueId n, UGen next)

data Label = Label UniqueId String
instance Eq Label where (Label id1 _) == (Label id2 _) = id1 == id2
instance Ord Label where compare (Label id1 _) (Label id2 _) = compare id1 id2
instance Show Label where show (Label (UniqueId uid) _) = "@" ++ show uid

type VarRef = EnvAddr

type CompileEnv = [[String]] -- stack frames
data InProgressBlock = PartialBlock
	{ pbBlock :: S.Seq (LCInstr VarRef Label)
	, pbEnv :: CompileEnv
	, pbLabel :: Label
	}
	deriving Show

data CompileState = CS
	{ csGlobals :: [String] -- global function names
	, csId :: UniqueIdGen

	, csBlock :: S.Seq (LCInstr VarRef Label)
	, csEnv :: CompileEnv
	, csLabel :: Label

	, csStack :: S.Seq InProgressBlock
	, csCompletedBlocks :: S.Seq (Label, S.Seq (LCInstr VarRef Label))
	}
	deriving Show

initialCompileState :: [String] -> CompileState
initialCompileState globals = CS
    { csGlobals = globals
	, csId = uniqGen
	, csBlock = S.empty
	, csEnv = []
	, csLabel = Label idzero ""
	, csStack = S.empty
	, csCompletedBlocks = S.empty
	}
	where
		(idzero, uniqGen) = uniqNew uniqGenNew

sToList :: S.Seq a -> [a]
sToList = unfoldr $ \xs -> case S.viewl xs of
	EmptyL -> Nothing
	a :< as -> Just (a,as)

prettyPrint :: CompileState -> String
prettyPrint cs = do
	(Label (UniqueId uid) name, instrs) <- sToList (csCompletedBlocks cs)
	concat
		[ "@", show uid, ":            ; ", name, "\n"
		, do
			inst <- sToList instrs
			concat [ "    ", show inst, "\n" ]
		]

data ErrorCondition =
	PopBlockEmptyStack
	| PopFrameEmptyStack
	| GlobalNotFound String
	| LocalNotFound String
	| LinkError String
    | DuplicateSymbol String
	deriving Show
type CompileError = (ErrorCondition, Maybe LFunction, Maybe LExpr)

type CompileM a = StateT CompileState (Either CompileError) a

sourceFunction :: LFunction -> CompileM a -> CompileM a
sourceFunction func act = StateT $ \s ->
	case runStateT act s of
		Left (eCond, Nothing, eExpr) -> Left (eCond, Just func, eExpr)
		res                          -> res

sourceExpr :: LExpr -> CompileM a -> CompileM a
sourceExpr expr act = StateT $ \s ->
	case runStateT act s of
		Left (eCond, eFunc, Nothing) -> Left (eCond, eFunc, Just expr)
		res                          -> res

combinePrefix :: String -> String -> String
combinePrefix ""     blockName = blockName
combinePrefix prefix blockName = prefix ++ "/" ++ blockName

labelName :: Label -> String
labelName (Label _ name) = name

pushBlock :: String -> CompileM Label
pushBlock blockName = do
	curPrefix <- gets (labelName . csLabel)
	let blockPrefix = combinePrefix curPrefix blockName
	blockLabel <- newLabel blockPrefix

	pushNewBlock blockLabel
	return blockLabel


pushNewBlock :: Label -> CompileM ()
pushNewBlock label = do
	modify $ \cs -> cs
		{ csStack = PartialBlock (csBlock cs) (csEnv cs) (csLabel cs) <| csStack cs
		, csBlock = S.empty
		, csLabel = label
		}

throwError :: ErrorCondition -> CompileM a
throwError eCond = lift (Left (eCond, Nothing, Nothing))

popBlock :: CompileM ()
popBlock = do
	inProgressBlocks <- gets (S.viewl . csStack)
	(pastBlock, restBlocks) <- case inProgressBlocks of
		EmptyL -> throwError PopBlockEmptyStack
		b :< bs -> return (b,bs)

	modify $ \cs -> cs 
		{ csCompletedBlocks = csCompletedBlocks cs |> (csLabel cs, csBlock cs)
		, csStack = restBlocks
		, csBlock = pbBlock pastBlock
		, csEnv = pbEnv pastBlock
		, csLabel = pbLabel pastBlock
		}

pushFrame :: [String] -> CompileM ()
pushFrame vars = do
	modify $ \cs -> cs
		{ csEnv = vars : csEnv cs
		}

popFrame :: CompileM ()
popFrame = do
	env <- gets csEnv
	case env of
		[] -> throwError PopFrameEmptyStack
		(_:restEnv) -> modify $ \cs -> cs { csEnv = restEnv }

lookupVar :: String -> CompileM VarRef
lookupVar var = do
	env <- gets csEnv
	case envLookup var env of
		Just addr -> return addr
		Nothing   -> throwError (LocalNotFound var)

lookupGlobal :: String -> CompileM VarRef
lookupGlobal var = do
	env <- gets csEnv
	globals <- gets csGlobals
	case globalLookup var (length env) globals of
		Just addr -> return addr
		Nothing   -> throwError (GlobalNotFound var)

instr :: LCInstr VarRef Label -> CompileM ()
instr i = modify $ \cs -> cs { csBlock = csBlock cs |> i }

newLabel :: String -> CompileM Label
newLabel name = do
	gen <- gets csId
	let (uniqId, newGen) = uniqNew gen
	modify $ \cs -> cs { csId = newGen }
	return $ Label uniqId name

envLookup :: String -> CompileEnv -> Maybe VarRef
envLookup var env = listToMaybe $ do
	(vars, frameIndex) <- zip env [0..]
	(varName, varIndex) <- zip vars [0..]
	guard (varName == var)
	return $ EnvAddr var frameIndex varIndex

globalLookup :: String -> Int -> [String] -> Maybe VarRef
globalLookup var envLength globalNames = listToMaybe $ do
	(global, globIndex) <- zip globalNames [0..]
	guard (global == var)
	return $ EnvAddr var envLength globIndex


block :: String -> CompileM a -> CompileM (a,Label)
block s act = do
	label <- pushBlock s
	res <- act
	popBlock
	return (res, label)

block_ :: String -> CompileM () -> CompileM Label
block_ s act = do
	label <- pushBlock s
	act
	popBlock
	return label

frame :: [String] -> CompileM a -> CompileM a
frame args act = do
	pushFrame args
	res <- act
	popFrame
	return res

frame_ :: [String] -> CompileM () -> CompileM ()
frame_ args act = do
	pushFrame args
	act
	popFrame

compileE :: LExpr -> CompileM ()
compileE expr = sourceExpr expr $ compileExpr expr

compileTE :: LExpr -> CompileM ()
compileTE expr = sourceExpr expr $ compileTailExpr expr

compileExpr :: LExpr -> CompileM ()
compileExpr (EConst n) = instr (LDC n)
compileExpr (EVar name) = do
	varRef <- lookupVar name
	instr (LD varRef)
compileExpr (EGlobal name) = do
	varRef <- lookupGlobal name
	instr (LD varRef)
compileExpr (EUnOp OAtom expr) = compileE expr >> instr ATOM
compileExpr (EUnOp OCar expr) = compileE expr >> instr CAR
compileExpr (EUnOp OCdr expr) = compileE expr >> instr CDR
compileExpr (EUnOp ONot expr) = compileE expr >> instr (LDC 0) >> instr CEQ     -- !x --> (x == 0)
compileExpr (EUnOp ONegate expr) = instr (LDC 0) >> compileE expr >> instr SUB  -- -x --> (0 - x)
compileExpr (EBinOp OAdd e1 e2) = compileE e1 >> compileE e2 >> instr ADD
compileExpr (EBinOp OSub e1 e2) = compileE e1 >> compileE e2 >> instr SUB
compileExpr (EBinOp OMul e1 e2) = compileE e1 >> compileE e2 >> instr MUL
compileExpr (EBinOp ODiv e1 e2) = compileE e1 >> compileE e2 >> instr DIV
compileExpr (EBinOp OEq e1 e2) = compileE e1 >> compileE e2 >> instr CEQ
compileExpr (EBinOp OGt e1 e2) = compileE e1 >> compileE e2 >> instr CGT
compileExpr (EBinOp OGte e1 e2) = compileE e1 >> compileE e2 >> instr CGTE
compileExpr (EBinOp OCons e1 e2) = compileE e1 >> compileE e2 >> instr CONS
compileExpr (EIf cond exTrue exFalse) = do
	-- compile conditions
	trueBlock <- block_ "ift" $ do
		compileE exTrue
		instr JOIN
	falseBlock <- block_ "iff" $ do
		compileE exFalse
		instr JOIN
	-- and back to this expression
	compileE cond
	instr (SEL trueBlock falseBlock)
compileExpr (EAp func args) = do
	mapM_ compileE args
	compileE func
	instr (AP $ length args)
compileExpr (ELambda args expr) = do
	-- compile the body of the lambda
	lam <- block_ "lam" $ frame_ args $ do
		compileTE expr
	-- make a closure for it
	instr (LDF lam)
compileExpr (ELet isRec bindings expr) = do
	let vars = map fst bindings
	let exprs = map snd bindings
	let blockName = if isRec then "letrec" else "let"

	-- compile the inner expression
	innerExpr <- block_ blockName $ frame_ vars $ do
		compileTE expr

	-- compile the expressions
	when isRec $ do
		instr (DUM $ length bindings)
		pushFrame (map fst bindings)

	-- compile the bound expressions, then jump to the inner expression
	forM_ exprs $ \boundExpr -> compileE boundExpr
	instr (LDF innerExpr)

	if isRec then instr (RAP $ length bindings) else instr (AP $ length bindings)
compileExpr (EAssign name value result) = compileAssign compileE name value result
compileExpr (ETrace traceExp resultExp) = compileTrace compileE traceExp resultExp
compileExpr (EError s) = instr (LDC n) >> instr BRK >> instr CAR where
	ascii = map fromEnum s
	fourcc = foldl' (\acc x -> acc*256 + x) 0 (take 4 ascii)
	n = fromIntegral fourcc

compileTailExpr :: LExpr -> CompileM ()
compileTailExpr (EIf cond exTrue exFalse) = do
	-- compile conditions
	trueBlock <- block_ "tift" $ do
		compileTE exTrue
	falseBlock <- block_ "tiff" $ do
		compileTE exFalse
	-- and back to this expression
	compileE cond
	instr (TSEL trueBlock falseBlock)
compileTailExpr (EAp func args) = do
	mapM_ compileE args
	compileE func
	instr (TAP $ length args)
compileTailExpr (ELet isRec bindings expr) = do
	let vars = map fst bindings
	let exprs = map snd bindings
	let blockName = if isRec then "tletrec" else "tlet"

	-- compile the inner expression
	innerExpr <- block_ blockName $ frame_ vars $ do
		compileTE expr

	-- compile the expressions
	when isRec $ do
		instr (DUM $ length bindings)
		pushFrame (map fst bindings)

	-- compile the bound expressions, then jump to the inner expression
	forM_ exprs $ \boundExpr -> compileE boundExpr
	instr (LDF innerExpr)

	if isRec then instr (TRAP $ length bindings) else instr (TAP $ length bindings)
compileTailExpr (EAssign name value result) = compileAssign compileTE name value result
compileTailExpr (ETrace traceExp resultExp) = compileTrace compileTE traceExp resultExp

-- in any other case, we can't take advantage of tail-call-optimization
-- so just use the regular compiler and add a RTN to exit the function
compileTailExpr e = compileExpr e >> instr RTN


compileTrace :: (LExpr -> CompileM ()) -> LExpr -> LExpr -> CompileM ()
compileTrace tailCompiler traceExp resultExp = do
	compileE traceExp
	instr DBUG
	tailCompiler resultExp

compileAssign :: (LExpr -> CompileM ()) -> String -> LExpr -> LExpr -> CompileM ()
compileAssign tailCompiler name value result = do
	-- compile the assignment and apply it
	compileE value
	varRef <- lookupVar name
	instr (ST varRef)
	tailCompiler result

compileF :: LFunction -> CompileM Label
compileF func@(LF name args expr) = sourceFunction func $ do
	funcLabel <- block_ name $ frame_ args $ do
		compileTE expr

	return funcLabel

getDuplicates :: [String] -> [String]
getDuplicates names = map fst $ filter (\(a,b) -> a == b) adjacentNames
    where   sortedNames = sort names
            adjacentNames = zip sortedNames (tail sortedNames)

compileAndLink :: LProgram -> Either CompileError [LCInstr EnvAddr Address]
compileAndLink program =
	let functions = lpFunctions program
	    globalNames = map lfName functions
	    duplicateNames = getDuplicates globalNames
	in flip evalStateT (initialCompileState $ globalNames) $ do
		case duplicateNames of
			[] -> return ()
			(x:_xs) -> throwError (DuplicateSymbol x)
		-- look up the label for "main"
		mainIndex <- case lookup (lpMain program) $ zip globalNames [0..] of
			Just n -> return n
			Nothing -> throwError (GlobalNotFound $ lpMain program)

		-- compile all the functions and get their labels
		labels <- mapM compileF functions

		-- build the RAP trampoline for calling main
		trampoline <- block_ "_trampoline" $ do
			-- get the number of argumets of main
			let mainFunction = functions !! mainIndex
			let mainLabel = labels !! mainIndex
			let arguments = lfArguments mainFunction

			-- Load the arguments from the 2nd-level frame
			forM_ (zip [0..] arguments) $ \(argIndex, argName) -> instr (LD $ EnvAddr argName 1 argIndex) -- TODO: this is a nasty hack, probably the linker should do it?

			-- Optional: null out the entry environment to save memory
			forM_ (zip [0..] arguments) $ \(argIndex, argName) -> do
				instr (LDC 0)
				instr (ST $ EnvAddr argName 1 argIndex)

			-- call main
			instr (LDF mainLabel)
			instr (TAP $ length arguments)

		-- build the entry point block
		let numFunctions = length functions
		instr (DUM numFunctions)
		forM_ labels $ \label -> instr (LDF label)
		instr (LDF trampoline)
		instr (TRAP numFunctions) -- Can we tail call here?

		-- Link the program
		compileState <- get
		lift $ link compileState

lookupLabel :: M.Map Label Address -> Label -> Either CompileError Address
lookupLabel m label = case M.lookup label m of
	Just addr -> return addr
	Nothing   -> Left (LinkError $ labelName label, Nothing, Nothing)

-- This is kind of ugly, it could just be traverse (lookupLabel m) if I made LCInstr
-- an instance of Traversable
linkInstr :: M.Map Label Address -> LCInstr VarRef Label -> Either CompileError (LCInstr EnvAddr Address)
linkInstr _ (LDC n) = return $ LDC n
linkInstr _ (LD env) = return $ LD env
linkInstr _ ADD = return $ ADD
linkInstr _ SUB = return $ SUB
linkInstr _ MUL = return $ MUL
linkInstr _ DIV = return $ DIV
linkInstr _ CEQ = return $ CEQ
linkInstr _ CGT = return $ CGT
linkInstr _ CGTE = return $ CGTE
linkInstr _ ATOM = return $ ATOM
linkInstr _ CONS = return $ CONS
linkInstr _ CAR = return $ CAR
linkInstr _ CDR = return $ CDR
linkInstr m (SEL labelTrue labelFalse) = do
	addrTrue <- lookupLabel m labelTrue
	addrFalse <- lookupLabel m labelFalse
	let cmt = "t: " ++ labelName labelTrue ++ "  f: " ++ labelName labelFalse
	return (COMMENT cmt $ SEL addrTrue addrFalse)
linkInstr _ JOIN = return $ JOIN
linkInstr m (LDF label) = do
	addr <- lookupLabel m label
	return (COMMENT (labelName label) (LDF addr))
linkInstr _ (AP n) = return $ AP n
linkInstr _ RTN = return $ RTN
linkInstr _ (DUM n) = return $ DUM n
linkInstr _ (RAP n) = return $ RAP n
linkInstr m (TSEL labelTrue labelFalse) = do
	addrTrue <- lookupLabel m labelTrue
	addrFalse <- lookupLabel m labelFalse
	let cmt = "t: " ++ labelName labelTrue ++ "  f: " ++ labelName labelFalse
	return (COMMENT cmt $ TSEL addrTrue addrFalse)
linkInstr _ (TAP n) = return $ TAP n
linkInstr _ (TRAP n) = return $ TRAP n
linkInstr _ (ST env) = return $ COMMENT (envName env) (ST env)
linkInstr _ DBUG = return $ DBUG
linkInstr _ BRK = return $ BRK
linkInstr m (COMMENT cmt i) = do
	linkedI <- linkInstr m i
	return $ COMMENT cmt linkedI

link :: CompileState -> Either CompileError [LCInstr EnvAddr Address]
link cs = do
	-- figure out the addresses for each block
	let basicBlocks :: [(Label, S.Seq (LCInstr EnvAddr Label))]
	    basicBlocks = sToList ((csLabel cs, csBlock cs) <| csCompletedBlocks cs)
	
	let blockAddresses :: M.Map Label Address
	    blockAddresses = M.fromList $ snd $ mapAccumL (\addr (label, code) -> (addr + S.length code, (label, addr))) 0 basicBlocks

	-- link labels in code blocks
	let convert = linkInstr blockAddresses
	linkedBlocks <- forM basicBlocks $ \(label, code) -> do
		addr <- lookupLabel blockAddresses label
		let instrs = addLabel label addr $ sToList code
		mapM convert instrs

	return (concat linkedBlocks)

addLabel :: Label -> Address -> [LCInstr env addr] -> [LCInstr env addr]
addLabel _label _addr []     = []
addLabel label  addr  (i:is) = COMMENT (show addr ++ ": " ++ labelName label) i : is

---
--- tests
---

test :: CompileM Label
test = compileF $ LmLanguage.untypeFunc $ Lambda_compiler_2.fNth

testP :: Either CompileError [LCInstr EnvAddr Address]
testP = compileAndLink $ Lambda_compiler_2.testProg

printAsm :: Either CompileError [LCInstr EnvAddr Address] -> IO ()
printAsm (Left exception) = print exception
printAsm (Right code) = mapM_ print code
