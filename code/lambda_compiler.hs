{-# LANGUAGE BangPatterns #-}
module Lambda_compiler(
	LError(..),
	LInterp,

	LInstruction(..),
	LCode,
	LFrame,
	LEnvironment,
	LAtom(..),
	LControl(..),
	LMachine(..),

	run,
	step,
	initialMachine,

	toLambdaMachine
	
) where
import Data.Int
import qualified Data.Vector as V
import Data.Vector (Vector, (!?))
import Control.Monad.State
import qualified Lambdaman_types as LT


run :: LMachine -> Either LError LMachine
run m = execStateT loop $ m {lmStopped = False} where
	loop = do
		stopped <- step
		when (not stopped) loop
    
data LError =
	ENoInstructions
	| EFrameOutOfRange
	| ELocalOutOfRange
	| EStackUnderflow
	| EDataError
	| EControlError
	| EDivByZero
	| ENotImplemented
	| ESimulationLogicError
	deriving (Show, Eq)

type LInterp a = StateT LMachine (Either LError) a
-- type LInterp a = Either LError a

throwError :: LError -> LInterp a
throwError = lift .Left

updateMachine :: (LMachine -> LMachine) -> LInterp ()
updateMachine = Control.Monad.State.modify

(?>) :: Maybe a -> LError -> LInterp a
Just a  ?> _ = return a
Nothing ?> e = throwError e
infix 0 ?>

data LInstruction =
	LDC Int32
	| LD Int Int		-- TODO: replace with variable name?
	| ADD | SUB | MUL | DIV
	| CEQ | CGT | CGTE
	| ATOM | CONS
	| CAR | CDR
	| SEL LCode LCode
	| JOIN
	| LDF LCode
	| AP Int
	| RTN
	| DUM Int
	| RAP Int
	| DumRap LCode [LCode]
	-- combination of DUM, many LDFs, RAP. note we don't support putting other atoms into the recursive environment at the moment
	| STOP
	deriving (Show, Eq)

type LCode = [LInstruction]
type LFrame = Vector LAtom
type LEnvironment = [LFrame]

data LAtom =
	LInt !Int32
	| LCons LAtom LAtom
	| LClosure LCode LEnvironment
	deriving (Show, Eq)

data LControl =
	LJoin LCode
	| LRet LCode LEnvironment
	| LStop

data LMachine = LMachine
	{ lmCode :: LCode
	, lmData :: [LAtom]
	, lmControl :: [LControl]
	, lmEnv :: LEnvironment
	, lmStopped :: Bool
	}

initialMachine :: LCode -> LEnvironment -> LMachine
initialMachine code env = LMachine code [] [LStop] env False

popI :: LInterp LInstruction
popI = do
	m <- get
	let iStack = lmCode m
	case iStack of
		[] -> throwError ENoInstructions
		(i:is) -> put (m { lmCode = is }) >> return i

pushD :: LAtom -> LInterp ()
pushD val = updateMachine $ \m -> m { lmData = val : lmData m }

popD :: LInterp LAtom
popD = do
	m <- get
	let dStack = lmData m
	case dStack of
		[] -> throwError EStackUnderflow
		(d:ds) -> put (m { lmData = ds }) >> return d

pushC :: LControl -> LInterp ()
pushC val = updateMachine $ \m -> m { lmControl = val : lmControl m }

popC :: LInterp LControl
popC = do
	m <- get
	let cStack = lmControl m
	case cStack of
		[] -> throwError EStackUnderflow
		(c:cs) -> put (m { lmControl = cs }) >> return c

atomToInt :: LAtom -> Maybe Int32
atomToInt (LInt n) = Just n
atomToInt _        = Nothing

atomToCons :: LAtom -> Maybe (LAtom, LAtom)
atomToCons (LCons car cdr) = Just (car,cdr)
atomToCons _               = Nothing

atomToClosure :: LAtom -> Maybe (LCode, LEnvironment)
atomToClosure (LClosure code env) = Just (code, env)
atomToClosure _                   = Nothing

-- returns if the machine is stopped
step :: LInterp Bool
step = do
	stopped <- gets lmStopped
	when (not stopped) $ do
		instr <- popI
		stepI instr
	gets lmStopped

getFrame :: Int -> LInterp LFrame
getFrame frameIndex
	| frameIndex < 0 = throwError EFrameOutOfRange
	| otherwise      = do
		env <- gets lmEnv
		go frameIndex env ?> EFrameOutOfRange
	where
		go 0 (f:_)  = return f
		go _ []     = Nothing
		go n (_:fs) = go (n-1) fs
    
intOp :: (Int32 -> Int32 -> LInterp Int32) -> LInterp ()
intOp f = do
	atomY <- popD
	atomX <- popD
	numY <- atomToInt atomY ?> EDataError
	numX <- atomToInt atomX ?> EDataError
	!result <- f numX numY
	pushD (LInt result)

stepI :: LInstruction -> LInterp ()
stepI (LDC n)       = pushD (LInt n)
stepI (LD foff v)   = do
	frame <- getFrame foff
	atomVar <- (frame !? v) ?> ELocalOutOfRange
	pushD atomVar
stepI ADD           = intOp (\x y -> return (x+y))
stepI SUB           = intOp (\x y -> return (x-y))
stepI MUL           = intOp (\x y -> return (x*y))
stepI DIV			= intOp (\x y -> if y == 0 then throwError EDivByZero else return (div x y))
stepI CEQ			= intOp (\x y -> if x == y then return 1 else return 0)
stepI CGT           = intOp (\x y -> if x > y then return 1 else return 0)
stepI CGTE          = intOp (\x y -> if x >= y then return 1 else return 0)
stepI ATOM          = do
	atom <- popD
	let !result = case atom of
		LInt _ -> 1
		_      -> 0
	pushD (LInt result)
stepI CONS          = do
	atomCdr <- popD
	atomCar <- popD
	pushD (LCons atomCar atomCdr)
stepI CAR           = do
	atom <- popD
	(atomCar,_) <- atomToCons atom ?> EDataError
	pushD atomCar
stepI CDR           = do
	atom <- popD
	(_,atomCdr) <- atomToCons atom ?> EDataError
	pushD atomCdr
stepI (SEL codeTrue codeFalse) = do
    -- get condition
	atom <- popD
	num <- atomToInt atom ?> EDataError
    -- save return "address"
	joinCode <- gets lmCode
	pushC (LJoin joinCode)
	-- "jump" to new block
	updateMachine $ \m -> m { lmCode = (if num == 0 then codeFalse else codeTrue) }
stepI JOIN = do
	control <- popC
	case control of
		LJoin code -> updateMachine $ \m -> m { lmCode = code }
		_          -> throwError EControlError
stepI (LDF code) = do
	-- get environment
	env <- gets lmEnv
	pushD (LClosure code env)
stepI (AP numArgs) = do
    -- get the function to call
	atom <- popD
	(funcCode,funcEnv) <- atomToClosure atom ?> EDataError
	newFrame <- liftM V.reverse $ V.replicateM numArgs popD
	-- save the current environment
	returnCode <- gets lmCode
	returnEnv <- gets lmEnv
	pushC (LRet returnCode returnEnv)
	-- jump to new target
	updateMachine $ \m -> m { lmCode = funcCode, lmEnv = newFrame : funcEnv }
stepI RTN = do
	control <- popC
	case control of
		LStop -> updateMachine $ \m -> m { lmStopped = True }
		LRet code env -> updateMachine $ \m -> m { lmCode = code, lmEnv = env }
		_ -> throwError EControlError
stepI (DUM _) = throwError ENotImplemented
stepI (RAP _) = throwError ENotImplemented
    -- These mutate a frame in-place, which I don't have support for.
stepI (DumRap toCall funcs) = do
	let numFuncs = length funcs
	returnEnv <- gets lmEnv
	-- add closures to environment
	let newFrame = V.generate numFuncs $ \idx -> LClosure (funcs !! idx) (newFrame : returnEnv)
	-- save the current environment
	returnCode <- gets lmCode
	pushC (LRet returnCode returnEnv)
	-- jump to new target
	updateMachine $ \m -> m { lmCode = toCall, lmEnv = newFrame : returnEnv }
stepI STOP = updateMachine $ \m -> m { lmStopped = True }

toLambdaMachine :: Monad m => LCode -> LT.GameState -> LT.LambdaManProgram m
toLambdaMachine code initialState = case result of
	Left lerror -> LT.MkProgram () (\_ _ -> return ((), LT.RIGHT)) (\_ -> "LMachine:InitError: " ++ show lerror)
	Right program -> program
	where
		-- run the code and get the initial state
		initialFrame = V.fromList [encodeGameState initialState, LInt 0]
		startMachine = initialMachine code [initialFrame]
		result = flip evalStateT startMachine $ do
			-- run until the first halt
			StateT $ \s -> fmap (\res -> ((), res)) (run s)
			
			-- read the top of the stack and unpack it
			initializeResult <- popD
			(initialAiState, stepFn) <- atomToCons initializeResult ?> ESimulationLogicError
			
			-- make sure the stepFn is a closure
			(stepCode, stepEnv) <- case stepFn of
				(LClosure c e) -> return (c,e)
				_ -> throwError ESimulationLogicError

			let go aiState gameState = case goResult of
				Left _ -> (aiState, LT.RIGHT)
				Right answer -> answer
				where
					-- build initial machine for this run
					callFrame = V.fromList [aiState, encodeGameState gameState]
					goMachine = initialMachine stepCode (callFrame : stepEnv)
					goResult = flip evalStateT goMachine $ do
						-- run until halt
						StateT $ \s -> fmap (\res -> ((), res)) (run s)

						-- read the return value from the top of the stack and unpack it
						stepResult <- popD
						(newAiState, direction) <- atomToCons stepResult ?> ESimulationLogicError

						-- convert the direction
						let ltDir = case direction of
							LInt 0 -> LT.UP
							LInt 1 -> LT.RIGHT
							LInt 2 -> LT.DOWN
							LInt 3 -> LT.LEFT
							_      -> LT.RIGHT  -- invalid, just return RIGHT

						return (newAiState, ltDir)

			return $ LT.MkProgram initialAiState (\aiState gameState -> return $ go aiState gameState) (\_ -> "LMachine") -- TODO: Implement a better Show function here

encodeGameState :: LT.GameState -> LAtom
encodeGameState gs = encodeTuple [encodedMap, encodedLambda, encodedGhosts, encodedFruit] where
	encodedMap = encodeMap (LT.gdGameMap $ LT.gsGameData gs)
	encodedLambda = encodeLambdaMan (LT.gdLambdaManStatus $ LT.gsGameData $ gs)
	encodedGhosts = encodeList encodeGhost $ LT.gsGhostCPU gs
	encodedFruit = LInt 0 -- TODO

encodeMap :: LT.GameMap -> LAtom
encodeMap gameMap = encodeList (encodeList encodeGridElem) $ LT.mapToGridList gameMap where
	encodeGridElem element = LInt (fromIntegral $ fromEnum element)

encodePosition :: LT.GamePosition -> LAtom
encodePosition pos = LCons (encodeInt $ LT.x pos) (encodeInt $ LT.y pos)

encodeEnum :: Enum a => a -> LAtom
encodeEnum = LInt . fromIntegral . fromEnum

encodeInt :: Integral a => a -> LAtom
encodeInt = LInt . fromIntegral

encodeLambdaMan :: LT.LambdaManStatus -> LAtom
encodeLambdaMan lm = encodeTuple
	[ encodeInt $ LT.lmVitality lm
	, encodePosition $ LT.lmLocation lm
	, encodeEnum $ LT.lmDirection lm
	, encodeInt $ LT.lmLives lm
	, encodeInt $ LT.lmScore lm
	]

encodeList :: (a -> LAtom) -> [a] -> LAtom
encodeList f = foldr (\x xs -> LCons (f x) xs) (LInt 0)

encodeGhost :: LT.GhostCPU -> LAtom
encodeGhost ghost = encodeTuple
	[ encodeEnum $ LT.gcGhostVitality ghost
	, encodePosition $ LT.gcGhostLocation ghost
	, encodeEnum $ LT.gcGhostDirection ghost
	]

encodeTuple :: [LAtom] -> LAtom
encodeTuple [] = LInt 0
encodeTuple [x] = x
encodeTuple (x:xs) = LCons x (encodeTuple xs)

