-- game_controller.hs

module Game_controller
where
import Lambdaman_types
import Ghost_machine

import Control.Monad.State.Lazy
import Data.Maybe
import qualified Data.Vector as V
import Data.Vector (Vector, (!), (//))
import Text.Printf
import Data.List (sort, partition)
--import Debug.Trace

type SimM m a = StateT (GameStateM m) m a

--import Control.Monad.IfElse
whenM :: Monad m => m Bool -> m () -> m ()
whenM test action = test >>= \t -> if t then action else return ()
if' :: Bool -> a -> a -> a
if' p a b = if p then a else b
bool :: a -> a -> Bool -> a
bool b a p = if' p a b
ifM :: Monad m => m Bool -> m a -> m a -> m a 
ifM p a b = p >>= bool b a

--kFRUIT_1_SPAWN, kFRUIT_1_DESPAWN, kFRUIT_2_SPAWN, kFRUIT_2_DESPAWN :: Time
--kFRUIT_1_SPAWN = 127 * 200
--kFRUIT_1_DESPAWN = 127 * 280
--kFRUIT_2_SPAWN = 127 * 400
--kFRUIT_2_DESPAWN = 127 * 480

kFRIGHT_MODE_DURATION :: Time
kFRIGHT_MODE_DURATION = 127 * 20

kPILL_SCORE, kPOWER_PILL_SCORE :: LambdaManScore
kPILL_SCORE = 10
kPOWER_PILL_SCORE = 50

kGHOST_START_DIRECTION, kPLAYER_START_DIRECTION :: GameDirection
kGHOST_START_DIRECTION = DOWN
kPLAYER_START_DIRECTION = DOWN

getTicks :: Monad m => SimM m Time
getTicks = do
	gameData <- getData
	return $ gdTicks gameData

-- getFrightModeCounter :: Monad m => SimM m Time
-- getFrightModeCounter = do
-- 	lambdaManStatus <- getLambdaManStatus
-- 	return $ lmVitality lambdaManStatus

-- setFrightCounter :: Monad m => Time -> SimM m ()
-- setFrightCounter newFrightCounter = do
-- 	time <- getTime 
-- 	let newFrightOverTime = time + newFrightCounter
-- 	removeFrightEvent
-- 	addEvent EVENT_FRIGHT_MODE_END newFrightOverTime
--	gameState <- get
--	gameData <- getData
--	lambdaManStatus <- getLambdaManStatus
--	put gameState { gsGameData = gameData { gdLambdaManStatus = lambdaManStatus { lmVitality = newFrightCounter } } }

setGameEvents :: Monad m => GameEventQueue -> SimM m ()
setGameEvents events = do
	gameState <- get
	gameData <- getData
	put gameState { gsGameData = gameData { gdEventQueue = events } }

removeFrightEvent :: Monad m => SimM m ()
removeFrightEvent = do
	gameData <- getData
	let gameEvents = gdEventQueue gameData
	let newGameEvents = filter notAFrightEvent gameEvents where
		notAFrightEvent gameEvent = (geEvent gameEvent) /= EVENT_FRIGHT_MODE_END
	setGameEvents newGameEvents

executeEnterFrightMode :: Monad m => SimM m () 
executeEnterFrightMode = do
	time <- getTime 
	let newFrightOverTime = time + kFRIGHT_MODE_DURATION
	removeFrightEvent
	addEvent EVENT_FRIGHT_MODE_END newFrightOverTime
	gameState <- get
	gameData <- getData
	lambdaManStatus <- getLambdaManStatus
	put gameState { gsGameData = gameData { gdLambdaManStatus = lambdaManStatus { lmVitality = kFRIGHT_MODE_DURATION } } }

--executeFrightModeDecrement :: Monad m => SimM m ()
--executeFrightModeDecrement = do
--	frightModeCounter <- getFrightModeCounter
--	if (frightModeCounter == 0) then return ()
--	else do
--		let newFrightModeCounter = frightModeCounter-1
--		setFrightCounter newFrightModeCounter
--		if (newFrightModeCounter /= 0) then return ()
--		else do
--			resetGhostChain
--			resetGhostsAfterFright

getMap :: Monad m => SimM m GameMap
getMap = do
	gameData <- getData
	return $ gdGameMap gameData

getGrid :: Monad m => SimM m GameGrid
getGrid = do
	gameMap <- getMap
	return $ gmGrid gameMap

getData :: Monad m => SimM m GameData
getData = do
	gameState <- get
	return $ gsGameData gameState

setMap :: Monad m => GameMap -> SimM m ()
setMap newGameMap = do
	gameState <- get
	gameData <- getData
	put $ gameState { gsGameData = gameData { gdGameMap = newGameMap } }

write :: Vector a -> Int -> a -> Vector a
write vector index element =  vector // [(index, element)]

getFruitSpawnLocation :: Monad m => SimM m GamePosition
getFruitSpawnLocation = do
	gameMap <- getMap
	return $ gmFruitSpawnLocation gameMap

setAtGridPosition :: Monad m => GamePosition -> MapElement -> SimM m ()
setAtGridPosition position newElement = do
	gameMap <- getMap
	let mapIndex = getMapIndex gameMap position
	grid <- getGrid
	let newGrid = write grid mapIndex newElement
	setMap gameMap { gmGrid = newGrid }
	return ()

getAtGridPosition :: Monad m => GamePosition -> SimM m MapElement
getAtGridPosition position = do
	gameMap <- getMap
	let mapIndex = getMapIndex gameMap position
	grid <- getGrid
	return $ grid ! mapIndex

getFruitStatus :: Monad m => SimM m Time
getFruitStatus = do
	gameData <- getData
	return $ gdFruitStatus gameData

setFruitStatus :: Monad m => Time -> SimM m ()
setFruitStatus newFruitStatus = do
	gameState <- get
	gameData <- getData
	put gameState { gsGameData = gameData { gdFruitStatus = newFruitStatus } } 

executeSpawnFruit :: Monad m => SimM m ()
executeSpawnFruit = do
	time <- getTime
	setFruitStatus (time + 127 * 80)
	--setAtGridPosition fruitLocation FRUIT

--executeFruitUpdate :: Monad m => Time -> SimM m ()
--executeFruitUpdate timeDelta = do
--	fruitStatus <- getFruitStatus
--	if (max(0, fruitStatus - time) > 0)
--		then setFruitStatus (fruitStatus - timeDelta)
--		else return ()

-- executeDespawnFruit :: Monad m => SimM m ()
-- executeDespawnFruit = do
-- 	fruitLocation <- getFruitSpawnLocation
-- 	setAtGridPosition fruitLocation EMPTY

--executeSpawnDespawnFruit :: Monad m => SimM m ()
--executeSpawnDespawnFruit = do
--	fruitLocation <- getFruitSpawnLocation
--	setAtGridPosition fruitLocation EMPTY

--executeFruitSpawnOrDespawn :: Monad m => SimM m ()
--executeFruitSpawnOrDespawn = do
--	time <- getTicks
--	case () of 
--		_
--			| (time == kFRUIT_1_SPAWN)	-> executeSpawnFruit 
--			| time == kFRUIT_1_DESPAWN	-> executeDespawnFruit
--			| time == kFRUIT_2_SPAWN	-> executeSpawnFruit
--			| time == kFRUIT_2_DESPAWN	-> executeDespawnFruit
--			| otherwise					-> return ()

--executeActions :: Monad m => SimM m ()
--executeActions = do
--	executeFrightModeDecrement
--	executeFruitSpawnOrDespawn

getWinState :: Monad m => SimM m GameWinState
getWinState = do
	gameData <- getData
	return $ gdWinState gameData

setWinState :: Monad m => GameWinState -> SimM m ()
setWinState newWinState = do
	gameState <- get
	gameData <- getData
	winState <- getWinState
	if (winState == GAME_RUNNING)
		then put $ gameState { gsGameData = gameData { gdWinState = newWinState } }
		else error "tried to set winState to win when from something other than GAME_RUNNING"

countNumPills :: GameMap -> Int
countNumPills gameMap = V.foldr countPill 0 grid where
	grid = gmGrid gameMap
	countPill element = if (element == PILL) 
		then (+1)
		else (+0)

executeGameWin :: Monad m => SimM m ()
executeGameWin = do
	gameMap <- getMap
	let numberPillsRemaining = countNumPills gameMap
	if (numberPillsRemaining == 0)
		then setWinState GAME_WIN
		else return ()

executeGameLoss :: Monad m => SimM m ()
executeGameLoss = do
	winState <- getWinState
	if (winState == GAME_RUNNING) 
		then do
			lives <- getLife
			if (lives == 0)
				then setWinState GAME_LOSS
				else return ()
		else return ()

removeElementAt :: Monad m => GamePosition -> SimM m ()
removeElementAt currentLocation = do
	setAtGridPosition currentLocation EMPTY

removePill, removePowerPill, removeFruit :: Monad m => GamePosition -> SimM m ()
removePill = removeElementAt
removePowerPill = removeElementAt
removeFruit _ = setFruitStatus 0

getLambdaManStatus :: Monad m => SimM m LambdaManStatus
getLambdaManStatus = do
	gameData <- getData
	return $ gdLambdaManStatus gameData

setLambdaManStatus :: Monad m => LambdaManStatus -> SimM m ()
setLambdaManStatus newLambdaManStatus = do
	gameState <- get
	gameData <- getData
	put $ gameState { gsGameData = gameData { gdLambdaManStatus = newLambdaManStatus } }

getScore :: Monad m => SimM m LambdaManScore
getScore = do
	lambdaManStatus <- getLambdaManStatus
	return $ lmScore lambdaManStatus

incrementScore :: Monad m => LambdaManScore -> SimM m ()
incrementScore scoreDelta = do
	score <- getScore
	gameState <- get
	gameData <- getData
	lambdaManStatus <- getLambdaManStatus
	let newScore = score + scoreDelta
	put gameState { gsGameData = gameData { gdLambdaManStatus = lambdaManStatus { lmScore = newScore } } }

getLambdaManLocation :: Monad m => SimM m LambdaManLocation
getLambdaManLocation = do
	lambdaManStatus  <- getLambdaManStatus
	return $ lmLocation lambdaManStatus

hasElement :: Monad m => MapElement -> GamePosition -> SimM m Bool
hasElement testElement position = do
	element <- getAtGridPosition position
	return (element == testElement)

hasPill, hasPowerPill, hasFruit :: Monad m => GamePosition -> SimM m Bool
hasPill loc = hasElement PILL loc
hasPowerPill = hasElement POWER_PILL
hasFruit position = do
	time <- getTime
	fruitPosition <- getFruitSpawnLocation
	fruitStatus <- getFruitStatus
	if (position == fruitPosition && fruitStatus - time > 0) 
		then return True
		else return False

executeLambdaManEatPill :: Monad m => SimM m ()
executeLambdaManEatPill = do
	currentLocation <- getLambdaManLocation
	whenM (hasPill currentLocation) $ do
		removePill currentLocation
		incrementScore kPILL_SCORE

reverseDirection :: GameDirection -> GameDirection
reverseDirection UP = DOWN
reverseDirection DOWN = UP
reverseDirection LEFT = RIGHT
reverseDirection RIGHT = LEFT

reverseGhostDirection :: GhostCPU -> GhostCPU
reverseGhostDirection ghost = ghost { gcGhostDirection = newDirection } where
	newDirection = reverseDirection $ gcGhostDirection ghost

getGhosts :: Monad m => SimM m [GhostCPU]
getGhosts = do
	gameState <- get
	return $ gsGhostCPU gameState

-- getGhosts :: Monad m => SimM m [GhostCPU]
-- getGhosts = return gets gsGhostCPU

clearDebugString :: Monad m => SimM m ()
clearDebugString = do
	gameState <- get
	let newState = gameState { gsDebugString = "", gsLogString = ""  }
	put newState

appendDebugString :: Monad m => String -> SimM m ()
appendDebugString str = do
	gameState <- get
	let oldDebugString = gsDebugString gameState
	let newState = gameState { gsDebugString = oldDebugString ++ str ++ "\n" }
	put newState

appendLogString :: Monad m => String -> SimM m ()
appendLogString str = do
	gameState <- get
	let oldLogString = gsLogString gameState
	let newState = gameState { gsLogString = oldLogString ++ str ++ "\n" }
	put newState

setGhosts :: Monad m => [GhostCPU] -> SimM m ()
setGhosts newGhosts = do
	gameState <- get
	let newState = gameState { gsGhostCPU = newGhosts }
	put newState

reverseGhostsDirections :: Monad m => SimM m ()
reverseGhostsDirections = do
	ghosts <- getGhosts
	let newGhosts = Prelude.map reverseGhostDirection ghosts
	setGhosts newGhosts

updateGhostFrightened :: GhostCPU -> GhostCPU
updateGhostFrightened ghost = results where
	currentVitality = gcGhostVitality ghost
	newVitality = case currentVitality of
		STANDARD -> FRIGHT_MODE
		other    -> other
	results = ghost { gcGhostVitality = newVitality }

setGhostsToFrighteneed :: Monad m => SimM m ()
setGhostsToFrighteneed = do
	ghosts <- getGhosts
	let newGhosts = Prelude.map updateGhostFrightened ghosts
	setGhosts newGhosts

executeLambdaManEatPowerPill :: Monad m => SimM m ()
executeLambdaManEatPowerPill = do
	currentLocation <- getLambdaManLocation
	whenM (hasPowerPill currentLocation) $ do
		removePowerPill currentLocation
		incrementScore kPOWER_PILL_SCORE
		executeEnterFrightMode
		setGhostsToFrighteneed
		reverseGhostsDirections

getMapWidth :: Monad m => SimM m Int
getMapWidth = do
	gameMap <- getMap
	return $ fromIntegral $ gmWidth gameMap

getMapHeight :: Monad m => SimM m Int
getMapHeight = do
	gameMap <- getMap
	return $ fromIntegral $ gmHeight gameMap

getMapLevel :: Monad m => SimM m Int
getMapLevel = do
	width  <- getMapWidth
	height <- getMapHeight
	let size = width * height
	return $ (size-1) `div` 100 + 1

getFruitValue :: Monad m => SimM m Int
getFruitValue = do
	level <- getMapLevel
	let result = case level of
		1 -> 100
		2 -> 300
		3 -> 500
		4 -> 500
		5 -> 700
		6 -> 700
		7 -> 1000
		8 -> 1000
		9 -> 2000
		10 -> 2000
		11 -> 3000
		12 -> 3000
		_  -> 5000
	return result

executeLambdaManEatFruit :: Monad m => SimM m ()
executeLambdaManEatFruit = do 
	currentLocation <- getLambdaManLocation
	whenM (hasFruit currentLocation) $ do
		removeFruit currentLocation
		fruitValue <- getFruitValue
		incrementScore fruitValue

executeLambdaManCollision :: Monad m => SimM m ()
executeLambdaManCollision = do
	executeLambdaManEatPill
	executeLambdaManEatPowerPill
	executeLambdaManEatFruit

inFrightMode :: Monad m => SimM m Bool
inFrightMode = do
	frightCounter <- getLambdaManVitality
	return $ frightCounter > 0

iterateGhosts :: Monad m => (GhostCPU -> SimM m (Maybe GhostCPU)) -> SimM m ()
iterateGhosts loopBody = do
	ghosts <- gets gsGhostCPU
	newGhosts <- forM ghosts $ \ghost -> do
		result <- loopBody ghost
		return $ fromMaybe ghost result
	modify $ \gameState -> gameState { gsGhostCPU = newGhosts }

isInvisible :: GhostCPU -> Bool
isInvisible ghost = (gcGhostVitality ghost) == INVISIBLE

getNumGhosts :: Monad m => SimM m Int
getNumGhosts = do
	ghosts <- getGhosts
	return $ length ghosts

getGhostIndicies :: Monad m => SimM m [Int]
getGhostIndicies = do
	numGhosts <- getNumGhosts
	return [0..(numGhosts-1)]

isGhostVisibleByIdx :: Monad m => Int -> SimM m Bool
isGhostVisibleByIdx ghostIdx = do
	ghost <- getGhost ghostIdx
	let vitality = gcGhostVitality ghost
	return $ vitality /= INVISIBLE

isGhostAtPositionByIdx :: Monad m => GhostLocation -> Int -> SimM m Bool
isGhostAtPositionByIdx location ghostIdx = do
	ghost <- getGhost ghostIdx
	let ghostLocation = gcGhostLocation ghost
	return $ location == ghostLocation

executeGhostCollision :: Monad m => SimM m ()
executeGhostCollision = do
	currentLocation <- getLambdaManLocation
	ghostIndicies <- getGhostIndicies
	visibleGhosts <- filterM isGhostVisibleByIdx ghostIndicies
	colocatedGhosts <- filterM (isGhostAtPositionByIdx currentLocation) visibleGhosts
	if (null colocatedGhosts) then return ()
	else do
		ifM (inFrightMode) 
			(mapM_ executeEatGhost colocatedGhosts)
			(executeEatLambdaMan)

executeEatLambdaMan :: Monad m => SimM m ()
executeEatLambdaMan = do
	appendDebugString (printf "executeEatLambdaMan")
	exeucteResetLambdaMan
	exeucteResetGhosts
	exeucteDecrementLife

getLife :: Monad m => SimM m LambdaManLives
getLife = do
	lambdaManStatus <- getLambdaManStatus
	return $ lmLives lambdaManStatus  

setLife :: Monad m => LambdaManLives -> SimM m ()
setLife newLife = do
	lambdaManStatus <- getLambdaManStatus
	setLambdaManStatus $ lambdaManStatus { lmLives = newLife }

getGhostChain :: Monad m => SimM m Int
getGhostChain = do
	lambdaManStatus <- getLambdaManStatus
	return $ lmGhostChain lambdaManStatus

setGhostChain :: Monad m => Int -> SimM m ()
setGhostChain chain = do
	lambdaManStatus <- getLambdaManStatus
	setLambdaManStatus $ lambdaManStatus { lmGhostChain = chain }

exeucteDecrementLife :: Monad m => SimM m ()
exeucteDecrementLife = do
	--gameState <- get
	--gameData <- getData
	life <- getLife
	setLife $ life-1



--	return $ gameState { gsGameData = gameData { gdGhostChain = ghostChain + 1 } }
--	ghostChain <- getGhostChain
--	return $ gameState { gsGameData = gameData { gdGhostChain = ghostChain + 1 } }

getLambdaManStartingLocation :: Monad m => SimM m LambdaManLocation
getLambdaManStartingLocation = do
	gameMap <- getMap
	return $ gmLambdaManSpawnLocation gameMap

getLambdaManStartingDirection :: Monad m => SimM m GameDirection
getLambdaManStartingDirection = return DOWN

setLambdaManLocationDirection :: Monad m => LambdaManLocation -> GameDirection -> SimM m ()
setLambdaManLocationDirection newLocation newDirection = do
	lambdaManStatus <- getLambdaManStatus
	setLambdaManStatus $ lambdaManStatus { lmLocation = newLocation, lmDirection = newDirection }

exeucteResetLambdaMan :: Monad m => SimM m ()
exeucteResetLambdaMan = do
	startingLocation <- getLambdaManStartingLocation 
	startingDireciton <- getLambdaManStartingDirection
	setLambdaManLocationDirection startingLocation startingDireciton

exeucteResetGhosts :: Monad m => SimM m ()
exeucteResetGhosts = do
	iterateGhosts $ \ghost ->
		return $ Just ghost { gcGhostLocation = gcGhostSpawnLocations ghost, gcGhostDirection = kGHOST_START_DIRECTION }

exeucteResetGhost :: State GhostCPU ()
exeucteResetGhost = 
	modify $ \cpu -> cpu { gcGhostLocation = gcGhostSpawnLocations cpu, gcGhostDirection = kGHOST_START_DIRECTION }

makeGhostInvisible :: State GhostCPU ()
makeGhostInvisible = 
	modify $ \cpu -> cpu { gcGhostVitality = INVISIBLE }


resetGhostChain :: Monad m => SimM m ()
resetGhostChain = do
	lambdaManStatus <- getLambdaManStatus
	setLambdaManStatus $ lambdaManStatus { lmGhostChain = 0 }

resetGhostAfterFright :: GhostCPU -> GhostCPU
resetGhostAfterFright ghost = ghost { gcGhostVitality = STANDARD }

resetGhostsAfterFright :: Monad m => SimM m ()
resetGhostsAfterFright = do
	ghosts <- getGhosts
	let newGhosts = map resetGhostAfterFright ghosts
	setGhosts newGhosts

incrementGhostChain :: Monad m => SimM m ()
incrementGhostChain = do
	ghostChain <- getGhostChain
	setGhostChain (ghostChain + 1)

-- withGhostCPU :: GhostCPU -> (a -> ) -> State GhostCPU ()
-- withGhostCPU = \ghost ->

getGhost :: Monad m => Int -> SimM m GhostCPU
getGhost index = do	
	gameState <- get
	let ghosts = gsGhostCPU gameState
	--let ghosts2 = trace (printf "ghostIndex: %d of %d" index (length ghosts))  ghosts
	return $ ghosts !! index

replaceElement :: [a] -> Int -> a -> [a]
replaceElement xs i newElement = fore ++ (newElement : aft)
  where fore = take i xs
        aft = drop (i+1) xs

setGhost :: Monad m => Int -> GhostCPU -> SimM m ()
setGhost index newGhost = do
	ghosts <- getGhosts
	let newGhosts = replaceElement ghosts index newGhost
	setGhosts newGhosts

getGhostChainScore :: Monad m => SimM m Int
getGhostChainScore = do
	chainIndex <- getGhostChain
	let score = case chainIndex of 
		0 -> 200
		1 -> 400
		2 -> 800
		_ -> 1600
	return score

executeEatGhost :: Monad m => Int -> SimM m ()
executeEatGhost ghostIndex = do
	appendDebugString (printf "executeEatGhost")
	ghost <- getGhost ghostIndex
	let newGhost = execState (do
		makeGhostInvisible 
		exeucteResetGhost) ghost
	setGhost ghostIndex newGhost
	chainScore <- getGhostChainScore
	incrementGhostChain 
	incrementScore chainScore

calcNextLambdaManMoveTimeDelta :: Monad m => SimM m Int
calcNextLambdaManMoveTimeDelta = do
	currentLocation <- getLambdaManLocation
	element <- getAtGridPosition currentLocation
	case element of
		EMPTY -> return 127
		WALL  -> error "lambda man in a wall"
		_     -> return 137

calcNextGhostMoveTimeDelta :: Monad m => Int -> SimM m Int
calcNextGhostMoveTimeDelta ghostIndex = do
	ghost <- getGhost ghostIndex
	let vitality = gcGhostVitality ghost
	let wrappedIndex = ghostIndex `mod` 4
	case vitality of
		STANDARD -> return $ [130,132,134,136] !! wrappedIndex
		_		 -> return $ [195,198,201,204] !! wrappedIndex

getEvents :: Monad m => SimM m GameEventQueue
getEvents = do
	gameData <- getData
	return $ gdEventQueue gameData

setEvents :: Monad m => GameEventQueue -> SimM m ()
setEvents newQueue = do
	gameState <- get
	gameData <- getData
	put gameState { gsGameData = gameData { gdEventQueue = newQueue } }

addEvent :: Monad m => GameEventType -> Time -> SimM m ()
addEvent eventType eventTime = do
	appendDebugString $ printf "add event: %s time: %d" (show eventType) eventTime
	let newEvent = GameEvent { geTime = eventTime, geEvent = eventType }
	events <- getEvents 
	let newEvents = newEvent : events
	setEvents newEvents


--setNextLambdaManMoveTime :: Monad m => Int -> SimM m ()
--setNextLambdaManMoveTime moveTime = do
	--addEvent EVENT_LAMBDAMAN_MOVE moveTime
	--lambdaManStatus <- getLambdaManStatus
	--setLambdaManStatus $ lambdaManStatus { lmNextMoveTime = moveTime }

getTime :: Monad m => SimM m Time
getTime = do
	gameData <- getData
	return $ gdTicks gameData

setTime :: Monad m => Time -> SimM m ()
setTime time = do
	gameState <- get
	gameData <- getData
	put gameState { gsGameData = gameData { gdTicks = time } }

--getNextLambdaManMoveTime :: Monad m => SimM m Time
--getNextLambdaManMoveTime = do
--	lambdaManStatus <- getLambdaManStatus
--	return $ lmNextMoveTime lambdaManStatus

-- ghost_execute_game_cycle :: GhostCPU -> GameState -> GhostExecutionState

--tryRunLambdaManProgram :: Monad m => SimM m (Maybe LambdaManMove)
--tryRunLambdaManProgram = do
--	time <- getTime 
--	nextLambdaManMoveTime <- getNextLambdaManMoveTime
--	if (time == nextLambdaManMoveTime) 
--		then do
--			nextMoveDelta <- calcNextLambdaManMoveTimeDelta
--			setNextLambdaManMoveTime (time + nextMoveDelta)
--			return $ Just DOWN
--		else
--			return Nothing

--getNextGhostMoveTime :: Monad m => Int -> SimM m Time
--getNextGhostMoveTime ghostIndex = do
--	ghost <- getGhost ghostIndex
--	return $ gcNextMoveTime ghost

--setNextGhostMoveTime :: Monad m => Int -> Time -> SimM m ()
--setNextGhostMoveTime ghostIndex newTime = do
--	addEvent (EVENT_GHOST_MOVE ghostIndex) newTime
	--ghost <- getGhost ghostIndex
	--let newGhost = ghost { gcNextMoveTime = newTime }
	--setGhost ghostIndex newGhost


-- tryRunGhostProgram :: Monad m => Int -> SimM m (Maybe GameDirection)
-- tryRunGhostProgram ghostIndex = do
-- 	time <- getTime 
-- 	nextGhostMoveTime <- getNextGhostMoveTime ghostIndex
-- 	if (time == nextGhostMoveTime)
-- 		then do
-- 			nextMoveDelta <- calcNextGhostMoveTimeDelta ghostIndex
-- 			setNextGhostMoveTime ghostIndex (time + nextMoveDelta)
-- 			ghost <- getGhost ghostIndex
-- 			gameState <- get
-- 			let ghostExecutionState = ghost_execute_game_cycle ghost gameState
-- 			let newGhost = gesGhostCPU ghostExecutionState
-- 			setGhost ghostIndex newGhost
-- 			let ghostMove = gesOutputDirection ghostExecutionState
-- 			return $ Just ghostMove
-- 		else 
-- 			return Nothing

-- tryRunGhostsPrograms :: Monad m => SimM m ([Maybe GameDirection])
-- tryRunGhostsPrograms = do
-- 	ghostIndicies <- getGhostIndicies
-- 	mapM tryRunGhostProgram ghostIndicies

executeLambdaManMove :: Monad m => LambdaManMove -> SimM m ()
executeLambdaManMove lambdaManMove = do
	currentLocation <- getLambdaManLocation 
	let newLocation = moveInDirection currentLocation lambdaManMove
	element <- getAtGridPosition newLocation
	appendDebugString (printf "lambdaman move: pos:%s programDir:%s nextElement:%s" (show currentLocation) (show lambdaManMove) (show element))
	if (element /= WALL)
		then setLambdaManLocationDirection newLocation lambdaManMove
		else return ()

getOffset :: GameDirection -> GamePosition
getOffset UP = GamePosition {x=0,y= -1}
getOffset DOWN = GamePosition {x=0,y=1}
getOffset LEFT = GamePosition {x= -1,y=0}
getOffset RIGHT = GamePosition {x=1,y=0}

allDirs :: [GameDirection]	
allDirs = [minBound..maxBound]

getValidDirs :: Monad m => GamePosition -> SimM m [GameDirection]
getValidDirs position = do
	-- element <- getAtGridPosition position
	filterM (canStep position) allDirs

addPosition :: GamePosition -> GamePosition -> GamePosition 
addPosition a b = retval where
	retval_x = (x a) + (x b)
	retval_y = (y a) + (y b) 
	retval = GamePosition { x = retval_x, y = retval_y }

subtractPosition :: GamePosition -> GamePosition -> (Int, Int)
subtractPosition a b = retval where
	retval_x = (fromIntegral $ x a) - (fromIntegral $ x b)
	retval_y = (fromIntegral $ y a) - (fromIntegral $ y b) 
	retval = (retval_x, retval_y)

moveInDirection :: GamePosition -> GameDirection -> GamePosition
moveInDirection position direction = newPosition where
	dirOffset = getOffset direction
	newPosition = addPosition position dirOffset

canStep :: Monad m => GamePosition -> GameDirection -> SimM m Bool
canStep position direction = do
	let newPosition = moveInDirection position direction
	element <- getAtGridPosition newPosition
	return $ element /= WALL

notOpposite :: GhostDirection -> GhostDirection -> Bool
notOpposite direction testDir = (direction == reverseDirection testDir)

calcLegalGhostMove :: Monad m => GhostLocation -> GhostDirection -> GhostDirection -> SimM m (Maybe GhostDirection)
calcLegalGhostMove currentLocation currentDir ghostMoveDir = do
	validDirs <- getValidDirs currentLocation
	if (null validDirs) 
		then return Nothing
		else if (elem ghostMoveDir validDirs && currentDir /= reverseDirection ghostMoveDir)
			then do
				--appendDebugString "ghost move valid\n"
				return $ Just ghostMoveDir
			else do 
				let nonOppositeDirs = filter (/= reverseDirection currentDir) validDirs
				--appendDebugString $ printf "ghost move invalid, picking %s\n" (show nonOppositeDirs)
				if (null nonOppositeDirs) 
					then return $ Just $ head validDirs
					else return $ Just $ head nonOppositeDirs

-- executeGhostMoves :: Monad m => [Maybe GameDirection] -> SimM m ()
-- executeGhostMoves ghostMoves = do
-- 	let enumeratedGhostMoves = zip ghostMoves [0..]
-- 	mapM_ executeGhostMove enumeratedGhostMoves

getGhostLocation :: Monad m => Int -> SimM m GhostLocation
getGhostLocation index = do
	ghost <- getGhost index
	return $ gcGhostLocation ghost

getGhostDirection :: Monad m => Int -> SimM m GhostDirection
getGhostDirection index = do
	ghost <- getGhost index
	return $ gcGhostDirection ghost

setGhostLocationDirection :: Monad m => Int -> GhostLocation -> GhostDirection -> SimM m ()
setGhostLocationDirection index newLocation newDirection = do
	ghost <- getGhost index
	let newGhost = ghost { gcGhostLocation = newLocation, gcGhostDirection = newDirection }
	setGhost index newGhost

executeGhostMove :: Monad m => Int -> GameDirection -> SimM m ()
executeGhostMove ghostIndex ghostMove = do
	currentLocation <- getGhostLocation ghostIndex
	currentDirection <- getGhostDirection ghostIndex
	maybeLegalMoveDirection <- calcLegalGhostMove currentLocation currentDirection ghostMove
	appendDebugString $ printf "ghost move: %s currentDir:%s programDir:%s legalDir:%s\n" (show currentLocation) (show currentDirection) (show ghostMove) (show maybeLegalMoveDirection)
	case maybeLegalMoveDirection of 
		Nothing -> return ()
		Just legalMoveDirection -> do
			let newLocation = moveInDirection currentLocation legalMoveDirection
			setGhostLocationDirection ghostIndex newLocation legalMoveDirection

executeIncrementTick :: Monad m => Time -> SimM m ()
executeIncrementTick timeDelta = do 
	time <- getTime 
	setTime (time+timeDelta)

getEndTime :: Monad m => SimM m Int
getEndTime = do
	mapWidth  <- getMapWidth
	mapHeight <- getMapHeight
	return $ 127 * mapWidth * mapHeight * 16


--tryRunOutOfTime :: Monad m => SimM m ()
--tryRunOutOfTime = do
--	time <- getTime
--	endTime <- getEndTime
--	if (time == endTime)
--		then 
--			setLife 0
--		else
--			return ()

executeGameTimeout :: Monad m => SimM m ()
executeGameTimeout = do
	setLife 0

executeFrightModeEnd :: Monad m => SimM m ()
executeFrightModeEnd = do
	resetGhostChain
	resetGhostsAfterFright

runLambdaManProgram :: Monad m => LambdaManProgram m -> GameState -> m (LambdaManProgram m, GameDirection)
runLambdaManProgram (MkProgram programState stepFunction showFunction) gameState = do
	(newState, move) <- stepFunction programState gameState
	let program = MkProgram newState stepFunction showFunction
	return (program, move)

setLambdaManProgram :: Monad m => LambdaManProgram m -> SimM m ()
setLambdaManProgram newProgram = do 
	gameState <- get
	put gameState { gsLambdaManProgram = newProgram }

executeEvent :: Monad m => GameEventType -> SimM m ()
executeEvent EVENT_GAME_TIMEOUT				= executeGameTimeout
executeEvent EVENT_LAMBDAMAN_MOVE			= do
	programState <- gets gsLambdaManProgram 
	gameState <- get
	(newProgramState, move) <- lift $ runLambdaManProgram programState (freezeGameState gameState)
	setLambdaManProgram newProgramState
	executeLambdaManMove move
	time <- getTime
	duration <- calcNextLambdaManMoveTimeDelta
	addEvent EVENT_LAMBDAMAN_MOVE (time+duration)
executeEvent (EVENT_GHOST_MOVE ghostIndex)	= do
	ghost <- getGhost ghostIndex
	gameState <- gets freezeGameState
	let ghostExecutionState = ghost_execute_game_cycle ghost gameState

	appendDebugString (gesErrorMsg ghostExecutionState)
	appendLogString (gesDebugMsg ghostExecutionState)

	let newGhost = gesGhostCPU ghostExecutionState
	setGhost ghostIndex newGhost
	let ghostMove = gesOutputDirection ghostExecutionState
	executeGhostMove ghostIndex ghostMove
	time <- getTime
	duration <- calcNextGhostMoveTimeDelta ghostIndex
	addEvent (EVENT_GHOST_MOVE ghostIndex) (time+duration)
executeEvent EVENT_FRUIT_SPAWN				= executeSpawnFruit
executeEvent EVENT_FRUIT_DESPAWN			= return () --executeDespawnFruit
executeEvent EVENT_FRIGHT_MODE_END			= executeFrightModeEnd

executeEvents :: Monad m => [GameEventType] -> SimM m ()
executeEvents nextEvents = mapM_ executeEvent nextEvents 

getLambdaManVitality :: Monad m => SimM m Int
getLambdaManVitality = do
	lambdaManStatus <- getLambdaManStatus
	return $ lmVitality lambdaManStatus

setLambdaManVitality :: Monad m => Int -> SimM m ()
setLambdaManVitality newVitality = do
	lambdaManStatus <- getLambdaManStatus
	setLambdaManStatus lambdaManStatus { lmVitality = newVitality } 

executeLambdaManVitalityUpdate :: Monad m => Time -> SimM m ()
executeLambdaManVitalityUpdate timeElapsed = do
	vitality <- getLambdaManVitality
	if (vitality > 0)
		then setLambdaManVitality $ vitality - timeElapsed
		else return ()

getLowestTime :: GameEventQueue -> Int
getLowestTime events = result where
	times = map geTime events
	result = minimum times

stripTime :: GameEventQueue -> [GameEventType]
stripTime = map geEvent

getNextEvents :: Monad m => SimM m (Time, [GameEventType])
getNextEvents = do
	eventQueue <- getEvents
	let lowestTime = getLowestTime eventQueue
	let (allOfLowestTime, restOfElements) = partition (\event -> (geTime event) == lowestTime) eventQueue
	let eventsOfLowestType = stripTime allOfLowestTime
	setEvents restOfElements
	return (lowestTime, sort eventsOfLowestType)

runTick :: Monad m => GameStateM m -> m (GameStateM m)
runTick = execStateT $ do
	winState <- getWinState
	if (winState /= GAME_RUNNING) then error "running a tick when game isn't running"
	else do
		clearDebugString
		timeStart <- getTime
		(newTime, nextEvents) <- getNextEvents
		let timeElapsed = newTime - timeStart
		executeIncrementTick timeElapsed
		appendDebugString $ printf "dt: %d\n" timeElapsed
		mapM_ (\event -> appendDebugString (printf "event: %s" (show event))) nextEvents

		executeEvents nextEvents


		--lambdaManMove <- tryRunLambdaManProgram
		--executeLambdaManMove lambdaManMove
		--ghostMoves <- tryRunGhostsPrograms
		--executeGhostMoves ghostMoves
		--executeActions

		executeLambdaManVitalityUpdate timeElapsed
		--executeFruitUpdate timeElapsed

		executeLambdaManCollision
		executeGhostCollision
		executeGameWin
		executeGameLoss

