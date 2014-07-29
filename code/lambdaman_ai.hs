-- lambdaman_ai.hs

module Lambdaman_ai
where

import Lambdaman_types
import Game_controller

import Control.Monad.State.Lazy
--import Data.Vector (fromList, findIndex, findIndices, toList, replicate)
import qualified Data.Vector as V
import Data.List (minimumBy)
import Data.Ord
import Data.Maybe
import qualified Data.Set as S
--import Text.Regex.Posix
import Data.Graph.AStar
import Debug.Trace
--import Text.Printf



aiCurrentLambdaMan :: LambdaManAI programState
aiCurrentLambdaMan = aiHunter
--aiCurrentLambdaMan = aiWalkToNearestPillOrFruit
-----------------



type LambdaManWorldInfo = GameState
type LambdaManAI programState = programState -> LambdaManWorldInfo -> (programState, GameDirection)


mapIsOccupied :: GameMap -> GamePosition -> Bool
mapIsOccupied gameMap position = result where
	mapIndex = getMapIndex gameMap position
	grid = gmGrid gameMap
	element = grid V.! mapIndex
	result = (element == WALL)

getGhostPositions :: State GameState [GamePosition]
getGhostPositions = do
	ghosts <- getGhosts
	let ghostPositions = map gcGhostLocation ghosts
	return ghostPositions

getDistToNearestGhost :: GamePosition -> State GameState Int
getDistToNearestGhost coord = do
	ghostPositions <- getGhostPositions
	let ghostDistances = map (manhattanDistance coord) ghostPositions
	let minDist = minimum ghostDistances
	return minDist

getDistToNearestVisibleGhost :: GamePosition -> State GameState (Maybe Int)
getDistToNearestVisibleGhost coord = do
	ghosts <- getGhosts
	let visibleGhosts = filter (\ghost -> INVISIBLE /= gcGhostVitality ghost) ghosts
	if (null visibleGhosts) 
		then return Nothing
		else do 
			let ghostPositions = map gcGhostLocation visibleGhosts
			let ghostDistances = map (manhattanDistance coord) ghostPositions
			let minDist = minimum ghostDistances
			return $ Just minDist

getDistToNearestVisibleGhostPathfind :: GamePosition -> State GameState (Maybe Int)
getDistToNearestVisibleGhostPathfind coord = do
	ghosts <- getGhosts
	let visibleGhosts = filter (\ghost -> INVISIBLE /= gcGhostVitality ghost) ghosts
	if (null visibleGhosts) 
		then return Nothing
		else do 
			let ghostPositions = map gcGhostLocation visibleGhosts
			ghostDistances <- mapM (pathfindDistance coord) ghostPositions
			let minDist = minimum ghostDistances
			return $ Just minDist

getDistToAllGhostPathfind :: GamePosition -> State GameState Int
getDistToAllGhostPathfind coord = do
	ghosts <- getGhosts
	let ghostPositions = map gcGhostLocation ghosts
	ghostDistances <- mapM (pathfindDistance coord) ghostPositions
	let scaledGhostsDistances = map (\n -> n + n `div` 4 + n `div` 8) ghostDistances
	return $ sum scaledGhostsDistances

getDistToPlayer :: GamePosition -> State GameState Int
getDistToPlayer coord = do
	playerCoord <- getLambdaManLocation
	let dist = manhattanDistance playerCoord coord
	return dist

pathfindDistance :: GamePosition -> GamePosition -> State GameState Int
pathfindDistance coordA coordB = do
	if (coordA == coordB)
		then return 0
		else do
			gameMap <- getMap
			let path = pathfind coordA coordB (mapIsOccupied gameMap)
			case path of 
				[] -> return 1000
				_  -> return $ length path
		
getDistToPill :: GamePosition -> State GameState Int
getDistToPill coord = do
	pillCoord <- getNearestPill coord
	if (coord == pillCoord) 
		then return 0
		else do 
			dist <- pathfindDistance pillCoord coord
			return dist

getDistToPowerPill :: GamePosition -> State GameState (Maybe Int)
getDistToPowerPill coord = do
	maybePillCoord <- getNearestPowerPill coord
	case maybePillCoord of 
		Nothing			-> return Nothing
		Just pillCoord	-> if (coord == pillCoord) 
			then return $ Just 0
			else do 
				dist <- pathfindDistance pillCoord coord
				return $ Just dist

aiNearest :: programState -> GameState -> (programState, GameDirection)
aiNearest = aiGreedy $ \coord -> do
	playerPosition <- getLambdaManLocation
	playerDistToGhost <- getDistToNearestGhost playerPosition
	element <- getAtGridPosition coord
	distToGhost  <- getDistToNearestGhost coord
	distToPlayer <- getDistToPlayer coord
	--distanceToPill <- getDistToPill coord

	if (playerDistToGhost < 3)
		then if (distToGhost > playerDistToGhost)
			then return $ 2 * distToPlayer - distToGhost
			else return 1000
		else if (element /= PILL && element /= POWER_PILL)
			then return 1000
			else if (distToGhost < 6)
				then return $ distToPlayer + (6 - distToGhost) * 2
				else return $ distToPlayer 

getDistToFruit :: GamePosition -> State GameState (Maybe Int)
getDistToFruit coord = do
	time <- getTime
	fruitStatus <- getFruitStatus
	fruitLocation <- getFruitSpawnLocation
	if (fruitStatus - time > 0)
		then do 
			dist <- pathfindDistance fruitLocation coord
			return $ Just dist
		else return Nothing

getDistanceToBest :: GamePosition -> State GameState Int
getDistanceToBest coord = do
	maybePowerPill  <- getDistToPowerPill coord
	maybeFruit		<- getDistToFruit coord
	distanceToPill  <- getDistToPill coord
	distanceToVisibleGhost <- getDistToNearestVisibleGhostPathfind coord
	let distanceToGhost = fromMaybe 500 distanceToVisibleGhost
	totalDistanceToGhosts <- getDistToAllGhostPathfind coord
	let distanceToPowerPill = fromMaybe 500 maybePowerPill
	let distanceToFruit = fromMaybe 500 maybeFruit
	numGhosts <- getNumGhosts

	dist <-
		if distanceToPowerPill <  10	then return $ -100 + distanceToPowerPill
		else if distanceToFruit < 10	then return $ -50 + distanceToFruit
		else return $ distanceToPill

	let caledDist = 8 * (dist + 4 * max 0 (8 - distanceToGhost)) - totalDistanceToGhosts `div` numGhosts
	return $ caledDist

	--return $ case (maybePowerPill, maybeFruit) of
	--	(Just pill, _)	-> pill
	--	(_, Just fruit) -> fruit
	--	_				-> distanceToPill

getDistanceToBestSafe :: GamePosition -> State GameState Int
getDistanceToBestSafe coord = do
	maybeFruit		<- getDistToFruit coord
	distanceToPill  <- getDistToPill coord
	let dist = case maybeFruit of
		Just fruit	-> fruit
		_			-> distanceToPill

	totalDistanceToGhosts <- getDistToAllGhostPathfind coord
	numGhosts <- getNumGhosts

	return $ 8 * dist - totalDistanceToGhosts `div` numGhosts



myTrace :: String -> Int -> Int
--myTrace str val = trace str val
myTrace _str val = val

aiHunter ::LambdaManAI programState
aiHunter = aiGreedy $ \coord -> do
	--element <- getAtGridPosition coord
	playerPosition <- getLambdaManLocation
	playerDistToGhost <- getDistToNearestGhost playerPosition
	coordDistToVisibleGhost  <- getDistToNearestVisibleGhost coord
	let coordDistToGhost = fromMaybe 50000 coordDistToVisibleGhost 
	distToPlayer <- getDistToPlayer coord
	--maybeDistToFruit  <- getDistToFruit coord
	--let distToFruit = fromMaybe 1000 maybeDistToFruit 
	--maybeDistPlayerToFruit <- getDistToFruit playerPosition
	--let distPlayerToFruit = fromMaybe 1000 maybeDistPlayerToFruit 

	--maybeDistPlayerToPowerPill  <- getDistToPowerPill playerPosition
	--let distPlayerToPowerPill = fromMaybe 1000 maybeDistPlayerToPowerPill

	frightModeRemaining <- getLambdaManVitality
	
	if (distToPlayer == 0) 
	then return 1000
	else do
		let pillDistanceValue = 
			if (playerDistToGhost < 4)
			then if (distToPlayer == 1)
				then do
					dist <- getDistanceToBest coord
					distanceToVisibleGhost <- getDistToNearestVisibleGhostPathfind coord
					let distToGhost = fromMaybe 50000 distanceToVisibleGhost 
					if (distToGhost <= playerDistToGhost)
						then return $ myTrace "50000+bestDist" $ 50000 + dist
						else return $ myTrace "bestDist" $ dist
				else return 100000
			else if distToPlayer == 1
				then do
					dist <- getDistanceToBestSafe coord 
					return $ myTrace "safeDist" $ dist
				else return 100000

		if (frightModeRemaining > 4 * 127 && coordDistToGhost == 0 && distToPlayer < 6)
			then return $ myTrace "killDist" $ distToPlayer - 100000
			else pillDistanceValue

getBoardCoords :: State GameState [GamePosition]
getBoardCoords = do
	mapWidth <- getMapWidth
	mapHeight <- getMapHeight
	--gameGrid <- getGrid
	--let mapHeight  = trace (printf "mapwidth:%d mapheight:%d gridLen:%d - %s " mapWidth _mapHeight (V.length gameGrid) (show gameGrid)) _mapHeight
	let coords = do
		py <- [0..mapHeight-1]
		px <- [0..mapWidth-1]
		return GamePosition{x=fromIntegral px, y=fromIntegral py}
	return coords

rankBoard :: (GamePosition -> State GameState Int) -> State GameState [(GamePosition, Int)]
rankBoard rankingFunction = do
	boardCoords <- getBoardCoords
	validBoardCoords <- filterM (\coord -> do
		element <- getAtGridPosition coord
		if (element == WALL) 
			then return False
			else return True
		) boardCoords
	rankings <- mapM rankingFunction validBoardCoords
	return $ zip validBoardCoords rankings

getBestLocation :: [(GamePosition, Int)] -> GamePosition
getBestLocation rankedBoard = results where
	bestBoard = minimumBy (comparing snd) rankedBoard
	results = fst bestBoard

aiGreedy :: (GamePosition -> State GameState Int) -> programState -> GameState -> (programState, GameDirection)
aiGreedy rankingFunction programState gameState = evalState fn gameState where 
	fn = do
		rankedBoard <- rankBoard rankingFunction
		let bestLocation = getBestLocation rankedBoard
		--frightMode <- getLambdaManVitality
		--let bestLocation = trace (printf "fright mode: %d bestPos: %s" frightMode (show _bestLocation)) _bestLocation
		move <- walkToLocation bestLocation
		let newProgramState = programState
		return (newProgramState, move)

walkToLocation :: GamePosition -> State GameState GameDirection
walkToLocation destPosition = do
	currentLocation <- getLambdaManLocation
	gameMap <- getMap
	let path = pathfind currentLocation destPosition (mapIsOccupied gameMap)
	case path of
		[] -> return DOWN
		_  -> return newDir where
			newDir = directionTo currentLocation (head path)

aiWalkToNearestPillOrFruit :: LambdaManAI programState
aiWalkToNearestPillOrFruit programState gameState = evalState fn gameState where 
		fn = do
			currentLocation <- getLambdaManLocation
			nearestPillLocation <- getNearestPill currentLocation
			move <- walkToLocation nearestPillLocation
			let newProgramState = programState
			return (newProgramState, move)

directionTo :: GamePosition -> GamePosition -> GameDirection
directionTo sourcePos destPos = result where
	diff = subtractPosition destPos sourcePos 
	result = case diff of 
		(-1,0) -> LEFT
		(1, 0) -> RIGHT
		(0,-1) -> UP
		(0, 1) -> DOWN
		_ -> error "path with step size > 1"

getNearestPill :: GamePosition -> State GameState GamePosition
getNearestPill sourceLocation = do
	coordGrid <- getCoordGrid
	let coordEdibles = V.filter ( \(_, element) -> isEdible element ) coordGrid
	let closestEdible = V.minimumBy (compareDistanceTo sourceLocation) coordEdibles
	return $ fst closestEdible

getNearestPowerPill :: GamePosition -> State GameState (Maybe GamePosition)
getNearestPowerPill sourceLocation = do
	coordGrid <- getCoordGrid
	let coordEdibles = V.filter ( \(_, element) -> element == POWER_PILL ) coordGrid
	if (V.null coordEdibles)
		then return Nothing
		else do
			let closestEdible = V.minimumBy (compareDistanceTo sourceLocation) coordEdibles
			return $ Just $ fst closestEdible

isEdible :: MapElement -> Bool
isEdible PILL = True
isEdible POWER_PILL = True
isEdible _ = False

getCoordGrid :: State GameState (ArrayMutability (GamePosition, MapElement))
getCoordGrid = do
	grid <- getGrid
	mapWidth <- getMapWidth
	mapHeight <- getMapHeight
	let coords = do
		py <- [0..mapHeight-1]
		px <- [0..mapWidth-1]
		return GamePosition{x=fromIntegral px, y=fromIntegral py}
	return $ V.zip (V.fromList coords) grid

manhattanDistance :: GamePosition -> GamePosition -> Int
manhattanDistance posA posB = result where
	(diff_x, diff_y) = subtractPosition posA posB
	result = abs (diff_x) + abs (diff_y)

getNeighborOffsets :: [GamePosition]
getNeighborOffsets = map getOffset allDirs

getNeighbors :: GamePosition -> [GamePosition]
getNeighbors sourcePosition = map (addPosition sourcePosition) getNeighborOffsets 

compareDistanceTo :: GamePosition -> (GamePosition, MapElement) -> (GamePosition, MapElement) -> Ordering
compareDistanceTo sourceLocation coordA coordB = result where
	posA = fst coordA
	posB = fst coordB
	result = comparing (manhattanDistance sourceLocation) posA posB

pathfind_old :: GamePosition -> GamePosition -> (GamePosition -> Bool) -> [GamePosition]
pathfind_old sourcePosition destPosition isOccupiedFn = result where
	neighbors = getNeighbors sourcePosition
	unoccupiedNeighbors = filter (not . isOccupiedFn) neighbors
	closest = minimumBy (comparing $ manhattanDistance destPosition) unoccupiedNeighbors
	result = if (null unoccupiedNeighbors) 
		then []
		else [closest]

pathfind :: GamePosition -> GamePosition -> (GamePosition -> Bool) -> [GamePosition]
pathfind sourcePosition destPosition isOccupiedFn = result where
	astarPath = aStar
		(S.fromList . validNeighbors)
		(\_ _ -> 1)
		(manhattanDistance destPosition)
		(\e -> e == destPosition)
		sourcePosition
	validNeighbors = \position -> filter (not . isOccupiedFn) (getNeighbors position)
	result = case astarPath of 
		Nothing -> []
		Just path -> path



-- find nearest pellet
-- step to pellet	

	


