{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Lambda_utils where
import LmLanguage
import LmPathfinder (pathfind, fPathfind, pathfindModule, coordEq)
import Lmcompile hiding (block)
import Lambda_compiler_2
import Quad_tree
import Prelude (Num(..), ($), (.), String, Bool(..), Maybe(..), fromIntegral, Show(..), Monad(..), Int)
import Prelude (Either(..), putStrLn)

----------------------------------------
--- Data Defitions
----------------------------------------

-- Enum for each cell in map state
kCellSymWall                   = 0
kCellSymEmpty                  = 1
kCellSymPill                   = 2
kCellSymPowerPill              = 3
kCellSymFruitLocation          = 4
kCellSymLMStartPos             = 5
kCellSymGhostStartingPosition  = 6

-- Enum for player/ghost direction
kDirectionUp = 0
kDirectionRight = 1
kDirectionDown = 2
kDirectionLeft = 3

-- Types
type MapState = [[Int]]
type LambdaManState = (Int, ((Int, Int), (Int, (Int, Int))))
type GhostState = (Int, ((Int, Int), Int))
type FruitState = Int

-- Enum for ghost vitality
kGhostStandard = 0
kGhostFright = 1
kGhostInvisible = 2


-- This is the main world state passed in init and every step. Technically we shouldn't need
-- to parse the MapState every frame as it only contains the initial starting positions, and
-- we can derive the current pallet pill locations etc
type WorldState = (MapState, (LambdaManState, ([GhostState], FruitState)))

-- (PlayerX, PlayerY)
type CellInfo = Int -- refer to kCellSymbol*
type MapQuadTree = QuadTree CellInfo
type MapPos = (Int, Int)
type MapSize = Int -- single dimension for simplicity. it will be the max
type ParsedState = (MapQuadTree, MapSize)
type AIState = (ParsedState, Int) -- to Extend. Second parameter not doing anything right now
type MaybeMapPos = Maybe MapPos
type MapInfo     = (MaybeMapPos, (MaybeMapPos, MaybeMapPos))


getPosX :: TExpr (Int, Int) -> TExpr Int
getPosX = fst
getPosY :: TExpr (Int, Int) -> TExpr Int
getPosY = snd

-- WorldState
getMapState :: (TExpr WorldState) -> TExpr MapState
getMapState = fst

getLMState :: (TExpr WorldState) -> TExpr LambdaManState
getLMState = fst . snd

getGhostState :: (TExpr WorldState) -> TExpr [GhostState]
getGhostState = fst . snd .snd

getFruitState :: (TExpr WorldState) -> TExpr FruitState
getFruitState = snd . snd .snd

-- LambdaManState
getLMVitality :: (TExpr LambdaManState) -> TExpr Int
getLMVitality = fst

getLMPos :: TExpr LambdaManState -> TExpr (Int, Int)
getLMPos = fst . snd

getLMDir :: TExpr LambdaManState -> TExpr Int
getLMDir = fst . snd . snd

getLMRemainingLives :: TExpr LambdaManState -> TExpr Int
getLMRemainingLives = fst . snd . snd . snd

getLMCurScore :: TExpr LambdaManState -> TExpr Int
getLMCurScore = snd . snd . snd . snd

-- Ghost State
getGhostVitality :: (TExpr GhostState) -> TExpr Int
getGhostVitality = fst

getGhostPos :: (TExpr GhostState) -> TExpr (Int, Int)
getGhostPos = fst . snd

getGhostDir :: (TExpr GhostState) -> TExpr Int
getGhostDir = snd . snd

-- ParsedState
initParsedState :: (TExpr ParsedState)
initParsedState = pair (makeQuadLeaf 1) 0

{-setPlayerPos :: (TExpr ParsedState) -> TExpr Int -> TExpr Int -> (TExpr ParsedState)-}
{-setPlayerPos _ y x = pair y x-}

getMapQuadTree :: TExpr ParsedState -> TExpr MapQuadTree
getMapQuadTree = fst

setMapQuadTree :: TExpr ParsedState -> TExpr MapQuadTree -> TExpr ParsedState
setMapQuadTree state quadTree = pair quadTree (snd state)

getMapSize :: TExpr ParsedState -> TExpr Int
getMapSize = snd

setMapSize :: TExpr ParsedState -> TExpr Int -> TExpr ParsedState
setMapSize state size = pair (fst state) size

{-getPlayerX :: (TExpr ParsedState) -> TExpr Int-}
{-getPlayerX = fst-}

{-getPlayerY :: (TExpr ParsedState) -> TExpr Int-}
{-getPlayerY = snd-}

-- AI State

getAIParsedState :: TExpr AIState -> TExpr ParsedState
getAIParsedState = fst

setAIParsedState :: TExpr AIState -> TExpr ParsedState -> TExpr AIState
setAIParsedState aiState parsedState = pair parsedState (snd aiState)

makeAIState :: TExpr ParsedState -> TExpr AIState
makeAIState parsedState = pair parsedState (int 1) -- 1 is bogus for now



----------------------------------------
--- World Parsing
----------------------------------------
fParseMapRow :: TFunction ((Arg MapState, Arg Int, Arg ParsedState) -> ParsedState)
fParseMapRow = function3 "parseMapRow" ("mapRows", "curRow", "parsedState") $ \(mapRows, curRow, parsedState) -> do
    ifm (null mapRows)
        (return parsedState)
        $ do
            withRec "parseCol" (\parseCol -> lam3 ("remainRow","col","curParsedState") $ \(remainRow,col,curParsedState) -> return $
                ifv (null remainRow)
                    curParsedState
                    (let value = head remainRow
                         quadTree = getMapQuadTree curParsedState
                         newQuadTree = replaceQuadLeaf quadTree (getMapSize curParsedState) value (pair col curRow)
                         newParsedState = setMapQuadTree curParsedState newQuadTree
                    in   ap3 parseCol (tail remainRow) (col + 1) newParsedState) )
                     {-in  (ifv (head remainRow == kCellSymLMStartPos)-}
                            {-parsedState $-}
                            {-ap2 parseCol (tail remainRow) (col + 1))) )-}
                (\parseCol -> return $ parseMapRow (tail mapRows) (curRow + 1) $ ap3 parseCol (head mapRows) 0 parsedState)
parseMapRow = bindAp3 fParseMapRow

fParseMapSize :: TFunction ((Arg MapState, Arg ParsedState) -> ParsedState)
fParseMapSize = function2 "parseMapSize" ("mapState", "parsedState") $ \(mapState, parsedState) -> return $
	let	vLength = length mapState
		hLength = length (head mapState)
		mapSize = roundUpToPower2 (max vLength hLength)
	in	setMapSize parsedState mapSize
parseMapSize = bindAp2 fParseMapSize
	
fParseMapState :: TFunction ((Arg MapState, Arg Int, Arg ParsedState) -> ParsedState)
fParseMapState = function3 "parseMapState" ("mapState", "curRow", "parsedState") $ \(mapState, curRow, parsedState) -> do
	with "parsedSizeState" (return $ parseMapSize mapState parsedState) $ \parsedSizeState -> return $
		let		allocatedQuadTree = allocateQuadTree (getMapSize parsedSizeState) kCellSymWall
				allocatedQuadTreeState = setMapQuadTree parsedSizeState allocatedQuadTree 
		in		{- debugTrace (getMapQuadTree allocatedQuadTreeState) $ -} parseMapRow mapState curRow allocatedQuadTreeState
parseMapState = bindAp3 fParseMapState

fParseWorldState :: TFunction (Arg WorldState -> ParsedState)
fParseWorldState = function1 "parseWorldState" "worldState" $ \worldState -> return $
    (parseMapState (fst worldState) (int 0) (initParsedState))
parseWorldState = bindAp1 fParseWorldState

{- TODO TEMP ONLY. Inefficient lookup -}
fMapLookup :: TFunction ((Arg MapState, Arg Int, Arg Int) -> Int)
fMapLookup = function3 "mapLookup" ("mapState", "posX", "posY") $ \(mapState, posX, posY) ->return $
    nth (nth mapState posY) posX
mapLookup = bindAp3 fMapLookup

fFastMapLookup :: TFunction ((Arg ParsedState, Arg Int, Arg Int) -> Int)
fFastMapLookup = function3 "fastMapLookup" ("parsedState", "posX", "posY") $ \(parsedState, posX, posY) ->return $
    let quadTree = getMapQuadTree parsedState
    in  findQuadLeaf quadTree (getMapSize parsedState) (pair posX posY)
fastMapLookup = bindAp3 fFastMapLookup

----------------------------------------
--- AI logic
----------------------------------------

{- TODO Test function to show basic decision making -}
fLMDecideDirection :: TFunction ((Arg ParsedState, Arg LambdaManState) -> Int)
fLMDecideDirection = function2 "lmDecideDirection" ("parsedState", "curLMPos") $ \(parsedState, curLMState) -> do
    with "leftSpot" (return $ fastMapLookup parsedState (getPosX (getLMPos curLMState) - 1) (getPosY $ getLMPos curLMState)) $ \leftSpot -> do
        with "rightSpot" (return $ fastMapLookup parsedState (getPosX (getLMPos curLMState) + 1) (getPosY $ getLMPos curLMState)) $ \rightSpot -> do
            with "curDir" (return $ getLMDir curLMState) $ \curDir -> return $
                ifv ((rightSpot == kCellSymWall) `and` (curDir == kDirectionRight))
                    kDirectionLeft
                    (ifv (or3 (curDir == kDirectionDown) (curDir == kDirectionUp) ((leftSpot == kCellSymWall) `and` (curDir == kDirectionLeft)))
                        kDirectionRight
                        curDir)
lmDecideDirection = bindAp2 fLMDecideDirection

----------------------------------------
--- AI logic Jeff
----------------------------------------

kInfDistance = 100000
kPowerPillCutoffDistance = 10
kPowerPillDistanceDefault = kPowerPillCutoffDistance + 1
kFruitCutoffDistance = 70

lam_subtractPos = function2 "lam_subtractPos" ("posA", "posB") $ \(posA, posB) -> do
	with "diffX" (return $ (fst posA) - (fst posB)) $ \diffX -> do 
		with "diffY" (return $ (snd posA) - (snd posB)) $ \diffY -> do 
			return $ pair diffX diffY

subtractPos = bindAp2 lam_subtractPos 

lam_myAbs = function1 "lam_myAbs" ("n") $ \n ->
	ifm (n < 0) 
		(return (-n))
		(return n)
myAbs = bindAp1 lam_myAbs

lam_absPos = function1 "lam_abs" ("pos") $ \pos -> do
	with "absX" (return $ myAbs $ fst pos) $ \absX-> do 
		with "absY" (return $ myAbs $ snd pos) $ \absY -> do 
			return $ pair absX absY

absPos = bindAp1 lam_absPos 

lam_manhattanDistance :: TFunction ((Arg (Int, Int), Arg (Int, Int)) -> Int)
lam_manhattanDistance = function2 "lam_manhattanDistance" ("posA", "posB") $ \(posA, posB) -> do
	with "diff" (return $ subtractPos posA posB) $ \diff -> do
		with "absDiff" (return $ absPos diff) $ \absDiff -> do
			with "sumDiff" (return $ (fst absDiff) + (snd absDiff)) $ \sumDiff -> do
				return $ sumDiff

manhattanDistance :: TExpr (Int, Int) -> TExpr (Int, Int) -> TExpr Int
manhattanDistance = bindAp2 lam_manhattanDistance 

kMaxPathfindDistance = 120

lam_pathfindDistance :: TFunction ((Arg (Int, Int), Arg (Int, Int), Arg ParsedState) -> Int)
lam_pathfindDistance = function3 "lam_pathfindDistance" ("posA", "posB", "parsedState") $ \(posA, posB, parsedState) -> do
	-- return $ manhattanDistance posA posB
	ifm (coordEq posA posB)
		(return 0)
		(with "pathfindResult" (return $ pathfind posA posB kMaxPathfindDistance parsedState) $ \pathfindResult -> do
			return $ snd $ debugTrace (pair (pair posA posB) pathfindResult) pathfindResult)

--pathfindDistance :: TExpr (Int, Int) -> TExpr (Int, Int) -> TExpr Int
pathfindDistance = bindAp3 lam_pathfindDistance


--lam_mapOver = function2 "lam_mapOver" ("func", "list") $ \(func, list) -> do
--	ifv (null list)
--		(return nil)
--		(with "element" (return $ head list) $ \element -> do
--			with "newElement" (return $ func element) $ \newElement -> do
--				with "rest" (return $ tail list) $ \rest-> do
--					with "newRest" (return $ mapOver func rest) \ newRest -> do
--						return $ cons newElement newRest
--		) 
--mapOver = bindAp2 lam_mapOver

-- lam_selectMin :: (a -> Int) -> a -> a -> a
lam_selectMin = function3 "lam_selectMin" ("func", "v0", "v1") $ \(func, v0, v1) ->
	ifm (ap1 func v0 < ap1 func v1) 
		(return v0)
		(return v1)

selectMin = bindAp3 lam_selectMin

-- minimumOver :: (a -> ord) -> [a] -> a
lam_minimumOver = function2 "lam_minimumOver" ("func", "list") $ \(func, list) -> do
	lam <- block $ lam2 ("v0", "v1") $ \(v0, v1) -> do
		return $ selectMin func v0 v1
	return $ foldr1 (lam) list

minimumOver = bindAp2 lam_minimumOver

lam_hasOneElement = function1 "lam_hasOneElement" ("list") $ \list -> do
	ifm (null list) 
		(return $ toExpr False)
		(ifm (null $ tail list)
			(return $ toExpr True)
			(return $ toExpr False)
		)

hasOneElement = bindAp1 lam_hasOneElement

lam_foldr1 = function2 "lam_foldr1" ("func", "list") $ \(func, list) -> do
	ifm (hasOneElement list)
		(return $ head list)
		(return $ ap2 func (head list) (foldr1 func $ tail list))

foldr1 = bindAp2 lam_foldr1

--type MapCoords [((Int, Int), Int)]
--toMapCoords :: MapState -> MapCoords
--lam_toMapCoords -> 
--	with "mapState" (return $ getMapState worldState) $ \mapState ->
--		zip 
--		return pair (pair curX curY) curElement

lam_zip = function2 "lam_zip" ("list0", "list1") $ \(list0, list1) -> do
	ifm (null list0) 
		(return nil)
		(ifm (null list1)
			(return nil)
			(return $ cons
				(pair (head list0) (head list1)) 
				(zip (tail list0) (tail list1))
			)
		)

zip = bindAp2 lam_zip


-- coord	:: MapCoord -> (Int, Int)
-- element  :: MapCoord -> Int
-- lam_coord   = fst
-- lam_element = snd

lam_filter = function2 "lam_filter" ("func", "list") $ \(func, list) -> do
	ifm (null list)
		(return nil)
		(ifm (ap1 func $ head list)
			(return $ cons 
				(head list) 
				(filter func (tail list))
			)
			(return $ filter func (tail list))
		)
filter = bindAp2 lam_filter

-- TODO

-- lam_getInfoFromMap :: WorldState -> (maybe neareset power pill, maybe nearest fruit, nearest pill)
--lam_getInfoFromMapRowCol = function2 "lam_getInfoFromMap" "worldState" $ \(mapElement, rowIndex, colIndex) -> do
--	return $ pair element (pair rowIndex colIndex)


lam_makeMapInfo :: TFunction ((Arg MaybeMapPos, Arg MaybeMapPos, Arg MaybeMapPos) -> MapInfo)
lam_makeMapInfo = function3 "lam_makeMapInfo" ("powerPill", "fruit", "pill") $ \(powerPill, fruit, pill) -> do
	return $ pair powerPill (pair fruit pill)
makeMapInfo :: TExpr MaybeMapPos -> TExpr MaybeMapPos -> TExpr MaybeMapPos -> TExpr MapInfo
makeMapInfo = bindAp3 lam_makeMapInfo

lam_closerLocation = function3 "lam_closerLocation" ("sourceCoord", "maybeOldCoord", "newCoord") $ \(sourceCoord, maybeOldCoord, newCoord) -> do
	ifm (isNothing maybeOldCoord)
		(return $ toExpr True)
		(ifm (manhattanDistance sourceCoord newCoord < manhattanDistance sourceCoord (fromJust $ maybeOldCoord))
			(return $ toExpr True)
			(return $ toExpr False)
		)
closerLocation = bindAp3 lam_closerLocation

mapInfo_Pill :: TExpr MapInfo -> TExpr MaybeMapPos
mapInfo_Pill = snd . snd

mapInfo_PowerPill :: TExpr MapInfo -> TExpr MaybeMapPos
mapInfo_PowerPill = fst

mapInfo_Fruit :: TExpr MapInfo -> TExpr MaybeMapPos
mapInfo_Fruit = fst . snd

lam_tryImprovePowerPill :: TFunction ((Arg MapPos, Arg MapInfo, Arg MapPos) -> MapInfo)
lam_tryImprovePowerPill = function3 "lam_tryImprovePowerPill" ("sourceCoord", "bestResultsSoFar", "newCoord") $ \(sourceCoord, bestResultsSoFar, newCoord) -> do
	ifm (closerLocation sourceCoord (mapInfo_Pill bestResultsSoFar) newCoord)
		(return $ makeMapInfo (just newCoord) (mapInfo_Fruit bestResultsSoFar) (mapInfo_Pill bestResultsSoFar))
		(return $ bestResultsSoFar)
tryImprovePowerPill = bindAp3 lam_tryImprovePowerPill

lam_tryImprovePill ::  TFunction ((Arg MapPos, Arg MapInfo, Arg MapPos) -> MapInfo)
lam_tryImprovePill = function3 "lam_tryImprovePill" ("sourceCoord", "bestResultsSoFar", "newCoord") $ \(sourceCoord, bestResultsSoFar, newCoord) -> do
	ifm (closerLocation sourceCoord (mapInfo_Pill bestResultsSoFar) newCoord)
		(return $ makeMapInfo (mapInfo_PowerPill bestResultsSoFar) (mapInfo_Fruit bestResultsSoFar) (just $ newCoord))
		(return $ bestResultsSoFar)
tryImprovePill = bindAp3 lam_tryImprovePill

lam_tryImproveFruit ::  TFunction ((Arg MapPos, Arg MapInfo, Arg MapPos) -> MapInfo)
lam_tryImproveFruit = function3 "lam_tryImproveFruit" ("sourceCoord", "bestResultsSoFar", "newCoord") $ \(sourceCoord, bestResultsSoFar, newCoord) -> do
	ifm (closerLocation sourceCoord (mapInfo_Fruit bestResultsSoFar) newCoord)
		(return $ makeMapInfo (mapInfo_PowerPill bestResultsSoFar) (just newCoord) (mapInfo_Pill bestResultsSoFar))
		(return bestResultsSoFar)
tryImproveFruit = bindAp3 lam_tryImproveFruit

lam_tryImproveResults ::  TFunction ((Arg MapPos, Arg MapInfo, Arg Int, Arg MapPos) -> MapInfo)
lam_tryImproveResults =	function4 "lam_tryImproveResults" ("sourceCoord", "bestResultsSoFar", "newElement", "newCoord") $ \(sourceCoord, bestResultsSoFar, newElement, newCoord) -> do
	ifm (newElement == kCellSymPill)
		(return $ tryImprovePill sourceCoord bestResultsSoFar newCoord)
		(ifm (newElement == kCellSymFruitLocation)
			(return $ tryImproveFruit sourceCoord bestResultsSoFar newCoord)
			(ifm (newElement == kCellSymPowerPill)
				(return $ tryImprovePowerPill sourceCoord bestResultsSoFar newCoord)
				(return $ bestResultsSoFar)
			)
		)
tryImproveResults = bindAp4 lam_tryImproveResults

lam_getInfoFromMapRecursive = function6 "lam_getInfoFromMapRecursive" ("sourceCoord", "bestResultsSoFar", "mapCurrentRow", "mapCurrentCol", "rowIndex", "colIndex") $ \(sourceCoord, bestResultsSoFar, mapCurrentRow, mapCurrentCol, rowIndex, colIndex) -> do
	ifm (null mapCurrentCol)
		(ifm (null mapCurrentRow)
			(return $ bestResultsSoFar)
			(return $ getInfoFromMapRecursive sourceCoord bestResultsSoFar (tail mapCurrentRow) (head mapCurrentRow) (rowIndex+1) (0))
		)
		(return $ getInfoFromMapRecursive 
			sourceCoord 
			(tryImproveResults sourceCoord bestResultsSoFar (head mapCurrentCol) (pair colIndex rowIndex))
			mapCurrentRow 
			(tail mapCurrentCol) 
			(rowIndex) 
			(colIndex+1)
		)
getInfoFromMapRecursive = bindAp6 lam_getInfoFromMapRecursive

lam_getInfoFromMap = function1 "lam_getInfoFromMap" "worldState" $ \worldState -> do
	with "mapGrid"			(return $ getMapState worldState) $ \mapGrid -> do
		with "playerCoord"			(return $ getLMPos $ getLMState worldState) $ \playerCoord -> do
			return $ getInfoFromMapRecursive
				(playerCoord)
				(makeMapInfo nothing nothing nothing)
				(tail mapGrid)
				(head mapGrid)
				0
				0
getInfoFromMap = bindAp1 lam_getInfoFromMap



--getPowerPills _x = error "undefined" -- can implemnt getNearestPowerPill instead
--getPills _x = error "undefined"      -- can implemnt getNearestPill instead
--getFruitLocation _x = pair 0 0	-- 
--nothing :: TExpr (Maybe a)
--nothing = unsafeCast nil

{-
lam_isNothing = function1 ("lam_isNothing") $ \v -> do
	ifm (fst v == nil)
		(return True)
		(return False)
isNothing = bindAp1 lam_isNothing 

lam_fromJust = function1 "lam_fromJust" ("v") $ \v -> do
	return $ snd v
fromJust = bindAp1 lam_fromJust
-}

{-
lam_getNearestPowerPill = function2 "lam_getNearestPowerPill" ("worldState", "coord") $ \(worldState, coord) -> do
	with "powerPills" (return $ getPowerPills worldState) $ \powerPills -> do
		ifm (null powerPills) 
			(return nothing)
			(do 
				lam <- block $ lam1 ("lamCoord") $ \lamCoord ->	do
					return $ manhattanDistance coord lamCoord
				with "bestPill" (return $ minimumOver (lam) powerPills) $ \bestPill ->
					return $ just $ bestPill
			)
getNearestPowerPill = bindAp2 lam_getNearestPowerPill
-}

{-
lam_getNearestPill = function2 "lam_getNearestPill" ("worldState", "coord") $ \(worldState, coord) -> do
	with "pills" (return $ getPills worldState) $ \pills -> do
			lam <- block $ lam1 ("lamCoord") $ \lamCoord ->	do
				return $ manhattanDistance coord lamCoord
			with "bestPill" (return $ minimumOver (lam) pills) $ \bestPill ->
				return $ bestPill
getNearestPill = bindAp2 lam_getNearestPill
-}

type EnhancedWorldState = (WorldState, MapInfo)

lam_getDistanceToPowerPill :: TFunction ((Arg ParsedState, Arg EnhancedWorldState, Arg MapPos) -> Maybe Int)
lam_getDistanceToPowerPill = function3 "lam_getDistanceToPowerPill" ("parsedState", "enhancedWorldState", "coord") $ \(parsedState, enhancedWorldState, coord) -> do
	ifm (isNothing $ mapInfo_PowerPill $ snd enhancedWorldState)
		(return $ nothing)
		(return $ just $ pathfindDistance coord (fromJust $ mapInfo_PowerPill $ snd enhancedWorldState) parsedState)
getDistanceToPowerPill = bindAp3 lam_getDistanceToPowerPill

-- getNearest_Pill_PowerPill_Fruit



lam_getDistanceToFruit :: TFunction ((Arg ParsedState, Arg EnhancedWorldState, Arg MapPos) -> Maybe Int)
lam_getDistanceToFruit = function3 "lam_getDistanceToFruit" ("parsedState", "enhancedWorldState", "coord") $ \(parsedState, enhancedWorldState, coord) -> do
	ifm (getFruitState (fst enhancedWorldState) == 0)
		(return nothing)
		(return $ just $ pathfindDistance coord (fromJust $ mapInfo_Fruit $ snd enhancedWorldState) parsedState)
getDistanceToFruit = bindAp3 lam_getDistanceToFruit

lam_getDistanceToPill :: TFunction ((Arg ParsedState, Arg EnhancedWorldState, Arg MapPos) -> Int)
lam_getDistanceToPill = function3 "lam_getDistanceToPill" ("parsedState", "enhancedWorldState", "coord") $ \(parsedState, enhancedWorldState, coord) -> do
	with "pill" (return $ fromJust $ mapInfo_Pill $ snd enhancedWorldState) $ \pill -> do
		(return $ pathfindDistance coord (pill) parsedState)
getDistanceToPill = bindAp3 lam_getDistanceToPill


lam_identity = function1 "lam_identity" ("a") $ \a -> return a
minimum = minimumOver (global lam_identity)


lam_isGhostVisible = function1 "lam_isGhostVisible" "ghost" $ \ghost -> do
	return (getGhostVitality ghost /= kGhostInvisible)

lam_getDistanceToGhost :: TFunction ((Arg WorldState, Arg MapPos) -> Maybe Int)
lam_getDistanceToGhost = function2 "lam_getDistanceToGhost" ("worldState", "coord") $ \(worldState, coord) -> do
	with "ghosts" (return $ getGhostState worldState) $ \ghosts-> do
		with "visibleGhosts" (return $ filter (global lam_isGhostVisible) ghosts) $ \visibleGhosts -> do
			lam <- block $ lam1 ("ghost") $ \ghost ->	do
				return $ manhattanDistance coord (getGhostPos ghost)
			with "visibleGhostDistances" (return $ map (lam) visibleGhosts) $ \visibleGhostDistances -> do
			ifm (null visibleGhostDistances)
				(return nothing)
				(return $ just $ minimum visibleGhostDistances)
getDistanceToGhost = bindAp2 lam_getDistanceToGhost


lam_foldl :: TFunction((Arg ((Arg b, Arg a) -> b), Arg b, Arg [a]) -> b) 
lam_foldl = function3 "lam_foldl" ("function", "accum", "list") $ \(func, accum, list) ->
	ifm (null list) 
		(return accum)
		(return $ foldl
			(func) 
			(ap2 func accum (head list)) 
			(tail list)
		)
foldl = bindAp3 lam_foldl


lam_count :: TFunction((Arg Int, Arg a) -> Int)
lam_count = function2 ("lam_count") ("a", "b") $ \(a, _) -> do return $ a + (int 1)
length :: TExpr [a] -> TExpr Int
length = foldl (global lam_count) 0

lam_plus = function2 ("lam_plus") ("a", "b") $ \(a, b) -> do return $ a+b

lam_sum = function1 "lam_sum" ("list") $ \list -> do
	return $ foldl (global lam_plus) 0 list
sum = bindAp1 lam_sum

lam_totalGhostDanger  :: TFunction ((Arg WorldState, Arg MapPos) -> Int)
lam_totalGhostDanger = function2 "lam_totalGhostDanger" ("worldState", "coord") $ \(worldState, coord) -> do
	with "ghosts" (return $ getGhostState worldState) $ \ghosts-> do
		with "visibleGhosts" (return $ filter (global lam_isGhostVisible) ghosts) $ \visibleGhosts -> do
			lam <- block $ lam1 "ghost" $ \ghost -> 
				do return $ calcGhostDangerAtPoint coord ghost
			with "visibleGhostDanger" (return $ map lam visibleGhosts) $ \visibleGhostDanger -> do
				return $ sum visibleGhostDanger
totalGhostDanger = bindAp2 lam_totalGhostDanger

kGhostDangerCuttoff = 15

lam_max = function2 "lam_max" ("v0", "v1") $ \(v0, v1) -> do
	return $ ifv (v0 > v1) v0 v1
max = bindAp2 lam_max	

lam_calcGhostDangerAtPoint = function2 "lam_calcGhostDangerAtPoint" ("coord", "ghost") $ \(coord, ghost) -> do
	with "dist" (return $ manhattanDistance coord (getGhostPos ghost) ) $ \dist -> do
		ifm (dist > kGhostDangerCuttoff) 
			(return 0)
			(return $ kGhostDangerCuttoff - dist)
calcGhostDangerAtPoint = bindAp2 lam_calcGhostDangerAtPoint

lam_getDistanceToBest_Safe :: TFunction ((Arg ParsedState, Arg EnhancedWorldState, Arg MapPos) -> Int)
lam_getDistanceToBest_Safe = function3 "lam_getDistanceToBest_Safe" ("parsedState", "enhancedWorldState", "coord") $ \(parsedState, enhancedWorldState, coord) -> do
	with "distFruit" (return $ getDistanceToFruit parsedState enhancedWorldState coord) $ \distFruit -> do
		with "dist" (
			ifm (fromMaybe distFruit (kFruitCutoffDistance + 1) < kFruitCutoffDistance)
				(return $ (-50) + fromMaybe distFruit (kFruitCutoffDistance + 1))
				(return $ getDistanceToPill parsedState enhancedWorldState coord)
			) $ \dist -> do
				--return $ debugTrace dist dist
				return $ 8 * dist + (totalGhostDanger (fst enhancedWorldState) coord)

getDistanceToBest_Safe = bindAp3 lam_getDistanceToBest_Safe

lam_getDistanceToBest_Scared :: TFunction ((Arg ParsedState, Arg EnhancedWorldState, Arg MapPos) -> Int)
lam_getDistanceToBest_Scared = function3 "lam_getDistanceToBest_Scared" ("parsedState", "enhancedWorldState", "coord") $ \(parsedState, enhancedWorldState, coord) -> do
	with "distPowerPill" (return $ getDistanceToPowerPill parsedState enhancedWorldState coord) $ \distPowerPill -> do
		with "distFruit" (return $ getDistanceToFruit parsedState enhancedWorldState coord) $ \distFruit -> do
			with "dist" (
				ifm (fromMaybe distPowerPill kPowerPillDistanceDefault < kPowerPillCutoffDistance) 
					(return $ (-100) + fromMaybe distPowerPill kPowerPillDistanceDefault)
					(ifm (fromMaybe distFruit (kFruitCutoffDistance + 1) < kFruitCutoffDistance)
						(return $ (-50) + fromMaybe distFruit (kFruitCutoffDistance + 1))
						(return $ getDistanceToPill parsedState enhancedWorldState coord)
					) 
				) $ \dist -> do
					with "distanceToGhost" (return $ getDistanceToGhost (fst enhancedWorldState) coord) $ \distanceToGhost -> do
						return $ 8 * (dist + 4 * max 0 (8 - fromMaybe distanceToGhost 8)) + (totalGhostDanger (fst enhancedWorldState) coord)
getDistanceToBest_Scared = bindAp3 lam_getDistanceToBest_Scared

lam_getOffset = function1 "lam_getOffset" "dir" $ \dir ->
	ifm (dir == kDirectionUp)
		(return $ pair 0 (-1))
		(ifm (dir == kDirectionDown)
			(return $ pair 0 (1))
			(ifm (dir == kDirectionLeft)
				(return $ pair (-1) 0)
				(return $ pair  (1) 0)
			)
		)
getOffset = bindAp1 lam_getOffset

lam_addPosition = function2 "lam_addPosition" ("posA", "posB") $ \(posA, posB) ->
	with "diffX" (return $ (fst posA) + (fst posB)) $ \diffX -> do 
		with "diffY" (return $ (snd posA) + (snd posB)) $ \diffY -> do 
			return $ pair diffX diffY
addPosition = bindAp2 lam_addPosition

lam_makeDirsAndCoords = function2 "lam_makeDirsAndCoords" ("coord", "dir") $ \(coord, dir) ->
	with "offset"	(return $ getOffset dir) $ \offset -> do
		return $ pair dir (addPosition coord offset)
makeDirsAndCoords = bindAp2 lam_makeDirsAndCoords

lam_canStep = function2 "lam_canStep" ("parsedState", "dirCoord") $ \(parsedState, dirCoord) -> do
	with "element" (return $ fastMapLookup parsedState (fst (snd dirCoord)) (snd (snd dirCoord)) ) $ \element -> do
		return $ element /= kCellSymWall

lam_getGhostPos = function1 "lam_getGhostPos" ("ghost") $ \(ghost) -> do
	return $ getGhostPos ghost

lam_getDistToNearestVisibleGhostPathfind :: TFunction ((Arg ParsedState, Arg WorldState, Arg MapPos) -> Maybe Int)
lam_getDistToNearestVisibleGhostPathfind = function3 "lam_getDistToNearestVisibleGhostPathfind" ("parsedState", "worldState", "coord") $ \(parsedState, worldState, coord) -> do
	with "ghosts" (return $ getGhostState worldState) $ \ghosts->
		with "visibleGhosts" (return $ filter (global lam_isGhostVisible) ghosts) $ \visibleGhosts ->
			ifm (null visibleGhosts)
				(return nothing)
				(with "visibleGhostsPositions" (return $ map (global lam_getGhostPos) visibleGhosts) $ \visibleGhostsPositions ->
						with "visibleGhostsApproxDistances" (return $ map (pAp2_1 (global lam_manhattanDistance) coord) visibleGhostsPositions) $ \visibleGhostsApproxDistances ->
							with "closestGhostPosDist" (return $ minimumOver (global lam_snd) (zip visibleGhostsPositions visibleGhostsApproxDistances)) $ \closestGhostPosDist ->
								ifm (snd closestGhostPosDist < 5)
									(return $ just $ pathfindDistance coord (fst closestGhostPosDist) parsedState)
									(return $ just $ snd closestGhostPosDist)
				)


{-
				lamPathfind <- block $ lam1 "otherCoord" $ \otherCoord -> do
					return $ pathfindDistance coord otherCoord parsedState	
				with "visibleGhostsDistances" (return $ map lamPathfind visibleGhostsPositions) $ \visibleGhostsDistances -> do
				ifm (null visibleGhostsDistances)
					(return nothing)
					(return $ just $ minimum visibleGhostsDistances)
-}

getDistToNearestVisibleGhostPathfind = bindAp3 lam_getDistToNearestVisibleGhostPathfind

lam_getPlayerDistToNearestGhost :: TFunction ((Arg ParsedState, Arg WorldState) -> Maybe Int)
lam_getPlayerDistToNearestGhost = function2 "lam_getPlayerDistToNearestGhost" ("parsedState", "worldState") $ \(parsedState, worldState) -> do
	with "playerCoord"			(return $ getLMPos $ getLMState $ worldState) $ \playerCoord -> do
		return $ getDistToNearestVisibleGhostPathfind parsedState worldState playerCoord
getPlayerDistToNearestGhost = bindAp2 lam_getPlayerDistToNearestGhost

lam_scoreCoord_killMode :: TFunction ((Arg ParsedState, Arg EnhancedWorldState, Arg MapPos) -> Int)
lam_scoreCoord_killMode = function3 "lam_scoreCoord_killMode" ("parsedState", "enhancedWorldState", "coord") $ \(parsedState, enhancedWorldState, coord) -> do
	return $ fromJust $ getDistToNearestVisibleGhostPathfind parsedState (fst enhancedWorldState) coord 
scoreCoord_killMode = bindAp3 lam_scoreCoord_killMode

lam_scoreCoord_safeMode :: TFunction ((Arg ParsedState, Arg EnhancedWorldState, Arg MapPos) -> Int)
lam_scoreCoord_safeMode = function3 "lam_scoreCoord_safeMode" ("parsedState", "enhancedWorldState", "coord") $ \(parsedState, enhancedWorldState, coord) -> do
	return $ getDistanceToBest_Safe parsedState enhancedWorldState coord
scoreCoord_safeMode = bindAp3 lam_scoreCoord_safeMode

lam_scoreCoord_scaredMode :: TFunction ((Arg ParsedState, Arg EnhancedWorldState, Arg MapPos) -> Int)
lam_scoreCoord_scaredMode = function3 "lam_scoreCoord_scaredMode" ("parsedState", "enhancedWorldState", "coord") $ \(parsedState, enhancedWorldState, coord) -> do
	with "playerDistToNearestGhost" (return $ fromJust $ getPlayerDistToNearestGhost parsedState (fst enhancedWorldState)) $ \playerDistToNearestGhost -> do
		with "bestDist" (return $ getDistanceToBest_Scared parsedState enhancedWorldState coord) $ \bestDist -> do
			with "distanceToVisibleGhost" (return $ fromJust $ getDistToNearestVisibleGhostPathfind parsedState (fst enhancedWorldState) coord) $ \distanceToVisibleGhost -> do
				ifm (distanceToVisibleGhost <= playerDistToNearestGhost)
					(return $ 50000 + bestDist)
					(return $ bestDist)
scoreCoord_scaredMode = bindAp3 lam_scoreCoord_scaredMode

lam_inFightMode = function1 "lam_inFightMode" ("worldState") $ \(worldState) -> do
	ifm (getLMVitality (getLMState worldState) > 0)
		(return $ toExpr True)
		(return $ toExpr False)
inFightMode = bindAp1 lam_inFightMode

-- if (frightModeRemaining > 4 * 127 && coordDistToGhost == 0 && distToPlayer < 6)
lam_inKillMode :: TFunction ((Arg ParsedState, Arg WorldState) -> Bool)
lam_inKillMode = function2 "lam_inKillMode" ("parsedState", "worldState") $ \(parsedState, worldState) -> do
		with "playerDistToNearestGhost" (return $ getPlayerDistToNearestGhost parsedState worldState) $ \playerDistToNearestGhost -> do
			ifm (isNothing playerDistToNearestGhost)
				(return $ toExpr False)
				(do 
					with "lamdaManVitality" (return $ getLMVitality (getLMState worldState)) $ \lamdaManVitality -> do
					ifm (lamdaManVitality > (4 * 127) && (fromJust (playerDistToNearestGhost)) < 6)
						(return $ toExpr True)
						(return $ toExpr False)
				)
inKillMode = bindAp2 lam_inKillMode

-- if (playerDistToGhost < 4)
lam_inScaredMode :: TFunction ((Arg ParsedState, Arg WorldState) -> Bool)
lam_inScaredMode = function2 "lam_inScaredMode" ("parsedState", "worldState") $ \(parsedState, worldState) -> do
	with "playerDistToNearestGhost" (return $ getPlayerDistToNearestGhost parsedState worldState) $ \playerDistToNearestGhost -> do
	ifm (isNothing playerDistToNearestGhost) 
		(return $ toExpr False)
		(ifm (fromJust (playerDistToNearestGhost) < 4)
			(return $ toExpr True)
			(return $ toExpr False)
		)
inScaredMode = bindAp2 lam_inScaredMode

lam_scoreCoord :: TFunction ((Arg ParsedState, Arg EnhancedWorldState, Arg MapPos) -> Int)
lam_scoreCoord = function3 "lam_scoreCoord" ("parsedState", "enhancedWorldState", "coord") $ \(parsedState, enhancedWorldState, coord) -> do
	ifm (inFightMode $ fst enhancedWorldState)
		(ifm (inKillMode parsedState $ fst enhancedWorldState)
			(return $ scoreCoord_killMode parsedState enhancedWorldState coord)
			(return $ scoreCoord_safeMode parsedState enhancedWorldState coord)
		)
		(ifm (inScaredMode parsedState $ fst enhancedWorldState)
			(return $ scoreCoord_scaredMode parsedState enhancedWorldState coord)
			(return $ scoreCoord_safeMode parsedState enhancedWorldState coord)
		)

scoreCoord = bindAp3 lam_scoreCoord

lam_scoreNeighbor = function3 "lam_scoreNeighbor" ("parsedState", "enhancedWorldState", "neighbor") $ \(parsedState, enhancedWorldState, neighbor) -> do
	with "coord" (return $ snd neighbor) $ \coord -> do
		with "score" (return $ scoreCoord parsedState enhancedWorldState coord) $ \score -> do
			return $ pair neighbor score
scoreNeighbor = bindAp3 lam_scoreNeighbor

lam_aiHunter :: TFunction ((Arg AIState, Arg WorldState) -> Int)
lam_aiHunter = function2 "lam_aiHunter" ("aiState", "worldState") $ \(aiState, worldState) -> do
	with "bestMapLocations"	(return $ getInfoFromMap worldState) $ \bestMapLocations -> do
		with "playerCoord"			(return $ getLMPos $ getLMState worldState) $ \playerCoord -> do
			with "dirs"					(return $ cons 0 (cons 1 (cons 2 (cons 3 nil))) ) $ \dirs -> do
				with "dirsAndCoords"		(return $ map (pAp2_1 (global lam_makeDirsAndCoords) playerCoord) dirs) $ \dirsAndCoords -> do
					with "validDirsAndCoords"	(return $ filter (pAp2_1 (global lam_canStep) (getAIParsedState aiState)) dirsAndCoords) $ \validDirsAndCoords -> do
						lamScoreNeighbor <- block $ lam1 "neighbor" $ \neighbor -> do
							return $ scoreNeighbor (getAIParsedState aiState) (pair worldState bestMapLocations) neighbor
						with "scores"				(return $ map lamScoreNeighbor validDirsAndCoords) $ \scores -> do
							ifm (null scores)
								(return kDirectionDown)
								(return $ (fst.fst) $ minimumOver (global lam_snd) scores)

aiHunter = bindAp2 lam_aiHunter


lam_aiSearch :: TFunction ((Arg AIState, Arg WorldState) -> Int)
lam_aiSearch = function2 "lam_aiSearch" ("aiState", "worldState") $ \(aiState, worldState) -> do
	with "bestMapLocations"	(return $ getInfoFromMap worldState) $ \_bestMapLocations -> do
		with "playerCoord"			(return $ getLMPos $ getLMState worldState) $ \playerCoord -> do
			with "dirs"					(return $ cons 0 (cons 1 (cons 2 (cons 3 nil))) ) $ \dirs -> do
				with "dirsAndCoords"		(return $ map (pAp2_1 (global lam_makeDirsAndCoords) playerCoord) dirs) $ \dirsAndCoords -> do
					with "validDirsAndCoords"	(return $ filter (pAp2_1 (global lam_canStep) (getAIParsedState aiState)) dirsAndCoords) $ \validDirsAndCoords -> do
						return $ debugTrace (fst $ head $ validDirsAndCoords) (fst $ head $ validDirsAndCoords)


lam_aiBad :: TFunction ((Arg AIState, Arg WorldState) -> Int)
lam_aiBad = function2 "lam_aiBad" ("aiState", "worldState") $ \(aiState, worldState) -> do
	with "bestMapLocations"	(return $ getInfoFromMap worldState) $ \_bestMapLocations -> do
		with "playerCoord"			(return $ getLMPos $ getLMState worldState) $ \playerCoord -> do
			with "dirs"					(return $ cons 0 (cons 1 (cons 2 (cons 3 nil))) ) $ \dirs -> do
				with "dirsAndCoords"		(return $ map (pAp2_1 (global lam_makeDirsAndCoords) playerCoord) dirs) $ \dirsAndCoords -> do
					with "validDirsAndCoords"	(return $ filter (pAp2_1 (global lam_canStep) (getAIParsedState aiState)) dirsAndCoords) $ \validDirsAndCoords -> do
						return $ debugTrace (fst $ head $ validDirsAndCoords) (fst $ head $ validDirsAndCoords)
aiBad = bindAp2 lam_aiBad

lam_snd = function1 "lam_snd" "p" $ \p -> do
	return $ snd p

	
----------------------------------------
--- Program Loop
----------------------------------------

-- Main entry loop
fLambdaManMain :: TFunction ((Arg WorldState, Arg () {-bogus-}) -> (AIState, (Arg AIState, Arg WorldState) -> (AIState, Int)))
fLambdaManMain = function2 "lambdaManMain" ("worldState", "ghostAI") $ \(worldState, _ghostAI {-todo-} ) -> do
    (with "parsedState" (return (parseWorldState worldState)) (\parsedState -> return $
        (pair (makeAIState parsedState) (global fLambdaManStep))))

-- Step function
fLambdaManStep :: TFunction ((Arg AIState, Arg WorldState) -> (AIState, Int))
fLambdaManStep = function2 "lambdaMainStep" ("aiState", "worldState") $ \(aiState, worldState) -> do
	return $ pair aiState (aiHunter aiState worldState)

    -- Testing only. Shouldn't parse map state every step
    --(with "parsedState" (return (parseWorldState worldState)) (\_parsedState -> return $
    --    pair aiState (lmDecideDirection (fst worldState) (getLMState worldState))))
        {-ifv (getPlayerX parsedState > 20)-}
            {-(pair aiState kDirectionLeft)-}
            {-(pair aiState kDirectionRight)))-}
lambdaMainStep = bindAp2 fLambdaManStep



----------------------------------------
-- Testing code
----------------------------------------
mainTest = do
    let mapState = toExpr [[0,5]]
        _gameState = pair mapState (pair 2 (pair 3 4))
    putStrLn $ show $ parseMapState mapState 1 initParsedState
    putStrLn "\n\nMap Row"
    putStrLn $ show $ fParseMapRow
    putStrLn "\n\nMap State"
    putStrLn $ show $ fParseMapState
    {-putStrLn $ show $ ifv (null mapState) (toExpr True) (toExpr False)-}
    {-putStrLn $ show $ parseGameState gameState-}

fTestParseMain :: TFunction (NoArgs -> ParsedState)
fTestParseMain = function0 "testParseMain" $ do
    let mapState = [
                    [0,0,0,0],
                    [0,0,5,0],
                    [0,0,0,0]
                   ] in
	return $ parseMapState (toExpr mapState) 0 initParsedState

testParseProg :: LProgram
testParseProg = mkProgram "lambdaManMain" -- "testParseMain"
				$ yeeModule
				>< jeffModule
				>< preludeModule
				>< pathfindModule

yeeModule :: LModule
yeeModule = newModule
                {-^^ fTestParseMain-}
                ^^ fNth
                ^^ fParseMapState
                ^^ fParseMapRow
                ^^ fParseWorldState
				^^ fParseMapSize
                ^^ fMapLookup
                ^^ fFastMapLookup
				^^ fLMDecideDirection
                ^^ fLambdaManMain
                ^^ fLambdaManStep
				^^ fMap
				^^ fAllocateQuadTree
                ^^ fReplaceQuadLeaf
                ^^ fFindQuadLeaf
                ^^ fRoundUpToPower2

jeffModule :: LModule
jeffModule = newModule
				^^ lam_subtractPos
				^^ lam_absPos
				^^ lam_manhattanDistance
				^^ lam_selectMin
				^^ lam_minimumOver
				^^ lam_hasOneElement
				^^ lam_foldr1
				^^ lam_zip
				^^ lam_getDistanceToPowerPill
				^^ lam_getDistanceToFruit
				^^ lam_getDistanceToPill
				^^ lam_identity
				^^ lam_isGhostVisible
				^^ lam_getDistanceToGhost
				^^ lam_foldl
				^^ lam_plus
				^^ lam_sum
				^^ lam_count
				^^ lam_totalGhostDanger
				^^ lam_calcGhostDangerAtPoint
				^^ lam_max
				^^ lam_getDistanceToBest_Scared
				^^ lam_getOffset
				^^ lam_addPosition
				^^ lam_makeDirsAndCoords
				^^ lam_canStep
				^^ lam_scoreCoord
				^^ lam_scoreNeighbor
				^^ lam_scoreCoord_killMode
				^^ lam_scoreCoord_safeMode
				^^ lam_scoreCoord_scaredMode
				^^ lam_inFightMode
				^^ lam_inKillMode
				^^ lam_inScaredMode
				^^ lam_getPlayerDistToNearestGhost
				^^ lam_getDistToNearestVisibleGhostPathfind
				^^ lam_getDistanceToBest_Safe
				^^ lam_getInfoFromMapRecursive
				^^ lam_getInfoFromMap
				^^ lam_makeMapInfo
				^^ lam_closerLocation
				^^ lam_tryImprovePowerPill
				^^ lam_tryImprovePill
				^^ lam_tryImproveFruit
				^^ lam_tryImproveResults
				^^ lam_filter
				^^ lam_myAbs
				^^ lam_aiHunter
				^^ lam_aiBad
				^^ lam_snd
				^^ lam_pathfindDistance
				^^ lam_getGhostPos


testParse :: Either CompileError [LCInstr EnvAddr Address]
testParse = compileAndLink $ testParseProg

mainCompile = printAsm testParse

main = mainCompile
