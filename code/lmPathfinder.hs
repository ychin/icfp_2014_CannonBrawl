{-# LANGUAGE ExistentialQuantification, GADTs, ScopedTypeVariables #-}
module LmPathfinder where
import Prelude (Num(..), ($), (.), String, Bool(..), Maybe(..), fromIntegral, Show(..), Monad(..), Int)
import LmLanguage
import Control.Monad.Identity (runIdentity)
import Quad_tree

type CellInfo = Int -- refer to kCellSymbol*
type MapQuadTree = QuadTree CellInfo
type ParsedState = (MapQuadTree, MapSize)
type MapSize = Int -- single dimension for simplicity. it will be the max
type MapState = [[MapTile]]
type LambdaManState = (Int, ((Int, Int), (Int, (Int, Int))))
type GhostState = (Int, ((Int, Int), Int))
type FruitState = Int

-- This is the main world state passed in init and every step. Technically we shouldn't need
-- to parse the MapState every frame as it only contains the initial starting positions, and
-- we can derive the current pallet pill locations etc
type WorldState = (MapState, (LambdaManState, ([GhostState], FruitState)))

type MapTile = Int
type Coord = (Int,Int)
type Direction = Int
type Monoid a = (a, (Arg a, Arg a) -> a)

mempty :: TExpr (Monoid a) -> TExpr a
mempty = fst

mappend :: TExpr (Monoid a) -> TExpr a -> TExpr a -> TExpr a
mappend = ap2 . snd

-- Note: Be careful about recursion, don't generate an infinite expression tree
caseList :: TExpr [a] -> ExprM (TExpr b) -> (TExpr a -> TExpr [a] -> ExprM (TExpr b)) -> ExprM (TExpr b)
caseList xs isNil isCons = ifm (null xs) isNil (isCons (head xs) (tail xs))

fFoldMap :: TFunction ((Arg (Monoid m), Arg (Arg a -> m), Arg [a]) -> m)
fFoldMap = function3 "foldMap" ("monoid", "elem", "xs0") $ \(monoid, elem, xs0) ->
	withRec "go" (\go -> lam2 ("z","xs") $ \(z,xs) -> caseList xs (return z) $ \y ys -> return $ ap2 go (mappend monoid z $ ap1 elem y) ys)
		$ \go -> return $ ap2 go (mempty monoid) xs0
foldMap :: TExpr (Monoid m) -> TExpr (Arg a -> m) -> TExpr [a] -> TExpr m
foldMap = bindAp3 fFoldMap

fFoldMap1 :: TFunction ((Arg ((Arg a, Arg a) -> a), Arg (Arg b -> a), Arg [b]) -> a)
fFoldMap1 = function3 "foldMap1" ("combine", "elem", "xs") $ \(combine, elem, xs) ->
	caseList xs (return $ error "FmEmptyList") $ \y ys -> return $ foldMap (pair (ap1 elem y) combine) elem ys
foldMap1 :: TExpr ((Arg a, Arg a) -> a) -> TExpr (Arg b -> a) -> TExpr [b] -> TExpr a
foldMap1 = bindAp3 fFoldMap1

fAppendMaybe :: TFunction ((Arg (Maybe a), Arg (Maybe a)) -> Maybe a)
fAppendMaybe = function2 "appendMaybe" ("l", "r") $ \(l,r) -> return $ ifv (isNothing l) r l

appendMaybe :: TExpr (Maybe a) -> TExpr (Maybe a) -> TExpr (Maybe a)
appendMaybe = bindAp2 fAppendMaybe

monoidMaybe :: TExpr (Monoid (Maybe a))
monoidMaybe = pair nothing (global fAppendMaybe)

kVisited :: TExpr Int
kVisited = 100

fLookup :: TFunction ((Arg (Arg a -> Bool), Arg [(a,b)]) -> Maybe b)
fLookup = function2 "lookup" ("testElt", "xs") $ \(testElt, xs) ->
	caseList xs (return nothing) $ \y ys ->
		ifm (ap1 testElt (fst y))
		    (return $ just $ snd y)
			(return $ lookup testElt ys)
lookup :: TExpr (Arg a -> Bool) -> TExpr [(a, b)] -> TExpr (Maybe b)
lookup = bindAp2 fLookup

fMapMaybe :: TFunction ((Arg (Arg a -> b), Arg (Maybe a)) -> Maybe b)
fMapMaybe = function2 "mapMaybe" ("f", "ma") $ \(f, ma) ->
	return $ inlineCaseMaybe ma nothing (\a -> just $ ap1 f a)

while :: TExpr (NoArgs -> Maybe a) -> ExprM (TExpr a)
while expr =
	withRec "loopFunc" 
		(\loopFunc -> 
			lam0 $ with "testElt" (return $ ap0 expr) $ \testElt ->
				ifm (isNothing testElt) (return $ ap0 loopFunc) (return $ fromJust testElt))
		(\loopFunc ->
			return $ ap0 loopFunc)

infixr 0 |>, |>=
infix 1 :==, :=

data Stmt a where
	(:=) :: Ref r -> TExpr r -> Stmt a
	Break :: TExpr Bool -> TExpr a -> Stmt a
	DoneIf :: TExpr (Maybe a) -> Stmt a
	Trace :: TExpr dbg -> Stmt a

data BindingExpr a = 
	String :== TExpr a
	
(|>) :: Stmt a -> TExpr a -> TExpr a
(r := v)            |> e = assign r v e
Break condition res |> e = ifv condition res e
DoneIf condition    |> e = fromMaybe condition e 
-- Trace dbg           |> e = debugTrace dbg e
Trace _             |> e = e

(|>=) :: BindingExpr a -> (TExpr a -> TExpr b) -> TExpr b
(name :== val) |>= k = runIdentity $ with name (return val) (return . k)

kUp, kRight, kDown, kLeft :: TExpr Int
kUp = 0
kRight = 1
kDown = 2
kLeft = 3

inlineManhattan :: TExpr Coord -> TExpr Coord -> TExpr Int
inlineManhattan p1 p2 = abs (fst p1 - fst p2) + abs (snd p1 - snd p2)

inlineSubPos :: TExpr Coord -> TExpr Coord -> TExpr Coord
inlineSubPos p1 p2 = pair (fst p1 - fst p2) (snd p1 - snd p2)

fastMapLookup :: TExpr ParsedState -> TExpr Int -> TExpr Int -> TExpr Int
fastMapLookup = ap3 $ unsafeGlobalName "fastMapLookup"

fManhattanDir :: TFunction ((Arg Coord, Arg Coord) -> Direction)
fManhattanDir = function2 "manhattanDir" ("src", "target") $ \(src,target) -> return $ inlineManhattanDir src target


manhattanDir :: TExpr Coord -> TExpr Coord -> TExpr Direction
manhattanDir = bindAp2 fManhattanDir

inlineManhattanDir :: TExpr Coord -> TExpr Coord -> TExpr Direction
inlineManhattanDir src target =
	runIdentity $ with "manhattanTmpDiff" (return $ inlineSubPos target src) $ \diff -> return $
		ifv (abs (fst diff) > abs (snd diff))
			(ifv (fst diff > 0)
				kRight
				kLeft
			)
			(ifv (snd diff > 0)
				kUp
				kDown
			)

fCoordEq :: TFunction ((Arg Coord, Arg Coord) -> Bool)
fCoordEq = function2 "coordEq" ("pos1", "pos2") $ \(pos1, pos2) -> return $
	(and (fst pos1 == fst pos2) (snd pos1 == snd pos2))
coordEq :: TExpr Coord -> TExpr Coord -> TExpr Bool
coordEq = bindAp2 fCoordEq

inlineCoordEq :: TExpr Coord -> TExpr Coord -> TExpr Bool
inlineCoordEq pos1 pos2 = (and (fst pos1 == fst pos2) (snd pos1 == snd pos2))

isImpassable :: TExpr ParsedState -> TExpr Coord -> TExpr Bool
isImpassable state coord = fastMapLookup state (fst coord) (snd coord) == 0

goUp, goDown, goLeft, goRight :: TExpr Coord -> TExpr Coord
goUp coord = pair (fst coord) (snd coord - 1)
goDown coord = pair (fst coord) (snd coord + 1)
goLeft coord = pair (fst coord - 1) (snd coord)
goRight coord = pair (fst coord + 1) (snd coord)

type Queue a = ([a], [a])

singletonQueue :: TExpr a -> TExpr (Queue a)
singletonQueue a = pair nil (cons a nil)

nullQueue :: TExpr (Queue a) -> TExpr Bool
nullQueue q = and (null $ fst q) (null $ snd q)

pushQueue :: TExpr a -> TExpr (Queue a) -> TExpr (Queue a)
pushQueue a q = pair (cons a (fst q)) (snd q)

popQueue :: TExpr (Queue a) -> TExpr (a, Queue a)
popQueue q = ifv (null $ snd q)
                 (reloadQueueAndPop $ reverse $ fst q)
				 (pair (head (snd q)) (pair (fst q) (tail (snd q))))

fReverseWithAccum :: TFunction ((Arg [a], Arg [a]) -> [a])
fReverseWithAccum = function2 "reverseWithAccum" ("acc", "list") $ \(acc, list) -> return $
	ifv (null list) 
		acc
	    (reverseWithAccum (cons (head list) acc) (tail list))
reverseWithAccum :: TExpr [a] -> TExpr [a] -> TExpr [a]
reverseWithAccum = bindAp2 fReverseWithAccum

reverse :: TExpr [a] -> TExpr [a]
reverse = reverseWithAccum nil

fReloadQueueAndPop :: TFunction (Arg [a] -> (a, Queue a))
fReloadQueueAndPop = function1 "reloadQueueAndPop" "q" $ \q -> return $
	pair (head q) (pair nil (tail q))
reloadQueueAndPop :: TExpr [a] -> TExpr (a, Queue a)
reloadQueueAndPop = bindAp1 fReloadQueueAndPop

fPathfind :: TFunction ((Arg Coord, Arg Coord, Arg Int, Arg ParsedState) -> (Direction,Int))
fPathfind = function4 "pathfind" ("src","target","maxSteps","map") $ \(src,target,maxSteps,map) ->
	with "defaultDir" (return $ inlineManhattanDir src target) $ \defaultDir ->
	with "defaultDist" (return $ inlineManhattan src target) $ \defaultDist ->
    -- refOpen :: Queue (Coord, Maybe PathTo)
	withMut "refOpen" (return $ singletonQueue (pair src nothing)) $ \refOpen ->
	-- refClosed :: [(Coord, Maybe PathTo)]
	withMut "refClosed" (return nil) $ \refClosed ->
	-- bestPos :: (Score, (Direction, Int))
	withMut "bestDir" (return $ pair defaultDist (pair defaultDir defaultDist)) $ \bestDir ->
	-- stepsRemaining :: Int
	withMut "stepsRemaining" (return maxSteps) $ \stepsRemaining ->
	-- visit :: Coord -> (Direction, Int) -> Maybe Direction
	with "visit" (lam2 ("location","dirDist") $ \(location, dirDist) -> return $
		let direction = fst dirDist in
		let distance = snd dirDist + 1 in

		-- if we find the target, we're done
		Break (ap2 (global fCoordEq) location target) (just $ just $ debugTrace (pair (int 3) (pair direction distance)) $ pair direction distance)	|>
		
		-- if we've already visited this location, ignore it
		Break (not $ isNothing $ lookup (pAp2_1 (global fCoordEq) location) $ deref refClosed) nothing		|>
		Break (not $ isNothing $ lookup (pAp2_1 (global fCoordEq) location) $ fst $ deref refOpen) nothing	|>
		Break (not $ isNothing $ lookup (pAp2_1 (global fCoordEq) location) $ snd $ deref refOpen) nothing	|>

		-- if this location is impassable, ignore it
		Break (isImpassable map location) nothing								|>

		-- If this is the best location we've found so far, remember it
		"score" :== inlineManhattan location target									|>= \score ->

		Trace (pair (int 0) $ pair score $ pair location dirDist)				|>

		DoneIf (ifv (score < fst (deref bestDir))
		            (assign bestDir (pair score $ pair direction (distance + score)) nothing)
					nothing)													|>

		-- Store into open set
		refOpen := pushQueue (pair location $ just $ pair direction distance) (deref refOpen)	|>

		-- keep looking
		(nothing :: TExpr (Maybe (Maybe (Direction, Int))))

	) $ \visitor -> 
	do
		let visit = ap2 visitor
		while $ lazy $
			-- if we failed to find the target, give best direction to get closer
			Break (deref stepsRemaining <= 0) (just $ snd $ deref bestDir)	|>
			Break (nullQueue $ deref refOpen) (just $ snd $ deref bestDir)	|>

			-- decrement # of steps
			stepsRemaining := deref stepsRemaining - 1						|>

			-- grab the next node from the open stack
			"qpop" :== popQueue	(deref refOpen)								|>= \qpop ->
			"cur" :== (fst qpop)											|>= \cur ->
			"curPos" :== (fst cur)											|>= \curPos ->
			"prev" :== (snd cur)											|>= \prev ->

			-- and remove it
			refOpen := snd qpop												|>

			Trace (pair (int 1) $ pair (deref stepsRemaining) cur)			|>

			-- visit the four adjacent positions
			DoneIf (visit (goLeft curPos) (fromMaybe prev $ pair kLeft 0))	|>
			DoneIf (visit (goUp curPos) (fromMaybe prev $ pair kUp 0))		|>
			DoneIf (visit (goRight curPos) (fromMaybe prev $ pair kRight 0))|>
			DoneIf (visit (goDown curPos) (fromMaybe prev $ pair kDown 0))	|>

			-- add this position to the closed list
			refClosed := cons cur (deref refClosed)							|>

			-- keep looking
			nothing

pathfind :: TExpr Coord -> TExpr Coord -> TExpr Int -> TExpr ParsedState -> TExpr (Direction, Int)
pathfind = bindAp4 fPathfind

pathfindModule :: LModule
pathfindModule = newModule
	^^ fFoldMap
	^^ fFoldMap1
	^^ fAppendMaybe
	^^ fLookup
	^^ fMapMaybe
	^^ fManhattanDir
	^^ fCoordEq
	^^ fReverseWithAccum
	^^ fReloadQueueAndPop
	^^ fPathfind


{-
fTraverseMap :: TFunction (Arg (Monoid a), Arg ((Arg Coord, Arg MapTile) -> a), Arg MapState) -> a)
fTraverseMap = function3 "traverseMap" ("monoid", "elem", "map") $ \(monoid, elem, map) ->
	foldl1 
-}



