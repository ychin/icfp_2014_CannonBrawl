{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Quad_tree where
{-import Lmcompile-}
import Lambda_compiler_2
import LmLanguage
import Lmcompile
import Prelude (Num(..), ($), (.), String, Bool(..), Maybe(..), fromIntegral, Show(..), Monad(..), Int)
import Prelude (Either(..), putStrLn)

{-data Quad a = QuadLeaf a | Quad (Quad a) deriving (Show)-}
{-newtype Quad a = Quad (a, a, a, a) deriving (Show)-}

data QuadTree a = QuadDummy -- This is not a real data structure. It's just to make Haskell happy when using TExpr so we can type checking
                  {-| Leaf (TExpr a)-}
                  {-| Quad (QuadTree (TExpr a)) (QuadTree (TExpr a)) (QuadTree (TExpr a)) (QuadTree (TExpr a))-}
                  deriving (Show)

quadLeaf :: TExpr (QuadTree a) -> TExpr a
quadLeaf = unsafeCast

quadTL :: TExpr (QuadTree a) -> TExpr (QuadTree a)
quadTL tree = fst.fst $ (unsafeCast tree :: TExpr((a, a), (a, a)))

quadTR :: TExpr (QuadTree a) -> TExpr (QuadTree a)
quadTR tree = snd.fst $ (unsafeCast tree :: TExpr((a, a), (a, a)))

quadBL :: TExpr (QuadTree a) -> TExpr (QuadTree a)
quadBL tree = fst.snd $ (unsafeCast tree :: TExpr((a, a), (a, a)))

quadBR :: TExpr (QuadTree a) -> TExpr (QuadTree a)
quadBR tree = snd.snd $ (unsafeCast tree :: TExpr((a, a), (a, a)))

makeQuadLeaf :: TExpr a -> TExpr (QuadTree a)
makeQuadLeaf = unsafeCast

makeQuadFromLeaf :: TExpr a -> TExpr a -> TExpr a -> TExpr a -> TExpr (QuadTree a)
makeQuadFromLeaf tl tr bl br = unsafeCast $ pair (pair tl tr) (pair bl br)

makeQuad :: TExpr (QuadTree a) -> TExpr (QuadTree a) -> TExpr (QuadTree a) -> TExpr (QuadTree a) -> TExpr (QuadTree a)
makeQuad tl tr bl br = unsafeCast $ pair (pair tl tr) (pair bl br)

fFindQuadLeaf :: TFunction ((Arg (QuadTree a), Arg Int, Arg (Int, Int)) -> a) -- map size is one-dim for now just for simplification
fFindQuadLeaf = function3 "findQuadLeaf" ("quadTree", "quadTreeSize", "pos") $ \(quadTree, quadTreeSize, pos) -> return $
    ifv (quadTreeSize == 1)
        (quadLeaf quadTree) $
        let posX = fst pos
            posY = snd pos
            halfQuadTreeSize = quadTreeSize / 2
            setPos newX newY = pair newX newY
        in  (ifv (posY < halfQuadTreeSize)
                (ifv (posX < halfQuadTreeSize)
                    (findQuadLeaf (quadTL quadTree) halfQuadTreeSize pos)
                    (findQuadLeaf (quadTR quadTree) halfQuadTreeSize (setPos (posX - halfQuadTreeSize) posY)) )
                (ifv (posX < halfQuadTreeSize) -- posY >= halfQuadTreeSize
                    (findQuadLeaf (quadBL quadTree) halfQuadTreeSize (setPos posX (posY - halfQuadTreeSize)))
                    (findQuadLeaf (quadBR quadTree) halfQuadTreeSize (setPos (posX - halfQuadTreeSize) (posY - halfQuadTreeSize))) ))
            {-(findQuadLeaf quadTree (quadTreeSize / 2) pos)-}
findQuadLeaf = bindAp3 fFindQuadLeaf

fReplaceQuadLeaf :: TFunction ((Arg (QuadTree a), Arg Int, Arg a, Arg (Int, Int)) -> (QuadTree a))
fReplaceQuadLeaf = function4 "replaceQuadLeaf" ("quadTree", "quadTreeSize", "newValue", "pos") $ \(quadTree, quadTreeSize, newValue, pos) -> return $
    ifv (quadTreeSize == 1)
        (makeQuadLeaf newValue) $
        let posX = fst pos
            posY = snd pos
            halfQuadTreeSize = quadTreeSize / 2
            setPos newX newY = pair newX newY
            tl = quadTL quadTree
            tr = quadTR quadTree
            bl = quadBL quadTree
            br = quadBR quadTree
        in  (ifv (posY < halfQuadTreeSize)
                (ifv (posX < halfQuadTreeSize)
                    (makeQuad (replaceQuadLeaf (quadTL quadTree) halfQuadTreeSize newValue pos) tr bl br)
                    (makeQuad tl (replaceQuadLeaf (quadTR quadTree) halfQuadTreeSize newValue (setPos (posX - halfQuadTreeSize) posY)) bl br) )
                (ifv (posX < halfQuadTreeSize) -- posY >= halfQuadTreeSize
                    (makeQuad tl tr (replaceQuadLeaf (quadBL quadTree) halfQuadTreeSize newValue (setPos posX (posY - halfQuadTreeSize))) br)
                    (makeQuad tl tr bl (replaceQuadLeaf (quadBR quadTree) halfQuadTreeSize newValue (setPos (posX - halfQuadTreeSize) (posY - halfQuadTreeSize)))) ))
replaceQuadLeaf = bindAp4 fReplaceQuadLeaf

fAllocateQuadTree :: TFunction ((Arg Int, Arg Int) -> QuadTree Int) -- map size is one-dim for now just for simplification
fAllocateQuadTree = function2 "fAllocateQuadTree" ("quadTreeSize", "initialValue") $ \(quadTreeSize, initialValue) -> return $
	ifv (quadTreeSize == 1)
		(makeQuadLeaf initialValue)
		(let halfQuadTreeSize = quadTreeSize / 2
		in  (makeQuad
				(allocateQuadTree halfQuadTreeSize initialValue)
				(allocateQuadTree halfQuadTreeSize initialValue)
				(allocateQuadTree halfQuadTreeSize initialValue)
				(allocateQuadTree halfQuadTreeSize initialValue)))
allocateQuadTree = bindAp2 fAllocateQuadTree

----------------------------------------
-- Test Cases
----------------------------------------

{-fTestQuadTree :: TFunction (NoArgs -> [(Bool, Int)])-}
{-fTestQuadTree :: TFunction (NoArgs -> QuadTree Int)-}
fTestQuadTree :: TFunction (NoArgs -> Int)
fTestQuadTree = function0 "testQuadTree" $ do
	let	quadTreeTL = makeQuadFromLeaf (int 1) (int 2) (int 3) (int 4)
		quadTreeTR = makeQuadFromLeaf (int 11) (int 12) (int 13) (int 14)
		quadTreeBL = makeQuadFromLeaf (int 21) (int 22) (int 23) (int 24)
		quadTreeBR = makeQuadFromLeaf (int 31) (int 32) (int 33) (int 34)
		quadTreeBig = makeQuad quadTreeTL quadTreeTR quadTreeBL quadTreeBR
		quadTreeBigger = makeQuad quadTreeBig quadTreeBig quadTreeBig quadTreeBig
		quadTreeSize = (int 8)
		_result1 = findQuadLeaf quadTreeBigger quadTreeSize (pair 6 3)
		_result2 = findQuadLeaf quadTreeBigger quadTreeSize (pair 4 3)
		_result3 = findQuadLeaf quadTreeBigger quadTreeSize (pair 3 4)
		_newQuadTree = replaceQuadLeaf quadTreeBigger quadTreeSize (int 100) (pair 6 3)
		_result1' = findQuadLeaf _newQuadTree quadTreeSize (pair 6 3)

		newTestCase = allocateQuadTree 23 0
		_afterSet = replaceQuadLeaf newTestCase 23 (int 1) (pair 7 14)
		_findCoord = findQuadLeaf _afterSet (int 23) (pair 7 14)

		in
		return $
			roundUpToPower2 (int 8)
			{-afterSet-}
			{-(allocateQuadTree (int 8)(int 10))-}
			{-cons (pair (result1' == (int 33)) result1')-}
			{-(cons (pair (result2 == (int 23)) result2)-}
			{-(cons (pair (result3 == (int 12)) result3)-}
			{-nil))-}

testQuadTreeProg :: LProgram
testQuadTreeProg = mkProgram "testQuadTree"
				    $ newModule
                    ^^ fTestQuadTree
                    ^^ fFindQuadLeaf
                    ^^ fReplaceQuadLeaf
					^^ fAllocateQuadTree
					^^ fRoundUpToPower2

testQuadTree :: Either CompileError [LCInstr EnvAddr Address]
testQuadTree = compileAndLink $ testQuadTreeProg

mainTestQuadTree = printAsm testQuadTree
{-main = mainTestQuadTree-}

