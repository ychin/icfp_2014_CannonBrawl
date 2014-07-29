{-# LANGUAGE FlexibleInstances #-}
module Lambda_compiler_2 where
import Prelude (Num(..), ($), (.), String, Bool(..), Maybe(..), error, fromIntegral, Show(..), Monad(..), Int)
import LmLanguage

testProg :: LProgram
testProg = mkProgram "main" $ newModule ^^ fTail ^^ fHead ^^ fNth ^^ fNull ^^ fMain ^^ fUpTo

fTail :: TFunction (Arg [a] -> [a])
fTail = function1 "tail" "xs" $ \xs -> return $ tail xs

fHead :: TFunction (Arg [a] -> a)
fHead = function1 "head" "xs" $ \xs -> return $ head xs

fNull :: TFunction (Arg [a] -> Bool)
fNull = function1 "null" "xs" $ \xs -> return $ null xs

fNth :: TFunction ((Arg [a], Arg Int) -> a)
fNth = function2 "nth" ("xs", "n") $ \(xs, n) -> return $
	ifv (n <= 0)
	    (head xs)
		(nth (tail xs) (n - 1))
nth :: TExpr [a] -> TExpr Int -> TExpr a
nth = ap2 (global fNth)

fUpTo :: TFunction (Arg Int -> [Int])
fUpTo = function1 "upto" "n" $ \n -> do
	withRec "upToN" (\upToN -> lam1 "v" $ \v -> return $ ifv (v > n) nil (cons v $ ap1 upToN (v+1)))
		$ \upToN -> return $ ap1 upToN 0
upTo :: TExpr Int -> TExpr [Int]
upTo = ap1 (global fUpTo)


fMap :: TFunction ((Arg (Arg a -> b), Arg [a]) -> [b])
fMap = function2 "map" ("f","xs") $ \(f,xs) -> do
	ifm (null xs) (return nil)
	    (return $ cons (ap1 f (head xs)) (map f (tail xs)))
map :: TExpr (Arg a -> b) -> TExpr [a] -> TExpr [b]
map = bindAp2 fMap

-- These are slow! Just use judiciously
-- This only works up to 256!!
fRoundUpToPower2 :: TFunction(Arg Int -> Int)
fRoundUpToPower2 = function1 "roundUpToPower2" "num" $ \num -> return $
    ifv (or9 (num == 1) (num == 2) (num == 4) (num == 8) (num == 16) (num == 32) (num == 64) (num == 128) (num == 256))
        num
        (ifv (num < 128)
            (ifv (num < 64)
                (ifv (num < 32)
                    (ifv (num < 16)
                        (ifv (num < 8)
                            (ifv (num < 4)
                                (ifv (num < 2)
                                    2
                                    4)
                                8)
                            16)
                        32)
                    64)
                128)
            256)
roundUpToPower2 :: TExpr Int -> TExpr Int
roundUpToPower2 = bindAp1 fRoundUpToPower2

fMain :: TFunction (NoArgs -> Int)
fMain = function0 "main" $ do
	return $ nth (upTo 5) 3

