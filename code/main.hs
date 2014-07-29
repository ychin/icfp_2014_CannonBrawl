{-# LANGUAGE ForeignFunctionInterface #-}
module Main where
import Game_loader
import Game_controller
import Lambdaman_types
import Lambdaman_ai (aiCurrentLambdaMan)

import System.Console.ANSI
import System.IO
import Data.List.Split
import Text.Printf
import qualified Data.Vector as V
import Control.Monad.State.Lazy
import Control.Concurrent
import Data.Maybe
import System.Environment   
-- import Data.Vector (Vector, (!), (//))

------------------------------------------------------------------------
-- run main stuff
------------------------------------------------------------------------

simpleLambdaManProgram :: Monad m => LambdaManProgram m
--simpleLambdaManProgram = MkProgram () (\_ _ -> return ((), RIGHT)) (\_ -> "Program<GoRight>")
simpleLambdaManProgram = MkProgram () (\aiSt gmSt -> return $ aiCurrentLambdaMan aiSt gmSt) (\_ -> "Program<aiWalkToNearestPillOrFruit>")


runGame :: Monad m => GameStateM m -> m (GameStateM m)
runGame initialState = do
	let winState =  gdWinState $ gsGameData initialState
	result <- case winState of
		GAME_RUNNING	-> runTick initialState >>= runGame
		_				-> return initialState
	return result

runInteractive :: GameStateM IO -> IO (GameStateM IO)
runInteractive gameState = do
	threadDelay (50 * 1000)

	maybe_char <- getCharNonBlocking

	let old_run_state = gsRunState gameState
	let new_run_state = runStateFromChar old_run_state maybe_char

	let gameState2 = gameState { gsRunState = new_run_state }

	if (new_run_state == RUNSTATE_STEP && (isNothing maybe_char) && (old_run_state /= RUNSTATE_FIRST))
		then runInteractive gameState2
		else do
			let frozenState = freezeGameState gameState
			prettyPrint frozenState
			writeLog frozenState
			gameState3 <- runTick gameState2
			runInteractive gameState3

runStateFromChar :: RunState -> Maybe Char -> RunState
runStateFromChar _ (Just 'r') = RUNSTATE_GO
runStateFromChar _ (Just 'p') = RUNSTATE_STEP
runStateFromChar RUNSTATE_FIRST _ = RUNSTATE_STEP
runStateFromChar old_run_state _ = old_run_state

main :: IO ()
main = mainRunGame
--main = mainTestPreprocessor
--main = mainLoadGhost


mainTestPreprocessor :: IO () 
mainTestPreprocessor = do
	file_data <- readFile "ghosts//destroyer.ghc"
	let file_lines = lines file_data
	let processed_lines = preprocess_ghost_lines file_lines
	putStrLn (unlines processed_lines)
	putStrLn $ printf "lines=%d" (length processed_lines)
	_ <- getLine
	return ()

mainLoadGhost :: IO () 
mainLoadGhost = do
	ghost_code <- ghost_loader_load_file "ghosts//miner.ghc"
	putStrLn $ show ghost_code
	_ <- getLine
	return ()

mainRunGame :: IO ()
mainRunGame = do

	args <- getArgs

	if ((length args) > 0)
		then do
			let ghost_files = tail args
			let map_file = head args
			initial_game_state <- game_loader_load_file map_file ghost_files simpleLambdaManProgram
			end_game_state <- runGame initial_game_state
			printFinalState $ freezeGameState end_game_state
		else do
			let ghost_files = ["ghosts/runner.ghc", "ghosts/flanker3.ghc", "ghosts/flanker3.ghc", "ghosts/flanker3.ghc"]
			--let map_file = "maps/map_lambda.txt"
			--let map_file = "maps/map_ghost_busters.txt"
			--let map_file = "maps/map_proton_pack.txt"
			--let map_file = "maps/world-1.txt"
			--let map_file = "maps/map_one_ghost.txt"
			let map_file = "maps/map_four_ghost.txt"
			initial_game_state <- game_loader_load_file map_file ghost_files simpleLambdaManProgram
			logStart ghost_files
			_ <- runInteractive initial_game_state
			return ()
	return ()

	--let end_game_state = runGame initial_game_state
	--let end_game_state = runTick initial_game_state
	--prettyPrint end_game_state
	--_ <- getLine
	--return ()

getCharNonBlocking :: IO (Maybe Char)
getCharNonBlocking = do
	ready <- hReady stdin
	if (ready)
		then do 
			somev <- getChar
			return (Just somev)
		else return Nothing

------------------------------------------------------------------------
-- pretty printing
------------------------------------------------------------------------

findObjectAtIndex :: GameState -> Int -> Maybe Char
findObjectAtIndex gameState index = fst $ runState fn gameState where 
	fn = do 
		currentLocation <- getLambdaManLocation
		gameMap <- getMap
		let lambdaManIndex = getMapIndex gameMap currentLocation
		if (lambdaManIndex == index) 
			then return $ Just '\\'
			else do
				time <- getTime
				fruitLocation <- getFruitSpawnLocation
				let fruitLocationIndex = getMapIndex gameMap fruitLocation
				fruitStatus <- getFruitStatus
				if (fruitLocationIndex == index && fruitStatus - time > 0) 
					then return $ Just '%'
					else do
						ghostIndicies <- getGhostIndicies
						ghostPositions <- mapM getGhostLocation ghostIndicies
						let ghostPositionIndicies = map (getMapIndex gameMap) ghostPositions
						if (elem index ghostPositionIndicies)
							then return $ Just 'M'
							else return Nothing

mergeCharsAndObjects :: GameState -> String -> String
mergeCharsAndObjects gameState mapChars = result where
	charsAndIndicies = zip mapChars [0..]
	charsAndObjects = map makeObjects charsAndIndicies where
		makeObjects (char, index) = (char, maybeObj) where
			maybeObj = findObjectAtIndex gameState index
	result = flattenChars charsAndObjects where
		flattenChars = map flattenChar
		flattenChar (char, maybeObject) = case maybeObject of
			Nothing  -> char
			Just object -> object
	

prettyPrintBoard :: GameState -> IO ()
prettyPrintBoard gameState = do
	let gameMap = gdGameMap $ gsGameData gameState
	let mapChars = mapToChars $ gmGrid gameMap
	let mapCharsAndObject = mergeCharsAndObjects gameState mapChars 
	let mapLines = charsToLines mapCharsAndObject (fromIntegral $ gmWidth gameMap)
	mapM_ (colorizePutStrLn gameState) mapLines

charsToLines :: String -> Int -> [String]
charsToLines str stride = chunksOf stride str

game_state_in_fright_mode :: GameState -> Bool
game_state_in_fright_mode gameState = result where
	result = evalState inFrightMode gameState

colorizePutStrLn :: GameState -> String -> IO ()
colorizePutStrLn gameState line = do
	mapM_ (colorizeMapChar gameState) line
	putStrLn ""
	setSGR [SetColor Foreground Vivid White, SetColor Background Dull Black]

colorizeCharToColor :: GameState -> Char -> IO ()
colorizeCharToColor _gameState '#' = setSGR [SetColor Foreground Vivid Blue, SetColor Background Vivid Blue]
colorizeCharToColor _gameState '\\' = setSGR [SetColor Foreground Vivid Green, SetColor Background Dull Green]
colorizeCharToColor gameState 'M'
	| game_state_in_fright_mode gameState	= setSGR [SetColor Foreground Vivid Blue, SetColor Background Dull Yellow]
	| otherwise								= setSGR [SetColor Foreground Vivid Red, SetColor Background Dull Red]
colorizeCharToColor _gameState '%' = setSGR [SetColor Foreground Vivid Magenta, SetColor Background Dull Black]
colorizeCharToColor _gameState '.' = setSGR [SetColor Foreground Vivid White, SetColor Background Dull Black]
colorizeCharToColor _gameState 'o' = setSGR [SetColor Foreground Vivid Yellow, SetColor Background Dull Black]
colorizeCharToColor _gameState _ = setSGR [SetColor Foreground Vivid White, SetColor Background Dull Black]

colorizeMapChar :: GameState -> Char -> IO ()
colorizeMapChar gameState ch = do
	colorizeCharToColor gameState ch
	--let ch2 = if (ch == '\\') then (toEnum 2 :: Char) else ch
	putStr [ch]

elementToChar :: MapElement -> Char
elementToChar WALL			= '#'
elementToChar EMPTY			= ' '
elementToChar PILL			= '.'
elementToChar POWER_PILL	= 'o'
elementToChar FRUIT				= ' '
elementToChar LAMBDAMAN_START	= ' '
elementToChar GHOST_START		= ' '

mapToChars :: GameGrid -> String
mapToChars grid = V.toList $ V.map elementToChar grid

prettyPrintState :: GameState -> IO ()
prettyPrintState gameState = do
	let winState =  gdWinState $ gsGameData gameState
	let fruitState =  gdFruitStatus $ gsGameData gameState
	setSGR [SetColor Foreground Vivid Green]
	putStrLn $ printf "winstate: %s, fruitState: %s" (show winState) (show fruitState)

prettyPrintOutput :: GameState -> IO ()
prettyPrintOutput gameState = do
	let score =  lmScore $ gdLambdaManStatus $ gsGameData gameState
	let lives =  lmLives $ gdLambdaManStatus $ gsGameData gameState
	let ticks =  gdTicks $ gsGameData gameState
	setSGR [SetColor Foreground Vivid Red]
	putStrLn $ printf "Score: %d, Lives: %d, Ticks: %d" score lives ticks
	setSGR [SetColor Foreground Vivid White]
	putStrLn $ gsDebugString gameState

prettyPrint :: GameState -> IO ()
prettyPrint gameState = do
	setCursorPosition 0 0
	prettyPrintBoard gameState
	clearFromCursorToScreenEnd
	prettyPrintState gameState
	prettyPrintOutput gameState

------------------------------------------------------------------------
-- log
------------------------------------------------------------------------

logStart  :: [String] -> IO ()
logStart ghost_files = do
	writeFile "log.txt" "GAME START!\n"
	let ghost_filename = head ghost_files
	file_data <- readFile ghost_filename
	let file_lines = lines file_data
	let processed_lines = preprocess_ghost_lines file_lines
	appendFile "log.txt" (ghost_filename ++ "\n")
	appendFile "log.txt" (unlines processed_lines)
	appendFile "log.txt" (printf "lines:%d" (length processed_lines))

writeLog :: GameState -> IO ()
writeLog gameState = do
	let ticks =  gdTicks $ gsGameData gameState
	appendFile "log.txt" (printf "\n------------------------------------------------------------------\nTicks: %d\n" ticks)
	appendFile "log.txt" (gsLogString gameState)

printFinalState :: GameState -> IO ()
printFinalState game_state = do
	let game_data = gsGameData game_state
	let lmstatus = gdLambdaManStatus game_data
	let score = lmScore lmstatus
	let lives = lmLives lmstatus
	let ticks = gdTicks game_data
	let winState = gdWinState game_data
	putStrLn $ printf "ticks:%d score:%d lives:%d win:%s" ticks score lives (show winState)
