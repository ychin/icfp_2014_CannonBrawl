module Game_loader
where

import Lambdaman_types
--import Game_controller
import Lambdaman_ai

--import Control.Monad.State.Lazy
import Data.Vector (fromList, findIndex, findIndices, toList, replicate)
import qualified Data.Vector as V
import Data.List (group, sort)
--import Data.Ord
--import Debug.Trace
--import qualified Data.Set as S
--import Text.Printf
import Text.Regex.Posix
--import Data.Graph.AStar
import Data.String.Utils

------------------------------------------------------------------------
-- Lambda-Man Loader
------------------------------------------------------------------------

simpleGhostProgram :: GhostCodeMemory
simpleGhostProgram = Data.Vector.replicate 256 G_HLT

{-
simpleGhostProgram = [
	G_INT 
	G_INT 0 0

pac_x, pac_y = getPlayerPos
idx  = getMeIndex
ghost_x, ghost_y = getGhostPos idx
diff_x, diff_y = sub pac ghost
abs_x, abx_y = abs diff
if (abs_x > abs_y)
	if diff_x > 0
		return RIGHT
	else 
		return LEFT
else
	if diff_y > 0
		return DOWN
	else 
		return UP

walkTowardsGhost = [
	INT 3 A,			-- A = ghostIndex
	INT 5 A B C,		-- B = ghostPos x, C = ghostPos y
	INT 1 D E,			-- D = pacPos x, E = pacPos y
	XOR D B,
	XOR E C,
	SUB D B,			-- D = abs x
	SUB E C,            -- E = abs y
	JGT xGTy, D,E

	INT 1 D E,			-- D = pacPos x, E = pacPos y
	JGT yGT0, 4,7
	INT 
	HLT
Label yGT0,
	INT 
	HLT
Label xGTy,
	JGT xGT0, 4,7
	INT 
	HLT
Label xGT0
	INT 
	HLT
]
-}

game_loader_load_file :: String -> [String] -> LambdaManProgram m -> IO (GameStateM m)
game_loader_load_file map_filename ghost_filenames lambda_man = do
  map_file <- readFile map_filename
  ghost_files <- mapM readFile ghost_filenames
  return (loader_create_game_state map_file ghost_files lambda_man)

makeGhostEvents :: [GhostCPU] -> [GameEvent]
makeGhostEvents ghost_cpu_list = results where
	moveTimes = cycle [130, 132, 134, 136]
	indicies = [0..]
	zipped = zip3 ghost_cpu_list indicies moveTimes
	results = map makeGhostEvent zipped where
		makeGhostEvent (_, index, moveTime) = GameEvent { geTime = moveTime, geEvent = EVENT_GHOST_MOVE index }
		
makeLambdaManEvents :: [GameEvent]
makeLambdaManEvents = [GameEvent { geTime = 127, geEvent = EVENT_LAMBDAMAN_MOVE }]

fruitDelay :: Time
fruitDelay = 127

makeFruitEvents :: [GameEvent]
makeFruitEvents = [
	GameEvent{ geTime = fruitDelay * 200, geEvent = EVENT_FRUIT_SPAWN},
	GameEvent{ geTime = fruitDelay * 280, geEvent = EVENT_FRUIT_DESPAWN },
	GameEvent{ geTime = fruitDelay * 400, geEvent = EVENT_FRUIT_SPAWN },
	GameEvent{ geTime = fruitDelay * 480, geEvent = EVENT_FRUIT_DESPAWN } 
	]

calcGameEnd :: GameMap -> Time
calcGameEnd game_map = result where
	mapWidth = fromIntegral $ gmWidth game_map
	mapHeight = fromIntegral $ gmHeight game_map
	result = 127 * mapWidth * mapHeight * 16

makeGameEndEvents :: GameMap -> [GameEvent]
makeGameEndEvents game_map = [ GameEvent { geTime = calcGameEnd game_map, geEvent = EVENT_FRIGHT_MODE_END } ]

initEventQueue :: GameMap -> [GhostCPU] -> GameEventQueue 
initEventQueue game_map ghost_cpu_list = results where
	ghostEvents		= makeGhostEvents ghost_cpu_list
	lambdaManEvents	= makeLambdaManEvents
	fruitEvents		= makeFruitEvents
	gameEndEvents	= makeGameEndEvents game_map
	results			= ghostEvents ++ lambdaManEvents ++ fruitEvents ++ gameEndEvents


loader_create_game_state :: String -> [String] -> LambdaManProgram m -> GameStateM m
loader_create_game_state map_file ghost_files lambda_man = result where
		ghost_programs = map loader_create_ghost_memory ghost_files :: [GhostCodeMemory]
		(ghost_cpu_list, game_map) = loader_read_map_file map_file ghost_programs
		game_data = GameData
			{
				gdGameMap = game_map, 
				gdLambdaManStatus = loader_create_lambda_man_status 1 game_map,
				gdLambdaManStatus2 = loader_create_lambda_man_status 2 game_map,
				gdTicks = 0,
				gdWinState = GAME_RUNNING,
				gdEventQueue = initEventQueue game_map ghost_cpu_list,
				gdFruitStatus = 0
			}
		lambda_man_program = lambda_man
		result = GameState
			{
				gsGameData = game_data, 
				gsGhostCPU = ghost_cpu_list, 
				gsLambdaManProgram = lambda_man_program,
				gsDebugString = "",
				gsLogString = "",
				gsRunState = RUNSTATE_FIRST
			}

loader_create_lambda_man_status :: Int -> GameMap -> LambdaManStatus
loader_create_lambda_man_status _which game_map = result where
		position = gmLambdaManSpawnLocation game_map
		result = LambdaManStatus
			{
				lmVitality = 0,
				lmLocation = position,
				lmDirection = DOWN,
				lmLives = 3, 
				lmScore = 0,
				lmGhostChain = 0
			}


loader_convert_map_char :: Char -> [MapElement]
loader_convert_map_char '#' = [WALL]
loader_convert_map_char '.' = [PILL]
loader_convert_map_char 'o' = [POWER_PILL]
loader_convert_map_char '=' = [GHOST_START]
loader_convert_map_char ' ' = [EMPTY]
loader_convert_map_char '%' = [FRUIT]
loader_convert_map_char '\\' = [LAMBDAMAN_START]
loader_convert_map_char _ = []

loader_map_file_convert :: String -> [MapElement]
loader_map_file_convert = concatMap loader_convert_map_char
	
loader_read_grid :: String -> GameGrid
loader_read_grid map_file = result where
	result = fromList (loader_map_file_convert map_file)

loader_index_to_position :: Int -> Int -> GamePosition
loader_index_to_position width index = result where
	result = GamePosition { x = fromIntegral (index `mod` width), y = fromIntegral (index `quot` width) }

loader_find_start_location :: GameGrid -> Int -> GamePosition
loader_find_start_location game_grid width = result where
	maybe_index = findIndex (\a -> a == LAMBDAMAN_START) game_grid
	result 
		| Just index <- maybe_index		= loader_index_to_position width index
		| otherwise						= error "map doesn't have starting position"

loader_find_ghost_location :: GameGrid -> Int -> [GamePosition]
loader_find_ghost_location game_grid width = result where
	indices_vector = findIndices (\a -> a == GHOST_START) game_grid
	indices_list = toList indices_vector
	result = map (loader_index_to_position width) indices_list

loader_find_fruit_location :: GameGrid -> Int -> GamePosition
loader_find_fruit_location game_grid width = result where
	indices_vector = findIndices (\a -> a == FRUIT) game_grid
	indices_list = toList indices_vector
	positions_list = map (loader_index_to_position width) indices_list
	result = head positions_list

loader_ghost_cpus :: [GamePosition] -> [GhostCodeMemory] -> [GhostCPU]
loader_ghost_cpus positions ghost_programs = map (mkCpu ghost_programs) $ zip positions [0..]

mkCpu :: [GhostCodeMemory] -> (GamePosition, Int) -> GhostCPU
mkCpu ghost_programs (position, index) = result where
	num_ghost_programs = length ghost_programs
	program_index = index `mod` num_ghost_programs
	use_program = ghost_programs !! program_index :: GhostCodeMemory
	result = GhostCPU 
		{
			gcGhostVitality = STANDARD,
			gcGhostLocation = position,
			gcGhostDirection = DOWN,
			gcGhostSpawnLocations = position,
			gcProgramCounter = 0,
			gcRegisters =  Data.Vector.replicate 8 0, 
			gcCodeMemory = use_program,
			gcDataMemory = Data.Vector.replicate 256 0,
			gesGhostIndex = fromIntegral index
		}	

--get_ghost_start_time :: Int -> Time
--get_ghost_start_time ghost_index = case (ghost_index `mod` 4) of
--	0 -> 130
--	1 -> 132
--	2 -> 134
--	3 -> 136

loader_read_map_file :: String -> [GhostCodeMemory] -> ([GhostCPU], GameMap)
loader_read_map_file map_file ghost_programs = result where
	file_lines = lines (filter (/= '\r') map_file)
	width = length (head file_lines)
	height = length file_lines
	map_size = width * height
	game_grid = loader_read_grid map_file
	grid_length = V.length game_grid
	lamba_man_start_location = loader_find_start_location game_grid width
	ghost_start_location_list = loader_find_ghost_location game_grid width
	fruit_spawn_location = loader_find_fruit_location game_grid width
	ghost_cpu_list = loader_ghost_cpus ghost_start_location_list ghost_programs
	game_map = GameMap
		{
			gmWidth = fromIntegral width,
			gmHeight = fromIntegral height, 
			gmGrid = game_grid,
			gmLambdaManSpawnLocation = lamba_man_start_location,
			gmFruitSpawnLocation = fruit_spawn_location
		}
	result = if (grid_length /= map_size) 
		then error "map size doesn't match num chars read from map"
		else (ghost_cpu_list, game_map)

--clean_grid :: GameGrid -> GameGrid 
--clean_grid grid = V.map removeStartLocations grid 
--removeStartLocations :: MapElement -> MapElement
--removeStartLocations FRUIT				= EMPTY
--removeStartLocations LAMBDAMAN_START	= EMPTY
--removeStartLocations GHOST_START		= EMPTY
--removeStartLocations element			= element

------------------------------------------------------------------------
-- Ghost Code (ghc) Loader
------------------------------------------------------------------------

ghost_loader_load_file :: String -> IO GhostCodeMemory
ghost_loader_load_file ghost_filename = do
  ghost_file <- readFile ghost_filename
  return (loader_create_ghost_memory ghost_file)

loader_create_ghost_memory :: String -> GhostCodeMemory
loader_create_ghost_memory file_data = result where
	file_lines = lines file_data
	processed_lines = preprocess_ghost_lines file_lines
	instruction_list = loader_load_ghost_lines processed_lines
	result = fromList instruction_list

type LabelItems = (String,String)
type LabelDictionary = [LabelItems]

preprocess_apply_a_label :: LabelItems -> String -> String
preprocess_apply_a_label (label,label_replacement) line = replace label label_replacement line

preprocess_apply_labels_line :: LabelDictionary -> String -> String
preprocess_apply_labels_line [] line = line
preprocess_apply_labels_line (label_item:xs) line = (preprocess_apply_a_label label_item (preprocess_apply_labels_line xs line))

preprocess_apply_labels :: LabelDictionary -> [String] -> [String]
preprocess_apply_labels _ [] = []
preprocess_apply_labels label_dict (line:xs) = (preprocess_apply_labels_line label_dict line) : (preprocess_apply_labels label_dict xs)

preprocess_find_label :: String -> Int -> (LabelDictionary, [String], Int)
preprocess_find_label line line_num
	| [[_,label]] <- line =~ "^([a-zA-Z0-9_]+:)" :: [[String]]	= ([(label,show line_num)], [], line_num)
	| otherwise													= ([], [strip(head (split ";" line))], line_num + 1)

preprocess_make_label_dict :: [String] -> Int -> (LabelDictionary, [String])
preprocess_make_label_dict [] _ = ([], [])
preprocess_make_label_dict (line:xs) line_num = result where
	(label_new, line_new, line_num_next) = preprocess_find_label line line_num
	(labels_rest, lines_rest) = preprocess_make_label_dict xs line_num_next
	result = (label_new ++ labels_rest, line_new ++ lines_rest)

preprocess_find_macro :: String -> [String]
preprocess_find_macro line = result where
	regex_matches = line =~ "(\\$[a-zA-Z0-9_]+)" :: [[String]]	
	result = map head regex_matches

preprocess_make_macro_list :: [String] -> [String]
preprocess_make_macro_list [] = []
preprocess_make_macro_list (line:xs) = (preprocess_find_macro line) ++ (preprocess_make_macro_list xs)

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

preprocess_make_macro_dict :: [String] -> LabelDictionary
preprocess_make_macro_dict file_lines = result where
	macro_list = preprocess_make_macro_list file_lines
	unique_macro_list = rmdups macro_list
	int_list = [0..] :: [Int]
	dict = zip unique_macro_list ["[" ++ (show i) ++ "]" | i <- int_list]
	result = dict

preprocess_add_macros :: [String] -> [String]
preprocess_add_macros [] = []
preprocess_add_macros (line:xs)
	| line == "ret"														= "mov pc,[h]" : preprocess_add_macros xs
	| [[_,label]] <- line =~ "^call ([a-zA-Z0-9_]+:)" :: [[String]]		= [  "dec h"  ,  "mov [h],pc"  ,  "add [h],3"  ,  "mov pc,"++label , "inc h"] ++ preprocess_add_macros xs
	| otherwise															= line : preprocess_add_macros xs

preprocess_trim :: [String] -> [String]
preprocess_trim [] = []
preprocess_trim (line:xs)
	| [[_]] <- (strip line) =~ "^;" :: [[String]]				= preprocess_trim xs
	| (strip line) == ""										= preprocess_trim xs
	| otherwise													= strip(head (split ";" line)) : preprocess_trim xs

preprocess_ghost_lines :: [String] -> [String]
preprocess_ghost_lines ghost_lines = result where
	trimmed_line = preprocess_trim ghost_lines
	macro_line = preprocess_add_macros trimmed_line 
	(dict1, lines2) = preprocess_make_label_dict macro_line 0
	dict2 = preprocess_make_macro_dict lines2
	complete_dict = dict1 ++ dict2
	labelled_lines = preprocess_apply_labels complete_dict lines2
	result = labelled_lines


loader_load_ghost_lines :: [String] -> [GhostInstruction]
loader_load_ghost_lines [] = []
loader_load_ghost_lines (xx:xs) = (loader_ghost_read_line xx) ++ (loader_load_ghost_lines xs)

loader_parse_arg :: String -> GhostArg
loader_parse_arg "a" = GARG_REGISTER GREG_A
loader_parse_arg "b" = GARG_REGISTER GREG_B
loader_parse_arg "c" = GARG_REGISTER GREG_C
loader_parse_arg "d" = GARG_REGISTER GREG_D
loader_parse_arg "e" = GARG_REGISTER GREG_E
loader_parse_arg "f" = GARG_REGISTER GREG_F
loader_parse_arg "g" = GARG_REGISTER GREG_G
loader_parse_arg "h" = GARG_REGISTER GREG_H
loader_parse_arg "pc" = GARG_REGISTER GREG_PC
loader_parse_arg "[a]" = GARG_PTR_REGISTER GREG_A
loader_parse_arg "[b]" = GARG_PTR_REGISTER GREG_B
loader_parse_arg "[c]" = GARG_PTR_REGISTER GREG_C
loader_parse_arg "[d]" = GARG_PTR_REGISTER GREG_D
loader_parse_arg "[e]" = GARG_PTR_REGISTER GREG_E
loader_parse_arg "[f]" = GARG_PTR_REGISTER GREG_F
loader_parse_arg "[g]" = GARG_PTR_REGISTER GREG_G
loader_parse_arg "[h]" = GARG_PTR_REGISTER GREG_H
loader_parse_arg arg 
	| [[_, num]] <- arg =~ "\\[([0-9]+)\\]" :: [[String]]	= GARG_PTR_CONSTANT (read num)
	| [[_, num]] <- arg =~ "([0-9]+)" :: [[String]]			= GARG_CONSTANT (read num)
	| otherwise												= GARG_CONSTANT 99

re_arg :: String
re_arg = "(\\[?[a-z0-9]+\\]?)"

re_one_arg :: String
re_one_arg = " +" ++ re_arg

re_two_arg :: String
re_two_arg = " +" ++ re_arg ++ " *, *" ++ re_arg

re_three_arg :: String
re_three_arg = " +" ++ re_arg ++ " *, *" ++ re_arg ++ " *, *" ++ re_arg

loader_ghost_read_line :: String -> [GhostInstruction]
loader_ghost_read_line ghost_line
	| [[_, arg1, arg2]]			<- ghost_line =~ ("mov" ++ re_two_arg) :: [[String]]		= [G_MOV (loader_parse_arg arg1) (loader_parse_arg arg2)]
	| [[_, arg]]				<- ghost_line =~ ("inc" ++ re_one_arg) :: [[String]]		= [G_INC (loader_parse_arg arg)]
	| [[_, arg]]				<- ghost_line =~ ("dec" ++ re_one_arg) :: [[String]]		= [G_DEC (loader_parse_arg arg)]
	| [[_, arg1, arg2]]			<- ghost_line =~ ("add" ++ re_two_arg) :: [[String]]		= [G_ADD (loader_parse_arg arg1) (loader_parse_arg arg2)]
	| [[_, arg1, arg2]]			<- ghost_line =~ ("sub" ++ re_two_arg) :: [[String]]		= [G_SUB (loader_parse_arg arg1) (loader_parse_arg arg2)]
	| [[_, arg1, arg2]]			<- ghost_line =~ ("mul" ++ re_two_arg) :: [[String]]		= [G_MUL (loader_parse_arg arg1) (loader_parse_arg arg2)]
	| [[_, arg1, arg2]]			<- ghost_line =~ ("div" ++ re_two_arg) :: [[String]]		= [G_DIV (loader_parse_arg arg1) (loader_parse_arg arg2)]
	| [[_, arg1, arg2]]			<- ghost_line =~ ("and" ++ re_two_arg) :: [[String]]		= [G_AND (loader_parse_arg arg1) (loader_parse_arg arg2)]
	| [[_, arg1, arg2]]			<- ghost_line =~ ("^or" ++ re_two_arg) :: [[String]]		= [G_OR  (loader_parse_arg arg1) (loader_parse_arg arg2)]
	| [[_, arg1, arg2]]			<- ghost_line =~ ("xor" ++ re_two_arg) :: [[String]]		= [G_XOR (loader_parse_arg arg1) (loader_parse_arg arg2)]
	| [[_, arg1, arg2, arg3]]	<- ghost_line =~ ("jlt" ++ re_three_arg) :: [[String]]		= [G_JLT (loader_parse_arg arg1) (loader_parse_arg arg2) (loader_parse_arg arg3)]
	| [[_, arg1, arg2, arg3]]	<- ghost_line =~ ("jeq" ++ re_three_arg) :: [[String]]		= [G_JEQ (loader_parse_arg arg1) (loader_parse_arg arg2) (loader_parse_arg arg3)]
	| [[_, arg1, arg2, arg3]]	<- ghost_line =~ ("jgt" ++ re_three_arg) :: [[String]]		= [G_JGT (loader_parse_arg arg1) (loader_parse_arg arg2) (loader_parse_arg arg3)]
	| [[_, arg]]				<- ghost_line =~ ("int" ++ re_one_arg) :: [[String]]		= [G_INT (loader_parse_arg arg)]
	| [[_]]						<- ghost_line =~ "hlt"	:: [[String]]						= [G_HLT]
	| [[_]]						<- ghost_line =~ ";"	:: [[String]]						= []
	| ghost_line == ""																		= []
	| otherwise																				= [G_INT (GARG_CONSTANT 99)]


