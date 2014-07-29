{-# LANGUAGE GADTs #-} -- for LambdamanProgram
module Lambdaman_types
where
import Data.Word
import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.List (unfoldr)
import Control.Monad.Identity

type CPUByte					= Word8
data GamePosition				= GamePosition {x::CPUByte, y::CPUByte} deriving (Show, Eq, Ord)
data GameDirection				= UP | RIGHT | DOWN | LEFT
								  deriving (Show, Eq, Ord, Bounded, Enum)
data MapElement					= WALL | EMPTY | PILL | POWER_PILL | FRUIT | LAMBDAMAN_START | GHOST_START
								  deriving (Show, Eq, Ord, Bounded, Enum)

type ArrayMutability s			= Vector s

data RunState					= RUNSTATE_GO | RUNSTATE_STEP | RUNSTATE_FIRST
								  deriving (Show, Eq, Ord, Bounded, Enum)
data GameStateM m				= GameState {
									gsGameData::GameData, 
									gsGhostCPU::[GhostCPU], 
									gsLambdaManProgram::LambdaManProgram m,
									gsDebugString::String,
									gsLogString::String,
									gsRunState::RunState
								}
								  deriving (Show)

type GameState = GameStateM Identity

freezeGameState :: GameStateM m -> GameState
freezeGameState gs = gs { gsLambdaManProgram = MkProgram () (\_ _ -> return ((), RIGHT)) (\_ -> "frozen") }

data GhostCPU					= GhostCPU {
									gcGhostVitality :: GhostVitality,
									gcGhostLocation :: GhostLocation,
									gcGhostDirection :: GhostDirection,
									gcGhostSpawnLocations :: GhostLocation,
									gcProgramCounter :: ProgramCounter, 
									gcRegisters :: GhostRegisters, 
									gcCodeMemory :: GhostCodeMemory, 
									gcDataMemory :: GhostDataMemory,
									gesGhostIndex :: CPUByte
								}
									deriving (Show, Eq)

data GameData					= GameData {
									gdGameMap::GameMap, 
									gdLambdaManStatus::LambdaManStatus,
									gdLambdaManStatus2::LambdaManStatus, 
									gdTicks::Time,
									gdWinState::GameWinState,
									gdEventQueue::GameEventQueue,
									gdFruitStatus::Time
								}
								  deriving (Show, Eq)

data GameEventType				= EVENT_GAME_TIMEOUT
								| EVENT_LAMBDAMAN_MOVE
								| EVENT_GHOST_MOVE Int
								| EVENT_FRUIT_SPAWN
								| EVENT_FRUIT_DESPAWN
								| EVENT_FRIGHT_MODE_END
									deriving (Show, Eq, Ord)

data GameEvent					= GameEvent { 
									geTime::Time, 
									geEvent::GameEventType 
								}
									deriving (Show, Eq)

type GameEventQueue				= [GameEvent]

data GameWinState				= GAME_RUNNING
								| GAME_WIN
								| GAME_LOSS
								  deriving (Show, Eq)

data GameMap					= GameMap {
									gmWidth::CPUByte, 
									gmHeight::CPUByte, 
									gmGrid::GameGrid,
									gmLambdaManSpawnLocation::LambdaManLocation,
									gmFruitSpawnLocation::GamePosition
								}
								  deriving (Show, Eq)
type GameGrid					= ArrayMutability MapElement

type LambdaManScore				= Int
data LambdaManStatus			= LambdaManStatus {
									lmVitality::LambdaManVitality, 
									lmLocation::LambdaManLocation, 
									lmDirection::LambdaManDirection, 
									lmLives::LambdaManLives, 
									lmScore::LambdaManScore,
									lmGhostChain::Int
								}
								  deriving (Show, Eq)
type LambdaManVitality		    = Int
type LambdaManLocation		    = GamePosition
type LambdaManDirection		    = GameDirection
type LambdaManLives				= CPUByte

data GhostVitality				= STANDARD | FRIGHT_MODE | INVISIBLE
								  deriving (Show, Eq, Ord, Bounded, Enum)
type GhostLocation				= GamePosition
type GhostDirection				= GameDirection

type FruitStatus				= CPUByte

type Time						= Int

type GhostCodeMemory			= Vector GhostInstruction
data GhostInstruction			= G_MOV GhostArg GhostArg 
								| G_INC GhostArg 
								| G_DEC GhostArg 
								| G_ADD GhostArg GhostArg 
								| G_SUB GhostArg GhostArg 
								| G_MUL GhostArg GhostArg 
								| G_DIV GhostArg GhostArg 
								| G_AND GhostArg GhostArg
								| G_OR GhostArg GhostArg 
								| G_XOR GhostArg GhostArg 
								| G_JLT GhostArg GhostArg GhostArg
								| G_JEQ GhostArg GhostArg GhostArg
								| G_JGT GhostArg GhostArg GhostArg
								| G_INT GhostArg
								| G_HLT
								  deriving (Show, Eq)

data GhostRegister				= GREG_A | GREG_B | GREG_C | GREG_D | GREG_E | GREG_F | GREG_G | GREG_H | GREG_PC
								  deriving (Show, Eq, Ord, Bounded, Enum)

data GhostArg					= GARG_REGISTER GhostRegister
								| GARG_PTR_REGISTER GhostRegister
								| GARG_CONSTANT CPUByte
								| GARG_PTR_CONSTANT CPUByte
								  deriving (Show, Eq)

type ProgramCounter				= CPUByte
type GhostRegisters				= Vector CPUByte
type GhostDataMemory			= Vector CPUByte

type LambdaManMove				= GameDirection
data LambdaManProgram m where
    MkProgram :: programState                                                 -- initial state
	          -> (programState -> GameState -> m (programState, LambdaManMove)) -- state-updating function
			  -> (programState -> String)									  -- show function
			  -> LambdaManProgram m

instance Show (LambdaManProgram m) where 
    show (MkProgram progState _ showFunc) = showFunc progState

getMapIndex :: GameMap -> GamePosition -> Int
getMapIndex gameMap position = index where
	px = x position
	py = y position
	index = if (px < 0 || py < 0 || px >= gmWidth gameMap || py >= gmHeight gameMap)
		then error "position outside game map bounds"
		else (fromIntegral px) + (fromIntegral py) * fromIntegral (gmWidth gameMap)

mapToGridList :: GameMap -> [[MapElement]]
mapToGridList gameMap = map V.toList $ unfoldr go (gmGrid gameMap)
    where
		width = fromIntegral $ gmWidth gameMap
		go :: V.Vector MapElement -> Maybe (V.Vector MapElement, V.Vector MapElement)
		go v | V.null v    = Nothing
		     | otherwise = Just (V.splitAt width v)


