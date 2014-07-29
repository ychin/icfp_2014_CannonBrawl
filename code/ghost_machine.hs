module Ghost_machine
where

import Lambdaman_types
import Data.Vector ((!), (//))
import Data.Bits
import Text.Printf
import Debug.Trace

data GhostExecuteStatus					= GhostExecuteRunning | GhostExecuteHalt | GhostExecuteError
										  deriving (Show, Eq, Ord, Bounded, Enum)
data GhostExecutionState				= GhostExecutionState 
										  {
											gesGhostCPU :: GhostCPU,
											gesOutputDirection :: GhostDirection,
											gesCyclesRemaining :: Int, 
											gesExecuteStatus :: GhostExecuteStatus,
											gesErrorMsg :: String,
											gesDebugMsg :: String,
											gesGameState :: GameState
										  }
										  deriving (Show)

------------------------------------------------------------------------
-- entry function
------------------------------------------------------------------------

ghost_execute_game_cycle :: GhostCPU -> GameState -> GhostExecutionState
ghost_execute_game_cycle ghostCPU game_state = result where
	ghostCPU_start = ghostCPU { gcProgramCounter = 0 }
	ghost_direction = gcGhostDirection ghostCPU
	execution_state_start = 
		GhostExecutionState
		{
			gesGhostCPU = ghostCPU_start, 
			gesOutputDirection = ghost_direction, 
			gesCyclesRemaining = 1024,
			--gesCyclesRemaining = 24,
			gesExecuteStatus = GhostExecuteRunning,
			gesErrorMsg = "OK\n",
			gesDebugMsg = "",
			gesGameState = game_state
		}
	result = ghost_execute execution_state_start
	
------------------------------------------------------------------------
-- Getters and Setters 
------------------------------------------------------------------------

execution_state_get_pc :: GhostExecutionState -> ProgramCounter
execution_state_get_pc execution_state = result where
	ghost_cpu = gesGhostCPU execution_state
	result = gcProgramCounter ghost_cpu

execution_state_get_instruction :: GhostExecutionState -> ProgramCounter -> GhostInstruction
execution_state_get_instruction execution_state pc = result where
	ghost_cpu = gesGhostCPU execution_state
	code_memory = gcCodeMemory ghost_cpu
	result = code_memory ! (fromIntegral pc)

execution_state_set_pc :: GhostExecutionState -> ProgramCounter -> GhostExecutionState
execution_state_set_pc execution_state pc_new = result where
	ghost_cpu = gesGhostCPU execution_state
	ghost_cpu_result = ghost_cpu { gcProgramCounter = pc_new }
	result = execution_state { gesGhostCPU = ghost_cpu_result }

------------------------------------------------------------------------
-- Debug
------------------------------------------------------------------------

ghost_set_error_string :: String -> GhostExecutionState -> GhostExecutionState
ghost_set_error_string str execution_state = result where
	error_msg_old = gesErrorMsg	execution_state
	error_msg_new = error_msg_old ++ "\n" ++ str 
	result = execution_state { gesErrorMsg = error_msg_new }

ghost_set_error :: String -> GhostExecutionState -> GhostExecutionState
ghost_set_error msg execution_state = result where
	execution_state2 = ghost_set_error_string msg execution_state
	result = execution_state2 { gesExecuteStatus = GhostExecuteError }

ghost_debug_string :: String -> GhostExecutionState -> GhostExecutionState
ghost_debug_string str execution_state = result where
	debug_msg_old = gesDebugMsg	execution_state
	_debug_msg_new = debug_msg_old ++ "\n" ++ str 
	result = execution_state { gesDebugMsg = _debug_msg_new }
	--result = execution_state

------------------------------------------------------------------------
-- Execution controller
------------------------------------------------------------------------

ghost_execute :: GhostExecutionState -> GhostExecutionState
ghost_execute execution_state = result where

	pc_start = execution_state_get_pc execution_state
	cycles_remaining_start = gesCyclesRemaining execution_state

	execution_state_after_instruction = ghost_execute_next_instruction execution_state

	pc_after_instruction = execution_state_get_pc execution_state_after_instruction

	execute_status_result = gesExecuteStatus execution_state_after_instruction

	pc_result = 
		if (pc_start == pc_after_instruction) then 
			(pc_start + 1) 
		else 
			pc_after_instruction

	execution_state_result1 = execution_state_set_pc execution_state_after_instruction pc_result

	cycles_remaining_result = cycles_remaining_start - 1;
	execution_state_result2 = execution_state_result1 { gesCyclesRemaining = cycles_remaining_result }

	result
		| cycles_remaining_result == 0						= ghost_set_error_string "ghost ran out of cycles" execution_state_result2
		| execute_status_result /= GhostExecuteRunning		= ghost_set_error_string "ghost done running" execution_state_result2
		| otherwise											= ghost_execute execution_state_result2			

ghost_execute_next_instruction :: GhostExecutionState -> GhostExecutionState
ghost_execute_next_instruction execution_state = result where
	pc = execution_state_get_pc execution_state
	instruction = execution_state_get_instruction execution_state pc
	execution_state2 = ghost_debug_string ("ghost exec " ++ (show instruction)) execution_state
	result = ghost_execute_instruction execution_state2 instruction

------------------------------------------------------------------------
-- Instructions Helpers
------------------------------------------------------------------------

ghost_read_memory :: CPUByte -> GhostExecutionState -> CPUByte
ghost_read_memory ptr execution_state = result where
	ghost_cpu = gesGhostCPU execution_state
	data_memory = gcDataMemory ghost_cpu
	result = data_memory ! (fromIntegral ptr)

ghost_read_reg :: GhostRegister -> GhostExecutionState -> CPUByte
ghost_read_reg reg execution_state = result reg where
	ghost_cpu = gesGhostCPU execution_state
	register_index = fromEnum reg
	registers = gcRegisters ghost_cpu
	result GREG_PC = gcProgramCounter ghost_cpu
	result _ = registers ! register_index

ghost_get_arg_value :: GhostArg -> GhostExecutionState -> CPUByte
ghost_get_arg_value arg execution_state = result arg where
	result (GARG_REGISTER reg) = ghost_read_reg reg execution_state
	result (GARG_PTR_REGISTER GREG_PC) = error "can't use PC as a ptr register"
	result (GARG_PTR_REGISTER reg) = ghost_read_memory (ghost_read_reg reg execution_state) execution_state
	result (GARG_CONSTANT constant) = constant
	result (GARG_PTR_CONSTANT constant) = ghost_read_memory constant execution_state

ghost_set_pc :: CPUByte -> GhostExecutionState -> GhostExecutionState
ghost_set_pc pc_new execution_state = result where
	ghost_cpu = gesGhostCPU execution_state
	ghost_cpu_new = ghost_cpu { gcProgramCounter = pc_new }
	--result = execution_state { gesGhostCPU = ghost_cpu_new }	
	result = ghost_debug_string ("pc=" ++ (show pc_new)) (execution_state { gesGhostCPU = ghost_cpu_new })


ghost_set_register_non_pc :: GhostRegister -> CPUByte -> GhostExecutionState -> GhostExecutionState
ghost_set_register_non_pc reg value execution_state = result where
	ghost_cpu = gesGhostCPU execution_state
	register_index = fromEnum reg
	registers = gcRegisters ghost_cpu
	registers_new = registers // [(register_index, value)]
	ghost_cpu_new = ghost_cpu { gcRegisters = registers_new }

	execution_state2 = ghost_debug_string ("set " ++ (show reg) ++ " = " ++ (show value)) execution_state

	result = execution_state2 { gesGhostCPU = ghost_cpu_new }	

ghost_set_register :: GhostRegister -> CPUByte -> GhostExecutionState -> GhostExecutionState
ghost_set_register reg value execution_state = result reg where
	result GREG_PC = ghost_set_pc value execution_state
	result _ = ghost_set_register_non_pc reg value execution_state

ghost_set_ptr_data :: CPUByte -> CPUByte -> GhostExecutionState -> GhostExecutionState
ghost_set_ptr_data ptr value execution_state = result where
	ghost_cpu = gesGhostCPU execution_state
	data_memory = gcDataMemory ghost_cpu
	data_memory_new = data_memory // [(fromIntegral ptr, value)]
	ghost_cpu_new = ghost_cpu { gcDataMemory = data_memory_new }
	result = execution_state { gesGhostCPU = ghost_cpu_new }	

ghost_set_ptr_register :: GhostRegister -> CPUByte -> GhostExecutionState -> GhostExecutionState
ghost_set_ptr_register ptr_reg value execution_state = result where
	ptr = ghost_read_reg ptr_reg execution_state
	result = ghost_set_ptr_data ptr value execution_state

ghost_set_arg_value :: GhostArg -> CPUByte -> GhostExecutionState -> GhostExecutionState
ghost_set_arg_value arg value execution_state = result arg where
	result (GARG_REGISTER reg) = ghost_set_register reg value execution_state
	result (GARG_PTR_REGISTER ptr_reg) = ghost_set_ptr_register ptr_reg value execution_state
	result (GARG_PTR_CONSTANT ptr_const) = ghost_set_ptr_data ptr_const value execution_state
	result _ = ghost_set_error "can't set a constant" execution_state

ghost_set_arg_value_not_pc :: GhostArg -> CPUByte -> GhostExecutionState -> GhostExecutionState
ghost_set_arg_value_not_pc arg value execution_state = result arg where
	result (GARG_REGISTER GREG_PC) = ghost_set_error "can't set pc" execution_state
	result _ = ghost_set_arg_value arg value execution_state

------------------------------------------------------------------------
-- Instructions
------------------------------------------------------------------------

ghost_execute_instruction  :: GhostExecutionState -> GhostInstruction -> GhostExecutionState
ghost_execute_instruction execution_state (G_MOV arg1 arg2) = ghost_mov arg1 arg2 execution_state
ghost_execute_instruction execution_state (G_INC arg1) = ghost_inc arg1 execution_state
ghost_execute_instruction execution_state (G_DEC arg1) = ghost_dec arg1 execution_state
ghost_execute_instruction execution_state (G_ADD arg1 arg2) = ghost_add arg1 arg2 execution_state
ghost_execute_instruction execution_state (G_SUB arg1 arg2) = ghost_sub arg1 arg2 execution_state
ghost_execute_instruction execution_state (G_MUL arg1 arg2) = ghost_mul arg1 arg2 execution_state
ghost_execute_instruction execution_state (G_DIV arg1 arg2) = ghost_div arg1 arg2 execution_state
ghost_execute_instruction execution_state (G_AND arg1 arg2) = ghost_and arg1 arg2 execution_state
ghost_execute_instruction execution_state (G_OR arg1 arg2) = ghost_or arg1 arg2 execution_state
ghost_execute_instruction execution_state (G_XOR arg1 arg2) = ghost_xor arg1 arg2 execution_state
ghost_execute_instruction execution_state (G_JLT arg1 arg2 arg3) = ghost_jlt arg1 arg2 arg3 execution_state
ghost_execute_instruction execution_state (G_JEQ arg1 arg2 arg3) = ghost_jeq arg1 arg2 arg3 execution_state
ghost_execute_instruction execution_state (G_JGT arg1 arg2 arg3) = ghost_jgt arg1 arg2 arg3 execution_state
ghost_execute_instruction execution_state (G_INT arg1) = ghost_int arg1 execution_state
ghost_execute_instruction execution_state G_HLT = ghost_hlt execution_state
	
ghost_mov :: GhostArg -> GhostArg -> GhostExecutionState -> GhostExecutionState
ghost_mov dest_arg source_arg execution_state = result where
	source_value = ghost_get_arg_value source_arg execution_state
	result = ghost_set_arg_value dest_arg source_value execution_state

ghost_inc :: GhostArg -> GhostExecutionState -> GhostExecutionState
ghost_inc arg execution_state = result where
	value = ghost_get_arg_value arg execution_state
	result = ghost_set_arg_value_not_pc arg (value + 1) execution_state

ghost_dec :: GhostArg -> GhostExecutionState -> GhostExecutionState
ghost_dec arg execution_state = result where
	value = ghost_get_arg_value arg execution_state
	result = ghost_set_arg_value_not_pc arg (value - 1) execution_state

ghost_add :: GhostArg -> GhostArg -> GhostExecutionState -> GhostExecutionState
ghost_add arg1 arg2 execution_state = result where
	value1 = ghost_get_arg_value arg1 execution_state
	value2 = ghost_get_arg_value arg2 execution_state
	result = ghost_set_arg_value_not_pc arg1 (value1 + value2) execution_state

ghost_sub :: GhostArg -> GhostArg -> GhostExecutionState -> GhostExecutionState
ghost_sub arg1 arg2 execution_state = result where
	value1 = ghost_get_arg_value arg1 execution_state
	value2 = ghost_get_arg_value arg2 execution_state
	result = ghost_set_arg_value_not_pc arg1 (value1 - value2) execution_state

ghost_mul :: GhostArg -> GhostArg -> GhostExecutionState -> GhostExecutionState
ghost_mul arg1 arg2 execution_state = result where
	value1 = ghost_get_arg_value arg1 execution_state
	value2 = ghost_get_arg_value arg2 execution_state
	result = ghost_set_arg_value_not_pc arg1 (value1 * value2) execution_state

ghost_div :: GhostArg -> GhostArg -> GhostExecutionState -> GhostExecutionState
ghost_div arg1 arg2 execution_state = result where
	value1 = ghost_get_arg_value arg1 execution_state
	value2 = ghost_get_arg_value arg2 execution_state
	result
		| value2 == 0 = ghost_set_error "divide by zero" execution_state
		| otherwise = ghost_set_arg_value_not_pc arg1 (value1 `quot` value2) execution_state

ghost_and :: GhostArg -> GhostArg -> GhostExecutionState -> GhostExecutionState
ghost_and arg1 arg2 execution_state = result where
	value1 = ghost_get_arg_value arg1 execution_state
	value2 = ghost_get_arg_value arg2 execution_state
	result = ghost_set_arg_value_not_pc arg1 (value1 .&. value2) execution_state

ghost_or :: GhostArg -> GhostArg -> GhostExecutionState -> GhostExecutionState
ghost_or arg1 arg2 execution_state = result where
	value1 = ghost_get_arg_value arg1 execution_state
	value2 = ghost_get_arg_value arg2 execution_state
	result = ghost_set_arg_value_not_pc arg1 (value1 .|. value2) execution_state

ghost_xor :: GhostArg -> GhostArg -> GhostExecutionState -> GhostExecutionState
ghost_xor arg1 arg2 execution_state = result where
	value1 = ghost_get_arg_value arg1 execution_state
	value2 = ghost_get_arg_value arg2 execution_state
	result = ghost_set_arg_value_not_pc arg1 (value1 `xor` value2) execution_state

ghost_jlt :: GhostArg -> GhostArg -> GhostArg -> GhostExecutionState -> GhostExecutionState
ghost_jlt targ arg1 arg2 execution_state = result where
	pc_new = ghost_get_arg_value targ execution_state
	value1 = ghost_get_arg_value arg1 execution_state
	value2 = ghost_get_arg_value arg2 execution_state
	result
		| value1 < value2	= ghost_set_register GREG_PC pc_new execution_state
		| otherwise			= execution_state

ghost_jeq :: GhostArg -> GhostArg -> GhostArg -> GhostExecutionState -> GhostExecutionState
ghost_jeq targ arg1 arg2 execution_state = result where
	pc_new = ghost_get_arg_value targ execution_state
	value1 = ghost_get_arg_value arg1 execution_state
	value2 = ghost_get_arg_value arg2 execution_state
	result
		| value1 == value2	= ghost_set_register GREG_PC pc_new execution_state
		| otherwise			= execution_state

ghost_jgt :: GhostArg -> GhostArg -> GhostArg -> GhostExecutionState -> GhostExecutionState
ghost_jgt targ arg1 arg2 execution_state = result where
	pc_new = ghost_get_arg_value targ execution_state
	value1 = ghost_get_arg_value arg1 execution_state
	value2 = ghost_get_arg_value arg2 execution_state
	result
		| value1 > value2	= ghost_set_register GREG_PC pc_new execution_state
		| otherwise			= execution_state

ghost_int :: GhostArg -> GhostExecutionState -> GhostExecutionState
ghost_int arg execution_state = result value where
	value = ghost_get_arg_value arg execution_state
	result 0 = ghost_int_0 execution_state
	result 1 = ghost_int_1 execution_state
	result 2 = ghost_int_2 execution_state
	result 3 = ghost_int_3 execution_state
	result 4 = ghost_int_4 execution_state
	result 5 = ghost_int_5 execution_state
	result 6 = ghost_int_6 execution_state
	result 7 = ghost_int_7 execution_state
	result 8 = ghost_int_8 execution_state
	result _ = ghost_set_error "bad int number" execution_state

ghost_hlt :: GhostExecutionState -> GhostExecutionState
ghost_hlt execution_state = result where
	result = execution_state { gesExecuteStatus = GhostExecuteHalt }

-- set ghost's new direction
ghost_int_0 :: GhostExecutionState -> GhostExecutionState
ghost_int_0 execution_state = result value where
	value = ghost_read_reg GREG_A execution_state
	result 0 = execution_state { gesOutputDirection = UP }
	result 1 = execution_state { gesOutputDirection = RIGHT }
	result 2 = execution_state { gesOutputDirection = DOWN }
	result 3 = execution_state { gesOutputDirection = LEFT }
	result _ = execution_state -- this isn't a error just a nop in the spec

-- get first Lambda-Man's coordinate
ghost_int_1 :: GhostExecutionState -> GhostExecutionState
ghost_int_1 execution_state = result where
	game_state = gesGameState execution_state
	game_data = gsGameData game_state
	lambdaman_status = gdLambdaManStatus game_data
	pos = lmLocation lambdaman_status
	execution_state2 = ghost_set_register GREG_A (x pos) execution_state	
	execution_state3 = ghost_set_register GREG_B (y pos) execution_state2	
	result = execution_state3

-- get second Lambda-Man's coordinate
ghost_int_2 :: GhostExecutionState -> GhostExecutionState
ghost_int_2 execution_state = result where
	game_state = gesGameState execution_state
	game_data = gsGameData game_state
	lambdaman_status = gdLambdaManStatus2 game_data
	pos = lmLocation lambdaman_status
	execution_state2 = ghost_set_register GREG_A (x pos) execution_state	
	execution_state3 = ghost_set_register GREG_B (y pos) execution_state2	
	result = execution_state3

-- get this ghost's index
ghost_int_3 :: GhostExecutionState -> GhostExecutionState
ghost_int_3 execution_state = result where
	ghost_cpu = gesGhostCPU execution_state
	ghost_index = gesGhostIndex ghost_cpu
	result = ghost_set_register GREG_A ghost_index execution_state	

-- get indexed ghost's starting coordinate
ghost_int_4 :: GhostExecutionState -> GhostExecutionState
ghost_int_4 execution_state = result where
	ghost_index = ghost_read_reg GREG_A execution_state
	ghost_index_int = fromIntegral ghost_index
	game_state = gesGameState execution_state
	ghost_list = gsGhostCPU game_state
	num_ghosts = length ghost_list
	indexed_ghost_data = ghost_list !! ghost_index_int
	ghost_stating_pos = gcGhostSpawnLocations indexed_ghost_data
	execution_state2 = ghost_set_register GREG_A (x ghost_stating_pos) execution_state	
	execution_state3 = ghost_set_register GREG_B (y ghost_stating_pos) execution_state2	
	result 
		| ghost_index_int > num_ghosts	= execution_state
		| otherwise						= execution_state3

-- get indexed ghost's current coordinate
ghost_int_5 :: GhostExecutionState -> GhostExecutionState
ghost_int_5 execution_state = result where
	ghost_index = ghost_read_reg GREG_A execution_state
	ghost_index_int = fromIntegral ghost_index
	game_state = gesGameState execution_state
	ghost_list = gsGhostCPU game_state
	num_ghosts = length ghost_list
	indexed_ghost_data = ghost_list !! ghost_index_int
	ghost_stating_pos = gcGhostLocation indexed_ghost_data
	execution_state2 = ghost_set_register GREG_A (x ghost_stating_pos) execution_state	
	execution_state3 = ghost_set_register GREG_B (y ghost_stating_pos) execution_state2	
	result 
		| ghost_index_int > num_ghosts	= execution_state
		| otherwise						= execution_state3

-- get indexed ghost's vitality and direction
ghost_int_6 :: GhostExecutionState -> GhostExecutionState
ghost_int_6 execution_state = result where
	ghost_index = ghost_read_reg GREG_A execution_state
	ghost_index_int = fromIntegral ghost_index
	game_state = gesGameState execution_state
	ghost_list = gsGhostCPU game_state
	num_ghosts = length ghost_list
	indexed_ghost_data = ghost_list !! ghost_index_int
	vitality = gcGhostVitality indexed_ghost_data
	vitality_byte = fromIntegral (fromEnum vitality) :: CPUByte
	direction = gcGhostDirection indexed_ghost_data
	direction_byte = fromIntegral (fromEnum direction) :: CPUByte
	execution_state2 = ghost_set_register GREG_A vitality_byte execution_state	
	execution_state3 = ghost_set_register GREG_B direction_byte execution_state2	
	result 
		| ghost_index_int > num_ghosts	= execution_state
		| otherwise						= execution_state3

-- get contents of map square
ghost_int_7 :: GhostExecutionState -> GhostExecutionState
ghost_int_7 execution_state = result where
	game_state = gesGameState execution_state
	game_data = gsGameData game_state
	game_map = gdGameMap game_data
	map_size_x = gmWidth game_map
	map_size_y = gmHeight game_map
	game_grid = gmGrid game_map
	map_x = ghost_read_reg GREG_A execution_state
	map_y = ghost_read_reg GREG_B execution_state
	map_position = GamePosition { x = map_x, y = map_y }
	map_index = getMapIndex game_map map_position
	map_cell_contents = game_grid ! map_index
	map_result
		| map_x >= map_size_x		= 0
		| map_y >= map_size_y		= 0
		| otherwise					= fromIntegral (fromEnum map_cell_contents)
	result = ghost_set_register GREG_A map_result execution_state	

ghost_int_8 :: GhostExecutionState -> GhostExecutionState
ghost_int_8 execution_state = result where
	reg_pc = ghost_read_reg GREG_PC execution_state
	reg_a = ghost_read_reg GREG_A execution_state
	reg_b = ghost_read_reg GREG_B execution_state
	reg_c = ghost_read_reg GREG_C execution_state
	reg_d = ghost_read_reg GREG_D execution_state
	reg_e = ghost_read_reg GREG_E execution_state
	reg_f = ghost_read_reg GREG_F execution_state
	reg_g = ghost_read_reg GREG_G execution_state
	reg_h = ghost_read_reg GREG_H execution_state
	str = printf "pc=%d a=%d b=%d c=%d d=%d e=%d f=%d g=%d h=%d" reg_pc reg_a reg_b reg_c reg_d reg_e reg_f reg_g reg_h
	result = ghost_set_error_string str execution_state
