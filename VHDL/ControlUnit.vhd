-- Control unit
-- Andrew Read
-- Created 22 May 2011

-- need delay of 1 cycle on READ from HW registers after setting address
-- all fetch and store needs 1 extra cycle to wait for TOS to be registered

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;

entity ControlUnit is
    Port ( rst : in STD_LOGIC;												-- reset
           clk : in STD_LOGIC;												-- clock
			  irq : in STD_LOGIC;												-- interrupt request
			  irv : in std_logic_vector(3 downto 0);						-- interrupt request vector  1 - 15
			  rti : out std_logic;												-- return from interrupt signal
			  TOS : in STD_LOGIC_VECTOR (31 downto 0);					-- Top Of Stack (TOS_n from datapath, one cycle ahead of registered value)
			  TOS_r : in STD_LOGIC_VECTOR (31 downto 0);					-- Top Of Stack (TOS from datapath, the registered value)
			  NOS : in STD_LOGIC_VECTOR (31 downto 0);					-- Next On Stack
			  TORS : in STD_LOGIC_VECTOR (31 downto 0);	  				-- Top Of Return Stack
			  equalzero : in STD_LOGIC;										-- flag '1' when TOS is zero
			  chip_RAM : in STD_LOGIC;											-- flag used to identify SRAM vs. PSDRAM memory access
			  MicroControl : out STD_LOGIC_VECTOR (13 downto 0);		-- ouput control logic
			  AuxControl : out STD_LOGIC_VECTOR (1 downto 0);			-- output control logic
			  Accumulator : out STD_LOGIC_VECTOR (31 downto 0);		-- literal value captured from memory for writing to TOS
			  ReturnAddress : out STD_LOGIC_VECTOR (31 downto 0);		-- return address on interrupt, BSR or JSR for writing to TORS
			  MEMaddr : out STD_LOGIC_VECTOR (31 downto 0);						  	  
			  MEMdatain_X : in STD_LOGIC_VECTOR (31 downto 0);			-- 32 bit wide SRAM data IN memory bus
			  MEMdataout_X : out STD_LOGIC_VECTOR (31 downto 0);		-- 32 bit wide SRAM data memory bus
			  MEMsize_X	: out STD_LOGIC_VECTOR (1 downto 0);			-- 32 bit wide SRAM data memory bus
--			  MEMsize_Xp: out STD_LOGIC_VECTOR (1 downto 0);			-- 32 bit wide SRAM data memory bus
			  MEM_WRQ_X : out STD_LOGIC;										-- 32 bit wide SRAM data memory bus	
			  MEMdatain_Y : in STD_LOGIC_VECTOR (7 downto 0);			-- 8 bit wide PSDRAM data memory bus
			  MEMdataout_Y : out STD_LOGIC_VECTOR (7 downto 0);		-- 8 bit wide PSRAM data memory bus			  
			  MEM_WRQ_Y : out STD_LOGIC;										-- 8 bit wide PSDRAM data memory bus
			  MEM_REQ_Y : out STD_LOGIC;										-- 8 bit wide PSDRAM data memory bus
			  MEM_RDY_Y : in STD_LOGIC;										-- 8 bit wide PSDRAM data memory bus		  		  
			  MEMdatain_Z : in STD_LOGIC_VECTOR (15 downto 0);			-- 16 bit wide PSDRAM data memory bus
			  MEMdataout_Z : out STD_LOGIC_VECTOR (15 downto 0);		-- 16 bit wide PSDRAM data memory bus
			  MEM_WRQ_Z : out STD_LOGIC;										-- 16 bit wide PSDRAM data memory bus
			  MEM_REQ_Z : out STD_LOGIC;										-- 16 bit wide PSDRAM data memory bus
			  MEM_RDY_Z : in STD_LOGIC											-- 16 bit wide PSDRAM data memory bus
           );
end ControlUnit;

architecture RTL of ControlUnit is

COMPONENT Microcode_ROM															-- storage of microcode in BLOCK RAM
  PORT (
    clka : IN STD_LOGIC;
    addra : IN STD_LOGIC_VECTOR(5 DOWNTO 0);
    douta : OUT STD_LOGIC_VECTOR(13 DOWNTO 0)
  );
END COMPONENT;

-- opcodes (bits 5 downto 0) of the instructions
constant ops_NOP : std_logic_vector(5 downto 0):= (others=>'0');
constant ops_DROP : std_logic_vector(5 downto 0) := CONV_STD_LOGIC_VECTOR(1,6);
constant ops_IFDUP : std_logic_vector(5 downto 0) := CONV_STD_LOGIC_VECTOR(3,6);
constant ops_INC : std_logic_vector(5 downto 0) := CONV_STD_LOGIC_VECTOR(18,6);
constant ops_SMULT : std_logic_vector(5 downto 0) := CONV_STD_LOGIC_VECTOR(22,6);
constant ops_UMULT : std_logic_vector(5 downto 0) := CONV_STD_LOGIC_VECTOR(23,6);
constant ops_SDIVMOD : std_logic_vector(5 downto 0) := CONV_STD_LOGIC_VECTOR(26,6);
constant ops_UDIVMOD : std_logic_vector(5 downto 0) := CONV_STD_LOGIC_VECTOR(27,6);
constant ops_LFETCH : std_logic_vector(5 downto 0) := CONV_STD_LOGIC_VECTOR(47,6);
constant ops_LSTORE : std_logic_vector(5 downto 0) := CONV_STD_LOGIC_VECTOR(48,6);
constant ops_WFETCH : std_logic_vector(5 downto 0) := CONV_STD_LOGIC_VECTOR(49,6);
constant ops_WSTORE : std_logic_vector(5 downto 0) := CONV_STD_LOGIC_VECTOR(50,6);
constant ops_CFETCH : std_logic_vector(5 downto 0) := CONV_STD_LOGIC_VECTOR(51,6);
constant ops_CSTORE : std_logic_vector(5 downto 0) := CONV_STD_LOGIC_VECTOR(52,6);
constant ops_BYTE : std_logic_vector(5 downto 0) := CONV_STD_LOGIC_VECTOR(53,6);
constant ops_WORD : std_logic_vector(5 downto 0) := CONV_STD_LOGIC_VECTOR(54,6);
constant ops_LONG : std_logic_vector(5 downto 0) := CONV_STD_LOGIC_VECTOR(55,6);
constant ops_JMP : std_logic_vector(5 downto 0) := CONV_STD_LOGIC_VECTOR(56,6);
constant ops_JSL : std_logic_vector(5 downto 0) := CONV_STD_LOGIC_VECTOR(57,6);
constant ops_JSR : std_logic_vector(5 downto 0) := CONV_STD_LOGIC_VECTOR(58,6);
constant ops_TRAP : std_logic_vector(5 downto 0) := CONV_STD_LOGIC_VECTOR(59,6);
constant ops_RETRAP : std_logic_vector(5 downto 0) := CONV_STD_LOGIC_VECTOR(60,6);
constant ops_RTI : std_logic_vector(5 downto 0) := CONV_STD_LOGIC_VECTOR(61,6);
constant ops_TEST : std_logic_vector(5 downto 0) := CONV_STD_LOGIC_VECTOR(63,6);

-- internal opcodes used for microcode
constant ops_PUSH : std_logic_vector(5 downto 0) :=  CONV_STD_LOGIC_VECTOR(59,6);
constant ops_REPLACE  : std_logic_vector(5 downto 0) := CONV_STD_LOGIC_VECTOR(60,6);
constant ops_JSI : std_logic_vector(5 downto 0) := CONV_STD_LOGIC_VECTOR(63,6);

-- branch codes (bits 7 downto 6) of the instructions
constant bps_RTS : std_logic_vector(1 downto 0) := "01";
constant bps_BEQ : std_logic_vector(1 downto 0) := "10";
constant bps_BRA : std_logic_vector(1 downto 0) := "11";

-- interrupt vector
constant int_vector_TRAP  : std_logic_vector (19 downto 0) := CONV_STD_LOGIC_VECTOR(2,20);
constant int_vector_IRQ0  : std_logic_vector (7 downto 0) := CONV_STD_LOGIC_VECTOR(2,8);

type state_T is (common, ifdup, smult, umult, sdivmod, udivmod, sdivmod_load, udivmod_load,
						Sstore_long, Sstore_word, Sstore_byte, SRAM_store, 
						Sfetch_long, Sfetch_word, Sfetch_byte,
						Dfetch_long, Dfetch_long2, Dfetch_long3, Dfetch_word, Dfetch_byte,
						Dstore_long, Dstore_long2, Dstore_long3, Dstore_word, Dstore_byte, Dstore2,
						load_byte, load_word, load_long, load_long2,
						jsl_state,
						skip1, skip2);
--type offset_T is (none, one, two, four);						
						
signal state, state_n  : state_T;										-- state machine
signal PC, PC_n, PC_plus, PC_plus_three, PC_jsl, PC_branch, PC_m1, PC_skipbranch : std_logic_vector (19 downto 0);		-- program counter logic
signal delta :std_logic_vector (19 downto 0);
signal plus : std_logic_vector (2 downto 0);
signal accumulator_i, accumulator_n : std_logic_vector (31 downto 0); -- shift register for compiling LONGS and WORDS with BYTE reads
signal accumulator_Y, accumulator_Z : std_logic_vector (31 downto 0); 
signal ReturnAddress_n, ReturnAddressJSL, PC_addr : std_logic_vector (31 downto 0);
signal int_vector_ext : std_logic_vector (19 downto 0);
signal int_vector_ext_i : std_logic_vector (7 downto 0);
signal ucode : std_logic_vector (5 downto 0);					-- address driver for microcode BLOCK RAM
--signal equalzero : std_logic;											-- flag '1' when TOS is zero
--signal chip_RAM : std_logic;
signal timer : integer range 0 to 63;								-- timer/counter for state machine
signal count : integer range 0 to 63;								-- Pedroni, "Circuit Design and Simulation with VHDL" p298
signal int_trig : std_logic;
signal irq_m1, irq_n : std_logic;
signal irv_i : std_logic_vector (7 downto 0);
signal retrap, retrap_n : std_logic_vector(1 downto 0);
signal AuxControl_i, AuxControl_n : std_logic_vector(1 downto 0);
signal opcode, next_opcode : std_logic_vector(5 downto 0);					-- opcode of current and next instructions
signal branch, next_branch : std_logic_vector(1 downto 0);					-- branch codes of current and next instructions
--signal offset : std_logic_vector(1 downto 0);	
signal MEMsize_X_n : STD_LOGIC_VECTOR (1 downto 0);	
signal delayed_RTS, delayed_RTS_n : STD_LOGIC;
signal MEMaddr_i : STD_LOGIC_VECTOR (31 downto 0);	
signal MEM_WRQ_X_i, MEM_WRQ_Y_i, MEM_WRQ_Z_i  : STD_LOGIC;
signal opcode_m1 : std_logic_vector (5 downto 0);										-- opcode of prior clock cycle

alias signbit is MEMdatain_X(29);

begin

	inst_Microcode_ROM : Microcode_ROM										-- microcode BLOCK RAM
	PORT MAP (
	 clka => clk,
	 addra => ucode,
	 douta => MicroControl
	);

	opcode <= MEMdatain_X(29 downto 24); 
	branch <= MEMdatain_X(31 downto 30); 
  
--	with offset select
--		next_opcode <= MEMdatain_X(29 downto 24) when "00",		-- re-prime pipleline
--							MEMdatain_X(21 downto 16) when "01",		-- single byte instruction
--							MEMdatain_X(13 downto 8) when "10",		-- two byte instruction (#.b)
--							MEMdatain_X(5 downto 0) when others;		-- three byte instruction (#.w)
--	
--	with offset select
--		next_branch <= MEMdatain_X(31 downto 30) when "00",		
--							MEMdatain_X(23 downto 22) when "01",
--							MEMdatain_X(15 downto 14) when "10",
--							MEMdatain_X(7 downto 6) when others;
 
	Accumulator <= Accumulator_i;
	AuxControl <= AuxControl_i;
	
--	equalzero <= '1' when TOS = 0 else '0'; 						-- flag used for ?DUP, BEQ, and TEST instructions
	
--	chip_RAM <= '1' when TOS(23 downto 16) = 0 else '0';		-- flag used to identify SRAM vs. PSDRAM memory access
						
	--chip_RAM <= '1' when (TOS(23) or TOS(22) or TOS(21) or TOS(20) or TOS(19) or TOS(18) or TOS(17) or TOS(16)) = '0' else '0';
	-- TOS(31) or TOS(30) or TOS(29) or TOS(28) or TOS(27) or TOS(26) or TOS(25) or TOS(24) or    - this memory not accessed
	-- chip_RAM <= '1' when (TOS < 65536) else '0';
						  
	accumulator_Y <= accumulator_i(23 downto 0) & MEMdatain_Y;	-- compile WORDs and LONGs from sequential reads
	accumulator_Z <= accumulator_i(15 downto 0) & MEMdatain_Z;	
	
	int_trig <= irq or irq_m1;
	irv_i <= "000" & irv & "0";										-- double the interrupt vector number
	int_vector_ext_i <= int_vector_IRQ0 + irv_i;					-- add to the IRQ0 base
	int_vector_ext <= "000000000000" & int_vector_ext_i;		-- extend to width of address bus

	--PC_plus <= PC + plus;												-- states set plus to "000", "001", etc. to increment PC as appropriate
	PC_plus <= PC + "001";
	PC_jsl <= MEMdatain_X(19 downto 0);
	delta  <= signbit & signbit & signbit & signbit & signbit & signbit & signbit & MEMdatain_X(28 downto 16);
	PC_branch <= PC + delta;											-- sign extended 14 bit branch for BRA or BEQ	
--	PC_skipbranch <= PC + "00000000000000000010";				-- PC + 2 for skipping a BEQ branch
	PC_addr <= "000000000000" & PC;
	PC_plus_three <= PC + "011";
	ReturnAddressJSL <= "000000000000" & PC_plus_three;
	
	MEMaddr <= MEMaddr_i;
	MEM_WRQ_X <= MEM_WRQ_X_i;
	MEM_WRQ_Y <= MEM_WRQ_Y_i;
	MEM_WRQ_Z <= MEM_WRQ_Z_i;
	
	-- combinatorial process to determine the size of the next opcode for incrementing the PC
--	process (next_opcode, next_branch)
--	begin
--		if 
--		next_branch = "00" then
--			if next_opcode = ops_LONG then
--				plus <= "101";											-- advance by 5 bytes for a long literal
--			elsif next_opcode = ops_WORD then
--				plus <= "011";											-- 3 bytes for a word literal
--			elsif next_opcode = ops_BYTE then
--				plus <= "010";											-- 2 bytes for a byte literal
--			elsif next_opcode = ops_JSL then
--				plus <= "100";											-- 4 bytes for a JSL instruction (i.e. for the return address)
--			else															
--				plus <= "001";			
--			end if;
--		else								-- branches do not utilize increment so leave PC unchanged as the base
--			plus <= "000";
--		end if;
--	end process;
	
	-- main control unit state machine
	
	
	process																			
	begin																		-- faster to separate next-state logic and eliminate shared resources
		wait until rising_edge(clk);
		if rst = '0' then																	
			if (count >= timer) then 
				state <= state_n;			
			end if;	
		else																	-- synchronous reset
			state <= skip1;	-- skip2
		end if;
	end process;
	
	process																			
	begin																		-- sequential (registered) section of state machine
		wait until rising_edge(clk);
		if rst = '0' then													
			count <= count + 1;
			irq_m1 <= irq_n;
			ReturnAddress <= ReturnAddress_n;	
			accumulator_i <= accumulator_n;			
			retrap <= retrap_n;
			MEMsize_X <= MEMsize_X_n;
			delayed_RTS <= delayed_RTS_n;
			opcode_m1 <= (others=>'0');
									
			if (count >= timer) then 
				count <= 0;	
				AuxControl_i <= AuxControl_n;	
				PC <= PC_n;													-- PC is updated only on the final cycle of multi-cycle opcode states
				PC_m1 <= PC;												-- PC_m1 is PC of prior cycle, needed for branch and returns due to 1 stage pipeline	
				opcode_m1 <= opcode;
			end if;	
		else																	-- synchronous reset
			count <= 0;
			PC <= (others=>'0');
			PC_m1 <= (others=>'0');
			irq_m1 <= '0';
			retrap <= (others=>'0');
			ReturnAddress <= (others=>'0');
			accumulator_i <= (others=>'0');
			AuxControl_i <= (others=>'0');
			MEMsize_X <= (others=>'0');
			delayed_RTS <= '0';
		end if;
	end process;

	process (state, state_n, PC, PC_n, PC_plus, PC_jsl, PC_branch, PC_skipbranch, PC_m1, PC_addr, delta, plus, 
				accumulator_i, accumulator_n, accumulator_Y, accumulator_Z,
				ucode, equalzero,branch, opcode, chip_RAM, MEMdatain_X, retrap,
				TOS, TOS_r, NOS, TORS, int_trig, MEM_RDY_Y, MEM_RDY_Z, int_vector_ext, int_vector_ext_i, branch, opcode, delayed_RTS)
	begin																					-- combinational section of state machine
		case state is

		when common =>																	-- common state executes most instructions in 1 clock cycle
		
		-- Next state logic		
			if int_trig = '1' or retrap(0) = '1' then							-- interrupts and timed trap take first priority
				state_n <= skip1;		-- skip2	
			elsif branch = bps_BRA then											-- check branches next as they use the opcode bits for offsets
				state_n <= skip1;		-- skip1				
			elsif branch = bps_BEQ then				
				state_n <= skip1; 	-- skip2
			elsif opcode = ops_JSL then
				state_n <= JSL_state;												-- return address is three bytes ahead of PC, need separate state to adjust
			elsif opcode = ops_JSR or opcode = ops_JMP or opcode = ops_TRAP or opcode = ops_RETRAP then
				state_n <= skip1;		-- skip2
			elsif opcode = ops_lfetch then
				if chip_RAM = '1' then
					state_n <= Sfetch_long;	
				else
					state_n <= Dfetch_long;
				end if;
			elsif opcode = ops_wfetch then
				if chip_RAM = '1' then
					state_n <= Sfetch_word;	
				else
					state_n <= Dfetch_word;
				end if;				
			elsif opcode = ops_cfetch then
				if chip_RAM = '1' then
					state_n <= Sfetch_byte;	
				else
					state_n <= Dfetch_byte;
				end if;	
--			elsif chip_RAM = '1' and (opcode = ops_CFETCH or opcode = ops_WFETCH or opcode = ops_LFETCH) then
--				state_n <= skip2;
--			elsif chip_RAM = '0' and opcode = ops_lfetch then
--				state_n <= Dfetch_long;	
--			elsif chip_RAM = '0' and opcode = ops_wfetch then
--				state_n <= Dfetch_word;
--			elsif chip_RAM = '0' and opcode = ops_cfetch then
--				state_n <= Dfetch_byte;	
			elsif opcode = ops_LSTORE then
				if chip_RAM = '1' then
					state_n <= Sstore_long;
				else
					state_n <= Dstore_long;
				end if;
			elsif opcode = ops_WSTORE then
				if chip_RAM = '1' then
					state_n <= Sstore_word;
				else
					state_n <= Dstore_word;
				end if;
			elsif opcode = ops_CSTORE then
				if chip_RAM = '1' then
					state_n <= Sstore_byte;
				else
					state_n <= Dstore_byte;
				end if;				
--			elsif chip_RAM = '1' and (opcode = ops_CSTORE or  opcode = ops_WSTORE or opcode = ops_LSTORE) then
--				state_n <= SRAM_store0;					
--			elsif chip_RAM = '0' and opcode = ops_lstore then
--				state_n <= Dstore_long;
--			elsif chip_RAM = '0' and opcode = ops_wstore then
--				state_n <= Dstore_word;					
--			elsif chip_RAM = '0' and opcode = ops_cstore then
--				state_n <= Dstore_byte;
			elsif opcode = ops_byte then
				state_n <= load_byte;
			elsif opcode = ops_word then
				state_n <= load_word;
			elsif opcode = ops_long then
				state_n <= load_long;				
--			elsif opcode = ops_long and branch /= bps_RTS then
--				state_n <= skip1;														-- extra cycle required since fetch is not wide enough to see beyond long literal
--			elsif opcode = ops_long and branch = bps_RTS then
--				state_n <= skip2;
			elsif opcode = ops_ifdup and equalzero = '1' then
				state_n <= ifdup;
			elsif opcode = ops_ifdup and equalzero = '0' then 	
				state_n <= skip1;
--			elsif opcode = ops_ifdup and equalzero = '0' and branch /= bps_RTS then 	
--				state_n <= skip1;
--			elsif opcode = ops_ifdup and equalzero = '0' and branch = bps_RTS then
--				state_n <= skip1;					-- skip2
			elsif opcode = ops_SDIVMOD then
				state_n <= sdivmod;		
			elsif opcode = ops_UDIVMOD then
				state_n <= udivmod;
			elsif opcode = ops_SMULT then
				state_n <= smult;
			elsif opcode = ops_UMULT then
				state_n <= umult;
			elsif branch = bps_RTS then											-- other RTS instructions									
				state_n <= skip1;					-- skip2	
			else
				state_n <= common;
			end if;
			timer <= 0;
			
			-- Memory write logic
			MEMdataout_X <= NOS;											-- 32bit SRAM						
			MEMdataout_Y <= NOS(7 downto 0);							-- 8bit PSDRAM access
--			if opcode = ops_LSTORE then								-- 16bit PSDRAM access
				MEMdataout_Z <= NOS(31 downto 16);
--			else
--				MEMdataout_Z <= NOS(15 downto 0);
--			end if;
--			
--			if int_trig = '0' and opcode = ops_CSTORE then 		-- write request trigger
--				if chip_RAM = '1' then
--					MEM_WRQ_X_i <= '1';
--					MEM_WRQ_Y_i <= '0';
--					MEM_WRQ_Z_i <= '0';	
--				else
--					MEM_WRQ_X_i <= '0';
--					MEM_WRQ_Y_i <= '1';
--					MEM_WRQ_Z_i <= '0';						
--				end if;		
--			elsif int_trig = '0' and opcode = ops_WSTORE then
--				if chip_RAM = '1' then
--					MEM_WRQ_X_i <= '1';
--					MEM_WRQ_Y_i <= '0';
--					MEM_WRQ_Z_i <= '0';	
--				else
--					MEM_WRQ_X_i <= '0';
--					MEM_WRQ_Y_i <= '0';
--					MEM_WRQ_Z_i <= '1';						
--				end if;				
--			elsif int_trig = '0' and opcode = ops_LSTORE then	
--				if chip_RAM = '1' then
--					MEM_WRQ_X_i <= '1';
--					MEM_WRQ_Y_i <= '0';
--					MEM_WRQ_Z_i <= '0';	
--				else
--					MEM_WRQ_X_i <= '0';
--					MEM_WRQ_Y_i <= '0';
--					MEM_WRQ_Z_i <= '1';						
--				end if;	
--			else
				MEMdataout_Z <= NOS(15 downto 0);
				MEM_WRQ_X_i <= '0';
				MEM_WRQ_Y_i <= '0';
				MEM_WRQ_Z_i <= '0';
--			end if;
			
			-- Memory read logic
--			if int_trig = '0' and (opcode = ops_CFETCH or opcode = ops_CSTORE) then
--				MEMsize_X_n <= "01";														-- MEMsize_X is delayed one cycle to coincide with the one cycle delay in memory read or write
--			elsif int_trig = '0' and (opcode = ops_WFETCH or opcode = ops_WSTORE)  then
--				MEMsize_X_n <= "10";
--			else
--				MEMsize_X_n <= "11";
--			end if;
			
			if opcode = ops_BYTE then						
				MEMsize_X_n <= "01";														
			elsif opcode = ops_WORD then
				MEMsize_X_n <= "10";
			else
				MEMsize_X_n <= "11";
			end if;
			
			--MEMsize_Xp <= "11";
			
--			if (int_trig = '0') and (chip_RAM = '0') and (opcode = ops_CFETCH) then
--				MEM_REQ_Y <= '1';
--			else
			MEM_REQ_Y <= '0';
--			end if;
			
--			if (int_trig = '0') and (chip_RAM = '0') and (opcode = ops_WFETCH or opcode = ops_LFETCH) then
--				MEM_REQ_Z <= '1';
--			else
			MEM_REQ_Z <= '0';
--			end if;
			
			-- Memory address logic
--			if int_trig = '0' and (opcode = ops_CFETCH or opcode = ops_WFETCH or opcode = ops_LFETCH or
--				opcode = ops_CSTORE or opcode = ops_WSTORE or opcode = ops_LSTORE) then  
--				MEMaddr_i <= TOS;											
--			else																
			MEMaddr_i <= PC_addr;											
--			end if;	
					
			-- Program counter logic
			if int_trig = '1' then
				PC_n <= int_vector_ext;												-- PC from external interrupt vector
			elsif retrap(0) = '1' then
				PC_n <= int_vector_TRAP;											-- PC from internal interrupt vector				
			elsif branch = bps_BRA then
				PC_n <= PC_branch;				
			elsif branch = bps_BEQ and equalzero = '1' then
--				if equalzero = '1' then
					PC_n <= PC_branch;	
--				else
--					PC_n <= PC_skipbranch;
--				end if;
			elsif branch = bps_RTS then
				PC_n <= TORS(19 downto 0);											-- PC from Top Of Return Stack (also covers RTI and RETRAP, which include RTS by default)
			elsif opcode = ops_TRAP then
				PC_n <= int_vector_TRAP;											-- PC from internal interrupt vector					
			elsif opcode = ops_JSL then
				PC_n <= PC_jsl;									
			elsif opcode = ops_JSR or opcode = ops_JMP then
				PC_n <= TOS(19 downto 0);											-- PC from TOS	
			elsif opcode = ops_CFETCH or opcode = ops_WFETCH or opcode = ops_LFETCH or
				opcode = ops_SDIVMOD or opcode = ops_UDIVMOD or opcode = ops_IFDUP or
				opcode = ops_CSTORE or opcode = ops_WSTORE or opcode = ops_LSTORE or
				opcode = ops_SMULT or opcode = ops_UMULT then  										
				PC_n <= PC;																-- PC update is done on the final cycle of multi-cycle instructions
			else
				PC_n <= PC_plus;												
			end if;		
			
			-- Program counter next instruction offset logic
--			if opcode = ops_WORD then
--				offset <= "11";											
--			elsif opcode = ops_BYTE then
--				offset <= "10";											
--			else															-- all other multi-byte instructions are either multicycle (#.L) or change the PC directly (BRA, BEQ, JSL)	
--				offset <= "01";										-- next instruction found one byte after a single byte instructions 				
--			end if;	
			
			-- Microcode logic
			if int_trig = '1' or opcode = ops_TRAP or retrap(0) = '1' then
				ucode <= ops_JSI;										-- interrupt microcode 
			elsif branch = bps_BRA then						
				ucode <= ops_NOP;										-- avoid executing the high 6 bits of the branch offset as an opcode!
			elsif branch = bps_BEQ then
				ucode <= ops_DROP;									-- drop the flag
			elsif opcode = ops_RTI or opcode = ops_TRAP or opcode = ops_RETRAP or retrap(0) = '1' then
				ucode <= ops_NOP;										-- these instructions have no microcode but overlap with internal microcode
			elsif opcode = ops_SDIVMOD or opcode = ops_UDIVMOD then
				ucode <= ops_NOP;										-- suppress microcode until last cycle of these instructions
			elsif (opcode = ops_CSTORE or opcode = ops_CFETCH or opcode = ops_WSTORE or   -- chip_RAM = '0' and 
									   opcode = ops_WFETCH or opcode = ops_LSTORE or opcode = ops_LFETCH) then
				ucode <= ops_NOP;										-- need to surpress the microcode until the data is ready				
			else
				ucode <= opcode;
			end if;

			-- Accumulator logic
			accumulator_n <= (others=>'0');
			
			-- Data multiplexer logic
--			if opcode = ops_CFETCH or opcode = ops_WFETCH or opcode = ops_LFETCH then  
--				AuxControl_n(2 downto 1) <= "01";					-- setting for SRAM fetch											
--			else																
				AuxControl_n(1 downto 1) <= "0";					-- setting for SRAM fetch or load literal instructions										
--			end if;

			-- Return stack control logic 
--			if (opcode = ops_RETRAP) and (int_trig = '0') then	-- override on interrupt
--				AuxControl_n(0 downto 0) <= "1";						-- decrement return stack pointer
--			elsif (branch = bps_RTS) and  (int_trig = '0') then
--				AuxControl_n(0 downto 0) <= "0";
--			else
			AuxControl_n(0 downto 0) <= "0";
			
			--end if;
			if int_trig = '1' or retrap(0) = '1' then											
				ReturnAddress_n <= "000000000000" & PC_m1;
			elsif opcode = ops_JSL then
				ReturnAddress_n <= "000000000000" & PC_plus_three;
			else
				ReturnAddress_n <= PC_addr;
			end if;
			
			-- Interrupt logic
			irq_n <= '0';
			if opcode = ops_RTI then
				rti <= '1';
			else
				rti <= '0';
			end if;
			
			-- Trap logic
			if opcode = ops_RETRAP then
				retrap_n <= "10";
			elsif opcode = ops_TRAP then
				retrap_n <= "00";
			else
				retrap_n <= "0" & retrap(1);
			end if;
			
			-- Delayed RTS logic
			if branch = bps_RTS and not (int_trig = '1' or retrap(0) = '1') then
				delayed_RTS_n <= '1';
			else
				delayed_RTS_n <= '0';
			end if;
	
		when ifdup =>											-- ifdup when TOS was zero
			if delayed_RTS = '0' then
				state_n <= common;
			else
				state_n <= skip1;
			end if;
			timer <= 0;
			PC_n <= PC_plus;
--			offset <= "00";	
			ucode <= ops_DROP;									-- DROP previously DUP'd value
			accumulator_n <= (others=>'0');
			MEMaddr_i <= PC_addr;	
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X_i <= '0';
			MEM_WRQ_Y_i <= '0';	
			MEM_WRQ_Z_i <= '0';
			MEMdataout_X <= NOS;
			MEMdataout_Y <= NOS(7 downto 0);
			MEMdataout_Z <= NOS(15 downto 0);
			MEMsize_X_n <= "11";
			--MEMsize_Xp <= "11";
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "0";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			delayed_RTS_n <= delayed_RTS;
			
		when smult =>										-- signed multiply
			state_n <= common;
			timer <= 4;  											-- wait for multiplier
			PC_n <= PC_plus;										-- PC update will take place only on transition to next state
--			offset <= "00";	
			ucode <= ops_SMULT;							
			accumulator_n <= (others=>'0');	
			MEMaddr_i <= PC_addr;	
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X_i <= '0';
			MEM_WRQ_Y_i <= '0';	
			MEM_WRQ_Z_i <= '0';
			MEMdataout_X <= NOS;
			MEMdataout_Y <= NOS(7 downto 0);
			MEMdataout_Z <= NOS(15 downto 0);
			MEMsize_X_n <= "11";
			--MEMsize_Xp <= "11";
			if delayed_RTS= '1' then
				AuxControl_n(0 downto 0) <= "1";					-- decrement return stack pointer
			else
				AuxControl_n(0 downto 0) <= "0";
			end if;
			AuxControl_n(1 downto 1) <= "0";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			delayed_RTS_n <= delayed_RTS;
	
		when umult =>										-- unsigned multiply
			state_n <= common;
			timer <= 4;  											-- wait for multiplier
			PC_n <= PC_plus;										-- PC update will take place only on transition to next state
--			offset <= "00";
			ucode <= ops_UMULT;									
			accumulator_n <= (others=>'0');	
			MEMaddr_i <= PC_addr;	
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X_i <= '0';
			MEM_WRQ_Y_i <= '0';	
			MEM_WRQ_Z_i <= '0';
			MEMdataout_X <= NOS;
			MEMdataout_Y <= NOS(7 downto 0);
			MEMdataout_Z <= NOS(15 downto 0);
			MEMsize_X_n <= "11";
			--MEMsize_Xp <= "11";
			if delayed_RTS= '1' then
				AuxControl_n(0 downto 0) <= "1";					-- decrement return stack pointer
			else
				AuxControl_n(0 downto 0) <= "0";
			end if;
			AuxControl_n(1 downto 1) <= "0";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			delayed_RTS_n <= delayed_RTS;
	
		when sdivmod =>										-- signed division
			state_n <= sdivmod_load;
			timer <= 42;  											-- wait for divider
			PC_n <= PC;
--			offset <= "00";	
			ucode <= ops_NOP;
			accumulator_n <= (others=>'0');	
			MEMaddr_i <= PC_addr;	
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X_i <= '0';
			MEM_WRQ_Y_i <= '0';	
			MEM_WRQ_Z_i <= '0';
			MEMdataout_X <= NOS;
			MEMdataout_Y <= NOS(7 downto 0);
			MEMdataout_Z <= NOS(15 downto 0);
			MEMsize_X_n <= "11";
			--MEMsize_Xp <= "11";
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "0";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			delayed_RTS_n <= delayed_RTS;

		when udivmod =>										-- unsigned division
			state_n <= udivmod_load;
			timer <= 41;											-- wait for divider
			PC_n <= PC;
--			offset <= "00";
			ucode <= ops_NOP;
			accumulator_n <= (others=>'0');	
			MEMaddr_i <= PC_addr;	
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X_i <= '0';
			MEM_WRQ_Y_i <= '0';	
			MEM_WRQ_Z_i <= '0';
			MEMdataout_X <= NOS;
			MEMdataout_Y <= NOS(7 downto 0);
			MEMdataout_Z <= NOS(15 downto 0);
			MEMsize_X_n <= "11";
			--MEMsize_Xp <= "11";
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "0";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			delayed_RTS_n <= delayed_RTS;
			
		when sdivmod_load =>									-- signed division
			state_n <= common;									
			timer <= 0;  
			PC_n <= PC_plus;
--			offset <= "00";
			ucode <= opcode; --ops_SDIVMOD;								-- load TOS and NOS with results
			accumulator_n <= (others=>'0');
			MEMaddr_i <= PC_addr;	
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X_i <= '0';
			MEM_WRQ_Y_i <= '0';	
			MEM_WRQ_Z_i <= '0';
			MEMdataout_X <= NOS;
			MEMdataout_Y <= NOS(7 downto 0);
			MEMdataout_Z <= NOS(15 downto 0);
			MEMsize_X_n <= "11";
			--MEMsize_Xp <= "11";
			if delayed_RTS= '1' then
				AuxControl_n(0 downto 0) <= "1";					-- decrement return stack pointer
			else
				AuxControl_n(0 downto 0) <= "0";
			end if;
			AuxControl_n(1 downto 1) <= "0";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			delayed_RTS_n <= delayed_RTS;
	
		when udivmod_load =>									-- unsigned division
			state_n <= common;
			timer <= 0;
			PC_n <= PC_plus;
--			offset <= "00";
			ucode <= opcode;								-- load TOS and NOS with results
			accumulator_n <= (others=>'0');
			MEMaddr_i <= PC_addr;	
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X_i <= '0';
			MEM_WRQ_Y_i <= '0';	
			MEM_WRQ_Z_i <= '0';
			MEMdataout_X <= NOS;
			MEMdataout_Y <= NOS(7 downto 0);
			MEMdataout_Z <= NOS(15 downto 0);
			MEMsize_X_n <= "11";
			--MEMsize_Xp <= "11";
			if delayed_RTS= '1' then
				AuxControl_n(0 downto 0) <= "1";					-- decrement return stack pointer
			else
				AuxControl_n(0 downto 0) <= "0";
			end if;
			AuxControl_n(1 downto 1) <= "0";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			delayed_RTS_n <= delayed_RTS;
			
		when Sfetch_byte =>											
			state_n <= skip1;																	
			ucode <= opcode_m1;											
			timer <= 0;
			PC_n <= PC;
--			offset <= "00";
			MEMaddr_i <= TOS_r;										-- address now available in registered TOS					
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X_i <= '0';
			MEM_WRQ_Y_i <= '0';	
			MEM_WRQ_Z_i <= '0';
			MEMdataout_X <= NOS;
			MEMdataout_Y <= NOS(7 downto 0);
			MEMdataout_Z <= NOS(15 downto 0);
			MEMsize_X_n <= "01";
			--MEMsize_Xp <= "11";										-- byte
			accumulator_n <= (others=>'0');			
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "0";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			delayed_RTS_n <= delayed_RTS;
			
		when Sfetch_word =>											
			state_n <= skip1;																	
			ucode <= opcode_m1;											
			timer <= 0;
			PC_n <= PC;
--			offset <= "00";
			MEMaddr_i <= TOS_r;										-- address now available in registered TOS					
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X_i <= '0';
			MEM_WRQ_Y_i <= '0';	
			MEM_WRQ_Z_i <= '0';
			MEMdataout_X <= NOS;
			MEMdataout_Y <= NOS(7 downto 0);
			MEMdataout_Z <= NOS(15 downto 0);
			MEMsize_X_n <= "10";
			--MEMsize_Xp <= "11";										-- word
			accumulator_n <= (others=>'0');			
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "0";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			delayed_RTS_n <= delayed_RTS;

		when Sfetch_long =>											
			state_n <= skip1;																	
			ucode <= opcode_m1;									
			timer <= 0;
			PC_n <= PC;
--			offset <= "00";
			MEMaddr_i <= TOS_r;										-- address now available in registered TOS					
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X_i <= '0';
			MEM_WRQ_Y_i <= '0';	
			MEM_WRQ_Z_i <= '0';
			MEMdataout_X <= NOS;
			MEMdataout_Y <= NOS(7 downto 0);
			MEMdataout_Z <= NOS(15 downto 0);
			MEMsize_X_n <= "11";
			--MEMsize_Xp <= "11";										-- long
			accumulator_n <= (others=>'0');			
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "0";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			delayed_RTS_n <= delayed_RTS;
			
		when Sstore_long =>											
			state_n <= SRAM_store;																	
			ucode <= ops_drop;											
			timer <= 0;
			PC_n <= PC;
--			offset <= "00";
			MEMaddr_i <= TOS_r;										-- address now available in registered TOS					
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X_i <= '1';
			MEM_WRQ_Y_i <= '0';	
			MEM_WRQ_Z_i <= '0';
			MEMdataout_X <= NOS;
			MEMdataout_Y <= NOS(7 downto 0);
			MEMdataout_Z <= NOS(15 downto 0);
			MEMsize_X_n <= "11";										-- long
			--MEMsize_Xp <= "11";								
			accumulator_n <= (others=>'0');			
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "0";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			delayed_RTS_n <= delayed_RTS;
			
		when Sstore_word =>											
			state_n <= SRAM_store;																	
			ucode <= ops_drop;											
			timer <= 0;
			PC_n <= PC;
--			offset <= "00";
			MEMaddr_i <= TOS_r;										-- address now available in registered TOS					
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X_i <= '1';
			MEM_WRQ_Y_i <= '0';	
			MEM_WRQ_Z_i <= '0';
			MEMdataout_X <= NOS;
			MEMdataout_Y <= NOS(7 downto 0);
			MEMdataout_Z <= NOS(15 downto 0);
			MEMsize_X_n <= "10";										-- word
			--MEMsize_Xp <= "11";								
			accumulator_n <= (others=>'0');			
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "0";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			delayed_RTS_n <= delayed_RTS;
			
		when Sstore_byte =>											
			state_n <= SRAM_store;																	
			ucode <= ops_drop;											
			timer <= 0;
			PC_n <= PC;
--			offset <= "00";
			MEMaddr_i <= TOS_r;										-- address now available in registered TOS					
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X_i <= '1';
			MEM_WRQ_Y_i <= '0';	
			MEM_WRQ_Z_i <= '0';
			MEMdataout_X <= NOS;
			MEMdataout_Y <= NOS(7 downto 0);
			MEMdataout_Z <= NOS(15 downto 0);
			MEMsize_X_n <= "01";										-- byte
			--MEMsize_Xp <= "11";								
			accumulator_n <= (others=>'0');			
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "0";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			delayed_RTS_n <= delayed_RTS;
			
		when SRAM_store =>											
			state_n <= skip1;																	
			ucode <= ops_drop;											
			timer <= 0;
			PC_n <= PC;
--			offset <= "00";
			MEMaddr_i <= TOS_r;										-- address still available in registered TOS					
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X_i <= '0';
			MEM_WRQ_Y_i <= '0';	
			MEM_WRQ_Z_i <= '0';
			MEMdataout_X <= NOS;
			MEMdataout_Y <= NOS(7 downto 0);
			MEMdataout_Z <= NOS(15 downto 0);
			MEMsize_X_n <= "11";
			--MEMsize_Xp <= "11";
			accumulator_n <= (others=>'0');			
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "0";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			delayed_RTS_n <= delayed_RTS;
	
		when Dfetch_long =>											
			if MEM_RDY_Z = '1' then
				state_n <= Dfetch_long2;									
				accumulator_n <= accumulator_Z;
				ucode <= ops_INC;
			else
				state_n <= Dfetch_long;
				accumulator_n <= accumulator_i;		
				ucode <= ops_NOP;	
			end if;
			timer <= 0;
			PC_n <= PC;					
--			offset <= "00";
			MEMaddr_i <= TOS_r;			-- MODIFIED! - was TOS (unregistred)	
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '1';
			MEM_WRQ_X_i <= '0';
			MEM_WRQ_Y_i <= '0';	
			MEM_WRQ_Z_i <= '0';
			MEMdataout_X <= NOS;
			MEMdataout_Y <= NOS(7 downto 0);
			MEMdataout_Z <= NOS(15 downto 0);		
			MEMsize_X_n <= "11";
			--MEMsize_Xp <= "11";
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "1";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			delayed_RTS_n <= delayed_RTS;

		when Dfetch_long2 =>											
			state_n <= Dfetch_long3;									
			accumulator_n <= accumulator_i;		
			ucode <= ops_INC;
			timer <= 0;
			PC_n <= PC;				
--			offset <= "00";
			MEMaddr_i <= TOS_r;				
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X_i <= '0';
			MEM_WRQ_Y_i <= '0';	
			MEM_WRQ_Z_i <= '0';
			MEMdataout_X <= NOS;
			MEMdataout_Y <= NOS(7 downto 0);
			MEMdataout_Z <= NOS(15 downto 0);
			MEMsize_X_n <= "11";
			--MEMsize_Xp <= "11";
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "1";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			delayed_RTS_n <= delayed_RTS;
	
		when Dfetch_long3 =>										-- wait for TOS_r to be updated		
			state_n <= Dfetch_word;									
			accumulator_n <= accumulator_i;		
			ucode <= ops_NOP;
			timer <= 0;
			PC_n <= PC;				
--			offset <= "00";
			MEMaddr_i <= TOS_r;				
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X_i <= '0';
			MEM_WRQ_Y_i <= '0';	
			MEM_WRQ_Z_i <= '0';
			MEMdataout_X <= NOS;
			MEMdataout_Y <= NOS(7 downto 0);
			MEMdataout_Z <= NOS(15 downto 0);
			MEMsize_X_n <= "11";
			--MEMsize_Xp <= "11";
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "1";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			delayed_RTS_n <= delayed_RTS;
	
		when Dfetch_word =>											
			if MEM_RDY_Z = '1' then
				state_n <= skip1;								-- skip2						
				accumulator_n <= accumulator_Z;
				ucode <= ops_REPLACE;	
			else
				state_n <= Dfetch_word;
				accumulator_n <= accumulator_i;	
				ucode <= ops_NOP;	
			end if;
			timer <= 0;	
			PC_n <= PC;
--			offset <= "00";
			MEMaddr_i <= TOS_r;			
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '1';			
			MEM_WRQ_X_i <= '0';
			MEM_WRQ_Y_i <= '0';	
			MEM_WRQ_Z_i <= '0';
			MEMdataout_X <= NOS;
			MEMdataout_Y <= NOS(7 downto 0);
			MEMdataout_Z <= NOS(15 downto 0);
			MEMsize_X_n <= "11";
			--MEMsize_Xp <= "11";
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "1";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			delayed_RTS_n <= delayed_RTS;
				
		when Dfetch_byte =>											
			if MEM_RDY_Y = '1' then
				state_n <= skip1;							-- skip2								
				accumulator_n <= accumulator_Y;
				ucode <= ops_REPLACE;
			else
				state_n <= Dfetch_byte;
				accumulator_n <= accumulator_i;	
				ucode <= ops_NOP;		
			end if;
			timer <= 0;
			PC_n <= PC;					
--			offset <= "00";
			MEMaddr_i <= TOS_r;
			MEM_REQ_Y <= '1';				
			MEM_REQ_Z <= '0';
			MEM_WRQ_X_i <= '0';
			MEM_WRQ_Y_i <= '0';	
			MEM_WRQ_Z_i <= '0';
			MEMdataout_X <= NOS;
			MEMdataout_Y <= NOS(7 downto 0);
			MEMdataout_Z <= NOS(15 downto 0);	
			MEMsize_X_n <= "11";
			--MEMsize_Xp <= "11";			
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "1";
			ReturnAddress_n <= PC_addr;	
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			delayed_RTS_n <= delayed_RTS;
				
		when Dstore_long =>										-- store BYTE in memory or final step of LONG or WORD store
			if MEM_RDY_Z = '1' then
				state_n <= Dstore_long2;
				ucode <= ops_INC;									-- drop value						
			else
				state_n <= Dstore_long;
				ucode <= ops_NOP;	
			end if;
--			offset <= "00";
			MEMaddr_i <= TOS_r;	
			timer <= 0;
			PC_n <= PC;						
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X_i <= '0';
			MEM_WRQ_Y_i <= '0';	
			MEM_WRQ_Z_i <= '1';
			MEMdataout_X <= NOS;
			MEMdataout_Y <= NOS(7 downto 0);
			MEMdataout_Z <= NOS(31 downto 16);	
			MEMsize_X_n <= "11";
			--MEMsize_Xp <= "11";	
			accumulator_n <= (others=>'0');			
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "0";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			delayed_RTS_n <= delayed_RTS;
			
		when Dstore_long2 =>											
			state_n <= Dstore_long3;									
			ucode <= ops_INC;
			timer <= 0;
			PC_n <= PC;
			MEMaddr_i <= TOS_r;							
--			offset <= "00";
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X_i <= '0';
			MEM_WRQ_Y_i <= '0';	
			MEM_WRQ_Z_i <= '0';
			MEMdataout_X <= NOS;
			MEMdataout_Y <= NOS(7 downto 0);
			MEMdataout_Z <= NOS(15 downto 0);
			MEMsize_X_n <= "11";
			--MEMsize_Xp <= "11";	
			accumulator_n <= (others=>'0');				
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "0";
			ReturnAddress_n <= PC_addr;		
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			delayed_RTS_n <= delayed_RTS;
			
		when Dstore_long3 =>									-- allow one cycle for TOS_r to update			
			state_n <= Dstore_word;									
			ucode <= ops_NOP;
			timer <= 0;
			PC_n <= PC;
			MEMaddr_i <= TOS_r;							
--			offset <= "00";
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X_i <= '0';
			MEM_WRQ_Y_i <= '0';	
			MEM_WRQ_Z_i <= '0';
			MEMdataout_X <= NOS;
			MEMdataout_Y <= NOS(7 downto 0);
			MEMdataout_Z <= NOS(15 downto 0);
			MEMsize_X_n <= "11";
			--MEMsize_Xp <= "11";	
			accumulator_n <= (others=>'0');				
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "0";
			ReturnAddress_n <= PC_addr;		
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			delayed_RTS_n <= delayed_RTS;			
			
		when Dstore_word =>										
			if MEM_RDY_Z = '1' then
				state_n <= Dstore2;
				ucode <= ops_DROP;									-- drop value				
			else
				state_n <= Dstore_word;
				ucode <= ops_NOP;	
			end if;
			timer <= 0;
			PC_n <= PC;					
--			offset <= "00";
			MEMaddr_i <= TOS_r;				
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X_i <= '0';
			MEM_WRQ_Y_i <= '0';	
			MEM_WRQ_Z_i <= '1';
			MEMdataout_X <= NOS;
			MEMdataout_Y <= NOS(7 downto 0);
			MEMdataout_Z <= NOS(15 downto 0);
			MEMsize_X_n <= "11";
			--MEMsize_Xp <= "11";	
			accumulator_n <= (others=>'0');			
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "0";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			delayed_RTS_n <= delayed_RTS;

		when Dstore_byte =>										
			if MEM_RDY_Y = '1' then
				state_n <= Dstore2;
				ucode <= ops_DROP;									-- drop value				
			else
				state_n <= Dstore_byte;
				ucode <= ops_NOP;	
			end if;
			timer <= 0;
			PC_n <= PC;				
--			offset <= "00";
			MEMaddr_i <= TOS_r;				
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X_i <= '0';
			MEM_WRQ_Y_i <= '1';	
			MEM_WRQ_Z_i <= '0';
			MEMdataout_X <= NOS;
			MEMdataout_Y <= NOS(7 downto 0);
			MEMdataout_Z <= NOS(15 downto 0);
			MEMsize_X_n <= "11";
			--MEMsize_Xp <= "11";	
			accumulator_n <= (others=>'0');			
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "1";
			ReturnAddress_n <= PC_addr;	
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			delayed_RTS_n <= delayed_RTS;

		when Dstore2 =>										
			state_n <= skip1;
			ucode <= ops_DROP;									-- drop address				
			timer <= 0;
			PC_n <= PC;					
--			offset <= "00";
			MEMaddr_i <= PC_addr;				
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X_i <= '0';
			MEM_WRQ_Y_i <= '0';	
			MEM_WRQ_Z_i <= '0';
			MEMdataout_X <= NOS;
			MEMdataout_Y <= NOS(7 downto 0);
			MEMdataout_Z <= NOS(15 downto 0);
			MEMsize_X_n <= "11";
			--MEMsize_Xp <= "11";	
			accumulator_n <= (others=>'0');
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "0";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			delayed_RTS_n <= delayed_RTS;

		when load_long =>										-- load a literal longword
			state_n <= load_long2;
			timer <= 0;
			PC_n <= PC_plus;
			ucode <= ops_NOP;																			
			accumulator_n <= (others=>'0');
--			offset <= "00";
			MEMaddr_i <= PC_addr;				
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X_i <= '0';
			MEM_WRQ_Y_i <= '0';	
			MEM_WRQ_Z_i <= '0';
			MEMsize_X_n <= "11";
			--MEMsize_Xp <= "11";
			MEMdataout_X <= NOS;
			MEMdataout_Y <= NOS(7 downto 0);
			MEMdataout_Z <= NOS(15 downto 0);	
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "0";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			delayed_RTS_n <= delayed_RTS;

		when load_long2 =>										
			state_n <= load_word;
			timer <= 0;
			PC_n <= PC_plus;
			ucode <= ops_NOP;																			
			accumulator_n <= (others=>'0');
--			offset <= "00";
			MEMaddr_i <= PC_addr;				
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X_i <= '0';
			MEM_WRQ_Y_i <= '0';	
			MEM_WRQ_Z_i <= '0';
			MEMsize_X_n <= "11";
			--MEMsize_Xp <= "11";
			MEMdataout_X <= NOS;
			MEMdataout_Y <= NOS(7 downto 0);
			MEMdataout_Z <= NOS(15 downto 0);	
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "0";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			delayed_RTS_n <= delayed_RTS;

		when load_word =>										
			state_n <= load_byte;
			timer <= 0;
			PC_n <= PC_plus;
			ucode <= ops_NOP;																			
			accumulator_n <= (others=>'0');
--			offset <= "00";
			MEMaddr_i <= PC_addr;				
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X_i <= '0';
			MEM_WRQ_Y_i <= '0';	
			MEM_WRQ_Z_i <= '0';
			MEMsize_X_n <= "11";
			--MEMsize_Xp <= "11";
			MEMdataout_X <= NOS;
			MEMdataout_Y <= NOS(7 downto 0);
			MEMdataout_Z <= NOS(15 downto 0);	
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "0";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			delayed_RTS_n <= delayed_RTS;

		when load_byte =>										
			state_n <= common;
			timer <= 0;
			PC_n <= PC_plus;
			ucode <= ops_NOP;																			
			accumulator_n <= (others=>'0');
--			offset <= "00";
			MEMaddr_i <= PC_addr;				
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X_i <= '0';
			MEM_WRQ_Y_i <= '0';	
			MEM_WRQ_Z_i <= '0';
			MEMsize_X_n <= "11";
			--MEMsize_Xp <= "11";
			MEMdataout_X <= NOS;
			MEMdataout_Y <= NOS(7 downto 0);
			MEMdataout_Z <= NOS(15 downto 0);	
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "0";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			delayed_RTS_n <= delayed_RTS;
			
		when jsl_state =>										
			state_n <= common;
			timer <= 0;
			PC_n <= PC_plus;
			ucode <= ops_NOP;																			
			accumulator_n <= (others=>'0');
--			offset <= "00";
			MEMaddr_i <= PC_addr;				
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X_i <= '0';
			MEM_WRQ_Y_i <= '0';	
			MEM_WRQ_Z_i <= '0';
			MEMsize_X_n <= "11";
			--MEMsize_Xp <= "11";
			MEMdataout_X <= NOS;
			MEMdataout_Y <= NOS(7 downto 0);
			MEMdataout_Z <= NOS(15 downto 0);	
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "0";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			delayed_RTS_n <= delayed_RTS;
			
		when skip1 =>										-- after changing PC, this wait state allows a memory read before execution of next instruction
			state_n <= common;
			timer <= 0;
			PC_n <= PC_plus;
			ucode <= ops_NOP;																			
			accumulator_n <= (others=>'0');
--			offset <= "00";
			MEMaddr_i <= PC_addr;				
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X_i <= '0';
			MEM_WRQ_Y_i <= '0';	
			MEM_WRQ_Z_i <= '0';
			MEMsize_X_n <= "11";
			--MEMsize_Xp <= "11";
			MEMdataout_X <= NOS;
			MEMdataout_Y <= NOS(7 downto 0);
			MEMdataout_Z <= NOS(15 downto 0);	
			if delayed_RTS= '1' then
				AuxControl_n(0 downto 0) <= "1";					-- decrement return stack pointer
			else
				AuxControl_n(0 downto 0) <= "0";
			end if;
			AuxControl_n(1 downto 1) <= "0";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			delayed_RTS_n <= delayed_RTS;
	
		when skip2 =>										
			state_n <= common;
			timer <= 1;
			PC_n <= PC_plus;
			ucode <= ops_NOP;							
			accumulator_n <= (others=>'0');
--			offset <= "00";
			MEMaddr_i <= PC_addr;				
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X_i <= '0';
			MEM_WRQ_Y_i <= '0';	
			MEM_WRQ_Z_i <= '0';
			MEMsize_X_n <= "11";
			--MEMsize_Xp <= "11";
			MEMdataout_X <= NOS;
			MEMdataout_Y <= NOS(7 downto 0);
			MEMdataout_Z <= NOS(15 downto 0);	
			if delayed_RTS= '1' then
				AuxControl_n(0 downto 0) <= "1";					-- decrement return stack pointer
			else
				AuxControl_n(0 downto 0) <= "0";
			end if;
			AuxControl_n(1 downto 1) <= "0";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			delayed_RTS_n <= delayed_RTS;

		end case;
	end process;
end RTL;

--Copyright and license
--=====================
--
--The N.I.G.E machine, its design and its source code are Copyright (C) 2012 by Andrew Richard Read and dual licensed.
--    
--(1) For commercial or proprietary use you must obtain a commercial license agreement with Andrew Richard Read (anding_eunding@yahoo.com)
--    
--(2) You can redistribute the N.I.G.E. Machine, its design and its source code and/or modify it under the terms of the GNU General Public License 
--as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. 
--
--The N.I.G.E Machine is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
--or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public 
--License along with this repository.  If not, see <http://www.gnu.org/licenses/>.


-- NEXT STEPS
-- remove LSL instruction