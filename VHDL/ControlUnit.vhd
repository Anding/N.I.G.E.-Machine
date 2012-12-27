-- Control unit
-- Andrew Read
-- Created 22 May 2011

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
			  MicroControl : out STD_LOGIC_VECTOR (13 downto 0);		-- ouput control logic
			  AuxControl : out STD_LOGIC_VECTOR (2 downto 0);			-- output control logic
			  Accumulator : out STD_LOGIC_VECTOR (31 downto 0);		-- literal value captured from memory for writing to TOS
			  ReturnAddress : out STD_LOGIC_VECTOR (31 downto 0);		-- return address on interrupt, BSR or JSR for writing to TORS
			  MEMaddr : out STD_LOGIC_VECTOR (31 downto 0);						  	  
			  MEMdatain_X_extended : in STD_LOGIC_VECTOR (39 downto 0);		-- 5 byte wide SRAM data IN memory bus
			  MEMdataout_X : out STD_LOGIC_VECTOR (31 downto 0);		-- 32 bit wide SRAM data memory bus
			  MEMsize_X	: out STD_LOGIC_VECTOR (1 downto 0);			-- 32 bit wide SRAM data memory bus
			  MEMsize_Xp: out STD_LOGIC_VECTOR (1 downto 0);			-- 32 bit wide SRAM data memory bus
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
--constant ops_SDIVMODLD : std_logic_vector(5 downto 0) := CONV_STD_LOGIC_VECTOR(61,6);
--constant ops_UDIVMODLD : std_logic_vector(5 downto 0) := CONV_STD_LOGIC_VECTOR(62,6);
constant ops_JSI : std_logic_vector(5 downto 0) := CONV_STD_LOGIC_VECTOR(63,6);

-- branch codes (bits 7 downto 6) of the instructions
constant bps_RTS : std_logic_vector(1 downto 0) := "01";
constant bps_BEQ : std_logic_vector(1 downto 0) := "10";
constant bps_BRA : std_logic_vector(1 downto 0) := "11";

-- interrupt vector
constant int_vector_TRAP  : std_logic_vector (31 downto 0) := CONV_STD_LOGIC_VECTOR(2,32);
constant int_vector_IRQ0  : std_logic_vector (7 downto 0) := CONV_STD_LOGIC_VECTOR(2,8);

type state_T is (common, load_long, load_word, load_byte, ifdup, smult, umult, sdivmod, udivmod, sdivmod_load, udivmod_load, 
						Sfetch_long, Sfetch_word, Sfetch_byte,
						Sstore_long, Sstore_long2, Sstore_word, Sstore_byte, Sstore_end,
						Dfetch_long, Dfetch_long2, Dfetch_word, Dfetch_byte,
						Dstore_long, Dstore_long2, Dstore_word, Dstore_byte, Dstore2,
						branchstate, branch_load, branch_eq_load, branch_ne_load, skip1, skip2);
type offset_T is (none, one, two, four);						
						
signal state, state_n  : state_T;										-- state machine
signal PC, PC_n, PC_plus, PC_jsl, PC_branch, PC_m1, PC_skipbranch : std_logic_vector (31 downto 0);		-- program counter logic
signal delta :std_logic_vector (31 downto 0);
signal plus : std_logic_vector (2 downto 0);
signal accumulator_i, accumulator_n, accumulator_X : std_logic_vector (31 downto 0); -- shift register for compiling LONGS and WORDS with BYTE reads
signal accumulator_Y, accumulator_Z : std_logic_vector (31 downto 0); 
signal ReturnAddress_n : std_logic_vector (31 downto 0);
signal int_vector_ext : std_logic_vector (31 downto 0);
signal int_vector_ext_i : std_logic_vector (7 downto 0);
signal ucode : std_logic_vector (5 downto 0);					-- address driver for microcode BLOCK RAM
signal equalzero : std_logic;											-- flag '1' when TOS is zero
signal SRAM : std_logic;
signal timer : integer range 0 to 63;								-- timer/counter for state machine
signal count : integer range 0 to 63;								-- Pedroni, "Circuit Design and Simulation with VHDL" p298
signal int_trig : std_logic;
signal irq_m1, irq_n : std_logic;
signal irv_i : std_logic_vector (7 downto 0);
signal retrap, retrap_n : std_logic_vector(1 downto 0);
signal AuxControl_i, AuxControl_n : std_logic_vector(2 downto 0);
signal opcode, next_opcode : std_logic_vector(5 downto 0);					-- opcode of current and next instructions
signal branch, next_branch : std_logic_vector(1 downto 0);					-- branch codes of current and next instructions
signal offset : std_logic_vector(1 downto 0);	

alias signbit is MEMdatain_X_extended(37);

begin

	inst_Microcode_ROM : Microcode_ROM										-- microcode BLOCK RAM
	PORT MAP (
	 clka => clk,
	 addra => ucode,
	 douta => MicroControl
	);
	
--	with offset select
	opcode <= MEMdatain_X_extended(37 downto 32); --when none,				-- position within 5 byte extended data
--						MEMdatain_X_extended(29 downto 24) when one,
--						MEMdatain_X_extended(21 downto 16) when two,
--						MEMdatain_X_extended(5 downto 0) when four;

--	with offset select
	branch <= MEMdatain_X_extended(39 downto 38); -- when none,				-- position within 5 byte extended data
--						MEMdatain_X_extended(31 downto 30) when one,
--						MEMdatain_X_extended(23 downto 22) when two,
--						MEMdatain_X_extended(7 downto 6) when four;
   
	with offset select
		next_opcode <= MEMdatain_X_extended(37 downto 32) when "00",		-- re-prime pipleline
							MEMdatain_X_extended(29 downto 24) when "01",		-- single byte instruction
							MEMdatain_X_extended(21 downto 16) when "10",		-- two byte instruction (#.b)
							MEMdatain_X_extended(13 downto 8) when others;		-- three byte instruction (#.w)
	
	with offset select
		next_branch <= MEMdatain_X_extended(39 downto 38) when "00",		-- likely don't need the extra byte in X_extended!
							MEMdatain_X_extended(31 downto 30) when "01",
							MEMdatain_X_extended(23 downto 22) when "10",
							MEMdatain_X_extended(15 downto 14) when others;
 
	Accumulator <= Accumulator_i;
	AuxControl <= AuxControl_i;
	
	equalzero <= '1' when TOS = 0 else '0'; 						-- flag used for ?DUP, BEQ, and TEST instructions
	SRAM <= '1' when (TOS(31) or TOS(30) or TOS(29) or TOS(28) or TOS(27) or TOS(26) or TOS(25) or TOS(24) or
						  TOS(23) or TOS(22) or TOS(21) or TOS(20) or TOS(19) or TOS(18) or TOS(17) or TOS(16)) = '0' else '0';
						  
	accumulator_X <= accumulator_i(23 downto 0) & MEMdatain_X_extended(15 downto 8);	-- compile WORDs and LONGs from sequential reads
	accumulator_Y <= accumulator_i(23 downto 0) & MEMdatain_Y;
	accumulator_Z <= accumulator_i(15 downto 0) & MEMdatain_Z;	
	
	int_trig <= irq or irq_m1;
	irv_i <= "000" & irv & "0";										-- double the interrupt vector number
	int_vector_ext_i <= int_vector_IRQ0 + irv_i;					-- add to the IRQ0 base
	int_vector_ext <= "000000000000000000000000" & int_vector_ext_i;	-- extend to width of address bus

	PC_plus <= PC + plus;												-- states set plus to "000", "001", etc. to increment PC as appropriate
	PC_jsl <= "00000000" & MEMdatain_X_extended(31 downto 8);
	delta  <= signbit & signbit & signbit & signbit & signbit & signbit & signbit & signbit & signbit & signbit & signbit & signbit & signbit & signbit & 
				  signbit & signbit & signbit & signbit & signbit & MEMdatain_X_extended(36 downto 24);
	PC_branch <= PC + delta;												-- sign extended 14 bit branch for BRA or BEQ	
	PC_skipbranch <= PC + "00000000000000000000000000000010";	-- PC + 2 for skipping a BEQ branch
	
	-- combinatorial process to determine the size of the next opcode for incrementing the PC
	process (next_opcode, next_branch)
	begin
		if 
		next_branch = "00" then
			if next_opcode = ops_LONG then
				plus <= "101";											-- advance by 5 bytes for a long literal
			elsif next_opcode = ops_WORD then
				plus <= "011";											-- 3 bytes for a word literal
			elsif next_opcode = ops_BYTE then
				plus <= "010";											-- 2 bytes for a byte literal
			elsif next_opcode = ops_JSL then
				plus <= "100";											-- 4 bytes for a JSL instruction (i.e. for the return address)
			else															
				plus <= "001";			
			end if;
		else								-- branches do not utilize increment so leave PC unchanged as the base
			plus <= "000";
		end if;
	end process;
	
	-- main control unit state machine
	process																			
	begin																		-- sequential (registered) section of state machine
		wait until rising_edge(clk);
		if rst = '0' then													
			count <= count + 1;
			irq_m1 <= irq_n;
			ReturnAddress <= ReturnAddress_n;	
			accumulator_i <= accumulator_n;			
			retrap <= retrap_n;
			AuxControl_i <= AuxControl_n;
			if (count >= timer) then 
				state <= state_n;
				count <= 0;	
				PC <= PC_n;													-- PC is updated only on the final cycle of multi-cycle opcode states
				PC_m1 <= PC;													-- PC_m1 is PC of prior cycle, needed for branch and returns due to 1 stage pipeline				
			end if;	
		else																	-- synchronous reset
			state <= skip2;
			count <= 0;
			PC <= (others=>'0');
			PC_m1 <= (others=>'0');
			irq_m1 <= '0';
			retrap <= "00";
			AuxControl_i <= "000";
		end if;
	end process;

	process (state, state_n, PC, PC_n, PC_plus, PC_jsl, PC_branch, PC_skipbranch, PC_m1, delta, plus, 
				accumulator_i, accumulator_n, accumulator_X, accumulator_X, accumulator_Y, accumulator_Z,
				ucode, equalzero,branch, opcode, SRAM, MEMdatain_X_extended, retrap,
				TOS, NOS, TORS, int_trig, MEM_RDY_Y, MEM_RDY_Z, int_vector_ext, int_vector_ext_i, branch, opcode)
	begin																					-- combinational section of state machine
		case state is

		when common =>																	-- common state executes most instructions in 1 clock cycle
		
		-- Next state logic		
			if int_trig = '1' then
				state_n <= skip2;					
			elsif branch = bps_RTS then											-- check branches first as they use the opcode bits for offsets
				state_n <= skip2;					
			elsif branch = bps_BRA  then											
				state_n <= skip2;
			elsif branch = bps_BEQ then				
				state_n <= branchstate;
			elsif opcode = ops_JSR or opcode = ops_JMP or opcode = ops_JSL or 
					opcode = ops_TRAP or retrap(0) = '1' then
				state_n <= skip2;
			elsif opcode = ops_lfetch then
				if SRAM = '1' then
					state_n <= skip2;					
				else
					state_n <= Dfetch_long;
				end if;	
			elsif opcode = ops_wfetch then
				if SRAM = '1' then
					state_n <= Sfetch_word;					
				else
					state_n <= Dfetch_word;
				end if;
			elsif opcode = ops_cfetch then
				if SRAM = '1' then 
					state_n <= Sfetch_byte;
				else
					state_n <= Dfetch_byte;	
				end if;
			elsif opcode = ops_lstore then
				if SRAM = '1' then
					state_n <= Sstore_long;					
				else
					state_n <= Dstore_long;
				end if;
			elsif opcode = ops_wstore then
				if SRAM = '1' then
					state_n <= Sstore_word;					
				else
					state_n <= Dstore_word;
				end if;							
			elsif opcode = ops_cstore then
				if SRAM = '1' then
					state_n <= Sstore_byte;					
				else
					state_n <= Dstore_byte;
				end if;
			elsif opcode = ops_long then
				state_n <= skip1;														-- extra cycle required since fetch is not wide enough to see beyond long literal
			elsif opcode = ops_ifdup and equalzero = '1' then
				state_n <= ifdup;
			elsif opcode = ops_ifdup and equalzero = '0' then 				-- to make both cases of ifdup execute in the same number of cycles (deterministic)
				state_n <= skip1;
			elsif opcode = ops_SDIVMOD then
				state_n <= sdivmod;		
			elsif opcode = ops_UDIVMOD then
				state_n <= udivmod;
			elsif opcode = ops_SMULT then
				state_n <= smult;
			elsif opcode = ops_UMULT then
				state_n <= umult;
			else
				state_n <= common;
			end if;
			timer <= 0;
			
			-- Memory logic
			if opcode = ops_CSTORE and int_trig = '0' then 		-- store instructions do not use a shift register as loading it would take an extra cycle
				MEMdataout_X <= NOS;
				MEMdataout_Y <= NOS(7 downto 0);
				MEMdataout_Z <= (others=>'0');
				if SRAM = '1' then
					MEM_WRQ_X <= '1';
					MEM_WRQ_Y <= '0';
					MEM_WRQ_Z <= '0';	
				else
					MEM_WRQ_X <= '0';
					MEM_WRQ_Y <= '1';
					MEM_WRQ_Z <= '0';						
				end if;		
			elsif opcode = ops_WSTORE and int_trig = '0' then
				MEMdataout_X <= NOS;
				MEMdataout_Y <= NOS(15 downto 8);
				MEMdataout_Z <= NOS(15 downto 0);
				if SRAM = '1' then
					MEM_WRQ_X <= '1';
					MEM_WRQ_Y <= '0';
					MEM_WRQ_Z <= '0';	
				else
					MEM_WRQ_X <= '0';
					MEM_WRQ_Y <= '0';
					MEM_WRQ_Z <= '1';						
				end if;				
			elsif opcode = ops_LSTORE and int_trig = '0' then
				MEMdataout_X <= NOS;
				MEMdataout_Y <= NOS(31 downto 24);	
				MEMdataout_Z <= NOS(31 downto 16);	
				if SRAM = '1' then
					MEM_WRQ_X <= '1';
					MEM_WRQ_Y <= '0';
					MEM_WRQ_Z <= '0';	
				else
					MEM_WRQ_X <= '0';
					MEM_WRQ_Y <= '0';
					MEM_WRQ_Z <= '1';						
				end if;	
			else
				MEMdataout_X <= (others=>'0');
				MEMdataout_Y <= (others=>'0');
				MEMdataout_Z <= (others=>'0');
				MEM_WRQ_X <= '0';
				MEM_WRQ_Y <= '0';
				MEM_WRQ_Z <= '0';
			end if;
			
			if opcode = ops_CFETCH or opcode = ops_CSTORE then
				MEMsize_X <= "11";
			elsif opcode = ops_WFETCH or opcode = ops_WSTORE then
				MEMsize_X <= "11";
			else
				MEMsize_X <= "11";
			end if;
			
			if opcode = ops_BYTE then						
				MEMsize_Xp <= "01";
			elsif opcode = ops_WORD then
				MEMsize_Xp <= "10";
			else
				MEMsize_Xp <= "11";
			end if;

			if opcode = ops_CFETCH or opcode = ops_WFETCH or opcode = ops_LFETCH or
				opcode = ops_CSTORE or opcode = ops_WSTORE or opcode = ops_LSTORE then  
				MEMaddr <= TOS;											
			else																
				MEMaddr <= PC;											
			end if;	
			
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';	
					
			-- Program counter logic
			if int_trig = '1' then
				PC_n <= int_vector_ext;								-- PC from external interrupt vector
			elsif branch = bps_BRA then
				PC_n <= PC_branch;				
			elsif branch = bps_BEQ then
				if equalzero = '1' then
					PC_n <= PC_branch;	
				else
					PC_n <= PC_skipbranch;
				end if;
			elsif opcode = ops_TRAP or retrap(0) = '1' then
				PC_n <= int_vector_TRAP;							-- PC from internal interrup vector
			elsif branch = bps_RTS then
				PC_n <= TORS;											-- PC from Top Of Return Stack (also covers RTI and RETRAP, which include RTS by default)
			elsif opcode = ops_JSL then
				PC_n <= PC_jsl;									
			elsif opcode = ops_JSR or opcode = ops_JMP then
				PC_n <= TOS;											-- PC from TOS	
			elsif opcode = ops_CFETCH or opcode = ops_WFETCH or opcode = ops_LFETCH or
				opcode = ops_SDIVMOD or opcode = ops_UDIVMOD or opcode = ops_IFDUP or
				opcode = ops_CSTORE or opcode = ops_WSTORE or opcode = ops_LSTORE or
				opcode = ops_SMULT or opcode = ops_UMULT or opcode = ops_LONG then  		
				PC_n <= PC;												-- PC update is done on the final cycle of multi-cycle instructions
			else
				PC_n <= PC_plus;												
			end if;		
			
			-- Program counter next instruction offset logic
			if opcode = ops_WORD then
				offset <= "11";											
			elsif opcode = ops_BYTE then
				offset <= "10";											
			else															
				offset <= "01";											
			end if;	
			
			-- Microcode logic
			if int_trig = '1' or opcode = ops_TRAP or retrap(0) = '1' then
				ucode <= ops_JSI;										-- interrupt microcode 
			elsif branch = bps_BEQ or branch = bps_BRA then
				ucode <= ops_NOP;										-- avoid executing the high 6 bits of the branch offset as an opcode!
			elsif opcode = ops_RTI or opcode = ops_TRAP or opcode = ops_RETRAP or retrap(0) = '1' then
				ucode <= ops_NOP;										-- these instructions have no microcode but overlap with internal microcode
			elsif opcode = ops_SDIVMOD or opcode = ops_UDIVMOD then
				ucode <= ops_NOP;										-- suppress microcode until last cycle of these instructions
			elsif SRAM = '0' and (opcode = ops_CSTORE or opcode = ops_CFETCH or opcode = ops_WSTORE or
									   opcode = ops_WFETCH or opcode = ops_LSTORE or opcode = ops_LFETCH) then
				ucode <= ops_NOP;										-- need to surpress the microcode when accessing DRAM					
			else
				ucode <= opcode;
			end if;

			-- Accumulator logic
			if branch = bps_BEQ or branch = bps_BRA then
				accumulator_n <= accumulator_X;					-- capture high 6 bits of branch address
			else
				accumulator_n <= (others=>'0');
			end if;
			
			-- Data multiplexer logic
			if opcode = ops_CFETCH or opcode = ops_WFETCH or opcode = ops_LFETCH then  
				AuxControl_n(2 downto 1) <= "01";					-- setting for SRAM fetch											
			else																
				AuxControl_n(2 downto 1) <= "00";					-- setting for load literal instructions										
			end if;

			-- Return stack control logic 
			if (branch = bps_RTS or opcode = ops_RETRAP) and (int_trig = '0') then	-- override on interrupt
				AuxControl_n(0 downto 0) <= "1";					-- decrement return stack pointer
			else
				AuxControl_n(0 downto 0) <= "0";
			end if;
			if int_trig = '1' or retrap(0) = '1' then											
				ReturnAddress_n <= PC_m1;
			else
				ReturnAddress_n <= PC;
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
			
--		when load_long =>											-- load a LONG immediate value	
--			state_n <= load_byte;			
--			timer <= 2;			
--			accumulator_n <= accumulator_X;						-- shift into accumulator byte by byte	
----			plus <= "001";		
--			offset <= "00";				
--			PC_n <= PC_plus;
--			ucode <= ops_NOP;
--			MEMaddr <= PC;			
--			MEM_REQ_Y <= '0';	
--			MEM_REQ_Z <= '0';
--			MEM_WRQ_X <= '0';
--			MEM_WRQ_Y <= '0';	
--			MEM_WRQ_Z <= '0';
--			MEMdataout_X <= (others=>'0');
--			MEMdataout_Y <= (others=>'0');
--			MEMdataout_Z <= (others=>'0');
--			MEMsize_X <= "11";
--			MEMsize_Xp <= "11";
--			AuxControl_n(0 downto 0) <= "0";
--			AuxControl_n(2 downto 1) <= "00";
--			ReturnAddress_n <= PC;
--			irq_n <= int_trig;	
--			rti <= '0';
--			retrap_n <= retrap;
				
--		when load_word =>											-- load a WORD immediate value
--			state_n <= load_byte;
--			accumulator_n <= accumulator_X;						-- shift into accumulator byte by byte	
--			plus <= "001";	
--			offset_n <= none;	
--			timer <= 0;
--			PC_n <= PC_plus;
--			ucode <= ops_NOP;
--			MEMaddr <= PC;	
--			MEM_REQ_Y <= '0';	
--			MEM_REQ_Z <= '0';
--			MEM_WRQ_X <= '0';
--			MEM_WRQ_Y <= '0';	
--			MEM_WRQ_Z <= '0';
--			MEMdataout_X <= (others=>'0');
--			MEMdataout_Y <= (others=>'0');
--			MEMdataout_Z <= (others=>'0');
--			MEMsize_X <= "11";
--			MEMsize_Xp <= "11";
--			AuxControl_n(0 downto 0) <= "0";
--			AuxControl_n(2 downto 1) <= "00";
--			ReturnAddress_n <= PC;
--			irq_n <= int_trig;
--			rti <= '0';
--			retrap_n <= retrap;
				
--		when load_byte =>											-- load a BYTE immediate value, or last step of WORD or LONG
--			state_n <= common;
--			accumulator_n <= accumulator_X;	
--			plus <= "001";											-- increment PC for next instruction	
--			offset_n <= none;	
--			ucode <= ops_PUSH;									-- PUSH immediate onto TOS				
--			timer <= 0;
--			PC_n <= PC_plus;
--			MEMaddr <= PC;	
--			MEM_REQ_Y <= '0';	
--			MEM_REQ_Z <= '0';
--			MEM_WRQ_X <= '0';
--			MEM_WRQ_Y <= '0';	
--			MEM_WRQ_Z <= '0';
--			MEMdataout_X <= (others=>'0');
--			MEMdataout_Y <= (others=>'0');
--			MEMdataout_Z <= (others=>'0');
--			MEMsize_X <= "11";
--			MEMsize_Xp <= "11";
--			AuxControl_n(0 downto 0) <= "0";
--			AuxControl_n(2 downto 1) <= "00";
--			ReturnAddress_n <= PC;
--			irq_n <= int_trig;
--			rti <= '0';
--			retrap_n <= retrap;
	
		when ifdup =>											-- ifdup when TOS was zero
			state_n <= common;
			timer <= 0;
			PC_n <= PC_plus;
			offset <= "00";	
			ucode <= ops_DROP;									-- DROP previously DUP'd value
			accumulator_n <= (others=>'0');
			MEMaddr <= PC;	
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X <= '0';
			MEM_WRQ_Y <= '0';	
			MEM_WRQ_Z <= '0';
			MEMdataout_X <= (others=>'0');
			MEMdataout_Y <= (others=>'0');
			MEMdataout_Z <= (others=>'0');
			MEMsize_X <= "11";
			MEMsize_Xp <= "11";
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(2 downto 1) <= "00";
			ReturnAddress_n <= PC;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			
		when smult =>										-- signed multiply
			state_n <= common;
			timer <= 4;  											-- wait for multiplier
			PC_n <= PC_plus;										-- PC update will take place only on transition to next state
			offset <= "01";	
			ucode <= ops_SMULT;							
			accumulator_n <= (others=>'0');	
			MEMaddr <= PC;	
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X <= '0';
			MEM_WRQ_Y <= '0';	
			MEM_WRQ_Z <= '0';
			MEMdataout_X <= (others=>'0');
			MEMdataout_Y <= (others=>'0');
			MEMdataout_Z <= (others=>'0');
			MEMsize_X <= "11";
			MEMsize_Xp <= "11";
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(2 downto 1) <= "00";
			ReturnAddress_n <= PC;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
	
		when umult =>										-- unsigned multiply
			state_n <= common;
			timer <= 4;  											-- wait for multiplier
			PC_n <= PC_plus;										-- PC update will take place only on transition to next state
			offset <= "01";
			ucode <= ops_UMULT;									
			accumulator_n <= (others=>'0');	
			MEMaddr <= PC;	
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X <= '0';
			MEM_WRQ_Y <= '0';	
			MEM_WRQ_Z <= '0';
			MEMdataout_X <= (others=>'0');
			MEMdataout_Y <= (others=>'0');
			MEMdataout_Z <= (others=>'0');
			MEMsize_X <= "11";
			MEMsize_Xp <= "11";
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(2 downto 1) <= "00";
			ReturnAddress_n <= PC;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
	
		when sdivmod =>										-- signed division
			state_n <= sdivmod_load;
			timer <= 42;  											-- wait for divider
			PC_n <= PC;
			offset <= "00";	
			ucode <= ops_NOP;
			accumulator_n <= (others=>'0');	
			MEMaddr <= PC;	
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X <= '0';
			MEM_WRQ_Y <= '0';	
			MEM_WRQ_Z <= '0';
			MEMdataout_X <= (others=>'0');
			MEMdataout_Y <= (others=>'0');
			MEMdataout_Z <= (others=>'0');
			MEMsize_X <= "11";
			MEMsize_Xp <= "11";
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(2 downto 1) <= "00";
			ReturnAddress_n <= PC;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;

		when udivmod =>										-- unsigned division
			state_n <= udivmod_load;
			timer <= 41;											-- wait for divider
			PC_n <= PC;
			offset <= "00";
			ucode <= ops_NOP;
			accumulator_n <= (others=>'0');	
			MEMaddr <= PC;	
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X <= '0';
			MEM_WRQ_Y <= '0';	
			MEM_WRQ_Z <= '0';
			MEMdataout_X <= (others=>'0');
			MEMdataout_Y <= (others=>'0');
			MEMdataout_Z <= (others=>'0');
			MEMsize_X <= "11";
			MEMsize_Xp <= "11";
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(2 downto 1) <= "00";
			ReturnAddress_n <= PC;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			
		when sdivmod_load =>									-- signed division
			state_n <= common;									
			timer <= 0;  
			PC_n <= PC_plus;
			offset <= "01";
			ucode <= ops_SDIVMOD;								-- load TOS and NOS with results
			accumulator_n <= (others=>'0');
			MEMaddr <= PC;	
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X <= '0';
			MEM_WRQ_Y <= '0';	
			MEM_WRQ_Z <= '0';
			MEMdataout_X <= (others=>'0');
			MEMdataout_Y <= (others=>'0');
			MEMdataout_Z <= (others=>'0');
			MEMsize_X <= "11";
			MEMsize_Xp <= "11";
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(2 downto 1) <= "00";
			ReturnAddress_n <= PC;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
	
		when udivmod_load =>									-- unsigned division
			state_n <= common;
			timer <= 0;
			PC_n <= PC_plus;
			offset <= "01";
			ucode <= ops_UDIVMOD;								-- load TOS and NOS with results
			accumulator_n <= (others=>'0');
			MEMaddr <= PC;	
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X <= '0';
			MEM_WRQ_Y <= '0';	
			MEM_WRQ_Z <= '0';
			MEMdataout_X <= (others=>'0');
			MEMdataout_Y <= (others=>'0');
			MEMdataout_Z <= (others=>'0');
			MEMsize_X <= "11";
			MEMsize_Xp <= "11";
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(2 downto 1) <= "00";
			ReturnAddress_n <= PC;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;

		when Sfetch_long =>										-- replace all with SKIP2 after placing a 1 cycle delay in MEMsize_X and setting in common					
			state_n <= skip1;									   -- may need to have a separate SIZE_W and SIZE_R for the store cycle?
			accumulator_n <= (others=>'0');
			ucode <= ops_NOP;				
			timer <= 0;
			PC_n <= PC;
			offset <= "00";
			MEMaddr <= PC;				
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X <= '0';
			MEM_WRQ_Y <= '0';	
			MEM_WRQ_Z <= '0';
			MEMdataout_X <= (others=>'0');
			MEMdataout_Y <= (others=>'0');
			MEMdataout_Z <= (others=>'0');	
			MEMsize_X <= "11";
			MEMsize_Xp <= "11";			
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(2 downto 1) <= "00";
			ReturnAddress_n <= PC;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;

		when Sfetch_word =>											
			state_n <= skip1;									
			accumulator_n <= (others=>'0');
			ucode <= ops_NOP;				
			timer <= 0;
			PC_n <= PC;
			offset <= "00";
			MEMaddr <= PC;				
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X <= '0';
			MEM_WRQ_Y <= '0';	
			MEM_WRQ_Z <= '0';
			MEMdataout_X <= (others=>'0');
			MEMdataout_Y <= (others=>'0');
			MEMdataout_Z <= (others=>'0');	
			MEMsize_X <= "10";
			MEMsize_Xp <= "11";
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(2 downto 1) <= "00";
			ReturnAddress_n <= PC;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;

		when Sfetch_byte =>											
			state_n <= skip1;									
			accumulator_n <= (others=>'0');
			ucode <= ops_NOP;				
			timer <= 0;
			PC_n <= PC;
			offset <= "00";
			MEMaddr <= PC;				
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X <= '0';
			MEM_WRQ_Y <= '0';	
			MEM_WRQ_Z <= '0';
			MEMdataout_X <= (others=>'0');
			MEMdataout_Y <= (others=>'0');
			MEMdataout_Z <= (others=>'0');
			MEMsize_X <= "01";
			MEMsize_Xp <= "11";
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(2 downto 1) <= "00";
			ReturnAddress_n <= PC;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;

		when Sstore_long =>											
			state_n <= skip1	--Sstore_long2;									
			ucode <= ops_drop;											
			timer <= 0;
			PC_n <= PC; --PC_plus;
			offset <= "00";
			MEMaddr <= PC;						
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X <= '1';
			MEM_WRQ_Y <= '0';	
			MEM_WRQ_Z <= '0';
			MEMdataout_X <= (others=>'0');
			MEMdataout_Y <= (others=>'0'); --NOS(23 downto 16);
			MEMdataout_Z <= (others=>'0');
			MEMsize_X <= "11";
			MEMsize_Xp <= "11";
			accumulator_n <= (others=>'0');			
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(2 downto 1) <= "00";
			ReturnAddress_n <= PC;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;

		when Sstore_long2 =>											
			state_n <= Sstore_word;									
			ucode <= ops_INC;											
			timer <= 0;
			PC_n <= PC_plus;
			--plus <= "000";
			offset <= "00";
			MEMaddr <= TOS;				
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X <= '1';
			MEM_WRQ_Y <= '0';	
			MEM_WRQ_Z <= '0';
			MEMdataout_X <= (others=>'0');
			MEMdataout_Y <= NOS(15 downto 8);
			MEMdataout_Z <= (others=>'0');
			MEMsize_X <= "11";
			MEMsize_Xp <= "11";
			accumulator_n <= (others=>'0');			
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(2 downto 1) <= "00";
			ReturnAddress_n <= PC;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;

		when Sstore_word =>											
			state_n <= Sstore_byte;									
			ucode <= ops_DROP;								-- drop address			
			timer <= 0;
			PC_n <= PC_plus;
			offset <= "00";
			--plus <= "000";		
			MEMaddr <= TOS;				
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X <= '1';
			MEM_WRQ_Y <= '0';	
			MEM_WRQ_Z <= '0';
			MEMdataout_X <= (others=>'0');
			MEMdataout_Y <= NOS(7 downto 0);
			MEMdataout_Z <= (others=>'0');
			MEMsize_X <= "11";
			MEMsize_Xp <= "11";
			accumulator_n <= (others=>'0');			
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(2 downto 1) <= "00";
			ReturnAddress_n <= PC;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;

		when Sstore_byte =>											
			state_n <= Sstore_end;									
			ucode <= ops_DROP;								-- drop address			
			timer <= 0;
			PC_n <= PC_plus;
			offset <= "00";
--			plus <= "000"; 		
			MEMaddr <= TOS_r;									-- registered value of TOS still contains address	
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X <= '0';
			MEM_WRQ_Y <= '0';	
			MEM_WRQ_Z <= '0';
			MEMdataout_X <= TOS;								-- unregistered value of TOS contains value
			MEMdataout_Y <= (others=>'0');
			MEMdataout_Z <= (others=>'0');
			MEMsize_X <= "01";
			MEMsize_Xp <= "11";
			accumulator_n <= (others=>'0');			
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(2 downto 1) <= "00";
			ReturnAddress_n <= PC;	
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;

		when Sstore_end =>									-- restart pipeline						
			state_n <= common;									
			ucode <= ops_DROP;								-- drop value			
			timer <= 0;
			PC_n <= PC_plus;
			--plus <= "001"; 		
			offset <= "00";
			MEMaddr <= PC;				
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X <= '0';
			MEM_WRQ_Y <= '0';	
			MEM_WRQ_Z <= '0';
			MEMdataout_X <= (others=>'0');
			MEMdataout_Y <= (others=>'0');
			MEMdataout_Z <= (others=>'0');
			MEMsize_X <= "11";
			MEMsize_Xp <= "11";
			accumulator_n <= (others=>'0');			
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(2 downto 1) <= "00";
			ReturnAddress_n <= PC;	
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;

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
			offset <= "00";
			MEMaddr <= TOS;				
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '1';
			MEM_WRQ_X <= '0';
			MEM_WRQ_Y <= '0';	
			MEM_WRQ_Z <= '0';
			MEMdataout_X <= (others=>'0');
			MEMdataout_Y <= (others=>'0');
			MEMdataout_Z <= (others=>'0');		
			MEMsize_X <= "11";
			MEMsize_Xp <= "11";
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(2 downto 1) <= "00";
			ReturnAddress_n <= PC;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;

		when Dfetch_long2 =>											
			state_n <= Dfetch_word;									
			accumulator_n <= accumulator_i;		
			ucode <= ops_INC;
			timer <= 0;
			PC_n <= PC;					
			offset <= "00";
			MEMaddr <= TOS;				
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X <= '0';
			MEM_WRQ_Y <= '0';	
			MEM_WRQ_Z <= '0';
			MEMdataout_X <= (others=>'0');
			MEMdataout_Y <= (others=>'0');
			MEMdataout_Z <= (others=>'0');
			MEMsize_X <= "11";
			MEMsize_Xp <= "11";
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(2 downto 1) <= "00";
			ReturnAddress_n <= PC;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			
		when Dfetch_word =>											
			if MEM_RDY_Z = '1' then
				state_n <= skip1;									
				accumulator_n <= accumulator_Z;
				ucode <= ops_REPLACE;	
			else
				state_n <= Dfetch_word;
				accumulator_n <= accumulator_i;	
				ucode <= ops_NOP;	
			end if;
			timer <= 0;	
			PC_n <= PC;
			offset <= "00";
			MEMaddr <= TOS;			
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '1';			
			MEM_WRQ_X <= '0';
			MEM_WRQ_Y <= '0';	
			MEM_WRQ_Z <= '0';
			MEMdataout_X <= (others=>'0');
			MEMdataout_Y <= (others=>'0');
			MEMdataout_Z <= (others=>'0');
			MEMsize_X <= "11";
			MEMsize_Xp <= "11";
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(2 downto 1) <= "11";
			ReturnAddress_n <= PC;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
				
		when Dfetch_byte =>											
			if MEM_RDY_Y = '1' then
				state_n <= skip1;									
				accumulator_n <= accumulator_Y;
				ucode <= ops_REPLACE;
			else
				state_n <= Dfetch_byte;
				accumulator_n <= accumulator_i;	
				ucode <= ops_NOP;		
			end if;
			timer <= 0;
			PC_n <= PC;					
			offset <= "00";
			MEMaddr <= TOS;
			MEM_REQ_Y <= '1';				
			MEM_REQ_Z <= '0';
			MEM_WRQ_X <= '0';
			MEM_WRQ_Y <= '0';	
			MEM_WRQ_Z <= '0';
			MEMdataout_X <= (others=>'0');
			MEMdataout_Y <= (others=>'0');
			MEMdataout_Z <= (others=>'0');	
			MEMsize_X <= "11";
			MEMsize_Xp <= "11";			
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(2 downto 1) <= "11";
			ReturnAddress_n <= PC;	
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
				
		when Dstore_long =>										-- store BYTE in memory or final step of LONG or WORD store
			if MEM_RDY_Z = '1' then
				state_n <= Dstore_long2;
				ucode <= ops_INC;									-- drop value						
			else
				state_n <= Dstore_long;
				ucode <= ops_NOP;	
			end if;
			offset <= "00";
			MEMaddr <= TOS;	
			timer <= 0;
			PC_n <= PC;						
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X <= '0';
			MEM_WRQ_Y <= '0';	
			MEM_WRQ_Z <= '1';
			MEMdataout_X <= (others=>'0');
			MEMdataout_Y <= (others=>'0');
			MEMdataout_Z <= NOS(31 downto 16);	
			MEMsize_X <= "11";
			MEMsize_Xp <= "11";	
			accumulator_n <= (others=>'0');			
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(2 downto 1) <= "00";
			ReturnAddress_n <= PC;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			
		when Dstore_long2 =>											
			state_n <= Dstore_word;									
			ucode <= ops_INC;
			timer <= 0;
			PC_n <= PC;
			MEMaddr <= TOS;							
			offset <= "00";
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X <= '0';
			MEM_WRQ_Y <= '0';	
			MEM_WRQ_Z <= '0';
			MEMdataout_X <= (others=>'0');
			MEMdataout_Y <= (others=>'0');
			MEMdataout_Z <= NOS(15 downto 0);
			MEMsize_X <= "11";
			MEMsize_Xp <= "11";	
			accumulator_n <= (others=>'0');				
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(2 downto 1) <= "00";
			ReturnAddress_n <= PC;		
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			
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
			offset <= "00";
			MEMaddr <= TOS;				
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X <= '0';
			MEM_WRQ_Y <= '0';	
			MEM_WRQ_Z <= '1';
			MEMdataout_X <= (others=>'0');
			MEMdataout_Y <= (others=>'0');
			MEMdataout_Z <= NOS(15 downto 0);
			MEMsize_X <= "11";
			MEMsize_Xp <= "11";	
			accumulator_n <= (others=>'0');			
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(2 downto 1) <= "00";
			ReturnAddress_n <= PC;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;

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
			offset <= "00";
			MEMaddr <= TOS;				
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X <= '0';
			MEM_WRQ_Y <= '1';	
			MEM_WRQ_Z <= '0';
			MEMdataout_X <= (others=>'0');
			MEMdataout_Y <= NOS(7 downto 0);
			MEMdataout_Z <= (others=>'0');
			MEMsize_X <= "11";
			MEMsize_Xp <= "11";	
			accumulator_n <= (others=>'0');			
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(2 downto 1) <= "00";
			ReturnAddress_n <= PC;	
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;

		when Dstore2 =>										
			state_n <= common;
			ucode <= ops_DROP;									-- drop address				
			timer <= 0;
			PC_n <= PC_plus;					
			offset <= "00";
			MEMaddr <= PC;				
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X <= '0';
			MEM_WRQ_Y <= '0';	
			MEM_WRQ_Z <= '0';
			MEMdataout_X <= (others=>'0');
			MEMdataout_Y <= (others=>'0');
			MEMdataout_Z <= (others=>'0');
			MEMsize_X <= "11";
			MEMsize_Xp <= "11";	
			accumulator_n <= (others=>'0');
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(2 downto 1) <= "00";
			ReturnAddress_n <= PC;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			
		when branchstate =>										-- BNE 
			state_n <= skip1;										-- change PC, allow one cycle to prime pipeline
			PC_n <= PC;
			ucode <= ops_DROP;									-- remove flag from TOS
			accumulator_n <= (others=>'0');				
			timer <= 0;		
			offset <= "00";
			MEMaddr <= PC;				
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X <= '0';
			MEM_WRQ_Y <= '0';	
			MEM_WRQ_Z <= '0';
			MEMdataout_X <= (others=>'0');
			MEMdataout_Y <= (others=>'0');
			MEMdataout_Z <= (others=>'0');
			MEMsize_X <= "11";
			MEMsize_Xp <= "11";	
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(2 downto 1) <= "00";
			ReturnAddress_n <= PC;			
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;	
	
--		when branch_eq_load =>							-- BNE when TOS = 0
--			state_n <= skip1;										-- change PC, allow one cycle for memory read
--			PC_n <= PC_bra_IMD;
--			ucode <= ops_DROP;									-- remove flag from TOS
--			accumulator_n <= (others=>'0');				
--			timer <= 0;		
----			plus <= "000";	
--			offset <= "00";
--			MEMaddr <= PC;				
--			MEM_REQ_Y <= '0';	
--			MEM_REQ_Z <= '0';
--			MEM_WRQ_X <= '0';
--			MEM_WRQ_Y <= '0';	
--			MEM_WRQ_Z <= '0';
--			MEMdataout_X <= (others=>'0');
--			MEMdataout_Y <= (others=>'0');
--			MEMdataout_Z <= (others=>'0');
--			MEMsize_X <= "11";
--			MEMsize_Xp <= "11";	
--			AuxControl_n(0 downto 0) <= "0";
--			AuxControl_n(2 downto 1) <= "00";
--			ReturnAddress_n <= PC;			
--			irq_n <= int_trig;
--			rti <= '0';
--			retrap_n <= retrap;
--			
--		when branch_ne_load =>							-- BNE when TOS /= 0
--																		-- state included so that conditional branches have fixed cycle time in either flag case
--			state_n <= skip1;										-- change PC, allow one cycle for memory read
--			timer <= 0;
--			PC_n <= PC;
--			ucode <= ops_DROP;									-- remove flag from TOS
--			--plus <= "000";
--			offset <= "00";
--			accumulator_n <= (others=>'0');
--			MEMaddr <= PC;				
--			MEM_REQ_Y <= '0';	
--			MEM_REQ_Z <= '0';
--			MEM_WRQ_X <= '0';
--			MEM_WRQ_Y <= '0';	
--			MEM_WRQ_Z <= '0';
--			MEMdataout_X <= (others=>'0');
--			MEMdataout_Y <= (others=>'0');
--			MEMdataout_Z <= (others=>'0');
--			MEMsize_X <= "11";
--			MEMsize_Xp <= "11";	
--			AuxControl_n(0 downto 0) <= "0";
--			AuxControl_n(2 downto 1) <= "00";
--			ReturnAddress_n <= PC;		
--			irq_n <= int_trig;
--			rti <= '0';
--			retrap_n <= retrap;
			
--		when branch_load =>								-- BRA
--			state_n <= skip1;										-- change PC, allow one cycle for memory read
--			PC_n <= PC_bra_IMD;	
--			accumulator_n <= (others=>'0');
--			ucode <= ops_NOP;			
--			timer <= 0;
--			--plus <= "000";
--			offset <= "00";
--			MEMaddr <= PC;	
--			MEM_REQ_Y <= '0';	
--			MEM_REQ_Z <= '0';
--			MEM_WRQ_X <= '0';
--			MEM_WRQ_Y <= '0';	
--			MEM_WRQ_Z <= '0';
--			MEMsize_X <= "11";
--			MEMsize_Xp <= "11";	
--			MEMdataout_X <= (others=>'0');
--			MEMdataout_Y <= (others=>'0');
--			MEMdataout_Z <= (others=>'0');		
--			AuxControl_n(0 downto 0) <= "0";
--			AuxControl_n(2 downto 1) <= "00";
--			ReturnAddress_n <= PC;
--			irq_n <= int_trig;
--			rti <= '0';
--			retrap_n <= retrap;

		when skip1 =>										-- after changing PC, this wait state allows a memory read before execution of next instruction
			state_n <= common;
			timer <= 0;
			PC_n <= PC_plus;
			ucode <= ops_NOP;																			
			accumulator_n <= (others=>'0');
			offset <= "00";
			MEMaddr <= PC;				
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X <= '0';
			MEM_WRQ_Y <= '0';	
			MEM_WRQ_Z <= '0';
			MEMsize_X <= "11";
			MEMsize_Xp <= "11";
			MEMdataout_X <= (others=>'0');
			MEMdataout_Y <= (others=>'0');
			MEMdataout_Z <= (others=>'0');	
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(2 downto 1) <= "00";
			ReturnAddress_n <= PC;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
	
		when skip2 =>										
			state_n <= common;
			timer <= 1;
			PC_n <= PC_plus;
			ucode <= ops_NOP;							
			accumulator_n <= (others=>'0');
			offset <= "00";
			MEMaddr <= PC;				
			MEM_REQ_Y <= '0';	
			MEM_REQ_Z <= '0';
			MEM_WRQ_X <= '0';
			MEM_WRQ_Y <= '0';	
			MEM_WRQ_Z <= '0';
			MEMsize_X <= "11";
			MEMsize_Xp <= "11";
			MEMdataout_X <= (others=>'0');
			MEMdataout_Y <= (others=>'0');
			MEMdataout_Z <= (others=>'0');	
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(2 downto 1) <= "00";
			ReturnAddress_n <= PC;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			
		when others =>
			null;
	
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
-- Further testing of branch instructions
-- JSR/JMP instruction
-- RTS/RTI instruction  (simplify RTI by making it RTI,RTS by default)
-- add ,rts to multicycle instructions
-- implement JSL instruction & update asmx
-- check timing on FETCH/STORE instructions for PSDRAM
-- FETCH/STORE for SRAM
-- remove BSR, LSL instructions
-- shorten PC to address 512KB
-- removed extended byte on MEMDatain_X_extended
-- consider looptest and +looptest instructions