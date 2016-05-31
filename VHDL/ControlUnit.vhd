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
			  blocked : in STD_LOGIC;											-- indicates that interrupts are currently blocked
			  TOS : in STD_LOGIC_VECTOR (31 downto 0);					-- Top Of Stack (TOS_n from datapath, one cycle ahead of registered value)
			  TOS_r : in STD_LOGIC_VECTOR (31 downto 0);					-- Top Of Stack (TOS from datapath, the registered value)
			  NOS_r : in STD_LOGIC_VECTOR (31 downto 0);					-- Next On Stack (registered value)		  
			  TORS : in STD_LOGIC_VECTOR (31 downto 0);	  				-- Subroutine return address
			  ExceptionAddress : in STD_LOGIC_VECTOR (31 downto 0);	-- Exception return address
			  equalzero : in STD_LOGIC;										-- flag '1' when TOS is zero
			  equalzero_r : in STD_LOGIC;										-- flag '1' when TOS (registered) is zero			  
			  chip_RAM : in STD_LOGIC;											-- flag used to identify SRAM vs. PSDRAM memory access
			  MicroControl : out STD_LOGIC_VECTOR (22 downto 0);		-- ouput control logic
			  AuxControl : out STD_LOGIC_VECTOR (1 downto 0);			-- output control logic
			  Accumulator : out STD_LOGIC_VECTOR (31 downto 0);		-- literal value captured from memory for writing to TOS
			  ReturnAddress : out STD_LOGIC_VECTOR (31 downto 0);		-- return address on interrupt, BSR or JSR for writing to TORS
			  MEMaddr : out STD_LOGIC_VECTOR (31 downto 0);						  	  
			  MEMdatain_X : in STD_LOGIC_VECTOR (31 downto 0);			-- 32 bit wide SRAM data IN memory bus
			  MEMdataout_X : out STD_LOGIC_VECTOR (31 downto 0);		-- 32 bit wide SRAM data memory bus
			  MEMsize_X	: out STD_LOGIC_VECTOR (1 downto 0);			-- 32 bit wide SRAM data memory bus
			  MEM_WRQ_X : out STD_LOGIC;										-- 32 bit wide SRAM data memory bus	
			-- 32 bit wide AXI databus
			s_axi_awaddr : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
			s_axi_awvalid : OUT STD_LOGIC;
			s_axi_awready : IN STD_LOGIC;
			-- write
			s_axi_wdata : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
			s_axi_wstrb : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
			s_axi_wvalid : OUT STD_LOGIC;
			s_axi_wready : IN STD_LOGIC;
			-- write response
--			s_axi_bresp : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
--			s_axi_bvalid : IN STD_LOGIC;
--			s_axi_bready : OUT STD_LOGIC;
			-- address read
			s_axi_araddr : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
			s_axi_arvalid : OUT STD_LOGIC;
			s_axi_arready : IN STD_LOGIC;
			-- read
--			s_axi_rresp : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
			s_axi_rvalid : IN STD_LOGIC;
			s_axi_rdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
--			s_axi_rready : OUT STD_LOGIC;
			-- virtualization
			PCfreeze : out STD_LOGIC_VECTOR (19 downto 0);
			PCthaw : in STD_LOGIC_VECTOR (19 downto 0);
			singleMulti : IN STD_LOGIC;
			pause : OUT STD_LOGIC;
			VirtualInterrupt : IN STD_LOGIC_VECTOR (19 downto 0);
			Interval : IN STD_LOGIC_VECTOR (15 downto 0);
			debug : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)			
           );
end ControlUnit;

architecture RTL of ControlUnit is

-- opcodes (bits 6 downto 0) of the instructions
constant ops_NOP : std_logic_vector(6 downto 0):= "0000000";
constant ops_DROP : std_logic_vector(6 downto 0) := "0000001";
constant ops_toR : std_logic_vector(6 downto 0) := "0000111";
constant ops_Rfrom : std_logic_vector(6 downto 0) := "0001001";
constant ops_CATCH : std_logic_vector(6 downto 0) := "0001011";
constant ops_THROW : std_logic_vector(6 downto 0) := "0001101";
--
constant ops_IFDUP : std_logic_vector(6 downto 0) := "0110011";
constant ops_INC : std_logic_vector(6 downto 0) := "0010001";
constant ops_SMULT : std_logic_vector(6 downto 0) := "0101001";
constant ops_UMULT : std_logic_vector(6 downto 0) := "0101010";
constant ops_SDIVMOD : std_logic_vector(6 downto 0) := "0101011";
constant ops_UDIVMOD : std_logic_vector(6 downto 0) := "0101100";
constant ops_LFETCH : std_logic_vector(6 downto 0) := "0101101";
constant ops_LSTORE : std_logic_vector(6 downto 0) := "0101110";
constant ops_WFETCH : std_logic_vector(6 downto 0) := "0101111";
constant ops_WSTORE : std_logic_vector(6 downto 0) := "0110000";
constant ops_CFETCH : std_logic_vector(6 downto 0) := "0110001";
constant ops_CSTORE : std_logic_vector(6 downto 0) := "0110010";
constant ops_BYTE : std_logic_vector(6 downto 0) := "0110100";
constant ops_WORD : std_logic_vector(6 downto 0) := "0110101";
constant ops_LONG : std_logic_vector(6 downto 0) := "0110110";
constant ops_JMP : std_logic_vector(6 downto 0) := "0110111";
constant ops_JSL : std_logic_vector(6 downto 0) := "0111000";
constant ops_JSR : std_logic_vector(6 downto 0) := "0111001";
constant ops_TRAP : std_logic_vector(6 downto 0) := "0111010";
constant ops_RETRAP : std_logic_vector(6 downto 0) := "0111011";
constant ops_RTI : std_logic_vector(6 downto 0) := "0111100";
constant ops_PAUSE : std_logic_vector(6 downto 0) := "0111101";

-- internal opcodes used for microcode
constant ops_THROW2 : std_logic_vector(6 downto 0) := "1000001"; 
constant ops_REPLACE  : std_logic_vector(6 downto 0) := "1000010";

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
						Dfetch_long, Dfetch_long2, Dfetch_word, Dfetch_byte, Dfetch_word2, Dfetch_byte2,
						Dstore_long, Dstore_word, Dstore_byte, Dstore2,
						throw0, throw, virtual_interrupt, skip1) ;					
						
signal state, state_n  : state_T;										-- state machine
signal PC, PC_n, PC_plus, PC_jsl, PC_branch, PC_m1, PC_skipbranch : std_logic_vector (19 downto 0);		-- program counter logic
signal PC_plus_two, PC_plus_three, PC_plus_four : std_logic_vector (19 downto 0);	
signal delta :std_logic_vector (19 downto 0);
signal ReturnAddress_n, ReturnAddressJSL, PC_addr : std_logic_vector (31 downto 0);
signal int_vector_ext : std_logic_vector (19 downto 0);
signal int_vector_ext_i : std_logic_vector (7 downto 0);
signal ucode : std_logic_vector (6 downto 0);					-- address driver for microcode BLOCK RAM
signal timer : integer range 0 to 63;								-- timer/counter for state machine
signal count : integer range 0 to 63;								-- Pedroni, "Circuit Design and Simulation with VHDL" p298
signal int_trig : std_logic;
signal irq_m1, irq_n : std_logic;
signal irv_i : std_logic_vector (7 downto 0);
signal retrap, retrap_n : std_logic_vector(1 downto 0);
signal AuxControl_i, AuxControl_n : std_logic_vector(1 downto 0);
signal opcode, next_opcode : std_logic_vector(6 downto 0);					-- opcode of current and next instructions
signal branch, next_branch : std_logic_vector(1 downto 0);					-- branch codes of current and next instructions
signal MEMsize_X_n : STD_LOGIC_VECTOR (1 downto 0);	
signal MEMaddr_i : STD_LOGIC_VECTOR (31 downto 0);	
signal MEM_WRQ_X_i : STD_LOGIC;
signal AXIaddr : STD_LOGIC_VECTOR (1 downto 0);
signal debug_i : std_logic_vector(7 downto 0);
signal PCthaw_m1 : STD_LOGIC_VECTOR (19 downto 0);
signal preemp_counter, preemp_counter_n : STD_LOGIC_VECTOR (15 downto 0);
signal preempt : STD_LOGIC;

-- axi handshake buffers
type handshake_type is (pending, ready, done);
signal s_axi_ar_state, s_axi_aw_state, s_axi_w_state : handshake_type;
signal s_axi_ar_state_n, s_axi_aw_state_n, s_axi_w_state_n : handshake_type;
signal s_axi_rvalid_r : STD_LOGIC;

alias signbit is MEMdatain_X(29);

COMPONENT Microcode_ROM
  PORT (
    clka : IN STD_LOGIC;
    addra : IN STD_LOGIC_VECTOR(6 DOWNTO 0);
    douta : OUT STD_LOGIC_VECTOR(22 DOWNTO 0)
  );
END COMPONENT;

begin

	inst_Microcode_ROM: Microcode_ROM										-- microcode BLOCK RAM
	PORT MAP (
	 clka => clk,
	 addra => ucode,
	 douta => MicroControl
	);

	opcode <= "0" & MEMdatain_X(29 downto 24); 
	branch <= MEMdatain_X(31 downto 30); 

	AuxControl <= AuxControl_i;			  
	
	-- AXI interface constant settings
	s_axi_awaddr <= TOS_r(31 downto 2) & "00";
	s_axi_araddr <= TOS_r(31 downto 2) & "00";	
--	s_axi_bready <= '1';	

	int_trig <= irq or irq_m1;
	irv_i <= "000" & irv & "0";										-- double the interrupt vector number
	int_vector_ext_i <= int_vector_IRQ0 + irv_i;					-- add to the IRQ0 base
	int_vector_ext <= "000000000000" & int_vector_ext_i;		-- extend to width of address bus

	PC_jsl <= MEMdatain_X(19 downto 0);
	delta  <= signbit & signbit & signbit & signbit & signbit & signbit & signbit & MEMdatain_X(28 downto 16);
	PC_branch <= PC + delta;											-- sign extended 14 bit branch for BRA or BEQ	
	PC_addr <= "000000000000" & PC;
	PC_plus <= PC + "001";
	PC_plus_two <= PC + "010";
	PC_plus_three <= PC + "011";
	PC_plus_four <= PC + "100";
	
	MEMaddr <= MEMaddr_i;
	MEM_WRQ_X <= MEM_WRQ_X_i;
				
	preempt <= '1' when (preemp_counter >= interval) and (interval /=0) and (singleMulti = '1')  and (blocked = '0')  else '0'; -- and (opcode < 41) and  (opcode /= 7) and (opcode /=2) and (branch = "00")
	
	-- main control unit state machine
	
	process																			
	begin																		-- faster to separate next-state logic and eliminate shared resources
		wait until rising_edge(clk);
		if rst = '0' then			
			s_axi_ar_state <= s_axi_ar_state_n;
			s_axi_aw_state <= s_axi_aw_state_n;
			s_axi_w_state <= s_axi_w_state_n;
			if (count >= timer) then 
				state <= state_n;			
			end if;	
		else	-- synchronous reset
			s_axi_ar_state <= ready;
			s_axi_aw_state <= ready;
			s_axi_w_state <= ready;				
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
			retrap <= retrap_n;
			MEMsize_X <= MEMsize_X_n;	
			PCthaw_m1 <= PCthaw;
			AuxControl_i <= AuxControl_n;
			debug <= debug_i;
			preemp_counter <= preemp_counter_n;
			s_axi_rvalid_r <= s_axi_rvalid;
--			if int_trig = '1' then
--				blocked <= '1';
--			elsif  opcode = ops_RTI and branch = "01" then 
--				blocked <= '0';
--			end if;
			if (count >= timer) then 
				count <= 0;	
				PC <= PC_n;													-- PC is updated only on the final cycle of multi-cycle opcode states
				PC_m1 <= PC;												-- PC_m1 is PC of prior cycle, needed for branch and returns due to 1 stage pipeline	
			end if;	
			if (state = common) then
				AXIaddr <= TOS(1 downto 0);								-- read address on AXI channel
			end if;
		else																	-- synchronous reset
			count <= 0;
			PC <= (others=>'0');
			PC_m1 <= (others=>'0');
			irq_m1 <= '0';
			retrap <= (others=>'0');
			ReturnAddress <= (others=>'0');
			AuxControl_i <= (others=>'0');
			MEMsize_X <= (others=>'0');
			PCthaw_m1 <= (others=>'0');
			debug <= (others=>'0');
			preemp_counter <= (others=>'0');
			s_axi_rvalid_r <= '0';
--			blocked <= '0';
		end if;
	end process;

	process (state, state_n, PC, PC_n, PC_plus, PC_jsl, PC_branch, PC_skipbranch, PC_m1, PC_addr, delta, PC_plus_two, PC_plus_three, PC_plus_four,
				ucode, equalzero, equalzero_r, branch, opcode, chip_RAM, MEMdatain_X, retrap, s_axi_rvalid_r, s_axi_rdata, axiaddr, preempt,
				TOS, TOS_r, NOS_r, TORS, int_trig, int_vector_ext, int_vector_ext_i, branch, opcode, ExceptionAddress, virtualInterrupt, SingleMulti, PCthaw, PCthaw_m1, preemp_counter, preemp_counter_n,
				s_axi_ar_state, s_axi_aw_state, s_axi_w_state)
	begin																					-- combinational section of state machine
		case state is

		when common =>																	-- common state executes most instructions in 1 clock cycle
		
		-- Next state logic		-- interrupts and timed trap take first priority
										-- check branches next as they use the opcode bits for offsets
	
			if int_trig = '1' or retrap(0) = '1' or branch = bps_BRA or branch = bps_BEQ 
				or opcode = ops_JSL  or opcode = ops_JSR or opcode = ops_JMP or opcode = ops_TRAP or opcode = ops_RETRAP or
				opcode = ops_byte or opcode = ops_word or opcode = ops_long or opcode = ops_catch or 
				(virtualInterrupt = 0 AND (opcode = ops_PAUSE or preempt = '1')) 
			-- or	((opcode = ops_toR or opcode = ops_Rfrom) and MEMdatain_X(23 downto 22) = "01")  -- insert one cycle delay between >R or R> and RTS
				then state_n <= skip1;																				-- 	to allow the return stack value to be stored in RAM
			elsif (opcode = ops_PAUSE OR preempt = '1') then	-- virtual interrupt
				state_n <= virtual_interrupt;
			elsif opcode = ops_throw then
				state_n <= throw0;
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
			elsif opcode = ops_ifdup then 
				state_n <= ifdup;
			elsif opcode = ops_SDIVMOD then
				state_n <= sdivmod;		
			elsif opcode = ops_UDIVMOD then
				state_n <= udivmod;
			elsif opcode = ops_SMULT then
				state_n <= smult;
			elsif opcode = ops_UMULT then
				state_n <= umult;
			elsif branch = bps_RTS then											-- other RTS instructions									
				state_n <= skip1;	
--			elsif opcode >= 51 then 												-- comparator format found to be slightly slower and LUT costly than OR format
--				state_n <= skip1;	
			else
				state_n <= common;
			end if;
			timer <= 0;
			
			-- Memory write logic
			MEMdataout_X <= NOS_r;											-- 32bit SRAM						
			MEM_WRQ_X_i <= '0';

			if opcode = ops_BYTE or opcode = ops_CSTORE then						
				MEMsize_X_n <= "01";														
			elsif opcode = ops_WORD or opcode = ops_WSTORE then
				MEMsize_X_n <= "10";
			else
				MEMsize_X_n <= "11";
			end if;
															
			MEMaddr_i <= PC_addr;											
					
			-- Program counter logic					-- This logic is a timing constraint!  Maybe flatten into a less hierachical structure.
			if int_trig = '1' then
				PC_n <= int_vector_ext;												-- PC from external interrupt vector
			elsif retrap(0) = '1' then
				PC_n <= int_vector_TRAP;											-- PC from internal interrupt vector	
			elsif preempt = '1' then 
				if virtualInterrupt = 0 then
					PC_n <= PCthaw;
				else
					PC_n <= virtualInterrupt;
				end if;
			else
			   case branch is 
					when bps_BEQ =>
						if equalzero = '1' then
							PC_n <= PC_branch;
						else
							PC_n <= PC_plus;		
						end if;	
					when bps_BRA =>
						PC_n <= PC_branch;	
					when bps_RTS =>
						PC_n <= TORS(19 downto 0);									-- PC from Top Of Return Stack (also covers RTI and RETRAP, which include RTS by default)	
					when others =>
						case opcode is
							when ops_TRAP =>
								PC_n <= int_vector_TRAP;							-- PC from internal interrupt vector
							when ops_JSL =>
								PC_n <= PC_jsl;	
							when ops_JSR =>
								PC_n <= TOS(19 downto 0);		
							when ops_JMP =>
								PC_n <= TOS(19 downto 0);	
							when ops_CATCH =>
								PC_n <= TOS(19 downto 0);
							when ops_THROW =>
								PC_n <= PC;
							when ops_word =>
								PC_n <= PC_plus_two;
							when ops_long =>
								PC_n <= PC_plus_four;
							when ops_CFETCH =>
								PC_n <= PC;											-- PC update is done on the final cycle of multi-cycle instructions
							when ops_WFETCH =>
								PC_n <= PC;
							when ops_LFETCH =>
								PC_n <= PC;
							when ops_CSTORE =>
								PC_n <= PC;
							when ops_WSTORE =>
								PC_n <= PC;
							when ops_LSTORE =>
								PC_n <= PC;
							when ops_SDIVMOD =>
								PC_n <= PC;
							when ops_UDIVMOD =>
								PC_n <= PC;	
							when ops_SMULT =>
								PC_n <= PC;
							when ops_UMULT =>
								PC_n <= PC;		
							when ops_IFDUP =>
								PC_n <= PC;	
							when ops_toR =>												-- insert one cycle delay between >R or R> and RTS
								if MEMdatain_X(23 downto 22) = "01" then
									PC_n <= PC;
								else
									PC_n <= PC_plus;
								end if;
							when ops_Rfrom =>												-- insert one cycle delay between >R or R> and RTS
								if MEMdatain_X(23 downto 22) = "01" then
									PC_n <= PC;
								else
									PC_n <= PC_plus;
								end if;	
							when ops_PAUSE =>
								if SingleMulti = '1' then
									if virtualInterrupt = 0 then
										PC_n <= PCthaw;
									else
										PC_n <= virtualInterrupt;
									end if;
								else
									PC_n <= PC;
								end if;
							when others =>
								PC_n <= PC_plus;
							end case;
				end case;
			end if;
				
			-- Microcode logic
			if int_trig = '1' or retrap(0) = '1' then
				ucode <= ops_JSL;												-- interrupt microcode 
			elsif preempt = '1' then
				ucode <= ops_PAUSE;
			else
				case branch is
					when bps_BRA =>
						ucode <= ops_NOP;										-- avoid executing the high 6 bits of the branch offset as an opcode!
					when bps_BEQ =>
						ucode <= ops_DROP;									-- drop the flag
					when others =>
						case opcode is
							when ops_SDIVMOD =>					-- suppress microcode until last cycle of these instructions
								ucode <= ops_NOP;
							when ops_UDIVMOD =>	
								ucode <= ops_NOP;	
							when ops_THROW =>
								ucode <= ops_NOP;
							when ops_PAUSE =>
								if SingleMulti = '1' then
									ucode <= opcode;
								else
									ucode <= ops_NOP;
								end if;
						   when others =>
								ucode <= opcode;
						end case;
				end case;
			end if;

			-- Accumulator logic
			accumulator <= (others=>'0');
			
			-- Data multiplexer logic														
				AuxControl_n(1 downto 1) <= "0";					-- setting for SRAM fetch or load literal instructions										

			-- Return stack control logic 
			if (int_trig = '0') and (preempt = '0') and (opcode = ops_RETRAP or branch = bps_RTS) then -- override on interrupt or preemptive switch
				AuxControl_n(0 downto 0) <= "1";						-- decrement return stack pointer
			else
				AuxControl_n(0 downto 0) <= "0";
			end if;
			
			if int_trig = '1' or retrap(0) = '1' then											
				ReturnAddress_n <= "000000000000" & PC_m1;
			elsif opcode = ops_JSL then
				ReturnAddress_n <= "000000000000" & PC_plus_three;
			else
				ReturnAddress_n <= PC_addr;
			end if;
			
			-- Interrupt logic
			irq_n <= '0';
			if opcode = ops_RTI and branch = "01" then 
				rti <= '1';												
			else
				rti <= '0';
			end if;
			
			-- Trap logic
			if opcode = ops_RETRAP and branch = "00" then
				retrap_n <= "10";
			elsif opcode = ops_TRAP and branch = "00" then
				retrap_n <= "00";
			else
				retrap_n <= "0" & retrap(1);
			end if;
			
			-- Delayed RTS logic
--			if branch = bps_RTS and not (int_trig = '1' or retrap(0) = '1') then
--				delayed_RTS_n <= '1';
--			else
--				delayed_RTS_n <= '0';
--			end if;
			
			-- AXI logic
--			s_axi_arvalid <= '0';
--			s_axi_awvalid <= '0';
			s_axi_wdata <= NOS_r;
			s_axi_wstrb <= "1111";
--			s_axi_wvalid <= '0';
--			s_axi_rready <= '0';	
			
			-- virtualization
			if	int_trig = '0' and ((singleMulti = '1' and opcode = ops_PAUSE) or preempt = '1') then
				pause <= '1'; 	
			else
				pause <= '0';
			end if;
			
			if singleMulti = '0' then										
					preemp_counter_n <= (others => '0');
			else
					preemp_counter_n <= preemp_counter + 1;	
			end if;
			
			if preempt = '1' then 
				PCfreeze <= PC_m1;
			else
				PCfreeze <= PC;
			end if;
			
			-- debug
			debug_i <= x"00";
	
		when ifdup =>											-- ifdup
			state_n <= common;
			timer <= 0;
			PC_n <= PC_plus;
			if equalzero_r = '1' then								-- TOS was equal to zero
				ucode <= ops_DROP;									-- DROP previously DUP'd value if TOS was zero
			else
				ucode <= ops_NOP;
			end if;
			accumulator <= (others=>'0');
			MEMaddr_i <= PC_addr;	
			MEM_WRQ_X_i <= '0';
			MEMdataout_X <= NOS_r;
			MEMsize_X_n <= "11";
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "0";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			-- delayed_RTS_n <= delayed_RTS;
--			s_axi_awvalid <= '0';
			s_axi_wdata <= NOS_r;
			s_axi_wstrb <= "1111";
--			s_axi_wvalid <= '0';
--			s_axi_arvalid <= '0';
--			s_axi_rready <= '0';	
			pause <= '0';
			debug_i <= x"01";
			preemp_counter_n <= preemp_counter;
			PCfreeze <= PC;
			
		when smult =>										-- signed multiply
			state_n <= common;
			timer <= 4;  											-- wait for multiplier
			PC_n <= PC_plus;										-- PC update will take place only on transition to next state	
			ucode <= ops_SMULT;							
			accumulator <= (others=>'0');	
			MEMaddr_i <= PC_addr;	
			MEM_WRQ_X_i <= '0';
			MEMdataout_X <= NOS_r;
			MEMsize_X_n <= "11";
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "0";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			-- delayed_RTS_n <= delayed_RTS;
--			s_axi_awvalid <= '0';
			s_axi_wdata <= NOS_r;
			s_axi_wstrb <= "1111";
--			s_axi_wvalid <= '0';
--			s_axi_arvalid <= '0';
--			s_axi_rready <= '0';	
			pause <= '0';
			debug_i <= x"02";
			preemp_counter_n <= preemp_counter;
			PCfreeze <= PC;
	
		when umult =>										-- unsigned multiply
			state_n <= common;
			timer <= 4;  											-- wait for multiplier
			PC_n <= PC_plus;										-- PC update will take place only on transition to next state
			ucode <= ops_UMULT;									
			accumulator <= (others=>'0');	
			MEMaddr_i <= PC_addr;	
			MEM_WRQ_X_i <= '0';
			MEMdataout_X <= NOS_r;
			MEMsize_X_n <= "11";
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "0";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			-- delayed_RTS_n <= delayed_RTS;
--			s_axi_awvalid <= '0';
			s_axi_wdata <= NOS_r;
			s_axi_wstrb <= "1111";
--			s_axi_wvalid <= '0';
--			s_axi_arvalid <= '0';
--			s_axi_rready <= '0';	
			pause <= '0';
			debug_i <= x"03";
			preemp_counter_n <= preemp_counter;
			PCfreeze <= PC;
	
		when sdivmod =>										-- signed division
			state_n <= sdivmod_load;
			timer <= 42;  											-- wait for divider
			PC_n <= PC;	
			ucode <= ops_NOP;
			accumulator <= (others=>'0');	
			MEMaddr_i <= PC_addr;	
			MEM_WRQ_X_i <= '0';
			MEMdataout_X <= NOS_r;
			MEMsize_X_n <= "11";
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "0";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			-- delayed_RTS_n <= delayed_RTS;
--			s_axi_awvalid <= '0';
			s_axi_wdata <= NOS_r;
			s_axi_wstrb <= "1111";
--			s_axi_wvalid <= '0';
--			s_axi_arvalid <= '0';
--			s_axi_rready <= '0';	
			pause <= '0';
			debug_i <= x"04";
			preemp_counter_n <= preemp_counter;
			PCfreeze <= PC;

		when udivmod =>										-- unsigned division
			state_n <= udivmod_load;
			timer <= 41;											-- wait for divider
			PC_n <= PC;
			ucode <= ops_NOP;
			accumulator <= (others=>'0');	
			MEMaddr_i <= PC_addr;	
			MEM_WRQ_X_i <= '0';
			MEMdataout_X <= NOS_r;
			MEMsize_X_n <= "11";
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "0";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			-- delayed_RTS_n <= delayed_RTS;
--			s_axi_awvalid <= '0';
			s_axi_wdata <= NOS_r;
			s_axi_wstrb <= "1111";
--			s_axi_wvalid <= '0';
--			s_axi_arvalid <= '0';
--			s_axi_rready <= '0';	
			pause <= '0';
			debug_i <= x"05";
			preemp_counter_n <= preemp_counter;
			PCfreeze <= PC;
			
		when sdivmod_load =>									-- signed division
			state_n <= common;									
			timer <= 0;  
			PC_n <= PC_plus;
			ucode <= ops_SDIVMOD; 								-- load TOS and NOS with results
			accumulator <= (others=>'0');
			MEMaddr_i <= PC_addr;	
			MEM_WRQ_X_i <= '0';
			MEMdataout_X <= NOS_r;
			MEMsize_X_n <= "11";
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "0";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			-- delayed_RTS_n <= delayed_RTS;
--			s_axi_awvalid <= '0';
			s_axi_wdata <= NOS_r;
			s_axi_wstrb <= "1111";
--			s_axi_wvalid <= '0';
--			s_axi_arvalid <= '0';
--			s_axi_rready <= '0';	
			pause <= '0';
			debug_i <= x"06";
			preemp_counter_n <= preemp_counter;
			PCfreeze <= PC;
	
		when udivmod_load =>									-- unsigned division
			state_n <= common;
			timer <= 0;
			PC_n <= PC_plus;
			ucode <= ops_UDIVMOD;								-- load TOS and NOS with results
			accumulator <= (others=>'0');
			MEMaddr_i <= PC_addr;	
			MEM_WRQ_X_i <= '0';
			MEMdataout_X <= NOS_r;
			MEMsize_X_n <= "11";
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "0";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			-- delayed_RTS_n <= delayed_RTS;
--			s_axi_awvalid <= '0';
			s_axi_wdata <= NOS_r;
			s_axi_wstrb <= "1111";
--			s_axi_wvalid <= '0';
--			s_axi_arvalid <= '0';
			pause <= '0';
			debug_i <= x"07";
			preemp_counter_n <= preemp_counter;
			PCfreeze <= PC;
			
		when Sfetch_byte =>											
			state_n <= skip1;																	
			ucode <= ops_REPLACE;											
			timer <= 0;
			PC_n <= PC;
			MEMaddr_i <= TOS_r;										-- address now available in registered TOS					
			MEM_WRQ_X_i <= '0';
			MEMdataout_X <= NOS_r;
			MEMsize_X_n <= "01";
			accumulator <= (others=>'0');			
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "0";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			-- delayed_RTS_n <= delayed_RTS;
--			s_axi_awvalid <= '0';
			s_axi_wdata <= NOS_r;
			s_axi_wstrb <= "1111";
--			s_axi_wvalid <= '0';
--			s_axi_arvalid <= '0';
--			s_axi_rready <= '0';	
			pause <= '0';
			debug_i <= x"08";
			preemp_counter_n <= preemp_counter;
			PCfreeze <= PC;
			
		when Sfetch_word =>											
			state_n <= skip1;																	
			ucode <= ops_REPLACE;											
			timer <= 0;
			PC_n <= PC;
			MEMaddr_i <= TOS_r;										-- address now available in registered TOS					
			MEM_WRQ_X_i <= '0';
			MEMdataout_X <= NOS_r;
			MEMsize_X_n <= "10";
			accumulator <= (others=>'0');			
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "0";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			-- delayed_RTS_n <= delayed_RTS;
--			s_axi_awvalid <= '0';
			s_axi_wdata <= NOS_r;
			s_axi_wstrb <= "1111";
--			s_axi_wvalid <= '0';
--			s_axi_arvalid <= '0';
--			s_axi_rready <= '0';	
			pause <= '0';
			debug_i <= x"09";
			preemp_counter_n <= preemp_counter;
			PCfreeze <= PC;

		when Sfetch_long =>											
			state_n <= skip1;																	
			ucode <= ops_REPLACE;									
			timer <= 0;
			PC_n <= PC;
			MEMaddr_i <= TOS_r;										-- address now available in registered TOS					
			MEM_WRQ_X_i <= '0';
			MEMdataout_X <= NOS_r;
			MEMsize_X_n <= "11";
			accumulator <= (others=>'0');			
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "0";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			-- delayed_RTS_n <= delayed_RTS;
--			s_axi_awvalid <= '0';
			s_axi_wdata <= NOS_r;
			s_axi_wstrb <= "1111";
--			s_axi_wvalid <= '0';
--			s_axi_arvalid <= '0';
--			s_axi_rready <= '0';	
			pause <= '0';
			debug_i <= x"0a";
			preemp_counter_n <= preemp_counter;
			PCfreeze <= PC;
			
		when Sstore_long =>											
			state_n <= SRAM_store;																	
			ucode <= ops_drop;											
			timer <= 0;
			PC_n <= PC;
			MEMaddr_i <= TOS_r;										-- address now available in registered TOS					
			MEM_WRQ_X_i <= '1';
			MEMdataout_X <= NOS_r;
			MEMsize_X_n <= "11";										-- long								
			accumulator <= (others=>'0');			
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "0";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			-- delayed_RTS_n <= delayed_RTS;
--			s_axi_awvalid <= '0';
			s_axi_wdata <= NOS_r;
			s_axi_wstrb <= "1111";
--			s_axi_wvalid <= '0';
--			s_axi_arvalid <= '0';
--			s_axi_rready <= '0';	
			pause <= '0';
			debug_i <= x"0b";
			preemp_counter_n <= preemp_counter;
			PCfreeze <= PC;
			
		when Sstore_word =>											
			state_n <= SRAM_store;																	
			ucode <= ops_drop;											
			timer <= 0;
			PC_n <= PC;
			MEMaddr_i <= TOS_r;										-- address now available in registered TOS					
			MEM_WRQ_X_i <= '1';
			MEMdataout_X <= NOS_r;
			MEMsize_X_n <= "10";										-- word							
			accumulator <= (others=>'0');			
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "0";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			-- delayed_RTS_n <= delayed_RTS;
--			s_axi_awvalid <= '0';
			s_axi_wdata <= NOS_r;
			s_axi_wstrb <= "1111";
--			s_axi_wvalid <= '0';
--			s_axi_arvalid <= '0';
--			s_axi_rready <= '0';	
			pause <= '0';
			debug_i <= x"0c";
			preemp_counter_n <= preemp_counter;
			PCfreeze <= PC;
			
		when Sstore_byte =>											
			state_n <= SRAM_store;																	
			ucode <= ops_drop;											
			timer <= 0;
			PC_n <= PC;
			MEMaddr_i <= TOS_r;										-- address now available in registered TOS					
			MEM_WRQ_X_i <= '1';
			MEMdataout_X <= NOS_r;
			MEMsize_X_n <= "01";										-- byte								
			accumulator <= (others=>'0');			
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "0";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			-- delayed_RTS_n <= delayed_RTS;
--			s_axi_awvalid <= '0';
			s_axi_wdata <= NOS_r;
			s_axi_wstrb <= "1111";
--			s_axi_wvalid <= '0';
--			s_axi_arvalid <= '0';
--			s_axi_rready <= '0';	
			pause <= '0';
			debug_i <= x"0d";
			preemp_counter_n <= preemp_counter;
			PCfreeze <= PC;
			
		when SRAM_store =>											
			state_n <= common;		-- skip1																	
			ucode <= ops_drop;											
			timer <= 0;
			PC_n <= PC_plus;			-- PC
			MEMaddr_i <= PC_addr;	-- TOS_r				
			MEM_WRQ_X_i <= '0';
			MEMdataout_X <= NOS_r;
			MEMsize_X_n <= "11";
			accumulator <= (others=>'0');			
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "0";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			-- delayed_RTS_n <= delayed_RTS;
--			s_axi_awvalid <= '0';
			s_axi_wdata <= NOS_r;
			s_axi_wstrb <= "1111";
--			s_axi_wvalid <= '0';
--			s_axi_arvalid <= '0';
--			s_axi_rready <= '0';	
			pause <= '0';
			debug_i <= x"0e";
			preemp_counter_n <= preemp_counter;
			PCfreeze <= PC;
	
		when Dfetch_long =>											
			if s_axi_ar_state = done then
				state_n <= Dfetch_long2;									
			else
				state_n <= Dfetch_long;	
			end if;
			ucode <= ops_NOP;
			accumulator <= s_axi_rdata(7 downto 0) & s_axi_rdata(15 downto 8) &	s_axi_rdata(23 downto 16) & s_axi_rdata(31 downto 24);  -- big endian <-> AXI conversion	
			timer <= 0;
			PC_n <= PC;					
			MEMaddr_i <= TOS_r;	
			MEM_WRQ_X_i <= '0';
			MEMdataout_X <= NOS_r;		
			MEMsize_X_n <= "11";
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "1";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			-- delayed_RTS_n <= delayed_RTS;
--			s_axi_awvalid <= '0';
			s_axi_wdata <= NOS_r;
			s_axi_wstrb <= "1111";
--			s_axi_wvalid <= '0';
--			s_axi_rready <= '0';	
--			s_axi_arvalid <= '1';
			pause <= '0';
			debug_i <= x"0f";
			preemp_counter_n <= preemp_counter;
			PCfreeze <= PC;

		when Dfetch_long2 =>	
			if s_axi_rvalid_r = '1' then
				state_n <= skip1;		
				ucode <= ops_NOP;
			else
				state_n <= Dfetch_long2;	
				ucode <= ops_REPLACE;	
			end if;	
			accumulator <= s_axi_rdata(7 downto 0) & s_axi_rdata(15 downto 8) &	s_axi_rdata(23 downto 16) & s_axi_rdata(31 downto 24);  -- big endian <-> AXI conversion	
			timer <= 0;
			PC_n <= PC;				
			MEMaddr_i <= TOS_r;				
			MEM_WRQ_X_i <= '0';
			MEMdataout_X <= NOS_r;
			MEMsize_X_n <= "11";
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "1";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			-- delayed_RTS_n <= delayed_RTS;
--			s_axi_awvalid <= '0';
			s_axi_wdata <= NOS_r;
			s_axi_wstrb <= "1111";
--			s_axi_wvalid <= '0';
--			s_axi_arvalid <= '0';
--			s_axi_rready <= '1';	
			pause <= '0';
			debug_i <= x"10";
			preemp_counter_n <= preemp_counter;
			PCfreeze <= PC;
	
		when Dfetch_word =>											
			if s_axi_ar_state = done then
				state_n <= Dfetch_word2;									
			else
				state_n <= Dfetch_word;	
			end if;
			ucode <= ops_NOP;
			if AXIADDR(1) = '0' then
				accumulator <= "0000000000000000" & s_axi_rdata(7 downto 0) & s_axi_rdata(15 downto 8) ;  -- big endian <-> AXI conversion	
			else
				accumulator <= "0000000000000000" & s_axi_rdata(23 downto 16) & s_axi_rdata(31 downto 24) ;
			end if;
			timer <= 0;
			PC_n <= PC;					
			MEMaddr_i <= TOS_r;	
			MEM_WRQ_X_i <= '0';
			MEMdataout_X <= NOS_r;		
			MEMsize_X_n <= "11";
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "1";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			-- delayed_RTS_n <= delayed_RTS;
--			s_axi_awvalid <= '0';
			s_axi_wdata <= NOS_r;
			s_axi_wstrb <= "1111";
--			s_axi_wvalid <= '0';
--			s_axi_rready <= '0';	
--			s_axi_arvalid <= '1';
			pause <= '0';
			debug_i <= x"11";
			preemp_counter_n <= preemp_counter;
			PCfreeze <= PC;

		when Dfetch_word2 =>	
			if s_axi_rvalid_r = '1' then
				state_n <= skip1;		
				ucode <= ops_NOP;	
			else
				state_n <= Dfetch_word2;	
				ucode <= ops_REPLACE;	
			end if;					
			if AXIADDR(1) = '0' then
				accumulator <= "0000000000000000" & s_axi_rdata(7 downto 0) & s_axi_rdata(15 downto 8) ;  -- big endian <-> AXI conversion	
			else
				accumulator <= "0000000000000000" & s_axi_rdata(23 downto 16) & s_axi_rdata(31 downto 24) ;
			end if;
			timer <= 0;
			PC_n <= PC;				
			MEMaddr_i <= TOS_r;				
			MEM_WRQ_X_i <= '0';
			MEMdataout_X <= NOS_r;
			MEMsize_X_n <= "11";
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "1";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			-- delayed_RTS_n <= delayed_RTS;
--			s_axi_awvalid <= '0';
			s_axi_wdata <= NOS_r;
			s_axi_wstrb <= "1111";
--			s_axi_wvalid <= '0';
--			s_axi_arvalid <= '0';
--			s_axi_rready <= '1';	
			pause <= '0';
			debug_i <= x"12";
			preemp_counter_n <= preemp_counter;
			PCfreeze <= PC;
				
		when Dfetch_byte =>											
			if s_axi_ar_state = done then
				state_n <= Dfetch_byte2;									
			else
				state_n <= Dfetch_byte;	
			end if;
			ucode <= ops_NOP;
			if AXIADDR = "00" then
				accumulator <= "000000000000000000000000" & s_axi_rdata(7 downto 0);			  -- big endian <-> AXI conversion	
			elsif AXIADDR = "01" then
				accumulator <= "000000000000000000000000" & s_axi_rdata(15 downto 8);
			elsif AXIADDR = "10" then
				accumulator <= "000000000000000000000000" & s_axi_rdata(23 downto 16);
			else 
				accumulator <= "000000000000000000000000" & s_axi_rdata(31 downto 24);			
			end if;
			timer <= 0;
			PC_n <= PC;					
			MEMaddr_i <= TOS_r;	
			MEM_WRQ_X_i <= '0';
			MEMdataout_X <= NOS_r;		
			MEMsize_X_n <= "11";
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "1";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			-- delayed_RTS_n <= delayed_RTS;
--			s_axi_awvalid <= '0';
			s_axi_wdata <= NOS_r;
			s_axi_wstrb <= "1111";
--			s_axi_wvalid <= '0';
--			s_axi_rready <= '0';	
--			s_axi_arvalid <= '1';
			pause <= '0';
			debug_i <= x"13";
			preemp_counter_n <= preemp_counter;
			PCfreeze <= PC;

		when Dfetch_byte2 =>	
			if s_axi_rvalid_r = '1' then
				state_n <= skip1;		
				ucode <= ops_NOP;	
			else
				state_n <= Dfetch_byte2;
				ucode <= ops_REPLACE;								
			end if;		
			if AXIADDR = "00" then
				accumulator <= "000000000000000000000000" & s_axi_rdata(7 downto 0);			  -- big endian <-> AXI conversion	
			elsif AXIADDR = "01" then
				accumulator <= "000000000000000000000000" & s_axi_rdata(15 downto 8);
			elsif AXIADDR = "10" then
				accumulator <= "000000000000000000000000" & s_axi_rdata(23 downto 16);
			else 
				accumulator <= "000000000000000000000000" & s_axi_rdata(31 downto 24);			
			end if;
			timer <= 0;
			PC_n <= PC;				
			MEMaddr_i <= TOS_r;				
			MEM_WRQ_X_i <= '0';
			MEMdataout_X <= NOS_r;
			MEMsize_X_n <= "11";
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "1";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			-- delayed_RTS_n <= delayed_RTS;
--			s_axi_awvalid <= '0';
			s_axi_wdata <= NOS_r;
			s_axi_wstrb <= "1111";
--			s_axi_wvalid <= '0';
--			s_axi_arvalid <= '0';
--			s_axi_rready <= '1';	
			pause <= '0';
			debug_i <= x"14";			
			preemp_counter_n <= preemp_counter;
			PCfreeze <= PC;
				
		when Dstore_long =>										
			if s_axi_aw_state = done and s_axi_w_state = done  then
				state_n <= Dstore2;		
				ucode <= ops_DROP;									
			else
				state_n <= Dstore_long;
				ucode <= ops_NOP;	
			end if;
			timer <= 0;
			PC_n <= PC;	
			MEMaddr_i <= TOS_r;			
			MEM_WRQ_X_i <= '0';
			MEMdataout_X <= NOS_r;
			MEMsize_X_n <= "11";
			accumulator <= (others=>'0');			
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "0";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			-- delayed_RTS_n <= delayed_RTS;
--			s_axi_awvalid <= '1';
			s_axi_wdata <= NOS_r(7 downto 0) & NOS_r(15 downto 8) & NOS_r(23 downto 16) & NOS_r(31 downto 24);  -- big endian <-> AXI conversion
			s_axi_wstrb <= "1111";
--			s_axi_wvalid <= '1';
--			s_axi_arvalid <= '0';
--			s_axi_rready <= '0';	
			pause <= '0';
			debug_i <= x"15";		
			preemp_counter_n <= preemp_counter;	
			PCfreeze <= PC;			
			
		when Dstore_word =>										
			if s_axi_aw_state = done and s_axi_w_state = done  then
				state_n <= Dstore2;		
				ucode <= ops_DROP;									
			else
				state_n <= Dstore_word;
				ucode <= ops_NOP;	
			end if;
			if TOS(1) = '0' then
				s_axi_wstrb <= "0011";
			else
				s_axi_wstrb <= "1100";
			end if;
			timer <= 0;
			PC_n <= PC;	
			MEMaddr_i <= TOS_r;			
			MEM_WRQ_X_i <= '0';
			MEMdataout_X <= NOS_r;
			MEMsize_X_n <= "11";	
			accumulator <= (others=>'0');			
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "0";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			-- delayed_RTS_n <= delayed_RTS;
			s_axi_wdata <= NOS_r(7 downto 0) & NOS_r(15 downto 8) & NOS_r(7 downto 0) & NOS_r(15 downto 8);
--			s_axi_awvalid <= '1';
--			s_axi_wvalid <= '1';
--			s_axi_arvalid <= '0';
--			s_axi_rready <= '0';	
			pause <= '0';
			debug_i <= x"16";	
			preemp_counter_n <= preemp_counter;
			PCfreeze <= PC;

		when Dstore_byte =>										
			if s_axi_aw_state = done and s_axi_w_state = done  then
				state_n <= Dstore2;		
				ucode <= ops_DROP;									
			else
				state_n <= Dstore_byte;
				ucode <= ops_NOP;	
			end if;
			if TOS(1 downto 0) = "00" then
				s_axi_wstrb <= "0001";
			elsif TOS(1 downto 0) = "01" then
				s_axi_wstrb <= "0010";
			elsif TOS(1 downto 0) = "10" then
				s_axi_wstrb <= "0100";	
			else
				s_axi_wstrb <= "1000";				
			end if;
			timer <= 0;
			PC_n <= PC;	
			MEMaddr_i <= TOS_r;			
			MEM_WRQ_X_i <= '0';
			MEMdataout_X <= NOS_r;
			MEMsize_X_n <= "11";	
			accumulator <= (others=>'0');			
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "0";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			-- delayed_RTS_n <= delayed_RTS;
			s_axi_wdata <= NOS_r(7 downto 0) & NOS_r(7 downto 0) & NOS_r(7 downto 0) & NOS_r(7 downto 0);
--			s_axi_awvalid <= '1';
--			s_axi_wvalid <= '1';
--			s_axi_arvalid <= '0';
--			s_axi_rready <= '0';	
			pause <= '0';
			debug_i <= x"17";	
			preemp_counter_n <= preemp_counter;
			PCfreeze <= PC;

		when Dstore2 =>										
			state_n <= skip1;
			ucode <= ops_DROP;									-- drop address				
			timer <= 0;
			PC_n <= PC;					
			MEMaddr_i <= PC_addr;				
			MEM_WRQ_X_i <= '0';
			MEMdataout_X <= NOS_r;
			MEMsize_X_n <= "11";	
			accumulator <= (others=>'0');
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "0";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
			-- delayed_RTS_n <= delayed_RTS;
--			s_axi_awvalid <= '0';
			s_axi_wdata <= NOS_r;
			s_axi_wstrb <= "1111";
--			s_axi_wvalid <= '0';
--			s_axi_arvalid <= '0';
--			s_axi_rready <= '0';	
			pause <= '0';
			debug_i <= x"18";	
			preemp_counter_n <= preemp_counter;
			PCfreeze <= PC;
			
		when throw0 =>
			state_n <= throw;							
			timer <= 0;
			if equalzero_r = '0' then							
				PC_n <= ExceptionAddress(19 downto 0);
				ucode <= ops_THROW;
			else
				PC_n <= PC;
				ucode <= ops_drop;
			end if;																			
			accumulator <= (others=>'0');
			MEMaddr_i <= PC_addr;				
			MEM_WRQ_X_i <= '0';
			MEMsize_X_n <= "11";
			MEMdataout_X <= NOS_r;	
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "0";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
--			s_axi_awvalid <= '0';
			s_axi_wdata <= NOS_r;
			s_axi_wstrb <= "1111";
--			s_axi_wvalid <= '0';
--			s_axi_arvalid <= '0';
--			s_axi_rready <= '0';	
			pause <= '0';
			debug_i <= x"21";
			preemp_counter_n <= preemp_counter;
			PCfreeze <= PC;
			
		when throw =>									
			state_n <= common;							-- after changing PC, this wait state allows a memory read before execution of next instruction
			timer <= 0;
			PC_n <= PC_plus;
			if equalzero_r = '0' then						-- throw on non-zero parameter
				ucode <= ops_THROW2;		
				rti <= '1';									-- THROW from interrupt will also cancel interrupt state
			else
				ucode <= ops_NOP;							-- no second cycle action on zero parameter
				rti <= '0';
			end if;
			accumulator <= (others=>'0');
			MEMaddr_i <= PC_addr;				
			MEM_WRQ_X_i <= '0';
			MEMsize_X_n <= "11";
			MEMdataout_X <= NOS_r;	
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "0";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			retrap_n <= retrap;
			-- delayed_RTS_n <= delayed_RTS;
--			s_axi_awvalid <= '0';
			s_axi_wdata <= NOS_r;
			s_axi_wstrb <= "1111";
--			s_axi_wvalid <= '0';
--			s_axi_arvalid <= '0';
--			s_axi_rready <= '0';	
			pause <= '0';
			debug_i <= x"19";
			preemp_counter_n <= preemp_counter;
			PCfreeze <= PC;
			
		when virtual_interrupt =>	
			state_n <= skip1;
			timer <= 0;
			PC_n <= PC;
			if SingleMulti = '1' then
				ucode <= ops_JSL;
			else
				ucode <= ops_NOP;
			end if;
			ucode <= ops_JSL;								-- push the saved PC address onto the return stack		
			accumulator <= (others=>'0');
			MEMaddr_i <= PC_addr;				
			MEM_WRQ_X_i <= '0';
			MEMsize_X_n <= "11";
			MEMdataout_X <= NOS_r;	
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "0";
			ReturnAddress_n <= "000000000000" & PCthaw_m1;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
--			s_axi_awvalid <= '0';
			s_axi_wdata <= NOS_r;
			s_axi_wstrb <= "1111";
--			s_axi_wvalid <= '0';
--			s_axi_arvalid <= '0';
--			s_axi_rready <= '0';	
			pause <= '0';
			debug_i <= x"20";
			preemp_counter_n <= preemp_counter;
			PCfreeze <= PC;
			
		when others =>										-- skip1, but use default case for speed optimization
			state_n <= common;							-- after changing PC, this wait state allows a memory read before execution of next instruction
			timer <= 0;
			PC_n <= PC_plus;
			ucode <= ops_NOP;																			
			accumulator <= (others=>'0');
			MEMaddr_i <= PC_addr;				
			MEM_WRQ_X_i <= '0';
			MEMsize_X_n <= "11";
			MEMdataout_X <= NOS_r;	
			AuxControl_n(0 downto 0) <= "0";
			AuxControl_n(1 downto 1) <= "0";
			ReturnAddress_n <= PC_addr;
			irq_n <= int_trig;
			rti <= '0';
			retrap_n <= retrap;
--			s_axi_awvalid <= '0';
			s_axi_wdata <= NOS_r;
			s_axi_wstrb <= "1111";
--			s_axi_wvalid <= '0';
--			s_axi_arvalid <= '0';
--			s_axi_rready <= '0';	
			pause <= '0';
			debug_i <= x"21";
			preemp_counter_n <= preemp_counter;
			PCfreeze <= PC;

		end case;
	end process;
	
	-- AXI4 write channel handshake control
process (state, s_axi_ar_state, s_axi_arready, s_axi_aw_state, s_axi_awready, s_axi_w_state, s_axi_wready)
begin
	
	case s_axi_ar_state is
	when ready =>
		if state = Dfetch_long or state = Dfetch_word or state = Dfetch_byte then
			s_axi_ar_state_n <= pending;
		else 
			s_axi_ar_state_n <= ready;
		end if;
	when pending =>
		if s_axi_arready = '1' then
			s_axi_ar_state_n <= done;
		else
			s_axi_ar_state_n <= pending;
		end if;		
	when done =>
		if state = common then
			s_axi_ar_state_n <= ready;
		else 
			s_axi_ar_state_n <= done;
		end if;		
	end case;
	
	case s_axi_aw_state is
	when ready =>
		if state = Dstore_long or state = Dstore_word or state = Dstore_byte then
			s_axi_aw_state_n <= pending;
		else 
			s_axi_aw_state_n <= ready;
		end if;
	when pending =>
		if s_axi_awready = '1' then
			s_axi_aw_state_n <= done;
		else
			s_axi_aw_state_n <= pending;
		end if;		
	when done =>
		if state = common then
			s_axi_aw_state_n <= ready;
		else 
			s_axi_aw_state_n <= done;
		end if;		
	end case;
	
	case s_axi_w_state is
	when ready =>
		if state = Dstore_long or state = Dstore_word or state = Dstore_byte then
			s_axi_w_state_n <= pending;
		else 
			s_axi_w_state_n <= ready;
		end if;
	when pending =>
		if s_axi_wready = '1' then
			s_axi_w_state_n <= done;
		else
			s_axi_w_state_n <= pending;
		end if;		
	when done =>
		if state = common then
			s_axi_w_state_n <= ready;
		else 
			s_axi_w_state_n <= done;
		end if;		
	end case;
end process;

with s_axi_aw_state select s_axi_awvalid <= '1' when pending, '0' when others;
with s_axi_w_state select s_axi_wvalid <= '1' when pending, '0' when others;
with s_axi_ar_state select s_axi_arvalid <= '1' when pending, '0' when others;

end RTL;

--Copyright and license
--=====================
--
--The N.I.G.E machine, its design and its source code are Copyright (C) 2015 by Andrew Richard Read and dual licensed.
--    
--(1) For commercial or proprietary use you must obtain a commercial license agreement with Andrew Richard Read (anding_eunding@yahoo.com)
--    
--(2) You can redistribute the N.I.G.E. Machine, its design and its source code and/or modify it under the terms of the GNU General Public License 
--as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. 
--
--The N.I.G.E Machine is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
--or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public 
--License along with this repository.  If not, see <http://www.gnu.org/licenses/>.