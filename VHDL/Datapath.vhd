-- Datapath
-- Andrew Read
-- Created 1 May 2011

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity Datapath is
    Port ( rst : in  STD_LOGIC;	 										-- reset
           clk : in  STD_LOGIC;	 										-- clock
			  MEMdatain_X : in STD_LOGIC_VECTOR (31 downto 0);	
			  Accumulator : in STD_LOGIC_VECTOR (31 downto 0);		-- Immediate value read from memory by control unit for writing to TOS
			  MicroControl : in  STD_LOGIC_VECTOR (13 downto 0);	-- control lines
			  AuxControl : in STD_LOGIC_VECTOR (1 downto 0);		-- control lines 
			  ReturnAddress : in STD_LOGIC_VECTOR (31 downto 0);	-- Return Address for JSR, BSR instructions
			  TOS : out STD_LOGIC_VECTOR (31 downto 0);				-- Top Of Stack (TOS_n, one cycle ahead of registered value)
			  TOS_r : out STD_LOGIC_VECTOR (31 downto 0);			-- Top Of Stack (Tthe registered value)			  
			  NOS : out STD_LOGIC_VECTOR (31 downto 0);				-- Next On Stack (NOS_n)
			  equalzero : out STD_LOGIC;									-- flag '1' when TOS is zero
			  chip_RAM : out STD_LOGIC;									-- flag used to identify SRAM vs. PSDRAM memory access
			  TORS : out STD_LOGIC_VECTOR (31 downto 0);			   -- Top Of Return Stack
			  PSaddr : out STD_LOGIC_VECTOR (8 downto 0);			-- Paramater stack memory
			  PSdatain : in STD_LOGIC_VECTOR (31 downto 0);	
			  PSdataout : out STD_LOGIC_VECTOR (31 downto 0);
			  PSw : out STD_LOGIC_VECTOR (0 downto 0);
			  RSaddr : out STD_LOGIC_VECTOR (8 downto 0);			-- Return stack memory
			  RSdatain : in STD_LOGIC_VECTOR (31 downto 0);	
			  RSdataout : out STD_LOGIC_VECTOR (31 downto 0);
			  RSw : out STD_LOGIC_VECTOR (0 downto 0)
			  );
end Datapath;

architecture RTL of Datapath is

COMPONENT Adder
PORT(
	rst : IN std_logic;
	clk : IN std_logic;
	PortA : IN std_logic_vector(31 downto 0);
	PortB : IN std_logic_vector(31 downto 0);
	ControlA : IN std_logic_vector(2 downto 0);
	ControlB : IN std_logic_vector(2 downto 0);          
	Output : OUT std_logic_vector(31 downto 0)
	);
END COMPONENT;

COMPONENT Comparator
PORT(
	PortA : IN std_logic_vector(31 downto 0);
	PortB : IN std_logic_vector(31 downto 0);
	Control : IN std_logic_vector(3 downto 0);          
	Output : OUT std_logic_vector(31 downto 0)
	);
END COMPONENT;

COMPONENT GenMux																-- general multiplexer
PORT(
	TOS : IN std_logic_vector(31 downto 0);
	NOS : IN std_logic_vector(31 downto 0);
	PSdata : IN std_logic_vector(31 downto 0);
	RSdata : IN std_logic_vector(31 downto 0);
	PSP : IN std_logic_vector(8 downto 0);
	RSP : IN std_logic_vector(8 downto 0);
	Data : IN std_logic_vector(31 downto 0);
	Control : IN std_logic_vector(2 downto 0);          
	Output : OUT std_logic_vector(31 downto 0)
	);
END COMPONENT;

COMPONENT Logic
PORT(
	PortA : IN std_logic_vector(31 downto 0);
	PortB : IN std_logic_vector(31 downto 0);
	Control : IN std_logic_vector(2 downto 0);          
	Output : OUT std_logic_vector(31 downto 0)
	);
END COMPONENT;


component signed_mult
	port (
	clk : IN STD_LOGIC;
	a: IN std_logic_VECTOR(31 downto 0);
	b: IN std_logic_VECTOR(31 downto 0);
	p: OUT std_logic_VECTOR(63 downto 0));
end component;

component unsigned_mult
	port (
	clk : IN STD_LOGIC;
	a: IN std_logic_VECTOR(31 downto 0);
	b: IN std_logic_VECTOR(31 downto 0);
	p: OUT std_logic_VECTOR(63 downto 0));
end component;

component signed_divider
	port (
	clk: IN std_logic;
	rfd: OUT std_logic;
	dividend: IN std_logic_VECTOR(31 downto 0);
	divisor: IN std_logic_VECTOR(31 downto 0);
	quotient: OUT std_logic_VECTOR(31 downto 0);
	fractional: OUT std_logic_VECTOR(31 downto 0));
end component;

component unsigned_divider
	port (
	clk: IN std_logic;
	rfd: OUT std_logic;
	dividend: IN std_logic_VECTOR(31 downto 0);
	divisor: IN std_logic_VECTOR(31 downto 0);
	quotient: OUT std_logic_VECTOR(31 downto 0);
	fractional: OUT std_logic_VECTOR(31 downto 0));
end component;

constant dont_care : std_logic_vector(31 downto 0) := "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX";
signal adder_out, compare_out, genmux_out, logic_out : std_logic_vector(31 downto 0);
signal unsigned_product, signed_product : std_logic_vector(63 downto 0);
signal signed_quotient, signed_remainder, unsigned_quotient, unsigned_remainder : std_logic_vector(31 downto 0);
signal TOS_i, NOS_i, TOS_n, NOS_alu, NOS_n : std_logic_vector (31 downto 0);
--signal TORS_i, TORS_n, TORS_n1, TORS_j : std_logic_vector (31 downto 0);
signal PwBuff : std_logic_vector(31 downto 0);
signal PSP, RSP, PSP_n, RSP_n, RSP_n1, PSP_m1, PSP_p1, RSP_m1, RSP_p1 : std_logic_vector (8 downto 0);
signal PSdataout_i, PSdatain_i : std_logic_vector (31 downto 0);
signal PSw_i, PSw_m1 : std_logic_vector (0 downto 0);
signal data : std_logic_vector (31 downto 0);

begin

	process 
	begin												-- sequential logic for registered values
		wait until rising_edge(clk);	
		if rst = '0' then
			TOS_i <= TOS_n;						-- TOS is held in a register
			NOS_i <= NOS_n;						-- NOS is held in a register
			PSP <= PSP_n;
			RSP <= RSP_n1;
			PwBuff <= PSdataOUT_i;				-- buffer for last written paramter stack value
			PSw_m1 <= PSw_i;
		else
			TOS_i <= (others=>'0');
			NOS_i <= (others=>'0');
			PSP <= (others=>'0');
			RSP <= (others=>'0');	
			PwBuff <= (others=>'0');	
			PSw_m1 <= (others=>'0');
		end if;
	end process;
	
	PSP_m1 <= PSP - 1;							-- available for incrementing and decrementing stack pointers
	PSP_p1 <= PSP + 1;
	PSaddr <= PSP_n; 								-- parameter stack address	
	PSdataout_i <= NOS_i;						-- for pushing NOS into memory	
	PSdataout <= PSdataout_i;	
	PSw <= PSw_i;	
	TOS <= TOS_n;									-- output TOS to control unit, once cycle ahead of registered value	IS THIS ESSENTIAL?	
	TOS_r <= TOS_i;								-- the registered value of TOS
	NOS <= NOS_n;									-- output NOS to control unit, once cycle ahead of regsitered value	

	-- Return stack
		--	Rstack_RAM must be configured as write first!
	
	RSP_m1 <= RSP - 1;							-- available for incrementing and decrementing stack pointers
	RSP_p1 <= RSP + 1;	
	RSaddr <= RSP_n1;								-- return stack address  (use the post-auxiliary override value for RSP so that RTS is processed same cycle)
	TORS <= RSdatain;								-- TORS is directly read from BLOCK RAM
	
	with AuxControl (0 downto 0) select					-- instruction RTS requires decrement of return stack pointer
		RSP_n1 <= RSP_m1 when "1",
					 RSP_n when others;	
					 
	with MicroControl(12 downto 11) select				-- multiplexer for setting return stack pointer
		RSP_n <= RSP_m1 when "01",
					RSP_p1 when "10",
					TOS_i(8 downto 0) when "11",
					RSP when others;	
					 
	with MicroControl(13 downto 13) select				-- multiplexer for selecting value to write to TORS
		RSdataout <= ReturnAddress when "1",
						 TOS_i 	when others;	
					 
	with MicroControl(12 downto 11) select				-- write enable on return stack memory follows increment of return stack pointer
		RSw	 <= "1" when "10",
					 "0" when others;		

	-- Parameter stack
	
	PSdatain_i <= PwBuff when PSw_m1 = "1"	-- because of 1 cycle memory latency, need to use the buffered value for a stack memory read
														--   if the stack memory was written just one cycle before (as memory update will not yet have occured)			
				else PSdatain;		
			
	with AuxControl (1 downto 1) select					-- immediate value for loading into TOS (one cycle delay to coincide with microcode)
		DATA <= 	MEMdatain_X when "0",						-- SRAM fetch or load literal
					accumulator when others;					-- PSDRAM control unit mediated fetch via accumulator
		
	with MicroControl(10 downto 9) select				-- multiplexer for parameter stack pointer
		PSP_n <= PSP_m1 when "01",
					PSP_p1 when "10",
					TOS_i(8 downto 0) when "11",
					PSP when others;
	
	PSw_i <= "1" when MicroControl(10 downto 9) = "10" or MicroControl = "00000010100001"	-- microcode for a ROT instruction
					else "0";										-- write enable on paramater stack memory either when incrementing stack pointer or on a ROT instruction
																	
	with MicroControl(8 downto 7) select				-- multiplexer for NOS register 
		NOS_n <= TOS_i 	when "01",
					PSdataIN_i when "10",
					NOS_alu when "11",
					NOS_i 	when others;	
					
	with MicroControl(6 downto 4) select				-- multiplexer for TOS register
		TOS_n <= logic_out when "001",
					genmux_out when "010",
					compare_out when "011",
					signed_product(63 downto 32) 	when "100",
					unsigned_product(63 downto 32) when "101",
					signed_quotient when "110",
					unsigned_quotient when "111",
					adder_out when others;
							
	with MicroControl(6 downto 4) select				-- multiplexer for ALU output directed at NOS
		NOS_alu <= signed_product(31 downto 0) when "100",
					unsigned_product(31 downto 0) when "101",
					signed_remainder when "110",
					unsigned_remainder when "111",
					dont_care when others;
	
	-- signals for control unit
	
	equalzero <= '1' when TOS_n = 0 else '0'; 
	chip_RAM <= '1' when TOS_n(23 downto 18) = 0 else '0';		-- flag used to identify SRAM vs. PSDRAM memory access

	Inst_Adder: Adder 
	PORT MAP(
	rst => rst,
	clk => clk,
	PortA => NOS_i,
	PortB => TOS_i,
	ControlA => MicroControl(2 downto 0),
	ControlB => MicroControl(6 downto 4),
	Output => adder_out);

	Inst_Comparator: Comparator 
	PORT MAP(
	PortA => NOS_i,
	PortB => TOS_i,
	Control => MicroControl(3 downto 0),
	Output => compare_out);

	Inst_GenMux: GenMux 										-- general multiplexer
	PORT MAP(
	TOS => TOS_i,
	NOS => NOS_i,
	PSdata => PSdataIN_i,
	RSdata => RSdatain,
	PSP => PSP,
	RSP => RSP,
	Data => Data,
	Control => MicroControl(2 downto 0),
	Output => genmux_out);

	Inst_Logic: Logic 
	PORT MAP(
	PortA => NOS_i,
	PortB => TOS_i,
	Control => MicroControl(2 downto 0),
	Output => logic_out);

	inst_signed_mult : signed_mult
	port map (
		clk => clk,
		a => NOS_i,
		b => TOS_i,
		p => signed_product);
		
	inst_unsigned_mult : unsigned_mult
	port map (
		clk => clk,
		a => NOS_i,
		b => TOS_i,
		p => unsigned_product);	

	inst_signed_divider : signed_divider
	port map (
		clk => clk,
		rfd => open,
		dividend => NOS_i,
		divisor => TOS_i,
		quotient => signed_quotient,
		fractional => signed_remainder);	

	inst_unsigned_divider : unsigned_divider
	port map (
		clk => clk,
		rfd => open,
		dividend => NOS_i,
		divisor => TOS_i,
		quotient => unsigned_quotient,
		fractional => unsigned_remainder);	

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