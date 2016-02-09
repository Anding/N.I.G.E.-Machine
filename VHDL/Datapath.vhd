-- Datapath
-- Andrew Read
-- Created 1 May 2011

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity Datapath is
	 Generic (	vmp_w : integer;
					psp_w : integer;
					rsp_w : integer;
					ssp_w : integer;
					esp_w : integer
					);
    Port ( rst : in  STD_LOGIC;	 										-- reset
           clk : in  STD_LOGIC;	 										-- clock
			  MEMdatain_X : in STD_LOGIC_VECTOR (31 downto 0);	
			  Accumulator : in STD_LOGIC_VECTOR (31 downto 0);		-- Immediate value read from memory by control unit for writing to TOS
			  MicroControl : in  STD_LOGIC_VECTOR (22 downto 0);	-- control lines
			  AuxControl : in STD_LOGIC_VECTOR (1 downto 0);		-- control lines 
			  ReturnAddress : in STD_LOGIC_VECTOR (31 downto 0);	-- Return Address for JSR, BSR instructions
			  TOS : out STD_LOGIC_VECTOR (31 downto 0);				-- Top Of Stack (TOS_n, one cycle ahead of registered value)
			  TOS_r : out STD_LOGIC_VECTOR (31 downto 0);			-- Top Of Stack (the registered value)			  
			  NOS_r : out STD_LOGIC_VECTOR (31 downto 0);			-- Next On Stack (the registered value)
			  equalzero : out STD_LOGIC;									-- flag '1' when TOS (unregistered) is zero
			  equalzero_r : out STD_LOGIC;								-- flag '1' when TOS (registered) is zero			  
			  chip_RAM : out STD_LOGIC;									-- flag used to identify SRAM vs. PSDRAM memory access
			  TORS : out STD_LOGIC_VECTOR (31 downto 0);			   -- Top Of Return Stack
			  ExceptionAddress : OUT STD_LOGIC_VECTOR (31 downto 0);
			  PSaddr : out STD_LOGIC_VECTOR (vmp_w + psp_w -1 downto 0);			-- Paramater stack memory
			  PSdatain : in STD_LOGIC_VECTOR (31 downto 0);	
			  PSdataout : out STD_LOGIC_VECTOR (31 downto 0);
			  PSw : out STD_LOGIC_VECTOR (0 downto 0);
			  RSaddr : out STD_LOGIC_VECTOR (vmp_w + rsp_w -1 downto 0);			-- Return stack memory
			  RSdatain : in STD_LOGIC_VECTOR (31 downto 0);	
			  RSdataout : out STD_LOGIC_VECTOR (31 downto 0);
			  RSw : out STD_LOGIC_VECTOR (0 downto 0);
			  SSaddr : out STD_LOGIC_VECTOR (vmp_w + ssp_w -1 downto 0);			-- Subroutine stack memory
			  SSdatain : in STD_LOGIC_VECTOR (351 downto 320);	
			  SSdataout : out STD_LOGIC_VECTOR (351 downto 320);
			  SSw : out STD_LOGIC_VECTOR (43 downto 40);
			  ESaddr : out STD_LOGIC_VECTOR (vmp_w + esp_w -1 downto 0);			-- Exception stack memory
			  ESdatain : in STD_LOGIC_VECTOR (303 downto 256);	
			  ESdataout : out STD_LOGIC_VECTOR (303 downto 256);
			  ESw : out STD_LOGIC_VECTOR (37 downto 32);
			  DatapathFreeze : out  STD_LOGIC_VECTOR (95 downto 0);					-- virtualization unit
			  DatapathThaw : in  STD_LOGIC_VECTOR (95 downto 0);
			  VM : in STD_LOGIC_VECTOR (vmp_w -1 downto 0)
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

-- ISE

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

---- VIDADO
--component signed_divider
--  PORT (
--  aclk : IN STD_LOGIC;
--  s_axis_divisor_tvalid : IN STD_LOGIC;
--  s_axis_divisor_tready : OUT STD_LOGIC;
--  s_axis_divisor_tdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
--  s_axis_dividend_tvalid : IN STD_LOGIC;
--  s_axis_dividend_tready : OUT STD_LOGIC;
--  s_axis_dividend_tdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
--  m_axis_dout_tvalid : OUT STD_LOGIC;
--  m_axis_dout_tdata : OUT STD_LOGIC_VECTOR(63 DOWNTO 0)
--);
--end component;
--
--component unsigned_divider
--  PORT (
--  aclk : IN STD_LOGIC;
--  s_axis_divisor_tvalid : IN STD_LOGIC;
--  s_axis_divisor_tready : OUT STD_LOGIC;
--  s_axis_divisor_tdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
--  s_axis_dividend_tvalid : IN STD_LOGIC;
--  s_axis_dividend_tready : OUT STD_LOGIC;
--  s_axis_dividend_tdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
--  m_axis_dout_tvalid : OUT STD_LOGIC;
--  m_axis_dout_tdata : OUT STD_LOGIC_VECTOR(63 DOWNTO 0)
--);
--end component;

constant dont_care : std_logic_vector(31 downto 0) := "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX";
constant blank : std_logic_vector(8 downto 0) := (others=>'0');
signal adder_out, compare_out, genmux_out, logic_out : std_logic_vector(31 downto 0);
signal unsigned_product, signed_product : std_logic_vector(63 downto 0);
signal signed_quotient, signed_remainder, unsigned_quotient, unsigned_remainder : std_logic_vector(31 downto 0);
signal TOS_i, NOS_i, TOS_n, NOS_alu, NOS_n : std_logic_vector (31 downto 0);
signal PwBuff : std_logic_vector(31 downto 0);
signal PSP, PSP_n, PSP_m1, PSP_p1 : std_logic_vector (psp_w -1 downto 0);
signal RSP, RSP_n, RSP_n1, RSP_m1, RSP_p1 : std_logic_vector (rsp_w -1 downto 0);
signal SSP, SSP_n, SSP_m1, SSP_p1 : std_logic_vector (ssp_w -1 downto 0);
signal ESP, ESP_n, ESP_m1, ESP_p1 : std_logic_vector (esp_w -1 downto 0);
signal PSdataout_i, PSdatain_i : std_logic_vector (31 downto 0);
signal PSw_i, PSw_m1 : std_logic_vector (0 downto 0);
signal data : std_logic_vector (31 downto 0);
signal equalzero_i, equalzero_n : std_logic;
signal m_axis_quotient_signed, m_axis_quotient_unsigned : std_logic_vector (63 downto 0);

begin

	process 
	begin												-- sequential logic for registered values
		wait until rising_edge(clk);	
		if rst = '0' then
			TOS_i <= TOS_n;						-- TOS is held in a register
			NOS_i <= NOS_n;						-- NOS is held in a register	
			PSP <= PSP_n;
			RSP <= RSP_n1;
			ESP <= ESP_n;
			SSP <= SSP_n;
			equalzero_i <= equalzero_n;
		else
			TOS_i <= (others=>'0');
			NOS_i <= (others=>'0');
			PSP <= (others=>'0');
			RSP <= (others=>'0');
			ESP <= (others=>'0');
			SSP <= (others=>'0');			
			equalzero_i <= '0';
		end if;
	end process;
	
	DatapathFreeze <= Blank(7 downto esp_w) & ESP_n & Blank(7 downto ssp_w) & SSP_n & Blank(7 downto rsp_w) & RSP_n1 & Blank(7 downto psp_w) & PSP_n & NOS_n & TOS_n;
	
	-- Return stack
		--	Rstack_RAM must be configured as write first!
	
	RSP_m1 <= RSP - 1;							-- available for incrementing and decrementing stack pointers
	RSP_p1 <= RSP + 1;	
	RSaddr <= VM(vmp_w -1 downto 0) & RSP_n1;					-- return stack address  (use the post-auxiliary override value for RSP so that RTS is processed same cycle)
	TORS <= "000000000" & SSdatain(342 downto 320);				-- TORS is directly read from BLOCK RAM
	ExceptionAddress <= "000000000" & ESdatain(294 downto 272);
	
	-- Subroutine stack
	
	SSP_m1 <= SSP - 1;							-- available for incrementing and decrementing stack pointers
	SSP_p1 <= SSP + 1;	
	SSaddr <= VM(vmp_w -1 downto 0) & SSP_n;								
	
	-- Exception stack
	
	ESP_m1 <= ESP - 1;							-- available for incrementing and decrementing stack pointers
	ESP_p1 <= ESP + 1;	
	ESaddr <= VM(vmp_w -1 downto 0) & ESP_n;	
								
	-- Return stack 				
	process (AuxControl, MicroControl, RSP_m1, RSP_p1, TOS_i, SSdatain, DatapathThaw, RSP)	
	begin
		if AuxControl (0 downto 0) = "1" then				-- instruction RTS requires reset of return stack pointer
			RSP_n1 <= SSdatain(343 + rsp_w -1 downto 343);
		else
			case MicroControl(15 downto 13) is				-- multiplexer for setting return stack pointer
				when "001" =>
					RSP_n1 <= RSP_m1;
				when "010" =>
					RSP_n1 <= RSP_p1;
				when "011" =>
					RSP_n1 <= SSdatain(343 + rsp_w -1 downto 343);
				when "100" =>
					RSP_n1 <= SSdatain(343 + rsp_w -1 downto 343) + 1;
				when "101" =>
					RSP_n1 <= (others=>'0');	
				when "110" =>
					RSp_n1 <= datapathThaw(72 + rsp_w -1 downto 72);
				when others =>
					RSP_n1 <= RSP;
				end case;
		end if;
	end process;
					 
	with MicroControl(16 downto 16) select				-- multiplexer for selecting value to write to TORS
		RSdataout <= ReturnAddress when "1",
						 TOS_i 	when others;	
					 
	with MicroControl(15 downto 13) select				-- write enable on return stack memory follows increment of return stack pointer
		RSw	 <= "1" when "010",
					 "0" when others;		

	-- Subroutine stack
	process (AuxControl, MicroControl, SSP_m1, SSP_p1, SSdatain, RSP_n1, RSP_m1, ESdatain, SSP, DatapathThaw)	
	begin
		if AuxControl (0 downto 0) = "1" then				-- instruction RTS requires decrement subroutine stack pointer
			SSP_n <= SSP_m1;
		elsif (RSP_n1 = SSdatain(343 + rsp_w -1 downto 343)) and (MicroControl(15 downto 13) = "001") and (SSP /= 0) then  -- THIS IS TRIGGERING INSTEAD OF A DATAPATH THAW
			SSP_n <= SSP_m1;										-- pop of return stack below the baseline requires decrement subroutine stack pointer
		else
			case MicroControl(19 downto 17) is				-- multiplexer for setting subroutine stack pointer
				when "001" =>
					SSP_n <= SSP_m1;
				when "010" =>
					SSP_n <= SSP_p1;
				when "011" =>
					SSP_n <= ESdatain(295 + ssp_w -1 downto 295);
				when "100" =>
					SSP_n <= (others=>'0');
				when "101" =>
					SSP_n <= datapathThaw(80 + ssp_w -1 downto 80);
				when others =>
					SSP_n <= SSP;
				end case;
		end if;
	end process;

	with MicroControl(19 downto 17) select				-- write enable on subroutine stack memory follows increment of subroutine stack pointer
		SSw	 <= "1111" when "010",
					 "0000" when others;		
	
	SSdataout <= Blank(9 - rsp_w - 1 downto 0) & RSP & ReturnAddress(22 downto 0);

	-- Exception stack
	process (AuxControl, MicroControl, ESP_m1, ESP_p1, ESdatain, SSP_n, SSP_m1, ESP, DatapathThaw)	
	begin
		if SSP_n = ESdatain(295 + ssp_w -1 downto 295) and (MicroControl(19 downto 17) = "001")  and (ESP /= 0) then   
			ESP_n <= ESP_m1;										-- pop of subroutine stack below the baseline requires decrement exception stack pointer
		else
			case MicroControl(22 downto 20) is				-- multiplexer for setting subroutine stack pointer
				when "001" =>
					ESP_n <= ESP_m1;
				when "010" =>
					ESP_n <= ESP_p1;
				when "011" =>
					ESP_n <= (others=>'0');
				when "100" =>
					ESP_n <= datapathThaw(88 + esp_w -1 downto 88);
				when others =>
					ESP_n <= ESP;
				end case;
		end if;
	end process;

	with MicroControl(22 downto 20) select				-- write enable on subroutine stack memory follows increment of subroutine stack pointer
		ESw	 <= "111111" when "010",
					 "000000" when others;		
	
	ESdataout <=  Blank(9 - ssp_w -1 downto 0) & SSP & (ReturnAddress(22 downto 0) + 1) & "0000000" & Blank(9 - psp_w -1 downto 0) & PSP ;
	
	-- Parameter stack
			--	Pstack_RAM must be configured as write first!

	PSP_m1 <= PSP - 1;							-- available for incrementing and decrementing stack pointers
	PSP_p1 <= PSP + 1;
	PSaddr <= VM(vmp_w -1 downto 0) & PSP_n; 	-- parameter stack address	
	PSdatain_i <= 	PSdatain;
	PSdataout_i <= NOS_i;						-- for pushing NOS into memory	
	PSdataout <= PSdataout_i;	
	PSw <= PSw_i;	
	TOS <= TOS_n;									-- output TOS to control unit, once cycle ahead of registered value	
	TOS_r <= TOS_i;								-- the registered value of TOS
--	NOS <= NOS_n;									-- output NOS to control unit, once cycle ahead of registered value	
	NOS_r <= NOS_i;
	
			
	with AuxControl (1 downto 1) select					-- immediate value for loading into TOS (one cycle delay to coincide with microcode)
		DATA <= 	MEMdatain_X when "0",						-- SRAM fetch or load literal
					accumulator when others;					-- PSDRAM control unit mediated fetch via accumulator
		
	with MicroControl(12 downto 10) select				-- multiplexer for parameter stack pointer
		PSP_n <= PSP_m1 when "001",
					PSP_p1 when "010",
					TOS_i(psp_w -1 downto 0) when "011",
					ESdatain(256 + psp_w -1 downto 256) when "100",
					Blank(psp_w -1 downto 0) when "101",
					datapathThaw(64 + psp_w -1 downto 64) when "110",
					PSP when others;
	
	PSw_i <= "1" when MicroControl(12 downto 10) = "010" or MicroControl = "00000000000000010100001"	-- microcode for a ROT instruction
					else "0";										-- write enable on paramater stack memory either when incrementing stack pointer or on a ROT instruction
																	
	with MicroControl(9 downto 7) select				-- multiplexer for NOS register 
		NOS_n <= TOS_i 	when "001",
					PSdataIN_i when "010",
					NOS_alu when "011",
					datapathThaw(63 downto 32) when "100",
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
	
	equalzero_n <= '1' when TOS_n = 0 else '0'; 
	equalzero <= equalzero_n;
	equalzero_r <= equalzero_i; 	
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

	Inst_GenMux: entity work.GenMux 										-- general multiplexer
	GENERIC MAP(
		psp_w => psp_w,
		rsp_w => rsp_w
		)
	PORT MAP(
	TOS => TOS_i,
	NOS => NOS_i,
	PSdata => PSdataIN_i,
	RSdata => RSdatain,
	PSP => PSP,
	datapathThaw => datapathThaw(31 downto 0),
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
		
-- ISE
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
		
---- Vivado 	
--	inst_signed_divider : signed_divider
--		port map (
--			aclk => clk,
--			s_axis_dividend_tdata => NOS_i,
--			s_axis_dividend_tvalid => '1',
--			s_axis_dividend_tready => open,
--			s_axis_divisor_tdata => TOS_i,
--			s_axis_divisor_tvalid => '1',
--			s_axis_divisor_tready => open,
--			m_axis_dout_tvalid => open,
--			m_axis_dout_tdata => m_axis_quotient_signed
--			);	
--			
--	signed_quotient <= m_axis_quotient_signed(63 downto 32);
--	signed_remainder <= m_axis_quotient_signed(31 downto 0);
--		
--		inst_unsigned_divider : unsigned_divider
--		port map (
--			aclk => clk,
--			s_axis_dividend_tdata => NOS_i,
--			s_axis_dividend_tvalid => '1',
--			s_axis_dividend_tready => open,
--			s_axis_divisor_tdata => TOS_i,
--			s_axis_divisor_tvalid => '1',
--			s_axis_divisor_tready => open,
--			m_axis_dout_tvalid => open,
--			m_axis_dout_tdata => m_axis_quotient_unsigned
--			);	
--
--	unsigned_quotient <= m_axis_quotient_unsigned(63 downto 32);
--	unsigned_remainder <= m_axis_quotient_unsigned(31 downto 0);

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