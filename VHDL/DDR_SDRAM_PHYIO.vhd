-- DDR2 memory interface
-- Andrew Read, March 2016
-- This project is based on a working DDR2 interface very kindly donated by a friend

LIBRARY IEEE;
USE IEEE.std_logic_1164.all;
USE IEEE.std_logic_arith.all;
USE IEEE.std_logic_textio.all;

entity DDR_SDRAM_PHYIO is 
port (
	CLK   : in  std_logic;   							-- 125MHz clock (this is the "system clock")
	CLK_130 : in std_logic;								-- 125MHz clock 130 degree phase shift (special clock for certain DDR2 SDRAM signals)
    reset : in  std_logic;								-- active low reset
     
	-- user interface
	wrrd_ba_add : in std_logic_vector(2 downto 0);		-- bank address
	wrrd_ras_add : in std_logic_vector(12 downto 0);	-- row address
	wrrd_cas_add : in std_logic_vector(8 downto 0);		-- column address
	wr_we : in std_logic_vector(3 downto 0);			-- set high to request write
	wr_dat : in std_logic_vector(31 downto 0);			-- write data
	wr_ack : out std_logic;								-- hold write request until wr_ack goes high
	rd_re : in std_logic;								-- set high to request read
	rd_dat : out std_logic_vector(63 downto 0);			-- read data
	rd_ack : out std_logic;   							-- hold read request until rd_ack goes high
	rd_valid : out std_logic;							-- accept read rd_dat during the same cycle that rd_valid is high
	refresh : in std_logic;								-- cycle refresh high to allow periodic SDRAM refresh (driven by SDRAM_CTRL)
	ref_ack	: out std_logic;							-- hold refresh request until ref_ack goes high

	-- DDR2 SDRAM interface (MT47H64M16HR-25E)
	SDRAM_A : out std_logic_vector(13 downto 0);		-- address inputs: should be (12 downto 0), no A[13] in 16x 
	SDRAM_BA : out std_logic_vector(2 downto 0);		-- bank address
	SDRAM_CKE      : out std_logic;						-- clock enable
	SDRAM_CK       : out std_logic;						-- positive clock (differential pair)
	SDRAM_nCK	   : out std_logic;						-- negative clock (differential pair)
	SDRAM_DQ       : inout std_logic_vector(15 downto 0);  	-- bidirectional data input / output  
	SDRAM_DQS	   : inout std_logic_vector(1 downto 0);	-- bidirectional data strobe (input not currently used)
	--SDRAM_nDQS	   : out std_logic_vector(1 downto 0);	-- differential DQS not currently used
	SDRAM_DM       : out std_logic_vector(1 downto 0);		-- data mask for write data
	SDRAM_nCAS     : out std_logic;						-- CAS# command input
	SDRAM_nCS      : out std_logic;						-- chip select
	SDRAM_nRAS     : out std_logic;						-- RAS# command input
	SDRAM_nWE      : out std_logic);					-- WE#  command input

end DDR_SDRAM_PHYIO;

-- This is a DD2 SDRAM interface developed on the Diglient Nexys4-DDR board.  It interfaces with the Micron MT47H64M16HR-25E 
-- The module is driven by two clocks due to the need to provide a strobe (DQS_out) at a nominal 90 degree phase off set from write data (DQ)
-- In this design the clock that drives the DQS strobe is actually at a 130 degree phase offset to the write data (DQ).  A 130 degree phase offset
-- is within the specification (see tDQSS on the datasheet 1Gb_DDR2 p34), but compared to a 90 degree phase offset the extra phase shift
-- allows a sufficent DQS write preamble (see tWPRE) without needing to toggle the clock enable port on DQS's ODDR buffer
-- This in turn saves the need for what would be a very time constrained cross clock domain signal between the FSM and ODDR/CE
--
-- Clocking summary
-- CLK drives the most of the logic internal to this memory controller, including the user interface and the finite state machine
-- CLK also drives SDRAM_DQ, and all of SDRAM_CAS#, etc. on the SDRAM
-- CLK_130 drives SDRAM_CK/n and SDRAM_DQS

-- A summary of MT47H64M16HR-25E addressing
-- Banks	 			    8 
-- Rows	 			    8,192 
-- Columns			    1,024 
-- Addresses	   67,108,864 
-- Word size	 	       16 
-- Bits	 		1,073,741,824 
-- Gbits			  1.00000

-- Known limitations
-- 1. DQS in not used in READ cycles
-- DD2 specifications require that DQ_in is clocked into registers at the transitions of DQS_in, after DQS_in has been delayed 90 degree
-- (relative to the clock period).  However to delay the actual DQS_in signals is not straightforward and so a workaround is currently being used.
-- DQ_in is being clocked in using an IDDR buffer driven by "not CLK_130" which has been found through trial and error to be a good enough
-- approximation to DQS_in to work in simulation and in hardware on the Nexys4-DDR at the tested clock frequencies
-- If higher clock frequencies are to be attempted then I would suggest to generate a third clock and experimenting with various phase offsets to drive DQ_in's IDDR
-- naturally any solution arrived at with this method will only be applicable to a particular board and clock frequency!

-- 2. The write postamble (WPST, p34) is (technically) violated although the Micron DDR model does not complain
-- The use of a 130 degree phase offset as opposed to a 90 degree phase offset clock to drive DQS allows a difficult cross-clock domain signal to be avoided
-- but the compromises is that the extra 40 degrees applied to the DQS write preamble is "stolen" from the write postamble (see tWPST, p36).  The Micron DDR2 model
-- is very sensitive to the correct DQS write preamble but does not raise any error or warning about this violation of the write postamble

-- 3. On die termination (ODT) is not used

architecture Struct of DDR_SDRAM_PHYIO is      

component OBUFDS is
-- OBUFDS creates a differential output from a single-ended input
-- see ug471 p. 45
  generic(
      CAPACITANCE : string     := "DONT_CARE";
      IOSTANDARD  : string     := "DEFAULT";	-- DIFF_HSTL_II_18 is the default for differential I/O's
      SLEW        : string     := "SLOW"
    );

  port(
    O  : out std_ulogic;		-- positive output same as input
    OB : out std_ulogic;		-- negative output opposite to input
    I : in std_ulogic			-- input
    );
end component;

component ODDR
-- ODDR (dual data rate output buffer) creates an 1-bit wide DDR output from a 2-bit wide clock-synchronous input without manual multiplexing
-- see ug471 p. 127
  generic(
	DDR_CLK_EDGE : string := "OPPOSITE_EDGE";  	-- in OPPOSITE_EDGE mode D2 is captured on a falling clock edge and
												-- 	presented at output on the next rising clock edge
												-- in SAME_EDGE mode D2 is captured on a rising clock edge and 
												-- 	presented at output one full clock cycle later on the rising edge
												-- in both cases D1 is captured on a rising clock edge and presented 
												-- 	at output on the next falling clock edge 
												
	INIT         : bit    := '0';				-- initial value of Q
	SRTYPE       : string := "SYNC"				-- set/reset with respect to clock
      );
  port(
      Q           : out std_ulogic;				-- output
      C           : in  std_ulogic;				-- clock input port
      CE          : in  std_ulogic;				-- clock enable port (low disables the output port on Q)
      D1          : in  std_ulogic;				-- inputs
      D2          : in  std_ulogic;
      R           : in  std_ulogic := 'L';		-- synchronous reset (ignore - not used)
      S           : in  std_ulogic := 'L'		-- synchronous set (ignore - not used)
    );
end component; 

component IDDR
-- IDDR (dual data rate input buffer) creates a 2-bit wide clock-synchronous output from a 1-bit wide DDR input without manual multiplexing
-- ug471 p110
  generic(
      DDR_CLK_EDGE : string := "OPPOSITE_EDGE";
      INIT_Q1      : bit    := '0';
      INIT_Q2      : bit    := '0';
      IS_C_INVERTED : bit := '0';
      IS_D_INVERTED : bit := '0';
      SRTYPE       : string := "SYNC"		-- set/reset with respect to clock
      );
  port(
      Q1          : out std_ulogic;			-- outputs
      Q2          : out std_ulogic;
      C           : in  std_ulogic;			-- clock input port
      CE          : in  std_ulogic;			-- clock enable port
      D           : in  std_ulogic;			-- input
      R           : in  std_ulogic := 'L';	-- synchronous reset (ignore - not used)
      S           : in  std_ulogic := 'L'	-- synchronous set (ignore - not used)
    );
end component;

type fsm_type is (init, 
			init_precharge, init_precharge_done,
			init_mode_2, init_mode_2_done,
			init_mode_3, init_mode_3_done,
			init_mode_1, init_mode_1_done,
			init_mode_0, init_mode_0_done, 
			init_precharge_0, init_precharge_0_done,    
			init_refresh_0, init_refresh_0_done,
			init_refresh_1, init_refresh_1_done,
			init_mode_0_2, init_mode_0_2_done, 
			init_mode_1_2, init_mode_1_2_done,
			init_mode_1_3, init_mode_1_3_done,   
			write_0, write_1, write_2, write_3, write_4, write_5,
			idle, 
			bank_0, bank_done,  
			active,
			precharge_0, precharge_done,
			read_0, read_1, read_2, read_3, read_3b, read_4, read_5, read_done,
			refresh_0, refresh_1);
			
-- DDR2 SRAM commands (1Gb_DDD2.pdf, p70) 						values of CKE CS# RAS# CAS# WE#
constant CMD_LOAD_MODE			: std_logic_vector(4 downto 0) := "10000";
constant CMD_REFRESH			: std_logic_vector(4 downto 0) := "10001";
constant CMD_ENTER_SELF_REFRESH : std_logic_vector(4 downto 0) := "00001";
constant CMD_EXIT_SELF_REFRESH	: std_logic_vector(4 downto 0) := "10111";
constant CMD_PRECHARGE			: std_logic_vector(4 downto 0) := "10010";
constant CMD_ACTIVATE			: std_logic_vector(4 downto 0) := "10011";
constant CMD_WRITE 				: std_logic_vector(4 downto 0) := "10100";  -- also WRITE with auto precharge
constant CMD_READ				: std_logic_vector(4 downto 0) := "10101";	-- also READ with auto precharge
constant CMD_NOP				: std_logic_vector(4 downto 0) := "10111";	-- also EXIT_POWER_DOWN
constant CMD_DESELECT			: std_logic_vector(4 downto 0) := "11111";
constant CMD_ENTER_POWER_DOWN	: std_logic_vector(4 downto 0) := "00111";

-- Refresh parameter, see documentation in SDRAM_CTRL
constant refreshCount			: integer range 0 to 8191 := 7;			-- number of REFRESH commands issued during each refresh phase, minus 1

-- Timing parameters (ref. 1GB_DDR2 datasheet page numbers)
-- ct_int			power up and stabilize clock, p87: ct > 400ns
-- ct_precharge		tRP, precharge period, p36: (ct + 1) > 12.5ns
-- ct_refresh		tRFC, refresh interval, p37: (ct + 1) > 127.5ns
-- ct_RCD			tRCD, ROW to COLUMN delay, p36: (ct + 1) > 12.5ns
-- ct_writerec		tWR, write recovery, p37: p (ct + 1) > 15ns
-- reg_CAS, ct_CAS	tCAS, CAS latency, pp32, 77:  Allowed values of CAS are 3, 4, 5, 6, 7

-- Timing parameters at 100MHz (based on a 9us clock period including allowance)
constant ct_init				: integer range 0 to 1023 := 45;	
constant ct_precharge			: integer range 0 to 1023 := 1;
constant ct_refresh				: integer range 0 to 1023 := 14;
constant ct_RCD					: integer range 0 to 1023 := 1;
constant ct_WR					: integer range 0 to 1023 := 1;
constant reg_CAS				: std_logic_vector(2 downto 0) := "011" ;
constant ct_CAS					: integer range 0 to 1023 := 0;  -- ct_CAS must be set to reg_CAS - 3

-- Timing parameters at 125MHz (based on a 7.2us clock period including allowance)
--constant ct_init				: integer range 0 to 1023 := 56;	
--constant ct_precharge			: integer range 0 to 1023 := 2;
--constant ct_refresh				: integer range 0 to 1023 := 17;
--constant ct_RCD					: integer range 0 to 1023 := 1;
--constant ct_WR					: integer range 0 to 1023 := 2;
--constant reg_CAS				: std_logic_vector(2 downto 0) := "011" ;
--constant ct_CAS					: integer range 0 to 1023 := 0;  -- ct_CAS must be set to reg_CAS - 3

-- Timing parameters at 200MHz (based on a 5us clock period)
--constant ct_init				: integer range 0 to 1023 := 80;	
--constant ct_precharge			: integer range 0 to 1023 := 2;
--constant ct_refresh				: integer range 0 to 1023 := 25;
--constant ct_RCD					: integer range 0 to 1023 := 2;
--constant ct_WR					: integer range 0 to 1023 := 3;
--constant reg_CAS				: std_logic_vector(2 downto 0) := "011" ;
--constant ct_CAS					: integer range 0 to 1023 := 0;  -- ct_CAS must be set to reg_CAS - 3


signal SDRAM_dq_out_tmp : std_logic_vector(15 downto 0);
signal SDRAM_dq_out : std_logic_vector(31 downto 0);
signal SDRAM_dq_in_tmp : std_logic_vector(15 downto 0);
signal SDRAM_dq_in, SDRAM_dq_in_reg : std_logic_vector(31 downto 0);
signal SDRAM_dqs_out_tmp : std_logic_vector(1 downto 0);
signal state : fsm_type; 
signal counter : integer range 0 to 1023;
signal dq_write, dq_write_reg : std_logic;
signal dqs_write, dqs_write_reg : std_logic; 
signal dm_write : std_logic_vector(3 downto 0);
signal bank_row_active : std_logic_vector(13 downto 0);
signal bank_active : std_logic_vector(2 downto 0);
signal clk_int_xor_delay : std_logic;
signal clk_int_rise : std_logic := '0';   
signal clk_int_fall : std_logic := '0';
signal clk_int_xor : std_logic;
signal command : std_logic_vector(4 downto 0);
signal rd_dat_r : std_logic_vector(63 downto 0);
signal dqs_out_ce : std_logic;	
signal SDRAM_DQS_reg : std_logic_vector(1 downto 0);
signal wr_dat_64 : std_logic_vector(63 downto 0);
signal wr_we_8 : std_logic_vector(7 downto 0);
signal counterRefresh : integer range 0 to 8191;  

begin

-----------------------------------------------------
--	PHY: SDRAM commands
-----------------------------------------------------

SDRAM_CKE	<= COMMAND(4);
SDRAM_nCS	<= COMMAND(3);
SDRAM_nRAS	<= COMMAND(2);
SDRAM_nCAS	<= COMMAND(1);
SDRAM_nWE	<= COMMAND(0);

-----------------------------------------------------
--	PHY: SDRAM_CLK
-----------------------------------------------------
     
OBUFDSi : OBUFDS 
  port map (
    O  => SDRAM_CK,
    OB => SDRAM_nCK,
    I => clk_130							
    );
-- SDRAM_CK/n is created with a 90 degree phase offset so that its transitions are at the midpoint of the stable
-- values of the control lines CAS#, RAS#, etc.

-----------------------------------------------------
--	PHY: SDRAM_DQ
-----------------------------------------------------
							  
SDRAM_dq_in_tmp <= SDRAM_DQ after 1 ps; -- reflect board timing "after 1 ps" is ignored by synthesis

dq_iddr : for i in 0 to 15 generate
dq_iddrn : IDDR
  generic map(
      DDR_CLK_EDGE => "SAME_EDGE_PIPELINED",
      INIT_Q1      => '0',
      INIT_Q2      => '0',
      IS_C_INVERTED => '0',
      IS_D_INVERTED => '0',
      SRTYPE       => "SYNC"
      )
  port map(
      Q1          => SDRAM_dq_in(i),			-- bits 15 downto 0 are taken on one clock edge and
      Q2          => SDRAM_dq_in(i + 16),		-- bits 32 downto 16 are taken on the opposite clock edge (see DDR_CLK_EDGE setting and timing diagrams in ug471 p110
      C           => not CLK_130,				-- a workaround, see limitation above: should actually be clocked by DQS_in after delaying that by 90 degrees.
      CE          => '1',
      D           => SDRAM_dq_in_tmp(i),
      R           => '0',
      S           => '0'
    );
end generate;
-- 16 IDDR buffers are uses to generate a 32-bit wide clock-synchronous output from 16-bit wide READ data originating on the DDR2 SDRAM
	
dq_oddr : for i in 0 to 15 generate				  
dq_oddrn : ODDR 
  generic map(
      DDR_CLK_EDGE => "SAME_EDGE",
      INIT         => '0',
      SRTYPE       => "SYNC"
      )
  port map(
      Q           => SDRAM_dq_out_tmp(i),
      C           => CLK,
      CE          => '1',
      D1          => SDRAM_dq_out(i),
      D2          => SDRAM_dq_out(i + 16),
      R           => '0',
      S           => '0'
    );
end generate;    
-- 16 IDDR buffers are uses to generate a 16-bit wide DDR output from 32-bit wide clock-synchronous WRITE data originating in this controller
 
SDRAM_DQ <= SDRAM_dq_out_tmp when (dq_write = '1')  else "ZZZZZZZZZZZZZZZZ";   				
-- SDRAM_DQ is bidirectional, set to hi-Z when not using it for a write

-----------------------------------------------------
--	PHY: SDRAM_DQS (single ended)
-----------------------------------------------------
			 
dqs_oddr : for i in 0 to 1 generate				  
dqs_oddrn : ODDR 
  generic map(
      DDR_CLK_EDGE => "SAME_EDGE",
      INIT         => '0',
      SRTYPE       => "SYNC"
      )
  port map(
      Q           => SDRAM_dqs_out_tmp(i),
      C           => CLK_130,
      CE          => '1',-- dq_write_reg,			--  DQS is not a free-running clock.  Only strobe when data is present on DQ, otherwise go low for pre/postamble
      D1          => '1',
      D2          => '0',
      R           => '0',
      S           => '0'
    );
end generate;  

SDRAM_DQS <= SDRAM_dqs_out_tmp when (dq_write = '1') else "ZZ";  -- (dqs_write_reg = '1') else "ZZ";  
-- SDRAM_DQ is bidirectional, set to hi-Z when not using it for a write, write_preamble, or write_postamble

-----------------------------------------------------
--	PHY: SDRAM_DM
-----------------------------------------------------
-- the data mask allows the selection of individual bytes during a 32-bit longword WRITE
-- bytes that should not be written are "masked" by setting DM high
-- there are two DM lines in a 16x SDRAM chip, one for the high byte and one for the low byte				  
-- The four bits of dm_write are ordered as follows "hi_word_hi_byte hi_word_lo_byte lo_word_lo_byte lo_word_lo_byte"

dm_oddr : for i in 0 to 1 generate				  
dm_oddrn : ODDR 
  generic map(
      DDR_CLK_EDGE => "SAME_EDGE",
      INIT         => '0',
      SRTYPE       => "SYNC"
      )
  port map(
      Q           => SDRAM_DM(i),		-- data mask.  assert high to mask, lo to write
      C           => CLK,				-- the write mask is written in phase with the data
      CE          => '1',
      D1          => dm_write(i),
      D2          => dm_write(i + 2),	-- "+ 2" to separate the hi-bytes and lo-bytes (see above) into the hi-byte and lo-byte DM lines
      R           => '0',
      S           => '0'
    );
end generate; 

-----------------------------------------------------
--	Stubs for 64 bit functionality
-----------------------------------------------------

wr_dat_64 <= x"01234567" & wr_dat;
wr_we_8 <= "0000" & wr_we;
rd_dat <= rd_dat_r(63 downto 0);

--process
--begin
--	wait until rising_edge(CLK);
--		dq_write_reg <= dq_write;
--		dqs_write_reg <= dqs_write;
--end process;

process
begin
	wait until rising_edge(CLK_130);
	SDRAM_dq_in_reg <= SDRAM_dq_in;
end process;
-----------------------------------------------------
--	FSM
-----------------------------------------------------
gen_fsm : process (CLK, reset)
variable bank : integer range 0 to 3;
begin
if (reset='1') then									-- reset state should be held for 200us MIN
	state <= init;
	SDRAM_A <= conv_std_logic_vector(0, SDRAM_A'length);
	SDRAM_BA <= "000";
	COMMAND <= CMD_ENTER_POWER_DOWN;
	dq_write <= '0';
	dqs_write <= '0';
	dm_write <= "1111";
	bank_active <= conv_std_logic_vector(0, bank_active'length);
	bank_row_active <= conv_std_logic_vector(0, bank_row_active'length); 
	wr_ack <= '0';         
	rd_valid <= '0';
	rd_ack <= '0';
    counter <= 0;
    bank_active <= "000";
    dqs_out_ce <= '0';
    ref_ack <= '0';
    counterRefresh <= 0;
    
elsif (CLK'event and CLK='1') then
case (state) is
-----------------------------------------------------
--	set nCS
-----------------------------------------------------
when init =>	
			if (counter = ct_init) then
				state <= init_precharge;
				counter <= 0;
			else
				counter <= counter + 1;
			end if;
			COMMAND <= CMD_NOP;
			wr_ack <= '0';
-----------------------------------------------------
--	initial precharge all command (1Gb_DDR2 p87)
-----------------------------------------------------
when init_precharge => 
			SDRAM_BA <= "000";	
			SDRAM_A <= "00010000000000";		-- A10 high indicates an all bank precharge command
			COMMAND <= CMD_PRECHARGE;
			counter <= 0;			
			state <= init_precharge_done;

when init_precharge_done =>
			SDRAM_BA <= "000";	
			SDRAM_A <= "00010000000000";
			COMMAND <= CMD_NOP;
			if (counter = ct_precharge) then	
				state <= init_mode_2;			-- follow prescribed init sequence (1Gb_DDR2 p87)
				counter <= 0;
			else
				counter <= counter + 1;
			end if;	
-----------------------------------------------------
--	init_mode 2 Register
-----------------------------------------------------   
when init_mode_2 =>
			SDRAM_BA <= "010";					-- Extended Mode Register (EMR) 2
			SDRAM_A <= "00000000000000";		-- SDRAM_A is used to set the mode register
												-- E7 '0' = 1x refresh rate (0C to 85C)
												-- all other bits must be zero (1GB_DDR p85)
			COMMAND <= CMD_LOAD_MODE;
			counter <= 0;			
			state <= init_mode_2_done;

when init_mode_2_done =>						-- NOP command	
			SDRAM_BA <= "000";	
			SDRAM_A <= "00000000000000";
			COMMAND <= CMD_NOP;
			if (counter = 1) then				-- tMRD timing requirement is 2 clock cycles (p37)
				state <= init_mode_3;			-- follow prescribed init sequence (1Gb_DDR2 p87)
				counter <= 0;
			else
				counter <= counter + 1;
			end if;	  
-----------------------------------------------------
--	init_mode 3 Register
-----------------------------------------------------   
when init_mode_3 =>
			SDRAM_BA <= "011";					-- Extended Mode Register (EMR) 3
			SDRAM_A <= "00000000000000";		-- See 1GB_DDR2 p86  for register definition		
			COMMAND <= CMD_LOAD_MODE;
			counter <= 0;
			state <= init_mode_3_done;
			
when init_mode_3_done =>			
			SDRAM_BA <= "000";	
			SDRAM_A <= "00000000000000";
			COMMAND <= CMD_NOP;
			if (counter = 1) then				-- tMRD timing requirement is 2 clock cycles (p37)
				state <= init_mode_1;
				counter <= 0;
			else
				counter <= counter + 1;
			end if;	         
-----------------------------------------------------
--	init_mode 1 Register
-----------------------------------------------------   
when init_mode_1 =>								
			SDRAM_BA <= "001";					-- Extended Mode Register (EMR) 1	
			SDRAM_A <= "00010000000100";		-- DQS# disable	/ RTT = 75 Ohms	
												-- See 1GB_DDR p81 for register definition 										
			COMMAND <= CMD_LOAD_MODE;
			counter <= 0;
			state <= init_mode_1_done;
			
when init_mode_1_done =>			
			SDRAM_BA <= "000";	
			SDRAM_A <= "00000000000000";
			COMMAND <= CMD_NOP;
			if (counter = 1) then				-- tMRD timing requirement is 2 clock cycles (p37)
				state <= init_mode_0;			-- follow prescribed init sequence (1Gb_DDR2 p87)
				counter <= 0;
			else
				counter <= counter + 1;
			end if;	
-----------------------------------------------------
--	init_mode 0 Register
-----------------------------------------------------   
when init_mode_0 =>
			SDRAM_BA <= "000";							-- Mode register
			SDRAM_A <= "0001010" & reg_CAS & "0010";	-- Burst length = 4 / Reset DLL / Write recovery = 3
			COMMAND <= CMD_LOAD_MODE;
			counter <= 0;
			state <= init_mode_0_done;
			
when init_mode_0_done =>			
			SDRAM_BA <= "000";	
			SDRAM_A <= "00000000000000";
			COMMAND <= CMD_NOP;
			if (counter = 199) then				-- 200 cycles of clock until READ/WRITE are required following DLL reset
				state <= init_precharge_0;		-- follow prescribed init sequence (1Gb_DDR2 p87)
				counter <= 0;
			else
				counter <= counter + 1;
			end if;	    
-----------------------------------------------------
--	Precharge 0
-----------------------------------------------------   
when init_precharge_0 =>						-- another init precharge command
			SDRAM_BA <= "000";	
 			SDRAM_A <= "00010000000000";
			COMMAND <= CMD_PRECHARGE;
			counter <= 0;
			state <= init_precharge_0_done;
			
when init_precharge_0_done =>			
			SDRAM_BA <= "000";	
			SDRAM_A <= "00000000000000";
			COMMAND <= CMD_NOP;
			if (counter = ct_precharge) then	-- tRPA (precharge all) timing requirement = 12.5ns (p36)
				state <= init_refresh_0;		-- follow prescribed init sequence (1Gb_DDR2 p87)
				counter <= 0;
			else
				counter <= counter + 1;
			end if;	  
-----------------------------------------------------
--	refresh 0
-----------------------------------------------------   
when init_refresh_0 =>
			SDRAM_BA <= "000";					-- REFRESH command
 			SDRAM_A <= "00010000000000";		-- A10 actually has no effect
			COMMAND <= CMD_REFRESH;
			counter <= 0;
			state <= init_refresh_0_done;
			
when init_refresh_0_done =>			
			SDRAM_BA <= "000";	
			SDRAM_A <= "00000000000000";
			COMMAND <= CMD_NOP;
			if (counter = ct_refresh) then		-- tRFC (REFRESH interval) = 127.5ns (p37)
				state <= init_refresh_1;		-- follow prescribed init sequence (1Gb_DDR2 p87)
				counter <= 0;
			else
				counter <= counter + 1;
			end if;	  
-----------------------------------------------------
--	refresh 1
-----------------------------------------------------   
when init_refresh_1 =>							-- two or more refresh commands are required (note 10, p89)
			SDRAM_BA <= "000";	
 			SDRAM_A <= "00010000000000";
			COMMAND <= CMD_REFRESH;
			counter <= 0;
			state <= init_refresh_1_done;
			
when init_refresh_1_done =>			
			SDRAM_BA <= "000";	
			SDRAM_A <= "00000000000000";
			COMMAND <= CMD_NOP;
			if (counter = ct_refresh) then
				state <= init_mode_0_2;			-- follow prescribed init sequence (1Gb_DDR2 p87)
				counter <= 0;
			else
				counter <= counter + 1;
			end if;	
-----------------------------------------------------
--	init_mode 0 Register 2nd
-----------------------------------------------------   
when init_mode_0_2 =>
			SDRAM_BA <= "000";							-- Mode register
 			SDRAM_A <= "0001000"  & reg_CAS & "0010";	--same settings EXCEPT do not reset the DLL
			COMMAND <= CMD_LOAD_MODE;
			counter <= 0;
			state <= init_mode_0_2_done;
			
when init_mode_0_2_done =>			
			SDRAM_BA <= "000";	
			SDRAM_A <= "00000000000000";
			COMMAND <= CMD_NOP;
			if (counter = 1) then				-- tMRD timing requirement is 2 clock cycles (p37)
				state <= init_mode_1_2;			-- follow prescribed init sequence (1Gb_DDR2 p87)
				counter <= 0;
			else
				counter <= counter + 1;
			end if;	          
-----------------------------------------------------
--	init_mode 1 Register 2nd
-----------------------------------------------------   
when init_mode_1_2 =>
			SDRAM_BA <= "001";					-- EMR 1
 			SDRAM_A <= "00011110000100";		-- Default OCD / DQS# disable / RTT = 75 Ohm
												-- See 1GB_DDR p81 for register definition 
			COMMAND <= CMD_LOAD_MODE;
			counter <= 0;
			state <= init_mode_1_2_done;
			
when init_mode_1_2_done =>			
			SDRAM_BA <= "000";	
			SDRAM_A <= "00000000000000";
			COMMAND <= CMD_NOP;
			if (counter = 1) then				-- tMRD timing requirement is 2 clock cycles (p37)
				state <= init_mode_1_3;			-- follow prescribed init sequence (1Gb_DDR2 p87)
				counter <= 0;
			else
				counter <= counter + 1;
			end if;	
-----------------------------------------------------
--	init_mode 1 Register 3rd
-----------------------------------------------------   
when init_mode_1_3 =>
			SDRAM_BA <= "001";					-- EMR 1
 			SDRAM_A <= "00010000000100";		-- Exit OCD / DQS# disable / RTT = 75 Ohm
--                     "00010001000100"			-- Exit OCD / DQS# disable / RTT = 75 Ohm
			COMMAND <= CMD_LOAD_MODE;
			counter <= 0;
			state <= init_mode_1_3_done;
			
when init_mode_1_3_done =>			
			SDRAM_BA <= "000";	
			SDRAM_A <= "00000000000000";
			COMMAND <= CMD_NOP;
			if (counter = 20) then				-- tMRD timing requirement is 2 clock cycles (p37)
				if (wr_we /= "0000") then
					state <= idle; --write_0;
					counter <= 0;
				end if;
			else
				counter <= counter + 1;
			end if;	             
-----------------------------------------------------
--	IDLE
-----------------------------------------------------
when idle =>	
			COMMAND <= CMD_NOP;
			wr_ack <= '0';
			rd_valid <= '0';
			dm_write <= not wr_we;
			SDRAM_dq_out <= wr_dat_64(31 downto 0);  
			counter <= 0;
 			SDRAM_A <= '0' & wrrd_ras_add;  		-- Row address in A[12:0] (8K) - 1Gb_DDR2 p2
 			 			
			if refresh = '1' then
				state <= refresh_0;
			         
			elsif (wr_we /= "0000") OR
		 	   (rd_re = '1') then 
		 	   	SDRAM_BA <= wrrd_ba_add;				-- Bank address in BA[2:0] (8) - 1Gb_DDR2 p2
				COMMAND <= CMD_ACTIVATE;
				state <= bank_0;
--				bank_active <= wrrd_ba_add; 			-- save the activating bank (to detect a change)
--				bank_row_active <= '0' & wrrd_ras_add;  -- save the activating row (to detect a change)
														-- ACTIVE to PRECHARGE delay tRAS = 70us MAX (p36) : has this been considered?
		 	end if;
-----------------------------------------------------
--	Bank Active
-----------------------------------------------------
when bank_0 => 									-- first state after activating a bank
			COMMAND <= CMD_NOP;		
			SDRAM_A <= "00000000000000";
			bank_active <= wrrd_ba_add; 			-- save the activating bank (to detect a change)
			bank_row_active <= '0' & wrrd_ras_add;  -- save the activating row (to detect a change)			
			if (counter = ct_RCD) then
				state <= active;
				counter <= 0;
			else
				counter <= counter + 1;
			end if;	

-----------------------------------------------------
--	Bank Active Done
-----------------------------------------------------
--when bank_done =>
--			COMMAND <= CMD_NOP;
--			state <= active;  					-- tRCD (ROW to COLUMN delay) = 12.5 ns
-----------------------------------------------------
--	Active
-----------------------------------------------------
when active =>									-- Command to Bank n, 1Gb_DDDR2 p71
			COMMAND <= CMD_NOP;
			wr_ack <= '0';
			rd_valid <= '0';
			dm_write <= not wr_we_8(3 downto 0);
			SDRAM_dq_out <= wr_dat_64(31 downto 0);			
			counter <= 0;
			SDRAM_A <= "00000000000000";
			SDRAM_A <= "0000" & wrrd_cas_add(8 downto 0) & '0';	  	-- For READ/WRITE column address in A[9:0] (1K) - MT47H64M16HR-25E is WORD addressable- 1Gb_DDR2 p2 
																	-- For a PRECHARGE operation only A[10] is significant and we require A[10] = '0' for single bank
			-----------------------------------------------------
			--	Refresh handling
			-----------------------------------------------------  
			if  (refresh = '1') then
				--SDRAM_A <= "00000000000000";	-- PRECHARGE    
				COMMAND <= CMD_PRECHARGE;
				state <= precharge_0;	
				
			-----------------------------------------------------
			--	Bank handling
			-----------------------------------------------------
			elsif(	(wr_we /= "0000") OR			
			   		(rd_re = '1') ) 							   		AND
			   	(	(NOT (bank_active = wrrd_ba_add)) OR							-- changing bank
			   		(NOT (bank_row_active(12 downto 0) = wrrd_ras_add)) ) then		-- changing row
				--SDRAM_A <= "00000000000000";        
				COMMAND <= CMD_PRECHARGE;
				state <= precharge_0;
				
			-----------------------------------------------------
			--	CAS handling
			-----------------------------------------------------     
			elsif (wr_we /= "0000") OR
			   	  (rd_re = '1') then
				if (wr_we /= "0000") then
					COMMAND <= CMD_WRITE;
					state <= write_1;
				else			
					state <= read_0; 
					COMMAND <= CMD_READ;
					rd_ack <= '1';
				end if;

			
		 	end if;
-----------------------------------------------------
--	Precharge All Delay
-----------------------------------------------------
when precharge_0 => 				-- tRPA (precharge all) timing requirement = 12.5ns (p36) 
			COMMAND <= CMD_NOP;
			SDRAM_A <= "00000000000000";
			counter <= counter + 1;
			if (counter = ct_precharge) then			
				counter <= 0;			
				if (refresh = '1') then
					state <= refresh_0;
				else
					state <= idle; 	
				end if;					
			end if;
-----------------------------------------------------
--	Precharge All Done
-----------------------------------------------------
--when precharge_done =>
--			COMMAND <= CMD_NOP;
--			counter <= counter + 1;
--			if (counter = 0) then						
--				if (refresh = '1') then
--					state <= refresh_0;
--				else
--					state <= idle; 	
--				end if;					
--			end if;
-----------------------------------------------------
--	Refresh All Delay
-----------------------------------------------------
when refresh_0 => 											
			COMMAND <= CMD_REFRESH;
			SDRAM_A <= "00000000000000";
			ref_ack <= '1';
			state <= refresh_1;	
			counter <= 0;	
			
when refresh_1 =>
			COMMAND <= CMD_NOP;
			SDRAM_A <= "00000000000000";
			ref_ack <= '0';
			counter <= counter + 1;
			if (counter = ct_refresh) then					-- tRFC = refresh-to-refresh (or refresh-to-activate) = 127.5 ns
				counterRefresh <= counterRefresh + 1;
				if counterRefresh = refreshCount then
					counterRefresh <= 0;
					state <= idle;
				else
					state <= refresh_0;				
				end if;
			end if;
-----------------------------------------------------
--	Write 0
-----------------------------------------------------
--when write_0 => 										-- SDRAM registers WRITE command
--			COMMAND <= CMD_NOP;
--			state <= write_1;
-----------------------------------------------------
--	Write 1
-----------------------------------------------------
when write_1 =>	
			COMMAND <= CMD_NOP;
			SDRAM_A <= "00000000000000";
			--dqs_write <= '1';
			if (counter = ct_CAS) then
				state <= write_2;
				counter <= 0;
			else
				counter <= counter + 1;
			end if;	
			
-----------------------------------------------------
--	Write 2
-----------------------------------------------------
when write_2 => 						-- 4n prefectch with x16 requires a second longword 
			dq_write <= '1';
			COMMAND <= CMD_NOP;		
			SDRAM_A <= "00000000000000";
			SDRAM_dq_out <= wr_dat_64(63 downto 32);
			dm_write <= not wr_we_8(7 downto 4);							-- dm is output through the DDR interface						
			state <= write_3;							--  however the last 32 bits are not presented by the controller
-----------------------------------------------------
--	Write 3
-----------------------------------------------------
when write_3 =>
			COMMAND <= CMD_NOP;
			SDRAM_A <= "00000000000000";
			state <= write_4;
-----------------------------------------------------
--	Write 4
-----------------------------------------------------
when write_4 =>
			dq_write <= '0';
			wr_ack <= '1';	
			COMMAND <= CMD_NOP;
			SDRAM_A <= "00000000000000";
			--dqs_write <= '0';
			counter <= 0;
			state <= write_5;

-----------------------------------------------------
--	Write 4
-----------------------------------------------------
when write_5 =>      
			wr_ack <= '0';
			COMMAND <= CMD_NOP;
			SDRAM_A <= "00000000000000";			
			counter <= counter + 1;
			if (counter = ct_WR) then						-- tWR Write Recovery time 15ns
				state <= active;
			end if;
-----------------------------------------------------
--	Read 0
-----------------------------------------------------
when read_0 =>											-- SDRAM registers READ command
			SDRAM_A <= "00000000000000";
			COMMAND <= CMD_NOP;
			if (counter = ct_CAS) then
				state <= read_1;
				counter <= 0;
			else
				counter <= counter + 1;
			end if;				
			rd_ack <= '0';
-----------------------------------------------------
--	Read 1
-----------------------------------------------------
when read_1 => 		
			SDRAM_A <= "00000000000000";									
			COMMAND <= CMD_NOP;
			state <= read_2; 
-----------------------------------------------------
--	Read 2
-----------------------------------------------------
when read_2 =>				
			SDRAM_A <= "00000000000000";							
			COMMAND <= CMD_NOP;
			state <= read_3; 
-----------------------------------------------------
--	Read 3
-----------------------------------------------------
when read_3 =>
			SDRAM_A <= "00000000000000"; 											
			COMMAND <= CMD_NOP;
			state <= read_3b;
			
when read_3b => 	
			SDRAM_A <= "00000000000000";										
			COMMAND <= CMD_NOP;
			state <= read_4;
-----------------------------------------------------
--	Read 3
-----------------------------------------------------
when read_4 =>
			SDRAM_A <= "00000000000000";
			COMMAND <= CMD_NOP;		
			rd_dat_r(31 downto 0) <= SDRAM_dq_in_reg;			-- register data in	- low word of burst
			state <= read_5;
						
when read_5 => 
			SDRAM_A <= "00000000000000";
			COMMAND <= CMD_NOP;		
			rd_dat_r(63 downto 32) <= SDRAM_dq_in_reg;			-- register data in - high word of burst
			rd_valid <= '1';	
			state <= active;
-----------------------------------------------------
--	Read Done
-----------------------------------------------------
--when read_done => 										-- IDDR buffer has registered signal at the output
--			COMMAND <= CMD_NOP;
--			rd_valid <= '1';							-- "rd_dat <= SDRAM_dq_in" is made as a concurrent statement and rd_dat is not registered
-- 			state <= active;							-- keep ROW open and return to ACTIVE state (next access likely on same row)							
--														-- 4n prefetch with x16 yields a 64 bit word, the latter 32 bits currently ignored
--			
-----------------------------------------------------
--	Others
-----------------------------------------------------
when others => 
end case;
end if;
end process;


end Struct;
