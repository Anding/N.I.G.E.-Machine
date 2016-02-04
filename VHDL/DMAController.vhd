library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;

Library UNISIM;
use UNISIM.vcomponents.all;

entity DMAcontroller is
    Port (	CLK : 	IN STD_LOGIC;												-- 100MHz
				-- AXI4 lite connections to CPU
				s_aresetn : in STD_LOGIC;												-- Active low.  Hold until memory warm up is complete
				-- address write
				s_axi_awaddr : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
				s_axi_awvalid : IN STD_LOGIC;
				s_axi_awready : OUT STD_LOGIC;
				-- write
				s_axi_wdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
				s_axi_wstrb : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
				s_axi_wvalid : IN STD_LOGIC;
				s_axi_wready : OUT STD_LOGIC;
				-- write response
--				s_axi_bresp : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
--				s_axi_bvalid : OUT STD_LOGIC;
--				s_axi_bready : IN STD_LOGIC;
				-- address read
				s_axi_araddr : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
				s_axi_arvalid : IN STD_LOGIC;
				s_axi_arready : OUT STD_LOGIC;
				-- read
				s_axi_rdata : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
--				s_axi_rresp : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
				s_axi_rvalid : OUT STD_LOGIC;
--				s_axi_rready : IN STD_LOGIC;
				-- AIX4 connections to Text buffer
				t_axi_araddr : IN  std_logic_vector(31 downto 0);
				t_axi_arlen : IN  std_logic_vector(7 downto 0);				-- Burst length = value + 1
--				t_axi_arsize : IN  std_logic_vector(2 downto 0);			-- Size in bytes: "01" = 2 bytes, "10" = 4 bytes
--				t_axi_arburst : IN  std_logic_vector(1 downto 0);			-- Type: "01" = INCR
				t_axi_arvalid : IN  std_logic;
				t_axi_arready : OUT  std_logic;
				t_axi_rdata : OUT  std_logic_vector(15 downto 0);
				t_axi_rresp : OUT  std_logic_vector(1 downto 0);			
				t_axi_rlast : OUT  std_logic;										-- Set high on last data item
				t_axi_rvalid : OUT  std_logic;
--				t_axi_rready : IN  std_logic;
				-- PSDRAM connections
				ADDR_SDRAM : out  STD_LOGIC_VECTOR (23 downto 1);		-- SDRAM connections to CellularRAM
				DATA_SDRAM : inout  STD_LOGIC_VECTOR (15 downto 0);
				OE_SDRAM : out  STD_LOGIC;
				WE_SDRAM : out  STD_LOGIC;
				ADV_SDRAM : out  STD_LOGIC;
				CLK_SDRAM : out STD_LOGIC;
				UB_SDRAM : out STD_LOGIC;
				LB_SDRAM : out  STD_LOGIC;
				CE_SDRAM : out  STD_LOGIC;
				CRE_SDRAM : out  STD_LOGIC;
				WAIT_SDRAM : in  STD_LOGIC;
				-- debug
				debug : out std_logic_vector(7 downto 0)
			  );			
end DMAcontroller;

architecture Behavioral of DMAcontroller is

type state_type is (	startup, idle,
							read_pagemode_lo, read_pagemode_hi, read_AXI_handshake,
							write_async_lo, write_async_gap, write_async_hi, write_AXI_a_handshake, write_AXI_b_handshake,
							set_RCR, set_gap1, set_BCR, set_gap2, read_after_set1, read_after_set2,
							init_burst1, init_burst2, init_burst3, init_burst4, burst, burst_pause, end_burst,
							resume_burst1, resume_burst2, resume_burst3 ); 
							
signal state, next_state : state_type; 
signal count, timer : std_logic_vector(7 downto 0);
signal s_axi_rdata_r, s_axi_rdata_n : std_logic_vector(31 downto 0);
signal s_axi_araddr_r, s_axi_awaddr_r, s_axi_wdata_r : std_logic_vector(31 downto 0);
signal s_axi_wstrb_r : std_logic_vector(3 downto 0);
signal CLK_SDRAM_i : std_logic;
signal DATA_IN_SDRAM, DATA_OUT_SDRAM : std_logic_vector(15 downto 0);
signal DATA_SDRAM_ISINPUT : std_logic;
signal ADDR_r, ADDR_n : std_logic_vector(23 downto 1);
signal TOP_r, TOP_n : std_logic_vector(23 downto 1);
signal t_axi_araddr_r : std_logic_vector(31 downto 0);
signal WAIT_SDRAM_m1 : std_logic;
signal t_axi_rdata_r, t_axi_rdata_n : std_logic_vector(15 downto 0);
signal debug_i : std_logic_vector(7 downto 0);
	
begin

   SYNC_PROC: process 
   begin
      wait until rising_edge(CLK);
         if (s_aresetn = '0') then
				state <= startup;
				debug <= (others=>'0');
         else
				count <= count + 1;
				debug <= debug_i;
				if state = idle then							-- set registers only on entering a cycle
					s_axi_awaddr_r <= s_axi_awaddr;
					s_axi_wstrb_r <= s_axi_wstrb;
					s_axi_wdata_r <= s_axi_wdata;				
					s_axi_araddr_r <= s_axi_araddr;
					t_axi_araddr_r <= t_axi_araddr;
				end if;
				s_axi_rdata_r <= s_axi_rdata_n;	
				t_axi_rdata_r <= t_axi_rdata_n;
				WAIT_SDRAM_m1 <= WAIT_SDRAM;
				ADDR_r <= ADDR_n;
				TOP_r <= TOP_n;
				if (count >= timer) then 
					state <= next_state;
					count <= (others=>'0');	
				end if;   
      end if;
   end process;
	
	-- AXI4 Lite
		-- handshake signals will not be registered.  Rationale: only one layer of logic from state register
		-- data and address I/O will be registered
		
	-- read channel	
	with state select
		s_axi_arready <=	'1' when read_pagemode_lo,														-- acknowledge at idle since this channel has first priority
								'0' when others;
								
	with state select
		s_axi_rvalid <=	'1' when read_AXI_handshake,						
								'0' when others;
	with state select
		s_axi_rdata_n <= 	DATA_IN_SDRAM & s_axi_rdata_r(15 downto 0) when read_pagemode_hi,
								s_axi_rdata_r(31 downto 16) & DATA_IN_SDRAM when read_pagemode_lo,
								s_axi_rdata_r when others;								
	
	s_axi_rdata <= s_axi_rdata_r;
--	s_axi_rresp	<= "00";
	
	-- write address, write, and write response channels
	with state select
		s_axi_awready <=	'1' when write_AXI_a_handshake,														-- acknowledge only after entering write cycle
								'0' when others;
	
	with state select
		s_axi_wready <=	'1' when write_AXI_a_handshake,														-- acknowledge only after entering write cycle
								'0' when others;	

--	with state select
--		s_axi_bvalid <=	'1' when write_AXI_b_handshake,						
--								'0' when others;
--	s_axi_bresp	<= "00";
	
	-- AXI 4 read channel	
	with state select
		t_axi_arready <=	'1' when init_burst2,															-- acknowledge only after entering read cycle
								'0' when others;
								
	with state select
		t_axi_rvalid <= 	'1' when burst,
								'0' when others;								
								
	t_axi_rlast <= 	'1' when (state = burst and ADDR_r = TOP_r)
								else '0';
								
	t_axi_rdata <= t_axi_rdata_r;
	t_axi_rresp	<= "00";
	
	ADDR_n <= 	t_axi_araddr_r(23 downto 1) when (state = init_burst2)
					else ADDR_r + "00000000000000000000001" when (state = burst)
					else ADDR_r;
					
	with state select
	TOP_n <= 	(t_axi_arlen(7 downto 0) + t_axi_araddr_r(23 downto 1)) when init_burst2,
					TOP_r when others;
				
	t_axi_rdata_n <= 	DATA_IN_SDRAM;								
	
	
	
	-- PSDRAM		
		-- control signals will not be registered
		-- data and address I/O will be registered or one level of logic on a register
	with state select
		ADDR_SDRAM	<=		s_axi_araddr_r(23 downto 2) & '0' when read_pagemode_lo,
								s_axi_awaddr_r(23 downto 2) & '0' when write_async_lo,
								s_axi_araddr_r(23 downto 2) & '1' when read_pagemode_hi,
								s_axi_awaddr_r(23 downto 2) & '1' when write_async_hi,
								"000"	& "0" & "00000000000" & "1" & "00" & "1" & "0" & "000" when set_RCR,
								"000" & "1" & "000" & "0" & "0" & "011" & "1" & "0" & "0" & "0" & "1" & "0" & "0" & "1" & "111" when set_BCR,
						    --"000     1		000     0     0     011     1     0     1     0		 0     0		 1     1     111"  v2.0
								t_axi_araddr_r(23 downto 1) when init_burst1,
								t_axi_araddr_r(23 downto 1) when init_burst2,
								t_axi_araddr_r(23 downto 1) when init_burst3,
								ADDR_r when resume_burst1,
								ADDR_r when resume_burst2,
								ADDR_r when resume_burst3,
								(others=>'0') when others;
								
	with state select
		ADV_SDRAM <= 		'0' when read_pagemode_lo,
								'0' when read_pagemode_hi,
								'0' when write_async_lo,
								'0' when write_async_hi,
								'0' when set_RCR,
								'0' when set_BCR,
								'0' when read_after_set1,
								'0' when read_after_set2,
								'0' when init_burst2,
								'0' when init_burst3,
								'0' when resume_burst2,
								'0' when resume_burst3,
								'1' when others;							
								
	with state select
		CE_SDRAM <= 		'0' when read_pagemode_lo,
								'0' when read_pagemode_hi,
								'0' when write_async_lo,
								'0' when write_async_hi,
								'0' when set_RCR,
								'0' when set_BCR,
								'0' when read_after_set1,
								'0' when read_after_set2,								
								'0' when init_burst2,
								'0' when init_burst3,
								'0' when init_burst4,
								'0' when resume_burst2,
								'0' when resume_burst3,						
								'0' when burst,
								'0' when end_burst,
								'1' when others;
		
	with state select
		OE_SDRAM <= 		'0' when others;
					
	with state select
		LB_SDRAM <= 		not s_axi_wstrb_r(0) when write_async_lo,
								not s_axi_wstrb_r(2) when write_async_hi,
								'0' when others;

	with state select
		UB_SDRAM <= 		not s_axi_wstrb_r(1) when write_async_lo,
								not s_axi_wstrb_r(3) when write_async_hi,
								'0' when others;

	with state select
		WE_SDRAM <=			'0' when write_async_lo,
								'0' when write_async_hi, 	
								'0' when set_RCR,
								'0' when set_BCR,
								'1' when others;
		
	with state select
		DATA_OUT_SDRAM <= 		s_axi_wdata_r(15 downto 0) when write_async_lo,
										s_axi_wdata_r(31 downto 16) when write_async_hi,
										(others => '0') when others;
										
	with state select
		DATA_SDRAM_ISINPUT <= 	'0' when write_async_lo,
										'0' when write_async_hi,
										'1' when others;
	
		
	with state select
		CLK_SDRAM_i <=		'1' when init_burst2,
								'1' when init_burst4,
								'1' when resume_burst2,						
								'1' when burst,
								'0' when others;
		
	with state select	
		CRE_SDRAM	<=		'1' when set_RCR,
								'1' when set_BCR,
								'0' when others;
		
	CLK_SDRAM_buffer : ODDR
   generic map(
      DDR_CLK_EDGE => "OPPOSITE_EDGE", -- "OPPOSITE_EDGE" or "SAME_EDGE" 
      INIT => '0',   	-- Initial value for Q port ('1' or '0')
      SRTYPE => "SYNC") -- Reset Type ("ASYNC" or "SYNC")
   port map (
      Q => CLK_SDRAM,   -- 1-bit DDR output
      C => CLK,    		-- 1-bit clock input
      CE => '1',  		-- 1-bit clock enable input
      D1 => CLK_SDRAM_i,-- 1-bit data input (positive edge)
      D2 => '0',  		-- 1-bit data input (negative edge)
      R => '0',    		-- 1-bit reset input
      S => '0'     		-- 1-bit set input
   );
	
	GEN_IOBUF:
	for i in 15 downto 0 generate
		DATA_SDRAM_buffer : IOBUF
		generic map (
			DRIVE => 12,
			IOSTANDARD => "DEFAULT",
			SLEW => "SLOW")
		port map (
			I => DATA_OUT_SDRAM(i),    -- Buffer input (i.e. output from FPGA)
			IO => DATA_SDRAM(i),			-- Buffer inout port (connect directly to top-level port)
			O => DATA_IN_SDRAM(i),     -- Buffer output (i.e. input to FPGA)
			T => DATA_SDRAM_ISINPUT		-- 3-state enable input, high=input, low=output 
		);	
	end generate;
 
   NEXT_STATE_DECODE: process (state, s_axi_arvalid, s_axi_awvalid, s_axi_wvalid, ADDR_r, TOP_r,
											t_axi_arvalid, s_axi_wstrb, wait_SDRAM, s_axi_wstrb_r)
   begin
      case (state) is
		
			when read_pagemode_lo =>
				timer <= CONV_STD_LOGIC_VECTOR(8,8);								-- 70ns async read access but simulator suggests needs 90ns
				next_state <= read_pagemode_hi;
				debug_i <= x"01";
				
			when read_pagemode_hi =>
				timer <= CONV_STD_LOGIC_VECTOR(6,8);								-- 20ns page read access but hardware tests suggests needs 70ns
				next_state <= read_AXI_handshake;
				debug_i <= x"02";
				
			when read_AXI_handshake =>
				timer <= (others=>'0');
--				if s_axi_rready = '1' then
					next_state <= idle;
--				else
--					next_state <= state;
--				end if;
				debug_i <= x"03";
				
			when write_AXI_a_handshake =>
				timer <= (others=>'0');
				if s_axi_wstrb(1 downto 0) = "00" then
					next_state <= write_async_hi;
				else
					next_state <= write_async_lo;
				end if;
				debug_i <= x"04";
				
			when write_async_lo =>
				timer <= CONV_STD_LOGIC_VECTOR(6,8);								-- 70ns async write cycle
				if s_axi_wstrb_r(3 downto 2) = "00" then
					next_state <= write_AXI_b_handshake;
				else
					next_state <= write_async_gap;			
				end if;
				debug_i <= x"05";
				
			when write_async_gap =>														-- 10ns interval between write cycles
				timer <= (others=>'0');
				next_state <= write_async_hi;
				debug_i <= x"06";
				
			when write_async_hi =>
				timer <= CONV_STD_LOGIC_VECTOR(6,8);								-- 70ns write cycle
				next_state <= write_AXI_b_handshake;		
				debug_i <= x"07";

			when write_AXI_b_handshake =>
				timer <= (others=>'0');
--				if s_axi_bready = '1' then
					next_state <= idle;
--				else
--					next_state <= state;
--				end if;		
				debug_i <= x"08";

			when startup =>
				timer <= (others=>'0');
				next_state <= set_RCR;
				debug_i <= x"09";
				
			when set_RCR =>
				timer <= CONV_STD_LOGIC_VECTOR(6,8);
				next_state <= set_gap1;	
				debug_i <= x"0A";				
				
			when set_BCR =>
				timer <= CONV_STD_LOGIC_VECTOR(6,8);
				next_state <= set_gap2;	
				debug_i <= x"0B";

			when set_gap1 =>
				timer <= (others=>'0');
				next_state <= read_after_set1;
				debug_i <= x"0C";

			when set_gap2 =>
				timer <= (others=>'0');
				next_state <= read_after_set2;
				debug_i <= x"0D";				

			when read_after_set1 =>
				timer <= CONV_STD_LOGIC_VECTOR(8,8);
				next_state <= set_BCR;
				debug_i <= x"0E";					

			when read_after_set2 =>
				timer <= CONV_STD_LOGIC_VECTOR(8,8);
				next_state <= idle;	
				debug_i <= x"0F";	
				
			when init_burst1 =>
				timer <= (others=>'0');
				next_state <= init_burst2;
				debug_i <= x"10";	
				
			when init_burst2 =>
				timer <= (others=>'0');
				next_state <= init_burst3;
				debug_i <= x"11";	

			when init_burst3 =>
				timer <= (others=>'0');
				next_state <= init_burst4;
				debug_i <= x"12";
				
			when init_burst4 =>
				timer <= (others=>'0');
				if WAIT_SDRAM = '0' then
					next_state <= burst;	
				else
					next_state <= state;
				end if;
				debug_i <= x"13";
					
			when burst =>
				timer <= (others=>'0');
				if (ADDR_r = TOP_r) then
					next_state <= end_burst;
				elsif WAIT_SDRAM = '1' then
					next_state <= burst_pause;
				else
					next_state <= state;
				end if;
				debug_i <= x"14";
				
			when burst_pause =>
				timer <= CONV_STD_LOGIC_VECTOR(8,8);
				next_state <= resume_burst1;	
				debug_i <= x"13";
					
			when resume_burst1 =>
				timer <= (others=>'0');
				next_state <= resume_burst2;
				debug_i <= x"14";
				
			when resume_burst2 =>
				timer <= (others=>'0');
				next_state <= resume_burst3;
				debug_i <= x"15";

			when resume_burst3 =>
				timer <= (others=>'0');
				next_state <= init_burst4;
				debug_i <= x"16";
				
			when end_burst =>
				timer <= (others=>'0');
				next_state <= idle;
				debug_i <= x"17";				
		
			when others =>																	-- idle
				timer <= (others=>'0');
            if s_axi_arvalid = '1' then	
               next_state <= read_pagemode_lo;
				elsif (s_axi_awvalid = '1' and s_axi_wvalid = '1') then
					next_state <= write_AXI_a_handshake;
				elsif t_axi_arvalid = '1' then --and t_axi_arsize = "001" and t_axi_arburst = "01" then
					next_state <= init_burst1;
				else
					next_state <= state;
            end if;
				debug_i <= x"00";
				
      end case;      
   end process;


end Behavioral;

