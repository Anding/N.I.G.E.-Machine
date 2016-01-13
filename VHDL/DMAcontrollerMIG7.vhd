library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;

Library UNISIM;
use UNISIM.vcomponents.all;

entity DMAcontrollerMIG7 is
	Port (
	CLK100MHZ	: IN STD_LOGIC;
	CLK200MHZ	: IN STD_LOGIC;
	RESET 		: IN STD_LOGIC;	
	-- AXI4 lite connections (as seen by SLAVE)--------------------   's' channel is connected to the CPU
	-- address of write channel
	s_axi_awvalid : IN STD_LOGIC;					-- handshake protocol: source indicates channel data valid
	s_axi_awready : OUT STD_LOGIC;					-- destination indicates that it can accept channel data
											-- transfer occurs only when valid and ready are high
											-- destination may hold ready high or wait for valid
											-- source may not wait for ready to assert valid
	s_axi_awaddr : IN STD_LOGIC_VECTOR(31 DOWNTO 0);			-- true byte addressing											
	-- write channel
	s_axi_wdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
	s_axi_wstrb : IN STD_LOGIC_VECTOR(3 DOWNTO 0);			-- active high byte mask for the data	
	s_axi_wvalid : IN STD_LOGIC;
	s_axi_wready : OUT STD_LOGIC;
	-- write response channel						-- this channel to be removed!
	--s_axi_bresp : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);			-- write response value: see constants
	--s_axi_bvalid : OUT STD_LOGIC;					-- BVALID is dependent on WVALID, WREADY, AWVALID, and AWREADY,  
	--s_axi_bready : IN STD_LOGIC;
	-- address of read channel
	s_axi_araddr : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
	s_axi_arvalid : IN STD_LOGIC;
	s_axi_arready : OUT STD_LOGIC;
	-- read/read-response channel
	s_axi_rdata : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
	--s_axi_rresp : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);			-- read response value: see constants
	s_axi_rvalid : OUT STD_LOGIC;					-- RVALID is dependent on ARVALID and ARREADY
	--s_axi_rready : IN STD_LOGIC;
	-- AIX4 read channel connections (as seen by SLAVE)-------------  't' channel is connected to the VGA buffer
	-- address of read channel
	t_axi_araddr : IN  std_logic_vector(31 downto 0);		-- true byte addressing	
										-- MASTER indicates only the first byte address, SLAVE will calculate the rest
	-- burst length specifies the number of beats in the data transfer
	t_axi_arlen : IN  std_logic_vector(7 downto 0);			-- No. beats = axi_arlen + 1
	-- burst size is the number of bytes to transfer in each beat (width)
--	t_axi_arsize : IN  std_logic_vector(2 downto 0);			-- size in bytes: "000" = 1 byte, "001" = 2 bytes, "010" = 4 bytes, "011" = 8 bytes, "100" = 16 bytes
	-- burst type is the nature of address increment during the burst 
	--t_axi_arburst : IN  std_logic_vector(1 downto 0);		-- see VHDL constants
	t_axi_arvalid : IN  std_logic;
	t_axi_arready : OUT  std_logic;
	-- read/read-response channel
	t_axi_rdata : OUT  std_logic_vector(127 downto 0);
	--t_axi_rresp : OUT  std_logic_vector(1 downto 0);			
	t_axi_rlast : OUT  std_logic;					-- set high to indicate last data word
	t_axi_rvalid : OUT  std_logic;
	--t_axi_rready : IN  std_logic;
	-- DDR2 memory connections -------------------------------------
      	-- Inouts
	ddr2_dq              : inout std_logic_vector(15 downto 0);
	ddr2_dqs_p           : inout std_logic_vector(1 downto 0);
	ddr2_dqs_n           : inout std_logic_vector(1 downto 0);
	-- Outputs
	ddr2_addr            : out   std_logic_vector(12 downto 0);
	ddr2_ba              : out   std_logic_vector(2 downto 0);
	ddr2_ras_n           : out   std_logic;
	ddr2_cas_n           : out   std_logic;
	ddr2_we_n            : out   std_logic;
	ddr2_ck_p            : out   std_logic_vector(0 downto 0);
	ddr2_ck_n            : out   std_logic_vector(0 downto 0);
	ddr2_cke             : out   std_logic_vector(0 downto 0);
	ddr2_cs_n            : out   std_logic_vector(0 downto 0);
	ddr2_dm              : out   std_logic_vector(1 downto 0);
	ddr2_odt             : out   std_logic_vector(0 downto 0)
	);			
end DMAcontrollerMIG7;

-- There are a number of deviations from the AXI4 protocol as follows:
-- 1. The write response channel axi_b is removed since the MIG7 does not confirm a write response itself
-- 2. The read responese signals axi_rresp are removed since the MIG7 does not confirm a read response itself
-- 3. Read channels are assumed to be always ready after a read request; axi_rready signals are removed
--	this saves implementing additional buffers in the DMA controller
-- 4. The burst size axi_arsize is removed since it is assumed that all bursts are the full width of the databus
-- 5. The burst type axi_abburst is removes since it is assumed that alll bursts are AXI_INCR
-- 6. Not a protocol deviation, but the controller will not affirm either of axi_awready or axi_wready until
--	both axi_awvalid and axi_wvalid are affirmed by the master

architecture RTL of DMAcontrollerMIG7 is

component MIG7
   port (
      -- Inouts
      ddr2_dq              : inout std_logic_vector(15 downto 0);
      ddr2_dqs_p           : inout std_logic_vector(1 downto 0);
      ddr2_dqs_n           : inout std_logic_vector(1 downto 0);
      -- Outputs
      ddr2_addr            : out   std_logic_vector(12 downto 0);
      ddr2_ba              : out   std_logic_vector(2 downto 0);
      ddr2_ras_n           : out   std_logic;
      ddr2_cas_n           : out   std_logic;
      ddr2_we_n            : out   std_logic;
      ddr2_ck_p            : out   std_logic_vector(0 downto 0);
      ddr2_ck_n            : out   std_logic_vector(0 downto 0);
      ddr2_cke             : out   std_logic_vector(0 downto 0);
      ddr2_cs_n            : out   std_logic_vector(0 downto 0);
      ddr2_dm              : out   std_logic_vector(1 downto 0);
      ddr2_odt             : out   std_logic_vector(0 downto 0);
      -- Inputs
      sys_clk_i	      : in	std_logic;     
      -- user interface signals
      app_addr             : in    std_logic_vector(26 downto 0);
      app_cmd              : in    std_logic_vector(2 downto 0);
      app_en               : in    std_logic;
      app_wdf_data         : in    std_logic_vector(127 downto 0);
      app_wdf_end          : in    std_logic;
      app_wdf_mask         : in    std_logic_vector(15 downto 0);
      app_wdf_wren         : in    std_logic;
      app_rd_data          : out   std_logic_vector(127 downto 0);
      app_rd_data_end      : out   std_logic;
      app_rd_data_valid    : out   std_logic;
      app_rdy              : out   std_logic;
      app_wdf_rdy          : out   std_logic;
      app_sr_req           : in    std_logic;
      app_sr_active        : out   std_logic;
      app_ref_req          : in    std_logic;
      app_ref_ack          : out   std_logic;
      app_zq_req           : in    std_logic;
      app_zq_ack           : out   std_logic;
      ui_clk               : out   std_logic;
      ui_clk_sync_rst      : out   std_logic;
      init_calib_complete  : out   std_logic;
      sys_rst		      : in	std_logic);
end component;

-- MIG7 user interface signals
signal sys_clk_i		: std_logic;				-- in: 200MHZ CLOCK
signal sys_rst		: std_logic;				-- in: active lo reset
-- user command information
signal app_en               : std_logic;				-- in: user holds app_en high with a valid app_cmd until app_rdy is asserted
signal app_cmd              : std_logic_vector(2 downto 0);	-- in: see VDHL constants
signal app_rdy              : std_logic;				-- out MIG7 registers a command provided rdy is high
signal app_addr             : std_logic_vector(26 downto 0);	-- in: true byte addressing
									-- addr needs to be incremented for each new command
-- write information
	-- write data must preceed WRITE command or follow within 2 clock cycles
signal app_wdf_data         : std_logic_vector(127 downto 0);	-- in: 64 bit data since 16 ddr2 lines and 2:1 MIG7 clocking
signal app_wdf_mask         : std_logic_vector(15 downto 0);	-- in: active high byte mask for the data (note this is a mask not a write strobe)	
signal app_wdf_wren         : std_logic;				-- in: user holds high throughout transfer to indicate valid data
signal app_wdf_rdy          : std_logic;				-- out MIG registers data provided rdy is high
signal app_wdf_end          : std_logic;				-- in: set high to indicate last data word
-- read information
signal app_rd_data          : std_logic_vector(127 downto 0);	-- out
signal app_rd_data_end      : std_logic;				-- out signals end of burst (not needed in handshake logic)
signal app_rd_data_valid    : std_logic;				-- out valid read data is on the bus
-- user
signal app_sr_req           : std_logic;				-- in: tie to '0'
signal app_sr_active        : std_logic;				-- out: disregard
-- user controlled DRAM refresh
signal app_ref_req          : std_logic;				-- in: tie to '0'
signal app_ref_ack          : std_logic;				-- out disregard
-- user controllerd ZQ calibration
signal app_zq_req           : std_logic;				-- in: tie to '0'
signal app_zq_ack           : std_logic;				-- out disregard
-- user interface
signal ui_clk               : std_logic;				-- out CLOCK
signal ui_clk_sync_rst      : std_logic;				-- out reset
-- calibration complete		
signal init_calib_complete  : std_logic;				-- out MIG7 requires 50-60uS to complete calibraton in simulator

-- MIG7 user interface app_cmd commands
constant MIG_WRITE               : std_logic_vector(2 downto 0) := "000";
constant MIG_READ                : std_logic_vector(2 downto 0) := "001";
-- AXI4 axi_axburst types
constant AXI_INCR          	     : std_logic_vector(1 downto 0) := "01";
-- AXI4 xRESP types
constant AXI_OKAY		     : std_logic_vector(1 downto 0) := "00";
constant AXI_SLVERR		     : std_logic_vector(1 downto 0) := "10";

type handshake_type is (pending, confirm);
type arbiter_type is (s_axi_read, s_axi_write, t_axi_read, t_axi_read_seq, none);

signal s_axi_aw_state, s_axi_w_state, s_axi_ar_state, t_axi_ar_state : handshake_type;
signal s_axi_aw_state_n, s_axi_w_state_n, s_axi_ar_state_n, t_axi_ar_state_n : handshake_type;
signal arbiter : arbiter_type;
signal s_axi_wlanes, s_axi_rlanes : STD_LOGIC_VECTOR(3 DOWNTO 2);
signal t_axi_araddr_r : STD_LOGIC_VECTOR(31 DOWNTO 0);
signal t_axi_arlen_r :  STD_LOGIC_VECTOR(7 downto 0);
signal t_axi_arlen_r1 : STD_LOGIC_VECTOR(7 downto 0);
signal s_axi_arlen_r1 : STD_LOGIC_VECTOR(0 downto 0);

begin


-- MIG7 overall control
sys_clk_i <= CLK200MHZ;
sys_rst <= not RESET;
app_sr_req <= '0';
app_ref_req <= '0';
app_zq_req <= '0';

-- registered signals
process 
begin
	wait until rising_edge(CLK100MHZ);
	if RESET = '1' or init_calib_complete = '0' then
		s_axi_aw_state <= pending;
		s_axi_w_state <= pending;
		s_axi_ar_state <= pending;
		t_axi_ar_state <= pending;		
		s_axi_rlanes <= "00";
		t_axi_araddr_r <= (others =>'0');
		t_axi_arlen_r <= (others=>'0');
		t_axi_arlen_r1 <= (others=>'0');
		s_axi_arlen_r1 <= (others=>'0');
	else
		s_axi_aw_state <= s_axi_aw_state_n;
		s_axi_w_state <= s_axi_w_state_n;	
		s_axi_ar_state <= s_axi_ar_state_n;
		t_axi_ar_state <= t_axi_ar_state_n;

		-- recursive state machine logic
		if arbiter = s_axi_read  and app_rdy = '1' then
			s_axi_rlanes <= s_axi_araddr(3 downto 2);				-- register the address lo bits so that the proper data can be transferred from
		end if;									-- the 128bit MIG7 bus to 32bit AXI4 bus when it is received
		
		if arbiter = t_axi_read and app_rdy = '1' then				-- t_axi_araddr_r is incremented by 16 bytes (128bits) each read command
			t_axi_araddr_r <= t_axi_araddr + 16;				-- to generate a burst-mode read
		elsif arbiter = t_axi_read_seq and app_rdy = '1' then			-- a read command requires thatthe MIG7 is ready to recieve the command
			t_axi_araddr_r <= t_axi_araddr_r + 16;				-- and that the arbiter is prioritizing it
		end if;
		
		if arbiter = t_axi_read and app_rdy = '1' then				-- t_axi_arlen_r is a counter that decemented with each successful read command
			t_axi_arlen_r <= t_axi_arlen;					-- during a burst.  The arbiter will maintain the burst state whilst the counter
		elsif arbiter = t_axi_read_seq and app_rdy = '1' then			-- is non-zero.  Note that in AXI convention axi_arlen = 0 indicates a burst
			t_axi_arlen_r <= t_axi_arlen_r - 1;				-- length of 1 beat
		end if;
		
		if arbiter = t_axi_read and app_rdy = '1' then 				-- t_axi_arlen_r1 is a counter that is decremented with each read receipt
			t_axi_arlen_r1 <= t_axi_arlen + 1;					-- during a burst.  The arbiter will block any other read requests until 
		elsif t_axi_arlen_r1 /= 0 and app_rd_data_valid = '1' then		-- all of the data for this channel has arrived.  This is very sub-optimal
			t_axi_arlen_r1 <= t_axi_arlen_r1 - 1;				-- but simplifies channel signalling on read data receipt
		end if;									-- an improvement will be made at a later stage
	
		if arbiter = s_axi_read and app_rdy = '1' then 				-- as for the t channel, but as the s channel is AXI4 lite, the burst
			s_axi_arlen_r1 <= "1";						-- size is always 1
		elsif s_axi_arlen_r1 ="1" and app_rd_data_valid = '1' then
			s_axi_arlen_r1 <= "0";
		end if;	
		
	end if;
end process;

-- Arbitrator
	-- this logic selects one of the AXI4 channels that may command the slave, should multiple channels issue a command at the same time
	-- t_axi_read_seq is a synthetic channel that generates all of the addresses required for a burst following a t_axi_read request
	-- The present arbitration is very sub-optimal because the arbiter blocks during the latent period between a read-request and
	-- provision of the read data. An improvement is planned at a later stage
process (t_axi_arlen_r, s_axi_awvalid, s_axi_wvalid, s_axi_arvalid, t_axi_arvalid, init_calib_complete, t_axi_arlen_r1, s_axi_arlen_r1)
begin
	if init_calib_complete = '1' then						-- MIG7 cannot accept commands until calibration completes (~60us)
		if t_axi_arlen_r /= 0 then						-- if a burst read on channel t is being processed, do not interrupt it
			arbiter <= t_axi_read_seq;						-- this logic may be changed at a later date to prioritize the CPU
		elsif s_axi_arvalid = '1' and t_axi_arlen_r1 = 0 then		-- s channel read is highest priority
			arbiter <= s_axi_read;
		elsif s_axi_awvalid = '1' and s_axi_wvalid = '1' then		-- accept s channel write only when the address and the data are both valid
			arbiter <= s_axi_write;
		elsif t_axi_arvalid = '1' and s_axi_arlen_r1 = 0 then		-- if a read on channel s has not yet retured, do not initiate a read on channel t
			arbiter <= t_axi_read;
		else 
			arbiter <= none;
		end if;
	else
		arbiter <= none;
	end if;
end process;

-- AXI4 handshake logic
	-- each AXI4 channel that commands the slave has a small state machine here for handshaking
	-- It is responsible for signalling READY in response to VAILD provided that 
	-- (i) the arbiter is allowing this channel to proceed and (ii) that the MIG7 has accepted the relvant command  
process (s_axi_aw_state, s_axi_awvalid, app_rdy, s_axi_w_state, s_axi_wvalid, app_wdf_rdy, t_axi_ar_state, t_axi_arvalid, arbiter)
begin
	-- s_axi_aw
	case s_axi_aw_state is
	when pending =>
		if s_axi_awvalid = '1' and app_rdy = '1' and arbiter = s_axi_write then
			s_axi_aw_state_n <= confirm;
		else
			s_axi_aw_state_n <= pending;
		end if;
	when confirm =>
		s_axi_aw_state_n <= pending;
	end case;

	-- s_axi_w
	case s_axi_w_state is
	when pending =>
		if s_axi_wvalid = '1' and app_wdf_rdy = '1' and arbiter = s_axi_write then
			s_axi_w_state_n <= confirm;
		else
			s_axi_w_state_n <= pending;
		end if;
	when confirm =>										
		s_axi_w_state_n <= pending;
	end case;	
	
	-- s_axi_ar
	case s_axi_ar_state is
	when pending =>
		if s_axi_arvalid = '1' and app_rdy = '1' and arbiter = s_axi_read then
			s_axi_ar_state_n <= confirm;
		else
			s_axi_ar_state_n <= pending;
		end if;
	when confirm =>
		s_axi_ar_state_n <= pending;
	end case;	
	
	-- t_axi_ar
	case t_axi_ar_state is
	when pending =>
		if t_axi_arvalid = '1' and app_rdy = '1' and arbiter = t_axi_read then
			t_axi_ar_state_n <= confirm;
		else
			t_axi_ar_state_n <= pending;
		end if;
	when confirm =>
		t_axi_ar_state_n <= pending;
	end case;	
end process;

with s_axi_aw_state select s_axi_awready <= '1' when confirm, '0' when others;
with s_axi_w_state  select s_axi_wready  <= '1' when confirm, '0' when others;		-- aw and w may signal ready separately.  need to fix control unit for this!
with s_axi_ar_state select s_axi_arready <= '1' when confirm, '0' when others;
with t_axi_ar_state select t_axi_arready <= '1' when confirm, '0' when others;

-- write response channel should be removed as it does not have any purpose
--s_axi_bresp <= AXI_OKAY;
--s_axi_bvalid <= '1';

-- s read channel
with s_axi_arlen_r1 select s_axi_rvalid <=  '0' when "0", app_rd_data_valid when others;	-- if this channel is expecting data, then pass through data-valid signals
-- read response should be removed as it does not have any purpose
--s_axi_rresp <= AXI_OKAY;
with s_axi_rlanes select										-- place data onto 32bit bus according to lo bits of original read address
	s_axi_rdata <= 	app_rd_data(31 downto 0)	when "00",
				app_rd_data(63 downto 31)	when "01",
				app_rd_data(95 downto 64)	when "10",
				app_rd_data(127 downto 96)	when others;

-- t channel read				
with t_axi_arlen_r1 select t_axi_rvalid <= '0' when "00000000", app_rd_data_valid when others;				
--t_axi_rresp <= AXI_OKAY;	
t_axi_rdata <= app_rd_data;
t_axi_rlast <= '1' when t_axi_arlen_r1 = 1 and app_rd_data_valid = '1' else '0';		-- signal last beat	

-- MIG user interface
with arbiter select app_cmd <= MIG_WRITE when s_axi_write, MIG_READ when others;

with arbiter select app_addr(26 downto 4) <= s_axi_awaddr(26 downto 4) when s_axi_write,
						s_axi_araddr(26 downto 4) when s_axi_read,
						t_axi_araddr(26 downto 4) when t_axi_read,
			   			t_axi_araddr_r(26 downto 4) when others;
			   	
app_addr (3 downto 0) <= "0000";

app_en <= '1' when 	(arbiter = s_axi_write and s_axi_aw_state = pending) or			-- enable a command to the MIG7 when arbiter indictes a channel request and it has not yet been satisfied
			(arbiter = s_axi_read and s_axi_ar_state = pending) or
			(arbiter = t_axi_read and t_axi_ar_state = pending) or
			(arbiter = t_axi_read_seq)
		else 	'0';
		
s_axi_wlanes <= s_axi_awaddr(3 downto 2);								-- set write mask of 128bit MIG7 channel according to lo bits of
with s_axi_wlanes select										-- address write, and also invert the byte strobe into a mask
	app_wdf_mask <=	"111111111111" & not s_axi_wstrb 		when "00",
				"11111111" & not s_axi_wstrb & "1111" 	when "01",
				"1111" & not s_axi_wstrb & "11111111" 	when "10",
				not s_axi_wstrb & "111111111111"		when others;
				
app_wdf_data <= s_axi_wdata & s_axi_wdata & s_axi_wdata & s_axi_wdata;			-- replicate 32bit write data onto 128bit bus and mask accordingly
	
app_wdf_wren <= '1' when arbiter = s_axi_write and s_axi_w_state = pending else '0';	-- enable a write to the MIG7 when arbiter indictes a channel request and it has not yet been satisfied
app_wdf_end <= '1' when arbiter = s_axi_write and s_axi_w_state = pending else '0';


inst_MIG7: MIG7
  port map (
     ddr2_dq              => ddr2_dq,
     ddr2_dqs_p           => ddr2_dqs_p,
     ddr2_dqs_n           => ddr2_dqs_n,
     ddr2_addr            => ddr2_addr,
     ddr2_ba              => ddr2_ba,
     ddr2_ras_n           => ddr2_ras_n,
     ddr2_cas_n           => ddr2_cas_n,
     ddr2_we_n            => ddr2_we_n,
     ddr2_ck_p            => ddr2_ck_p,
     ddr2_ck_n            => ddr2_ck_n,
     ddr2_cke             => ddr2_cke,
     ddr2_cs_n            => ddr2_cs_n,
     ddr2_dm              => ddr2_dm,
     ddr2_odt             => ddr2_odt,
     sys_clk_i 	     => sys_clk_i,
     app_addr             => app_addr,
     app_cmd              => app_cmd,
     app_en               => app_en,
     app_wdf_data         => app_wdf_data,
     app_wdf_end          => app_wdf_end,
     app_wdf_mask         => app_wdf_mask,
     app_wdf_wren         => app_wdf_wren,
     app_rd_data          => app_rd_data,
     app_rd_data_end      => app_rd_data_end,
     app_rd_data_valid    => app_rd_data_valid,
     app_rdy              => app_rdy,
     app_wdf_rdy          => app_wdf_rdy,
     app_sr_req           => app_sr_req,
     app_sr_active        => app_sr_active,
     app_ref_req          => app_ref_req,
     app_ref_ack          => app_ref_ack,
     app_zq_req           => app_zq_req,
     app_zq_ack           => app_zq_ack,
     ui_clk               => ui_clk,
     ui_clk_sync_rst      => ui_clk_sync_rst,    
     init_calib_complete  => init_calib_complete,
     sys_rst => sys_rst
     );

end RTL;

