library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;

Library UNISIM;
use UNISIM.vcomponents.all;

entity DMAcontrollerDDR is
	Port (
	CLK	: IN STD_LOGIC;
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
	-- DDR SRDRAM controller -------------------------------------
	wrrd_ba_add	: out std_logic_vector(2 downto 0);
	wrrd_ras_add : out std_logic_vector(12 downto 0);
	wrrd_cas_add : out std_logic_vector(8 downto 0);
	wr_we		: out std_logic_vector(3 downto 0);
	wr_dat		: out std_logic_vector(31 downto 0);
	wr_ack		: in std_logic;
	rd_re		: out std_logic;
	rd_dat		: in std_logic_vector(31 downto 0);
	rd_ack		: in std_logic;
	rd_valid	: in std_logic
	);			
end DMAcontrollerDDR;

-- There are a number of deviations from the AXI4 protocol as follows:
-- 1. The write response channel axi_b is removed since the MIG7 does not confirm a write response itself
-- 2. The read responese signals axi_rresp are removed since the MIG7 does not confirm a read response itself
-- 3. Read channels are assumed to be always ready after a read request; axi_rready signals are removed
--	this saves implementing additional buffers in the DMA controller
-- 4. The burst size axi_arsize is removed since it is assumed that all bursts are the full width of the databus
-- 5. The burst type axi_abburst is removes since it is assumed that alll bursts are AXI_INCR
-- 6. Not a protocol deviation, but the controller will not affirm either of axi_awready or axi_wready until
--	both axi_awvalid and axi_wvalid are affirmed by the master

architecture RTL of DMAcontrollerDDR is

type handshake_type is (pending, confirm);
type arbiter_type is (s_axi_read, s_axi_write, t_axi_read, t_axi_read_seq, none);

signal s_axi_aw_state, s_axi_w_state, s_axi_ar_state, t_axi_ar_state : handshake_type;
signal s_axi_aw_state_n, s_axi_w_state_n, s_axi_ar_state_n, t_axi_ar_state_n : handshake_type;
signal arbiter : arbiter_type;


begin

-- registered signals
process 
begin
	wait until rising_edge(CLK);
	if RESET = '1' then
		s_axi_aw_state <= pending;
		s_axi_w_state <= pending;
		s_axi_ar_state <= pending;
		t_axi_ar_state <= pending;		
	else
		s_axi_aw_state <= s_axi_aw_state_n;
		s_axi_w_state <= s_axi_w_state_n;	
		s_axi_ar_state <= s_axi_ar_state_n;
		t_axi_ar_state <= t_axi_ar_state_n;		
	end if;
end process;

-- Arbitrator
	-- this logic selects one of the AXI4 channels that may command the slave, should multiple channels issue a command at the same time
	-- t_axi_read_seq is a synthetic channel that generates all of the addresses required for a burst following a t_axi_read request
	-- The present arbitration is very sub-optimal because the arbiter blocks during the latent period between a read-request and
	-- provision of the read data. An improvement is planned at a later stage


-- AXI4 handshake logic
	-- each AXI4 channel that commands the slave has a small state machine here for handshaking
	-- It is responsible for signalling READY in response to VAILD provided that 
	-- (i) the arbiter is allowing this channel to proceed and (ii) that the MIG7 has accepted the relvant command  

with s_axi_aw_state select s_axi_awready <= '1' when confirm, '0' when others;
with s_axi_w_state  select s_axi_wready  <= '1' when confirm, '0' when others;		-- aw and w may signal ready separately.  need to fix control unit for this!
with s_axi_ar_state select s_axi_arready <= '1' when confirm, '0' when others;
with t_axi_ar_state select t_axi_arready <= '1' when confirm, '0' when others;

end RTL;

