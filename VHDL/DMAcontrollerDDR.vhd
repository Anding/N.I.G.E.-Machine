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
	s_axi_awvalid : IN STD_LOGIC;						-- handshake protocol: source indicates channel data valid
	s_axi_awready : OUT STD_LOGIC;						-- destination indicates that it can accept channel data
														-- transfer occurs only when valid and ready are high
														-- destination may hold ready high or wait for valid
														-- source may not wait for ready to assert valid
	s_axi_awaddr : IN STD_LOGIC_VECTOR(31 DOWNTO 0);	-- true byte addressing											
	-- write channel
	s_axi_wdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
	s_axi_wstrb : IN STD_LOGIC_VECTOR(3 DOWNTO 0);		-- active high byte mask for the data	
	s_axi_wvalid : IN STD_LOGIC;
	s_axi_wready : OUT STD_LOGIC;
	-- address of read channel
	s_axi_araddr : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
	s_axi_arvalid : IN STD_LOGIC;
	s_axi_arready : OUT STD_LOGIC;
	-- read/read-response channel
	s_axi_rdata : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
	s_axi_rvalid : OUT STD_LOGIC;						-- RVALID is dependent on ARVALID and ARREADY
	
	-- AIX4 read channel connections (as seen by SLAVE)-------------  't' channel is connected to the VGA buffer
	-- address of read channel
	t_axi_araddr : IN  std_logic_vector(31 downto 0);		-- true byte addressing	
															-- MASTER indicates only the first byte address, SLAVE will calculate the rest
	t_axi_arlen : IN  std_logic_vector(7 downto 0);			-- No. beats = axi_arlen + 1
	t_axi_arvalid : IN  std_logic;
	t_axi_arready : OUT  std_logic;
	t_axi_rdata : OUT  std_logic_vector(15 downto 0);	
	t_axi_rlast : OUT  std_logic;					-- set high to indicate last data word
	t_axi_rvalid : OUT  std_logic;

	-- DDR SRDRAM controller -------------------------------------
	wrrd_ba_add	: out std_logic_vector(2 downto 0);
	wrrd_ras_add : out std_logic_vector(12 downto 0);
	wrrd_cas_add : out std_logic_vector(8 downto 0);
	wr_we		: out std_logic_vector(3 downto 0);
	wr_dat		: out std_logic_vector(31 downto 0);
	wr_ack		: in std_logic;
	rd_re		: out std_logic;
	rd_dat		: in std_logic_vector(63 downto 0);
	rd_ack		: in std_logic;
	rd_valid	: in std_logic
	);			
end DMAcontrollerDDR;

-- There are a number of deviations from the AXI4 protocol as follows:
-- 1. The write response channel axi_b is removed
-- 2. The read responese signals axi_rresp are removed
-- 3. Read channels are assumed to be always ready after a read request; axi_rready signals are removed
--	this saves implementing additional buffers in the DMA controller
-- 4. The burst size axi_arsize is removed since it is assumed that all bursts are the full width of the databus
-- 5. The burst type axi_abburst is removes since it is assumed that alll bursts are AXI_INCR
-- 6. Not a protocol deviation, but the controller will not affirm either of axi_awready or axi_wready until
--	both axi_awvalid and axi_wvalid are affirmed by the master

architecture RTL of DMAcontrollerDDR is

type arbiter_type is (s_axi_read, s_axi_write, t_axi_read, t_axi_read_seq, none);
type sequencer_type is (idle, read_request, read_wait, output, finish);

signal arbiter, arbiter_n : arbiter_type;
signal sequencer, sequencer_n : sequencer_type;
signal sequencer_rd_re : std_logic;
signal sequencer_rvalid : std_logic;
signal sequencer_rlast : std_logic;
signal sequencer_arready : std_logic;
signal sequencer_addr,  sequencer_addr_n : std_logic_vector(31 downto 0);
signal sequencer_count, sequencer_count_n : std_logic_vector(7 downto 0);
signal sequencer_bytes, sequencer_bytes_n : std_logic_vector(1 downto 0);
signal sequencer_data, sequencer_data_n : std_logic_vector(63 downto 0);

begin

-- registered signals
process 
begin
	wait until rising_edge(CLK);
	if RESET = '1' then	
		arbiter <= none;	
		sequencer <= idle;
		sequencer_addr <= (others=>'0');
		sequencer_count <= (others=>'0');
		sequencer_bytes <= (others=>'0');
		sequencer_data <= (others=>'0');
	else
		arbiter <= arbiter_n;		
		sequencer <= sequencer_n;
		sequencer_addr <= sequencer_addr_n;
		sequencer_count <= sequencer_count_n;
		sequencer_bytes <= sequencer_bytes_n;
		sequencer_data <= sequencer_data_n;
	end if;
end process;

-- arbiter next state process
process (arbiter, rd_ack, rd_valid, wr_ack, s_axi_awvalid, s_axi_wvalid, s_axi_arvalid, t_axi_arvalid, sequencer)
begin
	arbiter_n <= arbiter;
	case arbiter is
		when none =>
			if s_axi_awvalid = '1' and s_axi_wvalid = '1' then
				arbiter_n <= s_axi_write;
			elsif s_axi_arvalid = '1' then
				arbiter_n <= s_axi_read;
			elsif t_axi_arvalid = '1' then
				arbiter_n <= t_axi_read;
			end if;
		
		when s_axi_write =>
			if wr_ack = '1' then 
				arbiter_n <= none;
			end if;
			
		when s_axi_read =>
			if rd_valid = '1' then
				arbiter_n <= none;
			end if;
			
		when others =>							-- t_axi_read
			if sequencer = finish then
				arbiter_n <= none;
			end if;
			
	end case ;
end process;

-- arbiter output process
process (rd_dat, s_axi_awaddr, s_axi_wdata, wr_ack, s_axi_awvalid, s_axi_wvalid, s_axi_wstrb, rd_ack, rd_valid, s_axi_arvalid, s_axi_araddr, arbiter,
		sequencer_arready, sequencer_rvalid, sequencer_data, sequencer_rlast, sequencer_rd_re, sequencer_addr)
begin
	case arbiter is
		when none =>
			s_axi_awready	<= '0';
			s_axi_wready	<= '0';
			s_axi_arready	<= '0';
			s_axi_rdata 	<= rd_dat(31 downto 0);
			s_axi_rvalid	<= '0';
			t_axi_arready	<= '0';
			t_axi_rvalid	<= '0';
			t_axi_rdata		<= sequencer_data(15 downto 0);
			t_axi_rlast		<= '0';
			rd_re			<= '0';
			wr_we			<= (others=>'0');
			wrrd_cas_add	<= s_axi_awaddr(9 downto 1);
			wrrd_ras_add	<= s_axi_awaddr(22 downto 10);
			wrrd_ba_add		<= s_axi_awaddr(25 downto 23);
			wr_dat			<= s_axi_wdata;
			
		when s_axi_write =>
			s_axi_awready	<= wr_ack;
			s_axi_wready	<= wr_ack;
			s_axi_arready	<= '0';
			s_axi_rdata 	<= rd_dat(31 downto 0);
			s_axi_rvalid	<= '0';
			t_axi_arready	<= '0';
			t_axi_rvalid	<= '0';
			t_axi_rdata		<= sequencer_data(15 downto 0);
			t_axi_rlast		<= '0';
			rd_re			<= '0';
			if (s_axi_awvalid = '1' and s_axi_wvalid = '1') then
				wr_we		<= s_axi_wstrb;
			else
				wr_we		<= (others=>'0');
			end if;				
			wrrd_cas_add	<= s_axi_awaddr(9 downto 1);
			wrrd_ras_add	<= s_axi_awaddr(22 downto 10);
			wrrd_ba_add		<= s_axi_awaddr(25 downto 23);
			wr_dat			<= s_axi_wdata;	
			
		when s_axi_read =>
			s_axi_awready	<= '0';
			s_axi_wready	<= '0';
			s_axi_arready	<= rd_ack;
			s_axi_rdata 	<= rd_dat(31 downto 0);
			s_axi_rvalid	<= rd_valid;
			t_axi_arready	<= '0';
			t_axi_rvalid	<= '0';
			t_axi_rdata		<= sequencer_data(15 downto 0);
			t_axi_rlast		<= '0';
			rd_re			<= s_axi_arvalid;
			wr_we			<= (others=>'0');
			wrrd_cas_add	<= s_axi_araddr(9 downto 1);
			wrrd_ras_add	<= s_axi_araddr(22 downto 10);
			wrrd_ba_add		<= s_axi_araddr(25 downto 23);
			wr_dat			<= s_axi_wdata;		

		when others =>											-- t_axi_read
			s_axi_awready	<= '0';
			s_axi_wready	<= '0';
			s_axi_arready	<= '0';
			s_axi_rdata 	<= rd_dat(31 downto 0);
			s_axi_rvalid	<= '0';
			t_axi_arready	<= sequencer_arready;
			t_axi_rvalid	<= sequencer_rvalid;
			t_axi_rdata		<= sequencer_data(15 downto 0);
			t_axi_rlast		<= sequencer_rlast;
			rd_re			<= sequencer_rd_re;
			wr_we			<= (others=>'0');
			wrrd_cas_add	<= sequencer_addr(9 downto 1);
			wrrd_ras_add	<= sequencer_addr(22 downto 10);
			wrrd_ba_add		<= sequencer_addr(25 downto 23);
			wr_dat			<= s_axi_wdata;					
			
		end case;
end process;

-- sequencer next state process
process (sequencer, arbiter, rd_ack, rd_valid, sequencer_count, sequencer_bytes)
begin
	sequencer_n <= sequencer;
	case sequencer is
	
	when idle =>
		if arbiter = t_axi_read then
			sequencer_n <= read_request;
		end if;
		
	when read_request =>
		if rd_ack = '1' then 
			sequencer_n <= read_wait;
		end if;
		
	when read_wait =>
		if rd_valid = '1' then 
			sequencer_n <= output;
		end if;
		
	when output =>
		if sequencer_count = 0 then
			sequencer_n <= finish;
		elsif sequencer_bytes = 0 then
			sequencer_n <= read_request;
		end if;
		
	when others =>							-- finish
		sequencer_n <= idle;

	end case;
end process;

-- sequencer output process
process (sequencer, sequencer_addr, sequencer_count, sequencer_bytes, sequencer_data, t_axi_araddr, t_axi_arlen, rd_dat, rd_valid)
begin

	sequencer_rd_re <= '0';
	sequencer_rvalid <= '0';
	sequencer_rlast <= '0';
	sequencer_arready <= '0';
	sequencer_addr_n <= sequencer_addr;
	sequencer_count_n <= sequencer_count;
	sequencer_bytes_n <= sequencer_bytes;
	sequencer_data_n <= sequencer_data;
	
	case sequencer is
	
	when idle =>
		sequencer_addr_n <= t_axi_araddr;
		sequencer_count_n <= t_axi_arlen;
		
	when read_request =>
		sequencer_rd_re <= '1';
		sequencer_arready <= '1';
		sequencer_bytes_n <= "11";
				
	when read_wait =>
		if rd_valid = '1' then 
			sequencer_data_n <= rd_dat;
		end if;
		
	when output =>
		sequencer_rvalid <= '1';
		sequencer_addr_n <= sequencer_addr + 2;
		sequencer_count_n <= sequencer_count - 1;
		sequencer_bytes_n <= sequencer_bytes - 1;
		sequencer_data_n <= "0000000000000000" & sequencer_data(63 downto 16);
		if sequencer_count = 0 then
			sequencer_rlast <= '1';
		end if;
		
	when others	=>							-- finish
		null;
		
	end case;
end process;
	
end RTL;

