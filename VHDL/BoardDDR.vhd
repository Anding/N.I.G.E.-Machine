library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
Library UNISIM;
use UNISIM.vcomponents.all;

entity Board_Nexys4DDR is

Generic (	
	vmp_w : integer := 5;
	psp_w : integer := 8;
	rsp_w : integer := 7;
	ssp_w : integer := 7;
	esp_w : integer := 4
	);
					
Port ( 	
	CLK_IN : in  STD_LOGIC;
	RGB : out  STD_LOGIC_VECTOR (11 downto 0);
	HSync : out  STD_LOGIC;
	VSync : out  STD_LOGIC;
	-- DDR RAM
	ddr2_dq	: inout std_logic_vector(15 downto 0);
	ddr2_dqs_p	: inout std_logic_vector(1 downto 0);
	ddr2_dqs_n	: inout std_logic_vector(1 downto 0);
	ddr2_addr	: out   std_logic_vector(12 downto 0);
	ddr2_ba	: out   std_logic_vector(2 downto 0);
	ddr2_ras_n	: out   std_logic;
	ddr2_cas_n	: out   std_logic;
	ddr2_we_n	: out   std_logic;
	ddr2_ck_p	: out   std_logic_vector(0 downto 0);
	ddr2_ck_n	: out   std_logic_vector(0 downto 0);
	ddr2_cke	: out   std_logic_vector(0 downto 0);
	ddr2_cs_n	: out   std_logic_vector(0 downto 0);
	ddr2_dm	: out   std_logic_vector(1 downto 0);
	ddr2_odt	: out   std_logic_vector(0 downto 0);
	-- RS232
	RXD_S0 : in STD_LOGIC;
	TXD_S0 : out STD_LOGIC;
	-- PS/2 keyboard
	PS2C : in STD_LOGIC;
	PS2D : in STD_LOGIC;
	-- Board
	SW : in STD_LOGIC_VECTOR (15 downto 0);
	sevenseg : out STD_LOGIC_VECTOR (6 downto 0);
	anode : out STD_LOGIC_VECTOR (7 downto 0);	
	CPUreset : in STD_LOGIC;
	RGB1_Red : out STD_LOGIC;
	RGB1_Green : out STD_LOGIC;
	RGB1_Blue : out STD_LOGIC;
	-- SPI
	SCK : out STD_LOGIC;
	MOSI : out STD_LOGIC;
	MISO : in STD_LOGIC;
	SD_CS : out STD_LOGIC;
	SD_CD : in STD_LOGIC;
	-- Ethernet
	PHYMDC : out  STD_LOGIC;
	PHYMDIO : inout  STD_LOGIC;
	PHYRSTN : out  STD_LOGIC;
	PHYCRS : in  STD_LOGIC;
	PHYRXERR : in  STD_LOGIC;
	PHYRXD : in  STD_LOGIC_VECTOR (1 downto 0);
	PHYTXEN : out  STD_LOGIC;
	PHYTXD : out  STD_LOGIC_VECTOR (1 downto 0);
	PHYCLK50MHZ : out  STD_LOGIC;
	PHYINTN : in  STD_LOGIC;
	JB : out  STD_LOGIC_VECTOR (7 downto 0);
	--SD_WP : In STD_LOGIC
	SD_RESET : out STD_LOGIC
	);
end Board_Nexys4DDR;

architecture RTL of Board_Nexys4DDR is

type bank_t is (Sys, Char, Color, Reg, Stacks, User, Vir);
constant blank : std_logic_vector(31 downto 0) := (others =>'0');
constant l : std_logic := '1';
constant o : std_logic_vector(0 downto 0) := "0";

signal SD_WP : std_logic;
signal bank, bank_n : bank_t;	
signal counter_clk, counter_ms : std_logic_vector(31 downto 0) := (others =>'0');
signal timer_ms : std_logic_vector(31 downto 0) := (others =>'0');	
signal reset, invReset, trig : std_logic;
signal VGAclk25, VGAclk50, VGAclk75, VGAclk150, clk100, CLK200MHZ: std_logic;
signal irq, rti, ms_irq : std_logic;
signal irv : std_logic_vector(3 downto 0);
signal irq_mask : std_logic_vector(15 downto 1);
signal PSdatain :  std_logic_vector(31 downto 0);
signal RSdatain :  std_logic_vector(31 downto 0);
signal MEMdatain_Xi :  std_logic_vector(31 downto 0);
signal MEMdata_Char :  std_logic_vector(15 downto 0);
signal MEMdata_Color :  std_logic_vector(15 downto 0);
signal MEMdata_Pstack, MEMdata_Rstack, MEMdata_Reg, MEMdata_stack_access : std_logic_vector(31 downto 0);   
signal MEMdata_User :  std_logic_vector(31 downto 0);      
signal PSaddr :  std_logic_vector(vmp_w + psp_w -1 downto 0);
signal PSdataout :  std_logic_vector(31 downto 0);
signal PSw :  std_logic_vector(0 to 0);
signal RSaddr :  std_logic_vector(vmp_w + rsp_w -1 downto 0);
signal RSdataout :  std_logic_vector(31 downto 0);
signal RSw :  std_logic_vector(0 to 0);
signal MEMaddr :  std_logic_vector(31 downto 0);
signal MEMdataout_X :  std_logic_vector(31 downto 0);
signal MEM_WRQ_X :  std_logic;
signal MEM_WRQ_XX : std_logic_vector(0 downto 0);
signal Sys_EN, Pstack_EN, Rstack_EN, Char_EN, Reg_EN, Color_EN, stack_access_EN, User_EN : std_logic;
signal txt_zero : std_logic_vector(23 downto 0);
signal DATA_OUT_VGA : std_logic_vector(7 downto 0) := (others=>'0');
signal ADDR_VGA : std_logic_vector(8 downto 0);
signal DATA_TEXT : std_logic_vector(15 downto 0) := (others=>'0');
signal ADDR_TEXT : std_logic_vector(7 downto 0);
signal DATA_Char : std_logic_vector(15 downto 0);
signal ADDR_Char : std_logic_vector(11 downto 0);
signal DATA_Color : std_logic_vector(15 downto 0);
signal ADDR_Color : std_logic_vector(7 downto 0);
signal RS232_TX_S0 : std_logic_vector(7 downto 0);
signal RS232_WR_S0 : std_logic;       
signal RS232_RX_S0 : std_logic_vector(7 downto 0);
signal RS232_RDA_S0 : std_logic;
signal RS232_TBE_S0 : std_logic;
signal RS232_DIVIDE_S0 : std_logic_vector(31 downto 0);
signal Boot_we : STD_LOGIC_VECTOR(0 DOWNTO 0);
signal Boot_data : STD_LOGIC_VECTOR(31 DOWNTO 0);
signal Boot_addr : STD_LOGIC_VECTOR(31 DOWNTO 2);
signal PS2_irq : std_logic;
signal PS2_data : std_logic_vector(7 downto 0);
signal mode : STD_LOGIC_VECTOR (4 downto 0);		
signal background : STD_LOGIC_VECTOR (15 downto 0);
signal ssData	: STD_LOGIC_VECTOR (31 downto 0);
signal CLKSPI, SD_wr : STD_LOGIC;
signal SD_dataout, SD_datain, SD_divide : STD_LOGIC_VECTOR (7 downto 0);
signal SD_status : STD_LOGIC_VECTOR (3 downto 0);
signal SD_control : STD_LOGIC_VECTOR (3 downto 0);
signal douta_sysram : std_logic_vector(31 downto 0);
signal doutb_sysram : std_logic_vector(31 downto 0);   
signal douta_sysram_r : std_logic_vector(31 downto 0);
signal doutb_sysram_r : std_logic_vector(31 downto 0); 
signal douta_sysram_i : std_logic_vector(31 downto 0);
signal doutb_sysram_i : std_logic_vector(31 downto 0);         
signal wea_sysram : std_logic_vector(3 downto 0);
signal wea_sysram_s : std_logic_vector(3 downto 0);
signal addra_sysram : std_logic_vector(31 downto 2);
signal addra_sysram_s : std_logic_vector(31 downto 2);
signal dina_sysram : std_logic_vector(31 downto 0);
signal dina_sysram_s : std_logic_vector(31 downto 0);
signal web_sysram : std_logic_vector(3 downto 0);
signal addrb_sysram : std_logic_vector(31 downto 2);
signal dinb_sysram : std_logic_vector(31 downto 0);
signal ena_sysram, enb_sysram : std_logic;
signal addra_userram : std_logic_vector(31 downto 2);
signal douta_userram : std_logic_vector(31 downto 0);
signal doutb_userram : std_logic_vector(31 downto 0);          
signal wea_userram : std_logic_vector(3 downto 0);
signal addrb_userram : std_logic_vector(31 downto 2);
signal dina_userram : std_logic_vector(31 downto 0);
signal dinb_userram : std_logic_vector(31 downto 0);
signal web_userram : std_logic_vector(3 downto 0);
signal ena_userram, enb_userram : std_logic;
signal addra_userram_all : std_logic_vector(31 downto 2);
signal addrb_userram_all : std_logic_vector(31 downto 2);
signal MEMdata_Sys, MEMdata_Sys_plus : std_logic_vector(31 downto 0);
signal MEMdata_Sys_quick : std_logic_vector(31 downto 0);
signal MEMsize_X, MEMsize_Xp : std_logic_vector(1 downto 0);
signal ram_en : std_logic;
signal VBLANK : std_logic;
signal s_axi_awaddr : std_logic_vector(31 downto 0);
signal s_axi_awvalid : std_logic;
signal s_axi_wdata : std_logic_vector(31 downto 0);
signal s_axi_wstrb : std_logic_vector(3 downto 0);
signal s_axi_wvalid : std_logic;
signal s_axi_bready : std_logic;
signal s_axi_araddr : std_logic_vector(31 downto 0);
signal s_axi_arvalid : std_logic;
signal s_axi_rready : std_logic;
signal t_axi_araddr : std_logic_vector(31 downto 0);
signal t_axi_arlen : std_logic_vector(7 downto 0);
signal t_axi_arsize : std_logic_vector(2 downto 0);
signal t_axi_arburst : std_logic_vector(1 downto 0);
signal t_axi_arvalid : std_logic;
signal t_axi_rready : std_logic;    
signal s_axi_awready : std_logic;
signal s_axi_wready : std_logic;
signal s_axi_bresp : std_logic_vector(1 downto 0);
signal s_axi_bvalid : std_logic;
signal s_axi_arready : std_logic;
signal s_axi_rdata : std_logic_vector(31 downto 0);
signal s_axi_rresp : std_logic_vector(1 downto 0);
signal s_axi_rvalid : std_logic;
signal t_axi_arready : std_logic;
signal t_axi_rdata : std_logic_vector(127 downto 0);
signal t_axi_rresp : std_logic_vector(1 downto 0);
signal t_axi_rlast : std_logic;
signal t_axi_rvalid : std_logic;
signal s_aresetn : std_logic;
signal VGA_columns : std_logic_vector(7 downto 0);
signal FetchNextRow : std_logic;
signal clk_system : std_logic;
signal clk_VGA : std_logic;
signal clk_MEM : std_logic;
signal debug : std_logic_vector(31 downto 0);
signal debug_CPU : std_logic_vector(7 downto 0);
signal debug_DMAcontroller : std_logic_vector(7 downto 0);
signal SSdataOUT : std_logic_vector(351 downto 0);
signal SSdataIN : std_logic_vector(351 downto 0);
signal SSw : std_logic_vector(43 downto 0);
signal SSaddr : std_logic_vector(vmp_w + ssp_w -1 downto 0);
signal ESdataOUT : std_logic_vector(303 downto 0);
signal ESdataIN : std_logic_vector(303 downto 0);
signal ESw : std_logic_vector(37 downto 0);
signal ESaddr : std_logic_vector(vmp_w + esp_w -1 downto 0);
signal VM : std_logic_vector(vmp_w -1 downto 0);
signal vir_EN : STD_LOGIC;
signal MEMdata_vir : STD_LOGIC_VECTOR(31 DOWNTO 0);
signal blocked : STD_LOGIC;
signal interlace	: STD_LOGIC_VECTOR (3 downto 0);	
signal charHeight: STD_LOGIC_VECTOR (3 downto 0);
signal charWidth: STD_LOGIC_VECTOR (3 downto 0);	
signal VGArows : STD_LOGIC_VECTOR (7 downto 0);					  
signal VGAcols : STD_LOGIC_VECTOR (7 downto 0);
signal MACdataRX, MACdataTX : STD_LOGIC_VECTOR(7 DOWNTO 0);
signal MACreadyRX, MACreadyTX, MACread_enable, MACchecksum_err, MACweTX, MACtransmit_request  : STD_LOGIC;
signal CLK50MHZ : STD_LOGIC;
signal SMIaddr :  std_logic_vector(9 downto 0);
signal SMIdataWrite :  std_logic_vector(15 downto 0);
signal SMIread_request :  std_logic;
signal SMIwrite_request :  std_logic;       
signal SMIdataRead :  std_logic_vector(15 downto 0);
signal SMIready :  std_logic;
signal sys_clk_i		: std_logic;				-- in: 200MHZ CLOCK
signal sys_rst		: std_logic;				-- in: active lo reset
signal app_en               : std_logic;				-- in: user holds app_en high with a valid app_cmd until app_rdy is asserted
signal app_cmd              : std_logic_vector(2 downto 0);	-- in: see VDHL constants
signal app_rdy              : std_logic;				-- out MIG7 registers a command provided rdy is high
signal app_addr             : std_logic_vector(26 downto 0);	-- in: true byte addressing
signal app_wdf_data         : std_logic_vector(127 downto 0);	-- in: 64 bit data since 16 ddr2 lines and 2:1 MIG7 clocking
signal app_wdf_mask         : std_logic_vector(15 downto 0);	-- in: active high byte mask for the data (note this is a mask not a write strobe)	
signal app_wdf_wren         : std_logic;				-- in: user holds high throughout transfer to indicate valid data
signal app_wdf_rdy          : std_logic;				-- out MIG registers data provided rdy is high
signal app_wdf_end          : std_logic;				-- in: set high to indicate last data word
signal app_rd_data          : std_logic_vector(127 downto 0);	-- out
signal app_rd_data_end      : std_logic;				-- out signals end of burst (not needed in handshake logic)
signal app_rd_data_valid    : std_logic;				-- out valid read data is on the bus
signal app_sr_req           : std_logic;				-- in: tie to '0'
signal app_sr_active        : std_logic;				-- out: disregard
signal app_ref_req          : std_logic;				-- in: tie to '0'
signal app_ref_ack          : std_logic;				-- out disregard
signal app_zq_req           : std_logic;				-- in: tie to '0'
signal app_zq_ack           : std_logic;				-- out disregard
signal ui_clk               : std_logic;				-- out CLOCK
signal ui_clk_sync_rst      : std_logic;				-- out reset	
signal init_calib_complete  : std_logic;				-- out MIG7 requires 50-60uS to complete calibraton in simulator


component CLOCKMANAGER
port (	-- Clock in ports
	CLK_IN1	: in     std_logic;
	-- Clock out ports
	CLK_OUT1	: out    std_logic;
	CLK_OUT2	: out    std_logic;
	CLK_OUT3	: out    std_logic;
	CLK_OUT4	: out    std_logic;
	CLK_OUT5	: out    std_logic;
	CLK_OUT6	: out    std_logic;
	CLK_OUT7	: out	  std_logic
 );
end component;

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

COMPONENT SYS_RAM
  PORT (
    clka : IN STD_LOGIC;
    ena : IN STD_LOGIC;
    wea : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
    addra : IN STD_LOGIC_VECTOR(14 DOWNTO 0);
    dina : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
    douta : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
    clkb : IN STD_LOGIC;
    enb : IN STD_LOGIC;
    web : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
    addrb : IN STD_LOGIC_VECTOR(14 DOWNTO 0);
    dinb : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
    doutb : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
  );
END COMPONENT;

COMPONENT RAM_for_Testbench
PORT(
	rst : IN std_logic;
	clk : IN std_logic;
	weA : IN std_logic_vector(3 downto 0);
	weB : IN std_logic_vector(3 downto 0);
	enA : IN std_logic;
	enB : IN std_logic;
	addressA : IN std_logic_vector(16 downto 2);
	data_inA : IN std_logic_vector(31 downto 0);
	addressB : IN std_logic_vector(16 downto 2);
	data_inB : IN std_logic_vector(31 downto 0);          
	data_outA : OUT std_logic_vector(31 downto 0);
	data_outB : OUT std_logic_vector(31 downto 0)
	);
END COMPONENT;

COMPONENT DMAcontrollerMIG7
PORT(
	CLK100MHZ	: IN STD_LOGIC;
	CLK200MHZ	: IN STD_LOGIC;
	reset : IN std_logic;
	s_axi_awaddr : IN std_logic_vector(31 downto 0);
	s_axi_awvalid : IN std_logic;
	s_axi_wdata : IN std_logic_vector(31 downto 0);
	s_axi_wstrb : IN std_logic_vector(3 downto 0);
	s_axi_wvalid : IN std_logic;
	s_axi_araddr : IN std_logic_vector(31 downto 0);
	s_axi_arvalid : IN std_logic;
	t_axi_araddr : IN std_logic_vector(31 downto 0);
	t_axi_arlen : IN std_logic_vector(7 downto 0);
	t_axi_arvalid : IN std_logic;   
	s_axi_awready : OUT std_logic;
	s_axi_wready : OUT std_logic;
	s_axi_arready : OUT std_logic;
	s_axi_rdata : OUT std_logic_vector(31 downto 0);
	s_axi_rvalid : OUT std_logic;
	t_axi_arready : OUT std_logic;
	t_axi_rdata : OUT std_logic_vector(127 downto 0);
	t_axi_rlast : OUT std_logic;
	t_axi_rvalid : OUT std_logic;
      app_addr             : out	std_logic_vector(26 downto 0);
      app_cmd              : out    std_logic_vector(2 downto 0);
      app_en               : out    std_logic;
      app_wdf_data         : out   std_logic_vector(127 downto 0);
      app_wdf_end          : out    std_logic;
      app_wdf_mask         : out    std_logic_vector(15 downto 0);
      app_wdf_wren         : out    std_logic;
      app_rd_data          : in   std_logic_vector(127 downto 0);
      app_rd_data_end      : in   std_logic;
      app_rd_data_valid    : in   std_logic;
      app_rdy              : in   std_logic;
      app_wdf_rdy          : in   std_logic;
      app_sr_req           : out    std_logic;
      app_sr_active        : in   std_logic;
      app_ref_req          : out std_logic;
      app_ref_ack          : in   std_logic;
      app_zq_req           : out    std_logic;
      app_zq_ack           : in   std_logic;
      ui_clk               : in   std_logic;
      ui_clk_sync_rst      : in   std_logic;
      init_calib_complete  : in	std_logic
	);
END COMPONENT;

COMPONENT TEXTbufferDDR
PORT(
	clk_MEM : IN std_logic;
	clk_VGA : IN std_logic;
	reset : in std_logic;
	VGAcols : IN std_logic_vector(7 downto 0);
	VBlank : IN std_logic;
	FetchNextRow : IN std_logic;
	txt_zero : IN std_logic_vector(23 downto 0);
	ADDR_TEXT : IN std_logic_vector(7 downto 0);
	t_axi_arready : IN std_logic;
	t_axi_rdata : IN std_logic_vector(127 downto 0);
	t_axi_rlast : IN std_logic;
	t_axi_rvalid : IN std_logic;          
	DATA_TEXT : OUT std_logic_vector(15 downto 0);
	t_axi_araddr : OUT std_logic_vector(31 downto 0);
	t_axi_arlen : OUT std_logic_vector(7 downto 0);
	t_axi_arvalid : OUT std_logic
	);
END COMPONENT;

COMPONENT SRAM_controller
PORT(
	RST : IN std_logic;
	CLK : IN std_logic;
	en : IN std_logic;
	ADDR : IN std_logic_vector(31 downto 0);
	size : IN std_logic_vector(1 downto 0);
	WE : IN std_logic_vector(0 to 0);
	DATA_in : IN std_logic_vector(31 downto 0);
	douta : IN std_logic_vector(31 downto 0);
	doutb : IN std_logic_vector(31 downto 0);          
	DATA_out : OUT std_logic_vector(31 downto 0);
	DATA_out_quick : OUT std_logic_vector(31 downto 0);
	wea : OUT std_logic_vector(3 downto 0);
	addra : OUT std_logic_vector(31 downto 2);
	dina : OUT std_logic_vector(31 downto 0);
	web : OUT std_logic_vector(3 downto 0);
	addrb : OUT std_logic_vector(31 downto 2);
	dinb : OUT std_logic_vector(31 downto 0);
	en_a : OUT std_logic;
	en_b : OUT std_logic
	);
END COMPONENT;

COMPONENT USER_RAM
  PORT (
    clka : IN STD_LOGIC;
    ena : IN STD_LOGIC;
    wea : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
    addra : IN STD_LOGIC_VECTOR(13 DOWNTO 0);
    dina : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
    douta : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
    clkb : IN STD_LOGIC;
    enb : IN STD_LOGIC;
    web : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
    addrb : IN STD_LOGIC_VECTOR(13 DOWNTO 0);
    dinb : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
    doutb : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
  );
END COMPONENT;

COMPONENT Char_RAM
  PORT (
    clka : IN STD_LOGIC;
    wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    addra : IN STD_LOGIC_VECTOR(11 DOWNTO 0);
    dina : IN STD_LOGIC_VECTOR(15 DOWNTO 0);
    douta : OUT STD_LOGIC_VECTOR(15 DOWNTO 0);
    clkb : IN STD_LOGIC;
    enb : IN STD_LOGIC;
    web : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    addrb : IN STD_LOGIC_VECTOR(11 DOWNTO 0);
    dinb : IN STD_LOGIC_VECTOR(15 DOWNTO 0);
    doutb : OUT STD_LOGIC_VECTOR(15 DOWNTO 0)
  );
END COMPONENT;

COMPONENT Color_RAM
  PORT (
    clka : IN STD_LOGIC;
    wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    addra : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
    dina : IN STD_LOGIC_VECTOR(15 DOWNTO 0);
    douta : OUT STD_LOGIC_VECTOR(15 DOWNTO 0);
    clkb : IN STD_LOGIC;
    enb : IN STD_LOGIC;
    web : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    addrb : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
    dinb : IN STD_LOGIC_VECTOR(15 DOWNTO 0);
    doutb : OUT STD_LOGIC_VECTOR(15 DOWNTO 0)
  );
END COMPONENT;

-- must be configured as WRITE FIRST
COMPONENT Pstack_RAM
  PORT (
    clka : IN STD_LOGIC;
    wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    addra : IN STD_LOGIC_VECTOR(12 DOWNTO 0);
    dina : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
    douta : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
  );
END COMPONENT;

-- must be configured as WRITE FIRST
COMPONENT Rstack_RAM
  PORT (
    clka : IN STD_LOGIC;
    wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    addra : IN STD_LOGIC_VECTOR(11 DOWNTO 0);
    dina : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
    douta : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
  );
END COMPONENT;

-- must be configured as WRITE FIRST
COMPONENT Sstack_RAM
  PORT (
    clka : IN STD_LOGIC;
    wea : IN STD_LOGIC_VECTOR(43 DOWNTO 0);
    addra : IN STD_LOGIC_VECTOR(11 DOWNTO 0);
    dina : IN STD_LOGIC_VECTOR(351 DOWNTO 0);
    douta : OUT STD_LOGIC_VECTOR(351 DOWNTO 0)
  );
END COMPONENT;

-- must be configured as WRITE FIRST
COMPONENT Estack_RAM
  PORT (
    clka : IN STD_LOGIC;
    wea : IN STD_LOGIC_VECTOR(37 DOWNTO 0);
    addra : IN STD_LOGIC_VECTOR(8 DOWNTO 0);
    dina : IN STD_LOGIC_VECTOR(303 DOWNTO 0);
    douta : OUT STD_LOGIC_VECTOR(303 DOWNTO 0)
  );
END COMPONENT;

COMPONENT HW_Registers
PORT(
	clk : IN std_logic;
	rst : IN std_logic;
	SD_datain : IN std_logic_vector(7 downto 0);
	SD_status : IN std_logic_vector(3 downto 0);
	VBLANK : IN std_logic;
	RS232_rx_S0 : IN std_logic_vector(7 downto 0);
	RS232_TBE_S0 : IN std_logic;
	RS232_RDA_S0 : IN std_logic;
	PS2_data : IN std_logic_vector(7 downto 0);
	counter_ms : IN std_logic_vector(31 downto 0);
	counter_clk : IN std_logic_vector(31 downto 0);
	MACreadyRX : IN std_logic;
	MACchecksum_err : IN std_logic;
	MACdataRX : IN std_logic_vector(7 downto 0);
	MACreadyTX : IN std_logic;
	SMIdataRead : IN std_logic_vector(15 downto 0);
	SMIready : IN std_logic;
	SW : IN std_logic_vector(15 downto 0);
	en : IN std_logic;
	addr : IN std_logic_vector(10 downto 0);
	datain : IN std_logic_vector(31 downto 0);
	wrq : IN std_logic_vector(0 to 0);          
	SD_dataout : OUT std_logic_vector(7 downto 0);
	SD_control : OUT std_logic_vector(3 downto 0);
	SD_wr : OUT std_logic;
	SD_divide : OUT std_logic_vector(7 downto 0);
	txt_zero : OUT std_logic_vector(23 downto 0);
	mode : OUT std_logic_vector(4 downto 0);
	background : OUT std_logic_vector(15 downto 0);
	interlace : OUT std_logic_vector(3 downto 0);
	charHeight : OUT std_logic_vector(3 downto 0);
	charWidth : OUT std_logic_vector(3 downto 0);
	VGArows : OUT std_logic_vector(7 downto 0);
	VGAcols : OUT std_logic_vector(7 downto 0);
	irq_mask : OUT std_logic_vector(15 downto 1);
	RS232_tx_S0 : OUT std_logic_vector(7 downto 0);
	RS232_wr_S0 : OUT std_logic;
	RS232_DIVIDE_S0 : OUT std_logic_vector(31 downto 0);
	MACread_enable : OUT std_logic;
	MACdataTX : OUT std_logic_vector(7 downto 0);
	MACweTX : OUT std_logic;
	MACtransmit_request : OUT std_logic;
	SMIaddr : OUT std_logic_vector(9 downto 0);
	SMIdataWrite : OUT std_logic_vector(15 downto 0);
	SMIread_request : OUT std_logic;
	SMIwrite_request : OUT std_logic;
	ssData : OUT std_logic_vector(31 downto 0);
	dataout : OUT std_logic_vector(31 downto 0)
	);
END COMPONENT;

COMPONENT CPU
Generic(
	vmp_w : integer;
	psp_w : integer;
	rsp_w : integer;
	ssp_w : integer;
	esp_w : integer
	);
PORT(
	rst : IN std_logic;
	clk : IN std_logic;
	irq : IN std_logic;
	irv : IN std_logic_vector(3 downto 0);
	blocked : IN std_logic;
	PSdatain : IN std_logic_vector(31 downto 0);
	RSdatain : IN std_logic_vector(31 downto 0);
	SSdatain : IN std_logic_vector(351 downto 320);
	ESdatain : IN std_logic_vector(303 downto 256);
	MEMdatain_X : IN std_logic_vector(31 downto 0);
	MEMdatain_X_quick : IN std_logic_vector(31 downto 0);
	s_axi_awready : IN std_logic;
	s_axi_wready : IN std_logic;
	s_axi_arready : IN std_logic;
	s_axi_rdata : IN std_logic_vector(31 downto 0);
	s_axi_rvalid : IN std_logic;
	vir_EN : IN std_logic;          
	rti : OUT std_logic;
	PSaddr : OUT std_logic_vector(vmp_w + psp_w -1 downto 0);
	PSdataout : OUT std_logic_vector(31 downto 0);
	PSw : OUT std_logic_vector(0 to 0);
	RSaddr : OUT std_logic_vector(vmp_w + rsp_w -1  downto 0);
	RSdataout : OUT std_logic_vector(31 downto 0);
	RSw : OUT std_logic_vector(0 to 0);
	SSaddr : out STD_LOGIC_VECTOR (vmp_w + ssp_w -1 downto 0);
	SSdataout : OUT std_logic_vector(351 downto 320);
	SSw : OUT std_logic_vector(43 downto 40);
	ESaddr : out STD_LOGIC_VECTOR (vmp_w + esp_w -1  downto 0);	
	ESdataout : OUT std_logic_vector(303 downto 256);
	ESw : OUT std_logic_vector(37 downto 32);
	MEMaddr : OUT std_logic_vector(31 downto 0);
	MEMdataout_X : OUT std_logic_vector(31 downto 0);
	MEM_WRQ_X : OUT std_logic;
	MEMsize_X : OUT std_logic_vector(1 downto 0);
	s_axi_awaddr : OUT std_logic_vector(31 downto 0);
	s_axi_awvalid : OUT std_logic;
	s_axi_wdata : OUT std_logic_vector(31 downto 0);
	s_axi_wstrb : OUT std_logic_vector(3 downto 0);
	s_axi_wvalid : OUT std_logic;
	s_axi_araddr : OUT std_logic_vector(31 downto 0);
	s_axi_arvalid : OUT std_logic;
	VM : OUT STD_LOGIC_VECTOR (vmp_w -1 downto 0);
	MEMdata_vir : OUT std_logic_vector(31 downto 0);
	debug : OUT std_logic_vector(7 downto 0)
	);
END COMPONENT;

COMPONENT stack_access
PORT(
	clk : IN std_logic;
	rst : IN std_logic;
	SSdatain : IN std_logic_vector(319 downto 0);
	SSwSignal : IN std_logic;
	ESdatain : IN std_logic_vector(255 downto 0);
	ESwSignal : IN std_logic;
	en : IN std_logic;
	addr : IN std_logic_vector(10 downto 0);
	datain : IN std_logic_vector(31 downto 0);
	wrq : IN std_logic_vector(0 to 0);          
	SSdataout : OUT std_logic_vector(319 downto 0);
	SSw : OUT std_logic_vector(39 downto 0);
	ESdataout : OUT std_logic_vector(255 downto 0);
	ESw : OUT std_logic_vector(31 downto 0);
	dataout : OUT std_logic_vector(31 downto 0)
	);
END COMPONENT;

COMPONENT Controller
PORT(
	clk : IN std_logic;
	trig : IN std_logic;          
	reset : OUT std_logic
	);
END COMPONENT;

COMPONENT VGA
PORT(
	clk_VGA : IN std_logic;
	reset : IN std_logic;
	mode : IN std_logic_vector(4 downto 0);
	background : IN std_logic_vector(15 downto 0);
	interlace : IN std_logic_vector(3 downto 0);
	charHeight : IN std_logic_vector(3 downto 0);
	charWidth : IN std_logic_vector(3 downto 0);
	VGArows : IN std_logic_vector(7 downto 0);
	VGAcols : IN std_logic_vector(7 downto 0);
	data_Text : IN std_logic_vector(15 downto 0);
	data_Char : IN std_logic_vector(15 downto 0);
	data_Color : IN std_logic_vector(15 downto 0);
	SW : IN std_logic_vector(15 downto 0);          
	addr_Text : OUT std_logic_vector(7 downto 0);
	addr_Char : OUT std_logic_vector(11 downto 0);
	addr_Color : OUT std_logic_vector(7 downto 0);
	HSync : OUT std_logic;
	VSync : OUT std_logic;
	RGB : OUT std_logic_vector(11 downto 0);
	VBlank : OUT std_logic;
	FetchNextRow : OUT std_logic
	);
END COMPONENT;

COMPONENT Interrupt
PORT(
	clk : IN std_logic;
	rst : IN std_logic;
	irq_mask : IN std_logic_vector(15 downto 1);
	RS232_RDA_S0 : IN std_logic;
	RS232_TBE_S0 : IN std_logic;
	PS2_irq : IN std_logic;
	ms_irq : IN std_logic;
	rti : IN std_logic;          
	irq : OUT std_logic;
	irv : OUT std_logic_vector(3 downto 0);
	blocked : OUT std_logic
	);
END COMPONENT;

COMPONENT UART
PORT(
	RXD : IN std_logic;
	DIVIDE : IN std_logic_vector(31 downto 0);
	TXDATA : IN std_logic_vector(7 downto 0);
	WR : IN std_logic;
	CLK : IN std_logic;          
	TXD : OUT std_logic;
	RXDATA : OUT std_logic_vector(7 downto 0);
	RDA : OUT std_logic;
	TBE : OUT std_logic
	);
END COMPONENT;

COMPONENT PS2KeyboardDecoder
PORT(
	clk : IN std_logic;
	PS2C : IN std_logic;
	PS2D : IN std_logic;          
	irq : OUT std_logic;
	data : OUT std_logic_vector(7 downto 0)
	);
END COMPONENT;

COMPONENT BootLoader
PORT(
	invReset : IN std_logic;
	clk : IN std_logic;
	RDA : IN std_logic;
	RXDATA : IN std_logic_vector(7 downto 0);          
	data : OUT std_logic_vector(31 downto 0);
	addr : OUT std_logic_vector(31 downto 2);
	we : OUT std_logic_vector(0 to 0)
	);
END COMPONENT;

COMPONENT ByteHEXdisplay
PORT(
	ssData : IN std_logic_vector(31 downto 0);
	clk : IN std_logic;
	count : IN std_logic_vector(15 downto 13);          
	sevenseg : OUT std_logic_vector(6 downto 0);
	anode : OUT std_logic_vector(7 downto 0)
	);
END COMPONENT;

COMPONENT SPImaster
PORT(
	CLK : IN std_logic;
	CLKSPI : IN std_logic;
	RESET : IN std_logic;
	WR : IN std_logic;
	DATA_OUT : IN std_logic_vector(7 downto 0);
	MISO : IN std_logic;
	mode : IN std_logic_vector(1 downto 0);
	MOSI_def : IN std_logic;          
	DATA_IN : OUT std_logic_vector(7 downto 0);
	TBE : OUT std_logic;
	MOSI : OUT std_logic;
	SCK : OUT std_logic
	);
END COMPONENT;

COMPONENT DIV
PORT(
	CLKin : IN std_logic;
	divide : IN std_logic_vector(7 downto 0);          
	CLKout : OUT std_logic
	);
END COMPONENT;

COMPONENT MediaAccessController
PORT(
	CLK100MHZ : IN std_logic;
	CLK50MHZ : IN std_logic;
	reset : IN std_logic;
	PHYCRS : IN std_logic;
	PHYRXERR : IN std_logic;
	PHYRXD : IN std_logic_vector(1 downto 0);
	PHYINTN : IN std_logic;
	read_enable : IN std_logic;
	dataTX : IN std_logic_vector(7 downto 0);
	weTX : IN std_logic;
	transmit_request : IN std_logic;          
	PHYCLK50MHZ : OUT std_logic;
	PHYRSTN : OUT std_logic;
	PHYTXEN : OUT std_logic;
	PHYTXD : OUT std_logic_vector(1 downto 0);
	dataRX : OUT std_logic_vector(7 downto 0);
	readyRX : OUT std_logic;
	Ethernet_IRQ : OUT std_logic;
	checksum_err : OUT std_logic;
	readyTX : OUT std_logic
	);
END COMPONENT;
		
COMPONENT SMI
PORT(
	CLK100MHz : IN std_logic;
	addr : IN std_logic_vector(9 downto 0);
	dataWrite : IN std_logic_vector(15 downto 0);
	read_request : IN std_logic;
	write_request : IN std_logic;    
	MDIO : INOUT std_logic;      
	dataRead : OUT std_logic_vector(15 downto 0);
	ready : OUT std_logic;
	MDC : OUT std_logic
	);
END COMPONENT;
	
begin

-----------------------------------------------------------------------------------------------------------------------------------
-- Debugging connections
-----------------------------------------------------------------------------------------------------------------------------------	

	-- Ethernet
	JB(0) <= PHYCRS;
	JB(1) <= '0';
	JB(2) <= '0';
	JB(3) <= '0';
	JB(4) <= '0';
	JB(5) <= '0';
	JB(6) <= '0';
	JB(7) <= '0';
	
	-- Debug and monitoring
	-- do not drive high continuously (use PWM)
	RGB1_Red <= '0';
	RGB1_Green <= '0';
	RGB1_Blue <= RS232_RDA_S0;
	
	-- can route to sevenseg display
	debug <= "0000000000000000" & debug_DMAcontroller & debug_CPU;

-----------------------------------------------------------------------------------------------------------------------------------
-- Global clocking and reset
-----------------------------------------------------------------------------------------------------------------------------------						

	clk_system <= clk100;
	clk_MEM <= clk100;
	invReset <= not reset;
	trig <= not CPUreset;
	
-----------------------------------------------------------------------------------------------------------------------------------
-- VGA clock selector
-----------------------------------------------------------------------------------------------------------------------------------	

	with mode(2 downto 0) select
		clk_VGA <= 	VGAclk25  when "001",
				VGAclk75  when "011",	
				VGAclk150 when "100",
				VGAclk50  when others; --"010"
	-- gated clocks are not good design practice in general but here we explicitly assume 
	-- that the VGA clock domain is not synchronized with the SYSTEM clock domain
	-- do not use these clocks to drive modules aside from VGA since they are be not timing constrained
	 
-----------------------------------------------------------------------------------------------------------------------------------
-- Global counters and timers
-----------------------------------------------------------------------------------------------------------------------------------	
	process														 
	begin
	wait until rising_edge(clk_system);					-- system clock rate
		counter_clk <= counter_clk + 1;
	end process;
	
	-- ms interrupt
	process														
	begin
	wait until rising_edge(clk100);						-- 100MHz clock
		if timer_ms = CONV_STD_LOGIC_VECTOR(100000,32) then
			timer_ms <=(others =>'0');
			counter_ms <= counter_ms + 1;
		else
			timer_ms <= timer_ms + 1;
		end if;
	end process;
	
	ms_irq <= '1' when timer_ms = "0000000000000000" else '0'; 

-----------------------------------------------------------------------------------------------------------------------------------
-- Memory module multiplexing
-----------------------------------------------------------------------------------------------------------------------------------	
	
	process
	begin
	wait until rising_edge(clk_system);
		bank <= bank_n;	
	end process;
	 
	with MEMaddr(17 downto 11) select
		bank_n <=	Stacks 	when "1110110",
				Color 		when "1110111",		
				Char 		when "1111000", 	-- must be aligned on 8k boundary
				Char 		when "1111001",
				Char 		when "1111010",
				Char		when "1111011",
				User 		when "1111100",	-- must be aligned on 4k boundary
				User 		when "1111101",					  
				Vir		when "1111110",
				Reg 		when "1111111",
				Sys 		when others;
					
	 Vir_EN <= '1' 		when bank_n = Vir else '0';
	 User_EN <= '1' 		when bank_n = User else '0';
	 Stack_access_EN <= '1' 	when bank_n = Stacks else '0';
	 Color_EN <= '1'		when bank_n = Color else '0';
	 Char_EN <= '1' 		when bank_n = Char else '0';
	 Reg_EN <= '1' 		when bank_n = Reg else '0';
	 Sys_EN <= '1' 		when bank_n = Sys else '0'; 
	 
	 with bank select										-- one cycle delayed to switch output
		MEMdatain_Xi <=	"0000000000000000" & MEMdata_Char	when Char,
					"0000000000000000" & MEMdata_Color when Color,
					MEMdata_Reg 				when Reg,
					Memdata_stack_access 		when stacks,
					MEMdata_User 				when user,
					MEMdata_Vir 				when vir,
					MEMdata_Sys 				when others;
					
					
	MEM_WRQ_XX(0) <= MEM_WRQ_X;				
	douta_sysram_i <= douta_sysram;
	doutb_sysram_i <= doutb_sysram;
	addra_userram_all(vmp_w + 10 downto 2) <= VM & addra_userram(10 downto 2);
	addrb_userram_all(vmp_w + 10 downto 2) <= VM & addrb_userram(10 downto 2); 		
	-- ISSUE THAT port A of SRAM is always enabled.  This could lead to overwrite problems - to be investigated
	wea_sysram <= wea_sysram_s when Boot_we = "0" else "1111";			-- splice IOExpansion data ahead of the SRAM
	dina_sysram <= dina_sysram_s when Boot_we = "0" else boot_data;
	addra_sysram <= addra_sysram_s when Boot_we = "0" else boot_addr;	

-----------------------------------------------------------------------------------------------------------------------------------
-- SD card connections
-----------------------------------------------------------------------------------------------------------------------------------	

	SD_WP <= '0';		-- available on PMOD SD card adapter but not Nexys 4 microSD card clot
	SD_RESET <= reset;	-- Nexys 4 microSD card slot needs SD_RESET driven low to power the SD card
	SD_CS <= SD_control(0);
	SD_status(1) <= SD_CD;
	SD_status(2) <= SD_WP;
	SD_status(3) <= MISO;

-----------------------------------------------------------------------------------------------------------------------------------
-- Module instantiations
-----------------------------------------------------------------------------------------------------------------------------------
	
inst_CLOCKMANAGER: CLOCKMANAGER
port map
(	-- Clock in ports
	CLK_IN1 => CLK_IN,
	-- Clock out ports
	CLK_OUT1 => VGACLK25,
	CLK_OUT2 => VGACLK50,
	CLK_OUT3 => VGACLK75,
	CLK_OUT4 => VGACLK150,	 
	CLK_OUT5 => CLK100,
	CLK_OUT6 => CLK50MHZ,
	CLK_OUT7 => CLK200MHZ
);	

--Inst_RAM_for_Testbench: RAM_for_Testbench 
--PORT MAP(
--	rst => reset,
--	clk => clk_system,
--	enA => ena_sysram,
--	enB => enb_sysram,
--	weA => wea_sysram,
--	weB => web_sysram,
--	addressA => addra_sysram (16 downto 2),
--	data_inA => dina_sysram,
--	data_outA => douta_sysram,
--	addressB => addrb_sysram (16 downto 2),
--	data_inB => dinb_sysram,
--	data_outB => doutb_sysram
--	);
	
Inst_SYS_RAM: SYS_RAM
PORT MAP (
	clka => clk_system,
	ena => '1',
	wea => wea_sysram,
	addra => addra_sysram (16 downto 2),
	dina => dina_sysram,
	douta => douta_sysram,
	clkb => clk_system,
	enb => enb_sysram,
	web => web_sysram,
	addrb => addrb_sysram (16 downto 2),
	dinb => dinb_sysram,
	doutb => doutb_sysram
	);

Inst_DMAcontrollerMIG7: DMAcontrollerMIG7
PORT MAP(
	CLK100MHZ => CLK100,
	CLK200MHZ => CLK200MHZ,
	reset => reset,
	s_axi_awaddr => s_axi_awaddr,
	s_axi_awvalid => s_axi_awvalid,
	s_axi_awready => s_axi_awready,
	s_axi_wdata => s_axi_wdata,
	s_axi_wstrb => s_axi_wstrb,
	s_axi_wvalid => s_axi_wvalid,
	s_axi_wready => s_axi_wready,
	s_axi_araddr => s_axi_araddr,
	s_axi_arvalid => s_axi_arvalid,
	s_axi_arready => s_axi_arready,
	s_axi_rdata => s_axi_rdata,
	s_axi_rvalid => s_axi_rvalid,
	t_axi_araddr => t_axi_araddr,
	t_axi_arlen => t_axi_arlen,
	t_axi_arvalid => t_axi_arvalid,
	t_axi_arready => t_axi_arready,
	t_axi_rdata => t_axi_rdata,
	t_axi_rlast => t_axi_rlast,
	t_axi_rvalid => t_axi_rvalid,
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
	init_calib_complete  => init_calib_complete
	);

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
	sys_clk_i 	     	=> CLK200MHZ,
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
	sys_rst => invRESET
     );

Inst_TEXTbufferDDR: TEXTbufferDDR 
PORT MAP(
	reset => reset,
	clk_MEM => clk_MEM,
	clk_VGA => clk_VGA,
	VGAcols => VGAcols,
	VBlank => VBlank,
	FetchNextRow => FetchNextRow,
	txt_zero => txt_zero,
	ADDR_TEXT => ADDR_TEXT,
	DATA_TEXT => DATA_TEXT,
	t_axi_araddr => t_axi_araddr,
	t_axi_arlen => t_axi_arlen,
	t_axi_arvalid => t_axi_arvalid,
	t_axi_arready => t_axi_arready,
	t_axi_rdata => t_axi_rdata,
	t_axi_rlast => t_axi_rlast,
	t_axi_rvalid => t_axi_rvalid
	);

Inst_SRAM_controller: SRAM_controller 
PORT MAP(
	RST => reset,
	CLK => clk_system,
	en => SYS_EN,
	ADDR => MEMaddr,
	size => MEMsize_X,
	WE => MEM_WRQ_XX,
	DATA_in => MEMdataout_X,
	DATA_out => MEMdata_Sys,
	DATA_out_quick => MEMdata_Sys_quick,
	wea => wea_sysram_s,
	addra => addra_sysram_s,
	dina => dina_sysram_s,
	douta => douta_sysram_i,
	web => web_sysram,
	addrb => addrb_sysram,
	dinb => dinb_sysram,
	doutb => doutb_sysram_i,
	en_a => ena_sysram,
	en_b => enb_sysram
	);

Inst_SRAM_controller_USER: SRAM_controller 
PORT MAP(
	RST => reset,
	CLK => clk_system,
	en => USER_EN,
	ADDR => MEMaddr,
	size => MEMsize_X,
	WE => MEM_WRQ_XX,
	DATA_in => MEMdataout_X,
	DATA_out => MEMdata_User,
	DATA_out_quick => open,
	wea => wea_userram,
	addra => addra_userram,
	dina => dina_userram,
	douta => douta_userram,
	web => web_userram,
	addrb => addrb_userram,
	dinb => dinb_userram,
	doutb => doutb_userram,
	en_a => ena_userram,
	en_b => enb_userram
	);	

inst_USER_RAM : USER_RAM
PORT MAP (
	clka => clk_system,
	ena => ena_userram,
	wea => wea_userram,
	addra => addra_userram_all(vmp_w + 10 downto 2),
	dina => dina_userram,
	douta => douta_userram,
	clkb => clk_system,
	enb => enb_userram,
	web => web_userram,
	addrb => addrb_userram_all(vmp_w + 10 downto 2),
	dinb => dinb_userram,
	doutb => doutb_userram
	);

inst_Char_RAM : Char_RAM
PORT MAP (
	clka => clk_VGA,
	wea => blank(0 downto 0),
	addra => addr_Char,
	dina => blank(15 downto 0),
	douta => data_Char,
	clkb => clk_system,
	enb => Char_EN,
	web => MEM_WRQ_XX,
	addrb => MEMaddr(12 downto 1),
	dinb => MEMdataout_X(15 downto 0),
	doutb => MEMdata_Char
	);		

inst_Color_RAM : Color_RAM
PORT MAP (
	clka => clk_VGA,
	wea => blank(0 downto 0),
	addra => addr_Color,
	dina => blank(15 downto 0),
	douta => data_Color,
	clkb => clk_system,
	enb => Color_EN,
	web => MEM_WRQ_XX,
	addrb => MEMaddr(8 downto 1),
	dinb => MEMdataout_X(15 downto 0),
	doutb => MEMdata_Color
	);

-- Pstack_RAM must be configured as WRITE FIRST
inst_Pstack_RAM : Pstack_RAM
PORT MAP (
	clka => clk_system,
	wea => PSw,
	addra => PSaddr,
	dina => PSdataOUT,
	douta => PSdataIN
	);

-- Rstack_RAM must be configured as WRITE FIRST
inst_Rstack_RAM : Rstack_RAM
PORT MAP (
	clka => clk_system,
	wea => RSw,
	addra => RSaddr,
	dina => RSdataOUT,
	douta => RSdataIN
	);

-- Sstack_RAM must be configured as WRITE FIRST
inst_Sstack_RAM : Sstack_RAM
PORT MAP (
	clka => clk_system,
	wea => SSw,
	addra => SSaddr,
	dina => SSdataOUT,
	douta => SSdataIN
	);

-- Estack_RAM must be configured as WRITE FIRST
inst_Estack_RAM : Estack_RAM
PORT MAP (
	clka => clk_system,
	wea => ESw,
	addra => ESaddr,
	dina => ESdataOUT,
	douta => ESdataIN
	);

inst_HW_Registers: HW_Registers 
PORT MAP(
	clk => CLK_SYSTEM,
	rst => reset,
	irq_mask => irq_mask,
	txt_zero => txt_zero,
	mode => mode,
	background => background,
	interlace => interlace,
	charHeight => charHeight,
	charWidth => charWidth, 
	VGArows => 	VGArows,	  
	VGAcols => VGAcols,
	en => reg_en,
	addr => MEMaddr(10 downto 0),
	datain => MEMdataout_X,
	dataout => MEMdata_Reg,
	wrq => MEM_WRQ_XX,
	RS232_rx_S0 => RS232_rx_S0,
	RS232_tx_S0 => RS232_tx_S0,
	RS232_wr_S0 => RS232_wr_S0,
	RS232_TBE_S0 => RS232_TBE_S0,
	RS232_RDA_S0 => RS232_RDA_S0,
	RS232_DIVIDE_S0 => RS232_DIVIDE_S0,		
	PS2_data => PS2_data,
	counter_clk => counter_clk,
	counter_ms => counter_ms,
	ssData => ssData,
	SW => SW,
	SD_dataout => SD_dataout,
	SD_datain => SD_datain,
	SD_status => SD_status,
	SD_control => SD_control,
	SD_wr => SD_wr,
	SD_divide => SD_divide,
	MACreadyRX => MACreadyRX,
	MACdataRX => MACdataRX,
	MACread_enable => MACread_enable,
	MACchecksum_err => MACchecksum_err,
	MACdataTX => MACdataTX,
	MACreadyTX => MACreadyTX,
	MACtransmit_request => MACtransmit_request,
	MACweTX => MACweTX,
	SMIaddr => SMIaddr,
	SMIdataWrite => SMIdataWrite,
	SMIread_request => SMIread_request,
	SMIwrite_request => SMIwrite_request,    
	SMIdataRead => SMIdataRead,
	SMIready => SMIready,
	VBLANK => VBLANK
	);  

inst_CPU: CPU 
GENERIC MAP(
	vmp_w => vmp_w,
	psp_w => psp_w,
	rsp_w => rsp_w,
	ssp_w => ssp_w,
	esp_w => esp_w
	)
PORT MAP(
	rst => reset,
	clk => CLK_SYSTEM,
	irq => irq,
	rti => rti,
	irv => irv,
	PSaddr => PSaddr,
	PSdatain => PSdatain,
	PSdataout => PSdataout,
	PSw => PSw,
	RSaddr => RSaddr,
	RSdatain => RSdatain,
	RSdataout => RSdataout,
	RSw => RSw,
	SSaddr => SSaddr,
	SSdatain => SSdatain(351 downto 320),	
	SSdataout => SSdataout(351 downto 320),
	SSw => SSw(43 downto 40),
	ESaddr => ESaddr,
	ESdatain => ESdatain(303 downto 256),
	ESdataout => ESdataout(303 downto 256),
	ESw => ESw(37 downto 32),
	MEMaddr => MEMaddr,
	MEMdatain_X => MEMdatain_Xi,
	MEMdatain_X_quick => MEMdata_Sys_quick,
	MEMdataout_X => MEMdataout_X,
	MEMsize_X => MEMsize_X,
	MEM_WRQ_X => MEM_WRQ_X,
	s_axi_awaddr => s_axi_awaddr,
	s_axi_awvalid => s_axi_awvalid,
	s_axi_awready => s_axi_awready,
	s_axi_wdata => s_axi_wdata,
	s_axi_wstrb => s_axi_wstrb,
	s_axi_wvalid => s_axi_wvalid,
	s_axi_wready => s_axi_wready,
	s_axi_araddr => s_axi_araddr,
	s_axi_arvalid => s_axi_arvalid,
	s_axi_arready => s_axi_arready,
	s_axi_rdata => s_axi_rdata,
	s_axi_rvalid => s_axi_rvalid,
	VM => VM,
	vir_EN => vir_EN,
	MEMdata_vir => MEMdata_vir,
	debug => debug_CPU,
	blocked => blocked
	);

Inst_stack_access: stack_access 
PORT MAP(
	clk => CLK_SYSTEM,
	rst => reset,
	SSdatain => SSdatain(319 downto 0),
	SSdataout => SSdataout(319 downto 0),
	SSw => SSw(39 downto 0),
	SSwSignal => SSw(40),
	ESdatain => ESdatain(255 downto 0),
	ESdataout => ESdataout(255 downto 0),
	ESw => ESw(31 downto 0),
	ESwSignal => ESw(32),
	en => stack_access_en,
	addr => MEMaddr(10 downto 0),
	datain => MEMdataout_X,
	dataout => MEMdata_stack_access,
	wrq => MEM_WRQ_XX
	);

Inst_Controller: Controller 
PORT MAP(
	clk => CLK_SYSTEM,
	trig => trig,
	reset => reset
	);

Inst_VGAController: VGA 
PORT MAP(
	CLK_VGA => CLK_VGA,
	reset => reset,
	mode	=> mode,
	background => background,
	data_Text => DATA_TEXT,
	addr_Text => ADDR_TEXT,
	data_Char => data_Char,
	addr_Char => addr_Char,
	data_Color => data_Color,
	addr_Color => addr_Color,		
	HSync => HSync,
	VSync => VSync,
	VBLANK => VBLANK,
	RGB => RGB,
	interlace => interlace,
	charHeight => charHeight,
	charWidth => charWidth, 
	VGArows => 	VGArows,	  
	VGAcols => VGAcols,
	FetchNextRow => FetchNextRow,
	SW => SW
	);	

Inst_Interrupt: Interrupt 
PORT MAP(
	clk => CLK_SYSTEM,
	rst => reset,
	irq_mask => irq_mask,
	RS232_RDA_S0 => RS232_RDA_S0,
	RS232_TBE_S0 => RS232_TBE_S0,
	PS2_irq => PS2_irq,
	ms_irq => ms_irq,
	rti => rti,
	irq => irq,
	irv => irv,
	blocked => blocked
	);

Inst_UART: UART 
PORT MAP(
	RXD => RXD_S0,
	TXD => TXD_S0,
	DIVIDE => RS232_DIVIDE_S0,
	TXDATA => RS232_tx_S0,
	RXDATA => RS232_rx_S0,
	RDA => RS232_RDA_S0,
	WR => RS232_WR_S0,
	TBE => RS232_TBE_S0,
	CLK => CLK_SYSTEM
	);

Inst_PS2KeyboardDecoder: PS2KeyboardDecoder 
PORT MAP(
	clk => CLK_SYSTEM,
	PS2C => PS2C,
	PS2D => PS2D,
	irq => PS2_irq,
	data => PS2_data
	);

Inst_BootLoader: BootLoader 
PORT MAP(
	invReset => invReset,
	clk => clk_system,
	RDA => RS232_RDA_S0,
	RXDATA => RS232_rx_S0,
	data => Boot_data,
	addr => Boot_addr,
	we => Boot_we
	);

Inst_ByteHEXdisplay: ByteHEXdisplay 
PORT MAP(
	ssData => ssData,
	clk => CLK_SYSTEM,
	count => counter_clk(15 downto 13),
	sevenseg => sevenseg,
	anode => anode
	);

Inst_SPImaster: SPImaster 
PORT MAP(
	CLK => CLK_SYSTEM,
	CLKSPI => CLKSPI,
	RESET => reset,
	DATA_IN => SD_datain,
	WR => SD_wr,
	TBE => SD_status(0),
	DATA_OUT => SD_dataout,
	MOSI => MOSI,
	MISO => MISO,
	SCK => SCK,
	mode => SD_control(3 downto 2),
	MOSI_def => SD_control(1)
	);

Inst_DIV: DIV 
PORT MAP(
	CLKin => CLK_SYSTEM,
	divide => SD_divide,
	CLKout => CLKSPI
);

Inst_MediaAccessController: MediaAccessController 
PORT MAP(
	CLK50MHZ => CLK50MHZ,
	CLK100MHZ => CLK_SYSTEM,
	reset => reset,
	PHYCRS => PHYCRS,
	PHYRXERR => PHYRXERR,
	PHYRXD => PHYRXD,
	PHYCLK50MHZ => PHYCLK50MHZ,
	PHYRSTN => PHYRSTN,
	PHYTXEN => PHYTXEN,
	PHYTXD => PHYTXD,
	PHYINTN => PHYINTN,
	dataRX => MACdataRX,
	readyRX => MACreadyRX,
	read_enable => MACread_enable,
	Ethernet_IRQ => open,
	checksum_err => MACchecksum_err,
	dataTX => MACdataTX,
	weTX => MACweTX,
	readyTX => MACreadyTX,
	transmit_request => MACtransmit_request
	);

Inst_SMI: SMI 
PORT MAP(
	CLK100MHz => CLK_SYSTEM,
	addr => SMIaddr,
	dataRead => SMIdataRead,
	dataWrite => SMIdataWrite,
	read_request => SMIread_request,
	write_request => SMIwrite_request,
	ready => SMIready,
	MDC => PHYMDC,
	MDIO => PHYMDIO
	);
		
end RTL;

