library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
Library UNISIM;
use UNISIM.vcomponents.all;

entity Board_Nexys4 is

Generic (	
	vmp_w : integer := 5;	
	psp_w : integer := 8;
	rsp_w : integer := 7;
	ssp_w : integer := 7;
	esp_w : integer := 4
	);
	
Port ( 
	CLK_IN : in  STD_LOGIC;
	-- VGA
	RGB : out  STD_LOGIC_VECTOR (11 downto 0);
	HSync : out  STD_LOGIC;
	VSync : out  STD_LOGIC;
	-- PSDRAM
	ADDR_SDRAM : out  STD_LOGIC_VECTOR (23 downto 1);		
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
	RGB1_Red : out STD_LOGIC;				  -- useful for debugging but do not drive high continuously
	RGB1_Green : out STD_LOGIC;
	RGB1_Blue : out STD_LOGIC;
	-- SPI for SD-card
	SCK : out STD_LOGIC;
	MOSI : out STD_LOGIC;
	MISO : in STD_LOGIC;
	SD_CS : out STD_LOGIC;
	SD_CD : in STD_LOGIC;
	SD_RESET : out STD_LOGIC;			  
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
	PHYINTN : in  STD_LOGIC
);
end Board_Nexys4;

architecture RTL of Board_Nexys4 is

constant blank : std_logic_vector(31 downto 0) := (others =>'0');
	
signal counter_clk, counter_ms : std_logic_vector(31 downto 0) := (others =>'0');
signal reset : std_logic;
signal VGAclk25, VGAclk50, VGAclk75, VGAclk150, clk100MHZ, clk50MHZ, clk200MHZ : std_logic;
signal irq, rti, ms_irq : std_logic;
signal irv : std_logic_vector(3 downto 0);
signal irq_mask : std_logic_vector(15 downto 1);
signal PSdatain :  std_logic_vector(31 downto 0);
signal RSdatain :  std_logic_vector(31 downto 0);
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
signal txt_zero : std_logic_vector(23 downto 0);
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
signal t_axi_rdata : std_logic_vector(15 downto 0);
signal t_axi_rresp : std_logic_vector(1 downto 0);
signal t_axi_rlast : std_logic;
signal t_axi_rvalid : std_logic;
signal s_aresetn : std_logic;

signal clk_system : std_logic;
signal clk_VGA : std_logic;
signal clk_MEM : std_logic;
signal VM : std_logic_vector(vmp_w -1 downto 0);
signal vir_EN : STD_LOGIC;
signal reg_EN : STD_LOGIC;
signal stack_access_EN :  STD_LOGIC;
signal	MEMaddr :  STD_LOGIC_VECTOR (31 downto 0);
signal MEMdata_vir : STD_LOGIC_VECTOR(31 DOWNTO 0);
signal	MEMdataout_X, MEMdatain_X, MEMdatain_X_quick, memdata_stack_access :  STD_LOGIC_VECTOR (31 downto 0);
signal	MEMsize_X :  STD_LOGIC_VECTOR (1 downto 0);
signal MEMdata_reg : STD_LOGIC_VECTOR (31 downto 0);
signal MEM_WRQ_X : STD_LOGIC;	
signal blocked : STD_LOGIC;
signal interlace	: STD_LOGIC_VECTOR (3 downto 0);	
signal charHeight: STD_LOGIC_VECTOR (3 downto 0);
signal charWidth: STD_LOGIC_VECTOR (3 downto 0);	
signal VGArows : STD_LOGIC_VECTOR (7 downto 0);					  
signal VGAcols : STD_LOGIC_VECTOR (7 downto 0);
signal MACdataRX, MACdataTX : STD_LOGIC_VECTOR(7 DOWNTO 0);
signal MACreadyRX, MACreadyTX, MACread_enable, MACchecksum_err, MACweTX, MACtransmit_request  : STD_LOGIC;
signal SMIaddr :  std_logic_vector(9 downto 0);
signal SMIdataWrite :  std_logic_vector(15 downto 0);
signal SMIread_request :  std_logic;
signal SMIwrite_request :  std_logic;       
signal SMIdataRead :  std_logic_vector(15 downto 0);
signal SMIready :  std_logic;
signal locked : std_logic;

component CLOCKMANAGER
port (
	CLK_IN1	: in  std_logic;
	CLK_OUT1	: out std_logic;
	CLK_OUT2	: out std_logic;
	CLK_OUT3	: out std_logic;
	CLK_OUT4	: out std_logic;
	CLK_OUT5	: out std_logic;
	CLK_OUT6	: out std_logic;
	CLK_OUT7	: out std_logic;
	LOCKED		: out std_logic
	 );
end component;
		
begin

inst_CLOCKMANAGER : CLOCKMANAGER
	port map
	(-- Clock in ports
		CLK_IN1 => CLK_IN,
		-- Clock out ports
		CLK_OUT1 => VGACLK25,
		CLK_OUT2 => VGACLK50,
		CLK_OUT3 => VGACLK75,
		CLK_OUT4 => VGACLK150,	 
		CLK_OUT5 => CLK100MHZ,
		CLK_OUT6 => CLK50MHZ,
		CLK_OUT7 => CLK200MHZ,
		LOCKED => LOCKED
	);
	
-- System and memory clock selector
clk_system <= CLK100MHZ;				-- Note above 100MHz vs. 95MHz
clk_mem <= CLK100MHZ;
	
-- VGA clock selector
	-- gated clocks are not good design practice in general but here we explicitly assume 
	-- that the VGA clock domain is not synchronized with the SYSTEM clock domain
	-- do not use these clocks to drive modules aside from VGA since they are be not timing constrained
with mode(2 downto 0) select
	clk_VGA <=	VGAclk25  when "001",
			VGAclk75  when "011",	
			VGAclk150 when "100",
			VGAclk50  when others; --"010"

-- SD card connections			  
SD_RESET <= reset;		-- Nexys 4 microSD card slot needs SD_RESET driven low to power the SD card
SD_CS <= SD_control(0);
SD_status(1) <= SD_CD;
SD_status(2) <= '0';		-- SD_WP signal not available on Nexys4 board
SD_status(3) <= MISO;

-- monitoring
RGB1_Red <= '0';		-- use these connections for debugging but do not drive high continuously (use PWM)
RGB1_Green <= '0';
RGB1_Blue <= RS232_RDA_S0;	

-- to be removed
s_aresetn <= not RESET;

inst_System_Memory: entity work.System_Memory 
GENERIC MAP(
	vmp_w => vmp_w,
	psp_w => psp_w,
	rsp_w => rsp_w,
	ssp_w => ssp_w,
	esp_w => esp_w
	)
PORT MAP(
	clk_system => clk_system,
	clk_vga => clk_vga,
	reset => reset,
	MEMaddr => MEMaddr,
	MEMdatain_X => MEMdatain_X,
	MEMdatain_X_quick => MEMdatain_X_quick,
	MEMdataout_X => MEMdataout_X,
	MEM_WRQ_X => MEM_WRQ_X,
	MEMsize_X => MEMsize_X,
	stack_access_EN => stack_access_EN,
	vir_EN => vir_EN,
	reg_en => reg_en,
	MEMdata_vir => MEMdata_vir,
	MEMdata_stack_access => MEMdata_stack_access,
	MEMdata_reg => MEMdata_reg,
	Boot_we => Boot_we,
	Boot_data => Boot_data,
       Boot_addr => Boot_addr,
	VM => VM,
	data_Char => data_Char,
	addr_Char => addr_Char,
	data_Color => data_Color,
	addr_Color => addr_Color
);
	
inst_HW_Registers: entity work.HW_Registers PORT MAP(
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
	wrq => MEM_WRQ_X,
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
			
inst_CPU: entity work.CPU 
GENERIC MAP(
	vmp_w => vmp_w,
	psp_w => psp_w,
	rsp_w => rsp_w,
	ssp_w => ssp_w,
	esp_w => esp_w
	)
PORT MAP(
	rst => reset,
	clk => clk_system,
	irq => irq,
	irv => irv,
	rti => rti,
	blocked => blocked,
	MEMaddr => MEMaddr,
	MEMdatain_X => MEMdatain_X,
	MEMdatain_X_quick => MEMdatain_X_quick,
	MEMdataout_X => MEMdataout_X,
	MEM_WRQ_X => MEM_WRQ_X,
	MEMsize_X => MEMsize_X,
	s_axi_awaddr => s_axi_awaddr,
	s_axi_awvalid => s_axi_awvalid,
	s_axi_awready => s_axi_awready,
	s_axi_wdata => s_axi_wdata,
	s_axi_wstrb => s_axi_wstrb,
	s_axi_wvalid => s_axi_wvalid,
	s_axi_wready => s_axi_wready,
--	s_axi_bresp => s_axi_bresp,
--	s_axi_bvalid => s_axi_bvalid,
--	s_axi_bready => s_axi_bready,
	s_axi_araddr => s_axi_araddr,
	s_axi_arvalid => s_axi_arvalid,
	s_axi_arready => s_axi_arready,
	s_axi_rdata => s_axi_rdata,
--	s_axi_rresp => s_axi_rresp,
	s_axi_rvalid => s_axi_rvalid,
--	s_axi_rready => s_axi_rready,
	VM => VM,
	vir_EN => vir_EN,
	MEMdata_vir => MEMdata_vir,
	stack_access_en => stack_access_en,
	MEMdata_stack_access => MEMdata_stack_access
);
	
inst_DMAcontroller: entity work.DMAcontroller PORT MAP(
	CLK => CLK_SYSTEM,
	RESET => reset,
	s_axi_awaddr => s_axi_awaddr,
	s_axi_awvalid => s_axi_awvalid,
	s_axi_awready => s_axi_awready,
	s_axi_wdata => s_axi_wdata,
	s_axi_wstrb => s_axi_wstrb,
	s_axi_wvalid => s_axi_wvalid,
	s_axi_wready => s_axi_wready,
--	s_axi_bresp => s_axi_bresp,
--	s_axi_bvalid => s_axi_bvalid,
--	s_axi_bready => s_axi_bready,
	s_axi_araddr => s_axi_araddr,
	s_axi_arvalid => s_axi_arvalid,
	s_axi_arready => s_axi_arready,
	s_axi_rdata => s_axi_rdata,
--	s_axi_rresp => s_axi_rresp,
	s_axi_rvalid => s_axi_rvalid,
--	s_axi_rready => s_axi_rready,
	t_axi_araddr => t_axi_araddr,
	t_axi_arlen => t_axi_arlen,
--	t_axi_arsize => t_axi_arsize,
--	t_axi_arburst => t_axi_arburst,
	t_axi_arvalid => t_axi_arvalid,
	t_axi_arready => t_axi_arready,
	t_axi_rdata => t_axi_rdata,
--	t_axi_rresp => t_axi_rresp,
	t_axi_rlast => t_axi_rlast,
	t_axi_rvalid => t_axi_rvalid,
--	t_axi_rready => t_axi_rready,
	ADDR_SDRAM => ADDR_SDRAM,
	DATA_SDRAM => DATA_SDRAM,
	OE_SDRAM => OE_SDRAM ,
	WE_SDRAM => WE_SDRAM,
	ADV_SDRAM => ADV_SDRAM ,
	CLK_SDRAM => CLK_SDRAM,
	UB_SDRAM => UB_SDRAM,
	LB_SDRAM => LB_SDRAM,
	CE_SDRAM => CE_SDRAM,
	CRE_SDRAM => CRE_SDRAM,
	WAIT_SDRAM => WAIT_SDRAM,
	debug => open
);
	
inst_Controller: entity work.Controller PORT MAP(
	clk => CLK_SYSTEM,
	locked => locked,
	CPUreset => CPUreset,
	reset => reset,
	counter_clk => counter_clk,
	counter_ms => counter_ms,
	ms_irq => ms_irq
);

inst_Interrupt: entity work.Interrupt PORT MAP(
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

inst_UART: entity work.UART PORT MAP(
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

inst_PS2KeyboardDecoder: entity work.PS2KeyboardDecoder PORT MAP(
	clk => CLK_SYSTEM,
	PS2C => PS2C,
	PS2D => PS2D,
	irq => PS2_irq,
	data => PS2_data
);
	
inst_BootLoader: entity work.BootLoader PORT MAP(
	reset => reset,
	clk => clk_system,
	RDA => RS232_RDA_S0,
	RXDATA => RS232_rx_S0,
	data => Boot_data,
	addr => Boot_addr,
	we => Boot_we
);
	
inst_ByteHEXdisplay: entity work.ByteHEXdisplay PORT MAP(
	ssData => ssData,
	clk => CLK_SYSTEM,
	count => counter_clk(15 downto 13),
	sevenseg => sevenseg,
	anode => anode
);

inst_SPImaster: entity work.SPImaster PORT MAP(
	CLK => CLK_SYSTEM,
	divide => SD_divide,
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

inst_MediaAccessController: entity work.MediaAccessController PORT MAP(
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

inst_SMI: entity work.SMI PORT MAP(
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

inst_Graphics: entity work.Graphics 
PORT MAP(
	clk_VGA => clk_VGA,
	clk_MEM => clk_MEM,
	reset => reset,
	HSync => HSync,
	VSync => VSync,
	RGB => RGB,
	mode => mode,
	background => background,
	interlace => interlace,
	charHeight => charHeight,
	charWidth => charWidth,
	VGArows => VGArows,
	VGAcols => VGAcols,
	VBlank => VBlank,
	txt_zero => txt_zero,
	t_axi_araddr => t_axi_araddr,
	t_axi_arlen => t_axi_arlen,
--	t_axi_arsize => t_axi_arsize,
--	t_axi_arburst => t_axi_arburst,
	t_axi_arvalid => t_axi_arvalid,
	t_axi_arready => t_axi_arready,
	t_axi_rdata => t_axi_rdata,
--	t_axi_rresp => t_axi_rresp,
	t_axi_rlast => t_axi_rlast,
	t_axi_rvalid => t_axi_rvalid,
--	t_axi_rready => t_axi_rready,
	data_Char => data_Char,
	addr_Char => addr_Char,
	data_Color => data_Color,
	addr_Color => addr_Color
);
		
end RTL;

