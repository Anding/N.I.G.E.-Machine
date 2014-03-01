library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
Library UNISIM;
use UNISIM.vcomponents.all;

entity Board_Nexys4 is
    Port ( CLK_IN : in  STD_LOGIC;
			  RGB : out  STD_LOGIC_VECTOR (11 downto 0);
           HSync : out  STD_LOGIC;
           VSync : out  STD_LOGIC;
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
			  RXD_S0 : in STD_LOGIC;
			  TXD_S0 : out STD_LOGIC;
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
			  -- SPI
			  SCK : out STD_LOGIC;
			  MOSI : out STD_LOGIC;
			  MISO : in STD_LOGIC;
			  SD_CS : out STD_LOGIC;
			  SD_CD : in STD_LOGIC;
			  --SD_WP : In STD_LOGIC
			  SD_RESET : out STD_LOGIC
			  );
end Board_Nexys4;

architecture RTL of Board_Nexys4 is

type bank_t is (Sys, Char, Color, Pstack, Rstack, Reg);
signal SD_WP : std_logic;
signal bank, bank_n : bank_t;	
signal counter_clk, counter_ms : std_logic_vector(31 downto 0) := (others =>'0');
signal timer_ms : std_logic_vector(31 downto 0) := (others =>'0');	
signal reset, invReset, trig : std_logic;
signal VGAclk25, VGAclk50, VGAclk75, clk100 : std_logic;
signal irq, rti, ms_irq : std_logic;
signal irv : std_logic_vector(3 downto 0);
signal irq_mask : std_logic_vector(15 downto 1);
signal PSdatain :  std_logic_vector(31 downto 0);
signal RSdatain :  std_logic_vector(31 downto 0);
signal MEMdatain_Xi :  std_logic_vector(31 downto 0);
signal MEMdata_Char :  std_logic_vector(7 downto 0);
signal MEMdata_Color :  std_logic_vector(15 downto 0);
signal MEMdata_Pstack, MEMdata_Rstack, MEMdata_Reg : std_logic_vector(31 downto 0);          
signal PSaddr :  std_logic_vector(8 downto 0);
signal PSdataout :  std_logic_vector(31 downto 0);
signal PSw :  std_logic_vector(0 to 0);
signal RSaddr :  std_logic_vector(8 downto 0);
signal RSdataout :  std_logic_vector(31 downto 0);
signal RSw :  std_logic_vector(0 to 0);
signal MEMaddr :  std_logic_vector(31 downto 0);
signal MEMdataout_X :  std_logic_vector(31 downto 0);
signal MEM_WRQ_X :  std_logic;
signal MEM_WRQ_XX : std_logic_vector(0 downto 0);
signal Sys_EN, Pstack_EN, Rstack_EN, Char_EN, Reg_EN, Color_EN : std_logic;
signal txt_zero : std_logic_vector(23 downto 0);
signal DATA_OUT_VGA : std_logic_vector(7 downto 0) := (others=>'0');
signal ADDR_VGA : std_logic_vector(8 downto 0);
signal DATA_TEXT : std_logic_vector(15 downto 0) := (others=>'0');
signal ADDR_TEXT : std_logic_vector(6 downto 0);
signal DATA_Char : std_logic_vector(7 downto 0);
signal ADDR_Char : std_logic_vector(10 downto 0);
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
signal wea_sysram : std_logic_vector(0 to 0);
signal wea_sysram_s : std_logic_vector(0 to 0);
signal addra_sysram : std_logic_vector(31 downto 2);
signal addra_sysram_s : std_logic_vector(31 downto 2);
signal dina_sysram : std_logic_vector(31 downto 0);
signal dina_sysram_s : std_logic_vector(31 downto 0);
signal web_sysram : std_logic_vector(0 to 0);
signal addrb_sysram : std_logic_vector(31 downto 2);
signal dinb_sysram : std_logic_vector(31 downto 0);
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
signal t_axi_rdata : std_logic_vector(31 downto 0);
signal t_axi_rresp : std_logic_vector(1 downto 0);
signal t_axi_rlast : std_logic;
signal t_axi_rvalid : std_logic;
signal s_aresetn : std_logic;
signal VGA_columns : std_logic_vector(6 downto 0);
signal VGA_active : std_logic;
signal VGA_newline : std_logic;
signal clk_system : std_logic;
signal clk_VGA : std_logic;
signal clk_MEM : std_logic;
signal debug : std_logic_vector(31 downto 0);
signal debug_CPU : std_logic_vector(7 downto 0);
signal debug_DMAcontroller : std_logic_vector(7 downto 0);

	component CLOCKMANAGER
	port
	 (-- Clock in ports
	  CLK_IN1           : in     std_logic;
	  -- Clock out ports
	  CLK_OUT1          : out    std_logic;
	  CLK_OUT2          : out    std_logic;
	  CLK_OUT3          : out    std_logic;
	  CLK_OUT4          : out    std_logic
	 );
	end component;
		
begin

	-- debug and monitoring
		-- use these connections for debugging but do not drive high continuously (use PWM)
	RGB1_Red <= '0';
	RGB1_Green <= '0';
	RGB1_Blue <= RS232_RDA_S0;
	
		-- can route to sevenseg display
	debug <= "0000000000000000" & debug_DMAcontroller & debug_CPU;
	
	inst_CLOCKMANAGER : CLOCKMANAGER
  port map
   (-- Clock in ports
    CLK_IN1 => CLK_IN,
    -- Clock out ports
	 CLK_OUT1 => VGACLK25,
    CLK_OUT2 => VGACLK50,
    CLK_OUT3 => VGACLK75,
	 CLK_OUT4 => CLK100);
	 
	-- System and memory clock selector
	clk_system <= clk100;
	clk_MEM <= clk100;
	
	-- VGA clock selector
		-- gated clocks are not good design practice in general but here we explicitly assume 
		-- that the VGA clock domain is not synchronized with the SYSTEM clock domain
		-- do not use these clocks to drive modules aside from VGA since they are be not timing constrained
	with mode(1 downto 0) select
			clk_VGA <= VGAclk25 when "01",
						  VGAclk50 when "10",
						  VGAclk75 when others;
	 
	-- global counters
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
	--ms_irq <= '1' when timer_ms(5 downto 0) = "100000" else '0';  -- for debugging interupt handling
	
	-- board level memory logic  
	 MEM_WRQ_XX(0) <= MEM_WRQ_X;	
	
	process
	begin
		wait until rising_edge(clk_system);
		bank <= bank_n;	
	end process;
	 
	with MEMaddr(17 downto 11) select
		bank_n <= Color when "1111011",
					 Pstack when "1111100",
					 Rstack when "1111101",
					 Char when "1111110",
					 Reg when "1111111",
					 Sys when others;
	 
	 Color_EN <= '1' when bank_n = Color else '0';
	 Pstack_EN <= '1' when bank_n = Pstack else '0';
	 Rstack_EN <= '1' when bank_n = Rstack else '0';
	 Char_EN <= '1' when bank_n = Char else '0';
	 Reg_EN <= '1' when bank_n = Reg else '0';
	 Sys_EN <= '1' when bank_n = Sys else '0'; 
	 ram_EN <= '1';
	 
	 with bank select														-- one cycle delayed to switch output
		MEMdatain_Xi <= MEMdata_Pstack when Pstack,
							MEMdata_Rstack when Rstack,
							"000000000000000000000000" & MEMdata_Char when Char,
							"0000000000000000" & MEMdata_Color when Color,
							MEMdata_Reg when Reg,
							MEMdata_Sys when others;
							
	-- splice IOExpansion data ahead of the SRAM
	 wea_sysram <= wea_sysram_s when Boot_we = "0" else boot_we;
	 dina_sysram <= dina_sysram_s when Boot_we = "0" else boot_data;
	 addra_sysram <= addra_sysram_s when Boot_we = "0" else boot_addr;	

	  inst_SYS_RAM : entity work.Sys_RAM
	  PORT MAP (
		 clka => clk_system,
		 ena => ram_en,
		 wea => wea_sysram,
		 addra => addra_sysram (16 downto 2),					-- 64K write depth 16384, 15downto2. 128K write depth 32768, 16 downto 2. 256K write depth 62976, 17downto2
		 dina => dina_sysram,
		 douta => douta_sysram,
		 clkb => clk_system,
		 enb => ram_en,
		 web => web_sysram,
		 addrb => addrb_sysram (16 downto 2),
		 dinb => dinb_sysram,
		 doutb => doutb_sysram
	  );
	  
--	Inst_RAM_for_Testbench: entity work.RAM_for_Testbench PORT MAP(
--		rst => reset,
--		clk => clk_system,
--		weA => wea_sysram(0),
--		weB => web_sysram(0),
--		addressA => addra_sysram (16 downto 2),
--		data_inA => dina_sysram,
--		data_outA => douta_sysram,
--		addressB => addrb_sysram (16 downto 2),
--		data_inB => dinb_sysram,
--		data_outB => doutb_sysram
--	);

	  douta_sysram_i <= douta_sysram;
	  doutb_sysram_i <= doutb_sysram;

		Inst_SRAM_controller: entity work.SRAM_controller PORT MAP(
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
		doutb => doutb_sysram_i
	);
	
	  inst_Char_RAM : entity work.Char_RAM
	  PORT MAP (
		 clka => clk_VGA,
		 wea => "0",
		 addra => addr_Char,
		 dina => (others=>'0'),
		 douta => data_Char,
		 clkb => clk_system,
		 enb => Char_EN,
		 web => MEM_WRQ_XX,
		 addrb => MEMaddr(10 downto 0),
		 dinb => MEMdataout_X(7 downto 0),
		 doutb => MEMdata_Char
	  );		
	
	  	inst_Pstack_RAM : entity work.Pstack_RAM
	  PORT MAP (
		 clka => clk_system,
		 wea => PSw,
		 addra => PSaddr,
		 dina => PSdataOUT,
		 douta => PSdataIN,
		 clkb => clk_system,
		 enb => Pstack_EN,
		 web => MEM_WRQ_XX,
		 addrb => MEMaddr(10 downto 2),
		 dinb => MEMdataout_X,
		 doutb => MEMdata_Pstack
	  );
	  
	  inst_Rstack_RAM : entity work.Rstack_RAM
	  PORT MAP (
		 clka => clk_system,
		 wea => RSw,
		 addra => RSaddr,
		 dina => RSdataOUT,
		 douta => RSdataIN,
		 clkb => clk_system,
		 enb => Rstack_EN,
		 web => MEM_WRQ_XX,
		 addrb => MEMaddr(10 downto 2),
		 dinb => MEMdataout_X,
		 doutb => MEMdata_Rstack
	  );	
	  
	  inst_Color_RAM : entity work.Color_RAM
	  PORT MAP (
		 clka => clk_VGA,
		 ena => '1',
		 wea => "0",
		 addra => addr_Color,
		 dina => (others=>'0'),
		 douta => data_Color,
		 clkb => clk_system,
		 enb => Color_EN,
		 web => MEM_WRQ_XX,
		 addrb => MEMaddr(8 downto 1),
		 dinb => MEMdataout_X(15 downto 0),
		 doutb => MEMdata_Color
	  );
	  
	  inst_HW_Registers: entity work.HW_Registers PORT MAP(
		clk => CLK_SYSTEM,
		rst => reset,
		irq_mask => irq_mask,
		txt_zero => txt_zero,
		mode => mode,
		background => background,
		en => reg_en,
		addr => MEMaddr(10 downto 0) ,
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
		VBLANK => VBLANK
	);
	
		inst_CPU: entity work.CPU PORT MAP(
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
		s_axi_bresp => s_axi_bresp,
		s_axi_bvalid => s_axi_bvalid,
		s_axi_bready => s_axi_bready,
		s_axi_araddr => s_axi_araddr,
		s_axi_arvalid => s_axi_arvalid,
		s_axi_arready => s_axi_arready,
		s_axi_rdata => s_axi_rdata,
		s_axi_rresp => s_axi_rresp,
		s_axi_rvalid => s_axi_rvalid,
		s_axi_rready => s_axi_rready,
		debug => debug_CPU
	);
	
	s_aresetn <= not RESET;
		
	Inst_DMAcontroller: entity work.DMAcontroller PORT MAP(
		CLK => CLK_SYSTEM,
		s_aresetn => s_aresetn,
		s_axi_awaddr => s_axi_awaddr,
		s_axi_awvalid => s_axi_awvalid,
		s_axi_awready => s_axi_awready,
		s_axi_wdata => s_axi_wdata,
		s_axi_wstrb => s_axi_wstrb,
		s_axi_wvalid => s_axi_wvalid,
		s_axi_wready => s_axi_wready,
		s_axi_bresp => s_axi_bresp,
		s_axi_bvalid => s_axi_bvalid,
		s_axi_bready => s_axi_bready,
		s_axi_araddr => s_axi_araddr,
		s_axi_arvalid => s_axi_arvalid,
		s_axi_arready => s_axi_arready,
		s_axi_rdata => s_axi_rdata,
		s_axi_rresp => s_axi_rresp,
		s_axi_rvalid => s_axi_rvalid,
		s_axi_rready => s_axi_rready,
		t_axi_araddr => t_axi_araddr,
		t_axi_arlen => t_axi_arlen,
		t_axi_arsize => t_axi_arsize,
		t_axi_arburst => t_axi_arburst,
		t_axi_arvalid => t_axi_arvalid,
		t_axi_arready => t_axi_arready,
		t_axi_rdata => t_axi_rdata,
		t_axi_rresp => t_axi_rresp,
		t_axi_rlast => t_axi_rlast,
		t_axi_rvalid => t_axi_rvalid,
		t_axi_rready => t_axi_rready,
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
		debug => debug_DMAcontroller
	);
	
		trig <= not CPUreset;
		
		Inst_Controller: entity work.Controller PORT MAP(
		clk => CLK_SYSTEM,
		trig => trig,
		reset => reset
	);
		
		Inst_VGAController: entity work.VGA PORT MAP(
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
		VGA_columns => VGA_columns,
		VGA_active => VGA_active,
		VGA_newline => VGA_newline
	);	
	
		Inst_TEXTbuffer: entity work.TEXTbuffer PORT MAP(
		clk_MEM => clk_MEM,
		clk_VGA => clk_VGA,
		VGA_columns => VGA_columns,
		VGA_active => VGA_active,
		VGA_newline => VGA_newline,
		txt_zero => txt_zero,
		ADDR_TEXT => ADDR_TEXT,
		DATA_TEXT => DATA_TEXT,
		t_axi_araddr => t_axi_araddr,
		t_axi_arlen => t_axi_arlen,
		t_axi_arsize => t_axi_arsize,
		t_axi_arburst => t_axi_arburst,
		t_axi_arvalid => t_axi_arvalid,
		t_axi_arready => t_axi_arready,
		t_axi_rdata => t_axi_rdata,
		t_axi_rresp => t_axi_rresp,
		t_axi_rlast => t_axi_rlast,
		t_axi_rvalid => t_axi_rvalid,
		t_axi_rready => t_axi_rready
	);
	
		Inst_Interrupt: entity work.Interrupt PORT MAP(
		clk => CLK_SYSTEM,
		rst => reset,
		irq_mask => irq_mask,
		RS232_RDA_S0 => RS232_RDA_S0,
		RS232_TBE_S0 => RS232_TBE_S0,
		PS2_irq => PS2_irq,
		ms_irq => ms_irq,
		rti => rti,
		irq => irq,
		irv => irv
	);
	
		Inst_UART: entity work.UART PORT MAP(
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
	
		Inst_PS2KeyboardDecoder: entity work.PS2KeyboardDecoder PORT MAP(
		clk => CLK_SYSTEM,
		PS2C => PS2C,
		PS2D => PS2D,
		irq => PS2_irq,
		data => PS2_data
	);

		invReset <= not reset;
		
		Inst_BootLoader: entity work.BootLoader PORT MAP(
		invReset => invReset,
		clk => clk_system,
		RDA => RS232_RDA_S0,
		RXDATA => RS232_rx_S0,
		data => Boot_data,
		addr => Boot_addr,
		we => Boot_we
	);
		
		Inst_ByteHEXdisplay: entity work.ByteHEXdisplay PORT MAP(
		ssData => ssData,
		clk => CLK_SYSTEM,
		count => counter_clk(15 downto 13),
		sevenseg => sevenseg,
		anode => anode
	);
	
		Inst_SPImaster: entity work.SPImaster PORT MAP(
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
	
		SD_WP <= '0';				-- available on PMOD SD card adapter but not Nexys 4 microSD card clot
		SD_RESET <= reset;		-- Nexys 4 microSD card slot needs SD_RESET driven low to power the SD card
	
		SD_CS <= SD_control(0);
		SD_status(1) <= SD_CD;
		SD_status(2) <= SD_WP;
		SD_status(3) <= MISO;
	
		Inst_DIV: entity work.DIV PORT MAP(
		CLKin => CLK_SYSTEM,
		divide => SD_divide,
		CLKout => CLKSPI
	);
	
end RTL;

