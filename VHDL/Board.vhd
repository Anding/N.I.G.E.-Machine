library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
Library UNISIM;
use UNISIM.vcomponents.all;

entity Board_Nexys2_1200 is
    Port ( CLK_IN : in  STD_LOGIC;
			  RGB : out  STD_LOGIC_VECTOR (7 downto 0);
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
			  SW : in STD_LOGIC_VECTOR (7 downto 0);
			  sevenseg : out STD_LOGIC_VECTOR (6 downto 0);
			  anode : out STD_LOGIC_VECTOR (3 downto 0);	
			  -- SPI
			  SCK : out STD_LOGIC;
			  MOSI : out STD_LOGIC;
			  MISO : in STD_LOGIC;
			  SD_CS : out STD_LOGIC;
			  SD_CD : in STD_LOGIC;
			  SD_WP : In STD_LOGIC;
			  -- Expansion
			  EppAstb: in std_logic;        
			  EppDstb: in std_logic;        
			  EppWr  : in std_logic;        
			  EppDB  : inout std_logic_vector(7 downto 0); 
			  EppWait: out std_logic
			  );
end Board_Nexys2_1200;

architecture RTL of Board_Nexys2_1200 is
type bank_t is (Sys, Char, Pstack, Rstack, Reg);
signal bank, bank_n : bank_t;	
signal counter_clk, counter_ms : std_logic_vector(31 downto 0) := (others =>'0');
signal timer_ms : std_logic_vector(15 downto 0) := (others =>'0');	
signal reset : std_logic;
signal clk_system, clk_vga, clk_mem, clk_2xsys : std_logic;
signal irq, rti, ms_irq : std_logic;
signal irv : std_logic_vector(3 downto 0);
signal irq_mask : std_logic_vector(15 downto 1);
signal PSdatain :  std_logic_vector(31 downto 0);
signal RSdatain :  std_logic_vector(31 downto 0);
signal MEMdatain_Xi :  std_logic_vector(31 downto 0);
signal MEMdata_Char :  std_logic_vector(7 downto 0);
signal MEMdata_Pstack, MEMdata_Rstack, MEMdata_Reg : std_logic_vector(31 downto 0);
signal MEMdatain_Y : std_logic_vector(7 downto 0);
signal MEM_RDY_Y :  std_logic;
signal MEMdatain_Z :  std_logic_vector(15 downto 0);
signal MEM_RDY_Z :  std_logic;          
signal PSaddr :  std_logic_vector(8 downto 0);
signal PSdataout :  std_logic_vector(31 downto 0);
signal PSw :  std_logic_vector(0 to 0);
signal RSaddr :  std_logic_vector(8 downto 0);
signal RSdataout :  std_logic_vector(31 downto 0);
signal RSw :  std_logic_vector(0 to 0);
signal MEMaddr :  std_logic_vector(31 downto 0);
signal MEMdataout_X :  std_logic_vector(31 downto 0);
signal MEMdataout_Y :  std_logic_vector(7 downto 0);
signal MEM_WRQ_X :  std_logic;
signal MEM_WRQ_XX : std_logic_vector(0 downto 0);
signal MEM_WRQ_Y :  std_logic;
signal MEM_REQ_Y :  std_logic;
signal MEMdataout_Z :  std_logic_vector(15 downto 0);
signal MEM_WRQ_Z :  std_logic;
signal MEM_REQ_Z : std_logic;
signal Sys_EN, Pstack_EN, Rstack_EN, Char_EN, Reg_EN : std_logic;
signal txt_zero : std_logic_vector(23 downto 0);
signal gfx_zero : std_logic_vector(15 downto 0);
signal DATA_OUT_VGA : std_logic_vector(7 downto 0);
signal ADDR_VGA : std_logic_vector(8 downto 0);
signal DATA_TXT : std_logic_vector(15 downto 0);
signal ADDR_TXT : std_logic_vector(6 downto 0);
signal DATA_Char : std_logic_vector(7 downto 0);
signal ADDR_Char : std_logic_vector(10 downto 0);
signal start_VGA, start_TXT : std_logic;
signal RS232_TX_S0 : std_logic_vector(7 downto 0);
signal RS232_WR_S0 : std_logic;       
signal RS232_RX_S0 : std_logic_vector(7 downto 0);
signal RS232_RDA_S0 : std_logic;
signal RS232_TBE_S0 : std_logic;
signal RS232_UBRR_S0 : std_logic_vector(15 downto 0);
signal Boot_we : STD_LOGIC_VECTOR(0 DOWNTO 0);
signal Boot_data : STD_LOGIC_VECTOR(31 DOWNTO 0);
signal Boot_addr : STD_LOGIC_VECTOR(15 DOWNTO 0);
signal PS2_irq : std_logic;
signal PS2_data : std_logic_vector(7 downto 0);
signal reset_trigger : std_logic;
signal mode : STD_LOGIC_VECTOR (4 downto 0);		
signal background : STD_LOGIC_VECTOR (7 downto 0);
signal ssData	: STD_LOGIC_VECTOR (15 downto 0);
signal CLKSPI, SD_wr : STD_LOGIC;
signal SD_dataout, SD_datain, SD_divide : STD_LOGIC_VECTOR (7 downto 0);
signal SD_status : STD_LOGIC_VECTOR (3 downto 0);
signal SD_control : STD_LOGIC_VECTOR (3 downto 0);
signal douta_sysram : std_logic_vector(31 downto 0);
signal doutb_sysram : std_logic_vector(31 downto 0);   
signal douta_sysram_r : std_logic_vector(31 downto 0);
signal doutb_sysram_r : std_logic_vector(31 downto 0);         
signal wea_sysram : std_logic_vector(0 to 0);
signal wea_sysram_s : std_logic_vector(0 to 0);
signal addra_sysram : std_logic_vector(15 downto 2);
signal addra_sysram_s : std_logic_vector(15 downto 2);
signal dina_sysram : std_logic_vector(31 downto 0);
signal dina_sysram_s : std_logic_vector(31 downto 0);
signal web_sysram : std_logic_vector(0 to 0);
signal addrb_sysram : std_logic_vector(15 downto 2);
signal dinb_sysram : std_logic_vector(31 downto 0);
signal MEMdata_Sys, MEMdata_Sys_plus : std_logic_vector(31 downto 0);
signal MEMdata_Sys_quick : std_logic_vector(31 downto 0);
signal MEMsize_X, MEMsize_Xp : std_logic_vector(1 downto 0);
--signal MEMsize_X_s : std_logic_vector(1 downto 0);
--signal MEMaddr_s :  std_logic_vector(31 downto 0);
--signal MEMdataout_X_s :  std_logic_vector(31 downto 0);
--signal MEM_WRQ_XX_s : std_logic_vector(0 downto 0);
--signal boot_active : std_logic;
signal ram_en : std_logic;

	COMPONENT DCM6
	PORT(
		CLKDV_SELECT_IN : IN std_logic;
		CLKIN_IN : IN std_logic;
		RST_IN : IN std_logic;          
		CLKDV_OUT : OUT std_logic;
		CLKIN_IBUFG_OUT : OUT std_logic;
		CLKMUX_OUT : OUT std_logic;
		CLK0_OUT : OUT std_logic;
		CLK2X_OUT : OUT std_logic;
		LOCKED_OUT : OUT std_logic
		);
	END COMPONENT;
	
	COMPONENT RS232v2
	PORT(
		RXD : IN std_logic;
		UBRR : IN std_logic_vector(15 downto 0);
		TXDATA : IN std_logic_vector(7 downto 0);
		WR : IN std_logic;
		CLK : IN std_logic;
		RST : IN std_logic;          
		TXD : OUT std_logic;
		RXDATA : OUT std_logic_vector(7 downto 0);
		RDA : OUT std_logic;
		TBE : OUT std_logic
		);
	END COMPONENT;
		
begin
	
	Inst_DCM6: DCM6 PORT MAP(
		CLKDV_SELECT_IN => mode(4),
		CLKIN_IN => CLK_IN,
		RST_IN => '0',
		CLKDV_OUT => open,
		CLKIN_IBUFG_OUT => open,
		CLKMUX_OUT => CLK_VGA,
		CLK0_OUT => CLK_SYSTEM,
		CLK2X_OUT => CLK_2XSYS,
		LOCKED_OUT => open
	);
	 
	-- global counters
	process														 
	begin
		wait until rising_edge(clk_system);					-- system clock rate
			counter_clk <= counter_clk + 1;
	end process;
	
	-- ms interrupt
	process														
	begin
		wait until rising_edge(clk_system);						-- 50MHz clock
		if timer_ms = CONV_STD_LOGIC_VECTOR(50000,16) then
			timer_ms <=(others =>'0');
			counter_ms <= counter_ms + 1;
		else
			timer_ms <= timer_ms + 1;
		end if;
	end process;
	
	ms_irq <= '1' when timer_ms = "0000000000000000" else '0';
	--ms_irq <= '1' when timer_ms(5 downto 0) = "100000" else '0';
	
	-- board level memory logic  
	 MEM_WRQ_XX(0) <= MEM_WRQ_X;	
	
	process
	begin
		wait until rising_edge(clk_system);
		bank <= bank_n;	
	end process;
	 
	with MEMaddr(15 downto 11) select
		bank_n <= Pstack when "11100",
					 Rstack when "11101",
					 Char when "11110",
					 Reg when "11111",
					 Sys when others;
	 
	 Pstack_EN <= '1' when bank_n = Pstack else '0';
	 Rstack_EN <= '1' when bank_n = Rstack else '0';
	 Char_EN <= '1' when bank_n = Char else '0';
	 Reg_EN <= '1' when bank_n = Reg else '0';
	 --Sys_EN <= '1' when ((bank_n = Sys and clk_system ='1') or Boot_we = "1") else '0';
	 Sys_EN <= '1' when bank_n = Sys else '0';
	 
	 with bank select														-- one cycle delayed to switch output
		MEMdatain_Xi <= MEMdata_Pstack when Pstack,
							MEMdata_Rstack when Rstack,
							"000000000000000000000000" & MEMdata_Char when Char,
							MEMdata_Reg when Reg,
							MEMdata_Sys when others;
							
	-- splice IOExpansion data ahead of the SRAM
	 wea_sysram <= wea_sysram_s when Boot_we = "0" else boot_we;
	 dina_sysram <= dina_sysram_s when Boot_we = "0" else boot_data;
	 addra_sysram <= addra_sysram_s when Boot_we = "0" else boot_addr(15 downto 2);	 
	 		
	 		
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

	Inst_SRAM_controller: entity work.SRAM_controller PORT MAP(
		RST => reset,
		CLK => clk_system,
		ADDR => MEMaddr,
		size => MEMsize_X,
		--size_plus => MEMsize_Xp,
		WE => MEM_WRQ_XX,
		DATA_in => MEMdataout_X,
		DATA_out => MEMdata_Sys,
		DATA_out_quick => MEMdata_Sys_quick,
		--DATA_out_plus => MEMdata_Sys_plus,
		wea => wea_sysram_s,
		addra => addra_sysram_s,
		dina => dina_sysram_s,
		douta => douta_sysram_r,
		web => web_sysram,
		addrb => addrb_sysram,
		dinb => dinb_sysram,
		doutb => doutb_sysram_r
	);
	
--	Inst_RAM_for_Testbench: entity work.RAM_for_Testbench PORT MAP(
--		rst => reset,
--		clk => clk_system,
--		weA => wea_sysram(0),
--		weB => web_sysram(0),
--		addressA => addra_sysram,
--		data_inA => dina_sysram,
--		data_outA => douta_sysram,
--		addressB => addrb_sysram,
--		data_inB => dinb_sysram,
--		data_outB => doutb_sysram
--	);

	  inst_SYS_RAM : entity work.Sys_RAM
	  PORT MAP (
		 clka => clk_system,--clk_2xsys,
		 ena => sys_en,
		 wea => wea_sysram,
		 addra => addra_sysram (10 downto 2),					-- write depth 12032, 15downto2
		 dina => dina_sysram,
		 douta => douta_sysram,
		 clkb => clk_system,--clk_2xsys,
		 enb => sys_en,
		 web => web_sysram,
		 addrb => addrb_sysram (10 downto 2),
		 dinb => dinb_sysram,
		 doutb => doutb_sysram
	  );
	  
	  --process												-- register SRAM outputs externally to expose for timing constraint
	  --begin
			--wait until rising_edge(clk_2xsys);
			douta_sysram_r <= douta_sysram;
			doutb_sysram_r <= doutb_sysram;
	  --end process;

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
	  
	  inst_HW_Registers: entity work.HW_Registers PORT MAP(
		clk => CLK_SYSTEM,
		clk2x => CLK_SYSTEM,--clk_2xsys,
		rst => reset,
		irq_mask => irq_mask,
		gfx_zero => gfx_zero,
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
		RS232_UBRR_S0 => RS232_UBRR_S0,		
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
		SD_divide => SD_divide
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
		--MEMdatain_X_plus => MEMdata_Sys_plus,
		MEMdataout_X => MEMdataout_X,
		MEMsize_X => MEMsize_X,
		--MEMsize_Xp => MEMsize_Xp,
		MEM_WRQ_X => MEM_WRQ_X,
		MEMdatain_Y => MEMdatain_Y,
		MEMdataout_Y => MEMdataout_Y,
		MEM_WRQ_Y => MEM_WRQ_Y,
		MEM_REQ_Y => MEM_REQ_Y,
		MEM_RDY_Y => MEM_RDY_Y,
		MEMdatain_Z => MEMdatain_Z,
		MEMdataout_Z => MEMdataout_Z,
		MEM_WRQ_Z => MEM_WRQ_Z,
		MEM_REQ_Z => MEM_REQ_Z,
		MEM_RDY_Z => MEM_RDY_Z
	);
		
		Inst_DMAController: entity work.DMAController PORT MAP(
		RESET => reset,
		CLK_MEM => CLK_SYSTEM,
		CLK_VGA => CLK_VGA,
		ADDR_SDRAM => ADDR_SDRAM,
		DATA_SDRAM => DATA_SDRAM,
		OE_SDRAM => OE_SDRAM,
		WE_SDRAM => WE_SDRAM,
		ADV_SDRAM => ADV_SDRAM,
		CLK_SDRAM => CLK_SDRAM,
		UB_SDRAM => UB_SDRAM,
		LB_SDRAM => LB_SDRAM,
		CE_SDRAM => CE_SDRAM,
		CRE_SDRAM => CRE_SDRAM,
		WAIT_SDRAM => WAIT_SDRAM,
		DATA_OUT_VGA => DATA_OUT_VGA,
		ADDR_VGA => ADDR_VGA,
		start_TXT => start_TXT,
		start_VGA => start_VGA,
		ADDR_ZERO => gfx_zero,
		mode => mode,
		DATA_IN_A => MEMdataout_Z,
		DATA_OUT_A => MEMdatain_Z,
		ADDR_A => MEMaddr(23 downto 1),
		REQ_A => MEM_REQ_Z,
		WRQ_A => MEM_WRQ_Z,
		RDY_A => MEM_RDY_Z,
		DATA_IN_B => (others=>'0'),
		ADDR_B => (others=>'0'),
		WRQ_B => '0',
		RDY_B => open,
		DATA_IN_C => MEMdataout_Y,
		DATA_OUT_C => MEMdatain_Y,
		ADDR_C => MEMaddr(23 downto 0),
		REQ_C => MEM_REQ_Y,
		WRQ_C => MEM_WRQ_Y,
		RDY_C => MEM_RDY_Y,
		DATA_OUT_TXT => DATA_TXT,	
		ADDR_TXT => ADDR_TXT,
		ADDR_ZERO_TXT => txt_zero
	);
	
		Inst_Controller: entity work.Controller PORT MAP(
		clk => CLK_SYSTEM,
		trig => reset_trigger,
		reset => reset
	);
		
		Inst_VGAController: entity work.VGA PORT MAP(
		CLK_VGA => CLK_VGA,
		RESET => reset,
		mode	=> mode,
		background => background,
		data_VGA => DATA_OUT_VGA,
		addr_VGA => ADDR_VGA,
		data_Text => DATA_TXT,
		addr_Text => ADDR_TXT,
		data_Char => data_Char,
		addr_Char => addr_Char,
		HSync => HSync,
		VSync => VSync,
		start_VGA => start_VGA,
		start_TXT => start_TXT,
		RGB => RGB
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
	
		Inst_RS232_SDA: RS232v2 PORT MAP(
		RXD => RXD_S0,
		TXD => TXD_S0,
		UBRR => RS232_UBRR_S0,
		TXDATA => RS232_tx_S0,
		RXDATA => RS232_rx_S0,
		RDA => RS232_RDA_S0,
		WR => RS232_WR_S0,
		TBE => RS232_TBE_S0,
		CLK => CLK_SYSTEM,
		RST => reset
	);
	
		Inst_PS2KeyboardDecoder: entity work.PS2KeyboardDecoder PORT MAP(
		clk => CLK_SYSTEM,
		PS2C => PS2C,
		PS2D => PS2D,
		irq => PS2_irq,
		data => PS2_data
	);
				
		Inst_IOExpansion: entity work.IOExpansionSYNC PORT MAP(
		reset => reset,
		clk => CLK_SYSTEM,
		EppAstb => EppAstb,
		EppDstb => EppDstb,
		EppWr => EppWr,
		EppDB => EppDB,
		EppWait =>EppWait ,
		data => Boot_data,
		addr => Boot_addr,
		we => Boot_we,
		reset_trigger => reset_trigger
		);
		
		Inst_ByteHEXdisplay: entity work.ByteHEXdisplay PORT MAP(
		ssData => ssData,
		clk50MHz => CLK_SYSTEM,
		count => counter_clk(15 downto 14),
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

