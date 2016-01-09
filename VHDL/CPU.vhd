-- CPU module
-- Andrew Read
-- Created 11 June 2011

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity CPU is
Generic (	
	vmp_w : integer;
	psp_w : integer;
	rsp_w : integer;
	ssp_w : integer;
	esp_w : integer
);
Port ( 	
	rst : in STD_LOGIC;
	clk : in STD_LOGIC;
	-- interrupt controller lines
	irq : in STD_LOGIC;
	irv : in std_logic_vector(3 downto 0);
	rti : out std_logic; 
	blocked : in std_logic;
	-- 32 bit wide SRAM databus				
	MEMaddr : out STD_LOGIC_VECTOR (31 downto 0);			
	MEMdatain_X : in STD_LOGIC_VECTOR (31 downto 0);
	MEMdatain_X_quick : in STD_LOGIC_VECTOR (31 downto 0);
	MEMdataout_X : out STD_LOGIC_VECTOR (31 downto 0);		
	MEM_WRQ_X : out STD_LOGIC;	
	MEMsize_X : out STD_LOGIC_VECTOR (1 downto 0);
	-- 32 bit wide AXI4 databus
	s_axi_awaddr : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
	s_axi_awvalid : OUT STD_LOGIC;
	s_axi_awready : IN STD_LOGIC;
	-- write
	s_axi_wdata : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
	s_axi_wstrb : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
	s_axi_wvalid : OUT STD_LOGIC;
	s_axi_wready : IN STD_LOGIC;
	-- write response
--	s_axi_bresp : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
--	s_axi_bvalid : IN STD_LOGIC;
--	s_axi_bready : OUT STD_LOGIC;
	-- address read
	s_axi_araddr : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
	s_axi_arvalid : OUT STD_LOGIC;
	s_axi_arready : IN STD_LOGIC;
	-- read
	s_axi_rdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
--	s_axi_rresp : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
	s_axi_rvalid : IN STD_LOGIC;
--	s_axi_rready : OUT STD_LOGIC;
	-- virtualization control
	VM : OUT STD_LOGIC_VECTOR (vmp_w -1 downto 0);
	vir_EN : IN STD_LOGIC;
	MEMdata_vir : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
	-- access to subroutine and exception stacks
	stack_access_en : IN STD_LOGIC;
	MEMdata_stack_access : OUT std_logic_vector(31 downto 0)
	);
end CPU;

architecture Structural of CPU is

signal	Accumulator : std_logic_vector(31 downto 0);
signal	MicroControl :  std_logic_vector(22 downto 0);
signal	AuxControl :  std_logic_vector(1 downto 0);
signal	ReturnAddress :  std_logic_vector(31 downto 0);          
signal	TOS, TOS_r :  std_logic_vector(31 downto 0);
signal	NOS, NOS_r :  std_logic_vector(31 downto 0);
signal	TORS :  std_logic_vector(31 downto 0);
signal ExceptionAddress : STD_LOGIC_VECTOR (31 downto 0);
signal	equalzero, equalzero_r : std_logic;
signal	chip_RAM : std_logic;
signal	MEMdataout_X_i : STD_LOGIC_VECTOR (31 downto 0);
signal	MEM_WRQ_X_i : STD_LOGIC;
signal MEMaddr_i : STD_LOGIC_VECTOR (31 downto 0);
signal VM_i : STD_LOGIC_VECTOR (vmp_w -1 downto 0);
signal	DatapathFreeze : STD_LOGIC_VECTOR (95 downto 0);					-- virtualization unit
signal	DatapathThaw : STD_LOGIC_VECTOR (95 downto 0);
signal pause : STD_LOGIC;
signal SingleMulti : STD_LOGIC;
signal PCfreeze, PCthaw, VirtualInterrupt : STD_LOGIC_VECTOR (19 downto 0);
signal	interval : STD_LOGIC_VECTOR (15 downto 0);
signal SSdataOUT : std_logic_vector(287 downto 0);
signal SSdataIN : std_logic_vector(287 downto 0);
signal SSw : std_logic_vector(35 downto 0);
signal SSaddr : std_logic_vector(vmp_w + ssp_w -1 downto 0);
signal ESdataOUT : std_logic_vector(239 downto 0);
signal ESdataIN : std_logic_vector(239 downto 0);
signal ESw : std_logic_vector(29 downto 0);
signal ESaddr : std_logic_vector(vmp_w + esp_w -1 downto 0);
signal PSaddr :  std_logic_vector(vmp_w + psp_w -1 downto 0);
signal PSdatain :  std_logic_vector(31 downto 0);
signal PSdataout :  std_logic_vector(31 downto 0);
signal PSw :  std_logic_vector(0 to 0);
signal RSaddr :  std_logic_vector(vmp_w + rsp_w -1 downto 0);
signal RSdatain :  std_logic_vector(31 downto 0);
signal RSdataout :  std_logic_vector(31 downto 0);
signal RSw :  std_logic_vector(0 to 0);
signal MEM_WRQ_XX : std_logic_vector(0 downto 0);
--signal PSaddr :  std_logic_vector(vmp_w + psp_w -1 downto 0);
--signal PSdataout : OUT std_logic_vector(31 downto 0);
--signal PSw : OUT std_logic_vector(0 downto 0);
--signal RSaddr : OUT std_logic_vector(vmp_w + rsp_w -1  downto 0);
--signal RSdataout : OUT std_logic_vector(31 downto 0);
--signal RSw : OUT std_logic_vector(0 downto 0);
--signal SSaddr : out STD_LOGIC_VECTOR (vmp_w + ssp_w -1 downto 0);			-- Subroutine stack memory
--signal SSdatain : in STD_LOGIC_VECTOR (351 downto 320);	
--signal SSdataout : out STD_LOGIC_VECTOR (351 downto 320);
--signal SSw : out STD_LOGIC_VECTOR (43 downto 40);
--signal ESaddr : out STD_LOGIC_VECTOR (vmp_w + esp_w -1  downto 0);			-- Exception stack memory
--signal ESdatain : in STD_LOGIC_VECTOR (303 downto 256);	
--signal ESdataout : out STD_LOGIC_VECTOR (303 downto 256);
--signal ESw : out STD_LOGIC_VECTOR (37 downto 32);

begin

	MEMaddr <= MEMaddr_i;
	MEMdataout_X <= MEMdataout_X_i;
	MEM_WRQ_X <= MEM_WRQ_X_i;
	VM <= VM_i;
	MEM_WRQ_XX(0) <= MEM_WRQ_X_i;
	
inst_Datapath: entity work.Datapath 
	
	GENERIC MAP(
		vmp_w => vmp_w,
		psp_w => psp_w,
		rsp_w => rsp_w,
		ssp_w => ssp_w,
		esp_w => esp_w
		)
		
	PORT MAP(
		rst => rst,
		clk => clk,
		MEMdatain_X => MEMdatain_X,
		Accumulator => Accumulator,
		MicroControl => MicroControl,
		AuxControl => AuxControl,
		ReturnAddress => ReturnAddress,
		TOS => TOS,
		TOS_r => TOS_r,
		NOS_r => NOS_r,
		TORS => TORS,
		ExceptionAddress => ExceptionAddress,
		equalzero => equalzero,
		equalzero_r => equalzero_r,
		chip_RAM => chip_RAM,
		PSaddr => PSaddr,
		PSdatain => PSdatain,
		PSdataout => PSdataout,
		PSw => PSw,
		RSaddr => RSaddr,
		RSdatain => RSdatain,
		RSdataout => RSdataout,
		RSw => RSw,
		SSaddr => SSaddr,
		SSdatain => SSdatain(287 downto 256),	
		SSdataout => SSdataout(287 downto 256),
		SSw => SSw(35 downto 32),
		ESaddr => ESaddr,
		ESdatain => ESdatain(239 downto 192),
		ESdataout => ESdataout(239 downto 192),
		ESw => ESw(29 downto 24),
		VM => VM_i,
		datapathfreeze => datapathfreeze,
		datapaththaw => datapaththaw
	);
	
inst_ControlUnit: entity work.ControlUnit 
	
	PORT MAP(
		rst => rst,
		clk => clk,
		irq => irq,
		rti => rti,
		irv => irv,
		blocked => blocked,
		TOS => TOS,
		TOS_r => TOS_r,
		NOS_r => NOS_r,
		TORS => TORS,
		ExceptionAddress => ExceptionAddress,
		equalzero => equalzero,
		equalzero_r => equalzero_r,
		chip_RAM => chip_RAM,
		MicroControl => MicroControl,
		AuxControl => AuxControl,
		Accumulator => Accumulator,
		ReturnAddress => ReturnAddress,
		MEMaddr => MEMaddr_i,
		MEMdatain_X => MEMdatain_X_quick,
		MEMdataout_X => MEMdataout_X_i,
		MEM_WRQ_X => MEM_WRQ_X_i,
		MEMsize_X => MEMsize_X,
		s_axi_awaddr => s_axi_awaddr,
		s_axi_awvalid => s_axi_awvalid,
		s_axi_awready => s_axi_awready,
		s_axi_wdata => s_axi_wdata,
		s_axi_wstrb => s_axi_wstrb,
		s_axi_wvalid => s_axi_wvalid,
		s_axi_wready => s_axi_wready,
--		s_axi_bresp => s_axi_bresp,
--		s_axi_bvalid => s_axi_bvalid,
--		s_axi_bready => s_axi_bready,
		s_axi_araddr => s_axi_araddr,
		s_axi_arvalid => s_axi_arvalid,
		s_axi_arready => s_axi_arready,
		s_axi_rdata => s_axi_rdata, 
--		s_axi_rresp => s_axi_rresp,
		s_axi_rvalid => s_axi_rvalid,
--		s_axi_rready => s_axi_rready,
		pause => pause,
		SingleMulti => SingleMulti,
		PCfreeze => PCfreeze,
		PCthaw => PCthaw,
		VirtualInterrupt => VirtualInterrupt,
		Interval => Interval
	);
	
inst_VirtualizationUnit: entity work.VirtualizationUnit 
	
	GENERIC MAP(
		vmp_w => vmp_w)
	
	PORT MAP(
		clk => clk,
		rst => rst,
		pause => pause,
		SingleMulti => SingleMulti,
		Interval => Interval,
		VM => VM_i,
		PCfreeze => PCfreeze,
		PCthaw => PCthaw,
		VirtualInterrupt => VirtualInterrupt,
		DatapathFreeze => DatapathFreeze,
		DatapathThaw => DatapathThaw,
		en => vir_EN,
		addr => MEMaddr_i(10 downto 0),
		datain => MEMdataout_X_i,
		dataout => MEMdata_vir,
		wrq => MEM_WRQ_X_i
	);
	
		-- Pstack_RAM must be configured as WRITE FIRST
inst_Pstack_RAM : entity work.Pstack_RAM
	  PORT MAP (
		 clka => clk,
		 wea => PSw,
		 addra => PSaddr,
		 dina => PSdataOUT,
		 douta => PSdataIN
	  );
	  
	  -- Rstack_RAM must be configured as WRITE FIRST
inst_Rstack_RAM : entity work.Rstack_RAM
	  PORT MAP (
		 clka => clk,
		 wea => RSw,
		 addra => RSaddr,
		 dina => RSdataOUT,
		 douta => RSdataIN
	  );
	  
		-- Sstack_RAM must be configured as WRITE FIRST
inst_Sstack_RAM : entity work.Sstack_RAM
		PORT MAP (
		clka => clk,
		wea => SSw,
		addra => SSaddr,
		dina => SSdataOUT,
		douta => SSdataIN
		);
		
		-- Estack_RAM must be configured as WRITE FIRST
inst_Estack_RAM : entity work.Estack_RAM
		PORT MAP (
		clka => clk,
		wea => ESw,
		addra => ESaddr,
		dina => ESdataOUT,
		douta => ESdataIN
		);
		
inst_stack_access: entity work.stack_access PORT MAP(
		clk => clk,
		rst => rst,
		SSdatain => SSdatain(255 downto 0),
		SSdataout => SSdataout(255 downto 0),
		SSw => SSw(31 downto 0),
		SSwSignal => SSw(32),
		ESdatain => ESdatain(191 downto 0),
		ESdataout => ESdataout(191 downto 0),
		ESw => ESw(23 downto 0),
		ESwSignal => ESw(24),
		en => stack_access_en,
		addr => MEMaddr_i(10 downto 0),
		datain => MEMdataout_X_i,
		dataout => MEMdata_stack_access,
		wrq => MEM_WRQ_XX
	);

end Structural;

