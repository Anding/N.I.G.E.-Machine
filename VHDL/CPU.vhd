-- CPU module
-- Andrew Read
-- Created 11 June 2011

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity CPU is
    Port ( 	rst : in STD_LOGIC;
				clk : in STD_LOGIC;
				irq : in STD_LOGIC;
				irv : in std_logic_vector(3 downto 0);
				rti : out std_logic; 
				-- Paramater and return stack storage
				PSaddr : OUT std_logic_vector(8 downto 0);
				PSdatain : IN std_logic_vector(31 downto 0);
				PSdataout : OUT std_logic_vector(31 downto 0);
				PSw : OUT std_logic_vector(0 downto 0);
				RSaddr : OUT std_logic_vector(8 downto 0);
				RSdatain : IN std_logic_vector(31 downto 0);
				RSdataout : OUT std_logic_vector(31 downto 0);
				RSw : OUT std_logic_vector(0 downto 0);
				-- 32 bit wide SRAM databus				
				MEMaddr : out STD_LOGIC_VECTOR (31 downto 0);			
			   MEMdatain_X : in STD_LOGIC_VECTOR (31 downto 0);			-- data at ADDR	
				MEMdatain_X_quick : in STD_LOGIC_VECTOR (31 downto 0);
			   MEMdataout_X : out STD_LOGIC_VECTOR (31 downto 0);		
			   MEM_WRQ_X : out STD_LOGIC;	
				MEMsize_X : out STD_LOGIC_VECTOR (1 downto 0);
				-- 32 bit wide AXI databus
				s_axi_awaddr : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
				s_axi_awvalid : OUT STD_LOGIC;
				s_axi_awready : IN STD_LOGIC;
				-- write
				s_axi_wdata : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
				s_axi_wstrb : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
				s_axi_wvalid : OUT STD_LOGIC;
				s_axi_wready : IN STD_LOGIC;
				-- write response
				s_axi_bresp : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
				s_axi_bvalid : IN STD_LOGIC;
				s_axi_bready : OUT STD_LOGIC;
				-- address read
				s_axi_araddr : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
				s_axi_arvalid : OUT STD_LOGIC;
				s_axi_arready : IN STD_LOGIC;
				-- read
				s_axi_rdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
				s_axi_rresp : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
				s_axi_rvalid : IN STD_LOGIC;
				s_axi_rready : OUT STD_LOGIC
				-- 8 bit wide PSDRAM databus
--			   MEMdatain_Y : in STD_LOGIC_VECTOR (7 downto 0);			
--				MEMdataout_Y : out STD_LOGIC_VECTOR (7 downto 0);		
--			   MEM_WRQ_Y : out STD_LOGIC;				
--			   MEM_REQ_Y : out STD_LOGIC;										
--			   MEM_RDY_Y : in STD_LOGIC;		
--				-- 16 bit wide PSDRAM bus
--			   MEMdatain_Z : in STD_LOGIC_VECTOR (15 downto 0);		
--			   MEMdataout_Z : out STD_LOGIC_VECTOR (15 downto 0);		
--			   MEM_WRQ_Z : out STD_LOGIC;				
--			   MEM_REQ_Z : out STD_LOGIC;										
--			   MEM_RDY_Z : in STD_LOGIC												
				);
end CPU;

architecture Structural of CPU is

	COMPONENT Datapath
	PORT(
		rst : IN std_logic;
		clk : IN std_logic;
		MEMdatain_X : in STD_LOGIC_VECTOR (31 downto 0);	
		Accumulator : IN std_logic_vector(31 downto 0);
		MicroControl : IN std_logic_vector(13 downto 0);
		AuxControl : IN std_logic_vector(1 downto 0);
		ReturnAddress : IN std_logic_vector(31 downto 0);          
		TOS : OUT std_logic_vector(31 downto 0);					-- unregistered, one cycle ahead of registered
		TOS_r : OUT STD_LOGIC_VECTOR (31 downto 0);				-- registered			
		NOS : OUT std_logic_vector(31 downto 0);
		TORS : OUT std_logic_vector(31 downto 0);
		equalzero: OUT std_logic;
		chip_RAM: OUT std_logic;
		PSaddr : OUT std_logic_vector(8 downto 0);
		PSdatain : IN std_logic_vector(31 downto 0);
		PSdataout : OUT std_logic_vector(31 downto 0);
		PSw : OUT std_logic_vector (0 downto 0);
		RSaddr : OUT std_logic_vector(8 downto 0);
		RSdatain : IN std_logic_vector(31 downto 0);
		RSdataout : OUT std_logic_vector(31 downto 0);
		RSw : OUT std_logic_vector (0 downto 0)
		);
	END COMPONENT;

	COMPONENT ControlUnit
	PORT(
		rst : IN std_logic;
		clk : IN std_logic;
		irq : IN std_logic;
		irv : in std_logic_vector(3 downto 0);
		rti : out std_logic;
		TOS : IN std_logic_vector(31 downto 0);
		TOS_r : in STD_LOGIC_VECTOR (31 downto 0);					
		NOS : IN std_logic_vector(31 downto 0);
		TORS : IN std_logic_vector(31 downto 0);
		equalzero : IN std_logic;
		chip_RAM : IN std_logic;
		MicroControl : OUT std_logic_vector(13 downto 0);
		AuxControl : OUT std_logic_vector(1 downto 0);
		Accumulator : OUT std_logic_vector(31 downto 0);
		ReturnAddress : OUT std_logic_vector(31 downto 0);
		MEMaddr : out STD_LOGIC_VECTOR (31 downto 0);			
		MEMdatain_X : in STD_LOGIC_VECTOR (31 downto 0);
		MEMdataout_X : out STD_LOGIC_VECTOR (31 downto 0);
		MEMsize_X : out STD_LOGIC_VECTOR (1 downto 0);	
		MEM_WRQ_X : out STD_LOGIC;							  		  
--		MEMdatain_Y : in STD_LOGIC_VECTOR (7 downto 0);			
--		MEMdataout_Y : out STD_LOGIC_VECTOR (7 downto 0);				
--		MEM_WRQ_Y : out STD_LOGIC;				
--		MEM_REQ_Y : out STD_LOGIC;										
--		MEM_RDY_Y : in STD_LOGIC;												  		  
--		MEMdatain_Z : in STD_LOGIC_VECTOR (15 downto 0);			
--		MEMdataout_Z : out STD_LOGIC_VECTOR (15 downto 0);		
--		MEM_WRQ_Z : out STD_LOGIC;				
--		MEM_REQ_Z : out STD_LOGIC;										
--		MEM_RDY_Z : in STD_LOGIC	
		s_axi_awaddr : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
		s_axi_awvalid : OUT STD_LOGIC;
		s_axi_awready : IN STD_LOGIC;
		-- write
		s_axi_wdata : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
		s_axi_wstrb : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
		s_axi_wvalid : OUT STD_LOGIC;
		s_axi_wready : IN STD_LOGIC;
		-- write response
		s_axi_bresp : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
		s_axi_bvalid : IN STD_LOGIC;
		s_axi_bready : OUT STD_LOGIC;
		-- address read
		s_axi_araddr : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
		s_axi_arvalid : OUT STD_LOGIC;
		s_axi_arready : IN STD_LOGIC;
		-- read
		s_axi_rdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
		s_axi_rresp : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
		s_axi_rvalid : IN STD_LOGIC;
		s_axi_rready : OUT STD_LOGIC										
		);
	END COMPONENT;

	signal	Accumulator : std_logic_vector(31 downto 0);
	signal	MicroControl :  std_logic_vector(13 downto 0);
	signal	AuxControl :  std_logic_vector(1 downto 0);
	signal	ReturnAddress :  std_logic_vector(31 downto 0);          
	signal	TOS, TOS_r :  std_logic_vector(31 downto 0);
	signal	NOS :  std_logic_vector(31 downto 0);
	signal	TORS :  std_logic_vector(31 downto 0);
	signal	equalzero : std_logic;
	signal	chip_RAM : std_logic;
--	signal 	MEMdatain_X : std_logic_vector(31 downto 0);

begin

--	MEMdatain_X_extended <= MEMdatain_X (31 downto 0) & MEMdatain_X_plus (7 downto 0);

	Inst_Datapath: Datapath PORT MAP(
		rst => rst,
		clk => clk,
		MEMdatain_X => MEMdatain_X,
		Accumulator => Accumulator,
		MicroControl => MicroControl,
		AuxControl => AuxControl,
		ReturnAddress => ReturnAddress,
		TOS => TOS,
		TOS_r => TOS_r,
		NOS => NOS,
		TORS => TORS,
		equalzero => equalzero,
		chip_RAM => chip_RAM,
		PSaddr => PSaddr,
		PSdatain => PSdatain,
		PSdataout => PSdataout,
		PSw => PSw,
		RSaddr => RSaddr,
		RSdatain => RSdatain,
		RSdataout => RSdataout,
		RSw => RSw
	);
	
	Inst_ControlUnit: ControlUnit PORT MAP(
		rst => rst,
		clk => clk,
		irq => irq,
		rti => rti,
		irv => irv,
		TOS => TOS,
		TOS_r => TOS_r,
		NOS => NOS,
		TORS => TORS,
		equalzero=> equalzero,
		chip_RAM => chip_RAM,
		MicroControl => MicroControl,
		AuxControl => AuxControl,
		Accumulator => Accumulator,
		ReturnAddress => ReturnAddress,
		MEMaddr => MEMaddr,
		MEMdatain_X => MEMdatain_X_quick,
		MEMdataout_X => MEMdataout_X,
		MEM_WRQ_X => MEM_WRQ_X,
		MEMsize_X => MEMsize_X,
--		MEMdatain_Y => MEMdatain_Y,
--		MEMdataout_Y => MEMdataout_Y,
--		MEM_WRQ_Y => MEM_WRQ_Y,
--		MEM_REQ_Y => MEM_REQ_Y,
--		MEM_RDY_Y => MEM_RDY_Y,
--		MEMdatain_Z => MEMdatain_Z,
--		MEMdataout_Z => MEMdataout_Z,
--		MEM_WRQ_Z => MEM_WRQ_Z,
--		MEM_REQ_Z => MEM_REQ_Z,
--		MEM_RDY_Z => MEM_RDY_Z,
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
		s_axi_rready => s_axi_rready
	);

end Structural;

