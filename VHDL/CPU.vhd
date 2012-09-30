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
				PSaddr : OUT std_logic_vector(8 downto 0);
				PSdatain : IN std_logic_vector(31 downto 0);
				PSdataout : OUT std_logic_vector(31 downto 0);
				PSw : OUT std_logic_vector(0 downto 0);
				RSaddr : OUT std_logic_vector(8 downto 0);
				RSdatain : IN std_logic_vector(31 downto 0);
				RSdataout : OUT std_logic_vector(31 downto 0);
				RSw : OUT std_logic_vector(0 downto 0);
				MEMaddr : out STD_LOGIC_VECTOR (31 downto 0);			-- 8 bit wide SRAM prog memory bus
			   MEMdatain_X : in STD_LOGIC_VECTOR (7 downto 0);			-- 8 bit wide SRAM bus
			   MEMdataout_XY : out STD_LOGIC_VECTOR (7 downto 0);		-- 8 bit wide SRAM bus
			   MEM_WRQ_X : out STD_LOGIC;				-- 8 bit wide SRAM bus			  		  
			   MEMdatain_Y : in STD_LOGIC_VECTOR (7 downto 0);			-- 8 bit wide DRAM bus
			   MEM_WRQ_Y : out STD_LOGIC;				-- 8 bit wide DRAM bus
			   MEM_REQ_Y : out STD_LOGIC;										-- 8 bit wide DRAM bus
			   MEM_RDY_Y : in STD_LOGIC;										-- 8 bit wide DRAM bus		  		  
			   MEMdatain_Z : in STD_LOGIC_VECTOR (15 downto 0);		-- 16 bit wide DRAM bus
			   MEMdataout_Z : out STD_LOGIC_VECTOR (15 downto 0);		-- 16 bit wide DRAM bus
			   MEM_WRQ_Z : out STD_LOGIC;				-- 16 bit wide DRAM bus
			   MEM_REQ_Z : out STD_LOGIC;										-- 16 bit wide DRAM bus
			   MEM_RDY_Z : in STD_LOGIC										-- 16 bit wide DRAM bus			
				);
end CPU;

architecture Structural of CPU is

	COMPONENT Datapath
	PORT(
		rst : IN std_logic;
		clk : IN std_logic;
		Immediate : IN std_logic_vector(31 downto 0);
		MicroControl : IN std_logic_vector(13 downto 0);
		AuxControl : IN std_logic_vector(0 downto 0);
		ReturnAddress : IN std_logic_vector(31 downto 0);          
		TOS : OUT std_logic_vector(31 downto 0);
		NOS : OUT std_logic_vector(31 downto 0);
		TORS : OUT std_logic_vector(31 downto 0);
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
		NOS : IN std_logic_vector(31 downto 0);
		TORS : IN std_logic_vector(31 downto 0);
		MicroControl : OUT std_logic_vector(13 downto 0);
		AuxControl : OUT std_logic_vector(0 downto 0);
		Immediate : OUT std_logic_vector(31 downto 0);
		ReturnAddress : OUT std_logic_vector(31 downto 0);
		MEMaddr : out STD_LOGIC_VECTOR (31 downto 0);			-- 8 bit wide SRAM prog memory bus
		MEMdatain_X : in STD_LOGIC_VECTOR (7 downto 0);			-- 8 bit wide SRAM bus
		MEMdataout_XY : out STD_LOGIC_VECTOR (7 downto 0);		-- 8 bit wide SRAM bus
		MEM_WRQ_X : out STD_LOGIC;				-- 8 bit wide SRAM bus			  		  
		MEMdatain_Y : in STD_LOGIC_VECTOR (7 downto 0);			-- 8 bit wide DRAM bus
		MEM_WRQ_Y : out STD_LOGIC;				-- 8 bit wide DRAM bus
		MEM_REQ_Y : out STD_LOGIC;										-- 8 bit wide DRAM bus
		MEM_RDY_Y : in STD_LOGIC;										-- 8 bit wide DRAM bus		  		  
		MEMdatain_Z : in STD_LOGIC_VECTOR (15 downto 0);			-- 16 bit wide DRAM bus
		MEMdataout_Z : out STD_LOGIC_VECTOR (15 downto 0);		-- 16 bit wide DRAM bus
		MEM_WRQ_Z : out STD_LOGIC;				-- 16 bit wide DRAM bus
		MEM_REQ_Z : out STD_LOGIC;										-- 16 bit wide DRAM bus
		MEM_RDY_Z : in STD_LOGIC											-- 16 bit wide DRAM bus
		);
	END COMPONENT;

	signal	Immediate : std_logic_vector(31 downto 0);
	signal	MicroControl :  std_logic_vector(13 downto 0);
	signal	AuxControl :  std_logic_vector(0 downto 0);
	signal	ReturnAddress :  std_logic_vector(31 downto 0);          
	signal	TOS :  std_logic_vector(31 downto 0);
	signal	NOS :  std_logic_vector(31 downto 0);
	signal	TORS :  std_logic_vector(31 downto 0);

begin

	Inst_Datapath: Datapath PORT MAP(
		rst => rst,
		clk => clk,
		Immediate => Immediate,
		MicroControl => MicroControl,
		AuxControl => AuxControl,
		ReturnAddress => ReturnAddress,
		TOS => TOS,
		NOS => NOS,
		TORS => TORS,
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
		-- err => err,
		TOS => TOS,
		NOS => NOS,
		TORS => TORS,
		MicroControl => MicroControl,
		AuxControl => AuxControl,
		Immediate => Immediate,
		ReturnAddress => ReturnAddress,
		MEMaddr => MEMaddr,
		MEMdatain_X => MEMdatain_X,
		MEMdataout_XY => MEMdataout_XY,
		MEM_WRQ_X => MEM_WRQ_X,
		MEMdatain_Y => MEMdatain_Y,
		MEM_WRQ_Y => MEM_WRQ_Y,
		MEM_REQ_Y => MEM_REQ_Y,
		MEM_RDY_Y => MEM_RDY_Y,
		MEMdatain_Z => MEMdatain_Z,
		MEMdataout_Z => MEMdataout_Z,
		MEM_WRQ_Z => MEM_WRQ_Z,
		MEM_REQ_Z => MEM_REQ_Z,
		MEM_RDY_Z => MEM_RDY_Z
	);

end Structural;

