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
				-- Memory address bus (common to all memory channels)
				MEMaddr : out STD_LOGIC_VECTOR (31 downto 0);			
				-- 32 bit wide SRAM databus	
			   MEMdatain_X : in STD_LOGIC_VECTOR (31 downto 0);			-- data at ADDR	
				MEMdatain_X_plus : in STD_LOGIC_VECTOR (31 downto 0);		-- data at ADDR+1 (for load literal instructions)
			   MEMdataout_X : out STD_LOGIC_VECTOR (31 downto 0);		
			   MEM_WRQ_X : out STD_LOGIC;	
				MEMsize_X : out STD_LOGIC_VECTOR (1 downto 0);	
				MEMsize_Xp : out STD_LOGIC_VECTOR (1 downto 0);	
				-- 8 bit wide PSDRAM databus
			   MEMdatain_Y : in STD_LOGIC_VECTOR (7 downto 0);			
				MEMdataout_Y : out STD_LOGIC_VECTOR (7 downto 0);		
			   MEM_WRQ_Y : out STD_LOGIC;				
			   MEM_REQ_Y : out STD_LOGIC;										
			   MEM_RDY_Y : in STD_LOGIC;		
				-- 16 bit wide PSDRAM bus
			   MEMdatain_Z : in STD_LOGIC_VECTOR (15 downto 0);		
			   MEMdataout_Z : out STD_LOGIC_VECTOR (15 downto 0);		
			   MEM_WRQ_Z : out STD_LOGIC;				
			   MEM_REQ_Z : out STD_LOGIC;										
			   MEM_RDY_Z : in STD_LOGIC												
				);
end CPU;

architecture Structural of CPU is

	COMPONENT Datapath
	PORT(
		rst : IN std_logic;
		clk : IN std_logic;
		MEMdatain_X : in STD_LOGIC_VECTOR (31 downto 0);	
		MEMdatain_X_plus : in STD_LOGIC_VECTOR (31 downto 0);
		Accumulator : IN std_logic_vector(31 downto 0);
		MicroControl : IN std_logic_vector(13 downto 0);
		AuxControl : IN std_logic_vector(2 downto 0);
		ReturnAddress : IN std_logic_vector(31 downto 0);          
		TOS : OUT std_logic_vector(31 downto 0);					-- unregistered, one cycle ahead of registered
		TOS_r : OUT STD_LOGIC_VECTOR (31 downto 0);				-- registered			
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
		TOS_r : in STD_LOGIC_VECTOR (31 downto 0);					
		NOS : IN std_logic_vector(31 downto 0);
		TORS : IN std_logic_vector(31 downto 0);
		MicroControl : OUT std_logic_vector(13 downto 0);
		AuxControl : OUT std_logic_vector(2 downto 0);
		Accumulator : OUT std_logic_vector(31 downto 0);
		ReturnAddress : OUT std_logic_vector(31 downto 0);
		MEMaddr : out STD_LOGIC_VECTOR (31 downto 0);			
		MEMdatain_X_extended : in STD_LOGIC_VECTOR (39 downto 0);
		MEMdataout_X : out STD_LOGIC_VECTOR (31 downto 0);
		MEMsize_X : out STD_LOGIC_VECTOR (1 downto 0);
		MEMsize_Xp : out STD_LOGIC_VECTOR (1 downto 0);	
		MEM_WRQ_X : out STD_LOGIC;							  		  
		MEMdatain_Y : in STD_LOGIC_VECTOR (7 downto 0);			
		MEMdataout_Y : out STD_LOGIC_VECTOR (7 downto 0);				
		MEM_WRQ_Y : out STD_LOGIC;				
		MEM_REQ_Y : out STD_LOGIC;										
		MEM_RDY_Y : in STD_LOGIC;												  		  
		MEMdatain_Z : in STD_LOGIC_VECTOR (15 downto 0);			
		MEMdataout_Z : out STD_LOGIC_VECTOR (15 downto 0);		
		MEM_WRQ_Z : out STD_LOGIC;				
		MEM_REQ_Z : out STD_LOGIC;										
		MEM_RDY_Z : in STD_LOGIC											
		);
	END COMPONENT;

	signal	Accumulator : std_logic_vector(31 downto 0);
	signal	MicroControl :  std_logic_vector(13 downto 0);
	signal	AuxControl :  std_logic_vector(2 downto 0);
	signal	ReturnAddress :  std_logic_vector(31 downto 0);          
	signal	TOS, TOS_r :  std_logic_vector(31 downto 0);
	signal	NOS :  std_logic_vector(31 downto 0);
	signal	TORS :  std_logic_vector(31 downto 0);
	signal 	MEMdatain_X_extended : std_logic_vector(39 downto 0);

begin

	MEMdatain_X_extended <= MEMdatain_X (31 downto 0) & MEMdatain_X_plus (7 downto 0);

	Inst_Datapath: Datapath PORT MAP(
		rst => rst,
		clk => clk,
		MEMdatain_X => MEMdatain_X,
		MEMdatain_X_plus => MEMdatain_X_plus,
		Accumulator => Accumulator,
		MicroControl => MicroControl,
		AuxControl => AuxControl,
		ReturnAddress => ReturnAddress,
		TOS => TOS,
		TOS_r => TOS_r,
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
		TOS => TOS,
		TOS_r => TOS_r,
		NOS => NOS,
		TORS => TORS,
		MicroControl => MicroControl,
		AuxControl => AuxControl,
		Accumulator => Accumulator,
		ReturnAddress => ReturnAddress,
		MEMaddr => MEMaddr,
		MEMdatain_X_extended => MEMdatain_X_extended,
		MEMdataout_X => MEMdataout_X,
		MEM_WRQ_X => MEM_WRQ_X,
		MEMsize_X => MEMsize_X,
		MEMsize_Xp => MEMsize_Xp,
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

end Structural;

