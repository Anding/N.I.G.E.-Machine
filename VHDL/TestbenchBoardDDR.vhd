
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.STD_LOGIC_ARITH.ALL;
 
ENTITY TestbenchBoardDDR IS
END TestbenchBoardDDR;
 
ARCHITECTURE behavior OF TestbenchBoardDDR IS 
 
    -- Component Declaration for the Unit Under Test (UUT)
 
    COMPONENT Board_Nexys4DDR
    Port ( CLK_IN : in  STD_LOGIC;
			  RGB : out  STD_LOGIC_VECTOR (11 downto 0);
           HSync : out  STD_LOGIC;
           VSync : out  STD_LOGIC;
	    --
	-- DDR RAM
	    SDRAM_A : out std_logic_vector(13 downto 0);
	    SDRAM_BA : out std_logic_vector(2 downto 0);
	    SDRAM_CKE      : out std_logic;
	    SDRAM_CK       : out std_logic;
	    SDRAM_nCK	   : out std_logic;
	    SDRAM_DQ       : inout std_logic_vector(15 downto 0);  
	    SDRAM_DQS	   : inout std_logic_vector(1 downto 0);
	    --SDRAM_nDQS	   : inout std_logic_vector(1 downto 0);
	    SDRAM_UDQM    : out std_logic;
	    SDRAM_LDQM    : out std_logic;
	    SDRAM_nCAS     : out std_logic;
	    SDRAM_nCS      : out std_logic;
	    SDRAM_nRAS     : out std_logic;
	    SDRAM_nWE      : out std_logic;
	    SDRAM_ODT      : out std_logic;
	    --
	  RXD_S0 : in STD_LOGIC;
	  TXD_S0 : out STD_LOGIC;
	  PS2C : in STD_LOGIC;
	  PS2D : in STD_LOGIC;
	  CPUreset : in STD_LOGIC;
	  -- SPI
	  SCK : out STD_LOGIC;
	  MOSI : out STD_LOGIC;
	  MISO : in STD_LOGIC;
	  SD_CS : out STD_LOGIC;
	  SD_CD : in STD_LOGIC;
	  SD_RESET : out STD_LOGIC;
	  -- Board
	  SW : in STD_LOGIC_VECTOR (15 downto 0);
	  sevenseg : out STD_LOGIC_VECTOR (6 downto 0);
	  anode : out STD_LOGIC_VECTOR (7 downto 0);
	  RGB1_Red : out STD_LOGIC;
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
    END COMPONENT;
    
COMPONENT ddr2 
PORT(
       ck		: IN std_logic;
       ck_n	: IN std_logic;
       cke	: IN std_logic;
       cs_n	: IN std_logic;
       ras_n	: IN std_logic;
       cas_n : IN std_logic;
       we_n	: IN std_logic;
       dm_rdqs : INOUT  std_logic_vector(1 downto 0);
       ba		: IN std_logic_vector (2 downto 0);
       addr	: IN std_logic_vector(12 downto 0);
       dq		: INOUT std_logic_vector(15 downto 0);
       dqs	: INOUT std_logic_vector(1 downto 0);
       dqs_n : INOUT std_logic_vector(1 downto 0);
       rdqs_n  : OUT std_logic_vector(1 downto 0);
       odt	: IN std_logic
       );
END COMPONENT;

   --Inputs
	signal CPUreset : std_logic := '1';
   signal CLK_IN : std_logic := '0';
   signal WAIT_SDRAM : std_logic := '0';
	signal RXD_S0 : std_logic := '1';
	signal PS2C :  std_logic := '1';
	signal PS2D :  std_logic := '1';	
	signal EppAstb: std_logic := '0';        
	signal EppDstb: std_logic := '0';        
	signal EppWr  : std_logic := '0';        
	signal EppDB  : std_logic_vector(7 downto 0) := (others=>'0');
	signal SW : std_logic_vector(15 downto 0) := (others=>'0');
	signal MISO : STD_LOGIC := '0';	
	signal SD_CD : STD_LOGIC := '0';
	signal PHYCRS : STD_LOGIC := '0';
	signal PHYRXERR : STD_LOGIC := '0';
	signal PHYRXD : STD_LOGIC_VECTOR (1 downto 0) := (others=>'0');
	signal PHYINTN : STD_LOGIC := '0';
	
	signal SDRAM_A 	: std_logic_vector(13 downto 0);
	signal SDRAM_BA 	: std_logic_vector(2 downto 0);
	signal SDRAM_CK      : std_logic;  
	signal SDRAM_nCK     : std_logic;
	signal SDRAM_DQ       : std_logic_vector(15 downto 0);   
	signal SDRAM_DM       : std_logic_vector(1 downto 0);   
	signal SDRAM_DQS      : std_logic_vector(1 downto 0);
	signal SDRAM_nDQS     : std_logic_vector(1 downto 0); 
	signal SDRAM_nRDQS    : std_logic_vector(1 downto 0);
	signal SDRAM_UDQM    : std_logic;
	signal SDRAM_LDQM    : std_logic;
	signal SDRAM_nCAS     : std_logic;
	signal SDRAM_nCS      : std_logic;
	signal SDRAM_nRAS     : std_logic;
	signal SDRAM_nWE      : std_logic; 
	
	signal SDRAM_CKE      : std_logic;      
	signal SDRAM_ODT      : std_logic;	
   
	--BiDirs
   signal DATA_SDRAM : std_logic_vector(15 downto 0);
	signal PHYMDIO : STD_LOGIC;
	
 	--Outputs
   signal RGB : std_logic_vector(11 downto 0);
   signal HSync : std_logic;
   signal VSync : std_logic;

	signal TXD_S0 : std_logic;
	signal RGB1_Red : std_logic;
	signal SCK : STD_LOGIC;
	signal MOSI : STD_LOGIC;
	signal SD_CS :  STD_LOGIC;
	signal SD_RESET : STD_LOGIC ;		
	signal PHYTXEN : STD_LOGIC;
	signal PHYTXD : STD_LOGIC_VECTOR (1 downto 0);
	signal PHYCLK50MHZ : STD_LOGIC;	
	signal PHYRSTN : STD_LOGIC;	
	signal PHYMDC : STD_LOGIC;	
	
   -- Clock period definitions
   constant CLK_IN_period : time := 10 ns;

	signal tx_line : std_logic := '1';
	signal PS2C_line : std_logic := '1';
	signal PS2D_line : std_logic := '1';

BEGIN

	-- Instantiate the Unit Under Test (UUT)
   prj: Board_Nexys4DDR PORT MAP (
          CLK_IN => CLK_IN,
          RGB => RGB,
          HSync => HSync,
          VSync => VSync,
	    SDRAM_A => SDRAM_A,
          SDRAM_BA => SDRAM_BA,
          SDRAM_CKE => SDRAM_CKE,
          SDRAM_CK => SDRAM_CK,
          SDRAM_nCK => SDRAM_nCK,
          SDRAM_DQ  => SDRAM_DQ,
          SDRAM_DQS => SDRAM_DQS,
          SDRAM_UDQM => SDRAM_UDQM,
          SDRAM_LDQM => SDRAM_LDQM,
          SDRAM_nCAS => SDRAM_nCAS,
          SDRAM_nCS => SDRAM_nCS,
          SDRAM_nRAS => SDRAM_nRAS,
          SDRAM_nWE => SDRAM_nWE,
          SDRAM_ODT => SDRAM_ODT,
			 TXD_S0 => TXD_S0,  
			 RXD_S0 => RXD_S0,	-- RXD_S0
			 PS2C => PS2C,
			 PS2D => PS2D,
			 SCK => SCK,
			 MOSI => MOSI,
			 MISO => MOSI,			-- MISO
			 SD_CS => SD_CS,
			 SD_CD => SD_CD,
			 SD_RESET => SD_RESET,
			 CPUreset => CPUreset,
			 SW => SW,
			 sevenseg => open,
			 anode => open,
			 RGB1_Red => RGB1_Red,
			 PHYMDC => PHYMDC,
          PHYMDIO => PHYMDIO,
          PHYRSTN => PHYRSTN,
          PHYCRS => PHYCRS,
          PHYRXERR => PHYRXERR,
          PHYRXD => PHYRXD,
          PHYTXEN => PHYTXEN,
          PHYTXD => PHYTXD,
          PHYCLK50MHZ => PHYCLK50MHZ,
          PHYINTN => PHYINTN
        );
	 
--	inst_ddr2: ddr2
--	PORT MAP (
--    ck => SDRAM_CK, 
--	ck_n => SDRAM_nCK, 
--	cke => SDRAM_CKE, 
--	cs_n => SDRAM_nCS, 
--	ras_n => SDRAM_nRAS, 
--	cas_n => SDRAM_nCAS, 
--	we_n => SDRAM_nWE, 
--	dm_rdqs => SDRAM_DM, 
--	ba => SDRAM_BA, 
--	addr => SDRAM_A(12 downto 0), 
--	dq => SDRAM_DQ, 
--	dqs => SDRAM_DQS,
--	dqs_n => SDRAM_nDQS,
--	rdqs_n => SDRAM_nRDQS,
--	odt => SDRAM_ODT
--	);		

--	Inst_SPIslave3: entity work.SPIslave3 PORT MAP(
--		CS => SD_CS,
--		MOSI => MOSI,
--		MISO => MISO,
--		SCK => SCK,
--		mode => "00"
--		);
	
	RXD_S0 <= tx_line;
	PS2C <= PS2C_line;
	PS2D <= PS2D_line;
	
	-- Ethernet loopback test
   PHYCRS <= PHYTXEN;
   PHYRXD <= PHYTXD;
   
   SDRAM_DM(0) <= SDRAM_LDQM;
   SDRAM_DM(1) <= SDRAM_UDQM; 
	
   -- Clock process definitions
   CLK_IN_process :process
   begin
		CLK_IN <= '0';
		wait for CLK_IN_period/2;
		CLK_IN <= '1';
		wait for CLK_IN_period/2;
   end process;


   -- Stimulus process
   stim_proc: process
	
	procedure 
		send_RS232 (
			signal tx_line_i: out std_logic;
			data: in std_logic_vector					-- usually 8 bits
		) is
		constant bit_time: time := 1.0 sec / 57600;
		begin
			-- Send the start bit
			 tx_line_i <= '0';
			 wait for bit_time;
			-- Send the data bits, least significant first
			 for i in data'reverse_range loop
				tx_line_i <= data(i);
				wait for bit_time;
			 end loop;
			-- Send the stop bit
			 tx_line_i <= '1';
			 wait for bit_time;
		end;

	procedure
		send_PS2 (
			signal PS2C_line_i: out std_logic;
			signal PS2D_line_i: out std_logic;
			data: in std_logic_vector(7 downto 0)
		) is
		constant half_bit_time: time := 1.0 sec / 24000;	-- 12 kHz
		begin
			-- Send the start bit
				PS2D_line_i <= '0';
				wait for half_bit_time;
				PS2C_line_i <= '0';
				wait for half_bit_time;
				PS2C_line_i <= '1';
			-- Send the data bits, least significant first
			 for i in data'reverse_range loop
				PS2D_line_i <= data(i);
				wait for half_bit_time;
				PS2C_line_i <= '0';
				wait for half_bit_time;
				PS2C_line_i <= '1';
			 end loop;
			 -- Send the parity bit
				PS2D_line_i <= data(0) xor data(1) xor data(2) xor data(3) xor data(4) xor data(5) xor data(6) xor data(7) xor '1';
				wait for half_bit_time;
				PS2C_line_i <= '0';
				wait for half_bit_time;
				PS2C_line_i <= '1';
			-- Send the stop bit
				PS2D_line_i <= '1';
			 	wait for half_bit_time;
				PS2C_line_i <= '0';
				wait for half_bit_time;
				PS2C_line_i <= '1';
		end;

   begin		
		wait for 50 us;		
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		send_RS232(tx_line, CONV_STD_LOGIC_VECTOR(192,8));	
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
--		wait for 20 ms;
--		send_PS2(PS2C_line, PS2D_line, "00110010");
		
		
      -- insert stimulus here 

      wait;
   end process;

END;
