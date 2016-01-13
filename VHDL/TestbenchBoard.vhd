
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.STD_LOGIC_ARITH.ALL;
 
ENTITY TestbenchBoard IS
END TestbenchBoard;
 
ARCHITECTURE behavior OF TestbenchBoard IS 
 
    -- Component Declaration for the Unit Under Test (UUT)
 
    COMPONENT Board_Nexys4
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
    
	COMPONENT cellram
	PORT(
		clk : IN std_logic;
		adv_n : IN std_logic;
		cre : IN std_logic;
		ce_n : IN std_logic;
		oe_n : IN std_logic;
		we_n : IN std_logic;
		lb_n : IN std_logic;
		ub_n : IN std_logic;
		addr : IN std_logic_vector(22 to 0);    
		dq : INOUT std_logic_vector(15 to 0);      
		o_wait : OUT std_logic
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
	
	--BiDirs
   signal DATA_SDRAM : std_logic_vector(15 downto 0);
	signal PHYMDIO : STD_LOGIC;
	
 	--Outputs
   signal RGB : std_logic_vector(11 downto 0);
   signal HSync : std_logic;
   signal VSync : std_logic;
   signal ADDR_SDRAM : std_logic_vector(23 downto 1);
	signal addr : std_logic_vector(22 downto 0);
   signal OE_SDRAM : std_logic;
   signal WE_SDRAM : std_logic;
   signal ADV_SDRAM : std_logic;
   signal CLK_SDRAM : std_logic;
   signal UB_SDRAM : std_logic;
   signal LB_SDRAM : std_logic;
   signal CE_SDRAM : std_logic;
   signal CRE_SDRAM : std_logic;
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
   uut: Board_Nexys4 PORT MAP (
          CLK_IN => CLK_IN,
          RGB => RGB,
          HSync => HSync,
          VSync => VSync,
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
		  
  instCellRAM: entity work.cellram PORT MAP (
          clk => CLK_SDRAM,
          adv_n => ADV_SDRAM,
          cre => CRE_SDRAM,
          o_wait => WAIT_SDRAM,
          ce_n => CE_SDRAM,
          oe_n => OE_SDRAM,
          we_n => WE_SDRAM,
          lb_n => LB_SDRAM,
          ub_n => UB_SDRAM,
          addr => addr,
          dq => DATA_SDRAM
        );

	Inst_SPIslave3: entity work.SPIslave3 PORT MAP(
		CS => SD_CS,
		MOSI => MOSI,
		MISO => MISO,
		SCK => SCK,
		mode => "00"
		);
	
	addr <= ADDR_SDRAM;
	RXD_S0 <= tx_line;
	PS2C <= PS2C_line;
	PS2D <= PS2D_line;
	
	-- Ethernet loopback test
   PHYCRS <= PHYTXEN;
   PHYRXD <= PHYTXD;
	
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
