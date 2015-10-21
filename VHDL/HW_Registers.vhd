-- Hardware registers
-- Andrew Read
-- Created 20 August 2011

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity HW_Registers is
    Port ( clk : in  STD_LOGIC;
           rst : in  STD_LOGIC;
			  -- SD card interface
			  SD_datain : in std_logic_vector(7 downto 0);			
			  SD_dataout : out std_logic_vector(7 downto 0);
			  SD_control : out std_logic_vector(3 downto 0);
			  SD_status : in std_logic_vector(3 downto 0);
			  SD_wr : out std_logic;
			  SD_divide : out std_logic_vector(7 downto 0);
			  -- VGA adapter
			  txt_zero : out std_logic_vector(23 downto 0);			-- base address of character graphics memory
			  mode	 	: out STD_LOGIC_VECTOR (4 downto 0);		-- graphics adapter mode
			  background : out STD_LOGIC_VECTOR (15 downto 0);		-- background color for graphics adapter text mode
			  interlace	: out	STD_LOGIC_VECTOR (3 downto 0);		-- number of interlace scan lines between character rows
			  charHeight: out	STD_LOGIC_VECTOR (3 downto 0);		-- height of a character in pixels LESS ONE 
			  charWidth: out	STD_LOGIC_VECTOR (3 downto 0);		-- width of a character in pixels LESS ONE
			  VGArows : out STD_LOGIC_VECTOR (7 downto 0);			-- number of complete character columns displayed on the screen					  
			  VGAcols : out STD_LOGIC_VECTOR (7 downto 0);			-- number of complete character columns displayed on the screen
			  VBLANK : in std_logic;										-- VGA vertical blank			
			  -- interrupt controller
			  irq_mask : out STD_LOGIC_VECTOR(15 downto 1);			-- mask for interrupt controller
			  -- RS232 port
			  RS232_rx_S0 : in std_logic_vector(7 downto 0);		-- RS232 port 0 datain
			  RS232_tx_S0 : out std_logic_vector(7 downto 0);		-- RS232 port 0 dataout
			  RS232_wr_S0 : out std_logic;								-- RS232 port 0 write request
																					-- RS232_wr_S0 needs to get triggered for one cycle only when a 
																					--   data byte is places in the register RS232_tx_S0
			  RS232_TBE_S0 : in std_logic;								-- RS232 port 0 Transfer Bus Enable (TBE) signal
			  RS232_RDA_S0 : in std_logic;								-- RS232 port 0 Read Data Available (RDA) signal
			  RS232_DIVIDE_S0 : out std_logic_vector(31 downto 0);	-- RS232 port 0 baud rate setting (DIVIDE = Clock (Baud+1))
			  -- PS2 keyboard port
			  PS2_data : in std_logic_vector(7 downto 0);			-- PS/2 port
			  -- counters
			  counter_ms : in std_logic_vector(31 downto 0);		-- 32 bit millisecond timer
			  counter_clk : in std_logic_vector(31 downto 0);		-- 32 bit clock timer
			  -- Ethernet MAC
			  MACready : in std_logic;
			  MACread_enable : out std_logic;
			  MACdata : in std_logic_vector(1 downto 0);
			  -- Nexys board
			  ssData	: out std_logic_vector(31 downto 0);			-- data for seven segment display
			  SW	: in std_logic_vector(15 downto 0);					-- switches onboard Nexys2
			  -- CPU system memory channel
			  en : in STD_LOGIC;													-- Enable is set by board level logic depending on higher bit of 
																											-- the address to enable this piece of memory when it is addressed	  
           addr : in  STD_LOGIC_VECTOR (10 downto 0);				-- Low bits only of address bus.  Higher bits control "en"
           datain : in  STD_LOGIC_VECTOR (31 downto 0);			
           dataout : out  STD_LOGIC_VECTOR (31 downto 0);
           wrq : in  STD_LOGIC_VECTOR (0 downto 0)
			  );
end HW_Registers;

architecture Behavioral of HW_Registers is

	-- registers for writeable signals
	signal SD_dataout_r : std_logic_vector(7 downto 0);
	signal SD_control_r : std_logic_vector(3 downto 0);
	signal SD_divide_r : std_logic_vector(7 downto 0);			
	signal txt_zero_r : std_logic_vector(23 downto 0);		
	signal irq_mask_r : std_logic_vector(15 downto 1);
	signal RS232_tx_r_S0 : std_logic_vector(7 downto 0);	
	signal DIVIDE_r_S0	: std_logic_vector(31 downto 0);
	signal background_r : std_logic_vector(15 downto 0);
	signal mode_r : std_logic_vector(4 downto 0);
	signal interlace_r	: STD_LOGIC_VECTOR (3 downto 0);
	signal charHeight_r: STD_LOGIC_VECTOR (3 downto 0);
	signal charWidth_r: STD_LOGIC_VECTOR (3 downto 0);	
	signal VGArows_r : STD_LOGIC_VECTOR (7 downto 0);						  
	signal VGAcols_r : STD_LOGIC_VECTOR (7 downto 0);		
	signal ssData_r : std_logic_vector(31 downto 0);
	
	signal addr_i : std_logic_vector(7 downto 0);					-- local address bus
	signal clk_i : std_logic;												-- local slow clock for debounce

	
	-- pipeline registers for hardware write
	signal en_r : std_logic :='0';
	signal addr_r : std_logic_vector(7 downto 0) :=(others=>'0');
	signal datain_r : std_logic_vector(31 downto 0) :=(others=>'0');
	signal wrq_r : std_logic_vector (0 downto 0) :="0";
	-- pipeline registers for hardware read
	signal RS232_rx_S0_r : std_logic_vector(7 downto 0);
	signal RS232_TBE_S0_r, RS232_RDA_S0_r : std_logic;	
	signal SW_r : std_logic_vector(15 downto 0);
	signal PS2_data_r : std_logic_vector(7 downto 0);
	signal SD_status_r : std_logic_vector(3 downto 0);
	signal SD_datain_r : std_logic_vector(7 downto 0);
	signal VBLANK_r : std_logic;
	
	signal dataout_i : STD_LOGIC_VECTOR (31 downto 0);
	
	constant blank3 : std_logic_vector(23 downto 0) := (others=>'0');
	constant blank2 : std_logic_vector(15 downto 0) := (others=>'0');
	constant blank1 : std_logic_vector(7 downto 0) := (others=>'0');
	
begin
	-- connect writable registers to outputs
	SD_dataout <= SD_dataout_r;
	SD_control <= SD_control_r;
	SD_divide <= SD_divide_r;
	txt_zero <= txt_zero_r;
	RS232_tx_S0 <= RS232_tx_r_S0;
	background <= background_r;
	mode <= mode_r;
	interlace <= interlace_r;
	charHeight <= charHeight_r;
	charWidth <= charWidth_r;	
	VGArows <= VGArows_r;						  
	VGAcols <= VGAcols_r;	
	RS232_DIVIDE_S0 <= DIVIDE_r_S0;
	ssData <= ssData_r;
	addr_i <= addr(7 downto 0);
	clk_i <= counter_clk(13);
	irq_mask <= irq_mask_r;
	
	-- update writable registers
	
	write_pipeline: process											-- pipeline for efficiency
	begin
		wait until rising_edge(clk);
		en_r <= en;
		addr_r <= addr(7 downto 0);
		datain_r <= datain;
		wrq_r <= wrq;
	end process;
	
	write_register: process
	begin
		wait until rising_edge(clk);
		if rst = '1' then												-- register initializion
			SD_dataout_r <= (others=>'0');	
			SD_control_r <= (others=>'0');	
			SD_divide_r <= "11111111";								-- divide by 254	
			DIVIDE_r_S0 <= CONV_STD_LOGIC_VECTOR(1736,32);		-- 10416 = 9600 BAUD at 100 MHz		
			irq_mask_r <= "000000000000111";						-- "000000000000111";
			txt_zero_r <= X"040600";									
			background_r <= X"0000";
			mode_r <= "11010";										-- 11010
			interlace_r <= X"2";
			charHeight_r <= X"7";
			charWidth_r <= X"7";	
			VGArows_r <= CONV_STD_LOGIC_VECTOR(60,8);						  
			VGAcols_r <= CONV_STD_LOGIC_VECTOR(100,8);
			RS232_tx_r_S0 <= (others=>'0');	
			ssData_r <= (others=>'0');
			
		elsif en_r = '1' and wrq_r = "1" then					-- writable registers
				case addr_r is			
					when x"00" =>										-- TEXT_ZERO 
						txt_zero_r <= datain_r(23 downto 0);

					when x"08" =>										-- char/text graphics background colour
						background_r <= datain_r(15 downto 0);		
						
					when x"0C" =>										-- graphics mode
						mode_r <= datain_r(4 downto 0);					
						
					when x"14" =>										-- RS232_S0 data_out
						RS232_tx_r_S0 <= datain_r(7 downto 0);	
				
					when x"18" =>										-- DIVIDE_S0
						DIVIDE_r_S0 <= datain_r(31 downto 0);
						
					when x"2C" =>										-- IRQ_mask
						IRQ_mask_r <= datain_r(15 downto 1);
					
					when x"30" =>										-- SEVENSEG
						ssData_r <= datain_r(31 downto 0);
						
					when x"38" =>										-- SD data
						SD_dataout_r <= datain_r(7 downto 0);
						
					when x"3C" =>										-- SD control
						SD_control_r <= datain_r(3 downto 0);
		
					when x"44" =>										-- SD clock divide
						SD_divide_r <= datain_r(7 downto 0);
	
					when x"4C" =>										-- interlace lines
						interlace_r <= datain_r(3 downto 0);
						
					when x"50" =>										-- charWidth
						charWidth_r <= datain_r(3 downto 0); 
						
					when x"54" =>										-- charHeight
						charHeight_r <= datain_r(3 downto 0); 
						
					when x"58" =>										-- VGArows
						VGArows_r <= datain_r(7 downto 0); 	
						
					when x"5C" =>										-- VGAcols
						VGAcols_r <= datain_r(7 downto 0); 
						
					when others =>
						null;	
				end case;		
			end if;
		
			if en_r = '1' and wrq_r = "1" and addr_r = x"14" then					-- write triggers
				RS232_wr_S0 <= '1';	
			else
				RS232_wr_S0 <= '0';
			end if;

			if en_r = '1' and wrq_r = "1" and addr_r = x"38" then 
				SD_wr <= '1';	
			else
				SD_wr <= '0';
			end if;

	end process;
	
	-- read hardware signals
	
	-- register external signals
	process
	begin
		wait until rising_edge(clk);
		SW_r <= SW;
		SD_status_r <= SD_status;	
	end process;
	
		PS2_data_r <= PS2_data;
		SD_datain_r <= SD_datain;		
	
	-- internal signals unregistered
		RS232_rx_S0_r <= RS232_rx_S0;
		RS232_TBE_S0_r <= RS232_TBE_S0;
		RS232_RDA_S0_r <= RS232_RDA_S0;
		VBLANK_r <= VBLANK;

	
		dataout <= dataout_i;
	
	process			
	begin																
		wait until rising_edge(clk);		-- register all outputs to reduce multiplexer delays
		if en = '1'  then						-- address bus now has a valid address.  Update output register before next CLK rising edge  and clk = '1'
			case addr_i is
				when x"00" =>										-- TEXT_ZERO
					dataout_i <= blank1 & txt_zero_r;
				
				when x"08" =>										-- char/text graphics background colour
					dataout_i <= blank2 & background_r;
					
				when x"0c" =>										-- graphics mode
					dataout_i <= blank3 & "000" & mode_r;
					
				when x"10" =>										-- RS232_S0 data_in
					dataout_i <= blank3 & RS232_rx_S0_r;
					
				when x"1c" =>										-- RS232 port signals
					dataout_i <= blank3 & "000000" & RS232_TBE_S0_r & RS232_RDA_S0_r ;					
					
				when x"20" =>										-- PS2 data_in
					dataout_i <= blank3 & PS2_data_r;
					
				when x"24" =>										-- timer_clk
					dataout_i	<= counter_clk;
					
				when x"28" =>										-- timer_ms byte 3
					dataout_i	<= counter_ms;
					
				when x"2C" =>										-- IRQ_mask
					dataout_i	<=	blank2 & IRQ_mask_r & "1";
					
				when x"34" =>										-- SW
					dataout_i	<= blank2 & SW_r;

				when x"38" =>										-- SD data
					dataout_i	<= blank3 & SD_datain_r;
					
				when x"3C" =>										-- SD control
					dataout_i  <= blank3 & "0000" & SD_control_r;

				when x"40"	=>										-- SD status
					dataout_i	<= blank3 & "0000" & SD_status_r;
					
				when x"48"	=>										-- VGA vertical blank
					dataout_i	<= blank3 & "0000000" & VBLANK_r;
					
				when x"4C" =>										-- interlace lines
					dataout_i	<= blank3 & "0000" &	interlace_r;
						
				when x"50" =>										-- charWidth
					dataout_i	<= blank3 & "0000" &	charWidth_r; 
						
				when x"54" =>										-- charHeight
					dataout_i	<= blank3 & "0000" &	charHeight_r; 
						
				when x"58" =>										-- VGArows
					dataout_i	<= blank3 &	VGArows_r; 	
						
				when x"5C" =>										-- VGAcols
					dataout_i	<= blank3 &	VGAcols_r; 
					
				when x"60" =>										-- MACready
					dataout_i	<= blank3 & "0000000" & MACready;
					
				when x"64" =>										-- MACdata
					dataout_i	<= blank3 & "000000" & MACdata;

				when others =>
					dataout_i <= (others=>'0');
					
			-- read triggers
			
			if en_r = '1' and addr_r = x"64" then 
				MACread_enable <= '1';	
			else
				MACread_enable <= '0';
			end if;

			end case;
		end if;
	end process;	
		
end Behavioral;

