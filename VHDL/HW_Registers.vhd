-- Hardware registers
-- Andrew Read
-- Created 20 August 2011

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity HW_Registers is
    Port ( clk : in  STD_LOGIC;
			  --clk2x : in STD_LOGIC;
           rst : in  STD_LOGIC;
			  -- connections to other hardware components
			  SD_datain : in std_logic_vector(7 downto 0);			-- SD card interface
			  SD_dataout : out std_logic_vector(7 downto 0);
			  SD_control : out std_logic_vector(3 downto 0);
			  SD_status : in std_logic_vector(3 downto 0);
			  SD_wr : out std_logic;
			  SD_divide : out std_logic_vector(7 downto 0);
			  --
			  gfx_zero : out std_logic_vector(23 downto 8);			-- base address of bitmapped graphics memory
			  txt_zero : out std_logic_vector(23 downto 0);			-- base address of character graphics memory
			  mode	 	: out STD_LOGIC_VECTOR (4 downto 0);			-- graphics adapter mode
			  background : out STD_LOGIC_VECTOR (7 downto 0);		-- background color for graphics adapter text mode
			  irq_mask : out STD_LOGIC_VECTOR(15 downto 1);			-- mask for interrupt controller
			  RS232_rx_S0 : in std_logic_vector(7 downto 0);		-- RS232 port 0 datain
			  RS232_tx_S0 : out std_logic_vector(7 downto 0);		-- RS232 port 0 dataout
			  RS232_wr_S0 : out std_logic;								-- RS232 port 0 write request
																					-- RS232_wr_S0 needs to get triggered for one cycle only when a 
																					--   data byte is places in the register RS232_tx_S0
			  RS232_TBE_S0 : in std_logic;								-- RS232 port 0 Transfer Bus Enable (TBE) signal
			  RS232_RDA_S0 : in std_logic;								-- RS232 port 0 Read Data Available (RDA) signal
			  RS232_UBRR_S0 : out std_logic_vector(15 downto 0);	-- RS232 port 0 baud rate setting (UBRR = Clock / (Baud+1) / 16)
			  PS2_data : in std_logic_vector(7 downto 0);			-- PS/2 port
			  counter_ms : in std_logic_vector(31 downto 0);		-- 32 bit millisecond timer
			  counter_clk : in std_logic_vector(31 downto 0);		-- 32 bit clock timer
			  ssData	: out std_logic_vector(15 downto 0);			-- data for seven segment display
			  SW	: in std_logic_vector(7 downto 0);					-- switches onboard Nexys2
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
	signal gfx_zero_r : std_logic_vector(23 downto 0);				
	signal txt_zero_r : std_logic_vector(23 downto 0);		
	signal irq_mask_r : std_logic_vector(15 downto 1);
	signal RS232_tx_r_S0 : std_logic_vector(7 downto 0);	
	signal UBRR_r_S0	: std_logic_vector(15 downto 0);
	signal background_r : std_logic_vector(7 downto 0);
	signal mode_r : std_logic_vector(4 downto 0);
	signal ssData_r : std_logic_vector(15 downto 0);
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
	signal SW_r : std_logic_vector(7 downto 0);
	signal PS2_data_r : std_logic_vector(7 downto 0);
	signal SD_status_r : std_logic_vector(3 downto 0);
	signal SD_datain_r : std_logic_vector(7 downto 0);
	
	signal dataout_i : STD_LOGIC_VECTOR (31 downto 0);
	
	constant blank3 : std_logic_vector(23 downto 0) := (others=>'0');
	constant blank2 : std_logic_vector(15 downto 0) := (others=>'0');
	constant blank1 : std_logic_vector(7 downto 0) := (others=>'0');
	
begin
	-- connect writable registers to outputs
	SD_dataout <= SD_dataout_r;
	SD_control <= SD_control_r;
	SD_divide <= SD_divide_r;
	gfx_zero <= gfx_zero_r(23 downto 8);
	txt_zero <= txt_zero_r;
	RS232_tx_S0 <= RS232_tx_r_S0;
	background <= background_r;
	mode <= mode_r;
	RS232_UBRR_S0 <= UBRR_r_S0;
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
			UBRR_r_S0 <= CONV_STD_LOGIC_VECTOR(325,16);		-- 325 = 9600 BAUD at 50 MHz		
			irq_mask_r <= "000000000000111";						-- "000000000000111";
			txt_zero_r <= X"010600";							
			gfx_zero_r <= X"000000";		
			background_r <= X"00";
			mode_r <= "11011";
			RS232_tx_r_S0 <= (others=>'0');	
			ssData_r <= (others=>'0');
			
		elsif en_r = '1' and wrq_r = "1" then					-- writable registers
				case addr_r is			
					when x"00" =>										-- TEXT_ZERO 
						txt_zero_r <= datain_r(23 downto 0);
					
					when x"04" =>										-- GFX_ZERO
						gfx_zero_r <= datain_r(23 downto 0);	

					when x"08" =>										-- char/text graphics background colour
						background_r <= datain_r(7 downto 0);		
						
					when x"0C" =>										-- graphics mode
						mode_r <= datain_r(4 downto 0);					
						
					when x"14" =>										-- RS232_S0 data_out
						RS232_tx_r_S0 <= datain_r(7 downto 0);	
				
					when x"18" =>										-- UBRR_S0
						UBRR_r_S0 <= datain_r(15 downto 0);
						
					when x"2C" =>										-- IRQ_mask
						IRQ_mask_r <= datain_r(15 downto 1);
					
					when x"30" =>										-- SEVENSEG
						ssData_r <= datain_r(15 downto 0);
						
					when x"38" =>										-- SD data
						SD_dataout_r <= datain_r(7 downto 0);
						
					when x"3C" =>										-- SD control
						SD_control_r <= datain_r(3 downto 0);
		
					when x"44" =>										-- SD clock divide
						SD_divide_r <= datain_r(7 downto 0);
					
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
	
	process															-- register all inputs (except system clocks) to reduce routing delays
	begin
		wait until rising_edge(clk);
		RS232_rx_S0_r <= RS232_rx_S0;
		RS232_TBE_S0_r <= RS232_TBE_S0;
		RS232_RDA_S0_r <= RS232_RDA_S0;
		SW_r <= SW;
		PS2_data_r <= PS2_data;
		SD_status_r <= SD_status;
		SD_datain_r <= SD_datain;
	end process;
	
		dataout <= dataout_i;
	
	process (en, addr, txt_zero_r, gfx_zero_r, background_r, mode_r, RS232_rx_S0_r, RS232_TBE_S0_r, RS232_RDA_S0_r, PS2_data_r, counter_clk,
				counter_ms, IRQ_mask_r, SW_r, SD_datain_r, SD_control_r, SD_status_r)
				
	begin																
		--wait until rising_edge(clk2x);						-- register all outputs to reduce multiplexer delays
		if en = '1'  then						-- address bus now has a valid address.  Update output register before next CLK rising edge  and clk = '1'
			case addr_i is
				when x"00" =>										-- TEXT_ZERO
					dataout_i <= blank1 & txt_zero_r;
				
				when x"04" =>										-- GFX_ZERO
					dataout_i <= blank1 & gfx_zero_r;			
					
				when x"08" =>										-- char/text graphics background colour
					dataout_i <= blank3 & background_r;
					
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
					dataout_i	<= blank3 & SW_r;

				when x"38" =>										-- SD data
					dataout_i	<= blank3 & SD_datain_r;
					
				when x"3C" =>										-- SD control
					dataout_i  <= blank3 & "0000" & SD_control_r;

				when x"40"	=>										-- SD status
					dataout_i	<= blank3 & "0000" & SD_status_r;

				when others =>
					dataout_i <= (others=>'0');

			end case;
		end if;
	end process;	
		
end Behavioral;

