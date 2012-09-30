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
			  RS232_rx_S1 : in std_logic_vector(7 downto 0);		-- RS232 port 1 (PMOD for SDLogger)
			  RS232_tx_S1 : out std_logic_vector(7 downto 0);
			  RS232_wr_S1 : out std_logic;
			  RS232_TBE_S1 : in std_logic;
			  RS232_RDA_S1 : in std_logic;
			  RS232_UBRR_S1 : out std_logic_vector(15 downto 0);
			  PS2_data : in std_logic_vector(7 downto 0);			-- PS/2 port
			  counter_ms : in std_logic_vector(31 downto 0);		-- 32 bit millisecond timer
			  counter_clk : in std_logic_vector(31 downto 0);		-- 32 bit clock timer
			  ssData	: out std_logic_vector(15 downto 0);			-- data for seven segment display
			  SW	: in std_logic_vector(7 downto 0);					-- switches onboard Nexys2
			  -- CPU system memory channel
			  en : in STD_LOGIC;													-- Enable is set by board level logic depending on higher bit of 
																											-- the address to enable this piece of memory when it is addressed	  
           addr : in  STD_LOGIC_VECTOR (10 downto 0);				-- Low bits only of 32-bit address bus.  Higher bits control "en"
           datain : in  STD_LOGIC_VECTOR (7 downto 0);			
           dataout : out  STD_LOGIC_VECTOR (7 downto 0);
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
	signal RS232_tx_r_S1 : std_logic_vector(7 downto 0);	
	signal UBRR_r_S1	: std_logic_vector(15 downto 0);
	signal background_r : std_logic_vector(7 downto 0);
	signal mode_r : std_logic_vector(4 downto 0);
	signal ssData_r : std_logic_vector(15 downto 0);
	signal addr_i : std_logic_vector(7 downto 0);					-- local address bus
	signal clk_i : std_logic;												-- local slow clock for debounce
	-- pipeline registers for hardware write
	signal en_r : std_logic :='0';
	signal addr_r : std_logic_vector(7 downto 0) :=(others=>'0');
	signal datain_r : std_logic_vector(7 downto 0) :=(others=>'0');
	signal wrq_r : std_logic_vector (0 downto 0) :="0";
	-- pipeline registers for hardware read
	signal RS232_rx_S0_r : std_logic_vector(7 downto 0);
	signal RS232_rx_S1_r : std_logic_vector(7 downto 0);
	signal RS232_TBE_S0_r, RS232_RDA_S0_r, RS232_TBE_S1_r, RS232_RDA_S1_r : std_logic;	
	signal SW_r : std_logic_vector(7 downto 0);
	signal PS2_data_r : std_logic_vector(7 downto 0);
	
begin
	-- connect writable registers to outputs
	SD_dataout <= SD_dataout_r;
	SD_control <= SD_control_r;
	SD_divide <= SD_divide_r;
	gfx_zero <= gfx_zero_r(23 downto 8);
	txt_zero <= txt_zero_r;
	RS232_tx_S0 <= RS232_tx_r_S0;
	RS232_tx_S1 <= RS232_tx_r_S1;
	background <= background_r;
	mode <= mode_r;
	RS232_UBRR_S0 <= UBRR_r_S0;
	RS232_UBRR_S1 <= UBRR_r_S1;
	ssData <= ssData_r;
	addr_i <= addr(7 downto 0);
	clk_i <= counter_clk(13);
	irq_mask <= irq_mask_r;
	
	write_pipeline: process
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
			UBRR_r_S1 <= CONV_STD_LOGIC_VECTOR(54,16);		--  54 = 57600 BAUD at 50 MHz
			irq_mask_r <= "000000000011111";
			txt_zero_r <= X"010700";							
			gfx_zero_r <= X"015200";		
			background_r <= X"00";
			mode_r <= "11011";
			RS232_tx_r_S0 <= (others=>'0');	
			RS232_tx_r_S1 <= (others=>'0');	
			ssData_r <= (others=>'0');
			
		elsif en_r = '1' and wrq_r = "1" then					-- writable registers
				case addr_r is			
					when x"01" =>										-- TEXT_ZERO byte 2
						txt_zero_r(23 downto 16) <= datain_r;
						
					when x"02" =>										-- TEXT_ZERO byte 1
						txt_zero_r(15 downto 8) <= datain_r;
						
					when x"03" =>										-- TEXT_ZERO byte 0
						txt_zero_r(7 downto 0) <= datain_r;
					
					when x"05" =>										-- GFX_ZERO byte 2
						gfx_zero_r(23 downto 16) <= datain_r;
						
					when x"06" =>										-- GFX_ZERO byte 1
						gfx_zero_r(15 downto 8) <= datain_r;

					when x"07" =>										-- GFX_ZERO byte 0
						txt_zero_r(7 downto 0) <= datain_r;				

					when x"08" =>										-- char/text graphics background colour
						background_r(7 downto 0) <= datain_r;		
						
					when x"09" =>										-- graphics mode
						mode_r <= datain_r(4 downto 0);					
						
					when x"0b" =>										-- RS232_S0 data_out
						RS232_tx_r_S0 <= datain_r;	
				
					when x"0c" =>										-- UBRR_S0 byte 1
						UBRR_r_S0(15 downto 8) <= datain_r;
						
					when x"0d" =>										-- UBRR_S0 byte 0
						UBRR_r_S0(7 downto 0) <= datain_r;
						
					when x"0f" =>										-- RS232_S1 data_out
						RS232_tx_r_S1 <= datain_r;
				
					when x"10" =>										-- UBRR_S1 byte 1
						UBRR_r_S1(15 downto 8) <= datain_r;
						
					when x"11" =>										-- UBRR_S1 byte 0
						UBRR_r_S1(7 downto 0) <= datain_r;	
						
					when x"1c" =>										-- IRQ_mask byte 1
						IRQ_mask_r(15 downto 8) <= datain_r;
					
					when x"1d" =>										-- IRQ_mask byte 0
						IRQ_mask_r(7 downto 1) <= datain_r(7 downto 1);
					
					when x"1e" =>										-- SEVENSEG byte 1
						ssData_r(15 downto 8) <= datain_r;
						
					when x"1f" =>										-- SEVENSEG byte 0
						ssData_r(7 downto 0) <= datain_r;
						
					when x"21" =>										-- SD data
						SD_dataout_r <= datain_r;
						
					when x"22" =>										-- SD control
						SD_control_r <= datain_r(3 downto 0);
					
					when x"24" =>										-- SD control
						SD_divide_r <= datain_r;
					
					when others =>
						null;	
				end case;		
			end if;
		
			if en_r = '1' and wrq_r = "1" and addr_r = x"0b" then					-- write triggers
				RS232_wr_S0 <= '1';	
			else
				RS232_wr_S0 <= '0';
			end if;
			
			if en_r = '1' and wrq_r = "1" and addr_r = x"0f" then 
				RS232_wr_S1 <= '1';	
			else
				RS232_wr_S1 <= '0';
			end if;	

			if en_r = '1' and wrq_r = "1" and addr_r = x"21" then 
				SD_wr <= '1';	
			else
				SD_wr <= '0';
			end if;

	end process;
	
--	read_pipeline: process
--	begin
--		wait until rising_edge(clk);
		RS232_rx_S0_r <= RS232_rx_S0;
		RS232_rx_S1_r <= RS232_rx_S1;
		RS232_TBE_S0_r <= RS232_TBE_S0;
		RS232_RDA_S0_r <= RS232_RDA_S0;
		RS232_TBE_S1_r <= RS232_TBE_S1;
		RS232_RDA_S1_r <= RS232_RDA_S1;
		PS2_data_r <= PS2_data;
		SW_r <= SW;
--	end process;
-- pipelining the read process was actually marginally less time efficient and 
--   consumed a few more LUT resources
	
	read_register: process
	begin
		wait until rising_edge(clk);
		if en = '1' and wrq = "0" then						-- readable registers
			case addr_i is
				when x"00" =>										-- TEXT_ZERO byte 3 (this is a null register)
					dataout <= (others=>'0');
					
				when x"01" =>										-- TEXT_ZERO byte 2
					dataout <= txt_zero_r(23 downto 16);
					
				when x"02" =>										-- TEXT_ZERO byte 1
					dataout <= txt_zero_r(15 downto 8);
					
				when x"03" =>										-- TEXT_ZERO byte 0
					dataout <= txt_zero_r(7 downto 0);
				
				when x"04" =>										-- GFX_ZERO byte 3 (this is a null register)
					dataout <= (others=>'0');
				
				when x"05" =>										-- GFX_ZERO byte 2
					dataout <= gfx_zero_r(23 downto 16);
					
				when x"06" =>										-- GFX_ZERO byte 1
					dataout <= gfx_zero_r(15 downto 8);	

				when x"07" =>										-- GFX_ZERO byte 0
					dataout <= txt_zero_r(7 downto 0);					
					
				when x"09" =>										-- graphics mode
					dataout <= "000" & mode_r;
					
				when x"0a" =>										-- RS232_S0 data_in
					dataout <= RS232_rx_S0_r;
					
				when x"0e" =>										-- RS232_S1 data_in
					dataout <= RS232_rx_S1_r;
					
				when x"12" =>										-- RS232 port signals
					dataout <= "0000" & RS232_TBE_S0_r & RS232_TBE_S1_r & RS232_RDA_S0_r & RS232_RDA_S1_r	;					
					
				when x"13" =>										-- PS2 data_in
					dataout <= PS2_data_r;
					
				when x"14"	=>										-- timer_clk byte 3
					dataout	<= counter_clk(31 downto 24);
					
				when x"15"	=>										-- timer_clk byte 2
					dataout	<= counter_clk(23 downto 16);					
					
				when x"16"	=>										-- timer_clk byte 1
					dataout	<= counter_clk(15 downto 8);
					
				when x"17"	=>										-- timer_clk byte 0
					dataout	<= counter_clk(7 downto 0);				

				when x"18"	=>										-- timer_ms byte 3
					dataout	<= counter_ms(31 downto 24);
					
				when x"19"	=>										-- timer_ms byte 2
					dataout	<= counter_ms(23 downto 16);					
					
				when x"1a"	=>										-- timer_ms byte 1
					dataout	<= counter_ms(15 downto 8);
					
				when x"1b"	=>										-- timer_ms byte 0
					dataout	<= counter_ms(7 downto 0);		
					
				when x"1c" =>										-- IRQ_mask byte 1
					dataout	<=	IRQ_mask_r(15 downto 8);
					
				when x"1d" =>										-- IRQ_mask byte 0
					dataout	<=	IRQ_mask_r(7 downto 1) & "0";	
					
				when x"20"	=>										-- SW
					dataout	<= SW_r;

				when x"21"	=>										-- SD data
					dataout	<= SD_datain;
					
				when x"22" =>
					dataout  <= "0000" & SD_control_r;

				when x"23"	=>										-- SD status
					dataout	<= "0000" & SD_status;

				when others =>
					dataout <= (others=>'0');

			end case;
		end if;
	end process;	
		
end Behavioral;

