-- DMA Controller
-- Andrew Read
-- Created March 2011

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity DMAController is
    Port ( RESET : in STD_LOGIC;												-- Active low.  Hold until memory warm up is complete
			  CLK_MEM: in  STD_LOGIC;	 										-- System and memory clock, 50-80MHz
			  CLK_VGA : in STD_LOGIC;											-- VGA clock (used to clock VGA buffer memory)
			  ADDR_SDRAM : out  STD_LOGIC_VECTOR (23 downto 1);		-- SDRAM connections to CellularRAM
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
           DATA_OUT_VGA : out  STD_LOGIC_VECTOR (7 downto 0);		-- Frame buffer connection to Gfx block of VGA signal generator
           ADDR_VGA : in  STD_LOGIC_VECTOR (8 downto 0);				-- Frame buffer reads from a 512 byte BLOCK RAM buffer
			  DATA_OUT_TXT : out  STD_LOGIC_VECTOR (15 downto 0);		-- Frame buffer connection to Text block of VGA signal generator
           ADDR_TXT : in  STD_LOGIC_VECTOR (6 downto 0);				-- Frame buffer reads from a 256 word BLOCK RAM buffer
           start_VGA : in  STD_LOGIC;				  						-- Vertical blank minus 1 line triggers DMA access for fram buffer
           start_TXT : in  STD_LOGIC;				  						-- Vertical blank minus 1 line triggers DMA access for fram buffer	
			  mode	: in STD_LOGIC_VECTOR (4 downto 0);					-- Graphics mode
           ADDR_ZERO : in  STD_LOGIC_VECTOR (23 downto 8);			-- Location in memory of the gfx frame buffer is ADDR_ZERO & "00000000"  
			  ADDR_ZERO_TXT :  in  STD_LOGIC_VECTOR (23 downto 0);	-- Location in memory of the text frame buffer   
			  DATA_IN_A : in STD_LOGIC_VECTOR (15 downto 0);			-- Port A is a 16bit read/write port 
			  DATA_OUT_A : out STD_LOGIC_VECTOR (15 downto 0);
			  ADDR_A : in STD_LOGIC_VECTOR (23 downto 1);
			  REQ_A : in STD_LOGIC;												-- Hold and keep high to request a read
			  WRQ_A : in STD_LOGIC;												-- Hold and keep high to request a write
			  RDY_A : out STD_LOGIC;											-- Goes high for one cycle to signal read or write completion
			  DATA_IN_B : in STD_LOGIC_VECTOR (7 downto 0);				-- Port B is an 8bit write only port 
			  ADDR_B : in STD_LOGIC_VECTOR (23 downto 0);
			  WRQ_B : in STD_LOGIC;
			  RDY_B : out STD_LOGIC;
			  DATA_IN_C: in STD_LOGIC_VECTOR (7 downto 0);				-- Port C is an 8bit read/write port
			  DATA_OUT_C: out STD_LOGIC_VECTOR (7 downto 0);
			  ADDR_C : in STD_LOGIC_VECTOR (23 downto 0);
			  REQ_C : in STD_LOGIC;
			  WRQ_C : in STD_LOGIC;
			  RDY_C : out STD_LOGIC
			  );						
end DMAController;

architecture Behavioral of DMAController is

type state_T is (setup, idle, pre_idle, read_A, write_A, finish_A, write_B, finish_B, read_C, write_C, finish_C, 
						read_burst_0, read_burst_1, read_burst_2, read_burst_3,
						read_burst_txt_0, read_burst_txt_1, read_burst_txt_2, read_burst_txt_3, read_burst_txt_4);
type state1_T is (idle, first_buffer, next_buffer);
signal state : state_T := setup;
signal state1, state2 : state1_T := idle;	
signal select_clk : std_logic;													-- Determines clocking of SDRAM (held low or clocked with SYSTEM_CLOCK)
signal counter : std_logic_vector (6 downto 0) := (others=>'0');		-- Local counter for state machine use
signal rowCounter : std_logic_vector(2 downto 0) := "000";
signal wea, wea_txt : std_logic_vector (0 downto 0) := "0";				-- Write enable the BLOCK RAM buffer
signal ADDR_BUF, ADDR_BUF_TXT : std_logic_vector (7 downto 0);			-- Write address of the BLOCK RAM buffer
signal ADDR_LINE : std_logic_vector (23 downto 8) := (OTHERS=>'0');	-- Main memory location to read the next 128 words to the buffer
signal ADDR_LINE_TXT : std_logic_vector (23 downto 1) := (OTHERS=>'0');	-- Main memory location to read the next 128 words to the buffer
signal bank, bank_txt_w, bank_txt_r : std_logic := '0';					-- Alternates two 128 word banks of the BLOCK RAM buffer
signal start_VGA_m1 : std_logic;													-- Vertical blank one cycle before (to detect rising edge)
signal start_TXT_m1 : std_logic;													-- Vertical blank one cycle before (to detect rising edge)
signal DATA_VGA_i, DATA_TXT_i : std_logic_vector (15 downto 0);		-- Read data from BLOCK RAM for multiplexing to VGA generator
signal VGA_buf_done, TXT_buf_done : std_logic := '0';
signal ADDR_TXT_i : std_logic_vector(7 downto 0);
signal ADDR_TXT_m1 :  std_logic_vector (6 downto 0);
signal COLUMNS : std_logic_vector (6 downto 0);
signal COLUMNS1 : std_logic_vector (6 downto 0);
signal ADDR_SDRAM_i : STD_LOGIC_VECTOR (23 downto 1);

component BUFFER_VGA																	-- 256 word (512 byte) buffer (i.e. 2* 128 word burst reads)
	port (
	clka: IN std_logic;
	wea: IN std_logic_VECTOR(0 downto 0);
	addra: IN std_logic_VECTOR(7 downto 0);
	dina: IN std_logic_VECTOR(15 downto 0);
	clkb: IN std_logic;
	addrb: IN std_logic_VECTOR(7 downto 0);
	doutb: OUT std_logic_VECTOR(15 downto 0));
end component;

component BUFFER_TXT
	port (
	clka: IN std_logic;
	wea: IN std_logic_VECTOR(0 downto 0);
	addra: IN std_logic_VECTOR(7 downto 0);
	dina: IN std_logic_VECTOR(15 downto 0);
	clkb: IN std_logic;
	addrb: IN std_logic_VECTOR(7 downto 0);
	doutb: OUT std_logic_VECTOR(15 downto 0));
end component;

begin
		-- internal VGA buffer RAM
		inst_BUFFER_VGA_GRAPHICS : BUFFER_VGA
		port map (
			clka => CLK_MEM,
			wea => wea,
			addra => ADDR_BUF,
			dina => DATA_SDRAM,
			clkb => CLK_VGA,
			addrb => ADDR_VGA(8 downto 1),
			doutb => DATA_VGA_i);
			
		inst_BUFFER_TXT_GRAPHICS : BUFFER_TXT
		port map (
			clka => CLK_MEM,
			wea => wea_txt, 			-- "0", wea_txt
			addra => ADDR_BUF_TXT,
			dina => DATA_SDRAM,
			clkb => CLK_VGA,
			addrb => ADDR_TXT_i,
			doutb => DATA_TXT_i);
			
		--- combinational logic
		ADDR_SDRAM <= ADDR_SDRAM_i;
		
		with ADDR_VGA(0) select												-- Multiplex WORD data of the buffer to BYTE requirment of VGA unit
			DATA_OUT_VGA <= DATA_VGA_i(7 downto 0) when '1',		-- ADDR_VGA is one cycle ahead of DATA_VGA_i (looks 'wrong way around' but correct)
							    DATA_VGA_i(15 downto 8) when others;
		
		DATA_OUT_TXT <= DATA_TXT_i;
		ADDR_TXT_i <= bank_txt_r & ADDR_TXT;
									
		ADDR_BUF(7) <= bank;													-- Feed alternate 128 word 'banks' in the BUFFER RAM
		ADDR_BUF(6 downto 0) <= counter;									-- Incremented with burst read									
	
		ADDR_BUF_TXT(7) <= bank_txt_w;									-- Feed alternate 128 word 'banks' in the BUFFER RAM
		ADDR_BUF_TXT(6 downto 0) <= counter;							-- Incremented with burst read	
	
		with select_clk select												-- Clocking of SDRAM
			CLK_SDRAM <= '0' when '0',
							 CLK_MEM when others;

		DATA_OUT_A <= DATA_SDRAM;											-- Port A output
		
		with ADDR_C(0) select												-- Port C output
			DATA_OUT_C <= DATA_SDRAM(7 downto 0) when '1',
							  DATA_SDRAM(15 downto 8) when others;
			
		with mode(4) select
			COLUMNS <= "1100011" when '1',								-- SVGA 99
						  "1001111" when others;							-- VGA 79
						  
		with mode(4) select
			COLUMNS1 <= "1100100" when '1',								-- SVGA 100
						   "1010000" when others;							-- VGA 80	
		-- VGA triggers
		process
			begin
				wait until rising_edge(CLK_MEM);
				start_VGA_m1 <= start_VGA;											-- Support rising edge detection		
				case state1 is
					when idle =>
						if START_VGA_m1 = '1' and START_VGA = '0' then		-- First buffer read of this screen
							state1 <= first_buffer;
						elsif ADDR_VGA (7 downto 0) = 1 then
							state1 <= next_buffer;
						end if;
					when first_buffer =>
						if VGA_buf_done = '1' then
							state1 <= idle;
						end if;
					when next_buffer =>
						if VGA_buf_done = '1' then
							state1 <= idle;
						end if;				
				end case;
			end process;
	
		-- TEXT triggers
		process
			begin
				wait until rising_edge(CLK_MEM);	
				start_TXT_m1 <= start_TXT;	
				ADDR_TXT_m1 <= ADDR_TXT;
				
				if START_TXT_m1 = '1' and START_TXT = '0' then
					rowCounter <= "000";
					bank_txt_r <= '0';
				end if;
				
				if ADDR_TXT = 0 and ADDR_TXT_m1 = COLUMNS then   -- VGA 79, SVGA 99
					if rowCounter = 7 then 
						bank_txt_r <= not bank_txt_r;
					end if;
					rowCounter <= rowCounter + 1;	
				end if;
				
				case state2 is
					when idle =>
						if START_TXT_m1 = '1' and START_TXT = '0' then
								state2 <= first_buffer;
							
						elsif ADDR_TXT = 0 and ADDR_TXT_m1 = COLUMNS and rowCounter = 1 then  -- VGA 79, SVGA 99
								state2 <= next_buffer;  	
						end if;	
						
					when first_buffer =>
						if TXT_buf_done = '1' then
							state2 <= idle;						
						end if;
						
					when next_buffer =>
						if TXT_buf_done = '1' then
							state2 <= idle;
						end if;				
				end case;
			end process;	
			
		-- state machine	
		process 
			begin
			wait until rising_edge(CLK_MEM);

			if RESET = '0' then												-- Reset is active hi				
				case state is
					when setup =>												-- Configure SDRAM for Synchronous output
						select_clk <= '0';
						ADV_SDRAM <= '0';		
						CE_SDRAM <= '0';	
						OE_SDRAM <= '1';		
						WE_SDRAM <= '0';						
						CRE_SDRAM <= '1';
						LB_SDRAM <= '0';
						UB_SDRAM <= '0';
						ADDR_SDRAM_i <= "00010000001110100011111";						
						counter <= counter + 1;
						if counter = 5 then 									-- 50MHz =3, 80MHz =5
							state <= pre_idle;
						end if;
						
					when pre_idle =>											-- Resets SDRAM control lines
						select_clk <= '0';
						ADV_SDRAM <= '1';							
						CE_SDRAM <= '1';							
						OE_SDRAM <= '0';		
						WE_SDRAM <= '1';	
						CRE_SDRAM <= '0';		
						LB_SDRAM <= '0';
						UB_SDRAM <= '0';	
						counter <= (others=>'0');		
						VGA_buf_done <= '0';	
						TXT_buf_done <= '0';
						RDY_A <= '0';
						RDY_B <= '0';
						RDY_C <= '0';
						state <= idle;

					when idle =>
					
						if state2 = first_buffer then					-- First buffer read of this screen
							ADDR_LINE_TXT <= ADDR_ZERO_TXT(23 downto 1);
							bank_txt_w <= '0';
							state <= read_burst_txt_0;
						
						elsif state2 = next_buffer then						-- Subsequent buffer reads
							ADDR_LINE_TXT <= ADDR_LINE_TXT + COLUMNS1;	-- triggered each 80/100 word reads    -- VGA 80, SVGA 100
							bank_txt_w <= not bank_txt_w;						-- trigger on 1 not 0 to avoid false starts when idle
							state <= read_burst_txt_0;							-- flip bank each time			
							
						elsif state1 = first_buffer then						-- First buffer read of this screen
							ADDR_LINE <= ADDR_ZERO;
							bank <= '0';
							state <= read_burst_0;
						
						elsif state1 = next_buffer then					-- Subsequent buffer reads
							ADDR_LINE <= ADDR_LINE + 1;					-- triggered each 128 word (256 byte) reads  
							bank <= not bank;									-- trigger on 1 not 0 to avoid false starts when idle
							state <= read_burst_0;							-- flip bank each time	
												
						elsif REQ_A = '1' then 								-- Read request on port A
							DATA_SDRAM <= (others => 'Z');				-- important to allow DATA_IN from SDRAM
							ADDR_SDRAM_i <= ADDR_A;
							ADV_SDRAM <= '0';
							CE_SDRAM <= '0';
							state <= read_A;	
							
						elsif WRQ_A = '1' then								-- Write request on port A
							DATA_SDRAM <= DATA_IN_A;						
							ADDR_SDRAM_i <= ADDR_A;	
							ADV_SDRAM <= '0';	
							CE_SDRAM <= '0';						
							WE_SDRAM <= '0';
							state <= write_A;
							
						elsif WRQ_B = '1' then								-- Write request on port B
							LB_SDRAM <= ADDR_B(0);							-- byte write only in word addressable memory
							UB_SDRAM <= not ADDR_B(0);						-- need to configure which byte to write
							DATA_SDRAM <= DATA_IN_B & DATA_IN_B;
							ADDR_SDRAM_i <= ADDR_B(23 downto 1);			-- change byte address space to word address space
							ADV_SDRAM <= '0';
							CE_SDRAM <= '0';
							WE_SDRAM <= '0';
							state <= write_B;
							
						elsif REQ_C = '1' then
							DATA_SDRAM <= (others => 'Z');				
							ADDR_SDRAM_i <= ADDR_A;
							ADV_SDRAM <= '0';
							CE_SDRAM <= '0';
							state <= read_C;
							
						elsif WRQ_C = '1' then
							LB_SDRAM <= not ADDR_C(0);							
							UB_SDRAM <= ADDR_C(0);						
							DATA_SDRAM <= DATA_IN_C & DATA_IN_C;
							ADDR_SDRAM_i <= ADDR_C(23 downto 1);			
							ADV_SDRAM <= '0';
							CE_SDRAM <= '0';
							WE_SDRAM <= '0';
							state <= write_C;						

						end if;
						
					when read_A =>												-- Timing requirement cycles
						counter <= counter + 1;
						if counter = 5 then			-- 50MHz =3, 80MHz =5 
							state <= finish_A;
						end if;

					when write_A =>											-- Timing requirement cycles
						counter <= counter + 1;
						if counter = 3 then 			-- 50MHz =1, 80MHz =3
							state <= finish_A;
						end if;
						
					when finish_A =>											-- Raise ready flag
						RDY_A <= '1';	
						state <= pre_idle;									-- Revert to 'idle' via 'pre_idle' so calling unit can clear original request first
									
					when write_B =>											-- Timing requirement cycles
						counter <= counter + 1;
						if counter = 3 then 			-- 50MHz =1, 80MHz =3
							state <= finish_B;
						end if;
					
					when finish_B =>											-- Raise ready flag
						RDY_B <= '1';
						state <= pre_idle;
					
					when read_C =>												-- Timing requirement cycles
						counter <= counter + 1;
						if counter = 5 then			-- 50MHz =3, 80MHz =5 
							state <= finish_C;
						end if;	
						
					when write_C =>											-- Timing requirement cycles
						counter <= counter + 1;
						if counter = 3 then 			-- 50MHz =1, 80MHz =3
							state <= finish_C;
						end if;
						
					when finish_C =>
						RDY_C <= '1';
						state <= pre_idle;					
						
					when read_burst_0 =>										-- Initialize burst read
						ADDR_SDRAM_i <= ADDR_LINE & "0000000";
						DATA_SDRAM <= (others => 'Z');							
						ADV_SDRAM <= '0';
						CE_SDRAM <= '0';
						VGA_buf_done <= '1';
						state <= read_burst_1;
						
					when read_burst_1 =>										-- Need to enable CE and ADV befor clocking		
						select_clk <= '1';
						state <= read_burst_2;
						
					when read_burst_2 =>										-- Wait for SDRAM to signal ready
						ADV_SDRAM <= '1';
						if WAIT_SDRAM = '0' then							-- Wait is deasserted one cycle before data arrives
							state <= read_burst_3;
							wea <= "1";
						end if;
					
					when read_burst_3 =>										-- Synchronous data reading of 128 words
						counter <= counter + 1;
						if counter = 127 then
							wea <= "0";											-- stop BLOCK RAM writes now to avoid unwanted rollover to address 0.
							counter <= (others=>'0');
							state <= pre_idle;
						end if;

					when read_burst_txt_0 =>								-- Initialize burst read for text data
						ADDR_SDRAM_i <= ADDR_LINE_TXT;
						DATA_SDRAM <= (others => 'Z');							
						ADV_SDRAM <= '0';
						CE_SDRAM <= '0';
						TXT_buf_done <= '1';
						state <= read_burst_txt_1;
						
					when read_burst_txt_1 =>								-- Need to enable CE and ADV befor clocking	
						select_clk <= '1';
						state <= read_burst_txt_2;
						
					when read_burst_txt_2 =>								-- Wait for SDRAM to signal ready
						ADV_SDRAM <= '1';
						if WAIT_SDRAM = '0' then							-- Wait is deasserted one cycle before data arrives
							state <= read_burst_txt_3;
							wea_txt <= "1";
						end if;
					
					when read_burst_txt_3 =>								-- Synchronous data reading of 128 words
						counter <= counter + 1;
						if counter = COLUMNS then							-- VGA 79, SVGA 99
							wea_txt <= "0";									-- stop BLOCK RAM writes now to avoid unwanted rollover to address 0.
							state <= pre_idle;
						elsif WAIT_SDRAM = '1' then						-- row boundary has been reached!
							wea_txt <= "0";
							state <= read_burst_txt_4;				
						end if;
						
					when read_burst_txt_4 =>										-- restart read on next row
						ADDR_SDRAM_i <= ADDR_LINE_TXT + counter;
						DATA_SDRAM <= (others => 'Z');							
						ADV_SDRAM <= '0';
						CE_SDRAM <= '0';
						state <= read_burst_txt_1;	
						
				end case;
			end if;
		end process;

end Behavioral;

