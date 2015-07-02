library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;

entity TEXTbuffer is port
			(
			clk_MEM : IN std_logic;
			clk_VGA : IN std_logic;
			-- VGA				  
			VGAcols : IN STD_LOGIC_VECTOR (7 downto 0);					-- number of complete character columns displayed on the screen
			VBlank : IN std_logic;												-- Vertical Blank indicator
			FetchNextRow : IN std_logic;										-- request that the next row of character data be fetched from memory
			txt_zero : IN std_logic_vector(23 downto 0);					-- base address of the screen buffer in PSDRAM
			ADDR_TEXT : IN std_logic_vector(7 downto 0);					-- column number being read by the VGA controller
			DATA_TEXT : OUT std_logic_vector(15 downto 0);				-- color and character data for the column in question
			-- AXI burst read channel
			t_axi_araddr : OUT  std_logic_vector(31 downto 0);
			t_axi_arlen : OUT  std_logic_vector(7 downto 0);			-- Burst length = value + 1.  Set directly from VGA_columns
			t_axi_arsize : OUT  std_logic_vector(2 downto 0);			-- Size in bytes: "001" = 2 bytes, "010" = 4 bytes
			t_axi_arburst : OUT  std_logic_vector(1 downto 0);			-- Type: "01" = INCR
			t_axi_arvalid : OUT  std_logic;
			t_axi_arready : IN  std_logic;
			t_axi_rdata : IN  std_logic_vector(31 downto 0);
			t_axi_rresp : IN  std_logic_vector(1 downto 0);			
			t_axi_rlast : IN  std_logic;										-- Set high on last data item
			t_axi_rvalid : IN  std_logic;
			t_axi_rready : OUT  std_logic
			);
end TEXTbuffer;

architecture Behavioral of TEXTbuffer is

type state_T is (blank, pause, first_fill, refill, switch_bank);
signal state, next_state : state_T;
signal newline, newline_m : std_logic_vector(2 downto 0);
signal newline_flag : std_logic;
signal active : std_logic_vector(2 downto 0);
signal axi_addr, axi_addr_n : std_logic_vector(23 downto 0);
signal wea : STD_LOGIC_VECTOR(0 DOWNTO 0);
signal buffer_addr, buffer_addr_n : STD_LOGIC_VECTOR(7 DOWNTO 0);
signal dina : STD_LOGIC_VECTOR(15 DOWNTO 0);
signal addra, addrb : STD_LOGIC_VECTOR(8 DOWNTO 0);
signal bank, bank_n : std_logic :='0';
signal line_count, line_count_n : std_logic_vector(2 downto 0);

begin
		t_axi_araddr <= "00000000" & axi_addr;		-- current start of row position in PSDRAM screen buffer 
		t_axi_arlen  <= VGAcols - 1;					-- number of words to read is the number of characters in a row (AXI4 starts counting 0 = one)
		t_axi_arsize <= "001";							-- readsize is word (16 bits)
		t_axi_arburst <= "01";							-- Type: "01" = INCR
		t_axi_rready <= '1';								-- The text buffer is always ready to read data
		
		addra <= (not bank) & buffer_addr;																							-- concatenate the active bank for writing with the current write address
		addrb <= bank & ADDR_TEXT;																										-- concatenate the active bank for reading with the current read address
		dina <= t_axi_rdata(31 downto 16) when buffer_addr(0) = '1' else t_axi_rdata(15 downto 0);				-- select WORD from longword bus		
			
		process				-- cross clock domain signals
		begin
			wait until rising_edge(clk_MEM);
			newline <= newline(1 downto 0) & FetchNextRow;
			newline_m <= newline;
			active <= active(1 downto 0) & NOT VBlank;
		end process;
		
		newline_flag <= '1' when (newline = "111" and newline_m /= "111") else '0';
		
		process				-- state machine and register update
		begin
			wait until rising_edge(clk_MEM);
			state <= next_state;																	
			axi_addr <= axi_addr_n;	
			line_count <= line_count_n;
			bank <= bank_n;
			buffer_addr <= buffer_addr_n;
		end process;
		
		-- state machine next state decode
		process (state, active, line_count, t_axi_rlast, newline_flag )
		begin
			if active = "111" then																									
				case (state) is
					when blank =>														-- waiting for VGA controller to signal the end of VBLANK and indicate that character data will be required
						next_state <= first_fill;										-- now go and read the first row of character date

					when first_fill =>												-- fill the first row of character data from the AXI4 memory bus into the local buffer
						if t_axi_rlast = '1' then
							next_state <= switch_bank;									-- now go and switch the buffer so that the first row of data just read is available to the VGA controller
						else
							next_state <= state;
						end if;							
						
					when refill =>														-- read the next row of character data from the AXI4 memory bus into the local buffer
						if t_axi_rlast = '1' then										-- keep reading until the bus signals that the full row of data has been sent
							next_state <= pause;											-- now go and wait for the VGA controller to use up this data
						else
							next_state <= state;											
						end if;		
						
					when switch_bank =>												-- spend one cycle in this state and use it to "switch over" the two buffer banks
						next_state <= refill;											-- now go and refill the character data on the side of the buffer that has already been used by the VGA controller
		
					when others =>														-- pause: wait for VGA controller to signal it has finished displaying this character row
						if newline_flag = '1' then										-- now go and switch over the two buffer banks
							next_state <= switch_bank;
						else
							next_state <= state;
						end if;
						
				end case;
			else																			-- state machine holds inactive during the VBLANK
				next_state <= blank;
			end if;
		end process;
		
		-- state machine output signal generation
		-- 	organized by signal rather than by state for clarity
									
		with state select
			axi_addr_n <= 	txt_zero when blank,									-- move through memory buffer incrementing by the 2 * number of characters in a column (memory format is word = data+color)
								axi_addr + (VGAcols & "0") when switch_bank,
								axi_addr when others;
								
		with state select
			t_axi_arvalid <=	'1' when refill,									-- AXI4 memory controller signal
									'1' when first_fill,
									'0' when others;
			
		with state select				
			bank_n <= 	not bank when switch_bank,								-- one buffer is being read by the VGA controller whilst the other can be filled via DMA access
							bank when others;
				
			buffer_addr_n <= 	(others=>'0') when (state = blank or state = switch_bank) else
									buffer_addr + 1 when (state = refill and t_axi_rvalid = '1') else  		-- increment the text buffer write address each time after valid data is presented 
									buffer_addr + 1 when (state = first_fill and t_axi_rvalid = '1') else 	-- increment the text buffer write address each time after valid data is presented 
									buffer_addr;
							
		wea <=	"1" when (state = refill and t_axi_rvalid = '1') else "0";
			
	inst_BUFFER_TXT : entity work.BUFFER_TXT
	PORT MAP (
	 clka => CLK_MEM,
	 wea => wea,
	 addra => addra,
	 dina => dina,
	 clkb => CLK_VGA,
	 addrb => addrb,
	 doutb => DATA_TEXT
	);

end Behavioral;

