library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;

entity TEXTbufferDDR is port
			(
			clk_MEM : IN std_logic;
			clk_VGA : IN std_logic;
			reset : in std_logic;
			-- VGA (trans regnum temporis)				  
			VGAcols : IN STD_LOGIC_VECTOR (7 downto 0);					-- number of complete character columns displayed on the screen
			VBlank : IN std_logic;												-- Vertical Blank indicator
			FetchNextRow : IN std_logic;										-- request that the next row of character data be fetched from memory
			-- HWregisters
			txt_zero : IN std_logic_vector(23 downto 0);					-- base address of the screen buffer in PSDRAM
			ADDR_TEXT : IN std_logic_vector(7 downto 0);					-- column number being read by the VGA controller
			DATA_TEXT : OUT std_logic_vector(15 downto 0);				-- color and character data for the column in question
			-- AXI burst read channel
			t_axi_araddr : OUT  std_logic_vector(31 downto 0);
			t_axi_arlen : OUT  std_logic_vector(7 downto 0);			-- Burst length = value + 1.  Set directly from VGA_columns
			t_axi_arvalid : OUT  std_logic;
			t_axi_arready : IN  std_logic;
			t_axi_rdata : IN  std_logic_vector(127 downto 0);		
			t_axi_rlast : IN  std_logic;										-- Set high on last data item
			t_axi_rvalid : IN  std_logic
			);
end TEXTbufferDDR;

architecture Behavioral of TEXTbufferDDR is

type state_T is (blank, pause, first_fill, refill, switch_bank);
type handshake_type is (pending, ready, done);
signal t_axi_ar_state : handshake_type;
signal t_axi_ar_state_n : handshake_type;
signal state, next_state : state_T;
signal newline, newline_m : std_logic_vector(4 downto 0);
signal newline_flag : std_logic;
signal active : std_logic_vector(4 downto 0);
signal axi_addr, axi_addr_n : std_logic_vector(23 downto 0);
signal wea : STD_LOGIC_VECTOR(0 DOWNTO 0);
signal buffer_addr, buffer_addr_n : STD_LOGIC_VECTOR(4 DOWNTO 0);
signal dina : STD_LOGIC_VECTOR(127 DOWNTO 0);
signal addra : STD_LOGIC_VECTOR(5 DOWNTO 0);
signal addrb : STD_LOGIC_VECTOR(8 DOWNTO 0);
signal bank, bank_n : std_logic :='0';
signal line_count, line_count_n : std_logic_vector(2 downto 0);
signal VGAcols_div8 : std_logic_vector(7 downto 0);
signal write_buffer : std_logic_vector(255 downto 0);

COMPONENT BUFFER_TXT_DDR
  PORT (
    clka : IN STD_LOGIC;
    wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    addra : IN STD_LOGIC_VECTOR(5 DOWNTO 0);
    dina : IN STD_LOGIC_VECTOR(127 DOWNTO 0);
    clkb : IN STD_LOGIC;
    addrb : IN STD_LOGIC_VECTOR(8 DOWNTO 0);
    doutb : OUT STD_LOGIC_VECTOR(15 DOWNTO 0)
  );
END COMPONENT;

begin
	t_axi_araddr <= "00000000" & axi_addr;		-- current start of row position in PSDRAM screen buffer 
	VGAcols_div8 <= "000" & VGAcols(7 downto 3);	
	t_axi_arlen  <= VGAcols_div8 - 1;			-- number of 128 bit words to read is the number of columns (each 16 bits of data) divided by 8.  axi_arlen is defined s.t. 0 => one word requested
	
	addra <= (not bank) & buffer_addr;			-- concatenate the active bank for writing with the current write address
	addrb <= bank & ADDR_TEXT;	
	
	with axi_addr(3 downto 1) select			-- re-align incoming 128 bit words according to the requested start address of the memory read																					-- concatenate the active bank for reading with the current read address
	dina <=	write_buffer(127 downto 0) when "000",
			write_buffer(143 downto 16) when "001",
			write_buffer(159 downto 32) when "010",
			write_buffer(175 downto 48) when "011",
			write_buffer(191 downto 64) when "100",
			write_buffer(207 downto 80) when "101",
			write_buffer(223 downto 96) when "110",
			write_buffer(239 downto 112) when others; --"111"
		
	process	-- cross clock domain signals
	begin
		wait until rising_edge(clk_MEM);
		if reset = '1' then
			newline <= (others =>'0');
			newline_m <= (others =>'0');
			active <= (others =>'0');
		else
			newline <= newline(3 downto 0) & FetchNextRow;
			newline_m <= newline;
			active <= active(3 downto 0) & NOT VBlank;
		end if;
	end process;
	
	newline_flag <= '1' when (newline = "11111" and newline_m /= "11111") else '0';
	
	-- state machine and register update
	process
	begin
		wait until rising_edge(clk_MEM);
		if reset = '1' then
			state <= blank;
			t_axi_ar_state <= ready;				
			axi_addr <= (others => '0');
			line_count <= (others =>'0');
			bank <= '0';
			buffer_addr <= (others => '0');
			write_buffer <= (others => '0');
		else
			state <= next_state;		
			t_axi_ar_state <= t_axi_ar_state_n;															
			axi_addr <= axi_addr_n;	
			line_count <= line_count_n;
			bank <= bank_n;
			buffer_addr <= buffer_addr_n;
			write_buffer <= t_axi_rdata & write_buffer(255 downto 128);
			if ((state = refill) or (state = first_fill)) and t_axi_rvalid = '1' then 
				wea <=	"1";
			else 
				wea <= "0";
			end if;
		end if;
	end process;
	
	-- state machine next state decode
	process (state, active, line_count, t_axi_rlast, newline_flag )
	begin
		if active = "11111" then																									
			case (state) is
				when blank =>						-- waiting for VGA controller to signal the end of VBLANK and indicate that character data will be required
					next_state <= first_fill;				-- now go and read the first row of character date

				when first_fill =>					-- fill the first row of character data from the AXI4 memory bus into the local buffer
					if t_axi_rlast = '1' then
						next_state <= switch_bank;			-- now go and switch the buffer so that the first row of data just read is available to the VGA controller
					else
						next_state <= state;
					end if;							
					
				when refill =>					-- read the next row of character data from the AXI4 memory bus into the local buffer
					if t_axi_rlast = '1' then				-- keep reading until the bus signals that the full row of data has been sent
						next_state <= pause;				-- now go and wait for the VGA controller to use up this data
					else
						next_state <= state;											
					end if;		
					
				when switch_bank =>					-- spend one cycle in this state and use it to "switch over" the two buffer banks
					next_state <= refill;				-- now go and refill the character data on the side of the buffer that has already been used by the VGA controller
	
				when others =>					-- pause: wait for VGA controller to signal it has finished displaying this character row
					if newline_flag = '1' then				-- now go and switch over the two buffer banks
						next_state <= switch_bank;
					else
						next_state <= state;
					end if;
					
			end case;
		else									-- state machine holds inactive during the VBLANK
			next_state <= blank;
		end if;
	end process;
	
	-- state machine output signal generation
	-- 	organized by signal rather than by state for clarity					
	with state select
		axi_addr_n <= txt_zero when blank,	
				axi_addr + (VGAcols & "0") when switch_bank,	-- move through memory buffer incrementing by the 2 * number of characters in a column (memory format is word = data+color)words
				axi_addr when others;
							
--	with state select
--		t_axi_arvalid <=	'1' when refill,					-- AXI4 memory controller signal
--					'1' when first_fill,
--					'0' when others;
		
	with state select				
		bank_n <= not bank when switch_bank,					-- one buffer is being read by the VGA controller whilst the other can be filled via DMA access
				bank when others;
		
	buffer_addr_n <= (others=>'0') when (state = blank or state = switch_bank) else
			buffer_addr + 1 when (state = refill and t_axi_rvalid = '1') else  		-- increment the text buffer write address each time after valid data is presented 
			buffer_addr + 1 when (state = first_fill and t_axi_rvalid = '1') else 	-- increment the text buffer write address each time after valid data is presented 
			buffer_addr;
							
			
	inst_BUFFER_TXT_DDR : BUFFER_TXT_DDR
	PORT MAP (
	 clka => CLK_MEM,
	 wea => wea,
	 addra => addra,
	 dina => dina,
	 clkb => CLK_VGA,
	 addrb => addrb,
	 doutb => DATA_TEXT
	);
	
	-- AXI4 channel handshake control
	process (next_state, t_axi_ar_state, t_axi_arready)
	begin
		-- s_axi_ar
		case t_axi_ar_state is
		when ready =>
			if next_state = refill or next_state = first_fill then
				t_axi_ar_state_n <= pending;
			else 
				t_axi_ar_state_n <= ready;
			end if;
		when pending =>
			if t_axi_arready = '1' then
				t_axi_ar_state_n <= done;
			else
				t_axi_ar_state_n <= pending;
			end if;		
		when done =>
			if next_state = pause then
				t_axi_ar_state_n <= ready;
			else 
				t_axi_ar_state_n <= done;
			end if;		
		end case;

	end process;

	with t_axi_ar_state select t_axi_arvalid <= '1' when pending, '0' when others;

end Behavioral;

