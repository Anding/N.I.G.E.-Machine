library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;

entity TEXTbuffer is port
			(
			clk_MEM : IN std_logic;
			clk_VGA : IN std_logic;
			-- VGA
			VGA_columns : IN std_logic_vector(6 downto 0);
			VGA_active : IN std_logic;
			VGA_newline : IN std_logic;
			txt_zero : IN std_logic_vector(23 downto 0);
			ADDR_TEXT : IN std_logic_vector(6 downto 0);
			DATA_TEXT : OUT std_logic_vector(15 downto 0);
			-- AXI burst read channel
			t_axi_araddr : OUT  std_logic_vector(31 downto 0);
			t_axi_arlen : OUT  std_logic_vector(7 downto 0);				-- Burst length = value + 1
			t_axi_arsize : OUT  std_logic_vector(2 downto 0);			-- Size in bytes: "01" = 2 bytes, "10" = 4 bytes
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

type state_T is (vblank, idle, new_line, new_row, run, post_run);
signal state, next_state : state_T;
signal newline, newline_m : std_logic_vector(2 downto 0);
signal newline_flag : std_logic;
signal active : std_logic_vector(2 downto 0);
signal axi_addr, axi_addr_n : std_logic_vector(23 downto 0);
signal wea : STD_LOGIC_VECTOR(0 DOWNTO 0);
signal buffer_addr, buffer_addr_n : STD_LOGIC_VECTOR(6 DOWNTO 0);
signal dina : STD_LOGIC_VECTOR(15 DOWNTO 0);
signal addra, addrb : STD_LOGIC_VECTOR(7 DOWNTO 0);
signal bank, bank_n : std_logic :='0';
signal line_count, line_count_n : std_logic_vector(2 downto 0);

begin
		t_axi_araddr <= "00000000" & axi_addr;
		t_axi_arlen  <= "0" & VGA_columns;				-- actually one packet too long, but no harm done
		t_axi_arsize <= "001";
		t_axi_arburst <= "01";
		t_axi_rready <= '1';
		
		addra <= (not bank) & buffer_addr;
		addrb <= bank & ADDR_TEXT;		
		dina <= t_axi_rdata(31 downto 16) when buffer_addr(0) = '1' else t_axi_rdata(15 downto 0);		
			
		process				-- cross clock domain signals
		begin
			wait until rising_edge(clk_MEM);
			newline <= newline(1 downto 0) & VGA_newline;
			newline_m <= newline;
			active <= active(1 downto 0) & VGA_active;
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
			
		process (state, active, line_count, t_axi_rlast, newline_flag )	-- next state decode
		begin
			if active = "111" then
				case (state) is
					when vblank =>
						next_state <= run;
						
					when new_line =>
						if line_count = "111" then
							next_state <= new_row;
						else
							next_state <= idle;
						end if;
						
					when new_row =>
						next_state <= run;
						
					when run =>
						if t_axi_rlast = '1' then
							next_state <= post_run;
						else
							next_state <= state;
						end if;		
						
					when post_run =>
						next_state <= idle;
		
					when others =>												-- idle
						if newline_flag = '1' then
							next_state <= new_line;
						else
							next_state <= state;
						end if;
						
				end case;
			else
				next_state <= vblank;
			end if;
		end process;
		
		with state select
			line_count_n <= 	(others=>'0') when vblank,
									line_count + 1 when new_line,
									line_count when others;	
									
		with state select
			axi_addr_n <= 	txt_zero when vblank,
								axi_addr + (VGA_columns & "0") when post_run,
								axi_addr when others;
								
		with state select
			t_axi_arvalid <=	'1' when run,
									'0' when others;
			
		with state select
			bank_n <= 	not bank when post_run,
							bank when others;
				
			buffer_addr_n <= 	(others=>'0') when (state = new_line) else
									(others=>'0') when (state = vblank) else
									buffer_addr + 1 when (state = run and t_axi_rvalid = '1') else
									buffer_addr;
							
			wea <=	"1" when (state = run and t_axi_rvalid = '1') else "0";
			
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

