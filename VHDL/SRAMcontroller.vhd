library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity SRAM_controller is
    Port ( 	RST : in STD_LOGIC;
				CLK : in STD_LOGIC;
				en : in STD_LOGIC;
				ADDR : in  STD_LOGIC_VECTOR (31 downto 0);					-- byte address from CPU
				size : in  STD_LOGIC_VECTOR (1 downto 0);						-- length of read or write 01 = byte, 02 = word, 03 = longword
				WE : in  STD_LOGIC_VECTOR (0 downto 0);						-- write enable from CPU
				DATA_in : in  STD_LOGIC_VECTOR (31 downto 0);				-- data from CPU cell to write to memory
				DATA_out : out  STD_LOGIC_VECTOR (31 downto 0);				-- data read from memory at address ADDR to write to CPU cell
				DATA_out_quick : out  STD_LOGIC_VECTOR (31 downto 0);		-- data without byte or word right shifted and zero padded
				wea : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);						-- direct connections to SRAM module
				addra : OUT STD_LOGIC_VECTOR(31 DOWNTO 2);					-- longword address
				dina : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);						
				douta : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
				web : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
				addrb : OUT STD_LOGIC_VECTOR(31 DOWNTO 2);
				dinb : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
				doutb : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
				en_a : OUT STD_LOGIC;
				en_b : OUT STD_LOGIC
	 );
end SRAM_controller;

architecture RTL of SRAM_controller is

	constant blank : std_logic_vector(31 downto 0):= "00000000000000000000000000000000";

	signal base, base_plus : std_logic_vector(31 downto 2);
	signal offset, offset_m : std_logic_vector(1 downto 0);
	signal DATA_out_agg, DATA_out_agg_plus, DATA_out_base, DATA_out_base_plus : std_logic_vector(31 downto 0);
	signal DATA_out_i, DATA_out_plus_i : std_logic_vector(31 downto 0);
	signal DATA_in_base, DATA_in_base_plus, DATA_in_r : std_logic_vector(31 downto 0);
	signal size_m : std_logic_vector(1 downto 0);
	
begin

	base <=  ADDR(31 downto 2);				-- convert the byte address, ADDR, to the longword address, base
	base_plus <= base + 1;						-- base_plus is the address of the next following longword
	offset <= ADDR(1 downto 0);				-- offset is the offset from base requested by ADDR
	en_a <= en;
	en_b <= en;
	addra <= base;
	addrb <= base_plus;
	dina <= DATA_in_base;						-- connect SRAM port a to the longword address and port b to the next following longword address
	dinb <= DATA_in_base_plus;
	data_out_base <= douta;			
	data_out_base_plus <= doutb;
	data_out_quick <= DATA_out_agg;
	DATA_out <= DATA_out_i;
	
	process
	begin
		wait until rising_edge(clk);
		offset_m <= offset;  			-- when reading need to use the one-cycle-old value of offset to take account of the one cycle delay in a memory read	
	end process;			

	 
   with offset_m select					-- read a longword at byte address ADDR by combining the data from port a and port b 
      DATA_out_agg <= 	DATA_out_base (31 downto 0) when "00",
								DATA_out_base (23 downto 0) & DATA_out_base_plus (31 downto 24) when "01",
								DATA_out_base (15 downto 0) & DATA_out_base_plus (31 downto 16) when "10",
								DATA_out_base (7 downto 0) & DATA_out_base_plus (31 downto 8) when others;
								
	with size select						-- realign data from memory bus with CPU cell and left pad with '0'
		DATA_out_i <=		DATA_out_agg (31 downto 0) when "11",
								blank(31 downto 16) & DATA_out_agg (31 downto 16) when "10",
								blank(31 downto 8) & DATA_out_agg (31 downto 24) when others;

	with offset & size select		-- align data from CPU cell with memory bus, port a
		DATA_in_base <= 	DATA_in when "0011",
								blank (31 downto 24) & DATA_in (31 downto 8) when "0111",
								blank (31 downto 16) & DATA_in (31 downto 16) when "1011",
								blank (31 downto 8) & DATA_in (31 downto 24) when "1111",
							  
								DATA_in (15 downto 0) & blank (15 downto 0) when "0010",
								blank (31 downto 24) & DATA_in (15 downto 0) & blank (7 downto 0) when "0110",
								blank (31 downto 16) & DATA_in (15 downto 0) when "1010",
								blank (31 downto 8) & DATA_in (15 downto 8) when "1110",

								DATA_in (7 downto 0) & blank (23 downto 0) when "0001",
								blank (31 downto 24) & DATA_in (7 downto 0) & blank (15 downto 0) when "0101",
								blank (31 downto 16) & DATA_in (7 downto 0) & blank (7 downto 0) when "1001",
								blank (31 downto 8) & DATA_in (7 downto 0) when others; -- "1101"

	with WE & offset & size select		-- align byte select write enable signals, port a
					wea <= 	"1111" when "10011",
								"0111" when "10111",
								"0011" when "11011",
								"0001" when "11111",
							  
								"1100" when "10010",
								"0110" when "10110",
								"0011" when "11010",
								"0001" when "11110",

								"1000" when "10001",
								"0100" when "10101",
								"0010" when "11001",
								"0001" when "11101",
								"0000" when others;
								
	with offset & size select			-- align data from CPU with memory bus, port b
		DATA_in_base_plus <=	DATA_in (7 downto 0) & blank (23 downto 0) when "0111",
								DATA_in (15 downto 0) & blank (15 downto 0) when "1011",
								DATA_in (23 downto 0) & blank (7 downto 0) when "1111",
								DATA_in (7 downto 0) & blank (23 downto 0) when "1110",
								blank when others;						  
							  
	with WE & offset & size select		-- align byte select write enable signals, port b
					web <=	"1000" when "10111",
								"1100" when "11011",
								"1110" when "11111",
								"1000" when "11110",
								"0000" when others;

		
end RTL;

