-- just registered datain_r
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity SRAM_controller is
    Port ( 	RST : in STD_LOGIC;
				CLK : in STD_LOGIC;
				ADDR : in  STD_LOGIC_VECTOR (31 downto 0);					-- byte address
				size : in  STD_LOGIC_VECTOR (1 downto 0);						-- length of read or write 01 = byte, 02 = word, 03 = longword
				size_plus : in  STD_LOGIC_VECTOR (1 downto 0);				-- length of data to read at ADDR+1
				WE : in  STD_LOGIC_VECTOR (0 downto 0);						-- write enable
				DATA_in : in  STD_LOGIC_VECTOR (31 downto 0);				-- data to write to memory
				DATA_out : out  STD_LOGIC_VECTOR (31 downto 0);				-- data read from memory at address ADDR
				DATA_out_quick : out  STD_LOGIC_VECTOR (31 downto 0);		
				DATA_out_plus : out  STD_LOGIC_VECTOR (31 downto 0);		-- data read from memory at address ADDR+1
				wea : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);						-- direct connections to SRAM module
				addra : OUT STD_LOGIC_VECTOR(15 DOWNTO 2);
				dina : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
				douta : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
				web : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
				addrb : OUT STD_LOGIC_VECTOR(15 DOWNTO 2);
				dinb : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
				doutb : IN STD_LOGIC_VECTOR(31 DOWNTO 0)
	 );
end SRAM_controller;

architecture RTL of SRAM_controller is
	signal base, base_plus : std_logic_vector(15 downto 2);
	signal offset, offset_m : std_logic_vector(1 downto 0);
	signal DATA_out_agg, DATA_out_agg_plus, DATA_out_base, DATA_out_base_plus : std_logic_vector(31 downto 0);
	signal DATA_out_i, DATA_out_plus_i : std_logic_vector(31 downto 0);
	signal DATA_in_base, DATA_in_base_plus, DATA_in_r : std_logic_vector(31 downto 0);
	signal WE_i, WE_m : std_logic_vector(0 downto 0);
	signal size_m : std_logic_vector(1 downto 0);
	
begin

	base <=  ADDR(15 downto 2);				-- convert the byte address, ADDR, to the longword address, base
	base_plus <= base + 1;						-- base_plus is the address of the next following longword
	offset <= ADDR(1 downto 0);				-- offset is the offset from base requested by ADDR
	WE_i <= WE;										-- need to operate on WE as a local signal for correct behaviour
	wea <= WE_m;
	web <= WE_m;
	addra <= base;
	addrb <= base_plus;
	dina <= DATA_in_base;						-- connect SRAM port a to the longword address and port b to the next following longword address
	dinb <= DATA_in_base_plus;
	data_out_base <= douta;			
	data_out_base_plus <= doutb;
	data_out_quick <= DATA_out_agg;
	DATA_out <= DATA_out_i;
	DATA_out_plus <= DATA_out_plus_i;
	
--	process
--	begin
--	if clk = '0' then
--		wea <= WE_m;
--		web <= WE_m;
--	else
--		wea <= '0';
--		web <= '0';
--	end if;
--	end process;
	
	process
	begin
		wait until rising_edge(clk);
		WE_m <= WE_i;-- 	write is a 2 cyle operation, first to read existing contents and then to overlay and write the new values to ports a and b
		offset_m <= offset;  -- when reading need to use the one-cycle-old value of offset to take account of the one cycle delay in a memory read	
		data_in_r <= data_in;
		size_m <= size;
	end process;			
	
--	process
--	begin
--		wait until rising_edge(clk2x);	
--		if clk = '1' then
--
--		else
--		   data_in_r <= data_in;		
--			WE_m <= WE_i;
--			offset_m <= offset;  -- when reading need to use the one-cycle-old value of offset to take account of the one cycle delay in a memory read				
--		end if;
--	end process;			-- 	write is a 2 cyle operation, first to read existing contents and then to overlay and write the new values to ports a and b
--		 

	 
   with offset_m select					-- read a longword at byte address ADDR by combining the data from port a and port b 
--		DATA_out_agg <= 	DATA_out_base (31 downto 0) when others;
      DATA_out_agg <= 	DATA_out_base (31 downto 0) when "00",
								DATA_out_base (23 downto 0) & DATA_out_base_plus (31 downto 24) when "01",
								DATA_out_base (15 downto 0) & DATA_out_base_plus (31 downto 16) when "10",
								DATA_out_base (7 downto 0) & DATA_out_base_plus (31 downto 8) when others;
								
	with offset_m select					-- read a longword at byte address ADDR+1 (this is used by load literal instructions)
--		DATA_out_agg_plus <= DATA_out_base (31 downto 0) when others;
		DATA_out_agg_plus <=	DATA_out_agg (23 downto 0) & DATA_out_base_plus (31 downto 24) when "00",
								DATA_out_agg (23 downto 0) & DATA_out_base_plus (23 downto 16) when "01",
								DATA_out_agg (23 downto 0) & DATA_out_base_plus (15 downto 8) when "10",
								DATA_out_base_plus when others;

	with size_plus select						-- trim the longword to word or byte length if needed
--		DATA_out_plus <=		DATA_out_agg_plus (31 downto 0) when others;
		DATA_out_plus_i <=		DATA_out_agg_plus (31 downto 0) when "11",
								"0000000000000000" & DATA_out_agg_plus (31 downto 16) when "10",
								"000000000000000000000000" & DATA_out_agg_plus (31 downto 24) when others;

	with size select
--		DATA_out <=			DATA_out_agg (31 downto 0) when others;
		DATA_out_i <=			DATA_out_agg (31 downto 0) when "11",
								"0000000000000000" & DATA_out_agg (31 downto 16) when "10",
								"000000000000000000000000" & DATA_out_agg (31 downto 24) when others;

	with offset_m & size select		-- overlay a longword, word, or byte at address ADDR with the existing memory contents in preparation for a write on port a
--		DATA_in_base <= 	DATA_in when others;
		DATA_in_base <= 	DATA_in_r when "0011",
								DATA_out_base (31 downto 24) & DATA_in_r (31 downto 8) when "0111",
								DATA_out_base (31 downto 16) & DATA_in_r (31 downto 16) when "1011",
								DATA_out_base (31 downto 8) & DATA_in_r (31 downto 24) when "1111",
							  
								DATA_in_r (15 downto 0) & DATA_out_base (15 downto 0) when "0010",
								DATA_out_base (31 downto 24) & DATA_in_r (15 downto 0) & DATA_out_base (7 downto 0) when "0110",
								DATA_out_base (31 downto 16) & DATA_in_r (15 downto 0) when "1010",
								DATA_out_base (31 downto 8) & DATA_in_r (15 downto 8) when "1110",

								DATA_in_r (7 downto 0) & DATA_out_base (23 downto 0) when "0001",
								DATA_out_base (31 downto 24) & DATA_in_r (7 downto 0) & DATA_out_base (15 downto 0) when "0101",
								DATA_out_base (31 downto 16) & DATA_in_r (7 downto 0) & DATA_out_base (7 downto 0) when "1001",
								DATA_out_base (31 downto 8) & DATA_in_r (7 downto 0) when others; -- "1101"
							  
	with offset_m & size_m select  -- overlay in preparation for a write for port b
--	DATA_in_base_plus <=DATA_out_base_plus when others;
	DATA_in_base_plus <=	DATA_in_r (7 downto 0) & DATA_out_base_plus (23 downto 0) when "0111",
								DATA_in_r (15 downto 0) & DATA_out_base_plus (15 downto 0) when "1011",
								DATA_in_r (23 downto 0) & DATA_out_base_plus (7 downto 0) when "1111",
								DATA_in_r (7 downto 0) & DATA_out_base_plus (23 downto 0) when "1110",
								DATA_out_base_plus when others;

		
end RTL;

