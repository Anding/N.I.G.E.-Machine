library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;


entity ByteHEXdisplay is
    Port ( ssData		: in  STD_LOGIC_VECTOR (31 downto 0);
			  clk		 	: in  STD_LOGIC;
			  count		: in  STD_LOGIC_VECTOR (15 downto 13);
           sevenseg 	: out  STD_LOGIC_VECTOR (6 downto 0);
           anode 		: out  STD_LOGIC_VECTOR (7 downto 0)
           );
end ByteHEXdisplay;

architecture Behavioral of ByteHEXdisplay is
	signal nibble : STD_LOGIC_VECTOR (3 downto 0);
	
begin

	PROCESS
	begin
		wait until rising_edge(clk);
		
			if count(15 downto 13) = "001" then				-- toggle on MSBits of count
				anode <= "11111110";
				nibble <= ssData(3 downto 0);				
			elsif count(15 downto 13) = "010" then
				anode <= "11111101";
				nibble <= ssData(7 downto 4);
			elsif count(15 downto 13) = "011" then
				anode <= "11111011";
				nibble <= ssData(11 downto 8);	
			elsif count(15 downto 13) = "100" then
				anode <= "11110111";
				nibble <= ssData(15 downto 12);
			elsif count(15 downto 13) = "101" then
				anode <= "11101111";
				nibble <= ssData(19 downto 16);	
			elsif count(15 downto 13) = "110" then
				anode <= "11011111";
				nibble <= ssData(23 downto 20);
			elsif count(15 downto 13) = "111" then
				anode <= "10111111";
				nibble <= ssData(27 downto 24);	
			else
				anode <= "01111111";
				nibble <= ssData(31 downto 28);				
			end if;
		
	case nibble is
		when "0000" => sevenseg <= "1000000";
		when "0001" => sevenseg <= "1111001";
		when "0010" => sevenseg <= "0100100";
		when "0011" => sevenseg <= "0110000";
		when "0100" => sevenseg <= "0011001";
		when "0101" => sevenseg <= "0010010";
		when "0110" => sevenseg <= "0000010";
		when "0111" => sevenseg <= "1111000";
		when "1000" => sevenseg <= "0000000";
		when "1001" => sevenseg <= "0011000";
		when "1010" => sevenseg <= "0001000";
		when "1011" => sevenseg <= "0000011";
		when "1100" => sevenseg <= "1000110";
		when "1101" => sevenseg <= "0100001";
		when "1110" => sevenseg <= "0000110";
		when others => sevenseg <= "0001110";
	end case;
		
end process;

end Behavioral;

