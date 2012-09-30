library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;


entity ByteHEXdisplay is
    Port ( ssData		: in  STD_LOGIC_VECTOR (15 downto 0);
			  clk50MHz 	: in  STD_LOGIC;
			  count		: in  STD_LOGIC_VECTOR (15 downto 14);
           sevenseg 	: out  STD_LOGIC_VECTOR (6 downto 0);
           anode 		: out  STD_LOGIC_VECTOR (3 downto 0)
           );
end ByteHEXdisplay;

architecture Behavioral of ByteHEXdisplay is
	signal nibble : STD_LOGIC_VECTOR (3 downto 0);
	
begin

	PROCESS
	begin
		wait until rising_edge(clk50MHz);
		
			if count(15 downto 14) = "00" then				-- toggle on MSBits of count
				anode <= "1110";
				nibble <= ssData(3 downto 0);				
			elsif count(15 downto 14) = "01" then
				anode <= "1101";
				nibble <= ssData(7 downto 4);
			elsif count(15 downto 14) = "10" then
				anode <= "1011";
				nibble <= ssData(11 downto 8);	
			else
				anode <= "0111";
				nibble <= ssData(15 downto 12);				
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

