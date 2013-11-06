library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity unsignedComparitor is
    Port ( a : in  STD_LOGIC_VECTOR (31 downto 0);
           b : in  STD_LOGIC_VECTOR (31 downto 0);
           a_lt_b : out  STD_LOGIC);
end unsignedComparitor;

architecture Behavioral of unsignedComparitor is

begin
	
	process (a, b)
	begin
		if a < b then
			a_lt_b <= '1';
		else
			a_lt_b <= '0';
		end if;
	end process;

end Behavioral;

