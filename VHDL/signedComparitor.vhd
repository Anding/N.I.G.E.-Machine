library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_SIGNED.ALL;

entity signedComparitor is
    Port ( a : in  STD_LOGIC_VECTOR (31 downto 0);
           b : in  STD_LOGIC_VECTOR (31 downto 0);
           a_lt_b : out  STD_LOGIC);
end signedComparitor;

architecture Behavioral of signedComparitor is

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

