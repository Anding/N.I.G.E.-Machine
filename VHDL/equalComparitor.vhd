library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity equalComparitor is
    Port ( a : in  STD_LOGIC_VECTOR (31 downto 0);
           b : in  STD_LOGIC_VECTOR (31 downto 0);
           a_eq_b : out  STD_LOGIC);
end equalComparitor;

architecture Behavioral of equalComparitor is

begin
	
	process (a, b)
	begin
		if a = b then
			a_eq_b <= '1';
		else
			a_eq_b <= '0';
		end if;
	end process;

end Behavioral;

