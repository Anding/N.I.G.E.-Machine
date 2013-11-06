library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_SIGNED.ALL;

entity equalZeroComparitor is
    Port ( a : in  STD_LOGIC_VECTOR (31 downto 0);
           a_eqzero : out  STD_LOGIC);
end equalZeroComparitor;

architecture Behavioral of equalZeroComparitor is

begin
	
	process (a)
	begin
		if a = 0 then
			a_eqzero <= '1';
		else
			a_eqzero <= '0';
		end if;
	end process;

end Behavioral;

