-- Andrew Read
-- Create Date: August 2012

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity DIV is
    Port ( CLKin : in  STD_LOGIC;
           divide : in  STD_LOGIC_VECTOR (7 downto 0);
           CLKout : out  STD_LOGIC);
end DIV;

architecture RTL of DIV is
signal count : std_logic_vector (7 downto 0) := "00000001";
signal CLKout_r : std_logic := '0';
signal limit : std_logic_vector (7 downto 0);

begin

limit <= '0' & divide(7 downto 1);
CLKout <= CLKout_r;
	
process
begin
	wait until rising_edge(clkin);
	if count >= limit then
		count <= "00000001";
		if CLKout_r = '0' then 
			CLKout_r <= '1';
		else
			CLKout_r <= '0';
		end if;
	else
		count <= count + 1;
	end if;
end process;	

end RTL;

