-- Reset controller
-- Created 13 August 2011

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;

entity Controller is
    Port ( clk : in  STD_LOGIC;
			  trig : in STD_LOGIC;
           reset : out STD_LOGIC);
end Controller;

architecture Behavioral of Controller is

type state_T is (startup, pause, run);
signal state : state_T := startup;
signal state_n : state_T;
signal counter : std_logic_vector(31 downto 0) := (others=>'0');
signal timer : std_logic_vector(31 downto 0);

begin

	process
		begin
			wait until rising_edge(clk);
			counter <= counter + 1;
			if counter >= timer then 
				counter <= (others=>'0');
				state <= state_n;
			end if;
	end process;
	
	process (state, trig)
		begin
		case state is
			when startup =>
				reset <= '1';
				state_n <= run;
				timer <= CONV_STD_LOGIC_VECTOR(10,32);	-- 25000000 quarter second at startup	
				
			when pause =>
				reset <= '1';	
				state_n <= run;										
				timer <= CONV_STD_LOGIC_VECTOR(400000000,32);	-- 4 seconds for RAM update (enough for 16KB at 57600 baud)
				
			when run =>													
				reset <= '0';
				if trig = '1' then
					state_n <= pause;
				else
					state_n <= run;
				end if;
				timer <= (others=>'0');	
				
		end case;
	end process;

end Behavioral;

