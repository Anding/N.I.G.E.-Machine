-- Reset controller
-- Created 13 August 2011

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;

entity Controller is
Port (
	clk : in  STD_LOGIC;
	locked : in STD_LOGIC;
	CPUreset : in STD_LOGIC;
	reset : out STD_LOGIC;
	counter_clk : out std_logic_vector(31 downto 0);
	counter_ms : out std_logic_vector(31 downto 0);
	ms_irq : out STD_LOGIC
	    );
end Controller;

architecture Behavioral of Controller is

type state_T is (startup, softreset, run);
signal state : state_T := startup;
signal state_n : state_T;
signal counter_clk_i, counter_ms_i : std_logic_vector(31 downto 0);
signal counter_clk_for_ms: std_logic_vector(16 downto 0);
signal counter, timer : std_logic_vector(31 downto 0);

begin

counter_clk <= counter_clk_i;
counter_ms <= counter_ms_i;
	
-- state machine and counter update
process (locked, clk)
begin
	if locked = '0' then
		state <= startup;
		counter <= (others=>'0');
	elsif rising_edge(clk) then
		if counter >= timer then 
			counter <= (others=>'0');
			state <= state_n;
		else
			counter <= counter + 1;
		end if;
	end if;
end process;

-- next state process
process (state, CPUreset)
	begin
	case state is
		when startup =>
			reset <= '1';
			state_n <= run;
			timer <= (others=>'0');
			
		when softreset =>
			reset <= '1';	
			state_n <= run;										
			timer <= CONV_STD_LOGIC_VECTOR(400000000,32);	-- 4 seconds for RAM update (enough for 16KB at 57600 baud)
			
		when run =>													
			reset <= '0';
			if CPUreset = '0' then
				state_n <= softreset;
			else
				state_n <= run;
			end if;
			timer <= (others=>'0');	
			
	end case;
end process;

-- ms interrupt
process														
begin
	wait until rising_edge(clk);
	if state /= run then
		counter_clk_i <= (others => '0');
		counter_clk_for_ms <= (others => '0');
		counter_ms_i <= (others => '0');
		ms_irq <= '0';
	else
		counter_clk_i <= counter_clk_i + 1;
		if counter_clk_for_ms = CONV_STD_LOGIC_VECTOR(100000,17) then
			counter_clk_for_ms <= (others =>'0');
			counter_ms_i <= counter_ms_i + 1;
			ms_irq <= '1';
		else
			counter_clk_for_ms <= counter_clk_for_ms + 1;
			ms_irq <= '0';
		end if;
	end if;
end process;

end Behavioral;

