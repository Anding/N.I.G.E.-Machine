library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity BootLoader is

Port (
	reset	: in std_logic;
	clk	: in std_logic;
	-- RS232 data
	RDA	: in std_logic;
	RXDATA	: in std_logic_vector(7 downto 0);
	-- RAM update signals
	data	: out std_logic_vector(31 downto 0); 
	addr	: out std_logic_vector(31 downto 2);  
	we	: out std_logic_vector(0 downto 0)
);

end BootLoader;

architecture Behavioral of BootLoader is
type state_T is (idle, reading, writing);
signal state : state_T;
signal next_state : state_T ;
signal addr_r, addr_n: std_logic_vector(31 downto 0);
signal RDA_m, RDAtrig : std_logic;
signal data_r, data_n : std_logic_vector(31 downto 0);
 
begin

	 addr <= addr_r(31 downto 2);
	 data <= data_r;
	 RDAtrig <= '1' when (RDA ='1' and RDA_m ='0') else '0';

	process
	begin
		wait until rising_edge(clk);
			state <= next_state;
			RDA_m <= RDA;
			data_r <= data_n;
			addr_r <= addr_n;
	end process;
	
	with state select we <= "1" when writing, "0" when others;
	
	process (addr_r, RDAtrig, state)
	begin
		if RDAtrig = '1' then
			addr_n <= addr_r + 1;
		elsif state = idle then
			addr_n <= (others=>'1');
		else
			addr_n <= addr_r;
		end if;
	end process;
	
	data_n <= data_r(23 downto 0) & RXDATA when RDAtrig = '1' else data_r;
					
	process (state, RDAtrig, addr_r, reset)
	begin
		if reset = '1' then										-- Bootloader is active during reset phase
			case state is
				when reading =>
					if addr_r(1 downto 0) = "10" and RDAtrig = '1' then
						next_state <= writing;
					else
						next_state <= state;
					end if;					
						
				when writing =>
					next_state <= reading;
					
				when others =>		-- idle
					if RDAtrig = '1' then
						next_state <= reading;
					else
						next_state <= state;
					end if;
					
			end case;
			
		else
			next_state <= idle;
		end if;
end process;

end behavioral;
