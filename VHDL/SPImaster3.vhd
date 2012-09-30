-- Andrew Read
-- Create Date: August 2012

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity SPImaster is
    Port (	CLK : in STD_LOGIC;
				CLKSPI : in STD_LOGIC;
				RESET : in STD_LOGIC;
				DATA_IN : out STD_LOGIC_VECTOR(7 downto 0);
				WR : in STD_LOGIC;
				TBE : out STD_LOGIC;
				DATA_OUT : in STD_LOGIC_VECTOR(7 downto 0);
				MOSI : out  STD_LOGIC;
				MISO : in  STD_LOGIC;
				SCK : out  STD_LOGIC;
				mode : in STD_LOGIC_VECTOR(1 downto 0);
				MOSI_def : in STD_LOGIC
				);
end SPImaster;

architecture RTL of SPImaster is
type state_T is (idle, run);
type trigstate_T is (idle, run1, run2);
signal state, state_n : state_T;
signal trigstate, trigstate_n : trigstate_T;
signal SR : std_logic_vector(8 downto 0);
signal count : std_logic_vector(2 downto 0) :=(others=>'0');
signal SR_load, SR_shift, SR_latch : std_logic_vector(8 downto 0);
signal SCK_en : std_logic;
signal CLKSPI_m : std_logic;

begin

with SCK_en select
	MOSI <= SR(8) when '1',
		     MOSI_def when others;

with mode(1) select
	SCK <= (SCK_en and CLKSPI) when '0',
			not (SCK_en and CLKSPI) when others;
			
SR_load <= DATA_OUT & '0';
SR_shift <= SR(7 downto 0) & '0';
SR_latch <= SR(8 downto 1) & MISO;
DATA_IN <= SR(8 downto 1);		

process
begin
	wait until rising_edge(clk);
	if reset = '1' then
		trigstate <= idle;
		CLKSPI_m <= '0';
	else
		trigstate <= trigstate_n;
		CLKSPI_m <= CLKSPI;
	end if;
end process;

process (trigstate, WR, state, CLKSPI, CLKSPI_m, mode)
begin
	case trigstate is
		when idle =>
			if WR = '1' and state = idle then 
				trigstate_n <= run1;
			else
				trigstate_n <= idle;
			end if;
			TBE <= '1';
			
		when run1 =>
			if (CLKSPI = '0' and CLKSPI_m = '1' and mode(0) = '0') or 
				(CLKSPI = '1' and CLKSPI_m = '0' and mode(0) = '1') then
				trigstate_n <= run2;
			else
				trigstate_n <= run1;
			end if;
			TBE <= '0';
			
		when run2 =>
			if state = idle then
				trigstate_n <= idle;
			else
				trigstate_n <= run2;
			end if;
			TBE <= '0';		
		
		end case;
end process;

process 
begin
	wait until rising_edge(clk);
	
	if reset = '1' then
		count <= (others=>'0');
		SR <= SR_load;
		
	elsif (CLKSPI = '1' and CLKSPI_m = '0' and mode(0) = '0') or 
			(CLKSPI = '0' and CLKSPI_m = '1' and mode(0) = '1') then
		if state = run then 
			SR <= SR_latch;
		end if;
	
	elsif (CLKSPI = '0' and CLKSPI_m = '1' and mode(0) = '0') or 
			(CLKSPI = '1' and CLKSPI_m = '0' and mode(0) = '1') then
		if state = run then
			SR <= SR_shift;
			count <= count + 1;
		elsif trigstate = run1 then
			SR <= SR_load;
		end if;
		state <= state_n;
		
	end if;
end process;

process (state, trigstate, count)
begin
	case state is
		when idle =>
			if trigstate = run1 then
				state_n <= run;
			else
				state_n <= idle;
			end if;
			sck_en <= '0';
		
		when run =>
			if count = "111" then 
				state_n <= idle;
			else
				state_n <= run;
			end if;		
			sck_en <= '1';
		
	end case;
end process;

end RTL;