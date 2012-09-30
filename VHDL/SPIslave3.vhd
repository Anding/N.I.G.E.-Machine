-- Andrew Read
-- Create Date: August 2012

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;


entity SPIslave3 is
    Port ( CS : in  STD_LOGIC;
           MOSI : in  STD_LOGIC;
           MISO : out  STD_LOGIC;
           SCK : in  STD_LOGIC;
			  mode : in STD_LOGIC_VECTOR(1 downto 0)
			  );
end SPIslave3;

architecture RTL of SPIslave3 is
signal SR : std_logic_vector(8 downto 0);
signal count : std_logic_vector(2 downto 0) :=(others=>'0');
signal data : std_logic_vector(7 downto 0) := "10101010";
signal SR_load, SR_shift, SR_latch : std_logic_vector(8 downto 0);
begin

MISO <= SR(8);
SR_load <= data & '0';
SR_shift <= SR(7 downto 0) & '0';
SR_latch <= SR(8 downto 1) & MOSI;

process (cs, SR_load, SR_latch, SR_shift, SCK, count, mode)
begin
	if cs='0' then
	
		count <= (others=>'0');
		SR <= SR_load;
		
	elsif (rising_edge(SCK) and mode(0)= '1')
		or	(falling_edge(SCK) and mode(1) = '0') then
		
		SR <= SR_latch;
		
	elsif (falling_edge(SCK) and mode(0)= '0')
		or	(rising_edge(SCK) and mode(1) = '1') then
		
		if count = 7 then
			SR <= SR_load;
		else
			SR <= SR_shift;
		end if;
		count <= count + 1;
		
	end if;
end process;

end RTL;

