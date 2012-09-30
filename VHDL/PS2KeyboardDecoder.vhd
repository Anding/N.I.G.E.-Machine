-- Andrew Read
-- Create Date: 25 April 2010

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity PS2KeyboardDecoder is
    Port ( clk : in STD_LOGIC;
			  PS2C : in  STD_LOGIC;
           PS2D : in  STD_LOGIC;
			  irq : out STD_LOGIC;
           data : out  STD_LOGIC_VECTOR (7 downto 0));
end PS2KeyboardDecoder;

architecture Behavioral of PS2KeyboardDecoder is
signal PS2C_m1, d_PS2C : STD_LOGIC;											-- for debounce and falling edge detection
signal count : STD_LOGIC_VECTOR (3 downto 0) := (others=>'0');		-- bit count
signal timeout : STD_LOGIC_VECTOR (12 downto 0) := (others=>'0');	-- timeout to avoid getting out of phase
signal value : STD_LOGIC_VECTOR (10 downto 0);							-- 11 bit data 
signal trig, err : std_logic;													-- internal logic

begin

	data <= value(8 downto 1);						-- data format is start, LSB ... MSB, parity, stop
	trig <= (not d_PS2C) and PS2C_m1;			-- falling edge on PS2C (debounced)
	err <= '0' when value(0) = '0' and value(10) = '1' and
					(value(1) xor value(2) xor value(3) xor value(4) xor value(5) xor value(6) xor value(7) xor value(8) xor value(9)) = '1'
					else '1';							-- check start, stop and parity bits
	irq <= '1' when count = 11 and err = '0' else '0';	-- interrupt
		
	process												-- debounce (from Pedronoi p386)
		variable c : integer range 0 to 127;
	begin
		wait until rising_edge(clk);
		if d_PS2C = PS2C then
			c := 0;
		else
			c := c + 1;
			if (c = 127) then 
				d_PS2C <= PS2C;
				c := 0;
			end if;
		end if;
	end process;

	process												-- PS/2 read
	begin
		wait until rising_edge(clk);
		
		PS2C_m1 <= d_PS2C;							-- for falling edge clock detection
		
		if d_PS2C = '0' then							-- timeout to avoid getting out of phase, reset each clock low
			timeout <= (others => '0');
		else
			timeout <= timeout + 1;
		end if;
		
		if count = 11 or timeout = 8191 then	-- manage bit counter
			count <= (others=>'0');
		elsif trig = '1' then
			count <= count + 1;
		end if;

		if trig = '1' then
			value <= PS2D & value(10 downto 1);	-- shift in bits
		end if;
	end process;
	
end Behavioral;

