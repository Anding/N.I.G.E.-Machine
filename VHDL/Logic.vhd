-- Logic unit
-- Andrew Read
-- Created 30 April 2011

--	0	AND
--	1	OR
--	2	INVERT
--	3	XOR
--	4	LSL
--	5	LSR
--	6	ASL, identical with LSL since there is no overflow flag
--	7	ASR

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity Logic is
    Port ( PortA : in  STD_LOGIC_VECTOR (31 downto 0);	-- NOS
           PortB : in  STD_LOGIC_VECTOR (31 downto 0);	-- TOS
           Control : in  STD_LOGIC_VECTOR (2 downto 0);
           Output : out  STD_LOGIC_VECTOR (31 downto 0));
end Logic;

architecture RTL of Logic is

begin

process (PortA, PortB, control)
	begin
		case control is
			when "000" =>
				Output <= PortA and PortB;
			when "001" =>
				Output <= PortA or PortB;
			when "011" =>
				Output <= PortA xor PortB;
			when "100" =>
				Output <= PortB(30 downto 0) & '0';
			when "101" =>
				Output <= '0' & PortB(31 downto 1);
			when "110" =>
				Output <= PortB(30 downto 0) & '0';  -- identical with LSL since there is no overflow flag
			when "111" =>
				Output <= PortB(31) & PortB(31 downto 1);
			when others =>
				Output <= not PortB;				-- INVERT is the default
		end case;
	end process;

end RTL;

