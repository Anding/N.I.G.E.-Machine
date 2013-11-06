-- Comparator unit
-- Andrew Read
-- Created 30 April 2011
-- Updated 4 June 2011

--	 0	 	=			NOS == TOS
--	 1	 	<>			NOT c0
--	 2	 	<		 	NOS < TOS
--	 3	 	>		 	NOT (c0 OR c2)
--	 4	 	U<			NOS U< TOS
--	 5	 	U>			NOT (c0 OR c4)
--	 6	 	=0			TOS==0
--	 7	 	<> 0	 	NOT c6
--	 8	 	< 0	 	TOS < 0
--	 9	 	> 0	 	NOT (c6 OR c8)
--	10	 	0		 	FALSE	
--	11	 	<=			c0 OR c2
--	12	 	>=			NOT c2
--	13	 	>= 0	 	NOT c8
--	14	 	<= 0	 	c6 OR c8
--	15	 	1		 	TRUE	

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity Comparator is
    Port ( PortA : in  STD_LOGIC_VECTOR (31 downto 0);			-- NOS
           PortB : in  STD_LOGIC_VECTOR (31 downto 0);			-- TOS
           Control : in  STD_LOGIC_VECTOR (3 downto 0);
           Output : out  STD_LOGIC_VECTOR (31 downto 0));
end Comparator;

architecture RTL of Comparator is

component signedComparitor
	port (
	a: IN std_logic_VECTOR(31 downto 0);
	b: IN std_logic_VECTOR(31 downto 0);
	a_lt_b: OUT std_logic);
	end component;

component unsignedComparitor
	port (
	a: IN std_logic_VECTOR(31 downto 0);
	b: IN std_logic_VECTOR(31 downto 0);
	a_lt_b: OUT std_logic);
	end component;

component equalComparitor
	port (
	a: IN std_logic_VECTOR(31 downto 0);
	b: IN std_logic_VECTOR(31 downto 0);
	a_eq_b: OUT std_logic);
	end component;

component equalZeroComparitor
	port (
	a: IN std_logic_VECTOR(31 downto 0);
	a_eqzero: OUT std_logic);
	end component;

signal c : std_logic_vector (15 downto 0);
signal output_i : std_logic;

begin

inst_equalComparitor : equalComparitor
		port map (
			a => PortA,
			b => PortB,
			a_eq_b => c(0));
			
c(1) <= not c(0);

inst_signedComparitor : signedComparitor
		port map (
			a => PortA,
			b => PortB,
			a_lt_b => c(2));
			
c(3) <= not (c(0) or c(2));
			
inst_unsignedComparitor : unsignedComparitor
		port map (
			a => PortA,
			b => PortB,
			a_lt_b => c(4));

c(5) <= not (c(0) or c(4));

inst_equalZeroComparitor : equalZeroComparitor
		port map (
			a => PortB,
			a_eqzero => c(6));

c(7) <= not c(6);

c(8) <= PortB(31);

c(9) <= not(c(6) or c(8));

c(10) <= '0';

c(11) <= c(0) or c(2);

c(12) <= not c(2);

c(13) <= not c(8);

c(14) <= c(6) or c(8);

c(15) <= '1';

with control select
	output_i <= c(0) 	when "0000",
					c(1) 	when "0001",
					c(2)	when "0010",
					c(3)	when "0011",
					c(4)	when "0100",
					c(5) 	when "0101",
					c(6) 	when "0110",
					c(7) 	when "0111",
					c(8) 	when "1000",
					c(9) 	when "1001",
					c(11)	when "1011",
					c(12) when "1100",
					c(13) when "1101",
					c(14) when "1110",
					c(15) when "1111",
					c(10)	when others;		-- false is the default output for U and X control inputs

with output_i select
	output <= 	"11111111111111111111111111111111" when '1',
					"00000000000000000000000000000000" when others;

end RTL;

