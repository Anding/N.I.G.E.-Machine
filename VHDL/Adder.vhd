-- Logic unit
-- Andrew Read
-- Created 30 April 2011
-- Updated 4 June 2011

--	0	TOS
--	1	NEGATE
--	2	ADD
-- 3 	SUB
--	4	ADDX
--	5	SUBX
--	6	1+
--	7	1-

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity Adder is
    Port ( rst : STD_LOGIC;
			  clk : STD_LOGIC;											-- needed for registering carry/borrow flag
			  PortA : in  STD_LOGIC_VECTOR (31 downto 0);		-- NOS
           PortB : in  STD_LOGIC_VECTOR (31 downto 0);		-- TOS
           ControlA : in  STD_LOGIC_VECTOR (2 downto 0);
			  ControlB : in STD_LOGIC_VECTOR (2 downto 0);
           Output : out  STD_LOGIC_VECTOR (31 downto 0));
end Adder;

architecture RTL of Adder is

component addsub
	port (
	a: IN std_logic_VECTOR(31 downto 0);
	b: IN std_logic_VECTOR(31 downto 0);
	add: IN std_logic;
	c_in: IN std_logic;
	c_out: OUT std_logic;
	s: OUT std_logic_VECTOR(31 downto 0));
end component;

constant zero : std_logic_vector (31 downto 0) := (others=>'0');
constant unity : std_logic_vector (31 downto 0) := "00000000000000000000000000000001";
constant minus1 : std_logic_vector (31 downto 0) := "11111111111111111111111111111111";
signal InputA : std_logic_vector (31 downto 0);
signal add, c_in, c_out : std_logic;
signal C, C_i : std_logic;											-- carry bit for extended precision arithmetic

begin
	instance_addsub : addsub
		port map (
			a => InputA,
			b => PortB,
			add => add,
			c_in => c_in,
			c_out => c_out,
			s => Output);

	with controlA select
		InputA <= 	zero 	when "001",
						PortA when "010",
						PortA when "011",
						PortA when "100",
						PortA	when "101",
						unity when "110",
						minus1 when "111",
						zero when others;			-- TOS is the default output for U or X input

	with controlA select
		add <= 	'0' when "001",
					'1' when "010",
					'0' when "011",
					'1' when "100",
					'0' when "101",
					'1' when "110",
					'1' when "111",
					'1' when others;				-- TOS is the default output for U or X input						
	
	with controlA select
		c_in <= 	'0' when "000",
					'1' when "001",
					'0' when "010",
					'1' when "011",
					 C  when "100",
					 C  when "101",
					'0' when "110",
					'0' when "111",
					'0' when others;				-- TOS is the default output for U or X input
	
	C_i <= c_out when (controlA /= "000" and controlB = "000") 
					 else C;							-- Only update carry bit when ALU is used 
	
	process 
	begin
		wait until rising_edge(clk);	
		if rst = '0' then
				C <= C_i;
		else
				C <= '0';
		end if;
	end process;
	
end RTL;

