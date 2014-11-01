-- RAM for testing CPU
-- Andrew Read
-- Created 23 May 2011

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use std.textio.all;

entity RAM_for_Testbench is
    Port ( rst : in  STD_LOGIC;
           clk : in  STD_LOGIC;
           weA : in  STD_LOGIC_VECTOR (3 downto 0);
			  weB : in  STD_LOGIC_VECTOR (3 downto 0);
			  enA	: in	STD_LOGIC;
			  enB : in	STD_LOGIC;
           addressA : in  STD_LOGIC_VECTOR (16 downto 2);
           data_inA : in  STD_LOGIC_VECTOR (31 downto 0);
           data_outA : out  STD_LOGIC_VECTOR (31 downto 0);
           addressB : in  STD_LOGIC_VECTOR (16 downto 2);
			  data_inB : in  STD_LOGIC_VECTOR (31 downto 0); 
           data_outB : out  STD_LOGIC_VECTOR (31 downto 0)
			  );
end RAM_for_Testbench;

architecture Behavioral of RAM_for_Testbench is
	type memory is array (0 to 8191) of std_logic_vector(31 downto 0);	-- 32K bytes
	file f : text open read_mode is "E:\N.I.G.E.-Machine\System\sram.txt";	
	signal sysRAM : memory := (others=>X"00000000");
	signal addressA_i, addressB_i : integer;
	signal CE_A, CE_B : std_logic;

begin
addressA_i <= CONV_INTEGER(addressA);
addressB_i <= CONV_INTEGER(addressB);
CE_A <= '1' when (addressA_i < 8192) and enA = '1' else '0';
CE_B <= '1' when (addressB_i < 8192) and enB = '1' else '0';

	process (clk, rst, CE_A, addressA_i, weA, data_inA, weB, data_inB, CE_B, addressB_i)
		variable l : line;
		variable i : integer := 0;
		variable j : bit_vector(31 downto 0);
		variable ok : boolean;
		variable temp_a : bit_vector(31 downto 0); 
		variable temp_b : bit_vector(31 downto 0); 
		
		begin
		if rst = '0' then
			if rising_edge(clk) then
			
				if CE_A = '1' then
					data_outA <= sysRAM(addressA_i);
					temp_a := To_BitVector(sysRAM(addressA_i));
					if (weA(0) = '1') then 
						temp_a(7 downto 0) := To_BitVector(data_inA(7 downto 0)); 
					end if;
					if (weA(1) = '1') then temp_a(15 downto 8) := To_BitVector(data_inA(15 downto 8)); end if;
					if (weA(2) = '1') then temp_a(23 downto 16) := To_BitVector(data_inA(23 downto 16)); end if;
					if (weA(3) = '1') then temp_a(31 downto 24) := To_BitVector(data_inA(31 downto 24)); end if;
					sysRAM(addressA_i) <= To_StdLogicVector(temp_a);
				else
					data_outA <= (others=>'0');
				end if;
				
				if CE_B = '1' then
					data_outB <= sysRAM(addressB_i);
					temp_b := To_BitVector(sysRAM(addressB_i));
					if (weB(0) = '1') then temp_b(7 downto 0) := To_BitVector(data_inB(7 downto 0)); end if;
					if (weB(1) = '1') then temp_b(15 downto 8) := To_BitVector(data_inB(15 downto 8)); end if;
					if (weB(2) = '1') then temp_b(23 downto 16) := To_BitVector(data_inB(23 downto 16)); end if;
					if (weB(3) = '1') then temp_b(31 downto 24) := To_BitVector(data_inB(31 downto 24)); end if;
					sysRAM(addressB_i) <= To_StdLogicVector(temp_b);
				else
					data_outB <= (others=>'0');
				end if;
				
			end if;
		else
			while not endfile(f) loop
				readline(f,l);
				read(l,j,ok);
				assert ok;
				sysRAM(i) <= To_StdLogicVector(j);				
				i := i + 1;
			end loop;	
			--file_close(f);
		end if;
	end process;
	
end Behavioral;

