-- Ethernet CRC-32 FCS calculator
-- Andrew Read, 31 Oct 2015

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity CRC32calc is
    Port ( clk : in  STD_LOGIC;
           reset : in  STD_LOGIC;
			  data : in  STD_LOGIC;
           checksum : out  STD_LOGIC_VECTOR (31 downto 0)	-- interpret the checksum as a single 32 bit value
			  );
end CRC32calc;

-- Technical notes
--		Clocking:
--		data is examined on the rising edge of each clock cycle

--		Data format:
--		if an RMII PHY interface is being checksumed, feed D0 before D1
--		if a pre-prepared Ethernet frame is being checksumed, feed the data byte by byte, LSB first for each byte

--		Checksum:
--		if the CRC-32 calculator is run over an incoming Ethernet frame excluding the last 4 bytes FCS, then the four octets should match the four octets of the FCS
--		if the CRC-32 calculator is run over an incoming Ethernet frame including the FCS, then the 32 bit checksum should always be the magic number hex 38FB2284 and the octets are "1C DF 44 21"
-- 	if the CRC-32 calculator is run over an outgoing Ethernet frame then the generated 32 bit checksum should be transmitted following the payload, high bit first

architecture RTL of CRC32calc is

signal SR : std_logic_vector (31 downto 0);			-- shift register that will be hold the CRC calculation
signal octets : STD_LOGIC_VECTOR (31 downto 0);		-- interpret the checksum as 4 little-endian octets
signal inbit : std_logic;

begin

-- Ethernet CRC-32 specifies bit inversion of the final checksum

checksum <= not SR;

-- if the checksum is to be intrepreted as 4 little-endian octets then the following bit reversals are required
--  debugging use only.  These signals are not connected and will be trimmed by the synthesizer

octets(0) <= not SR(7);
octets(1) <= not SR(6);
octets(2) <= not SR(5);
octets(3) <= not SR(4);
octets(4) <= not SR(3);
octets(5) <= not SR(2);
octets(6) <= not SR(1);
octets(7) <= not SR(0);
octets(8) <= not SR(15);
octets(9) <= not SR(14);
octets(10) <= not SR(13);
octets(11) <= not SR(12);
octets(12) <= not SR(11);
octets(13) <= not SR(10);
octets(14) <= not SR(9);
octets(15) <= not SR(8);
octets(16) <= not SR(23);
octets(17) <= not SR(22);
octets(18) <= not SR(21);
octets(19) <= not SR(20);
octets(20) <= not SR(19);
octets(21) <= not SR(18);
octets(22) <= not SR(17);
octets(23) <= not SR(16);
octets(24) <= not SR(31);
octets(25) <= not SR(30);
octets(26) <= not SR(29);
octets(27) <= not SR(28);
octets(28) <= not SR(27);
octets(29) <= not SR(26);
octets(30) <= not SR(25);
octets(31) <= not SR(24);


-- Standard CRC-32 algorithm implemented as a shift register with feedback
--	 ref. Hackers Delight, Henry S. Warren Jr., Chapter 14
inbit <= data XOR SR(31);

process
begin
	wait until rising_edge(clk);
	if reset = '1' then 
		SR <= (others=>'1');								-- Ethernet CRC-32 sets all bits high before conversion
	else
		SR(31 downto 27) <= SR(30 downto 26);
		SR(26) <= inbit XOR SR(25);
		SR(25 downto 24) <= SR(24 downto 23);
		SR(23) <= inbit XOR SR(22);
		SR(22) <= inbit XOR SR(21);
		SR(21 downto 17) <= SR(20 downto 16);
		SR(16) <= inbit XOR SR(15);
		SR(15 downto 13) <= SR(14 downto 12);
		SR(12) <= inbit XOR SR(11);
		SR(11) <= inbit XOR SR(10);
		SR(10) <= inbit XOR SR(9);
		SR(9) <= SR(8);
		SR(8) <= inbit XOR SR(7);
		SR(7) <= inbit XOR SR(6);
		SR(6) <= SR(5);
		SR(5) <= inbit XOR SR(4);
		SR(4) <= inbit XOR SR(3);
		SR(3) <= SR(2);
		SR(2) <= inbit XOR SR(1);
		SR(1) <= inbit XOR SR(0);
		SR(0) <= inbit;
	end if;
end process;

end RTL;

