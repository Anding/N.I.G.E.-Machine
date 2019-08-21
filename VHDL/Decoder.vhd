-- Decoder
-- Andrew Read
-- Created 21 August 2019

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use work.Common.all;

entity Decoder is
	port (	opcode : in std_logic_vector(6 downto 0);
			ESP_control : out ESP_control_type;
			SSP_control : out SSP_control_type
	);

end Decoder;

architecture RTL of Decoder is
	
begin
	with opcode select ESP_control <=
		plus_one when "0001011",
		zero when "0001100",
		minus_one when "0001101",
		from_ts when "0111101",
		no_change when others;
	
	with opcode select SSP_control <=
		plus_one when "0001011",
		zero when "0001100",
		from_es when "0001101",
		plus_one when "0111000",
		plus_one when "0111001",
		from_ts when "0111101",
		no_change when others;	
			
end RTL;
