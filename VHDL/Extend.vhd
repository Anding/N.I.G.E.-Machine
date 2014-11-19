-- GenMux unit
-- Andrew Read
-- Created 30 April 2011
-- Modified 2 May 2011

--	 0		NOS
--	 1		[PSP] = PSdata
--	 2		[RSP] = RSdata
--	 3		PSP
--	 4		RSP
--	 5		XCHAR
--	 6		XWORD
--	 7		Data

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity GenMux is
	 Generic (	psp_w : integer;
					rsp_w : integer
					);
    Port ( TOS : in STD_LOGIC_VECTOR (31 downto 0);
			  NOS : in  STD_LOGIC_VECTOR (31 downto 0);
			  PSdata : in STD_LOGIC_VECTOR (31 downto 0);
			  RSdata : in STD_LOGIC_VECTOR (31 downto 0);
			  PSP : in STD_LOGIC_VECTOR (psp_w -1 downto 0);
			  RSP : in STD_LOGIC_VECTOR (rsp_w -1 downto 0);
			  Data : in STD_LOGIC_VECTOR (31 downto 0);
           Control : in  STD_LOGIC_VECTOR (2 downto 0);
           Output : out  STD_LOGIC_VECTOR (31 downto 0));
end GenMux;

architecture RTL of GenMux is
alias sc is TOS(7);
alias sw is TOS(15);
constant blank : std_logic_vector (31 downto 0) := "00000000000000000000000000000000";

begin
 
	with Control select Output
		<= PSdata when "001",
			RSdata when "010",
			blank (31 - psp_w downto 0) & PSP when "011",
			blank (31 - psp_w downto 0) & RSP when "100",
			sc & sc & sc & sc & sc & sc & sc & sc & sc & sc & sc & sc & sc & sc & sc & sc & sc & sc & sc & sc & sc & sc & sc & sc & TOS(7 downto 0) when "101",
			sw & sw & sw & sw & sw & sw & sw & sw & sw & sw & sw & sw & sw & sw & sw & sw & TOS(15 downto 0) when "110",
			Data when "111",
			NOS when others;      -- NOS as default
end RTL;

