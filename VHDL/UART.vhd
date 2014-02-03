-- RS232 port
-- Andrew Read

-- Setup for 1 start bit, 8 data bits, no parity, 1 stop bit.

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;

entity UART is
    Port ( RXD : in  STD_LOGIC;
           TXD : out  STD_LOGIC;
           DIVIDE : in  STD_LOGIC_VECTOR (31 downto 0);
           TXDATA : in  STD_LOGIC_VECTOR (7 downto 0);
           RXDATA : out  STD_LOGIC_VECTOR (7 downto 0);
           RDA : out  STD_LOGIC;
           WR : in  STD_LOGIC;
           TBE : out  STD_LOGIC;
           CLK : in  STD_LOGIC);
end UART;

architecture RTL of UART is

type readState_type is (idle, start1, start2, data, stop);
type writeState_type is (idle, start, data, stop);
signal readState : readState_type := idle;
signal next_readState : readState_type;
signal writeState : writeState_type := idle;
signal next_writeState : writeState_type;
signal readCount : std_logic_vector(31 downto 0) := (others=>'0');
signal writeCount : std_logic_vector(31 downto 0) := (others=>'0');
signal writeTimer, readTimer : std_logic_vector(31 downto 0);
signal RXBUFFER : std_logic_vector(4 downto 0);
signal RXBUFFER4, RXBUFFER3, RXBUFFER2, RXBUFFER1, RXBUFFER0 : std_logic_vector(2 downto 0);
signal RXCOUNT : std_logic_vector(2 downto 0);
signal RXCLEAN : std_logic;
signal readLen, readLen_n : std_logic_vector(2 downto 0);
signal writeLen, writeLen_n : std_logic_vector(2 downto 0);
signal RXregister, RXregister_n : std_logic_vector(7 downto 0);
signal TXregister, TXregister_n : std_logic_vector(7 downto 0);

begin

	-- debounce RXDATA
	process
	begin
		wait until rising_edge(CLK);
			RXBUFFER <= RXBUFFER(3 downto 0) & RXD;
	end process;
	
	-- arbitrate RXDATA value (noise reduction)
	RXBUFFER4 <= "00" & RXBUFFER(4);
	RXBUFFER3 <= "00" & RXBUFFER(3);
	RXBUFFER2 <= "00" & RXBUFFER(2);
	RXBUFFER1 <= "00" & RXBUFFER(1);
	RXBUFFER0 <= "00" & RXBUFFER(0);

	RXCOUNT <= RXBUFFER4 + RXBUFFER3 + RXBUFFER2 + RXBUFFER1 + RXBUFFER0;
	
	RXCLEAN <= 	'0' when (RXCOUNT < 3)
						 else '1';

	-- synchronous read process
	process
	begin
		wait until rising_edge(CLK);
			readCount <= readCount + 1;
			if (readCount >= readTimer) then 
					readState <= next_readState;
					readCount <= (others=>'0');	
					readLen <= readLen_n;
					RXregister <= RXregister_n;
			end if;  
	end process;
	
	-- read signal updates
	with readState select
		readLen_n <=	readLen + 1 when data,						
							(others=>'0') when others;
							
	with readState select
		RXregister_n <= RXCLEAN & RXregister(7 downto 1) when data,
							 RXregister when others;
							 
	with readState select
		RDA <= 	'1' when stop,
					'0' when others;
					
	RXDATA <= RXregister;	
	
	-- asynchronous read process
	process (readState, RXBUFFER, DIVIDE, readLen)
   begin
      case (readState) is
		
			when start1 =>
				readTimer <= '0' & DIVIDE(31 downto 1);	-- delay until middle of the start bit	
				next_readState <= data;
			
			when start2 =>
				readTimer <= DIVIDE;
				next_readState <= data;
				
			when data =>
				readTimer <= DIVIDE;
				if readLen < 7 then
					next_readState <= data;
				else
					next_readState <= stop;
				end if;
			
			when stop =>
				readTimer <= DIVIDE;
				next_readState <= idle;
			
			when others =>											-- idle
				readTimer <= (others=>'0');
				if RXBUFFER = "00000" then				-- leading edge of start bit with RXDATA asserted low
					next_readState <= start1;
				else
					next_readState <= readState;
				end if;
				
		end case;
	end process;
	
	
	
	-- synchronous write process
	process
	begin
		wait until rising_edge(CLK);
			writeCount <= writeCount + 1;
			if (writeCount >= writeTimer) then 
					writeState <= next_writeState;
					writeCount <= (others=>'0');	
					writeLen <= writeLen_n;
					TXregister <= TXregister_n;
			end if;  
	end process;
	
	-- write signal updates
	with writeState select
		writeLen_n <=	writeLen + 1 when data,						
							(others=>'0') when others;
							
	with writeState select
		TXregister_n <= '0' & TXregister(7 downto 1) when data,
							 TXdata when others;
							 
	with writeState select
		TBE <= 	'1' when idle,
					'0' when others;
					
	with writeState select
		TXD <= '0' when start,
				 TXregister(0) when data,
				 '1' when others;
	
	-- asynchronous write process
	process (writeState, DIVIDE, writeLen, WR)
   begin
      case (writeState) is
		
			when start =>
				writeTimer <= DIVIDE;
				next_writeState <= data;
				
			when data =>
				writeTimer <= DIVIDE;
				if writeLen < 7 then
					next_writeState <= data;
				else
					next_writeState <= stop;
				end if;
				
			when stop =>
				writeTimer <= DIVIDE;				
				next_writeState <= idle;
		
			when others =>											-- idle
				writeTimer <= (others=>'0');
				if WR = '1' then
					next_writeState <= start;
				else
					next_writeState <= writeState;
				end if;
				
		end case;
	end process;

end RTL;

