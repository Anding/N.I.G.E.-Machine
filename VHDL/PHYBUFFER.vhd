library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity PHYBUFFER is
    Port ( CLK100MHZ : in  STD_LOGIC;
           PHYCRS : in  STD_LOGIC;								-- RMII PHY signals
           PHYRXERR : in  STD_LOGIC;
           PHYRXD : in  STD_LOGIC_VECTOR(1 downto 0);
           PHYCLK50MHZ : out  STD_LOGIC;
			  data : out STD_LOGIC_VECTOR(7 downto 0);		-- FIFO buffer data out port for captured Ethernet frames
			  ready : out STD_LOGIC;								-- flag: continue to read data until this flag goes low
			  read_enable : in STD_LOGIC;							-- assert this line to read a data byte from the FIFO
			  checksum_err : out STD_LOGIC;						-- flag: bad frame checksum (indicated at the end of capture)
			  Ethernet_IRQ : out STD_LOGIC						-- IRQ: asserts for one cycle when a new Ethernet frame in encountered
			  );
end PHYBUFFER;

architecture RTL of PHYBUFFER is

-- FIFO buffer MUST be configured with "first word fall through"
COMPONENT FIFO8
  PORT (
    clk : IN STD_LOGIC;
	 srst : IN STD_LOGIC;
    din : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
    wr_en : IN STD_LOGIC;
    rd_en : IN STD_LOGIC;
    dout : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
    full : OUT STD_LOGIC;
    empty : OUT STD_LOGIC
  );
END COMPONENT;

type state_ethernet_type is (preamble_0, preamble_1, payload_init, payload, waiting, bad_checksum, finish);
type state_FIFO_type is (waiting, loading, emptying, cleaning);
signal state_ethernet, state_ethernet_n : state_ethernet_type := waiting;
signal state_FIFO, state_FIFO_n : state_FIFO_type := waiting;
signal PHYCLKout : STD_LOGIC;
signal SR_long : STD_LOGIC_VECTOR(7 DOWNTO 0);
signal SR_long_count : STD_LOGIC_VECTOR(3 DOWNTO 0);
signal full8, empty8, RXDWE8, valid_flag, next_bit : STD_LOGIC;
signal reset_buffer, reset_CRC : STD_LOGIC;
signal checksum : STD_LOGIC_VECTOR(31 DOWNTO 0);

begin
  
INST_FIFO_8 : FIFO8
  PORT MAP (
    clk => CLK100MHZ,
    din => SR_long,
    wr_en => RXDWE8,
    rd_en => read_enable,
    dout => data,
    full => full8,
    empty => empty8,
	 srst => reset_buffer
  );
  
Inst_CRC32calc: entity work.CRC32calc 
	PORT MAP(
		clk => CLK100MHZ,
		reset => reset_CRC,
		data => next_bit,
		octets => open,
		checksum => checksum
	);
 
PROCESS

BEGIN
	wait until rising_edge(CLK100MHZ);
	
	-- generate a 50MHz clock for the RMII interface
	if PHYCLKout = '0' then			-- rising edge of 50MHz clock
		PHYCLKout <= '1';
		valid_flag <= PHYCRS;
	else
		PHYCLKout <= '0';
	end if;
	
	-- FSM state update
	state_ethernet <= state_ethernet_n;
	state_FIFO <= state_FIFO_n;
	
	-- 8 bit shift register to assemble the incoming bits as octets
	SR_long <= next_bit & SR_long(7 downto 1);				-- LSB first
	
	if (state_ethernet = payload_init or state_ethernet = payload or state_ethernet = finish) then
		if SR_long_count = "1000" then 
			SR_long_count <= "0001";				
		else
			SR_long_count <= SR_long_count + 1;
		end if;
	else	
		SR_long_count <= (others=>'0');	
	end if;
		
END PROCESS;

-- next state logic for Ethernet frame state machine
PROCESS (state_ethernet, valid_flag, next_bit, checksum)
begin
	case state_ethernet is
		when waiting =>											-- physical layer sends a high bit
			if valid_flag = '1' then 
				state_ethernet_n <= preamble_0;
			else
				state_ethernet_n <= waiting;
			end if;
			
		when preamble_0 =>										-- detect 1010 pattern in preamble
			if valid_flag = '1' and next_bit = '1' then
				state_ethernet_n <= preamble_1;
			else
				state_ethernet_n <= preamble_0;
			end if;
			
		when preamble_1 =>										-- 11 pattern ends start of frame delimiter
			if valid_flag = '1' and next_bit = '1' then
				state_ethernet_n <= payload_init;
			else
				state_ethernet_n <= preamble_0;
			end if;
			
		when payload_init =>										-- first cycle of payload
			state_ethernet_n <= payload;
			
		when finish =>
			state_ethernet_n <= waiting;
			
		when bad_checksum =>
			state_ethernet_n <= waiting;
	
		when others => -- payload
			if checksum = X"38FB2284" then					-- magic constant for sucessful checksum computation over message and valid checksum -> deem end-of-frame
				state_ethernet_n <= finish;					
			elsif valid_flag = '0' then						-- carrier sense goes low without valid checksum -> deem failure
				state_ethernet_n <= bad_checksum;
			else
				state_ethernet_n <= payload;
			end if;
			
	end case;
end process;

-- next state logic for FIFO buffer state machine
PROCESS (state_FIFO, state_ethernet, empty8)
begin
	case state_FIFO is
			
		when waiting =>
			if state_ethernet = payload_init then			-- new Ethernet frame encountered
				state_FIFO_n <= loading;
			else
				state_FIFO_n <= waiting;
			end if;
			
		when loading =>
			if state_ethernet = finish then					-- Ethernet frame completes with valid checksum
				state_FIFO_n <= emptying;
			elsif state_ethernet = bad_checksum then		-- Ethernet frame completes without valid checksum
				state_FIFO_n <= cleaning;
			else 
				state_FIFO_n <= loading;
			end if;
			
		when emptying =>
			if empty8 = '1' then 								-- CPU has emptied the FIFO buffer
				state_FIFO_n <= waiting;
			else
				state_FIFO_n <= emptying;
			end if;
			
		when others => -- cleaning
			if empty8 = '1' then 								-- CPU has emptied the FIFO buffer
				state_FIFO_n <= waiting;
			else
				state_FIFO_n <= cleaning;
			end if;
			
		end case;
end process;

-- capture the incoming RMII data at 2 bit/50MHz and convert to 1 bit/100MHz
with PHYCLKout select
	next_bit <= PHYRXD(0) when '0',
					PHYRXD(1) when others;
							
-- hold CRC reset until the payload begins
with state_ethernet select 
	reset_CRC <= '0' when payload,
					 '0' when payload_init,
					 '1' when others;
					 
-- write to FIFO buffer after each octet is assembled, provided the buffer is loading						
RXDWE8 <= '1' when (state_FIFO = loading and SR_long_count = "1000") else '0'; 

-- IRQ when a new frame is encountered, provided the buffer is available to load
Ethernet_IRQ <= '1' when (state_FIFO = waiting and state_ethernet = payload_init) else '0';
				 
-- 50 MHz RMII clock
PHYCLK50MHZ <= PHYCLKout;

-- FIFO buffer signals
with state_FIFO select
	ready <= '0' when waiting,							-- CPU can start to read the frame while it is still being captured
				'1' when others;

with state_FIFO select									-- THIS WILL NOT FUNCTION CORRECTLY... further work
	checksum_err <= '1' when cleaning,				
						 '0' when others;

reset_buffer <= '0';
							
end RTL;

