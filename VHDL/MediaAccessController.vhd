-- Andrew Read
-- October 2015

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity MediaAccessController is
    Port ( CLK100MHZ : in  STD_LOGIC;
			  CLK50MHZ : in STD_LOGIC;
			  reset : in STD_LOGIC;
			  -- RMII PHY signals
           PHYCRS : in  STD_LOGIC;								
           PHYRXERR : in  STD_LOGIC;
           PHYRXD : in  STD_LOGIC_VECTOR(1 downto 0);
           PHYCLK50MHZ : out  STD_LOGIC;
           PHYRSTN : out  STD_LOGIC;           
			  PHYTXEN : out  STD_LOGIC;
           PHYTXD : out  STD_LOGIC_VECTOR (1 downto 0);
           PHYINTN : in  STD_LOGIC;
			  -- FIFO buffer for received frames
			  dataRX : out STD_LOGIC_VECTOR(7 downto 0);		-- FIFO buffer data out port for captured Ethernet frames
			  readyRX : out STD_LOGIC;								-- flag: continue to read data until this flag goes low
			  read_enable : in STD_LOGIC;							-- assert this line to read a data byte from the RX_FIFO
			  Ethernet_IRQ : out STD_LOGIC;						-- IRQ: asserts for one cycle when a new Ethernet frame in encountered		  
			  checksum_err : out STD_LOGIC;						-- flag: bad frame checksum (indicated at the end of capture)
			  -- FIFO buffer for frames for transmission
			  dataTX : in STD_LOGIC_VECTOR(7 downto 0);		-- FIFO buffer data in port for Ethernet frames due for transmission
			  weTX : in STD_LOGIC;		  							-- assert this line to write a byte to the TX_FIFO
			  readyTX : OUT STD_LOGIC;								-- flag: when high indicates the unit is available to accept a new Ethernet frame for transmission
			  transmit_request : in STD_LOGIC					-- assert this line to indicate that the Ethernet frame is now complete in the buffer and request transmission
			  );
end MediaAccessController;

architecture RTL of MediaAccessController is

-- FIFO buffer MUST be configured with "first word fall through" option set
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

-- registered I/O     
	-- combinatorial signals that update registered outputs
signal PHYTXEN_i : STD_LOGIC;
signal PHYTXD_i : STD_LOGIC_VECTOR (1 downto 0);
	-- registers that are updated from combinatorial inputs
signal PHYCRS_i : STD_LOGIC;
signal PHYRXD_i : STD_LOGIC_VECTOR (1 downto 0);

-- state machines
type state_ethernet_RX_type is (preamble_0, preamble_1, payload_init, payload, waiting, bad_checksum, finish);
type state_ethernet_TX_type is (waiting, preamble, start_of_frame, payload, pad, fcs, gap);
type state_FIFO_RX_type is (waiting, loading, emptying);
signal state_ethernet_RX, state_ethernet_RX_n : state_ethernet_RX_type := waiting;
signal state_ethernet_TX, state_ethernet_TX_n : state_ethernet_TX_type := waiting;
signal state_FIFO_RX, state_FIFO_RX_n : state_FIFO_RX_type := waiting;
signal state_ethernet_TX_timer, state_ethernet_TX_count : INTEGER RANGE 0 TO 95;
signal nibbleCount : INTEGER RANGE 0 TO 255;

-- enumerated multiplexer for transmission data
type output_multiplexer_type is (shift_register, checksum_direct, checksum_registered);
signal output_multiplexer : output_multiplexer_type := shift_register;

-- PHY and derived signals
signal PHYCLKout, valid_flag: STD_LOGIC;

-- receive and transmit shift registers
signal SR_RX : STD_LOGIC_VECTOR(7 DOWNTO 0);
signal SR_RX_count : STD_LOGIC_VECTOR(3 DOWNTO 0);
signal SR_TX : STD_LOGIC_VECTOR(7 DOWNTO 0);
signal SR_TX_count : STD_LOGIC_VECTOR(4 DOWNTO 0);

-- receive and transmit checksums
signal reset_CRC_RX, reset_CRC_TX : STD_LOGIC;
signal next_bit_RX, next_bit_TX : STD_LOGIC;
signal checksum_RX, checksum_TX, checksum_TX_r : STD_LOGIC_VECTOR(31 DOWNTO 0);
constant magicChecksum : STD_LOGIC_VECTOR(31 DOWNTO 0) := X"38FB2284";				-- magic constant indicates successful checksum re-computation over message plus valid checksum

-- FIFO buffers and related
signal fullRX, emptyRX, emptyTX, fullTX, weRX : STD_LOGIC;
signal next_byte, dataTX_out : STD_LOGIC_VECTOR(7 DOWNTO 0);
signal read_enableTX: STD_LOGIC;
signal transmit_request_m : STD_LOGIC;

begin
  
INST_FIFO_RX : FIFO8
  PORT MAP (
    clk => CLK100MHZ,
    din => SR_RX,
    wr_en => weRX,
    rd_en => read_enable,
    dout => dataRX,
    full => fullRX,
    empty => emptyRX,
	 srst => reset
	 );
  
INST_FIFO_TX : FIFO8
  PORT MAP (
    clk => CLK100MHZ,
    din => dataTX,
    wr_en => weTX,
    rd_en => read_enableTX,
    full => fullTX,
    dout => dataTX_out,
    empty => emptyTX,
	 srst => reset
  );
  
Inst_CRC32calc_RX: entity work.CRC32calc 
	PORT MAP(
		clk => CLK100MHZ,
		reset => reset_CRC_RX,
		data => next_bit_RX,
		checksum => checksum_RX
	);
	
Inst_CRC32calc_TX: entity work.CRC32calc 
	PORT MAP(
		clk => CLK100MHZ,
		reset => reset_CRC_TX,
		data => next_bit_TX,
		checksum => checksum_TX
	);
 
-- sequential logic
-------------------

--PHYCLKout <= CLK50MHZ;

--PROCESS
--BEGIN
--	wait until rising_edge(CLK50MHZ);
	--	 register inputs
		PHYCRS_i <= PHYCRS;
		PHYRXD_i <= PHYRXD;

--END PROCESS;


PROCESS
BEGIN
	wait until rising_edge(CLK100MHZ);
	
	-- generate a 50MHz clock for the RMII interface
	if PHYCLKout = '0' then			-- rising edge of 50MHz clock
		PHYCLKout <= '1';
		valid_flag <= PHYCRS_i;		-- synchronize trigger with update of shift register		
	
	else
		PHYCLKout <= '0';
		-- register outputs in falling edge (these signals will be read by the PHY chip on the rising edge)
		PHYTXEN <= PHYTXEN_i;
		PHYTXD <= PHYTXD_i;		
	end if;
	 
	-- Receiver logic
	-----------------
	
	-- FSM state update	
	state_ethernet_RX <= state_ethernet_RX_n;
	state_FIFO_RX <= state_FIFO_RX_n;
	
	-- 8 bit shift register to assemble the incoming bits as octets
	SR_RX <= next_bit_RX & SR_RX(7 downto 1);				-- shift in LSB first
	
	-- reset the shift register with each new frame recieved and after each byte
	if (state_ethernet_RX = payload_init or state_ethernet_RX = payload or state_ethernet_RX = finish) then
		if SR_RX_count = "1000" then 
			SR_RX_count <= "0001";						-- reset to 1 since SR_RX_count equals the actual number of valid bits in the register (1 to 8)
		else
			SR_RX_count <= SR_RX_count + 1;
		end if;
	else	
		SR_RX_count <= (others=>'0');	
	end if;
	
	-- recursive state machine design - these registers that need to be updated at stages during the reception of the ethernet frame
	-- flag bad checksums as necessary
	if state_ethernet_RX = preamble_0 then 
			checksum_err <= '0';
	elsif state_ethernet_RX = bad_checksum then
			checksum_err <= '1';
	end if;
	
	-- Transmitter logic
	--------------------
	
	-- FSM state update
	if state_ethernet_TX_count >= state_ethernet_TX_timer then				-- timed state machine transition format
		state_ethernet_TX <= state_ethernet_TX_n;
		state_ethernet_TX_count <= 0;
	else
		state_ethernet_TX_count <= state_ethernet_TX_count + 1; 
	end if;	
	
	-- 8 bit shift register to dispatch queued bytes over 2-bit RMII port
	if PHYCLKout = '0' then																-- PHYCLKout = '0', synchronize with rising edge of 50MHz PHYCLK													
		if SR_TX_count = 0 then
			SR_TX <= next_byte;
		else
			SR_TX <= "00" & SR_TX(7 downto 2);										-- shift out the LSB first, in 2-bit nibbles
		end if;
	end if;
	
	-- reset the shift register at the start of each new frame and after each byte transmitted
	if PHYCLKout = '0' then	
		if (state_ethernet_TX = waiting) or 
		(SR_TX_count = 3) then --and ((state_ethernet_TX = preamble) or (state_ethernet_TX = start_of_frame) or (state_ethernet_TX = payload))) then  
										-- these states transmit byte-by-byte but FCS does not, hence need to confirm the state
			SR_TX_count <= (others => '0');
		else
			SR_TX_count <= SR_TX_count + 1;
		end if;
	end if;
	
	-- recursive state machine design - these registers that need to be updated at stages during the transmisson of the ethernet frame
	
	-- drive PHY transmission enable high at the start of the transmission preamble and low when complete
	--		use registers rather than combinatorial logic for a one cycle gap to coincide with the shift register
	if state_ethernet_TX = preamble then
		PHYTXEN_i <= '1';
	elsif (state_ethernet_TX = gap) or (state_ethernet_TX = waiting) then
		PHYTXEN_i <= '0';		
	end if;
	
	-- reset the transmission checksum prior to each new each frame
	if state_ethernet_TX = payload then	
		reset_CRC_TX <= '0';
	elsif state_ethernet_TX = waiting then
		reset_CRC_TX <= '1';
	end if;
	
	-- register the checksum on the cycle when it is prepared, subsequently shift out in 2-bit nibbles
	if state_ethernet_TX = fcs and state_ethernet_TX_count = 1 then
		checksum_TX_r <= checksum_TX;
	elsif PHYCLKout = '0' then
		checksum_TX_r <= checksum_TX_r (29 downto 0) & "00";
	end if;	
	
	-- set the output multiplexer according to the stage of the transmission
	if state_ethernet_TX = fcs and state_ethernet_TX_count = 0 then
		output_multiplexer <= checksum_direct;
	elsif state_ethernet_TX = fcs and state_ethernet_TX_count= 1 then
		output_multiplexer <= checksum_registered;
	elsif state_ethernet_TX = gap then
		output_multiplexer <= shift_register;
	end if;
		
	-- make transmit request a signal for two 100MHz cycles so that it can be synchronize with the 50MHz PHYCLK
	transmit_request_m <= transmit_request;		

	-- counts the bits/bytes of the payload ready to pad as needed to 60 bytes (full frame 64 bytes)
	if state_ethernet_TX = start_of_frame then
		nibbleCount <= 0;
	elsif (state_ethernet_TX = payload or state_ethernet_TX = pad) and PHYCLKout = '0' and nibbleCount < 240 then
		nibbleCount <= nibbleCount + 1;										-- 2 bits each 50MHz cycle, need to count only to a maximum of 240 nibbles = 60 bytes
	end if;
		
END PROCESS;

-- next state logic for TX Ethernet frame state machine
PROCESS (state_ethernet_TX, emptyTX, transmit_request, transmit_request_m, PHYCLKout, nibbleCount)
begin
	case state_ethernet_TX is
		when waiting =>													-- synchronize TX initialization with falling edge of the PHY 50 MHz clock
																				--  once 100MHx cycle is used to load the shift regsiter and is ready to be read on the subsequent 50MHz rising edge
			if PHYCLKout = '1' and (transmit_request = '1' or transmit_request_m = '1') then  -- transmit_request is 10ns long so need to OR with a one cycle lagging clone to sample at 20ns
				state_ethernet_TX_n  <= preamble;
			else
				state_ethernet_TX_n  <= waiting;
			end if;
			state_ethernet_TX_timer <= 0;								-- transition immediately that the signal is received
		
		when preamble =>
			state_ethernet_TX_n <= start_of_frame;
			state_ethernet_TX_timer <= 55;							-- transition after 7 bytes ( 7 bytes * (8 bits per byte / 2 bits per nibble) * (100MHz/50Mhz) - 1 = 55 ) 
																				--  minus 1 since counter begins at zero
		when start_of_frame =>
			state_ethernet_TX_n <= payload;
			state_ethernet_TX_timer <= 7;								-- transition after one byte
			
		when payload =>
			if emptyTX = '0' then 										-- continue transmitting payload until FIFO buffer is empty
				state_ethernet_TX_n <= payload;
			elsif nibbleCount = 240 then
				state_ethernet_TX_n <= fcs;							-- 60 bytes or more already transmitted
			else
				state_ethernet_TX_n <= pad;
			end if;
			state_ethernet_TX_timer <= 7;								-- transition after an integer multiple of bytes (i.e. make sure last byte is fully transmitted before transition)
			
		when pad =>
			if nibbleCount = 240 then
				state_ethernet_TX_n <= fcs;
			else
				state_ethernet_TX_n <= pad;
			end if;		
			state_ethernet_TX_timer <= 7;					
			
		when fcs =>														
			state_ethernet_TX_n <= gap;
			state_ethernet_TX_timer <= 31;							-- transition after 32 bits		
			
		when gap =>															
			state_ethernet_TX_n <= waiting;
			state_ethernet_TX_timer <= 95;							-- transition after 12 bytes
			
		end case;
end process;

-- next state logic for RX Ethernet frame state machine
PROCESS (state_ethernet_RX, valid_flag, next_bit_RX, checksum_RX)
begin
	case state_ethernet_RX is
		when waiting =>											-- physical layer sends a high bit
			if valid_flag = '1' then 
				-- debug
				--state_ethernet_RX_n <= preamble_0;
				state_ethernet_RX_n <= payload_init;
			else
				state_ethernet_RX_n <= waiting;
			end if;
			
		when preamble_0 =>										-- detect 1010 pattern in preamble
			if valid_flag = '1' and next_bit_RX = '1' then
				state_ethernet_RX_n <= preamble_1;
			else
				state_ethernet_RX_n <= preamble_0;
			end if;
			
		when preamble_1 =>										-- 11 pattern ends start of frame delimiter
			if valid_flag = '1' and next_bit_RX = '1' then
				state_ethernet_RX_n <= payload_init;
			else
				state_ethernet_RX_n <= preamble_0;
			end if;
			
		when payload_init =>										-- first cycle of payload, state used to trigger a one cycle IRQ
			state_ethernet_RX_n <= payload;
			
		when finish =>												-- first cycle after payload, state used to trigger a transition of the RX_FIFO buffer
			state_ethernet_RX_n <= waiting;
			
		when bad_checksum =>										-- first cycle after payload, state used to trigger a transition of the RX_FIFO buffer
			state_ethernet_RX_n <= waiting;
	
		when others => 											-- payload
			if checksum_RX = magicChecksum then				-- magic checksum reached -> deem success	
				state_ethernet_RX_n <= finish;					
			elsif valid_flag = '0' then						-- carrier sense goes low without valid checksum -> deem failure
				state_ethernet_RX_n <= bad_checksum;
			else
				state_ethernet_RX_n <= payload;
			end if;
			
	end case;
end process;

-- next state logic for RX FIFO buffer state machine
--		new incoming frames are dropped unless the FIFO buffer has been emptied already - simplifies further handling in software
PROCESS (state_FIFO_RX, state_ethernet_RX, emptyRX)
begin
	case state_FIFO_RX is
			
		when waiting =>												-- new Ethernet frame encountered and the buffer is empty, start to load the buffer
			if state_ethernet_RX = payload_init then			
				state_FIFO_RX_n <= loading;
			else
				state_FIFO_RX_n <= waiting;
			end if;
			
		when loading =>
			if state_ethernet_RX = finish or state_ethernet_RX = bad_checksum then	-- Ethernet frame completes
				state_FIFO_RX_n <= emptying;
			else 
				state_FIFO_RX_n <= loading;
			end if;
			
		when others =>  												-- emptying
			if emptyRX = '1' then 									-- CPU has emptied the FIFO buffer
				state_FIFO_RX_n <= waiting;
			else
				state_FIFO_RX_n <= emptying;
			end if;
			
		end case;
end process;

-- Receive combinatorial logic
------------------------------

-- capture the incoming RMII data at 2 bit/50MHz and convert to 1 bit/100MHz
with PHYCLKout select
	next_bit_RX <= PHYRXD_i(0) when '0',
					PHYRXD_i(1) when others;
							
-- hold RX checksum reset until the payload begins
with state_ethernet_RX select 
	reset_CRC_RX <= '0' when payload,
					 '0' when payload_init,
					 '1' when others;
					 
-- write to FIFO buffer after each octet is assembled, provided the buffer is loading						
weRX <= '1' when (state_FIFO_RX = loading and SR_RX_count = "1000") else '0'; 

-- IRQ when a new frame is encountered, provided the buffer is available to load
Ethernet_IRQ <= '1' when (state_FIFO_RX = waiting and state_ethernet_RX = payload_init) else '0';
				 
-- 50 MHz RMII clock
PHYCLK50MHZ <= PHYCLKout;

-- RX FIFO buffer signals
with state_FIFO_RX select
	readyRX <= '0' when waiting,							-- CPU can start to read the frame while it is still being captured
				'1' when others;

-- Transmit combinatorial logic
-------------------------------

-- TX FIFO buffer signals
readyTX <= emptyTX;

-- bits for checksum calculation
with PHYCLKout select
	next_bit_TX <= SR_TX(0) when '1',
						SR_TX(1) when others;

-- select transmission byte according to Ethernet frame stage				  
with state_ethernet_TX select
	next_byte <= "01010101" when preamble,
					 "11010101" when start_of_frame,			-- remember, this is transmitted LSB first
					 dataTX_out when payload,						-- from FIFO
					 "00000000" when others;					-- including pad

-- trigger FIFO to advance internal pointer each time a byte is read				  
read_enableTX <= '1' when (PHYCLKout = '0' and state_ethernet_TX = payload and SR_TX_count = 0) else '0';
				 
-- connect low bits of shift register to PHY data lanes using a multiplexer
with output_multiplexer select
PHYTXD_i(0) <= checksum_TX(30) when checksum_direct,				-- first bits of checksum needs as soon as they are calulcated, no time to register
				 checksum_TX_r(30) when checksum_registered,		
				 SR_TX(1) when others;									-- shift register for payload and pad
				 
with output_multiplexer select
PHYTXD_i(1) <= checksum_TX(31) when checksum_direct,
				 checksum_TX_r(31) when checksum_registered,
				 SR_TX(0) when others;
					
-- other PHY signals
--------------------

-- PHY reset low
PHYRSTN <= not reset;
			
end RTL;

