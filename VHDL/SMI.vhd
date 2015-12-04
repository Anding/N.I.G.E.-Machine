library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity SMI is
    Port ( CLK100MHz : in STD_LOGIC;
			  addr : in STD_LOGIC_VECTOR (9 downto 0);
			  dataRead : out  STD_LOGIC_VECTOR (15 downto 0);
           dataWrite : in  STD_LOGIC_VECTOR (15 downto 0);
           read_request : in  STD_LOGIC;
           write_request : in  STD_LOGIC;
           ready : out  STD_LOGIC;
			  MDC : out STD_LOGIC;
			  MDIO : inout STD_LOGIC
			  );
end SMI;

architecture RTL of SMI is

-- state machine
type state_t is (waiting, preamble, init, address, turnAround, data);
signal state, state_n : state_t := waiting;
signal state_counter, state_timer : integer range 0 to 2047 := 0;	
type direction_t is (reading, writing);
signal direction : direction_t := reading;

-- clock divider for MDC
signal cycles : integer range 0 to 31;
constant cycles_top : integer := 20;
signal MDC_i : std_logic := '0';
signal update : std_logic;

-- Data I/O
signal SR_out, SR_out_next : std_logic_vector (31 downto 0);
signal SR_in : std_logic_vector (15 downto 0);

begin

MDC <= '0' when state = waiting else MDC_i;									-- 420ns MDC clock cycle time
MDIO <=  SR_out(31) when (state = preamble or state = init or state = address or (state = data and direction = writing)) else 'Z';

ready <= '1' when state = waiting else '0';
dataRead <= SR_in;

-- state machine update
process
begin
	wait until rising_edge(CLK100MHz);
	
	-- MDC timer
	if state /= waiting then
		if cycles = cycles_top then						
			cycles <= 0; 
			MDC_i <= not MDC_i;
		else
			cycles <= cycles + 1;
		end if;
	else
		cycles <= 0;
		MDC_i <= '0';
	end if;
	
	-- state machine and shift register update
	if state_counter >= state_timer then					-- state transitions on MDC falling edge
		state_counter <= 0;
		state <= state_n;
		SR_out <= SR_out_next;
	else
		state_counter <= state_counter + 1;					-- must update each 100MHz tickS
		if (cycles = cycles_top and MDC_i = '1') then
			SR_out <= SR_out(30 downto 0) & '0';
			if state = data then 
				SR_in <= SR_in(14 downto 0) & MDIO;
			end if;
		end if;
	end if;
	
	-- recursive state machine logic
	if state = waiting then
		if write_request = '1' then
			direction <= writing;
		else
			direction <= reading;
		end if;
	end if;

end process;

-- next state logic
process (state, read_request, write_request, direction)
begin
	case state is
		when waiting =>
		if read_request = '1' or write_request = '1' then
			state_n <= preamble;
		else
			state_n <= waiting;
		end if;
		state_timer <= 0;
		SR_out_next <= (others =>'1');
		
		when preamble =>
		state_n <= init;
		state_timer <= 1343;					-- 42 cycles * 32 bits - 1/2 cycle
		if direction = reading then 
			SR_out_next <= "01100000000000000000000000000000";
		else
			SR_out_next <= "01010000000000000000000000000000";
		end if;
		
		when init =>
		state_n <= address;
		state_timer <= 168;					-- 42 cycles * 4 bits
		SR_out_next <= addr & "0000000000000000000000";
		
		when address =>						-- 42 cycles * 10 bits
		state_n <= turnaround;
		state_timer <= 420;
		SR_out_next <= (others =>'0');
		
		when turnaround =>					-- 42 cycles * 2 bits
		state_n <= data;
		state_timer <= 84;
		SR_out_next <= dataWrite & "0000000000000000";
		
		when others =>					-- data
		state_n <= waiting;
		state_timer <= 672;					-- 42 cycles * 16 bits
		SR_out_next <= (others => '0');
			
	end case;
end process;




	

end RTL;

