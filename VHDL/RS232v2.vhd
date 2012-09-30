-- RS232 port
-- Andrew Read

-- Setup for 1 start bit, 8 data bits, no parity, 1 stop bit.

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity RS232v2 is
    Port ( RXD : in  STD_LOGIC;
           TXD : out  STD_LOGIC;
           UBRR : in  STD_LOGIC_VECTOR (15 downto 0);
           TXDATA : in  STD_LOGIC_VECTOR (7 downto 0);
           RXDATA : out  STD_LOGIC_VECTOR (7 downto 0);
           RDA : out  STD_LOGIC;
           WR : in  STD_LOGIC;
           TBE : out  STD_LOGIC;
           CLK : in  STD_LOGIC;
			  RST : in STD_LOGIC);
end RS232v2;

architecture RTL of RS232v2 is

type r_stateT is (idle, start, read_bit, read_wait, read_end);		-- Independant circuits for read (receive) and write (transmit)
type w_stateT is (write_idle, write_bit); 
type w_trigstateT is (idle, triggered);
type writeSR_modeT is (load, shift, hold);
type wbitcount_modeT is (zero, inc, hold);
type TXD_modeT is ( onn, off);

signal sixteenthcount, sixteenthcount_m : std_logic_vector (15 downto 0) := (others=>('0'));	-- Counter with time period (zero to top) of 1/16th of the baud rate
--signal sixteenth_clk : std_logic; --, bit_time : std_logic;
signal bitcount, bitcount_m : std_logic_vector (3 downto 0) := (others=>'0');	
signal bit_clk : std_logic;
signal r_state, r_state_n : r_stateT := idle;
signal w_state, w_state_n : w_stateT := write_idle;
signal w_trigstate : w_trigstateT := idle;
signal readSR, readSR_n : std_logic_vector (8 downto 0);			-- Receive shift register
signal rbitcount, rbitcount_n : std_logic_vector (3 downto 0):= (others=>'0');	-- Counter for number of bits received
signal rclkcount, rclktimer  : std_logic_vector (4 downto 0):= (others=>'0');	-- Increments each time baudcount rolls over, for use by read (receive) circuit
signal writeSR : std_logic_vector (8 downto 0);		-- Transmit shift register
signal writeSR_mode : writeSR_modeT;
signal wbitcount : std_logic_vector (3 downto 0):= (others=>'0');	-- Counter for number of bits transmitted
signal wbitcount_mode : wbitcount_modeT;
signal RXDATA_r, RXDATA_n : std_logic_vector ( 7 downto 0);		-- register for recieved data
signal TXD_mode : TXD_modeT;

begin	

	RXDATA <= RXDATA_r;								-- connect output port to register
	
-- RS232 clocks	
-- sixteenth rate clock
	process
	begin	
		wait until rising_edge(clk);
			if sixteenthcount = UBRR then				-- UBRR = CLOCK / (BAUD + 1) / 16
				sixteenthcount <= (others=>'0');
			else
				sixteenthcount <= sixteenthcount + 1;
			end if;
			sixteenthcount_m <= sixteenthcount;
	end process;
	
--	with sixteenthcount select							-- variable size counter must reference zero for clock signal
--		sixteenth_clk <= '1' when "0000000000000000",
--					   '0' when others;

-- bit rate clock
	process 																	
	begin					
		wait until rising_edge(clk);
		--wait until rising_edge(sixteenth_clk);		
		if sixteenthcount = "0000000000000000" and sixteenthcount_m /= "0000000000000000" then
			bitcount <= bitcount + 1;
			bitcount_m <= bitcount;
	   end if;
	end process;
	
--	bit_clk <= bitcount(3);								-- fixed 16 cycle clock can simply reference high byte of the counter for clock signal
																-- need signal assignment for "wait rising_edge(bit_clk);"
	
-- system clock domain FSM for write process
	-- simple behavioural FSM appropriate here
	process
	begin
		wait until rising_edge(clk);					
		case w_trigstate is
			when idle =>
				--TBE <= '1';
				if WR = '1' then							-- WR trigger needs to be in clk domain because signal is held high for only one cycle			
					w_trigstate <= triggered;
				end if;	
			when triggered =>
				--TBE <= '0';
				if wbitcount = "1100" then				-- FSM references a bit_clk domain signal to reset itself
					w_trigstate <= idle;
				end if;
		end case;
	end process;
	
-- bit clock domain FSM for write process
	-- FSM, state section 
   process
	begin					
		--wait until rising_edge(bit_clk);
		wait until rising_edge(clk);
		if sixteenthcount = "0000000000000001" and sixteenthcount_m = "0000000000000000"
			and bitcount(3) = '1' and bitcount_m(3) = '0' then
			if rst = '0' then									
				w_state <= w_state_n;	
			else				
				w_state <= write_idle;
			end if;
		end if;
	end process;
	
	-- FSM, control section
	process (w_state, w_trigstate, wbitcount)
	begin										
		case w_state is						
			when write_idle => 				
				TXD_mode <= off;	
				writeSR_mode <= load;
				wbitcount_mode <= zero;
				if w_trigstate = triggered then
					w_state_n <= write_bit;
				else
					w_state_n <= write_idle;
				end if;			
							
			when write_bit =>					
				TXD_mode <= onn;	
				writeSR_mode <= shift;
				wbitcount_mode <= inc;
				if wbitcount = "1011" then
					w_state_n <= write_idle;		
				else
					w_state_n <= write_bit;		
				end if;

		end case;

	end process;
	
	-- registers
	process
	begin
		wait until rising_edge(clk);
		if sixteenthcount = "0000000000000001" and sixteenthcount_m = "0000000000000000"
			and bitcount(3) = '1' and bitcount_m(3) ='0' then
			case writeSR_mode is
				when load =>
					writeSR <= TXDATA & '0';					-- start bit is '0' in RS232 protocol
				when shift =>
					writeSR <= '1' & writeSR(8 downto 1);  -- shift in '1' as stop bits
				when hold =>
					null;
			end case;
			
			case wbitcount_mode is
				when zero =>
					wbitcount <= (others=>'0');
				when inc =>
					wbitcount <= wbitcount + 1;
				when hold =>
					null;
			end case;
		end if;
	end process;
	
	-- combination logic
	with TXD_mode select
		TXD <= writeSR(0) when onn,
			    '1' when off;
				 
	TBE <= '1' when (w_trigstate = idle) and (w_state = write_idle) and (wbitcount = "0000") else '0';
	
-- read process		
-- FSM, state and register section
	process 																	
	begin					
		wait until rising_edge(clk);
		--wait until rising_edge(sixteenth_clk);		
		if sixteenthcount = "0000000000000000" and sixteenthcount_m /= "0000000000000000" then	
			rclkcount <= rclkcount + 1;
			RXDATA_r <= RXDATA_n;
			readSR <= readSR_n;
			rbitcount <= rbitcount_n;			
			if (rclkcount >= rclktimer) then 
				r_state <= r_state_n;
				rclkcount <= (others=>'0');	
			end if;
	   end if;
	end process;	
	
-- FSM, control section

	process (r_state, RXDATA_r, RXD, readSR, rbitcount)
	begin
		case r_state is						
			when idle =>
				RDA <= '0';
				readSR_n <= readSR;
				RXDATA_n <= RXDATA_r;
				rbitcount_n <= "0000";
				rclktimer <= "00000"; 				
				if RXD = '0' then			 						-- RS232 protocol start bit is a space						
					r_state_n <= start;
				else
					r_state_n <= idle;
				end if;
			
			when start =>
				RDA <= '0';
				readSR_n <= readSR;
				RXDATA_n <= RXDATA_r;
				rbitcount_n <= "0000";
				rclktimer <= "10101"; 							-- 21	time periods (+3) at 16x baud rate to get from the start of the start bit to the middle of the first data bit
				r_state_n <= read_bit;
				
			when read_bit =>
				RDA <= '0';
				readSR_n <= RXD & readSR(8 downto 1);
				rbitcount_n <= rbitcount + 1;
				rclktimer <= "00000";	
				if rbitcount = "1000" then				   	-- 8+1=9 bits including 1 stop bit
					RXDATA_n <= readSR(8 downto 1);
					r_state_n <= read_end;
				else
					RXDATA_n <= RXDATA_r;
					r_state_n <= read_wait;
				end if;

			when read_wait =>
				RDA <= '0';
				readSR_n <= readSR;
				RXDATA_n <= RXDATA_r;
				rbitcount_n <= rbitcount;
				rclktimer <= "01110"; 	
				r_state_n <= read_bit;

			when read_end =>
				RDA <= '1';	
				readSR_n <= readSR;
				RXDATA_n <= RXDATA_r;
				rbitcount_n <= "0000";
				rclktimer <= "00000";
				r_state_n <= idle;
			end case;	
	end process;
	
end RTL;	
	