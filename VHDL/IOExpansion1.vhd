library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity IOExpansionSYNC is

    Port (
		reset : in std_logic;
		clk	: in std_logic;
-- Epp-like bus signals
      EppAstb: in std_logic;        -- Address strobe
      EppDstb: in std_logic;        -- Data strobe
      EppWr  : in std_logic;        -- Port write signal
      EppDB  : inout std_logic_vector(7 downto 0); -- port data bus
      EppWait: out std_logic;        -- Port wait signal
-- user extended signals 
      data  : out std_logic_vector(31 downto 0); 
      addr  : out std_logic_vector(15 downto 0);  
      we		: out std_logic_vector(0 downto 0)
         );

end IOExpansionSYNC;

architecture Behavioral of IOExpansionSYNC is
type state_T is (idle, data_read, data_write, data_write_complete, addr_read, addr_write);
signal state, state_n : state_T := idle;
signal busEppInternal: std_logic_vector (7 downto 0);
signal regEppAdr, regEppAdr_n: std_logic_vector (7 downto 0); -- Epp address 
signal addr_i: std_logic_vector(15 downto 0);
signal regVer: std_logic_vector(7 downto 0); --  0x00    I/O returns the complement of written value
signal reset_m : std_logic;
signal data_long_r : std_logic_vector(31 downto 0);
 
begin

	 addr <= addr_i;
	 --data <= EppDB;
	 data <= data_long_r;
	 --we <= "1" when (state = data_write and regEppAdr = X"FF") else "0";
	 we <= "1" when (state = data_write and regEppAdr = X"FF" and addr_i(1 downto 0) = "11") else "0";
	 
	 process
	 begin
		wait until rising_edge(clk);
		if state = data_write then
			data_long_r <= data_long_r(23 downto 0) & EppDB;
		end if;
	 end process;

	
-- output to Epp interface (before Z buffer)
    busEppInternal <= 
       regVer when (regEppAdr = x"00") else
       addr_i(15 downto 8) when (regEppAdr = x"FD") else
       addr_i(7 downto 0) when (regEppAdr = x"FE") else
		 (others=>'0');

 -- input from Epp interface, infer flipflops
   process 
    begin
	 wait until rising_edge(clk);
	 reset_m <= reset;
	 
	 if reset = '0' and reset_m = '1' then
			addr_i <= (others=>'0');
			
	 elsif state = data_write then 
	 
			case regEppAdr is
				when X"00" => 
					regVer <= not EppDB;    
					
				when X"FD" =>
					addr_i <= EppDB & "00000000";	
					
				when X"FE" =>
					addr_i <= addr_i(15 downto 8) & EppDB;
					
				when X"FF" =>
					addr_i <= addr_i + 1;
					
				when others =>
					null;
			
			end case;
			
	 end if;
    end process;

-- state machine logic
	process
	begin
		wait until rising_edge(clk);
		state <= state_n;
		regEppAdr <= regEppAdr_n;
	end process;
	
	process (state, EppAstb, EppDstb, regEppAdr, EppDB, EppWr, busEppInternal)
		begin
		case state is
		
		when idle =>
			if EppAstb = '0' then
				if EppWr = '0' then 
					state_n <= addr_write;
					EppDB <= (others=>'Z');
				else
					state_n <= addr_read;
					EppDB <= regEppAdr;
				end if;
			elsif EppDstb = '0' then
				if EppWr = '0' then 
					state_n <= data_write;
					EppDB <= (others=>'Z');
				else
					state_n <= data_read;
					EppDB <= busEppInternal;
				end if;
			else
				state_n <= idle;
				EppDB <= (others=>'Z');
			end if;
			EppWait <= '0';
			regEppAdr_n <= regEppAdr;
		
		when addr_write =>
			if EppAstb = '1' then
				state_n <= idle;	
			else
				state_n <= addr_write;
			end if;
			EppWait <= '1';
			regEppAdr_n <= EppDB;
			EppDB <= (others=>'Z');
	
		when addr_read =>
			if EppAstb = '1' then
				state_n <= idle;
			else
				state_n <= addr_read;
			end if;	
			EppWait <= '1';
			regEppAdr_n <= regEppAdr;
			EppDB <= regEppAdr;
	
		when data_write =>
			state_n <= data_write_complete;
			EppWait <= '1';
			regEppAdr_n <= regEppAdr;
			EppDB <= (others=>'Z');
			
		when data_read =>
			if EppDstb = '1' then
				state_n <= idle;
			else
				state_n <= data_read;
			end if;
			EppWait <= '1';
			regEppAdr_n <= regEppAdr;
			EppDB <= busEppInternal;
			
		when data_write_complete =>
			if EppDstb = '1' then
				state_n <= idle;
			else
				state_n <= data_write_complete;
			end if;
			EppWait <= '1';
			regEppAdr_n <= regEppAdr;
			EppDB <= (others=>'Z');		

		end case;
	end process;		

end Behavioral;
