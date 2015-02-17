-- Interrupt scheduler
-- Andrew Read
-- Created 28 August 2011

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity Interrupt is
    Port ( clk : in  STD_LOGIC;
           rst : in  STD_LOGIC;
			  irq_mask : in STD_LOGIC_VECTOR(15 downto 1);		
           RS232_RDA_S0 : in  STD_LOGIC;
           RS232_TBE_S0 : in  STD_LOGIC;
           PS2_irq : in  STD_LOGIC;
			  ms_irq : in STD_LOGIC;
           rti : in  STD_LOGIC;										-- RTI from CPU
           irq : out  STD_LOGIC;										-- IRQ to CPU
			  irv : out STD_LOGIC_VECTOR(3 downto 0); 			-- Interrupt number 1 - 15
			  blocked : out STD_LOGIC );
end Interrupt;

architecture Behavioral of Interrupt is
	type state_T is (run, int, idle);
	signal state, state_n : state_T;
	signal ira_input, ira_input_m1, ira_register, ira_output, local_mask : std_logic_vector(7 downto 1);  -- extend width for interrupts 8 - 15
	signal irv_n, irv_register : std_logic_vector(3 downto 0);
	
begin
	-- interrupt assignment in priority order
	ira_input(1) <= irq_mask(1) and RS232_RDA_S0;
	ira_input(2) <= irq_mask(2) and RS232_TBE_S0;
	ira_input(3) <= irq_mask(3) and PS2_irq; 
	ira_input(4) <= irq_mask(4) and ms_irq; 	
	ira_input(5) <= irq_mask(5) and '0'; 							-- unused at present
	ira_input(6) <= irq_mask(6) and '0'; 
	ira_input(7) <= irq_mask(7) and '0';							
	-- extend here for interrupts 8 - 15

	ira_output <= (ira_input and not ira_input_m1) or ira_register;
	irv <= irv_n;
	with state select blocked <= '1' when int, '0' when others;

	process 
	begin
		wait until rising_edge(clk);
		if rst = '1' then
			state <= idle;
			ira_register <= (others=>'0');
			ira_input_m1 <= "0000010";									-- default TBE state is high
			irv_register <= (others=>'0');
		else
			state <= state_n;
			ira_register <= ira_output and local_mask;			
			ira_input_m1 <= ira_input;
			irv_register <= irv_n;
		end if;
	end process;
	
	process (state, ira_output, irv_register, rti)
	begin
		case state is
			when idle =>
				state_n <= run;
				irq <= '0';
				local_mask <= (others=>'1');
				irv_n <= x"0";
					
			when run =>
				if ira_output(1) = '1' then
					irv_n <= x"1";
					irq <= '1';
					local_mask <= not "0000001";
					state_n <= int;
				elsif ira_output(2) = '1' then
					irv_n <= x"2";
					irq <= '1';
					local_mask <= not "0000010";
					state_n <= int;
				elsif ira_output(3) = '1' then
					irv_n <= x"3";
					irq <= '1';
					local_mask <= not "0000100";
					state_n <= int;
				elsif ira_output(4) = '1' then
					irv_n <= x"4";
					irq <= '1';
					local_mask <= not "0001000";
					state_n <= int;
				elsif ira_output(5) = '1' then
					irv_n <= x"5";
					irq <= '1';
					local_mask <= not "0010000";
					state_n <= int;
				elsif ira_output(6) = '1' then
					irv_n <= x"6";
					irq <= '1';
					local_mask <= not "0100000";
					state_n <= int;
				elsif ira_output(7) = '1' then
					irv_n <= x"7";
					irq <= '1';
					local_mask <= not "1000000";
					state_n <= int;
				-- extend here for interrupts 8 - 15
				else
					irv_n <= x"0";
					irq <= '0';
					state_n <= run;
					local_mask <= not "0000000";
				end if;		
				local_mask <= not ira_output;			
					
			when int =>
				if rti = '1' then
					state_n <= run;
				else
					state_n <= int;
				end if;
				irq <= '0';
				local_mask <= (others=>'1');
				irv_n <= irv_register;
		end case;
	end process;
end Behavioral;

