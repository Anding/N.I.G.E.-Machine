library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity stack_access is
Port ( clk : in  STD_LOGIC;
		  rst : in  STD_LOGIC;
		  -- connections to subroutine and exeception stacks 
			SSdatain : in STD_LOGIC_VECTOR (255 downto 0);	
			SSdataout : out STD_LOGIC_VECTOR (255 downto 0);
			SSw : out STD_LOGIC_VECTOR (31 downto 0);
			SSwSignal : in STD_LOGIC;											-- write by datapath signalled here
			ESdatain : in STD_LOGIC_VECTOR (191 downto 0);	
			ESdataout : out STD_LOGIC_VECTOR (191 downto 0);
			ESw : out STD_LOGIC_VECTOR (23 downto 0);
			ESwSignal :	in STD_LOGIC;											-- write by datapath signalled here
		  -- CPU system memory channel
		  en : in STD_LOGIC;												
		  addr : in  STD_LOGIC_VECTOR (10 downto 0);				
		  datain : in  STD_LOGIC_VECTOR (31 downto 0);			
		  dataout : out  STD_LOGIC_VECTOR (31 downto 0);
		  wrq : in  STD_LOGIC_VECTOR (0 downto 0)
		  );
end stack_access;

architecture RTL of stack_access is
signal addr_i : std_logic_vector(7 downto 0);					-- local address bus
signal dataout_i : STD_LOGIC_VECTOR (31 downto 0);
signal ESdatain_m : STD_LOGIC_VECTOR (191 downto 0);
signal ESwSignal_m, SSwSignal_m : std_logic;
signal datain_m : STD_LOGIC_VECTOR (31 downto 0);	

begin

	-- Read logic
	addr_i <= addr(7 downto 0);
	dataout <= dataout_i;
			 
	process		
	begin																
		wait until rising_edge(clk);		
		if en = '1'  then
			case addr_i is
				when x"00" =>										
					dataout_i <= SSdatain(31 downto 0);
				when x"04" =>										
					dataout_i <= SSdatain(63 downto 32);
				when x"08" =>										
					dataout_i <= SSdatain(95 downto 64);
				when x"0C" =>										
					dataout_i <= SSdatain(127 downto 96);					
				when x"10" =>										
					dataout_i <= SSdatain(159 downto 128);
				when x"14" =>										
					dataout_i <= SSdatain(191 downto 160);					
				when x"18" =>										
					dataout_i <= SSdatain(223 downto 192);
				when x"1C" =>										
					dataout_i <= SSdatain(255 downto 224);
--				when x"20" =>										
--					dataout_i <= SSdatain(287 downto 256);
--				when x"24" =>										
--					dataout_i <= SSdatain(319 downto 288);
--				when x"28" =>										
--					dataout_i <= SSdatain(351 downto 320);
--				when x"2C" =>										
--					dataout_i <= SSdatain(383 downto 352);
--				when x"30" =>										
--					dataout_i <= SSdatain(415 downto 384);		
--				when x"34" =>										
--					dataout_i <= SSdatain(447 downto 416);
--				when x"38" =>										
--					dataout_i <= SSdatain(479 downto 448);
--				when x"3C" =>										
--					dataout_i <= SSdatain(511 downto 480);
				when x"80" =>										
					dataout_i <= ESdatain(31 downto 0);
				when x"84" =>										
					dataout_i <= ESdatain(63 downto 32);
				when x"88" =>										
					dataout_i <= ESdatain(95 downto 64);
				when x"8C" =>										
					dataout_i <= ESdatain(127 downto 96);					
				when x"90" =>										
					dataout_i <= ESdatain(159 downto 128);
				when x"94" =>										
					dataout_i <= ESdatain(191 downto 160);					
--				when x"98" =>										
--					dataout_i <= ESdatain(223 downto 192);
--				when x"9C" =>										
--					dataout_i <= ESdatain(255 downto 224);		
				when others =>
					dataout_i <= (others=>'0');
			end case;
		end if;
	end process;
	
	-- Write logic
	process
	begin
		wait until rising_edge(clk);
		ESdatain_m <= ESdatain;
		SSwSignal_m <= SSwSignal;
		ESwSignal_m <= ESwSignal;
		datain_m <= datain;
	end process;	
	
	with SSwSignal_m select 
		SSdataout <= 	datain_m & datain_m &
							datain_m & datain_m & datain_m & datain_m & datain_m & datain_m when '0',
							(others=>'0') when others;		-- zero local variables when subroutine stack advances
							
	with ESwSignal_m select 				 
		ESdataout <= 	datain_m & datain_m & datain_m & datain_m & datain_m & datain_m when '0',
							ESdatain_m when others;			-- "copy down" environment variables when exception stack advances

	process	--(SSwSignal, ESwSignal, en, wrq, addr_i)
	begin																
		wait until rising_edge(clk);
		SSw <= (others=>'0');
		ESw <= (others=>'0');
		
		if SSwSignal = '1' then								-- write when subroutine stack advances
			SSw <= (others=>'1');
		end if	;	
		
		if ESwSignal = '1' then								-- write when exception stack advances
			ESw <= (others=>'1');
		end if;
	
		if en = '1' and wrq = "1" then
			case addr_i is
				when x"00" =>	
					SSw <= x"0000000F";
				when x"04" =>	
					SSw <= x"000000F0";
				when x"08" =>	
					SSw <= x"00000F00";
				when x"0C" =>	
					SSw <= x"0000F000";
				when x"10" =>	
					SSw <= x"000F0000";
				when x"14" =>	
					SSw <= x"00F00000";
				when x"18" =>	
					SSw <= x"0F000000";
				when x"1C" =>	
					SSw <= x"F0000000";	
--				when x"20" =>	
--					SSw <= x"0F00000000";
--				when x"24" =>	
--					SSw <= x"F000000000";
--				when x"28" =>	
--					SSw <= x"00000F0000000000";
--				when x"2C" =>	
--					SSw <= x"0000F00000000000";
--				when x"30" =>	
--					SSw <= x"000F000000000000";
--				when x"34" =>	
--					SSw <= x"00F0000000000000";
--				when x"38" =>	
--					SSw <= x"0F00000000000000";
--				when x"3C" =>	
--					SSw <= x"F000000000000000";	
				when x"80" =>	
					ESw <= x"00000F";
				when x"84" =>	
					ESw <= x"0000F0";
				when x"88" =>	
					ESw <= x"000F00";
				when x"8C" =>	
					ESw <= x"00F000";
				when x"90" =>	
					ESw <= x"0F0000";
				when x"94" =>	
					ESw <= x"F00000";
--				when x"98" =>	
--					ESw <= x"0F000000";
--				when x"9C" =>	
--					ESw <= x"F0000000";	
				when others =>
			end case;
		end if;
	end process;
		

end RTL;

