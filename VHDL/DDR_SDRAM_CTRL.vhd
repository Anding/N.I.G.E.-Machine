-- DDR2 memory interface
-- Andrew Read, March 2016
-- This project is based on a working DDR2 interface very kindly donated by a friend

LIBRARY IEEE;
USE IEEE.std_logic_1164.all;
USE IEEE.std_logic_arith.all;
USE IEEE.std_logic_textio.all;

entity DDR_SDRAM_CTRL is 
port (
	CLK   : in  std_logic;								-- 125MHz clock
	CLK_130 : in std_logic;								-- 125MHz clock 130 degree phase shift 
    reset : in  std_logic;
        
	wrrd_ba_add : in std_logic_vector(2 downto 0);		-- bank address	
	wrrd_ras_add : in std_logic_vector(12 downto 0);  	-- row address
	wrrd_cas_add : in std_logic_vector(8 downto 0);  	-- column address
														-- this is technically a 16-bit word address but SDRAM_PHY ignores the least significant bit of the column address
														-- so the address space is effectively 32-bit word addressable
	
	wr_we : in std_logic_vector(3 downto 0);
	wr_dat : in std_logic_vector(31 downto 0);
	wr_ack : out std_logic;

	rd_re : in std_logic;
	rd_dat : out std_logic_vector(63 downto 0);
	rd_ack : out std_logic;   
	rd_valid : out std_logic;

	SDRAM_A : out std_logic_vector(13 downto 0);
	SDRAM_BA : out std_logic_vector(2 downto 0);
	SDRAM_CKE      : out std_logic;
	SDRAM_CK       : out std_logic;
	SDRAM_nCK	   : out std_logic;
	SDRAM_DQ       : inout std_logic_vector(15 downto 0);    
	SDRAM_DQS	   : inout std_logic_vector(1 downto 0);
	--SDRAM_nDQS	   : inout std_logic_vector(1 downto 0);
	SDRAM_DM    : out std_logic_vector(1 downto 0);
	SDRAM_nCAS     : out std_logic;
	SDRAM_nCS      : out std_logic;
	SDRAM_nRAS     : out std_logic;
	SDRAM_nWE      : out std_logic);

end DDR_SDRAM_CTRL;


architecture Struct of DDR_SDRAM_CTRL is      

component DDR_SDRAM_PHYIO is 
port (
	CLK   : in  std_logic;
	CLK_130 : in std_logic;
    reset : in  std_logic; 

	wrrd_ba_add : in std_logic_vector(2 downto 0);
	wrrd_ras_add : in std_logic_vector(12 downto 0);
	wrrd_cas_add : in std_logic_vector(8 downto 0);
	
	wr_we : in std_logic_vector(3 downto 0);
	wr_dat : in std_logic_vector(31 downto 0);
	wr_ack : out std_logic;

	rd_re : in std_logic;
	rd_dat : out std_logic_vector(63 downto 0);
	rd_ack : out std_logic;
	rd_valid : out std_logic;        
	
	refresh : in std_logic;
	ref_ack	: out std_logic;

	SDRAM_A : out std_logic_vector(13 downto 0);
	SDRAM_BA : out std_logic_vector(2 downto 0);
	SDRAM_CKE      : out std_logic;
	SDRAM_CK       : out std_logic;
	SDRAM_nCK	   : out std_logic;
	SDRAM_DQ       : inout std_logic_vector(15 downto 0); 
	SDRAM_DQS	   : inout std_logic_vector(1 downto 0);
	--SDRAM_nDQS	   : inout std_logic_vector(1 downto 0);
	SDRAM_DM    : out std_logic_vector(1 downto 0);
	SDRAM_nCAS     : out std_logic;
	SDRAM_nCS      : out std_logic;
	SDRAM_nRAS     : out std_logic;
	SDRAM_nWE      : out std_logic);

end component;     

----------------------------------------------------
-- Refresh parameters
----------------------------------------------------
-- This module, SDRAM_CTRL, raises a refresh request each refreshInterval clock cycles
-- SDRAM_PHY issues refreshCount consecutive REFRESH commands in response to each refresh request
-- Each REFRESH command takes at least tRFC = 127.5 ns
-- The refresh period of the MT47H64M16HR-25E is 64ms, thus to comply with the specification
-- refreshInterval * ( refreshCount [defined in SDRAM_PHYIO.vhd] + 1 ) * clock_period <= 64ms

-- Example refresh strategies at 125MHz (based on a 7.2us clock period including allowance)
-- 1. Refresh the entire SDRAM once each 64ms, blocking the device for 1ms each time
-- 		constant refreshInterval : integer range 0 to 16777215 := 8888888;	
-- 		constant refreshCount : integer range 0 to 8191 := 8191;
--
-- 2. Refresh the entire SDRAM once each 0.75ms, blocking the device for 12.5us each time
-- 		constant refreshInterval : integer range 0 to 16777215 := 54000;	
-- 		constant refreshCount : integer range 0 to 8191 := 95;
--
-- 3. Refresh the SRDRAM once each 62.5us, blocking the device for 1us each time
--		constant refreshInterval : integer range 0 to 16777215 := 4500;
-- 		constant refreshCount : integer range 0 to 8191 := 7;	

constant refreshInterval : integer range 0 to 16777215 := 4500;		-- number of clock cycles between each refresh request                    
signal refresh_time_cnt : integer range 0 to 16777215;
signal refresh : std_logic;
signal ref_ack : std_logic;

begin  
 
-----------------------------------------------------
--	Refresh Mechanism
-----------------------------------------------------

refresh_gen : process (CLK, reset)
begin
	if (reset='1') then
		refresh_time_cnt <= 0;
		refresh <= '0';
	elsif (CLK'event and CLK='1') then	
		-- free running timer/counter
		if (refresh_time_cnt = refreshInterval) then       
			refresh_time_cnt <= 0;
		else
			refresh_time_cnt <= refresh_time_cnt + 1;
		end if;
		-- refresh request signal
		if (refresh_time_cnt = refreshInterval) then
			refresh <= '1';
		elsif (ref_ack = '1') then
			refresh <= '0';
		end if;
	end if;
end process;                  

-- instantiate DDR_SDRAM_PHYIO
DDR_SDRAM_PHYIOi : DDR_SDRAM_PHYIO 
port map (
	CLK   => CLK,
	CLK_130 => CLK_130,
    reset => reset,  

	wrrd_ba_add => wrrd_ba_add,
	wrrd_ras_add => wrrd_ras_add,
	wrrd_cas_add => wrrd_cas_add,
	
	wr_we => wr_we,
	wr_dat => wr_dat,
	wr_ack => wr_ack, 
	
	rd_re => rd_re,
	rd_dat => rd_dat,
	rd_ack => rd_ack,  
	rd_valid => rd_valid,  
	
	refresh => refresh,
	ref_ack => ref_ack,

	SDRAM_A 		=> SDRAM_A,
	SDRAM_BA 		=> SDRAM_BA,
	SDRAM_CKE      	=> SDRAM_CKE,
	SDRAM_CK        => SDRAM_CK,
	SDRAM_nCK	    => SDRAM_nCK,
	SDRAM_DQ       	=> SDRAM_DQ,
	SDRAM_DQS	    => SDRAM_DQS,
	--SDRAM_nDQS	    => SDRAM_nDQS,
	SDRAM_DM    	=> SDRAM_DM,
	SDRAM_nCAS     	=> SDRAM_nCAS,
	SDRAM_nCS      	=> SDRAM_nCS,
	SDRAM_nRAS     	=> SDRAM_nRAS,
	SDRAM_nWE      	=> SDRAM_nWE);

end Struct;
