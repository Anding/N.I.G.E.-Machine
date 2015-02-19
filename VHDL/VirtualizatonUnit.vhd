library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;


entity VirtualizationUnit is
	 Generic (vmp_w : integer);
						
    Port (	clk : in STD_LOGIC;
				rst : in STD_LOGIC;
				-- virtualization control signals
				pause : in STD_LOGIC;
				SingleMulti : out STD_LOGIC;
				interval : out STD_LOGIC_VECTOR (15 downto 0);
				-- number of the currently executing virtual machine
				VM : out  STD_LOGIC_VECTOR (vmp_w -1 downto 0);
				-- freeze the current VM and thaw the next VM
				PCfreeze : in STD_LOGIC_VECTOR (19 downto 0);
				PCthaw : out STD_LOGIC_VECTOR (19 downto 0);
				VirtualInterrupt : out STD_LOGIC_VECTOR (19 downto 0);
				DatapathFreeze : in  STD_LOGIC_VECTOR (95 downto 0);
				DatapathThaw : out  STD_LOGIC_VECTOR (95 downto 0);
				-- system memory bus interface
				en : in STD_LOGIC;
				addr : in  STD_LOGIC_VECTOR (10 downto 0);
				datain : in  STD_LOGIC_VECTOR (31 downto 0);
				dataout : out  STD_LOGIC_VECTOR (31 downto 0);
				wrq : in  STD_LOGIC
			);
end VirtualizationUnit;

architecture RTL of VirtualizationUnit is

constant blank : STD_LOGIC_VECTOR(31 downto 0) := (others=>'0');
signal currentVM, nextVM : STD_LOGIC_VECTOR (vmp_w - 1 downto 0);
signal TaskControl, dataoutTaskControl : STD_LOGIC_VECTOR (15 downto 0);
signal PCoverride, dataoutPCoverride : STD_LOGIC_VECTOR(19 downto 0);
signal dataoutVirtualInterrupt : STD_LOGIC_VECTOR(19 downto 0);
signal dataoutRegisters : STD_LOGIC_VECTOR(31 downto 0);
signal CPUFreeze, CPUThaw : STD_LOGIC_VECTOR (115 downto 0);
signal wrq_i : STD_LOGIC_VECTOR(3 downto 0);
signal addr_m : STD_LOGIC_VECTOR(10 downto 9);
signal reg_SingleMulti : STD_LOGIC;
signal wrq_VirtualInterrupt, wrq_PCoverride : STD_LOGIC;
signal addr_local : STD_LOGIC_VECTOR(vmp_w -1 +2 downto 2);
signal datain_local : STD_LOGIC_VECTOR(19 downto 0);
signal task_switch : STD_LOGIC;
signal trueSwitch : STD_LOGIC;
signal reg_interval : STD_LOGIC_VECTOR(15 downto 0) := (others=>'0');
signal pauseDelay : STD_LOGIC_VECTOR(4 downto 0);

begin

-- virtual machine update

trueSwitch <= '0' when (currentVM = nextVM) else '1';				-- avoid a task switch to the same task, which would fail

nextVM <= TaskControl(vmp_w -1 downto 0);
VM <= currentVM;
interval <= reg_interval;
task_switch <= pause;
SingleMulti <= '1' when (reg_SingleMulti = '1' and trueSwitch = '1' and (pauseDelay = "00000")) else '0';

process
begin
	wait until rising_edge(clk);
	if rst = '1' then
		currentVM <= (others=>'0');
		pauseDelay <= "00000";
	else
		if task_switch = '1' then 
			currentVM <= nextVM;
			pauseDelay <= "10000";
		else
			currentVM <= currentVM;
			pauseDelay <= '0' & pauseDelay(4 downto 1);
		end if;
	end if;
end process;

-- freeze and thaw CPU state

with PCoverride select
	PCthaw <= CPUthaw(19 downto 0) when "00000000000000000000",
	PCoverride when others;
	
DatapathThaw <= CPUthaw(115 downto 20);
	
CPUfreeze <=  DatapathFreeze & PCfreeze;

-- system memory bus mapping

	-- 09876543210
	--          xx   	byte/word address is disregarded
	--   6543210		map with space to address 128 virtual machines	
	-- 10					4 blocks: registers, TaskControl, PCoverride, VirtualInterrupt

wrq_i <= "0001" when en = '1' and wrq = '1' and addr(10 downto 9) = "00" ELSE
			"0010" when en = '1' and wrq = '1' and addr(10 downto 9) = "01" ELSE
			"0100" when en = '1' and wrq = '1' and addr(10 downto 9) = "10" ELSE
			"1000" when en = '1' and wrq = '1' and addr(10 downto 9) = "11" ELSE
			"0000";

process											-- dataout is one cycle delayed so multiplex based on prior address value
begin
	wait until rising_edge(clk);
	addr_m <= addr(10 downto 9);
end process;

with addr_m	select
	dataout <= blank(31 downto 16) & dataoutTaskControl when "01",
	blank(31 downto 20) & dataoutPCoverride when "10",
	blank(31 downto 20) & dataoutVirtualInterrupt when "11",
	dataoutRegisters when others; -- "00";
	
-- when a pause signal occurs utilize the writeable port on the PCoverride and VirtualInterrupt
 -- 	distributed RAM to clear the PCoverride address and VirtualInterupt address so that
 --	these values will serve one time only
 
 with task_switch select
	addr_local <= nextVM when '1',
	addr(vmp_w +1 downto 2) when others;
	
 with task_switch select
	wrq_PCoverride <= '1' when '1',
	wrq_i(2) when others;
	
 with task_switch select
	wrq_VirtualInterrupt <= '1' when '1',
	wrq_i(3) when others;
	
 with task_switch select
	datain_local <= (others=>'0') when '1',
	datain(19 downto 0) when others;
			
-- distributed RAM modules

Inst_Freezer_RAM : entity work.Freezer_RAM
  PORT MAP (
    a => currentVM,
    d => CPUfreeze,
    dpra => nextVM,
    clk => clk,
    we => pause,
    qdpo => CPUthaw
  );

Inst_TaskControl_RAM : entity work.TaskControl_RAM					-- dual port RAM, depth 32, width 16
  PORT MAP (
    clk => clk,  
    a => addr(vmp_w +1 downto 2),
    d => datain(15 downto 0),
    we => wrq_i(1),	
    qspo => dataoutTaskControl,	 
    dpra => currentVM,
    qdpo => TaskControl	 
  );

  
 Inst_PCOveride_RAM : entity work.PCOveride_RAM  					-- dual port RAM, depth 32, width 20
  PORT MAP (
    clk => clk,  
    a => addr_local,
    d => datain_local,
    we => wrq_PCoverride,	 
    qspo => dataoutPCoverride,	 
    dpra => nextVM,
    qdpo => PCOverride
  );

 Inst_VirtualInterrupt_RAM : entity work.VirtualInterrupt_RAM	-- dual port RAM, depth 32, width 20
  PORT MAP (
    clk => clk,
    a => addr_local,
    d => datain_local,
    we => wrq_VirtualInterrupt,	 
    qspo => dataoutVirtualInterrupt,	 
    dpra => nextVM,
    qdpo => VirtualInterrupt
  );
  
 -- local registers
 
	-- read cycle
 process
 begin
	wait until rising_edge(clk);
	
	case addr(5 downto 2) is
		when X"0" =>
			dataoutRegisters <= blank(31 downto 1) & reg_SingleMulti;
			
		when X"1" =>
			dataoutRegisters <= blank(31 downto vmp_w) & CurrentVM;
			
		when X"2" =>
			dataoutRegisters <= blank(31 downto 16) & reg_interval;
			
		when others =>
			dataoutRegisters <= blank;
		end case;
end process;

	-- write cycle
process
begin
	wait until rising_edge(clk);
	
	if rst = '1' then
		reg_SingleMulti <= '0';
		
	elsif wrq_i(0) = '1' then
		case addr(5 downto 2) is
		
			when X"0" =>
				reg_SingleMulti <= datain(0);
				
			when X"2"=>
				reg_interval <= datain(15 downto 0);
				
			when others =>
				null;
				
		end case;
	end if;
end process;

end RTL;

