library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity System_Memory is
Generic (	
	vmp_w : integer;
	psp_w : integer;
	rsp_w : integer;
	ssp_w : integer;
	esp_w : integer
);
PORT (	
	clk_system : in std_logic;
	clk_vga : in std_logic;
	reset : in std_logic;
	MEMaddr : in STD_LOGIC_VECTOR (31 downto 0);			
	MEMdatain_X : out STD_LOGIC_VECTOR (31 downto 0);
	MEMdatain_X_quick : out STD_LOGIC_VECTOR (31 downto 0);
	MEMdataout_X : in STD_LOGIC_VECTOR (31 downto 0);		
	MEM_WRQ_X : in STD_LOGIC;	
	MEMsize_X : in STD_LOGIC_VECTOR (1 downto 0);
	stack_access_EN : out STD_LOGIC;
	vir_EN : out STD_LOGIC;
	reg_en : out STD_LOGIC;
	MEMdata_vir : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
	MEMdata_stack_access : IN std_logic_vector(31 downto 0);
	MEMdata_reg : IN std_logic_vector(31 downto 0);
	Boot_we : IN STD_LOGIC_VECTOR (0 downto 0);
	Boot_data  : IN std_logic_vector(31 downto 0); 
       Boot_addr  : IN std_logic_vector(31 downto 2);
	VM : IN STD_LOGIC_VECTOR (vmp_w -1 downto 0);
	data_Char : out STD_LOGIC_VECTOR (15 downto 0);
	addr_Char : in STD_LOGIC_VECTOR (11 downto 0);
	data_Color : out STD_LOGIC_VECTOR (15 downto 0);
	addr_Color : in STD_LOGIC_VECTOR (7 downto 0)
);
end System_Memory;

architecture RTL of System_Memory is

type bank_t is (Sys, Char, Color, Reg, Stack_access, User, Vir);--, Pstack, Rstack);
signal bank, bank_n : bank_t;
signal douta_sysram : std_logic_vector(31 downto 0);
signal doutb_sysram : std_logic_vector(31 downto 0);   
signal douta_sysram_r : std_logic_vector(31 downto 0);
signal doutb_sysram_r : std_logic_vector(31 downto 0); 
signal douta_sysram_i : std_logic_vector(31 downto 0);
signal doutb_sysram_i : std_logic_vector(31 downto 0);         
signal wea_sysram : std_logic_vector(3 downto 0);
signal wea_sysram_s : std_logic_vector(3 downto 0);
signal addra_sysram : std_logic_vector(31 downto 2);
signal addra_sysram_s : std_logic_vector(31 downto 2);
signal dina_sysram : std_logic_vector(31 downto 0);
signal dina_sysram_s : std_logic_vector(31 downto 0);
signal web_sysram : std_logic_vector(3 downto 0);
signal addrb_sysram : std_logic_vector(31 downto 2);
signal dinb_sysram : std_logic_vector(31 downto 0);
signal ena_sysram, enb_sysram : std_logic;
signal addra_userram : std_logic_vector(31 downto 2);
signal douta_userram : std_logic_vector(31 downto 0);
signal doutb_userram : std_logic_vector(31 downto 0);          
signal wea_userram : std_logic_vector(3 downto 0);
signal addrb_userram : std_logic_vector(31 downto 2);
signal dina_userram : std_logic_vector(31 downto 0);
signal dinb_userram : std_logic_vector(31 downto 0);
signal web_userram : std_logic_vector(3 downto 0);
signal ena_userram, enb_userram : std_logic;
signal addra_userram_all : std_logic_vector(31 downto 2);
signal addrb_userram_all : std_logic_vector(31 downto 2);
signal MEMdata_Char :  std_logic_vector(15 downto 0);
signal MEMdata_Color :  std_logic_vector(15 downto 0);
signal MEMdata_Pstack, MEMdata_Rstack : std_logic_vector(31 downto 0);   
signal MEMdata_User :  std_logic_vector(31 downto 0);      
signal MEM_WRQ_XX : std_logic_vector(0 downto 0);
signal Sys_EN, Pstack_EN, Rstack_EN, Char_EN, Color_EN, User_EN : std_logic;
signal MEMdata_Sys, MEMdata_Sys_plus : std_logic_vector(31 downto 0);
signal ram_en : std_logic;

begin

inst_SYS_RAM : entity work.Sys_RAM
  PORT MAP (
	 clka => clk_system,
	 ena => ram_en,
	 wea => wea_sysram,
	 addra => addra_sysram (16 downto 2),
	 dina => dina_sysram,
	 douta => douta_sysram,
	 clkb => clk_system,
	 enb => enb_sysram,
	 web => web_sysram,
	 addrb => addrb_sysram (16 downto 2),
	 dinb => dinb_sysram,
	 doutb => doutb_sysram
);
  
--inst_RAM_for_Testbench: entity work.RAM_for_Testbench PORT MAP(
--	rst => reset,
--	clk => clk_system,
--	enA => ena_sysram,
--	enB => enb_sysram,
--	weA => wea_sysram,
--	weB => web_sysram,
--	addressA => addra_sysram (16 downto 2),
--	data_inA => dina_sysram,
--	data_outA => douta_sysram,
--	addressB => addrb_sysram (16 downto 2),
--	data_inB => dinb_sysram,
--	data_outB => doutb_sysram
--);

process
begin
	wait until rising_edge(clk_system);
	bank <= bank_n;	
end process;
 
with MEMaddr(17 downto 11) select
	bank_n <= Stack_access 	when "1110110",
	Color 				when "1110111",		
	Char 				when "1111000", 	-- must be aligned on 8k boundary
	Char 				when "1111001",
	Char 				when "1111010",
	Char				when "1111011",
	User 				when "1111100",	-- must be aligned on 4k boundary
	User 				when "1111101",					  
	Vir				when "1111110",
	Reg 				when "1111111",
	Sys 				when others;
			
Vir_EN <= '1' when bank_n = Vir else '0';
User_EN <= '1' when bank_n = User else '0';
Stack_access_EN <= '1' when bank_n = Stack_access else '0';
Color_EN <= '1' when bank_n = Color else '0';
Char_EN <= '1' when bank_n = Char else '0';
Reg_EN <= '1' when bank_n = Reg else '0';
Sys_EN <= '1' when bank_n = Sys else '0'; 
 
with bank select	-- one cycle delayed to switch output
MEMdatain_X <=	"0000000000000000" & MEMdata_Char when Char,
			"0000000000000000" & MEMdata_Color when Color,
			MEMdata_Reg when Reg,
			Memdata_stack_access when stack_access,
			MEMdata_User when user,
			MEMdata_Vir when vir,
			MEMdata_Sys when others;
						
-- splice IOExpansion data ahead of the SRAM
wea_sysram <= wea_sysram_s when Boot_we = "0" else "1111";
dina_sysram <= dina_sysram_s when Boot_we = "0" else boot_data;
addra_sysram <= addra_sysram_s when Boot_we = "0" else boot_addr;	
ram_en <= ena_sysram or reset;

douta_sysram_i <= douta_sysram;
doutb_sysram_i <= doutb_sysram;
  
addra_userram_all(vmp_w + 10 downto 2) <= VM & addra_userram(10 downto 2);
addrb_userram_all(vmp_w + 10 downto 2) <= VM & addrb_userram(10 downto 2);

MEM_WRQ_XX(0) <= MEM_WRQ_X;

inst_SRAM_controller: entity work.SRAM_controller PORT MAP(
	RST => reset,
	CLK => clk_system,
	en => SYS_EN,
	ADDR => MEMaddr,
	size => MEMsize_X,
	WE => MEM_WRQ_XX,
	DATA_in => MEMdataout_X,
	DATA_out => MEMdata_Sys,
	DATA_out_quick => MEMdatain_X_quick,
	wea => wea_sysram_s,
	addra => addra_sysram_s,
	dina => dina_sysram_s,
	douta => douta_sysram_i,
	web => web_sysram,
	addrb => addrb_sysram,
	dinb => dinb_sysram,
	doutb => doutb_sysram_i,
	en_a => ena_sysram,
	en_b => enb_sysram
);

inst_SRAM_controller_USER: entity work.SRAM_controller PORT MAP(
	RST => reset,
	CLK => clk_system,
	en => USER_EN,
	ADDR => MEMaddr,
	size => MEMsize_X,
	WE => MEM_WRQ_XX,
	DATA_in => MEMdataout_X,
	DATA_out => MEMdata_User,
	DATA_out_quick => open,
	wea => wea_userram,
	addra => addra_userram,
	dina => dina_userram,
	douta => douta_userram,
	web => web_userram,
	addrb => addrb_userram,
	dinb => dinb_userram,
	doutb => doutb_userram,
	en_a => ena_userram,
	en_b => enb_userram
);	

inst_USER_RAM : entity work.USER_RAM
  PORT MAP (
	 clka => clk_system,
	 ena => ena_userram,
	 wea => wea_userram,
	 addra => addra_userram_all(vmp_w + 10 downto 2),
	 dina => dina_userram,
	 douta => douta_userram,
	 clkb => clk_system,
	 enb => enb_userram,
	 web => web_userram,
	 addrb => addrb_userram_all(vmp_w + 10 downto 2),
	 dinb => dinb_userram,
	 doutb => doutb_userram
  );

inst_Char_RAM : entity work.Char_RAM
  PORT MAP (
	 clka => clk_VGA,
	 wea => "0",
	 addra => addr_Char,
	 dina => (others=>'0'),
	 douta => data_Char,
	 clkb => clk_system,
	 enb => Char_EN,
	 web => MEM_WRQ_XX,
	 addrb => MEMaddr(12 downto 1),
	 dinb => MEMdataout_X(15 downto 0),
	 doutb => MEMdata_Char
  );		

inst_Color_RAM : entity work.Color_RAM
  PORT MAP (
	 clka => clk_VGA,
	 ena => '1',
	 wea => "0",
	 addra => addr_Color,
	 dina => (others=>'0'),
	 douta => data_Color,
	 clkb => clk_system,
	 enb => Color_EN,
	 web => MEM_WRQ_XX,
	 addrb => MEMaddr(8 downto 1),
	 dinb => MEMdataout_X(15 downto 0),
	 doutb => MEMdata_Color
  );
	    

end RTL;

