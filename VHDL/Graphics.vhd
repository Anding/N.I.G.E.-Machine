library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity Graphics is

Port ( 
	clk_VGA	: in STD_LOGIC;
	clk_MEM	: in STD_LOGIC;
	reset 		: in STD_LOGIC;
	-- VGAsignals
	HSync : out  STD_LOGIC;
	VSync : out  STD_LOGIC;
	RGB : out  STD_LOGIC_VECTOR (11 downto 0);				  
	-- HWregisters
	mode		: in STD_LOGIC_VECTOR (4 downto 0);	-- VGA mode in hardware registers	
	background : in STD_LOGIC_VECTOR (15 downto 0);		-- background color for 0&256 char color mode
	interlace	: in	STD_LOGIC_VECTOR (3 downto 0);	-- number of interlace scan lines between character rows
	charHeight: in	STD_LOGIC_VECTOR (3 downto 0);	-- height of a character in pixels LESS ONE
	charWidth: in	STD_LOGIC_VECTOR (3 downto 0);		-- width of a character in pixels LESS ONE
	VGArows : in STD_LOGIC_VECTOR (7 downto 0);		-- number of complete character columns displayed on the screen					  
	VGAcols : in STD_LOGIC_VECTOR (7 downto 0);		-- number of complete character columns displayed on the scree
	VBlank	: out STD_LOGIC;					-- vertical blank		
	txt_zero : IN std_logic_vector(23 downto 0);		-- base address of the screen buffer in PSDRAM
	-- AXI burst read channel
	t_axi_araddr : OUT  std_logic_vector(31 downto 0);
	t_axi_arlen : OUT  std_logic_vector(7 downto 0);		-- Burst length = value + 1.  Set directly from VGA_columns
--	t_axi_arsize : OUT  std_logic_vector(2 downto 0);	-- Size in bytes: "001" = 2 bytes, "010" = 4 bytes
--	t_axi_arburst : OUT  std_logic_vector(1 downto 0);	-- Type: "01" = INCR
	t_axi_arvalid : OUT  std_logic;
	t_axi_arready : IN  std_logic;
	t_axi_rdata : IN  std_logic_vector(15 downto 0);
--	t_axi_rresp : IN  std_logic_vector(1 downto 0);			
	t_axi_rlast : IN  std_logic;				-- Set high on last data item
	t_axi_rvalid : IN  std_logic;
--	t_axi_rready : OUT  std_logic;		
	-- Connections to CHAR RAM and COLOR RAM
	data_Char : in STD_LOGIC_VECTOR (15 downto 0);
	addr_Char : out STD_LOGIC_VECTOR (11 downto 0);
	data_Color : in STD_LOGIC_VECTOR (15 downto 0);
	addr_Color : out STD_LOGIC_VECTOR (7 downto 0)				
	);
end Graphics;

architecture RTL of Graphics is

signal DATA_OUT_VGA : std_logic_vector(7 downto 0) := (others=>'0');
signal ADDR_VGA : std_logic_vector(8 downto 0);
signal DATA_TEXT : std_logic_vector(15 downto 0) := (others=>'0');
signal ADDR_TEXT : std_logic_vector(7 downto 0);
signal FetchNextRow : std_logic;
signal VBlank_i : std_logic;

begin

VBlank <= VBlank_i;

inst_VGAController: entity work.VGA PORT MAP(
	CLK_VGA => CLK_VGA,
	reset => reset,
	mode	=> mode,
	background => background,
	data_Text => DATA_TEXT,
	addr_Text => ADDR_TEXT,
	data_Char => data_Char,
	addr_Char => addr_Char,
	data_Color => data_Color,
	addr_Color => addr_Color,		
	HSync => HSync,
	VSync => VSync,
	VBLANK => VBLANK_i,
	RGB => RGB,
	interlace => interlace,
	charHeight => charHeight,
	charWidth => charWidth, 
	VGArows => 	VGArows,	  
	VGAcols => VGAcols,
	FetchNextRow => FetchNextRow
);	
	
inst_TEXTbuffer: entity work.TEXTbuffer PORT MAP(
	clk_MEM => clk_MEM,
	clk_VGA => clk_VGA,
	VGAcols => VGAcols,
	VBlank => VBlank_i,
	FetchNextRow => FetchNextRow,
	txt_zero => txt_zero,
	ADDR_TEXT => ADDR_TEXT,
	DATA_TEXT => DATA_TEXT,
	t_axi_araddr => t_axi_araddr,
	t_axi_arlen => t_axi_arlen,
--	t_axi_arsize => t_axi_arsize,
--	t_axi_arburst => t_axi_arburst,
	t_axi_arvalid => t_axi_arvalid,
	t_axi_arready => t_axi_arready,
	t_axi_rdata => t_axi_rdata,
--	t_axi_rresp => t_axi_rresp,
	t_axi_rlast => t_axi_rlast,
	t_axi_rvalid => t_axi_rvalid
--	t_axi_rready => t_axi_rready
);

end RTL;

