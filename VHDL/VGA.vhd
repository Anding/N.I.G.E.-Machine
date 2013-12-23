-- VGA pixel graphics controller
-- Andrew Read
-- Created 9 May 2010

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity VGA is
    Port ( clk_VGA	: in STD_LOGIC;								-- 25/50MHz
			  RESET	   : in STD_LOGIC;
			  mode		: in STD_LOGIC_VECTOR (4 downto 0);		-- (4) VGA/SVGA	(3) interlace off/on (2) bitmapped off/on, 
																				-- (1) 16&16/0&256 char color mode, (0) character mapped off/on
			  background : in STD_LOGIC_VECTOR (7 downto 0);	-- background color for 0&256 char color mode
			  data_VGA : in STD_LOGIC_VECTOR (7 downto 0);		-- bitmapped memory
			  addr_VGA : out STD_LOGIC_VECTOR (8 downto 0);
			  data_Text : in STD_LOGIC_VECTOR (15 downto 0);	-- text memory
			  addr_Text : out STD_LOGIC_VECTOR (6 downto 0);	-- refers to the column 0 - 79
			  data_Char : in STD_LOGIC_VECTOR (7 downto 0);		-- character memory
			  addr_Char : out STD_LOGIC_VECTOR (10 downto 0);
           HSync : out  STD_LOGIC;									-- VGA adapter connections
           VSync : out  STD_LOGIC;
--			  start_VGA : out  STD_LOGIC;								-- triggers for DMA to access first line of memory
--			  start_TXT : out  STD_LOGIC;	
			  VBLANK	: out STD_LOGIC;									-- vertical blank
			  RGB : out  STD_LOGIC_VECTOR (11 downto 0);
			  VGA_columns : out std_logic_vector(6 downto 0);
			  VGA_active : out std_logic := '0';
			  VGA_newline : out std_logic := '0'
			  );
end VGA;

architecture Behavioral of VGA is
signal Vcount : std_logic_vector(10 downto 0) := CONV_STD_LOGIC_VECTOR(665,11); 					-- := CONV_STD_LOGIC_VECTOR(526,11); 		-- Vertical pixel count
signal tVcount, height : std_logic_vector(3 downto 0) := (others=>'0'); 		-- text Vertical count
signal Hcount : std_logic_vector(10 downto 0) := (others=>'0'); 					-- Horizontal pixel count
signal addressPixel : std_logic_vector(8 downto 0):= (others=>'0');  			-- On screen address of current pixel
signal addressText : std_logic_vector(6 downto 0):= (others=>'0');		  		-- On screen address of current character
signal addressChar : std_logic_vector(10 downto 0):= (others=>'0');		  		-- Char RAM lookup of current character

signal Hblk0, Hblk1, Hblk2, Hblk3, Hblk4: std_logic;									-- Signals video blank period
signal Vblk0,vblk1m_i: std_logic := '1';													-- Signals video blank period
signal pixelGFX0, pixelGFX1, pixelGFX2 : std_logic_vector(7 downto 0);			-- Graphics pixel pipeline
signal char_pixels : STD_LOGIC_VECTOR(7 downto 0);										-- Character shift register.  8 or 16 bit depending on character width
signal char_color : STD_LOGIC_VECTOR(7 downto 0);										-- Colour data for current character
signal Ha, Hb, Hc, Hd, He : std_logic_vector(10 downto 0);
signal Va, Vb, Vc, Vd, Ve : std_logic_vector(10 downto 0);
signal COLUMNS : std_logic_vector(6 downto 0);

begin

	addr_VGA <= addressPixel;
	addr_Text <= addressText;
	addr_Char <= addressChar;
--	start_VGA <= vblk1m_i and (not RESET) and (mode(2));
--	VGA_active <= (not vblk1m_i) and (not RESET) and (mode(0));
	VBLANK <= vblk1m_i;
	VGA_COLUMNS <= COLUMNS + 1;
	
	with mode(3) select 															-- select the interlace mode
		height <= "1001" when '1',
					 "0111" when others;
	PROCESS (mode)
	begin
		if mode(4) = '0' then									-- VGA
			Ha <= CONV_STD_LOGIC_VECTOR(799,11);				-- last horizontal pixel on the full line
			Hb <= CONV_STD_LOGIC_VECTOR(660,11);				-- beginning of horizontal front porch (Hsync begins)
			Hc <= CONV_STD_LOGIC_VECTOR(756,11);				-- end of horizontal front porch (Hsync ends)
			Hd <= CONV_STD_LOGIC_VECTOR(639,11);				-- last visible horizontal pixel
			Va <= CONV_STD_LOGIC_VECTOR(527,11);				-- last vertical pixel on the full screen
			Vb <= CONV_STD_LOGIC_VECTOR(491,11);				-- beginning of vertical front porch (Vsync begins)
			Vc <= CONV_STD_LOGIC_VECTOR(493,11);				-- end of vertical front porch (Vsync ends)
			Vd <= CONV_STD_LOGIC_VECTOR(478,11);				-- last but one visible vertical pixel
			Ve <= CONV_STD_LOGIC_VECTOR(526,11);				-- last vertical pixel less one
			COLUMNS <= CONV_STD_LOGIC_VECTOR(79,7);
		else															-- SVGA
			Ha <= CONV_STD_LOGIC_VECTOR(1039,11);
			Hb <= CONV_STD_LOGIC_VECTOR(861,11);
			Hc <= CONV_STD_LOGIC_VECTOR(981,11);
			Hd <= CONV_STD_LOGIC_VECTOR(799,11);
			Va <= CONV_STD_LOGIC_VECTOR(666,11);
			Vb <= CONV_STD_LOGIC_VECTOR(637,11);
			Vc <= CONV_STD_LOGIC_VECTOR(643,11);
			Vd <= CONV_STD_LOGIC_VECTOR(598,11);
			Ve <= CONV_STD_LOGIC_VECTOR(665,11);
			COLUMNS <= CONV_STD_LOGIC_VECTOR(99,7);
		end if;
	end process;
							
	PROCESS
	variable text_f, text_b : std_logic_vector(11 downto 0);			-- text background and forground colors	
	begin
		wait until rising_edge(clk_VGA);	
	
		-- Horizontal pixel clock		
		if Hcount = Ha then 					-- last horizontal pixel: VGA 799, SVGA 1039
			Hcount <= (others => '0');	
		else
			Hcount <= Hcount + 1;			
		end if;	
		
		-- Vertical pixel clock
		if Hcount = Ha then 					-- last horizontal pixel: VGA 799, SVGA 1039
			if Vcount = Va then					-- last vertical pixel: VGA 527, SVGA 666
				Vcount <= (others => '0');				
			else
				Vcount <= Vcount + 1;					
			end if;	
		end if;
		
		-- text Vertical count
		if Hcount = Ha then					-- last horizontal pixel: VGA 799, SVGA 1039
			if Vcount = Va or tVcount = height then		-- last vertical pixel: VGA 527, SVGA 666
				tVcount <= (others => '0');
			else
				tVcount <= tVcount + 1;
			end if;
		end if;
		
		-- Horizontal sync signal
		if Hcount = Hb then						-- screen width plus horizontal front porch (HFP) plus 2: VGA 660, SVGA 861 
			HSync <= '0';
		elsif Hcount = Hc then					-- screen width plus HFB plus Hpulse plus 2: VGA 756, SVGA 981 
			HSync <= '1';			
		end if;
		
		-- Vertical sync signal
		if Vcount = Vb then						-- screen height plus vertical front porch (VFP) plus one: VGA 491, SVGA 637
			VSync <= '0';			
		elsif Vcount = Vc then					-- screen height plus VFP plus Vpulse plus one: VGA 493, SVGA 643
			VSync <= '1';			
		end if;	
		
		-- Vertical blank signal
		if Hcount = Ha then					-- last horizontal pixel: VGA 799, SVGA 1039
			if Vcount = Vd then					-- screen height less two: VGA 478, SVGA 598
				vblk1m_i <= '1';					-- one row early, for driving DMA to fetch memory in advance
			elsif Vcount = Ve then 		   -- last vertical pixel less one: VGA 526, SVGA 665
				vblk1m_i <= '0';		
			end if;
			vblk0 <= vblk1m_i;					-- vertical blank signal		
		end if;	
	
		-- Horizontal blank signals
		if Hcount = Hd then						-- last visible pixel: VGA 639, SVGA 799
			Hblk0 <= '1';							-- Hblk0 synchronized with pixel clocks for pixel creation
		elsif Hcount = Ha then				-- last horizontal pixel: VGA 799, SVGA 1039
			Hblk0 <= '0';							
		end if;
		Hblk1 <= Hblk0;
		Hblk2 <= Hblk1;												
		Hblk3 <= Hblk2;
		Hblk4 <= Hblk3;							-- Hblk4 synchronized with sync signals for pixel out	
		
		-- TEXTbuffer new line
		if Hcount = Hb and tVcount < 8 then
			VGA_newline <= '1';
		elsif Hcount = Ha then
			VGA_newline <= '0';
		end if;
		
		-- TEXTbuffer VGA active
		if Vcount = Va then					-- also check for mode(0) 
			VGA_active <= '1';
		elsif Vcount = Vd then
			VGA_active <= '0';
		end if;
		
		-- Graphics RAM address generator
		if RESET = '0' and mode(2) = '1' and vblk0 = '0' then									
			if Hblk0 = '0' then
				addressPixel <= addressPixel + 1; 	-- increment the address within display region
			end if;
		else
			addressPixel <= (others=>'0');
		end if;
	  
	  	-- Text RAM address generator
		if RESET = '0' and mode(0) = '1' and vblk0 = '0' and tVcount < 8 then
			if Hblk0 = '0' and Hcount(2 downto 0) = 7 then		-- (2 downto 0) or (3 downto 0) = 7 or 15, for 8 or 16 bit character width		
				if addressText = COLUMNS then				-- VGA 79, SVGA 99			
					addressText <= (others=>'0');		-- next frame row	
				else
					addressText <= addressText + 1;	-- last frame column: move to next character
				end if;	
			end if;	
		else
			addressText <= (others=>'0');
		end if;
			
		-- Read text memory and set char memory address
		addressChar(10 downto 3)<= data_Text(7 downto 0); 		-- char contents of Text RAM
		addressChar(2 downto 0) <= tVcount(2 downto 0);			-- Vcount(2 downto 0) or (3 downto 1) for 8 or 16 bit char width	
	  
	  	-- Read char memory and control shift register
		if (Hcount(2 downto 0) = 4) then								-- trip at 4 for synchronization with output signals
			char_pixels <= data_Char; 
			char_color <= data_Text(15 downto 8);					-- colour content of Text RAM				
		else
			char_pixels <= char_pixels(6 downto 0) & '0';		-- shift the register: 6 or 14 for 8 or 16 bit char width
		end if;
	  
		-- Read graphics memory
		if mode(2) = '1' then
			pixelGFX0 <= data_VGA;						-- content of graphics memory	
		else 
			pixelGFX0 <= (others=>'0');
		end if;
		pixelGFX1 <= pixelGFX0;						-- pipeline delay to synchronize graphics with text
		pixelGFX2 <= pixelGFX1;
		
		-- Define character color scheme
		if not(tVcount < 8) then					-- interlace lines
			text_f := background(7) & background(6) & background(5) & background(5) &
						 background(4) & background(3) & background(2) & background(2) &
						 background(1) & background(1) & background(0) & background(0);
			text_b := background(7) & background(6) & background(5) & background(5) &
						 background(4) & background(3) & background(2) & background(2) &
						 background(1) & background(1) & background(0) & background(0);
		elsif mode(1 downto 0) = "11" then 
			text_b := background(7) & background(6) & background(5) & background(5) &
						 background(4) & background(3) & background(2) & background(2) &
						 background(1) & background(1) & background(0) & background(0);		
			text_f := char_color(7) & char_color(6) & char_color(5) & char_color(5) &
						 char_color(4) & char_color(3) & char_color(2) & char_color(2) &
						 char_color(1) & char_color(1) & char_color(0) & char_color(0);
		elsif mode(1 downto 0) = "01" then
			text_b := char_color(7) & char_color(7) & char_color(7) & char_color(6) & 
						char_color(6) & char_color(6) & char_color(5) & char_color(5) & 
						char_color(5) & char_color(4) & char_color(4) & char_color(4);		
			text_f := char_color(3) & char_color(3) & char_color(3) & char_color(2) & 
						char_color(2) & char_color(2) & char_color(1) & char_color(1) & 
						char_color(1) & char_color(0) & char_color(0) & char_color(0);				
		else
			text_f := (others=>'0');
			text_b := (others=>'0');
		end if;
		
		-- Drive pixel to output
		if Hblk4 = '0' and Vblk0 = '0' then			-- Hblk4 for synchronization
			if char_pixels(7) = '1' then		
				RGB <= text_f; --XOR pixelGFX2;			-- RGB combine text and graphics pixels in display area
			else
				RGB <= text_b; --XOR pixelGFX2;
			end if;
		else											
			RGB <= (others=>'0');						-- RGB low in horizantal and vertical blank interval
		end if;

	end process;

end Behavioral;

