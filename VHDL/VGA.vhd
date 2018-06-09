-- VGA pixel graphics controller
-- Andrew Read
-- Created 9 May 2010

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity VGA is
    Port ( clk_VGA	: in STD_LOGIC;
			  reset 		: in STD_LOGIC;
			  mode	: in STD_LOGIC_VECTOR(4 downto 0);
			  background : in STD_LOGIC_VECTOR (15 downto 0);	-- background color for 0&256 char color mode
			  interlace	: in	STD_LOGIC_VECTOR (3 downto 0);		-- number of interlace scan lines between character rows
			  charHeight: in	STD_LOGIC_VECTOR (3 downto 0);		-- height of a character in pixels LESS ONE
			  charWidth: in	STD_LOGIC_VECTOR (3 downto 0);		-- width of a character in pixels LESS ONE
			  Ha, Hb, Hc, Hd : in std_logic_vector(11 downto 0);
			  Va, Vb, Vc, Vd : in std_logic_vector(11 downto 0);
			  data_Text : in STD_LOGIC_VECTOR (15 downto 0);	-- screen buffer for current character
			  addr_Text : out STD_LOGIC_VECTOR (7 downto 0);	-- refers to the current column
			  data_Char : in STD_LOGIC_VECTOR (15 downto 0);	-- character memory	UPDATE to (15 downto 0) for 16 bit wide
			  addr_Char : out STD_LOGIC_VECTOR (11 downto 0);  -- UPDATE to (11 downto 0) for 16 bit wide
			  data_Color : in STD_LOGIC_VECTOR (15 downto 0);	-- color memory
			  addr_Color : out STD_LOGIC_VECTOR (7 downto 0);			  
          	  HSync : out  STD_LOGIC;									-- VGA adapter connections
              VSync : out  STD_LOGIC;
			  RGB : out  STD_LOGIC_VECTOR (11 downto 0);			  
			  VBlank	: out STD_LOGIC;									-- vertical blank		  
			  FetchNextRow : out std_logic;
			  FetchFirstRow : out std_logic
			  );
end VGA;

architecture Behavioral of VGA is
signal Vcount : std_logic_vector(11 downto 0) := CONV_STD_LOGIC_VECTOR(0,12); 					--  Vertical pixel count  := CONV_STD_LOGIC_VECTOR(664,12);
signal tVcount, Height : std_logic_vector(4 downto 0) := (others=>'0'); 			-- text Vertical count
signal tHcount, Width : STD_LOGIC_VECTOR (3 downto 0);
signal Hcount : std_logic_vector(11 downto 0) := (others=>'0'); 					-- Horizontal pixel count
signal addressText : std_logic_vector(7 downto 0):= (others=>'0');		  		-- Column number of current character position
signal addressChar : std_logic_vector(11 downto 0):= (others=>'0');		  		-- Char RAM lookup of current character
signal addressColor : std_logic_vector(7 downto 0):= (others=>'0');	

signal VBlank_i : std_logic_vector(7 downto 0);
signal HBlank_i : std_logic_vector(7 downto 0);
signal pixelGFX0, pixelGFX1, pixelGFX2 : std_logic_vector(7 downto 0);			-- Graphics pixel pipeline
signal char_pixels : STD_LOGIC_VECTOR(15 downto 0);										-- Character shift register.  8 or 16 bit depending on character width
signal char_color : STD_LOGIC_VECTOR(11 downto 0);										-- Colour data for current character
signal back_color : STD_LOGIC_VECTOR(11 downto 0);		
signal Ha_r, Hb_r, Hc_r, Hd_r : std_logic_vector(11 downto 0);
signal Va_r, Vb_r, Vc_r, Vd_r : std_logic_vector(11 downto 0);
signal background_r : STD_LOGIC_VECTOR (15 downto 0);
signal COLUMNS : std_logic_vector(7 downto 0);
signal Hsync_i, Vsync_i : std_logic_vector(7 downto 0);
signal RGB_i :  STD_LOGIC_VECTOR (11 downto 0);
signal mode_r : STD_LOGIC_VECTOR (4 downto 0);

constant delay : integer := 5;

begin

	-- register VGA outputs
	process
	begin
		wait until rising_edge(clk_VGA);

	end process;

	addr_Text <= addressText;
	addr_Char <= addressChar; 
	addr_Color <= addressColor;
	VBLANK <= VBLANK_i(delay);
					
	PROCESS
	variable text_f, text_b : std_logic_vector(11 downto 0);			-- text background and forground colors	
	begin
		wait until rising_edge(clk_VGA);
		
		-- register outputs	
				RGB <= RGB_i;
		Hsync <= Hsync_i(delay);
		Vsync <= Vsync_i(delay);
		
		-- register inputs
		Ha_r <= Ha; Hb_r <= Hb; Hc_r <= Hc; Hd_r <= Hd;
		Va_r <= Va; Vb_r <= Vb; Vc_r <= Vc; Vd_r <= Vd;	
		mode_r <= mode;
		background_r <= background;
		Height <= ('0' & interlace) + ('0' & charHeight);
		Width <= charWidth;
		 
		if reset = '1' then
			Vcount <= (others=>'0');
			tHcount <= (others=>'0');			
			HSync_i <= (others=>'0');
			HBLANK_i <= (others=>'0');
			Vcount <= (others=>'0');
			tVcount <= (others=>'0');			
			VSync_i <= (others=>'0');
			VBLANK_i <= (others=>'0');
			addressText <= (others=>'0');
			addressChar <= (others=>'0');
			addressColor <= (others=>'0');
			char_pixels <= (others=>'0');
			char_color <= (others=>'0');
			back_color <= (others=>'0');
			RGB_i <= (others=>'0');
		else
		
		-- pipeline
		HSync_i(7 downto 1) <= HSync_i(6 downto 0); 
		VSync_i(7 downto 1) <= VSync_i(6 downto 0); 
		HBLANK_i(7 downto 1) <= HBLANK_i(6 downto 0); 
		VBLANK_i(7 downto 1) <= VBLANK_i(6 downto 0); 	
		
		-- Horizontal pixel clock
		if Hcount = Hd_r then
			Hcount <= (others => '0');	
		else
			Hcount <= Hcount + 1;			
		end if;	
		
		-- Horizontal sync signal				-- sync signals are active low
		if Hcount = Hd_r then
			HSync_i(0) <= '0';
		elsif Hcount = Ha_r then
			HSync_i(0) <= '1';			
		end if;
		
		-- Horizontal blank signals				-- blank signals are active high
		if Hcount = Hc_r then
			HBLANK_i(0) <= '1';
		elsif Hcount = Hb_r then
			HBLANK_i(0) <= '0';
		end if;
		
		-- Vertical counters
		if Hcount = Hd_r then				-- Vertical counters are updated at the last horizontal pixel of each line
		
			-- Vertical pixel count
			if Vcount = Vd_r then
				Vcount <= (others => '0');				
			else
				Vcount <= Vcount + 1;		
			end if;
				
			-- Vertical sync signal
			if Vcount = Vd_r then
				VSync_i(0) <= '0';			
			elsif Vcount = Va_r then
				VSync_i(0) <= '1';			
			end if;		
			
			-- Vertical blank signals
			if Vcount = Vc_r then
				VBLANK_i(0) <= '1';	
			elsif Vcount = Vb_r then
				VBLANK_i(0) <= '0';
			end if;		
			
			-- text Vertical count
			if VBLANK_i(0) = '1' or tVcount = height then		
				tVcount <= (others=>'0');
			else
				tVcount <= tVcount + 1;
			end if;	
			
			if Vcount = Va_r then								-- Fetch the first row at Va_r, since it will be needed for display at Vb_r
					FetchFirstRow <= '1';
			else
					FetchFirstRow <= '0';
			end if;
																
			-- TEXTbuffer new line
			if tVcount = CharHeight then		-- request a new row of characters after the current row has been used for CharHeight scanlines
					FetchNextRow <= '1';							
			else
					FetchNextRow <= '0';
			end if;		
			
		end if;	
			
		if HBLANK_i(0) = '1' or tHcount = width then
				tHcount <= (others=>'0');	
		else	
				tHcount <= tHcount + 1;				
		end if;		
		
		if HBLANK_i(0) = '1' then
				addressText <= (others=>'0');	
		elsif tHcount = width then
				addressText <= addressText + 1;			
		end if;		
			
		-- Read text memory and set char and color memory address
		addressChar(11 downto 4) <= data_Text(15 downto 8); 		-- char contents of Text RAM
		addressChar(3 downto 0) <= tVcount(3 downto 0);				-- UPDATE to (3 downto 0) for 16 bit wide characters
		if mode_r(3) = '1' then												-- 256/0 color mode
			addressColor <= data_TEXT(7 downto 0);
		else																		-- 16/16 color mode
			addressColor <= "0000" & data_TEXT(3 downto 0);
		end if;
	  
	  	-- Read char and color memory and control shift register
		if tHcount = 4 then									
			char_pixels <= data_Char;
			char_color <= data_color(11 downto 0);
			case data_TEXT(7 downto 4) is									-- this colour table for 16&16 color mode
				when X"0" =>	back_color <= X"000";	-- black
				when X"1" =>	back_color <= X"888";	-- grey
				when X"2" =>	back_color <= X"BBB";	-- silver
				when X"3" =>	back_color <= X"FFF";	-- white
				when X"4" =>	back_color <= X"F00";	-- red
				when X"5" =>	back_color <= X"FF0";	-- yellow
				when X"6" =>	back_color <= X"0F0";	-- green
				when X"7" =>	back_color <= X"0FF";	-- cyan
				when X"8" =>	back_color <= X"00F";	-- blue
				when X"9" =>	back_color <= X"F0F";	-- magenta
				when X"A" =>	back_color <= X"800";	-- maroon
				when X"B" =>	back_color <= X"880";	-- olive
				when X"C" =>	back_color <= X"080";	-- dark green
				when X"D" =>	back_color <= X"088";	-- teal
				when X"E" =>	back_color <= X"05A";	-- dark blue
				when others =>	back_color <= X"808";	-- purple		
			end case;
		else
			char_pixels <= char_pixels(14 downto 0) & '0';	-- shift the register MSB first: 6/14
		end if;
	  		
		-- Define character color scheme
		if tVcount > CharHeight then								-- interlace lines  -- replace with tVcount >= CharHeight
			text_f := background_r(11 downto 0);				-- use back_color somehow here anyway?
			text_b := background_r(11 downto 0);
		else
			text_f := char_color;
			if mode_r(3) = '1' then 							-- 16&16 color mode
				text_b := background(11 downto 0);	
			else														-- 256&0 color mode
				text_b := back_color;
			end if;
		end if;
		
		-- Drive pixel to output
		if VBLANK_i(delay) = '0' and HBLANK_i(delay) = '0' then
		
		if char_pixels(15) = '1' then	 	
			RGB_i <= text_f;
		else
			RGB_i <= text_b;
		end if;
--			RGB_i <= (others=>'1');
		else											
			RGB_i <= (others=>'0');							-- RGB low in horizantal and vertical blank interval and when VGA is off
		end if;
		
		end if;
	end process;

end Behavioral;

