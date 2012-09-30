\ colorscheme ( n --, update the color scheme)
: colorscheme
IF
	128 255		\ C= Amiga look
ELSE
	0 58			\ C= PET look
THEN	
palette 1 + c! background
;

: colormode ( n --, change the color mode)
63497 swap IF
	63497 dup c@ 2 or swap c!		\ 0&256 color mode
ELSE
	63497 dup c@ 253 and swap c!	\ 16&16 color mode
THEN
;

\ font ( --, display the character set)
: font 256 13 do 9 emit i emit loop ;



