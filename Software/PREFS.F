DECIMAL

\ Extract an Amiga bitmapped font

variable ySize
variable xSize
variable loChar
variable hiChar
variable modulo
variable charData
variable charLoc
variable charSpace
variable charKern

: open-font ( c-addr u -- fileid ior)
\ load an Amiga font file and examine the header information
	R/O open-file			( fileid ior) 
	?dup if exit then		( fileid)
	dup file-buffer drop 	( fileid addr)\ address of file data
	dup 110 + w@ ySize !
	dup 114 + w@ xSize !
	dup 122 + c@ loChar !
	dup 123 + c@ hiChar !
	dup 128 + w@ 8 * modulo !			\ store the modulo in bits rather than bytes
	dup 126 + w@ 32 + over + charData !	\ AmigaDOS files have a 32 byte pointer offset
	dup 132 + w@ 32 + over + charLoc !
	dup 136 + w@ 32 + over + charSpace !
	dup 140 + w@ 32 + over + charKern !
	drop			( fileid)
	0			( fileid 0)
;

: mask.store over w! 2 + ;
34 BUFFER: extractMask
extractMask binary
0000000000000000 mask.store
1000000000000000 mask.store
1100000000000000 mask.store
1110000000000000 mask.store
1111000000000000 mask.store
1111100000000000 mask.store
1111110000000000 mask.store
1111111000000000 mask.store
1111111100000000 mask.store
1111111110000000 mask.store
1111111111000000 mask.store
1111111111100000 mask.store
1111111111110000 mask.store
1111111111111000 mask.store
1111111111111100 mask.store
1111111111111110 mask.store
1111111111111111 mask.store
drop decimal

: extract-bits { offset BitmapAddr bitWidth -- bitmapData }
\ extract bitwidth bits starting at offset from a bitmap at Addr
\ bitwidth is maximum 16. Bitmap will be left aligned as a word on the stack
		BitmapAddr c@ 		( byte0)	\ addr is a local -  accessed parameter style
		256 *						\ bring 3 consecutive bytes onto the stack	
		BitmapAddr 1+  c@				\ left shift byte by byte for big endian format
		or 256 *
		BitmapAddr 2 + c@
 		or				( rawData)
		8 offset - Rshift		\ align the bitmap at the word boundary
		bitwidth 2*			( shiftedData maskNo) \ prepare to mask off surplus bits
		extractMask + w@		( shiftedData mask)
		and				( data)
;

241664 constant charRAM
260176	constant charWidth
260180 constant charHeight

: extract-char { char | startBit bitWidth -- }
\ extract character line from the bitmap
	char loChar - 4 * 			\ each char takes 4 bytes in the charLoc data
	charLoc @ + 			( a)
	dup w@ -> startBit
	2 + w@ -> bitWidth
	char 16 * charRAM +		( charAddr)
	ySize @ 0 
	DO				\ loop through each row of the character
		startBit 8 /MOD 	( charAddr offset bytes) 	\ locate within the bitmap
		charData @ +		( charAddr offset BitmapAddr)
		bitWidth		( charAddr offset BitmapAddr bitWidth)
		extract-bits 		( charAddr bitmapData)
		over w!
		2 +
		startBit modulo @ + -> startBit		\ start of next row of the character
	LOOP				( charAddr)
	16 ySize @ ?DO		\ fill the remaining rows until 16 with blanks
		0 over w!
		2 +
	LOOP
	drop
;

: blank-char
\ blank-char ( char --, fill a character with blanks)
	16 * charRAM + ( charAddr)
	16 0 DO 0 over w! 2 + LOOP
	drop
;

: AMIGAFONT 
\ AMIGAFONT <filepath> ( --)
\ load and extract an Amiga bitmap font into character memory
	32 word count open-font	(fileid ior)
	IF ." could not open font file" EXIT
	cls
	
	\ fill the unspecified chars with blanks
	loChar @ 0   ?DO blank-char LOOP 	
	255 hiChar @ ?DO blank-char LOOP
	
	\ extract each character in turn
	hiChar @ 1+ lochar @ DO		
		i extract-char
	LOOP
	close-file drop
	
	\ readjust the row and column settings
	xSize @ charWidth !
	ySize @ charHeight !
	scrset					
;
	
	