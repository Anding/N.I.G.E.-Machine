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

: extract-bits { offset addr bitWidth -- bitmapData }
\ extract bitwidth bits starting at offset from a bitmap at addr
\ bitwidth is maximum 16. Bitmap will be left aligned as a word on the stack
		addr c@ 		( byte0)	\ addr is a local -  accessed parameter style
		256 *					\ bring 3 consecutive bytes onto the stack	
		addr 1+  c@				\ left shift byte by byte for big endian format
		or 256 *
		addr 2 + c@
 		or				( rawData)
		8 offset - Rshift		\ align the bitmap at the word boundary
		bitwidth 2*			( shiftedData maskNo) \ prepare to mask off surplus bits
		extractMask + w@		( shiftedData mask)
		and				( data)
;

: extract-char { char | startBit bitWidth -- }
\ extract character line from the bitmap
	char 4 * 			\ each char takes 4 bytes in the charLoc data
	charLoc @ + 			( a)
	dup w@ -> startBit
	2 + w@ -> bitWidth
	ySize @ 0 
	DO				\ loop through each row of the character
		startBit 8 /MOD 	( offset bytes) 	\ locate the start of this char in the bitmap
		charData @ +		( offset addr)
		bitWidth		( offset addr bitWidth)
		extract-bits 		( bitmapData)
		binary 16 .R
		startBit modulo @ + -> startBit		\ start of next row of the character
	LOOP
	decimal
;
