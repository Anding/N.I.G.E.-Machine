24 buffer: FAT.filestring			\ allow space for overwrite to simplify name conversions

: FAT.String2Filename ( addr n -- addr, convert an ordinary string to a FAT directory entry)
	>R >R
	FAT.filestring dup dup dup
	12 + swap DO 32 i c! LOOP	 		\ fill the output string with blanks
	R> R>
	12 min over + swap 							
	DO						\ loop over the input string upto 12 characters
		i c@ dup 46 =				\ .
		IF
			drop drop dup 8 +		\ re-position in output string
		ELSE
			( UPPER) over c! 1+		\ save and increment position in output string
		THEN				
	LOOP
	drop
;

: FAT.ParseFileString ( 