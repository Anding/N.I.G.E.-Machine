: .FontInfo ( c-addr u --)
\ examine an Amiga font file and display header information
	R/O open-file 
	IF c" open-file error" THROW	( fileid)
	file-buffer drop			( addr)	\ address of file data
	dup 110 + w@ ." Ysize   " 9 emit . cr
	dup 114 + w@ ." Xsize   " 9 emit . cr
	dup 122 + c@ ." LoChar  " 9 emit . cr
	dup 123 + c@ ." HiChar  " 9 emit . cr
	dup 128 + w@ ." Modulo  " 9 emit . cr
	dup 126 + w@ ." CharData" 9 emit . cr
	dup 132 + w@ ." CharLoc " 9 emit . cr
	drop
;
	
	