\ Disassembler tools

\ .' ( addr -- c-addr n true | false) if addr points to the executable of a forth word, provide the name of the word and return true.  Otherwise return false
: .'
	0 GET-ENTRY 		( addr NF)		\ only the FORTH worldlist will be searched
	BEGIN
		over over U<	( addr NF flag)
	WHILE
		4 - @ 		( addr NF')		\ get the name field of the next word linked in the dictionary
	REPEAT
	dup >R 		( addr NF R:NF)
	dup c@ 31 and + 3 + 	( addr CF R:NF)
	= IF
		R>
		dup c@ 31 and
		swap 1+ swap
		true
	ELSE
		R>
		drop
		false
	THEN
;

\ STRINGTABLE
\ compile time: " LOOKUP-ABC" ," A" ," B" etc. - compile counted-strings into a lookup table, including allocating relevant space
\ run time:	 ( n - c-addr u), return the n'th string in the lookup table
\ implementation is compact but not speed optimized - suitable for small tables only
: STRINGTABLE 
	CREATE 
	DOES> 	 			( n pfield)
		swap 0 ?DO	
			dup c@ + 1+
		LOOP
		count			( c-addr u)
;

\ DASM.lookup-opcode ( opcode -- c-addr u, return the text for a given opcode)
STRINGTABLE DASM.lookup-ops
," NOP"
," DROP"
," DUP"
," SWAP"
," OVER"
," NIP"
," ROT"
," >R"
," R@"
," R>"
," PSP@"
," CATCH"
," RESETSP"
," THROW"
," +"
," -"
," NEGATE"
," 1+"
," 1-"
," 2/"
," ADDX"
," SUBX"
," ="
," <>"
," <"
," >"
," U<"
," U>"
," 0="
," 0<>"
," 0<"
," 0>"
," ZERO"
," AND"
," OR"
," INVERT"
," XOR"
," LSL"
," LSR"
," XBYTE"
," XWORD"
," MULTS"
," MULTU"
," DIVS"
," DIVU"
," FETCH.L"
," STORE.L"
," FETCH.W"
," STORE.W"
," FETCH.B"
," STORE.B"
," ?DUP"
," #.B"
," #.W"
," #.L"
," JMP"
," JSL"
," JSR"
," TRAP"
," RETRAP"
," RTI"
," PAUSE"
," --"
," --"

\ DASM ( addr n --) disassemble n bytes starting at addr
: DASM
	over + swap	( end addr)
	BEGIN
		over over >
	WHILE		
		cr
		dup 6 u.r 9 emit		\ print address
		dup c@			( end addr n)
		dup 2 u.r 9 emit		\ print byte
		dup 128 and		( end addr n branchFLAG)
		IF				\ is a branch
			dup 64 and
			IF
				." BRA"
			ELSE
				." BEQ"
			THEN
			63 and 256 * >R		( end addr+1 R: n-hi)
			1+ dup c@ 			( end addr+1 n-lo)
			R> +
			dup 8192 and
			IF
				49152 or
			THEN
			xword
			dup 9 emit .
			over + 9 emit u.
		ELSE				\ not a branch
			dup 64 =			( end addr n rtsFLAG)
			IF
				." RTS"		\ avoid "NOP,RTS" by this special case
				drop
			ELSE				( end addr n)
				dup 63 and		( end addr n opcode)
				DASM.lookup-ops type		
				dup 64 and		( end addr n rtsFLAG)
				IF
					." ,RTS"
					drop
				ELSE			
					CASE		( end addr n)
						52 OF 1+ dup c@ 9 emit u. ENDOF				\ load.b  \ dup t1 !
						53 OF 1+ dup w@ 9 emit u. 1+ ENDOF				\ load.w 
						54 OF 1+ dup  @ 9 emit u. 3 + ENDOF			\ load.l
						56 OF dup @ 16777215 and dup 9 emit u.			\ jsl
							.' IF 9 emit type THEN						
							3 + ENDOF		
					ENDCASE	( end addr')
				THEN		
			THEN
		THEN
		1+					( end next-addr)
	REPEAT
	drop
	drop
	cr
;

\ SIZEOF ( xt -- n), return the size of an executable
: SIZEOF
	1- 1- w@ 16383 and
;

\ see WORD, disassemble the word 
: SEE
	' dup SIZEOF dup		( xt size size)
	rot swap DASM			( size)
	. ." code bytes" cr	\ tell the length of code for this WORD
;

		