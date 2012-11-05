\ Cross assembler for the N.I.G.E. Machine

\ Copyright and license

\ The N.I.G.E machine, its design and its source code are Copyright (C) 2012 by Andrew Richard Read and dual licensed.
 
\ (1) For commercial or proprietary use you must obtain a commercial license agreement with Andrew Richard Read (anding_eunding@yahoo.com)
 
\ (2) You can redistribute the N.I.G.E. Machine, its design and its source code and/or modify it under the terms of the GNU General Public 
\ License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.  
\
\ The N.I.G.E Machine is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of 
\ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of 
\ the GNU General Public License along with this repository.  If not, see <http://www.gnu.org/licenses/>.

\ ---------------------------------------------------------------------------------------------------------------------------------

\ Variables
 decimal
 create full-buffer 67 C, 79 C, 78 C, 83 C, 84 C, 65 C, 78 C, 84 C, 32 C, 247 allot		\ Prefix with "CONSTANT " (9 characters)
 variable labl 					\ pointer to start of label in the buffer
 variable inst					\ '' instruction 
 variable qual 					\ '' qualifier 
 variable expr					\ '' expression 
 variable labl-n					\ count of label characters in buffer
 variable inst-n
 variable qual-n
 variable expr-n
 variable PC						\ program counter
 variable fileid					\ source file handle
 variable fileid-w1					\ output file handle
 variable fileid-w2
 variable fileid-w3
 variable lineno					\ 
 variable membuf					\ buffer for writing an output byte		
 4000 buffer: flow-table				\ table of PC values for IF ELSE THEN statements 
 272 buffer: instruction-count			\ count of instructions
 variable flow-pointer				\ pointer into the flow-table
 
: rel									\ useful for branch instructions.  e.g bra b1 b0 rel
	1+ -
;

: del									\ useful for .SF calculations
	1- -
;

: line-buffer								\ quasi-variable companion to full-buffer
	labl @                              			\ labels always start at character 0 of full-buffer = character 9 of line-buffer                                                                           
;
	full-buffer 9 + labl !					\ assign label since it is fixed

: eval-expr								\ evalute an expression using the FORTH interpreter
	expr @ expr-n @ evaluate
;

: eval-inst						( pass)
	inst @ inst-n @ 				( )		\ reference the instruction token 
	?dup 
	if 								\ check length
		evaluate				( )		
	else								\ nin length, need to consume stack parameters
		drop					( pass)	\ drop address
		drop					( )		\ drop pass variable
		0					( size)	\ size of zero
	then
;

: close-all
	fileid @ close-file drop 
	fileid-w1 @ close-file drop
	fileid-w2 @ close-file drop
	fileid-w3 @ close-file drop
;

: NNNN 						( n ---)
	0 <# # # # # #> type space  					\ output number in format 0000
;

: tab
	9 emit
;

\ Assembler functions that will be called by instructions
: pass1 						( size -- )		\ Program counter logic                                                                                                    
	labl-n @         
	if									\ Assign any label with current value of PC
		PC @                        	( size PC --)
		full-buffer labl-n @ 9 +    	( size PC "CONSTANT label")
		evaluate                    	( size)
	then	
	PC +!                           					\ increment the PC with the instruction size
;

: pass2				    		( opcode size --) 	\ Write out the machine langauge
	\ cr tab PC @ NNNN tab tab tab tab 
	?dup 
	if 									\ Ignore instructions of size zero
		cr tab tab tab tab tab
		dup PC +!				( opcode size --)	\ recalculate the PC for display purpose
		decimal							\ important to guarantee ASCII output file fomat
		qual @ qual-n @ evaluate    				\ implement ,RTS qualifier if applicable
		0 do			    					\ print and output bytes
			dup 0 <# [char] , hold #S #> fileid-w1 @ write-line 	( -- flag)
			if ." Error writing output file .coe" close-all abort then
			
			dup 0 <# #S #> fileid-w2 @ write-line 			( -- flag)
			if ." Error writing output file .txt" close-all abort then
		
			dup membuf ! membuf 1 fileid-w3 @ write-file
			if ." Error writing output file .bin" close-all abort then
		\	drop	
		    . space
		loop	
	then
;


: pass							( [opcode] size pass)
\ assembler pass
	if
		pass2
	else
		pass1
	then
;

\ Simple INSTRUCTION defining word
\ for opcodes that can be translated directly into machine code
\ handles size and opcode value
: INSTRUCTION                          		 
	CREATE						( size opcode --)		
		C, C,                       				\ compile the defined values
		
	DOES>						( pass -- [opcode] size)
		swap                        	( addr pass --)
		if                          				\ pass 2
			dup C@ swap 1+ c@       	( -- opcode size)
		else                        				\ pass 1
			dup C@ 4 * instruction-count + 1 swap +!		\ increment the instruction count
			1+ C@                   	( -- size)
		then
;

\ Simple instructions
1 0 INSTRUCTION _NOP
1 1 INSTRUCTION _DROP
1 2 INSTRUCTION _DUP
1 3 INSTRUCTION _?DUP
1 4 INSTRUCTION _SWAP
1 5 INSTRUCTION _OVER
1 6 INSTRUCTION _NIP
1 7 INSTRUCTION _ROT
1 8 INSTRUCTION _>R
1 9 INSTRUCTION _R@
1 9 INSTRUCTION _I		\ loop counter
1 10 INSTRUCTION _R>
1 11 INSTRUCTION _PSP@
1 12 INSTRUCTION _RSP@
1 13 INSTRUCTION _PSP!
1 14 INSTRUCTION _RSP!
1 15 INSTRUCTION _+
1 16 INSTRUCTION _-
1 17 INSTRUCTION _NEGATE
1 18 INSTRUCTION _1+
1 19 INSTRUCTION _1-
1 20 INSTRUCTION _2*
1 21 INSTRUCTION _2/
1 22 INSTRUCTION _MULTS
1 23 INSTRUCTION _MULTU
1 24 INSTRUCTION _ADDX
1 25 INSTRUCTION _SUBX
1 26 INSTRUCTION _DIVS
1 27 INSTRUCTION _DIVU
1 28 INSTRUCTION _=
1 29 INSTRUCTION _<>
1 30 INSTRUCTION _<
1 31 INSTRUCTION _>
1 32 INSTRUCTION _U<
1 33 INSTRUCTION _U>
1 34 INSTRUCTION _0=
1 34 INSTRUCTION _NOT	\ boolean logical NOT
1 35 INSTRUCTION _0<>
1 36 INSTRUCTION _0<
1 37 INSTRUCTION _0>
1 38 INSTRUCTION _FALSE
1 38 INSTRUCTION _ZERO	\ ZERO is equivalent to FALSE
1 39 INSTRUCTION _AND
1 40 INSTRUCTION _OR
1 41 INSTRUCTION _INVERT
1 42 INSTRUCTION _XOR
1 43 INSTRUCTION _LSL
1 44 INSTRUCTION _LSR
1 45 INSTRUCTION _XBYTE
1 46 INSTRUCTION _XWORD
1 47 INSTRUCTION _FETCH.L
1 48 INSTRUCTION _STORE.L
1 49 INSTRUCTION _FETCH.W
1 50 INSTRUCTION _STORE.W
1 51 INSTRUCTION _FETCH.B
1 52 INSTRUCTION _STORE.B
1 56 INSTRUCTION _JMP
1 57 INSTRUCTION _BSR
1 58 INSTRUCTION _JSR
1 59 INSTRUCTION _TRAP
1 60 INSTRUCTION _RETRAP
1 61 INSTRUCTION _RTI
1 63 INSTRUCTION _TEST
1 64 INSTRUCTION _RTS

\ Helper routines that break values into byte by byte on the stack

: push-long						( n -- n n n n)
	dup 255 and swap				( d nnnn)
	8 rshift dup 255 and swap			( d c nnnn)
	8 rshift dup 255 and swap			( d c b nnnn)
	8 rshift 255 and 				( d c b a)	
;

: push-word						( n -- n n)
	dup 255 and swap
	8 rshift 255 and 
;

: push-byte						( n -- n)
	255 and
;

: push-13bits						( n -- n n)
	dup 255 and swap
	8 rshift 63 and 							\ high byte limited to 5 bits only
;

\ Complex instructions
: _CALL						( pass -- [opcode] size) \ assumes a .W size address
	if									\ pass 2
		eval-expr >R 				( r: addr)		\ JSR
		58					( 58)		\ JSR 
		R> push-word				( 58 b a)	\ literal
		54					( 58 b a 54)	\ #.w
		4					( 58 b a 54 4)\ size
	else									\ pass 1		
		4							\ size
		\ update instruction count
		instruction-count 54 4 * + 1 swap +!
		instruction-count 58 4 * + 1 swap +!
	then
;

: _LOAD.L						( pass -- [opcode] size)
	if                              					\ pass 2
		eval-expr
		push-long							\ place on stack as 4 bytes
		55					( d c b a 55)		\ opcode
		5					( d c b a 55 5)	\ size                      
	else                            					\ pass 1
		5  		                   				\ size = 5
		\ update instruction count
		instruction-count 55 4 * + 1 swap +!
	then
;

: _#.L
	_LOAD.L
;

: _LOAD.W 						( pass-- [opcode] size)
	if									\ pass 2
		eval-expr
		push-word							\ place on stack as 2 bytes
		54
		3                       	
	else									\ pass 1
		\ update instruction count
		instruction-count 54 4 * + 1 swap +!
		3 
	then
;

: _#.W
	_LOAD.W
;

: _LOAD.B						( pass-- [opcode] size)
	if									\ pass 2
		eval-expr	
		push-byte							\ place on stack as a byte 
		53
		2                      	
	else									\ pass 1
		\ update instruction count
		instruction-count 53 4 * + 1 swap +!
		2 
	then
;

: _#.B
	_LOAD.B
;

: _BEQ							( pass-- [opcode] size)
	if									\ pass 2
		eval-expr				( nnnn)
		push-13bits
		128 or					( a b+128)	 	\ 128 = BEQ
		2					( a b+128 2) 		\ size = 2	
	else									\ pass 1
		\ update instruction count
		instruction-count 67 4 * + 1 swap +!
		2 								\ size = 2
	then
;

: _BRA							( pass-- [opcode] size)
	if									\ pass 2
		eval-expr	
		push-13bits
		192 or								\ 192 = BRA
		2	
	else									\ pass 1
		\ update instruction count
		instruction-count 66 4 * + 1 swap +!
		2 								\ size = 2
	then
;

\ Qualifier
: ,RTS							( opcode size -- opcode size)
  	swap
  	64 or									\ 64 = RTS
	swap
	\ update instruction count
	instruction-count 65 4 * + 1 swap +!
;

\ Macro directives for conditional flow
\ The assembler utilizes an array to hold program counter values and branch offsets for conditional statements
\ an IF statement on pass1 will store the current PC value in the next available space in the array, and place a pointer to that array location on the stack
\ a THEN statement on pass1 will reference an array location using the current TOS pointer and convert the PC value in the array into the required offset
\ an ELSE statement works like both an IF and a THEN, but the offset is to the statement following the ELSE
\ on pass 2 the stack is not used and each IF or ELSE statement simply steps through the array to find the appropriate offsets for BEQ or BRA.  THEN has no action in pass 2
\ the combination of an array with pointers held on the stack takes care of storing PC values and offsets for nested conditionals
\
: flow-pointer++					( -- pointer++)
	flow-pointer dup @				( addr pointer)	\ flow-pointer steps through the sequence of control statements
	dup cell + rot !				( pointer++) 		\ increment (by a long) and save
;

: save-PC						( pointer --)
\ save the PC into the flow array at the location of the current TOS pointer
	PC @ swap !
;

: make-BEQ						( pointer -- n1 n2)
\ take an offset from the flow array for a BEQ statement
		@					( offset) 
		16383 and							\ limit to 14 bits
		32768 or				( BEQ n)
		push-word				( n1 n2)
;

: make-BRA						( pointer -- n1 n2)
\ take an offset from the flow array for a BRA statement
		@					( offset) 
		16383 and							\ limit to 14 bits
		49152 or				( BRA n)
		push-word				( n1 n2)
;

: calc-fwd-offset					( pointer n --)		
\ calculate the offset based on the current PC and the saved PC in the array position given by the pointer at top of stack
\ n is added to the final offset
		swap					( n pointer)		
		dup @					( n pointer fromPC)	\ fromPC corresponding IF or ELSE statement
		PC @ 					( n pointer fromPC toPC)
		swap rel 				( n pointer offset)	\ calculate the offset
		rot +					( pointer final-offset)
		swap !								\ save the offset in the flow-array		
;		

: calc-rev-offset					( pointer n --)		
\ calculate the offset based on the current PC and the saved PC in the reverse direction
		swap					( n pointer)		
		dup @					( n pointer toPC)	\ fromPC corresponding IF or ELSE statement
		PC @ 					( n pointer toPC fromPC)
		rel 					( n pointer offset)	\ calculate the offset
		rot +					( pointer final-offset)
		swap !								\ save the offset in the flow-array		
;	

: _IF							( pass -- [pointer] [opcode] size)
	flow-pointer++ 				( pass pointer)
	swap						( pointer pass)
	if									\ pass 2
		make-BEQ				( n1 n2)
		2					( n1 n2 size)		\ instruction size
	else									\ pass 1
		\ update instruction count
		instruction-count 67 4 * + 1 swap +!
		dup
		save-PC				( pointer)
		2					( pointer size)
	then
;

: _WHILE
	_IF
;

: _THEN						( IF/ELSEpointer pass  --  size)
	if 0
	else									\ pass 1 
		0 					( pointer sub-offset)
		calc-fwd-offset						\ IF-THEN or ELSE-THEN branch
		0 					( size)								
	then
;

: _ELSE						( IFpointer pass  -- [ELSEpointer] size)	\ IFpointer is the pointer on the stack from the last IF statement
	flow-pointer++				( IFpointer pass ELSEpointer)		\ pArray is a pointer to next cell in the array			
	swap						( IFp ELSEp pass)
	if									\ pass 2 - similar to _IF
		make-BRA				( n1 n2)
		2					( n1 n2 size)		\ instruction size
	else									\ pass 1 - similar to _IF and _THEN
		\ update instruction count
		instruction-count 66 4 * + 1 swap +!
		dup
		save-PC				( IFp ELSEp)		\ save the current PC in the flow-array		
		swap 					( ELSEp IFp)
		2
		calc-fwd-offset						\ IF-ELSE branch to the statement after the ELSE
		2					( pArray size)
	then
;	
	
: _BEGIN						( pass -- [BEGINpointer] size)
	flow-pointer++				( pass pointer)
	swap
	if 0
	else									\ pass 1 	
		dup
		save-PC
		0					( pointer size)
	then
;

: _AGAIN						( BEGINpointer pass -- size)
	if									\ pass 2					
		make-BRA				( n1 n2)
		2					( n1 n2 size)
	else									\ pass 1
		\ update instruction count
		instruction-count 66 4 * + 1 swap +!
		0					( pointer sub-offset)
		calc-rev-offset			( pointer)								
		2					( size)
	then
;

: _REPEAT						( BEGINpointer WHILEpointer pass -- size)
	if									\ pass 2					
		make-BRA				( n1 n2)
		2					( n1 n2 size)
	else									\ pass 1	
		\ update instruction count
		instruction-count 66 4 * + 1 swap +!
		2					( pointer pointer sub-offset)	
		calc-fwd-offset						\ WHILE-REPEAT branch to after REPEAT
		0					( pointer sub-offset)	
		calc-rev-offset						\ REPEAT-BEGIN branch
		2					( size)
	then
;
		
: _UNTIL						( BEGINpointer pass -- size)
	if									\ pass 2					
		make-BEQ				( n1 n2)
		2					( n1 n2 size)					
	else									\ pass 1
		\ update instruction count
		instruction-count 67 4 * + 1 swap +!
		0					( pointer sub-offset)
		calc-rev-offset						\ UNTIL-BEGIN branch							
		2					( size)
	then
;

: _DO							( pass -- [pointer] size)
\ Forth code
\ 	( limit index)
\ swap	( index limit)
\ >R	( index R: limit)
\ >R	( R: limit index)
	flow-pointer++				( pass pointer)
	swap
	if									\ pass 2
		8 8 4 					( >R >R swap)
		3					( code.. size)
	else									\ pass 1
		dup					( pointer pointer)
		save-PC				( pointer)
		3					( pointer size)
	then
;

: _LOOP						( pointer pass -- size)
\ Forth code
\ 	( R: limit index)
\ R>	( index R: limit)
\ 1+	( index+ R: limit)
\ dup	( index+ index+ R: limit)
\ R@	( index+ index+ limit R: limit)
\ <	( index+ flag R: limit)
\ not  ( index+ flag' R: limit)
\ swap ( flag index+  R: limit)
\ >R   ( flag R: limit index+)
\ BEQ
\ R> R> drop drop ( -- R: --)
	if									\ pass 2
		>R
	       1 1 10 10
		R>					( code.. pointer)
		make-BEQ	
		8 4 34 30 9 2 18 10		
		14					( code.. size)
	else									\ pass 1
		\ update instruction count
		instruction-count 67 4 * + 1 swap +!
		-5					( pointer sub-offset)\ accounts for code in DO and LOOP
		calc-rev-offset			
		14					( size)
	then
;

: _+LOOP							\ KNOWN ISSUE - +LOOP only works with positive increments!
\ Forth code
\ R>	( flag1 n index R: limit)
\ +	( flag1 index+ R: limit)
\ dup	( flag1 index+ index+ R: limit)
\ R@	( flag1 index+ index+ limit R: limit)
\ <	( flag1 index+ flag R: limit)
\ not	( flag1 index+ flag2 R: limit)
\ swap ( flag1 flag2 index+  R: limit)
\ >R   ( flag1 flag2 R: limit index+)
\ BEQ
\ R> R> drop drop ( -- R: --)
	if									\ pass 2
		>R
		1 1 10 10	
		R>					( code.. pointer)
		make-BEQ	
		8 4 34 30 9 2 15 10
		14					( code.. size)
	else									\ pass 1
		\ update instruction count
		instruction-count 67 4 * + 1 swap +!
		-5					( pointer sub-offset)
		calc-rev-offset			
		14					( size)
	then
;

\ Assembler directives
: _CMD							( pass -- size)
\ execute the expression field in pass 1
	if	0
	else									\ pass 1 
		eval-expr
		0
	then
;	
	
: _EQU							( pass -- size)
	if    0
	else									\ pass 1
		eval-expr				( value)
		full-buffer labl-n @ 9 +    	( value "CONSTANT label")
		evaluate							\ assign value to label
		0 labl-n !							\ prevent label overwrite by pass 1 logic
		0					( size)
	then
;

: _DC.L						( pass -- size)
	if									\ pass 2
		eval-expr
		push-long
		4								\ size = 4
	else									\ pass 1
		4 								\ size = 4
	then 
;

: _DC.W						( pass -- size)
	if									\ pass 2
		eval-expr
		push-word
		2								\ size = 2
	else									\ pass 1
		2 								\ size = 2
	then 
;

: _DC.B						( pass -- size)
	if									\ pass 2
		eval-expr
		depth								\ size = number of bytes
	else									\ pass 1
		eval-expr
		depth 								\ determine the number of bytes
		dup >R
		0 do drop loop						\ remove data from stack this pass
		R>								\ size = number of bytes
	then 
;

: _DS.L						( pass -- size)
	if									\ pass 2
		eval-expr							\ number of longs to store
		4 *								\ convert to bytes
		dup >R							
		0 do 0 loop							\ 0 fill stack
		R>					( 0 0 0 ... size)
	else									\ pass 1
		eval-expr
		4 *
	then
;

: _DS.W						( pass -- size)
	if									\ pass 2
		eval-expr							\ number of words to store
		2 *								\ convert to bytes
		dup >R							
		0 do 0 loop							\ 0 fill stack
		R>					( 0 0 0 ... size)
	else									\ pass 1
		eval-expr
		2 *
	then
;

: _DS.B						( pass -- size)
	if									\ pass 2
		eval-expr							\ number of bytes to store
		dup >R							
		0 do 0 loop							\ 0 fill stack
		R>					( 0 0 0 ... size)
	else									\ pass 1
		eval-expr
	then
;

\ Utility functions

: scan-debug			    			( u pass)
	decimal							\ important to guarantee ASCII output file fomat
	0=
	if								\ pass 1
		space
		lineno dup @ 1+ dup . swap !			\ update and print line number	
		drop
	else								\ pass 2
		cr
		lineno dup @ 1+ dup . swap !  			\ update and print line number
		tab PC @ NNNN tab					\ print the PC				
		tab line-buffer swap type				\ echo the source line
	then
;

: whitespace                        		( c -- flag)
	dup 9 =                         		( c flag)   		\ tab
	swap 32 = or                    		( flag)     		\ space
;

: commentchar                       		( c -- flag)
	dup 59 =                            	( c flag)     	\ semi-colon
	swap 40 = or					( flag)		\ left parenthesis
;

: qualchar                          		( c -- flag)
	44 =                            		( flag)     		\ comma
;

\ Parser functions
: sub-tab
	\ remove tabs and substitute spaces
	0	( u 0)
	DO
		i			( i)
		line-buffer +		( addr)
		c@			( c)
		9 =
		IF
			32
			i
			line-buffer +
			c!
		THEN
	LOOP
;

: scan-line	                        		( u --)

	\ initialize token variables to empty
	0 labl-n !                      
	0 inst-n !
	0 qual-n !
	0 expr-n !

	\ add white space and comment marker to end of line to simplify state cases
	line-buffer + dup 32 swap !    		( line-buffer+u)    \ tab
	1+ dup 59 swap !               		( line-buffer+u+1)  \ semi-colon
	1+                             		( line-buffer+u+2)

	\ enter cycling state machine
	0 swap                          		( state line-buffer+u+2)    \ state <= label
	line-buffer                     		( state line-buffer+u+2 line-buffer)
	do									\ process each character in turn
	    \ i c@ emit                 					\ debug
		i c@ swap                   	( c state)
		case
		0 of                    					\ state = label
			dup whitespace
			if
				drop
				1 1             				\ state <= whitespace, next char
				i labl @ - labl-n ! 				\ save length of label
			else
				commentchar
				if
					0 65535				\ dummy case variable, exit loop
				else
					0 1             			\ same state, next char
				then
			then                		( state loop)
              endof

		1 of                    					\ state = whitespace after label
			whitespace
			if
				1 1              				\ same state, next char
			else
				2 0              				\ state <= instruction, same char
				i 1- dup inst !  				\ save start address of instruction
				95 swap c!       				\ prefix with _ character
			then
              endof

		2 of                    					\ state = instruction
			dup whitespace
			if
				drop
				3 1              				\ state <= whitespace, next char
				i inst @ - inst-n ! 				\ save length of instruction
			else
				qualchar
				if
					5 1         				\ state <= qualifier, next char
					i inst @ - inst-n ! 			\ save length of instruction
					i qual !           			\ save start address of qualifier including underscore
				else
					2 1         				\ same state, next char
				then
			then
              endof

		3 of                    					\ state = whitespace after instruction
			whitespace
			if
				3 1              				\ same state, next char
			else
				4 0              				\ state <= expression, same char
				i expr !         				\ save start address of expression
			then
              endof

		4 of                    					\ state = expression
			commentchar
			if
				i expr @ - expr-n ! 				\ save length of expression
				0 65535           				\ dummy case variable, exit loop
			else
				4 1               				\ same state, next char
			then
              endof

		5 of                    					\ state = qualifier
			whitespace
			if
				3 1                				\ state <= whitespace, next char
				i qual @ - qual-n !  			\ save length of qualifier
			else
				5 1                				\ same state, next char
			then
              endof

		endcase
	+loop
	drop                            					\ drop the case variable
;

: scan-src                          		( c-addr u)
	\ open source file
	r/o	open-file				( fileid ior)
	abort" Error opening source file" 		( fileid)
	fileid !
	
	\ open output files
	C" E:\N.I.G.E.-Machine\System\SRAM.bin" count w/o create-file
	abort" Error opening output file .bin"	( fileid-w)
	fileid-w3 !					( fileid-w)
	
	C" E:\N.I.G.E.-Machine\System\SRAM.txt" count w/o create-file
	abort" Error opening output file .txt"	( fileid-w)
	fileid-w2 !					( fileid-w)
	
	C" E:\N.I.G.E.-Machine\System\SRAM.coe" count w/o create-file
	abort" Error opening output file .coe"	( fileid-w)
	fileid-w1 !					( fileid-w)	
	S" memory_initialization_radix=10;" fileid-w1 @ write-line drop ( fileid-w)
	S" memory_initialization_vector=" fileid-w1 @ write-line drop 	 ( )
	
	\ clear instruction count memory
	instruction-count 268 erase
	
	\ make two passes pass 1 (i=0) and pass 2 (i=1)
	2 0 do
		\ initialize counters and pointers
		0 PC ! 
		0 lineno !	
		flow-table flow-pointer !	
		cr
		begin
			\ read a line from source file
			line-buffer 245 fileid @    		( line-buffer n fileid)
			read-line			        	( u flag ior)
			if
				." Error reading source file" close-all abort
			then				        	( u flag)
		while                           			( u)
			\ process the line from source file
			dup dup					( u u u)
			i scan-debug 					( u u)	\ log file
			sub-tab					( u )
			scan-line					\ scan the line into the token variables
			i eval-inst 					\ implement instruction according to pass
			i pass						\ assembler pass
		repeat				
		\ rewind file for second pass
		0 fileid @ reposition-file drop
	loop 
	\ final output, close files and remove locks
	S" 0;" fileid-w1 @ write-line drop ( )
	close-all
	cr
;

\ CPU N.I.G.E cross assembler
\ usage S" <path and filename>" asmx
: asmtop
;									\ dummy until MARKER is defined

: asmx
	scan-src
;

: stats								\ instruction count statistics
0 instruction-count dup 272 + swap 				\ 65 = ,RTS
cr									\ 66 = BRA
." inst" 9 emit ." count" cr					\ 67 = BEQ
do 
	dup . 1+ 9 emit I @ .
	cr
4 +loop
drop
;
	