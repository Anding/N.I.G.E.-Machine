\ EDITOR functions

\ line data structure
\ 00 FWD reference
\ 04 BCK reference
\ 08 length of line (excluding line endings, -1 for list headers)
\ 12 line number (starting at 1)
\ 16 text (128 bytes reserved)

: ED.linelen
	8 + 
;

: ED.linenum
	12 +
;

: ED.linetxt
	16 +
;

24 buffer: ED.filehead		\ list header for the file being edited
variable ED.fileid			\ FORTH file access fileid for the file being edited
variable ED.cursorline		\ pointer to the list node where the cursor is currently
variable ED.cursorcol		\ current column position of the cursor
variable ED.linecount		\ number of lines in the current file
variable ED.scr1stlinenum		\ first line number of the visible screen
variable ED.scrlines			\ number of lines available in the screen
variable ED.buf1stlinenum		\ first line number of the rendered text buffer
variable ED.buflines			\ number of lines available in the buffer
variable ED.refreshline		\ pointer to the list node of the first line to be refreshed for the buffer
variable ED.refreshcount		\ number of lines to be refreshed in the buffer (-1 = full buffer refill)
variable ED.lastscreenplace		\ restore the screen here

\ allocate memory for editor's own screen buffer
192 ed.buflines !
ed.buflines @ 128 * 2* allocate drop				\ 128 maximum number of columns
constant ED.SCREENBASE


\ Position the cursor at the first character of the first line
: ED.home
	ED.filehead LIST.fwd ED.cursorline !
	0 ED.cursorcol !
;

\ Position the cursor at the first character of the last line
: ED.end
	ED.filehead LIST.bck LIST.bck ED.cursorline !
	0 ED.cursorcol !
;

\ Parse the file into the editor memory structure
: ED.load ( --)
	ED.filehead dup LIST.init
	dup ED.linelen -1 swap !			\ store -1 flag in header and footer
	dup 20 + -1 swap !
	0 >R							( last-node R:linenum)
	BEGIN							( last-node R:linenum)
		144 allocate 					( last-node addr ior R:linenum)
		IF c" ALLOCATE failed" THROW THEN		
		dup ED.linetxt				( last-node addr text-addr R:linenum)
		128 ED.fileid @ READ-LINE 			( last-node addr u2 flag ior R:linenum)
		IF c" READ-LINE failed" THROW THEN		
	WHILE
		over ED.linelen !			\ store the length u2, excluding delimiters
		R> 1+ dup >R					( last-node addr newlinenum R:newlinenum)
		over ED.linenum !			\ store the line number
		dup rot LIST.ins				( node)		
	REPEAT
		drop						( last-node addr R:linenum)
		R@ 0= IF				\ it was an empty file
			dup ED.linelen 0 swap !		( last-node newnode R:linenum)
			dup ED.linenum 1 swap !		( last-node newnode R:linenum)
			swap LIST.ins			\ file to edit will comprise a single blank line
		ELSE
			free drop drop		\ free the allocated but unneeded line
		THEN 	
		R> ED.linecount !
		
	ED.home
;
	
\ Close and exit without saving changes
: ED.exit
	ED.filehead LIST.FWD
	BEGIN	
		dup ED.linelen @ -1 <>
	WHILE
		dup LIST.FWD swap
		dup LIST.rem free drop
	REPEAT
	drop
	ED.fileid @ CLOSE-FILE drop
	VWAIT
	1 COLORMODE
	ED.lastscreenplace @ SCREENPLACE !
;

\ Save the modified file
: ED.save
	ED.fileid @ >R				( R:fileid)
	0 0 R@ REPOSITION-FILE drop
	ED.filehead LIST.FWD
	BEGIN
		dup ED.linelen @ -1 <>			\ look for last node with value of -1
	WHILE
		dup ED.linetxt			( node text-addr R:fileid)
		over ED.linelen @			( node text-addr u R:fileid)
		ED.fileid @ WRITE-LINE drop		( node R:fileid)
		LIST.FWD				( next-node R:fileid)
	REPEAT
	drop	
	R@ file-position drop
	R@ resize-file drop					\ resize file to bytes just written
	R> flush-file drop					\ force SD card refresh
;

\ Renumber all lines from this node
: ED.renumber ( node --)
	dup ED.linenum @ swap			( linenumber node)
	LIST.FWD
	BEGIN	
		dup ED.linelen @ -1 <>
	WHILE
		swap 1+ swap				( linenumber+1 node)
		over over				( linenumber+1 node linenumber+1 node)
		ED.linenum !
		LIST.FWD				( linenumber+1 nextnode)
	REPEAT
	drop ED.linecount !
;

\ Implement a carriage return press
: ED.carriage-return
	\ create a new line and after the old line
	144 allocate IF c" Allocate failed" THROW THEN	( addr)
	ED.cursorline @ over over LIST.ins		( new-line old-line)
	\ copy the characters following the cursor to the new line
	dup ED.linelen @ ED.cursorcol @ - >R	( new-line old-line R:count)
	over over ED.linetxt ED.cursorcol @ +	( new-line old-line new-line addr-src)
	swap ED.linetxt				( new-line old-line addr-src addr-dest)
	R@ MOVE					( new-line old-line R:count)
	\ set the sizes of the old and new lines
	ED.cursorcol @ swap ED.linelen !		( new-line R:count)
	R> swap ED.linelen !				( --)
	\ renumber the lines forward
	ED.cursorline @ dup ED.renumber			( --)
	\ update the line and column position
	LIST.fwd ED.cursorline !	
	0 ED.cursorcol !
	\ set the refresh requirements
	ED.cursorline @ LIST.bck ED.refreshline ! -1 ED.refreshcount !	\ redraw the buffer from the first changed line
;

\ Implement a keypress (insert mode)
: ED.insert ( char --)
	ED.cursorline @ 				( char line)
	dup ED.linelen @				( char line linelen)
	dup 128 < IF					\ check within maximum line length				
		ED.cursorcol @ -				( char line len)
		?dup IF					\ there are characters to be copies
			over ED.linetxt ED.cursorcol @ +	( char line len addr-src)
			dup 1+ 				( char line len addr-src addr-dest)
			rot move				( char line)
		THEN
		\ insert the new character
		swap over ED.linetxt ED.cursorcol @ + c!	( line)
		\ increment line length
		ED.linelen 1 swap +!				( )			
		\ move cursor right
		1 ED.cursorcol +!		
		\ set refresh requirements
		1 ED.refreshcount ! ED.cursorline @ ED.refreshline !
	THEN
;

\ Helper function to regulate cursor up and down
: ED.cursorupdown ( node new-node --)
	dup ED.linelen @ dup -1 =			\ check for an end node
	IF
		drop drop				\ end node - revert cursor
	ELSE	
		ED.cursorcol dup >R @ MIN R> !		\ cursor must be within the line
		nip
	THEN
	ED.cursorline !					\ save the current node to ED.cursorline
;

\ Implement cursor up
: ED.cursorup
	ED.cursorline @ dup
	LIST.BCK					( node prior-node)
	ED.cursorupdown
;
	
\ Implement cursor down
: ED.cursordown
	ED.cursorline @ dup
	LIST.FWD					( node next-node)
	ED.cursorupdown
;

\ Move forward throught a number of lines
: ED.rollfwd		( node count  -- newnode)
	0 DO
		LIST.FWD
		dup ED.linelen @ -1 = IF
			LIST.BCK
			LEAVE
		THEN
	LOOP
;

\ Move back through a number of lines
: ED.rollbck		( node count  -- newnode)
	0 DO
		LIST.BCK
		dup ED.linelen @ -1 = IF
			LIST.FWD
			LEAVE
		THEN
	LOOP
;

\ Implement Page Up keypress
: ED.pageup
	ED.cursorline @
	dup 12 ED.rollbck		\ 12 lines back
	ED.cursorupdown
;

\ Implement Page Down keypress
: ED.pagedown
	ED.cursorline @
	dup 12 ED.rollfwd		\ 12 lines forward
	ED.cursorupdown
;

\ Helper function to regulate cursor left and right
: ED.cursorleftright ( node new-node  -- True | False)
	dup ED.linelen @ -1 =			\ check for an end node
	IF
		drop					\ remove new-node
		false					\ roll failed
	ELSE
		nip					\ remove node
		true					\ roll suceeded
	THEN
	swap ED.cursorline !				\ save the current node to ED.cursorline
;

\ Implement cursor right
: ED.cursorright
	ED.cursorcol @ 1+ dup
	ED.cursorline @ ED.linelen @ > not IF	\ check within length of current line	
		ED.cursorcol !				\ move right
	ELSE						\ roll to next line
		drop
		ED.cursorline @ dup LIST.FWD
		ED.cursorleftright
		IF					\ roll sucessful
			0 ED.cursorcol !			\ cursor at start of line
		THEN
	THEN
;

\ Implement cursor left
: ED.cursorleft
	ED.cursorcol @ 1- dup
	0< not IF					\ check within current line
		ED.cursorcol !			\ move left
	ELSE						\ roll to previous line
		drop
		ED.cursorline @ dup LIST.BCK
		ED.cursorleftright			
		IF					\ roll sucessful, cursor at end of line
			ED.cursorline @ ED.linelen @ 	\ current line length
			ED.cursorcol !			\ cursor within current line but not left of 0
		THEN
	THEN
;

\ Implement backspace
: ED.backspace
	ED.cursorcol @ ?dup 0=
	IF	\ delete a line						
		ED.cursorline @ dup LIST.BCK dup ED.linelen @ 	( line2 line1 length1) 
		dup -1 <>
		IF	\ not top of file			( line2 line1 length1 flag) 
			\ reposition the cursor
			dup ED.cursorcol !			( line2 line1 length1)
			\ calculate src, dest, len and move
			dup 128 swap - >R			( line2 line1 length1 R: sparelength1)
			rot dup ED.linelen @			( line1 length1 line2 length2 R: sparelength1)
			R> min >R				( line1 length1 line2 R: movelen)
			-rot					( line2 line1 length1 R: movelen)
			over ED.linetxt + >R			( line2 line1 R: movelen addr-dest)
			over ED.linetxt >R			( line2 line1 R: movelen addr-dest addr-src)
			R> R> R@ move				( line2 line1 R: movelen)
			\ update line1 length
			R> over ED.linelen +!		( line2 line1)
			\ update pointer to current line
			ED.cursorline !			( line2)
			\ remove line2
			dup LIST.rem				( line2)
			free drop	
			\ renumber lines forward
			ED.cursorline @ ED.renumber			
		ELSE
			drop drop drop
		THEN
		\ set refresh requirements, noting that all following lines need to be redrawn
		ED.cursorline @ ED.refreshline ! -1 ED.refreshcount !
	ELSE	\ delete a character				( column)
		ED.cursorline @ swap				( line column)
		\ count characters right of the cursor
		over ED.linelen @ over - >R			( line column R: copylen)
		over ED.linetxt +				( line addr-src R: copylen)
		dup 1-						( line addr-src addr-dest R: copylen)
		\ copy text back one byte
		R> move					( line)
		ED.linelen -1 swap +!			\ decrement line length
		ED.cursorcol -1 swap +!			\ cursor back
		\ set refresh requirements
		ED.cursorline @ ED.refreshline ! 1 ED.refreshcount !
	THEN
;

\ Implemement delete
: ED.delete
	ED.cursorright
	ED.backspace
;

: ED.cursoraddr ( -- addr, the address of the cursor within the buffer)
	ed.cursorline @ 0 over ED.linetxt				( line col start)
	ED.cursorcol @ over + swap					( line col finish start)
	?DO
		I C@ 9 = IF 						( line col) 
			TAB @ over over 				( line col tab col tab)
			/MOD drop -					( line col step)
		ELSE 
			1						( line col step)			 
		THEN 
		+							( line col')
	LOOP								( line col)
	COLS C@ 1- MIN						( line col)				\ avoid screen overrun
	swap ed.linenum @ ed.buf1stlinenum @ - COLS C@ * 	( col offsetForLines)
	+ 2* ED.SCREENBASE +						( addr)
;

: ED.drawcursor
	ED.cursoraddr
	dup w@
	255 and 10 16 * 2 + 256 * or				\ cursorcolor: background = 10, textcolor = 2
	swap w!
;

: ED.undrawcursor
	ED.cursoraddr
	dup w@
	255 and 2 256 * or						\ textcolor = 2
	swap w!
;

\ paste a character into the buffer and prepare for the next
: ED.refreshchar ( addr c - next-addr)
	2 256 * or							\ textcolor = 2
	over w!
	1+ 1+
;
 
 : ED.refresh ( line address --)
	swap 0 							( dest line col) 			\ start at column 0
	over ED.linetxt						( dest line col src)
	rot ED.linelen @						( dest col src len)
	over + swap							( dest col finish start)
	?DO								( dest col)	
		I C@							( dest col char)
		dup 9 = IF											\ tab character
			drop TAB @ over over 			( dest col tab col tab)
			/MOD drop - 32 swap				( dest col space count)
		ELSE
			1						( dest col char count)
		THEN
		0 DO							( dest col char)
			swap 1+					( dest char col')		
			dup COLS C@ 1- < IF				( dest char col' )			\ within the line
				swap rot over				( col' char dest char)
				2 256 * or				( col' char dest char+color)	\ textcolor = 2
				over w!				( col' char dest)
				1+ 1+					( col' char dest')
				-rot					( dest' col' char)
			ELSE
				swap					( dest col' char)
			THEN						
		LOOP	
		drop							( dest' col')
	LOOP
	drop drop							( --)
;
 	
 \ refresh the buffer using the currently set refresh parameters
: ED.refreshbuffer ( --)
	ED.refreshcount @ ?dup
	IF
		dup -1 = IF					\ buffer from changed line until end
			drop
			ED.refreshline @ dup
			ED.linenum @ ED.buf1stlinenum @ -		( firstline unchangedlines)
			ED.buflines @ swap -				( firstline linecount)
		ELSE	
			ED.refreshline @ swap			( first-line linecount)
			VWAIT					\ reduce flicker
		THEN
		\ calculate the starting address in the buffer
		over ED.linenum @ ED.buf1stlinenum @ -		( first-line linecount offsetlines)
		COLS C@ 2* *						( first-line linecount offsetbytes)
		ED.SCREENBASE + swap					( first-line address linecount)
		\ clear the lines to be refreshed
		over over COLS C@ 2* * 0 fill
		\ render text
		0 DO
			over ED.linelen @ -1 = IF LEAVE THEN 	\ end of file or empty file
			over over ED.refresh				( line address)
			COLS C@ 2* +					( line next-address)
			swap LIST.FWD swap				( next-line next-address)
		LOOP
		drop drop
	THEN
;
 
\ Mainloop
: ED.mainloop
	SCREENPLACE @ ED.lastscreenplace !
	1 ED.buf1stlinenum !
	1 ED.scr1stlinenum !	
	ROWS C@ ED.scrlines ! 	
	ED.cursorline @ ED.refreshline !
	-1 ED.refreshcount !
	VWAIT 0 COLORMODE	
	
	BEGIN
		ED.refreshbuffer 
		ED.scr1stlinenum @ ED.buf1stlinenum @ - 
		COLS C@ 2* * ED.SCREENBASE + SCREENPLACE !
		ED.drawcursor
	
		KEY 
		0 ED.refreshcount !
		ED.undrawcursor					\ undraw cursor at current cursor position
		CASE
			  4 OF ED.cursorup ENDOF
			  5 OF ED.cursordown ENDOF
			  6 OF ED.cursorleft ENDOF
			  7 OF ED.cursorright ENDOF
			 29 OF ED.pageup ENDOF
			 30 OF ED.pagedown ENDOF
			 12 OF ED.home ENDOF
			  3 OF ED.end ENDOF
			127 OF ED.delete ENDOF
			  8 OF ED.backspace ENDOF
			 10 OF ED.carriage-return ENDOF
			 27 OF ED.EXIT EXIT ENDOF			\ Escape = exit without save
			 14 OF ED.SAVE ED.EXIT EXIT ENDOF		\ F1 = save and exit
			 15 OF ED.SAVE ENDOF				\ F2 = save and continue
			dup ED.insert		\ remember dup test variable consumed by ENDCASE!				
		ENDCASE

		\ check the position of the cursor relative to the buffer
		ED.cursorline @ ED.linenum @				( cursorlinenum)
		dup ED.buf1stlinenum @ < 					( cursorlinenum flag)
		IF								( cursorlinenum)
			drop
			ED.cursorline @ 15 ED.rollbck		\ move buffer to start 15 lines above the current cursor
			dup ED.linenum @ ED.buf1stlinenum !	
			ED.refreshline !				\ flag a full redraw of the buffer
			-1 ED.refreshcount !
		ELSE ED.buf1stlinenum @ ED.buflines @ + < not 
			IF				
				ED.cursorline @ 15 ED.rollfwd	\ move buffer to start 15 lines below the current cursor
				ED.buflines @ 1- ED.rollbck
				dup ED.linenum @ ED.buf1stlinenum !
				ED.refreshline !			\ flag a full redraw of the buffer
				-1 ED.refreshcount !
			THEN
		THEN
		
		\ check the position of the cursor relative to the screen
		ED.cursorline @ ED.linenum @ dup				( cursorlinenum cursorlinenum)
		ED.scr1stlinenum @ < 
		IF								( cursorlinenum)
			ED.scr1stlinenum !							\ scroll the screen up
		ELSE dup ED.scr1stlinenum @ ED.scrlines @ + < not 
			IF							( cursorlinenum)
				ED.scrlines @ - 1+ ED.scr1stlinenum ! 			\ scroll the screen down
			ELSE
				drop
			THEN
		THEN	
		

	AGAIN
;

\ Open an existing file and commence editing
: EDIT ( "filename"  --)  
	32 WORD COUNT		( c-addr n)
	R/W OPEN-FILE IF c" OPEN-FILE failed" THROW THEN
	ED.fileid !
	ED.load ED.mainloop
;

\ Open a new file and commence editing
: EDITNEW ( "filename" --)
	32 WORD COUNT		( c-addr n)
	R/W CREATE-FILE IF c" CREATE-FILE failed" THROW THEN
	ED.fileid !
	ED.load ED.mainloop
;	

variable printfile.ID
128 buffer: printfile.buf

: {print-file} ( -- inner routine for print-file)
	CR
	2 ink c!
	0			\ line count
	BEGIN
		printfile.buf dup 128 printfile.ID @ read-line
		IF c" read-line failed" THROW THEN
	WHILE
		type CR
		1+				\ update the line count
		dup rows c@ /mod drop
		0= IF key drop THEN	\ screenfull - wait for a keypress to continue	
	REPEAT
	drop drop		\ drop address and length
	. ." lines"
;

\ Display the contents of a file
: print-file ( c-addr u -- display the contents of a file)
	R/W open-file 
	IF c" open-file failed" THROW THEN
	printfile.ID !
	
	['] {print-file} catch 		\ clean-up in success or fail case
	printfile.ID @ close-file drop 
	6 ink c!		\ reset ink color
	IF THROW THEN
;

\ Display the contents of a file
: printout ( "FILENAME" -- display the contents of a file)
	32 WORD COUNT
	print-file 
;
