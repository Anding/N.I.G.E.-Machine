\
\ tt.pfe	Tetris for terminals, redone in ANSI-Forth.
\		Written 05Apr94 by Dirk Uwe Zoller,
\			e-mail duz@roxi.rz.fht-mannheim.de.
\		Look&feel stolen from Mike Taylor's "TETRIS FOR TERMINALS"
\
\		Please copy and share this program, modify it for your system
\		and improve it as you like. But don't remove this notice.
\
\		Thank you.
\
\ Adapted for N.I.G.E by Ulrich Hoffmann <uho@xlerb.de> 2017-10-24


\ make a clean start: if forget-tt exists, remove tetris game first

marker (tetris)

: \needs ( <name> -- ) bl word find nip IF postpone \ THEN ; immediate

\needs forget-tt   (tetris)   marker forget-tt

forget-tt 


\ now define game. Use forget-tt if done playing.

marker forget-tt 

wordlist constant tetris

get-order tetris swap 1+ set-order 

tetris set-current


decimal

: 2constant ( x1 x2 -- )  Create , , does> ( -- x1 x2 ) 2@ ;

: d0= ( d -- f ) or 0= ;
: d= ( d1 d2 -- f )  d- d0= ;

: value ( x -- )  Create , Does> @ ;

: >body ( a1 -- a2 ) 10 + ;

: off ( a -- )  0 swap ! ;
: on  ( a -- ) -1 swap ! ;

: >= ( n1 n2 -- f ) < 0= ;

: blank ( c-addr u -- ) bl fill ;

27 constant #esc

: ansi-at-xy ( x y -- )
   #esc emit   [char] [ emit   
   <# u#s u#> type   [char] ; emit   
   <# u#s u#> type   [char] H emit ;

' csr-y 7 + constant 'csr-y
' csr-x 7 + constant 'csr-x

: nige-at-xy ( x y -- )  'csr-y !  'csr-x ! ;

 0 constant #black
 1 constant #grey
 2 constant #silver
 3 constant #white
 4 constant #red
 5 constant #yellow
 6 constant #green
 7 constant #cyan
 8 constant #blue
 9 constant #magenta
10 constant #maroon
11 constant #olive
12 constant #darkgreen
13 constant #teal
14 constant #amigablue
15 constant #purple
19 constant #brightyellow
48 constant #brightgreen

: nige-set-color ( c -- ) ink c! ;

: ansi-page ( -- ) #esc emit ." [2J" 0 0 ansi-at-xy ;


variable 'page       : page ( -- )        'page  @ execute ;
variable 'at-xy      : at-xy ( x y -- )   'at-xy @ execute ;
variable 'set-color  : set-color ( c -- ) 'set-color @ execute ;

forth-wordlist set-current

: ansi ( -- ) 
   ['] ansi-page  'page !
   ['] ansi-at-xy 'at-xy ! 
   ['] drop       'set-color ! ;

: nige ( -- )
   ['] cls 'page !
   ['] nige-at-xy 'at-xy !
   ['] nige-set-color 'set-color ! ;

tetris set-current

nige

: gamecolor ( -- ) #white set-color ;

\ Variables, constants

bl bl 2constant empty		\ an empty position
variable wiping			\ if true: wipe brick, else draw brick
 2 constant col0		\ position of the pit
 0 constant row0

10 constant wide		\ size of pit in brick positions
20 constant deep

char J	value left-key		\ customize if you don't like them
char K	value rot-key
char L	value right-key
bl	value drop-key
char P	value pause-key
12	value refresh-key
char Q	value quit-key

variable score
variable pieces
variable levels
variable delay

variable brow			\ where the brick is
variable bcol


\ stupid random number generator

variable seed

:  randomize   counter seed ! ;

hex 10450405 Constant generator decimal

: rnd  ( -- n )  seed @ generator um* drop 1+ dup seed ! ;

: random ( n -- 0..n-1 )  rnd um* nip ;

\ Access pairs of characters in memory:

: 2c@		dup 1+ c@ swap c@ ;
: 2c!		dup >r c! r> 1+ c! ;

: d<>		d= 0= ;


\ Drawing primitives:

: piece-color ( c -- )
	dup [char] # = IF drop #red           EXIT THEN
	dup [char] < = IF drop #brightyellow  EXIT THEN
	dup [char] { = IF drop #brightgreen   EXIT THEN
	dup [char] ( = IF drop #cyan          EXIT THEN
	dup [char] [ = IF drop #blue          EXIT THEN
	dup [char] @ = IF drop #magenta       EXIT THEN
	dup [char] % = IF drop #olive         EXIT THEN
	drop #white ;


: 2emit	( c1 c2 -- )	dup piece-color set-color   emit emit ;

: position	\ row col --- ; cursor to the position in the pit
		2* col0 + swap row0 + at-xy ;

: stone		\ c1 c2 --- ; draw or undraw these two characters
		wiping @ if  2drop 2 spaces  else  2emit  then ;


\ Define the pit where bricks fall into:

: def-pit	create	wide deep * 2* allot
		does>	rot wide * rot + 2* + ;

def-pit pit

: empty-pit	deep 0 do wide 0 do  empty j i pit 2c!
		loop loop ;


\ Displaying:

: draw-bottom	\ --- ; redraw the bottom of the pit
		deep -1 position
		[char] + dup stone
		wide 0 do  [char] = dup stone  loop
		[char] + dup stone ;

: draw-frame	\ --- ; draw the border of the pit
		deep 0 do
		    i -1   position [char] | dup stone
		    i wide position [char] | dup stone
		loop  draw-bottom ;

: bottom-msg	\ addr cnt --- ; output a message in the bottom of the pit
		deep over 2/ wide swap - 2/ position gamecolor type ;

: draw-line	\ line ---
		dup 0 position  wide 0 do  dup i pit 2c@ 2emit  loop  drop ;

: draw-pit	\ --- ; draw the contents of the pit
		deep 0 do  i draw-line  loop ;

: show-key	\ char --- ; visualization of that character
		dup bl <
		if  [char] @ or  [char] ^ emit  emit  space
		else  [char] ` emit  emit  [char] ' emit
		then ;

: show-help	\ --- ; display some explanations
		30  1 at-xy ." ***** T E T R I S *****"
		30  2 at-xy ." ======================="
		30  4 at-xy ." Use keys:"
		32  5 at-xy left-key	show-key ."  Move left"
		32  6 at-xy rot-key	show-key ."  Rotate"
		32  7 at-xy right-key	show-key ."  Move right"
		32  8 at-xy drop-key	show-key ."  Drop"
		32  9 at-xy pause-key	show-key ."  Pause"
		32 10 at-xy refresh-key	show-key ."  Refresh"
		32 11 at-xy quit-key	show-key ."  Quit"
		32 13 at-xy ." -> "
		30 16 at-xy ." Score:"
		30 17 at-xy ." Pieces:"
		30 18 at-xy ." Levels:"
		 0 22 at-xy ."  ==== This program was written 1994 in pure dpANS Forth by Dirk Uwe Zoller ===="
		 0 23 at-xy ."  ==== Ported to N.I.G.E, ANSI terminal & color support by Ulrich Hoffmann  ===="
		 0 24 at-xy ."  =================== Copy it, port it, play it, enjoy it! =====================" ;

: update-score	\ --- ; display current score
        gamecolor
		38 16 at-xy score @ 3 .r
		38 17 at-xy pieces @ 3 .r
		38 18 at-xy levels @ 3 .r ;

: refresh	\ --- ; redraw everything on screen
		page gamecolor draw-frame draw-pit show-help update-score ;


\ Define shapes of bricks:

: def-brick	create does>	rot 4 * rot + 2* + ;

: _" ( ccc" -- )  [char] " parse  dup >r here swap move r> allot ;

def-brick brick1	
			_"         "
			_" ######  "
			_"   ##    "
			_"         "

def-brick brick2	
			_"         "
			_" <><><><>"
			_"         "
			_"         "

def-brick brick3	
			_"         "
			_"   {}{}{}"
			_"   {}    "
			_"         "

def-brick brick4	
			_"         "
			_" ()()()  "
			_"     ()  "
			_"         "

def-brick brick5	
			_"         "
			_"   [][]  "
			_"   [][]  "
			_"         "

def-brick brick6	
			_"         "
			_" @@@@    "
			_"   @@@@  "
			_"         "

def-brick brick7
			_"         "
			_"   %%%%  "
			_" %%%%    "
			_"         "

\ this brick is actually in use:

def-brick brick 0 c,
			_"         "
			_"         "
			_"         "
			_"         "

def-brick scratch 0 c,
			_"         "
			_"         "
			_"         "
			_"         "

create bricks	' brick1 ,  ' brick2 ,  ' brick3 ,  ' brick4 ,
		' brick5 ,  ' brick6 ,  ' brick7 ,

create brick-val 1 c, 2 c, 3 c, 3 c, 4 c, 5 c, 5 c,

: is-brick	\ brick --- ; activate a shape of brick
		>body ['] brick >body 32 move ;

: new-brick	\ --- ; select a new brick by random, count it
		1 pieces +!  7 random
		bricks over cells + @ is-brick
		brick-val swap chars + c@ score +! ;

: rotleft
        4 0 do 4 0 do
		    j i brick 2c@  3 i - j scratch 2c!
		loop loop
		['] scratch is-brick ;

: rotright
        4 0 do 4 0 do
		    j i brick 2c@  i 3 j - scratch 2c!
		loop loop
		['] scratch is-brick ;

: draw-brick	\ row col ---
		4 0 do 4 0 do
		    j i brick 2c@  empty d<>
		    if  over j + over i +  position
			j i brick 2c@  stone
		    then
		loop loop  2drop ;

: show-brick	wiping off draw-brick ;
: hide-brick	wiping on  draw-brick ;

: put-brick	\ row col --- ; put the brick into the pit
		4 0 do 4 0 do
		    j i brick 2c@  empty d<>
		    if  over j +  over i +  pit
			j i brick 2c@  rot 2c!
		    then
		loop loop  2drop ;

: remove-brick	\ row col --- ; remove the brick from that position
		4 0 do 4 0 do
		    j i brick 2c@  empty d<>
		    if  over j + over i + pit empty rot 2c!  then
		loop loop  2drop ;

: test-brick	\ row col --- flag ; could the brick be there?
		4 0 do 4 0 do
		    j i brick 2c@ empty d<>
		    if  over j +  over i +
			over dup 0< swap deep >= or
			over dup 0< swap wide >= or
			2swap pit 2c@  empty d<>
			or or if  unloop unloop 2drop false  exit  then
		    then
		loop loop  2drop true ;

: move-brick	\ rows cols --- flag ; try to move the brick
		brow @ bcol @ remove-brick
		swap brow @ + swap bcol @ + 2dup test-brick
		if  brow @ bcol @ hide-brick
		    2dup bcol ! brow !  2dup show-brick put-brick  true
		else  2drop brow @ bcol @ put-brick  false
		then ;

: rotate-brick	\ flag --- flag ; left/right, success
		brow @ bcol @ remove-brick
		dup if  rotright  else  rotleft  then
		brow @ bcol @ test-brick
		over if  rotleft  else  rotright  then
		if  brow @ bcol @ hide-brick
		    if  rotright  else  rotleft  then
		    brow @ bcol @ put-brick
		    brow @ bcol @ show-brick  true
		else  drop false  then ;

: insert-brick	\ row col --- flag ; introduce a new brick
		2dup test-brick
		if  2dup bcol ! brow !
		    2dup put-brick  draw-brick  true
		else  2drop false  then ;

: drop-brick	\ --- ; move brick down fast
		begin  1 0 move-brick 0=  until ;

: move-line	\ from to ---
		over 0 pit  over 0 pit  wide 2*  move  draw-line
		dup 0 pit  wide 2*  blank  draw-line ;

: line-full	\ line-no --- flag
		true  wide 0
		do  over i pit 2c@ empty d=
		    if  drop false  leave  then
		loop nip ;

: remove-lines	\ ---
		deep deep
		begin
		    swap
		    begin  1- dup 0< if  2drop exit  then  dup line-full
		    while  1 levels +!  10 score +!  repeat
		    swap 1-
		    2dup <> if  2dup move-line  then
		again ;

: to-upper	\ char --- char ; convert to upper case
    dup [char] a [char] z 1+ within if
	bl -
    then ;

: interaction	\ --- flag
		case  key to-upper
		    left-key	of  0 -1 move-brick drop  endof
		    right-key	of  0  1 move-brick drop  endof
		    rot-key	of  0 rotate-brick drop  endof
		    drop-key	of  drop-brick  endof
		    pause-key	of  S"  paused " bottom-msg  key drop
				    draw-bottom  endof
		    refresh-key	of  refresh  endof
		    quit-key	of  false exit  endof
		endcase  true ;

: initialize	\ --- ; prepare for playing
		randomize empty-pit refresh
		0 score !  0 pieces !  0 levels !  100 delay ! ;

: adjust-delay	\ --- ; make it faster with increasing score
		levels @
		dup  50 < if  100 over -  else
		dup 100 < if   62 over 4 / -  else
		dup 500 < if   31 over 16 / -  else  0  then then then
		delay !  drop ;

: play-game	\ --- ; play one tetris game
		begin
		    new-brick
		    -1 3 insert-brick
		while
		    begin  4 0
			do  35 13 at-xy
			    delay @ ms key?
			    if interaction 0=
				if  unloop exit  then
			    then
			loop
			1 0 move-brick  0=
		    until
		    remove-lines
		    update-score
		    adjust-delay
		repeat ;

forth-wordlist set-current

: tt		\ --- ; play the tetris game
		initialize
		s"  Press any key " bottom-msg key drop draw-bottom
		begin
		    play-game
		    s"  Again? " bottom-msg key to-upper [char] Y =
		while  initialize  repeat
		0 24 at-xy cr ;


get-order nip 1- set-order

cr .( Tetris on N.I.G.E. )
cr .( Usage: )
cr .(    nige tt     play on N.I.G.E. vga )
cr .(    ansi tt     play on serial line )
cr .(    forget-tt   done playing, remove game from dictionary )
cr 
cr .( May the Forth be with you. Have fun. )
cr
