260236 constant regbase		\ hardware register base
260108 constant regm		\ 

: set
	create ( visible front sync whole <NAME> --)
	, , , , 
	does> ( -- visible front sync back whole) 
	dup 3 cells +  
	do i @ -1 cells +loop
; 

: prep { visible front sync whole -- a b c d}
	\ use NIGE local variables
	sync 1- ( a)
	whole front - visible - 1- ( b)
	whole front - 1- ( c)
	whole 1- ( d)
;

640 16 96 800 set H1
480 9 2 525 set V1
800 37 128 1056 set H2
600 0 4 628 set V2
1024 22 136 1328 set H3
768 3 6 806 set V3
1920 529 78 2642 set H4
1080 3 5 1125 set V4


: H! ( a b c d --) 
	regbase dup 3 cells +
	do i ! -1 cells +loop
;

: V! ( a b c d --)
	regbase dup 4 cells +  swap 7 cells +
 	do i ! -1 cells +loop
;

: (VGA) ( H V mode --)
	cls
	regm dup @ 248 and rot or swap ! ( H V)
	prep V!
	prep H!
	scrset
;
	

: VGA1 H1 V1 1 (VGA) ;
: VGA2 H2 V2 2 (VGA) ;
: VGA3 H3 V3 3 (VGA) ;
: VGA4 H4 V4 4 (VGA) ;

: 4dup ( a b c d -- a b c d a b c d)
	2dup >R >R
	2swap
	2dup >R >R
	2swap
	R> R> R> R>
;

: update ( a b c d K -- a b c d)
	CASE
		[char] A OF >R >R >R 1+ R> R> R> ENDOF
		[char] a OF >R >R >R 1- R> R> R> ENDOF
		[char] B OF >R >R 1+ R> R> ENDOF
		[char] b OF >R >R 1- R> R> ENDOF
		[char] C OF >R 1+ R> ENDOF
		[char] c OF >R 1- R> ENDOF
		[char] D OF 1+ ENDOF
		[char] d OF 1- ENDOF
	ENDCASE
;

: Htest ( a b c d -- a b c d)
	BEGIN
		key
		dup [char] x = not
	WHILE
		update
		4dup prep H!
	REPEAT
	drop . . . .
;

: Vtest ( a b c d -- a b c d)
	BEGIN
		key
		dup [char] x = not
	WHILE
		update
		4dup prep V!
	REPEAT
	drop . . . .
; 
