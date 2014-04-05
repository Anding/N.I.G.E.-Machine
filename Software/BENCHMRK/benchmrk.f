DECIMAL

\ *************************************
\ Let's measure the generated code size
\ *************************************

here CONSTANT start-here

\ ************************************
\ FORTH, Inc.  32 Bit Benchmark Source
\ ************************************

CELL NEGATE CONSTANT -CELL

\ ***********************
\ Benchmark support words
\ ***********************

\ column positions
40 constant time-pos
50 constant iter-pos
60 constant each-pos
70 constant extra-pos

: .HEADER	\ -- ; display test header
  xcr ." Test time including overhead"
  time-pos 3 + >pos  ." ms"
  iter-pos >pos ." # tests"
  each-pos >pos  ." us each"
;

variable ms-elapsed	\ elapsed time for one test
variable ms-total	\ cumulative time for a series of tests

: TIMER ( ms iterations -- )
  >r                                    \ number of iterations
  counter swap -                        \ elapsed time in ms
  dup ms-elapsed !			\ save for later
  dup ms-total +!			\ accumulate in series
  time-pos >pos  dup 5 .r
  iter-pos >pos  r@ .
  r@ 1 >
  if
    each-pos >pos
    1000 r> */ 5 .r
  else
    drop  r> drop
  then
;

: .ann		\ -- ; banner announcment
  XCR  ;

: [$ 		\ -- ms
  COUNTER ;

: [[$$		\ -- ; initialises a set of tests
  0 ms-total !
;

\ $]  is the suffix to a testing word.  It takes the fast ticks
\    timer value and calculates the elapsed time.  It does do
\    some display words before calculating the time, but it is
\    assumed that this will take minimal time to execute.
: $] 		( ms n -- )   TIMER ;

: $$]]		\ iterations --
  >r                                    \ number of iterations
  ms-total @
  time-pos >pos  dup 5 .r
  iter-pos >pos  r@ .
  r@ 1 >
  if
    each-pos >pos
    1000000 r> */ 5 .r
  else
    drop  r> drop
  then
;


\ ******
\ Arrays
\ ******


\ ****************************
\ Basic FORTH, Inc. Benchmarks
\ ****************************
\ This series of tests analyses the Forth primitives.

1000000 constant /prims
\ -- #iterations; all of these words return the number of iterations
: $DO$    .ann ." DO LOOP"  [$  /prims DUP 0 DO  I [o/n] DROP LOOP  $] ;
: $*$     .ann ." *"        [$  /prims DUP 0 DO  I I * [o/n] DROP  LOOP  $] ;
: $/$     .ann ." /"        [$  /prims DUP 1+ 1 DO  1000 I / [o/n] DROP LOOP  $] ;
: $+$     .ann ." +"        [$  /prims DUP 1+ 1 DO  1000 I + [o/n] DROP  LOOP  $] ;
: $/MOD$  .ann ." /MOD"     [$  /prims DUP 1+ 1 DO  1000 I /MOD [o/n] 2DROP  LOOP  $] ;
: $*/$    .ann ." */"       [$  /prims DUP 1+ 1 DO  I I I */ [o/n] DROP  LOOP  $] ;


\ ****************************************
\ Eratosthenes sieve benchmark program
\ This is NOT the original BYTE benchmark.
\ ****************************************

3000 CONSTANT /SIEVE
4096 CONSTANT SIZE
TEMP CONSTANT FLAGS

: DO-PRIME
	 FLAGS SIZE -1 FILL
	 0 SIZE 0
	 DO I FLAGS + C@
	      IF I 2* 3 + DUP I +
		    BEGIN DUP SIZE <
		    WHILE DUP FLAGS + 0 SWAP C! OVER +
		    REPEAT 2DROP
			  1+
	      THEN
	 LOOP
	DROP
	   ;

: $SIEVE$   .ann ." Eratosthenes sieve (4kB array)"  [$ /SIEVE 0 DO DO-PRIME LOOP /SIEVE $] ;


\ *******************
\ Fibonacci recursion
\ *******************

38 constant /fib

: FIB ( n -- n' )
   DUP 1 > IF
      DUP 1- RECURSE  SWAP 2-  RECURSE  +
   THEN ;
   
: $FIB$
   .ann ." Fibonacci recursion (depth " /fib . ." )"
   [$  /fib FIB drop /fib $] ;

\ *********************************
\ QuickSort from Hoare & Wil Baden
\ also contains the array fill test
\ *********************************

10000 CONSTANT /FILL
1000 CONSTANT /SORT
1000 constant /array

7 CELLS CONSTANT THRESHOLD

: Precedes  ( n n - f )    u< ;

: Exchange  ( a1 a2 -- )   2DUP  @ SWAP @ ROT !  SWAP ! ;

: Both-Ends  ( f l pivot - f l )
    >R  BEGIN   OVER @ R@ precedes
        WHILE  CELL 0 D+   REPEAT
        BEGIN   R@ OVER @ precedes
        WHILE  CELL -      REPEAT   R> DROP ;

: Order3  ( f l - f l pivot)   2DUP OVER - 2/ -CELL AND + >R
      DUP @ R@ @ precedes IF DUP R@ Exchange THEN
      OVER @ R@ @ SWAP precedes
        IF OVER R@ Exchange  DUP @ R@ @ precedes
          IF DUP R@ Exchange THEN  THEN   R>  ;

: Partition  ( f l - f l' f' l)   Order3 @ >R  2DUP
      CELL -CELL D+  BEGIN    R@ Both-Ends 2DUP 1+ precedes
      IF  2DUP Exchange CELL -CELL D+  THEN
      2DUP SWAP precedes   UNTIL R> DROP SWAP ROT ;

: Sink  ( f key where - f)   ROT >R
   BEGIN   CELL - 2DUP @ precedes
   WHILE  DUP @ OVER CELL + !  DUP R@ =
        IF  ! R> EXIT THEN   ( key where)
   REPEAT  CELL + ! R> ;

: Insertion  ( f l)   2DUP precedes
    IF  CELL + OVER CELL +   DO  I @ I Sink  CELL +LOOP DROP
    ELSE  ( f l) 2DROP  THEN ;

: Hoarify  ( f l - ...)
    BEGIN   2DUP THRESHOLD 0 D+ precedes
    WHILE  Partition  2DUP - >R  2OVER - R> >  IF 2SWAP THEN
    REPEAT Insertion ;

: QUICK  ( f l)   DEPTH >R   BEGIN  Hoarify DEPTH R@ <
      UNTIL  R> DROP ;

: SORT  ( a n)   DUP 0= IF c" Nothing to sort"  THROW THEN
    1- CELLS  OVER +  QUICK ;

: pointers ( n -- a)
	CELLS TEMP +
;

: fillp		\ -- ; fill sort array once
  /array 0 ?DO  /array I -  I POINTERS !  LOOP ;

: $FILL$  .ann ." Array fill (1000 items)"   [$  /FILL 0 DO  fillp  LOOP  /FILL $]  ;

: (sort)  /SORT 0 DO   fillp  0 POINTERS /array SORT   LOOP ;

: $SORT$
  .ann ." Quick sort (1000 items)"
  [$  (sort) /SORT  $] ;

\ *******************************
\ End of Forth Inc benchmark code
\ *******************************


\ *********************************
\ "Random" Numbers
\ *********************************

1000 constant /random
4 constant /random-size

variable ShiftRegister
       1 ShiftRegister !

: RandBit	\ -- 0..1 ; Generates a "random" bit.
  ShiftRegister @ 1 and                 \ Gen result bit for this time thru.
  dup 0<>                               \ Tap at position 31.
  ShiftRegister @ 8 and 0<>             \ Tap at position 28.
  xor 0<>                               \ If the XOR of the taps is non-zero...
  if
    [ HEX ] 40000000 [ DECIMAL ]        \ ...shift in a "one" bit else...
  else
    [ HEX ] 00000000 [ DECIMAL ]        \ ...shift in a "zero" bit.
  then
  ShiftRegister @ u2/                   \ Shift register one bit right.
  or                                    \ OR in new left-hand bit.
  ShiftRegister !                       \ Store new shift register value.
;

: RandBits      \ n -- 0..2^(n-1) ; Generate an n-bit "random" number.
  0                                     \ Result's start value.
  swap 0
  do
    2* RandBit or                       \ Generate next "random" bit.
  loop
;

: (randtest)    \ --
  1 ShiftRegister !
  TEMP
  /random-size 256 * 0 do 32 RandBits over i cells + ! loop
  drop
;

: $RAND$
  .ann ." Generate random numbers (4kB array)"
  [$  /random 0 DO (randtest) LOOP /random $] ;


\ ***********
\ Bubble sort
\ ***********
\ A CLASSICAL BENCHMARK OF AN O(N**2) ALGORITHM; BUBBLE SORT
\
\ PART OF THE PROGRAMS GATHERED BY JOHN HENNESSY FOR THE MIPS
\ RISC PROJECT AT STANFORD. TRANSLATED TO FORTH BY MARTY FRAEMAN
\ JOHNS HOPKINS UNIVERSITY/APPLIED PHYSICS LABORATORY.

100 constant /bubble

: MYBOUNDS  OVER + SWAP ;	\ why not BOUNDS ?

VARIABLE SEED ( -- ADDR)

: INITIATE-SEED ( -- )  74755 SEED ! ;

: RANDOM  ( -- N )  SEED @ 1309 * 13849 + 65535 AND DUP SEED ! ;

800 CONSTANT ELEMENTS ( -- INT)

TEMP CONSTANT LIST

: INITIATE-LIST ( -- )
  LIST ELEMENTS CELLS + LIST
  DO  RANDOM I !  CELL +LOOP
;

: DUMP-LIST ( -- )
  LIST ELEMENTS CELLS + LIST
  DO  I @ .  CELL +LOOP
  CR
;

: VERIFY-LIST ( -- )
  LIST ELEMENTS 1- CELLS MYBOUNDS DO
    I 2@ > IF c" BUBBLE-SORT: NOT SORTED" THROW THEN
  CELL +LOOP
;

: BUBBLE ( -- )
  ELEMENTS 1 DO
    LIST ELEMENTS I - CELLS MYBOUNDS DO
      I 2@ > IF I 2@ SWAP I 2! THEN
    CELL +LOOP
  LOOP
;

: BUBBLE-SORT ( -- )
  INITIATE-SEED
  INITIATE-LIST
  BUBBLE
  VERIFY-LIST
;

: BUBBLE-WITH-FLAG ( -- )
  1  ELEMENTS 1 DO
    -1  LIST ELEMENTS I - CELLS MYBOUNDS DO
      I 2@ > IF I 2@ SWAP I 2! DROP 0 THEN
    CELL +LOOP
    IF LEAVE THEN
  LOOP
  DROP
;

: BUBBLE-SORT-WITH-FLAG ( -- )
  INITIATE-SEED
  INITIATE-LIST
  BUBBLE-WITH-FLAG
  VERIFY-LIST
;

: $BUBBLE$	\ --
  .ann ." Bubble sort (800 elements)"
  [$  /bubble 0 do  bubble-sort  loop  /bubble $]
;

: $BUBBLE-FLAG$	\ --
  .ann ." Bubble sort with flag (800 elements)"
  [$  /bubble 0 do  bubble-sort-with-flag  loop  /bubble $]
;


\ ********************
\ Eight queens problem
\ ********************
\ Taken from SP-Forth 4 - samples\bench\queens.f
\ Al.Chepyzhenko

50 CONSTANT /QUEENS

\ CARRAY  creates a byte size array.
: CARRAY ( n -- )
  CREATE  ALLOT  DOES> ( n -- a )  + ;
  
8  CARRAY Gori
8  CARRAY Verti
15 CARRAY Dio1
15 CARRAY Dio2

: Clear ( -- )
   8 0 DO 0 I Verti C! LOOP
  15 0 DO 0 I Dio1  C! LOOP
  15 0 DO 0 I Dio2  C! LOOP ;

: Check ( n -- f )
  Clear TRUE SWAP 1+ 0
  DO
      I Gori C@
      DUP Verti DUP C@
      IF
          DROP DROP DROP FALSE
      ELSE
          TRUE SWAP C!
          DUP I + Dio1 DUP C@
          IF
              DROP DROP DROP FALSE
          ELSE
              TRUE SWAP C!
              DUP 7 + I - Dio2 DUP C@
              IF
                  DROP DROP DROP FALSE
              ELSE
                  TRUE SWAP C! DROP TRUE AND
              THEN
          THEN
      THEN
  LOOP ;

: Print ( -- )
  8 0
  DO
   I Gori C@ .
  LOOP CR ;

: TRYTO ( n )
  8 0
  DO
      I OVER Gori C!
      DUP Check
      IF
          DUP 7 <
          IF   DUP 1+ RECURSE  THEN
      THEN
  LOOP
  DROP
  ;

: $QUEENS$	\ --
  .ann ." Eight Queens Problem"
  [$  /QUEENS 0 do  0 TRYTO  loop  /QUEENS $]
;

\ *************************
\ The main benchmark driver
\ *************************

variable CodeSize
0 CodeSize !

: .CodeSize
  xcr ." Benchmark code size"
  time-pos 2- >pos
  CodeSize @ 7 .r ."  bytes."
;

: BENCHMARK
   .CodeSize
   cr
   .ann ." This system's primitives" CR
   .header
   [[$$
     $DO$
     $+$ $*$  $/$  
     $/MOD$  $*/$
     $FILL$
   CR  ." Total:"  1 $$]]

   cr cr
   .ann ." This system's application performance" CR
   .header
   [[$$
     $SIEVE$  $FIB$ $SORT$  $RAND$ $BUBBLE$ $QUEENS$
   CR  ." Total:"  1 $$]]
;

decimal
here start-here - CodeSize !