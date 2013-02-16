\ *************************************
\ Let's measure the generated code size
\ *************************************

here CONSTANT start-here

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

here start-here - CodeSize !

: BENCHMARK
   .CodeSize
   cr
   .header
   [[$$
     $SIEVE$  $FIB$
   CR  ." Total:"  1 $$]]
;
