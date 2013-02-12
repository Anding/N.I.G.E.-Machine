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

;

decimal
here start-here - CodeSize !