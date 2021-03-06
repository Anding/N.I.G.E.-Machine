DECIMAL

: xcr
	cr emptyidle
;

: counter
	GetTickCount		
;

: >pos		\ n -- ; step to position n
	out
	@ - spaces
;

: [o/n]		\ --
  postpone []
; immediate

 4096 BUFFER: TEMP	\ common storage for all tests

\ ***********************
\ Benchmark support words
\ ***********************

CELL NEGATE CONSTANT -CELL

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

variable CodeSize
0 CodeSize !

: .CodeSize
  xcr ." Benchmark code size"
  time-pos 2- >pos
  CodeSize @ 7 .r ."  bytes."
;
