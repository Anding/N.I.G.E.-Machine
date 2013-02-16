\ *************************************
\ Let's measure the generated code size
\ *************************************

here CONSTANT start-here

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

here start-here - CodeSize !

: BENCHMARK
   .CodeSize
   cr
   .header
   [[$$
     $RAND$ $QUEENS$
   CR  ." Total:"  1 $$]]
;
