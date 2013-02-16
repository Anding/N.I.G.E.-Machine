\ *************************************
\ Let's measure the generated code size
\ *************************************

here CONSTANT start-here

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
    I 2@ > IF ." BUBBLE-SORT: NOT SORTED" ABORT THEN
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

here start-here - CodeSize !

: BENCHMARK
   .CodeSize
   cr
   .header
   [[$$
     $BUBBLE$
   CR  ." Total:"  1 $$]]
;
