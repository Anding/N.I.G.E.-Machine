\ *************************************
\ Let's measure the generated code size
\ *************************************

here CONSTANT start-here

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

: SORT  ( a n)   DUP 0= IF ." Nothing to sort"  ABORT THEN
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
  
here start-here - CodeSize !

: BENCHMARK
   .CodeSize
   cr
   .header
   [[$$
     $FILL$ $SORT$
   CR  ." Total:"  1 $$]]
;
