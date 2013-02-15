\ *************************************
\ Let's measure the generated code size
\ *************************************

here CONSTANT start-here

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

here start-here - CodeSize !

: BENCHMARK
   .CodeSize
   cr
   .header
   [[$$
     $DO$
     $+$ 
     $*$  
     $/$  
     $/MOD$  
     $*/$
   CR  ." Total:"  1 $$]]
;
