\ STRUCTURE words, machine code optimized, following http://www.forth200x.org/structures.html
: begin-structure : here 1+ 0 55 c, 0 , postpone ; ;	\ 44 is the opcode for #.l which will be compiled into the definition
: end-structure swap ! ;
: +field over : postpone literal 15 c, postpone ; + ;	\ 15 is the opcode for +
: field: 4 +field ;
: wfield: 2 +field ;
: cfield: 1 +field ;

