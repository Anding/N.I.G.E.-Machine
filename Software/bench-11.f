DECIMAL

: xcr
	cr
;

: counter
	63528 @			
;

: >pos		\ n -- ; step to position n
	CSR-X 
	@ - spaces
;

: 2-		\ n -- n-2
	2 -
;

: CELL
	1 CELLS
;

: [o/n]		\ -- ; stop optimiser treating * DROP etc as no code
; immediate

4096 SBUFFER: TEMP		\ common storage for all tests
