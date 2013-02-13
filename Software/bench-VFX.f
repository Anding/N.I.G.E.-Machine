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
