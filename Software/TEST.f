HEX
03D004 CONSTANT LOCAL1
03D080	CONSTANT ENV0
DECIMAL

: A
	LOCAL1 ?
;

: B
	99 LOCAL1 !
	A
	LOCAL1 ?
;

: TEST
	B
;
	