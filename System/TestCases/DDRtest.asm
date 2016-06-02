_TEXT_ZERO	equ	hex 040000
sevenseg	equ	hex 03f830
;
		nop
		nop
		nop
		nop
; write data
		#.l	hex 11223344
		#.l	_TEXT_ZERO
		store.l
		#.l	hex 55667788
		#.l _TEXT_ZERO 4 +
		store.l
; read data
		#.l _TEXT_ZERO
		jsl qm	
		#.l _TEXT_ZERO 4 +
		jsl qm
; end
		BEGIN
		AGAIN
; subroutines
qm		fetch.l				; ( addr --, read memory at addr and output to sevenseg)
		#.l	sevenseg
		store.l
		rts
;