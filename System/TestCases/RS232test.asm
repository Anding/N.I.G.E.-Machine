; RS232test
RS232tx	equ	hex 03f814
	nop
	nop
	nop
	nop
	#.b	129
	#.l	RS232tx
	store.l
l0	bra l0
;