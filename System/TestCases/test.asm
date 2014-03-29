sub0	equ	hex 03D000
	nop
	nop
	nop
	nop
	#.b	255
	#.l	sub0
	store.l
	#.l	sub0	
	fetch.l
l0	bra l0