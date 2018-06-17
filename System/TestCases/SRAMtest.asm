sevenseg	equ	hex 03F830
	nop
	nop
	nop
	nop
	#.w		hex 7766
	#.l		hex 1000 		; decimal 4096
	store.w
	#.l		hex 1000	
	fetch.w
	#.l		sevenseg
	store.l
	nop
	nop
	pause
l0	bra l0
	nop
	nop
	nop
	nop
	