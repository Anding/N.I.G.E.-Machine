sevenseg	equ	hex 03F830
	nop
	nop
	nop
	nop
;	#.l		hex 77665544
;	#.l		hex 1000 		; decimal 4096
;	store.l
	#.l		243740	
	fetch.w
	#.l		sevenseg
	store.l
l0	bra l0
	nop
	nop
	nop
	nop
	