; BootLoader test
sevenseg	equ	hex 03f830
	ds.l	128
	jsl	l0
	bra	11
l0	R@
	#.l	sevenseg
	store.l
l1	bra	l1