sevenseg	equ	hex f830
irv0	nop 		     	; reset
	nop
irv1	nop			; trap
	nop	
irv2	nop			; RS232_RDA_S0
	nop
irv3	nop			; RS232_TBE_S0
	nop
irv4	nop			; PS2_irq
	nop
irv5	nop			; ms a rel
	nop
irv6	nop
	nop
start	#.w	hex abcd
	jsl	sub1
	ds.b	1024
	ds.b	1024
	ds.b	1024
	ds.b	1024
	ds.b	1024
	ds.b	1024
	ds.b	1024
	ds.b	1024	
sub1	#.w	sevenseg
	store.l
	bra	-1