sevenseg	equ	hex f830
irv0	bra start irv0 rel  	; reset
irv1	bra start irv1 rel	; trap	
irv2	bra start irv2 rel	; RS232_RDA_S0
irv3	nop			; RS232_TBE_S0
	nop
irv4	nop			; PS2_irq
	nop
irv5	nop			; ms a rel
	nop
irv6	nop
	bra -1
start	nop
;	#.w	hex f80c
;	zero
;	store.w
	#.b 0
	fetch.w
	jsl	sub1
	ds.b	1024
	ds.b	1024
	ds.b	1024
	ds.b	1024
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