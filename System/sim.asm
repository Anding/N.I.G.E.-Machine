irv0	bra start irv0 rel ; reset
irv1	nop			; trap
	rti	
irv2	nop			; RS232_RDA_S0
	nop
irv3	nop			; RS232_TBE_S0
	nop
irv4	nop			; PS2_irq
	nop
irv5	nop			; ms a rel
	rti
irv6	nop
	nop
start	jsl	sub1
	bra	-1
sub1	#.b	hex ff
	rts