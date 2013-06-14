	bra -1		; reset
	nop		; trap
	nop
	nop		; RS232_RDA_S0
	nop
	nop		; RS232_TBE_S0
	nop
	nop		; PS2_irq
	nop
a	bra	ms a rel
	nop
	nop
	nop
	nop
ms	#.b	hex ff
	rti