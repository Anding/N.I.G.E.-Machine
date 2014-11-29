TaskControl	equ	244224
PCoverride 	equ	244736
VirtualInt	equ	244248
;
	dc.l	0
	#.w	hex ABCD
	#.l	TaskControl
	store.l
	nop
	nop
	nop
	nop
	#.l	TaskControl
	fetch.l
l0	bra	l0
	