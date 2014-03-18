	nop
	nop
	nop
	nop
	#.b	255
	>R
	#.b	127
	>R
	jsl	l1
	jsl	l2
l0	bra	l0
	nop
	nop
	nop
l1	#.b	65
	>R
	rts
l2	R>
	jsl	l3
	nop
	nop
	nop
l3	#.l	l4
	catch
	zero
l5	bra	l5
	nop
	nop
	nop
l4	zero
	zero
	zero
	resetsp
l6	bra	l6
	