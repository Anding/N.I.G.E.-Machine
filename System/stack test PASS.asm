; stack-test
sevenseg	equ	hex F830
;
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	#.w hex 1111
	#.w hex 2222
	#.w hex 3333
	drop
	drop
	jsl display
	bra -1
;
display	#.w	sevenseg
		store.w
		rts	