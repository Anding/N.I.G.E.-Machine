sevenseg	equ	hex F830
textzero	equ	hex 010600
	nop
	nop
	nop
	nop
color	equ	hex 0100
	#.l	textzero
	#.b	5
	zero
	DO
		#.b	255
		zero
		DO
			R@
			R@
			#.w	256
			multu
			drop
			or
			over
			store.w
			1+
			1+
		LOOP
	LOOP
	jsl	announce
l0	bra	l0
; Announce a test
announce	#.w	sevenseg
		store.l
		rts
		