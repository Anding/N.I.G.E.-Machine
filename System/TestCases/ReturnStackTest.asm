;	N.I.G.E. Machine return stack test suite
;
sevenseg	equ	hex F830
		ds.l	2		; BLOCK RAM simulator bug
		nop
		nop
		nop
RTStest	#.b	1		
		jsl	announce
;
		jsl	pushpop
		jsl	dbl
		jsl	adjacent
		drop
		jsl	RSPtest
		#.b	255
		jsl	announce
l0		bra	l0
;
pushpop	#.b	2
		dup
		jsl	announce
		>R
		R>
		#.b	2
		-
		#.w	assert
		jmp
;
dbl		#.b	3
		jsl	announce
		jsl	dbl2
		rts
dbl2		rts
;
adjacent	#.b	4
		dup
		jsl 	announce
		>R
		R>
		rts
;
RSPtest	#.b	5
		jsl 	announce	
		RSP@
		#.b	1
		>R
		RSP@
		-
		1+		; first check
		#.b	2
		>R
		R@
		1-
		1-		
		+
		RSP@
		1-
		RSP!
		R>
		1-		; second check
		+
		#.w	assert
		jmp
;
; Announce a test
announce	#.w	sevenseg
		store.l
		rts
;
; Confirm result equals zero
assert		0=
		IF
			rts
		THEN
l0		bra	l0
;