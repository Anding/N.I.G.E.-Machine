sub0		equ	hex 03D000
env0		equ	hex 03D080
sevenseg	equ	hex 03F830
		nop
		nop
		nop
		nop
		jsl	envstack
		jsl	substack
		#.b	255
		jsl	announce
stop		bra	stop
;
envstack	#.b	1
		jsl	announce
		#.l	hex FFEEDDCC
		#.l	env0
		store.l			; set env variable at this level
		#.l	t2
		catch
		zero
		#.l	env0
		fetch.l
		#.l	hex FFEEDDCC
		=
		jsl	assert			; check retention of env0
		rts
;
t2		#.l	env0
		fetch.l
		#.l	hex FFEEDDCC
		=				; check copy down
		jsl	assert
		#.l	hex AABBCCDD
		#.l	env0
		store.l			; re-write env variable	
		#.b	1
		throw	
;
substack	#.b	2
		jsl	announce
		#.l	hex FFEEDDCC
		#.l	sub0
		store.l			; set sub variable at this level
		#.l	t3
		jsr
		#.l	sub0
		fetch.l
		#.l	hex FFEEDDCC
		=
		jsl	assert			; check retention of sub0
		rts
;
t3		#.l	sub0
		fetch.l
		jsl	assert
		#.l	hex AABBCCDD
		#.l	sub0
		store.l			; re-write sub variable	
		rts	
; Announce a test
announce	#.l	sevenseg
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