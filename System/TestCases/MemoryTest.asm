sevenseg	equ	hex F830
		nop
		nop
		nop
		nop	
		jsl	writeLreadL
		jsl	writeWreadL
		jsl	writeBreadL
		jsl	writeLreadW
		jsl	writeLreadB
		jsl	fourwrites
		#.b	255
		jsl	announce
l1		bra	l1
;		
writeLreadL	#.b	01
		jsl	announce
		#.l	hex 89ABCDEF
		#.l	65536		
		store.l
		#.l	65536
		fetch.l
;
;		jsl announce
;stop		bra stop
;
		#.l	hex 89ABCDEF
		-
		#.w	assert
		jmp	
;
;
writeWreadL	#.b	02
		jsl	announce
		#.w	hex 89AB
		#.l	65536
		store.w	
		#.w	hex CDEF
		#.l	65538
		store.w	
		#.l	65536
		fetch.l
		#.l	hex 89ABCDEF
		-
		#.w	assert
		jmp	
;
writeBreadL	#.b	03
		jsl	announce
		#.b	hex 89
		#.l	65536
		store.b	
		#.b	hex AB
		#.l	65537
		store.b
		#.b	hex 0CD
		#.l	65538
		store.b	
		#.b	hex EF
		#.l	65539
		store.b
		#.l	65536
		fetch.l
		#.l	hex 89ABCDEF
		-
		#.w	assert
		jmp
writeLreadW	#.b	04
		jsl	announce
		#.l	hex 89ABCDEF
		#.l	65536
		store.l
		#.l	65536
		fetch.w
		#.w	hex 89AB
		-				; flag1
		#.l	65538
		fetch.w
		#.w	hex CDEF
		-				; flag2
		or
		#.w	assert
		jmp
writeLreadB	#.b	05
		jsl	announce
		#.l	hex 89ABCDEF
		#.l	65536
		store.l
		#.l	65536
		fetch.b
		#.w	hex 89
		-				; flag1
		#.l	65537
		fetch.b
		#.w	hex 0AB
		-				; flag2
		#.l	65538
		fetch.b
		#.w	hex 0CD
		-				; flag1
		#.l	65539
		fetch.b
		#.w	hex 0EF
		-
		or
		or
		or
		#.w	assert
		jmp	
;
fourwrites	#.b	06
		jsl	announce
		#.l	hex 89ABCDEF
		#.l	hex 010000		
		store.l
		#.l	hex 12345678
		#.l	hex 010004
		store.l
		#.l	hex 13579BDF
		#.l	hex 010008
		store.l
		#.l	hex 02468ACE
		#.l	hex 01000C
		store.l
		#.l	hex 010000
		fetch.l
		#.l	hex 89ABCDEF
		-
		#.l	hex 010004
		fetch.l
		#.l	hex 12345678
		-
		#.l	hex 010008
		fetch.l
		#.l	hex 13579BDF
		-
		#.l	hex 01000C
		fetch.l
		#.l	hex 02468ACE
		-
		or
		or
		or
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
