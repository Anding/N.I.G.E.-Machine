sevenseg	equ	hex 03f830
;memloc	equ	hex 040000	; 040000 first word of PSDRAM
;memloc	equ	hex 01FFFC	; 01FFFC is last word of SRAM
memloc		equ	hex 03C000	; 03C000 first word of USER RAM
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
		#.l	memloc	
		store.l
		#.l	memloc
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
		#.l	memloc
		store.w	
		#.w	hex CDEF
		#.l	memloc 2 +
		store.w	
		#.l	memloc
		fetch.l
		#.l	hex 89ABCDEF
		-
		#.w	assert
		jmp	
;
writeBreadL	#.b	03
		jsl	announce
		#.b	hex 89
		#.l	memloc
		store.b	
		#.b	hex AB
		#.l	memloc 1 +
		store.b
		#.b	hex 0CD
		#.l	memloc 2 +
		store.b	
		#.b	hex EF
		#.l	memloc 3 +
		store.b
		#.l	memloc
		fetch.l
		#.l	hex 89ABCDEF
		-
		#.w	assert
		jmp
writeLreadW	#.b	04
		jsl	announce
		#.l	hex 89ABCDEF
		#.l	memloc
		store.l
		#.l	memloc
		fetch.w
		#.w	hex 89AB
		-				; flag1
		#.l	memloc 2 +
		fetch.w
		#.w	hex CDEF
		-				; flag2
		or
		#.w	assert
		jmp
writeLreadB	#.b	05
		jsl	announce
		#.l	hex 89ABCDEF
		#.l	memloc
		store.l
		#.l	memloc
		fetch.b
		#.w	hex 89
		-				; flag1
		#.l	memloc 1 +
		fetch.b
		#.w	hex 0AB
		-				; flag2
		#.l	memloc 2 +
		fetch.b
		#.w	hex 0CD
		-				; flag1
		#.l	memloc 3 +
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
		#.l	memloc		
		store.l
		#.l	hex 12345678
		#.l	memloc 4 +
		store.l
		#.l	hex 13579BDF
		#.l	memloc 8 +
		store.l
		#.l	hex 02468ACE
		#.l	hex memloc C +
		store.l
		#.l	memloc
		fetch.l
		#.l	hex 89ABCDEF
		-
		#.l	memloc 4 +
		fetch.l
		#.l	hex 12345678
		-
		#.l	memloc 8 +
		fetch.l
		#.l	hex 13579BDF
		-
		#.l	hex memloc C +
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
