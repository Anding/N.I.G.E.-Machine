;	N.I.G.E. Machine test suite
;	20us test time
;
sevenseg	equ	hex F830
		ds.l	2
		jsl	loadliteral
		jsl	llrts
		jsl	fas
		jsl	fasd
		jsl	frts
		jsl	ifdup
		jsl	multiply
		jsl	divide
		#.b	255
		jsl	announce
l1		bra	l1 l1	rel
;	
;load literals	
loadliteral	#.b	1
		jsl	announce
		#.b	hex	a9
		#.w	hex	bbcc
		#.l	hex	ffff438a
		+
		+
		1+
		#.w	assert
		jmp
; 
; loadliterals with ,rts
llrts		#.b	2
		jsl	announce
		jsl	llsub1
		+
		+
		1+
		#.w	assert
		jmp
llsub1		jsl	llsub2
		#.b,rts hex a9
llsub2		jsl	llsub3
		#.w,rts hex bbcc
llsub3		#.l,rts hex ffff438a 
;
; fetch and store
fas		#.b	3
		jsl	announce
		#.w	0
		jsl	fssub1
		#.w	1
		jsl	fssub1		
		#.w	2
		jsl	fssub1
		#.w	3
		jsl	fssub1
		+
		+
		+
		#.w	assert
		jmp		
fssub1		#.l	hex	ffffffff
		over
		store.l
		#.w	hex	eeee
		over
		store.w
		#.b	hex	cc
		over
		store.b
		dup
		fetch.l
		over
		fetch.w
		+
		swap
		fetch.b
		+
		#.l	hex	33103247
		+
		rts
;
; fetch and store PSDRAM
fasd		#.b	4
		jsl	announce
		#.w	65536
		jsl	fssub1
		#.w	65537
		jsl	fssub1		
		#.w	65538
		jsl	fssub1
		#.w	65539
		jsl	fssub1
		+
		+
		+
		#.w	assert
		jmp	
;		
; fetch store,rts	
frts		#.b	5
		jsl	announce
		jsl	fsrsub3
		jsl	fsrsub6
		+
		+
		#.l	hex	33103247
		+
		#.w	assert
		jmp
fsrsub1	#.l	hex	ffffffff
		zero
		store.l,rts
fsrsub2	jsl	fsrsub1
		#.w	hex	eeee
		zero
		store.w,rts
fsrsub3	jsl	fsrsub2
		#.b	hex	cc
		zero
		store.b,rts
fsrsub4	zero
		fetch.l,rts		
fsrsub5	jsl	fsrsub4
		zero
		fetch.w,rts
fsrsub6	jsl	fsrsub5
		zero
		fetch.b,rts
;
; ifdup
ifdup		#.b	6
		jsl	announce
		#.l	hex 7fffffff
		jsl	ifdups1
		+
		1+
		1+
		#.l	hex ffffffff
		zero
		jsl	ifdups1
		drop
		1+
		+
		#.w	assert
		jmp
ifdups1	?dup,rts
;
; multipy
multiply	#.b	7
		jsl	announce
		#.l	-1
		#.l	2
		jsl	mulsub1
		1+
		+
		1+
		1+
		#.l	-1
		#.l	2
		jsl	mulsub2
		+
		1+
		+
		#.w	assert
		jmp
mulsub1	mults,rts
mulsub2	multu,rts
;	
; divide
divide		#.b	8
		jsl	announce
		#.l	-1
		#.l	2
		jsl	divsub1
		+
		#.l	hex 80000000
		+
		#.l	-2
		#.l	2
		jsl	divsub2
		+
		1+
		+
		#.w	assert
		jmp
divsub1	divu,rts
divsub2	divs,rts	
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
l0		bra	l0 l0 rel
;