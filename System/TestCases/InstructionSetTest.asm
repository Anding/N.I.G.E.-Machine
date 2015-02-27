;	N.I.G.E. Machine test suite
;	30us test time
sub0		equ	hex 03F000
env0		equ	hex 03F080
sevenseg	equ	hex 03F830
PSDRAM		equ	hex 040000
SRAM		equ	hex 001000
USERRAM	equ	hex 03C000
SingleMulti	equ	243712
CurrentVM	equ	243716
Interval	equ	243720
TaskControl	equ	244224
PCoverride 	equ	244736
		ds.l	2			; BLOCK RAM simulator bug
;
		#.b	1			; task 0 switches to task 1
		#.l	TaskControl
		store.w
		#.l	test1			; task 1 PCoverride address
		#.l	PCoverride 4 +	
		store.l
		#.b	1			; enable multitasking
		#.l	SingleMulti
		store.b
		#.b	1			; enable pre-emptive multitasking
		#.l	Interval
		store.b
;
main	begin
		jsl debug
		#.b	5
		zero
		DO
			nop
		LOOP
	again
;
debug	begin
;		pause
		zero
		not
	until
		#.b	10
		>R
		R@
		drop
		R>
		drop
	rts
;
test1		#.b	0	
		jsl	announce	; test subroutine call
		#.b	1	
		jsl	announce	; test branching	
		zero			
		1+
l0		beq	l1
		zero
l2		beq	l3
l4		bra	l1		
l3		jsl	loadliteral	; run test suite
		jsl	llrts
		jsl	fas
		jsl	fasd
		jsl	frts
		jsl	ifdup
		jsl	multiply
		jsl	divide
		jsl	bincompare
		jsl	uncompare
		jsl	bitwise
		jsl	pushpop
		jsl	dbl
		jsl	adjacent
		jsl	pstack
		jsl	envstack
		jsl	substack
		jsl	arith
		jsl	others
		jsl	throw0
		jsl	fasu
		#.b	255
		jsl	announce
l1		bra	l1
;	
;load literals	
loadliteral	#.b	2
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
llrts		#.b	3
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
fas		#.b	4
		jsl	announce
		#.l	hex SRAM
		jsl	fssub1
		#.l	hex SRAM 1 +
		jsl	fssub1		
		#.l	hex SRAM 2 +
		jsl	fssub1
		#.l	hex SRAM 3 +
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
fasd		#.b	5
		jsl	announce
		#.l	PSDRAM 
		jsl	fssub1
		#.l	PSDRAM 4 +
		jsl	fssub1		
		#.l	PSDRAM 8 +
		jsl	fssub1
		#.l	PSDRAM 12 +
		jsl	fssub1
		+
		+
		+
		#.w	assert
		jmp	
;		
; fetch and store USERRAM
fasu		#.b	22
		jsl	announce
		#.l	USERRAM 
		jsl	fssub1
		#.l	USERRAM 1 +
		jsl	fssub1		
		#.l	USERRAM 2 +
		jsl	fssub1
		#.l	USERRAM 3 +
		jsl	fssub1
		+
		+
		+
		#.w	assert
		jmp	
;
; fetch store,rts	
frts		#.b	6
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
		#.l	SRAM
		store.l,rts
fsrsub2	jsl	fsrsub1
		#.w	hex	eeee
		#.l	SRAM
		store.w,rts
fsrsub3	jsl	fsrsub2
		#.b	hex	cc
		#.l	SRAM
		store.b,rts
fsrsub4	#.l	SRAM
		fetch.l,rts		
fsrsub5	jsl	fsrsub4
		#.l	SRAM
		fetch.w,rts
fsrsub6	jsl	fsrsub5
		#.l	SRAM
		fetch.b,rts
;
; ifdup
ifdup		#.b	7
		jsl	announce
		zero
		#.l	hex 7fffffff
		jsl	ifdups1
		+
		1+
		1+			
		jsl	assert
		#.l	hex ffffffff
		zero
		jsl	ifdups1
		drop
		1+
		jsl	assert
		rts
ifdups1	?dup,rts
;
; multipy
multiply	#.b	8
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
divide		#.b	9
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
;
; binary compare
bincompare	#.b	10
		jsl	announce
		zero
		#.w	bcmplist
		#.w	bcmplist
		DO
			#.l	hex fffffffe
			#.l	hex fffffffd
			R@
			jsr
			1+
			lsl
			#.l	hex fffffffd
			#.l	hex fffffffe
			R@
			jsr
			1+
			+
			lsl
			#.l	hex fffffffe
			#.l	hex fffffffe
			R@
			jsr
			1+
			+
			R@
			#.w	bcmplist 
			-
			#.w	bresultlist
			+
			fetch.b
			-
			+
		LOOP
		#.w	assert
		jmp
bcmplist	=,rts
		<>,rts
		<,rts
		>,rts
		U<,rts
bcmplistE	U>,rts
bresultlist	dc.b	binary 110
		dc.b	binary	001
		dc.b	binary	011
		dc.b	binary	101
		dc.b	binary	101
		dc.b	binary	011
;
; Unary compare
uncompare	#.b	11
		jsl 	announce
		zero
		#.w	ucmplist
		#.w	ucmplist
		DO
			#.l	hex 1
			R@
			jsr
			1+
			lsl
			#.l	hex 0
			R@
			jsr
			1+
			+
			lsl
			#.l	hex ffffffff
			R@
			jsr
			1+
			+
			R@
			#.w	ucmplist 
			-
			#.w	uresultlist
			+
			fetch.b
			-
			+
		LOOP
		#.w	assert
		jmp
ucmplist	0=,rts
		0<>,rts
		0<,rts
ucmplistE	0>,rts
uresultlist	dc.b	binary 101
		dc.b	binary	010
		dc.b	binary	110
		dc.b	binary	011
;
; bitwise test
bitwise	#.b	12
		jsl	announce
		#.l	hex ffffffff
		invert
		zero
		#.l	hex ffffffff
		and
		+
		#.l	hex 0000ffff
		#.l	hex ffff0000
		or
		1+
		+
		#.l	hex ffffffff
		#.l	hex ffffffff
		xor
		+
		#.b	-1
		xbyte
		1+
		+
		#.w	-1
		xword
		1+
		+
		#.b	2
		lsr
		1-
		+
		#.b	1
		lsl
		1-
		1-
		+
		#.w	assert
		jmp
;
pushpop	#.b	13
		jsl	announce
		#.b	2
		>R
		R>
		#.b	2
		-
		#.w	assert
		jmp
;
dbl		#.b	14
		jsl	announce
		jsl	dbl2
		rts
dbl2		rts
;
adjacent	#.b	15
		dup
		jsl 	announce
		>R
		R>
		rts
;
pstack		#.b	16
		jsl	announce
		#.b	1
		#.b	2
		#.b	3
		+
		+
		#.b	6
		-
		jsl	assert			
		rts		
envstack	#.b	17
		jsl	announce
		#.l	hex FFEEDDCC
		#.l	env0
		store.l			; set env variable at this level
		#.l	envt2
		catch
		zero
		#.l	env0
		fetch.l
		#.l	hex FFEEDDCC
		-
		jsl	assert			; check retention of env0
		rts
;
envt2		#.l	env0
		fetch.l
		#.l	hex FFEEDDCC
		-				; check copy down
		jsl	assert
		#.l	hex AABBCCDD
		#.l	env0
		store.l			; re-write env variable	
		#.b	1
		throw	
;
substack	#.b	18
		jsl	announce
		#.l	hex FFEEDDCC
		#.l	sub0
		store.l			; set sub variable at this level
		#.l	subt3
		jsr
		#.l	sub0
		fetch.l
		#.l	hex FFEEDDCC
		-
		jsl	assert			; check retention of sub0
		rts
;
subt3		#.l	sub0
		fetch.l
		jsl	assert
		#.l	hex AABBCCDD
		#.l	sub0
		store.l			; re-write sub variable	
		rts	
;
; arithmatic test
arith    	#.b	19
		jsl 	announce
		#.l	hex	12345678
		#.l	hex	aaaa0000
		#.l	hex	00001111
		#.l	hex	88888888	
		jsl	D+sub
		#.l	hex	33328888
		-
		swap
		#.l	hex 	1234678A
		-
		+
		#.l	hex	90005555
		#.l	hex	44443333		
		#.l	hex	00012222
		#.l	hex	67890000	
		jsl	D+sub
		#.l	hex	abcd3333
		-
		swap
		#.l	hex 	90017777
		-
		+
		+
		#.l	hex	12345678
		#.l	hex	aaaa0000	
		#.l	hex	00001111
		#.l	hex	88888888	
		jsl	D-sub
		#.l	hex	22217778
		-
		swap
		#.l	hex 	12344567
		-
		+
		+
		#.l	hex	90005555
		#.l	hex	44443333		
		#.l	hex	00012222
		#.l	hex	67890000	
		jsl	D-sub
		#.l	hex	dcbb3333
		-
		swap
		#.l	hex 	8fff3333
		-
		+
		+
		
		#.w	assert
		jmp
; D+ code
D+sub		SWAP		( h2 l2 l1 h1)
		>R		( h2 l2 l1 R: h1)
		rot		( l2 l1 h2 R: h1)
		>R		( l2 l1 R: h1 h2)
		+		( l3 R: h1 h2)
		R>		( l3 h2 R: h1)
		R>		( l3 h2 h1)
		ADDX		( l3 h3)
		swap,rts	( h3 l3)
;
; D- 	(ud1 ud2 -- ud3)  double precision arithmetic
D-sub		>R		( h2 l2 h1 R: l1)
		SWAP		( h2 h1 l2 R: l1)
		>R		( h2 h1 R: l1 l2)
		-		( h3 R: l1 l2)
		R>		( h3 l2 R: l1)
		R>		( h3 l2 l1)
		SUBX,rts	( h3 l3)
;
; others	
others		#.b	20
		jsl 	announce
		#.w	-1	
		xword
		negate
		zero
		#.b	-1
		xbyte
		nip
		+
		#.b	1
		2*
		1-
		1-
		+
		#.b	2
		2/
		1-
		+
		#.w	assert
		jmp
; THROW 0
THROW0		#.b	21
		jsl 	announce
		#.w	THROW0a		
		catch
		rts			;  subroutine stack return address
l1		bra	l1		;  exception stack return address
THROW0a	zero
		throw
		rts
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