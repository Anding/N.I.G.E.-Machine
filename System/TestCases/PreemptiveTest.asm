SingleMulti	equ	243712
CurrentVM	equ	243716
Interval	equ	243720
TaskControl	equ	244224
PCoverride 	equ	244736
VirtualInt	equ	245248
mscounter	equ	hex 03F824		; actually the 100 MHz system clock
sevenseg	equ	hex 03F830
;
	nop
	nop
	nop
	nop
;
	#.b	1			; task 0 switches to task 1
	#.l	TaskControl
	store.w
	#.l	test			; task 1 PCoverride address
	#.l	PCoverride 4 +	
	store.l
	#.b	1			; enable multitasking
	#.l	SingleMulti
	store.b
	#.b	9			; enable pre-emptive multitasking
	#.l	Interval
	store.b
;
	zero
	zero
	zero
;
	begin
		NOP	( 0)
		PSP@	( 1)
		DUP	( 2)
		SWAP	( 3)
		OVER	( 4)
		ROT	( 4)	
		DROP	( 3)
		NIP	( 2)
		>R	( 1)
		R@	( 2)
		R>	( 3)
		+	( 2)
		-	( 1)
		NEGATE	( 1)
		1+	( 1)
		1-	( 1)
		2/	( 1)
		zero	( 2)
		ADDX	( 1)
		zero	( 2)
		SUBX	( 1)
		zero	( 2)
		=	( 1)
		zero	( 2)
		<>	( 1)
		zero	( 2)
		<	( 1)
		zero	( 2)
		>	( 1)
		zero	( 2)
		U<	( 1)
		zero	( 2)
		U>	( 1)
		0=	( 1)
		0<>	( 1)
		0<	( 1)
		0>	( 1)
		zero	( 2)
		AND	( 1)
		zero	( 2)
		OR	( 1)
		INVERT	( 1)
		zero	( 2)
		XOR	( 1)
		LSL	( 1)
		LSR	( 1)
		XBYTE	( 1)
		XWORD  ( 1)
		zero	( 2)
		MULTS	( 1)
		zero	( 2)
		MULTU	( 1)
		zero	( 2)
		DIVS	( 1)
		zero	( 2)
		DIVU	( 1)
		zero	( 2)
		?DUP	( 2)
		not	( 2)
		?DUP	( 3)
		drop	( 2)
		drop	( 1)
		drop 	( 0)
		#.B	1	( 1)
		#.W	1	( 2)
		#.L	1	( 3)
		drop	( 2)
		drop	( 1)
		drop	( 0)
	again	
;
test	begin
		PSP@
		#.l	sevenseg
		store.l
		#.b 	30
		jsl	ms
	again
;
MS		>R			( R:n)
		#.l	MScounter
		fetch.l		( start R:n)
			BEGIN		
;				pause
				#.l	MScounter	
				fetch.l		( start now R:n)
				over			( start now start R:n)
				-			( start m R:n)
				R@			( start m n R:n)
				U>			( start R:n)
			UNTIL
		R>
		drop
		drop,rts
;