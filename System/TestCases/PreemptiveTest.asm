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
	#.l	test1			; task 1 PCoverride address
	#.l	PCoverride 4 +	
	store.l
	#.b	1			; enable multitasking
	#.l	SingleMulti
	store.b
	#.b	25			; enable pre-emptive multitasking
	#.l	Interval
	store.b
;
test3	begin
		jsl debug
		jsl csr-fwd.cf
	again
;
debug	begin
		pause
		zero
		not
	until
		#.b 10
		zero
		DO
		LOOP
	rts
;
test1	begin
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
				pause
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
CSR-FWD.CF	#.w	CSR-X
		fetch.l
		dup
		#.w	COLS
		fetch.b
		1-
		=
		IF	
			drop
;			jsl	NEWLINE.CF
		ELSE
			1+
			#.w	CSR-X
			store.l
		THEN
CSR-FWD.Z	rts
CSR-X		dc.l	0
COLS		dc.l	0