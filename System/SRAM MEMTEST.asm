; SRAM-MEMTEST
sevenseg	equ	hex F830
memlo		equ	hex 0400
memhi		equ	hex B000		; B000
pattern1	equ	hex DDDDDDDD
pattern2	equ	hex 55555555
;
;
		nop
		nop
		nop
		nop
		nop
start		#.w	hex 8888
		jsl	display
		#.w	pattern1
		#.w	memhi
		#.w	memlo
		jsl	ram-write
		#.w	hex BBBB
		jsl	display
		#.w	pattern1
;		#.b	hex ff
;		and
		#.w	memhi
		#.w	memlo
		jsl	ram-read		
		#.w	hex CCCC
		jsl	display
		bra	-1
;
display	#.w	sevenseg
		store.w
		rts
;
error		jsl	display
		bra	-1
;
ram-write	DO		( pattern end start)
			dup
			R@
			store.w
			1+
			#.b	2
		+LOOP
		drop,rts
;
ram-read	DO		( pattern end start)
			dup
			R@
			fetch.w
			<>
			IF
				R@
				fetch.w
				JSL error
			THEN
			1+
			#.b	2
		+LOOP
		drop,rts