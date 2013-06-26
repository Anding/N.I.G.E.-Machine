; SRAM-MEMTEST
sevenseg	equ	hex F830
memlo		equ	hex 0400
memhi		equ	hex 0405		; B000
pattern1	equ	hex AAAAAAAA
pattern2	equ	hex 55555555
;
;
		nop
		nop
		nop
		nop
start		#.w	hex AAAA
		jsl	display
		#.b	pattern1
		#.w	memhi
		#.w	memlo
		jsl	ram-write.b
		#.w	hex BBBB
		jsl	display
		#.b	pattern1
		#.w	memhi
		#.w	memlo
		jsl	ram-read.b		
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
ram-write.b	DO		( pattern end start)
			dup
			R@
			store.b
			1+
		LOOP
		drop,rts
;
ram-read.b	DO		( pattern end start)
			dup
			R@
			fetch.b
			<>
			IF
				R@
				JSL error
			THEN
			1+
		LOOP
		drop,rts