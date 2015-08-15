charRAM	equ	hex 03B000
sevenseg	equ	hex 03F830
		nop
		nop
		nop
		nop
		#.l	charRAM
		dup
		#.b	64
		+
		swap
		DO
			R@
			fetch.w
			#.l	sevenseg
			store.w
			#.b	2			
		+LOOP
		begin
		again