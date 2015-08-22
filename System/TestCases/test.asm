charRAM	equ	hex 03C000
sevenseg	equ	hex 03F830
		nop
		nop
		nop
		nop
		#.l	hex 03D020
		#.l	hex 03D000
		DO
			R@
			fetch.w
			#.l	sevenseg
			store.w
			#.b	2			
		+LOOP
		begin
		again