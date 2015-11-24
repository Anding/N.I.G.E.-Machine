sevenseg	equ	hex 03F830
		nop
		nop
		nop
		nop
		#.b	0
		#.b	9
		DO
			R@
			#.l sevenseg
			store.b
			#.b	0
			not		; -1
		+LOOP
		begin
		again