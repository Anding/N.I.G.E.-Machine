sevenseg	equ	hex 03f830
memoryzero	equ	hex 040000
		nop
		nop
		nop
		nop
		#.l	hex AABBCCDD		
		#.l	memoryzero
		store.l
		#.l memoryzero
		fetch.l
		#.l	sevenseg
		store.l
		BEGIN
		AGAIN