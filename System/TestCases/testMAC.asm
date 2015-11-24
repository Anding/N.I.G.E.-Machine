sevenseg	equ	hex 03F830
MACdataTX	equ	hex 03F870
MACtransmit	equ	hex 03F874
		nop
		nop
		nop
		nop
		#.l	testframe
		dup			( addr)
		fetch.b		( addr n)
		over			( addr n addr)
		swap			( addr addr n)
		-			( addr lo)
		swap			( lo addr)
		1-			( lo hi)
		DO
			R@
			fetch.b
			#.l	MACdataTX
			store.b
			zero
			not		; -1
		+LOOP
		zero
		#.l	MACtransmit
		store.b
		begin
		again
		nop
		nop
		nop
		nop
		dc.b	hex FF FF FF FF FF FF 00 08 DC 1E 52 60 08 06 00 01 08 00 06 04 00 01 00 08 DC 1E 52 60 C0 A8 0B 64 00 00 00 00 00 00 C0 A8 0B 01
testFrame	dc.b	43		; number of bytes.  Read from here less one backwards.. FCS 