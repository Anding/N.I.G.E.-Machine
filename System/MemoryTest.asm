	nop
	nop
	nop
	nop	
	#.w	hex 89AB
	#.l	65536
	store.w	
	#.w	hex CDEF
	#.l	65538
	store.w
	#.l	65536
	fetch.l
l0	bra	l0