	#.b	1
	>R	
	#.b	2
	>R
	jsl	subA
;	nop
	R>			( x 2)
	R>			( x 2 1)
	drop			( x 2)
	drop			( x)
	bra 0
	nop
subA	#.b	21
	#.l	65536
	store.l
	#.l	65536
	fetch.l,rts
