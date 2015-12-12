sevenseg	equ	hex 03F830
SMIaddr	equ	hex 03F878
SMIdataWrite	equ	hex 03F87C
SMIread_req	equ	hex 03F880
SMIready	equ	hex 03F884
SMIdataRead	equ	hex 03F888
	nop
	nop
	nop
	nop
;	bra	l_B
; write test
	#.w	binary 0000100000	; register 0 of PHY 1
	#.l	SMIaddr
	store.w
	#.w	binary 1010101010101011
	#.l	SMIdataWrite
	store.w
	begin
	again
	nop
	nop
	nop
	nop
; read test
l_B	#.w	binary 0000100000	; register 0 of PHY 1
	#.l	SMIaddr
	store.w	
	#.l	SMIread_req
	fetch.b
	drop
	#.l	SMIready
	begin
		dup
		fetch.b
	until
	drop
	#.l	SMIdataRead
	fetch.w
	#.l	sevenseg
	store.w
	begin
	again
	nop
	nop
	