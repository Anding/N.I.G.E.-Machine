local0		equ	hex 03F000
local1		equ	hex 03F004
sevenseg	equ	hex 03F830
		nop
		nop
		nop
		nop
		#.b	1
		#.b	2
		jsl	test
here		bra	here
;
;
double		#.l	local0
		store.l
		#.l	local0
		fetch.l
		2*
		rts
test		#.l	local1
		store.l
		#.l	local0
		store.l
		#.l	local1
		fetch.l
		jsl	double
		#.l	local0
		fetch.l
		rts