USERRAM	equ	hex 03e000
SDRAM	equ	hex 040000
svnseg	equ	hex 03f830
data	equ	hex ddccbbaa
		nop
		nop
		nop
		nop
; write longword
		#.l	data
		dup
		dup
		#.l	SRAM
		store.l
		#.l	USERRAM
		store.l	
		#.l	SDRAM
		store.l	
; read words
		jsl flash
		#.l	SRAM
		jsl out.w
		jsl flash
		#.l	USERRAM
		jsl out.w	
		jsl flash
		#.l	SDRAM
		jsl out.w
; read bytes	
		jsl flash	
		#.l	SRAM
		jsl out.b
		jsl flash
		#.l	USERRAM
		jsl out.b			
		jsl flash
		#.l	SDRAM
		jsl out.b
; end
		BEGIN
		AGAIN
;
; subroutines
out.w	fetch.w			;( addr --) WORD fetch from address and display
		#.l	svnseg
		store.l
		rts
out.b	fetch.b			;( addr --) BYTE fetch from address and display
		#.l	svnseg
		store.l
		rts
flash	#.b	hex ff		;( --) display FF
		#.l svnseg
		store.l
		rts
;
SRAM	nop				; when using testbench SRAM a limited address range is available
;