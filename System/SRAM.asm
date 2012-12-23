; N.I.G.E Machine FORTH system software
;
; Copyright and license
;
; The N.I.G.E machine, its design and its source code are Copyright (C) 2012 by Andrew Richard Read and dual licensed.
;    
; (1) For commercial or proprietary use you must obtain a commercial license agreement with Andrew Richard Read (anding_eunding@yahoo.com)
;    
; (2) You can redistribute the N.I.G.E. Machine, its design and its source code and/or modify it under the terms of the GNU General Public 
; License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.  
;
; The N.I.G.E Machine is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of 
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of 
; the GNU General Public License along with this repository.  If not, see <http://www.gnu.org/licenses/>.
;
; ---------------------------------------------------------------------------------------------
		cmd	asmtop			; forget previous label and other definitions
		cmd	MARKER asmtop
;
; ---------------------------------------------------------------------------------------------	
; Hardware registers
TEXT_ZERO	equ	hex f800
background	equ	hex f808
mode		equ	hex f809
RS232rx	equ	hex f80a
RS232tx	equ	hex f80b
RS232baud	equ	hex f80c
HWstatus	equ	hex f80e
HWmask_TBE	equ	02
PS2rx		equ	hex f80f
CLKcounter	equ	hex ff10
MScounter	equ	hex f814
intmask	equ	hex f819		; low byte
intmask_MS	equ	16
SPI.data	equ	hex f81d  
SPI.control	equ	hex f81e  
SPI.status	equ	hex f81f  
SPI.divide	equ	hex f820  
;
; Interrupt mask
;
; ---------------------------------------------------------------------------------------------	
; Memory map (PSDRAM)
RSrxBUF	equ	hex 010000		; RS232 buffer (256 bytes)	
PSBUF		equ	hex 010100		; PS/2 keyboard buffer (256 bytes)
_input_buff	equ	hex 010200		; default input buffer location (used by ACCEPT)
_input_size	equ	hex ff			; default input buffer size (used by ACCEPT)
_PAD		equ	hex 010400		; PAD location (256 bytes below + 256 bytes above here)
_STRING	equ	hex 010500		; buffer for internal string storage (e.g. S")
_TEXT_ZERO	equ	hex 010600		; default text memory location (2 screens * 100 * 75 * 2 bytes)
_TEXT_END	equ	hex 017B30		; one byte beyond the text memory location
_FAT.buf	equ	hex 017B30		; FAT 512 byte storage space location 
_FAT.buffat	equ	hex 017D30		; FAT 512 byte storage space location for file allocation table
_END		equ	hex 017F30		; HEAP location
;
; ---------------------------------------------------------------------------------------------
; Machine specifics
EOL		equ	10			; line separator = ASCII 10
~EOL		equ	13			; ignore this character, not line separator = ASCII 13
;
; .NF flags
PRECEDENCE	equ	128
IMMED		equ	64			; IMMEDIATE
SMDGE		equ	32			; SMUDGE
;
; .SF flags (hi byte)
MUSTINLINE	equ	hex 8000
NOINLINE	equ	hex 4000
;
; opcodes
opDROP		equ	1
op#.B		equ	53
op#.W		equ	54
op#.L		equ	55
opJMP		equ	56
opJSR		equ	58
opRTS		equ	64
opJSRRTS	equ	58 256 * 64 +
opOVER=	equ	5 256 * 28 +
opBEQ		equ	128 256 *
opBRA		equ	192 256 *
;
; ---------------------------------------------------------------------------------------------
; Interrupt vectors
;
V.RST		BRA	START.CF V.RST rel 			; RESET
V.TRAP		RTI
		NOP						; TRAP
V.RDA		BRA	RDA V.RDA rel				; RS232 RDA
V.TBE		BRA	TBE V.TBE rel				; RS232 TBE
V.PS2		BRA	PS2 V.PS2 rel				; PS2
V.MS		BRA	MS V.MS rel				; MS
		RTI
		NOP
		RTI
		NOP
		RTI
		NOP
		RTI
		NOP
		RTI
		NOP
		RTI
		NOP
		RTI
		NOP
		RTI
		NOP
		RTI
		NOP
		RTI
		NOP		
		RTI
		NOP		
;	
; ---------------------------------------------------------------------------------------------
; Interrupt handlers
;
RDA		#.w 	RSrxWPOS	; buffer write position
		fetch.b
		1+
		#.b	hex ff		; constrain to 256 byte length
		and
		dup			 
		#.w 	RSrxRPOS	; buffer read position
		fetch.b
		<>
		IF			; write equals read => skip input
			dup			
			#.w RSrxWPOS
			store.b		; RS_wr
			#.l RSrxBUF		; RS_wr RSrxBUF_S0
			+			; RSrxBUF_S0+RS_wr
			#.w RS232rx	
			fetch.b
			swap			; RS232rx_S0 RSrxBUF_S0+RS_wr
			store.b
			zero			; dummy to balance stack
		THEN
		drop			; needed for skip logic
		rti
RSrxWPOS	dc.b	hex ff
RSrxRPOS	dc.b	hex ff
;
TBE		#.l	RStxCNT
		fetch.l	
		?dup
		IF			; remaining char count zero
			1-			(count-1 )
			#.l	RStxCNT	(count addr)
			store.l
			#.l 	RStxBUF	(buf&)
			dup			(buf& buf&)
			fetch.l		(buf& addr)
			1+			(buf& addr+1)
			dup			(buf& addr+1 addr+1)
			rot			(addr+1 addr+1 buf&)
			store.l		(addr+1)
			fetch.b		(char)
			#.w RS232tx		(char RS232tx)
			store.b
		THEN
TBE.Z		rti
RStxCNT	dc.l	hex 00
RStxBUF	dc.l	hex 00
;
PS2		#.w 	PS2rx	
		fetch.b			( raw)
		#.w 	PS2DECODE.CF		; decode the raw character
		jsr				( char)
		?dup				; process only completed keystrokes
		IF
			#.w 	PSWPOS		; buffer write position
			fetch.b
			1+
			#.b	hex ff		; constrain to 256 byte length
			and			( char n)	
			dup			( char n n)
			#.w 	PSWPOS		; update buffer write position
			store.b		( char n)
			#.l	PSBUF		; add to base address
			+			( char addr)
			store.b		; save key in the buffer
		THEN
		rti
PSWPOS		dc.b	hex ff
PSRPOS		dc.b	hex ff
;
MS		#.w	MS.TIMEOUT
		fetch.l
		?dup
		IF
			1-
			dup
			#.w MS.TIMEOUT
			store.l
			0=
			IF
				#.b	5
				#.w	ERROR.CF
				>R
			THEN
		THEN
		rti
MS.TIMEOUT	dc.l	hex 00
; -----------------------------------------------------------------------------------------------
; boot code	within branch distance from 0 
START.CF	#.w	CLS.CF
		jsr
		#.w	START.0	; First part of power-on message
		#.b	52
		#.w	TYPE.CF
		jsr
		#.w	UNUSED.CF	; Show free bytes
		jsr
		#.w 	UDOT.CF
		jsr
		#.w	START.1	
		#.b	12
		#.w	TYPE.CF
		jsr
		#.w	QUIT.CF
		jmp
;
START.0	dc.b	32 char . char E char . char G char . char I char . char N 32 char * char * char * EOL
		dc.b	EOL EOL char * char * char * 32 char H char T char R char O char F 32 char e char n char i char h char c char a char M 
		dc.b	char B char M 32 char 6 char 1 32 char M char A char R char D char S char P
		dc.b	32 char M char A char R char S 32 char ,
START.1	dc.b	EOL EOL char e char e char r char f 32 char s char e char t char y char b
; ----------------------------------------------------------------------------------------------
; FORTH CORE DICTIONARY
;
SEMIT.LF	dc.l	0		
SEMIT.NF	dc.b	5 128 +	
		dc.b 	char T char I char M char E char S
SEMIT.SF	dc.w 	SEMIT.Z SEMIT.CF del 	
		BEGIN
SEMIT.CF		#.w HWstatus
			fetch.b	
			#.b HWmask_TBE
			and			; TBE bit
		UNTIL		
		#.w RS232tx
		store.b
SEMIT.Z	rts
;	
SKEY?.LF	dc.l 	SEMIT.NF
SKEY?.NF	dc.b	5 128 +
		dc.b	char ? char Y char E char K char S
SKEY?.SF	dc.w	SKEY?.Z SKEY?.CF del
SKEY?.CF	#.w RSrxWPOS	
		fetch.b
		#.w RSrxRPOS
		fetch.b
SKEY?.Z	<>,rts
;
SKEY.LF	dc.l	SKEY?.NF
SKEY.NF	dc.b	4 128 +
		dc.b	char Y char E char K char S
SKEY.SF	dc.w 	SKEY.Z SKEY.CF del
		BEGIN
SKEY.CF	#.w	SKEY?.CF
		jsr
		UNTIL
		#.w 	RSrxRPOS
		dup			(rx& rx&)
		fetch.b		(rx& rx)
		1+			(rx& rx+1)
		#.b	255
		and						; maintain byte width
		dup			(rx& rx+1 rx+1)
		rot			(rx+1 rx+1 rx&)
		store.b		(rx+1)
		#.l 	RSrxBUF	(rx+1 addr)
		+			(addr+rx+1)
		fetch.b		(char)
SKEY.Z	rts
;
; TYPE		(c-addr n --, type a string to RS232_S0)
STYPE.LF	dc.l	SKEY.NF
STYPE.NF	dc.b	5 128 +
		dc.b 	char E char P char Y char T char S
STYPE.SF	dc.w	STYPE.Z STYPE.CF del
STYPE.CF	?dup				( c-addr n true | c-addr false)			
		IF				; check not zero length string
; EMIT the first character
			over			( c-addr n c-addr)
			fetch.b		( c-addr n char)
			#.w	SEMIT.CF	
			jsr			( c-addr n)
; check length of remaining string
			1-			( c-addr n-1)
			?dup			( c-addr n-1 true | c-addr false)
			IF			; check more characters remaining
; write to TBE interrupt handler
				#.w RStxCNT		( c-addr n-1 RStxCNT)
				store.l		( c-addr 
				#.w RStxBUF		( c-addr RStxBUF)
				store.l		( )
				zero			( dummy)
			THEN
		THEN
STYPE.Z	drop,rts		( )
;	
; SZERO ( --) reposition the RS232 read buffer
SZERO.LF	dc.l	STYPE.NF
SZERO.NF	dc.b	5 128 +
		dc.b	char O char R char E char Z char S
SZERO.SF	dc.w	SZERO.Z SZERO.CF del
SZERO.CF	#.b	hex ff
		dup
		#.w	RSrxWPOS
		store.b
		#.w	RSrxRPOS
		store.b
SZERO.Z	rts
;
KKEY?.LF	dc.l 	SZERO.NF
KKEY?.NF	dc.b	5 128 +
		dc.b 	char ? char Y char E char K char K
KKEY?.SF	dc.w	KKEY?.Z KKEY?.CF del
KKEY?.CF	#.w 	PSWPOS	
		fetch.b
		#.w 	PSRPOS
		fetch.b
KKEY?.Z	<>,rts
;
KKEY.LF	dc.l	KKEY?.NF
KKEY.NF	dc.b	4 128 +
		dc.b	char Y char E char K char K
KKEY.SF	dc.w 	KKEY.Z KKEY.CF del
KKEY.CF	BEGIN
		#.w	KKEY?.CF
		jsr
		UNTIL
		#.w 	PSRPOS
		dup			(rx& rx&)
		fetch.b		(rx& rx)
		1+			(rx& rx+1)
		#.b	hex ff
		and						; maintain 256 char width
		dup			(rx& rx+1 rx+1)
		rot			(rx+1 rx+1 rx&)
		store.b		(rx+1)
		#.l 	PSBUF		(rx+1 addr)
		+			(addr+rx+1)
		fetch.b		(char)
KKEY.Z		rts
;
ACCEPT.LF	dc.l	KKEY.NF
ACCEPT.NF	dc.b	6 128 +
		dc.b	char T char P char E char C char C char A
ACCEPT.SF	dc.w	ACCEPT.Z ACCEPT.CF del
ACCEPT.CF	zero			( addr n u) u = current char count
; 	check space in buffer
ACCEPT.0	over			( addr n u n)
		over			( addr n u n u)
		<>
ACCEPT.1	beq 	ACCEPT.10 ACCEPT.1 rel	; reached maximum input length
;	receive next character
		rot			( n u addr)
		#.w 	KEY.CF
		jsr			( n u addr char)
;	test for LF
		dup			( n u addr char char)
		#.b 	EOL		; LF terminator
		<>			( n u addr char flag)
ACCEPT.2	beq	ACCEPT.6 ACCEPT.2 rel	; terminator received
;	test for CR
		dup
		#.b 	13		; CR
		<>
ACCEPT.11	beq	ACCEPT.12 ACCEPT.11 rel	; CR received
;	emit character (backspace is echoed, CR is not)
		dup			( n u addr char char)
		#.w EMIT.CF	
		jsr			( n u addr char)	
;	test for backspace	
		dup			( n u addr char char)
		#.b 	8		; Backspace
		<>			( n u addr char flag)
ACCEPT.3	beq	ACCEPT.7 ACCEPT.3 rel	; backspace received
;	test for tab
		dup
		#.b	9
		=
ACCEPT.14	beq	ACCEPT.15 ACCEPT.14 rel	; not TAB
		drop			( n u addr )		; drop TAB
		#.b	32		( n u addr space)	; replace with space
;	save char
ACCEPT.15	over			( n u addr char addr)
		store.b		( n u addr)
;	advance counters
		1+			( n u addr+1)
		swap			( n addr+1 u)
		1+			( n addr+1 u+1)
		swap			( n u+1 addr+1)
;	repeat
ACCEPT.4	rot			( u+1 addr+1 n)	; rshuffle stack
		rot			( addr+1 n u+1)		
ACCEPT.5	bra 	ACCEPT.0 ACCEPT.5 rel	; repeat next char	
;	ignore CR
ACCEPT.12	drop			( n u addr)
ACCEPT.13	bra	ACCEPT.4 ACCEPT.13 rel
;	process terminator and put the LF into the buffer and increment
ACCEPT.6	swap			( n u char addr)
		store.b		( n u)
;		drop			( n u addr)
;		drop			( n u)
		1+			( n u+1)
		nip,rts		( u+1)
;	process backspace
ACCEPT.7	drop			( n u addr)
		over			( n u addr u)
		0<>			( n u addr flag) 
ACCEPT.8	beq	ACCEPT.4 ACCEPT.8 rel	; ignore backspace when u=0
		1-			( n u addr-1)
		swap			( n addr-1 u)
		1-			( n addr-1 u-1)
		swap			( n u-1 addr-1)
ACCEPT.9	bra 	ACCEPT.4 ACCEPT.9 rel	; backed-up
;	clean-up stack for return
ACCEPT.10	nip			( addr u)
ACCEPT.Z	nip,rts		( u)
;
; UPPER ( char -- CHAR, convert one character to UPPERCASE)
UPPER.LF	dc.l 	ACCEPT.NF
UPPER.NF	dc.b	5 128 +
		dc.b	char R char E char P char P char U
UPPER.SF	dc.w	UPPER.Z UPPER.CF del
UPPER.CF	dup			( char char)
		#.b char a 		( char a)
		< 			( char flag)
		not			( char flag')
		IF
			#.b 32 	( char 32)
			- 		( CHAR)
		THEN	
UPPER.Z	rts
;
; Convert a single character to a number in the current base.
; DIGIT   ( char -- n true | char false )
DIGIT.LF	dc.l	UPPER.NF
DIGIT.NF	dc.b	5 128 +
		dc.b	char T char I char G char I char D
DIGIT.SF	dc.w	DIGIT.Z DIGIT.CF del
DIGIT.CF	#.w	UPPER.CF
		jsr			( CHAR)	; convert char lower to upper
; deal with alphanumerics
		dup 				( char char)
		dup 				( char char char)
		#.b char A 			( char char char A)
		1- 
		>				( char char flag)
		IF 				; alphabetic 
			#.b char A 
			- 
			#.b char 9
			+ 
			1+			( char n)
		ELSE 				; numeric
			dup 			( char char char)
			#.b char 9 
			>			( char n flag)
			IF	; between 9 and A is bad
				drop 	 	( char) 
				zero		( char 0) ; trigger error below
			THEN			
		THEN
; convert modified ASCII value to number
		#.b char 0 			( char n '0')
		-				( char n)
		dup 				( char n n)
		#.w BASE_ 	
		fetch.l			( char n n base)
		<
; check validity
		IF 				; within base range
			dup 			( char n n)
			1+ 			( char n n)
			0>			( char n flag)
			IF 			; a valid digit
				nip 		( n)
				zero		
				not		( n true)
			ELSE 			; not a valid digit
				drop 
				zero		( char false)
			THEN
		ELSE 				; out of base range
			drop 			( char)
			zero			( char false)
		THEN
DIGIT.Z	rts		
;
; D+ 	(ud1 ud2 -- ud3)  double precision arithmetic
D+.LF		dc.l	DIGIT.NF
D+.NF		dc.b	2 128 +
		dc.b	char + char D
D+.SF		dc.w	D+.Z D+.CF del
D+.CF		#.w	intmask		; disable interrupts to protect ADDX flag
		fetch.b
		>R
		>R
		SWAP
		>R
		+
		R>
		R>
		ADDX
		R>
		#.w	intmask
		store.b
D+.Z		rts
;
; D- 	(ud1 ud2 -- ud3)  double precision arithmetic
D-.LF		dc.l	D+.NF
D-.NF		dc.b	2 128 +
		dc.b	char - char D
D-.SF		dc.w	D-.Z D-.CF del
D-.CF		#.w	intmask		; disable interrupts to protect SUBX flag
		fetch.b
		>R	
		>R
		SWAP
		>R
		-
		R>
		R>
		SUBX
		R>
		#.w	intmask
		store.b
D-.Z		rts
;
; >NUMBER ( ud1 c-addr1 u1 -- ud2 c-addr2 u2 , convert till bad char , CORE )
>NUMBER.LF	dc.l	D-.NF
>NUMBER.NF	dc.b	7 128 +
		dc.b	char R char E char B char M char U char N char >
>NUMBER.SF	dc.w	>NUMBER.Z >NUMBER.CF del
>NUMBER.CF	>R			( ud c-addr R: u1)
	BEGIN
		r@ 			( ud c-addr u1 R: u1)
		0>    ; any characters left?
		IF
			dup 		( ud c-addr c-addr)
			fetch.b 	( ud c-addr char)
;			#.w BASE_ 	
;			fetch.l	( ud c-addr char base)
			#.w DIGIT.CF  
			jsr		( ud c-addr , n true | char false)
			IF		; is a digit
				zero
				not	( ud c-addr n true)
			ELSE		; is not a digit
				drop 
				zero	( ud c-addr false)
			THEN
		ELSE
			zero		( ud c-addr false)
		THEN
	WHILE 				( -- ud c-addr n)
		swap 			( ud n c-addr) 
		>R  			( ud1lo ud1hi n R: u c-addr)
; multiply ud1hi * base
		swap  			( ud1lo n ud1hi)
		#.w BASE_ 
		fetch.l		( ud1lo n ud1hi base)
		multu
		drop 			( ud1lo n ud1hi*baselo)	; discard bits 39 - 32
; multiply ud1lo * base
		rot  			( n ud1hi*baselo ud1lo)
		#.w BASE_
		fetch.l		( n ud1hi*baselo ud1lo base)
		multu			( n ud1hi*baselo ud1lo*basello ud1lo*baselhi )
; add in one step the two hi parts of the multiplicaton, and n with the lo part of the multiplciation
		#.w D+.CF  		
		jsr			( ud2 )
		R> 			( ud2 c-addr1 R: u1)
		1+     		( ud2 c-addr2 R: u1)	; increment address
		R> 			( ud2 c-addr2 u1 R:)
		1- 			( ud2 c-addr2 u2 R:)
		>R  			( ud2 c-addr2 R: u2)	; decrement count
	REPEAT
		R>			( ud2 c-addr2 u2)
>NUMBER.Z	rts
;
; NUMBER?	( c-addr u - 0 | n 1 , convert number and return with success flag)
NUMBER?.LF	dc.l	>NUMBER.NF
NUMBER?.NF	dc.b	7 128 +
		dc.b	char ? char R char E char B char M char U char N
NUMBER?.SF	dc.w	NUMBER?.Z NUMBER?.CF del
NUMBER?.CF	over			( c-addr1 u1 c-addr)
		fetch.b		( c-addr1 u1 char)
		#.b char -		( c-addr1 u1 char '-')
		=			( c-addr1 u1 flag)
; test for a minus sign and save flag
		dup			( c-addr1 u1 flag flag)
		>R			( c-addr1 u1 flag R: flag)		
		IF			; minus sign, ignore first character
			1-		( c-addr1 u1' R: flag)
			swap		( u1' c-addr1 R: flag)
			1+		( u1' c-addr1' R: flag)
			swap		( c-addr1' u1' E: flag)
		THEN
; set ud1 = 0
		>R			( c-addr1 R: flag u1)
		>R			( R: flag u1 c-addr1)
		zero
		zero			( ud1 R: flag u1 c-addr1)
		R>			( ud1 c-addr1 R: flag u1)
		R>			( ud1 c-addr1 u1 R: flag)
; convert with >NUMBER
		#.w	>NUMBER.CF
		jsr			( ud2 c-addr1 u2 R: flag)
		IF			; u2 is number of unconverted chars
			drop		( ud2 R: flag)
			drop		( ud2lo R: flag)
			drop		( R: flag)
			R>		( flag)
			drop		( ) 
			zero,rts	( 0)
		THEN
		drop			( ud2 R: flag)
		drop			( u R: flag)
		R>			( u flag)
		IF
			negate		( n)
		THEN
		#.b	1		( n 1)
NUMBER?.Z	rts
;		
; HOLD		( char --, output a character)
HOLD.LF	dc.l	NUMBER?.NF
HOLD.NF	dc.b	4 128 +
		dc.b	char D char L char O char H
HOLD.SF	dc.w 	HOLD.Z HOLD.CF del
; Decrement hld address
HOLD.CF	#.w hld_		(char hld*)
		dup			(char hld* hld*)
		fetch.l		(char hld* hld)
		1-			(char hld* hld-1)
		dup			(char hld* hld-1 hld-1)
		rot			(char hld-1 hld-1 hld*)
		store.l		(char hld-1)
; Store the character in the new address
		store.b
HOLD.Z		rts
;
; <#     ( -- , setup conversion )
<#.LF		dc.l	HOLD.NF
<#.NF		dc.b	2 128 +
		dc.b	char # char <
<#.SF		dc.w	<#.Z <#.CF del
<#.CF		#.l _pad		( pad)
		#.w hld_		( pad hld*)
		store.l	
<#.Z		rts
;
; u#>     ( u -- addr len , finish single precision conversion )
U#>.LF		dc.l	<#.NF
U#>.NF		dc.b	3 128 +
		dc.b	char > char # char U
U#>.SF		dc.w	U#>.Z U#>.CF del
U#>.CF		drop  
		#.w hld_		( hld*)
		fetch.l		( hld)
		#.L _pad  		( hld pad)
		over 			( hld pad hld)
U#>.Z		-,rts			( hld len)
;
; sign   ( n -- , add '-' if negative )
SIGN.LF	dc.l	U#>.NF
SIGN.NF	dc.b	4 128 +
		dc.b	char N char G char I char S
SIGN.SF	dc.w	SIGN.Z SIGN.CF del
SIGN.CF	0<
		If
			#.b	char -
			#.w 	HOLD.CF
			jsr
		THEN
SIGN.Z		rts
;
; U#      ( u -- u , convert one digit, single precision )
U#.LF		dc.l	SIGN.NF
U#.NF		dc.b	2 128 +
		dc.b	char # char U
U#.SF		dc.w	U#.Z U#.CF del
U#.CF		#.w BASE_ 	
		fetch.l		( n base)
		divu			( u-rem u-quot)
		swap
		dup			( u-quot u-rem u-rem)
		#.b	9
		>			( u-quot u-rem flag)
	IF  
		#.b	7 
		+
	THEN
		#.b 	char 0 	( u-quot n char0)
		+ 			( u-quot char)
		#.w	HOLD.CF
		jsr			( u-quot)
U#.Z		rts
;
; U#S     ( u -- u , convert remaining digits )
U#S.LF		dc.l	U#.NF
U#S.NF		dc.b	3 128 +
		dc.b	char S char # char U
U#S.SF		dc.w	U#S.Z U#S.CF del
U#S.CF	BEGIN
		#.w	U#.CF
		jsr			( u )
		dup			( u u)
		0=			( u flag)
	UNTIL
U#S.Z		rts			( u)
;
; ABS ( n -- +n , make a single precision number positive)
ABS.LF		dc.l	U#S.NF
ABS.NF		dc.b	3 128 +
		dc.b	char S char B char A
ABS.SF		dc.w	ABS.Z ABS.CF del
ABS.CF		dup
		0<
	IF
		negate
	THEN
ABS.Z		rts
;	
; . ( n -- , print a single precision signed number)
DOT.LF		dc.l	ABS.NF
DOT.NF		dc.b	1 128 +
		dc.b 	char .
DOT.SF		dc.w	DOT.Z DOT.CF del
DOT.CF		dup			( n n)			
		#.w	ABS.CF		( n u)
		jsr			( n u)
		#.w 	<#.CF		
		jsr			( n u)
		#.w 	U#S.CF
		jsr
		swap			( u n)
		#.w	SIGN.CF	( u)
		jsr
		#.w 	U#>.CF		
		jsr			( c-addr len)
		#.w 	TYPE.CF		
		jsr			
		#.b	32
		#.w	EMIT.CF	
		jsr
DOT.Z		rts
;
; . ( n x -- , print a single precision signed number with at least x printed digits)
DOTR.LF	dc.l	DOT.NF
DOTR.NF	dc.b	2 128 +
		dc.b 	char R char . 
DOTR.SF	dc.w	DOTR.Z DOTR.CF del
DOTR.CF	1-						; deduct 1 since one digit is guaranteed anyway
		>R			( n R:x)		; push digit count to return stack
		dup			( n n)			
		#.w	ABS.CF		( n u)			; prepare for a sign digit
		jsr			( n u)
		#.w 	<#.CF					; initiate conversion
		jsr			( n u)
		R>			( n u x)
		zero			( n u x 0)
		DO						; print the fixed digits
			#.w	U#.CF
			jsr
		LOOP
		#.w 	U#S.CF					; print remaining digits
		jsr
		swap			( u n)
		#.w	SIGN.CF	( u)			; sign
		jsr
		#.w 	U#>.CF					; complete conversion
		jsr			( c-addr len)
		#.w 	TYPE.CF				; output
		jsr			
		#.b	32
		#.w	EMIT.CF	
		jsr
DOTR.Z		rts
;	
; U. ( n -- , print a single precision unsigned number)
UDOT.LF	dc.l	DOTR.NF
UDOT.NF	dc.b	2 128 +
		dc.b 	char . char U
UDOT.SF	dc.w	UDOT.Z UDOT.CF del
UDOT.CF	#.w 	<#.CF
		jsr			( u)
		#.w 	U#S.CF		
		jsr			( u)
		#.w 	U#>.CF
		jsr			( c-addr len)
		#.w 	TYPE.CF		
		jsr
		#.b	32
		#.w	EMIT.CF
		jsr
UDOT.Z		rts
;
; U.R ( u x -- , print a single precision unsigned number)
UDOTR.LF	dc.l	UDOT.NF
UDOTR.NF	dc.b	3 128 +
		dc.b 	char R char . char U
UDOTR.SF	dc.w	UDOTR.Z UDOTR.CF del
UDOTR.CF	1-
		>R
		#.w 	<#.CF
		jsr			( u)
		R>			( u x)
		zero			( u x 0)
		DO						; print the fixed digits
			#.w	U#.CF
			jsr
		LOOP
		#.w 	U#S.CF		
		jsr			( u)
		#.w 	U#>.CF
		jsr			( c-addr len)
		#.w 	TYPE.CF		
		jsr
		#.b	32
		#.w	EMIT.CF
		jsr
UDOTR.Z	rts
;
; MIN	( n1 n2 -- n3, minimum)
MIN.LF		dc.l	UDOTR.NF
MIN.NF		dc.b	3 128 +
		dc.b	char N char I char M
MIN.SF		dc.w	MIN.Z MIN.CF del
MIN.CF		over	( n1 n2 n1)
		over	( n1 n2 n1 n2)
		<	( n1 n2 flag)
		IF		
			drop
		ELSE
			nip
		THEN
MIN.Z		rts
;
; MAX	( n1 n2 -- n3, minimum)
MAX.LF		dc.l	MIN.NF
MAX.NF		dc.b	3 128 +
		dc.b	char X char A char M
MAX.SF		dc.w	MAX.Z MAX.CF del
MAX.CF		over	( n1 n2 n1)
		over	( n1 n2 n1 n2)
		>	( n1 n2 flag)
		IF		
			drop
		ELSE
			nip
		THEN
MAX.Z		rts
;
; WITHIN ( x1 x2 x3 -- flag), true if x2 <= x1 < x3 false otherwise
WITHIN.LF	dc.l	MAX.NF
WITHIN.NF	dc.b	6 128 +
		dc.b	char N char I char H char T char I char W
WITHIN.SF	dc.w	WITHIN.Z WITHIN.CF del
WITHIN.CF	rot			( x2 x3 x1)
		dup			( x2 x3 x1 x1)
		>R			( x2 x3 x1 R:x1)
		>			( x2 flag R:x1)
		R>			( x2 flag x1)
		rot			( flag x1 x2)
		<			
		not			( flag flag)
WITHIN.Z	and,rts		( flag)
;
; COMP ( n1 n2 -- n, return -1 if n1<n2, +1 if n1>2, 0 if n1=n2)
COMP.LF	dc.l	WITHIN.NF
COMP.NF	dc.b	4 128 +
		dc.b 	char P char M char O char C 
COMP.SF	dc.w	COMP.Z COMP.CF del
COMP.CF	over		(n1 n2 n1)
		over		(n1 n2 n1 n2)
		<		(n1 n2 <flag)
		rot		(n2 <flag n1)
		rot		(<flag n1 n2)
		>		(<flag flag)
		negate		(<flag >flag)		\ +1 if n1>n2
		+		(n)			\ combine flags
COMP.Z	rts	
;
; COMPARE ( c-addr1 u1 c-addr2 u2 - n, compare two strings)	
COMPARE.LF	dc.l	COMP.NF
COMPARE.NF	dc.b	7 128 +
		dc.b	char E char R char A char P char M char O char C
COMPARE.SF	dc.w	COMPARE.Z COMPARE.CF del
COMPARE.CF	rot		( addr1 addr2 u2 u1)
		over		( addr1 addr2 u2 u1 u2)
		over		( addr1 addr2 u2 u1 u2 u1)		
		#.w	COMP.CF
		jsr		( addr1 addr2 u2 u1 ><flag)
		>R		( addr1 addr2 u2 u1 R: ><flag)
		#.w	MIN.CF
		jsr		( addr1 addr2 umin)
		?dup
	IF
		zero		( addr1 addr2 u 0)
		DO			( addr1 addr2)
			over		( addr1 addr2 addr1)
			fetch.b	( addr1 addr2 char1)
			over		( addr1 addr2 char1 addr2)
			fetch.b	( addr1 addr2 char1 char2)
			<>		( addr1 addr2 flag)
			IF	
				over		( addr1 addr2 addr1)
				fetch.b	( addr1 addr2 char1)
				over		( addr1 addr2 char1 addr2)	
				fetch.b	( addr1 addr2 char1 char2)	
				#.w	COMP.CF
				jsr		( addr1 addr2 ><flag R: I J n)
				nip		( addr2 ><flag R: I J n)
				nip		( ><flag R: I J n)
				R>
				drop
				R>
				drop
				R>
				drop,rts	( ><flag R:)	\ exit immediately
			THEN
				1+		( addr1 addr2+)
				swap		( addr2+ addr1)
				1+		( addr2+ addr1+)
				swap		( addr1+ addr2+)
		LOOP
	THEN			( addr1 addr2 R: n)
		drop		( addr1 R: n)
		drop		( R: n)
		R>		( n)
COMPARE.Z	rts
;
; $= ( c-addr1 u1 c-addr2 u2 - n, test two strings for equality CASE INSENSITIVE)
$=.LF		dc.l	COMPARE.NF
$=.NF		dc.b	2 128 +
		dc.b	char = char $
$=.SF		dc.w	$=.Z $=.CF del
$=.CF		rot	( c-addr1 c-addr2 u2 u1)
		over	( c-addr1 c-addr2 u2 u1 u2)
		<>	( c-addr1 c-addr2 u2 flag)
		IF					; unequal length
			drop	( c-addr1 c-addr2)
			drop	( c-addr1)
			drop 	( )
			zero 	( 0)
			rts
		THEN
		zero	( c-addr1 c-addr2 u2 0)
		DO	( c-addr1 c-addr2)
			over		( c-addr1 c-addr2 c-addr1)
			fetch.b	( c-addr1 c-addr2 char1)
			#.w	UPPER.CF
			jsr
			over		( c-addr1 c-addr2 char1 c-addr2)
			fetch.b	( c-addr1 c-addr2 char1 char2)
			#.w	UPPER.CF
			jsr
			<>		( c-addr1 c-addr2 flag)
			IF				; unequal characters
				drop	( c-addr1)
				drop	( )
				R>	; remove loop variables
				drop
				R>
				drop
				zero	( 0)
				rts
			THEN
			1+	( c-addr1 c-addr2+)
			swap	( c-addr2+ c-addr1)
			1+	( c-addr2+ c-addr1+)
			swap	( c-addr1+ c-addr2+)
		LOOP
		drop	( c-addr1)
		drop 	( )
		zero				
$=.Z		not,rts				; equal strings
;
; COUNT ( addr -- c-addr n)
;
COUNT.LF	dc.l	$=.NF
COUNT.NF	dc.b	5 128 +
		dc.b 	char T char N char U char O char C
COUNT.SF	dc.w	COUNT.Z COUNT.CF del
COUNT.CF	dup		( addr addr)
		1+		( addr c-addr)
		swap		( c-addr addr)
		fetch.b	( c-addr n)
COUNT.Z	rts
;
; FIND (addr -- addr 0 | xt 1 | xt -1)
;
FIND.LF	dc.l	COUNT.NF
FIND.NF	dc.b	4 128 + 
		dc.b 	char D char N char I char F
FIND.SF	dc.w	FIND.Z FIND.CF del
FIND.CF	dup		( addr addr)
		#.w	COUNT.CF
		jsr		( addr c-addr1 n1)			
; top of dictonary
		#.w	LAST-NF	( addr c-addr1 n1 &LAST-NF)
		fetch.l		( addr c-addr1 n1 NF)
		BEGIN			( addr c-addr1 n1 NF)
; check smudge bit not set
			dup			( addr c-addr1 n1 NF NF)
			>R			( addr c-addr1 n1 NF R: NF)	
			fetch.b		( addr c-addr1 n1 n' R: NF)
			#.b	32		( addr c-addr1 n1 n' 32 R: NF)
			and			( addr c-addr1 n1 flag R: NF)			
			0=			( addr c-addr1 n1 flag' R: NF)
			IF
; prepare and perform string equality test
				over			( addr c-addr1 n1 c-addr1 R: NF)
				over			( addr c-addr1 n1 c-addr1 n1 R: NF)
				R@			( addr c-addr1 n1 c-addr1 n1 NF R: NF)
				dup			( addr c-addr1 n1 c-addr1 n1 NF NF R: NF)
				1+			( addr c-addr1 n1 c-addr1 n1 NF c-addr2 R: NF)
				swap			( addr c-addr1 n1 c-addr1 n1 c-addr2 NF R: NF)
				fetch.b		( addr c-addr1 n1 c-addr1 n1 c-addr2 n2' R: NF)
				#.b 	31		( addr c-addr1 n1 c-addr1 n1 c-addr2 n2' 31 R: NF)
				and			( addr c-addr1 n1 c-addr1 n1 c-addr2 n2 R: NF)
				#.w	$=.CF	
				jsr			( addr c-addr1 n1 flag R: NF)
				IF			( addr c-addr1 n1 R: NF)
; match found
					drop			( addr c-addr1)
					drop			( addr)
					drop			( )
					R>			( NF)
					dup			( NF NF)
					fetch.b		( NF n')
					dup			( NF n' n')
; check immediate bit 
					#.b	64		( NF n' n' 64)
					and			( NF n' I)
					IF
						#.b 1			( NF n' 1)
					ELSE
						zero			( NF n' 0)
						not			( NF n' -1)
					THEN			( NF n' Iflag)
					>R			( NF n' R: Iflag)
; get CF and exit					
					#.b	31		( NF n' 31 R: Iflag)
					and			( NF n R: Iflag)
					+			( NF+n R: Iflag)
					#.b	3		( NF+n 3 R: Iflag)
					+			( CF R: Iflag)
					R>			( CF Iflag)
					rts		
				THEN
			THEN		
; not matched. get next word
			R>		( addr c-addr1 n1 NF1)
			#.b	4	( addr c-addr1 n1 NF1 4)
			-		( addr c-addr1 n1 LF)
			fetch.l	( addr c-addr1 n1 NF2)
			?dup		( addr c-addr1 n1, false | NF2 true)
			0=		( addr c-addr1 n1, true  | NF2 false)
		UNTIL		( addr c-addr1 n1 | addr c-addr1 n1 NF2)
		drop		( addr c-addr1)
		drop		( addr )
FIND.Z		zero,rts	( addr 0)
;
WORDS.LF	dc.l 	FIND.NF
WORDS.NF	dc.b	5 128 +
		dc.b	char S char D char R char O char W
WORDS.SF	dc.w	WORDS.Z WORDS.CF del
WORDS.CF	#.w	CR.CF
		jsr
		#.w	TAB		( &tab)
		dup			( &tab &tab)
		fetch.l		( &tab tab)
		swap			( tab &tab) 
		#.b	10		( tab &tab 20)
		over			( tab &tab 20 &tab)
		store.l		( tab &tab)
		#.w	LAST-NF	( tab &tab LF)
;		zero
;		>R	; save count
		BEGIN
			fetch.l
			?dup
		WHILE
;			R>			; retrive count
;			1+
;			>R			; save count
			dup		( NF NF)
			1+		( NF NF+1)
			over		( NF NF+1 NF)
			fetch.b	( NF NF+1 n')
			#.b	31	( NF NF+1 n' 31)
			and		( NF NF+1 n)
			#.w	TYPE.CF
			jsr		( NF)
			#.b	09
			#.w	EMIT.CF
			jsr
			#.b	4
			-		( LF)
		REPEAT
		store.l			; save original tab setting
;		R>			; retrive count
WORDS.Z	rts
;
; PARSE ( char -- c-addr n, parse the input buffer into the parse buffer)
PARSE.LF	dc.l	WORDS.NF
PARSE.NF	dc.b	5 128 +
		dc.b 	char E char S char R char A char P
PARSE.SF	dc.w	PARSE.Z PARSE.CF del
PARSE.CF	>R			( R: char)
		#.w	HERE_		; use the natural location of the next name field as the parse buffer
		fetch.l		
		#.b	5		; +4 to get to the NF, then +1 again to get to the first char in the NF
		+			( parse_buff R: char)
		#.l	input_buff	
		fetch.l		( parse_buff input_buff R: char)
		#.l	IN_LEN		
		fetch.l		( parse_buff input_buff IN_LEN R: char)
		over			( parse_buff input_buff IN_LEN input_buff R: char)
		+			( parse_buff input_buff i_buff_last R: char)
		>R			( parse_buff input_buff R: char i_buff_last)
		#.l	>IN_
		fetch.l		( parse_buff input_buff IN R: char ibl)
		+			( parse_buff i_buff R: char ibl)
		BEGIN
; check for buffer overrun
			dup			( p_buff i_buff i_buff R: char ibl)
			R@			( p_buff i-buff i_buff ibl R: char ibl)
			<			( p_buff i_buff flag R: char ibl)
PARSE.0		beq	PARSE.1 PARSE.0 rel
; fetch next character from input buffer
			dup			( p_buff i_buff i_buff R: char ibl)
			fetch.b		( p_buff i_buff charN R: char ibl)
; test against delimiter
			R>			( p_buff i_buff charN ibl R: char)
			R@			( p_buff i_buff charN ibl char R: char)
			swap			( p_buff i_buff charN char ibl R: char)
			>R			( p_buff i_buff charN char R: char ibl)
			dup
			#.b	32	
			=			; if delimiter is space, also delimit on CR, LF and TAB
			IF
				over		( .. charN char charN)
				<>		( .. charN flag)
				over		( .. charN flag charN)
				#.b	13
				<>		( .. charN flag flag)
				and		( .. charN flag)
				over
				#.b	10
				<>
				and
				over
				#.b	9
				<>
				and	
			ELSE
				over			( p_buff i_buff charN char charN R: char ibl)
				<>			( p_buff i_buff charN flag R: char ibl)
			THEN
		WHILE
; copy char to parse buffer and increment i_buff and p_buff
			rot			( i_buff charN p_buff R: char ibl)
			dup			( i_buff charN p_buff p_buff R: char ibl)
			rot			( i_buff p_buff p_buff charN R: char ibl)
;			#.w	UPPER.CF	
;			jsr			( i_buff p_buff p_buff CHAR R: char ibl)
			swap			( i_buff p_buff CHAR p_buff R: char ibl)
			store.b		( i_buff p_buff R: char ibl)
			1+			( i_buff p_buff+ R: char ibl)
			swap			( p_buff+ i_buff R: char ibl)
			1+			( p_buff+ i_buff+ R: char ibl)
		REPEAT
; at delimiter or end of buffer, save IN, finalize return paramaters
		drop			( p_buff i_buff R: char ibl)
		1+			( p_buff i_buff+ R: char ibl)		; if delimiter, increment i_buff
PARSE.1	#.l	input_buff
		fetch.l		( p_buff i_buff+ i_buff R: char ibl)
		-			( p_buff IN R: char ibl)
		#.l	>IN_		( p_buff IN+ >IN R: char ibl)
		store.l		( p_buff R: char ibl)	
		R>			( p_buff char R: ibl)
		drop			( p_buff R: ibl)
		R>			( p_buff ibl)
		drop			( p_buff)
		#.w	HERE_	
		fetch.l		( p_buff parse_buff)
		#.b	5
		+			( p_buff parse_buff)
		swap			( parse_buff p_buff)
		over			( parse_buff p_buff parse_buff)
PARSE.Z	-,rts			( parse_buff n)
;
; WORD ( char -- addr, parse the next word from the input buffer)
WORD.LF	dc.l	PARSE.NF
WORD.NF	dc.b	4 128 +
		dc.b	char D char R char O char W
WORD.SF	dc.w	WORD.Z WORD.SF del
WORD.CF	BEGIN
			dup		( char char)
			#.w	PARSE.CF
			jsr		( char c-addr n)
			dup		( char c-addr n flag)
			0=		( char c-addr n flag')
WORD.0			beq	WORD.2 WORD.0 rel				; parse returned at least one character
			#.l IN_LEN
			fetch.l	( char c-addr n IN_LEN)
			#.l >IN_
			fetch.l	( char c-addr n IN_LEN IN)
			>		( char c-addr n flag)
WORD.1			beq	WORD.2 WORD.1 rel
			drop		( char c-addr)
			drop		( char)
		AGAIN	
WORD.2		swap		( char n c-addr)
		1-		( char n addr)
		swap		( char addr n)
		over		( char addr n addr)
		store.b	( char addr)
WORD.Z		nip,rts	( addr)	
;	
; INTERPRET, interpert a line from the input buffer
INTERPRET.LF	dc.l	WORD.NF
INTERPRET.NF	dc.b	9 128 +
		dc.b	char T char E char R char P char R char E char T char N char I
INTERPRET.SF	dc.w	INTERPRET.Z INTERPRET.CF del
INTERPRET.CF	#.w	{INTERPRET}.CF
		jsr							; inner workings of INTERPRET
		#.w	STATE_
		fetch.l
		0=
		IF	; compile is not set		
			#.w	INTERPRET.1				; TYPE OK-depth CR
			#.b	4
			#.w	TYPE.CF
			jsr
			#.w	DEPTH.CF
			jsr
			#.w	UDOT.CF
			jsr
		THEN
		#.b	EOL
		#.w	EMIT.CF
		jsr
INTERPRET.Z	rts
INTERPRET.1	dc.b 	 char - char K char O  32
;
; inner workings of INTERPET
{INTERPRET}.CF BEGIN
			#.w	IN_LEN 
			fetch.l	
			#.w	>IN_ 
			fetch.l	( IN_LEN IN)
			> 		( flag)		; confirm that input_buff has characters waiting to be processes
		WHILE
			#.b	32 
			#.w	WORD.CF
			jsr			( char -- addr)	; scan input_buff from >IN, skip leading blanks
			dup			( addr addr)
			fetch.b		( addr n)
			IF						; confirm some characters were parsed
				#.w	FIND.CF	
				jsr			( addr -- xt n | addr 0)  	; lookup the word
				?dup
				IF					; valid execution token
					0<			( xt flag)
					IF					; not IMMEDIATE 
						#.w	STATE_
						fetch.l	( xt STATE)
						IF				; compile is set
							#.w COMPILE,.CF
						THEN
					THEN
					jsr					; either execute or compile
				ELSE					; not an XT
					dup			( addr addr)
					#.w	COUNT.CF
					jsr			( addr c-addr n)
					#.w	NUMBER?.CF	(
					jsr			( addr, 0 | n 1)
					0=
					IF						; ERROR if not a valid number
						#.w 	PALETTE 2 +
						fetch.b
						#.w	INK
						store.b
						#.w	CR.CF
						jsr
						#.w 	COUNT.CF			; echo the input string
						jsr
						#.w	TYPE.CF
						jsr
						#.b	1				; ERROR#
						#.w	ERROR.CF
						jsr					
					THEN	
					nip			( n)
					#.w	STATE_
					fetch.l
					IF	; compile is set
						#.w LITERAL.CF
						jsr
					THEN
				THEN
			ELSE
				drop					; drop unneeded address of parse buffer
			THEN
		REPEAT
{INTERPRET}.Z	rts
;	
ERROR.LF	dc.l 	INTERPRET.NF
ERROR.NF	dc.b	5 128 +
		dc.b	char R char O char R char R char E
ERROR.SF	dc.w	ERROR.Z ERROR.CF del
ERROR.CF	#.w 	PALETTE 2 +
		fetch.b
		#.w	INK
		store.b
		#.w	ERROR.0	; TYPE ERROR CR
		#.b	7
		#.w	TYPE.CF
		jsr
		#.w	DOT.CF
		jsr
		#.b	EOL
		#.w	EMIT.CF
		jsr
		#.w	QUIT.CF
		jsr	
ERROR.Z	rts
ERROR.0	dc.b 	32 char R char O char R char R char E EOL
;
;ABORT
ABORT.LF	dc.l 	ERROR.NF
ABORT.NF	dc.b	5 128 +
		dc.b 	char T char R char O char B char A
ABORT.SF	dc.w	ABORT.Z ABORT.CF del
ABORT.CF	#.w	QUIT.CF
		jsr
ABORT.Z	rts		; never reached
;
; QUIT
QUIT.LF	dc.l	ABORT.NF
QUIT.NF	dc.b	4 128 +
		dc.b 	char T char I char U char Q
QUIT.SF	dc.w	QUIT.Z QUIT.CF del
QUIT.CF	zero
		#.w	TIMEOUT.CF				; clear any timeouts
		jsr
		zero 						; zero stack pointers
		RSP!
		zero 
		PSP!
		zero						; set state to interpreting
		#.w	STATE_
		store.l
		#.l	_input_buff				; restore default input buffer location and size
		#.w	input_buff
		store.l	
		#.l	_input_size
		#.w	input_size			
		store.l
		BEGIN
			#.w	PALETTE 0 +			; set input colour
			fetch.b
			#.w	INK
			store.b
			#.w	input_buff
			fetch.l
			#.w	input_size
			fetch.l			( input_buff size)
			#.w	ACCEPT.CF 			; fill input_buff from the terminal 
			jsr				( input_buff size -- len)	
			#.w	IN_LEN 			; save number of characters in input buffer
			store.l					
			zero
			#.w	>IN_ 				; set >IN to zero
			store.l		
			#.w	PALETTE 1 +			; set output color
			fetch.b
			#.w	INK
			store.b
			#.w	INTERPRET.CF			; INTERPRET the line
			jsr		
		AGAIN
QUIT.Z		rts		; never reached
;
; SOURCE ( -- c-addr len , return the address and length of the input buffer)
SOURCE.LF	dc.l	QUIT.NF
SOURCE.NF	dc.b	6 128 +
		dc.b 	char E char C char R char U char O char S
SOURCE.SF	dc.w	SOURCE.Z SOURCE.CF del
SOURCE.CF	#.w	input_buff
		fetch.l
		#.w	input_size
		fetch.l
SOURCE.Z	rts
;	
; SAVE_INPUT ( --), save the current input source specificiation
SAVE-INPUT.LF	dc.l	SOURCE.NF
SAVE-INPUT.NF	dc.b	10 128 +
		dc.b	char T char U char P char N char I char - char E char V char A char S
SAVE-INPUT.SF	dc.w	SAVE-INPUT.Z SAVE-INPUT.Z del
SAVE-INPUT.CF	#.w	input_buff
		fetch.l
		#.w	input_buff_a
		store.l
		#.w	input_size
		fetch.l	
		#.w	input_size_a
		store.l
		#.w	IN_LEN
		fetch.l
		#.w	IN_LEN_a
		store.l
		#.w	>IN_
		fetch.l
		#.w	>IN_a	
		store.l
SAVE-INPUT.Z	rts
;
; RESTORE-INPUT ( --), restore the current input source specification
RESTORE-INPUT.LF	dc.l	SAVE-INPUT.NF
RESTORE-INPUT.NF	dc.b	13 128 +
			dc.b	char T char U char P char N char I char - char E char R char O char T char S char E char R
RESTORE-INPUT.SF	dc.w	RESTORE-INPUT.Z RESTORE-INPUT.CF del
RESTORE-INPUT.CF	#.w	input_buff_a
			fetch.l
			#.w	input_buff
			store.l
			#.w	input_size_a
			fetch.l	
			#.w	input_size
			store.l
			#.w	IN_LEN_a
			fetch.l
			#.w	IN_LEN
			store.l
			#.w	>IN_a
			fetch.l
			#.w	>IN_	
			store.l
RESTORE-INPUT.Z	rts
;
; EVALUATE ( c-addr u --)
EVALUATE.LF	dc.l	RESTORE-INPUT.NF
EVALUATE.NF	dc.b	8 128 +
		dc.b	char E char T char A char U char L char A char V char E
EVALUATE.SF	dc.w	EVALUATE.Z EVALUATE.CF del
EVALUATE.CF	>R
		>R
		#.w 	SAVE-INPUT.CF
		jsr
		R>
		R>			( <input-state> c-addr u --)
		#.w	IN_LEN
		store.l
		#.w	input_buff
		store.l
		zero
		#.w	>IN_
		store.l
		#.w	{INTERPRET}.CF
		jsr
		#.w	RESTORE-INPUT.CF
		jsr
EVALUATE.Z	rts
;
; FORTH words with direct instructions
;
NOP.LF		dc.l	EVALUATE.NF
NOP.NF		dc.b	3 128 +
		dc.b	char P char O char N
NOP.SF		dc.w	1
NOP.CF		nop,rts
;
DROP.LF	dc.l	NOP.NF
DROP.NF	dc.b	4 128 +
		dc.b	char P char O char R char D
DROP.SF	dc.w	1
DROP.CF	drop,rts
;
DUP.LF		dc.l	DROP.NF
DUP.NF		dc.b	3 128 +
		dc.b	char P char U char D
DUP.SF		dc.w	1
DUP.CF		dup,rts
;
?DUP.LF	dc.l	DUP.NF
?DUP.NF	dc.b	4 128 +
		dc.b	char P char U char D char ?
?DUP.SF	dc.w	2
?DUP.CF	?dup
		rts
;
SWAP.LF	dc.l	?DUP.NF
SWAP.NF	dc.b	4 128 +
		dc.b	char P char A char W char S
SWAP.SF	dc.w	1
SWAP.CF	swap,rts
;
OVER.LF	dc.l	SWAP.NF
OVER.NF	dc.b	4  128 +
		dc.b	char R char E char V char O
OVER.SF	dc.w	1
OVER.CF	over,rts
;
NIP.LF		dc.l	OVER.NF
NIP.NF		dc.b	3 128 +
		dc.b	char P char I char N
NIP.SF		dc.w	1
NIP.CF		nip,rts
;
ROT.LF		dc.l	NIP.NF
ROT.NF		dc.b	3 128 +
		dc.b	char T char O char R
ROT.SF		dc.w	1
ROT.CF		rot,rts
;
-ROT.LF	dc.l	ROT.NF
-ROT.NF	dc.b	4 128 +
		dc.b	char T char O char R char -
-ROT.SF	dc.w	-ROT.Z -ROT.CF del
-ROT.CF	rot
-ROT.Z		rot,rts
;
>R.LF		dc.l	-ROT.NF
>R.NF		dc.b	2 128 +
		dc.b	char R char >
>R.SF		dc.w	2
>R.CF		>R
		rts
;
R@.LF		dc.l	>R.NF
R@.NF		dc.b	2 128 +
		dc.b	char @ char R
R@.SF		dc.w	2
R@.CF		R@
		rts
;
I.LF		dc.l	R@.NF
I.NF		dc.b	1 128 +
		dc.b	char I
I.SF		dc.w	2 MUSTINLINE +
I.CF		R@
		rts
;
J.LF		dc.l	I.NF
J.NF		dc.b	1 128 +
		dc.b	char J
J.SF		dc.w	J.Z J.CF del MUSTINLINE +
J.CF		R>	( I R:L1 J L2)
		R>	( I L1 R:J L2)
		R@	( I L1 J R:J L2)
		rot	( L1 J I R:J L2)
		rot	( J I L1 R:J L2)
		>R	( J I R:L1 J L2)
		>R	( J R:I L1 J L2)
J.Z		rts
;		
R>.LF		dc.l	J.NF
R>.NF		dc.b	2 128 +
		dc.b	char > char R
R>.SF		dc.w	2
R>.CF		R>
		rts
;
DEPTH.LF	dc.l	R>.NF
DEPTH.NF	dc.b	5 128 +
		dc.b	char H char T char P char E char D
DEPTH.SF	dc.w	1
DEPTH.CF	PSP@,rts
;
RDEPTH.LF	dc.l	DEPTH.NF
RDEPTH.NF	dc.b	6 128 +
		dc.b	char H char T char P char E char D char R
RDEPTH.SF	dc.w	2
RDEPTH.CF	RSP@
		rts
;
+.LF		dc.l	RDEPTH.NF
+.NF		dc.b	1 128 +
		dc.b 	char +
+.SF		dc.w	1
+.CF		+,rts
;
-.LF		dc.l	+.NF
-.NF		dc.b	1 128 +
		dc.b	char -
-.SF		dc.w	1
-.CF		-,rts
;
NEGATE.LF	dc.l	-.NF
NEGATE.NF	dc.b	6 128 +
		dc.b	char E char T char A char G char E char N
NEGATE.SF	dc.w	1
NEGATE.CF	negate,rts
;
1+.LF		dc.l	NEGATE.NF
1+.NF		dc.b	2 128 +
		dc.b	char + char 1
1+.SF		dc.w	1
1+.CF		1+,rts
;
1-.LF		dc.l	1+.NF
1-.NF		dc.b	2 128 +
		dc.b	char - char 1
1-.SF		dc.w	1
1-.CF		1-,rts
;
2*.LF		dc.l	1-.NF
2*.NF		dc.b	2 128 +
		dc.b	char * char 2
2*.SF		dc.w	1
2*.CF		2*,rts
;
2/.LF		dc.l	2*.NF
2/.NF		dc.b 	2 128 +
		dc.b	char / char 2
2/.SF		dc.w	1
2/.CF		2/,rts
;
U2/.LF		dc.l	2/.NF
U2/.NF		dc.b 	3 128 +
		dc.s	U2/
U2/.SF		dc.w	1
U2/.CF		lsr,rts
;
M*.LF		dc.l	U2/.NF
M*.NF		dc.b	2 128 +
		dc.s 	M*
M*.SF		dc.w	2
M*.CF		mults
		rts
;
UM*.LF		dc.l	M*.NF
UM*.NF		dc.b	3 128 +
		dc.s 	UM* 
UM*.SF		dc.w	2
UM*.CF		multu
		rts
;
*.LF		dc.l	UM*.NF
*.NF		dc.b	1 128 +
		dc.b 	char *
*.SF		dc.w	2
*.CF		mults
		drop,rts
;
/.LF		dc.l	*.NF
/.NF		dc.b	1 128 +
		dc.b	char /
/.SF		dc.w	2
/.CF		divs
		nip,rts
;
MOD.LF		dc.l	/.NF
MOD.NF		dc.b	3 128 +
		dc.b	char D char O char M
MOD.SF		dc.w	2
MOD.CF		divs
		drop,rts
;
*/.LF		dc.l	MOD.NF
*/.NF		dc.b	2 128 +
		dc.b	char / char *
*/.SF		dc.w	*/.Z */.CF del
*/.CF		>R
		mults
		drop
		R>
		divs
*/.Z		nip,rts
;
*/MOD.LF	dc.l	*/.NF
*/MOD.NF	dc.b	5 128 +
		dc.b	char D char O char M char / char *
*/MOD.SF	dc.w	*/MOD.Z */MOD.CF del
*/MOD.CF	>R
		mults
		drop
		R>
		divs
*/MOD.Z	rts
;
/MOD.LF	dc.l	*/MOD.NF
/MOD.NF	dc.b	4 128 +
		dc.b char D char O char M char /
/MOD.SF	dc.w	2
/MOD.CF	divs
		rts
;
=.LF		dc.l	/MOD.NF
=.NF		dc.b	1 128 +
		dc.b	char =
=.SF		dc.w	1
=.CF		=,rts
;
<>.LF		dc.l	=.NF
<>.NF		dc.b	2 128 +
		dc.b	char > char <
<>.SF		dc.w	1
<>.CF		<>,rts
;
<.LF		dc.l	<>.NF
<.NF		dc.b	1 128 +
		dc.b	char <
<.SF		dc.w	1
<.CF		<,rts
;
>.LF		dc.l	<.NF
>.NF		dc.b	1 128 +
		dc.b	char >
>.SF		dc.w	1
>.CF		>,rts
;
U<.LF		dc.l	>.NF
U<.NF		dc.b	2 128 +
		dc.b	char < char U
U<.SF		dc.w	1
U<.CF		U<,rts
;
U>.LF		dc.l	U<.NF
U>.NF		dc.b	2 128 +
		dc.b	char > char U
U>.SF		dc.w	1
U>.CF		U>,rts
;
0=.LF		dc.l	U>.NF
0=.NF		dc.b	2 128 +
		dc.b	char = char 0
0=.SF		dc.w	1
0=.CF		0=,rts
;
NOT.LF		dc.l	0=.NF
NOT.NF		dc.b	3 128 +
		dc.b	char T char O char N
NOT.SF		dc.w	1
NOT.CF		0=,rts
;
0<>.LF		dc.l	NOT.NF
0<>.NF		dc.b	3 128 +
		dc.b	char > char < char 0
0<>.SF		dc.w	1
0<>.CF		0<>,rts
;
0<.LF		dc.l	0<>.NF
0<.NF		dc.b	2 128 +
		dc.b	char < char 0
0<.SF		dc.w	1
0<.CF		0<,rts
;
0>.LF		dc.l	0<.NF
0>.NF		dc.b	2 128 +
		dc.b	char > char 0
0>.SF		dc.w	1
0>.CF		0>,rts
;
FALSE.LF	dc.l	0>.NF
FALSE.NF	dc.b	5 128 +
		dc.b	char E char S char L char A char F
FALSE.SF	dc.w	1
FALSE.CF	false,rts
;
0.LF		dc.l	FALSE.NF
0.NF		dc.b	1 128 +
		dc.s	0
0.SF		dc.w	1
0.CF		false,rts
;
-1.LF		dc.l	0.NF
-1.NF		dc.b	2 128 +
		dc.s	-1
-1.SF		dc.w	2		
-1.CF		zero
		0=,rts
;
TRUE.LF	dc.l	-1.NF
TRUE.NF	dc.b	4 128 +
		dc.b	char E char U char R char T
TRUE.SF	dc.w	2
TRUE.CF	zero
		0=,rts
;
AND.LF		dc.l	TRUE.NF
AND.NF		dc.b	3 128 +
		dc.b	char D char N char A
AND.SF		dc.w	1
AND.CF		and,rts
;
OR.LF		dc.l	AND.NF
OR.NF		dc.b	2 128 +
		dc.b	char R char O
OR.SF		dc.w	1
OR.CF		or,rts
;
INVERT.LF	dc.l	OR.NF
INVERT.NF	dc.b	6 128 +
		dc.b	char T char R char E char V char N char I
INVERT.SF	dc.w	1
INVERT.CF	invert,rts
;
XOR.LF		dc.l	INVERT.NF
XOR.NF		dc.b	3 128 +
		dc.b	char R char O char X
XOR.SF		dc.w	1
XOR.CF		xor,rts
;
; LSHIFT (x1 u -- x2)
LSHIFT.LF	dc.l	XOR.NF
LSHIFT.NF	dc.b	6 128 +
		dc.b	char T char F char I char H char S char L
LSHIFT.SF	dc.w	LSHIFT.Z LSHIFT.CF del
LSHIFT.CF	BEGIN
			?dup	( x1 u true -- x1 false)
		WHILE
			1-	( x1 u')
			swap	( u' x1)
			lsl	( u' x2)
			swap	( x2 u')
		REPEAT
LSHIFT.Z	rts
;
; RSHIFT (x1 u -- x2)
RSHIFT.LF	dc.l	LSHIFT.NF
RSHIFT.NF	dc.b	6 128 +
		dc.b	char T char F char I char H char S char R
RSHIFT.SF	dc.w	RSHIFT.Z RSHIFT.CF del
RSHIFT.CF	BEGIN
			?dup	( x1 u true -- x1 false)
		WHILE
			1-	( x1 u')
			swap	( u' x1)
			lsr	( u' x2)
			swap	( x2 u')
		REPEAT
RSHIFT.Z	rts
;
CR.LF		dc.l	RSHIFT.NF
CR.NF		dc.b	2 128 +
		dc.b	char R char C
CR.SF		dc.w	CR.Z CR.CF del
CR.CF		#.b	EOL
		#.w	EMIT.CF
		jsr
CR.Z		rts
;
SPACE.LF	dc.l	CR.NF
SPACE.NF	dc.b	5 128 +
		dc.b	char E char C char A char P char S
SPACE.SF	dc.w	SPACE.Z SPACE.CF del
SPACE.CF	#.b	32
		#.w	EMIT.CF
		jsr
SPACE.Z	rts
;	
SPACES.LF	dc.l	SPACE.NF
SPACES.NF	dc.b	6 128 +
		dc.b	char S char E char C char A char P char S
SPACES.SF	dc.w	SPACES.Z SPACES.CF del
SPACES.CF	zero	( limit index)
		DO
			#.b 	32
			#.w	EMIT.CF
			jsr
		LOOP
SPACES.Z	rts
;
XBYTE.LF	dc.l	SPACES.NF
XBYTE.NF	dc.b	5 128 +
		dc.b	char E char T char Y char B char X
XBYTE.SF	dc.w	1
XBYTE.CF	xbyte,rts
;
XWORD.LF	dc.l	XBYTE.NF
XWORD.NF	dc.b	5 128 +
		dc.b	char D char R char O char W char X
XWORD.SF	dc.w	1
XWORD.CF	xword,rts
;
@.LF		dc.l	XWORD.NF
@.NF		dc.b	1 128 +
		dc.b 	char @
@.SF		dc.w 	2
@.CF		fetch.l
		rts
;
!.LF		dc.l	@.NF
!.NF		dc.b	1 128 +
		dc.b 	char !
!.SF		dc.w 	2
!.CF		store.l
		rts
;
; +! ( n addr --)
+!.LF		dc.l	!.NF
+!.NF		dc.b	2 128 +
		dc.b 	char ! char +
+!.SF		dc.w 	+!.Z +!.CF del
+!.CF		swap		( addr n)
		over		( addr n addr)
		fetch.l	( addr n X)
		+		( addr Y)
		swap		( Y addr)
		store.l	( )
+!.Z		rts	
;	
W@.LF		dc.l	+!.NF
W@.NF		dc.b	2 128 +
		dc.b 	char @ char W
W@.SF		dc.w 	2
W@.CF		fetch.w
		rts
;
W!.LF		dc.l	W@.NF
W!.NF		dc.b	2 128 +
		dc.b 	char ! char W
W!.SF		dc.w 	2
W!.CF		store.w
		rts
;		
; W+! ( n addr --)
W+!.LF		dc.l	W!.NF
W+!.NF		dc.b	3 128 +
		dc.b 	char ! char + char W
W+!.SF		dc.w 	W+!.Z W+!.CF del
W+!.CF		swap		( addr n)
		over		( addr n addr)
		fetch.w	( addr n X)
		+		( addr Y)
		swap		( Y addr)
		store.w	( )
W+!.Z		rts	
;
C@.LF		dc.l	W+!.NF
C@.NF		dc.b	2 128 +
		dc.b 	char @ char C
C@.SF		dc.w 	2
C@.CF		fetch.b
		rts
;
C!.LF		dc.l	C@.NF
C!.NF		dc.b	2 128 +
		dc.b 	char ! char C
C!.SF		dc.w 	2
C!.CF		store.b
		rts
;
; C+! ( n addr --)
C+!.LF		dc.l	C!.NF
C+!.NF		dc.b	3 128 +
		dc.b 	char ! char + char C
C+!.SF		dc.w 	C+!.Z C+!.CF del
C+!.CF		swap		( addr n)
		over		( addr n addr)
		fetch.b	( addr n X)
		+		( addr Y)
		swap		( Y addr)
		store.b	( )
C+!.Z		rts
;	
DECIMAL.LF	dc.l	C+!.NF
DECIMAL.NF	dc.b	7 128 +
		dc.b	char L char A char M char I char C char E char D
DECIMAL.SF	dc.w	DECIMAL.Z DECIMAL.CF del
DECIMAL.CF	#.b	10
		#.w	BASE_
		store.l
DECIMAL.Z	rts		
;
HEX.LF		dc.l	DECIMAL.NF
HEX.NF		dc.b	3 128 +
		dc.b	char X char E char H
HEX.SF		dc.w	HEX.Z HEX.CF del
HEX.CF		#.b	16
		#.w	BASE_
		store.l
HEX.Z	rts	
;
BINARY.LF	dc.l	HEX.NF
BINARY.NF	dc.b	6 128 +
		dc.b	char Y char R char A char N char I char B
BINARY.SF	dc.w	BINARY.Z BINARY.CF del
BINARY.CF	#.b	2
		#.w	BASE_
		store.l
BINARY.Z	rts	
;	
CHAR.LF	dc.l	BINARY.NF
CHAR.NF	dc.b	4 128 +
		dc.b	char R char A char H char C
CHAR.SF	dc.w	CHAR.Z char.CF del
CHAR.CF	#.b	32
		#.w	WORD.CF
		jsr			( addr)
		1+			( c-addr)
		fetch.b		( char)
CHAR.Z		rts
;
BL.LF		dc.l	CHAR.NF
BL.NF		dc.b	2 128 +
		dc.b	char L char B
BL.SF		dc.w	BL.Z BL.CF del
BL.CF		#.b	32
BL.Z		rts
;
2DROP.LF	dc.l	BL.NF
2DROP.NF	dc.b	5 128 +
		dc.b	char P char O char R char D char 2
2DROP.SF	dc.w	2
2DROP.CF	drop
		drop,rts
;
2DUP.LF	dc.l	2DROP.NF
2DUP.NF	dc.b	4 128 +
		dc.b	char P char U char D char 2
2DUP.SF	dc.w	2DUP.Z 2DUP.CF del
2DUP.CF	over	
2DUP.Z		over,rts
;
2OVER.LF	dc.l	2DUP.NF
2OVER.NF	dc.b	5 128 +
		dc.b	char R char E char V char O char 2
2OVER.SF	dc.w	2OVER.Z 2OVER.CF del
2OVER.CF	#.b	3
		#.w	PICK.CF
		jsr
		#.b	3
		#.w	PICK.CF
		jsr
2OVER.Z	rts
;
2SWAP.LF	dc.l	2OVER.NF
2SWAP.NF	dc.b	5 128 +
		dc.b	char P char A char W char S char 2
2SWAP.SF	dc.w	2SWAP.Z 2SWAP.CF del
2SWAP.CF	>R		( 1 2 3 R:4)
		rot		( 2 3 1 R:4)
		rot		( 3 1 2 R:4)
		R>		( 3 1 2 4)
		rot		( 3 2 4 1)
2SWAP.Z	rot,rts	( 3 4 1 2)
;
; PICK ( x2 x1 x0 n -- xn)
;
PICK.LF	dc.l	2SWAP.NF
PICK.NF	dc.b	4 128 +
		dc.b	char K char C char I char P
PICK.SF	dc.w	PICK.Z PICK.CF del
PICK.CF	psp@		( n depth)
		swap		( depth n)
		-
		#.b	4
		multu		( offsetLO offsetHI)
		drop		( offset)
		#.w	hex E004
		+		( addr)
		fetch.l	( n)
PICK.Z		rts
;	
; DUMP ( addr n --, display memory)
DUMP.LF	dc.l	PICK.NF
DUMP.NF	dc.b	4 128 +
		dc.b	char P char M char U char D
DUMP.SF	dc.w	DUMP.Z DUMP.CF del
DUMP.CF	over	( addr n addr)
		+	( addr end)
		>R	( addr R: end)
		BEGIN
			#.b	EOL
			#.w	EMIT.CF
			jsr		
			dup		( addr addr)
			#.b	6					; 6 digit output for the address
			#.w	UDOTR.CF
			jsr		( addr)
			#.b	32
			#.w	EMIT.CF
			jsr
			dup		( addr addr)			
			#.b	8	( addr 8)
			#.w	TYPERAW.CF
			jsr						; print the literals		
			#.b	32
			#.w	EMIT.CF
			jsr	
			#.b	8
			zero
			DO
				#.b 32
				#.w EMIT.CF
				jsr
				dup		( addr addr)
				fetch.b	( addr n)
				#.b	2				; 2 digit output for the byte
				#.w	UDOTR.CF
				jsr
				1+		( addr+)
			LOOP		( addr addr+8)
			dup		( addr+8 addr+8 R: end)
			R@		( addr+8 addr+8 end R: end)
			<		( addr+8 flag R: end)
			not		( addr+8 flag' R: end)
		UNTIL
		#.b	EOL
		#.w	EMIT.CF
		jsr
		R>			( addr end)
		drop		
DUMP.Z		drop,rts			
;	
; .S ( -- non destructive stack print)
DOTS.LF	dc.l	DUMP.NF
DOTS.NF	dc.b	2 128 +
		dc.b	char S char .
DOTS.SF	dc.w	DOTS.Z DOTS.CF del
DOTS.CF	psp@			( depth)
		#.b	4		( depth 4)
		multu
		drop			( offset)
		#.w	hex E008
		+			( addr)
		BEGIN
			dup		( addr addr)
			#.w hex E008	( addr addr E008)
			>
		WHILE
			#.b	EOL
			#.w	EMIT.CF
			jsr
			dup		( addr addr)
			fetch.l	( addr n)
			#.w	DOT.CF
			jsr	
			#.b	4	( addr 4)
			-		( addr-4)
		REPEAT
		#.b	EOL
		#.w	EMIT.CF
		jsr		
DOTS.Z		drop,rts
;
; UNUSED ( -- n, number of unused bytes in dataspace)
UNUSED.LF	dc.l	DOTS.NF
UNUSED.NF	dc.b	6 128 +
		dc.b	char D char E char S char U char N char U
UNUSED.SF	dc.w	UNUSED.Z UNUSED.CF del
UNUSED.CF	#.w	45056		; 12 * 2K BLOCK RAM
		#.w	HERE_
		fetch.l
UNUSED.Z	-,rts
;
; CHECKMEM - internal word
CHECKMEM.CF	#.w	UNUSED.CF
		jsr		( free)
		#.b	128	( free 128)
		<
		IF
			#.w	CHECKMEM.0
			#.b	8
			#.w	TYPE.CF
			jsr
			#.w	ABORT.CF
			jsr
		THEN
		rts
CHECKMEM.0	dc.b char M char E char M 32 char W char O char L
CHECKMEM.Z	dc.b EOL
;
; CREATE ( -- construct a dictionary entry for name)
CREATE.LF	dc.l	UNUSED.NF
CREATE.NF	dc.b	6 128 +
		dc.b	char E char T char A char E char R char C
CREATE.SF	dc.w	CREATE.Z CREATE.CF del
CREATE.CF	#.w	CHECKMEM.CF
		jsr		
		#.b	32
		#.w	WORD.CF			
		jsr			( addr)		; parse next word
		drop						; HEAD assumes that the name field is already complete		
		#.w	HEAD.CF
		jsr			( CF)			; create header
		dup			( CF CF)
		1-
		1-			( CF SF)	
		#.b	6		( CF SF 6)		; create runtime code is 6 bytes (ignoring >DOES)
		swap			( CF 6 SF)
		store.w		( CF)			; store 6 in size field
		#.b	op#.L		( CF op#.L)
		over			( CF op#.L CF)
		store.b		( CF)			; compile #.L	- do not use LITERAL since #.w or #.l would depend on address size
		1+			( CF')			; but in this case the distance to the PFA must be known exactly
		dup
		#.b	11		( CF CF 11)		; PFA will be 10 bytes from this point
		+			( CF PFA)
		over			( CF PFA CF)
		store.l		( CF)			; store PFA
		#.b	4		( CF 4)
		+			( CF')
		#.b	opRTS		( CF op)
		over			( CF op CF)
		store.b					; compile RTS
		#.b	7		( CF 7)		; extra bytes to leave space for >DOES redirection code if necessary + RTS 
		+			( CF')	
;
; common to both versions
		#.w	HERE_		( CF &CF)			
		store.l		( )			; update variable HERE		
CREATE.Z	rts		
;					
; DOES> ( -- , run time behaviour of a defining word)
DOES>.LF	dc.l	CREATE.NF
DOES>.NF	dc.b	5 128 + 
		dc.b	char > char S char E char O char D
DOES>.SF	dc.w	DOES>.Z DOES>.CF del
DOES>.CF	#.w	LAST-SF				; find SF in create word
		fetch.l		( SF)
		#.b	12
		over			( SF 12 SF)
		store.w					; update SF: create does> runtime code is 11 bytes
		#.b	7		( SF 7)		; starting position for does code is 7 bytes ahead of SF
		+			( CF)			; CF is position of RTS left by create
		#.b	op#.L		( CF op#.L)
		over			( CF op#.L CF)	
		store.b		( CF)			; compile #.L	
		1+			( CF')			; increment CF	
		R>			( CF dest)		; postone words following DOES>
		over			( CF dest CF)
		store.l		( CF)			; compile dest 
		#.b	4		( CF 4)
		+			( CF')
		#.b	opJSR		( CF opJSR)
		over			( CF opJSR CF)
		store.b					; compile JSR  (need to use a JSR/RTS pair rather than JMP incase inlined)	
		1+			( CF')
		#.b	opRTS		( CF opRTS)
		swap
		store.b					; compile RTS
DOES>.Z	rts
;
; HEAD ( -- CF, make a dictionary header assuming the name field has already been set by WORD) - internal word
HEAD.CF	#.w	HERE_
		fetch.l		( addr LF)
		dup
		>R			( addr LF R: LF)
		#.w	LAST-NF	
		fetch.l		( addr LF LAST-NF R:LF)		
		swap
		store.l		( addr R:LF)		; set Link Field
		R>			( addr LF)
		#.b	4
		+			( addr NF)	
		dup			( addr NF NF)
		#.w 	LAST-NF	( addr NF NF &LAST-NF)
		store.l		( addr NF)		; save Name Field in LAST-NF
; the prior version of HEAD copied the name string from the parse buffer
;		>R			( addr R:NF)
;		#.w	COUNT.CF
;		jsr			( c-addr u R:NF)
;		dup			( c-addr u u R:NF)
;		#.b	128		( c-addr u u 128 R:NF)
;		or			( c-addr u u' R:NF)	; set precedence bit in Name Field
;		R@			( c-addr u u NF R:NF)
;		store.b		( c-addr u R:NF)	; save string size in Name Field
;		R>			( c-addr u NF)
;		over			( c-addr u NF u)
;		over			( c-addr u NF u NF)
;		+		
;		1+			( c-addr u NF SF)
;		>R			( c-addr u NF R:SF)
;		1+			( c-addr u NF+1 R:SF)
;		swap			( c-addr NF+1 u R:SF)
;		#.w	MOVE.CF				; copy string in Name Field
;		jsr			( R:SF)
;		R>			( SF)
;		dup			( SF SF)
		dup			( NF NF)
		fetch.b		( NF len)
		dup		
		>R			( NF len R:len)
		#.b	128
		or			( NF len') 		; set precedence bit in name field
		over			( NF len' NF)
		store.b		( NF)
		R>			( NF len)
		+
		1+			( SF)			; add len+1 to move from NF to SF
		dup			( SF SF)
		#.w	LAST-SF	( SF LAST-SF)	
		store.l		( SF)			; save Size Field
		#.b	2
		+			( CF)
		dup			( CF CF)
		#.w	HERE_					; update HERE
		store.l		(CF)
		rts
;
; MOVE ( addr-s addr-d n, memory copy)
MOVE.LF	dc.l	DOES>.NF
MOVE.NF	dc.b	4 128 +
		dc.b 	char E char V char O char M
MOVE.SF	dc.w	MOVE.Z MOVE.CF del
MOVE.CF	?dup	
		IF	( s d n)
			>R	( s d R:n)
			over	( s d s R:n)
			over	( s d s d R:n)
			>	( s d flag R:n)
			IF	; copy up
				R>	( s d n)
				ZERO	( s d n 0)
				DO	( s d)
					over 		( s d s)
					fetch.b	( s d c)
					over		( s d c d)
					store.b	( s d)
					1+		( s d+1)
					swap		( d+1 s)
					1+		( d+1 s+1)
					swap		( s+1 d+1)
				LOOP
			ELSE	; copy down
				R@	( s d n R:n)
				+	( s d+n R:n)
				1-	( s d+n-1 R;n)
				swap	( d+n-1 s R:n)
				R@	( d+n-1 s n R:n)
				+	( d+n-1 s+n R:n)
				1-	( d+n-1 s+n-1 R:n)
				swap	( s+n-1 d+n-1 R:n)
				R>	( s d n)
				ZERO	( s d n 0)
				DO	( s d)
					over 		( s d s)
					fetch.b	( s d c)
					over		( s d c d)
					store.b	( s d)
					1-		( s d-1)
					swap		( d-1 s)
					1-		( d-1 s-1)
					swap		( s-1 d-1)
				LOOP				
			THEN	
		THEN
		drop
		drop
MOVE.Z		rts
;
; FILL ( addr n b --, fill a region of memory with n bytes)
FILL.LF	dc.l	MOVE.NF
FILL.NF	dc.b	4 128 +
		dc.b	char L char L char I char F
FILL.SF	dc.w	FILL.Z FILL.CF del
FILL.CF	rot	( n b addr)
		rot	( b addr n)
		?dup
		IF	( b addr n)
			ZERO 		( b addr n 0)
			DO  		( addr b)
				over		( b addr b )
				over		( b addr b addr)
				store.b	( b addr)
				1+		( b addr+1)
			LOOP
		THEN
		drop
FILL.Z		drop,rts			
;
;
; FILL.W ( addr n w --, fill a region of memory with n words)
FILL.W.LF	dc.l	FILL.NF
FILL.W.NF	dc.b	6 128 +
		dc.b	char W char . char L char L char I char F
FILL.W.SF	dc.w	FILL.W.Z FILL.W.CF del
FILL.W.CF	rot	( n w addr)
		rot	( w addr n)
		?dup
		IF	( w addr n)
			ZERO 		( w addr n 0)
			DO  		( addr b)
				over		( w addr w )
				over		( w addr w addr)
				store.w	( w addr)
				1+		( w addr+1)
				1+		( w addr+2)
			LOOP
		THEN
		drop
FILL.W.Z	drop,rts
;
; , (u -- , allocate 4 bytes and store long from the stack)
COMMA.LF	dc.l	FILL.W.NF
COMMA.NF	dc.b	1 128 +
		dc.b 	44
COMMA.SF	dc.w 	COMMA.Z COMMA.CF del
COMMA.CF	#.w	HERE_	( u &HERE)
		dup		( u &HERE &HERE)
		>R		( u &HERE R:&HERE)
		fetch.l	( u HERE R:&HERE)
		swap		( HERE u R:&HERE)
		over		( HERE u HERE R:&HERE)
		store.l	( HERE : R:&HERE)		; compile
		#.b 4		( HERE 4 R:&HERE)
		+		( HERE' R:&HERE)
		R>		( HERE &HERE)
		store.l					; update HERE
COMMA.Z	rts
;
; W, (w -- , allocate 2 bytes and store a word from the stack)
W,.LF		dc.l	COMMA.NF
W,.NF		dc.b	2 128 +
		dc.b 	44 char W
W,.SF		dc.w 	W,.Z W,.CF del
W,.CF		#.w	HERE_	( w &HERE)
		dup		( w &HERE &HERE)
		>R		( w &HERE R:&HERE)
		fetch.l	( w HERE R:&HERE)
		swap		( HERE w R:&HERE)
		over		( HERE w HERE R:&HERE)
		store.w	( HERE R:&HERE)		; compile
		1+		
		1+		( HERE' R:&HERE)
		R>		( HERE  &HERE)
		store.l					; update HERE
W,.Z		rts
;
; C, (b -- , allocate and store 1 byte from the stack)
C,.LF		dc.l	W,.NF
C,.NF		dc.b	2 128 +
		dc.b 	44 char C
C,.SF		dc.w 	C,.Z C,.CF del
C,.CF		#.w	HERE_	( b &HERE)
		dup		( u &HERE &HERE)
		>R		( u &HERE R:&HERE)
		fetch.l	( u HERE R:&HERE)
		swap		( HERE u R:&HERE)
		over		( HERE u HERE R:&HERE)
		store.b	( HERE R:&HERE)		; compile
		1+		( HERE' R:&HERE)
		R>		( HERE' &HERE)
		store.l					; update HERE
C,.Z		rts
;
; M, (addr u -- , allocate and store u bytes from addr.  u is not saved)
M,.LF		dc.l	C,.NF
M,.NF		dc.b	2 128 +
		dc.b 	44 char M
M,.SF		dc.w 	M,.Z M,.CF del
M,.CF		#.w	HERE_	( addr u &HERE)
		dup		( addr u &HERE &HERE)
		>R		( addr u &HERE R:&HERE)
		fetch.l	( addr u HERE R:&HERE)
		dup		( addr u HERE HERE R:&HERE)
		>R		( addr u HERE R:&HERE HERE)
		swap		( addr HERE u R:&HERE HERE)
		dup		( addr HERE u u R:&HERE HERE)
		>R		( addr HERE u R:&HERE HERE u)
		#.w	MOVE.CF
		jsr		( R:&HERE HERE u)
		R>		( u R:&HERE HERE)
		R>		( u HERE R:&HERE)
		+		( HERE' R:&HERE)
		R>		( HERE &HERE)
		store.l					; update HERE
M,.Z		rts
; $, ( addr u -- , compile a counted string)
$,.LF		dc.l	M,.NF
$,.NF		dc.b	2 128 +
		dc.b	44 char $
$,.SF		dc.w	$,.Z $,.CF del
$,.CF		dup			( addr n n)
		#.w	C,.CF
		jsr						; compile the count
		#.w	M,.CF
		jsr						; compile the characters of the string
$,.Z		rts
;
; ALLOT ( n -- allocate n bytes)
ALLOT.LF	dc.l	$,.NF
ALLOT.NF	dc.b	5 128 +
		dc.b	char T char O char L char L char A
ALLOT.SF	dc.w	ALLOT.Z ALLOT.CF del
ALLOT.CF	#.w	HERE_	( n HERE)
		#.w	+!.CF
		jsr
ALLOT.Z	rts
;
; VARIABLE ( -- create a variable)
VARIABLE.LF	dc.l	ALLOT.NF
VARIABLE.NF	dc.b	8 128 +
		dc.b	char E char L char B char A char I char R char A char V
VARIABLE.SF	dc.w	VARIABLE.Z VARIABLE.CF del
VARIABLE.CF	#.w	COLON.CF				; initiate the word
		jsr
		#.w	HERE_
		fetch.l
		#.b	6					; allow enough space for a 4 byte PFA address, load and RTS
		+
		dup				(PFA PFA)
		#.w	LITERAL.CF				; compile the PFA
		jsr
		#.w	SEMICOLON.CF
		jsr
		#.b	4			( PFA 4)	; allocate space for the the PFA
		+				( HERE')
		#.w  	HERE_			( HERE' &HERE)						
		store.l
VARIABLE.Z	rts
; COLON
COLON.LF	dc.l	VARIABLE.NF
COLON.NF	dc.b	1 128 +
		dc.b	char :
COLON.SF	dc.w	COLON.Z COLON.CF del
COLON.CF	#.w	CHECKMEM.CF
		jsr
		#.b	32
		#.w	WORD.CF			
		jsr			( addr)		; parse next word
		drop 						; HEAD assumes that the name field is already complete
		#.w	HEAD.CF
		jsr			( CF)			; create header
COLON.1	#.w	LAST-CF	( CF &LAST-CF)
		store.l					; update LAST-CF		
		#.w	LAST-NF	( &LAST-NF)		; SMUDGE the word
		fetch.l		( NF)			
		dup			( NF NF)
		fetch.b		( NF size)
		#.b	SMDGE
		or			( NF size')
		swap			( size' NF)
		store.b
		zero			( 0)			; set compilation state
		0=			( true)		
		#.w	STATE_		( true &STATE)	
		store.l
COLON.Z	rts			
;
; SEMICOLON
SEMICOLON.LF	dc.l	COLON.NF
SEMICOLON.NF	dc.b	1 128 + IMMED +
		dc.b	59
SEMICOLON.SF	dc.w	SEMICOLON.Z SEMICOLON.CF del	
SEMICOLON.CF	#.b	opRTS		( opRTS)		
		#.w 	C,.CF
		jsr						; compile RTS
		#.w	HERE_
		fetch.l		( HERE)
		#.w	LAST-CF
		fetch.l		( HERE LAST-CF)
		-			( size)
		#.w	LAST-SF
		fetch.l		( size LAST-CF)
		store.w					; update size field
		#.w	LAST-NF				; un-SMUDGE the word
		fetch.l		( NF)
		dup			( NF NF)
		fetch.b		( NF len)
		#.b	SMDGE invert
		and			( NF len')		
		swap			( len' NF)
		store.b
		zero			( false)		; un-set compilation state
		#.w	STATE_		( false &STATE)	
		store.l		
SEMICOLON.Z	rts		
;	
; COMPILE, ( xt --, compile an execution token)
COMPILE,.LF	dc.l	SEMICOLON.NF
COMPILE,.NF	dc.b	8 128 +
		dc.b	44 char E char L char I char P char M char O char C
COMPILE,.SF	dc.w	COMPILE,.Z COMPILE,.CF del
COMPILE,.CF	dup			( CF CF)
		1-
		1-			( CF SF)
		fetch.w		( CF len)
		#.w	MUSTINLINE 	
		over
		and			( CF len flag)
		IF							;  must inline - set len = zero
			drop
			zero
		THEN	
		#.w	INLINESIZE
		fetch.l 		(CF len INLINESIZE)
		U>			(CF flag)
		IF	; subroutine thread
			#.w	LITERAL.CF
			jsr						; compile execution token as literal
			#.b	opJSR		( opJSR)
			#.w	C,.CF
			jsr						; compile JSR
		ELSE	; compile inline
			#.w	HERE_		( CF &HERE)
			dup			( CF &HERE &HERE)
			>R			( CF &HERE R:&HERE)
			fetch.l		( CF HERE R:&HERE)
			dup			( CF HERE HERE R:&HERE)
			>R			( CF HERE R:&HERE HERE)
			over			( CF HERE CF R:&HERE HERE)
			1-
			1-			( CF HERE NF R:&HERE HERE)
			fetch.w		( CF HERE len R:&HERE HERE)
			#.w	hex 3fff
			and							; wipeout INLINE flags
			dup			( CF HERE len len R:&HERE HERE)
			>R			( CF HERE len R:&HERE HERE len)
			#.w MOVE.CF
			jsr			( R:&HERE HERE len)		; copy code field
			R>			( len R:&HERE HERE)
			R>			( len HERE R:&HERE)
			+		
			1-			( last R:&HERE)
			dup			( last last R:&HERE)
			fetch.b		( last op R:&HERE)
			#.b	63		( last op 63 R:&HERE)
			and			( last op' R:&HERE)		; remove RTS instruction
			?dup
			IF	; ,RTS
				over			( last op' last R:&HERE)
				store.b		( last R:&HERE)	; revise final opcode
				1+			( last' R:&HERE)
			THEN
			R>			( last' &HERE)
			store.l						; update HERE				
		THEN
COMPILE,.Z	rts
;
; ' ( NAME -- xt, find a word and return it's execution token or abort)
TICK.LF	dc.l	COMPILE,.NF
TICK.NF	dc.b	1 128 +
		dc.b	39
TICK.SF	dc.w	TICK.Z TICK.CF del NOINLINE +
TICK.CF	#.b	32 		
		#.w	WORD.CF		
		jsr			( addr)
		dup			( addr addr)
		fetch.b		( addr n)
		IF						; confirm some characters were parsed
			#.w	FIND.CF
			jsr			( addr 0 | XT true)
			IF
				rts		( XT)
			THEN
		THEN
		#.b	2					; ERROR#
		#.w	ERROR.CF
		jsr
TICK.Z		rts
;
; LITERAL ( n -- , compile a LITERAL)
LITERAL.LF	dc.l	TICK.NF
LITERAL.NF	dc.b	7 128 + IMMED +
		dc.b	char L char A char R char E char T char I char L
LITERAL.SF	dc.w	LITERAL.Z LITERAL.CF del
LITERAL.CF	#.w 	HERE_		( n &HERE)
		dup			( n &HERE &HERE)
		>R			( n &HERE R:&HERE)
		fetch.l		( n HERE)
		swap			( HERE n)
		dup			( HERE n n)
		#.b	255		( HERE n n 255)
		U>			( HERE n flag)
		IF	
			dup		( HERE n n)
			#.w	65535		( HERE n n 65535)
			U>			( HERE n flag)
			IF			; long
				over			( HERE n HERE)
				#.b	op#.L		( HERE n HERE op)		; opcode LOAD.L
				over			( HERE n HERE op HERE)
				store.b		( HERE n HERE)		; compile opcode
				1+
				store.l		( HERE)			; compile long
				#.b 	5		( HERE 5)
			ELSE			; word
				over			( HERE n HERE)
				#.b	op#.W		( HERE n HERE op)		; LOAD.W
				over			( HERE n HERE op HERE)
				store.b		( HERE n HERE)		; compile opcode
				1+
				store.w		( HERE)			; compile word
				#.b	3		( HERE 3)
			THEN
		ELSE	; byte
			over			( HERE n HERE)
			#.b	op#.B		( HERE n HERE op)		; LOAD.B
			over			( HERE n HERE op HERE)
			store.b		( HERE n HERE)		; compile opcode
			1+
			store.b		( HERE)			; compile byte
			#.b	2		( HERE 3)		
		THEN
		+		( HERE' R:&HERE)
		R>		( HERE' &HERE)
		store.l
LITERAL.Z	rts	
;			
; CONSTANT 
CONSTANT.LF	dc.l	LITERAL.NF
CONSTANT.NF	dc.b	8 128 +
		dc.b	char T char N char A char T char S char N char O char C
CONSTANT.SF	dc.w	CONSTANT.Z CONSTANT.CF del
CONSTANT.CF	#.w	COLON.CF				; initiate the word
		jsr
		#.w	LITERAL.CF				; compile the constant
		jsr
		#.w	SEMICOLON.CF				; finish the word
		jsr
;	traditional, CREATE, version of CONSTANT is inefficient
;	because at run-time the PFA is placed on stack and then the literal is called from there
;		#.w	CREATE.CF
;		jsr
;		#.w	COMMA.CF
;		jsr
;		#.w	DOES>.CF
;		jsr
;		fetch.l
CONSTANT.Z	rts
;
; IMMEDIATE , mark the most recently defined word as IMMEDIATE
IMMEDIATE.LF	dc.l	CONSTANT.NF
IMMEDIATE.NF	dc.b	9 128 +
		dc.b 	char E char T char A char I char D char E char M char M char I
IMMEDIATE.SF	dc.w	IMMEDIATE.Z IMMEDIATE.CF del
IMMEDIATE.CF	#.w	LAST-NF
		fetch.l	( NF)	
		dup		( NF NF)
		fetch.b	( NF nf)
		#.B	IMMED
		or		( NF nf')
		swap		( nf NF)
		store.b	
IMMEDIATE.Z	rts
;
; ['] , compile a reference to the next word so that at run time its XT will be placed on the stack
['].LF		dc.l	IMMEDIATE.NF	
['].NF		dc.b	3 128 + 
		dc.b	93 39 91
['].SF		dc.w	['].Z ['].CF del
['].CF		#.w	TICK.CF
		jsr
		#.w	LITERAL.CF
		jsr
['].Z		rts
;
; POSTPONE , force compilation of an immediate word
POSTPONE.LF	dc.l	['].NF
POSTPONE.NF	dc.b	8 128 + IMMED +
		dc.b	char E char N char O char P char T char S char O char P
POSTPONE.SF	dc.w	POSTPONE.Z POSTPONE.CF del
POSTPONE.CF	#.w	TICK.CF
		jsr
		#.w	COMPILE,.CF
		jsr
POSTPONE.Z	rts
;
; [CHAR] , IMMEDIATE CHAR
[CHAR].LF	dc.l	POSTPONE.NF
[CHAR].NF	dc.b	6 128 + IMMED +
		dc.b 	93 char R char A char H char C 91
[CHAR].SF	dc.w	[CHAR].Z [CHAR].CF del
[CHAR].CF	#.w	CHAR.CF
		jsr
[CHAR].Z	rts
;
; or.w! ( word addr -- ) or word with memory - internal word
or.w!.CF	swap		( addr word)
		over		( addr word addr)
		fetch.w	( addr word before)
		or		( addr after)
		swap		( after before)
		store.w	( )
		rts
;
; fwd-offset ( org  -- offset) calculate forward offset from HERE - internal word
fwd-offset.CF	#.w	HERE_	( org &HERE)
		fetch.l	( org dest)
		swap		( dest org)
		-		( diff)
		1-		( offset)
		#.w	16383	( offset mask)
		and,rts	( offset')
;
; rev-offset ( org  -- offset) calculate reverse offset from HERE - internal word
rev-offset.CF	#.w	HERE_	( dest &HERE)
		fetch.l	( dest org)
		-		( diff)
		1-		( offset)
		#.w	16383	( offset mask)
		and,rts	( offset')
;
; IF
IF.LF		dc.l	[CHAR].NF
IF.NF		dc.b	2 128 + IMMED +
		dc.b	char F char I
IF.SF		dc.w	IF.Z IF.CF del
IF.CF		#.w	HERE_
		fetch.l	
		#.w	opBEQ			; compile BEQ for forward branch
		#.w	W,.CF
		jsr				
IF.Z		rts		( HERE)
;
; THEN
THEN.LF	dc.l	IF.NF
THEN.NF	dc.b	4 128 + IMMED +
		dc.b 	char N char E char H char T
THEN.SF	dc.w	THEN.Z THEN.CF del
THEN.CF	dup		( org org)
		#.w	fwd-offset.CF		; calculate branch offset
		jsr		( org offset)	
		swap		( offset org)
		#.w	or.w!.CF		; compile forward branch offset at origin
		jsr				
THEN.Z		rts
;	
ELSE.LF	dc.l	THEN.NF
ELSE.NF	dc.b	4 128 + IMMED +
		dc.b 	char E char S char L char E
ELSE.SF	dc.w	ELSE.Z ELSE.CF del
ELSE.CF	#.w	HERE_	( org &HERE)	
		fetch.l	( org HERE)
		#.w	opBRA			; compile BRA for forward branch to THEN
		#.w	W,.CF
		jsr		( org HERE)
		swap		( HERE org)
		dup		( HERE org org)
		#.w	fwd-offset.CF		; calculate forward branch offset from IF		
		jsr		( HERE org offset)	
		swap		( HERE offset org)
		#.w	or.w!.CF		; compile forward branch offset at IF
		jsr			
ELSE.Z		rts		( HERE)
;
BEGIN.LF	dc.l	ELSE.NF
BEGIN.NF	dc.b	5 128 + IMMED +
		dc.b	char N char I char G char E char B
BEGIN.SF	dc.w	BEGIN.Z BEGIN.CF del
BEGIN.CF	#.w	HERE_
		fetch.l			; save destination for backward branch
BEGIN.Z	rts
;
AGAIN.LF	dc.l	BEGIN.NF
AGAIN.NF	dc.b	5 128 + IMMED +
		dc.b	char N char I char A char G char A
AGAIN.SF	dc.w	AGAIN.Z AGAIN.CF del
AGAIN.CF	#.w	rev-offset.CF
		jsr		( offset)	; calculate backward branch offset
		#.w	opBRA
		or		( op)
		#.w	W,.CF
		jsr				; compile unconditional backward branch 
AGAIN.Z	rts
;
UNTIL.LF	dc.l	AGAIN.NF
UNTIL.NF	dc.b	5 128 + IMMED +
		dc.b	char L char I char T char N char U
UNTIL.SF	dc.w	UNTIL.Z UNTIL.CF del
UNTIL.CF	#.w	rev-offset.CF
		jsr		( offset)	; calculate backward branch offset
		#.w	opBEQ
		or		( op)
		#.w	W,.CF
		jsr				; compile conditional backward branch
UNTIL.Z	rts
;
WHILE.LF	dc.l	UNTIL.NF
WHILE.NF	dc.b	5 128 + IMMED +
		dc.b	char E char L char I char H char W
WHILE.SF	dc.w	WHILE.Z WHILE.CF del
WHILE.CF	#.w	IF.CF
		jsr		( offset)	; WHILE is equivalent to IF
WHILE.Z	rts
;
REPEAT.LF	dc.l	WHILE.NF
REPEAT.NF	dc.b	6 128 + IMMED +
		dc.b	char T char A char E char P char E char R
REPEAT.SF	dc.w	REPEAT.Z REPEAT.CF del
REPEAT.CF	swap	( org dest)		; org at WHILE, dest at BEGIN
		#.w	AGAIN.CF
		jsr				; compile reverse branch to BEGIN 
		#.w	THEN.CF
		jsr				; compile forward branch from WHILE
REPEAT.Z		rts
;
DO.LF		dc.l	REPEAT.NF
DO.NF		dc.b	2 128 + IMMED +
		dc.b 	char O char D
DO.SF		dc.w	DO.Z DO.CF del 1 +
DO.CF		#.w	DO0.RUN		; code to set flag for an uncoditional loop
DO.Z		bra	{DO}.CF DO.Z rel		
;
?DO.LF		dc.l	DO.NF
?DO.NF		dc.b	3 128 + IMMED +
		dc.b 	char O char D char ?
?DO.SF		dc.w	?DO.Z ?DO.CF del 1 +
?DO.CF		#.w	DO2.RUN			; code to set flag for a conditional loop
?DO.Z		bra	{DO}.CF ?DO.Z rel	
;
; {DO} common to DO and ?DO  - internal word
{DO}.CF	#.w	COMPILE,.CF			; code to set flag for IF
		jsr
		#.w	DO1.RUN			; code to initialize return stack
		#.w	COMPILE,.CF	
		jsr				
		#.w	pushHERE.CF			; place origin of forward branch on compile stack
		jsr
		#.w	opBEQ				; compile BEQ for forward branch
		#.w	W,.CF
		jsr							
		#.w	HERE_				; save destination for backward branch from LOOP/+LOOP
		fetch.l
{DO}.Z		rts
;
DO0.SF		dc.w	DO0.Z DO0.RUN del
DO0.RUN	zero		( limit index 0)
		0=		( limit index -1)
		rot		( index -1 limit)
DO0.Z		rot,rts	( -1 limit index)
;
DO1.SF		dc.w	DO1.Z DO1.RUN del		; must be compiled inline
DO1.RUN	swap		( flag index limit)
		>R		( flag index R:limit)
		>R		( flag R:limit index)
DO1.Z		rts
;
DO2.SF		dc.w	DO2.Z DO2.RUN del
DO2.RUN	over		( limit index limit)
		over		( limit index limit index)
		<>		( limit index flag)
		rot		( indes flag limit)
DO2.Z		rot,rts	( flag limit index)
;
LOOP.LF	dc.l	?DO.NF
LOOP.NF	dc.b	4 128 + IMMED +
		dc.b 	char P char O char O char L
LOOP.SF	dc.w	LOOP.Z LOOP.CF del
LOOP.CF	#.w 	LOOP1.RUN			; code to increment and test loop paramaters
LOOP.Z		bra	{LOOP}.CF LOOP.Z rel
;
+LOOP.LF	dc.l	LOOP.NF			; code to add n and test loop paramaters
+LOOP.NF	dc.b	5 128 + IMMED +
		dc.b	char P char O char O char L char +
+LOOP.SF	dc.w	+LOOP.Z +LOOP.CF del
+LOOP.CF	#.w 	LOOP2.RUN
+LOOP.Z	bra	{LOOP}.CF +LOOP.Z rel
;
;{LOOP} common to LOOP and +LOOP, internal word
{LOOP}.CF	#.w	COMPILE,.CF				; code to add/increment and test loop paramaters
		jsr
		#.w	UNTIL.CF				; conditional backwards branch to DO
		jsr
		#.w	COMPILESTACKP				; compile forward branch to LEAVE's (including implicit conditional LEAVE before DO)
		dup			( &P &P)
		fetch.l		( &P limit)
		#.l	_PAD		( &P limit index)
		rot			( limit index &P)
		over			( limit index &P index)
		swap			( limit index index &P)
		store.l		( limit index)	; reset pointer to start of the compile stack
		DO
			R@		( pstack)
			fetch.l	( dest)
			#.w	THEN.CF		
			jsr
			#.b	4	( 4)
		+LOOP
		#.w	UNLOOP.CF				; code to remove loop parameters
		#.w	COMPILE,.CF
		jsr
{LOOP}.Z	rts
;
; LOOP1 ( R: limit index), increment and test loop paramaters	
LOOP1.SF	dc.w	LOOP1.Z LOOP1.RUN del	; must be compiled inline
LOOP1.RUN	R>		( index R: limit)
		1+		( index+ R: limit)
		dup		( index+ index+ R: limit)
		R@		( index+ index+ limit R: limit)
		<		( index+ flag R: limit)
		not  		( index+ flag' R: limit)
		swap 		( flag index+  R: limit)
		>R   		( flag R: limit index+)
LOOP1.Z	rts
;
; LOOP2 ( n R: limit index), add n and test loop paramaters
LOOP2.SF	dc.w	LOOP2.Z LOOP2.RUN del MUSTINLINE +	; must be compiled inline
LOOP2.RUN	dup
		0<
		swap		( flag1 n R: limit index)		; sign of index variable
		R>		( flag1 n index R: limit)
		+		( flag1 index+ R: limit)
		dup		( flag1 index+ index+ R: limit)
		R@		( flag1 index+ index+ limit R: limit)
		<		( flag1 index+ flag R: limit)
		not  		( flag1 index+ flag2 R: limit)
		swap 		( flag1 flag2 index+  R: limit)
		>R   		( flag1 flag2 R: limit index+)
		xor		( flag R: limit index+)		; if index variable was negative, invert flag
LOOP2.Z	rts
;
; UNLOOP, remove loop paramaters
UNLOOP.LF	dc.l	+LOOP.NF
UNLOOP.NF	dc.b	6 128 +
		dc.b	char P char O char O char L char N char U
UNLOOP.SF	dc.w	UNLOOP.Z UNLOOP.CF del MUSTINLINE + ; must be compiled inline
UNLOOP.CF	R>						 ; assume no return address
		R>
		drop
UNLOOP.Z	drop,rts
;
; LEAVE, exit current loop
LEAVE.LF	dc.l	UNLOOP.NF
LEAVE.NF	dc.b	5 128 + IMMED +
		dc.b	char E char V char A char E char L
LEAVE.SF	dc.w	LEAVE.Z LEAVE.CF del
LEAVE.CF	#.w	pushHERE.CF
		jsr
		#.w	opBRA					; compile BRA for forward branch
		#.w	W,.CF
		jsr			
LEAVE.Z	rts
;
;pushHERE, place HERE on COMPILEstack  - internal word
pushHERE.CF	#.w	COMPILEstackP	( &P)			; save HERE to COMPILE stack
		dup	( &P &P)
		>R	( &P R:&P)
		fetch.l	( P R:&P)
		#.w	HERE_	( P &HERE R:&P)	
		fetch.l	( P HERE R:&P)		
		over		( P HERE P R:&P)
		store.l	( P R:&P)
		#.b	4
		+		( P' R:&P)
		R>		( P' &P)			; increment COMPILEstack pointer
		store.l	
		rts
;		
; CASE, mark the start of a CASE...OF..ENDOF...ENDCASE structure
CASE.LF	dc.l	LEAVE.NF
CASE.NF	dc.b	4 128 + IMMED +
		dc.b	char E char S char A char C
CASE.SF	dc.w	1
		zero						; set casecount to zero					
CASE.Z		rts						
;
OF.LF		dc.l	CASE.NF
OF.NF		dc.b	2 128 + IMMED +
		dc.b	char F char O
OF.SF		dc.w	OF.Z OF.CF del
OF.CF		>R						; push casecount to the return stack
		#.w	opOVER=
		#.w	W,.CF					; compile OVER = for making the test
		jsr
		#.w	IF.CF					; push HERE and prepare BEQ for forward branch = IF 
		jsr
		#.b	opDROP
		#.w	C,.CF					; compile DROP (if test is sucessful and code continues)
		jsr
		R>						; get casecount from return stack
		1+						; increment CASECOUNT
OF.Z		rts
;
ENDOF.LF	dc.l	OF.NF
ENDOF.NF	dc.b	5 128 + IMMED +
		dc.b	char F char O char D char N char E
ENDOF.SF	dc.w	ENDOF.Z ENDOF.CF del
ENDOF.CF	>R						; push casecount to the return stack
		#.w	ELSE.CF				
		jsr						; push here and prepare BRA for forward branch, then resolve prior forward branch from OF = ELSE
		R>						; get casecount from the return stack
ENDOF.Z	rts
;
ENDCASE.LF	dc.l	ENDOF.NF
ENDCASE.NF	dc.b	7 128 + IMMED +
		dc.b	char E char S char A char C char D char N char E
ENDCASE.SF	dc.w	ENDCASE.Z ENDCASE.CF del
ENDCASE.CF	#.b	opDROP
		#.w	C,.CF
		jsr						; compile DROP  (remove test variable in default case)
		zero				( casecount zero)
		DO
			#.w	THEN.CF
			jsr					; resolve forward branch from each ENDOF = THEN
		LOOP
ENDCASE.Z	rts
;
; [ , exit compilation state
[.LF		dc.l	ENDCASE.NF
[.NF		dc.b	1 128 + IMMED +
		dc.b	91
[.SF		dc.w	[.Z [.CF del
[.CF		zero
		#.w	STATE_
		store.l
[.Z		rts
;
; ] , enter compilation state
].LF		dc.l	[.NF
].NF		dc.b	1 128 + IMMED +
		dc.b	93
].SF		dc.w	].Z ].CF del
].CF		zero			( 0)			
		0=			( true)		
		#.w	STATE_		( true &STATE)	
		store.l
].Z		rts
;
; RECURSE, compile the XT of the current word
RECURSE.LF	dc.l	].NF
RECURSE.NF	dc.b	7 128 + IMMED +
		dc.b	char E char S char R char U char C char E char R
RECURSE.SF	dc.w	RECURSE.Z RECURSE.CF del
RECURSE.CF	#.w	LAST-CF
		fetch.l		( xt)
		#.w	LITERAL.CF
		jsr						; compile execution token as literal
		#.b	opJSR		( opJSR)
		#.w	C,.CF
		jsr	
RECURSE.Z	rts
;
; EXIT
EXIT.LF	dc.l	RECURSE.NF
EXIT.NF	dc.b	4 128 +
		dc.b	char T char I char X char E
EXIT.SF	dc.w	EXIT.Z EXIT.CF del
EXIT.CF	R>						; obtain return address
EXIT.Z		jmp						; simple 'RTS' would be discarded by inline compiler
;
; RTI, return from interrupt
RTI.LF		dc.l	EXIT.NF
RTI.NF		dc.b	3 128 +
		dc.b	char I char T char R
RTI.CF		dc.w	1
		RTI
;
; EXECUTE ( xt --)
EXECUTE.LF	dc.l	RTI.NF
EXECUTE.NF	dc.b	7 128 +
		dc.b	char E char T char U char C char E char X char E
EXECUTE.SF	dc.w	2
EXECUTE.CF	jsr
EXECUTE.Z	rts						
;
; CELL+
CELL+.LF	dc.l	EXECUTE.NF
CELL+.NF	dc.b	5 128 +
		dc.b 	char + char L char L char E char C
CELL+.SF	dc.w	CELL+.Z CELL+.CF del
CELL+.CF	#.b	4
CELL+.Z	+,rts
;
; CELLS
CELLS.LF	dc.l	CELL+.NF
CELLS.NF	dc.b	5 128 +
		dc.b 	char S char L char L char E char C
CELLS.SF	dc.w	CELLS.Z CELLS.CF del
CELLS.CF	#.b	4
		mults
CELLS.Z	drop,rts
;	
; SLITERAL ( addr u , compile a string literal as an executable that will be re-presented at run time as a string addr u)
SLITERAL.LF	dc.l	CELLS.NF
SLITERAL.NF	dc.b	8 128 + IMMED +	
		dc.b 	char L char A char R char E char T char I char L char S
SLITERAL.SF	dc.w	SLITERAL.Z SLITERAL.CF del
SLITERAL.CF	dup		( addr u u)
		1+		( addr u offset)
		#.w	opBRA
		or		( addr u op)
		#.w	W,.CF	
		jsr		( addr u)	
		dup		( addr u u)
		>R		( addr u R:u)
		#.w	HERE_ 	( addr u &HERE R:u)
		fetch.l	( addr u HERE R:u)
		>R		( addr u R:u HERE)
		#.w	M,.CF					; compile the characters of the string
		jsr		( R:u HERE)
		R>		( HERE R:u)
		#.w	LITERAL.CF
		jsr		( R:u)
		R>		( u)
		#.w	LITERAL.CF
		jsr
SLITERAL.Z	rts
;
; CLITERAL ( addr u, compile a string literal as an executable that will be re-presented at run time as a counted string c-addr)
CLITERAL.LF	dc.l	SLITERAL.NF
CLITERAL.NF	dc.b	8 128 + IMMED +	
		dc.b 	char L char A char R char E char T char I char L char C
CLITERAL.SF	dc.w	CLITERAL.Z CLITERAL.CF del
CLITERAL.CF	dup		( addr u u)
		1+
		1+		( addr u offset)
		#.w	opBRA					
		or		( addr u op)
		#.w	W,.CF					; compile a branch over the body of the string
		jsr		( addr u)	
		#.w	HERE_ 	( addr u &HERE R:u)		; save the address of the string
		fetch.l	( addr u HERE R:u)
		>R		( addr u R:u HERE)
		#.w	$,.CF					; compile the count and characters of the string
		jsr		( R:u HERE)
		R>		( HERE R:u)
		#.w	LITERAL.CF				; compile the address	
		jsr
CLITERAL.Z	rts
;
; ," <string> ( --) compile the following string into the data space as a counted string)
,".LF		dc.l	CLITERAL.NF
,".NF		dc.b	2 128 +
		dc.b	34 44
,".SF		dc.w	,".Z ,".CF del
,".CF		#.w	IN$.CF
		jsr				( addr n)	; string held in parse buffer
		#.w	$,.CF					; compile the string
		jsr						
,".Z		rts
;
; STRINGlOC	( n -- addr, return a pointer to the next space in the string bufffer -- internal function)
STRINGLOC.CF	#.w	STRINGP
		dup
		>R
		fetch.l
		#.l	_STRING
		-
		over
		+
		#.w	256
		>
		IF
			#.l	_STRING
			R@
			store.l
		THEN
		R@
		fetch.l
		dup
		rot
		+
		R>
		store.l
STRINGLOC.Z	rts	
;	
; S" <string> ( - addr u), mode dependant string function
S".LF		dc.l	,".NF
S".NF		dc.b	2 128 + IMMED +
		dc.b	34 char S
S".SF		dc.w	S".Z S".CF del
S".CF		#.w	IN$.CF
		jsr				( addr n)	; string held in parse buffer	
		#.w	STATE_
		fetch.l
		IF						; compile mode
			#.w	SLITERAL.CF				; compile into dictionary
			jsr					
		ELSE						; interpret mode
			dup						; copy to the pad and return to user
			>R			( addr n R:n)
			dup
			#.w	STRINGLOC.CF
			jsr			( addr n str R:n)	
			dup		
			>R			( addr n str R:n str)
			swap			( addr str n R:n str)
			#.w	MOVE.CF
			jsr			( R:n str)
			R>
			R>			( str n)
		THEN
S".Z		rts
;
; C" <string>	( -- c-addr), mode dependant counted string function
C".LF		dc.l	S".NF
C".NF		dc.b	2 128 + IMMED +
		dc.b 	34 char C
C".SF		dc.w	C".Z C".CF del
C".CF		#.w	IN$.CF
		jsr				( addr n)	; string held in parse buffer
		#.w	STATE_
		fetch.l
		IF						; compile mode
			#.w	CLITERAL.CF				
			jsr						; compile into dictionary
		ELSE						; interpret mode	
			dup
			#.w	STRINGLOC.CF
			jsr						
			dup			( addr n pad pad)
			>R			( addr n pad R:pad)
			over			( addr n pad n R:pad)
			over			( addr n pad n pad R:pad)
			store.b		( addr n pad R:pad)	; copy the length byte to the PAD
			1+			( addr n pad+1 R:pad)
			swap			( addr pad+1 n R:pad)
			#.w	MOVE.CF				; copy the string to the PAD's second byte
			jsr			( R:pad)
			R>			( pad)		
		THEN
C".Z		rts
;
; IN$ <string> ( -- addr n), read a string - internal word
IN$.CF		#.b 	34					; char "
		#.w	WORD.CF
		jsr				( addr)
		#.w	COUNT.CF
		jsr				( addr n)	
IN$.Z		rts
;
; ." <string>, print a string
.".LF		dc.l	C".NF
.".NF		dc.b	2 128 + IMMED +
		dc.b	34 46
.".SF		dc.w	.".Z .".CF del
.".CF		#.w	S".CF					; compile
		jsr
		#.w	TYPE.CF
		#.w	COMPILE,.CF
		jsr
.".Z		rts
;
; ( comment
BRA.LF		dc.l	.".NF
BRA.NF		dc.b	1 128 + IMMED +
		dc.b	40					; char (
BRA.SF		dc.w	BRA.Z BRA.CF del
BRA.CF		#.b	41					; char )
		#.w	PARSE.CF
		jsr		( addr)
		drop
BRA.Z		drop,rts	
;
; \ comment
\.LF		dc.l	BRA.NF
\.NF		dc.b	1 128 + IMMED +
		dc.b	92					; char \
\.SF		dc.w	\.Z \.CF del
;\.CF		#.w	IN_LEN
;		fetch.l
;		#.w	>IN_
;		store.l
\.CF		#.b	EOL					; end of line
		#.w	WORD.CF
		jsr
\.Z		drop,rts	
;
; .( printing comment
DOTBRA.LF	dc.l	\.NF
DOTBRA.NF	dc.b	2 128 +
		dc.b	40 46
DOTBRA.SF	dc.w	DOTBRA.Z DOTBRA.CF del
DOTBRA.CF	#.b	41					; char )
		#.w	WORD.CF
		jsr
		#.w	COUNT.CF
		jsr
		#.w	TYPE.CF
		jsr
DOTBRA.Z		rts
;
; ALIGN ( --, align data-space space pointer - NOP on the NIGE Machine)
ALIGN.LF	dc.l	DOTBRA.NF
ALIGN.NF	dc.b	5 128 +
		dc.b	char N char G char I char L char A
ALIGN.SF	dc.w	1
		rts
; ALIGNED  ( addr -- a-addr, align an address)
ALIGNED.LF	dc.l	ALIGN.NF
ALIGNED.NF	dc.b	7 128 +
		dc.b	char D char E char N char G char I char L char A
ALIGNED.SF	dc.w	ALIGNED.Z ALIGNED.CF -
ALIGNED.CF	1+
		lsr
ALIGNED.Z	lsl,rts
;
; MARKER (--, create a deletion boundary)
MARKER.LF	dc.l	ALIGNED.NF
MARKER.NF	dc.b	6 128 +
		dc.b	char R char E char K char R char A char M
MARKER.SF	dc.w	MARKER.Z MARKER.CF del
MARKER.CF	#.w	HERE_						; current HERE will be the LF of the MARKER word
		fetch.l
		#.w	HERE1						; current SDRAM POINTER
		fetch.l		
		#.w	COLON.CF					; create the word (*after* placing HERE on the stack)
		jsr	
		#.w	LITERAL.CF					; compile the SDRAM pointer as a literal to be pushed onto the stack at runtime
		jsr				
		#.w	LITERAL.CF					; compile the LF as a literal to be pushed onto the stack at runtime
		jsr		
		#.w	MARKER.RUN					
		#.w	LITERAL.CF					; compile address of the MARKER runtime code
		jsr
		#.b	opJMP						
		#.w	C,.CF						; compile a jump to the MARKET runtime code
		jsr
		#.w	SEMICOLON.CF					; finish the word
		jsr
MARKER.Z	rts
; runtime code for MARKER
MARKER.RUN	dup				( SD LF LF)		; SD is the SDRAM pointer, LF is the LF of the word
		fetch.l			( SD LF NF')		; NF' is the NF of the prior word
		#.w	LAST-NF
		store.l			( SD LF)		; store the NF of the previous word in the last word variable (used by HEAD for linking)
		#.w	HERE_						; point HERE at the address of the work
		store.l			( SD)			
		#.w	HERE1						; reset the SDRAM pointer too
		store.l
		rts
;	
; BUFFER: ( n --), create a storate table in PSDRAM (overwritten by dynamic memory allocation words)
BUFFER:.LF	dc.l	MARKER.NF
BUFFER:.NF	dc.b	7 128 +
		dc.b	58 char R char E char F char F char U char B
BUFFER:.SF	dc.w	BUFFER:.Z BUFFER:.CF del
BUFFER:.CF	1+
		lsr
		lsl							; round up next even number
		#.w	COLON.CF
		jsr
		#.w	HERE1
		fetch.l
		dup
		#.w	LITERAL.CF					; compile the location of the buffer as a literal
		jsr
		#.w	SEMICOLON.CF
		jsr
		+	
		#.w	HERE1
		store.l						; update the position of the SDRAM data pointer
BUFFER:.Z	rts
;
; SBUFFER: ( n --), create a storate table in SDRAM
SBUFFER:.LF	dc.l	BUFFER:.NF
SBUFFER:.NF	dc.b	8 128 +
		dc.b	58 char R char E char F char F char U char B char S
SBUFFER:.SF	dc.w	SBUFFER:.Z SBUFFER:.CF del
SBUFFER:.CF	#.w	COLON.CF
		jsr
		zero
		1-							; dummy value to ensure #.l is used
		#.w	LITERAL.CF					; compile the location of the buffer as a literal
		jsr	
		#.w	SEMICOLON.CF
		jsr	
		#.w	HERE_						; actual address
		fetch.l		
		dup			( n addr addr)
		#.b	5
		-			( n addr loc)
		store.l
		#.w	ALLOT.CF					; update the position of the data pointer
		jsr
SBUFFER:.Z	rts
;
DEFER.LF	dc.l	SBUFFER:.NF
DEFER.NF	dc.b	5 128 +
		dc.b	char R char E char F char E char D
DEFER.SF	dc.w	DEFER.Z DEFER.CF del
DEFER.CF	#.w	COLON.CF					; prepare the word
		jsr
		#.w	DEFER.RUN	( XT)				; push default XT for DEFER	on the stack			
		#.w	HERE_
		fetch.l		( XT CF)			; push the CF address of this word on the stack		
		#.b	op#.l					
		#.w	C,.CF						; compile #.l
		jsr
		#.b	4						
		#.w	ALLOT.CF					; reserve space for the vector address
		jsr		
		#.w	{IS}.CF					; implement the vector
		jsr			
		#.b	opJSR
		#.w	C,.CF						; compile JMP
		jsr	
		#.w	SEMICOLON.CF					; terminate the word
		jsr
DEFER.Z	rts
;
; DEFER.RUN is the default behaviour for an uninitialized vector
DEFER.RUN	#.b	3
		#.w	ERROR.CF
		jmp
;
; IS <name> ( XT --), implement a vector in a defer word
IS.LF		dc.l	DEFER.NF
IS.NF		dc.b	2 128 +
		dc.b	char S char I
IS.SF		dc.w	IS.Z IS.CF del
IS.CF		#.w	TICK.CF
		jsr
		#.w	{IS}.CF
		jsr
IS.Z		rts
;
; {IS} ( XT CF --), implement the vector in a DEFER word, internal word
{IS}.CF	1+	( XT PF --)					; update the CF to the PF (after the #.l)
		store.l
		rts
;
; ? ( addr --), output the contents of a memory address
?.LF		dc.l	IS.NF
?.NF		dc.b	1 128 +
		dc.b	char ?
?.SF		dc.w	?.Z ?.CF del
?.CF		fetch.l
		#.w	UDOT.CF
		jsr
?.Z		rts
;
;PS2DECODE ( raw -- ascii)
PS2FLAGS	dc.b	0						; modifier flags
SHIFT_FLAG 	EQU 	1
CAPS_FLAG	EQU 	2
ALT_FLAG	EQU	4
CTRL_FLAG	EQU	8
CTRL_STATE	EQU	16
SPECIAL_FLAG	EQU	32
UP_FLAG	EQU	64
;
PS2DECODE.LF	dc.l	?.NF
PS2DECODE.NF	dc.b	9 128 +
		dc.b	char E char D char O char C char E char D char 2 char S char P
PS2DECODE.SF	dc.w	PS2DECODE.Z PS2DECODE.CF del
PS2DECODE.CF	#.b	hex 12	( raw 12)			; LEFT SHIFT
		over		( raw 12 raw)
		=		( raw flag)
		over		( raw flag raw)
		#.b	hex 59	( raw flag raw 59)		; RIGHT SHIFT
		=		( raw flag flag)
		or		( raw flag)
		IF
			drop
			zero
			#.b	SHIFT_FLAG	( zero SHIFT)
		ELSE
			
			#.b	hex 11		( raw 11)		; ALT
			over			( raw 11 raw)
			=			( raw flag)
			IF
				drop
				zero
				#.b	ALT_FLAG	( zero ALT)
			ELSE
				#.b	hex 14		( raw 14)		; CTRL
				over			( raw 14 raw)
				=			( raw flag)
				IF
					drop
					zero
					#.b	CTRL_FLAG	( zero CTRL)
				ELSE
					#.b	hex 58		( raw 58)		; CAPS LOCK
					over			( raw 58 raw)
					=			( raw flag)
						IF
							drop
							zero
							#.b	CAPS_FLAG	( zero CAPS)
						ELSE
							#.b	hex E0		( raw E0)		; SPECIAL
							over			( raw E0 raw)
							=			( raw flag)						
						IF
							drop
							zero
							#.b	SPECIAL_FLAG	( SPECIAL)
						ELSE						
							#.b	hex F0		( raw F0)		; UP
							over			( raw F0 over)
							=			( raw flag)
							IF						
								drop
								zero
								#.b	UP_FLAG	( zero UP)							
							ELSE				( raw)		; process keystroke	
								#.w	PS2FLAGS
								fetch.b		( raw FLAGS)
								dup			( raw FLAGS FLAGS)
								#.b	UP_FLAG
								and			( raw FLAGS UPtest)
								IF						; ignore if an upstroke
									drop
									drop
									zero
								ELSE			( raw FLAGS)		; lookup table reference
									swap		( FLAGS raw)
									over		( FLAGS raw FLAGS)
									#.b	SHIFT_FLAG CAPS_FLAG or
									and		( FLAGS raw ifShiftCaps)
									dup
									#.b	1
									=
									swap
									#.b	2
									=
									or
									IF						
										#.w	PS2ASCII 118 +
									ELSE										
										over
										#.b	SPECIAL_FLAG
										and	( flags raw ifSpecial)
										IF
											dup
											#.b 74
											=
											IF
												#.w	PS2ASCII 138 +
											ELSE
												dup
												#.b	90
												=
												IF
													#.w	PS2ASCII 123 +
												ELSE
													#.w	PS2ASCII 109 +
												THEN
											THEN
										ELSE
											#.w	PS2ASCII
										THEN
									THEN
									+			
									fetch.b		( flags ASCII)	
									over			( flags ASCII flags)	; CTRL modifier
									#.b	CTRL_FLAG	
									and			( flags ASCII ifCTRL)
									IF
										#.b	96
										-		( flags ASCII')
									THEN
									swap			( ASCII flags)	; ALT modifier
									#.b	ALT_FLAG	
									and			( ASCII ifCTRL)
									IF
										#.b	128
										+		( ASCII')
									THEN	
									dup			( ASCII ASCII)	; check validity
									0<			( ASCII flag)
									IF
										drop
										zero
									THEN
								THEN
								zero			( ASCII zero)
							THEN
						THEN
					THEN
				THEN
			THEN
		THEN			( zero flag)		; process a modifier
		#.w	PS2FLAGS				; update modifier flags
		dup			( flag &FLAGS &FLAGS)
		>R			( flag &FLAGS R:&FLAGS)
		fetch.b		( flag FLAGS)
		#.b	SPECIAL_FLAG 	invert			; remove prior SPECIAL flag
		and			( flag FLAGS')					
		#.b	UP_FLAG					; test if last code was UP
		over
		and			( flag FLAGS testUP)
		IF							; switch off modifier when key UP
			over		( flag FLAGS flag)
			#.b	CAPS_FLAG
			and
			IF						; ignore caps	
				nip		( FLAGS)
			ELSE
				swap		( FLAGS flag)
				invert		( FLAGS ~flag)		; remove modifier
				and		( FLAGS')				
			THEN
				#.b		UP_FLAG invert		; remove prior UP flag
				and		( FLAGS')
		ELSE							; switch on modifier otherwise
			over		( flag FLAGS flag)
			#.b	CAPS_FLAG
			and
			IF						; toggle caps
				xor		( FLAGS')
			ELSE						; set other modifiers
				or		( FLAGS')
			THEN
		THEN
		R>			( FLAGS' &FLAGS)
		store.b	
PS2DECODE.Z	rts			( zero)
;
PS2ASCII    dc.b 0
 dc.b 22
 dc.b 0
 dc.b 18
 dc.b 16
 dc.b 14
 dc.b 15
 dc.b 25
 dc.b 0
 dc.b 23
 dc.b 21
 dc.b 19
 dc.b 17
 dc.b 9
 dc.b 96
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 113
 dc.b 49
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 122
 dc.b 115
 dc.b 97
 dc.b 119
 dc.b 50
 dc.b 0
 dc.b 0
 dc.b 99
 dc.b 120
 dc.b 100
 dc.b 101
 dc.b 52
 dc.b 51
 dc.b 0
 dc.b 0
 dc.b 32
 dc.b 118
 dc.b 102
 dc.b 116
 dc.b 114
 dc.b 53
 dc.b 0
 dc.b 0
 dc.b 110
 dc.b 98
 dc.b 104
 dc.b 103
 dc.b 121
 dc.b 54
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 109
 dc.b 106
 dc.b 117
 dc.b 55
 dc.b 56
 dc.b 0
 dc.b 0
 dc.b 44
 dc.b 107
 dc.b 105
 dc.b 111
 dc.b 48
 dc.b 57
 dc.b 0
 dc.b 0
 dc.b 46
 dc.b 47
 dc.b 108
 dc.b 59
 dc.b 112
 dc.b 45
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 39
 dc.b 0
 dc.b 91
 dc.b 61
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 10
 dc.b 93
 dc.b 0
 dc.b 92
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 8
 dc.b 0
 dc.b 0
 dc.b 49
 dc.b 0
 dc.b 52
 dc.b 55
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 48
 dc.b 46
 dc.b 50
 dc.b 53
 dc.b 54
 dc.b 56
 dc.b 27
 dc.b 0
 dc.b 24
 dc.b 43
 dc.b 51
 dc.b 45
 dc.b 42
 dc.b 57
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 20
 dc.b 126
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 81
 dc.b 33
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 90
 dc.b 83
 dc.b 65
 dc.b 87
 dc.b 64
 dc.b 0
 dc.b 0
 dc.b 67
 dc.b 88
 dc.b 68
 dc.b 69
 dc.b 36
 dc.b 35
 dc.b 0
 dc.b 0
 dc.b 32
 dc.b 86
 dc.b 70
 dc.b 84
 dc.b 82
 dc.b 37
 dc.b 0
 dc.b 0
 dc.b 78
 dc.b 66
 dc.b 72
 dc.b 71
 dc.b 89
 dc.b 94
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 77
 dc.b 74
 dc.b 85
 dc.b 38
 dc.b 42
 dc.b 0
 dc.b 0
 dc.b 60
 dc.b 75
 dc.b 73
 dc.b 79
 dc.b 41
 dc.b 40
 dc.b 0
 dc.b 0
 dc.b 62
 dc.b 63
 dc.b 76
 dc.b 58
 dc.b 80
 dc.b 95
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 34
 dc.b 0
 dc.b 123
 dc.b 43
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 10
 dc.b 125
 dc.b 0
 dc.b 124
 dc.b 47
 dc.b 10
 dc.b 3
 dc.b 0
 dc.b 6
 dc.b 12
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 11
 dc.b 127
 dc.b 5
 dc.b 0
 dc.b 7
 dc.b 4
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 30
 dc.b 0
 dc.b 0
 dc.b 29
;
;KKEY.LF	dc.l	PS2DECODE.NF
;KKEY.NF	dc.b	4 128 +
;		dc.b	char Y char E char K char K
;KKEY.SF	dc.w	KKEY.Z KKEY.CF del
;KKEY.CF	BEGIN
;			#.w	EKEY.CF
;			jsr
;			#.w	PS2DECODE.CF
;			jsr
;			?dup
;		UNTIL
;KKEY.Z		rts	
;
; CSR_ADDR ( -- addr, return current cursor address)
CSR-ADDR.LF	dc.l	PS2DECODE.NF
CSR-ADDR.NF	dc.b	8 128 +
		dc.b	char R char D char D char A char - char R char S char C
CSR-ADDR.SF	dc.w	CSR-ADDR.Z CSR-ADDR.CF del
CSR-ADDR.CF	#.w	CSR-X			
		fetch.l
		lsl
		#.w	CSR-Y
		fetch.l
		#.w	COLS
		fetch.b
		2*
		mults
		drop
		+
		#.w	TEXT_ZERO
		fetch.l
CSR-ADDR.Z	+,rts				( addr)
;	
; CSR-PLOT ( c --, plot literal character at the current cursor position)
CSR-PLOT.LF	dc.l	CSR-ADDR.NF
CSR-PLOT.NF	dc.b	8 128 +
		dc.b	char T char O char L char P char - char R char S char C
CSR-PLOT.SF	dc.w	CSR-PLOT.Z CSR-PLOT.CF del
CSR-PLOT.CF	#.w	INK
		fetch.w			( _c c_)
		or				( w)
		#.w	CSR-ADDR.CF
		jsr				( w addr)
		store.w
CSR-PLOT.Z	rts		
;
; CSR-ON
CSR-ON.LF	dc.l	CSR-PLOT.NF
CSR-ON.NF	dc.b	6 128 +
		dc.b	char N char O char - char R char S char C
CSR-ON.SF	dc.w	CSR-ON.Z CSR-ON.CF del
CSR-ON.CF	#.b	char _			( c)
		#.w	CSR-ADDR.CF
		jsr				
		1+				( c addr)	; address of character
		dup				( c addr addr)
		fetch.b			( c addr char)
		#.w	CSR			( c addr char csr)
		store.b			( c addr)
		store.b		
CSR-ON.Z	rts
; saved byte underneath cursor
CSR		dc.b	32
;
CSR-OFF.LF	dc.l	CSR-ON.NF
CSR-OFF.NF	dc.b	7 128 +
		dc.b	char F char F char O char - char R char S char C
CSR-OFF.SF	dc.w	CSR-OFF.Z CSR-ON.Z del
CSR-OFF.CF	#.w	CSR
		fetch.b			( char)
		#.w	CSR-ADDR.CF		
		jsr
		1+				( char addr)
		store.b
CSR-OFF.Z	rts
;
;SCROLL	(n -- flag, scroll the screen fwd or back n lines.  returns true if out of range)
SCROLL.LF	dc.l	CSR-OFF.NF
SCROLL.NF	dc.b	6 128 +
		dc.b	char L char L char O char R char C char S
SCROLL.SF	dc.w	SCROLL.Z SCROLL.CF del
SCROLL.CF	#.w	COLS				
		fetch.b
		2*				; width of the screen including color bytes
		mults
		drop				( delta)
		#.w	TEXT_ZERO
		fetch.l			( delta current)	
		+				( new)
		dup				( new new)		; check bottom of range
		#.l	_TEXT_ZERO		( new new base)	
		<				( new flag)
		IF							; below bottom of range
			drop
			#.l	_TEXT_ZERO
			zero
			0=			( new true)
		ELSE
			dup				( new new)		; check top of range
			#.l	_TEXT_ZERO
			#.w	COLS
			fetch.b
			2*
			#.w	ROWS
			fetch.b
			multu
			drop
			+				( new new base)
			dup				( new new base base)
			rot				( new base base new)
			<				( new base flag)
			IF							; above top of range
					nip
					zero
					0=			( base true)	
			ELSE
					drop
					zero			( new false)
			THEN
		THEN
		swap				( result new)
		#.w	TEXT_ZERO
		store.l
SCROLL.Z	rts		
;
; clear screen, internal word
{CLS}.CF	#.l	_TEXT_ZERO		( start start)		; clear screen memory	
		dup
		#.w	15000			( start start 15000)			; number of words in 2 screens
		#.w	PALETTE 4 +
		fetch.b
		#.w 	256
		multu				
		drop				( start start 15000 color)		; fill word
		#.w	FILL.W.CF	
		jsr				( start)
		#.w	TEXT_ZERO					; reset pointer to TEXT_ZERO
		store.l
		zero							; reset cursor position
		#.w	CSR-X
		store.l	
		zero
		#.w	CSR-Y
		store.l
{CLS}.Z	rts		
;
CLS.LF		dc.l	SCROLL.NF
CLS.NF		dc.b	3 128 +
		dc.b	char S char L char C
CLS.SF		dc.w	CLS.Z CLS.CF	del
CLS.CF		#.w	{CLS}.CF
		jsr
CLS.Z		rts
;
; SCRSET ( --, set the ROWS and COLS according to the video mode)
SCRSET.LF	dc.l	CLS.NF
SCRSET.NF	dc.b	6 128 +
		dc.b	char T char E char S char R char C char S
SCRSET.SF	dc.w	SCRSET.Z SCRSET.CF del
SCRSET.CF	#.w	mode			; check screen mode
		fetch.b
		#.b	binary 00011000
		and
		lsr
		lsr				( v) ; v is 0, 2, 4, 6
		#.w	SCRSET
		+
		dup
		fetch.b			; pickup the columns
		#.w	COLS
		store.b			; save the variable
		1+
		fetch.b			
		#.w	ROWS			
		store.b
SCRSET.Z	rts	
SCRSET		dc.b	60 80			; VGA, interlace off, ROWS, COLUMNS
		dc.b	48 80			; VGA, interlace on
		dc.b	75 100			; SVGA, interlace off
		dc.b	60 100			; SVGA, interlace on
;
; NEWLINE ( --, implement a newline)
NEWLINE.LF	dc.l	SCRSET.NF
NEWLINE.NF	dc.b	7 128 +
		dc.b	char E char N char I char L char W char E char N
NEWLINE.SF	dc.w	NEWLINE.Z NEWLINE.CF del
NEWLINE.CF	#.w	CSR-Y
		fetch.l
		dup				( y y)
		#.w	ROWS
		fetch.b
		1-
		=				( y flag)		; test if the cursor is at the bottom of the screen or not
		IF							; cursor is bottom of screen
			drop			(  --)
			#.b	1
			#.w 	SCROLL.CF				; SCROLL forwards 1 row
			jsr			( flag)
			IF						; SCROLL returned true, indicating that the screen page is now at the bottom of the buffer
				#.w	TEXT_ZERO			; Step 1 is to copy the current screen to the top of the buffer
				fetch.l
				#.w	COLS
				fetch.b
				2*
				+						; source for copy begins one line from top of page
				#.l	_TEXT_ZERO				; destination
				#.w	14800					; size = maximum full screen less the top line
				#.w	MOVE.CF				; copy screen contents to top of buffer
				jsr
				#.w	ROWS				; Step 2 is to blank the rest of the buffer
				fetch.b				( rows)
				1-					( rows-1)
				#.w	COLS
				fetch.b				( rows-1 cols)
				2*					( rows-1 col-bytes)
				multu
				drop					( bytes)
				#.l	_TEXT_ZERO
				+					( s-addr);  start address at the last line of the first page
				dup					( s-addr s-addr)
				#.l 	_TEXT_END			( s-addr s-addr end-addr)
				swap
				-					( s-addr bytes)
				2/					( s-addr words)					
				#.w	PALETTE 4 +				; set input color (to high byte of fill word)				
				fetch.b					
				#.w	256
				multu
				drop
				#.w	FILL.W.CF				; clear remaining screen memory
				jsr
				#.l	_TEXT_ZERO			; Step 3 is to reset pointer to TEXT_ZERO	
				#.w	TEXT_ZERO			
				store.l				
			THEN
		ELSE							; cursor is mid screen - simply need to go down one row
			1+			( y')		
			#.w 	CSR-Y		( y &ADDR)
			store.l
		THEN
		zero							; zero x position
		#.w	CSR-X
		store.l
NEWLINE.Z	rts
;
CSR-FWD.LF 	dc.l	NEWLINE.NF
CSR-FWD.NF 	dc.b	7 128 +
		dc.b	char D char W char F char - char R char S char C
CSR-FWD.SF	dc.w	CSR-FWD.Z CSR-FWD.CF del
CSR-FWD.CF	#.w	CSR-X
		fetch.l
		dup
		#.w	COLS
		fetch.b
		1-
		=
		IF	
			drop
			#.w	NEWLINE.CF
			jsr
		ELSE
			1+
			#.w	CSR-X
			store.l
		THEN
CSR-FWD.Z	rts
;
CSR-BACK.LF	dc.l	CSR-FWD.NF
CSR-BACK.NF	dc.b	8 128 +
		dc.b	char K char C char A char B char - char R char S char C
CSR-BACK.SF	dc.w	CSR-BACK.Z CSR-BACK.CF del
CSR-BACK.CF	#.w	CSR-X
		fetch.l
		?dup
		IF
			1-
			#.w	CSR-X
			store.l
		THEN
CSR-BACK.Z	rts
;
CSR-TAB.LF	dc.l	CSR-BACK.NF
CSR-TAB.NF	dc.b	7 128 +
		dc.b	char B char A char T char - char R char S char C
CSR-TAB.SF	dc.w	CSR-TAB.Z CSR-TAB.CF del
CSR-TAB.CF	#.w	CSR-X
		fetch.l		( x)
		#.w	TAB
		fetch.l		( x t)
		over
		over			( x t x t)
		divu	
		drop			( x t r)
		-
		+
		dup
		#.w	COLS
		fetch.b
		1-
		>
		IF
			drop
			#.w	NEWLINE.CF
			jsr
		ELSE
			#.w	CSR-X
			store.l
		THEN
CSR-TAB.Z	rts
;
; VEMITRAW ( n --, emit a character to the VDU excluding non-printing recognition and cursor update)
VEMITRAW.LF	dc.l	CSR-TAB.NF
VEMITRAW.NF	dc.b	8 128 +
		dc.s	VEMITRAW
VEMITRAW.SF	dc.w	VEMITRAW.Z VEMITRAW.CF del
VEMITRAW.CF	#.w	CSR-PLOT.CF		; plot
		jsr
		#.w	CSR-FWD.CF		; advance cursor
		jsr	
VEMITRAW.Z	rts
;
; {VEMIT} (n --, emit a character to the VDU including non-priniting recognition but excluding cursor update, internal word)
{VEMIT}.CF 	#.b	EOL
		over
		=
		IF								; newline
			drop
			#.w 	NEWLINE.CF
			jsr
		ELSE
			#.b	~EOL
			over
			=
			IF							; not newline - ignore
				drop
			ELSE
				#.b	8
				over
				=
				IF						; backspace
					drop
					#.w 	CSR-BACK.CF
					jsr
					zero
					#.w 	CSR-PLOT.CF
					jsr
				ELSE
					#.b	9
					over
					=
					IF					; tab
						drop
						#.w	CSR-TAB.CF
						jsr
					ELSE
						#.b	12
						over
						=
						IF				; clear screen
							drop
							#.w	{CLS}.CF
							jsr
						ELSE				; other literal
							#.w	VEMITRAW.CF		 ; emit the literal
							jsr	
						THEN
					THEN
				THEN
			THEN
		THEN
		rts
;
; VEMIT ( n --, emit a character to the VDU, including non-printing recognition and cursor update)
VEMIT.LF	dc.l	VEMITRAW.NF
VEMIT.NF	dc.b	5 128 +
		dc.b	char T char I char M char E char V
		dc.w	VEMIT.Z VEMIT.CF del
VEMIT.CF	#.w	CSR-OFF.CF					; undraw cursor
		jsr
		#.w	{VEMIT}.CF
		jsr
		#.w	CSR-ON.CF					; draw cursor
		jsr
VEMIT.Z	rts
;
; VTYPERAW ( addr len, type to VDU, excluding non-printing recognition including cursor update)
VTYPERAW.LF	dc.l	VEMIT.NF
VTYPERAW.NF	dc.b	8 128 +
		dc.b	char W char A char R char E char P char Y char T char V
VTYPERAW.SF	dc.w	VTYPERAW.Z VTYPERAW.CF del
VTYPERAW.CF	#.w	CSR-OFF.CF					; undraw cursor
		jsr
		?dup
		IF
			over			( addr len addr)
			+			( start end)
			swap			( end start)
			DO
				R@
				fetch.b
				#.w	VEMITRAW.CF
				jsr
			LOOP
		THEN
		#.w	CSR-ON.CF					; draw cursor
		jsr
VTYPERAW.Z	rts
;
; VTYPE ( addr len, type to VDU, including non-printing recognition and cursor update)
VTYPE.LF	dc.l	VTYPERAW.NF
VTYPE.NF	dc.b	5 128 +
		dc.b	char E char P char Y char T char V
VTYPE.SF	dc.w	VTYPE.Z VTYPE.CF del
VTYPE.CF	#.w	CSR-OFF.CF					; undraw cursor
		jsr
		?dup
		IF
			over			( addr len addr)
			+			( start end)
			swap			( end start)
			DO
				R@
				fetch.b
				#.w	{VEMIT}.CF
				jsr
			LOOP
		THEN
		#.w	CSR-ON.CF					; draw cursor
		jsr
VTYPE.Z	rts
;
KEY?.LF	dc.l	VTYPE.NF
KEY?.NF	dc.b	4 128 +
		dc.b	char ? char Y char E char K
KEY?.SF	dc.w	KEY?.Z KEY?.CF del
KEY?.CF	#.w	KEY?_VECTOR
		fetch.l
		jsr
KEY?.Z		rts
;
KEY.LF		dc.l	KEY?.NF
KEY.NF		dc.b	3 128 +
		dc.b	char Y char E char K
KEY.SF		dc.w	KEY.Z KEY.CF del
KEY.CF		#.w	KEY_VECTOR
		fetch.l
		jsr
KEY.Z		rts
;
EMIT.LF	dc.l	KEY.NF
EMIT.NF	dc.b	4 128 +
		dc.b	char T char I char M char E
EMIT.SF	dc.w	EMIT.Z EMIT.CF del
EMIT.CF	#.w	EMIT_VECTOR
		fetch.l
		jsr
EMIT.Z		rts
;
TYPE.LF	dc.l	EMIT.NF
TYPE.NF	dc.b	4 128 +
		dc.b	char E char P char Y char T
TYPE.SF	dc.w	TYPE.Z TYPE.CF del
TYPE.CF	#.w	TYPE_VECTOR
		fetch.l
		jsr
TYPE.Z		rts
;
TYPERAW.LF	dc.l	TYPE.NF
TYPERAW.NF	dc.b	7 128 +
		dc.b	char W char A char R char E char P char Y char T
TYPERAW.SF	dc.w	TYPERAW.Z TYPERAW.CF del
TYPERAW.CF	#.w	TYPERAW_VECTOR
		fetch.l
		jsr
TYPERAW.Z	rts
;
>REMOTE.LF	dc.l	TYPERAW.NF
>REMOTE.NF	dc.b	7 128 +
		dc.b	char E char T char O char M char E char R char >
>REMOTE.SF	dc.w	>REMOTE.Z >REMOTE.CF del
>REMOTE.CF	#.w	SEMIT.CF
		#.w	EMIT_VECTOR
		store.l
		#.w	STYPE.CF
		#.w	TYPE_VECTOR
		store.l
		#.w	STYPE.CF
		#.w	TYPERAW_VECTOR
		store.l	
>REMOTE.Z	rts	
;
>LOCAL.LF	dc.l	>REMOTE.NF
>LOCAL.NF	dc.b	6 128 +
		dc.b	char L char A char C char O char L char >
>LOCAL.SF	dc.w	>LOCAL.Z >LOCAL.CF del
>LOCAL.CF	#.w	VEMIT.CF
		#.w	EMIT_VECTOR
		store.l
		#.w	VTYPE.CF
		#.w	TYPE_VECTOR
		store.l
		#.w	VTYPERAW.CF
		#.w	TYPERAW_VECTOR
		store.l	
>LOCAL.Z	rts
;	
<LOCAL.LF	dc.l	>LOCAL.NF
<LOCAL.NF	dc.b	6 128 +
		dc.b	char L char A char C char O char L char <
<LOCAL.SF	dc.w	<LOCAL.Z <LOCAL.CF del
<LOCAL.CF	#.w	KKEY.CF
		#.w	KEY_VECTOR
		store.l
		#.w	KKEY?.CF
		#.w	KEY?_VECTOR
		store.l
<LOCAL.Z	rts
;
<REMOTE.LF	dc.l	<LOCAL.NF
<REMOTE.NF	dc.b	7 128 +
		dc.b	char E char T char O char M char E char R char <
<REMOTE.SF	dc.w	<REMOTE.Z <REMOTE.CF del
<REMOTE.CF	#.w	SKEY.CF
		#.w	KEY_VECTOR
		store.l
		#.w	SKEY?.CF
		#.w	KEY?_VECTOR
		store.l
<REMOTE.Z	rts
;
; SPI functions
SPI.CS-hi	#.w	SPI.control					; DESELECT - CS is active low
		dup
		fetch.b	
		#.b	1 
		or 
		swap
		store.b	 
		rts
;
SPI.CS-lo 	#.w	SPI.control 					; SELECT - CS is active low
		dup
		fetch.b
		#.b	254 
		and 
		swap
		store.b
		rts	
;	
SPI.MOSI-hi	#.w	SPI.control 
		dup
		fetch.b
		#.b	2 
		or 
		swap
		store.b
		rts
;
SPI.MOSI-lo 	#.w	SPI.control 
		dup
		fetch.b
		#.b	253 
		and 
		swap
		store.b 
		rts
;
SPI.slow 	#.b	255 						; 196kHz at 50MHz clock
		#.w	SPI.divide 
		store.b
		rts
;	
SPI.fast 	#.b	8 						; 6.25MHz at 50MHz
		#.w	SPI.divide 
		store.b	
		rts
;
; SPI.wait ( --, wait until the SPI transfer-bus is available)	
SPI.wait 	#.w	SPI.status 
		BEGIN
			dup
			fetch.b
			#.b	1
			and 
		UNTIL
		drop,rts
;
; SPI.put ( n --, put a byte to the SPI port)
SPI.put	CALL	SPI.wait 
		#.w	SPI.data
		store.b
		rts
;
; SPI.get ( -- n, get a byte from the SPI port)
SPI.get	#.b	255 
		CALL	SPI.put 
		CALL	SPI.wait 
		#.w	SPI.data
		fetch.b
		rts
;
; SD card functions
; SD.cmd ( chk b1 b2 b3 b4 cmd# --, SD command)
SD.cmd		#.b	6
		zero
		DO
			CALL	SPI.put 
		LOOP
		rts
;
; SD.get-rsp ( -- n, get first byte of response from the sd-card)
SD.get-rsp	zero
		BEGIN
			drop
			CALL	SPI.get
			dup 
			#.b	255 
			<>
		UNTIL
		rts
;
; SD.get-R1 ( -- n, get an R1 response from the sd-card)
SD.get-R1	CALL	SD.get-rsp
		CALL	spi.get 
		drop		; one further read always required
		rts
;
;SD.ver			; xxxxx [block/byte] [v2/v1];
SD.ver		dc.l	0	
;
; SD.init ( --, SD card reset, version check and initialize)
SD.init.LF	dc.l	<REMOTE.NF
SD.init.NF	dc.b	7 128 +
		dc.s	SD.init
SD.init.SF	dc.w	SD.init.Z SD.init.CF del
SD.init.CF	#.w	5000 
		CALL	timeout.cf
		CALL	spi.slow 
		CALL	spi.cs-hi 		; power sequence dummy clock
		#.b	80 
		zero
		DO
			#.b	255 
			CALL	spi.put 
		LOOP
		CALL	spi.cs-lo 	
		BEGIN				; CMD0 repeated until good
			#.b	149 
			zero
			zero
			zero
			zero
			#.b	64 
			CALL	sd.cmd 
			CALL	sd.get-R1 
			#.b	1 
			<>
		WHILE				
			#.b	100 		; 100 ms delay
			CALL	ms.cf
		REPEAT
		#.b	135 			; CMD8	
		#.b	170 
		#.b	1 
		zero
		zero
		#.b	72 
		CALL 	sd.cmd 
		CALL	sd.get-rsp 
		#.b	1 
		= 
		IF					; CMD8 accepted, read data bytes
			#.b	4 
			zero
			DO 
				CALL	spi.get 
			LOOP		( b4 b3 b2 b1)
			CALL	spi.get 		; one further read always required
			drop 			
			#.b	170 
			= 
			swap 			( b4 b3 f b2)
			#.b	1 
			= 
			and 
			nip 
			nip 
			IF			( f) 	; 01xAA confirmed, initialize card
			BEGIN
				#.b 	1 		; CMD55
				zero
				zero
				zero
				zero
				#.b	119 
				CALL	sd.cmd 
				CALL	sd.get-R1 	; CMD55 is just a header
				drop			
				#.b	1 		; CMD41hi
				zero 
				zero
				zero 
				#.b	64 
				#.b	105 
				CALL	sd.cmd	
				CALL	sd.get-R1
				0= 
			UNTIL
			#.b	1 			; CMD58
			zero
			zero 
			zero 
			zero 
			#.b	122 
			CALL	sd.cmd	
			CALL	sd.get-rsp 		; ignore R1
			drop		
			#.b	4
			zero 
			DO 
				CALL	spi.get
			LOOP		( b4 b3 b2 b1)
			CALL	spi.get 		; one further read always required
			drop 			
			drop 
			drop 
			drop 
			#.b	64 			; test CSS bit in OCR
			and	
			IF
				#.b	3 		; SD V2.0 block address
				#.w	sd.ver
				store.l		
			ELSE
				#.b	1 		; SD V.20 byte address
				#.w	sd.ver 
				store.l		
			THEN
			CALL	spi.fast		; V2.0 supports high speed
		ELSE					; 01xAA mismatch
			#.w	1001 
			CALL	ERROR
		THEN
	ELSE						; CMD8 rejected, initialize card
		BEGIN
			#.b	1 			; CMD55
			zero 
			zero 
			zero 
			zero 
			#.b	119 
			CALL	sd.cmd 
			CALL	sd.get-R1 		; CMD55 is just a header
			drop		
			#.b	1 			; CMD41lo
			zero 
			zero
			zero
			zero
			#.b	105 
			CALL	sd.cmd		
			CALL	sd.get-R1
			0=
		UNTIL
			zero 				; SD V1.0
			#.w	sd.ver 
			store.l			
		THEN
		#.b	1 				; CMD16
		zero 
		#.b	2 
		zero 
		zero 
		#.b	80 
		CALL	sd.cmd 
		CALL	sd.get-rsp 
		drop	
		CALL 	SPI.CS-hi 			; DESELECT
		#.b	255 
		CALL	spi.put			
		zero	
		CALL	timeout.cf
SD.init.Z	rts
;
; SD.sector-code ( n -- b4 b3 b2 b1, scale and split sector address)
SD.sector-code 	#.w	sd.ver 			
		fetch.l 
		#.b	2 
		and			
		0=
		IF					; scale sector to bytes
			#.w	512 
			multu
			drop
		THEN	
		dup 		
		#.b	255 
		and 					; bits 0 - 7
		swap				
		#.b	3 
		zero 
		DO					; bits 8 - 31
			#.b	8 
			CALL	rshift.cf
			dup
			#.b	255 
			and 
			swap
		LOOP
		drop,rts		
;
; SD.select&check ( --, select and wait for SD card)
SD.select&check 	CALL spi.cs-lo		; SELECT
		BEGIN				
			CALL spi.get 
			#.b	255 
			=				; if CS is asserted while card is busy then card will set D0 low
		UNTIL
		rts
;
; SD.read-sector ( addr n --, read 512 bytes from sector n into a buffer at addr)
SD.read-sector.LF	dc.l	SD.init.NF
SD.read-sector.NF	dc.b	14 128 +
			dc.b	char R char O char T char C char E char S char - char D char A char E char R char . char D char S
SD.read-sector.SF	dc.w	SD.read-sector.Z SD.read-sector.CF del
SD.read-sector.CF	#.w	1000 
		CALL 	timeout.cf
		CALL 	sd.select&check
		#.b	1 				; checksum
		swap					
		CALL 	sd.sector-code		; encode sector number
		#.b	81 				; complete CMD17
		CALL 	sd.cmd				
		CALL 	sd.get-R1 
		0<> 
		IF					; check response OK
			#.w	1005 
			CALL	ERROR.CF
		THEN
		BEGIN					; wait for data token
			CALL	spi.get
			#.b	254 
			=
		UNTIL
		dup 
		#.w	512 
		+ 
		swap 
		DO 					; read sector
			CALL 	spi.get 
			I 
			store.b 
		LOOP	
		#.b	3 
		zero 
		DO 					; drop CRC and read safety byte
			CALL 	spi.get 
			drop 
		LOOP			
		CALL 	SPI.CS-hi 
		#.b	255 
		CALL	spi.put			; DESELECT
		zero 
		CALL	timeout.CF
SD.read-sector.Z		rts
;
; SD.write-sector ( addr n --, write 512 byte to sector n from addr)
SD.write-sector.LF	dc.l	SD.read-sector.NF
SD.write-sector.NF	dc.b	15 128 +
			dc.b	char R char O char T char C char E char S char - char E char T char I char R char W char . char D char S
SD.write-sector.SF	dc.w	SD.write-sector.Z SD.write-sector.CF del
SD.write-sector.CF	#.w	2000
		CALL	timeout.CF
		CALL	sd.select&check	
		#.b	1 				; checksum
		swap					
		CALL	sd.sector-code		; encode sector number
		#.b	88 				; complete CMD24
		CALL 	sd.cmd				
		CALL	sd.get-R1 
		0<> 
		IF					; check response OK
			#.w	1010 
			CALL	ERROR.CF
		THEN
		#.b	255 
		CALL	spi.put			; space
		#.b	254 
		CALL	spi.put			; initiate data packet
		dup 
		#.w	512 
		+ 
		swap 
		DO 					; write sector
			I 
			fetch.b 
			CALL spi.put 
		LOOP	
		#.b	2 
		zero 
		DO 					; dummy checksum
			#.b	1 
			CALL	spi.put 
		LOOP			
		CALL	sd.get-R1 
		#.b	31 
		and 
		#.b	5 
		<> 
		IF					; check data response
			#.w	1011 
			CALL	ERROR.CF		; write error
		THEN	
		CALL	SPI.CS-hi 
		#.b	255 
		CALL	spi.put			; DESELECT
		zero 
		CALL	timeout.cf
SD.write-sector.Z		rts
;
; FAT.read-long ( addr n -- x, get a little endian longword from the buffer)
FAT.read-long.LF	dc.l	SD.write-sector.NF
FAT.read-long.NF	dc.b	13 128 +
			dc.s	FAT.read-long
FAT.read-long.SF	dc.w	FAT.read-long.Z FAT.read-long.CF del
FAT.read-long.CF	+
		dup 
		#.b	4 
		+ 
		swap 
		DO 
			i 
			fetch.b 
		LOOP
		#.b	3 
		zero 
		DO 
			#.w	256 
			multu
			drop 
			+ 
		LOOP
FAT.read-long.Z	rts
;
; FAT.write-long ( x addr n --, write a little endian longword x to the buffer at position n)
FAT.write-long.LF	dc.l	FAT.read-long.NF
FAT.write-long.NF	dc.b	14 128 +
			dc.s	FAT.write-long
FAT.write-long.SF	dc.w	FAT.write-long.Z FAT.write-long.CF del
FAT.write-long.CF	+ 
		>R 
		>R	( R: x addr+n)
		R@ 
		#.b	24 
		CALL	rshift.cf 
		#.b	255 
		and	
		R@ 
		#.b	16 
		CALL	rshift.cf
		#.b	255 
		and	
		R@ 
		#.b	8 
		CALL	rshift.cf 
		#.b	255 
		and	
		R> 
		#.b	255 
		and
		R> 
		dup 
		#.b	4 
		+ 
		swap
		DO 
			i 
			store.b 
		LOOP
FAT.write-long.Z	rts
;
	
; FAT.read-word ( addr n -- x, get a little endian word from the buffer)
FAT.read-word.LF	dc.l	FAT.write-long.NF
FAT.read-word.NF	dc.b	13 128 +
			dc.s	FAT.read-word
FAT.read-word.SF	dc.w	FAT.read-word.Z FAT.read-word.CF del
FAT.read-word.CF	1+ 
		+
		dup 
		fetch.b 
		#.w	256 
		multu
		drop 
		swap
		1- 
		fetch.b 
FAT.read-word.Z	+,rts
;
; FAT.write-word ( x addr n --, write a litte endian word to the buffer)
FAT.write-word.LF	dc.l	FAT.read-word.NF
FAT.write-word.NF	dc.b	14 128 +
			dc.s	FAT.write-word
FAT.write-word.SF	dc.w	FAT.write-word.Z FAT.write-word.CF del
FAT.write-word.CF	+ 
		>R 
		>R	( R : x addr+n)
		R@ 
		#.b	8 
		CALL	rshift.cf 
		#.b	255 
		and	
		R> 
		#.b	255 
		and
		R@ 
		store.b
		R> 
		1+ 
		store.b
FAT.write-word.Z		rts
;
; MOUNT ( --, initiaize the SD card and FAT data structures)
MOUNT.LF	dc.l	FAT.write-word.NF
MOUNT.NF	dc.b	5 128 +
		dc.s	MOUNT
MOUNT.SF	dc.w	MOUNT.Z MOUNT.CF del
MOUNT.CF	call	sd.init.cf
		#.l	_fat.buf 
		dup
		>R
		zero 
		call	sd.read-sector.cf
		R@
		#.w	510 
		call	fat.read-word.cf 
		#.w	43605 
		<> 
		IF		; confirm sector signature 0xAA55
			#.w	2000 
			call	error.cf
		THEN
		R@ 
		#.b	82 
		CALL	fat.read-word.cf 
		#.w	16710 
		<> 
		IF		; confirm FAT32 signature 0x4146
			#.w	2001 
			CALL	error.cf
		THEN
		R@			
		#.b	13 
		+ 
		fetch.b 
		#.w	fat.secperclus 
		store.l
		R@
		#.b	44 
		CALL	fat.read-long.cf 
		dup 
		#.w	fat.rootclus 
		store.l 
		#.w	FAT.CurrentDirectory 
		store.l
		R@
		#.b	32 
		CALL	fat.read-long.cf 
		#.w	fat.TotalSectors 
		store.l
		R@ 
		#.b	14 
		CALL	fat.read-word.cf 
		dup 
		#.w	fat.rsvdseccnt 
		store.l				( RsvdSecCnt)
		R@ 
		#.b	16 
		+ 
		fetch.b				( RsvdSecCnt NumFATs)
		R@
		#.b	36 
		CALL	fat.read-long.cf		( RsvdSecCnt NumFATs SecPerFAT)
		multu
		drop
		+ 
		#.w	fat.firstdatasector 
		store.l
		R@
		#.b	1 
		CALL	sd.read-sector.cf		; FAT32 FSInfo
		R@
		zero 
		CALL	fat.read-long.cf 
		#.l	hex 41615252 
		<> 
		IF					; confirm valid FSInfo sector
			#.w	2002 
			CALL	error					
		THEN
		R>
		#.w	492 
		CALL	fat.read-long.cf 
		dup
		zero
		1- 
		= 
		IF 
			drop 
			#.b	2 
		THEN
		#.w	FAT.NextFreeCluster 
		store.l
		zero 
		#.w	FAT.FATinBuf 
		store.l				; FAT buffer initialized
MOUNT.Z		rts
;
; FAT.UpdateFSInfo ( --, update the FAT32 FSInfo sector with next free cluster)
FAT.UpdateFSInfo.LF	dc.l	MOUNT.NF
FAT.UpdateFSInfo.NF	dc.b	16 128 +
			dc.s	FAT.UpdateFSInfo
FAT.UpdateFSInfo.SF	dc.w	FAT.UpdateFSInfo.Z FAT.UpdateFSInfo.CF del
FAT.UpdateFSInfo.CF	#.l	_fat.buf 
		dup
		>R
		#.b	1 
		CALL	sd.read-sector.cf
		#.w	FAT.NextFreeCluster 
		fetch.l 
		R@	 			
		#.w	492 
		CALL	FAT.write-long.CF
		R> 
		#.b	1 
		CALL	sd.write-sector.cf
FAT.UpdateFSInfo.Z		rts
;
; FAT.clus2sec ( n -- n, given a valid cluster number return the number of the first sector in that cluster)
FAT.clus2sec.LF	dc.l	FAT.UpdateFSInfo.NF
FAT.clus2sec.NF	dc.b	12 128 +
			dc.s	FAT.clus2sec
FAT.clus2sec.SF	dc.w	FAT.clus2sec.Z FAT.clus2sec.CF del
FAT.clus2sec.CF	1-				
		1-					; first cluster is number 2
		#.w	fat.secperclus 
		fetch.l
		multu
		drop
		#.w	fat.firstdatasector 
		fetch.l
FAT.clus2sec.Z	+,rts
;
;
; FAT.prep-fat ( n -- ThisFATEntOffset, calulate location and load the appropriate FAT sector into fat.fatbuf)
FAT.prep-fat	#.b	4 
		multu
		drop			( FATOffset)
		#.w	512 
		divu			( rem quo)
		#.w	fat.rsvdseccnt 
		fetch.l 
		+ 			( ThisFATEntOffset ThisFATSecNum)
		dup 
		#.w	FAT.FATinBuf 
		fetch.l 
		<> 
		IF
			dup 
			#.w	FAT.FATinBuf 
			store.l			; remember the buffered sector
			#.l	_fat.buffat 
			swap	 			( ThisFATEntOffset fat.fatbuf ThisFATSecNum)
			CALL	SD.read-sector.cf	( ThisFATEntOffset)
		ELSE
			drop				( ThisFATEntOffset)
		THEN
		rts
;
; FAT.get-fat ( n -- x, return the FAT entry for a given cluster)
FAT.get-fat.LF	dc.l	FAT.clus2sec.NF
FAT.get-fat.NF	dc.b	11 128 +
			dc.s	FAT.get-fat
FAT.get-fat.SF	dc.w	FAT.get-fat.Z FAT.get-fat.CF del
FAT.get-fat.CF	CALL	FAT.prep-fat
		#.l	_fat.buffat 
		swap					( fat.buf ThisFATEntOffset)
		CALL	fat.read-long.cf
		#.l	hex	0FFFFFFF
FAT.get-fat.Z	and,rts
;
; FAT.put-fat ( value cluster --, place value in the FAT location for cluster)
FAT.put-fat.LF	dc.l	FAT.get-fat.NF
FAT.put-fat.NF	dc.b	11 128 +
			dc.s	FAT.put-fat
FAT.put-fat.SF	dc.w	FAT.put-fat.Z FAT.put-fat.CF del
FAT.put-fat.CF	CALL	FAT.prep-fat		( value ThisFATEntOffset)
		#.l	_fat.buffat 
		swap					( value fat.buf ThisFATEntOffset)
		CALL	fat.write-long.CF
		#.l	_FAT.buffat 
		#.w	FAT.FATinBuf 
		fetch.l
		CALL	SD.write-sector.cf
FAT.put-fat.Z		rts
;
; FAT.string2filename ( addr n -- addr, convert an ordinary string to a short FAT filename)
FAT.string2filename.LF	dc.l	FAT.put-fat.NF
FAT.string2filename.NF	dc.b	19 128 +
				dc.s 	FAT.string2filename
FAT.string2filename.SF	dc.w	FAT.string2filename.Z FAT.string2filename.CF del
FAT.string2filename.CF	>R 
		>R		
		#.l	_PAD 				; was FAT.filestring
		dup 
		dup 
		dup					 
		#.b	12 
		+ 
		swap 
		DO 					; fill the output string with blanks
			#.b	32 
			i 
			store.b 
		LOOP	 			
		R> 
		R>					( filestring filestring addr n)
		?dup 
		IF		
			#.b	12 
			CALL	min.cf
			over 
			+ 
			swap 				( filestring filestring addrE addr)					
			DO				; loop over the input string up to 12 characters
				i 
				fetch.b 
				dup 
				#.b	46 		; .
				= 				 
				IF
					drop 
					drop 
					dup 
					#.b	8 
					+		; re-position in output string
				ELSE
					CALL	upper.cf
					over 
					store.b 
					1+		; save and increment position in output string
				THEN				
			LOOP
			drop
		ELSE					; zero length interpret as ".." for up directory
			drop 
			#.b	46 
			over 
			store.b 
			1+
			#.b	46 
			swap 
			store.b
		THEN
FAT.string2filename.Z	rts
;
; FAT.find-file-local ( dirCluster addr n -- dirSector dirOffset firstCluster size flags TRUE | FALSE, find in local folder)
FAT.find-file-local	CALL	FAT.String2Filename.CF	( cluster filestring)
		swap 
		dup 
		>R					( filestring cluster R:cluster)
		BEGIN
			CALL	FAT.Clus2Sec.CF	( filestring firstSec R:cluster)
			dup 
			#.w	FAT.SecPerClus 
			fetch.l 
			+ 
			swap				( filestring lastSec firstSec R:cluster)	
		DO					( filestring R:LOOP cluster)	; examine each sector in the cluster
			#.l	_FAT.buf 
			i 
			CALL	SD.read-sector.cf
			#.l	_FAT.buf 
			dup 
			#.w	512 
			+ 
			swap 
			DO				( filestring R:LOOP LOOP cluster)	; examine each 32 byte entry in the sector
				i 
				fetch.b 
				dup 
				0= 
				IF 			; empty entry and no following entries - exit false flag
					UNLOOP
					UNLOOP 
					nip 
					R> 
					drop 
					rts 
				THEN	
				#.b	229 
				<> 
				IF							; non-0xE5 first byte indicates valid entry
					i 
					#.b	11 
					+ 
					fetch.b 
					#.b	15 
					and 
					#.b	15 
					<> 
					IF						; is not a long-name entry
						dup 
						#.b	11 
						i 
						#.b	11 
						CALL	$=.cf 
						IF					; test string match
							drop							; remove filestring	
							j							; dirSector
							i 
							#.l	_FAT.buf 
							-							; directory offset 
							i 
							#.b	20 
							CALL	FAT.read-word.cf 
							#.l	65536 
							multu
							drop
							i 
							#.b	26 
							CALL 	FAT.read-word.cf 
							+ 							; startCluster
							i 
							#.b	28 
							CALL	FAT.read-long.cf 				; size		
							i 
							#.b	11 
							+ 
							fetch.b						; flags
							UNLOOP 
							UNLOOP
							R> 
							drop 
							zero
							1-
							rts							; exit with true flag
						THEN
					THEN
				THEN
				#.b	32 
				+LOOP
				LOOP 
				R>					( filestring currentCluster)
				CALL	FAT.get-fat.CF		( filestring nextCluster)
				dup 
				#.l	hex 0FFFFFFF 								; End-of-clusters mark
				=					( filestring nextCluster flag) 	
			UNTIL
		drop 
		drop 
		zero,rts									; likely bad directory
;
; FAT.find-file ( addr n -- dirSector dirOffset firstCluster size flags TRUE | FALSE, find from current directory)
FAT.find-file.LF	dc.l	FAT.string2filename.NF
FAT.find-file.NF	dc.b	13 128 +
			dc.s	FAT.find-file
FAT.find-file.SF	dc.w	FAT.find-file.Z FAT.find-file.CF del
FAT.find-file.CF	#.w	FAT.CurrentDirectory 
		fetch.l 
		rot 
		rot
		over 
		+ 
		1- 
		dup 
		>R 
		over 								( cluster startAddr endAddr startAddr R:endAddr-1)
		over
		over
		-
		IF
			DO							( cluster startAddr R: LOOP endAddr-1)
				i 
				fetch.b 
				dup 
				#.b	92 
				= 
				swap 
				#.b	47 
				= 
				or 
				IF 
					i 
					over 
					-					( cluster Addr n)
					CALL	FAT.find-file-local 
					IF 
						dup 
						#.b	15 
						and 
						#.b	15 
						<> 
						swap 
						#.b	16 
						= 
						and 
						IF					; is a directory
							drop 
							nip 
							nip 
							?dup 
							0= 
							IF 				; root directory adjustment
								#.w	FAT.RootClus 
								fetch.l 
							THEN		
							i 
							1+			( newCluster newAddr)
						ELSE					; cannot parse filepath - not a directory
							UNLOOP 
							R> 
							drop 
							drop 
							drop 
							drop 
							drop 
							zero 
							rts	
					THEN
				ELSE						; cannot parse filepath - not found
					UNLOOP 
					R> 
					drop 
					zero 
					rts				
				THEN
			THEN
			LOOP							( cluster Addr R:endAddr-1)
		ELSE
			drop
			drop
		THEN
		dup 
		fetch.b 
		dup 
		#.b	92 
		= 
		swap 
		#.b	47 
		= 
		or 
		IF 
			R> 						( cluster addr 0)	; n=0 will be interpreted as ".."
			drop 
			zero						
		ELSE
			R> 
			1+ 
			over 
			- 						( cluster addr n)
		THEN
		CALL	FAT.find-file-local 	
FAT.find-file.Z	rts
;
; EX
; FAT.load-file ( addr firstCluster --, load a file to addr given the first cluster, cluster by cluster)
FAT.load-file.LF	dc.l	FAT.find-file.NF
FAT.load-file.NF	dc.b	13 128 +
			dc.s	FAT.load-file
FAT.load-file.SF	dc.w	FAT.load-file.Z FAT.load-file.CF del
FAT.load-file.CF	BEGIN						
			dup 
			>R					( addr currentCluster R:currentCluster)
			CALL	FAT.Clus2Sec.CF		( addr firstSec R:currentCluster)
			dup 
			#.w	FAT.SecPerClus 
			fetch.l 
			+ 
			swap					( addr lastSec firstSec R:currentCluster)
			DO
				dup 
				i 
				CALL SD.read-sector.cf		
				#.w	512 
				+				( addr)
			LOOP
			R>					( addr currentCluster)
			CALL	FAT.get-fat.CF		( addr nextCluster)
			dup 
			#.l	hex 0FFFFFFF 			; End-of-clusters mark
			=					( addr nextCluster flag) 	
		UNTIL
		drop 
FAT.load-file.Z	drop,rts
;
; include ( "FILEPATH" --)
include.LF	dc.l	FAT.load-file.NF
include.NF	dc.b	7 128 +
		dc.s	include
include.SF	dc.w 	include.Z include.CF del
include.CF	#.b	32 
		CALL	word.cf
		CALL	count.cf 
		CALL	FAT.find-file.CF 			( dirSector dirOffset firstCluster size flags TRUE | FALSE)
		IF
			drop 
			>R 
			nip 
			nip 
			#.l	hex 00FF0000 			; addr is 64K below top of memory
			dup 
			rot					( addr addr firstCluster R:size)			
			CALL	FAT.load-file.CF 		( addr R:size)
			R> 
			CALL	evaluate.cf 			( )
		ELSE
			#.b	4 
			CALL	ERROR.CF
		THEN
include.Z		rts
;				
COLOR-TABLE.LF dc.l	include.NF
COLOR-TABLE.NF dc.b	11 128 +
		 dc.b char E char L char B char A char T char - char R char O char L char O char C
COLOR-TABLE.SF dc.w	COLOR-TABLE.Z COLOR-TABLE.CF del
COLOR-TABLE.CF #.w	CR.CF
		 jsr
		 #.w	256
		 zero
		 DO
			R@
			#.w	INK
			store.b
			R@
			#.w 	DOT.CF
			jsr
			#.w	SPACE.CF
			jsr
		LOOP
		#.w	PALETTE 1 +
		fetch.b
		#.w	INK
		store.b
COLOR-TABLE.Z	rts
;
; MS ( n --, wait for n ms)
MS.LF		dc.l	COLOR-TABLE.NF
MS.NF		dc.b	2 128 +
		dc.b	char S char M
MS.SF		dc.w	MS.Z MS.CF del
MS.CF		#.w	MScounter
		fetch.l		( n ms)
		+			( end)
			begin
				#.w	MScounter
				fetch.l		( end now)
				over			( end now end)
				=
			until
MS.Z		drop,rts				
;
; RESET ( --)
RESET.LF	dc.l	MS.NF
RESET.NF	dc.b	5 128 +
		dc.b	char T char E char S char E char R
RESET.SF	dc.w	RESET.Z RESET.CF del
RESET.CF	#.w	END
		#.w	HERE_
		store.l
		#.w	_END
		#.w	HERE1
		store.l
		#.w	LAST.NF
		#.w	LAST-NF
		store.l
		#.b	10
		#.w 	BASE_
		store.l
		#.w	START.CF
		jsr
RESET.Z	rts
;
; TIMEOUT ( n --, set a reset timer for n milliseconds)
TIMEOUT.LF	dc.l	RESET.NF
TIMEOUT.NF	dc.b	7 128 +
		dc.b	char T char U char O char E char M char I char T
TIMEOUT.SF	dc.w	TIMEOUT.Z TIMEOUT.CF del
TIMEOUT.CF	?dup
		IF
			#.w	MS.TIMEOUT
			store.l
			#.w	intmask
			fetch.b
			#.b	intmask_MS
			or
		ELSE
			#.w	intmask
			fetch.b
			#.b	255 intmask_MS -
			and
		THEN
		#.w	intmask
		store.b			
TIMEOUT.Z	rts
;
; BACKGROUND ( n --, set the background color)
BACKGROUND.LF	dc.l	TIMEOUT.NF
BACKGROUND.NF	dc.b	10 128 +
		dc.b	char D char N char U char O char R char G char K char C char A char B
BACKGROUND.SF	dc.w	BACKGROUND.Z BACKGROUND.CF del
BACKGROUND.CF	#.w	background
		store.b
BACKGROUND.Z	rts
;
; INTERLACE ( flag --, set interlace mode on or off)
INTERLACE.LF	dc.l	BACKGROUND.NF
INTERLACE.NF	dc.b	9 128 +
		dc.b	char E char C char A char L char R char E char T char N char I
INTERLACE.SF	dc.w	INTERLACE.Z INTERLACE.CF del
INTERLACE.CF	#.w	mode
		fetch.b
		swap
		IF
			#.b	8
			or
		ELSE
			#.b	247
			and
		THEN
		#.w	mode
		store.b	
		#.w	SCRSET.CF
		jsr
INTERLACE.Z	rts
;
; SVGA ( flag --. set SGVA mode on or off)
VGA.LF	dc.l	INTERLACE.NF
VGA.NF	dc.b	3 128 +
		dc.b	char A char G char V 
VGA.SF	dc.w	VGA.Z VGA.CF del
VGA.CF		#.w	CLS.CF
		jsr
		#.w	mode
		fetch.b
		swap
		IF
			#.b	16
			or
		ELSE
			#.b	239
			and
		THEN
		#.w	mode
		store.b	
		#.w	SCRSET.CF
		jsr
VGA.Z		rts
;
; BAUD ( rate --, set the baud rate)
BAUD.LF	dc.l	VGA.NF
BAUD.NF	dc.b	4 128 +
		dc.b	char D char U char A char B
BAUD.SF	dc.w	BAUD.Z BAUD.CF del
BAUD.CF	#.l	3125000		( baud clock/16)
		swap				( clock/16 baud)
		1+
		divu
		nip				( ubrr)
		#.w	RS232baud
		store.w
BAUD.Z		rts
; ---------------------------------------------------------------------------------------------
; Forth global variables
;
PAD.LF		dc.l	BAUD.NF
PAD.NF		dc.b	3 128 +
		dc.b	char D char A char P
PAD.SF		dc.w	PAD.Z PAD.CF del			; because the PFA is extrated from the return address
PAD.CF		#.l	_PAD
PAD.Z		rts	
;
HERE.LF	dc.l	PAD.NF
HERE.NF	dc.b	4 128 +
		dc.b	char E char R char E char H
HERE.SF	dc.w	HERE.Z HERE.CF del
HERE.CF	#.w	HERE_
		fetch.l
HERE.Z		rts
HERE_		dc.l	END
;	
; HERE for the SDRAM space
HERE1.LF	dc.l	HERE.NF
HERE1.NF	dc.b	5 128 +
		dc.b	char 1 char E char R char E char H
HERE1.SF	dc.w	HERE1.Z HERE1.CF del
HERE1.CF	#.w	HERE1
		fetch.l
HERE1.Z	rts
HERE1		dc.l	_END
;
; maximum word length for inline compilation 
; do not reduce below 9 since some control structures
; *must* be compiled inline e.g. LOOP1.RUN
INLINESIZE.LF	dc.l	HERE1.NF
INLINESIZE.NF	dc.b	10 128 +
		dc.b	char E char Z char I char S char E char N char I char L char N char I
INLINESIZE.SF	dc.w	INLINESIZE.Z INLINESIZE.CF del
INLINESIZE.CF	#.w	INLINESIZE
INLINESIZE.Z	rts
INLINESIZE	dc.l	10					 
;
BASE.LF	dc.l	INLINESIZE.NF
BASE.NF	dc.b	4 128 +
		dc.b	char E char S char A char B
BASE.SF	dc.w	BASE.Z BASE.CF del
BASE.CF	#.w	BASE_
BASE.Z		rts
BASE_		dc.l 	10
;
STATE.LF	dc.l	BASE.NF
STATE.NF	dc.b	5 128 +
		dc.b	char E char T char A char T char S
STATE.SF	dc.w	STATE.Z STATE.CF del
STATE.CF	#.w	STATE_
STATE.Z	rts
STATE_		dc.l	0
;
>IN.LF		dc.l	STATE.NF
>IN.NF		dc.b	3 128 +
		dc.b	char N char I 62
>IN.SF		dc.w	>IN.Z >IN.CF del
>IN.CF		#.w	>IN_
>IN.Z		rts
>IN_		dc.l	0
;
PALETTE.LF	dc.l	>IN.NF
PALETTE.NF	dc.b	7 128 +
		dc.b	char E char T char T char E char L char A char P
PALETTE.SF	dc.w	PALETTE.Z PALETTE.CF del
PALETTE.CF	#.w	PALETTE
PALETTE.Z	rts
PALETTE	dc.b	55			; input (yellow)
		dc.b	58			; output (green)
		dc.b	7			; error (red)
		dc.b	240			; dos (blue)
		dc.b	255			; cursor (white)
		ds.b	11			; 11 more user colours in the palette
; 
INK.LF	dc.l	PALETTE.NF
INK.NF	dc.b	3 128 +
		dc.b	char K char N char I
INK.SF	dc.w	INK.Z INK.CF del
INK.CF	#.w	INK
INK.Z		rts
INK		dc.b	56			; note this variable is BYTE length!
		ds.b	3			; padding
;
CSR-X.LF	dc.l	INK.NF
CSR-X.NF	dc.b	5 128 +
		dc.b	char X char - char R char S char C
CSR-X.SF	dc.w	CSR-X.Z CSR-X.CF del
CSR-X.CF	#.w	CSR-X
CSR-X.Z	rts
CSR-X		dc.l	0
;
CSR-Y.LF	dc.l	CSR-X.NF
CSR-Y.NF	dc.b	5 128 +
		dc.b	char Y char - char R char S char C
CSR-Y.SF	dc.w	CSR-Y.Z CSR-Y.CF del
CSR-Y.CF	#.w	CSR-Y
CSR-Y.Z	rts
CSR-Y		dc.l	0
;
TAB.LF		dc.l	CSR-Y.NF
TAB.NF		dc.b	3 128 +
		dc.b	char B char A char T
TAB.SF		dc.w	TAB.Z TAB.CF del
TAB.CF		#.w	TAB
TAB.Z		rts
TAB		dc.l	3
;
ROWS.LF	dc.l	TAB.NF
ROWS.NF	dc.b	4 128 +
		dc.b	char S char W char O char R
ROWS.SF	dc.w	ROWS.Z ROWS.CF del
ROWS.CF	#.w	ROWS
ROWS.Z		rts
ROWS		dc.b	60
;
COLS.LF	dc.l	ROWS.NF
COLS.NF	dc.b	4 128 +
		dc.b	char S char L char O char C
COLS.SF	dc.w	COLS.Z COLS.CF del
COLS.CF	#.w	COLS
COLS.Z		rts
COLS		dc.b	100
;
FAT.SecPerClus.LF		dc.l	COLS.NF
FAT.SecPerClus.NF		dc.b	14 128 +
				dc.s	FAT.SecPerClus
FAT.SecPerClus.SF		dc.w	FAT.SecPerClus.Z FAT.SecPerClus.CF del
FAT.SecPerClus.CF		#.w	FAT.SecPerClus
FAT.SecPerClus.Z		rts
FAT.SecPerClus		dc.l	0	; sectors per cluster
;
FAT.TotalSectors.LF		dc.l	FAT.SecPerClus.NF
FAT.TotalSectors.NF		dc.b	16 128 +
				dc.s 	FAT.TotalSectors
FAT.TotalSectors.SF		dc.w	FAT.TotalSectors.Z FAT.TotalSectors.CF del
FAT.TotalSectors.CF		#.w	FAT.TotalSectors
FAT.TotalSectors.Z		rts
FAT.TotalSectors		dc.l 	0	; total sectors on the disk
;
FAT.NextFreeCluster.LF	dc.l	FAT.TotalSectors.NF
FAT.NextFreeCluster.NF	dc.b	19 128 +
				dc.s	FAT.NextFreeCluster
FAT.NextFreeCluster.SF	dc.w	FAT.NextFreeCluster.Z FAT.NextFreeCluster.CF del
FAT.NextFreeCluster.CF	#.w 	FAT.NextFreeCluster
FAT.NextFreeCluster.Z	rts
FAT.NextFreeCluster		dc.l 	0	; where to look for the next free cluster
;
FAT.CurrentDirectory.LF	dc.l	FAT.NextFreeCluster.NF
FAT.CurrentDirectory.NF	dc.b	20 128 +	
				dc.s	FAT.CurrentDirectory
FAT.CurrentDirectory.SF	dc.w	FAT.CurrentDirectory.Z FAT.CurrentDirectory.CF del
FAT.CurrentDirectory.CF	#.w	FAT.CurrentDirectory
FAT.CurrentDirectory.Z	rts
FAT.CurrentDirectory		dc.l 	0	; cluster number of current directory
;
FAT.RootClus.LF		dc.l	FAT.CurrentDirectory.NF
FAT.RootClus.NF		dc.b	12 128 +
				dc.s	FAT.RootClus
FAT.RootClus.SF		dc.w	FAT.RootClus.Z FAT.RootClus.CF del
FAT.RootClus.CF		#.w	FAT.RootClus
FAT.RootClus.Z		rts
FAT.RootClus			dc.l 	0	; first cluster of root directory
;
FAT.buf.LF			dc.l	FAT.RootClus.NF
FAT.buf.NF			dc.b	7 128 +
				dc.s	FAT.buf
FAT.buf.SF			dc.w	FAT.buf.Z FAT.buf.CF del
FAT.buf.CF			#.l	_FAT.buf
FAT.buf.Z			rts
;
; LAST returns the address of a variable pointing to the last name field in the dictionary
LAST.LF	dc.l	FAT.buf.NF
LAST.NF	dc.b	4 128 +
		dc.b	char T char S char A char L
LAST.SF	dc.w	LAST.Z LAST.CF del
LAST.CF	#.w	LAST-NF
LAST.Z		rts
LAST-NF	dc.l 	LAST.NF			; NF of last word created by HEAD, must be initialized
; ---------------------------------------------------------------------------------------------
; Forth internal variables	
;
IN_LEN			dc.l	0		; number of characters in input buffer (set by QUIT's inner loop)
IN_LEN_a		dc.l	0		; used by SAVE-INPUT and RESTORE-INPUT
>IN_a			dc.l	0		; used by SAVE-INPUT and RESTORE-INPUT
HLD_			dc.l 	0		; pointer for number output words (HOLD, etc.)
STRINGP		dc.l	_STRING	; pointer within the string buffer
LAST-CF		dc.l	0		; CF of last word created by HEAD
LAST-SF		dc.l	0		; SF of last word created by HEAD
input_buff		dc.l 	_input_buff	; input buffer location (returned by SOURCE)
input_buff_a		dc.l	0		; used by SAVE-INPUT and RESTORE-INPUT
input_size		dc.l	_input_size	; length of input buffer (returned by SOURCE)
input_size_a		dc.l	0		; used by SAVE-INPUT and RESTORE-INPUT
COMPILEstackP		dc.l	_PAD		; pointer for the compiler stack
TYPE_VECTOR		dc.l	VTYPE.CF	; VTYPE.CF
TYPERAW_VECTOR 	dc.l	VTYPERAW.CF	; VTYPERAW.CF
EMIT_VECTOR		dc.l	VEMIT.CF	; VEMIT.CF
KEY_VECTOR		dc.l	KKEY.CF	; KKEY.CF
KEY?_VECTOR		dc.l	KKEY?.CF	; EKEY?.CF
FAT.RsvdSecCnt	dc.l	0		; number of reserved sectors
FAT.FirstDataSector	dc.l	0		; first sector after FAT
FAT.FATinBuf		dc.l	0		; the currently buffered FAT sector
;
; marker for initializing HERE
END		dc.l	0
;