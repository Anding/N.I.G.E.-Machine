MACreadyRX		equ	hex 03f860  
MACdataRX		equ	hex 03f864  
MACchecksum_err	equ	hex 03f868  
MACreadyTX		equ	hex 03f86c
MACdataTX		equ	hex 03f870
MACtransmit_req equ	hex 03f874
SMIaddr			equ	hex 03f878
SMIdataWrite	equ	hex 03f87C
SMIread_request	equ	hex 03f880
SMIready		equ	hex 03f884
SMIdataRead		equ	hex 03f888
	nop
	nop
	nop
	nop
; fill ethernet transmit buffer
	#.l	frame1
	dup						( &len &len)
	#.b	4 
	+ 						( &len &data)
	dup 					( &len &data &data)
	rot 					( &data &data &len)
	fetch.l 				( &data &data len)
	+ 						( &data &end)
	swap 					( &end &data)
	DO
		R@ 
		fetch.b  
		#.l	MACdataTX
		store.l
	LOOP
; initiate transmit
	zero
	#.l	MACtransmit_req 
	store.l
;
	BEGIN
	AGAIN
;
; example Ethernet frame
frame1	dc.l	42			; number of bytes
	dc.b	hex	00
	dc.b	hex	FF
	dc.b	hex	FF
	dc.b	hex	FF
	dc.b	hex	FF
	dc.b	hex	FF
	dc.b	hex	00
	dc.b	hex	08
	dc.b	hex	DC
	dc.b	hex	1E
	dc.b	hex	52
	dc.b	hex	60
	dc.b	hex	08
	dc.b	hex	06 
	dc.b	hex	00
	dc.b	hex	01
	dc.b	hex	08
	dc.b	hex	00
	dc.b	hex	06
	dc.b	hex	04
	dc.b	hex	00
	dc.b	hex	01
	dc.b	hex	00
	dc.b	hex	08
	dc.b	hex	DC
	dc.b	hex	1E 
	dc.b	hex	52
	dc.b	hex	60
	dc.b	hex	C0
	dc.b	hex	A8
	dc.b	hex	0B
	dc.b	hex	64
	dc.b	hex	00
	dc.b	hex	00
	dc.b	hex	00
	dc.b	hex	00
	dc.b	hex	00
	dc.b	hex	00
	dc.b	hex	C0
	dc.b	hex	A8
	dc.b	hex	0B
	dc.b	hex	01