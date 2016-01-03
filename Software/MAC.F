hex
03f860 constant MACreadyRX
03f864 constant MACdataRX
03f868 constant MACchecksum_err
03f86c	constant MACreadyTX
03f870	constant MACdataTX
03f874	constant MACtransmit_req
03f878 constant SMIaddr
03f87C constant SMIdataWrite
03f880 constant SMIread_request
03f884 constant SMIready
03f888 constant SMIdataRead

decimal

: octet ( -- x)
	MACdataRX @
;

: .frame
	hex
	CR
	BEGIN
	MACreadyRX @ 0<>
	WHILE
		octet 2 .r
	REPEAT 
	\ MACchecksum_err @ if ." CRC error" THEN
	CR
	decimal
;


: capture
	>remote
	begin
		MACreadyRX @ 0<> IF .frame THEN
	key? until
	>local
;

: new-frame ( --)
	create
	does> ( pfa --)
	hex
	CR
	dup 4 + dup rot @ + swap 
	DO
		i c@ 
		dup 2 .r 
		MACdataTX !
	LOOP
	CR
	decimal
	0 MACtransmit_req !
;


new-frame frame1
42 , 						\ number of bytes
hex 
FF c, FF c, FF c, FF c, FF c, FF c,
00 c, 08 c, DC c, 1E c, 52 c, 60 c,
08 c, 06 c, 
00 c, 01 c, 08 c, 00 c, 06 c, 04 c,
00 c, 01 c, 00 c, 08 c, DC c, 1E c, 
52 c, 60 c, C0 c, A8 c, 0B c, 64 c,
00 c, 00 c, 00 c, 00 c, 00 c, 00 c,
C0 c, A8 c, 0B c, 01 c,
decimal

new-frame frame2
60 ,
hex
FF c, FF c, FF c, FF c, FF c, FF c,
00 c, 00 c, 00 c, 04 c, 14 c, 13 c,
08 c, 00 c, 
45 c, 00 c, 00 c, 2E c, 00 c, 00 c,
00 c, 00 c, 40 c, 11 c, 7A c, C0 c,
00 c, 00 c, 00 c, 00 c, FF c, FF c,
FF c, FF c, 00 c, 00 c, 50 c, DA c,
00 c, 12 c, 00 c, 00 c, 42 c, 42 c,
42 c, 42 c, 42 c, 42 c, 42 c, 42 c,
42 c, 42 c, 42 c, 42 c, 42 c, 42 c,
42 c, 42 c, 42 c, 42 c,
decimal

new-frame frame3
1 ,
hex
0 c,
decimal

new-frame frame4
48 ,
hex
ff c, ff c, ff c, ff c, ff c, ff c,
ff c, ff c, ff c, ff c, ff c, ff c,
ff c, ff c, ff c, ff c, ff c, ff c,
ff c, ff c, ff c, ff c, ff c, ff c,
ff c, ff c, ff c, ff c, ff c, ff c,
ff c, ff c, ff c, ff c, ff c, ff c,
decimal

: SMI@ ( reg -- d)
	32 +	\ PHY address 00001-----
	SMIaddr !
	SMIread_request @ drop
	BEGIN
		SMIready @ 
	UNTIL
	SMIdataRead @
;

: SMI! ( d reg --)
	32 +
	SMIaddr !
	SMIdataWrite !
;

: SMI? ( reg)
	SMI@
	binary
	16 .r
	decimal
;

: TEST 
	.frame
	24832 0 SMI! \ Ethernet PHY in near loopback mode
	frame1
	." Test result: " CR
	.frame
;

