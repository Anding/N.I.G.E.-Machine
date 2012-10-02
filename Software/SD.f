\ hardware registers
63521 constant SPI.data
63522 constant SPI.control
63523 constant SPI.status
63524 constant SPI.divide

\ settings with no stack effect
: SPI.CS-hi SPI.control c@ 1 or SPI.control c! ;
: SPI.CS-lo SPI.control c@ 254 and SPI.control c! ;

: SPI.MOSI-hi SPI.control c@ 2 or SPI.control c! ;
: SPI.MOSI-lo SPI.control c@ 253 and SPI.control c! ;

: SPI.slow 255 SPI.divide c! ;		\ 196 kHz
: SPI.fast 8 SPI.divide c! ;		\ 6.25 MHz

\ command set

: SPI.wait ( --, wait until the SPI transfer-bus is available)
	SPI.status begin dup c@ 1 and until drop ;

: SPI.put ( n --, put a byte to the SPI port)
	SPI.wait SPI.data c! ;
	
: SPI.get ( -- n, get a byte from the SPI port)
	255 SPI.put SPI.wait SPI.data c@ ;

: SD.cmd ( chk b1 b2 b3 b4 cmd# --, SD command)
	6 0 do SPI.put loop ;

: SD.get-rsp ( -- n, get first byte of response from the sd-card)
	false BEGIN
		drop
		SPI.get
		dup 255 <>
	UNTIL
;

: SD.get-R1 ( -- n, get an R1 response from the sd-card)
	sd.get-rsp
	spi.get drop			\ one further read always required
;

variable SD.ver			\ xxxxx [block/byte] [v2/v1]

: SD.init ( --, SD card reset, version check and initialize)
	5000 timeout
	spi.slow spi.cs-hi 80 0 do 255 spi.put loop spi.cs-lo 	\ power sequence dummy clock
	BEGIN
		149 0 0 0 0 64 sd.cmd 			\ CMD0 repeated until good
		sd.get-R1 1 <>
	WHILE
		100 wait					\ 100 ms delay
	REPEAT
	135 170 1 0 0 72 sd.cmd 				\ CMD8
	sd.get-rsp 1 = IF
		\ CMD8 accepted, read data bytes
		4 0 do spi.get loop		( b4 b3 b2 b1)
		spi.get drop 				\ one further read always required
		170 = swap 			( b4 b3 f b2)
		1 = and nip nip IF		( f)
			\ 01xAA confirmed, initialize card
			BEGIN
				1 0 0 0 0 119 sd.cmd 	\ CMD55
				sd.get-R1 drop			\ CMD55 is just a header
				1 0 0 0 64 105 sd.cmd	\ CMD41hi
				sd.get-R1 0 =
			UNTIL
			1 0 0 0 0 122 sd.cmd		\ CMD58
			sd.get-rsp drop		\ ignore R1
			4 0 do spi.get loop		( b4 b3 b2 b1)
			spi.get drop 			\ one further read always required
			drop drop drop 64 and	\ test CSS bit in OCR
			IF
				3 sd.ver !		\ SD V2.0 block address
			ELSE
				1 sd.ver !		\ SD V.20 byte address
			THEN
			spi.fast			\ V2.0 supports high speed
		ELSE
			\ 01xAA mismatch
			1001 ERROR
		THEN
	ELSE
		\ CMD8 rejected, initialize card
		BEGIN
			1 0 0 0 0 119 sd.cmd 	\ CMD55
			sd.get-R1 drop		\ CMD55 is just a header
			1 0 0 0 0 105 sd.cmd		\ CMD41lo
			sd.get-R1 0 =
		UNTIL
			0 sd.ver !			\ SD V1.0
	THEN
	1 0 2 0 0 80 sd.cmd sd.get-rsp drop		\ CMD16
	0 timeout
;

: SD.sector-code ( n -- b4 b3 b2 b1, scale and split sector address)
	sd.ver @ 2 and			\ scale sector
	0 = IF
		512 *
	THEN
	>R 		
	R@ 255 and				\ bits 7 - 0
	R@ 8 rshift 255 and
	R@ 16 rshift 255 and			
	R> 24 rshift				\ bits 31 - 24
;	

: SD.busy-check ( --, wait for SD card after asserting CS)
	spi.cs-hi
	BEGIN				
		spi.get 
		0 <>				\ if CS is asserted while busy card will set D0 low
	UNTIL
	spi.cs-lo
;


: SD.read-sector ( addr n --, read 512 bytes from sector n into a buffer at addr)
	1000 timeout
	sd.busy-check
	1 swap					\ checksum
	sd.sector-code			\ encode sector number
	81 sd.cmd				\ CMD17
	sd.get-R1 0 <> IF			\ check response OK
		1005 ERROR
	THEN
	BEGIN					\ wait for data token
		spi.get
		254 =
	UNTIL
	dup 512 + swap DO spi.get I c! LOOP	\ read sector
	3 0 DO spi.get drop LOOP			\ drop CRC and read safety byte
	0 timeout
;

\ debug tools	
: dummy 80 0 do 255 spi.put loop ;
: cmd0 149 0 0 0 0 64 sd.cmd ;
: cmd1	1 0 0 0 0 65 sd.cmd ;
: cmd8 135 170 1 0 0 72 sd.cmd ;
: cmd55 1 0 0 0 0 119 sd.cmd ;
: cmd41lo 1 0 0 0 0 105 sd.cmd ;
: cmd41hi 1 0 0 0 64 105 sd.cmd ;
: cmd16 1 0 2 0 0 80 sd.cmd ;
: cmd58 1 0 0 0 0 122 sd.cmd ;	
	