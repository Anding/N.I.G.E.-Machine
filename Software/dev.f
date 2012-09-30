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

: sd.sector-code ( n -- b4 b3 b2 b1, scale and split sector address)
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

: sd.busy-check ( --, wait for SD card after asserting CS)
	spi.cs-hi
	BEGIN				
		spi.get 
		0 <>				\ if CS is asserted while busy card will set D0 low
	UNTIL
	spi.cs-lo
;

: sd.read-sector ( addr n --, read 512 bytes from sector n into a buffer at addr)
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

: fat.write-sector ( addr n --, write 512 byte to sector n from addr)
	1000 timeout
	sd.busy-check
	1 swap					\ checksum
	sd.sector-code			\ encode sector number
	88 sd.cmd				\ CMD24
	sd.get-R1 0 <> IF			\ check response OK
		1010 ERROR
	THEN
	255 spi.put				\ space
	254 spi.put				\ initiate data packet
	dup 512 + swap DO I c@ spi.put LOOP	\ write sector
	2 0 DO 1 spi.put LOOP			\ dummy checksum
	sd.get-R1 31 and 5 <> IF			\ check data response
		1011 ERROR				\ write error
	THEN	
	0 timeout
;

: fat.read-long ( addr n -- x, get a little endian longword from the buffer)
	+ dup 4 + swap DO i c@ LOOP
	3 0 DO 256 * + LOOP
;

: fat.write-long ( x addr n --, write a little endian longword to the buffer)
	+ >R >R	( R: x addr+n)
	R@ 24 rshift 255 and	
	R@ 16 rshift 255 and	
	R@ 8 rshift 255 and	
	R> 255 and
	R> dup 4 + swap
	DO i c! LOOP
;
	
: fat.read-word ( addr n -- x, get a little endian word from the buffer)
	1 + +
	dup c@ 256 * swap
	1- c@ +
;

: fat.write-word ( x addr n --, write a litte endian word to the buffer)
	+ >R >R	( R : x addr+n)
	R@ 8 rshift 255 and	
	R> 255 and
	R@ c!
	R> 1+ c!
;

variable fat.SecPerClus		\ sectors per cluster
variable fat.RsvdSecCnt		\ number of reserved sectors
variable fat.RootClus		\ first cluster of root directory
variable fat.FirstDataSector	
512 buffer: fat.buf			\ buffer for sector access

: fat.init ( --, initiaize the FAT data structures)
	sd.init
	fat.buf 0 sd.read-sector
	fat.buf 510 fat.read-word hex aa55 decimal <> IF		\ confirm sector signature
		2000 error
	THEN
	fat.buf 82 fat.read-word hex 4146 decimal <> IF		\ confirm FAT32 signature
		2001 error
	THEN
	fat.buf 13 + c@ fat.secperclus !
	fat.buf 44 fat.read-long fat.rootclus !
	fat.buf 14 fat.read-word dup fat.rsvdseccnt !	( RsvdSecCnt)
	fat.buf 16 c@						( RsvdSecCnt NumFATs)
	fat.buf 36 fat.read-long				( RsvdSecCnt NumFATs SecPerFAT)
	* + fat.firstdatasector !
;
	
: fat.Clus2Sec ( n -- n, given a valid cluster number return the number of the first sector in that cluster)
	2 -				\ first cluster is number 2
	fat.secperclus @ *
	fat.firstdatasector @ +
;

: fat.get-fat ( n -- x, return the FAT entry for a given cluster)
	4 *			( FATOffset)
	dup 512 /MOD		( rem quo)
	rsvdseccnt @ + 	( ThisFATEntOffset ThisFATSecNum)
	fat.buf swap	 	( ThisFATEntOffset fat.buf ThisFATSecNum)
	SD.read-sector	( ThisFATEntOffset)
	fat.buf swap		( fat.buf ThisFATEntOffset)
	SD.read-long
	hex 0FFFFFFF decimal and
;

: fat.dir ( --, print the root directory contents)
;

: fat.seek-file ( addr n -- size cluster | 0, given a file name return the size and first cluster of the file, or zero if not found)
;

: fat.load-file ( addr size cluster, load a file to addr given the file size and first cluster)
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
	