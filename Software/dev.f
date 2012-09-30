\ development of the FAT file system

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
	fat.buf 510 fat.read-word 43605 <> IF		\ confirm sector signature 0xAA55
		2000 error
	THEN
	fat.buf 82 fat.read-word 16710 <> IF		\ confirm FAT32 signature 0x4146
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
	fat.rsvdseccnt @ + 	( ThisFATEntOffset ThisFATSecNum)
	fat.buf swap	 	( ThisFATEntOffset fat.buf ThisFATSecNum)
	SD.read-sector	( ThisFATEntOffset)
	fat.buf swap		( fat.buf ThisFATEntOffset)
	fat.read-long
	268435455 and  \ 0x0FFFFFFF
;

: fat.dir ( --, print the root directory contents)
;

: fat.seek-file ( addr n -- size cluster | 0, given a file name return the size and first cluster of the file, or zero if not found)
;

: fat.load-file ( addr size cluster, load a file to addr given the file size and first cluster)
;
