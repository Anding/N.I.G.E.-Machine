\ development of the FAT file system

: FAT.write-sector ( addr n --, write 512 byte to sector n from addr)
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

: FAT.read-long ( addr n -- x, get a little endian longword from the buffer)
	+ dup 4 + swap DO i c@ LOOP
	3 0 DO 256 * + LOOP
;

: FAT.write-long ( x addr n --, write a little endian longword to the buffer)
	+ >R >R	( R: x addr+n)
	R@ 24 rshift 255 and	
	R@ 16 rshift 255 and	
	R@ 8 rshift 255 and	
	R> 255 and
	R> dup 4 + swap
	DO i c! LOOP
;
	
: FAT.read-word ( addr n -- x, get a little endian word from the buffer)
	1 + +
	dup c@ 256 * swap
	1- c@ +
;

: FAT.write-word ( x addr n --, write a litte endian word to the buffer)
	+ >R >R	( R : x addr+n)
	R@ 8 rshift 255 and	
	R> 255 and
	R@ c!
	R> 1+ c!
;

variable FAT.SecPerClus		\ sectors per cluster
variable FAT.RsvdSecCnt		\ number of reserved sectors
variable FAT.RootClus		\ first cluster of root directory
variable FAT.FirstDataSector	
512 buffer: FAT.buf			\ buffer for sector access

: FAT.init ( --, initiaize the FAT data structures)
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
	fat.buf 16 + c@					( RsvdSecCnt NumFATs)
	fat.buf 36 fat.read-long				( RsvdSecCnt NumFATs SecPerFAT)
	* + fat.firstdatasector !
;
	
: FAT.Clus2Sec ( n -- n, given a valid cluster number return the number of the first sector in that cluster)
	2 -				\ first cluster is number 2
	fat.secperclus @ *
	fat.firstdatasector @ +
;

: FAT.get-fat ( n -- x, return the FAT entry for a given cluster)
	4 *			( FATOffset)
	512 /MOD		( rem quo)
	fat.rsvdseccnt @ + 	( ThisFATEntOffset ThisFATSecNum)
	fat.buf swap	 	( ThisFATEntOffset fat.buf ThisFATSecNum)
	SD.read-sector	( ThisFATEntOffset)
	fat.buf swap		( fat.buf ThisFATEntOffset)
	fat.read-long
	268435455 and  \ 0x0FFFFFFF
;

: FAT.copynonblank ( out-addr in-addr -- out-addr+1, copy a non-space character from in-out)
		c@ dup 32 = IF
			drop			\ skip spaces
		ELSE
			over c!		\ copy non-spaces
			1+			\ increment return string address
		THEN
;

11 buffer: FAT.filestring

: FAT.Filename2String ( addr -- addr n, convert a FAT filename to an ordinary string)
	FAT.filestring over 				\ remember address and use FAT.filestring for the return string address
	dup 8 + swap DO
		i FAT.copynonblank
	LOOP
	swap 8 + ( out-addr in-addr+8)
	dup c@	32 <> IF
		swap 46 over c! 1+ swap		\ dot for file extension
		dup 3 + swap DO
			i FAT.copynonblank
		LOOP
	ELSE
		drop	
	THEN
	FAT.filestring dup rot swap -	( addr n)
;

: FAT.DIR ( n --, print the directory at the cluster n)
	FAT.Clus2Sec		( firstRootSec)
	dup FAT.SecPerClus @ + swap		( lastRootSec firstRootSec)		\ ignore any clusters after the first
	cr DO										\ examine each sector in the first root cluster
		FAT.buf i SD.read-sector
		FAT.buf dup 512 + swap DO						\ examine each 32 byte entry in the sector
			i c@ ?dup 0= IF UNLOOP UNLOOP EXIT	THEN			\ empty entry and no following entries
			229 <> IF							\ non-0xE5 first byte indicates valid entry
				i 11 + c@ 
				dup 15 and 15 <> IF					\ is not a long-name entry
					dup 8 and IF					\ is a system volume name
						58 emit i 11 type cr
					ELSE
						dup 16 and IF				\ is a directory ELSE is a short-name entry
							92 emit
						THEN					
							i FAT.Filename2String type	
							3 0 do 9 emit loop
							i 28 FAT.read-long u. 	\ file size
							2 0 do 9 emit loop
							i 20 FAT.read-word 65536 *	\ first cluster hi
							i 26 FAT.read-word + u. cr	\ first cluster lo
					THEN
				THEN
				drop
			THEN
		32 +LOOP
	LOOP	
;

: DIR ( --, print the root directory contents)
	fat.init
	FAT.ROOTClus @ 
	FAT.DIR								
;

: FAT.seek-file ( addr n -- size cluster | 0, given a file name return the size and first cluster of the file, or zero if not found)
;

: FAT.load-file ( addr size cluster, load a file to addr given the file size and first cluster)
;
