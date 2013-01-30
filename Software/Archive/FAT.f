\ FAT file system

\ Baseline functionality needed for INCLUDE

\ FORTH variables
variable FAT.SecPerClus		\ sectors per cluster
variable FAT.TotalSectors		\ total sectors on the disk
variable FAT.NextFreeCluster	\ where to look for the next free cluster
variable FAT.CurrentDirectory	\ cluster number of current directory
variable FAT.RootClus		\ first cluster of root directory
512 buffer: FAT.buf			\ buffer for general sector access

\ internal variables
variable FAT.RsvdSecCnt		\ number of reserved sectors
variable FAT.FirstDataSector	\ first sector after FAT
variable FAT.FATinBuf		\ the currently buffered FAT sector
512 buffer: FAT.fatbuf		\ buffer specifically for FAT

: FAT.read-long ( addr n -- x, get a little endian longword from the buffer)
	+ dup 4 + swap DO i c@ LOOP
	3 0 DO 256 * + LOOP
;

\ EX
: FAT.write-long ( x addr n --, write a little endian longword x to the buffer at position n)
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

\ EX
: MOUNT ( --, initiaize the SD card and FAT data structures)
	sd.init
	fat.buf 0 sd.read-sector
	fat.buf 510 fat.read-word 43605 <> IF		\ confirm sector signature 0xAA55
		2000 error
	THEN
	fat.buf 82 fat.read-word 16710 <> IF		\ confirm FAT32 signature 0x4146
		2001 error
	THEN
	fat.buf 13 + c@ fat.secperclus !
	fat.buf 44 fat.read-long dup fat.rootclus ! FAT.CurrentDirectory !
	fat.buf 32 fat.read-long fat.TotalSectors !
	fat.buf 14 fat.read-word dup fat.rsvdseccnt !	( RsvdSecCnt)
	fat.buf 16 + c@					( RsvdSecCnt NumFATs)
	fat.buf 36 fat.read-long				( RsvdSecCnt NumFATs SecPerFAT)
	* + fat.firstdatasector !
	fat.buf 1 sd.read-sector				\ FAT32 FSInfo
	fat.buf 0 fat.read-long 1096897106 <> IF
		2002 error					\ confirm valid FSInfo sector
	THEN
	fat.buf 492 fat.read-long dup -1 = IF drop 2 THEN
	FAT.NextFreeCluster !
	0 FAT.FATinBuf !					\ FAT buffer overwritten
;

\ EX
: FAT.UpdateFSInfo ( --, update the FAT32 FSInfo sector with next free cluster)
	fat.buf 1 sd.read-sector
	FAT.NextFreeCluster @ fat.buf 492 FAT.write-long
	fat.buf 1 sd.write-sector
	0 FAT.FATinBuf !					\ FAT buffer overwritten
;

\ EX
: FAT.clus2sec ( n -- n, given a valid cluster number return the number of the first sector in that cluster)
	2 -				\ first cluster is number 2
	fat.secperclus @ *
	fat.firstdatasector @ +
;

: FAT.prep-fat ( n -- ThisFATEntOffset, calulate location and load the appropriate FAT sector into fat.fatbuf)
	4 *			( FATOffset)
	512 /MOD		( rem quo)
	fat.rsvdseccnt @ + 	( ThisFATEntOffset ThisFATSecNum)
	dup FAT.FATinBuf @ <> IF
		dup FAT.FATinBuf !			\ remember the buffered sector
		fat.fatbuf swap	 ( ThisFATEntOffset fat.fatbuf ThisFATSecNum)
		SD.read-sector	 ( ThisFATEntOffset)
	ELSE
		drop			( ThisFATEntOffset)
	THEN
;

\ EX
: FAT.get-fat ( n -- x, return the FAT entry for a given cluster)
	FAT.prep-fat
	fat.fatbuf swap		( fat.buf ThisFATEntOffset)
	fat.read-long
	268435455 and  \ 0x0FFFFFFF
;

\ EX
: FAT.put-fat ( value cluster --, place value in the FAT location for cluster)
	FAT.prep-fat			( value ThisFATEntOffset)
	fat.fatbuf swap		( value fat.buf ThisFATEntOffset)
	fat.write-long
	FAT.fatbuf FAT.FATinBuf @ SD.write-sector
;

\ EX
: FAT.string2filename ( addr n -- addr, convert an ordinary string to a short FAT filename)
		>R >R
		PAD dup dup dup					\ was FAT.filestring 
		12 + swap DO 32 i c! LOOP	 			\ fill the output string with blanks
		R> R>					( filestring filestring addr n)
	?dup IF		
		12 min over + swap 			( filestring filestring addrE addr)					
		DO							\ loop over the input string upto 12 characters
			i c@ dup 46 = 				\ . 
			IF
				drop drop dup 8 +			\ re-position in output string
			ELSE
				upper over c! 1+			\ save and increment position in output string
			THEN				
		LOOP
		drop
	ELSE								\ zero length interpret as ".."
		drop 46 over c! 1+
		46 swap c!
	THEN
;

: FAT.find-file-local ( dirCluster addr n -- dirSector dirOffset firstCluster size flags TRUE | FALSE, find in local folder)
	FAT.String2Filename	( cluster filestring)
	swap dup >R		( filestring cluster R:cluster)
	BEGIN
		FAT.Clus2Sec				( filestring firstSec R:cluster)
		dup FAT.SecPerClus @ + swap		( filestring lastSec firstSec R:cluster)	
		DO					( filestring R:LOOP cluster)	\ examine each sector in the cluster
			FAT.buf i SD.read-sector
			FAT.buf dup 512 + swap DO	( filestring R:LOOP LOOP cluster)	\ examine each 32 byte entry in the sector
				i c@ dup 0= IF UNLOOP UNLOOP nip R> drop EXIT THEN	\ empty entry and no following entries - exit false flag
				229 <> IF							\ non-0xE5 first byte indicates valid entry
					i 11 + c@ 15 and 15 <> IF				\ is not a long-name entry
						dup 11 i 11 $= IF				\ test string match
							drop					\ remove filestring	
							j							\ dirSector
							i FAT.buf -						\ directory offset 
							i 20 FAT.read-word 65536 * i 26 FAT.read-word + 	\ startCluster
							i 28 FAT.read-long 					\ size		
							i 11 + c@						\ flags
							UNLOOP UNLOOP R> drop -1 EXIT			\ exit with true flag
						THEN
					THEN
				THEN
			32 +LOOP
		LOOP 
		R>					( filestring currentCluster)
		FAT.get-fat				( filestring nextCluster)
		dup 268435455 =			( filestring nextCluster flag) 	\ End-of-clusters mark
	UNTIL
	drop drop 0										\ likely bad directory
;

\ EX	
: FAT.find-file ( addr n -- dirSector dirOffset firstCluster size flags TRUE | FALSE, find from current directory)
	FAT.CurrentDirectory @ rot rot
	over + 1- dup >R over 					( cluster startAddr endAddr startAddr R:endAddr-1)
	?DO								( cluster startAddr R: LOOP endAddr-1)
		i c@ dup 92 = swap 47 = or IF 
			i over -					( cluster Addr n)
			FAT.find-file-local IF 
				dup 15 and 15 <> swap 16 = and IF			\ is a directory
					drop nip nip 
					?dup 0= IF FAT.RootClus @ THEN		\ root directory adjustment
					i 1+				( newCluster newAddr)
				ELSE
					UNLOOP R> drop drop drop drop drop 0 EXIT	\ cannot parse filepath - not a directory
				THEN
			ELSE
				UNLOOP R> drop 0 EXIT				\ cannot parse filepath - not found
			THEN
		THEN
	LOOP								( cluster Addr R:endAddr-1)
	dup c@ dup 92 = swap 47 = or IF 
		R> drop 0						( cluster addr 0)	\ n=0 interpreted as ".."
	ELSE
		R> 1+ over - 						( cluster addr n)
	THEN
	FAT.find-file-local 								
;

\ EX
: FAT.load-file ( addr firstCluster --, load a file to addr given the first cluster, cluster by cluster)
	BEGIN						( addr currentCluster)
		dup >R					( addr currentCluster R:currentCluster)
		FAT.Clus2Sec				( addr firstSec R:currentCluster)
		dup FAT.SecPerClus @ + swap		( addr lastSec firstSec R:currentCluster)
		DO
			dup i SD.read-sector		
			512 +				( addr)
		LOOP
		R>					( addr currentCluster)
		FAT.get-fat				( addr nextCluster)
		dup 268435455 =			( addr nextCluster flag) 	\ End-of-clusters mark
	UNTIL
	drop drop
;

\ EX
: include ( "FILEPATH" --)
	32 WORD count FAT.find-file 			( dirSector dirOffset firstCluster size flags TRUE | FALSE)
	IF
		drop >R nip nip 16744448 dup rot		( addr addr firstCluster R:size)			\ addr is 32K below top of memory
		FAT.load-file 				( addr R:size)
		R> evaluate 					( )
	ELSE
		4 ERROR
	THEN
;	

\ --------------------------------------------------------------------------------------------------
