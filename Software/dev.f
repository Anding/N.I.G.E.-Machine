\ development of the FAT file system

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
variable FAT.CurrentDirectory	\ cluster number of current directory
variable FAT.NextFreeCluster	\ where to look for the next free cluster
variable FAT.TotalSectors		\ total sectors on the disk
variable FAT.FATinBuf		\ the currently buffered FAT sector
512 buffer: FAT.buf			\ buffer for general sector access
512 buffer: FAT.fatbuf		\ buffer specifically for FAT

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
	0 FAT.FATinBuf !
;

: FAT.UpdateFSInfo ( --, update the FAT32 FSInfo sector with next free cluster)
	fat.buf 1 sd.read-sector
	fat.buf 488 FAT.NextFreeCluster @ FAT.write-long
	fat.buf 1 sd.write-sector
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
	dup FAT.FATinBuf @ <> IF
		dup FAT.FATinBuf !			\ remember the buffered sector
		fat.fatbuf swap	 ( ThisFATEntOffset fat.fatbuf ThisFATSecNum)
		SD.read-sector	 ( ThisFATEntOffset)
	ELSE
		drop			( ThisFATEntOffset)
	THEN
	fat.fatbuf swap		( fat.buf ThisFATEntOffset)
	fat.read-long
	268435455 and  \ 0x0FFFFFFF
;

: FAT.FindFreeCluster ( -- n, return the first free cluster on the disk)
	FAT.TotalSectors @ Fat.SecPerClus @ / >R	\ total clusters
	FAT.NextFreeCluster @
	BEGIN
		dup R@ > IF drop 2 THEN		\ if over last cluster then return to 0
		dup FAT.get-fat 0<>
	WHILE
		1+
	REPEAT
	R> drop
;

: FAT.copynonblank ( out-addr in-addr -- out-addr+1, copy a non-space character from in-out)
		c@ dup 32 = IF
			drop			\ skip spaces
		ELSE
			over c!		\ copy non-spaces
			1+			\ increment return string address
		THEN
;

24 buffer: FAT.filestring			\ allow space for overwrite to simplify name conversions

: FAT.Filename2String ( addr -- addr n, convert a short FAT filename to an ordinary string)
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

: FAT.String2Filename ( addr n -- addr, convert an ordinary string to a short FAT filename)
	>R >R
	FAT.filestring dup dup dup
	12 + swap DO 32 i c! LOOP	 		\ fill the output string with blanks
	R> R>
	12 min over + swap 							
	DO						\ loop over the input string upto 12 characters
		i c@ dup 46 =				\ .
		IF
			drop drop dup 8 +		\ re-position in output string
		ELSE
			UPPER over c! 1+		\ save and increment position in output string
		THEN				
	LOOP
	drop
;

: DIR ( n --, print the directory at the cluster n)
	FAT.CurrentDirectory @
	FAT.Clus2Sec		( firstSec)
	dup FAT.SecPerClus @ + swap		( lastSec firstSec)			\ ignore any clusters after the first [128 entries per directory]
	cr DO										\ examine each sector in the cluster
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

: FAT.find-file ( cluster addr n -- size cluster flags TRUE | FALSE, given a file name scan the given directory and return the size, first cluster and flags of the file, or false if not found)
	FAT.String2Filename	( cluster filestring)
	swap FAT.Clus2Sec	( filestring firstSec)
	dup FAT.SecPerClus @ + swap		( filestring lastSec firstSec)	\ ignore any clusters after the first [128 entries per directory]
	DO										\ examine each sector in the cluster
		FAT.buf i SD.read-sector
		FAT.buf dup 512 + swap DO						\ examine each 32 byte entry in the sector
			i c@ dup 0= IF UNLOOP UNLOOP nip EXIT THEN		\ empty entry and no following entries - exit with false flag
			dup 229 <> IF							\ non-0xE5 first byte indicates valid entry
				15 and 15 <> IF					\ is not a long-name entry
					dup 11 i 11 $= IF				\ test string match
						drop					\ remove filestring
						i 28 FAT.read-long 					\ size						
						i 20 FAT.read-word 65536 * i 26 FAT.read-word + 	\ cluster	
						i 11 + c@						\ flags
						UNLOOP UNLOOP 0 not EXIT				\ exit with true flag
					THEN
				THEN
			THEN
		32 +LOOP
	LOOP
;


: FAT.load-file ( addr firstCluster, load a file to addr given the first cluster, assuming size <> 0)
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
		dup 268435455 =			( addr nextCluster flag)  \ End-of-clusters mark
	UNTIL
	drop drop
;

: CLOSE-FILE ( fileid - ior, close the file identified by fileid and return an I/O result)
;

: CREATE-FILE ( c-addr u fam -- fileid ior, if the file already exists, re-create it as a replacement empty file)
;

: DELETE-FILE ( c-addr u -- ior)
;

: FLUSH-FILE ( fileid -- ior, force any buffered contents to disk)
;

: OPEN-FILE ( c-addr u fam -- fileid ior, open a file for sequential access)
;

: RENAME-FILE ( c-addr1 u1 c-addr2 u2 -- ior, rename file1 -> file2) 
;

: RESIZE-FILE ( ud fileid - ior)
;

: READ-FILE ( addr u1 fileid -- u2 ior, read u1 characters and store at addr, return u2 the number of characters sucessfully read)
;

: READ-LINE ( addr u1 fileid -- us flag ior)
\ if no exception occurs ior=0 and flag=true
\ if FILE-POSITION = FILE-SIZE before executing READ-LINE, ior=0 and flag=false
;

: WRITE-FILE ( addr u fileid -- ior)
\ increase FILE-SIZE if necessary
\ after this operation FILE-POSITION returns the next file position after the last character written to the file
\ and FILE-SIZE returns a value equal to or greater than FILE-POSITION
;

: WRITE-LINE ( addr u fileid -- ior, as WRITE-FILE but a line terminator is added)
;

: FILE-POSITION ( filid - uD ior)
;

: FILE-SIZE ( fileid - uD ior)
;
\ access methods R/O, R/W, W/O

: REPOSITION-FILE ( ud fileid - ior)
;