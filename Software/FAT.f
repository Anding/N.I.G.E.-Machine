\ development of the FAT file system

: FAT.read-long ( addr n -- x, get a little endian longword from the buffer)
	+ dup 4 + swap DO i c@ LOOP
	3 0 DO 256 * + LOOP
;

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
24 buffer: FAT.filestring		\ allow space for overwrite to simplify name conversions
24 BUFFER: FILE.LIST

FILE.LIST LIST.INIT			\ do not include with MOUNT so that MOUNT can be repeated safely

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
	0 FAT.FATinBuf !
;

: FAT.UpdateFSInfo ( --, update the FAT32 FSInfo sector with next free cluster)
	fat.buf 1 sd.read-sector
	FAT.NextFreeCluster @ fat.buf 492 FAT.write-long
	fat.buf 1 sd.write-sector
;
	
: FAT.Clus2Sec ( n -- n, given a valid cluster number return the number of the first sector in that cluster)
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

: FAT.get-fat ( n -- x, return the FAT entry for a given cluster)
	FAT.prep-fat
	fat.fatbuf swap		( fat.buf ThisFATEntOffset)
	fat.read-long
	268435455 and  \ 0x0FFFFFFF
;

: FAT.put-fat ( m n --, place value m in the FAT location for cluster n)
	FAT.prep-fat			( m ThisFATEntOffset)
	fat.fatbuf swap		( m fat.buf ThisFATEntOffset)
	fat.write-long
	FAT.fatbuf FAT.FATinBuf @ SD.write-sector
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
	dup 1+ FAT.NextFreeCluster !		\ move to next cluster
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

: FAT.find-file-local ( dirCluster addr n -- dirSector dirOffset firstCluster size flags TRUE | FALSE)
	FAT.String2Filename	( cluster filestring)
	swap dup >R
	BEGIN
		FAT.Clus2Sec				( filestring firstSec)
		dup FAT.SecPerClus @ + swap		( filestring lastSec firstSec)	
		DO					( filestring)				\ examine each sector in the cluster
			FAT.buf i SD.read-sector
			FAT.buf dup 512 + swap DO						\ examine each 32 byte entry in the sector
				i c@ dup 0= IF UNLOOP UNLOOP R> drop nip EXIT THEN	\ empty entry and no following entries - exit false flag
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
				ELSE
					drop
				THEN
			32 +LOOP
		LOOP 
		R>					( filestring currentCluster)
		FAT.get-fat				( filestring nextCluster)
		dup 268435455 =			( filestring nextCluster flag) 	\ End-of-clusters mark
	UNTIL
	drop drop 0										\ likely bad directory
;
	
: FAT.find-file ( dirStartCluster addr n -- dirSector dirOffset firstCluster size flags TRUE | FALSE)
	over + dup >R over 						( cluster startAddr endAddr startAddr R:endAddr)
	DO								( cluster startAddr R:endAddr)
		i c@ 92 = IF 
			i over -					( cluster Addr n)
			FAT.find-file-local IF 
				dup 15 and 15 <> swap 16 = and IF			\ is a directory
					drop nip nip i 1+		( newCluster newAddr)
				ELSE
					UNLOOP R> drop drop drop drop drop 0 EXIT	\ cannot parse filepath - not a directory
				THEN
			ELSE
				UNLOOP R> drop 0 EXIT				\ cannot parse filepath - not found
			THEN
		THEN
	LOOP								( cluster Addr R:endAddr)
	R> over - 							( cluster addr n)
	FAT.find-file-local								\ clean stack if failed
;

: DIR ( n --, print the directory at the cluster n)
	FAT.CurrentDirectory @ dup >R
	BEGIN
		FAT.Clus2Sec				( firstSec)
		dup FAT.SecPerClus @ + swap		( lastSec firstSec)			
		cr DO										\ examine each sector in the cluster
			FAT.buf i SD.read-sector
			FAT.buf dup 512 + swap DO						\ examine each 32 byte entry in the sector
				i c@ ?dup 0= IF UNLOOP UNLOOP R> drop EXIT THEN		\ empty entry and no following entries
				229 <> IF							\ non-0xE5 first byte indicates valid entry
					i 11 + c@ 
					dup 15 and 15 <> IF					\ is not a long-name entry
						2 u.r 9 emit					\ flags
						i 28 FAT.read-long 6 u.r 			\ file size
						9 emit
						i 20 FAT.read-word 65536 *			\ first cluster hi
						i 26 FAT.read-word + 6 u.r  		\ first cluster lo
						9 emit
						i FAT.Filename2String type	
						cr
					ELSE
						drop
					THEN
				THEN
			32 +LOOP
		LOOP	
		R>					( currentCluster)
		FAT.get-fat				( nextCluster)
		dup 268435455 =			( nextCluster flag) 			\ End-of-clusters mark
	UNTIL
	drop
;

: CD ( "FILEPATH" --, set the current diretory)
	FAT.CurrentDirectory @
	32 word count					( dirCurrentCluster addr n --)
	FAT.find-file                      	( dirSector dirOffset firstCluster size flags TRUE | FALSE)
	IF 						( dirSector dirOffset firstCluster size flags)
		dup 15 and 15 <> swap 16 = and IF						\ is a directory
			drop nip nip			( cluster)
			FAT.CurrentDirectory !
		ELSE
			3001 error
		THEN
	ELSE
		3000 error
	THEN
;

depth .

: FAT.load-file ( addr firstCluster, load a file to addr given the first cluster, assuming size <> 0, cluster by cluster)
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
depth .
: FAT.save-file ( addr size firstCluster , save a file to disk assuming size <> 0)
	rot rot over + >R swap >R			( startAddr R:endAddr firstCluster)
	BEGIN
		R@ FAT.Clus2Sec			( Addr Sector R:endAddr Cluster)
		dup 8 + swap DO							\ write a complete sector
			dup i SD.write-sector
			512 +
		LOOP					( Addr R: endAddr Cluster)
		R> over R@ swap			( Addr cluster endAddr Addr	R:endAddr)
	> WHILE					( Addr cluster R:endAddr)
		FAT.FindFreeCluster dup >R		( Addr cluster nextCluster R:endAddr nextCluster)
		swap FAT.put-fat			( Addr R:endAddr nextCluster)
	REPEAT						
	268435455 swap FAT.put-fat							\ last cluster marker
	drop R> drop
	FAT.UpdateFSInfo								\ save latest free cluster
;
depth .
\ STRUCTURE needed for ANSI FORTH file words (40 bytes)
\ 00	&nextfile
\ 04	&priorfile
\ 08	Mode ( 1 = R, 2 = W, 4 = modified)
\ 12	Size of file
\ 16	FirstCluster
\ 20	DirectoryOffset
\ 24	DirectorySector
\ 28	File-Position
\ 32	Size of allocated space
\ 36	pointer to buffer

: FILE-SIZE ( fileid - uD ior)
	12 + @ 0
;
depth .
: FILE-POSITION ( fileid - uD ior)
	28 + @ 0
;
depth .
: REPOSITION-FILE ( ud fileid - ior)
	28 + ! 0
;
depth .
: FILE-BUFFER ( fileid - uD ior)
	36 + @ 0
;
depth .
: R/O	1 ;
: W/O 	2 ;
: R/W	3 ;
depth .
: FLUSH-FILE ( fileid -- ior, force any buffered contents to disk)
	dup 8 + @ 4 and IF								\ check modified flag
		>R R@ 36 + @ R@ 12 + @ R@ 16 + @		( addr size firstCluster R:fileid) 
		FAT.save-file								\ save the buffer
		R@ 8 + @ 251 or R@ 8 + !						\ clear modified flag
	THEN
	0
;
depth .
: CLOSE-FILE ( fileid - ior, close the file identified by fileid and return an I/O result)
	>R R@ 8 + @ 4 and IF								\ check modified flag
		R@ 36 + @ R@ 12 + @ R@ 16 + @			( addr size firstCluster R:fileid) 
		FAT.buf R@ 24 + @ sd.read-sector 			( R:fileid)	\ read the DirectorySector into the buffer
		R@ 12 + @ FAT.buf R@ 20 + @ + 28 FAT.write-long	( R:fileid)	\ update the file size
		FAT.buf R@ 24 + @ sd.write-sector 			( R:fileid)	\ write the modified dir sector to disk
	THEN	
	R@ LIST.rem									\ unlist the fileid	
	R@ 36 + @ free drop								\ free buffer							
	R> free 							( ior)		\ free header
;
depth .
: DELETE-FILE ( c-addr u -- ior)
	FAT.CurrentDirectory @ rot rot		( dirCurrentCluster addr n --)
	FAT.find-file IF
		drop drop drop FAT.buf +		( dirSector FAT.buf+dirOffset)
		229 swap c!
		FAT.buf swap SD.write-sector
		0
	ELSE
		-1
	THEN
;
depth .
: FAT.FindFreeEntry ( dirCluster -- dirSector dirOffset TRUE | FALSE, find the first available entry in a directory) 				
	BEGIN
		dup >R							( firstCluster R:Cluster)
		FAT.Clus2Sec						( firstSec R:Cluster)
		dup FAT.SecPerClus @ + swap				( lastSec firstSec R:Cluster)			
		DO										\ examine each sector in the cluster
			FAT.buf i SD.read-sector
			FAT.buf dup 512 + swap DO						\ examine each 32 byte entry in the sector
				i c@ dup 0= over 229 = or IF
					j i 0 not			( dirSector dirOffset R:Cluster)
					UNLOOP UNLOOP R> drop -1 EXIT 
				THEN
			32 +LOOP
		LOOP	
		R>							( currentCluster)
		FAT.get-fat						( nextCluster)
		dup 268435455 =					( nextCluster flag) 	\ End-of-clusters mark
	UNTIL
	drop 0								\ no more space in last cluster [ignore expanding DIR to next cluster]
;
depth .
: FAT.size2space ( size FAM -- space, decide how much space to allocate to a new file)	
	2 and IF 2* THEN								\ double for existing files with write access
	FAT.SecPerClus @ 512 * >R			( size R: clusterSize)	\ cluster size in bytes	
	R@ 1- invert and R> +			( space)			\ round-up to next whole cluster
;
depth .
: FAT.new-file ( dirSector dirOffset firstCluster size fam -- fileid ior)
	36 allocate 0= IF					
		>R 	
		over over FAT.size2space R@ 32 + !					\ buffer space
		R@ 8 + !								\ FAM
		R@ 12 + !								\ Size
		R@ 16 + !								\ First cluster 
		R@ 20 + !								\ DirectoryOffset
		R@ 24 + !								\ DirectorySector
		0 R@ 28 + !								\ File-Position	
		R@ 32 + @ allocate 0= IF 		( buffer R:fileid)	
			R@ 36 + !							\ pointer to buffer		
			R@ FILE.LIST LIST.ins					\ add to file list
			R> 0 				( fileid ior)
		ELSE
			R> free -1 			( fileidDummy ior)
		THEN
	ELSE
		drop drop drop drop -1		( fileidDummy ior)
	THEN
;
depth .
: CREATE-FILE ( c-addr u fam -- fileid ior, if the file already exists, re-create it as a replacement empty file)
	>R over over >R >R				( addr u R:fam u c-addr)
	DELETE-FILE drop				( --) 							\ delete existing file
	FAT.CurrentDirectory @										\ create new directory entry
	FAT.FindFreeEntry IF				( dirSector dirOffset R: fam u c-addr)
		dup FAT.buf + dup 32 0 fill			( dirSector dirOffset addr R: fam u c-addr)	\ zero the directory entry
		R> R> FAT.String2Filename			( dirSector dirOffset addr c-addr R: fam )
		over 11 move					( dirSector dirOffset addr R: fam) 		\ filename
		32 over 11 + c!											\ FLAGS - archive bit set
		FAT.FindFreeCluster swap			( dirSector dirOffset firstCluster addr R: fam)
		over 65535 and over 26 FAT.write-word	( dirSector dirOffset firstCluster addr R: fam)	\ DIR_FstClusLO
		over 65536 / swap 20 FAT.write-word	( dirSector dirOffset firstCluster  R: fam)	\ DIR_FstClusHI		
		rot dup FAT.buf swap SD.write-sector	( dirOffset firstCluster dirSector R: fam)	\ update dir sector on SD
		rot rot 0 R>					( dirSector dirOffset firstCluster size fam)
		FAT.new-file
	ELSE
		-1
		
	THEN
	
;

depth .
: OPEN-FILE ( c-addr u fam -- fileid ior, open a file for sequential access)
	>R
	FAT.CurrentDirectory @ rot rot		( dirCurrentCluster addr n R:FAM)
	FAT.find-file IF				( dirSector dirOffset startCluster size flags R:FAM) 
		15 and 15 <> IF			( dirSector dirOffset startCluster size R:FAM)	\ check not a long directory entry
			R> FAT.new-file 0= IF	( fileid)
				dup 36 + @ over 16 + @ FAT.load-file	( fileid)		
				0
			ELSE
				-1			( fileid ior)						\ new-file process failed
			THEN
		ELSE
			drop drop drop -1 		( fileidDummy ior)					\ was a long directory entry
		THEN
	ELSE
		-1 dup					( fileidDummy ior) 					\ file not found
	THEN	
;
depth .
: RENAME-FILE ( c-addr1 u1 c-addr2 u2 -- ior, rename file1 -> file2) 
;
depth .
: RESIZE-FILE ( ud fileid - ior)
	>R R@ 36 + @ over			( ud addr1 ud  R:fileid)
	resize					( ud addr2 ior R:fileid)
	0= IF
		R@ 36 + !						\ new buffer address
		R> 32 + !						\ new size allocation
		0				( ior)
	ELSE
		R> drop drop drop -1		( ior)
	THEN
;
depth .
: READ-FILE ( addr u1 fileid -- u2 ior, read u1 characters and store at addr, return u2 the number of characters sucessfully read)
\ check if the read will go over size
\ if not, stright copy
\ otherwise copy bytes available
\ update file-position
	>R
	R@ 16 + @ R@ 32 + @ -		\ remaining un-read characters
	min 
	R@ 36 + rot rot 
	dup >R
	move
	R> R> over over			( u2 fileid u2 fileid)
	32 + @ + swap 32 + !
	0
;
depth .
: READ-LINE ( addr u1 fileid -- us flag ior)
\ if no exception occurs ior=0 and flag=true
\ if FILE-POSITION = FILE-SIZE before executing READ-LINE, ior=0 and flag=false
\ check file position
\ copy byte by byte until LF (ignore CR)
;
depth .
: WRITE-FILE ( addr u fileid -- ior)
\ increase FILE-SIZE if necessary
\ after this operation FILE-POSITION returns the next file position after the last character written to the file
\ and FILE-SIZE returns a value equal to or greater than FILE-POSITION
;
depth .
: WRITE-LINE ( addr u fileid -- ior, as WRITE-FILE but a line terminator is added)
;
depth .
