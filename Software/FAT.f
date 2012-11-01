\ FAT file system

\ Baseline functionality needed for INCLUDE

variable FAT.SecPerClus		\ sectors per cluster
variable FAT.RsvdSecCnt		\ number of reserved sectors
variable FAT.RootClus		\ first cluster of root directory
variable FAT.FirstDataSector	\ first sector after FAT
variable FAT.CurrentDirectory	\ cluster number of current directory
variable FAT.NextFreeCluster	\ where to look for the next free cluster
variable FAT.TotalSectors		\ total sectors on the disk
variable FAT.FATinBuf		\ the currently buffered FAT sector
512 buffer: FAT.buf			\ buffer for general sector access
512 buffer: FAT.fatbuf		\ buffer specifically for FAT
24 buffer: FAT.filestring		\ allow space for overwrite to simplify name conversions

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

: FAT.get-fat ( n -- x, return the FAT entry for a given cluster)
	FAT.prep-fat
	fat.fatbuf swap		( fat.buf ThisFATEntOffset)
	fat.read-long
	268435455 and  \ 0x0FFFFFFF
;

: FAT.string2filename ( addr n -- addr, convert an ordinary string to a short FAT filename)
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

: FAT.find-file-local ( dirCluster addr n -- dirSector dirOffset firstCluster size flags TRUE | FALSE, find in local folder)
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
	
: FAT.find-file ( addr n -- dirSector dirOffset firstCluster size flags TRUE | FALSE, find from current directory)
	FAT.CurrentDirectory @ rot rot
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

: _include ( "FILEPATH" --)
	32 WORD FAT.find-file 				( dirSector dirOffset firstCluster size flags TRUE | FALSE)
	IF
		drop dup allocate drop			( dirSector dirOffset firstCluster size addr)
		dup >R rot	 				( dirSector dirOffset size addr firstCluster R:addr)
		FAT.load-file 				( dirSector dirOffset size R:addr)
		R@ swap evaluate drop drop			( R:addr)
		R> free
	ELSE
		4 ERROR
	THEN
;	

unused .

\ Functioinality for ANSI FORTH file words 

\ fileid structure
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

24 BUFFER: FILE.LIST			\ list of open files
FILE.LIST LIST.INIT			\ do not include with MOUNT so that MOUNT can be repeated safely

: FAT.put-fat ( value cluster --, place value in the FAT location for cluster)
	FAT.prep-fat			( value ThisFATEntOffset)
	fat.fatbuf swap		( value fat.buf ThisFATEntOffset)
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

: FAT.UpdateFSInfo ( --, update the FAT32 FSInfo sector with next free cluster)
	fat.buf 1 sd.read-sector
	FAT.NextFreeCluster @ fat.buf 492 FAT.write-long
	fat.buf 1 sd.write-sector
;

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

: FILE-SIZE ( fileid - uD ior)
	12 + @ 0
;

: FILE-POSITION ( fileid - uD ior)
	28 + @ 0
;

: REPOSITION-FILE ( ud fileid - ior)
	28 + ! 0
;

: FILE-BUFFER ( fileid - uD ior)
	36 + @ 0
;

: R/O	1 ;
: W/O 	2 ;
: R/W	3 ;

: FLUSH-FILE ( fileid -- ior, force any buffered contents to disk)
	>R R@ 8 + @ 4 and IF								\ check modified flag
		R@ 36 + @ R@ 12 + @ R@ 16 + @			( addr size firstCluster R:fileid) 
		FAT.save-file						( R:fileid)
		FAT.buf R@ 24 + @ sd.read-sector 			( R:fileid)	\ read the DirectorySector into the buffer
		R@ 12 + @ FAT.buf R@ 20 + @ + 28 FAT.write-long	( R:fileid)	\ update the file size
		FAT.buf R> 24 + @ sd.write-sector 			( R:fileid)	\ write the modified dir sector to disk
	THEN	
	0
;

: CLOSE-FILE ( fileid - ior, close the file identified by fileid and return an I/O result)
	>R R@ FLUSH-FILE drop							
	R@ LIST.rem									\ unlist the fileid	
	R@ 36 + @ free drop								\ free buffer							
	R> free 							( ior)		\ free header
;

: FAT.FindFreeEntry ( dirCluster -- dirSector dirOffset TRUE | FALSE, find the first available entry in a directory) 				
	BEGIN
		dup >R							( firstCluster R:Cluster)
		FAT.Clus2Sec						( firstSec R:Cluster)
		dup FAT.SecPerClus @ + swap				( lastSec firstSec R:Cluster)			
		DO										\ examine each sector in the cluster
			FAT.buf i SD.read-sector
			FAT.buf dup 512 + swap DO						\ examine each 32 byte entry in the sector
				i c@ dup 0= swap 229 = or IF
					j i FAT.buf - -1		( dirSector dirOffset TRUE R:Cluster)
					UNLOOP UNLOOP R> drop EXIT 
				THEN
			32 +LOOP
		LOOP	
		R>							( currentCluster)
		FAT.get-fat						( nextCluster)
		dup 268435455 =					( nextCluster flag) 	\ End-of-clusters mark
	UNTIL
	drop 0								\ no more space in last cluster [ignore expanding DIR to next cluster]
;

: FAT.size2space ( size FAM -- space, decide how much space to allocate to a file)	
	2 and IF 2* THEN								\ double for existing files with write access
	FAT.SecPerClus @ 512 * >R			( size R: clusterSize)	\ cluster size in bytes	
	R@ 1- invert and R> +			( space)			\ round-up to next whole cluster
;

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

: CREATE-FILE ( c-addr u fam -- fileid ior, if the file already exists, re-create it as a replacement empty file)
	>R over over >R >R				( addr u R:fam u c-addr)
	DELETE-FILE drop				( R:fam u c-addr) 						\ delete existing file
	FAT.CurrentDirectory @											\ create new directory entry
	FAT.FindFreeEntry IF				( dirSector dirOffset R: fam u c-addr)
		dup FAT.buf + dup 32 0 fill			( dirSector dirOffset addr R: fam u c-addr)	\ zero the directory entry
		R> R> FAT.String2Filename			( dirSector dirOffset addr c-addr R: fam )
		over 11 move					( dirSector dirOffset addr R: fam) 		\ filename
		32 over 11 + c!				( dirSector dirOffset addr R: fam)			\ FLAGS - archive bit set
		FAT.FindFreeCluster 				( dirSector dirOffset addr firstCluster R: fam)
		268435455 over FAT.put-fat swap		( dirSector dirOffset firstCluster addr R: fam)	\ update FAT
		over 65535 and over 26 FAT.write-word	( dirSector dirOffset firstCluster addr R: fam)	\ DIR_FstClusLO
		over 65536 / swap 20 FAT.write-word	( dirSector dirOffset firstCluster  R: fam)	\ DIR_FstClusHI		
		rot dup FAT.buf swap SD.write-sector	( dirOffset firstCluster dirSector R: fam)	\ update dir sector on SD
		rot rot 0 R>					( dirSector dirOffset firstCluster size fam)
		FAT.new-file
	ELSE
		-1
	THEN
;

: OPEN-FILE ( c-addr u fam -- fileid ior, open a file for sequential access)
	>R
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

: RESIZE-FILE ( ud fileid - ior)
	>R FAT.size2space R@ 36 + @ over	( space addr1 space R:fileid)
	resize					( space addr2 ior R:fileid)
	0= IF
		R@ 36 + !						\ new buffer address
		R> 32 + !						\ new size allocation
		0				( ior)
	ELSE
		R> drop drop drop -1		( ior)
	THEN
;

: READ-FILE ( addr u1 fileid -- u2 ior, read u1 characters and store at addr, return u2 the number of characters sucessfully read)
	>R					( addr u1 R:fileid)
	R@ 12 + @ R@ 28 + @ -		( addr u1 rem R:fileid)		\ check remaining un-read characters
	min 					( addr u2 R:fileid)
	R@ 36 + @ R@ 28 + @ + rot rot 	( src addr u2 R:fileid)		\ calculate source address
	dup >R					( src addr u2 R:fileid u2)
	move					( R:fileid u2)
	R> R> over over			( u2 fileid u2 fileid)
	28 + @ + swap 28 + !			( u2) 					\ update FILE-POSITION
	0
;

: READ-LINE ( addr u1 fileid -- u2 flag ior)
	>R over swap						( addrS addr u1 R: fileid)
	R@ 12 + @						( addrS addr u1 filesize  R: fileid)
	R@ 28 + @						( addrS addr u1 filesize fileposition R: fileid)
	- ifdup IF										\ unread characters available	
		min						( addrS addr u2 R: fileid)
		R@ 36 + @ R@ 28 + @ + swap over + DO	( addrS addr R: LOOP fileid)
			i c@ dup 10 = IF			( addrS addr c R: LOOP fileid)
				drop UNLOOP							\ LF - end
				i 1+ R@ 36 + @ R@ 28 + @ + - R@ 12 + @ min R> 28 + !	\ update FILE-POSITION
				swap - -1 0 EXIT		( u2 true ior)
			THEN
			dup 13 = IF
				drop								\ CR - ignore
			ELSE
				over c! 1+							\ ordinary character - copy then increment addr
			THEN
		LOOP										\ end of file
		R@ 12 + @ R> 28 + !								\ FILE-POSITION = FILE-SIZE
		swap - -1 0					( u2 true ior)	

	ELSE							\ if FILE-POSITION = FILE-SIZE before executing READ-LINE, ior=0 and flag=false
		R> drop drop drop drop 0 0 0 		( 0 false ior)	
	THEN
;
	  
: WRITE-FILE ( addr u fileid -- ior)
	>R R@ 28 + @ over over +  		( addr u oldPos newPos R:fileid)	
	dup R@ 32 + @ > IF			( addr u oldPos newPos R:fileid) 	\ check allocated space overrun
		dup R@ RESIZE-FILE
	THEN
	dup R@ 28 + !				( addr u oldpos newPos R:fileid)	\ update FILE-POSITION
	R@ 12 + @ max R@ 12 + !		( addr u oldpos R:fileid)		\ update FILE-SIZE
	R@ 8 + @ 4 or R@ 8 + !		( addr u oldpos R:fileid)		\ write modified flag
	R> 36 + @ + 				( addrS u addrD)
	swap move				
	0
;

2 BUFFER: CRLF
13 CRLF c!
10 CRLF 1+ c!

: WRITE-LINE ( addr u fileid -- ior, as WRITE-FILE but a line terminator is added)
	>R R@ WRITE-FILE drop		
	CRLF 2	R> WRITE-FILE
;

: DELETE-FILE ( c-addr u -- ior)
	FAT.find-file IF				( dirSector dirOffset firstCluster size flags)
		drop drop BEGIN								\ clear the FAT entries
			dup FAT.get-fat		( dirSector dirOffset cluster nextCluster)
			0 rot FAT.put-fat		( dirSector dirOffset nextCluster)
			dup 268435455 =		
		UNTIL
		drop FAT.buf +			( dirSector FAT.buf+dirOffset)
		229 swap c!									\ smudge the directory entry
		FAT.buf swap SD.write-sector
		0
	ELSE
		-1
	THEN
;

: RENAME-FILE ( c-addr1 u1 c-addr2 u2 -- ior, rename file1 -> file2) 
	>R >R	FAT.find-file IF			( dirSector dirOffset startCluster size flags R:u2 c-addr2)  
		drop drop drop 				( dirSector dirOffset R:u2 c-addr2)
		FAT.buf + 					( dirSector addrD R:u2 c-addr2)
		R> R> FAT.String2Filename			( dirSector addrD addrS)
		swap 11 move
		FAT.buf swap SD.write-sector
		0
	ELSE
		-1
	THEN
;

unused .

\ File utility functions

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

: DIR ( --, list the current directory)
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

: CD ( "FILEPATH" --, set the current directory)
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

: COPY-FILE ( c-addr1 u1 c-addr2 u2 -- ior, copy file1-> file2)
	>R >R
	R/O OPEN-FILE drop			( fileid1 R:u2 c-addr2)
	R> R> R/W CREATE-FILE drop		( fileid1 fileid2)
	over 12 + @ over 12 + !		\ copy file 1 size -> file 2
	over 36 + @ over 36 + !		\ copy file 1 pointer -> file 2
	7 over 8 + !				\ write modified flag on file 2
	close-file drop close-file
;

: COPY ( "FILE1" "FILE2" --, copy file1 -> file2)
	32 WORD 32 WORD
	COPY-FILE
	IF
		3000 error
	THEN
;

: DELETE ( "FILEPATH" --, delete a file)
	32 WORD DELETE-FILE
	IF
		3000 error
	THEN
;

: RENAME ( "FROM" "TO" -- rename a file)
	32 WORD 32 WORD
	RENAME-FILE
	IF
		5 error
	THEN
;

unused .