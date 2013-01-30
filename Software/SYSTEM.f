\ LIST functions
\	Default list structure is a circular, double-linked list
\ 	All nodes are 12 bytes organized as:
\		Forward link reference
\		Backward link reference
\		Value (user defined, typically 0 for a header node)
\	List pointers always point to the list node's byte zero
\ 	A list header is 24 bytes long and comprises two adjacent nodes that circular reference each other

: LIST.fwd ( n -- n, given a list node reference return the next forward list node)
	@
;

: LIST.bck ( n -- n, given a list node reference return the next back list node)
	4 + @
;

: LIST.val ( n -- x, given a list node reference return address of the value field)
	8 + 
;

: LIST.rem ( n --, given a list node remove it from the list)
	dup LIST.FWD over LIST.BCK !	\ back node now references forward node 
	dup LIST.BCK swap LIST.FWD 4 + !	\ forward node now references back node
;

: LIST.ins ( m n --, insert list node m in front of list node n)
	swap over over 4 + !			\ node m references back to node n
	over over swap LIST.FWD swap !	\ node m references forward to node n+1
	over over swap LIST.FWD 4 + !	\ node n+1 references back to node m
	swap !					\ node n references forward to node m
;

: LIST.init ( addr --, initialize a 24 byte circular list header at addr)
	dup 12 +	( addrF addrB)	\ address of front and back nodes
	dup LIST.VAL 0 swap !			\ zero back node
	over over !
	over over 4 + !
	swap
	dup LIST.VAL 0 swap !			\ zero front node
	over over !
	4 + !		
;

\ : LIST.initLin ( addr --, initialize a 24 byte linear list header at addr)
\	dup 12 +	( addrF addrB)		\ address of front and back nodes
\	over over 4 + !	
\	dup 0 swap !
\	over 4 + 0 swap !
\	swap !
\ ;

: LIST.? ( addr -- iterate over a circular list showing references and values)
	dup CR
	BEGIN
		dup . dup LIST.VAL @ . CR
		LIST.FWD 
		over over =
	UNTIL
	drop drop
;

\ ----------------------------------------------------------------------------------------------------------------

\ Dynamic memory allocation functions (the heap)

24 BUFFER: MEM.freeList	\ header for the free memory block list
24 BUFFER: MEM.usedList	\ header for the used memory block list
variable MEM.pointer		\ roving pointer to list of free memory blocks - Knuth's efficiency enhancement

: MEM.init ( addr size --, initialize heap of this address and size)	\ ensure addr and size are aligned!
	4 - over over + 1 swap ! 8 -	( addr size-12)		\ place a dummy marker at top of memory
	over LIST.VAL !			( addr )			\ store size field of free memory block		
	dup MEM.pointer !			( addr )			\ initialize roving pointer <- addr		
	MEM.freeList dup list.init		( addr MEM.freeList)		\ create the list header
	LIST.INS				(  )				\ insert the free memory block to the list
	mem.usedlist list.init		(  )				\ initialize the used memory list
;

: MEM.? ( MemList --, summarize the used or free memory situation)
	dup >R >R 0 -2 0 				( max num sum R:startRef currentRef) \ -2 adjustment for list header nodes
	BEGIN
		R@ LIST.VAL @ 1 invert and		( max num sum size R:startRef currentRef)
		dup >R +				( max num sum+ R:startRef currentRef size)
		rot R> max				( num sum+ max+ R:startRef currentRef)
		rot 1+ rot				( max+ num+ sum+ R:startRef currentRef)
		R> LIST.FWD R@ over >R		( max+ num+ sum+ nextRef startRef R:startRef nextRef)
		=					( max+ num+ sum+ flag R:startRef nextRef)
	UNTIL
	R> R> drop drop
	cr ." Total " u.
	."  Blocks " u.			
	."  Largest " u. cr
;

: MEM.mark	( ref size flag -- mark the block at ref with size and allocated flag (true = allocated)
	>R swap			( size ref R: flag)
	over over + 4 -		( size refLo refHi R: flag)
	ROT R> IF			( refLo refHi size)
		1 or				\ mark low bit of size field since memory addresses are always even
	THEN
	dup rot !			( refLo size')
	swap LIST.VAL !
;

: MEM.SIZE ( addr -- n, show the size of an allocated memory block)
	4 - @ 17 -				\ -16 for the marker bytes and -1 for the tag flag
;

: ALLOCATE ( u -- addr ior, allocate u bytes of memory, where u>0 )
	32 max								\ minimum block allocation to reduce fragmentation
	32 + >R 							\ need 32 extra bytes for tags in two blocks
	MEM.pointer @ dup BEGIN			( rov ref R: u)	
		dup LIST.VAL @ 			( rov ref sizeOfBlock R: u')
		R@ U< not IF							\ suitable block is available
			dup LIST.FWD MEM.pointer !					\ move roving pointer on on		
			dup LIST.VAL @ 		( rov ref size R: u')
			R@ - 16 + 			( rov ref newsize R: u')	\ 16 bytes for each block
			over over 0 MEM.mark		( rov ref newsize R: u')	\ mark the free block
			+ dup R> 16 - -1 MEM.mark	( rov nextRef)		\ mark the allocated block
			dup MEM.usedList LIST.INS					\ add to the used memory list
			nip 12 + 0 EXIT		( addr 0)			\ success
		THEN
		LIST.FWD				( rov addr' R: u')
		over over = IF
			R> drop
			EXIT							\ complete cycle of the avail list
		THEN
	AGAIN
;

: FREE ( addr -- ior, free the previously allocated memory at u)
	12 - dup >R					( R:thisRef)				\ offset to this block's base reference
	dup LIST.REM										\ remove from the used memory list
	4 - @						( sizeBelow R:thisRef)		\ check block below
	dup 1 and 0= IF				( sizeBelow R:thisRef)		\ block below is free
		dup R@ swap - 			( sizeBelow newRef R:thisRef) 	\ calculate new block reference
		dup LIST.REM				( sizeBelow newRef R:thisRef) 	\ remove lower block from the list
		R> LIST.VAL @ rot + 1 - 		( newRef newSize)			\ calculate newsize (-1 for allocation marker)
	ELSE
		drop R> dup LIST.VAL @ 1 -		( thisRef thisSize)
	THEN 						( Ref Size)
	swap >R 					( Size R:thisRef)	
	R@ over + LIST.VAL @				( Size sizeAbove R:thisRef)		\ check the block above
	dup 1 and 0= IF 				( Size sizeAbove flag R:thisRef)		\ block above is free
		over R@ + 				( Size sizeAbove upperRef R:thisRef)	\ reference upper block
		LIST.REM				( Size sizeAbove R:thisRef)		\ remove upper block from the list
		+ 					( newSize R:thisRef)			\ calculate newsize (-1 for allocation marker)
		R> swap 				( thisRef newSize)			
	ELSE
		drop R> swap				( thisRef thisSize)
	THEN 
	over swap 0 MEM.mark				( Ref)					\ mark the memory size
	dup MEM.pointer !									\ update the general pointer
	MEM.freeList LIST.INS	 							\ add the free block to the list
	0
;


: RESIZE ( addr1 u - addr2 ior)
	over over allocate 0= IF			( addr1 u addr1 addr2)
		dup >R rot				( addr1 addr1 addr2 u R:addr2)
		move
		free drop
		R> 0 EXIT
	ELSE
		-1 EXIT
	THEN
;

: AVAIL ( --, show free memory)
	MEM.freeList
	MEM.?
;

: UNAVAIL ( --, show used memory)
	MEM.usedList
	MEM.?
;

\ redefine BUFFER:
: BUFFER: ( n "NAME" --) \ NAME ( -- addr)
	: 					\ create the word
	55 c, 					\ compile 55, the opcode for #.l, into the definition
	allocate drop ,			\ store the address in the definition
	postpone ;				\ finish the word
;

\ autoconfigure the heap
here1 16711680 over -				( here1 n)	\ here1 is the bottom of the heap and must be aligned
MEM.init								\ 16711680 is top of memory less 64K used for include buffer

\ ----------------------------------------------------------------------------------------------------------------

\ FAT file system

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
FILE.LIST LIST.INIT			

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
	12 + @ 0 0
;

: FILE-POSITION ( fileid - uD ior)
	28 + @ 0 0
;

: REPOSITION-FILE ( uD fileid - ior)
	nip 28 + ! 0
;

: FILE-BUFFER ( fileid - addr ior)
	36 + @ 0
;

: R/O	1 ;
: W/O 	2 ;
: R/W	3 ;

: FLUSH-FILE ( fileid -- ior, force any buffered contents to disk)
	dup 8 + @ 4 and IF								\ check modified flag
		>R R@ 36 + @ R@ 12 + @ R@ 16 + @			( addr size firstCluster R:fileid) 
		FAT.save-file						( R:fileid)
		FAT.buf R@ 24 + @ sd.read-sector 			( R:fileid)	\ read the DirectorySector into the buffer
		R@ 12 + @ FAT.buf R@ 20 + @ + 28 FAT.write-long	( R:fileid)	\ update the file size
		FAT.buf R> 24 + @ sd.write-sector 			( R:fileid)	\ write the modified dir sector to disk
	ELSE
		drop
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

: FAT.size2space ( size -- space, rule for the space to allocate to a file)	
	FAT.SecPerClus @ 512 * >R			( size R: clusterSize)	\ cluster size in bytes	
	R@ 1- invert and R> +			( space)			\ round-up to next whole cluster
;

: FAT.new-file ( dirSector dirOffset firstCluster size fam -- fileid ior)
	36 allocate 0= IF					
		>R 					( dirSector dirOffset firstCluster size fam R:fileid)
		over over 2 and IF 2* THEN		( dirSector dirOffset firstCluster size size* R:fileid)	\ double size when requesting space if W access
		FAT.size2space R@ 32 + !						\ buffer space
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

: DELETE-FILE ( c-addr u -- ior)
	FAT.find-file IF				( dirSector dirOffset firstCluster size flags)
		drop drop 				( dirSector dirOffset firstCluster)
		BEGIN					( dirSector dirOffset cluster)	\ clear the FAT entries
			dup FAT.get-fat		( dirSector dirOffset cluster nextCluster)
			0 rot FAT.put-fat		( dirSector dirOffset nextCluster)
			dup 268435455 =		
		UNTIL
		drop 	
		FAT.buf +				( dirSector FAT.buf+dirOffset)
		229 swap c!				( dirSector)				\ smudge the directory entry
		FAT.buf swap SD.write-sector	( )	
		0
	ELSE
		-1
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

: RESIZE-FILE ( uD fileid - ior)
	>R drop 	 			( n R:fileid)		\ convert double to single
	FAT.size2space R@ 36 + @ over	( space addr1 space R:fileid)
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

\ update CR and LF characters below for different line ending specifications
: READ-LINE ( addr u1 fileid -- u2 flag ior)
	>R over swap						( addrS addr u1 R:fileid)
	R@ 12 + @						( addrS addr u1 filesize  R:fileid)
	R@ 28 + @						( addrS addr u1 filesize fileposition R:fileid)
	- ?dup IF												\ unread characters available	
		min						( addrS addr u2 R:fileid)
		R@ 36 + @ R@ 28 + @ + swap over + swap DO	( addrS addr R:LOOP fileid)
			i c@ dup 10 = IF			( addrS addr c R:LOOP fileid)		\ LF - end
				drop i UNLOOP			( addrS addr lastRead R:fileid)
				R@ 36 + @ - 1+ R> 28 + !	( addrS addr newPos R: LOOP fileid)	\ update FILE-POSITION					
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
		dup 0 R@ RESIZE-FILE							\ RESIZE-FILE takes uD size argument
	THEN
	dup R@ 28 + !				( addr u oldpos newPos R:fileid)	\ update FILE-POSITION
	R@ 12 + @ max R@ 12 + !		( addr u oldpos R:fileid)		\ update FILE-SIZE
	R@ 8 + @ 4 or R@ 8 + !		( addr u oldpos R:fileid)		\ write modified flag
	R> 36 + @ + 				( addrS u addrD)
	swap move				
	0
;

2 SBUFFER: CRLF
13 CRLF c!
10 CRLF 1+ c!

: WRITE-LINE ( addr u fileid -- ior, as WRITE-FILE but a line terminator is added)
	>R R@ WRITE-FILE drop		
	CRLF 2	R> WRITE-FILE
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
		R> R> drop drop -1
	THEN
;

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
	PAD over 					\ remember address and use FAT.filestring for the return string address
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
	PAD dup rot swap -	( addr n)		\ was FAT.filestring
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
			?dup 0= IF FAT.RootClus @ THEN					\ subdirectories reference root at 0
			FAT.CurrentDirectory !
		ELSE
			3001 error
		THEN
	ELSE
		3000 error
	THEN
;

: DELETE ( "FILEPATH" --, delete a file)
	32 word count DELETE-FILE
	IF
		3000 error
	THEN
;

