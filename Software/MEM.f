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
		R@ LIST.VAL @ 1 not and		( max num sum size R:startRef currentRef)
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
here1 16744448 over -				( here1 n)	\ here1 is the bottom of the heap and must be aligned
MEM.init								\ 16744448 is top of memory less 32K used for include buffer

