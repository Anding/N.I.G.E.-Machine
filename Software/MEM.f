\ Double linked list functions
\ 	All nodes have the structure:
\		Forward link reference
\		Backwward link reference
\		Value (user defined)

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

: LIST.ins ( n m --, insert list node m in front of list node n)
	over over 4 + !			\ node m references back to node n
	over over swap LIST.FWD swap !	\ node m references forward to node n+1
	over over swap LIST.FWD 4 + !	\ node n+1 references back to node m
	swap !					\ node n references forward to node m
;

: LIST.init ( addr --, initialize a 24 byte circular list header at addr)
\  HEAD forward link -> TAIL
\  HEAD backward link -> TAIL
\  HEAD value field 
\  TAIL forward link -> HEAD
\  TAIL backward link -> HEAD
\  TAIL value field
\ 
	dup 12 +	( addrF addrB)	\ address of front and back nodes
	LIST.VAL 0 swap !				\ zero back node
	over over !
	over over 4 + !
	swap
	LIST.VAL 0 swap !				\ zero front node
	over over !
	4 + !		
;

: LIST.initL ( addr --, initialize a 24 byte linear list header at addr)
\  HEAD forward link -> TAIL
\  HEAD backward link = 0
\  HEAD value field 
\  TAIL forward link = 0
\  TAIL backward link -> HEAD
\  TAIL value field
\ 
	dup 12 +	( addrF addrB)		\ address of front and back nodes
	over over 4 + !	
	dup 0 swap !
	over 4 + 0 swap !
	swap !
;

\ debug
: LIST.? ( addr -- iterate over a circular list showing references and values)
	dup CR
	BEGIN
		dup . dup LIST.VAL @ . CR
		LIST.FWD 
		over over =
	UNTIL
	drop drop
;

: node? ( addr -- show the contents of a list node)
	cr dup  12 + swap do i u. i @ u. cr 4 +loop
;	


\ Heap dynamic storage allocation

24 BUFFER: MEM.freeList	\ header for the free memory block list
24 BUFFER: MEM.usedList	\ header for the used memory block list
variable MEM.pointer		\ roving pointer to list of free memory blocks

: MEM.init ( addr size --, initialize heap of this address and size)
	4 - over over + 1 swap ! 8 - \ need an allocated marker at top of memory
	MEM.freeList dup list.init	\ create the list header
	over LIST.VAL !		\ store size field of free memory block	
	MEM.freeList over LIST.INS	\ insert the free memory block to the list
	MEM.pointer !			\ initialize ROVER <- addr	
	mem.usedlist list.init	\ initialize the used memory list
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

: ALLOCATE ( u -- addr ior, allocate u bytes of memory, where u>0 )
	32 max								\ minimum block allocation to reduce fragmentation
	32 + >R 							\ need 32 extra bytes for tags in two blocks
	MEM.pointer @ dup BEGIN			( rov ref R: u)	
		dup LIST.VAL @ 			( rov ref sizeOfBlock R: u')
		R@ U< not IF							\ suitable block is available
			dup LIST.FWD MEM.pointer !				\ move roving pointer on on		
			dup LIST.VAL @ 		( rov ref size R: u')
			R@ - 16 + 			( rov ref newsize R: u')	\ 16 bytes for each block
			over over 0 MEM.mark		( rov ref newsize R: u')	\ mark the free block
			+ dup R> 16 - -1 MEM.mark	( rov nextRef)		\ mark the allocated block
			MEM.usedList over LIST.INS					\ add to the used memory list
			nip 12 + 0 EXIT		( addr 0)		\ success
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
	dup 1 and 0= IF				( sizeBelow flag R:thisRef)		\ block below is free
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
	MEM.freeList swap LIST.INS	 							\ add the free block to the list
;

\ debug
8192 BUFFER: RAM

: MEM.free?
	MEM.freeList LIST.?
;

: MEM.used?
	MEM.usedList LIST.?
;