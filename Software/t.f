\ Double linked circular list functions

: LIST.FWD ( n -- n, given a list node reference return the next forward list node)
	@
;

: LIST.BCK ( n -- n, given a list node reference return the next back list node)
	4 + @
;

: LIST.DEL ( n --, given a list node remove it from the list)
	dup LIST.FWD over LIST.BCK !	\ back node now references forward node 
	LIST.BCK over LIST.FWD 4 + !	\ forward node now references back node
;

: LIST.INS ( n m --, insert list node m in front of list node n)
	over over 4 + !			\ node m references back to node n
	over over swap LIST.FWD swap !	\ node m references forward to node n+1
	over over swap LIST.FWD 4 + !	\ node n+1 references back to node m
	swap !					\ node n references forward to node m
;

: LIST.INIT ( addr --, initialize a list at addr)
	dup 12 +	( addrF addrB)	\ address of front and back nodes
	over over !
	over over 4 + !
	swap
	over over !
	4 + !		
;

\ Heap dynamic storage allocation

24 BUFFER: MEM.list		\ header for the memory list
variable MEM.ROVER		\ roving pointer to list of free nodes in heap
: MEM.INIT ( addr size --, initialize heap of this address and size)
	MEM.list dup LIST.INIT	\ create the list header
	dup 8 + 0 swap !		\ size of the head is zero
	16 + 0 swap !			\ size of the tail is zero
	over 8 + !			\ store size field of free memory block	
	MEM.list over	LIST.INS	\ insert the free memory block to the list
	MEM.ROVER !			\ initialize ROVER <- addr	
;

: MEM.size	( ref size flag -- mark the block at ref with size.  flag true = allocated)
	>R swap			( size ref R: flag)
	over over + 4 -		( size refLo refHi R: flag)
	ROT R> IF			( refLo refHi size)
		1 or				\ mark low bit of size field since memory addresses are always even
	THEN
	dup rot !			( refLo size')
	swap 8 + !
;

: ALLOCATE ( u -- addr ior, allocate u bytes of memory, where u>0 )
	256 max							\ minimum block allocation to reduce fragmentation
	32 + >R 								\ need 32 extra bytes for tags in two blocks
	MEM.ROVER @ BEGIN				( ref R: u)	
		dup 8 + @ 				( ref sizeOfBlock R: u')
		R@ U< not IF							\ suitable block is available
			.s dup LIST.FWD MEM.ROVER !				\ move ROVER on		
			dup 8 + @ 			( ref size R: u')
			R@ - 16 + 			( ref newsize R: u')	\ 16 bytes for 
			over over 0 MEM.size		( ref newsize R: u')	\ size the free block
			+ dup R> 16 - -1 MEM.size	( nextRef)		\ size the allocated block
			12 + 0 EXIT			( addr)		\ success
		THEN
		LIST.FWD
		dup MEM.ROVER @ = IF
			R> drop
			dup EXIT						\ complete cycle of the avail list
		THEN
	AGAIN
;


\ test purposes
8192 BUFFER: RAM

: ?MEM.LIST
	cr MEM.LIST dup  24 + swap do i u. i @ u. cr 4 +loop
;

: ?node ( addr)
	cr dup  12 + swap do i u. i @ u. cr 4 +loop
;	
	
