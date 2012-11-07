; FAT file system
;
_FAT.buf			equ	0		; UPDATE to 512 byte storage space location
;
; FORTH variables
FAT.SecPerClus		dc.l	0		; sectors per cluster
FAT.TotalSectors		dc.l 	0		; total sectors on the disk
FAT.NextFreeCluster		dc.l 	0		; where to look for the next free cluster
FAT.CurrentDirectory		dc.l 	0		; cluster number of current directory
FAT.RootClus			dc.l 	0		; first cluster of root directory
FAT.buf			dc.l	_FAT.buf	; buffer for general sector access
;
; internal variables
FAT.RsvdSecCnt		dc.l	0		; number of reserved sectors
FAT.FirstDataSector		dc.l	0		; first sector after FAT
FAT.FATinBuf			dc.l	0		; the currently buffered FAT sector
;
; FAT.read-long ( addr n -- x, get a little endian longword from the buffer)
FAT.read-long	+
		dup 
		#.b	4 
		+ 
		swap 
		DO 
			i 
			fetch.b 
		LOOP
		#.b	3 
		zero 
		DO 
			#.b	256 
			multu
			drop 
			+ 
		LOOP
		rts
;
; EX
; FAT.write-long ( x addr n --, write a little endian longword x to the buffer at position n)
FAT.write-long	+ 
		>R 
		>R	( R: x addr+n)
		R@ 
		#.b	24 
		CALL	rshift.cf 
		#.b	255 
		and	
		R@ 
		#.b	16 
		CALL	rshift.cf
		#.b	255 
		and	
		R@ 
		#.b	8 
		CALL	rshift.cf 
		#.b	255 
		and	
		R> 
		#.b	255 
		and
		R> 
		dup 
		#.b	4 
		+ 
		swap
		DO 
			i 
			store.b 
		LOOP
		rts
;
	
; FAT.read-word ( addr n -- x, get a little endian word from the buffer)
FAT.read-word	1+ 
		+
		dup 
		fetch.b 
		#.b	256 
		multu
		drop 
		swap
		1- 
		fetch.b 
		+,rts
;
; FAT.write-word ( x addr n --, write a litte endian word to the buffer)
FAT.write-word	+ 
		>R 
		>R	( R : x addr+n)
		R@ 
		#.b	8 
		CALL	rshift.cf 
		#.b	255 
		and	
		R> 
		#.b	255 
		and
		R@ 
		store.b
		R> 
		1+ 
		store.b
		rts
;
; EX
; MOUNT ( --, initiaize the SD card and FAT data structures)
MOUNT		call	sd.init
		#.w	fat.buf 
		zero 
		call	sd.read-sector.cf
		#.w	fat.buf 
		#.w	510 
		call	fat.read-word 
		#.w	43605 
		<> 
		IF		; confirm sector signature 0xAA55
			#.w	2000 
			call	error.cf
		THEN
		#.w	fat.buf 
		#.b	82 
		CALL	fat.read-word 
		#.w	16710 
		<> 
		IF		; confirm FAT32 signature 0x4146
			#.w	2001 
			CALL	error
		THEN
		#.w	fat.buf 			; OPTIMIZE with fat.buf -> R@
		#.b	13 
		+ 
		store.b 
		#.w	fat.secperclus 
		store.l
		#.w	fat.buf 
		#.b	44 
		CALL	fat.read-long 
		dup 
		#.w	fat.rootclus 
		store.l 
		#.w	FAT.CurrentDirectory 
		store.l
		#.w	fat.buf 
		#.b	32 
		CALL	fat.read-long 
		#.w	fat.TotalSectors 
		store.l
		#.w	fat.buf 
		#.b	14 
		CALL	fat.read-word 
		dup 
		#.w	fat.rsvdseccnt 
		store.l				( RsvdSecCnt)
		#.w	fat.buf 
		#.b	16 
		+ 
		fetch.b				( RsvdSecCnt NumFATs)
		#.w	fat.buf 
		#.b	36 
		CALL	fat.read-long			( RsvdSecCnt NumFATs SecPerFAT)
		multu
		drop
		+ 
		#.w	fat.firstdatasector 
		store.l
		#.w	fat.buf 
		#.b	1 
		CALL	sd.read-sector.cf		; FAT32 FSInfo
		#.w	fat.buf 
		zero 
		CALL	fat.read-long 
		#.l	hex 41615252 
		<> 
		IF					; confirm valid FSInfo sector
			#.w	2002 
			CALL	error					
		THEN
		#.w	fat.buf 
		#.w	492 
		CALL	fat.read-long 
		dup
		zero
		1- 
		= 
		IF 
			drop 
			#.b	2 
		THEN
		#.w	FAT.NextFreeCluster 
		store.l
		zero 
		#.w	FAT.FATinBuf 
		store.l				; FAT buffer overwritten
		rts
;
; EX
; FAT.UpdateFSInfo ( --, update the FAT32 FSInfo sector with next free cluster)
FAT.UpdateFSInfo	#.w	fat.buf 
		#.b	1 
		CALL	sd.read-sector.cf
		#.w	FAT.NextFreeCluster 
		store.l 
		#.w	fat.buf 			; OPTIMIZE FAT.buf -> R@
		#.w	492 
		CALL	FAT.write-long
		#.w	fat.buf 
		#.b	1 
		CALL	sd.write-sector.cf
		zero 
		#.w	FAT.FATinBuf 
		store.l				; FAT buffer overwritten
		rts
;
; EX
; FAT.clus2sec ( n -- n, given a valid cluster number return the number of the first sector in that cluster)
FAT.clus2sec	1-				
		1-					; first cluster is number 2
		#.w	fat.secperclus 
		fetch.l
		multu
		drop
		#.w	fat.firstdatasector 
		fetch.l
		+,rts
;
;
; FAT.prep-fat ( n -- ThisFATEntOffset, calulate location and load the appropriate FAT sector into fat.fatbuf)
FAT.prep-fat	#.b	4 
		multu
		drop			( FATOffset)
		#.w	512 
		divu			( rem quo)
		#.w	fat.rsvdseccnt 
		fetch.l 
		+ 			( ThisFATEntOffset ThisFATSecNum)
		dup 
		#.w	FAT.FATinBuf 
		fetch.l 
		<> 
		IF
			dup 
			#.w	FAT.FATinBuf 
			store.l			; remember the buffered sector
			#.w	fat.buf 
			swap	 			( ThisFATEntOffset fat.fatbuf ThisFATSecNum)
			CALL	SD.read-sector.cf	( ThisFATEntOffset)
		ELSE
			drop				( ThisFATEntOffset)
		THEN
		rts
;
; EX
; FAT.get-fat ( n -- x, return the FAT entry for a given cluster)
FAT.get-fat	CALL	FAT.prep-fat
		#.w	fat.buf 
		swap					( fat.buf ThisFATEntOffset)
		CALL	fat.read-long
		#.l	hex	0FFFFFFF
		and,rts
;
; EX
; FAT.put-fat ( value cluster --, place value in the FAT location for cluster)
FAT.put-fat	CALL	FAT.prep-fat			( value ThisFATEntOffset)
		#.w	fat.buf 
		swap					( value fat.buf ThisFATEntOffset)
		CALL	fat.write-long
		#.w	FAT.buf 
		#.w	FAT.FATinBuf 
		fetch.l
		CALL	SD.write-sector.cf
		rts
;
; EX
; FAT.string2filename ( addr n -- addr, convert an ordinary string to a short FAT filename)
FAT.string2filename	>R 
		>R		
		#.l	_PAD 				; was FAT.filestring
		dup 
		dup 
		dup					 
		#.b	12 
		+ 
		swap 
		DO 					; fill the output string with blanks
			#.b	32 
			i 
			store.b 
		LOOP	 			
		R> 
		R>					( filestring filestring addr n)
		?dup 
		IF		
			#.b	12 
			CALL	min.cf
			over 
			+ 
			swap 				( filestring filestring addrE addr)					
			DO				; loop over the input string up to 12 characters
				i 
				fetch.b 
				dup 
				#.b	46 		; .
				= 				 
				IF
					drop 
					drop 
					dup 
					#.b	8 
					+		; re-position in output string
				ELSE
					CALL	upper.cf
					over 
					store.b 
					1+		; save and increment position in output string
				THEN				
			LOOP
			drop
		ELSE					; zero length interpret as ".." for up directory
			drop 
			#.b	46 
			over 
			store.b 
			1+
			#.b	46 
			swap 
			store.b
		THEN
		rts
;
; FAT.find-file-local ( dirCluster addr n -- dirSector dirOffset firstCluster size flags TRUE | FALSE, find in local folder)
FAT.find-file-local	CALL	FAT.String2Filename	( cluster filestring)
		swap 
		dup 
		>R					( filestring cluster R:cluster)
		BEGIN
			CALL	FAT.Clus2Sec		( filestring firstSec R:cluster)
			dup 
			#.w	FAT.SecPerClus 
			fetch.l 
			+ 
			swap				( filestring lastSec firstSec R:cluster)	
		DO					( filestring R:LOOP cluster)	; examine each sector in the cluster
			#.w	FAT.buf 
			i 
			CALL	SD.read-sector.cf
			#.w	FAT.buf 
			dup 
			#.w	512 
			+ 
			swap 
			DO				( filestring R:LOOP LOOP cluster)	; examine each 32 byte entry in the sector
				i 
				fetch.b 
				dup 
				0= 
				IF 			; empty entry and no following entries - exit false flag
					CALL	UNLOOP.CF
					CALL	UNLOOP.CF 
					nip 
					R> 
					drop 
					rts 
				THEN	
				#.b	229 
				<> 
				IF							; non-0xE5 first byte indicates valid entry
					i 
					#.b	11 
					+ 
					fetch.b 
					#.b	15 
					and 
					#.b	15 
					<> 
					IF						; is not a long-name entry
						dup 
						#.b	11 
						i 
						#.b	11 
						CALL	$=.cf 
						IF					; test string match
							drop							; remove filestring	
							CALL	j.cf						; dirSector
							i 
							#.w	FAT.buf 
							-							; directory offset 
							i 
							#.b	20 
							CALL	FAT.read-word 
							#.l	65536 
							multu
							drop
							i 
							#.b	26 
							CALL FAT.read-word 
							+ 							; startCluster
							i 
							#.b	28 
							CALL	FAT.read-long 				; size		
							i 
							#.b	11 
							+ 
							fetch.b						; flags
							CALL UNLOOP.cf 
							CALL UNLOOP.cf
							R> 
							drop 
							zero
							1-
							rts							; exit with true flag
						THEN
					THEN
				THEN
				#.b	32 
				+LOOP
				LOOP 
				R>					( filestring currentCluster)
				CALL	FAT.get-fat			( filestring nextCluster)
				dup 
				#.l	hex 0FFFFFFF 								; End-of-clusters mark
				=					( filestring nextCluster flag) 	
			UNTIL
		drop 
		drop 
		zero,rts									; likely bad directory
;
; EX	
; FAT.find-file ( addr n -- dirSector dirOffset firstCluster size flags TRUE | FALSE, find from current directory)
FAT.find-file	#.w	FAT.CurrentDirectory 
		fetch.l 
		rot 
		rot
		over 
		+ 
		1- 
		dup 
		>R 
		over 								( cluster startAddr endAddr startAddr R:endAddr-1)
		over
		over
		-
		IF
			DO							( cluster startAddr R: LOOP endAddr-1)
				i 
				fetch.b 
				dup 
				#.b	92 
				= 
				swap 
				#.b	47 
				= 
				or 
				IF 
					i 
					over 
					-					( cluster Addr n)
					CALL	FAT.find-file-local 
					IF 
						dup 
						#.b	15 
						and 
						#.b	15 
						<> 
						swap 
						#.b	16 
						= 
						and 
						IF					; is a directory
							drop 
							nip 
							nip 
							?dup 
							0= 
							IF 				; root directory adjustment
								#.w	FAT.RootClus 
								fetch.l 
							THEN		
							i 
							1+			( newCluster newAddr)
						ELSE					; cannot parse filepath - not a directory
							CALL	UNLOOP.cf 
							R> 
							drop 
							drop 
							drop 
							drop 
							drop 
							zero 
							rts	
					THEN
				ELSE						; cannot parse filepath - not found
					CALL	UNLOOP.cf 
					R> 
					drop 
					zero 
					rts				
				THEN
			THEN
			LOOP							( cluster Addr R:endAddr-1)
		ELSE
			drop
			drop
		THEN
		dup 
		fetch.b 
		dup 
		#.b	92 
		= 
		swap 
		#.b	47 
		= 
		or 
		IF 
			R> 						( cluster addr 0)	; n=0 will be interpreted as ".."
			drop 
			zero						
		ELSE
			R> 
			1+ 
			over 
			- 						( cluster addr n)
		THEN
		CALL	FAT.find-file-local 	
		rts
;
; EX
; FAT.load-file ( addr firstCluster --, load a file to addr given the first cluster, cluster by cluster)
FAT.load-file	BEGIN						
			dup 
			>R					( addr currentCluster R:currentCluster)
			CALL	FAT.Clus2Sec			( addr firstSec R:currentCluster)
			dup 
			#.w	FAT.SecPerClus 
			fetch.l 
			+ 
			swap					( addr lastSec firstSec R:currentCluster)
			DO
				dup 
				i 
				CALL SD.read-sector.cf		
				#.w	512 
				+				( addr)
			LOOP
			R>					( addr currentCluster)
			CALL	FAT.get-fat			( addr nextCluster)
			dup 
			#.l	hex 0FFFFFFF 			; End-of-clusters mark
			=					( addr nextCluster flag) 	
		UNTIL
		drop 
		drop,rts
;
; EX
; include ( "FILEPATH" --)
include	#.b	32 
		CALL	word.cf
		CALL	count.cf 
		CALL	FAT.find-file 			( dirSector dirOffset firstCluster size flags TRUE | FALSE)
		IF
			drop 
			>R 
			nip 
			nip 
			#.l	hex 00FF8000 			; addr is 32K below top of memory
			dup 
			rot					( addr addr firstCluster R:size)			
			CALL	FAT.load-file 		( addr R:size)
			R> 
			CALL	evaluate.cf 			( )
		ELSE
			#.b	4 
			CALL	ERROR.CF
		THEN
		rts