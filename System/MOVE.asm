; This version of MOVE is partially optimized but is not suitable for FORTH.asm
; because it does not properly deal with misaligned PSDRAM addresses
;
;
; MOVE ( addr-s addr-d n, memory copy)
; 	simple optimization: 
; 	if n is a multiple of 4, then use longword fetch and store
; 	if n is even, then use word fetch and store
; 	otherwise use byte fetch and store
;	Caution! if addr-s or addr-d are in PSDRAM they must longword or word aligned according to the size of n!
;
MOVE.LF	dc.l	DOES>.NF
MOVE.NF	dc.b	4 128 +
		dc.b 	char E char V char O char M
MOVE.SF	dc.w	MOVE.Z MOVE.CF del
MOVE.CF	?dup	
		IF	( s d n)
			>R	( s d R:n)
			over	( s d s R:n)
			over	( s d s d R:n)
			>	( s d flag R:n)
			IF	; copy up if s>d
				R>	( s d n)
				dup	( s d n n)				; optimization
				dup	( s d n n n)
				lsr
				lsr
				lsl
				lsl
				-	(s d n rem)
				dup
				0=	(s d n rem rem)
				IF	( s d n rem)	
					drop	( s d n)			; longword
					2/
					2/	( s d n/4)
					zero	( s d n/4 0)
					DO	( s d)
						over 		( s d s)
						fetch.l	( s d c)
						over		( s d c d)
						store.l	( s d)
						#.b 4		( s d 4)
						+		( s d+4)
						swap		( d+4 s)
						#.b 4
						+		( d+4 s+4)
						swap		( s+4 d+4)
					LOOP
				ELSE	( s d n rem)
					#.b 2	( s d n rem 2)
					=	( s d n flag)
					IF	( s d n)				; word
						2/	( s d n/2)		
						zero	( s d n/2 0)
						DO	( s d)
							over 		( s d s)
							fetch.w	( s d c)
							over		( s d c d)
							store.w	( s d)
							1+		
							1+		( s d+2)
							swap		( d+2 s)
							1+		
							1+		( d+2 s+2)
							swap		( s+2 d+2)
						LOOP	
					ELSE ( s d n )				; byte
						zero	( s d n 0)			
						DO	( s d)
							over 		( s d s)
							fetch.b	( s d c)
							over		( s d c d)
							store.b	( s d)
							1+		( s d+1)
							swap		( d+1 s)
							1+		( d+1 s+1)
							swap		( s+1 d+1)
						LOOP
					THEN
				THEN
			ELSE	( s d R:n)			; copy down if s<d
				R@	( s d n R:n)
				dup	( s d n n R:n)				; optimization
				dup	( s d n n n R:n)
				lsr
				lsr
				lsl
				lsl
				-	( s d n rem R:n)
				dup	( s d n rem rem R:n)
				0=	( s d n rem flag R:n)
				IF	( s d n rem R:n)	
					drop	( s d n R:n)			; longword	
					+	( s d+n R:n)			; recalculate top addresses
					#.b 4
					-	( s d+n-4 R:n)
					swap	( d+n-4 s R:n)
					R@	( d+n-4 s n R:n)
					+	( d+n-4 s+n R:n)
					#.b 4
					-	( d+n-4 s+n-4 R:n)
					swap	( s+n-4 d+n-4 R:n)
					R>	( s d n)
					2/
					2/	( s d n/4)
					zero	( s d n/4 0)
					DO	( s d)
						over 		( s d s)
						fetch.l	( s d c)
						over		( s d c d)
						store.l	( s d)
						#.b 4
						-		( s d-4)
						swap		( d-4 s)
						#.b 4
						-		( d-4 s-4)
						swap		( s-4 d-4)
					LOOP
				ELSE	( s d n rem R:n)
					#.b 	hex ff
					=	( s d n flag R:n)
					IF	( s d n)				; word
						+	( s d+n R:n)
						1-
						1-	( s d+n-2 R:n)
						swap	( d+n-2 s R:n)
						R@	( d+n-2 s n R:n)
						+	( d+n-2 s+n R:n)
						1-
						1-	( d+n-2 s+n-2 R:n)
						swap	( s+n-2 d+n-2 R:n)
						R>	( s d n)
						2/	( s d n/2)
						zero	( s d n/2 0)
						DO	( s d)
							over 		( s d s)
							fetch.w	( s d c)
							over		( s d c d)
							store.w	( s d)
							1-
							1-		( s d-2)
							swap		( d-2 s)
							1-
							1-		( d-2 s-2)
							swap		( s-2 d-2)
						LOOP				
					ELSE ( s d n R:n)				; byte				
						+	( s d+n R:n)
						1-	( s d+n-1 R;n)
						swap	( d+n-1 s R:n)
						R@	( d+n-1 s n R:n)
						+	( d+n-1 s+n R:n)
						1-	( d+n-1 s+n-1 R:n)
						swap	( s+n-1 d+n-1 R:n)
						R>	( s d n)
						zero	( s d n 0)
						DO	( s d)
							over 		( s d s)
							fetch.b	( s d c)
							over		( s d c d)
							store.b	( s d)
							1-		( s d-1)
							swap		( d-1 s)
							1-		( d-1 s-1)
							swap		( s-1 d-1)
						LOOP
					THEN
				THEN
			THEN	
		THEN
		drop
MOVE.Z		drop,rts