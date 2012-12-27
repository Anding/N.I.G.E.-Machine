h0	bra 	start h0 rel
data	dc.l	hex aabbccdd	
start	#.w	data	
	fetch.l
	bra 	0
	
	