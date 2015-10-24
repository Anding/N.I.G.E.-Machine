hex
03f860 constant MACready
03f864 constant MACdata
decimal

: octet ( -- x)		\ fetch an octet from the MAC FIFO buffer
	0
	4 0 DO			\ lo nibble first
		2 rshift
		MACdata @
		6 lshift
		or
	LOOP
;

0 constant #preamble
1 constant #destination
2 constant #source
3 constant #type
7 constant #other

: .frame
	#preamble
	BEGIN

	CASE
	#preamble	OF MACdata @ 3 = IF #destination ELSE #preamble THEN ENDOF
	#destination	OF CR ." Dest: " 6 0 DO octet . LOOP #source ENDOF
	#source	OF CR ." Src:  " 6 0 DO octet . LOOP #length ENDOF
	#type		OF CR ." Type:  " octet 256 * octet + . CR #other ENDOF			
	#other		OF octet . #other ENDOF
	ENDCASE	

	MACready @ 0=
	UNTIL
	drop CR
;	
