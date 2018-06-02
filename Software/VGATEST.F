260236 constant regbase		\ hardware register base

variable Ha
variable Hb 
variable Hc
variable Hd
variable Va
variable Vb
variable Vc
variable Vd

: set
	create ( visible front sync whole <NAME> --)
	, , , , 
	does> ( -- visible front sync back whole) 
	dup 3 cells +  
	do i @ -1 cells +loop
; 

: prep { visible front sync whole -- a b c d}
	\ use NIGE local variables
	whole ( a)
	visible front + ( b)
	visible front + sync + ( c)
	visible 1- ( d)
;

640 16 96 800 set H1
480 10 2 525 set V1
800 56 120 1040 set H2
600 37 6 666 set V2
1024 24 136 1328 set H3
768 3 6 806 set V3
1920 128 350 2592 set H4
1080 4 5 1125 set V4


: loadH ( a b c d --) 
	regbase dup 3 cells +
	do i ! -1 cells +loop
;

: loadV ( a b c d --)
	regbase dup 4 cells +  swap 7 cells +
 	do i ! -1 cells +loop
;

: VGA1 H1 prep loadH V1 prep loadV 1 VGA ;
: VGA2 H2 prep loadH V2 prep loadV 2 VGA ;
: VGA3 H3 prep loadH V3 prep loadV 3 VGA ;
: VGA4 H4 prep loadH V4 prep loadV 4 VGA ;










