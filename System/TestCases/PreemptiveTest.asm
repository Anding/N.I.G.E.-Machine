SingleMulti	equ	243712
Interval	equ	243720
TaskControl	equ	244224
PCoverride 	equ	244736
VirtualInt	equ	245248
;
	nop
	nop
	nop
	nop
	#.b	1			; task 0 switches to task 1
	#.l	TaskControl
	store.w
	#.l	thrd1			; task 1 PCoverride address
	#.l	PCoverride 4 +	
	store.l
	#.b	1			; enable multitasking
	#.l	SingleMulti
	store.b
	#.b	20			; enable pre-emptive multitasking
	#.l	Interval
	store.b
	nop
	#.b	hex e1
l0	1+
	1-
	pause
	bra	l0
	nop
	nop
	nop
	nop
	nop
	nop
thrd1	#.b	hex f1
	nop
	nop
l1	1+
	1-
	pause
	bra	l1