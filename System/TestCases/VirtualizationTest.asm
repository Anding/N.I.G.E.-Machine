SingleMulti	equ	243712
TaskControl	equ	244224
PCoverride 	equ	244736
VirtualInt	equ	245248
;
	nop
	nop
	nop
	nop
	#.b	1			; enable multitasking
	#.l	SingleMulti
	store.b
	nop
	nop
	pause
;	#.b	1			; task 0 switches to task 1
;	#.l	TaskControl
;	store.w
;	#.l	thrd1			; task 1 PCoverride address
;	#.l	PCoverride 4 +	
;	store.l
;	#.l	six			; Virtual Interrupt address
;	#.l	VirtualInt 4 +	
;	store.l
;	#.l	thrd0	
;	catch
;	zero	
;	drop
end	bra	end
;
thrd0	pause				; Initialize thread 1
;	#.l	VI			; Virtual Interrupt address
;	#.l	VirtualInt 4 +	
;	store.l
	nop				; Critical to allow VirtualInterrupt memory to be updated before calling pause
	nop				; minimum 2 cycles between pause - update nextVM, update CPUthaw
	pause				; Switch to thread 1 (expect a virtual interrupt to occur)
	rts
	nop
	nop
	nop
	nop
	nop
	nop
	nop
thrd1	BEGIN
		pause			; Switch to thread 0
		1+
	AGAIN
	nop
	nop
	nop
	nop
	nop
;
six	#.b	hex 0AB
	rts
;
	nop
	nop
	nop
	nop
	nop
	nop
VI	#.b	hex 0DE
	rts