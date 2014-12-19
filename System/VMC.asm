SingleMulti	equ	243712
CurrentVM	equ	243716
TaskControl	equ	244224
PCoverride 	equ	244736
VirtualInt	equ	245248
VMcount	equ	2				; count of available virtual machines
;
		nop
		nop
		nop
		nop
; SINGLE ( --, disable multitasking.  PAUSE instructions will be treated as a two-cycle NOP)
SINGLE.CF	zero
		#.l	SingleMulti
		store.b,rts
;
; MULTI ( --, enable multitasking)
MULTI.CF	#.b	1				; enable multitasking
		#.l	SingleMulti
		store.b,rts
;
; VM->TCREG ( n -- addr, convert a virtual machine number to the respective Task Control register)
VM->TCREG 	2*					; TaskControl registers are 16 bits wide but longword aligned
		2*
		#.l	TaskControl
		+,rts
;
; VM->PCOREG ( n -- addr, convert a virtual machine number to the respective PCoverride register)
VM->PCOREG 	2*					; PCoverride registers are 32 bits wide and longword aligned
		2*
		#.l	PCoverride
		+,rts
;
; VM->VIREG ( n -- addr, convert a virtual machine number to the respective VirtualInterrupt register)
VM->VIREG 	2*					; VirtualInterrupt registers are 32 bits wide and longword aligned
		2*
		#.l	VirtualInt
		+,rts
;
; VIRQ ( XT VM --, make a virtual interrupt request for the rask running on VM to JSR to XT when it next executes)
VIRQ.CF	jsl	VM->VIREG
		store.l				; minimum 2 cycles MUST now be allowed before pause (update nextVM, update CPUthaw)
		rts					
;
; INIT-MULTI ( -- initialize multitasking at machine switch on or reset)
INIT-MULTI.CF	#.l	TaskControl			; zero all task control registers
		dup
		dup
		#.b	VMcount 4 *
		+
		swap
		DO
			zero
			R@
			store.w			; each task control register is 16 bits wide
			#.b	4			; each task control register is longword aligned
		+LOOP		
		#.w	8000				; indicate that VM 0 is assigned by setting bit 31 = true
		store.w,rts				
;
; INIT-TASK ( -- initialize a new task - always the first code executed by a new task)
INIT-TASK.CF	resetsp
;		copy in initial stack parameters from shared memory
;		initialize user variable area
		rts
;
; RUN ( p1 p2 … pn XT -- VM# true | false, find and initialize a new VM to take n stack paramaters and execute task XT. Return VM# true if successful, or false otherwise)
;						 note that XT must be an infinite loop or must contain code to self-abort the task
RUN.CF		#.l	TaskControl
		#.l	TaskControl VMcount 4 * +
		>R
		BEGIN					; find an unassigned VM
			dup
			fetch.w
			#.w	8000			; assigned VM's have bit 31 = true
			and
		WHILE
			#.b	4			
			+				; each task control register is longword aligned
			dup
			R@
			>
			IF				; failure - all VM's checked and were assigned
				R>
				drop
				drop
				drop
; should also drop n stack parameters
				false
				rts
			THEN
		REPEAT	
		R>
		drop
		dup					( p1 p2 … pn XT reg reg)
		#.w	hex 8000
		store.l							; set the flag in the VM Task Control register to indicate that it is assigned
		#.l	TaskControl
		-
		2/
		2/					( p1 p2 … pn XT VM#n); VM#n = number of available virtual machine)
		dup					( p1 p2 … pn XT VM#n VM#n)
		jsl	WAKE.cf						; insert the new VM into the task execution list
		swap
		over					( p1 p2 … pn VM#n XT VM#n)
		jsl	VM->PCOREG					
		store.l				( p1 p2 … pn VM#n)	; the new XT will become the program counter for the new task
		#.l	INIT-TASK.CF
		over
		jsl	VM->VIREG
		store.l				( p1 p2 … pn VM#n)	; the next task will first execute a virtual interrupt to TASK-INIT.CF
		zero
		not,rts				( true)
;
; MASK@ ( addr mask --, read the data at address u and bitwise though the mask)
MASK@		swap
		fetch.l
		and,rts
;
; MASK! ( u addr mask --, store u at address addr with bitwise write-enable through the mask)
MASK!		>R		( u addr R:mask)
		dup		( u addr addr R:mask)
		fetch.l	( u addr data R:mask)
		R@		( u addr data mask R:mask)
		not		( u addr data mask~ R:mask)
		and		( u addr data~ R:mask)		; preserve only the write protected bits of the original data 
		rot		( addr data~ u R:mask)
		R>		( addr data~ u mask)
		and		( addr data~ u~)			; preseve only the write enabled bits of the new data
		or		( addr u)				; merge the original and new data
		swap
		store.l,rts
;
; GETNEXTTASK ( VM# -- read the next task number from the Task Control register for VM#)
GETNEXTTASK.CF	jsl	VM->TCREG
			#.b	hex 1F
			jsl 	MASK@				; bottom 5 bits of task control register are always the next-to-execute-task
			rts
;
; GETPRIORTASK ( VM# -- read the prior task number from the Task Control register for VM#)
GETPRIORTASK.CF	jsl	VM->TCREG
			#.w	hex 03E0			; bits 10 downto 6 of task control register point to the prior-executing task
			jsl	MASK@			
			#.b	5
			jsl	RSHIFT.CF
			rts
;
; SETNEXTTASK ( n VM# -- update the Task Control register for VM# to point to next task n)
SETNEXTTASK.CF	jsl	VM->TCREG			( n VM_TCREG)
			#.b	hex 1F				; bottom 5 bits of task control register are always the next-to-execute-task
			jsl	MASK!
			rts
;
; SETPRIORTASK ( n VM# -- update the Task Control register for VM# to point to the prior task n)
SETPRIORTASK.CF	swap
			#.b	5
			jsl	LSHIFT.CF
			swap
			jsl	VM->TCREG
			#.w	hex 03E0			; bits 10 downto 6 of task control register point to the prior-executing task
			jsl	MASK!
			rts
;		
; WAKE	( wake_VM --, wake task wake_VM by inserting it into the list of executing tasks after the current task)
WAKE.CF	#.l	CurrentVM
		fetch.b				( wake_VM current_VM)
		over
		over					( wake_VM current_VM wake_VM current_VM)
		jsl	GETNEXTTASK.CF		( wake_VM current_VM wake_VM next_VM)
		jsl	SETPRIORTASK.CF		( wake_VM current_VM)				; update backwards link from current next task
		over
		over					( wake_VM current_VM wake_VM current_VM)
		jsl	SETNEXTTASK.CF		( wake_VM current_VM)				; update forward link from current task
		over
		over
		swap					( wake_VM current_VM current_VM wake_VM)
		jsl	SETPRIORTASK.CF		( wake_VM current_VM)				; update backward link from inserted task
		jsl	GETNEXTTASK.CF		( wake_VM next_VM)
		swap					( next_VM wake_VM)
		jsl	SETNEXTTASK.CF									; update forward link from inserted task
		rts
;	
; SLEEP ( sleep_VM --, put task sleep_VM to sleep by removing it from the list of executing tasks)		
SLEEP.CF	dup					( sleep_VM sleep_VM)
		jsl	GETNEXTTASK.CF		( sleep_VM next_VM)
		swap	GETPRIORTASK.CF		( next_VM prior_VM)
		over
		over					( next_VM prior_VM next_VM prior_VM)
		jsl	SETNEXTTASK.CF		( next_VM prior_VM)					; update forward link of prior task
		swap					( prior_VM next_VM)	
		jsl	SETPRIORTASK.CF									; update backward link of next task
		rts
;
; STOP ( VM# --, deallocate task VM# and remove it from the list of currently executing tasks)
STOP.CF	jsl	VM->TCREG
		zero
		over
		#.w	hex 8000				; deallocate this task but do not disturb the next-task pointer, in case VM# is the current task
		jsl	MASK!
		jsl	SLEEP.CF				; remove this task from the list of currently executing tasks
		rts
;
		
	
		
		
		
		
	