SingleMulti	equ	243712
CurrentVM	equ	243716
TaskControl	equ	244224
PCoverride 	equ	244736
VirtualInt	equ	245248
sevenseg	equ	hex 03F830
VMcount	equ	32				; count of available virtual machines
;
		nop
		nop
		nop
		nop
		jsl	INIT-MULTI.CF
		zero
		#.l	TEST
		jsl	run.cf
		drop
		drop
		#.b	20
		zero
		DO
			pause
		LOOP
		#.l	double
		#.b	1
		jsl	virq.cf
l0		pause
		bra l0
;
showTC		2*
		2*
		#.l	TaskControl
		+
		fetch.l
		#.l	sevenseg
		store.l,rts		
;
; LSHIFT (x1 u -- x2)
LSHIFT.CF	BEGIN
			?dup	( x1 u true -- x1 false)
		WHILE
			1-	( x1 u')
			swap	( u' x1)
			lsl	( u' x2)
			swap	( x2 u')
		REPEAT
LSHIFT.Z	rts
;
; RSHIFT (x1 u -- x2)
RSHIFT.CF	BEGIN
			?dup	( x1 u true -- x1 false)
		WHILE
			1-	( x1 u')
			swap	( u' x1)
			lsr	( u' x2)
			swap	( x2 u')
		REPEAT
RSHIFT.Z	rts
;
; SINGLE ( --, disable multitasking.  PAUSE instructions will be treated as a two or three-cycle NOP)
SINGLE.CF	zero
		#.l	SingleMulti
		store.b,rts
;
; MULTI ( --, enable multitasking)
MULTI.CF	#.b	1
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
		#.w	hex 8000			; indicate that VM 0 is assigned by setting bit 15 = true
		swap
		store.w
		jsl	MULTI.CF			; initialize multitasking
		rts				
;
; INIT-TASK ( -- initialize a new task - always the first code executed by a new task)
INIT-TASK.CF	jsl	single.cf			; suspend multitasking
		resetsp				; reset stack pointers
		#.l	INIT-TASK.0			; copy initial stack parameters from shared memory
		fetch.b				( n)
		1-					( n~)
		2*					( 2n)
		2*					( 4n)
		#.l	INIT-TASK.1			( 4n addr)
		+					( addr~)
		BEGIN
			dup				( addr~ addr~)
			#.l	INIT-TASK.1		( addr~ addr~ addr)
			<				( addr~ flag)
			NOT				( addr~ flag~)
		WHILE
			dup				( addr~ addr~)
			fetch.l			( addr~ u)
			swap				( u addr~)
			#.b	4
			-
		REPEAT
		drop					( p1 p2 … pn)
;		jsl	USERINIT.CF			; initialize user variable area
		#.l	INIT-TASK.2			; fetch the XT of this task and place on the stack
		fetch.l				
		jsl	multi.cf			; reenable multitasking (all data is now on the local stack)
		pause					; initialization finished - run XT upon next execution
		jmp					; jump to the XT
;
INIT-TASK.0	ds.b	1				; number of stack parameters passed to new task
INIT-TASK.1	ds.l	16				; storage for 16 stack parameters passed to new task
INIT-TASK.2	ds.l	1				; XT of new task
;
; RUN ( p1 p2 … pn n XT -- VM# true | false, find and initialize a new VM to take n stack paramaters and execute task XT. Return VM# true if successful, or false otherwise)
;	note that XT must be an infinite loop or must contain code to self-abort the task
RUN.CF		jsl	single.cf						; suspend multitasking
		#.l	INIT-TASK.2			( … pn n n XT addr)
		store.l							; record the XT of the new task
		dup					( … pn n n)
		#.l	INIT-TASK.0
		store.b				( … pn n)		; record the number of stack parameters to be passed
		?dup					( … pn n n | 0)
		IF								; copy stack parameters to a shared memory area
			2*
			2*				( … pn 4n)
			#.l	INIT-TASK.1		( … pn 4n addr)
			dup				( … pn 4n addr addr)
			rot				( … pn addr addr 4n)
			+				( … pn addr addr-end)
			swap				( … pn addr-end addr)
			DO
				R@			( … pi addri)
				store.l
				#.b 	4
			+LOOP				( … pi)
		THEN
		#.l	TaskControl			( TC0)			; find an unassigned VM
		#.l	TaskControl VMcount 1- 4 * + ( TC0 TCn)
		>R					( TC0 R:TCn)
		BEGIN								
			dup				( TC TC R:TCn)
			fetch.w			( TC raw R:TCn)
			#.w	hex 8000					; assigned VM's have bit 15 = true
			and				( TC0 bit15 R:TCn)
		WHILE
			#.b	4			( TC 4 R:TCn)
			+							; each task control register is longword aligned
			dup				( TC~ TC~ R:TCn)
			R@				( TC~ TC~ TCn R:TCn)
			>				( TC~ flag R:TCn)
			IF							; failure - all VM's checked and were assigned
				R>			( TC~ TCn)
				drop			( TC~)
				drop			( )
				false			( 0)
				jsl	multi.cf				; re-enable multitasking
				rts
			THEN
		REPEAT					( TC~ R:TCn)
		R>					( VM#n_TC TCn)
		drop					( VM#n_TC)
		dup					( VM#n_TC VM#n_TC)
		#.w	hex 8000			( VM#n_TC VM#n_TC $8000)
		swap					( VM#n_TC $8000 VM#n_TC)
		store.w				( VM#n_TC) 		; set the flag in the VM Task Control register to indicate that it is assigned
		#.l	TaskControl			( VM#n_TC TC0) 
		-					( offset) 
		2/					( offset/2) 
		2/					( VM#n)		; VM#n = number of available virtual machine)
		dup					( VM#n VM#n)
		jsl	WAKE.cf			( VM#n)		; insert the new VM into the task execution list
		dup					( VM#n VM#n)
		#.l	INIT-TASK.CF			( VM#n VM#n	init-task)
		swap					( VM#n init-task VM#n)
		jsl	VM->PCOREG			( VM#n init-task VM#n_PCO)	
		store.l				( VM#n)		; the new XT will become the program counter for the new task
		false					( VM#n false)		; minimum 2 cycles MUST now be allowed before pause (update nextVM, update CPUthaw)
		not					( VM#n true)
		jsl	multi.cf						; re-enable multitasking
		pause								; switch now to task just created since RUN is not reentrant
		rts					( VM#n true)
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
		invert		( u addr data mask~ R:mask)
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
WAKE.CF	jsl	single.cf				; suspend multitasking
		#.l	CurrentVM
		fetch.b				( wake_VM current_VM)
		over
		over					( wake_VM current_VM wake_VM current_VM)
		jsl	GETNEXTTASK.CF		( wake_VM current_VM wake_VM next_VM)
		dup
		>R					( R:next_VM)
		jsl	SETPRIORTASK.CF		( wake_VM current_VM)				; update backwards link from current next task
		over
		over					( wake_VM current_VM wake_VM current_VM)
		jsl	SETNEXTTASK.CF		( wake_VM current_VM)				; update forward link from current task
		over
		over
		swap					( wake_VM current_VM current_VM wake_VM)
		jsl	SETPRIORTASK.CF		( wake_VM current_VM)				; update backward link from inserted task
		drop
		R>
		swap					( next_VM wake_VM)
		jsl	SETNEXTTASK.CF									; update forward link from inserted task
		jsl	multi.cf				; re-enable multitasking
		rts
;	
; SLEEP ( sleep_VM --, put task sleep_VM to sleep by removing it from the list of executing tasks)		
SLEEP.CF	jsl	single.cf				; suspend multitasking
		dup					( sleep_VM sleep_VM)
		jsl	GETNEXTTASK.CF		( sleep_VM next_VM)
		swap	
		jsl	GETPRIORTASK.CF		( next_VM prior_VM)
		over
		over					( next_VM prior_VM next_VM prior_VM)
		jsl	SETNEXTTASK.CF		( next_VM prior_VM)					; update forward link of prior task
		swap					( prior_VM next_VM)	
		jsl	SETPRIORTASK.CF									; update backward link of next task
		jsl	multi.cf				; re-enable multitasking		
		rts
;
; STOP ( VM# --, deallocate task VM# and remove it from the list of currently executing tasks)
STOP.CF	jsl	single.cf				; suspend multitasking	
		dup
		jsl	SLEEP.CF				; remove this task from the list of currently executing tasks
		jsl	VM->TCREG
		zero
		swap
		#.w	hex 8000				; deallocate this task but do not disturb the next-task pointer, in case VM# is the current task
		jsl	MASK!
		jsl	multi.cf				; re-enable multitasking		
		rts
;
; THIS-SLEEP ( --, put the current task to sleep and switch to the next task)
THIS-SLEEP.CF	#.l	CurrentVM
		fetch.b
		jsl	SLEEP.cf
		pause						; current task must sleep to preserve intregity of next task list
		rts
; 
; THIS-STOP ( --, stop the current task and switch to the next task)
THIS-STOP.CF	#.l	CurrentVM
		fetch.b
		jsl	STOP.cf
		pause						; current task must sleep to preserve intregity of next task list
		rts
;
; THIS-VM ( -- VM#, return the number of the current virtual machine)
THIS-VM.CF	#.l	CurrentVM
		fetch.b,rts
;	
TEST		#.b	1
		BEGIN
			dup
			#.l 	sevenseg
			store.l
			1+
			nop
			nop
			nop
			nop
			nop
			nop
			nop
			nop
			nop
			pause
		AGAIN
reset		drop
		#.b 10
		rts
double		2*
		rts
		