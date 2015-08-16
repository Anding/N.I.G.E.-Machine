; N.I.G.E Machine FORTH system software
;
; Copyright and license
;
; The N.I.G.E machine, its design and its source code are Copyright (C) 2012-2014 by Andrew Richard Read and dual licensed.
;    
; (1) For commercial or proprietary use you must obtain a commercial license agreement with Andrew Richard Read (andrew81244@outlook.com)
;    
; (2) You can redistribute the N.I.G.E. Machine, its design and its source code and/or modify it under the terms of the GNU General Public 
; License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.  
;
; The N.I.G.E Machine is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of 
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of 
; the GNU General Public License along with this repository.  If not, see <http://www.gnu.org/licenses/>.
;
; ---------------------------------------------------------------------------------------------
		cmd	asmtop			; forget previous label and other definitions
		cmd	MARKER asmtop
;
; -----------------------------------------------------------------------------------------------
; ASSEMBLER CONSTANTS
; -----------------------------------------------------------------------------------------------
; **** SYSTEM HARDWARE ****
;
system-freq	equ	950000000
;
; **** MULTITASKING  ****
;
SingleMulti	equ	hex 03f000 ;258048
CurrentVM	equ	hex 03f004 ;258052
Interval	equ	hex 03f008 ;258056
TaskControl	equ	hex 03f200 ;258560
PCoverride 	equ	hex 03f400 ;259072
VirtualInt	equ	hex 03f600 ;259584
VMcount	equ	32		; count of available tasks
;
; **** HARDWARE REGISTERS ****
;
TEXT_ZERO	equ	hex 03f800
BACKGROUND	equ	hex 03f808
mode		equ	hex 03f80c
Interlace	equ	hex 03f84c
charWidth	equ	hex 03f850
charHeight	equ	hex 03f854
VGArows	equ	hex 03f858
VGAcols	equ	hex 03f85c
RS232rx	equ	hex 03f810
RS232tx	equ	hex 03f814
RS232DIVIDE	equ	hex 03f818
HWstatus	equ	hex 03f81c
HWmask_TBE	equ	02
PS2rx		equ	hex 03f820
CLKcounter	equ	hex 03f824
MScounter	equ	hex 03f828
intmask	equ	hex 03f82c		
intmask_MS	equ	16
sevenseg	equ	hex 03f830
SPI.data	equ	hex 03f838  
SPI.control	equ	hex 03f83C  
SPI.status	equ	hex 03f840  
SPI.divide	equ	hex 03f844 
VBLANK		equ	hex 03f848 
;
; **** MEMORY MAP ****
;
USERRAM	equ	hex 03e000		; USER RAM area
SSTACK		equ	hex 03b000		; Subroutine stack
local0		equ	SSTACK			; local variables on the subroutine stack
local1		equ	SSTACK 4 +
local2		equ	SSTACK 8 +
local3		equ	SSTACK 12 +
ESTACK		equ	hex 03b080		; Exception stack
;
SRAMSIZE	equ	124 1024 * 512 -	; Amount of SRAM in bytes
USERRAMSIZE	equ	2048			; Amount of USER RAM in bytes
;
RSrxBUF	equ	124 1024 * 512 -	; RS232 buffer (256 bytes) location
PSBUF		equ	124 1024 * 256 -	; PS/2 keyboard buffer (256 bytes) location
;
_PAD		equ	USERRAM 1024 +	; PAD location
_STRING	equ	_PAD			; buffer for interpret mode string storage (e.g. S")
_LOCAL.buf	equ	_PAD			; buffer for local variable names during compilation
_COMPILE	equ	_PAD 256 + 		; buffer for compiling LEAVE references in DO loops
_PADEND	equ	_PAD 511 +		; last PAD character - picture numeric output builds downwards from here
_input_buff	equ	USERRAM 1536 +	; default ACCEPT input buffer location
_input_size	equ	256			; default ACCEPT input buffer size
STRINGMAX	equ	480			; area within the PAD that can be used for interpret mode strings		
;
SCREENWORDS	equ	135 240 2  * *	; number of WORDS in the screen buffer (135 rows * 240 cols * 2 screens)
_TEXT_ZERO	equ	hex 040000				; default text memory location
_TEXT_END	equ	SCREENWORDS 2* _TEXT_ZERO +		; one byte beyond the text memory location
_FAT.buf	equ	_TEXT_END				; FAT 512 byte storage space location
_FAT.buffat	equ	_TEXT_END 512 +			; FAT 512 byte storage space location for file allocation table
_END		equ	_TEXT_END 1024 +			; HEAP location
;
; **** FORTH LANGUAGE CONSTANTS ****
;
EOL		equ	10			; line separator = ASCII 10
~EOL		equ	13			; ignore this character, not line separator = ASCII 13
;
; .NF flags
PRECEDENCE	equ	128
IMMED		equ	64			; IMMEDIATE
SMDGE		equ	32			; SMUDGE
;
; .SF flags (hi byte)
MUSTINLINE	equ	hex 8000		; Force inline compilation
NOINLINE	equ	hex 4000		; Prohibit inline compilation
;
; **** N.I.G.E. MACHINE OPCODES ****
;
opDROP		equ	1
op#.B		equ	52
op#.W		equ	53
op#.L		equ	54
opJMP		equ	55
opJSL		equ	56
opJSR		equ	57
opRTS		equ	64
opOVER=	equ	4 256 * 22 +
opBEQ		equ	128 256 *
opBRA		equ	192 256 *
opFETCH.L	equ	45
opSTORE.L	equ	46
;
; ----------------------------------------------------------------------------------------------
; Interrupt vectors
; -----------------------------------------------------------------------------------------------
;
V.RST		BRA	start.CF	 			; RESET
V.TRAP		RTI
		NOP						; TRAP
V.RDA		BRA	RDA					; RS232 RDA
V.TBE		BRA	TBE					; RS232 TBE
V.PS2		BRA	PS2					; PS2
V.MS		RTI						; MS (formerly used for timeout)
		NOP
		RTI
		NOP
		RTI
		NOP
		RTI
		NOP
		RTI
		NOP
		RTI
		NOP
		RTI
		NOP
		RTI
		NOP
		RTI
		NOP
		RTI
		NOP
		RTI
		NOP		
		RTI
		NOP		
;	
; ----------------------------------------------------------------------------------------------
; Interrupt handlers
; -----------------------------------------------------------------------------------------------
;
RDA		#.w 	RSrxWPOS	; buffer write position
		fetch.b
		1+
		#.b	hex ff		; constrain to 256 byte length
		and
		dup			 
		#.w 	RSrxRPOS	; buffer read position
		fetch.b
		<>
		IF			; write equals read => skip input
			dup			
			#.w RSrxWPOS
			store.b		; RS_wr
			#.l RSrxBUF		; RS_wr RSrxBUF_S0
			+			; RSrxBUF_S0+RS_wr
			#.l RS232rx	
			fetch.b
			swap			; RS232rx_S0 RSrxBUF_S0+RS_wr
			store.b
			zero			; dummy to balance stack
		THEN
		drop			; needed for skip logic
		rti
RSrxWPOS	dc.b	hex ff
RSrxRPOS	dc.b	hex ff
;
TBE		#.l	RStxCNT
		fetch.l	
		?dup
		IF			; remaining char count not yet zero
			1-			(count-1 )
			#.l	RStxCNT	(count addr)
			store.l
			#.l 	RStxBUF	(buf&)
			dup			(buf& buf&)
			fetch.l		(buf& addr)
			1+			(buf& addr+1)
			dup			(buf& addr+1 addr+1)
			rot			(addr+1 addr+1 buf&)
			store.l		(addr+1)
			fetch.b		(char)
			#.l RS232tx		(char RS232tx)
			store.b
		THEN
TBE.Z		rti
RStxCNT	dc.l	hex 00
RStxBUF	dc.l	hex 00
;
PS2		#.l 	PS2rx	
		fetch.b			( raw)
		jsl 	PS2DECODE.CF		( char, decode the raw character)			
		?dup				; process only completed keystrokes
		IF
			#.w 	PSWPOS		; buffer write position
			fetch.b
			1+
			#.b	hex ff		; constrain to 256 byte length
			and			( char n)	
			dup			( char n n)
			#.w 	PSWPOS		; update buffer write position
			store.b		( char n)
			#.l	PSBUF		; add to base address
			+			( char addr)
			store.b		; save key in the buffer
		THEN
		rti
PSWPOS		dc.b	hex ff
PSRPOS		dc.b	hex ff
;
;MSIRQ		#.w	MS.TIMEOUT
;		fetch.l
;		?dup
;		IF
;			1-
;			dup
;			#.w MS.TIMEOUT
;			store.l
;			0=
;			IF
;				#.l	MS.ERR
;				THROW
;			THEN
;		THEN
;		rti
;MS.TIMEOUT	dc.l	hex 00
;MS.ERR		dc.b	15
;		dc.s	Timeout expired
;
; -----------------------------------------------------------------------------------------------
; Boot code (within branch distance from 0)
; -----------------------------------------------------------------------------------------------
;	
START.CF	jsl	ESTACKINIT.CF
		jsl	USERINIT.CF
		jsl	INIT-MULTI.CF
; configure the screen
		jsl	SCRSET.CF
		jsl	CLS.CF
		#.b	6
		#.w	INK
		store.b
; Power-on message
		#.w	START.0	
		#.b	START.1 START.0 - 
		jsl	TYPE.CF
		jsl	UNUSED.CF	; Show free bytes
		jsl 	UDOT.CF
		#.w	START.1	
		#.b	12
		jsl	TYPE.CF
		#.b 	100
		zero
		jsl	QUIT.CF	; QUIT will not return but JSL is more efficient than #.W JMP
;
START.0	dc.s	******************************		
		dc.b 	EOL
		dc.s	*** N.I.G.E. Machine FORTH ***
		dc.b	EOL
		dc.s	******************************		
		dc.b 	EOL		
		dc.b	EOL
		dc.s	PSDRAM 16 MB, SRAM
		dc.b	32
START.1	dc.s	bytes free
		dc.b	EOL EOL
;
; ----------------------------------------------------------------------------------------------
; WORLDLIST table
; ----------------------------------------------------------------------------------------------
;
LAST-NF	dc.l	GET-ENTRY.NF		; must be initialize to the NF of the last word in the dictionary
		dc.l	STUB.NF		; all other worldlists are initialized to the stub word
		dc.l	STUB.NF
		dc.l	STUB.NF
		dc.l	STUB.NF
		dc.l	STUB.NF
		dc.l	STUB.NF
		dc.l	STUB.NF
		dc.l	STUB.NF
		dc.l	STUB.NF
		dc.l	STUB.NF
		dc.l	STUB.NF
		dc.l	STUB.NF
		dc.l	STUB.NF		; 14 wordlists are available
HERE_		dc.l	END			; storage location of the variable HERE
HERE1		dc.l	_END			; HERE1 tracks PSDRAM space allocated by BUFFER:
;
; Preceeding 64 byte area is saved by MARKER at compile time and restored by MARKER at run time		
;
; ----------------------------------------------------------------------------------------------
; Start of FORTH dictionary
; ----------------------------------------------------------------------------------------------
;
; stub word to mark the end of the dictionary
		dc.l	0
STUB.NF	dc.b	0
		dc.w	1
		rts
; ----------------------------------------------------------------------------------------------
; Virtual Machine Monitor
; ----------------------------------------------------------------------------------------------
;
; SINGLE ( --, disable multitasking.  PAUSE instructions will be treated as a two or three-cycle NOP)
SINGLE.LF	dc.l	STUB.NF
SINGLE.NF	dc.b	6 128 +
		dc.s	SINGLE
SINGLE.SF	dc.w	SINGLE.Z SINGLE.CF del
SINGLE.CF	zero
		#.l	SingleMulti
SINGLE.Z	store.b,rts
;
; MULTI ( --, enable multitasking)
MULTI.LF	dc.l	SINGLE.NF
MULTI.NF	dc.b	5 128 +
		dc.s	MULTI
MULTI.SF	dc.w	MULTI.Z MULTI.CF del
MULTI.CF	#.b	1
		#.l	SingleMulti
MULTI.Z	store.b,rts
;
; PREEMPTIVE ( n --, enable preeemptive multitasking with n instructions per task switch)
PREEMPTIVE.LF	dc.l	MULTI.NF
PREEMPTIVE.NF	dc.b	10 128 +
		dc.s	PREEMPTIVE
PREEMPTIVE.SF	dc.w	PREEMPTIVE.Z PREEMPTIVE.CF del
PREEMPTIVE.CF	jsl	checksuspend		( n v)		; suspend multitasking
		swap				( v n)
		#.l	Interval		( v n interval)
		store.w			( v)
		#.l	SingleMulti				; re-enable multitasking to its prior state
PREEMPTIVE.Z	store.b,rts		
;
;CHECKSUSPEND ( -- v, get the multitasking status then suspend multitasking)
CHECKSUSPEND	#.l	SingleMulti
		dup				( reg reg)
		fetch.b			( reg v)
		zero				( reg v 0)
		rot				( v 0 reg)
		store.b,rts			( v)
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
VIRQ.LF	dc.l	PREEMPTIVE.NF
VIRQ.NF	dc.b	4 128 +
		dc.s	VIRQ
VIRQ.SF	dc.w	VIRQ.Z VIRQ.CF del
VIRQ.CF	jsl	VM->VIREG
		store.l				; minimum 2 cycles MUST now be allowed before pause (update nextVM, update CPUthaw)
VIRQ.Z		rts					
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
		jsl	USERINIT.CF			; initialize user variable area
		jsl	ESTACKINIT.CF			; initialize exception stack
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
RUN.LF		dc.l	VIRQ.NF
RUN.NF		dc.b	3 128 +
		dc.s	RUN
RUN.SF		dc.w	RUN.Z RUN.CF del NOINLINE +	
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
RUN.Z		rts					( VM#n true)
;
; MASK@ ( addr mask --, read the data at address u and bitwise though the mask)
MASK@.LF	dc.l	RUN.NF
MASK@.NF	dc.b	5 128 +
		dc.s	MASK@
MASK@.SF	dc.w	MASK@.Z MASK@ del	
MASK@		swap
		fetch.l
MASK@.Z	and,rts
;
; MASK! ( u addr mask --, store u at address addr with bitwise write-enable through the mask)
MASK!.LF	dc.l	MASK@.NF
MASK!.NF	dc.b	5 128 +
		dc.s	MASK!
MASK!.SF	dc.w	MASK!.Z MASK! del	
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
MASK!.Z	store.l,rts
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
WAKE.LF	dc.l	MASK!.NF
WAKE.NF	dc.b	4 128 +
		dc.s	WAKE
WAKE.SF	dc.w	WAKE.Z WAKE.CF del	
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
WAKE.Z		rts
;	
; SLEEP ( sleep_VM --, put task sleep_VM to sleep by removing it from the list of executing tasks)
SLEEP.LF	dc.l	WAKE.NF
SLEEP.NF	dc.b	5 128 +
		dc.s	SLEEP
SLEEP.SF	dc.w	SLEEP.Z SLEEP.CF del		
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
SLEEP.Z	rts
;
; STOP ( VM# --, deallocate task VM# and remove it from the list of currently executing tasks)
STOP.LF	dc.l	SLEEP.NF
STOP.NF	dc.b	4 128 +
		dc.s	STOP
STOP.SF	dc.w	STOP.Z STOP.CF del
STOP.CF	jsl	single.cf				; suspend multitasking	
		dup
		jsl	SLEEP.CF				; remove this task from the list of currently executing tasks
		jsl	VM->TCREG
		zero
		swap
		#.w	hex 8000				; deallocate this task but do not disturb the next-task pointer, in case VM# is the current task
		jsl	MASK!
		jsl	multi.cf				; re-enable multitasking		
STOP.Z		rts
;
; THIS-SLEEP ( --, put the current task to sleep and switch to the next task)
THIS-SLEEP.LF	dc.l	STOP.NF
THIS-SLEEP.NF	dc.b	10 128 +
		dc.s	THIS-SLEEP
THIS-SLEEP.SF	dc.w	THIS-SLEEP.Z THIS-SLEEP.CF del
THIS-SLEEP.CF	#.l	CurrentVM
		fetch.b
		jsl	SLEEP.cf
		pause						; current task must sleep to preserve intregity of next task list
THIS-SLEEP.Z	rts
; 
; THIS-STOP ( --, stop the current task and switch to the next task)
THIS-STOP.LF	dc.l	THIS-SLEEP.NF
THIS-STOP.NF	dc.b	9 128 +
		dc.s	THIS-STOP
THIS-STOP.SF	dc.w	THIS-STOP.Z THIS-STOP.CF del
THIS-STOP.CF	#.l	CurrentVM
		fetch.b
		jsl	STOP.cf
		pause						; current task must sleep to preserve intregity of next task list
THIS-STOP.Z	rts
;
; THIS-VM ( -- VM#, return the number of the current virtual machine)
THIS-VM.LF	dc.l	THIS-STOP.NF
THIS-VM.NF	dc.b	7 128 +
		dc.s	THIS-VM
THIS-VM.SF	dc.w	THIS-VM.Z THIS-VM.CF del
THIS-VM.CF	#.l	CurrentVM
THIS-VM.Z	fetch.b,rts
;
; PAUSE ( --, suspend this task and transfer execution to the next task)
PAUSE.LF	dc.l	THIS-VM.NF
PAUSE.NF	dc.b	5 128 +
		dc.s	PAUSE
PAUSE.SF	dc.w	1 MUSTINLINE +
PAUSE.CF	pause,rts
;
; ACQUIRE ( addr --, acquire the binary semaphore at addr or wait in a busy loop until it becomes free
ACQUIRE.LF	dc.l	PAUSE.NF
ACQUIRE.NF	dc.b	7 128 +
		dc.s	ACQUIRE
ACQUIRE.SF	dc.w	ACQUIRE.Z ACQUIRE.CF del
ACQUIRE.CF	BEGIN			( addr)
			jsl	CHECKSUSPEND	( addr v)		; suspend multitasking
			swap			( v addr)				
			dup			( v addr addr)
			jsl	free.cf	( v addr flag)
			0=			( v addr flag~)
		WHILE			( v addr)		
			swap			( addr v)
			#.l	SingleMulti				; re-enable multitasking to its prior state
			store.b			
			pause			( addr)		
		REPEAT			( v addr)
		#.l	CurrentVM
		fetch.b		( v addr VM)
		#.b	255
		xor
		swap			( v VM~ addr)
		store.b		( v)
		#.l	SingleMulti				; re-enable multitasking to its prior state
ACQUIRE.Z	store.b,rts		
;
; RELEASE ( addr --, release the binary semaphore at addr)
RELEASE.LF	dc.l	ACQUIRE.NF
RELEASE.NF	dc.b	7 128 +
		dc.s	RELEASE
RELEASE.SF	dc.w	RELEASE.Z RELEASE.CF del
RELEASE.CF	jsl	CHECKSUSPEND	( v)			; suspend multitasking
		swap			( v addr)				
		dup
		jsl	free.cf	( v addr flag)
		IF			( v addr)		; either this task has the semaphore or it is already free
			zero			( v addr 0)
			swap			( v 0 addr)
			store.b		( v)
		ELSE						; this task has not acquired the semaphore
			drop			( v)
		THEN
		#.l	SingleMulti				; re-enable multitasking to its prior state
RELEASE.Z	store.b,rts
;
; FREE ( addr -- flag, check if the binary semaphore at addr is available to this task and return true, or otherwise return false)
FREE.CF	fetch.b		( v) 			;binary semaphores are byte variables
		dup			( v v)
		0=			( v flag0)		; no task has the semaphore
		swap			( flag0 v)
		#.l	CurrentVM
		fetch.b		( flag0 v VM)
		#.b	255
		xor			( flag0 v VM~)
		=			( flag0 flag=)	; this task already has the semaphore
		or,rts			( flag)
; 
; ----------------------------------------------------------------------------------------------
; Low level hardware control
; ----------------------------------------------------------------------------------------------
;
; **** MILLISECOND TIMER ****
;
; TIMEOUT ( n --, set a reset timer for n milliseconds)
;TIMEOUT.LF	dc.l	PAUSE.NF
;TIMEOUT.NF	dc.b	7 128 +
;		dc.b	char T char U char O char E char M char I char T
;TIMEOUT.SF	dc.w	TIMEOUT.Z TIMEOUT.CF del
;TIMEOUT.CF	?dup
;		IF
;			#.w	MS.TIMEOUT
;			store.l
;			#.l	intmask
;			fetch.l
;			#.b	intmask_MS
;			or
;		ELSE
;			#.l	intmask
;			fetch.l
;			#.b	intmask_MS
;			invert
;			and
;		THEN
;		#.l	intmask
;TIMEOUT.Z	store.l,rts	
;		
; MS ( n --, wait for n ms)
MS.LF		dc.l	RELEASE.NF
MS.NF		dc.b	2 128 +
		dc.s	MS
MS.SF		dc.w	MS.Z MS.CF del
MS.CF		>R			( R:n)
		#.l	MScounter
		fetch.l		( start R:n)
			BEGIN		
				pause
				#.l	MScounter	
				fetch.l		( start now R:n)
				over			( start now start R:n)
				-			( start m R:n)
				R@			( start m n R:n)
				U>			( start R:n)
			UNTIL
		R>
		drop
MS.Z		drop,rts
;
; COUNTER ( n --, current count of the rolling 32 bit MS counter)
COUNTER.LF		dc.l	MS.NF
COUNTER.NF		dc.b	7 128 +
			dc.s	COUNTER
COUNTER.SF		dc.w	COUNTER.Z COUNTER.CF del
COUNTER.CF		#.l	MScounter
COUNTER.Z		fetch.l,rts
;
; **** RS232 ****
;
SEMIT.LF	dc.l	COUNTER.NF		
SEMIT.NF	dc.b	5 128 +	
		dc.b 	char T char I char M char E char S
SEMIT.SF	dc.w 	SEMIT.Z SEMIT.CF del 	
SEMIT.CF	#.l	sem-RS232
		jsl	acquire.cf
		BEGIN		; check STYPE is not already in progress	
			pause
			#.l	RStxCNT
			fetch.l
			0=
		UNTIL
		BEGIN		; wait for TBE
			pause
			#.l HWstatus
			fetch.b	
			#.b HWmask_TBE
			and			; TBE bit
		UNTIL		
		#.l RS232tx
		store.b
		#.l	sem-RS232
		jsl	release.cf
SEMIT.Z	rts
;	
SKEY?.LF	dc.l 	SEMIT.NF
SKEY?.NF	dc.b	5 128 +
		dc.b	char ? char Y char E char K char S
SKEY?.SF	dc.w	SKEY?.Z SKEY?.CF del
SKEY?.CF	#.l	sem-RS232
		jsl	acquire.cf
		#.w RSrxWPOS	
		fetch.b
		#.w RSrxRPOS
		fetch.b
		<>
		#.l	sem-RS232
		jsl	release.cf		
SKEY?.Z	rts
;
SKEY.LF	dc.l	SKEY?.NF
SKEY.NF	dc.b	4 128 +
		dc.b	char Y char E char K char S
SKEY.SF	dc.w 	SKEY.Z SKEY.CF del
		
SKEY.CF	#.l	sem-RS232
		jsl	acquire.cf
		BEGIN
			jsl	SKEY?.CF
			pause
		UNTIL
		#.w 	RSrxRPOS
		dup			(rx& rx&)
		fetch.b		(rx& rx)
		1+			(rx& rx+1)
		#.b	255
		and						; maintain byte width
		dup			(rx& rx+1 rx+1)
		rot			(rx+1 rx+1 rx&)
		store.b		(rx+1)
		#.l 	RSrxBUF	(rx+1 addr)
		+			(addr+rx+1)
		fetch.b		(char)
		#.l	sem-RS232
		jsl	release.cf
SKEY.Z		rts
;
; STYPE	(c-addr n --, type a string to RS232_S0)
STYPE.LF	dc.l	SKEY.NF
STYPE.NF	dc.b	5 128 +
		dc.b 	char E char P char Y char T char S
STYPE.SF	dc.w	STYPE.Z STYPE.CF del
STYPE.CF	#.l	sem-RS232
		jsl	acquire.cf
		?dup				( c-addr n true | c-addr false)			
		IF			; check not zero length string
; EMIT the first character
			over			( c-addr n c-addr)
			fetch.b		( c-addr n char)
			jsl	SEMIT.CF	( c-addr n)			
; check length of remaining string
			1-			( c-addr n-1)
			?dup			( c-addr n-1 true | c-addr false)
			IF			; check more characters remaining
; write to TBE interrupt handler
				#.w RStxCNT		( c-addr n-1 RStxCNT)
				store.l		( c-addr 
				#.w RStxBUF		( c-addr RStxBUF)
				store.l		( )
				zero			( dummy)
			THEN
		THEN
		drop		( )
		#.l	sem-RS232
		jsl	release.cf
STYPE.Z	rts
;	
; SZERO ( --) reposition the RS232 read buffer
SZERO.LF	dc.l	STYPE.NF
SZERO.NF	dc.b	5 128 +
		dc.b	char O char R char E char Z char S
SZERO.SF	dc.w	SZERO.Z SZERO.CF del
SZERO.CF	#.l	sem-RS232
		jsl	acquire.cf
		#.b	hex ff
		dup
		#.w	RSrxWPOS
		store.b
		#.w	RSrxRPOS
		store.b
		#.l	sem-RS232
		jsl	release.cf		
SZERO.Z	rts
;
; BAUD ( rate --, set the baud rate)
BAUD.LF	dc.l	SZERO.NF
BAUD.NF	dc.b	4 128 +
		dc.b	char D char U char A char B
BAUD.SF	dc.w	BAUD.Z BAUD.CF del
BAUD.CF	#.l	sem-RS232
		jsl	acquire.cf
		#.l	system-freq		( system freq)
		swap				( freq baud)
		1+
		divu
		nip				( divide)
		#.l	RS232DIVIDE
		store.l
		#.l	sem-RS232
		jsl	release.cf			
BAUD.Z		rts
;
; **** KEYBOARD ****
;
KKEY?.LF	dc.l 	BAUD.NF
KKEY?.NF	dc.b	5 128 +
		dc.b 	char ? char Y char E char K char K
KKEY?.SF	dc.w	KKEY?.Z KKEY?.CF del
KKEY?.CF	#.l	sem-keyboard
		jsl 	acquire.cf
		#.w 	PSWPOS	
		fetch.b
		#.w 	PSRPOS
		fetch.b
		<>
		#.l	sem-keyboard
		jsl 	release.cf		
KKEY?.Z	rts
;
KKEY.LF	dc.l	KKEY?.NF
KKEY.NF	dc.b	4 128 +
		dc.b	char Y char E char K char K
KKEY.SF	dc.w 	KKEY.Z KKEY.CF del
KKEY.CF	#.l	sem-keyboard
		jsl 	acquire.cf
		BEGIN
			pause
			jsl	KKEY?.CF
		UNTIL
		#.w 	PSRPOS
		dup			(rx& rx&)
		fetch.b		(rx& rx)
		1+			(rx& rx+1)
		#.b	hex ff
		and						; maintain 256 char width
		dup			(rx& rx+1 rx+1)
		rot			(rx+1 rx+1 rx&)
		store.b		(rx+1)
		#.l 	PSBUF		(rx+1 addr)
		+			(addr+rx+1)
		fetch.b		(char)
		#.l	sem-keyboard
		jsl 	release.cf	
KKEY.Z		rts
; PS2DECODE ( raw -- ascii)
PS2FLAGS	dc.b	0						; modifier flags
SHIFT_FLAG 	EQU 	1
CAPS_FLAG	EQU 	2
ALT_FLAG	EQU	4
CTRL_FLAG	EQU	8
CTRL_STATE	EQU	16
SPECIAL_FLAG	EQU	32
UP_FLAG	EQU	64
;
PS2DECODE.LF	dc.l	KKEY.NF
PS2DECODE.NF	dc.b	9 128 +
		dc.b	char E char D char O char C char E char D char 2 char S char P
PS2DECODE.SF	dc.w	PS2DECODE.Z PS2DECODE.CF del
PS2DECODE.CF	#.b	hex 12	( raw 12)			; LEFT SHIFT
		over		( raw 12 raw)
		=		( raw flag)
		over		( raw flag raw)
		#.b	hex 59	( raw flag raw 59)		; RIGHT SHIFT
		=		( raw flag flag)
		or		( raw flag)
		IF
			drop
			zero
			#.b	SHIFT_FLAG	( zero SHIFT)
		ELSE
			
			#.b	hex 11		( raw 11)		; ALT
			over			( raw 11 raw)
			=			( raw flag)
			IF
				drop
				zero
				#.b	ALT_FLAG	( zero ALT)
			ELSE
				#.b	hex 14		( raw 14)		; CTRL
				over			( raw 14 raw)
				=			( raw flag)
				IF
					drop
					zero
					#.b	CTRL_FLAG	( zero CTRL)
				ELSE
					#.b	hex 58		( raw 58)		; CAPS LOCK
					over			( raw 58 raw)
					=			( raw flag)
						IF
							drop
							zero
							#.b	CAPS_FLAG	( zero CAPS)
						ELSE
							#.b	hex E0		( raw E0)		; SPECIAL
							over			( raw E0 raw)
							=			( raw flag)						
						IF
							drop
							zero
							#.b	SPECIAL_FLAG	( SPECIAL)
						ELSE						
							#.b	hex F0		( raw F0)		; UP
							over			( raw F0 over)
							=			( raw flag)
							IF						
								drop
								zero
								#.b	UP_FLAG	( zero UP)							
							ELSE				( raw)		; process keystroke	
								#.w	PS2FLAGS
								fetch.b		( raw FLAGS)
								dup			( raw FLAGS FLAGS)
								#.b	UP_FLAG
								and			( raw FLAGS UPtest)
								IF						; ignore if an upstroke
									drop
									drop
									zero
								ELSE			( raw FLAGS)		; lookup table reference
									swap		( FLAGS raw)
									over		( FLAGS raw FLAGS)
									#.b	SHIFT_FLAG CAPS_FLAG or
									and		( FLAGS raw ifShiftCaps)
									dup
									#.b	1
									=
									swap
									#.b	2
									=
									or
									IF						
										#.w	PS2ASCII 118 +
									ELSE										
										over
										#.b	SPECIAL_FLAG
										and	( flags raw ifSpecial)
										IF
											dup
											#.b 74
											=
											IF
												#.w	PS2ASCII 138 +
											ELSE
												dup
												#.b	90
												=
												IF
													#.w	PS2ASCII 123 +
												ELSE
													#.w	PS2ASCII 109 +
												THEN
											THEN
										ELSE
											#.w	PS2ASCII
										THEN
									THEN
									+			
									fetch.b		( flags ASCII)	
									over			( flags ASCII flags)	; CTRL modifier
									#.b	CTRL_FLAG	
									and			( flags ASCII ifCTRL)
									IF
										#.b	96
										-		( flags ASCII')
									THEN
									swap			( ASCII flags)	; ALT modifier
									#.b	ALT_FLAG	
									and			( ASCII ifCTRL)
									IF
										#.b	128
										+		( ASCII')
									THEN	
									dup			( ASCII ASCII)	; check validity
									0<			( ASCII flag)
									IF
										drop
										zero
									THEN
								THEN
								zero			( ASCII zero)
							THEN
						THEN
					THEN
				THEN
			THEN
		THEN			( zero flag)		; process a modifier
		#.w	PS2FLAGS				; update modifier flags
		dup			( flag &FLAGS &FLAGS)
		>R			( flag &FLAGS R:&FLAGS)
		fetch.b		( flag FLAGS)
		#.b	SPECIAL_FLAG 	invert			; remove prior SPECIAL flag
		and			( flag FLAGS')					
		#.b	UP_FLAG					; test if last code was UP
		over
		and			( flag FLAGS testUP)
		IF							; switch off modifier when key UP
			over		( flag FLAGS flag)
			#.b	CAPS_FLAG
			and
			IF						; ignore caps	
				nip		( FLAGS)
			ELSE
				swap		( FLAGS flag)
				invert		( FLAGS ~flag)		; remove modifier
				and		( FLAGS')				
			THEN
				#.b		UP_FLAG invert		; remove prior UP flag
				and		( FLAGS')
		ELSE							; switch on modifier otherwise
			over		( flag FLAGS flag)
			#.b	CAPS_FLAG
			and
			IF						; toggle caps
				xor		( FLAGS')
			ELSE						; set other modifiers
				or		( FLAGS')
			THEN
		THEN
		R>			( FLAGS' &FLAGS)
		store.b	
PS2DECODE.Z		rts
;
PS2ASCII	CMD			; label here but do nothing    
 dc.b 0
 dc.b 22
 dc.b 0
 dc.b 18
 dc.b 16
 dc.b 14
 dc.b 15
 dc.b 25
 dc.b 0
 dc.b 23
 dc.b 21
 dc.b 19
 dc.b 17
 dc.b 9
 dc.b 96
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 113
 dc.b 49
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 122
 dc.b 115
 dc.b 97
 dc.b 119
 dc.b 50
 dc.b 0
 dc.b 0
 dc.b 99
 dc.b 120
 dc.b 100
 dc.b 101
 dc.b 52
 dc.b 51
 dc.b 0
 dc.b 0
 dc.b 32
 dc.b 118
 dc.b 102
 dc.b 116
 dc.b 114
 dc.b 53
 dc.b 0
 dc.b 0
 dc.b 110
 dc.b 98
 dc.b 104
 dc.b 103
 dc.b 121
 dc.b 54
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 109
 dc.b 106
 dc.b 117
 dc.b 55
 dc.b 56
 dc.b 0
 dc.b 0
 dc.b 44
 dc.b 107
 dc.b 105
 dc.b 111
 dc.b 48
 dc.b 57
 dc.b 0
 dc.b 0
 dc.b 46
 dc.b 47
 dc.b 108
 dc.b 59
 dc.b 112
 dc.b 45
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 39
 dc.b 0
 dc.b 91
 dc.b 61
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 10
 dc.b 93
 dc.b 0
 dc.b 92
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 8
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 6
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 46
 dc.b 5
 dc.b 0
 dc.b 7
 dc.b 4
 dc.b 27
 dc.b 0
 dc.b 24
 dc.b 43
 dc.b 0
 dc.b 45
 dc.b 42
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 20
 dc.b 126
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 81
 dc.b 33
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 90
 dc.b 83
 dc.b 65
 dc.b 87
 dc.b 64
 dc.b 0
 dc.b 0
 dc.b 67
 dc.b 88
 dc.b 68
 dc.b 69
 dc.b 36
 dc.b 35
 dc.b 0
 dc.b 0
 dc.b 32
 dc.b 86
 dc.b 70
 dc.b 84
 dc.b 82
 dc.b 37
 dc.b 0
 dc.b 0
 dc.b 78
 dc.b 66
 dc.b 72
 dc.b 71
 dc.b 89
 dc.b 94
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 77
 dc.b 74
 dc.b 85
 dc.b 38
 dc.b 42
 dc.b 0
 dc.b 0
 dc.b 60
 dc.b 75
 dc.b 73
 dc.b 79
 dc.b 41
 dc.b 40
 dc.b 0
 dc.b 0
 dc.b 62
 dc.b 63
 dc.b 76
 dc.b 58
 dc.b 80
 dc.b 95
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 34
 dc.b 0
 dc.b 123
 dc.b 43
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 10
 dc.b 125
 dc.b 0
 dc.b 124
 dc.b 47
 dc.b 10
 dc.b 3
 dc.b 0
 dc.b 6
 dc.b 12
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 11
 dc.b 127
 dc.b 5
 dc.b 0
 dc.b 7
 dc.b 4
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 0
 dc.b 30
 dc.b 0
 dc.b 0
 dc.b 29
;
;
; **** SCREEN ****
;
; CSR_ADDR ( -- addr, return current cursor address)
CSR-ADDR.LF	dc.l	PS2DECODE.NF
CSR-ADDR.NF	dc.b	8 128 +
		dc.b	char R char D char D char A char - char R char S char C
CSR-ADDR.SF	dc.w	CSR-ADDR.Z CSR-ADDR.CF del
CSR-ADDR.CF	#.l	sem-screen
		jsl	acquire.cf
		#.w	CSR-X			
		fetch.l
		lsl
		#.w	CSR-Y
		fetch.l
		#.l	VGAcols
		fetch.b
		2*
		mults
		drop
		+
		#.l	TEXT_ZERO
		fetch.l
		+
		#.l	sem-screen
		jsl	release.cf		
CSR-ADDR.Z	rts				( addr)
;	
; CSR-PLOT ( c --, plot literal character at the current cursor position)
CSR-PLOT.LF	dc.l	CSR-ADDR.NF
CSR-PLOT.NF	dc.b	8 128 +
		dc.b	char T char O char L char P char - char R char S char C
CSR-PLOT.SF	dc.w	CSR-PLOT.Z CSR-PLOT.CF del
CSR-PLOT.CF	#.l	sem-screen
		jsl	acquire.cf
		#.w	INK
		fetch.w			( _c c_)
		or				( w)
		jsl	CSR-ADDR.CF		( w addr)			
		store.w
		#.l	sem-screen
		jsl	release.cf		
CSR-PLOT.Z	rts		
;
; CSR-ON
CSR-ON.LF	dc.l	CSR-PLOT.NF
CSR-ON.NF	dc.b	6 128 +
		dc.b	char N char O char - char R char S char C
CSR-ON.SF	dc.w	CSR-ON.Z CSR-ON.CF del
CSR-ON.CF	#.l	sem-screen
		jsl	acquire.cf
		#.b	char _			( c)
		#.w	PALETTE 4 +
		fetch.b
		#.w	256
		multu
		drop
		or
		jsl	CSR-ADDR.CF		
		dup				( c addr addr)
		fetch.w			( c addr char)
		#.w	CSR			( c addr char csr)
		store.w			( c addr)
		store.w
		#.l	sem-screen
		jsl	release.cf		
CSR-ON.Z	rts		
; saved color+character underneath cursor
CSR		dc.w	0
;
CSR-OFF.LF	dc.l	CSR-ON.NF
CSR-OFF.NF	dc.b	7 128 +
		dc.b	char F char F char O char - char R char S char C
CSR-OFF.SF	dc.w	CSR-OFF.Z CSR-ON.Z del
CSR-OFF.CF	#.l	sem-screen
		jsl	acquire.cf
		#.w	CSR
		fetch.b			( char)
		jsl	CSR-ADDR.CF		
		store.w
		#.l	sem-screen
		jsl	release.cf		
CSR-OFF.Z	rts
;
;SCROLL	(n -- flag, scroll the screen fwd or back n lines.  returns true if out of range)
SCROLL.LF	dc.l	CSR-OFF.NF
SCROLL.NF	dc.b	6 128 +
		dc.b	char L char L char O char R char C char S
SCROLL.SF	dc.w	SCROLL.Z SCROLL.CF del
SCROLL.CF	#.l	sem-screen
		jsl	acquire.cf
		#.l	VGAcols				
		fetch.b
		2*				; width of the screen including color bytes
		mults
		drop				( delta)
		#.l	TEXT_ZERO
		fetch.l			( delta current)	
		+				( new)
		dup				( new new)		; check bottom of range
		#.l	_TEXT_ZERO		( new new base)	
		<				( new flag)
		IF							; below bottom of range
			drop
			#.l	_TEXT_ZERO
			zero
			0=			( new true)
		ELSE
			dup				( new new)		; check top of range
			#.l	_TEXT_ZERO
			#.l	VGAcols
			fetch.b
			2*
			#.l	VGArows
			fetch.b
			multu
			drop
			+				( new new base)
			dup				( new new base base)
			rot				( new base base new)
			<				( new base flag)
			IF							; above top of range
					nip
					zero
					0=			( base true)	
			ELSE
					drop
					zero			( new false)
			THEN
		THEN
		swap				( result new)
		#.l	TEXT_ZERO
		store.l
		#.l	sem-screen
		jsl	release.cf
SCROLL.Z	rts		
;
; (CLS, clear the screen)
CLS.LF		dc.l	SCROLL.NF
CLS.NF		dc.b	3 128 +
		dc.b	char S char L char C
CLS.SF		dc.w	CLS.Z CLS.CF	del
CLS.CF		#.l	sem-screen
		jsl	acquire.cf
		#.l	_TEXT_ZERO		( start start)		; clear screen memory	
		dup
		#.w	SCREENWORDS		( start start 24576)			; number of words in 2 screens
		zero				( start start 24576 color)		; fill word	
		jsl	VWAIT.CF
		jsl	FILL.W.CF		( start)			
		#.l	TEXT_ZERO					; reset pointer to TEXT_ZERO
		store.l
		jsl	HOME.cf
		#.l	sem-screen
		jsl	release.cf		
CLS.Z		rts
;
; HOME ( --, position the cursor upper left without clearing the screen)
HOME.LF	dc.l	CLS.NF
HOME.NF	dc.b	4 128 +
		dc.s	HOME
HOME.SF	dc.w	HOME.Z HOME.CF del
HOME.CF	#.l	sem-screen
		jsl	acquire.cf
		zero							; reset cursor position
		#.w	CSR-X
		store.l	
		zero
		#.w	CSR-Y
		store.l
		#.l	sem-screen
		jsl	release.cf
HOME.Z	rts	
;
; VWAIT ( --, wait for a VGA vertical blank)
VWAIT.LF	dc.l	HOME.NF
VWAIT.NF	dc.b	5 128 +
		dc.S	VWAIT
VWAIT.SF	dc.w	VWAIT.Z VWAIT.CF del
VWAIT.CF	#.l	VBLANK
		BEGIN							; wait for 0 to ensure we get a full interval
			pause
			dup
			fetch.b
			0=
		UNTIL
		BEGIN							; wait for 1 
			dup
			fetch.b
		UNTIL
VWAIT.Z	drop,rts
;
; SCRSET ( --, set the ROWS and COLS according to the video mode)
SCRSET.LF	dc.l	VWAIT.NF
SCRSET.NF	dc.b	6 128 +
		dc.b	char T char E char S char R char C char S
SCRSET.SF	dc.w	SCRSET.Z SCRSET.CF del
SCRSET.CF	#.l	sem-screen		; multitasking lock
		jsl	acquire.cf
		#.l	mode			; check screen mode
		fetch.b			( mode)
		#.b	binary 00000111
		and				( vga-mode)
		1-				( 0/1/2/3)
		2*				( offset)
		#.w	SCRSET.H
		over
		+				( offset addr)				
		fetch.w			( offset Hpixels)
		#.l	charWidth		
		fetch.b			
		1+				( offset Hpixels width)
		divu
		nip				( offset cols)
		#.l	VGAcols
		store.b			( offset)
		#.w	SCRSET.V
		+				( addr)				
		fetch.w			( Vpixels)
		#.l	charHeight		
		fetch.b			
		1+				( Vpixels height)
		#.l	interlace
		fetch.b
		+				( Vpixels height-with-interlace)
		divu
		nip				( rows)
		1-			
		#.l	VGArows			
		store.b
		#.l	sem-screen		; multitasking lock
		jsl	release.cf	
SCRSET.Z	rts
SCRSET.H	dc.w	640			; VGA -  number of pixels horizontally
		dc.w	800			; SVGA
		dc.w	1024			; XGA
		dc.w	1920			; HD
SCRSET.V	dc.w	480			; VGA - number of pixels vertically		
		dc.w	600			; SVGA
		dc.w	768			; XGA
		dc.w	1080			; HD
; NEWLINE ( --, implement a newline)
NEWLINE.LF	dc.l	SCRSET.NF
NEWLINE.NF	dc.b	7 128 +
		dc.b	char E char N char I char L char W char E char N
NEWLINE.SF	dc.w	NEWLINE.Z NEWLINE.CF del
NEWLINE.CF	#.l	sem-screen
		jsl	acquire.cf
		#.w	CSR-Y
		fetch.l
		dup				( y y)
		#.l	VGArows
		fetch.b
		1-
		=				( y flag)		; test if the cursor is at the bottom of the screen or not
		IF							; cursor is bottom of screen
			drop			(  --)
			#.b	1
			jsl	SCROLL.CF	( flag)		; SCROLL forwards 1 row			
			IF						; SCROLL returned true, indicating that the screen page is now at the bottom of the buffer
				#.l	TEXT_ZERO			; Step 1 is to copy the current screen to the top of the buffer
				fetch.l
				#.l	VGAcols
				fetch.b
				2*
				+						; source for copy begins one line from top of page
				#.l	_TEXT_ZERO				; destination
				#.l	VGArows				; Step 2 is to blank the rest of the buffer
				fetch.b				( rows)
				1-					( rows-1)
				#.l	VGAcols
				fetch.b				( rows-1 cols)
				2*					( rows-1 col-bytes)
				multu
				drop					( bytes) ; size = maximum full screen less the top line 
				dup
				>R
				jsl	VWAIT.CF
				jsl	MOVE.CF
				R>
				#.l	_TEXT_ZERO
				+					( s-addr);  start address at the last line of the first page
				dup					( s-addr s-addr)
				#.l 	_TEXT_END			( s-addr s-addr end-addr)
				swap
				-					( s-addr bytes)
				2/					( s-addr words)					
				zero						; blank color
				jsl	FILL.W.CF				; clear remaining screen memory
				#.l	_TEXT_ZERO			; Step 3 is to reset pointer to TEXT_ZERO	
				#.l	TEXT_ZERO			
				store.l				
			THEN
		ELSE							; cursor is mid screen - simply need to go down one row
			1+			( y')		
			#.w 	CSR-Y		( y &ADDR)
			store.l
		THEN
		zero							; zero x position
		#.w	CSR-X
		store.l
		#.l	sem-screen
		jsl	release.cf
NEWLINE.Z	rts
;
CSR-FWD.LF 	dc.l	NEWLINE.NF
CSR-FWD.NF 	dc.b	7 128 +
		dc.b	char D char W char F char - char R char S char C
CSR-FWD.SF	dc.w	CSR-FWD.Z CSR-FWD.CF del
CSR-FWD.CF	#.l	sem-screen
		jsl	acquire.cf
		#.w	CSR-X
		fetch.l
		dup
		#.l	VGAcols
		fetch.b
		1-
		=
		IF	
			drop
			jsl	NEWLINE.CF
		ELSE
			1+
			#.w	CSR-X
			store.l
		THEN
		#.l	sem-screen
		jsl	release.cf
CSR-FWD.Z	rts
;
CSR-BACK.LF	dc.l	CSR-FWD.NF
CSR-BACK.NF	dc.b	8 128 +
		dc.b	char K char C char A char B char - char R char S char C
CSR-BACK.SF	dc.w	CSR-BACK.Z CSR-BACK.CF del
CSR-BACK.CF	#.l	sem-screen
		jsl	acquire.cf	
		#.w	CSR-X
		fetch.l
		?dup
		IF
			1-
			#.w	CSR-X
			store.l
		THEN
		#.l	sem-screen
		jsl	release.cf		
CSR-BACK.Z	rts
;
CSR-TAB.LF	dc.l	CSR-BACK.NF
CSR-TAB.NF	dc.b	7 128 +
		dc.b	char B char A char T char - char R char S char C
CSR-TAB.SF	dc.w	CSR-TAB.Z CSR-TAB.CF del
CSR-TAB.CF	#.l	sem-screen
		jsl	acquire.cf
	#.w	CSR-X
		fetch.l		( x)
		#.w	TAB
		fetch.l		( x t)
		over
		over			( x t x t)
		divu	
		drop			( x t r)
		-
		+
		dup
		#.l	VGAcols
		fetch.b
		1-
		>
		IF
			drop
			jsl	NEWLINE.CF
		ELSE
			#.w	CSR-X
			store.l
		THEN
		#.l	sem-screen
		jsl	release.cf
CSR-TAB.Z	rts
;
; EMITRAW ( n --, emit a character to the VDU excluding non-printing recognition and cursor update)
EMITRAW.LF	dc.l	CSR-TAB.NF
EMITRAW.NF	dc.b	7 128 +
		dc.s	EMITRAW
EMITRAW.SF	dc.w	EMITRAW.Z EMITRAW.CF del
EMITRAW.CF	#.l	sem-screen
		jsl	acquire.cf
		jsl	CSR-PLOT.CF		; plot
		jsl	CSR-FWD.CF		; advance cursor	
		#.l	sem-screen
		jsl	release.cf		
EMITRAW.Z	rts
;
; VEMIT (n --, emit a character to the VDU including non-priniting recognition but excluding cursor update, internal word)
VEMIT.LF	dc.l	EMITRAW.NF
VEMIT.NF	dc.b	5 128 +
		dc.b	char T char I char M char E char V
VEMIT.SF	dc.w	VEMIT.Z VEMIT.CF del
VEMIT.CF	#.l	sem-screen
		jsl	acquire.cf
		#.b	EOL
		over
		=
		IF								; newline
			drop
			jsl	NEWLINE.CF
		ELSE
			#.b	~EOL
			over
			=
			IF							; not newline - ignore
				drop
			ELSE
				#.b	8
				over
				=
				IF						; backspace
					drop
					jsl 	CSR-BACK.CF
					zero
					jsl 	CSR-PLOT.CF
				ELSE
					#.b	9
					over
					=
					IF					; tab
						drop
						jsl	CSR-TAB.CF
					ELSE
						#.b	12
						over
						=
						IF				; clear screen
							drop
							jsl	CLS.CF
						ELSE				; other literal
							jsl	EMITRAW.CF		 ; emit the literal	
						THEN
					THEN
				THEN
			THEN
		THEN
		#.l	sem-screen
		jsl	release.cf
VEMIT.Z	rts
;
; TYPERAW ( addr len, type to VDU, excluding non-printing recognition including cursor update)
TYPERAW.LF	dc.l	VEMIT.NF
TYPERAW.NF	dc.b	7 128 +
		dc.b	char W char A char R char E char P char Y char T
TYPERAW.SF	dc.w	TYPERAW.Z TYPERAW.CF del
TYPERAW.CF	#.l	sem-screen
		jsl	acquire.cf
		?dup
		IF
			over			( addr len addr)
			+			( start end)
			swap			( end start)
			DO
				R@
				fetch.b
				jsl	EMITRAW.CF
			LOOP
		THEN
		#.l	sem-screen
		jsl	release.cf
TYPERAW.Z	rts
;
; VTYPE ( addr len, type to VDU, including non-printing recognition)
VTYPE.LF	dc.l	TYPERAW.NF
VTYPE.NF	dc.b	5 128 +
		dc.b	char E char P char Y char T char V
VTYPE.SF	dc.w	VTYPE.Z VTYPE.CF del
VTYPE.CF	#.l	sem-screen
		jsl	acquire.cf
		?dup
		IF
			over			( addr len addr)
			+			( start end)
			swap			( end start)
			DO
				R@
				fetch.b
				jsl	VEMIT.CF
			LOOP
		ELSE				\ null length
			drop			\ drop address
		THEN
		#.l	sem-screen
		jsl	release.cf
VTYPE.Z	rts
;				
; BACKGROUND ( n --, set the background color)
BACKGROUND.LF	dc.l	VTYPE.NF
BACKGROUND.NF	dc.b	10 128 +
		dc.s	BACKGROUND
BACKGROUND.SF	dc.w	BACKGROUND.Z BACKGROUND.CF del
BACKGROUND.CF	#.l	BACKGROUND
BACKGROUND.Z	store.w,rts
;
; SEVENSEG ( n --, write to the seven-segment display)
SEVENSEG.LF	dc.l	BACKGROUND.NF
SEVENSEG.NF	dc.b	8 128 +
		dc.s	SEVENSEG
SEVENSEG.SF	dc.w	SEVENSEG.Z SEVENSEG.CF del
SEVENSEG.CF	#.l	SEVENSEG
SEVENSEG.Z	store.l,rts
;
; INTERLACE ( lines --, set number of interlace lines)
INTERLACE.LF	dc.l	SEVENSEG.NF
INTERLACE.NF	dc.b	9 128 +
		dc.s	INTERLACE
INTERLACE.SF	dc.w	INTERLACE.Z INTERLACE.CF del
INTERLACE.CF	#.l	sem-screen
		jsl	acquire.cf
		#.l	interlace
		store.b	
		jsl	SCRSET.CF
		#.l	sem-screen
		jsl	release.cf		
INTERLACE.Z	rts
;
; SCREENSIZE ( mode --, set VGA mode.  0=off, 1=VGA, 2=SVGA, 3=XGA, 4=HD)
VGA.LF	dc.l	INTERLACE.NF
VGA.NF	dc.b	3 128 +
		dc.s	VGA
VGA.SF	dc.w	VGA.Z VGA.CF del
VGA.CF		#.l	sem-screen
		jsl	acquire.cf
		jsl	CLS.CF				( VGA)
		#.l	mode
		fetch.b				( VGA mode)
		#.b	binary	11111000		
		and					( VGA mode`)
		or					( mode``)
		#.l	mode
		store.b	
		jsl	SCRSET.CF
		#.l	sem-screen
		jsl	release.cf
VGA.Z		rts
;
; COLORMODE ( flag --. set 16/16 or 256/0 color mode)
COLORMODE.LF	dc.l	VGA.NF
COLORMODE.NF	dc.b	9 128 +
		dc.s	COLORMODE
COLORMODE.SF	dc.w	COLORMODE.Z COLORMODE.CF del
COLORMODE.CF	#.l	sem-screen
		jsl	acquire.cf
		#.l	mode
		fetch.b
		swap
		IF
			#.b	8
			or
		ELSE
			#.b	247
			and
		THEN
		#.l	mode
		store.b	
		#.l	sem-screen
		jsl	release.cf		
COLORMODE.Z	rts
;
;PALETTE.LF	dc.l	COLORMODE.NF
;PALETTE.NF	dc.b	7 128 +
;		dc.b	char E char T char T char E char L char A char P
;PALETTE.SF	dc.w	4
;PALETTE.CF	#.w	PALETTE
;		rts
PALETTE	dc.b	5			; input (yellow)
		dc.b	6			; output (green)
		dc.b	4			; error (red)
		dc.b	8			; (blue)
		dc.b	3			; cursor (white)
; 
; SCREENBASE ( -- addr) CONSTANT address of the pre-allocated screen buffer
SCREENBASE.LF	dc.l	COLORMODE.NF
SCREENBASE.NF	dc.b	10 128 +
		dc.s	SCREENBASE
SCREENBASE.SF	dc.w	SCREENBASE.Z SCREENBASE.SF del
SCREENBASE.CF	#.l	 _TEXT_ZERO
SCREENBASE.Z	rts				; cannot use #.l,rts with inline compiler
;
; SCREENPLACE ( -- addr) VARIABLE address of the current screen buffer
SCREENPLACE.LF	dc.l	SCREENBASE.NF
SCREENPLACE.NF	dc.b	11 128 +
			dc.s	SCREENPLACE
SCREENPLACE.SF	dc.w	SCREENPLACE.Z SCREENPLACE.CF del
SCREENPLACE.CF	#.l TEXT_ZERO
SCREENPLACE.Z		rts			; cannot use #.l,rts with inline compiler
;
INK.LF		dc.l	SCREENPLACE.NF
INK.NF		dc.b	3 128 +
		dc.b	char K char N char I
INK.SF		dc.w	INK.Z INK.CF del
INK.CF		#.l	INK
INK.Z		rts
INK		dc.b	6			; note this variable is BYTE length!
		ds.b	3			; padding
;
CSR-X.LF	dc.l	INK.NF
CSR-X.NF	dc.b	5 128 +
		dc.b	char X char - char R char S char C
CSR-X.SF	dc.w	CSR-X.Z CSR-X.CF del
CSR-X.CF	#.l	CSR-X
		fetch.l
CSR-X.Z	rts
CSR-X		dc.l	0
;
CSR-Y.LF	dc.l	CSR-X.NF
CSR-Y.NF	dc.b	5 128 +
		dc.b	char Y char - char R char S char C
CSR-Y.SF	dc.w	CSR-Y.Z CSR-Y.CF del
CSR-Y.CF	#.l	CSR-Y
		fetch.l
CSR-Y.Z	rts
CSR-Y		dc.l	0
;
TAB.LF		dc.l	CSR-Y.NF
TAB.NF		dc.b	3 128 +
		dc.b	char B char A char T
TAB.SF		dc.w	TAB.Z TAB.CF del
TAB.CF		#.l	TAB
TAB.Z		rts
TAB		dc.l	7			; default to 7 spaces
;
ROWS.LF	dc.l	TAB.NF
ROWS.NF	dc.b	4 128 +
		dc.b	char S char W char O char R
ROWS.SF	dc.w	ROWS.Z ROWS.CF del
ROWS.CF	#.l	VGArows
		fetch.b
ROWS.Z		rts
;
COLS.LF	dc.l	ROWS.NF
COLS.NF	dc.b	4 128 +
		dc.b	char S char L char O char C
COLS.SF	dc.w	COLS.Z COLS.CF del
COLS.CF	#.l	VGAcols
		fetch.b
COLS.Z		rts
COLS		dc.w	100
;
>REMOTE.LF	dc.l	COLS.NF
>REMOTE.NF	dc.b	7 128 +
		dc.b	char E char T char O char M char E char R char >
>REMOTE.SF	dc.w	>REMOTE.Z >REMOTE.CF del
>REMOTE.CF	#.w	SEMIT.CF
		#.l	EMIT_VECTOR
		store.l
		#.w	STYPE.CF
		#.l	TYPE_VECTOR
>REMOTE.Z	store.l,rts	
;
>LOCAL.LF	dc.l	>REMOTE.NF
>LOCAL.NF	dc.b	6 128 +
		dc.b	char L char A char C char O char L char >
>LOCAL.SF	dc.w	>LOCAL.Z >LOCAL.CF del
>LOCAL.CF	#.w	VEMIT.CF
		#.l	EMIT_VECTOR
		store.l
		#.w	VTYPE.CF
		#.l	TYPE_VECTOR
>LOCAL.Z	store.l,rts	
;	
<LOCAL.LF	dc.l	>LOCAL.NF
<LOCAL.NF	dc.b	6 128 +
		dc.b	char L char A char C char O char L char <
<LOCAL.SF	dc.w	<LOCAL.Z <LOCAL.CF del
<LOCAL.CF	#.w	KKEY.CF
		#.l	KEY_VECTOR
		store.l
		#.w	KKEY?.CF
		#.l	KEY?_VECTOR
<LOCAL.Z	store.l,rts
;
<REMOTE.LF	dc.l	<LOCAL.NF
<REMOTE.NF	dc.b	7 128 +
		dc.b	char E char T char O char M char E char R char <
<REMOTE.SF	dc.w	<REMOTE.Z <REMOTE.CF del
<REMOTE.CF	#.w	SKEY.CF
		#.l	KEY_VECTOR
		store.l
		#.w	SKEY?.CF
		#.l	KEY?_VECTOR
<REMOTE.Z	store.l,rts
;
; **** SPI, SD & FAT FILE SYSTEM ****
; SPI functions
SPI.CS-hi	#.l	SPI.control					; DESELECT - CS is active low
		dup
		fetch.b	
		#.b	1 
		or 
		swap
		store.b,rts	 
;
SPI.CS-lo 	#.l	SPI.control 					; SELECT - CS is active low
		dup
		fetch.b
		#.b	254 
		and 
		swap
		store.b,rts
;	
SPI.MOSI-hi	#.l	SPI.control 
		dup
		fetch.b
		#.b	2 
		or 
		swap
		store.b,rts
;
SPI.MOSI-lo 	#.l	SPI.control 
		dup
		fetch.b
		#.b	253 
		and 
		swap
		store.b,rts 
;
SPI.slow 	#.b	255 						; 392kHz at 100MHz clock
		#.l	SPI.divide 
		store.b,rts
;	
SPI.fast 	#.b	16 						; 6.25MHz at 100MHz
		#.l	SPI.divide 
		store.b,rts	
;
; SPI.wait ( --, wait until the SPI transfer-bus is available)	
SPI.wait 	#.l	SPI.status 
		BEGIN
			pause
			dup
			fetch.b
			#.b	1
			and 
		UNTIL
		drop,rts
;
; SPI.put ( n --, put a byte to the SPI port)
SPI.put	jsl	SPI.wait 
		#.l	SPI.data
		store.b,rts
;
; SPI.get ( -- n, get a byte from the SPI port)
SPI.get	#.b	255 
		jsl	SPI.put 
		jsl	SPI.wait 
		#.l	SPI.data
		fetch.b,rts
;
; SD card functions
; SD.cmd ( chk b1 b2 b3 b4 cmd# --, SD command)
SD.cmd		#.b	6
		zero
		DO
			jsl	SPI.put 
		LOOP
		rts
;
; SD.get-rsp ( -- n, get first byte of response from the sd-card)
SD.get-rsp	#.w	1024			; 1024 try limit
		#.l	local0
		store.w
		zero
		BEGIN
			#.l	SD.get-rsp.0	; inline function call countout
			#.l	countout.cf
			jmp		
SD.get-rsp.0		drop
			jsl	SPI.get
			dup 
			#.b	255 
			<>
		UNTIL
		rts
;
; SD.get-R1 ( -- n, get an R1 response from the sd-card)
SD.get-R1	jsl	SD.get-rsp
		jsl	spi.get 
		drop,rts	; one further read always required
;
;SD.ver			; xxxxx [block/byte] [v2/v1];
SD.ver		dc.l	0	
;
; countout ( return-address --, decrement the counter in local0, jump back to return address if >0, or else throw an exception)
; must be called via JMP since local0 is a subroutine local
countout.cf		#.l	local0 ( r-addr local0)
			dup		( r-addr local0 local0)
			fetch.l	( r-addr local0 n)
			dup		( r-addr local0 n n)
			1-		( r-addr local0 n n-1)
			rot		( r-addr n n-1 local0)
			store.l	( r-addr n)
			IF
				jmp
			THEN
			#.l	countout.err
			THROW
;		
countout.err	dc.b	22
		dc.s	SD-card not responding
;
; SD.init ( --, SD card reset, version check and initialize)
SD.init.LF	dc.l	<REMOTE.NF
SD.init.NF	dc.b	7 128 +
		dc.s	SD.init
SD.init.SF	dc.w	SD.init.Z SD.init.CF del
;		#.w	5000 
;		jsl	timeout.cf
SD.init.CF	jsl	spi.slow 
		jsl	spi.cs-hi 		; power sequence dummy clock
		#.b	80 
		zero
		DO
			#.b	255 
			jsl	spi.put 
		LOOP
		jsl	spi.cs-lo
		#.b	25			; limit of 25 tries
		#.l	local0
		store.b
		BEGIN				; CMD0 repeated until good
			#.b	149 
			zero
			zero
			zero
			zero
			#.b	64 
			jsl	sd.cmd 
			jsl	sd.get-R1 
			#.b	1 
			<>
		WHILE	
			#.l	SD.init.0	; inline function call countout
			#.l	countout.cf
			jmp			
SD.init.0		#.b	100 		; 100 ms delay
			jsl	ms.cf
	REPEAT
		#.b	135 			; CMD8	
		#.b	170 
		#.b	1 
		zero
		zero
		#.b	72 
		jsl 	sd.cmd 
		jsl	sd.get-rsp 
		#.b	1 
		= 
		IF					; CMD8 accepted, read data bytes
			#.b	4 
			zero
			DO 
				jsl	spi.get 
			LOOP		( b4 b3 b2 b1)
			jsl	spi.get 		; one further read always required
			drop 			
			#.b	170 
			= 
			swap 			( b4 b3 f b2)
			#.b	1 
			= 
			and 
			nip 
			nip 
			IF			( f) 	; 01xAA confirmed, initialize card
			BEGIN
				#.b 	1 		; CMD55
				zero
				zero
				zero
				zero
				#.b	119 
				jsl	sd.cmd 
				jsl	sd.get-R1 	; CMD55 is just a header
				drop			
				#.b	1 		; CMD41hi
				zero 
				zero
				zero 
				#.b	64 
				#.b	105 
				jsl	sd.cmd	
				jsl	sd.get-R1
				0= 
			UNTIL
			#.b	1 			; CMD58
			zero
			zero 
			zero 
			zero 
			#.b	122 
			jsl	sd.cmd	
			jsl	sd.get-rsp 		; ignore R1
			drop		
			#.b	4
			zero 
			DO 
				jsl	spi.get
			LOOP		( b4 b3 b2 b1)
			jsl	spi.get 		; one further read always required
			drop 			
			drop 
			drop 
			drop 
			#.b	64 			; test CSS bit in OCR
			and	
			IF
				#.b	3 		; SD V2.0 block address
				#.w	sd.ver
				store.l		
			ELSE
				#.b	1 		; SD V.20 byte address
				#.w	sd.ver 
				store.l		
			THEN
			jsl	spi.fast		; V2.0 supports high speed
		ELSE					; 01xAA mismatch
			#.l	SD.init.err
			THROW
		THEN
	ELSE						; CMD8 rejected, initialize card
		BEGIN
			#.b	1 			; CMD55
			zero 
			zero 
			zero 
			zero 
			#.b	119 
			jsl	sd.cmd 
			jsl	sd.get-R1 		; CMD55 is just a header
			drop		
			#.b	1 			; CMD41lo
			zero 
			zero
			zero
			zero
			#.b	105 
			jsl	sd.cmd		
			jsl	sd.get-R1
			0=
		UNTIL
			zero 				; SD V1.0
			#.w	sd.ver 
			store.l			
		THEN
		#.b	1 				; CMD16
		zero 
		#.b	2 
		zero 
		zero 
		#.b	80 
		jsl	sd.cmd 
		jsl	sd.get-rsp 
		drop	
		jsl 	SPI.CS-hi 			; DESELECT
		#.b	255 
		jsl	spi.put			
;		zero	
;		jsl	timeout.cf
SD.init.Z	rts
SD.init.err	#.b	29
		dc.s	SD card initialization failed
;
; SD.sector-code ( n -- b4 b3 b2 b1, scale and split sector address)
SD.sector-code 	#.w	sd.ver 			
		fetch.l 
		#.b	2 
		and			
		0=
		IF					; scale sector to bytes
			#.w	512 
			multu
			drop
		THEN	
		dup 		
		#.b	255 
		and 					; bits 0 - 7
		swap				
		#.b	3 
		zero 
		DO					; bits 8 - 31
			#.b	8 
			jsl	rshift.cf
			dup
			#.b	255 
			and 
			swap
		LOOP
		drop,rts		
;
; SD.select&check ( --, select and wait for SD card)
SD.select&check 	jsl spi.cs-lo		; SELECT
		#.w	32768			; limit of tries
		#.l	local0
		store.w				
		BEGIN				
			#.l	SD.select&check.0	; inline function call to countout
			#.l	countout.cf
			jmp
SD.select&check.0	jsl spi.get 
			#.b	255 
			=				; if CS is asserted while card is busy then card will set D0 low			
		UNTIL
		rts
;
; SD.wait-token ( --, wait for an SD-card data token)
SD.wait-token.CF	#.w	32768			; limit of tries
			#.l	local0
			store.w
			BEGIN	
				#.l	SD.wait-token.0	; inline function call to countout
				#.l	countout.cf
				jmp
SD.wait-token.0		jsl	spi.get
				#.b	254 
				=
			UNTIL
			rts
;
; SD.read-sector ( addr n --, read 512 bytes from sector n of the SD card into a buffer at addr)
SD.read-sector.LF	dc.l	SD.init.NF
SD.read-sector.NF	dc.b	14 128 +
			dc.b	char R char O char T char C char E char S char - char D char A char E char R char . char D char S
SD.read-sector.SF	dc.w	SD.read-sector.Z SD.read-sector.CF del
;			#.w	1000 
;			jsl 	timeout.cf
SD.read-sector.CF		jsl 	sd.select&check
		#.b	1 				; checksum
		swap					
		jsl 	sd.sector-code		; encode sector number
		#.b	81 				; complete CMD17
		jsl 	sd.cmd				
		jsl 	sd.get-R1 
		0<> 
		IF					; check response OK
			#.l	SD.read-sector.ERR
			THROW
		THEN
		jsl	SD.wait-token.CF
;		BEGIN					; wait for data token
;			jsl	spi.get
;			#.b	254 
;			=
;		UNTIL
		dup 
		#.w	512 
		+ 
		swap 
		DO 					; read sector
			jsl 	spi.get 
			I 
			store.b 
		LOOP	
		#.b	3 
		zero 
		DO 					; drop CRC and read safety byte
			jsl 	spi.get 
			drop 
		LOOP			
		jsl 	SPI.CS-hi 
		#.b	255 
		jsl	spi.put			; DESELECT
;		zero 
;		jsl	timeout.CF
SD.read-sector.Z	rts
SD.read-sector.ERR	dc.b	21
			dc.s	SD.read-sector failed
;
; SD.verify-sector ( addr n --, verify 512 bytes from sector n of the SD card with a buffer at addr)
SD.verify-sector.LF	dc.l	SD.read-sector.NF
SD.verify-sector.NF	dc.b	16 128 +
			dc.s	SD.VERIFY-SECTOR
SD.verify-sector.SF	dc.w	SD.verify-sector.Z SD.verify-sector.CF del
;		#.w	1000 
;		jsl 	timeout.cf
SD.verify-sector.CF		jsl 	sd.select&check
		#.b	1 				; checksum
		swap					
		jsl 	sd.sector-code		; encode sector number
		#.b	81 				; complete CMD17
		jsl 	sd.cmd				
		jsl 	sd.get-R1 
		0<> 
		IF					; check response OK
			#.l	SD.verify-sector.ERR
			THROW
		THEN
		jsl	SD.wait-token.CF
;		BEGIN					; wait for data token
;			jsl	spi.get
;			#.b	254 
;			=
;		UNTIL
		dup 
		#.w	512 
		+ 
		swap 
		DO 					; read sector
			jsl 	spi.get 
			I 
			fetch.b 
			=
			not
			IF
				#.l	SD.verify-sector.ERR
				THROW
			THEN
		LOOP	
		#.b	3 
		zero 
		DO 					; drop CRC and read safety byte
			jsl 	spi.get 
			drop 
		LOOP			
		jsl 	SPI.CS-hi 
		#.b	255 
		jsl	spi.put			; DESELECT
;		zero 
;		jsl	timeout.CF
SD.verify-sector.Z		rts
SD.verify-sector.ERR	dc.b	23
			dc.s	SD.verify-sector failed
;
; SD.write-sector ( addr n --, write 512 byte to sector n of the SD card from addr)
SD.write-sector.LF	dc.l	SD.verify-sector.NF
SD.write-sector.NF	dc.b	15 128 +
			dc.b	char R char O char T char C char E char S char - char E char T char I char R char W char . char D char S
SD.write-sector.SF	dc.w	SD.write-sector.Z SD.write-sector.CF del
;		#.w	10000
;		jsl	timeout.CF
SD.write-sector.CF		jsl	sd.select&check	
		#.b	1 				; checksum
		swap					
		jsl	sd.sector-code		; encode sector number
		#.b	88 				; complete CMD24
		jsl 	sd.cmd				
		jsl	sd.get-R1 
		0<> 
		IF					; check response OK
			#.l	SD.write-sector.ERR 
			THROW
		THEN
		#.b	255 
		jsl	spi.put			; space
		#.b	254 
		jsl	spi.put			; initiate data packet
		dup 
		#.w	512 
		+ 
		swap 
		DO 					; write sector
			I 
			fetch.b 	
			jsl spi.put 			
		LOOP	
		#.b	2 
		zero 
		DO 					; dummy checksum
			#.b	1 			
			jsl	spi.put 
		LOOP			
		jsl	sd.get-R1 
		#.b	31 
		and 
		#.b	5 
		<> 
		IF					; check data response
			#.l	SD.write-sector.ERR 
			THROW
		THEN	
		jsl	SPI.CS-hi 
		#.b	255 
		jsl	spi.put			; DESELECT
;		zero 
;		jsl	timeout.cf
SD.write-sector.Z		rts
SD.write-sector.ERR	dc.b	22
			dc.s	SD.write-sector failed
;
; FAT.read-long ( addr n -- x, get a little endian longword from the buffer)
FAT.read-long.LF	dc.l	SD.write-sector.NF
FAT.read-long.NF	dc.b	13 128 +
			dc.s	FAT.read-long
FAT.read-long.SF	dc.w	FAT.read-long.Z FAT.read-long.CF del
FAT.read-long.CF	+
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
			#.w	256 
			multu
			drop 
			+ 
		LOOP
FAT.read-long.Z	rts
;
; FAT.write-long ( x addr n --, write a little endian longword x to the buffer at position n)
FAT.write-long.LF	dc.l	FAT.read-long.NF
FAT.write-long.NF	dc.b	14 128 +
			dc.s	FAT.write-long
FAT.write-long.SF	dc.w	FAT.write-long.Z FAT.write-long.CF del
FAT.write-long.CF	+ 
		>R 
		>R	( R: x addr+n)
		R@ 
		#.b	24 
		jsl	rshift.cf 
		#.b	255 
		and	
		R@ 
		#.b	16 
		jsl	rshift.cf
		#.b	255 
		and	
		R@ 
		#.b	8 
		jsl	rshift.cf 
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
FAT.write-long.Z	rts
;
; FAT.read-word ( addr n -- x, get a little endian word from the buffer)
FAT.read-word.LF	dc.l	FAT.write-long.NF
FAT.read-word.NF	dc.b	13 128 +
			dc.s	FAT.read-word
FAT.read-word.SF	dc.w	FAT.read-word.Z FAT.read-word.CF del
FAT.read-word.CF	1+ 
		+
		dup 
		fetch.b 
		#.w	256 
		multu
		drop 
		swap
		1- 
		fetch.b 
FAT.read-word.Z	+,rts
;
; FAT.write-word ( x addr n --, write a little endian word to the buffer)
FAT.write-word.LF	dc.l	FAT.read-word.NF
FAT.write-word.NF	dc.b	14 128 +
			dc.s	FAT.write-word
FAT.write-word.SF	dc.w	FAT.write-word.Z FAT.write-word.CF del
FAT.write-word.CF	+ 
		>R 
		>R	( R : x addr+n)
		R@ 
		#.b	8 
		jsl	rshift.cf 
		#.b	255 
		and	
		R> 
		#.b	255 
		and
		R@ 
		store.b
		R> 
		1+ 
FAT.write-word.Z	store.b,rts
;
; MOUNT ( --, initiaize the SD card and FAT data structures)
MOUNT.LF	dc.l	FAT.write-word.NF
MOUNT.NF	dc.b	5 128 +
		dc.s	MOUNT
MOUNT.SF	dc.w	MOUNT.Z MOUNT.CF del
MOUNT.CF	jsl	sd.init.cf
		zero
		#.w 	FAT.PtnOff
		store.l
		#.l	_fat.buf 
		dup
		>R
		zero 
		jsl	FAT.read-sector.cf
		jsl	checkFAT32
		not					; sector may be a partiton table
		IF
			R@
			dup
			#.w	454			; Offset to sector of first partition
			jsl	fat.read-long.cf	( _fat.buf firstSectorOfFirstPartition )
			#.w 	FAT.PtnOff
			store.l
			zero
			jsl	FAT.read-sector.cf	
			jsl	checkFAT32
			not
			IF	
				#.l	MOUNT.ERR
				THROW
			THEN
		THEN
		R@			
		#.b	13 
		+ 
		fetch.b 
		#.w	fat.secperclus 
		store.l
		R@
		#.b	44 
		jsl	fat.read-long.cf 
		dup 
		#.w	fat.rootclus 
		store.l 
		#.w	FAT.CurrentDirectory 
		store.l
		R@
		#.b	32 
		jsl	fat.read-long.cf 
		#.w	fat.TotalSectors 
		store.l
		R@ 
		#.b	14 
		jsl	fat.read-word.cf 
		dup 
		#.w	fat.rsvdseccnt 
		store.l				( RsvdSecCnt)
		R@ 
		#.b	16 
		+ 
		fetch.b				( RsvdSecCnt NumFATs)
		R@
		#.b	36 
		jsl	fat.read-long.cf		( RsvdSecCnt NumFATs SecPerFAT)
		multu
		drop
		+ 
		#.w	fat.firstdatasector 
		store.l
		R@
		#.b	1 
		jsl	FAT.read-sector.cf		; FAT32 FSInfo
		R@
		zero 
		jsl	fat.read-long.cf 
		#.l	hex 41615252 
		<> 
		IF					; confirm valid FSInfo sector
			#.l	MOUNT.ERR
			THROW					
		THEN
		R>
		#.w	492 
		jsl	fat.read-long.cf 
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
MOUNT.Z	store.l,rts				; FAT buffer initialized
MOUNT.ERR	dc.b	24
		dc.s	Not a valid FAT32 volume
;
; checkFAT32  ( -- flag) examine the current sector to see if it is a FAT32 boot sector
checkFAT32	#.l	_fat.buf
		#.w	510 
		jsl	fat.read-word.cf 
		#.w	43605 				; confirm sector signature 0xAA55
		= 
		#.l	_fat.buf
		#.b	82 				; confirm FAT32 signature 0x4146
		jsl	fat.read-word.cf 
		#.w	16710 
		=
		and,rts
;
; Add the approprate partition offset and then call SD.read-sector
; FAT.read-sector ( addr n --, read 512 bytes from sector n of the current partition into a buffer at addr)
FAT.read-sector.LF	dc.l	MOUNT.NF
FAT.read-sector.NF	dc.b	15 128 +
			dc.s	FAT.read-sector
FAT.read-sector.SF	dc.w	FAT.read-sector.Z FAT.read-sector.CF del
FAT.read-sector.CF	#.w 	FAT.PtnOff
			fetch.l
			+
			jsl SD.read-sector.CF
FAT.read-sector.Z	rts				
;
; Add the appropriate partition offset and then call SD.write-sector
; FAT.write-sector ( addr n --, write and verify 512 byte to sector n of the current partition from addr)
FAT.write-sector.LF	dc.l	FAT.read-sector.NF
FAT.write-sector.NF	dc.b	16 128 +
			dc.s	FAT.write-sector
FAT.write-sector.SF	dc.w	FAT.write-sector.Z FAT.write-sector.CF del
FAT.write-sector.CF	#.w 	FAT.PtnOff
			fetch.l
			+
			over
			over
			jsl SD.write-sector.CF
			jsl SD.verify-sector.CF
FAT.write-sector.Z	rts
;
; FAT.UpdateFSInfo ( --, update the FAT32 FSInfo sector with next free cluster)
FAT.UpdateFSInfo.LF	dc.l	FAT.write-sector.NF
FAT.UpdateFSInfo.NF	dc.b	16 128 +
			dc.s	FAT.UpdateFSInfo
FAT.UpdateFSInfo.SF	dc.w	FAT.UpdateFSInfo.Z FAT.UpdateFSInfo.CF del
FAT.UpdateFSInfo.CF	#.l	_fat.buf 
		dup
		>R
		#.b	1 
		jsl	FAT.read-sector.cf
		#.w	FAT.NextFreeCluster 
		fetch.l 
		R@	 			
		#.w	492 
		jsl	FAT.write-long.CF
		R> 
		#.b	1 
		jsl	FAT.write-sector.cf
FAT.UpdateFSInfo.Z		rts
;
; FAT.clus2sec ( n -- n, given a valid cluster number return the number of the first sector in that cluster)
FAT.clus2sec.LF	dc.l	FAT.UpdateFSInfo.NF
FAT.clus2sec.NF	dc.b	12 128 +
			dc.s	FAT.clus2sec
FAT.clus2sec.SF	dc.w	FAT.clus2sec.Z FAT.clus2sec.CF del
FAT.clus2sec.CF	1-				
		1-					; first cluster is number 2
		#.w	fat.secperclus 
		fetch.l
		multu
		drop
		#.w	fat.firstdatasector 
		fetch.l
FAT.clus2sec.Z	+,rts
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
			#.l	_fat.buffat 
			swap	 			( ThisFATEntOffset fat.fatbuf ThisFATSecNum)
			jsl	FAT.read-sector.cf	( ThisFATEntOffset)
		ELSE
			drop				( ThisFATEntOffset)
		THEN
		rts
;
; FAT.get-fat ( n -- x, return the FAT entry for a given cluster)
FAT.get-fat.LF	dc.l	FAT.clus2sec.NF
FAT.get-fat.NF	dc.b	11 128 +
			dc.s	FAT.get-fat
FAT.get-fat.SF	dc.w	FAT.get-fat.Z FAT.get-fat.CF del
FAT.get-fat.CF	jsl	FAT.prep-fat
			#.l	_fat.buffat 
			swap					( fat.buf ThisFATEntOffset)
			jsl	fat.read-long.cf
			#.l	hex	0FFFFFFF
FAT.get-fat.Z		and,rts
;
; FAT.put-fat ( value cluster --, place value in the FAT location for cluster)
FAT.put-fat.LF	dc.l	FAT.get-fat.NF
FAT.put-fat.NF	dc.b	11 128 +
			dc.s	FAT.put-fat
FAT.put-fat.SF	dc.w	FAT.put-fat.Z FAT.put-fat.CF del
FAT.put-fat.CF	jsl	FAT.prep-fat		( value ThisFATEntOffset)
			#.l	_fat.buffat 
			swap					( value fat.buf ThisFATEntOffset)
			jsl	fat.write-long.CF
			#.l	_FAT.buffat 
			#.w	FAT.FATinBuf 
			fetch.l
			jsl	FAT.write-sector.cf
FAT.put-fat.Z		rts
;
FAT.filestring	ds.b	12
; FAT.string2filename ( addr n -- addr, convert an ordinary string to a short FAT filename)
FAT.string2filename.LF	dc.l	FAT.put-fat.NF
FAT.string2filename.NF	dc.b	19 128 +
				dc.s 	FAT.string2filename
FAT.string2filename.SF	dc.w	FAT.string2filename.Z FAT.string2filename.CF del
FAT.string2filename.CF	>R 
		>R		
		#.l	FAT.filestring
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
			jsl	min.cf
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
					jsl	upper.cf
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
FAT.string2filename.Z	rts
;
; FAT.find-file-local ( dirCluster addr n -- dirSector dirOffset firstCluster size flags TRUE | FALSE, find in local folder)
FAT.find-file-local	jsl FAT.String2Filename.CF	( cluster filestring)
		swap 
		dup 
		>R					( filestring cluster R:cluster)
		BEGIN
			jsl	FAT.Clus2Sec.CF	( filestring firstSec R:cluster)
			dup 
			#.w	FAT.SecPerClus 
			fetch.l 
			+ 
			swap				( filestring lastSec firstSec R:cluster)	
		DO					( filestring R:LOOP cluster)	; examine each sector in the cluster
			#.l	_FAT.buf 
			i 
			jsl	FAT.read-sector.cf
			#.l	_FAT.buf 
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
					UNLOOP
					UNLOOP 
					nip 
					R> 
					drop 
;					#.w	FAT.find-file-local.e1
;					jsl	COUNT.CF
;					jsl	TYPE.cf
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
						jsl	$=.cf 
						IF					; test string match
							drop							; remove filestring	
							j							; dirSector
							i 
							#.l	_FAT.buf 
							-							; directory offset 
							i 
							#.b	20 
							jsl	FAT.read-word.cf 
							#.l	65536 
							multu
							drop
							i 
							#.b	26 
							jsl 	FAT.read-word.cf 
							+ 							; startCluster
							i 
							#.b	28 
							jsl	FAT.read-long.cf 				; size		
							i 
							#.b	11 
							+ 
							fetch.b						; flags
							UNLOOP 
							UNLOOP
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
				jsl	FAT.get-fat.CF		( filestring nextCluster)
				dup 
				#.l	hex 0FFFFFFF 								; End-of-clusters mark
				=					( filestring nextCluster flag) 	
			UNTIL
		drop 
		drop 
;		#.w	FAT.find-file-local.e2
;		jsl	COUNT.CF
;		jsl	TYPE.CF
		zero
		rts									; likely bad directory
;
;FAT.find-file-local.e1	dc.b 25
;				dc.s reached end of directory 
;FAT.find-file-local.e2	dc.b 29
;				dc.s reached end-of-clusters mark 
;
; FAT.find-file ( addr n -- dirSector dirOffset firstCluster size flags TRUE | FALSE, find from current directory)
FAT.find-file.LF	dc.l	FAT.string2filename.NF
FAT.find-file.NF	dc.b	13 128 +
			dc.s	FAT.find-file
FAT.find-file.SF	dc.w	FAT.find-file.Z FAT.find-file.CF del
FAT.find-file.CF	#.w	FAT.CurrentDirectory 
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
					jsl	FAT.find-file-local 
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
							UNLOOP 
							R> 
							drop 
							drop 
							drop 
							drop 
							drop 
;							#.w	FAT.find-file.e1
;							jsl	COUNT.CF
;							jsl	TYPE.CF
							zero 
							rts	
					THEN
				ELSE						; cannot parse filepath - not found
					UNLOOP 
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
		jsl	FAT.find-file-local 	
FAT.find-file.Z	rts
;FAT.find-file.e0	dc.b 26
;			dc.s filepath is not a directory 
;
; EX
; FAT.load-file ( addr firstCluster --, load a file to addr given the first cluster, cluster by cluster)
FAT.load-file.LF	dc.l	FAT.find-file.NF
FAT.load-file.NF	dc.b	13 128 +
			dc.s	FAT.load-file
FAT.load-file.SF	dc.w	FAT.load-file.Z FAT.load-file.CF del
FAT.load-file.CF	BEGIN						
			dup 
			>R					( addr currentCluster R:currentCluster)
			jsl	FAT.Clus2Sec.CF		( addr firstSec R:currentCluster)
			dup 
			#.w	FAT.SecPerClus 
			fetch.l 
			+ 
			swap					( addr lastSec firstSec R:currentCluster)
			DO
				dup 
				i 
				jsl FAT.read-sector.cf		
				#.w	512 
				+				( addr)
			LOOP
			R>					( addr currentCluster)
			jsl	FAT.get-fat.CF		( addr nextCluster)
			dup 
			#.l	hex 0FFFFFFF 			; End-of-clusters mark
			=					( addr nextCluster flag) 	
		UNTIL
		drop 
FAT.load-file.Z	drop,rts
;
; include ( "FILEPATH" --)
include.LF	dc.l	FAT.load-file.NF
include.NF	dc.b	7 128 +
		dc.s	include
include.SF	dc.w 	include.Z include.CF del
include.CF	#.b	32 
		jsl	word.cf
		jsl	count.cf 
		jsl	FAT.find-file.CF 			( dirSector dirOffset firstCluster size flags TRUE | FALSE)
		IF
			drop 
			>R 
			nip 
			nip 
			#.l	hex 00FF0000 			; addr is 64K below top of memory
			dup 
			rot					( addr addr firstCluster R:size)			
			jsl	FAT.load-file.CF 		( addr R:size)
			R> 
			jsl	evaluate.cf 			( )
		ELSE
			#.l	include.ERR 
			THROW
		THEN
include.Z		rts
include.ERR	dc.b	14
		dc.s	File not found
;
; FAT variables
FAT.RsvdSecCnt		dc.l	0		; number of reserved sectors
FAT.FirstDataSector		dc.l	0		; first sector after FAT
FAT.FATinBuf			dc.l	0		; the currently buffered FAT sector
;
FAT.SecPerClus.LF		dc.l	include.NF
FAT.SecPerClus.NF		dc.b	14 128 +
				dc.s	FAT.SecPerClus
FAT.SecPerClus.SF		dc.w	4
FAT.SecPerClus.CF		#.w	FAT.SecPerClus
				rts
FAT.SecPerClus		dc.l	0	; sectors per cluster
;
FAT.TotalSectors.LF		dc.l	FAT.SecPerClus.NF
FAT.TotalSectors.NF		dc.b	16 128 +
				dc.s 	FAT.TotalSectors
FAT.TotalSectors.SF		dc.w	4
FAT.TotalSectors.CF		#.w	FAT.TotalSectors
				rts
FAT.TotalSectors		dc.l 	0	; total sectors on the disk
;
FAT.NextFreeCluster.LF	dc.l	FAT.TotalSectors.NF
FAT.NextFreeCluster.NF	dc.b	19 128 +
				dc.s	FAT.NextFreeCluster
FAT.NextFreeCluster.SF	dc.w	4
FAT.NextFreeCluster.CF	#.w	FAT.NextFreeCluster
				rts
FAT.NextFreeCluster		dc.l 	0	; where to look for the next free cluster
;
FAT.CurrentDirectory.LF	dc.l	FAT.NextFreeCluster.NF
FAT.CurrentDirectory.NF	dc.b	20 128 +	
				dc.s	FAT.CurrentDirectory
FAT.CurrentDirectory.SF	dc.w	4
FAT.CurrentDirectory.CF	#.w	FAT.CurrentDirectory
				rts
FAT.CurrentDirectory		dc.l 	0	; cluster number of current directory
;
FAT.RootClus.LF		dc.l	FAT.CurrentDirectory.NF
FAT.RootClus.NF		dc.b	12 128 +
				dc.s	FAT.RootClus
FAT.RootClus.SF		dc.w	4
FAT.RootClus.CF		#.w	FAT.RootClus
				rts
FAT.RootClus			dc.l 	0	; first cluster of root directory
;
FAT.PtnOff.LF			dc.l	FAT.RootClus.NF
FAT.PtnOff.NF			dc.b	10 128 +
				dc.s	FAT.PtnOff
FAT.PtnOff.SF			dc.w	4
FAT.PtnOff.CF			#.w	FAT.PtnOff
				rts
FAT.PtnOff			dc.l 	0	; partition offset
;
FAT.buf.LF			dc.l	FAT.PtnOff.NF
FAT.buf.NF			dc.b	7 128 +
				dc.s	FAT.buf
FAT.buf.SF			dc.w	6
FAT.buf.CF			#.l	_FAT.buf
				rts
;
; -----------------------------------------------------------------------------------------------------------
; FORTH CORE DICTIONARY
; -----------------------------------------------------------------------------------------------------------
;
; RESET ( --)
RESET.LF	dc.l	FAT.buf.NF
RESET.NF	dc.b	5 128 +
		dc.s	RESET
RESET.SF	dc.w	RESET.Z RESET.CF del
RESET.CF	zero					; reset the compilation wordlist to FORTH
		jsl	SET-CURRENT.CF
		jsl	START.CF
RESET.Z	rts
;
ACCEPT.LF	dc.l	RESET.NF
ACCEPT.NF	dc.b	6 128 +
		dc.b	char T char P char E char C char C char A
ACCEPT.SF	dc.w	ACCEPT.Z ACCEPT.CF del NOINLINE +
ACCEPT.CF	zero			( addr n u) u = current char count
; 	check space in buffer
ACCEPT.0	over			( addr n u n)
		over			( addr n u n u)
		<>
ACCEPT.1	beq 	ACCEPT.10			; reached maximum input length
;	receive next character
		rot			( n u addr)
		jsl	CSR-ON.CF
		jsl 	KEY.CF		( n u addr char)	
		jsl	CSR-OFF.CF
;	test for LF
		dup			( n u addr char char)
		#.b 	EOL		; LF terminator
		<>			( n u addr char flag)
ACCEPT.2	beq	ACCEPT.6			; terminator received
;	test for CR
		dup
		#.b 	13		; CR
		<>
ACCEPT.11	beq	ACCEPT.12			; CR received
;	emit character (backspace is echoed, CR is not)
		dup			( n u addr char char)
		jsl	EMIT.CF	( n u addr char)			
;	test for backspace	
		dup			( n u addr char char)
		#.b 	8		; Backspace
		<>			( n u addr char flag)
ACCEPT.3	beq	ACCEPT.7			; backspace received
;	test for tab
		dup
		#.b	9
		=
ACCEPT.14	beq	ACCEPT.15 	; not TAB
		drop			( n u addr )		; drop TAB
		#.b	32		( n u addr space)	; replace with space
;	save char
ACCEPT.15	over			( n u addr char addr)
		store.b		( n u addr)
;	advance counters
		1+			( n u addr+1)
		swap			( n addr+1 u)
		1+			( n addr+1 u+1)
		swap			( n u+1 addr+1)
;	repeat
ACCEPT.4	rot			( u+1 addr+1 n)	; rshuffle stack
		rot			( addr+1 n u+1)		
ACCEPT.5	bra 	ACCEPT.0			; repeat next char	
;	ignore CR
ACCEPT.12	drop			( n u addr)
ACCEPT.13	bra	ACCEPT.4
;	process terminator and put the LF into the buffer and increment
ACCEPT.6	swap			( n u char addr)
		store.b		( n u)
;		drop			( n u addr)
;		drop			( n u)
		1+			( n u+1)
		nip,rts		( u+1)
;	process backspace
ACCEPT.7	drop			( n u addr)
		over			( n u addr u)
		0<>			( n u addr flag) 
ACCEPT.8	beq	ACCEPT.4 			; ignore backspace when u=0
		1-			( n u addr-1)
		swap			( n addr-1 u)
		1-			( n addr-1 u-1)
		swap			( n u-1 addr-1)
ACCEPT.9	bra 	ACCEPT.4			; backed-up
;	clean-up stack for return
ACCEPT.10	nip			( addr u)
ACCEPT.Z	nip,rts		( u)
;
; UPPER ( char -- CHAR, convert one character to UPPERCASE)
UPPER.LF	dc.l 	ACCEPT.NF
UPPER.NF	dc.b	5 128 +
		dc.b	char R char E char P char P char U
UPPER.SF	dc.w	UPPER.Z UPPER.CF del
UPPER.CF	dup			( char char)
		#.b 	char a 	( char a)
		#.b 	char z		( char a z)
		1+			( char a z+1)
		jsl 	within.cf	( char flag)
		IF
			#.b 32 	( char 32)
			- 		( CHAR)
		THEN	
UPPER.Z	rts
;
; Convert a single character to a number in the current base.
; DIGIT   ( char -- n true | char false )
DIGIT.LF	dc.l	UPPER.NF
DIGIT.NF	dc.b	5 128 +
		dc.b	char T char I char G char I char D
DIGIT.SF	dc.w	DIGIT.Z DIGIT.CF del
DIGIT.CF	jsl	UPPER.CF	( CHAR)	; convert char lower to upper			
; deal with alphanumerics
		dup 				( char char)
		dup 				( char char char)
		#.b char A 			( char char char A)
		1- 
		>				( char char flag)
		IF 				; alphabetic 
			#.b char A 
			- 
			#.b char 9
			+ 
			1+			( char n)
		ELSE 				; numeric
			dup 			( char char char)
			#.b char 9 
			>			( char n flag)
			IF	; between 9 and A is bad
				drop 	 	( char) 
				zero		( char 0) ; trigger error below
			THEN			
		THEN
; convert modified ASCII value to number
		#.b char 0 			( char n '0')
		-				( char n)
		dup 				( char n n)
		#.l BASE_ 	
		fetch.l			( char n n base)
		<
; check validity
		IF 				; within base range
			dup 			( char n n)
			1+ 			( char n n)
			0>			( char n flag)
			IF 			; a valid digit
				nip 		( n)
				zero		
				not		( n true)
			ELSE 			; not a valid digit
				drop 
				zero		( char false)
			THEN
		ELSE 				; out of base range
			drop 			( char)
			zero			( char false)
		THEN
DIGIT.Z	rts		
;
; D+ 	(ud1 ud2 -- ud3)  double precision arithmetic
D+.LF		dc.l	DIGIT.NF
D+.NF		dc.b	2 128 +
		dc.b	char + char D
D+.SF		dc.w	D+.Z D+.CF del
D+.CF		#.l	intmask		; disable interrupts to protect ADDX flag
		dup
		fetch.l
		>R
		zero
		swap 
		store.b
; suspend multitasking		
		jsl	CHECKSUSPEND	( v)	
		>R
; D+ code
		SWAP		( h2 l2 l1 h1)
		>R		( h2 l2 l1 R: h1)
		rot		( l2 l1 h2 R: h1)
		>R		( l2 l1 R: h1 h2)
		+		( l3 R: h1 h2)
		R>		( l3 h2 R: h1)
		R>		( l3 h2 h1)
		ADDX		( l3 h3)
		swap		( h3 l3)
; re-enable multitasking to its prior state
		R>
		#.l	SingleMulti 
		store.b		
; restore interrupts
		R>
		#.l	intmask
D+.Z		store.l,rts
;
; D- 	(ud1 ud2 -- ud3)  double precision arithmetic
D-.LF		dc.l	D+.NF
D-.NF		dc.b	2 128 +
		dc.b	char - char D
D-.SF		dc.w	D-.Z D-.CF del
D-.CF		#.l	intmask		; disable interrupts to protect SUBX flag
		dup
		fetch.l
		>R
		zero
		swap 
		store.b
; suspend multitasking		
		jsl	CHECKSUSPEND	( v)	
		>R
; D- code	
		>R
		SWAP
		>R
		-
		R>
		R>
		SUBX
; re-enable multitasking to its prior state
		R>
		#.l	SingleMulti 
		store.b			
; restore interrupts
		R>
		#.l	intmask
D-.Z		store.l,rts
;
; >NUMBER ( ud1 c-addr1 u1 -- ud2 c-addr2 u2 , convert till bad char , CORE )
>NUMBER.LF	dc.l	D-.NF
>NUMBER.NF	dc.b	7 128 +
		dc.b	char R char E char B char M char U char N char >
>NUMBER.SF	dc.w	>NUMBER.Z >NUMBER.CF del
>NUMBER.CF	>R				( ud c-addr R: u1)
	BEGIN
		r@ 				( ud c-addr u1 R: u1)
		0>    ; any characters left?
		IF
			dup 			( ud c-addr c-addr)
			fetch.b 		( ud c-addr char)
;			#.w BASE_ 	
;			fetch.l		( ud c-addr char base)
			jsl	DIGIT.CF  ( ud c-addr , n true | char false)	
			IF		; is a digit
				zero
				not		( ud c-addr n true)
			ELSE		; is not a digit
				drop 
				zero		( ud c-addr false)
			THEN
		ELSE
			zero			( ud c-addr false)
		THEN
	WHILE 					( -- ud c-addr n)
		swap 				( ud n c-addr) 
		>R  				( ud1lo ud1hi n R: u c-addr)
; multiply ud1hi * base
		swap  				( ud1lo n ud1hi)
		#.l BASE_ 
		fetch.l			( ud1lo n ud1hi base)
		multu
		drop 				( ud1lo n ud1hi*baselo)	; discard bits 39 - 32
; multiply ud1lo * base
		rot  				( n ud1hi*baselo ud1lo)
		#.l BASE_
		fetch.l			( n ud1hi*baselo ud1lo base)
		multu				( n ud1hi*baselo ud1lo*basello ud1lo*baselhi )
; add in one step the two hi parts of the multiplicaton, and n with the lo part of the multiplciation
		jsl	D+.CF  		( ud2 )			
		R> 				( ud2 c-addr1 R: u1)
		1+     			( ud2 c-addr2 R: u1)	; increment address
		R> 				( ud2 c-addr2 u1 R:)
		1- 				( ud2 c-addr2 u2 R:)
		>R  				( ud2 c-addr2 R: u2)	; decrement count
	REPEAT
		R>				( ud2 c-addr2 u2)
>NUMBER.Z	rts
;
; NUMBER?	( c-addr u - 0 | n 1 , convert number and return with success flag)
NUMBER?.LF	dc.l	>NUMBER.NF
NUMBER?.NF	dc.b	7 128 +
		dc.b	char ? char R char E char B char M char U char N
NUMBER?.SF	dc.w	NUMBER?.Z NUMBER?.CF del NOINLINE +	; No inline due to mid-code exit point
NUMBER?.CF	over			( c-addr1 u1 c-addr)
		fetch.b		( c-addr1 u1 char)
		#.b char -		( c-addr1 u1 char '-')
		=			( c-addr1 u1 flag)
; test for a minus sign and save flag
		dup			( c-addr1 u1 flag flag)
		>R			( c-addr1 u1 flag R: flag)		
		IF			; minus sign, ignore first character
			1-		( c-addr1 u1' R: flag)
			swap		( u1' c-addr1 R: flag)
			1+		( u1' c-addr1' R: flag)
			swap		( c-addr1' u1' E: flag)
		THEN
; set ud1 = 0
		>R			( c-addr1 R: flag u1)
		>R			( R: flag u1 c-addr1)
		zero
		zero			( ud1 R: flag u1 c-addr1)
		R>			( ud1 c-addr1 R: flag u1)
		R>			( ud1 c-addr1 u1 R: flag)
; convert with >NUMBER
		jsl	>NUMBER.CF	( ud2 c-addr1 u2 R: flag)		
		IF			; u2 is number of unconverted chars
			drop		( ud2 R: flag)
			drop		( ud2lo R: flag)
			drop		( R: flag)
			R>		( flag)
			drop		( ) 
			zero,rts	( 0)
		THEN
		drop			( ud2 R: flag)
		drop			( u R: flag)
		R>			( u flag)
		IF
			negate		( n)
		THEN
NUMBER?.Z	#.b,rts	1		( n 1)		; #.b,rts safe as NOINLINE is set
;		
; HOLD		( char --, output a character)
HOLD.LF	dc.l	NUMBER?.NF
HOLD.NF	dc.b	4 128 +
		dc.b	char D char L char O char H
HOLD.SF	dc.w 	HOLD.Z HOLD.CF del
; Decrement hld address
HOLD.CF	#.l hld_		(char hld*)
		dup			(char hld* hld*)
		fetch.l		(char hld* hld)
		1-			(char hld* hld-1)
		dup			(char hld* hld-1 hld-1)
		rot			(char hld-1 hld-1 hld*)
		store.l		(char hld-1)
; Store the character in the new address
HOLD.Z		store.b,rts
;
; <#     ( -- , setup conversion )
<#.LF		dc.l	HOLD.NF
<#.NF		dc.b	2 128 +
		dc.b	char # char <
<#.SF		dc.w	<#.Z <#.CF del
<#.CF		#.l _padend		( padend)
		#.l hld_		( padend hld*)
<#.Z		store.l,rts	
;
; u#>     ( u -- addr len , finish single precision conversion )
U#>.LF		dc.l	<#.NF
U#>.NF		dc.b	3 128 +
		dc.b	char > char # char U
U#>.SF		dc.w	U#>.Z U#>.CF del
U#>.CF		drop  
		#.l hld_		( hld*)
		fetch.l		( hld)
		#.L _padend  		( hld padend)
		over 			( hld padend hld)
U#>.Z		-,rts			( hld len)
;
; sign   ( n -- , add '-' if negative )
SIGN.LF	dc.l	U#>.NF
SIGN.NF	dc.b	4 128 +
		dc.b	char N char G char I char S
SIGN.SF	dc.w	SIGN.Z SIGN.CF del
SIGN.CF	0<
		If
			#.b	char -
			jsl 	HOLD.CF
		THEN
SIGN.Z		rts
;
; U#      ( u -- u , convert one digit, single precision )
U#.LF		dc.l	SIGN.NF
U#.NF		dc.b	2 128 +
		dc.b	char # char U
U#.SF		dc.w	U#.Z U#.CF del
U#.CF		#.l BASE_ 	
		fetch.l		( n base)
		divu			( u-rem u-quot)
		swap
		dup			( u-quot u-rem u-rem)
		#.b	9
		>			( u-quot u-rem flag)
	IF  
		#.b	7 
		+
	THEN
		#.b 	char 0 	( u-quot n char0)
		+ 			( u-quot char)
		jsl	HOLD.CF	( u-quot)
U#.Z		rts
;
; U#S     ( u -- u , convert remaining digits )
U#S.LF		dc.l	U#.NF
U#S.NF		dc.b	3 128 +
		dc.b	char S char # char U
U#S.SF		dc.w	U#S.Z U#S.CF del
U#S.CF		BEGIN
			jsl	U#.CF	( u )			
			dup		( u u)
			0=		( u flag)
		UNTIL
U#S.Z		rts		( u)
;
; ABS ( n -- +n , make a single precision number positive)
ABS.LF		dc.l	U#S.NF
ABS.NF		dc.b	3 128 +
		dc.b	char S char B char A
ABS.SF		dc.w	ABS.Z ABS.CF del
ABS.CF		dup
		0<
	IF
		negate
	THEN
ABS.Z		rts
;	
; . ( n -- , print a single precision signed number)
DOT.LF		dc.l	ABS.NF
DOT.NF		dc.b	1 128 +
		dc.b 	char .
DOT.SF		dc.w	DOT.Z DOT.CF del
DOT.CF		dup			( n n)			
		jsl	ABS.CF		( n u)
		jsl	<#.CF		( n u)		
		jsl 	U#S.CF
		swap			( u n)
		jsl	SIGN.CF	( u)
		jsl 	U#>.CF		( c-addr len)		
		jsl 	TYPE.CF				
		#.b	32
		jsl	EMIT.CF	
DOT.Z		rts
;
; . ( n x -- , print a single precision signed number with at least x printed digits)
DOTR.LF	dc.l	DOT.NF
DOTR.NF	dc.b	2 128 +
		dc.b 	char R char . 
DOTR.SF	dc.w	DOTR.Z DOTR.CF del
DOTR.CF	1-						; deduct 1 since one digit is guaranteed anyway
		>R			( n R:x)		; push digit count to return stack
		dup			( n n)			
		jsl	ABS.CF		( n u)			; prepare for a sign digit
		jsl 	<#.CF		( n u)			; initiate conversion		
		R>			( n u x)
		zero			( n u x 0)
		DO						; print the fixed digits
			jsl	U#.CF
		LOOP
		jsl	U#S.CF					; print remaining digits
		swap			( u n)
		jsl	SIGN.CF	( u)			; sign
		jsl 	U#>.CF		( c-addr len)		; complete conversion		
		jsl 	TYPE.CF				; output			
		#.b	32
		jsl	EMIT.CF	
DOTR.Z		rts
;	
; U. ( n -- , print a single precision unsigned number)
UDOT.LF	dc.l	DOTR.NF
UDOT.NF	dc.b	2 128 +
		dc.b 	char . char U
UDOT.SF	dc.w	UDOT.Z UDOT.CF del
UDOT.CF	jsl 	<#.CF		( u)		
		jsl 	U#S.CF		( u)		
		jsl 	U#>.CF		( c-addr len)			
		jsl 	TYPE.CF		
		#.b	32
		jsl	EMIT.CF
UDOT.Z		rts
;
; U.R ( u x -- , print a single precision unsigned number)
UDOTR.LF	dc.l	UDOT.NF
UDOTR.NF	dc.b	3 128 +
		dc.b 	char R char . char U
UDOTR.SF	dc.w	UDOTR.Z UDOTR.CF del
UDOTR.CF	1-
		>R
		jsl 	<#.CF		( u)			
		R>			( u x)
		zero			( u x 0)
		DO						; print the fixed digits
			jsl	U#.CF
		LOOP
		jsl 	U#S.CF		( u)			
		jsl 	U#>.CF		( c-addr len)		
		jsl 	TYPE.CF		
		#.b	32
		jsl	EMIT.CF
UDOTR.Z	rts
;
; MIN	( n1 n2 -- n3, minimum)
MIN.LF		dc.l	UDOTR.NF
MIN.NF		dc.b	3 128 +
		dc.b	char N char I char M
MIN.SF		dc.w	MIN.Z MIN.CF del
MIN.CF		over	( n1 n2 n1)
		over	( n1 n2 n1 n2)
		<	( n1 n2 flag)
		IF		
			drop
		ELSE
			nip
		THEN
MIN.Z		rts
;
; MAX	( n1 n2 -- n3, minimum)
MAX.LF		dc.l	MIN.NF
MAX.NF		dc.b	3 128 +
		dc.b	char X char A char M
MAX.SF		dc.w	MAX.Z MAX.CF del
MAX.CF		over	( n1 n2 n1)
		over	( n1 n2 n1 n2)
		>	( n1 n2 flag)
		IF		
			drop
		ELSE
			nip
		THEN
MAX.Z		rts
;
; WITHIN ( x1 x2 x3 -- flag), true if x2 <= x1 < x3 false otherwise
WITHIN.LF	dc.l	MAX.NF
WITHIN.NF	dc.b	6 128 +
		dc.b	char N char I char H char T char I char W
WITHIN.SF	dc.w	WITHIN.Z WITHIN.CF del
WITHIN.CF	rot			( x2 x3 x1)
		dup			( x2 x3 x1 x1)
		>R			( x2 x3 x1 R:x1)
		>			( x2 flag R:x1)
		R>			( x2 flag x1)
		rot			( flag x1 x2)
		<			
		not			( flag flag)
WITHIN.Z	and,rts		( flag)
;
; COMP ( n1 n2 -- n, return -1 if n1<n2, +1 if n1>2, 0 if n1=n2)
COMP.LF	dc.l	WITHIN.NF
COMP.NF	dc.b	4 128 +
		dc.b 	char P char M char O char C 
COMP.SF	dc.w	COMP.Z COMP.CF del
COMP.CF	over		(n1 n2 n1)
		over		(n1 n2 n1 n2)
		<		(n1 n2 <flag)
		rot		(n2 <flag n1)
		rot		(<flag n1 n2)
		>		(<flag flag)
		negate		(<flag >flag)		\ +1 if n1>n2
COMP.Z		+,rts		(n)			\ combine flags	
;
; COMPARE ( c-addr1 u1 c-addr2 u2 - n, compare two strings)	
COMPARE.LF	dc.l	COMP.NF
COMPARE.NF	dc.b	7 128 +
		dc.b	char E char R char A char P char M char O char C
COMPARE.SF	dc.w	COMPARE.Z COMPARE.CF del NOINLINE +
COMPARE.CF	rot		( addr1 addr2 u2 u1)
		over		( addr1 addr2 u2 u1 u2)
		over		( addr1 addr2 u2 u1 u2 u1)		
		jsl	COMP.CF ( addr1 addr2 u2 u1 ><flag)		
		>R		( addr1 addr2 u2 u1 R: ><flag)
		#.w	MIN.CF	( addr1 addr2 umin)		
		?dup
	IF
		zero		( addr1 addr2 u 0)
		DO			( addr1 addr2)
			over		( addr1 addr2 addr1)
			fetch.b	( addr1 addr2 char1)
			over		( addr1 addr2 char1 addr2)
			fetch.b	( addr1 addr2 char1 char2)
			<>		( addr1 addr2 flag)
			IF	
				over		( addr1 addr2 addr1)
				fetch.b	( addr1 addr2 char1)
				over		( addr1 addr2 char1 addr2)	
				fetch.b	( addr1 addr2 char1 char2)	
				jsl	COMP.CF	( addr1 addr2 ><flag R: I J n)		
				nip		( addr2 ><flag R: I J n)
				nip		( ><flag R: I J n)
				R>
				drop
				R>
				drop
				R>
				drop,rts	( ><flag R:)	\ exit immediately
			THEN
				1+		( addr1 addr2+)
				swap		( addr2+ addr1)
				1+		( addr2+ addr1+)
				swap		( addr1+ addr2+)
		LOOP
	THEN			( addr1 addr2 R: n)
		drop		( addr1 R: n)
		drop		( R: n)
		R>		( n)
COMPARE.Z	rts
;
; $= ( c-addr1 u1 c-addr2 u2 - n, test two strings for equality CASE INSENSITIVE)
$=.LF		dc.l	COMPARE.NF
$=.NF		dc.b	2 128 +
		dc.b	char = char $
$=.SF		dc.w	$=.Z $=.CF del NOINLINE +
$=.CF		rot	( c-addr1 c-addr2 u2 u1)
		over	( c-addr1 c-addr2 u2 u1 u2)
		<>	( c-addr1 c-addr2 u2 flag)
		IF					; unequal length
			drop	( c-addr1 c-addr2)
			drop	( c-addr1)
			drop 	( )
			zero,rts 	( 0)
		THEN
		zero	( c-addr1 c-addr2 u2 0)
		DO	( c-addr1 c-addr2)
			over		( c-addr1 c-addr2 c-addr1)
			fetch.b	( c-addr1 c-addr2 char1)
			jsl	UPPER.CF
			over		( c-addr1 c-addr2 char1 c-addr2)
			fetch.b	( c-addr1 c-addr2 char1 char2)
			jsl	UPPER.CF
			<>		( c-addr1 c-addr2 flag)
			IF				; unequal characters
				drop	( c-addr1)
				drop	( )
				R>	; remove loop variables
				drop
				R>
				drop
				zero,rts	( 0)
			THEN
			1+	( c-addr1 c-addr2+)
			swap	( c-addr2+ c-addr1)
			1+	( c-addr2+ c-addr1+)
			swap	( c-addr1+ c-addr2+)
		LOOP
		drop	( c-addr1)
		drop 	( )
		zero				
$=.Z		not,rts				; equal strings
;
; COUNT ( addr -- c-addr n)
;
COUNT.LF	dc.l	$=.NF
COUNT.NF	dc.b	5 128 +
		dc.b 	char T char N char U char O char C
COUNT.SF	dc.w	COUNT.Z COUNT.CF del
COUNT.CF	dup		( addr addr)
		1+		( addr c-addr)
		swap		( c-addr addr)
COUNT.Z	fetch.b,rts	( c-addr n)
;
; FIND (addr -- addr 0 | xt 1 | xt -1)
;
FIND.LF	dc.l	COUNT.NF
FIND.NF	dc.b	4 128 + 
		dc.s 	FIND
FIND.SF	dc.w	FIND.Z FIND.CF del NOINLINE +
FIND.CF	#.l	WID.COUNT
		fetch.b		( addr count)
		?dup								; check count of the search order is not zero
		IF			( addr count)	
			#.l	WID.ORDER	( addr count &WID)
			dup			( addr count &WID &WID)
			rot			( addr &WID &WID count)
			+			( addr &WID &END)
			swap			( addr &END &WID)
			DO			( addr )			; search each world list in turn
				R@			( addr &WID)
				fetch.b		( addr WID)
				jsl	{FIND}.CF	( addr 0 | xt 1 | xt -1)
				?dup						; check the results of FIND within that wordlist
				IF		
					UNLOOP			( xt 1 | xt -1)
					rts
				THEN			( addr)			
			LOOP							; all world lists searched with no result
		THEN			( addr)
		zero
FIND.Z		rts
;
;  {FIND} (addr WID -- addr 0 | xt 1 | xt -1, takes a counted string)
{FIND}.CF	over				( addr WID addr)
		>R				( addr WID R:addr)
		swap				( WID addr R:addr)
		jsl	COUNT.CF		( WID c-addr n R:addr)
		rot				( c-addr n WID R:addr)
		jsl	SEARCH-WORDLIST.CF	(0 | xt 1 | xt -1 R:addr)
		dup
		IF
			R>				( xt 1 addr | xt -1 addr)
			drop				( xt 1 | xt -1)
		ELSE
			R>				( 0 addr)
			swap				( addr 0)
		THEN
		rts
;
; SEARCH-WORDLIST (c-addr n WID -- 0 | xt 1 | xt -1, takes a string length pair and a WID)
SEARCH-WORDLIST.LF	dc.l	FIND.NF
SEARCH-WORDLIST.NF	dc.b	128 15 +
			dc.s	SEARCH-WORDLIST
SEARCH-WORDLIST.WF	dc.w	SEARCH-WORDLIST.CF SEARCH-WORDLIST.CF del
SEARCH-WORDLIST.CF	>R		( c-addr1 n1 R:WID)
		?dup
		IF			( c-addr1 n1 R:WID)
; top of dictonary
		R>			( c-addr1 n1 WID)
		jsl	GET-ENTRY.CF	( c-addr1 n1 NF)
		BEGIN			( c-addr1 n1 NF)
; check smudge bit not set
			dup			( c-addr1 n1 NF NF)
			>R			( c-addr1 n1 NF R: NF)	
			fetch.b		( c-addr1 n1 n' R: NF)
			#.b	SMDGE		( c-addr1 n1 n' 32 R: NF)
			and			( c-addr1 n1 flag R: NF)			
			0=			( c-addr1 n1 flag' R: NF)
			IF
; prepare and perform string equality test
				over			( c-addr1 n1 c-addr1 R: NF)
				over			( c-addr1 n1 c-addr1 n1 R: NF)
				R@			( c-addr1 n1 c-addr1 n1 NF R: NF)
				dup			( c-addr1 n1 c-addr1 n1 NF NF R: NF)
				1+			( c-addr1 n1 c-addr1 n1 NF c-addr2 R: NF)
				swap			( c-addr1 n1 c-addr1 n1 c-addr2 NF R: NF)
				fetch.b		( c-addr1 n1 c-addr1 n1 c-addr2 n2' R: NF)
				#.b 	31		( c-addr1 n1 c-addr1 n1 c-addr2 n2' 31 R: NF)
				and			( c-addr1 n1 c-addr1 n1 c-addr2 n2 R: NF)
				jsl	$=.CF		( c-addr1 n1 flag R: NF)		
				IF			( c-addr1 n1 R: NF)
; match found
					drop			( c-addr1)
					drop			( )
					R>			( NF)
					dup			( NF NF)
					fetch.b		( NF n')
					dup			( NF n' n')
; check immediate bit 
					#.b	IMMED		( NF n' n' 64)
					and			( NF n' I)
					IF
						#.b 1			( NF n' 1)
					ELSE
						zero			( NF n' 0)
						not			( NF n' -1)
					THEN			( NF n' Iflag)
					>R			( NF n' R: Iflag)
; get CF and exit					
					#.b	31		( NF n' 31 R: Iflag)
					and			( NF n R: Iflag)
					+			( NF+n R: Iflag)
					#.b	3		( NF+n 3 R: Iflag)
					+			( CF R: Iflag)
					R>			( CF Iflag)
					rts		
				THEN
			THEN		
; not matched. get next word
			R>		( c-addr1 n1 NF1)
			#.b	4	( c-addr1 n1 NF1 4)
			-		( c-addr1 n1 LF)
			fetch.l	( c-addr1 n1 NF2)
			?dup		( c-addr1 n1, false | NF2 true)
			0=		( c-addr1 n1, true  | NF2 false)
		UNTIL		( c-addr1 n1 | c-addr1 n1 NF2)
		drop		( c-addr1)
		drop		( )
		zero,rts	( 0)
		ELSE				; zero length string was passed
			R>
			drop
			drop
			drop
			zero
		THEN
SEARCH-WORDLIST.Z		rts
;
WORDS.LF	dc.l 	SEARCH-WORDLIST.NF
WORDS.NF	dc.b	5 128 +
		dc.b	char S char D char R char O char W
WORDS.SF	dc.w	WORDS.Z WORDS.CF del
WORDS.CF	jsl	CR.CF
		#.w	TAB		( &tab)			; change tab setting for optimal display
		dup			( &tab &tab)
		fetch.l		( &tab tab)
		swap			( tab &tab) 
		#.b	10		( tab &tab 20)
		over			( tab &tab 20 &tab)
		store.l		( tab &tab)			; ( ignore tab &tab comment from here to end)
		#.l	WID.COUNT	
		fetch.b		( count)
		?dup								; check count of the search order is not zero
		IF			( count)	
			#.l	WID.ORDER	( count &WID)
			dup			( count &WID &WID)
			rot			( &WID &WID count)
			+			( &WID &END)
			swap			( &END &WID)
			DO			( )				; review each world list in turn
				R@			( &WID)
				fetch.b		( WID)
				jsl	{WORDS}.CF	( )			
			LOOP	
		THEN
WORDS.Z	store.l,rts						; restore original tab setting		
;		
; {WORDS} ( WID --, display the words in this wordlist)
{WORDS}.CF	jsl	GET-ENTRY.CF	( NF)
		BEGIN
			?dup
		WHILE
			dup			( NF NF)
			1+			( NF NF+1)
			over			( NF NF+1 NF)
			fetch.b		( NF NF+1 n')
			#.b	31		( NF NF+1 n' 31)
			and			( NF NF+1 n)
			?dup
			IF
				jsl	TYPE.CF 	( NF)		
				#.b	09
				jsl	EMIT.CF
			ELSE
				drop			( NF)
			THEN			( NF)
			jsl	NF>LF.CF	( LF)
			fetch.l		( NF)
		REPEAT
		rts
;
; PARSE ( char -- c-addr n, parse the input buffer into the parse buffer)
PARSE.LF	dc.l	WORDS.NF
PARSE.NF	dc.b	5 128 +
		dc.b 	char E char S char R char A char P
PARSE.SF	dc.w	PARSE.Z PARSE.CF del
PARSE.CF	>R			( R: char)
		#.w	HERE_		; use the natural location of the next name field as the parse buffer
		fetch.l		
		#.b	5		; +4 to get to the NF, then +1 again to get to the first char in the NF
		+			( parse_buff R: char)
		#.l	input_buff	
		fetch.l		( parse_buff input_buff R: char)
		#.l	IN_LEN		
		fetch.l		( parse_buff input_buff IN_LEN R: char)
		over			( parse_buff input_buff IN_LEN input_buff R: char)
		+			( parse_buff input_buff i_buff_last R: char)
		>R			( parse_buff input_buff R: char i_buff_last)
		#.l	>IN_
		fetch.l		( parse_buff input_buff IN R: char ibl)
		+			( parse_buff i_buff R: char ibl)
		BEGIN
; check for buffer overrun
			dup			( p_buff i_buff i_buff R: char ibl)
			R@			( p_buff i-buff i_buff ibl R: char ibl)
			<			( p_buff i_buff flag R: char ibl)
PARSE.0		beq	PARSE.1
; fetch next character from input buffer
			dup			( p_buff i_buff i_buff R: char ibl)
			fetch.b		( p_buff i_buff charN R: char ibl)
; test against delimiter
			R>			( p_buff i_buff charN ibl R: char)
			R@			( p_buff i_buff charN ibl char R: char)
			swap			( p_buff i_buff charN char ibl R: char)
			>R			( p_buff i_buff charN char R: char ibl)
			dup
			#.b	32	
			=			; if delimiter is space, also delimit on CR, LF and TAB
			IF
				over		( .. charN char charN)
				<>		( .. charN flag)
				over		( .. charN flag charN)
				#.b	13
				<>		( .. charN flag flag)
				and		( .. charN flag)
				over
				#.b	10
				<>
				and
				over
				#.b	9
				<>
				and	
			ELSE
				over			( p_buff i_buff charN char charN R: char ibl)
				<>			( p_buff i_buff charN flag R: char ibl)
			THEN
		WHILE
; copy char to parse buffer and increment i_buff and p_buff
			rot			( i_buff charN p_buff R: char ibl)
			dup			( i_buff charN p_buff p_buff R: char ibl)
			rot			( i_buff p_buff p_buff charN R: char ibl)
;			#.w	UPPER.CF	
;			jsr			( i_buff p_buff p_buff CHAR R: char ibl)
			swap			( i_buff p_buff CHAR p_buff R: char ibl)
			store.b		( i_buff p_buff R: char ibl)
			1+			( i_buff p_buff+ R: char ibl)
			swap			( p_buff+ i_buff R: char ibl)
			1+			( p_buff+ i_buff+ R: char ibl)
		REPEAT
; at delimiter or end of buffer, save IN, finalize return paramaters
		drop			( p_buff i_buff R: char ibl)
		1+			( p_buff i_buff+ R: char ibl)		; if delimiter, increment i_buff
PARSE.1	#.l	input_buff
		fetch.l		( p_buff i_buff+ i_buff R: char ibl)
		-			( p_buff IN R: char ibl)
		#.l	>IN_		( p_buff IN+ >IN R: char ibl)
		store.l		( p_buff R: char ibl)	
		R>			( p_buff char R: ibl)
		drop			( p_buff R: ibl)
		R>			( p_buff ibl)
		drop			( p_buff)
		#.w	HERE_	
		fetch.l		( p_buff parse_buff)
		#.b	5
		+			( p_buff parse_buff)
		swap			( parse_buff p_buff)
		over			( parse_buff p_buff parse_buff)
PARSE.Z	-,rts			( parse_buff n)
;
; WORD ( char -- addr, parse the next word from the input buffer)
WORD.LF	dc.l	PARSE.NF
WORD.NF	dc.b	4 128 +
		dc.b	char D char R char O char W
WORD.SF	dc.w	WORD.Z WORD.SF del
WORD.CF	BEGIN
			dup		( char char)
			jsl	PARSE.CF	( char c-addr n)
			dup		( char c-addr n flag)
			0=		( char c-addr n flag')
WORD.0			beq	WORD.2						; parse returned at least one character
			#.l IN_LEN
			fetch.l	( char c-addr n IN_LEN)
			#.l >IN_
			fetch.l	( char c-addr n IN_LEN IN)
			>		( char c-addr n flag)
WORD.1			beq	WORD.2
			drop		( char c-addr)
			drop		( char)
		AGAIN	
WORD.2		nop
		swap		( char n c-addr)
		1-		( char n addr)
		swap		( char addr n)
		over		( char addr n addr)		
		store.b	( char addr)				
WORD.Z		nip,rts	( addr)	
;	
; INTERPRET, interpert a line from the input buffer
INTERPRET.LF	dc.l	WORD.NF
INTERPRET.NF	dc.b	9 128 +
		dc.b	char T char E char R char P char R char E char T char N char I
INTERPRET.SF	dc.w	INTERPRET.Z INTERPRET.CF del
INTERPRET.CF	jsl	{INTERPRET}.CF			; inner workings of INTERPRET							
		#.l	STATE_
		fetch.l
		0=
		IF	; compile is not set		
			#.w	INTERPRET.1				; TYPE OK-depth CR
			#.b	4
			jsl	TYPE.CF
			jsl	DEPTH.CF
			jsl	UDOT.CF
		THEN
		#.b	EOL
		jsl	EMIT.CF
INTERPRET.Z	rts
INTERPRET.1	dc.b 	 char - char K char O  32
;
; inner workings of INTERPET
{INTERPRET}.CF BEGIN
			#.l	IN_LEN 
			fetch.l	
			#.l	>IN_ 
			fetch.l	( IN_LEN IN)
			> 		( flag)		; confirm that input_buff has characters waiting to be processes
		WHILE
			#.b	32 
			jsl	WORD.CF	( addr)	; scan input_buff from >IN, skip leading blanks	
			dup
			fetch.b		( addr n)
			IF					; confirm some characters were parsed
				#.l	STATE_
				fetch.l	
				IF				; compile mode
					dup	( addr addr)	
					jsl LOCAL.recog 	; recognize locals first
					IF					; it is a local
						nip				; drop the word address - we won't need it again
						jsl	LOCAL.ref		; get the address of this local
						jsl	LITERAL.CF		; compile the address as a literal
						#.b	opFETCH.L
						jsl	C,.CF			; compile a fetch.l instruction
					ELSE			
						dup
						jsl	FIND.CF		; lookup the word in the dictionary		
						?dup
						IF				; valid execution token
							0<
							IF					; not IMMEDIATE, compile
								nip
								jsl COMPILE,.CF
							ELSE					; IMMEDIATE, execute
								nip
								jsr				
							THEN
						ELSE				
							jsl	COUNT.CF	( addr c-addr n)		
							jsl	NUMBER?.CF	( addr, 0 | n 1)			
							IF			; valid number
								nip
								jsl	LITERAL.CF
							ELSE			; not recognized in compile mode
								#.w 	PALETTE 2 +
								fetch.b
								#.w	INK
								store.b
								jsl	CR.CF
								jsl 	COUNT.CF	; echo the input string
								jsl	TYPE.CF
								#.l	{INTERPRET}.ERR1
								THROW		
							THEN
						THEN
					THEN
				ELSE			; interpret mode
					dup
					jsl	FIND.CF			; lookup the word		
					IF					; valid execution token, execute
						nip
						jsr				
					ELSE
						jsl	COUNT.CF	( addr c-addr n)		
						jsl	NUMBER?.CF	( addr 0 | n 1)
						IF				; valid number
							nip				; drop the word address - we won't need it again
						ELSE				; not recognized in interpret mode
							#.w 	PALETTE 2 +
							fetch.b
							#.w	INK
							store.b
							jsl	CR.CF
							jsl 	COUNT.CF		; echo the input string
							jsl	TYPE.CF
							#.l	{INTERPRET}.ERR
							THROW		
						THEN
					THEN
				THEN
			ELSE	( addr)
				drop
			THEN
		REPEAT
{INTERPRET}.Z	rts
{INTERPRET}.ERR	dc.b	27
			dc.s	Not recognized by INTERPRET
{INTERPRET}.ERR1	dc.b	25
			dc.s	Not recognized by COMPILE			
;
; ERROR ( c --) display a counted string for an error message	
ERROR.LF	dc.l 	INTERPRET.NF
ERROR.NF	dc.b	5 128 +
		dc.b	char R char O char R char R char E
ERROR.SF	dc.w	ERROR.Z ERROR.CF del
ERROR.CF	#.w 	PALETTE 2 +		; error colour code (usually red)
		fetch.b
		#.w	INK
		store.b
		jsl	CR.CF
		#.w	ERROR.0		; TYPE ERROR:
		#.b	7
		jsl	TYPE.CF		
		jsl	COUNT.CF
		jsl	TYPE.CF
		#.b	EOL
		jsl	EMIT.CF	
ERROR.Z	rts
ERROR.0	dc.s 	ERROR: 
;
;ABORT
ABORT.LF	dc.l 	ERROR.NF
ABORT.NF	dc.b	5 128 +
		dc.b 	char T char R char O char B char A
ABORT.SF	dc.w	ABORT.Z ABORT.CF del
ABORT.CF	jsl	QUIT.CF
ABORT.Z	rts		; never reached
;
; QUIT
QUIT.LF	dc.l	ABORT.NF
QUIT.NF	dc.b	4 128 +
		dc.b 	char T char I char U char Q
QUIT.SF	dc.w	QUIT.Z QUIT.CF del
QUIT.CF	BEGIN
			RESETSP			; zero stack pointers
; access inner QUIT loop with catch			
			#.l	{QUIT}.CF
			CATCH
			zero				; ALWAYS code catch followed by zero in N.I.G.E. assembler
			?dup				; non-zero throw code is a counted string
			IF
				jsl	error.cf	; report the error
			THEN
		AGAIN
QUIT.Z		rts					; never reached
;			
;		zero
;		jsl	TIMEOUT.CF				; clear any timeouts
{QUIT}.CF	zero						; set state to interpreting
		#.l	STATE_
		store.l
		#.l	_input_buff				; restore default input buffer location and size
		#.l	input_buff
		store.l	
		#.l	_input_size
		#.l	input_size			
		store.l
		BEGIN					; MAIN SYSTEM LOOP
			#.w	PALETTE 0 +			; set input colour
			fetch.b
			#.w	INK
			store.b
			#.l	input_buff
			fetch.l
			#.l	input_size
			fetch.l			( input_buff size)
			jsl	ACCEPT.CF 		( input_buff size -- len , fill input_buff from the terminal)				
			#.l	IN_LEN 			; save number of characters in input buffer
			store.l					
			zero
			#.l	>IN_ 				; set >IN to zero
			store.l		
			#.w	PALETTE 1 +			; set output color
			fetch.b
			#.w	INK
			store.b
			jsl	INTERPRET.CF			; INTERPRET the line		
		AGAIN
		rts					; never reached
;
; SOURCE ( -- c-addr len , return the address and length of the input buffer)
SOURCE.LF	dc.l	QUIT.NF
SOURCE.NF	dc.b	6 128 +
		dc.b 	char E char C char R char U char O char S
SOURCE.SF	dc.w	SOURCE.Z SOURCE.CF del
SOURCE.CF	#.l	input_buff
		fetch.l
		#.l	input_size
SOURCE.Z	fetch.l,rts
;	
; SAVE_INPUT ( --), save the current input source specificiation
SAVE-INPUT.LF	dc.l	SOURCE.NF
SAVE-INPUT.NF	dc.b	10 128 +
		dc.b	char T char U char P char N char I char - char E char V char A char S
SAVE-INPUT.SF	dc.w	SAVE-INPUT.Z SAVE-INPUT.Z del
SAVE-INPUT.CF	#.l	input_buff
		fetch.l
		#.l	input_buff_a
		store.l
		#.l	input_size
		fetch.l	
		#.l	input_size_a
		store.l
		#.l	IN_LEN
		fetch.l
		#.l	IN_LEN_a
		store.l
		#.l	>IN_
		fetch.l
		#.l	>IN_a	
SAVE-INPUT.Z	store.l,rts
;
; RESTORE-INPUT ( --), restore the current input source specification
RESTORE-INPUT.LF	dc.l	SAVE-INPUT.NF
RESTORE-INPUT.NF	dc.b	13 128 +
			dc.b	char T char U char P char N char I char - char E char R char O char T char S char E char R
RESTORE-INPUT.SF	dc.w	RESTORE-INPUT.Z RESTORE-INPUT.CF del
RESTORE-INPUT.CF	#.l	input_buff_a
			fetch.l
			#.l	input_buff
			store.l
			#.l	input_size_a
			fetch.l	
			#.l	input_size
			store.l
			#.l	IN_LEN_a
			fetch.l
			#.l	IN_LEN
			store.l
			#.l	>IN_a
			fetch.l
			#.l	>IN_	
RESTORE-INPUT.Z	store.l,rts
;
; EVALUATE ( c-addr u --)
EVALUATE.LF	dc.l	RESTORE-INPUT.NF
EVALUATE.NF	dc.b	8 128 +
		dc.b	char E char T char A char U char L char A char V char E
EVALUATE.SF	dc.w	EVALUATE.Z EVALUATE.CF del
EVALUATE.CF	>R
		>R
		jsl	SAVE-INPUT.CF
		R>
		R>			( <input-state> c-addr u --)
		#.l	IN_LEN
		store.l
		#.l	input_buff
		store.l
		zero
		#.l	>IN_
		store.l
		jsl	{INTERPRET}.CF
		jsl	RESTORE-INPUT.CF
EVALUATE.Z	rts
;
NOP.LF		dc.l	EVALUATE.NF
NOP.NF		dc.b	3 128 +
		dc.b	char P char O char N
NOP.SF		dc.w	1
NOP.CF		nop,rts
;
DROP.LF	dc.l	NOP.NF
DROP.NF	dc.b	4 128 +
		dc.b	char P char O char R char D
DROP.SF	dc.w	1
DROP.CF	drop,rts
;
DUP.LF		dc.l	DROP.NF
DUP.NF		dc.b	3 128 +
		dc.b	char P char U char D
DUP.SF		dc.w	1
DUP.CF		dup,rts
;
?DUP.LF	dc.l	DUP.NF
?DUP.NF	dc.b	4 128 +
		dc.b	char P char U char D char ?
?DUP.SF	dc.w	1
?DUP.CF	?dup,rts
;
SWAP.LF	dc.l	?DUP.NF
SWAP.NF	dc.b	4 128 +
		dc.b	char P char A char W char S
SWAP.SF	dc.w	1
SWAP.CF	swap,rts
;
OVER.LF	dc.l	SWAP.NF
OVER.NF	dc.b	4  128 +
		dc.b	char R char E char V char O
OVER.SF	dc.w	1
OVER.CF	over,rts
;
NIP.LF		dc.l	OVER.NF
NIP.NF		dc.b	3 128 +
		dc.b	char P char I char N
NIP.SF		dc.w	1
NIP.CF		nip,rts
;
ROT.LF		dc.l	NIP.NF
ROT.NF		dc.b	3 128 +
		dc.b	char T char O char R
ROT.SF		dc.w	1
ROT.CF		rot,rts
;
-ROT.LF	dc.l	ROT.NF
-ROT.NF	dc.b	4 128 +
		dc.b	char T char O char R char -
-ROT.SF	dc.w	-ROT.Z -ROT.CF del
-ROT.CF	rot
-ROT.Z		rot,rts
;
>R.LF		dc.l	-ROT.NF
>R.NF		dc.b	2 128 +
		dc.b	char R char >
>R.SF		dc.w	2
>R.CF		>R
		rts
;
R@.LF		dc.l	>R.NF
R@.NF		dc.b	2 128 +
		dc.b	char @ char R
R@.SF		dc.w	2
R@.CF		R@
		rts
;
I.LF		dc.l	R@.NF
I.NF		dc.b	1 128 +
		dc.b	char I
I.SF		dc.w	2 MUSTINLINE +
I.CF		R@
		rts
;
J.LF		dc.l	I.NF
J.NF		dc.b	1 128 +
		dc.b	char J
J.SF		dc.w	J.Z J.CF del MUSTINLINE +
J.CF		R>	( I R:L1 J L2)
		R>	( I L1 R:J L2)
		R@	( I L1 J R:J L2)
		rot	( L1 J I R:J L2)
		rot	( J I L1 R:J L2)
		>R	( J I R:L1 J L2)
		>R	( J R:I L1 J L2)
J.Z		rts
;		
R>.LF		dc.l	J.NF
R>.NF		dc.b	2 128 +
		dc.b	char > char R
R>.SF		dc.w	2
R>.CF		R>
		rts
;
DEPTH.LF	dc.l	R>.NF
DEPTH.NF	dc.b	5 128 +
		dc.b	char H char T char P char E char D
DEPTH.SF	dc.w	1
DEPTH.CF	PSP@,rts
;
CATCH.LF	dc.l	DEPTH.NF
CATCH.NF	dc.b	5 128 +
		dc.s	CATCH
CATCH.SF	dc.w	2 MUSTINLINE +
CATCH.CF	catch
		zero,rts			; zero required following catch for FORTH behaviour
;
THROW.LF	dc.l	CATCH.NF
THROW.NF	dc.b	5 128 +
		dc.s	THROW
THROW.SF	dc.w	2 MUSTINLINE +
THROW.CF	throw
		rts
;
;RDEPTH.LF	dc.l	DEPTH.NF
;RDEPTH.NF	dc.b	6 128 +
;		dc.b	char H char T char P char E char D char R
;RDEPTH.SF	dc.w	2
;RDEPTH.CF	RSP@
;		rts				; separate RST since it changes the return stack size
;
+.LF		dc.l	THROW.NF
+.NF		dc.b	1 128 +
		dc.b 	char +
+.SF		dc.w	1
+.CF		+,rts
;
-.LF		dc.l	+.NF
-.NF		dc.b	1 128 +
		dc.b	char -
-.SF		dc.w	1
-.CF		-,rts
;
NEGATE.LF	dc.l	-.NF
NEGATE.NF	dc.b	6 128 +
		dc.b	char E char T char A char G char E char N
NEGATE.SF	dc.w	1
NEGATE.CF	negate,rts
;
1+.LF		dc.l	NEGATE.NF
1+.NF		dc.b	2 128 +
		dc.b	char + char 1
1+.SF		dc.w	1
1+.CF		1+,rts
;
1-.LF		dc.l	1+.NF
1-.NF		dc.b	2 128 +
		dc.b	char - char 1
1-.SF		dc.w	1
1-.CF		1-,rts
;
2*.LF		dc.l	1-.NF
2*.NF		dc.b	2 128 +
		dc.b	char * char 2
2*.SF		dc.w	1
2*.CF		2*,rts
;
2/.LF		dc.l	2*.NF
2/.NF		dc.b 	2 128 +
		dc.b	char / char 2
2/.SF		dc.w	1
2/.CF		2/,rts
;
U2/.LF		dc.l	2/.NF
U2/.NF		dc.b 	3 128 +
		dc.s	U2/
U2/.SF		dc.w	1
U2/.CF		lsr,rts
;
M*.LF		dc.l	U2/.NF
M*.NF		dc.b	2 128 +
		dc.s 	M*
M*.SF		dc.w	1
M*.CF		mults,rts
;
UM*.LF		dc.l	M*.NF
UM*.NF		dc.b	3 128 +
		dc.s 	UM*
UM*.SF		dc.w	1
UM*.CF		multu,rts
;
*.LF		dc.l	UM*.NF
*.NF		dc.b	1 128 +
		dc.b 	char *
*.SF		dc.w	2
*.CF		mults
		drop,rts
;
/.LF		dc.l	*.NF
/.NF		dc.b	1 128 +
		dc.b	char /
/.SF		dc.w	2
/.CF		divs
		nip,rts
;
MOD.LF		dc.l	/.NF
MOD.NF		dc.b	3 128 +
		dc.b	char D char O char M
MOD.SF		dc.w	2
MOD.CF		divs
		drop,rts
;
*/.LF		dc.l	MOD.NF
*/.NF		dc.b	2 128 +
		dc.b	char / char *
*/.SF		dc.w	*/.Z */.CF del
*/.CF		>R
		mults
		drop
		R>
		divs
*/.Z		nip,rts
;
*/MOD.LF	dc.l	*/.NF
*/MOD.NF	dc.b	5 128 +
		dc.b	char D char O char M char / char *
*/MOD.SF	dc.w	*/MOD.Z */MOD.CF del
*/MOD.CF	>R
		mults
		drop
		R>
*/MOD.Z	divs,rts
;
/MOD.LF	dc.l	*/MOD.NF
/MOD.NF	dc.b	4 128 +
		dc.b char D char O char M char /
/MOD.SF	dc.w	1
/MOD.CF	divs,rts
;
=.LF		dc.l	/MOD.NF
=.NF		dc.b	1 128 +
		dc.b	char =
=.SF		dc.w	1
=.CF		=,rts
;
<>.LF		dc.l	=.NF
<>.NF		dc.b	2 128 +
		dc.b	char > char <
<>.SF		dc.w	1
<>.CF		<>,rts
;
<.LF		dc.l	<>.NF
<.NF		dc.b	1 128 +
		dc.b	char <
<.SF		dc.w	1
<.CF		<,rts
;
>.LF		dc.l	<.NF
>.NF		dc.b	1 128 +
		dc.b	char >
>.SF		dc.w	1
>.CF		>,rts
;
U<.LF		dc.l	>.NF
U<.NF		dc.b	2 128 +
		dc.b	char < char U
U<.SF		dc.w	1
U<.CF		U<,rts
;
U>.LF		dc.l	U<.NF
U>.NF		dc.b	2 128 +
		dc.b	char > char U
U>.SF		dc.w	1
U>.CF		U>,rts
;
0=.LF		dc.l	U>.NF
0=.NF		dc.b	2 128 +
		dc.b	char = char 0
0=.SF		dc.w	1
0=.CF		0=,rts
;
NOT.LF		dc.l	0=.NF
NOT.NF		dc.b	3 128 +
		dc.b	char T char O char N
NOT.SF		dc.w	1
NOT.CF		0=,rts
;
0<>.LF		dc.l	NOT.NF
0<>.NF		dc.b	3 128 +
		dc.b	char > char < char 0
0<>.SF		dc.w	1
0<>.CF		0<>,rts
;
0<.LF		dc.l	0<>.NF
0<.NF		dc.b	2 128 +
		dc.b	char < char 0
0<.SF		dc.w	1
0<.CF		0<,rts
;
0>.LF		dc.l	0<.NF
0>.NF		dc.b	2 128 +
		dc.b	char > char 0
0>.SF		dc.w	1
0>.CF		0>,rts
;
FALSE.LF	dc.l	0>.NF
FALSE.NF	dc.b	5 128 +
		dc.b	char E char S char L char A char F
FALSE.SF	dc.w	1
FALSE.CF	false,rts
;
0.LF		dc.l	FALSE.NF
0.NF		dc.b	1 128 +
		dc.s	0
0.SF		dc.w	1
0.CF		false,rts
;
-1.LF		dc.l	0.NF
-1.NF		dc.b	2 128 +
		dc.s	-1
-1.SF		dc.w	2		
-1.CF		zero
		0=,rts
;
TRUE.LF	dc.l	-1.NF
TRUE.NF	dc.b	4 128 +
		dc.b	char E char U char R char T
TRUE.SF	dc.w	2
TRUE.CF	zero
		0=,rts
;
AND.LF		dc.l	TRUE.NF
AND.NF		dc.b	3 128 +
		dc.b	char D char N char A
AND.SF		dc.w	1
AND.CF		and,rts
;
OR.LF		dc.l	AND.NF
OR.NF		dc.b	2 128 +
		dc.b	char R char O
OR.SF		dc.w	1
OR.CF		or,rts
;
INVERT.LF	dc.l	OR.NF
INVERT.NF	dc.b	6 128 +
		dc.b	char T char R char E char V char N char I
INVERT.SF	dc.w	1
INVERT.CF	invert,rts
;
XOR.LF		dc.l	INVERT.NF
XOR.NF		dc.b	3 128 +
		dc.b	char R char O char X
XOR.SF		dc.w	1
XOR.CF		xor,rts
;
; LSHIFT (x1 u -- x2)
LSHIFT.LF	dc.l	XOR.NF
LSHIFT.NF	dc.b	6 128 +
		dc.b	char T char F char I char H char S char L
LSHIFT.SF	dc.w	LSHIFT.Z LSHIFT.CF del
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
RSHIFT.LF	dc.l	LSHIFT.NF
RSHIFT.NF	dc.b	6 128 +
		dc.b	char T char F char I char H char S char R
RSHIFT.SF	dc.w	RSHIFT.Z RSHIFT.CF del
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
CR.LF		dc.l	RSHIFT.NF
CR.NF		dc.b	2 128 +
		dc.b	char R char C
CR.SF		dc.w	CR.Z CR.CF del
CR.CF		#.b	EOL
		jsl	EMIT.CF
CR.Z		rts
;
SPACE.LF	dc.l	CR.NF
SPACE.NF	dc.b	5 128 +
		dc.b	char E char C char A char P char S
SPACE.SF	dc.w	SPACE.Z SPACE.CF del
SPACE.CF	#.b	32
		jsl	EMIT.CF
SPACE.Z	rts
;	
SPACES.LF	dc.l	SPACE.NF
SPACES.NF	dc.b	6 128 +
		dc.b	char S char E char C char A char P char S
SPACES.SF	dc.w	SPACES.Z SPACES.CF del
SPACES.CF	zero	( limit index)
		DO
			#.b 	32
			jsl	EMIT.CF
		LOOP
SPACES.Z	rts
;
XBYTE.LF	dc.l	SPACES.NF
XBYTE.NF	dc.b	5 128 +
		dc.b	char E char T char Y char B char X
XBYTE.SF	dc.w	1
XBYTE.CF	xbyte,rts
;
XWORD.LF	dc.l	XBYTE.NF
XWORD.NF	dc.b	5 128 +
		dc.b	char D char R char O char W char X
XWORD.SF	dc.w	1
XWORD.CF	xword,rts
;
@.LF		dc.l	XWORD.NF
@.NF		dc.b	1 128 +
		dc.b 	char @
@.SF		dc.w 	1
@.CF		fetch.l,rts
;
2@.LF		dc.l	@.NF
2@.NF		dc.b	2 128 +
		dc.s	2@
2@.SF		dc.w	2@.Z 2@.CF del
2@.CF		dup	
		#.b	4
		+
		fetch.l
		swap
2@.Z		fetch.l,rts
;
!.LF		dc.l	2@.NF
!.NF		dc.b	1 128 +
		dc.b 	char !
!.SF		dc.w 	1
!.CF		store.l,rts
;
2!.LF		dc.l	!.NF
2!.NF		dc.b	2 128 +	
		dc.s	2!
2!.SF		dc.w	2!.Z 2!.CF del
2!.CF		dup
		rot
		rot
		store.l
		#.b	4
		+	
2!.Z		store.l,rts
;
;@-- ( addr -- n, return the value at addr and decrement the value in memory)
@--.LF		dc.l	2!.NF
@--.NF		dc.b	3 128 +
		dc.s	@--
@--.SF		dc.w 	@--.Z @--.CF del
@--.CF		dup		( addr addr)
		fetch.l	( addr n)
		dup		( addr n n)
		1-		( addr n n-1)
		rot		( n n-1 addr)
@--.Z		store.l,rts	( n)
;
;@++ ( addr -- n, return the value at addr and increment the value in memory)
@++.LF		dc.l	@--.NF
@++.NF		dc.b	3 128 +
		dc.s	@++
@++.SF		dc.w 	@++.Z @++.CF del
@++.CF		dup		( addr addr)
		fetch.l	( addr n)
		dup		( addr n n)
		1+		( addr n n+1)
		rot		( n n+1 addr)
@++.Z		store.l,rts	( n)
;
; +! ( n addr --)
+!.LF		dc.l	@++.NF
+!.NF		dc.b	2 128 +
		dc.b 	char ! char +
+!.SF		dc.w 	+!.Z +!.CF del
+!.CF		swap		( addr n)
		over		( addr n addr)
		fetch.l	( addr n X)
		+		( addr Y)
		swap		( Y addr)
+!.Z		store.l,rts	( )	
;	
W@.LF		dc.l	+!.NF
W@.NF		dc.b	2 128 +
		dc.b 	char @ char W
W@.SF		dc.w 	1
W@.CF		fetch.w,rts
;
W!.LF		dc.l	W@.NF
W!.NF		dc.b	2 128 +
		dc.b 	char ! char W
W!.SF		dc.w 	1
W!.CF		store.w,rts
;		
; W+! ( n addr --)
W+!.LF		dc.l	W!.NF
W+!.NF		dc.b	3 128 +
		dc.b 	char ! char + char W
W+!.SF		dc.w 	W+!.Z W+!.CF del
W+!.CF		swap		( addr n)
		over		( addr n addr)
		fetch.w	( addr n X)
		+		( addr Y)
		swap		( Y addr)
W+!.Z		store.w,rts	( )	
;
C@.LF		dc.l	W+!.NF
C@.NF		dc.b	2 128 +
		dc.b 	char @ char C
C@.SF		dc.w 	1
C@.CF		fetch.b,rts
;
C!.LF		dc.l	C@.NF
C!.NF		dc.b	2 128 +
		dc.b 	char ! char C
C!.SF		dc.w 	1
C!.CF		store.b,rts
;
; C+! ( n addr --)
C+!.LF		dc.l	C!.NF
C+!.NF		dc.b	3 128 +
		dc.b 	char ! char + char C
C+!.SF		dc.w 	C+!.Z C+!.CF del
C+!.CF		swap		( addr n)
		over		( addr n addr)
		fetch.b	( addr n X)
		+		( addr Y)
		swap		( Y addr)
C+!.Z		store.b,rts	( )
;	
DECIMAL.LF	dc.l	C+!.NF
DECIMAL.NF	dc.b	7 128 +
		dc.b	char L char A char M char I char C char E char D
DECIMAL.SF	dc.w	DECIMAL.Z DECIMAL.CF del
DECIMAL.CF	#.b	10
		#.l	BASE_
DECIMAL.Z	store.l,rts		
;
HEX.LF		dc.l	DECIMAL.NF
HEX.NF		dc.b	3 128 +
		dc.b	char X char E char H
HEX.SF		dc.w	HEX.Z HEX.CF del
HEX.CF		#.b	16
		#.l	BASE_
HEX.Z		store.l,rts
;
BINARY.LF	dc.l	HEX.NF
BINARY.NF	dc.b	6 128 +
		dc.b	char Y char R char A char N char I char B
BINARY.SF	dc.w	BINARY.Z BINARY.CF del
BINARY.CF	#.b	2
		#.l	BASE_
BINARY.Z	store.l,rts	
;	
CHAR.LF	dc.l	BINARY.NF
CHAR.NF	dc.b	4 128 +
		dc.b	char R char A char H char C
CHAR.SF	dc.w	CHAR.Z char.CF del
CHAR.CF	#.b	32
		jsl	WORD.CF	( addr)		
		1+			( c-addr)
CHAR.Z		fetch.b,rts		( char)
;
CHARS.LF	dc.l	CHAR.NF
CHARS.NF	dc.b	5 128 +
		dc.s	CHARS
CHARS.SF	dc.w	1
CHARS.CF	rts
;
BL.LF		dc.l	CHARS.NF
BL.NF		dc.b	2 128 +
		dc.b	char L char B
BL.SF		dc.w	2
BL.CF		#.b	32
		rts
;
2DROP.LF	dc.l	BL.NF
2DROP.NF	dc.b	5 128 +
		dc.b	char P char O char R char D char 2
2DROP.SF	dc.w	2
2DROP.CF	drop
		drop,rts
;
2DUP.LF	dc.l	2DROP.NF
2DUP.NF	dc.b	4 128 +
		dc.b	char P char U char D char 2
2DUP.SF	dc.w	2DUP.Z 2DUP.CF del
2DUP.CF	over	
2DUP.Z		over,rts
;
2OVER.LF	dc.l	2DUP.NF
2OVER.NF	dc.b	5 128 +
		dc.b	char R char E char V char O char 2
2OVER.SF	dc.w	2OVER.Z 2OVER.CF del
2OVER.CF	#.b	3
		jsl	PICK.CF
		#.b	3
		jsl	PICK.CF
2OVER.Z	rts
;
2SWAP.LF	dc.l	2OVER.NF
2SWAP.NF	dc.b	5 128 +
		dc.b	char P char A char W char S char 2
2SWAP.SF	dc.w	2SWAP.Z 2SWAP.CF del
2SWAP.CF	>R		( 1 2 3 R:4)
		rot		( 2 3 1 R:4)
		rot		( 3 1 2 R:4)
		R>		( 3 1 2 4)
		rot		( 3 2 4 1)
2SWAP.Z	rot,rts	( 3 4 1 2)
;
; PICK ( x2 x1 x0 n -- xn)
PICK.LF	dc.l	2SWAP.NF
PICK.NF	dc.b	4 128 +
		dc.b	char K char C char I char P
PICK.SF	dc.w	PICK.Z PICK.CF del
PICK.CF	?dup	( xn ... x1 x0 n n | xn ... x1 x0 0)
		IF	( xn ... x1 x0 n)				; n > 0
			dup		( xn ... x1 x0 n n)
			#.l	local0					; save a copy of n in local0
			store.l	( xn ... x1 x0 n)				
			BEGIN		
				dup	( xn ... x1 n n R:x0)
			WHILE
				swap	( xn ... n x1 : R:x0)
				>R	( xn ... n R:x0 x1)
				1-
			REPEAT
			drop
			dup		( xn xn)
			#.l	local1					; save xn in local1
			store.l	( xn)
			#.l	local0
			fetch.l	( xn n)
			BEGIN		
				dup	( xn ... n n R:x0 x1)
			WHILE
				R>	( xn ... n x1 R:x0)			
				swap	( xn ... n : R:x0)
				1-
			REPEAT
			drop
			#.l	local1					; place xn on the top of stack
			fetch.l	( xn ... x2 x1 xn)
		ELSE	( xn ... x1 x0)
			dup						; 0 PICK = DUP
		THEN
PICK.Z		rts
;		
;PICK.CF	psp@		( n depth)
;		swap		( depth n)
;		-
;		#.b	4
;		multu		( offsetLO offsetHI)
;		drop		( offset)
;		#.l	PSTACK
;		#.b	4
;		+
;		+		( addr)
;PICK.Z		fetch.l,rts	( n)
;	
; DUMP ( addr n --, display memory)
DUMP.LF	dc.l	PICK.NF
DUMP.NF	dc.b	4 128 +
		dc.b	char P char M char U char D
DUMP.SF	dc.w	DUMP.Z DUMP.CF del
DUMP.CF	over	( addr n addr)
		+	( addr end)
		>R	( addr R: end)
		BEGIN
			#.b	EOL
			jsl	EMIT.CF		
			dup		( addr addr)
			#.b	6					; 6 digit output for the address
			jsl	UDOTR.CF	( addr)	
			#.b	32
			jsl	EMIT.CF
			dup		( addr addr)			
			#.b	8	( addr 8)
			jsl	TYPERAW.CF				; print the literals						
			#.b	32
			jsl	EMIT.CF
			#.b	8
			zero
			DO
				#.b 32
				jsl 	EMIT.CF
				dup		( addr addr)
				fetch.b	( addr n)
				#.b	2				; 2 digit output for the byte
				jsl	UDOTR.CF
				1+		( addr+)
			LOOP		( addr addr+8)
			dup		( addr+8 addr+8 R: end)
			R@		( addr+8 addr+8 end R: end)
			<		( addr+8 flag R: end)
			not		( addr+8 flag' R: end)
		UNTIL
		#.b	EOL
		jsl	EMIT.CF
		R>			( addr end)
		drop		
DUMP.Z		drop,rts			
;	
; .S ( -- non destructive stack print)
DOTS.LF	dc.l	DUMP.NF
DOTS.NF	dc.b	2 128 +
		dc.b	char S char .
DOTS.SF	dc.w	DOTS.Z DOTS.CF del
DOTS.CF	PSP@		( xn ... x1 x0 n)
		?dup		( xn ... x1 x0 n n | 0)
		IF		( xn ... x1 x0 n)
			dup		( xn ... x1 x0 n n)
			#.l	local0					; save a copy of n in local0
			store.l	( xn ... x1 x0 n)				
			BEGIN		
				dup	( xn ... x1 n n R:x0)
			WHILE
				swap	( xn ... n x1 : R:x0)
				#.b	EOL
				jsl	EMIT.CF
				dup
				jsl	DOT.CF					
				>R	( xn ... n R:x0 x1)
				1-
			REPEAT
			drop
			#.l	local0
			fetch.l	( xn n)
			BEGIN		
				dup	( xn ... n n R:x0 x1)
			WHILE
				R>	( xn ... n x1 R:x0)			
				swap	( xn ... x1 n : R:x0)
				1-
			REPEAT
			drop
		THEN
		#.b	EOL
		jsl	EMIT.CF
DOTS.Z		rts
;	
;DOTS.CF	psp@			( depth)
;		#.b	4		( depth 4)
;		multu
;		drop			( offset)
;		#.l	PSTACK
;		#.b	8
;		+
;		+			( addr)
;		BEGIN
;			dup		( addr addr)
;			#.l	PSTACK
;			#.b	8
;			+		( addr addr PSTACK+8)
;			>
;		WHILE
;			#.b	EOL
;			jsl	EMIT.CF
;			dup		( addr addr)
;			fetch.l	( addr n)
;			jsl	DOT.CF	
;			#.b	4	( addr 4)
;			-		( addr-4)
;		REPEAT
;		#.b	EOL
;		jsl	EMIT.CF	
;DOTS.Z		drop,rts
;
; UNUSED ( -- n, number of unused bytes in dataspace)
UNUSED.LF	dc.l	DOTS.NF
UNUSED.NF	dc.b	6 128 +
		dc.b	char D char E char S char U char N char U
UNUSED.SF	dc.w	UNUSED.Z UNUSED.CF del
UNUSED.CF	#.l	SRAMSIZE
		#.w	HERE_
		fetch.l
UNUSED.Z	-,rts
;
; CHECKMEM - internal word
CHECKMEM.CF	jsl	UNUSED.CF	( free)	
		#.b	32		( free 32)
		<
		IF
			#.w	CHECKMEM.0
			#.b	8
			jsl	TYPE.CF
			jsl	ABORT.CF
		THEN
		rts
CHECKMEM.0	dc.b char M char E char M 32 char W char O char L
CHECKMEM.Z	dc.b EOL
;
; CREATE ( -- construct a dictionary entry for name)
CREATE.LF	dc.l	UNUSED.NF
CREATE.NF	dc.b	6 128 +
		dc.b	char E char T char A char E char R char C
CREATE.SF	dc.w	CREATE.Z CREATE.CF del
CREATE.CF	jsl	CHECKMEM.CF		
		#.b	32
		jsl	WORD.CF	( addr)		; parse next word				
		drop						; HEAD assumes that the name field is already complete		
		jsl	HEAD.CF	( CF)			; create header		
		dup			( CF CF)
		1-
		1-			( CF SF)	
		#.b	6		( CF SF 6)		; create runtime code is 6 bytes (without >DOES)
		swap			( CF 6 SF)
		store.w		( CF)			; store 6 in size field
		#.b	op#.L		( CF op#.L)
		over			( CF op#.L CF)
		store.b		( CF)			; compile #.L	- do not use LITERAL since #.w or #.l would depend on address size
		1+			( CF')			; but in this case the distance to the PFA must be known exactly
		dup
		#.b	9		( CF CF 9)		; PFA will be so many bytes from this point
		+			( CF PFA)
		over			( CF PFA CF)
		store.l		( CF)			; store PFA
		#.b	4		( CF 4)
		+			( CF')
		#.b	opRTS		( CF op)
		over			( CF op CF)
		store.b					; compile RTS
		#.b	5		( CF 5)		; extra bytes to leave space for >DOES redirection code if necessary + RTS 
		+			( CF')	
		#.w	HERE_		( CF &CF)			
CREATE.Z	store.l,rts		( )			; update variable HERE			
;					
; DOES> ( -- , run time behaviour of a defining word)
DOES>.LF	dc.l	CREATE.NF
DOES>.NF	dc.b	5 128 + 
		dc.b	char > char S char E char O char D
DOES>.SF	dc.w	DOES>.Z DOES>.CF del
DOES>.CF	jsl	GET-COMP-ENTRY.CF	( NF)
		jsl	NF>SF.CF	( SF)			; find SF in create word
;		#.w	LAST-SF				
;		fetch.l		( SF)
		#.b	10
		over			( SF 10 SF)
		store.w					; update SF: for create does> runtime code size (6 - 1 + 5)
		#.b	7		( SF 7)		; starting position for does code is 7 bytes ahead of SF
		+			( CF)			; CF is position of RTS left by create	
		R>			( CF dest)		; postone words following DOES>
		over			( CF dest CF)
		store.l		( CF)			; compile dest 
		#.b	opJSL
		over			( CF opJSL CF)
		store.b		( CF)			; overwrite high byte with JSL instruction, leaving 3 byte address
		#.b	4		( CF 4)
		+			( CF')
		#.b	opRTS		( CF opRTS)
		swap
DOES>.Z	store.b,rts					; compile RTS  (need to use a JSL/RTS pair rather than JMP incase inlined)	
;
; HEAD ( -- CF, make a dictionary header assuming the name field has already been set by WORD) - internal word
HEAD.CF	#.w	HERE_
		fetch.l		( addr LF)
		dup
		>R			( addr LF R:LF)
		jsl	GET-COMP-ENTRY.CF	( addr LF NF R:LF)
		swap			( addr NF LF R:LF)	
		store.l		( addr R:LF)		; link the new word (link field) to the prior WID entry point (name field)
		R>			( addr LF)
		#.b	4
		+			( addr NF)	
		dup			( addr NF NF)
		jsl 	SET-COMP-ENTRY.CF	( addr NF )		; update WID entry point with Name Field	
		dup			( NF NF)
		fetch.b		( NF len)
		dup		
		>R			( NF len R:len)
		#.b	128
		or			( NF len') 		; set precedence bit in name field
		over			( NF len' NF)
		store.b		( NF)
		R>			( NF len)
		+
		1+			( SF)			; add len+1 to move from NF to SF
;		dup			( SF SF)	
;		#.w	LAST-SF	( SF LAST-SF)	
;		store.l		( SF)			; save Size Field
		#.b	2		( SF 2)
		+			( CF)
		dup			( CF CF)
		#.w	HERE_					; update HERE
		store.l,rts		( CF)
;
; MOVE ( addr-s addr-d n, memory copy)
; byte-by-byte, not optimized (TBD)
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
			IF	; copy up
				R>	( s d n)
				ZERO	( s d n 0)
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
			ELSE	; copy down
				R@	( s d n R:n)
				+	( s d+n R:n)
				1-	( s d+n-1 R;n)
				swap	( d+n-1 s R:n)
				R@	( d+n-1 s n R:n)
				+	( d+n-1 s+n R:n)
				1-	( d+n-1 s+n-1 R:n)
				swap	( s+n-1 d+n-1 R:n)
				R>	( s d n)
				ZERO	( s d n 0)
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
		drop
MOVE.Z		drop,rts
;
; FILL ( addr n b --, fill a region of memory with n bytes)
FILL.LF	dc.l	MOVE.NF
FILL.NF	dc.b	4 128 +
		dc.b	char L char L char I char F
FILL.SF	dc.w	FILL.Z FILL.CF del
FILL.CF	rot	( n b addr)
		rot	( b addr n)
		?dup
		IF	( b addr n)
			ZERO 		( b addr n 0)
			DO  		( addr b)
				over		( b addr b )
				over		( b addr b addr)
				store.b	( b addr)
				1+		( b addr+1)
			LOOP
		THEN
		drop
FILL.Z		drop,rts			
;
;
; FILL.W ( addr n w --, fill a region of memory with n words)
FILL.W.LF	dc.l	FILL.NF
FILL.W.NF	dc.b	6 128 +
		dc.b	char W char . char L char L char I char F
FILL.W.SF	dc.w	FILL.W.Z FILL.W.CF del
FILL.W.CF	rot	( n w addr)
		rot	( w addr n)
		?dup
		IF	( w addr n)
			ZERO 		( w addr n 0)
			DO  		( addr b)
				over		( w addr w )
				over		( w addr w addr)
				store.w	( w addr)
				1+		( w addr+1)
				1+		( w addr+2)
			LOOP
		THEN
		drop
FILL.W.Z	drop,rts
;
; , (u -- , allocate 4 bytes and store long from the stack)
COMMA.LF	dc.l	FILL.W.NF
COMMA.NF	dc.b	1 128 +
		dc.b 	44
COMMA.SF	dc.w 	COMMA.Z COMMA.CF del
COMMA.CF	#.w	HERE_	( u &HERE)
		dup		( u &HERE &HERE)
		>R		( u &HERE R:&HERE)
		fetch.l	( u HERE R:&HERE)
		swap		( HERE u R:&HERE)
		over		( HERE u HERE R:&HERE)
		store.l	( HERE : R:&HERE)		; compile
		#.b 4		( HERE 4 R:&HERE)
		+		( HERE' R:&HERE)
		R>		( HERE &HERE)
COMMA.Z	store.l,rts					; update HERE
;
; W, (w -- , allocate 2 bytes and store a word from the stack)
W,.LF		dc.l	COMMA.NF
W,.NF		dc.b	2 128 +
		dc.b 	44 char W
W,.SF		dc.w 	W,.Z W,.CF del
W,.CF		#.w	HERE_	( w &HERE)
		dup		( w &HERE &HERE)
		>R		( w &HERE R:&HERE)
		fetch.l	( w HERE R:&HERE)
		swap		( HERE w R:&HERE)
		over		( HERE w HERE R:&HERE)
		store.w	( HERE R:&HERE)		; compile
		1+		
		1+		( HERE' R:&HERE)
		R>		( HERE  &HERE)
W,.Z		store.l,rts					; update HERE
;
; C, (b -- , allocate and store 1 byte from the stack)
C,.LF		dc.l	W,.NF
C,.NF		dc.b	2 128 +
		dc.b 	44 char C
C,.SF		dc.w 	C,.Z C,.CF del
C,.CF		#.w	HERE_	( b &HERE)
		dup		( u &HERE &HERE)
		>R		( u &HERE R:&HERE)
		fetch.l	( u HERE R:&HERE)
		swap		( HERE u R:&HERE)
		over		( HERE u HERE R:&HERE)
		store.b	( HERE R:&HERE)		; compile
		1+		( HERE' R:&HERE)
		R>		( HERE' &HERE)
C,.Z		store.l,rts					; update HERE
;
; M, (addr u -- , allocate and store u bytes from addr.  u is not saved)
M,.LF		dc.l	C,.NF
M,.NF		dc.b	2 128 +
		dc.b 	44 char M
M,.SF		dc.w 	M,.Z M,.CF del
M,.CF		#.w	HERE_	( addr u &HERE)
		dup		( addr u &HERE &HERE)
		>R		( addr u &HERE R:&HERE)
		fetch.l	( addr u HERE R:&HERE)
		dup		( addr u HERE HERE R:&HERE)
		>R		( addr u HERE R:&HERE HERE)
		swap		( addr HERE u R:&HERE HERE)
		dup		( addr HERE u u R:&HERE HERE)
		>R		( addr HERE u R:&HERE HERE u)
		jsl	MOVE.CF ( R:&HERE HERE u)		
		R>		( u R:&HERE HERE)
		R>		( u HERE R:&HERE)
		+		( HERE' R:&HERE)
		R>		( HERE &HERE)
M,.Z		store.l,rts					; update HERE
; $, ( addr u -- , compile a counted string)
$,.LF		dc.l	M,.NF
$,.NF		dc.b	2 128 +
		dc.b	44 char $
$,.SF		dc.w	$,.Z $,.CF del
$,.CF		dup			( addr n n)
		jsl	C,.CF					; compile the count					
		jsl	M,.CF					; compile the characters of the string						
$,.Z		rts
;
; ALLOT ( n -- allocate n bytes)
ALLOT.LF	dc.l	$,.NF
ALLOT.NF	dc.b	5 128 +
		dc.b	char T char O char L char L char A
ALLOT.SF	dc.w	ALLOT.Z ALLOT.CF del
ALLOT.CF	#.w	HERE_	( n HERE)
		jsl	+!.CF
ALLOT.Z	rts
;
; VARIABLE ( <name> -- , create a variable)
VARIABLE.LF	dc.l	ALLOT.NF
VARIABLE.NF	dc.b	8 128 +
		dc.b	char E char L char B char A char I char R char A char V
VARIABLE.SF	dc.w	VARIABLE.Z VARIABLE.CF del
VARIABLE.CF	jsl	COLON.CF				; initiate the word
		#.w	HERE_
		fetch.l
		#.b	6					; allow enough space for a 4 byte PFA address, load and RTS
		+
		dup				( PFA PFA)
		jsl	LITERAL.CF				; compile the PFA
		jsl	SEMICOLON.CF
		zero				( PFA 0)	; zero the variable
		over				( PFA 0 PFA)
		store.l
		#.b	4			( PFA 4)	; allocate space for the the PFA
		+				( HERE')
		#.w  	HERE_			( HERE' &HERE)						
VARIABLE.Z	store.l,rts
; USER ( n <name> --, create a user variable at offset n characters from the start of the user area)
USER.LF	dc.l	VARIABLE.NF
USER.NF	dc.b	4 128 +
		dc.s	USER
USER.SF 	dc.w	USER.Z USER.CF del
USER.CF	jsl	COLON.CF
		#.l	USERRAM
		+
		jsl	LITERAL.CF
		jsl	SEMICOLON.CF
USER.Z		rts
; +USER ( n <name> --, create a user variable of size n characters at the next available slot)
+USER.LF	dc.l	USER.NF
+USER.NF	dc.b	5 128 +
		dc.s	+USER
+USER.SF	dc.w	+USER.Z +USER.CF del
+USER.CF	jsl	COLON.CF
		#.l	USERNEXT_
		fetch.l
		dup
		jsl	LITERAL.CF
		jsl	SEMICOLON.CF
		+
		#.l	USERNEXT_
+USER.Z	store.l,rts
; COLON
COLON.LF	dc.l	+USER.NF
COLON.NF	dc.b	1 128 +
		dc.b	char :
COLON.SF	dc.w	COLON.Z COLON.CF del
COLON.CF	jsl	CHECKMEM.CF
		#.b	32
		jsl	WORD.CF	( addr)		; parse next word					
		drop 						; HEAD assumes that the name field is already complete
		jsl	HEAD.CF	( CF)			; create header	
		drop
;		#.w	LAST-CF	( CF &LAST-CF)
;		store.l					; update LAST-CF		
COLON.1	jsl	GET-COMP-ENTRY.CF	( NF)			; SMUDGE the word	
		dup			( NF NF)
		fetch.b		( NF size)
		#.b	SMDGE
		or			( NF size')
		swap			( size' NF)
		store.b
		zero			( 0)			; set compilation state
		0=			( true)		
		#.l	STATE_		( true &STATE)	
		store.l	
		#.l	_COMPILE				; reset the compile stack pointer	
		#.w	COMPILEstackP	
		store.l					
		zero						; set local.count to zero
		#.l	LOCAL.count	
COLON.Z	store.l,rts			
;
; SEMICOLON
SEMICOLON.LF	dc.l	COLON.NF
SEMICOLON.NF	dc.b	1 128 + IMMED +
		dc.b	59
SEMICOLON.SF	dc.w	SEMICOLON.Z SEMICOLON.CF del	
SEMICOLON.CF	#.b	opRTS		( opRTS)		
		jsl 	C,.CF					; compile RTS					
		#.w	HERE_
		fetch.l		( HERE)
		jsl	GET-COMP-ENTRY.CF	( HERE NF)
		>R			( HERE R:NF)
		R@			( HERE NF R:NF)
		jsl	NF>CF.CF	( HERE CF R:NF)
		-			( size R:NF)
		R@			( size NF R:NF)
		jsl	NF>SF.CF	( size SF R:NF)
		store.w		( R:NF)		; update size field
		R@			( NF R:NF)		; un-SMUDGE the word
		fetch.b		( c R:NF)
		#.b	SMDGE invert	( c no-smudge R:NF)
		and			( c' R:NF)	
		R>			( c' NF)
		store.b
		zero			( false)		; un-set compilation state
		#.l	STATE_		( false &STATE)	
SEMICOLON.Z	store.l,rts			
;	
; COMPILE, ( xt --, compile an execution token)
COMPILE,.LF	dc.l	SEMICOLON.NF
COMPILE,.NF	dc.b	8 128 +
		dc.b	44 char E char L char I char P char M char O char C
COMPILE,.SF	dc.w	COMPILE,.Z COMPILE,.CF del
COMPILE,.CF	dup			( CF CF)
		1-
		1-			( CF SF)
		fetch.w		( CF len)
		#.w	MUSTINLINE 	
		over
		and			( CF len flag)
		IF							;  must inline - set len = zero
			drop
			zero
		THEN			( CF len)
		#.w	HERE_		( CF len &HERE)
		dup			( CF len &HERE &HERE)
		>R			( CF len &HERE R:&HERE)
		fetch.l		( CF len HERE R:&HERE)
		>R			( CF len R:&HERE HERE)		
		#.w	INLINESIZE
		fetch.l 		(CF len INLINESIZE R:&HERE HERE)
		U>			(CF flag)
		IF	; subroutine thread
			R@		(CF HERE R:&HERE HERE)
			store.l
			#.b	opJSL		
			R@		(opJSL HERE R:&HERE HERE)
			store.b					; overwrite high byte with JSL leaving 24 bit address
			R>		( HERE R:&HERE)
			#.b	4
			+
			R>		( HERE &HERE)
			store.l
		ELSE	; compile inline
			R@			( CF HERE R:&HERE HERE)
			over			( CF HERE CF R:&HERE HERE)
			1-
			1-			( CF HERE NF R:&HERE HERE)
			fetch.w		( CF HERE len R:&HERE HERE)
			#.w	hex 3fff
			and							; wipeout INLINE flags
			dup			( CF HERE len len R:&HERE HERE)
			>R			( CF HERE len R:&HERE HERE len)
			jsl	MOVE.CF	( R:&HERE HERE len)		; copy code field		
			R>			( len R:&HERE HERE)
			R>			( len HERE R:&HERE)
			+		
			1-			( last R:&HERE)
			dup			( last last R:&HERE)
			fetch.b		( last op R:&HERE)
			#.b	63		( last op 63 R:&HERE)
			and			( last op' R:&HERE)		; remove RTS instruction
			?dup
			IF	; ,RTS
				over			( last op' last R:&HERE)
				store.b		( last R:&HERE)	; revise final opcode
				1+			( last' R:&HERE)
			THEN
			R>			( last' &HERE)
			store.l						; update HERE				
		THEN
COMPILE,.Z	rts
;
; triplet ( n -- , compile a number as a 24 bit value, internal word used by compile ,)
TRIPLET.CF	dup 
		#.b 	255 
		and 
		swap				( c nnnn)
		#.b	8 
		jsl	rshift.cf 
		dup 
		#.b	255 
		and 
		swap				( c b nnnn)
		#.b	8 
		jsl	rshift.cf 
		#.b	255 
		and 				( c b a, a is the big end)
		jsl	c,.cf
		jsl	c,.cf
		jsl	c,.cf
		rts
;
; ' ( NAME -- xt, find a word and return it's execution token or abort)
TICK.LF	dc.l	COMPILE,.NF
TICK.NF	dc.b	1 128 +
		dc.b	39
TICK.SF	dc.w	TICK.Z TICK.CF del NOINLINE +
TICK.CF	#.b	32 		
		jsl	WORD.CF	( addr)				
		dup			( addr addr)
		fetch.b		( addr n)
		IF						; confirm some characters were parsed
			jsl	FIND.CF	( addr 0 | XT true)		
			IF
				rts		( XT)
			THEN
		THEN
		#.l	TICK.ERR
		THROW
TICK.Z		rts
TICK.ERR	dc.b	14
		dc.s	Word not found
;
; LITERAL ( n -- , compile a LITERAL)
LITERAL.LF	dc.l	TICK.NF
LITERAL.NF	dc.b	7 128 + IMMED +
		dc.b	char L char A char R char E char T char I char L
LITERAL.SF	dc.w	LITERAL.Z LITERAL.CF del
LITERAL.CF	#.w 	HERE_		( n &HERE)
		dup			( n &HERE &HERE)
		>R			( n &HERE R:&HERE)
		fetch.l		( n HERE)
		swap			( HERE n)
		dup			( HERE n n)
		#.b	255		( HERE n n 255)
		U>			( HERE n flag)
		IF	
			dup		( HERE n n)
			#.w	65535		( HERE n n 65535)
			U>			( HERE n flag)
			IF			; long
				over			( HERE n HERE)
				#.b	op#.L		( HERE n HERE op)		; opcode LOAD.L
				over			( HERE n HERE op HERE)
				store.b		( HERE n HERE)		; compile opcode
				1+
				store.l		( HERE)			; compile long
				#.b 	5		( HERE 5)
			ELSE			; word
				over			( HERE n HERE)
				#.b	op#.W		( HERE n HERE op)		; LOAD.W
				over			( HERE n HERE op HERE)
				store.b		( HERE n HERE)		; compile opcode
				1+
				store.w		( HERE)			; compile word
				#.b	3		( HERE 3)
			THEN
		ELSE	; byte
			over			( HERE n HERE)
			#.b	op#.B		( HERE n HERE op)		; LOAD.B
			over			( HERE n HERE op HERE)
			store.b		( HERE n HERE)		; compile opcode
			1+
			store.b		( HERE)			; compile byte
			#.b	2		( HERE 3)		
		THEN
		+		( HERE' R:&HERE)
		R>		( HERE' &HERE)
LITERAL.Z	store.l,rts				
; CONSTANT 
CONSTANT.LF	dc.l	LITERAL.NF
CONSTANT.NF	dc.b	8 128 +
		dc.b	char T char N char A char T char S char N char O char C
CONSTANT.SF	dc.w	CONSTANT.Z CONSTANT.CF del
CONSTANT.CF	jsl	COLON.CF				; initiate the word
		jsl	LITERAL.CF				; compile the constant
		jsl	SEMICOLON.CF				; finish the word
CONSTANT.Z	rts
;
; IMMEDIATE , mark the most recently defined word as IMMEDIATE
IMMEDIATE.LF	dc.l	CONSTANT.NF
IMMEDIATE.NF	dc.b	9 128 +
		dc.b 	char E char T char A char I char D char E char M char M char I
IMMEDIATE.SF	dc.w	IMMEDIATE.Z IMMEDIATE.CF del
IMMEDIATE.CF	jsl	GET-COMP-ENTRY.CF	( NF)	
		dup		( NF NF)
		fetch.b	( NF nf)
		#.B	IMMED
		or		( NF nf')
		swap		( nf NF)
IMMEDIATE.Z	store.b,rts	
;
; ['] , compile a reference to the next word so that at run time its XT will be placed on the stack
['].LF		dc.l	IMMEDIATE.NF	
['].NF		dc.b	3 128 + IMMED + 
		dc.b	93 39 91
['].SF		dc.w	['].Z ['].CF del
['].CF		jsl	TICK.CF
		jsl	LITERAL.CF
['].Z		rts
;
; POSTPONE , force compilation of an immediate word
POSTPONE.LF	dc.l	['].NF
POSTPONE.NF	dc.b	8 128 + IMMED +
		dc.b	char E char N char O char P char T char S char O char P
POSTPONE.SF	dc.w	POSTPONE.Z POSTPONE.CF del
POSTPONE.CF	jsl	TICK.CF
		jsl	COMPILE,.CF
POSTPONE.Z	rts
;
; [CHAR] , IMMEDIATE CHAR
[CHAR].LF	dc.l	POSTPONE.NF
[CHAR].NF	dc.b	6 128 + IMMED +
		dc.b 	93 char R char A char H char C 91
[CHAR].SF	dc.w	[CHAR].Z [CHAR].CF del
[CHAR].CF	jsl	CHAR.CF
[CHAR].Z	rts
;
; or.w! ( word addr -- ) or word with memory - internal word
or.w!.CF	swap		( addr word)
		over		( addr word addr)
		fetch.w	( addr word before)
		or		( addr after)
		swap		( after before)
		store.w,rts	( )
;
; fwd-offset ( org  -- offset) calculate forward offset from HERE - internal word
fwd-offset.CF	#.w	HERE_	( org &HERE)
		fetch.l	( org dest)
		swap		( dest org)
		-		( diff)
		1-		( offset)			; not needed in v1.1
		#.w	16383	( offset mask)
		and,rts	( offset')
;
; rev-offset ( org  -- offset) calculate reverse offset from HERE - internal word
rev-offset.CF	#.w	HERE_	( dest &HERE)
		fetch.l	( dest org)
		-		( diff)
		1-		( offset)			; not needed in v1.1
		#.w	16383	( offset mask)
		and,rts	( offset')
;
; IF
IF.LF		dc.l	[CHAR].NF
IF.NF		dc.b	2 128 + IMMED +
		dc.b	char F char I
IF.SF		dc.w	IF.Z IF.CF del
IF.CF		#.w	HERE_
		fetch.l	
		#.w	opBEQ			; compile BEQ for forward branch
		jsl	W,.CF				
IF.Z		rts		( HERE)
;
; THEN
THEN.LF	dc.l	IF.NF
THEN.NF	dc.b	4 128 + IMMED +
		dc.b 	char N char E char H char T
THEN.SF	dc.w	THEN.Z THEN.CF del
THEN.CF	dup		( org org)
		jsl	fwd-offset.CF		( org offset, calculate branch offset)		
		swap		( offset org)
		jsl	or.w!.CF		; compile forward branch offset at origin				
THEN.Z		rts
;	
ELSE.LF	dc.l	THEN.NF
ELSE.NF	dc.b	4 128 + IMMED +
		dc.b 	char E char S char L char E
ELSE.SF	dc.w	ELSE.Z ELSE.CF del
ELSE.CF	#.w	HERE_	( org &HERE)	
		fetch.l	( org HERE)
		#.w	opBRA			; compile BRA for forward branch to THEN
		jsl	W,.CF	( org HERE)
		swap		( HERE org)
		dup		( HERE org org)
		jsl	fwd-offset.CF		( HERE org offset) ; calculate forward branch offset from IF				
		swap		( HERE offset org)
		jsl	or.w!.CF		; compile forward branch offset at IF			
ELSE.Z		rts		( HERE)
;
BEGIN.LF	dc.l	ELSE.NF
BEGIN.NF	dc.b	5 128 + IMMED +
		dc.b	char N char I char G char E char B
BEGIN.SF	dc.w	BEGIN.Z BEGIN.CF del
BEGIN.CF	#.w	HERE_
BEGIN.Z	fetch.l,rts			; save destination for backward branch
;
AGAIN.LF	dc.l	BEGIN.NF
AGAIN.NF	dc.b	5 128 + IMMED +
		dc.b	char N char I char A char G char A
AGAIN.SF	dc.w	AGAIN.Z AGAIN.CF del
AGAIN.CF	jsl	rev-offset.CF		( offset)	; calculate backward branch offset	
		#.w	opBRA
		or		( op)
		jsl	W,.CF			; compile unconditional backward branch 				
AGAIN.Z	rts
;
UNTIL.LF	dc.l	AGAIN.NF
UNTIL.NF	dc.b	5 128 + IMMED +
		dc.b	char L char I char T char N char U
UNTIL.SF	dc.w	UNTIL.Z UNTIL.CF del
UNTIL.CF	jsl	rev-offset.CF		( offset)	; calculate backward branch offset		
		#.w	opBEQ
		or				( op)
		jsl	W,.CF			; compile conditional backward branch			
UNTIL.Z	rts
;
WHILE.LF	dc.l	UNTIL.NF
WHILE.NF	dc.b	5 128 + IMMED +
		dc.b	char E char L char I char H char W
WHILE.SF	dc.w	WHILE.Z WHILE.CF del
WHILE.CF	jsl	IF.CF		( offset)	; WHILE is equivalent to IF		
WHILE.Z	rts
;
REPEAT.LF	dc.l	WHILE.NF
REPEAT.NF	dc.b	6 128 + IMMED +
		dc.b	char T char A char E char P char E char R
REPEAT.SF	dc.w	REPEAT.Z REPEAT.CF del
REPEAT.CF	swap	( org dest)		; org at WHILE, dest at BEGIN
		jsl	AGAIN.CF		; compile reverse branch to BEGIN 		
		jsl	THEN.CF		; compile forward branch from WHILE			
REPEAT.Z	rts
;
DO.LF		dc.l	REPEAT.NF
DO.NF		dc.b	2 128 + IMMED +
		dc.b 	char O char D
DO.SF		dc.w	DO.Z DO.CF del 1 +
DO.CF		#.w	DO0.RUN			; code to set flag for an uncoditional loop
DO.Z		bra	{DO}.CF		
;
?DO.LF		dc.l	DO.NF
?DO.NF		dc.b	3 128 + IMMED +
		dc.b 	char O char D char ?
?DO.SF		dc.w	?DO.Z ?DO.CF del 1 +
?DO.CF		#.w	DO2.RUN			; code to set flag for a conditional loop
?DO.Z		bra	{DO}.CF	
;
; {DO} common to DO and ?DO  - internal word
{DO}.CF	jsl	COMPILE,.CF			; code to set flag for IF
		#.w	DO1.RUN			; code to initialize return stack
		jsl	COMPILE,.CF	
		zero
		jsl	pushCONTROL.CF		; place control structure spacer on compile stack		
		#.w	HERE_	
		fetch.l
		jsl	pushCONTROL.CF		; place origin of forward branch on compile stack
		#.w	opBEQ				; compile BEQ for forward branch
		jsl	W,.CF						
		#.w	HERE_				; save destination for backward branch from LOOP/+LOOP
{DO}.Z		fetch.l,rts
;
DO0.SF		dc.w	DO0.Z DO0.RUN del		; must be compiled inline
DO0.RUN	zero		( limit index 0)	; initialization code for do
		0=		( limit index -1)
		rot		( index -1 limit)
DO0.Z		rot,rts	( -1 limit index)
;
DO1.SF		dc.w	DO1.Z DO1.RUN del		; must be compiled inline
DO1.RUN	swap		( flag index limit)	; common code for do and ?do
		>R		( flag index R:limit)
		>R		( flag R:limit index)
DO1.Z		rts
;
DO2.SF		dc.w	DO2.Z DO2.RUN del		; must be compiled inline	
DO2.RUN	over		( limit index limit)	; initialization code for ?do
		over		( limit index limit index)
		<>		( limit index flag)
		rot		( indes flag limit)
DO2.Z		rot,rts	( flag limit index)
;
LOOP.LF	dc.l	?DO.NF
LOOP.NF	dc.b	4 128 + IMMED +
		dc.b 	char P char O char O char L
LOOP.SF	dc.w	LOOP.Z LOOP.CF del
LOOP.CF	#.w 	LOOP1.RUN			; code to increment and test loop paramaters
LOOP.Z		bra	{LOOP}.CF
;
+LOOP.LF	dc.l	LOOP.NF			; code to add n and test loop paramaters
+LOOP.NF	dc.b	5 128 + IMMED +
		dc.b	char P char O char O char L char +
+LOOP.SF	dc.w	+LOOP.Z +LOOP.CF del
+LOOP.CF	#.w 	LOOP2.RUN
+LOOP.Z	bra	{LOOP}.CF
;
;{LOOP} common to LOOP and +LOOP, internal word
{LOOP}.CF	jsl	COMPILE,.CF				; code to add/increment and test loop paramaters
		jsl	UNTIL.CF				; conditional backwards branch to DO
		#.w	COMPILESTACKP				; compile forward branch to LEAVE's (including implicit conditional LEAVE before DO)
		>R			( R:&P)
		R@			( &P R:&P)
		fetch.l		( P R:&P)
		BEGIN		
			dup		( P P R:&P)
			fetch.l	( P dest R:&P)
			swap		( dest P R:&P)
			#.b	4	( dest P 4 R:&P)	
			-		( dest P` R:&P)	; working from top of stack to bottom (LIFO)
			swap		( p dest R:&P)
			?dup		( P dest dest | P 0 R:&P)
		WHILE			( P dest R:&P)	; check for control structure separator zero
			jsl THEN.CF	( P R:&P)
		REPEAT			( P R:&P)
		R>			( P &P)
		store.l
		#.w	UNLOOP.CF				; code to remove loop parameters
		jsl	COMPILE,.CF		
{LOOP}.Z	rts
;
; LOOP1 ( R: limit index), increment and test loop paramaters	
LOOP1.SF	dc.w	LOOP1.Z LOOP1.RUN del	; must be compiled inline
LOOP1.RUN	R>		( index R: limit)
		1+		( index+ R: limit)
		dup		( index+ index+ R: limit)
		R@		( index+ index+ limit R: limit)
		<		( index+ flag R: limit)
		not  		( index+ flag' R: limit)
		swap 		( flag index+  R: limit)
		>R   		( flag R: limit index+)
LOOP1.Z	rts
;
; LOOP2 ( n R: limit index), add n and test loop paramaters
LOOP2.SF	dc.w	LOOP2.Z LOOP2.RUN del MUSTINLINE +	; must be compiled inline
LOOP2.RUN	dup
		0<
		swap		( flag1 n R: limit index)		; sign of index variable
		R>		( flag1 n index R: limit)
		+		( flag1 index+ R: limit)
		dup		( flag1 index+ index+ R: limit)
		R@		( flag1 index+ index+ limit R: limit)
		<		( flag1 index+ flag R: limit)
		not  		( flag1 index+ flag2 R: limit)
		swap 		( flag1 flag2 index+  R: limit)
		>R   		( flag1 flag2 R: limit index+)
LOOP2.Z	xor,rts	( flag R: limit index+)		; if index variable was negative, invert flag
;
; UNLOOP, remove loop paramaters
UNLOOP.LF	dc.l	+LOOP.NF
UNLOOP.NF	dc.b	6 128 +
		dc.b	char P char O char O char L char N char U
UNLOOP.SF	dc.w	UNLOOP.Z UNLOOP.CF del MUSTINLINE + ; must be compiled inline
UNLOOP.CF	R>						 ; assume no return address
		R>
		drop
UNLOOP.Z	drop,rts
;
; LEAVE, exit current loop
LEAVE.LF	dc.l	UNLOOP.NF
LEAVE.NF	dc.b	5 128 + IMMED +
		dc.b	char E char V char A char E char L
LEAVE.SF	dc.w	LEAVE.Z LEAVE.CF del
LEAVE.CF	#.w	HERE_	
		fetch.l
		jsl	pushCONTROL.CF			; push HERE to the controlSTACK
		#.w	opBRA					; compile BRA for forward branch
		jsl	W,.CF			
LEAVE.Z	rts
;
; pushCONTROL ( n --), push a number onto the COMPILEstack - internal word
pushCONTROL.CF #.w	COMPILEstackP	( n &P)
		>R			( n R:&P)
		R@			( n &P R:&P)
		fetch.l		( n P R:&P)		
		#.b	4		( n P 4 R:&P)
		+			( n P` R:&P)		
		swap			( P n R:&P)
		over			( P n P R:&P)
		store.l		( P R:&P)
		R>			( P' &P)
		store.l,rts
;				
; CASE, mark the start of a CASE...OF..ENDOF...ENDCASE structure
CASE.LF	dc.l	LEAVE.NF
CASE.NF	dc.b	4 128 + IMMED +
		dc.b	char E char S char A char C
CASE.SF	dc.w	1
		zero						; set casecount to zero					
CASE.Z		rts						
;
OF.LF		dc.l	CASE.NF
OF.NF		dc.b	2 128 + IMMED +
		dc.b	char F char O
OF.SF		dc.w	OF.Z OF.CF del
OF.CF		>R						; push casecount to the return stack
		#.w	opOVER=
		jsl	W,.CF					; compile OVER = for making the test
		jsl	IF.CF					; push HERE and prepare BEQ for forward branch = IF 
		#.b	opDROP
		jsl	C,.CF					; compile DROP (if test is sucessful and code continues)
		R>						; get casecount from return stack
OF.Z		1+,rts						; increment CASECOUNT
;
ENDOF.LF	dc.l	OF.NF
ENDOF.NF	dc.b	5 128 + IMMED +
		dc.b	char F char O char D char N char E
ENDOF.SF	dc.w	ENDOF.Z ENDOF.CF del
ENDOF.CF	>R				; push casecount to the return stack
		jsl	ELSE.CF		; push here and prepare BRA for forward branch, then resolve prior forward branch from OF = ELSE
		R>				; get casecount from the return stack
ENDOF.Z	rts
;
ENDCASE.LF	dc.l	ENDOF.NF
ENDCASE.NF	dc.b	7 128 + IMMED +
		dc.b	char E char S char A char C char D char N char E
ENDCASE.SF	dc.w	ENDCASE.Z ENDCASE.CF del
ENDCASE.CF	#.b	opDROP
		jsl	C,.CF			; compile DROP  (remove test variable in default case)						
		zero				( casecount zero)
		DO
			jsl	THEN.CF	; resolve forward branch from each ENDOF = THEN					
		LOOP
ENDCASE.Z	rts
;
; [ , exit compilation state
[.LF		dc.l	ENDCASE.NF
[.NF		dc.b	1 128 + IMMED +
		dc.b	91
[.SF		dc.w	[.Z [.CF del
[.CF		zero
		#.l	STATE_
[.Z		store.l,rts
;
; ] , enter compilation state
].LF		dc.l	[.NF
].NF		dc.b	1 128 + IMMED +
		dc.b	93
].SF		dc.w	].Z ].CF del
].CF		zero			( 0)			
		0=			( true)		
		#.l	STATE_		( true &STATE)	
].Z		store.l,rts
;
; RECURSE, compile a jsl to the XT of the current word
RECURSE.LF	dc.l	].NF
RECURSE.NF	dc.b	7 128 + IMMED +
		dc.b	char E char S char R char U char C char E char R
RECURSE.SF	dc.w	RECURSE.Z RECURSE.CF del
RECURSE.CF	jsl	GET-COMP-ENTRY.CF	( NF)
		jsl	NF>CF.CF	( XT)
;		#.w	LAST-CF
;		fetch.l		( xt)
		#.w	HERE_
		dup
		>R
		fetch.l
		dup
		>R			(xt HERE R:&HERE HERE)			
		store.l
		#.b	opJSL		
		R@			(opJSL HERE R:&HERE HERE)
		store.b					; overwrite high byte with JSL leaving 24 bit address
		R>			( HERE R:&HERE)
		#.b	4
		+
		R>			( HERE &HERE)
RECURSE.Z	store.l,rts
;
; EXIT
EXIT.LF	dc.l	RECURSE.NF
EXIT.NF	dc.b	4 128 +
		dc.b	char T char I char X char E
EXIT.SF	dc.w	EXIT.Z EXIT.CF del
EXIT.CF	R>						; obtain return address
EXIT.Z		jmp						; simple 'RTS' would be discarded by inline compiler
;
; RTI, return from interrupt
RTI.LF		dc.l	EXIT.NF
RTI.NF		dc.b	3 128 +
		dc.b	char I char T char R
RTI.SF		dc.w	2
		RTI
		RTS						; never reached - to comply with needs of inline compiler
;
; EXECUTE ( xt --)
EXECUTE.LF	dc.l	RTI.NF
EXECUTE.NF	dc.b	7 128 +
		dc.b	char E char T char U char C char E char X char E
EXECUTE.SF	dc.w	2
EXECUTE.CF	jsr
EXECUTE.Z	rts						
;
; CELL+
CELL+.LF	dc.l	EXECUTE.NF
CELL+.NF	dc.b	5 128 +
		dc.b 	char + char L char L char E char C
CELL+.SF	dc.w	CELL+.Z CELL+.CF del
CELL+.CF	#.b	4
CELL+.Z	+,rts
;
; CELLS
CELLS.LF	dc.l	CELL+.NF
CELLS.NF	dc.b	5 128 +
		dc.b 	char S char L char L char E char C
CELLS.SF	dc.w	CELLS.Z CELLS.CF del
CELLS.CF	#.b	4
		mults
CELLS.Z	drop,rts
;	
; SLITERAL ( addr u , compile a string literal as an executable that will be re-presented at run time as a string addr u)
SLITERAL.LF	dc.l	CELLS.NF
SLITERAL.NF	dc.b	8 128 + IMMED +	
		dc.b 	char L char A char R char E char T char I char L char S
SLITERAL.SF	dc.w	SLITERAL.Z SLITERAL.CF del
SLITERAL.CF	dup		( addr u u)
		#.b	1					; 2 in v1.1
		+		( addr u offset)
		#.w	opBRA
		or		( addr u op)
		jsl	W,.CF	( addr u)		
		dup		( addr u u)
		>R		( addr u R:u)
		#.w	HERE_ 	( addr u &HERE R:u)
		fetch.l	( addr u HERE R:u)
		>R		( addr u R:u HERE)
		jsl	M,.CF	( R:u HERE)			; compile the characters of the string		
		R>		( HERE R:u)
		jsl	LITERAL.CF	( R:u)		
		R>		( u)
		jsl	LITERAL.CF
SLITERAL.Z	rts
;
; CLITERAL ( addr u, compile a string literal as an executable that will be re-presented at run time as a counted string c-addr)
CLITERAL.LF	dc.l	SLITERAL.NF
CLITERAL.NF	dc.b	8 128 + IMMED +	
		dc.b 	char L char A char R char E char T char I char L char C
CLITERAL.SF	dc.w	CLITERAL.Z CLITERAL.CF del
CLITERAL.CF	dup		( addr u u)
		#.b	2					; 3 in v1.1
		+		( addr u offset)
		#.w	opBRA					
		or		( addr u op)
		jsl	W,.CF	( addr u)			; compile a branch over the body of the string			
		#.w	HERE_ 	( addr u &HERE R:u)		; save the address of the string
		fetch.l	( addr u HERE R:u)
		>R		( addr u R:u HERE)
		jsl	$,.CF	( R:u HERE)			; compile the count and characters of the string		
		R>		( HERE R:u)
		jsl	LITERAL.CF				; compile the address	
CLITERAL.Z	rts
;
; ," <string> ( --) compile the following string into the data space as a counted string)
,".LF		dc.l	CLITERAL.NF
,".NF		dc.b	2 128 +
		dc.b	34 44
,".SF		dc.w	,".Z ,".CF del
,".CF		jsl	IN$.CF			( addr n)	; string held in parse buffer			
		jsl	$,.CF					; compile the string					
,".Z		rts
;
; STRINGlOC	( n -- addr, return a pointer to the next space in the string buffer -- internal function)
; n is the length of the string to be saved in the buffer.  STRINGLOC will check for potential buffer overrun and wrap
STRINGLOC.CF	#.l	STRINGP
		dup				( n STRINGP STRINGP)
		>R				( n STRINGP R:STRINGP)
		fetch.l			( n next R:STRINGP)		; next is the current position in the buffer
		#.l	_STRING		( n next bottom R:STRINGP)	; bottom is the bottom of the buffer
		-				( n offset R:STRINGP)
		over
		+				( n offset+n R:STRINGP)
		#.w	STRINGMAX		; length of the string buffer
		>				( n flag R:STRINGP)
		IF				; offset+n lies beyond the end of the allocated buffer
			#.l	_STRING
			R@			( n bottom STRINGP)
			store.l		; revert to the bottom of the buffer
		THEN
		R@				( n STRINGP R:STRINGP)
		fetch.l			( n next R:STRINGP)
		dup				( n next next R:STRINGP)
		rot				( next next n R:STRINGP)
		+				( next new-next R:STRINGP)
		R>				
STRINGLOC.Z	store.l,rts			( next)	; save new-next
;	
; S" <string> ( - addr u), mode dependant string function
S".LF		dc.l	,".NF
S".NF		dc.b	2 128 + IMMED +
		dc.b	34 char S
S".SF		dc.w	S".Z S".CF del
S".CF		jsl	IN$.CF			( addr n)	; string held in parse buffer				
		#.l	STATE_
		fetch.l
		IF						; compile mode
			jsl	SLITERAL.CF				; compile into dictionary				
		ELSE						; interpret mode
			dup						; copy to the buffer and return to user
			>R			( addr n R:n)
			dup
			jsl	STRINGLOC.CF	( addr n str R:n)    ; str is the next free buffer location
			dup		
			>R			( addr n str R:n str)
			swap			( addr str n R:n str)
			jsl	MOVE.CF	( R:n str)		
			R>
			R>			( str n)
		THEN
S".Z		rts
;
; C" <string>	( -- c-addr), mode dependant counted string function
C".LF		dc.l	S".NF
C".NF		dc.b	2 128 + IMMED +
		dc.b 	34 char C
C".SF		dc.w	C".Z C".CF del
C".CF		jsl	IN$.CF			( addr n)	; string held in parse buffer			
		#.l	STATE_
		fetch.l
		IF						; compile mode
			jsl	CLITERAL.CF			; compile into dictionary				
		ELSE						; interpret mode	
			dup
			jsl	STRINGLOC.CF  ( addr n str str)	; str is the next free buffer location		
			dup			( addr n str str)
			>R			( addr n str R:str)
			over			( addr n str n R:str)
			over			( addr n str n str R:str)
			store.b		( addr n str R:str)	; copy the length byte to the buffer
			1+			( addr n str+1 R:str)
			swap			( addr str+1 n R:str)
			jsl	MOVE.CF	( R:str)		; copy the string to the buffer's second byte
			R>			( str)		
		THEN
C".Z		rts
;
; IN$ <string> ( -- addr n), read a string - internal word
IN$.CF		#.b 	34					; char "
		jsl	WORD.CF		( addr)			
		jsl	COUNT.CF		( addr n)				
IN$.Z		rts
;
; ." <string>, print a string
.".LF		dc.l	C".NF
.".NF		dc.b	2 128 + IMMED +
		dc.b	34 46
.".SF		dc.w	.".Z .".CF del
.".CF		jsl	S".CF					; compile
		#.w	TYPE.CF
		jsl	COMPILE,.CF
.".Z		rts
;
; ( comment
BRA.LF		dc.l	.".NF
BRA.NF		dc.b	1 128 + IMMED +
		dc.b	40					; char (
BRA.SF		dc.w	BRA.Z BRA.CF del
BRA.CF		#.b	41					; char )
		jsl	PARSE.CF	( addr n)	
		drop
BRA.Z		drop,rts	
;
; \ comment
\.LF		dc.l	BRA.NF
\.NF		dc.b	1 128 + IMMED +
		dc.b	92					; char \
\.SF		dc.w	\.Z \.CF del
\.CF		#.b	EOL					; end of line
		jsl	WORD.CF
\.Z		drop,rts	
;
; .( printing comment
DOTBRA.LF	dc.l	\.NF
DOTBRA.NF	dc.b	2 128 + IMMED +
		dc.b	40 46
DOTBRA.SF	dc.w	DOTBRA.Z DOTBRA.CF del
DOTBRA.CF	#.b	41					; char )
		jsl	PARSE.CF
		jsl	TYPE.CF
DOTBRA.Z	rts
;
; ALIGN ( --, align data-space space pointer - NOP on the NIGE Machine)
ALIGN.LF	dc.l	DOTBRA.NF
ALIGN.NF	dc.b	5 128 +
		dc.b	char N char G char I char L char A
ALIGN.SF	dc.w	1
		rts
; ALIGNED  ( addr -- a-addr, align up to longword boundary)
ALIGNED.LF	dc.l	ALIGN.NF
ALIGNED.NF	dc.b	7 128 +
		dc.b	char D char E char N char G char I char L char A
ALIGNED.SF	dc.w	ALIGNED.Z ALIGNED.CF del
ALIGNED.CF	#.b	3
		+
		lsr
		lsr
		lsl
ALIGNED.Z	lsl,rts
;
; MARKER (--, create a deletion boundary)
; 	Marker does not restore search orders as these are private to individual user areas
; 	however all wordlists are initialized with a stub and can be safely searched when empty
MARKER.LF	dc.l	ALIGNED.NF
MARKER.NF	dc.b	6 128 +
		dc.s	MARKER
MARKER.SF	dc.w	MARKER.Z MARKER.CF del NOINLINE +
MARKER.CF	#.w	LAST-NF	( s)				; start of the wordlist table
		#.w	HERE_						; current space in the dictionary
		fetch.l		( s d)
		>R			( s R:d)
		R@			( s d R:d)
		#.b	64		( s d n R:d)			; size of the wordlist table, including the HERE and HERE1 variable locations
		jsl	MOVE.CF					; copy the table to the dictionary
		#.b	64		( 64 R:d)
		jsl	ALLOT.CF	( R:d)				; advance the dictionary pointer over the table just created
		jsl	COLON.CF	( R:d)				; start the word definition
		R>			( d)
		jsl	LITERAL.CF					; compile as a literal the address of wordlist table copy just made in the dictionary
		#.w	MARKER.RUN					
		jsl	LITERAL.CF					; compile address of the MARKER runtime code
		#.b	opJMP						
		jsl	C,.CF						; compile a jump
		jsl	SEMICOLON.CF					; finish the word
MARKER.Z	rts
; runtime code for MARKER
MARKER.RUN	#.w	LAST-NF		( s d)			; the address of the dictionary copy will already be on the stack
		#.b	64			( s d n)
		jsl	MOVE.CF					; restore the wordlist table from the dictionary copy
		rts
;	
; BUFFER: ( n --), create a storate table in PSDRAM (definition replaced in dynamic memory allocation wordset)
BUFFER:.LF	dc.l	MARKER.NF
BUFFER:.NF	dc.b	7 128 +
		dc.b	58 char R char E char F char F char U char B
BUFFER:.SF	dc.w	BUFFER:.Z BUFFER:.CF del
BUFFER:.CF	1+
		lsr
		lsl							; round up next even number
		jsl	COLON.CF
		#.w	HERE1
		fetch.l
		dup
		jsl	LITERAL.CF					; compile the location of the buffer as a literal
		jsl	SEMICOLON.CF
		+	
		#.w	HERE1
BUFFER:.Z	store.l,rts						; update the position of the SDRAM data pointer
;
; SBUFFER: ( n --), create a storage table in SDRAM
SBUFFER:.LF	dc.l	BUFFER:.NF
SBUFFER:.NF	dc.b	8 128 +
		dc.b	58 char R char E char F char F char U char B char S
SBUFFER:.SF	dc.w	SBUFFER:.Z SBUFFER:.CF del
SBUFFER:.CF	jsl	COLON.CF
		zero
		1-							; dummy value to ensure #.l is used
		jsl	LITERAL.CF					; compile the location of the buffer as a literal	
		jsl	SEMICOLON.CF
		#.w	HERE_						; actual address
		fetch.l		
		dup			( n addr addr)
		#.b	5
		-			( n addr loc)
		store.l
		jsl	ALLOT.CF					; update the position of the data pointer
SBUFFER:.Z	rts
;
DEFER.LF	dc.l	SBUFFER:.NF
DEFER.NF	dc.b	5 128 +
		dc.s	DEFER
DEFER.SF	dc.w	DEFER.Z DEFER.CF del
DEFER.CF	jsl	COLON.CF					; prepare the word
; 	cannot use LITERAL to store the vector because need to fix size = long
		#.w	HERE_
		fetch.l		( CF)
		#.b	op#.l		( CF opcode)			
		over			( CF opcode CF)
		store.b		( CF)				; compile a #.l instruction
		1+			( CF+1)
		#.w	DEFER.RUN	( CF XT)			; push default XT for a DEFER word	on the stack
		over
		store.l		( CF)				; store the vector	
		#.b	4		( CF 4)
		+			( CF+4)
		#.b	opJSR		( CF opcode)
		over			( CF opcode CF)
		store.b		( CF)				; compile a JMP instruction
		1+			( CF+1)
		#.w	HERE_
		store.l						; update HERE for the space used			
		jsl	SEMICOLON.CF					; terminate the word
DEFER.Z	rts
;
; DEFER.RUN is the default XT for an uninitialized vector
DEFER.RUN	#.l	DEFER.ERR
		THROW
;
DEFER.ERR	dc.b	20
		dc.s	Uninitialized vector
; IS <name> ( XT --), implement a vector in a defer word
IS.LF		dc.l	DEFER.NF
IS.NF		dc.b	2 128 +
		dc.s	IS
IS.SF		dc.w	IS.Z IS.CF del
IS.CF		jsl	TICK.CF		( XT word.cf)
; 	KEY?, KEY, EMIT and TYPE hold their indirection vectors on the exception stack
		dup			
		#.l	KEY?.CF		
		=
		IF				( XT word.cf)
			drop
			#.l 	KEY?_VECTOR	( XT vector)
			ELSE			( XT word.cf)
			dup
			#.l	KEY.CF
			=
			IF				( XT word.cf)
				drop
				#.l 	KEY_VECTOR	( XT vector)
				ELSE
				dup
				#.l	EMIT.CF
				=
				IF				( XT word.cf)
					drop
					#.l 	EMIT_VECTOR	( XT vector)
					ELSE	
					dup
					#.l	TYPE.CF
					=
					IF				( XT word.cf)
						drop
						#.l 	TYPE_VECTOR	( XT vector)
; 	other DEFER words hold their indirection vectors in the dictionary entry				
						ELSE	
							1+			( XT vector)
					THEN
				THEN
			THEN
		THEN	( XT vector)
		store.l			; write the exection token to the vector
IS.Z		rts
;
; ? ( addr --), output the contents of a memory address
?.LF		dc.l	IS.NF
?.NF		dc.b	1 128 +
		dc.b	char ?
?.SF		dc.w	?.Z ?.CF del
?.CF		fetch.l
		jsl	UDOT.CF
?.Z		rts
;
KEY?.LF	dc.l	?.NF
KEY?.NF	dc.b	4 128 +
		dc.b	char ? char Y char E char K
KEY?.SF	dc.w	KEY?.Z KEY?.CF del
KEY?.CF	#.l	KEY?_VECTOR
		fetch.l
		jsr
KEY?.Z		rts
;
KEY.LF		dc.l	KEY?.NF
KEY.NF		dc.b	3 128 +
		dc.b	char Y char E char K
KEY.SF		dc.w	KEY.Z KEY.CF del
KEY.CF		#.l	KEY_VECTOR
		fetch.l
		jsr
KEY.Z		rts
;
EMIT.LF	dc.l	KEY.NF
EMIT.NF	dc.b	4 128 +
		dc.b	char T char I char M char E
EMIT.SF	dc.w	EMIT.Z EMIT.CF del
EMIT.CF	#.l	EMIT_VECTOR
		fetch.l
		jsr
EMIT.Z		rts
;
TYPE.LF	dc.l	EMIT.NF
TYPE.NF	dc.b	4 128 +
		dc.b	char E char P char Y char T
TYPE.SF	dc.w	TYPE.Z TYPE.CF del
TYPE.CF	#.l	TYPE_VECTOR
		fetch.l
		jsr
TYPE.Z		rts
;
PAD.LF		dc.l	TYPE.NF
PAD.NF		dc.b	3 128 +
		dc.b	char D char A char P
PAD.SF		dc.w	6
PAD.CF		#.l	_PAD
		rts
;
HERE.LF	dc.l	PAD.NF
HERE.NF	dc.b	4 128 +
		dc.b	char E char R char E char H
HERE.SF	dc.w	4
HERE.CF	#.w	HERE_
		fetch.l,rts
;	
; HERE for the SDRAM space
HERE1.LF	dc.l	HERE.NF
HERE1.NF	dc.b	5 128 +
		dc.b	char 1 char E char R char E char H
HERE1.SF	dc.w	4
HERE1.CF	#.w	HERE1
		fetch.l,rts
;
; maximum word length for inline compilation 
; do not reduce below 9 since some control structures
; *must* be compiled inline e.g. LOOP1.RUN
INLINESIZE.LF	dc.l	HERE1.NF
INLINESIZE.NF	dc.b	10 128 +
		dc.b	char E char Z char I char S char E char N char I char L char N char I
INLINESIZE.SF	dc.w	4
INLINESIZE.CF	#.w	INLINESIZE
		rts
INLINESIZE	dc.l	10					 
;
BASE.LF	dc.l	INLINESIZE.NF
BASE.NF	dc.b	4 128 +
		dc.b	char E char S char A char B
BASE.SF	dc.w	4
BASE.CF	#.l	BASE_
		rts
;BASE_		dc.l	10
;
STATE.LF	dc.l	BASE.NF
STATE.NF	dc.b	5 128 +
		dc.b	char E char T char A char T char S
STATE.SF	dc.w	4
STATE.CF	#.l	STATE_
		rts
;
>IN.LF		dc.l	STATE.NF
>IN.NF		dc.b	3 128 +
		dc.b	char N char I 62
>IN.SF		dc.w	4
>IN.CF		#.l	>IN_
		rts
>IN_		dc.l	0
;
; LOCAL.CREATE ( addr --, copy the counted string at addr to the local buffer at the poistion of local.count and increment local.count)
LOCAL.CREATE	#.l	_LOCAL.BUF
		#.l	LOCAL.COUNT
		fetch.l
		?dup
		IF
			zero
			DO			; step through each occupied slot in the local buffer
				dup
				fetch.b
				+
				1+
			LOOP
		THEN				( addr1 addr2)
		over
		fetch.b
		1+				( addr1 addr2 u)
		jsl 	move.cf		; copy the counted string into the local buffer
		#.b	1			; increment LOCAL.COUNT
		#.l	LOCAL.COUNT
		jsl	+!.cf
		rts
;	
; LOCAL.RECOG	 ( addr -- n true | false, attempt to recognize the counted string at addr as a local, if found return the local number and true)
LOCAL.RECOG	#.l	_LOCAL.BUF
		#.l	LOCAL.COUNT
		fetch.l				( addr1 addr2 local.count)
		?dup
		IF
			zero
			DO				; step through each occupied slot in the local buffer
				over
				over			( addr1 addr2 addr1 addr2)
				jsl	count.cf	( addr1 addr2 addr1 c-addr2 u2)
				rot			( addr1 addr2 c-addr2 u2 addr1)
				jsl 	count.cf	( addr1 addr2 c-addr2 u2 c-addr1 u1)
				jsl	$=.cf		( addr1 addr2 flag)
				IF			; match found
					drop
					drop
					R@
					zero
					not		( n true)
					unloop
					rts
				THEN
				dup
				fetch.b
				+
				1+
			LOOP
		THEN					( addr1 addr2)
		drop
		drop
		zero,rts				(false)
;
; TO <local> ( x --, store x in local)
TO.LF		dc.l	>IN.NF		
TO.NF		dc.b	2 128 + IMMED + 
		dc.s	TO
TO.SF		dc.w	TO.Z TO.CF del
TO.CF		#.b	32
		jsl 	word.cf
		jsl	local.recog
		IF
			jsl	LOCAL.ref	; fixed address on the subroutine stack
			jsl	literal.cf	; compile the address
			#.b	opSTORE.L	
			jsl	C,.CF		; compile a STORE.L instruction
		ELSE
			#.l	TO.1
			THROW
		THEN
TO.Z		rts
TO.1		dc.b	29
		dc.s	Local variable not recognized
;
;  -> <local> ( x --, store x in local)
->.LF		dc.l	TO.NF		
->.NF		dc.b	2 128 + IMMED + 
		dc.s	->
->.SF		dc.w	->.Z ->.CF del
->.CF		jsl	TO.CF
->.Z		rts
;		
; {, start the local variable definition list (pre-ANSI 200x notation)
{.LF		dc.l	->.NF		
{.NF		dc.b	1 128 + IMMED + 
		dc.s	{
{.SF		dc.w	{.z {.cf del
{.CF		jsl	{:.cf
{.Z		rts
;
; {:, start local variable definition list (ANSI 200x notation)
{:.LF		dc.l	{.NF		
{:.NF		dc.b	2 128 + IMMED + 
		dc.s	{:
{:.SF		dc.w	{:.z {:.cf del
{:.CF		#.l	local.count
		fetch.l			; this marks the last of the currently set locals
		zero				; flag to indicate whether | has been passed	
		BEGIN
			#.b	32
			jsl	word.cf
			dup
			jsl 	count.cf
			#.l	{:.1
			#.b	2
			jsl	$=.cf
			IF				; --
				drop
				not
				IF				; | wasn't used, named inputs finish here
					#.l	local.count
					fetch.l
				THEN
				zero
				not			; stop parsing
				#.b	125		; gather up everything until end of stack comments
				jsl	parse.cf	; use parse to avoid skipping a leading }
				drop
				drop
			ELSE
				dup
				jsl	count.cf
				#.l	{:.2
				#.b	2
				jsl	$=.cf
				over
				jsl	count.cf
				#.l	{:.3
				#.b	1
				jsl	$=.cf	
				or
				IF			; :} or }
					drop
					not
					IF			; | wasn't used, named inputs finish here
						#.l	local.count
						fetch.l
					THEN
					zero
					not			; stop parsing
				ELSE
					dup 
					jsl	count.cf
					#.l	{:.4
					#.b	1
					jsl	$=.cf	
					IF			; |
						drop
						drop
						#.l	local.count
						fetch.l
						zero
						not			; | has been passed, named inputs finish here
						false			; continue parsing
					ELSE			; a local variable
						jsl	local.create
						#.l	local.count
						fetch.l
						#.b	16
						>			
						IF			; exceeded available local varaible storage
							#.l	{:.5
							THROW
						ELSE
							false			; continue parsing
						THEN
					THEN
				THEN
			THEN
		UNTIL			( first-local last-local)
		BEGIN
			over
			over
			=
			not
		WHILE				; there are named locals to be compliled
			1-			; n'th local is variable number n-1
			dup
			jsl	LOCAL.ref	; fixed address on the subroutine stack
			jsl	literal.cf	; compile the address
			#.b	opSTORE.L	
			jsl	C,.CF		; compile a STORE.L instruction
		REPEAT
		drop
{:.z		drop,rts
{:.1		dc.s	--		
{:.2		dc.s	:}
{:.3		dc.s	}
{:.4		dc.s	|
{:.5		dc.b	24
		dc.s	Too many local variables
;
; LOCAL.ref	( n -- addr, return the address of local variable number n)
LOCAL.ref	2*
		2*
		#.l	SSTACK
		+
		rts
;
;
;NF>LF.CF	( NF -- LF, return the link field of the word given its name field)
NF>LF.CF	#.b	4
		-,rts
;
;NF>SF.CF	( NF -- LF, return the size field of the word given its name field)
NF>SF.CF	dup
		fetch.b
		#.b	31
		and
		+
		1+,rts
;
;NF>CF.CF	( NF -- LF, return the code field (XT) of a word given its name field)
NF>CF.CF	dup
		fetch.b
		#.b	31
		and
		+
		#.b	3
		+,rts
;
DEFINITIONS.LF	dc.l	{:.NF
DEFINITIONS.NF	dc.b	11 128 +
			dc.s	DEFINITIONS
DEFINITIONS.SF	dc.w	DEFINITIONS.NF DEFINITIONS.CF del
DEFINITIONS.CF	#.l	WID.ORDER
			fetch.b
			#.l	WID.COMPILE
DEFINITIONS.Z		store.b,rts
;
WORDLIST.LF		dc.l	DEFINITIONS.NF
WORDLIST.NF		dc.b	8 128 +
			dc.s	WORDLIST
WORDLIST.SF		dc.w	WORDLIST.Z WORDLIST.CF del
WORDLIST.CF		#.l	WID.COUNT
			>R			( R:&count)
			R@			( &count R:&count)
			fetch.b		( count R:&count)
			1+			( count+1 R:&count)
			dup			( count+1 count+1 R:&count)
			R>			( count+1 count+1 &count)
WORDLIST.Z		store.b,rts		( count+1)	
;		
; GET-ORDER	( -- WIDn...WID1 n, return the number and set of wordlists.  WID1 is the first searched)
GET-ORDER.LF		dc.l	WORDLIST.NF
GET-ORDER.NF		dc.b	9 128 +
			dc.s	GET-ORDER
GET-ORDER.SF		dc.w	GET-ORDER.Z GET-ORDER.CF del
GET-ORDER.CF		#.l	WID.COUNT
			fetch.b		( n)
			dup			( n)			; save count in local 0
			#.l	local0				
			store.b		( n)
			#.l	WID.order	( n addr-gnd)
			+			( addr-top)
			1+			( addr-top+1)
			BEGIN
				1-			( addr~)
				dup			( addr addr)
				fetch.b		( addr WIDx)
				swap			( WIDx addr)
				dup			( WIDx addr addr)
				#.l	WID.order	( WIDx addr addr addr-gnd)
				=			( WIDx addr flag)
			UNTIL			( WIDn..WID1 addr)
			drop			( WIDn..WID1)
			#.l	local0
GET-ORDER.Z		fetch.b,rts		( WIDn..WID1 n)
;	
; SET-ORDER ( WIDn...WID1 n --, WID1 is searched first)
SET-ORDER.LF		dc.l	GET-ORDER.NF
SET-ORDER.NF		dc.b	9 128 +
			dc.s	SET-ORDER
SET-ORDER.SF		dc.w	SET-ORDER.Z SET-ORDER.CF del
SET-ORDER.CF		dup			( WIDn...WID1 n n)
			#.l	WID.COUNT
			store.b		( WIDn...WID1 n)
			#.l	WID.ORDER	( WIDn...WID1 n &WID)
			dup			( WIDn...WID1 n &WID &WID)
			rot			( WIDn...WID1 &WID &WID n)
			+			( WIDn...WID1 &WID &END)
			swap			( WIDn...WID1 &END &WID)	
			DO
					R@	( WIDn...WID1 &WIDi)
					store.b
			LOOP
SET-ORDER.Z		rts
;
SET-CURRENT.LF	dc.l	SET-ORDER.NF
SET-CURRENT.NF	dc.b	11 128 +
			dc.s	SET-CURRENT
SET-CURRENT.SF	dc.w	6
SET-CURRENT.CF	#.l 	WID.COMPILE
			store.b,rts
;
GET-CURRENT.LF	dc.l	SET-CURRENT.NF
GET-CURRENT.NF	dc.b	11 128 +
			dc.s	GET-CURRENT
GET-CURRENT.SF	dc.w	6
GET-CURRENT.CF	#.l 	WID.COMPILE
			fetch.b,rts
;
FORTH-WORDLIST.LF	dc.l	GET-CURRENT.NF
FORTH-WORDLIST.NF	dc.b	14 128 +
			dc.s	FORTH-WORDLIST
FORTH-WORDLIST.SF	dc.w	1
FORTH-WORDLIST.CF	zero,rts
;
;GET-COMP-ENTRY.CF	( -- NF, get the entry point (name field) of the compilation wordlist)
GET-COMP-ENTRY.CF	#.l	WID.COMPILE
			fetch.b		( WID)
			jsl	GET-ENTRY.CF	( NF)
			rts
;
;SET-COMP-ENTRY.CF	( NF -- set the entry point of the compilation wordlist to the name field NF)
SET-COMP-ENTRY.CF	#.l	WID.COMPILE
			fetch.b		( NF WID)
			jsl	CELLS.CF
			#.l	LAST-NF
			+			
			store.l,rts
;
;GET-ENTRY		( WID -- NF, get the entry point (name field) of the wordlist WID)
GET-ENTRY.LF		dc.l	FORTH-WORDLIST.NF
GET-ENTRY.NF		dc.b	9 128 +
			dc.s	GET-ENTRY
GET-ENTRY.SF		dc.w	GET-ENTRY.Z GET-ENTRY.CF del
GET-ENTRY.CF		jsl	CELLS.CF
			#.l	LAST-NF
			+
GET-ENTRY.Z		fetch.l,rts	
;
; ------------------------------------------------------------------------------------------------------------
; internal FORTH dictionary variables	
; ------------------------------------------------------------------------------------------------------------
COMPILEstackP		dc.l	_COMPILE	; pointer for the compiler stack (used by LEAVE)
USERNEXT_		dc.l	USERRAM 44 +	; next available location for a user variable
LOCAL.COUNT		dc.l	0		; used in the creation of the local variable buffer
sem-keyboard		dc.b	0		; binary semaphore for read access to the keyboard buffer
sem-screen		dc.b	0		; binary semaphore for write access to the screen
sem-RS232		dc.b	0		; binary semaphore for read/write access to the RS232 port
IN_LEN_a		dc.l	0		; used by SAVE-INPUT and RESTORE-INPUT
>IN_a			dc.l	0		; used by SAVE-INPUT and RESTORE-INPUT
input_buff_a		dc.l	0		; used by SAVE-INPUT and RESTORE-INPUT
input_size_a		dc.l	0		; used by SAVE-INPUT and RESTORE-INPUT
;
; ------------------------------------------------------------------------------------------------------------
; USER variables - this region will be copied to USER memory area on initiation
; ------------------------------------------------------------------------------------------------------------
input_buff		equ 	USERRAM 0 +	; address of the input buffer
USERRAM_MAP		dc.l	_input_buff		
input_size		equ	USERRAM 4 +	; size capacity of the input buffer
			dc.l	_input_size
>IN_			equ	USERRAM 8 +	; offset to current parse area
			dc.l	0
IN_LEN			equ	USERRAM 12 +	; number of characters in the input buffer
			dc.l	0
HLD_			equ 	USERRAM 16 +	; pointer for number output words (HOLD, etc.)
			dc.l	0
STRINGP		equ	USERRAM 20 +	; pointer within the string buffer
			dc.l	_PAD
STATE_			equ 	USERRAM 24 +	; compile=1 / interpret=0 flag 
			dc.l	0	
WID.COMPILE		equ	USERRAM 28 +	; current compilation wordlist (WID=0 is the FORTH wordlist)
			dc.b	0
WID.COUNT		equ	USERRAM 29 +	; number of WID's in the search list
			dc.b	1
WID.ORDER		equ	USERRAM 30 +	; current WID search order, first to last
			ds.b	14		; space for 14 wordlists
;			#.l	USERRAM 			; zero the user memory area
;			#.w	USERRAMSIZE			; 	excluded by default to increase performance
;			zero					; 	of new task launch
;			jsl	FILL.CF				
USERINIT.CF		#.l	USERRAM_MAP			; code to initialize the user memory area		
			#.l	USERRAM
			#.w	44					; number of USER variable bytes to initialize
			jsl	MOVE.CF				; copy the map to the user memory area
			rts
;
; ------------------------------------------------------------------------------------------------------------
; Exception stack variables - this region will be copied to exception stack on initiation
; ------------------------------------------------------------------------------------------------------------
BASE_			equ	ESTACK 0 +	; BASE is located on the exception stack
ESTACK_MAP		dc.l	10
TYPE_VECTOR		equ	ESTACK 4 +	; input and output vectors also located on the exception stack
			dc.l	VTYPE.CF
EMIT_VECTOR		equ	ESTACK 8 +
			dc.l	VEMIT.CF
KEY_VECTOR		equ	ESTACK 12 +
			dc.l	KKEY.CF
KEY?_VECTOR		equ	ESTACK 16 +
			dc.l	KKEY?.CF
			ds.l	8 5 -		; spare slots on the exception stack
;
ESTACKINIT.CF		#.l	ESTACK_MAP			; code to initialize the exception stack
			#.l	ESTACK
			#.b	8				; 8 exception stack variables
			zero	( s d 8 0)
			DO					; MOVE not used since this area is longword addressable only
;
				over		( s d s)
				fetch.l	( s d lw)
				over		( s d lw d)
				store.l	( s d)
				#.b 4		( s d 4)
				+		( s d+4)
				swap
				#.b 4		( d+4 s 4)
				+		( d+4 s+4)
				swap		( s d)		
			LOOP
			drop
			drop
			rts					
; marker for initializing HERE
END			dc.l	0
;