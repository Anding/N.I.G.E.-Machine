N.I.G.E. Machine port to the Digilent cmod-A7
=============================================

1. Rebuild and test the latest version on the Nexys 4 board (i.e with SRAM not DDR external memory) 
2. Migrate to the latest version of Vivado 
	- Update IP
3. Create separate main branches for the Nexys4 and mod-A7 implementations.  Switch to the cmod-A7 branch
4. Redirect console I/O from VGA and keyboard to the serial port over USB
5. Obtain or develop a suitable serial port terminal on Windows
	- check Andre Lamothe's toolchain which has VT100 emulation
6. Reconfirm operation of the Forth bootloader functionalty built into the system software
7. Develop a VHDL module to report to a serial interface 7 segment display on a PMOD
	- serial communication should be implemented independent of CPU
	- report from the present 7-seg a hardware register in parallel with the Nexys4 built in 7-seg
	- FSM design with a modest refresh rate is likely
	- consider developing this module in a new repository as with a stand-alone board.vhd harness for reuse in other projects
	- test both within the Nexys4 system and on a standalone cmod-A7
7. Confirm that the existing Nexys4 Power-On-Self-Test reports over the PMOD display
8. Disconnect modules that will not be ported to the cmod-a7 and retest
	- VGA display and supporting modules
	- Nexys4 specific hardware (7-seg, switches, most LED's, PS-2 keyboard interface, etc.)
	- reduce the SRAM system memory capcity to the cmod-A7 limit
9. Migrate to the cmod-A7
	- pin definitions file (UCF)
	- Vivado FPGA settings
10. Work on syntheis and place and route on the cmod-A7
	- consider downgrading the clock frequency to 50MHz if needed to achieve place and route
11. Test and debug
12. Consider further development based on experience to date
	

	 
