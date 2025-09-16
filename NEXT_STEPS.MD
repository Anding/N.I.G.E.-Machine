# Next steps

### Verify last known good operation
1. Program the Nexys4 (SDRAM version) with Vivado 2024.2
2. Use VGA monitor and attached keyboard to confirm operation in microcontroller mode
3. Confirm inclusion and operation of Forth system software from an SD-card
4. Confirm operation of the secondary terminal via UART over USB
   - If not operational, swap the primary and secondary terminal in the code

### Build a testbench file on SD-card to provide simple functionality testing 
1. Base on simple-tester - test results output to the terminal
2. CPU arithmetic and logic
3. Memory
4. Multi-tasking

### Switch over sytem software to the terminal
1. Change Forth.asm to run through the terminal as the main task
2. Verify in hardware with the testbench

### Remove VGA output and keyboard input
1. Comment out in the constraints file and the board entity definition
2. Comment out associated driver modules
3. Simplify the hardware_register module by "tieing off" the lines to these components
3. Verify in hardware with the testbench

### Remove other components not included on the CMOD-A&
1. Sevenseg
2. PHY ethernet controller
3. Switches, buttons and LED's
4. Approach as for VGA

### Code improvements
1. Review simplify branch and consider the ideas for code improvements
2. Verify in hardware with the testbench

### Shrink BLOCK RAM usage below 50 units
1. Reduce task space from 32 to 16 or 8
2. If necessary reduce stack depths

### Temporarily disable SDRAM
1. Retain the SDRAM instructions at processor level
2. "Tie-off" the SDRAM databus to zero and comment out the SDRAM controller







