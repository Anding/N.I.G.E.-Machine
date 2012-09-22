====================
The N.I.G.E. Machine
====================

The N.I.G.E. Machine is a user-expandable micro-computer system that runs on an FPGA development board. It is designed specifically for the rapid prototyping of experimental scientific hardware or other devices. The key components of the system include a stack-based softcore CPU optimized for embedded control, a FORTH software environment, and a flexible digital logic layer that interfaces the micro-computer components with the external environment. The system is currently hosted on a Digilent Nexys 2 development board.


COPYRIGHT AND LICENSE
=====================

The N.I.G.E machine, its design and its source code are Copyright (C) 2012 by Andrew Richard Read and dual licensed.
    
(1) For commercial or proprietary use you must obtain a commercial license agreement with Andrew Richard Read (anding_eunding@yahoo.com)
    
(2) You can redistribute the N.I.G.E. Machine, its design and its source code and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

The N.I.G.E Machine is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this repository.  If not, see <http://www.gnu.org/licenses/>.


GETTING STARTED
===============

Prerequisites
=============

Suggested experience
--------------------

* Some familiarity with digital logic design using one or other FPGA development environments is advised in order to get started with this project.

* The intended application of the N.I.G.E. Machine is in controlling electronic hardware.  Prior experience with microcontrollers and digital electronics could be useful in that regard.

* The project is hosted on Github.  If you wish to contribute to the project you will need to use the GIT version control system. See the following resource:
<http://learn.github.com/p/intro.html>

* The system software is the FORTH computer language, however familiarity with FORTH is _not_ a prerequisite.  FORTH (and its advantages in this kind of application) are readily assimilated on the fly.  See the following resource:
<http://www.forth.com/starting-forth/>.


Hardware
--------

The system currently runs on a Digilent Nexys 2 development board, although it may be ported to other FPGA development boards.  Please note that there are two versions of the Nexys 2, 1200k gates and 500k gates.  The design has been developed and tested with the 1200k gate board.  A port to the 500k gate board would likely be quite feasible but has not been tested.  If acquiring a new board then the 1200k gate version is is recommended.
<http://www.digilentinc.com/Products/Detail.cfm?NavPath=2,400,789&Prod=NEXYS2>


Software
--------

The required software for using this project is available from the internet at no charge (under the respective license agreements of the providers):

* Xilinx ISE is required for synthesizing the FPGA configuration files.  ISE version 13.2, webpack edition, is currently used for the development of the project.  Later versions of ISE would likely be suitable but some update of project files and minor troubleshooting might be required.  If installing Xilinx ISE for the first time then version 13.2, webpack edition, is recommended.
<http://www.xilinx.com/support/download/index.htm>

* The latest version of Digilent Adept is required for downloading the design files to the Nexys 2 board.
<http://www.digilentinc.com/Products/Detail.cfm?NavPath=2%2C66%2C828&Prod=ADEPT2>

Other software packages are not required but may be helpful for development work:

* Programmers Notepad and FORTH files add-on.
<http://www.pnotepad.org/>
<http://www.pnotepad.org/add-ons/>

* VFX Forth from MicroProcessor Engineering
<http://www.mpeforth.com/>


Getting Started
===============

1. Install Xilix ISE and Digilent Adept software on your PC
2. Connect the Nexys2 board to your PC via USB and confirm the connection with Adept.  Also connect a VGA monitor and PS/2 keyboard to the board
3. Clone the Github hosted repository to your PC
4. Unzip the file Xilinx.zip
5. Confirm the folder structure as follows
\Resources
\System
\VHDL
\Xilinx
6. Run the file Xilinx\NIGE_Machine.xise to launch this project in Xilinx ISE
7. Within ISE, in the Design Hierachy window (top left) double click "inst_SYS_RAM - SYS_RAM".  A CORE generator wizard will launch
On page 4 of the wizard, browse to the init file \System\SRAM.coe and click "Generate"
8. Similarly, make sure the init file for "inst_Char_RAM - Char_RAM" points to \System\Character_ROM.coe and then generate
Steps 7 and 8 are required because ISE uses absolute file references that need to be set for your local hard drive configuration
9. Select "uut - Board - Behavioral" in the Design Hierachy window
10. Run "Generate Programming File" and confirm no errors are reported
11. In the Digilent Adept software, for PROM (second row), browse to Xilinx\board.bit and click "Program"
12. Turn to the Nexys 2 board.  Confirm the N.I.G.E. Machine startup message on the VGA monitor (it may be necessary to adjust the horizontal/vertical VGA positioning)
13. Run a short FORTH programme.  For example "1 1 + . <return>"
14. Congratulations!  The project is up and running.

Project structure
=================

\Resourses contains documentation and other reference materials

\VHDL contains all of the logic design (VHDL files) for the N.I.G.E. Machine

\System contains other design files that are not VHDL, such as the FORTH system software

\Xilinx is treated slightly differently from the other folders.  Xilinx ISE uses binary configuration files and also regenerates a significant number of intermediate files with each run.  For these reasons it has been considered best NOT to version control the Xilinx ISE project files.  They are placed in a separate folder (\Xilinx) that is referenced in .gitignore and taken outside the scope of version control.  The supplied file Xilinx.zip is a reasonably updated snapshot of the project for the purpose of providing users with a starting point for ISE (see Getting Started)

Possible next steps
===================

* The file "\Resources\NIGE Machine EuroFORTH 2012.pdf" is a copy of the academic paper presented at EuroFORTH 2012 at Exeter College, Oxford.  It contains a reasonably detailed explanation of the N.I.G.E. Machine with an example application, and is a good starting point for further review.

* It is very much hoped that the N.I.G.E. Machine will find uses in its intended application of supporting the prototyping and use of experimental scientific hardware.  If this applies to you, your initiative and contact will be warmly welcomed.  In fact it may be possible to work together to configure the N.I.G.E. Machine for your intended application.

* The project is under constant development.  You are invited and encouraged to contribute! The preferred collaboration model via Github is a small team using the Shared Repository model rather than ad-hoc Fork and Pull (<https://help.github.com/articles/using-pull-requests>).

* Please contact Andrew Read (anding_eunding@yahoo.com) if you need some help or would like to collaborate.

