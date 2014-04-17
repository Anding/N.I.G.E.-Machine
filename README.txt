====================
The N.I.G.E. Machine
====================

The N.I.G.E. Machine is a user-expandable micro-computer system that runs on an FPGA development board. It is designed specifically for the rapid prototyping of experimental scientific hardware or other devices. The key components of the system include a stack-based softcore CPU optimized for embedded control, a FORTH software environment, and a flexible digital logic layer that interfaces the micro-computer components with the external environment. The system is currently hosted on a Digilent Nexys 2 or Nexys 4 development boards.

Short video introduction to the system: 
<http://www.youtube.com/watch?v=0v-HuVLRoUc>
Demonstration of an example application with a light sensor:
<http://www.youtube.com/watch?v=0Kj5EMdnkMk>

Refereed papers about the system as presented at EuroFORTH 2012 and 2013: 
http://www.complang.tuwien.ac.at/anton/euroforth/ef12/papers/
http://www.complang.tuwien.ac.at/anton/euroforth/ef13/papers/



Copyright and license
=====================

The N.I.G.E machine, its design and its source code are Copyright (C) 2012-2014 by Andrew Richard Read and dual licensed.
    
(1) For commercial or proprietary use you must obtain a commercial license agreement with Andrew Read (andrew81244@outlook.com)
    
(2) You can redistribute the N.I.G.E. Machine, its design and its source code and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.  

The N.I.G.E Machine is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this repository.  If not, see <http://www.gnu.org/licenses/>.


Reference manual
================

A 50 page reference manual is available that covers getting started through to system customization, with detailed appendices.  Please see the file \Resources\Nige Machine Manual.pdf (in branch v3.0)


Respoistory structure
=====================

\Resources contains documentation and other reference materials (please go to branch v3.0 to find the reference manual)

\VHDL contains all of the logic design (VHDL files) for the N.I.G.E. Machine

\System contains other design files that are not VHDL, such as the FORTH system software and the character ROM

\Software contains FORTH language application software that may be run on the N.I.G.E. Machine

\Xilinx is treated slightly differently from the other folders.  Xilinx ISE uses binary configuration files and also regenerates a significant number of intermediate files with each run.  For these reasons it has been considered best NOT to version control the Xilinx ISE project files.  They are placed in a separate folder (\Xilinx) that is referenced in .gitignore and taken outside the scope of version control.  The supplied file Xilinx.zip is a reasonably updated snapshot of the project for the purpose of providing users with a starting point for ISE (see "Getting started")


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

The system currently runs on a Digilent Nexys 2 or Nexys 4 development board.  Please note that there are two versions of the Nexys 2, 1200k gates and 500k gates.  The design has been developed and tested with the 1200k gate board.  A port to the 500k gate board would likely be quite feasible but has not been tested.  If acquiring a new board then the Nexys 4 is recommended.
http://www.digilentinc.com/Products/Detail.cfm?NavPath=2,400,789&Prod=NEXYS2
https://digilentinc.com/Products/Detail.cfm?NavPath=2,400,1184&Prod=NEXYS4


Software
--------

The required software for using this project is available from the internet at no charge (under the respective license agreements of the providers):

* Xilinx ISE is required for synthesizing the FPGA configuration files.  ISE version 14.6, webpack edition, is currently used for the development of the project and should be used for maximum compatibilty.
<http://www.xilinx.com/support/download/index.htm>

* The latest version of Digilent Adept is required for downloading the design files to the Nexys 2 board.  It is not necessary for the Nexys 4 board.
<http://www.digilentinc.com/Products/Detail.cfm?NavPath=2%2C66%2C828&Prod=ADEPT2>

Other software packages are not required but may be helpful for development work:

* Programmers Notepad and FORTH files add-on. 
<http://www.pnotepad.org/>
<http://www.pnotepad.org/add-ons/>
Set Tools/Options/General/Defaults "Tab Width" to 7

* VFX Forth from MicroProcessor Engineering
<http://www.mpeforth.com/>


Versions
========
There are several versions in this repository 
v3.0 is applicable to the Nexys4 board.  It is being actively developed with bug fixes and new features.
v2.0 is applicable to the Nexys2 board (1200K gate).  It has been thoroughly tested but some small bugs likely remain.  It is not currently being further developed
(There is also an experimental branch of v2.0 applicable to the v2.0 Nexys2 board (500K gate))
v1.0 is the original version that was presented at EuroFORTH 2012


Possible next steps
===================

* It is very much hoped that the N.I.G.E. Machine will find uses in its intended application of supporting the prototyping and use of experimental scientific hardware.  If this applies to you, your initiative and contact would be warmly welcomed.  In fact it may be possible to work together to configure the N.I.G.E. Machine for your intended application.

* The project is under constant development.  You are invited and encouraged to contribute! The preferred collaboration model via Github is a small team using the Shared Repository model rather than ad-hoc Fork and Pull (<https://help.github.com/articles/using-pull-requests>).

* Please contact Andrew Read (andrew81244@outlook.com) if you need some help or would like to collaborate.

