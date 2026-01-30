The Open Distribution System Simulator, OpenDSS

Copyright (c) 2008-2026, Electric Power Research Institute, Inc.
All rights reserved.

Version 11.0.0.1 - Charlottesville

This version of the program is named after the birthplace of Tom McDermott, who, together with Roger Dugan, has been instrumental in developing the core architecture of OpenDSS and advancing the program since its inception.


Changes this version
====================

Protection Elements Enhancements:

- Enhanced Fuses, SwtControls, Reclosers and OC Relay Functions with new properties and improved functionality
- Added RatedCurrent property to SwtControls, Fuses, Reclosers and Relays (for Relays, these are data points of the associated protected switch or breaker)
- Added InterruptingRating property for Fuses, Reclosers and Relays (for Relays, these are data points of the associated protected switch or breaker)
- Added support for unganged operation, single-phase tripping and single-phase lockout for Reclosers and OC Relay Functions
- Standardized TCC curve behavior - no curves assigned by default ('none' is used as keyword to represent no curve specified. 'none' can also be used to specify non-existing curve setting)
- Enhanced Recloser and OC Relay Function properties: renamed properties for consistency and to reflect terms more commonly used in the industry, all while maintaining backwards compatibility
- Added separate pickup currents for fast and slow curves for both phase and ground elements on Reclosers
- Improved SwtControl to support ungagged operation with single switch object
- More informative event logging and debug tracing for Reclosers and OC Relay Functions

RegControl Improvements:

- Added fwdThreshold property to RegControl for uneven no-load zone bands
- Updated revThreshold property description for better clarity on no-load band configuration

BatchEdit Enhancements:

- Added conditionals to BatchEdit command allowing users to focus edits on object subsets within affected classes

Control Element Fixes:

- Fixed StorageController synchronization issue between %kWBandLow and kWBandLow properties
- Updates to prevent convergence issues on StorageController under very specific edge cases
- Fixed floating-point precision issue in InvControl volt-var function affecting 64-bit version

CIM Export Updates:

- Fixed CIM export for PV systems and batteries to follow negative sign convention
- Addressed missing ACLineSegmentPhase instances in low voltage circuits
- Fixed CIM export issues for LineCode-based lines

COM and DDLL Improvements:

- Added missing properties and methods to DirectDLL to match COM interface. Enhanced Direct DLL DSS Properties interface to better mimic COM interface behavior.
- Added CktElement.AllLosses property to retrieve complex losses array (total, load, and no-load losses)
- Updated COM and DDLL interfaces for all protection element changes

Bug Fixes and Stability:

- Fixed bug in parser when Solve command includes option settings with equals signs and spaces
- Fixed memory mapping bug when working with files of different sizes for pmult/qmult
- Fixed issue in Overload reports not considering seasonal ratings when enabled
- Fixed transformer seasonal ratings factor and updated property descriptions
- Fixed issue rejecting negative values for PCE elements
- Resolved progress bar synchronization issues and added guardrails for abrupt program termination

Platform and Development:

- Enhanced support for Linux environments including pyControl debugging with xterm
- Added OpenDSS version checking enhancement for better error handling with internet connection issues

Documentation and Examples:

- Added examples for current and power calculations in DSS
- Added example for projecting polar data into time-domain for dynamics simulation waveforms
- Updated documentation to reflect all changes
- Enhanced property descriptions for clarity and consistency

C++:

- All Delphi changes have been ported to the C++ version including protection elements, RegControl, and BatchEdit enhancements
- Fixed C++ version issue where program would hang applying long line correction


Changes in previous releases
============================

v 10.2.0.1
- Fixes sync issues with load allocation routines due to lack of sync with solution actor.
- Restores A-Diakoptics functionality that was broken in v10.1.0.1.
- Adds RegControl idle flags (idle, idleReverse, idleForward) for controlling tap position under no-load, forward, and reverse flow conditions.
- Introduces pyControl object allowing users to model elements in native Python with script integration into simulation.
- Fixes DLL/COM issues including Y value retrieval, reactor pointer verification, AllowForms assignment, XYCurves, and allocation factors.

v 10.1.0.1
- Provides better sync between actors and main thread through queues preventing polling.
- Includes the property SemiConLayer the declare the existence of a semicon layer for CNData/LineGeometry objects.
- Improves the sync with the progress bar app.
- Removes over estimation (Size) for IBR working in GFM mode for better exposing the short circuit features of the model.

v 10.0.0.2
- Solves a bug found when reporting powers for generators while using NCIM solution mode.
- Adds interfaces in COM / DLL for directly handling Storage and WindGen objects.


The Version 8 was the first to be delivered with parallel processing capabilities in both 32-bit (X86) and 
64-bit (X64) versions. Version 9 is the latest update. The files are still listed under the 'Version8' folder on Sourceforge.net.
The OpenDSSInstaller download includes both X86 and X64 versions of Version 9, along 
with optional documentation and examples.  

If you have 64-bit Windows, you may install both the 64-bit and 32-bit 
versions.  The 32-bit version is required if you plan to automate OpenDSS 
from Excel or any other 32-bit program.  The 64-bit version is required to 
automate OpenDSS from 64-bit MatLab or other 64-bit apps on a 64-bit system.  

Installation
============

The installer will give you a choice to install the executables and 
optional files under a target directory of your choice, such as 
c:\opendss.  Files that are specific to the 32-bit version will be written 
to an x86 subdirectory, such as c:\opendss\x86.  Files that are specific 
to the 64-bit version will be written to an x64 subdirectory, such as 
c:\opendss\x64.  The EXE and DLL files should not be moved after 
installation, but may be updated in place with newer versions.
  
On a 64-bit system, you may install and use both the 32-bit and 64-bit 
versions with no conflict between them.  

Short-cuts to the program and manual are created under Start Menu/OpenDSS.
Please see the manual, OpenDSSManual.PDF, for an overview of the program. 
The most up-to-date reference information will always be found through the 
software's "Help / DSS Help" menu command.

If you have an earlier version of OpenDSS installed and registered, such as 7.4.3,
remove it completely. Otherwise, Windows may retain a registry entry to the
old 32-bit COM server when you start it up from a 32-bit program.

COM Automation
==============

The COM Server in OpenDSSEngine.DLL may be automated.  The installer will 
register either or both versions, depending on your selection.  Even 
though the file names and registration commands match, they are in 
separate locations and Windows will activate the correct version required 
by the calling program.  For example, 64-bit MatLab will call the 64-bit 
OpenDSSEngine.DLL and 32-bit Microsoft Excel will call the 32-bit version.  
(Note: The 64-bit version of Excel is rarely installed.) 

Background
==========

The OpenDSS is a simulator specifically designed to represent electric 
power distribution circuits.  OpenDSS is designed to support most types of 
power distribution planning analysis associated with the interconnection 
of distributed generation (DG) to utility systems.  It also supports many 
other types of frequency-domain circuit simulations commonly performed on 
utility electric power distribution systems.  It represents unbalanced 
conditions, stochastic processes, and other aspects of electrical power 
distribution systems and equipment in far greater detail than many other 
tools, including commercial products.  Through COM and scripting 
interfaces, other programs can drive OpenDSS in highly customized 
simulations, Monte Carlo analysis, etc.  Users can define their own models 
through dynamic linking, scripting, or automation.  

Electric Power Research Institute, Inc.  (http://www.epri.com) uses 
OpenDSS in its research and services work, and continues to enhance the 
software.  Earlier proprietary versions were used in dozens of studies for 
electric utility clients, and in a Web-based wind power simulator.  
There were several goals in making OpenDSS an open-source project in 2008: 

1 - Enhance the modeling capabilities available to government 
laboratories, universities, and other researchers engaged in grid 
modernization work.  

2 - Encourage interfaces between OpenDSS and complementary tools, such as 
communication system simulators or model compilers.  

3 - Encourage the adoption of items 1 and 2 into commercial products used 
by electric utilities.  

4 - Encourage collaborative efforts between industry, government, and 
university researchers in power distribution system analysis for grid 
modernization efforts.  

5 - Provide a capable testing platform for data and object modeling 
efforts currently underway in the electric utility industry.

Source Code
===========

The programming language for OpenDSS is Delphi 
(http://www.embarcadero.com), currently version Delphi 10.4 Sydney.  A free community version is available
to certain non-commercial users (see license requirements on Embarcadero site).
There is also a Free Pascal (Lazarus) version of the program.  Some of the supporting modules may 
require a C++ compiler to build from source.  OpenDSS source code is 
available from the following SVN repository: 

https://sourceforge.net/p/electricdss/code/HEAD/tree/trunk/


Third-party Components
======================

KLUSolve.DLL is open source software, available from
www.sourceforge.net/projects/klusolve

Other convenient Sourceforge.net Links
======================================

OpenDSS Download Files:

http://sourceforge.net/projects/electricdss/files/

Getting Started

http://sourceforge.net/apps/mediawiki/electricdss/index.php?title=Getting_Started

Latest Tech Notes in Wiki

Selected Tech Notes are available in the Doc Folder

Questions and Answers

Selected Q&A files are available in the Doc Folder

OpenDSS Forum

http://sourceforge.net/p/electricdss/discussion/861976/

What is Unique About OpenDSS?

OpenDSS was derived from a family of power system harmonics solvers designed for analysis of distributions systems. This analysis requires very detailed models of the circuit topology. This gives the program the capability to represent nearly any circuit topology that might be encountered on a power distribution system. While the power flow solution is the most common application, the program is technically not a power flow program. The solution method and circuit modeling has more in common with harmonics and dynamics solvers.

The program was developed because users realized in 1996 that they were not getting the correct answer for distributed generation problems when using only the typical static power flow analysis used for distribution planning. OpenDSS was one of the first programs to implement an efficient quasi-static time series (QSTS) simulation for DER analysis. The capability was built into the program from the start.

It was recognized that it is not possible to anticipate everything that users will want to do in DER analysis and build a single user interface for this. This issue was addressed by building the program around a scripting interface that is user defined. Also, the COM interface was added so that users could drive the program from other software such as MATLAB and Python. This feature is popular among researchers and graduate students who want to do things not already supported in the program. Of course, EPRI uses this feature extensively in its research. A direct function call DLL interface (OpenDSSDirect.DLL) was also developed to allow this feature to be used on platforms and computer languages that do not support COM.



IEEE Test Cases

https://sourceforge.net/p/electricdss/code/HEAD/tree/trunk/Version8/Distrib/IEEETestCases/

EPRI Test Circuits
https://sourceforge.net/p/electricdss/code/HEAD/tree/trunk/Version8/Distrib/EPRITestCircuits/

Source Code

https://sourceforge.net/p/electricdss/code/HEAD/tree/trunk/Version8/Source/

Top level of Distribution area (Releases)

https://sourceforge.net/p/electricdss/code/HEAD/tree/trunk/Version8/Distrib/

Examples

https://sourceforge.net/p/electricdss/code/HEAD/tree/trunk/Version8/Distrib/Examples/

License
=======

Use of this software is subject to a license. The terms are in:

 1 - A file called "license.txt" distributed with the software,
 2 - The user manual, and
 3 - The executable program's Help/About dialog box
