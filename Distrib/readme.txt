The Open Distribution System Simulator, OpenDSS

Copyright (c) 2008-2009, Electric Power Research Institute, Inc.
All rights reserved.

Version 6.3.1

Third-party Components
======================

DSSGraph.DLL uses a commercial product "SDL Delphi Component Suite", 
available from www.sdlsuite.com

KLUSolve.DLL is open source software, available from
www.sourceforge.net/projects/klusolve

Introduction
============

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
electric utility clients, and in a Web-based wind power simulator at 
http://www.uwig.org/distwind.  There are several goals in making OpenDSS 
an open-source project at this time: 

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
efforts currently underway in the electric utility industry, at 
http://cimug.ucaiug.org and http://www.multispeak.org.  

OpenDSS runs on 32-bit Windows, and the programming language is Delphi 
(http://www.codegear.com).  Some of the supporting modules may require a 
C++ compiler to build from source.  

Installation
============

Unzip the contents of this file into a directory of your choice, such as 
c:\opendss. The standalone executable may be started in two ways:

1 - invoke "OpenDSS" from a command prompt, or
2 - double-click on "OpenDSS.exe" from Windows Explorer

See the manual, OpenDSSManual.PDF, for an overview of the program. 
The most up-to-date reference information will always be found through the 
software's "Help / DSS Help" menu command.

The DLL version in OpenDSSEngine.DLL may be automated. For the time being, 
you would need a type library tool and some experience with COM scripting 
to make any headway. In the future, we'll provide examples and 
documentation.

Source Code
===========

OpenDSS source code is available from the following SVN repository:

https://electricdss.svn.sourceforge.net/svnroot/electricdss


License
=======

Use of this software is subject to a license. The terms are in:

 1 - A file called "license.txt" distributed with the software,
 2 - The user manual, and
 3 - The executable program's Help/About dialog box
