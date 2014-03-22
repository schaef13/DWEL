DWEL
====

Data processing tools for Dual-Wavelength Echidna Lidar.

1. Prerequisite
===============

Most DWEL programs are written in Interactive Data Language (IDL) code. Also they use ENVI functions. You need install both ENVI and IDL to run these DWEL processing programs. 

A few programs for image jitter fix for the data collected before we fixed the instrument's encoder inaccuracy problem are written in Matlab. If you need to correct the encoder values of the scans and fix the jitter, you also need to install Matlab to run these programs. If you can't install or find a Matlab to use, contact Zhan Li (zhanli1986 at gmail dot com) to help you run the programs and fix the data. 

2. Installation
===============

No particular installation action is required to use the DWEL programs. Just open IDL, compile the program you want to use and run it. Detailed steps as following.

2.1 Start IDL
------------
On linux, in terminal, 
$ idl (start IDL in command-line mode)
or
$ idlde (start IDL in GUI mode)


2.2 Compile the program you want to use
---------------------------------------
In IDL command prompt, 
IDL>> .compile the/full/name/of/the/.pro/file/you/want/to/use

2.3 Run the program
-------------------
IDL>> name_of_the_program, input_arguments
All the DWEL programs now are running in IDL command-line mode to enable batch processing and easy running on servers.


