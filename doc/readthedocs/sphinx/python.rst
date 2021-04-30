EnergyPlus Python Overview
--------------------------

Although the core EnergyPlus simulation engine is written in C++, there are actually several languages and tools that go into building, packaging and running EnergyPlus.
There are FORTRAN tools that are built around the edges of EnergyPlus and have been developed to preprocess inputs prior to running EnergyPlus, or postprocess results.
There are GUI tools packaged with EnergyPlus that are built with Visual Basic and RealBasic.
In recent years, Python has been employed to handle several tasks around EnergyPlus, although until recently they have been mostly on the developer side.
With the most recent versions, Python is starting to impact packages and user experiences, so this page will be a sort of one place to get all the information.

There are several aspects where Python comes into play with EnergyPlus, each with their own section here.

Requirements
============

As of the time of this writing, EnergyPlus is dependent on Python 3.5+.
This dependency is primarily because most of the code that we use in EnergyPlus is written with type hints that were added in Python 3.5.
For packaging, we currently link EnergyPlus with 3.7.

Building EnergyPlus
===================

Building EnergyPlus requires Python.  This is because we have a number of scripts that are used in the build process, including processing the IDD file into a JSON schema.
During packaging, Python is also used to generate supporting files and documentation.

Linking EnergyPlus with Python (Plugins)
========================================

By default EnergyPlus is not linked to Python during a build, which is suitable for many developer environments.
However, for running Python plugins, EnergyPlus needs to be linked to the Python dynamic library and the Python standard library.
On Windows and Mac, simply installing Python 3.5+ through the normal means will suffice.
On Linux, the Python headers are also needed and they are included separately if you are linking with the system Python.
On Debian, the headers can be downloaded with `apt install python3-dev`, and then EnergyPlus build system will find them during compilation.

EnergyPlus API
==============

More.

Example Usages
==============

Coming.
