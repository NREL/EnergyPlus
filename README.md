EnergyPlusTeam
==================

This is the root of the EnergyPlus Development Repository.  

# Build Process

The build process is setup by Cmake.  On all platforms, you will want to clone the repository to your machine, then create a folder for building.  This build folder is commonly placed right next to the source folder, but doesn't have to be.   The repository folder will be called the "source tree", and the build folder is called the "build tree."  The steps for each platform are described in the following sections.

## Windows

1. Install cmake, from [http://www.cmake.org/cmake/resources/software.html].  This includes a GUI.  From the start menu, you should be able to launch the gui (it is called cmake-gui).  
2. Point cmake source code to the root of the repository, the root of the source tree.
3. Point cmake build to the folder you created, the root of the build tree.
4. Click configure.  This may bring up a dialog box where you choose the build environment to use.  Step 6 will vary based on what you choose.
5. Check the configuration settings, and click Generate.
6. Close cmake and build the target:
  - If you chose a GCC Makefile based build, open up a command prompt and browse to the root of the build tree.  Run the command "make" (or make -j N where N is the number of threads to use in compiling).  Make will first build any dependencies on E+ and then compile and link E+ itself.
  - If you chose a Visual Studio based build, browse to the build folder in Windows Explorer (TM), and there will be a Visual Studio (TM) solution file you can click on, and it will include multiple projects for each build target.  You should be able to build the E+ target directly.
  - If you chose an Eclipse based build, open Eclipse and use File -> Import, General -> Exising Projects into Workspace.  Point the root directory to the root of your build tree, and click Finish.  The project should appear in the project explorer.  Right click on the project, choose Make Targets, Build, and select the EnergyPlus/Build target.  Eclipse should then start building the dependencies and EnergyPlus itself.


## Linux

These instructions were written around Ubuntu 12.04, but should be valid for other Debian/*buntu and Linux-in-general distributions.  The differences will be in the package-management system commands, and possibly in the shell being used (sh, bash, zsh, etc.).

0. Make sure g++ is sufficient for compiling the code.  It is recommended to have g++ 4.8.1+.  This version is not currently available in the default Ubuntu 12.04 repositories, but can be installed alongside 4.6.  Newer distributions should have a more up to date GCC distribution included.
1. Install cmake, for Debian/Ubuntu it should be as simple as apt-get cmake.  The terminal (curses) GUI can also be gotten with apt-get install cmake-curses-gui.
2. Navigate in the terminal to the root of the build tree.  You will launch the cmake GUI from this location.  The program must be passed an argument that specifies the location of the root of the source tree.  For example, if the build directory is directly next to the source directory, the command could be called for the default build environment by calling the GUI as such: 
```
ccmake "../EnergyPlusTeam"
```
Optionally, a build environment can be specified.  For example, if cmake is being used to set up an Eclipse build environment, the following calling signature should be used:
```
ccmake -G "Eclipse CDT4 - Unix Makefiles" "../EnergyPlusTeam"
```
3. 


## Mac

# Platform Status

This has been tested on Ubuntu 12.04 (with g++ 4.8.1) and Windows 7 (with MinGW 4.8.2) and builds E+ successfully.

# Build Configurations

Currently the cmake project is set up to build the Objexx library and EnergyPlus itself.  The external libraries (DeLight, SQLite, External Interface) do build, but they are not set up to link to E+ yet.  The E+ source that communicates with these files need to be updated to get this to work.  These external dependencies will become a core part of E+, not optional dependencies.


# To-Do

Things to work on:
 - Get E+ building against all the libraries
 - Need to add the testing tools into this repo and customize it for the executable structure
 - Evaluate how to interact with the FORTRAN pieces to the E+ experience (EPMacro, ExpandObjects, ReadVars, Transition)
