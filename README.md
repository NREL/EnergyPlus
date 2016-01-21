EnergyPlus
===================

This is the EnergyPlus Development Repository.  

# Contact/Support

 - The Department of Energy maintains a [public website for EnergyPlus](http://www.energyplus.gov) where you can find much more information about the program.  
 - For detailed developer information, consult the [wiki](https://github.com/nrel/EnergyPlusTeam/wiki).  
 - For support questions, please utilize the [EnergyPlus Helpdesk](http://energyplus.helpserve.com/).

# Releases
Although iteration **(pre-)releases** will be posted to this repository during a development cycle, users should generally avoid these, as input syntax may change which won't be supported by the major release version transition tools, and could require manual intervention to remedy.  If a release is intended for active use by users, such as a bug-fix-only or performance-only re-release, it will be clearly specified on the release notes and a public announcement will accompany this type of release.

# Contributing Development
EnergyPlus is an open-source project and the EnergyPlus team accepts contributions to EnergyPlus source, utilities, test files, documentation, and other materials distributed with the program.  If you actively do development that you'd like to contribute back to the project, you might start with a ticket on the Helpdesk to engage a developer to learn more.

# Building EnergyPlus

Now the fun part, building EnergyPlus.  The build process is setup by Cmake.  On all platforms, you will want to clone the repository to your machine, then create a folder for building.  This build folder is commonly placed right next to the source folder, but doesn't have to be.   The repository folder will be called the "source tree", and the build folder is called the "build tree."  The steps for each platform are described in the following sections.

## Windows

The C++ code in EnergyPlus utilizes many C++11 features, and so a modern compiler is required.  On Windows, the latest version of Visual Studio 2013 suffices well.  Using GCC on Windows is fine, but instructions aren't provided here.  These instructions are for building the base EnergyPlus executable.

1. Install Visual Studio 2013 Express **for WIndows Desktop**, at least update 3.  This is available [here](http://www.visualstudio.com/downloads/download-visual-studio-vs#d-express-windows-desktop)
2. Install cmake, from [http://www.cmake.org/cmake/resources/software.html].  This includes a GUI.  From the start menu, you should be able to launch the gui (it is called cmake-gui).  
3. Point cmake source code to the root of the repository, the root of the source tree. e.g. C:\GitHubClones\EnergyPlus
4. Point cmake build to the folder you created, the root of the build tree.  CMake will offer to create this folder if it doesn't exist. e.g. C:\GitHubClones\EnergyPlus\Build (This may be anywhere on your system, but this arrangement is convenient, because the EnergyPlus repo ignores folders named Build or build.)
5. Click configure.  This may bring up a dialog box where you choose the build environment to use. Choose Visual Studio 12 or Visual Studio 12 Win64 (for 64-bit builds), which is the 2013 version of the toolchain.
6. Check the configuration settings, keeping them default should suffice, and click Generate.
7. Browse to the build folder and there will be a Visual Studio solution file you can click on, EnergyPlus.sln. It will include multiple projects for each build target.  Choose the type of build (Debug or Release), then right click on EnergyPlus in the solution explorer and "Build".  This will build EnergyPlus and all of its dependencies.  The target executables will be found in the Products subfolder.
8. The solution will include the ZERO_CHECK project as the default target to run when you execute, so when you are ready to debug, manually set the default target to EnergyPlus.
9. In the EnergyPlus and EnergyPlusLib projects: right click on each project and select Properties. Under Configuration Properties select Debugging. In the Environment field, enter ```_NO_DEBUG_HEAP=1```.  The debug heap in Visual Studio doesn't play nicely with the input processor in EnergyPlus, and for debug builds, skipping this will result in a _very_ long IDD reading time. If unit tests are active (BUILD_TESTING is checked in CMake) then also set this for energyplus_tests.
10. For the EnergyPlus project, right click on the project and select Properties. Under Configuration Properties select Debugging. In the Working Directory field, enter $(SolutionDir)\Products. This is the target path for the Energy+.idd file and will be the working directory for interactive debugging.  Put in.idf, in.epw and the like here.  Or enter command line arguments in the Command Arguments field to specify the locations of files.

## Linux

These instructions were written around Ubuntu, but should be valid for other Debian/*buntu and Linux-in-general distributions.  The differences will be in the package-management system commands, and possibly in the shell being used (sh, bash, zsh, etc.).

1. Make sure g++ is sufficient for compiling the code.  It is recommended to have g++ 4.8.1+.  This version is not currently available in the default Ubuntu 12.04 repositories, but can be installed alongside 4.6.  Ubuntu 14.04 includes a modern enough version in the main repositories.  
2. Install cmake tools and the cmake "GUI", for Debian/Ubuntu it should be as simple as apt-get cmake cmake-curses-gui.
3. Navigate in the terminal to the root of the build tree.  You will launch the cmake GUI from this location.  The program must be passed an argument that specifies the location of the root of the source tree.  For example, if the build directory is directly next to the source directory, the command could be called for the default build environment by calling the GUI as such: ```ccmake ../EnergyPlusTeam```

4. Configure the build in ccmake, then generate the make files. By default, the build type is set to "Release", but this can be changed to "Debug" (or other build types) from within ccmake.
5. Since you are already in the build folder, just run make -j N, where N is the number of process you'd like to employ to build the project.

## Mac

These instructions were written for Mac OS X Version 10.9.  Newer versions of OS X are expected to work, but not yet tested.  Older versions of Mac OS X may work with some tweaking of the project configuration, but are not supported at this time.

1. Install Xcode which is available on the App Store.
2. Install CMake from http://www.cmake.org/download/.
3. Launch CMake.app and click the "Browse Source" button, then select the directory where the source is located. (Alternatively, you may use ccmake from the command line similar to the Linux instructions.)
4. Make a new directory at the root of the source tree called build.
5. In CMake.app "Browse Build" and select the new build directory.
5. Press Configure. By default, the build type is set to "Release", but this can be changed to "Debug" (or other build types) from within CMake.app.
6. When prompted to specify the generator choose "Unix Makefiles" and default native compilers.
6. Press Generate.
7. Using the command line interface of Terminal.app navigate to the build directory.
8. Type make -j N, where N is the number of precess you'd like to employ to build the project.

# Building Installer Packages

Building an installer package is simply a matter of compiling the "Package" target.  On Windows you must have the NSIS tool installed first.  On Mac you must install PackageMaker.  Once the appropriate packaging tool is installed, use the CMake interface to turn on the "BUILD\_PACKAGE" option and regenerate the project.  A separate repository contains many binary pieces that are downloaded during the process of compiling the installer.  These are being transitioned to build from source where possible, in which case they will end up in this repository along with the other source.  Because this content is automatically downloaded from a separate GitHub repository during the packaging process, you will need to have an internet connection while generating an installer.  The completed installer package will be copied into the root of the build directory.
