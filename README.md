EnergyPlus [![](https://img.shields.io/github/downloads/nrel/energyplus/total.svg?colorB=FF7300)]()
===================================================================================================



This is the EnergyPlus Development Repository.  EnergyPlus™ is a whole building energy simulation program that engineers, architects, and researchers use to model both energy consumption and water use in buildings.

## Contact/Support

 - The Department of Energy maintains a [public website for EnergyPlus](https://energyplus.net) where you can find much more information about the program.
 - For detailed developer information, consult the [wiki](https://github.com/nrel/EnergyPlusTeam/wiki).
 - Many users (and developers) of EnergyPlus are active on [Unmet Hours](https://unmethours.com/), so that's a great place to start if you have a question about EnergyPlus or building simulation.
 - For more in-depth, developer-driven support, please utilize the [EnergyPlus Helpdesk](http://energyplus.helpserve.com/).

## Testing

![](https://github.com/NREL/EnergyPlus/workflows/Custom%20Check/badge.svg) ![](https://github.com/NREL/EnergyPlus/workflows/Documentation/badge.svg) ![](https://github.com/NREL/EnergyPlus/workflows/CppCheck/badge.svg)

Every commit and every release of EnergyPlus undergoes rigorous testing.
The testing consists of building EnergyPlus, of course, then there are unit tests, integration tests, API tests, and regression tests.
Since 2014, most of the testing has been performed by our bots ([Tik-Tok](https://github.com/nrel-bot), [Gort](https://github.com/nrel-bot-2), and [Marvin](https://github.com/nrel-bot-3)), using a fork of the [Decent CI](https://github.com/lefticus/decent_ci) continuous integration system.
We are now adapting our efforts to use the Github Actions system to handle more of our testing processes.
In the meantime, while Decent CI is still handling the regression and bulkier testing, results from Decent CI are still available on the testing [dashboard](http://nrel.github.io/EnergyPlusBuildResults/).

## Releases [![](https://img.shields.io/github/release/NREL/energyplus.svg?colorB=FF7300)](https://github.com/NREL/EnergyPlus/releases/latest)

![](https://github.com/NREL/EnergyPlus/workflows/Windows%20Releases/badge.svg) ![](https://github.com/NREL/EnergyPlus/workflows/Mac%20Releases/badge.svg) ![](https://github.com/NREL/EnergyPlus/workflows/Linux%20Releases/badge.svg)

EnergyPlus is released twice annually, usually in March and September.
It is recommended all use of EnergyPlus is production workflows use these formal, public releases.
Iteration **(pre-)releases** may be created during a development cycle, however users should generally avoid these, as input syntax may change which won't be supported by the major release version transition tools, and could require manual intervention to remedy.
If an interim release is intended for active use by users, such as a bug-fix-only or performance-only re-release, it will be clearly specified on the release notes and a public announcement will accompany this type of release.
Our releases are now built by Github Actions.

## Core Documentation

Program documentation is installed alongside the program, with the pdfs also available [online](https://energyplus.net/documentation).
Big Ladder also produces html based documentation [online](http://bigladdersoftware.com/epx/docs/).

## API Documentation

[![Read the Docs](https://img.shields.io/readthedocs/eplus?label=Python%20API%20Latest%20%28beta%29)](https://eplus.readthedocs.io/en/stable/)

An API is being developed to allow access to internal EnergyPlus functionality and open up the possibility for many new workflow opportunities around EnergyPlus.
A C API is developed to expose the C++ functions, then Python bindings are built on top of that to maximize the accessibility.
Temporary API documentation is available [here](https://nrel.github.io/EnergyPlus/api/index.html), but we are working on moving the API documentation to ReadTheDocs.
Currently the Python documentation is being built there, for the latest develop snapshot.
Many improvements to come in this area. 

## License & Contributing Development [![](https://img.shields.io/badge/license-BSD--3--like-FF7300.svg)](https://github.com/NREL/EnergyPlus/blob/develop/LICENSE.txt)

EnergyPlus is available under a BSD-3-like license.
For more information, check out the [license file](https://github.com/NREL/EnergyPlus/blob/develop/LICENSE.txt).
The EnergyPlus team accepts contributions to EnergyPlus source, utilities, test files, documentation, and other materials distributed with the program.
The current EnergyPlus contribution policy is now available on the EnergyPlus [contribution policy page](https://www.energyplus.net/contributing).
If you are interested in contributing, please start there, but feel free to reach out to the team.

## Building EnergyPlus

A detailed description of compiling EnergyPlus on multiple platforms is available on the [wiki](https://github.com/NREL/EnergyPlus/wiki/BuildingEnergyPlus).
Also, as we are adapting to using Github Actions, the recipes for building EnergyPlus can be found in our [workflow files](https://github.com/NREL/EnergyPlus/tree/develop/.github/workflows).
