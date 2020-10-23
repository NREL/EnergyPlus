# SSC (SAM Simulation Core)
[![Build Status](https://travis-ci.org/NREL/ssc.svg?branch=develop)](https://travis-ci.org/NREL/ssc)
[![FOSSA Status](https://app.fossa.io/api/projects/git%2Bgithub.com%2FNREL%2Fssc.svg?type=shield)](https://app.fossa.io/projects/git%2Bgithub.com%2FNREL%2Fssc?ref=badge_shield)

The SSC Open Source Project repository contains the source code for the technology and financial models contained within the National Renewable Energy Laboratory's System Advisor Model (SAM). For more details about SAM's capabilities, see the SAM website at [https://sam.nrel.gov/](https://sam.nrel.gov).

You could think of SSC as the home for the algorithms behind the SAM desktop program. Most people run the code through the desktop user interface, but SSC can also be run directly using the [SAM Sofware Develoment Kit](https://sam.nrel.gov/sdk).

SSC requires building four other open-source projects:

- [Google Test](https://github.com/google/googletest)
- [LK](https://github.com/nrel/lk)
- [wxWidgets](https://www.wxwidgets.org/)
- [WEX](https://github.com/nrel/wex)
- [jsoncpp](https://github.com/open-source-parsers/jsoncpp)

However, if you remove SDKtool and TCSconsole from your SSC project, you can build SSC without any other software dependencies. Please see the main [SAM project wiki](https://github.com/NREL/SAM/wiki) for complete build instructions and software dependencies.

SSC directly includes source code from three other open-source projects, and builds them as part of its build process.  These projects and their respective licenses are:
- [NLopt](https://nlopt.readthedocs.io/en/latest/) - code located [here](https://github.com/NREL/ssc/tree/develop/nlopt), [LGPL license](https://nlopt.readthedocs.io/en/latest/NLopt_License_and_Copyright/)
- [lp_solve](http://lpsolve.sourceforge.net/5.5/) - code located [here](https://github.com/NREL/ssc/tree/develop/lpsolve), [LGPL license](https://www.gnu.org/licenses/old-licenses/lgpl-2.1.en.html)
- [splinter](https://github.com/bgrimstad/splinter) - code located [here](https://github.com/NREL/ssc/tree/develop/splinter), [MPL license](https://github.com/bgrimstad/splinter/blob/master/LICENSE)


To explore the code and understand the algorithms used in SSC, start by looking in the "SSC" project at the compute modules (files starting with cmod_) to find the compute module for the technology or financial model of interest.

# Contributing

Please see the contribution guidelines in the main [SAM project readme](https://github.com/NREL/SAM/blob/develop/README.md).

# License

SSC is licensed with BSD-3-Clause terms, found [here](https://github.com/NREL/SAM/blob/develop/LICENSE).

# Citing this package

System Advisor Model Version 2020.2.29 (2020.2.29). SSC source code. National Renewable Energy Laboratory. Golden, CO. Accessed May 27, 2020. https://github.com/NREL/ssc
