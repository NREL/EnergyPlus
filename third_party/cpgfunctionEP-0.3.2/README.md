# cpgfunctionEP
An open source low level (C++) implementation of [Massimo Cimmino][1]'s
g-function methodology. The g-function calculation has to do with the distribution
of heat in a ground heat exchanger (GHE). The g-function, after computed,
can be used to simulate a
ground source heat pump to determine the heat pump
exiting fluid temperatures after a
period of time. It is of paramount importance to accurately predict
the thermal response of the borefield. An improper prediction of the
ground response could result in a system too large (waste of money) or a
system too small (resulting in failure). The g-function is currently the only
known methodology to accurately predict the thermal response of the ground.

Computing the g-function is a computationally demanding procedure. Historically,
g-functions are pre-computed and stored in libraries (databases) which can be
accessed in GHE design tools. This is done so that the ground source heat pump
can be sized within a few seconds, rather than waiting anywhere from minutes
to hours for a g-function to be computed. This code base was implemented to help
solve a major limitation (problem) associated with
computing large sets of g-functions for borefields containing anywhere from 1
to 1024 boreholes. Massimo Cimmino's open source implementation of the g-function
calculation
in Python ([pygfunction][2]) has a limitation; at times the memory consumption is
inordinate.
This library was implemented so that computing g-functions would require
significantly less memory to increase throughput on high performance clusters
which contain many "low memory" nodes, and only a few "high memory" nodes.

The g-function is greatly dependent on the boundary condition used. The following
is a checklist of boundary conditions contained in this library:

- [ ] Uniform heat flux (UHF)
- [x] Uniform borehole wall temperature (UBHWT)
- [ ] Uniform inlet fluid temperature (UIFT)

[1]: https://github.com/MassimoCimmino
[2]: https://github.com/MassimoCimmino/pygfunction

# Build

Create a build with cmake:

```
cd /path/to/repo
mkdir build
cd build
ccmake ..   # set settings as needed, shouldn't be any of interest, configure then generate
```

On a Visual Studio generator this will create a solution you can launch into Visual Studio.  
On makefile style generators, this will create a makefile.  From the build directory, just make the project:

```
make -j 4  # number of processors to use to build
```

Then you can run the test:

```
ctest
```

On my platform this resulted in:
```
Test project /home/edwin/Projects/cpgfunction/cmake-build-debug
    Start 1: RunTest1
1/1 Test #1: RunTest1 .........................   Passed    0.79 sec
```

This ran what used to just be the main.cpp built file.

# Acknowledgments
The initial funding of this research project was provided by Dr. Jeffrey D. 
Spitler of Oklahoma State University. Further development of the library was
funded by the National Renewable Energy Laboratory (NREL), which is owned by
the U.S. Department of Energy (DOE). The NREL contract requests delivery of 
cpgfunction for use in EnergyPlus as a third party tool to calculate fast and 
accurate g-functions. Initial integration into EnergyPlus makes use of the 
UBHWT boundary condition with the adaptive discretization scheme to approximate 
UIFT g-functions.

The original g-function theory appeared in a Per Eskilon's PhD thesis,
his advisor who helped develop that theory was Prof. Johan Claesson of Lund
University in Sweden. Massimo Cimmino, Assistant Professor at Polytechnique
Montreal an Canada, has built on the work of Eskilson, Claesson
and others. Cimmino has developed a methodology for g-function calculations,
written programs to compute the g-function and then made the program open source.
The openness of Cimmino's g-function calculation is monumental.
