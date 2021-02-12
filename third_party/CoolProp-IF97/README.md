[![Build Status](https://travis-ci.org/CoolProp/IF97.svg?branch=master)](https://travis-ci.org/CoolProp/IF97)

CoolProp IF97
=============

(c) Ian Bell and the CoolProp team  

This repository implements the IF97 formulation for the properties of pure water substance.  

Why?
----

Almost all of the other implementations of IF97 are not free and closed-source.  This implementation is:

* Entirely free and open-source (see license below)
* Written in optimized standard C++ code so it will compile anywhere
* Fast
* Easy-to-use (just a single header)  

Try it
------

Build the stand-alone CMake project doing something like 
    
```
mkdir build
cd build
cmake ..
cmake --build .
```

This will spit out the values for the computer-program verification, they should agree with the values from http://www.iapws.org/relguide/IF97-Rev.pdf and other IAPWS documents as noted in the output.  In Region 3, the backwards equations are used, which results in some loss of precision, but it is usually less than 0.001%.  

Accessing IF97 from your software
---------------------------------

The simplest way to include IF97 in your C++ project is to simply copy and use the header file. If you are looking for an installation command, you can use `cmake .. -DIF97_HEADER_MODULE=ON; cmake --build . --target install`.

There is a `wrapper` directory for 3rd party applications, including:
- [Mathcad 15](wrapper/Mathcad) (Jeff Henning main contributor)
- [Mathcad Prime](wrapper/Mathcad) (Jeff Henning main contributor)
- [postgreSQL](wrapper/Postgresql) (Martin Mouterde main contributor)

See the README in each wrapper directory for instructions on building and installing each.

Furthermore, there is an interface library target that installs the header file. This target can also be digested by other CMake-based projects using the `add_subdirectory` command. Enable it with the CMake option `cmake .. -DIF97_CMAKE_MODULE=ON; cmake --build . --target install`.


Compiler Switches
-----------------

There are two compiler switches that can be used to modify the behavior of the IF97 function library.  

- ``REGION3_ITERATE``: If defined in the main program, will use the supplemental backward equations in Region 3 (mostly the supercritical region) to generate an initial guess for Density as a function of Temperature and Pressure and then use that initial guess for a Newton-Raphson solution of the original IF97 Revised Release for p = f(T,rho) to generate a more accurate solution.  If ``REGION3_ITERATE`` is not defined, the supplemental backward equations in Region 3 are used directly, which an error on the order of 1E-6, but about 2.6 times faster.  

- ``IAPWS_UNITS``: By default, all input and output values of the IF97 functions are in SI Units, including [Pa] for Pressure and \[J\] \(Joule\) for Energy (Enthalpy, Entropy, etc.).  By defining ``IAPWS_UNITS``, Pressure inputs/outputs will use [MPa] and all *_thermodynamic_* properties will use units of \[kJ\] \(instead of \[J\]\) as originally defined in the IAPWS IF97 Release documents.  For example the function *_hmass(T,p)_* will require pressure input units of [MPa] and return values in [kJ/kg].  All other unit types (kg, m, K) are SI units.   Transport properties of Viscosity [Pa-s], Thermal Conductivity [W/m-K], and Surface Tension [N/m] always return values in these SI units, independent of the condition of the ``IAPWS_UNITS`` flag, however pressure *_input_* values *_will_* depend on the condition of the ``IAPWS_UNITS`` compiler flag.  

Usage
-----

See ``IF97.cpp``.  

The primary functions needed are ``rhomass_Tp(T,p)``, ``hmass_Tp(T,p)``, etc. where in all cases, the units are base-SI units (Pa, K, J/kg, etc.)  
  
Liquid and vapor values along the saturation curve can be obtained using ``rholiq_p(p)``, ``rhovap_p(p)``, ``sliq_p(p)``, ``svap_p(p)``, etc.; all as a function of pressure.

There are also ``Tsat97(p)`` and ``psat97(T)`` functions to get values from the saturation line.

Backward functions have been implemented to return temperature as a function of pressure and either enthalpy or entropy; ``T_phmass(p,h)`` and ``T_psmass(p,s)``.  Backward functions have also been implemented to return temperature or pressure as a function of enthalpy and entropy, ``p_hsmass(h,s)`` and ``T_hsmass(h,s)``, to facilitate thermodynamic cycle calculations.

Transport property functions have been implemented for temperature/pressure state points as well as along the saturation curve.  These include
- Viscosity functions: ``visc_Tp(T,p)``, ``viscliq_p(p)``, and ``viscvap_p(p)``
- Thermal Conductivity functions: ``tcond_Tp(T,p)``, ``tcondliq_p(p)``, and ``tcondvap_p(p)``
- Surface Tension: ``sigma97(t)``
- Prandtl Number: ``prandtl_Tp(T,p)``, ``prandtlliq_p(p)``, and ``prandtlvap_p(p)``  

Utility functions provide forward and backward evaluations using vapor quality in the steam dome.  These include:
- Forward functions: ``Q_phmass(p,h)``, ``Q_pumass(p,u)``, ``Q_psmass(p,s)``, ``Q_prhomass(p,rho)``, ``Q_pv(p,v)``
- Backward functions: ``hmass_pQ(p,Q)``, ``umass_pQ(p,Q)``, ``smass_pQ(p,Q)``, ``rhomass_pQ(p,Q)``, ``vmass_pQ(p,Q)``

As of IF97 v2.0.0, a utility function ``get_if97_version()`` will return the official version string for this IF97 implementation.  

License
-------

MIT-style license (see LICENSE)

Basically, you can do anything you like with the code.  The MIT license is a very permissive license, allowing you to modify, distribute, sell, etc. the code.  It is *not* a copy-left license, you can use this in commercial code.  

You are strongly requested, but not required, to cite both this repository and that of CoolProp: www.coolprop.org  
