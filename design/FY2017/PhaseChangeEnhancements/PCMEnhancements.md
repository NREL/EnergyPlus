# Phase Change Project

This project is based on making enhancements to the phase change material simulation model in EnergyPlus.

## Purpose/Background

The purpose of this project is to make improvements to tbe building envelope simulation model, specifically to the dynamic properties, phase change material simulation model that is embedded as a part of the finite difference transient wall conduction model.

The enhancements include:

- Adding this capability
- And this one

These capabilities have already been prototyped in a modified version of the last FORTRAN version of EnergyPlus, 8.1.0.009.  The changes were made by NRGSim and they have used this modified code base in their workflow for some time.

The United States Department of Energy started the Small Business Voucher program in order to assist small companies in getting their potentially-energy-saving technologies or products to market.  This project was selected as a candidate to receive funding (with company cost-share) to enable NREL to review/test/deploy the changes in the current version of EnergyPlus.

## Project Overview

The project will consist of three distinct phases:

- Phase 1: A refactoring pass will be made over the existing C++ code base to prepare for adding more enhancements.  Code readability and performance will be the foci of this refactoring work, which is expected to cause no-diffs in test results unless a bug is found in the code and repaired.
- Phase 2: The prototype FORTRAN code will be modified/cleaned/etc. to prepare it for implementation in the C++ code
- Phase 3: The new version will be implemented in the C++ code.

## Phase 1: Refactoring Existing C++ Codebase

The existing codebase is managed almost entirely in `HeatBalFiniteDiffManager`.

### Existing Code Analysis

The header file lists the relevant variables, structs, and functions:

- Structs
 - `ConstructionDataFD` contains individual arrays for each layer a construction, mostly of static construction properties, followed by a couple variables for the construction as a whole.
  - This is weird.  Anytime you have multiple arrays allocated to the same amount, it screams of a needed refactor to create a different, single struct, that gets allocated once to the right size.
 - `SurfaceDataFD` contains individual arrays for a particular surface, mostly dynamic state properties such as temperature, followed by just a few surface-specific variables.  It also contains a single inline function for updating the moisture balance properties.
  - Again, ripe for refactoring.
 - `MaterialDataFD` contains two dimensional arrays that characterize the enthalpy and thermal conductivity as a function of temperature for a particular material, if it has these variable properties.
  - The overall nature of the arrays could likely be improved, but this isn't too bad.
- Variables
 - There are a few const variables that are simply initialization values, no problem.
 - There are a few integers that appear to be ripe for an enum class conversion.
 - There are a few arrays that appear to be allocated to the number of surfaces, that should simply be in the surface class structure.
 - There are then arrays of each of the structs listed above.
- Functions
 - `ManageHeatBalFiniteDiff` is the main entry point to this manager, which accepts a surface index, and two arguments: inside and outside surface temperature *references*, which may be passed back out to the caller
 - `GetCondFDInput` is the main input processor for this manager, which reads idf input and allocates/populates module level arrays.  This accepts no arguments.
 - `InitHeatBalFiniteDiff` is the main initialization function for this manager, which accepts no arguments - indicating it updates for all finite difference calculations, not just the current surface.  This should likely be changed to be surface-specific.
 - `InitialInitHeatBalFiniteDiff` appears to be a one-time initialization function for this manager?  This probably can be easily refactored away into the main, single, initialization function, but we'll see.
 - `CalcHeatBalFiniteDiff` is the main calculation function for this manager, calling all lower level worker functions as needed.  This function has the same argument signature as the main entry point to the manager.
 - `ReportFiniteDiffInits` may be simply a one-time reporting function that reports eio-style outputs
 - `CalcNodeHeatFlux` appears as a worker function that simply reports the node heat flux for a given node, although it only accepts a surface number and the total number of nodes, not a particular node...confusing.
 - `terpld` is a worker function for interpolating within the 2-d arrays, this needs to be either specified for this module (as there are likely performance gains to be had), or generalized out of this module.
 - `ExteriorBCEqns`, `InteriorNodeEqns`, `IntInterfaceNodeEqns`, `InteriorBCEqns` are worker functions that do the grunt work of solving the finite difference equations for different configurations.  These functions take way too many arguments and should be cleaned up heavily.
 - `CheckFDSurfaceTempLimits` is a simple worker for checking if any temperatures are out of bounds, only minor clean up likely.

### Improved Design Possibilities

## Phase 2: Preparing Modified FORTRAN Codebase

### Characterizing the Scope

### Input Changes

### Code Changes

### Testing

## Phase 3: Implementing New Model

### Final Input Design

### Final Code Design

### Testing Results and Demonstration

## References
