// EnergyPlus Headers
#include <DataMoistureBalance.hh>
#include <DataPrecisionGlobals.hh>

namespace EnergyPlus {

namespace DataMoistureBalance {

	// MODULE INFORMATION:
	//       AUTHOR         Richard J. Liesen
	//       DATE WRITTEN   May 2000
	//       MODIFIED       April 2008; CondFD still uses some of this data.
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This module should contain the information that is needed to between the
	// MTF moisture modules and the calculation of the transfer functions
	// Data is still used in the CondFD solution.

	// Using/Aliasing
	using namespace DataPrecisionGlobals;

	// Data
	// module should be available to other modules and routines.  Thus,
	// all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS

	// Parameters for the definition and limitation of arrays:

	// This is more or less the traditional value from BLAST.
	Real64 const Lam( 2500000.0 ); // heat of adsorption for building materials

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// MODULE VARIABLE DECLARATIONS:
	// Public Variables that will also be used in the Moisture Surface Balance
	Array3D< Real64 > FluxH; // transfer function coeff for calculating the CPF Flux history term
	Array5D< Real64 > IcoefH; // transfer function coeff for calculating the CPF history term
	Array4D< Real64 > Icoef; // transfer function coeff for calculating the CPF history term
	Array2D< Real64 > DiffC; // Thermal Diffusivity in combined potential formulation (CPF)
	// for each equation
	Array2D< Real64 > mtinc; // # of Moisture transfer function time increment for each equation
	Array1D< Real64 > S1; // Thermal Diffusivity in combined potential formulation (CPF)
	// for each equation
	Array1D< Real64 > R2; // Thermal Diffusivity in combined potential formulation (CPF)
	// for each equation
	Array1D< Real64 > TempOutsideAirFD; // Temperature outside air for the FD surface

	Array2D_int mhstry; // # of FD History terms for each equation
	Array1D_int CMTF; // Type of material layer
	Array2D_int Nmrf; // # of Moisture Response Factors for CPF Solution

	//variables used for MTF moisture implementation
	Array1D< Real64 > RhoVaporAirOut; // Vapor Density outside surface
	Array1D< Real64 > RhoVaporAirIn; // Vapor Density inside surface
	Array1D< Real64 > HConvExtFD; // thermal convection coefficient outside surface
	Array1D< Real64 > HMassConvExtFD; // mass convection coefficient outside surface
	Array1D< Real64 > HConvInFD; // thermal convection coefficient inside surface
	Array1D< Real64 > HMassConvInFD; // mass convection coefficient inside surface
	Array1D< Real64 > RhoVaporSurfIn; // Vapor Density inside surface
	Array1D< Real64 > HSkyFD; // Sky Convection Coefficient
	Array1D< Real64 > HGrndFD; // Ground Convection Coefficient
	Array1D< Real64 > HAirFD; // Air Convection Coefficient

	//     NOTICE
	//     Copyright (c) 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.
	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in main.cc.
	//     NOTICE: The U.S. Government is granted for itself and others acting on its
	//     behalf a paid-up, nonexclusive, irrevocable, worldwide license in this data to
	//     reproduce, prepare derivative works, and perform publicly and display publicly.
	//     Beginning five (5) years after permission to assert copyright is granted,
	//     subject to two possible five year renewals, the U.S. Government is granted for
	//     itself and others acting on its behalf a paid-up, non-exclusive, irrevocable
	//     worldwide license in this data to reproduce, prepare derivative works,
	//     distribute copies to the public, perform publicly and display publicly, and to
	//     permit others to do so.
	//     TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.

} // DataMoistureBalance

} // EnergyPlus
