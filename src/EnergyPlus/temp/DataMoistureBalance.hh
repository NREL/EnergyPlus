#ifndef DataMoistureBalance_hh_INCLUDED
#define DataMoistureBalance_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>
#include <ObjexxFCL/FArray2D.hh>
#include <ObjexxFCL/FArray3D.hh>
#include <ObjexxFCL/FArray4D.hh>
#include <ObjexxFCL/FArray5D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace DataMoistureBalance {

	// Data
	// module should be available to other modules and routines.  Thus,
	// all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS

	// Parameters for the definition and limitation of arrays:

	// This is more or less the traditional value from BLAST.
	extern Real64 const Lam; // heat of adsorption for building materials

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// MODULE VARIABLE DECLARATIONS:
	// Public Variables that will also be used in the Moisture Surface Balance
	extern FArray3D< Real64 > FluxH; // transfer function coeff for calculating the CPF Flux history term
	extern FArray5D< Real64 > IcoefH; // transfer function coeff for calculating the CPF history term
	extern FArray4D< Real64 > Icoef; // transfer function coeff for calculating the CPF history term
	extern FArray2D< Real64 > DiffC; // Thermal Diffusivity in combined potential formulation (CPF)
	// for each equation
	extern FArray2D< Real64 > mtinc; // # of Moisture transfer function time increment for each equation
	extern FArray1D< Real64 > S1; // Thermal Diffusivity in combined potential formulation (CPF)
	// for each equation
	extern FArray1D< Real64 > R2; // Thermal Diffusivity in combined potential formulation (CPF)
	// for each equation
	extern FArray1D< Real64 > TempOutsideAirFD; // Temperature outside air for the FD surface

	extern FArray2D_int mhstry; // # of FD History terms for each equation
	extern FArray1D_int CMTF; // Type of material layer
	extern FArray2D_int Nmrf; // # of Moisture Response Factors for CPF Solution

	//variables used for MTF moisture implementation
	extern FArray1D< Real64 > RhoVaporAirOut; // Vapor Density outside surface
	extern FArray1D< Real64 > RhoVaporAirIn; // Vapor Density inside surface
	extern FArray1D< Real64 > HConvExtFD; // thermal convection coefficient outside surface
	extern FArray1D< Real64 > HMassConvExtFD; // mass convection coefficient outside surface
	extern FArray1D< Real64 > HConvInFD; // thermal convection coefficient inside surface
	extern FArray1D< Real64 > HMassConvInFD; // mass convection coefficient inside surface
	extern FArray1D< Real64 > RhoVaporSurfIn; // Vapor Density inside surface
	extern FArray1D< Real64 > HSkyFD; // Sky Convection Coefficient
	extern FArray1D< Real64 > HGrndFD; // Ground Convection Coefficient
	extern FArray1D< Real64 > HAirFD; // Air Convection Coefficient

} // DataMoistureBalance

} // EnergyPlus

#endif
