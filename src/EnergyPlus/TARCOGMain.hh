#ifndef TARCOGMain_hh_INCLUDED
#define TARCOGMain_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1A.hh>
#include <ObjexxFCL/Array2A.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace TARCOGMain {

	// Functions

	void
	TARCOG90(
		int const nlayer, // Number of layers (glass + SD)
		int const iwd, // Wind direction:
		Real64 & tout, // Outdoor temperature [K]
		Real64 & tind, // Indoor temperature [K]
		Real64 & trmin, // Indoor mean radiant temperature [K]
		Real64 const wso, // Outdoor wind speed [m/s]
		Real64 const wsi, // Inside forced air speed [m/s]
		Real64 const dir, // Direct solar radiation [W/m2]
		Real64 const outir, // IR radiance of window's exterior surround [W/m2]
		int const isky, // Flag for sky temperature(Tsky) and sky emittance(esky)
		Real64 const tsky, // Night sky temperature [K]
		Real64 & esky, // Effective night sky emittance
		Real64 const fclr, // Fraction of sky that is clear
		Real64 const VacuumPressure, // maximal pressure for gas to be considered as vacuum
		Real64 & VacuumMaxGapThickness, // maximum allowed thickness without producing warning message
		int const CalcDeflection, // Deflection calculation flag:
		Real64 const Pa, // Atmospheric (outside/inside) pressure (used onlu if CalcDeflection = 1)
		Real64 const Pini, // Initial presssure at time of fabrication (used only if CalcDeflection = 1)
		Real64 const Tini, // Initial temperature at time of fabrication (used only if CalcDeflection = 1)
		Array1A< Real64 > gap, // Vector of gap widths [m]
		Array1A< Real64 > GapDefMax, // Vector of gap widths in deflected state. It will be used as input
		Array1A< Real64 > thick, // Vector of glazing layer thicknesses [m]
		Array1A< Real64 > scon, // Vector of conductivities of each glazing layer  [W/mK]
		Array1A< Real64 > const YoungsMod, // Youngs Modulus coefficients used in deflection calculations
		Array1A< Real64 > const PoissonsRat, // Poissons Ratio coefficients used in deflection calculations
		Array1A< Real64 > const tir, // Vector of IR transmittances of each surface
		Array1A< Real64 > const emis, // Vector of IR emittances of each surface
		Real64 const totsol, // Total solar transmittance of the IGU
		Real64 const tilt, // Window tilt [degrees]
		Array1A< Real64 > const asol, // Vector of Absorbed solar energy fractions for each layer
		Real64 const height, // IGU cavity height
		Real64 const heightt, // Window height
		Real64 const width, // Window width
		Array1A< Real64 > const presure, // Vector of gas pressures in gaps [N/m2]
		Array2A_int const iprop, // Matrix of gas codes – see mgas definition
		Array2A< Real64 > const frct, // Matrix of mass percentages in gap mixtures
		Array2A< Real64 > const xgcon, // Matrix of constants for gas conductivity calc
		Array2A< Real64 > const xgvis, // Matrix of constants for gas dynamic viscosity calc
		Array2A< Real64 > const xgcp, // Matrix of constants for gas specific heat calc at constant pressure
		Array1A< Real64 > const xwght, // Vector of Molecular weights for gasses
		Array1A< Real64 > const gama, // Vector of spefic heat ration for low pressure calc
		Array1A_int const nmix, // Vector of number of gasses in gas mixture of each gap
		Array1A_int const SupportPillar, // Shows whether or not gap have support pillar
		Array1A< Real64 > const PillarSpacing, // Pillar spacing for each gap (used in case there is support pillar)
		Array1A< Real64 > const PillarRadius, // Pillar radius for each gap (used in case there is support pillar)
		Array1A< Real64 > theta, // Vector of average temperatures of glazing surfaces [K]
		Array1A< Real64 > LayerDef, // Vector of layers deflection. [m]
		Array1A< Real64 > q, // Vector of various heat fluxes [W/m2]
		Array1A< Real64 > qv, // Vector of heat fluxes to each gap by ventillation [W/m2]
		Real64 & ufactor, // Center of glass U-value [W/m2 K]
		Real64 & sc, // Shading Coefficient
		Real64 & hflux, // Net heat flux between room and window [W/m2]
		Real64 & hcin, // Indoor convective surface heat transfer coefficient  [W/m2 K]
		Real64 & hcout, // Outdoor convective surface heat transfer coefficient [W/m2 K]
		Real64 & hrin, // Indoor radiative surface heat transfer coefficient [W/m2 K]
		Real64 & hrout, // Outdoor radiative surface heat transfer coefficient [W/m2 K]
		Real64 & hin, // Indoor combined film coefficient (if non-zero) [W/m2K]
		Real64 & hout, // Outdoor combined film coefficient (if non-zero) [W/m2K]
		Array1A< Real64 > hcgas, // Convective part of gap effective conductivity (including in and out)
		Array1A< Real64 > hrgas, // Radiative part of gap effective conductivity (including in and out)
		Real64 & shgc, // Solar heat gain coefficient – per ISO 15099
		int & nperr, // Error code
		std::string & ErrorMessage, // To store error message from tarcog execution
		Real64 & shgct, // Solar heat gain coefficient – per old procedure
		Real64 & tamb, // Outdoor environmental temperature [K]
		Real64 & troom, // Indoor environmental temperature [K]
		Array1A_int const ibc, // Vector of boundary condition flags (ibc(1) - outdoor, ibc(2) - indoor
		Array1A< Real64 > const Atop, // Vector with areas of top openings – between SD layers and top of
		Array1A< Real64 > const Abot, // Vector with areas of bottom openings – between SD layers and
		Array1A< Real64 > const Al, // Vector with areas of left-hand side openings – between SD layers and
		Array1A< Real64 > const Ar, // Vector of areas of right-hand side openings – between SD layers and
		Array1A< Real64 > const Ah, // Vector of total areas of holes for each SD [m2]
		Array1A< Real64 > const SlatThick, // Thickness of the slat material [m]
		Array1A< Real64 > const SlatWidth, // Slat width [m]
		Array1A< Real64 > const SlatAngle, // Slat tilt angle [deg]
		Array1A< Real64 > const SlatCond, // Conductivity of the slat material [W/m.K]
		Array1A< Real64 > const SlatSpacing, // Distance between slats [m]
		Array1A< Real64 > const SlatCurve, // Curvature radius of the slat [m]
		Array1A< Real64 > const vvent, // Vector of velocities for forced ventilation, for each gap, and for
		Array1A< Real64 > const tvent, // Vector of temperatures of ventilation gas for forced ventilation,
		Array1A_int const LayerType, // Glazing layer type flag
		Array1A_int const nslice, // Vector of numbers of slices in a laminated glazing layers
		Array1A< Real64 > const LaminateA, // Left-hand side array for creating slice equations
		Array1A< Real64 > const LaminateB, // Right-hand side array for creating slice equations
		Array1A< Real64 > const sumsol, // Array of absorbed solar energy fractions for each laminated
		Array1A< Real64 > hg, // Gas conductance of the glazing cavity [W/m2 K]
		Array1A< Real64 > hr, // Radiation conductance of the glazing cavity [W/m2 K]
		Array1A< Real64 > hs, // Thermal conductance of the glazing cavity [W/m2 K]
		Real64 & he, // External heat transfer coefficient [W/m2 K] – EN673 and ISO 10292 procedure
		Real64 & hi, // Internal heat transfer coefficient [W/m2 K] – EN673 and ISO 10292 procedure
		Array1A< Real64 > Ra, // Vector of Rayleigh numbers, for each gap
		Array1A< Real64 > Nu, // Vector of Nusselt numbers, for each gap
		int const standard, // Calculation standard switch:
		int const ThermalMod, // Thermal model:
		int const Debug_mode, // Switch for debug output files:
		std::string const & Debug_dir, // Target directory for debug files
		std::string const & Debug_file, // File name template for debug files
		int const win_ID, // ID of window (passed by W6)
		int const igu_ID, // ID of the IGU (passed by W6)
		Real64 & ShadeEmisRatioOut, // Ratio of modified to glass emissivity at the outermost glazing surface
		Real64 & ShadeEmisRatioIn, // Ratio of modified to glass emissivity at the innermost glazing surface
		Real64 & ShadeHcRatioOut, // Ratio of modified to unshaded Hc at the outermost glazing surface
		Real64 & ShadeHcRatioIn, // Ratio of modified to unshaded Hc at the innermost glazing surface
		Real64 & HcUnshadedOut, // Hc value at outermost glazing surface of an unshaded subsystem [W/m2 K]
		Real64 & HcUnshadedIn, // Hc value at innermost glazing surface of an unshaded subsystem [W/m2 K]
		Array1A< Real64 > Keff, // Vector of keff values for gaps [W/m.K]
		Array1A< Real64 > ShadeGapKeffConv, // Vector of convective keff values for areas above/below
		Real64 const SDScalar, // Factor of Venetian SD layer contribution to convection
		int const SHGCCalc, // SHGC calculation switch:
		int & NumOfIterations // Number of iterations for reacing solution
	);

	//     NOTICE

	//     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
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

} // TARCOGMain

} // EnergyPlus

#endif
