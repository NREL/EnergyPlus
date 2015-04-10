#ifndef Photovoltaics_hh_INCLUDED
#define Photovoltaics_hh_INCLUDED

// C++ Headers
#include <functional>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace Photovoltaics {

	// Data
	//MODULE PARAMETER DEFINITIONS:
	// na

	// DERIVED TYPE DEFINITIONS:
	//   see DataPhotovoltaics.cc

	extern Array1D_bool CheckEquipName;

	//SUBROUTINE SPECIFICATIONS FOR MODULE Photovoltaics

	// The following subroutines are used for the SIMPLE model

	// The following subroutines and functions are used for only the EQUIVALENT ONE-DIODE model

	// The following subroutines and functions are used for the Sandia model.

	//  OO get set methods for coupling to exterior vented baffle cavity mounting configurations

	// Functions

	void
	SimPVGenerator(
		int const GeneratorType, // type of Generator !unused1208
		std::string const & GeneratorName, // user specified name of Generator
		int & GeneratorIndex,
		bool const RunFlag, // is PV ON or OFF as determined by schedules in ElecLoadCenter
		Real64 const PVLoad // electrical load on the PV (not really used... PV models assume "full on" !unused1208
	);

	void
	GetPVGeneratorResults(
		int const GeneratorType, // type of Generator !unused1208
		int const GeneratorIndex,
		Real64 & GeneratorPower, // electrical power
		Real64 & GeneratorEnergy, // electrical energy
		Real64 & ThermalPower,
		Real64 & ThermalEnergy
	);

	// *************

	void
	GetPVInput();

	// **************************************

	void
	CalcSimplePV(
		int const thisPV,
		bool const RunFlag // unused1208
	);

	void
	ReportPV( int const PVnum );

	// *************

	void
	CalcSandiaPV(
		int const PVnum, // ptr to current PV system
		bool const RunFlag // controls if generator is scheduled *ON*
	);

	// ********************
	// begin routines for Equivalent one-diode model by Bradley/Ulleberg

	void
	InitTRNSYSPV( int const PVnum ); // the number of the GENERATOR:PHOTOVOLTAICS (passed in)

	// *************

	void
	CalcTRNSYSPV(
		int const PVnum, // BTG added intent
		bool const RunFlag // BTG added intent    !flag tells whether the PV is ON or OFF
	);

	void
	POWER(
		Real64 const IO, // passed in from CalcPV
		Real64 const IL, // passed in from CalcPV
		Real64 const RSER, // passed in from CalcPV
		Real64 const AA, // passed in from CalcPV
		Real64 const EPS, // passed in from CalcPV
		Real64 & II, // current [A]
		Real64 & VV, // voltage [V]
		Real64 & PP // power [W]
	);

	void
	NEWTON(
		Real64 & XX,
		std::function< Real64( Real64 const, Real64 const, Real64 const, Real64 const, Real64 const, Real64 const ) > FXX,
		std::function< Real64( Real64 const, Real64 const, Real64 const, Real64 const, Real64 const ) > DER,
		Real64 const & II, //Autodesk Aliased to XX in some calls
		Real64 const & VV, //Autodesk Aliased to XX in some calls
		Real64 const IO,
		Real64 const IL,
		Real64 const RSER,
		Real64 const AA,
		Real64 const XS,
		Real64 const EPS
	);

	void
	SEARCH(
		Real64 & A,
		Real64 & B,
		Real64 & P,
		int & K,
		Real64 & IO,
		Real64 & IL,
		Real64 & RSER,
		Real64 & AA,
		Real64 const EPS,
		int const KMAX
	);

	Real64
	FUN(
		Real64 const II,
		Real64 const VV,
		Real64 const IL,
		Real64 const IO,
		Real64 const RSER,
		Real64 const AA
	);

	Real64
	FI(
		Real64 const II,
		Real64 const VV,
		Real64 const IO,
		Real64 const RSER,
		Real64 const AA
	);

	Real64
	FV(
		Real64 const II,
		Real64 const VV,
		Real64 const IO,
		Real64 const RSER,
		Real64 const AA
	);

	// End routines for Equivalent One-Diode model as implemented by Bradley
	//************************************************************************

	// Begin supporting routines for Sandia PV model
	// -------------------------------------------------------------------------------

	Real64
	SandiaModuleTemperature(
		Real64 const Ibc, // beam radiation on collector plane, W/m2
		Real64 const Idc, // Diffuse radiation on collector plane, W/m2
		Real64 const Ws, // wind speed, m/s
		Real64 const Ta, // ambient temperature, degC
		Real64 const fd, // fraction of Idc used (empirical constant)
		Real64 const a, // empirical constant
		Real64 const b // empirical constant
	);

	// -------------------------------------------------------------------------------
	// -------------------------------------------------------------------------------

	Real64
	SandiaTcellFromTmodule(
		Real64 const Tm, // module temperature (deg C)
		Real64 const Ibc, // beam radiation on collector plane, W/m2
		Real64 const Idc, // Diffuse radiation on collector plane, W/m2
		Real64 const fd, // fraction of Idc used (empirical constant)
		Real64 const DT0 // (Tc-Tm) at E=1000 W/m2 (empirical constant known as delta T), deg C
	);

	// -------------------------------------------------------------------------------

	Real64
	SandiaCellTemperature(
		Real64 const Ibc, // beam radiation on collector plane W/m2
		Real64 const Idc, // Diffuse radiation on collector plane W/m2
		Real64 const Ws, // wind speed, m/s
		Real64 const Ta, // ambient temperature, degC
		Real64 const fd, // fraction of Idc used (empirical constant)
		Real64 const a, // empirical constant
		Real64 const b, // empirical constant
		Real64 const DT0 // (Tc-Tm) at E=1000 W/m2 (empirical constant known as dTc), deg C
	);

	// -------------------------------------------------------------------------------

	Real64
	SandiaEffectiveIrradiance(
		Real64 const Tc, // cell temperature (deg C)
		Real64 const Isc, // short-circuit current under operating conditions (A)
		Real64 const Isc0, // reference Isc at Tc=25 C, Ic=1000 W/m2 (A)
		Real64 const aIsc // Isc temperature coefficient (degC^-1)
	);

	// -------------------------------------------------------------------------------

	Real64
	AbsoluteAirMass(
		Real64 const SolZen, // solar zenith angle (deg)
		Real64 const Altitude // site altitude (m)
	);

	// -------------------------------------------------------------------------------

	Real64
	SandiaF1(
		Real64 const AMa, // absolute air mass
		Real64 const a0, // empirical constant, module-specific
		Real64 const a1, // empirical constant, module-specific
		Real64 const a2, // empirical constant, module-specific
		Real64 const a3, // empirical constant, module-specific
		Real64 const a4 // empirical constant, module-specific
	);

	// -------------------------------------------------------------------------------

	Real64
	SandiaF2(
		Real64 const IncAng, // incidence angle (deg)
		Real64 const b0, // empirical module-specific constants
		Real64 const b1, // empirical module-specific constants
		Real64 const b2, // empirical module-specific constants
		Real64 const b3, // empirical module-specific constants
		Real64 const b4, // empirical module-specific constants
		Real64 const b5 // empirical module-specific constants
	);

	// -------------------------------------------------------------------------------

	Real64
	SandiaImp(
		Real64 const Tc, // cell temperature (degC)
		Real64 const Ee, // effective irradiance (W/m2)
		Real64 const Imp0, // current at MPP at SRC (1000 W/m2, 25 C) (A)
		Real64 const aImp, // Imp temperature coefficient (degC^-1)
		Real64 const C0, // empirical module-specific constants
		Real64 const C1 // empirical module-specific constants
	);

	// -------------------------------------------------------------------------------

	Real64
	SandiaIsc(
		Real64 const Tc, // cell temperature (deg C)
		Real64 const Isc0, // Isc at Tc=25 C, Ic=1000 W/m2 (A)
		Real64 const Ibc, // beam radiation on collector plane (W/m2)
		Real64 const Idc, // Diffuse radiation on collector plane (W/m2)
		Real64 const F1, // Sandia F1 function for air mass effects
		Real64 const F2, // Sandia F2 function of incidence angle
		Real64 const fd, // module-specific empirical constant
		Real64 const aIsc // Isc temperature coefficient (degC^-1)
	);

	// -------------------------------------------------------------------------------

	Real64
	SandiaIx(
		Real64 const Tc, // cell temperature (deg C)
		Real64 const Ee, // effective irradiance
		Real64 const Ix0, // Ix at SRC (1000 W/m2, 25 C) (A)
		Real64 const aIsc, // Isc temp coefficient (/C)
		Real64 const aImp, // Imp temp coefficient (/C)
		Real64 const C4, // empirical module-specific constants
		Real64 const C5 // empirical module-specific constants
	);

	// -------------------------------------------------------------------------------

	Real64
	SandiaIxx(
		Real64 const Tc, // cell temperature (deg C)
		Real64 const Ee, // effective irradiance (W/m2 ?)
		Real64 const Ixx0, // Ixx at SRC (1000 W/m2, 25 C) (A)
		Real64 const aImp, // Imp temp coefficient (/C)
		Real64 const C6, // empirical module-specific constants
		Real64 const C7 // empirical module-specific constants
	);

	// -------------------------------------------------------------------------------

	Real64
	SandiaVmp(
		Real64 const Tc, // cell temperature (deg C)
		Real64 const Ee, // effective irradiance
		Real64 const Vmp0, // Vmp at SRC (1000 W/m2, 25 C) (V)
		Real64 const NcellSer, // # cells in series
		Real64 const DiodeFactor, // module-specIFic empirical constant
		Real64 const BVmp0, // Vmp temperature coefficient (V/C)
		Real64 const mBVmp, // change in BVmp with irradiance
		Real64 const C2, // empirical module-specific constants
		Real64 const C3 // empirical module-specific constants
	);

	// -------------------------------------------------------------------------------

	Real64
	SandiaVoc(
		Real64 const Tc, // cell temperature (deg C)
		Real64 const Ee, // effective irradiance
		Real64 const Voc0, // Voc at SRC (1000 W/m2, 25 C) (V)
		Real64 const NcellSer, // # cells in series
		Real64 const DiodeFactor, // module-specIFic empirical constant
		Real64 const BVoc0, // Voc temperature coefficient (V/C)
		Real64 const mBVoc // change in BVoc with irradiance
	);

	void
	SetVentedModuleQdotSource(
		int const VentModNum,
		Real64 const QSource // source term in Watts
	);

	void
	GetExtVentedCavityIndex(
		int const SurfacePtr,
		int & VentCavIndex
	);

	void
	GetExtVentedCavityTsColl(
		int const VentModNum,
		Real64 & TsColl
	);

	// -------------------------------------------------------------------------------

	//     EnergyPlus V1.2 and beyond include models for photovoltaic calculations called
	//     Generator:Photovoltaic:Simple and Generator:PV:Sandia implemented by the Center for
	//     Buildings and Thermal Systems, National Renewable Energy Laboratory, 1617 Cole Blvd
	//     MS 2722, Golden, CO, 80401

	//     EnergyPlus v1.1.1 and beyond includes model for Photovoltaic calculations, now
	//     referred to as the Generator:PV:Equivalent One-Diode model developed by Thermal Energy
	//     System Specialists, 2916 Marketplace Drive, Suite 104, Madison, WI 53719;
	//     Tel: (608) 274-2577

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

} // Photovoltaics

} // EnergyPlus

#endif
