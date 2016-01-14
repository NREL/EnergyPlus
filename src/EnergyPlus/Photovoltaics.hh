// EnergyPlus, Copyright (c) 1996-2016, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
//
// If you have questions about your rights to use or distribute this software, please contact
// Berkeley Lab's Innovation & Partnerships Office at IPO@lbl.gov.
//
// NOTICE: This Software was developed under funding from the U.S. Department of Energy and the
// U.S. Government consequently retains certain rights. As such, the U.S. Government has been
// granted for itself and others acting on its behalf a paid-up, nonexclusive, irrevocable,
// worldwide license in the Software to reproduce, distribute copies to the public, prepare
// derivative works, and perform publicly and display publicly, and to permit others to do so.
//
// Redistribution and use in source and binary forms, with or without modification, are permitted
// provided that the following conditions are met:
//
// (1) Redistributions of source code must retain the above copyright notice, this list of
//     conditions and the following disclaimer.
//
// (2) Redistributions in binary form must reproduce the above copyright notice, this list of
//     conditions and the following disclaimer in the documentation and/or other materials
//     provided with the distribution.
//
// (3) Neither the name of the University of California, Lawrence Berkeley National Laboratory,
//     the University of Illinois, U.S. Dept. of Energy nor the names of its contributors may be
//     used to endorse or promote products derived from this software without specific prior
//     written permission.
//
// (4) Use of EnergyPlus(TM) Name. If Licensee (i) distributes the software in stand-alone form
//     without changes from the version obtained under this License, or (ii) Licensee makes a
//     reference solely to the software portion of its product, Licensee must refer to the
//     software as "EnergyPlus version X" software, where "X" is the version number Licensee
//     obtained under this License and may not use a different name for the software. Except as
//     specifically required in this Section (4), Licensee shall not use in a company name, a
//     product name, in advertising, publicity, or other promotional activities any name, trade
//     name, trademark, logo, or other designation of "EnergyPlus", "E+", "e+" or confusingly
//     similar designation, without Lawrence Berkeley National Laboratory's prior written consent.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
//
// You are under no obligation whatsoever to provide any bug fixes, patches, or upgrades to the
// features, functionality or performance of the source code ("Enhancements") to anyone; however,
// if you choose to make your Enhancements available either publicly, or directly to Lawrence
// Berkeley National Laboratory, without imposing a separate written license agreement for such
// Enhancements, then you hereby grant the following license: a non-exclusive, royalty-free
// perpetual license to install, use, modify, prepare derivative works, incorporate into other
// computer software, distribute, and sublicense such enhancements or derivative works thereof,
// in binary and source code form.

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

} // Photovoltaics

} // EnergyPlus

#endif
