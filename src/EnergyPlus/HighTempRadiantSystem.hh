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

#ifndef HighTempRadiantSystem_hh_INCLUDED
#define HighTempRadiantSystem_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace HighTempRadiantSystem {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:
	extern std::string const cGas;
	extern std::string const cNaturalGas;
	extern std::string const cElectric;
	extern std::string const cElectricity;
	extern int const Gas;
	extern int const Electric;
	extern std::string const cMATControl; // Control for using mean air temperature
	extern std::string const cMRTControl; // Control for using mean radiant temperature
	extern std::string const cOperativeControl; // Control for using operative temperature
	extern std::string const cMATSPControl; // Control for to MAT setpoint
	extern std::string const cMRTSPControl; // Control for to MRT setpoint
	extern std::string const cOperativeSPControl; // Control for operative temperature setpoint
	extern int const MATControl;
	extern int const MRTControl;
	extern int const OperativeControl;
	extern int const MATSPControl;
	extern int const MRTSPControl;
	extern int const OperativeSPControl;

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	// Standard, run-of-the-mill variables...
	extern int NumOfHighTempRadSys; // Number of hydronic low tempererature radiant systems
	extern Array1D< Real64 > QHTRadSource; // Need to keep the last value in case we are still iterating
	extern Array1D< Real64 > QHTRadSrcAvg; // Need to keep the last value in case we are still iterating
	extern Array1D< Real64 > ZeroSourceSumHATsurf; // Equal to the SumHATsurf for all the walls in a zone with no source
	// Record keeping variables used to calculate QHTRadSrcAvg locally
	extern Array1D< Real64 > LastQHTRadSrc; // Need to keep the last value in case we are still iterating
	extern Array1D< Real64 > LastSysTimeElapsed; // Need to keep the last value in case we are still iterating
	extern Array1D< Real64 > LastTimeStepSys; // Need to keep the last value in case we are still iterating
	extern Array1D_bool MySizeFlag;
	extern Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE HighTempRadiantSystem

	// Types

	struct HighTempRadiantSystemData
	{
		// Members
		// Input data
		std::string Name; // name of hydronic radiant system
		std::string SchedName; // availability schedule
		int SchedPtr; // index to schedule
		std::string ZoneName; // Name of zone the system is serving
		int ZonePtr; // Point to this zone in the Zone derived type
		int HeaterType; // Type of heater (gas or electric)
		Real64 MaxPowerCapac; // Maximum capacity of the radiant heater in Watts
		Real64 CombustionEffic; // Combustion efficiency (only valid for a gas heater)
		Real64 FracRadiant; // Fraction of heater power that is given off as radiant heat
		Real64 FracLatent; // Fraction of heater power that is given off as latent heat
		Real64 FracLost; // Fraction of heater power that is lost to the outside environment
		Real64 FracConvect; // Fraction of heater power that is given off as convective heat
		// (by definition this is 1 minus the sum of all other fractions)
		int ControlType; // Control type for the system (MAT, MRT, or op temp)
		Real64 ThrottlRange; // Throttling range for heating [C]
		std::string SetptSched; // Schedule name for the zone setpoint temperature
		int SetptSchedPtr; // Schedule index for the zone setpoint temperature
		Real64 FracDistribPerson; // Fraction of fraction radiant incident on a "person" in the space
		int TotSurfToDistrib; // Total number of surfaces the heater sends radiation to
		Array1D_string SurfaceName; // Surface name in the list of surfaces heater sends radiation to
		Array1D_int SurfacePtr; // Surface number in the list of surfaces heater sends radiation to
		Array1D< Real64 > FracDistribToSurf; // Fraction of fraction radiant incident on the surface
		// Other parameters
		// Report data
		Real64 ElecPower; // system electric consumption in Watts
		Real64 ElecEnergy; // system electric consumption in Joules
		Real64 GasPower; // system gas consumption in Watts
		Real64 GasEnergy; // system gas consumption in Joules
		Real64 HeatPower; // actual heating sent to zone (convective and radiative) in Watts
		Real64 HeatEnergy; // actual heating sent to zone (convective and radiative) in Joules
		int HeatingCapMethod; // - Method for High Temperature Radiant heating capacity scalable sizing calculation (HeatingDesignCapacity, CapacityPerFloorArea, FracOfAutosizedHeatingCapacity)
		Real64 ScaledHeatingCapacity; // - High Temperature Radiant scaled maximum heating capacity {W} or scalable variable for sizing in {-}, or {W/m2}

		// Default Constructor
		HighTempRadiantSystemData() :
			SchedPtr( 0 ),
			ZonePtr( 0 ),
			HeaterType( 0 ),
			MaxPowerCapac( 0.0 ),
			CombustionEffic( 0.0 ),
			FracRadiant( 0.0 ),
			FracLatent( 0.0 ),
			FracLost( 0.0 ),
			FracConvect( 0.0 ),
			ControlType( 0 ),
			ThrottlRange( 0.0 ),
			SetptSchedPtr( 0 ),
			FracDistribPerson( 0.0 ),
			TotSurfToDistrib( 0 ),
			ElecPower( 0.0 ),
			ElecEnergy( 0.0 ),
			GasPower( 0.0 ),
			GasEnergy( 0.0 ),
			HeatPower( 0.0 ),
			HeatEnergy( 0.0 ),
			HeatingCapMethod( 0 ),
			ScaledHeatingCapacity( 0.0 )
		{}

	};

	struct HighTempRadSysNumericFieldData
	{
		// Members
		Array1D_string FieldNames;

		// Default Constructor
		HighTempRadSysNumericFieldData()
		{}

	};

	// Object Data
	extern Array1D< HighTempRadiantSystemData > HighTempRadSys;
	extern Array1D< HighTempRadSysNumericFieldData > HighTempRadSysNumericFields;

	// Functions

	void
	SimHighTempRadiantSystem(
		std::string const & CompName, // name of the low temperature radiant system
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & LoadMet, // load met by the radiant system, in Watts
		int & CompIndex
	);

	void
	GetHighTempRadiantSystem();

	void
	InitHighTempRadiantSystem(
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		int const RadSysNum // Index for the low temperature radiant system under consideration within the derived types
	);

	void
	SizeHighTempRadiantSystem( int const RadSysNum );

	void
	CalcHighTempRadiantSystem( int const RadSysNum ); // name of the low temperature radiant system

	void
	CalcHighTempRadiantSystemSP(
		bool const FirstHVACIteration, // true if this is the first HVAC iteration at this system time step !unused1208
		int const RadSysNum // name of the low temperature radiant system
	);

	void
	UpdateHighTempRadiantSystem(
		int const RadSysNum, // Index for the low temperature radiant system under consideration within the derived types
		Real64 & LoadMet // load met by the radiant system, in Watts
	);

	void
	UpdateHTRadSourceValAvg( bool & HighTempRadSysOn ); // .TRUE. if the radiant system has run this zone time step

	void
	DistributeHTRadGains();

	void
	ReportHighTempRadiantSystem( int const RadSysNum ); // Index for the low temperature radiant system under consideration within the derived types

	Real64
	SumHATsurf( int const ZoneNum ); // Zone number

} // HighTempRadiantSystem

} // EnergyPlus

#endif
