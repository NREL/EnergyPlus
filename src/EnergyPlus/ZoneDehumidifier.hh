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

#ifndef ZoneDehumidifier_hh_INCLUDED
#define ZoneDehumidifier_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace ZoneDehumidifier {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:

	// Unit type index
	extern int const ZoneDehumidUnit; // 1 is the index for ZoneHVAC:Dehumidifier:DX

	// Water Systems
	extern int const CondensateDiscarded; // Default mode where water is "lost"
	extern int const CondensateToTank; // Collect coil condensate from air and store in water storage tank

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:

	extern int NumDehumidifiers; // Number of zone dehumidifier objects in the input file

	extern bool GetInputFlag; // Set to FALSE after first time input is "gotten"
	extern Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE:
	// Driver/Manager Routines

	// Get Input routines for module

	// Initialization routines for module

	// Algorithms/Calculation routines for the module

	// Update routine to update node information

	// Reporting routines for module

	// Get either inlet or outlet node number

	// Types

	struct ZoneDehumidifierData
	{
		// Members
		// input data and others required during calculations
		std::string Name; // Name of unit
		std::string UnitType; // Type of unit
		int UnitType_Num; // Parameter equivalent to type of unit
		int SchedPtr; // Index number to availability schedule
		Real64 RatedWaterRemoval; // Rated water removal [liters/day]
		Real64 RatedEnergyFactor; // Rated energy factor [liters/kWh]
		Real64 RatedAirVolFlow; // Rated air flow rate through the dehumidifier [m3/s]
		Real64 RatedAirMassFlow; // Rated air mass flow rate through the dehumidifier [kg/s]
		Real64 MinInletAirTemp; // Minimum dry-bulb temperature for dehumidifier operation [C]
		Real64 MaxInletAirTemp; // Maximum dry-bulb temperature for dehumidifier operation [C]
		Real64 InletAirMassFlow; // Inlet air mass flow rate for the time step being simulated [kg/s]
		Real64 OutletAirEnthalpy; // Dehumidifier outlet air enthalpy [J/kg]
		Real64 OutletAirHumRat; // Dehumidifier outlet air humidity ratio [kg/kg]
		Real64 OffCycleParasiticLoad; // Off Cycle Parasitic Load, user input [W]
		int AirInletNodeNum; // Inlet air node number
		int AirOutletNodeNum; // Outlet air node number
		int WaterRemovalCurveIndex; // Index for water removal curve
		int WaterRemovalCurveType; // Water removal curve type. 2 = biquadratic
		int WaterRemovalCurveErrorCount; // Count number of times water removal curve returns a negative value
		int WaterRemovalCurveErrorIndex; // Index for negative value water removal factor recurring messages
		int EnergyFactorCurveIndex; // Index for energy factor curve
		int EnergyFactorCurveType; // Energy factor curve type. 2 = biquadratic
		int EnergyFactorCurveErrorCount; // Count number of times energy factor curve returns negative value
		int EnergyFactorCurveErrorIndex; // Index for negative value energy factor recurring messages
		int PartLoadCurveIndex; // Index for part load curve
		int PartLoadCurveType; // Part load curve type. 1 = quadratic, cubic = 3
		int LowPLFErrorCount; // Count number of times PLF < 0.7
		int LowPLFErrorIndex; // Index for PLF < 0.7 recurring warning messages
		int HighPLFErrorCount; // Count number of times PLF > 1.0
		int HighPLFErrorIndex; // Index for PLF > 1.0 recurring warning messages
		int HighRTFErrorCount; // Count number of times RTF > 1.0
		int HighRTFErrorIndex; // Index for RTF > 1.0 recurring warning messages
		int PLFPLRErrorCount; // Count number of times PLF < PLR
		int PLFPLRErrorIndex; // Index for PLF < PLR recurring warning messages
		int CondensateCollectMode; // Where does water come from
		std::string CondensateCollectName; // Name of water storage (collection) tank
		int CondensateTankID; // Condensate collection tank ID number
		int CondensateTankSupplyARRID; // Condensate collection tank supply ID number
		// Report data
		Real64 SensHeatingRate; // Zone Dehumidifier Sensible Heating Rate [W]
		Real64 SensHeatingEnergy; // Zone Dehumidifier Sensible Heating Energy [J]
		Real64 WaterRemovalRate; // Zone Dehumidifier Water Removal Rate [kg/s]
		Real64 WaterRemoved; // Zone Dehumidifier Water Removed [kg]
		Real64 ElecPower; // Zone Dehumidifier Electric Power [W]
		Real64 ElecConsumption; // Zone Dehumidifier Electric Consumption [J]
		Real64 DehumidPLR; // Zone Dehumidifier Part-Load Ratio [-]
		Real64 DehumidRTF; // Zone Dehumidifier Runtime Fraction [-]
		Real64 DehumidCondVolFlowRate; // Zone Dehumidifier Condensate Volumetric Flow Rate [m3/s]
		Real64 DehumidCondVol; // Zone Dehumidifier Condensate Volume [m3]
		Real64 OutletAirTemp; // Zone Dehumidifier Outlet Air Temperature [C]
		Real64 OffCycleParasiticElecPower; // Zone Dehumidifier Off-Cycle Parasitic Electric Power [W]
		Real64 OffCycleParasiticElecCons; // Zone Dehumidifier Off-Cycle Parasitic Electric Consumption [J]

		// Default Constructor
		ZoneDehumidifierData() :
			UnitType_Num( 0 ),
			SchedPtr( 0 ),
			RatedWaterRemoval( 0.0 ),
			RatedEnergyFactor( 0.0 ),
			RatedAirVolFlow( 0.0 ),
			RatedAirMassFlow( 0.0 ),
			MinInletAirTemp( 0.0 ),
			MaxInletAirTemp( 0.0 ),
			InletAirMassFlow( 0.0 ),
			OutletAirEnthalpy( 0.0 ),
			OutletAirHumRat( 0.0 ),
			OffCycleParasiticLoad( 0.0 ),
			AirInletNodeNum( 0 ),
			AirOutletNodeNum( 0 ),
			WaterRemovalCurveIndex( 0 ),
			WaterRemovalCurveType( 0 ),
			WaterRemovalCurveErrorCount( 0 ),
			WaterRemovalCurveErrorIndex( 0 ),
			EnergyFactorCurveIndex( 0 ),
			EnergyFactorCurveType( 0 ),
			EnergyFactorCurveErrorCount( 0 ),
			EnergyFactorCurveErrorIndex( 0 ),
			PartLoadCurveIndex( 0 ),
			PartLoadCurveType( 0 ),
			LowPLFErrorCount( 0 ),
			LowPLFErrorIndex( 0 ),
			HighPLFErrorCount( 0 ),
			HighPLFErrorIndex( 0 ),
			HighRTFErrorCount( 0 ),
			HighRTFErrorIndex( 0 ),
			PLFPLRErrorCount( 0 ),
			PLFPLRErrorIndex( 0 ),
			CondensateCollectMode( CondensateDiscarded ),
			CondensateTankID( 0 ),
			CondensateTankSupplyARRID( 0 ),
			SensHeatingRate( 0.0 ),
			SensHeatingEnergy( 0.0 ),
			WaterRemovalRate( 0.0 ),
			WaterRemoved( 0.0 ),
			ElecPower( 0.0 ),
			ElecConsumption( 0.0 ),
			DehumidPLR( 0.0 ),
			DehumidRTF( 0.0 ),
			DehumidCondVolFlowRate( 0.0 ),
			DehumidCondVol( 0.0 ),
			OutletAirTemp( 0.0 ),
			OffCycleParasiticElecPower( 0.0 ),
			OffCycleParasiticElecCons( 0.0 )
		{}

	};

	// Object Data
	extern Array1D< ZoneDehumidifierData > ZoneDehumid;

	// Functions

	void
	clear_state();

	void
	SimZoneDehumidifier(
		std::string const & CompName, // Name of the zone dehumidifier
		int const ZoneNum, // Number of zone being served
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & QSensOut, // Sensible capacity delivered to zone (W)
		Real64 & QLatOut, // Latent capacity delivered to zone (kg/s), dehumidify = negative
		int & CompIndex // Index to the zone dehumidifier
	);

	void
	GetZoneDehumidifierInput();

	void
	InitZoneDehumidifier( int const ZoneDehumNum ); // Number of the current zone dehumidifier being simulated

	void
	SizeZoneDehumidifier();

	void
	CalcZoneDehumidifier(
		int const ZoneDehumNum, // Index number of the current zone dehumidifier being simulated
		Real64 const QZnDehumidReq, // Dehumidification load to be met (kg/s), negative value means dehumidification load
		Real64 & SensibleOutput, // Sensible (heating) output (W), sent to load predictor for next simulation time step
		Real64 & LatentOutput // Latent (dehumidification) output provided (kg/s)
	);

	void
	UpdateZoneDehumidifier( int const ZoneDehumNum ); // Number of the current zone dehumidifier being simulated

	void
	ReportZoneDehumidifier( int const DehumidNum ); // Index of the current zone dehumidifier being simulated

	bool
	GetZoneDehumidifierNodeNumber( int const NodeNumber ); // Node being tested

} // ZoneDehumidifier

} // EnergyPlus

#endif
