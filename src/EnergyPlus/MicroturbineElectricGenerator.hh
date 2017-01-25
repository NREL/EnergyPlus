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

#ifndef MicroturbineElectricGenerator_hh_INCLUDED
#define MicroturbineElectricGenerator_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobalConstants.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace MicroturbineElectricGenerator {

	// Using/Aliasing
	using DataGlobalConstants::iGeneratorMicroturbine;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// na

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	extern int NumMTGenerators; // number of MT Generators specified in input
	extern bool GetMTInput; // then TRUE, calls subroutine to read input file.

	extern Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE MicroturbineElectricGenerator

	// Types

	struct MTGeneratorSpecs
	{
		// Members
		//      User inputs
		std::string Name; // User identifier (name)
		Real64 RefElecPowerOutput; // Reference Electrical Power Output from generator (W)
		Real64 MinElecPowerOutput; // Minimum Electrical Power Output (W)
		Real64 MaxElecPowerOutput; // Maximum Electrical Power Output (W)
		Real64 RefThermalPowerOutput; // Reference Electrical Power Output from generator (W)
		Real64 MinThermalPowerOutput; // Minimum Electrical Power Output (W)
		Real64 MaxThermalPowerOutput; // Maximum Electrical Power Output (W)
		Real64 RefElecEfficiencyLHV; // Reference Electrical Efficiency based on fuel LHV
		Real64 RefCombustAirInletTemp; // Reference Combustion Air Inlet Temperature (C)
		Real64 RefCombustAirInletHumRat; // Reference Combustion Air Inlet Humidity Ratio (kg/kg)
		Real64 RefElevation; // Reference Elevation (m)
		int ElecPowFTempElevCurveNum; // Curve index for Electrical Power as a function of temp and elev.
		int ElecEffFTempCurveNum; // Curve index for Electrical Efficiency function of temp
		int ElecEffFPLRCurveNum; // Curve index for Electrical Efficiency as a function of PLR
		Real64 FuelHigherHeatingValue; // Higher Heating Value for Fuel (kJ/kg)
		Real64 FuelLowerHeatingValue; // Lower Heating Value for Fuel (kJ/kg)
		Real64 StandbyPower; // Standby Power entered by user (W)
		Real64 AncillaryPower; // Ancillary Power entered by user (W)
		int AncillaryPowerFuelCurveNum; // Index to ancillary power modifer curve (function of fuel input)
		int HeatRecInletNodeNum; // Heat Recovery Water Inlet Node number
		int HeatRecOutletNodeNum; // Heat Recovery Water Outlet Node number
		Real64 RefThermalEffLHV; // Reference Thermal Efficiency (LHV Basis)
		Real64 RefInletWaterTemp; // Reference Inlet Water Temperature for heat recovery (C)
		bool InternalFlowControl; // A9, \field Heat Recovery Water Flow Operating Mode
		bool PlantFlowControl; // Default = Plant Control
		Real64 RefHeatRecVolFlowRate; // Reference Heat Recovery Water Flow Rate (m3/s)
		int HeatRecFlowFTempPowCurveNum; // Curve index for Heat Recovery Water Flow Rate function of temp & power
		int ThermEffFTempElevCurveNum; // Curve index for Thermal Efficiency function of temp & elevation
		int HeatRecRateFPLRCurveNum; // Curve index for Heat Recovery Rate function of part-load ratio
		int HeatRecRateFTempCurveNum; // Curve index for Heat Recovery Rate function of inlet water temp
		int HeatRecRateFWaterFlowCurveNum; // Curve index for Heat Recovery Rate function of water flow rate
		Real64 HeatRecMinVolFlowRate; // Minimum Heat Recovery Water volume Flow Rate (m3/s)
		Real64 HeatRecMaxVolFlowRate; // Maximum Heat Recovery Water volume Flow Rate (m3/s)
		Real64 HeatRecMaxWaterTemp; // Maximum Heat Recovery Water Temperature (C)
		int CombustionAirInletNodeNum; // Combustion Air Inlet Node number
		int CombustionAirOutletNodeNum; // Combustion Air Outlet (Exhaust) Node number
		bool ExhAirCalcsActive; // Flag to enable exhaust air calculations
		Real64 RefExhaustAirMassFlowRate; // Reference Exhaust Air Mass Flow Rate (kg/s)
		Real64 ExhaustAirMassFlowRate; // Actual Exhaust Air Mass Flow Rate (kg/s)
		int ExhFlowFTempCurveNum; // Curve index for Exhaust Air Flow Rate function of inlet air temp
		int ExhFlowFPLRCurveNum; // Curve index for Exhaust Air Flow Rate function of part-load ratio
		Real64 NomExhAirOutletTemp; // Nominal Exhaust Air Outlet Temperature (C)
		int ExhAirTempFTempCurveNum; // Curve index for Exhaust Air Temperature function of inlet air temp
		int ExhAirTempFPLRCurveNum; // Curve index for Exhaust Air Temperature function of part-load ratio
		Real64 ExhaustAirTemperature; // Combustion exhaust air temperature (C)
		Real64 ExhaustAirHumRat; // Combustion exhaust air humidity ratio (kg/kg)
		//      Other required variables/calculated values
		int CompType_Num;
		Real64 RefCombustAirInletDensity; // Reference combustion air inlet density (kg/m3)
		Real64 MinPartLoadRat; // Min allowed operating frac full load
		Real64 MaxPartLoadRat; // Max allowed operating frac full load
		Real64 FuelEnergyUseRateHHV; // Rate of Fuel Energy required to run microturbine, HHV basis (W)
		Real64 FuelEnergyUseRateLHV; // Rate of Fuel Energy required to run microturbine, LHV basis (W)
		Real64 QHeatRecovered; // Recovered exhaust energy rate to heat water  (W)
		Real64 ExhaustEnergyRec; // Recovered exhaust energy to heat water (J)
		Real64 DesignHeatRecMassFlowRate; // Design Water mass flow rate through heat recovery loop (kg/s)
		bool HeatRecActive; // TRUE when heat recovery water inlet and outlet nodes are defined
		Real64 HeatRecInletTemp; // Inlet Temperature of the heat recovery fluid (C)
		Real64 HeatRecOutletTemp; // Outlet Temperature of the heat recovery fluid (C)
		Real64 HeatRecMinMassFlowRate; // Minimum heat recovery water mass flow rate (kg/s)
		Real64 HeatRecMaxMassFlowRate; // Maximum heat recovery water mass flow rate (kg/s)
		Real64 HeatRecMdot; // Heat Recovery Loop Mass flow rate (kg/s)
		int HRLoopNum; // cooling water plant loop index number, for heat recovery
		int HRLoopSideNum; // cooling water plant loop side index, for heat recovery
		int HRBranchNum; // cooling water plant loop branch index, for heat recovery
		int HRCompNum; // cooling water plant loop component index, for heat recovery
		Real64 FuelMdot; // Fuel Amount used (kg/s)
		Real64 ElecPowerGenerated; // Electric power generated (W)
		Real64 StandbyPowerRate; // Standby power rate this time step (W)
		Real64 AncillaryPowerRate; // Ancillary power rate this time step (W)
		//     Warning message variables
		int PowerFTempElevErrorIndex; // Index to power as a function of temp/elevation warning message
		//       INTEGER    :: PowerFTempElevErrorCount     = 0   ! Counter for power as a function of temp/elevation warning messages
		int EffFTempErrorIndex; // Index to efficiency as a function of temperature warning message
		//       INTEGER    :: EffFTempErrorCount           = 0   ! Counter for efficiency as a function of temperature warning messages
		int EffFPLRErrorIndex; // Index to efficiency as a function of PLR warning message
		//       INTEGER    :: EffFPLRErrorCount            = 0   ! Counter for efficiency as a function of PLR warning messages
		int ExhFlowFTempErrorIndex; // Index to exhaust flow as a function of temp warning message
		//       INTEGER    :: ExhFlowFTempErrorCount       = 0   ! Counter for exhaust flow as a function of temp warning messages
		int ExhFlowFPLRErrorIndex; // Index to exhaust flow as a function of PLR warning message
		//       INTEGER    :: ExhFlowFPLRErrorCount        = 0   ! Counter for exhaust flow as a function of PLR warning messages
		int ExhTempFTempErrorIndex; // Index to exhaust temp as a function of temp warning message
		//       INTEGER    :: ExhTempFTempErrorCount       = 0   ! Counter for exhaust temp as a function of temp warning messages
		int ExhTempFPLRErrorIndex; // Index to exhaust temp as a function of PLR warning message
		//       INTEGER    :: ExhTempFPLRErrorCount        = 0   ! Counter for exhaust temp as a function of PLR warning messages
		int HRMinFlowErrorIndex; // Index to reclaim water flow rate warning message
		//       INTEGER    :: HRMinFlowErrorCount          = 0   ! Counter for reclaim water flow rate warning messages
		int HRMaxFlowErrorIndex; // Index to reclaim water flow rate warning message
		//       INTEGER    :: HRMaxFlowErrorCount          = 0   ! Counter for reclaim water flow rate warning messages
		int ExhTempLTInletTempIndex; // Index to exhaust temp < combustion inlet air temp warning messages
		//       INTEGER    :: ExhTempLTInletTempCount      = 0   ! Counter for exhaust temp < combustion inlet air temp warning messages
		int ExhHRLTInletHRIndex; // Index to exhaust hum rat < combustion inlet air hum rat warning messages
		//       INTEGER    :: ExhHRLTInletHRCount          = 0   ! Counter for exhaust hum rat < combustion inlet air hum rat warn messages
		int AnciPowerIterErrorIndex; // Index to Ancillary Power iteration loop warning messages
		//       INTEGER    :: AnciPowerIterErrorCount      = 0   ! Count for Ancillary Power iteration loop warning messages
		int AnciPowerFMdotFuelErrorIndex; // Index to Ancillary Power as a function of fuel input warning messages
		//       INTEGER    :: AnciPowerFMdotFuelErrorCount = 0   ! Count for Ancillary Power as a function of fuel input warning messages
		int HeatRecRateFPLRErrorIndex; // Index to heat recovery rate as a function of PLR warning messages
		//       INTEGER    :: HeatRecRateFPLRErrorCount    = 0   ! Count for heat recovery rate as a function of PLR warning messages
		int HeatRecRateFTempErrorIndex; // Index to heat recovery rate as a function of temp warning messages
		//       INTEGER    :: HeatRecRateFTempErrorCount   = 0   ! Count for heat recovery rate as a function of temp warning messages
		int HeatRecRateFFlowErrorIndex; // Index to heat recovery rate as a function of flow warning messages
		//       INTEGER    :: HeatRecRateFFlowErrorCount   = 0   ! Count for heat recovery rate as a function of flow warning messages
		int ThermEffFTempElevErrorIndex; // Index to thermal efficiency as a function of temp/elevation warnings
		//       INTEGER    :: ThermEffFTempElevErrorCount  = 0   ! Count for thermal efficiency as a function of temp/elevation warnings

		// Default Constructor
		MTGeneratorSpecs() :
			RefElecPowerOutput( 0.0 ),
			MinElecPowerOutput( 0.0 ),
			MaxElecPowerOutput( 0.0 ),
			RefThermalPowerOutput( 0.0 ),
			MinThermalPowerOutput( 0.0 ),
			MaxThermalPowerOutput( 0.0 ),
			RefElecEfficiencyLHV( 0.0 ),
			RefCombustAirInletTemp( 0.0 ),
			RefCombustAirInletHumRat( 0.0 ),
			RefElevation( 0.0 ),
			ElecPowFTempElevCurveNum( 0 ),
			ElecEffFTempCurveNum( 0 ),
			ElecEffFPLRCurveNum( 0 ),
			FuelHigherHeatingValue( 0.0 ),
			FuelLowerHeatingValue( 0.0 ),
			StandbyPower( 0.0 ),
			AncillaryPower( 0.0 ),
			AncillaryPowerFuelCurveNum( 0 ),
			HeatRecInletNodeNum( 0 ),
			HeatRecOutletNodeNum( 0 ),
			RefThermalEffLHV( 0.0 ),
			RefInletWaterTemp( 0.0 ),
			InternalFlowControl( false ),
			PlantFlowControl( true ),
			RefHeatRecVolFlowRate( 0.0 ),
			HeatRecFlowFTempPowCurveNum( 0 ),
			ThermEffFTempElevCurveNum( 0 ),
			HeatRecRateFPLRCurveNum( 0 ),
			HeatRecRateFTempCurveNum( 0 ),
			HeatRecRateFWaterFlowCurveNum( 0 ),
			HeatRecMinVolFlowRate( 0.0 ),
			HeatRecMaxVolFlowRate( 0.0 ),
			HeatRecMaxWaterTemp( 0.0 ),
			CombustionAirInletNodeNum( 0 ),
			CombustionAirOutletNodeNum( 0 ),
			ExhAirCalcsActive( false ),
			RefExhaustAirMassFlowRate( 0.0 ),
			ExhaustAirMassFlowRate( 0.0 ),
			ExhFlowFTempCurveNum( 0 ),
			ExhFlowFPLRCurveNum( 0 ),
			NomExhAirOutletTemp( 0.0 ),
			ExhAirTempFTempCurveNum( 0 ),
			ExhAirTempFPLRCurveNum( 0 ),
			ExhaustAirTemperature( 0.0 ),
			ExhaustAirHumRat( 0.0 ),
			CompType_Num( iGeneratorMicroturbine ),
			RefCombustAirInletDensity( 0.0 ),
			MinPartLoadRat( 0.0 ),
			MaxPartLoadRat( 0.0 ),
			FuelEnergyUseRateHHV( 0.0 ),
			FuelEnergyUseRateLHV( 0.0 ),
			QHeatRecovered( 0.0 ),
			ExhaustEnergyRec( 0.0 ),
			DesignHeatRecMassFlowRate( 0.0 ),
			HeatRecActive( false ),
			HeatRecInletTemp( 0.0 ),
			HeatRecOutletTemp( 0.0 ),
			HeatRecMinMassFlowRate( 0.0 ),
			HeatRecMaxMassFlowRate( 0.0 ),
			HeatRecMdot( 0.0 ),
			HRLoopNum( 0 ),
			HRLoopSideNum( 0 ),
			HRBranchNum( 0 ),
			HRCompNum( 0 ),
			FuelMdot( 0.0 ),
			ElecPowerGenerated( 0.0 ),
			StandbyPowerRate( 0.0 ),
			AncillaryPowerRate( 0.0 ),
			PowerFTempElevErrorIndex( 0 ),
			EffFTempErrorIndex( 0 ),
			EffFPLRErrorIndex( 0 ),
			ExhFlowFTempErrorIndex( 0 ),
			ExhFlowFPLRErrorIndex( 0 ),
			ExhTempFTempErrorIndex( 0 ),
			ExhTempFPLRErrorIndex( 0 ),
			HRMinFlowErrorIndex( 0 ),
			HRMaxFlowErrorIndex( 0 ),
			ExhTempLTInletTempIndex( 0 ),
			ExhHRLTInletHRIndex( 0 ),
			AnciPowerIterErrorIndex( 0 ),
			AnciPowerFMdotFuelErrorIndex( 0 ),
			HeatRecRateFPLRErrorIndex( 0 ),
			HeatRecRateFTempErrorIndex( 0 ),
			HeatRecRateFFlowErrorIndex( 0 ),
			ThermEffFTempElevErrorIndex( 0 )
		{}

	};

	struct ReportVars
	{
		// Members
		Real64 PowerGen; // Reporting: Electric power produced (W)
		Real64 EnergyGen; // Reporting: Electric energy produced (J)
		Real64 QHeatRecovered; // Reporting: Heat recovered from exhaust to heat water (W)
		Real64 ExhaustEnergyRec; // Reporting: Heat recovered from exhaust to heat water (J)
		Real64 FuelEnergyUseRateHHV; // Reporting: Fuel Energy use rate, HHV basis (W)
		Real64 FuelEnergyHHV; // Reporting: Fuel Energy used (J)
		Real64 FuelMdot; // Reporting: Fuel Amount used (kg/s)
		Real64 ElectricEfficiencyLHV; // Reporting: Electric efficiency LHV (-)
		Real64 ThermalEfficiencyLHV; // Reporting: Thermal (heat recovery to water) efficiency LHV (-)
		Real64 HeatRecInletTemp; // Reporting: Heat Recovery Loop Inlet Temperature (C)
		Real64 HeatRecOutletTemp; // Reporting: Heat Recovery Loop Outlet Temperature (C)
		Real64 HeatRecMdot; // Reporting: Heat Recovery Loop Mass flow rate (kg/s)
		Real64 AncillaryPowerRate; // Reporting: Ancillary power use rate (W)
		Real64 AncillaryEnergy; // Reporting: Ancillary energy use (J)
		Real64 StandbyPowerRate; // Reporting: Standby power use rate (W)
		Real64 StandbyEnergy; // Reporting: Standby energy use (J)
		Real64 ExhAirMassFlowRate; // Actual Exhaust Air Mass Flow Rate (kg/s)
		Real64 ExhAirTemperature; // Combustion exhaust air temperature (C)

		// Default Constructor
		ReportVars() :
			PowerGen( 0.0 ),
			EnergyGen( 0.0 ),
			QHeatRecovered( 0.0 ),
			ExhaustEnergyRec( 0.0 ),
			FuelEnergyUseRateHHV( 0.0 ),
			FuelEnergyHHV( 0.0 ),
			FuelMdot( 0.0 ),
			ElectricEfficiencyLHV( 0.0 ),
			ThermalEfficiencyLHV( 0.0 ),
			HeatRecInletTemp( 0.0 ),
			HeatRecOutletTemp( 0.0 ),
			HeatRecMdot( 0.0 ),
			AncillaryPowerRate( 0.0 ),
			AncillaryEnergy( 0.0 ),
			StandbyPowerRate( 0.0 ),
			StandbyEnergy( 0.0 ),
			ExhAirMassFlowRate( 0.0 ),
			ExhAirTemperature( 0.0 )
		{}

	};

	// Object Data
	extern Array1D< MTGeneratorSpecs > MTGenerator; // dimension to number of generators
	extern Array1D< ReportVars > MTGeneratorReport;

	// Functions

	void
	SimMTGenerator(
		int const GeneratorType, // Type of generator !unused1208
		std::string const & GeneratorName, // User-specified name of generator
		int & GeneratorIndex, // Index to microturbine generator
		bool const RunFlag, // Simulate generator when TRUE
		Real64 const MyLoad, // Generator demand (W)
		bool const FirstHVACIteration // Simulation flag for First HVAC (system) iteration
	);

	void
	SimMTPlantHeatRecovery(
		std::string const & CompType, // unused1208
		std::string const & CompName,
		int const CompTypeNum, // unused1208
		int & CompNum,
		bool const RunFlag, // unused1208
		bool & InitLoopEquip,
		Real64 & MyLoad, // unused1208
		Real64 & MaxCap,
		Real64 & MinCap,
		Real64 & OptCap,
		bool const FirstHVACIteration // TRUE if First iteration of simulation !unused1208
	);

	// End MT Generator Module Driver Subroutine
	//******************************************************************************

	// Beginning of Microturbine (MT) Generator Module Get Input Subroutine
	//******************************************************************************

	void
	GetMTGeneratorInput();

	// End of Get Input subroutine for the MT Generator Module
	//******************************************************************************

	// Begin MT Generator Module Initialize Subroutine
	// *****************************************************************************

	void
	InitMTGenerators(
		int const GenNum,
		bool const RunFlag,
		Real64 const MyLoad, // electrical load in W
		bool const FirstHVACIteration
	);

	//  End of MT Generator Module Initialize Subroutine
	// *****************************************************************************

	//  Beginning of MT Generator Model Calculation Subroutine
	// *****************************************************************************

	void
	CalcMTGeneratorModel(
		int const GeneratorNum, // Generator number
		bool const RunFlag, // TRUE when generator is being asked to operate
		Real64 const MyLoad, // Generator demand (W)
		bool const FirstHVACIteration // unused1208
	);

	//  End of MT Generator Model Calculation Subroutine
	// *****************************************************************************

	//  Beginning of record keeping subroutine for the MT Generator Module
	// *****************************************************************************

	void
	UpdateMTGeneratorRecords( int const Num ); // Generator number

	void
	GetMTGeneratorResults(
		int const GeneratorType, // type of Generator !unused1208
		int const GeneratorIndex,
		Real64 & GeneratorPower, // electrical power
		Real64 & GeneratorEnergy, // electrical energy
		Real64 & ThermalPower, // heat power
		Real64 & ThermalEnergy // heat energy
	);

	void
	GetMTGeneratorExhaustNode(
		int const CompType,
		std::string const & CompName,
		int & ExhaustOutletNodeNum
	);

	// End of Record Keeping subroutine for the MT Generator Module
	// *****************************************************************************

} // MicroturbineElectricGenerator

} // EnergyPlus

#endif
