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

// C++ Headers
#include <cassert>
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>

// EnergyPlus Headers
#include <HighTempRadiantSystem.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHeatBalSurface.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DataSurfaces.hh>
#include <DataZoneEnergyDemands.hh>
#include <DataZoneEquipment.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <HeatBalanceSurfaceManager.hh>
#include <InputProcessor.hh>
#include <OutputProcessor.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace HighTempRadiantSystem {

	// Module containing the routines dealing with the high temperature radiant systems

	// MODULE INFORMATION:
	//       AUTHOR         Rick Strand
	//       DATE WRITTEN   February 2001
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// The purpose of this module is to simulate high temperature radiant systems.
	// It is the intention of this module to cover all types of high temperature
	// radiant systems (gas and electric)

	// METHODOLOGY EMPLOYED:
	// Based on work done in BLAST, the EnergyPlus low temperature radiant system
	// model, this model has similar inherent challenges that are similar to the
	// low temperature radiant system.  Because it is a system that directly
	// effects the surface heat balances, it must be a part of both the heat
	// balance routines and linked in with the HVAC system.
	// REFERENCES:
	// Building Systems Laboratory, BLAST User's Guide/Reference.
	// Maloney, Dan. 1987. "Development of a radiant heater model and the
	//   incorporation of thermal comfort considerations into the BLAST
	//   energy analysis program", M.S. thesis, University of Illinois at
	//   Urbana-Champaign (Dept. of Mechanical and Industrial Engineering).

	// OTHER NOTES: none

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::BeginTimeStepFlag;
	using DataGlobals::SysSizingCalc;
	using DataGlobals::ScheduleAlwaysOn;
	using DataGlobals::DisplayExtraWarnings;
	using DataHVACGlobals::SmallLoad;

	// Use statements for access to subroutines in other modules

	// Data
	// MODULE PARAMETER DEFINITIONS:
	std::string const cGas( "Gas" );
	std::string const cNaturalGas( "NaturalGas" );
	std::string const cElectric( "Electric" );
	std::string const cElectricity( "Electricity" );
	int const Gas( 1 );
	int const Electric( 2 );
	std::string const cMATControl( "MeanAirTemperature" ); // Control for using mean air temperature
	std::string const cMRTControl( "MeanRadiantTemperature" ); // Control for using mean radiant temperature
	std::string const cOperativeControl( "OperativeTemperature" ); // Control for using operative temperature
	std::string const cMATSPControl( "MeanAirTemperatureSetpoint" ); // Control for to MAT setpoint
	std::string const cMRTSPControl( "MeanRadiantTemperatureSetpoint" ); // Control for to MRT setpoint
	std::string const cOperativeSPControl( "OperativeTemperatureSetpoint" ); // Control for operative temperature setpoint
	int const MATControl( 1001 );
	int const MRTControl( 1002 );
	int const OperativeControl( 1003 );
	int const MATSPControl( 1004 );
	int const MRTSPControl( 1005 );
	int const OperativeSPControl( 1006 );

	static std::string const BlankString;

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	// Standard, run-of-the-mill variables...
	int NumOfHighTempRadSys( 0 ); // Number of hydronic low tempererature radiant systems
	Array1D< Real64 > QHTRadSource; // Need to keep the last value in case we are still iterating
	Array1D< Real64 > QHTRadSrcAvg; // Need to keep the last value in case we are still iterating
	Array1D< Real64 > ZeroSourceSumHATsurf; // Equal to the SumHATsurf for all the walls in a zone with no source
	// Record keeping variables used to calculate QHTRadSrcAvg locally
	Array1D< Real64 > LastQHTRadSrc; // Need to keep the last value in case we are still iterating
	Array1D< Real64 > LastSysTimeElapsed; // Need to keep the last value in case we are still iterating
	Array1D< Real64 > LastTimeStepSys; // Need to keep the last value in case we are still iterating
	Array1D_bool MySizeFlag;
	Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE HighTempRadiantSystem

	// Object Data
	Array1D< HighTempRadiantSystemData > HighTempRadSys;
	Array1D< HighTempRadSysNumericFieldData > HighTempRadSysNumericFields;

	// Functions

	void
	SimHighTempRadiantSystem(
		std::string const & CompName, // name of the low temperature radiant system
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & LoadMet, // load met by the radiant system, in Watts
		int & CompIndex
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   February 2001
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is the "manager" for the high temperature radiant
		// system model.  It is called from the outside and controls the
		// actions and subroutine calls to lower levels as appropriate.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus manager subroutine layout

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using General::TrimSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool GetInputFlag( true ); // First time, input is "gotten"
		int RadSysNum; // Radiant system number/index in local derived types

		// FLOW:
		if ( GetInputFlag ) {
			GetHighTempRadiantSystem();
			GetInputFlag = false;
		}

		// Find the correct ZoneHVAC:HighTemperatureRadiant
		if ( CompIndex == 0 ) {
			RadSysNum = FindItemInList( CompName, HighTempRadSys );
			if ( RadSysNum == 0 ) {
				ShowFatalError( "SimHighTempRadiantSystem: Unit not found=" + CompName );
			}
			CompIndex = RadSysNum;
		} else {
			RadSysNum = CompIndex;
			if ( RadSysNum > NumOfHighTempRadSys || RadSysNum < 1 ) {
				ShowFatalError( "SimHighTempRadiantSystem:  Invalid CompIndex passed=" + TrimSigDigits( RadSysNum ) + ", Number of Units=" + TrimSigDigits( NumOfHighTempRadSys ) + ", Entered Unit name=" + CompName );
			}
			if ( CheckEquipName( RadSysNum ) ) {
				if ( CompName != HighTempRadSys( RadSysNum ).Name ) {
					ShowFatalError( "SimHighTempRadiantSystem: Invalid CompIndex passed=" + TrimSigDigits( RadSysNum ) + ", Unit name=" + CompName + ", stored Unit Name for that index=" + HighTempRadSys( RadSysNum ).Name );
				}
				CheckEquipName( RadSysNum ) = false;
			}
		}

		InitHighTempRadiantSystem( FirstHVACIteration, RadSysNum );

		{ auto const SELECT_CASE_var( HighTempRadSys( RadSysNum ).ControlType );
		if ( ( SELECT_CASE_var == MATControl ) || ( SELECT_CASE_var == MRTControl ) || ( SELECT_CASE_var == OperativeControl ) ) {
			CalcHighTempRadiantSystem( RadSysNum );
		} else if ( ( SELECT_CASE_var == MATSPControl ) || ( SELECT_CASE_var == MRTSPControl ) || ( SELECT_CASE_var == OperativeSPControl ) ) {
			CalcHighTempRadiantSystemSP( FirstHVACIteration, RadSysNum );
		}}

		UpdateHighTempRadiantSystem( RadSysNum, LoadMet );

		ReportHighTempRadiantSystem( RadSysNum );

	}

	void
	GetHighTempRadiantSystem()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   February 2001
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine reads the input for high temperature radiant systems
		// from the user input file.  This will contain all of the information
		// needed to simulate a high temperature radiant system.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHeatBalance::Zone;
		using DataSurfaces::Surface;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using InputProcessor::SameString;
		using InputProcessor::VerifyName;
		using InputProcessor::GetObjectDefMaxArgs;
		using ScheduleManager::GetScheduleIndex;
		using General::TrimSigDigits;
		using DataSizing::AutoSize;
		using DataSizing::HeatingDesignCapacity;
		using DataSizing::CapacityPerFloorArea;
		using DataSizing::FractionOfAutosizedHeatingCapacity;
		using namespace DataIPShortCuts;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const MaxCombustionEffic( 1.00 ); // Limit the combustion efficiency to perfection
		Real64 const MaxFraction( 1.0 ); // Limit the highest allowed fraction for heat transfer parts
		Real64 const MinCombustionEffic( 0.01 ); // Limit the minimum combustion efficiency
		Real64 const MinFraction( 0.0 ); // Limit the lowest allowed fraction for heat transfer parts
		Real64 const MinThrottlingRange( 0.5 ); // Smallest throttling range allowed in degrees Celsius
		//  INTEGER,          PARAMETER :: MaxDistribSurfaces = 20    ! Maximum number of surfaces that a radiant heater can radiate to
		static std::string const RoutineName( "GetHighTempRadiantSystem: " ); // include trailing blank space
		int const iHeatCAPMAlphaNum( 4 ); // get input index to High Temperature Radiant system heating capacity sizing method
		int const iHeatDesignCapacityNumericNum( 1 ); // get input index to High Temperature Radiant system heating capacity
		int const iHeatCapacityPerFloorAreaNumericNum( 2 ); // get input index to High Temperature Radiant system heating capacity per floor area sizing
		int const iHeatFracOfAutosizedCapacityNumericNum( 3 ); //  get input index to High Temperature Radiant system heating capacity sizing as fraction of autozized heating capacity

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 AllFracsSummed; // Sum of the fractions radiant, latent, and lost (must be <= 1)
		static bool ErrorsFound( false ); // Set to true if errors in input, fatal at end of routine
		int IOStatus; // Used in GetObjectItem
		int Item; // Item to be "gotten"
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		int SurfNum; // Surface number DO loop counter
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name

		// FLOW:
		// Initializations and allocations
		NumOfHighTempRadSys = GetNumObjectsFound( "ZoneHVAC:HighTemperatureRadiant" );

		HighTempRadSys.allocate( NumOfHighTempRadSys );
		CheckEquipName.allocate( NumOfHighTempRadSys );
		HighTempRadSysNumericFields.allocate( NumOfHighTempRadSys );
		CheckEquipName = true;

		// extensible object, do not need max args because using IPShortCuts

		cCurrentModuleObject = "ZoneHVAC:HighTemperatureRadiant";
		// Obtain all of the user data related to high temperature radiant systems...
		for ( Item = 1; Item <= NumOfHighTempRadSys; ++Item ) {

			GetObjectItem( cCurrentModuleObject, Item, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			HighTempRadSysNumericFields( Item ).FieldNames.allocate( NumNumbers );
			HighTempRadSysNumericFields( Item ).FieldNames = "";
			HighTempRadSysNumericFields( Item ).FieldNames = cNumericFieldNames;

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), HighTempRadSys, Item - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			// General user input data
			HighTempRadSys( Item ).Name = cAlphaArgs( 1 );

			HighTempRadSys( Item ).SchedName = cAlphaArgs( 2 );
			if ( lAlphaFieldBlanks( 2 ) ) {
				HighTempRadSys( Item ).SchedPtr = ScheduleAlwaysOn;
			} else {
				HighTempRadSys( Item ).SchedPtr = GetScheduleIndex( cAlphaArgs( 2 ) );
				if ( HighTempRadSys( Item ).SchedPtr == 0 ) {
					ShowSevereError( cCurrentModuleObject + ": invalid " + cAlphaFieldNames( 2 ) + " entered =" + cAlphaArgs( 2 ) + " for " + cAlphaFieldNames( 1 ) + " = " + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}
			}

			HighTempRadSys( Item ).ZoneName = cAlphaArgs( 3 );
			HighTempRadSys( Item ).ZonePtr = FindItemInList( cAlphaArgs( 3 ), Zone );
			if ( HighTempRadSys( Item ).ZonePtr == 0 ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 3 ) + " = " + cAlphaArgs( 3 ) );
				ShowContinueError( "Occurs for " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}

			//HighTempRadSys( Item ).MaxPowerCapac = rNumericArgs( 1 );


			// Determine High Temp Radiant heating design capacity sizing method
			if ( SameString( cAlphaArgs( iHeatCAPMAlphaNum ), "HeatingDesignCapacity" ) ) {
				HighTempRadSys( Item ).HeatingCapMethod = HeatingDesignCapacity;

				if ( !lNumericFieldBlanks( iHeatDesignCapacityNumericNum ) ) {
					HighTempRadSys( Item ).ScaledHeatingCapacity = rNumericArgs( iHeatDesignCapacityNumericNum );
					if ( HighTempRadSys( Item ).ScaledHeatingCapacity < 0.0 && HighTempRadSys( Item ).ScaledHeatingCapacity != AutoSize ) {
						ShowSevereError( cCurrentModuleObject + " = " + HighTempRadSys( Item ).Name );
						ShowContinueError( "Illegal " + cNumericFieldNames( iHeatDesignCapacityNumericNum ) + " = " + TrimSigDigits( rNumericArgs( iHeatDesignCapacityNumericNum ), 7 ) );
						ErrorsFound = true;
					}
				} else {
					ShowSevereError( cCurrentModuleObject + " = " + HighTempRadSys( Item ).Name );
					ShowContinueError( "Input for " + cAlphaFieldNames( iHeatCAPMAlphaNum ) + " = " + cAlphaArgs( iHeatCAPMAlphaNum ) );
					ShowContinueError( "Blank field not allowed for " + cNumericFieldNames( iHeatDesignCapacityNumericNum ) );
					ErrorsFound = true;
				}
			} else if ( SameString( cAlphaArgs( iHeatCAPMAlphaNum ), "CapacityPerFloorArea" ) ) {
				HighTempRadSys( Item ).HeatingCapMethod = CapacityPerFloorArea;
				if ( !lNumericFieldBlanks( iHeatCapacityPerFloorAreaNumericNum ) ) {
					HighTempRadSys( Item ).ScaledHeatingCapacity = rNumericArgs( iHeatCapacityPerFloorAreaNumericNum );
					if ( HighTempRadSys( Item ).ScaledHeatingCapacity <= 0.0 ) {
						ShowSevereError( cCurrentModuleObject + " = " + HighTempRadSys( Item ).Name );
						ShowContinueError( "Input for " + cAlphaFieldNames( iHeatCAPMAlphaNum ) + " = " + cAlphaArgs( iHeatCAPMAlphaNum ) );
						ShowContinueError( "Illegal " + cNumericFieldNames( iHeatCapacityPerFloorAreaNumericNum ) + " = " + TrimSigDigits( rNumericArgs( iHeatCapacityPerFloorAreaNumericNum ), 7 ) );
						ErrorsFound = true;
					} else if ( HighTempRadSys( Item ).ScaledHeatingCapacity == AutoSize ) {
						ShowSevereError( cCurrentModuleObject + " = " + HighTempRadSys(Item).Name);
						ShowContinueError( "Input for " + cAlphaFieldNames( iHeatCAPMAlphaNum ) + " = " + cAlphaArgs( iHeatCAPMAlphaNum ) );
						ShowContinueError( "Illegal " + cNumericFieldNames( iHeatCapacityPerFloorAreaNumericNum ) + " = Autosize" );
						ErrorsFound = true;
					}
				} else {
					ShowSevereError( cCurrentModuleObject + " = " + HighTempRadSys( Item ).Name );
					ShowContinueError( "Input for " + cAlphaFieldNames( iHeatCAPMAlphaNum ) + " = " + cAlphaArgs( iHeatCAPMAlphaNum ) );
					ShowContinueError( "Blank field not allowed for " + cNumericFieldNames( iHeatCapacityPerFloorAreaNumericNum ) );
					ErrorsFound = true;
				}
			} else if ( SameString( cAlphaArgs( iHeatCAPMAlphaNum ), "FractionOfAutosizedHeatingCapacity" ) ) {
				HighTempRadSys( Item ).HeatingCapMethod = FractionOfAutosizedHeatingCapacity;
				if ( !lNumericFieldBlanks( iHeatFracOfAutosizedCapacityNumericNum ) ) {
					HighTempRadSys( Item ).ScaledHeatingCapacity = rNumericArgs( iHeatFracOfAutosizedCapacityNumericNum );
					if ( HighTempRadSys( Item ).ScaledHeatingCapacity < 0.0 ) {
						ShowSevereError( cCurrentModuleObject + " = " + HighTempRadSys( Item ).Name );
						ShowContinueError( "Illegal " + cNumericFieldNames( iHeatFracOfAutosizedCapacityNumericNum ) + " = " + TrimSigDigits( rNumericArgs( iHeatFracOfAutosizedCapacityNumericNum ), 7 ) );
						ErrorsFound = true;
					}
				} else {
					ShowSevereError( cCurrentModuleObject + " = " + HighTempRadSys( Item ).Name );
					ShowContinueError( "Input for " + cAlphaFieldNames( iHeatCAPMAlphaNum ) + " = " + cAlphaArgs( iHeatCAPMAlphaNum ) );
					ShowContinueError( "Blank field not allowed for " + cNumericFieldNames( iHeatFracOfAutosizedCapacityNumericNum ) );
					ErrorsFound = true;
				}
			} else {
				ShowSevereError( cCurrentModuleObject + " = " + HighTempRadSys( Item ).Name );
				ShowContinueError( "Illegal " + cAlphaFieldNames( iHeatCAPMAlphaNum ) + " = " + cAlphaArgs( iHeatCAPMAlphaNum ) );
				ErrorsFound = true;
			}

			if ( SameString( cAlphaArgs( 5 ), cNaturalGas ) ) {
				HighTempRadSys( Item ).HeaterType = Gas;
			} else if ( SameString( cAlphaArgs( 5 ), cElectricity ) ) {
				HighTempRadSys( Item ).HeaterType = Electric;
			} else if ( SameString( cAlphaArgs( 5 ), cGas ) ) {
				HighTempRadSys( Item ).HeaterType = Gas;
			} else if ( SameString( cAlphaArgs( 5 ), cElectric ) ) {
				HighTempRadSys( Item ).HeaterType = Electric;
			} else {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 5 ) + " = " + cAlphaArgs( 5 ) );
				ShowContinueError( "Occurs for " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}

			if ( HighTempRadSys( Item ).HeaterType == Gas ) {
				HighTempRadSys( Item ).CombustionEffic = rNumericArgs( 4 );
				// Limit the combustion efficiency to between zero and one...
				if ( HighTempRadSys( Item ).CombustionEffic < MinCombustionEffic ) {
					HighTempRadSys( Item ).CombustionEffic = MinCombustionEffic;
					ShowWarningError( cNumericFieldNames( 4 ) + " was less than the allowable minimum, reset to minimum value." );
					ShowContinueError( "Occurs for " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
				}
				if ( HighTempRadSys( Item ).CombustionEffic > MaxCombustionEffic ) {
					HighTempRadSys( Item ).CombustionEffic = MaxCombustionEffic;
					ShowWarningError( cNumericFieldNames( 4 ) + " was greater than the allowable maximum, reset to maximum value." );
					ShowContinueError( "Occurs for " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
				}
			} else {
				HighTempRadSys( Item ).CombustionEffic = MaxCombustionEffic; // No inefficiency in the heater
			}

			HighTempRadSys( Item ).FracRadiant = rNumericArgs( 5 );
			if ( HighTempRadSys( Item ).FracRadiant < MinFraction ) {
				HighTempRadSys( Item ).FracRadiant = MinFraction;
				ShowWarningError( cNumericFieldNames( 5 ) + " was less than the allowable minimum, reset to minimum value." );
				ShowContinueError( "Occurs for " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
			}
			if ( HighTempRadSys( Item ).FracRadiant > MaxFraction ) {
				HighTempRadSys( Item ).FracRadiant = MaxFraction;
				ShowWarningError( cNumericFieldNames( 5 ) + " was greater than the allowable maximum, reset to maximum value." );
				ShowContinueError( "Occurs for " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
			}

			HighTempRadSys( Item ).FracLatent = rNumericArgs( 6 );
			if ( HighTempRadSys( Item ).FracLatent < MinFraction ) {
				HighTempRadSys( Item ).FracLatent = MinFraction;
				ShowWarningError( cNumericFieldNames( 6 ) + " was less than the allowable minimum, reset to minimum value." );
				ShowContinueError( "Occurs for " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
			}
			if ( HighTempRadSys( Item ).FracLatent > MaxFraction ) {
				HighTempRadSys( Item ).FracLatent = MaxFraction;
				ShowWarningError( cNumericFieldNames( 6 ) + " was greater than the allowable maximum, reset to maximum value." );
				ShowContinueError( "Occurs for " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
			}

			HighTempRadSys( Item ).FracLost = rNumericArgs( 7 );
			if ( HighTempRadSys( Item ).FracLost < MinFraction ) {
				HighTempRadSys( Item ).FracLost = MinFraction;
				ShowWarningError( cNumericFieldNames( 7 ) + " was less than the allowable minimum, reset to minimum value." );
				ShowContinueError( "Occurs for " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
			}
			if ( HighTempRadSys( Item ).FracLost > MaxFraction ) {
				HighTempRadSys( Item ).FracLost = MaxFraction;
				ShowWarningError( cNumericFieldNames( 7 ) + " was greater than the allowable maximum, reset to maximum value." );
				ShowContinueError( "Occurs for " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
			}

			// Based on the input for fractions radiant, latent, and lost, determine the fraction convective (remaining fraction)
			AllFracsSummed = HighTempRadSys( Item ).FracRadiant + HighTempRadSys( Item ).FracLatent + HighTempRadSys( Item ).FracLost;
			if ( AllFracsSummed > MaxFraction ) {
				ShowSevereError( "Fractions radiant, latent, and lost sum up to greater than 1 for" + cAlphaArgs( 1 ) );
				ShowContinueError( "Occurs for " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
				ErrorsFound = true;
				HighTempRadSys( Item ).FracConvect = 0.0;
			} else {
				HighTempRadSys( Item ).FracConvect = 1.0 - AllFracsSummed;
			}

			// Process the temperature control type
			if ( SameString( cAlphaArgs( 6 ), cMATControl ) ) {
				HighTempRadSys( Item ).ControlType = MATControl;
			} else if ( SameString( cAlphaArgs( 6 ), cMRTControl ) ) {
				HighTempRadSys( Item ).ControlType = MRTControl;
			} else if ( SameString( cAlphaArgs( 6 ), cOperativeControl ) ) {
				HighTempRadSys( Item ).ControlType = OperativeControl;
			} else if ( SameString( cAlphaArgs( 6 ), cMATSPControl ) ) {
				HighTempRadSys( Item ).ControlType = MATSPControl;
			} else if ( SameString( cAlphaArgs( 6 ), cMRTSPControl ) ) {
				HighTempRadSys( Item ).ControlType = MRTSPControl;
			} else if ( SameString( cAlphaArgs( 6 ), cOperativeSPControl ) ) {
				HighTempRadSys( Item ).ControlType = OperativeSPControl;
			} else {
				ShowWarningError( "Invalid " + cAlphaFieldNames( 6 ) + " = " + cAlphaArgs( 6 ) );
				ShowContinueError( "Occurs for " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
				ShowContinueError( "Control reset to OPERATIVE control for this " + cCurrentModuleObject );
				HighTempRadSys( Item ).ControlType = OperativeControl;
			}

			HighTempRadSys( Item ).ThrottlRange = rNumericArgs( 8 );
			if ( HighTempRadSys( Item ).ThrottlRange < MinThrottlingRange ) {
				HighTempRadSys( Item ).ThrottlRange = 1.0;
				ShowWarningError( cNumericFieldNames( 8 ) + " is below the minimum allowed." );
				ShowContinueError( "Occurs for " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
				ShowContinueError( "Thus, the throttling range value has been reset to 1.0" );
			}

			HighTempRadSys( Item ).SetptSched = cAlphaArgs( 7 );
			HighTempRadSys( Item ).SetptSchedPtr = GetScheduleIndex( cAlphaArgs( 7 ) );
			if ( ( HighTempRadSys( Item ).SetptSchedPtr == 0 ) && ( ! lAlphaFieldBlanks( 7 ) ) ) {
				ShowSevereError( cAlphaFieldNames( 7 ) + " not found: " + cAlphaArgs( 7 ) );
				ShowContinueError( "Occurs for " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}

			HighTempRadSys( Item ).FracDistribPerson = rNumericArgs( 9 );
			if ( HighTempRadSys( Item ).FracDistribPerson < MinFraction ) {
				HighTempRadSys( Item ).FracDistribPerson = MinFraction;
				ShowWarningError( cNumericFieldNames( 9 ) + " was less than the allowable minimum, reset to minimum value." );
				ShowContinueError( "Occurs for " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
			}
			if ( HighTempRadSys( Item ).FracDistribPerson > MaxFraction ) {
				HighTempRadSys( Item ).FracDistribPerson = MaxFraction;
				ShowWarningError( cNumericFieldNames( 9 ) + " was greater than the allowable maximum, reset to maximum value." );
				ShowContinueError( "Occurs for " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
			}

			HighTempRadSys( Item ).TotSurfToDistrib = NumNumbers - 9;
			//    IF (HighTempRadSys(Item)%TotSurfToDistrib > MaxDistribSurfaces) THEN
			//      CALL ShowSevereError('Trying to distribute radiant energy to too many surfaces for heater '//TRIM(cAlphaArgs(1)))
			//      CALL ShowContinueError('Occurs for '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)))
			//      ErrorsFound=.TRUE.
			//    END IF
			HighTempRadSys( Item ).SurfaceName.allocate( HighTempRadSys( Item ).TotSurfToDistrib );
			HighTempRadSys( Item ).SurfacePtr.allocate( HighTempRadSys( Item ).TotSurfToDistrib );
			HighTempRadSys( Item ).FracDistribToSurf.allocate( HighTempRadSys( Item ).TotSurfToDistrib );

			AllFracsSummed = HighTempRadSys( Item ).FracDistribPerson;
			for ( SurfNum = 1; SurfNum <= HighTempRadSys( Item ).TotSurfToDistrib; ++SurfNum ) {
				HighTempRadSys( Item ).SurfaceName( SurfNum ) = cAlphaArgs( SurfNum + 7 );
				HighTempRadSys( Item ).SurfacePtr( SurfNum ) = FindItemInList( cAlphaArgs( SurfNum + 7 ), Surface );
				HighTempRadSys( Item ).FracDistribToSurf( SurfNum ) = rNumericArgs( SurfNum + 9 );
				// Error trap for surfaces that do not exist or surfaces not in the zone the radiant heater is in
				if ( HighTempRadSys( Item ).SurfacePtr( SurfNum ) == 0 ) {
					ShowSevereError( RoutineName + "Invalid Surface name = " + HighTempRadSys( Item ).SurfaceName( SurfNum ) );
					ShowContinueError( "Occurs for " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				} else if ( Surface( HighTempRadSys( Item ).SurfacePtr( SurfNum ) ).Zone != HighTempRadSys( Item ).ZonePtr ) {
					ShowWarningError( "Surface referenced in ZoneHVAC:HighTemperatureRadiant not in same zone as Radiant System, surface=" + HighTempRadSys( Item ).SurfaceName( SurfNum ) );
					ShowContinueError( "Surface is in Zone=" + Zone( Surface( HighTempRadSys( Item ).SurfacePtr( SurfNum ) ).Zone ).Name + " ZoneHVAC:HighTemperatureRadiant in Zone=" + cAlphaArgs( 3 ) );
					ShowContinueError( "Occurs for " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
				}
				// Error trap for fractions that are out of range
				if ( HighTempRadSys( Item ).FracDistribToSurf( SurfNum ) < MinFraction ) {
					HighTempRadSys( Item ).FracDistribToSurf( SurfNum ) = MinFraction;
					ShowWarningError( cNumericFieldNames( SurfNum + 9 ) + " was less than the allowable minimum, reset to minimum value." );
					ShowContinueError( "Occurs for " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
				}
				if ( HighTempRadSys( Item ).FracDistribToSurf( SurfNum ) > MaxFraction ) {
					HighTempRadSys( Item ).FracDistribToSurf( SurfNum ) = MaxFraction;
					ShowWarningError( cNumericFieldNames( SurfNum + 9 ) + " was greater than the allowable maximum, reset to maximum value." );
					ShowContinueError( "Occurs for " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
				}

				if ( HighTempRadSys( Item ).SurfacePtr( SurfNum ) != 0 ) {
					Surface( HighTempRadSys( Item ).SurfacePtr( SurfNum ) ).IntConvSurfGetsRadiantHeat = true;
				}

				AllFracsSummed += HighTempRadSys( Item ).FracDistribToSurf( SurfNum );

			} // ...end of DO loop through surfaces that the heater radiates to.

			// Error trap if the fractions add up to greater than 1.0
			if ( AllFracsSummed > ( MaxFraction + 0.01 ) ) {
				ShowSevereError( "Fraction of radiation distributed to surfaces sums up to greater than 1 for " + cAlphaArgs( 1 ) );
				ShowContinueError( "Occurs for " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}
			if ( AllFracsSummed < ( MaxFraction - 0.01 ) ) { // User didn't distribute all of the radiation warn that some will be lost
				ShowWarningError( "Fraction of radiation distributed to surfaces sums up to less than 1 for " + cAlphaArgs( 1 ) );
				ShowContinueError( "As a result, some of the radiant energy delivered by the high temp radiant heater will be lost." );
				ShowContinueError( "Occurs for " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
			}

		} // ...end of DO loop through all of the high temperature radiant heaters

		// Set up the output variables for high temperature radiant heaters
		// cCurrentModuleObject = "ZoneHVAC:HighTemperatureRadiant"
		for ( Item = 1; Item <= NumOfHighTempRadSys; ++Item ) {
			SetupOutputVariable( "Zone Radiant HVAC Heating Rate [W]", HighTempRadSys( Item ).HeatPower, "System", "Average", HighTempRadSys( Item ).Name );
			SetupOutputVariable( "Zone Radiant HVAC Heating Energy [J]", HighTempRadSys( Item ).HeatEnergy, "System", "Sum", HighTempRadSys( Item ).Name, _, "ENERGYTRANSFER", "HEATINGCOILS", _, "System" );
			if ( HighTempRadSys( Item ).HeaterType == Gas ) {
				SetupOutputVariable( "Zone Radiant HVAC Gas Rate [W]", HighTempRadSys( Item ).GasPower, "System", "Average", HighTempRadSys( Item ).Name );
				SetupOutputVariable( "Zone Radiant HVAC Gas Energy [J]", HighTempRadSys( Item ).GasEnergy, "System", "Sum", HighTempRadSys( Item ).Name, _, "Gas", "Heating", _, "System" );
			} else if ( HighTempRadSys( Item ).HeaterType == Electric ) {
				SetupOutputVariable( "Zone Radiant HVAC Electric Power [W]", HighTempRadSys( Item ).ElecPower, "System", "Average", HighTempRadSys( Item ).Name );
				SetupOutputVariable( "Zone Radiant HVAC Electric Energy [J]", HighTempRadSys( Item ).ElecEnergy, "System", "Sum", HighTempRadSys( Item ).Name, _, "ELECTRICITY", "Heating", _, "System" );
			}

		}

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in input.  Preceding condition(s) cause termination." );
		}

	}

	void
	InitHighTempRadiantSystem(
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		int const RadSysNum // Index for the low temperature radiant system under consideration within the derived types
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   February 2001
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine initializes variables relating to high temperature
		// radiant heating systems.

		// METHODOLOGY EMPLOYED:
		// Simply initializes whatever needs initializing.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::NumOfZones;
		using DataGlobals::BeginEnvrnFlag;
		using DataZoneEquipment::ZoneEquipInputsFilled;
		using DataZoneEquipment::CheckZoneEquipmentList;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool firstTime( true ); // For one-time initializations
		int ZoneNum; // Intermediate variable for keeping track of the zone number
		static bool MyEnvrnFlag( true );
		static bool ZoneEquipmentListChecked( false ); // True after the Zone Equipment List has been checked for items
		int Loop;

		// FLOW:
		if ( firstTime ) {
			ZeroSourceSumHATsurf.dimension( NumOfZones, 0.0 );
			QHTRadSource.dimension( NumOfHighTempRadSys, 0.0 );
			QHTRadSrcAvg.dimension( NumOfHighTempRadSys, 0.0 );
			LastQHTRadSrc.dimension( NumOfHighTempRadSys, 0.0 );
			LastSysTimeElapsed.dimension( NumOfHighTempRadSys, 0.0 );
			LastTimeStepSys.dimension( NumOfHighTempRadSys, 0.0 );
			MySizeFlag.dimension( NumOfHighTempRadSys, true );
			firstTime = false;
		}

		// need to check all units to see if they are on Zone Equipment List or issue warning
		if ( ! ZoneEquipmentListChecked && ZoneEquipInputsFilled ) {
			ZoneEquipmentListChecked = true;
			for ( Loop = 1; Loop <= NumOfHighTempRadSys; ++Loop ) {
				if ( CheckZoneEquipmentList( "ZoneHVAC:HighTemperatureRadiant", HighTempRadSys( Loop ).Name ) ) continue;
				ShowSevereError( "InitHighTempRadiantSystem: Unit=[ZoneHVAC:HighTemperatureRadiant," + HighTempRadSys( Loop ).Name + "] is not on any ZoneHVAC:EquipmentList.  It will not be simulated." );
			}
		}

		if ( ! SysSizingCalc && MySizeFlag( RadSysNum ) ) {
			// for each radiant systen do the sizing once.
			SizeHighTempRadiantSystem( RadSysNum );
			MySizeFlag( RadSysNum ) = false;
		}

		if ( BeginEnvrnFlag && MyEnvrnFlag ) {
			ZeroSourceSumHATsurf = 0.0;
			QHTRadSource = 0.0;
			QHTRadSrcAvg = 0.0;
			LastQHTRadSrc = 0.0;
			LastSysTimeElapsed = 0.0;
			LastTimeStepSys = 0.0;
			MyEnvrnFlag = false;
		}
		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag = true;
		}

		if ( BeginTimeStepFlag && FirstHVACIteration ) { // This is the first pass through in a particular time step
			ZoneNum = HighTempRadSys( RadSysNum ).ZonePtr;
			ZeroSourceSumHATsurf( ZoneNum ) = SumHATsurf( ZoneNum ); // Set this to figure out what part of the load the radiant system meets
			QHTRadSrcAvg( RadSysNum ) = 0.0; // Initialize this variable to zero (radiant system defaults to off)
			LastQHTRadSrc( RadSysNum ) = 0.0; // At the beginning of a time step, reset to zero so average calculation can start again
			LastSysTimeElapsed( RadSysNum ) = 0.0; // At the beginning of a time step, reset to zero so average calculation can start again
			LastTimeStepSys( RadSysNum ) = 0.0; // At the beginning of a time step, reset to zero so average calculation can start again
		}

	}

	void
	SizeHighTempRadiantSystem( int const RadSysNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   February 2002
		//       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
		//                      July 2014, B. Nigusse, added scalable sizing
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing high temperature radiant components for which max power input has not been
		// specified in the input.

		// METHODOLOGY EMPLOYED:
		// Obtains design heating load from the zone sizing arrays

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSizing;
		using ReportSizingManager::RequestSizing;
		using ReportSizingManager::ReportSizingOutput;
		using General::RoundSigDigits;
		using DataHVACGlobals::HeatingCapacitySizing;
		using DataHeatBalance::Zone;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "SizeHighTempRadiantSystem" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS
		Real64 MaxPowerCapacDes; // Design maximum capacity for reproting
		Real64 MaxPowerCapacUser; // User hard-sized maximum capacity for reproting
		bool IsAutoSize; // Indicator to autosizing nominal capacity

		std::string CompName; // component name
		std::string CompType; // component type
		std::string SizingString; // input field sizing description (e.g., Nominal Capacity)
		Real64 TempSize; // autosized value of coil input field
		int FieldNum = 1; // IDD numeric field number where input field description is found
		int SizingMethod; // Integer representation of sizing method name (e.g., CoolingAirflowSizing, HeatingAirflowSizing, CoolingCapacitySizing, HeatingCapacitySizing, etc.)
		bool PrintFlag; // TRUE when sizing information is reported in the eio file
		int CapSizingMethod( 0 ); // capacity sizing methods (HeatingDesignCapacity, CapacityPerFloorArea, FractionOfAutosizedCoolingCapacity, and FractionOfAutosizedHeatingCapacity )

		IsAutoSize = false;
		MaxPowerCapacDes = 0.0;
		MaxPowerCapacUser = 0.0;
		DataScalableCapSizingON = false;

		if ( CurZoneEqNum > 0 ) {

			CompType = "ZoneHVAC:HighTemperatureRadiant";
			CompName = HighTempRadSys( RadSysNum ).Name;
			DataFracOfAutosizedHeatingCapacity = 1.0;
			DataZoneNumber = HighTempRadSys( RadSysNum ).ZonePtr;
			SizingMethod = HeatingCapacitySizing;
			FieldNum = 1;
			PrintFlag = true;
			SizingString = HighTempRadSysNumericFields( RadSysNum ).FieldNames( FieldNum ) + " [W]";
			CapSizingMethod = HighTempRadSys( RadSysNum ).HeatingCapMethod;
			ZoneEqSizing( CurZoneEqNum ).SizingMethod( SizingMethod ) = CapSizingMethod;
			if ( CapSizingMethod == HeatingDesignCapacity || CapSizingMethod == CapacityPerFloorArea || CapSizingMethod == FractionOfAutosizedHeatingCapacity ) {

				if ( CapSizingMethod == HeatingDesignCapacity ) {
					if ( HighTempRadSys( RadSysNum ).ScaledHeatingCapacity == AutoSize ) {
						CheckZoneSizing( CompType, CompName );
						ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = CalcFinalZoneSizing( CurZoneEqNum ).DesHeatLoad * CalcFinalZoneSizing( CurZoneEqNum ).HeatSizingFactor / (HighTempRadSys( RadSysNum ).FracRadiant + HighTempRadSys( RadSysNum ).FracConvect );
					} else {
						ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = HighTempRadSys( RadSysNum ).ScaledHeatingCapacity;
					}
					ZoneEqSizing( CurZoneEqNum ).HeatingCapacity = true;
					TempSize = ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad;
				} else if ( CapSizingMethod == CapacityPerFloorArea ) {
					ZoneEqSizing( CurZoneEqNum ).HeatingCapacity = true;
					ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = HighTempRadSys( RadSysNum ).ScaledHeatingCapacity * Zone( DataZoneNumber ).FloorArea;
					TempSize = ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad;
					DataScalableCapSizingON = true;
				} else if ( CapSizingMethod == FractionOfAutosizedHeatingCapacity ) {
					CheckZoneSizing( CompType, CompName );
					ZoneEqSizing( CurZoneEqNum ).HeatingCapacity = true;
					DataFracOfAutosizedHeatingCapacity = HighTempRadSys( RadSysNum ).ScaledHeatingCapacity;
					ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = CalcFinalZoneSizing( CurZoneEqNum ).DesHeatLoad * CalcFinalZoneSizing( CurZoneEqNum ).HeatSizingFactor / (HighTempRadSys( RadSysNum ).FracRadiant + HighTempRadSys( RadSysNum ).FracConvect );
					TempSize = AutoSize;
					DataScalableCapSizingON = true;
				} else {
					TempSize = HighTempRadSys( RadSysNum ).ScaledHeatingCapacity;
				}
				RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
				HighTempRadSys( RadSysNum ).MaxPowerCapac = TempSize;
			}

		}

	}

	void
	CalcHighTempRadiantSystem( int const RadSysNum ) // name of the low temperature radiant system
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   February 2001
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine does all of the stuff that is necessary to simulate
		// a high temperature radiant heating system.

		// METHODOLOGY EMPLOYED:
		// Follows the methods used by many other pieces of zone equipment except
		// that we are controlling the input to the heater element.  Note that
		// cooling is not allowed for such a system.  Controls are very basic at
		// this point using just a linear interpolation between being off at
		// one end of the throttling range, fully on at the other end, and varying
		// linearly in between.

		// REFERENCES:
		// Other EnergyPlus modules
		// Building Systems Laboratory, BLAST User's Guide/Reference.
		// Fanger, P.O. "Analysis and Applications in Environmental Engineering",
		//   Danish Technical Press, 1970.
		// Maloney, Dan. 1987. "Development of a radiant heater model and the
		//   incorporation of thermal comfort considerations into the BLAST
		//   energy analysis program", M.S. thesis, University of Illinois at
		//   Urbana-Champaign (Dept. of Mechanical and Industrial Engineering).

		// Using/Aliasing
		using DataHeatBalance::MRT;
		using DataHeatBalFanSys::MAT;
		using namespace DataZoneEnergyDemands;
		using ScheduleManager::GetCurrentScheduleValue;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 HeatFrac; // fraction of maximum energy input to radiant system [dimensionless]
		Real64 OffTemp; // Temperature above which the radiant system should be completely off [C]
		Real64 OpTemp; // Operative temperature [C]
		//  REAL(r64)    :: QZnReq         ! heating or cooling needed by zone [Watts]
		Real64 SetPtTemp; // Setpoint temperature [C]
		int ZoneNum; // number of zone being served

		// FLOW:
		// initialize local variables
		ZoneNum = HighTempRadSys( RadSysNum ).ZonePtr;
		HeatFrac = 0.0;

		if ( GetCurrentScheduleValue( HighTempRadSys( RadSysNum ).SchedPtr ) <= 0 ) {

			// Unit is off or has no load upon it; set the flow rates to zero and then
			// simulate the components with the no flow conditions
			QHTRadSource( RadSysNum ) = 0.0;

		} else { // Unit might be on-->this section is intended to control the output of the
			// high temperature radiant heater (temperature controlled)

			// Determine the current setpoint temperature and the temperature at which the unit should be completely off
			SetPtTemp = GetCurrentScheduleValue( HighTempRadSys( RadSysNum ).SetptSchedPtr );
			OffTemp = SetPtTemp + 0.5 * HighTempRadSys( RadSysNum ).ThrottlRange;
			OpTemp = ( MAT( ZoneNum ) + MRT( ZoneNum ) ) / 2.0; // Approximate the "operative" temperature

			// Determine the fraction of maximum power to the unit (limiting the fraction range from zero to unity)
			{ auto const SELECT_CASE_var( HighTempRadSys( RadSysNum ).ControlType );
			if ( SELECT_CASE_var == MATControl ) {
				HeatFrac = ( OffTemp - MAT( ZoneNum ) ) / HighTempRadSys( RadSysNum ).ThrottlRange;
			} else if ( SELECT_CASE_var == MRTControl ) {
				HeatFrac = ( OffTemp - MRT( ZoneNum ) ) / HighTempRadSys( RadSysNum ).ThrottlRange;
			} else if ( SELECT_CASE_var == OperativeControl ) {
				OpTemp = 0.5 * ( MAT( ZoneNum ) + MRT( ZoneNum ) );
				HeatFrac = ( OffTemp - OpTemp ) / HighTempRadSys( RadSysNum ).ThrottlRange;
			}}
			if ( HeatFrac < 0.0 ) HeatFrac = 0.0;
			if ( HeatFrac > 1.0 ) HeatFrac = 1.0;

			// Set the heat source for the high temperature electric radiant system
			QHTRadSource( RadSysNum ) = HeatFrac * HighTempRadSys( RadSysNum ).MaxPowerCapac;

		}

	}

	void
	CalcHighTempRadiantSystemSP(
		bool const EP_UNUSED( FirstHVACIteration ), // true if this is the first HVAC iteration at this system time step !unused1208
		int const RadSysNum // name of the low temperature radiant system
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   February 2008
		//       MODIFIED       Sep 2011 LKL/BG - resimulate only zones needing it for Radiant systems
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine does all of the stuff that is necessary to simulate
		// a high temperature radiant heating system using setpoint temperature control.

		// METHODOLOGY EMPLOYED:
		// Follows the methods used by many other pieces of zone equipment except
		// that we are controlling the input to the heater element.  Note that
		// cooling is not allowed for such a system.  Controls are very basic and
		// use an iterative approach to get close to what we need.

		// REFERENCES:
		// Other EnergyPlus modules
		// Building Systems Laboratory, BLAST User's Guide/Reference.
		// Fanger, P.O. "Analysis and Applications in Environmental Engineering",
		//   Danish Technical Press, 1970.
		// Maloney, Dan. 1987. "Development of a radiant heater model and the
		//   incorporation of thermal comfort considerations into the BLAST
		//   energy analysis program", M.S. thesis, University of Illinois at
		//   Urbana-Champaign (Dept. of Mechanical and Industrial Engineering).

		// Using/Aliasing
		using DataHeatBalance::MRT;
		using DataHeatBalFanSys::MAT;
		using ScheduleManager::GetCurrentScheduleValue;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		float const TempConvToler( 0.1 ); // Temperature controller tries to converge to within 0.1C
		int const MaxIterations( 10 ); // Maximum number of iterations to achieve temperature control
		// (10 interval halvings achieves control to 0.1% of capacity)
		// These two parameters are intended to achieve reasonable control
		// without excessive run times.

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		bool ConvergFlag; // convergence flag for temperature control
		//unused  INTEGER, SAVE :: ErrIterCount=0   ! number of time max iterations has been exceeded
		float HeatFrac; // fraction of maximum energy input to radiant system [dimensionless]
		float HeatFracMax; // maximum range of heat fraction
		float HeatFracMin; // minimum range of heat fraction
		int IterNum; // iteration number
		Real64 SetPtTemp; // Setpoint temperature [C]
		int ZoneNum; // number of zone being served
		Real64 ZoneTemp( 0.0 ); // zone temperature (MAT, MRT, or Operative Temperature, depending on control type) [C]

		// FLOW:
		// initialize local variables
		ZoneNum = HighTempRadSys( RadSysNum ).ZonePtr;
		QHTRadSource( RadSysNum ) = 0.0;

		if ( GetCurrentScheduleValue( HighTempRadSys( RadSysNum ).SchedPtr ) > 0 ) {

			// Unit is scheduled on-->this section is intended to control the output of the
			// high temperature radiant heater (temperature controlled)

			// Determine the current setpoint temperature and the temperature at which the unit should be completely off
			SetPtTemp = GetCurrentScheduleValue( HighTempRadSys( RadSysNum ).SetptSchedPtr );

			// Now, distribute the radiant energy of all systems to the appropriate
			// surfaces, to people, and the air; determine the latent portion
			DistributeHTRadGains();

			// Now "simulate" the system by recalculating the heat balances
			HeatBalanceSurfaceManager::CalcHeatBalanceOutsideSurf( ZoneNum );
			HeatBalanceSurfaceManager::CalcHeatBalanceInsideSurf( ZoneNum );

			// First determine whether or not the unit should be on
			// Determine the proper temperature on which to control
			{ auto const SELECT_CASE_var( HighTempRadSys( RadSysNum ).ControlType );
			if ( SELECT_CASE_var == MATSPControl ) {
				ZoneTemp = MAT( ZoneNum );
			} else if ( SELECT_CASE_var == MRTSPControl ) {
				ZoneTemp = MRT( ZoneNum );
			} else if ( SELECT_CASE_var == OperativeSPControl ) {
				ZoneTemp = 0.5 * ( MAT( ZoneNum ) + MRT( ZoneNum ) );
			} else {
				assert( false );
			}}

			if ( ZoneTemp < ( SetPtTemp - TempConvToler ) ) {

				// Use simple interval halving to find the best operating fraction to achieve proper temperature control
				IterNum = 0;
				ConvergFlag = false;
				HeatFracMax = 1.0;
				HeatFracMin = 0.0;

				while ( ( IterNum <= MaxIterations ) && ( ! ConvergFlag ) ) {

					// In the first iteration (IterNum=0), try full capacity and see if that is the best solution
					if ( IterNum == 0 ) {
						HeatFrac = 1.0;
					} else {
						HeatFrac = ( HeatFracMin + HeatFracMax ) / 2.0;
					}

					// Set the heat source for the high temperature radiant system
					QHTRadSource( RadSysNum ) = HeatFrac * HighTempRadSys( RadSysNum ).MaxPowerCapac;

					// Now, distribute the radiant energy of all systems to the appropriate
					// surfaces, to people, and the air; determine the latent portion
					DistributeHTRadGains();

					// Now "simulate" the system by recalculating the heat balances
					HeatBalanceSurfaceManager::CalcHeatBalanceOutsideSurf( ZoneNum );
					HeatBalanceSurfaceManager::CalcHeatBalanceInsideSurf( ZoneNum );

					// Redetermine the current value of the controlling temperature
					{ auto const SELECT_CASE_var( HighTempRadSys( RadSysNum ).ControlType );
					if ( SELECT_CASE_var == MATControl ) {
						ZoneTemp = MAT( ZoneNum );
					} else if ( SELECT_CASE_var == MRTControl ) {
						ZoneTemp = MRT( ZoneNum );
					} else if ( SELECT_CASE_var == OperativeControl ) {
						ZoneTemp = 0.5 * ( MAT( ZoneNum ) + MRT( ZoneNum ) );
					}}

					if ( ( std::abs( ZoneTemp - SetPtTemp ) ) <= TempConvToler ) {
						// The radiant heater has controlled the zone temperature to the appropriate level--stop iterating
						ConvergFlag = true;
					} else if ( ZoneTemp < SetPtTemp ) {
						// The zone temperature is too low--try increasing the radiant heater output
						if ( IterNum == 0 ) {
							// Heater already at capacity--this is the best that we can do
							ConvergFlag = true;
						} else {
							HeatFracMin = HeatFrac;
						}
					} else { // (ZoneTemp > SetPtTemp)
						// The zone temperature is too high--try decreasing the radiant heater output
						if ( IterNum > 0 ) HeatFracMax = HeatFrac;
					}

					++IterNum;

				}

			}

		}

	}

	void
	UpdateHighTempRadiantSystem(
		int const RadSysNum, // Index for the low temperature radiant system under consideration within the derived types
		Real64 & LoadMet // load met by the radiant system, in Watts
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   February 2001
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine does any updating that needs to be done for high
		// temperature radiant heating systems.  This routine has two functions.
		// First, it needs to keep track of the average high temperature
		// radiant source.  The method for doing this is similar to low
		// temperature systems except that heat input is kept locally on
		// a system basis rather than a surface basis.  This is because a high
		// temperature system affects many surfaces while a low temperature
		// system directly affects only one surface.  This leads to the second
		// function of this subroutine which is to account for the affect of
		// all high temperature radiant systems on each surface.  This
		// distribution must be "redone" every time to be sure that we have
		// properly accounted for all of the systems.

		// METHODOLOGY EMPLOYED:
		// For the source average update, if the system time step elapsed is
		// still what it used to be, then either we are still iterating or we
		// had to go back and shorten the time step.  As a result, we have to
		// subtract out the previous value that we added.  If the system time
		// step elapsed is different, then we just need to add the new values
		// to the running average.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::TimeStepZone;
		using DataGlobals::BeginEnvrnFlag;
		using DataHeatBalFanSys::SumConvHTRadSys;
		using DataHVACGlobals::TimeStepSys;
		using DataHVACGlobals::SysTimeElapsed;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ZoneNum; // Zone index number for the current radiant system
		static bool MyEnvrnFlag( true );

		// FLOW:
		if ( BeginEnvrnFlag && MyEnvrnFlag ) {
			MyEnvrnFlag = false;
		}
		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag = true;
		}
		// First, update the running average if necessary...
		if ( LastSysTimeElapsed( RadSysNum ) == SysTimeElapsed ) {
			// Still iterating or reducing system time step, so subtract old values which were
			// not valid
			QHTRadSrcAvg( RadSysNum ) -= LastQHTRadSrc( RadSysNum ) * LastTimeStepSys( RadSysNum ) / TimeStepZone;
		}

		// Update the running average and the "last" values with the current values of the appropriate variables
		QHTRadSrcAvg( RadSysNum ) += QHTRadSource( RadSysNum ) * TimeStepSys / TimeStepZone;

		LastQHTRadSrc( RadSysNum ) = QHTRadSource( RadSysNum );
		LastSysTimeElapsed( RadSysNum ) = SysTimeElapsed;
		LastTimeStepSys( RadSysNum ) = TimeStepSys;

		{ auto const SELECT_CASE_var( HighTempRadSys( RadSysNum ).ControlType );
		if ( ( SELECT_CASE_var == MATControl ) || ( SELECT_CASE_var == MRTControl ) || ( SELECT_CASE_var == OperativeControl ) ) {
			// Only need to do this for the non-SP controls (SP has already done this enough)
			// Now, distribute the radiant energy of all systems to the appropriate
			// surfaces, to people, and the air; determine the latent portion
			DistributeHTRadGains();

			// Now "simulate" the system by recalculating the heat balances
			ZoneNum = HighTempRadSys( RadSysNum ).ZonePtr;
			HeatBalanceSurfaceManager::CalcHeatBalanceOutsideSurf( ZoneNum );
			HeatBalanceSurfaceManager::CalcHeatBalanceInsideSurf( ZoneNum );
		}}

		if ( QHTRadSource( RadSysNum ) <= 0.0 ) {
			LoadMet = 0.0; // System wasn't running so it can't meet a load
		} else {
			ZoneNum = HighTempRadSys( RadSysNum ).ZonePtr;
			LoadMet = ( SumHATsurf( ZoneNum ) - ZeroSourceSumHATsurf( ZoneNum ) ) + SumConvHTRadSys( ZoneNum );
		}

	}

	void
	UpdateHTRadSourceValAvg( bool & HighTempRadSysOn ) // .TRUE. if the radiant system has run this zone time step
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   February 2001
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// To transfer the average value of the heat source over the entire
		// zone time step back to the heat balance routines so that the heat
		// balance algorithms can simulate one last time with the average source
		// to maintain some reasonable amount of continuity and energy balance
		// in the temperature and flux histories.

		// METHODOLOGY EMPLOYED:
		// All of the record keeping for the average term is done in the Update
		// routine so the only other thing that this subroutine does is check to
		// see if the system was even on.  If any average term is non-zero, then
		// one or more of the radiant systems was running.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int RadSysNum; // DO loop counter for surface index

		// FLOW:
		HighTempRadSysOn = false;

		// If this was never allocated, then there are no radiant systems in this input file (just RETURN)
		if ( ! allocated( QHTRadSrcAvg ) ) return;

		// If it was allocated, then we have to check to see if this was running at all...
		for ( RadSysNum = 1; RadSysNum <= NumOfHighTempRadSys; ++RadSysNum ) {
			if ( QHTRadSrcAvg( RadSysNum ) != 0.0 ) {
				HighTempRadSysOn = true;
				break; //DO loop
			}
		}

		QHTRadSource = QHTRadSrcAvg;

		DistributeHTRadGains(); // QHTRadSource has been modified so we need to redistribute gains

	}

	void
	DistributeHTRadGains()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   February 2001
		//       MODIFIED       April 2010 Brent Griffith, max limit to protect surface temperature calcs
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// To distribute the gains from the high temperature radiant heater
		// as specified in the user input file.  This includes distribution
		// of long wavelength radiant gains to surfaces and "people" as well
		// as latent, lost, and convective portions of the total gain.

		// METHODOLOGY EMPLOYED:
		// We must cycle through all of the radiant systems because each
		// surface could feel the effect of more than one radiant system.
		// Note that the energy radiated to people is assumed to affect them
		// but them it is assumed to be convected to the air.  This is why
		// the convective portion shown below has two parts to it.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::NumOfZones;
		using DataHeatBalance::Zone;
		using DataHeatBalFanSys::SumConvHTRadSys;
		using DataHeatBalFanSys::SumLatentHTRadSys;
		using DataHeatBalFanSys::QHTRadSysToPerson;
		using DataHeatBalFanSys::QHTRadSysSurf;
		using DataHeatBalFanSys::MaxRadHeatFlux;
		using DataSurfaces::Surface;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const SmallestArea( 0.001 ); // Smallest area in meters squared (to avoid a divide by zero)

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int RadSurfNum; // Counter for surfaces receiving radiation from radiant heater
		int RadSysNum; // Counter for the radiant systems
		int SurfNum; // Pointer to the Surface derived type
		int ZoneNum; // Pointer to the Zone derived type
		Real64 ThisSurfIntensity; // temporary for W/m2 term for rad on a surface

		// FLOW:
		// Initialize arrays
		SumConvHTRadSys = 0.0;
		SumLatentHTRadSys = 0.0;
		QHTRadSysSurf = 0.0;
		QHTRadSysToPerson = 0.0;

		for ( RadSysNum = 1; RadSysNum <= NumOfHighTempRadSys; ++RadSysNum ) {

			ZoneNum = HighTempRadSys( RadSysNum ).ZonePtr;

			QHTRadSysToPerson( ZoneNum ) = QHTRadSource( RadSysNum ) * HighTempRadSys( RadSysNum ).FracRadiant * HighTempRadSys( RadSysNum ).FracDistribPerson;

			SumConvHTRadSys( ZoneNum ) += QHTRadSource( RadSysNum ) * HighTempRadSys( RadSysNum ).FracConvect;

			SumLatentHTRadSys( ZoneNum ) += QHTRadSource( RadSysNum ) * HighTempRadSys( RadSysNum ).FracLatent;

			for ( RadSurfNum = 1; RadSurfNum <= HighTempRadSys( RadSysNum ).TotSurfToDistrib; ++RadSurfNum ) {
				SurfNum = HighTempRadSys( RadSysNum ).SurfacePtr( RadSurfNum );
				if ( Surface( SurfNum ).Area > SmallestArea ) {
					ThisSurfIntensity = ( QHTRadSource( RadSysNum ) * HighTempRadSys( RadSysNum ).FracRadiant * HighTempRadSys( RadSysNum ).FracDistribToSurf( RadSurfNum ) / Surface( SurfNum ).Area );
					QHTRadSysSurf( SurfNum ) += ThisSurfIntensity;

					if ( ThisSurfIntensity > MaxRadHeatFlux ) { // CR 8074, trap for excessive intensity (throws off surface balance )
						ShowSevereError( "DistributeHTRadGains:  excessive thermal radiation heat flux intensity detected" );
						ShowContinueError( "Surface = " + Surface( SurfNum ).Name );
						ShowContinueError( "Surface area = " + RoundSigDigits( Surface( SurfNum ).Area, 3 ) + " [m2]" );
						ShowContinueError( "Occurs in ZoneHVAC:HighTemperatureRadiant = " + HighTempRadSys( RadSysNum ).Name );
						ShowContinueError( "Radiation intensity = " + RoundSigDigits( ThisSurfIntensity, 2 ) + " [W/m2]" );
						ShowContinueError( "Assign a larger surface area or more surfaces in ZoneHVAC:HighTemperatureRadiant" );
						ShowFatalError( "DistributeHTRadGains:  excessive thermal radiation heat flux intensity detected" );
					}
				} else { // small surface
					ShowSevereError( "DistributeHTRadGains:  surface not large enough to receive thermal radiation heat flux" );
					ShowContinueError( "Surface = " + Surface( SurfNum ).Name );
					ShowContinueError( "Surface area = " + RoundSigDigits( Surface( SurfNum ).Area, 3 ) + " [m2]" );
					ShowContinueError( "Occurs in ZoneHVAC:HighTemperatureRadiant = " + HighTempRadSys( RadSysNum ).Name );
					ShowContinueError( "Assign a larger surface area or more surfaces in ZoneHVAC:HighTemperatureRadiant" );
					ShowFatalError( "DistributeHTRadGains:  surface not large enough to receive thermal radiation heat flux" );

				}

			}

		}

		// Here an assumption is made regarding radiant heat transfer to people.
		// While the QHTRadSysToPerson array will be used by the thermal comfort
		// routines, the energy transfer to people would get lost from the perspective
		// of the heat balance.  So, to avoid this net loss of energy which clearly
		// gets added to the zones, we must account for it somehow.  This assumption
		// that all energy radiated to people is converted to convective energy is
		// not very precise, but at least it conserves energy.
		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
			SumConvHTRadSys( ZoneNum ) += QHTRadSysToPerson( ZoneNum );
		}

	}

	void
	ReportHighTempRadiantSystem( int const RadSysNum ) // Index for the low temperature radiant system under consideration within the derived types
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   February 2001
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine simply produces output for the high temperature radiant system.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::SecInHour;
		using DataHVACGlobals::TimeStepSys;
		using DataSurfaces::Surface;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		// FLOW:
		if ( HighTempRadSys( RadSysNum ).HeaterType == Gas ) {
			HighTempRadSys( RadSysNum ).GasPower = QHTRadSource( RadSysNum ) / HighTempRadSys( RadSysNum ).CombustionEffic;
			HighTempRadSys( RadSysNum ).GasEnergy = HighTempRadSys( RadSysNum ).GasPower * TimeStepSys * SecInHour;
			HighTempRadSys( RadSysNum ).ElecPower = 0.0;
			HighTempRadSys( RadSysNum ).ElecEnergy = 0.0;
		} else if ( HighTempRadSys( RadSysNum ).HeaterType == Electric ) {
			HighTempRadSys( RadSysNum ).GasPower = 0.0;
			HighTempRadSys( RadSysNum ).GasEnergy = 0.0;
			HighTempRadSys( RadSysNum ).ElecPower = QHTRadSource( RadSysNum );
			HighTempRadSys( RadSysNum ).ElecEnergy = HighTempRadSys( RadSysNum ).ElecPower * TimeStepSys * SecInHour;
		} else {
			ShowWarningError( "Someone forgot to add a high temperature radiant heater type to the reporting subroutine" );
		}
		HighTempRadSys( RadSysNum ).HeatPower = QHTRadSource( RadSysNum );
		HighTempRadSys( RadSysNum ).HeatEnergy = HighTempRadSys( RadSysNum ).HeatPower * TimeStepSys * SecInHour;

	}

	Real64
	SumHATsurf( int const ZoneNum ) // Zone number
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   July 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function calculates the zone sum of Hc*Area*Tsurf.  It replaces the old SUMHAT.
		// The SumHATsurf code below is also in the CalcZoneSums subroutine in ZoneTempPredictorCorrector
		// and should be updated accordingly.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSurfaces;
		using namespace DataHeatBalance;
		using namespace DataHeatBalSurface;

		// Return value
		Real64 SumHATsurf;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int SurfNum; // Surface number
		Real64 Area; // Effective surface area

		// FLOW:
		SumHATsurf = 0.0;

		for ( SurfNum = Zone( ZoneNum ).SurfaceFirst; SurfNum <= Zone( ZoneNum ).SurfaceLast; ++SurfNum ) {
			if ( ! Surface( SurfNum ).HeatTransSurf ) continue; // Skip non-heat transfer surfaces

			Area = Surface( SurfNum ).Area;

			if ( Surface( SurfNum ).Class == SurfaceClass_Window ) {
				if ( SurfaceWindow( SurfNum ).ShadingFlag == IntShadeOn || SurfaceWindow( SurfNum ).ShadingFlag == IntBlindOn ) {
					// The area is the shade or blind area = the sum of the glazing area and the divider area (which is zero if no divider)
					Area += SurfaceWindow( SurfNum ).DividerArea;
				}

				if ( SurfaceWindow( SurfNum ).FrameArea > 0.0 ) {
					// Window frame contribution
					SumHATsurf += HConvIn( SurfNum ) * SurfaceWindow( SurfNum ).FrameArea * ( 1.0 + SurfaceWindow( SurfNum ).ProjCorrFrIn ) * SurfaceWindow( SurfNum ).FrameTempSurfIn;
				}

				if ( SurfaceWindow( SurfNum ).DividerArea > 0.0 && SurfaceWindow( SurfNum ).ShadingFlag != IntShadeOn && SurfaceWindow( SurfNum ).ShadingFlag != IntBlindOn ) {
					// Window divider contribution (only from shade or blind for window with divider and interior shade or blind)
					SumHATsurf += HConvIn( SurfNum ) * SurfaceWindow( SurfNum ).DividerArea * ( 1.0 + 2.0 * SurfaceWindow( SurfNum ).ProjCorrDivIn ) * SurfaceWindow( SurfNum ).DividerTempSurfIn;
				}
			}

			SumHATsurf += HConvIn( SurfNum ) * Area * TempSurfInTmp( SurfNum );
		}

		return SumHATsurf;

	}

} // HighTempRadiantSystem

} // EnergyPlus
