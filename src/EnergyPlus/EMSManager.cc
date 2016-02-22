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
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <EMSManager.hh>
#include <CommandLineInterface.hh>
#include <DataAirLoop.hh>
#include <DataAirSystems.hh>
#include <DataGlobals.hh>
#include <DataStringGlobals.hh>
#include <DataHeatBalance.hh>
#include <DataLoopNode.hh>
#include <DataPrecisionGlobals.hh>
#include <DataRuntimeLanguage.hh>
#include <DataSurfaces.hh>
#include <DataZoneControls.hh>
#include <EMSManager.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <OutAirNodeManager.hh>
#include <OutputProcessor.hh>
#include <RuntimeLanguageProcessor.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

//note there are routines that lie outside of the Module at the end of this file

namespace EMSManager {

	// MODULE INFORMATION:
	//       AUTHOR         Peter Graham Ellis
	//       DATE WRITTEN   June 2006
	//       MODIFIED       Brent Griffith
	//                      May - August 2009
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This module manages the programmable energy management system(EMS).

	// METHODOLOGY EMPLOYED:

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataRuntimeLanguage;

	// Data
	// MODULE PARAMETER DEFINITIONS
	int const iTemperatureSetPoint( 101 ); // integer for node setpoint control type
	int const iTemperatureMinSetPoint( 102 ); // integer for node setpoint control type
	int const iTemperatureMaxSetPoint( 103 ); // integer for node setpoint control type
	int const iHumidityRatioSetPoint( 104 ); // integer for node setpoint control type
	int const iHumidityRatioMinSetPoint( 105 ); // integer for node setpoint control type
	int const iHumidityRatioMaxSetPoint( 106 ); // integer for node setpoint control type
	int const iMassFlowRateSetPoint( 107 ); // integer for node setpoint control type
	int const iMassFlowRateMinSetPoint( 108 ); // integer for node setpoint control type
	int const iMassFlowRateMaxSetPoint( 109 ); // integer for node setpoint control type

	static std::string const BlankString;

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE TYPE DECLARATIONS:

	// MODULE VARIABLE DECLARATIONS:
	bool GetEMSUserInput( true ); // Flag to prevent input from being read multiple times
	bool ZoneThermostatActuatorsHaveBeenSetup( false );
	bool FinishProcessingUserInput( true ); // Flag to indicate still need to process input

	// SUBROUTINE SPECIFICATIONS:

	// Functions
	void
	clear_state()
	{
		GetEMSUserInput = true ;
		ZoneThermostatActuatorsHaveBeenSetup = false ;
		FinishProcessingUserInput = true ;
	}

	void
	CheckIfAnyEMS()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   April 2009
		//       MODIFIED       Rui Zhang February 2010
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Determine if EMS is used in model and set flag
		// This needs to be checked early so calls to SetupEMSActuator
		// can be avoided if there is no EMS in model.
		// We cannot do error checking during the full get input until later in the simulation.

		// METHODOLOGY EMPLOYED:
		// Get number of EMS-related input objects and set
		// global logical AnyEnergyManagementSystemInModel

		// REFERENCES:
		// na

		// USE STATEMENTS:
		//  USE DataIPShortCuts
		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using DataGlobals::AnyEnergyManagementSystemInModel;
		using General::ScanForReports;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int write_stat;
		std::string cCurrentModuleObject;

		cCurrentModuleObject = "EnergyManagementSystem:Sensor";
		NumSensors = GetNumObjectsFound( cCurrentModuleObject );

		cCurrentModuleObject = "EnergyManagementSystem:Actuator";
		numActuatorsUsed = GetNumObjectsFound( cCurrentModuleObject );

		cCurrentModuleObject = "EnergyManagementSystem:ProgramCallingManager";
		NumProgramCallManagers = GetNumObjectsFound( cCurrentModuleObject );

		cCurrentModuleObject = "EnergyManagementSystem:Program";
		NumErlPrograms = GetNumObjectsFound( cCurrentModuleObject );

		cCurrentModuleObject = "EnergyManagementSystem:Subroutine";
		NumErlSubroutines = GetNumObjectsFound( cCurrentModuleObject );

		cCurrentModuleObject = "EnergyManagementSystem:GlobalVariable";
		NumUserGlobalVariables = GetNumObjectsFound( cCurrentModuleObject );

		cCurrentModuleObject = "EnergyManagementSystem:OutputVariable";
		NumEMSOutputVariables = GetNumObjectsFound( cCurrentModuleObject );

		cCurrentModuleObject = "EnergyManagementSystem:MeteredOutputVariable";
		NumEMSMeteredOutputVariables = GetNumObjectsFound( cCurrentModuleObject );

		cCurrentModuleObject = "EnergyManagementSystem:CurveOrTableIndexVariable";
		NumEMSCurveIndices = GetNumObjectsFound( cCurrentModuleObject );

		cCurrentModuleObject = "ExternalInterface:Variable";
		NumExternalInterfaceGlobalVariables = GetNumObjectsFound( cCurrentModuleObject );

		// added for FMUImport
		cCurrentModuleObject = "ExternalInterface:FunctionalMockupUnitImport:To:Variable";
		NumExternalInterfaceFunctionalMockupUnitImportGlobalVariables = GetNumObjectsFound( cCurrentModuleObject );

		// added for FMUExport
		cCurrentModuleObject = "ExternalInterface:FunctionalMockupUnitExport:To:Variable";
		NumExternalInterfaceFunctionalMockupUnitExportGlobalVariables = GetNumObjectsFound( cCurrentModuleObject );

		cCurrentModuleObject = "ExternalInterface:Actuator";
		NumExternalInterfaceActuatorsUsed = GetNumObjectsFound( cCurrentModuleObject );

		// added for FMUImport
		cCurrentModuleObject = "ExternalInterface:FunctionalMockupUnitImport:To:Actuator";
		NumExternalInterfaceFunctionalMockupUnitImportActuatorsUsed = GetNumObjectsFound( cCurrentModuleObject );

		// added for FMUExport
		cCurrentModuleObject = "ExternalInterface:FunctionalMockupUnitExport:To:Actuator";
		NumExternalInterfaceFunctionalMockupUnitExportActuatorsUsed = GetNumObjectsFound( cCurrentModuleObject );

		cCurrentModuleObject = "EnergyManagementSystem:ConstructionIndexVariable";
		NumEMSConstructionIndices = GetNumObjectsFound( cCurrentModuleObject );

		// added for FMU
		if ( ( NumSensors + numActuatorsUsed + NumProgramCallManagers + NumErlPrograms + NumErlSubroutines + NumUserGlobalVariables + NumEMSOutputVariables + NumEMSCurveIndices + NumExternalInterfaceGlobalVariables + NumExternalInterfaceActuatorsUsed + NumEMSConstructionIndices + NumEMSMeteredOutputVariables + NumExternalInterfaceFunctionalMockupUnitImportActuatorsUsed + NumExternalInterfaceFunctionalMockupUnitImportGlobalVariables + NumExternalInterfaceFunctionalMockupUnitExportActuatorsUsed + NumExternalInterfaceFunctionalMockupUnitExportGlobalVariables ) > 0 ) {
			AnyEnergyManagementSystemInModel = true;
		} else {
			AnyEnergyManagementSystemInModel = false;
		}

		if ( AnyEnergyManagementSystemInModel ) {

			ScanForReports( "EnergyManagementSystem", OutputEDDFile );
			if ( OutputEDDFile ) {
				// open up output file for EMS EDD file  EMS Data and Debug
				OutputEMSFileUnitNum = GetNewUnitNumber();
				{ IOFlags flags; flags.ACTION( "write" ); gio::open( OutputEMSFileUnitNum, DataStringGlobals::outputEddFileName, flags ); write_stat = flags.ios(); }
				if ( write_stat != 0 ) {
					ShowFatalError( "CheckIFAnyEMS: Could not open file "+ DataStringGlobals::outputEddFileName +" for output (write)." );
				}
			}
		} else {
			ScanForReports( "EnergyManagementSystem", OutputEDDFile );
			if ( OutputEDDFile ) {
				ShowWarningError( "CheckIFAnyEMS: No EnergyManagementSystem has been set up in the input file but output is requested." );
				ShowContinueError( "No EDD file will be produced. Refer to EMS Application Guide and/or InputOutput Reference to set up your EnergyManagementSystem." );
			}
		}

	}

	// MODULE SUBROUTINES:

	void
	ManageEMS(
		int const iCalledFrom, // indicates where subroutine was called from, parameters in DataGlobals.
		Optional_int_const ProgramManagerToRun // specific program manager to run
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   June 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  Brent Griffith, April 2009
		//                      added calling point argument and logic.
		//                      Collapsed SimulateEMS into this routine

		// PURPOSE OF THIS SUBROUTINE:

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// Using/Aliasing
		using DataGlobals::AnyEnergyManagementSystemInModel;
		using DataGlobals::emsCallFromSetupSimulation;
		using DataGlobals::emsCallFromExternalInterface;
		using DataGlobals::emsCallFromBeginNewEvironment;
		using DataGlobals::emsCallFromUserDefinedComponentModel;

		using RuntimeLanguageProcessor::EvaluateStack;
		using RuntimeLanguageProcessor::BeginEnvrnInitializeRuntimeLanguage;
		using OutputProcessor::MeterType;
		using OutputProcessor::RealVariables;
		using OutputProcessor::RealVariableType;
		using OutputProcessor::RVar;
		using OutputProcessor::RVariableTypes;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int ErlVariableNum; // local index
		int ProgramManagerNum; // local index and loop
		int ErlProgramNum; // local index
		int ActuatorUsedLoop; // local loop
		int EMSActuatorVariableNum;
		bool AnyProgramRan; // local logical
		int tmpInteger;
		//  INTEGER  :: ProgramNum

		// FLOW:
		if ( ! AnyEnergyManagementSystemInModel ) return; // quick return if nothing to do

		if ( iCalledFrom == emsCallFromBeginNewEvironment ) BeginEnvrnInitializeRuntimeLanguage();

		InitEMS( iCalledFrom );

		if ( iCalledFrom == emsCallFromSetupSimulation ) {
			ProcessEMSInput( true );
			return;
		}

		// Run the Erl programs depending on calling point.
		AnyProgramRan = false;
		if ( iCalledFrom != emsCallFromUserDefinedComponentModel ) {
			for ( ProgramManagerNum = 1; ProgramManagerNum <= NumProgramCallManagers; ++ProgramManagerNum ) {

				if ( EMSProgramCallManager( ProgramManagerNum ).CallingPoint == iCalledFrom ) {
					for ( ErlProgramNum = 1; ErlProgramNum <= EMSProgramCallManager( ProgramManagerNum ).NumErlPrograms; ++ErlProgramNum ) {
						EvaluateStack( EMSProgramCallManager( ProgramManagerNum ).ErlProgramARR( ErlProgramNum ) );
						AnyProgramRan = true;
					}
				}
			}
		} else { // call specific program manager
			if ( present( ProgramManagerToRun ) ) {
				for ( ErlProgramNum = 1; ErlProgramNum <= EMSProgramCallManager( ProgramManagerToRun ).NumErlPrograms; ++ErlProgramNum ) {
					EvaluateStack( EMSProgramCallManager( ProgramManagerToRun ).ErlProgramARR( ErlProgramNum ) );
					AnyProgramRan = true;
				}
			}
		}

		if ( iCalledFrom == emsCallFromExternalInterface ) {
			AnyProgramRan = true;
		}

		if ( ! AnyProgramRan ) return;

		// Set actuated variables with new values
		for ( ActuatorUsedLoop = 1; ActuatorUsedLoop <= numActuatorsUsed + NumExternalInterfaceActuatorsUsed + NumExternalInterfaceFunctionalMockupUnitImportActuatorsUsed + NumExternalInterfaceFunctionalMockupUnitExportActuatorsUsed; ++ActuatorUsedLoop ) {
			ErlVariableNum = EMSActuatorUsed( ActuatorUsedLoop ).ErlVariableNum;
			if ( ! ( ErlVariableNum > 0 ) ) continue; // this can happen for good reason during sizing

			EMSActuatorVariableNum = EMSActuatorUsed( ActuatorUsedLoop ).ActuatorVariableNum;
			if ( ! ( EMSActuatorVariableNum > 0 ) ) continue; // this can happen for good reason during sizing

			if ( ErlVariable( ErlVariableNum ).Value.Type == ValueNull ) {
				EMSActuatorAvailable( EMSActuatorVariableNum ).Actuated = false;
			} else {
				// Set the value and the actuated flag remotely on the actuated object via the pointer
				{ auto const SELECT_CASE_var( EMSActuatorAvailable( EMSActuatorVariableNum ).PntrVarTypeUsed );

				if ( SELECT_CASE_var == PntrReal ) {
					EMSActuatorAvailable( EMSActuatorVariableNum ).Actuated = true;
					EMSActuatorAvailable( EMSActuatorVariableNum ).RealValue = ErlVariable( ErlVariableNum ).Value.Number;
				} else if ( SELECT_CASE_var == PntrInteger ) {
					EMSActuatorAvailable( EMSActuatorVariableNum ).Actuated = true;
					tmpInteger = std::floor( ErlVariable( ErlVariableNum ).Value.Number );
					EMSActuatorAvailable( EMSActuatorVariableNum ).IntValue = tmpInteger;
				} else if ( SELECT_CASE_var == PntrLogical ) {
					EMSActuatorAvailable( EMSActuatorVariableNum ).Actuated = true;
					if ( ErlVariable( ErlVariableNum ).Value.Number == 0.0 ) {
						EMSActuatorAvailable( EMSActuatorVariableNum ).LogValue = false;
					} else if ( ErlVariable( ErlVariableNum ).Value.Number == 1.0 ) {
						EMSActuatorAvailable( EMSActuatorVariableNum ).LogValue = true;
					} else {
						EMSActuatorAvailable( EMSActuatorVariableNum ).LogValue = false;
					}

				} else {

				}}
			}

		}

		ReportEMS();

	}

	void
	InitEMS( int const iCalledFrom ) // indicates where subroutine was called from, parameters in DataGlobals.
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   May 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// collect routines needed to initialize EMS

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::DoingSizing;
		using DataGlobals::KickOffSimulation;
		using DataGlobals::BeginEnvrnFlag;
		using DataGlobals::emsCallFromZoneSizing;
		using DataGlobals::emsCallFromSystemSizing;
		using DataGlobals::emsCallFromUserDefinedComponentModel;
		using RuntimeLanguageProcessor::InitializeRuntimeLanguage;
		using RuntimeLanguageProcessor::SetErlValueNumber;
		using ScheduleManager::GetCurrentScheduleValue;
		using DataZoneControls::GetZoneAirStatsInputFlag;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int InternalVarUsedNum; // local index and loop
		int InternVarAvailNum; // local index
		int SensorNum; // local loop and index
		int ErlVariableNum; // local index
		Real64 tmpReal; // temporary local integer

		if ( GetEMSUserInput ) {
			SetupZoneInfoAsInternalDataAvail();
			SetupWindowShadingControlActuators();
			SetupSurfaceConvectionActuators();
			SetupSurfaceConstructionActuators();
			SetupSurfaceOutdoorBoundaryConditionActuators();
			GetEMSInput();
			GetEMSUserInput = false;
		}

		if ( ! GetZoneAirStatsInputFlag && ! ZoneThermostatActuatorsHaveBeenSetup ) {
			SetupThermostatActuators();
			ZoneThermostatActuatorsHaveBeenSetup = true;
		}

		// need to delay setup of HVAC actuator until after the systems input has been processed (if present)
		if ( FinishProcessingUserInput && ! DoingSizing && ! KickOffSimulation ) {
			SetupNodeSetPointsAsActuators();
			SetupPrimaryAirSystemAvailMgrAsActuators();
			//    CALL SetupWindowShadingControlActuators !this is too late for including in sizing, moved to GetEMSUserInput
			//    CALL SetupThermostatActuators !this is too late for including in sizing, moved to GetEMSUserInput
			//    CALL SetupSurfaceConvectionActuators !this is too late for including in sizing, moved to GetEMSUserInput
			FinishProcessingUserInput = false;
		}

		InitializeRuntimeLanguage();

		if ( ( BeginEnvrnFlag ) || ( iCalledFrom == emsCallFromZoneSizing ) || ( iCalledFrom == emsCallFromSystemSizing ) || ( iCalledFrom == emsCallFromUserDefinedComponentModel ) ) {

			// another pass at trying to setup input data.
			if ( FinishProcessingUserInput ) ProcessEMSInput( false );

			// update internal data variables being used by Erl
			for ( InternalVarUsedNum = 1; InternalVarUsedNum <= NumInternalVariablesUsed; ++InternalVarUsedNum ) {
				ErlVariableNum = EMSInternalVarsUsed( InternalVarUsedNum ).ErlVariableNum;
				InternVarAvailNum = EMSInternalVarsUsed( InternalVarUsedNum ).InternVarNum;
				if ( ! ( InternVarAvailNum > 0 ) ) continue; // sometimes executes before completely finished setting up.
				if ( ! ( ErlVariableNum > 0 ) ) continue;

				{ auto const SELECT_CASE_var( EMSInternalVarsAvailable( InternVarAvailNum ).PntrVarTypeUsed );

				if ( SELECT_CASE_var == PntrReal ) {

					ErlVariable( ErlVariableNum ).Value = SetErlValueNumber( EMSInternalVarsAvailable( InternVarAvailNum ).RealValue );

				} else if ( SELECT_CASE_var == PntrInteger ) {

					tmpReal = double( EMSInternalVarsAvailable( InternVarAvailNum ).IntValue );
					ErlVariable( ErlVariableNum ).Value = SetErlValueNumber( tmpReal );

				}}

			}

		}

		// Update sensors with current data
		for ( SensorNum = 1; SensorNum <= NumSensors; ++SensorNum ) {
			ErlVariableNum = Sensor( SensorNum ).VariableNum;
			if ( ( ErlVariableNum > 0 ) && ( Sensor( SensorNum ).Index > 0 ) ) {
				if ( Sensor( SensorNum ).SchedNum == 0 ) { // not a schedule so get from output processor

					ErlVariable( ErlVariableNum ).Value = SetErlValueNumber( GetInternalVariableValue( Sensor( SensorNum ).Type, Sensor( SensorNum ).Index ), ErlVariable( ErlVariableNum ).Value );
				} else { // schedule so use schedule service

					ErlVariable( ErlVariableNum ).Value = SetErlValueNumber( GetCurrentScheduleValue( Sensor( SensorNum ).SchedNum ), ErlVariable( ErlVariableNum ).Value );
				}
			}
		}

	}

	void
	ReportEMS()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   June 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates report variables.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// Using/Aliasing
		using RuntimeLanguageProcessor::ReportRuntimeLanguage;

		// SUBROUTINE ARGUMENT DEFINITIONS:
		//INTEGER, INTENT(IN) :: ListNum

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// FLOW:
		ReportRuntimeLanguage();

	}

	void
	GetEMSInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   June 2006
		//       MODIFIED       BG April 2009, finishing, renaming, etc.
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Gets the EMS input from the input file.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// Using/Aliasing
		using DataGlobals::AnyEnergyManagementSystemInModel;
		using DataGlobals::emsCallFromZoneSizing;
		using DataGlobals::emsCallFromSystemSizing;
		using DataGlobals::emsCallFromBeginNewEvironment;
		using DataGlobals::emsCallFromBeginNewEvironmentAfterWarmUp;
		using DataGlobals::emsCallFromBeginTimestepBeforePredictor;
		using DataGlobals::emsCallFromBeforeHVACManagers;
		using DataGlobals::emsCallFromAfterHVACManagers;
		using DataGlobals::emsCallFromHVACIterationLoop;
		using DataGlobals::emsCallFromEndZoneTimestepBeforeZoneReporting;
		using DataGlobals::emsCallFromEndZoneTimestepAfterZoneReporting;
		using DataGlobals::emsCallFromEndSystemTimestepBeforeHVACReporting;
		using DataGlobals::emsCallFromEndSystemTimestepAfterHVACReporting;
		using DataGlobals::emsCallFromComponentGetInput;
		using DataGlobals::emsCallFromUserDefinedComponentModel;
		using DataGlobals::emsCallFromUnitarySystemSizing;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::FindItemInList;
		using InputProcessor::SameString;
		using InputProcessor::GetObjectDefMaxArgs;
		//  USE OutputProcessor, ONLY: GetReportVarPointerForEMS
		//  USE DataIPShortCuts

		using RuntimeLanguageProcessor::InitializeRuntimeLanguage;
		using RuntimeLanguageProcessor::FindEMSVariable;
		using RuntimeLanguageProcessor::NewEMSVariable;
		using RuntimeLanguageProcessor::ExternalInterfaceInitializeErlVariable;
		using RuntimeLanguageProcessor::SetErlValueNumber;

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int StackNum;
		int SensorNum;
		int ActuatorNum;
		int ActuatorVariableNum;
		//  INTEGER                     :: ProgramNum
		int VariableNum; // local do loop index
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		int AlphaNum;
		int IOStat; // IO Status when calling get input subroutine
		//  CHARACTER(len=MaxNameLength), DIMENSION(99) :: AlphArray  ! Character string data  ! 99 should really be some kind of constant
		//  REAL(r64), DIMENSION(1)          :: NumArray  ! Numeric data
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		static bool ErrorsFound( false );
		//  CHARACTER(len=MaxNameLength)   :: objNameMsg = ' '
		Array1D_string cAlphaFieldNames;
		Array1D_string cNumericFieldNames;
		Array1D_bool lNumericFieldBlanks;
		Array1D_bool lAlphaFieldBlanks;
		Array1D_string cAlphaArgs;
		Array1D< Real64 > rNumericArgs;
		std::string cCurrentModuleObject;
		int VarType;
		int VarIndex;
		bool FoundObjectType;
		bool FoundObjectName;
		bool FoundActuatorName;
		int NumErlProgramsThisManager; // temporary size of Erl programs in EMSProgramCallManager
		int ManagerProgramNum; // index counter for Erl programs inside EMSProgramCallManager
		int CallManagerNum; // loop counter for EMSProgramCallManager structure
		int InternVarNum; // do loop counter for internal variables used (outer)
		int InternalVarAvailNum; // do loop counter for internal variables available (inner)
		int Loop; // do loop counter
		static int MaxNumAlphas( 0 ); // argument for call to GetObjectDefMaxArgs
		static int MaxNumNumbers( 0 ); // argument for call to GetObjectDefMaxArgs
		static int TotalArgs( 0 ); // argument for call to GetObjectDefMaxArgs
		bool errFlag;

		// FLOW:
		cCurrentModuleObject = "EnergyManagementSystem:Sensor";
		GetObjectDefMaxArgs( cCurrentModuleObject, TotalArgs, NumAlphas, NumNums );
		MaxNumNumbers = NumNums;
		MaxNumAlphas = NumAlphas;
		cCurrentModuleObject = "EnergyManagementSystem:Actuator";
		GetObjectDefMaxArgs( cCurrentModuleObject, TotalArgs, NumAlphas, NumNums );
		MaxNumNumbers = max( MaxNumNumbers, NumNums );
		MaxNumAlphas = max( MaxNumAlphas, NumAlphas );
		cCurrentModuleObject = "EnergyManagementSystem:ProgramCallingManager";
		GetObjectDefMaxArgs( cCurrentModuleObject, TotalArgs, NumAlphas, NumNums );
		MaxNumNumbers = max( MaxNumNumbers, NumNums );
		MaxNumAlphas = max( MaxNumAlphas, NumAlphas );
		cCurrentModuleObject = "EnergyManagementSystem:Program";
		GetObjectDefMaxArgs( cCurrentModuleObject, TotalArgs, NumAlphas, NumNums );
		MaxNumNumbers = max( MaxNumNumbers, NumNums );
		MaxNumAlphas = max( MaxNumAlphas, NumAlphas );
		cCurrentModuleObject = "EnergyManagementSystem:Subroutine";
		GetObjectDefMaxArgs( cCurrentModuleObject, TotalArgs, NumAlphas, NumNums );
		MaxNumNumbers = max( MaxNumNumbers, NumNums );
		MaxNumAlphas = max( MaxNumAlphas, NumAlphas );
		cCurrentModuleObject = "EnergyManagementSystem:OutputVariable";
		GetObjectDefMaxArgs( cCurrentModuleObject, TotalArgs, NumAlphas, NumNums );
		MaxNumNumbers = max( MaxNumNumbers, NumNums );
		MaxNumAlphas = max( MaxNumAlphas, NumAlphas );
		cCurrentModuleObject = "ExternalInterface:Variable";
		GetObjectDefMaxArgs( cCurrentModuleObject, TotalArgs, NumAlphas, NumNums );
		MaxNumNumbers = max( MaxNumNumbers, NumNums );
		MaxNumAlphas = max( MaxNumAlphas, NumAlphas );
		cCurrentModuleObject = "ExternalInterface:Actuator";
		GetObjectDefMaxArgs( cCurrentModuleObject, TotalArgs, NumAlphas, NumNums );
		MaxNumNumbers = max( MaxNumNumbers, NumNums );
		MaxNumAlphas = max( MaxNumAlphas, NumAlphas );
		//  cCurrentModuleObject = 'EnergyManagementSystem:Sensor'
		//  CALL GetObjectDefMaxArgs(cCurrentModuleObject,TotalArgs,NumAlphas,NumNums)
		//  MaxNumNumbers=MAX(MaxNumNumbers,NumNums)
		//  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)
		cCurrentModuleObject = "EnergyManagementSystem:GlobalVariable";
		GetObjectDefMaxArgs( cCurrentModuleObject, TotalArgs, NumAlphas, NumNums );
		MaxNumNumbers = max( MaxNumNumbers, NumNums );
		MaxNumAlphas = max( MaxNumAlphas, NumAlphas );

		cAlphaFieldNames.allocate( MaxNumAlphas );
		cAlphaArgs.allocate( MaxNumAlphas );
		lAlphaFieldBlanks.dimension( MaxNumAlphas, false );
		cNumericFieldNames.allocate( MaxNumNumbers );
		rNumericArgs.dimension( MaxNumNumbers, 0.0 );
		lNumericFieldBlanks.dimension( MaxNumNumbers, false );

		cCurrentModuleObject = "EnergyManagementSystem:Sensor";
		if ( NumSensors > 0 ) {
			Sensor.allocate( NumSensors );

			for ( SensorNum = 1; SensorNum <= NumSensors; ++SensorNum ) {
				GetObjectItem( cCurrentModuleObject, SensorNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

				IsNotOK = false;
				IsBlank = false;
				VerifyName( cAlphaArgs( 1 ), Sensor, SensorNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
				}

				ValidateEMSVariableName( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaFieldNames( 1 ), errFlag, ErrorsFound );
				if ( ! errFlag ) {
					Sensor( SensorNum ).Name = cAlphaArgs( 1 );

					// really needs to check for conflicts with program and function names too...done later
					VariableNum = FindEMSVariable( cAlphaArgs( 1 ), 0 );

					if ( VariableNum > 0 ) {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ShowContinueError( "Object name conflicts with a global variable name in EMS" );
						ErrorsFound = true;
					} else {
						VariableNum = NewEMSVariable( cAlphaArgs( 1 ), 0 );
						Sensor( SensorNum ).VariableNum = VariableNum;
					}
				}

				if ( cAlphaArgs( 2 ) == "*" ) cAlphaArgs( 2 ).clear();
				Sensor( SensorNum ).UniqueKeyName = cAlphaArgs( 2 );
				Sensor( SensorNum ).OutputVarName = cAlphaArgs( 3 );

				VarIndex = GetMeterIndex( cAlphaArgs( 3 ) );
				if ( VarIndex > 0 ) {
					if ( ! lAlphaFieldBlanks( 2 ) ) {
						ShowWarningError( "Unused" + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ShowContinueError( "Meter Name found; Key Name will be ignored" ); // why meters have no keys..
					} else {
						Sensor( SensorNum ).Type = 3;
						Sensor( SensorNum ).Index = VarIndex;
						Sensor( SensorNum ).CheckedOkay = true;
					}
				} else {
					// Search for variable names
					GetVariableTypeAndIndex( cAlphaArgs( 3 ), cAlphaArgs( 2 ), VarType, VarIndex );
					if ( VarType != 0 ) {
						Sensor( SensorNum ).Type = VarType;
						if ( VarIndex != 0 ) {
							Sensor( SensorNum ).Index = VarIndex;
							Sensor( SensorNum ).CheckedOkay = true;
						}
					}
				}

			} // SensorNum
		}

		cCurrentModuleObject = "EnergyManagementSystem:Actuator";

		if ( numActuatorsUsed + NumExternalInterfaceActuatorsUsed + NumExternalInterfaceFunctionalMockupUnitImportActuatorsUsed + NumExternalInterfaceFunctionalMockupUnitExportActuatorsUsed > 0 ) {
			EMSActuatorUsed.allocate( numActuatorsUsed + NumExternalInterfaceActuatorsUsed + NumExternalInterfaceFunctionalMockupUnitImportActuatorsUsed + NumExternalInterfaceFunctionalMockupUnitExportActuatorsUsed );
			for ( ActuatorNum = 1; ActuatorNum <= numActuatorsUsed + NumExternalInterfaceActuatorsUsed + NumExternalInterfaceFunctionalMockupUnitImportActuatorsUsed + NumExternalInterfaceFunctionalMockupUnitExportActuatorsUsed; ++ActuatorNum ) {
				// If we process the ExternalInterface actuators, all we need to do is to change the
				// name of the module object, and shift the ActuatorNum in GetObjectItem
				if ( ActuatorNum <= numActuatorsUsed ) {
					GetObjectItem( cCurrentModuleObject, ActuatorNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				} else if ( ActuatorNum > numActuatorsUsed && ActuatorNum <= numActuatorsUsed + NumExternalInterfaceActuatorsUsed ) {
					cCurrentModuleObject = "ExternalInterface:Actuator";
					GetObjectItem( cCurrentModuleObject, ActuatorNum - numActuatorsUsed, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				} else if ( ActuatorNum > numActuatorsUsed + NumExternalInterfaceActuatorsUsed && ActuatorNum <= ( numActuatorsUsed + NumExternalInterfaceActuatorsUsed + NumExternalInterfaceFunctionalMockupUnitImportActuatorsUsed ) ) {
					cCurrentModuleObject = "ExternalInterface:FunctionalMockupUnitImport:To:Actuator";
					GetObjectItem( cCurrentModuleObject, ActuatorNum - numActuatorsUsed - NumExternalInterfaceActuatorsUsed, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				} else if ( ActuatorNum > numActuatorsUsed + NumExternalInterfaceActuatorsUsed + NumExternalInterfaceFunctionalMockupUnitImportActuatorsUsed && ActuatorNum <= numActuatorsUsed + NumExternalInterfaceActuatorsUsed + NumExternalInterfaceFunctionalMockupUnitImportActuatorsUsed + NumExternalInterfaceFunctionalMockupUnitExportActuatorsUsed ) {
					cCurrentModuleObject = "ExternalInterface:FunctionalMockupUnitExport:To:Actuator";
					GetObjectItem( cCurrentModuleObject, ActuatorNum - numActuatorsUsed - NumExternalInterfaceActuatorsUsed - NumExternalInterfaceFunctionalMockupUnitImportActuatorsUsed, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				}

				IsNotOK = false;
				IsBlank = false;
				VerifyName( cAlphaArgs( 1 ), EMSActuatorUsed, ActuatorNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
				}

				ValidateEMSVariableName( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaFieldNames( 1 ), errFlag, ErrorsFound );
				if ( ! errFlag ) {
					EMSActuatorUsed( ActuatorNum ).Name = cAlphaArgs( 1 );

					// really needs to check for conflicts with program and function names too...
					VariableNum = FindEMSVariable( cAlphaArgs( 1 ), 0 );

					if ( VariableNum > 0 ) {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ShowContinueError( "Object name conflicts with a global variable name in EMS" );
						ErrorsFound = true;
					} else {
						VariableNum = NewEMSVariable( cAlphaArgs( 1 ), 0 );
						EMSActuatorUsed( ActuatorNum ).ErlVariableNum = VariableNum;
						if ( ActuatorNum > numActuatorsUsed ) {
							// Initialize variables for the ExternalInterface variables
							ExternalInterfaceInitializeErlVariable( VariableNum, SetErlValueNumber( rNumericArgs( 1 ) ), lNumericFieldBlanks( 1 ) );
						}
					}
				}

				// need to store characters to finish processing later (once available Actuators have all been setup)
				EMSActuatorUsed( ActuatorNum ).ComponentTypeName = cAlphaArgs( 3 );
				EMSActuatorUsed( ActuatorNum ).UniqueIDName = cAlphaArgs( 2 );
				EMSActuatorUsed( ActuatorNum ).ControlTypeName = cAlphaArgs( 4 );

				FoundObjectType = false;
				FoundObjectName = false;
				FoundActuatorName = false;
				for ( ActuatorVariableNum = 1; ActuatorVariableNum <= numEMSActuatorsAvailable; ++ActuatorVariableNum ) {
					if ( SameString( EMSActuatorAvailable( ActuatorVariableNum ).ComponentTypeName, cAlphaArgs( 3 ) ) ) {
						FoundObjectType = true;
						if ( SameString( EMSActuatorAvailable( ActuatorVariableNum ).UniqueIDName, cAlphaArgs( 2 ) ) ) {
							FoundObjectName = true;
							if ( SameString( EMSActuatorAvailable( ActuatorVariableNum ).ControlTypeName, cAlphaArgs( 4 ) ) ) {
								FoundActuatorName = true;
								break;
							}
						}
					}
				}

				if ( FoundActuatorName ) {
					EMSActuatorUsed( ActuatorNum ).ActuatorVariableNum = ActuatorVariableNum;
					EMSActuatorUsed( ActuatorNum ).CheckedOkay = true;
				}
			} // ActuatorNum
		}

		cCurrentModuleObject = "EnergyManagementSystem:InternalVariable";
		NumInternalVariablesUsed = GetNumObjectsFound( cCurrentModuleObject );
		if ( NumInternalVariablesUsed > 0 ) {
			EMSInternalVarsUsed.allocate( NumInternalVariablesUsed );

			for ( InternVarNum = 1; InternVarNum <= NumInternalVariablesUsed; ++InternVarNum ) {
				GetObjectItem( cCurrentModuleObject, InternVarNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

				IsNotOK = false;
				IsBlank = false;
				VerifyName( cAlphaArgs( 1 ), EMSInternalVarsUsed, InternVarNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
				}

				ValidateEMSVariableName( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaFieldNames( 1 ), errFlag, ErrorsFound );
				if ( ! errFlag ) {
					EMSInternalVarsUsed( InternVarNum ).Name = cAlphaArgs( 1 );
					VariableNum = FindEMSVariable( cAlphaArgs( 1 ), 0 );
					if ( VariableNum > 0 ) {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ShowContinueError( "Object name conflicts with a global variable name in EMS" );
						ErrorsFound = true;
					} else {
						VariableNum = NewEMSVariable( cAlphaArgs( 1 ), 0 );
						EMSInternalVarsUsed( InternVarNum ).ErlVariableNum = VariableNum;
					}

					EMSInternalVarsUsed( InternVarNum ).UniqueIDName = cAlphaArgs( 2 );
					EMSInternalVarsUsed( InternVarNum ).InternalDataTypeName = cAlphaArgs( 3 );

					FoundObjectType = false;
					FoundObjectName = false;
					for ( InternalVarAvailNum = 1; InternalVarAvailNum <= numEMSInternalVarsAvailable; ++InternalVarAvailNum ) {
						if ( SameString( EMSInternalVarsAvailable( InternalVarAvailNum ).DataTypeName, cAlphaArgs( 3 ) ) ) {
							FoundObjectType = true;
							if ( SameString( EMSInternalVarsAvailable( InternalVarAvailNum ).UniqueIDName, cAlphaArgs( 2 ) ) ) {
								FoundObjectName = true;
								break; // InternalVarAvailNum now holds needed index pointer
							}
						}
					}

					if ( FoundObjectName ) {
						EMSInternalVarsUsed( InternVarNum ).InternVarNum = InternalVarAvailNum;
						EMSInternalVarsUsed( InternVarNum ).CheckedOkay = true;
					}

				}
			}
		}

		InitializeRuntimeLanguage(); // Loads built-in globals and functions, then performs GetInput for runtime language objects

		if ( NumProgramCallManagers > 0 ) {
			cCurrentModuleObject = "EnergyManagementSystem:ProgramCallingManager";
			EMSProgramCallManager.allocate( NumProgramCallManagers );

			for ( CallManagerNum = 1; CallManagerNum <= NumProgramCallManagers; ++CallManagerNum ) {

				GetObjectItem( cCurrentModuleObject, CallManagerNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

				IsNotOK = false;
				IsBlank = false;
				VerifyName( cAlphaArgs( 1 ), EMSProgramCallManager, CallManagerNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
				}
				EMSProgramCallManager( CallManagerNum ).Name = cAlphaArgs( 1 );

				{ auto const SELECT_CASE_var( cAlphaArgs( 2 ) );

				if ( SELECT_CASE_var == "BEGINNEWENVIRONMENT" ) {
					EMSProgramCallManager( CallManagerNum ).CallingPoint = emsCallFromBeginNewEvironment;
				} else if ( SELECT_CASE_var == "AFTERNEWENVIRONMENTWARMUPISCOMPLETE" ) {
					EMSProgramCallManager( CallManagerNum ).CallingPoint = emsCallFromBeginNewEvironmentAfterWarmUp;
				} else if ( SELECT_CASE_var == "BEGINTIMESTEPBEFOREPREDICTOR" ) {
					EMSProgramCallManager( CallManagerNum ).CallingPoint = emsCallFromBeginTimestepBeforePredictor;
				} else if ( SELECT_CASE_var == "AFTERPREDICTORBEFOREHVACMANAGERS" ) {
					EMSProgramCallManager( CallManagerNum ).CallingPoint = emsCallFromBeforeHVACManagers;
				} else if ( SELECT_CASE_var == "AFTERPREDICTORAFTERHVACMANAGERS" ) {
					EMSProgramCallManager( CallManagerNum ).CallingPoint = emsCallFromAfterHVACManagers;
				} else if ( SELECT_CASE_var == "INSIDEHVACSYSTEMITERATIONLOOP" ) {
					EMSProgramCallManager( CallManagerNum ).CallingPoint = emsCallFromHVACIterationLoop;
				} else if ( SELECT_CASE_var == "ENDOFZONETIMESTEPBEFOREZONEREPORTING" ) {
					EMSProgramCallManager( CallManagerNum ).CallingPoint = emsCallFromEndZoneTimestepBeforeZoneReporting;
				} else if ( SELECT_CASE_var == "ENDOFZONETIMESTEPAFTERZONEREPORTING" ) {
					EMSProgramCallManager( CallManagerNum ).CallingPoint = emsCallFromEndZoneTimestepAfterZoneReporting;
				} else if ( SELECT_CASE_var == "ENDOFSYSTEMTIMESTEPBEFOREHVACREPORTING" ) {
					EMSProgramCallManager( CallManagerNum ).CallingPoint = emsCallFromEndSystemTimestepBeforeHVACReporting;
				} else if ( SELECT_CASE_var == "ENDOFSYSTEMTIMESTEPAFTERHVACREPORTING" ) {
					EMSProgramCallManager( CallManagerNum ).CallingPoint = emsCallFromEndSystemTimestepAfterHVACReporting;
				} else if ( SELECT_CASE_var == "ENDOFZONESIZING" ) {
					EMSProgramCallManager( CallManagerNum ).CallingPoint = emsCallFromZoneSizing;
				} else if ( SELECT_CASE_var == "ENDOFSYSTEMSIZING" ) {
					EMSProgramCallManager( CallManagerNum ).CallingPoint = emsCallFromSystemSizing;
				} else if ( SELECT_CASE_var == "AFTERCOMPONENTINPUTREADIN" ) {
					EMSProgramCallManager( CallManagerNum ).CallingPoint = emsCallFromComponentGetInput;
				} else if ( SELECT_CASE_var == "USERDEFINEDCOMPONENTMODEL" ) {
					EMSProgramCallManager( CallManagerNum ).CallingPoint = emsCallFromUserDefinedComponentModel;
				} else if ( SELECT_CASE_var == "UNITARYSYSTEMSIZING" ) {
					EMSProgramCallManager( CallManagerNum ).CallingPoint = emsCallFromUnitarySystemSizing;
				} else {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}}

				NumErlProgramsThisManager = NumAlphas - 2;
				EMSProgramCallManager( CallManagerNum ).NumErlPrograms = NumErlProgramsThisManager;
				EMSProgramCallManager( CallManagerNum ).ErlProgramARR.allocate( NumErlProgramsThisManager );
				ManagerProgramNum = 0;
				for ( AlphaNum = 3; AlphaNum <= NumAlphas; ++AlphaNum ) {
					// find program name in Stack structure
					if ( lAlphaFieldBlanks( AlphaNum ) ) { // throw error
						ShowSevereError( "Invalid " + cAlphaFieldNames( AlphaNum ) + '=' + cAlphaArgs( AlphaNum ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ShowContinueError( "Program names cannot be blank" );
						ErrorsFound = true;
					}

					StackNum = FindItemInList( cAlphaArgs( AlphaNum ), ErlStack );

					if ( StackNum > 0 ) { // found it
						// check for duplicate and warn.
						for ( Loop = 1; Loop <= ManagerProgramNum; ++Loop ) {
							if ( EMSProgramCallManager( CallManagerNum ).ErlProgramARR( Loop ) == StackNum ) {
								ShowWarningError( "Duplicate " + cAlphaFieldNames( AlphaNum ) + '=' + cAlphaArgs( AlphaNum ) );
								ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
								ShowContinueError( "Erl program appears more than once, and the simulation continues." );
							}
						}

						++ManagerProgramNum;

						EMSProgramCallManager( CallManagerNum ).ErlProgramARR( ManagerProgramNum ) = StackNum;

					} else {
						ShowSevereError( "Invalid " + cAlphaFieldNames( AlphaNum ) + '=' + cAlphaArgs( AlphaNum ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ShowContinueError( "Program Name not found." );
						ErrorsFound = true;
					}
				} // AlphaNum

			}

		} else { // no program calling manager in input
			if ( NumErlPrograms > 0 ) {
				cCurrentModuleObject = "EnergyManagementSystem:ProgramCallingManager";
				ShowWarningError( "Energy Management System is missing input object " + cCurrentModuleObject );
				ShowContinueError( "EnergyPlus Runtime Language programs need a calling manager to control when they get executed" );

			}

		}

		cAlphaFieldNames.deallocate();
		cAlphaArgs.deallocate();
		lAlphaFieldBlanks.deallocate();
		cNumericFieldNames.deallocate();
		rNumericArgs.deallocate();
		lNumericFieldBlanks.deallocate();

		if ( ErrorsFound ) {
			ShowFatalError( "Errors found in getting Energy Management System input. Preceding condition causes termination." );
		}

	}

	void
	ProcessEMSInput( bool const reportErrors ) // .  If true, then report out errors ,otherwise setup what we can
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   May 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// contains Some input checks that need to be deferred until later in the simulation

		// METHODOLOGY EMPLOYED:
		// Loop over objects doing input checks.
		// Had to break up get user input into two phases because
		// the actuators can't be set up until all the HVAC systems are read in, sized, etc.
		// but we also want to allow customizing sizing calcs which occur much earlier in the simulation.
		//  so here we do a final pass and throw the errors that would usually occur during get input.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		//  USE DataIPShortCuts, ONLY: cCurrentModuleObject
		// Using/Aliasing
		using InputProcessor::SameString;
		using RuntimeLanguageProcessor::BeginEnvrnInitializeRuntimeLanguage;
		using ScheduleManager::GetScheduleIndex;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int SensorNum; // local loop
		//  INTEGER :: VariableNum  ! local do loop index
		int VarIndex;
		int VarType;
		static bool ErrorsFound( false );
		int ActuatorNum;
		bool FoundObjectType;
		bool FoundObjectName;
		bool FoundActuatorName;
		int ActuatorVariableNum;
		int InternVarNum; // local do loop index
		int InternalVarAvailNum; // local do loop index
		std::string cCurrentModuleObject;

		cCurrentModuleObject = "EnergyManagementSystem:Sensor";
		for ( SensorNum = 1; SensorNum <= NumSensors; ++SensorNum ) {
			if ( Sensor( SensorNum ).CheckedOkay ) continue;

			// try again to process sensor.
			VarIndex = GetMeterIndex( Sensor( SensorNum ).OutputVarName );
			if ( VarIndex > 0 ) {

				Sensor( SensorNum ).Type = 3;
				Sensor( SensorNum ).Index = VarIndex;

			} else {
				// Search for variable names
				GetVariableTypeAndIndex( Sensor( SensorNum ).OutputVarName, Sensor( SensorNum ).UniqueKeyName, VarType, VarIndex );
				if ( VarType == 0 ) {
					if ( reportErrors ) {
						ShowSevereError( "Invalid Output:Variable or Output:Meter Name =" + Sensor( SensorNum ).OutputVarName );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + Sensor( SensorNum ).Name );
						ShowContinueError( "Output:Variable Name not found" );
						ErrorsFound = true;
					}
				} else if ( VarIndex == 0 ) {
					if ( reportErrors ) {
						ShowSevereError( "Invalid Output:Variable or Output:Meter Index Key Name =" + Sensor( SensorNum ).UniqueKeyName );
						ShowContinueError( "For Output:Variable or Output:Meter = " + Sensor( SensorNum ).OutputVarName );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + Sensor( SensorNum ).Name );
						ShowContinueError( "Unique Key Name not found." );
						ErrorsFound = true;
					}
				} else {
					Sensor( SensorNum ).Type = VarType;
					Sensor( SensorNum ).Index = VarIndex;
					Sensor( SensorNum ).CheckedOkay = true;
					// If variable is Schedule Value, then get the schedule id to register it as being used
					if ( SameString( Sensor( SensorNum ).OutputVarName, "Schedule Value" ) ) {
						Sensor( SensorNum ).SchedNum = GetScheduleIndex( Sensor( SensorNum ).UniqueKeyName );
						if ( Sensor( SensorNum ).SchedNum == 0 ) {
							Sensor( SensorNum ).CheckedOkay = false;
							if ( reportErrors ) {
								ShowSevereError( "Invalid Output:Variable or Output:Meter Index Key Name =" + Sensor( SensorNum ).UniqueKeyName );
								ShowContinueError( "For Output:Variable or Output:Meter = " + Sensor( SensorNum ).OutputVarName );
								ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + Sensor( SensorNum ).Name );
								ShowContinueError( "Schedule Name not found." );
								ErrorsFound = true;
							}
						}
					}

				}
			}

		} // SensorNum

		// added for FMU
		for ( ActuatorNum = 1; ActuatorNum <= numActuatorsUsed + NumExternalInterfaceActuatorsUsed + NumExternalInterfaceFunctionalMockupUnitImportActuatorsUsed + NumExternalInterfaceFunctionalMockupUnitExportActuatorsUsed; ++ActuatorNum ) {
			// If we process the ExternalInterface actuators, all we need to do is to change the

			if ( ActuatorNum <= numActuatorsUsed ) {
				cCurrentModuleObject = "EnergyManagementSystem:Actuator";
			} else if ( ActuatorNum > numActuatorsUsed && ActuatorNum <= numActuatorsUsed + NumExternalInterfaceActuatorsUsed ) {
				cCurrentModuleObject = "ExternalInterface:Actuator";
			} else if ( ActuatorNum > numActuatorsUsed + NumExternalInterfaceActuatorsUsed && ActuatorNum <= numActuatorsUsed + NumExternalInterfaceActuatorsUsed + NumExternalInterfaceFunctionalMockupUnitImportActuatorsUsed ) {
				cCurrentModuleObject = "ExternalInterface:FunctionalMockupUnitImport:To:Actuator";
			} else if ( ActuatorNum > numActuatorsUsed + NumExternalInterfaceActuatorsUsed + NumExternalInterfaceFunctionalMockupUnitImportActuatorsUsed && ActuatorNum <= numActuatorsUsed + NumExternalInterfaceActuatorsUsed + NumExternalInterfaceFunctionalMockupUnitImportActuatorsUsed + NumExternalInterfaceFunctionalMockupUnitExportActuatorsUsed ) {
				cCurrentModuleObject = "ExternalInterface:FunctionalMockupUnitExport:To:Actuator";
			}

			if ( EMSActuatorUsed( ActuatorNum ).CheckedOkay ) continue;
			FoundObjectType = false;
			FoundObjectName = false;
			FoundActuatorName = false;
			for ( ActuatorVariableNum = 1; ActuatorVariableNum <= numEMSActuatorsAvailable; ++ActuatorVariableNum ) {
				if ( SameString( EMSActuatorAvailable( ActuatorVariableNum ).ComponentTypeName, EMSActuatorUsed( ActuatorNum ).ComponentTypeName ) ) {
					FoundObjectType = true;
					if ( SameString( EMSActuatorAvailable( ActuatorVariableNum ).UniqueIDName, EMSActuatorUsed( ActuatorNum ).UniqueIDName ) ) {
						FoundObjectName = true;
						if ( SameString( EMSActuatorAvailable( ActuatorVariableNum ).ControlTypeName, EMSActuatorUsed( ActuatorNum ).ControlTypeName ) ) {
							FoundActuatorName = true;
							break;
						}
					}
				}
			}

			if ( ! FoundObjectType ) {
				if ( reportErrors ) {
					ShowSevereError( "Invalid Actuated Component Type =" + EMSActuatorUsed( ActuatorNum ).ComponentTypeName );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + EMSActuatorUsed( ActuatorNum ).Name );
					ShowContinueError( "Component Type not found" );
					if ( OutputEDDFile ) {
						ShowContinueError( "Review .edd file for valid component types." );
					} else {
						ShowContinueError( "Use Output:EnergyManagementSystem object to create .edd file for valid component types." );
					}
					ErrorsFound = true;
				}
			}

			if ( ! FoundObjectName ) {
				if ( reportErrors ) {
					ShowSevereError( "Invalid Actuated Component Unique Name =" + EMSActuatorUsed( ActuatorNum ).UniqueIDName );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + EMSActuatorUsed( ActuatorNum ).Name );
					ShowContinueError( "Component Unique key name not found " );
					if ( OutputEDDFile ) {
						ShowContinueError( "Review edd file for valid component names." );
					} else {
						ShowContinueError( "Use Output:EnergyManagementSystem object to create .edd file for valid component names." );
					}
					ErrorsFound = true;
				}
			}

			if ( ! FoundActuatorName ) {
				if ( reportErrors ) {
					ShowSevereError( "Invalid Actuated Component Control Type =" + EMSActuatorUsed( ActuatorNum ).ControlTypeName );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + EMSActuatorUsed( ActuatorNum ).Name );
					ShowContinueError( "Control Type not found" );
					if ( OutputEDDFile ) {
						ShowContinueError( "Review edd file for valid component control types." );
					} else {
						ShowContinueError( "Use Output:EnergyManagementSystem object to create .edd file for valid component control types." );
					}
					ErrorsFound = true;
				}
			} else {
				EMSActuatorUsed( ActuatorNum ).ActuatorVariableNum = ActuatorVariableNum;
				EMSActuatorUsed( ActuatorNum ).CheckedOkay = true;
			}
		} // ActuatorNum

		cCurrentModuleObject = "EnergyManagementSystem:InternalVariable";
		for ( InternVarNum = 1; InternVarNum <= NumInternalVariablesUsed; ++InternVarNum ) {
			if ( EMSInternalVarsUsed( InternVarNum ).CheckedOkay ) continue;
			FoundObjectType = false;
			FoundObjectName = false;
			for ( InternalVarAvailNum = 1; InternalVarAvailNum <= numEMSInternalVarsAvailable; ++InternalVarAvailNum ) {
				if ( SameString( EMSInternalVarsAvailable( InternalVarAvailNum ).DataTypeName, EMSInternalVarsUsed( InternVarNum ).InternalDataTypeName ) ) {
					FoundObjectType = true;
					if ( SameString( EMSInternalVarsAvailable( InternalVarAvailNum ).UniqueIDName, EMSInternalVarsUsed( InternVarNum ).UniqueIDName ) ) {
						FoundObjectName = true;
						break; // InternalVarAvailNum now holds needed index pointer
					}
				}
			}

			if ( ! FoundObjectType ) {
				if ( reportErrors ) {
					ShowSevereError( "Invalid Internal Data Type =" + EMSInternalVarsUsed( InternVarNum ).InternalDataTypeName );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + EMSInternalVarsUsed( InternVarNum ).Name );
					ShowContinueError( "Internal data type name not found" );
					ErrorsFound = true;
				}
			}

			if ( ! FoundObjectName ) {
				if ( reportErrors ) {
					ShowSevereError( "Invalid Internal Data Index Key Name =" + EMSInternalVarsUsed( InternVarNum ).UniqueIDName );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + EMSInternalVarsUsed( InternVarNum ).Name );
					ShowContinueError( "Internal data unique identifier not found" );
					ErrorsFound = true;
				}
			} else {
				EMSInternalVarsUsed( InternVarNum ).InternVarNum = InternalVarAvailNum;
				EMSInternalVarsUsed( InternVarNum ).CheckedOkay = true;
			}

		}
		if ( reportErrors ) {
			EchoOutActuatorKeyChoices();
			EchoOutInternalVariableChoices();
		}

		if ( ErrorsFound ) {
			ShowFatalError( "Errors found in processing Energy Management System input. Preceding condition causes termination." );
		}

		if ( reportErrors ) {
			BeginEnvrnInitializeRuntimeLanguage();
		}

	}

	void
	GetVariableTypeAndIndex(
		std::string const & VarName,
		std::string const & VarKeyName,
		int & VarType,
		int & VarIndex
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   June 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// local helper routine intended to lookup report variables only.
		//    Use GetMeterIndex for meters.

		// METHODOLOGY EMPLOYED:
		// make calls to OutputProcessor methods GetVariableKeyCountandType and GetVariableKeys

		// USE STATEMENTS:

		// Using/Aliasing
		using RuntimeLanguageProcessor::EvaluateStack;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumKeys;
		int KeyNum;
		int AvgOrSum;
		int StepType;
		std::string Units;
		Array1D_string KeyName;
		Array1D_int KeyIndex;
		bool Found;

		// FLOW:
		VarType = 0;
		VarIndex = 0;
		Found = false;
		GetVariableKeyCountandType( VarName, NumKeys, VarType, AvgOrSum, StepType, Units );

		// note that schedules are not getting VarType set right...

		if ( NumKeys > 0 ) {
			KeyName.allocate( NumKeys );
			KeyIndex.allocate( NumKeys );
			GetVariableKeys( VarName, VarType, KeyName, KeyIndex );

			if ( KeyName( 1 ) == "ENVIRONMENT" ) {
				VarIndex = KeyIndex( 1 );
			} else {
				for ( KeyNum = 1; KeyNum <= NumKeys; ++KeyNum ) {
					if ( KeyName( KeyNum ) == VarKeyName ) {
						Found = true;
						break;
					}
				}
				if ( Found ) VarIndex = KeyIndex( KeyNum );
			}

			KeyName.deallocate();
			KeyIndex.deallocate();
		}

	}

	void
	EchoOutActuatorKeyChoices()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   April 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// echo out actuators registered with SetupEMSActuator for user access

		// METHODOLOGY EMPLOYED:
		// mine structure and write to edd file
		// note this executes after final processing and sizing-related calling points may already execute Erl programs

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static gio::Fmt fmtA( "(A)" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		if ( OutputEMSActuatorAvailFull ) {

			gio::write( OutputEMSFileUnitNum, fmtA ) << "! <EnergyManagementSystem:Actuator Available>, Component Unique Name, Component Type,  Control Type, Units";
			for ( int ActuatorLoop = 1; ActuatorLoop <= numEMSActuatorsAvailable; ++ActuatorLoop ) {
				gio::write( OutputEMSFileUnitNum, fmtA ) << "EnergyManagementSystem:Actuator Available," + EMSActuatorAvailable( ActuatorLoop ).UniqueIDName + ',' + EMSActuatorAvailable( ActuatorLoop ).ComponentTypeName + ',' + EMSActuatorAvailable( ActuatorLoop ).ControlTypeName + ',' + EMSActuatorAvailable( ActuatorLoop ).Units;
			}
		} else if ( OutputEMSActuatorAvailSmall ) {
			gio::write( OutputEMSFileUnitNum, fmtA ) << "! <EnergyManagementSystem:Actuator Available>, *, Component Type, Control Type, Units";
			int FoundTypeName;
			int FoundControlType;
			for ( int ActuatorLoop = 1; ActuatorLoop <= numEMSActuatorsAvailable; ++ActuatorLoop ) {
				if ( ActuatorLoop + 1 <= numEMSActuatorsAvailable ) {
					FoundTypeName = FindItemInList( EMSActuatorAvailable( ActuatorLoop ).ComponentTypeName, EMSActuatorAvailable( {ActuatorLoop + 1,numEMSActuatorsAvailable} ), &EMSActuatorAvailableType::ComponentTypeName, numEMSActuatorsAvailable - ( ActuatorLoop + 1 ) );
					FoundControlType = FindItemInList( EMSActuatorAvailable( ActuatorLoop ).ControlTypeName, EMSActuatorAvailable( {ActuatorLoop + 1,numEMSActuatorsAvailable} ), &EMSActuatorAvailableType::ControlTypeName, numEMSActuatorsAvailable - ( ActuatorLoop + 1 ) );
				} else {
					FoundTypeName = 1;
					FoundControlType = 1;
				}
				if ( ( FoundTypeName == 0 ) || ( FoundControlType == 0 ) ) {
					gio::write( OutputEMSFileUnitNum, fmtA ) << "EnergyManagementSystem:Actuator Available, *," + EMSActuatorAvailable( ActuatorLoop ).ComponentTypeName + ',' + EMSActuatorAvailable( ActuatorLoop ).ControlTypeName + ',' + EMSActuatorAvailable( ActuatorLoop ).Units;
				}
			}
		}

	}

	void
	EchoOutInternalVariableChoices()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   April 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// echo out actuators registered with SetupEMSActuator for user access

		// METHODOLOGY EMPLOYED:
		// mine structure and write to eio file

		// REFERENCES:
		// na

		// USE STATEMENTS
		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static gio::Fmt fmtA( "(A)" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		if ( OutputEMSInternalVarsFull ) {

			gio::write( OutputEMSFileUnitNum, fmtA ) << "! <EnergyManagementSystem:InternalVariable Available>, Unique Name, Internal Data Type, Units ";
			for ( int InternalDataLoop = 1; InternalDataLoop <= numEMSInternalVarsAvailable; ++InternalDataLoop ) {
				gio::write( OutputEMSFileUnitNum, fmtA ) << "EnergyManagementSystem:InternalVariable Available," + EMSInternalVarsAvailable( InternalDataLoop ).UniqueIDName + ',' + EMSInternalVarsAvailable( InternalDataLoop ).DataTypeName + ',' + EMSInternalVarsAvailable( InternalDataLoop ).Units;
			}

		} else if ( OutputEMSInternalVarsSmall ) {
			gio::write( OutputEMSFileUnitNum, fmtA ) << "! <EnergyManagementSystem:InternalVariable Available>, *, Internal Data Type";
			for ( int InternalDataLoop = 1; InternalDataLoop <= numEMSInternalVarsAvailable; ++InternalDataLoop ) {
				int Found( 0 );
				if ( InternalDataLoop + 1 <= numEMSInternalVarsAvailable ) {
					Found = FindItemInList( EMSInternalVarsAvailable( InternalDataLoop ).DataTypeName, EMSInternalVarsAvailable( {InternalDataLoop + 1,numEMSInternalVarsAvailable} ), &InternalVarsAvailableType::DataTypeName, numEMSInternalVarsAvailable - ( InternalDataLoop + 1 ) );
				}
				if ( Found == 0 ) {
					gio::write( OutputEMSFileUnitNum, fmtA ) << "EnergyManagementSystem:InternalVariable Available, *," + EMSInternalVarsAvailable( InternalDataLoop ).DataTypeName + ',' + EMSInternalVarsAvailable( InternalDataLoop ).Units;
				}
			}
		}

	}

	void
	SetupNodeSetPointsAsActuators()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   May 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// make system nodes in model available for EMS control

		// METHODOLOGY EMPLOYED:
		// Loop over node structures and make calls to SetupEMSActuator
		// the pattern for the basic node setpoints is a little different in that the actuators directly
		// affect the node variables, rather than using seperate logical override flag and ems values

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataLoopNode::Node;
		using DataLoopNode::NodeID;
		using DataLoopNode::NumOfNodes;
		using OutAirNodeManager::NumOutsideAirNodes;
		using OutAirNodeManager::OutsideAirNodeList;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int LoopNode; // local do loop index
		static bool lDummy; // not going to setup a pointer to logical control //Fix Changed to static: Passed to SetupEMSActuator as source of persistent Reference
		// (could this ever cause a fault?) // It caused illegal memory access/corruption
		// make it optional in Setup call?
		int OutsideAirNodeNum; // local do loop index
		int NodeNum; // local index.

		lDummy = false;

		if ( NumOfNodes > 0 ) {

			for ( LoopNode = 1; LoopNode <= NumOfNodes; ++LoopNode ) {
				// setup the setpoint for each type of variable that can be controlled
				SetupEMSActuator( "System Node Setpoint", NodeID( LoopNode ), "Temperature Setpoint", "[C]", lDummy, Node( LoopNode ).TempSetPoint );
				SetupEMSActuator( "System Node Setpoint", NodeID( LoopNode ), "Temperature Minimum Setpoint", "[C]", lDummy, Node( LoopNode ).TempSetPointLo );
				SetupEMSActuator( "System Node Setpoint", NodeID( LoopNode ), "Temperature Maximum Setpoint", "[C]", lDummy, Node( LoopNode ).TempSetPointHi );
				SetupEMSActuator( "System Node Setpoint", NodeID( LoopNode ), "Humidity Ratio Setpoint", "[kgWater/kgDryAir]", lDummy, Node( LoopNode ).HumRatSetPoint );
				SetupEMSActuator( "System Node Setpoint", NodeID( LoopNode ), "Humidity Ratio Maximum Setpoint", "[kgWater/kgDryAir]", lDummy, Node( LoopNode ).HumRatMax );
				SetupEMSActuator( "System Node Setpoint", NodeID( LoopNode ), "Humidity Ratio Minimum Setpoint", "[kgWater/kgDryAir]", lDummy, Node( LoopNode ).HumRatMin );
				SetupEMSActuator( "System Node Setpoint", NodeID( LoopNode ), "Mass Flow Rate Setpoint", "[kg/s]", lDummy, Node( LoopNode ).MassFlowRateSetPoint );
				SetupEMSActuator( "System Node Setpoint", NodeID( LoopNode ), "Mass Flow Rate Maximum Available Setpoint", "[kg/s]", lDummy, Node( LoopNode ).MassFlowRateMaxAvail );
				SetupEMSActuator( "System Node Setpoint", NodeID( LoopNode ), "Mass Flow Rate Minimum Available Setpoint", "[kg/s]", lDummy, Node( LoopNode ).MassFlowRateMinAvail );
			}

		} // NumOfNodes > 0

		if ( NumOutsideAirNodes > 0 ) {
			for ( OutsideAirNodeNum = 1; OutsideAirNodeNum <= NumOutsideAirNodes; ++OutsideAirNodeNum ) {
				NodeNum = OutsideAirNodeList( OutsideAirNodeNum );
				SetupEMSActuator( "Outdoor Air System Node", NodeID( NodeNum ), "Drybulb Temperature", "[C]", Node( NodeNum ).EMSOverrideOutAirDryBulb, Node( NodeNum ).EMSValueForOutAirDryBulb );
				SetupEMSActuator( "Outdoor Air System Node", NodeID( NodeNum ), "Wetbulb Temperature", "[C]", Node( NodeNum ).EMSOverrideOutAirWetBulb, Node( NodeNum ).EMSValueForOutAirWetBulb );
			}
		}

	}

	void
	UpdateEMSTrendVariables()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   May 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Logged trend data

		// METHODOLOGY EMPLOYED:
		// Store current value of Erl Variable in Trend stack
		// Trend arrays are pushed so that the latest value is
		//  always at index 1.  old values get lost.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::AnyEnergyManagementSystemInModel;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na
		static int TrendNum( 0 ); // local loop counter
		static int ErlVarNum( 0 );
		static int TrendDepth( 0 );
		static Real64 currentVal( 0.0 );

		// checks with quick return if no updates needed.
		if ( ! AnyEnergyManagementSystemInModel ) return;
		if ( NumErlTrendVariables == 0 ) return;

		for ( TrendNum = 1; TrendNum <= NumErlTrendVariables; ++TrendNum ) {
			ErlVarNum = TrendVariable( TrendNum ).ErlVariablePointer;
			TrendDepth = TrendVariable( TrendNum ).LogDepth;
			if ( ( ErlVarNum > 0 ) && ( TrendDepth > 0 ) ) {
				currentVal = ErlVariable( ErlVarNum ).Value.Number;
				// push into trend
				TrendVariable( TrendNum ).tempTrendARR = TrendVariable( TrendNum ).TrendValARR;
				TrendVariable( TrendNum ).TrendValARR( 1 ) = currentVal;
				TrendVariable( TrendNum ).TrendValARR( {2,TrendDepth} ) = TrendVariable( TrendNum ).tempTrendARR( {1,TrendDepth - 1} );

			}
		}

	}

	void
	CheckIfNodeSetPointManagedByEMS(
		int const NodeNum, // index of node being checked.
		int const SetPointType,
		bool & ErrorFlag
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   May 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Provide method to verify that a specific node is (probably) managed by EMS

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::SameString;
		using DataLoopNode::NodeID;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static int Loop( 0 ); // local do loop index
		std::string cControlTypeName;
		std::string cComponentTypeName;
		std::string cNodeName;
		static bool FoundControl( false );

		FoundControl = false;

		cNodeName = NodeID( NodeNum );
		cComponentTypeName = "System Node Setpoint";
		{ auto const SELECT_CASE_var( SetPointType );

		if ( SELECT_CASE_var == iTemperatureSetPoint ) {
			cControlTypeName = "Temperature Setpoint";
		} else if ( SELECT_CASE_var == iTemperatureMinSetPoint ) {
			cControlTypeName = "Temperature Minimum Setpoint";
		} else if ( SELECT_CASE_var == iTemperatureMaxSetPoint ) {
			cControlTypeName = "Temperature Maximum Setpoint";
		} else if ( SELECT_CASE_var == iHumidityRatioSetPoint ) {
			cControlTypeName = "Humidity Ratio Setpoint";
		} else if ( SELECT_CASE_var == iHumidityRatioMinSetPoint ) {
			cControlTypeName = "Humidity Ratio Minimum Setpoint";
		} else if ( SELECT_CASE_var == iHumidityRatioMaxSetPoint ) {
			cControlTypeName = "Humidity Ratio Maximum Setpoint";
		} else if ( SELECT_CASE_var == iMassFlowRateSetPoint ) {
			cControlTypeName = "Mass Flow Rate Setpoint";
		} else if ( SELECT_CASE_var == iMassFlowRateMinSetPoint ) {
			cControlTypeName = "Mass Flow Rate Minimum Available Setpoint";
		} else if ( SELECT_CASE_var == iMassFlowRateMaxSetPoint ) {
			cControlTypeName = "Mass Flow Rate Maximum Available Setpoint";
		}}

		for ( Loop = 1; Loop <= numActuatorsUsed + NumExternalInterfaceActuatorsUsed; ++Loop ) {
			if ( ( SameString( EMSActuatorUsed( Loop ).ComponentTypeName, cComponentTypeName ) ) && ( SameString( EMSActuatorUsed( Loop ).UniqueIDName, cNodeName ) ) && ( SameString( EMSActuatorUsed( Loop ).ControlTypeName, cControlTypeName ) ) ) {
				FoundControl = true;
			}

		}

		if ( ( ! ErrorFlag ) && ( ! FoundControl ) ) ErrorFlag = true;

	}

	bool
	CheckIfNodeMoreInfoSensedByEMS(
		int const nodeNum, // index of node being checked.
		std::string const & varName
	) {
	bool returnValue;

	returnValue = false;
	for (auto loop = 1; loop <= NumSensors; ++loop ) {
		if ( Sensor( loop ).UniqueKeyName == DataLoopNode::NodeID( nodeNum ) && InputProcessor::SameString(Sensor( loop ).OutputVarName ,varName) ) {
			returnValue = true;
		}
	}

	return returnValue;
	}

	void
	SetupPrimaryAirSystemAvailMgrAsActuators()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   May 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// make air system status available as EMS actuator

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataAirLoop::PriAirSysAvailMgr;
		using DataAirSystems::PrimaryAirSystem;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static int numAirLoops( 0 );
		static int Loop( 0 );
		static bool lDummy; //Fix Changed to static: Passed to SetupEMSActuator as source of persistent Reference

		lDummy = false;

		if ( allocated( PriAirSysAvailMgr ) ) {
			numAirLoops = isize( PriAirSysAvailMgr );
			for ( Loop = 1; Loop <= numAirLoops; ++Loop ) {
				SetupEMSActuator( "AirLoopHVAC", PrimaryAirSystem( Loop ).Name, "Availability Status", "[ ]", lDummy, PriAirSysAvailMgr( Loop ).AvailStatus );

			}

		} else {

		}

	}

	void
	SetupWindowShadingControlActuators()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   May 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// make calls to SetupEMSactuator for public data for Window Shades

		// METHODOLOGY EMPLOYED:
		// Loop thru SurfaceWindow and register any shading controls

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataSurfaces::Surface;
		using DataSurfaces::SurfaceWindow;
		using DataSurfaces::TotSurfaces;
		using DataSurfaces::SurfaceClass_Window;
		using DataSurfaces::ExternalEnvironment;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static int loopSurfNum( 0 ); // local do loop index

		for ( loopSurfNum = 1; loopSurfNum <= TotSurfaces; ++loopSurfNum ) {

			if ( Surface( loopSurfNum ).Class != SurfaceClass_Window ) continue;
			if ( Surface( loopSurfNum ).ExtBoundCond != ExternalEnvironment ) continue;
			if ( Surface( loopSurfNum ).WindowShadingControlPtr == 0 ) continue;

			SetupEMSActuator( "Window Shading Control", Surface( loopSurfNum ).Name, "Control Status", "[ShadeStatus]", SurfaceWindow( loopSurfNum ).ShadingFlagEMSOn, SurfaceWindow( loopSurfNum ).ShadingFlagEMSValue );

			if ( SurfaceWindow( loopSurfNum ).MovableSlats ) {
				SetupEMSActuator( "Window Shading Control", Surface( loopSurfNum ).Name, "Slat Angle", "[degrees]", SurfaceWindow( loopSurfNum ).SlatAngThisTSDegEMSon, SurfaceWindow( loopSurfNum ).SlatAngThisTSDegEMSValue );

			}

		}

	}

	void
	SetupThermostatActuators()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   May 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Make zone thermostats, humidistats, and comfort controls available to EMS

		// METHODOLOGY EMPLOYED:
		// Loop over structures and call SetupEMSactuator for public data in DataZoneControls.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataZoneControls;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static int Loop( 0 ); // local do loop index

		for ( Loop = 1; Loop <= NumTempControlledZones; ++Loop ) {
			SetupEMSActuator( "Zone Temperature Control", TempControlledZone( Loop ).ZoneName, "Heating Setpoint", "[C]", TempControlledZone( Loop ).EMSOverrideHeatingSetPointOn, TempControlledZone( Loop ).EMSOverrideHeatingSetPointValue );
			SetupEMSActuator( "Zone Temperature Control", TempControlledZone( Loop ).ZoneName, "Cooling Setpoint", "[C]", TempControlledZone( Loop ).EMSOverrideCoolingSetPointOn, TempControlledZone( Loop ).EMSOverrideCoolingSetPointValue );
		}

		for ( Loop = 1; Loop <= NumHumidityControlZones; ++Loop ) {
			SetupEMSActuator( "Zone Humidity Control", HumidityControlZone( Loop ).ZoneName, "Relative Humidity Humidifying Setpoint", "[%]", HumidityControlZone( Loop ).EMSOverrideHumidifySetPointOn, HumidityControlZone( Loop ).EMSOverrideHumidifySetPointValue );
			SetupEMSActuator( "Zone Humidity Control", HumidityControlZone( Loop ).ZoneName, "Relative Humidity Dehumidifying Setpoint", "[%]", HumidityControlZone( Loop ).EMSOverrideDehumidifySetPointOn, HumidityControlZone( Loop ).EMSOverrideDehumidifySetPointValue );
		}

		for ( Loop = 1; Loop <= NumComfortControlledZones; ++Loop ) {
			SetupEMSActuator( "Zone Comfort Control", ComfortControlledZone( Loop ).ZoneName, "Heating Setpoint", "[]", ComfortControlledZone( Loop ).EMSOverrideHeatingSetPointOn, ComfortControlledZone( Loop ).EMSOverrideHeatingSetPointValue );
			SetupEMSActuator( "Zone Comfort Control", ComfortControlledZone( Loop ).ZoneName, "Cooling Setpoint", "[]", ComfortControlledZone( Loop ).EMSOverrideCoolingSetPointOn, ComfortControlledZone( Loop ).EMSOverrideCoolingSetPointValue );
		}

	}

	void
	SetupSurfaceConvectionActuators()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   May 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Setup EMS actuators available for surface convection coefficients

		// METHODOLOGY EMPLOYED:
		// access public data and loop over it.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataSurfaces::Surface;
		using DataSurfaces::TotSurfaces;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int SurfNum; // local loop index.

		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
			SetupEMSActuator( "Surface", Surface( SurfNum ).Name, "Interior Surface Convection Heat Transfer Coefficient", "[W/m2-K]", Surface( SurfNum ).EMSOverrideIntConvCoef, Surface( SurfNum ).EMSValueForIntConvCoef );
			SetupEMSActuator( "Surface", Surface( SurfNum ).Name, "Exterior Surface Convection Heat Transfer Coefficient", "[W/m2-K]", Surface( SurfNum ).EMSOverrideExtConvCoef, Surface( SurfNum ).EMSValueForExtConvCoef );
		}

	}

	void
	SetupSurfaceConstructionActuators()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   Jan 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// setup EMS actuators available for surface construction

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataSurfaces::Surface;
		using DataSurfaces::TotSurfaces;
		using DataHeatBalance::TotConstructs;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int SurfNum; // local loop index.

		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {

			if ( ! Surface( SurfNum ).HeatTransSurf ) continue;

			SetupEMSActuator( "Surface", Surface( SurfNum ).Name, "Construction State", "[ ]", Surface( SurfNum ).EMSConstructionOverrideON, Surface( SurfNum ).EMSConstructionOverrideValue );
		}

		//Setup error checking storage

		if ( ! allocated( EMSConstructActuatorChecked ) ) EMSConstructActuatorChecked.allocate( TotConstructs, TotSurfaces );
		EMSConstructActuatorChecked = false;

		if ( ! allocated( EMSConstructActuatorIsOkay ) ) EMSConstructActuatorIsOkay.allocate( TotConstructs, TotSurfaces );
		EMSConstructActuatorIsOkay = false;

	}

	void
	SetupSurfaceOutdoorBoundaryConditionActuators()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   May 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// setup EMS actuators for outside boundary conditions by surface

		// METHODOLOGY EMPLOYED:
		// loop through all surfaces, cycle if not heat transfer or outdoors BC

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataSurfaces::Surface;
		using DataSurfaces::TotSurfaces;
		using DataSurfaces::ExternalEnvironment;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int SurfNum; // local loop index.

		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {

			if ( ! Surface( SurfNum ).HeatTransSurf ) continue;
			if ( ! ( Surface( SurfNum ).ExtBoundCond == ExternalEnvironment ) ) continue;

			SetupEMSActuator( "Surface", Surface( SurfNum ).Name, "Outdoor Air Drybulb Temperature", "[C]", Surface( SurfNum ).OutDryBulbTempEMSOverrideOn, Surface( SurfNum ).OutDryBulbTempEMSOverrideValue );

			SetupEMSActuator( "Surface", Surface( SurfNum ).Name, "Outdoor Air Wetbulb Temperature", "[C]", Surface( SurfNum ).OutWetBulbTempEMSOverrideOn, Surface( SurfNum ).OutWetBulbTempEMSOverrideValue );
			if ( Surface( SurfNum ).ExtWind ) {
				SetupEMSActuator( "Surface", Surface( SurfNum ).Name, "Outdoor Air Wind Speed", "[m/s]", Surface( SurfNum ).WindSpeedEMSOverrideOn, Surface( SurfNum ).WindSpeedEMSOverrideValue );
			}
		}

	}

	void
	SetupZoneInfoAsInternalDataAvail()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   May 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// set up zone-related info as internal data

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::NumOfZones;
		using DataHeatBalance::Zone;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int ZoneNum;

		if ( allocated( Zone ) ) {
			for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {

				SetupEMSInternalVariable( "Zone Floor Area", Zone( ZoneNum ).Name, "[m2]", Zone( ZoneNum ).FloorArea );
				SetupEMSInternalVariable( "Zone Air Volume", Zone( ZoneNum ).Name, "[m3]", Zone( ZoneNum ).Volume );
				SetupEMSInternalVariable( "Zone Multiplier", Zone( ZoneNum ).Name, "[ ]", Zone( ZoneNum ).Multiplier );
				SetupEMSInternalVariable( "Zone List Multiplier", Zone( ZoneNum ).Name, "[ ]", Zone( ZoneNum ).ListMultiplier );
			}

		}

	}

} // EMSManager

//Moved these setup EMS actuator routines out of module to solve circular use problems between
//  ScheduleManager and OutputProcessor. Followed pattern used for SetupOutputVariable

void
SetupEMSActuator(
	std::string const & cComponentTypeName,
	std::string const & cUniqueIDName,
	std::string const & cControlTypeName,
	std::string const & cUnits,
	bool & lEMSActuated,
	Real64 & rValue
)
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Peter Graham Ellis
	//       DATE WRITTEN   June 2006
	//       MODIFIED       Brent Griffith April 2009,
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// register a new actuator for EMS
	//   set  up pointer to logical and real value

	// METHODOLOGY EMPLOYED:
	// push size of ActuatorVariable and add a new one.
	//  check for duplicates.

	// Using/Aliasing
	using InputProcessor::MakeUPPERCase;
	using namespace DataPrecisionGlobals;
	using namespace DataRuntimeLanguage;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

	// Object Data

	// FLOW:

	std::string const UpperCaseObjectType( MakeUPPERCase( cComponentTypeName ) );
	std::string const UpperCaseObjectName( MakeUPPERCase( cUniqueIDName ) );
	std::string const UpperCaseActuatorName( MakeUPPERCase( cControlTypeName ) );

	EMSActuatorKey const key( UpperCaseObjectType, UpperCaseObjectName, UpperCaseActuatorName );

	if ( EMSActuator_lookup.find( key ) == EMSActuator_lookup.end() ) {
		if ( numEMSActuatorsAvailable == 0 ) {
			EMSActuatorAvailable.allocate( varsAvailableAllocInc );
			numEMSActuatorsAvailable = 1;
			maxEMSActuatorsAvailable = varsAvailableAllocInc;
		} else {
			if ( numEMSActuatorsAvailable + 1 > maxEMSActuatorsAvailable ) {
				EMSActuatorAvailable.redimension( maxEMSActuatorsAvailable *= 2 );
			}
			++numEMSActuatorsAvailable;
		}

		auto & actuator( EMSActuatorAvailable( numEMSActuatorsAvailable ) );
		actuator.ComponentTypeName = cComponentTypeName;
		actuator.UniqueIDName = cUniqueIDName;
		actuator.ControlTypeName = cControlTypeName;
		actuator.Units = cUnits;
		actuator.Actuated >>= lEMSActuated; // Pointer assigment
		actuator.RealValue >>= rValue; // Pointer assigment
		actuator.PntrVarTypeUsed = PntrReal;
		EMSActuator_lookup.insert( key );
	}

}

void
SetupEMSActuator(
	std::string const & cComponentTypeName,
	std::string const & cUniqueIDName,
	std::string const & cControlTypeName,
	std::string const & cUnits,
	bool & lEMSActuated,
	int & iValue
)
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Brent Griffith
	//       DATE WRITTEN   May 2009
	//       MODIFIED
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// register a new actuator for EMS
	//   set  up pointer to logical and integer value

	// METHODOLOGY EMPLOYED:
	// push size of ActuatorVariable and add a new one.
	//  check for duplicates.

	// Using/Aliasing
	using InputProcessor::MakeUPPERCase;
	using namespace DataPrecisionGlobals;
	using namespace DataRuntimeLanguage;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

	// Object Data

	// FLOW:
	//  IF (.NOT. ActuatorFileOpen) THEN
	//    !OPEN(88,file='eplusout.add')
	//    !WRITE(88, '(A)') 'Object Type,Actuator Name'
	//    ActuatorFileOpen = .TRUE.
	//  END IF

	std::string const UpperCaseObjectType( MakeUPPERCase( cComponentTypeName ) );
	std::string const UpperCaseObjectName( MakeUPPERCase( cUniqueIDName ) );
	std::string const UpperCaseActuatorName( MakeUPPERCase( cControlTypeName ) );

	EMSActuatorKey const key( UpperCaseObjectType, UpperCaseObjectName, UpperCaseActuatorName );

	if ( EMSActuator_lookup.find( key ) == EMSActuator_lookup.end() ) {
		if ( numEMSActuatorsAvailable == 0 ) {
			EMSActuatorAvailable.allocate( varsAvailableAllocInc );
			numEMSActuatorsAvailable = 1;
			maxEMSActuatorsAvailable = varsAvailableAllocInc;
		} else {
			if ( numEMSActuatorsAvailable + 1 > maxEMSActuatorsAvailable ) {
				EMSActuatorAvailable.redimension( maxEMSActuatorsAvailable *= 2 );
			}
			++numEMSActuatorsAvailable;
		}

		auto & actuator( EMSActuatorAvailable( numEMSActuatorsAvailable ) );
		actuator.ComponentTypeName = cComponentTypeName;
		actuator.UniqueIDName = cUniqueIDName;
		actuator.ControlTypeName = cControlTypeName;
		actuator.Units = cUnits;
		actuator.Actuated >>= lEMSActuated; // Pointer assigment
		actuator.IntValue >>= iValue; // Pointer assigment
		actuator.PntrVarTypeUsed = PntrInteger;
		EMSActuator_lookup.insert( key );
	}

}

void
SetupEMSActuator(
	std::string const & cComponentTypeName,
	std::string const & cUniqueIDName,
	std::string const & cControlTypeName,
	std::string const & cUnits,
	bool & lEMSActuated,
	bool & lValue
)
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Brent Griffith
	//       DATE WRITTEN   August 2009
	//       MODIFIED
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// register a new actuator for EMS
	//   set  up pointer to logical and logical value

	// METHODOLOGY EMPLOYED:
	// push size of ActuatorVariable and add a new one.
	//  check for duplicates.

	// Using/Aliasing
	using InputProcessor::MakeUPPERCase;
	using namespace DataPrecisionGlobals;
	using namespace DataRuntimeLanguage;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

	// Object Data

	// FLOW:

	std::string const UpperCaseObjectType( MakeUPPERCase( cComponentTypeName ) );
	std::string const UpperCaseObjectName( MakeUPPERCase( cUniqueIDName ) );
	std::string const UpperCaseActuatorName( MakeUPPERCase( cControlTypeName ) );

	EMSActuatorKey const key( UpperCaseObjectType, UpperCaseObjectName, UpperCaseActuatorName );

	if ( EMSActuator_lookup.find( key ) == EMSActuator_lookup.end() ) {
		if ( numEMSActuatorsAvailable == 0 ) {
			EMSActuatorAvailable.allocate( varsAvailableAllocInc );
			numEMSActuatorsAvailable = 1;
			maxEMSActuatorsAvailable = varsAvailableAllocInc;
		} else {
			if ( numEMSActuatorsAvailable + 1 > maxEMSActuatorsAvailable ) {
				EMSActuatorAvailable.redimension( maxEMSActuatorsAvailable *= 2 );
			}
			++numEMSActuatorsAvailable;
		}

		auto & actuator( EMSActuatorAvailable( numEMSActuatorsAvailable ) );
		actuator.ComponentTypeName = cComponentTypeName;
		actuator.UniqueIDName = cUniqueIDName;
		actuator.ControlTypeName = cControlTypeName;
		actuator.Units = cUnits;
		actuator.Actuated >>= lEMSActuated; // Pointer assigment
		actuator.LogValue >>= lValue; // Pointer assigment
		actuator.PntrVarTypeUsed = PntrLogical;
		EMSActuator_lookup.insert( key );
	}

}

void
SetupEMSInternalVariable(
	std::string const & cDataTypeName,
	std::string const & cUniqueIDName,
	std::string const & cUnits,
	Real64 & rValue
)
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Brent Griffith
	//       DATE WRITTEN   May 2009
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// Setup internal data source and make available to EMS

	// METHODOLOGY EMPLOYED:
	// <description>

	// REFERENCES:
	// na

	// Using/Aliasing
	using InputProcessor::SameString;
	using namespace DataPrecisionGlobals;
	using namespace DataRuntimeLanguage;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS:
	// na

	// DERIVED TYPE DEFINITIONS:
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	int InternalVarAvailNum; // loop index
	bool FoundInternalDataType;
	bool FoundDuplicate;

	// Object Data

	FoundInternalDataType = false;
	FoundDuplicate = false;

	for ( InternalVarAvailNum = 1; InternalVarAvailNum <= numEMSInternalVarsAvailable; ++InternalVarAvailNum ) {
		if ( ( SameString( cDataTypeName, EMSInternalVarsAvailable( InternalVarAvailNum ).DataTypeName ) ) && ( SameString( cUniqueIDName, EMSInternalVarsAvailable( InternalVarAvailNum ).UniqueIDName ) ) ) {
			FoundDuplicate = true;
			break;
		}
	}

	if ( FoundDuplicate ) {
		ShowSevereError( "Duplicate internal variable was sent to SetupEMSInternalVariable." );
		ShowContinueError( "Internal variable type = " + cDataTypeName + " ; name = " + cUniqueIDName );
		ShowContinueError( "Called from SetupEMSInternalVariable." );
	} else {
		// add new internal data variable
		if ( numEMSInternalVarsAvailable == 0 ) {
			EMSInternalVarsAvailable.allocate( varsAvailableAllocInc );
			numEMSInternalVarsAvailable = 1;
			maxEMSInternalVarsAvailable = varsAvailableAllocInc;
		} else {
			if ( numEMSInternalVarsAvailable + 1 > maxEMSInternalVarsAvailable ) {
				EMSInternalVarsAvailable.redimension( maxEMSInternalVarsAvailable += varsAvailableAllocInc );
			}
			++numEMSInternalVarsAvailable;
		}

		InternalVarAvailNum = numEMSInternalVarsAvailable;
		EMSInternalVarsAvailable( InternalVarAvailNum ).DataTypeName = cDataTypeName;
		EMSInternalVarsAvailable( InternalVarAvailNum ).UniqueIDName = cUniqueIDName;
		EMSInternalVarsAvailable( InternalVarAvailNum ).Units = cUnits;
		EMSInternalVarsAvailable( InternalVarAvailNum ).RealValue >>= rValue;
		EMSInternalVarsAvailable( InternalVarAvailNum ).PntrVarTypeUsed = PntrReal;
	}

}

void
SetupEMSInternalVariable(
	std::string const & cDataTypeName,
	std::string const & cUniqueIDName,
	std::string const & cUnits,
	int & iValue
)
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Brent Griffith
	//       DATE WRITTEN   May 2009
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// Setup internal data source and make available to EMS

	// METHODOLOGY EMPLOYED:
	// <description>

	// REFERENCES:
	// na

	// Using/Aliasing
	using InputProcessor::SameString;
	using namespace DataPrecisionGlobals;
	using namespace DataRuntimeLanguage;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS:
	// na

	// DERIVED TYPE DEFINITIONS:
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	int InternalVarAvailNum; // loop index
	bool FoundInternalDataType;
	bool FoundDuplicate;

	// Object Data

	FoundInternalDataType = false;
	FoundDuplicate = false;

	for ( InternalVarAvailNum = 1; InternalVarAvailNum <= numEMSInternalVarsAvailable; ++InternalVarAvailNum ) {
		if ( ( SameString( cDataTypeName, EMSInternalVarsAvailable( InternalVarAvailNum ).DataTypeName ) ) && ( SameString( cUniqueIDName, EMSInternalVarsAvailable( InternalVarAvailNum ).UniqueIDName ) ) ) {
			FoundDuplicate = true;
			break;
		}
	}

	if ( FoundDuplicate ) {
		ShowSevereError( "Duplicate internal variable was sent to SetupEMSInternalVariable." );
		ShowContinueError( "Internal variable type = " + cDataTypeName + " ; name = " + cUniqueIDName );
		ShowContinueError( "called from SetupEMSInternalVariable" );
	} else {
		// add new internal data variable
		if ( numEMSInternalVarsAvailable == 0 ) {
			EMSInternalVarsAvailable.allocate( varsAvailableAllocInc );
			numEMSInternalVarsAvailable = 1;
			maxEMSInternalVarsAvailable = varsAvailableAllocInc;
		} else {
			if ( numEMSInternalVarsAvailable + 1 > maxEMSInternalVarsAvailable ) {
				EMSInternalVarsAvailable.redimension( maxEMSInternalVarsAvailable += varsAvailableAllocInc );
			}
			++numEMSInternalVarsAvailable;
		}

		InternalVarAvailNum = numEMSInternalVarsAvailable;
		EMSInternalVarsAvailable( InternalVarAvailNum ).DataTypeName = cDataTypeName;
		EMSInternalVarsAvailable( InternalVarAvailNum ).UniqueIDName = cUniqueIDName;
		EMSInternalVarsAvailable( InternalVarAvailNum ).Units = cUnits;
		EMSInternalVarsAvailable( InternalVarAvailNum ).IntValue >>= iValue;
		EMSInternalVarsAvailable( InternalVarAvailNum ).PntrVarTypeUsed = PntrInteger;
	}

}

} // EnergyPlus
