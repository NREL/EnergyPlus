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
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <WindTurbine.hh>
#include <CommandLineInterface.hh>
#include <DataEnvironment.hh>
#include <DataGenerators.hh>
#include <DataGlobalConstants.hh>
#include <DataStringGlobals.hh>
#include <DataHVACGlobals.hh>
#include <DataPrecisionGlobals.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <OutputProcessor.hh>
#include <Psychrometrics.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

// (ref: Object: Generator:WindTurbine)

namespace WindTurbine {
	// Module containing the data for wind turbine system

	// MODULE INFORMATION:
	//       AUTHOR         Daeho Kang
	//       DATE WRITTEN   October 2009
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This module is to calculate the electrical power output that wind turbine systems produce.
	// Both horizontal and vertical axis wind turbine systems are modeled.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// Sathyajith Mathew. 2006. Wind Energy: Fundamental, Resource Analysis and Economics. Springer,
	//     Chap. 2, pp. 11-15
	// Mazharul Islam, David S.K. Ting, and Amir Fartaj. 2008. Aerodynamic Models for Darrieus-type sSraight-bladed
	//     Vertical Axis Wind Turbines. Renewable & Sustainable Energy Reviews, Volume 12, pp.1087-1109

	// OTHER NOTES: none

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataGenerators;
	using DataGlobals::Pi;
	using DataGlobals::SecInHour;
	using DataGlobals::BeginEnvrnFlag;
	using DataGlobals::DegToRadians;
	using DataGlobals::ScheduleAlwaysOn;

	// Data
	// MODULE PARAMETER DEFINITIONS
	int const HAWT( 1 ); // 'HorizontalAxisWindTurbine'
	int const VAWT( 2 ); // 'VerticalAxisWindTurbine'

	int const FSFP( 1 ); // 'FixedSpeedFixedPitch'
	int const FSVP( 2 ); // 'FixedSpeedVariablePitch'
	int const VSFP( 3 ); // 'VariableSpeedFixedPitch'
	int const VSVP( 4 ); // 'VariableSpeedVariablePitch'

	static std::string const BlankString;

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLES DECLARATIONS:
	int NumWindTurbines( 0 ); // Total wind turbine statements in inputs

	// Subroutine Specifications for the Heat Balance Module

	// Object Data
	Array1D< WindTurbineParams > WindTurbineSys;

	// Functions

	void
	SimWindTurbine(
		int const EP_UNUSED( GeneratorType ), // Type of Generator
		std::string const & GeneratorName, // User specified name of Generator
		int & GeneratorIndex, // Generator index
		bool const RunFlag, // ON or OFF
		Real64 const EP_UNUSED( WTLoad ) // Electrical load on WT (not used)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Daeho Kang
		//       DATE WRITTEN   October 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine manages the simulation of wind turbine component.
		// This drivers manages the calls to all of the other drivers and simulation algorithms.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using General::TrimSigDigits;
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool GetInputFlag( true );
		int WindTurbineNum;
		// Obtains and allocates heat balance related parameters from input

		if ( GetInputFlag ) {
			GetWindTurbineInput();
			GetInputFlag = false;
		}

		if ( GeneratorIndex == 0 ) {
			WindTurbineNum = FindItemInList( GeneratorName, WindTurbineSys );
			if ( WindTurbineNum == 0 ) {
				ShowFatalError( "SimWindTurbine: Specified Generator not one of Valid Wind Turbine Generators " + GeneratorName );
			}
			GeneratorIndex = WindTurbineNum;
		} else {
			WindTurbineNum = GeneratorIndex;
			if ( WindTurbineNum > NumWindTurbines || WindTurbineNum < 1 ) {
				ShowFatalError( "SimWindTurbine: Invalid GeneratorIndex passed=" + TrimSigDigits( WindTurbineNum ) + ", Number of Wind Turbine Generators=" + TrimSigDigits( NumWindTurbines ) + ", Generator name=" + GeneratorName );
			}
			if ( GeneratorName != WindTurbineSys( WindTurbineNum ).Name ) {
				ShowFatalError( "SimMWindTurbine: Invalid GeneratorIndex passed=" + TrimSigDigits( WindTurbineNum ) + ", Generator name=" + GeneratorName + ", stored Generator Name for that index=" + WindTurbineSys( WindTurbineNum ).Name );
			}
		}

		InitWindTurbine( WindTurbineNum );

		CalcWindTurbine( WindTurbineNum, RunFlag );

		ReportWindTurbine( WindTurbineNum );

	}

	void
	GetWTGeneratorResults(
		int const EP_UNUSED( GeneratorType ), // Type of Generator
		int const GeneratorIndex, // Generator number
		Real64 & GeneratorPower, // Electrical power
		Real64 & GeneratorEnergy, // Electrical energy
		Real64 & ThermalPower,
		Real64 & ThermalEnergy
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   Aug. 2008
		//       MODIFIED       D Kang, October 2009 for Wind Turbine
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine provides a "get" method to collect results for individual electic load centers.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		GeneratorPower = WindTurbineSys( GeneratorIndex ).Power;
		GeneratorEnergy = WindTurbineSys( GeneratorIndex ).Energy;

		// Thermal energy is ignored
		ThermalPower = 0.0;
		ThermalEnergy = 0.0;

	}

	void
	GetWindTurbineInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Daeho Kang
		//       DATE WRITTEN   October 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine gets input data for wind turbine components
		// and stores it in the wind turbine data structure.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using InputProcessor::SameString;
		using InputProcessor::VerifyName;
		using InputProcessor::GetObjectDefMaxArgs;
		using ScheduleManager::GetScheduleIndex;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const CurrentModuleObject( "Generator:WindTurbine" );
		Real64 const SysEffDefault( 0.835 ); // Default value of overall system efficiency
		Real64 const MaxTSR( 12.0 ); // Maximum tip speed ratio
		Real64 const DefaultPC( 0.25 ); // Default power coefficient
		Real64 const MaxPowerCoeff( 0.59 ); // Maximum power coefficient
		Real64 const DefaultH( 50.0 ); // Default of height for local wind speed

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool ErrorsFound( false ); // If errors detected in input
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		int WindTurbineNum; // Wind turbine number
		int NumAlphas; // Number of Alphas for each GetobjectItem call
		int NumNumbers; // Number of Numbers for each GetobjectItem call
		int NumArgs;
		int IOStat;
		Array1D_string cAlphaArgs; // Alpha input items for object
		Array1D_string cAlphaFields; // Alpha field names
		Array1D_string cNumericFields; // Numeric field names
		Array1D< Real64 > rNumericArgs; // Numeric input items for object
		Array1D_bool lAlphaBlanks; // Logical array, alpha field input BLANK = .TRUE.
		Array1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.

		// Initializations and allocations
		GetObjectDefMaxArgs( CurrentModuleObject, NumArgs, NumAlphas, NumNumbers );
		cAlphaArgs.allocate( NumAlphas );
		cAlphaFields.allocate( NumAlphas );
		cNumericFields.allocate( NumNumbers );
		rNumericArgs.dimension( NumNumbers, 0.0 );
		lAlphaBlanks.dimension( NumAlphas, true );
		lNumericBlanks.dimension( NumNumbers, true );

		NumWindTurbines = GetNumObjectsFound( CurrentModuleObject );

		WindTurbineSys.allocate( NumWindTurbines );

		// Flow
		for ( WindTurbineNum = 1; WindTurbineNum <= NumWindTurbines; ++WindTurbineNum ) {

			GetObjectItem( CurrentModuleObject, WindTurbineNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), WindTurbineSys, WindTurbineNum, IsNotOK, IsBlank, CurrentModuleObject + " Name" );

			if ( IsNotOK ) {
				ErrorsFound = true;
			}

			WindTurbineSys( WindTurbineNum ).Name = cAlphaArgs( 1 ); // Name of wind turbine

			WindTurbineSys( WindTurbineNum ).Schedule = cAlphaArgs( 2 ); // Get schedule
			if ( lAlphaBlanks( 2 ) ) {
				WindTurbineSys( WindTurbineNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				WindTurbineSys( WindTurbineNum ).SchedPtr = GetScheduleIndex( cAlphaArgs( 2 ) );
				if ( WindTurbineSys( WindTurbineNum ).SchedPtr == 0 ) {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFields( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\" not found." );
					ErrorsFound = true;
				}
			}
			// Select rotor type
			{ auto const SELECT_CASE_var( cAlphaArgs( 3 ) );
			if ( ( SELECT_CASE_var == "HORIZONTALAXISWINDTURBINE" ) || ( SELECT_CASE_var == "HAWT" ) || ( SELECT_CASE_var == "NONE" ) || ( SELECT_CASE_var == "" ) ) {
				WindTurbineSys( WindTurbineNum ).RotorType = HAWT;
			} else if ( ( SELECT_CASE_var == "VERTICALAXISWINDTURBINE" ) || ( SELECT_CASE_var == "VAWT" ) ) {
				WindTurbineSys( WindTurbineNum ).RotorType = VAWT;
			} else {
				ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFields( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\"." );
				ErrorsFound = true;
			}}

			// Select control type
			{ auto const SELECT_CASE_var( cAlphaArgs( 4 ) );
			if ( ( SELECT_CASE_var == "FIXEDSPEEDFIXEDPITCH" ) || ( SELECT_CASE_var == "FSFP" ) ) {
				WindTurbineSys( WindTurbineNum ).ControlType = FSFP;
			} else if ( ( SELECT_CASE_var == "FIXEDSPEEDVARIABLEPITCH" ) || ( SELECT_CASE_var == "FSVP" ) ) {
				WindTurbineSys( WindTurbineNum ).ControlType = FSVP;
			} else if ( ( SELECT_CASE_var == "VARIABLESPEEDFIXEDPITCH" ) || ( SELECT_CASE_var == "VSFP" ) ) {
				WindTurbineSys( WindTurbineNum ).ControlType = VSFP;
			} else if ( ( SELECT_CASE_var == "VARIABLESPEEDVARIABLEPITCH" ) || ( SELECT_CASE_var == "VSVP" ) || ( SELECT_CASE_var == "NONE" ) || ( SELECT_CASE_var == "" ) ) {
				WindTurbineSys( WindTurbineNum ).ControlType = VSVP;
			} else {
				ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFields( 4 ) + "=\"" + cAlphaArgs( 4 ) + "\"." );
				ErrorsFound = true;
			}}

			WindTurbineSys( WindTurbineNum ).RatedRotorSpeed = rNumericArgs( 1 ); // Maximum rotor speed in rpm
			if ( WindTurbineSys( WindTurbineNum ).RatedRotorSpeed <= 0.0 ) {
				if ( lNumericBlanks( 1 ) ) {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cNumericFields( 1 ) + " is required but input is blank." );
				} else {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cNumericFields( 1 ) + "=[" + RoundSigDigits( rNumericArgs( 1 ), 2 ) + "] must be greater than zero." );
				}
				ErrorsFound = true;
			}

			WindTurbineSys( WindTurbineNum ).RotorDiameter = rNumericArgs( 2 ); // Rotor diameter in m
			if ( WindTurbineSys( WindTurbineNum ).RotorDiameter <= 0.0 ) {
				if ( lNumericBlanks( 2 ) ) {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cNumericFields( 2 ) + " is required but input is blank." );
				} else {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cNumericFields( 2 ) + "=[" + RoundSigDigits( rNumericArgs( 2 ), 1 ) + "] must be greater than zero." );
				}
				ErrorsFound = true;
			}

			WindTurbineSys( WindTurbineNum ).RotorHeight = rNumericArgs( 3 ); // Overall height of the rotor
			if ( WindTurbineSys( WindTurbineNum ).RotorHeight <= 0.0 ) {
				if ( lNumericBlanks( 3 ) ) {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cNumericFields( 3 ) + " is required but input is blank." );
				} else {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cNumericFields( 3 ) + "=[" + RoundSigDigits( rNumericArgs( 3 ), 1 ) + "] must be greater than zero." );
				}
				ErrorsFound = true;
			}

			WindTurbineSys( WindTurbineNum ).NumOfBlade = rNumericArgs( 4 ); // Total number of blade
			if ( WindTurbineSys( WindTurbineNum ).NumOfBlade == 0 ) {
				ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cNumericFields( 4 ) + "=[" + RoundSigDigits( rNumericArgs( 4 ), 0 ) + "] must be greater than zero." );
				ErrorsFound = true;
			}

			WindTurbineSys( WindTurbineNum ).RatedPower = rNumericArgs( 5 ); // Rated average power
			if ( WindTurbineSys( WindTurbineNum ).RatedPower == 0.0 ) {
				if ( lNumericBlanks( 5 ) ) {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cNumericFields( 5 ) + " is required but input is blank." );
				} else {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cNumericFields( 5 ) + "=[" + RoundSigDigits( rNumericArgs( 5 ), 2 ) + "] must be greater than zero." );
				}
				ErrorsFound = true;
			}

			WindTurbineSys( WindTurbineNum ).RatedWindSpeed = rNumericArgs( 6 ); // Rated wind speed
			if ( WindTurbineSys( WindTurbineNum ).RatedWindSpeed == 0.0 ) {
				if ( lNumericBlanks( 6 ) ) {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cNumericFields( 6 ) + " is required but input is blank." );
				} else {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cNumericFields( 6 ) + "=[" + RoundSigDigits( rNumericArgs( 6 ), 2 ) + "] must be greater than zero." );
				}
				ErrorsFound = true;
			}

			WindTurbineSys( WindTurbineNum ).CutInSpeed = rNumericArgs( 7 ); // Minimum wind speed for system operation
			if ( WindTurbineSys( WindTurbineNum ).CutInSpeed == 0.0 ) {
				if ( lNumericBlanks( 7 ) ) {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cNumericFields( 7 ) + " is required but input is blank." );
				} else {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cNumericFields( 7 ) + "=[" + RoundSigDigits( rNumericArgs( 7 ), 2 ) + "] must be greater than zero." );
				}
				ErrorsFound = true;
			}

			WindTurbineSys( WindTurbineNum ).CutOutSpeed = rNumericArgs( 8 ); // Minimum wind speed for system operation
			if ( WindTurbineSys( WindTurbineNum ).CutOutSpeed == 0.0 ) {
				if ( lNumericBlanks( 8 ) ) {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cNumericFields( 8 ) + " is required but input is blank." );
				} else if ( WindTurbineSys( WindTurbineNum ).CutOutSpeed <= WindTurbineSys( WindTurbineNum ).RatedWindSpeed ) {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cNumericFields( 8 ) + "=[" + RoundSigDigits( rNumericArgs( 8 ), 2 ) + "] must be greater than " + cNumericFields( 6 ) + "=[" + RoundSigDigits( rNumericArgs( 6 ), 2 ) + "]." );
				} else {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cNumericFields( 8 ) + "=[" + RoundSigDigits( rNumericArgs( 8 ), 2 ) + "] must be greater than zero" );
				}
				ErrorsFound = true;
			}

			WindTurbineSys( WindTurbineNum ).SysEfficiency = rNumericArgs( 9 ); // Overall wind turbine system efficiency
			if ( lNumericBlanks( 9 ) || WindTurbineSys( WindTurbineNum ).SysEfficiency == 0.0 || WindTurbineSys( WindTurbineNum ).SysEfficiency > 1.0 ) {
				WindTurbineSys( WindTurbineNum ).SysEfficiency = SysEffDefault;
				ShowWarningError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cNumericFields( 9 ) + "=[" + RoundSigDigits( rNumericArgs( 9 ), 2 ) + "]." );
				ShowContinueError( "...The default value of " + RoundSigDigits( SysEffDefault, 3 ) + " for " + cNumericFields( 9 ) + " was assumed." );
			}

			WindTurbineSys( WindTurbineNum ).MaxTipSpeedRatio = rNumericArgs( 10 ); // Maximum tip speed ratio
			if ( WindTurbineSys( WindTurbineNum ).MaxTipSpeedRatio == 0.0 ) {
				if ( lNumericBlanks( 10 ) ) {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cNumericFields( 10 ) + " is required but input is blank." );
				} else {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cNumericFields( 10 ) + "=[" + RoundSigDigits( rNumericArgs( 10 ), 2 ) + "] must be greater than zero." );
				}
				ErrorsFound = true;
			}
			if ( WindTurbineSys( WindTurbineNum ).SysEfficiency > MaxTSR ) {
				WindTurbineSys( WindTurbineNum ).SysEfficiency = MaxTSR;
				ShowWarningError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cNumericFields( 10 ) + "=[" + RoundSigDigits( rNumericArgs( 10 ), 2 ) + "]." );
				ShowContinueError( "...The default value of " + RoundSigDigits( MaxTSR, 1 ) + " for " + cNumericFields( 10 ) + " was assumed." );
			}

			WindTurbineSys( WindTurbineNum ).MaxPowerCoeff = rNumericArgs( 11 ); // Maximum power coefficient
			if ( WindTurbineSys( WindTurbineNum ).RotorType == HAWT && WindTurbineSys( WindTurbineNum ).MaxPowerCoeff == 0.0 ) {
				if ( lNumericBlanks( 11 ) ) {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cNumericFields( 11 ) + " is required but input is blank." );
				} else {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cNumericFields( 11 ) + "=[" + RoundSigDigits( rNumericArgs( 11 ), 2 ) + "] must be greater than zero." );
				}
				ErrorsFound = true;
			}
			if ( WindTurbineSys( WindTurbineNum ).MaxPowerCoeff > MaxPowerCoeff ) {
				WindTurbineSys( WindTurbineNum ).MaxPowerCoeff = DefaultPC;
				ShowWarningError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cNumericFields( 11 ) + "=[" + RoundSigDigits( rNumericArgs( 11 ), 2 ) + "]." );
				ShowContinueError( "...The default value of " + RoundSigDigits( DefaultPC, 2 ) + " for " + cNumericFields( 11 ) + " will be used." );
			}

			WindTurbineSys( WindTurbineNum ).LocalAnnualAvgWS = rNumericArgs( 12 ); // Local wind speed annually averaged
			if ( WindTurbineSys( WindTurbineNum ).LocalAnnualAvgWS == 0.0 ) {
				if ( lNumericBlanks( 12 ) ) {
					ShowWarningError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cNumericFields( 12 ) + " is necessary for accurate prediction but input is blank." );
				} else {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cNumericFields( 12 ) + "=[" + RoundSigDigits( rNumericArgs( 12 ), 2 ) + "] must be greater than zero." );
					ErrorsFound = true;
				}
			}

			WindTurbineSys( WindTurbineNum ).HeightForLocalWS = rNumericArgs( 13 ); // Height of local meteorological station
			if ( WindTurbineSys( WindTurbineNum ).HeightForLocalWS == 0.0 ) {
				if ( WindTurbineSys( WindTurbineNum ).LocalAnnualAvgWS == 0.0 ) {
					WindTurbineSys( WindTurbineNum ).HeightForLocalWS = 0.0;
				} else {
					WindTurbineSys( WindTurbineNum ).HeightForLocalWS = DefaultH;
					if ( lNumericBlanks( 13 ) ) {
						ShowWarningError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cNumericFields( 13 ) + " is necessary for accurate prediction but input is blank." );
						ShowContinueError( "...The default value of " + RoundSigDigits( DefaultH, 2 ) + " for " + cNumericFields( 13 ) + " will be used." );
					} else {
						ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cNumericFields( 13 ) + "=[" + RoundSigDigits( rNumericArgs( 13 ), 2 ) + "] must be greater than zero." );
						ErrorsFound = true;
					}
				}
			}

			WindTurbineSys( WindTurbineNum ).ChordArea = rNumericArgs( 14 ); // Chord area of a single blade for VAWTs
			if ( WindTurbineSys( WindTurbineNum ).RotorType == VAWT && WindTurbineSys( WindTurbineNum ).ChordArea == 0.0 ) {
				if ( lNumericBlanks( 14 ) ) {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cNumericFields( 14 ) + " is required but input is blank." );
				} else {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cNumericFields( 14 ) + "=[" + RoundSigDigits( rNumericArgs( 14 ), 2 ) + "] must be greater than zero." );
				}
				ErrorsFound = true;
			}

			WindTurbineSys( WindTurbineNum ).DragCoeff = rNumericArgs( 15 ); // Blade drag coefficient
			if ( WindTurbineSys( WindTurbineNum ).RotorType == VAWT && WindTurbineSys( WindTurbineNum ).DragCoeff == 0.0 ) {
				if ( lNumericBlanks( 15 ) ) {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cNumericFields( 15 ) + " is required but input is blank." );
				} else {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cNumericFields( 15 ) + "=[" + RoundSigDigits( rNumericArgs( 15 ), 2 ) + "] must be greater than zero." );
				}
				ErrorsFound = true;
			}

			WindTurbineSys( WindTurbineNum ).LiftCoeff = rNumericArgs( 16 ); // Blade lift coefficient
			if ( WindTurbineSys( WindTurbineNum ).RotorType == VAWT && WindTurbineSys( WindTurbineNum ).LiftCoeff == 0.0 ) {
				if ( lNumericBlanks( 16 ) ) {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cNumericFields( 16 ) + " is required but input is blank." );
				} else {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cNumericFields( 16 ) + "=[" + RoundSigDigits( rNumericArgs( 16 ), 2 ) + "] must be greater than zero." );
				}
				ErrorsFound = true;
			}

			WindTurbineSys( WindTurbineNum ).PowerCoeffC1 = rNumericArgs( 17 ); // Empirical power coefficient C1
			if ( lNumericBlanks( 17 ) ) {
				WindTurbineSys( WindTurbineNum ).PowerCoeffC1 = 0.0;
			}
			WindTurbineSys( WindTurbineNum ).PowerCoeffC2 = rNumericArgs( 18 ); // Empirical power coefficient C2
			if ( lNumericBlanks( 18 ) ) {
				WindTurbineSys( WindTurbineNum ).PowerCoeffC2 = 0.0;
			}
			WindTurbineSys( WindTurbineNum ).PowerCoeffC3 = rNumericArgs( 19 ); // Empirical power coefficient C3
			if ( lNumericBlanks( 19 ) ) {
				WindTurbineSys( WindTurbineNum ).PowerCoeffC3 = 0.0;
			}
			WindTurbineSys( WindTurbineNum ).PowerCoeffC4 = rNumericArgs( 20 ); // Empirical power coefficient C4
			if ( lNumericBlanks( 20 ) ) {
				WindTurbineSys( WindTurbineNum ).PowerCoeffC4 = 0.0;
			}
			WindTurbineSys( WindTurbineNum ).PowerCoeffC5 = rNumericArgs( 21 ); // Empirical power coefficient C5
			if ( lNumericBlanks( 21 ) ) {
				WindTurbineSys( WindTurbineNum ).PowerCoeffC5 = 0.0;
			}
			WindTurbineSys( WindTurbineNum ).PowerCoeffC6 = rNumericArgs( 22 ); // Empirical power coefficient C6
			if ( lNumericBlanks( 22 ) ) {
				WindTurbineSys( WindTurbineNum ).PowerCoeffC6 = 0.0;
			}

		}

		cAlphaArgs.deallocate();
		cAlphaFields.deallocate();
		cNumericFields.deallocate();
		rNumericArgs.deallocate();
		lAlphaBlanks.deallocate();
		lNumericBlanks.deallocate();

		if ( ErrorsFound ) ShowFatalError( CurrentModuleObject + " errors occurred in input.  Program terminates." );

		for ( WindTurbineNum = 1; WindTurbineNum <= NumWindTurbines; ++WindTurbineNum ) {
			SetupOutputVariable( "Generator Produced Electric Power [W]", WindTurbineSys( WindTurbineNum ).Power, "System", "Average", WindTurbineSys( WindTurbineNum ).Name );
			SetupOutputVariable( "Generator Produced Electric Energy [J]", WindTurbineSys( WindTurbineNum ).Energy, "System", "Sum", WindTurbineSys( WindTurbineNum ).Name, _, "ElectricityProduced", "WINDTURBINE", _, "Plant" );
			SetupOutputVariable( "Generator Turbine Local Wind Speed [m/s]", WindTurbineSys( WindTurbineNum ).LocalWindSpeed, "System", "Average", WindTurbineSys( WindTurbineNum ).Name );
			SetupOutputVariable( "Generator Turbine Local Air Density [kg/m3]", WindTurbineSys( WindTurbineNum ).LocalAirDensity, "System", "Average", WindTurbineSys( WindTurbineNum ).Name );
			SetupOutputVariable( "Generator Turbine Tip Speed Ratio []", WindTurbineSys( WindTurbineNum ).TipSpeedRatio, "System", "Average", WindTurbineSys( WindTurbineNum ).Name );
			{ auto const SELECT_CASE_var( WindTurbineSys( WindTurbineNum ).RotorType );
			if ( SELECT_CASE_var == HAWT ) {
				SetupOutputVariable( "Generator Turbine Power Coefficient []", WindTurbineSys( WindTurbineNum ).PowerCoeff, "System", "Average", WindTurbineSys( WindTurbineNum ).Name );
			} else if ( SELECT_CASE_var == VAWT ) {
				SetupOutputVariable( "Generator Turbine Chordal Component Velocity [m/s]", WindTurbineSys( WindTurbineNum ).ChordalVel, "System", "Average", WindTurbineSys( WindTurbineNum ).Name );
				SetupOutputVariable( "Generator Turbine Normal Component Velocity [m/s]", WindTurbineSys( WindTurbineNum ).NormalVel, "System", "Average", WindTurbineSys( WindTurbineNum ).Name );
				SetupOutputVariable( "Generator Turbine Relative Flow Velocity [m/s]", WindTurbineSys( WindTurbineNum ).RelFlowVel, "System", "Average", WindTurbineSys( WindTurbineNum ).Name );
				SetupOutputVariable( "Generator Turbine Attack Angle [deg]", WindTurbineSys( WindTurbineNum ).AngOfAttack, "System", "Average", WindTurbineSys( WindTurbineNum ).Name );
			}}
		}

	}

	void
	InitWindTurbine( int const WindTurbineNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Daeho Kang
		//       DATE WRITTEN   Oct 2009
		//       MODIFIED       Linda K. Lawrie, December 2009 for reading stat file
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine reads monthly average wind speed from stat file and then
		// determines annual average wind speed. Differences between this TMY wind speed
		// and local wind speed that the user inputs are then factored.
		// IF the user has no local wind data and does not enter the local wind speed to be factored,
		// then the factor of 1 is assigned, so that wind speed estimated
		// at the particular rotor height is used with no factorization.
		// It also initializes module variables at each time step.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// Using/Aliasing
		using DataEnvironment::WeatherFileWindModCoeff;
		using DataEnvironment::SiteWindBLHeight;
		using DataEnvironment::SiteWindExp;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static char const TabChr( '\t' ); // Tab character
		static gio::Fmt fmtA( "(A)" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool MyOneTimeFlag( true );
		int ReadStatus; // Reading status of stat file
		int statFile; // Weather Stat File
		std::string::size_type lnPtr; // scan pointer for Line input
		int mon; // loop counter
		bool wsStatFound; // logical noting that wind stats were found
		bool fileExists; // true if Stat file exists
		bool warningShown; // true if the <365 warning has already been shown
		std::string lineIn;
		Array1D< Real64 > MonthWS( 12 );
		static Real64 AnnualTMYWS( 0.0 ); // Annual average wind speed in stat file
		Real64 LocalTMYWS; // Annual average wind speed at the rotor height

		// Estimate average annual wind speed once
		if ( MyOneTimeFlag ) {
			wsStatFound = false;
			{ IOFlags flags; gio::inquire( DataStringGlobals::inStatFileName, flags ); fileExists = flags.exists(); }
			if ( fileExists ) {
				statFile = GetNewUnitNumber();
				ReadStatus = 0;
				{ IOFlags flags; flags.ACTION( "READ" ); gio::open( statFile, DataStringGlobals::inStatFileName, flags ); ReadStatus = flags.ios(); }
				if ( ReadStatus != 0 ) {
					ShowFatalError( "InitWindTurbine: Could not open file "+DataStringGlobals::inStatFileName+" for input (read)." );
				}
				while ( ReadStatus == 0 ) { //end of file
					{ IOFlags flags; gio::read( statFile, fmtA, flags ) >> lineIn; ReadStatus = flags.ios(); }
					// reconcile line with different versions of stat file
					lnPtr = index( lineIn, "Wind Speed" );
					if ( lnPtr == std::string::npos ) continue;
					// have hit correct section.
					while ( ReadStatus == 0 ) { // find daily avg line
						{ IOFlags flags; gio::read( statFile, fmtA, flags ) >> lineIn; ReadStatus = flags.ios(); }
						lnPtr = index( lineIn, "Daily Avg" );
						if ( lnPtr == std::string::npos ) continue;
						// tab delimited file
						lineIn.erase( 0, lnPtr + 10 );
						MonthWS = 0.0;
						wsStatFound = true;
						warningShown = false;
						for ( mon = 1; mon <= 12; ++mon ) {
							lnPtr = index( lineIn, TabChr );
							if ( lnPtr != 1 ) {
								if ( ( lnPtr == std::string::npos ) || ( ! stripped( lineIn.substr( 0, lnPtr ) ).empty() ) ) {
									if ( lnPtr != std::string::npos ) {
										gio::read( lineIn.substr( 0, lnPtr ), "*" ) >> MonthWS( mon );
										lineIn.erase( 0, lnPtr + 1 );
									}
								} else { // blank field
									if ( ! warningShown ) {
										ShowWarningError( "InitWindTurbine: read from " + DataStringGlobals::inStatFileName + " file shows <365 days in weather file. Annual average wind speed used will be inaccurate." );
										lineIn.erase( 0, lnPtr + 1 );
										warningShown = true;
									}
								}
							} else { // two tabs in succession
								if ( ! warningShown ) {
									ShowWarningError( "InitWindTurbine: read from " + DataStringGlobals::inStatFileName + " file shows <365 days in weather file. Annual average wind speed used will be inaccurate." );
									lineIn.erase( 0, lnPtr + 1 );
									warningShown = true;
								}
							}
						}
						break;
					}
					if ( wsStatFound ) break;
				}
				gio::close( statFile );
				if ( wsStatFound ) {
					AnnualTMYWS = sum( MonthWS ) / 12.0;
				} else {
					ShowWarningError( "InitWindTurbine: stat file did not include Wind Speed statistics. TMY Wind Speed adjusted at the height is used." );
				}
			} else { // No stat file
				ShowWarningError( "InitWindTurbine: stat file missing. TMY Wind Speed adjusted at the height is used." );
			}

			MyOneTimeFlag = false;

		}

		WindTurbineSys( WindTurbineNum ).AnnualTMYWS = AnnualTMYWS;

		// Factor differences between TMY wind data and local wind data once
		if ( AnnualTMYWS > 0.0 && WindTurbineSys( WindTurbineNum ).WSFactor == 0.0 && WindTurbineSys( WindTurbineNum ).LocalAnnualAvgWS > 0 ) {
			// Convert the annual wind speed to the local wind speed at the height of the local station, then factor
			LocalTMYWS = AnnualTMYWS * WeatherFileWindModCoeff * std::pow( WindTurbineSys( WindTurbineNum ).HeightForLocalWS / SiteWindBLHeight, SiteWindExp );
			WindTurbineSys( WindTurbineNum ).WSFactor = LocalTMYWS / WindTurbineSys( WindTurbineNum ).LocalAnnualAvgWS;
		}
		// Assign factor of 1.0 if no stat file or no input of local average wind speed
		if ( WindTurbineSys( WindTurbineNum ).WSFactor == 0.0 ) WindTurbineSys( WindTurbineNum ).WSFactor = 1.0;

		// Do every time step initialization
		WindTurbineSys( WindTurbineNum ).Power = 0.0;
		WindTurbineSys( WindTurbineNum ).TotPower = 0.0;
		WindTurbineSys( WindTurbineNum ).PowerCoeff = 0.0;
		WindTurbineSys( WindTurbineNum ).TipSpeedRatio = 0.0;
		WindTurbineSys( WindTurbineNum ).ChordalVel = 0.0;
		WindTurbineSys( WindTurbineNum ).NormalVel = 0.0;
		WindTurbineSys( WindTurbineNum ).RelFlowVel = 0.0;
		WindTurbineSys( WindTurbineNum ).AngOfAttack = 0.0;
		WindTurbineSys( WindTurbineNum ).TanForce = 0.0;
		WindTurbineSys( WindTurbineNum ).NorForce = 0.0;
		WindTurbineSys( WindTurbineNum ).TotTorque = 0.0;

	}

	void
	CalcWindTurbine(
		int const WindTurbineNum, // System is on
		bool const EP_UNUSED( RunFlag ) // System is on
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Daeho Kang
		//       DATE WRITTEN   Octorber 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// Sathyajith Mathew. 2006. Wind Energy: Fundamental, Resource Analysis and Economics. Springer,
		//     Chap. 2, pp. 11-15
		// Mazharul Islam, David S.K. Ting, and Amir Fartaj. 2008. Aerodynamic Models for Darrieus-type sSraight-bladed
		//     Vertical Axis Wind Turbines. Renewable & Sustainable Energy Reviews, Volume 12, pp.1087-1109

		// Using/Aliasing
		using ScheduleManager::GetCurrentScheduleValue;
		using Psychrometrics::PsyWFnTdbTwbPb;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using DataEnvironment::WindSpeedAt;
		using DataEnvironment::OutDryBulbTempAt;
		using DataEnvironment::OutWetBulbTempAt;
		using DataEnvironment::OutBaroPressAt;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const MaxTheta( 90.0 ); // Maximum of theta
		Real64 const MaxDegree( 360.0 ); // Maximum limit of outdoor air wind speed in m/s
		Real64 const SecInMin( 60.0 );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 LocalWindSpeed; // Ambient wind speed at the specific height in m/s
		Real64 RotorH; // Height of the rotor in m
		Real64 RotorD; // Diameter of the rotor in m
		Real64 LocalHumRat; // Local humidity ratio of the air at the specific height
		Real64 LocalAirDensity; // Local density at the specific height in m
		Real64 PowerCoeff; // Power coefficient
		Real64 SweptArea; // Swept area of the rotor in m2
		Real64 WTPower( 0.0 ); // Total Power generated by the turbine in the quasi-steady state in Watts
		Real64 Power; // Actual power of the turbine in Watts
		Real64 TipSpeedRatio; // Tip speed ratio (TSR)
		Real64 TipSpeedRatioAtI; // Tip speed ratio at the ith time step
		Real64 AzimuthAng; // Azimuth angle of blades in degree
		Real64 ChordalVel; // Chordal velocity component in m/s
		Real64 NormalVel; // Normal velocity component in m/s
		Real64 AngOfAttack; // Angle of attack of a single blade in degree
		Real64 RelFlowVel; // Relative flow velocity component in m/s
		Real64 TanForce; // Tangential force in N.m
		Real64 NorForce; // Normal force in N.m
		Real64 RotorVel; // Rotor velocity in m/s
		Real64 AvgTanForce; // Average tangential force in N.m
		Real64 Constant; // Constants within integrand of tangential force
		Real64 IntRelFlowVel; // Integration of relative flow velocity
		Real64 TotTorque; // Total torque for the number of blades
		Real64 Omega; // Angular velocity of rotor in rad/s
		Real64 TanForceCoeff; // Tnagential force coefficient
		Real64 NorForceCoeff; // Normal force coefficient
		Real64 Period; // Period of sine and cosine functions
		Real64 C1; // Empirical power coefficient C1
		Real64 C2; // Empirical power coefficient C2
		Real64 C3; // Empirical power coefficient C3
		Real64 C4; // Empirical power coefficient C4
		Real64 C5; // Empirical power coefficient C5
		Real64 C6; // Empirical power coefficient C6
		Real64 LocalTemp; // Ambient air temperature at the height in degree C
		Real64 LocalPress; // Local atmospheric pressure at the particular height in Pa
		Real64 InducedVel; // Induced velocity on the rotor in m/s
		//unused REAL(r64) :: SysEfficiency     ! Overall wind turbine system efficiency including generator and inverter
		Real64 MaxPowerCoeff; // Maximum power coefficient
		Real64 RotorSpeed; // Speed of rotors

		// Estimate local velocity and density
		RotorH = WindTurbineSys( WindTurbineNum ).RotorHeight;
		RotorD = WindTurbineSys( WindTurbineNum ).RotorDiameter;
		RotorSpeed = WindTurbineSys( WindTurbineNum ).RatedRotorSpeed;
		LocalTemp = OutDryBulbTempAt( RotorH );
		LocalPress = OutBaroPressAt( RotorH );
		LocalHumRat = PsyWFnTdbTwbPb( LocalTemp, OutWetBulbTempAt( RotorH ), LocalPress );
		LocalAirDensity = PsyRhoAirFnPbTdbW( LocalPress, LocalTemp, LocalHumRat );
		LocalWindSpeed = WindSpeedAt( RotorH );
		LocalWindSpeed /= WindTurbineSys( WindTurbineNum ).WSFactor;

		// Flow
		// Check wind conditions for system operation
		if ( GetCurrentScheduleValue( WindTurbineSys( WindTurbineNum ).SchedPtr ) > 0 && LocalWindSpeed > WindTurbineSys( WindTurbineNum ).CutInSpeed && LocalWindSpeed < WindTurbineSys( WindTurbineNum ).CutOutSpeed ) {

			// System is on
			Period = 2.0 * Pi;
			Omega = ( RotorSpeed * Period ) / SecInMin;
			SweptArea = ( Pi * pow_2( RotorD ) ) / 4;
			TipSpeedRatio = ( Omega * ( RotorD / 2.0 ) ) / LocalWindSpeed;

			// Limit maximum tip speed ratio
			if ( TipSpeedRatio > WindTurbineSys( WindTurbineNum ).MaxTipSpeedRatio ) {
				TipSpeedRatio = WindTurbineSys( WindTurbineNum ).MaxTipSpeedRatio;
			}

			{ auto const SELECT_CASE_var( WindTurbineSys( WindTurbineNum ).RotorType );
			if ( SELECT_CASE_var == HAWT ) { // Horizontal axis wind turbine

				MaxPowerCoeff = WindTurbineSys( WindTurbineNum ).MaxPowerCoeff;
				// Check if empirical constants are available
				C1 = WindTurbineSys( WindTurbineNum ).PowerCoeffC1;
				C2 = WindTurbineSys( WindTurbineNum ).PowerCoeffC2;
				C3 = WindTurbineSys( WindTurbineNum ).PowerCoeffC3;
				C4 = WindTurbineSys( WindTurbineNum ).PowerCoeffC4;
				C5 = WindTurbineSys( WindTurbineNum ).PowerCoeffC5;
				C6 = WindTurbineSys( WindTurbineNum ).PowerCoeffC6;

				Real64 const LocalWindSpeed_3( pow_3( LocalWindSpeed ) );
				if ( C1 > 0.0 && C2 > 0.0 && C3 > 0.0 && C4 >= 0.0 && C5 > 0.0 && C6 > 0.0 ) {
					// Analytical approximation
					// Maximum power, i.e., rotor speed is at maximum, and pitch angle is zero
					//TipSpeedRatioAtI = 1.0 / ( ( 1.0 / ( TipSpeedRatio + 0.08 * PitchAngle ) ) - ( 0.035 / ( pow_3( PitchAngle ) + 1.0 ) ) ); //Tuned PitchAngle is zero
					TipSpeedRatioAtI = TipSpeedRatio / ( 1.0 - ( TipSpeedRatio * 0.035 ) );
					//PowerCoeff = C1 * ( ( C2 / TipSpeedRatioAtI ) - ( C3 * PitchAngle ) - ( C4 * std::pow( PitchAngle, 1.5 ) ) - C5 ) * ( std::exp( -( C6 / TipSpeedRatioAtI ) ) ); //Tuned PitchAngle is zero
					PowerCoeff = C1 * ( ( C2 / TipSpeedRatioAtI ) - C5 ) * std::exp( -( C6 / TipSpeedRatioAtI ) );
					if ( PowerCoeff > MaxPowerCoeff ) {
						PowerCoeff = MaxPowerCoeff;
					}
					WTPower = 0.5 * LocalAirDensity * PowerCoeff * SweptArea * LocalWindSpeed_3;
				} else { // Simple approximation
					WTPower = 0.5 * LocalAirDensity * SweptArea * LocalWindSpeed_3 * MaxPowerCoeff;
					PowerCoeff = MaxPowerCoeff;
				}
				// Maximum of rated power
				if ( LocalWindSpeed >= WindTurbineSys( WindTurbineNum ).RatedWindSpeed || WTPower > WindTurbineSys( WindTurbineNum ).RatedPower ) {
					WTPower = WindTurbineSys( WindTurbineNum ).RatedPower;
					PowerCoeff = WTPower / ( 0.5 * LocalAirDensity * SweptArea * LocalWindSpeed_3 );
				}
				// Recalculated Cp at the rated power
				WindTurbineSys( WindTurbineNum ).PowerCoeff = PowerCoeff;

			} else if ( SELECT_CASE_var == VAWT ) { // Vertical axis wind turbine
				RotorVel = Omega * ( RotorD / 2.0 );
				// Recalculated omega, if TSR is greater than the maximum
				if ( TipSpeedRatio >= WindTurbineSys( WindTurbineNum ).MaxTipSpeedRatio ) {
					RotorVel = LocalWindSpeed * WindTurbineSys( WindTurbineNum ).MaxTipSpeedRatio;
					Omega = RotorVel / ( RotorD / 2.0 );
				}

				AzimuthAng = MaxDegree / WindTurbineSys( WindTurbineNum ).NumOfBlade;
				// Azimuth angle between zero and 90 degree
				if ( AzimuthAng > MaxTheta ) { // Number of blades is 2 or 3
					AzimuthAng -= MaxTheta;
					if ( AzimuthAng == MaxTheta ) { // 2 blades
						AzimuthAng = 0.0;
					}
				} else if ( AzimuthAng == MaxTheta ) { // 4 blades
					AzimuthAng = 0.0;
				}

				InducedVel = LocalWindSpeed * 2.0 / 3.0;
				// Velocity components
				Real64 const sin_AzimuthAng( std::sin( AzimuthAng * DegToRadians ) );
				Real64 const cos_AzimuthAng( std::cos( AzimuthAng * DegToRadians ) );
				ChordalVel = RotorVel + InducedVel * cos_AzimuthAng;
				NormalVel = InducedVel * sin_AzimuthAng;
				RelFlowVel = std::sqrt( pow_2( ChordalVel ) + pow_2( NormalVel ) );

				// Angle of attack
				AngOfAttack = std::atan( ( sin_AzimuthAng / ( ( RotorVel / LocalWindSpeed ) / ( InducedVel / LocalWindSpeed ) + cos_AzimuthAng ) ) );

				// Force coefficients
				Real64 const sin_AngOfAttack( std::sin( AngOfAttack * DegToRadians ) );
				Real64 const cos_AngOfAttack( std::cos( AngOfAttack * DegToRadians ) );
				TanForceCoeff = std::abs( WindTurbineSys( WindTurbineNum ).LiftCoeff * sin_AngOfAttack - WindTurbineSys( WindTurbineNum ).DragCoeff * cos_AngOfAttack );
				NorForceCoeff = WindTurbineSys( WindTurbineNum ).LiftCoeff * cos_AngOfAttack + WindTurbineSys( WindTurbineNum ).DragCoeff * sin_AngOfAttack;

				// Net tangential and normal forces
				Real64 const RelFlowVel_2( pow_2( RelFlowVel ) );
				Real64 const density_fac( 0.5 * LocalAirDensity * WindTurbineSys( WindTurbineNum ).ChordArea * RelFlowVel_2 );
				TanForce = TanForceCoeff * density_fac;
				NorForce = NorForceCoeff * density_fac;
				Constant = ( 1.0 / Period ) * ( TanForce / RelFlowVel_2 );

				// Relative flow velocity is the only function of theta in net tangential force
				// Integral of cos(theta) on zero to 2pi goes to zero
				// Integrate constants only
				IntRelFlowVel = pow_2( RotorVel ) * Period + pow_2( InducedVel ) * Period;

				// Average tangential force on a single blade
				AvgTanForce = Constant * IntRelFlowVel;
				TotTorque = WindTurbineSys( WindTurbineNum ).NumOfBlade * AvgTanForce * ( RotorD / 2.0 );
				WTPower = TotTorque * Omega;

				// Check if power produced is greater than maximum or rated power
				if ( WTPower > WindTurbineSys( WindTurbineNum ).RatedPower ) {
					WTPower = WindTurbineSys( WindTurbineNum ).RatedPower;
				}

				WindTurbineSys( WindTurbineNum ).ChordalVel = ChordalVel;
				WindTurbineSys( WindTurbineNum ).NormalVel = NormalVel;
				WindTurbineSys( WindTurbineNum ).RelFlowVel = RelFlowVel;
				WindTurbineSys( WindTurbineNum ).TanForce = TanForce;
				WindTurbineSys( WindTurbineNum ).NorForce = NorForce;
				WindTurbineSys( WindTurbineNum ).TotTorque = TotTorque;

			} else {
				assert( false );
			}}

			if ( WTPower > WindTurbineSys( WindTurbineNum ).RatedPower ) {
				WTPower = WindTurbineSys( WindTurbineNum ).RatedPower;
			}

			// Actual power generated by the wind turbine system
			Power = WTPower * WindTurbineSys( WindTurbineNum ).SysEfficiency;

			WindTurbineSys( WindTurbineNum ).Power = Power;
			WindTurbineSys( WindTurbineNum ).TotPower = WTPower;
			WindTurbineSys( WindTurbineNum ).LocalWindSpeed = LocalWindSpeed;
			WindTurbineSys( WindTurbineNum ).LocalAirDensity = LocalAirDensity;
			WindTurbineSys( WindTurbineNum ).TipSpeedRatio = TipSpeedRatio;

		} else { // System is off
			WindTurbineSys( WindTurbineNum ).Power = 0.0;
			WindTurbineSys( WindTurbineNum ).TotPower = 0.0;
			WindTurbineSys( WindTurbineNum ).PowerCoeff = 0.0;
			WindTurbineSys( WindTurbineNum ).LocalWindSpeed = LocalWindSpeed;
			WindTurbineSys( WindTurbineNum ).LocalAirDensity = LocalAirDensity;
			WindTurbineSys( WindTurbineNum ).TipSpeedRatio = 0.0;
			WindTurbineSys( WindTurbineNum ).ChordalVel = 0.0;
			WindTurbineSys( WindTurbineNum ).NormalVel = 0.0;
			WindTurbineSys( WindTurbineNum ).RelFlowVel = 0.0;
			WindTurbineSys( WindTurbineNum ).AngOfAttack = 0.0;
			WindTurbineSys( WindTurbineNum ).TanForce = 0.0;
			WindTurbineSys( WindTurbineNum ).NorForce = 0.0;
			WindTurbineSys( WindTurbineNum ).TotTorque = 0.0;
		}

	}

	void
	ReportWindTurbine( int const WindTurbineNum )
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Daeho Kang
		//       DATE WRITTEN   October 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine fills remaining report variables.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHVACGlobals::TimeStepSys;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		WindTurbineSys( WindTurbineNum ).Energy = WindTurbineSys( WindTurbineNum ).Power * TimeStepSys * SecInHour;

	}

	//*****************************************************************************************

} // WindTurbine

} // EnergyPlus
