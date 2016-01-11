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
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EarthTube.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSurfaces.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <OutputProcessor.hh>
#include <Psychrometrics.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace EarthTube {
	// Module containing the data for Earth Tube system

	// MODULE INFORMATION:
	//       AUTHOR         Kwang Ho Lee
	//       DATE WRITTEN   November 2005
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To encapsulate the data and algorithyms required to manage the EarthTube System Component

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// 1. M. Krarti, "Analytical Model to Predict Annual Soil Surface Temperature Variation",
	// Journal of Solar Energy Engineering 117, 1995, pp 91-99
	// 2. K. Labs In: J. Cook, editor, "Passive Cooling",
	// Cambridge Massachusetts, MIT Press, 1989, pp 206-212

	// OTHER NOTES: none

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataGlobals;
	using namespace DataEnvironment;
	using namespace DataHeatBalFanSys;
	using namespace DataHeatBalance; // This is the heat balance super block data module
	using namespace DataSurfaces;

	// Use statements for access to subroutines in other modules
	using namespace Psychrometrics;

	// Data
	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLES DECLARATIONS:
	static std::string const BlankString;

	int TotEarthTube( 0 ); // Total EarthTube Statements in input
	// Parameters for Ventilation
	int const NaturalEarthTube( 0 );
	int const IntakeEarthTube( 1 );
	int const ExhaustEarthTube( 2 );

	//         Subroutine Specifications for the Heat Balance Module
	// Driver Routines

	// Get Input routines for module

	// Algorithms for the module

	// Reporting routines for module

	// Object Data
	Array1D< EarthTubeData > EarthTubeSys;
	Array1D< EarthTubeZoneReportVars > ZnRptET;

	// MODULE SUBROUTINES:
	//*************************************************************************

	// Functions

	void
	ManageEarthTube()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Kwang Ho Lee
		//       DATE WRITTEN   November 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine manages the simulation of EarthTube unit.
		// This driver manages the calls to all of
		// the other drivers and simulation algorithms.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool GetInputFlag( true );
		static bool ErrorsFound( false );

		// Obtains and Allocates heat balance related parameters from input file
		if ( GetInputFlag ) {
			GetEarthTube( ErrorsFound );
			GetInputFlag = false;
		}

		if ( TotEarthTube == 0 ) return;

		CalcEarthTube();

		ReportEarthTube();

	}

	void
	GetEarthTube( bool & ErrorsFound ) // If errors found in input
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Kwang Ho Lee
		//       DATE WRITTEN   November 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine obtains input data for EarthTube units and
		// stores it in the EarthTube data structure.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using InputProcessor::VerifyName;
		using ScheduleManager::GetScheduleIndex;
		using ScheduleManager::GetScheduleValuesForDay;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const EarthTubeTempLimit( 100.0 ); // degrees Celsius

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//unused1208    CHARACTER(len=MaxNameLength), DIMENSION(10) :: AlphaName
		//unused1208    REAL(r64), DIMENSION(20)              :: IHGNumbers
		int NumAlpha;
		int NumNumber;
		int IOStat;
		int Loop;
		int Loop1;
		Array1D_bool RepVarSet;

		RepVarSet.dimension( NumOfZones, true );

		// Following used for reporting
		ZnRptET.allocate( NumOfZones );

		cCurrentModuleObject = "ZoneEarthtube";
		TotEarthTube = GetNumObjectsFound( cCurrentModuleObject );

		EarthTubeSys.allocate( TotEarthTube );

		for ( Loop = 1; Loop <= TotEarthTube; ++Loop ) {
			GetObjectItem( cCurrentModuleObject, Loop, cAlphaArgs, NumAlpha, rNumericArgs, NumNumber, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			// First Alpha is Zone Name
			EarthTubeSys( Loop ).ZonePtr = FindItemInList( cAlphaArgs( 1 ), Zone );
			if ( EarthTubeSys( Loop ).ZonePtr == 0 ) {
				ShowSevereError( cCurrentModuleObject + ": " + cAlphaFieldNames( 1 ) + " not found=" + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}

			// Second Alpha is Schedule Name
			EarthTubeSys( Loop ).SchedName = cAlphaArgs( 2 );
			EarthTubeSys( Loop ).SchedPtr = GetScheduleIndex( cAlphaArgs( 2 ) );
			if ( EarthTubeSys( Loop ).SchedPtr == 0 ) {
				if ( lAlphaFieldBlanks( 2 ) ) {
					ShowSevereError( cCurrentModuleObject + ": " + cAlphaFieldNames( 2 ) + " is required, missing for " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) );
				} else {
					ShowSevereError( cCurrentModuleObject + ": invalid " + cAlphaFieldNames( 2 ) + " entered=" + cAlphaArgs( 2 ) + " for " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) );
				}
				ErrorsFound = true;
			}

			// Overall parameters and their limits
			EarthTubeSys( Loop ).DesignLevel = rNumericArgs( 1 );

			EarthTubeSys( Loop ).MinTemperature = rNumericArgs( 2 );
			if ( ( EarthTubeSys( Loop ).MinTemperature < -EarthTubeTempLimit ) || ( EarthTubeSys( Loop ).MinTemperature > EarthTubeTempLimit ) ) {
				ShowSevereError( cCurrentModuleObject + ": " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) + " must have a minimum temperature between -" + RoundSigDigits( EarthTubeTempLimit, 0 ) + "C and " + RoundSigDigits( EarthTubeTempLimit, 0 ) + 'C' );
				ShowContinueError( "Entered value=" + RoundSigDigits( EarthTubeSys( Loop ).MinTemperature, 0 ) );
				ErrorsFound = true;
			}

			EarthTubeSys( Loop ).MaxTemperature = rNumericArgs( 3 );
			if ( ( EarthTubeSys( Loop ).MaxTemperature < -EarthTubeTempLimit ) || ( EarthTubeSys( Loop ).MaxTemperature > EarthTubeTempLimit ) ) {
				ShowSevereError( cCurrentModuleObject + ": " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) + " must have a maximum temperature between -" + RoundSigDigits( EarthTubeTempLimit, 0 ) + "C and " + RoundSigDigits( EarthTubeTempLimit, 0 ) + 'C' );
				ShowContinueError( "Entered value=" + RoundSigDigits( EarthTubeSys( Loop ).MaxTemperature, 0 ) );
				ErrorsFound = true;
			}

			EarthTubeSys( Loop ).DelTemperature = rNumericArgs( 4 ); //  3/12/03  Negative del temp now allowed COP

			{ auto const SELECT_CASE_var( cAlphaArgs( 3 ) ); // Fan type character input-->convert to integer
			if ( SELECT_CASE_var == "EXHAUST" ) {
				EarthTubeSys( Loop ).FanType = ExhaustEarthTube;
			} else if ( SELECT_CASE_var == "INTAKE" ) {
				EarthTubeSys( Loop ).FanType = IntakeEarthTube;
			} else if ( ( SELECT_CASE_var == "NATURAL" ) || ( SELECT_CASE_var == "NONE" ) || ( SELECT_CASE_var == BlankString ) ) {
				EarthTubeSys( Loop ).FanType = NaturalEarthTube;
			} else {
				ShowSevereError( cCurrentModuleObject + ": " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) + ", " + cAlphaFieldNames( 3 ) + " invalid=" + cAlphaArgs( 3 ) );
				ErrorsFound = true;
			}}

			EarthTubeSys( Loop ).FanPressure = rNumericArgs( 5 );
			if ( EarthTubeSys( Loop ).FanPressure < 0.0 ) {
				ShowSevereError( cCurrentModuleObject + ": " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) + ", " + cNumericFieldNames( 5 ) + " must be positive, entered value=" + RoundSigDigits( EarthTubeSys( Loop ).FanPressure, 2 ) );
				ErrorsFound = true;
			}

			EarthTubeSys( Loop ).FanEfficiency = rNumericArgs( 6 );
			if ( ( EarthTubeSys( Loop ).FanEfficiency <= 0.0 ) || ( EarthTubeSys( Loop ).FanEfficiency > 1.0 ) ) {
				ShowSevereError( cCurrentModuleObject + ": " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) + ", " + cNumericFieldNames( 6 ) + " must be greater than zero and less than or equal to one, entered value=" + RoundSigDigits( EarthTubeSys( Loop ).FanEfficiency, 2 ) );
				ErrorsFound = true;
			}

			EarthTubeSys( Loop ).r1 = rNumericArgs( 7 );
			if ( EarthTubeSys( Loop ).r1 <= 0.0 ) {
				ShowSevereError( cCurrentModuleObject + ": " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) + ", " + cNumericFieldNames( 7 ) + " must be positive, entered value=" + RoundSigDigits( EarthTubeSys( Loop ).r1, 2 ) );
				ErrorsFound = true;
			}

			EarthTubeSys( Loop ).r2 = rNumericArgs( 8 );
			if ( EarthTubeSys( Loop ).r2 <= 0.0 ) {
				ShowSevereError( cCurrentModuleObject + ": " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) + ", " + cNumericFieldNames( 8 ) + " must be positive, entered value=" + RoundSigDigits( EarthTubeSys( Loop ).r2, 2 ) );
				ErrorsFound = true;
			}

			EarthTubeSys( Loop ).r3 = 2.0 * EarthTubeSys( Loop ).r1;

			EarthTubeSys( Loop ).PipeLength = rNumericArgs( 9 );
			if ( EarthTubeSys( Loop ).PipeLength <= 0.0 ) {
				ShowSevereError( cCurrentModuleObject + ": " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) + ", " + cNumericFieldNames( 9 ) + " must be positive, entered value=" + RoundSigDigits( EarthTubeSys( Loop ).PipeLength, 2 ) );
				ErrorsFound = true;
			}

			EarthTubeSys( Loop ).PipeThermCond = rNumericArgs( 10 );
			if ( EarthTubeSys( Loop ).PipeThermCond <= 0.0 ) {
				ShowSevereError( cCurrentModuleObject + ": " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) + ", " + cNumericFieldNames( 10 ) + " must be positive, entered value=" + RoundSigDigits( EarthTubeSys( Loop ).PipeThermCond, 2 ) );
				ErrorsFound = true;
			}

			EarthTubeSys( Loop ).z = rNumericArgs( 11 );
			if ( EarthTubeSys( Loop ).z <= 0.0 ) {
				ShowSevereError( cCurrentModuleObject + ": " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) + ", " + cNumericFieldNames( 11 ) + " must be positive, entered value=" + RoundSigDigits( EarthTubeSys( Loop ).z, 2 ) );
				ErrorsFound = true;
			}
			if ( EarthTubeSys( Loop ).z <= ( EarthTubeSys( Loop ).r1 + EarthTubeSys( Loop ).r2 + EarthTubeSys( Loop ).r3 ) ) {
				ShowSevereError( cCurrentModuleObject + ": " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) + ", " + cNumericFieldNames( 11 ) + " must be greater than 3*" + cNumericFieldNames( 7 ) + " + " + cNumericFieldNames( 8 ) + " entered value=" + RoundSigDigits( EarthTubeSys( Loop ).z, 2 ) + " ref sum=" + RoundSigDigits( EarthTubeSys( Loop ).r1 + EarthTubeSys( Loop ).r2 + EarthTubeSys( Loop ).r3, 2 ) );
				ErrorsFound = true;
			}

			{ auto const SELECT_CASE_var( cAlphaArgs( 4 ) ); // Soil type character input --> convert to number
			if ( SELECT_CASE_var == "HEAVYANDSATURATED" ) {
				EarthTubeSys( Loop ).SoilThermDiff = 0.0781056;
				EarthTubeSys( Loop ).SoilThermCond = 2.42;
			} else if ( SELECT_CASE_var == "HEAVYANDDAMP" ) {
				EarthTubeSys( Loop ).SoilThermDiff = 0.055728;
				EarthTubeSys( Loop ).SoilThermCond = 1.3;
			} else if ( SELECT_CASE_var == "HEAVYANDDRY" ) {
				EarthTubeSys( Loop ).SoilThermDiff = 0.0445824;
				EarthTubeSys( Loop ).SoilThermCond = 0.865;
			} else if ( SELECT_CASE_var == "LIGHTANDDRY" ) {
				EarthTubeSys( Loop ).SoilThermDiff = 0.024192;
				EarthTubeSys( Loop ).SoilThermCond = 0.346;
			} else {
				ShowSevereError( cCurrentModuleObject + ": " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) + ", " + cAlphaFieldNames( 4 ) + " invalid=" + cAlphaArgs( 4 ) );
				ErrorsFound = true;
			}}

			EarthTubeSys( Loop ).AverSoilSurTemp = rNumericArgs( 12 );
			EarthTubeSys( Loop ).ApmlSoilSurTemp = rNumericArgs( 13 );
			EarthTubeSys( Loop ).SoilSurPhaseConst = int( rNumericArgs( 14 ) );

			// Override any user input for cases where natural ventilation is being used
			if ( EarthTubeSys( Loop ).FanType == NaturalEarthTube ) {
				EarthTubeSys( Loop ).FanPressure = 0.0;
				EarthTubeSys( Loop ).FanEfficiency = 1.0;
			}

			EarthTubeSys( Loop ).ConstantTermCoef = rNumericArgs( 15 );
			EarthTubeSys( Loop ).TemperatureTermCoef = rNumericArgs( 16 );
			EarthTubeSys( Loop ).VelocityTermCoef = rNumericArgs( 17 );
			EarthTubeSys( Loop ).VelocitySQTermCoef = rNumericArgs( 18 );

			if ( EarthTubeSys( Loop ).ZonePtr > 0 ) {
				if ( RepVarSet( EarthTubeSys( Loop ).ZonePtr ) ) {
					RepVarSet( EarthTubeSys( Loop ).ZonePtr ) = false;
					SetupOutputVariable( "Earth Tube Zone Sensible Cooling Energy [J]", ZnRptET( EarthTubeSys( Loop ).ZonePtr ).EarthTubeHeatLoss, "System", "NonState", Zone( EarthTubeSys( Loop ).ZonePtr ).Name );
					SetupOutputVariable( "Earth Tube Zone Sensible Cooling Rate [W]", ZnRptET( EarthTubeSys( Loop ).ZonePtr ).EarthTubeHeatLossRate, "System", "State", Zone( EarthTubeSys( Loop ).ZonePtr ).Name );
					SetupOutputVariable( "Earth Tube Zone Sensible Heating Energy [J]", ZnRptET( EarthTubeSys( Loop ).ZonePtr ).EarthTubeHeatGain, "System", "NonState", Zone( EarthTubeSys( Loop ).ZonePtr ).Name );
					SetupOutputVariable( "Earth Tube Zone Sensible Heating Rate [W]", ZnRptET( EarthTubeSys( Loop ).ZonePtr ).EarthTubeHeatGainRate, "System", "State", Zone( EarthTubeSys( Loop ).ZonePtr ).Name );
					SetupOutputVariable( "Earth Tube Air Flow Volume [m3]", ZnRptET( EarthTubeSys( Loop ).ZonePtr ).EarthTubeVolume, "System", "NonState", Zone( EarthTubeSys( Loop ).ZonePtr ).Name );
					SetupOutputVariable( "Earth Tube Current Density Air Volume Flow Rate [m3/s]", ZnRptET( EarthTubeSys( Loop ).ZonePtr ).EarthTubeVolFlowRate, "System", "State", Zone( EarthTubeSys( Loop ).ZonePtr ).Name );
					SetupOutputVariable( "Earth Tube Standard Density Air Volume Flow Rate [m3/s]", ZnRptET( EarthTubeSys( Loop ).ZonePtr ).EarthTubeVolFlowRateStd, "System", "State", Zone( EarthTubeSys( Loop ).ZonePtr ).Name );
					SetupOutputVariable( "Earth Tube Air Flow Mass [kg]", ZnRptET( EarthTubeSys( Loop ).ZonePtr ).EarthTubeMass, "System", "NonState", Zone( EarthTubeSys( Loop ).ZonePtr ).Name );
					SetupOutputVariable( "Earth Tube Air Mass Flow Rate [kg/s]", ZnRptET( EarthTubeSys( Loop ).ZonePtr ).EarthTubeMassFlowRate, "System", "State", Zone( EarthTubeSys( Loop ).ZonePtr ).Name );
					SetupOutputVariable( "Earth Tube Fan Electric Energy [J]", ZnRptET( EarthTubeSys( Loop ).ZonePtr ).EarthTubeFanElec, "System", "NonState", Zone( EarthTubeSys( Loop ).ZonePtr ).Name, _, "Electricity", _, _, "Building" );
					SetupOutputVariable( "Earth Tube Fan Electric Power [W]", ZnRptET( EarthTubeSys( Loop ).ZonePtr ).EarthTubeFanElecPower, "System", "State", Zone( EarthTubeSys( Loop ).ZonePtr ).Name );
					SetupOutputVariable( "Earth Tube Zone Inlet Air Temperature [C]", ZnRptET( EarthTubeSys( Loop ).ZonePtr ).EarthTubeAirTemp, "System", "State", Zone( EarthTubeSys( Loop ).ZonePtr ).Name );
					SetupOutputVariable( "Earth Tube Ground Interface Temperature [C]", EarthTubeSys( Loop ).GroundTempz1z2t, "System", "State", Zone( EarthTubeSys( Loop ).ZonePtr ).Name );
					SetupOutputVariable( "Earth Tube Outdoor Air Heat Transfer Rate [W]", ZnRptET( EarthTubeSys( Loop ).ZonePtr ).EarthTubeOATreatmentPower, "System", "State", Zone( EarthTubeSys( Loop ).ZonePtr ).Name );
				}
			}
		}

		// Check to make sure there is only on ventilation statement per zone
		for ( Loop = 1; Loop <= TotEarthTube; ++Loop ) {
			for ( Loop1 = Loop + 1; Loop1 <= TotEarthTube - 1; ++Loop1 ) {
				if ( EarthTubeSys( Loop ).ZonePtr == EarthTubeSys( Loop1 ).ZonePtr ) {
					ShowSevereError( cAlphaArgs( 1 ) + " is assigned to more than one " + cCurrentModuleObject );
					ShowContinueError( "Only one such assignment is allowed." );
					ErrorsFound = true;
				}
			}
		}

		if ( ErrorsFound ) {
			ShowFatalError( cCurrentModuleObject + ": Errors getting input.  Program terminates." );
		}

	}

	void
	CalcEarthTube()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Kwang Ho Lee
		//       DATE WRITTEN   November 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine simulates the components making up the EarthTube unit.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using ScheduleManager::GetCurrentScheduleValue;
		using ScheduleManager::GetScheduleIndex;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Loop;
		int NZ;
		Real64 Process1; // Variable Used in the Middle of the Calculation
		Real64 GroundTempz1z2t; // Average Ground Temperature between Depth z1 and z2 at time t

		Real64 AirThermCond; // Thermal Conductivity of Air (W/mC)
		Real64 AirKinemVisco; // Kinematic Viscosity of Air (m2/s)
		Real64 AirThermDiffus; // Thermal Diffusivity of Air (m2/s)
		Real64 Re; // Reynolds Number for Flow Inside Pipe
		Real64 Pr; // Prandtl Number for Flow Inside Pipe
		Real64 Nu; // Nusselt Number for Flow Inside Pipe
		Real64 fa; // Friction Factor of Pipe
		Real64 PipeHeatTransCoef; // Convective Heat Transfer Coefficient at Inner Pipe Surface
		Real64 Rc; // Thermal Resistance due to Convection between Air and Pipe Inner Surface
		Real64 Rp; // Thermal Resistance due to Conduction between Pipe Inner and Outer Surface
		Real64 Rs; // Thermal Resistance due to Conduction between Pipe Outer Surface and Soil
		Real64 Rt; // Total Thermal Resistance between Pipe Air and Soil
		Real64 OverallHeatTransCoef; // Overall Heat Transfer Coefficient of Earth Tube
		Real64 AverPipeAirVel; // Average Pipe Air Velocity (m/s)
		Real64 AirMassFlowRate; // Actual Mass Flow Rate of Air inside Pipe
		Real64 AirSpecHeat; // Specific Heat of Air
		Real64 AirDensity; // Density of Air
		Real64 InsideEnthalpy;
		Real64 OutletAirEnthalpy;
		Real64 InsideDewPointTemp;
		Real64 InsideHumRat;
		static Array1D< Real64 > EVF; // DESIGN EARTHTUBE FLOW RATE (M**3/SEC)

		// Allocate the EVF array
		if ( ! allocated( EVF ) ) EVF.allocate( NumOfZones );

		EVF = 0.0;
		MCPTE = 0.0;
		MCPE = 0.0;
		EAMFL = 0.0;

		for ( Loop = 1; Loop <= TotEarthTube; ++Loop ) {

			NZ = EarthTubeSys( Loop ).ZonePtr;
			EarthTubeSys( Loop ).FanPower = 0.0;
			// Skip this if the zone is below the minimum temperature limit
			if ( MAT( NZ ) < EarthTubeSys( Loop ).MinTemperature ) continue;
			// Skip this if the zone is above the maximum temperature limit
			if ( MAT( NZ ) > EarthTubeSys( Loop ).MaxTemperature ) continue;
			// Skip if below the temperature difference limit
			if ( std::abs( MAT( NZ ) - OutDryBulbTemp ) < EarthTubeSys( Loop ).DelTemperature ) continue;

			AirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, OutDryBulbTemp, OutHumRat );
			AirSpecHeat = PsyCpAirFnWTdb( OutHumRat, OutDryBulbTemp );
			EVF( NZ ) = EarthTubeSys( Loop ).DesignLevel * GetCurrentScheduleValue( EarthTubeSys( Loop ).SchedPtr );
			MCPE( NZ ) = EVF( NZ ) * AirDensity * AirSpecHeat * ( EarthTubeSys( Loop ).ConstantTermCoef + std::abs( OutDryBulbTemp - MAT( NZ ) ) * EarthTubeSys( Loop ).TemperatureTermCoef + WindSpeed * ( EarthTubeSys( Loop ).VelocityTermCoef + WindSpeed * EarthTubeSys( Loop ).VelocitySQTermCoef ) );

			EAMFL( NZ ) = MCPE( NZ ) / AirSpecHeat;
			if ( EarthTubeSys( Loop ).FanEfficiency > 0.0 ) {
				EarthTubeSys( Loop ).FanPower = EAMFL( NZ ) * EarthTubeSys( Loop ).FanPressure / ( EarthTubeSys( Loop ).FanEfficiency * AirDensity );
			}

			AverPipeAirVel = EVF( NZ ) / Pi / pow_2( EarthTubeSys( Loop ).r1 );
			AirMassFlowRate = EVF( NZ ) * AirDensity;

			// Calculation of Average Ground Temperature between Depth z1 and z2 at time t
			GroundTempz1z2t = EarthTubeSys( Loop ).AverSoilSurTemp - EarthTubeSys( Loop ).ApmlSoilSurTemp * std::exp( -EarthTubeSys( Loop ).z * std::sqrt( Pi / 365.0 / EarthTubeSys( Loop ).SoilThermDiff ) ) * std::cos( 2.0 * Pi / 365.0 * ( DayOfYear - EarthTubeSys( Loop ).SoilSurPhaseConst - EarthTubeSys( Loop ).z / 2.0 * std::sqrt( 365.0 / Pi / EarthTubeSys( Loop ).SoilThermDiff ) ) );
			EarthTubeSys( Loop ).GroundTempz1z2t = GroundTempz1z2t;

			// Calculation of Convective Heat Transfer Coefficient at Inner Pipe Surface
			AirThermCond = 0.02442 + 0.6992 * OutDryBulbTemp / 10000.0;
			AirKinemVisco = ( 0.1335 + 0.000925 * OutDryBulbTemp ) / 10000.0;
			AirThermDiffus = ( 0.0014 * OutDryBulbTemp + 0.1872 ) / 10000.0;
			Re = 2.0 * EarthTubeSys( Loop ).r1 * AverPipeAirVel / AirKinemVisco;
			Pr = AirKinemVisco / AirThermDiffus;
			if ( Re <= 2300.0 ) {
				Nu = 3.66;
			} else if ( Re <= 4000.0 ) {
				fa = std::pow( 1.58 * std::log( Re ) - 3.28, -2 );
				Process1 = ( fa / 2.0 ) * ( Re - 1000.0 ) * Pr / ( 1.0 + 12.7 * std::sqrt( fa / 2.0 ) * ( std::pow( Pr, 2.0 / 3.0 ) - 1.0 ) );
				Nu = ( Process1 - 3.66 ) / ( 1700.0 ) * Re + ( 4000.0 * 3.66 - 2300.0 * Process1 ) / 1700.0;
			} else {
				fa = std::pow( 1.58 * std::log( Re ) - 3.28, -2 );
				Nu = ( fa / 2.0 ) * ( Re - 1000.0 ) * Pr / ( 1.0 + 12.7 * std::sqrt( fa / 2.0 ) * ( std::pow( Pr, 2.0 / 3.0 ) - 1.0 ) );
			}
			PipeHeatTransCoef = Nu * AirThermCond / 2.0 / EarthTubeSys( Loop ).r1;

			// Claculation of Thermal Resistance and Overall Heat Transger Coefficient
			Rc = 1.0 / 2.0 / Pi / EarthTubeSys( Loop ).r1 / PipeHeatTransCoef;
			Rp = std::log( ( EarthTubeSys( Loop ).r1 + EarthTubeSys( Loop ).r2 ) / EarthTubeSys( Loop ).r1 ) / 2.0 / Pi / EarthTubeSys( Loop ).PipeThermCond;
			Rs = std::log( ( EarthTubeSys( Loop ).r1 + EarthTubeSys( Loop ).r2 + EarthTubeSys( Loop ).r3 ) / ( EarthTubeSys( Loop ).r1 + EarthTubeSys( Loop ).r2 ) ) / 2.0 / Pi / EarthTubeSys( Loop ).SoilThermCond;
			Rt = Rc + Rp + Rs;
			OverallHeatTransCoef = 1.0 / Rt;

			if ( AirMassFlowRate * AirSpecHeat == 0.0 ) {
				EarthTubeSys( Loop ).InsideAirTemp = GroundTempz1z2t;

			} else {

				//Calculation of Pipe Outlet Air Temperature
				if ( OutDryBulbTemp > GroundTempz1z2t ) {
					Process1 = ( std::log( std::abs( OutDryBulbTemp - GroundTempz1z2t ) ) * AirMassFlowRate * AirSpecHeat - OverallHeatTransCoef * EarthTubeSys( Loop ).PipeLength ) / ( AirMassFlowRate * AirSpecHeat );
					EarthTubeSys( Loop ).InsideAirTemp = std::exp( Process1 ) + GroundTempz1z2t;
				} else if ( OutDryBulbTemp == GroundTempz1z2t ) {
					EarthTubeSys( Loop ).InsideAirTemp = GroundTempz1z2t;
				} else {
					Process1 = ( std::log( std::abs( OutDryBulbTemp - GroundTempz1z2t ) ) * AirMassFlowRate * AirSpecHeat - OverallHeatTransCoef * EarthTubeSys( Loop ).PipeLength ) / ( AirMassFlowRate * AirSpecHeat );
					EarthTubeSys( Loop ).InsideAirTemp = GroundTempz1z2t - std::exp( Process1 );
				}

			}

			InsideDewPointTemp = PsyTdpFnWPb( OutHumRat, OutBaroPress );

			if ( EarthTubeSys( Loop ).InsideAirTemp >= InsideDewPointTemp ) {
				InsideEnthalpy = PsyHFnTdbW( EarthTubeSys( Loop ).InsideAirTemp, OutHumRat );
				// Intake fans will add some heat to the air, raising the temperature for an intake fan...
				if ( EarthTubeSys( Loop ).FanType == IntakeEarthTube ) {
					if ( EAMFL( NZ ) == 0.0 ) {
						OutletAirEnthalpy = InsideEnthalpy;
					} else {
						OutletAirEnthalpy = InsideEnthalpy + EarthTubeSys( Loop ).FanPower / EAMFL( NZ );
					}
					EarthTubeSys( Loop ).AirTemp = PsyTdbFnHW( OutletAirEnthalpy, OutHumRat );
				} else {
					EarthTubeSys( Loop ).AirTemp = EarthTubeSys( Loop ).InsideAirTemp;
				}
				MCPTE( NZ ) = MCPE( NZ ) * EarthTubeSys( Loop ).AirTemp;

			} else {
				InsideHumRat = PsyWFnTdpPb( EarthTubeSys( Loop ).InsideAirTemp, OutBaroPress );
				InsideEnthalpy = PsyHFnTdbW( EarthTubeSys( Loop ).InsideAirTemp, InsideHumRat );
				// Intake fans will add some heat to the air, raising the temperature for an intake fan...
				if ( EarthTubeSys( Loop ).FanType == IntakeEarthTube ) {
					if ( EAMFL( NZ ) == 0.0 ) {
						OutletAirEnthalpy = InsideEnthalpy;
					} else {
						OutletAirEnthalpy = InsideEnthalpy + EarthTubeSys( Loop ).FanPower / EAMFL( NZ );
					}
					EarthTubeSys( Loop ).AirTemp = PsyTdbFnHW( OutletAirEnthalpy, InsideHumRat );
				} else {
					EarthTubeSys( Loop ).AirTemp = EarthTubeSys( Loop ).InsideAirTemp;
				}
				MCPTE( NZ ) = MCPE( NZ ) * EarthTubeSys( Loop ).AirTemp;
			}

		}

	}

	void
	ReportEarthTube()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Kwang Ho Lee
		//       DATE WRITTEN   November 2005
		//       MODIFIED       B. Griffith April 2010 added output reports
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine fills remaining report variables.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHVACGlobals::TimeStepSys;
		using DataEnvironment::StdRhoAir;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ZoneLoop; // Counter for the # of zones (nz)
		int EarthTubeNum; // Counter for EarthTube statements
		Real64 AirDensity;
		Real64 CpAir;
		Real64 ReportingConstant; // reporting constant for this module

		ReportingConstant = TimeStepSys * SecInHour;

		for ( ZoneLoop = 1; ZoneLoop <= NumOfZones; ++ZoneLoop ) { // Start of zone loads report variable update loop ...

			// Break the infiltration load into heat gain and loss components.
			AirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, OutDryBulbTemp, OutHumRat );
			CpAir = PsyCpAirFnWTdb( OutHumRat, OutDryBulbTemp );
			ZnRptET( ZoneLoop ).EarthTubeVolume = ( MCPE( ZoneLoop ) / CpAir / AirDensity ) * ReportingConstant;
			ZnRptET( ZoneLoop ).EarthTubeMass = ( MCPE( ZoneLoop ) / CpAir ) * ReportingConstant;
			ZnRptET( ZoneLoop ).EarthTubeVolFlowRate = MCPE( ZoneLoop ) / CpAir / AirDensity;
			ZnRptET( ZoneLoop ).EarthTubeVolFlowRateStd = MCPE( ZoneLoop ) / CpAir / StdRhoAir;
			ZnRptET( ZoneLoop ).EarthTubeMassFlowRate = MCPE( ZoneLoop ) / CpAir;

			ZnRptET( ZoneLoop ).EarthTubeFanElec = 0.0;
			ZnRptET( ZoneLoop ).EarthTubeAirTemp = 0.0;
			for ( EarthTubeNum = 1; EarthTubeNum <= TotEarthTube; ++EarthTubeNum ) {
				if ( EarthTubeSys( EarthTubeNum ).ZonePtr == ZoneLoop ) {
					ZnRptET( ZoneLoop ).EarthTubeFanElec = EarthTubeSys( EarthTubeNum ).FanPower * ReportingConstant;
					ZnRptET( ZoneLoop ).EarthTubeFanElecPower = EarthTubeSys( EarthTubeNum ).FanPower;

					// Break the EarthTube load into heat gain and loss components.

					if ( ZT( ZoneLoop ) > EarthTubeSys( EarthTubeNum ).AirTemp ) {

						ZnRptET( ZoneLoop ).EarthTubeHeatLoss = MCPE( ZoneLoop ) * ( ZT( ZoneLoop ) - EarthTubeSys( EarthTubeNum ).AirTemp ) * ReportingConstant;
						ZnRptET( ZoneLoop ).EarthTubeHeatLossRate = MCPE( ZoneLoop ) * ( ZT( ZoneLoop ) - EarthTubeSys( EarthTubeNum ).AirTemp );
						ZnRptET( ZoneLoop ).EarthTubeHeatGain = 0.0;
						ZnRptET( ZoneLoop ).EarthTubeHeatGainRate = 0.0;

					} else if ( ZT( ZoneLoop ) <= EarthTubeSys( EarthTubeNum ).AirTemp ) {

						ZnRptET( ZoneLoop ).EarthTubeHeatGain = MCPE( ZoneLoop ) * ( EarthTubeSys( EarthTubeNum ).AirTemp - ZT( ZoneLoop ) ) * ReportingConstant;
						ZnRptET( ZoneLoop ).EarthTubeHeatGainRate = MCPE( ZoneLoop ) * ( EarthTubeSys( EarthTubeNum ).AirTemp - ZT( ZoneLoop ) );
						ZnRptET( ZoneLoop ).EarthTubeHeatLoss = 0.0;
						ZnRptET( ZoneLoop ).EarthTubeHeatLossRate = 0.0;

					}

					ZnRptET( ZoneLoop ).EarthTubeAirTemp = EarthTubeSys( EarthTubeNum ).AirTemp;
					ZnRptET( ZoneLoop ).EarthTubeOATreatmentPower = MCPE( ZoneLoop ) * ( EarthTubeSys( EarthTubeNum ).AirTemp - OutDryBulbTemp );
					break; // DO loop
				}
			}

		} // ... end of zone loads report variable update loop.

	}

	//        End of Module Subroutines for EarthTube

	//*****************************************************************************************

} // EarthTube

} // EnergyPlus
