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
#include <UnitHeater.hh>
#include <BranchNodeConnections.hh>
#include <DataEnvironment.hh>
#include <DataHVACGlobals.hh>
#include <DataHeatBalance.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DataZoneEnergyDemands.hh>
#include <DataZoneEquipment.hh>
#include <Fans.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <HeatingCoils.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <PlantUtilities.hh>
#include <Psychrometrics.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <SteamCoils.hh>
#include <UtilityRoutines.hh>
#include <WaterCoils.hh>

namespace EnergyPlus {

namespace UnitHeater {

	// Module containing the routines dealing with the Unit Heater

	// MODULE INFORMATION:
	//       AUTHOR         Rick Strand
	//       DATE WRITTEN   May 2000
	//       MODIFIED       Brent Griffith, Sept 2010, plant upgrades, fluid properties
	//       MODIFIED       Bereket Nigusse, FSEC, October 2013, Added cycling fan operating mode
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To simulate unit heaters.  It is assumed that unit heaters are zone equipment
	// without any connection to outside air other than through a separately defined
	// air loop.

	// METHODOLOGY EMPLOYED:
	// Units are modeled as a collection of a fan and a heating coil.  The fan
	// can either be a continuously running fan or an on-off fan which turns on
	// only when there is actually a heating load.  This fan control works together
	// with the unit operation schedule to determine what the unit heater actually
	// does at a given point in time.

	// REFERENCES:
	// ASHRAE Systems and Equipment Handbook (SI), 1996. pp. 31.3-31.8
	// Rick Strand's unit heater module which was based upon Fred Buhl's fan coil
	// module (FanCoilUnits.cc)

	// OTHER NOTES: none

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataLoopNode;
	using DataGlobals::BeginEnvrnFlag;
	using DataGlobals::BeginDayFlag;
	using DataGlobals::InitConvTemp;
	using DataGlobals::SysSizingCalc;
	using DataGlobals::ScheduleAlwaysOn;
	using DataGlobals::DisplayExtraWarnings;
	using DataHVACGlobals::SmallMassFlow;
	using DataHVACGlobals::SmallLoad;
	using DataHVACGlobals::FanElecPower;
	using DataHVACGlobals::SmallAirVolFlow;
	using DataHVACGlobals::cFanTypes;
	using DataHVACGlobals::CycFanCycCoil;
	using DataHVACGlobals::ContFanCycCoil;

	// Use statements for access to subroutines in other modules
	using namespace ScheduleManager;
	using Psychrometrics::PsyRhoAirFnPbTdbW;
	using Psychrometrics::PsyHFnTdbW;
	using Psychrometrics::PsyCpAirFnWTdb;
	using namespace FluidProperties;

	// Data
	// MODULE PARAMETER DEFINITIONS
	std::string const cMO_UnitHeater( "ZoneHVAC:UnitHeater" );

	// Character parameters for outside air control types:
	std::string const ElectricCoil( "ElectricCoil" );
	std::string const GasCoil( "GasCoil" );
	std::string const WaterCoil( "WaterCoil" );
	std::string const SteamCoil( "SteamCoil" );

	static std::string const fluidNameSteam( "STEAM" );

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:

	namespace {
		// These were static variables within different functions. They were pulled out into the namespace
		// to facilitate easier unit testing of those functions.
		// These are purposefully not in the header file as an extern variable. No one outside of this should
		// use these. They are cleared by clear_state() for use by unit tests, but normal simulations should be unaffected.
		// This is purposefully in an anonymous namespace so nothing outside this implementation file can use it.
		bool InitUnitHeaterOneTimeFlag( true );
		bool GetUnitHeaterInputFlag( true );
	}

	bool HCoilOn; // TRUE if the heating coil (gas or electric especially) should be running
	int NumOfUnitHeats; // Number of unit heaters in the input file
	Real64 QZnReq; // heating or cooling needed by zone [watts]
	Array1D_bool MySizeFlag;
	Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE UnitHeater

	// Object Data
	Array1D< UnitHeaterData > UnitHeat;
	Array1D< UnitHeatNumericFieldData > UnitHeatNumericFields;

	// Functions

	void
	clear_state()
	{
		HCoilOn = false;
		NumOfUnitHeats = 0;
		QZnReq = 0.0;
		MySizeFlag.deallocate();
		CheckEquipName.deallocate();
		UnitHeat.deallocate();
		UnitHeatNumericFields.deallocate();
		InitUnitHeaterOneTimeFlag = true;
		GetUnitHeaterInputFlag = true;
	}

	void
	SimUnitHeater(
		std::string const & CompName, // name of the fan coil unit
		int const ZoneNum, // number of zone being served
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & PowerMet, // Sensible power supplied (W)
		Real64 & LatOutputProvided, // Latent add/removal supplied by window AC (kg/s), dehumid = negative
		int & CompIndex
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   May 2000
		//       MODIFIED       Don Shirey, Aug 2009 (LatOutputProvided)
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This is the main driver subroutine for the Unit Heater simulation.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using DataSizing::ZoneHeatingOnlyFan;
		using General::TrimSigDigits;
		using DataSizing::ZoneEqUnitHeater;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int UnitHeatNum; // index of unit heater being simulated

		// FLOW:
		if ( GetUnitHeaterInputFlag ) {
			GetUnitHeaterInput();
			GetUnitHeaterInputFlag = false;
		}

		// Find the correct Unit Heater Equipment
		if ( CompIndex == 0 ) {
			UnitHeatNum = FindItemInList( CompName, UnitHeat );
			if ( UnitHeatNum == 0 ) {
				ShowFatalError( "SimUnitHeater: Unit not found=" + CompName );
			}
			CompIndex = UnitHeatNum;
		} else {
			UnitHeatNum = CompIndex;
			if ( UnitHeatNum > NumOfUnitHeats || UnitHeatNum < 1 ) {
				ShowFatalError( "SimUnitHeater:  Invalid CompIndex passed=" + TrimSigDigits( UnitHeatNum ) + ", Number of Units=" + TrimSigDigits( NumOfUnitHeats ) + ", Entered Unit name=" + CompName );
			}
			if ( CheckEquipName( UnitHeatNum ) ) {
				if ( CompName != UnitHeat( UnitHeatNum ).Name ) {
					ShowFatalError( "SimUnitHeater: Invalid CompIndex passed=" + TrimSigDigits( UnitHeatNum ) + ", Unit name=" + CompName + ", stored Unit Name for that index=" + UnitHeat( UnitHeatNum ).Name );
				}
				CheckEquipName( UnitHeatNum ) = false;
			}
		}

		ZoneEqUnitHeater = true;

		InitUnitHeater( UnitHeatNum, ZoneNum, FirstHVACIteration );

		ZoneHeatingOnlyFan = true;

		CalcUnitHeater( UnitHeatNum, ZoneNum, FirstHVACIteration, PowerMet, LatOutputProvided );

		ZoneHeatingOnlyFan = false;

		//  CALL UpdateUnitHeater

		ReportUnitHeater( UnitHeatNum );

		ZoneEqUnitHeater = false;
	}

	void
	GetUnitHeaterInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   May 2000
		//       MODIFIED       Chandan Sharma, FSEC, March 2011: Added ZoneHVAC sys avail manager
		//                      Bereket Nigusse, FSEC, April 2011: eliminated input node names
		//                                                         & added fan object type
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Obtain the user input data for all of the unit heaters in the input file.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// REFERENCES:
		// Fred Buhl's fan coil module (FanCoilUnits.cc)

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using InputProcessor::FindItemInList;
		using InputProcessor::GetObjectDefMaxArgs;
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::SetUpCompSets;
		using Fans::GetFanType;
		using Fans::GetFanOutletNode;
		using Fans::GetFanIndex;
		using Fans::GetFanVolFlow;
		using Fans::GetFanAvailSchPtr;
		using WaterCoils::GetCoilWaterInletNode;
		using SteamCoils::GetCoilSteamInletNode;
		using SteamCoils::GetSteamCoilIndex;
		using DataZoneEquipment::ZoneEquipConfig;
		using DataSizing::AutoSize;
		using General::TrimSigDigits;
		using DataHVACGlobals::FanType_SimpleConstVolume;
		using DataHVACGlobals::FanType_SimpleVAV;
		using DataHVACGlobals::FanType_SimpleOnOff;
		using DataGlobals::NumOfZones;
		using DataPlant::TypeOf_CoilWaterSimpleHeating;
		using DataPlant::TypeOf_CoilSteamAirHeating;
		using DataSizing::ZoneHVACSizing;

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
		static bool ErrorsFound( false ); // Set to true if errors in input, fatal at end of routine
		int IOStatus; // Used in GetObjectItem
		bool IsBlank; // TRUE if the name is blank
		bool IsNotOK; // TRUE if there was a problem with a list name
		static bool errFlag( false ); // interim error flag
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		int NumFields; // Total number of fields in object
		int UnitHeatNum; // Item to be "gotten"
		static std::string const RoutineName( "GetUnitHeaterInput: " ); // include trailing blank space
		Real64 FanVolFlow; // Fan volumetric flow rate
		std::string CurrentModuleObject;
		Array1D_string Alphas; // Alpha items for object
		Array1D< Real64 > Numbers; // Numeric items for object
		Array1D_string cAlphaFields; // Alpha field names
		Array1D_string cNumericFields; // Numeric field names
		Array1D_bool lAlphaBlanks; // Logical array, alpha field input BLANK = .TRUE.
		Array1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.
		int CtrlZone; // index to loop counter
		int NodeNum; // index to loop counter
		bool ZoneNodeNotFound; // used in error checking

		// FLOW:

		// Figure out how many unit heaters there are in the input file
		CurrentModuleObject = cMO_UnitHeater;
		NumOfUnitHeats = GetNumObjectsFound( CurrentModuleObject );
		GetObjectDefMaxArgs( CurrentModuleObject, NumFields, NumAlphas, NumNumbers );

		Alphas.allocate( NumAlphas );
		Numbers.dimension( NumNumbers, 0.0 );
		cAlphaFields.allocate( NumAlphas );
		cNumericFields.allocate( NumNumbers );
		lAlphaBlanks.dimension( NumAlphas, true );
		lNumericBlanks.dimension( NumNumbers, true );

		// Allocate the local derived type and do one-time initializations for all parts of it
		if ( NumOfUnitHeats > 0 ) {
			UnitHeat.allocate( NumOfUnitHeats );
			CheckEquipName.allocate( NumOfUnitHeats );
			UnitHeatNumericFields.allocate( NumOfUnitHeats );
		}
		CheckEquipName = true;

		for ( UnitHeatNum = 1; UnitHeatNum <= NumOfUnitHeats; ++UnitHeatNum ) { // Begin looping over all of the unit heaters found in the input file...

			GetObjectItem( CurrentModuleObject, UnitHeatNum, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			UnitHeatNumericFields( UnitHeatNum ).FieldNames.allocate( NumNumbers );
			UnitHeatNumericFields( UnitHeatNum ).FieldNames = "";
			UnitHeatNumericFields( UnitHeatNum ).FieldNames = cNumericFields;

			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), UnitHeat, UnitHeatNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}

			UnitHeat( UnitHeatNum ).Name = Alphas( 1 );
			UnitHeat( UnitHeatNum ).SchedName = Alphas( 2 );
			if ( lAlphaBlanks( 2 ) ) {
				UnitHeat( UnitHeatNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				UnitHeat( UnitHeatNum ).SchedPtr = GetScheduleIndex( Alphas( 2 ) ); // convert schedule name to pointer
				if ( UnitHeat( UnitHeatNum ).SchedPtr == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + ": invalid " + cAlphaFields( 2 ) + " entered =" + Alphas( 2 ) + " for " + cAlphaFields( 1 ) + '=' + Alphas( 1 ) );
					ErrorsFound = true;
				}
			}

			// Main air nodes (except outside air node):
			UnitHeat( UnitHeatNum ).AirInNode = GetOnlySingleNode( Alphas( 3 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsParent );

			UnitHeat( UnitHeatNum ).AirOutNode = GetOnlySingleNode( Alphas( 4 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsParent );

			// Fan information:
			UnitHeat( UnitHeatNum ).FanType = Alphas( 5 );
			UnitHeat( UnitHeatNum ).FanName = Alphas( 6 );
			UnitHeat( UnitHeatNum ).MaxAirVolFlow = Numbers( 1 );

			errFlag = false;
			ValidateComponent( UnitHeat( UnitHeatNum ).FanType, UnitHeat( UnitHeatNum ).FanName, errFlag, CurrentModuleObject );
			if ( errFlag ) {
				ShowContinueError( "specified in " + CurrentModuleObject + " = \"" + UnitHeat( UnitHeatNum ).Name + "\"." );
				ErrorsFound = true;
			} else {
				GetFanType( UnitHeat( UnitHeatNum ).FanName, UnitHeat( UnitHeatNum ).FanType_Num, errFlag, CurrentModuleObject, UnitHeat( UnitHeatNum ).Name );

				{ auto const SELECT_CASE_var( UnitHeat( UnitHeatNum ).FanType_Num );
				if ( ( SELECT_CASE_var == FanType_SimpleConstVolume ) || ( SELECT_CASE_var == FanType_SimpleVAV ) || ( SELECT_CASE_var == FanType_SimpleOnOff ) ) {
					// Get fan outlet node
					UnitHeat( UnitHeatNum ).FanOutletNode = GetFanOutletNode( UnitHeat( UnitHeatNum ).FanType, UnitHeat( UnitHeatNum ).FanName, errFlag );
					if ( errFlag ) {
						ShowContinueError( "specified in " + CurrentModuleObject + " = \"" + UnitHeat( UnitHeatNum ).Name + "\"." );
						ErrorsFound = true;
					}
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + " = \"" + Alphas( 1 ) + "\"" );
					ShowContinueError( "Fan Type must be Fan:ConstantVolume or Fan:VariableVolume" );
					ErrorsFound = true;
				}}
				GetFanIndex( UnitHeat( UnitHeatNum ).FanName, UnitHeat( UnitHeatNum ).Fan_Index, errFlag, CurrentModuleObject );
				if ( errFlag ) {
					ErrorsFound = true;
				} else {
					GetFanVolFlow( UnitHeat( UnitHeatNum ).Fan_Index, FanVolFlow );

					if ( FanVolFlow != AutoSize && UnitHeat( UnitHeatNum ).MaxAirVolFlow != AutoSize && FanVolFlow < UnitHeat( UnitHeatNum ).MaxAirVolFlow ) {
						ShowSevereError( "Specified in " + CurrentModuleObject + " = " + UnitHeat( UnitHeatNum ).Name );
						ShowContinueError( "...air flow rate (" + TrimSigDigits( FanVolFlow, 7 ) + ") in fan object " + UnitHeat( UnitHeatNum ).FanName + " is less than the unit heater maximum supply air flow rate (" + TrimSigDigits( UnitHeat( UnitHeatNum ).MaxAirVolFlow, 7 ) + ")." );
						ShowContinueError( "...the fan flow rate must be greater than or equal to the unit heater maximum supply air flow rate." );
						ErrorsFound = true;
					} else if ( FanVolFlow == AutoSize && UnitHeat( UnitHeatNum ).MaxAirVolFlow != AutoSize ) {
						ShowWarningError( "Specified in " + CurrentModuleObject + " = " + UnitHeat( UnitHeatNum ).Name );
						ShowContinueError( "...the fan flow rate is autosized while the unit heater flow rate is not." );
						ShowContinueError( "...this can lead to unexpected results where the fan flow rate is less than required." );
					} else if ( FanVolFlow != AutoSize && UnitHeat( UnitHeatNum ).MaxAirVolFlow == AutoSize ) {
						ShowWarningError( "Specified in " + CurrentModuleObject + " = " + UnitHeat( UnitHeatNum ).Name );
						ShowContinueError( "...the unit heater flow rate is autosized while the fan flow rate is not." );
						ShowContinueError( "...this can lead to unexpected results where the fan flow rate is less than required." );
					}
					UnitHeat( UnitHeatNum ).FanAvailSchedPtr = GetFanAvailSchPtr( UnitHeat( UnitHeatNum ).FanType, UnitHeat( UnitHeatNum ).FanName, errFlag );
				}
			}

			// Heating coil information:
			{ auto const SELECT_CASE_var( Alphas( 7 ) );
			if ( SELECT_CASE_var == "COIL:HEATING:WATER" ) {
				UnitHeat( UnitHeatNum ).HCoilType = WaterCoil;
				UnitHeat( UnitHeatNum ).HCoil_PlantTypeNum = TypeOf_CoilWaterSimpleHeating;
			} else if ( SELECT_CASE_var == "COIL:HEATING:STEAM" ) {
				UnitHeat( UnitHeatNum ).HCoilType = SteamCoil;
				UnitHeat( UnitHeatNum ).HCoil_PlantTypeNum = TypeOf_CoilSteamAirHeating;
			} else if ( SELECT_CASE_var == "COIL:HEATING:ELECTRIC" ) {
				UnitHeat( UnitHeatNum ).HCoilType = ElectricCoil;
			} else if ( SELECT_CASE_var == "COIL:HEATING:GAS" ) {
				UnitHeat( UnitHeatNum ).HCoilType = GasCoil;
			} else {
				ShowSevereError( "Illegal " + cAlphaFields( 7 ) + " = " + Alphas( 7 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + '=' + UnitHeat( UnitHeatNum ).Name );
				ErrorsFound = true;
				errFlag = true;
			}}
			if ( ! errFlag ) {
				UnitHeat( UnitHeatNum ).HCoilTypeCh = Alphas( 7 );
				UnitHeat( UnitHeatNum ).HCoilName = Alphas( 8 );
				ValidateComponent( Alphas( 7 ), UnitHeat( UnitHeatNum ).HCoilName, IsNotOK, CurrentModuleObject );
				if ( IsNotOK ) {
					ShowContinueError( "specified in " + CurrentModuleObject + " = \"" + UnitHeat( UnitHeatNum ).Name + "\"" );
					ErrorsFound = true;
				} else {
					// The heating coil control node is necessary for hot water and steam coils, but not necessary for an
					// electric or gas coil.
					if ( UnitHeat( UnitHeatNum ).HCoilType == WaterCoil || UnitHeat( UnitHeatNum ).HCoilType == SteamCoil ) {
						// mine the hot water or steam node from the coil object
						errFlag = false;
						if ( UnitHeat( UnitHeatNum ).HCoilType == WaterCoil ) {
							UnitHeat( UnitHeatNum ).HotControlNode = GetCoilWaterInletNode( "Coil:Heating:Water", UnitHeat( UnitHeatNum ).HCoilName, errFlag );
						} else { // its a steam coil
							UnitHeat( UnitHeatNum ).HCoil_Index = GetSteamCoilIndex( "COIL:HEATING:STEAM", UnitHeat( UnitHeatNum ).HCoilName, errFlag );
							UnitHeat( UnitHeatNum ).HotControlNode = GetCoilSteamInletNode( UnitHeat( UnitHeatNum ).HCoil_Index, UnitHeat( UnitHeatNum ).HCoilName, errFlag );
						}
						// Other error checks should trap before it gets to this point in the code, but including just in case.
						if ( errFlag ) {
							ShowContinueError( "that was specified in " + CurrentModuleObject + " = \"" + UnitHeat( UnitHeatNum ).Name + "\"" );
							ErrorsFound = true;
						}
					}
				}
			}

			UnitHeat( UnitHeatNum ).FanSchedPtr = GetScheduleIndex( Alphas( 9 ) );
			// Default to cycling fan when fan operating mode schedule is not present
			if ( ! lAlphaBlanks( 9 ) && UnitHeat( UnitHeatNum ).FanSchedPtr == 0 ) {
				ShowSevereError( CurrentModuleObject + " \"" + UnitHeat( UnitHeatNum ).Name + "\" " + cAlphaFields( 9 ) + " not found: " + Alphas( 9 ) );
				ErrorsFound = true;
			} else if ( lAlphaBlanks( 9 ) ) {
				if ( UnitHeat( UnitHeatNum ).FanType_Num == FanType_SimpleOnOff ) {
					UnitHeat( UnitHeatNum ).OpMode = CycFanCycCoil;
				} else {
					UnitHeat( UnitHeatNum ).OpMode = ContFanCycCoil;
				}
			}

			// Check fan's schedule for cycling fan operation if constant volume fan is used
			if ( UnitHeat( UnitHeatNum ).FanSchedPtr > 0 && UnitHeat( UnitHeatNum ).FanType_Num == FanType_SimpleConstVolume ) {
				if ( ! CheckScheduleValueMinMax( UnitHeat( UnitHeatNum ).FanSchedPtr, ">", 0.0, "<=", 1.0 ) ) {
					ShowSevereError( CurrentModuleObject + " = " + Alphas( 1 ) );
					ShowContinueError( "For " + cAlphaFields( 5 ) + " = " + Alphas( 5 ) );
					ShowContinueError( "Fan operating mode must be continuous (fan operating mode schedule values > 0)." );
					ShowContinueError( "Error found in " + cAlphaFields( 9 ) + " = " + Alphas( 9 ) );
					ShowContinueError( "...schedule values must be (>0., <=1.)" );
					ErrorsFound = true;
				}
			}

			UnitHeat( UnitHeatNum ).FanOperatesDuringNoHeating = Alphas( 10 );
			if ( ( ! SameString( UnitHeat( UnitHeatNum ).FanOperatesDuringNoHeating, "Yes" ) ) && ( ! SameString( UnitHeat( UnitHeatNum ).FanOperatesDuringNoHeating, "No" ) ) ) {
				ErrorsFound = true;
				ShowSevereError( "Illegal " + cAlphaFields( 10 ) + " = " + Alphas( 10 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + '=' + UnitHeat( UnitHeatNum ).Name );
			} else if ( SameString( UnitHeat( UnitHeatNum ).FanOperatesDuringNoHeating, "No" ) ) {
				UnitHeat( UnitHeatNum ).FanOffNoHeating = true;
			}

			UnitHeat( UnitHeatNum ).MaxVolHotWaterFlow = Numbers( 2 );
			UnitHeat( UnitHeatNum ).MinVolHotWaterFlow = Numbers( 3 );
			UnitHeat( UnitHeatNum ).MaxVolHotSteamFlow = Numbers( 2 );
			UnitHeat( UnitHeatNum ).MinVolHotSteamFlow = Numbers( 3 );

			UnitHeat( UnitHeatNum ).HotControlOffset = Numbers( 4 );
			// Set default convergence tolerance
			if ( UnitHeat( UnitHeatNum ).HotControlOffset <= 0.0 ) {
				UnitHeat( UnitHeatNum ).HotControlOffset = 0.001;
			}

			if ( ! lAlphaBlanks( 11 ) ) {
				UnitHeat( UnitHeatNum ).AvailManagerListName = Alphas( 11 );
			}

			UnitHeat( UnitHeatNum ).HVACSizingIndex = 0;
			if ( ! lAlphaBlanks( 12 )) {
				UnitHeat( UnitHeatNum ).HVACSizingIndex = FindItemInList( Alphas( 12 ), ZoneHVACSizing );
				if (UnitHeat( UnitHeatNum ).HVACSizingIndex == 0) {
					ShowSevereError( cAlphaFields( 12 ) + " = " + Alphas( 12 ) + " not found.");
					ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + UnitHeat( UnitHeatNum ).Name );
					ErrorsFound = true;
				}
			}

			// check that unit heater air inlet node must be the same as a zone exhaust node
			ZoneNodeNotFound = true;
			for ( CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone ) {
				if ( ! ZoneEquipConfig( CtrlZone ).IsControlled ) continue;
				for ( NodeNum = 1; NodeNum <= ZoneEquipConfig( CtrlZone ).NumExhaustNodes; ++NodeNum ) {
					if ( UnitHeat( UnitHeatNum ).AirInNode == ZoneEquipConfig( CtrlZone ).ExhaustNode( NodeNum ) ) {
						ZoneNodeNotFound = false;
						break;
					}
				}
			}
			if ( ZoneNodeNotFound ) {
				ShowSevereError( CurrentModuleObject + " = \"" + UnitHeat( UnitHeatNum ).Name + "\". Unit heater air inlet node name must be the same as a zone exhaust node name." );
				ShowContinueError( "..Zone exhaust node name is specified in ZoneHVAC:EquipmentConnections object." );
				ShowContinueError( "..Unit heater air inlet node name = " + NodeID( UnitHeat( UnitHeatNum ).AirInNode ) );
				ErrorsFound = true;
			}
			// check that unit heater air outlet node is a zone inlet node.
			ZoneNodeNotFound = true;
			for ( CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone ) {
				if ( ! ZoneEquipConfig( CtrlZone ).IsControlled ) continue;
				for ( NodeNum = 1; NodeNum <= ZoneEquipConfig( CtrlZone ).NumInletNodes; ++NodeNum ) {
					if ( UnitHeat( UnitHeatNum ).AirOutNode == ZoneEquipConfig( CtrlZone ).InletNode( NodeNum ) ) {
						UnitHeat( UnitHeatNum ).ZonePtr = CtrlZone;
						ZoneNodeNotFound = false;
						break;
					}
				}
			}
			if ( ZoneNodeNotFound ) {
				ShowSevereError( CurrentModuleObject + " = \"" + UnitHeat( UnitHeatNum ).Name + "\". Unit heater air outlet node name must be the same as a zone inlet node name." );
				ShowContinueError( "..Zone inlet node name is specified in ZoneHVAC:EquipmentConnections object." );
				ShowContinueError( "..Unit heater air outlet node name = " + NodeID( UnitHeat( UnitHeatNum ).AirOutNode ) );
				ErrorsFound = true;
			}

			// Add fan to component sets array
			SetUpCompSets( CurrentModuleObject, UnitHeat( UnitHeatNum ).Name, UnitHeat( UnitHeatNum ).FanType, UnitHeat( UnitHeatNum ).FanName, NodeID( UnitHeat( UnitHeatNum ).AirInNode ), NodeID( UnitHeat( UnitHeatNum ).FanOutletNode ) );

			// Add heating coil to component sets array
			SetUpCompSets( CurrentModuleObject, UnitHeat( UnitHeatNum ).Name, UnitHeat( UnitHeatNum ).HCoilTypeCh, UnitHeat( UnitHeatNum ).HCoilName, NodeID( UnitHeat( UnitHeatNum ).FanOutletNode ), NodeID( UnitHeat( UnitHeatNum ).AirOutNode ) );

		} // ...loop over all of the unit heaters found in the input file

		Alphas.deallocate();
		Numbers.deallocate();
		cAlphaFields.deallocate();
		cNumericFields.deallocate();
		lAlphaBlanks.deallocate();
		lNumericBlanks.deallocate();

		if ( ErrorsFound ) ShowFatalError( RoutineName + "Errors found in input" );

		// Setup Report variables for the Unit Heaters, CurrentModuleObject='ZoneHVAC:UnitHeater'
		for ( UnitHeatNum = 1; UnitHeatNum <= NumOfUnitHeats; ++UnitHeatNum ) {
			SetupOutputVariable( "Zone Unit Heater Heating Rate [W]", UnitHeat( UnitHeatNum ).HeatPower, "System", "Average", UnitHeat( UnitHeatNum ).Name );
			SetupOutputVariable( "Zone Unit Heater Heating Energy [J]", UnitHeat( UnitHeatNum ).HeatEnergy, "System", "Sum", UnitHeat( UnitHeatNum ).Name );
			SetupOutputVariable( "Zone Unit Heater Fan Electric Power [W]", UnitHeat( UnitHeatNum ).ElecPower, "System", "Average", UnitHeat( UnitHeatNum ).Name );
			// Note that the unit heater fan electric is NOT metered because this value is already metered through the fan component
			SetupOutputVariable( "Zone Unit Heater Fan Electric Energy [J]", UnitHeat( UnitHeatNum ).ElecEnergy, "System", "Sum", UnitHeat( UnitHeatNum ).Name );
			SetupOutputVariable( "Zone Unit Heater Fan Availability Status []", UnitHeat( UnitHeatNum ).AvailStatus, "System", "Average", UnitHeat( UnitHeatNum ).Name );
			if ( UnitHeat( UnitHeatNum ).FanType_Num == FanType_SimpleOnOff ) {
				SetupOutputVariable( "Zone Unit Heater Fan Part Load Ratio []", UnitHeat( UnitHeatNum ).FanPartLoadRatio, "System", "Average", UnitHeat( UnitHeatNum ).Name );
			}
		}

	}

	void
	InitUnitHeater(
		int const UnitHeatNum, // index for the current unit heater
		int const ZoneNum, // number of zone being served
		bool const EP_UNUSED( FirstHVACIteration ) // TRUE if 1st HVAC simulation of system timestep
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   May 2000
		//       MODIFIED       Chandan Sharma, FSEC, March 2011: Added ZoneHVAC sys avail manager
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine initializes all of the data elements which are necessary
		// to simulate a unit heater.

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger initializations.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataEnvironment::StdRhoAir;
		using DataZoneEquipment::ZoneEquipInputsFilled;
		using DataZoneEquipment::CheckZoneEquipmentList;
		using DataZoneEquipment::UnitHeater_Num;
		using DataHVACGlobals::ZoneComp;
		using DataHVACGlobals::ZoneCompTurnFansOn;
		using DataHVACGlobals::ZoneCompTurnFansOff;
		using DataHVACGlobals::FanType_SimpleOnOff;
		using DataPlant::PlantLoop;
		using DataPlant::TypeOf_CoilWaterSimpleHeating;
		using DataPlant::TypeOf_CoilSteamAirHeating;
		using DataPlant::ScanPlantLoopsForObject;
		using FluidProperties::GetDensityGlycol;
		using PlantUtilities::InitComponentNodes;
		using PlantUtilities::SetComponentFlowRate;
		using DataGlobals::AnyPlantInModel;
		using namespace DataZoneEnergyDemands;
		using WaterCoils::SimulateWaterCoilComponents;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "InitUnitHeater" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static Array1D_bool MyEnvrnFlag;
		static Array1D_bool MyPlantScanFlag;
		static Array1D_bool MyZoneEqFlag; // used to set up zone equipment availability managers
		static bool ZoneEquipmentListChecked( false ); // True after the Zone Equipment List has been checked for items
		int Loop;
		int HotConNode; // hot water control node number in unit heater loop
		int InNode; // inlet node number in unit heater loop
		int OutNode; // outlet node number in unit heater loop
		Real64 RhoAir; // air density at InNode
		Real64 TempSteamIn;
		Real64 SteamDensity;
		Real64 rho; // local fluid density
		bool errFlag;
		static bool SetMassFlowRateToZero( false ); // TRUE when mass flow rates need to be set to zero
		// FLOW:

		// Do the one time initializations
		if ( InitUnitHeaterOneTimeFlag ) {

			MyEnvrnFlag.allocate( NumOfUnitHeats );
			MySizeFlag.allocate( NumOfUnitHeats );
			MyPlantScanFlag.allocate( NumOfUnitHeats );
			MyZoneEqFlag.allocate ( NumOfUnitHeats );
			MyEnvrnFlag = true;
			MySizeFlag = true;
			MyPlantScanFlag = true;
			MyZoneEqFlag = true;
			InitUnitHeaterOneTimeFlag = false;

		}

		if ( allocated( ZoneComp ) ) {
			if ( MyZoneEqFlag( UnitHeatNum ) ) { // initialize the name of each availability manager list and zone number
				ZoneComp( UnitHeater_Num ).ZoneCompAvailMgrs( UnitHeatNum ).AvailManagerListName = UnitHeat( UnitHeatNum ).AvailManagerListName;
				ZoneComp( UnitHeater_Num ).ZoneCompAvailMgrs( UnitHeatNum ).ZoneNum = ZoneNum;
				MyZoneEqFlag ( UnitHeatNum ) = false;
			}
			UnitHeat( UnitHeatNum ).AvailStatus = ZoneComp( UnitHeater_Num ).ZoneCompAvailMgrs( UnitHeatNum ).AvailStatus;
		}

		if ( MyPlantScanFlag( UnitHeatNum ) && allocated( PlantLoop ) ) {
			if ( ( UnitHeat( UnitHeatNum ).HCoil_PlantTypeNum == TypeOf_CoilWaterSimpleHeating ) || ( UnitHeat( UnitHeatNum ).HCoil_PlantTypeNum == TypeOf_CoilSteamAirHeating ) ) {
				errFlag = false;
				ScanPlantLoopsForObject( UnitHeat( UnitHeatNum ).HCoilName, UnitHeat( UnitHeatNum ).HCoil_PlantTypeNum, UnitHeat( UnitHeatNum ).HWLoopNum, UnitHeat( UnitHeatNum ).HWLoopSide, UnitHeat( UnitHeatNum ).HWBranchNum, UnitHeat( UnitHeatNum ).HWCompNum, _, _, _, _, _, errFlag );
				if ( errFlag ) {
					ShowContinueError( "Reference Unit=\"" + UnitHeat( UnitHeatNum ).Name + "\", type=ZoneHVAC:UnitHeater" );
					ShowFatalError( "InitUnitHeater: Program terminated due to previous condition(s)." );
				}

				UnitHeat( UnitHeatNum ).HotCoilOutNodeNum = PlantLoop( UnitHeat( UnitHeatNum ).HWLoopNum ).LoopSide( UnitHeat( UnitHeatNum ).HWLoopSide ).Branch( UnitHeat( UnitHeatNum ).HWBranchNum ).Comp( UnitHeat( UnitHeatNum ).HWCompNum ).NodeNumOut;
			}
			MyPlantScanFlag( UnitHeatNum ) = false;
		} else if ( MyPlantScanFlag( UnitHeatNum ) && ! AnyPlantInModel ) {
			MyPlantScanFlag( UnitHeatNum ) = false;
		}
		// need to check all units to see if they are on Zone Equipment List or issue warning
		if ( ! ZoneEquipmentListChecked && ZoneEquipInputsFilled ) {
			ZoneEquipmentListChecked = true;
			for ( Loop = 1; Loop <= NumOfUnitHeats; ++Loop ) {
				if ( CheckZoneEquipmentList( "ZoneHVAC:UnitHeater", UnitHeat( Loop ).Name ) ) continue;
				ShowSevereError( "InitUnitHeater: Unit=[UNIT HEATER," + UnitHeat( Loop ).Name + "] is not on any ZoneHVAC:EquipmentList.  It will not be simulated." );
			}
		}

		if ( ! SysSizingCalc && MySizeFlag( UnitHeatNum ) && ! MyPlantScanFlag( UnitHeatNum ) ) {

			SizeUnitHeater( UnitHeatNum );

			MySizeFlag( UnitHeatNum ) = false;
		} // Do the one time initializations

		if ( BeginEnvrnFlag && MyEnvrnFlag( UnitHeatNum ) && ! MyPlantScanFlag( UnitHeatNum ) ) {
			InNode = UnitHeat( UnitHeatNum ).AirInNode;
			OutNode = UnitHeat( UnitHeatNum ).AirOutNode;
			HotConNode = UnitHeat( UnitHeatNum ).HotControlNode;
			RhoAir = StdRhoAir;

			// set the mass flow rates from the input volume flow rates
			UnitHeat( UnitHeatNum ).MaxAirMassFlow = RhoAir * UnitHeat( UnitHeatNum ).MaxAirVolFlow;

			// set the node max and min mass flow rates
			Node( OutNode ).MassFlowRateMax = UnitHeat( UnitHeatNum ).MaxAirMassFlow;
			Node( OutNode ).MassFlowRateMin = 0.0;

			Node( InNode ).MassFlowRateMax = UnitHeat( UnitHeatNum ).MaxAirMassFlow;
			Node( InNode ).MassFlowRateMin = 0.0;

			if ( UnitHeat( UnitHeatNum ).HCoilType == WaterCoil ) {
				rho = GetDensityGlycol( PlantLoop( UnitHeat( UnitHeatNum ).HWLoopNum ).FluidName, 60.0, PlantLoop( UnitHeat( UnitHeatNum ).HWLoopNum ).FluidIndex, RoutineName );

				UnitHeat( UnitHeatNum ).MaxHotWaterFlow = rho * UnitHeat( UnitHeatNum ).MaxVolHotWaterFlow;
				UnitHeat( UnitHeatNum ).MinHotWaterFlow = rho * UnitHeat( UnitHeatNum ).MinVolHotWaterFlow;
				InitComponentNodes( UnitHeat( UnitHeatNum ).MinHotWaterFlow, UnitHeat( UnitHeatNum ).MaxHotWaterFlow, UnitHeat( UnitHeatNum ).HotControlNode, UnitHeat( UnitHeatNum ).HotCoilOutNodeNum, UnitHeat( UnitHeatNum ).HWLoopNum, UnitHeat( UnitHeatNum ).HWLoopSide, UnitHeat( UnitHeatNum ).HWBranchNum, UnitHeat( UnitHeatNum ).HWCompNum );
			}
			if ( UnitHeat( UnitHeatNum ).HCoilType == SteamCoil ) {
				TempSteamIn = 100.00;
				SteamDensity = GetSatDensityRefrig( fluidNameSteam, TempSteamIn, 1.0, UnitHeat( UnitHeatNum ).HCoil_FluidIndex, RoutineName );
				UnitHeat( UnitHeatNum ).MaxHotSteamFlow = SteamDensity * UnitHeat( UnitHeatNum ).MaxVolHotSteamFlow;
				UnitHeat( UnitHeatNum ).MinHotSteamFlow = SteamDensity * UnitHeat( UnitHeatNum ).MinVolHotSteamFlow;

				InitComponentNodes( UnitHeat( UnitHeatNum ).MinHotSteamFlow, UnitHeat( UnitHeatNum ).MaxHotSteamFlow, UnitHeat( UnitHeatNum ).HotControlNode, UnitHeat( UnitHeatNum ).HotCoilOutNodeNum, UnitHeat( UnitHeatNum ).HWLoopNum, UnitHeat( UnitHeatNum ).HWLoopSide, UnitHeat( UnitHeatNum ).HWBranchNum, UnitHeat( UnitHeatNum ).HWCompNum );
			}

			MyEnvrnFlag( UnitHeatNum ) = false;
		} // ...end start of environment inits

		if ( ! BeginEnvrnFlag ) MyEnvrnFlag( UnitHeatNum ) = true;

		// These initializations are done every iteration...
		InNode = UnitHeat( UnitHeatNum ).AirInNode;
		OutNode = UnitHeat( UnitHeatNum ).AirOutNode;

		QZnReq = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputRequired; // zone load needed
		if ( UnitHeat( UnitHeatNum ).FanSchedPtr > 0 ) {
			if ( GetCurrentScheduleValue( UnitHeat( UnitHeatNum ).FanSchedPtr ) == 0.0 && UnitHeat( UnitHeatNum ).FanType_Num == FanType_SimpleOnOff ) {
				UnitHeat( UnitHeatNum ).OpMode = CycFanCycCoil;
			} else {
				UnitHeat( UnitHeatNum ).OpMode = ContFanCycCoil;
			}
			if ( ( QZnReq < SmallLoad ) || CurDeadBandOrSetback( ZoneNum ) ) {
				// Unit is available, but there is no load on it or we are in setback/deadband
				if ( ! UnitHeat( UnitHeatNum ).FanOffNoHeating && GetCurrentScheduleValue( UnitHeat( UnitHeatNum ).FanSchedPtr ) > 0.0 ) {
					UnitHeat( UnitHeatNum ).OpMode = ContFanCycCoil;
				}
			}
		}

		SetMassFlowRateToZero = false;
		if ( GetCurrentScheduleValue( UnitHeat( UnitHeatNum ).SchedPtr ) > 0 ) {
			if ( ( GetCurrentScheduleValue( UnitHeat( UnitHeatNum ).FanAvailSchedPtr ) > 0 || ZoneCompTurnFansOn ) && ! ZoneCompTurnFansOff ) {
				if ( UnitHeat( UnitHeatNum ).FanOffNoHeating && ( ( ZoneSysEnergyDemand( ZoneNum ).RemainingOutputRequired < SmallLoad ) || ( CurDeadBandOrSetback( ZoneNum ) ) ) ) {
					SetMassFlowRateToZero = true;
				}
			} else {
				SetMassFlowRateToZero = true;
			}
		} else {
			SetMassFlowRateToZero = true;
		}

		if ( SetMassFlowRateToZero ) {
			Node( InNode ).MassFlowRate = 0.0;
			Node( InNode ).MassFlowRateMaxAvail = 0.0;
			Node( InNode ).MassFlowRateMinAvail = 0.0;
			Node( OutNode ).MassFlowRate = 0.0;
			Node( OutNode ).MassFlowRateMaxAvail = 0.0;
			Node( OutNode ).MassFlowRateMinAvail = 0.0;
		} else {
			Node( InNode ).MassFlowRate = UnitHeat( UnitHeatNum ).MaxAirMassFlow;
			Node( InNode ).MassFlowRateMaxAvail = UnitHeat( UnitHeatNum ).MaxAirMassFlow;
			Node( InNode ).MassFlowRateMinAvail = UnitHeat( UnitHeatNum ).MaxAirMassFlow;
			Node( OutNode ).MassFlowRate = UnitHeat( UnitHeatNum ).MaxAirMassFlow;
			Node( OutNode ).MassFlowRateMaxAvail = UnitHeat( UnitHeatNum ).MaxAirMassFlow;
			Node( OutNode ).MassFlowRateMinAvail = UnitHeat( UnitHeatNum ).MaxAirMassFlow;
		}

		// Just in case the unit is off and conditions do not get sent through
		// the unit for some reason, set the outlet conditions equal to the inlet
		// conditions of the unit heater
		Node( OutNode ).Temp = Node( InNode ).Temp;
		Node( OutNode ).Press = Node( InNode ).Press;
		Node( OutNode ).HumRat = Node( InNode ).HumRat;
		Node( OutNode ).Enthalpy = Node( InNode ).Enthalpy;

	}

	void
	SizeUnitHeater( int const UnitHeatNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   February 2002
		//       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
		//                      July 2014, B. Nigusse, added scalable sizing
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing Unit Heater components for which flow rates have not been
		// specified in the input.

		// METHODOLOGY EMPLOYED:
		// Obtains flow rates from the zone sizing arrays and plant sizing data.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSizing;
		using namespace InputProcessor;
		using WaterCoils::SetCoilDesFlow;
		using WaterCoils::GetCoilWaterInletNode;
		using WaterCoils::GetCoilWaterOutletNode;
		using SteamCoils::GetCoilSteamInletNode;
		using SteamCoils::GetCoilSteamOutletNode;
		using DataPlant::PlantLoop;
		using DataPlant::MyPlantSizingIndex;
		using Psychrometrics::CPHW;
		using ReportSizingManager::ReportSizingOutput;
		using ReportSizingManager::RequestSizing;
		using General::RoundSigDigits;
		using DataHVACGlobals::HeatingAirflowSizing;
		using DataHVACGlobals::HeatingCapacitySizing;
		using DataHeatBalance::Zone;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "SizeUnitHeater" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int PltSizHeatNum; // index of plant sizing object for 1st heating loop
		bool ErrorsFound;
		Real64 DesCoilLoad;
		Real64 TempSteamIn;
		Real64 EnthSteamInDry;
		Real64 EnthSteamOutWet;
		Real64 LatentHeatSteam;
		Real64 SteamDensity;
		static int RefrigIndex( 0 );
		static int CoilWaterInletNode( 0 );
		static int CoilWaterOutletNode( 0 );
		static int CoilSteamInletNode( 0 );
		static int CoilSteamOutletNode( 0 );
		Real64 Cp; // local temporary for fluid specific heat
		Real64 rho; // local temporary for fluid density
		bool IsAutoSize; // Indicator to autosize
		Real64 MaxAirVolFlowDes; // Autosized maximum air flow for reporting
		Real64 MaxAirVolFlowUser; // Hardsized maximum air flow for reporting
		Real64 MaxVolHotWaterFlowDes; // Autosized maximum hot water flow for reporting
		Real64 MaxVolHotWaterFlowUser; // Hardsized maximum hot water flow for reporting
		Real64 MaxVolHotSteamFlowDes; // Autosized maximum hot steam flow for reporting
		Real64 MaxVolHotSteamFlowUser; // Hardsized maximum hot steam flow for reporting
		std::string CompName; // component name
		std::string CompType; // component type
		std::string SizingString; // input field sizing description (e.g., Nominal Capacity)
		Real64 TempSize; // autosized value of coil input field
		int FieldNum = 1; // IDD numeric field number where input field description is found
		int SizingMethod; // Integer representation of sizing method name (e.g., CoolingAirflowSizing, HeatingAirflowSizing, CoolingCapacitySizing, HeatingCapacitySizing, etc.)
		bool PrintFlag; // TRUE when sizing information is reported in the eio file
		int zoneHVACIndex; // index of zoneHVAC equipment sizing specification
		int SAFMethod( 0 ); // supply air flow rate sizing method (SupplyAirFlowRate, FlowPerFloorArea, FractionOfAutosizedCoolingAirflow, FractionOfAutosizedHeatingAirflow ...)
		int CapSizingMethod(0); // capacity sizing methods (HeatingDesignCapacity, CapacityPerFloorArea, FractionOfAutosizedCoolingCapacity, and FractionOfAutosizedHeatingCapacity )

		PltSizHeatNum = 0;
		ErrorsFound = false;
		IsAutoSize = false;
		MaxAirVolFlowDes = 0.0;
		MaxAirVolFlowUser = 0.0;
		MaxVolHotWaterFlowDes = 0.0;
		MaxVolHotWaterFlowUser = 0.0;
		MaxVolHotSteamFlowDes = 0.0;
		MaxVolHotSteamFlowUser = 0.0;

		DataScalableSizingON = false;
		DataScalableCapSizingON = false;
		ZoneHeatingOnlyFan = true;
		CompType = "ZoneHVAC:UnitHeater";
		CompName = UnitHeat( UnitHeatNum ).Name;
		DataZoneNumber = UnitHeat( UnitHeatNum ).ZonePtr;

		if ( CurZoneEqNum > 0 ) {
			if ( UnitHeat( UnitHeatNum ).HVACSizingIndex > 0 ) {
				zoneHVACIndex = UnitHeat( UnitHeatNum ).HVACSizingIndex;
				SizingMethod = HeatingAirflowSizing;
				FieldNum = 1; //  N1 , \field Maximum Supply Air Flow Rate
				PrintFlag = true;
				SizingString = UnitHeatNumericFields( UnitHeatNum ).FieldNames( FieldNum ) + " [m3/s]";
				SAFMethod = ZoneHVACSizing( zoneHVACIndex ).HeatingSAFMethod;
				ZoneEqSizing( CurZoneEqNum ).SizingMethod( SizingMethod ) = SAFMethod;
				if ( SAFMethod == None || SAFMethod == SupplyAirFlowRate || SAFMethod == FlowPerFloorArea || SAFMethod == FractionOfAutosizedHeatingAirflow ) {
					if ( SAFMethod == SupplyAirFlowRate ) {
						if ( ZoneHVACSizing( zoneHVACIndex ).MaxHeatAirVolFlow > 0.0 ) {
							ZoneEqSizing( CurZoneEqNum ).AirVolFlow = ZoneHVACSizing( zoneHVACIndex ).MaxHeatAirVolFlow;
							ZoneEqSizing( CurZoneEqNum ).SystemAirFlow = true;
						}
						TempSize = ZoneHVACSizing( zoneHVACIndex ).MaxHeatAirVolFlow;
					} else if ( SAFMethod == FlowPerFloorArea ) {
						ZoneEqSizing( CurZoneEqNum ).SystemAirFlow = true;
						ZoneEqSizing( CurZoneEqNum ).AirVolFlow = ZoneHVACSizing( zoneHVACIndex ).MaxHeatAirVolFlow * Zone( DataZoneNumber ).FloorArea;
						TempSize = ZoneEqSizing( CurZoneEqNum ).AirVolFlow;
						DataScalableSizingON = true;
					} else if ( SAFMethod == FractionOfAutosizedHeatingAirflow ) {
						DataFracOfAutosizedCoolingAirflow = ZoneHVACSizing( zoneHVACIndex ).MaxHeatAirVolFlow;
						TempSize = AutoSize;
						DataScalableSizingON = true;
					} else {
						TempSize = ZoneHVACSizing( zoneHVACIndex ).MaxHeatAirVolFlow;
					}
					RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
					UnitHeat( UnitHeatNum ).MaxAirVolFlow = TempSize;

				} else if ( SAFMethod == FlowPerHeatingCapacity ) {
					SizingMethod = HeatingCapacitySizing;
					TempSize = AutoSize;
					PrintFlag = false;
					DataScalableSizingON = true;
					DataFlowUsedForSizing = FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow;
					RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
					if ( ZoneHVACSizing( zoneHVACIndex ).HeatingCapMethod == FractionOfAutosizedHeatingCapacity ) {
						DataFracOfAutosizedHeatingCapacity = ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity;
					}
					DataAutosizedHeatingCapacity = TempSize;
					DataFlowPerHeatingCapacity = ZoneHVACSizing( zoneHVACIndex ).MaxHeatAirVolFlow;
					SizingMethod = HeatingAirflowSizing;
					PrintFlag = true;
					TempSize = AutoSize;
					RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
					UnitHeat( UnitHeatNum ).MaxAirVolFlow = TempSize;
				}
				DataScalableSizingON = false;
			} else {
				// no scalble sizing method has been specified. Sizing proceeds using the method
				// specified in the zoneHVAC object
				SizingMethod = HeatingAirflowSizing;
				FieldNum = 1; // N1 , \field Maximum Supply Air Flow Rate
				PrintFlag = true;
				SizingString = UnitHeatNumericFields( UnitHeatNum ).FieldNames( FieldNum ) + " [m3/s]";
				TempSize = UnitHeat( UnitHeatNum ).MaxAirVolFlow;
				RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
				UnitHeat( UnitHeatNum ).MaxAirVolFlow = TempSize;
			}
		}

		IsAutoSize = false;
		if ( UnitHeat( UnitHeatNum ).MaxVolHotWaterFlow == AutoSize ) {
			IsAutoSize = true;
		}

		if ( UnitHeat( UnitHeatNum ).HCoilType == WaterCoil ) {

			if ( CurZoneEqNum > 0 ) {
				if ( ! IsAutoSize && ! ZoneSizingRunDone ) { // Simulation continue
					if ( UnitHeat( UnitHeatNum ).MaxVolHotWaterFlow > 0.0 ) {
						ReportSizingOutput( "ZoneHVAC:UnitHeater", UnitHeat( UnitHeatNum ).Name, "User-Specified Maximum Hot Water Flow [m3/s]", UnitHeat( UnitHeatNum ).MaxVolHotWaterFlow );
					}
				} else {
					CheckZoneSizing( "ZoneHVAC:UnitHeater", UnitHeat( UnitHeatNum ).Name );

					CoilWaterInletNode = GetCoilWaterInletNode( "Coil:Heating:Water", UnitHeat( UnitHeatNum ).HCoilName, ErrorsFound );
					CoilWaterOutletNode = GetCoilWaterOutletNode( "Coil:Heating:Water", UnitHeat( UnitHeatNum ).HCoilName, ErrorsFound );
					if ( IsAutoSize ) {
						PltSizHeatNum = MyPlantSizingIndex( "Coil:Heating:Water", UnitHeat( UnitHeatNum ).HCoilName, CoilWaterInletNode, CoilWaterOutletNode, ErrorsFound );
						if ( PltSizHeatNum > 0 ) {
							SizingMethod = HeatingCapacitySizing;
							if ( UnitHeat( UnitHeatNum ).HVACSizingIndex > 0 ) {
								zoneHVACIndex = UnitHeat( UnitHeatNum ).HVACSizingIndex;
								CapSizingMethod = ZoneHVACSizing( zoneHVACIndex ).HeatingCapMethod;
								ZoneEqSizing( CurZoneEqNum ).SizingMethod( SizingMethod ) = CapSizingMethod;
								if ( CapSizingMethod == HeatingDesignCapacity || CapSizingMethod == CapacityPerFloorArea || CapSizingMethod == FractionOfAutosizedHeatingCapacity ) {
									if ( CapSizingMethod == HeatingDesignCapacity ) {
										if ( ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity == AutoSize ) {
											ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = FinalZoneSizing( CurZoneEqNum ).DesHeatLoad;
										} else {
											ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity;
										}
										ZoneEqSizing( CurZoneEqNum ).HeatingCapacity = true;
										TempSize = AutoSize;
									} else if ( CapSizingMethod == CapacityPerFloorArea ) {
										ZoneEqSizing( CurZoneEqNum ).HeatingCapacity = true;
										ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity * Zone( DataZoneNumber ).FloorArea;
										DataScalableCapSizingON = true;
									} else if ( CapSizingMethod == FractionOfAutosizedHeatingCapacity ) {
										DataFracOfAutosizedHeatingCapacity = ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity;
										DataScalableCapSizingON = true;
										TempSize = AutoSize;
									}
								}
								PrintFlag = false;
								RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
								DesCoilLoad = TempSize;
							} else {
								SizingString = "";
								PrintFlag = false;
								TempSize = AutoSize;
								ZoneEqSizing( CurZoneEqNum ).HeatingCapacity = true;
								ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = FinalZoneSizing( CurZoneEqNum ).DesHeatLoad;
								RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
								DesCoilLoad = TempSize;
							}

							if ( DesCoilLoad >= SmallLoad ) {
								rho = GetDensityGlycol( PlantLoop( UnitHeat( UnitHeatNum ).HWLoopNum ).FluidName, 60.0, PlantLoop( UnitHeat( UnitHeatNum ).HWLoopNum ).FluidIndex, RoutineName );
								Cp = GetSpecificHeatGlycol( PlantLoop( UnitHeat( UnitHeatNum ).HWLoopNum ).FluidName, 60.0, PlantLoop( UnitHeat( UnitHeatNum ).HWLoopNum ).FluidIndex, RoutineName );
								MaxVolHotWaterFlowDes = DesCoilLoad / ( PlantSizData( PltSizHeatNum ).DeltaT * Cp * rho );
							} else {
								MaxVolHotWaterFlowDes = 0.0;
							}

						} else {
							ShowSevereError( "Autosizing of water flow requires a heating loop Sizing:Plant object" );
							ShowContinueError( "Occurs in ZoneHVAC:UnitHeater Object=" + UnitHeat( UnitHeatNum ).Name );
							ErrorsFound = true;
						}
					}
					if ( IsAutoSize ) {
						UnitHeat( UnitHeatNum ).MaxVolHotWaterFlow = MaxVolHotWaterFlowDes;
						ReportSizingOutput( "ZoneHVAC:UnitHeater", UnitHeat( UnitHeatNum ).Name, "Design Size Maximum Hot Water Flow [m3/s]", MaxVolHotWaterFlowDes );
					} else {
						if ( UnitHeat( UnitHeatNum ).MaxVolHotWaterFlow > 0.0 && MaxVolHotWaterFlowDes > 0.0 ) {
							MaxVolHotWaterFlowUser = UnitHeat( UnitHeatNum ).MaxVolHotWaterFlow;
							ReportSizingOutput( "ZoneHVAC:UnitHeater", UnitHeat( UnitHeatNum ).Name, "Design Size Maximum Hot Water Flow [m3/s]", MaxVolHotWaterFlowDes, "User-Specified Maximum Hot Water Flow [m3/s]", MaxVolHotWaterFlowUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( MaxVolHotWaterFlowDes - MaxVolHotWaterFlowUser ) / MaxVolHotWaterFlowUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeUnitHeater: Potential issue with equipment sizing for ZoneHVAC:UnitHeater " + UnitHeat( UnitHeatNum ).Name );
									ShowContinueError( "User-Specified Maximum Hot Water Flow of " + RoundSigDigits( MaxVolHotWaterFlowUser, 5 ) + " [m3/s]" );
									ShowContinueError( "differs from Design Size Maximum Hot Water Flow of " + RoundSigDigits( MaxVolHotWaterFlowDes, 5 ) + " [m3/s]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
					}

				}
			}
		} else {
			UnitHeat( UnitHeatNum ).MaxVolHotWaterFlow = 0.0;
		}

		IsAutoSize = false;
		if ( UnitHeat( UnitHeatNum ).MaxVolHotSteamFlow == AutoSize ) {
			IsAutoSize = true;
		}

		if ( UnitHeat( UnitHeatNum ).HCoilType == SteamCoil ) {

			if ( CurZoneEqNum > 0 ) {
				if ( ! IsAutoSize && ! ZoneSizingRunDone ) { // Simulation continue
					if ( UnitHeat( UnitHeatNum ).MaxVolHotSteamFlow > 0.0 ) {
						ReportSizingOutput( "ZoneHVAC:UnitHeater", UnitHeat( UnitHeatNum ).Name, "User-Specified Maximum Steam Flow [m3/s]", UnitHeat( UnitHeatNum ).MaxVolHotSteamFlow );
					}
				} else {
					CheckZoneSizing( "ZoneHVAC:UnitHeater", UnitHeat( UnitHeatNum ).Name );

					CoilSteamInletNode = GetCoilSteamInletNode( "Coil:Heating:Steam", UnitHeat( UnitHeatNum ).HCoilName, ErrorsFound );
					CoilSteamOutletNode = GetCoilSteamInletNode( "Coil:Heating:Steam", UnitHeat( UnitHeatNum ).HCoilName, ErrorsFound );
					if ( IsAutoSize ) {
						PltSizHeatNum = MyPlantSizingIndex( "Coil:Heating:Steam", UnitHeat( UnitHeatNum ).HCoilName, CoilSteamInletNode, CoilSteamOutletNode, ErrorsFound );
						if ( PltSizHeatNum > 0 ) {
							if ( UnitHeat( UnitHeatNum ).HVACSizingIndex > 0 ) {
								zoneHVACIndex = UnitHeat( UnitHeatNum ).HVACSizingIndex;
								SizingMethod = HeatingCapacitySizing;
								CapSizingMethod = ZoneHVACSizing( zoneHVACIndex ).HeatingCapMethod;
								ZoneEqSizing( CurZoneEqNum ).SizingMethod( SizingMethod ) = CapSizingMethod;
								if ( CapSizingMethod == HeatingDesignCapacity || CapSizingMethod == CapacityPerFloorArea || CapSizingMethod == FractionOfAutosizedHeatingCapacity ) {
									if ( CapSizingMethod == HeatingDesignCapacity ) {
										if ( ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity == AutoSize ) {
											ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = FinalZoneSizing( CurZoneEqNum ).DesHeatLoad;
										} else {
											ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity;
										}
										ZoneEqSizing( CurZoneEqNum ).HeatingCapacity = true;
										TempSize = AutoSize;
									} else if ( CapSizingMethod == CapacityPerFloorArea ) {
										ZoneEqSizing( CurZoneEqNum ).HeatingCapacity = true;
										ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity * Zone( DataZoneNumber ).FloorArea;
										DataScalableCapSizingON = true;
									} else if ( CapSizingMethod == FractionOfAutosizedHeatingCapacity ) {
										DataFracOfAutosizedHeatingCapacity = ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity;
										TempSize = AutoSize;
										DataScalableCapSizingON = true;
									}
								}
								SizingMethod = HeatingCapacitySizing;
								PrintFlag = false;
								RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
								DesCoilLoad = TempSize;
							} else {
								DesCoilLoad = FinalZoneSizing( CurZoneEqNum ).DesHeatLoad;
							}
							if ( DesCoilLoad >= SmallLoad ) {
								TempSteamIn = 100.00;
								EnthSteamInDry = GetSatEnthalpyRefrig( fluidNameSteam, TempSteamIn, 1.0, RefrigIndex, RoutineName );
								EnthSteamOutWet = GetSatEnthalpyRefrig( fluidNameSteam, TempSteamIn, 0.0, RefrigIndex, RoutineName );
								LatentHeatSteam = EnthSteamInDry - EnthSteamOutWet;
								SteamDensity = GetSatDensityRefrig( fluidNameSteam, TempSteamIn, 1.0, RefrigIndex, RoutineName );
								MaxVolHotSteamFlowDes = DesCoilLoad / ( SteamDensity * ( LatentHeatSteam + PlantSizData( PltSizHeatNum ).DeltaT * CPHW( PlantSizData( PltSizHeatNum ).ExitTemp ) ) );
							} else {
								MaxVolHotSteamFlowDes = 0.0;
							}
						} else {
							ShowSevereError( "Autosizing of Steam flow requires a heating loop Sizing:Plant object" );
							ShowContinueError( "Occurs in ZoneHVAC:UnitHeater Object=" + UnitHeat( UnitHeatNum ).Name );
							ErrorsFound = true;
						}
					}
					if ( IsAutoSize ) {
						UnitHeat( UnitHeatNum ).MaxVolHotSteamFlow = MaxVolHotSteamFlowDes;
						ReportSizingOutput( "ZoneHVAC:UnitHeater", UnitHeat( UnitHeatNum ).Name, "Design Size Maximum Steam Flow [m3/s]", MaxVolHotSteamFlowDes );
					} else {
						if ( UnitHeat( UnitHeatNum ).MaxVolHotSteamFlow > 0.0 && MaxVolHotSteamFlowDes > 0.0 ) {
							MaxVolHotSteamFlowUser = UnitHeat( UnitHeatNum ).MaxVolHotSteamFlow;
							ReportSizingOutput( "ZoneHVAC:UnitHeater", UnitHeat( UnitHeatNum ).Name, "Design Size Maximum Steam Flow [m3/s]", MaxVolHotSteamFlowDes, "User-Specified Maximum Steam Flow [m3/s]", MaxVolHotSteamFlowUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( MaxVolHotSteamFlowDes - MaxVolHotSteamFlowUser ) / MaxVolHotSteamFlowUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeUnitHeater: Potential issue with equipment sizing for ZoneHVAC:UnitHeater " + UnitHeat( UnitHeatNum ).Name );
									ShowContinueError( "User-Specified Maximum Steam Flow of " + RoundSigDigits( MaxVolHotSteamFlowUser, 5 ) + " [m3/s]" );
									ShowContinueError( "differs from Design Size Maximum Steam Flow of " + RoundSigDigits( MaxVolHotSteamFlowDes, 5 ) + " [m3/s]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
					}
				}
			}
		} else {
			UnitHeat( UnitHeatNum ).MaxVolHotSteamFlow = 0.0;
		}

		// set the design air flow rate for the heating coil

		SetCoilDesFlow( UnitHeat( UnitHeatNum ).HCoilTypeCh, UnitHeat( UnitHeatNum ).HCoilName, UnitHeat( UnitHeatNum ).MaxAirVolFlow, ErrorsFound );
		if ( CurZoneEqNum > 0 ) {
			ZoneEqSizing( CurZoneEqNum ).MaxHWVolFlow = UnitHeat( UnitHeatNum ).MaxVolHotWaterFlow;
		}

		if ( ErrorsFound ) {
			ShowFatalError( "Preceding sizing errors cause program termination" );
		}

	}

	void
	CalcUnitHeater(
		int & UnitHeatNum, // number of the current fan coil unit being simulated
		int const ZoneNum, // number of zone being served
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & PowerMet, // Sensible power supplied (W)
		Real64 & LatOutputProvided // Latent power supplied (kg/s), negative = dehumidification
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   May 2000
		//       MODIFIED       Don Shirey, Aug 2009 (LatOutputProvided)
		//                      July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine mainly controls the action of the unit heater
		// based on the user input for controls and the defined controls
		// algorithms.  There are currently (at the initial creation of this
		// subroutine) two control methods: on-off fan operation or continuous
		// fan operation.

		// METHODOLOGY EMPLOYED:
		// Unit is controlled based on user input and what is happening in the
		// simulation.  There are various cases to consider:
		// 1. OFF: Unit is schedule off.  All flow rates are set to zero and
		//    the temperatures are set to zone conditions.
		// 2. NO LOAD OR COOLING/ON-OFF FAN CONTROL: Unit is available, but
		//    there is no heating load.  All flow rates are set to zero and
		//    the temperatures are set to zone conditions.
		// 3. NO LOAD OR COOLING/CONTINUOUS FAN CONTROL: Unit is available and
		//    the fan is running (if it is scheduled to be available also).
		//    No heating is provided, only circulation via the fan running.
		// 4. HEATING: The unit is on/available and there is a heating load.
		//    The heating coil is modulated (constant fan speed) to meet the
		//    heating load.

		// REFERENCES:
		// ASHRAE Systems and Equipment Handbook (SI), 1996. page 31.7

		// Using/Aliasing
		using namespace DataZoneEnergyDemands;
		using DataHVACGlobals::ZoneCompTurnFansOn;
		using DataHVACGlobals::ZoneCompTurnFansOff;
		using DataHVACGlobals::FanType_SimpleOnOff;
		using DataZoneEquipment::UnitHeater_Num;
		using General::SolveRegulaFalsi;
		using PlantUtilities::SetComponentFlowRate;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const MaxIter( 100 ); // maximum number of iterations

		// INTERFACE BLOCK SPECIFICATIONS

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ControlNode; // the hot water inlet node
		int InletNode; // unit air inlet node
		int OutletNode; // unit air outlet node
		Real64 ControlOffset; // tolerance for output control
		Real64 MaxWaterFlow; // maximum water flow for heating or cooling [kg/sec]
		Real64 MinWaterFlow; // minimum water flow for heating or cooling [kg/sec]
		Real64 QUnitOut; // heating or sens. cooling provided by fan coil unit [watts]
		Real64 LatentOutput; // Latent (moisture) add/removal rate, negative is dehumidification [kg/s]
		Real64 SpecHumOut; // Specific humidity ratio of outlet air (kg moisture / kg moist air)
		Real64 SpecHumIn; // Specific humidity ratio of inlet air (kg moisture / kg moist air)
		Real64 mdot; // local temporary for fluid mass flow rate
		Array1D< Real64 > Par( 3 ); // parameters passed to RegulaFalsi function
		int OpMode;
		Real64 PartLoadFrac;
		Real64 NoOutput;
		Real64 FullOutput;
		int SolFlag; // return flag from RegulaFalsi for sensible load
		bool UnitOn;

		// initialize local variables
		FanElecPower = 0.0;
		QUnitOut = 0.0;
		NoOutput = 0.0;
		FullOutput = 0.0;
		LatentOutput = 0.0;
		MaxWaterFlow = 0.0;
		MinWaterFlow = 0.0;
		PartLoadFrac = 0.0;
		SolFlag = 0; // # of iterations IF positive, -1 means failed to converge, -2 means bounds are incorrect
		InletNode = UnitHeat( UnitHeatNum ).AirInNode;
		OutletNode = UnitHeat( UnitHeatNum ).AirOutNode;
		ControlNode = UnitHeat( UnitHeatNum ).HotControlNode;
		ControlOffset = UnitHeat( UnitHeatNum ).HotControlOffset;
		UnitOn = false;
		OpMode = UnitHeat( UnitHeatNum ).OpMode;

		if ( OpMode != CycFanCycCoil ) {

			if ( GetCurrentScheduleValue( UnitHeat( UnitHeatNum ).SchedPtr ) <= 0 || ( ( GetCurrentScheduleValue( UnitHeat( UnitHeatNum ).FanAvailSchedPtr ) <= 0 && ! ZoneCompTurnFansOn ) || ZoneCompTurnFansOff ) ) {
				// Case 1: OFF-->unit schedule says that it it not available
				//         OR child fan in not available OR child fan not being cycled ON by sys avail manager
				//         OR child fan being forced OFF by sys avail manager
				HCoilOn = false;
				if ( UnitHeat( UnitHeatNum ).HCoilType == WaterCoil ) {
					mdot = 0.0; // try to turn off

					SetComponentFlowRate( mdot, UnitHeat( UnitHeatNum ).HotControlNode, UnitHeat( UnitHeatNum ).HotCoilOutNodeNum, UnitHeat( UnitHeatNum ).HWLoopNum, UnitHeat( UnitHeatNum ).HWLoopSide, UnitHeat( UnitHeatNum ).HWBranchNum, UnitHeat( UnitHeatNum ).HWCompNum );
				}
				if ( UnitHeat( UnitHeatNum ).HCoilType == SteamCoil ) {
					mdot = 0.0; // try to turn off

					SetComponentFlowRate( mdot, UnitHeat( UnitHeatNum ).HotControlNode, UnitHeat( UnitHeatNum ).HotCoilOutNodeNum, UnitHeat( UnitHeatNum ).HWLoopNum, UnitHeat( UnitHeatNum ).HWLoopSide, UnitHeat( UnitHeatNum ).HWBranchNum, UnitHeat( UnitHeatNum ).HWCompNum );
				}
				CalcUnitHeaterComponents( UnitHeatNum, FirstHVACIteration, QUnitOut );

			} else if ( ( QZnReq < SmallLoad ) || CurDeadBandOrSetback( ZoneNum ) ) {
				// Unit is available, but there is no load on it or we are in setback/deadband
				if ( ! UnitHeat( UnitHeatNum ).FanOffNoHeating ) {

					// Case 2: NO LOAD OR COOLING/ON-OFF FAN CONTROL-->turn everything off
					//         because there is no load on the unit heater
					HCoilOn = false;
					if ( UnitHeat( UnitHeatNum ).HCoilType == WaterCoil ) {
						mdot = 0.0; // try to turn off

						SetComponentFlowRate( mdot, UnitHeat( UnitHeatNum ).HotControlNode, UnitHeat( UnitHeatNum ).HotCoilOutNodeNum, UnitHeat( UnitHeatNum ).HWLoopNum, UnitHeat( UnitHeatNum ).HWLoopSide, UnitHeat( UnitHeatNum ).HWBranchNum, UnitHeat( UnitHeatNum ).HWCompNum );
					}
					if ( UnitHeat( UnitHeatNum ).HCoilType == SteamCoil ) {
						mdot = 0.0; // try to turn off

						SetComponentFlowRate( mdot, UnitHeat( UnitHeatNum ).HotControlNode, UnitHeat( UnitHeatNum ).HotCoilOutNodeNum, UnitHeat( UnitHeatNum ).HWLoopNum, UnitHeat( UnitHeatNum ).HWLoopSide, UnitHeat( UnitHeatNum ).HWBranchNum, UnitHeat( UnitHeatNum ).HWCompNum );
					}
					CalcUnitHeaterComponents( UnitHeatNum, FirstHVACIteration, QUnitOut );

				} else {
					// Case 3: NO LOAD OR COOLING/CONTINUOUS FAN CONTROL-->let the fan
					//         continue to run even though there is no load (air circulation)
					// Note that the flow rates were already set in the initialization routine
					// so there is really nothing else left to do except call the components.

					HCoilOn = false;
					if ( UnitHeat( UnitHeatNum ).HCoilType == WaterCoil ) {
						mdot = 0.0; // try to turn off

						if ( UnitHeat( UnitHeatNum ).HWLoopNum > 0 ) {
							SetComponentFlowRate( mdot, UnitHeat( UnitHeatNum ).HotControlNode, UnitHeat( UnitHeatNum ).HotCoilOutNodeNum, UnitHeat( UnitHeatNum ).HWLoopNum, UnitHeat( UnitHeatNum ).HWLoopSide, UnitHeat( UnitHeatNum ).HWBranchNum, UnitHeat( UnitHeatNum ).HWCompNum );
						}
					}
					if ( UnitHeat( UnitHeatNum ).HCoilType == SteamCoil ) {
						mdot = 0.0; // try to turn off
						if ( UnitHeat( UnitHeatNum ).HWLoopNum > 0 ) {
							SetComponentFlowRate( mdot, UnitHeat( UnitHeatNum ).HotControlNode, UnitHeat( UnitHeatNum ).HotCoilOutNodeNum, UnitHeat( UnitHeatNum ).HWLoopNum, UnitHeat( UnitHeatNum ).HWLoopSide, UnitHeat( UnitHeatNum ).HWBranchNum, UnitHeat( UnitHeatNum ).HWCompNum );
						}
					}

					CalcUnitHeaterComponents( UnitHeatNum, FirstHVACIteration, QUnitOut );

				}

			} else { // Case 4: HEATING-->unit is available and there is a heating load

				{ auto const SELECT_CASE_var( UnitHeat( UnitHeatNum ).HCoilType );

				if ( SELECT_CASE_var == WaterCoil ) {

					//On the first HVAC iteration the system values are given to the controller, but after that
					// the demand limits are in place and there needs to be feedback to the Zone Equipment
					if ( FirstHVACIteration ) {
						MaxWaterFlow = UnitHeat( UnitHeatNum ).MaxHotWaterFlow;
						MinWaterFlow = UnitHeat( UnitHeatNum ).MinHotWaterFlow;
					} else {
						MaxWaterFlow = Node( ControlNode ).MassFlowRateMaxAvail;
						MinWaterFlow = Node( ControlNode ).MassFlowRateMinAvail;
					}
					// control water flow to obtain output matching QZnReq
					ControlCompOutput( UnitHeat( UnitHeatNum ).Name, cMO_UnitHeater, UnitHeatNum, FirstHVACIteration, QZnReq, ControlNode, MaxWaterFlow, MinWaterFlow, ControlOffset, UnitHeat( UnitHeatNum ).ControlCompTypeNum, UnitHeat( UnitHeatNum ).CompErrIndex, _, _, _, _, _, UnitHeat( UnitHeatNum ).HWLoopNum, UnitHeat( UnitHeatNum ).HWLoopSide, UnitHeat( UnitHeatNum ).HWBranchNum );

				} else if ( ( SELECT_CASE_var == ElectricCoil ) || ( SELECT_CASE_var == GasCoil ) || ( SELECT_CASE_var == SteamCoil ) ) {
					HCoilOn = true;
					CalcUnitHeaterComponents( UnitHeatNum, FirstHVACIteration, QUnitOut );

				}}
			}
			QUnitOut = Node( OutletNode ).MassFlowRate * ( PsyHFnTdbW( Node( OutletNode ).Temp, Node( InletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( InletNode ).HumRat ) );
			if ( Node( InletNode ).MassFlowRateMax > 0.0 ) {
				UnitHeat( UnitHeatNum ).FanPartLoadRatio = Node( InletNode ).MassFlowRate / Node( InletNode ).MassFlowRateMax;
			}
		} else { // OnOff fan and cycling
			if ( ( QZnReq < SmallLoad ) || ( CurDeadBandOrSetback( ZoneNum ) ) || GetCurrentScheduleValue( UnitHeat( UnitHeatNum ).SchedPtr ) <= 0 || ( ( GetCurrentScheduleValue( UnitHeat( UnitHeatNum ).FanAvailSchedPtr ) <= 0 && ! ZoneCompTurnFansOn ) || ZoneCompTurnFansOff ) ) {
				// Case 1: OFF-->unit schedule says that it it not available
				//         OR child fan in not available OR child fan not being cycled ON by sys avail manager
				//         OR child fan being forced OFF by sys avail manager
				PartLoadFrac = 0.0;
				HCoilOn = false;
				CalcUnitHeaterComponents( UnitHeatNum, FirstHVACIteration, QUnitOut, OpMode, PartLoadFrac );

				if ( Node( InletNode ).MassFlowRateMax > 0.0 ) {
					UnitHeat( UnitHeatNum ).FanPartLoadRatio = Node( InletNode ).MassFlowRate / Node( InletNode ).MassFlowRateMax;
				}

			} else { // Case 4: HEATING-->unit is available and there is a heating load

				HCoilOn = true;
				UnitOn = true;

				// Find part load ratio of unit heater coils
				PartLoadFrac = 0.0;
				CalcUnitHeaterComponents( UnitHeatNum, FirstHVACIteration, NoOutput, OpMode, PartLoadFrac );
				if ( ( NoOutput - QZnReq ) < SmallLoad ) {
					// Unit heater is unable to meet the load with coil off, set PLR = 1
					PartLoadFrac = 1.0;
					CalcUnitHeaterComponents( UnitHeatNum, FirstHVACIteration, FullOutput, OpMode, PartLoadFrac );
					if ( ( FullOutput - QZnReq ) > SmallLoad ) {
						// Unit heater full load capacity is able to meet the load, Find PLR
						Par( 1 ) = double( UnitHeatNum );
						Par( 2 ) = 0.0; // FLAG, IF 1.0 then FirstHVACIteration equals TRUE, if 0.0 then FirstHVACIteration equals false
						if ( FirstHVACIteration ) Par( 2 ) = 1.0;
						Par( 3 ) = double( UnitHeat( UnitHeatNum ).OpMode );
						// Tolerance is in fraction of load, MaxIter = 30, SolFalg = # of iterations or error as appropriate
						SolveRegulaFalsi( 0.001, MaxIter, SolFlag, PartLoadFrac, CalcUnitHeaterResidual, 0.0, 1.0, Par );
					}
				}

				CalcUnitHeaterComponents( UnitHeatNum, FirstHVACIteration, QUnitOut, OpMode, PartLoadFrac );

			} // ...end of unit ON/OFF IF-THEN block
			UnitHeat( UnitHeatNum ).PartLoadFrac = PartLoadFrac;
			UnitHeat( UnitHeatNum ).FanPartLoadRatio = PartLoadFrac;
			Node( OutletNode ).MassFlowRate = Node( InletNode ).MassFlowRate;

		}

		// CR9155 Remove specific humidity calculations
		SpecHumOut = Node( OutletNode ).HumRat;
		SpecHumIn = Node( InletNode ).HumRat;
		LatentOutput = Node( OutletNode ).MassFlowRate * ( SpecHumOut - SpecHumIn ); // Latent rate (kg/s), dehumid = negative

		QUnitOut = Node( OutletNode ).MassFlowRate * ( PsyHFnTdbW( Node( OutletNode ).Temp, Node( InletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( InletNode ).HumRat ) );

		// Report variables...
		UnitHeat( UnitHeatNum ).HeatPower = max( 0.0, QUnitOut );
		UnitHeat( UnitHeatNum ).ElecPower = FanElecPower;

		PowerMet = QUnitOut;
		LatOutputProvided = LatentOutput;

	}

	void
	CalcUnitHeaterComponents(
		int const UnitHeatNum, // Unit index in unit heater array
		bool const FirstHVACIteration, // flag for 1st HVAV iteration in the time step
		Real64 & LoadMet, // load met by unit (watts)
		Optional_int_const OpMode, // fan operating mode
		Optional< Real64 const > PartLoadRatio // part-load ratio
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   May 2000
		//       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine launches the individual component simulations.
		// This is called either when the unit is off to carry null conditions
		// through the unit or during control iterations to continue updating
		// what is going on within the unit.

		// METHODOLOGY EMPLOYED:
		// Simply calls the different components in order.

		// REFERENCES:
		// na

		// Using/Aliasing
		using Fans::SimulateFanComponents;
		using HeatingCoils::SimulateHeatingCoilComponents;
		using WaterCoils::SimulateWaterCoilComponents;
		using SteamCoils::SimulateSteamCoilComponents;
		using DataZoneEquipment::UnitHeater_Num;
		using DataHVACGlobals::ZoneCompTurnFansOn;
		using DataHVACGlobals::ZoneCompTurnFansOff;
		using DataHVACGlobals::FanType_SimpleOnOff;
		using PlantUtilities::SetComponentFlowRate;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 AirMassFlow; // total mass flow through the unit
		Real64 CpAirZn; // specific heat of dry air at zone conditions (zone conditions same as unit inlet)
		int HCoilInAirNode; // inlet node number for fan exit/coil inlet
		int InletNode; // unit air inlet node
		int OutletNode; // unit air outlet node
		Real64 QCoilReq; // Heat addition required from an electric/gas heating coil
		Real64 mdot; // local temporary for fluid mass flow rate
		int FanOpMode; // Fan operting mode or fan type
		Real64 PartLoadFrac; // part-load ratio
		// FLOW:

		InletNode = UnitHeat( UnitHeatNum ).AirInNode;
		OutletNode = UnitHeat( UnitHeatNum ).AirOutNode;
		QCoilReq = 0.0;

		if ( present( PartLoadRatio ) ) {
			PartLoadFrac = PartLoadRatio;
		} else {
			PartLoadFrac = 1.0;
		}
		if ( present( OpMode ) ) {
			FanOpMode = OpMode;
		} else {
			FanOpMode = ContFanCycCoil;
		}
		if ( FanOpMode != CycFanCycCoil ) {
			SimulateFanComponents( UnitHeat( UnitHeatNum ).FanName, FirstHVACIteration, UnitHeat( UnitHeatNum ).Fan_Index, _, ZoneCompTurnFansOn, ZoneCompTurnFansOff );

			{ auto const SELECT_CASE_var( UnitHeat( UnitHeatNum ).HCoilType );

			if ( SELECT_CASE_var == WaterCoil ) {

				SimulateWaterCoilComponents( UnitHeat( UnitHeatNum ).HCoilName, FirstHVACIteration, UnitHeat( UnitHeatNum ).HCoil_Index );
			} else if ( SELECT_CASE_var == SteamCoil ) {

				if ( ! HCoilOn ) {
					QCoilReq = 0.0;
				} else {
					HCoilInAirNode = UnitHeat( UnitHeatNum ).FanOutletNode;
					CpAirZn = PsyCpAirFnWTdb( Node( UnitHeat( UnitHeatNum ).AirInNode ).HumRat, Node( UnitHeat( UnitHeatNum ).AirInNode ).Temp );
					QCoilReq = QZnReq - Node( HCoilInAirNode ).MassFlowRate * CpAirZn * ( Node( HCoilInAirNode ).Temp - Node( UnitHeat( UnitHeatNum ).AirInNode ).Temp );
				}
				if ( QCoilReq < 0.0 ) QCoilReq = 0.0; // a heating coil can only heat, not cool
				SimulateSteamCoilComponents( UnitHeat( UnitHeatNum ).HCoilName, FirstHVACIteration, UnitHeat( UnitHeatNum ).HCoil_Index, QCoilReq );

			} else if ( ( SELECT_CASE_var == ElectricCoil ) || ( SELECT_CASE_var == GasCoil ) ) {

				if ( ! HCoilOn ) {
					QCoilReq = 0.0;
				} else {
					HCoilInAirNode = UnitHeat( UnitHeatNum ).FanOutletNode;
					CpAirZn = PsyCpAirFnWTdb( Node( UnitHeat( UnitHeatNum ).AirInNode ).HumRat, Node( UnitHeat( UnitHeatNum ).AirInNode ).Temp );
					QCoilReq = QZnReq - Node( HCoilInAirNode ).MassFlowRate * CpAirZn * ( Node( HCoilInAirNode ).Temp - Node( UnitHeat( UnitHeatNum ).AirInNode ).Temp );
				}
				if ( QCoilReq < 0.0 ) QCoilReq = 0.0; // a heating coil can only heat, not cool
				SimulateHeatingCoilComponents( UnitHeat( UnitHeatNum ).HCoilName, FirstHVACIteration, QCoilReq, UnitHeat( UnitHeatNum ).HCoil_Index );

			}}

			AirMassFlow = Node( OutletNode ).MassFlowRate;

			Node( InletNode ).MassFlowRate = Node( OutletNode ).MassFlowRate; // maintain continuity through unit heater

		} else { // OnOff fan cycling

			Node( InletNode ).MassFlowRate = Node( InletNode ).MassFlowRateMax * PartLoadFrac;
			AirMassFlow = Node( InletNode ).MassFlowRate;
			// Set the fan inlet node maximum available mass flow rates for cycling fans
			Node( InletNode ).MassFlowRateMaxAvail = AirMassFlow;

			if ( QCoilReq < 0.0 ) QCoilReq = 0.0; // a heating coil can only heat, not cool

			SimulateFanComponents( UnitHeat( UnitHeatNum ).FanName, FirstHVACIteration, UnitHeat( UnitHeatNum ).Fan_Index, _, ZoneCompTurnFansOn, ZoneCompTurnFansOff );

			{ auto const SELECT_CASE_var( UnitHeat( UnitHeatNum ).HCoilType );

			if ( SELECT_CASE_var == WaterCoil ) {

				if ( ! HCoilOn ) {
					mdot = 0.0;
					QCoilReq = 0.0;
				} else {
					HCoilInAirNode = UnitHeat( UnitHeatNum ).FanOutletNode;
					CpAirZn = PsyCpAirFnWTdb( Node( UnitHeat( UnitHeatNum ).AirInNode ).HumRat, Node( UnitHeat( UnitHeatNum ).AirInNode ).Temp );
					QCoilReq = QZnReq - Node( HCoilInAirNode ).MassFlowRate * CpAirZn * ( Node( HCoilInAirNode ).Temp - Node( UnitHeat( UnitHeatNum ).AirInNode ).Temp );
					mdot = UnitHeat( UnitHeatNum ).MaxHotWaterFlow * PartLoadFrac;
				}
				if ( QCoilReq < 0.0 ) QCoilReq = 0.0; // a heating coil can only heat, not cool
				SetComponentFlowRate( mdot, UnitHeat( UnitHeatNum ).HotControlNode, UnitHeat( UnitHeatNum ).HotCoilOutNodeNum, UnitHeat( UnitHeatNum ).HWLoopNum, UnitHeat( UnitHeatNum ).HWLoopSide, UnitHeat( UnitHeatNum ).HWBranchNum, UnitHeat( UnitHeatNum ).HWCompNum );
				SimulateWaterCoilComponents( UnitHeat( UnitHeatNum ).HCoilName, FirstHVACIteration, UnitHeat( UnitHeatNum ).HCoil_Index, QCoilReq, FanOpMode, PartLoadFrac );
			} else if ( SELECT_CASE_var == SteamCoil ) {
				if ( ! HCoilOn ) {
					mdot = 0.0;
					QCoilReq = 0.0;
				} else {
					HCoilInAirNode = UnitHeat( UnitHeatNum ).FanOutletNode;
					CpAirZn = PsyCpAirFnWTdb( Node( UnitHeat( UnitHeatNum ).AirInNode ).HumRat, Node( UnitHeat( UnitHeatNum ).AirInNode ).Temp );
					QCoilReq = QZnReq - Node( HCoilInAirNode ).MassFlowRate * CpAirZn * ( Node( HCoilInAirNode ).Temp - Node( UnitHeat( UnitHeatNum ).AirInNode ).Temp );
					mdot = UnitHeat( UnitHeatNum ).MaxHotSteamFlow * PartLoadFrac;
				}
				if ( QCoilReq < 0.0 ) QCoilReq = 0.0; // a heating coil can only heat, not cool
				SetComponentFlowRate( mdot, UnitHeat( UnitHeatNum ).HotControlNode, UnitHeat( UnitHeatNum ).HotCoilOutNodeNum, UnitHeat( UnitHeatNum ).HWLoopNum, UnitHeat( UnitHeatNum ).HWLoopSide, UnitHeat( UnitHeatNum ).HWBranchNum, UnitHeat( UnitHeatNum ).HWCompNum );
				SimulateSteamCoilComponents( UnitHeat( UnitHeatNum ).HCoilName, FirstHVACIteration, UnitHeat( UnitHeatNum ).HCoil_Index, QCoilReq, _, FanOpMode, PartLoadFrac );
			} else if ( ( SELECT_CASE_var == ElectricCoil ) || ( SELECT_CASE_var == GasCoil ) ) {

				if ( ! HCoilOn ) {
					QCoilReq = 0.0;
				} else {
					HCoilInAirNode = UnitHeat( UnitHeatNum ).FanOutletNode;
					CpAirZn = PsyCpAirFnWTdb( Node( UnitHeat( UnitHeatNum ).AirInNode ).HumRat, Node( UnitHeat( UnitHeatNum ).AirInNode ).Temp );
					QCoilReq = QZnReq - Node( HCoilInAirNode ).MassFlowRate * CpAirZn * ( Node( HCoilInAirNode ).Temp - Node( UnitHeat( UnitHeatNum ).AirInNode ).Temp );
				}
				if ( QCoilReq < 0.0 ) QCoilReq = 0.0; // a heating coil can only heat, not cool
				SimulateHeatingCoilComponents( UnitHeat( UnitHeatNum ).HCoilName, FirstHVACIteration, QCoilReq, UnitHeat( UnitHeatNum ).HCoil_Index, _, _, FanOpMode, PartLoadFrac );
			}}
			Node( OutletNode ).MassFlowRate = Node( InletNode ).MassFlowRate; // maintain continuity through unit heater

		}
		LoadMet = AirMassFlow * ( PsyHFnTdbW( Node( OutletNode ).Temp, Node( InletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( InletNode ).HumRat ) );

	}

	//SUBROUTINE UpdateUnitHeater

	// No update routine needed in this module since all of the updates happen on
	// the Node derived type directly and these updates are done by other routines.

	//END SUBROUTINE UpdateUnitHeater

	void
	ReportUnitHeater( int const UnitHeatNum ) // Unit index in unit heater array
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   May 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine needs a description.

		// METHODOLOGY EMPLOYED:
		// Needs description, as appropriate.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::SecInHour;
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
		// na

		// FLOW:
		UnitHeat( UnitHeatNum ).HeatEnergy = UnitHeat( UnitHeatNum ).HeatPower * TimeStepSys * SecInHour;
		UnitHeat( UnitHeatNum ).ElecEnergy = UnitHeat( UnitHeatNum ).ElecPower * TimeStepSys * SecInHour;

	}

	Real64
	CalcUnitHeaterResidual(
		Real64 const PartLoadRatio, // heating coil part load ratio
		Array1< Real64 > const & Par // Function parameters
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Chandan Sharma/Bereket Nigusse, FSEC
		//       DATE WRITTEN   October 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// To calculate the part-load ratio for the unit heater

		// METHODOLOGY EMPLOYED:
		// Use SolveRegulaFalsi to call this Function to converge on a solution

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Residuum( 0.0 ); // Result (force to 0)

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		//   Parameter description example:
		//   Par(1)  = REAL(UnitHeaterNum,r64) ! Index to UnitHeater
		//   Par(2)  = 0.0                     ! FirstHVACIteration FLAG, IF 1.0 then TRUE, if 0.0 then FALSE
		//   Par(3)  = REAL(OpMode,r64)        ! Fan control, IF 1.0 then cycling fan, if 0.0 then continuous fan

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int UnitHeaterNum; // Index to this UnitHeater
		bool FirstHVACIteration; // FirstHVACIteration flag
		int OpMode; // Cycling fan or constant fan
		Real64 QUnitOut; // heating provided by unit heater [watts]

		// Convert parameters to usable variables
		UnitHeaterNum = int( Par( 1 ) ); //Autodesk:OPTIONAL Par used without PRESENT check
		FirstHVACIteration = ( Par( 2 ) == 1.0 );
		OpMode = int( Par( 3 ) );

		CalcUnitHeaterComponents( UnitHeaterNum, FirstHVACIteration, QUnitOut, OpMode, PartLoadRatio );

		// Calculate residual based on output calculation flag
		if ( QZnReq != 0.0 ) {
			Residuum = ( QUnitOut - QZnReq ) / QZnReq;
		}

		return Residuum;

	}

} // UnitHeater

} // EnergyPlus
