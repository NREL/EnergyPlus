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
#include <WindowAC.hh>
#include <BranchNodeConnections.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHVACGlobals.hh>
#include <DataLoopNode.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DataZoneEnergyDemands.hh>
#include <DataZoneEquipment.hh>
#include <DXCoils.hh>
#include <EMSManager.hh>
#include <Fans.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <HVACHXAssistedCoolingCoil.hh>
#include <InputProcessor.hh>
#include <MixedAir.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <Psychrometrics.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace WindowAC {

	// Module containing the routines dealing window air conditioner units

	// MODULE INFORMATION:
	//       AUTHOR         Fred Buhl
	//       DATE WRITTEN   May 2000
	//       MODIFIED       Richard Raustad, FSEC Oct 2003
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To encapsulate the data and algorithms needed to simulate window air
	// conditioner units.

	// METHODOLOGY EMPLOYED:
	// Units are modeled as a collection of components: outside air mixer,
	// fan and DX coil. Control is by means of cycling: either continuous
	// air flow with the DX compressor cycling on/off or the entire unit -
	// fan and compressor cycling on/off. Cycling behavior is not explicitly
	// modeled - instead cycling inefficiencies must be included in the efficiency
	// curves of the DX module.

	// REFERENCES: none

	// OTHER NOTES: none

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataLoopNode;
	using namespace DataSizing;
	using DataGlobals::BeginEnvrnFlag;
	using DataGlobals::BeginDayFlag;
	using DataGlobals::SecInHour;
	using DataGlobals::OutputFileDebug;
	using DataGlobals::SysSizingCalc;
	using DataGlobals::DisplayExtraWarnings;
	using DataEnvironment::OutBaroPress;
	using DataEnvironment::OutRelHum;
	using DataEnvironment::StdBaroPress;
	using DataEnvironment::StdRhoAir;
	using DataHVACGlobals::SmallMassFlow;
	using DataHVACGlobals::SmallLoad;
	using DataHVACGlobals::FanElecPower;
	using DataHVACGlobals::DXElecCoolingPower;
	using DataHVACGlobals::OnOffFanPartLoadFraction;
	using DataHVACGlobals::SmallAirVolFlow;
	using DataHVACGlobals::CoilDX_CoolingSingleSpeed;
	using DataHVACGlobals::CoilDX_CoolingHXAssisted;
	using DataHVACGlobals::CycFanCycCoil;
	using DataHVACGlobals::ContFanCycCoil;
	using DataHVACGlobals::DrawThru;
	using DataHVACGlobals::BlowThru;
	using DataHVACGlobals::SingleHeatingSetPoint;

	// Use statements for access to subroutines in other modules
	using namespace ScheduleManager;
	using Psychrometrics::PsyRhoAirFnPbTdbW;
	using Psychrometrics::PsyCpAirFnWTdb;
	using Psychrometrics::PsyHFnTdbW;

	// Data
	// MODULE PARAMETER DEFINITIONS
	int const WindowAC_UnitType( 1 );
	std::string const cWindowAC_UnitType( "ZoneHVAC:WindowAirConditioner" );
	Array1D_string const cWindowAC_UnitTypes( 1, cWindowAC_UnitType );

	// Compressor operation
	int const On( 1 ); // normal compressor operation
	int const Off( 0 ); // signal DXCoil that compressor shouldn't run

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:

	namespace {
		bool MyOneTimeFlag( true );
		bool ZoneEquipmentListChecked( false );
	}

	int NumWindAC( 0 );
	int NumWindACCyc( 0 );
	Array1D_bool MySizeFlag;
	bool GetWindowACInputFlag( true ); // First time, input is "gotten"
	bool CoolingLoad( false ); // defines a cooling load
	Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE

	// Object Data
	Array1D< WindACData > WindAC;
	Array1D< WindACNumericFieldData > WindACNumericFields; // holds window AC numeric input fields character field name

	// Functions

	void
	clear_state()
	{
		NumWindAC = 0;
		NumWindACCyc = 0;
		GetWindowACInputFlag = true;
		CoolingLoad = false;
		MyOneTimeFlag = true;
		ZoneEquipmentListChecked = false;
		MySizeFlag.deallocate();
		CheckEquipName.deallocate();
		WindAC.deallocate();
		WindACNumericFields.deallocate();
	}

	void
	SimWindowAC(
		std::string const & CompName, // name of the window AC unit
		int const ZoneNum, // number of zone being served
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & PowerMet, // Sensible power supplied by window AC (W)
		Real64 & LatOutputProvided, // Latent add/removal supplied by window AC (kg/s), dehumid = negative
		int & CompIndex // component index
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   May 2000
		//       MODIFIED       Don Shirey, Aug 2009 (LatOutputProvided)
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Manages the simulation of a window AC unit. Called from SimZone Equipment

		// METHODOLOGY EMPLOYED:
		// NA

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::TrimSigDigits;
		using InputProcessor::FindItemInList;
		using DataZoneEnergyDemands::ZoneSysEnergyDemand;
		using DataHeatBalFanSys::TempControlType;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int WindACNum; // index of window AC unit being simulated
		Real64 QZnReq; // zone load (W)
		Real64 RemainingOutputToCoolingSP; // - remaining load to cooling setpoint (W)

		// FLOW

		// First time SimWindowAC is called, get the input for all the window AC units
		if ( GetWindowACInputFlag ) {
			GetWindowAC();
			GetWindowACInputFlag = false;
		}

		// Find the correct Window AC Equipment
		if ( CompIndex == 0 ) {
			WindACNum = FindItemInList( CompName, WindAC );
			if ( WindACNum == 0 ) {
				ShowFatalError( "SimWindowAC: Unit not found=" + CompName );
			}
			CompIndex = WindACNum;
		} else {
			WindACNum = CompIndex;
			if ( WindACNum > NumWindAC || WindACNum < 1 ) {
				ShowFatalError( "SimWindowAC:  Invalid CompIndex passed=" + TrimSigDigits( WindACNum ) + ", Number of Units=" + TrimSigDigits( NumWindAC ) + ", Entered Unit name=" + CompName );
			}
			if ( CheckEquipName( WindACNum ) ) {
				if ( CompName != WindAC( WindACNum ).Name ) {
					ShowFatalError( "SimWindowAC: Invalid CompIndex passed=" + TrimSigDigits( WindACNum ) + ", Unit name=" + CompName + ", stored Unit Name for that index=" + WindAC( WindACNum ).Name );
				}
				CheckEquipName( WindACNum ) = false;
			}
		}

		RemainingOutputToCoolingSP = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToCoolSP;

		if ( RemainingOutputToCoolingSP < 0.0 && TempControlType( ZoneNum ) != SingleHeatingSetPoint ) {
			QZnReq = RemainingOutputToCoolingSP;
		} else {
			QZnReq = 0.0;
		}

		ZoneEqDXCoil = true;
		ZoneCoolingOnlyFan = true;

		// Initialize the window AC unit
		InitWindowAC( WindACNum, QZnReq, ZoneNum, FirstHVACIteration );

		SimCyclingWindowAC( WindACNum, ZoneNum, FirstHVACIteration, PowerMet, QZnReq, LatOutputProvided );

		// Report the result of the simulation
		ReportWindowAC( WindACNum );

		ZoneEqDXCoil = false;
		ZoneCoolingOnlyFan = false;

	}

	void
	GetWindowAC()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   May 2000
		//       MODIFIED       Chandan Sharma, FSEC, March 2011: Added zone sys avail manager
		//                      Bereket Nigusse, FSEC, April 2011: eliminated input node names,
		//                                                         added OA Mixer object type
		//                                                         and fan object type
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Obtains input data for window AC units and stores it in window AC data structures

		// METHODOLOGY EMPLOYED:
		// Uses "Get" routines to read in data.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using InputProcessor::GetObjectDefMaxArgs;
		using InputProcessor::FindItemInList;
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::SetUpCompSets;
		using Fans::GetFanIndex;
		using Fans::GetFanVolFlow;
		using Fans::GetFanAvailSchPtr;
		using Fans::GetFanType;
		using General::TrimSigDigits;
		auto & GetDXCoilOutletNode( DXCoils::GetCoilOutletNode );
		auto & GetDXHXAsstdCoilOutletNode( HVACHXAssistedCoolingCoil::GetCoilOutletNode );
		using DataGlobals::AnyEnergyManagementSystemInModel;
		using DataGlobals::NumOfZones;
		using DataGlobals::ScheduleAlwaysOn;
		using MixedAir::GetOAMixerIndex;
		using MixedAir::GetOAMixerNodeNumbers;
		using DataHVACGlobals::FanType_SimpleConstVolume;
		using DataHVACGlobals::FanType_SimpleOnOff;
		using DataHVACGlobals::cFanTypes;
		using DataZoneEquipment::ZoneEquipConfig;
		using DataSizing::ZoneHVACSizing;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetWindowAC: " ); // include trailing blank space

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int WindACIndex; // loop index
		int WindACNum; // current window AC number
		std::string CompSetFanInlet;
		std::string CompSetCoolInlet;
		std::string CompSetFanOutlet;
		std::string CompSetCoolOutlet;
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		Array1D_int OANodeNums( 4 ); // Node numbers of Outdoor air mixer (OA, EA, RA, MA)
		int IOStatus; // Used in GetObjectItem
		static bool ErrorsFound( false ); // Set to true if errors in input, fatal at end of routine
		static bool errFlag( false ); // Local error flag for GetOAMixerNodeNums
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		static bool FanErrFlag( false ); // Error flag used in GetFanIndex call
		Real64 FanVolFlow; // Fan volumetric flow rate
		bool CoilNodeErrFlag; // Used in error messages for mining coil outlet node number
		std::string CurrentModuleObject; // Object type for getting and error messages
		Array1D_string Alphas; // Alpha input items for object
		Array1D_string cAlphaFields; // Alpha field names
		Array1D_string cNumericFields; // Numeric field names
		Array1D< Real64 > Numbers; // Numeric input items for object
		Array1D_bool lAlphaBlanks; // Logical array, alpha field input BLANK = .TRUE.
		Array1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.
		static int TotalArgs( 0 ); // Total number of alpha and numeric arguments (max) for a
		//  INTEGER                              :: FanType           ! Integer index for Fan type
		int CtrlZone; // index to loop counter
		int NodeNum; // index to loop counter
		bool ZoneNodeNotFound; // used in error checking

		// find the number of each type of window AC unit
		CurrentModuleObject = "ZoneHVAC:WindowAirConditioner";

		NumWindACCyc = GetNumObjectsFound( CurrentModuleObject );
		NumWindAC = NumWindACCyc;
		// allocate the data structures
		WindAC.allocate( NumWindAC );
		CheckEquipName.dimension( NumWindAC, true );
		WindACNumericFields.allocate( NumWindAC );

		GetObjectDefMaxArgs( CurrentModuleObject, TotalArgs, NumAlphas, NumNumbers );

		Alphas.allocate( NumAlphas );
		cAlphaFields.allocate( NumAlphas );
		cNumericFields.allocate( NumNumbers );
		Numbers.dimension( NumNumbers, 0.0 );
		lAlphaBlanks.dimension( NumAlphas, true );
		lNumericBlanks.dimension( NumNumbers, true );

		// loop over window AC units; get and load the input data
		for ( WindACIndex = 1; WindACIndex <= NumWindACCyc; ++WindACIndex ) {

			GetObjectItem( CurrentModuleObject, WindACIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			WindACNum = WindACIndex;

			WindACNumericFields( WindACNum ).FieldNames.allocate( NumNumbers );
			WindACNumericFields( WindACNum ).FieldNames = "";
			WindACNumericFields( WindACNum ).FieldNames = cNumericFields;

			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), WindAC, WindACNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			WindAC( WindACNum ).Name = Alphas( 1 );
			WindAC( WindACNum ).UnitType = WindowAC_UnitType; // 'ZoneHVAC:WindowAirConditioner'
			WindAC( WindACNum ).Sched = Alphas( 2 );
			if ( lAlphaBlanks( 2 ) ) {
				WindAC( WindACNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				WindAC( WindACNum ).SchedPtr = GetScheduleIndex( Alphas( 2 ) ); // convert schedule name to pointer
				if ( WindAC( WindACNum ).SchedPtr == 0 ) {
					ShowSevereError( CurrentModuleObject + "=\"" + WindAC( WindACNum ).Name + "\" invalid data." );
					ShowContinueError( "invalid-not found " + cAlphaFields( 2 ) + "=\"" + Alphas( 2 ) + "\"." );
					ErrorsFound = true;
				}
			}
			WindAC( WindACNum ).MaxAirVolFlow = Numbers( 1 );
			WindAC( WindACNum ).OutAirVolFlow = Numbers( 2 );

			WindAC( WindACNum ).AirInNode = GetOnlySingleNode( Alphas( 3 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsParent );

			WindAC( WindACNum ).AirOutNode = GetOnlySingleNode( Alphas( 4 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsParent );

			WindAC( WindACNum ).OAMixType = Alphas( 5 );
			WindAC( WindACNum ).OAMixName = Alphas( 6 );
			// Get outdoor air mixer node numbers
			errFlag = false;
			ValidateComponent( WindAC( WindACNum ).OAMixType, WindAC( WindACNum ).OAMixName, errFlag, CurrentModuleObject );
			if ( errFlag ) {
				ShowContinueError( "specified in " + CurrentModuleObject + " = \"" + WindAC( WindACNum ).Name + "\"." );
				ErrorsFound = true;
			} else {
				// Get outdoor air mixer node numbers
				OANodeNums = GetOAMixerNodeNumbers( WindAC( WindACNum ).OAMixName, errFlag );
				if ( errFlag ) {
					ShowContinueError( "that was specified in " + CurrentModuleObject + " = \"" + WindAC( WindACNum ).Name + "\"" );
					ShowContinueError( "..OutdoorAir:Mixer is required. Enter an OutdoorAir:Mixer object with this name." );
					ErrorsFound = true;
				} else {
					WindAC( WindACNum ).OutsideAirNode = OANodeNums( 1 );
					WindAC( WindACNum ).AirReliefNode = OANodeNums( 2 );
					WindAC( WindACNum ).MixedAirNode = OANodeNums( 4 );
				}
			}

			WindAC( WindACNum ).FanType = Alphas( 7 );
			WindAC( WindACNum ).FanName = Alphas( 8 );

			FanErrFlag = false;
			ValidateComponent( WindAC( WindACNum ).FanType, WindAC( WindACNum ).FanName, FanErrFlag, CurrentModuleObject );
			if ( FanErrFlag ) {
				ShowContinueError( "specified in " + CurrentModuleObject + " = \"" + WindAC( WindACNum ).Name + "\"." );
				ErrorsFound = true;
			} else {
				GetFanType( WindAC( WindACNum ).FanName, WindAC( WindACNum ).FanType_Num, FanErrFlag, CurrentModuleObject, WindAC( WindACNum ).Name );
				{ auto const SELECT_CASE_var( WindAC( WindACNum ).FanType_Num );
				if ( ( SELECT_CASE_var == FanType_SimpleOnOff ) || ( SELECT_CASE_var == FanType_SimpleConstVolume ) ) {
					GetFanIndex( WindAC( WindACNum ).FanName, WindAC( WindACNum ).FanIndex, FanErrFlag, CurrentModuleObject );
					if ( FanErrFlag ) {
						ShowContinueError( " specified in " + CurrentModuleObject + " = \"" + WindAC( WindACNum ).Name + "\"." );
						ErrorsFound = true;
					} else {
						GetFanVolFlow( WindAC( WindACNum ).FanIndex, FanVolFlow );
						if ( FanVolFlow != AutoSize ) {
							if ( FanVolFlow < WindAC( WindACNum ).MaxAirVolFlow ) {
								ShowWarningError( "Air flow rate = " + TrimSigDigits( FanVolFlow, 7 ) + " in fan object " + WindAC( WindACNum ).FanName + " is less than the maximum supply air flow rate (" + TrimSigDigits( WindAC( WindACNum ).MaxAirVolFlow, 7 ) + ") in the " + CurrentModuleObject + " object." );
								ShowContinueError( " The fan flow rate must be >= to the " + cNumericFields( 1 ) + " in the " + CurrentModuleObject + " object." );
								ShowContinueError( " Occurs in " + CurrentModuleObject + " = " + WindAC( WindACNum ).Name );
								ErrorsFound = true;
							}
						}
					}
				} else {
					ShowSevereError( CurrentModuleObject + " = \"" + Alphas( 1 ) + "\"." );
					ShowContinueError( "Fan Type must be Fan:OnOff, or Fan:ConstantVolume." );
					ErrorsFound = true;
				}}
				// Get the fan's availability schedule
				WindAC( WindACNum ).FanAvailSchedPtr = GetFanAvailSchPtr( WindAC( WindACNum ).FanType, WindAC( WindACNum ).FanName, FanErrFlag );
				if ( FanErrFlag ) {
					ShowContinueError( "...occurs in " + CurrentModuleObject + " = " + WindAC( WindACNum ).Name );
					ErrorsFound = true;
				}
			}

			WindAC( WindACNum ).DXCoilName = Alphas( 10 );

			if ( SameString( Alphas( 9 ), "Coil:Cooling:DX:SingleSpeed" ) || SameString( Alphas( 9 ), "CoilSystem:Cooling:DX:HeatExchangerAssisted" ) ) {
				WindAC( WindACNum ).DXCoilType = Alphas( 9 );
				CoilNodeErrFlag = false;
				if ( SameString( Alphas( 9 ), "Coil:Cooling:DX:SingleSpeed" ) ) {
					WindAC( WindACNum ).DXCoilType_Num = CoilDX_CoolingSingleSpeed;
					WindAC( WindACNum ).CoilOutletNodeNum = GetDXCoilOutletNode( WindAC( WindACNum ).DXCoilType, WindAC( WindACNum ).DXCoilName, CoilNodeErrFlag );
				} else if ( SameString( Alphas( 9 ), "CoilSystem:Cooling:DX:HeatExchangerAssisted" ) ) {
					WindAC( WindACNum ).DXCoilType_Num = CoilDX_CoolingHXAssisted;
					WindAC( WindACNum ).CoilOutletNodeNum = GetDXHXAsstdCoilOutletNode( WindAC( WindACNum ).DXCoilType, WindAC( WindACNum ).DXCoilName, CoilNodeErrFlag );
				}
				if ( CoilNodeErrFlag ) {
					ShowContinueError( " that was specified in " + CurrentModuleObject + " = \"" + WindAC( WindACNum ).Name + "\"." );
					ErrorsFound = true;
				}
			} else {
				ShowWarningError( "Invalid " + cAlphaFields( 9 ) + " = " + Alphas( 9 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + WindAC( WindACNum ).Name );
				ErrorsFound = true;
			}

			WindAC( WindACNum ).FanSchedPtr = GetScheduleIndex( Alphas( 11 ) );

			// Default to cycling fan when fan mode schedule is not present
			if ( ! lAlphaBlanks( 11 ) && WindAC( WindACNum ).FanSchedPtr == 0 ) {
				ShowSevereError( CurrentModuleObject + " \"" + WindAC( WindACNum ).Name + "\" " + cAlphaFields( 11 ) + " not found: " + Alphas( 11 ) );
				ErrorsFound = true;
			} else if ( lAlphaBlanks( 11 ) ) {
				WindAC( WindACNum ).OpMode = CycFanCycCoil;
			}

			if ( SameString( Alphas( 12 ), "BlowThrough" ) ) WindAC( WindACNum ).FanPlace = BlowThru;
			if ( SameString( Alphas( 12 ), "DrawThrough" ) ) WindAC( WindACNum ).FanPlace = DrawThru;
			if ( WindAC( WindACNum ).FanPlace == 0 ) {
				ShowSevereError( "Invalid " + cAlphaFields( 12 ) + " = " + Alphas( 12 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + WindAC( WindACNum ).Name );
				ErrorsFound = true;
			}

			WindAC( WindACNum ).ConvergenceTol = Numbers( 3 );

			if ( ! lAlphaBlanks( 13 ) ) {
				WindAC( WindACNum ).AvailManagerListName = Alphas( 13 );
			}

			WindAC( WindACNum ).HVACSizingIndex = 0;
			if ( ! lAlphaBlanks( 14 ) ) {
				WindAC( WindACNum ).HVACSizingIndex = FindItemInList( Alphas( 14 ), ZoneHVACSizing );
				if ( WindAC( WindACNum ).HVACSizingIndex == 0) {
					ShowSevereError( cAlphaFields( 14 ) + " = " + Alphas( 14 ) + " not found.");
					ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + WindAC( WindACNum ).Name );
					ErrorsFound = true;
				}
			}

			// Add fan to component sets array
			if ( WindAC( WindACNum ).FanPlace == BlowThru ) {

				// Window AC air inlet node must be the same as a zone exhaust node and the OA Mixer return node
				// check that Window AC air inlet node is the same as a zone exhaust node.
				ZoneNodeNotFound = true;
				for ( CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone ) {
					if ( ! ZoneEquipConfig( CtrlZone ).IsControlled ) continue;
					for ( NodeNum = 1; NodeNum <= ZoneEquipConfig( CtrlZone ).NumExhaustNodes; ++NodeNum ) {
						if ( WindAC( WindACNum ).AirInNode == ZoneEquipConfig( CtrlZone ).ExhaustNode( NodeNum ) ) {
							ZoneNodeNotFound = false;
							break;
						}
					}
				}
				if ( ZoneNodeNotFound ) {
					ShowSevereError( CurrentModuleObject + " = \"" + WindAC( WindACNum ).Name + "\". Window AC air inlet node name must be the same as a zone exhaust node name." );
					ShowContinueError( "..Zone exhaust node name is specified in ZoneHVAC:EquipmentConnections object." );
					ShowContinueError( "..Window AC air inlet node name = " + NodeID( WindAC( WindACNum ).AirInNode ) );
					ErrorsFound = true;
				}
				// check that Window AC air outlet node is a zone inlet node.
				ZoneNodeNotFound = true;
				for ( CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone ) {
					if ( ! ZoneEquipConfig( CtrlZone ).IsControlled ) continue;
					for ( NodeNum = 1; NodeNum <= ZoneEquipConfig( CtrlZone ).NumInletNodes; ++NodeNum ) {
						if ( WindAC( WindACNum ).AirOutNode == ZoneEquipConfig( CtrlZone ).InletNode( NodeNum ) ) {
							WindAC(WindACNum).ZonePtr = CtrlZone;
							ZoneNodeNotFound = false;
							break;
						}
					}
				}
				if ( ZoneNodeNotFound ) {
					ShowSevereError( CurrentModuleObject + " = \"" + WindAC( WindACNum ).Name + "\". Window AC air outlet node name must be the same as a zone inlet node name." );
					ShowContinueError( "..Zone inlet node name is specified in ZoneHVAC:EquipmentConnections object." );
					ShowContinueError( "..Window AC air outlet node name = " + NodeID( WindAC( WindACNum ).AirOutNode ) );
					ErrorsFound = true;
				}
				CompSetFanInlet = NodeID( WindAC( WindACNum ).MixedAirNode );
				CompSetFanOutlet = "UNDEFINED";
				CompSetCoolInlet = "UNDEFINED";
				CompSetCoolOutlet = NodeID( WindAC( WindACNum ).AirOutNode );
			} else { // draw through fan from IF (WindAC(WindACNum)%FanPlace == BlowThru) THEN
				// check that Window AC air inlet node is the same as a zone exhaust node.
				ZoneNodeNotFound = true;
				for ( CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone ) {
					if ( ! ZoneEquipConfig( CtrlZone ).IsControlled ) continue;
					for ( NodeNum = 1; NodeNum <= ZoneEquipConfig( CtrlZone ).NumExhaustNodes; ++NodeNum ) {
						if ( WindAC( WindACNum ).AirInNode == ZoneEquipConfig( CtrlZone ).ExhaustNode( NodeNum ) ) {
							ZoneNodeNotFound = false;
							break;
						}
					}
				}
				if ( ZoneNodeNotFound ) {
					ShowSevereError( CurrentModuleObject + " = \"" + WindAC( WindACNum ).Name + "\"." " Window AC air inlet node name must be the same as a zone exhaust node name." );
					ShowContinueError( "..Zone exhaust node name is specified in ZoneHVAC:EquipmentConnections object." );
					ShowContinueError( "..Window AC inlet node name = " + NodeID( WindAC( WindACNum ).AirInNode ) );
					ErrorsFound = true;
				}
				// check that Window AC air outlet node is the same as a zone inlet node.
				ZoneNodeNotFound = true;
				for ( CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone ) {
					if ( ! ZoneEquipConfig( CtrlZone ).IsControlled ) continue;
					for ( NodeNum = 1; NodeNum <= ZoneEquipConfig( CtrlZone ).NumInletNodes; ++NodeNum ) {
						if ( WindAC( WindACNum ).AirOutNode == ZoneEquipConfig( CtrlZone ).InletNode( NodeNum ) ) {
							WindAC( WindACNum ).ZonePtr = CtrlZone;
							ZoneNodeNotFound = false;
							break;
						}
					}
				}
				if ( ZoneNodeNotFound ) {
					ShowSevereError( CurrentModuleObject + " = \"" + WindAC( WindACNum ).Name + "\". Window AC air outlet node name must be the same as a zone inlet node name." );
					ShowContinueError( "..Zone inlet node name is specified in ZoneHVAC:EquipmentConnections object." );
					ShowContinueError( "..Window AC outlet node name = " + NodeID( WindAC( WindACNum ).AirOutNode ) );
					ErrorsFound = true;
				}
				CompSetFanInlet = NodeID( WindAC( WindACNum ).CoilOutletNodeNum );
				CompSetFanOutlet = NodeID( WindAC( WindACNum ).AirOutNode );
				CompSetCoolInlet = NodeID( WindAC( WindACNum ).MixedAirNode );
				CompSetCoolOutlet = NodeID( WindAC( WindACNum ).CoilOutletNodeNum );
			}
			// Add fan to component sets array
			SetUpCompSets( cWindowAC_UnitTypes( WindAC( WindACNum ).UnitType ), WindAC( WindACNum ).Name, WindAC( WindACNum ).FanType, WindAC( WindACNum ).FanName, CompSetFanInlet, CompSetFanOutlet );

			// Add cooling coil to component sets array
			SetUpCompSets( cWindowAC_UnitTypes( WindAC( WindACNum ).UnitType ), WindAC( WindACNum ).Name, WindAC( WindACNum ).DXCoilType, WindAC( WindACNum ).DXCoilName, CompSetCoolInlet, CompSetCoolOutlet );

			// Set up component set for OA mixer - use OA node and Mixed air node
			SetUpCompSets( cWindowAC_UnitTypes( WindAC( WindACNum ).UnitType ), WindAC( WindACNum ).Name, WindAC( WindACNum ).OAMixType, WindAC( WindACNum ).OAMixName, NodeID( WindAC( WindACNum ).OutsideAirNode ), NodeID( WindAC( WindACNum ).MixedAirNode ) );
		}

		Alphas.deallocate();
		cAlphaFields.deallocate();
		cNumericFields.deallocate();
		Numbers.deallocate();
		lAlphaBlanks.deallocate();
		lNumericBlanks.deallocate();

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in getting " + CurrentModuleObject + " input.  Preceding condition causes termination." );
		}

		for ( WindACNum = 1; WindACNum <= NumWindAC; ++WindACNum ) {
			// Setup Report variables for the Fan Coils
			SetupOutputVariable( "Zone Window Air Conditioner Total Cooling Rate [W]", WindAC( WindACNum ).TotCoolEnergyRate, "System", "Average", WindAC( WindACNum ).Name );
			SetupOutputVariable( "Zone Window Air Conditioner Total Cooling Energy [J]", WindAC( WindACNum ).TotCoolEnergy, "System", "Sum", WindAC( WindACNum ).Name );
			SetupOutputVariable( "Zone Window Air Conditioner Sensible Cooling Rate [W]", WindAC( WindACNum ).SensCoolEnergyRate, "System", "Average", WindAC( WindACNum ).Name );
			SetupOutputVariable( "Zone Window Air Conditioner Sensible Cooling Energy [J]", WindAC( WindACNum ).SensCoolEnergy, "System", "Sum", WindAC( WindACNum ).Name );
			SetupOutputVariable( "Zone Window Air Conditioner Latent Cooling Rate [W]", WindAC( WindACNum ).LatCoolEnergyRate, "System", "Average", WindAC( WindACNum ).Name );
			SetupOutputVariable( "Zone Window Air Conditioner Latent Cooling Energy [J]", WindAC( WindACNum ).LatCoolEnergy, "System", "Sum", WindAC( WindACNum ).Name );
			SetupOutputVariable( "Zone Window Air Conditioner Electric Power [W]", WindAC( WindACNum ).ElecPower, "System", "Average", WindAC( WindACNum ).Name );
			SetupOutputVariable( "Zone Window Air Conditioner Electric Energy [J]", WindAC( WindACNum ).ElecConsumption, "System", "Sum", WindAC( WindACNum ).Name );
			SetupOutputVariable( "Zone Window Air Conditioner Fan Part Load Ratio []", WindAC( WindACNum ).FanPartLoadRatio, "System", "Average", WindAC( WindACNum ).Name );
			SetupOutputVariable( "Zone Window Air Conditioner Compressor Part Load Ratio []", WindAC( WindACNum ).CompPartLoadRatio, "System", "Average", WindAC( WindACNum ).Name );
			SetupOutputVariable( "Zone Window Air Conditioner Fan Availability Status []", WindAC( WindACNum ).AvailStatus, "System", "Average", WindAC( WindACNum ).Name );
			if ( AnyEnergyManagementSystemInModel ) {
				SetupEMSActuator( "Window Air Conditioner", WindAC( WindACNum ).Name, "Part Load Ratio", "[fraction]", WindAC( WindACNum ).EMSOverridePartLoadFrac, WindAC( WindACNum ).EMSValueForPartLoadFrac );
			}

		}

	}

	void
	InitWindowAC(
		int const WindACNum, // number of the current window AC unit being simulated
		Real64 & QZnReq, // zone load (modified as needed) (W)
		int const ZoneNum, // index to zone
		bool const FirstHVACIteration // TRUE when first HVAC iteration
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   May 2000
		//       MODIFIED       Chandan Sharma, FSEC, March 2011: Added zone sys avail manager
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for initializations of the Window AC Components.

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger initializations.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataZoneEquipment::ZoneEquipInputsFilled;
		using DataZoneEquipment::CheckZoneEquipmentList;
		using DataZoneEquipment::WindowAC_Num;
		using DataZoneEnergyDemands::ZoneSysEnergyDemand;
		using DataZoneEnergyDemands::CurDeadBandOrSetback;
		using DataHVACGlobals::ZoneComp;
		using DataHVACGlobals::ZoneCompTurnFansOn;
		using DataHVACGlobals::ZoneCompTurnFansOff;
		using DataHVACGlobals::SmallLoad;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int InNode; // inlet node number in window AC loop
		int OutNode; // outlet node number in window AC loop
		int InletNode; // inlet node number for window AC WindACNum
		int OutsideAirNode; // outside air node number in window AC loop
		int AirRelNode; // relief air node number in window AC loop
		Real64 RhoAir; // air density at InNode
		//////////// hoisted into namespace ////////////////////////////////////////////////
		// static bool MyOneTimeFlag( true );
		// static bool ZoneEquipmentListChecked( false ); // True after the Zone Equipment List has been checked for items
		////////////////////////////////////////////////////////////////////////////////////
		int Loop; // loop counter
		static Array1D_bool MyEnvrnFlag; // one time initialization flag
		static Array1D_bool MyZoneEqFlag; // used to set up zone equipment availability managers
		Real64 QToCoolSetPt; // sensible load to cooling setpoint (W)
		Real64 NoCompOutput; // sensible load delivered with compressor off (W)

		// Do the one time initializations
		if ( MyOneTimeFlag ) {

			MyEnvrnFlag.allocate( NumWindAC );
			MySizeFlag.allocate( NumWindAC );
			MyZoneEqFlag.allocate ( NumWindAC );
			MyEnvrnFlag = true;
			MySizeFlag = true;
			MyZoneEqFlag = true;
			MyOneTimeFlag = false;

		}

		if ( allocated( ZoneComp ) ) {
			if ( MyZoneEqFlag( WindACNum ) ) { // initialize the name of each availability manager list and zone number
				ZoneComp( WindowAC_Num ).ZoneCompAvailMgrs( WindACNum ).AvailManagerListName = WindAC( WindACNum ).AvailManagerListName;
				ZoneComp( WindowAC_Num ).ZoneCompAvailMgrs( WindACNum ).ZoneNum = ZoneNum;
				MyZoneEqFlag ( WindACNum ) = false;
			}
			WindAC( WindACNum ).AvailStatus = ZoneComp( WindowAC_Num ).ZoneCompAvailMgrs( WindACNum ).AvailStatus;
		}

		// need to check all Window AC units to see if they are on Zone Equipment List or issue warning
		if ( ! ZoneEquipmentListChecked && ZoneEquipInputsFilled ) {
			ZoneEquipmentListChecked = true;
			for ( Loop = 1; Loop <= NumWindAC; ++Loop ) {
				if ( CheckZoneEquipmentList( cWindowAC_UnitTypes( WindAC( Loop ).UnitType ), WindAC( Loop ).Name ) ) continue;
				ShowSevereError( "InitWindowAC: Window AC Unit=[" + cWindowAC_UnitTypes( WindAC( Loop ).UnitType ) + ',' + WindAC( Loop ).Name + "] is not on any ZoneHVAC:EquipmentList.  It will not be simulated." );
			}
		}

		if ( ! SysSizingCalc && MySizeFlag( WindACNum ) ) {

			SizeWindowAC( WindACNum );

			MySizeFlag( WindACNum ) = false;
		}

		// Do the Begin Environment initializations
		if ( BeginEnvrnFlag && MyEnvrnFlag( WindACNum ) ) {
			InNode = WindAC( WindACNum ).AirInNode;
			OutNode = WindAC( WindACNum ).AirOutNode;
			OutsideAirNode = WindAC( WindACNum ).OutsideAirNode;
			RhoAir = StdRhoAir;
			// set the mass flow rates from the input volume flow rates
			WindAC( WindACNum ).MaxAirMassFlow = RhoAir * WindAC( WindACNum ).MaxAirVolFlow;
			WindAC( WindACNum ).OutAirMassFlow = RhoAir * WindAC( WindACNum ).OutAirVolFlow;
			// set the node max and min mass flow rates
			Node( OutsideAirNode ).MassFlowRateMax = WindAC( WindACNum ).OutAirMassFlow;
			Node( OutsideAirNode ).MassFlowRateMin = 0.0;
			Node( OutNode ).MassFlowRateMax = WindAC( WindACNum ).MaxAirMassFlow;
			Node( OutNode ).MassFlowRateMin = 0.0;
			Node( InNode ).MassFlowRateMax = WindAC( WindACNum ).MaxAirMassFlow;
			Node( InNode ).MassFlowRateMin = 0.0;
			MyEnvrnFlag( WindACNum ) = false;
		} // end one time inits

		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( WindACNum ) = true;
		}

		if ( WindAC( WindACNum ).FanSchedPtr > 0 ) {
			if ( GetCurrentScheduleValue( WindAC( WindACNum ).FanSchedPtr ) == 0.0 ) {
				WindAC( WindACNum ).OpMode = CycFanCycCoil;
			} else {
				WindAC( WindACNum ).OpMode = ContFanCycCoil;
			}
		}

		// These initializations are done every iteration
		InletNode = WindAC( WindACNum ).AirInNode;
		OutsideAirNode = WindAC( WindACNum ).OutsideAirNode;
		AirRelNode = WindAC( WindACNum ).AirReliefNode;
		// Set the inlet node mass flow rate
		if ( GetCurrentScheduleValue( WindAC( WindACNum ).SchedPtr ) <= 0.0 || ( GetCurrentScheduleValue( WindAC( WindACNum ).FanAvailSchedPtr ) <= 0.0 && ! ZoneCompTurnFansOn ) || ZoneCompTurnFansOff ) {
			WindAC( WindACNum ).PartLoadFrac = 0.0;
			Node( InletNode ).MassFlowRate = 0.0;
			Node( InletNode ).MassFlowRateMaxAvail = 0.0;
			Node( InletNode ).MassFlowRateMinAvail = 0.0;
			Node( OutsideAirNode ).MassFlowRate = 0.0;
			Node( OutsideAirNode ).MassFlowRateMaxAvail = 0.0;
			Node( OutsideAirNode ).MassFlowRateMinAvail = 0.0;
			Node( AirRelNode ).MassFlowRate = 0.0;
			Node( AirRelNode ).MassFlowRateMaxAvail = 0.0;
			Node( AirRelNode ).MassFlowRateMinAvail = 0.0;
		} else {
			WindAC( WindACNum ).PartLoadFrac = 1.0;
			Node( InletNode ).MassFlowRate = WindAC( WindACNum ).MaxAirMassFlow;
			Node( InletNode ).MassFlowRateMaxAvail = Node( InletNode ).MassFlowRate;
			Node( InletNode ).MassFlowRateMinAvail = Node( InletNode ).MassFlowRate;
			Node( OutsideAirNode ).MassFlowRate = WindAC( WindACNum ).OutAirMassFlow;
			Node( OutsideAirNode ).MassFlowRateMaxAvail = WindAC( WindACNum ).OutAirMassFlow;
			Node( OutsideAirNode ).MassFlowRateMinAvail = 0.0;
			Node( AirRelNode ).MassFlowRate = WindAC( WindACNum ).OutAirMassFlow;
			Node( AirRelNode ).MassFlowRateMaxAvail = WindAC( WindACNum ).OutAirMassFlow;
			Node( AirRelNode ).MassFlowRateMinAvail = 0.0;
		}

		// Original thermostat control logic (works only for cycling fan systems)
		if ( QZnReq < ( -1.0 * SmallLoad ) && ! CurDeadBandOrSetback( ZoneNum ) && WindAC( WindACNum ).PartLoadFrac > 0.0 ) {
			CoolingLoad = true;
		} else {
			CoolingLoad = false;
		}

		// Constant fan systems are tested for ventilation load to determine if load to be met changes.
		if ( WindAC( WindACNum ).OpMode == ContFanCycCoil && WindAC( WindACNum ).PartLoadFrac > 0.0 && ( GetCurrentScheduleValue( WindAC( WindACNum ).FanAvailSchedPtr ) > 0.0 || ZoneCompTurnFansOn ) && ! ZoneCompTurnFansOn ) {

			CalcWindowACOutput( WindACNum, FirstHVACIteration, WindAC( WindACNum ).OpMode, 0.0, false, NoCompOutput );

			QToCoolSetPt = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToCoolSP;

			// If the unit has a net heating capacity and the zone temp is below the Tstat cooling setpoint
			if ( NoCompOutput > ( -1.0 * SmallLoad ) && QToCoolSetPt > ( -1.0 * SmallLoad ) && CurDeadBandOrSetback( ZoneNum ) ) {
				if ( NoCompOutput > QToCoolSetPt ) {
					QZnReq = QToCoolSetPt;
					CoolingLoad = true;
				}
			}
		}

	}

	void
	SizeWindowAC( int const WindACNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   January 2002
		//       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
		//                      July 2014, B. Nigusse, added scalable sizing
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing Window AC  Unit components for which flow rates have not been
		// specified in the input.

		// METHODOLOGY EMPLOYED:
		// Obtains flow rates from the zone or system sizing arrays

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSizing;
		using namespace InputProcessor;
		using ReportSizingManager::ReportSizingOutput;
		using ReportSizingManager::RequestSizing;
		using General::RoundSigDigits;
		using DataHVACGlobals::SystemAirflowSizing;
		using DataHVACGlobals::CoolingAirflowSizing;
		using DataHVACGlobals::CoolingCapacitySizing;
		using DataHeatBalance::Zone;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName("SizeWindowAC: "); // include trailing blank space

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 MaxAirVolFlowDes; // Autosized maximum air flow for reporting
		Real64 MaxAirVolFlowUser; // Hardsized maximum air flow for reporting
		Real64 OutAirVolFlowDes; // Autosized outdoor air flow for reporting
		Real64 OutAirVolFlowUser; // Hardsized outdoor ari flow for reporting
		bool IsAutoSize; // Indicator to autosize
		std::string CompName; // component name
		std::string CompType; // component type
		std::string SizingString; // input field sizing description (e.g., Nominal Capacity)
		Real64 TempSize; // autosized value of coil input field
		int FieldNum = 2; // IDD numeric field number where input field description is found
		int SizingMethod; // Integer representation of sizing method name (e.g., CoolingAirflowSizing, HeatingAirflowSizing, CoolingCapacitySizing, HeatingCapacitySizing, etc.)
		bool PrintFlag; // TRUE when sizing information is reported in the eio file
		int zoneHVACIndex; // index of zoneHVAC equipment sizing specification
		int SAFMethod( 0 ); // supply air flow rate sizing method (SupplyAirFlowRate, FlowPerFloorArea, FractionOfAutosizedCoolingAirflow, FractionOfAutosizedHeatingAirflow ...)
		int CapSizingMethod(0);  // capacity sizing methods (HeatingDesignCapacity, CapacityPerFloorArea, FractionOfAutosizedCoolingCapacity, and FractionOfAutosizedHeatingCapacity )

		IsAutoSize = false;
		MaxAirVolFlowDes = 0.0;
		MaxAirVolFlowUser = 0.0;
		OutAirVolFlowDes = 0.0;
		OutAirVolFlowUser = 0.0;
		DataFracOfAutosizedCoolingAirflow = 1.0;
		DataFracOfAutosizedHeatingAirflow = 1.0;
		DataFracOfAutosizedCoolingCapacity = 1.0;
		DataFracOfAutosizedHeatingCapacity = 1.0;
		DataScalableSizingON = false;
		ZoneHeatingOnlyFan = false;
		ZoneCoolingOnlyFan = true;
		CompType = "ZoneHVAC:WindowAirConditioner";
		CompName = WindAC( WindACNum ).Name;
		DataZoneNumber = WindAC( WindACNum ).ZonePtr;

		if ( CurZoneEqNum > 0 ) {
			if ( WindAC( WindACNum ).HVACSizingIndex > 0 ) {
				zoneHVACIndex = WindAC( WindACNum ).HVACSizingIndex;
				// N1 , \field Maximum Supply Air Flow Rate
				SizingMethod = CoolingAirflowSizing;
				FieldNum = 1;
				SizingString = WindACNumericFields( WindACNum ).FieldNames( FieldNum ) + " [m3/s]";
				PrintFlag = true;
				SAFMethod = ZoneHVACSizing( zoneHVACIndex ).CoolingSAFMethod;
				ZoneEqSizing( CurZoneEqNum ).SizingMethod( SizingMethod ) = SAFMethod;
				if ( SAFMethod == None || SAFMethod == SupplyAirFlowRate || SAFMethod == FlowPerFloorArea || SAFMethod == FractionOfAutosizedCoolingAirflow ) {
					if ( SAFMethod == SupplyAirFlowRate ) {
						if ( ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow > 0.0 ) {
							ZoneEqSizing( CurZoneEqNum ).AirVolFlow = ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow;
							ZoneEqSizing( CurZoneEqNum ).SystemAirFlow = true;
						}
						TempSize = ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow;
					} else if ( SAFMethod == FlowPerFloorArea ) {
						ZoneEqSizing( CurZoneEqNum ).SystemAirFlow = true;
						ZoneEqSizing( CurZoneEqNum ).AirVolFlow = ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow * Zone( DataZoneNumber ).FloorArea;
						TempSize = ZoneEqSizing( CurZoneEqNum ).AirVolFlow;
						DataScalableSizingON = true;
					} else if ( SAFMethod == FractionOfAutosizedCoolingAirflow ) {
						DataFracOfAutosizedCoolingAirflow = ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow;
						TempSize = AutoSize;
						DataScalableSizingON = true;
					} else {
						TempSize = ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow;
					}
					RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
					WindAC( WindACNum ).MaxAirVolFlow = TempSize;

				} else if ( SAFMethod == FlowPerCoolingCapacity ) {
					SizingMethod = CoolingCapacitySizing;
					TempSize = AutoSize;
					PrintFlag = false;
					DataScalableSizingON = true;
					DataFlowUsedForSizing = FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow;
					if ( ZoneHVACSizing( zoneHVACIndex ).CoolingCapMethod == FractionOfAutosizedCoolingCapacity ) {
						DataFracOfAutosizedCoolingCapacity = ZoneHVACSizing( zoneHVACIndex ).ScaledCoolingCapacity;
					}
					RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
					DataCapacityUsedForSizing = TempSize;
					DataFlowPerCoolingCapacity = ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow;
					SizingMethod = CoolingAirflowSizing;
					PrintFlag = true;
					TempSize = AutoSize;
					RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
					WindAC( WindACNum ).MaxAirVolFlow = TempSize;
				}
				//DataScalableSizingON = false;

				// initialize capacity sizing variables: cooling
				CapSizingMethod = ZoneHVACSizing( zoneHVACIndex ).CoolingCapMethod;
				ZoneEqSizing( CurZoneEqNum ).SizingMethod( SizingMethod ) = CapSizingMethod;
				if ( CapSizingMethod == CoolingDesignCapacity || CapSizingMethod == CapacityPerFloorArea || CapSizingMethod == FractionOfAutosizedCoolingCapacity ) {
					if ( CapSizingMethod == HeatingDesignCapacity ) {
						if ( ZoneHVACSizing( zoneHVACIndex ).ScaledCoolingCapacity > 0.0 ) {
							ZoneEqSizing( CurZoneEqNum ).CoolingCapacity = true;
							ZoneEqSizing( CurZoneEqNum ).DesCoolingLoad = ZoneHVACSizing( zoneHVACIndex ).ScaledCoolingCapacity;
						}
					} else if ( CapSizingMethod == CapacityPerFloorArea ) {
						ZoneEqSizing( CurZoneEqNum ).CoolingCapacity = true;
						ZoneEqSizing( CurZoneEqNum ).DesCoolingLoad = ZoneHVACSizing( zoneHVACIndex ).ScaledCoolingCapacity * Zone( DataZoneNumber ).FloorArea;
						DataScalableCapSizingON = true;
					} else if ( CapSizingMethod == FractionOfAutosizedCoolingCapacity ) {
						DataFracOfAutosizedCoolingCapacity = ZoneHVACSizing( zoneHVACIndex ).ScaledCoolingCapacity;
						DataScalableCapSizingON = true;
					}
				}
			} else {
				// no scalble sizing method has been specified. Sizing proceeds using the method
				// specified in the zoneHVAC object
				// N1 , \field Maximum Supply Air Flow Rate
				SizingMethod = SystemAirflowSizing;
				FieldNum = 1;
				PrintFlag = true;
				SizingString = WindACNumericFields( WindACNum ).FieldNames( FieldNum ) + " [m3/s]";
				TempSize = WindAC( WindACNum ).MaxAirVolFlow;
				RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
				WindAC( WindACNum ).MaxAirVolFlow = TempSize;
			}
		}

		if ( WindAC( WindACNum ).OutAirVolFlow == AutoSize ) {

			if ( CurZoneEqNum > 0 ) {

				CheckZoneSizing( cWindowAC_UnitTypes( WindAC( WindACNum ).UnitType ), WindAC( WindACNum ).Name );
				WindAC( WindACNum ).OutAirVolFlow = min( FinalZoneSizing( CurZoneEqNum ).MinOA, WindAC( WindACNum ).MaxAirVolFlow );
				if ( WindAC( WindACNum ).OutAirVolFlow < SmallAirVolFlow ) {
					WindAC( WindACNum ).OutAirVolFlow = 0.0;
				}
				ReportSizingOutput( cWindowAC_UnitTypes( WindAC( WindACNum ).UnitType ), WindAC( WindACNum ).Name, "Maximum Outdoor Air Flow Rate [m3/s]", WindAC( WindACNum ).OutAirVolFlow );

			}

		}

		if ( CurZoneEqNum > 0 ) {
			ZoneEqSizing( CurZoneEqNum ).OAVolFlow = WindAC( WindACNum ).OutAirVolFlow;
			ZoneEqSizing( CurZoneEqNum ).AirVolFlow = WindAC( WindACNum ).MaxAirVolFlow;
		}

	}

	void
	SimCyclingWindowAC(
		int const WindACNum, // number of the current window AC unit being simulated
		int const EP_UNUSED( ZoneNum ), // number of zone being served !unused1208
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & PowerMet, // Sensible power supplied (W)
		Real64 const QZnReq, // Sensible load to be met (W)
		Real64 & LatOutputProvided // Latent power supplied (kg/s), negative = dehumidification
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   May 2000
		//       MODIFIED       Buhl/Shirey Mar 2001, Shirey Aug 2009 (LatOutputProvided)
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Simulate a cycling window air conditioner unit; adjust its output to match the
		// remaining zone load.

		// METHODOLOGY EMPLOYED:
		// If unit is on, calls ControlWindACOutput to obtain the desired unit output

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
		Real64 PartLoadFrac; // unit part load fraction
		Real64 QUnitOut; // Dry air sens. cooling provided by AC unit [watts]
		Real64 SensCoolOut; // Moist air sensible cooling rate [W]
		Real64 LatentOutput; // Latent (moisture) add/removal rate, negative is dehumidification [kg/s]
		bool UnitOn; // TRUE if unit is on
		bool CoilOn; // TRUE if coil is on
		int OutletNode; // unit air outlet node
		int InletNode; // unit air inlet node
		Real64 QTotUnitOut; // total unit output [watts]
		Real64 AirMassFlow; // air mass flow rate [kg/sec]
		Real64 CpAir; // inlet air specific heat [J/kg-C]
		Real64 Test;
		int OpMode; // operating mode (fan cycling or continious; DX coil always cycles)
		Real64 MinHumRat; // minimum of inlet & outlet humidity ratio
		bool HXUnitOn; // Used to control HX heat recovery as needed
		Real64 SpecHumOut; // Specific humidity ratio of outlet air (kg moisture / kg moist air)
		Real64 SpecHumIn; // Specific humidity ratio of inlet air (kg moisture / kg moist air)

		// zero the fan and DX coil electricity consumption
		FanElecPower = 0.0;
		DXElecCoolingPower = 0.0;
		// initialize local variables
		UnitOn = true;
		CoilOn = true;
		QUnitOut = 0.0;
		LatentOutput = 0.0;
		OutletNode = WindAC( WindACNum ).AirOutNode;
		InletNode = WindAC( WindACNum ).AirInNode;
		AirMassFlow = Node( InletNode ).MassFlowRate;
		Test = AirMassFlow;
		CpAir = PsyCpAirFnWTdb( Node( InletNode ).HumRat, Node( InletNode ).Temp );
		OpMode = WindAC( WindACNum ).OpMode;

		// set the on/off flags
		if ( WindAC( WindACNum ).OpMode == CycFanCycCoil ) {
			// cycling unit: only runs if there is a load.
			if ( ! CoolingLoad || AirMassFlow < SmallMassFlow ) {
				UnitOn = false;
				CoilOn = false;
			}
		} else if ( WindAC( WindACNum ).OpMode == ContFanCycCoil ) {
			// continuous unit: fan runs if scheduled on; coil runs only if cooling load
			if ( AirMassFlow < SmallMassFlow ) {
				UnitOn = false;
				CoilOn = false;
			} else if ( ! CoolingLoad ) {
				CoilOn = false;
			}
		}

		OnOffFanPartLoadFraction = 1.0;

		if ( UnitOn && CoilOn ) {
			HXUnitOn = false;
			ControlCycWindACOutput( WindACNum, FirstHVACIteration, OpMode, QZnReq, PartLoadFrac, HXUnitOn );
		} else {
			PartLoadFrac = 0.0;
			HXUnitOn = false;
		}

		WindAC( WindACNum ).PartLoadFrac = PartLoadFrac;

		CalcWindowACOutput( WindACNum, FirstHVACIteration, OpMode, PartLoadFrac, HXUnitOn, QUnitOut );

		// Reseting AirMassFlow to inlet node mass flow rate since inlet mass flow rate may be getting
		// manipulated in subroutine CalcWindowACOutput

		AirMassFlow = Node( InletNode ).MassFlowRate;
		MinHumRat = min( Node( InletNode ).HumRat, Node( OutletNode ).HumRat );
		QUnitOut = AirMassFlow * ( PsyHFnTdbW( Node( OutletNode ).Temp, MinHumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, MinHumRat ) );

		SensCoolOut = AirMassFlow * ( PsyHFnTdbW( Node( OutletNode ).Temp, MinHumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, MinHumRat ) );

		// CR9155 Remove specific humidity calculations
		SpecHumOut = Node( OutletNode ).HumRat;
		SpecHumIn = Node( InletNode ).HumRat;
		LatentOutput = AirMassFlow * ( SpecHumOut - SpecHumIn ); // Latent rate, kg/s

		QTotUnitOut = AirMassFlow * ( Node( OutletNode ).Enthalpy - Node( InletNode ).Enthalpy );

		// report variables
		WindAC( WindACNum ).CompPartLoadRatio = WindAC( WindACNum ).PartLoadFrac;
		if ( WindAC( WindACNum ).OpMode == CycFanCycCoil ) {
			WindAC( WindACNum ).FanPartLoadRatio = WindAC( WindACNum ).PartLoadFrac;
		} else {
			if ( UnitOn ) {
				WindAC( WindACNum ).FanPartLoadRatio = 1.0;
			} else {
				WindAC( WindACNum ).FanPartLoadRatio = 0.0;
			}
		}
		WindAC( WindACNum ).SensCoolEnergyRate = std::abs( min( 0.0, SensCoolOut ) );
		WindAC( WindACNum ).TotCoolEnergyRate = std::abs( min( 0.0, QTotUnitOut ) );
		WindAC( WindACNum ).SensCoolEnergyRate = min( WindAC( WindACNum ).SensCoolEnergyRate, WindAC( WindACNum ).TotCoolEnergyRate );
		WindAC( WindACNum ).LatCoolEnergyRate = WindAC( WindACNum ).TotCoolEnergyRate - WindAC( WindACNum ).SensCoolEnergyRate;
		WindAC( WindACNum ).ElecPower = FanElecPower + DXElecCoolingPower;

		PowerMet = QUnitOut;
		LatOutputProvided = LatentOutput;

	}

	void
	ReportWindowAC( int const WindACNum ) // number of the current AC unit being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   May 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Fills some of the report variables for the window AC units

		// METHODOLOGY EMPLOYED:
		// NA

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
		Real64 ReportingConstant;

		// FLOW

		ReportingConstant = TimeStepSys * SecInHour;

		WindAC( WindACNum ).SensCoolEnergy = WindAC( WindACNum ).SensCoolEnergyRate * ReportingConstant;
		WindAC( WindACNum ).TotCoolEnergy = WindAC( WindACNum ).TotCoolEnergyRate * ReportingConstant;
		WindAC( WindACNum ).LatCoolEnergy = WindAC( WindACNum ).LatCoolEnergyRate * ReportingConstant;
		WindAC( WindACNum ).ElecConsumption = WindAC( WindACNum ).ElecPower * ReportingConstant;

	}

	void
	CalcWindowACOutput(
		int const WindACNum, // Unit index in fan coil array
		bool const FirstHVACIteration, // flag for 1st HVAV iteration in the time step
		int const OpMode, // operating mode: CycFanCycCoil | ContFanCycCoil
		Real64 const PartLoadFrac, // unit part load fraction
		bool const HXUnitOn, // Flag to toggle HX heat recovery as needed
		Real64 & LoadMet // load met by unit (watts)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   May 2000
		//       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Simulate the components making up the cycling window AC unit.

		// METHODOLOGY EMPLOYED:
		// Simulates the unit components sequentially in the air flow direction.

		// REFERENCES:
		// na

		// Using/Aliasing
		using MixedAir::SimOAMixer;
		using Fans::SimulateFanComponents;
		using DXCoils::SimDXCoil;
		using HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil;
		using InputProcessor::SameString;
		using DataHVACGlobals::ZoneCompTurnFansOn;
		using DataHVACGlobals::ZoneCompTurnFansOff;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int OutletNode; // unit air outlet node
		int InletNode; // unit air inlet node
		int OutsideAirNode; // outside air node number in window AC loop
		int AirRelNode; // relief air node number in window AC loop
		Real64 AirMassFlow; // total mass flow through the unit
		Real64 MinHumRat; // minimum of inlet & outlet humidity ratio

		// FLOW

		OutletNode = WindAC( WindACNum ).AirOutNode;
		InletNode = WindAC( WindACNum ).AirInNode;
		OutsideAirNode = WindAC( WindACNum ).OutsideAirNode;
		AirRelNode = WindAC( WindACNum ).AirReliefNode;
		// for cycling fans, pretend we have VAV
		if ( OpMode == CycFanCycCoil ) {
			Node( InletNode ).MassFlowRate = Node( InletNode ).MassFlowRateMax * PartLoadFrac;
			// Don't let the outside air flow be > supply air flow
			Node( OutsideAirNode ).MassFlowRate = min( Node( OutsideAirNode ).MassFlowRateMax, Node( InletNode ).MassFlowRate );
			Node( AirRelNode ).MassFlowRate = Node( OutsideAirNode ).MassFlowRate;
		}
		AirMassFlow = Node( InletNode ).MassFlowRate;
		SimOAMixer( WindAC( WindACNum ).OAMixName, FirstHVACIteration, WindAC( WindACNum ).OAMixIndex );

		// if blow through, simulate fan then coil. For draw through, simulate coil then fan.
		if ( WindAC( WindACNum ).FanPlace == BlowThru ) {
			SimulateFanComponents( WindAC( WindACNum ).FanName, FirstHVACIteration, WindAC( WindACNum ).FanIndex, _, ZoneCompTurnFansOn, ZoneCompTurnFansOff );
		}

		if ( WindAC( WindACNum ).DXCoilType_Num == CoilDX_CoolingHXAssisted ) {
			SimHXAssistedCoolingCoil( WindAC( WindACNum ).DXCoilName, FirstHVACIteration, On, PartLoadFrac, WindAC( WindACNum ).DXCoilIndex, WindAC( WindACNum ).OpMode, HXUnitOn );
		} else {
			SimDXCoil( WindAC( WindACNum ).DXCoilName, On, FirstHVACIteration, WindAC( WindACNum ).DXCoilIndex, WindAC( WindACNum ).OpMode, PartLoadFrac );
		}

		if ( WindAC( WindACNum ).FanPlace == DrawThru ) {
			SimulateFanComponents( WindAC( WindACNum ).FanName, FirstHVACIteration, WindAC( WindACNum ).FanIndex, _, ZoneCompTurnFansOn, ZoneCompTurnFansOff );
		}

		MinHumRat = min( Node( InletNode ).HumRat, Node( OutletNode ).HumRat );
		LoadMet = AirMassFlow * ( PsyHFnTdbW( Node( OutletNode ).Temp, MinHumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, MinHumRat ) );

	}

	void
	ControlCycWindACOutput(
		int const WindACNum, // Unit index in fan coil array
		bool const FirstHVACIteration, // flag for 1st HVAV iteration in the time step
		int const OpMode, // operating mode: CycFanCycCoil | ContFanCycCoil
		Real64 const QZnReq, // cooling output needed by zone [W]
		Real64 & PartLoadFrac, // unit part load fraction
		bool & HXUnitOn // Used to control HX heat recovery as needed
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   May 2000
		//       MODIFIED       Shirey, May 2001
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Determine the part load fraction of the air conditioner for this time step

		// METHODOLOGY EMPLOYED:
		// Linear interpolation between max and min outputs

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::TrimSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const MaxIter( 50 ); // maximum number of iterations
		Real64 const MinPLF( 0.0 ); // minimum part load factor allowed

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 FullOutput; // unit full output [W]
		Real64 NoCoolOutput; // output when no active cooling [W]
		Real64 ActualOutput; // output at current partloadfrac [W]
		Real64 Error; // error between QznReq and ActualOutput [W]
		Real64 ErrorToler; // error tolerance
		int Iter; // iteration counter
		//CHARACTER(len=20) :: ErrNum
		//INTEGER,SAVE :: ErrCount=0
		Real64 DelPLF;
		Real64 Relax;

		// DX Cooling HX assisted coils can cycle the heat exchanger, see if coil ON, HX OFF can meet humidity setpoint if one exists
		if ( WindAC( WindACNum ).DXCoilType_Num == CoilDX_CoolingHXAssisted ) {
			// Check for a setpoint at the HX outlet node, if it doesn't exist always run the HX
			if ( Node( WindAC( WindACNum ).CoilOutletNodeNum ).HumRatMax == SensedNodeFlagValue ) {
				HXUnitOn = true;
			} else {
				HXUnitOn = false;
			}
		} else {
			HXUnitOn = false;
		}

		if ( WindAC( WindACNum ).EMSOverridePartLoadFrac ) {

			PartLoadFrac = WindAC( WindACNum ).EMSValueForPartLoadFrac;
		}

		// Get result when DX coil is off
		CalcWindowACOutput( WindACNum, FirstHVACIteration, OpMode, 0.0, HXUnitOn, NoCoolOutput );

		// If NoCoolOutput < QZnReq, the coil needs to be off
		if ( NoCoolOutput < QZnReq ) {
			PartLoadFrac = 0.0;
			return;
		}

		// Get full load result
		CalcWindowACOutput( WindACNum, FirstHVACIteration, OpMode, 1.0, HXUnitOn, FullOutput );

		// Since we are cooling, we expect FullOutput to be < 0 and FullOutput < NoCoolOutput
		// Check that this is the case; if not set PartLoadFrac = 0.0 (off) and return
		if ( FullOutput >= 0.0 || FullOutput >= NoCoolOutput ) {
			PartLoadFrac = 0.0;
			return;
		}

		// If the QZnReq <= FullOutput the unit needs to run full out
		if ( QZnReq <= FullOutput && WindAC( WindACNum ).DXCoilType_Num != CoilDX_CoolingHXAssisted ) {
			PartLoadFrac = 1.0;
			return;
		}

		// If the QZnReq <= FullOutput and a HXAssisted coil is used, check the node setpoint for a maximum humidity ratio set piont
		// HumRatMax will be equal to -999 if no setpoint exists or some set point managers may still use 0 as a no moisture load indicator
		if ( QZnReq <= FullOutput && WindAC( WindACNum ).DXCoilType_Num == CoilDX_CoolingHXAssisted && Node( WindAC( WindACNum ).CoilOutletNodeNum ).HumRatMax <= 0.0 ) {
			PartLoadFrac = 1.0;
			return;
		}

		// QZnReq should now be greater than FullOutput and less than NoCoolOutput)
		// Calculate the part load fraction

		PartLoadFrac = max( MinPLF, std::abs( QZnReq - NoCoolOutput ) / std::abs( FullOutput - NoCoolOutput ) );

		ErrorToler = WindAC( WindACNum ).ConvergenceTol; //Error tolerance for convergence from input deck
		Error = 1.0; //initialize error value for comparison against tolerance
		Iter = 0; //initialize iteration counter
		Relax = 1.0;

		while ( ( std::abs( Error ) > ErrorToler ) && ( Iter <= MaxIter ) && PartLoadFrac > MinPLF ) {
			// Get result when DX coil is operating at partloadfrac
			CalcWindowACOutput( WindACNum, FirstHVACIteration, OpMode, PartLoadFrac, HXUnitOn, ActualOutput );
			Error = ( QZnReq - ActualOutput ) / QZnReq;
			DelPLF = ( QZnReq - ActualOutput ) / FullOutput;
			PartLoadFrac += Relax * DelPLF;
			PartLoadFrac = max( MinPLF, min( 1.0, PartLoadFrac ) );
			++Iter;
			if ( Iter == 16 ) {
				Relax = 0.5;
			}
		}
		if ( Iter > MaxIter ) {
			if ( WindAC( WindACNum ).MaxIterIndex1 == 0 ) {
				ShowWarningMessage( "ZoneHVAC:WindowAirConditioner=\"" + WindAC( WindACNum ).Name + "\" -- Exceeded max iterations while adjusting compressor sensible runtime to meet the zone load within the cooling convergence tolerance." );
				ShowContinueErrorTimeStamp( "Iterations=" + TrimSigDigits( MaxIter ) );
			}
			ShowRecurringWarningErrorAtEnd( "ZoneHVAC:WindowAirConditioner=\"" + WindAC( WindACNum ).Name + "\"  -- Exceeded max iterations error (sensible runtime) continues...", WindAC( WindACNum ).MaxIterIndex1 );
		}

		// HX is off up until this point where the outlet air humidity ratio is tested to see if HX needs to be turned on
		if ( WindAC( WindACNum ).DXCoilType_Num == CoilDX_CoolingHXAssisted && Node( WindAC( WindACNum ).CoilOutletNodeNum ).HumRatMax < Node( WindAC( WindACNum ).CoilOutletNodeNum ).HumRat && Node( WindAC( WindACNum ).CoilOutletNodeNum ).HumRatMax > 0.0 ) {

			//   Run the HX to recovery energy and improve latent performance
			HXUnitOn = true;

			//   Get full load result
			CalcWindowACOutput( WindACNum, FirstHVACIteration, OpMode, 1.0, HXUnitOn, FullOutput );

			if ( Node( WindAC( WindACNum ).CoilOutletNodeNum ).HumRatMax < Node( WindAC( WindACNum ).CoilOutletNodeNum ).HumRat || QZnReq <= FullOutput ) {
				PartLoadFrac = 1.0;
				return;
			}

			Error = 1.0; //initialize error value for comparison against tolerance
			Iter = 0; //initialize iteration counter
			Relax = 1.0;

			while ( ( std::abs( Error ) > ErrorToler ) && ( Iter <= MaxIter ) && PartLoadFrac > MinPLF ) {
				// Get result when DX coil is operating at partloadfrac
				CalcWindowACOutput( WindACNum, FirstHVACIteration, OpMode, PartLoadFrac, HXUnitOn, ActualOutput );
				Error = ( QZnReq - ActualOutput ) / QZnReq;
				DelPLF = ( QZnReq - ActualOutput ) / FullOutput;
				PartLoadFrac += Relax * DelPLF;
				PartLoadFrac = max( MinPLF, min( 1.0, PartLoadFrac ) );
				++Iter;
				if ( Iter == 16 ) {
					Relax = 0.5;
				}
			}
			if ( Iter > MaxIter ) {
				if ( WindAC( WindACNum ).MaxIterIndex2 == 0 ) {
					ShowWarningMessage( "ZoneHVAC:WindowAirConditioner=\"" + WindAC( WindACNum ).Name + "\" -- Exceeded max iterations while adjusting compressor latent runtime to meet the zone load within the cooling convergence tolerance." );
					ShowContinueErrorTimeStamp( "Iterations=" + TrimSigDigits( MaxIter ) );
				}
				ShowRecurringWarningErrorAtEnd( "ZoneHVAC:WindowAirConditioner=\"" + WindAC( WindACNum ).Name + "\"  -- Exceeded max iterations error (latent runtime) continues...", WindAC( WindACNum ).MaxIterIndex2 );
			}

		} // WindAC(WindACNum)%DXCoilType_Num == CoilDX_CoolingHXAssisted && *

	}

	int
	GetWindowACZoneInletAirNode( int const WindACNum )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         B Griffith
		//       DATE WRITTEN   Dec  2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// lookup function for zone inlet node

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		int GetWindowACZoneInletAirNode;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na
		if ( GetWindowACInputFlag ) {
			GetWindowAC();
			GetWindowACInputFlag = false;
		}

		GetWindowACZoneInletAirNode = WindAC( WindACNum ).AirOutNode;

		return GetWindowACZoneInletAirNode;
	}

	int
	GetWindowACOutAirNode( int const WindACNum )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         B Griffith
		//       DATE WRITTEN   Dec  2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// lookup function for OA inlet node

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		int GetWindowACOutAirNode;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na
		if ( GetWindowACInputFlag ) {
			GetWindowAC();
			GetWindowACInputFlag = false;
		}

		GetWindowACOutAirNode = WindAC( WindACNum ).OutsideAirNode;

		return GetWindowACOutAirNode;

	}

	int
	GetWindowACReturnAirNode( int const WindACNum )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         B Griffith
		//       DATE WRITTEN   Dec  2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// lookup function for mixer return air node for ventilation load reporting

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using MixedAir::GetOAMixerReturnNodeNumber;

		// Return value
		int GetWindowACReturnAirNode;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na
		if ( GetWindowACInputFlag ) {
			GetWindowAC();
			GetWindowACInputFlag = false;
		}

		if ( WindACNum > 0 && WindACNum <= NumWindAC ) {
			if ( WindAC( WindACNum ).OAMixIndex > 0 ) {
				GetWindowACReturnAirNode = GetOAMixerReturnNodeNumber( WindAC( WindACNum ).OAMixIndex );
			} else {
				GetWindowACReturnAirNode = 0;
			}
		} else {
			GetWindowACReturnAirNode = 0;
		}

		return GetWindowACReturnAirNode;

	}

	int
	GetWindowACMixedAirNode( int const WindACNum )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         B Griffith
		//       DATE WRITTEN   Dec  2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// lookup function for mixed air node for ventilation rate reporting

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using MixedAir::GetOAMixerMixedNodeNumber;

		// Return value
		int GetWindowACMixedAirNode;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na
		if ( GetWindowACInputFlag ) {
			GetWindowAC();
			GetWindowACInputFlag = false;
		}

		if ( WindACNum > 0 && WindACNum <= NumWindAC ) {
			if ( WindAC( WindACNum ).OAMixIndex > 0 ) {
				GetWindowACMixedAirNode = GetOAMixerMixedNodeNumber( WindAC( WindACNum ).OAMixIndex );
			} else {
				GetWindowACMixedAirNode = 0;
			}
		} else {
			GetWindowACMixedAirNode = 0;
		}

		return GetWindowACMixedAirNode;

	}

} // WindowAC

} // EnergyPlus
