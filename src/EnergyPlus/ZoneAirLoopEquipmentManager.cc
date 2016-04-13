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

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <ZoneAirLoopEquipmentManager.hh>
#include <BranchNodeConnections.hh>
#include <DataAirLoop.hh>
#include <DataDefineEquip.hh>
#include <DataGlobals.hh>
#include <DataHVACGlobals.hh>
#include <DataLoopNode.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DataZoneEquipment.hh>
#include <DualDuct.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <HVACCooledBeam.hh>
#include <HVACSingleDuctInduc.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <PoweredInductionUnits.hh>
#include <Psychrometrics.hh>
#include <SingleDuct.hh>
#include <UserDefinedComponents.hh>
#include <UtilityRoutines.hh>
#include <AirTerminalUnit.hh>
#include <HVACFourPipeBeam.hh>

namespace EnergyPlus {

namespace ZoneAirLoopEquipmentManager {
	// Module containing the routines dealing with the ZoneAirLoopEquipmentManager

	// MODULE INFORMATION:
	//       AUTHOR         Russ Taylor
	//       DATE WRITTEN   May 1997
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// Needs description

	// METHODOLOGY EMPLOYED: none

	// REFERENCES: none

	// OTHER NOTES: none

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::NumOfZones;
	using DataGlobals::BeginEnvrnFlag;
	using DataGlobals::BeginDayFlag;
	using DataGlobals::BeginHourFlag;
	using DataGlobals::BeginTimeStepFlag;
	using DataGlobals::SecInHour;
	using DataHVACGlobals::FirstTimeStepSysFlag;
	using namespace DataDefineEquip;

	// Use statements for access to subroutines in other modules

	// Data
	// MODULE PARAMETER DEFINITIONS:
	bool MyOneTimeFlag( true );

	namespace {

		Array1D_bool EachOnceFlag;

	}

	// DERIVED TYPE DEFINITIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:
	// na

	namespace {
		// These were static variables within different functions. They were pulled out into the namespace
		// to facilitate easier unit testing of those functions.
		// These are purposefully not in the header file as an extern variable. No one outside of this should
		// use these. They are cleared by clear_state() for use by unit tests, but normal simulations should be unaffected.
		// This is purposefully in an anonymous namespace so nothing outside this implementation file can use it.
		bool GetAirDistUnitsFlag(true); // If TRUE, Air Distribution Data has not been read in yet
		bool InitAirDistUnitsFlag( true ); // If TRUE, Air Distribution unit actualtzonenums have not been initialized yet
	}

	// SUBROUTINE SPECIFICATIONS FOR MODULE ZoneAirLoopEquipmentManager

	// Functions
	void
	clear_state()
	{

		GetAirDistUnitsFlag = true;
		EachOnceFlag = true;
		MyOneTimeFlag = true;

		InitAirDistUnitsFlag = true;
	}

	void
	ManageZoneAirLoopEquipment(
		std::string const & ZoneAirLoopEquipName,
		bool const FirstHVACIteration,
		Real64 & SysOutputProvided,
		Real64 & NonAirSysOutput,
		Real64 & LatOutputProvided, // Latent add/removal supplied by window AC (kg/s), dehumid = negative
		int const ActualZoneNum,
		int & ControlledZoneNum,
		int & CompIndex
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Russ Taylor
		//       DATE WRITTEN   May 1997
		//       MODIFIED       Don Shirey, Aug 2009 (LatOutputProvided)
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calls the zone thermal control simulations and the interfaces
		// (water-air, refrigerant-air, steam-air, electric-electric,
		// water-water, etc)

		// METHODOLOGY EMPLOYED:
		// Needs description, as appropriate.

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::TrimSigDigits;
		using InputProcessor::FindItemInList;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		bool SimZone;
		int AirDistUnitNum;

		// Beginning of Code

		GetZoneAirLoopEquipment();

		// Find the correct Zone Air Distribution Unit Equipment
		if ( CompIndex == 0 ) {
			AirDistUnitNum = FindItemInList( ZoneAirLoopEquipName, AirDistUnit );
			if ( AirDistUnitNum == 0 ) {
				ShowFatalError( "ManageZoneAirLoopEquipment: Unit not found=" + ZoneAirLoopEquipName );
			}
			CompIndex = AirDistUnitNum;
		} else {
			AirDistUnitNum = CompIndex;
			if ( AirDistUnitNum > NumAirDistUnits || AirDistUnitNum < 1 ) {
				ShowFatalError( "ManageZoneAirLoopEquipment:  Invalid CompIndex passed=" + TrimSigDigits( AirDistUnitNum ) + ", Number of Units=" + TrimSigDigits( NumAirDistUnits ) + ", Entered Unit name=" + ZoneAirLoopEquipName );
			}
			if ( ZoneAirLoopEquipName != AirDistUnit( AirDistUnitNum ).Name ) {
				ShowFatalError( "ManageZoneAirLoopEquipment: Invalid CompIndex passed=" + TrimSigDigits( AirDistUnitNum ) + ", Unit name=" + ZoneAirLoopEquipName + ", stored Unit Name for that index=" + AirDistUnit( AirDistUnitNum ).Name );
			}
		}

		InitZoneAirLoopEquipment( FirstHVACIteration, AirDistUnitNum, ActualZoneNum );

		SimZoneAirLoopEquipment( AirDistUnitNum, SysOutputProvided, NonAirSysOutput, LatOutputProvided, FirstHVACIteration, ControlledZoneNum, ActualZoneNum );

		//  CALL RecordZoneAirLoopEquipment

		// ReportZoneAirLoopEquipment( AirDistUnitNum );

		SimZone = false;

	}

	void
	GetZoneAirLoopEquipment()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Russ Taylor
		//       DATE WRITTEN   June 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Get all the system related equipment which may be attached to
		// a zone

		// METHODOLOGY EMPLOYED:
		// Needs description, as appropriate.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::GetObjectItemNum;
		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using NodeInputManager::GetOnlySingleNode;
		using namespace DataLoopNode;
		using BranchNodeConnections::SetUpCompSets;
		using DataZoneEquipment::ZoneEquipConfig;
		using DualDuct::GetDualDuctOutdoorAirRecircUse;
		using SingleDuct::GetATMixerPriNode;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetZoneAirLoopEquipment: " ); // include trailing blank space
		static std::string const CurrentModuleObject( "ZoneHVAC:AirDistributionUnit" ); // Object type for getting and error messages

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int AirDistUnitNum;
		int AirDistCompUnitNum;
		int ZoneEqNum; // zone equip config index
		int InletNum; // zone equip config inlet node index
		int NumAlphas;
		int NumNums;
		int IOStat;
		static Array1D_string AlphArray( 4 ); //Tuned Made static
		static Array1D< Real64 > NumArray( 2 ); //Tuned Made static
		static bool ErrorsFound( false ); // If errors detected in input
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		static Array1D_string cAlphaFields( 4 ); // Alpha field names //Tuned Made static
		static Array1D_string cNumericFields( 2 ); // Numeric field names //Tuned Made static
		static Array1D_bool lAlphaBlanks( 4 ); // Logical array, alpha field input BLANK = .TRUE. //Tuned Made static
		static Array1D_bool lNumericBlanks( 2 ); // Logical array, numeric field input BLANK = .TRUE. //Tuned Made static
		bool DualDuctRecircIsUsed; // local temporary for deciding if recirc side used by dual duct terminal
		static int ATMixerPriNode( 0 ); // primary air inlet node for air terminal mixers

		// make sure the input data is read in only once
		if ( ! GetAirDistUnitsFlag ) {
			return;
		} else {
			GetAirDistUnitsFlag = false;
		}

		NumAirDistUnits = GetNumObjectsFound( CurrentModuleObject );

		AirDistUnit.allocate( NumAirDistUnits );

		if ( NumAirDistUnits > 0 ) {

			for ( AirDistUnitNum = 1; AirDistUnitNum <= NumAirDistUnits; ++AirDistUnitNum ) {
				GetObjectItem( CurrentModuleObject, AirDistUnitNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields ); //  data for one zone

				IsNotOK = false;
				IsBlank = false;
				VerifyName( AlphArray( 1 ), AirDistUnit, AirDistUnitNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
				}
				AirDistUnit( AirDistUnitNum ).Name = AlphArray( 1 );
				//Input Outlet Node Num
				AirDistUnit( AirDistUnitNum ).OutletNodeNum = GetOnlySingleNode( AlphArray( 2 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsParent );
				AirDistUnit( AirDistUnitNum ).InletNodeNum = 0;
				AirDistUnit( AirDistUnitNum ).NumComponents = 1;
				AirDistCompUnitNum = 1;
				//Load the air Distribution Unit Equip and Name
				AirDistUnit( AirDistUnitNum ).EquipType( AirDistCompUnitNum ) = AlphArray( 3 );
				AirDistUnit( AirDistUnitNum ).EquipName( AirDistCompUnitNum ) = AlphArray( 4 );
				ValidateComponent( AlphArray( 3 ), AlphArray( 4 ), IsNotOK, CurrentModuleObject );
				if ( IsNotOK ) {
					ShowContinueError( "In " + CurrentModuleObject + " = " + AlphArray( 1 ) );
					ErrorsFound = true;
				}
				AirDistUnit( AirDistUnitNum ).UpStreamLeakFrac = NumArray( 1 );
				AirDistUnit( AirDistUnitNum ).DownStreamLeakFrac = NumArray( 2 );
				if ( AirDistUnit( AirDistUnitNum ).DownStreamLeakFrac <= 0.0 ) {
					AirDistUnit( AirDistUnitNum ).LeakLoadMult = 1.0;
				}
				else if ( AirDistUnit( AirDistUnitNum ).DownStreamLeakFrac < 1.0 && AirDistUnit( AirDistUnitNum ).DownStreamLeakFrac > 0.0 ) {
					AirDistUnit( AirDistUnitNum ).LeakLoadMult = 1.0 / ( 1.0 - AirDistUnit( AirDistUnitNum ).DownStreamLeakFrac );
				}
				else {
					ShowSevereError( "Error found in " + CurrentModuleObject + " = " + AirDistUnit( AirDistUnitNum ).Name );
					ShowContinueError( cNumericFields( 2 ) + " must be less than 1.0" );
					ErrorsFound = true;
				}
				if ( AirDistUnit( AirDistUnitNum ).UpStreamLeakFrac > 0.0 ) {
					AirDistUnit( AirDistUnitNum ).UpStreamLeak = true;
				}
				else {
					AirDistUnit( AirDistUnitNum ).UpStreamLeak = false;
				}
				if ( AirDistUnit( AirDistUnitNum ).DownStreamLeakFrac > 0.0 ) {
					AirDistUnit( AirDistUnitNum ).DownStreamLeak = true;
				}
				else {
					AirDistUnit( AirDistUnitNum ).DownStreamLeak = false;
				}

				// Validate EquipType for Air Distribution Unit
				if ( SameString( AirDistUnit( AirDistUnitNum ).EquipType( AirDistCompUnitNum ), "AirTerminal:DualDuct:ConstantVolume" ) ) {
					AirDistUnit( AirDistUnitNum ).EquipType_Num( AirDistCompUnitNum ) = DualDuctConstVolume;
					if ( AirDistUnit( AirDistUnitNum ).UpStreamLeak || AirDistUnit( AirDistUnitNum ).DownStreamLeak ) {
						ShowSevereError( "Error found in " + CurrentModuleObject + " = " + AirDistUnit( AirDistUnitNum ).Name );
						ShowContinueError( "Simple duct leakage model not available for " + cAlphaFields( 3 ) + " = " + AirDistUnit( AirDistUnitNum ).EquipType( AirDistCompUnitNum ) );
						ErrorsFound = true;
					}
				}
				else if ( SameString( AirDistUnit( AirDistUnitNum ).EquipType( AirDistCompUnitNum ), "AirTerminal:DualDuct:VAV" ) ) {
					AirDistUnit( AirDistUnitNum ).EquipType_Num( AirDistCompUnitNum ) = DualDuctVAV;
					if ( AirDistUnit( AirDistUnitNum ).UpStreamLeak || AirDistUnit( AirDistUnitNum ).DownStreamLeak ) {
						ShowSevereError( "Error found in " + CurrentModuleObject + " = " + AirDistUnit( AirDistUnitNum ).Name );
						ShowContinueError( "Simple duct leakage model not available for " + cAlphaFields( 3 ) + " = " + AirDistUnit( AirDistUnitNum ).EquipType( AirDistCompUnitNum ) );
						ErrorsFound = true;
					}
				}
				else if ( SameString( AirDistUnit( AirDistUnitNum ).EquipType( AirDistCompUnitNum ), "AirTerminal:DualDuct:VAV:OutdoorAir" ) ) {
					AirDistUnit( AirDistUnitNum ).EquipType_Num( AirDistCompUnitNum ) = DualDuctVAVOutdoorAir;
					if ( AirDistUnit( AirDistUnitNum ).UpStreamLeak || AirDistUnit( AirDistUnitNum ).DownStreamLeak ) {
						ShowSevereError( "Error found in " + CurrentModuleObject + " = " + AirDistUnit( AirDistUnitNum ).Name );
						ShowContinueError( "Simple duct leakage model not available for " + cAlphaFields( 3 ) + " = " + AirDistUnit( AirDistUnitNum ).EquipType( AirDistCompUnitNum ) );
						ErrorsFound = true;
					}
				}
				else if ( SameString( AirDistUnit( AirDistUnitNum ).EquipType( AirDistCompUnitNum ), "AirTerminal:SingleDuct:ConstantVolume:Reheat" ) ) {
					AirDistUnit( AirDistUnitNum ).EquipType_Num( AirDistCompUnitNum ) = SingleDuctConstVolReheat;
				}
				else if ( SameString( AirDistUnit( AirDistUnitNum ).EquipType( AirDistCompUnitNum ), "AirTerminal:SingleDuct:VAV:Reheat" ) ) {
					AirDistUnit( AirDistUnitNum ).EquipType_Num( AirDistCompUnitNum ) = SingleDuctVAVReheat;
				}
				else if ( SameString( AirDistUnit( AirDistUnitNum ).EquipType( AirDistCompUnitNum ), "AirTerminal:SingleDuct:VAV:NoReheat" ) ) {
					AirDistUnit( AirDistUnitNum ).EquipType_Num( AirDistCompUnitNum ) = SingleDuctVAVNoReheat;
				}
				else if ( SameString( AirDistUnit( AirDistUnitNum ).EquipType( AirDistCompUnitNum ), "AirTerminal:SingleDuct:VAV:HeatAndCool:Reheat" ) ) {
					AirDistUnit( AirDistUnitNum ).EquipType_Num( AirDistCompUnitNum ) = SingleDuctCBVAVReheat;
				}
				else if ( SameString( AirDistUnit( AirDistUnitNum ).EquipType( AirDistCompUnitNum ), "AirTerminal:SingleDuct:VAV:HeatAndCool:NoReheat" ) ) {
					AirDistUnit( AirDistUnitNum ).EquipType_Num( AirDistCompUnitNum ) = SingleDuctCBVAVNoReheat;
				}
				else if ( SameString( AirDistUnit( AirDistUnitNum ).EquipType( AirDistCompUnitNum ), "AirTerminal:SingleDuct:SeriesPIU:Reheat" ) ) {
					AirDistUnit( AirDistUnitNum ).EquipType_Num( AirDistCompUnitNum ) = SingleDuct_SeriesPIU_Reheat;
					if ( AirDistUnit( AirDistUnitNum ).UpStreamLeak || AirDistUnit( AirDistUnitNum ).DownStreamLeak ) {
						ShowSevereError( "Error found in " + CurrentModuleObject + " = " + AirDistUnit( AirDistUnitNum ).Name );
						ShowContinueError( "Simple duct leakage model not available for " + cAlphaFields( 3 ) + " = " + AirDistUnit( AirDistUnitNum ).EquipType( AirDistCompUnitNum ) );
						ErrorsFound = true;
					}
				}
				else if ( SameString( AirDistUnit( AirDistUnitNum ).EquipType( AirDistCompUnitNum ), "AirTerminal:SingleDuct:ParallelPIU:Reheat" ) ) {
					AirDistUnit( AirDistUnitNum ).EquipType_Num( AirDistCompUnitNum ) = SingleDuct_ParallelPIU_Reheat;
					if ( AirDistUnit( AirDistUnitNum ).UpStreamLeak || AirDistUnit( AirDistUnitNum ).DownStreamLeak ) {
						ShowSevereError( "Error found in " + CurrentModuleObject + " = " + AirDistUnit( AirDistUnitNum ).Name );
						ShowContinueError( "Simple duct leakage model not available for " + cAlphaFields( 3 ) + " = " + AirDistUnit( AirDistUnitNum ).EquipType( AirDistCompUnitNum ) );
						ErrorsFound = true;
					}
				}
				else if ( SameString( AirDistUnit( AirDistUnitNum ).EquipType( AirDistCompUnitNum ), "AirTerminal:SingleDuct:ConstantVolume:FourPipeInduction" ) ) {
					AirDistUnit( AirDistUnitNum ).EquipType_Num( AirDistCompUnitNum ) = SingleDuct_ConstVol_4PipeInduc;
					if ( AirDistUnit( AirDistUnitNum ).UpStreamLeak || AirDistUnit( AirDistUnitNum ).DownStreamLeak ) {
						ShowSevereError( "Error found in " + CurrentModuleObject + " = " + AirDistUnit( AirDistUnitNum ).Name );
						ShowContinueError( "Simple duct leakage model not available for " + cAlphaFields( 3 ) + " = " + AirDistUnit( AirDistUnitNum ).EquipType( AirDistCompUnitNum ) );
						ErrorsFound = true;
					}
				}
				else if ( SameString( AirDistUnit( AirDistUnitNum ).EquipType( AirDistCompUnitNum ), "AirTerminal:SingleDuct:VAV:Reheat:VariableSpeedFan" ) ) {
					AirDistUnit( AirDistUnitNum ).EquipType_Num( AirDistCompUnitNum ) = SingleDuctVAVReheatVSFan;
					if ( AirDistUnit( AirDistUnitNum ).UpStreamLeak || AirDistUnit( AirDistUnitNum ).DownStreamLeak ) {
						ShowSevereError( "Error found in " + CurrentModuleObject + " = " + AirDistUnit( AirDistUnitNum ).Name );
						ShowContinueError( "Simple duct leakage model not available for " + cAlphaFields( 3 ) + " = " + AirDistUnit( AirDistUnitNum ).EquipType( AirDistCompUnitNum ) );
						ErrorsFound = true;
					}
				}
				else if ( SameString( AirDistUnit( AirDistUnitNum ).EquipType( AirDistCompUnitNum ), "AirTerminal:SingleDuct:ConstantVolume:CooledBeam" ) ) {
					AirDistUnit( AirDistUnitNum ).EquipType_Num( AirDistCompUnitNum ) = SingleDuctConstVolCooledBeam;
					if ( AirDistUnit( AirDistUnitNum ).UpStreamLeak || AirDistUnit( AirDistUnitNum ).DownStreamLeak ) {
						ShowSevereError( "Error found in " + CurrentModuleObject + " = " + AirDistUnit( AirDistUnitNum ).Name );
						ShowContinueError( "Simple duct leakage model not available for " + cAlphaFields( 3 ) + " = " + AirDistUnit( AirDistUnitNum ).EquipType( AirDistCompUnitNum ) );
						ErrorsFound = true;
					}
				} else if ( SameString( AirDistUnit( AirDistUnitNum ).EquipType( AirDistCompUnitNum ), "AirTerminal:SingleDuct:ConstantVolume:FourPipeBeam" ) ) {
					AirDistUnit( AirDistUnitNum ).EquipType_Num( AirDistCompUnitNum ) = SingleDuctConstVolFourPipeBeam;
					AirDistUnit( AirDistUnitNum ).airTerminalPtr = FourPipeBeam::HVACFourPipeBeam::fourPipeBeamFactory( SingleDuctConstVolFourPipeBeam, AirDistUnit( AirDistUnitNum ).EquipName( 1 ) );
					if ( AirDistUnit( AirDistUnitNum ).UpStreamLeak || AirDistUnit( AirDistUnitNum ).DownStreamLeak ) {
						ShowSevereError( "Error found in " + CurrentModuleObject + " = " + AirDistUnit( AirDistUnitNum ).Name );
						ShowContinueError( "Simple duct leakage model not available for " + cAlphaFields( 3 ) + " = " + AirDistUnit( AirDistUnitNum ).EquipType( AirDistCompUnitNum ) );
						ErrorsFound = true;
					}
				} else if ( SameString( AirDistUnit( AirDistUnitNum ).EquipType( AirDistCompUnitNum ), "AirTerminal:SingleDuct:UserDefined" ) ) {
					AirDistUnit( AirDistUnitNum ).EquipType_Num( AirDistCompUnitNum ) = SingleDuctUserDefined;
				}
				else if ( SameString( AirDistUnit( AirDistUnitNum ).EquipType( AirDistCompUnitNum ), "AirTerminal:SingleDuct:InletSideMixer" ) ) {
					AirDistUnit( AirDistUnitNum ).EquipType_Num( AirDistCompUnitNum ) = SingleDuctInletATMixer;
					GetATMixerPriNode( AirDistUnit( AirDistUnitNum ).EquipName( 1 ), ATMixerPriNode );
					AirDistUnit( AirDistUnitNum ).InletNodeNum = ATMixerPriNode;
					if ( AirDistUnit( AirDistUnitNum ).UpStreamLeak || AirDistUnit( AirDistUnitNum ).DownStreamLeak ) {
						ShowSevereError( "Error found in " + CurrentModuleObject + " = " + AirDistUnit( AirDistUnitNum ).Name );
						ShowContinueError( "Simple duct leakage model not available for " + cAlphaFields( 3 ) + " = " + AirDistUnit( AirDistUnitNum ).EquipType( AirDistCompUnitNum ) );
						ErrorsFound = true;
					}
				}
				else if ( SameString( AirDistUnit( AirDistUnitNum ).EquipType( AirDistCompUnitNum ), "AirTerminal:SingleDuct:SupplySideMixer" ) ) {
					AirDistUnit( AirDistUnitNum ).EquipType_Num( AirDistCompUnitNum ) = SingleDuctSupplyATMixer;
					GetATMixerPriNode( AirDistUnit( AirDistUnitNum ).EquipName( 1 ), ATMixerPriNode );
					AirDistUnit( AirDistUnitNum ).InletNodeNum = ATMixerPriNode;
					if ( AirDistUnit( AirDistUnitNum ).UpStreamLeak || AirDistUnit( AirDistUnitNum ).DownStreamLeak ) {
						ShowSevereError( "Error found in " + CurrentModuleObject + " = " + AirDistUnit( AirDistUnitNum ).Name );
						ShowContinueError( "Simple duct leakage model not available for " + cAlphaFields( 3 ) + " = " + AirDistUnit( AirDistUnitNum ).EquipType( AirDistCompUnitNum ) );
						ErrorsFound = true;
					}
				}
				else {
					ShowSevereError( "Error found in " + CurrentModuleObject + " = " + AirDistUnit( AirDistUnitNum ).Name );
					ShowContinueError( "Invalid " + cAlphaFields( 3 ) + " = " + AirDistUnit( AirDistUnitNum ).EquipType( AirDistCompUnitNum ) );
					ErrorsFound = true;
				}

				// Set up component set for air terminal unit
				if ( ( AirDistUnit( AirDistUnitNum ).EquipType_Num( AirDistCompUnitNum ) == DualDuctConstVolume ) || ( AirDistUnit( AirDistUnitNum ).EquipType_Num( AirDistCompUnitNum ) == DualDuctVAV ) ) {
					//  For dual duct units, set up two component sets, one for heat and one for cool
					SetUpCompSets( CurrentModuleObject, AirDistUnit( AirDistUnitNum ).Name, AirDistUnit( AirDistUnitNum ).EquipType( AirDistCompUnitNum ) + ":HEAT", AirDistUnit( AirDistUnitNum ).EquipName( AirDistCompUnitNum ), "UNDEFINED", AlphArray( 2 ) );
					SetUpCompSets( CurrentModuleObject, AirDistUnit( AirDistUnitNum ).Name, AirDistUnit( AirDistUnitNum ).EquipType( AirDistCompUnitNum ) + ":COOL", AirDistUnit( AirDistUnitNum ).EquipName( AirDistCompUnitNum ), "UNDEFINED", AlphArray( 2 ) );
					//  For dual duct units with decoupled OA and RA, set up two component sets, one for OA (Outdoor Air)
					//  and one for RA (Recirculated Air)
				}
				else if ( AirDistUnit( AirDistUnitNum ).EquipType_Num( AirDistCompUnitNum ) == DualDuctVAVOutdoorAir ) {
					SetUpCompSets( CurrentModuleObject, AirDistUnit( AirDistUnitNum ).Name, AirDistUnit( AirDistUnitNum ).EquipType( AirDistCompUnitNum ) + ":OutdoorAir", AirDistUnit( AirDistUnitNum ).EquipName( AirDistCompUnitNum ), "UNDEFINED", AlphArray( 2 ) );
					GetDualDuctOutdoorAirRecircUse( AirDistUnit( AirDistUnitNum ).EquipType( AirDistCompUnitNum ), AirDistUnit( AirDistUnitNum ).EquipName( AirDistCompUnitNum ), DualDuctRecircIsUsed );
					if ( DualDuctRecircIsUsed ) {
						SetUpCompSets( CurrentModuleObject, AirDistUnit( AirDistUnitNum ).Name, AirDistUnit( AirDistUnitNum ).EquipType( AirDistCompUnitNum ) + ":RecirculatedAir", AirDistUnit( AirDistUnitNum ).EquipName( AirDistCompUnitNum ), "UNDEFINED", AlphArray( 2 ) );
					}
				}
				else {
					SetUpCompSets( CurrentModuleObject, AirDistUnit( AirDistUnitNum ).Name, AirDistUnit( AirDistUnitNum ).EquipType( AirDistCompUnitNum ), AirDistUnit( AirDistUnitNum ).EquipName( AirDistCompUnitNum ), "UNDEFINED", AlphArray( 2 ) );
				}

				// find and save corresponding zone equip config
				for ( ZoneEqNum = 1; ZoneEqNum <= NumOfZones; ++ZoneEqNum ) {
					if ( !ZoneEquipConfig( ZoneEqNum ).IsControlled ) continue;
					for ( InletNum = 1; InletNum <= ZoneEquipConfig( ZoneEqNum ).NumInletNodes; ++InletNum ) {
						if ( ZoneEquipConfig( ZoneEqNum ).InletNode( InletNum ) == AirDistUnit( AirDistUnitNum ).OutletNodeNum ) {
							AirDistUnit( AirDistUnitNum ).ZoneEqNum = ZoneEqNum;
							ZoneEquipConfig( ZoneEqNum ).ADUNum = AirDistUnitNum;
						}
					}
				}

				if ( AirDistUnit( AirDistUnitNum ).UpStreamLeak || AirDistUnit( AirDistUnitNum ).DownStreamLeak ) {
					ZoneEquipConfig( AirDistUnit( AirDistUnitNum ).ZoneEqNum ).SupLeakToRetPlen = true;
				}

			} //End of Air Dist Do Loop
			for ( AirDistUnitNum = 1; AirDistUnitNum <= NumAirDistUnits; ++AirDistUnitNum ) {
				SetupOutputVariable( "Zone Air Terminal Sensible Heating Energy [J]", AirDistUnit( AirDistUnitNum ).HeatGain, "System", "Sum", AirDistUnit( AirDistUnitNum ).Name );
				SetupOutputVariable( "Zone Air Terminal Sensible Cooling Energy [J]", AirDistUnit( AirDistUnitNum ).CoolGain, "System", "Sum", AirDistUnit( AirDistUnitNum ).Name );
				SetupOutputVariable( "Zone Air Terminal Sensible Heating Rate [W]", AirDistUnit( AirDistUnitNum ).HeatRate, "System", "Sum", AirDistUnit( AirDistUnitNum ).Name );
				SetupOutputVariable( "Zone Air Terminal Sensible Cooling Rate [W]", AirDistUnit( AirDistUnitNum ).CoolRate, "System", "Sum", AirDistUnit( AirDistUnitNum ).Name );
			}
		}
		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in getting " + CurrentModuleObject + " Input" );
		}

	}

	void
	InitZoneAirLoopEquipment(
		bool const EP_UNUSED( FirstHVACIteration ), // unused1208
		int const AirDistUnitNum,
		int const ZoneNum
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Russ Taylor
		//       DATE WRITTEN   Nov 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is left for Module format consistency -- not needed in this module.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		// using DataSizing::FinalZoneSizing;

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

		// Do the Begin Simulation initializations
		if ( InitAirDistUnitsFlag ) {
			EachOnceFlag.allocate( NumAirDistUnits );
			EachOnceFlag = true;
			InitAirDistUnitsFlag = false;
		}
		if ( EachOnceFlag( AirDistUnitNum )) {
			AirDistUnit( AirDistUnitNum ).ZoneNum = ZoneNum;
			// ZoneEqNum = AirDistUnit( AirDistUnitNum ).ZoneEqNum;
			// if ( allocated( FinalZoneSizing ) ) {
				// AirDistUnit( AirDistUnitNum ).AccountForDOAS = FinalZoneSizing( ZoneEqNum ).AccountForDOAS;
			// }
			EachOnceFlag( AirDistUnitNum ) = false;
		}

		// every time step
		AirDistUnit( AirDistUnitNum ).MassFlowRateDnStrLk = 0.0;
		AirDistUnit( AirDistUnitNum ).MassFlowRateTU = 0.0;
		AirDistUnit( AirDistUnitNum ).MassFlowRateZSup = 0.0;
		AirDistUnit( AirDistUnitNum ).MassFlowRateSup = 0.0;
		AirDistUnit( AirDistUnitNum ).HeatRate = 0.0;
		AirDistUnit( AirDistUnitNum ).CoolRate = 0.0;
		AirDistUnit( AirDistUnitNum ).HeatGain = 0.0;
		AirDistUnit( AirDistUnitNum ).CoolGain = 0.0;
	}

	void
	SimZoneAirLoopEquipment(
		int const AirDistUnitNum,
		Real64 & SysOutputProvided,
		Real64 & NonAirSysOutput,
		Real64 & LatOutputProvided, // Latent add/removal provided by this unit (kg/s), dehumidify = negative
		bool const FirstHVACIteration,
		int const ControlledZoneNum,
		int const ActualZoneNum
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Russ Taylor
		//       DATE WRITTEN   May 1997
		//       MODIFIED       Don Shirey, Aug 2009 (LatOutputProvided)
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Simulates primary system air supplied to a zone and calculates
		// airflow requirements

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataZoneEquipment::ZoneEquipConfig;
		using DataLoopNode::Node;
		using DataAirLoop::AirLoopFlow;
		using DualDuct::SimulateDualDuct;
		using SingleDuct::SimulateSingleDuct;
		using PoweredInductionUnits::SimPIU;
		using Psychrometrics::PsyCpAirFnWTdb;
		using HVACSingleDuctInduc::SimIndUnit;
		using HVACCooledBeam::SimCoolBeam;
		using UserDefinedComponents::SimAirTerminalUserDefined;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		bool ProvideSysOutput;

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int AirDistCompNum;
		int InNodeNum; // air distribution unit inlet node
		int OutNodeNum; // air distribution unit outlet node
		static int AirLoopNum( 0 ); // index of air loop
		Real64 CpAirZn;
		Real64 CpAirSys;
		Real64 MassFlowRateMaxAvail; // max avail mass flow rate excluding leaks [kg/s]
		Real64 MassFlowRateMinAvail; // min avail mass flow rate excluding leaks [kg/s]
		Real64 MassFlowRateUpStreamLeakMax; // max upstream leak flow rate [kg/s]
		static Real64 DesFlowRatio( 0.0 ); // ratio of system to sum of zones design flow rate
		static Real64 SpecHumOut( 0.0 ); // Specific humidity ratio of outlet air (kg moisture / kg moist air)
		static Real64 SpecHumIn( 0.0 ); // Specific humidity ratio of inlet air (kg moisture / kg moist air)

		ProvideSysOutput = true;
		for ( AirDistCompNum = 1; AirDistCompNum <= AirDistUnit( AirDistUnitNum ).NumComponents; ++AirDistCompNum ) {
			NonAirSysOutput = 0.0;
			InNodeNum = AirDistUnit( AirDistUnitNum ).InletNodeNum;
			OutNodeNum = AirDistUnit( AirDistUnitNum ).OutletNodeNum;
			MassFlowRateMaxAvail = 0.0;
			MassFlowRateMinAvail = 0.0;
			// check for no plenum
			// set the max and min avail flow rates taking into acount the upstream leak
			if ( InNodeNum > 0 ) {
				if ( AirDistUnit( AirDistUnitNum ).UpStreamLeak ) {
					MassFlowRateMaxAvail = Node( InNodeNum ).MassFlowRateMaxAvail;
					MassFlowRateMinAvail = Node( InNodeNum ).MassFlowRateMinAvail;
					AirLoopNum = ZoneEquipConfig( ControlledZoneNum ).AirLoopNum;
					if ( AirLoopNum > 0 ) {
						DesFlowRatio = AirLoopFlow( AirLoopNum ).SysToZoneDesFlowRatio;
					} else {
						DesFlowRatio = 1.0;
					}
					MassFlowRateUpStreamLeakMax = AirDistUnit( AirDistUnitNum ).UpStreamLeakFrac * Node( InNodeNum ).MassFlowRateMax * DesFlowRatio;
					if ( MassFlowRateMaxAvail > MassFlowRateUpStreamLeakMax ) {
						AirDistUnit( AirDistUnitNum ).MassFlowRateUpStrLk = MassFlowRateUpStreamLeakMax;
						Node( InNodeNum ).MassFlowRateMaxAvail = MassFlowRateMaxAvail - MassFlowRateUpStreamLeakMax;
					} else {
						AirDistUnit( AirDistUnitNum ).MassFlowRateUpStrLk = MassFlowRateMaxAvail;
						Node( InNodeNum ).MassFlowRateMaxAvail = 0.0;
					}
					Node( InNodeNum ).MassFlowRateMinAvail = max( 0.0, MassFlowRateMinAvail - AirDistUnit( AirDistUnitNum ).MassFlowRateUpStrLk );
				}
			}

			{ auto const SELECT_CASE_var( AirDistUnit( AirDistUnitNum ).EquipType_Num( AirDistCompNum ) );

			if ( SELECT_CASE_var == DualDuctConstVolume ) {
				SimulateDualDuct( AirDistUnit( AirDistUnitNum ).EquipName( AirDistCompNum ), FirstHVACIteration, ActualZoneNum, ZoneEquipConfig( ControlledZoneNum ).ZoneNode, AirDistUnit( AirDistUnitNum ).EquipIndex( AirDistCompNum ) );

			} else if ( SELECT_CASE_var == DualDuctVAV ) {
				SimulateDualDuct( AirDistUnit( AirDistUnitNum ).EquipName( AirDistCompNum ), FirstHVACIteration, ActualZoneNum, ZoneEquipConfig( ControlledZoneNum ).ZoneNode, AirDistUnit( AirDistUnitNum ).EquipIndex( AirDistCompNum ) );

			} else if ( SELECT_CASE_var == DualDuctVAVOutdoorAir ) {
				SimulateDualDuct( AirDistUnit( AirDistUnitNum ).EquipName( AirDistCompNum ), FirstHVACIteration, ActualZoneNum, ZoneEquipConfig( ControlledZoneNum ).ZoneNode, AirDistUnit( AirDistUnitNum ).EquipIndex( AirDistCompNum ) );

			} else if ( SELECT_CASE_var == SingleDuctVAVReheat ) {
				SimulateSingleDuct( AirDistUnit( AirDistUnitNum ).EquipName( AirDistCompNum ), FirstHVACIteration, ActualZoneNum, ZoneEquipConfig( ControlledZoneNum ).ZoneNode, AirDistUnit( AirDistUnitNum ).EquipIndex( AirDistCompNum ) );

			} else if ( SELECT_CASE_var == SingleDuctCBVAVReheat ) {
				SimulateSingleDuct( AirDistUnit( AirDistUnitNum ).EquipName( AirDistCompNum ), FirstHVACIteration, ActualZoneNum, ZoneEquipConfig( ControlledZoneNum ).ZoneNode, AirDistUnit( AirDistUnitNum ).EquipIndex( AirDistCompNum ) );

			} else if ( SELECT_CASE_var == SingleDuctVAVNoReheat ) {
				SimulateSingleDuct( AirDistUnit( AirDistUnitNum ).EquipName( AirDistCompNum ), FirstHVACIteration, ActualZoneNum, ZoneEquipConfig( ControlledZoneNum ).ZoneNode, AirDistUnit( AirDistUnitNum ).EquipIndex( AirDistCompNum ) );

			} else if ( SELECT_CASE_var == SingleDuctCBVAVNoReheat ) {
				SimulateSingleDuct( AirDistUnit( AirDistUnitNum ).EquipName( AirDistCompNum ), FirstHVACIteration, ActualZoneNum, ZoneEquipConfig( ControlledZoneNum ).ZoneNode, AirDistUnit( AirDistUnitNum ).EquipIndex( AirDistCompNum ) );

			} else if ( SELECT_CASE_var == SingleDuctConstVolReheat ) {
				SimulateSingleDuct( AirDistUnit( AirDistUnitNum ).EquipName( AirDistCompNum ), FirstHVACIteration, ActualZoneNum, ZoneEquipConfig( ControlledZoneNum ).ZoneNode, AirDistUnit( AirDistUnitNum ).EquipIndex( AirDistCompNum ) );

			} else if ( SELECT_CASE_var == SingleDuct_SeriesPIU_Reheat ) {
				SimPIU( AirDistUnit( AirDistUnitNum ).EquipName( AirDistCompNum ), FirstHVACIteration, ActualZoneNum, ZoneEquipConfig( ControlledZoneNum ).ZoneNode, AirDistUnit( AirDistUnitNum ).EquipIndex( AirDistCompNum ) );

			} else if ( SELECT_CASE_var == SingleDuct_ParallelPIU_Reheat ) {
				SimPIU( AirDistUnit( AirDistUnitNum ).EquipName( AirDistCompNum ), FirstHVACIteration, ActualZoneNum, ZoneEquipConfig( ControlledZoneNum ).ZoneNode, AirDistUnit( AirDistUnitNum ).EquipIndex( AirDistCompNum ) );

			} else if ( SELECT_CASE_var == SingleDuct_ConstVol_4PipeInduc ) {
				SimIndUnit( AirDistUnit( AirDistUnitNum ).EquipName( AirDistCompNum ), FirstHVACIteration, ActualZoneNum, ZoneEquipConfig( ControlledZoneNum ).ZoneNode, AirDistUnit( AirDistUnitNum ).EquipIndex( AirDistCompNum ) );

			} else if ( SELECT_CASE_var == SingleDuctVAVReheatVSFan ) {
				SimulateSingleDuct( AirDistUnit( AirDistUnitNum ).EquipName( AirDistCompNum ), FirstHVACIteration, ActualZoneNum, ZoneEquipConfig( ControlledZoneNum ).ZoneNode, AirDistUnit( AirDistUnitNum ).EquipIndex( AirDistCompNum ) );

			} else if ( SELECT_CASE_var == SingleDuctConstVolCooledBeam ) {
				SimCoolBeam( AirDistUnit( AirDistUnitNum ).EquipName( AirDistCompNum ), FirstHVACIteration, ActualZoneNum, ZoneEquipConfig( ControlledZoneNum ).ZoneNode, AirDistUnit( AirDistUnitNum ).EquipIndex( AirDistCompNum ), NonAirSysOutput );

			} else if ( SELECT_CASE_var == SingleDuctConstVolFourPipeBeam ) {
				AirDistUnit( AirDistUnitNum ).airTerminalPtr->simulate(FirstHVACIteration,  NonAirSysOutput );

			} else if ( SELECT_CASE_var == SingleDuctUserDefined ) {
				SimAirTerminalUserDefined( AirDistUnit( AirDistUnitNum ).EquipName( AirDistCompNum ), FirstHVACIteration, ActualZoneNum, ZoneEquipConfig( ControlledZoneNum ).ZoneNode, AirDistUnit( AirDistUnitNum ).EquipIndex( AirDistCompNum ) );

			} else if ( SELECT_CASE_var == SingleDuctInletATMixer ) {
				ProvideSysOutput = false;

			} else if ( SELECT_CASE_var == SingleDuctSupplyATMixer ) {
				ProvideSysOutput = false;

			} else {
				ShowSevereError( "Error found in ZoneHVAC:AirDistributionUnit=" + AirDistUnit( AirDistUnitNum ).Name );
				ShowContinueError( "Invalid Component=" + AirDistUnit( AirDistUnitNum ).EquipType( AirDistCompNum ) );
				ShowFatalError( "Preceding condition causes termination." );

			}}

			// do leak mass flow calcs
			if ( InNodeNum > 0 ) {
				if ( AirDistUnit( AirDistUnitNum ).UpStreamLeak ) {
					Node( InNodeNum ).MassFlowRateMaxAvail = MassFlowRateMaxAvail;
					Node( InNodeNum ).MassFlowRateMinAvail = MassFlowRateMinAvail;
				}
				if ( ( AirDistUnit( AirDistUnitNum ).UpStreamLeak || AirDistUnit( AirDistUnitNum ).DownStreamLeak ) && MassFlowRateMaxAvail > 0.0 ) {
					AirDistUnit( AirDistUnitNum ).MassFlowRateTU = Node( InNodeNum ).MassFlowRate;
					AirDistUnit( AirDistUnitNum ).MassFlowRateZSup = AirDistUnit( AirDistUnitNum ).MassFlowRateTU * ( 1.0 - AirDistUnit( AirDistUnitNum ).DownStreamLeakFrac );
					AirDistUnit( AirDistUnitNum ).MassFlowRateDnStrLk = AirDistUnit( AirDistUnitNum ).MassFlowRateTU * AirDistUnit( AirDistUnitNum ).DownStreamLeakFrac;
					AirDistUnit( AirDistUnitNum ).MassFlowRateSup = AirDistUnit( AirDistUnitNum ).MassFlowRateTU + AirDistUnit( AirDistUnitNum ).MassFlowRateUpStrLk;
					Node( InNodeNum ).MassFlowRate = AirDistUnit( AirDistUnitNum ).MassFlowRateSup;
					Node( OutNodeNum ).MassFlowRate = AirDistUnit( AirDistUnitNum ).MassFlowRateZSup;
					Node( OutNodeNum ).MassFlowRateMaxAvail = max( 0.0, MassFlowRateMaxAvail - AirDistUnit( AirDistUnitNum ).MassFlowRateDnStrLk - AirDistUnit( AirDistUnitNum ).MassFlowRateUpStrLk );
					Node( OutNodeNum ).MassFlowRateMinAvail = max( 0.0, MassFlowRateMinAvail - AirDistUnit( AirDistUnitNum ).MassFlowRateDnStrLk - AirDistUnit( AirDistUnitNum ).MassFlowRateUpStrLk );
					AirDistUnit( AirDistUnitNum ).MaxAvailDelta = MassFlowRateMaxAvail - Node( OutNodeNum ).MassFlowRateMaxAvail;
					AirDistUnit( AirDistUnitNum ).MinAvailDelta = MassFlowRateMinAvail - Node( OutNodeNum ).MassFlowRateMinAvail;
				}
			}

		}
		// Sign convention: SysOutputProvided <0 Zone is cooled
		//                  SysOutputProvided >0 Zone is heated
		CpAirZn = PsyCpAirFnWTdb( Node( ZoneEquipConfig( ControlledZoneNum ).ZoneNode ).HumRat, Node( ZoneEquipConfig( ControlledZoneNum ).ZoneNode ).Temp );
		CpAirSys = PsyCpAirFnWTdb( Node( AirDistUnit( AirDistUnitNum ).OutletNodeNum ).HumRat, Node( AirDistUnit( AirDistUnitNum ).OutletNodeNum ).Temp );
		SysOutputProvided = Node( AirDistUnit( AirDistUnitNum ).OutletNodeNum ).MassFlowRate *
			( CpAirSys * Node( AirDistUnit( AirDistUnitNum ).OutletNodeNum ).Temp - CpAirZn * Node( ZoneEquipConfig( ControlledZoneNum ).ZoneNode ).Temp );
		// AirDistUnit( AirDistUnitNum ).HeatRate = max( 0.0, SysOutputProvided );
		// AirDistUnit( AirDistUnitNum ).CoolRate = std::abs( min( 0.0, SysOutputProvided ));
		if ( ProvideSysOutput ) {
			// Sign convention: LatOutputProvided <0 Zone is dehumidified
			//                  LatOutputProvided >0 Zone is humidified
			// CR9155 Remove specific humidity calculations
			SpecHumOut = Node( AirDistUnit( AirDistUnitNum ).OutletNodeNum ).HumRat;
			SpecHumIn = Node( ZoneEquipConfig( ControlledZoneNum ).ZoneNode ).HumRat;
			LatOutputProvided = Node( AirDistUnit( AirDistUnitNum ).OutletNodeNum ).MassFlowRate * ( SpecHumOut - SpecHumIn ); // Latent rate (kg/s), dehumid = negative
		} else {
			SysOutputProvided = 0.0;
			LatOutputProvided = 0.0;
		}

	}

	void
	UpdateZoneAirLoopEquipment()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Russ Taylor
		//       DATE WRITTEN   Nov 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is left for Module format consistency -- not needed in this module.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

	}

	// void
		// ReportZoneAirLoopEquipment(
		// int const AirDistUnitNum
		// )
	// {
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Russ Taylor
		//       DATE WRITTEN:    Nov 1997

		// PURPOSE OF THIS SUBROUTINE: This subroutine

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// Using/Aliasing
		// using DataHVACGlobals::TimeStepSys;

		//report the Direct Air Output
		// AirDistUnit( AirDistUnitNum ).HeatGain = AirDistUnit( AirDistUnitNum ).HeatRate * TimeStepSys * SecInHour;
		// AirDistUnit( AirDistUnitNum ).CoolGain = AirDistUnit( AirDistUnitNum ).CoolRate * TimeStepSys * SecInHour;
	// }

} // ZoneAirLoopEquipmentManager

} // EnergyPlus
