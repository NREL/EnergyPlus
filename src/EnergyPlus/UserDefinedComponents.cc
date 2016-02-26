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
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <UserDefinedComponents.hh>
#include <BranchNodeConnections.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataLoopNode.hh>
#include <DataPrecisionGlobals.hh>
#include <DataRuntimeLanguage.hh>
#include <DataWater.hh>
#include <DataZoneEnergyDemands.hh>
#include <DataZoneEquipment.hh>
#include <EMSManager.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <GlobalNames.hh>
#include <HeatBalanceInternalHeatGains.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <PlantUtilities.hh>
#include <Psychrometrics.hh>
#include <UtilityRoutines.hh>
#include <WaterManager.hh>

namespace EnergyPlus {

namespace UserDefinedComponents {

	// Module containing the routines dealing with the User Defined HVAC and Plant component models

	// MODULE INFORMATION:
	//       AUTHOR         B. Griffith
	//       DATE WRITTEN   January 2012
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// Collect component models for custom program with Erl.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::emsCallFromUserDefinedComponentModel;
	using DataGlobals::BeginEnvrnFlag;
	using DataGlobals::NumOfZones;
	using namespace DataPlant;
	using namespace DataLoopNode;
	using namespace DataRuntimeLanguage;
	using DataWater::WaterStorage;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// na

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:

	int NumUserPlantComps( 0 );
	int NumUserCoils( 0 );
	int NumUserZoneAir( 0 );
	int NumUserAirTerminals( 0 );

	Array1D_bool CheckUserPlantCompName;
	Array1D_bool CheckUserCoilName;
	Array1D_bool CheckUserZoneAirName;
	Array1D_bool CheckUserAirTerminal;
	bool GetInput( true );

	// SUBROUTINE SPECIFICATIONS FOR MODULE <module_name>:

	// Object Data
	Array1D< UserPlantComponentStruct > UserPlantComp;
	Array1D< UserCoilComponentStruct > UserCoil;
	Array1D< UserZoneHVACForcedAirComponentStruct > UserZoneAirHVAC;
	Array1D< UserAirTerminalComponentStruct > UserAirTerminal;

	// Functions

	void
	SimUserDefinedPlantComponent(
		int const LoopNum, // plant loop sim call originated from
		int const LoopSideNum, // plant loop side sim call originated from
		std::string const & EP_UNUSED( EquipType ), // type of equipment, 'PlantComponent:UserDefined'
		std::string const & EquipName, // user name for component
		int & CompIndex,
		bool & InitLoopEquip,
		Real64 const MyLoad,
		Real64 & MaxCap,
		Real64 & MinCap,
		Real64 & OptCap
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   Jan 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// User Defined plant generic component

		// METHODOLOGY EMPLOYED:
		// This routine to be called from PlantLoopEquipment.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using General::TrimSigDigits;
		using EMSManager::ManageEMS;
		using PlantUtilities::InitComponentNodes;
		using PlantUtilities::RegisterPlantCompDesignFlow;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int CompNum;
		int ThisLoop;
		int Loop;

		//Autodesk:Uninit Initialize variables used uninitialized
		ThisLoop = 0; //Autodesk:Uninit Force default initialization

		if ( GetInput ) {
			GetUserDefinedComponents();
			GetInput = false;
		}

		// Find the correct Equipment
		if ( CompIndex == 0 ) {
			CompNum = FindItemInList( EquipName, UserPlantComp );
			if ( CompNum == 0 ) {
				ShowFatalError( "SimUserDefinedPlantComponent: User Defined Plant Component not found" );
			}
			CompIndex = CompNum;
		} else {
			CompNum = CompIndex;
			if ( CompNum < 1 || CompNum > NumUserPlantComps ) {
				ShowFatalError( "SimUserDefinedPlantComponent: Invalid CompIndex passed=" + TrimSigDigits( CompNum ) + ", Number of units =" + TrimSigDigits( NumUserPlantComps ) + ", Entered Unit name = " + EquipName );
			}
			if ( CheckUserPlantCompName( CompNum ) ) {
				if ( EquipName != UserPlantComp( CompNum ).Name ) {
					ShowFatalError( "SimUserDefinedPlantComponent: Invalid CompIndex passed=" + TrimSigDigits( CompNum ) + ", Unit name=" + EquipName + ", stored unit name for that index=" + UserPlantComp( CompNum ).Name );
				}
				CheckUserPlantCompName( CompNum ) = false;
			}
		}

		if ( InitLoopEquip || BeginEnvrnFlag ) {
			InitPlantUserComponent( CompNum, LoopNum, MyLoad );
			// find loop connection number from LoopNum and LoopSide
			ThisLoop = 0;
			for ( Loop = 1; Loop <= UserPlantComp( CompNum ).NumPlantConnections; ++Loop ) {
				if ( LoopNum != UserPlantComp( CompNum ).Loop( Loop ).LoopNum ) continue;
				if ( LoopSideNum != UserPlantComp( CompNum ).Loop( Loop ).LoopSideNum ) continue;
				ThisLoop = Loop;
			}
			if ( ThisLoop > 0 ) {
				if ( UserPlantComp( CompNum ).Loop( ThisLoop ).ErlInitProgramMngr > 0 ) {
					ManageEMS( emsCallFromUserDefinedComponentModel, UserPlantComp( CompNum ).Loop( ThisLoop ).ErlInitProgramMngr );
				}
				// now interface sizing related values with rest of E+
				MinCap = UserPlantComp( CompNum ).Loop( ThisLoop ).MinLoad;
				MaxCap = UserPlantComp( CompNum ).Loop( ThisLoop ).MaxLoad;
				OptCap = UserPlantComp( CompNum ).Loop( ThisLoop ).OptLoad;

				InitComponentNodes( UserPlantComp( CompNum ).Loop( ThisLoop ).MassFlowRateMin, UserPlantComp( CompNum ).Loop( ThisLoop ).MassFlowRateMax, UserPlantComp( CompNum ).Loop( ThisLoop ).InletNodeNum, UserPlantComp( CompNum ).Loop( ThisLoop ).OutletNodeNum, UserPlantComp( CompNum ).Loop( ThisLoop ).LoopNum, UserPlantComp( CompNum ).Loop( ThisLoop ).LoopSideNum, UserPlantComp( CompNum ).Loop( ThisLoop ).BranchNum, UserPlantComp( CompNum ).Loop( ThisLoop ).CompNum );

				RegisterPlantCompDesignFlow( UserPlantComp( CompNum ).Loop( ThisLoop ).InletNodeNum, UserPlantComp( CompNum ).Loop( ThisLoop ).DesignVolumeFlowRate );

			} else {
				// throw warning
				ShowFatalError( "SimUserDefinedPlantComponent: did not find where called from loop number called from =" + TrimSigDigits( LoopNum ) + " , loop side called from =" + TrimSigDigits( LoopSideNum ) );
			}
			return;
		}

		ThisLoop = 0;
		for ( Loop = 1; Loop <= UserPlantComp( CompNum ).NumPlantConnections; ++Loop ) {
			if ( LoopNum != UserPlantComp( CompNum ).Loop( Loop ).LoopNum ) continue;
			if ( LoopSideNum != UserPlantComp( CompNum ).Loop( Loop ).LoopSideNum ) continue;
			ThisLoop = Loop;
		}

		InitPlantUserComponent( CompNum, ThisLoop, MyLoad );

		if ( ThisLoop > 0 ) {
			if ( UserPlantComp( CompNum ).Loop( ThisLoop ).ErlSimProgramMngr > 0 ) {
				ManageEMS( emsCallFromUserDefinedComponentModel, UserPlantComp( CompNum ).Loop( ThisLoop ).ErlSimProgramMngr );
			}
		}

		if ( UserPlantComp( CompNum ).ErlSimProgramMngr > 0 ) {
			ManageEMS( emsCallFromUserDefinedComponentModel, UserPlantComp( CompNum ).ErlSimProgramMngr );
		}

		ReportPlantUserComponent( CompNum, ThisLoop );

	}

	void
	SimCoilUserDefined(
		std::string const & EquipName, // user name for component
		int & CompIndex,
		int const AirLoopNum,
		bool & HeatingActive,
		bool & CoolingActive
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   Feb. 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using General::TrimSigDigits;
		using EMSManager::ManageEMS;
		using PlantUtilities::SetComponentFlowRate;
		using PlantUtilities::InitComponentNodes;
		using PlantUtilities::RegisterPlantCompDesignFlow;
		using Psychrometrics::PsyHFnTdbW;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 EnthInlet;
		Real64 EnthOutlet;
		int CompNum;

		if ( GetInput ) {
			GetUserDefinedComponents();
			GetInput = false;
		}

		// Find the correct Equipment
		if ( CompIndex == 0 ) {
			CompNum = FindItemInList( EquipName, UserCoil );
			if ( CompNum == 0 ) {
				ShowFatalError( "SimUserDefinedPlantComponent: User Defined Coil not found" );
			}
			CompIndex = CompNum;
		} else {
			CompNum = CompIndex;
			if ( CompNum < 1 || CompNum > NumUserCoils ) {
				ShowFatalError( "SimUserDefinedPlantComponent: Invalid CompIndex passed=" + TrimSigDigits( CompNum ) + ", Number of units =" + TrimSigDigits( NumUserCoils ) + ", Entered Unit name = " + EquipName );
			}
			if ( CheckUserCoilName( CompNum ) ) {
				if ( EquipName != UserCoil( CompNum ).Name ) {
					ShowFatalError( "SimUserDefinedPlantComponent: Invalid CompIndex passed=" + TrimSigDigits( CompNum ) + ", Unit name=" + EquipName + ", stored unit name for that index=" + UserCoil( CompNum ).Name );
				}
				CheckUserCoilName( CompNum ) = false;
			}
		}

		if ( BeginEnvrnFlag ) {
			if ( UserCoil( CompNum ).ErlInitProgramMngr > 0 ) {
				ManageEMS( emsCallFromUserDefinedComponentModel, UserCoil( CompNum ).ErlInitProgramMngr );
			}

			if ( UserCoil( CompNum ).PlantIsConnected ) {

				InitComponentNodes( UserCoil( CompNum ).Loop.MassFlowRateMin, UserCoil( CompNum ).Loop.MassFlowRateMax, UserCoil( CompNum ).Loop.InletNodeNum, UserCoil( CompNum ).Loop.OutletNodeNum, UserCoil( CompNum ).Loop.LoopNum, UserCoil( CompNum ).Loop.LoopSideNum, UserCoil( CompNum ).Loop.BranchNum, UserCoil( CompNum ).Loop.CompNum );

				RegisterPlantCompDesignFlow( UserCoil( CompNum ).Loop.InletNodeNum, UserCoil( CompNum ).Loop.DesignVolumeFlowRate );

			}
		}

		InitCoilUserDefined( CompNum );

		if ( UserCoil( CompNum ).ErlSimProgramMngr > 0 ) {
			ManageEMS( emsCallFromUserDefinedComponentModel, UserCoil( CompNum ).ErlSimProgramMngr );
		}

		ReportCoilUserDefined( CompNum );

		if ( AirLoopNum != -1 ) { // IF the sysem is not an equipment of outdoor air unit
			// determine if heating or cooling on primary air stream
			if ( Node( UserCoil( CompNum ).Air( 1 ).InletNodeNum ).Temp < Node( UserCoil( CompNum ).Air( 1 ).OutletNodeNum ).Temp ) {
				HeatingActive = true;
			} else {
				HeatingActive = false;
			}

			EnthInlet = PsyHFnTdbW( Node( UserCoil( CompNum ).Air( 1 ).InletNodeNum ).Temp, Node( UserCoil( CompNum ).Air( 1 ).InletNodeNum ).HumRat );
			EnthOutlet = PsyHFnTdbW( Node( UserCoil( CompNum ).Air( 1 ).OutletNodeNum ).Temp, Node( UserCoil( CompNum ).Air( 1 ).OutletNodeNum ).HumRat );
			if ( EnthInlet > EnthOutlet ) {
				CoolingActive = true;
			} else {
				CoolingActive = false;
			}
		}

	}

	void
	SimZoneAirUserDefined(
		std::string const & CompName, // name of the packaged terminal heat pump
		int const ZoneNum, // number of zone being served
		Real64 & SensibleOutputProvided, // sensible capacity delivered to zone
		Real64 & LatentOutputProvided, // Latent add/removal  (kg/s), dehumid = negative
		int & CompIndex // index to zone hvac unit
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   February, 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using General::TrimSigDigits;
		using EMSManager::ManageEMS;
		using PlantUtilities::SetComponentFlowRate;
		using PlantUtilities::InitComponentNodes;
		using PlantUtilities::RegisterPlantCompDesignFlow;
		using Psychrometrics::PsyHFnTdbW;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int CompNum;
		int Loop;
		Real64 AirMassFlow;
		Real64 MinHumRat;
		Real64 SpecHumOut;
		Real64 SpecHumIn;

		if ( GetInput ) {
			GetUserDefinedComponents();
			GetInput = false;
		}

		// Find the correct Equipment
		if ( CompIndex == 0 ) {
			CompNum = FindItemInList( CompName, UserZoneAirHVAC );
			if ( CompNum == 0 ) {
				ShowFatalError( "SimUserDefinedPlantComponent: User Defined Coil not found" );
			}
			CompIndex = CompNum;
		} else {
			CompNum = CompIndex;
			if ( CompNum < 1 || CompNum > NumUserZoneAir ) {
				ShowFatalError( "SimUserDefinedPlantComponent: Invalid CompIndex passed=" + TrimSigDigits( CompNum ) + ", Number of units =" + TrimSigDigits( NumUserZoneAir ) + ", Entered Unit name = " + CompName );
			}
			if ( CheckUserZoneAirName( CompNum ) ) {
				if ( CompName != UserZoneAirHVAC( CompNum ).Name ) {
					ShowFatalError( "SimUserDefinedPlantComponent: Invalid CompIndex passed=" + TrimSigDigits( CompNum ) + ", Unit name=" + CompName + ", stored unit name for that index=" + UserZoneAirHVAC( CompNum ).Name );
				}
				CheckUserZoneAirName( CompNum ) = false;
			}
		}

		if ( BeginEnvrnFlag ) {
			InitZoneAirUserDefined( CompNum, ZoneNum );

			if ( UserZoneAirHVAC( CompNum ).ErlInitProgramMngr > 0 ) {
				ManageEMS( emsCallFromUserDefinedComponentModel, UserZoneAirHVAC( CompNum ).ErlInitProgramMngr );
			}
			if ( UserZoneAirHVAC( CompNum ).NumPlantConnections > 0 ) {
				for ( Loop = 1; Loop <= UserZoneAirHVAC( CompNum ).NumPlantConnections; ++Loop ) {

					InitComponentNodes( UserZoneAirHVAC( CompNum ).Loop( Loop ).MassFlowRateMin, UserZoneAirHVAC( CompNum ).Loop( Loop ).MassFlowRateMax, UserZoneAirHVAC( CompNum ).Loop( Loop ).InletNodeNum, UserZoneAirHVAC( CompNum ).Loop( Loop ).OutletNodeNum, UserZoneAirHVAC( CompNum ).Loop( Loop ).LoopNum, UserZoneAirHVAC( CompNum ).Loop( Loop ).LoopSideNum, UserZoneAirHVAC( CompNum ).Loop( Loop ).BranchNum, UserZoneAirHVAC( CompNum ).Loop( Loop ).CompNum );

					RegisterPlantCompDesignFlow( UserZoneAirHVAC( CompNum ).Loop( Loop ).InletNodeNum, UserZoneAirHVAC( CompNum ).Loop( Loop ).DesignVolumeFlowRate );
				}
			}

		} // BeginEnvrnFlag

		InitZoneAirUserDefined( CompNum, ZoneNum );

		if ( UserZoneAirHVAC( CompNum ).ErlSimProgramMngr > 0 ) {
			ManageEMS( emsCallFromUserDefinedComponentModel, UserZoneAirHVAC( CompNum ).ErlSimProgramMngr );
		}

		ReportZoneAirUserDefined( CompNum );

		// calculate delivered capacity
		AirMassFlow = min( Node( UserZoneAirHVAC( CompNum ).ZoneAir.InletNodeNum ).MassFlowRate, Node( UserZoneAirHVAC( CompNum ).ZoneAir.OutletNodeNum ).MassFlowRate );
		// calculate sensible load met using delta enthalpy at a constant (minimum) humidity ratio)
		MinHumRat = min( Node( UserZoneAirHVAC( CompNum ).ZoneAir.InletNodeNum ).HumRat, Node( UserZoneAirHVAC( CompNum ).ZoneAir.OutletNodeNum ).HumRat );
		SensibleOutputProvided = AirMassFlow * ( PsyHFnTdbW( Node( UserZoneAirHVAC( CompNum ).ZoneAir.OutletNodeNum ).Temp, MinHumRat ) - PsyHFnTdbW( Node( UserZoneAirHVAC( CompNum ).ZoneAir.InletNodeNum ).Temp, MinHumRat ) );

		// CR9155 Remove specific humidity calculations
		SpecHumOut = Node( UserZoneAirHVAC( CompNum ).ZoneAir.OutletNodeNum ).HumRat;
		SpecHumIn = Node( UserZoneAirHVAC( CompNum ).ZoneAir.InletNodeNum ).HumRat;
		LatentOutputProvided = AirMassFlow * ( SpecHumOut - SpecHumIn ); // Latent rate, kg/s (dehumid = negative)

	}

	void
	SimAirTerminalUserDefined(
		std::string const & CompName,
		bool const EP_UNUSED( FirstHVACIteration ),
		int const ZoneNum,
		int const EP_UNUSED( ZoneNodeNum ),
		int & CompIndex
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   March 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// simulation call for generic air terminal

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using General::TrimSigDigits;
		using EMSManager::ManageEMS;
		using PlantUtilities::SetComponentFlowRate;
		using PlantUtilities::InitComponentNodes;
		using PlantUtilities::RegisterPlantCompDesignFlow;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int CompNum;
		int Loop;

		if ( GetInput ) {
			GetUserDefinedComponents();
			GetInput = false;
		}

		// Find the correct Equipment
		if ( CompIndex == 0 ) {
			CompNum = FindItemInList( CompName, UserAirTerminal );
			if ( CompNum == 0 ) {
				ShowFatalError( "SimUserDefinedPlantComponent: User Defined Coil not found" );
			}
			CompIndex = CompNum;
		} else {
			CompNum = CompIndex;
			if ( CompNum < 1 || CompNum > NumUserAirTerminals ) {
				ShowFatalError( "SimUserDefinedPlantComponent: Invalid CompIndex passed=" + TrimSigDigits( CompNum ) + ", Number of units =" + TrimSigDigits( NumUserAirTerminals ) + ", Entered Unit name = " + CompName );
			}
			if ( CheckUserAirTerminal( CompNum ) ) {
				if ( CompName != UserAirTerminal( CompNum ).Name ) {
					ShowFatalError( "SimUserDefinedPlantComponent: Invalid CompIndex passed=" + TrimSigDigits( CompNum ) + ", Unit name=" + CompName + ", stored unit name for that index=" + UserAirTerminal( CompNum ).Name );
				}
				CheckUserAirTerminal( CompNum ) = false;
			}
		}

		if ( BeginEnvrnFlag ) {
			InitAirTerminalUserDefined( CompNum, ZoneNum );

			if ( UserAirTerminal( CompNum ).ErlInitProgramMngr > 0 ) {
				ManageEMS( emsCallFromUserDefinedComponentModel, UserAirTerminal( CompNum ).ErlInitProgramMngr );
			}
			if ( UserAirTerminal( CompNum ).NumPlantConnections > 0 ) {
				for ( Loop = 1; Loop <= UserAirTerminal( CompNum ).NumPlantConnections; ++Loop ) {

					InitComponentNodes( UserAirTerminal( CompNum ).Loop( Loop ).MassFlowRateMin, UserAirTerminal( CompNum ).Loop( Loop ).MassFlowRateMax, UserAirTerminal( CompNum ).Loop( Loop ).InletNodeNum, UserAirTerminal( CompNum ).Loop( Loop ).OutletNodeNum, UserAirTerminal( CompNum ).Loop( Loop ).LoopNum, UserAirTerminal( CompNum ).Loop( Loop ).LoopSideNum, UserAirTerminal( CompNum ).Loop( Loop ).BranchNum, UserAirTerminal( CompNum ).Loop( Loop ).CompNum );

					RegisterPlantCompDesignFlow( UserAirTerminal( CompNum ).Loop( Loop ).InletNodeNum, UserAirTerminal( CompNum ).Loop( Loop ).DesignVolumeFlowRate );
				}

			}

		} // BeginEnvrnFlag

		InitAirTerminalUserDefined( CompNum, ZoneNum );

		if ( UserAirTerminal( CompNum ).ErlSimProgramMngr > 0 ) {
			ManageEMS( emsCallFromUserDefinedComponentModel, UserAirTerminal( CompNum ).ErlSimProgramMngr );
		}

		ReportAirTerminalUserDefined( CompNum );

	}

	void
	GetUserDefinedComponents()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   Jan 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectDefMaxArgs;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using InputProcessor::VerifyName;
		using General::RoundSigDigits;
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::TestCompSet;
		using DataHeatBalance::Zone;
		using DataHeatBalance::IntGainTypeOf_PlantComponentUserDefined;
		using DataHeatBalance::IntGainTypeOf_CoilUserDefined;
		using DataHeatBalance::IntGainTypeOf_ZoneHVACForcedAirUserDefined;
		using DataHeatBalance::IntGainTypeOf_AirTerminalUserDefined;
		using WaterManager::SetupTankDemandComponent;
		using WaterManager::SetupTankSupplyComponent;
		using DataZoneEquipment::ZoneEquipConfig;
		using GlobalNames::VerifyUniqueCoilName;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static gio::Fmt fmtLD( "*" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool ErrorsFound( false );
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		int IOStat; // IO Status when calling get input subroutine
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		static int MaxNumAlphas( 0 ); // argument for call to GetObjectDefMaxArgs
		static int MaxNumNumbers( 0 ); // argument for call to GetObjectDefMaxArgs
		static int TotalArgs( 0 ); // argument for call to GetObjectDefMaxArgs
		Array1D_string cAlphaFieldNames;
		Array1D_string cNumericFieldNames;
		Array1D_bool lNumericFieldBlanks;
		Array1D_bool lAlphaFieldBlanks;
		Array1D_string cAlphaArgs;
		Array1D< Real64 > rNumericArgs;
		std::string cCurrentModuleObject;
		int CompLoop;
		int ConnectionLoop;
		int NumPlantConnections;
		int NumAirConnections;
		std::string LoopStr;
		int aArgCount;
		int StackMngrNum;
		static bool lDummy; //Fix Changed to static: Passed to SetupEMSActuator as source of persistent Reference
		//  INTEGER  :: alphaNum
		//  INTEGER  :: Loop
		int MgrCountTest;
		int CtrlZone; // controlled zone do loop index
		int SupAirIn; // controlled zone supply air inlet index
		bool errFlag;

		cCurrentModuleObject = "PlantComponent:UserDefined";
		GetObjectDefMaxArgs( cCurrentModuleObject, TotalArgs, NumAlphas, NumNums );
		MaxNumNumbers = NumNums;
		MaxNumAlphas = NumAlphas;

		cAlphaFieldNames.allocate( MaxNumAlphas );
		cAlphaArgs.allocate( MaxNumAlphas );
		lAlphaFieldBlanks.dimension( MaxNumAlphas, false );
		cNumericFieldNames.allocate( MaxNumNumbers );
		rNumericArgs.dimension( MaxNumNumbers, 0.0 );
		lNumericFieldBlanks.dimension( MaxNumNumbers, false );

		//need to make sure GetEMSInput has run...

		cCurrentModuleObject = "PlantComponent:UserDefined";
		NumUserPlantComps = GetNumObjectsFound( cCurrentModuleObject );
		if ( NumUserPlantComps > 0 ) {
			UserPlantComp.allocate( NumUserPlantComps );
			CheckUserPlantCompName.dimension( NumUserPlantComps, true );
			for ( CompLoop = 1; CompLoop <= NumUserPlantComps; ++CompLoop ) {
				GetObjectItem( cCurrentModuleObject, CompLoop, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				IsNotOK = false;
				IsBlank = false;
				VerifyName( cAlphaArgs( 1 ), UserPlantComp, CompLoop - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
				}
				UserPlantComp( CompLoop ).Name = cAlphaArgs( 1 );

				// now get program manager for model simulations
				if ( ! lAlphaFieldBlanks( 2 ) ) {
					StackMngrNum = FindItemInList( cAlphaArgs( 2 ), EMSProgramCallManager );
					if ( StackMngrNum > 0 ) { // found it
						UserPlantComp( CompLoop ).ErlSimProgramMngr = StackMngrNum;
					} else {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ShowContinueError( "Program Manager Name not found." );
						ErrorsFound = true;
					}
				}

				NumPlantConnections = std::floor( rNumericArgs( 1 ) );

				if ( ( NumPlantConnections >= 1 ) && ( NumPlantConnections <= 4 ) ) {
					UserPlantComp( CompLoop ).Loop.allocate( NumPlantConnections );
					UserPlantComp( CompLoop ).NumPlantConnections = NumPlantConnections;
					for ( ConnectionLoop = 1; ConnectionLoop <= NumPlantConnections; ++ConnectionLoop ) {
						LoopStr = RoundSigDigits( ConnectionLoop );
						aArgCount = ( ConnectionLoop - 1 ) * 6 + 3;
						UserPlantComp( CompLoop ).Loop( ConnectionLoop ).InletNodeNum = GetOnlySingleNode( cAlphaArgs( aArgCount ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, ConnectionLoop, ObjectIsNotParent );
						UserPlantComp( CompLoop ).Loop( ConnectionLoop ).OutletNodeNum = GetOnlySingleNode( cAlphaArgs( aArgCount + 1 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, ConnectionLoop, ObjectIsNotParent );

						TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( aArgCount ), cAlphaArgs( aArgCount + 1 ), "Plant Nodes " + LoopStr );

						{ auto const SELECT_CASE_var( cAlphaArgs( aArgCount + 2 ) );
						if ( SELECT_CASE_var == "DEMANDSLOAD" ) {
							UserPlantComp( CompLoop ).Loop( ConnectionLoop ).HowLoadServed = HowMet_NoneDemand;
						} else if ( SELECT_CASE_var == "MEETSLOADWITHPASSIVECAPACITY" ) {
							UserPlantComp( CompLoop ).Loop( ConnectionLoop ).HowLoadServed = HowMet_PassiveCap;
						} else if ( SELECT_CASE_var == "MEETSLOADWITHNOMINALCAPACITY" ) {
							UserPlantComp( CompLoop ).Loop( ConnectionLoop ).HowLoadServed = HowMet_ByNominalCap;
						} else if ( SELECT_CASE_var == "MEETSLOADWITHNOMINALCAPACITYLOWOUTLIMIT" ) {
							UserPlantComp( CompLoop ).Loop( ConnectionLoop ).HowLoadServed = HowMet_ByNominalCapLowOutLimit;
							// actuator for low out limit
							SetupEMSActuator( "Plant Connection " + LoopStr, UserPlantComp( CompLoop ).Name, "Low Outlet Temperature Limit", "[C]", lDummy, UserPlantComp( CompLoop ).Loop( ConnectionLoop ).LowOutTempLimit );
						} else if ( SELECT_CASE_var == "MEETSLOADWITHNOMINALCAPACITYHIOUTLIMIT" ) {
							UserPlantComp( CompLoop ).Loop( ConnectionLoop ).HowLoadServed = HowMet_ByNominalCapHiOutLimit;
							// actuator for hi out limit
							SetupEMSActuator( "Plant Connection " + LoopStr, UserPlantComp( CompLoop ).Name, "High Outlet Temperature Limit", "[C]", lDummy, UserPlantComp( CompLoop ).Loop( ConnectionLoop ).HiOutTempLimit );
						}}

						{ auto const SELECT_CASE_var( cAlphaArgs( aArgCount + 3 ) );
						if ( SELECT_CASE_var == "NEEDSFLOWIFLOOPON" ) {
							UserPlantComp( CompLoop ).Loop( ConnectionLoop ).FlowPriority = LoopFlowStatus_NeedyIfLoopOn;
						} else if ( SELECT_CASE_var == "NEEDSFLOWANDTURNSLOOPON" ) {
							UserPlantComp( CompLoop ).Loop( ConnectionLoop ).FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
						} else if ( SELECT_CASE_var == "RECEIVESWHATEVERFLOWAVAILABLE" ) {
							UserPlantComp( CompLoop ).Loop( ConnectionLoop ).FlowPriority = LoopFlowStatus_TakesWhatGets;
						}}

						// find program manager for initial setup, begin environment and sizing of this plant connection
						if ( ! lAlphaFieldBlanks( aArgCount + 4 ) ) {
							StackMngrNum = FindItemInList( cAlphaArgs( aArgCount + 4 ), EMSProgramCallManager );
							if ( StackMngrNum > 0 ) { // found it
								UserPlantComp( CompLoop ).Loop( ConnectionLoop ).ErlInitProgramMngr = StackMngrNum;
							} else {
								ShowSevereError( "Invalid " + cAlphaFieldNames( aArgCount + 4 ) + '=' + cAlphaArgs( aArgCount + 4 ) );
								ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
								ShowContinueError( "Program Manager Name not found." );
								ErrorsFound = true;
							}
						}

						// find program to call for model simulations for just this plant connection
						if ( ! lAlphaFieldBlanks( aArgCount + 5 ) ) {
							StackMngrNum = FindItemInList( cAlphaArgs( aArgCount + 5 ), EMSProgramCallManager );
							if ( StackMngrNum > 0 ) { // found it
								UserPlantComp( CompLoop ).Loop( ConnectionLoop ).ErlSimProgramMngr = StackMngrNum;
							} else {
								ShowSevereError( "Invalid " + cAlphaFieldNames( aArgCount + 4 ) + '=' + cAlphaArgs( aArgCount + 4 ) );
								ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
								ShowContinueError( "Program Manager Name not found." );
								ErrorsFound = true;
							}
						}
						//Setup Internal Variables
						//model input related internal variables
						SetupEMSInternalVariable( "Inlet Temperature for Plant Connection " + LoopStr, UserPlantComp( CompLoop ).Name, "[C]", UserPlantComp( CompLoop ).Loop( ConnectionLoop ).InletTemp );
						SetupEMSInternalVariable( "Inlet Mass Flow Rate for Plant Connection " + LoopStr, UserPlantComp( CompLoop ).Name, "[kg/s]", UserPlantComp( CompLoop ).Loop( ConnectionLoop ).InletMassFlowRate );
						if ( UserPlantComp( CompLoop ).Loop( ConnectionLoop ).HowLoadServed != HowMet_NoneDemand ) {
							SetupEMSInternalVariable( "Load Request for Plant Connection " + LoopStr, UserPlantComp( CompLoop ).Name, "[W]", UserPlantComp( CompLoop ).Loop( ConnectionLoop ).MyLoad );
						}
						SetupEMSInternalVariable( "Inlet Density for Plant Connection " + LoopStr, UserPlantComp( CompLoop ).Name, "[kg/m3]", UserPlantComp( CompLoop ).Loop( ConnectionLoop ).InletRho );
						SetupEMSInternalVariable( "Inlet Specific Heat for Plant Connection " + LoopStr, UserPlantComp( CompLoop ).Name, "[J/kg-C]", UserPlantComp( CompLoop ).Loop( ConnectionLoop ).InletCp );
						// model results related actuators
						SetupEMSActuator( "Plant Connection " + LoopStr, UserPlantComp( CompLoop ).Name, "Outlet Temperature", "[C]", lDummy, UserPlantComp( CompLoop ).Loop( ConnectionLoop ).OutletTemp );
						SetupEMSActuator( "Plant Connection " + LoopStr, UserPlantComp( CompLoop ).Name, "Mass Flow Rate", "[kg/s]", lDummy, UserPlantComp( CompLoop ).Loop( ConnectionLoop ).MassFlowRateRequest );
						// model initialization and sizing related actuators
						SetupEMSActuator( "Plant Connection " + LoopStr, UserPlantComp( CompLoop ).Name, "Minimum Mass Flow Rate", "[kg/s]", lDummy, UserPlantComp( CompLoop ).Loop( ConnectionLoop ).MassFlowRateMin );
						SetupEMSActuator( "Plant Connection " + LoopStr, UserPlantComp( CompLoop ).Name, "Maximum Mass Flow Rate", "[kg/s]", lDummy, UserPlantComp( CompLoop ).Loop( ConnectionLoop ).MassFlowRateMax );
						SetupEMSActuator( "Plant Connection " + LoopStr, UserPlantComp( CompLoop ).Name, "Design Volume Flow Rate", "[m3/s]", lDummy, UserPlantComp( CompLoop ).Loop( ConnectionLoop ).DesignVolumeFlowRate );
						SetupEMSActuator( "Plant Connection " + LoopStr, UserPlantComp( CompLoop ).Name, "Minimum Loading Capacity", "[W]", lDummy, UserPlantComp( CompLoop ).Loop( ConnectionLoop ).MinLoad );
						SetupEMSActuator( "Plant Connection " + LoopStr, UserPlantComp( CompLoop ).Name, "Maximum Loading Capacity", "[W]", lDummy, UserPlantComp( CompLoop ).Loop( ConnectionLoop ).MaxLoad );
						SetupEMSActuator( "Plant Connection " + LoopStr, UserPlantComp( CompLoop ).Name, "Optimal Loading Capacity", "[W]", lDummy, UserPlantComp( CompLoop ).Loop( ConnectionLoop ).OptLoad );
					}
				}

				if ( ! lAlphaFieldBlanks( 27 ) ) {
					UserPlantComp( CompLoop ).Air.InletNodeNum = GetOnlySingleNode( cAlphaArgs( 27 ), ErrorsFound, cCurrentModuleObject, UserPlantComp( CompLoop ).Name, NodeType_Air, NodeConnectionType_OutsideAirReference, 1, ObjectIsNotParent );
					//model input related internal variables
					SetupEMSInternalVariable( "Inlet Temperature for Air Connection", UserPlantComp( CompLoop ).Name, "[C]", UserPlantComp( CompLoop ).Air.InletTemp );
					SetupEMSInternalVariable( "Inlet Mass Flow Rate for Air Connection", UserPlantComp( CompLoop ).Name, "[kg/s]", UserPlantComp( CompLoop ).Air.InletMassFlowRate );
					SetupEMSInternalVariable( "Inlet Humidity Ratio for Air Connection", UserPlantComp( CompLoop ).Name, "[kgWater/kgDryAir]", UserPlantComp( CompLoop ).Air.InletHumRat );
					SetupEMSInternalVariable( "Inlet Density for Air Connection", UserPlantComp( CompLoop ).Name, "[kg/m3]", UserPlantComp( CompLoop ).Air.InletRho );
					SetupEMSInternalVariable( "Inlet Specific Heat for Air Connection", UserPlantComp( CompLoop ).Name, "[J/kg-C]", UserPlantComp( CompLoop ).Air.InletCp );
				}

				if ( ! lAlphaFieldBlanks( 28 ) ) {
					UserPlantComp( CompLoop ).Air.OutletNodeNum = GetOnlySingleNode( cAlphaArgs( 28 ), ErrorsFound, cCurrentModuleObject, UserPlantComp( CompLoop ).Name, NodeType_Air, NodeConnectionType_ReliefAir, 1, ObjectIsNotParent );
					//outlet air node results
					SetupEMSActuator( "Air Connection", UserPlantComp( CompLoop ).Name, "Outlet Temperature", "[C]", lDummy, UserPlantComp( CompLoop ).Air.OutletTemp );
					SetupEMSActuator( "Air Connection", UserPlantComp( CompLoop ).Name, "Outlet Humidity Ratio", "[kgWater/kgDryAir]", lDummy, UserPlantComp( CompLoop ).Air.OutletHumRat );
					SetupEMSActuator( "Air Connection", UserPlantComp( CompLoop ).Name, "Mass Flow Rate", "[kg/s]", lDummy, UserPlantComp( CompLoop ).Air.OutletMassFlowRate );
				}

				if ( ! lAlphaFieldBlanks( 29 ) ) {
					SetupTankDemandComponent( cAlphaArgs( 1 ), cCurrentModuleObject, cAlphaArgs( 29 ), ErrorsFound, UserPlantComp( CompLoop ).Water.SupplyTankID, UserPlantComp( CompLoop ).Water.SupplyTankDemandARRID );

					UserPlantComp( CompLoop ).Water.SuppliedByWaterSystem = true;
					SetupEMSActuator( "Water System", UserPlantComp( CompLoop ).Name, "Supplied Volume Flow Rate", "[m3/s]", lDummy, UserPlantComp( CompLoop ).Water.SupplyVdotRequest );
				}

				if ( ! lAlphaFieldBlanks( 30 ) ) {
					SetupTankSupplyComponent( cAlphaArgs( 1 ), cCurrentModuleObject, cAlphaArgs( 30 ), ErrorsFound, UserPlantComp( CompLoop ).Water.CollectionTankID, UserPlantComp( CompLoop ).Water.CollectionTankSupplyARRID );
					UserPlantComp( CompLoop ).Water.CollectsToWaterSystem = true;
					SetupEMSActuator( "Water System", UserPlantComp( CompLoop ).Name, "Collected Volume Flow Rate", "[m3/s]", lDummy, UserPlantComp( CompLoop ).Water.CollectedVdot );
				}

				if ( ! lAlphaFieldBlanks( 31 ) ) {

					UserPlantComp( CompLoop ).Zone.ZoneNum = FindItemInList( cAlphaArgs( 31 ), Zone );
					if ( UserPlantComp( CompLoop ).Zone.ZoneNum == 0 ) {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Ambient Zone Name not found = " + cAlphaArgs( 31 ) );
						ErrorsFound = true;
					} else {
						UserPlantComp( CompLoop ).Zone.DeviceHasInternalGains = true;
						SetupZoneInternalGain( UserPlantComp( CompLoop ).Zone.ZoneNum, cCurrentModuleObject, cAlphaArgs( 1 ), IntGainTypeOf_PlantComponentUserDefined, UserPlantComp( CompLoop ).Zone.ConvectionGainRate, UserPlantComp( CompLoop ).Zone.ReturnAirConvectionGainRate, UserPlantComp( CompLoop ).Zone.ThermalRadiationGainRate, UserPlantComp( CompLoop ).Zone.LatentGainRate, UserPlantComp( CompLoop ).Zone.ReturnAirLatentGainRate, UserPlantComp( CompLoop ).Zone.CarbonDioxideGainRate, UserPlantComp( CompLoop ).Zone.GenericContamGainRate );

						SetupEMSActuator( "Component Zone Internal Gain", UserPlantComp( CompLoop ).Name, "Sensible Heat Gain Rate", "[W]", lDummy, UserPlantComp( CompLoop ).Zone.ConvectionGainRate );
						SetupEMSActuator( "Component Zone Internal Gain", UserPlantComp( CompLoop ).Name, "Return Air Heat Sensible Gain Rate", "[W]", lDummy, UserPlantComp( CompLoop ).Zone.ReturnAirConvectionGainRate );
						SetupEMSActuator( "Component Zone Internal Gain", UserPlantComp( CompLoop ).Name, "Thermal Radiation Heat Gain Rate", "[W]", lDummy, UserPlantComp( CompLoop ).Zone.ThermalRadiationGainRate );
						SetupEMSActuator( "Component Zone Internal Gain", UserPlantComp( CompLoop ).Name, "Latent Heat Gain Rate", "[W]", lDummy, UserPlantComp( CompLoop ).Zone.LatentGainRate );
						SetupEMSActuator( "Component Zone Internal Gain", UserPlantComp( CompLoop ).Name, "Return Air Latent Heat Gain Rate", "[W]", lDummy, UserPlantComp( CompLoop ).Zone.ReturnAirLatentGainRate );
						SetupEMSActuator( "Component Zone Internal Gain", UserPlantComp( CompLoop ).Name, "Carbon Dioxide Gain Rate", "[W]", lDummy, UserPlantComp( CompLoop ).Zone.CarbonDioxideGainRate );
						SetupEMSActuator( "Component Zone Internal Gain", UserPlantComp( CompLoop ).Name, "Gaseous Contaminant Gain Rate", "[W]", lDummy, UserPlantComp( CompLoop ).Zone.GenericContamGainRate );
					}
				}

				// make sure user has entered at least some erl program managers to actually calculate something
				MgrCountTest = 0;
				if ( UserPlantComp( CompLoop ).ErlSimProgramMngr > 0 ) MgrCountTest = 1;
				for ( ConnectionLoop = 1; ConnectionLoop <= NumPlantConnections; ++ConnectionLoop ) {
					if ( UserPlantComp( CompLoop ).Loop( ConnectionLoop ).ErlInitProgramMngr > 0 ) ++MgrCountTest;
					if ( UserPlantComp( CompLoop ).Loop( ConnectionLoop ).ErlSimProgramMngr > 0 ) ++MgrCountTest;
				}
				if ( MgrCountTest == 0 ) {
					ShowSevereError( "Invalid " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ShowContinueError( "At least one program calling manager is needed." );
					ErrorsFound = true;
				}

			}
		} //NumUserPlantComps > 0

		if ( ErrorsFound ) {
			ShowFatalError( "GetUserDefinedComponents: Errors found in processing " + cCurrentModuleObject + " input." );
		}

		cCurrentModuleObject = "Coil:UserDefined";
		NumUserCoils = GetNumObjectsFound( cCurrentModuleObject );
		if ( NumUserCoils > 0 ) {
			UserCoil.allocate( NumUserCoils );
			CheckUserCoilName.dimension( NumUserCoils, true );
			for ( CompLoop = 1; CompLoop <= NumUserCoils; ++CompLoop ) {
				GetObjectItem( cCurrentModuleObject, CompLoop, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				IsNotOK = false;
				IsBlank = false;
				VerifyName( cAlphaArgs( 1 ), UserCoil, CompLoop - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
				}
				VerifyUniqueCoilName( cCurrentModuleObject, cAlphaArgs( 1 ), errFlag, cCurrentModuleObject + " Name" );
				if ( errFlag ) {
					ErrorsFound = true;
				}
				UserCoil( CompLoop ).Name = cAlphaArgs( 1 );

				// now get program manager for model simulations
				if ( ! lAlphaFieldBlanks( 2 ) ) {
					StackMngrNum = FindItemInList( cAlphaArgs( 2 ), EMSProgramCallManager );
					if ( StackMngrNum > 0 ) { // found it
						UserCoil( CompLoop ).ErlSimProgramMngr = StackMngrNum;
					} else {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ShowContinueError( "Program Manager Name not found." );
						ErrorsFound = true;
					}
				}

				// now get program manager for model initializations
				if ( ! lAlphaFieldBlanks( 3 ) ) {
					StackMngrNum = FindItemInList( cAlphaArgs( 3 ), EMSProgramCallManager );
					if ( StackMngrNum > 0 ) { // found it
						UserCoil( CompLoop ).ErlInitProgramMngr = StackMngrNum;
					} else {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 3 ) + '=' + cAlphaArgs( 3 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ShowContinueError( "Program Manager Name not found." );
						ErrorsFound = true;
					}
				}

				NumAirConnections = std::floor( rNumericArgs( 1 ) );
				if ( ( NumAirConnections >= 1 ) && ( NumAirConnections <= 2 ) ) {
					UserCoil( CompLoop ).Air.allocate( NumAirConnections );
					UserCoil( CompLoop ).NumAirConnections = NumAirConnections;
					for ( ConnectionLoop = 1; ConnectionLoop <= NumAirConnections; ++ConnectionLoop ) {
						aArgCount = ( ConnectionLoop - 1 ) * 2 + 4;
						UserCoil( CompLoop ).Air( ConnectionLoop ).InletNodeNum = GetOnlySingleNode( cAlphaArgs( aArgCount ), ErrorsFound, cCurrentModuleObject, UserCoil( CompLoop ).Name, NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

						LoopStr = RoundSigDigits( ConnectionLoop );
						//model input related internal variables
						SetupEMSInternalVariable( "Inlet Temperature for Air Connection " + LoopStr, UserCoil( CompLoop ).Name, "[C]", UserCoil( CompLoop ).Air( ConnectionLoop ).InletTemp );
						SetupEMSInternalVariable( "Inlet Mass Flow Rate for Air Connection " + LoopStr, UserCoil( CompLoop ).Name, "[kg/s]", UserCoil( CompLoop ).Air( ConnectionLoop ).InletMassFlowRate );
						SetupEMSInternalVariable( "Inlet Humidity Ratio for Air Connection " + LoopStr, UserCoil( CompLoop ).Name, "[kgWater/kgDryAir]", UserCoil( CompLoop ).Air( ConnectionLoop ).InletHumRat );
						SetupEMSInternalVariable( "Inlet Density for Air Connection " + LoopStr, UserCoil( CompLoop ).Name, "[kg/m3]", UserCoil( CompLoop ).Air( ConnectionLoop ).InletRho );
						SetupEMSInternalVariable( "Inlet Specific Heat for Air Connection " + LoopStr, UserCoil( CompLoop ).Name, "[J/kg-C]", UserCoil( CompLoop ).Air( ConnectionLoop ).InletCp );

						UserCoil( CompLoop ).Air( ConnectionLoop ).OutletNodeNum = GetOnlySingleNode( cAlphaArgs( aArgCount + 1 ), ErrorsFound, cCurrentModuleObject, UserCoil( CompLoop ).Name, NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
						SetupEMSActuator( "Air Connection " + LoopStr, UserCoil( CompLoop ).Name, "Outlet Temperature", "[C]", lDummy, UserCoil( CompLoop ).Air( ConnectionLoop ).OutletTemp );
						SetupEMSActuator( "Air Connection " + LoopStr, UserCoil( CompLoop ).Name, "Outlet Humidity Ratio", "[kgWater/kgDryAir]", lDummy, UserCoil( CompLoop ).Air( ConnectionLoop ).OutletHumRat );
						SetupEMSActuator( "Air Connection " + LoopStr, UserCoil( CompLoop ).Name, "Mass Flow Rate", "[kg/s]", lDummy, UserCoil( CompLoop ).Air( ConnectionLoop ).OutletMassFlowRate );

						TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( aArgCount ), cAlphaArgs( aArgCount + 1 ), "Air Nodes " + LoopStr );

					}

					if ( ! lAlphaFieldBlanks( 8 ) ) {
						{ auto const SELECT_CASE_var( cAlphaArgs( 8 ) );

						if ( SELECT_CASE_var == "YES" ) {
							UserCoil( CompLoop ).PlantIsConnected = true;
						} else if ( SELECT_CASE_var == "NO" ) {
							UserCoil( CompLoop ).PlantIsConnected = false;
						}}

					} else {
						UserCoil( CompLoop ).PlantIsConnected = false;
					}

					if ( UserCoil( CompLoop ).PlantIsConnected ) { // get input
						UserCoil( CompLoop ).Loop.InletNodeNum = GetOnlySingleNode( cAlphaArgs( 9 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 2, ObjectIsNotParent );
						UserCoil( CompLoop ).Loop.OutletNodeNum = GetOnlySingleNode( cAlphaArgs( 10 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 2, ObjectIsNotParent );

						TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 9 ), cAlphaArgs( 10 ), "Plant Nodes" );

						// this model is only for plant connections that are "Demand"
						UserCoil( CompLoop ).Loop.HowLoadServed = HowMet_NoneDemand;
						// this model is only for plant connections that are needy and turn loop on
						UserCoil( CompLoop ).Loop.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;

						//Setup Internal Variables
						//model input related internal variables
						SetupEMSInternalVariable( "Inlet Temperature for Plant Connection", UserCoil( CompLoop ).Name, "[C]", UserCoil( CompLoop ).Loop.InletTemp );
						SetupEMSInternalVariable( "Inlet Mass Flow Rate for Plant Connection", UserCoil( CompLoop ).Name, "[kg/s]", UserCoil( CompLoop ).Loop.InletMassFlowRate );
						SetupEMSInternalVariable( "Inlet Density for Plant Connection", UserCoil( CompLoop ).Name, "[kg/m3]", UserCoil( CompLoop ).Loop.InletRho );
						SetupEMSInternalVariable( "Inlet Specific Heat for Plant Connection", UserCoil( CompLoop ).Name, "[J/kg-C]", UserCoil( CompLoop ).Loop.InletCp );
						// model results related actuators
						SetupEMSActuator( "Plant Connection", UserCoil( CompLoop ).Name, "Outlet Temperature", "[C]", lDummy, UserCoil( CompLoop ).Loop.OutletTemp );
						SetupEMSActuator( "Plant Connection", UserCoil( CompLoop ).Name, "Mass Flow Rate", "[kg/s]", lDummy, UserCoil( CompLoop ).Loop.MassFlowRateRequest );
						// model initialization and sizing related actuators
						SetupEMSActuator( "Plant Connection", UserCoil( CompLoop ).Name, "Design Volume Flow Rate", "[m3/s]", lDummy, UserCoil( CompLoop ).Loop.DesignVolumeFlowRate );

						SetupEMSActuator( "Plant Connection", UserCoil( CompLoop ).Name, "Minimum Mass Flow Rate", "[kg/s]", lDummy, UserCoil( CompLoop ).Loop.MassFlowRateMin );
						SetupEMSActuator( "Plant Connection", UserCoil( CompLoop ).Name, "Maximum Mass Flow Rate", "[kg/s]", lDummy, UserCoil( CompLoop ).Loop.MassFlowRateMax );
					}

					if ( ! lAlphaFieldBlanks( 11 ) ) {
						SetupTankDemandComponent( cAlphaArgs( 1 ), cCurrentModuleObject, cAlphaArgs( 11 ), ErrorsFound, UserCoil( CompLoop ).Water.SupplyTankID, UserCoil( CompLoop ).Water.SupplyTankDemandARRID );

						UserCoil( CompLoop ).Water.SuppliedByWaterSystem = true;
						SetupEMSActuator( "Water System", UserCoil( CompLoop ).Name, "Supplied Volume Flow Rate", "[m3/s]", lDummy, UserCoil( CompLoop ).Water.SupplyVdotRequest );
					}

					if ( ! lAlphaFieldBlanks( 12 ) ) {
						SetupTankSupplyComponent( cAlphaArgs( 1 ), cCurrentModuleObject, cAlphaArgs( 12 ), ErrorsFound, UserCoil( CompLoop ).Water.CollectionTankID, UserCoil( CompLoop ).Water.CollectionTankSupplyARRID );
						UserCoil( CompLoop ).Water.CollectsToWaterSystem = true;
						SetupEMSActuator( "Water System", UserCoil( CompLoop ).Name, "Collected Volume Flow Rate", "[m3/s]", lDummy, UserCoil( CompLoop ).Water.CollectedVdot );
					}

					if ( ! lAlphaFieldBlanks( 13 ) ) {

						UserCoil( CompLoop ).Zone.ZoneNum = FindItemInList( cAlphaArgs( 13 ), Zone );
						if ( UserCoil( CompLoop ).Zone.ZoneNum == 0 ) {
							ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Ambient Zone Name not found = " + cAlphaArgs( 13 ) );
							ErrorsFound = true;
						} else {
							UserCoil( CompLoop ).Zone.DeviceHasInternalGains = true;
							SetupZoneInternalGain( UserCoil( CompLoop ).Zone.ZoneNum, cCurrentModuleObject, cAlphaArgs( 1 ), IntGainTypeOf_CoilUserDefined, UserCoil( CompLoop ).Zone.ConvectionGainRate, UserCoil( CompLoop ).Zone.ReturnAirConvectionGainRate, UserCoil( CompLoop ).Zone.ThermalRadiationGainRate, UserCoil( CompLoop ).Zone.LatentGainRate, UserCoil( CompLoop ).Zone.ReturnAirLatentGainRate, UserCoil( CompLoop ).Zone.CarbonDioxideGainRate, UserCoil( CompLoop ).Zone.GenericContamGainRate );

							SetupEMSActuator( "Component Zone Internal Gain", UserCoil( CompLoop ).Name, "Sensible Heat Gain Rate", "[W]", lDummy, UserCoil( CompLoop ).Zone.ConvectionGainRate );
							SetupEMSActuator( "Component Zone Internal Gain", UserCoil( CompLoop ).Name, "Return Air Heat Sensible Gain Rate", "[W]", lDummy, UserCoil( CompLoop ).Zone.ReturnAirConvectionGainRate );
							SetupEMSActuator( "Component Zone Internal Gain", UserCoil( CompLoop ).Name, "Thermal Radiation Heat Gain Rate", "[W]", lDummy, UserCoil( CompLoop ).Zone.ThermalRadiationGainRate );
							SetupEMSActuator( "Component Zone Internal Gain", UserCoil( CompLoop ).Name, "Latent Heat Gain Rate", "[W]", lDummy, UserCoil( CompLoop ).Zone.LatentGainRate );
							SetupEMSActuator( "Component Zone Internal Gain", UserCoil( CompLoop ).Name, "Return Air Latent Heat Gain Rate", "[W]", lDummy, UserCoil( CompLoop ).Zone.ReturnAirLatentGainRate );
							SetupEMSActuator( "Component Zone Internal Gain", UserCoil( CompLoop ).Name, "Carbon Dioxide Gain Rate", "[W]", lDummy, UserCoil( CompLoop ).Zone.CarbonDioxideGainRate );
							SetupEMSActuator( "Component Zone Internal Gain", UserCoil( CompLoop ).Name, "Gaseous Contaminant Gain Rate", "[W]", lDummy, UserCoil( CompLoop ).Zone.GenericContamGainRate );
						}
					}

				}
			}

		} //NumUserCoils > 0

		if ( ErrorsFound ) {
			ShowFatalError( "GetUserDefinedComponents: Errors found in processing " + cCurrentModuleObject + " input." );
		}

		cCurrentModuleObject = "ZoneHVAC:ForcedAir:UserDefined";
		NumUserZoneAir = GetNumObjectsFound( cCurrentModuleObject );
		if ( NumUserZoneAir > 0 ) {
			UserZoneAirHVAC.allocate( NumUserZoneAir );
			CheckUserZoneAirName.dimension( NumUserZoneAir, true );
			for ( CompLoop = 1; CompLoop <= NumUserZoneAir; ++CompLoop ) {
				GetObjectItem( cCurrentModuleObject, CompLoop, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				IsNotOK = false;
				IsBlank = false;
				VerifyName( cAlphaArgs( 1 ), UserZoneAirHVAC, CompLoop - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
				}
				UserZoneAirHVAC( CompLoop ).Name = cAlphaArgs( 1 );

				// now get program manager for model simulations
				if ( ! lAlphaFieldBlanks( 2 ) ) {
					StackMngrNum = FindItemInList( cAlphaArgs( 2 ), EMSProgramCallManager );
					if ( StackMngrNum > 0 ) { // found it
						UserZoneAirHVAC( CompLoop ).ErlSimProgramMngr = StackMngrNum;
					} else {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ShowContinueError( "Program Manager Name not found." );
						ErrorsFound = true;
					}
				}

				// now get program manager for model initializations
				if ( ! lAlphaFieldBlanks( 3 ) ) {
					StackMngrNum = FindItemInList( cAlphaArgs( 3 ), EMSProgramCallManager );
					if ( StackMngrNum > 0 ) { // found it
						UserZoneAirHVAC( CompLoop ).ErlInitProgramMngr = StackMngrNum;
					} else {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 3 ) + '=' + cAlphaArgs( 3 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ShowContinueError( "Program Manager Name not found." );
						ErrorsFound = true;
					}
				}

				UserZoneAirHVAC( CompLoop ).ZoneAir.InletNodeNum = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, UserZoneAirHVAC( CompLoop ).Name, NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
				//model input related internal variables
				SetupEMSInternalVariable( "Inlet Temperature for Primary Air Connection", UserZoneAirHVAC( CompLoop ).Name, "[C]", UserZoneAirHVAC( CompLoop ).ZoneAir.InletTemp );
				SetupEMSInternalVariable( "Inlet Humidity Ratio for Primary Air Connection", UserZoneAirHVAC( CompLoop ).Name, "[kgWater/kgDryAir]", UserZoneAirHVAC( CompLoop ).ZoneAir.InletHumRat );
				SetupEMSInternalVariable( "Inlet Density for Primary Air Connection", UserZoneAirHVAC( CompLoop ).Name, "[kg/m3]", UserZoneAirHVAC( CompLoop ).ZoneAir.InletRho );
				SetupEMSInternalVariable( "Inlet Specific Heat for Primary Air Connection", UserZoneAirHVAC( CompLoop ).Name, "[J/kg-C]", UserZoneAirHVAC( CompLoop ).ZoneAir.InletCp );

				SetupEMSInternalVariable( "Remaining Sensible Load to Heating Setpoint", UserZoneAirHVAC( CompLoop ).Name, "[W]", UserZoneAirHVAC( CompLoop ).RemainingOutputToHeatingSP );
				SetupEMSInternalVariable( "Remaining Sensible Load to Cooling Setpoint", UserZoneAirHVAC( CompLoop ).Name, "[W]", UserZoneAirHVAC( CompLoop ).RemainingOutputToCoolingSP );
				SetupEMSInternalVariable( "Remaining Latent Load to Humidifying Setpoint", UserZoneAirHVAC( CompLoop ).Name, "[kg/s]", UserZoneAirHVAC( CompLoop ).RemainingOutputReqToHumidSP );
				SetupEMSInternalVariable( "Remaining Latent Load to Dehumidifying Setpoint", UserZoneAirHVAC( CompLoop ).Name, "[kg/s]", UserZoneAirHVAC( CompLoop ).RemainingOutputReqToDehumidSP );

				SetupEMSActuator( "Primary Air Connection", UserZoneAirHVAC( CompLoop ).Name, "Inlet Mass Flow Rate", "[kg/s]", lDummy, UserZoneAirHVAC( CompLoop ).ZoneAir.InletMassFlowRate );
				UserZoneAirHVAC( CompLoop ).ZoneAir.OutletNodeNum = GetOnlySingleNode( cAlphaArgs( 5 ), ErrorsFound, cCurrentModuleObject, UserZoneAirHVAC( CompLoop ).Name, NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
				SetupEMSActuator( "Primary Air Connection", UserZoneAirHVAC( CompLoop ).Name, "Outlet Temperature", "[C]", lDummy, UserZoneAirHVAC( CompLoop ).ZoneAir.OutletTemp );
				SetupEMSActuator( "Primary Air Connection", UserZoneAirHVAC( CompLoop ).Name, "Outlet Humidity Ratio", "[kgWater/kgDryAir]", lDummy, UserZoneAirHVAC( CompLoop ).ZoneAir.OutletHumRat );
				SetupEMSActuator( "Primary Air Connection", UserZoneAirHVAC( CompLoop ).Name, "Outlet Mass Flow Rate", "[kg/s]", lDummy, UserZoneAirHVAC( CompLoop ).ZoneAir.OutletMassFlowRate );

				if ( ! lAlphaFieldBlanks( 6 ) ) {
					UserZoneAirHVAC( CompLoop ).SourceAir.InletNodeNum = GetOnlySingleNode( cAlphaArgs( 6 ), ErrorsFound, cCurrentModuleObject, UserZoneAirHVAC( CompLoop ).Name, NodeType_Air, NodeConnectionType_Inlet, 2, ObjectIsNotParent );
					//model input related internal variables
					SetupEMSInternalVariable( "Inlet Temperature for Secondary Air Connection", UserZoneAirHVAC( CompLoop ).Name, "[C]", UserZoneAirHVAC( CompLoop ).SourceAir.InletTemp );

					SetupEMSInternalVariable( "Inlet Humidity Ratio for Secondary Air Connection", UserZoneAirHVAC( CompLoop ).Name, "[kgWater/kgDryAir]", UserZoneAirHVAC( CompLoop ).SourceAir.InletHumRat );
					SetupEMSInternalVariable( "Inlet Density for Secondary Air Connection", UserZoneAirHVAC( CompLoop ).Name, "[kg/m3]", UserZoneAirHVAC( CompLoop ).SourceAir.InletRho );
					SetupEMSInternalVariable( "Inlet Specific Heat for Secondary Air Connection", UserZoneAirHVAC( CompLoop ).Name, "[J/kg-C]", UserZoneAirHVAC( CompLoop ).SourceAir.InletCp );
					SetupEMSActuator( "Secondary Air Connection", UserZoneAirHVAC( CompLoop ).Name, "Inlet Mass Flow Rate", "[kg/s]", lDummy, UserZoneAirHVAC( CompLoop ).SourceAir.InletMassFlowRate );
				}

				if ( ! lAlphaFieldBlanks( 7 ) ) {
					UserZoneAirHVAC( CompLoop ).SourceAir.OutletNodeNum = GetOnlySingleNode( cAlphaArgs( 7 ), ErrorsFound, cCurrentModuleObject, UserZoneAirHVAC( CompLoop ).Name, NodeType_Air, NodeConnectionType_Outlet, 2, ObjectIsNotParent );
					SetupEMSActuator( "Secondary Air Connection", UserZoneAirHVAC( CompLoop ).Name, "Outlet Temperature", "[C]", lDummy, UserZoneAirHVAC( CompLoop ).SourceAir.OutletTemp );
					SetupEMSActuator( "Secondary Air Connection", UserZoneAirHVAC( CompLoop ).Name, "Outlet Humidity Ratio", "[kgWater/kgDryAir]", lDummy, UserZoneAirHVAC( CompLoop ).SourceAir.OutletHumRat );
					SetupEMSActuator( "Secondary Air Connection", UserZoneAirHVAC( CompLoop ).Name, "Mass Flow Rate", "[kg/s]", lDummy, UserZoneAirHVAC( CompLoop ).SourceAir.OutletMassFlowRate );
				}

				if ( ( UserZoneAirHVAC( CompLoop ).SourceAir.InletNodeNum > 0 ) && ( UserZoneAirHVAC( CompLoop ).SourceAir.OutletNodeNum > 0 ) ) {
					//  CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(6),cAlphaArgs(7),'Air Nodes')
				}

				NumPlantConnections = std::floor( rNumericArgs( 1 ) );
				UserZoneAirHVAC( CompLoop ).NumPlantConnections = NumPlantConnections;
				if ( ( NumPlantConnections >= 1 ) && ( NumPlantConnections <= 3 ) ) {
					UserZoneAirHVAC( CompLoop ).Loop.allocate( NumPlantConnections );
					for ( ConnectionLoop = 1; ConnectionLoop <= NumPlantConnections; ++ConnectionLoop ) {
						aArgCount = ( ConnectionLoop - 1 ) * 2 + 8;
						UserZoneAirHVAC( CompLoop ).Loop( ConnectionLoop ).InletNodeNum = GetOnlySingleNode( cAlphaArgs( aArgCount ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, ( ConnectionLoop + 2 ), ObjectIsNotParent );
						UserZoneAirHVAC( CompLoop ).Loop( ConnectionLoop ).OutletNodeNum = GetOnlySingleNode( cAlphaArgs( aArgCount + 1 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, ( ConnectionLoop + 2 ), ObjectIsNotParent );
						TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( aArgCount ), cAlphaArgs( aArgCount + 1 ), "Plant Nodes" );
						UserZoneAirHVAC( CompLoop ).Loop( ConnectionLoop ).HowLoadServed = HowMet_NoneDemand;
						UserZoneAirHVAC( CompLoop ).Loop( ConnectionLoop ).FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
						//Setup Internal Variables
						gio::write( LoopStr, fmtLD ) << ConnectionLoop;
						strip( LoopStr );
						//model input related internal variables
						SetupEMSInternalVariable( "Inlet Temperature for Plant Connection " + LoopStr, UserZoneAirHVAC( CompLoop ).Name, "[C]", UserZoneAirHVAC( CompLoop ).Loop( ConnectionLoop ).InletTemp );
						SetupEMSInternalVariable( "Inlet Mass Flow Rate for Plant Connection " + LoopStr, UserZoneAirHVAC( CompLoop ).Name, "[kg/s]", UserZoneAirHVAC( CompLoop ).Loop( ConnectionLoop ).InletMassFlowRate );
						SetupEMSInternalVariable( "Inlet Density for Plant Connection " + LoopStr, UserZoneAirHVAC( CompLoop ).Name, "[kg/m3]", UserZoneAirHVAC( CompLoop ).Loop( ConnectionLoop ).InletRho );
						SetupEMSInternalVariable( "Inlet Specific Heat for Plant Connection " + LoopStr, UserZoneAirHVAC( CompLoop ).Name, "[J/kg-C]", UserZoneAirHVAC( CompLoop ).Loop( ConnectionLoop ).InletCp );
						// model results related actuators
						SetupEMSActuator( "Plant Connection " + LoopStr, UserZoneAirHVAC( CompLoop ).Name, "Outlet Temperature", "[C]", lDummy, UserZoneAirHVAC( CompLoop ).Loop( ConnectionLoop ).OutletTemp );
						SetupEMSActuator( "Plant Connection " + LoopStr, UserZoneAirHVAC( CompLoop ).Name, "Mass Flow Rate", "[kg/s]", lDummy, UserZoneAirHVAC( CompLoop ).Loop( ConnectionLoop ).MassFlowRateRequest );
						// model initialization and sizing related actuators
						SetupEMSActuator( "Plant Connection " + LoopStr, UserZoneAirHVAC( CompLoop ).Name, "Minimum Mass Flow Rate", "[kg/s]", lDummy, UserZoneAirHVAC( CompLoop ).Loop( ConnectionLoop ).MassFlowRateMin );
						SetupEMSActuator( "Plant Connection " + LoopStr, UserZoneAirHVAC( CompLoop ).Name, "Maximum Mass Flow Rate", "[kg/s]", lDummy, UserZoneAirHVAC( CompLoop ).Loop( ConnectionLoop ).MassFlowRateMax );
						SetupEMSActuator( "Plant Connection " + LoopStr, UserZoneAirHVAC( CompLoop ).Name, "Design Volume Flow Rate", "[m3/s]", lDummy, UserZoneAirHVAC( CompLoop ).Loop( ConnectionLoop ).DesignVolumeFlowRate );

					}
				}

				if ( ! lAlphaFieldBlanks( 14 ) ) {
					SetupTankDemandComponent( cAlphaArgs( 1 ), cCurrentModuleObject, cAlphaArgs( 14 ), ErrorsFound, UserZoneAirHVAC( CompLoop ).Water.SupplyTankID, UserZoneAirHVAC( CompLoop ).Water.SupplyTankDemandARRID );

					UserZoneAirHVAC( CompLoop ).Water.SuppliedByWaterSystem = true;
					SetupEMSActuator( "Water System", UserZoneAirHVAC( CompLoop ).Name, "Supplied Volume Flow Rate", "[m3/s]", lDummy, UserZoneAirHVAC( CompLoop ).Water.SupplyVdotRequest );
				}

				if ( ! lAlphaFieldBlanks( 15 ) ) {
					SetupTankSupplyComponent( cAlphaArgs( 1 ), cCurrentModuleObject, cAlphaArgs( 15 ), ErrorsFound, UserZoneAirHVAC( CompLoop ).Water.CollectionTankID, UserZoneAirHVAC( CompLoop ).Water.CollectionTankSupplyARRID );
					UserZoneAirHVAC( CompLoop ).Water.CollectsToWaterSystem = true;
					SetupEMSActuator( "Water System", UserZoneAirHVAC( CompLoop ).Name, "Collected Volume Flow Rate", "[m3/s]", lDummy, UserZoneAirHVAC( CompLoop ).Water.CollectedVdot );
				}

				if ( ! lAlphaFieldBlanks( 16 ) ) {

					UserZoneAirHVAC( CompLoop ).Zone.ZoneNum = FindItemInList( cAlphaArgs( 16 ), Zone );
					if ( UserZoneAirHVAC( CompLoop ).Zone.ZoneNum == 0 ) {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Ambient Zone Name not found = " + cAlphaArgs( 16 ) );
						ErrorsFound = true;
					} else {
						UserZoneAirHVAC( CompLoop ).Zone.DeviceHasInternalGains = true;
						SetupZoneInternalGain( UserZoneAirHVAC( CompLoop ).Zone.ZoneNum, cCurrentModuleObject, cAlphaArgs( 1 ), IntGainTypeOf_ZoneHVACForcedAirUserDefined, UserZoneAirHVAC( CompLoop ).Zone.ConvectionGainRate, UserZoneAirHVAC( CompLoop ).Zone.ReturnAirConvectionGainRate, UserZoneAirHVAC( CompLoop ).Zone.ThermalRadiationGainRate, UserZoneAirHVAC( CompLoop ).Zone.LatentGainRate, UserZoneAirHVAC( CompLoop ).Zone.ReturnAirLatentGainRate, UserZoneAirHVAC( CompLoop ).Zone.CarbonDioxideGainRate, UserZoneAirHVAC( CompLoop ).Zone.GenericContamGainRate );

						SetupEMSActuator( "Component Zone Internal Gain", UserZoneAirHVAC( CompLoop ).Name, "Sensible Heat Gain Rate", "[W]", lDummy, UserZoneAirHVAC( CompLoop ).Zone.ConvectionGainRate );
						SetupEMSActuator( "Component Zone Internal Gain", UserZoneAirHVAC( CompLoop ).Name, "Return Air Heat Sensible Gain Rate", "[W]", lDummy, UserZoneAirHVAC( CompLoop ).Zone.ReturnAirConvectionGainRate );
						SetupEMSActuator( "Component Zone Internal Gain", UserZoneAirHVAC( CompLoop ).Name, "Thermal Radiation Heat Gain Rate", "[W]", lDummy, UserZoneAirHVAC( CompLoop ).Zone.ThermalRadiationGainRate );
						SetupEMSActuator( "Component Zone Internal Gain", UserZoneAirHVAC( CompLoop ).Name, "Latent Heat Gain Rate", "[W]", lDummy, UserZoneAirHVAC( CompLoop ).Zone.LatentGainRate );
						SetupEMSActuator( "Component Zone Internal Gain", UserZoneAirHVAC( CompLoop ).Name, "Return Air Latent Heat Gain Rate", "[W]", lDummy, UserZoneAirHVAC( CompLoop ).Zone.ReturnAirLatentGainRate );
						SetupEMSActuator( "Component Zone Internal Gain", UserZoneAirHVAC( CompLoop ).Name, "Carbon Dioxide Gain Rate", "[m3/s]", lDummy, UserZoneAirHVAC( CompLoop ).Zone.CarbonDioxideGainRate );
						SetupEMSActuator( "Component Zone Internal Gain", UserZoneAirHVAC( CompLoop ).Name, "Gaseous Contaminant Gain Rate", "[m3/s]", lDummy, UserZoneAirHVAC( CompLoop ).Zone.GenericContamGainRate );
					}
				}

			}
		} //NumUserZoneAir > 0

		if ( ErrorsFound ) {
			ShowFatalError( "GetUserDefinedComponents: Errors found in processing " + cCurrentModuleObject + " input." );
		}

		cCurrentModuleObject = "AirTerminal:SingleDuct:UserDefined";
		NumUserAirTerminals = GetNumObjectsFound( cCurrentModuleObject );
		if ( NumUserAirTerminals > 0 ) {
			UserAirTerminal.allocate( NumUserAirTerminals );
			CheckUserAirTerminal.dimension( NumUserAirTerminals, true );
			for ( CompLoop = 1; CompLoop <= NumUserAirTerminals; ++CompLoop ) {
				GetObjectItem( cCurrentModuleObject, CompLoop, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				IsNotOK = false;
				IsBlank = false;
				VerifyName( cAlphaArgs( 1 ), UserAirTerminal, CompLoop - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
				}
				UserAirTerminal( CompLoop ).Name = cAlphaArgs( 1 );

				// now get program manager for model simulations
				if ( ! lAlphaFieldBlanks( 2 ) ) {
					StackMngrNum = FindItemInList( cAlphaArgs( 2 ), EMSProgramCallManager );
					if ( StackMngrNum > 0 ) { // found it
						UserAirTerminal( CompLoop ).ErlSimProgramMngr = StackMngrNum;
					} else {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ShowContinueError( "Program Manager Name not found." );
						ErrorsFound = true;
					}
				}

				// now get program manager for model initializations
				if ( ! lAlphaFieldBlanks( 3 ) ) {
					StackMngrNum = FindItemInList( cAlphaArgs( 3 ), EMSProgramCallManager );
					if ( StackMngrNum > 0 ) { // found it
						UserAirTerminal( CompLoop ).ErlInitProgramMngr = StackMngrNum;
					} else {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 3 ) + '=' + cAlphaArgs( 3 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ShowContinueError( "Program Manager Name not found." );
						ErrorsFound = true;
					}
				}

				UserAirTerminal( CompLoop ).AirLoop.InletNodeNum = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, UserAirTerminal( CompLoop ).Name, NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent, cAlphaFieldNames( 4 ) );
				//model input related internal variables
				SetupEMSInternalVariable( "Inlet Temperature for Primary Air Connection", UserAirTerminal( CompLoop ).Name, "[C]", UserAirTerminal( CompLoop ).AirLoop.InletTemp );
				SetupEMSInternalVariable( "Inlet Humidity Ratio for Primary Air Connection", UserAirTerminal( CompLoop ).Name, "[kgWater/kgDryAir]", UserAirTerminal( CompLoop ).AirLoop.InletHumRat );
				SetupEMSInternalVariable( "Inlet Density for Primary Air Connection", UserAirTerminal( CompLoop ).Name, "[kg/m3]", UserAirTerminal( CompLoop ).AirLoop.InletRho );
				SetupEMSInternalVariable( "Inlet Specific Heat for Primary Air Connection", UserAirTerminal( CompLoop ).Name, "[J/kg-C]", UserAirTerminal( CompLoop ).AirLoop.InletCp );

				SetupEMSInternalVariable( "Remaining Sensible Load to Heating Setpoint", UserAirTerminal( CompLoop ).Name, "[W]", UserAirTerminal( CompLoop ).RemainingOutputToHeatingSP );
				SetupEMSInternalVariable( "Remaining Sensible Load to Cooling Setpoint", UserAirTerminal( CompLoop ).Name, "[W]", UserAirTerminal( CompLoop ).RemainingOutputToCoolingSP );
				SetupEMSInternalVariable( "Remaining Latent Load to Humidifying Setpoint", UserAirTerminal( CompLoop ).Name, "[kg/s]", UserAirTerminal( CompLoop ).RemainingOutputReqToHumidSP );
				SetupEMSInternalVariable( "Remaining Latent Load to Dehumidifying Setpoint", UserAirTerminal( CompLoop ).Name, "[kg/s]", UserAirTerminal( CompLoop ).RemainingOutputReqToDehumidSP );

				SetupEMSActuator( "Primary Air Connection", UserAirTerminal( CompLoop ).Name, "Inlet Mass Flow Rate", "[kg/s]", lDummy, UserAirTerminal( CompLoop ).AirLoop.InletMassFlowRate );
				UserAirTerminal( CompLoop ).AirLoop.OutletNodeNum = GetOnlySingleNode( cAlphaArgs( 5 ), ErrorsFound, cCurrentModuleObject, UserAirTerminal( CompLoop ).Name, NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent, cAlphaFieldNames( 5 ) );
				SetupEMSActuator( "Primary Air Connection", UserAirTerminal( CompLoop ).Name, "Outlet Temperature", "[C]", lDummy, UserAirTerminal( CompLoop ).AirLoop.OutletTemp );
				SetupEMSActuator( "Primary Air Connection", UserAirTerminal( CompLoop ).Name, "Outlet Humidity Ratio", "[kgWater/kgDryAir]", lDummy, UserAirTerminal( CompLoop ).AirLoop.OutletHumRat );
				SetupEMSActuator( "Primary Air Connection", UserAirTerminal( CompLoop ).Name, "Outlet Mass Flow Rate", "[kg/s]", lDummy, UserAirTerminal( CompLoop ).AirLoop.OutletMassFlowRate );
				TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 4 ), cAlphaArgs( 5 ), "Air Nodes" );

				// Fill the Zone Equipment data with the inlet node number of this unit.
				for ( CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone ) {
					if ( ! ZoneEquipConfig( CtrlZone ).IsControlled ) continue;
					for ( SupAirIn = 1; SupAirIn <= ZoneEquipConfig( CtrlZone ).NumInletNodes; ++SupAirIn ) {
						if ( UserAirTerminal( CompLoop ).AirLoop.OutletNodeNum == ZoneEquipConfig( CtrlZone ).InletNode( SupAirIn ) ) {
							if ( ZoneEquipConfig( CtrlZone ).AirDistUnitCool( SupAirIn ).OutNode > 0 ) {
								ShowSevereError( "Error in connecting a terminal unit to a zone" );
								ShowContinueError( NodeID( UserAirTerminal( CompLoop ).AirLoop.OutletNodeNum ) + " already connects to another zone" );
								ShowContinueError( "Occurs for terminal unit " + cCurrentModuleObject + " = " + UserAirTerminal( CompLoop ).Name );
								ShowContinueError( "Check terminal unit node names for errors" );
								ErrorsFound = true;
							} else {
								ZoneEquipConfig( CtrlZone ).AirDistUnitCool( SupAirIn ).InNode = UserAirTerminal( CompLoop ).AirLoop.InletNodeNum;
								ZoneEquipConfig( CtrlZone ).AirDistUnitCool( SupAirIn ).OutNode = UserAirTerminal( CompLoop ).AirLoop.OutletNodeNum;
							}

							UserAirTerminal( CompLoop ).ActualCtrlZoneNum = CtrlZone;

						}
					}
				}

				if ( ! lAlphaFieldBlanks( 6 ) ) {
					UserAirTerminal( CompLoop ).SourceAir.InletNodeNum = GetOnlySingleNode( cAlphaArgs( 6 ), ErrorsFound, cCurrentModuleObject, UserAirTerminal( CompLoop ).Name, NodeType_Air, NodeConnectionType_Inlet, 2, ObjectIsNotParent, cAlphaFieldNames( 6 ) );
					//model input related internal variables
					SetupEMSInternalVariable( "Inlet Temperature for Secondary Air Connection", UserAirTerminal( CompLoop ).Name, "[C]", UserAirTerminal( CompLoop ).SourceAir.InletTemp );

					SetupEMSInternalVariable( "Inlet Humidity Ratio for Secondary Air Connection", UserAirTerminal( CompLoop ).Name, "[kgWater/kgDryAir]", UserAirTerminal( CompLoop ).SourceAir.InletHumRat );
					SetupEMSInternalVariable( "Inlet Density for Secondary Air Connection", UserAirTerminal( CompLoop ).Name, "[kg/m3]", UserAirTerminal( CompLoop ).SourceAir.InletRho );
					SetupEMSInternalVariable( "Inlet Specific Heat for Secondary Air Connection", UserAirTerminal( CompLoop ).Name, "[J/kg-C]", UserAirTerminal( CompLoop ).SourceAir.InletCp );
					SetupEMSActuator( "Secondary Air Connection", UserAirTerminal( CompLoop ).Name, "Inlet Mass Flow Rate", "[kg/s]", lDummy, UserAirTerminal( CompLoop ).SourceAir.InletMassFlowRate );
				}

				if ( ! lAlphaFieldBlanks( 7 ) ) {
					UserAirTerminal( CompLoop ).SourceAir.OutletNodeNum = GetOnlySingleNode( cAlphaArgs( 7 ), ErrorsFound, cCurrentModuleObject, UserAirTerminal( CompLoop ).Name, NodeType_Air, NodeConnectionType_Outlet, 2, ObjectIsNotParent, cAlphaFieldNames( 7 ) );
					SetupEMSActuator( "Secondary Air Connection", UserAirTerminal( CompLoop ).Name, "Outlet Temperature", "[C]", lDummy, UserAirTerminal( CompLoop ).SourceAir.OutletTemp );
					SetupEMSActuator( "Secondary Air Connection", UserAirTerminal( CompLoop ).Name, "Outlet Humidity Ratio", "[kgWater/kgDryAir]", lDummy, UserAirTerminal( CompLoop ).SourceAir.OutletHumRat );
					SetupEMSActuator( "Secondary Air Connection", UserAirTerminal( CompLoop ).Name, "Mass Flow Rate", "[kg/s]", lDummy, UserAirTerminal( CompLoop ).SourceAir.OutletMassFlowRate );
				}

				if ( ( UserAirTerminal( CompLoop ).SourceAir.InletNodeNum > 0 ) && ( UserAirTerminal( CompLoop ).SourceAir.OutletNodeNum > 0 ) ) {
					//  CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(6),cAlphaArgs(7),'Air Nodes')
				}

				NumPlantConnections = std::floor( rNumericArgs( 1 ) );
				UserAirTerminal( CompLoop ).NumPlantConnections = NumPlantConnections;
				if ( ( NumPlantConnections >= 1 ) && ( NumPlantConnections <= 2 ) ) {
					UserAirTerminal( CompLoop ).Loop.allocate( NumPlantConnections );
					for ( ConnectionLoop = 1; ConnectionLoop <= NumPlantConnections; ++ConnectionLoop ) {
						aArgCount = ( ConnectionLoop - 1 ) * 2 + 8;
						UserAirTerminal( CompLoop ).Loop( ConnectionLoop ).InletNodeNum = GetOnlySingleNode( cAlphaArgs( aArgCount ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, ( ConnectionLoop + 2 ), ObjectIsNotParent, cAlphaFieldNames( aArgCount ) );
						UserAirTerminal( CompLoop ).Loop( ConnectionLoop ).OutletNodeNum = GetOnlySingleNode( cAlphaArgs( aArgCount + 1 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, ( ConnectionLoop + 2 ), ObjectIsNotParent, cAlphaFieldNames( aArgCount + 1 ) );
						TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( aArgCount ), cAlphaArgs( aArgCount + 1 ), "Plant Nodes" );
						UserAirTerminal( CompLoop ).Loop( ConnectionLoop ).HowLoadServed = HowMet_NoneDemand;
						UserAirTerminal( CompLoop ).Loop( ConnectionLoop ).FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
						//Setup Internal Variables
						LoopStr = RoundSigDigits( ConnectionLoop );
						//model input related internal variables
						SetupEMSInternalVariable( "Inlet Temperature for Plant Connection " + LoopStr, UserAirTerminal( CompLoop ).Name, "[C]", UserAirTerminal( CompLoop ).Loop( ConnectionLoop ).InletTemp );
						SetupEMSInternalVariable( "Inlet Mass Flow Rate for Plant Connection " + LoopStr, UserAirTerminal( CompLoop ).Name, "[kg/s]", UserAirTerminal( CompLoop ).Loop( ConnectionLoop ).InletMassFlowRate );
						SetupEMSInternalVariable( "Inlet Density for Plant Connection " + LoopStr, UserAirTerminal( CompLoop ).Name, "[kg/m3]", UserAirTerminal( CompLoop ).Loop( ConnectionLoop ).InletRho );
						SetupEMSInternalVariable( "Inlet Specific Heat for Plant Connection " + LoopStr, UserAirTerminal( CompLoop ).Name, "[J/kg-C]", UserAirTerminal( CompLoop ).Loop( ConnectionLoop ).InletCp );
						// model results related actuators
						SetupEMSActuator( "Plant Connection " + LoopStr, UserAirTerminal( CompLoop ).Name, "Outlet Temperature", "[C]", lDummy, UserAirTerminal( CompLoop ).Loop( ConnectionLoop ).OutletTemp );
						SetupEMSActuator( "Plant Connection " + LoopStr, UserAirTerminal( CompLoop ).Name, "Mass Flow Rate", "[kg/s]", lDummy, UserAirTerminal( CompLoop ).Loop( ConnectionLoop ).MassFlowRateRequest );
						// model initialization and sizing related actuators
						SetupEMSActuator( "Plant Connection " + LoopStr, UserAirTerminal( CompLoop ).Name, "Minimum Mass Flow Rate", "[kg/s]", lDummy, UserAirTerminal( CompLoop ).Loop( ConnectionLoop ).MassFlowRateMin );
						SetupEMSActuator( "Plant Connection " + LoopStr, UserAirTerminal( CompLoop ).Name, "Maximum Mass Flow Rate", "[kg/s]", lDummy, UserAirTerminal( CompLoop ).Loop( ConnectionLoop ).MassFlowRateMax );
						SetupEMSActuator( "Plant Connection " + LoopStr, UserAirTerminal( CompLoop ).Name, "Design Volume Flow Rate", "[m3/s]", lDummy, UserAirTerminal( CompLoop ).Loop( ConnectionLoop ).DesignVolumeFlowRate );

					}
				}

				if ( ! lAlphaFieldBlanks( 12 ) ) {
					SetupTankDemandComponent( cAlphaArgs( 1 ), cCurrentModuleObject, cAlphaArgs( 12 ), ErrorsFound, UserAirTerminal( CompLoop ).Water.SupplyTankID, UserAirTerminal( CompLoop ).Water.SupplyTankDemandARRID );

					UserAirTerminal( CompLoop ).Water.SuppliedByWaterSystem = true;
					SetupEMSActuator( "Water System", UserAirTerminal( CompLoop ).Name, "Supplied Volume Flow Rate", "[m3/s]", lDummy, UserAirTerminal( CompLoop ).Water.SupplyVdotRequest );
				}

				if ( ! lAlphaFieldBlanks( 13 ) ) {
					SetupTankSupplyComponent( cAlphaArgs( 1 ), cCurrentModuleObject, cAlphaArgs( 13 ), ErrorsFound, UserAirTerminal( CompLoop ).Water.CollectionTankID, UserAirTerminal( CompLoop ).Water.CollectionTankSupplyARRID );
					UserAirTerminal( CompLoop ).Water.CollectsToWaterSystem = true;
					SetupEMSActuator( "Water System", UserAirTerminal( CompLoop ).Name, "Collected Volume Flow Rate", "[m3/s]", lDummy, UserAirTerminal( CompLoop ).Water.CollectedVdot );
				}

				if ( ! lAlphaFieldBlanks( 14 ) ) {

					UserAirTerminal( CompLoop ).Zone.ZoneNum = FindItemInList( cAlphaArgs( 14 ), Zone );
					if ( UserAirTerminal( CompLoop ).Zone.ZoneNum == 0 ) {
						ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  Ambient Zone Name not found = " + cAlphaArgs( 14 ) );
						ErrorsFound = true;
					} else {
						UserAirTerminal( CompLoop ).Zone.DeviceHasInternalGains = true;
						SetupZoneInternalGain( UserAirTerminal( CompLoop ).Zone.ZoneNum, cCurrentModuleObject, cAlphaArgs( 1 ), IntGainTypeOf_AirTerminalUserDefined, UserAirTerminal( CompLoop ).Zone.ConvectionGainRate, UserAirTerminal( CompLoop ).Zone.ReturnAirConvectionGainRate, UserAirTerminal( CompLoop ).Zone.ThermalRadiationGainRate, UserAirTerminal( CompLoop ).Zone.LatentGainRate, UserAirTerminal( CompLoop ).Zone.ReturnAirLatentGainRate, UserAirTerminal( CompLoop ).Zone.CarbonDioxideGainRate, UserAirTerminal( CompLoop ).Zone.GenericContamGainRate );

						SetupEMSActuator( "Component Zone Internal Gain", UserAirTerminal( CompLoop ).Name, "Sensible Heat Gain Rate", "[W]", lDummy, UserAirTerminal( CompLoop ).Zone.ConvectionGainRate );
						SetupEMSActuator( "Component Zone Internal Gain", UserAirTerminal( CompLoop ).Name, "Return Air Heat Sensible Gain Rate", "[W]", lDummy, UserZoneAirHVAC( CompLoop ).Zone.ReturnAirConvectionGainRate );
						SetupEMSActuator( "Component Zone Internal Gain", UserAirTerminal( CompLoop ).Name, "Thermal Radiation Heat Gain Rate", "[W]", lDummy, UserAirTerminal( CompLoop ).Zone.ThermalRadiationGainRate );
						SetupEMSActuator( "Component Zone Internal Gain", UserAirTerminal( CompLoop ).Name, "Latent Heat Gain Rate", "[W]", lDummy, UserAirTerminal( CompLoop ).Zone.LatentGainRate );
						SetupEMSActuator( "Component Zone Internal Gain", UserAirTerminal( CompLoop ).Name, "Return Air Latent Heat Gain Rate", "[W]", lDummy, UserAirTerminal( CompLoop ).Zone.ReturnAirLatentGainRate );
						SetupEMSActuator( "Component Zone Internal Gain", UserAirTerminal( CompLoop ).Name, "Carbon Dioxide Gain Rate", "[W]", lDummy, UserAirTerminal( CompLoop ).Zone.CarbonDioxideGainRate );
						SetupEMSActuator( "Component Zone Internal Gain", UserAirTerminal( CompLoop ).Name, "Gaseous Contaminant Gain Rate", "[W]", lDummy, UserAirTerminal( CompLoop ).Zone.GenericContamGainRate );
					}
				}

			}
		} //NumUserZoneAir > 0

		if ( ErrorsFound ) {
			ShowFatalError( "GetUserDefinedComponents: Errors found in processing " + cCurrentModuleObject + " input." );
		}

	}

	void
	InitPlantUserComponent(
		int const CompNum,
		int const LoopNum,
		Real64 const MyLoad
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         <author>
		//       DATE WRITTEN   <date_written>
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataEnvironment::OutBaroPress;
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSpecificHeatGlycol;
		using PlantUtilities::InitComponentNodes;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using Psychrometrics::PsyCpAirFnWTdb;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "InitPlantUserComponent" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool MyOneTimeFlag( true ); // one time flag
		static Array1D_bool MyEnvrnFlag; // environment flag
		static Array1D_bool MyFlag;
		int ConnectionNum;
		bool errFlag;
		//  REAL(r64) :: rho
		//  REAL(r64) :: Cp

		if ( MyOneTimeFlag ) {
			MyFlag.allocate( NumUserPlantComps );
			MyEnvrnFlag.allocate( NumUserPlantComps );
			MyFlag = true;
			MyEnvrnFlag = true;
			MyOneTimeFlag = false;
		}

		if ( MyFlag( CompNum ) ) {
			// locate the connections to the plant loops
			for ( ConnectionNum = 1; ConnectionNum <= UserPlantComp( CompNum ).NumPlantConnections; ++ConnectionNum ) {
				errFlag = false;
				ScanPlantLoopsForObject( UserPlantComp( CompNum ).Name, TypeOf_PlantComponentUserDefined, UserPlantComp( CompNum ).Loop( ConnectionNum ).LoopNum, UserPlantComp( CompNum ).Loop( ConnectionNum ).LoopSideNum, UserPlantComp( CompNum ).Loop( ConnectionNum ).BranchNum, UserPlantComp( CompNum ).Loop( ConnectionNum ).CompNum, _, _, _, UserPlantComp( CompNum ).Loop( ConnectionNum ).InletNodeNum );
				if ( errFlag ) {
					ShowFatalError( "InitPlantUserComponent: Program terminated due to previous condition(s)." );
				}

				//set user input for flow priority
				PlantLoop( UserPlantComp( CompNum ).Loop( ConnectionNum ).LoopNum ).LoopSide( UserPlantComp( CompNum ).Loop( ConnectionNum ).LoopSideNum ).Branch( UserPlantComp( CompNum ).Loop( ConnectionNum ).BranchNum ).Comp( UserPlantComp( CompNum ).Loop( ConnectionNum ).CompNum ).FlowPriority = UserPlantComp( CompNum ).Loop( ConnectionNum ).FlowPriority;

				// set user input for how loads served
				PlantLoop( UserPlantComp( CompNum ).Loop( ConnectionNum ).LoopNum ).LoopSide( UserPlantComp( CompNum ).Loop( ConnectionNum ).LoopSideNum ).Branch( UserPlantComp( CompNum ).Loop( ConnectionNum ).BranchNum ).Comp( UserPlantComp( CompNum ).Loop( ConnectionNum ).CompNum ).HowLoadServed = UserPlantComp( CompNum ).Loop( ConnectionNum ).HowLoadServed;

			}

			MyFlag( CompNum ) = false;
		}

		if ( LoopNum <= 0 || LoopNum > UserPlantComp( CompNum ).NumPlantConnections ) return;

		// fill internal variable targets
		UserPlantComp( CompNum ).Loop( LoopNum ).MyLoad = MyLoad;
		UserPlantComp( CompNum ).Loop( LoopNum ).InletRho = GetDensityGlycol( PlantLoop( UserPlantComp( CompNum ).Loop( LoopNum ).LoopNum ).FluidName, Node( UserPlantComp( CompNum ).Loop( LoopNum ).InletNodeNum ).Temp, PlantLoop( UserPlantComp( CompNum ).Loop( LoopNum ).LoopNum ).FluidIndex, RoutineName );
		UserPlantComp( CompNum ).Loop( LoopNum ).InletCp = GetSpecificHeatGlycol( PlantLoop( UserPlantComp( CompNum ).Loop( LoopNum ).LoopNum ).FluidName, Node( UserPlantComp( CompNum ).Loop( LoopNum ).InletNodeNum ).Temp, PlantLoop( UserPlantComp( CompNum ).Loop( LoopNum ).LoopNum ).FluidIndex, RoutineName );
		UserPlantComp( CompNum ).Loop( LoopNum ).InletMassFlowRate = Node( UserPlantComp( CompNum ).Loop( LoopNum ).InletNodeNum ).MassFlowRate;
		UserPlantComp( CompNum ).Loop( LoopNum ).InletTemp = Node( UserPlantComp( CompNum ).Loop( LoopNum ).InletNodeNum ).Temp;
		if ( UserPlantComp( CompNum ).Air.InletNodeNum > 0 ) {
			UserPlantComp( CompNum ).Air.InletRho = PsyRhoAirFnPbTdbW( OutBaroPress, Node( UserPlantComp( CompNum ).Air.InletNodeNum ).Temp, Node( UserPlantComp( CompNum ).Air.InletNodeNum ).HumRat, RoutineName );
			UserPlantComp( CompNum ).Air.InletCp = PsyCpAirFnWTdb( Node( UserPlantComp( CompNum ).Air.InletNodeNum ).HumRat, Node( UserPlantComp( CompNum ).Air.InletNodeNum ).Temp );
			UserPlantComp( CompNum ).Air.InletTemp = Node( UserPlantComp( CompNum ).Air.InletNodeNum ).Temp;
			UserPlantComp( CompNum ).Air.InletMassFlowRate = Node( UserPlantComp( CompNum ).Air.InletNodeNum ).MassFlowRate;
			UserPlantComp( CompNum ).Air.InletHumRat = Node( UserPlantComp( CompNum ).Air.InletNodeNum ).HumRat;
		}

	}

	void
	InitCoilUserDefined( int const CompNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         <author>
		//       DATE WRITTEN   <date_written>
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSpecificHeatGlycol;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using Psychrometrics::PsyCpAirFnWTdb;
		using DataEnvironment::OutBaroPress;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "InitCoilUserDefined" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool MyOneTimeFlag( true ); // one time flag
		static Array1D_bool MyFlag;
		bool errFlag;
		int Loop;

		if ( MyOneTimeFlag ) {
			MyFlag.dimension( NumUserCoils, true );
			MyOneTimeFlag = false;
		}

		if ( MyFlag( CompNum ) ) {
			if ( UserCoil( CompNum ).PlantIsConnected ) {
				errFlag = false;
				ScanPlantLoopsForObject( UserCoil( CompNum ).Name, TypeOf_CoilUserDefined, UserCoil( CompNum ).Loop.LoopNum, UserCoil( CompNum ).Loop.LoopSideNum, UserCoil( CompNum ).Loop.BranchNum, UserCoil( CompNum ).Loop.CompNum );
				if ( errFlag ) {
					ShowFatalError( "InitPlantUserComponent: Program terminated due to previous condition(s)." );
				}
				//set user input for flow priority
				PlantLoop( UserCoil( CompNum ).Loop.LoopNum ).LoopSide( UserCoil( CompNum ).Loop.LoopSideNum ).Branch( UserCoil( CompNum ).Loop.BranchNum ).Comp( UserCoil( CompNum ).Loop.CompNum ).FlowPriority = UserCoil( CompNum ).Loop.FlowPriority;

				// set user input for how loads served
				PlantLoop( UserCoil( CompNum ).Loop.LoopNum ).LoopSide( UserCoil( CompNum ).Loop.LoopSideNum ).Branch( UserCoil( CompNum ).Loop.BranchNum ).Comp( UserCoil( CompNum ).Loop.CompNum ).HowLoadServed = UserCoil( CompNum ).Loop.HowLoadServed;

			}
			MyFlag( CompNum ) = false;
		}

		// fill internal variable targets
		for ( Loop = 1; Loop <= UserCoil( CompNum ).NumAirConnections; ++Loop ) {
			UserCoil( CompNum ).Air( Loop ).InletRho = PsyRhoAirFnPbTdbW( OutBaroPress, Node( UserCoil( CompNum ).Air( Loop ).InletNodeNum ).Temp, Node( UserCoil( CompNum ).Air( Loop ).InletNodeNum ).HumRat, RoutineName );

			UserCoil( CompNum ).Air( Loop ).InletCp = PsyCpAirFnWTdb( Node( UserCoil( CompNum ).Air( Loop ).InletNodeNum ).HumRat, Node( UserCoil( CompNum ).Air( Loop ).InletNodeNum ).Temp );
			UserCoil( CompNum ).Air( Loop ).InletTemp = Node( UserCoil( CompNum ).Air( Loop ).InletNodeNum ).Temp;
			UserCoil( CompNum ).Air( Loop ).InletMassFlowRate = Node( UserCoil( CompNum ).Air( Loop ).InletNodeNum ).MassFlowRate;
			UserCoil( CompNum ).Air( Loop ).InletHumRat = Node( UserCoil( CompNum ).Air( Loop ).InletNodeNum ).HumRat;
		}

		if ( UserCoil( CompNum ).PlantIsConnected ) {
			UserCoil( CompNum ).Loop.InletRho = GetDensityGlycol( PlantLoop( UserCoil( CompNum ).Loop.LoopNum ).FluidName, Node( UserCoil( CompNum ).Loop.InletNodeNum ).Temp, PlantLoop( UserCoil( CompNum ).Loop.LoopNum ).FluidIndex, RoutineName );
			UserCoil( CompNum ).Loop.InletCp = GetSpecificHeatGlycol( PlantLoop( UserCoil( CompNum ).Loop.LoopNum ).FluidName, Node( UserCoil( CompNum ).Loop.InletNodeNum ).Temp, PlantLoop( UserCoil( CompNum ).Loop.LoopNum ).FluidIndex, RoutineName );
			UserCoil( CompNum ).Loop.InletTemp = Node( UserCoil( CompNum ).Loop.InletNodeNum ).Temp;
			UserCoil( CompNum ).Loop.InletMassFlowRate = Node( UserCoil( CompNum ).Loop.InletNodeNum ).MassFlowRate;
		}

	}

	void
	InitZoneAirUserDefined(
		int const CompNum,
		int const ZoneNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Feb. 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// initialize data for user-defined zone HVAC forced air component model

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSpecificHeatGlycol;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using Psychrometrics::PsyCpAirFnWTdb;
		using DataEnvironment::OutBaroPress;
		using DataZoneEnergyDemands::ZoneSysEnergyDemand;
		using DataZoneEnergyDemands::ZoneSysMoistureDemand;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "InitZoneAirUserDefined" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool MyOneTimeFlag( true ); // one time flag
		static Array1D_bool MyFlag;
		bool errFlag;
		int Loop;

		if ( MyOneTimeFlag ) {
			MyFlag.dimension( NumUserZoneAir, true );
			MyOneTimeFlag = false;
		}

		if ( MyFlag( CompNum ) ) {
			if ( UserZoneAirHVAC( CompNum ).NumPlantConnections > 0 ) {
				for ( Loop = 1; Loop <= UserZoneAirHVAC( CompNum ).NumPlantConnections; ++Loop ) {
					errFlag = false;
					ScanPlantLoopsForObject( UserZoneAirHVAC( CompNum ).Name, TypeOf_ZoneHVACAirUserDefined, UserZoneAirHVAC( CompNum ).Loop( Loop ).LoopNum, UserZoneAirHVAC( CompNum ).Loop( Loop ).LoopSideNum, UserZoneAirHVAC( CompNum ).Loop( Loop ).BranchNum, UserZoneAirHVAC( CompNum ).Loop( Loop ).CompNum, _, _, _, UserZoneAirHVAC( CompNum ).Loop( Loop ).InletNodeNum );
					if ( errFlag ) {
						ShowFatalError( "InitPlantUserComponent: Program terminated due to previous condition(s)." );
					}
					//set user input for flow priority
					PlantLoop( UserZoneAirHVAC( CompNum ).Loop( Loop ).LoopNum ).LoopSide( UserZoneAirHVAC( CompNum ).Loop( Loop ).LoopSideNum ).Branch( UserZoneAirHVAC( CompNum ).Loop( Loop ).BranchNum ).Comp( UserZoneAirHVAC( CompNum ).Loop( Loop ).CompNum ).FlowPriority = UserZoneAirHVAC( CompNum ).Loop( Loop ).FlowPriority;

					// set user input for how loads served
					PlantLoop( UserZoneAirHVAC( CompNum ).Loop( Loop ).LoopNum ).LoopSide( UserZoneAirHVAC( CompNum ).Loop( Loop ).LoopSideNum ).Branch( UserZoneAirHVAC( CompNum ).Loop( Loop ).BranchNum ).Comp( UserZoneAirHVAC( CompNum ).Loop( Loop ).CompNum ).HowLoadServed = UserZoneAirHVAC( CompNum ).Loop( Loop ).HowLoadServed;
				}

			}
		}
		// fill internal variable targets
		UserZoneAirHVAC( CompNum ).RemainingOutputToHeatingSP = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToHeatSP;
		UserZoneAirHVAC( CompNum ).RemainingOutputToCoolingSP = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToCoolSP;
		UserZoneAirHVAC( CompNum ).RemainingOutputReqToDehumidSP = ZoneSysMoistureDemand( ZoneNum ).RemainingOutputReqToDehumidSP;
		UserZoneAirHVAC( CompNum ).RemainingOutputReqToHumidSP = ZoneSysMoistureDemand( ZoneNum ).RemainingOutputReqToHumidSP;

		UserZoneAirHVAC( CompNum ).ZoneAir.InletRho = PsyRhoAirFnPbTdbW( OutBaroPress, Node( UserZoneAirHVAC( CompNum ).ZoneAir.InletNodeNum ).Temp, Node( UserZoneAirHVAC( CompNum ).ZoneAir.InletNodeNum ).HumRat, RoutineName );
		UserZoneAirHVAC( CompNum ).ZoneAir.InletCp = PsyCpAirFnWTdb( Node( UserZoneAirHVAC( CompNum ).ZoneAir.InletNodeNum ).HumRat, Node( UserZoneAirHVAC( CompNum ).ZoneAir.InletNodeNum ).Temp );
		UserZoneAirHVAC( CompNum ).ZoneAir.InletTemp = Node( UserZoneAirHVAC( CompNum ).ZoneAir.InletNodeNum ).Temp;
		UserZoneAirHVAC( CompNum ).ZoneAir.InletHumRat = Node( UserZoneAirHVAC( CompNum ).ZoneAir.InletNodeNum ).HumRat;

		if ( UserZoneAirHVAC( CompNum ).SourceAir.InletNodeNum > 0 ) {
			UserZoneAirHVAC( CompNum ).SourceAir.InletRho = PsyRhoAirFnPbTdbW( OutBaroPress, Node( UserZoneAirHVAC( CompNum ).SourceAir.InletNodeNum ).Temp, Node( UserZoneAirHVAC( CompNum ).SourceAir.InletNodeNum ).HumRat, RoutineName );
			UserZoneAirHVAC( CompNum ).SourceAir.InletCp = PsyCpAirFnWTdb( Node( UserZoneAirHVAC( CompNum ).SourceAir.InletNodeNum ).HumRat, Node( UserZoneAirHVAC( CompNum ).SourceAir.InletNodeNum ).Temp );
			UserZoneAirHVAC( CompNum ).SourceAir.InletTemp = Node( UserZoneAirHVAC( CompNum ).SourceAir.InletNodeNum ).Temp;
			UserZoneAirHVAC( CompNum ).SourceAir.InletHumRat = Node( UserZoneAirHVAC( CompNum ).SourceAir.InletNodeNum ).HumRat;
		}

		if ( UserZoneAirHVAC( CompNum ).NumPlantConnections > 0 ) {
			for ( Loop = 1; Loop <= UserZoneAirHVAC( CompNum ).NumPlantConnections; ++Loop ) {
				UserZoneAirHVAC( CompNum ).Loop( Loop ).InletRho = GetDensityGlycol( PlantLoop( UserZoneAirHVAC( CompNum ).Loop( Loop ).LoopNum ).FluidName, Node( UserZoneAirHVAC( CompNum ).Loop( Loop ).InletNodeNum ).Temp, PlantLoop( UserZoneAirHVAC( CompNum ).Loop( Loop ).LoopNum ).FluidIndex, RoutineName );
				UserZoneAirHVAC( CompNum ).Loop( Loop ).InletCp = GetSpecificHeatGlycol( PlantLoop( UserZoneAirHVAC( CompNum ).Loop( Loop ).LoopNum ).FluidName, Node( UserZoneAirHVAC( CompNum ).Loop( Loop ).InletNodeNum ).Temp, PlantLoop( UserZoneAirHVAC( CompNum ).Loop( Loop ).LoopNum ).FluidIndex, RoutineName );
				UserZoneAirHVAC( CompNum ).Loop( Loop ).InletTemp = Node( UserZoneAirHVAC( CompNum ).Loop( Loop ).InletNodeNum ).Temp;
				UserZoneAirHVAC( CompNum ).Loop( Loop ).InletMassFlowRate = Node( UserZoneAirHVAC( CompNum ).Loop( Loop ).InletNodeNum ).MassFlowRate;
			}
		}

	}

	void
	InitAirTerminalUserDefined(
		int const CompNum,
		int const ZoneNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   March 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSpecificHeatGlycol;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using Psychrometrics::PsyCpAirFnWTdb;
		using DataEnvironment::OutBaroPress;
		using DataZoneEnergyDemands::ZoneSysEnergyDemand;
		using DataZoneEnergyDemands::ZoneSysMoistureDemand;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "InitAirTerminalUserDefined" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool MyOneTimeFlag( true ); // one time flag
		static Array1D_bool MyFlag;
		bool errFlag;
		int Loop;

		if ( MyOneTimeFlag ) {
			MyFlag.dimension( NumUserAirTerminals, true );
			MyOneTimeFlag = false;
		}

		if ( MyFlag( CompNum ) ) {
			if ( UserAirTerminal( CompNum ).NumPlantConnections > 0 ) {
				for ( Loop = 1; Loop <= UserAirTerminal( CompNum ).NumPlantConnections; ++Loop ) {
					errFlag = false;
					ScanPlantLoopsForObject( UserAirTerminal( CompNum ).Name, TypeOf_AirTerminalUserDefined, UserAirTerminal( CompNum ).Loop( Loop ).LoopNum, UserAirTerminal( CompNum ).Loop( Loop ).LoopSideNum, UserAirTerminal( CompNum ).Loop( Loop ).BranchNum, UserAirTerminal( CompNum ).Loop( Loop ).CompNum, _, _, _, UserAirTerminal( CompNum ).Loop( Loop ).InletNodeNum );
					if ( errFlag ) {
						ShowFatalError( "InitPlantUserComponent: Program terminated due to previous condition(s)." );
					}
					//set user input for flow priority
					PlantLoop( UserAirTerminal( CompNum ).Loop( Loop ).LoopNum ).LoopSide( UserAirTerminal( CompNum ).Loop( Loop ).LoopSideNum ).Branch( UserAirTerminal( CompNum ).Loop( Loop ).BranchNum ).Comp( UserAirTerminal( CompNum ).Loop( Loop ).CompNum ).FlowPriority = UserAirTerminal( CompNum ).Loop( Loop ).FlowPriority;

					// set user input for how loads served
					PlantLoop( UserAirTerminal( CompNum ).Loop( Loop ).LoopNum ).LoopSide( UserAirTerminal( CompNum ).Loop( Loop ).LoopSideNum ).Branch( UserAirTerminal( CompNum ).Loop( Loop ).BranchNum ).Comp( UserAirTerminal( CompNum ).Loop( Loop ).CompNum ).HowLoadServed = UserAirTerminal( CompNum ).Loop( Loop ).HowLoadServed;
				}

			}
		}
		// fill internal variable targets
		UserAirTerminal( CompNum ).RemainingOutputToHeatingSP = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToHeatSP;
		UserAirTerminal( CompNum ).RemainingOutputToCoolingSP = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToCoolSP;
		UserAirTerminal( CompNum ).RemainingOutputReqToDehumidSP = ZoneSysMoistureDemand( ZoneNum ).RemainingOutputReqToDehumidSP;
		UserAirTerminal( CompNum ).RemainingOutputReqToHumidSP = ZoneSysMoistureDemand( ZoneNum ).RemainingOutputReqToHumidSP;

		UserAirTerminal( CompNum ).AirLoop.InletRho = PsyRhoAirFnPbTdbW( OutBaroPress, Node( UserAirTerminal( CompNum ).AirLoop.InletNodeNum ).Temp, Node( UserAirTerminal( CompNum ).AirLoop.InletNodeNum ).HumRat, RoutineName );
		UserAirTerminal( CompNum ).AirLoop.InletCp = PsyCpAirFnWTdb( Node( UserAirTerminal( CompNum ).AirLoop.InletNodeNum ).HumRat, Node( UserAirTerminal( CompNum ).AirLoop.InletNodeNum ).Temp );
		UserAirTerminal( CompNum ).AirLoop.InletTemp = Node( UserAirTerminal( CompNum ).AirLoop.InletNodeNum ).Temp;
		UserAirTerminal( CompNum ).AirLoop.InletHumRat = Node( UserAirTerminal( CompNum ).AirLoop.InletNodeNum ).HumRat;

		if ( UserAirTerminal( CompNum ).SourceAir.InletNodeNum > 0 ) {
			UserAirTerminal( CompNum ).SourceAir.InletRho = PsyRhoAirFnPbTdbW( OutBaroPress, Node( UserAirTerminal( CompNum ).SourceAir.InletNodeNum ).Temp, Node( UserAirTerminal( CompNum ).SourceAir.InletNodeNum ).HumRat, RoutineName );
			UserAirTerminal( CompNum ).SourceAir.InletCp = PsyCpAirFnWTdb( Node( UserAirTerminal( CompNum ).SourceAir.InletNodeNum ).HumRat, Node( UserAirTerminal( CompNum ).SourceAir.InletNodeNum ).Temp );
			UserAirTerminal( CompNum ).SourceAir.InletTemp = Node( UserAirTerminal( CompNum ).SourceAir.InletNodeNum ).Temp;
			UserAirTerminal( CompNum ).SourceAir.InletHumRat = Node( UserAirTerminal( CompNum ).SourceAir.InletNodeNum ).HumRat;
		}

		if ( UserAirTerminal( CompNum ).NumPlantConnections > 0 ) {
			for ( Loop = 1; Loop <= UserAirTerminal( CompNum ).NumPlantConnections; ++Loop ) {
				UserAirTerminal( CompNum ).Loop( Loop ).InletRho = GetDensityGlycol( PlantLoop( UserAirTerminal( CompNum ).Loop( Loop ).LoopNum ).FluidName, Node( UserAirTerminal( CompNum ).Loop( Loop ).InletNodeNum ).Temp, PlantLoop( UserAirTerminal( CompNum ).Loop( Loop ).LoopNum ).FluidIndex, RoutineName );
				UserAirTerminal( CompNum ).Loop( Loop ).InletCp = GetSpecificHeatGlycol( PlantLoop( UserAirTerminal( CompNum ).Loop( Loop ).LoopNum ).FluidName, Node( UserAirTerminal( CompNum ).Loop( Loop ).InletNodeNum ).Temp, PlantLoop( UserAirTerminal( CompNum ).Loop( Loop ).LoopNum ).FluidIndex, RoutineName );
				UserAirTerminal( CompNum ).Loop( Loop ).InletTemp = Node( UserAirTerminal( CompNum ).Loop( Loop ).InletNodeNum ).Temp;
				UserAirTerminal( CompNum ).Loop( Loop ).InletMassFlowRate = Node( UserAirTerminal( CompNum ).Loop( Loop ).InletNodeNum ).MassFlowRate;
			}
		}

	}

	void
	ReportPlantUserComponent(
		int const CompNum,
		int const LoopNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   Feb. 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// report model results

		// METHODOLOGY EMPLOYED:
		// copy actuated values to structures elsewhere in program.

		// REFERENCES:
		// na

		// Using/Aliasing
		using PlantUtilities::SetComponentFlowRate;
		using PlantUtilities::SafeCopyPlantNode;
		using Psychrometrics::PsyHFnTdbW;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		SafeCopyPlantNode( UserPlantComp( CompNum ).Loop( LoopNum ).InletNodeNum, UserPlantComp( CompNum ).Loop( LoopNum ).OutletNodeNum );

		//unload Actuators to node data structure

		Node( UserPlantComp( CompNum ).Loop( LoopNum ).OutletNodeNum ).Temp = UserPlantComp( CompNum ).Loop( LoopNum ).OutletTemp;

		//make mass flow requests, just this loop
		SetComponentFlowRate( UserPlantComp( CompNum ).Loop( LoopNum ).MassFlowRateRequest, UserPlantComp( CompNum ).Loop( LoopNum ).InletNodeNum, UserPlantComp( CompNum ).Loop( LoopNum ).OutletNodeNum, UserPlantComp( CompNum ).Loop( LoopNum ).LoopNum, UserPlantComp( CompNum ).Loop( LoopNum ).LoopSideNum, UserPlantComp( CompNum ).Loop( LoopNum ).BranchNum, UserPlantComp( CompNum ).Loop( LoopNum ).CompNum );

		if ( UserPlantComp( CompNum ).Air.OutletNodeNum > 0 ) {
			Node( UserPlantComp( CompNum ).Air.OutletNodeNum ).Temp = UserPlantComp( CompNum ).Air.OutletTemp;
			Node( UserPlantComp( CompNum ).Air.OutletNodeNum ).HumRat = UserPlantComp( CompNum ).Air.OutletHumRat;
			Node( UserPlantComp( CompNum ).Air.OutletNodeNum ).MassFlowRate = UserPlantComp( CompNum ).Air.OutletMassFlowRate;
			Node( UserPlantComp( CompNum ).Air.OutletNodeNum ).Enthalpy = PsyHFnTdbW( UserPlantComp( CompNum ).Air.OutletTemp, UserPlantComp( CompNum ).Air.OutletHumRat );
		}

		if ( UserPlantComp( CompNum ).Water.SuppliedByWaterSystem ) {
			WaterStorage( UserPlantComp( CompNum ).Water.SupplyTankID ).VdotRequestDemand( UserPlantComp( CompNum ).Water.SupplyTankDemandARRID ) = UserPlantComp( CompNum ).Water.SupplyVdotRequest;
		}

		if ( UserPlantComp( CompNum ).Water.CollectsToWaterSystem ) {
			WaterStorage( UserPlantComp( CompNum ).Water.CollectionTankID ).VdotAvailSupply( UserPlantComp( CompNum ).Water.CollectionTankSupplyARRID ) = UserPlantComp( CompNum ).Water.CollectedVdot;
		}

		if ( UserPlantComp( CompNum ).Loop( LoopNum ).HowLoadServed == HowMet_ByNominalCapLowOutLimit ) {
			PlantLoop( UserPlantComp( CompNum ).Loop( LoopNum ).LoopNum ).LoopSide( UserPlantComp( CompNum ).Loop( LoopNum ).LoopSideNum ).Branch( UserPlantComp( CompNum ).Loop( LoopNum ).BranchNum ).Comp( UserPlantComp( CompNum ).Loop( LoopNum ).CompNum ).MinOutletTemp = UserPlantComp( CompNum ).Loop( LoopNum ).LowOutTempLimit;
		}

		if ( UserPlantComp( CompNum ).Loop( LoopNum ).HowLoadServed == HowMet_ByNominalCapHiOutLimit ) {
			PlantLoop( UserPlantComp( CompNum ).Loop( LoopNum ).LoopNum ).LoopSide( UserPlantComp( CompNum ).Loop( LoopNum ).LoopSideNum ).Branch( UserPlantComp( CompNum ).Loop( LoopNum ).BranchNum ).Comp( UserPlantComp( CompNum ).Loop( LoopNum ).CompNum ).MaxOutletTemp = UserPlantComp( CompNum ).Loop( LoopNum ).HiOutTempLimit;
		}

	}

	void
	ReportCoilUserDefined( int const CompNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   Feb. 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// report model outputs

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using PlantUtilities::SetComponentFlowRate;
		using PlantUtilities::SafeCopyPlantNode;
		using Psychrometrics::PsyHFnTdbW;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Loop;

		for ( Loop = 1; Loop <= UserCoil( CompNum ).NumAirConnections; ++Loop ) {
			if ( UserCoil( CompNum ).Air( Loop ).OutletNodeNum > 0 ) {
				Node( UserCoil( CompNum ).Air( Loop ).OutletNodeNum ).Temp = UserCoil( CompNum ).Air( Loop ).OutletTemp;
				Node( UserCoil( CompNum ).Air( Loop ).OutletNodeNum ).HumRat = UserCoil( CompNum ).Air( Loop ).OutletHumRat;
				Node( UserCoil( CompNum ).Air( Loop ).OutletNodeNum ).MassFlowRate = UserCoil( CompNum ).Air( Loop ).OutletMassFlowRate;
				Node( UserCoil( CompNum ).Air( Loop ).OutletNodeNum ).Enthalpy = PsyHFnTdbW( UserCoil( CompNum ).Air( Loop ).OutletTemp, UserCoil( CompNum ).Air( Loop ).OutletHumRat );

				Node( UserCoil( CompNum ).Air( Loop ).OutletNodeNum ).MassFlowRateMinAvail = Node( UserCoil( CompNum ).Air( Loop ).InletNodeNum ).MassFlowRateMinAvail;
				Node( UserCoil( CompNum ).Air( Loop ).OutletNodeNum ).MassFlowRateMaxAvail = Node( UserCoil( CompNum ).Air( Loop ).InletNodeNum ).MassFlowRateMaxAvail;

			}
		}

		if ( UserCoil( CompNum ).PlantIsConnected ) {
			//make mass flow requests
			SetComponentFlowRate( UserCoil( CompNum ).Loop.MassFlowRateRequest, UserCoil( CompNum ).Loop.InletNodeNum, UserCoil( CompNum ).Loop.OutletNodeNum, UserCoil( CompNum ).Loop.LoopNum, UserCoil( CompNum ).Loop.LoopSideNum, UserCoil( CompNum ).Loop.BranchNum, UserCoil( CompNum ).Loop.CompNum );
			SafeCopyPlantNode( UserCoil( CompNum ).Loop.InletNodeNum, UserCoil( CompNum ).Loop.OutletNodeNum );
			//unload Actuators to node data structure
			Node( UserCoil( CompNum ).Loop.OutletNodeNum ).Temp = UserCoil( CompNum ).Loop.OutletTemp;
		}

		if ( UserCoil( CompNum ).Water.SuppliedByWaterSystem ) {
			WaterStorage( UserCoil( CompNum ).Water.SupplyTankID ).VdotRequestDemand( UserCoil( CompNum ).Water.SupplyTankDemandARRID ) = UserCoil( CompNum ).Water.SupplyVdotRequest;
		}

		if ( UserCoil( CompNum ).Water.CollectsToWaterSystem ) {
			WaterStorage( UserCoil( CompNum ).Water.CollectionTankID ).VdotAvailSupply( UserCoil( CompNum ).Water.CollectionTankSupplyARRID ) = UserCoil( CompNum ).Water.CollectedVdot;
		}

	}

	void
	ReportZoneAirUserDefined( int const CompNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   Feb. 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// report model outputs

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using PlantUtilities::SetComponentFlowRate;
		using PlantUtilities::SafeCopyPlantNode;
		using Psychrometrics::PsyHFnTdbW;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Loop;

		Node( UserZoneAirHVAC( CompNum ).ZoneAir.InletNodeNum ).MassFlowRate = UserZoneAirHVAC( CompNum ).ZoneAir.InletMassFlowRate;

		Node( UserZoneAirHVAC( CompNum ).ZoneAir.OutletNodeNum ).Temp = UserZoneAirHVAC( CompNum ).ZoneAir.OutletTemp;
		Node( UserZoneAirHVAC( CompNum ).ZoneAir.OutletNodeNum ).HumRat = UserZoneAirHVAC( CompNum ).ZoneAir.OutletHumRat;
		Node( UserZoneAirHVAC( CompNum ).ZoneAir.OutletNodeNum ).MassFlowRate = UserZoneAirHVAC( CompNum ).ZoneAir.OutletMassFlowRate;
		Node( UserZoneAirHVAC( CompNum ).ZoneAir.OutletNodeNum ).Enthalpy = PsyHFnTdbW( UserZoneAirHVAC( CompNum ).ZoneAir.OutletTemp, UserZoneAirHVAC( CompNum ).ZoneAir.OutletHumRat );

		if ( UserZoneAirHVAC( CompNum ).SourceAir.OutletNodeNum > 0 ) {
			Node( UserZoneAirHVAC( CompNum ).SourceAir.OutletNodeNum ).Temp = UserZoneAirHVAC( CompNum ).SourceAir.OutletTemp;
			Node( UserZoneAirHVAC( CompNum ).SourceAir.OutletNodeNum ).HumRat = UserZoneAirHVAC( CompNum ).SourceAir.OutletHumRat;
			Node( UserZoneAirHVAC( CompNum ).SourceAir.OutletNodeNum ).MassFlowRate = UserZoneAirHVAC( CompNum ).SourceAir.OutletMassFlowRate;
			Node( UserZoneAirHVAC( CompNum ).SourceAir.OutletNodeNum ).Enthalpy = PsyHFnTdbW( UserZoneAirHVAC( CompNum ).SourceAir.OutletTemp, UserZoneAirHVAC( CompNum ).SourceAir.OutletHumRat );
		}

		if ( UserZoneAirHVAC( CompNum ).NumPlantConnections > 0 ) {
			for ( Loop = 1; Loop <= UserZoneAirHVAC( CompNum ).NumPlantConnections; ++Loop ) {
				//make mass flow requests
				SetComponentFlowRate( UserZoneAirHVAC( CompNum ).Loop( Loop ).MassFlowRateRequest, UserZoneAirHVAC( CompNum ).Loop( Loop ).InletNodeNum, UserZoneAirHVAC( CompNum ).Loop( Loop ).OutletNodeNum, UserZoneAirHVAC( CompNum ).Loop( Loop ).LoopNum, UserZoneAirHVAC( CompNum ).Loop( Loop ).LoopSideNum, UserZoneAirHVAC( CompNum ).Loop( Loop ).BranchNum, UserZoneAirHVAC( CompNum ).Loop( Loop ).CompNum );
				SafeCopyPlantNode( UserZoneAirHVAC( CompNum ).Loop( Loop ).InletNodeNum, UserZoneAirHVAC( CompNum ).Loop( Loop ).OutletNodeNum );
				//unload Actuators to node data structure
				Node( UserZoneAirHVAC( CompNum ).Loop( Loop ).OutletNodeNum ).Temp = UserZoneAirHVAC( CompNum ).Loop( Loop ).OutletTemp;
			}
		}

		if ( UserZoneAirHVAC( CompNum ).Water.SuppliedByWaterSystem ) {
			WaterStorage( UserZoneAirHVAC( CompNum ).Water.SupplyTankID ).VdotRequestDemand( UserZoneAirHVAC( CompNum ).Water.SupplyTankDemandARRID ) = UserZoneAirHVAC( CompNum ).Water.SupplyVdotRequest;
		}

		if ( UserZoneAirHVAC( CompNum ).Water.CollectsToWaterSystem ) {
			WaterStorage( UserZoneAirHVAC( CompNum ).Water.CollectionTankID ).VdotAvailSupply( UserZoneAirHVAC( CompNum ).Water.CollectionTankSupplyARRID ) = UserZoneAirHVAC( CompNum ).Water.CollectedVdot;
		}

	}

	void
	ReportAirTerminalUserDefined( int const CompNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   March 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using PlantUtilities::SetComponentFlowRate;
		using PlantUtilities::SafeCopyPlantNode;
		using Psychrometrics::PsyHFnTdbW;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na
		int Loop;

		Node( UserAirTerminal( CompNum ).AirLoop.InletNodeNum ).MassFlowRate = UserAirTerminal( CompNum ).AirLoop.InletMassFlowRate;

		Node( UserAirTerminal( CompNum ).AirLoop.OutletNodeNum ).Temp = UserAirTerminal( CompNum ).AirLoop.OutletTemp;
		Node( UserAirTerminal( CompNum ).AirLoop.OutletNodeNum ).HumRat = UserAirTerminal( CompNum ).AirLoop.OutletHumRat;
		Node( UserAirTerminal( CompNum ).AirLoop.OutletNodeNum ).MassFlowRate = UserAirTerminal( CompNum ).AirLoop.OutletMassFlowRate;
		Node( UserAirTerminal( CompNum ).AirLoop.OutletNodeNum ).Enthalpy = PsyHFnTdbW( UserAirTerminal( CompNum ).AirLoop.OutletTemp, UserAirTerminal( CompNum ).AirLoop.OutletHumRat );
		if ( UserAirTerminal( CompNum ).SourceAir.OutletNodeNum > 0 ) {
			Node( UserAirTerminal( CompNum ).SourceAir.OutletNodeNum ).Temp = UserAirTerminal( CompNum ).SourceAir.OutletTemp;
			Node( UserAirTerminal( CompNum ).SourceAir.OutletNodeNum ).HumRat = UserAirTerminal( CompNum ).SourceAir.OutletHumRat;
			Node( UserAirTerminal( CompNum ).SourceAir.OutletNodeNum ).MassFlowRate = UserAirTerminal( CompNum ).SourceAir.OutletMassFlowRate;
			Node( UserAirTerminal( CompNum ).SourceAir.OutletNodeNum ).Enthalpy = PsyHFnTdbW( UserAirTerminal( CompNum ).SourceAir.OutletTemp, UserAirTerminal( CompNum ).SourceAir.OutletHumRat );
		}

		if ( UserAirTerminal( CompNum ).NumPlantConnections > 0 ) {
			for ( Loop = 1; Loop <= UserAirTerminal( CompNum ).NumPlantConnections; ++Loop ) {
				//make mass flow requests
				SetComponentFlowRate( UserAirTerminal( CompNum ).Loop( Loop ).MassFlowRateRequest, UserAirTerminal( CompNum ).Loop( Loop ).InletNodeNum, UserAirTerminal( CompNum ).Loop( Loop ).OutletNodeNum, UserAirTerminal( CompNum ).Loop( Loop ).LoopNum, UserAirTerminal( CompNum ).Loop( Loop ).LoopSideNum, UserAirTerminal( CompNum ).Loop( Loop ).BranchNum, UserAirTerminal( CompNum ).Loop( Loop ).CompNum );
				SafeCopyPlantNode( UserAirTerminal( CompNum ).Loop( Loop ).InletNodeNum, UserAirTerminal( CompNum ).Loop( Loop ).OutletNodeNum );
				//unload Actuators to node data structure
				Node( UserAirTerminal( CompNum ).Loop( Loop ).OutletNodeNum ).Temp = UserAirTerminal( CompNum ).Loop( Loop ).OutletTemp;
			}
		}

		if ( UserAirTerminal( CompNum ).Water.SuppliedByWaterSystem ) {
			WaterStorage( UserAirTerminal( CompNum ).Water.SupplyTankID ).VdotRequestDemand( UserAirTerminal( CompNum ).Water.SupplyTankDemandARRID ) = UserAirTerminal( CompNum ).Water.SupplyVdotRequest;
		}

		if ( UserAirTerminal( CompNum ).Water.CollectsToWaterSystem ) {
			WaterStorage( UserAirTerminal( CompNum ).Water.CollectionTankID ).VdotAvailSupply( UserAirTerminal( CompNum ).Water.CollectionTankSupplyARRID ) = UserAirTerminal( CompNum ).Water.CollectedVdot;
		}

	}

	void
	GetUserDefinedCoilIndex(
		std::string const & CoilName,
		int & CoilIndex,
		bool & ErrorsFound,
		std::string const & CurrentModuleObject
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   August 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine sets an index for a given user defined Cooling Coil -- issues error message if that
		// coil is not a legal user defined Cooling Coil.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItem;

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

		// Obtains and allocates TESCoil related parameters from input file
		if ( GetInput ) { // First time subroutine has been called, get input data
			GetUserDefinedComponents();
			GetInput = false; // Set logic flag to disallow getting the input data on future calls to this subroutine
		}

		if ( NumUserCoils > 0 ) {
			CoilIndex = FindItem( CoilName, UserCoil, NumUserCoils );
		} else {
			CoilIndex = 0;
		}

		if ( CoilIndex == 0 ) {
			ShowSevereError( CurrentModuleObject + ", GetUserDefinedCoilIndex: User Defined Cooling Coil not found=" + CoilName );
			ErrorsFound = true;
		}

	}

	void
	GetUserDefinedCoilAirInletNode(
		std::string const & CoilName,
		int & CoilAirInletNode,
		bool & ErrorsFound,
		std::string const & CurrentModuleObject
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   July 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine gets a given user defined Cooling Coil's air inlet node -- issues error message if that
		// coil is not a legal user defined Cooling Coil and sets air node to 0, otherwise, returns inlet air node number.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItem;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int CoilIndex;

		// Obtains and allocates TESCoil related parameters from input file
		if ( GetInput ) { // First time subroutine has been called, get input data
			GetUserDefinedComponents();
			GetInput = false; // Set logic flag to disallow getting the input data on future calls to this subroutine
		}

		if ( NumUserCoils > 0 ) {
			CoilIndex = FindItem( CoilName, UserCoil, NumUserCoils );
		} else {
			CoilIndex = 0;
		}

		if ( CoilIndex == 0 ) {
			ShowSevereError( CurrentModuleObject + ", GetTESCoilIndex: TES Cooling Coil not found=" + CoilName );
			ErrorsFound = true;
			CoilAirInletNode = 0;
		} else {
			CoilAirInletNode = UserCoil( CoilIndex ).Air( 1 ).InletNodeNum;
		}

	}

	void
	GetUserDefinedCoilAirOutletNode(
		std::string const & CoilName,
		int & CoilAirOutletNode,
		bool & ErrorsFound,
		std::string const & CurrentModuleObject
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   July 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine gets a given user defined Cooling Coil's air outlet node -- issues error message if that
		// coil is not a legal user defined Cooling Coil and sets air node to 0, otherwise, returns outlet air node number.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItem;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int CoilIndex;

		// Obtains and allocates TESCoil related parameters from input file
		if ( GetInput ) { // First time subroutine has been called, get input data
			GetUserDefinedComponents();
			GetInput = false; // Set logic flag to disallow getting the input data on future calls to this subroutine
		}

		if ( NumUserCoils > 0 ) {
			CoilIndex = FindItem( CoilName, UserCoil, NumUserCoils );
		} else {
			CoilIndex = 0;
		}

		if ( CoilIndex == 0 ) {
			ShowSevereError( CurrentModuleObject + ", GetTESCoilIndex: TES Cooling Coil not found=" + CoilName );
			ErrorsFound = true;
			CoilAirOutletNode = 0;
		} else {
			CoilAirOutletNode = UserCoil( CoilIndex ).Air( 1 ).OutletNodeNum;
		}

	}

} // UserDefinedComponents

} // EnergyPlus
