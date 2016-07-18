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
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <MixedAir.hh>
#include <BranchNodeConnections.hh>
#include <CurveManager.hh>
#include <DataAirLoop.hh>
#include <DataAirSystems.hh>
#include <DataContaminantBalance.hh>
#include <DataDefineEquip.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DataZoneControls.hh>
#include <DataZoneEnergyDemands.hh>
#include <DataZoneEquipment.hh>
#include <DesiccantDehumidifiers.hh>
#include <EMSManager.hh>
#include <EvaporativeCoolers.hh>
#include <Fans.hh>
#include <FaultsManager.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <HeatingCoils.hh>
#include <HeatRecovery.hh>
#include <HVACDXHeatPumpSystem.hh>
#include <HVACDXSystem.hh>
#include <HVACHXAssistedCoolingCoil.hh>
#include <HVACUnitarySystem.hh>
#include <Humidifiers.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutAirNodeManager.hh>
#include <OutputProcessor.hh>
#include <OutputReportPredefined.hh>
#include <PhotovoltaicThermalCollectors.hh>
#include <Psychrometrics.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <SetPointManager.hh>
#include <SteamCoils.hh>
#include <TranspiredCollector.hh>
#include <UserDefinedComponents.hh>
#include <WaterCoils.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace MixedAir {

	// Module containing the routines dealing with the mixed air portion
	// of the HVAC air loop.

	// MODULE INFORMATION:
	//       AUTHOR         Fred Buhl
	//       DATE WRITTEN   October 1998
	//       MODIFIED       Shirey/Raustad FSEC, June/Aug 2003, Jan 2004
	//                      Lawrie, March 2006 - Module order (per template)
	//                      Craig Wray 22Aug2010 - Added Fan ComponentModel
	//                      Chandan Sharma, FSEC, 25Aug 2011 - Added ProportionalControl
	//                           to enhance CO2 based DCV control
	//                      Feb 2013 Bereket Nigusse, FSEC
	//                        Added DX Coil Model For 100% OA systems
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To encapsulate the data and algorithms required to
	// simulate the mixed air portion of the EPlus air loop.

	// METHODOLOGY EMPLOYED:
	// An algorithmic controller will be employed - there is no attempt to
	// simulate real controllers for the economizer. The mixed air controller
	// will sense various node conditions and set some node flow rates.  Mixed
	// air components will operate with predetermined flow rates.

	// REFERENCES:

	// OTHER NOTES:

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataLoopNode;
	using namespace DataAirLoop;
	using DataGlobals::BeginEnvrnFlag;
	using DataGlobals::BeginDayFlag;
	using DataGlobals::NumOfZones;
	using DataGlobals::SysSizingCalc;
	using DataGlobals::AnyEnergyManagementSystemInModel;
	using DataGlobals::DoZoneSizing;
	using DataGlobals::ScheduleAlwaysOn;
	using DataGlobals::OutputFileInits;
	using namespace DataEnvironment;
	using namespace DataHVACGlobals;
	using namespace ScheduleManager;
	using namespace DataSizing;
	using DataContaminantBalance::Contaminant;
	using DataContaminantBalance::OutdoorCO2;
	using DataContaminantBalance::ZoneCO2GainFromPeople;
	using DataContaminantBalance::ZoneAirCO2;
	using DataContaminantBalance::OutdoorGC;
	using namespace FaultsManager;

	// Data
	// MODULE PARAMETER DEFINITIONS
	int const NoLockoutPossible( 0 );
	int const LockoutWithHeatingPossible( 1 );
	int const LockoutWithCompressorPossible( 2 );

	int const NoEconomizer( 0 );
	// Changed by Amit as a part on New Feature Proposal
	int const FixedDryBulb( 1 );
	int const FixedEnthalpy( 2 );
	int const DifferentialDryBulb( 3 );
	int const DifferentialEnthalpy( 4 );
	int const FixedDewPointAndDryBulb( 5 );
	int const ElectronicEnthalpy( 6 );
	int const DifferentialDryBulbAndEnthalpy( 7 );
	// coil operation
	int const On( 1 ); // normal coil operation
	int const Off( 0 ); // signal coil shouldn't run
	// component types addressed by this module
	int const OAMixer_Num( 1 );
	int const Fan_Simple_CV( 2 );
	int const Fan_Simple_VAV( 3 );
	int const WaterCoil_SimpleCool( 4 );
	int const WaterCoil_Cooling( 5 );
	int const WaterCoil_SimpleHeat( 6 );
	int const SteamCoil_AirHeat( 7 );
	int const WaterCoil_DetailedCool( 8 );
	int const Coil_ElectricHeat( 9 );
	int const Coil_GasHeat( 10 );
	int const WaterCoil_CoolingHXAsst( 11 );
	int const DXSystem( 12 );
	int const HeatXchngr( 13 );
	int const Desiccant( 14 );
	int const Unglazed_SolarCollector( 15 );
	int const EvapCooler( 16 );
	int const PVT_AirBased( 17 );
	int const Fan_ComponentModel( 18 ); // cpw22Aug2010 (new)
	int const DXHeatPumpSystem( 19 );
	int const Coil_UserDefined( 20 );
	int const UnitarySystem( 21 );
	int const Humidifier( 22 );

	int const ControllerOutsideAir( 2 );
	int const ControllerStandAloneERV( 3 );

	//Zone Outdoor Air Method
	//INTEGER, PARAMETER :: ZOAM_FlowPerPerson = 1  ! set the outdoor air flow rate based on number of people in the zone
	//INTEGER, PARAMETER :: ZOAM_FlowPerZone = 2    ! sum the outdoor air flow rate per zone based on user input
	//INTEGER, PARAMETER :: ZOAM_FlowPerArea = 3    ! sum the outdoor air flow rate based on zone area
	//INTEGER, PARAMETER :: ZOAM_FlowPerACH = 4     ! sum the outdoor air flow rate based on number of air changes for the zone
	//INTEGER, PARAMETER :: ZOAM_Sum = 5            ! sum the outdoor air flow rate of the people component and the space floor area component
	//INTEGER, PARAMETER :: ZOAM_Max = 6            ! use the maximum of the outdoor air flow rate of the people component and
	//                                              ! the space floor area component
	//System Outdoor Air Method
	//INTEGER, PARAMETER :: SOAM_ZoneSum = 1  ! Sum the outdoor air flow rates of all zones
	//INTEGER, PARAMETER :: SOAM_VRP = 2      ! Use ASHRAE Standard 62.1-2007 to calculate the system level outdoor air flow rates
	//                                        !  considering the zone air distribution effectiveness and the system ventilation efficiency
	//INTEGER, PARAMETER :: SOAM_IAQP = 3     ! Use ASHRAE Standard 62.1-2007 IAQP to calculate the system level outdoor air flow rates
	//                                        ! based on the CO2 setpoint
	//INTEGER, PARAMETER :: SOAM_ProportionalControl = 4     ! Use ASHRAE Standard 62.1-2004 or Trane Engineer's newsletter (volume 34-5)
	//                                                       ! to calculate the system level outdoor air flow rates
	//INTEGER, PARAMETER :: SOAM_IAQPGC = 5   ! Use ASHRAE Standard 62.1-2004 IAQP to calculate the system level outdoor air flow rates
	//                                        ! based on the generic contaminant setpoint
	//INTEGER, PARAMETER :: SOAM_IAQPCOM = 6  ! Take the maximum outdoor air rate from both CO2 and generic contaminant controls
	//                                        ! based on the generic contaminant setpoint
	//INTEGER, PARAMETER :: SOAM_ProportionalControlDesOcc = 7     ! Use ASHRAE Standard 62.1-2004 or Trane Engineer's newsletter (volume 34-5)
	//                                                       ! to calculate the system level outdoor air flow rates based on design occupancy

	Array1D_string const CurrentModuleObjects( 8, { "AirLoopHVAC:OutdoorAirSystem", "AirLoopHVAC:OutdoorAirSystem:EquipmentList", "AirLoopHVAC:ControllerList", "AvailabilityManagerAssignmentList", "Controller:OutdoorAir", "ZoneHVAC:EnergyRecoveryVentilator:Controller", "Controller:MechanicalVentilation", "OutdoorAir:Mixer" } );

	// Parameters below (CMO - Current Module Object.  used primarily in Get Inputs)
	// Multiple Get Input routines in this module or these would be in individual routines.
	int const CMO_OASystem( 1 );
	int const CMO_AirLoopEqList( 2 );
	int const CMO_ControllerList( 3 );
	int const CMO_SysAvailMgrList( 4 );
	int const CMO_OAController( 5 );
	int const CMO_ERVController( 6 );
	int const CMO_MechVentilation( 7 );
	int const CMO_OAMixer( 8 );

	static std::string const BlankString;

	//Type declarations in MixedAir module

	//MODULE VARIABLE DECLARATIONS:
	int NumControllerLists( 0 ); // Number of Controller Lists
	int NumOAControllers( 0 ); // Number of OA Controllers (includes ERV controllers)
	int NumERVControllers( 0 ); // Number of ERV Controllers
	int NumOAMixers( 0 ); // Number of Outdoor Air Mixers
	int NumVentMechControllers( 0 ); // Number of Controller:MechanicalVentilation objects in input deck

	Array1D_bool MyOneTimeErrorFlag;
	Array1D_bool MyOneTimeCheckUnitarySysFlag;
	bool GetOASysInputFlag( true ); // Flag set to make sure you get input once
	bool GetOAMixerInputFlag( true ); // Flag set to make sure you get input once
	bool GetOAControllerInputFlag( true ); // Flag set to make sure you get input once
	namespace {
	// These were static variables within different functions. They were pulled out into the namespace
	// to facilitate easier unit testing of those functions.
	// These are purposefully not in the header file as an extern variable. No one outside of this should
	// use these. They are cleared by clear_state() for use by unit tests, but normal simulations should be unaffected.
	// This is purposefully in an anonymous namespace so nothing outside this implementation file can use it.
		bool InitOAControllerOneTimeFlag( true );
		Array1D_bool InitOAControllerSetPointCheckFlag( true );
		bool InitOAControllerSetUpAirLoopHVACVariables( true );
		bool AllocateOAControllersFlag( true );
		Array1D_string DesignSpecOAObjName; // name of the design specification outdoor air object
		Array1D_int DesignSpecOAObjIndex; // index of the design specification outdoor air object
		Array1D_string VentMechZoneName; // Zone or Zone List to apply mechanical ventilation rate
		Array1D< Real64 > VentMechZoneOAAreaRate; // Mechanical ventilation rate (m3/s/m2) for zone or zone list
		Array1D< Real64 > VentMechZoneOAPeopleRate; // Mechanical ventilation rate (m3/s/person) for zone or zone list
		Array1D< Real64 > VentMechZoneOAFlow; // Mechanical ventilation rate (m3/s/person) for zone or zone list
		Array1D< Real64 > VentMechZoneOAACH; // Mechanical ventilation rate (m3/s/person) for zone or zone list

		Array1D< Real64 > VentMechZoneADEffCooling; // Zone air distribution effectiveness in cooling mode
		// for each zone or zone list
		Array1D< Real64 > VentMechZoneADEffHeating; // Zone air distribution effectiveness in heating mode
		// for each zone or zone list
		Array1D_int VentMechZoneADEffSchPtr; // Pointer to the zone air distribution effectiveness schedule
		// for each zone or zone list
		Array1D_string VentMechZoneADEffSchName; // Zone air distribution effectiveness
		//  schedule name for each zone or zone list

		Array1D< Real64 > VentMechZoneSecondaryRecirculation; // Zone air secondary recirculation ratio
		//  for each zone or zone list
		Array1D_string DesignSpecZoneADObjName; // name of the design specification zone air
		//  distribution object for each zone or zone list
		Array1D_int DesignSpecZoneADObjIndex; // index of the design specification zone air distribution object
	}
	//SUBROUTINE SPECIFICATIONS FOR MODULE MixedAir
	// Driver/Manager Routines

	// Get Input routines for module

	// Initialization routines for module

	// Algorithms/Calculation routines for the module

	// Sizing routine for the module

	// Update routines to check convergence and update nodes

	// Utility routines for the module

	// Object Data
	Array1D< ControllerListProps > ControllerLists;
	Array1D< OAControllerProps > OAController;
	Array1D< OAMixerProps > OAMixer;
	Array1D< VentilationMechanicalProps > VentilationMechanical;

	// Functions

	Real64 OAGetFlowRate( int OAPtr )
	{
		Real64 FlowRate( 0 );
		if ( ( OAPtr > 0 ) && ( OAPtr <= NumOAControllers ) && ( StdRhoAir != 0 ) )
		{
			FlowRate = OAController( OAPtr ).OAMassFlow / StdRhoAir;
		}
		return FlowRate;
	}
	Real64 OAGetMinFlowRate( int OAPtr )
	{
		Real64 MinFlowRate( 0 );
		if ( ( OAPtr > 0 ) && ( OAPtr <= NumOAControllers ) )
		{
			MinFlowRate = OAController( OAPtr ).MinOA;
		}
		return MinFlowRate;
	}
	void OASetDemandManagerVentilationState( int OAPtr, bool aState )
	{
		if ( ( OAPtr > 0 ) && ( OAPtr <= NumOAControllers ) )
		{
			OAController( OAPtr ).ManageDemand = aState;
		}
	}
	void OASetDemandManagerVentilationFlow( int OAPtr, Real64 aFlow )
	{
		if ( ( OAPtr > 0 ) && ( OAPtr <= NumOAControllers ) )
		{
			OAController( OAPtr ).DemandLimitFlowRate = aFlow * StdRhoAir;
		}
	}
	int GetOAController( std::string const & OAName )
	{
		int CurrentOAController( 0 );
		for ( int i = 1; i <= NumOAControllers; i++ )
		{
			if ( OAName == OAController( i ).Name ) {
				CurrentOAController = i;
				break;
			}
		}
		return CurrentOAController;
	}
	// Clears the global data in MixedAir.
	// Needed for unit tests, should not be normally called.
	void
	clear_state()
	{
		NumControllerLists = 0;
		NumOAControllers = 0;
		NumERVControllers = 0;
		NumOAMixers = 0;
		NumVentMechControllers = 0;
		MyOneTimeErrorFlag.deallocate();
		MyOneTimeCheckUnitarySysFlag.deallocate();
		GetOASysInputFlag = true;
		GetOAMixerInputFlag = true;
		GetOAControllerInputFlag = true;
		InitOAControllerOneTimeFlag = true;
		InitOAControllerSetPointCheckFlag.deallocate();
		InitOAControllerSetUpAirLoopHVACVariables = true;
		AllocateOAControllersFlag = true;
		ControllerLists.deallocate();
		OAController.deallocate();
		OAMixer.deallocate();
		VentilationMechanical.deallocate();
		VentMechZoneName.deallocate();
		DesignSpecOAObjName.deallocate();
		DesignSpecOAObjIndex.deallocate();
		VentMechZoneOAFlow.deallocate();
		VentMechZoneOAACH.deallocate();
		VentMechZoneOAAreaRate.deallocate();
		VentMechZoneOAPeopleRate.deallocate();
		VentMechZoneADEffSchName.deallocate();
		VentMechZoneADEffCooling.deallocate();
		VentMechZoneADEffHeating.deallocate();
		VentMechZoneADEffSchPtr.deallocate();
		VentMechZoneADEffSchName.deallocate();
		VentMechZoneSecondaryRecirculation.deallocate();
		DesignSpecZoneADObjName.deallocate();
		DesignSpecZoneADObjIndex.deallocate();
	}

	void
	ManageOutsideAirSystem(
		std::string const & OASysName,
		bool const FirstHVACIteration,
		int const AirLoopNum,
		int & OASysNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   Oct 1998
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE
		// Manage the outside air system

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		if ( GetOASysInputFlag ) {
			GetOutsideAirSysInputs();
			GetOASysInputFlag = false;
		}

		if ( OASysNum == 0 ) {
			OASysNum = FindItemInList( OASysName, OutsideAirSys );
			if ( OASysNum == 0 ) {
				ShowFatalError( "ManageOutsideAirSystem: AirLoopHVAC:OutdoorAirSystem not found=" + OASysName );
			}
		}

		InitOutsideAirSys( OASysNum, FirstHVACIteration );

		SimOutsideAirSys( OASysNum, FirstHVACIteration, AirLoopNum );

	}

	void
	SimOutsideAirSys(
		int const OASysNum,
		bool const FirstHVACIteration,
		int const AirLoopNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   Oct 1998
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE
		// Simulate the controllers and components in the outside air system.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using InputProcessor::SameString;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int CompNum;
		//INTEGER :: CtrlNum
		int OAMixerNum;
		int OAControllerNum;
		static std::string CompType; //Tuned Made static
		static std::string CompName; //Tuned Made static
		static std::string CtrlName; //Tuned Made static
		bool FatalErrorFlag( false );
		bool Sim( true );
		bool OAHeatCoil( false );
		bool OACoolCoil( false );
		bool OAHX( false );
		bool ReSim( false );

		// SimOutsideAirSys can handle only 1 controller right now.  This must be
		// an Outside Air Controller.  This is because of the lack of iteration
		// and convergence control in the following code.
		//  DO CtrlNum=1,OutsideAirSys(OASysNum)%NumControllers
		//    CtrlName = OutsideAirSys(OASysNum)%ControllerName(CtrlNum)
		//    CALL SimOAController(CtrlName,FirstHVACIteration)
		//  END DO
		CtrlName = OutsideAirSys( OASysNum ).ControllerName( 1 );
		CurOASysNum = OASysNum;
		SimOAController( CtrlName, OutsideAirSys( OASysNum ).ControllerIndex( 1 ), FirstHVACIteration, AirLoopNum );

		for ( CompNum = 1; CompNum <= OutsideAirSys( OASysNum ).NumComponents; ++CompNum ) {
			CompType = OutsideAirSys( OASysNum ).ComponentType( CompNum );
			CompName = OutsideAirSys( OASysNum ).ComponentName( CompNum );
			SimOAComponent( CompType, CompName, OutsideAirSys( OASysNum ).ComponentType_Num( CompNum ), FirstHVACIteration, OutsideAirSys( OASysNum ).ComponentIndex( CompNum ), AirLoopNum, Sim, OASysNum, OAHeatCoil, OACoolCoil, OAHX );
			if ( OAHX ) ReSim = true;
		}
		// if there were heat exchangers and/or desiccant wheel in the OA path, need to simulate again
		// in reverse order to propagate the air flow and conditions out the relief air path to the relief air
		// exit node
		if ( ReSim ) {
			for ( CompNum = OutsideAirSys( OASysNum ).NumComponents - 1; CompNum >= 1; --CompNum ) {
				CompType = OutsideAirSys( OASysNum ).ComponentType( CompNum );
				CompName = OutsideAirSys( OASysNum ).ComponentName( CompNum );
				SimOAComponent( CompType, CompName, OutsideAirSys( OASysNum ).ComponentType_Num( CompNum ), FirstHVACIteration, OutsideAirSys( OASysNum ).ComponentIndex( CompNum ), AirLoopNum, Sim, OASysNum, OAHeatCoil, OACoolCoil, OAHX );
			}
			// now simulate again propogate current temps back through OA system
			for( CompNum = 1; CompNum <= OutsideAirSys( OASysNum ).NumComponents; ++CompNum ) {
				CompType = OutsideAirSys( OASysNum ).ComponentType( CompNum );
				CompName = OutsideAirSys( OASysNum ).ComponentName( CompNum );
				SimOAComponent( CompType, CompName, OutsideAirSys( OASysNum ).ComponentType_Num( CompNum ), FirstHVACIteration, OutsideAirSys( OASysNum ).ComponentIndex( CompNum ), AirLoopNum, Sim, OASysNum, OAHeatCoil, OACoolCoil, OAHX );
			}
		}

		if ( MyOneTimeErrorFlag( OASysNum ) ) {
			if ( OutsideAirSys( OASysNum ).NumControllers - OutsideAirSys( OASysNum ).NumSimpleControllers > 1 ) {
				ShowWarningError( "AirLoopHVAC:OutdoorAirSystem " + OutsideAirSys( OASysNum ).Name + " has more than 1 outside air controller; only the 1st will be used" );
			}
			for ( CompNum = 1; CompNum <= OutsideAirSys( OASysNum ).NumComponents; ++CompNum ) {
				CompType = OutsideAirSys( OASysNum ).ComponentType( CompNum );
				CompName = OutsideAirSys( OASysNum ).ComponentName( CompNum );
				if ( SameString( CompType, "OutdoorAir:Mixer" ) ) {
					OAMixerNum = FindItemInList( CompName, OAMixer );
					OAControllerNum = FindItemInList( CtrlName, OAController );
					if ( OAController( OAControllerNum ).MixNode != OAMixer( OAMixerNum ).MixNode ) {
						ShowSevereError( "The mixed air node of Controller:OutdoorAir=\"" + OAController( OAControllerNum ).Name + "\"" );
						ShowContinueError( "should be the same node as the mixed air node of OutdoorAir:Mixer=\"" + OAMixer( OAMixerNum ).Name + "\"." );
						ShowContinueError( "Controller:OutdoorAir mixed air node=\"" + NodeID( OAController( OAControllerNum ).MixNode ) + "\"." );
						ShowContinueError( "OutdoorAir:Mixer mixed air node=\"" + NodeID( OAMixer( OAMixerNum ).MixNode ) + "\"." );
						FatalErrorFlag = true;
					}
					if ( OAController( OAControllerNum ).RelNode != OAMixer( OAMixerNum ).RelNode ) {
						ShowSevereError( "The relief air node of Controller:OutdoorAir=\"" + OAController( OAControllerNum ).Name + "\"" );
						ShowContinueError( "should be the same node as the relief air node of OutdoorAir:Mixer=\"" + OAMixer( OAMixerNum ).Name + "\"." );
						ShowContinueError( "Controller:OutdoorAir relief air node=\"" + NodeID( OAController( OAControllerNum ).RelNode ) + "\"." );
						ShowContinueError( "OutdoorAir:Mixer relief air node=\"" + NodeID( OAMixer( OAMixerNum ).RelNode ) + "\"." );
						FatalErrorFlag = true;
					}
					if ( OAController( OAControllerNum ).RetNode != OAMixer( OAMixerNum ).RetNode ) {
						ShowSevereError( "The return air node of Controller:OutdoorAir=\"" + OAController( OAControllerNum ).Name + "\"" );
						ShowContinueError( "should be the same node as the return air node of OutdoorAir:Mixer=\"" + OAMixer( OAMixerNum ).Name + "\"." );
						ShowContinueError( "Controller:OutdoorAir return air node=\"" + NodeID( OAController( OAControllerNum ).RetNode ) + "\"." );
						ShowContinueError( "OutdoorAir:Mixer return air node=\"" + NodeID( OAMixer( OAMixerNum ).RetNode ) + "\"." );
						FatalErrorFlag = true;
					}
				}
			}
			MyOneTimeErrorFlag( OASysNum ) = false;
			if ( FatalErrorFlag ) ShowFatalError( "Previous severe error(s) cause program termination" );
		}

		CurOASysNum = 0;
		AirLoopControlInfo( AirLoopNum ).OASysComponentsSimulated = true;

	}

	void
	SimOAComponent(
		std::string const & CompType, // the component type
		std::string const & CompName, // the component Name
		int const CompTypeNum, // Component Type -- Integerized for this module
		bool const FirstHVACIteration,
		int & CompIndex,
		int const AirLoopNum, // air loop index for economizer lockout coordination
		bool const Sim, // if TRUE, simulate component; if FALSE, just set the coil exisitence flags
		int const OASysNum, // index to outside air system
		Optional_bool OAHeatingCoil, // TRUE indicates a heating coil has been found
		Optional_bool OACoolingCoil, // TRUE indicates a cooling coil has been found
		Optional_bool OAHX // TRUE indicates a heat exchanger has been found
	)
	{

		// SUBROUTINE INFORMATION
		//             AUTHOR:  Russ Taylor, Dan Fisher, Fred Buhl
		//       DATE WRITTEN:  Oct 1997
		//           MODIFIED:  Dec 1997 Fred Buhl, D Shirey Feb/Sept 2003
		//                      Nov 2004 M. J. Witte, GARD Analytics, Inc.
		//                        Add DXSystem:AirLoop as valid OA system equipment
		//                        Work supported by ASHRAE research project 1254-RP
		//      RE-ENGINEERED:  This is new code, not reengineered

		// PURPOSE OF THIS SUBROUTINE:
		// Calls the individual air loop component simulation routines

		// METHODOLOGY EMPLOYED: None

		// REFERENCES: None

		// USE Statements
		// Using/Aliasing
		using Fans::SimulateFanComponents;
		using DataAirLoop::AirLoopInputsFilled;
		using WaterCoils::SimulateWaterCoilComponents;
		using HeatingCoils::SimulateHeatingCoilComponents;
		using HeatRecovery::SimHeatRecovery;
		using DesiccantDehumidifiers::SimDesiccantDehumidifier;
		using HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil;
		using HVACDXSystem::SimDXCoolingSystem;
		using HVACDXHeatPumpSystem::SimDXHeatPumpSystem;
		using SteamCoils::SimulateSteamCoilComponents;
		using TranspiredCollector::SimTranspiredCollector;
		using EvaporativeCoolers::SimEvapCooler;
		using PhotovoltaicThermalCollectors::SimPVTcollectors;
		using PhotovoltaicThermalCollectors::CalledFromOutsideAirSystem;
		using UserDefinedComponents::SimCoilUserDefined;
		using HVACUnitarySystem::SimUnitarySystem;
		using HVACUnitarySystem::GetUnitarySystemOAHeatCoolCoil;
		using HVACUnitarySystem::CheckUnitarySysCoilInOASysExists;
		using Humidifiers::SimHumidifier;
		// Locals
		// SUBROUTINE ARGUMENTS:

		// SUBROUTINE PARAMETER DEFINITIONS: None

		// INTERFACE BLOCK DEFINITIONS: None

		// DERIVED TYPE DEFINITIONS: None

		// SUBROUTINE LOCAL VARIABLE DEFINITIONS

		OAHeatingCoil = false;
		OACoolingCoil = false;
		OAHX = false;
		Real64 AirloopPLR;
		int FanOpMode;

		{ auto const SELECT_CASE_var( CompTypeNum );

		if ( SELECT_CASE_var == OAMixer_Num ) { // 'OutdoorAir:Mixer'
			if ( Sim ) {
				SimOAMixer( CompName, FirstHVACIteration, CompIndex );
			}

			// Fan Types
		} else if ( SELECT_CASE_var == Fan_Simple_CV ) { // 'Fan:ConstantVolume'
			if ( Sim ) {
				SimulateFanComponents( CompName, FirstHVACIteration, CompIndex );
			}
		} else if ( SELECT_CASE_var == Fan_Simple_VAV ) { // 'Fan:VariableVolume'
			if ( Sim ) {
				SimulateFanComponents( CompName, FirstHVACIteration, CompIndex );
			}
			//cpw22Aug2010 Add Fan:ComponentModel (new num=18)
		} else if ( SELECT_CASE_var == Fan_ComponentModel ) { // 'Fan:ComponentModel'
			if ( Sim ) {
				SimulateFanComponents( CompName, FirstHVACIteration, CompIndex );
			}

			// Coil Types
		} else if ( SELECT_CASE_var == WaterCoil_Cooling ) { // 'Coil:Cooling:Water'
			if ( Sim ) {
				SimulateWaterCoilComponents( CompName, FirstHVACIteration, CompIndex );
			}
			OACoolingCoil = true;
		} else if ( SELECT_CASE_var == WaterCoil_SimpleHeat ) { // 'Coil:Heating:Water')
			if ( Sim ) {
				SimulateWaterCoilComponents( CompName, FirstHVACIteration, CompIndex );
			}
			OAHeatingCoil = true;
		} else if ( SELECT_CASE_var == SteamCoil_AirHeat ) { // 'Coil:Heating:Steam'
			if ( Sim ) {
				SimulateSteamCoilComponents( CompName, FirstHVACIteration, CompIndex, 0.0 );
			}
			OAHeatingCoil = true;
		} else if ( SELECT_CASE_var == WaterCoil_DetailedCool ) { // 'Coil:Cooling:Water:DetailedGeometry'
			if ( Sim ) {
				SimulateWaterCoilComponents( CompName, FirstHVACIteration, CompIndex );
			}
			OACoolingCoil = true;
		} else if ( SELECT_CASE_var == Coil_ElectricHeat ) { // 'Coil:Heating:Electric'
			if ( Sim ) {
				//     stand-alone coils are temperature controlled (do not pass QCoilReq in argument list, QCoilReq overrides temp SP)
				SimulateHeatingCoilComponents( CompName, FirstHVACIteration, _, CompIndex );
			}
			OAHeatingCoil = true;
		} else if ( SELECT_CASE_var == Coil_GasHeat ) { // 'Coil:Heating:Gas'
			if ( Sim ) {
				//     stand-alone coils are temperature controlled (do not pass QCoilReq in argument list, QCoilReq overrides temp SP)
				SimulateHeatingCoilComponents( CompName, FirstHVACIteration, _, CompIndex );
			}
			OAHeatingCoil = true;
		} else if ( SELECT_CASE_var == WaterCoil_CoolingHXAsst ) { // 'CoilSystem:Cooling:Water:HeatExchangerAssisted'
			if ( Sim ) {
				SimHXAssistedCoolingCoil( CompName, FirstHVACIteration, On, 0.0, CompIndex, ContFanCycCoil );
			}
			OACoolingCoil = true;
		} else if ( SELECT_CASE_var == DXSystem ) { // CoilSystem:Cooling:DX  old 'AirLoopHVAC:UnitaryCoolOnly'
			if ( Sim ) {
				SimDXCoolingSystem( CompName, FirstHVACIteration, AirLoopNum, CompIndex );
			}
			OACoolingCoil = true;
		} else if ( SELECT_CASE_var == UnitarySystem ) { // AirLoopHVAC:UnitarySystem
			if ( Sim ) {
				SimUnitarySystem( CompName, FirstHVACIteration, AirLoopNum, CompIndex );
			}
			if ( AirLoopInputsFilled ) GetUnitarySystemOAHeatCoolCoil( CompName, OACoolingCoil, OAHeatingCoil );
			if ( MyOneTimeCheckUnitarySysFlag( OASysNum ) ) {
				if ( AirLoopInputsFilled ) {
					CheckUnitarySysCoilInOASysExists( CompName );
					MyOneTimeCheckUnitarySysFlag( OASysNum ) = false;
				}
			}
		} else if ( SELECT_CASE_var == DXHeatPumpSystem ) {
			if ( Sim ) {
				SimDXHeatPumpSystem( CompName, FirstHVACIteration, AirLoopNum, CompIndex );
			}
			OAHeatingCoil = true;
		} else if ( SELECT_CASE_var == Coil_UserDefined ) {
			if ( Sim ) {
				SimCoilUserDefined( CompName, CompIndex, AirLoopNum, OAHeatingCoil, OACoolingCoil );
			}
			// Heat recovery
		} else if ( SELECT_CASE_var == HeatXchngr ) { // 'HeatExchanger:AirToAir:FlatPlate', 'HeatExchanger:AirToAir:SensibleAndLatent',
			// 'HeatExchanger:Desiccant:BalancedFlow'
			if ( Sim ) {
				if( AirLoopControlInfo( AirLoopNum ).FanOpMode == DataHVACGlobals::CycFanCycCoil ) {
					FanOpMode = DataHVACGlobals::CycFanCycCoil;
				} else {
					FanOpMode = DataHVACGlobals::ContFanCycCoil;
				}
				if( FanOpMode == DataHVACGlobals::CycFanCycCoil ) {
					// HX's in the OA system can be troublesome given that the OA flow rate is not necessarily proportional to air loop PLR
					// adding that user input for branch flow rate, HX nominal flow rate, OA system min/max flow rate will not necessarily be perfectly input,
					// a compromise is used for OA sys HX's as the ratio of flow to max. Issue #4298.
//					AirloopPLR = AirLoopFlow( AirLoopNum ).FanPLR;
					AirloopPLR = OAController( OASysNum ).OAMassFlow / OAController( OASysNum ).MaxOAMassFlowRate;
				} else {
					AirloopPLR = 1.0;
				}
				SimHeatRecovery( CompName, FirstHVACIteration, CompIndex, FanOpMode, AirloopPLR, _, _, _, AirLoopControlInfo( AirLoopNum ).HeatRecoveryBypass, AirLoopControlInfo( AirLoopNum ).HighHumCtrlActive );
			}
			OAHX = true;

			// Desiccant Dehumidifier
		} else if ( SELECT_CASE_var == Desiccant ) { // 'Dehumidifier:Desiccant:NoFans'
			// 'Dehumidifier:Desiccant:System'
			if ( Sim ) {
				SimDesiccantDehumidifier( CompName, FirstHVACIteration, CompIndex );
			}
			OAHX = true;

			// Humidifiers
		} else if ( SELECT_CASE_var == Humidifier ) { // 'Humidifier:Steam:Electric'
			// 'Humidifier:Steam:Gas'
			if ( Sim ) {
				SimHumidifier( CompName, FirstHVACIteration, CompIndex );
			}

			// Unglazed Transpired Solar Collector
		} else if ( SELECT_CASE_var == Unglazed_SolarCollector ) { // 'SolarCollector:UnglazedTranspired'
			if ( Sim ) {
				SimTranspiredCollector( CompName, CompIndex );
			}

			// Air-based Photovoltaic-thermal flat plate collector
		} else if ( SELECT_CASE_var == PVT_AirBased ) { // 'SolarCollector:FlatPlate:PhotovoltaicThermal'
			if ( Sim ) {
				SimPVTcollectors( CompIndex, FirstHVACIteration, CalledFromOutsideAirSystem, CompName );
			}

			// Evaporative Cooler Types
		} else if ( SELECT_CASE_var == EvapCooler ) { // 'EvaporativeCooler:Direct:CelDekPad','EvaporativeCooler:Indirect:CelDekPad'
			// 'EvaporativeCooler:Indirect:WetCoil','EvaporativeCooler:Indirect:ResearchSpecial'
			if ( Sim ) {
				SimEvapCooler( CompName, CompIndex );
			}

		} else {
			ShowFatalError( "Invalid Outside Air Component=" + CompType );

		}}

	}

	void
	SimOAMixer(
		std::string const & CompName,
		bool const FirstHVACIteration,
		int & CompIndex
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   Oct 1998
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE
		// Simulate an Outside Air Mixer component

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int OAMixerNum;

		if ( GetOAMixerInputFlag ) {
			GetOAMixerInputs();
			GetOAMixerInputFlag = false;
		}

		if ( CompIndex == 0 ) {
			OAMixerNum = FindItemInList( CompName, OAMixer );
			CompIndex = OAMixerNum;
			if ( OAMixerNum == 0 ) {
				ShowFatalError( "SimOAMixer: OutdoorAir:Mixer not found=" + CompName );
			}
		} else {
			OAMixerNum = CompIndex;
		}

		InitOAMixer( OAMixerNum, FirstHVACIteration );

		CalcOAMixer( OAMixerNum );

		UpdateOAMixer( OAMixerNum );

		ReportOAMixer( OAMixerNum );

	}

	void
	SimOAController(
		std::string const & CtrlName,
		int & CtrlIndex,
		bool const FirstHVACIteration,
		int const AirLoopNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   Oct 1998
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE
		// Simulate an Outside Air Controller component

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int OAControllerNum;

		if ( ( GetOAControllerInputFlag ) && ( AirLoopNum > 0 ) ) { // Gets input for object  first time Sim routine is called from an airloop
			GetOAControllerInputs();
			GetOAControllerInputFlag = false;
		}

		if ( CtrlIndex == 0 ) {
			if ( NumOAControllers > 0 ) {
				OAControllerNum = FindItemInList( CtrlName, OAController );
			} else {
				OAControllerNum = 0;
			}
			CtrlIndex = OAControllerNum;
			if ( OAControllerNum == 0 ) {
				ShowFatalError( "SimOAController: Outside Air Controller not found=" + CtrlName );
			}
		} else {
			OAControllerNum = CtrlIndex;
		}

		InitOAController( OAControllerNum, FirstHVACIteration, AirLoopNum );

		OAController( OAControllerNum ).CalcOAController( AirLoopNum );

		OAController( OAControllerNum ).UpdateOAController();

	}

	// Get Input Section of the Module
	//******************************************************************************

	void
	GetOutsideAirSysInputs()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   Oct 1998
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE
		// Input the Outside Air System data and store it in the OutsideAirSys array.

		// METHODOLOGY EMPLOYED:
		// Use the Get routines from the InputProcessor module.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace InputProcessor;
		using BranchNodeConnections::TestCompSet;
		using BranchNodeConnections::SetUpCompSets;
		using HVACDXSystem::CheckDXCoolingCoilInOASysExists;

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetOutsideAirSysInputs: " ); // include trailing blank space

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int NumNums; // Number of real numbers returned by GetObjectItem
		int NumAlphas; // Number of alphanumerics returned by GetObjectItem
		int IOStat;
		Array1D< Real64 > NumArray;
		Array1D_string AlphArray;
		int OASysNum;
		int CompNum;
		int Item;
		//unused0909INTEGER :: NumComponents
		int AlphaNum;
		std::string ComponentListName;
		std::string ControllerListName;
		std::string AvailManagerListName;
		int NumInList;
		int InListNum;
		int ListNum;
		int NumSimpControllers; // number of Controller:Simple objects in an OA System
		static bool ErrorsFound( false );
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		std::string CurrentModuleObject; // Object type for getting and messages
		Array1D_string cAlphaFields; // Alpha field names
		Array1D_string cNumericFields; // Numeric field names
		Array1D_bool lAlphaBlanks; // Logical array, alpha field input BLANK = .TRUE.
		Array1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.
		static int MaxNums( 0 ); // Maximum number of numeric input fields
		static int MaxAlphas( 0 ); // Maximum number of alpha input fields
		static int TotalArgs( 0 ); // Total number of alpha and numeric arguments (max) for a
		//  certain object in the input file

		if ( ! GetOASysInputFlag ) return;

		GetObjectDefMaxArgs( CurrentModuleObjects( CMO_OASystem ), TotalArgs, NumAlphas, NumNums );
		MaxNums = max( MaxNums, NumNums );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		GetObjectDefMaxArgs( CurrentModuleObjects( CMO_AirLoopEqList ), TotalArgs, NumAlphas, NumNums );
		MaxNums = max( MaxNums, NumNums );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		GetObjectDefMaxArgs( CurrentModuleObjects( CMO_ControllerList ), TotalArgs, NumAlphas, NumNums );
		MaxNums = max( MaxNums, NumNums );
		MaxAlphas = max( MaxAlphas, NumAlphas );

		AlphArray.allocate( MaxAlphas );
		cAlphaFields.allocate( MaxAlphas );
		NumArray.dimension( MaxNums, 0.0 );
		cNumericFields.allocate( MaxNums );
		lAlphaBlanks.dimension( MaxAlphas, true );
		lNumericBlanks.dimension( MaxNums, true );

		CurrentModuleObject = CurrentModuleObjects( CMO_ControllerList );
		NumControllerLists = GetNumObjectsFound( CurrentModuleObject );

		ControllerLists.allocate( NumControllerLists );

		for ( Item = 1; Item <= NumControllerLists; ++Item ) {

			// create a reference for convenience
			auto & thisControllerList( ControllerLists( Item ) );
			GetObjectItem( CurrentModuleObject, Item, AlphArray, NumAlphas, NumArray, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( AlphArray( 1 ), ControllerLists, Item - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
			}
			thisControllerList.Name = AlphArray( 1 );
			IsNotOK = false;
			IsBlank = false;
			thisControllerList.NumControllers = ( NumAlphas - 1 ) / 2;
			thisControllerList.ControllerType.allocate( thisControllerList.NumControllers );
			thisControllerList.ControllerName.allocate( thisControllerList.NumControllers );
			AlphaNum = 2;
			for ( CompNum = 1; CompNum <= thisControllerList.NumControllers; ++CompNum ) {
				if ( SameString( AlphArray( AlphaNum ), "Controller:WaterCoil" ) || SameString( AlphArray( AlphaNum ), "Controller:OutdoorAir" ) ) {
					thisControllerList.ControllerType( CompNum ) = AlphArray( AlphaNum );
					thisControllerList.ControllerName( CompNum ) = AlphArray( AlphaNum + 1 );
					// loop over all previous controller lists to check if this controllers is also present on previous controllers
					for ( int previousListNum = 1; previousListNum < Item; ++previousListNum ) {
						// loop over each of the controllers listed for this list
						auto & previousList( ControllerLists( previousListNum ) );
						for ( int PreviousListControllerNum = 1; PreviousListControllerNum <= previousList.NumControllers; ++PreviousListControllerNum ) {
							if ( ( previousList.ControllerType( PreviousListControllerNum ) == thisControllerList.ControllerType( CompNum ) ) && ( previousList.ControllerName( PreviousListControllerNum ) == thisControllerList.ControllerName( CompNum ) ) ) {
								ShowSevereError( "Controller instance repeated in multiple " + CurrentModuleObject + " objects" );
								ShowContinueError( "Found in " + CurrentModuleObject + " = " + thisControllerList.Name );
								ShowContinueError( "Also found in " + CurrentModuleObject + " = " + previousList.Name );
								ErrorsFound = true;
							}
						}
					}
				} else {
					ShowSevereError( "For " + CurrentModuleObject + "=\"" + AlphArray( 1 ) + "\" invalid " + cAlphaFields( AlphaNum ) );
					ShowContinueError( "...entered=\"" + AlphArray( AlphaNum ) + "\", should be Controller:WaterCoil or Controller:OutdoorAir." );
					ErrorsFound = true;
				}
				AlphaNum += 2;
			}

		}

		CurrentModuleObject = CurrentModuleObjects( CMO_OASystem );

		NumOASystems = GetNumObjectsFound( CurrentModuleObject );

		OutsideAirSys.allocate( NumOASystems );
		OASysEqSizing.allocate( NumOASystems );
		MyOneTimeErrorFlag.dimension( NumOASystems, true );
		MyOneTimeCheckUnitarySysFlag.dimension( NumOASystems,true );

		for ( OASysNum = 1; OASysNum <= NumOASystems; ++OASysNum ) {

			GetObjectItem( CurrentModuleObject, OASysNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( AlphArray( 1 ), OutsideAirSys, OASysNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
			}
			OutsideAirSys( OASysNum ).Name = AlphArray( 1 );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( AlphArray( 2 ), OutsideAirSys, &OutsideAirSysProps::ControllerListName, OASysNum - 1, IsNotOK, IsBlank, CurrentModuleObject + ' ' + cAlphaFields( 2 ) + " Name" );
			if ( IsNotOK && AlphArray( 1 ) != "xxxxx" ) {
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = \"" + AlphArray( 1 ) + "\"." );
				ErrorsFound = true;
				if ( IsBlank ) AlphArray( 2 ) = "xxxxx";
			}
			ControllerListName = AlphArray( 2 );
			OutsideAirSys( OASysNum ).ControllerListName = AlphArray( 2 );
			ComponentListName = AlphArray( 3 );
			OutsideAirSys( OASysNum ).ComponentListName = AlphArray( 3 );
			AvailManagerListName = AlphArray( 4 );

			TestCompSet( CurrentModuleObject, AlphArray( 1 ), "UNDEFINED", "UNDEFINED", "Air Nodes" );

			if ( ! lAlphaBlanks( 3 ) ) {
				ListNum = GetObjectItemNum( CurrentModuleObjects( CMO_AirLoopEqList ), ComponentListName );
				if ( ListNum > 0 ) {
					GetObjectItem( CurrentModuleObjects( CMO_AirLoopEqList ), ListNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat );
					NumInList = ( NumAlphas - 1 ) / 2;
					OutsideAirSys( OASysNum ).NumComponents = NumInList;
					OutsideAirSys( OASysNum ).ComponentName.allocate( NumInList );
					OutsideAirSys( OASysNum ).ComponentType.allocate( NumInList );
					OutsideAirSys( OASysNum ).ComponentType_Num.dimension( NumInList, 0 );
					OutsideAirSys( OASysNum ).ComponentIndex.dimension( NumInList, 0 );
					for ( InListNum = 1; InListNum <= NumInList; ++InListNum ) {
						OutsideAirSys( OASysNum ).ComponentName( InListNum ) = AlphArray( InListNum * 2 + 1 );
						OutsideAirSys( OASysNum ).ComponentType( InListNum ) = AlphArray( InListNum * 2 );

						// Add equipment to component sets array
						SetUpCompSets( CurrentModuleObject, OutsideAirSys( OASysNum ).Name, OutsideAirSys( OASysNum ).ComponentType( InListNum ), OutsideAirSys( OASysNum ).ComponentName( InListNum ), "UNDEFINED", "UNDEFINED" );
					}
				} else {
					ShowSevereError( CurrentModuleObject + " = \"" + AlphArray( 1 ) + "\" invalid " + cAlphaFields( 3 ) + "=\"" + AlphArray( 3 ) + "\" not found." );
					ErrorsFound = true;
				}
			} else {
				ShowSevereError( CurrentModuleObject + " = \"" + AlphArray( 1 ) + "\" invalid " + cAlphaFields( 3 ) + " is blank and must be entered." );
				ErrorsFound = true;
			}

			ListNum = 0;
			NumSimpControllers = 0;
			if ( ! lAlphaBlanks( 2 ) ) {
				ListNum = GetObjectItemNum( CurrentModuleObjects( CMO_ControllerList ), ControllerListName );
				if ( ListNum > 0 ) {
					GetObjectItem( CurrentModuleObjects( CMO_ControllerList ), ListNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat );
					NumInList = ( NumAlphas - 1 ) / 2;
					OutsideAirSys( OASysNum ).NumControllers = NumInList;
					OutsideAirSys( OASysNum ).ControllerName.allocate( NumInList );
					OutsideAirSys( OASysNum ).ControllerType.allocate( NumInList );
					OutsideAirSys( OASysNum ).ControllerIndex.dimension( NumInList, 0 );
					for ( InListNum = 1; InListNum <= NumInList; ++InListNum ) {
						OutsideAirSys( OASysNum ).ControllerName( InListNum ) = AlphArray( InListNum * 2 + 1 );
						OutsideAirSys( OASysNum ).ControllerType( InListNum ) = AlphArray( InListNum * 2 );
						if ( ! SameString( OutsideAirSys( OASysNum ).ControllerType( InListNum ), CurrentModuleObjects( CMO_OAController ) ) ) {
							++NumSimpControllers;
						}
					}
				} else {
					ShowSevereError( CurrentModuleObject + " = \"" + AlphArray( 1 ) + "\" invalid " + cAlphaFields( 2 ) + "=\"" + AlphArray( 2 ) + "\" not found." );
					ErrorsFound = true;
				}
			} else {
				ShowSevereError( CurrentModuleObject + " = \"" + AlphArray( 1 ) + "\" invalid " + cAlphaFields( 2 ) + " is blank and must be entered." );
				ErrorsFound = true;
			}
			OutsideAirSys( OASysNum ).ControllerListNum = ListNum;
			OutsideAirSys( OASysNum ).NumSimpleControllers = NumSimpControllers;

			if ( ! lAlphaBlanks( 4 ) ) {
				ListNum = GetObjectItemNum( CurrentModuleObjects( CMO_SysAvailMgrList ), AvailManagerListName );
				if ( ListNum <= 0 ) {
					ShowSevereError( CurrentModuleObject + " = \"" + AlphArray( 1 ) + "\" invalid " + cAlphaFields( 4 ) + "=\"" + AlphArray( 4 ) + "\" not found." );
					ErrorsFound = true;
				}
			}
		}

		for ( OASysNum = 1; OASysNum <= NumOASystems; ++OASysNum ) {
			for ( CompNum = 1; CompNum <= OutsideAirSys( OASysNum ).NumComponents; ++CompNum ) {

				{ auto const SELECT_CASE_var( MakeUPPERCase( OutsideAirSys( OASysNum ).ComponentType( CompNum ) ) );

				if ( SELECT_CASE_var == "OUTDOORAIR:MIXER" ) {
					OutsideAirSys( OASysNum ).ComponentType_Num( CompNum ) = OAMixer_Num;

					// Fan Types
				} else if ( SELECT_CASE_var == "FAN:CONSTANTVOLUME" ) {
					OutsideAirSys( OASysNum ).ComponentType_Num( CompNum ) = Fan_Simple_CV;
				} else if ( SELECT_CASE_var == "FAN:VARIABLEVOLUME" ) {
					OutsideAirSys( OASysNum ).ComponentType_Num( CompNum ) = Fan_Simple_VAV;
					//cpw22Aug2010 Add Fan:ComponentModel (new)
				} else if ( SELECT_CASE_var == "FAN:COMPONENTMODEL" ) {
					OutsideAirSys( OASysNum ).ComponentType_Num( CompNum ) = Fan_ComponentModel;

					// Coil Types
				} else if ( SELECT_CASE_var == "COIL:COOLING:WATER" ) {
					OutsideAirSys( OASysNum ).ComponentType_Num( CompNum ) = WaterCoil_Cooling;
				} else if ( SELECT_CASE_var == "COIL:HEATING:WATER" ) {
					OutsideAirSys( OASysNum ).ComponentType_Num( CompNum ) = WaterCoil_SimpleHeat;
				} else if ( SELECT_CASE_var == "COIL:HEATING:STEAM" ) {
					OutsideAirSys( OASysNum ).ComponentType_Num( CompNum ) = SteamCoil_AirHeat;
				} else if ( SELECT_CASE_var == "COIL:COOLING:WATER:DETAILEDGEOMETRY" ) {
					OutsideAirSys( OASysNum ).ComponentType_Num( CompNum ) = WaterCoil_DetailedCool;
				} else if ( SELECT_CASE_var == "COIL:HEATING:ELECTRIC" ) {
					OutsideAirSys( OASysNum ).ComponentType_Num( CompNum ) = Coil_ElectricHeat;
				} else if ( SELECT_CASE_var == "COIL:HEATING:GAS" ) {
					OutsideAirSys( OASysNum ).ComponentType_Num( CompNum ) = Coil_GasHeat;
				} else if ( SELECT_CASE_var == "COILSYSTEM:COOLING:WATER:HEATEXCHANGERASSISTED" ) {
					OutsideAirSys( OASysNum ).ComponentType_Num( CompNum ) = WaterCoil_CoolingHXAsst;
				} else if ( SELECT_CASE_var == "COILSYSTEM:COOLING:DX" ) {
					OutsideAirSys( OASysNum ).ComponentType_Num( CompNum ) = DXSystem;
					// set the data for 100% DOAS DX cooling coil
					CheckDXCoolingCoilInOASysExists( OutsideAirSys( OASysNum ).ComponentName( CompNum ) );
				} else if ( SELECT_CASE_var == "COILSYSTEM:HEATING:DX" ) {
					OutsideAirSys( OASysNum ).ComponentType_Num( CompNum ) = DXHeatPumpSystem;
				} else if ( SELECT_CASE_var == "AIRLOOPHVAC:UNITARYSYSTEM" ) {
					OutsideAirSys( OASysNum ).ComponentType_Num( CompNum ) = UnitarySystem;
				} else if ( SELECT_CASE_var == "COIL:USERDEFINED" ) {
					OutsideAirSys( OASysNum ).ComponentType_Num( CompNum ) = Coil_UserDefined;
					// Heat recovery
				} else if ( SELECT_CASE_var == "HEATEXCHANGER:AIRTOAIR:FLATPLATE" ) {
					OutsideAirSys( OASysNum ).ComponentType_Num( CompNum ) = HeatXchngr;
				} else if ( SELECT_CASE_var == "HEATEXCHANGER:AIRTOAIR:SENSIBLEANDLATENT" ) {
					OutsideAirSys( OASysNum ).ComponentType_Num( CompNum ) = HeatXchngr;
				} else if ( SELECT_CASE_var == "HEATEXCHANGER:DESICCANT:BALANCEDFLOW" ) {
					OutsideAirSys( OASysNum ).ComponentType_Num( CompNum ) = HeatXchngr;

					// Desiccant Dehumidifier
				} else if ( SELECT_CASE_var == "DEHUMIDIFIER:DESICCANT:NOFANS" ) {
					OutsideAirSys( OASysNum ).ComponentType_Num( CompNum ) = Desiccant;
				} else if ( SELECT_CASE_var == "DEHUMIDIFIER:DESICCANT:SYSTEM" ) {
					OutsideAirSys( OASysNum ).ComponentType_Num( CompNum ) = Desiccant;
					// Humidifiers: Humidifier:Steam:Electric and Humidifier:Steam:Gas
				} else if ( SELECT_CASE_var == "HUMIDIFIER:STEAM:ELECTRIC" ) {
					OutsideAirSys( OASysNum ).ComponentType_Num( CompNum ) = Humidifier;
				} else if ( SELECT_CASE_var == "HUMIDIFIER:STEAM:GAS" ) {
					OutsideAirSys( OASysNum ).ComponentType_Num( CompNum ) = Humidifier;

					// Unglazed Transpired Solar Collector
				} else if ( SELECT_CASE_var == "SOLARCOLLECTOR:UNGLAZEDTRANSPIRED" ) {
					OutsideAirSys( OASysNum ).ComponentType_Num( CompNum ) = Unglazed_SolarCollector;

					// PVT air heater
				} else if ( SELECT_CASE_var == "SOLARCOLLECTOR:FLATPLATE:PHOTOVOLTAICTHERMAL" ) {
					OutsideAirSys( OASysNum ).ComponentType_Num( CompNum ) = PVT_AirBased;
					// Evaporative Cooler Types
				} else if ( SELECT_CASE_var == "EVAPORATIVECOOLER:DIRECT:CELDEKPAD" ) {
					OutsideAirSys( OASysNum ).ComponentType_Num( CompNum ) = EvapCooler;
				} else if ( SELECT_CASE_var == "EVAPORATIVECOOLER:INDIRECT:CELDEKPAD" ) {
					OutsideAirSys( OASysNum ).ComponentType_Num( CompNum ) = EvapCooler;
				} else if ( SELECT_CASE_var == "EVAPORATIVECOOLER:INDIRECT:WETCOIL" ) {
					OutsideAirSys( OASysNum ).ComponentType_Num( CompNum ) = EvapCooler;
				} else if ( SELECT_CASE_var == "EVAPORATIVECOOLER:INDIRECT:RESEARCHSPECIAL" ) {
					OutsideAirSys( OASysNum ).ComponentType_Num( CompNum ) = EvapCooler;
				} else if ( SELECT_CASE_var == "EVAPORATIVECOOLER:DIRECT:RESEARCHSPECIAL" ) {
					OutsideAirSys( OASysNum ).ComponentType_Num( CompNum ) = EvapCooler;
				} else {
					ShowSevereError( CurrentModuleObject + " = \"" + AlphArray( 1 ) + "\" invalid Outside Air Component=\"" + OutsideAirSys( OASysNum ).ComponentType( CompNum ) + "\"." );
					ErrorsFound = true;

				}}
			}

		}

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in getting " + CurrentModuleObject + '.' );
		}

		AlphArray.deallocate();
		cAlphaFields.deallocate();
		NumArray.deallocate();
		cNumericFields.deallocate();
		lAlphaBlanks.deallocate();
		lNumericBlanks.deallocate();

		GetOASysInputFlag = false;

	}

	void
	GetOAControllerInputs()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   Oct 1998
		//       MODIFIED       Shirey/Raustad FSEC, June 2003, Jan 2004
		//                      Mangesh Basarkar, 06/2011: Getting zone OA specifications from Design Specification Object
		//                      Tianzhen Hong, 3/2012: getting zone air distribution effectiveness and secondary recirculation
		//                       from DesignSpecification:ZoneAirDistribution objects

		// PURPOSE OF THIS SUBROUTINE
		// Input the OAController data and store it in the OAController array.
		// Input the Ventilation:Mechanical data and store it in the VentilationMechanical array.
		//  Condense Ventilation:Mechanical data array to include only unique zones specified for each instance of this object.

		// METHODOLOGY EMPLOYED:
		// Use the Get routines from the InputProcessor module.

		// Using/Aliasing
		using namespace InputProcessor;
		using namespace DataDefineEquip;
		using General::TrimSigDigits;
		using General::RoundSigDigits;
		using NodeInputManager::GetOnlySingleNode;
		using DataZoneEquipment::ZoneEquipConfig;
		using DataZoneEquipment::ZoneEquipList;
		using DataZoneEquipment::NumOfZoneEquipLists;
		using DataHeatBalance::Zone;
		using DataHeatBalance::ZoneList;
		using CurveManager::GetCurveIndex;
		using CurveManager::GetCurveType;
		using namespace OutputReportPredefined;

		using DataContaminantBalance::Contaminant;
		using OutAirNodeManager::CheckOutAirNodeNumber;

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetOAControllerInputs: " ); // include trailing blank space

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int MaxNumAirLoopZones; // maximum number of heating plus cooling zones attached to any air loop
		int NumAirLoopZones; // number of heating plus cooling zones attached to a given air loop
		int NumofAirLoop; // counter for NumPrimaryAirSys
		int NumAirLoopCooledZones; // number of cooling zones for a given air loop
		int NumAirLoopCooledZonesTemp; // index for number of cooling zones
		int AirLoopZones; // total number of unique heating and cooling zones for each air loop
		int NumAirLoopHeatedZones; // number of heating zones for a given air loop
		int NumAirLoopHeatedZonesTemp; // index for number of heating zones
		int ZoneNum; // zone number attached to a given air loop
		bool CommonZone; // logical for the same zone being a cooling zone and a heating zone
		int NumNums; // Number of real numbers returned by GetObjectItem
		int NumAlphas; // Number of alphanumerics returned by GetObjectItem
		int OutAirNum; // Number of Controller:OutdoorAir or CONTROLLER:STAND ALONE ERV objects
		int OAControllerNum; // Index to Controller:OutdoorAir or CONTROLLER:STAND ALONE ERV objects
		int VentMechNum; // Number of VENTILATION:MECHANICAL objects
		int groupNum; // Index to group in extensible VENTILATION:MECHANICAL object
		int IOStat; // Status of GetObjectItem call
		Array1D< Real64 > NumArray;
		Array1D_string AlphArray;
		std::string CurrentModuleObject; // Object type for getting and messages
		Array1D_string cAlphaFields; // Alpha field names
		Array1D_string cNumericFields; // Numeric field names
		Array1D_bool lAlphaBlanks; // Logical array, alpha field input BLANK = .TRUE.
		Array1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.
		static bool ErrorsFound( false ); // Flag identifying errors found during get input
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		int ZoneListNum; // Index to Zone List
		int ScanZoneListNum; // Index used to loop through zone list
		int MechVentZoneCount; // Index counter for zones with mechanical ventilation
		bool ErrorInName; // Error returned from VerifyName call
		int NumArg; // Number of arguments from GetObjectDefMaxArgs call
		int MaxAlphas; // Maximum alphas in multiple objects
		int MaxNums; // Maximum numbers in multiple objects

		int NumGroups; // Number of extensible input groups of the VentilationMechanical object
		static int ObjIndex( 0 );
		static int EquipListIndex( 0 );
		static int EquipNum( 0 );
		static int EquipListNum( 0 );
		static int ADUNum( 0 );
		int jZone;
		int i;

		// Formats
		static gio::Fmt Format_700( "('!<Controller:MechanicalVentilation>,Name,Availability Schedule Name,Demand Controlled Ventilation {Yes/No},','System Outdoor Air Method,Zone Maximum Outdoor Air Fraction,Number of Zones,Zone Name,DSOA Name,DSZAD Name')" );
		static gio::Fmt fmtA( "(A)" );

		//First, call other get input routines in this module to make sure data is filled during this routine.
		if ( GetOASysInputFlag ) { // Gets input for object  first time Sim routine is called
			GetOutsideAirSysInputs();
			GetOASysInputFlag = false;
		}
		if ( GetOAMixerInputFlag ) { // Gets input for object  first time Sim routine is called
			GetOAMixerInputs();
			GetOAMixerInputFlag = false;
		}

		GetObjectDefMaxArgs( CurrentModuleObjects( CMO_OAController ), NumArg, NumAlphas, NumNums );
		MaxAlphas = NumAlphas;
		MaxNums = NumNums;
		GetObjectDefMaxArgs( CurrentModuleObjects( CMO_ERVController ), NumArg, NumAlphas, NumNums );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		MaxNums = max( MaxNums, NumNums );
		GetObjectDefMaxArgs( CurrentModuleObjects( CMO_MechVentilation ), NumArg, NumAlphas, NumNums );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		MaxNums = max( MaxNums, NumNums );

		AlphArray.allocate( MaxAlphas );
		NumArray.dimension( MaxNums, 0.0 );
		lAlphaBlanks.dimension( MaxAlphas, true );
		lNumericBlanks.dimension( MaxNums, true );
		cAlphaFields.allocate( MaxAlphas );
		cNumericFields.allocate( MaxNums );


		// Count OAcontrollers and ERVcontrollers and allocate arrays
		AllocateOAControllers();


		// If there are ERV controllers, they have been filled before now NumOAControllers includes the count of NumERVControllers
		if ( NumOAControllers > NumERVControllers ) {
			CurrentModuleObject = CurrentModuleObjects( CMO_OAController );
			int currentOAControllerNum = 0;
			for ( OutAirNum = NumERVControllers+1; OutAirNum <= NumOAControllers; ++OutAirNum ) {
				++currentOAControllerNum;
				GetObjectItem( CurrentModuleObject, currentOAControllerNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );
				IsNotOK = false;
				IsBlank = false;
				VerifyName( AlphArray( 1 ), OAController, OutAirNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
				}

				ProcessOAControllerInputs( CurrentModuleObject, OutAirNum, AlphArray, NumAlphas, NumArray, NumNums, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields, ErrorsFound );

				// add applicable faults identifier to avoid string comparison at each time step
				//  loop through each fault for each OA controller
				for ( i = 1; i <= NumFaults; ++i ) {
					if ( Faults( i ).ControllerTypeEnum != iController_AirEconomizer ) continue;
					if ( SameString( OAController( OutAirNum ).Name, Faults( i ).ControllerName ) ) {
						Faults( i ).ControllerID = OutAirNum;
					}
				}

			} // LOOP FOR OutAirNum

			if ( ErrorsFound ) {
				AlphArray.deallocate();
				NumArray.deallocate();
				lNumericBlanks.deallocate();
				lAlphaBlanks.deallocate();
				cAlphaFields.deallocate();
				cNumericFields.deallocate();
				ShowFatalError( RoutineName + "Errors found in getting " + CurrentModuleObject + " inputs." );
			}

		}

		GetOAControllerInputFlag = false;

		// Find the maximum number of zones attached to any air loop, used for mechanical ventilation objects
		MaxNumAirLoopZones = 0;
		for ( NumofAirLoop = 1; NumofAirLoop <= NumPrimaryAirSys; ++NumofAirLoop ) {
			NumAirLoopZones = AirToZoneNodeInfo( NumofAirLoop ).NumZonesCooled + AirToZoneNodeInfo( NumofAirLoop ).NumZonesHeated;
			// NumZonesCooled + NumZonesHeated must be > 0 or Fatal error is issued in SimAirServingZones
			MaxNumAirLoopZones = max( MaxNumAirLoopZones, NumAirLoopZones ); // Max number of zones on any air loop being simulated
		}

		if ( NumPrimaryAirSys > 0 ) {
			AirLoopZoneInfo.allocate( NumPrimaryAirSys ); // Defined in DataAirLoop.cc
		}

		// Find the zones attached to each air loop
		for ( NumofAirLoop = 1; NumofAirLoop <= NumPrimaryAirSys; ++NumofAirLoop ) {
			AirLoopZoneInfo( NumofAirLoop ).Zone.allocate( MaxNumAirLoopZones );
			AirLoopZoneInfo( NumofAirLoop ).ActualZoneNumber.allocate( MaxNumAirLoopZones );
			NumAirLoopCooledZones = AirToZoneNodeInfo( NumofAirLoop ).NumZonesCooled;
			AirLoopZones = NumAirLoopCooledZones;
			NumAirLoopHeatedZones = AirToZoneNodeInfo( NumofAirLoop ).NumZonesHeated;
			// Store cooling zone numbers in AirLoopZoneInfo data structure
			for ( NumAirLoopCooledZonesTemp = 1; NumAirLoopCooledZonesTemp <= NumAirLoopCooledZones; ++NumAirLoopCooledZonesTemp ) {
				AirLoopZoneInfo( NumofAirLoop ).Zone( NumAirLoopCooledZonesTemp ) = AirToZoneNodeInfo( NumofAirLoop ).CoolCtrlZoneNums( NumAirLoopCooledZonesTemp );
				AirLoopZoneInfo( NumofAirLoop ).ActualZoneNumber( NumAirLoopCooledZonesTemp ) = ZoneEquipConfig( AirToZoneNodeInfo( NumofAirLoop ).CoolCtrlZoneNums( NumAirLoopCooledZonesTemp ) ).ActualZoneNum;
			}
			// Store heating zone numbers in AirLoopZoneInfo data structure
			// Only store zone numbers that aren't already defined as cooling zones above
			for ( NumAirLoopHeatedZonesTemp = 1; NumAirLoopHeatedZonesTemp <= NumAirLoopHeatedZones; ++NumAirLoopHeatedZonesTemp ) {
				ZoneNum = AirToZoneNodeInfo( NumofAirLoop ).HeatCtrlZoneNums( NumAirLoopHeatedZonesTemp );
				CommonZone = false;
				for ( NumAirLoopCooledZonesTemp = 1; NumAirLoopCooledZonesTemp <= NumAirLoopCooledZones; ++NumAirLoopCooledZonesTemp ) {
					if ( ZoneNum != AirToZoneNodeInfo( NumofAirLoop ).CoolCtrlZoneNums( NumAirLoopCooledZonesTemp ) ) continue;
					CommonZone = true;
				}
				if ( ! CommonZone ) {
					++AirLoopZones;
					AirLoopZoneInfo( NumofAirLoop ).Zone( AirLoopZones ) = ZoneNum;
					AirLoopZoneInfo( NumofAirLoop ).ActualZoneNumber( AirLoopZones ) = ZoneEquipConfig( ZoneNum ).ActualZoneNum;
				}
			}
			AirLoopZoneInfo( NumofAirLoop ).NumZones = AirLoopZones;
		}

		// Process Controller:MechanicalVentilation objects
		CurrentModuleObject = CurrentModuleObjects( CMO_MechVentilation );
		NumVentMechControllers = GetNumObjectsFound( CurrentModuleObject );
		if ( NumVentMechControllers > 0 ) {
			VentilationMechanical.allocate( NumVentMechControllers );
			for ( VentMechNum = 1; VentMechNum <= NumVentMechControllers; ++VentMechNum ) {
				GetObjectItem( CurrentModuleObject, VentMechNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

				MechVentZoneCount = 0;

				NumGroups = ( NumAlphas + NumNums - 5 ) / 3;
				if ( mod( ( NumAlphas + NumNums - 5 ), 3 ) != 0 ) ++NumGroups;
				VentilationMechanical( VentMechNum ).Name = AlphArray( 1 );

				// Check Controller:MechanicalVentilation object name
				ErrorInName = false;
				IsBlank = false;
				VerifyName( AlphArray( 1 ), VentilationMechanical, VentMechNum - 1, ErrorInName, IsBlank, CurrentModuleObject + " Name" );
				if ( ErrorInName ) {
					ErrorsFound = true;
					if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
				}

				VentilationMechanical( VentMechNum ).SchName = AlphArray( 2 );
				if ( lAlphaBlanks( 2 ) ) {
					VentilationMechanical( VentMechNum ).SchPtr = ScheduleAlwaysOn;
				} else {
					VentilationMechanical( VentMechNum ).SchPtr = GetScheduleIndex( AlphArray( 2 ) ); // convert schedule name to pointer
					if ( VentilationMechanical( VentMechNum ).SchPtr == 0 ) {
						ShowSevereError( CurrentModuleObject + "=\"" + AlphArray( 1 ) + "\" invalid " + cAlphaFields( 2 ) + "=\"" + AlphArray( 2 ) + "\" not found." );
						ErrorsFound = true;
					}
				}

				// Adding new flag for DCV
				if ( SameString( AlphArray( 3 ), "Yes" ) ) {
					VentilationMechanical( VentMechNum ).DCVFlag = true;
				} else if ( SameString( AlphArray( 3 ), "No" ) || lAlphaBlanks( 3 ) ) {
					VentilationMechanical( VentMechNum ).DCVFlag = false;
				} else {
					ShowSevereError( CurrentModuleObject + "=\"" + AlphArray( 1 ) + "\" invalid value " + cAlphaFields( 3 ) + "=\"" + AlphArray( 3 ) + "\"." );
					ShowContinueError( "...Valid values are \"Yes\" or \"No\"." );
					ErrorsFound = true;
				}

				// System outdoor air method
				{ auto const SELECT_CASE_var( MakeUPPERCase( AlphArray( 4 ) ) );
				if ( SELECT_CASE_var == "ZONESUM" ) { // Simplifily sum the zone OA flow rates
					VentilationMechanical( VentMechNum ).SystemOAMethod = SOAM_ZoneSum;
				} else if ( ( SELECT_CASE_var == "VRP" ) || ( SELECT_CASE_var == "VENTILATIONRATEPROCEDURE" ) ) { // Ventilation Rate Procedure based on ASHRAE Standard 62.1-2007
					if ( SameString( AlphArray( 4 ), "VRP" ) ) {
						ShowSevereError( CurrentModuleObject + "=\"" + AlphArray( 1 ) + "\"." );
						ShowContinueError( "Deprecated value in " + cAlphaFields( 4 ) + "=\"" + AlphArray( 4 ) + "\", using VentilationRateProcedure." );
					}
					VentilationMechanical( VentMechNum ).SystemOAMethod = SOAM_VRP;
				} else if ( ( SELECT_CASE_var == "IAQP" ) || ( SELECT_CASE_var == "INDOORAIRQUALITYPROCEDURE" ) ) { // Indoor Air Quality Procedure based on ASHRAE Standard 62.1-2007
					VentilationMechanical( VentMechNum ).SystemOAMethod = SOAM_IAQP;
					if ( SameString( AlphArray( 4 ), "IAQP" ) ) {
						ShowSevereError( CurrentModuleObject + "=\"" + AlphArray( 1 ) + "\"." );
						ShowContinueError( "Deprecated value in " + cAlphaFields( 4 ) + "=\"" + AlphArray( 4 ) + "\", using IndoorAirQualityProcedure." );
					}
					if ( ! Contaminant.CO2Simulation ) {
						ShowSevereError( CurrentModuleObject + "=\"" + AlphArray( 1 ) + "\" valid " + cAlphaFields( 2 ) + "=\"" + AlphArray( 2 ) + "\" requires CO2 simulation." );
						ShowContinueError( "The choice must be Yes for the field Carbon Dioxide Concentration in ZoneAirContaminantBalance" );
						ErrorsFound = true;
					}
				} else if ( SELECT_CASE_var == "PROPORTIONALCONTROLBASEDONOCCUPANCYSCHEDULE" ) { // Proportional Control based on ASHRAE Standard 62.1-2004
					VentilationMechanical( VentMechNum ).SystemOAMethod = SOAM_ProportionalControlSchOcc;
					if ( ! Contaminant.CO2Simulation ) {
						ShowSevereError( CurrentModuleObject + "=\"" + AlphArray( 1 ) + "\" valid " + cAlphaFields( 2 ) + "=\"" + AlphArray( 2 ) + "\" requires CO2 simulation." );
						ShowContinueError( "The choice must be Yes for the field Carbon Dioxide Concentration in ZoneAirContaminantBalance" );
						ErrorsFound = true;
					}
				} else if ( SELECT_CASE_var == "PROPORTIONALCONTROLBASEDONDESIGNOCCUPANCY" ) { // Proportional Control based on ASHRAE Standard 62.1-2004
					VentilationMechanical( VentMechNum ).SystemOAMethod = SOAM_ProportionalControlDesOcc;
					if ( !Contaminant.CO2Simulation ) {
						ShowSevereError( CurrentModuleObject + "=\"" + AlphArray( 1 ) + "\" valid " + cAlphaFields( 2 ) + "=\"" + AlphArray( 2 ) + "\" requires CO2 simulation." );
						ShowContinueError( "The choice must be Yes for the field Carbon Dioxide Concentration in ZoneAirContaminantBalance" );
						ErrorsFound = true;
					}
				} else if ( SELECT_CASE_var == "INDOORAIRQUALITYPROCEDUREGENERICCONTAMINANT" ) { // Indoor Air Quality Procedure based on generic contaminant setpoint
					VentilationMechanical( VentMechNum ).SystemOAMethod = SOAM_IAQPGC;
					if ( ! Contaminant.GenericContamSimulation ) {
						ShowSevereError( CurrentModuleObject + "=\"" + AlphArray( 1 ) + "\" valid " + cAlphaFields( 2 ) + "=\"" + AlphArray( 2 ) + "\" requires generic contaminant simulation." );
						ShowContinueError( "The choice must be Yes for the field Generic Contaminant Concentration in ZoneAirContaminantBalance" );
						ErrorsFound = true;
					}
				} else if ( SELECT_CASE_var == "INDOORAIRQUALITYPROCEDURECOMBINED" ) { // Indoor Air Quality Procedure based on both generic contaminant and CO2 setpoint
					VentilationMechanical( VentMechNum ).SystemOAMethod = SOAM_IAQPCOM;
					if ( ! Contaminant.GenericContamSimulation ) {
						ShowSevereError( CurrentModuleObject + "=\"" + AlphArray( 1 ) + "\" valid " + cAlphaFields( 2 ) + "=\"" + AlphArray( 2 ) + "\" requires generic contaminant simulation." );
						ShowContinueError( "The choice must be Yes for the field Generic Contaminant Concentration in ZoneAirContaminantBalance" );
						ErrorsFound = true;
					}
					if ( ! Contaminant.CO2Simulation ) {
						ShowSevereError( CurrentModuleObject + "=\"" + AlphArray( 1 ) + "\" valid " + cAlphaFields( 2 ) + "=\"" + AlphArray( 2 ) + "\" requires CO2 simulation." );
						ShowContinueError( "The choice must be Yes for the field Carbon Dioxide Concentration in ZoneAirContaminantBalance" );
						ErrorsFound = true;
					}
				} else { // If specified incorrectly, show errors
					VentilationMechanical( VentMechNum ).SystemOAMethod = SOAM_ZoneSum;
					ShowWarningError( CurrentModuleObject + "=\"" + AlphArray( 1 ) + "\" incorrect specification for " + cAlphaFields( 4 ) + ", the ZoneSum method will be used." );
					//ErrorsFound=.TRUE.
				}}

				//Zone maximum outdoor air fraction
				VentilationMechanical( VentMechNum ).ZoneMaxOAFraction = NumArray( 1 );

				VentMechZoneName.allocate( NumGroups );
				DesignSpecOAObjName.allocate( NumGroups );
				DesignSpecOAObjIndex.dimension( NumGroups, 0 );
				VentMechZoneOAAreaRate.dimension( NumGroups, 0.0 );
				VentMechZoneOAPeopleRate.dimension( NumGroups, 0.0 );
				VentMechZoneOAFlow.allocate( NumGroups );
				VentMechZoneOAACH.allocate( NumGroups );
				// use defaults for Cooling and Heating Effectiveness
				VentMechZoneADEffCooling.dimension( NumGroups, 1.0 );
				VentMechZoneADEffHeating.dimension( NumGroups, 1.0 );
				VentMechZoneADEffSchPtr.dimension( NumGroups, 0 );
				VentMechZoneADEffSchName.allocate( NumGroups );

				VentMechZoneSecondaryRecirculation.dimension( NumGroups, 0.0 );
				DesignSpecZoneADObjName.allocate( NumGroups );
				DesignSpecZoneADObjIndex.dimension( NumGroups, 0 );

				//   First time through find the total number of zones requiring mechanical ventilation
				//   May include duplicate zones. Will check for duplicate zones further down in this subroutine.
				for ( groupNum = 1; groupNum <= NumGroups; ++groupNum ) {
					VentMechZoneName( groupNum ) = AlphArray( ( groupNum - 1 ) * 3 + 5 );

					//     Getting OA details from design specification OA object
					if ( ! lAlphaBlanks( ( groupNum - 1 ) * 3 + 6 ) ) {
						DesignSpecOAObjName( groupNum ) = AlphArray( ( groupNum - 1 ) * 3 + 6 );
						ObjIndex = FindItemInList( DesignSpecOAObjName( groupNum ), OARequirements );
						DesignSpecOAObjIndex( groupNum ) = ObjIndex;

						if ( ObjIndex > 0 ) {
							VentMechZoneOAAreaRate( groupNum ) = OARequirements( ObjIndex ).OAFlowPerArea;
							VentMechZoneOAPeopleRate( groupNum ) = OARequirements( ObjIndex ).OAFlowPerPerson;
							VentMechZoneOAFlow( groupNum ) = OARequirements( ObjIndex ).OAFlowPerZone;
							VentMechZoneOAACH( groupNum ) = OARequirements( ObjIndex ).OAFlowACH;
						} else {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VentilationMechanical( VentMechNum ).Name + "\", invalid" );
							ShowContinueError( "... not found " + cAlphaFields( ( groupNum - 1 ) * 3 + 6 ) + "=\"" + DesignSpecOAObjName( groupNum ) + "\"." );
							ErrorsFound = true;
						}
					}

					// Get zone air distribution details from design specification Zone Air Distribution object
					if ( ! lAlphaBlanks( ( groupNum - 1 ) * 3 + 7 ) ) {
						DesignSpecZoneADObjName( groupNum ) = AlphArray( ( groupNum - 1 ) * 3 + 7 );
						ObjIndex = FindItemInList( DesignSpecZoneADObjName( groupNum ), ZoneAirDistribution );
						DesignSpecZoneADObjIndex( groupNum ) = ObjIndex;

						if ( ObjIndex > 0 ) {
							// found the design specification Zone Air Distribution object
							VentMechZoneADEffCooling( groupNum ) = ZoneAirDistribution( ObjIndex ).ZoneADEffCooling;
							VentMechZoneADEffHeating( groupNum ) = ZoneAirDistribution( ObjIndex ).ZoneADEffHeating;
							VentMechZoneSecondaryRecirculation( groupNum ) = ZoneAirDistribution( ObjIndex ).ZoneSecondaryRecirculation;
							VentMechZoneADEffSchName( groupNum ) = ZoneAirDistribution( ObjIndex ).ZoneADEffSchName;
							VentMechZoneADEffSchPtr( groupNum ) = GetScheduleIndex( VentMechZoneADEffSchName( groupNum ) );
						} else {
							// Cannot find the design specification Zone Air Distribution object
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VentilationMechanical( VentMechNum ).Name + "\", invalid" );
							ShowContinueError( "... not found " + cAlphaFields( ( groupNum - 1 ) * 3 + 7 ) + "=\"" + DesignSpecZoneADObjName( groupNum ) + "\"." );
							ErrorsFound = true;
						}
					}

					ZoneNum = FindItemInList( VentMechZoneName( groupNum ), Zone );
					if ( ZoneNum > 0 ) {
						++MechVentZoneCount;
					} else {
						ZoneListNum = FindItemInList( VentMechZoneName( groupNum ), ZoneList );
						if ( ZoneListNum > 0 ) {
							MechVentZoneCount += ZoneList( ZoneListNum ).NumOfZones;
						} else {
							ShowWarningError( CurrentModuleObject + "=\"" + AlphArray( 1 ) + "\" invalid " + cAlphaFields( ( groupNum - 1 ) * 3 + 5 ) + " not found." );
							ShowContinueError( "Missing " + cAlphaFields( ( groupNum - 1 ) * 3 + 5 ) + " = " + VentMechZoneName( groupNum ) );
							ErrorsFound = true;
						}
					}
				}

				VentilationMechanical( VentMechNum ).NumofVentMechZones = MechVentZoneCount;

				// Now allocate and store unique zone and associated ventilation rate information
				VentilationMechanical( VentMechNum ).VentMechZone.dimension( MechVentZoneCount, 0 );
				VentilationMechanical( VentMechNum ).ZoneDesignSpecOAObjName.dimension( MechVentZoneCount );
				VentilationMechanical( VentMechNum ).ZoneDesignSpecOAObjIndex.dimension( MechVentZoneCount, 0 );
				VentilationMechanical( VentMechNum ).ZoneOAAreaRate.dimension( MechVentZoneCount, 0.0 );
				VentilationMechanical( VentMechNum ).ZoneOAPeopleRate.dimension( MechVentZoneCount, 0.0 );
				VentilationMechanical( VentMechNum ).ZoneOAFlowRate.dimension( MechVentZoneCount, 0.0 );
				VentilationMechanical( VentMechNum ).ZoneOAACHRate.dimension( MechVentZoneCount, 0.0 );

				// added for new DCV, 2/12/2009
				VentilationMechanical( VentMechNum ).ZoneADEffCooling.dimension( MechVentZoneCount, 1.0 );
				// Zone air distribution effectiveness in heating mode
				VentilationMechanical( VentMechNum ).ZoneADEffHeating.dimension( MechVentZoneCount, 1.0 );
				// Indices to the zone air distribution effectiveness schedules
				VentilationMechanical( VentMechNum ).ZoneADEffSchPtr.dimension( MechVentZoneCount, 0 );
				// Zone air distribution effectiveness schedule names
				VentilationMechanical( VentMechNum ).ZoneADEffSchName.allocate( MechVentZoneCount );
				// Zone air secondary recirculation ratio, added 3/2012
				VentilationMechanical( VentMechNum ).ZoneSecondaryRecirculation.dimension( MechVentZoneCount, 0.0 );
				VentilationMechanical( VentMechNum ).ZoneDesignSpecADObjName.allocate( MechVentZoneCount );
				VentilationMechanical( VentMechNum ).ZoneDesignSpecADObjIndex.dimension( MechVentZoneCount, 0 );

				MechVentZoneCount = 0;

				//   Loop through zone names and list of zone names and store data
				for ( groupNum = 1; groupNum <= NumGroups; ++groupNum ) {
					ZoneNum = FindItemInList( VentMechZoneName( groupNum ), Zone );
					if ( ZoneNum > 0 ) {
						if ( any_eq( VentilationMechanical( VentMechNum ).VentMechZone, ZoneNum ) ) {
							//          Disregard duplicate zone names, show warning and do not store data for this zone
							ShowWarningError( "Zone name = " + VentMechZoneName( groupNum ) + " for " + CurrentModuleObject + " object = " + VentilationMechanical( VentMechNum ).Name );
							ShowContinueError( "is specified more than once. The first ventilation values specified for this zone will be used" );
							ShowContinueError( "and the rest will be ignored. Simulation will continue.." );
						} else {
							//          Store unique zone names
							++MechVentZoneCount;
							VentilationMechanical( VentMechNum ).VentMechZone( MechVentZoneCount ) = ZoneNum;
							// Populating new temp array to hold design spec OA object for each zone
							if ( ! DesignSpecOAObjName( groupNum ).empty() ) {
								VentilationMechanical( VentMechNum ).ZoneDesignSpecOAObjName( MechVentZoneCount ) = DesignSpecOAObjName( groupNum );
								VentilationMechanical( VentMechNum ).ZoneDesignSpecOAObjIndex( MechVentZoneCount ) = DesignSpecOAObjIndex( groupNum );
								VentilationMechanical( VentMechNum ).ZoneOAAreaRate( MechVentZoneCount ) = VentMechZoneOAAreaRate( groupNum );
								VentilationMechanical( VentMechNum ).ZoneOAPeopleRate( MechVentZoneCount ) = VentMechZoneOAPeopleRate( groupNum );
								VentilationMechanical( VentMechNum ).ZoneOAFlowRate( MechVentZoneCount ) = VentMechZoneOAFlow( groupNum );
								VentilationMechanical( VentMechNum ).ZoneOAACHRate = VentMechZoneOAACH( groupNum );
							} else {
								if ( DoZoneSizing ) {
									ObjIndex = FindItemInList( VentMechZoneName( groupNum ), ZoneSizingInput, &ZoneSizingInputData::ZoneName );
									if ( ObjIndex > 0 ) {
										VentilationMechanical( VentMechNum ).ZoneDesignSpecOAObjName( MechVentZoneCount ) = ZoneSizingInput( ObjIndex ).DesignSpecOAObjName;
										VentilationMechanical( VentMechNum ).ZoneDesignSpecOAObjIndex( MechVentZoneCount ) = ZoneSizingInput( ObjIndex ).ZoneDesignSpecOAIndex;
										ObjIndex = VentilationMechanical( VentMechNum ).ZoneDesignSpecOAObjIndex( MechVentZoneCount );
										if ( ObjIndex > 0 ) {
											VentilationMechanical( VentMechNum ).ZoneOAAreaRate( MechVentZoneCount ) = OARequirements( ObjIndex ).OAFlowPerArea;
											VentilationMechanical( VentMechNum ).ZoneOAPeopleRate( MechVentZoneCount ) = OARequirements( ObjIndex ).OAFlowPerPerson;
											VentilationMechanical( VentMechNum ).ZoneOAFlowRate( MechVentZoneCount ) = OARequirements( ObjIndex ).OAFlowPerZone;
											VentilationMechanical( VentMechNum ).ZoneOAACHRate = OARequirements( ObjIndex ).OAFlowACH;
										} else { // use defaults
											VentilationMechanical( VentMechNum ).ZoneOAAreaRate( MechVentZoneCount ) = 0.0;
											// since this is case with no DesSpcOA object, cannot determine the method and default would be Flow/Person which should default to this flow rate
											VentilationMechanical( VentMechNum ).ZoneOAPeopleRate( MechVentZoneCount ) = 0.00944;
											VentilationMechanical( VentMechNum ).ZoneOAFlowRate( MechVentZoneCount ) = 0.0;
											VentilationMechanical( VentMechNum ).ZoneOAACHRate = 0.0;
										}
									} else {
										ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VentilationMechanical( VentMechNum ).Name + "\", missing" );
										ShowContinueError( "...blank (required entry) - cannot locate in Sizing:Zone for Zone=\"" + VentMechZoneName( groupNum ) + "\"." );
										ErrorsFound = true;
									}
								}
							}
							// Zone Air Distribution inputs
							if ( ! DesignSpecZoneADObjName( groupNum ).empty() ) {
								// new DCV inputs
								VentilationMechanical( VentMechNum ).ZoneADEffCooling( MechVentZoneCount ) = VentMechZoneADEffCooling( groupNum );
								VentilationMechanical( VentMechNum ).ZoneADEffHeating( MechVentZoneCount ) = VentMechZoneADEffHeating( groupNum );
								VentilationMechanical( VentMechNum ).ZoneADEffSchPtr( MechVentZoneCount ) = VentMechZoneADEffSchPtr( groupNum );
								VentilationMechanical( VentMechNum ).ZoneADEffSchName( MechVentZoneCount ) = VentMechZoneADEffSchName( groupNum );
								VentilationMechanical( VentMechNum ).ZoneSecondaryRecirculation( MechVentZoneCount ) = VentMechZoneSecondaryRecirculation( groupNum );
								VentilationMechanical( VentMechNum ).ZoneDesignSpecADObjName( MechVentZoneCount ) = DesignSpecZoneADObjName( groupNum );
								VentilationMechanical( VentMechNum ).ZoneDesignSpecADObjIndex( MechVentZoneCount ) = DesignSpecZoneADObjIndex( groupNum );
							} else {
								if ( DoZoneSizing ) {
									ObjIndex = FindItemInList( VentMechZoneName( groupNum ), ZoneSizingInput, &ZoneSizingInputData::ZoneName );
									if ( ObjIndex > 0 ) {
										VentilationMechanical( VentMechNum ).ZoneDesignSpecADObjName( MechVentZoneCount ) = ZoneSizingInput( ObjIndex ).ZoneAirDistEffObjName;
										VentilationMechanical( VentMechNum ).ZoneDesignSpecADObjIndex( MechVentZoneCount ) = ZoneSizingInput( ObjIndex ).ZoneAirDistributionIndex;
										ObjIndex = VentilationMechanical( VentMechNum ).ZoneDesignSpecADObjIndex( MechVentZoneCount );
									} else {
										ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VentilationMechanical( VentMechNum ).Name + "\", missing" );
										ShowContinueError( "...blank (required entry) - cannot locate in Sizing:Zone for Zone=\"" + VentMechZoneName( groupNum ) + "\"." );
										ErrorsFound = true;
									}
								}
							}
						}
					} else {
						//       Not a zone name, must be a zone list
						ZoneListNum = FindItemInList( VentMechZoneName( groupNum ), ZoneList );
						if ( ZoneListNum > 0 ) {
							for ( ScanZoneListNum = 1; ScanZoneListNum <= ZoneList( ZoneListNum ).NumOfZones; ++ScanZoneListNum ) {
								// check to make sure zone name is unique (not listed more than once)...
								ZoneNum = ZoneList( ZoneListNum ).Zone( ScanZoneListNum );
								if ( any_eq( VentilationMechanical( VentMechNum ).VentMechZone, ZoneNum ) ) {
									//             Disregard duplicate zone names, show warning and do not store data for this zone
									ShowWarningError( "Zone name = " + Zone( ZoneNum ).Name + " in ZoneList = " + VentMechZoneName( groupNum ) + " for " + CurrentModuleObject + " object = " + VentilationMechanical( VentMechNum ).Name );
									ShowContinueError( "is a duplicate. The first ventilation values specified for this zone will be used " );
									ShowContinueError( "and the rest will be ignored. The simulation will continue..." );
								} else {
									//           Store data for each zone name from zone list (duplicate zone names accounted for in HeatBalanceManager)
									++MechVentZoneCount;
									VentilationMechanical( VentMechNum ).VentMechZone( MechVentZoneCount ) = ZoneNum;
									// Populating new temp array to hold design spec OA object for each zone
									if ( ! DesignSpecOAObjName( groupNum ).empty() ) {
										VentilationMechanical( VentMechNum ).ZoneDesignSpecOAObjName( MechVentZoneCount ) = DesignSpecOAObjName( groupNum );
										VentilationMechanical( VentMechNum ).ZoneDesignSpecOAObjIndex( MechVentZoneCount ) = DesignSpecOAObjIndex( groupNum );
										ObjIndex = VentilationMechanical( VentMechNum ).ZoneDesignSpecOAObjIndex( MechVentZoneCount );
									} else {
										if ( DoZoneSizing ) {
											ObjIndex = FindItemInList( Zone( ZoneList( ZoneListNum ).Zone( ScanZoneListNum ) ).Name, ZoneSizingInput, &ZoneSizingInputData::ZoneName );
											if ( ObjIndex > 0 ) {
												VentilationMechanical( VentMechNum ).ZoneDesignSpecOAObjName( MechVentZoneCount ) = ZoneSizingInput( ObjIndex ).DesignSpecOAObjName;
												VentilationMechanical( VentMechNum ).ZoneDesignSpecOAObjIndex( MechVentZoneCount ) = ZoneSizingInput( ObjIndex ).ZoneDesignSpecOAIndex;
												ObjIndex = VentilationMechanical( VentMechNum ).ZoneDesignSpecOAObjIndex( MechVentZoneCount );
												if ( ObjIndex > 0 ) {
													VentilationMechanical( VentMechNum ).ZoneADEffCooling( MechVentZoneCount ) = ZoneAirDistribution( ObjIndex ).ZoneADEffCooling;
													VentilationMechanical( VentMechNum ).ZoneADEffHeating( MechVentZoneCount ) = ZoneAirDistribution( ObjIndex ).ZoneADEffHeating;
													VentilationMechanical( VentMechNum ).ZoneADEffSchPtr( MechVentZoneCount ) = ZoneAirDistribution( ObjIndex ).ZoneADEffSchPtr;
													VentilationMechanical( VentMechNum ).ZoneADEffSchName( MechVentZoneCount ) = ZoneAirDistribution( ObjIndex ).ZoneADEffSchName;
													VentilationMechanical( VentMechNum ).ZoneSecondaryRecirculation( MechVentZoneCount ) = ZoneAirDistribution( ObjIndex ).ZoneSecondaryRecirculation;
												}
											} else {
												ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VentilationMechanical( VentMechNum ).Name + "\", missing" );
												ShowContinueError( "...blank (required entry) - cannot locate in Sizing:Zone for Zone=\"" + Zone( ZoneList( ZoneListNum ).Zone( ScanZoneListNum ) ).Name + "\"." );
												ErrorsFound = true;
											}
										}
									}
									if ( ObjIndex > 0 ) {
										VentilationMechanical( VentMechNum ).ZoneOAAreaRate( MechVentZoneCount ) = OARequirements( ObjIndex ).OAFlowPerArea;
										VentilationMechanical( VentMechNum ).ZoneOAPeopleRate( MechVentZoneCount ) = OARequirements( ObjIndex ).OAFlowPerPerson;
										VentilationMechanical( VentMechNum ).ZoneOAFlowRate( MechVentZoneCount ) = OARequirements( ObjIndex ).OAFlowPerZone;
										VentilationMechanical( VentMechNum ).ZoneOAACHRate( MechVentZoneCount ) = OARequirements( ObjIndex ).OAFlowACH;
									}

									if ( ! VentilationMechanical( VentMechNum ).ZoneDesignSpecADObjName( MechVentZoneCount ).empty() ) {
										// new DCV inputs
										VentilationMechanical( VentMechNum ).ZoneDesignSpecADObjName( MechVentZoneCount ) = DesignSpecZoneADObjName( groupNum );
										VentilationMechanical( VentMechNum ).ZoneDesignSpecADObjIndex( MechVentZoneCount ) = DesignSpecZoneADObjIndex( groupNum );
										ObjIndex = VentilationMechanical( VentMechNum ).ZoneDesignSpecADObjIndex( MechVentZoneCount );
									} else {
										if ( DoZoneSizing ) {
											ObjIndex = FindItemInList( Zone( ZoneList( ZoneListNum ).Zone( ScanZoneListNum ) ).Name, ZoneSizingInput, &ZoneSizingInputData::ZoneName );
											if ( ObjIndex > 0 ) {
												VentilationMechanical( VentMechNum ).ZoneDesignSpecADObjName( MechVentZoneCount ) = ZoneSizingInput( ObjIndex ).ZoneAirDistEffObjName;
												VentilationMechanical( VentMechNum ).ZoneDesignSpecADObjIndex( MechVentZoneCount ) = ZoneSizingInput( ObjIndex ).ZoneAirDistributionIndex;
												ObjIndex = VentilationMechanical( VentMechNum ).ZoneDesignSpecADObjIndex( MechVentZoneCount );
											} else {
												ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + VentilationMechanical( VentMechNum ).Name + "\", missing" );
												ShowContinueError( "...blank (required entry) - cannot locate in Sizing:Zone for Zone=\"" + Zone( ZoneList( ZoneListNum ).Zone( ScanZoneListNum ) ).Name + "\"." );
												ErrorsFound = true;
											}
										}
									}
									if ( ObjIndex > 0 ) {
										VentilationMechanical( VentMechNum ).ZoneADEffCooling( MechVentZoneCount ) = ZoneAirDistribution( ObjIndex ).ZoneADEffCooling;
										VentilationMechanical( VentMechNum ).ZoneADEffHeating( MechVentZoneCount ) = ZoneAirDistribution( ObjIndex ).ZoneADEffHeating;
										VentilationMechanical( VentMechNum ).ZoneADEffSchPtr( MechVentZoneCount ) = ZoneAirDistribution( ObjIndex ).ZoneADEffSchPtr;
										VentilationMechanical( VentMechNum ).ZoneADEffSchName( MechVentZoneCount ) = ZoneAirDistribution( ObjIndex ).ZoneADEffSchName;
										VentilationMechanical( VentMechNum ).ZoneSecondaryRecirculation( MechVentZoneCount ) = ZoneAirDistribution( ObjIndex ).ZoneSecondaryRecirculation;
									}
								}
							}
						}
					}
				}

				//   Overwrite previous number of zones with number that does not include duplicates
				VentilationMechanical( VentMechNum ).NumofVentMechZones = MechVentZoneCount;

				VentMechZoneName.deallocate();
				DesignSpecOAObjName.deallocate();
				DesignSpecOAObjIndex.deallocate();
				VentMechZoneOAAreaRate.deallocate();
				VentMechZoneOAPeopleRate.deallocate();
				VentMechZoneADEffCooling.deallocate();
				VentMechZoneADEffHeating.deallocate();
				VentMechZoneADEffSchPtr.deallocate();
				VentMechZoneADEffSchName.deallocate();
				VentMechZoneOAFlow.deallocate();
				VentMechZoneOAACH.deallocate();

				DesignSpecZoneADObjName.deallocate();
				DesignSpecZoneADObjIndex.deallocate();
				VentMechZoneSecondaryRecirculation.deallocate();

			}

			for ( VentMechNum = 1; VentMechNum <= NumVentMechControllers; ++VentMechNum ) {
				for ( jZone = 1; jZone <= VentilationMechanical( VentMechNum ).NumofVentMechZones; ++jZone ) {
					if ( VentilationMechanical( VentMechNum ).SystemOAMethod == SOAM_ProportionalControlSchOcc ) {
						if ( VentilationMechanical( VentMechNum ).ZoneOAACHRate( jZone ) > 0.0 || VentilationMechanical( VentMechNum ).ZoneOAFlowRate( jZone ) > 0.0 ) {
							ShowWarningError( CurrentModuleObject + "=\"" + VentilationMechanical( VentMechNum ).Name + "\", inappropriate outdoor air method" );
							ShowContinueError( "Inappropriate method for Design Specification Outdoor Air Object Name=\"" + VentilationMechanical( VentMechNum ).ZoneDesignSpecOAObjName( jZone ) + "\"." );
							ShowContinueError( "For Zone=\"" + Zone( VentilationMechanical( VentMechNum ).VentMechZone( jZone ) ).Name + "\"." );
							ShowContinueError( "Since System Outdoor Air Method= ProportionalControlBasedonOccupancySchedule\", AirChanges/Hour or Flow/Zone outdoor air methods are not valid. Simulation continues.... " );
						}
					}
					if ( VentilationMechanical( VentMechNum ).SystemOAMethod == SOAM_ProportionalControlDesOcc ) {
						if ( VentilationMechanical( VentMechNum ).ZoneOAACHRate( jZone ) > 0.0 || VentilationMechanical( VentMechNum ).ZoneOAFlowRate( jZone ) > 0.0 ) {
							ShowWarningError( CurrentModuleObject + "=\"" + VentilationMechanical( VentMechNum ).Name + "\", inappropriate outdoor air method" );
							ShowContinueError( "Inappropriate method for Design Specification Outdoor Air Object Name=\"" + VentilationMechanical( VentMechNum ).ZoneDesignSpecOAObjName( jZone ) + "\"." );
							ShowContinueError( "For Zone=\"" + Zone( VentilationMechanical( VentMechNum ).VentMechZone( jZone ) ).Name + "\"." );
							ShowContinueError( "Since System Outdoor Air Method= ProportionalControlBasedonDesignOccupancy\", AirChanges/Hour or Flow/Zone outdoor air methods are not valid. Simulation continues.... " );
						}
					}

					// Error check to see if a single duct air terminal is assigned to a zone that has zone secondary recirculation
					if ( VentilationMechanical( VentMechNum ).ZoneSecondaryRecirculation( jZone ) > 0.0 ) {
						ZoneNum = VentilationMechanical( VentMechNum ).VentMechZone( jZone );
						if ( ZoneNum > 0 ) {
							EquipListIndex = ZoneEquipConfig( ZoneNum ).EquipListIndex;
							if ( EquipListIndex > 0 ) {
								for ( EquipListNum = 1; EquipListNum <= NumOfZoneEquipLists; ++EquipListNum ) {
									if ( EquipListNum == EquipListIndex ) {
										for ( EquipNum = 1; EquipNum <= ZoneEquipList( EquipListNum ).NumOfEquipTypes; ++EquipNum ) {
											if ( SameString( ZoneEquipList( EquipListNum ).EquipType( EquipNum ), "ZONEHVAC:AIRDISTRIBUTIONUNIT" ) ) {
												for ( ADUNum = 1; ADUNum <= NumAirDistUnits; ++ADUNum ) {
													if ( SameString( ZoneEquipList( EquipListNum ).EquipName( EquipNum ), AirDistUnit( ADUNum ).Name ) ) {
														if ( ( AirDistUnit( ADUNum ).EquipType_Num( EquipNum ) == SingleDuctVAVReheat ) || ( AirDistUnit( ADUNum ).EquipType_Num( EquipNum ) == SingleDuctConstVolReheat ) || ( AirDistUnit( ADUNum ).EquipType_Num( EquipNum ) == SingleDuctVAVNoReheat ) || ( AirDistUnit( ADUNum ).EquipType_Num( EquipNum ) == SingleDuctVAVReheatVSFan ) || ( AirDistUnit( ADUNum ).EquipType_Num( EquipNum ) == SingleDuctCBVAVReheat ) || ( AirDistUnit( ADUNum ).EquipType_Num( EquipNum ) == SingleDuctCBVAVNoReheat ) || ( AirDistUnit( ADUNum ).EquipType_Num( EquipNum ) == SingleDuctConstVolCooledBeam ) || ( AirDistUnit( ADUNum ).EquipType_Num( EquipNum ) == SingleDuctConstVolFourPipeBeam )  || ( AirDistUnit( ADUNum ).EquipType_Num( EquipNum ) == DualDuctVAVOutdoorAir ) ) {
															ShowWarningError( CurrentModuleObject + "=\"" + VentilationMechanical( VentMechNum ).Name + "\", inappropriate use of Zone secondary recirculation" );
															ShowContinueError( "A zone secondary recirculation fraction is specified for zone served by " );
															ShowContinueError( "...terminal unit \"" + AirDistUnit( ADUNum ).Name + "\" , that indicates a single path system" );
															ShowContinueError( "For Zone=\"" + Zone( VentilationMechanical( VentMechNum ).VentMechZone( jZone ) ).Name + "\"." );
															ShowContinueError( "...The zone secondary recirculation for that zone was set to 0.0" );
															VentilationMechanical( VentMechNum ).ZoneSecondaryRecirculation( jZone ) = 0.0;
														}
														goto EquipLoop_exit;
													}
												}
											}
										}
									}
								}
								EquipLoop_exit: ;
							}
						}
					}
					if ( VentilationMechanical( VentMechNum ).ZoneDesignSpecOAObjName( jZone ).empty() ) {
						ShowSevereError( CurrentModuleObject + "=\"" + VentilationMechanical( VentMechNum ).Name + "\", Design Specification Outdoor Air Object Name blank" );
						ShowContinueError( "For Zone=\"" + Zone( VentilationMechanical( VentMechNum ).VentMechZone( jZone ) ).Name + "\"." );
						ShowContinueError( "This field either needs to be filled in in this object or Sizing:Zone object." );
						ShowContinueError( "For this run, default values for these fields will be used." );
					}
					if ( VentilationMechanical( VentMechNum ).ZoneOAPeopleRate( jZone ) <= 0.0 && VentilationMechanical( VentMechNum ).DCVFlag ) {
						ShowWarningError( CurrentModuleObject + "=\"" + VentilationMechanical( VentMechNum ).Name + "\", Zone OA/person rate" );
						ShowContinueError( "For Zone=\"" + Zone( VentilationMechanical( VentMechNum ).VentMechZone( jZone ) ).Name + "\"." );
						ShowContinueError( "Zone outside air per person rate not set in Design Specification Outdoor Air Object=\"" + VentilationMechanical( VentMechNum ).ZoneDesignSpecOAObjName( jZone ) + "\"." );
					}

					if ( VentilationMechanical( VentMechNum ).ZoneOAAreaRate( jZone ) < 0.0 ) {
						ShowSevereError( CurrentModuleObject + "=\"" + VentilationMechanical( VentMechNum ).Name + "\", invalid Outdoor Air flow per area" );
						ShowContinueError( "For Zone=\"" + Zone( VentilationMechanical( VentMechNum ).VentMechZone( jZone ) ).Name + "\"." );
						ShowContinueError( "invalid Outdoor Air flow per area specified in object=\"" + OARequirements( VentilationMechanical( VentMechNum ).ZoneDesignSpecOAObjIndex( jZone ) ).Name + "\". Value must be >= 0.0." );
						ErrorsFound = true;
					}
					if ( VentilationMechanical( VentMechNum ).ZoneOAPeopleRate( jZone ) < 0.0 ) {
						ShowSevereError( CurrentModuleObject + "=\"" + VentilationMechanical( VentMechNum ).Name + "\", invalid Outdoor Air flow per person" );
						ShowContinueError( "For Zone=\"" + Zone( VentilationMechanical( VentMechNum ).VentMechZone( jZone ) ).Name + "\"." );
						ShowContinueError( "invalid Outdoor Air flow per person specified in object \"" + OARequirements( VentilationMechanical( VentMechNum ).ZoneDesignSpecOAObjIndex( jZone ) ).Name + "\". Value must be >= 0.0." );
						ErrorsFound = true;
					}
				}
			}

			// Link OA controller object with mechanical ventilation object
			for ( OAControllerNum = 1; OAControllerNum <= NumOAControllers; ++OAControllerNum ) {
				OAController( OAControllerNum ).VentMechObjectNum = FindItemInList( OAController( OAControllerNum ).VentilationMechanicalName, VentilationMechanical );
				if ( OAController( OAControllerNum ).VentMechObjectNum == 0 && ! OAController( OAControllerNum ).VentilationMechanicalName.empty() ) {
					ShowSevereError( CurrentModuleObject + "=\"" + OAController( OAControllerNum ).VentilationMechanicalName + "\", non-match to Controller:OutdoorAir" );
					ShowContinueError( "Invalid specified in Controller:OutdoorAir object = " + OAController( OAControllerNum ).Name );
					ShowContinueError( CurrentModuleObject + " object name must match the " + CurrentModuleObject + " object name specified in Controller:OutdoorAir." );
					ErrorsFound = true;
				}
			}

			// write to .eio file
			gio::write( OutputFileInits, Format_700 );
			for ( VentMechNum = 1; VentMechNum <= NumVentMechControllers; ++VentMechNum ) {
				{ IOFlags flags; flags.ADVANCE( "NO" ); gio::write( OutputFileInits, fmtA, flags ) << " Controller:MechanicalVentilation," + VentilationMechanical( VentMechNum ).Name + ',' + VentilationMechanical( VentMechNum ).SchName + ','; }
				if ( VentilationMechanical( VentMechNum ).DCVFlag ) {
					{ IOFlags flags; flags.ADVANCE( "NO" ); gio::write( OutputFileInits, fmtA, flags ) << "Yes,"; }
				} else {
					{ IOFlags flags; flags.ADVANCE( "NO" ); gio::write( OutputFileInits, fmtA, flags ) << "No,"; }
				}
				if ( VentilationMechanical( VentMechNum ).SystemOAMethod == SOAM_ZoneSum ) {
					{ IOFlags flags; flags.ADVANCE( "NO" ); gio::write( OutputFileInits, fmtA, flags ) << "ZoneSum,"; }
				} else if ( VentilationMechanical( VentMechNum ).SystemOAMethod == SOAM_VRP ) {
					{ IOFlags flags; flags.ADVANCE( "NO" ); gio::write( OutputFileInits, fmtA, flags ) << "VentilationRateProcedure,"; }
				} else if ( VentilationMechanical( VentMechNum ).SystemOAMethod == SOAM_IAQP ) {
					{ IOFlags flags; flags.ADVANCE( "NO" ); gio::write( OutputFileInits, fmtA, flags ) << "IndoorAirQualityProcedure,"; }
				} else if ( VentilationMechanical( VentMechNum ).SystemOAMethod == SOAM_ProportionalControlSchOcc ) {
					{ IOFlags flags; flags.ADVANCE( "NO" ); gio::write( OutputFileInits, fmtA, flags ) << "ProportionalControlBasedonOccupancySchedule,"; }
				} else if ( VentilationMechanical( VentMechNum ).SystemOAMethod == SOAM_ProportionalControlDesOcc ) {
					{ IOFlags flags; flags.ADVANCE( "NO" ); gio::write( OutputFileInits, fmtA, flags ) << "ProportionalControlBasedOnDesignOccupancy,"; }
				} else if ( VentilationMechanical( VentMechNum ).SystemOAMethod == SOAM_IAQPGC ) {
					{ IOFlags flags; flags.ADVANCE( "NO" ); gio::write( OutputFileInits, fmtA, flags ) << "IndoorAirQualityGenericContaminant,"; }
				} else if ( VentilationMechanical( VentMechNum ).SystemOAMethod == SOAM_IAQPCOM ) {
					{ IOFlags flags; flags.ADVANCE( "NO" ); gio::write( OutputFileInits, fmtA, flags ) << "IndoorAirQualityProcedureCombined,"; }
				} else {
					{ IOFlags flags; flags.ADVANCE( "NO" ); gio::write( OutputFileInits, fmtA, flags ) << "Invalid/Unknown,"; }
				}
				{ IOFlags flags; flags.ADVANCE( "NO" ); gio::write( OutputFileInits, fmtA, flags ) << RoundSigDigits( VentilationMechanical( VentMechNum ).ZoneMaxOAFraction, 2 ) + ','; }
				{ IOFlags flags; flags.ADVANCE( "NO" ); gio::write( OutputFileInits, fmtA, flags ) << RoundSigDigits( VentilationMechanical( VentMechNum ).NumofVentMechZones ) + ','; }
				for ( jZone = 1; jZone <= VentilationMechanical( VentMechNum ).NumofVentMechZones; ++jZone ) {
					if ( jZone < VentilationMechanical( VentMechNum ).NumofVentMechZones ) {
						{ IOFlags flags; flags.ADVANCE( "NO" ); gio::write( OutputFileInits, fmtA, flags ) << Zone( VentilationMechanical( VentMechNum ).VentMechZone( jZone ) ).Name + ',' + VentilationMechanical( VentMechNum ).ZoneDesignSpecOAObjName( jZone ) + ',' + VentilationMechanical( VentMechNum ).ZoneDesignSpecADObjName( jZone ) + ','; }
					} else {
						gio::write( OutputFileInits, fmtA ) << Zone( VentilationMechanical( VentMechNum ).VentMechZone( jZone ) ).Name + ',' + VentilationMechanical( VentMechNum ).ZoneDesignSpecOAObjName( jZone ) + ',' + VentilationMechanical( VentMechNum ).ZoneDesignSpecADObjName( jZone );
					}
				}
			}

		} // Number of Mechanical Ventilation Objects > 0

		AlphArray.deallocate();
		NumArray.deallocate();
		lNumericBlanks.deallocate();
		lAlphaBlanks.deallocate();
		cAlphaFields.deallocate();
		cNumericFields.deallocate();

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found when getting " + CurrentModuleObject + " inputs." );
		}

	}

	void
	AllocateOAControllers()
	{

		// PURPOSE OF THIS SUBROUTINE:
		// Allocate the OA controller arrays which are shared by Controller:OutdoorAir and ZoneHVAC:EnergyRecoveryVentilator:Controller

		using namespace InputProcessor;

		if ( AllocateOAControllersFlag ) {
			NumOAControllers = GetNumObjectsFound( CurrentModuleObjects( CMO_OAController ) );
			NumERVControllers = GetNumObjectsFound( CurrentModuleObjects( CMO_ERVController ) );
			NumOAControllers += NumERVControllers;
			OAController.allocate( NumOAControllers );
			AllocateOAControllersFlag = false;
		}

	}

	void
	GetOAMixerInputs()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   Oct 1998
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE
		// Input the OAMixer data and store it in the OAMixer array.

		// METHODOLOGY EMPLOYED:
		// Use the Get routines from the InputProcessor module.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace InputProcessor;
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::TestCompSet;

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetOAMixerInputs: " ); // include trailing blank space

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int NumNums; // Number of REAL(r64) numbers returned by GetObjectItem
		int NumAlphas; // Number of alphanumerics returned by GetObjectItem
		int NumArg; // Number of arguments from GetObjectDefMaxArgs call
		int OutAirNum;
		int IOStat;
		Array1D< Real64 > NumArray; // array that holds numeric input values
		Array1D_string AlphArray; // array that holds alpha input values
		std::string CurrentModuleObject; // Object type for getting and messages
		Array1D_string cAlphaFields; // Alpha field names
		Array1D_string cNumericFields; // Numeric field names
		Array1D_bool lAlphaBlanks; // Logical array, alpha field input BLANK = .TRUE.
		Array1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.
		static bool ErrorsFound( false );
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name

		if ( ! GetOAMixerInputFlag ) return;

		GetObjectDefMaxArgs( CurrentModuleObjects( CMO_OAMixer ), NumArg, NumAlphas, NumNums );

		AlphArray.allocate( NumAlphas );
		NumArray.dimension( NumNums, 0.0 );
		lNumericBlanks.dimension( NumNums, true );
		lAlphaBlanks.dimension( NumAlphas, true );
		cAlphaFields.allocate( NumAlphas );
		cNumericFields.allocate( NumNums );

		CurrentModuleObject = CurrentModuleObjects( CMO_OAMixer );

		NumOAMixers = GetNumObjectsFound( CurrentModuleObject );

		if ( NumOAMixers > 0 ) {

			OAMixer.allocate( NumOAMixers );

			for ( OutAirNum = 1; OutAirNum <= NumOAMixers; ++OutAirNum ) {
				GetObjectItem( CurrentModuleObject, OutAirNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );
				IsNotOK = false;
				IsBlank = false;
				VerifyName( AlphArray( 1 ), OAMixer, OutAirNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
				}
				OAMixer( OutAirNum ).Name = AlphArray( 1 );
				OAMixer( OutAirNum ).MixNode = GetOnlySingleNode( AlphArray( 2 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
				//  Set connection type to 'Inlet', because this is not necessarily directly from
				//  outside air.  Outside Air Inlet Node List will set the connection to outside air
				OAMixer( OutAirNum ).InletNode = GetOnlySingleNode( AlphArray( 3 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
				OAMixer( OutAirNum ).RelNode = GetOnlySingleNode( AlphArray( 4 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_ReliefAir, 1, ObjectIsNotParent );
				OAMixer( OutAirNum ).RetNode = GetOnlySingleNode( AlphArray( 5 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
				// Check for dupes in the four nodes.
				if ( OAMixer( OutAirNum ).MixNode == OAMixer( OutAirNum ).InletNode ) {
					ShowSevereError( CurrentModuleObject + " = " + OAMixer( OutAirNum ).Name + ' ' + cAlphaFields( 3 ) + " = " + NodeID( OAMixer( OutAirNum ).InletNode ) + " duplicates the " + cAlphaFields( 2 ) + '.' );
					ErrorsFound = true;
				} else if ( OAMixer( OutAirNum ).MixNode == OAMixer( OutAirNum ).RelNode ) {
					ShowSevereError( CurrentModuleObject + " = " + OAMixer( OutAirNum ).Name + ' ' + cAlphaFields( 4 ) + " = " + NodeID( OAMixer( OutAirNum ).RelNode ) + " duplicates the " + cAlphaFields( 2 ) + '.' );
					ErrorsFound = true;
				} else if ( OAMixer( OutAirNum ).MixNode == OAMixer( OutAirNum ).RetNode ) {
					ShowSevereError( CurrentModuleObject + " = " + OAMixer( OutAirNum ).Name + ' ' + cAlphaFields( 5 ) + " = " + NodeID( OAMixer( OutAirNum ).RetNode ) + " duplicates the " + cAlphaFields( 2 ) + '.' );
					ErrorsFound = true;
				}

				if ( OAMixer( OutAirNum ).InletNode == OAMixer( OutAirNum ).RelNode ) {
					ShowSevereError( CurrentModuleObject + " = " + OAMixer( OutAirNum ).Name + ' ' + cAlphaFields( 4 ) + " = " + NodeID( OAMixer( OutAirNum ).RelNode ) + " duplicates the " + cAlphaFields( 3 ) + '.' );
					ErrorsFound = true;
				} else if ( OAMixer( OutAirNum ).InletNode == OAMixer( OutAirNum ).RetNode ) {
					ShowSevereError( CurrentModuleObject + " = " + OAMixer( OutAirNum ).Name + ' ' + cAlphaFields( 5 ) + " = " + NodeID( OAMixer( OutAirNum ).RetNode ) + " duplicates the " + cAlphaFields( 3 ) + '.' );
					ErrorsFound = true;
				}

				if ( OAMixer( OutAirNum ).RelNode == OAMixer( OutAirNum ).RetNode ) {
					ShowSevereError( CurrentModuleObject + " = " + OAMixer( OutAirNum ).Name + ' ' + cAlphaFields( 5 ) + " = " + NodeID( OAMixer( OutAirNum ).RetNode ) + " duplicates the " + cAlphaFields( 4 ) + '.' );
					ErrorsFound = true;
				}

				TestCompSet( CurrentModuleObject, OAMixer( OutAirNum ).Name, AlphArray( 3 ), AlphArray( 2 ), "Air Nodes" );

			}

		}

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in getting " + CurrentModuleObject );
		}

		GetOAMixerInputFlag = false;

	}

	void
	ProcessOAControllerInputs(
		std::string const & CurrentModuleObject,
		int const OutAirNum,
		Array1_string const & AlphArray,
		int & NumAlphas,
		Array1< Real64 > const & NumArray,
		int & NumNums,
		Array1_bool const & lNumericBlanks, //Unused
		Array1_bool const & lAlphaBlanks,
		Array1_string const & cAlphaFields,
		Array1_string const & cNumericFields, //Unused
		bool & ErrorsFound // If errors found in input
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   Oct 1998
		//       MODIFIED       Shirey/Raustad FSEC, June 2003, Jan 2004
		//                      Mangesh Basarkar, 06/2011: Getting zone OA specifications from Design Specification Object
		//                      Tianzhen Hong, 3/2012: getting zone air distribution effectiveness and secondary recirculation
		//                       from DesignSpecification:ZoneAirDistribution objects
		//       RE-ENGINEERED  MJW: Split out processing controller:outdoorair input to facilitate unit testing, Feb 2015

		// PURPOSE OF THIS SUBROUTINE
		// Input the OAController data and store it in the OAController array.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace InputProcessor;
		using namespace DataDefineEquip;
		using General::TrimSigDigits;
		using General::RoundSigDigits;
		using NodeInputManager::GetOnlySingleNode;
		using DataZoneEquipment::ZoneEquipConfig;
		using DataZoneEquipment::ZoneEquipList;
		using DataZoneEquipment::NumOfZoneEquipLists;
		using DataHeatBalance::Zone;
		using DataHeatBalance::ZoneList;
		using CurveManager::GetCurveIndex;
		using CurveManager::GetCurveType;
		using namespace OutputReportPredefined;

		using DataAirSystems::PrimaryAirSystem;
		using DataZoneControls::HumidityControlZone;
		using DataZoneControls::NumHumidityControlZones;
		using DataContaminantBalance::Contaminant;
		using OutAirNodeManager::CheckOutAirNodeNumber;

		using SetPointManager::GetMixedAirNumWithCoilFreezingCheck;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetOAControllerInputs: " ); // include trailing blank space

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int OAControllerNum; // Index to Controller:OutdoorAir or CONTROLLER:STAND ALONE ERV objects
		int ControlledZoneNum; // Index to controlled zones
		bool AirNodeFound; // Used to determine if control zone is valid
		bool AirLoopFound; // Used to determine if control zone is served by furnace air loop
		int AirLoopNumber; // Used to determine if control zone is served by furnace air loop
		int BranchNum; // Used to determine if control zone is served by furnace air loop
		int CompNum; // Used to determine if control zone is served by furnace air loop
		int HStatZoneNum; // Used to determine if control zone has a humidistat object
		int OASysNum; // Used to find OA System index for OA Controller
		int OASysIndex; // Index to OA System
		bool OASysFound; // OA Controller found OA System index
		Real64 OAFlowRatio; // Ratio of minimum OA flow rate to maximum OA flow rate

		OAController( OutAirNum ).Name = AlphArray( 1 );
		OAController( OutAirNum ).ControllerType = CurrentModuleObject;
		OAController( OutAirNum ).ControllerType_Num = ControllerOutsideAir;
		OAController( OutAirNum ).MaxOA = NumArray( 2 );
		OAController( OutAirNum ).MinOA = NumArray( 1 );
		OAController( OutAirNum ).MixNode = GetOnlySingleNode( AlphArray( 4 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Sensor, 1, ObjectIsNotParent );
		OAController( OutAirNum ).OANode = GetOnlySingleNode( AlphArray( 5 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Actuator, 1, ObjectIsNotParent );
		if ( ! CheckOutAirNodeNumber( OAController( OutAirNum ).OANode ) ) {
			ShowWarningError( CurrentModuleObject + "=\"" + AlphArray( 1 ) + "\": " + cAlphaFields( 5 ) + "=\"" + AlphArray( 5 ) + "\" is not an OutdoorAir:Node." );
			ShowContinueError( "Confirm that this is the intended source for the outdoor air stream." );
		}
		if ( SameString( AlphArray( 6 ), "NoEconomizer" ) ) {
			OAController( OutAirNum ).Econo = NoEconomizer;
		} else if ( SameString( AlphArray( 6 ), "FixedDryBulb" ) ) {
			OAController( OutAirNum ).Econo = FixedDryBulb;
		} else if ( SameString( AlphArray( 6 ), "FixedEnthalpy" ) ) {
			OAController( OutAirNum ).Econo = FixedEnthalpy;
		} else if ( SameString( AlphArray( 6 ), "FixedDewPointAndDryBulb" ) ) {
			OAController( OutAirNum ).Econo = FixedDewPointAndDryBulb;
		} else if ( SameString( AlphArray( 6 ), "DifferentialDryBulb" ) ) {
			OAController( OutAirNum ).Econo = DifferentialDryBulb;
		} else if ( SameString( AlphArray( 6 ), "DifferentialEnthalpy" ) ) {
			OAController( OutAirNum ).Econo = DifferentialEnthalpy;
		} else if ( SameString( AlphArray( 6 ), "DifferentialDryBulbAndEnthalpy" ) ) {
			OAController( OutAirNum ).Econo = DifferentialDryBulbAndEnthalpy;
		} else if ( SameString( AlphArray( 6 ), "ElectronicEnthalpy" ) ) {
			OAController( OutAirNum ).Econo = ElectronicEnthalpy;
		} else {
			ShowSevereError( CurrentModuleObject + "=\"" + AlphArray( 1 ) + "\" invalid " + cAlphaFields( 6 ) + "=\"" + AlphArray( 6 ) + "\" value." );
			ErrorsFound = true;
		}
		//Bypass choice - Added by Amit for new feature implementation
		if ( SameString( AlphArray( 7 ), "ModulateFlow" ) ) {
			OAController( OutAirNum ).EconBypass = false;
		} else if ( SameString( AlphArray( 7 ), "MinimumFlowWithBypass" ) ) {
			OAController( OutAirNum ).EconBypass = true;
		} else {
			ShowSevereError( CurrentModuleObject + "=\"" + AlphArray( 1 ) + "\" invalid " + cAlphaFields( 7 ) + "=\"" + AlphArray( 7 ) + "\" value." );
			ErrorsFound = true;
		}

		//    IF((OAController(OutAirNum)%Econo > NoEconomizer) .AND. OAController(OutAirNum)%EconBypass) THEN
		//      CALL ShowSevereError(TRIM(CurrentModuleObject)//'="'//TRIM(AlphArray(1))//'" invalid '//  &
		//         TRIM(cAlphaFields(6))//'="'//TRIM(AlphArray(6))//'" and ')
		//      CALL ShowContinueError(TRIM(cAlphaFields(7))//'="'//TRIM(AlphArray(7))//'" incompatible specifications.')
		//      ErrorsFound = .TRUE.
		//    END IF
		if ( SameString( AlphArray( 9 ), "NoLockout" ) ) {
			OAController( OutAirNum ).Lockout = NoLockoutPossible;
		} else if ( SameString( AlphArray( 9 ), "LockoutWithHeating" ) ) {
			OAController( OutAirNum ).Lockout = LockoutWithHeatingPossible;
		} else if ( SameString( AlphArray( 9 ), "LockoutWithCompressor" ) ) {
			OAController( OutAirNum ).Lockout = LockoutWithCompressorPossible;
		} else {
			ShowSevereError( CurrentModuleObject + "=\"" + AlphArray( 1 ) + "\" invalid " + cAlphaFields( 9 ) + "=\"" + AlphArray( 9 ) + "\" value." );
			ErrorsFound = true;
		}
		if ( SameString( AlphArray( 10 ), "FixedMinimum" ) ) {
			OAController( OutAirNum ).FixedMin = true;
		} else {
			OAController( OutAirNum ).FixedMin = false;
		}
		if ( lNumericBlanks( 3 ) ) {
			OAController( OutAirNum ).TempLim = BlankNumeric;
		} else {
			OAController( OutAirNum ).TempLim = NumArray( 3 );
		}

		if ( lNumericBlanks( 4 ) ) {
			OAController( OutAirNum ).EnthLim = BlankNumeric;
		} else {
			OAController( OutAirNum ).EnthLim = NumArray( 4 );
		}
		if ( lNumericBlanks( 5 ) ) {
			OAController( OutAirNum ).DPTempLim = BlankNumeric;
		} else {
			OAController( OutAirNum ).DPTempLim = NumArray( 5 );
		}

		if ( lNumericBlanks( 6 ) ) {
			OAController( OutAirNum ).TempLowLim = BlankNumeric;
		} else {
			OAController( OutAirNum ).TempLowLim = NumArray( 6 );
		}

		if ( ! lAlphaBlanks( 8 ) ) {
			OAController( OutAirNum ).EnthalpyCurvePtr = GetCurveIndex( AlphArray( 8 ) ); // convert curve name to number
			if ( OAController( OutAirNum ).EnthalpyCurvePtr == 0 ) {
				ShowSevereError( CurrentModuleObject + "=\"" + AlphArray( 1 ) + "\" invalid " + cAlphaFields( 8 ) + "=\"" + AlphArray( 8 ) + "\" not found." );
				ErrorsFound = true;
			} else {
				// Verify Curve Object, only legal types are Quadratic and Cubic
				{ auto const SELECT_CASE_var( GetCurveType( OAController( OutAirNum ).EnthalpyCurvePtr ) );

				if ( SELECT_CASE_var == "QUADRATIC" ) {

				} else if ( SELECT_CASE_var == "CUBIC" ) {

				} else {
					ShowSevereError( CurrentModuleObject + "=\"" + AlphArray( 1 ) + "\" invalid " + cAlphaFields( 8 ) + "=\"" + AlphArray( 8 ) + "\"." );
					ShowContinueError( "...must be Quadratic or Cubic curve." );
					ErrorsFound = true;
				}}
			}
		}

		OAController( OutAirNum ).RelNode = GetOnlySingleNode( AlphArray( 2 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Actuator, 1, ObjectIsNotParent );
		OAController( OutAirNum ).RetNode = GetOnlySingleNode( AlphArray( 3 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Sensor, 1, ObjectIsNotParent );
		OAController( OutAirNum ).MinOASch = AlphArray( 11 );
		OAController( OutAirNum ).MinOASchPtr = GetScheduleIndex( AlphArray( 11 ) );
		if ( OAController( OutAirNum ).MinOASchPtr == 0 && ( ! lAlphaBlanks( 11 ) ) ) {
			ShowSevereError( CurrentModuleObject + "=\"" + AlphArray( 1 ) + "\" invalid " + cAlphaFields( 11 ) + "=\"" + AlphArray( 11 ) + "\" not found." );
			ErrorsFound = true;
		}

		// Changed by Amit for new feature implementation
		OAController( OutAirNum ).MinOAflowSch = AlphArray( 12 );
		OAController( OutAirNum ).MinOAflowSchPtr = GetScheduleIndex( AlphArray( 12 ) );
		if ( OAController( OutAirNum ).MinOAflowSchPtr == 0 && ( ! lAlphaBlanks( 12 ) ) ) {
			ShowSevereError( CurrentModuleObject + "=\"" + AlphArray( 1 ) + "\" invalid " + cAlphaFields( 12 ) + "=\"" + AlphArray( 12 ) + "\" not found." );
			ErrorsFound = true;
		}

		OAController( OutAirNum ).MaxOAflowSch = AlphArray( 13 );
		OAController( OutAirNum ).MaxOAflowSchPtr = GetScheduleIndex( AlphArray( 13 ) );
		if ( OAController( OutAirNum ).MaxOAflowSchPtr == 0 && ( ! lAlphaBlanks( 13 ) ) ) {
			ShowSevereError( CurrentModuleObject + "=\"" + AlphArray( 1 ) + "\" invalid " + cAlphaFields( 13 ) + "=\"" + AlphArray( 13 ) + "\" not found." );
			ErrorsFound = true;
		}
		OAController( OutAirNum ).VentilationMechanicalName = AlphArray( 14 );

		//   Check for a time of day economizer control schedule
		OAController( OutAirNum ).EconomizerOASchedPtr = GetScheduleIndex( AlphArray( 15 ) );

		//   High humidity control option can be used with any economizer flag
		if ( SameString( AlphArray( 16 ), "Yes" ) ) {

			OAController( OutAirNum ).HumidistatZoneNum = FindItemInList( AlphArray( 17 ), Zone );

			// Get the node number for the zone with the humidistat
			if ( OAController( OutAirNum ).HumidistatZoneNum > 0 ) {
				AirNodeFound = false;
				AirLoopFound = false;
				OASysFound = false;
				for ( ControlledZoneNum = 1; ControlledZoneNum <= NumOfZones; ++ControlledZoneNum ) {
					if ( ZoneEquipConfig( ControlledZoneNum ).ActualZoneNum != OAController( OutAirNum ).HumidistatZoneNum ) continue;
					//           Find the controlled zone number for the specified humidistat location
					OAController( OutAirNum ).NodeNumofHumidistatZone = ZoneEquipConfig( ControlledZoneNum ).ZoneNode;
					//           Determine which OA System uses this OA Controller
					OASysIndex = 0;
					for ( OASysNum = 1; OASysNum <= NumOASystems; ++OASysNum ) {
						for ( OAControllerNum = 1; OAControllerNum <= OutsideAirSys( OASysNum ).NumControllers; ++OAControllerNum ) {
							if ( ! SameString( OutsideAirSys( OASysNum ).ControllerType( OAControllerNum ), CurrentModuleObject ) || ! SameString( OutsideAirSys( OASysNum ).ControllerName( OAControllerNum ), OAController( OutAirNum ).Name ) ) continue;
							OASysIndex = OASysNum;
							OASysFound = true;
							break;
						}
						if ( OASysFound ) break;
					}
					//           Determine if furnace is on air loop served by the humidistat location specified
					AirLoopNumber = ZoneEquipConfig( ControlledZoneNum ).AirLoopNum;
					if ( AirLoopNumber > 0 && OASysIndex > 0 ) {
						for ( BranchNum = 1; BranchNum <= PrimaryAirSystem( AirLoopNumber ).NumBranches; ++BranchNum ) {
							for ( CompNum = 1; CompNum <= PrimaryAirSystem( AirLoopNumber ).Branch( BranchNum ).TotalComponents; ++CompNum ) {
								if ( ! SameString( PrimaryAirSystem( AirLoopNumber ).Branch( BranchNum ).Comp( CompNum ).Name, OutsideAirSys( OASysIndex ).Name ) || ! SameString( PrimaryAirSystem( AirLoopNumber ).Branch( BranchNum ).Comp( CompNum ).TypeOf, "AirLoopHVAC:OutdoorAirSystem" ) ) continue;
								AirLoopFound = true;
								break;
							}
							if ( AirLoopFound ) break;
						}
						for ( HStatZoneNum = 1; HStatZoneNum <= NumHumidityControlZones; ++HStatZoneNum ) {
							if ( HumidityControlZone( HStatZoneNum ).ActualZoneNum != OAController( OutAirNum ).HumidistatZoneNum ) continue;
							AirNodeFound = true;
							break;
						}
					} else {
						if ( AirLoopNumber == 0 ) {
							ShowSevereError( "Did not find a Primary Air Loop for " + OAController( OutAirNum ).ControllerType + " = \"" + OAController( OutAirNum ).Name + "\"" );
							ShowContinueError( "Specified " + cAlphaFields( 17 ) + " = " + AlphArray( 17 ) );
							ErrorsFound = true;
						}
						if ( OASysIndex == 0 ) {
							ShowSevereError( "Did not find an AirLoopHVAC:OutdoorAirSystem for " + OAController( OutAirNum ).ControllerType + " = \"" + OAController( OutAirNum ).Name + "\"" );
							ErrorsFound = true;
						}
					}
					break;
				}
				if ( ! AirNodeFound ) {
					ShowSevereError( "Did not find Air Node (Zone with Humidistat), " + OAController( OutAirNum ).ControllerType + " = \"" + OAController( OutAirNum ).Name + "\"" );
					ShowContinueError( "Specified " + cAlphaFields( 17 ) + " = " + AlphArray( 17 ) );
					ShowContinueError( "Both a ZoneHVAC:EquipmentConnections object and a ZoneControl:Humidistat object must be specified for this zone." );
					ErrorsFound = true;
				}
				if ( ! AirLoopFound ) {
					ShowSevereError( "Did not find correct Primary Air Loop for " + OAController( OutAirNum ).ControllerType + " = \"" + OAController( OutAirNum ).Name + "\"" );
					ShowContinueError( cAlphaFields( 17 ) + " = " + AlphArray( 17 ) + " is not served by this Primary Air Loop equipment." );
					ErrorsFound = true;
				}
			} else {
				ShowSevereError( "Did not find Air Node (Zone with Humidistat), " + OAController( OutAirNum ).ControllerType + " = \"" + OAController( OutAirNum ).Name + "\"" );
				ShowContinueError( "Specified " + cAlphaFields( 17 ) + " = " + AlphArray( 17 ) );
				ShowContinueError( "Both a ZoneHVAC:EquipmentConnections object and a ZoneControl:Humidistat object must be specified for this zone." );
				ErrorsFound = true;
			}

			OAController( OutAirNum ).HighRHOAFlowRatio = NumArray( 7 );
			if ( OAController( OutAirNum ).HighRHOAFlowRatio <= 0.0 && NumNums > 6 ) {
				ShowWarningError( CurrentModuleObject + " \"" + OAController( OutAirNum ).Name + "\"" );
				ShowContinueError( ' ' + cNumericFields( 7 ) + " must be greater than 0." );
				ShowContinueError( ' ' + cNumericFields( 7 ) + " is reset to 1 and the simulation continues." );
				OAController( OutAirNum ).HighRHOAFlowRatio = 1.0;
			}

			if ( SameString( AlphArray( 16 ), "Yes" ) && OAController( OutAirNum ).FixedMin ) {
				if ( OAController( OutAirNum ).MaxOA > 0.0 && OAController( OutAirNum ).MinOA != AutoSize ) {
					OAFlowRatio = OAController( OutAirNum ).MinOA / OAController( OutAirNum ).MaxOA;
					if ( OAController( OutAirNum ).HighRHOAFlowRatio < OAFlowRatio ) {
						ShowWarningError( CurrentModuleObject + " \"" + OAController( OutAirNum ).Name + "\"" );
						ShowContinueError( "... A fixed minimum outside air flow rate and high humidity control have been specified." );
						ShowContinueError( "... The " + cNumericFields( 7 ) + " is less than the ratio of the outside air controllers minimum to maximum outside air flow rate." );
						ShowContinueError( "... Controller " + cNumericFields( 1 ) + " = " + TrimSigDigits( OAController( OutAirNum ).MinOA, 4 ) + " m3/s." );
						ShowContinueError( "... Controller " + cNumericFields( 2 ) + " = " + TrimSigDigits( OAController( OutAirNum ).MaxOA, 4 ) + " m3/s." );
						ShowContinueError( "... Controller minimum to maximum flow ratio = " + TrimSigDigits( OAFlowRatio, 4 ) + '.' );
						ShowContinueError( "... " + cNumericFields( 7 ) + " = " + TrimSigDigits( OAController( OutAirNum ).HighRHOAFlowRatio, 4 ) + '.' );
					}
				}
			}

			if ( NumAlphas >= 18 ) {
				if ( SameString( AlphArray( 18 ), "Yes" ) ) {
					OAController( OutAirNum ).ModifyDuringHighOAMoisture = false;
				} else if ( SameString( AlphArray( 18 ), "No" ) ) {
					OAController( OutAirNum ).ModifyDuringHighOAMoisture = true;
				} else {
					ShowSevereError( CurrentModuleObject + " \"" + OAController( OutAirNum ).Name + "\", invalid field value" );
					ShowContinueError( "..." + cAlphaFields( 18 ) + "=\"" + AlphArray( 18 ) + "\" - valid values are \"Yes\" or \"No\"." );
					ErrorsFound = true;
				}
			} else {
				if( OAController( OutAirNum ).Econo == NoEconomizer ) {
					OAController( OutAirNum ).ModifyDuringHighOAMoisture = true;
				} else {
					OAController( OutAirNum ).ModifyDuringHighOAMoisture = false;
					ShowWarningError( CurrentModuleObject + " \"" + OAController( OutAirNum ).Name + "\", missing field value" );
					ShowContinueError( "..." + cAlphaFields( 18 ) + " will default to Yes when " + cAlphaFields( 16 ) + "= \"Yes\"" );
				}
			}

		} else if ( SameString( AlphArray( 16 ), "No" ) || lAlphaBlanks( 16 ) ) {
			if ( NumAlphas >= 18 ) {
				if ( ! SameString( AlphArray( 18 ), "Yes" ) && ! SameString( AlphArray( 18 ), "No" ) ) {
					ShowSevereError( CurrentModuleObject + " \"" + OAController( OutAirNum ).Name + "\", invalid field value" );
					ShowContinueError( "..." + cAlphaFields( 18 ) + "=\"" + AlphArray( 18 ) + "\" - valid values are \"Yes\" or \"No\"." );
					ErrorsFound = true;
				}
			}
		} else { // Invalid field 16
			ShowSevereError( CurrentModuleObject + " \"" + OAController( OutAirNum ).Name + "\", invalid field value" );
			ShowContinueError( "..." + cAlphaFields( 16 ) + "=\"" + AlphArray( 16 ) + "\" - valid values are \"Yes\" or \"No\"." );
			ErrorsFound = true;
			if ( NumAlphas >= 18 ) {
				if ( ! SameString( AlphArray( 18 ), "Yes" ) && ! SameString( AlphArray( 18 ), "No" ) ) {
					ShowSevereError( CurrentModuleObject + " \"" + OAController( OutAirNum ).Name + "\", invalid field value" );
					ShowContinueError( "..." + cAlphaFields( 18 ) + "=\"" + AlphArray( 18 ) + "\" - valid values are \"Yes\" or \"No\"." );
					ErrorsFound = true;
				}
			}
		}

		if ( NumAlphas > 18 ) {
			if ( ! lAlphaBlanks( 19 ) ) {
				if ( SameString( AlphArray( 19 ), "BypassWhenWithinEconomizerLimits" ) ) {
					OAController( OutAirNum ).HeatRecoveryBypassControlType = BypassWhenWithinEconomizerLimits;
				} else if ( SameString( AlphArray( 19 ), "BypassWhenOAFlowGreaterThanMinimum" ) ) {
					OAController( OutAirNum ).HeatRecoveryBypassControlType = BypassWhenOAFlowGreaterThanMinimum;
				} else {
					ShowWarningError( CurrentModuleObject + "=\"" + AlphArray( 1 ) + "\" invalid " + cAlphaFields( 19 ) + "=\"" + AlphArray( 19 ) + "\"." );
					ShowContinueError( "...assuming \"BypassWhenWithinEconomizerLimits\" and the simulation continues." );
					OAController( OutAirNum ).HeatRecoveryBypassControlType = BypassWhenWithinEconomizerLimits;
				}
			}
		}

		if ( SameString( AlphArray( 16 ), "Yes" ) && OAController( OutAirNum ).Econo == NoEconomizer ) {
			ShowWarningError( OAController( OutAirNum ).ControllerType + " \"" + OAController( OutAirNum ).Name + "\"" );
			ShowContinueError( "...Economizer operation must be enabled when " + cAlphaFields( 16 ) + " is set to YES." );
			ShowContinueError( "...The high humidity control option will be disabled and the simulation continues." );
		}

		OAController( OutAirNum ).MixedAirSPMNum = GetMixedAirNumWithCoilFreezingCheck( OAController( OutAirNum ).MixNode );

	}

	// End of Get Input subroutines for the Module
	//******************************************************************************

	// Beginning Initialization Section of the Module
	//******************************************************************************

	void
	InitOutsideAirSys(
		int const EP_UNUSED( OASysNum ), // unused1208
		bool const FirstHVACIteration
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   Oct 1998
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE
		// Initialize the OutsideAirSys data structure

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// Using/Aliasing
		using namespace DataLoopNode;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		if ( BeginEnvrnFlag && FirstHVACIteration ) {
		}

		if ( BeginDayFlag ) {
		}

		// Each time step
		if ( FirstHVACIteration ) {
		}

		// Each iteration

	}

	void
	InitOAController(
		int const OAControllerNum,
		bool const FirstHVACIteration,
		int const AirLoopNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   Oct 1998
		//       MODIFIED       Shirey/Raustad FSEC, June/Aug 2003, Feb 2004
		//                      Tianzhen Hong, Feb 2009 for DCV
		//                      Tianzhen Hong, Aug 2013 for economizer faults
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE
		// Initialize the OAController data structure with input node data

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// Using/Aliasing
		using namespace DataLoopNode;
		using DataHeatBalance::ZoneIntGain;
		using DataHeatBalance::Zone;
		using DataHeatBalance::ZoneList;
		using DataHeatBalance::TotPeople;
		using DataHeatBalance::People;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using InputProcessor::FindItemInList;
		using InputProcessor::FindItem;
		using InputProcessor::SameString;
		using General::RoundSigDigits;
		using namespace OutputReportPredefined;
		using EMSManager::iTemperatureSetPoint;
		using EMSManager::CheckIfNodeSetPointManagedByEMS;
		using DataAirSystems::PrimaryAirSystem;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		/////////// hoisted into namespace
		//static bool MyOneTimeFlag( true ); // One-time initialization flag
		//static bool MySetPointCheckFlag( true ); // One-time initialization flag
		//static bool SetUpAirLoopHVACVariables( true ); // One-time initialization flag
		//////////////////////////////////
		static Array1D_bool OAControllerMyOneTimeFlag; // One-time initialization flag
		static Array1D_bool OAControllerMyEnvrnFlag; // One-time initialization flag
		static Array1D_bool OAControllerMySizeFlag; // One-time initialization flag
		static Array1D_bool MechVentCheckFlag; // One-time initialization flag
		bool FoundZone; // Logical determines if ZONE object is accounted for in VENTILATION:MECHANICAL object
		bool FoundAreaZone; // Logical determines if ZONE object is accounted for in VENTILATION:MECHANICAL object
		bool FoundPeopleZone; // Logical determines if ZONE object is accounted for in VENTILATION:MECHANICAL object
		bool OASysFound; // Logical determines if OA system found
		bool AirLoopFound; // Logical determines if primary air loop found
		bool ErrorsFound; // Errors found getting input
		Real64 RhoAirStdInit; // Standard air density
		Real64 TotalPeopleOAFlow; // Total outside air required for PEOPLE objects served by this OA controller
		int MixedAirNode; // Controller:OutdoorAir mixed air node
		int ZoneNum; // DO loop index (zone number)
		int ZoneIndex; // Index to zone in mechanical ventilation zone list
		int AirLoopZoneInfoZoneNum; // Index to AirLoopZoneInfo structure
		int NumZone; // Zone number in AirLoopZoneInfo structure
		int PeopleNum; // Index to PEOPLE objects
		int NumMechVentZone; // Index to number of zones in VentilationMechanical structure
		int TempMechVentArrayCounter; // Temporary array counter
		int thisOASys; // Temporary array counter
		int thisNumForMixer; // Temporary array counter
		int thisMixerIndex; // Temporary array counter
		int OASysNum; // Temporary array counter
		int found; // Temporary index to equipment
		int OANode; // OA node index
		int VentMechObjectNum; // Temporary variable
		int OAControllerLoop; // Index to OA controller in an OA system
		int OAControllerLoop2; // Index to OA controller in an OA system
		int thisAirLoop; // Temporary array counter
		int BranchNum; // Temporary array counter
		int CompNum; // Temporary array counter
		std::string equipName; // Temporary equipment name
		std::string airloopName; // Temporary equipment name
		std::string zoneName;
		int jZone;

		Real64 rSchVal;
		Real64 rOffset;
		int i;
		int iEco;

		ErrorsFound = false;
		OANode = 0;

		auto & thisOAController( OAController( OAControllerNum ) );

		if (InitOAControllerOneTimeFlag) {
			OAControllerMyOneTimeFlag.dimension(NumOAControllers, true);
			OAControllerMyEnvrnFlag.dimension( NumOAControllers, true );
			OAControllerMySizeFlag.dimension( NumOAControllers, true );
			MechVentCheckFlag.dimension( NumOAControllers, true );
			InitOAControllerSetPointCheckFlag.dimension ( NumOAControllers, true );
			InitOAControllerOneTimeFlag = false;
		}
		if ( OAControllerMyOneTimeFlag( OAControllerNum ) ) {
			// Determine Inlet node index for OAController, not a user input for controller, but is obtained from OutsideAirSys and OAMixer
			{ auto const SELECT_CASE_var( thisOAController.ControllerType_Num );

			if ( SELECT_CASE_var == ControllerOutsideAir ) {
				thisOASys = 0;
				for ( OASysNum = 1; OASysNum <= NumOASystems; ++OASysNum ) {
					// find which OAsys has this controller
					found = FindItemInList( thisOAController.Name, OutsideAirSys( OASysNum ).ControllerName, isize( OutsideAirSys( OASysNum ).ControllerName ) );
					if ( found != 0 ) {
						thisOASys = OASysNum;
						break; // we found it
					}
				}
				if ( thisOASys == 0 ) {
					ShowSevereError( "InitOAController: Did not find OAController=\"" + thisOAController.Name + "\"." );
					ShowContinueError( "in list of valid OA Controllers." );
					ErrorsFound = true;
				}
				thisNumForMixer = FindItem( CurrentModuleObjects( CMO_OAMixer ), OutsideAirSys( thisOASys ).ComponentType, isize( OutsideAirSys( thisOASys ).ComponentType ) );
				if ( thisNumForMixer != 0 ) {
					equipName = OutsideAirSys( thisOASys ).ComponentName( thisNumForMixer );
					thisMixerIndex = FindItemInList( equipName, OAMixer );
					if ( thisMixerIndex != 0 ) {
						thisOAController.InletNode = OAMixer( thisMixerIndex ).InletNode;
					} else {
						ShowSevereError( "InitOAController: Did not find OAMixer=\"" + equipName + "\"." );
						ShowContinueError( "in list of valid OA Mixers." );
						ErrorsFound = true;
					}
				} else {
					ShowSevereError( "InitOAController: Did not find OutdoorAir:Mixer Component=\"OutdoorAir:Mixer\"." );
					ShowContinueError( "in list of valid OA Components." );
					ErrorsFound = true;
				}

				if ( thisOAController.InletNode == 0 ) { //throw an error
					ShowSevereError( "InitOAController: Failed to find proper inlet node for OutdoorAir:Mixer and Controller = " + thisOAController.Name );
					ErrorsFound = true;
				}

			} else if ( SELECT_CASE_var == ControllerStandAloneERV ) {
				// set the inlet node to also equal the OA node because this is a special controller for economizing stand alone ERV
				// with the assumption that equipment is bypassed....

				thisOAController.InletNode = thisOAController.OANode;

			} else {
				ShowSevereError( "InitOAController: Failed to find ControllerType: " + thisOAController.ControllerType );
				ErrorsFound = true;

			}}

			OAControllerMyOneTimeFlag( OAControllerNum ) = false;

		}

		if ( ! SysSizingCalc && InitOAControllerSetPointCheckFlag( OAControllerNum ) && DoSetPointTest && ! FirstHVACIteration ) {
			MixedAirNode = thisOAController.MixNode;
			if ( MixedAirNode > 0 ) {
				//      IF (OAController(OAControllerNum)%Econo == 1 .AND. .NOT. AirLoopControlInfo(AirLoopNum)%CyclingFan) THEN
				if ( thisOAController.Econo > NoEconomizer && AirLoopControlInfo( AirLoopNum ).AnyContFan ) {
					if ( Node( MixedAirNode ).TempSetPoint == SensedNodeFlagValue ) {
						if ( ! AnyEnergyManagementSystemInModel ) {
							ShowSevereError( "MixedAir: Missing temperature setpoint for economizer controller " + thisOAController.Name );
							ShowSevereError( "Node Referenced (by Controller)=" + NodeID( MixedAirNode ) );
							ShowContinueError( "  use a Setpoint Manager with Control Variable = \"Temperature\" to establish a setpoint at the mixed air node." );
							SetPointErrorFlag = true;
						} else {
							// add call to check node in EMS
							CheckIfNodeSetPointManagedByEMS( MixedAirNode, iTemperatureSetPoint, SetPointErrorFlag );
							if ( SetPointErrorFlag ) {
								ShowSevereError( "MixedAir: Missing temperature setpoint for economizer controller " + thisOAController.Name );
								ShowSevereError( "Node Referenced (by Controller)=" + NodeID( MixedAirNode ) );
								ShowContinueError( "  use a Setpoint Manager with Control Variable = \"Temperature\" to establish a setpoint at the mixed air node." );
								ShowContinueError( "Or add EMS Actuator to provide temperature setpoint at this node" );
							}
						}
					}
				}
			}

			InitOAControllerSetPointCheckFlag( OAControllerNum ) = false;
		}

		if ( ! SysSizingCalc && OAControllerMySizeFlag( OAControllerNum ) ) {
			thisOAController.SizeOAController();
			if ( AirLoopNum > 0 ) {
				AirLoopControlInfo( AirLoopNum ).OACtrlNum = OAControllerNum;
				AirLoopControlInfo( AirLoopNum ).OACtrlName = thisOAController.Name;
				if ( thisOAController.Lockout == LockoutWithHeatingPossible ) {
					AirLoopControlInfo( AirLoopNum ).CanLockoutEconoWithHeating = true;
					AirLoopControlInfo( AirLoopNum ).CanLockoutEconoWithCompressor = false;
					AirLoopControlInfo( AirLoopNum ).CanNotLockoutEcono = false;
				} else if ( thisOAController.Lockout == LockoutWithCompressorPossible ) {
					AirLoopControlInfo( AirLoopNum ).CanLockoutEconoWithHeating = false;
					AirLoopControlInfo( AirLoopNum ).CanLockoutEconoWithCompressor = true;
					AirLoopControlInfo( AirLoopNum ).CanNotLockoutEcono = false;
				} else {
					AirLoopControlInfo( AirLoopNum ).CanLockoutEconoWithHeating = false;
					AirLoopControlInfo( AirLoopNum ).CanLockoutEconoWithCompressor = false;
					AirLoopControlInfo( AirLoopNum ).CanNotLockoutEcono = true;
				}
			}
			if ( ( thisOAController.MaxOA - thisOAController.MinOA ) < -SmallAirVolFlow ) {
				ShowSevereError( "For Controller:OutdoorAir: " + thisOAController.Name );
				ShowContinueError( "  maximum outdoor air flow rate (" + RoundSigDigits( thisOAController.MaxOA, 4 ) + ") < minimum outdoor air flow rate (" + RoundSigDigits( thisOAController.MinOA, 4 ) + ')' );
				ShowContinueError( "  To set the minimum outside air flow rate use the \"Design (minimum) outdoor air flow rate\" field in the Sizing:System object" );
				ErrorsFound = true;
			}

			if ( AirLoopNum > 0 ) {
				// Fix #3001 (CR 8225) moved here as part of #5119
				Real64 DesSupplyVolFlowRate = AirLoopFlow( AirLoopNum ).DesSupply / StdRhoAir;
				if ( ( thisOAController.MinOA - DesSupplyVolFlowRate ) > 0.0001 ) {
					ShowWarningError( "InitOAController: Minimum Outdoor Air Flow Rate for Controller:OutdoorAir=" + thisOAController.Name + " is greater than Design Supply Air Flow Rate for AirLoopHVAC=" + PrimaryAirSystem( AirLoopNum ).Name + "." );
					ShowContinueError( "...Minimum Outdoor Air Flow Rate=" + RoundSigDigits( thisOAController.MinOA, 6 ) + " will be reset to loop Design Supply Air Flow Rate=" + RoundSigDigits( DesSupplyVolFlowRate, 6 ) );
					thisOAController.MinOA = DesSupplyVolFlowRate;
				} else if ( ( thisOAController.MinOA - DesSupplyVolFlowRate ) > 0.0 ) {
					// If difference is tiny, reset silently
					thisOAController.MinOA = DesSupplyVolFlowRate;
				}
				if ( ( thisOAController.MaxOA - DesSupplyVolFlowRate ) > 0.0001 ) {
					ShowWarningError( "InitOAController: Maximum Outdoor Air Flow Rate for Controller:OutdoorAir=" + thisOAController.Name + " is greater than Design Supply Air Flow Rate for AirLoopHVAC=" + PrimaryAirSystem( AirLoopNum ).Name + "." );
					ShowContinueError( "...Maximum Outdoor Air Flow Rate=" + RoundSigDigits( thisOAController.MaxOA, 6 ) + " will be reset to loop Design Supply Air Flow Rate=" + RoundSigDigits( DesSupplyVolFlowRate, 6 ) );
					thisOAController.MaxOA = DesSupplyVolFlowRate;
				}
				else if ( ( thisOAController.MaxOA - DesSupplyVolFlowRate ) > 0.0 ) {
					// If difference is tiny, reset silently
					thisOAController.MaxOA = DesSupplyVolFlowRate;
				}
			}

			OAControllerMySizeFlag( OAControllerNum ) = false;
		}

		if ( BeginEnvrnFlag && OAControllerMyEnvrnFlag( OAControllerNum ) ) {
			OANode = thisOAController.OANode;
			RhoAirStdInit = StdRhoAir;
			thisOAController.MinOAMassFlowRate = thisOAController.MinOA * RhoAirStdInit;
			thisOAController.MaxOAMassFlowRate = thisOAController.MaxOA * RhoAirStdInit;
			OAControllerMyEnvrnFlag( OAControllerNum ) = false;
			Node( OANode ).MassFlowRateMax = thisOAController.MaxOAMassFlowRate;

			//predefined reporting
			if ( thisOAController.Econo > NoEconomizer ) {
				equipName = thisOAController.Name;
				// 90.1 descriptor for economizer controls
				// Changed by Amit for New Feature implementation
				if ( thisOAController.Econo == DifferentialEnthalpy ) {
					PreDefTableEntry( pdchEcoKind, equipName, "DifferentialEnthalpy" );
				} else if ( thisOAController.Econo == DifferentialDryBulb ) {
					PreDefTableEntry( pdchEcoKind, equipName, "DifferentialDryBulb" );
				} else if ( thisOAController.Econo == FixedEnthalpy ) {
					PreDefTableEntry( pdchEcoKind, equipName, "FixedEnthalpy" );
				} else if ( thisOAController.Econo == FixedDryBulb ) {
					PreDefTableEntry( pdchEcoKind, equipName, "FixedDryBulb" );
				} else {
					PreDefTableEntry( pdchEcoKind, equipName, "Other" );
				}

				PreDefTableEntry( pdchEcoMinOA, equipName, thisOAController.MinOA );
				PreDefTableEntry( pdchEcoMaxOA, equipName, thisOAController.MaxOA );
				//EnergyPlus input echos for economizer controls
				// Chnged by Amit for new feature implementation
				if ( thisOAController.Econo == DifferentialDryBulb ) {
					PreDefTableEntry( pdchEcoRetTemp, equipName, "Yes" );
				} else {
					PreDefTableEntry( pdchEcoRetTemp, equipName, "No" );
				}
				if ( thisOAController.Econo == DifferentialEnthalpy ) {
					PreDefTableEntry( pdchEcoRetTemp, equipName, "Yes" );
				} else {
					PreDefTableEntry( pdchEcoRetTemp, equipName, "No" );
				}
				if ( thisOAController.Econo == FixedDryBulb ) {
					PreDefTableEntry( pdchEcoRetTemp, equipName, thisOAController.TempLim );
				} else {
					PreDefTableEntry( pdchEcoRetTemp, equipName, "-" );
				}
				if ( thisOAController.Econo == FixedEnthalpy ) {
					PreDefTableEntry( pdchEcoRetTemp, equipName, thisOAController.EnthLim );
				} else {
					PreDefTableEntry( pdchEcoRetTemp, equipName, "-" );
				}
			}
		}

		if ( ! BeginEnvrnFlag ) {
			OAControllerMyEnvrnFlag( OAControllerNum ) = true;
		}

		VentMechObjectNum = thisOAController.VentMechObjectNum;
		if ( MechVentCheckFlag( OAControllerNum ) ) {
			// Make these checks only once at the beginning of the simulation

			// Make sure all air loop zones and air loop zones with people objects are covered by mechanical ventilation
			// Issue a warning only if the zone is not accounted for in the associated mechanical ventilation object
			if ( VentMechObjectNum > 0 ) {
				auto & vent_mech( VentilationMechanical( VentMechObjectNum ) );

				// Make sure all zones with mechanical ventilation are on the correct air loop
				TempMechVentArrayCounter = 0;
				for ( NumMechVentZone = 1; NumMechVentZone <= vent_mech.NumofVentMechZones; ++NumMechVentZone ) {
					ZoneNum = vent_mech.VentMechZone( NumMechVentZone );
					auto const & zone( Zone( ZoneNum ) );
					FoundZone = false;

					for ( AirLoopZoneInfoZoneNum = 1; AirLoopZoneInfoZoneNum <= AirLoopZoneInfo( AirLoopNum ).NumZones; ++AirLoopZoneInfoZoneNum ) {
						NumZone = AirLoopZoneInfo( AirLoopNum ).ActualZoneNumber( AirLoopZoneInfoZoneNum );
						if ( ZoneNum == NumZone ) {
							FoundZone = true;
							++TempMechVentArrayCounter;
							if ( TempMechVentArrayCounter < NumMechVentZone ) { // Copy to lower index
								vent_mech.VentMechZone( TempMechVentArrayCounter ) = vent_mech.VentMechZone( NumMechVentZone );
								vent_mech.ZoneOAAreaRate( TempMechVentArrayCounter ) = vent_mech.ZoneOAAreaRate( NumMechVentZone );
								vent_mech.ZoneOAPeopleRate( TempMechVentArrayCounter ) = vent_mech.ZoneOAPeopleRate( NumMechVentZone );
								vent_mech.ZoneOAFlowRate( TempMechVentArrayCounter ) = vent_mech.ZoneOAFlowRate( NumMechVentZone );
								vent_mech.ZoneOAACHRate( TempMechVentArrayCounter ) = vent_mech.ZoneOAACHRate( NumMechVentZone );
								vent_mech.ZoneDesignSpecOAObjIndex( TempMechVentArrayCounter ) = vent_mech.ZoneDesignSpecOAObjIndex( NumMechVentZone );
								vent_mech.ZoneDesignSpecOAObjName( TempMechVentArrayCounter ) = vent_mech.ZoneDesignSpecOAObjName( NumMechVentZone );

								// new DCV
								vent_mech.ZoneADEffCooling( TempMechVentArrayCounter ) = vent_mech.ZoneADEffCooling( NumMechVentZone );
								vent_mech.ZoneADEffHeating( TempMechVentArrayCounter ) = vent_mech.ZoneADEffHeating( NumMechVentZone );
								vent_mech.ZoneADEffSchPtr( TempMechVentArrayCounter ) = vent_mech.ZoneADEffSchPtr( NumMechVentZone );
								vent_mech.ZoneADEffSchName( TempMechVentArrayCounter ) = vent_mech.ZoneADEffSchName( NumMechVentZone );
							}

							// Sum outside air per unit floor area for each mechanical ventilation object only once per simulation
							vent_mech.TotAreaOAFlow += zone.FloorArea * zone.Multiplier * zone.ListMultiplier * vent_mech.ZoneOAAreaRate( NumMechVentZone );
							vent_mech.TotZoneOAFlow += zone.Multiplier * zone.ListMultiplier * vent_mech.ZoneOAFlowRate( NumMechVentZone );
							vent_mech.TotZoneOAACH += zone.Multiplier * zone.ListMultiplier * ( vent_mech.ZoneOAACHRate( NumMechVentZone ) * zone.Volume / 3600.0 );
							break;
						}
					}
					if ( ! FoundZone ) {
						ShowWarningError( "Zone name = " + zone.Name + " in " + CurrentModuleObjects( CMO_MechVentilation ) + " object name = " + thisOAController.VentilationMechanicalName + " is not on the same air loop as Controller:OutdoorAir = " + thisOAController.Name );
						ShowContinueError( "This zone will not be used and the simulation will continue..." );
					}
				}

				// Shrink final arrays to conserve environment space
				if ( TempMechVentArrayCounter < vent_mech.NumofVentMechZones ) {
					vent_mech.VentMechZone.redimension( TempMechVentArrayCounter );
					vent_mech.ZoneOAAreaRate.redimension( TempMechVentArrayCounter );
					vent_mech.ZoneOAPeopleRate.redimension( TempMechVentArrayCounter );
					vent_mech.ZoneOAFlowRate.redimension( TempMechVentArrayCounter );
					vent_mech.ZoneOAACHRate.redimension( TempMechVentArrayCounter );
					vent_mech.ZoneDesignSpecOAObjIndex.redimension( TempMechVentArrayCounter );
					vent_mech.ZoneDesignSpecOAObjName.redimension( TempMechVentArrayCounter );

					vent_mech.ZoneADEffCooling.redimension( TempMechVentArrayCounter );
					vent_mech.ZoneADEffHeating.redimension( TempMechVentArrayCounter );
					vent_mech.ZoneADEffSchPtr.redimension( TempMechVentArrayCounter );
					vent_mech.ZoneADEffSchName.redimension( TempMechVentArrayCounter );

					vent_mech.NumofVentMechZones = TempMechVentArrayCounter;
				}

				// predefined report
				for ( jZone = 1; jZone <= vent_mech.NumofVentMechZones; ++jZone ) {
					zoneName = Zone( vent_mech.VentMechZone( jZone ) ).Name;
					PreDefTableEntry( pdchDCVventMechName, zoneName, vent_mech.Name );
					PreDefTableEntry( pdchDCVperPerson, zoneName, vent_mech.ZoneOAPeopleRate( jZone ), 6 );
					PreDefTableEntry( pdchDCVperArea, zoneName, vent_mech.ZoneOAAreaRate( jZone ), 6 );

					// added for new DCV inputs
					PreDefTableEntry( pdchDCVZoneADEffCooling, zoneName, vent_mech.ZoneADEffCooling( jZone ), 2 );
					PreDefTableEntry( pdchDCVZoneADEffHeating, zoneName, vent_mech.ZoneADEffHeating( jZone ), 2 );
					PreDefTableEntry( pdchDCVZoneADEffSchName, zoneName, GetScheduleName( vent_mech.ZoneADEffSchPtr( jZone ) ) );
				}

				// Check to see if any zones on an air loop are not accounted for by a mechanical ventilation object
				for ( AirLoopZoneInfoZoneNum = 1; AirLoopZoneInfoZoneNum <= AirLoopZoneInfo( AirLoopNum ).NumZones; ++AirLoopZoneInfoZoneNum ) {
					NumZone = AirLoopZoneInfo( AirLoopNum ).ActualZoneNumber( AirLoopZoneInfoZoneNum );
					FoundAreaZone = false;
					FoundPeopleZone = false;
					for ( NumMechVentZone = 1; NumMechVentZone <= vent_mech.NumofVentMechZones; ++NumMechVentZone ) {
						ZoneNum = vent_mech.VentMechZone( NumMechVentZone );
						if ( ZoneNum == NumZone ) {
							FoundAreaZone = true;
							if ( vent_mech.ZoneOAPeopleRate( NumMechVentZone ) > 0.0 ) {
								FoundPeopleZone = true;
							}
							break;
						}
					}
					if ( ! FoundAreaZone ) {
						ShowWarningError( "Zone name = " + Zone( NumZone ).Name + " is not accounted for by " + CurrentModuleObjects( CMO_MechVentilation ) + " object name = " + thisOAController.VentilationMechanicalName );
						ShowContinueError( "Ventilation per unit floor area has not been specified for this zone, which is connected to" );
						ShowContinueError( "the air loop served by Controller:OutdoorAir = " + thisOAController.Name + ". Simulation will continue..." );
					}
					if ( ! FoundPeopleZone ) {
						// Loop through people objects to see if this zone has a people object and only then show a warning
						for ( PeopleNum = 1; PeopleNum <= TotPeople; ++PeopleNum ) {
							if ( People( PeopleNum ).ZonePtr == NumZone ) {
								if ( ! FoundAreaZone ) {
									//  !             If the zone was found, then the people ventilation rate is set to 0
									//                CALL ShowWarningError('PEOPLE object for zone = '//TRIM(Zone(NumZone)%Name)// &
									//                                    ' is not accounted for by '//TRIM(CurrentModuleObjects(CMO_MechVentilation))//  &
									//                                    ' object name = '//TRIM(OAController(OAControllerNum)%VentilationMechanicalName))
									//                CALL ShowContinueError('A "PEOPLE" object has been specified in the idf for this zone, '// &
									//                                       'but the ventilation rate is set to 0 in this Controller:MechanicalVentilation Object.')
									//                CALL ShowContinueError('Check ventilation rate in Controller:MechanicalVentilation object. '//  &
									//                   ' Simulation will continue.')
									//              ELSE
									//             If the zone was not found, then the PEOPLE objects are not accounted for
									ShowWarningError( "PEOPLE object for zone = " + Zone( NumZone ).Name + " is not accounted for by " + CurrentModuleObjects( CMO_MechVentilation ) + " object name = " + thisOAController.VentilationMechanicalName );
									ShowContinueError( "A \"PEOPLE\" object has been specified in the idf for this zone, but it is not included in this " + CurrentModuleObjects( CMO_MechVentilation ) + " Object." );
									ShowContinueError( "Check " + CurrentModuleObjects( CMO_MechVentilation ) + " object. Simulation will continue." );
								}
							}
						}
					} else { // People > 0, check to make sure there is a people statement in the zone
						FoundAreaZone = false;
						for ( PeopleNum = 1; PeopleNum <= TotPeople; ++PeopleNum ) {
							if ( People( PeopleNum ).ZonePtr != NumZone ) continue;
							FoundAreaZone = true;
							break;
						}
						if ( ! FoundAreaZone ) {
							ShowWarningError( CurrentModuleObjects( CMO_MechVentilation ) + " = \"" + thisOAController.VentilationMechanicalName + "\", Zone=\"" + Zone( NumZone ).Name + "\"." );
							ShowContinueError( "No \"PEOPLE\" object has been specified in the idf for this zone, but the ventilation rate is > 0 in this Controller:MechanicalVentilation Object." );
							ShowContinueError( "Check ventilation rate in Controller:MechanicalVentilation object.  Simulation will continue." );
						}
					}
				}

			}

			MechVentCheckFlag( OAControllerNum ) = false;

		}
		//****

		// Perform a one time initialization of AirloopHVAC OA System report variables
		// If AirloopHVAC objects are used, NumPrimaryAirSys > 0 and the initialization proceeds and then sets
		// SetUpAirLoopHVACVariables to .FALSE. so this is never done again and only the first IF is checked
		// each time through Init. If for some reason the primary air system have not yet been read in, this
		// code waits for the air loop data to be available before performing the report variable initialization.
		// If AirloopHVAC objects are not used, NumPrimaryAirSys is always equal to 0 and only these
		// two IF statements are checked each time through Init (e.g., if StandAloneERV controllers are used
		// without AirloopHVAC objects).
		if ( InitOAControllerSetUpAirLoopHVACVariables ) {
			if ( AirLoopNum > 0 ) {
				// Added code to report (TH, 10/20/2008):
				//   air economizer status (1 = on, 0 = off or does not exist), and
				//   actual and minimum outside air fraction (0 to 1)
				for ( OAControllerLoop = 1; OAControllerLoop <= NumOAControllers; ++OAControllerLoop ) {
					auto & loopOAController( OAController( OAControllerLoop ) );

					//Find the outside air system that has the OA controller
					if ( loopOAController.ControllerType_Num == ControllerStandAloneERV ) continue; // ERV controller not on airloop
					OASysFound = false;
					thisOASys = 0;
					for ( OASysNum = 1; OASysNum <= NumOASystems; ++OASysNum ) {
						for ( OAControllerLoop2 = 1; OAControllerLoop2 <= OutsideAirSys( OASysNum ).NumControllers; ++OAControllerLoop2 ) {
							if ( SameString( OutsideAirSys( OASysNum ).ControllerName( OAControllerLoop2 ), loopOAController.Name ) ) {
								thisOASys = OASysNum;
								OASysFound = true;
								break;
							}
						}
						if ( OASysFound ) break;
					}

					if ( thisOASys <= 0 ) {
						//Check outside air system name
						ShowWarningError( "Cannot find the AirLoopHVAC:OutdoorAirSystem for the OA Controller: " + thisOAController.Name );
						AirLoopFound = false;
					} else {
						//Find the primary air loop that has the outside air system
						AirLoopFound = false;
						for ( thisAirLoop = 1; thisAirLoop <= NumPrimaryAirSys; ++thisAirLoop ) {
							for ( BranchNum = 1; BranchNum <= PrimaryAirSystem( thisAirLoop ).NumBranches; ++BranchNum ) {
								for ( CompNum = 1; CompNum <= PrimaryAirSystem( thisAirLoop ).Branch( BranchNum ).TotalComponents; ++CompNum ) {
									if ( ! SameString( PrimaryAirSystem( thisAirLoop ).Branch( BranchNum ).Comp( CompNum ).Name, OutsideAirSys( thisOASys ).Name ) || ! SameString( PrimaryAirSystem( thisAirLoop ).Branch( BranchNum ).Comp( CompNum ).TypeOf, "AirLoopHVAC:OutdoorAirSystem" ) ) continue;
									AirLoopFound = true;
									break;
								}
								if ( AirLoopFound ) break;
							}
							if ( AirLoopFound ) break;
						}
					}
					// Check primary air loop name
					if ( AirLoopFound && thisAirLoop > 0 ) {
						airloopName = PrimaryAirSystem( thisAirLoop ).Name; // OutsideAirSys(OASysIndex)%Name
					} else {
						ShowWarningError( "Cannot find the primary air loop for the OA Controller: " + thisOAController.Name );
						airloopName = "AirLoop not found";
					}

					//    Note use of OAControllerLoop here to keep DO Loop index separate from InitOAController local variable
					// CurrentModuleObject='AirLoopHVAC'
					SetupOutputVariable( "Air System Outdoor Air Economizer Status []", loopOAController.EconomizerStatus, "System", "Average", airloopName );

					SetupOutputVariable( "Air System Outdoor Air Heat Recovery Bypass Status []", loopOAController.HeatRecoveryBypassStatus, "System", "Average", airloopName );

					SetupOutputVariable( "Air System Outdoor Air Heat Recovery Bypass Heating Coil Activity Status []", loopOAController.HRHeatingCoilActive, "System", "Average", airloopName );
					SetupOutputVariable( "Air System Outdoor Air Heat Recovery Bypass Minimum Outdoor Air Mixed Air Temperature [C]", loopOAController.MixedAirTempAtMinOAFlow, "System", "Average", airloopName );

					SetupOutputVariable( "Air System Outdoor Air High Humidity Control Status []", loopOAController.HighHumCtrlStatus, "System", "Average", airloopName );

					SetupOutputVariable( "Air System Outdoor Air Flow Fraction []", loopOAController.OAFractionRpt, "System", "Average", airloopName );

					SetupOutputVariable( "Air System Outdoor Air Minimum Flow Fraction []", loopOAController.MinOAFracLimit, "System", "Average", airloopName );

					SetupOutputVariable( "Air System Outdoor Air Mass Flow Rate [kg/s]", loopOAController.OAMassFlow, "System", "Average", airloopName );

					SetupOutputVariable( "Air System Mixed Air Mass Flow Rate [kg/s]", loopOAController.MixMassFlow, "System", "Average", airloopName );

					if ( loopOAController.MixedAirSPMNum > 0 ) {
						SetupOutputVariable( "Air System Outdoor Air Maximum Flow Fraction []", loopOAController.MaxOAFracBySetPoint, "System", "Average", airloopName );
					}

					if ( AnyEnergyManagementSystemInModel ) {
						SetupEMSInternalVariable( "Outdoor Air Controller Maximum Mass Flow Rate", loopOAController.Name, "[kg/s]", loopOAController.MaxOAMassFlowRate );
						SetupEMSInternalVariable( "Outdoor Air Controller Minimum Mass Flow Rate", loopOAController.Name, "[kg/s]", loopOAController.MinOAMassFlowRate );
						SetupEMSActuator( "Outdoor Air Controller", loopOAController.Name, "Air Mass Flow Rate", "[kg/s]", loopOAController.EMSOverrideOARate, loopOAController.EMSOARateValue );
					}

					VentMechObjectNum = loopOAController.VentMechObjectNum;
					if ( VentMechObjectNum > 0 && thisAirLoop > 0){
						if (!VentilationMechanical( VentMechObjectNum ).DCVFlag){
							AirLoopControlInfo( thisAirLoop ).AirLoopDCVFlag = false;
						}
					}
				}

				InitOAControllerSetUpAirLoopHVACVariables = false;

			}
		}

		// Each time step
		if ( FirstHVACIteration ) {
			// Mixed air setpoint. Set by a setpoint manager.
			if ( thisOAController.ControllerType_Num == ControllerOutsideAir ) {
				if ( Node( thisOAController.MixNode ).TempSetPoint > SensedNodeFlagValue ) {
					thisOAController.MixSetTemp = Node( thisOAController.MixNode ).TempSetPoint;
				} else {
					thisOAController.MixSetTemp = thisOAController.TempLowLim;
				}

				TotalPeopleOAFlow = 0.0;
				if ( VentMechObjectNum != 0 ) {
					auto & vent_mech( VentilationMechanical( VentMechObjectNum ) );
					for ( ZoneIndex = 1; ZoneIndex <= vent_mech.NumofVentMechZones; ++ZoneIndex ) {
						ZoneNum = vent_mech.VentMechZone( ZoneIndex );

						// ZoneIntGain(ZoneNum)%NOFOCC is the number of occupants of a zone at each time step, already counting the occupant schedule
						if( vent_mech.ZoneDesignSpecOAObjIndex(ZoneIndex) > 0 ) {
							int OAFlowMethod = OARequirements( vent_mech.ZoneDesignSpecOAObjIndex( ZoneIndex ) ).OAFlowMethod;
							if ( OAFlowMethod == OAFlowPPer || OAFlowMethod == OAFlowSum || OAFlowMethod == OAFlowMax ) {
								TotalPeopleOAFlow += ZoneIntGain( ZoneNum ).NOFOCC * Zone( ZoneNum ).Multiplier * Zone( ZoneNum ).ListMultiplier * vent_mech.ZoneOAPeopleRate( ZoneIndex );
							}
						} else {
							// use default value if design specification object is missing
							TotalPeopleOAFlow += ZoneIntGain( ZoneNum ).NOFOCC * Zone( ZoneNum ).Multiplier * Zone( ZoneNum ).ListMultiplier * vent_mech.ZoneOAPeopleRate( ZoneIndex );
						}
					}
					vent_mech.TotPeopleOAFlow = TotalPeopleOAFlow;
				}
			} else {
				// Stand Alone ERV does not require a termperature setpoint schedule, make setpoint equal to lower economizer limit
				thisOAController.MixSetTemp = thisOAController.TempLowLim;
			}

		}

		// Each iteration

		if ( thisOAController.ControllerType_Num == ControllerOutsideAir ) {
			// zone exhaust mass flow is saved in AirLoopFlow%ZoneExhaust
			// the zone exhaust mass flow that is said to be balanced by simple air flows is saved in AirLoopFlow%ZoneExhaustBalanced
			if ( AirLoopNum > 0 ) {
				//OAController(OAControllerNum)%ExhMassFlow = AirLoopFlow(AirLoopNum)%ZoneExhaust
				thisOAController.ExhMassFlow = AirLoopFlow( AirLoopNum ).ZoneExhaust - AirLoopFlow( AirLoopNum ).ZoneExhaustBalanced - AirLoopFlow( AirLoopNum ).RetFlowAdjustment;
				if ( AirLoopControlInfo( AirLoopNum ).LoopFlowRateSet && ! FirstHVACIteration ) {
					// if flow rate has been specified by a manager, set it to the specified value
					thisOAController.MixMassFlow = AirLoopFlow( AirLoopNum ).ReqSupplyFrac * AirLoopFlow( AirLoopNum ).DesSupply;
				} else {
					thisOAController.MixMassFlow = Node( thisOAController.RetNode ).MassFlowRate + thisOAController.ExhMassFlow;
				}
			} else {
				thisOAController.ExhMassFlow = 0.0;
				thisOAController.MixMassFlow = Node( thisOAController.RetNode ).MassFlowRate;
			}
			if ( Node( thisOAController.MixNode ).MassFlowRateMaxAvail <= 0.0 ) {
				thisOAController.MixMassFlow = 0.0;
			}
		} else {
			// Mixed and exhaust flow rates are passed through to model CONTROLLER:STAND ALONE ERV in SimOAController
			thisOAController.OAMassFlow = thisOAController.MaxOAMassFlowRate;
			thisOAController.MixMassFlow = thisOAController.MaxOAMassFlowRate;
			thisOAController.ExhMassFlow = Node( thisOAController.RetNode ).MassFlowRate;
		}
		thisOAController.ExhMassFlow = max( thisOAController.ExhMassFlow, 0.0 );

		// Outside air values
		thisOAController.OATemp = Node( thisOAController.OANode ).Temp;
		thisOAController.OAEnth = Node( thisOAController.OANode ).Enthalpy;
		thisOAController.OAPress = Node( thisOAController.OANode ).Press;
		thisOAController.OAHumRat = Node( thisOAController.OANode ).HumRat;

		// Inlet air values (on OA input side)
		thisOAController.InletTemp = Node( thisOAController.InletNode ).Temp;
		thisOAController.InletEnth = Node( thisOAController.InletNode ).Enthalpy;
		thisOAController.InletPress = Node( thisOAController.InletNode ).Press;
		thisOAController.InletHumRat = Node( thisOAController.InletNode ).HumRat;

		// Return air values
		thisOAController.RetTemp = Node( thisOAController.RetNode ).Temp;
		thisOAController.RetEnth = Node( thisOAController.RetNode ).Enthalpy;

		// Check sensors faults for the air economizer
		iEco = thisOAController.Econo;
		if ( AnyFaultsInModel && ( iEco > NoEconomizer ) ) {
			for ( i = 1; i <= NumFaults; ++i ) {
				if ( ( Faults( i ).ControllerTypeEnum == iController_AirEconomizer ) && ( Faults( i ).ControllerID == OAControllerNum ) ) {

					if ( GetCurrentScheduleValue( Faults( i ).AvaiSchedPtr ) > 0.0 ) {
						rSchVal = 1.0;
						if ( Faults( i ).SeveritySchedPtr > 0 ) {
							rSchVal = GetCurrentScheduleValue( Faults( i ).SeveritySchedPtr );
						}
					} else {
						// no fault
						continue;
					}

					rOffset = rSchVal * Faults( i ).Offset;

					if ( std::abs( rOffset ) < 0.000000001 ) continue;

					// ECONOMIZER - outdoor air dry-bulb temperature sensor offset
					{ auto const SELECT_CASE_var( iEco );
					if ( ( SELECT_CASE_var == FixedDryBulb ) || ( SELECT_CASE_var == DifferentialDryBulb ) || ( SELECT_CASE_var == FixedDewPointAndDryBulb ) || ( SELECT_CASE_var == ElectronicEnthalpy ) || ( SELECT_CASE_var == DifferentialDryBulbAndEnthalpy ) ) {
						if ( Faults( i ).FaultTypeEnum == iFault_TemperatureSensorOffset_OutdoorAir ) {
							// FaultModel:TemperatureSensorOffset:OutdoorAir
							thisOAController.OATemp += rOffset;
							thisOAController.InletTemp += rOffset;
						}
					} else {
					}}

					// ECONOMIZER - outdoor air humidity ratio sensor offset. really needed ???
					{ auto const SELECT_CASE_var( iEco );
					if ( ( SELECT_CASE_var == FixedDewPointAndDryBulb ) || ( SELECT_CASE_var == ElectronicEnthalpy ) ) {
						if ( Faults( i ).FaultTypeEnum == iFault_HumiditySensorOffset_OutdoorAir ) {
							// FaultModel:HumiditySensorOffset:OutdoorAir
							thisOAController.OAHumRat += rOffset;
							thisOAController.InletHumRat += rOffset;
						}
					} else {
					}}

					// ECONOMIZER - outdoor air enthalpy sensor offset
					{ auto const SELECT_CASE_var( iEco );
					if ( ( SELECT_CASE_var == FixedEnthalpy ) || ( SELECT_CASE_var == ElectronicEnthalpy ) || ( SELECT_CASE_var == DifferentialDryBulbAndEnthalpy ) ) {
						if ( Faults( i ).FaultTypeEnum == iFault_EnthalpySensorOffset_OutdoorAir ) {
							// FaultModel:EnthalpySensorOffset:OutdoorAir
							thisOAController.OAEnth += rOffset;
							thisOAController.InletEnth += rOffset;
						}
					} else {
					}}

					// ECONOMIZER - return air dry-bulb temperature sensor offset
					{ auto const SELECT_CASE_var( iEco );
					if ( ( SELECT_CASE_var == DifferentialDryBulb ) || ( SELECT_CASE_var == DifferentialDryBulbAndEnthalpy ) ) {
						if ( Faults( i ).FaultTypeEnum == iFault_TemperatureSensorOffset_ReturnAir ) {
							// FaultModel:TemperatureSensorOffset:ReturnAir
							thisOAController.RetTemp += rOffset;
						}
					} else {
					}}

					// ECONOMIZER - return air enthalpy sensor offset
					{ auto const SELECT_CASE_var( iEco );
					if ( ( SELECT_CASE_var == ElectronicEnthalpy ) || ( SELECT_CASE_var == DifferentialDryBulbAndEnthalpy ) ) {
						if ( Faults( i ).FaultTypeEnum == iFault_EnthalpySensorOffset_ReturnAir ) {
							// FaultModel:EnthalpySensorOffset:ReturnAir
							thisOAController.RetEnth += rOffset;
						}
					} else {
					}}
				}
			}
		}

		if ( ErrorsFound ) {
			ShowFatalError( "Error in " + CurrentModuleObjects( CMO_OAController ) + "; program terminated" );
		}

	}

	void
	InitOAMixer(
		int const OAMixerNum,
		bool const FirstHVACIteration
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   Oct 1998
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE
		// Initialize the OAMixer data structure with input node data

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// Using/Aliasing
		using namespace DataLoopNode;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int RetNode;
		int InletNode;
		int RelNode;

		RetNode = OAMixer( OAMixerNum ).RetNode;
		InletNode = OAMixer( OAMixerNum ).InletNode;
		RelNode = OAMixer( OAMixerNum ).RelNode;

		if ( BeginEnvrnFlag && FirstHVACIteration ) {
		}

		if ( BeginDayFlag ) {
		}

		if ( FirstHVACIteration ) {
		}

		// Each iteration

		// Return air stream data
		OAMixer( OAMixerNum ).RetTemp = Node( RetNode ).Temp;
		OAMixer( OAMixerNum ).RetHumRat = Node( RetNode ).HumRat;
		OAMixer( OAMixerNum ).RetEnthalpy = Node( RetNode ).Enthalpy;
		OAMixer( OAMixerNum ).RetPressure = Node( RetNode ).Press;
		OAMixer( OAMixerNum ).RetMassFlowRate = Node( RetNode ).MassFlowRate;
		// Outside air stream data
		OAMixer( OAMixerNum ).OATemp = Node( InletNode ).Temp;
		OAMixer( OAMixerNum ).OAHumRat = Node( InletNode ).HumRat;
		OAMixer( OAMixerNum ).OAEnthalpy = Node( InletNode ).Enthalpy;
		OAMixer( OAMixerNum ).OAPressure = Node( InletNode ).Press;
		OAMixer( OAMixerNum ).OAMassFlowRate = Node( InletNode ).MassFlowRate;
		// Relief air data
		OAMixer( OAMixerNum ).RelMassFlowRate = Node( RelNode ).MassFlowRate;

	}

	// End of Initialization Section of the Module
	//******************************************************************************

	// Beginning Calculation Section of the Module
	//******************************************************************************

	void
	OAControllerProps::CalcOAController(
		int const AirLoopNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   Oct 1998
		//       MODIFIED       Shirey/Raustad FSEC, June 2003
		//                      Tianzhen Hong, Feb 2009 for new DCV
		//                      Brent Griffith ,EMS override of OA rate
		//                      Mangesh Basarkar, 06/2011: Modifying outside air calculation based on DCV flag
		//                      Chandan Sharma, FSEC, 25Aug 2011 - Added ProportionalControl
		//                           to enhance CO2 based DCV control
		//                      Tianzhen Hong, March 2012, zone maximum OA fraction - a TRACE feature
		//                      Tianzhen Hong, March 2012, multi-path VRP based on ASHRAE 62.1-2010
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE
		// Determine the outside air flow

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// DOE-2.1E Supplement pages 3.97 - 3.100
		// BLAST User Reference pages 183 - 186
		// ASHRAE Standard 62.1-2010

		// Using/Aliasing
		using General::RoundSigDigits;
		using CurveManager::CurveValue;
		using InputProcessor::FindItemInList;
		using InputProcessor::GetNumObjectsFound;
		using DataHeatBalFanSys::ZoneAirHumRat;
		using DataGlobals::WarmupFlag;
		using DataGlobals::DoingSizing;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcOAController: " );
		static std::string const CurrentModuleObject( CurrentModuleObjects( CMO_OAController ) );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 OutAirMinFrac; // Local variable used to calculate min OA fraction

		Real64 MechVentOutsideAirMinFrac; // fraction of OA specified by mechanical ventilation object
		Real64 MechVentOutsideAirFlow; // outside air mass flow rate specified by mechanical ventilation object
		Real64 MinOASchedVal; // value of the minimum outside air schedule
		Real64 OASignal; // Outside air flow rate fraction (0.0 to 1.0)
		bool AirLoopCyclingFan; // Type of air loop fan (TRUE if Fan:OnOff)
		Real64 MinOAflowfracVal;
		Real64 MaxOAflowfracVal;
		bool HighHumidityOperationFlag; // TRUE if zone humidistat senses a high humidity condition
		Real64 RecircTemp; // - return air temp, used for custom economizer control calculation
		Real64 MixedAirTempAtMinOAFlow; // - mixed air temperature at min flow rate, used for custom economizer control calculation
		Real64 RecircMassFlowRateAtMinOAFlow; // recirc air mass flow rate at min OA, used for custom economizer control calculation
		Real64 ReliefMassFlowAtMinOA; // relief air mass flow rate at min OA, used for custom economizer control calculation
		Real64 SysSA( 0.0 ); // System supply air flow rate
		MinOASchedVal = 1.0;

		if ( AirLoopNum > 0 ) {
			AirLoopCyclingFan = ( AirLoopControlInfo( AirLoopNum ).FanOpMode == CycFanCycCoil );
		} else {
			AirLoopCyclingFan = false;
		}

		// Check for no flow
		if ( this->MixMassFlow <= SmallMassFlow ) {

			this->OAMassFlow = 0.0; // outside air mass flow rate
			this->RelMassFlow = 0.0; // relief air mass flow rate
			this->MixMassFlow = 0.0; // mixed air mass flow rate
			this->MinOAFracLimit = 0.0; // minimum OA fraction limit

			this->EconomizerStatus = 0; // economizer status for reporting
			this->HeatRecoveryBypassStatus = 0; // HR bypass status for reporting
			this->HRHeatingCoilActive = 0; // resets report variable
			this->MixedAirTempAtMinOAFlow = Node( this->RetNode ).Temp; // track return T
			this->HighHumCtrlStatus = 0; // high humdity control status for reporting
			this->OAFractionRpt = 0.0; // actual OA fraction for reporting

			this->EconoActive = false; // DataAirLoop variable (OA Controllers)
			this->HighHumCtrlActive = false; // DataAirLoop variable (OA Controllers)

			// also reset air loop data for use by other routines
			if ( AirLoopNum > 0 ) {
				auto & curAirLoopControlInfo( AirLoopControlInfo( AirLoopNum ) );
				auto & curAirLoopFlow( AirLoopFlow( AirLoopNum ) );

				curAirLoopControlInfo.EconoActive = false; // DataAirLoop variable (AirloopHVAC)
				curAirLoopControlInfo.HeatRecoveryBypass = false; // DataAirLoop variable (AirloopHVAC)
				curAirLoopControlInfo.HighHumCtrlActive = false; // DataAirLoop variable (AirloopHVAC)
				curAirLoopControlInfo.ResimAirLoopFlag = false; // DataAirLoop variable (AirloopHVAC)
				curAirLoopFlow.OAFrac = 0.0; // DataAirLoop variable (AirloopHVAC)
				curAirLoopFlow.OAMinFrac = 0.0; // DataAirLoop variable (AirloopHVAC)
				curAirLoopFlow.MinOutAir = 0.0;
			}

			return;
		}

		// set OutAirMinFrac
		if ( AirLoopNum > 0 ) {
			auto & curAirLoopFlow( AirLoopFlow( AirLoopNum ) );

			if ( curAirLoopFlow.DesSupply >= SmallAirVolFlow ) {
				OutAirMinFrac = this->MinOAMassFlowRate / curAirLoopFlow.DesSupply;
			} else {
				OutAirMinFrac = 0.0;
			}
		} else {
			if ( this->MaxOA >= SmallAirVolFlow ) {
				OutAirMinFrac = this->MinOA / this->MaxOA;
			} else {
				OutAirMinFrac = 0.0;
			}
		}
		if ( this->MinOASchPtr > 0 ) {
			MinOASchedVal = GetCurrentScheduleValue( this->MinOASchPtr );
			MinOASchedVal = min( max( MinOASchedVal, 0.0 ), 1.0 );
			OutAirMinFrac *= MinOASchedVal;
		}

		// Get mechanical ventilation
		MechVentOutsideAirFlow = 0.0;
		MechVentOutsideAirMinFrac = 0.0;
		if ( AirLoopNum > 0 && this->VentMechObjectNum != 0 ) {
			auto & curAirLoopControlInfo( AirLoopControlInfo( AirLoopNum ) );
			auto & curAirLoopFlow( AirLoopFlow( AirLoopNum ) );

			// Get system supply air flow rate
			if ( curAirLoopControlInfo.LoopFlowRateSet ) {
				// if flow rate has been specified by a manager, set it to the specified value
				// DesSupply and SupFlow are mass flow rate in kg/s
				SysSA = curAirLoopFlow.ReqSupplyFrac * curAirLoopFlow.DesSupply;
			} else {
				SysSA = curAirLoopFlow.SupFlow;
			}
			VentilationMechanical( this->VentMechObjectNum ).CalcMechVentController( SysSA, MechVentOutsideAirFlow );
			MechVentOutsideAirMinFrac = MechVentOutsideAirFlow / curAirLoopFlow.DesSupply;
			if ( curAirLoopFlow.FanPLR > 0.0) {
				MechVentOutsideAirMinFrac *= curAirLoopFlow.FanPLR;
				MechVentOutsideAirFlow *= curAirLoopFlow.FanPLR;
			}
		}
		//****** use greater of Mechanical Ventilation Outside Air fraction and OutAirMinFrac
		OutAirMinFrac = max( OutAirMinFrac, MechVentOutsideAirMinFrac );

		OutAirMinFrac = min( max( OutAirMinFrac, 0.0 ), 1.0 );

		// At this point, OutAirMinFrac is still based on AirLoopFlow.DesSupply
		if ( AirLoopNum > 0 ) {
			auto & curAirLoopFlow( AirLoopFlow( AirLoopNum ) );

			curAirLoopFlow.MinOutAir = OutAirMinFrac * curAirLoopFlow.DesSupply;

			// calculate mixed air temp at min OA flow rate
			ReliefMassFlowAtMinOA = max( curAirLoopFlow.MinOutAir - this->ExhMassFlow, 0.0 );
			RecircMassFlowRateAtMinOAFlow = max( Node( this->RetNode ).MassFlowRate - ReliefMassFlowAtMinOA, 0.0 );
			if ( ( RecircMassFlowRateAtMinOAFlow + curAirLoopFlow.MinOutAir ) > 0.0 ) {
				RecircTemp = Node( this->RetNode ).Temp;
				MixedAirTempAtMinOAFlow = ( RecircMassFlowRateAtMinOAFlow * RecircTemp + curAirLoopFlow.MinOutAir * Node( this->OANode ).Temp ) / ( RecircMassFlowRateAtMinOAFlow + curAirLoopFlow.MinOutAir );
			} else {
				MixedAirTempAtMinOAFlow = Node( this->RetNode ).Temp;
			}
			this->MixedAirTempAtMinOAFlow = MixedAirTempAtMinOAFlow;
		}

		// Economizer
		CalcOAEconomizer( AirLoopNum, OutAirMinFrac, OASignal, HighHumidityOperationFlag );

		// Apply Minimum Fraction of Outdoor Air Schedule
		if ( this->MinOAflowSchPtr > 0 ) {
			MinOAflowfracVal = GetCurrentScheduleValue( this->MinOAflowSchPtr );
			MinOAflowfracVal = min( max( MinOAflowfracVal, 0.0 ), 1.0 );
			OutAirMinFrac = max( MinOAflowfracVal, OutAirMinFrac );
			OASignal = max( MinOAflowfracVal, OASignal );
		}

		if ( this->MaxOAflowSchPtr > 0 ) {
			MaxOAflowfracVal = GetCurrentScheduleValue( this->MaxOAflowSchPtr );
			MaxOAflowfracVal = min( max( MaxOAflowfracVal, 0.0 ), 1.0 );
			if ( MaxOAflowfracVal < OutAirMinFrac ) {
				OutAirMinFrac = MaxOAflowfracVal;
			}
			OASignal = min( MaxOAflowfracVal, OASignal );

			if ( this->MinOAflowSchPtr > 0 ) {
				if ( MaxOAflowfracVal < MinOAflowfracVal ) {
					ShowWarningError( "Min OA flow frac Greater than Max OA flow frac - check the Schedules in \"Controller:OutdoorAir \" " + this->MinOAflowSch + this->MaxOAflowSch );
				}
			}

		}

		this->OAMassFlow = OASignal * this->MixMassFlow;

		// Do not allow OA to be below Ventilation:Mechanical flow rate or above mixed mass flow rate
		if ( AirLoopNum > 0 && VentMechObjectNum != 0 ) {
			if ( MechVentOutsideAirFlow > this->OAMassFlow ) {
				this->OAMassFlow = min( MechVentOutsideAirFlow, this->MixMassFlow );
			}
		}

		// Do not allow OA to be below Exh for controller:outside air
		if ( this->ControllerType_Num == ControllerOutsideAir ) {
			this->OAMassFlow = max( this->ExhMassFlow, this->OAMassFlow );
		}

		// if fixed minimum, don't let go below min OA
		if ( this->FixedMin ) {
			// cycling fans allow "average" min OA to be below minimum
			if ( ! AirLoopCyclingFan ) {
				this->OAMassFlow = max( this->OAMassFlow, this->MinOAMassFlowRate * MinOASchedVal );
			}
		}

		// Don't let OA flow be > mixed air flow.
		// Seems if RAB (return air bypass) that this should be don't let OA flow be > design supply flow but that causes other issues
		this->OAMassFlow = min( this->OAMassFlow, this->MixMassFlow );

		// Don't let the OA flow be > than the max OA limit. OA for high humidity control is allowed to be greater than max OA.
		// Night Ventilation has priority and may override an OASignal > 1 high humidity condition with OASignal = 1
		if ( HighHumidityOperationFlag ) {
			this->OAMassFlow = min( this->OAMassFlow, this->MaxOAMassFlowRate * max( 1.0, OASignal ) );
		} else {
			this->OAMassFlow = min( this->OAMassFlow, this->MaxOAMassFlowRate );
		}

		if ( !WarmupFlag && !DoingSizing && ( this->ManageDemand ) && ( this->OAMassFlow > this->DemandLimitFlowRate ) )
			this->OAMassFlow = this->DemandLimitFlowRate;
		if ( this->EMSOverrideOARate ) {
			this->OAMassFlow = this->EMSOARateValue;
		}

		// save the min outside air flow fraction and max outside air mass flow rate
		if ( AirLoopNum > 0 ) {
			auto & curAirLoopControlInfo( AirLoopControlInfo( AirLoopNum ) );
			auto & curAirLoopFlow( AirLoopFlow( AirLoopNum ) );

			curAirLoopFlow.OAMinFrac = OutAirMinFrac;
			curAirLoopFlow.MinOutAir = OutAirMinFrac * this->MixMassFlow;
			if ( this->MixMassFlow > 0.0 ) {
				curAirLoopFlow.OAFrac = this->OAMassFlow / this->MixMassFlow;
			} else {
				curAirLoopFlow.OAFrac = 0.0;
			}
			this->MinOAFracLimit = OutAirMinFrac;
			if ( HighHumidityOperationFlag && OASignal > 1.0 ) {
				curAirLoopFlow.MaxOutAir = this->MaxOAMassFlowRate * OASignal;
			} else {
				curAirLoopFlow.MaxOutAir = this->MaxOAMassFlowRate;
			}

			// MJW - Not sure if this is necessary but keeping it for now
			if ( curAirLoopControlInfo.HeatingActiveFlag && curAirLoopControlInfo.EconomizerFlowLocked ) {
				// The airloop needs to be simulated again so that the heating coil & HX can be resimulated
				if ( curAirLoopControlInfo.HeatRecoveryResimFlag && curAirLoopControlInfo.OASysComponentsSimulated ) {
					curAirLoopControlInfo.ResimAirLoopFlag = true;
					curAirLoopControlInfo.HeatRecoveryResimFlag = false;
					curAirLoopControlInfo.HeatRecoveryResimFlag2 = true;
					// on the first iteration, air loop heating coils have not be simulated so HeatingCoilActive=FALSE
					// on the second iteration, the heating coils could have been on, but logic tests here could deactivate heating coil
					// reset heating coil active status and HX since logic tests may turn off heating coil
					// the ResimAirLoopFlag will force another iteration and things should line up on subsequent iterations
					curAirLoopControlInfo.HeatingActiveFlag = false;
					this->HRHeatingCoilActive = 0;
					curAirLoopControlInfo.HeatRecoveryBypass = true;
					this->HeatRecoveryBypassStatus = 1;
				} else if ( curAirLoopControlInfo.HeatRecoveryResimFlag2 ) {
					curAirLoopControlInfo.ResimAirLoopFlag = true;
					curAirLoopControlInfo.HeatRecoveryResimFlag2 = false;
				} else {
					curAirLoopControlInfo.ResimAirLoopFlag = false;
				}
			} else {
				this->HRHeatingCoilActive = 0;
			}

		} // if (AirLoopNum > 0)

		// Set the relief air flow rate (must be done last to account for changes in OAMassFlow
		this->RelMassFlow = max( this->OAMassFlow - this->ExhMassFlow, 0.0 );

		// Save OA fraction for reporting
		if ( this->MixMassFlow > 0 ) {
			this->OAFractionRpt = this->OAMassFlow / this->MixMassFlow;
		} else {
			if ( this->OAMassFlow > 0 ) {
				this->OAFractionRpt = OASignal;
			} else {
				this->OAFractionRpt = 0.0;
			}
		}
	}

	void
	VentilationMechanicalProps::CalcMechVentController(
		Real64 & SysSA,
		Real64 & MechVentOutsideAirFlow
	)
	{
		using DataContaminantBalance::ZoneSysContDemand;
		using DataGlobals::DisplayExtraWarnings;
		using DataHeatBalance::ZoneIntGain;
		using DataHeatBalance::Zone;
		using DataHeatBalance::People;
		using DataHeatBalance::TotPeople;
		using DataZoneEnergyDemands::ZoneSysEnergyDemand;
		using DataZoneEquipment::ZoneEquipConfig;
		using General::RoundSigDigits;
		using Psychrometrics::PsyRhoAirFnPbTdbW;

		static std::string const RoutineName("CalcMechVentController: ");
		static std::string const CurrentModuleObject(CurrentModuleObjects(CMO_MechVentilation));

		// new local variables for DCV
		Real64 ZoneOAPeople; // Zone OA flow rate based on number of occupants
		Real64 ZoneOAArea; // Zone OA flow rate based on space floor area
		Real64 ZoneOAFlow; // Zone OA flow rate based on simple flow
		Real64 ZoneOAACH; // Zone OA flow rate based on air changes per hour
		Real64 ZoneOABZ; // Zone breathing-zone OA flow rate
		Real64 ZoneOAMin; // Minimum Zone OA flow rate when the zone is unoccupied (i.e. ZoneOAPeople = 0)
		// used for "ProportionalControl" System outdoor air method
		Real64 ZoneOAMax; // Maximum Zone OA flow rate (ZoneOAPeople + ZoneOAArea)
		// used for "ProportionalControl" System outdoor air method
		Real64 ZoneOA; // Zone OA flow rate
		Real64 ZoneOAFrac; // Zone OA fraction (as a fraction of actual supply air flow rate)
		Real64 ZoneEz; // Zone air distribution effectiveness
		Real64 ZoneSA; // Zone supply air flow rate
		Real64 ZonePA; // Zone primary air flow rate
		Real64 SysOAuc; // System uncorrected OA flow rate
		Real64 SysOA; // System supply OA flow rate
		Real64 SysEv; // System ventilation efficiency
		Real64 NodeTemp; // node temperature
		Real64 NodeHumRat; // node humidity ratio
		Real64 MassFlowRate; // Temporary variable
		Real64 ZoneLoad; // Zone loads
		int ZoneIndex;
		int ZoneNum;
		int ZoneADEffSchPtr;
		std::string ZoneName; // Zone name
		int OAIndex; // index to design specification outdoor air objects
		int PeopleNum;
		Real64 ZoneMaxCO2; // Breathing-zone CO2 concentartion
		Real64 ZoneMinCO2; // Minimum CO2 concentration in zone
		Real64 ZoneContamControllerSched; // Schedule value for ZoneControl:ContaminantController
		Real64 CO2PeopleGeneration; // CO2 generation from people at design level

		static Real64 Ep( 1.0 ); // zone primary air fraction
		static Real64 Er( 0.0 ); // zone secondary recirculation fraction
		static Real64 Fa( 1.0 ); // temporary variable used in multi-path VRP calc
		static Real64 Fb( 1.0 );
		static Real64 Fc( 1.0 );
		static Real64 Xs( 1.0 ); // uncorrected system outdoor air fraction
		static Real64 Evz( 1.0 ); // zone ventilation efficiency

		int PriNode; // primary node of zone terminal unit
		int InletNode; // outlet node of zone terminal unit

		ZoneMaxCO2 = 0.0;
		ZoneMinCO2 = 0.0;
		ZoneOAMin = 0.0;
		ZoneOAMax = 0.0;
		ZoneContamControllerSched = 0.0;
		MechVentOutsideAirFlow = 0.0;

		// Apply mechanical ventilation only when it is available/allowed
		if ( GetCurrentScheduleValue( this->SchPtr ) > 0 ) {
			if ( this->SystemOAMethod == SOAM_IAQP ) {
				// IAQP for CO2 control
				SysOA = 0.0;
				for ( ZoneIndex = 1; ZoneIndex <= this->NumofVentMechZones; ++ZoneIndex ) {
					ZoneNum = this->VentMechZone( ZoneIndex );
					SysOA += ZoneSysContDemand( ZoneNum ).OutputRequiredToCO2SP;
				}
				MechVentOutsideAirFlow = SysOA;
			} else if ( this->SystemOAMethod == SOAM_IAQPGC ) {
				// IAQP for generic contaminant control
				SysOA = 0.0;
				for ( ZoneIndex = 1; ZoneIndex <= this->NumofVentMechZones; ++ZoneIndex ) {
					ZoneNum = this->VentMechZone( ZoneIndex );
					SysOA += ZoneSysContDemand( ZoneNum ).OutputRequiredToGCSP;
				}
				MechVentOutsideAirFlow = SysOA;
			} else if ( this->SystemOAMethod == SOAM_IAQPCOM ) {
				// IAQP for both CO2 and generic contaminant control
				SysOA = 0.0;
				for ( ZoneIndex = 1; ZoneIndex <= this->NumofVentMechZones; ++ZoneIndex ) {
					ZoneNum = this->VentMechZone( ZoneIndex );
					SysOA += ZoneSysContDemand( ZoneNum ).OutputRequiredToCO2SP;
				}
				MechVentOutsideAirFlow = SysOA;
				SysOA = 0.0;
				for ( ZoneIndex = 1; ZoneIndex <= this->NumofVentMechZones; ++ZoneIndex ) {
					ZoneNum = this->VentMechZone( ZoneIndex );
					SysOA += ZoneSysContDemand( ZoneNum ).OutputRequiredToGCSP;
				}
				MechVentOutsideAirFlow = max( SysOA, MechVentOutsideAirFlow );
			} else {
				// for system OA methods: Zone_Sum, VRP, CO2 methods
				// new code for DCV method complying with the VRP defined in ASHRAE Standard 62.1-2010

				// Loop through each zone first to calc uncorrected system OA flow rate
				SysOAuc = 0.0;
				SysOA = 0.0;
				for ( ZoneIndex = 1; ZoneIndex <= this->NumofVentMechZones; ++ZoneIndex ) {
					ZoneNum = this->VentMechZone( ZoneIndex );
					auto & curZone( Zone( ZoneNum ) );

					// Calc the zone OA flow rate based on the people component
					// ZoneIntGain(ZoneNum)%NOFOCC is the number of occupants of a zone at each time step, already counting the occupant schedule
					//  Checking DCV flag before calculating zone OA per person
					if ( this->DCVFlag && this->SystemOAMethod != SOAM_ProportionalControlDesOcc ) {
						ZoneOAPeople = ZoneIntGain( ZoneNum ).NOFOCC * curZone.Multiplier * curZone.ListMultiplier * this->ZoneOAPeopleRate( ZoneIndex );
					} else {
						ZoneOAPeople = curZone.TotOccupants * curZone.Multiplier * curZone.ListMultiplier * this->ZoneOAPeopleRate( ZoneIndex );
					}

					// Calc the zone OA flow rate based on the floor area component
					ZoneOAArea = curZone.FloorArea * curZone.Multiplier * curZone.ListMultiplier * this->ZoneOAAreaRate( ZoneIndex );
					ZoneOAFlow = curZone.Multiplier * curZone.ListMultiplier * this->ZoneOAFlowRate( ZoneIndex );
					ZoneOAACH = curZone.Multiplier * curZone.ListMultiplier * ( this->ZoneOAACHRate( ZoneIndex ) * Zone( ZoneIndex ).Volume ) / 3600.0;

					// Calc the breathing-zone OA flow rate
					OAIndex = this->ZoneDesignSpecOAObjIndex( ZoneIndex );
					if ( OAIndex > 0 ) {
						{ auto const SELECT_CASE_var( OARequirements( OAIndex ).OAFlowMethod );
						if ( SELECT_CASE_var == OAFlowPPer ) {
							ZoneOABZ = ZoneOAPeople;
						} else if ( SELECT_CASE_var == OAFlow ) {
							ZoneOABZ = ZoneOAFlow;
						} else if ( SELECT_CASE_var == OAFlowPerArea ) {
							ZoneOABZ = ZoneOAArea;
						} else if ( SELECT_CASE_var == OAFlowACH ) {
							ZoneOABZ = ZoneOAACH;
						} else if ( SELECT_CASE_var == OAFlowSum ) {
							ZoneOABZ = ZoneOAPeople + ZoneOAArea + ZoneOAFlow + ZoneOAACH;
						} else if ( SELECT_CASE_var == OAFlowMax ) {
							ZoneOABZ = max( ZoneOAPeople, ZoneOAArea, ZoneOAFlow, ZoneOAACH );
						} else {
							ZoneOABZ = 0.0;
						}}
					} else {
						ZoneOABZ = ZoneOAPeople;
					}

					if ( this->SystemOAMethod == SOAM_ZoneSum ) {
						// Sum the zone OA flow rates and done
						SysOA += ZoneOABZ;
					} else {
						// Calc the uncorrected system OA flow rate - VRP and ProportionalControl
						SysOAuc += ZoneOABZ;
					}
				}

				// get system supply air flow rate
				if ( this->SystemOAMethod == SOAM_VRP || this->SystemOAMethod == SOAM_ProportionalControlSchOcc || this->SystemOAMethod == SOAM_ProportionalControlDesOcc ) {

					// System supply air flow rate is always greater than or equal the system outdoor air flow rate
					if ( ( SysSA > 0.0 ) && ( SysSA < ( SysOAuc * StdRhoAir ) ) ) SysSA = SysOAuc * StdRhoAir;

					// calc Xs - average outdoor air fraction
					if ( SysSA > 0.0 ) {
						Xs = ( SysOAuc * StdRhoAir ) / SysSA;
					} else {
						Xs = 0.0;
					}

					// Loop through each zone again
					SysEv = 2.0; // starting with a big fraction
					for ( ZoneIndex = 1; ZoneIndex <= this->NumofVentMechZones; ++ZoneIndex ) {
						ZoneNum = this->VentMechZone( ZoneIndex );
						int ZoneEquipConfigNum = ZoneNum; // correspondence - 1:1 of ZoneEquipConfig to Zone index
						ZoneEz = 0.0;

						// Assign references
						auto & curZone( Zone( ZoneNum ) );
						auto & curZoneEquipConfig( ZoneEquipConfig( ZoneEquipConfigNum ) );
						auto & curZoneSysEnergyDemand( ZoneSysEnergyDemand( ZoneEquipConfigNum ) );
						ZoneName = curZone.Name;

						// Calc the zone OA flow rate based on the people component
						// ZoneIntGain(ZoneNum)%NOFOCC is the number of occupants of a zone at each time step, already counting the occupant schedule
						//  Checking DCV flag before calculating zone OA per person
						if ( this->DCVFlag && this->SystemOAMethod != SOAM_ProportionalControlDesOcc ) {
							ZoneOAPeople = ZoneIntGain( ZoneNum ).NOFOCC * curZone.Multiplier * curZone.ListMultiplier * this->ZoneOAPeopleRate( ZoneIndex );
						} else {
							ZoneOAPeople = curZone.TotOccupants * curZone.Multiplier * curZone.ListMultiplier * this->ZoneOAPeopleRate( ZoneIndex );
							CO2PeopleGeneration = 0.0;
							if ( this->SystemOAMethod == SOAM_ProportionalControlDesOcc ) {
								// Accumulate CO2 generation from people at design occupancy and current activity level
								for ( PeopleNum = 1; PeopleNum <= TotPeople; ++PeopleNum ) {
									if ( People( PeopleNum ).ZonePtr != ZoneNum ) continue;
									CO2PeopleGeneration += People( PeopleNum ).NumberOfPeople * People( PeopleNum ).CO2RateFactor * GetCurrentScheduleValue( People( PeopleNum ).ActivityLevelPtr );
								}
							}
						}

						// Calc the zone OA flow rate based on the floor area component
						ZoneOAArea = curZone.FloorArea * curZone.Multiplier * curZone.ListMultiplier * this->ZoneOAAreaRate( ZoneIndex );
						ZoneOAFlow = curZone.Multiplier * curZone.ListMultiplier * this->ZoneOAFlowRate( ZoneIndex );
						ZoneOAACH = curZone.Multiplier * curZone.ListMultiplier * ( this->ZoneOAACHRate( ZoneIndex ) * Zone( ZoneIndex ).Volume ) / 3600.0;

						// Calc the breathing-zone OA flow rate
						OAIndex = this->ZoneDesignSpecOAObjIndex( ZoneIndex );
						if ( OAIndex > 0 ) {
							{ auto const SELECT_CASE_var( OARequirements( OAIndex ).OAFlowMethod );
							if ( SELECT_CASE_var == OAFlowPPer ) {
								ZoneOABZ = ZoneOAPeople;
							} else if ( SELECT_CASE_var == OAFlow ) {
								ZoneOABZ = ZoneOAFlow;
							} else if ( SELECT_CASE_var == OAFlowPerArea ) {
								ZoneOABZ = ZoneOAArea;
							} else if ( SELECT_CASE_var == OAFlowACH ) {
								ZoneOABZ = ZoneOAACH;
							} else if ( SELECT_CASE_var == OAFlowSum ) {
								ZoneOABZ = ZoneOAPeople + ZoneOAArea + ZoneOAFlow + ZoneOAACH;
							} else if ( SELECT_CASE_var == OAFlowMax ) {
								ZoneOABZ = max( ZoneOAPeople, ZoneOAArea, ZoneOAFlow, ZoneOAACH );
							} else {
								ZoneOABZ = 0.0;
							}}
						}

						// use the ventilation rate procedure in ASHRAE Standard 62.1-2007
						// Calc the zone supplied OA flow rate counting the zone air distribution effectiveness
						//  First check whether the zone air distribution effectiveness schedule exists, if yes uses it;
						//   otherwise uses the inputs of zone distribution effectiveness in cooling mode or heating mode
						ZoneADEffSchPtr = this->ZoneADEffSchPtr( ZoneIndex );
						if ( ZoneADEffSchPtr > 0 ) {
							// Get schedule value for the zone air distribution effectiveness
							ZoneEz = GetCurrentScheduleValue( ZoneADEffSchPtr );
						} else {
							ZoneLoad = ZoneSysEnergyDemand( curZoneEquipConfig.ActualZoneNum ).TotalOutputRequired;

							// Zone in cooling mode
							if ( ZoneLoad < 0.0 ) ZoneEz = this->ZoneADEffCooling( ZoneIndex );

							// Zone in heating mode
							if ( ZoneLoad > 0.0 ) ZoneEz = this->ZoneADEffHeating( ZoneIndex );
						}
						if ( ZoneEz <= 0.0 ) {
							//Enforce defaults
							ZoneEz = 1.0;
						}

						// Calc zone supply OA flow rate
						if ( this->SystemOAMethod == SOAM_VRP ) {
							// the VRP case
							ZoneOA = ZoneOABZ / ZoneEz;

						} else if ( this->SystemOAMethod == SOAM_ProportionalControlSchOcc || this->SystemOAMethod == SOAM_ProportionalControlDesOcc ) {
							// Check whether "Carbon Dioxide Control Availability Schedule" for ZoneControl:ContaminantController is specified
							if ( curZone.ZoneContamControllerSchedIndex > 0.0 ) {
								// Check the availability schedule value for ZoneControl:ContaminantController
								ZoneContamControllerSched = GetCurrentScheduleValue( curZone.ZoneContamControllerSchedIndex );
								if ( ZoneContamControllerSched > 0.0 ) {
									ZoneOAMin = ZoneOAArea / ZoneEz;
									ZoneOAMax = ( ZoneOAArea + ZoneOAPeople ) / ZoneEz;

									if ( ZoneOAPeople > 0.0 ) {
										if ( ZoneCO2GainFromPeople( ZoneNum ) > 0.0 ) {
											if ( curZone.ZoneMinCO2SchedIndex > 0.0 ) {
												// Take the schedule value of "Minimum Carbon Dioxide Concentration Schedule Name"
												// in the ZoneControl:ContaminantController
												ZoneMinCO2 = GetCurrentScheduleValue( curZone.ZoneMinCO2SchedIndex );
											} else {
												ZoneMinCO2 = OutdoorCO2;
											}

											// Calculate zone maximum target CO2 concentration in PPM
											if ( this->SystemOAMethod == SOAM_ProportionalControlDesOcc ) {
												ZoneMaxCO2 = OutdoorCO2 + ( CO2PeopleGeneration * curZone.Multiplier * curZone.ListMultiplier * 1.0e6 ) / ZoneOAMax;
											} else {
												ZoneMaxCO2 = OutdoorCO2 + ( ZoneCO2GainFromPeople( ZoneNum ) * curZone.Multiplier * curZone.ListMultiplier * 1.0e6 ) / ZoneOAMax;
											}

											if ( ZoneMaxCO2 <= ZoneMinCO2 ) {
												++this->CO2MaxMinLimitErrorCount;
												if ( this->SystemOAMethod == SOAM_ProportionalControlSchOcc ) {
													if ( this->CO2MaxMinLimitErrorCount < 2 ) {
														ShowSevereError( RoutineName + CurrentModuleObject + " = \"" + this->Name + "\"." );
														ShowContinueError( "For System Outdoor Air Method = ProportionalControlBasedonOccupancySchedule, maximum target CO2 concentration (" + RoundSigDigits( ZoneMaxCO2, 2 ) + "), is not greater than minimum target CO2 concentration (" + RoundSigDigits( ZoneMinCO2, 2 ) + ")." );
														ShowContinueError( "\"ProportionalControlBasedonOccupancySchedule\" will not be modeled. Default \"VentilationRateProcedure\" will be modeled. Simulation continues..." );
														ShowContinueErrorTimeStamp( "" );
													} else {
														ShowRecurringWarningErrorAtEnd( CurrentModuleObject + " = \"" + this->Name + "\", For System Outdoor Air Method = ProportionalControlBasedonOccupancySchedule, maximum target CO2 concentration is not greater than minimum target CO2 concentration. Error continues...", this->CO2MaxMinLimitErrorIndex );
													}
												}
												if ( this->SystemOAMethod == SOAM_ProportionalControlDesOcc ) {
													if ( this->CO2MaxMinLimitErrorCount < 2 ) {
														ShowSevereError( RoutineName + CurrentModuleObject + " = \"" + this->Name + "\"." );
														ShowContinueError( "For System Outdoor Air Method = ProportionalControlBasedonDesignOccupancy, maximum target CO2 concentration (" + RoundSigDigits( ZoneMaxCO2, 2 ) + "), is not greater than minimum target CO2 concentration (" + RoundSigDigits( ZoneMinCO2, 2 ) + ")." );
														ShowContinueError( "\"ProportionalControlBasedonDesignOccupancy\" will not be modeled. Default \"VentilationRateProcedure\" will be modeled. Simulation continues..." );
														ShowContinueErrorTimeStamp( "" );
													} else {
														ShowRecurringWarningErrorAtEnd( CurrentModuleObject + " = \"" + this->Name + "\", For System Outdoor Air Method = ProportionalControlBasedonDesignOccupancy, maximum target CO2 concentration is not greater than minimum target CO2 concentration. Error continues...", this->CO2MaxMinLimitErrorIndex );
													}
												}

												ZoneOA = ZoneOABZ / ZoneEz;
											} else {

												if ( ZoneAirCO2( ZoneNum ) <= ZoneMinCO2 ) {
													// Zone air CO2 concentration is less than minimum zone CO2 concentration, set the Zone OA flow rate to
													// minimum Zone OA flow rate when the zone is unoccupied
													ZoneOA = ZoneOAMin;
												} else if ( ZoneAirCO2( ZoneNum ) >= ZoneMaxCO2 ) {
													// Zone air CO2 concentration is greater than maximum zone CO2 concentration, set the Zone OA flow rate to
													// maximum Zone OA flow rate (i.e. ZoneOAArea + ZoneOAPeople)
													ZoneOA = ZoneOAMax;
												} else {
													// Zone air CO2 concentration is between maximum and minimum limits of zone CO2 concentration,
													// set Zone OA flow rate by proportionally adjusting between ZoneOAMin and ZoneOAMax
													ZoneOA = ZoneOAMin + ( ZoneOAMax - ZoneOAMin ) * ( ( ZoneAirCO2( ZoneNum ) - ZoneMinCO2 ) / ( ZoneMaxCO2 - ZoneMinCO2 ) );
												}
											}
										} else {
											if ( DisplayExtraWarnings ) {
												++this->CO2GainErrorCount;
												if ( this->SystemOAMethod == SOAM_ProportionalControlSchOcc ) {
													if ( this->CO2GainErrorCount < 2 ) {
														ShowSevereError( RoutineName + CurrentModuleObject + " = \"" + this->Name + "\"." );
														ShowContinueError( "For System Outdoor Air Method = ProportionalControlBasedonOccupancySchedule, CO2 generation from people is not greater than zero. Occurs in Zone =\"" + curZone.Name + "\". " );
														ShowContinueError( "\"ProportionalControlBasedonOccupancySchedule\" will not be modeled. Default \"VentilationRateProcedure\" will be modeled. Simulation continues..." );
														ShowContinueErrorTimeStamp( "" );
													} else {
														ShowRecurringWarningErrorAtEnd( CurrentModuleObject + " = \"" + this->Name + "\", For System Outdoor Air Method = ProportionalControlBasedonOccupancySchedule, CO2 generation from people is not greater than zero. Error continues...", this->CO2GainErrorIndex );
													}
												}
												if ( this->SystemOAMethod == SOAM_ProportionalControlDesOcc ) {
													if ( this->CO2GainErrorCount < 2 ) {
														ShowSevereError( RoutineName + CurrentModuleObject + " = \"" + this->Name + "\"." );
														ShowContinueError( "For System Outdoor Air Method = ProportionalControlBasedonDesignOccupancy, CO2 generation from people is not greater than zero. Occurs in Zone =\"" + curZone.Name + "\". " );
														ShowContinueError( "\"ProportionalControlBasedonDesignOccupancy\" will not be modeled. Default \"VentilationRateProcedure\" will be modeled. Simulation continues..." );
														ShowContinueErrorTimeStamp( "" );
													} else {
														ShowRecurringWarningErrorAtEnd( CurrentModuleObject + " = \"" + this->Name + "\", For System Outdoor Air Method = ProportionalControlBasedonDesignOccupancy, CO2 generation from people is not greater than zero. Error continues...", this->CO2GainErrorIndex );
													}
												}
											}
											ZoneOA = ZoneOABZ / ZoneEz;
										}
									} else {
										// ZoneOAPeople is less than or equal to zero
										ZoneOA = ZoneOABZ / ZoneEz;
									}
								} else {
									// ZoneControl:ContaminantController is scheduled off (not available)
									ZoneOA = ZoneOABZ / ZoneEz;
								}
							} else {
								// "Carbon Dioxide Control Availability Schedule" for ZoneControl:ContaminantController not found
								ZoneOA = ZoneOABZ / ZoneEz;
							}
							SysOA = SysOA + ZoneOA;
						}

						// Get the zone supply air flow rate
						ZoneSA = 0.0;
						ZonePA = 0.0;
						Ep = 1.0;
						if ( ZoneEquipConfigNum > 0 ) {
							for ( int InNodeIndex = 1; InNodeIndex <= curZoneEquipConfig.NumInletNodes; ++InNodeIndex ) {
								// Assume primary air is always stored at the AirDistUnitCool (cooling deck if dual duct)
								PriNode = curZoneEquipConfig.AirDistUnitCool( InNodeIndex ).InNode;
								if ( PriNode > 0 ) {
									NodeTemp = Node( PriNode ).Temp;
									NodeHumRat = Node( PriNode ).HumRat;
									MassFlowRate = Node( PriNode ).MassFlowRate;
								} else {
									MassFlowRate = 0.0;
								}
								// total primary air to terminal units of the zone
								if ( MassFlowRate > 0.0 ) ZonePA += MassFlowRate / PsyRhoAirFnPbTdbW( OutBaroPress, NodeTemp, NodeHumRat );

								// or InletNode = ZoneEquipConfig(ZoneEquipConfigNum)%AirDistUnitCool(InNodeIndex)%OutNode
								InletNode = curZoneEquipConfig.InletNode( InNodeIndex );
								if ( InletNode > 0 ) {
									NodeTemp = Node( InletNode ).Temp;
									NodeHumRat = Node( InletNode ).HumRat; // ZoneAirHumRat(ZoneNum)
									MassFlowRate = Node( InletNode ).MassFlowRate;
								} else {
									MassFlowRate = 0.0;
								}
								// total supply air to the zone
								if ( MassFlowRate > 0.0 ) ZoneSA += MassFlowRate / PsyRhoAirFnPbTdbW( OutBaroPress, NodeTemp, NodeHumRat );
							}

							// calc zone primary air fraction
							if ( ZoneSA > 0.0 ) Ep = ZonePA / ZoneSA;
							if ( Ep > 1.0 ) Ep = 1.0;
						}

						// Calc the zone OA fraction = Zone OA flow rate / Zone supply air flow rate
						if ( ZoneSA > 0.0 ) {
							ZoneOAFrac = ZoneOA / ZoneSA;
							// Zone OA fraction cannot be more than 1
							if ( ZoneOAFrac > 1.0 ) ZoneOAFrac = 1.0;
						} else {
							ZoneOAFrac = 0.0;
						}

						// added for TRACE - zone maximum OA fraction - calculate the adjustment factor for the TU/zone supply air flow
						// only for VRP system OA method
						curZoneSysEnergyDemand.SupplyAirAdjustFactor = 1.0;

						if ( this->SystemOAMethod == SOAM_VRP ) {
							if ( ZoneOAFrac > this->ZoneMaxOAFraction ) {
								if ( this->ZoneMaxOAFraction > 0.0 ) {
									curZoneSysEnergyDemand.SupplyAirAdjustFactor = ZoneOAFrac / this->ZoneMaxOAFraction;
								} else {
									curZoneSysEnergyDemand.SupplyAirAdjustFactor = 1.0;
								}

								// cap zone OA fraction at the maximum specified
								ZoneOAFrac = this->ZoneMaxOAFraction;
							}
						}

						// Zone air secondary recirculation fraction
						Er = this->ZoneSecondaryRecirculation( ZoneIndex );
						if ( Er > 0.0 ) {
							// multi-path ventilation system using VRP
							Fa = Ep + ( 1.0 - Ep ) * Er;
							Fb = Ep;
							Fc = 1.0 - ( 1.0 - ZoneEz ) * ( 1.0 - Er ) * ( 1.0 - Ep );

							// Calc zone ventilation efficiency
							if ( Fa > 0.0 ) {
								Evz = 1.0 + Xs * Fb / Fa - ZoneOAFrac * Ep * Fc / Fa;
							} else {
								Evz = 1.0;
							}
						} else {
							// single-path ventilation system
							Evz = 1.0 + Xs - ZoneOAFrac;
						}

						// calc system ventilation efficiency = Minimum of zone ventilation efficiency
						if ( Evz < 0.0 ) Evz = 0.0;
						if ( Evz < SysEv ) SysEv = Evz;

					} // zone loop

					// Calc the system supply OA flow rate counting the system ventilation efficiency
					if ( SysEv <= 0.0 ) SysEv = 1.0;

					// Calc system outdoor air requirement
					if ( this->SystemOAMethod == SOAM_ProportionalControlSchOcc || this->SystemOAMethod == SOAM_ProportionalControlDesOcc ) {
						SysOA = SysOA / SysEv;
					} else {
						SysOA = SysOAuc / SysEv;
					}
				}

				// Finally calc the system supply OA mass flow rate
				MechVentOutsideAirFlow = SysOA * StdRhoAir;
			}

		} else {
			MechVentOutsideAirFlow = 0.0;
		}

	}

	void
	OAControllerProps::CalcOAEconomizer(
		int const AirLoopNum,
		Real64 const OutAirMinFrac,
		Real64 & OASignal,
		bool & HighHumidityOperationFlag
		)
	{
		using DataZoneEnergyDemands::ZoneSysMoistureDemand;
		using General::SolveRegulaFalsi;
		using SetPointManager::GetCoilFreezingCheckFlag;

		static std::string const RoutineName("CalcOAEconomizer: ");
		static std::string const CurrentModuleObject(CurrentModuleObjects(CMO_OAController));
		int const MaxIte(500); // Maximum number of iterations
		Real64 const Acc(0.0001); // Accuracy of result
		bool AirLoopEconoLockout; // Economizer lockout flag
		bool AirLoopNightVent; // Night Ventilation flag for air loop
		bool EconomizerOperationFlag; // TRUE if OA economizer is active
		Real64 EconomizerAirFlowScheduleValue; // value of economizer operation schedule (push-button type control schedule)
		Real64 MaximumOAFracBySetPoint; // The maximum OA fraction due to freezing cooling coil check
		Real64 OutAirSignal; // Used to set OA mass flow rate
		static Array1D< Real64 > Par(4); // Par(1) = mixed air node number //Tuned Made static
										 // Par(2) = return air node number
										 // Par(3) = outside air node number
										 // Par(4) = mixed air mass flow rate
		int SolFla; // Flag of solver

		if ( AirLoopNum > 0 ) {
			// Check lockout with heating for any airloop - will lockout economizer even on airloops without a unitary system
			if ( this->Lockout == LockoutWithHeatingPossible ) {
				// For all system types (even ones that don't set AirLoopEconoLockout) lock out economizer if unfavorable for heating
				if ( AirLoopControlInfo( AirLoopNum ).CheckHeatRecoveryBypassStatus && AirLoopControlInfo( AirLoopNum ).OASysComponentsSimulated ) {

					if ( this->MixedAirTempAtMinOAFlow <= Node( this->MixNode ).TempSetPoint ) {
						AirLoopControlInfo( AirLoopNum ).EconomizerFlowLocked = true;
						// this->OAMassFlow = AirLoopFlow( AirLoopNum ).MinOutAir;
						// AirLoopFlow( AirLoopNum ).OAFrac = this->OAMassFlow / this->MixMassFlow;
						AirLoopControlInfo( AirLoopNum ).EconoLockout = true;
						EconomizerOperationFlag = false ;
					} else {
						AirLoopControlInfo( AirLoopNum ).EconomizerFlowLocked = false;
						this->HRHeatingCoilActive = 0;
					} 
					AirLoopControlInfo( AirLoopNum ).CheckHeatRecoveryBypassStatus = false;
				}
			}
		}

		if ( AirLoopNum > 0 ) {
			AirLoopEconoLockout = AirLoopControlInfo( AirLoopNum ).EconoLockout;
			AirLoopNightVent = AirLoopControlInfo( AirLoopNum ).NightVent;
		} else {
			AirLoopEconoLockout = false;
			AirLoopNightVent = false;
		}

		// Define an outside air signal
		if ( this->MixedAirSPMNum > 0 ) {
			this->CoolCoilFreezeCheck = GetCoilFreezingCheckFlag( this->MixedAirSPMNum );
		} else {
			this->CoolCoilFreezeCheck = false;
		}

		if ( std::abs( this->RetTemp - this->InletTemp ) > SmallTempDiff ) {
			OutAirSignal = ( this->RetTemp - this->MixSetTemp ) / ( this->RetTemp - this->InletTemp );
			if ( this->CoolCoilFreezeCheck ) {
				this->MaxOAFracBySetPoint = 0.0;
				MaximumOAFracBySetPoint = OutAirSignal;
			}
		} else {
			if ( this->RetTemp - this->MixSetTemp < 0.0 ) {
				if ( this->RetTemp - this->InletTemp >= 0.0 ) {
					OutAirSignal = -1.0;
				} else {
					OutAirSignal = 1.0;
				}
			} else {
				if ( this->RetTemp - this->InletTemp >= 0.0 ) {
					OutAirSignal = 1.0;
				} else {
					OutAirSignal = -1.0;
				}
			}
		}
		OutAirSignal = min( max( OutAirSignal, OutAirMinFrac ), 1.0 );

		// If no economizer, set to minimum and disable economizer and high humidity control
		if ( this->Econo == NoEconomizer ) {
			OutAirSignal = OutAirMinFrac;
			EconomizerOperationFlag = false;
			EconomizerAirFlowScheduleValue = 0.0;
			HighHumidityOperationFlag = false;
		} else if ( this->MaxOA < SmallAirVolFlow ) {
			OutAirSignal = OutAirMinFrac;
			EconomizerOperationFlag = false;
			EconomizerAirFlowScheduleValue = 0.0;
			HighHumidityOperationFlag = false;
		} else if ( AirLoopEconoLockout ) {
			OutAirSignal = OutAirMinFrac;
			EconomizerOperationFlag = false;
			EconomizerAirFlowScheduleValue = 0.0;
			HighHumidityOperationFlag = false;
		} else {
			//Changed by Amit for new implementation
			// Otherwise do the limit checks
			EconomizerOperationFlag = true;
			// Outside air temp greater than mix air setpoint
			if ( this->InletTemp > this->MixSetTemp ) {
				OutAirSignal = 1.0;
			}
			// Return air temp limit
			if ( this->Econo == DifferentialDryBulb ) {
				if ( this->InletTemp > this->RetTemp ) {
					OutAirSignal = OutAirMinFrac;
					EconomizerOperationFlag = false;
				}
				this->Checksetpoints( OutAirMinFrac, OutAirSignal, EconomizerOperationFlag );
			}
			// Return air enthalpy limit
			if ( this->Econo == DifferentialEnthalpy ) {
				if ( this->InletEnth > this->RetEnth ) {
					OutAirSignal = OutAirMinFrac;
					EconomizerOperationFlag = false;
				}
				this->Checksetpoints( OutAirMinFrac, OutAirSignal, EconomizerOperationFlag );
			}
			// Outside air temperature limit
			if ( this->Econo == FixedDryBulb ) {
				this->Checksetpoints( OutAirMinFrac, OutAirSignal, EconomizerOperationFlag );
			}
			//Fixed Enthalpy limit
			if ( this->Econo == FixedEnthalpy ) {
				this->Checksetpoints( OutAirMinFrac, OutAirSignal, EconomizerOperationFlag );
			}
			//FIXED DEW POINT AND DRY BULB TEMPERATURE STRATEGY
			if ( this->Econo == FixedDewPointAndDryBulb ) {
				this->Checksetpoints( OutAirMinFrac, OutAirSignal, EconomizerOperationFlag );
			}
			// ELECRONIC ENTHALPY, HUMIDITY RATIO CURVE
			if ( this->Econo == ElectronicEnthalpy ) {
				this->Checksetpoints( OutAirMinFrac, OutAirSignal, EconomizerOperationFlag );
			}
			// Differential dry bulb and enthalpy strategy
			if ( this->Econo == DifferentialDryBulbAndEnthalpy ) {
				if ( this->InletTemp > this->RetTemp ) {
					OutAirSignal = OutAirMinFrac;
					EconomizerOperationFlag = false;
				}
				if ( this->InletEnth > this->RetEnth ) {
					OutAirSignal = OutAirMinFrac;
					EconomizerOperationFlag = false;
				}
				this->Checksetpoints( OutAirMinFrac, OutAirSignal, EconomizerOperationFlag );
			}

			if ( this->TempLowLim != BlankNumeric && this->OATemp < this->TempLowLim ) {
				OutAirSignal = OutAirMinFrac;
				EconomizerOperationFlag = false;
			}
			// Increase air flow for humidity control
			// (HumidistatZoneNum is greater than 0 IF High Humidity Control Flag = YES, checked in GetInput)
			if ( this->HumidistatZoneNum > 0 ) {
				//   IF humidistat senses a moisture load check to see if modifying air flow is appropriate, otherwise disable modified air flow
				if ( ZoneSysMoistureDemand( this->HumidistatZoneNum ).TotalOutputRequired < 0.0 ) {
					//     IF OAController is not allowed to modify air flow during high outdoor humrat condition, then disable modified air flow
					//     if indoor humrat is less than or equal to outdoor humrat
					if ( ! this->ModifyDuringHighOAMoisture && Node( this->NodeNumofHumidistatZone ).HumRat <= this->OAHumRat ) {
						HighHumidityOperationFlag = false;
					} else {
						HighHumidityOperationFlag = true;
					}
				} else {
					HighHumidityOperationFlag = false;
				}
			} else {
				HighHumidityOperationFlag = false;
			}

			// Check time of day economizer schedule, enable economizer if schedule value > 0
			EconomizerAirFlowScheduleValue = 0.0;
			if ( this->EconomizerOASchedPtr > 0 ) {
				EconomizerAirFlowScheduleValue = GetCurrentScheduleValue( this->EconomizerOASchedPtr );
				if ( EconomizerAirFlowScheduleValue > 0.0 ) {
					EconomizerOperationFlag = true;
					OutAirSignal = 1.0;
				}
			}

		}

		// OutAirSignal will not give exactly the correct mixed air temperature (equal to the setpoint) since
		// it was calculated using the approximate method of sensible energy balance. Now we have to get the
		// accurate result using a full mass, enthalpy and moisture balance and iteration.
		if ( OutAirSignal > OutAirMinFrac && OutAirSignal < 1.0 && this->MixMassFlow > VerySmallMassFlow && this->ControllerType_Num == ControllerOutsideAir && ! AirLoopNightVent ) {
			Par( 1 ) = this->MixNode;
			Par( 2 ) = this->RetNode;
			Par( 3 ) = this->InletNode;
			Par( 4 ) = this->MixMassFlow;
			SolveRegulaFalsi( Acc, MaxIte, SolFla, OASignal, MixedAirControlTempResidual, OutAirMinFrac, 1.0, Par );
			if ( SolFla < 0 ) {
				OASignal = OutAirSignal;
			}
		} else {
			OASignal = OutAirSignal;
		}

		// Economizer choice "Bypass" forces minimum OA except when high humidity air flow is active based on indoor RH
		if ( this->EconBypass && EconomizerAirFlowScheduleValue == 0.0 ) {
			OASignal = OutAirMinFrac;
		}

		// Set outdoor air signal based on OA flow ratio if high humidity air flow is enabled
		if ( HighHumidityOperationFlag ) {
			if ( this->MixMassFlow > 0.0 ) {
				//   calculate the actual ratio of outside air to mixed air so the magnitude of OA during high humidity control is correct
				OASignal = max( OutAirMinFrac, ( this->HighRHOAFlowRatio * this->MaxOAMassFlowRate / this->MixMassFlow ) );
			}
		}

		if ( this->CoolCoilFreezeCheck ) {
			MaximumOAFracBySetPoint = min( max( MaximumOAFracBySetPoint, 0.0 ), 1.0 );
			this->MaxOAFracBySetPoint = MaximumOAFracBySetPoint;

			// This should not be messing with OutAirMinFrac, freeze protection should only limit economizer operation
			// if (MaximumOAFracBySetPoint < OutAirMinFrac) {
			// OutAirMinFrac = MaximumOAFracBySetPoint;
			//	if (AirLoopNum > 0) AirLoopFlow(AirLoopNum).MinOutAir = OutAirMinFrac * this->MixMassFlow;
			//}
			OASignal = max( min( MaximumOAFracBySetPoint, OASignal ), OutAirMinFrac );
		}

		if ( AirLoopNum > 0 ) {

			// Set the air loop economizer and high humidity control flags.
			AirLoopControlInfo( AirLoopNum ).EconoActive = EconomizerOperationFlag;
			AirLoopControlInfo( AirLoopNum ).HighHumCtrlActive = HighHumidityOperationFlag;
			if ( AirLoopControlInfo( AirLoopNum ).EconomizerFlowLocked ) {
				this->OAMassFlow = AirLoopFlow( AirLoopNum ).MinOutAir;
				AirLoopFlow( AirLoopNum ).OAFrac = this->OAMassFlow / this->MixMassFlow;
			}

			// Check heat exchanger bypass control
			AirLoopControlInfo( AirLoopNum ).HeatRecoveryBypass = false;
			this->HeatRecoveryBypassStatus = 0;
			if ( EconomizerOperationFlag ) {
				if ( this->HeatRecoveryBypassControlType == BypassWhenWithinEconomizerLimits ) {
					AirLoopControlInfo( AirLoopNum ).HeatRecoveryBypass = true;
					this->HeatRecoveryBypassStatus = 1;
				} else if ( this->HeatRecoveryBypassControlType == BypassWhenOAFlowGreaterThanMinimum ) {
					if ( OASignal > OutAirMinFrac ) {
						AirLoopControlInfo( AirLoopNum ).HeatRecoveryBypass = true;
						this->HeatRecoveryBypassStatus = 1;
					}
				}
			}
		}

		// Night ventilation control overrides economizer and high humidity control.
		if (AirLoopNightVent) OASignal = 1.0;

		// Set economizer report variable and status flag
		if ( this->Econo == NoEconomizer ) {
			// No economizer
			this->EconomizerStatus = 0;
			this->EconoActive = false;
		} else {
			// With economizer.
			if ( EconomizerOperationFlag ) {
				// Economizer is enabled
				this->EconomizerStatus = 1;
				this->EconoActive = true;
			} else {
				// Economizer is disabled
				this->EconomizerStatus = 0;
				this->EconoActive = false;
			}
		}

		// Set high humidity control report variable and status flag
		if ( HighHumidityOperationFlag ) {
			this->HighHumCtrlStatus = 1;
			this->HighHumCtrlActive = true;
		} else {
			this->HighHumCtrlStatus = 0;
			this->HighHumCtrlActive = false;
		}

	}
	void
	CalcOAMixer( int const OAMixerNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   Oct 1998
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE
		// Calculate the mixed air flow and conditions

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// Using/Aliasing
		using Psychrometrics::PsyTdbFnHW;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 RecircMassFlowRate;
		Real64 RecircPressure;
		Real64 RecircEnthalpy;
		Real64 RecircHumRat;

		// Define a recirculation mass flow rate
		RecircMassFlowRate = OAMixer( OAMixerNum ).RetMassFlowRate - OAMixer( OAMixerNum ).RelMassFlowRate;
		//In certain low flow conditions the return air mass flow rate can be below the outside air value established
		//  by the user.  This check will ensure that this condition does not result in unphysical air properties.
		if ( RecircMassFlowRate < 0.0 ) {
			RecircMassFlowRate = 0.0;
			OAMixer( OAMixerNum ).RelMassFlowRate = OAMixer( OAMixerNum ).RetMassFlowRate;
		}

		// Pass through the return air conditions to the relief air stream.  The return air is "split" to
		// the relief air and the recirculation air.
		OAMixer( OAMixerNum ).RelTemp = OAMixer( OAMixerNum ).RetTemp;
		OAMixer( OAMixerNum ).RelHumRat = OAMixer( OAMixerNum ).RetHumRat;
		OAMixer( OAMixerNum ).RelEnthalpy = OAMixer( OAMixerNum ).RetEnthalpy;
		OAMixer( OAMixerNum ).RelPressure = OAMixer( OAMixerNum ).RetPressure;
		RecircPressure = OAMixer( OAMixerNum ).RetPressure;
		RecircEnthalpy = OAMixer( OAMixerNum ).RetEnthalpy;
		RecircHumRat = OAMixer( OAMixerNum ).RetHumRat;
		// The recirculation air and the outside air are mixed to form the mixed air stream
		OAMixer( OAMixerNum ).MixMassFlowRate = OAMixer( OAMixerNum ).OAMassFlowRate + RecircMassFlowRate;
		// Check for zero flow
		if ( OAMixer( OAMixerNum ).MixMassFlowRate <= VerySmallMassFlow ) {
			OAMixer( OAMixerNum ).MixEnthalpy = OAMixer( OAMixerNum ).RetEnthalpy;
			OAMixer( OAMixerNum ).MixHumRat = OAMixer( OAMixerNum ).RetHumRat;
			OAMixer( OAMixerNum ).MixPressure = OAMixer( OAMixerNum ).RetPressure;
			OAMixer( OAMixerNum ).MixTemp = OAMixer( OAMixerNum ).RetTemp;
			return;
		}

		OAMixer( OAMixerNum ).MixEnthalpy = ( RecircMassFlowRate * RecircEnthalpy + OAMixer( OAMixerNum ).OAMassFlowRate * OAMixer( OAMixerNum ).OAEnthalpy ) / OAMixer( OAMixerNum ).MixMassFlowRate;
		OAMixer( OAMixerNum ).MixHumRat = ( RecircMassFlowRate * RecircHumRat + OAMixer( OAMixerNum ).OAMassFlowRate * OAMixer( OAMixerNum ).OAHumRat ) / OAMixer( OAMixerNum ).MixMassFlowRate;
		OAMixer( OAMixerNum ).MixPressure = ( RecircMassFlowRate * RecircPressure + OAMixer( OAMixerNum ).OAMassFlowRate * OAMixer( OAMixerNum ).OAPressure ) / OAMixer( OAMixerNum ).MixMassFlowRate;
		// Mixed air temperature is calculated from the mixed air enthalpy and humidity ratio.
		OAMixer( OAMixerNum ).MixTemp = PsyTdbFnHW( OAMixer( OAMixerNum ).MixEnthalpy, OAMixer( OAMixerNum ).MixHumRat );

	}

	// End of Calculation/Simulation Section of the Module
	//******************************************************************************

	// Beginning Sizing Section of the Module
	//******************************************************************************

	void
	OAControllerProps::SizeOAController()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   September 2001
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing OAController Components for which flow rates have not been
		// specified in the input.

		// METHODOLOGY EMPLOYED:
		// Obtains flow rates from the zone or system sizing arrays.

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::TrimSigDigits;
		using ReportSizingManager::ReportSizingOutput;
		using InputProcessor::SameString;
		using HVACHXAssistedCoolingCoil::GetHXDXCoilName;
		using HVACHXAssistedCoolingCoil::GetHXCoilType;
		using WaterCoils::SetCoilDesFlow;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const CurrentModuleObject( CurrentModuleObjects( CMO_OAController ) );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 OAFlowRatio; // Used for error checking
		std::string CompType; // Component type
		std::string CompName; // Component name
		std::string CoilName;
		std::string CoilType;
		int CompNum;
		bool ErrorsFound;

		ErrorsFound = false;
		if ( this->MaxOA == AutoSize ) {

			if ( CurSysNum > 0 ) {

				{ auto const SELECT_CASE_var( this->ControllerType_Num );

				if ( SELECT_CASE_var == ControllerOutsideAir ) {

					CheckSysSizing( CurrentModuleObject, this->Name );

					{ auto const SELECT_CASE_var1( CurDuctType );
					if ( SELECT_CASE_var1 == Main ) {
						this->MaxOA = FinalSysSizing( CurSysNum ).DesMainVolFlow;
					} else if ( SELECT_CASE_var1 == Cooling ) {
						this->MaxOA = FinalSysSizing( CurSysNum ).DesCoolVolFlow;
					} else if ( SELECT_CASE_var1 == Heating ) {
						this->MaxOA = FinalSysSizing( CurSysNum ).DesHeatVolFlow;
					} else if ( SELECT_CASE_var1 == Other ) {
						this->MaxOA = FinalSysSizing( CurSysNum ).DesMainVolFlow;
					} else {
						this->MaxOA = FinalSysSizing( CurSysNum ).DesMainVolFlow;
					}}

				} else if ( SELECT_CASE_var == ControllerStandAloneERV ) {

				} else {

				}}

			} else if ( CurZoneEqNum > 0 ) {

				{ auto const SELECT_CASE_var( this->ControllerType_Num );

				if ( SELECT_CASE_var == ControllerOutsideAir ) {

					CheckZoneSizing( CurrentModuleObject, this->Name );
					this->MaxOA = max( FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow, FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow );

				} else if ( SELECT_CASE_var == ControllerStandAloneERV ) {

				} else {

				}}

			}

			if ( this->MaxOA < SmallAirVolFlow ) {
				this->MaxOA = 0.0;
			}

			ReportSizingOutput( CurrentModuleObject, this->Name, "Maximum Outdoor Air Flow Rate [m3/s]", this->MaxOA );

		}

		if ( this->MinOA == AutoSize ) {

			if ( CurSysNum > 0 ) {

				CheckSysSizing( CurrentModuleObject, this->Name );
				if ( FinalSysSizing( CurSysNum ).DesOutAirVolFlow >= SmallAirVolFlow ) {
					this->MinOA = min( FinalSysSizing( CurSysNum ).DesOutAirVolFlow, this->MaxOA );
				} else {
					this->MinOA = 0.0;
				}

			}

			ReportSizingOutput( CurrentModuleObject, this->Name, "Minimum Outdoor Air Flow Rate [m3/s]", this->MinOA );

			if ( this->HumidistatZoneNum > 0 && this->FixedMin ) {
				if ( this->MaxOA > 0.0 ) {
					OAFlowRatio = this->MinOA / this->MaxOA;
					if ( this->HighRHOAFlowRatio < OAFlowRatio ) {
						ShowWarningError( CurrentModuleObject + " \"" + this->Name + "\"" );
						ShowContinueError( "... A fixed minimum outdoor air flow rate and high humidity control have been specified." );
						ShowContinueError( "... The High Humidity Outdoor Air Flow Ratio is less than the ratio of the outdoor air controllers minimum to maximum outside air flow rate." );
						ShowContinueError( "... Controller minimum flow rate = " + TrimSigDigits( this->MinOA, 4 ) + " m3/s." );
						ShowContinueError( "... Controller maximum flow rate = " + TrimSigDigits( this->MaxOA, 4 ) + " m3/s." );
						ShowContinueError( "... Controller minimum to maximum flow ratio = " + TrimSigDigits( OAFlowRatio, 4 ) + '.' );
						ShowContinueError( "... High humidity control flow ratio = " + TrimSigDigits( this->HighRHOAFlowRatio, 4 ) + '.' );
					}
				}
			}

		}
		// If there is an outside air system, loop over components in the OA system; pass the design air flow rate
		// to the coil components that don't have design air flow as an input.
		if ( CurOASysNum > 0 ) {
			for ( CompNum = 1; CompNum <= OutsideAirSys( CurOASysNum ).NumComponents; ++CompNum ) {
				CompType = OutsideAirSys( CurOASysNum ).ComponentType( CompNum );
				CompName = OutsideAirSys( CurOASysNum ).ComponentName( CompNum );
				if ( SameString( CompType, "COIL:COOLING:WATER:DETAILEDGEOMETRY" ) || SameString( CompType, "COIL:HEATING:WATER" ) || SameString( CompType, "COILSYSTEM:COOLING:WATER:HEATEXCHANGERASSISTED" ) ) {
					if ( SameString( CompType, "COILSYSTEM:COOLING:WATER:HEATEXCHANGERASSISTED" ) ) {
						CoilName = GetHXDXCoilName( CompType, CompName, ErrorsFound );
						CoilType = GetHXCoilType( CompType, CompName, ErrorsFound );
					} else {
						CoilName = CompName;
						CoilType = CompType;
					}
					SetCoilDesFlow( CoilType, CoilName, this->MinOA, ErrorsFound );
				}
			} // End of component loop
		}
		if ( ErrorsFound ) {
			ShowFatalError( "Preceding sizing errors cause program termination" );
		}

	}

	// End of Sizing Section of the Module
	//******************************************************************************

	// Beginning Update/Reporting Section of the Module
	//******************************************************************************

	void
	OAControllerProps::UpdateOAController()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   Oct 1998
		//       MODIFIED       Shirey/Raustad FSEC, June 2003
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE
		// Move the results of CalcOAController to the affected nodes

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// Using/Aliasing
		using namespace DataLoopNode;
		using DataGlobals::WarmupFlag;
		using DataGlobals::DoingSizing;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int OutAirNodeNum;
		int InletAirNodeNum;
		int RelAirNodeNum;
		int RetAirNodeNum;

		OutAirNodeNum = this->OANode;
		InletAirNodeNum = this->InletNode;
		RelAirNodeNum = this->RelNode;
		RetAirNodeNum = this->RetNode;

		if ( this->ControllerType_Num == ControllerOutsideAir ) {
			// The outside air controller sets the outside air flow rate and the relief air flow rate
			if ( !WarmupFlag && !DoingSizing && ( this->ManageDemand ) && ( this->OAMassFlow > this->DemandLimitFlowRate ) )
			{
				Node( OutAirNodeNum ).MassFlowRate = this->DemandLimitFlowRate;
				Node( InletAirNodeNum ).MassFlowRate = this->DemandLimitFlowRate;
				Node( OutAirNodeNum ).MassFlowRateMaxAvail = this->DemandLimitFlowRate;
			}
			else
			{
				Node( OutAirNodeNum ).MassFlowRate = this->OAMassFlow;
				Node( InletAirNodeNum ).MassFlowRate = this->OAMassFlow;
				Node( OutAirNodeNum ).MassFlowRateMaxAvail = this->OAMassFlow;
			}
			Node( RelAirNodeNum ).MassFlowRate = this->RelMassFlow;
		} else {
			// The ERV controller sets the supply and secondary inlet node information for the Stand Alone ERV
			// Currently, the Stand Alone ERV only has constant air flows (supply and exhaust), and these are
			// already set in HVACStandAloneERV.cc (subroutine init). Therefore, these flow assignments below are
			// currently redundant but may be useful in the future as mass flow rates can vary based on the controller signal.
			if ( !WarmupFlag && !DoingSizing && ( this->ManageDemand ) && ( this->OAMassFlow > this->DemandLimitFlowRate ) )
			{
				Node( OutAirNodeNum ).MassFlowRate = this->DemandLimitFlowRate;
				Node( OutAirNodeNum ).MassFlowRateMaxAvail = this->DemandLimitFlowRate;
			}
			else
			{
				Node( OutAirNodeNum ).MassFlowRate = this->OAMassFlow;
				Node( OutAirNodeNum ).MassFlowRateMaxAvail = this->OAMassFlow;
			}
			Node( RetAirNodeNum ).MassFlowRate = Node( this->RetNode ).MassFlowRate;
			Node( RetAirNodeNum ).MassFlowRateMaxAvail = Node( this->RetNode ).MassFlowRate;
		}

	}

	void
	UpdateOAMixer( int const OAMixerNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   Oct 1998
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE
		// Move the results of CalcOAMixer to the affected nodes

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// Using/Aliasing
		using namespace DataLoopNode;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int MixNode;
		int RelNode;
		int RetNode;

		MixNode = OAMixer( OAMixerNum ).MixNode;
		RelNode = OAMixer( OAMixerNum ).RelNode;
		RetNode = OAMixer( OAMixerNum ).RetNode;
		// Move mixed air data to the mixed air node
		Node( MixNode ).MassFlowRate = OAMixer( OAMixerNum ).MixMassFlowRate;
		Node( MixNode ).Temp = OAMixer( OAMixerNum ).MixTemp;
		Node( MixNode ).HumRat = OAMixer( OAMixerNum ).MixHumRat;
		Node( MixNode ).Enthalpy = OAMixer( OAMixerNum ).MixEnthalpy;
		Node( MixNode ).Press = OAMixer( OAMixerNum ).MixPressure;
		Node( MixNode ).MassFlowRateMaxAvail = OAMixer( OAMixerNum ).MixMassFlowRate;
		// Move the relief air data to the relief air node
		Node( RelNode ).MassFlowRate = OAMixer( OAMixerNum ).RelMassFlowRate;
		Node( RelNode ).Temp = OAMixer( OAMixerNum ).RelTemp;
		Node( RelNode ).HumRat = OAMixer( OAMixerNum ).RelHumRat;
		Node( RelNode ).Enthalpy = OAMixer( OAMixerNum ).RelEnthalpy;
		Node( RelNode ).Press = OAMixer( OAMixerNum ).RelPressure;
		Node( RelNode ).MassFlowRateMaxAvail = OAMixer( OAMixerNum ).RelMassFlowRate;

		if ( Contaminant.CO2Simulation ) {
			Node( RelNode ).CO2 = Node( RetNode ).CO2;
			if ( OAMixer( OAMixerNum ).MixMassFlowRate <= VerySmallMassFlow ) {
				Node( MixNode ).CO2 = Node( RetNode ).CO2;
			} else {
				Node( MixNode ).CO2 = ( ( Node( RetNode ).MassFlowRate - Node( RelNode ).MassFlowRate ) * Node( RetNode ).CO2 + OAMixer( OAMixerNum ).OAMassFlowRate * OutdoorCO2 ) / OAMixer( OAMixerNum ).MixMassFlowRate;
			}
		}

		if ( Contaminant.GenericContamSimulation ) {
			Node( RelNode ).GenContam = Node( RetNode ).GenContam;
			if ( OAMixer( OAMixerNum ).MixMassFlowRate <= VerySmallMassFlow ) {
				Node( MixNode ).GenContam = Node( RetNode ).GenContam;
			} else {
				Node( MixNode ).GenContam = ( ( Node( RetNode ).MassFlowRate - Node( RelNode ).MassFlowRate ) * Node( RetNode ).GenContam + OAMixer( OAMixerNum ).OAMassFlowRate * OutdoorGC ) / OAMixer( OAMixerNum ).MixMassFlowRate;
			}
		}

	}

	void
	ReportOAMixer( int const EP_UNUSED( OAMixerNum ) ) // unused1208
	{

		// SUBROUTINE ARGUMENT DEFINITIONS

	}

	// End of Sizing Section of the Module
	//******************************************************************************

	// Beginning Utility Section of the Module
	//******************************************************************************

	Real64
	MixedAirControlTempResidual(
		Real64 const OASignal, // Relative outside air flow rate (0 to 1)
		Array1< Real64 > const & Par // par(1) = mixed node number
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   April, 2003
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function TMixSetPoint - TMix.
		// Economizer damper position (OASignal) is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Using a mass and energy balance at the mixed air node, calculates the
		// mixed air temperature given the outside air damper position.

		// REFERENCES:

		// Using/Aliasing
		using Psychrometrics::PsyTdbFnHW;

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// par(2) = return node number
		// par(3) = outside air node number
		// par(4) = mixed air flow rate

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int MixNode; // mixed air node number
		int RetNode; // return air node number
		int OANode; // outside air node number
		Real64 MixMassFlowRate; // mixed air mass flow rare [kg/s]
		Real64 OAMassFlowRate; // outside air mass flow rate [kg/s]
		Real64 RecircMassFlowRate; // recirculated air mass flow rate [kg/s]
		Real64 RecircEnth; // recirculated air specific enthalpy [J/kg]
		Real64 RecircHumRat; // recirculated air humidity ratio [kg water/kg dry air]
		Real64 MixEnth; // mixed air specific enthalpy [J/kg]
		Real64 MixHumRat; // mixed air humidity ratio [kg water/kg dry air]
		Real64 MixTemp; // mixed air temperature [C]

		MixNode = int( Par( 1 ) );
		RetNode = int( Par( 2 ) );
		OANode = int( Par( 3 ) );
		MixMassFlowRate = Par( 4 );
		OAMassFlowRate = OASignal * MixMassFlowRate;
		RecircMassFlowRate = max( MixMassFlowRate - OAMassFlowRate, 0.0 );
		RecircEnth = Node( RetNode ).Enthalpy;
		RecircHumRat = Node( RetNode ).HumRat;
		MixEnth = ( RecircMassFlowRate * RecircEnth + OAMassFlowRate * Node( OANode ).Enthalpy ) / MixMassFlowRate;
		MixHumRat = ( RecircMassFlowRate * RecircHumRat + OAMassFlowRate * Node( OANode ).HumRat ) / MixMassFlowRate;
		MixTemp = PsyTdbFnHW( MixEnth, MixHumRat );
		Residuum = Node( MixNode ).TempSetPoint - MixTemp;

		return Residuum;
	}

	Array1D_int
	GetOAMixerNodeNumbers(
		std::string const & OAMixerName, // must match OA mixer names for the OA mixer type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   June 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the given OA mixer and returns the node numbers.  If
		// incorrect OA mixer name is given, ErrorsFound is returned as true
		// as zero.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Return value
		Array1D_int OANodeNumbers( 4 ); // return OA mixer nodes

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int WhichOAMixer;

		// Obtains and Allocates OA mixer related parameters from input file
		if ( GetOAMixerInputFlag ) { //First time subroutine has been entered
			GetOAMixerInputs();
			GetOAMixerInputFlag = false;
		}

		WhichOAMixer = FindItemInList( OAMixerName, OAMixer );
		if ( WhichOAMixer != 0 ) {
			OANodeNumbers( 1 ) = OAMixer( WhichOAMixer ).InletNode;
			OANodeNumbers( 2 ) = OAMixer( WhichOAMixer ).RelNode;
			OANodeNumbers( 3 ) = OAMixer( WhichOAMixer ).RetNode;
			OANodeNumbers( 4 ) = OAMixer( WhichOAMixer ).MixNode;
		}

		if ( WhichOAMixer == 0 ) {
			ShowSevereError( "GetOAMixerNodeNumbers: Could not find OA Mixer = \"" + OAMixerName + "\"" );
			ErrorsFound = true;
			OANodeNumbers = 0;
		}

		return OANodeNumbers;

	}

	int
	GetNumOAMixers()
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   October 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// After making sure get input is done, the number of OA mixers is returned.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		int NumberOfOAMixers;

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

		if ( GetOAMixerInputFlag ) { //First time subroutine has been entered
			GetOAMixerInputs();
			GetOAMixerInputFlag = false;
		}

		NumberOfOAMixers = NumOAMixers;

		return NumberOfOAMixers;

	}

	int
	GetNumOAControllers()
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   October 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// After making sure get input is done, the number of OA Controllers is returned.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		int NumberOfOAControllers;

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

		if ( AllocateOAControllersFlag ) {
			// Make sure OAControllers are allocated
			AllocateOAControllers();
		}

		NumberOfOAControllers = NumOAControllers;

		return NumberOfOAControllers;

	}

	int
	GetOAMixerReliefNodeNumber( int const OAMixerNum ) // Which Mixer
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   October 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// After making sure get input is done, the relief node number of indicated
		// mixer is returned.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::TrimSigDigits;

		// Return value
		int ReliefNodeNumber; // Relief Node Number

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

		if ( GetOAMixerInputFlag ) { //First time subroutine has been entered
			GetOAMixerInputs();
			GetOAMixerInputFlag = false;
		}

		if ( OAMixerNum > NumOAMixers ) {
			ShowFatalError( "GetOAMixerReliefNodeNumber: Requested Mixer #=" + TrimSigDigits( OAMixerNum ) + ", which is > number of OA Mixers=" + TrimSigDigits( NumOAMixers ) );
		}

		ReliefNodeNumber = OAMixer( OAMixerNum ).RelNode;

		return ReliefNodeNumber;

	}

	int
	GetOASysControllerListIndex( int const OASysNumber ) // OA Sys Number
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   April 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// After making sure get input is done, the Controller List index of the indicated
		// OA System is returned.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Return value
		int OASysControllerListNum; // OA Sys Controller List index

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

		if ( GetOASysInputFlag ) {
			GetOutsideAirSysInputs();
			GetOASysInputFlag = false;
		}

		OASysControllerListNum = OutsideAirSys( OASysNumber ).ControllerListNum;

		return OASysControllerListNum;

	}

	int
	GetOASysNumSimpControllers( int const OASysNumber ) // OA Sys Number
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   April 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// After making sure get input is done, the number of Controller:Simple objects in the
		// OA System is returned.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Return value
		int OASysNumSimpControllers; // number of Controller:Simple objects in this OA System

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

		if ( GetOASysInputFlag ) {
			GetOutsideAirSysInputs();
			GetOASysInputFlag = false;
		}

		OASysNumSimpControllers = OutsideAirSys( OASysNumber ).NumSimpleControllers;

		return OASysNumSimpControllers;

	}

	int
	GetOASysNumHeatingCoils( int const OASysNumber ) // OA Sys Number
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   May 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// After making sure get input is done, the number of heating coils in the
		// OA System is returned.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Return value
		int NumHeatingCoils; // number of heating coils in this OA System

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		std::string CompType;
		std::string CompName;
		bool Sim( false );
		bool FirstHVACIteration( false );
		bool OAHeatingCoil( false );
		bool OACoolingCoil( false );
		int CompNum;
		int AirLoopNum( 0 );
		bool OAHX( false );

		if ( GetOASysInputFlag ) {
			GetOutsideAirSysInputs();
			GetOASysInputFlag = false;
		}

		NumHeatingCoils = 0;
		for ( CompNum = 1; CompNum <= OutsideAirSys( OASysNumber ).NumComponents; ++CompNum ) {
			CompType = OutsideAirSys( OASysNumber ).ComponentType( CompNum );
			CompName = OutsideAirSys( OASysNumber ).ComponentName( CompNum );
			SimOAComponent( CompType, CompName, OutsideAirSys( OASysNumber ).ComponentType_Num( CompNum ), FirstHVACIteration, OutsideAirSys( OASysNumber ).ComponentIndex( CompNum ), AirLoopNum, Sim, OASysNumber, OAHeatingCoil, OACoolingCoil, OAHX );
			if ( OAHeatingCoil ) {
				++NumHeatingCoils;
			}
		}

		return NumHeatingCoils;

	}

	int
	GetOASysNumHXs( int const OASysNumber )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Fred Buhl, Rongpeng Zhang
		//       DATE WRITTEN   Oct. 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// After making sure get input is done, the number of heat recovery exchangers in the
		// OA System is returned.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Return value
		int NumHX; // number of heat exchangers in this OA System

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int CompNum;
		int CompNum_end;

		if( GetOASysInputFlag ) {
			GetOutsideAirSysInputs();
			GetOASysInputFlag = false;
		}

		NumHX = 0;

		auto const & componentType_Num = OutsideAirSys( OASysNumber ).ComponentType_Num;
		for( CompNum = 1, CompNum_end = OutsideAirSys( OASysNumber ).NumComponents; CompNum <= CompNum_end; ++CompNum ) {
			int const componentTypeNum = componentType_Num( CompNum );
			if ( HeatXchngr == componentTypeNum || Desiccant == componentTypeNum ) {
				++NumHX;
			}
		}

		return NumHX;

	}

	int
	GetOASysNumCoolingCoils( int const OASysNumber ) // OA Sys Number
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   May 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// After making sure get input is done, the number of cooling coils in the
		// OA System is returned.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Return value
		int NumCoolingCoils; // number of cooling coils in this OA System

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		std::string CompType;
		std::string CompName;
		bool Sim( false );
		bool FirstHVACIteration( false );
		bool OAHeatingCoil( false );
		bool OACoolingCoil( false );
		int CompNum;
		int AirLoopNum( 0 );
		bool OAHX( false );

		if ( GetOASysInputFlag ) {
			GetOutsideAirSysInputs();
			GetOASysInputFlag = false;
		}

		NumCoolingCoils = 0;
		for ( CompNum = 1; CompNum <= OutsideAirSys( OASysNumber ).NumComponents; ++CompNum ) {
			CompType = OutsideAirSys( OASysNumber ).ComponentType( CompNum );
			CompName = OutsideAirSys( OASysNumber ).ComponentName( CompNum );
			SimOAComponent( CompType, CompName, OutsideAirSys( OASysNumber ).ComponentType_Num( CompNum ), FirstHVACIteration, OutsideAirSys( OASysNumber ).ComponentIndex( CompNum ), AirLoopNum, Sim, OASysNumber, OAHeatingCoil, OACoolingCoil, OAHX );
			if ( OACoolingCoil ) {
				++NumCoolingCoils;
			}
		}

		return NumCoolingCoils;

	}

	int
	GetOASystemNumber( std::string const & OASysName ) // OA Sys Name
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   October 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// After making sure get input is done, the OA System number of indicated
		// OA System is returned.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Return value
		int OASysNumber; // OA Sys Number

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

		if ( GetOASysInputFlag ) {
			GetOutsideAirSysInputs();
			GetOASysInputFlag = false;
		}

		OASysNumber = FindItemInList( OASysName, OutsideAirSys );

		return OASysNumber;

	}

	int
	FindOAMixerMatchForOASystem( int const OASysNumber ) // Which OA System
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   October 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// After making sure get input is done, the matched mixer number is found.
		// Note -- only the first is looked at for an Outside Air System.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using InputProcessor::SameString;

		// Return value
		int OAMixerNumber; // Mixer Number

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int OACompNum;

		if ( GetOAMixerInputFlag ) {
			GetOAMixerInputs();
			GetOAMixerInputFlag = false;
		}

		OAMixerNumber = 0;
		if ( OASysNumber > 0 && OASysNumber <= NumOASystems ) {
			for ( OACompNum = 1; OACompNum <= OutsideAirSys( OASysNumber ).NumComponents; ++OACompNum ) {
				if ( SameString( OutsideAirSys( OASysNumber ).ComponentType( OACompNum ), "OUTDOORAIR:MIXER" ) ) {
					OAMixerNumber = FindItemInList( OutsideAirSys( OASysNumber ).ComponentName( OACompNum ), OAMixer );
					break;
				}
			}
		}

		return OAMixerNumber;

	}

	int
	GetOAMixerIndex( std::string const & OAMixerName ) // Which Mixer
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   December 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// After making sure get input is done, the mixer index of indicated
		// mixer is returned.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItem;

		// Return value
		int OAMixerIndex; // Mixer Index

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

		if ( GetOAMixerInputFlag ) {
			GetOAMixerInputs();
			GetOAMixerInputFlag = false;
		}

		OAMixerIndex = FindItem( OAMixerName, OAMixer );

		if ( OAMixerIndex == 0 ) {
			ShowSevereError( "GetOAMixerIndex: Could not find OutdoorAir:Mixer, Name=\"" + OAMixerName + "\"" );
		}

		return OAMixerIndex;

	}

	int
	GetOAMixerInletNodeNumber( int const OAMixerNumber ) // Which Mixer
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   October 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// After making sure get input is done, the mixer inlet node number of indicated
		// mixer is returned.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		int OAMixerInletNodeNumber; // Mixer Inlet Node Number

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

		if ( GetOAMixerInputFlag ) {
			GetOAMixerInputs();
			GetOAMixerInputFlag = false;
		}

		OAMixerInletNodeNumber = 0;
		if ( OAMixerNumber > 0 && OAMixerNumber <= NumOAMixers ) {
			OAMixerInletNodeNumber = OAMixer( OAMixerNumber ).InletNode;
		}

		return OAMixerInletNodeNumber;

	}

	int
	GetOAMixerReturnNodeNumber( int const OAMixerNumber ) // Which Mixer
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   December 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// After making sure get input is done, the mixer return node number of indicated
		// mixer is returned.

		// METHODOLOGY EMPLOYED:
		// followed Linda Lawrie's GetOAMixerInletNodeNumber

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		int OAMixerReturnNodeNumber; // Mixer Inlet Node Number

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

		if ( GetOAMixerInputFlag ) {
			GetOAMixerInputs();
			GetOAMixerInputFlag = false;
		}

		OAMixerReturnNodeNumber = 0;
		if ( OAMixerNumber > 0 && OAMixerNumber <= NumOAMixers ) {
			OAMixerReturnNodeNumber = OAMixer( OAMixerNumber ).RetNode;
		}

		return OAMixerReturnNodeNumber;

	}

	int
	GetOAMixerMixedNodeNumber( int const OAMixerNumber ) // Which Mixer
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   December 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// After making sure get input is done, the mixer mixed air node number of indicated
		// mixer is returned.

		// METHODOLOGY EMPLOYED:
		// followed Linda Lawrie's GetOAMixerInletNodeNumber

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		int OAMixerMixedNodeNumber; // Mixer Inlet Node Number

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

		if ( GetOAMixerInputFlag ) {
			GetOAMixerInputs();
			GetOAMixerInputFlag = false;
		}

		OAMixerMixedNodeNumber = 0;
		if ( OAMixerNumber > 0 && OAMixerNumber <= NumOAMixers ) {
			OAMixerMixedNodeNumber = OAMixer( OAMixerNumber ).MixNode;
		}

		return OAMixerMixedNodeNumber;

	}

	bool
	CheckForControllerWaterCoil(
		std::string const & ControllerType, // should be passed in as UPPERCASE
		std::string const & ControllerName // should be passed in as UPPERCASE
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   May 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This routine checks the controller list for existance of the
		// reference coil.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::SameString;

		// Return value
		bool OnControllerList; // true if found on controller list

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int Num;
		int CompNum;

		if ( GetOASysInputFlag ) {
			GetOutsideAirSysInputs();
			GetOASysInputFlag = false;
		}

		OnControllerList = false;

		for ( Num = 1; Num <= NumControllerLists; ++Num ) {
			for ( CompNum = 1; CompNum <= ControllerLists( Num ).NumControllers; ++CompNum ) {

				if ( ! SameString( ControllerLists( Num ).ControllerType( CompNum ), ControllerType ) ) continue;
				if ( ! SameString( ControllerLists( Num ).ControllerName( CompNum ), ControllerName ) ) continue;
				OnControllerList = true;
				break;
			}
		}

		return OnControllerList;

	}

	void
	CheckControllerLists( bool & ErrFound )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   May 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine checks for a "dangling" controller list (AirLoopHVAC:ControllerList).
		// It must be either found on a AirLoopHVAC or AirLoopHVAC:OutdoorAirSystem.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CheckControllerLists" );
		static std::string const CurrentModuleObject( "AirLoopHVAC:ControllerList" );
		static std::string const AirLoopObject( "AirLoopHVAC" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumAlphas;
		int NumNumbers;
		int NumControllers;
		int NumAirLoop;
		std::string ControllerListName;
		int Item;
		int IOStat;
		int Found;
		int Count;
		int Loop;
		std::string AirLoopName;

		if ( GetOASysInputFlag ) {
			GetOutsideAirSysInputs();
			GetOASysInputFlag = false;
		}

		NumControllers = GetNumObjectsFound( CurrentModuleObject );
		NumAirLoop = GetNumObjectsFound( AirLoopObject );
		AirLoopName = "";

		for ( Item = 1; Item <= NumControllers; ++Item ) {

			GetObjectItem( CurrentModuleObject, Item, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStat );
			ControllerListName = cAlphaArgs( 1 );
			Count = 0;

			// Check AirLoopHVAC -- brute force, get each AirLoopHVAC

			for ( Loop = 1; Loop <= NumAirLoop; ++Loop ) {
				GetObjectItem( AirLoopObject, Loop, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStat );
				if ( cAlphaArgs( 2 ) != ControllerListName ) continue;
				++Count;
				if ( Count == 1 ) AirLoopName = cAlphaArgs( 1 );
			}

			//  Now check AirLoopHVAC and AirLoopHVAC:OutdoorAirSystem
			Found = 0;
			if ( NumOASystems > 0 ) {
				Found = FindItemInList( ControllerListName, OutsideAirSys, &OutsideAirSysProps::ControllerListName );
				if ( Found > 0 ) ++Count;
			}

			if ( Count == 0 ) {
				ShowSevereError( CurrentModuleObject + "=\"" + ControllerListName + "\" is not referenced on a AirLoopHVAC or AirLoopHVAC:OutdoorAirSystem object." );
				ErrFound = true;
			} else if ( Count > 1 ) {
				ShowSevereError( CurrentModuleObject + "=\"" + ControllerListName + "\" has too many references on AirLoopHVAC or AirLoopHVAC:OutdoorAirSystem objects." );
				if ( Found > 0 ) {
					ShowContinueError( "...AirLoopHVAC:OutdoorAirSystem=\"" + OutsideAirSys( Found ).Name + "\"." );
				}
				ShowContinueError( "...also on AirLoopHVAC=\"" + AirLoopName + "\"." );
				ErrFound = true;
			}
		}

	}

	void
	CheckOAControllerName(
		std::string const & OAControllerName, // proposed name
		int const NumCurrentOAControllers, // Count on number of controllers
		bool & IsNotOK, // Pass through to VerifyName
		bool & IsBlank, // Pass through to VerifyName
		std::string const & SourceID // Pass through to VerifyName
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   October 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// When OA Controller data is gotten from other routines, must check to make sure
		// new name doesn't duplicate.  (Essentially a pass through to call Verify Name)
		// Currently, this is only called from HVACStandAlongERV::GetStandaloneERV()

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::VerifyName;

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

		if ( AllocateOAControllersFlag ) {
			// Make sure OAControllers are allocated
			AllocateOAControllers();
		}

		VerifyName( OAControllerName, OAController, NumCurrentOAControllers, IsNotOK, IsBlank, SourceID );

	}

	void
	OAControllerProps::Checksetpoints(
		Real64 const OutAirMinFrac, // Local variable used to calculate min OA fraction
		Real64 & OutAirSignal, // Used to set OA mass flow rate
		bool & EconomizerOperationFlag // logical used to show economizer status
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Amit bhansali
		//       DATE WRITTEN   August 2008?
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine checks the setpoints of the upper limits of temperatures, limit enthalpy
		// Limit dew point, Enthalpy curve

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using Psychrometrics::PsyTdpFnWPb;
		using CurveManager::CurveValue;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 OADPTemp; // Dew Point Temperature calculation

		if ( this->TempLim != BlankNumeric && this->OATemp > this->TempLim ) {
			OutAirSignal = OutAirMinFrac;
			EconomizerOperationFlag = false;
		}
		// Outside air enthalpy limit
		if ( this->EnthLim != BlankNumeric && this->OAEnth > this->EnthLim ) {
			OutAirSignal = OutAirMinFrac;
			EconomizerOperationFlag = false;
		}

		if ( this->DPTempLim != BlankNumeric ) {
			OADPTemp = PsyTdpFnWPb( this->OAHumRat, this->OAPress );
			if ( OADPTemp > this->DPTempLim ) {
				OutAirSignal = OutAirMinFrac;
				EconomizerOperationFlag = false;
			}
		}

		if ( this->EnthalpyCurvePtr > 0 ) {
			if ( this->OAHumRat > CurveValue( this->EnthalpyCurvePtr, this->OATemp ) ) {
				OutAirSignal = OutAirMinFrac;
				EconomizerOperationFlag = false;
			}
		}

	}

	int
	GetNumOASystems()
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   November 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Get Number of OA Systems, After making sure get input is done

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		int NumberOfOASystems; // Number of OA Systems

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

		if ( GetOASysInputFlag ) {
			GetOutsideAirSysInputs();
			GetOASysInputFlag = false;
		}

		NumberOfOASystems = NumOASystems;

		return NumberOfOASystems;
	}

	int
	GetOACompListNumber( int const OASysNum ) // OA Sys Number
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Heejin Cho
		//       DATE WRITTEN   November 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// After making sure get input is done, the OA System number of indicated
		// OA System is returned.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Return value
		int NumOACompList; // OA Comp Number

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

		if ( GetOASysInputFlag ) {
			GetOutsideAirSysInputs();
			GetOASysInputFlag = false;
		}

		NumOACompList = OutsideAirSys( OASysNum ).NumComponents;

		return NumOACompList;
	}

	std::string
	GetOACompName(
		int const OASysNum, // OA Sys Number
		int const InListNum // In-list Number
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Heejin Cho
		//       DATE WRITTEN   November 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// After making sure get input is done, the number of heating coils in the
		// OA System is returned.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Return value
		std::string OACompName;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:
		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:

		if ( GetOASysInputFlag ) {
			GetOutsideAirSysInputs();
			GetOASysInputFlag = false;
		}

		OACompName = OutsideAirSys( OASysNum ).ComponentName( InListNum );

		return OACompName;
	}

	std::string
	GetOACompType(
		int const OASysNum, // OA Sys Number
		int const InListNum // In-list Number
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Heejin Cho
		//       DATE WRITTEN   November 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// After making sure get input is done, the number of heating coils in the
		// OA System is returned.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Return value
		std::string OACompType;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:
		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:

		if ( GetOASysInputFlag ) {
			GetOutsideAirSysInputs();
			GetOASysInputFlag = false;
		}

		OACompType = OutsideAirSys( OASysNum ).ComponentType( InListNum );

		return OACompType;
	}

	int
	GetOACompTypeNum(
		int const OASysNum, // OA Sys Number
		int const InListNum // In-list Number
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Heejin Cho
		//       DATE WRITTEN   November 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// After making sure get input is done, the number of heating coils in the
		// OA System is returned.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Return value
		int OACompTypeNum;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:
		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:

		if ( GetOASysInputFlag ) {
			GetOutsideAirSysInputs();
			GetOASysInputFlag = false;
		}

		OACompTypeNum = OutsideAirSys( OASysNum ).ComponentType_Num( InListNum );

		return OACompTypeNum;
	}

	// End of Utility Section of the Module
	//******************************************************************************

} // MixedAir

} // EnergyPlus
