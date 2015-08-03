// C++ Headers
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>

// EnergyPlus Headers
#include <PlantLoadProfile.hh>
#include <BranchNodeConnections.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <EMSManager.hh>
#include <FluidProperties.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <PlantUtilities.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace PlantLoadProfile {

	// MODULE INFORMATION:
	//       AUTHOR         Peter Graham Ellis
	//       DATE WRITTEN   January 2004
	//       MODIFIED       Brent Griffith, plant rewrite, general fluid types
	//                      allow flow requests with out load requests
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This module simulates a scheduled load profile on the demand side of the plant loop.

	// METHODOLOGY EMPLOYED:
	// The plant load profile object provides a scheduled load on the plant loop.  Unlike most plant equipment
	// on the demand side, i.e. zone equipment, this object does not have a zone associated with it.
	// For this reason the plant load profile can only be called for simulation by the non-zone equipment
	// manager (see NonZoneEquipmentManager.cc).

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::BeginEnvrnFlag;
	using DataGlobals::InitConvTemp;
	using DataPlant::PlantLoop;
	using DataPlant::TypeOf_PlantLoadProfile;
	using DataPlant::ScanPlantLoopsForObject;
	using PlantUtilities::SetComponentFlowRate;
	using PlantUtilities::InitComponentNodes;

	// Data
	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE TYPE DECLARATIONS:

	// MODULE VARIABLE DECLARATIONS:
	int NumOfPlantProfile;

	// SUBROUTINE SPECIFICATIONS:

	// Object Data
	Array1D< PlantProfileData > PlantProfile;

	// MODULE SUBROUTINES:

	// Functions

	void
	SimulatePlantProfile(
		std::string const & EP_UNUSED( EquipTypeName ), // description of model (not used until different types of profiles)
		std::string const & EquipName, // the user-defined name
		int const EP_UNUSED( EquipTypeNum ), // the plant parameter ID for equipment model
		int & ProfileNum, // the index for specific load profile
		bool const EP_UNUSED( FirstHVACIteration ),
		bool const InitLoopEquip // flag indicating if called in special initialization mode.
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   January 2004
		//       MODIFIED       Brent Griffith, generalize fluid cp
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Simulates the plant load profile object.

		// METHODOLOGY EMPLOYED:
		// This is a very simple simulation.  InitPlantProfile does the work of getting the scheduled load and flow rate.
		// Flow is requested and the actual available flow is set.  The outlet temperature is calculated.

		// USE STATEMENTS:

		// Using/Aliasing
		using FluidProperties::GetSpecificHeatGlycol;
		using InputProcessor::FindItemInList;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static std::string const RoutineName( "SimulatePlantProfile" );
		Real64 DeltaTemp;
		static bool GetInput( true );
		Real64 Cp; // local fluid specific heat

		// FLOW:
		if ( GetInput ) {
			GetPlantProfileInput();
			GetInput = false;
		}

		if ( InitLoopEquip ) {
			ProfileNum = FindItemInList( EquipName, PlantProfile.Name(), NumOfPlantProfile );
			if ( ProfileNum != 0 ) {
				InitPlantProfile( ProfileNum );
				return;
			}

		}

		if ( ProfileNum != 0 ) {

			InitPlantProfile( ProfileNum );

			if ( PlantProfile( ProfileNum ).MassFlowRate > 0.0 ) {

				Cp = GetSpecificHeatGlycol( PlantLoop( PlantProfile( ProfileNum ).WLoopNum ).FluidName, PlantProfile( ProfileNum ).InletTemp, PlantLoop( PlantProfile( ProfileNum ).WLoopNum ).FluidIndex, RoutineName );

				DeltaTemp = PlantProfile( ProfileNum ).Power / ( PlantProfile( ProfileNum ).MassFlowRate * Cp );
			} else {
				PlantProfile( ProfileNum ).Power = 0.0;
				DeltaTemp = 0.0;
			}

			PlantProfile( ProfileNum ).OutletTemp = PlantProfile( ProfileNum ).InletTemp - DeltaTemp;

			UpdatePlantProfile( ProfileNum );
			ReportPlantProfile( ProfileNum );

		} else {
			ShowFatalError( "SimulatePlantProfile: plant load profile not found =" + EquipName );

		}

	}

	void
	GetPlantProfileInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   January 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Gets the plant load profile input from the input file and sets up the objects.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using ScheduleManager::GetScheduleIndex;
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::TestCompSet;
		using DataGlobals::AnyEnergyManagementSystemInModel;
		using namespace DataLoopNode;
		using namespace DataIPShortCuts; // Data for field names, blank numerics

		// Locals
		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool ErrorsFound( false ); // Set to true if errors in input, fatal at end of routine
		int IOStatus; // Used in GetObjectItem
		bool IsBlank; // TRUE if the name is blank
		bool IsNotOK; // TRUE if there was a problem with a list name
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		int ProfileNum; // PLANT LOAD PROFILE (PlantProfile) object number
		//  CHARACTER(len=MaxNameLength)   :: FoundBranchName
		//  INTEGER                        :: BranchControlType

		// FLOW:
		cCurrentModuleObject = "LoadProfile:Plant";
		NumOfPlantProfile = GetNumObjectsFound( cCurrentModuleObject );

		if ( NumOfPlantProfile > 0 ) {
			PlantProfile.allocate( NumOfPlantProfile );

			for ( ProfileNum = 1; ProfileNum <= NumOfPlantProfile; ++ProfileNum ) {
				GetObjectItem( cCurrentModuleObject, ProfileNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, _, cAlphaFieldNames, cNumericFieldNames );

				// PlantProfile name
				IsNotOK = false;
				IsBlank = false;
				VerifyName( cAlphaArgs( 1 ), PlantProfile.Name(), ProfileNum - 1, IsNotOK, IsBlank, cCurrentModuleObject );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
				}
				PlantProfile( ProfileNum ).Name = cAlphaArgs( 1 );
				PlantProfile( ProfileNum ).TypeNum = TypeOf_PlantLoadProfile; // parameter assigned in DataPlant !DSU

				PlantProfile( ProfileNum ).InletNode = GetOnlySingleNode( cAlphaArgs( 2 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
				PlantProfile( ProfileNum ).OutletNode = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

				PlantProfile( ProfileNum ).LoadSchedule = GetScheduleIndex( cAlphaArgs( 4 ) );

				if ( PlantProfile( ProfileNum ).LoadSchedule == 0 ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"  The Schedule for " + cAlphaFieldNames( 4 ) + " called " + cAlphaArgs( 4 ) + " was not found." );
					ErrorsFound = true;
				}

				PlantProfile( ProfileNum ).PeakVolFlowRate = rNumericArgs( 1 );

				PlantProfile( ProfileNum ).FlowRateFracSchedule = GetScheduleIndex( cAlphaArgs( 5 ) );

				if ( PlantProfile( ProfileNum ).FlowRateFracSchedule == 0 ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"  The Schedule for " + cAlphaFieldNames( 5 ) + " called " + cAlphaArgs( 5 ) + " was not found." );

					ErrorsFound = true;
				}

				// Check plant connections
				TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 2 ), cAlphaArgs( 3 ), cCurrentModuleObject + " Nodes" );

				// Setup report variables
				SetupOutputVariable( "Plant Load Profile Mass Flow Rate [kg/s]", PlantProfile( ProfileNum ).MassFlowRate, "System", "Average", PlantProfile( ProfileNum ).Name );

				SetupOutputVariable( "Plant Load Profile Heat Transfer Rate [W]", PlantProfile( ProfileNum ).Power, "System", "Average", PlantProfile( ProfileNum ).Name );

				SetupOutputVariable( "Plant Load Profile Heat Transfer Energy [J]", PlantProfile( ProfileNum ).Energy, "System", "Sum", PlantProfile( ProfileNum ).Name, _, "ENERGYTRANSFER", "Heating", _, "Plant" ); // is EndUseKey right?

				SetupOutputVariable( "Plant Load Profile Heating Energy [J]", PlantProfile( ProfileNum ).HeatingEnergy, "System", "Sum", PlantProfile( ProfileNum ).Name, _, "PLANTLOOPHEATINGDEMAND", "Heating", _, "Plant" );

				SetupOutputVariable( "Plant Load Profile Cooling Energy [J]", PlantProfile( ProfileNum ).CoolingEnergy, "System", "Sum", PlantProfile( ProfileNum ).Name, _, "PLANTLOOPCOOLINGDEMAND", "Cooling", _, "Plant" );

				if ( AnyEnergyManagementSystemInModel ) {
					SetupEMSActuator( "Plant Load Profile", PlantProfile( ProfileNum ).Name, "Mass Flow Rate", "[kg/s]", PlantProfile( ProfileNum ).EMSOverrideMassFlow, PlantProfile( ProfileNum ).EMSMassFlowValue );
					SetupEMSActuator( "Plant Load Profile", PlantProfile( ProfileNum ).Name, "Power", "[W]", PlantProfile( ProfileNum ).EMSOverridePower, PlantProfile( ProfileNum ).EMSPowerValue );
				}

				if ( ErrorsFound ) ShowFatalError( "Errors in " + cCurrentModuleObject + " input." );

			} // ProfileNum
		}

	}

	void
	InitPlantProfile( int const ProfileNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   January 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Initializes the plant load profile object during the plant simulation.

		// METHODOLOGY EMPLOYED:
		// Inlet and outlet nodes are initialized.  The scheduled load and flow rate is obtained, flow is requested, and the
		// actual available flow is set.

		// Using/Aliasing
		using DataGlobals::SysSizingCalc;
		using PlantUtilities::RegisterPlantCompDesignFlow;
		using DataLoopNode::Node;
		using ScheduleManager::GetCurrentScheduleValue;
		using ScheduleManager::GetScheduleMaxValue;
		using FluidProperties::GetDensityGlycol;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static std::string const RoutineName( "InitPlantProfile" );
		int InletNode;
		int OutletNode;
		Real64 MaxFlowMultiplier;
		Real64 FluidDensityInit;
		bool errFlag;

		// FLOW:

		// Do the one time initializations
		if ( PlantProfile( ProfileNum ).SetLoopIndexFlag ) {
			if ( allocated( PlantLoop ) ) {
				errFlag = false;
				ScanPlantLoopsForObject( PlantProfile( ProfileNum ).Name, PlantProfile( ProfileNum ).TypeNum, PlantProfile( ProfileNum ).WLoopNum, PlantProfile( ProfileNum ).WLoopSideNum, PlantProfile( ProfileNum ).WLoopBranchNum, PlantProfile( ProfileNum ).WLoopCompNum, _, _, _, _, _, errFlag );
				if ( errFlag ) {
					ShowFatalError( "InitPlantProfile: Program terminated for previous conditions." );
				}

				PlantProfile( ProfileNum ).SetLoopIndexFlag = false;
			}
		}

		// FLOW:
		InletNode = PlantProfile( ProfileNum ).InletNode;
		OutletNode = PlantProfile( ProfileNum ).OutletNode;

		if ( ! SysSizingCalc && PlantProfile( ProfileNum ).InitSizing ) {
			RegisterPlantCompDesignFlow( InletNode, PlantProfile( ProfileNum ).PeakVolFlowRate );
			PlantProfile( ProfileNum ).InitSizing = false;
		}

		if ( BeginEnvrnFlag && PlantProfile( ProfileNum ).Init ) {
			// Clear node initial conditions
			//DSU? can we centralize these temperature inits
			//    Node(InletNode)%Temp = 0.0
			Node( OutletNode ).Temp = 0.0;

			FluidDensityInit = GetDensityGlycol( PlantLoop( PlantProfile( ProfileNum ).WLoopNum ).FluidName, InitConvTemp, PlantLoop( PlantProfile( ProfileNum ).WLoopNum ).FluidIndex, RoutineName );

			MaxFlowMultiplier = GetScheduleMaxValue( PlantProfile( ProfileNum ).FlowRateFracSchedule );

			InitComponentNodes( 0.0, PlantProfile( ProfileNum ).PeakVolFlowRate * FluidDensityInit * MaxFlowMultiplier, InletNode, OutletNode, PlantProfile( ProfileNum ).WLoopNum, PlantProfile( ProfileNum ).WLoopSideNum, PlantProfile( ProfileNum ).WLoopBranchNum, PlantProfile( ProfileNum ).WLoopCompNum );

			PlantProfile( ProfileNum ).EMSOverrideMassFlow = false;
			PlantProfile( ProfileNum ).EMSMassFlowValue = 0.0;
			PlantProfile( ProfileNum ).EMSOverridePower = false;
			PlantProfile( ProfileNum ).EMSPowerValue = 0.0;
			PlantProfile( ProfileNum ).Init = false;

		}

		if ( ! BeginEnvrnFlag ) PlantProfile( ProfileNum ).Init = true;

		PlantProfile( ProfileNum ).InletTemp = Node( InletNode ).Temp;
		PlantProfile( ProfileNum ).Power = GetCurrentScheduleValue( PlantProfile( ProfileNum ).LoadSchedule );

		if ( PlantProfile( ProfileNum ).EMSOverridePower ) PlantProfile( ProfileNum ).Power = PlantProfile( ProfileNum ).EMSPowerValue;

		FluidDensityInit = GetDensityGlycol( PlantLoop( PlantProfile( ProfileNum ).WLoopNum ).FluidName, PlantProfile( ProfileNum ).InletTemp, PlantLoop( PlantProfile( ProfileNum ).WLoopNum ).FluidIndex, RoutineName );

		// Get the scheduled mass flow rate
		PlantProfile( ProfileNum ).VolFlowRate = PlantProfile( ProfileNum ).PeakVolFlowRate * GetCurrentScheduleValue( PlantProfile( ProfileNum ).FlowRateFracSchedule );

		PlantProfile( ProfileNum ).MassFlowRate = PlantProfile( ProfileNum ).VolFlowRate * FluidDensityInit;

		if ( PlantProfile( ProfileNum ).EMSOverrideMassFlow ) PlantProfile( ProfileNum ).MassFlowRate = PlantProfile( ProfileNum ).EMSMassFlowValue;

		// Request the mass flow rate from the plant component flow utility routine
		SetComponentFlowRate( PlantProfile( ProfileNum ).MassFlowRate, InletNode, OutletNode, PlantProfile( ProfileNum ).WLoopNum, PlantProfile( ProfileNum ).WLoopSideNum, PlantProfile( ProfileNum ).WLoopBranchNum, PlantProfile( ProfileNum ).WLoopCompNum );

		PlantProfile( ProfileNum ).VolFlowRate = PlantProfile( ProfileNum ).MassFlowRate / FluidDensityInit;

	}

	void
	UpdatePlantProfile( int const ProfileNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   January 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Updates the node variables with local variables.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// Using/Aliasing
		using DataLoopNode::Node;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int OutletNode;

		// FLOW:

		OutletNode = PlantProfile( ProfileNum ).OutletNode;

		// Set outlet node variables that are possibly changed
		Node( OutletNode ).Temp = PlantProfile( ProfileNum ).OutletTemp;

		//DSU? enthalpy? quality etc? central routine? given inlet node, fluid type, delta T, properly fill all node vars?

	}

	void
	ReportPlantProfile( int const ProfileNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   January 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates report variables.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// Using/Aliasing
		using DataGlobals::SecInHour;
		using DataHVACGlobals::TimeStepSys;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// FLOW:
		PlantProfile( ProfileNum ).Energy = PlantProfile( ProfileNum ).Power * TimeStepSys * SecInHour;

		if ( PlantProfile( ProfileNum ).Energy >= 0.0 ) {
			PlantProfile( ProfileNum ).HeatingEnergy = PlantProfile( ProfileNum ).Energy;
			PlantProfile( ProfileNum ).CoolingEnergy = 0.0;
		} else {
			PlantProfile( ProfileNum ).HeatingEnergy = 0.0;
			PlantProfile( ProfileNum ).CoolingEnergy = std::abs( PlantProfile( ProfileNum ).Energy );
		}

	}

	//     NOTICE

	//     Copyright (c) 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.

	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in main.cc.

	//     NOTICE: The U.S. Government is granted for itself and others acting on its
	//     behalf a paid-up, nonexclusive, irrevocable, worldwide license in this data to
	//     reproduce, prepare derivative works, and perform publicly and display publicly.
	//     Beginning five (5) years after permission to assert copyright is granted,
	//     subject to two possible five year renewals, the U.S. Government is granted for
	//     itself and others acting on its behalf a paid-up, non-exclusive, irrevocable
	//     worldwide license in this data to reproduce, prepare derivative works,
	//     distribute copies to the public, perform publicly and display publicly, and to
	//     permit others to do so.

	//     TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.

} // PlantLoadProfile

} // EnergyPlus
