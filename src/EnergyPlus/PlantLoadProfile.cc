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
	bool GetPlantLoadProfileInputFlag( true );
	int NumOfPlantProfile;

	// Object Data
	Array1D< PlantProfileData > PlantProfile;

	PlantComponent *
	PlantProfileData::factory( std::string objectName ) {
		if ( GetPlantLoadProfileInputFlag ) {
			GetPlantProfileInput();
			GetPlantLoadProfileInputFlag = false;
		}
		// Now look for this particular pipe in the list
		for ( auto & plp : PlantProfile ) {
			if ( plp.Name == objectName ) {
				return &plp;
			}
		}
		// If we didn't find it, fatal
		ShowFatalError( "PlantLoadProfile::factory: Error getting inputs for pipe named: " + objectName );
		// Shut up the compiler
		return nullptr;
	}

	void
	PlantProfileData::onInitLoopEquip( const PlantLocation & EP_UNUSED( calledFromLocation ) )
	{
		this->InitPlantProfile( );
	}

	void
	PlantProfileData::simulate( const PlantLocation & EP_UNUSED( calledFromLocation ),
				    bool const EP_UNUSED( FirstHVACIteration ),
				    Real64 & EP_UNUSED( CurLoad ),
		       		    bool const EP_UNUSED( RunFlag ) )
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

		this->InitPlantProfile( );

		if ( this->MassFlowRate > 0.0 ) {
			Real64 Cp = GetSpecificHeatGlycol( PlantLoop( this->WLoopNum ).FluidName, this->InletTemp, PlantLoop( this->WLoopNum ).FluidIndex, RoutineName );
			DeltaTemp = this->Power / ( this->MassFlowRate * Cp );
		} else {
			this->Power = 0.0;
			DeltaTemp = 0.0;
		}

		this->OutletTemp = this->InletTemp - DeltaTemp;

		this->UpdatePlantProfile( );
		this->ReportPlantProfile( );

	} // simulate()

	void
	PlantProfileData::InitPlantProfile()
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
		Real64 FluidDensityInit;
		bool errFlag;

		// FLOW:

		// Do the one time initializations
		if ( this->SetLoopIndexFlag ) {
			if ( allocated( PlantLoop ) ) {
				errFlag = false;
				ScanPlantLoopsForObject( this->Name, this->TypeNum, this->WLoopNum, this->WLoopSideNum, this->WLoopBranchNum, this->WLoopCompNum, _, _, _, _, _, errFlag );
				if ( errFlag ) {
					ShowFatalError( "InitPlantProfile: Program terminated for previous conditions." );
				}

				this->SetLoopIndexFlag = false;
			}
		}

		if ( ! SysSizingCalc && this->InitSizing ) {
			RegisterPlantCompDesignFlow( InletNode, this->PeakVolFlowRate );
			this->InitSizing = false;
		}

		if ( BeginEnvrnFlag && this->Init ) {
			// Clear node initial conditions
			//DSU? can we centralize these temperature inits
			//    Node(InletNode)%Temp = 0.0
			Node( OutletNode ).Temp = 0.0;

			FluidDensityInit = GetDensityGlycol( PlantLoop( this->WLoopNum ).FluidName, InitConvTemp, PlantLoop( this->WLoopNum ).FluidIndex, RoutineName );

			Real64 MaxFlowMultiplier = GetScheduleMaxValue( this->FlowRateFracSchedule );

			InitComponentNodes( 0.0, this->PeakVolFlowRate * FluidDensityInit * MaxFlowMultiplier, this->InletNode, this->OutletNode, this->WLoopNum, this->WLoopSideNum, this->WLoopBranchNum, this->WLoopCompNum );

			this->EMSOverrideMassFlow = false;
			this->EMSMassFlowValue = 0.0;
			this->EMSOverridePower = false;
			this->EMSPowerValue = 0.0;
			this->Init = false;

		}

		if ( ! BeginEnvrnFlag ) this->Init = true;

		this->InletTemp = Node( InletNode ).Temp;
		this->Power = GetCurrentScheduleValue( this->LoadSchedule );

		if ( this->EMSOverridePower ) this->Power = this->EMSPowerValue;

		FluidDensityInit = GetDensityGlycol( PlantLoop( this->WLoopNum ).FluidName, this->InletTemp, PlantLoop( this->WLoopNum ).FluidIndex, RoutineName );

		// Get the scheduled mass flow rate
		this->VolFlowRate = this->PeakVolFlowRate * GetCurrentScheduleValue( this->FlowRateFracSchedule );

		this->MassFlowRate = this->VolFlowRate * FluidDensityInit;

		if ( this->EMSOverrideMassFlow ) this->MassFlowRate = this->EMSMassFlowValue;

		// Request the mass flow rate from the plant component flow utility routine
		SetComponentFlowRate( this->MassFlowRate, InletNode, OutletNode, this->WLoopNum, this->WLoopSideNum, this->WLoopBranchNum, this->WLoopCompNum );

		this->VolFlowRate = this->MassFlowRate / FluidDensityInit;

	} // InitPlantProfile()

	void
	PlantProfileData::UpdatePlantProfile()
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

		OutletNode = this->OutletNode;

		// Set outlet node variables that are possibly changed
		Node( OutletNode ).Temp = this->OutletTemp;

		//DSU? enthalpy? quality etc? central routine? given inlet node, fluid type, delta T, properly fill all node vars?

	}

	void
	PlantProfileData::ReportPlantProfile()
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
		this->Energy = this->Power * TimeStepSys * SecInHour;

		if ( this->Energy >= 0.0 ) {
			this->HeatingEnergy = this->Energy;
			this->CoolingEnergy = 0.0;
		} else {
			this->HeatingEnergy = 0.0;
			this->CoolingEnergy = std::abs( this->Energy );
		}

	}

	// Functions
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
				VerifyName( cAlphaArgs( 1 ), PlantProfile, ProfileNum - 1, IsNotOK, IsBlank, cCurrentModuleObject );
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
	clear_state(){
		NumOfPlantProfile = 0;
		GetPlantLoadProfileInputFlag = true;
		PlantProfile.deallocate();
	}


} // namespace PlantLoadProfile

} // namespace EnergyPlus
