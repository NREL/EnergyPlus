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
#include <AirTerminalUnit.hh>
#include <HVACFourPipeBeam.hh>
#include <BranchNodeConnections.hh>
#include <DataContaminantBalance.hh>
#include <DataDefineEquip.hh>
#include <DataEnvironment.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DataZoneEnergyDemands.hh>
#include <DataZoneEquipment.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <PlantUtilities.hh>
#include <Psychrometrics.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>
#include <CurveManager.hh>


namespace EnergyPlus {

namespace FourPipeBeam {

	Array1D< std::shared_ptr< HVACFourPipeBeam > > FourPipeBeams; // dimension to number of machines


//	HVACFourPipeBeam::HVACFourPipeBeam(){}
	///// Note use of shared_ptr here is not a good pattern, not to be replicated without further discussion.
	std::shared_ptr< AirTerminalUnit >
	HVACFourPipeBeam::fourPipeBeamFactory(
		int EP_UNUSED(objectType),
		std::string objectName
	){


		using InputProcessor::GetObjectItemNum;
		using InputProcessor::GetObjectItem;

		using InputProcessor::SameString;
		using DataLoopNode::NodeConnectionType_Inlet;
		using DataLoopNode::NodeConnectionType_Outlet;
		using DataLoopNode::NodeType_Air;
		using DataLoopNode::NodeType_Water;
		using DataLoopNode::ObjectIsNotParent;
		using DataLoopNode::ObjectIsParent;
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::TestCompSet;
		using BranchNodeConnections::SetUpCompSets;
		using DataZoneEquipment::ZoneEquipConfig;
		using namespace DataSizing;
		using DataDefineEquip::AirDistUnit;
		using DataDefineEquip::NumAirDistUnits;
		using CurveManager::GetCurveIndex;
		using namespace DataIPShortCuts;
		using ScheduleManager::GetScheduleIndex;
		using DataGlobals::ScheduleAlwaysOn;
		static std::string const routineName( "FourPipeBeamFactory " ); // include trailing blank space

		int beamIndex; // loop index


		static int NumAlphas( 0 ); // Number of Alphas for each GetObjectItem call
		static int NumNumbers( 0 ); // Number of Numbers for each GetObjectItem call

		//  certain object in the input file
		int IOStatus; // Used in GetObjectItem
		bool errFlag = false;
		static bool ErrorsFound( false ); // Set to true if errors in input, fatal at end of routine
		bool found = false;
		int ctrlZone; // controlled zome do loop index
		int supAirIn; // controlled zone supply air inlet index
		bool airNodeFound;
		int aDUIndex;

		///// Note use of shared_ptr here is not a good pattern, not to be replicated without further discussion.
		std::shared_ptr< HVACFourPipeBeam > thisBeam( new HVACFourPipeBeam() );

		// find the number of cooled beam units
		cCurrentModuleObject = "AirTerminal:SingleDuct:ConstantVolume:FourPipeBeam";

		NumAlphas = 16;
		NumNumbers = 11;

		// find beam index from name
		beamIndex = InputProcessor::GetObjectItemNum(cCurrentModuleObject, objectName );
		if ( beamIndex > 0 ) {
			InputProcessor::GetObjectItem( cCurrentModuleObject, beamIndex, cAlphaArgs, NumAlphas,
				 rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				found = true;
		} else {
			ErrorsFound = true;
		}

		GlobalNames::VerifyUniqueADUName( cCurrentModuleObject, cAlphaArgs( 1 ), errFlag, cCurrentModuleObject + " Name" );
		if ( errFlag ) {
			ErrorsFound = true;
		}
		thisBeam->name = cAlphaArgs( 1 );
		thisBeam->unitType = cCurrentModuleObject;

		if ( lAlphaFieldBlanks( 2 ) ) {
			thisBeam->airAvailSchedNum = ScheduleAlwaysOn;
		} else {
			thisBeam->airAvailSchedNum = GetScheduleIndex( cAlphaArgs( 2 ) ); // convert schedule name to pointer
			if ( thisBeam->airAvailSchedNum  == 0 ) {
				ShowSevereError( routineName + cCurrentModuleObject + ": invalid " + cAlphaFieldNames( 2 ) + " entered =" + cAlphaArgs( 2 ) + " for " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}
		}
		if ( lAlphaFieldBlanks( 3 ) ) {
			thisBeam->coolingAvailSchedNum = ScheduleAlwaysOn;
		} else {
			thisBeam->coolingAvailSchedNum = GetScheduleIndex( cAlphaArgs( 3 ) ); // convert schedule name to index
			if ( thisBeam->coolingAvailSchedNum  == 0 ) {
				ShowSevereError( routineName + cCurrentModuleObject + ": invalid " + cAlphaFieldNames( 3 ) + " entered =" + cAlphaArgs( 3 ) + " for " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}
		}
		if ( lAlphaFieldBlanks( 4 ) ) {
			thisBeam->heatingAvailSchedNum = ScheduleAlwaysOn;
		} else {
			thisBeam->heatingAvailSchedNum = GetScheduleIndex( cAlphaArgs( 4 ) ); // convert schedule name to index
			if ( thisBeam->heatingAvailSchedNum  == 0 ) {
				ShowSevereError( routineName + cCurrentModuleObject + ": invalid " + cAlphaFieldNames( 4 ) + " entered =" + cAlphaArgs( 4 ) + " for " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}
		}

		thisBeam->airInNodeNum = GetOnlySingleNode( cAlphaArgs( 5 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent, cAlphaFieldNames( 5 ) );
		thisBeam->airOutNodeNum = GetOnlySingleNode( cAlphaArgs( 6 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent, cAlphaFieldNames( 6 ) );
		if ( lAlphaFieldBlanks( 7 ) && lAlphaFieldBlanks( 8 ) ) { // no chilled water nodes, no beam cooling
			thisBeam->beamCoolingPresent = false;
		} else if ( lAlphaFieldBlanks( 7 ) && ! lAlphaFieldBlanks( 8 ) ){ // outlet node but no inlet node for chilled water
			thisBeam->beamCoolingPresent = false;
			ShowWarningError( routineName + cCurrentModuleObject + ": missing " + cAlphaFieldNames( 7 ) + " for " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) + ", simulation continues with no beam cooling" );
		} else if ( ! lAlphaFieldBlanks( 7 ) &&  lAlphaFieldBlanks( 8 ) ){ // inlet node but no outlet node for chilled water
			thisBeam->beamCoolingPresent = false;
			ShowWarningError( routineName + cCurrentModuleObject + ": missing " + cAlphaFieldNames( 8 ) + " for " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) + ", simulation continues with no beam cooling" );
		} else {
			thisBeam->beamCoolingPresent = true;
			thisBeam->cWInNodeNum = GetOnlySingleNode( cAlphaArgs( 7 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 2, ObjectIsParent, cAlphaFieldNames( 7 ) );
			thisBeam->cWOutNodeNum = GetOnlySingleNode( cAlphaArgs( 8 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 2, ObjectIsParent, cAlphaFieldNames( 8 ) );
		}
		if ( lAlphaFieldBlanks( 9 ) && lAlphaFieldBlanks( 10 ) ) { // no hot water nodes, no beam heating
			thisBeam->beamHeatingPresent = false;
		} else if ( lAlphaFieldBlanks( 9 ) && ! lAlphaFieldBlanks( 10 ) ){ // outlet node but no inlet node for hot water
			thisBeam->beamHeatingPresent = false;
			ShowWarningError( routineName + cCurrentModuleObject + ": missing " + cAlphaFieldNames( 9 ) + " for " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) + ", simulation continues with no beam heating" );
		} else if ( ! lAlphaFieldBlanks( 9 ) &&  lAlphaFieldBlanks( 10 ) ){ // inlet node but no outlet node for hot water
			thisBeam->beamHeatingPresent = false;
			ShowWarningError( routineName + cCurrentModuleObject + ": missing " + cAlphaFieldNames( 10 ) + " for " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) + ", simulation continues with no beam heating" );
		} else {
			thisBeam->beamHeatingPresent = true;
			thisBeam->hWInNodeNum = GetOnlySingleNode( cAlphaArgs( 9 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 2, ObjectIsParent, cAlphaFieldNames( 9 ) );
			thisBeam->hWOutNodeNum = GetOnlySingleNode( cAlphaArgs( 10 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 2, ObjectIsParent, cAlphaFieldNames( 10 ) );
		}
		thisBeam->vDotDesignPrimAir = rNumericArgs( 1 );
		if ( thisBeam->vDotDesignPrimAir == AutoSize ) {
			thisBeam->vDotDesignPrimAirWasAutosized = true;
		}
		thisBeam->vDotDesignCW = rNumericArgs( 2 );
		if ( thisBeam->vDotDesignCW == AutoSize && thisBeam->beamCoolingPresent ) {
			thisBeam->vDotDesignCWWasAutosized = true;
		}
		thisBeam->vDotDesignHW = rNumericArgs( 3 );
		if ( thisBeam->vDotDesignHW == AutoSize && thisBeam->beamHeatingPresent ) {
			thisBeam->vDotDesignHWWasAutosized = true;
		}
		thisBeam->totBeamLength = rNumericArgs( 4 );
		if ( thisBeam->totBeamLength ==  AutoSize ) {
			thisBeam->totBeamLengthWasAutosized = true;
		}
		thisBeam->vDotNormRatedPrimAir  = rNumericArgs( 5 );
		thisBeam->qDotNormRatedCooling  = rNumericArgs( 6 );
		thisBeam->deltaTempRatedCooling = rNumericArgs( 7 );
		thisBeam->vDotNormRatedCW       = rNumericArgs( 8 );

		thisBeam->modCoolingQdotDeltaTFuncNum = GetCurveIndex( cAlphaArgs( 11 ) );
		if ( thisBeam->modCoolingQdotDeltaTFuncNum == 0 && thisBeam->beamCoolingPresent ) {
			ShowSevereError( routineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
			ShowContinueError( "Invalid " + cAlphaFieldNames( 11 ) + '=' + cAlphaArgs( 11 ) );
			ErrorsFound = true;
		}
		thisBeam->modCoolingQdotAirFlowFuncNum = GetCurveIndex( cAlphaArgs( 12 ) );
		if ( thisBeam->modCoolingQdotAirFlowFuncNum == 0 && thisBeam->beamCoolingPresent ) {
			ShowSevereError( routineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
			ShowContinueError( "Invalid " + cAlphaFieldNames( 12 ) + '=' + cAlphaArgs( 12 ) );
			ErrorsFound = true;
		}
		thisBeam->modCoolingQdotCWFlowFuncNum = GetCurveIndex( cAlphaArgs( 13 ) );
		if ( thisBeam->modCoolingQdotCWFlowFuncNum == 0 && thisBeam->beamCoolingPresent ) {
			ShowSevereError( routineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
			ShowContinueError( "Invalid " + cAlphaFieldNames( 13 ) + '=' + cAlphaArgs( 13 ) );
			ErrorsFound = true;
		}
		thisBeam->qDotNormRatedHeating  = rNumericArgs( 9 );
		thisBeam->deltaTempRatedHeating = rNumericArgs( 10 );
		thisBeam->vDotNormRatedHW       = rNumericArgs( 11 );
		thisBeam->modHeatingQdotDeltaTFuncNum = GetCurveIndex( cAlphaArgs( 14 ) );
		if ( thisBeam->modHeatingQdotDeltaTFuncNum == 0 && thisBeam->beamHeatingPresent ) {
			ShowSevereError( routineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
			ShowContinueError( "Invalid " + cAlphaFieldNames( 14 ) + '=' + cAlphaArgs( 14 ) );
			ErrorsFound = true;
		}
		thisBeam->modHeatingQdotAirFlowFuncNum = GetCurveIndex( cAlphaArgs( 15 ) );
		if ( thisBeam->modHeatingQdotAirFlowFuncNum == 0 && thisBeam->beamHeatingPresent ) {
			ShowSevereError( routineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
			ShowContinueError( "Invalid " + cAlphaFieldNames( 15 ) + '=' + cAlphaArgs( 15 ) );
			ErrorsFound = true;
		}
		thisBeam->modHeatingQdotHWFlowFuncNum = GetCurveIndex( cAlphaArgs( 16 ) );
		if ( thisBeam->modHeatingQdotHWFlowFuncNum == 0 && thisBeam->beamHeatingPresent ) {
			ShowSevereError( routineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
			ShowContinueError( "Invalid " + cAlphaFieldNames( 16 ) + '=' + cAlphaArgs( 16 ) );
			ErrorsFound = true;
		}
		// Register component set data
		TestCompSet( cCurrentModuleObject, thisBeam->name, DataLoopNode::NodeID( thisBeam->airInNodeNum ),
						DataLoopNode::NodeID( thisBeam->airOutNodeNum ), "Air Nodes" );
		if ( thisBeam->beamCoolingPresent ) {
			TestCompSet( cCurrentModuleObject, thisBeam->name, DataLoopNode::NodeID( thisBeam->cWInNodeNum ),
						DataLoopNode::NodeID( thisBeam->cWOutNodeNum ), "Chilled Water Nodes" );
		}
		if ( thisBeam->beamHeatingPresent ) {
			TestCompSet( cCurrentModuleObject, thisBeam->name, DataLoopNode::NodeID( thisBeam->hWInNodeNum ),
						DataLoopNode::NodeID( thisBeam->hWOutNodeNum ), "Hot Water Nodes" );
		}

		//Setup the Cooled Beam reporting variables
		if ( thisBeam->beamCoolingPresent ) {
			SetupOutputVariable( "Zone Air Terminal Beam Sensible Cooling Energy [J]", thisBeam->beamCoolingEnergy, "System", "Sum", thisBeam->name, _, "ENERGYTRANSFER", "COOLINGCOILS", _, "System" );
			SetupOutputVariable( "Zone Air Terminal Beam Sensible Cooling Rate [W]", thisBeam->beamCoolingRate, "System", "Average", thisBeam->name );
		}
		if ( thisBeam->beamHeatingPresent ) {
			SetupOutputVariable( "Zone Air Terminal Beam Sensible Heating Energy [J]", thisBeam->beamHeatingEnergy, "System", "Sum", thisBeam->name, _, "ENERGYTRANSFER", "HEATINGCOILS", _, "System" );
			SetupOutputVariable( "Zone Air Terminal Beam Sensible Heating Rate [W]", thisBeam->beamHeatingRate, "System", "Average", thisBeam->name );
		}
		SetupOutputVariable( "Zone Air Terminal Primary Air Sensible Cooling Energy [J]", thisBeam->supAirCoolingEnergy, "System", "Sum", thisBeam->name );
		SetupOutputVariable( "Zone Air Terminal Primary Air Sensible Cooling Rate [W]", thisBeam->supAirCoolingRate, "System", "Average", thisBeam->name );
		SetupOutputVariable( "Zone Air Terminal Primary Air Sensible Heating Energy [J]", thisBeam->supAirHeatingEnergy, "System", "Sum", thisBeam->name );
		SetupOutputVariable( "Zone Air Terminal Primary Air Sensible Heating Rate [W]", thisBeam->supAirHeatingRate, "System", "Average", thisBeam->name );
		SetupOutputVariable( "Zone Air Terminal Primary Air Flow Rate [m3/s]", thisBeam->primAirFlow, "System", "Average", thisBeam->name );

		// Fill the Zone Equipment data with the supply air inlet node number of this unit.
		airNodeFound = false;
		for ( ctrlZone = 1; ctrlZone <= DataGlobals::NumOfZones; ++ctrlZone ) {
			if ( ! ZoneEquipConfig( ctrlZone ).IsControlled ) continue;
			for ( supAirIn = 1; supAirIn <= ZoneEquipConfig( ctrlZone ).NumInletNodes; ++supAirIn ) {
				if ( thisBeam->airOutNodeNum == ZoneEquipConfig( ctrlZone ).InletNode( supAirIn ) ) {
					thisBeam->zoneIndex = ctrlZone;
					thisBeam->zoneNodeIndex = ZoneEquipConfig( ctrlZone ).ZoneNode;
					ZoneEquipConfig( ctrlZone ).AirDistUnitCool( supAirIn ).InNode = thisBeam->airInNodeNum;
					ZoneEquipConfig( ctrlZone ).AirDistUnitCool( supAirIn ).OutNode = thisBeam->airOutNodeNum;
					if ( thisBeam->beamHeatingPresent ) {
						ZoneEquipConfig( ctrlZone ).AirDistUnitHeat( supAirIn ).InNode = thisBeam->airInNodeNum;
						ZoneEquipConfig( ctrlZone ).AirDistUnitHeat( supAirIn ).OutNode =thisBeam->airOutNodeNum;
					}
					airNodeFound = true;
					break;
				}
			}
		}
		if ( ! airNodeFound ) {
			ShowSevereError( "The outlet air node from the " + cCurrentModuleObject + " = " + thisBeam->name );
			ShowContinueError( "did not have a matching Zone Equipment Inlet Node, Node =" + cAlphaArgs( 5 ) );
			ErrorsFound = true;
		}


		for ( aDUIndex = 1; aDUIndex <= NumAirDistUnits; ++aDUIndex ) {
			if ( thisBeam->airOutNodeNum == AirDistUnit( aDUIndex ).OutletNodeNum ) {
				thisBeam->aDUNum = aDUIndex;
			}
		}
		// assumes if there isn't one assigned, it's an error
		if ( thisBeam->aDUNum == 0 ) {
			ShowSevereError( routineName + "No matching Air Distribution Unit, for Unit = [" + cCurrentModuleObject + ',' + thisBeam->name + "]." );
			ShowContinueError( "...should have outlet node=" + DataLoopNode::NodeID( thisBeam->airOutNodeNum ) );
			ErrorsFound = true;
		}

		if ( found && !ErrorsFound ) {
			FourPipeBeams.push_back( thisBeam );
			return thisBeam;
		} else {
			ShowFatalError( routineName + "Errors found in getting input. Preceding conditions cause termination." );
			return nullptr;
		}

	}




	void
	HVACFourPipeBeam::simulate(
		bool const FirstHVACIteration, // TRUE if first HVAC iteration in time step
		Real64 & NonAirSysOutput // convective cooling by the beam system [W]
	)
	{


		// initialize the unit
		this->init( FirstHVACIteration );

		// control and simulate the beam
		if ( ! this->mySizeFlag ) {
			this->control( FirstHVACIteration, NonAirSysOutput );

			// Update the current unit's outlet nodes.
			this->update();

			// Fill the report variables.
			this->report();
		}
	}

	void
	HVACFourPipeBeam::init(
		bool const FirstHVACIteration // TRUE if first air loop solution this HVAC step
	)
	{

		// Using
		using DataZoneEquipment::ZoneEquipInputsFilled;
		using DataZoneEquipment::CheckZoneEquipmentList;
		using DataDefineEquip::AirDistUnit;
		using DataPlant::PlantLoop;
		using DataPlant::ScanPlantLoopsForObject;
		using DataPlant::TypeOf_FourPipeBeamAirTerminal;
		using PlantUtilities::InitComponentNodes;
		using PlantUtilities::SetComponentFlowRate;
		using DataGlobals::SysSizingCalc;
		using DataGlobals::BeginEnvrnFlag;
		using DataLoopNode::Node;
		using ScheduleManager::GetCurrentScheduleValue;

		static std::string const routineName( "HVACFourPipeBeam::init" );

		bool errFlag = false;

		if ( this->plantLoopScanFlag && allocated( PlantLoop ) ) {
			errFlag = false;
			if (this->beamCoolingPresent){
				ScanPlantLoopsForObject( this->name, TypeOf_FourPipeBeamAirTerminal, this->cWLocation.loopNum,
					this->cWLocation.loopSideNum, this->cWLocation.branchNum, this->cWLocation.compNum, _, _, _,
					this->cWInNodeNum, _, errFlag );
				if ( errFlag ) {
					ShowFatalError( routineName + " Program terminated for previous conditions." );
				}
			}
			if (this->beamHeatingPresent ){
				ScanPlantLoopsForObject( this->name, TypeOf_FourPipeBeamAirTerminal, this->hWLocation.loopNum,
					this->hWLocation.loopSideNum, this->hWLocation.branchNum, this->hWLocation.compNum, _, _, _,
					this->hWInNodeNum, _, errFlag );
				if ( errFlag ) {
					ShowFatalError( routineName + " Program terminated for previous conditions." );
				}
			}
			this->plantLoopScanFlag = false;
		}

		if ( ! this->zoneEquipmentListChecked && ZoneEquipInputsFilled ) {
			// Check to see if there is a Air Distribution Unit on the Zone Equipment List
			if ( this->aDUNum != 0 ) {
				if ( ! CheckZoneEquipmentList( "ZONEHVAC:AIRDISTRIBUTIONUNIT", AirDistUnit( this->aDUNum ).Name ) ) {
					ShowSevereError( routineName + ": ADU=[Air Distribution Unit," + AirDistUnit( this->aDUNum ).Name + "] is not on any ZoneHVAC:EquipmentList." );
					ShowContinueError( "...Unit=[" + this->unitType + ',' + this->name + "] will not be simulated." );
				}
			this->zoneEquipmentListChecked = true;
			}
		}

		if ( ! SysSizingCalc && this->mySizeFlag && ! this->plantLoopScanFlag ) {
	//	if ( DataGlobals::SysSizingCalc && this->mySizeFlag && ! this->plantLoopScanFlag ) {

			this->set_size(); // calculate autosize values (in any) and convert volume flow rates to mass flow rates
			if (this->beamCoolingPresent ) { // initialize chilled water design mass flow rate in plant routines
				InitComponentNodes( 0.0,
									this->mDotDesignCW,
									this->cWInNodeNum,
									this->cWOutNodeNum,
									this->cWLocation.loopNum,
									this->cWLocation.loopSideNum,
									this->cWLocation.branchNum,
									this->cWLocation.compNum
									);
			}
			if (this->beamHeatingPresent ) { // initialize hot water design mass flow rate in plant routines
				InitComponentNodes( 0.0,
									this->mDotDesignHW,
									this->hWInNodeNum,
									this->hWOutNodeNum,
									this->hWLocation.loopNum,
									this->hWLocation.loopSideNum,
									this->hWLocation.branchNum,
									this->hWLocation.compNum
									);
			}
			this->mySizeFlag = false;
		}

		// Do the Begin Environment initializations
		if ( BeginEnvrnFlag && this->myEnvrnFlag ) {

			Node( this->airInNodeNum  ).MassFlowRateMax = this->mDotDesignPrimAir;
			Node( this->airOutNodeNum ).MassFlowRateMax = this->mDotDesignPrimAir;
			Node( this->airInNodeNum  ).MassFlowRateMin = 0.0;
			Node( this->airOutNodeNum ).MassFlowRateMin = 0.0;

			if (this->beamCoolingPresent ) { // initialize chilled water design mass flow rate in plant routines
				InitComponentNodes( 0.0,
									this->mDotDesignCW,
									this->cWInNodeNum,
									this->cWOutNodeNum,
									this->cWLocation.loopNum,
									this->cWLocation.loopSideNum,
									this->cWLocation.branchNum,
									this->cWLocation.compNum
									);
			}
			if (this->beamHeatingPresent ) { // initialize hot water design mass flow rate in plant routines
				InitComponentNodes( 0.0,
									this->mDotDesignHW,
									this->hWInNodeNum,
									this->hWOutNodeNum,
									this->hWLocation.loopNum,
									this->hWLocation.loopSideNum,
									this->hWLocation.branchNum,
									this->hWLocation.compNum
									);
			}

			this->myEnvrnFlag = false;
		} // end one time inits

		if ( ! BeginEnvrnFlag ) {
			this->myEnvrnFlag = true;
		}

		// Do the start of HVAC time step initializations
		if ( FirstHVACIteration ) {
			// check availability schedules and set flags
			if (GetCurrentScheduleValue( this->airAvailSchedNum ) > 0.0 ){
				this->airAvailable =  true;
			} else {
				this->airAvailable = false;
			}
			if ( this->airAvailable && beamCoolingPresent
				&& ( GetCurrentScheduleValue( this->coolingAvailSchedNum ) > 0.0) ) {
				this->coolingAvailable = true;
			} else {
				this->coolingAvailable = false;
			}
			if ( this->airAvailable && beamHeatingPresent &&
				( GetCurrentScheduleValue( this->heatingAvailSchedNum ) > 0.0) ) {
				this->heatingAvailable = true;
			} else {
				this->heatingAvailable = false;
			}
			// check for upstream zero flow. If nonzero and air available, set primary flow to max
			if ( this->airAvailable && Node( this->airInNodeNum ).MassFlowRate > 0.0 ) {
				Node( this->airInNodeNum ).MassFlowRate = this->mDotDesignPrimAir;
			} else {
				Node( this->airInNodeNum ).MassFlowRate = 0.0;
			}
			// reset the max and min avail flows
			if ( this->airAvailable && Node( this->airInNodeNum ).MassFlowRateMaxAvail > 0.0 ) {
				Node( this->airInNodeNum ).MassFlowRateMaxAvail = this->mDotDesignPrimAir;
				Node( this->airInNodeNum ).MassFlowRateMinAvail = this->mDotDesignPrimAir;
			} else {
				Node( this->airInNodeNum ).MassFlowRateMaxAvail = 0.0;
				Node( this->airInNodeNum ).MassFlowRateMinAvail = 0.0;
			}
		}

		// do these initializations every time step
		if ( beamCoolingPresent ) {
			this->cWTempIn = Node( this->cWInNodeNum ).Temp;
			this->cWTempOut = this->cWTempIn;
		}
		if ( beamHeatingPresent ) {
			this->hWTempIn = Node( this->hWInNodeNum ).Temp;
			this->hWTempOut = this->hWTempIn;
		}
		this->mDotSystemAir = Node( this->airInNodeNum ).MassFlowRateMaxAvail;
		Node( this->airInNodeNum ).MassFlowRate = this->mDotSystemAir;
		this->tDBZoneAirTemp = Node( this->zoneNodeIndex ).Temp;
		this->tDBSystemAir = Node( this->airInNodeNum ).Temp;
		this->cpZoneAir = Psychrometrics::PsyCpAirFnWTdb( Node( this->zoneNodeIndex ).HumRat,
															Node( this->zoneNodeIndex ).Temp );
		this->cpSystemAir = Psychrometrics::PsyCpAirFnWTdb( Node( this->airInNodeNum ).HumRat,
															Node( this->airInNodeNum ).Temp );
		this->qDotBeamCooling = 0.0;
		this->qDotBeamHeating = 0.0;
		this->supAirCoolingRate = 0.0;
		this->supAirHeatingRate = 0.0;
		this->beamCoolingRate = 0.0;
		this->beamHeatingRate = 0.0;
		this->primAirFlow = 0.0;

	} //init

	void
	HVACFourPipeBeam::set_size()
	{

		// Using
		using DataEnvironment::StdRhoAir;
		using namespace DataSizing;
		using namespace InputProcessor;
		using PlantUtilities::RegisterPlantCompDesignFlow;
		using ReportSizingManager::ReportSizingOutput;
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSpecificHeatGlycol;
		using DataPlant::PlantLoop;
		using DataPlant::MyPlantSizingIndex;
		using Psychrometrics::PsyCpAirFnWTdb;
		using namespace std::placeholders;
		using General::SolveRegulaFalsi;

		static std::string const routineName( "HVACFourPipeBeam::set_size " );
		static int pltSizCoolNum( 0 ); // index of plant sizing object for the cooling loop
		static int pltSizHeatNum( 0 );

		bool ErrorsFound = false;
		Real64 rho; // local fluid density
		bool noHardSizeAnchorAvailable; // aid for complex logic surrounding mix of hard size and autosizes
		Real64 cpAir = 0.0;
		int SolFlag;
		Real64 ErrTolerance = 0.001;

		Real64 mDotAirSolutionHeating = 0.0;
		Real64 mDotAirSolutionCooling = 0.0;
		Real64 originalTermUnitSizeMaxVDot = 0.0;
		Real64 originalTermUnitSizeCoolVDot = 0.0;
		Real64 originalTermUnitSizeHeatVDot = 0.0;

		// convert rated primary flow rate to mass flow rate using standard pressure and dry air at 20.0
		this->mDotNormRatedPrimAir = this->vDotNormRatedPrimAir * DataEnvironment::rhoAirSTP;

		noHardSizeAnchorAvailable = false;

		if ( CurZoneEqNum > 0 ) {
			originalTermUnitSizeMaxVDot = std::max( TermUnitFinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow,
												TermUnitFinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow );
			originalTermUnitSizeCoolVDot = TermUnitFinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow;
			originalTermUnitSizeHeatVDot = TermUnitFinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow;
		}

		if ( this->totBeamLengthWasAutosized && this->vDotDesignPrimAirWasAutosized
			&& this->vDotDesignCWWasAutosized && this->vDotDesignHWWasAutosized ) {
			noHardSizeAnchorAvailable = true;
		} else if ( this->totBeamLengthWasAutosized && this->vDotDesignPrimAirWasAutosized
			&& this->vDotDesignCWWasAutosized && ! beamHeatingPresent) {
			noHardSizeAnchorAvailable = true;
		} else if ( this->totBeamLengthWasAutosized && this->vDotDesignPrimAirWasAutosized
			&& ! this->beamCoolingPresent && this->vDotDesignHWWasAutosized ) {
			noHardSizeAnchorAvailable = true;
		} else if ( ! this->totBeamLengthWasAutosized ) { // the simplest case is where length is not autosized
			//use the normalized rated values (likely defaulted) with length to calculate any that are autosized
			if ( this->vDotDesignPrimAirWasAutosized ) {
				this->vDotDesignPrimAir = this->vDotNormRatedPrimAir * this->totBeamLength;
			}
			if ( this->vDotDesignCWWasAutosized ) {
				this->vDotDesignCW = this->vDotNormRatedCW * this->totBeamLength;
			}
			if ( vDotDesignHWWasAutosized ) {
				this->vDotDesignHW = this->vDotNormRatedHW * this->totBeamLength;
			}
		} else { // need to find beam length
			//the next simplest case is if the supply air rate is given
			if ( ! this->vDotDesignPrimAirWasAutosized ) { //
				// find length from air flow rate and then proceed
				this->totBeamLength = this->vDotDesignPrimAir / this->vDotNormRatedPrimAir;
				if ( this->vDotDesignCWWasAutosized ) {
					this->vDotDesignCW = this->vDotNormRatedCW * this->totBeamLength;
				}
				if ( vDotDesignHWWasAutosized ) {
					this->vDotDesignHW = this->vDotNormRatedHW * this->totBeamLength;
				}
			} else { // both air and length are autosized
				if (this->beamCoolingPresent && ! this->vDotDesignCWWasAutosized ) { // we have a chilled water flow rate to use
					this->totBeamLength = this->vDotDesignCW / this->vDotNormRatedCW ;
					this->vDotDesignPrimAir = this->vDotNormRatedPrimAir * this->totBeamLength;
					if ( vDotDesignHWWasAutosized ) {
						this->vDotDesignHW = this->vDotNormRatedHW * this->totBeamLength;
					}
				} else if ( this->beamHeatingPresent && ! this->vDotDesignHWWasAutosized ) {// we have a hot water flow rate to use
					this->totBeamLength = this->vDotDesignHW / this->vDotNormRatedHW;
					this->vDotDesignPrimAir = this->vDotNormRatedPrimAir * this->totBeamLength;
					if ( this->vDotDesignCWWasAutosized ) { // don't think it can come here but...
						this->vDotDesignCW = this->vDotNormRatedCW * this->totBeamLength;
					}
				} else {
					// should not come here, developer exception
				}
			} // no air flow rate
		} // no beam length

		if ( noHardSizeAnchorAvailable && ( CurZoneEqNum > 0 ) ) { // need to use central sizing results to calculate

			// set up for solver

			CheckZoneSizing( this->unitType, this->name );
			//minimum flow rate is from air flow rate on the terminal unit final zone size ( typically ventilation minimum and may be too low)
			Real64 minFlow( 0.0 );
			Real64 maxFlowCool( 0.0 );
			minFlow = std::min( DataEnvironment::StdRhoAir * originalTermUnitSizeMaxVDot, FinalZoneSizing( CurZoneEqNum ).DesOAFlow * DataEnvironment::StdRhoAir );
			minFlow = std::max( 0.0, minFlow );
			//max flow is as if the air supply was sufficient to provide all the conditioning

			if ( beamCoolingPresent ) {
				cpAir = PsyCpAirFnWTdb( FinalZoneSizing( CurZoneEqNum ).DesCoolCoilInHumRatTU, FinalZoneSizing( CurZoneEqNum ).DesCoolCoilInTempTU );

				if ( ( FinalZoneSizing( CurZoneEqNum ).ZoneTempAtCoolPeak - FinalZoneSizing( CurZoneEqNum ).DesCoolCoilInTempTU ) > 2.0) { // avoid div by zero and blow up
					maxFlowCool = FinalZoneSizing( CurZoneEqNum ).DesCoolLoad
							/ (cpAir * ( FinalZoneSizing( CurZoneEqNum ).ZoneTempAtCoolPeak - FinalZoneSizing( CurZoneEqNum ).DesCoolCoilInTempTU ) );
				} else {
					maxFlowCool = FinalZoneSizing( CurZoneEqNum ).DesCoolLoad
							/ (cpAir * 2.0) ;
				}
				if ( minFlow * 3.0 >= maxFlowCool ) {
					minFlow = maxFlowCool / 3.0 ; // make sure min is significantly lower than max.
				}

				pltSizCoolNum = MyPlantSizingIndex( "four pipe beam unit", this->name, this->cWInNodeNum, this->cWOutNodeNum, ErrorsFound );
				if ( pltSizCoolNum == 0 ) {
					ShowSevereError( "Autosizing of water flow requires a cooling loop Sizing:Plant object" );
					ShowContinueError( "Occurs in " + this->unitType + " Object=" + this->name );
					ErrorsFound = true;
				} else {
					this->cWTempIn = DataSizing::PlantSizData(pltSizCoolNum).ExitTemp;
				}
				this->mDotHW = 0.0;
				this->tDBZoneAirTemp = FinalZoneSizing( CurZoneEqNum ).ZoneTempAtCoolPeak;
				this->tDBSystemAir = FinalZoneSizing( CurZoneEqNum ).DesCoolCoilInTempTU;
				this->cpZoneAir = PsyCpAirFnWTdb(	DataSizing::FinalZoneSizing( CurZoneEqNum ).ZoneHumRatAtCoolPeak,
													DataSizing::FinalZoneSizing( CurZoneEqNum ).ZoneTempAtCoolPeak);
				this->cpSystemAir =  PsyCpAirFnWTdb (	DataSizing::FinalZoneSizing( CurZoneEqNum ).DesCoolCoilInHumRatTU ,
														DataSizing::FinalZoneSizing( CurZoneEqNum ).DesCoolCoilInTempTU );
				this->qDotZoneReq = -1.0 * FinalZoneSizing( CurZoneEqNum ).DesCoolLoad;
				this->qDotZoneToCoolSetPt = -1.0 * FinalZoneSizing( CurZoneEqNum ).DesCoolLoad;
				this->airAvailable = true;
				this->coolingAvailable = true;
				this->heatingAvailable = false;
				SolveRegulaFalsi( ErrTolerance, 50, SolFlag, mDotAirSolutionCooling, std::bind( &HVACFourPipeBeam::residualSizing, this, _1 ), minFlow, maxFlowCool );
				if ( SolFlag == -1 ) {
					ShowWarningError( "Cooling load sizing search failed in four pipe beam unit called " + this->name );
					ShowContinueError( "  Iteration limit exceeded in calculating size for design cooling load" );
				} else if ( SolFlag == -2 ) {
					ShowWarningError( "Cooling load sizing search failed in four pipe beam unit called " + this->name );
					ShowContinueError( "  Bad size limits" );
				}
			}

			if ( beamHeatingPresent ) {
				cpAir = PsyCpAirFnWTdb( FinalZoneSizing( CurZoneEqNum ).DesHeatCoilInHumRatTU, FinalZoneSizing( CurZoneEqNum ).DesHeatCoilInTempTU );
				Real64 maxFlowHeat = 0.0;
				if ( ( FinalZoneSizing( CurZoneEqNum ).DesHeatCoilInTempTU - FinalZoneSizing( CurZoneEqNum ).ZoneTempAtHeatPeak ) > 2.0) { // avoid div by zero and blow up
					maxFlowHeat = FinalZoneSizing( CurZoneEqNum ).DesHeatLoad
							/ (cpAir * ( FinalZoneSizing( CurZoneEqNum ).DesHeatCoilInTempTU - FinalZoneSizing( CurZoneEqNum ).ZoneTempAtHeatPeak ) );
				} else {
					maxFlowHeat = FinalZoneSizing( CurZoneEqNum ).DesHeatLoad
							/ (cpAir * 2.0) ;
				}

				pltSizHeatNum = MyPlantSizingIndex( "four pipe beam unit", this->name, this->hWInNodeNum, this->hWOutNodeNum, ErrorsFound );
				if ( pltSizHeatNum == 0 ) {
					ShowSevereError( "Autosizing of water flow requires a heating loop Sizing:Plant object" );
					ShowContinueError( "Occurs in " + this->unitType + " Object=" + this->name );
					ErrorsFound = true;
				} else {
					this->hWTempIn = DataSizing::PlantSizData(pltSizHeatNum).ExitTemp;
				}
				this->mDotCW = 0.0;
				this->tDBZoneAirTemp = FinalZoneSizing( CurZoneEqNum ).ZoneTempAtHeatPeak;
				this->tDBSystemAir = FinalZoneSizing( CurZoneEqNum ).DesHeatCoilInTempTU;
				this->cpZoneAir = PsyCpAirFnWTdb(	DataSizing::FinalZoneSizing( CurZoneEqNum ).ZoneHumRatAtHeatPeak,
													DataSizing::FinalZoneSizing( CurZoneEqNum ).ZoneTempAtHeatPeak);
				this->cpSystemAir =  PsyCpAirFnWTdb (	DataSizing::FinalZoneSizing( CurZoneEqNum ).DesHeatCoilInHumRatTU ,
														DataSizing::FinalZoneSizing( CurZoneEqNum ).DesHeatCoilInTempTU );
				this->qDotZoneReq = FinalZoneSizing( CurZoneEqNum ).DesHeatLoad;
				this->qDotZoneToHeatSetPt = FinalZoneSizing( CurZoneEqNum ).DesHeatLoad;
				this->airAvailable = true;
				this->heatingAvailable = true;
				this->coolingAvailable = false;
				SolveRegulaFalsi( ErrTolerance, 50, SolFlag, mDotAirSolutionHeating, std::bind( &HVACFourPipeBeam::residualSizing, this, _1 ), 0.0, maxFlowHeat );
				if ( SolFlag == -1 ) {
					ShowWarningError( "Heating load sizing search failed in four pipe beam unit called " + this->name );
					ShowContinueError( "  Iteration limit exceeded in calculating size for design heating load" );
				} else if ( SolFlag == -2 ) {
					ShowWarningError( "Heating load sizing search failed in four pipe beam unit called " + this->name );
					ShowContinueError( "  Bad size limits" );
				}
			}

			// take the larger of heating and cooling
			this->mDotDesignPrimAir = std::max(mDotAirSolutionHeating, mDotAirSolutionCooling );
			// make sure this is higher than the zone OA requirement
			this->mDotDesignPrimAir = std::max(this->mDotDesignPrimAir, FinalZoneSizing( CurZoneEqNum ).DesOAFlow * DataEnvironment::StdRhoAir);
			this->vDotDesignPrimAir = this->mDotDesignPrimAir/ DataEnvironment::StdRhoAir;
			this->totBeamLength = this->vDotDesignPrimAir / this->vDotNormRatedPrimAir;
			if ( this->vDotDesignCWWasAutosized ) {
				this->vDotDesignCW = this->vDotNormRatedCW * this->totBeamLength;
			}
			if ( vDotDesignHWWasAutosized ) {
				this->vDotDesignHW = this->vDotNormRatedHW * this->totBeamLength;
			}
		}
		// fill in mass flow rate versions of working variables (regardless of autosizing)
		this->mDotDesignPrimAir = this->vDotDesignPrimAir * DataEnvironment::StdRhoAir;

		if ( ( originalTermUnitSizeMaxVDot > 0.0 ) && ( originalTermUnitSizeMaxVDot != this->vDotDesignPrimAir ) && ( CurZoneEqNum > 0 ) ) {
			// perturb system size to handle chnage in system size calculated without knowing about 4 pipe beam
			DataSizing::FinalSysSizing( DataZoneEquipment::ZoneEquipConfig(CurZoneEqNum).AirLoopNum ).DesMainVolFlow
				+= ( this->vDotDesignPrimAir - originalTermUnitSizeMaxVDot );
			DataSizing::FinalSysSizing( DataZoneEquipment::ZoneEquipConfig(CurZoneEqNum).AirLoopNum ).DesCoolVolFlow
				+= ( this->vDotDesignPrimAir - originalTermUnitSizeCoolVDot );
			DataSizing::FinalSysSizing( DataZoneEquipment::ZoneEquipConfig(CurZoneEqNum).AirLoopNum ).DesHeatVolFlow
				+= ( this->vDotDesignPrimAir - originalTermUnitSizeHeatVDot );
			DataSizing::FinalSysSizing( DataZoneEquipment::ZoneEquipConfig(CurZoneEqNum).AirLoopNum ).MassFlowAtCoolPeak
				+= ( this->vDotDesignPrimAir - originalTermUnitSizeCoolVDot ) * DataEnvironment::StdRhoAir;

			ReportSizingOutput( this->unitType, this->name, "AirLoopHVAC Design Supply Air Flow Rate Adjustment [m3/s]",
								( this->vDotDesignPrimAir - originalTermUnitSizeMaxVDot ) );
		}

		if ( this->beamCoolingPresent ) {
			rho = FluidProperties::GetDensityGlycol( DataPlant::PlantLoop( this->cWLocation.loopNum ).FluidName, DataGlobals::InitConvTemp,
									DataPlant::PlantLoop( this->cWLocation.loopNum ).FluidIndex, routineName );
			this->mDotNormRatedCW = this->vDotNormRatedCW * rho;
			this->mDotDesignCW = this->vDotDesignCW * rho ;
			PlantUtilities::InitComponentNodes( 0.0,
									this->mDotDesignCW,
									this->cWInNodeNum,
									this->cWOutNodeNum,
									this->cWLocation.loopNum,
									this->cWLocation.loopSideNum,
									this->cWLocation.branchNum,
									this->cWLocation.compNum
									);
		}
		if ( this->beamHeatingPresent ) {
			rho = FluidProperties::GetDensityGlycol( DataPlant::PlantLoop( this->hWLocation.loopNum ).FluidName, DataGlobals::InitConvTemp,
									DataPlant::PlantLoop( this->hWLocation.loopNum ).FluidIndex, routineName );
			this->mDotNormRatedHW = this->vDotNormRatedHW * rho;
			this->mDotDesignHW = this->vDotDesignHW * rho;
			PlantUtilities::InitComponentNodes( 0.0,
									this->mDotDesignHW,
									this->hWInNodeNum,
									this->hWOutNodeNum,
									this->hWLocation.loopNum,
									this->hWLocation.loopSideNum,
									this->hWLocation.branchNum,
									this->hWLocation.compNum
									);
		}

		// report final sizes if autosized
		if ( this->vDotDesignPrimAirWasAutosized ) {
			ReportSizingOutput( this->unitType, this->name, "Supply Air Flow Rate [m3/s]", this->vDotDesignPrimAir );
		}
		if ( this->vDotDesignCWWasAutosized ) {
			ReportSizingOutput( this->unitType, this->name, "Maximum Total Chilled Water Flow Rate [m3/s]", this->vDotDesignCW );
		}
		if ( this->vDotDesignHWWasAutosized ) {
			ReportSizingOutput( this->unitType, this->name, "Maximum Total Hot Water Flow Rate [m3/s]", this->vDotDesignHW );
		}
		if ( this->totBeamLengthWasAutosized ) {
			ReportSizingOutput( this->unitType, this->name, "Zone Total Beam Length [m]", this->totBeamLength);
		}
		// save the design water volume flow rate for use by the water loop sizing algorithms
		if ( this->vDotDesignCW > 0.0 && this->beamCoolingPresent ) {
			RegisterPlantCompDesignFlow( this->cWInNodeNum, this->vDotDesignCW );
		}
		if ( this->vDotDesignHW > 0.0 && this->beamHeatingPresent ) {
			RegisterPlantCompDesignFlow( this->hWInNodeNum, this->vDotDesignHW );
		}
		if ( ErrorsFound ) {
			ShowFatalError( "Preceding four pipe beam sizing errors cause program termination" );
		}

	} //set_size

	Real64 HVACFourPipeBeam::residualSizing(
		Real64 const airFlow // air flow in kg/s
	){

		static std::string const routineName( "Real64 HVACFourPipeBeam::residualSizing " );
		Real64 rho; // local fluid density
		Real64 Residuum; // residual to be minimized to zero

		this->mDotSystemAir =airFlow;
		this->vDotDesignPrimAir = this->mDotSystemAir / DataEnvironment::StdRhoAir ;

		this->totBeamLength = this->vDotDesignPrimAir / this->vDotNormRatedPrimAir;
		if ( this->vDotDesignCWWasAutosized ) {
			this->vDotDesignCW = this->vDotNormRatedCW * this->totBeamLength;
			rho = FluidProperties::GetDensityGlycol( DataPlant::PlantLoop( this->cWLocation.loopNum ).FluidName, DataGlobals::InitConvTemp,
									DataPlant::PlantLoop( this->cWLocation.loopNum ).FluidIndex, routineName );
			this->mDotNormRatedCW = this->vDotNormRatedCW * rho;
			this->mDotCW = this->vDotDesignCW * rho;
			if ( this-> beamCoolingPresent ) {
				PlantUtilities::InitComponentNodes(0.0,
										this->mDotCW,
										this->cWInNodeNum,
										this->cWOutNodeNum,
										this->cWLocation.loopNum,
										this->cWLocation.loopSideNum,
										this->cWLocation.branchNum,
										this->cWLocation.compNum
										);
			}
		}
		if ( vDotDesignHWWasAutosized ) {
			this->vDotDesignHW = this->vDotNormRatedHW * this->totBeamLength;
			rho = FluidProperties::GetDensityGlycol( DataPlant::PlantLoop( this->hWLocation.loopNum ).FluidName, DataGlobals::InitConvTemp,
									DataPlant::PlantLoop( this->hWLocation.loopNum ).FluidIndex, routineName );
			this->mDotNormRatedHW = this->vDotNormRatedHW * rho;
			this->mDotHW = this->vDotDesignHW * rho;
			if ( this-> beamHeatingPresent ) {
				PlantUtilities::InitComponentNodes( 0.0,
										this->mDotHW,
										this->hWInNodeNum,
										this->hWOutNodeNum,
										this->hWLocation.loopNum,
										this->hWLocation.loopSideNum,
										this->hWLocation.branchNum,
										this->hWLocation.compNum
										);
			}
		}
		this->calc();
		if ( this->qDotZoneReq != 0.0 ) {
			Residuum = ( ( this->qDotZoneReq - this->qDotTotalDelivered )
						/ this->qDotZoneReq );
		} else {
			Residuum = 1.0;
		}
		return Residuum;

	}

	void
	HVACFourPipeBeam::control(
		bool const EP_UNUSED( FirstHVACIteration ), // TRUE if 1st HVAC simulation of system timestep
		Real64 & NonAirSysOutput // convective cooling by the beam system [W]
	)
	{


		// Using/Aliasing
		using namespace DataZoneEnergyDemands;
		using General::SolveRegulaFalsi;
		using PlantUtilities::SetComponentFlowRate;
		using namespace std::placeholders;

		bool dOASMode = false ; // true if unit is operating as DOAS terminal with no heating or cooling by beam

		int SolFlag;
		Real64 ErrTolerance;

		NonAirSysOutput = 0.0; // initialize

		if ( this->mDotSystemAir < DataHVACGlobals::VerySmallMassFlow || ( ! this->airAvailable  && ! this->coolingAvailable && ! this->heatingAvailable ) ) { //unit is off
			this->mDotHW = 0.0;
			if ( this-> beamHeatingPresent )
			{
				SetComponentFlowRate(this->mDotHW, this->hWInNodeNum, this->hWOutNodeNum, this->hWLocation.loopNum,
								this->hWLocation.loopSideNum, this->hWLocation.branchNum, this->hWLocation.compNum );
			}
			this->hWTempOut = this->hWTempIn;
			// assume if there is still flow that unit has an internal bypass and convector does not still heat
			this->mDotCW = 0.0;
			this->cWTempOut = this->cWTempIn;
			if ( this-> beamCoolingPresent )
			{
				SetComponentFlowRate(this->mDotCW, this->cWInNodeNum, this->cWOutNodeNum, this->cWLocation.loopNum,
								this->cWLocation.loopSideNum, this->cWLocation.branchNum, this->cWLocation.compNum );
			}
			// assume if there is still flow that unit has an internal bypass and convector does not still cool
			// don't even need to run calc
			return;
		}


		if ( this->airAvailable && this->mDotSystemAir > DataHVACGlobals::VerySmallMassFlow && ! this->coolingAvailable && ! this->heatingAvailable) {
			dOASMode = true;
			this->mDotHW = 0.0;
			if ( this -> beamHeatingPresent )
			{
				SetComponentFlowRate(this->mDotHW, this->hWInNodeNum, this->hWOutNodeNum, this->hWLocation.loopNum,
								this->hWLocation.loopSideNum, this->hWLocation.branchNum, this->hWLocation.compNum );
			}
			// assume if there is still flow that unit has an internal bypass and convector does not still heat
			this->hWTempOut = this->hWTempIn;
			this->mDotCW = 0.0;
			if ( this-> beamCoolingPresent )
			{
				SetComponentFlowRate(this->mDotCW, this->cWInNodeNum, this->cWOutNodeNum, this->cWLocation.loopNum,
								this->cWLocation.loopSideNum, this->cWLocation.branchNum, this->cWLocation.compNum );
			}
			// assume if there is still flow that unit has an internal bypass and convector does not still cool
			this->cWTempOut = this->cWTempIn;
			this->calc();

			return;
		}

		// get zone loads
		this->qDotZoneReq         = ZoneSysEnergyDemand( this->zoneIndex ).RemainingOutputRequired;
		this->qDotZoneToHeatSetPt = ZoneSysEnergyDemand( this->zoneIndex ).RemainingOutputReqToHeatSP;
		this->qDotZoneToCoolSetPt = ZoneSysEnergyDemand( this->zoneIndex ).RemainingOutputReqToCoolSP;

		// decide if beam is in heating or cooling

		this->qDotSystemAir = this->mDotSystemAir*( (this->cpSystemAir * this->tDBSystemAir) - (this->cpZoneAir * this->tDBZoneAirTemp) );

		this->qDotBeamReq = this->qDotZoneReq - this->qDotSystemAir;

		if ( this->qDotBeamReq < - DataHVACGlobals::SmallLoad && this->coolingAvailable ){ // beam cooling needed
			// first calc with max chilled water flow
			this->mDotHW = 0.0;
			if ( this->beamHeatingPresent )
			{
				SetComponentFlowRate(this->mDotHW, this->hWInNodeNum, this->hWOutNodeNum, this->hWLocation.loopNum,
								this->hWLocation.loopSideNum, this->hWLocation.branchNum, this->hWLocation.compNum );
			}
			this->hWTempOut = this->hWTempIn;
			this->mDotCW = this->mDotDesignCW;
			this->calc();
			if ( this->qDotBeamCooling < ( qDotBeamReq - DataHVACGlobals::SmallLoad ) ) {
				// can overcool, modulate chilled water flow rate to meet load
				this->qDotBeamCoolingMax = this->qDotBeamCooling;
				ErrTolerance = 0.01;
				SolveRegulaFalsi( ErrTolerance, 50, SolFlag, this->mDotCW, std::bind( &HVACFourPipeBeam::residualCooling, this, _1 ), 0.0, this->mDotDesignCW );
				if ( SolFlag == -1 ) {
					//ShowWarningError( "Cold water control failed in four pipe beam unit called " + this->name );
					//ShowContinueError( "  Iteration limit exceeded in calculating cold water mass flow rate" );
				} else if ( SolFlag == -2 ) {
					//ShowWarningError( "Cold water control failed in four pipe beam unit called " + this->name );
					//ShowContinueError( "  Bad cold water flow limits" );
				}
				this->calc();
				NonAirSysOutput = this->qDotBeamCooling;
				return;
			} else { // can run flat out without overcooling, which we just did
				NonAirSysOutput = this->qDotBeamCooling;
				return;

			}


		} else if ( qDotBeamReq > DataHVACGlobals::SmallLoad && this->heatingAvailable ){ // beam heating needed
			// first calc with max hot water flow
			this->mDotCW = 0.0;
			if ( this->beamCoolingPresent )
			{
				SetComponentFlowRate(this->mDotCW, this->cWInNodeNum, this->cWOutNodeNum, this->cWLocation.loopNum,
								this->cWLocation.loopSideNum, this->cWLocation.branchNum, this->cWLocation.compNum );
			}
			this->cWTempOut = this->cWTempIn;
			this->mDotHW = this->mDotDesignHW;
			this->calc();
			if ( this->qDotBeamHeating > ( qDotBeamReq + DataHVACGlobals::SmallLoad ) ) {
				this->qDotBeamHeatingMax = this->qDotBeamHeating;
				// can overheat, modulate hot water flow to meet load
				ErrTolerance = 0.01;
				SolveRegulaFalsi( ErrTolerance, 50, SolFlag, this->mDotHW, std::bind( &HVACFourPipeBeam::residualHeating, this , _1), 0.0, this->mDotDesignHW);
				if ( SolFlag == -1 ) {
					//ShowWarningError( "Hot water control failed in four pipe beam unit called " + this->name );
					//ShowContinueError( "  Iteration limit exceeded in calculating hot water mass flow rate" );
				} else if ( SolFlag == -2 ) {
					//ShowWarningError( "Hot water control failed in four pipe beam called " + this->name );
					//ShowContinueError( "  Bad hot water flow limits" );
				}
				this->calc();
				NonAirSysOutput = this->qDotBeamHeating;
				return;

			} else { // can run flat out without overheating, which we just did
				NonAirSysOutput = this->qDotBeamHeating;
				return;

			}

		} else {
			this->mDotHW = 0.0;
			if ( this-> beamHeatingPresent )
			{
				SetComponentFlowRate(this->mDotHW, this->hWInNodeNum, this->hWOutNodeNum, this->hWLocation.loopNum,
								this->hWLocation.loopSideNum, this->hWLocation.branchNum, this->hWLocation.compNum );
			}
			this->hWTempOut = this->hWTempIn;
			// assume if there is still flow that unit has an internal bypass and convector does not still heat
			this->mDotCW = 0.0;
			this->cWTempOut = this->cWTempIn;
			if ( this -> beamCoolingPresent)
			{
				SetComponentFlowRate(this->mDotCW, this->cWInNodeNum, this->cWOutNodeNum, this->cWLocation.loopNum,
								this->cWLocation.loopSideNum, this->cWLocation.branchNum, this->cWLocation.compNum );
			}
			// assume if there is still flow that unit has an internal bypass and convector does not still cool
			// don't even need to run calc
			return;
		}

		return;
	}

	void
	HVACFourPipeBeam::calc(){

		// Using/Aliasing
		using PlantUtilities::SetComponentFlowRate;
		using FluidProperties::GetSpecificHeatGlycol;
		using FluidProperties::GetDensityGlycol;
		using DataPlant::PlantLoop;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const routineName( "HVACFourPipeBeam::calc " );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 fModCoolCWMdot; //Cooling capacity modification factor function of chilled water flow rate
		Real64 fModCoolDeltaT; //Cooling capacity modification factor function of air-water temperature difference
		Real64 fModCoolAirMdot; // Cooling capacity modification factor function of primary air flow rate
		Real64 fModHeatHWMdot; //Heating capacity modification factor function of hot water flow rate
		Real64 fModHeatDeltaT; //Heating capacity modification factor function of water - air temperature difference
		Real64 fModHeatAirMdot; //Heating capacity modification factor function of primary air flow rate
		Real64 cp; // local fluid specific heat

		this->qDotBeamHeating = 0.0;
		this->qDotBeamCooling = 0.0;
		this->qDotSystemAir = this->mDotSystemAir*( (this->cpSystemAir * this->tDBSystemAir) - (this->cpZoneAir * this->tDBZoneAirTemp) );

		if ( this->coolingAvailable &&  this->mDotCW > DataHVACGlobals::VerySmallMassFlow ) {
			//test chilled water flow against plant, it might not all be available
			SetComponentFlowRate(	this->mDotCW,
									this->cWInNodeNum,
									this->cWOutNodeNum,
									this->cWLocation.loopNum,
									this->cWLocation.loopSideNum,
									this->cWLocation.branchNum,
									this->cWLocation.compNum );
			fModCoolCWMdot = CurveManager::CurveValue( this->modCoolingQdotCWFlowFuncNum,
										( ( this->mDotCW / this->totBeamLength )
											/ this->mDotNormRatedCW ) );
			fModCoolDeltaT = CurveManager::CurveValue( this->modCoolingQdotDeltaTFuncNum,
										( ( this->tDBZoneAirTemp - this->cWTempIn )
											/ this->deltaTempRatedCooling ) );
			fModCoolAirMdot = CurveManager::CurveValue( this->modCoolingQdotAirFlowFuncNum,
										( ( this->mDotSystemAir / this->totBeamLength )
											/ this->mDotNormRatedPrimAir) );
			this->qDotBeamCooling = -1.0 * this->qDotNormRatedCooling * fModCoolDeltaT * fModCoolAirMdot * fModCoolCWMdot * this->totBeamLength;
			cp = GetSpecificHeatGlycol(PlantLoop(this->cWLocation.loopNum).FluidName, this->cWTempIn, PlantLoop(this->cWLocation.loopNum).FluidIndex, routineName);
			if ( this->mDotCW > 0.0 ) {
				this->cWTempOut = this->cWTempIn - ( this->qDotBeamCooling / (this->mDotCW * cp ) );
			} else {
				this->cWTempOut = this->cWTempIn;
			}
			// check if non physical temperature rise, can't be warmer than air
			if ( this->cWTempOut > (std::max( this->tDBSystemAir , this->tDBZoneAirTemp ) - 1.0 ) ) {
				// throw recurring warning as this indicates a problem in beam model input
				ShowRecurringWarningErrorAtEnd( routineName + " four pipe beam name " + this->name +
					", chilled water outlet temperature is too warm. Capacity was limited. check beam capacity input ",
					this-> cWTempOutErrorCount, this->cWTempOut, this->cWTempOut);
				//  restrict it within 1.0 C of warmest air and recalculate cooling
				this->cWTempOut = (std::max( this->tDBSystemAir , this->tDBZoneAirTemp ) - 1.0 );
				this->qDotBeamCooling = this->mDotCW * cp * (this->cWTempIn - this->cWTempOut);
			}
		} else {
			this->mDotCW = 0.0;
			if ( this-> beamCoolingPresent )
			{
				SetComponentFlowRate( this->mDotCW,
									this->cWInNodeNum,
									this->cWOutNodeNum,
									this->cWLocation.loopNum,
									this->cWLocation.loopSideNum,
									this->cWLocation.branchNum,
									this->cWLocation.compNum );
			}
			this->cWTempOut = this->cWTempIn;
			this->qDotBeamCooling = 0.0;
		}
		if ( this->heatingAvailable && this->mDotHW > DataHVACGlobals::VerySmallMassFlow ){
			//test hot water flow against plant, it might not all be available
			SetComponentFlowRate(	this->mDotHW,
									this->hWInNodeNum,
									this->hWOutNodeNum,
									this->hWLocation.loopNum,
									this->hWLocation.loopSideNum,
									this->hWLocation.branchNum,
									this->hWLocation.compNum );
			fModHeatHWMdot = CurveManager::CurveValue( this->modHeatingQdotHWFlowFuncNum,
										( ( this->mDotHW / this->totBeamLength )
											/ this->mDotNormRatedHW ) );
			fModHeatDeltaT = CurveManager::CurveValue( this->modHeatingQdotDeltaTFuncNum,
										( (this->hWTempIn - this->tDBZoneAirTemp )
											/ this->deltaTempRatedHeating ) );
			fModHeatAirMdot = CurveManager::CurveValue( this->modHeatingQdotAirFlowFuncNum,
										( ( this->mDotSystemAir / this->totBeamLength )
											/ this->mDotNormRatedPrimAir) );
			this->qDotBeamHeating = this->qDotNormRatedHeating * fModHeatDeltaT * fModHeatAirMdot * fModHeatHWMdot * this->totBeamLength;
			cp = GetSpecificHeatGlycol(PlantLoop(this->hWLocation.loopNum).FluidName, this->hWTempIn, PlantLoop(this->hWLocation.loopNum).FluidIndex, routineName);
			if ( this->mDotHW > 0.0 ) {
				this->hWTempOut = this->hWTempIn - ( this->qDotBeamHeating / (this->mDotHW * cp ) );
			} else {
				this->hWTempOut = this->hWTempIn;
			}
			// check if non physical temperature drop, can't be cooler than air
			if ( this->hWTempOut < (std::min( this->tDBSystemAir , this->tDBZoneAirTemp ) + 1.0 ) ) {
				// throw recurring warning as this indicates a problem in beam model input
				ShowRecurringWarningErrorAtEnd( routineName + " four pipe beam name " + this->name +
					", hot water outlet temperature is too cool. Capacity was limited. check beam capacity input ",
					this-> hWTempOutErrorCount, this->hWTempOut, this->hWTempOut);
				//  restrict it within 1.0 C of warmest air and recalculate cooling
				this->hWTempOut = (std::min( this->tDBSystemAir , this->tDBZoneAirTemp ) + 1.0 );
				this->qDotBeamHeating = this->mDotHW * cp * (this->hWTempIn - this->hWTempOut);
			}
		} else {
			this->mDotHW = 0.0;
			if ( this-> beamHeatingPresent )
			{
				SetComponentFlowRate(	this->mDotHW,
										this->hWInNodeNum,
										this->hWOutNodeNum,
										this->hWLocation.loopNum,
										this->hWLocation.loopSideNum,
										this->hWLocation.branchNum,
										this->hWLocation.compNum );
			}
			this->hWTempOut = this->hWTempIn;
			this->qDotBeamHeating = 0.0;
		}

		this->qDotTotalDelivered = this->qDotSystemAir + this->qDotBeamCooling + this->qDotBeamHeating;
	}

	Real64 HVACFourPipeBeam::residualCooling(
		Real64 const cWFlow // cold water flow rate in kg/s
	)
	{

		Real64 Residuum; // residual to be minimized to zero
		this->mDotHW = 0.0;
		this->mDotCW = cWFlow;
		this->calc();
		if ( this->qDotBeamCoolingMax != 0.0 ) {
			Residuum = ( ( ( this->qDotZoneToCoolSetPt - this->qDotSystemAir )- this->qDotBeamCooling )
						/ this->qDotBeamCoolingMax );
		} else {
			Residuum = 1.0;
		}
		return Residuum;
	}
	Real64 HVACFourPipeBeam::residualHeating(
		Real64 const hWFlow // hot water flow rate in kg/s
	)
	{

		Real64 Residuum; // residual to be minimized to zero
		this->mDotHW = hWFlow;
		this->mDotCW = 0.0;
		this->calc();
		if ( this->qDotBeamHeatingMax != 0.0 ) {
			Residuum = ( ( ( this->qDotZoneToHeatSetPt - this->qDotSystemAir ) - this->qDotBeamHeating )
							/ this->qDotBeamHeatingMax );
		} else {
			Residuum = 1.0;
		}

		return Residuum;
	}
	void
	HVACFourPipeBeam::update() const // update node date elsewhere in EnergyPlus, does not change state of this
	{

		using DataContaminantBalance::Contaminant;
		using PlantUtilities::SafeCopyPlantNode;

		// Set the outlet air nodes of the unit; note that all quantities are unchanged from inlet to outlet
		DataLoopNode::Node( this->airOutNodeNum ).MassFlowRate = DataLoopNode::Node( this->airInNodeNum ).MassFlowRate;
		DataLoopNode::Node( this->airOutNodeNum ).Temp = DataLoopNode::Node( this->airInNodeNum ).Temp;
		DataLoopNode::Node( this->airOutNodeNum ).HumRat = DataLoopNode::Node( this->airInNodeNum ).HumRat;
		DataLoopNode::Node( this->airOutNodeNum ).Enthalpy = DataLoopNode::Node( this->airInNodeNum ).Enthalpy;
		DataLoopNode::Node( this->airOutNodeNum ).Quality = DataLoopNode::Node( this->airInNodeNum ).Quality;
		DataLoopNode::Node( this->airOutNodeNum ).Press = DataLoopNode::Node( this->airInNodeNum ).Press;
		DataLoopNode::Node( this->airOutNodeNum ).MassFlowRateMin = DataLoopNode::Node( this->airInNodeNum ).MassFlowRateMin;
		DataLoopNode::Node( this->airOutNodeNum ).MassFlowRateMax = DataLoopNode::Node( this->airInNodeNum ).MassFlowRateMax;
		DataLoopNode::Node( this->airOutNodeNum ).MassFlowRateMinAvail = DataLoopNode::Node( this->airInNodeNum ).MassFlowRateMinAvail;
		DataLoopNode::Node( this->airOutNodeNum ).MassFlowRateMaxAvail = DataLoopNode::Node( this->airInNodeNum ).MassFlowRateMaxAvail;

		if ( Contaminant.CO2Simulation ) {
			DataLoopNode::Node( this->airOutNodeNum ).CO2 = DataLoopNode::Node( this->airInNodeNum ).CO2;
		}

		if ( Contaminant.GenericContamSimulation ) {
			DataLoopNode::Node( this->airOutNodeNum ).GenContam = DataLoopNode::Node( this->airInNodeNum ).GenContam;
		}

		// Set the outlet water nodes for the unit

		if ( this-> beamCoolingPresent ) {
			SafeCopyPlantNode( this->cWInNodeNum, this->cWOutNodeNum );
			DataLoopNode::Node( this->cWOutNodeNum ).Temp = this->cWTempOut;
		}
		if ( this-> beamHeatingPresent ){
			SafeCopyPlantNode( this->hWInNodeNum, this->hWOutNodeNum );
			DataLoopNode::Node( this->hWOutNodeNum ).Temp = this->hWTempOut;
		}

	}

	void
	HVACFourPipeBeam::report() // fill out local output variables for reporting
	{

		Real64 ReportingConstant;

		ReportingConstant = DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

		if ( this->beamCoolingPresent ) {
			this->beamCoolingRate = std::abs( this->qDotBeamCooling ); // report var has positive sign convention
			this->beamCoolingEnergy = this->beamCoolingRate * ReportingConstant;
		}
		if ( this-> beamHeatingPresent ) {
			this->beamHeatingRate = this->qDotBeamHeating;
			this->beamHeatingEnergy = this->beamHeatingRate * ReportingConstant;
		}
		if ( qDotSystemAir <= 0.0 ) { // cooling
			this->supAirCoolingRate = std::abs( this->qDotSystemAir );
			this->supAirHeatingRate = 0.0;
		} else {
			this->supAirHeatingRate = this->qDotSystemAir;
			this->supAirCoolingRate = 0.0;
		}
		this->supAirCoolingEnergy = this->supAirCoolingRate * ReportingConstant;
		this->supAirHeatingEnergy = this->supAirHeatingRate * ReportingConstant;

		this->primAirFlow = this->mDotSystemAir / DataEnvironment::StdRhoAir;

	}

} // HVACFourPipeBeam

} // EnergyPlus
