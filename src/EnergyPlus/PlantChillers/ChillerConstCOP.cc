#include <memory>

#include <ObjexxFCL/Array1D.hh>

#include <BranchNodeConnections.hh>
#include <PlantChillers/ChillerBase.hh> // would like to remove this...
#include <DataEnvironment.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <EMSManager.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <GlobalNames.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutAirNodeManager.hh>
#include <OutputProcessor.hh>
#include <PlantChillers/ChillerConstCOP.hh>
#include <PlantComponent.hh>
#include <PlantLocation.hh>
#include <PlantUtilities.hh>
#include <Psychrometrics.hh>
#include <ScheduleManager.hh>

namespace EnergyPlus {

namespace PlantChillers {

	Array1D< std::shared_ptr< ChillerConstCOP > > ConstCOPChiller; // dimension to number of machines

	ChillerConstCOP::ChillerConstCOP(){}
		
	std::shared_ptr< PlantComponent >
	ChillerConstCOP::constCOPChillerFactory(
		int EP_UNUSED(objectType),
		std::string objectName
	) {
		
		using namespace DataIPShortCuts;
		
		int IOStat;
		int NumAlphas;
		int NumNums;
		bool found = false;
		bool errFlag = false;
		bool ErrorsFound = false;

		std::string const RoutineName = "ConstCOPChillerFactory";
		std::string const cCurrentModuleObject = "Chiller:ConstantCOP";
		int const NumConstCOPChillers = InputProcessor::GetNumObjectsFound( cCurrentModuleObject );

		std::shared_ptr<ChillerConstCOP> thisChiller( new ChillerConstCOP() );

		for ( int ChillerNum = 1; ChillerNum <= NumConstCOPChillers; ++ChillerNum ) {
			InputProcessor::GetObjectItem( cCurrentModuleObject, ChillerNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			if ( objectName == DataIPShortCuts::cAlphaArgs( 1 ) ) {
				found = true;
			} else {
				continue;
			}

			GlobalNames::VerifyUniqueChillerName( cCurrentModuleObject, cAlphaArgs( 1 ), errFlag, cCurrentModuleObject + " Name" );
			if ( errFlag ) {
				ErrorsFound = true;
			}
			thisChiller->name = cAlphaArgs( 1 );
			thisChiller->NomCap = rNumericArgs( 1 );
			if ( rNumericArgs( 1 ) == 0.0 ) {
				ShowSevereError( "Invalid " + cNumericFieldNames( 1 ) + '=' + General::RoundSigDigits( rNumericArgs( 1 ), 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}
			thisChiller->COP = rNumericArgs( 2 );
			if ( rNumericArgs( 2 ) == 0.0 ) {
				ShowSevereError( "Invalid " + cNumericFieldNames( 2 ) + '=' + General::RoundSigDigits( rNumericArgs( 2 ), 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}

			//Set the Condenser Type from input
			if ( cAlphaArgs( 6 ) == "AIRCOOLED" ) {
				thisChiller->condenserType = ChillerCondenserType::AirCooled;
			} else if ( cAlphaArgs( 6 ) == "EVAPORATIVELYCOOLED" ) {
				thisChiller->condenserType = ChillerCondenserType::EvapCooled;
			} else if ( cAlphaArgs( 6 ) == "WATERCOOLED" ) {
				thisChiller->condenserType = ChillerCondenserType::WaterCooled;
			} else {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 6 ) + '=' + cAlphaArgs( 6 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}

			thisChiller->EvapVolFlowRate = rNumericArgs( 3 );
			if ( thisChiller->condenserType == ChillerCondenserType::AirCooled || thisChiller->condenserType == ChillerCondenserType::EvapCooled ) { // Condenser flow rate not used for these cond types
				thisChiller->CondVolFlowRate = 0.0011;
			} else {
				thisChiller->CondVolFlowRate = rNumericArgs( 4 );
			}
			thisChiller->SizFac = rNumericArgs( 5 );

			thisChiller->EvapInletNodeNum = NodeInputManager::GetOnlySingleNode( cAlphaArgs( 2 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Inlet, 1, DataLoopNode::ObjectIsNotParent );
			thisChiller->EvapOutletNodeNum = NodeInputManager::GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Outlet, 1, DataLoopNode::ObjectIsNotParent );
			BranchNodeConnections::TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 2 ), cAlphaArgs( 3 ), "Chilled Water Nodes" );

			if ( thisChiller->condenserType == ChillerCondenserType::AirCooled || thisChiller->condenserType == ChillerCondenserType::EvapCooled ) {
				// Connection not required for air or evap cooled condenser
				//If the condenser inlet is blank for air cooled and evap cooled condensers then supply a generic name
				//  since it is not used elsewhere for connection
				if ( lAlphaFieldBlanks( 4 ) ) {
					if ( len( cAlphaArgs( 1 ) ) < DataGlobals::MaxNameLength - 21 ) { // protect against long name leading to > 100 chars
						cAlphaArgs( 4 ) = cAlphaArgs( 1 ) + " CONDENSER INLET NODE";
					} else {
						cAlphaArgs( 4 ) = cAlphaArgs( 1 ).substr( 0, 79 ) + " CONDENSER INLET NODE";
					}
				}
				if ( lAlphaFieldBlanks( 5 ) ) {
					if ( len( cAlphaArgs( 1 ) ) < DataGlobals::MaxNameLength - 22 ) { // protect against long name leading to > 100 chars
						cAlphaArgs( 5 ) = cAlphaArgs( 1 ) + " CONDENSER OUTLET NODE";
					} else {
						cAlphaArgs( 5 ) = cAlphaArgs( 1 ).substr( 0, 78 ) + " CONDENSER OUTLET NODE";
					}
				}

				thisChiller->CondInletNodeNum = NodeInputManager::GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), DataLoopNode::NodeType_Air, DataLoopNode::NodeConnectionType_OutsideAirReference, 2, DataLoopNode::ObjectIsNotParent );
				bool Okay = true;
				OutAirNodeManager::CheckAndAddAirNodeNumber( thisChiller->CondInletNodeNum, Okay );
				if ( ! Okay ) {
					ShowWarningError( cCurrentModuleObject + ", Adding OutdoorAir:Node=" + cAlphaArgs( 4 ) );
				}

				thisChiller->CondOutletNodeNum = NodeInputManager::GetOnlySingleNode( cAlphaArgs( 5 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), DataLoopNode::NodeType_Air, DataLoopNode::NodeConnectionType_Outlet, 2, DataLoopNode::ObjectIsNotParent );
			} else if ( thisChiller->condenserType == WaterCooled ) {
				thisChiller->CondInletNodeNum = NodeInputManager::GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Inlet, 2, DataLoopNode::ObjectIsNotParent );
				thisChiller->CondOutletNodeNum = NodeInputManager::GetOnlySingleNode( cAlphaArgs( 5 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Outlet, 2, DataLoopNode::ObjectIsNotParent );
				BranchNodeConnections::TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 4 ), cAlphaArgs( 5 ), "Condenser Water Nodes" );
				//Condenser Inlet node name is necessary for Water Cooled
				if ( lAlphaFieldBlanks( 4 ) ) {
					ShowSevereError( "Invalid, " + cAlphaFieldNames( 4 ) + "is blank " );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				} else if ( lAlphaFieldBlanks( 5 ) ) {
					ShowSevereError( "Invalid, " + cAlphaFieldNames( 5 ) + "is blank " );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}
			} else {
				thisChiller->CondInletNodeNum = NodeInputManager::GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), DataLoopNode::NodeType_Unknown, DataLoopNode::NodeConnectionType_Inlet, 2, DataLoopNode::ObjectIsNotParent );
				thisChiller->CondOutletNodeNum = NodeInputManager::GetOnlySingleNode( cAlphaArgs( 5 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), DataLoopNode::NodeType_Unknown, DataLoopNode::NodeConnectionType_Outlet, 2, DataLoopNode::ObjectIsNotParent );
				BranchNodeConnections::TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 4 ), cAlphaArgs( 5 ), "Condenser (unknown?) Nodes" );
				//Condenser Inlet node name is necessary for Water Cooled
				if ( lAlphaFieldBlanks( 4 ) ) {
					ShowSevereError( "Invalid, " + cAlphaFieldNames( 4 ) + "is blank " );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				} else if ( lAlphaFieldBlanks( 5 ) ) {
					ShowSevereError( "Invalid, " + cAlphaFieldNames( 5 ) + "is blank " );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}
			}

			{ auto const SELECT_CASE_var( cAlphaArgs( 7 ) );
			if ( SELECT_CASE_var == "CONSTANTFLOW" ) {
				thisChiller->FlowMode = ChillerFlowMode::ConstantFlow;
			} else if ( SELECT_CASE_var == "VARIABLEFLOW" ) {
				thisChiller->FlowMode = ChillerFlowMode::LeavingSetPointModulated;
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"," );
				ShowContinueError( "Invalid " + cAlphaFieldNames( 7 ) + '=' + cAlphaArgs( 7 ) );
				ShowContinueError( "Key choice is now called \"LeavingSetpointModulated\" and the simulation continues" );
			} else if ( SELECT_CASE_var == "LEAVINGSETPOINTMODULATED" ) {
				thisChiller->FlowMode = ChillerFlowMode::LeavingSetPointModulated;
			} else if ( SELECT_CASE_var == "NOTMODULATED" ) {
				thisChiller->FlowMode = ChillerFlowMode::NotModulated;
			} else {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"," );
				ShowContinueError( "Invalid " + cAlphaFieldNames( 7 ) + '=' + cAlphaArgs( 7 ) );
				ShowContinueError( "Available choices are ConstantFlow, NotModulated, or LeavingSetpointModulated" );
				ShowContinueError( "Flow mode NotModulated is assumed and the simulation continues." );
				thisChiller->FlowMode = ChillerFlowMode::NotModulated;
			}}

			//   Basin heater power as a function of temperature must be greater than or equal to 0
			thisChiller->BasinHeaterPowerFTempDiff = rNumericArgs( 6 );
			if ( rNumericArgs( 6 ) < 0.0 ) {
				ShowSevereError( cCurrentModuleObject + ", \"" + thisChiller->name + "\" TRIM(cNumericFieldNames(6)) must be >= 0" );
				ErrorsFound = true;
			}

			thisChiller->BasinHeaterSetPointTemp = rNumericArgs( 7 );

			if ( thisChiller->BasinHeaterPowerFTempDiff > 0.0 ) {
				if ( NumNums < 7 ) {
					thisChiller->BasinHeaterSetPointTemp = 2.0;
				}
				if ( thisChiller->BasinHeaterSetPointTemp < 2.0 ) {
					ShowWarningError( cCurrentModuleObject + ":\"" + thisChiller->name + "\", " + cNumericFieldNames( 7 ) + " is less than 2 deg C. Freezing could occur." );
				}
			}

			if ( ! lAlphaFieldBlanks( 8 ) ) {
				thisChiller->BasinHeaterSchedulePtr = ScheduleManager::GetScheduleIndex( cAlphaArgs( 8 ) );
				if ( thisChiller->BasinHeaterSchedulePtr == 0 ) {
					ShowWarningError( cCurrentModuleObject + ", \"" + thisChiller->name + "\" TRIM(cAlphaFieldNames(8)) \"" + cAlphaArgs( 8 ) + "\" was not found. Basin heater operation will not be modeled and the simulation continues" );
				}
			}

			SetupOutputVariable( "Chiller Electric Power [W]", thisChiller->report.Power, "System", "Average", thisChiller->name );
			SetupOutputVariable( "Chiller Electric Energy [J]", thisChiller->report.Energy, "System", "Sum", thisChiller->name, _, "ELECTRICITY", "Cooling", _, "Plant" );

			SetupOutputVariable( "Chiller Evaporator Cooling Rate [W]", thisChiller->report.QEvap, "System", "Average", thisChiller->name );
			SetupOutputVariable( "Chiller Evaporator Cooling Energy [J]", thisChiller->report.EvapEnergy, "System", "Sum", thisChiller->name, _, "ENERGYTRANSFER", "CHILLERS", _, "Plant" );
			SetupOutputVariable( "Chiller Evaporator Inlet Temperature [C]", thisChiller->report.EvapInletTemp, "System", "Average", thisChiller->name );
			SetupOutputVariable( "Chiller Evaporator Outlet Temperature [C]", thisChiller->report.EvapOutletTemp, "System", "Average", thisChiller->name );
			SetupOutputVariable( "Chiller Evaporator Mass Flow Rate [kg/s]", thisChiller->report.Evapmdot, "System", "Average", thisChiller->name );
			SetupOutputVariable( "Chiller COP [W/W]", thisChiller->ActualCOP, "System", "Average", thisChiller->name );

			SetupOutputVariable( "Chiller Condenser Heat Transfer Rate [W]", thisChiller->report.QCond, "System", "Average", thisChiller->name );
			SetupOutputVariable( "Chiller Condenser Heat Transfer Energy [J]", thisChiller->report.CondEnergy, "System", "Sum", thisChiller->name, _, "ENERGYTRANSFER", "HEATREJECTION", _, "Plant" );

			//Condenser mass flow and outlet temp are valid for water cooled
			if ( thisChiller->condenserType == ChillerCondenserType::WaterCooled ) {
				SetupOutputVariable( "Chiller Condenser Inlet Temperature [C]", thisChiller->report.CondInletTemp, "System", "Average", thisChiller->name );
				SetupOutputVariable( "Chiller Condenser Outlet Temperature [C]", thisChiller->report.CondOutletTemp, "System", "Average", thisChiller->name );
				SetupOutputVariable( "Chiller Condenser Mass Flow Rate [kg/s]", thisChiller->report.Condmdot, "System", "Average", thisChiller->name );
			} else if ( thisChiller->condenserType == ChillerCondenserType::AirCooled ) {
				SetupOutputVariable( "Chiller Condenser Inlet Temperature [C]", thisChiller->report.CondInletTemp, "System", "Average", thisChiller->name );
			} else if ( thisChiller->condenserType == ChillerCondenserType::EvapCooled ) {
				SetupOutputVariable( "Chiller Condenser Inlet Temperature [C]", thisChiller->report.CondInletTemp, "System", "Average", thisChiller->name );
				if ( thisChiller->BasinHeaterPowerFTempDiff > 0.0 ) {
					SetupOutputVariable( "Chiller Basin Heater Electric Power [W]", thisChiller->report.BasinHeaterPower, "System", "Average", thisChiller->name );
					SetupOutputVariable( "Chiller Basin Heater Electric Energy [J]", thisChiller->report.BasinHeaterConsumption, "System", "Sum", thisChiller->name, _, "Electric", "CHILLERS", _, "Plant" );
				}
			}
			if ( DataGlobals::AnyEnergyManagementSystemInModel ) {
				SetupEMSInternalVariable( "Chiller Nominal Capacity", thisChiller->name, "[W]", thisChiller->NomCap );
			}
			break;
		}

		if ( found && !ErrorsFound ) {
			ConstCOPChiller.push_back( thisChiller );
			return thisChiller;
		} else {
			ShowFatalError( "GetConstCOPChillerInput: Errors getting input" );
			// add a dummy return here to hush up the compiler
			return nullptr;
		}

	}

	int ChillerConstCOP::performEveryTimeInit( const PlantLocation & calledFromLocation )
	{
		// assign parameters from the topology component
		auto & thisTopologyComponent = DataPlant::PlantLoop( calledFromLocation.LoopNum ).LoopSide( calledFromLocation.LoopSideNum ).Branch( calledFromLocation.BranchNum ).Comp( calledFromLocation.CompNum );
		this->curLoad = thisTopologyComponent.MyLoad;
		this->runFlag = thisTopologyComponent.ON;
		
		int EvapInletNode = this->EvapInletNodeNum;
		int EvapOutletNode = this->EvapOutletNodeNum;
		int CondInletNode = this->CondInletNodeNum;
		int CondOutletNode = this->CondOutletNodeNum;

		if ( ( this->FlowMode == LeavingSetPointModulated ) && ( this->ModulatedFlowSetToLoop ) ) {
			// fix for clumsy old input that worked because loop setpoint was spread.
			//  could be removed with transition, testing , model change, period of being obsolete.
			DataLoopNode::Node( this->EvapOutletNodeNum ).TempSetPoint = DataLoopNode::Node( DataPlant::PlantLoop( this->chwLocation.LoopNum ).TempSetPointNodeNum ).TempSetPoint;
			DataLoopNode::Node( this->EvapOutletNodeNum ).TempSetPointHi = DataLoopNode::Node( DataPlant::PlantLoop( this->chwLocation.LoopNum ).TempSetPointNodeNum ).TempSetPointHi;
		}

		Real64 mdot = 0.0;
		Real64 mdotCond = 0.0;
		if ( ( this->curLoad < 0.0 ) && this->runFlag ) {
			mdot = this->EvapMassFlowRateMax;
			mdotCond = this->CondMassFlowRateMax;
		}

		PlantUtilities::SetComponentFlowRate( mdot, EvapInletNode, EvapOutletNode, this->chwLocation.LoopNum, this->chwLocation.LoopSideNum, this->chwLocation.BranchNum, this->chwLocation.CompNum );
		if ( this->condenserType == ChillerCondenserType::WaterCooled ) {
			PlantUtilities::SetComponentFlowRate( mdotCond, CondInletNode, CondOutletNode, this->condLocation.LoopNum, this->condLocation.LoopSideNum, this->condLocation.BranchNum, this->condLocation.CompNum );
		}

		if ( this->condenserType == ChillerCondenserType::EvapCooled ) {
			BasinHeaterPower = 0.0;
		}

		return 0;
	}

	int ChillerConstCOP::performOneTimeInit( const PlantLocation & calledFromLocation )
	{

		// need to be careful that the one-time init, etc., is at the topology component level!!!!
		this->performEveryTimeInit( calledFromLocation );
		auto & thisTopologyComponent = DataPlant::PlantLoop( calledFromLocation.LoopNum ).
												  LoopSide( calledFromLocation.LoopSideNum ).
												  Branch( calledFromLocation.BranchNum ).
												  Comp( calledFromLocation.CompNum );

		// set topology information -- doing this once so we can interconnect right away
		bool err = false;
		DataPlant::ScanPlantLoopsForObject( this->name, DataPlant::TypeOf_Chiller_ConstCOP, this->chwLocation.LoopNum, this->chwLocation.LoopSideNum, this->chwLocation.BranchNum, this->chwLocation.CompNum, _, _, _, this->EvapInletNodeNum, _, err );
		if ( this->condenserType != ChillerCondenserType::AirCooled && this->condenserType != ChillerCondenserType::EvapCooled ) {
			DataPlant::ScanPlantLoopsForObject( this->name,DataPlant:: TypeOf_Chiller_ConstCOP, this->condLocation.LoopNum, this->condLocation.LoopSideNum, this->condLocation.BranchNum, this->condLocation.CompNum, _, _, _, this->EvapInletNodeNum, _, err );
			PlantUtilities::InterConnectTwoPlantLoopSides( this->chwLocation.LoopNum, this->chwLocation.LoopSideNum, this->condLocation.LoopNum, this->condLocation.LoopSideNum, DataPlant::TypeOf_Chiller_ConstCOP, true );
		}
		if ( err ) {
			ShowFatalError( "CalcConstCOPChillerModel: Program terminated due to previous condition(s)." );
			return 1; // so compiler understands we won't get past here
		}

		// set flow information and assign a setpoint if needed
		if ( this->FlowMode == ChillerFlowMode::ConstantFlow ) {
			// reset flow priority
			DataPlant::PlantLoop( this->chwLocation.LoopNum ).LoopSide( this->chwLocation.LoopSideNum ).Branch( this->chwLocation.BranchNum ).Comp( this->chwLocation.CompNum ).FlowPriority = DataPlant::LoopFlowStatus_NeedyIfLoopOn;
		} else if ( this->FlowMode == ChillerFlowMode::LeavingSetPointModulated ) {
			// reset flow priority
			DataPlant::PlantLoop( this->chwLocation.LoopNum ).LoopSide( this->chwLocation.LoopSideNum ).Branch( this->chwLocation.BranchNum ).Comp( this->chwLocation.CompNum ).FlowPriority = DataPlant::LoopFlowStatus_NeedyIfLoopOn;

			// check if setpoint on outlet node
			if ( ( DataLoopNode::Node( this->EvapOutletNodeNum ).TempSetPoint == DataLoopNode::SensedNodeFlagValue ) && ( DataLoopNode::Node( this->EvapOutletNodeNum ).TempSetPointHi == DataLoopNode::SensedNodeFlagValue ) ) {
				if ( ! DataGlobals::AnyEnergyManagementSystemInModel ) {
					if ( ! this->ModulatedFlowErrDone ) {
						ShowWarningError( "Missing temperature setpoint for LeavingSetpointModulated mode chiller named " + this->name );
						ShowContinueError( "  A temperature setpoint is needed at the outlet node of a chiller in variable flow mode, use a SetpointManager" );
						ShowContinueError( "  The overall loop setpoint will be assumed for chiller. The simulation continues ... " );
						this->ModulatedFlowErrDone = true;
					}
				} else {
					// need call to EMS to check node
					bool setpointErr = false; // but not really fatal yet, but should be.
					EMSManager::CheckIfNodeSetPointManagedByEMS( this->EvapOutletNodeNum, EMSManager::iTemperatureSetPoint, setpointErr );
					if ( setpointErr ) {
						if ( ! this->ModulatedFlowErrDone ) {
							ShowWarningError( "Missing temperature setpoint for LeavingSetpointModulated mode chiller named " + this->name );
							ShowContinueError( "  A temperature setpoint is needed at the outlet node of a chiller evaporator in variable flow mode" );
							ShowContinueError( "  use a Setpoint Manager to establish a setpoint at the chiller evaporator outlet node " );
							ShowContinueError( "  or use an EMS actuator to establish a setpoint at the outlet node " );
							ShowContinueError( "  The overall loop setpoint will be assumed for chiller. The simulation continues ... " );
							this->ModulatedFlowErrDone = true;
						}
					}
				}
				this->ModulatedFlowSetToLoop = true;
				DataLoopNode::Node( this->EvapOutletNodeNum ).TempSetPoint = DataLoopNode::Node( DataPlant::PlantLoop( this->chwLocation.LoopNum ).TempSetPointNodeNum ).TempSetPoint;
				DataLoopNode::Node( this->EvapOutletNodeNum ).TempSetPointHi = DataLoopNode::Node( DataPlant::PlantLoop( this->chwLocation.LoopNum ).TempSetPointNodeNum ).TempSetPointHi;
			}
		}

		// size the chiller as needed and assign the resulting min/max/opt capacities to the plant topology
		if ( calledFromLocation.LoopNum == this->chwLocation.LoopNum ) {
			this->sizeChiller();
			thisTopologyComponent.MinLoad = 0.0;
			thisTopologyComponent.MaxLoad = this->NomCap;
			thisTopologyComponent.OptLoad = this->NomCap;
		} else {
			thisTopologyComponent.MinLoad = 0.0;
			thisTopologyComponent.MaxLoad = 0.0;
			thisTopologyComponent.OptLoad = 0.0;
		}
		thisTopologyComponent.SizFac = this->SizFac;
		thisTopologyComponent.TempDesCondIn = 0.0;
		thisTopologyComponent.TempDesEvapOut = 0.0;
		return 0;
	}

	int ChillerConstCOP::performBeginEnvrnInit( const PlantLocation & EP_UNUSED(calledFromLocation) )
	{
		Real64 const TempDesCondIn( 25.0 ); // Design condenser inlet temp. C
		//Initialize critical Demand Side Variables at the beginning of each environment
		std::string const RoutineName = "ConstCOPChillerInit";
		if ( DataPlant::PlantFirstSizesOkayToFinalize ) {
			Real64 rho = FluidProperties::GetDensityGlycol( DataPlant::PlantLoop( this->chwLocation.LoopNum ).FluidName, DataGlobals::InitConvTemp, DataPlant::PlantLoop( this->chwLocation.LoopNum ).FluidIndex, RoutineName );
			this->EvapMassFlowRateMax = this->EvapVolFlowRate * rho;
			PlantUtilities::InitComponentNodes( 0.0, this->EvapMassFlowRateMax, this->EvapInletNodeNum, this->EvapOutletNodeNum, this->chwLocation.LoopNum, this->chwLocation.LoopSideNum, this->chwLocation.BranchNum, this->chwLocation.CompNum );

			auto & condInletNode = DataLoopNode::Node( this->CondInletNodeNum );
			auto & condOutletNode = DataLoopNode::Node( this->CondOutletNodeNum );
			//init maximum available condenser flow rate
			if ( this->condenserType == ChillerCondenserType::WaterCooled ) {
				condInletNode.Temp = TempDesCondIn;
				rho = FluidProperties::GetDensityGlycol( DataPlant::PlantLoop( this->condLocation.LoopNum ).FluidName, DataGlobals::InitConvTemp, DataPlant::PlantLoop( this->condLocation.LoopNum ).FluidIndex, RoutineName );
				this->CondMassFlowRateMax = rho * this->CondVolFlowRate;
				PlantUtilities::InitComponentNodes( 0.0, this->CondMassFlowRateMax, this->CondInletNodeNum, this->CondOutletNodeNum, this->condLocation.LoopNum, this->condLocation.LoopSideNum, this->condLocation.BranchNum, this->condLocation.CompNum );
			} else { // air or evap-air
				condInletNode.MassFlowRate = this->CondVolFlowRate * Psychrometrics::PsyRhoAirFnPbTdbW( DataEnvironment::StdBaroPress, TempDesCondIn, 0.0, RoutineName );
				condOutletNode.MassFlowRate = condInletNode.MassFlowRate;
				condInletNode.MassFlowRateMaxAvail = condInletNode.MassFlowRate;
				condInletNode.MassFlowRateMax = condInletNode.MassFlowRate;
				condOutletNode.MassFlowRateMax = condInletNode.MassFlowRate;
				condInletNode.MassFlowRateMinAvail = 0.0;
				condInletNode.MassFlowRateMin = 0.0;
				condOutletNode.MassFlowRateMinAvail = 0.0;
				condOutletNode.MassFlowRateMin = 0.0;
			}
		}
		return 0;
	}

	int ChillerConstCOP::performFirstHVACInit( const PlantLocation & EP_UNUSED(calledFromLocation) ){return 0;}

	int ChillerConstCOP::simulate( const PlantLocation & calledFromLocation, bool const & FirstHVACIteration )
	{
		if ( calledFromLocation.LoopNum == this->chwLocation.LoopNum ) {
			// Calculate Load
			// IF MinPlr, MaxPlr, OptPlr are not defined, assume min = 0, max=opt=Nomcap
			this->calcChiller();
			this->updateChiller();
		} else if ( calledFromLocation.LoopNum == this->condLocation.LoopNum ) {
			PlantUtilities::UpdateChillerComponentCondenserSide( this->condLocation.LoopNum, this->condLocation.LoopSideNum, DataPlant::TypeOf_Chiller_ConstCOP, this->CondInletNodeNum, this->CondOutletNodeNum, this->report.Qcond, this->report.CondInletTemp, this->report.CondOutletTemp, this->report.Condmdot, FirstHVACIteration );
		}
		return 0;
	}
	int ChillerConstCOP::sizeChiller(){return 0;}
	int ChillerConstCOP::calcChiller(){return 0;}
	int ChillerConstCOP::updateChiller(){return 0;}
}

}
