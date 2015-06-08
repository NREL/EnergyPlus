#include <memory>

#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/string.functions.hh>
#include <ObjexxFCL/gio.hh>

#include <BranchNodeConnections.hh>
#include <PlantChillers/ChillerBase.hh> // would like to remove this...
#include <DataBranchAirLoopPlant.hh>
#include <DataEnvironment.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataSizing.hh>
#include <EMSManager.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <GlobalNames.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutAirNodeManager.hh>
#include <OutputProcessor.hh>
#include <OutputReportPredefined.hh>
#include <PlantChillers/ChillerConstCOP.hh>
#include <PlantComponent.hh>
#include <PlantLocation.hh>
#include <PlantUtilities.hh>
#include <Psychrometrics.hh>
#include <ReportSizingManager.hh>
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
			SetupOutputVariable( "Chiller Evaporator Mass Flow Rate [kg/s]", thisChiller->report.EvapMassFlowRate, "System", "Average", thisChiller->name );
			SetupOutputVariable( "Chiller COP [W/W]", thisChiller->ActualCOP, "System", "Average", thisChiller->name );

			SetupOutputVariable( "Chiller Condenser Heat Transfer Rate [W]", thisChiller->report.QCond, "System", "Average", thisChiller->name );
			SetupOutputVariable( "Chiller Condenser Heat Transfer Energy [J]", thisChiller->report.CondEnergy, "System", "Sum", thisChiller->name, _, "ENERGYTRANSFER", "HEATREJECTION", _, "Plant" );

			//Condenser mass flow and outlet temp are valid for water cooled
			if ( thisChiller->condenserType == ChillerCondenserType::WaterCooled ) {
				SetupOutputVariable( "Chiller Condenser Inlet Temperature [C]", thisChiller->report.CondInletTemp, "System", "Average", thisChiller->name );
				SetupOutputVariable( "Chiller Condenser Outlet Temperature [C]", thisChiller->report.CondOutletTemp, "System", "Average", thisChiller->name );
				SetupOutputVariable( "Chiller Condenser Mass Flow Rate [kg/s]", thisChiller->report.CondMassFlowRate, "System", "Average", thisChiller->name );
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

	int ChillerConstCOP::performFirstHVACInit( const PlantLocation & EP_UNUSED(calledFromLocation) )
	{
		return 0;
	}

	int ChillerConstCOP::performInitLoopEquip( const PlantLocation & calledFromLocation ) {
		auto & thisTopologyComponent = DataPlant::PlantLoop( calledFromLocation.LoopNum ).
												  LoopSide( calledFromLocation.LoopSideNum ).
												  Branch( calledFromLocation.BranchNum ).
												  Comp( calledFromLocation.CompNum );
		// do standard initialization
		this->performEveryTimeInit( calledFromLocation );
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


	int ChillerConstCOP::simulate( const PlantLocation & calledFromLocation, bool const & FirstHVACIteration )
	{
		if ( calledFromLocation.LoopNum == this->chwLocation.LoopNum ) {
			// Calculate Load
			this->calcChiller( calledFromLocation );
			this->updateChiller();
		} else if ( calledFromLocation.LoopNum == this->condLocation.LoopNum ) {
			PlantUtilities::UpdateChillerComponentCondenserSide( this->condLocation.LoopNum, this->condLocation.LoopSideNum, DataPlant::TypeOf_Chiller_ConstCOP, this->CondInletNodeNum, this->CondOutletNodeNum, this->report.Qcond, this->report.CondInletTemp, this->report.CondOutletTemp, this->report.CondMassFlowRate, FirstHVACIteration );
		}
		return 0;
	}

	int ChillerConstCOP::sizeChiller()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   March 2008
		//       MODIFIED       November 2013 Daeho Kang, add component sizing table entries
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing Constabt COP Chiller Components for which capacities and flow rates
		// have not been specified in the input.

		// METHODOLOGY EMPLOYED:
		// Obtains evaporator flow rate from the plant sizing array. Calculates nominal capacity from
		// the evaporator flow rate and the chilled water loop design delta T. The condenser flow rate
		// is calculated from the nominal capacity, the COP, and the condenser loop design delta T.

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "SizeConstCOPChiller" );

		Real64 EvapVolFlowRateUser = 0.0; // Hardsized evaporator flow for reporting
		Real64 CondVolFlowRateUser = 0.0; // Hardsized condenser flow for reporting

		bool ErrorsFound = false;
		Real64 tmpEvapVolFlowRate = this->EvapVolFlowRate;
		Real64 tmpCondVolFlowRate = this->CondVolFlowRate;
		Real64 tmpNomCap = this->NomCap;
		Real64 NomCapUser = 0.0;
		
		int PltSizCondNum = 0; // Plant Sizing index for condenser loop
		if ( this->condenserType == ChillerCondenserType::WaterCooled ) {
			PltSizCondNum = DataPlant::PlantLoop( this->condLocation.LoopNum ).PlantSizNum;
		}

		int PltSizNum = DataPlant::PlantLoop( this->chwLocation.LoopNum ).PlantSizNum;

		// size nominal capacity
		if ( PltSizNum > 0 ) {
			if ( DataSizing::PlantSizData( PltSizNum ).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow ) {
				Real64 rho = FluidProperties::GetDensityGlycol( DataPlant::PlantLoop( this->chwLocation.LoopNum ).FluidName, DataGlobals::InitConvTemp, DataPlant::PlantLoop( this->chwLocation.LoopNum ).FluidIndex, RoutineName );
				Real64 Cp = FluidProperties::GetSpecificHeatGlycol( DataPlant::PlantLoop( this->chwLocation.LoopNum ).FluidName, DataGlobals::InitConvTemp, DataPlant::PlantLoop( this->chwLocation.LoopNum ).FluidIndex, RoutineName );
				tmpNomCap = Cp * rho * DataSizing::PlantSizData( PltSizNum ).DeltaT * DataSizing::PlantSizData( PltSizNum ).DesVolFlowRate * this->SizFac;
				if ( ! this->NomCapWasAutoSized ) tmpNomCap = this->NomCap;
			} else {
				if ( this->NomCapWasAutoSized ) tmpNomCap = 0.0;
			}
			if ( DataPlant::PlantFirstSizesOkayToFinalize ) {
				if ( this->NomCapWasAutoSized ) {
					this->NomCap = tmpNomCap;
					if ( DataPlant::PlantFinalSizesOkayToReport ) {
						ReportSizingManager::ReportSizingOutput( "Chiller:ConstantCOP", this->name, "Design Size Nominal Capacity [W]", tmpNomCap );
					}
					if (DataPlant:: PlantFirstSizesOkayToReport ) {
						ReportSizingManager::ReportSizingOutput( "Chiller:ConstantCOP", this->name, "Initial Design Size Nominal Capacity [W]", tmpNomCap );
					}
				} else { // Hard-size with sizing data
					if ( this->NomCap > 0.0 && tmpNomCap > 0.0 ) {
						NomCapUser = this->NomCap;
						if ( DataPlant::PlantFinalSizesOkayToReport ) {
							ReportSizingManager::ReportSizingOutput( "Chiller:ConstantCOP", this->name, "Design Size Nominal Capacity [W]", tmpNomCap, "User-Specified Nominal Capacity [W]", NomCapUser );
							if ( DataGlobals::DisplayExtraWarnings ) {
								if ( ( std::abs( tmpNomCap - NomCapUser ) / NomCapUser ) > DataSizing::AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeChillerConstantCOP: Potential issue with equipment sizing for " + this->name );
									ShowContinueError( "User-Specified Nominal Capacity of " + General::RoundSigDigits( NomCapUser, 2 ) + " [W]" );
									ShowContinueError( "differs from Design Size Nominal Capacity of " + General::RoundSigDigits( tmpNomCap, 2 ) + " [W]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
						tmpNomCap = NomCapUser;
					}
				}
			}
		} else {
			if ( this->NomCapWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize ) {
				ShowSevereError( "Autosizing of Constant COP Chiller nominal capacity requires a loop Sizing:Plant object" );
				ShowContinueError( "Occurs in Chiller:ConstantCOP object=" + this->name );
				ErrorsFound = true;
			}
			if ( ! this->NomCapWasAutoSized && DataPlant::PlantFinalSizesOkayToReport && ( this->NomCap > 0.0 ) ) {
				ReportSizingManager::ReportSizingOutput( "Chiller:ConstantCOP", this->name, "User-Specified Nominal Capacity [W]", this->NomCap );
			}
		}

		// size evap flow rate
		if ( PltSizNum > 0 ) {
			if ( DataSizing::PlantSizData( PltSizNum ).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow ) {
				tmpEvapVolFlowRate = DataSizing::PlantSizData( PltSizNum ).DesVolFlowRate * this->SizFac;
				if ( ! this->EvapVolFlowRateWasAutoSized ) tmpEvapVolFlowRate = this->EvapVolFlowRate;
			} else {
				if ( this->EvapVolFlowRateWasAutoSized ) tmpEvapVolFlowRate = 0.0;
			}
			if ( DataPlant::PlantFirstSizesOkayToFinalize ) {
				if ( this->EvapVolFlowRateWasAutoSized ) {
					this->EvapVolFlowRate = tmpEvapVolFlowRate;
					if ( DataPlant::PlantFinalSizesOkayToReport ) {
						ReportSizingManager::ReportSizingOutput( "Chiller:ConstantCOP", this->name, "Design Size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate );
					}
					if ( DataPlant::PlantFirstSizesOkayToReport ) {
						ReportSizingManager::ReportSizingOutput( "Chiller:ConstantCOP", this->name, "Initial Design Size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate );
					}
				} else {
					if ( this->EvapVolFlowRate > 0.0 && tmpEvapVolFlowRate > 0.0 ) {
						EvapVolFlowRateUser = this->EvapVolFlowRate;
						if ( DataPlant::PlantFinalSizesOkayToReport ) {
							ReportSizingManager::ReportSizingOutput( "Chiller:ConstantCOP", this->name, "Design Size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate, "User-Specified Design Chilled Water Flow Rate [m3/s]", EvapVolFlowRateUser );
							if ( DataGlobals::DisplayExtraWarnings ) {
								if ( ( std::abs( tmpEvapVolFlowRate - EvapVolFlowRateUser ) / EvapVolFlowRateUser ) > DataSizing::AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeChillerConstantCOP: Potential issue with equipment sizing for " + this->name );
									ShowContinueError( "User-Specified Design Chilled Water Flow Rate of " + General::RoundSigDigits( EvapVolFlowRateUser, 5 ) + " [m3/s]" );
									ShowContinueError( "differs from Design Size Design Chilled Water Flow Rate of " + General::RoundSigDigits( tmpEvapVolFlowRate, 5 ) + " [m3/s]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
						tmpEvapVolFlowRate = EvapVolFlowRateUser;
					}
				}
			}
		} else {
			if ( this->EvapVolFlowRateWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize ) {
				ShowSevereError( "Autosizing of Constant COP Chiller evap flow rate requires a loop Sizing:Plant object" );
				ShowContinueError( "Occurs in Chiller:ConstantCOP object=" + this->name );
				ErrorsFound = true;
			}
			if ( ! this->EvapVolFlowRateWasAutoSized && DataPlant::PlantFinalSizesOkayToReport && ( this->EvapVolFlowRate > 0.0 ) ) {
					ReportSizingManager::ReportSizingOutput( "Chiller:ConstantCOP", this->name, "User-Specified Design Chilled Water Flow Rate [m3/s]", this->EvapVolFlowRate );
			}
		}

		PlantUtilities::RegisterPlantCompDesignFlow( this->EvapInletNodeNum, tmpEvapVolFlowRate );

		if ( this->condenserType == ChillerCondenserType::WaterCooled ) {
			if ( PltSizCondNum > 0 && PltSizNum > 0 ) {
				if ( DataSizing::PlantSizData( PltSizNum ).DesVolFlowRate >= DataHVACGlobals::SmallWaterVolFlow && tmpNomCap > 0.0 ) {
					Real64 rho = FluidProperties::GetDensityGlycol( DataPlant::PlantLoop( this->condLocation.LoopNum ).FluidName, 29.44, DataPlant::PlantLoop( this->condLocation.LoopNum ).FluidIndex, RoutineName );

					Real64 Cp = FluidProperties::GetSpecificHeatGlycol( DataPlant::PlantLoop( this->condLocation.LoopNum ).FluidName, 29.44, DataPlant::PlantLoop( this->condLocation.LoopNum ).FluidIndex, RoutineName );
					tmpCondVolFlowRate = tmpNomCap * ( 1.0 + 1.0 / this->COP ) / ( DataSizing::PlantSizData( PltSizCondNum ).DeltaT * Cp * rho );
					if ( ! this->CondVolFlowRateWasAutoSized ) tmpCondVolFlowRate = this->CondVolFlowRate;
				} else {
					if ( this->CondVolFlowRateWasAutoSized ) tmpCondVolFlowRate = 0.0;
				}
				if ( DataPlant::PlantFirstSizesOkayToFinalize ) {
					if ( this->CondVolFlowRateWasAutoSized ) {
						this->CondVolFlowRate = tmpCondVolFlowRate;
						if ( DataPlant::PlantFinalSizesOkayToReport ) {
							ReportSizingManager::ReportSizingOutput( "Chiller:ConstantCOP", this->name, "Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate );
						}
						if ( DataPlant::PlantFirstSizesOkayToReport ) {
							ReportSizingManager::ReportSizingOutput( "Chiller:ConstantCOP", this->name, "Initial Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate );
						}
					} else {
						if ( this->CondVolFlowRate > 0.0 && tmpCondVolFlowRate > 0.0 ) {
							CondVolFlowRateUser = this->CondVolFlowRate;
							if ( DataPlant::PlantFinalSizesOkayToReport ) {
								ReportSizingManager::ReportSizingOutput( "Chiller:ConstantCOP", this->name, "Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate, "User-Specified Design Condenser Water Flow Rate [m3/s]", CondVolFlowRateUser );
								if ( DataGlobals::DisplayExtraWarnings ) {
									if ( ( std::abs( tmpCondVolFlowRate - CondVolFlowRateUser ) / CondVolFlowRateUser ) > DataSizing::AutoVsHardSizingThreshold ) {
										ShowMessage( "SizeChillerConstantCOP: Potential issue with equipment sizing for " + this->name );
										ShowContinueError( "User-Specified Design Condenser Water Flow Rate of " + General::RoundSigDigits( CondVolFlowRateUser, 5 ) + " [m3/s]" );
										ShowContinueError( "differs from Design Size Design Condenser Water Flow Rate of " + General::RoundSigDigits( tmpCondVolFlowRate, 5 ) + " [m3/s]" );
										ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
										ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
									}
								}
							}
							tmpCondVolFlowRate = CondVolFlowRateUser;
						}
					}
				}
			} else {
				if ( this->CondVolFlowRateWasAutoSized && DataPlant::PlantFirstSizesOkayToFinalize ) {
					ShowSevereError( "Autosizing of Constant COP Chiller condenser flow rate requires a condenser" );
					ShowContinueError( "loop Sizing:Plant object" );
					ShowContinueError( "Occurs in Chiller:ConstantCOP object=" + this->name );
					ErrorsFound = true;
				}
				if ( ! this->CondVolFlowRateWasAutoSized && DataPlant::PlantFinalSizesOkayToReport && ( this->CondVolFlowRate > 0.0 ) ) {
						ReportSizingManager::ReportSizingOutput( "Chiller:ConstantCOP", this->name, "User-Specified Design Condenser Water Flow Rate [m3/s]", this->CondVolFlowRate );
				}
			}
			// save the design condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
			PlantUtilities::RegisterPlantCompDesignFlow( this->CondInletNodeNum, tmpCondVolFlowRate );
		}

		if ( ErrorsFound ) {
			ShowFatalError( "Preceding sizing errors cause program termination" );
		}

		//create predefined report
		if ( DataPlant::PlantFinalSizesOkayToReport ) {
			OutputReportPredefined::PreDefTableEntry( OutputReportPredefined::pdchMechType, this->name, "Chiller:ConstantCOP" );
			OutputReportPredefined::PreDefTableEntry( OutputReportPredefined::pdchMechNomEff, this->name, this->COP );
			OutputReportPredefined::PreDefTableEntry( OutputReportPredefined::pdchMechNomCap, this->name, this->NomCap );
		}

		return 0;
	}

	int ChillerConstCOP::calcChiller( const PlantLocation & EP_UNUSED(calledFromLocation) )
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dan Fisher
		//       DATE WRITTEN   Sept. 1998
		//       MODIFIED       Richard Liesen Nov-Dec 2001; Jan 2002,
		//                      Chandan Sharma, FSEC, February 2010, Added basin heater

		static gio::Fmt OutputFormat( "(F6.2)" );
		static std::string OutputChar; // character string for warning messages
		static const std::string RoutineName = "CalcConstCOPChiller";

		auto & EvapInletNode = DataLoopNode::Node( this->EvapInletNodeNum );
		auto & EvapOutletNode = DataLoopNode::Node( this->EvapOutletNodeNum );
		auto & CondInletNode = DataLoopNode::Node( this->CondInletNodeNum );
		//auto & CondOutletNode = DataLoopNode::Node( this->CondOutletNodeNum );
		auto & chwPlantLoop = DataPlant::PlantLoop( this->chwLocation.LoopNum );
		auto & chwComponent = chwPlantLoop.LoopSide( this->chwLocation.LoopSideNum ).Branch( this->chwLocation.BranchNum ).Comp( this->chwLocation.CompNum );
		Real64 EvapDeltaTemp = 0.0;
		
		//set module level chiller inlet and temperature variables
		int setPointNodeNum;
		if ( ( this->FlowMode == ChillerFlowMode::LeavingSetPointModulated ) || ( chwComponent.CurOpSchemeType == DataPlant::CompSetPtBasedSchemeType ) || ( EvapOutletNode.TempSetPoint != DataLoopNode::SensedNodeFlagValue ) ) {
			setPointNodeNum = this->EvapOutletNodeNum;
		} else {
			setPointNodeNum = chwPlantLoop.TempSetPointNodeNum;
		}
		Real64 TempEvapOutSetPoint;
		if ( chwPlantLoop.LoopDemandCalcScheme == DataPlant::SingleSetPoint ) {
			TempEvapOutSetPoint = DataLoopNode::Node( setPointNodeNum ).TempSetPoint;
		} else {
			TempEvapOutSetPoint = DataLoopNode::Node( setPointNodeNum ).TempSetPointHi;
		}
		//Real64 EvapDeltaTemp = std::abs( EvapInletNode.Temp - TempEvapOutSetPoint );
		this->report.EvapInletTemp = EvapInletNode.Temp;

		//If no component demand, or chiller OFF, or Chiller type set to 'Passive' by free
		//cooling heat exchanger, then set condenser side flow and heat transfer rates set to zero
		if ( ( chwComponent.MyLoad >= 0.0 ) || ( !chwComponent.ON ) ) {

			//If Chiller load is 0 or greater or chiller is not running then leave the subroutine.  Before leaving,
			//if the component control is SERIESACTIVE we set the component flow to inlet flow so that
			//flow resolver will not shut down the branch
			
			// DIFFs possibility: trying to just request zero instead of the below block...the setcompflowrate function should be doing this same logic
			this->report.EvapMassFlowRate = 0.0;
			PlantUtilities::SetComponentFlowRate( this->report.EvapMassFlowRate, this->EvapInletNodeNum, this->EvapOutletNodeNum, this->chwLocation.LoopNum, this->chwLocation.LoopSideNum, this->chwLocation.BranchNum, this->chwLocation.CompNum );
			if ( this->condenserType == ChillerCondenserType::WaterCooled ) {
				this->report.CondMassFlowRate = 0.0;
				PlantUtilities::SetComponentFlowRate( this->report.CondMassFlowRate, this->CondInletNodeNum, this->CondOutletNodeNum, this->condLocation.LoopNum, this->condLocation.LoopSideNum, this->condLocation.BranchNum, this->condLocation.CompNum  );
			}
			//if ( chwComponent.FlowCtrl == DataPlant::ControlType_SeriesActive || chwPlantLoop.LoopSide( this->chwLocation.LoopSideNum ).FlowLock == 1 ) {
				//EvapMassFlowRate = Node( EvapInletNode ).MassFlowRate;
			//} else {
				//EvapMassFlowRate = 0.0;
				//SetComponentFlowRate( EvapMassFlowRate, EvapInletNode, EvapOutletNode, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum );
			//}
			//if ( this->CondenserType == WaterCooled ) {
				//if ( PlantLoop( this->CDLoopNum ).LoopSide( this->CDLoopSideNum ).Branch( this->CDBranchNum ).Comp( this->CDCompNum ).FlowCtrl == ControlType_SeriesActive ) {
					//CondMassFlowRate = Node( CondInletNode ).MassFlowRate;
				//} else {
					//CondMassFlowRate = 0.0;
					//SetComponentFlowRate( CondMassFlowRate, CondInletNode, CondOutletNode, this->CDLoopNum, this->CDLoopSideNum, this->CDBranchNum, this->CDCompNum );
				//}
			//}

			this->report.EvapOutletTemp = EvapInletNode.Temp;
			this->report.CondOutletTemp = CondInletNode.Temp;

			this->report.Power = 0.0;
			this->report.QEvap = 0.0;
			this->report.QCond = 0.0;
			this->report.Energy = 0.0;
			this->report.EvapEnergy = 0.0;
			this->report.CondEnergy = 0.0;

			if ( this->condenserType == ChillerCondenserType::EvapCooled ) {
				CalcBasinHeaterPower( this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, BasinHeaterPower );
			}
			this->PrintMessage = false;
			return 0;
		}

		//   calculate end time of current time step
		Real64 CurrentEndTime = DataGlobals::CurrentTime + DataHVACGlobals::SysTimeElapsed;

		//   Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
		//   Wait for next time step to print warnings. If simulation iterates, print out
		//   the warning for the last iteration only. Must wait for next time step to accomplish this.
		//   If a warning occurs and the simulation down shifts, the warning is not valid.
		if ( CurrentEndTime > this->CurrentEndTimeLast && DataHVACGlobals::TimeStepSys >= this->TimeStepSysLast ) {
			if ( this->PrintMessage ) {
				++this->MsgErrorCount;
				//       Show single warning and pass additional info to ShowRecurringWarningErrorAtEnd
				if ( this->MsgErrorCount < 2 ) {
					ShowWarningError( this->MsgBuffer1 + '.' );
					ShowContinueError( this->MsgBuffer2 );
				} else {
					ShowRecurringWarningErrorAtEnd( this->MsgBuffer1 + " error continues.", this->ErrCount1, this->MsgDataLast, this->MsgDataLast, _, "[C]", "[C]" );
				}
			}
		}

		//   save last system time step and last end time of current time step (used to determine if warning is valid)
		this->TimeStepSysLast = DataHVACGlobals::TimeStepSys;
		this->CurrentEndTimeLast = CurrentEndTime;

		//otherwise the chiller is running...

		if ( this->condenserType == ChillerCondenserType::AirCooled ) { //Condenser inlet temp = outdoor temp
			CondInletNode.Temp = CondInletNode.OutAirDryBulb;
			//  Warn user if entering condenser temperature falls below 0C
			if ( CondInletNode.Temp < 0.0 && ! DataGlobals::WarmupFlag ) {
				this->PrintMessage = true;
				gio::write( OutputChar, OutputFormat ) << CondInletNode.Temp;
				this->MsgBuffer1 = "CalcConstCOPChillerModel - Chiller:ConstantCOP \"" + this->name + "\" - Air Cooled Condenser Inlet Temperature below 0C";
				this->MsgBuffer2 = "... Outdoor Dry-bulb Condition = " + OutputChar + " C. Occurrence info = " + DataEnvironment::EnvironmentName + ", " + DataEnvironment::CurMnDy + ' ' + General::CreateSysTimeIntervalString();
				this->MsgDataLast = CondInletNode.Temp;
			} else {
				this->PrintMessage = false;
			}
		} else if ( this->condenserType == ChillerCondenserType::EvapCooled ) { //Condenser inlet temp = (outdoor wet bulb)
			CondInletNode.Temp = CondInletNode.OutAirWetBulb;
			//  Warn user if evap condenser wet bulb temperature falls below 10C
			if ( CondInletNode.Temp < 10.0 && ! DataGlobals::WarmupFlag ) {
				this->PrintMessage = true;
				gio::write( OutputChar, OutputFormat ) << CondInletNode.Temp;
				this->MsgBuffer1 = "CalcConstCOPChillerModel - Chiller:ConstantCOP \"" + this->name + "\" - Evap Cooled Condenser Inlet Temperature below 10C";
				this->MsgBuffer2 = "... Outdoor Wet-bulb Condition = " + OutputChar + " C. Occurrence info = " + DataEnvironment::EnvironmentName + ", " + DataEnvironment::CurMnDy + ' ' + General::CreateSysTimeIntervalString();
				this->MsgDataLast = CondInletNode.Temp;
			} else {
				this->PrintMessage = false;
			}
		} // End of the Air Cooled/Evap Cooled Logic block

		// If not air or evap cooled then set to the condenser node that is attached to a cooling tower
		this->report.CondInletTemp = CondInletNode.Temp;

		//Set condenser flow rate
		if ( this->condenserType == ChillerCondenserType::WaterCooled ) {
			this->report.CondMassFlowRate = this->CondMassFlowRateMax;
			PlantUtilities::SetComponentFlowRate( this->report.CondMassFlowRate, this->CondInletNodeNum, this->CondOutletNodeNum, this->condLocation.LoopNum, this->condLocation.LoopSideNum, this->condLocation.BranchNum, this->condLocation.CompNum );
			PlantUtilities::PullCompInterconnectTrigger( this->chwLocation.LoopNum, this->chwLocation.LoopSideNum, this->chwLocation.BranchNum, this->chwLocation.CompNum, this->CondMassFlowIndex, this->condLocation.LoopNum, this->condLocation.LoopSideNum, DataPlant::CriteriaType_MassFlowRate, this->report.CondMassFlowRate );
			if ( this->report.CondMassFlowRate < DataBranchAirLoopPlant::MassFlowTolerance ) return 0;
		}

		// If FlowLock is True, the new resolved mdot is used to update Power, QEvap, Qcond, and condenser side outlet temperature.

		Real64 Cp = FluidProperties::GetSpecificHeatGlycol( chwPlantLoop.FluidName, EvapInletNode.Temp, chwPlantLoop.FluidIndex, RoutineName );

		if ( chwPlantLoop.LoopSide( this->chwLocation.LoopSideNum ).FlowLock == 0 ) {
			this->PossibleSubcooling = false;
			this->report.QEvap = std::abs( chwComponent.MyLoad );
			this->report.Power = std::abs( chwComponent.MyLoad ) / this->COP;

			// Either set the flow to the Constant value or caluclate the flow for the variable volume
			if ( ( this->FlowMode == ConstantFlow ) || ( this->FlowMode == NotModulated ) ) {

				// Start by assuming max (design) flow
				this->report.EvapMassFlowRate = this->EvapMassFlowRateMax;
				// Use SetComponentFlowRate to decide actual flow
				PlantUtilities::SetComponentFlowRate( this->report.EvapMassFlowRate, this->EvapInletNodeNum, this->EvapOutletNodeNum, this->chwLocation.LoopNum, this->chwLocation.LoopSideNum, this->chwLocation.BranchNum, this->chwLocation.CompNum );
				// Evaluate delta temp based on actual flow rate
				if ( this->report.EvapMassFlowRate != 0.0 ) {
					EvapDeltaTemp = this->report.QEvap / this->report.EvapMassFlowRate / Cp;
				} else {
					EvapDeltaTemp = 0.0;
				}
				// Evaluate outlet temp based on delta
				this->report.EvapOutletTemp = EvapInletNode.Temp - EvapDeltaTemp;

			} else if ( this->FlowMode == LeavingSetPointModulated ) {

				// Calculate the Delta Temp from the inlet temp to the chiller outlet setpoint
				if ( chwPlantLoop.LoopDemandCalcScheme == DataPlant::SingleSetPoint ) {
					EvapDeltaTemp = std::abs( EvapInletNode.Temp - EvapOutletNode.TempSetPoint );
				} else if ( chwPlantLoop.LoopDemandCalcScheme == DataPlant::DualSetPointDeadBand ) {
					EvapDeltaTemp = std::abs( EvapInletNode.Temp - EvapOutletNode.TempSetPointHi );
				}

				if ( EvapDeltaTemp > DataPlant::DeltaTempTol ) {
					this->report.EvapMassFlowRate = std::abs( this->report.QEvap / Cp / EvapDeltaTemp );
					if ( ( this->report.EvapMassFlowRate - this->EvapMassFlowRateMax ) > DataBranchAirLoopPlant::MassFlowTolerance ) this->PossibleSubcooling = true;
					//Check to see if the Maximum is exceeded, if so set to maximum
					this->report.EvapMassFlowRate = min( this->EvapMassFlowRateMax, this->report.EvapMassFlowRate );
					// Use SetComponentFlowRate to decide actual flow
					PlantUtilities::SetComponentFlowRate( this->report.EvapMassFlowRate, this->EvapInletNodeNum, this->EvapOutletNodeNum, this->chwLocation.LoopNum, this->chwLocation.LoopSideNum, this->chwLocation.BranchNum, this->chwLocation.CompNum );
					if ( chwPlantLoop.LoopDemandCalcScheme == DataPlant::SingleSetPoint ) {
						this->report.EvapOutletTemp = EvapOutletNode.TempSetPoint;
					} else if ( chwPlantLoop.LoopDemandCalcScheme == DataPlant::DualSetPointDeadBand ) {
						this->report.EvapOutletTemp = EvapOutletNode.TempSetPointHi;
					}
				} else {
					// Try to request zero flow
					this->report.EvapMassFlowRate = 0.0;
					// Use SetComponentFlowRate to decide actual flow
					PlantUtilities::SetComponentFlowRate( this->report.EvapMassFlowRate, this->EvapInletNodeNum, this->EvapOutletNodeNum, this->chwLocation.LoopNum, this->chwLocation.LoopSideNum, this->chwLocation.BranchNum, this->chwLocation.CompNum );
					// No deltaT since component is not running
					this->report.EvapOutletTemp = EvapInletNode.Temp;

				}
			} //End of Constant or Variable Flow If Block for FlowLock = 0 (or making a flow request)
		} else { // If FlowLock is True

			this->report.EvapMassFlowRate = EvapInletNode.MassFlowRate;
			PlantUtilities::SetComponentFlowRate( this->report.EvapMassFlowRate, this->EvapInletNodeNum, this->EvapOutletNodeNum, this->chwLocation.LoopNum, this->chwLocation.LoopSideNum, this->chwLocation.BranchNum, this->chwLocation.CompNum );
			//   Some other component set the flow to 0. No reason to continue with calculations.
			if ( this->report.EvapMassFlowRate == 0.0 ) {
				//MyLoad = 0.0; we don't need to report MyLoad back as zero; it was just a reference to a local variable inside SimPlantEquip
				if ( this->condenserType == ChillerCondenserType::EvapCooled ) {
					CalcBasinHeaterPower( this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, BasinHeaterPower );
				}
				this->PrintMessage = false;
				return 0;
			}

			//Recalculate the Delta Temp
			if ( this->PossibleSubcooling ) {
				this->report.QEvap = std::abs( chwComponent.MyLoad );
				EvapDeltaTemp = this->report.QEvap / this->report.EvapMassFlowRate / Cp;
				this->report.EvapOutletTemp = EvapInletNode.Temp - EvapDeltaTemp;
				if ( this->report.EvapOutletTemp < EvapOutletNode.TempMin ) {
					this->report.EvapOutletTemp = EvapOutletNode.TempMin;
					EvapDeltaTemp = EvapInletNode.Temp - this->report.EvapOutletTemp;
					this->report.QEvap = this->report.EvapMassFlowRate * Cp * EvapDeltaTemp;
				}
			} else {
				EvapDeltaTemp = EvapInletNode.Temp - TempEvapOutSetPoint;
				//Calculate the evaporator heat transfer at the specified flow which could have changed
				//  in the Flow Resolution step.
				this->report.QEvap = std::abs( this->report.EvapMassFlowRate * Cp * EvapDeltaTemp );
				this->report.EvapOutletTemp = TempEvapOutSetPoint;
			}
			
			//Check that the Evap outlet temp honors both plant loop temp low limit and also the chiller low limit
			if ( this->report.EvapOutletTemp < EvapOutletNode.TempMin ) {
				if ( ( EvapInletNode.Temp - EvapOutletNode.TempMin ) > DataPlant::DeltaTempTol ) {
					this->report.EvapOutletTemp = EvapOutletNode.TempMin;
					EvapDeltaTemp = EvapInletNode.Temp - this->report.EvapOutletTemp;
					this->report.QEvap = this->report.EvapMassFlowRate * Cp * EvapDeltaTemp;
				} else {
					this->report.EvapOutletTemp = EvapInletNode.Temp;
					EvapDeltaTemp = EvapInletNode.Temp - this->report.EvapOutletTemp;
					this->report.QEvap = this->report.EvapMassFlowRate * Cp * EvapDeltaTemp;
				}
			}
			
			// If load exceeds the distributed load set to the distributed load
			if ( this->report.QEvap > std::abs( chwComponent.MyLoad ) ) {
				if ( this->report.EvapMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance ) {
					this->report.QEvap = std::abs( chwComponent.MyLoad );
					EvapDeltaTemp = this->report.QEvap / this->report.EvapMassFlowRate / Cp;
					this->report.EvapOutletTemp = EvapInletNode.Temp - EvapDeltaTemp;
				} else {
					this->report.QEvap = 0.0;
					this->report.EvapOutletTemp = EvapInletNode.Temp;
				}
			}

			// Checks QEvaporator on the basis of the machine limits.
			if ( this->report.QEvap > this->NomCap ) {
				if ( this->report.EvapMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance ) {
					this->report.QEvap = this->NomCap;
					EvapDeltaTemp = this->report.QEvap / this->report.EvapMassFlowRate / Cp;
					this->report.EvapOutletTemp = EvapInletNode.Temp - EvapDeltaTemp;
				} else {
					this->report.QEvap = 0.0;
					this->report.EvapOutletTemp = EvapInletNode.Temp;
				}
			}
			
			//Calculate the Power consumption of the Const COP chiller which is a simplified calculation
			this->report.Power = this->report.QEvap / this->COP;
			if ( this->report.EvapMassFlowRate == 0.0 ) {
				this->report.QEvap = 0.0;
				this->report.EvapOutletTemp = EvapInletNode.Temp;
				this->report.Power = 0.0;
				this->PrintMessage = false;
			}
			if ( this->report.QEvap == 0.0 && this->condenserType == ChillerCondenserType::EvapCooled ) {
				CalcBasinHeaterPower( this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, BasinHeaterPower );
			}

		} //This is the end of the FlowLock Block

		//QCondenser is calculated the same for each type, but the power consumption should be different
		//  depending on the performance coefficients used for the chiller model.
		this->report.QCond = this->report.Power + this->report.QEvap;

		if ( this->condenserType == ChillerCondenserType::WaterCooled ) {
			Real64 CpCond = FluidProperties::GetSpecificHeatGlycol( chwPlantLoop.FluidName, this->report.CondInletTemp, chwPlantLoop.FluidIndex, RoutineName );
			if ( this->report.CondMassFlowRate > DataBranchAirLoopPlant::MassFlowTolerance ) {
				this->report.CondOutletTemp = this->report.QCond / this->report.CondMassFlowRate / CpCond + this->report.CondInletTemp;
			} else {
				ShowSevereError( "CalcConstCOPChillerModel: Condenser flow = 0, for CONST COP Chiller=" + this->name );
				ShowContinueErrorTimeStamp( "" );
			}
		} else { // Air Cooled or Evap Cooled
			//  Set condenser outlet temp to condenser inlet temp for Air Cooled or Evap Cooled
			//  since there is no CondMassFlowRate and would divide by zero
			this->report.CondOutletTemp = this->report.CondInletTemp;
		}

		//Calculate Energy
		this->report.CondEnergy = this->report.QCond * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
		this->report.Energy = this->report.Power * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
		this->report.EvapEnergy = this->report.QEvap * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

		//check for problems BG 9/12/06 (deal with observed negative energy results)
		if ( this->report.Energy < 0.0 ) { // there is a serious problem

			if ( this->condenserType == ChillerCondenserType::WaterCooled ) {
				// first check for run away condenser loop temps (only reason yet to be observed for this?)
				if ( this->report.CondInletTemp > 70.0 ) {
					ShowSevereError( "CalcConstCOPChillerModel: Condenser loop inlet temperatures over 70.0 C for ConstCOPChiller=" + this->name );
					ShowContinueErrorTimeStamp( "" );
					ShowContinueError( "Condenser loop water temperatures are too high at" + General::RoundSigDigits( this->report.CondInletTemp, 2 ) );
					ShowContinueError( "Check input for condenser plant loop, especially cooling tower" );
					ShowContinueError( "Evaporator inlet temperature: " + General::RoundSigDigits( EvapInletNode.Temp, 2 ) );
					ShowFatalError( "Program Terminates due to previous error condition" );
				}
			}
			// If makes it here, set limits, chiller can't have negative energy/power
			// proceeding silently for now but may want to throw error here
			this->report.Power = 0.0;
			this->report.Energy = 0.0;

		}
		return 0;
	}

	int ChillerConstCOP::updateChiller()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Dan Fisher
		//       DATE WRITTEN:    October 1998

		Real64 ReportingConstant = DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

		auto & EvapInletNode = DataLoopNode::Node( this->EvapInletNodeNum );
		auto & EvapOutletNode = DataLoopNode::Node(  this->EvapOutletNodeNum );
		auto & CondInletNode = DataLoopNode::Node( this->CondInletNodeNum );
		auto & CondOutletNode = DataLoopNode::Node( this->CondOutletNodeNum );

		if ( this->curLoad >= 0.0 || ! this->runFlag ) { //Chiller not running so pass inlet states to outlet states
			this->report.Power = 0.0;
			this->report.QEvap = 0.0;
			this->report.QCond = 0.0;
			this->report.Energy = 0.0;
			this->report.EvapEnergy = 0.0;
			this->report.CondEnergy = 0.0;
			this->report.CondInletTemp = CondInletNode.Temp;
			this->report.EvapInletTemp = EvapInletNode.Temp;
			this->report.CondOutletTemp = CondOutletNode.Temp;  // DIFFS HERE
			this->report.EvapOutletTemp = EvapOutletNode.Temp;
			this->ActualCOP = 0.0;
			if ( this->condenserType == ChillerCondenserType::EvapCooled ) {
				this->report.BasinHeaterConsumption = BasinHeaterPower * ReportingConstant;
			}

			//set outlet node temperatures
			EvapOutletNode.Temp = EvapInletNode.Temp;
			CondOutletNode.Temp = CondInletNode.Temp;

		} else {
			//this->report.Power = Power;
			//this->report.QEvap = QEvaporator;
			//this->report.QCond = QCondenser;
			//this->report.Energy = Energy;
			//this->report.EvapEnergy = EvaporatorEnergy;
			//this->report.CondEnergy = CondenserEnergy;
			this->report.CondInletTemp = CondInletNode.Temp;
			this->report.EvapInletTemp = EvapInletNode.Temp;
			//this->report.CondOutletTemp = CondOutletTemp;
			//this->report.EvapOutletTemp = EvapOutletTemp;
			if ( this->report.Power != 0.0 ) {
				this->ActualCOP = this->report.QEvap / this->report.Power;
			} else {
				this->ActualCOP = 0.0;
			}
			if ( this->condenserType == ChillerCondenserType::EvapCooled ) {
				this->report.BasinHeaterConsumption = BasinHeaterPower * ReportingConstant;
			}

			//set outlet node temperatures
			EvapOutletNode.Temp = this->report.EvapOutletTemp;
			CondOutletNode.Temp = this->report.CondOutletTemp;
		}

		return 0;
	}
}

}
