// EnergyPlus, Copyright (c) 1996-2018, The Board of Trustees of the University of Illinois,
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy), Oak Ridge
// National Laboratory, managed by UT-Battelle, Alliance for Sustainable Energy, LLC, and other
// contributors. All rights reserved.
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
//     similar designation, without the U.S. Department of Energy's prior written consent.
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

#include <string>
#include <vector>

#include <BranchNodeConnections.hh>
#include <DataGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <FluidProperties.hh>
#include <InputProcessing/InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <PlantComponent.hh>
#include <PlantUtilities.hh>
#include <UtilityRoutines.hh>
#include <WaterToWaterHeatPumpEIR.hh>

namespace EnergyPlus {
	namespace EIRWaterToWaterHeatPumps {

		bool getInputsWWHP(true);
		std::vector<EIRWaterToWaterHeatPump> eir_wwhp;

		void EIRWaterToWaterHeatPump::clear_state() {
			getInputsWWHP = true;
			eir_wwhp.clear();
		}

		int EIRWaterToWaterHeatPump::add(int a, int b) {
			return a + b;
		}

		void EIRWaterToWaterHeatPump::simulate(const EnergyPlus::PlantLocation &calledFromLocation,
											   bool const FirstHVACIteration,
											   Real64 &CurLoad,
											   bool const RunFlag) {
			std::string const routineName = "WaterToWaterHeatPumpEIR::simulate";

			// std::cout << RunFlag << ", " << CurLoad << std::endl;
			if (!DataGlobals::KickOffSimulation) {
				int i = 1;
			}

			if (calledFromLocation.loopNum == this->sourceSideLocation.loopNum) { // condenser side
				PlantUtilities::UpdateChillerComponentCondenserSide(this->sourceSideLocation.loopNum,
																	this->sourceSideLocation.loopSideNum,
																	this->plantTypeOfNum,
																	this->sourceSideNodes.inlet,
																	this->sourceSideNodes.outlet,
																	this->sourceSideHeatTransfer,
																	this->sourceSideInletTemp,
																	this->sourceSideOutletTemp,
																	this->sourceSideMassFlowRate,
																	FirstHVACIteration);
				return;
			}

			this->running = RunFlag;
			if (!this->running) {
				this->loadSideMassFlowRate = 0.0;
				this->sourceSideMassFlowRate = 0.0;
				PlantUtilities::SetComponentFlowRate(this->loadSideMassFlowRate,
													 this->loadSideNodes.inlet,
													 this->loadSideNodes.outlet,
													 this->loadSideLocation.loopNum,
													 this->loadSideLocation.loopSideNum,
													 this->loadSideLocation.branchNum,
													 this->loadSideLocation.compNum);
				PlantUtilities::SetComponentFlowRate(this->sourceSideMassFlowRate,
													 this->sourceSideNodes.inlet,
													 this->sourceSideNodes.outlet,
													 this->sourceSideLocation.loopNum,
													 this->sourceSideLocation.loopSideNum,
													 this->sourceSideLocation.branchNum,
													 this->sourceSideLocation.compNum);
				PlantUtilities::PullCompInterconnectTrigger(this->loadSideLocation.loopNum,
															this->loadSideLocation.loopSideNum,
															this->loadSideLocation.branchNum,
															this->loadSideLocation.compNum,
															this->condMassFlowRateTriggerIndex,
															this->sourceSideLocation.loopNum,
															this->sourceSideLocation.loopSideNum,
															DataPlant::CriteriaType_MassFlowRate,
															this->sourceSideMassFlowRate);
				// Set flows if the heat pump is running
			} else { // the heat pump must run
				this->loadSideMassFlowRate = this->loadSideDesignMassFlowRate;
				this->sourceSideMassFlowRate = this->sourceSideDesignMassFlowRate;
				PlantUtilities::SetComponentFlowRate(this->loadSideMassFlowRate,
													 this->loadSideNodes.inlet,
													 this->loadSideNodes.outlet,
													 this->loadSideLocation.loopNum,
													 this->loadSideLocation.loopSideNum,
													 this->loadSideLocation.branchNum,
													 this->loadSideLocation.compNum);
				PlantUtilities::SetComponentFlowRate(this->sourceSideMassFlowRate,
													 this->sourceSideNodes.inlet,
													 this->sourceSideNodes.outlet,
													 this->sourceSideLocation.loopNum,
													 this->sourceSideLocation.loopSideNum,
													 this->sourceSideLocation.branchNum,
													 this->sourceSideLocation.compNum);

				// if there's no flow in one, try to turn the entire heat pump off
				if (this->loadSideMassFlowRate <= 0.0 || this->sourceSideMassFlowRate <= 0.0) {

					this->loadSideMassFlowRate = 0.0;
					this->sourceSideMassFlowRate = 0.0;
					this->running = false;

					PlantUtilities::SetComponentFlowRate(this->loadSideMassFlowRate,
														 this->loadSideNodes.inlet,
														 this->loadSideNodes.outlet,
														 this->loadSideLocation.loopNum,
														 this->loadSideLocation.loopSideNum,
														 this->loadSideLocation.branchNum,
														 this->loadSideLocation.compNum);
					PlantUtilities::SetComponentFlowRate(this->sourceSideMassFlowRate,
														 this->sourceSideNodes.inlet,
														 this->sourceSideNodes.outlet,
														 this->sourceSideLocation.loopNum,
														 this->sourceSideLocation.loopSideNum,
														 this->sourceSideLocation.branchNum,
														 this->sourceSideLocation.compNum);
					PlantUtilities::PullCompInterconnectTrigger(this->loadSideLocation.loopNum,
																this->loadSideLocation.loopSideNum,
																this->loadSideLocation.branchNum,
																this->loadSideLocation.compNum,
																this->condMassFlowRateTriggerIndex,
																this->sourceSideLocation.loopNum,
																this->sourceSideLocation.loopSideNum,
																DataPlant::CriteriaType_MassFlowRate,
																this->sourceSideMassFlowRate);
				}
				PlantUtilities::PullCompInterconnectTrigger(this->loadSideLocation.loopNum,
															this->loadSideLocation.loopSideNum,
															this->loadSideLocation.branchNum,
															this->loadSideLocation.compNum,
															this->condMassFlowRateTriggerIndex,
															this->sourceSideLocation.loopNum,
															this->sourceSideLocation.loopSideNum,
															DataPlant::CriteriaType_MassFlowRate,
															this->sourceSideMassFlowRate);
			}
			this->loadSideInletTemp = DataLoopNode::Node(this->loadSideNodes.inlet).Temp;
			this->sourceSideInletTemp = DataLoopNode::Node(this->sourceSideNodes.inlet).Temp;

			if (this->loadSideMassFlowRate > 0 && this->sourceSideMassFlowRate > 0) {
				// for today, assume the heat transfer could be rejected perfectly and in full
				this->loadSideHeatTransfer = CurLoad;
				Real64 Cp = FluidProperties::GetSpecificHeatGlycol(
						DataPlant::PlantLoop(this->loadSideLocation.loopNum).FluidName,
						DataGlobals::CWInitConvTemp,
						DataPlant::PlantLoop(this->loadSideLocation.loopNum).FluidIndex,
						routineName);
				this->loadSideOutletTemp =
						this->loadSideInletTemp + this->loadSideHeatTransfer / (this->loadSideMassFlowRate * Cp);

				// assume a dummy value for power usage
				this->powerUsage = this->loadSideHeatTransfer / 10.0;

				// then you can calculate source side impacts
				this->sourceSideHeatTransfer = this->loadSideHeatTransfer - this->powerUsage;
				this->sourceSideOutletTemp =
						this->sourceSideInletTemp - this->sourceSideHeatTransfer / (this->sourceSideMassFlowRate * Cp);

			} else {
				this->loadSideHeatTransfer = 0.0;
				this->loadSideOutletTemp = this->loadSideInletTemp;
				this->powerUsage = 0.0;
				this->sourceSideHeatTransfer = 0.0;
				this->sourceSideOutletTemp = this->sourceSideInletTemp;
			}

			// update nodes
			DataLoopNode::Node(this->loadSideNodes.outlet).Temp = this->loadSideOutletTemp;
			DataLoopNode::Node(this->sourceSideNodes.outlet).Temp = this->sourceSideOutletTemp;
		}

		void EIRWaterToWaterHeatPump::onInitLoopEquip(const PlantLocation &EP_UNUSED(calledFromLocation)) {
			std::string const routineName = "initWaterToWaterHeatPumpEIR";

			if (this->oneTimeInit) {
				// setup output variables
				SetupOutputVariable("EIR WWHP Load Side Heat Transfer", OutputProcessor::Unit::W,
									this->loadSideHeatTransfer, "System", "Average", this->name);
				SetupOutputVariable("EIR WWHP Source Side Heat Transfer", OutputProcessor::Unit::W,
									this->sourceSideHeatTransfer, "System", "Average", this->name);
				SetupOutputVariable("EIR WWHP Load Side Inlet Temperature", OutputProcessor::Unit::C,
									this->loadSideInletTemp, "System", "Average", this->name);
				SetupOutputVariable("EIR WWHP Load Side Outlet Temperature", OutputProcessor::Unit::C,
									this->loadSideOutletTemp, "System", "Average", this->name);
				SetupOutputVariable("EIR WWHP Source Side Inlet Temperature", OutputProcessor::Unit::C,
									this->sourceSideInletTemp, "System", "Average", this->name);
				SetupOutputVariable("EIR WWHP Source Side Outlet Temperature", OutputProcessor::Unit::C,
									this->sourceSideOutletTemp, "System", "Average", this->name);
				SetupOutputVariable("EIR WWHP Power Usage", OutputProcessor::Unit::W, this->powerUsage, "System",
									"Average", this->name);
				//SetupOutputVariable("EIR WWHP Running", OutputProcessor::Unit::None, eir.running, "System", "Average", eir.name);

				// find this component on the plant
				bool errFlag = false;
				PlantUtilities::ScanPlantLoopsForObject(this->name,
														this->plantTypeOfNum,
														this->loadSideLocation.loopNum,
														this->loadSideLocation.loopSideNum,
														this->loadSideLocation.branchNum,
														this->loadSideLocation.compNum,
														_,
														_,
														_,
														this->loadSideNodes.inlet,
														_,
														errFlag);

				if (this->loadSideLocation.loopSideNum != DataPlant::SupplySide) { // throw error
					ShowSevereError(routineName + ": Invalid connections for " +
									DataPlant::ccSimPlantEquipTypes(this->plantTypeOfNum) + " name = \"" +
									this->name + "\"");
					ShowContinueError("The load side connections are not on the Supply Side of a plant loop");
					errFlag = true;
				}

				PlantUtilities::ScanPlantLoopsForObject(this->name,
														this->plantTypeOfNum,
														this->sourceSideLocation.loopNum,
														this->sourceSideLocation.loopSideNum,
														this->sourceSideLocation.branchNum,
														this->sourceSideLocation.compNum,
														_,
														_,
														_,
														this->sourceSideNodes.inlet,
														_,
														errFlag);

				if (this->sourceSideLocation.loopSideNum != DataPlant::DemandSide) { // throw error
					ShowSevereError(routineName + ": Invalid connections for " +
									DataPlant::ccSimPlantEquipTypes(this->plantTypeOfNum) + " name = \"" +
									this->name + "\"");
					ShowContinueError("The source side connections are not on the Demand Side of a plant loop");
					errFlag = true;
				}

				// make sure it is not the same loop on both sides.
				if (this->loadSideLocation.loopNum ==
					this->sourceSideLocation.loopNum) { // user is being too tricky, don't allow
					ShowSevereError(routineName + ": Invalid connections for " +
									DataPlant::ccSimPlantEquipTypes(this->plantTypeOfNum) + " name = \"" +
									this->name + "\"");
					ShowContinueError("The load and source sides need to be on different loops.");
					errFlag = true;
				} else {

					PlantUtilities::InterConnectTwoPlantLoopSides(this->loadSideLocation.loopNum,
																  this->loadSideLocation.loopSideNum,
																  this->sourceSideLocation.loopNum,
																  this->sourceSideLocation.loopSideNum,
																  this->plantTypeOfNum,
																  true);
				}

				if (errFlag) {
					ShowFatalError(routineName + ": Program terminated due to previous condition(s).");
				}
				this->oneTimeInit = false;
			} // plant setup

			if (DataGlobals::BeginEnvrnFlag && this->envrnInit && DataPlant::PlantFirstSizesOkayToFinalize) {
				Real64 rho = FluidProperties::GetDensityGlycol(
						DataPlant::PlantLoop(this->loadSideLocation.loopNum).FluidName,
						DataGlobals::InitConvTemp,
						DataPlant::PlantLoop(this->loadSideLocation.loopNum).FluidIndex,
						routineName);
				this->loadSideDesignMassFlowRate = rho * this->loadSideDesignVolFlowRate;
				PlantUtilities::InitComponentNodes(0.0,
												   this->loadSideDesignMassFlowRate,
												   this->loadSideNodes.inlet,
												   this->loadSideNodes.outlet,
												   this->loadSideLocation.loopNum,
												   this->loadSideLocation.loopSideNum,
												   this->loadSideLocation.branchNum,
												   this->loadSideLocation.compNum);

				rho = FluidProperties::GetDensityGlycol(
						DataPlant::PlantLoop(this->sourceSideLocation.loopNum).FluidName,
						DataGlobals::InitConvTemp,
						DataPlant::PlantLoop(this->sourceSideLocation.loopNum).FluidIndex,
						routineName);
				this->sourceSideDesignMassFlowRate = rho * this->sourceSideDesignVolFlowRate;
				PlantUtilities::InitComponentNodes(0.0,
												   this->sourceSideDesignMassFlowRate,
												   this->sourceSideNodes.inlet,
												   this->sourceSideNodes.outlet,
												   this->sourceSideLocation.loopNum,
												   this->sourceSideLocation.loopSideNum,
												   this->sourceSideLocation.branchNum,
												   this->sourceSideLocation.compNum);
				this->envrnInit = false;
			}
			if (!DataGlobals::BeginEnvrnFlag) {
				this->envrnInit = true;
			}

		}

		PlantComponent *EIRWaterToWaterHeatPump::factory(int plantTypeOfNum, std::string objectName) {
			if (getInputsWWHP) {
				EIRWaterToWaterHeatPump::processInputForEIRWWHP();
				getInputsWWHP = false;
			}

			for (auto &wwhp : eir_wwhp) {
				if (wwhp.name == objectName && wwhp.plantTypeOfNum == plantTypeOfNum) {
					return &wwhp;
				}
			}

			ShowFatalError("EIR_WWHP factory: Error getting inputs for wwhp named: " + objectName);
			return nullptr;
		}

		void EIRWaterToWaterHeatPump::processInputForEIRWWHP() {
			using namespace DataIPShortCuts;

			bool errorsFound = false;

			cCurrentModuleObject = "HeatPump:WaterToWater:EIR:Heating";
			int numWWHP = inputProcessor->getNumObjectsFound(cCurrentModuleObject);
			if (numWWHP > 0) {
				for (int wwhpNum = 1; wwhpNum <= numWWHP; ++wwhpNum) {
					EIRWaterToWaterHeatPump thisWWHP;
					thisWWHP.plantTypeOfNum = DataPlant::TypeOf_HeatPumpEIRHeating;
					int NumAlphas, NumNumbers, IOStatus;
					inputProcessor->getObjectItem(cCurrentModuleObject,
												  wwhpNum,
												  cAlphaArgs,
												  NumAlphas,
												  rNumericArgs,
												  NumNumbers,
												  IOStatus,
												  lNumericFieldBlanks,
												  _,
												  cAlphaFieldNames,
												  cNumericFieldNames);
					// HeatPump:WaterToWater:EIR:Heating,
					//   A1,  \field Name
					//   A2,  \field Load Side Inlet Node Name
					//   A3,  \field Load Side Outlet Node Name
					//   A4,  \field Source Side Inlet Node Name
					//   A5,  \field Source Side Outlet Node Name
					//   N1,  \field Load Side Design Flow Rate
					//   N2;  \field Source Side Design Flow Rate
					thisWWHP.name = cAlphaArgs(1);
					std::string loadSideInletNodeName = cAlphaArgs(2);
					std::string loadSideOutletNodeName = cAlphaArgs(3);
					std::string sourceSideInletNodeName = cAlphaArgs(4);
					std::string sourceSideOutletNodeName = cAlphaArgs(5);
					thisWWHP.loadSideDesignVolFlowRate = rNumericArgs(1);
					thisWWHP.sourceSideDesignVolFlowRate = rNumericArgs(2);
					int const flowPath1 = 1, flowPath2 = 2;
					thisWWHP.loadSideNodes.inlet = NodeInputManager::GetOnlySingleNode(loadSideInletNodeName,
																					   errorsFound,
																					   cCurrentModuleObject,
																					   thisWWHP.name,
																					   DataLoopNode::NodeType_Water,
																					   DataLoopNode::NodeConnectionType_Inlet,
																					   flowPath1,
																					   DataLoopNode::ObjectIsNotParent);
					thisWWHP.loadSideNodes.outlet = NodeInputManager::GetOnlySingleNode(loadSideOutletNodeName,
																						errorsFound,
																						cCurrentModuleObject,
																						thisWWHP.name,
																						DataLoopNode::NodeType_Water,
																						DataLoopNode::NodeConnectionType_Outlet,
																						flowPath1,
																						DataLoopNode::ObjectIsNotParent);
					thisWWHP.sourceSideNodes.inlet = NodeInputManager::GetOnlySingleNode(sourceSideInletNodeName,
																						 errorsFound,
																						 cCurrentModuleObject,
																						 thisWWHP.name,
																						 DataLoopNode::NodeType_Water,
																						 DataLoopNode::NodeConnectionType_Inlet,
																						 flowPath2,
																						 DataLoopNode::ObjectIsNotParent);
					thisWWHP.sourceSideNodes.outlet = NodeInputManager::GetOnlySingleNode(sourceSideOutletNodeName,
																						  errorsFound,
																						  cCurrentModuleObject,
																						  thisWWHP.name,
																						  DataLoopNode::NodeType_Water,
																						  DataLoopNode::NodeConnectionType_Outlet,
																						  flowPath2,
																						  DataLoopNode::ObjectIsNotParent);
					BranchNodeConnections::TestCompSet(
							cCurrentModuleObject, thisWWHP.name, loadSideInletNodeName, loadSideOutletNodeName,
							"Hot Water Nodes");
					BranchNodeConnections::TestCompSet(
							cCurrentModuleObject, thisWWHP.name, sourceSideInletNodeName, sourceSideOutletNodeName,
							"Condenser Water Nodes");

					eir_wwhp.push_back(thisWWHP);
				}
			}
		}

	} // namespace EIRWaterToWaterHeatPumps
} // namespace EnergyPlus
