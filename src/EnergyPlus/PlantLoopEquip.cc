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
#include <algorithm>

// ObjexxFCL Headers

// EnergyPlus Headers
#include <PlantLoopEquip.hh>
#include <BaseboardRadiator.hh>
#include <Boilers.hh>
#include <BoilerSteam.hh>
#include <ChillerAbsorption.hh>
#include <ChillerElectricEIR.hh>
#include <ChillerExhaustAbsorption.hh>
#include <ChillerGasAbsorption.hh>
#include <ChillerIndirectAbsorption.hh>
#include <ChillerReformulatedEIR.hh>
#include <CondenserLoopTowers.hh>
#include <CTElectricGenerator.hh>
#include <DataGlobals.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <EvaporativeFluidCoolers.hh>
#include <FluidCoolers.hh>
#include <FuelCellElectricGenerator.hh>
#include <GroundHeatExchangers.hh>
#include <HeatPumpWaterToWaterCOOLING.hh>
#include <HeatPumpWaterToWaterHEATING.hh>
#include <HeatPumpWaterToWaterSimple.hh>
#include <HVACVariableRefrigerantFlow.hh>
#include <HWBaseboardRadiator.hh>
#include <ICEngineElectricGenerator.hh>
#include <IceThermalStorage.hh>
#include <MicroCHPElectricGenerator.hh>
#include <MicroturbineElectricGenerator.hh>
#include <OutsideEnergySources.hh>
#include <PhotovoltaicThermalCollectors.hh>
#include <PipeHeatTransfer.hh>
#include <Pipes.hh>
#include <PlantCentralGSHP.hh>
#include <PlantChillers.hh>
#include <PlantComponentTemperatureSources.hh>
#include <PlantHeatExchangerFluidToFluid.hh>
#include <PlantLoadProfile.hh>
#include <PlantLocation.hh>
#include <PlantPipingSystemsManager.hh>
#include <PlantValves.hh>
#include <PondGroundHeatExchanger.hh>
#include <Pumps.hh>
#include <RefrigeratedCase.hh>
#include <ScheduleManager.hh>
#include <SolarCollectors.hh>
#include <SteamBaseboardRadiator.hh>
#include <SurfaceGroundHeatExchanger.hh>
#include <UserDefinedComponents.hh>
#include <UtilityRoutines.hh>
#include <WaterCoils.hh>
#include <WaterThermalTanks.hh>
#include <WaterUse.hh>

namespace EnergyPlus {

namespace PlantLoopEquip {

	// MODULE INFORMATION:
	//       AUTHOR         Sankaranarayanan K P
	//       DATE WRITTEN   July 2005
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This module contains subroutine that calls the required component for simulation. The components are selected
	// using a CASE statement.

	// METHODOLOGY EMPLOYED:
	// Needs description, as appropriate.

	// REFERENCES: none

	// OTHER NOTES: none

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataPlant;
	using DataLoopNode::Node;

	// Data
	// SUBROUTINE SPECIFICATION

	// MODULE SUBROUTINES

	// Functions

	void
	SimPlantEquip(
		int const LoopNum, // loop counter
		int const LoopSideNum, // loop counter
		int const BranchNum,
		int const Num,
		bool const FirstHVACIteration, // TRUE if First iteration of simulation
		bool & InitLoopEquip,
		bool const GetCompSizFac // Tells component routine to return the component sizing fraction
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dan Fisher
		//       DATE WRITTEN   July 1998
		//       MODIFIED       June 2000  -Brandon Anderson
		//                             Changed to Group Similar Components.  Components will
		//                         be defined by ComponentType:SpecificComponent.
		//                         The colon will act as the type delimeter, So all
		//                         components of one type will be grouped. ex.(Boilers,Chillers)
		//                       May 2003 - Simon Rees
		//                         Added initial loop to force free cooling chiller etc to be
		//                         simulated before other components.
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine calls the appropriate routines to simulate
		// the equipment on the plant.

		// METHODOLOGY EMPLOYED:
		// This subroutine employs a rule-based
		// scheme to operate the plant equipment simulation without
		// requiring a detailed flow network solver.  The scheme is based
		// on several restrictive assumptions which may be relaxed when
		// a more detailed solution technique is developed.  The current
		// assumptions are:
		//    1.   All loop cooling/heating equipment is connected
		//         in parallel.
		//    2.   Only one circulation pump may be specified per loop.
		//    3.   The circulation pump must be specified first in the
		//         simulation order and is assumed to be connected in
		//         series with the cooling/heating equipment.
		//    4.   The Circ. pump determines the maximum flow rate for
		//         the loop.
		// The scheme is valid only for Part Load based plant equipment
		// models (currently the only type implemented).  Each equipment
		// simulation updates its outlet node temperature, estimates its
		// flow rate and returns a remaining loop demand which is passed
		// on to the other available equipment.

		// NOTE: All Equipment return the index of their lists during "InitLoopEquip"
		// as a time reduction measure.  Specific ifs are set to catch those modules that don't.
		// If you add a module or new equipment type, you must set up this structure.

		// REFERENCES:
		// na

		// Using/Aliasing
		using ScheduleManager::GetCurrentScheduleValue;
		using Boilers::SimBoiler;
		using WaterThermalTanks::SimWaterThermalTank;
		using ChillerAbsorption::SimBLASTAbsorber;
		using ChillerIndirectAbsorption::SimIndirectAbsorber;
		using ChillerGasAbsorption::SimGasAbsorber;
		using ChillerExhaustAbsorption::SimExhaustAbsorber;
		using PlantChillers::SimChiller;
		using ChillerElectricEIR::SimElectricEIRChiller;
		using ChillerReformulatedEIR::SimReformulatedEIRChiller;
		using HeatPumpWaterToWaterHEATING::SimHPWatertoWaterHEATING;
		using HeatPumpWaterToWaterCOOLING::SimHPWatertoWaterCOOLING;
		using HeatPumpWaterToWaterSimple::SimHPWatertoWaterSimple;
		using OutsideEnergySources::SimOutsideEnergy;
		using Pumps::SimPumps;

		using PlantHeatExchangerFluidToFluid::SimFluidHeatExchanger;
		using CondenserLoopTowers::SimTowers;
		using FluidCoolers::SimFluidCoolers;
		using EvaporativeFluidCoolers::SimEvapFluidCoolers;
		using BoilerSteam::SimSteamBoiler;
		using IceThermalStorage::SimIceStorage;
		using FuelCellElectricGenerator::SimFuelCellPlantHeatRecovery;
		using MicroCHPElectricGenerator::SimMicroCHPPlantHeatRecovery;
		using PlantValves::SimPlantValves;
		using ICEngineElectricGenerator::SimICEPlantHeatRecovery;
		using CTElectricGenerator::SimCTPlantHeatRecovery;
		using MicroturbineElectricGenerator::SimMTPlantHeatRecovery;

		// using PlantLoadProfile::PlantProfileData::simulate;
		using WaterCoils::UpdateWaterToAirCoilPlantConnection;
		using WaterUse::SimulateWaterUseConnection;
		using SolarCollectors::SimSolarCollector;
		using BaseboardRadiator::UpdateBaseboardPlantConnection;
		using HWBaseboardRadiator::UpdateHWBaseboardPlantConnection;
		using SteamBaseboardRadiator::UpdateSteamBaseboardPlantConnection;
		using RefrigeratedCase::SimRefrigCondenser;
		using PhotovoltaicThermalCollectors::SimPVTcollectors;
		using PhotovoltaicThermalCollectors::CalledFromPlantLoopEquipMgr;
		using PlantPipingSystemsManager::SimPipingSystemCircuit;
		using UserDefinedComponents::SimUserDefinedPlantComponent;
		using HVACVariableRefrigerantFlow::SimVRFCondenserPlant;
		using PlantComponentTemperatureSources::SimWaterSource;
		using PlantCentralGSHP::SimCentralGroundSourceHeatPump;

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int EquipNum; // Plant side component list equipment number
		int EquipTypeNum;
		bool RunFlag; // TRUE if operating this iteration
		int EquipFlowCtrl;
		Real64 CurLoad;
		Real64 MaxLoad;
		Real64 MinLoad;
		Real64 OptLoad;
		Real64 SizingFac; // the component sizing fraction
		int GeneralEquipType; // Basic Equipment type from EquipType Used to help organize this routine
		Real64 TempCondInDesign; // Design condenser inlet temp. C , or 25.d0
		Real64 TempEvapOutDesign;
		EnergyPlus::PlantLocation sim_component_location( LoopNum, LoopSideNum, BranchNum, Num );

		// set up a reference for this component
		auto & sim_component( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( Num ) );

		static std::vector< int > compsToSimAfterInitLoopEquip = { TypeOf_Pipe, TypeOf_PipeSteam };

		//set local variables
		EquipTypeNum = sim_component.TypeOf_Num;
		EquipFlowCtrl = sim_component.FlowCtrl;
		GeneralEquipType = sim_component.GeneralEquipType;
		EquipNum = sim_component.CompNum;
		RunFlag = sim_component.ON;
		CurLoad = sim_component.MyLoad;

		if ( sim_component.compPtr != nullptr ) {
			if ( InitLoopEquip ) {
				sim_component.compPtr->onInitLoopEquip( sim_component_location );
				sim_component.compPtr->getDesignCapacities( sim_component_location, sim_component.MaxLoad, sim_component.MinLoad, sim_component.OptLoad );
				sim_component.compPtr->getDesignTemperatures( sim_component.TempDesCondIn, sim_component.TempDesEvapOut );

				// KLUGEY HACK ALERT!!!
				// Some components before transition were never checking InitLoopEquip, and each call to SimXYZ would actually just pass through the calculation
				// Other components, on the other hand, would check InitLoopEquip, do a few things, then exit early without doing any calculation
				// This may be wrong...but during this transition, it would be very nice to keep no diffs
				// Thus, I will return here for all components that actually returned after their onInitLoopEquip stuff
				//   and I will fall through and actually call simulate on the components that did that before
				// I anticipate the list of components that fall through to be very small, so that is the check I will do.
				// If std::find returns the .end() iterator, that means it didn't find it in the list, which means it's not one of the ones to fall through, so RETURN
				if ( std::find( compsToSimAfterInitLoopEquip.begin(), compsToSimAfterInitLoopEquip.end(), EquipTypeNum ) == compsToSimAfterInitLoopEquip.end() ) {
					return;
				}
			}
			if ( GetCompSizFac ) {
				sim_component.compPtr->getSizingFactor( sim_component.SizFac );
			}
		}

		//select equipment and call equiment simulation
		//PIPES
		//Pipe has no special types at the moment, so find it this way
		if ( GeneralEquipType == GenEquipTypes_Pipe ) {
			if ( EquipTypeNum == TypeOf_Pipe ) {
				sim_component.compPtr->simulate( sim_component_location, FirstHVACIteration, CurLoad, RunFlag );

			} else if ( EquipTypeNum == TypeOf_PipeSteam ) {
				sim_component.compPtr->simulate( sim_component_location, FirstHVACIteration, CurLoad, RunFlag );

			} else if ( EquipTypeNum == TypeOf_PipeExterior ) {
				sim_component.compPtr->simulate( sim_component_location, FirstHVACIteration, CurLoad, RunFlag );

			} else if ( EquipTypeNum == TypeOf_PipeInterior ) {
				sim_component.compPtr->simulate( sim_component_location, FirstHVACIteration, CurLoad, RunFlag );

			} else if ( EquipTypeNum == TypeOf_PipeUnderground ) {
				sim_component.compPtr->simulate( sim_component_location, FirstHVACIteration, CurLoad, RunFlag );

			} else if ( EquipTypeNum == TypeOf_PipingSystemPipeCircuit ) {
				SimPipingSystemCircuit( sim_component.Name, sim_component.CompNum, InitLoopEquip, FirstHVACIteration );

			} else {
				ShowSevereError( "SimPlantEquip: Invalid Pipe Type=" + sim_component.TypeOf );
				ShowContinueError( "Occurs in Plant Loop=" + PlantLoop( LoopNum ).Name );
				ShowFatalError( "Preceding condition causes termination." );
			}

		} else if ( GeneralEquipType == GenEquipTypes_Pump ) {
			//DSU? This is still called by the sizing routine, is that OK?

			//      SELECT CASE(EquipTypeNum)
			//       CASE (TypeOf_LoopPump)
			//         ! Loop pumps are simulated before this routine
			//         PumpHeat = 0.0
			//       CASE (TypeOf_BranchPump)  ! This is the branch pump case
			//        BranchInletNode = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%NodeNumIn
			//        LastNodeOnBranch  = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%NodeNumOut
			//        BranchFlowRequest = Node(LastNodeOnBranch)%MassFlowRate
			//        IF(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%FlowLock == 0) THEN    !DSU
			//          PumpPowerToLoop = .FALSE.
			//        ELSE
			//          PumpPowerToLoop = .TRUE.
			//        END IF
			//        CALL SimPumps(EquipName,LoopNum,BranchInletNode,BranchFlowRequest, &
			//                       RunLoopPumps,InitialBranchFlow,PumpPowerToLoop,     &
			//                       PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%PumpIndex,PumpOutletNode,PumpHeat)
			//        PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%ON = RunLoopPumps
			//        PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%PumpMassFlow = node(PumpOutletNode)%MassFlowRate
			//        ! This will only stay TRUE for this branch
			//        If(PumpPowerToLoop) AddPumpHeat = .True.
			//        IF(PlantLoop(LoopNum)%LoopSide(SupplySide)%FlowLock == 0) Node(PumpOutletNode)%temp = &   !DSU
			//           PlantLoop(LoopNum)%LoopSide(DemandSide)%FlowRequestTemperature
			//    END SELECT

			//CHILLERS
		} else if ( GeneralEquipType == GenEquipTypes_Chiller ) {
			if ( ( EquipTypeNum == TypeOf_Chiller_EngineDriven ) || ( EquipTypeNum == TypeOf_Chiller_Electric ) || ( EquipTypeNum == TypeOf_Chiller_ConstCOP ) || ( EquipTypeNum == TypeOf_Chiller_CombTurbine ) ) {
				SimChiller( LoopNum, LoopSideNum, EquipTypeNum, sim_component.Name, EquipFlowCtrl, EquipNum, RunFlag, FirstHVACIteration, InitLoopEquip, CurLoad, MaxLoad, MinLoad, OptLoad, GetCompSizFac, SizingFac, TempCondInDesign, TempEvapOutDesign );
				if ( InitLoopEquip ) {
					sim_component.MaxLoad = MaxLoad;
					sim_component.MinLoad = MinLoad;
					sim_component.OptLoad = OptLoad;
					sim_component.CompNum = EquipNum;
					sim_component.TempDesCondIn = TempCondInDesign;
					sim_component.TempDesEvapOut = TempEvapOutDesign;
				}
				if ( GetCompSizFac ) {
					sim_component.SizFac = SizingFac;
				}

			} else if ( EquipTypeNum == TypeOf_Chiller_Absorption ) {
				SimBLASTAbsorber( sim_component.TypeOf, sim_component.Name, EquipFlowCtrl, LoopNum, LoopSideNum, EquipNum, RunFlag, FirstHVACIteration, InitLoopEquip, CurLoad, MaxLoad, MinLoad, OptLoad, GetCompSizFac, SizingFac, TempCondInDesign );
				if ( InitLoopEquip ) {
					sim_component.MaxLoad = MaxLoad;
					sim_component.MinLoad = MinLoad;
					sim_component.OptLoad = OptLoad;
					sim_component.CompNum = EquipNum;
					sim_component.TempDesCondIn = TempCondInDesign;
				}
				if ( GetCompSizFac ) {
					sim_component.SizFac = SizingFac;
				}

			} else if ( EquipTypeNum == TypeOf_Chiller_Indirect_Absorption ) {
				SimIndirectAbsorber( sim_component.TypeOf, sim_component.Name, EquipFlowCtrl, LoopNum, LoopSideNum, EquipNum, RunFlag, FirstHVACIteration, InitLoopEquip, CurLoad, MaxLoad, MinLoad, OptLoad, GetCompSizFac, SizingFac, TempCondInDesign );
				if ( InitLoopEquip ) {
					sim_component.MaxLoad = MaxLoad;
					sim_component.MinLoad = MinLoad;
					sim_component.OptLoad = OptLoad;
					sim_component.CompNum = EquipNum;
					sim_component.TempDesCondIn = TempCondInDesign;
				}
				if ( GetCompSizFac ) {
					sim_component.SizFac = SizingFac;
				}

			} else if ( EquipTypeNum == TypeOf_Chiller_ElectricEIR ) {
				SimElectricEIRChiller( sim_component.TypeOf, sim_component.Name, EquipFlowCtrl, EquipNum, LoopNum, RunFlag, FirstHVACIteration, InitLoopEquip, CurLoad, MaxLoad, MinLoad, OptLoad, GetCompSizFac, SizingFac, TempCondInDesign, TempEvapOutDesign );
				if ( InitLoopEquip ) {
					sim_component.MaxLoad = MaxLoad;
					sim_component.MinLoad = MinLoad;
					sim_component.OptLoad = OptLoad;
					sim_component.CompNum = EquipNum;
					sim_component.TempDesCondIn = TempCondInDesign;
					sim_component.TempDesEvapOut = TempEvapOutDesign;
				}
				if ( GetCompSizFac ) {
					sim_component.SizFac = SizingFac;
				}

			} else if ( EquipTypeNum == TypeOf_Chiller_ElectricReformEIR ) {
				SimReformulatedEIRChiller( sim_component.TypeOf, sim_component.Name, EquipFlowCtrl, EquipNum, LoopNum, RunFlag, FirstHVACIteration, InitLoopEquip, CurLoad, MaxLoad, MinLoad, OptLoad, GetCompSizFac, SizingFac, TempCondInDesign, TempEvapOutDesign );
				if ( InitLoopEquip ) {
					sim_component.MaxLoad = MaxLoad;
					sim_component.MinLoad = MinLoad;
					sim_component.OptLoad = OptLoad;
					sim_component.CompNum = EquipNum;
					sim_component.TempDesCondIn = TempCondInDesign;
					sim_component.TempDesEvapOut = TempEvapOutDesign;
				}
				if ( GetCompSizFac ) {
					sim_component.SizFac = SizingFac;
				}

				// Chiller-Heater needs to know whether it is being called for heating or cooling
				// Since loops are generic, pass the branch inlet nodenum
			} else if ( EquipTypeNum == TypeOf_Chiller_DFAbsorption ) {
				SimGasAbsorber( sim_component.TypeOf, sim_component.Name, EquipFlowCtrl, EquipNum, RunFlag, FirstHVACIteration, InitLoopEquip, CurLoad, PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).NodeNumIn, MaxLoad, MinLoad, OptLoad, GetCompSizFac, SizingFac, TempCondInDesign, TempEvapOutDesign ); //DSU
				if ( InitLoopEquip ) {
					sim_component.MaxLoad = MaxLoad;
					sim_component.MinLoad = MinLoad;
					sim_component.OptLoad = OptLoad;
					sim_component.CompNum = EquipNum;
					sim_component.TempDesCondIn = TempCondInDesign;
					sim_component.TempDesEvapOut = TempEvapOutDesign;
				}
				if ( GetCompSizFac ) {
					sim_component.SizFac = SizingFac;
				}

				// Exhaust Fired Absorption Chiller

			} else if ( EquipTypeNum == TypeOf_Chiller_ExhFiredAbsorption ) {
				SimExhaustAbsorber( sim_component.TypeOf, sim_component.Name, EquipFlowCtrl, EquipNum, RunFlag, FirstHVACIteration, InitLoopEquip, CurLoad, PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).NodeNumIn, MaxLoad, MinLoad, OptLoad, GetCompSizFac, SizingFac ); //DSU
				if ( InitLoopEquip ) {
					sim_component.MaxLoad = MaxLoad;
					sim_component.MinLoad = MinLoad;
					sim_component.OptLoad = OptLoad;
					sim_component.CompNum = EquipNum;
				}
				if ( GetCompSizFac ) {
					sim_component.SizFac = SizingFac;
				}

			} else {
				ShowSevereError( "SimPlantEquip: Invalid Chiller Type=" + sim_component.TypeOf );
				ShowContinueError( "Occurs in Plant Loop=" + PlantLoop( LoopNum ).Name );
				ShowFatalError( "Preceding condition causes termination." );
			}

			if ( InitLoopEquip && EquipNum == 0 ) {
				ShowSevereError( "InitLoop did not set Equipment Index for Chiller=" + sim_component.TypeOf );
				ShowContinueError( "..Chiller Name=" + sim_component.Name + ", in Plant Loop=" + PlantLoop( LoopNum ).Name );
				ShowFatalError( "Previous condition causes termination." );
			}

			//HEAT PUMPS
		} else if ( GeneralEquipType == GenEquipTypes_HeatPump ) {
			if ( EquipTypeNum == TypeOf_HPWaterPECooling ) {
				SimHPWatertoWaterCOOLING( sim_component.TypeOf, sim_component.Name, EquipNum, FirstHVACIteration, InitLoopEquip, CurLoad, MaxLoad, MinLoad, OptLoad, LoopNum ); //DSU
				if ( InitLoopEquip ) {
					sim_component.MaxLoad = MaxLoad;
					sim_component.MinLoad = MinLoad;
					sim_component.OptLoad = OptLoad;
					sim_component.CompNum = EquipNum;
				}

			} else if ( EquipTypeNum == TypeOf_HPWaterPEHeating ) {
				SimHPWatertoWaterHEATING( sim_component.TypeOf, sim_component.Name, EquipNum, FirstHVACIteration, InitLoopEquip, CurLoad, MaxLoad, MinLoad, OptLoad, LoopNum ); //DSU
				if ( InitLoopEquip ) {
					sim_component.MaxLoad = MaxLoad;
					sim_component.MinLoad = MinLoad;
					sim_component.OptLoad = OptLoad;
					sim_component.CompNum = EquipNum;
				}

			} else if ( EquipTypeNum == TypeOf_HPWaterEFCooling ) {
				SimHPWatertoWaterSimple( sim_component.TypeOf, EquipTypeNum, sim_component.Name, EquipNum, FirstHVACIteration, InitLoopEquip, CurLoad, MaxLoad, MinLoad, OptLoad, LoopNum ); //DSU
				if ( InitLoopEquip ) {
					sim_component.MaxLoad = MaxLoad;
					sim_component.MinLoad = MinLoad;
					sim_component.OptLoad = OptLoad;
					sim_component.CompNum = EquipNum;
				}

			} else if ( EquipTypeNum == TypeOf_HPWaterEFHeating ) {
				SimHPWatertoWaterSimple( sim_component.TypeOf, EquipTypeNum, sim_component.Name, EquipNum, FirstHVACIteration, InitLoopEquip, CurLoad, MaxLoad, MinLoad, OptLoad, LoopNum ); //DSU
				if ( InitLoopEquip ) {
					sim_component.MaxLoad = MaxLoad;
					sim_component.MinLoad = MinLoad;
					sim_component.OptLoad = OptLoad;
					sim_component.CompNum = EquipNum;
				}

			} else if ( EquipTypeNum == TypeOf_HeatPumpVRF ) {

				SimVRFCondenserPlant( sim_component.TypeOf, EquipTypeNum, sim_component.Name, EquipNum, FirstHVACIteration, InitLoopEquip, CurLoad, MaxLoad, MinLoad, OptLoad, LoopNum ); //DSU
				if ( InitLoopEquip ) {
					sim_component.MaxLoad = MaxLoad;
					sim_component.MinLoad = MinLoad;
					sim_component.OptLoad = OptLoad;
					sim_component.CompNum = EquipNum;
				}

			} else {
				ShowSevereError( "SimPlantEquip: Invalid Heat Pump Type=" + sim_component.TypeOf );
				ShowContinueError( "Occurs in Plant Loop=" + PlantLoop( LoopNum ).Name );
				ShowFatalError( "Preceding condition causes termination." );
			}

			if ( InitLoopEquip && EquipNum == 0 ) {
				ShowSevereError( "InitLoop did not set Equipment Index for HeatPump=" + sim_component.TypeOf );
				ShowContinueError( "..HeatPump Name=" + sim_component.Name + ", in Plant Loop=" + PlantLoop( LoopNum ).Name );
				ShowFatalError( "Previous condition causes termination." );
			}

			//TOWERS
		} else if ( GeneralEquipType == GenEquipTypes_CoolingTower ) {

			//TOWERS
			if ( EquipTypeNum == TypeOf_CoolingTower_SingleSpd ) {

				SimTowers( sim_component.TypeOf, sim_component.Name, EquipNum, RunFlag, InitLoopEquip, CurLoad, MaxLoad, MinLoad, OptLoad, GetCompSizFac, SizingFac ); //DSU
				if ( InitLoopEquip ) {
					sim_component.MaxLoad = MaxLoad;
					sim_component.MinLoad = MinLoad;
					sim_component.OptLoad = OptLoad;
					sim_component.CompNum = EquipNum;
				}
				if ( GetCompSizFac ) {
					sim_component.SizFac = SizingFac;
				}

			} else if ( EquipTypeNum == TypeOf_CoolingTower_TwoSpd ) {

				SimTowers( sim_component.TypeOf, sim_component.Name, EquipNum, RunFlag, InitLoopEquip, CurLoad, MaxLoad, MinLoad, OptLoad, GetCompSizFac, SizingFac ); //DSU
				if ( InitLoopEquip ) {
					sim_component.MaxLoad = MaxLoad;
					sim_component.MinLoad = MinLoad;
					sim_component.OptLoad = OptLoad;
					sim_component.CompNum = EquipNum;
				}
				if ( GetCompSizFac ) {
					sim_component.SizFac = SizingFac;
				}

			} else if ( EquipTypeNum == TypeOf_CoolingTower_VarSpd ) { // 'CoolingTower:VariableSpeed'

				SimTowers( sim_component.TypeOf, sim_component.Name, EquipNum, RunFlag, InitLoopEquip, CurLoad, MaxLoad, MinLoad, OptLoad, GetCompSizFac, SizingFac ); //DSU
				if ( InitLoopEquip ) {
					sim_component.MaxLoad = MaxLoad;
					sim_component.MinLoad = MinLoad;
					sim_component.OptLoad = OptLoad;
					sim_component.CompNum = EquipNum;
				}
				if ( GetCompSizFac ) {
					sim_component.SizFac = SizingFac;
				}

			} else if ( EquipTypeNum == TypeOf_CoolingTower_VarSpdMerkel ) {

				SimTowers( sim_component.TypeOf, sim_component.Name, EquipNum, RunFlag, InitLoopEquip, CurLoad, MaxLoad, MinLoad, OptLoad, GetCompSizFac, SizingFac );
				if ( InitLoopEquip ) {
					sim_component.MaxLoad = MaxLoad;
					sim_component.MinLoad = MinLoad;
					sim_component.OptLoad = OptLoad;
					sim_component.CompNum = EquipNum;
				}
				if ( GetCompSizFac ) {
					sim_component.SizFac = SizingFac;
				}
			} else {
				ShowSevereError( "SimPlantEquip: Invalid Tower Type=" + sim_component.TypeOf );
				ShowContinueError( "Occurs in Plant Loop=" + PlantLoop( LoopNum ).Name );
				ShowFatalError( "Preceding condition causes termination." );
			}

			if ( InitLoopEquip && EquipNum == 0 ) {
				ShowSevereError( "InitLoop did not set Equipment Index for Cooling Tower=" + sim_component.TypeOf );
				ShowContinueError( "..Tower Name=" + sim_component.Name + ", in Plant Loop=" + PlantLoop( LoopNum ).Name );
				ShowFatalError( "Previous condition causes termination." );
			}

			//FLUID COOLERS
		} else if ( GeneralEquipType == GenEquipTypes_FluidCooler ) {

			//FluidCoolerS
			if ( EquipTypeNum == TypeOf_FluidCooler_SingleSpd ) {

				SimFluidCoolers( sim_component.TypeOf, sim_component.Name, EquipNum, RunFlag, InitLoopEquip, MaxLoad, MinLoad, OptLoad ); //DSU
				if ( InitLoopEquip ) {
					sim_component.MaxLoad = MaxLoad;
					sim_component.MinLoad = MinLoad;
					sim_component.OptLoad = OptLoad;
					sim_component.CompNum = EquipNum;
				}

			} else if ( EquipTypeNum == TypeOf_FluidCooler_TwoSpd ) {

				SimFluidCoolers( sim_component.TypeOf, sim_component.Name, EquipNum, RunFlag, InitLoopEquip, MaxLoad, MinLoad, OptLoad ); //DSU
				if ( InitLoopEquip ) {
					sim_component.MaxLoad = MaxLoad;
					sim_component.MinLoad = MinLoad;
					sim_component.OptLoad = OptLoad;
					sim_component.CompNum = EquipNum;
				}
			} else {
				ShowSevereError( "SimPlantEquip: Invalid FluidCooler Type=" + sim_component.TypeOf );
				ShowContinueError( "Occurs in Plant Loop=" + PlantLoop( LoopNum ).Name );
				ShowFatalError( "Preceding condition causes termination." );
			}

			if ( InitLoopEquip && EquipNum == 0 ) {
				ShowSevereError( "InitLoop did not set Equipment Index for Fluid Cooler=" + sim_component.TypeOf );
				ShowContinueError( "..Fluid Cooler Name=" + sim_component.Name + ", in Plant Loop=" + PlantLoop( LoopNum ).Name );
				ShowFatalError( "Previous condition causes termination." );
			}

		} else if ( GeneralEquipType == GenEquipTypes_EvapFluidCooler ) {

			//EvapFluidCoolers
			if ( EquipTypeNum == TypeOf_EvapFluidCooler_SingleSpd ) {

				SimEvapFluidCoolers( sim_component.TypeOf, sim_component.Name, EquipNum, RunFlag, InitLoopEquip, MaxLoad, MinLoad, OptLoad, GetCompSizFac, SizingFac ); //DSU
				if ( InitLoopEquip ) {
					sim_component.MaxLoad = MaxLoad;
					sim_component.MinLoad = MinLoad;
					sim_component.OptLoad = OptLoad;
					sim_component.CompNum = EquipNum;
				}

			} else if ( EquipTypeNum == TypeOf_EvapFluidCooler_TwoSpd ) {

				SimEvapFluidCoolers( sim_component.TypeOf, sim_component.Name, EquipNum, RunFlag, InitLoopEquip, MaxLoad, MinLoad, OptLoad, GetCompSizFac, SizingFac ); //DSU
				if ( InitLoopEquip ) {
					sim_component.MaxLoad = MaxLoad;
					sim_component.MinLoad = MinLoad;
					sim_component.OptLoad = OptLoad;
					sim_component.CompNum = EquipNum;
				}
			} else {
				ShowSevereError( "SimPlantEquip: Invalid EvapFluidCooler Type=" + sim_component.TypeOf );
				ShowContinueError( "Occurs in Plant Loop=" + PlantLoop( LoopNum ).Name );
				ShowFatalError( "Preceding condition causes termination." );
			}

			if ( InitLoopEquip && EquipNum == 0 ) {
				ShowSevereError( "InitLoop did not set Equipment Index for Fluid Cooler=" + sim_component.TypeOf );
				ShowContinueError( "..Fluid Cooler Name=" + sim_component.Name + ", in Plant Loop=" + PlantLoop( LoopNum ).Name );
				ShowFatalError( "Previous condition causes termination." );
			}

			//BOILERS
		} else if ( GeneralEquipType == GenEquipTypes_Boiler ) {
			if ( EquipTypeNum == TypeOf_Boiler_Simple ) {
				SimBoiler( sim_component.TypeOf, sim_component.Name, EquipFlowCtrl, EquipNum, RunFlag, InitLoopEquip, CurLoad, MaxLoad, MinLoad, OptLoad, GetCompSizFac, SizingFac ); //DSU
				if ( InitLoopEquip ) {
					sim_component.MaxLoad = MaxLoad;
					sim_component.MinLoad = MinLoad;
					sim_component.OptLoad = OptLoad;
					sim_component.CompNum = EquipNum;
				}
				if ( GetCompSizFac ) {
					sim_component.SizFac = SizingFac;
				}

			} else if ( EquipTypeNum == TypeOf_Boiler_Steam ) {
				SimSteamBoiler( sim_component.TypeOf, sim_component.Name, EquipFlowCtrl, EquipNum, RunFlag, FirstHVACIteration, InitLoopEquip, CurLoad, MaxLoad, MinLoad, OptLoad, GetCompSizFac, SizingFac ); //DSU
				if ( InitLoopEquip ) {
					sim_component.MaxLoad = MaxLoad;
					sim_component.MinLoad = MinLoad;
					sim_component.OptLoad = OptLoad;
					sim_component.CompNum = EquipNum;
				}
				if ( GetCompSizFac ) {
					sim_component.SizFac = SizingFac;
				}

			} else {
				ShowSevereError( "SimPlantEquip: Invalid Boiler Type=" + sim_component.TypeOf );
				ShowContinueError( "Occurs in Plant Loop=" + PlantLoop( LoopNum ).Name );
				ShowFatalError( "Preceding condition causes termination." );
			}

			if ( InitLoopEquip && EquipNum == 0 ) {
				ShowSevereError( "InitLoop did not set Equipment Index for Boiler=" + sim_component.TypeOf );
				ShowContinueError( "..Boiler Name=" + sim_component.Name + ", in Plant Loop=" + PlantLoop( LoopNum ).Name );
				ShowFatalError( "Previous condition causes termination." );
			}

			//WATER HEATER
		} else if ( GeneralEquipType == GenEquipTypes_WaterThermalTank ) {

			if ( ( EquipTypeNum == TypeOf_WtrHeaterMixed ) || ( EquipTypeNum == TypeOf_WtrHeaterStratified ) ) {
				SimWaterThermalTank( EquipTypeNum, sim_component.Name, EquipNum, RunFlag, InitLoopEquip, CurLoad, MaxLoad, MinLoad, OptLoad, FirstHVACIteration, LoopNum, LoopSideNum ); //DSU
				if ( InitLoopEquip ) {
					sim_component.MaxLoad = MaxLoad;
					sim_component.MinLoad = MinLoad;
					sim_component.OptLoad = OptLoad;
					sim_component.CompNum = EquipNum;
				}

				// HEAT PUMP WATER HEATER
			} else if ( EquipTypeNum == TypeOf_HeatPumpWtrHeaterPumped || EquipTypeNum == TypeOf_HeatPumpWtrHeaterWrapped ) {
				SimWaterThermalTank( EquipTypeNum, sim_component.Name, EquipNum, RunFlag, InitLoopEquip, CurLoad, MaxLoad, MinLoad, OptLoad, FirstHVACIteration, LoopNum, LoopSideNum ); //DSU
				if ( InitLoopEquip ) {
					sim_component.MaxLoad = MaxLoad;
					sim_component.MinLoad = MinLoad;
					sim_component.OptLoad = OptLoad;
					sim_component.CompNum = EquipNum;
				}

			} else {
				ShowSevereError( "SimPlantEquip: Invalid Water Heater Type=" + sim_component.TypeOf );
				ShowContinueError( "Occurs in Plant Loop=" + PlantLoop( LoopNum ).Name );
				ShowFatalError( "Preceding condition causes termination." );
			}

			if ( InitLoopEquip && EquipNum == 0 ) {
				ShowSevereError( "InitLoop did not set Equipment Index for Water Heater=" + sim_component.TypeOf );
				ShowContinueError( "..Water Thermal Tank Name=" + sim_component.Name + ", in Plant Loop=" + PlantLoop( LoopNum ).Name );
				ShowFatalError( "Previous condition causes termination." );
			}

			//PURCHASED
		} else if ( GeneralEquipType == GenEquipTypes_Purchased ) {
			if ( EquipTypeNum == TypeOf_PurchChilledWater ) {
				SimOutsideEnergy( sim_component.TypeOf, sim_component.Name, EquipFlowCtrl, EquipNum, RunFlag, InitLoopEquip, CurLoad, MaxLoad, MinLoad, OptLoad, FirstHVACIteration ); //DSU
				if ( InitLoopEquip ) {
					sim_component.MaxLoad = MaxLoad;
					sim_component.MinLoad = MinLoad;
					sim_component.OptLoad = OptLoad;
					sim_component.CompNum = EquipNum;
				}

			} else if ( EquipTypeNum == TypeOf_PurchHotWater ) {
				SimOutsideEnergy( sim_component.TypeOf, sim_component.Name, EquipFlowCtrl, EquipNum, RunFlag, InitLoopEquip, CurLoad, MaxLoad, MinLoad, OptLoad, FirstHVACIteration ); //DSU
				if ( InitLoopEquip ) {
					sim_component.MaxLoad = MaxLoad;
					sim_component.MinLoad = MinLoad;
					sim_component.OptLoad = OptLoad;
					sim_component.CompNum = EquipNum;
				}

			} else {
				ShowSevereError( "SimPlantEquip: Invalid District Energy Type=" + sim_component.TypeOf );
				ShowContinueError( "Occurs in Plant Loop=" + PlantLoop( LoopNum ).Name );
				ShowFatalError( "Preceding condition causes termination." );
			}

			if ( InitLoopEquip && EquipNum == 0 ) {
				ShowSevereError( "InitLoop did not set Equipment Index for District Energy=" + sim_component.TypeOf );
				ShowContinueError( "..District Cooling/Heating Name=" + sim_component.Name + ", in Plant Loop=" + PlantLoop( LoopNum ).Name );
				ShowFatalError( "Previous condition causes termination." );
			}

		} else if ( GeneralEquipType == GenEquipTypes_HeatExchanger ) {

			if ( EquipTypeNum == TypeOf_FluidToFluidPlantHtExchg ) {
				SimFluidHeatExchanger( LoopNum, LoopSideNum, sim_component.TypeOf, sim_component.Name, EquipNum, InitLoopEquip, CurLoad, MaxLoad, MinLoad, OptLoad, FirstHVACIteration );
				if ( InitLoopEquip ) {
					sim_component.MaxLoad = MaxLoad;
					sim_component.MinLoad = MinLoad;
					sim_component.OptLoad = OptLoad;
					sim_component.CompNum = EquipNum;
				}

			} else {
				ShowSevereError( "SimPlantEquip: Invalid Heat Exchanger Type=" + sim_component.TypeOf );
				ShowContinueError( "Occurs in Plant Loop=" + PlantLoop( LoopNum ).Name );
				ShowFatalError( "Preceding condition causes termination." );

			}

		} else if ( GeneralEquipType == GenEquipTypes_GroundHeatExchanger ) {

			if ( EquipTypeNum == TypeOf_GrndHtExchgVertical ) { // 'GROUND HEAT EXCHANGER:VERTICAL'
				sim_component.compPtr->simulate( sim_component_location, FirstHVACIteration, CurLoad, RunFlag );

			} else if ( EquipTypeNum == TypeOf_GrndHtExchgSurface ) { // 'GROUND HEAT EXCHANGER:SURFACE'
				sim_component.compPtr->simulate( sim_component_location, FirstHVACIteration, CurLoad, RunFlag );

			} else if ( EquipTypeNum == TypeOf_GrndHtExchgPond ) { // 'GROUND HEAT EXCHANGER:POND'
				sim_component.compPtr->simulate( sim_component_location, FirstHVACIteration, CurLoad, RunFlag );

			} else if ( EquipTypeNum == TypeOf_GrndHtExchgHorizTrench ) {
				SimPipingSystemCircuit( sim_component.Name, sim_component.CompNum, InitLoopEquip, FirstHVACIteration );

				if ( InitLoopEquip ) {
					sim_component.CompNum = EquipNum;
				}

			} else if ( EquipTypeNum == TypeOf_GrndHtExchgSlinky ) { // 'GROUND HEAT EXCHANGER:SLINKY'
				sim_component.compPtr->simulate( sim_component_location, FirstHVACIteration, CurLoad, RunFlag );


			}
			// THERMAL STORAGE
		} else if ( GeneralEquipType == GenEquipTypes_ThermalStorage ) {

			// If component setpoint based control is active for this equipment
			// then reset CurLoad to original EquipDemand.
			// Allow negative CurLoad.  For cold storage this means the storage should
			// charge, for hot storage, this means the storage should discharge.
			if ( sim_component.CurOpSchemeType == CompSetPtBasedSchemeType ) {
				CurLoad = sim_component.EquipDemand;
				if ( CurLoad != 0 ) RunFlag = true;
			}

			if ( EquipTypeNum == TypeOf_TS_IceSimple ) {

				SimIceStorage( sim_component.TypeOf, sim_component.Name, EquipNum, RunFlag, FirstHVACIteration, InitLoopEquip, CurLoad ); //DSU | ,EquipFlowCtrl

				if ( InitLoopEquip ) {
					sim_component.MaxLoad = 0.0;
					sim_component.MinLoad = 0.0;
					sim_component.OptLoad = 0.0;
					sim_component.CompNum = EquipNum;
				}

			} else if ( EquipTypeNum == TypeOf_TS_IceDetailed ) {

				SimIceStorage( sim_component.TypeOf, sim_component.Name, EquipNum, RunFlag, FirstHVACIteration, InitLoopEquip, CurLoad ); //DSU | ,EquipFlowCtrl

				// Not sure what this really needs to do here...
				if ( InitLoopEquip ) {
					sim_component.MaxLoad = 0.0;
					sim_component.MinLoad = 0.0;
					sim_component.OptLoad = 0.0;
					sim_component.CompNum = EquipNum;
				}

			} else if ( ( EquipTypeNum == TypeOf_ChilledWaterTankMixed ) || ( EquipTypeNum == TypeOf_ChilledWaterTankStratified ) ) {
				SimWaterThermalTank( EquipTypeNum, sim_component.Name, EquipNum, RunFlag, InitLoopEquip, CurLoad, MaxLoad, MinLoad, OptLoad, FirstHVACIteration, LoopNum, LoopSideNum ); //DSU
				if ( InitLoopEquip ) {
					sim_component.MaxLoad = MaxLoad;
					sim_component.MinLoad = MinLoad;
					sim_component.OptLoad = OptLoad;
					sim_component.CompNum = EquipNum;
				}

			} else {
				ShowSevereError( "SimPlantEquip: Invalid Chilled/Ice Thermal Storage Type=" + sim_component.TypeOf );
				ShowContinueError( "Occurs in Plant Loop=" + PlantLoop( LoopNum ).Name );
				ShowFatalError( "Preceding condition causes termination." );
			}

			if ( InitLoopEquip && EquipNum == 0 ) {
				ShowSevereError( "InitLoop did not set Equipment Index for Thermal Storage=" + sim_component.TypeOf );
				ShowContinueError( "..Chilled/Ice Thermal Storage Name=" + sim_component.Name + ", in Plant Loop=" + PlantLoop( LoopNum ).Name );
				ShowFatalError( "Previous condition causes termination." );
			}

		} else if ( GeneralEquipType == GenEquipTypes_Valve ) {
			if ( EquipTypeNum == TypeOf_ValveTempering ) {
				SimPlantValves( EquipTypeNum, sim_component.Name, EquipNum, RunFlag, InitLoopEquip, CurLoad, MaxLoad, MinLoad, OptLoad, FirstHVACIteration ); //DSU
				if ( InitLoopEquip ) {
					sim_component.MaxLoad = MaxLoad;
					sim_component.MinLoad = MinLoad;
					sim_component.OptLoad = OptLoad;
					sim_component.CompNum = EquipNum;
				}
			} else {
				ShowSevereError( "SimPlantEquip: Invalid Valve Type=" + sim_component.TypeOf );
				ShowContinueError( "Occurs in Plant Loop=" + PlantLoop( LoopNum ).Name );
				ShowFatalError( "Preceding condition causes termination." );
			}

			if ( InitLoopEquip && EquipNum == 0 ) {
				ShowSevereError( "InitLoop did not set Equipment Index for Valves=" + sim_component.TypeOf );
				ShowContinueError( "..Valve Name=" + sim_component.Name + ", in Plant Loop=" + PlantLoop( LoopNum ).Name );
				ShowFatalError( "Previous condition causes termination." );
			}

		} else if ( GeneralEquipType == GenEquipTypes_Generator ) {
			// for heat recovery plant interactions.

			if ( EquipTypeNum == TypeOf_Generator_FCExhaust ) {
				SimFuelCellPlantHeatRecovery( sim_component.TypeOf, sim_component.Name, TypeOf_Generator_FCExhaust, EquipNum, RunFlag, InitLoopEquip, CurLoad, MaxLoad, MinLoad, OptLoad, FirstHVACIteration ); //DSU
				if ( InitLoopEquip ) {
					sim_component.MaxLoad = MaxLoad;
					sim_component.MinLoad = MinLoad;
					sim_component.OptLoad = OptLoad;
					sim_component.CompNum = EquipNum;
				}

			} else if ( EquipTypeNum == TypeOf_Generator_FCStackCooler ) {
				SimFuelCellPlantHeatRecovery( sim_component.TypeOf, sim_component.Name, TypeOf_Generator_FCStackCooler, EquipNum, RunFlag, InitLoopEquip, CurLoad, MaxLoad, MinLoad, OptLoad, FirstHVACIteration ); //DSU
				if ( InitLoopEquip ) {
					sim_component.MaxLoad = MaxLoad;
					sim_component.MinLoad = MinLoad;
					sim_component.OptLoad = OptLoad;
					sim_component.CompNum = EquipNum;
				}

			} else if ( EquipTypeNum == TypeOf_Generator_MicroCHP ) {
				SimMicroCHPPlantHeatRecovery( sim_component.TypeOf, sim_component.Name, EquipNum, RunFlag, InitLoopEquip, CurLoad, MaxLoad, MinLoad, OptLoad, FirstHVACIteration ); //DSU
				if ( InitLoopEquip ) {
					sim_component.MaxLoad = MaxLoad;
					sim_component.MinLoad = MinLoad;
					sim_component.OptLoad = OptLoad;
					sim_component.CompNum = EquipNum;
				}

			} else if ( EquipTypeNum == TypeOf_Generator_MicroTurbine ) {
				SimMTPlantHeatRecovery( sim_component.TypeOf, sim_component.Name, TypeOf_Generator_MicroTurbine, EquipNum, RunFlag, InitLoopEquip, CurLoad, MaxLoad, MinLoad, OptLoad, FirstHVACIteration ); //DSU
				if ( InitLoopEquip ) {
					sim_component.MaxLoad = MaxLoad;
					sim_component.MinLoad = MinLoad;
					sim_component.OptLoad = OptLoad;
					sim_component.CompNum = EquipNum;
				}

			} else if ( EquipTypeNum == TypeOf_Generator_ICEngine ) {
				SimICEPlantHeatRecovery( sim_component.TypeOf, sim_component.Name, TypeOf_Generator_ICEngine, EquipNum, RunFlag, InitLoopEquip, CurLoad, MaxLoad, MinLoad, OptLoad, FirstHVACIteration ); //DSU
				if ( InitLoopEquip ) {
					sim_component.MaxLoad = MaxLoad;
					sim_component.MinLoad = MinLoad;
					sim_component.OptLoad = OptLoad;
					sim_component.CompNum = EquipNum;
				}

			} else if ( EquipTypeNum == TypeOf_Generator_CTurbine ) {
				SimCTPlantHeatRecovery( sim_component.TypeOf, sim_component.Name, TypeOf_Generator_CTurbine, EquipNum, RunFlag, InitLoopEquip, CurLoad, MaxLoad, MinLoad, OptLoad, FirstHVACIteration ); //DSU
				if ( InitLoopEquip ) {
					sim_component.MaxLoad = MaxLoad;
					sim_component.MinLoad = MinLoad;
					sim_component.OptLoad = OptLoad;
					sim_component.CompNum = EquipNum;
				}

			} else {
				ShowSevereError( "SimPlantEquip: Invalid Generator Type=" + sim_component.TypeOf );
				ShowContinueError( "Occurs in Plant Loop=" + PlantLoop( LoopNum ).Name );
				ShowFatalError( "Preceding condition causes termination." );

			}

			if ( InitLoopEquip && EquipNum == 0 ) {
				ShowSevereError( "InitLoop did not set Equipment Index for Generator=" + sim_component.TypeOf );
				ShowContinueError( "..Generator Name=" + sim_component.Name + ", in Plant Loop=" + PlantLoop( LoopNum ).Name );
				ShowFatalError( "Previous condition causes termination." );
			}

		} else if ( GeneralEquipType == GenEquipTypes_LoadProfile ) { // DSU2 draft out InitLoopEquip on a demand side component

			if ( EquipTypeNum == TypeOf_PlantLoadProfile ) {
				sim_component.compPtr->simulate( sim_component_location, FirstHVACIteration, CurLoad, RunFlag );
				if ( InitLoopEquip ) {
					sim_component.CompNum = EquipNum;

				}

			} else {
				ShowSevereError( "SimPlantEquip: Invalid Load Profile Type=" + sim_component.TypeOf );
				ShowContinueError( "Occurs in Plant Loop=" + PlantLoop( LoopNum ).Name );
				ShowFatalError( "Preceding condition causes termination." );
			}

		} else if ( GeneralEquipType == GenEquipTypes_DemandCoil ) { //DSU3
			// for now these are place holders, the sim routines are called from other places, unclear if we need
			//  to call an update routine, or if air-side updates are sufficient.  this is where plant updates would be called from

			if ( EquipTypeNum == TypeOf_CoilWaterCooling ) {
				//          CALL UpdateWaterToAirCoilPlantConnection(EquipTypeNum,EquipName,EquipFlowCtrl,LoopNum,LoopSideNum,EquipNum,&
				//                                                   FirstHVACIteration, InitLoopEquip)
				//          IF(InitLoopEquip)THEN
				//            PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
				//          ENDIF
			} else if ( EquipTypeNum == TypeOf_CoilWaterDetailedFlatCooling ) {
				//          CALL UpdateWaterToAirCoilPlantConnection(EquipTypeNum,EquipName,EquipFlowCtrl,LoopNum,LoopSideNum,EquipNum,&
				//                                                   FirstHVACIteration, InitLoopEquip)
				//          IF(InitLoopEquip)THEN
				//            PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
				//          ENDIF
			} else if ( EquipTypeNum == TypeOf_CoilWaterSimpleHeating ) {
				//!          CALL UpdateWaterToAirCoilPlantConnection(EquipTypeNum,EquipName,EquipFlowCtrl,LoopNum,LoopSideNum,EquipNum,&
				//!                                                   FirstHVACIteration, InitLoopEquip)
				//!
				//          IF(InitLoopEquip)THEN
				//            PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
				//          ENDIF
			} else if ( EquipTypeNum == TypeOf_CoilSteamAirHeating ) {
				//CALL UpdateSteamToAirCoilPlantConnection()

			} else if ( EquipTypeNum == TypeOf_CoilWAHPHeatingEquationFit ) {

			} else if ( EquipTypeNum == TypeOf_CoilWAHPCoolingEquationFit ) {

			} else if ( EquipTypeNum == TypeOf_CoilVSWAHPHeatingEquationFit ) {

			} else if ( EquipTypeNum == TypeOf_CoilVSWAHPCoolingEquationFit ) {

			} else if ( EquipTypeNum == TypeOf_CoilWAHPHeatingParamEst ) {

			} else if ( EquipTypeNum == TypeOf_CoilWAHPCoolingParamEst ) {

			} else if ( EquipTypeNum == TypeOf_PackagedTESCoolingCoil ) {

			} else {
				ShowSevereError( "SimPlantEquip: Invalid Load Coil Type=" + sim_component.TypeOf );
				ShowContinueError( "Occurs in Plant Loop=" + PlantLoop( LoopNum ).Name );
				ShowFatalError( "Preceding condition causes termination." );

			} //DSU3

		} else if ( GeneralEquipType == GenEquipTypes_WaterUse ) {

			if ( EquipTypeNum == TypeOf_WaterUseConnection ) {

				SimulateWaterUseConnection( EquipTypeNum, sim_component.Name, EquipNum, InitLoopEquip, FirstHVACIteration );

				if ( InitLoopEquip ) {
					sim_component.CompNum = EquipNum;
				}
			} else {
				ShowSevereError( "SimPlantEquip: Invalid Load Coil Type=" + sim_component.TypeOf );
				ShowContinueError( "Occurs in Plant Loop=" + PlantLoop( LoopNum ).Name );
				ShowFatalError( "Preceding condition causes termination." );
			}

		} else if ( GeneralEquipType == GenEquipTypes_SolarCollector ) {

			if ( ( EquipTypeNum == TypeOf_SolarCollectorFlatPlate ) || ( EquipTypeNum == TypeOf_SolarCollectorICS ) ) {

				SimSolarCollector( EquipTypeNum, sim_component.Name, EquipNum, InitLoopEquip, FirstHVACIteration );

				if ( InitLoopEquip ) {
					sim_component.CompNum = EquipNum;
				}

			} else if ( EquipTypeNum == TypeOf_PVTSolarCollectorFlatPlate ) {

				SimPVTcollectors( EquipNum, FirstHVACIteration, CalledFromPlantLoopEquipMgr, sim_component.Name, InitLoopEquip );

				if ( InitLoopEquip ) {
					sim_component.CompNum = EquipNum;
				}

			} else {
				ShowSevereError( "SimPlantEquip: Invalid Solar Collector Type=" + sim_component.TypeOf );
				ShowContinueError( "Occurs in Plant Loop=" + PlantLoop( LoopNum ).Name );
				ShowFatalError( "Preceding condition causes termination." );
			}

		} else if ( GeneralEquipType == GenEquipTypes_ZoneHVACDemand ) { //ZoneHVAC and air terminal models with direct plant connections
			// for now these are place holders, the sim routines are called from other places, unclear if we need
			//  to call an update routine, or if air-side updates are sufficient.  this is where plant updates would be called from

			if ( EquipTypeNum == TypeOf_Baseboard_Conv_Water ) {

			} else if ( EquipTypeNum == TypeOf_Baseboard_Rad_Conv_Steam ) {

			} else if ( EquipTypeNum == TypeOf_Baseboard_Rad_Conv_Water ) {

			} else if ( EquipTypeNum == TypeOf_LowTempRadiant_VarFlow ) {

			} else if ( EquipTypeNum == TypeOf_LowTempRadiant_ConstFlow ) {

			} else if ( EquipTypeNum == TypeOf_CooledBeamAirTerminal ) {

			} else if ( EquipTypeNum == TypeOf_FourPipeBeamAirTerminal ) {

			} else if ( EquipTypeNum == TypeOf_MultiSpeedHeatPumpRecovery ) {

			} else if ( EquipTypeNum == TypeOf_UnitarySystemRecovery ) {

			} else if ( EquipTypeNum == TypeOf_SwimmingPool_Indoor ) {

			} else {

				ShowSevereError( "SimPlantEquip: Invalid ZoneHVAC Type=" + sim_component.TypeOf );
				ShowContinueError( "Occurs in Plant Loop=" + PlantLoop( LoopNum ).Name );
				ShowFatalError( "Preceding condition causes termination." );

			}

		} else if ( GeneralEquipType == GenEquipTypes_Refrigeration ) {

			if ( EquipTypeNum == TypeOf_RefrigSystemWaterCondenser ) {
				SimRefrigCondenser( EquipTypeNum, sim_component.Name, EquipNum, FirstHVACIteration, InitLoopEquip );

				if ( InitLoopEquip ) {
					sim_component.CompNum = EquipNum;
				}

			} else if ( EquipTypeNum == TypeOf_RefrigerationWaterCoolRack ) {
				SimRefrigCondenser( EquipTypeNum, sim_component.Name, EquipNum, FirstHVACIteration, InitLoopEquip );

				if ( InitLoopEquip ) {
					sim_component.CompNum = EquipNum;
				}

			} else {
				ShowSevereError( "SimPlantEquip: Invalid Refrigeration Type=" + sim_component.TypeOf );
				ShowContinueError( "Occurs in Plant Loop=" + PlantLoop( LoopNum ).Name );
				ShowFatalError( "Preceding condition causes termination." );
			}

		} else if ( GeneralEquipType == GenEquipTypes_PlantComponent ) {

			if ( EquipTypeNum == TypeOf_PlantComponentUserDefined ) {

				SimUserDefinedPlantComponent( LoopNum, LoopSideNum, sim_component.TypeOf, sim_component.Name, EquipNum, InitLoopEquip, CurLoad, MaxLoad, MinLoad, OptLoad );
				if ( InitLoopEquip ) {
					sim_component.MaxLoad = MaxLoad;
					sim_component.MinLoad = MinLoad;
					sim_component.OptLoad = OptLoad;
					sim_component.CompNum = EquipNum;
				}

			} else if ( EquipTypeNum == TypeOf_WaterSource ) {

				SimWaterSource( sim_component.Name, EquipFlowCtrl, EquipNum, RunFlag, FirstHVACIteration, InitLoopEquip, CurLoad, MaxLoad, MinLoad, OptLoad, GetCompSizFac, SizingFac ); //DSU
				if ( InitLoopEquip ) {
					sim_component.MaxLoad = MaxLoad;
					sim_component.MinLoad = MinLoad;
					sim_component.OptLoad = OptLoad;
					sim_component.CompNum = EquipNum;
				}

			} else {
				//        CALL ShowSevereError('SimPlantEquip: Invalid Component Equipment Type='//TRIM(EquipType))
				//        CALL ShowContinueError('Occurs in Plant Loop='//TRIM(PlantLoop(LoopNum)%Name))
				//        CALL ShowFatalError('Preceding condition causes termination.')

			}

		} else if ( GeneralEquipType == GenEquipTypes_CentralHeatPumpSystem ) {

			if ( EquipTypeNum == TypeOf_CentralGroundSourceHeatPump ) {

				SimCentralGroundSourceHeatPump( sim_component.Name, EquipFlowCtrl, EquipNum, LoopNum, RunFlag, FirstHVACIteration, InitLoopEquip, CurLoad, MaxLoad, MinLoad, OptLoad, GetCompSizFac, SizingFac );
				if ( InitLoopEquip ) {
					sim_component.MaxLoad = MaxLoad;
					sim_component.MinLoad = MinLoad;
					sim_component.OptLoad = OptLoad;
					sim_component.CompNum = EquipNum;
				}

				if ( GetCompSizFac ) {
					sim_component.SizFac = SizingFac;
				}

			} else {
				ShowSevereError( "SimPlantEquip: Invalid Central Heat Pump System Type=" + sim_component.TypeOf );
				ShowContinueError( "Occurs in Plant Loop=" + PlantLoop( LoopNum ).Name );
				ShowFatalError( "Preceding condition causes termination." );
			}

		} else {
			ShowSevereError( "SimPlantEquip: Invalid Equipment Type=" + sim_component.TypeOf );
			ShowContinueError( "Occurs in Plant Loop=" + PlantLoop( LoopNum ).Name );
			ShowFatalError( "Preceding condition causes termination." );
		} // TypeOfEquip

	}

} // PlantLoopEquip

} // EnergyPlus
