// C++ Headers
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <IntegratedHeatPump.hh>
#include <BranchNodeConnections.hh>
#include <CurveManager.hh>
#include <DataAirSystems.hh>
#include <DataContaminantBalance.hh>
#include <DataEnvironment.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <DataWater.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <GlobalNames.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutAirNodeManager.hh>
#include <OutputProcessor.hh>
#include <OutputReportPredefined.hh>
#include <PlantUtilities.hh>
#include <Psychrometrics.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>
#include <WaterManager.hh>
#include <WaterThermalTanks.hh>
#include <InputProcessor.hh>

namespace EnergyPlus {

	namespace IntegratedHeatPumps {

		// USE STATEMENTS:
		// Use statements for data only modules
		// Using/Aliasing
		using namespace DataPrecisionGlobals;
		using namespace DataLoopNode;
		using namespace DataGlobals;
		using General::RoundSigDigits;

		// Use statements for access to subroutines in other modules

		// Data
		//MODULE PARAMETER DEFINITIONS

		static std::string const BlankString;

		// DERIVED TYPE DEFINITIONS

		// MODULE VARIABLE DECLARATIONS:
		// Identifier is VarSpeedCoil
		int NumIHPs(0); // The Number of Water to Air Heat Pumps found in the Input
		bool GetCoilsInputFlag(true);

		// SUBROUTINE SPECIFICATIONS FOR MODULE

		// Driver/Manager Routines

		// Get Input routines for module

		// Initialization routines for module

		// Update routines to check convergence and update nodes

		// Update routine

		// Utility routines
		//SHR, bypass factor routines

		// Object Data
		Array1D<IntegratedHeatPumpData> IntegratedHeatPumpUnits;

		// MODULE SUBROUTINES:
		//*************************************************************************

		// Functions
		void
			clear_state()
		{

			IntegratedHeatPumpUnits.deallocate();
		}


		void
			SimIHP(
			std::string const & CompName, // Coil Name
			int & CompIndex, // Index for Component name
			int const CyclingScheme, // Continuous fan OR cycling compressor
			Real64 & MaxONOFFCyclesperHour, // Maximum cycling rate of heat pump [cycles/hr]
			Real64 & HPTimeConstant, // Heat pump time constant [s]
			Real64 & FanDelayTime, // Fan delay time, time delay for the HP's fan to
			int const CompOp, // compressor on/off. 0 = off; 1= on
			Real64 const PartLoadFrac,//part load fraction
			int const SpeedNum, // compressor speed number
			Real64 const SpeedRatio, // compressor speed ratio
			Real64 const SensLoad, // Sensible demand load [W]
			Real64 const LatentLoad, // Latent demand load [W]
			bool const IsCallbyWH, //whether the call from the water heating loop or air loop, true = from water heating loop
			bool const FirstHVACIteration, // TRUE if First iteration of simulation
			Optional< Real64 const > OnOffAirFlowRat // ratio of comp on to comp off air flow rate
			)
		{

			//       AUTHOR         Bo Shen, ORNL
			//       DATE WRITTEN   March 2016
			//       RE-ENGINEERED  na

			// PURPOSE OF THIS SUBROUTINE:
			// This subroutine manages variable-speed integrated Air source heat pump simulation.

			// METHODOLOGY EMPLOYED:

			// REFERENCES:
			// N/A

			// Using/Aliasing
			using InputProcessor::FindItemInList;
			using General::TrimSigDigits;
			using VariableSpeedCoils::SimVariableSpeedCoils;
			using VariableSpeedCoils::VarSpeedCoil;
			using VariableSpeedCoils::UpdateVarSpeedCoil;
			using VariableSpeedCoils::InitVarSpeedCoil;

			// Locals
			// SUBROUTINE ARGUMENT DEFINITIONS:

			// SUBROUTINE PARAMETER DEFINITIONS:
			// na

			// INTERFACE BLOCK SPECIFICATIONS
			// na

			// DERIVED TYPE DEFINITIONS
			// na


			// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
			int DXCoilNum(0); // The IHP No that you are currently dealing with

			// Obtains and Allocates ASIHP related parameters from input file
			if (GetCoilsInputFlag) { //First time subroutine has been entered
				GetIHPInput();
				//    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
				GetCoilsInputFlag = false;
			}

			if (CompIndex == 0) {
				DXCoilNum = FindItemInList(CompName, IntegratedHeatPumpUnits);
				if (DXCoilNum == 0) {
					ShowFatalError("Integrated Heat Pump not found=" + CompName);
				}
				CompIndex = DXCoilNum;
			}
			else {
				DXCoilNum = CompIndex;
				if (DXCoilNum > NumIHPs || DXCoilNum < 1) {
					ShowFatalError("SimIHP: Invalid CompIndex passed=" + TrimSigDigits(DXCoilNum) +
						", Number of Integrated HPs=" + TrimSigDigits(NumIHPs) + ", IHP name=" + CompName);
				}
				if (!CompName.empty() && CompName != IntegratedHeatPumpUnits(DXCoilNum).Name) {
					ShowFatalError("SimIHP: Invalid CompIndex passed=" + TrimSigDigits(DXCoilNum) +
						", Integrated HP name=" + CompName + ", stored Integrated HP Name for that index=" + IntegratedHeatPumpUnits(DXCoilNum).Name);
				}
			};

			if (IntegratedHeatPumpUnits(DXCoilNum).IHPCoilsSized == false) SizeIHP(DXCoilNum);

			InitializeIHP(DXCoilNum);

			switch (IntegratedHeatPumpUnits(DXCoilNum).CurMode)
			{
			case SCMode:
				if (false == IsCallbyWH)//process when called from air loop
				{
					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SCDWHCoolCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SCDWHWHCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SHDWHHeatCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SHDWHWHCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SCWHCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);

					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SCCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, PartLoadFrac,
						SpeedNum, SpeedRatio, SensLoad, LatentLoad, OnOffAirFlowRat);

					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SHCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).DWHCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
				}


				break;
			case SHMode:
				if (false == IsCallbyWH)//process when called from air loop
				{
					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SCDWHCoolCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SCDWHWHCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SHDWHHeatCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SHDWHWHCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SCWHCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SCCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);

					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SHCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, PartLoadFrac,
						SpeedNum, SpeedRatio, SensLoad, LatentLoad, OnOffAirFlowRat);

					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).DWHCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
				}

				break;
			case DWHMode:
				if (true == IsCallbyWH)//process when called from water loop
				{
					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SCDWHCoolCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SCDWHWHCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SHDWHHeatCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SHDWHWHCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SCWHCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SCCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SHCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);

					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).DWHCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, PartLoadFrac,
						SpeedNum, SpeedRatio, SensLoad, LatentLoad, OnOffAirFlowRat);
					//IntegratedHeatPumpUnits(DXCoilNum).TotalHeatingEnergyRate = VarSpeedCoil(IntegratedHeatPumpUnits(DXCoilNum).DWHCoilIndex).TotalHeatingEnergyRate;
				}

				break;
			case SCWHMatchSCMode:
				if (false == IsCallbyWH)//process when called from air loop
				{
					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SCDWHCoolCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SCDWHWHCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SHDWHHeatCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SHDWHWHCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SCCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).DWHCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);

					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SCWHCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, PartLoadFrac,
						SpeedNum, SpeedRatio, SensLoad, LatentLoad, OnOffAirFlowRat);
					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SHCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
				}


				break;
			case SCWHMatchWHMode:
				if (true == IsCallbyWH)//process when called from water loop
				{
					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SCDWHCoolCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SCDWHWHCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SHDWHHeatCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SHDWHWHCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SCCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).DWHCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);

					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SCWHCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, PartLoadFrac,
						SpeedNum, SpeedRatio, SensLoad, LatentLoad, OnOffAirFlowRat);

					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SHCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
				}

				break;
			case SCDWHMode:
				if (false == IsCallbyWH)//process when called from air loop
				{
					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SHDWHHeatCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SHDWHWHCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SCWHCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SCCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).DWHCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);

					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SCDWHWHCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, PartLoadFrac,
						SpeedNum, SpeedRatio, SensLoad, LatentLoad, OnOffAirFlowRat);
					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SCDWHCoolCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, PartLoadFrac,
						SpeedNum, SpeedRatio, SensLoad, LatentLoad, OnOffAirFlowRat);

					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SHCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
				}

				break;
			case SHDWHElecHeatOffMode:
			case SHDWHElecHeatOnMode:
				if (false == IsCallbyWH)//process when called from air loop
				{
					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SCDWHCoolCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SCDWHWHCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SCWHCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SCCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SHCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).DWHCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);

					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SHDWHWHCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, PartLoadFrac,
						SpeedNum, SpeedRatio, SensLoad, LatentLoad, OnOffAirFlowRat);
					SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SHDWHHeatCoilIndex,
						CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, PartLoadFrac,
						SpeedNum, SpeedRatio, SensLoad, LatentLoad, OnOffAirFlowRat);
				}

				break;
			case IdleMode:
			default://clear up
				SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SCDWHCoolCoilIndex,
					CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
				SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SCDWHWHCoilIndex,
					CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
				SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SHDWHHeatCoilIndex,
					CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
				SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SHDWHWHCoilIndex,
					CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
				SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SCWHCoilIndex,
					CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
				SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SCCoilIndex,
					CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
				SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SHCoilIndex,
					CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
				SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).DWHCoilIndex,
					CyclingScheme, MaxONOFFCyclesperHour, HPTimeConstant, FanDelayTime, CompOp, 0.0, 1, 0.0, 0.0, 0.0, OnOffAirFlowRat);
				break;
			}

			UpdateIHP(DXCoilNum);

		}


		void
			GetIHPInput()
		{

			// SUBROUTINE INFORMATION:
			//       AUTHOR         Bo Shen
			//       DATE WRITTEN   December, 2015
			//       RE-ENGINEERED  na

			// PURPOSE OF THIS SUBROUTINE:
			// Obtains input data for Integrated HPs and stores it in IHP data structures

			// METHODOLOGY EMPLOYED:
			// Uses "Get" routines to read in data.

			// REFERENCES:
			// n/a

			// Using/Aliasing
			using namespace InputProcessor;
			using namespace NodeInputManager;
			using BranchNodeConnections::TestCompSet;
			using BranchNodeConnections::SetUpCompSets;
			using GlobalNames::VerifyUniqueCoilName;
			using namespace OutputReportPredefined;
			using General::TrimSigDigits;
			using VariableSpeedCoils::VarSpeedCoil; 
			using VariableSpeedCoils::GetCoilIndexVariableSpeed;
			using VariableSpeedCoils::SetAirNodes;
			using VariableSpeedCoils::SetWaterNodes;

			// Locals
			// SUBROUTINE ARGUMENT DEFINITIONS:
			// na

			// SUBROUTINE PARAMETER DEFINITIONS:
			static std::string const RoutineName("GetIHPInput: "); // include trailing blank space

			// INTERFACE BLOCK SPECIFICATIONS
			// na

			// DERIVED TYPE DEFINITIONS
			// na

			// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
			int DXCoilNum; //No of IHP DX system
			int IHPNum; // The Water to Air HP that you are currently loading input into
			int NumASIHPs; // Counter for air-source integrated heat pumps

			int NumAlphas; // Number of variables in String format
			int NumNums; // Number of variables in Numeric format
			int NumParams; // Total number of input fields
			static int MaxNums(0); // Maximum number of numeric input fields
			static int MaxAlphas(0); // Maximum number of alpha input fields
			std::string CoilName; // Name of the  Coil
			std::string Coiltype; // type of coil

			std::string CurrentModuleObject; // for ease in getting objects
			std::string sIHPType; //specify IHP type
			Array1D_string AlphArray; // Alpha input items for object
			Array1D_string cAlphaFields; // Alpha field names
			Array1D_string cNumericFields; // Numeric field names
			Array1D< Real64 > NumArray; // Numeric input items for object
			Array1D_bool lAlphaBlanks; // Logical array, alpha field input BLANK = .TRUE.
			Array1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.

			static bool ErrorsFound(false); // If errors detected in input	
			int CoilCounter; // Counter

			int IOStat;
			int AlfaFieldIncre; // increment number of Alfa field

			bool IsNotOK; // Flag to verify name
			bool IsBlank; // Flag for blank name
			bool errFlag;
			int InNode(0);//inlet air or water node
			int OutNode(0);//outlet air or water node


			NumASIHPs = GetNumObjectsFound("COILSYSTEM:INTEGRATEDHEATPUMP:AIRSOURCE");
			NumIHPs = NumASIHPs;//later will add water source integrated HPs

			DXCoilNum = 0;

			if (NumIHPs <= 0) {
				ShowSevereError("No Equipment found in Integrated Heat Pumps");
				ErrorsFound = true;
			}

			// Allocate Arrays
			if (NumIHPs > 0) {
				IntegratedHeatPumpUnits.allocate(NumIHPs);
			}

			//air-source integrated heat pump
			GetObjectDefMaxArgs("COILSYSTEM:INTEGRATEDHEATPUMP:AIRSOURCE", NumParams, NumAlphas, NumNums);
			MaxNums = max(MaxNums, NumNums);
			MaxAlphas = max(MaxAlphas, NumAlphas);

			AlphArray.allocate(MaxAlphas);
			cAlphaFields.allocate(MaxAlphas);
			lAlphaBlanks.dimension(MaxAlphas, true);
			cNumericFields.allocate(MaxNums);
			lNumericBlanks.dimension(MaxNums, true);
			NumArray.dimension(MaxNums, 0.0);

			// Get the data for air-source IHPs
			CurrentModuleObject = "COILSYSTEM:INTEGRATEDHEATPUMP:AIRSOURCE"; //for reporting
			sIHPType = "COILSYSTEM:INTEGRATEDHEATPUMP:AIRSOURCE"; //for checking

			for (CoilCounter = 1; CoilCounter <= NumASIHPs; ++CoilCounter) {

				++DXCoilNum;
				AlfaFieldIncre = 1;

				GetObjectItem(CurrentModuleObject, CoilCounter, AlphArray, NumAlphas, NumArray, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields);

				IsNotOK = false;
				IsBlank = false;

				VerifyName(AlphArray(1), IntegratedHeatPumpUnits, DXCoilNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name");
				if (IsNotOK) {
					ErrorsFound = true;
					if (IsBlank) AlphArray(1) = "xxxxx";
				}
				VerifyUniqueCoilName(CurrentModuleObject, AlphArray(1), errFlag, CurrentModuleObject + " Name");
				if (errFlag) {
					ErrorsFound = true;
				}

				IntegratedHeatPumpUnits(DXCoilNum).NodeConnected = false;
				IntegratedHeatPumpUnits(DXCoilNum).Name = AlphArray(1);
				IntegratedHeatPumpUnits(DXCoilNum).IHPtype = "AIRSOURCE_IHP";

				IntegratedHeatPumpUnits(DXCoilNum).AirCoolInletNodeNum =
					GetOnlySingleNode(AlphArray(2), ErrorsFound, CurrentModuleObject, AlphArray(1), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent);
				IntegratedHeatPumpUnits(DXCoilNum).AirHeatInletNodeNum =
					GetOnlySingleNode(AlphArray(3), ErrorsFound, CurrentModuleObject, AlphArray(1), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent);
				IntegratedHeatPumpUnits(DXCoilNum).AirOutletNodeNum =
					GetOnlySingleNode(AlphArray(4), ErrorsFound, CurrentModuleObject, AlphArray(1), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent);
				IntegratedHeatPumpUnits(DXCoilNum).WaterInletNodeNum =
					GetOnlySingleNode(AlphArray(5), ErrorsFound, CurrentModuleObject, AlphArray(1), NodeType_Water, NodeConnectionType_Inlet, 2, ObjectIsNotParent);
				IntegratedHeatPumpUnits(DXCoilNum).WaterOutletNodeNum =
					GetOnlySingleNode(AlphArray(6), ErrorsFound, CurrentModuleObject, AlphArray(1), NodeType_Water, NodeConnectionType_Outlet, 2, ObjectIsNotParent);
				IntegratedHeatPumpUnits(DXCoilNum).WaterTankoutNod =
					GetOnlySingleNode(AlphArray(7), ErrorsFound, CurrentModuleObject, AlphArray(1), NodeType_Water, NodeConnectionType_Outlet, 2, ObjectIsNotParent);
				IntegratedHeatPumpUnits(DXCoilNum).ODAirInletNodeNum =
					GetOnlySingleNode(AlphArray(8), ErrorsFound, CurrentModuleObject, AlphArray(1), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent);
				IntegratedHeatPumpUnits(DXCoilNum).ODAirOutletNodeNum =
					GetOnlySingleNode(AlphArray(9), ErrorsFound, CurrentModuleObject, AlphArray(1), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent);

				//give up checking node connection rules as IHP doesn't apply
				//TestCompSet(CurrentModuleObject, AlphArray(1), AlphArray(2), AlphArray(3), "Air Nodes");
				//TestCompSet(CurrentModuleObject, AlphArray(1), AlphArray(3), AlphArray(4), "Air Nodes");
				//TestCompSet(CurrentModuleObject, AlphArray(1), AlphArray(5), AlphArray(6), "Water Nodes");

				//SetUpCompSets(sIHPType, AlphArray(1), "COIL:COOLING:DX:VARIABLESPEED", AlphArray(8), AlphArray(2), AlphArray(3));//add SC
				//SetUpCompSets(sIHPType, AlphArray(1), "COIL:HEATING:DX:VARIABLESPEED", AlphArray(9), AlphArray(3), AlphArray(4));//add SH
				//SetUpCompSets(sIHPType, AlphArray(1), "COIL:WATERHEATING:AIRTOWATERHEATPUMP:VARIABLESPEED", AlphArray(10), "", "");//add dwh
				//SetUpCompSets(sIHPType, AlphArray(1), "COIL:WATERHEATING:AIRTOWATERHEATPUMP:VARIABLESPEED", AlphArray(11), AlphArray(2), AlphArray(3));//add SCwh
				//SetUpCompSets(sIHPType, AlphArray(1), "COIL:COOLING:DX:VARIABLESPEED", AlphArray(12), AlphArray(2), AlphArray(3));//add SCdwh
				//SetUpCompSets(sIHPType, AlphArray(1), "COIL:WATERHEATING:AIRTOWATERHEATPUMP:VARIABLESPEED", AlphArray(13), "", "");//add dwh
				//SetUpCompSets(sIHPType, AlphArray(1), "COIL:HEATING:DX:VARIABLESPEED", AlphArray(14), AlphArray(3), AlphArray(4));//add SH
				//SetUpCompSets(sIHPType, AlphArray(1), "COIL:WATERHEATING:AIRTOWATERHEATPUMP:VARIABLESPEED", AlphArray(15), "", "");//add dwh

				IntegratedHeatPumpUnits(DXCoilNum).SCCoilType = "COIL:COOLING:DX:VARIABLESPEED";
				IntegratedHeatPumpUnits(DXCoilNum).SCCoilName = AlphArray(10);
				Coiltype = IntegratedHeatPumpUnits(DXCoilNum).SCCoilType;
				CoilName = IntegratedHeatPumpUnits(DXCoilNum).SCCoilName;

				ValidateComponent(Coiltype, CoilName, IsNotOK, CurrentModuleObject);
				if (IsNotOK) {
					ShowContinueError("...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
					ErrorsFound = true;
				}
				else {
					errFlag = false;
					IntegratedHeatPumpUnits(DXCoilNum).SCCoilIndex = GetCoilIndexVariableSpeed(Coiltype, CoilName, errFlag);
					if (errFlag) {
						ShowContinueError("...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
						ErrorsFound = true;
					}
				}


				IntegratedHeatPumpUnits(DXCoilNum).SHCoilType = "COIL:HEATING:DX:VARIABLESPEED";
				IntegratedHeatPumpUnits(DXCoilNum).SHCoilName = AlphArray(11);
				Coiltype = IntegratedHeatPumpUnits(DXCoilNum).SHCoilType;
				CoilName = IntegratedHeatPumpUnits(DXCoilNum).SHCoilName;

				ValidateComponent(Coiltype, CoilName, IsNotOK, CurrentModuleObject);
				if (IsNotOK) {
					ShowContinueError("...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
					ErrorsFound = true;
				}
				else {
					errFlag = false;
					IntegratedHeatPumpUnits(DXCoilNum).SHCoilIndex = GetCoilIndexVariableSpeed(Coiltype, CoilName, errFlag);
					if (errFlag) {
						ShowContinueError("...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
						ErrorsFound = true;
					}
				}

				IntegratedHeatPumpUnits(DXCoilNum).DWHCoilType = "COIL:WATERHEATING:AIRTOWATERHEATPUMP:VARIABLESPEED";
				IntegratedHeatPumpUnits(DXCoilNum).DWHCoilName = AlphArray(12);
				Coiltype = IntegratedHeatPumpUnits(DXCoilNum).DWHCoilType;
				CoilName = IntegratedHeatPumpUnits(DXCoilNum).DWHCoilName;

				ValidateComponent(Coiltype, CoilName, IsNotOK, CurrentModuleObject);
				if (IsNotOK) {
					ShowContinueError("...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
					ErrorsFound = true;
				}
				else {
					errFlag = false;
					IntegratedHeatPumpUnits(DXCoilNum).DWHCoilIndex = GetCoilIndexVariableSpeed(Coiltype, CoilName, errFlag);
					if (errFlag) {
						ShowContinueError("...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
						ErrorsFound = true;
					}
				}

				IntegratedHeatPumpUnits(DXCoilNum).SCWHCoilType = "COIL:WATERHEATING:AIRTOWATERHEATPUMP:VARIABLESPEED";
				IntegratedHeatPumpUnits(DXCoilNum).SCWHCoilName = AlphArray(13);
				Coiltype = IntegratedHeatPumpUnits(DXCoilNum).SCWHCoilType;
				CoilName = IntegratedHeatPumpUnits(DXCoilNum).SCWHCoilName;

				ValidateComponent(Coiltype, CoilName, IsNotOK, CurrentModuleObject);
				if (IsNotOK) {
					ShowContinueError("...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
					ErrorsFound = true;
				}
				else {
					errFlag = false;
					IntegratedHeatPumpUnits(DXCoilNum).SCWHCoilIndex = GetCoilIndexVariableSpeed(Coiltype, CoilName, errFlag);
					if (errFlag) {
						ShowContinueError("...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
						ErrorsFound = true;
					}
				}


				IntegratedHeatPumpUnits(DXCoilNum).SCDWHCoolCoilType = "COIL:COOLING:DX:VARIABLESPEED";
				IntegratedHeatPumpUnits(DXCoilNum).SCDWHCoolCoilName = AlphArray(14);
				Coiltype = IntegratedHeatPumpUnits(DXCoilNum).SCDWHCoolCoilType;
				CoilName = IntegratedHeatPumpUnits(DXCoilNum).SCDWHCoolCoilName;

				ValidateComponent(Coiltype, CoilName, IsNotOK, CurrentModuleObject);
				if (IsNotOK) {
					ShowContinueError("...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
					ErrorsFound = true;
				}
				else {
					errFlag = false;
					IntegratedHeatPumpUnits(DXCoilNum).SCDWHCoolCoilIndex = GetCoilIndexVariableSpeed(Coiltype, CoilName, errFlag);
					if (errFlag) {
						ShowContinueError("...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
						ErrorsFound = true;
					}
				}


				IntegratedHeatPumpUnits(DXCoilNum).SCDWHWHCoilType = "COIL:WATERHEATING:AIRTOWATERHEATPUMP:VARIABLESPEED";
				IntegratedHeatPumpUnits(DXCoilNum).SCDWHWHCoilName = AlphArray(15);
				Coiltype = IntegratedHeatPumpUnits(DXCoilNum).SCDWHWHCoilType;
				CoilName = IntegratedHeatPumpUnits(DXCoilNum).SCDWHWHCoilName;

				ValidateComponent(Coiltype, CoilName, IsNotOK, CurrentModuleObject);
				if (IsNotOK) {
					ShowContinueError("...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
					ErrorsFound = true;
				}
				else {
					errFlag = false;
					IntegratedHeatPumpUnits(DXCoilNum).SCDWHWHCoilIndex = GetCoilIndexVariableSpeed(Coiltype, CoilName, errFlag);
					if (errFlag) {
						ShowContinueError("...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
						ErrorsFound = true;
					}
					else{
						VarSpeedCoil(IntegratedHeatPumpUnits(DXCoilNum).SCDWHWHCoilIndex).bIsDesuperheater = true;
					}
				}

				IntegratedHeatPumpUnits(DXCoilNum).SHDWHHeatCoilType = "COIL:HEATING:DX:VARIABLESPEED";
				IntegratedHeatPumpUnits(DXCoilNum).SHDWHHeatCoilName = AlphArray(16);
				Coiltype = IntegratedHeatPumpUnits(DXCoilNum).SHDWHHeatCoilType;
				CoilName = IntegratedHeatPumpUnits(DXCoilNum).SHDWHHeatCoilName;

				ValidateComponent(Coiltype, CoilName, IsNotOK, CurrentModuleObject);
				if (IsNotOK) {
					ShowContinueError("...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
					ErrorsFound = true;
				}
				else {
					errFlag = false;
					IntegratedHeatPumpUnits(DXCoilNum).SHDWHHeatCoilIndex = GetCoilIndexVariableSpeed(Coiltype, CoilName, errFlag);
					if (errFlag) {
						ShowContinueError("...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
						ErrorsFound = true;
					}
				}

				IntegratedHeatPumpUnits(DXCoilNum).SHDWHWHCoilType = "COIL:WATERHEATING:AIRTOWATERHEATPUMP:VARIABLESPEED";
				IntegratedHeatPumpUnits(DXCoilNum).SHDWHWHCoilName = AlphArray(17);
				Coiltype = IntegratedHeatPumpUnits(DXCoilNum).SHDWHWHCoilType;
				CoilName = IntegratedHeatPumpUnits(DXCoilNum).SHDWHWHCoilName;

				ValidateComponent(Coiltype, CoilName, IsNotOK, CurrentModuleObject);
				if (IsNotOK) {
					ShowContinueError("...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
					ErrorsFound = true;
				}
				else {
					errFlag = false;
					IntegratedHeatPumpUnits(DXCoilNum).SHDWHWHCoilIndex = GetCoilIndexVariableSpeed(Coiltype, CoilName, errFlag);
					if (errFlag) {
						ShowContinueError("...specified in " + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
						ErrorsFound = true;
					}
					else{
						VarSpeedCoil(IntegratedHeatPumpUnits(DXCoilNum).SHDWHWHCoilIndex).bIsDesuperheater = true;
					}
				}

				IntegratedHeatPumpUnits(DXCoilNum).TindoorOverCoolAllow = NumArray(1);
				IntegratedHeatPumpUnits(DXCoilNum).TambientOverCoolAllow = NumArray(2);
				IntegratedHeatPumpUnits(DXCoilNum).TindoorWHHighPriority = NumArray(3);
				IntegratedHeatPumpUnits(DXCoilNum).TambientWHHighPriority = NumArray(4);
				IntegratedHeatPumpUnits(DXCoilNum).ModeMatchSCWH = int(NumArray(5));
				IntegratedHeatPumpUnits(DXCoilNum).MinSpedSCWH = int(NumArray(6));
				IntegratedHeatPumpUnits(DXCoilNum).WaterVolSCDWH = NumArray(7);
				IntegratedHeatPumpUnits(DXCoilNum).MinSpedSCDWH = int(NumArray(8));
				IntegratedHeatPumpUnits(DXCoilNum).TimeLimitSHDWH = NumArray(9);
				IntegratedHeatPumpUnits(DXCoilNum).MinSpedSHDWH = int(NumArray(10));

				if (false == IntegratedHeatPumpUnits(DXCoilNum).NodeConnected)
				{
					//air node connections
					InNode = IntegratedHeatPumpUnits(DXCoilNum).AirCoolInletNodeNum;
					OutNode = IntegratedHeatPumpUnits(DXCoilNum).AirHeatInletNodeNum;
					//all the SC air coils have indoor air nodes for cooling
					SetAirNodes(IntegratedHeatPumpUnits(DXCoilNum).SCCoilName, ErrorsFound, InNode, OutNode);
					SetAirNodes(IntegratedHeatPumpUnits(DXCoilNum).SCWHCoilName, ErrorsFound, InNode, OutNode);
					SetAirNodes(IntegratedHeatPumpUnits(DXCoilNum).SCDWHCoolCoilName, ErrorsFound, InNode, OutNode);

					InNode = IntegratedHeatPumpUnits(DXCoilNum).AirHeatInletNodeNum;
					OutNode = IntegratedHeatPumpUnits(DXCoilNum).AirOutletNodeNum;
					//all the SH air coils have indoor air nodes for heating
					SetAirNodes(IntegratedHeatPumpUnits(DXCoilNum).SHCoilName, ErrorsFound, InNode, OutNode);
					SetAirNodes(IntegratedHeatPumpUnits(DXCoilNum).SHDWHHeatCoilName, ErrorsFound, InNode, OutNode);

					//water node connections
					InNode = IntegratedHeatPumpUnits(DXCoilNum).WaterInletNodeNum;
					OutNode = IntegratedHeatPumpUnits(DXCoilNum).WaterOutletNodeNum;
					//all the water coils have the same water side node connnections
					SetWaterNodes(IntegratedHeatPumpUnits(DXCoilNum).SCWHCoilName, ErrorsFound, InNode, OutNode);
					SetWaterNodes(IntegratedHeatPumpUnits(DXCoilNum).SCDWHWHCoilName, ErrorsFound, InNode, OutNode);
					SetWaterNodes(IntegratedHeatPumpUnits(DXCoilNum).SHDWHWHCoilName, ErrorsFound, InNode, OutNode);
					SetWaterNodes(IntegratedHeatPumpUnits(DXCoilNum).DWHCoilName, ErrorsFound, InNode, OutNode);

					//outdoor air node connections
					//DWH, SCDWH, SHDWH coils have the same outdoor air nodes
					InNode = IntegratedHeatPumpUnits(DXCoilNum).ODAirInletNodeNum;
					OutNode = IntegratedHeatPumpUnits(DXCoilNum).ODAirOutletNodeNum;
					SetAirNodes(IntegratedHeatPumpUnits(DXCoilNum).DWHCoilName, ErrorsFound, InNode, OutNode);//SCDWHCoil has outdoor air nodes
					SetAirNodes(IntegratedHeatPumpUnits(DXCoilNum).SCDWHWHCoilName, ErrorsFound, InNode, OutNode);//SCDWHCoil has outdoor air nodes
					SetAirNodes(IntegratedHeatPumpUnits(DXCoilNum).SHDWHWHCoilName, ErrorsFound, InNode, OutNode);//SHDWHWHCoil has outdoor air nodes

					IntegratedHeatPumpUnits(DXCoilNum).NodeConnected = true;
				};


				IntegratedHeatPumpUnits(DXCoilNum).IHPCoilsSized = false;
				IntegratedHeatPumpUnits(DXCoilNum).CoolVolFlowScale = 1.0; //scale coil flow rates to match the parent fan object
				IntegratedHeatPumpUnits(DXCoilNum).HeatVolFlowScale = 1.0; //scale coil flow rates to match the parent fan object
				IntegratedHeatPumpUnits(DXCoilNum).CurMode = IdleMode;
				IntegratedHeatPumpUnits(DXCoilNum).MaxHeatAirMassFlow = 1e10;
				IntegratedHeatPumpUnits(DXCoilNum).MaxHeatAirVolFlow = 1e10;
				IntegratedHeatPumpUnits(DXCoilNum).MaxCoolAirMassFlow = 1e10;
				IntegratedHeatPumpUnits(DXCoilNum).MaxCoolAirVolFlow = 1e10;
			}


			if (ErrorsFound) {
				ShowFatalError(RoutineName + "Errors found in getting " + CurrentModuleObject + " input.  Preceding condition(s) causes termination.");
			}

		}

		void
			SizeIHP(int const DXCoilNum)
		{
			using VariableSpeedCoils::SimVariableSpeedCoils;
			using VariableSpeedCoils::SetVarSpeedCoilData;
			using VariableSpeedCoils::SizeVarSpeedCoil;
			using VariableSpeedCoils::VarSpeedCoil;
			using DataSizing::AutoSize;
			using General::TrimSigDigits;

			static bool ErrorsFound(false); // If errors detected in input
			Real64 RatedCapacity(0.0); //rated building cooling load

			// Obtains and Allocates AS-IHP related parameters from input file
			if (GetCoilsInputFlag) { //First time subroutine has been entered
				GetIHPInput();
				GetCoilsInputFlag = false;
			};

			if (DXCoilNum > NumIHPs || DXCoilNum < 1) {
				ShowFatalError("SizeIHP: Invalid CompIndex passed=" + TrimSigDigits(DXCoilNum) +
					", Number of Integrated HPs=" + TrimSigDigits(NumIHPs) + ", IHP name=" + "AS-IHP");
			}

			if (true == IntegratedHeatPumpUnits(DXCoilNum).IHPCoilsSized){ return; }

			//associate SC coil with SH coil
			SetVarSpeedCoilData(IntegratedHeatPumpUnits(DXCoilNum).SCCoilIndex, ErrorsFound, _, IntegratedHeatPumpUnits(DXCoilNum).SHCoilIndex);
			if (ErrorsFound) {
				ShowSevereError("SizeIHP: Could not match cooling coil\"" + IntegratedHeatPumpUnits(DXCoilNum).SCCoilName +
					"\" with heating coil=\"" + IntegratedHeatPumpUnits(DXCoilNum).SHCoilName + "\"");
				ErrorsFound = false;
			};

			SizeVarSpeedCoil(IntegratedHeatPumpUnits(DXCoilNum).SCCoilIndex);//size cooling coil
			if (ErrorsFound) {
				ShowFatalError("SizeIHP: failed to size SC coil\"" + IntegratedHeatPumpUnits(DXCoilNum).SCCoilName + "\"");
				ErrorsFound = false;
			}
			else
			{
				RatedCapacity = VarSpeedCoil(IntegratedHeatPumpUnits(DXCoilNum).SCCoilIndex).RatedCapCoolTotal;
			};

			SizeVarSpeedCoil(IntegratedHeatPumpUnits(DXCoilNum).SHCoilIndex);//size heating coil
			if (ErrorsFound) {
				ShowSevereError("SizeIHP: failed to size SH coil\"" + IntegratedHeatPumpUnits(DXCoilNum).SHCoilName + "\"");
				ErrorsFound = false;
			};

			//pass SC coil capacity to SCDWH cool coil
			if (VarSpeedCoil(IntegratedHeatPumpUnits(DXCoilNum).SCDWHCoolCoilIndex).RatedCapCoolTotal == AutoSize)
			{
				VarSpeedCoil(IntegratedHeatPumpUnits(DXCoilNum).SCDWHCoolCoilIndex).RatedCapCoolTotal = RatedCapacity;
			};

			//associate SCDWH air coil to SHDWH air coil
			SetVarSpeedCoilData(IntegratedHeatPumpUnits(DXCoilNum).SCDWHCoolCoilIndex, ErrorsFound, _, IntegratedHeatPumpUnits(DXCoilNum).SHDWHHeatCoilIndex);

			//size SCDWH air coil
			SizeVarSpeedCoil(IntegratedHeatPumpUnits(DXCoilNum).SCDWHCoolCoilIndex);
			if (ErrorsFound) {
				ShowSevereError("SizeIHP: failed to size SCDWH cooling coil\"" + IntegratedHeatPumpUnits(DXCoilNum).SCDWHCoolCoilName + "\"");
				ErrorsFound = false;
			};

			//size SHDWH air coil
			SizeVarSpeedCoil(IntegratedHeatPumpUnits(DXCoilNum).SHDWHHeatCoilIndex);
			if (ErrorsFound) {
				ShowSevereError("SizeIHP: failed to size SHDWH heating coil\"" + IntegratedHeatPumpUnits(DXCoilNum).SHDWHHeatCoilName + "\"");
				ErrorsFound = false;
			};


			//size the water coils below
			//size SCWH water coil
			if (VarSpeedCoil(IntegratedHeatPumpUnits(DXCoilNum).SCWHCoilIndex).RatedCapWH == AutoSize)
			{
				VarSpeedCoil(IntegratedHeatPumpUnits(DXCoilNum).SCWHCoilIndex).RatedCapWH =
					RatedCapacity / (1.0 - 1.0 / VarSpeedCoil(IntegratedHeatPumpUnits(DXCoilNum).SCWHCoilIndex).RatedCOPHeat);
			}

			SizeVarSpeedCoil(IntegratedHeatPumpUnits(DXCoilNum).SCWHCoilIndex);
			if (ErrorsFound) {
				ShowSevereError("SizeIHP: failed to size SCWH coil\"" + IntegratedHeatPumpUnits(DXCoilNum).SCWHCoilName + "\"");
				ErrorsFound = false;
			};

			//size DWH water coil
			if (VarSpeedCoil(IntegratedHeatPumpUnits(DXCoilNum).DWHCoilIndex).RatedCapWH == AutoSize)
			{
				VarSpeedCoil(IntegratedHeatPumpUnits(DXCoilNum).DWHCoilIndex).RatedCapWH = RatedCapacity;
			}

			SizeVarSpeedCoil(IntegratedHeatPumpUnits(DXCoilNum).DWHCoilIndex);
			if (ErrorsFound) {
				ShowSevereError("SizeIHP: failed to size DWH coil\"" + IntegratedHeatPumpUnits(DXCoilNum).DWHCoilName + "\"");
				ErrorsFound = false;
			};

			//size SCDWH water coil
			if (VarSpeedCoil(IntegratedHeatPumpUnits(DXCoilNum).SCDWHWHCoilIndex).RatedCapWH == AutoSize)
			{
				VarSpeedCoil(IntegratedHeatPumpUnits(DXCoilNum).SCDWHWHCoilIndex).RatedCapWH = RatedCapacity * 0.13;
			}

			SizeVarSpeedCoil(IntegratedHeatPumpUnits(DXCoilNum).SCDWHWHCoilIndex);
			if (ErrorsFound) {
				ShowSevereError("SizeIHP: failed to size SCDWH water heating coil\"" + IntegratedHeatPumpUnits(DXCoilNum).SCDWHWHCoilName + "\"");
				ErrorsFound = false;
			};


			//size SHDWH water coil
			if (VarSpeedCoil(IntegratedHeatPumpUnits(DXCoilNum).SHDWHWHCoilIndex).RatedCapWH == AutoSize)
			{
				VarSpeedCoil(IntegratedHeatPumpUnits(DXCoilNum).SHDWHWHCoilIndex).RatedCapWH = RatedCapacity * 0.1;
			}

			SizeVarSpeedCoil(IntegratedHeatPumpUnits(DXCoilNum).SHDWHWHCoilIndex);

			if (ErrorsFound) {
				ShowSevereError("SizeIHP: failed to size SHDWH water heating coil\"" + IntegratedHeatPumpUnits(DXCoilNum).SHDWHWHCoilName + "\"");
				ErrorsFound = false;
			};

			IntegratedHeatPumpUnits(DXCoilNum).IHPCoilsSized = true;
		}



		void
			InitializeIHP(int const DXCoilNum)
		{
			using General::TrimSigDigits;

			// Obtains and Allocates AS-IHP related parameters from input file
			if (GetCoilsInputFlag) { //First time subroutine has been entered
				GetIHPInput();
				GetCoilsInputFlag = false;
			}

			if (DXCoilNum > NumIHPs || DXCoilNum < 1) {
				ShowFatalError("InitializeIHP: Invalid CompIndex passed=" + TrimSigDigits(DXCoilNum) +
					", Number of Integrated HPs=" + TrimSigDigits(NumIHPs) + ", IHP name=" + "AS-IHP");
			}

		}


		void
			UpdateIHP(int const DXCoilNum)
		{
			using General::TrimSigDigits;

			// Obtains and Allocates AS-IHP related parameters from input file
			if (GetCoilsInputFlag) { //First time subroutine has been entered
				GetIHPInput();
				GetCoilsInputFlag = false;
			}

			if (DXCoilNum > NumIHPs || DXCoilNum < 1) {
				ShowFatalError("UpdateIHP: Invalid CompIndex passed=" + TrimSigDigits(DXCoilNum) +
					", Number of Integrated HPs=" + TrimSigDigits(NumIHPs) + ", IHP name=" + "AS-IHP");
			}


		}

		void
			DecideWorkMode(int const DXCoilNum,
			Real64 const SensLoad, // Sensible demand load [W]
			Real64 const LatentLoad // Latent demand load [W]
			)//shall be called from a air loop parent
		{
			//       AUTHOR         Bo Shen, ORNL
			//       DATE WRITTEN   March 2016
			//       RE-ENGINEERED  na

			// PURPOSE OF THIS SUBROUTINE:
			// This subroutine determine the IHP working mode in the next time step, 
			// it should be called by an air loop parent object, when FirstHVACIteration == true

			// METHODOLOGY EMPLOYED:

			// REFERENCES:
			// N/A

			// Using/Aliasing

			using DataHVACGlobals::SmallLoad;
			using DataEnvironment::OutDryBulbTemp;
			using WaterThermalTanks::SimWaterThermalTank;
			using WaterThermalTanks::GetWaterThermalTankInput;
			using DataHVACGlobals::TimeStepSys;
			using General::TrimSigDigits;

			Real64 MyLoad(0.0);
			Real64 MaxCap(0.0);
			Real64 MinCap(0.0);
			Real64 OptCap(0.0);
			Real64 WHHeatTimeSav(0.0);//time accumulation for water heating
			Real64 WHHeatVolSave(0.0);//volume accumulation for water heating

			// Obtains and Allocates AS-IHP related parameters from input file
			if (GetCoilsInputFlag) { //First time subroutine has been entered
				GetIHPInput();
				GetCoilsInputFlag = false;
			}

			if (DXCoilNum > NumIHPs || DXCoilNum < 1) {
				ShowFatalError("DecideWorkMode: Invalid CompIndex passed=" + TrimSigDigits(DXCoilNum) +
					", Number of Integrated HPs=" + TrimSigDigits(NumIHPs) + ", IHP name=" + "AS-IHP");
			}

			if (IntegratedHeatPumpUnits(DXCoilNum).IHPCoilsSized == false) SizeIHP(DXCoilNum);

			//decide working mode at the first moment
			//check if there is a water heating call
			IntegratedHeatPumpUnits(DXCoilNum).IsWHCallAvail = false;
			IntegratedHeatPumpUnits(DXCoilNum).CheckWHCall = true; //set checking flag
			if (0 == IntegratedHeatPumpUnits(DXCoilNum).WHtankID)//not initialized yet
			{
				IntegratedHeatPumpUnits(DXCoilNum).IsWHCallAvail = false;
			}
			else
			{
				Node(IntegratedHeatPumpUnits(DXCoilNum).WaterOutletNodeNum).Temp =
					Node(IntegratedHeatPumpUnits(DXCoilNum).WaterInletNodeNum).Temp;
				SimWaterThermalTank(
					IntegratedHeatPumpUnits(DXCoilNum).WHtankType, IntegratedHeatPumpUnits(DXCoilNum).WHtankName,
					IntegratedHeatPumpUnits(DXCoilNum).WHtankID,
					false, false,
					MyLoad, MaxCap, MinCap, OptCap, true // TRUE if First iteration of simulation
					);
			}
			IntegratedHeatPumpUnits(DXCoilNum).CheckWHCall = false;//clear checking flag

			//keep the water heating time and volume history
			WHHeatTimeSav = IntegratedHeatPumpUnits(DXCoilNum).SHDWHRunTime;
			if (SCDWHMode == IntegratedHeatPumpUnits(DXCoilNum).CurMode) {
				WHHeatVolSave = IntegratedHeatPumpUnits(DXCoilNum).WaterFlowAccumVol +
				Node(IntegratedHeatPumpUnits(DXCoilNum).WaterTankoutNod).MassFlowRate / 983.0 * TimeStepSys * SecInHour;//983 - water density at 60 C
			}
			else
			{
				WHHeatVolSave = 0.0; 
			}
			
			//clear the accumulation amount for other modes
			IntegratedHeatPumpUnits(DXCoilNum).SHDWHRunTime = 0.0;
			IntegratedHeatPumpUnits(DXCoilNum).WaterFlowAccumVol = 0.0;

			if (false == IntegratedHeatPumpUnits(DXCoilNum).IsWHCallAvail)//no water heating call
			{
				if ((SensLoad < (-1.0 * SmallLoad)) || (LatentLoad < (-1.0 * SmallLoad)))//space cooling mode
				{
					IntegratedHeatPumpUnits(DXCoilNum).CurMode = SCMode;
				}
				else if (SensLoad > SmallLoad)
				{
					if ((IntegratedHeatPumpUnits(DXCoilNum).ControlledZoneTemp > IntegratedHeatPumpUnits(DXCoilNum).TindoorOverCoolAllow) &&
						(OutDryBulbTemp > IntegratedHeatPumpUnits(DXCoilNum).TambientOverCoolAllow))//used for cooling season, avoid heating after SCWH mode
						IntegratedHeatPumpUnits(DXCoilNum).CurMode = IdleMode;
					else
						IntegratedHeatPumpUnits(DXCoilNum).CurMode = SHMode;
				}
				else
				{
					IntegratedHeatPumpUnits(DXCoilNum).CurMode = IdleMode;
				}
			}
			//below has water heating calls
			else if ((SensLoad < (-1.0 * SmallLoad)) || (LatentLoad < (-1.0 * SmallLoad)))//simultaneous SC and WH calls
			{
				if (WHHeatVolSave < IntegratedHeatPumpUnits(DXCoilNum).WaterVolSCDWH)//small water heating amount
				{
					IntegratedHeatPumpUnits(DXCoilNum).CurMode = SCDWHMode;
					IntegratedHeatPumpUnits(DXCoilNum).WaterFlowAccumVol = WHHeatVolSave;
				}
				else
				{
					if (1 == IntegratedHeatPumpUnits(DXCoilNum).ModeMatchSCWH) //water heating priority
						IntegratedHeatPumpUnits(DXCoilNum).CurMode = SCWHMatchWHMode;
					else  //space cooling piority
						IntegratedHeatPumpUnits(DXCoilNum).CurMode = SCWHMatchSCMode;
				};

			}
			else if ((IntegratedHeatPumpUnits(DXCoilNum).ControlledZoneTemp > IntegratedHeatPumpUnits(DXCoilNum).TindoorOverCoolAllow) &&
				(OutDryBulbTemp > IntegratedHeatPumpUnits(DXCoilNum).TambientOverCoolAllow))//over-cooling allowed, water heating priority
			{
				IntegratedHeatPumpUnits(DXCoilNum).CurMode = SCWHMatchWHMode;
			}
			else if ((IntegratedHeatPumpUnits(DXCoilNum).ControlledZoneTemp > IntegratedHeatPumpUnits(DXCoilNum).TindoorWHHighPriority) &&
				(OutDryBulbTemp > IntegratedHeatPumpUnits(DXCoilNum).TambientWHHighPriority)) //ignore space heating request
			{
				IntegratedHeatPumpUnits(DXCoilNum).CurMode = DWHMode;
			}
			else if (SensLoad > SmallLoad)
			{
				IntegratedHeatPumpUnits(DXCoilNum).SHDWHRunTime = WHHeatTimeSav + TimeStepSys * SecInHour;

				if (WHHeatTimeSav > IntegratedHeatPumpUnits(DXCoilNum).TimeLimitSHDWH)
				{
					IntegratedHeatPumpUnits(DXCoilNum).CurMode = SHDWHElecHeatOnMode;
				}
				else
				{
					IntegratedHeatPumpUnits(DXCoilNum).CurMode = SHDWHElecHeatOffMode;
				};
			}
			else
			{
				IntegratedHeatPumpUnits(DXCoilNum).CurMode = DWHMode;
			}

			//clear up, important
			ClearCoils(DXCoilNum);
		}

		void ClearCoils(int const DXCoilNum)
		{
			using VariableSpeedCoils::SimVariableSpeedCoils;
			using General::TrimSigDigits;

			Real64 EMP1(0.0), EMP2(0.0), EMP3(0.0); //place holder to calling clear up function
			int CycFanCycCoil(1);//fan cycl manner place holder

			// Obtains and Allocates WatertoAirHP related parameters from input file
			if (GetCoilsInputFlag) { //First time subroutine has been entered
				GetIHPInput();
				//    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
				GetCoilsInputFlag = false;
			}

			if (DXCoilNum > NumIHPs || DXCoilNum < 1) {
				ShowFatalError("ClearCoils: Invalid CompIndex passed=" + TrimSigDigits(DXCoilNum) +
					", Number of Integrated HPs=" + TrimSigDigits(NumIHPs) + ", IHP name=" + "AS-IHP");
			}

			//clear up
			SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SCDWHCoolCoilIndex, CycFanCycCoil, EMP1, EMP2, EMP3, 1, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0);
			SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SCDWHWHCoilIndex, CycFanCycCoil, EMP1, EMP2, EMP3, 1, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0);
			SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SHDWHHeatCoilIndex, CycFanCycCoil, EMP1, EMP2, EMP3, 1, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0);
			SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SHDWHWHCoilIndex, CycFanCycCoil, EMP1, EMP2, EMP3, 1, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0);
			SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SCWHCoilIndex, CycFanCycCoil, EMP1, EMP2, EMP3, 1, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0);
			SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SCCoilIndex, CycFanCycCoil, EMP1, EMP2, EMP3, 1, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0);
			SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).SHCoilIndex, CycFanCycCoil, EMP1, EMP2, EMP3, 1, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0);
			SimVariableSpeedCoils(BlankString, IntegratedHeatPumpUnits(DXCoilNum).DWHCoilIndex, CycFanCycCoil, EMP1, EMP2, EMP3, 1, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0);

			return;
		}

		int
			GetCurWorkMode(int const DXCoilNum)
		{
			using General::TrimSigDigits;

			// Obtains and Allocates WatertoAirHP related parameters from input file
			if (GetCoilsInputFlag) { //First time subroutine has been entered
				GetIHPInput();
				//    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
				GetCoilsInputFlag = false;
			}

			if (DXCoilNum > NumIHPs || DXCoilNum < 1) {
				ShowFatalError("GetCurWorkMode: Invalid CompIndex passed=" + TrimSigDigits(DXCoilNum) +
					", Number of Integrated HPs=" + TrimSigDigits(NumIHPs) + ", IHP name=" + "AS-IHP");
			}

			if (IntegratedHeatPumpUnits(DXCoilNum).IHPCoilsSized == false) SizeIHP(DXCoilNum);

			return(IntegratedHeatPumpUnits(DXCoilNum).CurMode);
		}


		int
			GetCoilIndexIHP(
			std::string const & CoilType, // must match coil types in this module
			std::string const & CoilName, // must match coil names for the coil type
			bool & ErrorsFound // set to true if problem
			)
		{

			// FUNCTION INFORMATION:
			//       AUTHOR         Bo Shen
			//       DATE WRITTEN   March 2016
			//       MODIFIED       na
			//       RE-ENGINEERED  na

			// PURPOSE OF THIS FUNCTION:
			// This function looks up the coil index for the given coil and returns it.  If
			// incorrect coil type or name is given, ErrorsFound is returned as true and index is returned
			// as zero.

			// METHODOLOGY EMPLOYED:
			// na

			// REFERENCES:
			// na

			// Using/Aliasing
			using InputProcessor::FindItemInList;

			// Return value
			int IndexNum; // returned index of matched coil

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

			// Obtains and Allocates WatertoAirHP related parameters from input file
			if (GetCoilsInputFlag) { //First time subroutine has been entered
				GetIHPInput();
				//    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
				GetCoilsInputFlag = false;
			}

			IndexNum = FindItemInList(CoilName, IntegratedHeatPumpUnits);

			if (IndexNum == 0) {
				ShowSevereError("GetCoilIndexIHP: Could not find CoilType=\"" + CoilType + "\" with Name=\"" + CoilName + "\"");
				ErrorsFound = true;
			}

			return IndexNum;
		}

		int
			GetCoilInletNodeIHP(
			std::string const & CoilType, // must match coil types in this module
			std::string const & CoilName, // must match coil names for the coil type
			bool & ErrorsFound // set to true if problem
			)

		{
			// FUNCTION INFORMATION:
			//       AUTHOR         Bo Shen
			//       DATE WRITTEN   March 2016
			//       MODIFIED       na
			//       RE-ENGINEERED  na

			// PURPOSE OF THIS FUNCTION:
			// This function looks up the given coil and returns the inlet node.  If
			// incorrect coil type or name is given, ErrorsFound is returned as true and value is returned
			// as zero.

			// METHODOLOGY EMPLOYED:
			// na

			// REFERENCES:
			// na

			// Using/Aliasing
			using InputProcessor::FindItemInList;

			// Return value
			int NodeNumber; // returned outlet node of matched coil

			// Locals
			// FUNCTION ARGUMENT DEFINITIONS:
			// FUNCTION PARAMETER DEFINITIONS:
			// na

			// INTERFACE BLOCK SPECIFICATIONS:
			// na

			// DERIVED TYPE DEFINITIONS:
			// na

			// FUNCTION LOCAL VARIABLE DECLARATIONS:
			int WhichCoil;

			// Obtains and Allocates WatertoAirHP related parameters from input file
			if (GetCoilsInputFlag) { //First time subroutine has been entered
				GetIHPInput();
				//    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
				GetCoilsInputFlag = false;
			}

			WhichCoil = FindItemInList(CoilName, IntegratedHeatPumpUnits);
			if (WhichCoil != 0) {
				NodeNumber = IntegratedHeatPumpUnits(WhichCoil).AirCoolInletNodeNum;
			}

			if (WhichCoil == 0) {
				ShowSevereError("GetCoilInletNodeIHP: Could not find CoilType=\"" + CoilType + "\" with Name=\"" + CoilName + "\"");
				ErrorsFound = true;
				NodeNumber = 0;
			}

			return NodeNumber;

		}

		int
			GetIHPDWHCoilPLFFPLR(
			std::string const & CoilType, // must match coil types in this module
			std::string const & CoilName, // must match coil names for the coil type
			int const Mode,//mode coil type
			bool & ErrorsFound // set to true if problem
			)
		{
			// FUNCTION INFORMATION:
			//       AUTHOR         Bo Shen
			//       DATE WRITTEN   March, 2016
			//       MODIFIED       na
			//       RE-ENGINEERED  na

			// PURPOSE OF THIS FUNCTION:
			// This function looks up the given coil and returns PLR curve index.  If
			// incorrect coil type or name is given, ErrorsFound is returned as true and value is returned
			// as zero.

			// METHODOLOGY EMPLOYED:
			// na

			// REFERENCES:
			// na

			// Using/Aliasing
			using InputProcessor::FindItemInList;
			using VariableSpeedCoils::GetVSCoilPLFFPLR;

			// Return value
			int PLRNumber; // returned outlet node of matched coil

			// Locals
			// FUNCTION ARGUMENT DEFINITIONS:
			// FUNCTION PARAMETER DEFINITIONS:
			// na

			// INTERFACE BLOCK SPECIFICATIONS:
			// na

			// DERIVED TYPE DEFINITIONS:
			// na

			// FUNCTION LOCAL VARIABLE DECLARATIONS:
			int WhichCoil;

			// Obtains and Allocates WatertoAirHP related parameters from input file
			if (GetCoilsInputFlag) { //First time subroutine has been entered
				GetIHPInput();
				//    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
				GetCoilsInputFlag = false;
			}

			WhichCoil = FindItemInList(CoilName, IntegratedHeatPumpUnits);
			if (WhichCoil != 0) {
				//this will be called by HPWH parent
				if (IntegratedHeatPumpUnits(WhichCoil).DWHCoilIndex > 0)
					PLRNumber = GetVSCoilPLFFPLR(IntegratedHeatPumpUnits(WhichCoil).DWHCoilType, IntegratedHeatPumpUnits(WhichCoil).DWHCoilName, ErrorsFound);
				else
					PLRNumber = GetVSCoilPLFFPLR(IntegratedHeatPumpUnits(WhichCoil).SCWHCoilType, IntegratedHeatPumpUnits(WhichCoil).SCWHCoilName, ErrorsFound);
			}
			else {
				WhichCoil = 0;
			}

			if (WhichCoil == 0) {
				ShowSevereError("GetIHPDWHCoilPLFFPLR: Could not find CoilType=\"" + CoilType + "\" with Name=\"" + CoilName + "\"");
				ErrorsFound = true;
				PLRNumber = 0;
			}

			return PLRNumber;
		}


		Real64
			GetDWHCoilCapacityIHP(
			std::string const & CoilType, // must match coil types in this module
			std::string const & CoilName, // must match coil names for the coil type
			int const Mode,//mode coil type
			bool & ErrorsFound // set to true if problem
			)
		{

			// FUNCTION INFORMATION:
			//       AUTHOR         Bo Shen
			//       DATE WRITTEN   Jan 2016
			//       MODIFIED       na
			//       RE-ENGINEERED  na

			// PURPOSE OF THIS FUNCTION:
			// This function looks up the rated coil capacity at the nominal speed level for the given coil and returns it.  If
			// incorrect coil type or name is given, ErrorsFound is returned as true and capacity is returned
			// as negative.

			// METHODOLOGY EMPLOYED:
			// na

			// REFERENCES:
			// na

			// Using/Aliasing
			using InputProcessor::FindItemInList;
			using VariableSpeedCoils::GetCoilCapacityVariableSpeed;

			// Return value
			Real64 CoilCapacity; // returned capacity of matched coil

			// Locals
			// FUNCTION ARGUMENT DEFINITIONS:

			// FUNCTION PARAMETER DEFINITIONS:
			// na

			// INTERFACE BLOCK SPECIFICATIONS:
			// na

			// DERIVED TYPE DEFINITIONS:
			// na

			// FUNCTION LOCAL VARIABLE DECLARATIONS:
			int WhichCoil;

			// Obtains and Allocates WatertoAirHP related parameters from input file
			if (GetCoilsInputFlag) { //First time subroutine has been entered
				GetIHPInput();
				//    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
				GetCoilsInputFlag = false;
			}

			WhichCoil = FindItemInList(CoilName, IntegratedHeatPumpUnits);
			if (WhichCoil != 0) {

				if (IntegratedHeatPumpUnits(WhichCoil).IHPCoilsSized == false) SizeIHP(WhichCoil);

				if (IntegratedHeatPumpUnits(WhichCoil).DWHCoilIndex > 0)
				{
					CoilCapacity =
						GetCoilCapacityVariableSpeed(IntegratedHeatPumpUnits(WhichCoil).DWHCoilType, IntegratedHeatPumpUnits(WhichCoil).DWHCoilName, ErrorsFound);
				}
				else{
					CoilCapacity =
						GetCoilCapacityVariableSpeed(IntegratedHeatPumpUnits(WhichCoil).SCWHCoilType, IntegratedHeatPumpUnits(WhichCoil).SCWHCoilName, ErrorsFound);
				}
			}
			else {
				WhichCoil = 0;
			}

			if (WhichCoil == 0) {
				ShowSevereError("GetCoilCapacityVariableSpeed: Could not find CoilType=\"" + CoilType + "\" with Name=\"" + CoilName + "\"");
				ErrorsFound = true;
				CoilCapacity = -1000.0;
			}

			return CoilCapacity;
		}

		int
			GetLowSpeedNumIHP(int const DXCoilNum)
		{
			using General::TrimSigDigits;

			int SpeedNum(0);

			// Obtains and Allocates WatertoAirHP related parameters from input file
			if (GetCoilsInputFlag) { //First time subroutine has been entered
				GetIHPInput();
				//    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
				GetCoilsInputFlag = false;
			}

			if (DXCoilNum > NumIHPs || DXCoilNum < 1) {
				ShowFatalError("GetLowSpeedNumIHP: Invalid CompIndex passed=" + TrimSigDigits(DXCoilNum) +
					", Number of Integrated HPs=" + TrimSigDigits(NumIHPs) + ", IHP name=" + "AS-IHP");
			}

			switch (IntegratedHeatPumpUnits(DXCoilNum).CurMode)
			{
			case IdleMode:
				SpeedNum = 1;
				break;
			case SCMode:
				SpeedNum = 1;
				break;
			case SHMode:
				SpeedNum = 1;
				break;
			case DWHMode:
				SpeedNum = 1;
				break;
			case SCWHMatchSCMode:
			case SCWHMatchWHMode:
				SpeedNum = IntegratedHeatPumpUnits(DXCoilNum).MinSpedSCWH;
				break;
			case SCDWHMode:
				SpeedNum = IntegratedHeatPumpUnits(DXCoilNum).MinSpedSCDWH;
				break;
			case SHDWHElecHeatOffMode:
			case SHDWHElecHeatOnMode:
				SpeedNum = IntegratedHeatPumpUnits(DXCoilNum).MinSpedSHDWH;
				break;
			default:
				SpeedNum = 1;
				break;
			}

			return(SpeedNum);
		}

		int
			GetMaxSpeedNumIHP(int const DXCoilNum)
		{
			using VariableSpeedCoils::VarSpeedCoil;
			using General::TrimSigDigits;

			// Obtains and Allocates WatertoAirHP related parameters from input file
			if (GetCoilsInputFlag) { //First time subroutine has been entered
				GetIHPInput();
				//    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
				GetCoilsInputFlag = false;
			}

			if (DXCoilNum > NumIHPs || DXCoilNum < 1) {
				ShowFatalError("GetMaxSpeedNumIHP: Invalid CompIndex passed=" + TrimSigDigits(DXCoilNum) +
					", Number of Integrated HPs=" + TrimSigDigits(NumIHPs) + ", IHP name=" + "AS-IHP");
			}

			int SpeedNum(0);

			switch (IntegratedHeatPumpUnits(DXCoilNum).CurMode)
			{
			case IdleMode:
				SpeedNum = VarSpeedCoil(IntegratedHeatPumpUnits(DXCoilNum).SCCoilIndex).NumOfSpeeds;
				break;
			case SCMode:
				SpeedNum = VarSpeedCoil(IntegratedHeatPumpUnits(DXCoilNum).SCCoilIndex).NumOfSpeeds;
				break;
			case SHMode:
				SpeedNum = VarSpeedCoil(IntegratedHeatPumpUnits(DXCoilNum).SHCoilIndex).NumOfSpeeds;
				break;
			case DWHMode:
				SpeedNum = VarSpeedCoil(IntegratedHeatPumpUnits(DXCoilNum).DWHCoilIndex).NumOfSpeeds;
				break;
			case SCWHMatchSCMode:
			case SCWHMatchWHMode:
				SpeedNum = VarSpeedCoil(IntegratedHeatPumpUnits(DXCoilNum).SCWHCoilIndex).NumOfSpeeds;
				break;
			case SCDWHMode:
				SpeedNum = VarSpeedCoil(IntegratedHeatPumpUnits(DXCoilNum).SCDWHCoolCoilIndex).NumOfSpeeds;
				break;
			case SHDWHElecHeatOffMode:
			case SHDWHElecHeatOnMode:
				SpeedNum = VarSpeedCoil(IntegratedHeatPumpUnits(DXCoilNum).SHDWHHeatCoilIndex).NumOfSpeeds;
				break;
			default:
				SpeedNum = VarSpeedCoil(IntegratedHeatPumpUnits(DXCoilNum).SCCoilIndex).NumOfSpeeds;
				break;
			}

			return(SpeedNum);
		}

		Real64
			GetAirVolFlowRateIHP(int const DXCoilNum, int const SpeedNum, Real64 const SpeedRatio,
			bool const IsCallbyWH //whether the call from the water heating loop or air loop, true = from water heating loop
			)
		{
			using VariableSpeedCoils::VarSpeedCoil;
			using General::TrimSigDigits;

			int IHPCoilIndex(0);
			Real64 AirVolFlowRate(0.0);
			Real64 FlowScale(1.0);
			bool IsResultFlow(false); //IsResultFlow = true, the air flow rate will be from a simultaneous mode, won't be re-calculated

			// Obtains and Allocates WatertoAirHP related parameters from input file
			if (GetCoilsInputFlag) { //First time subroutine has been entered
				GetIHPInput();
				GetCoilsInputFlag = false;
			}

			if (DXCoilNum > NumIHPs || DXCoilNum < 1) {
				ShowFatalError("GetAirVolFlowRateIHP: Invalid CompIndex passed=" + TrimSigDigits(DXCoilNum) +
					", Number of Integrated HPs=" + TrimSigDigits(NumIHPs) + ", IHP name=" + "AS-IHP");
			}

			if (IntegratedHeatPumpUnits(DXCoilNum).IHPCoilsSized == false) SizeIHP(DXCoilNum);

			FlowScale = 0.0;
			switch (IntegratedHeatPumpUnits(DXCoilNum).CurMode)
			{
			case IdleMode:
				IHPCoilIndex = IntegratedHeatPumpUnits(DXCoilNum).SCCoilIndex;
				break;
			case SCMode:
				IHPCoilIndex = IntegratedHeatPumpUnits(DXCoilNum).SCCoilIndex;
				if (false == IsCallbyWH)//call from air loop
				{
					FlowScale = IntegratedHeatPumpUnits(DXCoilNum).CoolVolFlowScale;
				}

				break;
			case SHMode:
				IHPCoilIndex = IntegratedHeatPumpUnits(DXCoilNum).SHCoilIndex;
				if (false == IsCallbyWH)//call from air loop
				{
					FlowScale = IntegratedHeatPumpUnits(DXCoilNum).HeatVolFlowScale;
				}
				break;
			case DWHMode:
				IHPCoilIndex = IntegratedHeatPumpUnits(DXCoilNum).DWHCoilIndex;
				FlowScale = 1.0;
				break;
			case SCWHMatchSCMode:
				IHPCoilIndex = IntegratedHeatPumpUnits(DXCoilNum).SCWHCoilIndex;
				FlowScale = IntegratedHeatPumpUnits(DXCoilNum).CoolVolFlowScale;
				if (true == IsCallbyWH)//call from water loop
				{
					IsResultFlow = true;
					AirVolFlowRate = VarSpeedCoil(IntegratedHeatPumpUnits(DXCoilNum).SCWHCoilIndex).AirVolFlowRate;
				}
				break;
			case SCWHMatchWHMode:
				IHPCoilIndex = IntegratedHeatPumpUnits(DXCoilNum).SCWHCoilIndex;
				FlowScale = IntegratedHeatPumpUnits(DXCoilNum).CoolVolFlowScale;
				if (false == IsCallbyWH)
				{
					IsResultFlow = true;
					AirVolFlowRate = VarSpeedCoil(IntegratedHeatPumpUnits(DXCoilNum).SCWHCoilIndex).AirVolFlowRate;
				}
				break;
			case SCDWHMode:
				IHPCoilIndex = IntegratedHeatPumpUnits(DXCoilNum).SCDWHCoolCoilIndex;
				FlowScale = IntegratedHeatPumpUnits(DXCoilNum).CoolVolFlowScale;
				if (true == IsCallbyWH)
				{
					IsResultFlow = true;
					AirVolFlowRate = VarSpeedCoil(IntegratedHeatPumpUnits(DXCoilNum).SCDWHCoolCoilIndex).AirVolFlowRate;
				}
				break;
			case SHDWHElecHeatOffMode:
			case SHDWHElecHeatOnMode:
				IHPCoilIndex = IntegratedHeatPumpUnits(DXCoilNum).SHDWHHeatCoilIndex;
				FlowScale = IntegratedHeatPumpUnits(DXCoilNum).HeatVolFlowScale;
				if (true == IsCallbyWH)
				{
					IsResultFlow = true;
					AirVolFlowRate = VarSpeedCoil(IntegratedHeatPumpUnits(DXCoilNum).SHDWHHeatCoilIndex).AirVolFlowRate;
				}
				break;
			default:
				IHPCoilIndex = IntegratedHeatPumpUnits(DXCoilNum).SCCoilIndex;
				FlowScale = 0.0;
				break;
			}


			if (false == IsResultFlow){
				if (1 == SpeedNum)  AirVolFlowRate = VarSpeedCoil(IHPCoilIndex).MSRatedAirVolFlowRate(SpeedNum);
				else AirVolFlowRate = SpeedRatio * VarSpeedCoil(IHPCoilIndex).MSRatedAirVolFlowRate(SpeedNum) +
					(1.0 - SpeedRatio) * VarSpeedCoil(IHPCoilIndex).MSRatedAirVolFlowRate(SpeedNum - 1);

				AirVolFlowRate = AirVolFlowRate * FlowScale;
			}


			if (AirVolFlowRate > IntegratedHeatPumpUnits(DXCoilNum).MaxCoolAirVolFlow) AirVolFlowRate = IntegratedHeatPumpUnits(DXCoilNum).MaxCoolAirVolFlow;
			if (AirVolFlowRate > IntegratedHeatPumpUnits(DXCoilNum).MaxHeatAirVolFlow) AirVolFlowRate = IntegratedHeatPumpUnits(DXCoilNum).MaxHeatAirVolFlow;

			return(AirVolFlowRate);
		}

		Real64
			GetWaterVolFlowRateIHP(int const DXCoilNum, int const SpeedNum, Real64 const SpeedRatio,
			bool const IsCallbyWH //whether the call from the water heating loop or air loop, true = from water heating loop
			)
		{
			using VariableSpeedCoils::VarSpeedCoil;
			using General::TrimSigDigits;

			int IHPCoilIndex(0);
			Real64 WaterVolFlowRate(0.0);


			// Obtains and Allocates WatertoAirHP related parameters from input file
			if (GetCoilsInputFlag) { //First time subroutine has been entered
				GetIHPInput();
				GetCoilsInputFlag = false;
			}

			if (DXCoilNum > NumIHPs || DXCoilNum < 1) {
				ShowFatalError("GetWaterVolFlowRateIHP: Invalid CompIndex passed=" + TrimSigDigits(DXCoilNum) +
					", Number of Integrated HPs=" + TrimSigDigits(NumIHPs) + ", IHP name=" + "AS-IHP");
			}

			if (IntegratedHeatPumpUnits(DXCoilNum).IHPCoilsSized == false) SizeIHP(DXCoilNum);

			switch (IntegratedHeatPumpUnits(DXCoilNum).CurMode)
			{
			case IdleMode:
				WaterVolFlowRate = 0.0;
				break;
			case SCMode:
				WaterVolFlowRate = VarSpeedCoil(IntegratedHeatPumpUnits(DXCoilNum).SCCoilIndex).WaterVolFlowRate;
				break;
			case SHMode:
				WaterVolFlowRate = VarSpeedCoil(IntegratedHeatPumpUnits(DXCoilNum).SHCoilIndex).WaterVolFlowRate;
				break;
			case DWHMode:
				IHPCoilIndex = IntegratedHeatPumpUnits(DXCoilNum).DWHCoilIndex;
				if (1 == SpeedNum)  WaterVolFlowRate = VarSpeedCoil(IHPCoilIndex).MSRatedWaterVolFlowRate(SpeedNum);
				else WaterVolFlowRate = SpeedRatio * VarSpeedCoil(IHPCoilIndex).MSRatedWaterVolFlowRate(SpeedNum) +
					(1.0 - SpeedRatio) * VarSpeedCoil(IHPCoilIndex).MSRatedWaterVolFlowRate(SpeedNum - 1);
				break;
			case SCWHMatchSCMode:
				WaterVolFlowRate = VarSpeedCoil(IntegratedHeatPumpUnits(DXCoilNum).SCWHCoilIndex).WaterVolFlowRate;
				break;
			case SCWHMatchWHMode:
				IHPCoilIndex = IntegratedHeatPumpUnits(DXCoilNum).SCWHCoilIndex;
				if (1 == SpeedNum)  WaterVolFlowRate = VarSpeedCoil(IHPCoilIndex).MSRatedWaterVolFlowRate(SpeedNum);
				else WaterVolFlowRate = SpeedRatio * VarSpeedCoil(IHPCoilIndex).MSRatedWaterVolFlowRate(SpeedNum) +
					(1.0 - SpeedRatio) * VarSpeedCoil(IHPCoilIndex).MSRatedWaterVolFlowRate(SpeedNum - 1);
				break;
			case SCDWHMode:
				WaterVolFlowRate = VarSpeedCoil(IntegratedHeatPumpUnits(DXCoilNum).SCDWHWHCoilIndex).WaterVolFlowRate;
				break;
			case SHDWHElecHeatOffMode:
			case SHDWHElecHeatOnMode:
				WaterVolFlowRate = VarSpeedCoil(IntegratedHeatPumpUnits(DXCoilNum).SHDWHWHCoilIndex).WaterVolFlowRate;
				break;
			default:
				WaterVolFlowRate = 0.0;
				break;
			}

			return(WaterVolFlowRate);
		}

		Real64
			GetAirMassFlowRateIHP(int const DXCoilNum, int const SpeedNum, Real64 const SpeedRatio,
			bool const IsCallbyWH //whether the call from the water heating loop or air loop, true = from water heating loop
			)
		{
			using VariableSpeedCoils::VarSpeedCoil;
			using General::TrimSigDigits;

			int IHPCoilIndex(0);
			Real64 AirMassFlowRate(0.0);
			Real64 FlowScale(1.0);
			bool IsResultFlow(false); //IsResultFlow = true, the air flow rate will be from a simultaneous mode, won't be re-calculated

			// Obtains and Allocates WatertoAirHP related parameters from input file
			if (GetCoilsInputFlag) { //First time subroutine has been entered
				GetIHPInput();
				GetCoilsInputFlag = false;
			}

			if (DXCoilNum > NumIHPs || DXCoilNum < 1) {
				ShowFatalError("GetAirMassFlowRateIHP: Invalid CompIndex passed=" + TrimSigDigits(DXCoilNum) +
					", Number of Integrated HPs=" + TrimSigDigits(NumIHPs) + ", IHP name=" + "AS-IHP");
			}

			if (IntegratedHeatPumpUnits(DXCoilNum).IHPCoilsSized == false) SizeIHP(DXCoilNum);

			FlowScale = 0.0;
			switch (IntegratedHeatPumpUnits(DXCoilNum).CurMode)
			{
			case IdleMode:
				IHPCoilIndex = IntegratedHeatPumpUnits(DXCoilNum).SCCoilIndex;
				break;
			case SCMode:
				IHPCoilIndex = IntegratedHeatPumpUnits(DXCoilNum).SCCoilIndex;
				if (false == IsCallbyWH)
				{
					FlowScale = IntegratedHeatPumpUnits(DXCoilNum).CoolVolFlowScale;
				}
				break;
			case SHMode:
				IHPCoilIndex = IntegratedHeatPumpUnits(DXCoilNum).SHCoilIndex;
				if (false == IsCallbyWH)
				{
					FlowScale = IntegratedHeatPumpUnits(DXCoilNum).HeatVolFlowScale;
				}
				break;
			case DWHMode:
				IHPCoilIndex = IntegratedHeatPumpUnits(DXCoilNum).DWHCoilIndex;
				FlowScale = 1.0;
				break;
			case SCWHMatchSCMode:
				IHPCoilIndex = IntegratedHeatPumpUnits(DXCoilNum).SCWHCoilIndex;
				FlowScale = IntegratedHeatPumpUnits(DXCoilNum).CoolVolFlowScale;
				if (true == IsCallbyWH)
				{
					IsResultFlow = true;
					AirMassFlowRate = VarSpeedCoil(IntegratedHeatPumpUnits(DXCoilNum).SCWHCoilIndex).AirMassFlowRate;
				}
				break;
			case SCWHMatchWHMode:
				IHPCoilIndex = IntegratedHeatPumpUnits(DXCoilNum).SCWHCoilIndex;
				FlowScale = IntegratedHeatPumpUnits(DXCoilNum).CoolVolFlowScale;
				if (false == IsCallbyWH)
				{
					IsResultFlow = true;
					AirMassFlowRate = VarSpeedCoil(IntegratedHeatPumpUnits(DXCoilNum).SCWHCoilIndex).AirMassFlowRate;
				}
				break;
			case SCDWHMode:
				IHPCoilIndex = IntegratedHeatPumpUnits(DXCoilNum).SCDWHCoolCoilIndex;
				FlowScale = IntegratedHeatPumpUnits(DXCoilNum).CoolVolFlowScale;
				if (true == IsCallbyWH)
				{
					IsResultFlow = true;
					AirMassFlowRate = VarSpeedCoil(IntegratedHeatPumpUnits(DXCoilNum).SCDWHCoolCoilIndex).AirMassFlowRate;
				}
				break;
			case SHDWHElecHeatOffMode:
			case SHDWHElecHeatOnMode:
				IHPCoilIndex = IntegratedHeatPumpUnits(DXCoilNum).SHDWHHeatCoilIndex;
				FlowScale = IntegratedHeatPumpUnits(DXCoilNum).HeatVolFlowScale;
				if (true == IsCallbyWH)
				{
					IsResultFlow = true;
					AirMassFlowRate = VarSpeedCoil(IntegratedHeatPumpUnits(DXCoilNum).SHDWHHeatCoilIndex).AirMassFlowRate;
				}
				break;
			default:
				IHPCoilIndex = IntegratedHeatPumpUnits(DXCoilNum).SCCoilIndex;
				FlowScale = 0.0;
				break;
			}


			if (false == IsResultFlow){
				if (1 == SpeedNum)  AirMassFlowRate = VarSpeedCoil(IHPCoilIndex).MSRatedAirMassFlowRate(SpeedNum);
				else AirMassFlowRate = SpeedRatio * VarSpeedCoil(IHPCoilIndex).MSRatedAirMassFlowRate(SpeedNum) +
					(1.0 - SpeedRatio) * VarSpeedCoil(IHPCoilIndex).MSRatedAirMassFlowRate(SpeedNum - 1);

				AirMassFlowRate = AirMassFlowRate * FlowScale;
			}

			if (AirMassFlowRate > IntegratedHeatPumpUnits(DXCoilNum).MaxCoolAirMassFlow) AirMassFlowRate = IntegratedHeatPumpUnits(DXCoilNum).MaxCoolAirMassFlow;
			if (AirMassFlowRate > IntegratedHeatPumpUnits(DXCoilNum).MaxHeatAirMassFlow) AirMassFlowRate = IntegratedHeatPumpUnits(DXCoilNum).MaxHeatAirMassFlow;

			//set max air flow rate
			Node(IntegratedHeatPumpUnits(DXCoilNum).AirCoolInletNodeNum).MassFlowRateMax = AirMassFlowRate;
			Node(IntegratedHeatPumpUnits(DXCoilNum).AirHeatInletNodeNum).MassFlowRateMax = AirMassFlowRate;
			Node(IntegratedHeatPumpUnits(DXCoilNum).AirOutletNodeNum).MassFlowRateMax = AirMassFlowRate;

			return(AirMassFlowRate);
		}

		void
			ConnectIHP(
			int const WhichCoil // must match coil names for the coil type
			)
		{
			using VariableSpeedCoils::SetAirNodes;
			using VariableSpeedCoils::SetWaterNodes;
			using General::TrimSigDigits;

			int DXCoilNum(0);
			int InNode(0);
			int OutNode(0);
			bool ErrorsFound(false);

			// Obtains and Allocates WatertoAirHP related parameters from input file
			if (GetCoilsInputFlag) { //First time subroutine has been entered
				GetIHPInput();
				GetCoilsInputFlag = false;
			}

			if (DXCoilNum > NumIHPs || DXCoilNum < 1) {
				ShowFatalError("ConnectIHP: Invalid CompIndex passed=" + TrimSigDigits(DXCoilNum) +
					", Number of Integrated HPs=" + TrimSigDigits(NumIHPs) + ", IHP name=" + "AS-IHP");
			}

			if (WhichCoil != 0) {
				//air node connections
				InNode = IntegratedHeatPumpUnits(DXCoilNum).AirCoolInletNodeNum;
				OutNode = IntegratedHeatPumpUnits(DXCoilNum).AirHeatInletNodeNum;
				//all the SC air coils have indoor air nodes for cooling
				SetAirNodes(IntegratedHeatPumpUnits(DXCoilNum).SCCoilName, ErrorsFound, InNode, OutNode);
				SetAirNodes(IntegratedHeatPumpUnits(DXCoilNum).SCWHCoilName, ErrorsFound, InNode, OutNode);
				SetAirNodes(IntegratedHeatPumpUnits(DXCoilNum).SCDWHCoolCoilName, ErrorsFound, InNode, OutNode);

				InNode = IntegratedHeatPumpUnits(DXCoilNum).AirHeatInletNodeNum;
				OutNode = IntegratedHeatPumpUnits(DXCoilNum).AirOutletNodeNum;
				//all the SH air coils have indoor air nodes for heating
				SetAirNodes(IntegratedHeatPumpUnits(DXCoilNum).SHCoilName, ErrorsFound, InNode, OutNode);
				SetAirNodes(IntegratedHeatPumpUnits(DXCoilNum).SHDWHHeatCoilName, ErrorsFound, InNode, OutNode);

				//water node connections
				InNode = IntegratedHeatPumpUnits(DXCoilNum).WaterInletNodeNum;
				OutNode = IntegratedHeatPumpUnits(DXCoilNum).WaterOutletNodeNum;
				//all the water coils have the same water side node connnections
				SetWaterNodes(IntegratedHeatPumpUnits(DXCoilNum).SCWHCoilName, ErrorsFound, InNode, OutNode);
				SetWaterNodes(IntegratedHeatPumpUnits(DXCoilNum).SCDWHWHCoilName, ErrorsFound, InNode, OutNode);
				SetWaterNodes(IntegratedHeatPumpUnits(DXCoilNum).SHDWHWHCoilName, ErrorsFound, InNode, OutNode);
				SetWaterNodes(IntegratedHeatPumpUnits(DXCoilNum).DWHCoilName, ErrorsFound, InNode, OutNode);

				//outdoor air node connections
				//DWH, SCDWH, SHDWH coils have the same outdoor air nodes
				InNode = IntegratedHeatPumpUnits(DXCoilNum).ODAirInletNodeNum;
				OutNode = IntegratedHeatPumpUnits(DXCoilNum).ODAirOutletNodeNum;
				SetAirNodes(IntegratedHeatPumpUnits(DXCoilNum).DWHCoilName, ErrorsFound, InNode, OutNode);//SCDWHCoil has outdoor air nodes
				SetAirNodes(IntegratedHeatPumpUnits(DXCoilNum).SCDWHWHCoilName, ErrorsFound, InNode, OutNode);//SCDWHCoil has outdoor air nodes
				SetAirNodes(IntegratedHeatPumpUnits(DXCoilNum).SHDWHWHCoilName, ErrorsFound, InNode, OutNode);//SHDWHWHCoil has outdoor air nodes

				IntegratedHeatPumpUnits(DXCoilNum).NodeConnected = true;
			}

		}

		void
			DisconnectIHP(
			int const WhichCoil // must match coil names for the coil type
			)
		{
			using VariableSpeedCoils::SetAirNodes;
			using VariableSpeedCoils::SetWaterNodes;
			using General::TrimSigDigits;

			int DXCoilNum(0);
			int InNode(0);
			int OutNode(0);
			bool ErrorsFound(false);

			// Obtains and Allocates WatertoAirHP related parameters from input file
			if (GetCoilsInputFlag) { //First time subroutine has been entered
				GetIHPInput();
				GetCoilsInputFlag = false;
			}

			if (DXCoilNum > NumIHPs || DXCoilNum < 1) {
				ShowFatalError("DisconnectIHP: Invalid CompIndex passed=" + TrimSigDigits(DXCoilNum) +
					", Number of Integrated HPs=" + TrimSigDigits(NumIHPs) + ", IHP name=" + "AS-IHP");
			}

			if (WhichCoil != 0) {

				//air node connections
				InNode = 0;
				OutNode = 0;
				//all the SC air coils have indoor air nodes for cooling
				SetAirNodes(IntegratedHeatPumpUnits(DXCoilNum).SCCoilName, ErrorsFound, InNode, OutNode);
				SetAirNodes(IntegratedHeatPumpUnits(DXCoilNum).SCWHCoilName, ErrorsFound, InNode, OutNode);
				SetAirNodes(IntegratedHeatPumpUnits(DXCoilNum).SCDWHCoolCoilName, ErrorsFound, InNode, OutNode);

				InNode = 0;
				OutNode = 0;
				//all the SH air coils have indoor air nodes for heating
				SetAirNodes(IntegratedHeatPumpUnits(DXCoilNum).SHCoilName, ErrorsFound, InNode, OutNode);
				SetAirNodes(IntegratedHeatPumpUnits(DXCoilNum).SHDWHHeatCoilName, ErrorsFound, InNode, OutNode);

				//water node connections
				InNode = 0;
				OutNode = 0;
				//all the water coils have the same water side node connnections
				SetWaterNodes(IntegratedHeatPumpUnits(DXCoilNum).SCWHCoilName, ErrorsFound, InNode, OutNode);
				SetWaterNodes(IntegratedHeatPumpUnits(DXCoilNum).SCDWHWHCoilName, ErrorsFound, InNode, OutNode);
				SetWaterNodes(IntegratedHeatPumpUnits(DXCoilNum).SHDWHWHCoilName, ErrorsFound, InNode, OutNode);
				SetWaterNodes(IntegratedHeatPumpUnits(DXCoilNum).DWHCoilName, ErrorsFound, InNode, OutNode);

				//outdoor air node connections
				//DWH, SCDWH, SHDWH coils have the same outdoor air nodes
				InNode = 0;
				OutNode = 0;
				SetAirNodes(IntegratedHeatPumpUnits(DXCoilNum).DWHCoilName, ErrorsFound, InNode, OutNode);//SCDWHCoil has outdoor air nodes
				SetAirNodes(IntegratedHeatPumpUnits(DXCoilNum).SCDWHWHCoilName, ErrorsFound, InNode, OutNode);//SCDWHCoil has outdoor air nodes
				SetAirNodes(IntegratedHeatPumpUnits(DXCoilNum).SHDWHWHCoilName, ErrorsFound, InNode, OutNode);//SHDWHWHCoil has outdoor air nodes

				IntegratedHeatPumpUnits(DXCoilNum).NodeConnected = false;
			}


		}

		//     NOTICE

		//     Copyright (c) 1996-2015 The Board of Trustees of the University of Illinois
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

	} // IntegratedHeatPumps

} // EnergyPlus