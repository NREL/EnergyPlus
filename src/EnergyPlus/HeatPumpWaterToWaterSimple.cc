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

// C++ Headers
#include <cmath>

// EnergyPlus Headers
#include <HeatPumpWaterToWaterSimple.hh>
#include <BranchNodeConnections.hh>
#include <DataEnvironment.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <GlobalNames.hh>
#include <InputProcessing/InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <OutputReportPredefined.hh>
#include <PlantUtilities.hh>
#include <ReportSizingManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace HeatPumpWaterToWaterSimple {

	// MODULE INFORMATION:
	//       AUTHOR         Kenneth Tang
	//       DATE WRITTEN   March 2005
	//       MODIFIED       Brent Griffith, plant upgrades, fluid properties
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This module simulates a Water-to-Water Heat Pump Simple (Equation-Fit Model)

	// METHODOLOGY EMPLOYED:
	// This simulation is based on a set of coefficients generated from
	// the manufacturer catalog data using the generalized least square method

	// REFERENCES:
	// (1) Tang,C.C.. 2005. Modeling Packaged Heat Pumps in a Quasi-Steady
	// State Energy Simulation Program. M.S. Thesis, Department of Mechanical and Aerospace Engineering,
	// Oklahoma State University. (downloadable from http://www.hvac.okstate.edu/)
	// (2) Murugappan, Arun. 2002. Implementing Ground Source Heat Pump and Ground
	// Loop Heat Exchanger Models in the EnergyPlus Simulation Environment,
	// M.S. Thesis, Department of Mechanical and Aerospace Engineering,
	// Oklahoma State University. (downloadable from http://www.hvac.okstate.edu/)

	// OTHER NOTES: none

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::BeginSimFlag;
	using DataGlobals::BeginEnvrnFlag;
	using DataGlobals::HourOfDay;
	using DataGlobals::KelvinConv;
	using DataGlobals::TimeStep;
	using DataGlobals::TimeStepZone;
	using DataGlobals::DayOfSim;
	using DataGlobals::WarmupFlag;
	using DataGlobals::SecInHour;
	using General::TrimSigDigits;
	using namespace DataLoopNode;

	// MODULE PARAMETER DEFINITIONS
	std::string const HPEqFitHeating( "HeatPump:WatertoWater:EquationFit:Heating" );
	std::string const HPEqFitHeatingUC( "HEATPUMP:WATERTOWATER:EQUATIONFIT:HEATING" );
	std::string const HPEqFitCooling( "HeatPump:WatertoWater:EquationFit:Cooling" );
	std::string const HPEqFitCoolingUC( "HEATPUMP:WATERTOWATER:EQUATIONFIT:COOLING" );

	// MODULE VARIABLE DECLARATIONS:
	int NumGSHPs( 0 ); // Number of GSHPs specified in input
	namespace {
		bool GetInputFlag( true ); // then TRUE, calls subroutine to read input file.
		bool InitWatertoWaterHPOneTimeFlag( true );
	}

	// Object Data
	Array1D< GshpSpecs > GSHP;
	Array1D< ReportVars > GSHPReport;
	std::unordered_map< std::string, std::string > HeatPumpWaterUniqueNames;

	void
	clear_state(){
		NumGSHPs = 0;
		GetInputFlag = true;
		InitWatertoWaterHPOneTimeFlag = true;
		HeatPumpWaterUniqueNames.clear();
		GSHP.deallocate();
		GSHPReport.deallocate();
	}

	void
	SimHPWatertoWaterSimple(
		std::string const & GSHPType, // Type of GSHP
		int const GSHPTypeNum, // Type of GSHP in Plant equipment
		std::string const & GSHPName, // User Specified Name of GSHP
		int & CompIndex, // Index of Equipment
		bool const FirstHVACIteration,
		bool & InitLoopEquip, // If not zero, calculate the max load for operating conditions
		Real64 const MyLoad, // Loop demand component will meet
		Real64 & MaxCap, // Maximum operating capacity of GSHP [W]
		Real64 & MinCap, // Minimum operating capacity of GSHP [W]
		Real64 & OptCap, // Optimal operating capacity of GSHP [W]
		int const LoopNum, // The calling loop number
		bool const getCompSizFac, // true if calling to get component sizing factor
		Real64 & sizingFac // component level sizing factor
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Kenneth Tang
		//       DATE WRITTEN   March 2005
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine manages Water-to-Water Heat Pump Simple (Equation-Fit Model)

		//Get input from IDF
		if ( GetInputFlag ) {
			GetWatertoWaterHPInput();
			GetInputFlag = false;
		}

		int GSHPNum( 0 );
		if ( CompIndex == 0 ) {
			GSHPNum = UtilityRoutines::FindItemInList( GSHPName, GSHP );
			if ( GSHPNum == 0 ) {
				ShowFatalError( "SimHPWatertoWaterSimple: Specified heat pump not one of valid heat pumps. Heat pump = " + GSHPName );
			}
			CompIndex = GSHPNum;
		} else {
			GSHPNum = CompIndex;
			if ( GSHPNum > NumGSHPs || GSHPNum < 1 ) {
				ShowFatalError( "SimHPWatertoWaterSimple: Invalide component index pass = " + General::TrimSigDigits( GSHPNum ) + ", number of units = " + General::TrimSigDigits( NumGSHPs )+ ", entered unit name=" + GSHPName );
			}
			if ( GSHP( GSHPNum ).checkEquipName ) {
				if ( GSHPName != GSHP( GSHPNum ).Name ) {
					ShowFatalError( "SimHPWatertoWaterSimple: Invalid CompIndex passed=" + General::TrimSigDigits( GSHPNum ) + ", Unit name=" + GSHPName + ", stored Unit Name for that index=" + GSHP( GSHPNum ).Name );
				}
				GSHP( GSHPNum ).checkEquipName = false;
			}
		}

		if ( InitLoopEquip ) {
			InitWatertoWaterHP( GSHPTypeNum, GSHPName, GSHPNum, FirstHVACIteration, MyLoad );
			if( LoopNum == GSHP( GSHPNum ).LoadLoopNum ) {
				if ( GSHPTypeNum == DataPlant::TypeOf_HPWaterEFCooling ) {
					sizeCoolingWaterToWaterHP( GSHPNum );
					MinCap = 0.0;
					MaxCap = GSHP( GSHPNum ).RatedCapCool;
					OptCap = GSHP( GSHPNum ).RatedCapCool;
				} else if ( GSHPTypeNum == DataPlant::TypeOf_HPWaterEFHeating ) {
					sizeHeatingWaterToWaterHP( GSHPNum );
					MinCap = 0.0;
					MaxCap = GSHP( GSHPNum ).RatedCapHeat;
					OptCap = GSHP( GSHPNum ).RatedCapHeat;
				} else {
					ShowFatalError( "SimHPWatertoWaterSimple: Module called with incorrect GSHPType=" + GSHPType );
				}
			} else {
				MinCap = 0.0;
				MaxCap = 0.0;
				OptCap = 0.0;
			}
			if ( getCompSizFac ) {
				sizingFac = GSHP( GSHPNum ).sizFac;
			}

			return;

		}

		if ( GSHPTypeNum == DataPlant::TypeOf_HPWaterEFCooling ) {
			if ( GSHPNum != 0 ) {
				if ( LoopNum == GSHP( GSHPNum ).LoadLoopNum ) { // chilled water loop

					InitWatertoWaterHP( GSHPTypeNum, GSHPName, GSHPNum, FirstHVACIteration, MyLoad );
					CalcWatertoWaterHPCooling( GSHPNum, MyLoad );
					UpdateGSHPRecords( GSHPNum );

				} else if ( LoopNum == GSHP( GSHPNum ).SourceLoopNum ) { // condenser loop
					PlantUtilities::UpdateChillerComponentCondenserSide( GSHP( GSHPNum ).SourceLoopNum, GSHP( GSHPNum ).SourceLoopSideNum, DataPlant::TypeOf_HPWaterEFCooling, GSHP( GSHPNum ).SourceSideInletNodeNum, GSHP( GSHPNum ).SourceSideOutletNodeNum, GSHPReport( GSHPNum ).QSource, GSHPReport( GSHPNum ).SourceSideInletTemp, GSHPReport( GSHPNum ).SourceSideOutletTemp, GSHPReport( GSHPNum ).SourceSideMassFlowRate, FirstHVACIteration );
				} else {
					ShowFatalError( "SimHPWatertoWaterSimple:: Invalid loop connection " + HPEqFitCooling + ", Requested Unit=" + GSHPName );
				}
			} else {
				ShowFatalError( "SimHPWatertoWaterSimple:: Invalid " + HPEqFitCooling + ", Requested Unit=" + GSHPName );
			}
		} else if ( GSHPTypeNum == DataPlant::TypeOf_HPWaterEFHeating ) {
			if ( GSHPNum != 0 ) {
				if ( LoopNum == GSHP( GSHPNum ).LoadLoopNum ) { // chilled water loop

					InitWatertoWaterHP( GSHPTypeNum, GSHPName, GSHPNum, FirstHVACIteration, MyLoad );
					CalcWatertoWaterHPHeating( GSHPNum, MyLoad );
					UpdateGSHPRecords( GSHPNum );
				} else if ( LoopNum == GSHP( GSHPNum ).SourceLoopNum ) { // condenser loop
					PlantUtilities::UpdateChillerComponentCondenserSide( GSHP( GSHPNum ).SourceLoopNum, GSHP( GSHPNum ).SourceLoopSideNum, DataPlant::TypeOf_HPWaterEFHeating, GSHP( GSHPNum ).SourceSideInletNodeNum, GSHP( GSHPNum ).SourceSideOutletNodeNum, - GSHPReport( GSHPNum ).QSource, GSHPReport( GSHPNum ).SourceSideInletTemp, GSHPReport( GSHPNum ).SourceSideOutletTemp, GSHPReport( GSHPNum ).SourceSideMassFlowRate, FirstHVACIteration );
				} else {
					ShowFatalError( "SimHPWatertoWaterSimple:: Invalid loop connection " + HPEqFitCooling + ", Requested Unit=" + GSHPName );
				}
			} else {
				ShowFatalError( "SimHPWatertoWaterSimple:: Invalid " + HPEqFitHeating + ", Requested Unit=" + GSHPName );
			}
		} else {
			ShowFatalError( "SimHPWatertoWaterSimple: Module called with incorrect GSHPType=" + GSHPType );
		} // TypeOfEquip

	}

	void
	GetWatertoWaterHPInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Kenneth Tang
		//       DATE WRITTEN   March 2005
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Obtain input from IDF and store them in data structures

		// Using/Aliasing
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::TestCompSet;
		using PlantUtilities::RegisterPlantCompDesignFlow;
		using DataPlant::TypeOf_HPWaterEFCooling;
		using DataPlant::TypeOf_HPWaterEFHeating;
		using DataPlant::ScanPlantLoopsForObject;

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int GSHPNum; // GSHP number
		int HPNum; // Counter
		int NumCoolCoil; // Number of Cooling Coils
		int NumHeatCoil; // Number of Heating Coils
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		int IOStat; // IO Status when calling get input subroutine

		static bool ErrorsFound( false );
		bool errFlag;

		NumCoolCoil = inputProcessor->getNumObjectsFound( HPEqFitCoolingUC );
		NumHeatCoil = inputProcessor->getNumObjectsFound( HPEqFitHeatingUC );
		NumGSHPs = NumCoolCoil + NumHeatCoil;

		if ( NumGSHPs <= 0 ) {
			ShowSevereError( "GetEquationFitWaterToWater Input: No Equipment found" );
			ErrorsFound = true;
		}

		if ( NumGSHPs > 0 ) {
			GSHP.allocate( NumGSHPs );
			HeatPumpWaterUniqueNames.reserve( NumGSHPs );
			GSHPReport.allocate( NumGSHPs );
			// initialize the data structures
		}

		//Load data structure for cooling coil
		for ( HPNum = 1; HPNum <= NumCoolCoil; ++HPNum ) {

			GSHPNum = HPNum;

			inputProcessor->getObjectItem( HPEqFitCoolingUC, HPNum, DataIPShortCuts::cAlphaArgs, NumAlphas, DataIPShortCuts::rNumericArgs, NumNums, IOStat,DataIPShortCuts::lNumericFieldBlanks, DataIPShortCuts::lAlphaFieldBlanks );
			GlobalNames::VerifyUniqueInterObjectName( HeatPumpWaterUniqueNames, DataIPShortCuts::cAlphaArgs( 1 ), HPEqFitCoolingUC, ErrorsFound );
			GSHP( GSHPNum ).WWHPPlantTypeOfNum = TypeOf_HPWaterEFCooling;
			GSHP( GSHPNum ).Name = DataIPShortCuts::cAlphaArgs( 1 );
			GSHP( GSHPNum ).RatedLoadVolFlowCool = DataIPShortCuts::rNumericArgs( 1 );
			if ( GSHP( GSHPNum ).RatedLoadVolFlowCool == DataSizing::AutoSize ) {
				GSHP( GSHPNum ).ratedLoadVolFlowCoolWasAutoSized = true;
			}
			GSHP( GSHPNum ).RatedSourceVolFlowCool = DataIPShortCuts::rNumericArgs( 2 );
			if ( GSHP( GSHPNum ).RatedSourceVolFlowCool == DataSizing::AutoSize ) {
				GSHP( GSHPNum ).ratedSourceVolFlowCoolWasAutoSized = true;
			}
			GSHP( GSHPNum ).RatedCapCool = DataIPShortCuts::rNumericArgs( 3 );
			if ( GSHP( GSHPNum ).RatedCapCool == DataSizing::AutoSize ) {
				GSHP( GSHPNum ).ratedCapCoolWasAutoSized = true;
			}
			GSHP( GSHPNum ).RatedPowerCool = DataIPShortCuts::rNumericArgs( 4 );
			if ( GSHP( GSHPNum ).RatedPowerCool == DataSizing::AutoSize ) {
				GSHP( GSHPNum ).ratedPowerCoolWasAutoSized = true;
			}
			GSHP( GSHPNum ).CoolCap1 = DataIPShortCuts::rNumericArgs( 5 );
			GSHP( GSHPNum ).CoolCap2 = DataIPShortCuts::rNumericArgs( 6 );
			GSHP( GSHPNum ).CoolCap3 = DataIPShortCuts::rNumericArgs( 7 );
			GSHP( GSHPNum ).CoolCap4 = DataIPShortCuts::rNumericArgs( 8 );
			GSHP( GSHPNum ).CoolCap5 = DataIPShortCuts::rNumericArgs( 9 );
			GSHP( GSHPNum ).CoolPower1 = DataIPShortCuts::rNumericArgs( 10 );
			GSHP( GSHPNum ).CoolPower2 = DataIPShortCuts::rNumericArgs( 11 );
			GSHP( GSHPNum ).CoolPower3 = DataIPShortCuts::rNumericArgs( 12 );
			GSHP( GSHPNum ).CoolPower4 = DataIPShortCuts::rNumericArgs( 13 );
			GSHP( GSHPNum ).CoolPower5 = DataIPShortCuts::rNumericArgs( 14 );

			if ( NumNums > 14 ) {
				if ( ! DataIPShortCuts::lNumericFieldBlanks( 15 ) ) {
					GSHP( GSHPNum ).refCOP = DataIPShortCuts::rNumericArgs( 15 );
				} else {
					GSHP( GSHPNum ).refCOP = 8.0;
				}

			} else {
				GSHP( GSHPNum ).refCOP = 8.0;
			}

			//calculate reference COP if hard sized
			if ( ! GSHP( GSHPNum ).ratedPowerCoolWasAutoSized && ! GSHP( GSHPNum ).ratedCapCoolWasAutoSized && GSHP( GSHPNum ).RatedPowerCool > 0.0 ) {
				GSHP( GSHPNum ).refCOP = GSHP( GSHPNum ).RatedCapCool / GSHP( GSHPNum ).RatedPowerCool;
			}

			if ( NumNums > 15 ) {
				if ( ! DataIPShortCuts::lNumericFieldBlanks( 16 ) ) {
					GSHP( GSHPNum ).sizFac = DataIPShortCuts::rNumericArgs( 16 );
				} else {
					GSHP( GSHPNum ).sizFac = 1.0;
				}
			} else {
				GSHP( GSHPNum ).sizFac = 1.0;
			}

			GSHP( GSHPNum ).SourceSideInletNodeNum = GetOnlySingleNode( DataIPShortCuts::cAlphaArgs( 2 ), ErrorsFound, HPEqFitCoolingUC, DataIPShortCuts::cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

			GSHP( GSHPNum ).SourceSideOutletNodeNum = GetOnlySingleNode( DataIPShortCuts::cAlphaArgs( 3 ), ErrorsFound, HPEqFitCoolingUC, DataIPShortCuts::cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			GSHP( GSHPNum ).LoadSideInletNodeNum = GetOnlySingleNode( DataIPShortCuts::cAlphaArgs( 4 ), ErrorsFound, HPEqFitCoolingUC, DataIPShortCuts::cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 2, ObjectIsNotParent );

			GSHP( GSHPNum ).LoadSideOutletNodeNum = GetOnlySingleNode( DataIPShortCuts::cAlphaArgs( 5 ), ErrorsFound, HPEqFitCoolingUC, DataIPShortCuts::cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 2, ObjectIsNotParent );

			// Test node sets
			TestCompSet( HPEqFitCoolingUC, DataIPShortCuts::cAlphaArgs( 1 ), DataIPShortCuts::cAlphaArgs( 2 ), DataIPShortCuts::cAlphaArgs( 3 ), "Condenser Water Nodes" );
			TestCompSet( HPEqFitCoolingUC, DataIPShortCuts::cAlphaArgs( 1 ), DataIPShortCuts::cAlphaArgs( 4 ), DataIPShortCuts::cAlphaArgs( 5 ), "Chilled Water Nodes" );

			if ( NumAlphas > 5 && ! DataIPShortCuts::lAlphaFieldBlanks( 6 ) ) {
				GSHP( GSHPNum ).companionName = DataIPShortCuts::cAlphaArgs( 6 );
			}

			// CurrentModuleObject='HeatPump:WatertoWater:EquationFit:Cooling'
			SetupOutputVariable( "Water to Water Heat Pump Electric Energy", OutputProcessor::Unit::J, GSHPReport( GSHPNum ).Energy, "System", "Sum", GSHP( GSHPNum ).Name, _, "Electricity", "Cooling", _, "Plant" );
			SetupOutputVariable( "Water to Water Heat Pump Load Side Heat Transfer Energy", OutputProcessor::Unit::J, GSHPReport( GSHPNum ).QLoadEnergy, "System", "Sum", GSHP( GSHPNum ).Name );
			SetupOutputVariable( "Water to Water Heat Pump Source Side Heat Transfer Energy", OutputProcessor::Unit::J, GSHPReport( GSHPNum ).QSourceEnergy, "System", "Sum", GSHP( GSHPNum ).Name );
		}

		//Load data structure for heating coil
		for ( HPNum = 1; HPNum <= NumHeatCoil; ++HPNum ) {

			GSHPNum = NumCoolCoil + HPNum;

			inputProcessor->getObjectItem( HPEqFitHeatingUC, HPNum, DataIPShortCuts::cAlphaArgs, NumAlphas, DataIPShortCuts::rNumericArgs, NumNums, IOStat, DataIPShortCuts::lNumericFieldBlanks, DataIPShortCuts::lAlphaFieldBlanks );
			GlobalNames::VerifyUniqueInterObjectName( HeatPumpWaterUniqueNames, DataIPShortCuts::cAlphaArgs( 1 ), HPEqFitHeatingUC, ErrorsFound );
			GSHP( GSHPNum ).WWHPPlantTypeOfNum = TypeOf_HPWaterEFHeating;
			GSHP( GSHPNum ).Name = DataIPShortCuts::cAlphaArgs( 1 );
			GSHP( GSHPNum ).RatedLoadVolFlowHeat = DataIPShortCuts::rNumericArgs( 1 );
			if ( GSHP( GSHPNum ).RatedLoadVolFlowHeat == DataSizing::AutoSize ) {
				GSHP( GSHPNum ).ratedLoadVolFlowHeatWasAutoSized = true;
			}
			GSHP( GSHPNum ).RatedSourceVolFlowHeat = DataIPShortCuts::rNumericArgs( 2 );
			if ( GSHP( GSHPNum ).RatedSourceVolFlowHeat == DataSizing::AutoSize ) {
				GSHP( GSHPNum ).ratedSourceVolFlowHeatWasAutoSized = true;
			}
			GSHP( GSHPNum ).RatedCapHeat = DataIPShortCuts::rNumericArgs( 3 );
			if ( GSHP( GSHPNum ).RatedCapHeat == DataSizing::AutoSize ) {
				GSHP( GSHPNum ).ratedCapHeatWasAutoSized = true;
			}
			GSHP( GSHPNum ).RatedPowerHeat = DataIPShortCuts::rNumericArgs( 4 );
			if ( GSHP( GSHPNum ).RatedPowerHeat == DataSizing::AutoSize ) {
				GSHP( GSHPNum ).ratedPowerHeatWasAutoSized = true;
			}

			GSHP( GSHPNum ).HeatCap1 = DataIPShortCuts::rNumericArgs( 5 );
			GSHP( GSHPNum ).HeatCap2 = DataIPShortCuts::rNumericArgs( 6 );
			GSHP( GSHPNum ).HeatCap3 = DataIPShortCuts::rNumericArgs( 7 );
			GSHP( GSHPNum ).HeatCap4 = DataIPShortCuts::rNumericArgs( 8 );
			GSHP( GSHPNum ).HeatCap5 = DataIPShortCuts::rNumericArgs( 9 );
			GSHP( GSHPNum ).HeatPower1 = DataIPShortCuts::rNumericArgs( 10 );
			GSHP( GSHPNum ).HeatPower2 = DataIPShortCuts::rNumericArgs( 11 );
			GSHP( GSHPNum ).HeatPower3 = DataIPShortCuts::rNumericArgs( 12 );
			GSHP( GSHPNum ).HeatPower4 = DataIPShortCuts::rNumericArgs( 13 );
			GSHP( GSHPNum ).HeatPower5 = DataIPShortCuts::rNumericArgs( 14 );

			if ( NumNums > 14 ) {
				if ( ! DataIPShortCuts::lNumericFieldBlanks( 15 ) ) {
					GSHP( GSHPNum ).refCOP = DataIPShortCuts::rNumericArgs( 15 );
				} else {
					GSHP( GSHPNum ).refCOP = 7.5;
				}

			} else {
				GSHP( GSHPNum ).refCOP = 7.5;
			}

			//calculate reference COP if hard sized
			if ( ! GSHP( GSHPNum ).ratedPowerHeatWasAutoSized && ! GSHP( GSHPNum ).ratedCapHeatWasAutoSized && GSHP( GSHPNum ).RatedPowerHeat > 0.0 ) {
				GSHP( GSHPNum ).refCOP = GSHP( GSHPNum ).RatedCapHeat / GSHP( GSHPNum ).RatedPowerHeat;
			}

			if ( NumNums > 15 ) {
				if ( ! DataIPShortCuts::lNumericFieldBlanks( 16 ) ) {
					GSHP( GSHPNum ).sizFac = DataIPShortCuts::rNumericArgs( 16 );
				} else {
					GSHP( GSHPNum ).sizFac = 1.0;
				}
			} else {
				GSHP( GSHPNum ).sizFac = 1.0;
			}

			GSHP( GSHPNum ).SourceSideInletNodeNum = GetOnlySingleNode( DataIPShortCuts::cAlphaArgs( 2 ), ErrorsFound, HPEqFitHeatingUC, DataIPShortCuts::cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

			GSHP( GSHPNum ).SourceSideOutletNodeNum = GetOnlySingleNode( DataIPShortCuts::cAlphaArgs( 3 ), ErrorsFound, HPEqFitHeatingUC, DataIPShortCuts::cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			GSHP( GSHPNum ).LoadSideInletNodeNum = GetOnlySingleNode( DataIPShortCuts::cAlphaArgs( 4 ), ErrorsFound, HPEqFitHeatingUC, DataIPShortCuts::cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 2, ObjectIsNotParent );

			GSHP( GSHPNum ).LoadSideOutletNodeNum = GetOnlySingleNode( DataIPShortCuts::cAlphaArgs( 5 ), ErrorsFound, HPEqFitHeatingUC, DataIPShortCuts::cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 2, ObjectIsNotParent );

			if ( NumAlphas > 5 && ! DataIPShortCuts::lAlphaFieldBlanks( 6 ) ) {
				GSHP( GSHPNum ).companionName = DataIPShortCuts::cAlphaArgs( 6 );
			}

			// Test node sets
			TestCompSet( HPEqFitHeatingUC, DataIPShortCuts::cAlphaArgs( 1 ), DataIPShortCuts::cAlphaArgs( 2 ), DataIPShortCuts::cAlphaArgs( 3 ), "Condenser Water Nodes" );
			TestCompSet( HPEqFitHeatingUC, DataIPShortCuts::cAlphaArgs( 1 ), DataIPShortCuts::cAlphaArgs( 4 ), DataIPShortCuts::cAlphaArgs( 5 ), "Hot Water Nodes" );

			// CurrentModuleObject='HeatPump:WatertoWater:EquationFit:Heating'
			SetupOutputVariable( "Water to Water Heat Pump Electric Energy", OutputProcessor::Unit::J, GSHPReport( GSHPNum ).Energy, "System", "Sum", GSHP( GSHPNum ).Name, _, "Electricity", "Heating", _, "Plant" );
			SetupOutputVariable( "Water to Water Heat Pump Load Side Heat Transfer Energy", OutputProcessor::Unit::J, GSHPReport( GSHPNum ).QLoadEnergy, "System", "Sum", GSHP( GSHPNum ).Name );
			SetupOutputVariable( "Water to Water Heat Pump Source Side Heat Transfer Energy", OutputProcessor::Unit::J, GSHPReport( GSHPNum ).QSourceEnergy, "System", "Sum", GSHP( GSHPNum ).Name );
		}


		//now process companion coils, if any
		for ( GSHPNum = 1; GSHPNum <= NumGSHPs; ++GSHPNum ) {
			if ( ! GSHP( GSHPNum ).companionName.empty() ) {
				GSHP( GSHPNum ).companionIndex = UtilityRoutines::FindItemInList( GSHP( GSHPNum ).companionName, GSHP );
				if ( GSHP( GSHPNum ).companionIndex == 0 ) {
					ShowSevereError( "GetEquationFitWaterToWater Input: did not find companion heat pump named '" + GSHP( GSHPNum ).companionName + "' in heat pump called " + GSHP( GSHPNum ).Name );
					ErrorsFound =  true;
				} else {
					GSHP( GSHPNum ).companionIdentified = true;
				}
			}
		}

		if ( ErrorsFound ) {
			ShowFatalError( "Errors found in processing input for Water to Water Heat Pumps" );
		}

		for ( GSHPNum = 1; GSHPNum <= NumGSHPs; ++GSHPNum ) {
			//setup output variables
			SetupOutputVariable( "Water to Water Heat Pump Electric Power", OutputProcessor::Unit::W, GSHPReport( GSHPNum ).Power, "System", "Average", GSHP( GSHPNum ).Name );
			SetupOutputVariable( "Water to Water Heat Pump Load Side Heat Transfer Rate", OutputProcessor::Unit::W, GSHPReport( GSHPNum ).QLoad, "System", "Average", GSHP( GSHPNum ).Name );
			SetupOutputVariable( "Water to Water Heat Pump Source Side Heat Transfer Rate", OutputProcessor::Unit::W, GSHPReport( GSHPNum ).QSource, "System", "Average", GSHP( GSHPNum ).Name );
			SetupOutputVariable( "Water to Water Heat Pump Load Side Outlet Temperature", OutputProcessor::Unit::C, GSHPReport( GSHPNum ).LoadSideOutletTemp, "System", "Average", GSHP( GSHPNum ).Name );
			SetupOutputVariable( "Water to Water Heat Pump Load Side Inlet Temperature", OutputProcessor::Unit::C, GSHPReport( GSHPNum ).LoadSideInletTemp, "System", "Average", GSHP( GSHPNum ).Name );
			SetupOutputVariable( "Water to Water Heat Pump Source Side Outlet Temperature", OutputProcessor::Unit::C, GSHPReport( GSHPNum ).SourceSideOutletTemp, "System", "Average", GSHP( GSHPNum ).Name );
			SetupOutputVariable( "Water to Water Heat Pump Source Side Inlet Temperature", OutputProcessor::Unit::C, GSHPReport( GSHPNum ).SourceSideInletTemp, "System", "Average", GSHP( GSHPNum ).Name );
			SetupOutputVariable( "Water to Water Heat Pump Load Side Mass Flow Rate", OutputProcessor::Unit::kg_s, GSHPReport( GSHPNum ).LoadSideMassFlowRate, "System", "Average", GSHP( GSHPNum ).Name );
			SetupOutputVariable( "Water to Water Heat Pump Source Side Mass Flow Rate", OutputProcessor::Unit::kg_s, GSHPReport( GSHPNum ).SourceSideMassFlowRate, "System", "Average", GSHP( GSHPNum ).Name );

			//scan for loop connection data
			errFlag = false;
			ScanPlantLoopsForObject( GSHP( GSHPNum ).Name, GSHP( GSHPNum ).WWHPPlantTypeOfNum, GSHP( GSHPNum ).SourceLoopNum, GSHP( GSHPNum ).SourceLoopSideNum, GSHP( GSHPNum ).SourceBranchNum, GSHP( GSHPNum ).SourceCompNum, _, _, _, GSHP( GSHPNum ).SourceSideInletNodeNum, _, errFlag );
			ScanPlantLoopsForObject( GSHP( GSHPNum ).Name, GSHP( GSHPNum ).WWHPPlantTypeOfNum, GSHP( GSHPNum ).LoadLoopNum, GSHP( GSHPNum ).LoadLoopSideNum, GSHP( GSHPNum ).LoadBranchNum, GSHP( GSHPNum ).LoadCompNum, _, _, _, GSHP( GSHPNum ).LoadSideInletNodeNum, _, errFlag );

			if ( ! errFlag ) {
				PlantUtilities::InterConnectTwoPlantLoopSides( GSHP( GSHPNum ).LoadLoopNum,  GSHP( GSHPNum ).LoadLoopSideNum,  GSHP( GSHPNum ).SourceLoopNum, GSHP( GSHPNum ).SourceLoopSideNum, GSHP( GSHPNum ).WWHPPlantTypeOfNum, true );
			}

			if ( errFlag ) {
				ShowFatalError( "GetWatertoWaterHPInput: Program terminated on scan for loop data" );
			}

		}

	}

	void
	InitWatertoWaterHP(
		int const GSHPTypeNum, // Type of GSHP
		std::string const & EP_UNUSED( GSHPName ), // User Specified Name of GSHP
		int const GSHPNum, // GSHP Number
		bool const EP_UNUSED( FirstHVACIteration ),
		Real64 const MyLoad // Demand Load
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Kenneth Tang
		//       DATE WRITTEN   March 2005
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for initializations of the Water-to-Water HP Simple

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger initializations.

		// REFERENCES:
		// (1) Tang,C.C.. 2005. Modeling Packaged Heat Pumps in a Quasi-Steady
		// State Energy Simulation Program. M.S. Thesis, Department of Mechanical and Aerospace Engineering,
		// Oklahoma State University. (downloadable from http://www.hvac.okstate.edu/)
		// (2) Murugappan, Arun. 2002. Implementing Ground Source Heat Pump and Ground
		// Loop Heat Exchanger Models in the EnergyPlus Simulation Environment,
		// M.S. Thesis, Department of Mechanical and Aerospace Engineering,
		// Oklahoma State University. (downloadable from http://www.hvac.okstate.edu/)

		// Using/Aliasing
		using DataHVACGlobals::SysTimeElapsed;
		using DataPlant::TypeOf_HPWaterEFCooling;
		using DataPlant::TypeOf_HPWaterEFHeating;
		using DataPlant::PlantLoop;
		using FluidProperties::GetDensityGlycol;
		using PlantUtilities::InitComponentNodes;
		using PlantUtilities::SetComponentFlowRate;

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "InitGshp" );

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int LoadSideInletNode; // Load Side Inlet Node
		int LoadSideOutletNode; // Load Side Outlet Node
		int SourceSideInletNode; // Source Side Inlet Node
		int SourceSideOutletNode; // Source Side Outlet Node
		static Array1D_bool MyEnvrnFlag; // Flag required to keep track of initialization
		static bool OneTimeFlag( true ); // One Time Flag
		static Real64 CurrentSimTime( 0.0 ); // Current Simulation Time
		static Real64 PrevSimTime( 0.0 ); // Previous Simulation Time
		static Array1D_bool MyPlanScanFlag;

		int LoopNum;
		int LoopSideNum;
		Real64 rho; // local fluid density

		if ( InitWatertoWaterHPOneTimeFlag ) {
			MyPlanScanFlag.allocate( NumGSHPs );
			MyEnvrnFlag.allocate( NumGSHPs );
			InitWatertoWaterHPOneTimeFlag = false;
			MyEnvrnFlag = true;
			MyPlanScanFlag = true;
		}

		GSHP( GSHPNum ).MustRun = true; // Reset MustRun flag to TRUE
		LoadSideInletNode = GSHP( GSHPNum ).LoadSideInletNodeNum;
		LoadSideOutletNode = GSHP( GSHPNum ).LoadSideOutletNodeNum;
		SourceSideInletNode = GSHP( GSHPNum ).SourceSideInletNodeNum;
		SourceSideOutletNode = GSHP( GSHPNum ).SourceSideOutletNodeNum;

		if ( MyEnvrnFlag( GSHPNum ) && BeginEnvrnFlag ) {
			//Initialize all report variables to a known state at beginning of simulation

			GSHPReport( GSHPNum ).Power = 0.0;
			GSHPReport( GSHPNum ).Energy = 0.0;
			GSHPReport( GSHPNum ).QLoad = 0.0;
			GSHPReport( GSHPNum ).QLoadEnergy = 0.0;
			GSHPReport( GSHPNum ).QSource = 0.0;
			GSHPReport( GSHPNum ).QSourceEnergy = 0.0;
			GSHPReport( GSHPNum ).LoadSideMassFlowRate = 0.0;
			GSHPReport( GSHPNum ).LoadSideInletTemp = 0.0;
			GSHPReport( GSHPNum ).LoadSideOutletTemp = 0.0;
			GSHPReport( GSHPNum ).SourceSideMassFlowRate = 0.0;
			GSHPReport( GSHPNum ).SourceSideInletTemp = 0.0;
			GSHPReport( GSHPNum ).SourceSideOutletTemp = 0.0;
			GSHP( GSHPNum ).IsOn = false;
			GSHP( GSHPNum ).MustRun = true;

			if ( GSHP( GSHPNum ).WWHPPlantTypeOfNum == TypeOf_HPWaterEFHeating ) {
				rho = GetDensityGlycol( PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidName, DataGlobals::CWInitConvTemp, PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidIndex, RoutineName );
				GSHP( GSHPNum ).LoadSideDesignMassFlow = GSHP( GSHPNum ).RatedLoadVolFlowHeat * rho;
				rho = GetDensityGlycol( PlantLoop( GSHP( GSHPNum ).SourceLoopNum ).FluidName, DataGlobals::CWInitConvTemp, PlantLoop( GSHP( GSHPNum ).SourceLoopNum ).FluidIndex, RoutineName );
				GSHP( GSHPNum ).SourceSideDesignMassFlow = GSHP( GSHPNum ).RatedSourceVolFlowHeat * rho;
			} else if ( GSHP( GSHPNum ).WWHPPlantTypeOfNum == TypeOf_HPWaterEFCooling ) {
				rho = GetDensityGlycol( PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidName, DataGlobals::CWInitConvTemp, PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidIndex, RoutineName );
				GSHP( GSHPNum ).LoadSideDesignMassFlow = GSHP( GSHPNum ).RatedLoadVolFlowCool * rho;
				rho = GetDensityGlycol( PlantLoop( GSHP( GSHPNum ).SourceLoopNum ).FluidName, DataGlobals::CWInitConvTemp, PlantLoop( GSHP( GSHPNum ).SourceLoopNum ).FluidIndex, RoutineName );
				GSHP( GSHPNum ).SourceSideDesignMassFlow = GSHP( GSHPNum ).RatedSourceVolFlowCool * rho;
			}

			InitComponentNodes( 0.0, GSHP( GSHPNum ).LoadSideDesignMassFlow, GSHP( GSHPNum ).LoadSideInletNodeNum, GSHP( GSHPNum ).LoadSideOutletNodeNum, GSHP( GSHPNum ).LoadLoopNum, GSHP( GSHPNum ).LoadLoopSideNum, GSHP( GSHPNum ).LoadBranchNum, GSHP( GSHPNum ).LoadCompNum );

			InitComponentNodes( 0.0, GSHP( GSHPNum ).SourceSideDesignMassFlow, GSHP( GSHPNum ).SourceSideInletNodeNum, GSHP( GSHPNum ).SourceSideOutletNodeNum, GSHP( GSHPNum ).SourceLoopNum, GSHP( GSHPNum ).SourceLoopSideNum, GSHP( GSHPNum ).SourceBranchNum, GSHP( GSHPNum ).SourceCompNum );

			if ( Node( GSHP( GSHPNum ).SourceSideOutletNodeNum ).TempSetPoint == SensedNodeFlagValue ) Node( GSHP( GSHPNum ).SourceSideOutletNodeNum ).TempSetPoint = 0.0;
			Node( GSHP( GSHPNum ).SourceSideInletNodeNum ).Temp = Node( GSHP( GSHPNum ).SourceSideOutletNodeNum ).TempSetPoint + 30;

			MyEnvrnFlag( GSHPNum ) = false;
		}
		// Reset the environment flag
		if ( ! BeginEnvrnFlag ) MyEnvrnFlag( GSHPNum ) = true;

		if ( PrevSimTime != CurrentSimTime ) {
			PrevSimTime = CurrentSimTime;
		}

		// Calculate the simulation time
		CurrentSimTime = ( DayOfSim - 1 ) * 24 + ( HourOfDay - 1 ) + ( TimeStep - 1 ) * TimeStepZone + SysTimeElapsed;

		// Initialize event time array when the environment simulation begins
		if ( CurrentSimTime == 0.0 && OneTimeFlag ) {
			OneTimeFlag = false;
		}

		LoopNum = GSHP( GSHPNum ).LoadLoopNum;
		LoopSideNum = GSHP( GSHPNum ).LoadLoopSideNum;

		if ( CurrentSimTime > 0.0 ) OneTimeFlag = true;

		if ( MyLoad > 0.0 && GSHPTypeNum == TypeOf_HPWaterEFHeating ) {
			GSHP( GSHPNum ).MustRun = true;
			GSHP( GSHPNum ).IsOn = true;
		} else if ( MyLoad < 0.0 && GSHPTypeNum == TypeOf_HPWaterEFCooling ) {
			GSHP( GSHPNum ).MustRun = true;
			GSHP( GSHPNum ).IsOn = true;
		} else {
			GSHP( GSHPNum ).MustRun = false;
			GSHP( GSHPNum ).IsOn = false;
		}

		//*******Set flow based on "flowlock" and "run" flags**********
		// Set flows if the heat pump is not running
		if ( ! GSHP( GSHPNum ).MustRun ) {
			GSHPReport( GSHPNum ).LoadSideMassFlowRate = 0.0;
			GSHPReport( GSHPNum ).SourceSideMassFlowRate = 0.0;

			SetComponentFlowRate( GSHPReport( GSHPNum ).LoadSideMassFlowRate, GSHP( GSHPNum ).LoadSideInletNodeNum, GSHP( GSHPNum ).LoadSideOutletNodeNum, GSHP( GSHPNum ).LoadLoopNum, GSHP( GSHPNum ).LoadLoopSideNum, GSHP( GSHPNum ).LoadBranchNum, GSHP( GSHPNum ).LoadCompNum );
			SetComponentFlowRate( GSHPReport( GSHPNum ).SourceSideMassFlowRate, GSHP( GSHPNum ).SourceSideInletNodeNum, GSHP( GSHPNum ).SourceSideOutletNodeNum, GSHP( GSHPNum ).SourceLoopNum, GSHP( GSHPNum ).SourceLoopSideNum, GSHP( GSHPNum ).SourceBranchNum, GSHP( GSHPNum ).SourceCompNum );
			PlantUtilities::PullCompInterconnectTrigger( GSHP( GSHPNum ).LoadLoopNum, GSHP( GSHPNum ).LoadLoopSideNum, GSHP( GSHPNum ).LoadBranchNum, GSHP( GSHPNum ).LoadCompNum , GSHP( GSHPNum ).CondMassFlowIndex, GSHP( GSHPNum ).SourceLoopNum, GSHP( GSHPNum ).LoadLoopSideNum, DataPlant::CriteriaType_MassFlowRate, GSHPReport( GSHPNum ).SourceSideMassFlowRate );
			// Set flows if the heat pump is running
		} else { // the heat pump must run

			GSHPReport( GSHPNum ).LoadSideMassFlowRate = GSHP( GSHPNum ).LoadSideDesignMassFlow;
			GSHPReport( GSHPNum ).SourceSideMassFlowRate = GSHP( GSHPNum ).SourceSideDesignMassFlow;
			// now check against and request in plant
			SetComponentFlowRate( GSHPReport( GSHPNum ).LoadSideMassFlowRate, GSHP( GSHPNum ).LoadSideInletNodeNum, GSHP( GSHPNum ).LoadSideOutletNodeNum, GSHP( GSHPNum ).LoadLoopNum, GSHP( GSHPNum ).LoadLoopSideNum, GSHP( GSHPNum ).LoadBranchNum, GSHP( GSHPNum ).LoadCompNum );
			SetComponentFlowRate( GSHPReport( GSHPNum ).SourceSideMassFlowRate, GSHP( GSHPNum ).SourceSideInletNodeNum, GSHP( GSHPNum ).SourceSideOutletNodeNum, GSHP( GSHPNum ).SourceLoopNum, GSHP( GSHPNum ).SourceLoopSideNum, GSHP( GSHPNum ).SourceBranchNum, GSHP( GSHPNum ).SourceCompNum );
			//if there's no flowin one, turn the entire "heat pump off"
			if ( GSHPReport( GSHPNum ).LoadSideMassFlowRate <= 0.0 || GSHPReport( GSHPNum ).SourceSideMassFlowRate <= 0.0 ) {

				GSHPReport( GSHPNum ).LoadSideMassFlowRate = 0.0;
				GSHPReport( GSHPNum ).SourceSideMassFlowRate = 0.0;
				GSHP( GSHPNum ).MustRun = false;

				SetComponentFlowRate( GSHPReport( GSHPNum ).LoadSideMassFlowRate, GSHP( GSHPNum ).LoadSideInletNodeNum, GSHP( GSHPNum ).LoadSideOutletNodeNum, GSHP( GSHPNum ).LoadLoopNum, GSHP( GSHPNum ).LoadLoopSideNum, GSHP( GSHPNum ).LoadBranchNum, GSHP( GSHPNum ).LoadCompNum );
				SetComponentFlowRate( GSHPReport( GSHPNum ).SourceSideMassFlowRate, GSHP( GSHPNum ).SourceSideInletNodeNum, GSHP( GSHPNum ).SourceSideOutletNodeNum, GSHP( GSHPNum ).SourceLoopNum, GSHP( GSHPNum ).SourceLoopSideNum, GSHP( GSHPNum ).SourceBranchNum, GSHP( GSHPNum ).SourceCompNum );
				PlantUtilities::PullCompInterconnectTrigger( GSHP( GSHPNum ).LoadLoopNum, GSHP( GSHPNum ).LoadLoopSideNum, GSHP( GSHPNum ).LoadBranchNum, GSHP( GSHPNum ).LoadCompNum , GSHP( GSHPNum ).CondMassFlowIndex, GSHP( GSHPNum ).SourceLoopNum, GSHP( GSHPNum ).LoadLoopSideNum, DataPlant::CriteriaType_MassFlowRate, GSHPReport( GSHPNum ).SourceSideMassFlowRate );
				return;
			}
			PlantUtilities::PullCompInterconnectTrigger( GSHP( GSHPNum ).LoadLoopNum, GSHP( GSHPNum ).LoadLoopSideNum, GSHP( GSHPNum ).LoadBranchNum, GSHP( GSHPNum ).LoadCompNum , GSHP( GSHPNum ).CondMassFlowIndex, GSHP( GSHPNum ).SourceLoopNum, GSHP( GSHPNum ).LoadLoopSideNum, DataPlant::CriteriaType_MassFlowRate, GSHPReport( GSHPNum ).SourceSideMassFlowRate );
		}

		// Get inlet temps
		GSHPReport( GSHPNum ).LoadSideInletTemp = Node( LoadSideInletNode ).Temp;
		GSHPReport( GSHPNum ).SourceSideInletTemp = Node( SourceSideInletNode ).Temp;

		// Outlet variables
		GSHPReport( GSHPNum ).Power = 0.0;
		GSHPReport( GSHPNum ).Energy = 0.0;
		GSHPReport( GSHPNum ).QLoad = 0.0;
		GSHPReport( GSHPNum ).QLoadEnergy = 0.0;
		GSHPReport( GSHPNum ).QSource = 0.0;
		GSHPReport( GSHPNum ).QSourceEnergy = 0.0;
		GSHPReport( GSHPNum ).LoadSideOutletTemp = 0.0;
		GSHPReport( GSHPNum ).SourceSideOutletTemp = 0.0;

	}

	void
	sizeCoolingWaterToWaterHP( int const GSHPNum ) // GSHP Number
	{
		//do sizing related calculations and reporting for cooling heat pumps
		bool errorsFound( false );
		static std::string const RoutineName( "sizeCoolingWaterToWaterHP" );
		Real64 tmpLoadSideVolFlowRate = GSHP( GSHPNum ).RatedLoadVolFlowCool;
		Real64 tmpSourceSideVolFlowRate = GSHP( GSHPNum ).RatedSourceVolFlowCool;
		Real64 tmpCoolingCap = GSHP( GSHPNum ).RatedCapCool;
		Real64 tmpPowerDraw= GSHP( GSHPNum ).RatedPowerCool;

		//if companion heating coil known, update info from that
		if ( GSHP( GSHPNum ).companionIdentified ) {
			GSHP( GSHPNum ).RatedLoadVolFlowHeat = GSHP( GSHP( GSHPNum ).companionIndex ).RatedLoadVolFlowHeat;
			GSHP( GSHPNum ).ratedLoadVolFlowHeatWasAutoSized = GSHP( GSHP( GSHPNum ).companionIndex ).ratedLoadVolFlowHeatWasAutoSized;
			GSHP( GSHPNum ).RatedSourceVolFlowHeat = GSHP( GSHP( GSHPNum ).companionIndex ).RatedSourceVolFlowHeat;
			GSHP( GSHPNum ).ratedSourceVolFlowHeatWasAutoSized = GSHP( GSHP( GSHPNum ).companionIndex ).ratedSourceVolFlowHeatWasAutoSized;
			GSHP( GSHPNum ).RatedCapHeat = GSHP( GSHP( GSHPNum ).companionIndex ).RatedCapHeat;
			GSHP( GSHPNum ).ratedCapHeatWasAutoSized = GSHP( GSHP( GSHPNum ).companionIndex ).ratedCapHeatWasAutoSized;
			GSHP( GSHPNum ).RatedPowerHeat = GSHP( GSHP( GSHPNum ).companionIndex ).RatedPowerHeat;
			GSHP( GSHPNum ).ratedPowerHeatWasAutoSized = GSHP( GSHP( GSHPNum ).companionIndex ).ratedPowerHeatWasAutoSized;
		}

		int pltLoadSizNum = DataPlant::PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).PlantSizNum;
		if ( pltLoadSizNum > 0 ) {
			if ( DataSizing::PlantSizData( pltLoadSizNum ).DesVolFlowRate > DataHVACGlobals::SmallWaterVolFlow ) {
				tmpLoadSideVolFlowRate = DataSizing::PlantSizData( pltLoadSizNum ).DesVolFlowRate * GSHP( GSHPNum ).sizFac;
				//now compare to companion coil and take higher
				if ( GSHP( GSHPNum ).companionIdentified ) {
					tmpLoadSideVolFlowRate = max ( tmpLoadSideVolFlowRate,GSHP( GSHPNum ).RatedLoadVolFlowHeat );
					//store flow rate right away regardless of PlantFirstSizesOkayToFinalize so that data are available
					GSHP( GSHPNum ).RatedLoadVolFlowCool = tmpLoadSideVolFlowRate;
				}
				Real64 rho = FluidProperties::GetDensityGlycol( DataPlant::PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidName, DataGlobals::CWInitConvTemp, DataPlant::PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidIndex, RoutineName );
				Real64 Cp = FluidProperties::GetSpecificHeatGlycol( DataPlant::PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidName, DataGlobals::CWInitConvTemp, DataPlant::PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidIndex, RoutineName );
				tmpCoolingCap = Cp * rho * DataSizing::PlantSizData( pltLoadSizNum ).DeltaT * tmpLoadSideVolFlowRate;
			} else if ( GSHP( GSHPNum ).companionIdentified && GSHP( GSHPNum ).RatedLoadVolFlowHeat > 0.0 ) {
				tmpLoadSideVolFlowRate = GSHP( GSHPNum ).RatedLoadVolFlowHeat;
				Real64 rho = FluidProperties::GetDensityGlycol( DataPlant::PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidName, DataGlobals::CWInitConvTemp, DataPlant::PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidIndex, RoutineName );
				Real64 Cp = FluidProperties::GetSpecificHeatGlycol( DataPlant::PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidName, DataGlobals::CWInitConvTemp, DataPlant::PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidIndex, RoutineName );
				tmpCoolingCap = Cp * rho * DataSizing::PlantSizData( pltLoadSizNum ).DeltaT * tmpLoadSideVolFlowRate;
			} else {
				if ( GSHP( GSHPNum ).ratedCapCoolWasAutoSized ) tmpCoolingCap = 0.0;
				if ( GSHP( GSHPNum ).ratedLoadVolFlowCoolWasAutoSized ) tmpLoadSideVolFlowRate = 0.0;
			}
			if ( DataPlant::PlantFirstSizesOkayToFinalize ) {
				if ( GSHP( GSHPNum ).ratedCapCoolWasAutoSized ) {
					GSHP( GSHPNum ).RatedCapCool = tmpCoolingCap;
					if ( DataPlant::PlantFinalSizesOkayToReport ) {
						ReportSizingManager::ReportSizingOutput( "HeatPump:WaterToWater:EquationFit:Cooling",GSHP( GSHPNum ).Name, "Design Size Nominal Capacity [W]", tmpCoolingCap );
					}
					if ( DataPlant::PlantFirstSizesOkayToReport ) {
						ReportSizingManager::ReportSizingOutput( "HeatPump:WaterToWater:EquationFit:Cooling",GSHP( GSHPNum ).Name, "Initial Design Size Nominal Capacity [W]", tmpCoolingCap );
					}
				} else {
					if ( GSHP( GSHPNum ).RatedCapCool > 0.0 && tmpCoolingCap > 0.0 ) {
						Real64 nomCoolingCapUser = GSHP( GSHPNum ).RatedCapCool;
						if ( DataPlant::PlantFinalSizesOkayToReport ) {
							if ( DataGlobals::DoPlantSizing ) {
								ReportSizingManager::ReportSizingOutput( "HeatPump:WaterToWater:EquationFit:Cooling",GSHP( GSHPNum ).Name,"Design Size Nominal Capacity [W]", tmpCoolingCap, "User-Specified Nominal Capacity [W]", nomCoolingCapUser );
							} else {
								ReportSizingManager::ReportSizingOutput( "HeatPump:WaterToWater:EquationFit:Cooling",GSHP( GSHPNum ).Name,"User-Specified Nominal Capacity [W]", nomCoolingCapUser );
							}

							if ( DataGlobals::DisplayExtraWarnings ) {
								if ( ( std::abs( tmpCoolingCap - nomCoolingCapUser ) / nomCoolingCapUser ) > DataSizing::AutoVsHardSizingThreshold ) {
									ShowMessage( "sizeCoolingWaterToWaterHP: Potential issue with equipment sizing for " + GSHP( GSHPNum ).Name );
									ShowContinueError( "User-Specified Nominal Capacity of " + General::RoundSigDigits( nomCoolingCapUser, 2 ) + " [W]" );
									ShowContinueError( "differs from Design Size Nominal Capacity of " + General::RoundSigDigits( tmpCoolingCap, 2 ) + " [W]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
						tmpCoolingCap = nomCoolingCapUser;
					}
				}
				if ( GSHP( GSHPNum ).ratedLoadVolFlowCoolWasAutoSized ) {
					GSHP( GSHPNum ).RatedLoadVolFlowCool = tmpLoadSideVolFlowRate;
					if ( DataPlant::PlantFinalSizesOkayToReport ) {
						ReportSizingManager::ReportSizingOutput( "HeatPump:WaterToWater:EquationFit:Cooling",GSHP( GSHPNum ).Name, "Design Size Load Side Volume Flow Rate [m3/s]", tmpLoadSideVolFlowRate );
					}
					if ( DataPlant::PlantFirstSizesOkayToReport ) {
						ReportSizingManager::ReportSizingOutput( "HeatPump:WaterToWater:EquationFit:Cooling",GSHP( GSHPNum ).Name, "Initial Design Size Load Side Volume Flow Rate [m3/s]", tmpLoadSideVolFlowRate );
					}
				} else {
					if ( GSHP( GSHPNum ).RatedLoadVolFlowCool > 0.0 && tmpLoadSideVolFlowRate > 0.0 ) {
						Real64 nomLoadSideVolFlowUser = GSHP( GSHPNum ).RatedLoadVolFlowCool;
						if ( DataPlant::PlantFinalSizesOkayToReport ) {
							if ( DataGlobals::DoPlantSizing ) {
								ReportSizingManager::ReportSizingOutput( "HeatPump:WaterToWater:EquationFit:Cooling",GSHP( GSHPNum ).Name, "Design Size Load Side Volume Flow Rate [m3/s]", tmpLoadSideVolFlowRate, "User-Specified Load Side Volume Flow Rate [m3/s]", nomLoadSideVolFlowUser );
							} else {
								ReportSizingManager::ReportSizingOutput( "HeatPump:WaterToWater:EquationFit:Cooling",GSHP( GSHPNum ).Name,"User-Specified Load Side Volume Flow Rate [m3/s]", nomLoadSideVolFlowUser );
							}
							if ( DataGlobals::DisplayExtraWarnings ) {
								if ( ( std::abs( tmpLoadSideVolFlowRate - nomLoadSideVolFlowUser ) / nomLoadSideVolFlowUser ) > DataSizing::AutoVsHardSizingThreshold ) {
									ShowMessage( "sizeCoolingWaterToWaterHP: Potential issue with equipment sizing for " + GSHP( GSHPNum ).Name );
									ShowContinueError( "User-Specified Load Side Volume Flow Rate of " + General::RoundSigDigits( nomLoadSideVolFlowUser, 2 ) + " [m3/s]" );
									ShowContinueError( "differs from Design Size Load Side Volume Flow Rate of " + General::RoundSigDigits( tmpLoadSideVolFlowRate, 2 ) + " [m3/s]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
						tmpLoadSideVolFlowRate = nomLoadSideVolFlowUser;
					}
				}
			}

		} else { // did not find load side loop plant sizing to go with this.
			if ( GSHP( GSHPNum ).companionIdentified ) {
				if ( GSHP( GSHPNum ).ratedLoadVolFlowHeatWasAutoSized && GSHP( GSHPNum ).RatedLoadVolFlowHeat > 0.0 ) {
					//fill load side flow rate size from companion coil
					tmpLoadSideVolFlowRate = GSHP( GSHPNum ).RatedLoadVolFlowHeat;
					if ( DataPlant::PlantFirstSizesOkayToFinalize ) {
						GSHP( GSHPNum ).RatedLoadVolFlowCool = tmpLoadSideVolFlowRate;
						if ( DataPlant::PlantFinalSizesOkayToReport ) {
							ReportSizingManager::ReportSizingOutput( "HeatPump:WaterToWater:EquationFit:Cooling",GSHP( GSHPNum ).Name, "Design Size Load Side Volume Flow Rate [m3/s]", tmpLoadSideVolFlowRate );
						}
						if ( DataPlant::PlantFirstSizesOkayToReport ) {
							ReportSizingManager::ReportSizingOutput( "HeatPump:WaterToWater:EquationFit:Cooling",GSHP( GSHPNum ).Name, "Initial Design Size Load Side Volume Flow Rate [m3/s]", tmpLoadSideVolFlowRate );
						}
					}
				}
				if ( GSHP( GSHPNum ).ratedCapHeatWasAutoSized && GSHP( GSHPNum ).RatedCapHeat > 0.0 ) {
					tmpCoolingCap = GSHP( GSHPNum ).RatedCapHeat;
					if ( DataPlant::PlantFirstSizesOkayToFinalize ) {
						GSHP( GSHPNum ).RatedCapCool = tmpCoolingCap;
						if ( DataPlant::PlantFinalSizesOkayToReport ) {
							ReportSizingManager::ReportSizingOutput( "HeatPump:WaterToWater:EquationFit:Cooling",GSHP( GSHPNum ).Name, "Design Size Nominal Capacity [W]", tmpCoolingCap );
						}
						if ( DataPlant::PlantFirstSizesOkayToReport ) {
							ReportSizingManager::ReportSizingOutput( "HeatPump:WaterToWater:EquationFit:Cooling",GSHP( GSHPNum ).Name, "Initial Design Size Nominal Capacity [W]", tmpCoolingCap );
						}
					}
				}
			} else { // no companion heatpump, no plant sizing object
				if ( ( GSHP( GSHPNum ).ratedLoadVolFlowCoolWasAutoSized || GSHP( GSHPNum ).ratedCapCoolWasAutoSized ) && DataPlant::PlantFirstSizesOkayToFinalize ) {
					ShowSevereError( "Autosizing of Water to Water Heat Pump requires a loop Sizing:Plant object.");
					ShowContinueError("Occurs in HeatPump:WaterToWater:EquationFit:Cooling object = " + GSHP( GSHPNum ).Name );
					errorsFound = true;
				}
			}

			if ( ! GSHP( GSHPNum ).ratedLoadVolFlowCoolWasAutoSized && DataPlant::PlantFinalSizesOkayToReport ) {
				ReportSizingManager::ReportSizingOutput( "HeatPump:WaterToWater:EquationFit:Cooling", GSHP( GSHPNum ).Name, "User-Specified Load Side Flow Rate [m3/s]", GSHP( GSHPNum ).RatedLoadVolFlowCool );
			}
			if ( ! GSHP( GSHPNum ).ratedCapCoolWasAutoSized && DataPlant::PlantFinalSizesOkayToReport ) {
				ReportSizingManager::ReportSizingOutput( "HeatPump:WaterToWater:EquationFit:Cooling", GSHP( GSHPNum ).Name, "User-Specified Nominal Capacity [W]", GSHP( GSHPNum ).RatedCapCool );
			}
		}
		if ( ! GSHP( GSHPNum ).ratedLoadVolFlowCoolWasAutoSized ) tmpLoadSideVolFlowRate = GSHP( GSHPNum ).RatedLoadVolFlowCool;
		int pltSourceSizNum = DataPlant::PlantLoop( GSHP( GSHPNum ).SourceLoopNum ).PlantSizNum;
		if ( pltSourceSizNum > 0 ) {
			Real64 rho = FluidProperties::GetDensityGlycol( DataPlant::PlantLoop( GSHP( GSHPNum ).SourceLoopNum ).FluidName, DataGlobals::CWInitConvTemp, DataPlant::PlantLoop( GSHP( GSHPNum ).SourceLoopNum ).FluidIndex, RoutineName );
			Real64 Cp = FluidProperties::GetSpecificHeatGlycol( DataPlant::PlantLoop( GSHP( GSHPNum ).SourceLoopNum ).FluidName, DataGlobals::CWInitConvTemp, DataPlant::PlantLoop( GSHP( GSHPNum ).SourceLoopNum ).FluidIndex, RoutineName );
			tmpSourceSideVolFlowRate = tmpCoolingCap * ( 1.0 + ( 1.0 / GSHP( GSHPNum ).refCOP ) ) / ( DataSizing::PlantSizData( pltSourceSizNum ).DeltaT * Cp * rho );
		} else {
			tmpSourceSideVolFlowRate = tmpLoadSideVolFlowRate; // set source side flow equal to load side flow, assumption
		}

		if ( GSHP( GSHPNum ).ratedSourceVolFlowCoolWasAutoSized ) {
			GSHP( GSHPNum ).RatedSourceVolFlowCool = tmpSourceSideVolFlowRate;
			if ( DataPlant::PlantFinalSizesOkayToReport ) {
				ReportSizingManager::ReportSizingOutput( "HeatPump:WaterToWater:EquationFit:Cooling",GSHP( GSHPNum ).Name, "Design Size Source Side Volume Flow Rate [m3/s]", tmpSourceSideVolFlowRate );
			}
			if ( DataPlant::PlantFirstSizesOkayToReport ) {
				ReportSizingManager::ReportSizingOutput( "HeatPump:WaterToWater:EquationFit:Cooling",GSHP( GSHPNum ).Name, "Initial Design Size Source Side Volume Flow Rate [m3/s]", tmpSourceSideVolFlowRate );
			}
		} else {
			if ( GSHP( GSHPNum ).RatedSourceVolFlowCool > 0.0 && tmpSourceSideVolFlowRate > 0.0 ) {
				Real64 nomSourceSideVolFlowUser = GSHP( GSHPNum ).RatedSourceVolFlowCool;
				if ( DataPlant::PlantFinalSizesOkayToReport ) {
					if ( DataGlobals::DoPlantSizing ) {
						ReportSizingManager::ReportSizingOutput( "HeatPump:WaterToWater:EquationFit:Cooling",GSHP( GSHPNum ).Name, "Design Size Source Side Volume Flow Rate [m3/s]", tmpSourceSideVolFlowRate, "User-Specified Source Side Volume Flow Rate [m3/s]", nomSourceSideVolFlowUser );
					} else {
						ReportSizingManager::ReportSizingOutput( "HeatPump:WaterToWater:EquationFit:Cooling",GSHP( GSHPNum ).Name,"User-Specified Source Side Volume Flow Rate [m3/s]", nomSourceSideVolFlowUser );
					}
					if ( DataGlobals::DisplayExtraWarnings ){
						if ( ( std::abs( tmpSourceSideVolFlowRate - nomSourceSideVolFlowUser ) / nomSourceSideVolFlowUser ) > DataSizing::AutoVsHardSizingThreshold ) {
							ShowMessage( "sizeCoolingWaterToWaterHP: Potential issue with equipment sizing for " + GSHP( GSHPNum ).Name );
							ShowContinueError( "User-Specified Source Side Volume Flow Rate of " + General::RoundSigDigits( nomSourceSideVolFlowUser, 2 ) + " [m3/s]" );
							ShowContinueError( "differs from Design Size Source Side Volume Flow Rate of " + General::RoundSigDigits( tmpSourceSideVolFlowRate, 2 ) + " [m3/s]" );
							ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
							ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
						}
					}
				}
				tmpSourceSideVolFlowRate = nomSourceSideVolFlowUser;
			}
		}
		if ( ! GSHP( GSHPNum ).ratedSourceVolFlowCoolWasAutoSized ) tmpSourceSideVolFlowRate = GSHP( GSHPNum ).RatedSourceVolFlowCool;
		if ( ! GSHP( GSHPNum ).ratedCapCoolWasAutoSized ) tmpCoolingCap = GSHP( GSHPNum ).RatedCapCool;
		if ( GSHP( GSHPNum ).ratedPowerCoolWasAutoSized ){
			tmpPowerDraw = tmpCoolingCap / GSHP( GSHPNum ).refCOP;
			GSHP( GSHPNum ).RatedPowerCool = tmpPowerDraw;
			if ( DataPlant::PlantFinalSizesOkayToReport ) {
				ReportSizingManager::ReportSizingOutput( "HeatPump:WaterToWater:EquationFit:Cooling",GSHP( GSHPNum ).Name, "Design Size Cooling Power Consumption [W]", tmpPowerDraw );
			}
			if ( DataPlant::PlantFirstSizesOkayToReport ) {
				ReportSizingManager::ReportSizingOutput( "HeatPump:WaterToWater:EquationFit:Cooling",GSHP( GSHPNum ).Name, "Initial Design Size Cooling Power Consumption [W]", tmpPowerDraw );
			}
		} else {
			if ( GSHP( GSHPNum ).RatedPowerCool > 0.0 && tmpPowerDraw > 0.0 ) {
				Real64 nomPowerDrawUser = GSHP( GSHPNum ).RatedPowerCool;
				if ( DataPlant::PlantFinalSizesOkayToReport ){
					if ( DataGlobals::DoPlantSizing ) {
						ReportSizingManager::ReportSizingOutput( "HeatPump:WaterToWater:EquationFit:Cooling",GSHP( GSHPNum ).Name, "Design Size Cooling Power Consumption [W]", tmpPowerDraw, "User-Specified Cooling Power Consumption [W]", nomPowerDrawUser );
					} else {
						ReportSizingManager::ReportSizingOutput( "HeatPump:WaterToWater:EquationFit:Cooling",GSHP( GSHPNum ).Name,"User-Specified Cooling Power Consumption [W]", nomPowerDrawUser );
					}
					if ( DataGlobals::DisplayExtraWarnings ){
						if ( ( std::abs( tmpPowerDraw - nomPowerDrawUser ) / nomPowerDrawUser ) > DataSizing::AutoVsHardSizingThreshold ) {
							ShowMessage( "sizeCoolingWaterToWaterHP: Potential issue with equipment sizing for " + GSHP( GSHPNum ).Name );
							ShowContinueError( "User-Specified Cooling Power Consumption of " + General::RoundSigDigits( nomPowerDrawUser, 2 ) + " [W]" );
							ShowContinueError( "differs from Design Size Cooling Power Consumption of " + General::RoundSigDigits( tmpPowerDraw, 2 ) + " [W]" );
							ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
							ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
						}
					}
				}
				tmpPowerDraw = nomPowerDrawUser;
				GSHP( GSHPNum ).refCOP = tmpCoolingCap / tmpPowerDraw;
			}
		}

		PlantUtilities::RegisterPlantCompDesignFlow( GSHP( GSHPNum ).LoadSideInletNodeNum, tmpLoadSideVolFlowRate );
		// only register half of the source side flow because we expect a companion heat pump to also register a flow and we don't want to double count
		PlantUtilities::RegisterPlantCompDesignFlow( GSHP( GSHPNum ).SourceSideInletNodeNum, tmpSourceSideVolFlowRate * 0.5 );

		if ( DataPlant::PlantFinalSizesOkayToReport ) {
			//create predefined report
			OutputReportPredefined::PreDefTableEntry( OutputReportPredefined::pdchMechType, GSHP( GSHPNum ).Name, "HeatPump:WaterToWater:EquationFit:Cooling" );
			OutputReportPredefined::PreDefTableEntry( OutputReportPredefined::pdchMechNomEff, GSHP( GSHPNum ).Name, GSHP( GSHPNum ).refCOP );
			OutputReportPredefined::PreDefTableEntry( OutputReportPredefined::pdchMechNomCap, GSHP( GSHPNum ).Name, GSHP( GSHPNum ).RatedPowerCool );
		}

		if ( errorsFound ) {
			ShowFatalError( "Preceding sizing errors cause program termination" );
		}
	}

	void
	sizeHeatingWaterToWaterHP( int const GSHPNum ) // GSHP Number
	{
		//do sizing related calculations and reporting for heating heat pumps
		bool errorsFound( false );
		static std::string const RoutineName( "sizeHeatingWaterToWaterHP" );
		Real64 tmpLoadSideVolFlowRate = GSHP( GSHPNum ).RatedLoadVolFlowHeat;
		Real64 tmpSourceSideVolFlowRate = GSHP( GSHPNum ).RatedSourceVolFlowHeat;
		Real64 tmpHeatingCap = GSHP( GSHPNum ).RatedCapHeat;
		Real64 tmpPowerDraw= GSHP( GSHPNum ).RatedPowerHeat;

		//if companion cooling coil known, update info from that
		if ( GSHP( GSHPNum ).companionIdentified ) {
			GSHP( GSHPNum ).RatedLoadVolFlowCool = GSHP( GSHP( GSHPNum ).companionIndex ).RatedLoadVolFlowCool;
			GSHP( GSHPNum ).ratedLoadVolFlowCoolWasAutoSized = GSHP( GSHP( GSHPNum ).companionIndex ).ratedLoadVolFlowCoolWasAutoSized;
			GSHP( GSHPNum ).RatedSourceVolFlowCool = GSHP( GSHP( GSHPNum ).companionIndex ).RatedSourceVolFlowCool;
			GSHP( GSHPNum ).ratedSourceVolFlowCoolWasAutoSized = GSHP( GSHP( GSHPNum ).companionIndex ).ratedSourceVolFlowCoolWasAutoSized;
			GSHP( GSHPNum ).RatedCapCool = GSHP( GSHP( GSHPNum ).companionIndex ).RatedCapCool;
			GSHP( GSHPNum ).ratedCapCoolWasAutoSized = GSHP( GSHP( GSHPNum ).companionIndex ).ratedCapCoolWasAutoSized;
			GSHP( GSHPNum ).RatedPowerCool = GSHP( GSHP( GSHPNum ).companionIndex ).RatedPowerCool;
			GSHP( GSHPNum ).ratedPowerCoolWasAutoSized = GSHP( GSHP( GSHPNum ).companionIndex ).ratedPowerCoolWasAutoSized;
		}

		int pltLoadSizNum = DataPlant::PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).PlantSizNum;
		if ( pltLoadSizNum > 0 ) {
			if ( DataSizing::PlantSizData( pltLoadSizNum ).DesVolFlowRate > DataHVACGlobals::SmallWaterVolFlow ) {
				tmpLoadSideVolFlowRate = DataSizing::PlantSizData( pltLoadSizNum ).DesVolFlowRate * GSHP( GSHPNum ).sizFac;
				//now compare to companion coil and take higher
				if ( GSHP( GSHPNum ).companionIdentified ) {
					tmpLoadSideVolFlowRate = max ( tmpLoadSideVolFlowRate,GSHP( GSHPNum ).RatedLoadVolFlowCool );
					//store flow rate right away regardless of PlantFirstSizesOkayToFinalize so that data are available for companion when PlantFirstSizesOkayToFinalize is true
					GSHP( GSHPNum ).RatedLoadVolFlowHeat = tmpLoadSideVolFlowRate;
				}
				Real64 rho = FluidProperties::GetDensityGlycol( DataPlant::PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidName, DataGlobals::HWInitConvTemp, DataPlant::PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidIndex, RoutineName );
				Real64 Cp = FluidProperties::GetSpecificHeatGlycol( DataPlant::PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidName, DataGlobals::HWInitConvTemp, DataPlant::PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidIndex, RoutineName );
				tmpHeatingCap = Cp * rho * DataSizing::PlantSizData( pltLoadSizNum ).DeltaT * tmpLoadSideVolFlowRate;
			} else if ( GSHP( GSHPNum ).companionIdentified && GSHP( GSHPNum ).RatedLoadVolFlowCool > 0.0 ) {
				tmpLoadSideVolFlowRate = GSHP( GSHPNum ).RatedLoadVolFlowCool;
				Real64 rho = FluidProperties::GetDensityGlycol( DataPlant::PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidName, DataGlobals::HWInitConvTemp, DataPlant::PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidIndex, RoutineName );
				Real64 Cp = FluidProperties::GetSpecificHeatGlycol( DataPlant::PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidName, DataGlobals::HWInitConvTemp, DataPlant::PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidIndex, RoutineName );
				tmpHeatingCap = Cp * rho * DataSizing::PlantSizData( pltLoadSizNum ).DeltaT * tmpLoadSideVolFlowRate;
			} else {
				if ( GSHP( GSHPNum ).ratedCapHeatWasAutoSized ) tmpHeatingCap = 0.0;
				if ( GSHP( GSHPNum ).ratedLoadVolFlowHeatWasAutoSized ) tmpLoadSideVolFlowRate = 0.0;
			}
			if ( DataPlant::PlantFirstSizesOkayToFinalize ) {
				if ( GSHP( GSHPNum ).ratedCapHeatWasAutoSized ) {
					GSHP( GSHPNum ).RatedCapHeat = tmpHeatingCap;
					if ( DataPlant::PlantFinalSizesOkayToReport ) {
						ReportSizingManager::ReportSizingOutput( "HeatPump:WaterToWater:EquationFit:Heating",GSHP( GSHPNum ).Name, "Design Size Nominal Capacity [W]", tmpHeatingCap );
					}
					if ( DataPlant::PlantFirstSizesOkayToReport ) {
						ReportSizingManager::ReportSizingOutput( "HeatPump:WaterToWater:EquationFit:Heating",GSHP( GSHPNum ).Name, "Initial Design Size Nominal Capacity [W]", tmpHeatingCap );
					}
				} else {
					if ( GSHP( GSHPNum ).RatedCapHeat > 0.0 && tmpHeatingCap > 0.0 ) {
						Real64 nomHeatingCapUser = GSHP( GSHPNum ).RatedCapHeat;
						if ( DataPlant::PlantFinalSizesOkayToReport ) {
							if ( DataGlobals::DoPlantSizing ) {
								ReportSizingManager::ReportSizingOutput( "HeatPump:WaterToWater:EquationFit:Heating",GSHP( GSHPNum ).Name, "Design Size Nominal Capacity [W]", tmpHeatingCap, "User-Specified Nominal Capacity [W]", nomHeatingCapUser );
							} else {
								ReportSizingManager::ReportSizingOutput( "HeatPump:WaterToWater:EquationFit:Heating",GSHP( GSHPNum ).Name,"User-Specified Nominal Capacity [W]", nomHeatingCapUser );
							}
							if ( DataGlobals::DisplayExtraWarnings ) {
								if ( ( std::abs( tmpHeatingCap - nomHeatingCapUser ) / nomHeatingCapUser ) > DataSizing::AutoVsHardSizingThreshold ) {
									ShowMessage( "sizeHeatingWaterToWaterHP: Potential issue with equipment sizing for " + GSHP( GSHPNum ).Name );
									ShowContinueError( "User-Specified Nominal Capacity of " + General::RoundSigDigits( nomHeatingCapUser, 2 ) + " [W]" );
									ShowContinueError( "differs from Design Size Nominal Capacity of " + General::RoundSigDigits( tmpHeatingCap, 2 ) + " [W]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
						tmpHeatingCap = nomHeatingCapUser;
					}
				}
				if ( GSHP( GSHPNum ).ratedLoadVolFlowHeatWasAutoSized ) {
					GSHP( GSHPNum ).RatedLoadVolFlowHeat = tmpLoadSideVolFlowRate;
					if ( DataPlant::PlantFinalSizesOkayToReport ) {
						ReportSizingManager::ReportSizingOutput( "HeatPump:WaterToWater:EquationFit:Heating",GSHP( GSHPNum ).Name, "Design Size Load Side Volume Flow Rate [m3/s]", tmpLoadSideVolFlowRate );
					}
					if ( DataPlant::PlantFirstSizesOkayToReport ) {
						ReportSizingManager::ReportSizingOutput( "HeatPump:WaterToWater:EquationFit:Heating",GSHP( GSHPNum ).Name, "Initial Design Size Load Side Volume Flow Rate [m3/s]", tmpLoadSideVolFlowRate );
					}
				} else {
					if ( GSHP( GSHPNum ).RatedLoadVolFlowHeat > 0.0 && tmpLoadSideVolFlowRate > 0.0 ) {
						Real64 nomLoadSideVolFlowUser = GSHP( GSHPNum ).RatedLoadVolFlowHeat;
						if ( DataPlant::PlantFinalSizesOkayToReport ) {
							if ( DataGlobals::DoPlantSizing ) {
								ReportSizingManager::ReportSizingOutput( "HeatPump:WaterToWater:EquationFit:Heating",GSHP( GSHPNum ).Name, "Design Size Load Side Volume Flow Rate [m3/s]", tmpLoadSideVolFlowRate, "User-Specified Load Side Volume Flow Rate [m3/s]", nomLoadSideVolFlowUser );
							} else {
								ReportSizingManager::ReportSizingOutput( "HeatPump:WaterToWater:EquationFit:Heating",GSHP( GSHPNum ).Name,"User-Specified Load Side Volume Flow Rate [m3/s]", nomLoadSideVolFlowUser );
							}
							if ( DataGlobals::DisplayExtraWarnings ) {
								if ( ( std::abs( tmpLoadSideVolFlowRate - nomLoadSideVolFlowUser ) / nomLoadSideVolFlowUser ) > DataSizing::AutoVsHardSizingThreshold ) {
									ShowMessage( "sizeHeatingWaterToWaterHP: Potential issue with equipment sizing for " + GSHP( GSHPNum ).Name );
									ShowContinueError( "User-Specified Load Side Volume Flow Rate of " + General::RoundSigDigits( nomLoadSideVolFlowUser, 2 ) + " [m3/s]" );
									ShowContinueError( "differs from Design Size Load Side Volume Flow Rate of " + General::RoundSigDigits( tmpLoadSideVolFlowRate, 2 ) + " [m3/s]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
						tmpLoadSideVolFlowRate = nomLoadSideVolFlowUser;
					}
				}
			}
		} else { // did not find plant sizing to go with this.
			if ( GSHP( GSHPNum ).companionIdentified ) {
				if ( GSHP( GSHPNum ).ratedLoadVolFlowHeatWasAutoSized && GSHP( GSHPNum ).RatedLoadVolFlowCool > 0.0 ) {
					//fill load side flow rate size from companion coil
					tmpLoadSideVolFlowRate = GSHP( GSHPNum ).RatedLoadVolFlowCool;
					if ( DataPlant::PlantFirstSizesOkayToFinalize ) {
						GSHP( GSHPNum ).RatedLoadVolFlowHeat = tmpLoadSideVolFlowRate;
						if ( DataPlant::PlantFinalSizesOkayToReport ) {
							ReportSizingManager::ReportSizingOutput( "HeatPump:WaterToWater:EquationFit:Heating",GSHP( GSHPNum ).Name, "Design Size Load Side Volume Flow Rate [m3/s]", tmpLoadSideVolFlowRate );
						}
						if ( DataPlant::PlantFirstSizesOkayToReport ) {
							ReportSizingManager::ReportSizingOutput( "HeatPump:WaterToWater:EquationFit:Heating",GSHP( GSHPNum ).Name, "Initial Design Size Load Side Volume Flow Rate [m3/s]", tmpLoadSideVolFlowRate );
						}
					}

				}
				if ( GSHP( GSHPNum ).ratedCapHeatWasAutoSized && GSHP( GSHPNum ).RatedCapCool > 0.0 ) {
					tmpHeatingCap = GSHP( GSHPNum ).RatedCapCool;
					if ( DataPlant::PlantFirstSizesOkayToFinalize ) {
						GSHP( GSHPNum ).RatedCapHeat = tmpHeatingCap;
						if ( DataPlant::PlantFinalSizesOkayToReport ) {
							ReportSizingManager::ReportSizingOutput( "HeatPump:WaterToWater:EquationFit:Heating",GSHP( GSHPNum ).Name, "Design Size Nominal Capacity [W]", tmpHeatingCap );
						}
						if ( DataPlant::PlantFirstSizesOkayToReport ) {
							ReportSizingManager::ReportSizingOutput( "HeatPump:WaterToWater:EquationFit:Heating",GSHP( GSHPNum ).Name, "Initial Design Size Nominal Capacity [W]", tmpHeatingCap );
						}
					}
				}

			} else { // no companion heatpump, no plant sizing object
				if ( ( GSHP( GSHPNum ).ratedLoadVolFlowHeatWasAutoSized || GSHP( GSHPNum ).ratedCapHeatWasAutoSized ) && DataPlant::PlantFirstSizesOkayToFinalize ) {
					ShowSevereError( "Autosizing of Water to Water Heat Pump requires a loop Sizing:Plant object." );
					ShowContinueError( "Occurs in HeatPump:WaterToWater:EquationFit:Heating object = " + GSHP( GSHPNum ).Name );
					errorsFound = true;
				}
			}

			if ( ! GSHP( GSHPNum ).ratedLoadVolFlowHeatWasAutoSized && DataPlant::PlantFinalSizesOkayToReport ) {
				ReportSizingManager::ReportSizingOutput( "HeatPump:WaterToWater:EquationFit:Heating", GSHP( GSHPNum ).Name, "User-Specified Load Side Flow Rate [m3/s]", GSHP( GSHPNum ).RatedLoadVolFlowHeat );
			}
			if ( ! GSHP( GSHPNum ).ratedCapHeatWasAutoSized && DataPlant::PlantFinalSizesOkayToReport ) {
				ReportSizingManager::ReportSizingOutput( "HeatPump:WaterToWater:EquationFit:Heating", GSHP( GSHPNum ).Name, "User-Specified Nominal Capacity [W]", GSHP( GSHPNum ).RatedCapHeat );
			}
		}
		if ( ! GSHP( GSHPNum ).ratedLoadVolFlowHeatWasAutoSized ) tmpLoadSideVolFlowRate = GSHP( GSHPNum ).RatedLoadVolFlowHeat;
		int pltSourceSizNum = DataPlant::PlantLoop( GSHP( GSHPNum ).SourceLoopNum ).PlantSizNum;
		if ( pltSourceSizNum > 0 ) {
			Real64 rho = FluidProperties::GetDensityGlycol( DataPlant::PlantLoop( GSHP( GSHPNum ).SourceLoopNum ).FluidName, DataGlobals::HWInitConvTemp, DataPlant::PlantLoop( GSHP( GSHPNum ).SourceLoopNum ).FluidIndex, RoutineName );
			Real64 Cp = FluidProperties::GetSpecificHeatGlycol( DataPlant::PlantLoop( GSHP( GSHPNum ).SourceLoopNum ).FluidName, DataGlobals::HWInitConvTemp, DataPlant::PlantLoop( GSHP( GSHPNum ).SourceLoopNum ).FluidIndex, RoutineName );
			tmpSourceSideVolFlowRate = tmpHeatingCap * ( 1.0 - ( 1.0 / GSHP( GSHPNum ).refCOP ) ) / ( DataSizing::PlantSizData( pltSourceSizNum ).DeltaT * Cp * rho );
		} else {
			tmpSourceSideVolFlowRate = tmpLoadSideVolFlowRate; // set source side flow equal to load side flow, assumption
		}
		if ( GSHP( GSHPNum ).ratedSourceVolFlowHeatWasAutoSized ) {
			GSHP( GSHPNum ).RatedSourceVolFlowHeat = tmpSourceSideVolFlowRate;
			if ( DataPlant::PlantFinalSizesOkayToReport ) {
				ReportSizingManager::ReportSizingOutput( "HeatPump:WaterToWater:EquationFit:Heating",GSHP( GSHPNum ).Name, "Design Size Source Side Volume Flow Rate [m3/s]", tmpSourceSideVolFlowRate );
			}
			if ( DataPlant::PlantFirstSizesOkayToReport ) {
				ReportSizingManager::ReportSizingOutput( "HeatPump:WaterToWater:EquationFit:Heating",GSHP( GSHPNum ).Name, "Initial Design Size Source Side Volume Flow Rate [m3/s]", tmpSourceSideVolFlowRate );
			}
		} else {
			if ( GSHP( GSHPNum ).RatedSourceVolFlowHeat > 0.0 && tmpSourceSideVolFlowRate > 0.0 ) {
				Real64 nomSourceSideVolFlowUser = GSHP( GSHPNum ).RatedSourceVolFlowHeat;
				if ( DataPlant::PlantFinalSizesOkayToReport ) {
					if ( DataGlobals::DoPlantSizing ) {
						ReportSizingManager::ReportSizingOutput( "HeatPump:WaterToWater:EquationFit:Heating",GSHP( GSHPNum ).Name, "Design Size Source Side Volume Flow Rate [m3/s]", tmpSourceSideVolFlowRate, "User-Specified Source Side Volume Flow Rate [m3/s]", nomSourceSideVolFlowUser );
					} else {
						ReportSizingManager::ReportSizingOutput( "HeatPump:WaterToWater:EquationFit:Heating",GSHP( GSHPNum ).Name,"User-Specified Source Side Volume Flow Rate [m3/s]", nomSourceSideVolFlowUser );
					}
					if ( DataGlobals::DisplayExtraWarnings ){
						if ( ( std::abs( tmpSourceSideVolFlowRate - nomSourceSideVolFlowUser ) / nomSourceSideVolFlowUser ) > DataSizing::AutoVsHardSizingThreshold ) {
							ShowMessage( "sizeHeatingWaterToWaterHP: Potential issue with equipment sizing for " + GSHP( GSHPNum ).Name );
							ShowContinueError( "User-Specified Source Side Volume Flow Rate of " + General::RoundSigDigits( nomSourceSideVolFlowUser, 2 ) + " [m3/s]" );
							ShowContinueError( "differs from Design Size Source Side Volume Flow Rate of " + General::RoundSigDigits( tmpSourceSideVolFlowRate, 2 ) + " [m3/s]" );
							ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
							ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
						}
					}
				}
				tmpSourceSideVolFlowRate = nomSourceSideVolFlowUser;
			}
		}
		if ( ! GSHP( GSHPNum ).ratedSourceVolFlowHeatWasAutoSized ) tmpSourceSideVolFlowRate = GSHP( GSHPNum ).RatedSourceVolFlowHeat;
		if ( ! GSHP( GSHPNum ).ratedCapHeatWasAutoSized ) tmpHeatingCap = GSHP( GSHPNum ).RatedCapHeat;
		if ( GSHP( GSHPNum ).ratedPowerHeatWasAutoSized ){
			tmpPowerDraw = tmpHeatingCap / GSHP( GSHPNum ).refCOP;
			GSHP( GSHPNum ).RatedPowerHeat = tmpPowerDraw;
			if ( DataPlant::PlantFinalSizesOkayToReport ) {
				ReportSizingManager::ReportSizingOutput( "HeatPump:WaterToWater:EquationFit:Heating",GSHP( GSHPNum ).Name, "Design Size Heating Power Consumption [W]", tmpPowerDraw );
			}
			if ( DataPlant::PlantFirstSizesOkayToReport ) {
				ReportSizingManager::ReportSizingOutput( "HeatPump:WaterToWater:EquationFit:Heating",GSHP( GSHPNum ).Name, "Initial Design Size Heating Power Consumption [W]", tmpPowerDraw );
			}
		} else {
			if ( GSHP( GSHPNum ).RatedPowerHeat > 0.0 && tmpPowerDraw > 0.0 ) {
				Real64 nomPowerDrawUser = GSHP( GSHPNum ).RatedPowerHeat;
				if ( DataPlant::PlantFinalSizesOkayToReport ){
					if ( DataGlobals::DoPlantSizing ) {
						ReportSizingManager::ReportSizingOutput( "HeatPump:WaterToWater:EquationFit:Heating",GSHP( GSHPNum ).Name, "Design Size Heating Power Consumption [W]", tmpPowerDraw, "User-Specified Heating Power Consumption [W]", nomPowerDrawUser );
					} else {
						ReportSizingManager::ReportSizingOutput( "HeatPump:WaterToWater:EquationFit:Heating",GSHP( GSHPNum ).Name,"User-Specified Heating Power Consumption [W]", nomPowerDrawUser );
					}
					if ( DataGlobals::DisplayExtraWarnings ){
						if ( ( std::abs( tmpPowerDraw - nomPowerDrawUser ) / nomPowerDrawUser ) > DataSizing::AutoVsHardSizingThreshold ) {
							ShowMessage( "sizeHeatingWaterToWaterHP: Potential issue with equipment sizing for " + GSHP( GSHPNum ).Name );
							ShowContinueError( "User-Specified Heating Power Consumption of " + General::RoundSigDigits( nomPowerDrawUser, 2 ) + " [W]" );
							ShowContinueError( "differs from Design Size Heating Power Consumption of " + General::RoundSigDigits( tmpPowerDraw, 2 ) + " [W]" );
							ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
							ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
						}
					}
				}
				tmpPowerDraw = nomPowerDrawUser;
				GSHP( GSHPNum ).refCOP = tmpHeatingCap / tmpPowerDraw;
			}
		}

		PlantUtilities::RegisterPlantCompDesignFlow( GSHP( GSHPNum ).LoadSideInletNodeNum, tmpLoadSideVolFlowRate );
		// register half of source side flow to avoid double counting
		PlantUtilities::RegisterPlantCompDesignFlow( GSHP( GSHPNum ).SourceSideInletNodeNum, tmpSourceSideVolFlowRate * 0.5 );

		if ( DataPlant::PlantFinalSizesOkayToReport ) {
			//create predefined report
			OutputReportPredefined::PreDefTableEntry( OutputReportPredefined::pdchMechType, GSHP( GSHPNum ).Name, "HeatPump:WaterToWater:EquationFit:Heating" );
			OutputReportPredefined::PreDefTableEntry( OutputReportPredefined::pdchMechNomEff, GSHP( GSHPNum ).Name, GSHP( GSHPNum ).refCOP );
			OutputReportPredefined::PreDefTableEntry( OutputReportPredefined::pdchMechNomCap, GSHP( GSHPNum ).Name, GSHP( GSHPNum ).RatedPowerHeat );
		}
		if ( errorsFound ) {
			ShowFatalError( "Preceding sizing errors cause program termination" );
		}
	}

	void
	CalcWatertoWaterHPCooling(
		int const GSHPNum, // GSHP Number
		Real64 const MyLoad // Operating Load
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Kenneth Tang
		//       DATE WRITTEN   March 2005
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS SUBROUTINE:
		// This routine simulate the heat pump peformance in cooling mode

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// (1) Tang,C.C.. 2005. Modeling Packaged Heat Pumps in a Quasi-Steady
		// State Energy Simulation Program. M.S. Thesis, Department of Mechanical and Aerospace Engineering,
		// Oklahoma State University. (downloadable from http://www.hvac.okstate.edu/)

		// Using/Aliasing
		using DataHVACGlobals::TimeStepSys;
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSpecificHeatGlycol;
		using DataPlant::PlantLoop;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		Real64 const CelsiustoKelvin( KelvinConv ); // Conversion from Celsius to Kelvin
		Real64 const Tref( 283.15 ); // Reference Temperature for performance curves,10C [K]
		static std::string const RoutineName( "CalcWatertoWaterHPCooling" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		Real64 CoolCapRated; // Rated Cooling Capacity [W]
		Real64 CoolPowerRated; // Rated Cooling Power Consumption[W]
		Real64 LoadSideVolFlowRateRated; // Rated Load Side Volumetric Flow Rate [m3/s]
		Real64 SourceSideVolFlowRateRated; // Rated Source Side Volumetric Flow Rate [m3/s]
		Real64 CoolCapCoeff1; // 1st coefficient of the cooling capacity performance curve
		Real64 CoolCapCoeff2; // 2nd coefficient of the cooling capacity performance curve
		Real64 CoolCapCoeff3; // 3rd coefficient of the cooling capacity performance curve
		Real64 CoolCapCoeff4; // 4th coefficient of the cooling capacity performance curve
		Real64 CoolCapCoeff5; // 5th coefficient of the cooling capacity performance curve
		Real64 CoolPowerCoeff1; // 1st coefficient of the cooling power consumption curve
		Real64 CoolPowerCoeff2; // 2nd coefficient of the cooling power consumption curve
		Real64 CoolPowerCoeff3; // 3rd coefficient of the cooling power consumption curve
		Real64 CoolPowerCoeff4; // 4th coefficient of the cooling power consumption curve
		Real64 CoolPowerCoeff5; // 5th coefficient of the cooling power consumption curve

		Real64 LoadSideMassFlowRate; // Load Side Mass Flow Rate [kg/s]
		Real64 LoadSideInletTemp; // Load Side Inlet Temperature [C]
		Real64 LoadSideOutletTemp; // Load side Outlet Temperature [C]
		Real64 SourceSideMassFlowRate; // Source Side Mass Flow Rate [kg/s]
		Real64 SourceSideInletTemp; // Source Side Inlet Temperature [C]
		Real64 SourceSideOutletTemp; // Source Side Outlet Temperature [C]

		Real64 func1; // Portion of the heat transfer and power equation
		Real64 func2; // Portion of the heat transfer and power equation
		Real64 func3; // Portion of the heat transfer and power equation
		Real64 func4; // Portion of the heat transfer and power equation
		Real64 Power; // Power Consumption [W]
		Real64 QLoad; // Cooling Capacity [W]
		Real64 QSource; // Source Side Heat Transfer Rate [W]
		Real64 PartLoadRatio; // Part-Load Ratio
		Real64 ReportingConstant;
		Real64 rhoLoadSide;
		Real64 rhoSourceSide;
		Real64 CpLoadSide;
		Real64 CpSourceSide;

		//  LOAD LOCAL VARIABLES FROM DATA STRUCTURE
		LoadSideVolFlowRateRated = GSHP( GSHPNum ).RatedLoadVolFlowCool;
		SourceSideVolFlowRateRated = GSHP( GSHPNum ).RatedSourceVolFlowCool;
		CoolCapRated = GSHP( GSHPNum ).RatedCapCool;
		CoolPowerRated = GSHP( GSHPNum ).RatedPowerCool;
		CoolCapCoeff1 = GSHP( GSHPNum ).CoolCap1;
		CoolCapCoeff2 = GSHP( GSHPNum ).CoolCap2;
		CoolCapCoeff3 = GSHP( GSHPNum ).CoolCap3;
		CoolCapCoeff4 = GSHP( GSHPNum ).CoolCap4;
		CoolCapCoeff5 = GSHP( GSHPNum ).CoolCap5;
		CoolPowerCoeff1 = GSHP( GSHPNum ).CoolPower1;
		CoolPowerCoeff2 = GSHP( GSHPNum ).CoolPower2;
		CoolPowerCoeff3 = GSHP( GSHPNum ).CoolPower3;
		CoolPowerCoeff4 = GSHP( GSHPNum ).CoolPower4;
		CoolPowerCoeff5 = GSHP( GSHPNum ).CoolPower5;

		LoadSideMassFlowRate = GSHPReport( GSHPNum ).LoadSideMassFlowRate;
		LoadSideInletTemp = GSHPReport( GSHPNum ).LoadSideInletTemp;
		SourceSideMassFlowRate = GSHPReport( GSHPNum ).SourceSideMassFlowRate;
		SourceSideInletTemp = GSHPReport( GSHPNum ).SourceSideInletTemp;

		// If heat pump is not operating, THEN return
		if ( ! GSHP( GSHPNum ).MustRun ) {
			return;
		}

		rhoLoadSide = GetDensityGlycol( PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidName, LoadSideInletTemp, PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidIndex, RoutineName );

		rhoSourceSide = GetDensityGlycol( PlantLoop( GSHP( GSHPNum ).SourceLoopNum ).FluidName, SourceSideInletTemp, PlantLoop( GSHP( GSHPNum ).SourceLoopNum ).FluidIndex, RoutineName );

		func1 = ( ( LoadSideInletTemp + CelsiustoKelvin ) / Tref );
		func2 = ( ( SourceSideInletTemp + CelsiustoKelvin ) / Tref );
		func3 = ( LoadSideMassFlowRate / ( LoadSideVolFlowRateRated * rhoLoadSide ) );
		func4 = ( SourceSideMassFlowRate / ( SourceSideVolFlowRateRated * rhoSourceSide ) );

		QLoad = CoolCapRated * ( CoolCapCoeff1 + ( func1 * CoolCapCoeff2 ) + ( func2 * CoolCapCoeff3 ) + ( func3 * CoolCapCoeff4 ) + ( func4 * CoolCapCoeff5 ) );

		Power = CoolPowerRated * ( CoolPowerCoeff1 + ( func1 * CoolPowerCoeff2 ) + ( func2 * CoolPowerCoeff3 ) + ( func3 * CoolPowerCoeff4 ) + ( func4 * CoolPowerCoeff5 ) );

		if ( ( QLoad <= 0.0 || Power <= 0.0 ) && ! WarmupFlag ) {
			if ( QLoad <= 0.0 ) {
				if ( GSHP( GSHPNum ).CoolCapNegativeCounter < 1 ) {
					++GSHP( GSHPNum ).CoolCapNegativeCounter;
					ShowWarningError( HPEqFitCooling + " \"" + GSHP( GSHPNum ).Name + "\":" );
					ShowContinueError( " Cooling capacity curve output is <= 0.0 (" + TrimSigDigits( QLoad, 4 ) + ")." );
					ShowContinueError( " Zero or negative value occurs with a load-side inlet temperature of " + TrimSigDigits( LoadSideInletTemp, 2 ) + " C," );
					ShowContinueError( " a source-side inlet temperature of " + TrimSigDigits( SourceSideInletTemp, 2 ) + " C," );
					ShowContinueError( " a load-side mass flow rate of " + TrimSigDigits( LoadSideMassFlowRate, 3 ) + " kg/s," );
					ShowContinueError( " and a source-side mass flow rate of " + TrimSigDigits( SourceSideMassFlowRate, 3 ) + " kg/s." );
					ShowContinueErrorTimeStamp( " The heat pump is turned off for this time step but simulation continues." );
				} else {
					ShowRecurringWarningErrorAtEnd( HPEqFitCooling + " \"" + GSHP( GSHPNum ).Name + "\": Cooling capacity curve output is <= 0.0 warning continues...", GSHP( GSHPNum ).CoolCapNegativeIndex, QLoad, QLoad );
				}
			}
			if ( Power <= 0.0 ) {
				if ( GSHP( GSHPNum ).CoolPowerNegativeCounter < 1 ) {
					++GSHP( GSHPNum ).CoolPowerNegativeCounter;
					ShowWarningError( HPEqFitCooling + " \"" + GSHP( GSHPNum ).Name + "\":" );
					ShowContinueError( " Cooling compressor power curve output is <= 0.0 (" + TrimSigDigits( Power, 4 ) + ")." );
					ShowContinueError( " Zero or negative value occurs with a load-side inlet temperature of " + TrimSigDigits( LoadSideInletTemp, 2 ) + " C," );
					ShowContinueError( " a source-side inlet temperature of " + TrimSigDigits( SourceSideInletTemp, 2 ) + " C," );
					ShowContinueError( " a load-side mass flow rate of " + TrimSigDigits( LoadSideMassFlowRate, 3 ) + " kg/s," );
					ShowContinueError( " and a source-side mass flow rate of " + TrimSigDigits( SourceSideMassFlowRate, 3 ) + " kg/s." );
					ShowContinueErrorTimeStamp( " The heat pump is turned off for this time step but simulation continues." );
				} else {
					ShowRecurringWarningErrorAtEnd( HPEqFitCooling + " \"" + GSHP( GSHPNum ).Name + "\": Cooling compressor power curve output is <= 0.0 warning continues...", GSHP( GSHPNum ).CoolPowerNegativeIndex, Power, Power );
				}
			}

			QLoad = 0.0;
			Power = 0.0;

		}

		QSource = QLoad + Power; //assume no losses

		//Control Strategy
		if ( std::abs( MyLoad ) < QLoad && QLoad != 0.0 ) {
			PartLoadRatio = std::abs( MyLoad ) / QLoad;
			QLoad = std::abs( MyLoad );
			Power *= PartLoadRatio;
			QSource *= PartLoadRatio;
		}

		CpLoadSide = GetSpecificHeatGlycol( PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidName, LoadSideInletTemp, PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidIndex, RoutineName );

		CpSourceSide = GetSpecificHeatGlycol( PlantLoop( GSHP( GSHPNum ).SourceLoopNum ).FluidName, SourceSideInletTemp, PlantLoop( GSHP( GSHPNum ).SourceLoopNum ).FluidIndex, RoutineName );

		LoadSideOutletTemp = LoadSideInletTemp - QLoad / ( LoadSideMassFlowRate * CpLoadSide );
		SourceSideOutletTemp = SourceSideInletTemp + QSource / ( SourceSideMassFlowRate * CpSourceSide );

		ReportingConstant = TimeStepSys * SecInHour;

		GSHPReport( GSHPNum ).Power = Power;
		GSHPReport( GSHPNum ).Energy = Power * ReportingConstant;
		GSHPReport( GSHPNum ).QSource = QSource;
		GSHPReport( GSHPNum ).QLoad = QLoad;
		GSHPReport( GSHPNum ).QSourceEnergy = QSource * ReportingConstant;
		GSHPReport( GSHPNum ).QLoadEnergy = QLoad * ReportingConstant;
		GSHPReport( GSHPNum ).LoadSideOutletTemp = LoadSideOutletTemp;
		GSHPReport( GSHPNum ).SourceSideOutletTemp = SourceSideOutletTemp;
	}

	void
	CalcWatertoWaterHPHeating(
		int const GSHPNum, // GSHP Number
		Real64 const MyLoad // Operating Load
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Kenneth Tang
		//       DATE WRITTEN   March 2005
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS SUBROUTINE:
		// This routine simulate the heat pump peformance in heating mode

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// (1) Tang,C.C.. 2005. Modeling Packaged Heat Pumps in a Quasi-Steady
		// State Energy Simulation Program. M.S. Thesis, Department of Mechanical and Aerospace Engineering,
		// Oklahoma State University. (downloadable from http://www.hvac.okstate.edu/)

		// Using/Aliasing
		using DataHVACGlobals::TimeStepSys;
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSpecificHeatGlycol;
		using DataPlant::PlantLoop;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		Real64 const CelsiustoKelvin( KelvinConv ); // Conversion from Celsius to Kelvin
		Real64 const Tref( 283.15 ); // Reference Temperature for performance curves,10C [K]
		static std::string const RoutineName( "CalcWatertoWaterHPHeating" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		Real64 HeatCapRated; // Rated Heating Capacity [W]
		Real64 HeatPowerRated; // Rated Heating Compressor Power[W]
		Real64 LoadSideVolFlowRateRated; // Rated Load Side Volumetric Flow Rate [m3/s]
		Real64 SourceSideVolFlowRateRated; // Rated Source Side Volumetric Flow Rate [m3/s]
		Real64 HeatCapCoeff1; // 1st coefficient of the heating capacity performance curve
		Real64 HeatCapCoeff2; // 2nd coefficient of the heating capacity performance curve
		Real64 HeatCapCoeff3; // 3rd coefficient of the heating capacity performance curve
		Real64 HeatCapCoeff4; // 4th coefficient of the heating capacity performance curve
		Real64 HeatCapCoeff5; // 5th coefficient of the heating capacity performance curve
		Real64 HeatPowerCoeff1; // 1st coefficient of the heating power consumption curve
		Real64 HeatPowerCoeff2; // 2nd coefficient of the heating power consumption curve
		Real64 HeatPowerCoeff3; // 3rd coefficient of the heating power consumption curve
		Real64 HeatPowerCoeff4; // 4th coefficient of the heating power consumption curve
		Real64 HeatPowerCoeff5; // 5th coefficient of the heating power consumption curve
		Real64 LoadSideMassFlowRate; // Load Side Mass Flow Rate [kg/s]
		Real64 LoadSideInletTemp; // Load Side Inlet Temperature [C]
		Real64 LoadSideOutletTemp; // Load side Outlet Temperature [C]
		Real64 SourceSideMassFlowRate; // Source Side Mass Flow Rate [kg/s]
		Real64 SourceSideInletTemp; // Source Side Inlet Temperature [C]
		Real64 SourceSideOutletTemp; // Source Side Outlet Temperature [C]
		Real64 func1; // Portion of the heat transfer and power equation
		Real64 func2; // Portion of the heat transfer and power equation
		Real64 func3; // Portion of the heat transfer and power equation
		Real64 func4; // Portion of the heat transfer and power equation
		Real64 Power; // Power Consumption [W]
		Real64 QLoad; // Cooling Capacity [W]
		Real64 QSource; // Source Side Heat Transfer Rate [W]
		Real64 PartLoadRatio; // Part Load Ratio
		Real64 ReportingConstant;
		Real64 rhoLoadSide;
		Real64 rhoSourceSide;
		Real64 CpLoadSide;
		Real64 CpSourceSide;

		//  LOAD LOCAL VARIABLES FROM DATA STRUCTURE
		LoadSideVolFlowRateRated = GSHP( GSHPNum ).RatedLoadVolFlowHeat;
		SourceSideVolFlowRateRated = GSHP( GSHPNum ).RatedSourceVolFlowHeat;
		HeatCapRated = GSHP( GSHPNum ).RatedCapHeat;
		HeatPowerRated = GSHP( GSHPNum ).RatedPowerHeat;
		HeatCapCoeff1 = GSHP( GSHPNum ).HeatCap1;
		HeatCapCoeff2 = GSHP( GSHPNum ).HeatCap2;
		HeatCapCoeff3 = GSHP( GSHPNum ).HeatCap3;
		HeatCapCoeff4 = GSHP( GSHPNum ).HeatCap4;
		HeatCapCoeff5 = GSHP( GSHPNum ).HeatCap5;
		HeatPowerCoeff1 = GSHP( GSHPNum ).HeatPower1;
		HeatPowerCoeff2 = GSHP( GSHPNum ).HeatPower2;
		HeatPowerCoeff3 = GSHP( GSHPNum ).HeatPower3;
		HeatPowerCoeff4 = GSHP( GSHPNum ).HeatPower4;
		HeatPowerCoeff5 = GSHP( GSHPNum ).HeatPower5;

		LoadSideMassFlowRate = GSHPReport( GSHPNum ).LoadSideMassFlowRate;
		LoadSideInletTemp = GSHPReport( GSHPNum ).LoadSideInletTemp;
		SourceSideMassFlowRate = GSHPReport( GSHPNum ).SourceSideMassFlowRate;
		SourceSideInletTemp = GSHPReport( GSHPNum ).SourceSideInletTemp;

		// If heat pump is not operating, THEN return
		if ( ! GSHP( GSHPNum ).MustRun ) {
			return;
		}
		rhoLoadSide = GetDensityGlycol( PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidName, LoadSideInletTemp, PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidIndex, RoutineName );

		rhoSourceSide = GetDensityGlycol( PlantLoop( GSHP( GSHPNum ).SourceLoopNum ).FluidName, SourceSideInletTemp, PlantLoop( GSHP( GSHPNum ).SourceLoopNum ).FluidIndex, RoutineName );

		func1 = ( ( LoadSideInletTemp + CelsiustoKelvin ) / Tref );
		func2 = ( ( SourceSideInletTemp + CelsiustoKelvin ) / Tref );
		func3 = ( LoadSideMassFlowRate / ( LoadSideVolFlowRateRated * rhoLoadSide ) );
		func4 = ( SourceSideMassFlowRate / ( SourceSideVolFlowRateRated * rhoSourceSide ) );

		QLoad = HeatCapRated * ( HeatCapCoeff1 + ( func1 * HeatCapCoeff2 ) + ( func2 * HeatCapCoeff3 ) + ( func3 * HeatCapCoeff4 ) + ( func4 * HeatCapCoeff5 ) );
		Power = HeatPowerRated * ( HeatPowerCoeff1 + ( func1 * HeatPowerCoeff2 ) + ( func2 * HeatPowerCoeff3 ) + ( func3 * HeatPowerCoeff4 ) + ( func4 * HeatPowerCoeff5 ) );

		if ( ( QLoad <= 0.0 || Power <= 0.0 ) && ! WarmupFlag ) {
			if ( QLoad <= 0.0 ) {
				if ( GSHP( GSHPNum ).HeatCapNegativeCounter < 1 ) {
					++GSHP( GSHPNum ).HeatCapNegativeCounter;
					ShowWarningError( HPEqFitHeating + " \"" + GSHP( GSHPNum ).Name + "\":" );
					ShowContinueError( " Heating capacity curve output is <= 0.0 (" + TrimSigDigits( QLoad, 4 ) + ")." );
					ShowContinueError( " Zero or negative value occurs with a load-side inlet temperature of " + TrimSigDigits( LoadSideInletTemp, 2 ) + " C," );
					ShowContinueError( " a source-side inlet temperature of " + TrimSigDigits( SourceSideInletTemp, 2 ) + " C," );
					ShowContinueError( " a load-side mass flow rate of " + TrimSigDigits( LoadSideMassFlowRate, 3 ) + " kg/s," );
					ShowContinueError( " and a source-side mass flow rate of " + TrimSigDigits( SourceSideMassFlowRate, 3 ) + " kg/s." );
					ShowContinueErrorTimeStamp( " The heat pump is turned off for this time step but simulation continues." );
				} else {
					ShowRecurringWarningErrorAtEnd( HPEqFitHeating + " \"" + GSHP( GSHPNum ).Name + "\": Heating capacity curve output is <= 0.0 warning continues...", GSHP( GSHPNum ).HeatCapNegativeIndex, QLoad, QLoad );
				}
			}
			if ( Power <= 0.0 ) {
				if ( GSHP( GSHPNum ).HeatPowerNegativeCounter < 1 ) {
					++GSHP( GSHPNum ).HeatPowerNegativeCounter;
					ShowWarningError( HPEqFitHeating + " \"" + GSHP( GSHPNum ).Name + "\":" );
					ShowContinueError( " Heating compressor power curve output is <= 0.0 (" + TrimSigDigits( Power, 4 ) + ")." );
					ShowContinueError( " Zero or negative value occurs with a load-side inlet temperature of " + TrimSigDigits( LoadSideInletTemp, 2 ) + " C," );
					ShowContinueError( " a source-side inlet temperature of " + TrimSigDigits( SourceSideInletTemp, 2 ) + " C," );
					ShowContinueError( " a load-side mass flow rate of " + TrimSigDigits( LoadSideMassFlowRate, 3 ) + " kg/s," );
					ShowContinueError( " and a source-side mass flow rate of " + TrimSigDigits( SourceSideMassFlowRate, 3 ) + " kg/s." );
					ShowContinueErrorTimeStamp( " The heat pump is turned off for this time step but simulation continues." );
				} else {
					ShowRecurringWarningErrorAtEnd( HPEqFitHeating + " \"" + GSHP( GSHPNum ).Name + "\": Heating compressor power curve output is <= 0.0 warning continues...", GSHP( GSHPNum ).HeatPowerNegativeIndex, Power, Power );
				}
			}

			QLoad = 0.0;
			Power = 0.0;

		}

		QSource = QLoad - Power; //assume no losses

		//Control Strategy
		if ( std::abs( MyLoad ) < QLoad && QLoad != 0.0 ) {
			PartLoadRatio = std::abs( MyLoad ) / QLoad;
			QLoad = std::abs( MyLoad );
			Power *= PartLoadRatio;
			QSource *= PartLoadRatio;
		}

		CpLoadSide = GetSpecificHeatGlycol( PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidName, LoadSideInletTemp, PlantLoop( GSHP( GSHPNum ).LoadLoopNum ).FluidIndex, RoutineName );

		CpSourceSide = GetSpecificHeatGlycol( PlantLoop( GSHP( GSHPNum ).SourceLoopNum ).FluidName, SourceSideInletTemp, PlantLoop( GSHP( GSHPNum ).SourceLoopNum ).FluidIndex, RoutineName );

		LoadSideOutletTemp = LoadSideInletTemp + QLoad / ( LoadSideMassFlowRate * CpLoadSide );
		SourceSideOutletTemp = SourceSideInletTemp - QSource / ( SourceSideMassFlowRate * CpSourceSide );

		ReportingConstant = TimeStepSys * SecInHour;

		GSHPReport( GSHPNum ).Power = Power;
		GSHPReport( GSHPNum ).Energy = Power * ReportingConstant;
		GSHPReport( GSHPNum ).QSource = QSource;
		GSHPReport( GSHPNum ).QLoad = QLoad;
		GSHPReport( GSHPNum ).QSourceEnergy = QSource * ReportingConstant;
		GSHPReport( GSHPNum ).QLoadEnergy = QLoad * ReportingConstant;
		GSHPReport( GSHPNum ).LoadSideOutletTemp = LoadSideOutletTemp;
		GSHPReport( GSHPNum ).SourceSideOutletTemp = SourceSideOutletTemp;
	}

	void
	UpdateGSHPRecords( int const GSHPNum ) // GSHP number
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Kenneth Tang
		//       DATE WRITTEN:    March 2005

		// PURPOSE OF THIS SUBROUTINE:
		// reporting

		// METHODOLOGY EMPLOYED: na

		// REFERENCES: na

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int SourceSideInletNode; // Source Side inlet node number, water side
		int SourceSideOutletNode; // Source Side outlet node number, water side
		int LoadSideInletNode; // Load Side inlet node number, water side
		int LoadSideOutletNode; // Load Side outlet node number, water side

		LoadSideInletNode = GSHP( GSHPNum ).LoadSideInletNodeNum;
		LoadSideOutletNode = GSHP( GSHPNum ).LoadSideOutletNodeNum;
		SourceSideInletNode = GSHP( GSHPNum ).SourceSideInletNodeNum;
		SourceSideOutletNode = GSHP( GSHPNum ).SourceSideOutletNodeNum;

		if ( ! GSHP( GSHPNum ).MustRun ) {
			// Heatpump is off; just pass through conditions
			GSHPReport( GSHPNum ).Power = 0.0;
			GSHPReport( GSHPNum ).Energy = 0.0;
			GSHPReport( GSHPNum ).QSource = 0.0;
			GSHPReport( GSHPNum ).QSourceEnergy = 0.0;
			GSHPReport( GSHPNum ).QLoad = 0.0;
			GSHPReport( GSHPNum ).QLoadEnergy = 0.0;
			GSHPReport( GSHPNum ).LoadSideOutletTemp = GSHPReport( GSHPNum ).LoadSideInletTemp;
			GSHPReport( GSHPNum ).SourceSideOutletTemp = GSHPReport( GSHPNum ).SourceSideInletTemp;
		}

		Node( SourceSideOutletNode ).Temp = GSHPReport( GSHPNum ).SourceSideOutletTemp;
		Node( LoadSideOutletNode ).Temp = GSHPReport( GSHPNum ).LoadSideOutletTemp;
	}

} // HeatPumpWaterToWaterSimple

} // EnergyPlus
