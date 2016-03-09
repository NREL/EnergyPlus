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
#include <cassert>
#include <cmath>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <DXCoils.hh>
#include <BranchNodeConnections.hh>
#include <CurveManager.hh>
#include <DataAirLoop.hh>
#include <DataAirSystems.hh>
#include <DataBranchNodeConnections.hh>
#include <DataContaminantBalance.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataLoopNode.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DataWater.hh>
#include <DataZoneEnergyDemands.hh>
#include <DataZoneEquipment.hh>
#include <EMSManager.hh>
#include <Fans.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <GlobalNames.hh>
#include <HeatBalanceInternalHeatGains.hh>
#include <HVACVariableRefrigerantFlow.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutAirNodeManager.hh>
#include <OutputProcessor.hh>
#include <OutputReportPredefined.hh>
#include <Psychrometrics.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <StandardRatings.hh>
#include <UtilityRoutines.hh>
#include <WaterManager.hh>

namespace EnergyPlus {

namespace DXCoils {

	// Module containing the DX coil simulation routines

	// MODULE INFORMATION:
	//       AUTHOR         Fred Buhl
	//       DATE WRITTEN   May 2000
	//       MODIFIED       Aug 2000, Don Shirey, Sept 2000, Feb/Oct 2001, Sept 2003, Jan 2004
	//                      Feb 2005, M. J. Witte, GARD Analytics, Inc., Add new coil type COIL:DX:MultiMode:CoolingEmpirical: Work supported by ASHRAE research project 1254-RP
	//                      Aug 2006, B Griffith, NREL, Added water system interactions for new water manager,
	//                      Feb 2010, B Nigusse, FSEC, Added Standard Rating for Coil:Cooling:DX:SingleSpeed
	//                      Apr 2010, Chandan Sharma, FSEC, Added basin heater routines for Coil:Cooling:DX:SingleSpeed, Coil:Cooling:DX:TwoSpeed,
	//                                Coil:Cooling:DX:MultiSpeed, and Coil:Cooling:DX:TwoStageWithHumidityControlMode
	//                      Feb 2013, Bereket Nigusse, FSEC, Added DX Coil Model For 100% OA systems
	//                      Jul 2015, RP Zhang, XF Pang, LBNL, Added new coil type for VRF-FluidTemperatureControl Model
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To encapsulate the data and algorithms required to simulate DX cooling coils in
	// EnergyPlus. Module currently models air-cooled or evap-cooled direct expansion systems
	// (split or packaged). Air-side performance is modeled to determine coil discharge
	// air conditions. The module also determines the DX unit's electrical energy usage.
	// Neither the air-side performance nor the electrical energy usage includes the effect
	// of supply air fan heat/energy usage. The supply air fan is modeled by other modules.

	// METHODOLOGY EMPLOYED:

	// REFERENCES:

	// OTHER NOTES:

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataLoopNode;
	using namespace DataGlobals;
	using namespace DataHVACGlobals;
	using namespace Psychrometrics;
	using DataEnvironment::StdPressureSeaLevel;
	using DataEnvironment::StdBaroPress;
	using DataEnvironment::EnvironmentName;
	using DataEnvironment::CurMnDy;
	using DataEnvironment::OutDryBulbTemp;
	using DataEnvironment::OutHumRat;
	using DataEnvironment::OutBaroPress;
	using DataEnvironment::OutWetBulbTemp;
	using DataHeatBalance::HeatReclaimDXCoil;

	// Use statements for access to subroutines in other modules
	using namespace ScheduleManager;

	// Data
	//MODULE PARAMETER DEFINITIONS
	// Defrost strategy (heat pump only)
	int const ReverseCycle( 1 ); // uses reverse cycle defrost strategy
	int const Resistive( 2 ); // uses electric resistance heater for defrost
	// Defrost control  (heat pump only)
	int const Timed( 1 ); // defrost cycle is timed
	int const OnDemand( 2 ); // defrost cycle occurs only when required
	// Compressor operation
	int const On( 1 ); // normal compressor operation
	int const Off( 0 ); // signal DXCoil that compressor shouldn't run

	Real64 const RatedInletAirTemp( 26.6667 ); // 26.6667C or 80F
	Real64 const RatedInletWetBulbTemp( 19.44 ); // 19.44 or 67F
	Real64 const RatedInletAirHumRat( 0.01125 ); // Humidity ratio corresponding to 80F dry bulb/67F wet bulb
	Real64 const RatedOutdoorAirTemp( 35.0 ); // 35 C or 95F
	Real64 const RatedInletAirTempHeat( 21.11 ); // 21.11C or 70F
	Real64 const RatedOutdoorAirTempHeat( 8.33 ); // 8.33 C or 47F
	Real64 const RatedInletWetBulbTempHeat( 15.55 ); // 15.55 or 60F

	Real64 const DryCoilOutletHumRatioMin( 0.00001 ); // dry coil outlet minimum hum ratio kgH2O/kgdry air

	// Curve Types
	int const Linear( 1 );
	int const BiLinear( 2 );
	int const Quadratic( 3 );
	int const BiQuadratic( 4 );
	int const Cubic( 5 );

	// Multimode DX Coil
	int const MaxCapacityStages( 2 ); // Maximum number of capacity stages supported
	int const MaxDehumidModes( 1 ); // Maximum number of enhanced dehumidification modes supported
	int const MaxModes( MaxCapacityStages * ( MaxDehumidModes + 1 ) ); // Maximum number of performance modes

	//Water Systems
	int const CondensateDiscarded( 1001 ); // default mode where water is "lost"
	int const CondensateToTank( 1002 ); // collect coil condensate from air and store in water storage tank

	int const WaterSupplyFromMains( 101 );
	int const WaterSupplyFromTank( 102 );

	int const NumValidOutputFuelTypes( 9 );
	Array1D_string const cValidOutputFuelTypes( NumValidOutputFuelTypes, { "Electricity", "Gas", "Propane", "Diesel", "Gasoline", "FuelOil#1", "FuelOil#2", "OtherFuel1", "OtherFuel2" } );

	static std::string const BlankString;

	// Fuel Types
	int const FuelTypeElectricity( 1 ); // Fuel type for electricity
	int const FuelTypeNaturalGas( 2 ); // Fuel type for natural gas
	int const FuelTypePropaneGas( 3 ); // Fuel type for propane gas
	int const FuelTypeDiesel( 4 ); // Fuel type for diesel
	int const FuelTypeGasoline( 5 ); // Fuel type for gasoline
	int const FuelTypeFuelOil1( 6 ); // Fuel type for fuel oil #1
	int const FuelTypeFuelOil2( 7 ); // Fuel type for fuel oil #2
	int const FuelTypeOtherFuel1( 8 ); // Fuel type for other fuel #1
	int const FuelTypeOtherFuel2( 9 ); // Fuel type for other fuel #2

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	Array1D< Real64 > DXCoilOutletTemp; // DX coil outlet dry bulb temperature [C]
	Array1D< Real64 > DXCoilOutletHumRat; // DX coil outlet humidity ratio [kgWater/kgDryAir]
	Array1D< Real64 > DXCoilPartLoadRatio; // DX coil part-load ratio
	Array1D_int DXCoilFanOpMode; // supply air fan operating mode
	Array1D< Real64 > DXCoilFullLoadOutAirTemp; // DX coil full load outlet dry bulb temperature [C]
	Array1D< Real64 > DXCoilFullLoadOutAirHumRat; // DX coil full load outlet humidity ratio [kgWater/kgDryAir]
	Array1D< Real64 > DXCoilTotalCooling; // DX cooling coil total cooling output [W]
	Array1D< Real64 > DXCoilTotalHeating; // DX heating coil total heating output [W]
	Array1D< Real64 > DXCoilCoolInletAirWBTemp; // DX cooling coil inlet air wet-bulb temp [C]
	Array1D< Real64 > DXCoilHeatInletAirDBTemp; // DX heating coil inlet air dry-bulb temp [C]
	Array1D< Real64 > DXCoilHeatInletAirWBTemp; // DX heating coil inlet air wet-bulb temp [C]

	int CurDXCoilNum( 0 );

	int NumDXCoils( 0 ); // Total number of DX coils
	Real64 HPWHHeatingCapacity( 0.0 ); // Used by Heat Pump:Water Heater object as total water heating capacity [W]
	Real64 HPWHHeatingCOP( 0.0 ); // Used by Heat Pump:Water Heater object as water heating COP [W/W]
	bool GetCoilsInputFlag( true ); // First time, input is "gotten"
	bool MyOneTimeFlag( true ); // One time flag used to allocate MyEnvrnFlag and MySizeFlag
	int NumVRFHeatingCoils( 0 ); // number of VRF heat pump heating coils
	int NumVRFCoolingCoils( 0 ); // number of VRF heat pump cooling coils
	int NumVRFHeatingFluidTCtrlCoils( 0 ); // number of VRF heat pump heating coils for FluidTCtrl Model
	int NumVRFCoolingFluidTCtrlCoils( 0 ); // number of VRF heat pump cooling coils for FluidTCtrl Model
	int NumDXHeatingCoils( 0 ); // number of DX heat pump heating coils
	int NumDoe2DXCoils( 0 ); // number of doe2 DX  coils
	int NumDXHeatPumpWaterHeaterPumpedCoils( 0 ); // number of DX  water heater coils, pumped
	int NumDXHeatPumpWaterHeaterWrappedCoils( 0 ); // number of DX  water heater coils, pumped
	int NumDXMulSpeedCoils( 0 ); // number of DX coils with multi-speed compressor
	int NumDXMulModeCoils( 0 ); // number of DX coils with multi-mode performance

	int NumDXMulSpeedCoolCoils( 0 ); // number of multispeed DX cooling coils
	int NumDXMulSpeedHeatCoils( 0 ); // number of multispeed DX heating coils
	Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE

	// Driver/Manager Routines

	// Get Input routines for module

	// Initialization routines for module

	// Update routines to check convergence and update nodes

	// Common routines

	// External function calls

	// Object Data
	Array1D< DXCoilData > DXCoil;
	Array1D< DXCoilNumericFieldData > DXCoilNumericFields;

	// Functions

	void
	SimDXCoil(
		std::string const & CompName, // name of the fan coil unit
		int const CompOp, // compressor operation; 1=on, 0=off
		bool const FirstHVACIteration, // True when first HVAC iteration
		int & CompIndex,
		int const FanOpMode, // allows parent object to control fan mode
		Optional< Real64 const > PartLoadRatio, // part load ratio (for single speed cycling unit)
		Optional< Real64 const > OnOffAFR, // ratio of compressor on airflow to compressor off airflow
		Optional< Real64 const > CoilCoolingHeatingPLRRatio, // used for cycling fan RH control
		Optional< Real64 const > MaxCap, // maximum cooling capacity of VRF terminal units
		Optional< Real64 const > CompCyclingRatio // cycling ratio of VRF condenser connected to this TU
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   May 2000
		//       MODIFIED       Don Shirey, Sept 2000, October 2001, June 2005
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Manages the simulation of a single speed on/off DX coil.

		// METHODOLOGY EMPLOYED:
		// NA

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using General::TrimSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int DXCoilNum; // index of fan coil unit being simulated
		Real64 AirFlowRatio; // ratio of compressor on airflow to compressor off airflow
		Real64 CompCycRatio; // compressor cycling ratio of VRF condenser

		// FLOW

		// First time SimDXCoil is called, get the input for all the DX coils (condensing units)
		if ( GetCoilsInputFlag ) {
			GetDXCoils();
			GetCoilsInputFlag = false; // Set GetInputFlag false so you don't get coil inputs again
		}

		if ( CompIndex == 0 ) {
			DXCoilNum = FindItemInList( CompName, DXCoil );
			if ( DXCoilNum == 0 ) {
				ShowFatalError( "DX Coil not found=" + CompName );
			}
			CompIndex = DXCoilNum;
		} else {
			DXCoilNum = CompIndex;
			if ( DXCoilNum > NumDXCoils || DXCoilNum < 1 ) {
				ShowFatalError( "SimDXCoil: Invalid CompIndex passed=" + TrimSigDigits( DXCoilNum ) + ", Number of DX Coils=" + TrimSigDigits( NumDXCoils ) + ", Coil name=" + CompName );
			}
			if ( CheckEquipName( DXCoilNum ) ) {
				if ( ! CompName.empty() && CompName != DXCoil( DXCoilNum ).Name ) {
					ShowFatalError( "SimDXCoil: Invalid CompIndex passed=" + TrimSigDigits( DXCoilNum ) + ", Coil name=" + CompName + ", stored Coil Name for that index=" + DXCoil( DXCoilNum ).Name );
				}
				CheckEquipName( DXCoilNum ) = false;
			}
		}

		if ( present( OnOffAFR ) ) {
			AirFlowRatio = OnOffAFR;
		} else {
			AirFlowRatio = 1.0;
		}

		if ( present( CompCyclingRatio ) ) {
			CompCycRatio = CompCyclingRatio;
		} else {
			CompCycRatio = 1.0;
		}

		CurDXCoilNum = DXCoilNum;

		// Initialize the DX coil unit
		InitDXCoil( DXCoilNum );

		// Select the correct unit type
		{ auto const SELECT_CASE_var( DXCoil( DXCoilNum ).DXCoilType_Num ); //Autodesk:OPTIONAL PartLoadRatio, MaxCap used in this block without PRESENT check

		if ( SELECT_CASE_var == CoilDX_CoolingSingleSpeed ) {

			if ( present( CoilCoolingHeatingPLRRatio ) ) {
				CalcDoe2DXCoil( DXCoilNum, CompOp, FirstHVACIteration, PartLoadRatio, FanOpMode, _, AirFlowRatio, CoilCoolingHeatingPLRRatio );
			} else {
				CalcDoe2DXCoil( DXCoilNum, CompOp, FirstHVACIteration, PartLoadRatio, FanOpMode, _, AirFlowRatio );
			}

		} else if ( SELECT_CASE_var == CoilDX_HeatingEmpirical ) {

			CalcDXHeatingCoil( DXCoilNum, PartLoadRatio, FanOpMode, AirFlowRatio );

		} else if ( SELECT_CASE_var == CoilDX_HeatPumpWaterHeaterPumped || SELECT_CASE_var == CoilDX_HeatPumpWaterHeaterWrapped ) {

			//   call the HPWHDXCoil routine to calculate water side performance set up the DX coil info for air-side calcs
			CalcHPWHDXCoil( DXCoilNum, PartLoadRatio );
			//    CALL CalcDOE2DXCoil(DXCoilNum, CompOp, FirstHVACIteration,PartLoadRatio), perform air-side calculations
			CalcDoe2DXCoil( DXCoilNum, 1, FirstHVACIteration, PartLoadRatio, FanOpMode );

		} else if ( SELECT_CASE_var == CoilVRF_Cooling ) {

			CalcVRFCoolingCoil( DXCoilNum, 1, FirstHVACIteration, PartLoadRatio, FanOpMode, CompCycRatio, _, _, MaxCap );

		} else if ( SELECT_CASE_var == CoilVRF_Heating ) {

			CalcDXHeatingCoil( DXCoilNum, PartLoadRatio, FanOpMode, _, MaxCap );

		} else if ( SELECT_CASE_var == CoilVRF_FluidTCtrl_Cooling ) {

			CalcVRFCoolingCoil_FluidTCtrl( DXCoilNum, 1, FirstHVACIteration, PartLoadRatio, FanOpMode, CompCycRatio, _, _ );

		} else if ( SELECT_CASE_var == CoilVRF_FluidTCtrl_Heating ) {

			CalcVRFHeatingCoil_FluidTCtrl( CompOp, DXCoilNum, PartLoadRatio, FanOpMode, _, MaxCap);

		} else {
			ShowSevereError( "Error detected in DX Coil=" + CompName );
			ShowContinueError( "Invalid DX Coil Type=" + DXCoil( DXCoilNum ).DXCoilType );
			ShowFatalError( "Preceding condition causes termination." );

		}}

		// Update the unit outlet nodes
		UpdateDXCoil( DXCoilNum );

		// Report the result of the simulation
		ReportDXCoil( DXCoilNum );

	}

	void
	SimDXCoilMultiSpeed(
		std::string const & CompName, // name of the fan coil unit
		Real64 const SpeedRatio, // = (CompressorSpeed - CompressorSpeedMin) /
		Real64 const CycRatio, // cycling part load ratio for variable speed
		int & CompIndex,
		Optional_int_const SpeedNum, // Speed number for multispeed cooling coil onlyn
		Optional_int_const FanOpMode, // Fan operation mode
		Optional_int_const CompOp, // Compressor on/off; 1=on, 0=off
		Optional_int_const SingleMode // Single mode operation Yes/No; 1=Yes, 0=No
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   September 2002
		//       MODIFIED       Lixing Gu, Sep. 2007
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Manages the simulation of a multi speed DX coil.

		// METHODOLOGY EMPLOYED:
		// NA

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using General::TrimSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		//   (CompressorSpeedMax - CompressorSpeedMin)
		// for variable speed or 2 speed compressors
		// or 2 speed compressors

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int DXCoilNum; // index of fan coil unit being simulated
		int SingleModeOper; // SingleMode Operation

		// FLOW

		// First time SimDXCoil is called, get the input for all the DX coils (condensing units)
		if ( GetCoilsInputFlag ) {
			GetDXCoils();
			GetCoilsInputFlag = false; // Set GetInputFlag false so you don't get coil inputs again
		}

		//  find correct DX Coil

		if ( CompIndex == 0 ) {
			DXCoilNum = FindItemInList( CompName, DXCoil );
			if ( DXCoilNum == 0 ) {
				ShowFatalError( "DX Coil not found=" + CompName );
			}
			CompIndex = DXCoilNum;
		} else {
			DXCoilNum = CompIndex;
			if ( DXCoilNum > NumDXCoils || DXCoilNum < 1 ) {
				ShowFatalError( "SimDXCoilMultiSpeed: Invalid CompIndex passed=" + TrimSigDigits( DXCoilNum ) + ", Number of DX Coils=" + TrimSigDigits( NumDXCoils ) + ", Coil name=" + CompName );
			}
			if ( CheckEquipName( DXCoilNum ) ) {
				if ( ! CompName.empty() && CompName != DXCoil( DXCoilNum ).Name ) {
					ShowFatalError( "SimDXCoilMultiSpeed: Invalid CompIndex passed=" + TrimSigDigits( DXCoilNum ) + ", Coil name=" + CompName + ", stored Coil Name for that index=" + DXCoil( DXCoilNum ).Name );
				}
				CheckEquipName( DXCoilNum ) = false;
			}
		}

		if ( present( SingleMode ) ) {
			SingleModeOper = SingleMode;
		} else {
			SingleModeOper = 0;
		}

		CurDXCoilNum = DXCoilNum;

		// Initialize the DX coil unit
		InitDXCoil( DXCoilNum );

		// Select the correct unit type
		{ auto const SELECT_CASE_var( DXCoil( DXCoilNum ).DXCoilType_Num );

		if ( SELECT_CASE_var == CoilDX_CoolingTwoSpeed ) {

			CalcMultiSpeedDXCoil( DXCoilNum, SpeedRatio, CycRatio );

		} else if ( SELECT_CASE_var == CoilDX_MultiSpeedCooling ) {
			if ( present( SpeedNum ) ) CalcMultiSpeedDXCoilCooling( DXCoilNum, SpeedRatio, CycRatio, SpeedNum, FanOpMode, CompOp, SingleModeOper ); //Autodesk:OPTIONAL FanOpMode, CompOp used without PRESENT check

		} else if ( SELECT_CASE_var == CoilDX_MultiSpeedHeating ) {
			if ( present( SpeedNum ) ) CalcMultiSpeedDXCoilHeating( DXCoilNum, SpeedRatio, CycRatio, SpeedNum, FanOpMode, SingleModeOper ); //Autodesk:OPTIONAL FanOpMode used without PRESENT check

		} else {
			ShowSevereError( "Error detected in DX Coil=" + CompName );
			ShowContinueError( "Invalid DX Coil Type=" + DXCoil( DXCoilNum ).DXCoilType );
			ShowFatalError( "Preceding condition causes termination." );

		}}

		// Update the unit outlet nodes
		UpdateDXCoil( DXCoilNum );

		// Report the result of the simulation
		ReportDXCoil( DXCoilNum );

	}

	void
	SimDXCoilMultiMode(
		std::string const & CompName, // name of the fan coil unit
		int const EP_UNUSED( CompOp ), // compressor operation; 1=on, 0=off !unused1208
		bool const FirstHVACIteration, // true if first hvac iteration
		Real64 const PartLoadRatio, // part load ratio
		int const DehumidMode, // dehumidification mode (0=normal, 1=enhanced)
		int & CompIndex,
		int const FanOpMode // allows parent object to control fan mode
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         M. J. Witte (based on SimDXCoilMultiSpeed by Fred Buhl)
		//       DATE WRITTEN   February 2005
		//       MODIFIED       April 2010, Chandan sharma, added basin heater
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Manages the simulation of a DX coil with multiple performance modes, such as
		// multiple stages, or sub-cool reheat for humidity control.

		// METHODOLOGY EMPLOYED:
		// NA

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using General::TrimSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "SimDXCoilMultiMode" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int DXCoilNum; // index of coil being simulated
		int PerfMode; // Performance mode for MultiMode DX coil; Always 1 for other coil types
		// 1-2=normal mode: 1=stage 1 only, 2=stage 1&2
		// 3-4=enhanced dehumidification mode: 3=stage 1 only, 4=stage 1&2
		Real64 AirMassFlow; // Dry air mass flow rate through coil [kg/s]

		Real64 S1OutletAirTemp; // Stage 1   Outlet air dry bulb temp [C]
		Real64 S1OutletAirHumRat; // Stage 1   Outlet air humidity ratio [kgWater/kgDryAir]
		Real64 S1OutletAirEnthalpy; // Stage 1   Outlet air enthalpy
		Real64 S1PLR; // Stage 1   Ratio of actual sensible cooling load to
		//           steady-state sensible cooling capacity
		Real64 S1TotalCoolingEnergyRate; // Stage 1   Total cooling rate [W]
		Real64 S1SensCoolingEnergyRate; // Stage 1   Sensible cooling rate [W]
		Real64 S1LatCoolingEnergyRate; // Stage 1   Latent cooling rate [W]
		Real64 S1ElecCoolingPower; // Stage 1   Electric power input [W]
		static Real64 S1RuntimeFraction( 0.0 ); // Stage 1   Run time fraction (overlaps with stage1&2 run time)
		Real64 S1EvapCondPumpElecPower; // Stage 1   Evaporative condenser pump electric power input [W]
		Real64 S1EvapWaterConsumpRate; // Stage 1   Evap condenser water consumption rate [m3/s]
		Real64 S1CrankcaseHeaterPower; // Stage 1   Report variable for average crankcase heater power [W]
		Real64 S1FFullLoadOutAirTemp; // Stage 1   Full load outlet temperature [C]
		Real64 S1FullLoadOutAirHumRat; // Stage 1   Full load outlet humidity ratio [kgWater/kgDryAir]

		Real64 S12OutletAirTemp; // Stage 1&2 Outlet air dry bulb temp [C]
		Real64 S12OutletAirHumRat; // Stage 1&2 Outlet air humidity ratio [kgWater/kgDryAir]
		Real64 S12OutletAirEnthalpy; // Stage 1&2 Outlet air enthalpy
		//  REAL(r64) :: S12PLR                  ! Stage 1&2 Ratio of actual sensible cooling load to
		//                                       !           steady-state sensible cooling capacity
		Real64 S12TotalCoolingEnergyRate; // Stage 1&2 Total cooling rate [W]
		Real64 S12SensCoolingEnergyRate; // Stage 1&2 Sensible cooling rate [W]
		Real64 S12LatCoolingEnergyRate; // Stage 1&2 Latent cooling rate [W]
		Real64 S12ElecCoolingPower; // Stage 1&2 Electric power input [W]
		Real64 S12ElecCoolFullLoadPower; // Stage 1&2 Electric power input at full load (PLR=1) [W]
		static Real64 S12RuntimeFraction( 0.0 ); // Stage 1&2 Run time fraction (overlaps with stage1 run time)
		Real64 S12EvapCondPumpElecPower; // Stage 1&2 Evaporative condenser pump electric power input [W]
		Real64 S12EvapWaterConsumpRate; // Stage 1&2 Evap condenser water consumption rate [m3/s]
		Real64 S12CrankcaseHeaterPower; // Stage 1&2 Report variable for average crankcase heater power [W]
		Real64 S2PLR; // Stage 2   Ratio of actual sensible cooling load to
		//           steady-state sensible cooling capacity
		static Real64 MinAirHumRat( 0.0 ); // minimum of the inlet air humidity ratio and the outlet air humidity ratio
		Real64 TSat; // calculation to avoid calling psych routines twice
		Real64 NodePress; // Pressure at condenser inlet node (Pa)
		// FLOW

		// First time SimDXCoil is called, get the input for all the DX coils (condensing units)
		if ( GetCoilsInputFlag ) {
			GetDXCoils();
			GetCoilsInputFlag = false; // Set GetInputFlag false so you don't get coil inputs again
		}

		//  find correct DX Coil
		if ( CompIndex == 0 ) {
			DXCoilNum = FindItemInList( CompName, DXCoil );
			if ( DXCoilNum == 0 ) {
				ShowFatalError( "DX Coil not found=" + CompName );
			}
			CompIndex = DXCoilNum;
		} else {
			DXCoilNum = CompIndex;
			if ( DXCoilNum > NumDXCoils || DXCoilNum < 1 ) {
				ShowFatalError( "SimDXCoilMultiMode: Invalid CompIndex passed=" + TrimSigDigits( DXCoilNum ) + ", Number of DX Coils=" + TrimSigDigits( NumDXCoils ) + ", Coil name=" + CompName );
			}
			if ( CheckEquipName( DXCoilNum ) ) {
				if ( ( CompName != "" ) && ( CompName != DXCoil( DXCoilNum ).Name ) ) {
					ShowFatalError( "SimDXCoilMultiMode: Invalid CompIndex passed=" + TrimSigDigits( DXCoilNum ) + ", Coil name=" + CompName + ", stored Coil Name for that index=" + DXCoil( DXCoilNum ).Name );
				}
				CheckEquipName( DXCoilNum ) = false;
			}
		}

		CurDXCoilNum = DXCoilNum;

		// Initialize the DX coil unit
		InitDXCoil( DXCoilNum );

		// Select the correct unit type
		{ auto const SELECT_CASE_var( DXCoil( DXCoilNum ).DXCoilType_Num );

		if ( SELECT_CASE_var == CoilDX_CoolingTwoStageWHumControl ) {

			// Initialize local variables
			S1RuntimeFraction = 0.0;
			S1OutletAirEnthalpy = DXCoil( DXCoilNum ).InletAirEnthalpy;
			S1OutletAirHumRat = DXCoil( DXCoilNum ).InletAirHumRat;
			S1OutletAirTemp = DXCoil( DXCoilNum ).InletAirTemp;
			S1ElecCoolingPower = 0.0;
			S1TotalCoolingEnergyRate = 0.0;
			S1SensCoolingEnergyRate = 0.0;
			S1LatCoolingEnergyRate = 0.0;
			S1CrankcaseHeaterPower = 0.0;
			S1EvapWaterConsumpRate = 0.0;
			S1EvapCondPumpElecPower = 0.0;

			S12RuntimeFraction = 0.0;
			S12OutletAirEnthalpy = DXCoil( DXCoilNum ).InletAirEnthalpy;
			S12OutletAirHumRat = DXCoil( DXCoilNum ).InletAirHumRat;
			S12OutletAirTemp = DXCoil( DXCoilNum ).InletAirTemp;
			S12ElecCoolingPower = 0.0;
			S12TotalCoolingEnergyRate = 0.0;
			S12SensCoolingEnergyRate = 0.0;
			S12LatCoolingEnergyRate = 0.0;
			S12CrankcaseHeaterPower = 0.0;
			S12EvapWaterConsumpRate = 0.0;
			S12EvapCondPumpElecPower = 0.0;

			DXCoil( DXCoilNum ).DehumidificationMode = DehumidMode;
			if ( DehumidMode > DXCoil( DXCoilNum ).NumDehumidModes ) {
				ShowFatalError( DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + "\" - Requested enhanced dehumidification mode not available." );
			}

			// If a single-stage coil OR If part load is zero,
			// run stage 1 at zero part load to set leaving conditions
			if ( ( DXCoil( DXCoilNum ).NumCapacityStages == 1 ) || ( PartLoadRatio <= 0.0 ) ) {
				// Run stage 1 at its part load
				PerfMode = DehumidMode * 2 + 1;
				CalcDoe2DXCoil( DXCoilNum, On, FirstHVACIteration, PartLoadRatio, FanOpMode, PerfMode );
				S1PLR = PartLoadRatio;
				S2PLR = 0.0;
			} else {
				// If a two-stage coil
				// Run stage 1 at full load
				PerfMode = DehumidMode * 2 + 1;
				CalcDoe2DXCoil( DXCoilNum, On, FirstHVACIteration, 1.0, FanOpMode, PerfMode );
				S1SensCoolingEnergyRate = DXCoil( DXCoilNum ).SensCoolingEnergyRate;
				if ( S1SensCoolingEnergyRate > 0.0 ) {
					S1PLR = PartLoadRatio;
				} else {
					S1PLR = 0.0;
				}
				// Run stage 1+2 at full load
				if ( DXCoil( DXCoilNum ).NumCapacityStages >= 2 ) {
					PerfMode = DehumidMode * 2 + 2;
					CalcDoe2DXCoil( DXCoilNum, On, FirstHVACIteration, 1.0, FanOpMode, PerfMode );
					S12SensCoolingEnergyRate = DXCoil( DXCoilNum ).SensCoolingEnergyRate;
					S12ElecCoolFullLoadPower = DXCoil( DXCoilNum ).ElecCoolingPower;
				}

				// Determine run-time fractions for each stage based on sensible capacities
				//   Relationships:
				//     Stage 1   PLR1=   Load/Cap1
				//     Stage1+2  PLR12=  Load/Cap12
				//     Stage 2   PLR2=   (Load-Cap1)/(Cap2)
				//     PLR = Load/(Cap1+Cap2)
				//     Load= PLR*(Cap1+Cap2)
				//     PLR1= MIN(1,(PLR*(Cap1+Cap2)/Cap1))
				//     PLR2= MIN(1,((PLR*(Cap1+Cap2)-Cap1)/Cap2))

				if ( S1SensCoolingEnergyRate > 0.0 ) {
					S1PLR = PartLoadRatio * S12SensCoolingEnergyRate / S1SensCoolingEnergyRate;
				} else {
					S1PLR = 0.0;
				}
				S1PLR = min( 1.0, S1PLR );
				S1PLR = max( 0.0, S1PLR );
				if ( ( S12SensCoolingEnergyRate - S1SensCoolingEnergyRate ) > 0.0 ) {
					S2PLR = ( PartLoadRatio * S12SensCoolingEnergyRate - S1SensCoolingEnergyRate ) / ( S12SensCoolingEnergyRate - S1SensCoolingEnergyRate );
				} else {
					S2PLR = 0.0;
				}
				S2PLR = min( 1.0, S2PLR );
				S2PLR = max( 0.0, S2PLR );

				// Run stage 1 at its part load
				PerfMode = DehumidMode * 2 + 1;
				CalcDoe2DXCoil( DXCoilNum, On, FirstHVACIteration, S1PLR, FanOpMode, PerfMode );
			}
			// For stage-1 only operation, all outputs are set by CalcDoe2DXCoil.
			// No further adjustments are necessary.

			// Run stage 2 if needed and available
			if ( ( S2PLR > 0.0 ) && ( DXCoil( DXCoilNum ).NumCapacityStages >= 2 ) ) {
				// Store stage 1 outputs
				S1RuntimeFraction = DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction;
				S1OutletAirEnthalpy = DXCoil( DXCoilNum ).OutletAirEnthalpy;
				S1OutletAirHumRat = DXCoil( DXCoilNum ).OutletAirHumRat;
				S1OutletAirTemp = DXCoil( DXCoilNum ).OutletAirTemp;
				S1ElecCoolingPower = DXCoil( DXCoilNum ).ElecCoolingPower;
				S1TotalCoolingEnergyRate = DXCoil( DXCoilNum ).TotalCoolingEnergyRate;
				S1SensCoolingEnergyRate = DXCoil( DXCoilNum ).SensCoolingEnergyRate;
				S1LatCoolingEnergyRate = DXCoil( DXCoilNum ).LatCoolingEnergyRate;
				S1CrankcaseHeaterPower = DXCoil( DXCoilNum ).CrankcaseHeaterPower;
				S1EvapWaterConsumpRate = DXCoil( DXCoilNum ).EvapWaterConsumpRate;
				S1EvapCondPumpElecPower = DXCoil( DXCoilNum ).EvapCondPumpElecPower;

				// Save first stage full load outlet conditions to pass to heat recovery
				S1FFullLoadOutAirTemp = DXCoilFullLoadOutAirTemp( DXCoilNum );
				S1FullLoadOutAirHumRat = DXCoilFullLoadOutAirHumRat( DXCoilNum );

				// Run stage 1+2 at its part load
				PerfMode = DehumidMode * 2 + 2;
				CalcDoe2DXCoil( DXCoilNum, On, FirstHVACIteration, S2PLR, FanOpMode, PerfMode );
				S12RuntimeFraction = DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction;
				S12OutletAirEnthalpy = DXCoil( DXCoilNum ).OutletAirEnthalpy;
				S12OutletAirHumRat = DXCoil( DXCoilNum ).OutletAirHumRat;
				S12OutletAirTemp = DXCoil( DXCoilNum ).OutletAirTemp;
				S12ElecCoolingPower = DXCoil( DXCoilNum ).ElecCoolingPower;
				S12TotalCoolingEnergyRate = DXCoil( DXCoilNum ).TotalCoolingEnergyRate;
				S12SensCoolingEnergyRate = DXCoil( DXCoilNum ).SensCoolingEnergyRate;
				S12LatCoolingEnergyRate = DXCoil( DXCoilNum ).LatCoolingEnergyRate;
				S12CrankcaseHeaterPower = DXCoil( DXCoilNum ).CrankcaseHeaterPower;
				S12EvapWaterConsumpRate = DXCoil( DXCoilNum ).EvapWaterConsumpRate;
				S12EvapCondPumpElecPower = DXCoil( DXCoilNum ).EvapCondPumpElecPower;

				// Determine combined performance
				DXCoil( DXCoilNum ).OutletAirEnthalpy = ( 1.0 - S12RuntimeFraction ) * S1OutletAirEnthalpy + S12RuntimeFraction * S12OutletAirEnthalpy;
				DXCoil( DXCoilNum ).OutletAirHumRat = ( 1.0 - S12RuntimeFraction ) * S1OutletAirHumRat + S12RuntimeFraction * S12OutletAirHumRat;
				DXCoil( DXCoilNum ).OutletAirTemp = PsyTdbFnHW( DXCoil( DXCoilNum ).OutletAirEnthalpy, DXCoil( DXCoilNum ).OutletAirHumRat );
				// Check for saturation error and modify temperature at constant enthalpy
				if ( DXCoil( DXCoilNum ).CondenserInletNodeNum( PerfMode ) != 0 ) {
					NodePress = Node( DXCoil( DXCoilNum ).CondenserInletNodeNum( PerfMode ) ).Press;
					// If node is not connected to anything, pressure = default, use weather data
					if ( NodePress == DefaultNodeValues.Press ) NodePress = OutBaroPress;
					TSat = PsyTsatFnHPb( DXCoil( DXCoilNum ).OutletAirEnthalpy, NodePress, RoutineName );
					if ( DXCoil( DXCoilNum ).OutletAirTemp < TSat ) {
						DXCoil( DXCoilNum ).OutletAirTemp = TSat;
					}
					DXCoil( DXCoilNum ).OutletAirHumRat = PsyWFnTdbH( DXCoil( DXCoilNum ).OutletAirTemp, DXCoil( DXCoilNum ).OutletAirEnthalpy, RoutineName );
				} else {
					TSat = PsyTsatFnHPb( DXCoil( DXCoilNum ).OutletAirEnthalpy, OutBaroPress, RoutineName );
					if ( DXCoil( DXCoilNum ).OutletAirTemp < TSat ) {
						DXCoil( DXCoilNum ).OutletAirTemp = TSat;
					}
					//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
					//      IF(DXCoil(DXCoilNum)%OutletAirTemp .LT. PsyTsatFnHPb(DXCoil(DXCoilNum)%OutletAirEnthalpy, &
					//                 Node(DXCoil(DXCoilNum)%AirInNode)%Press)) THEN
					//        DXCoil(DXCoilNum)%OutletAirTemp = PsyTsatFnHPb(DXCoil(DXCoilNum)%OutletAirEnthalpy, &
					//                 Node(DXCoil(DXCoilNum)%AirInNode)%Press)
					DXCoil( DXCoilNum ).OutletAirHumRat = PsyWFnTdbH( DXCoil( DXCoilNum ).OutletAirTemp, DXCoil( DXCoilNum ).OutletAirEnthalpy, RoutineName );
				}

				//      DXCoil(DXCoilNum)%ElecCoolingPower = (1-S12RuntimeFraction)*S1ElecCoolingPower &
				//                                             +S12RuntimeFraction*S12ElecCoolingPower
				//  S12ElecCoolingPower overstates S1 portion of power, because it is also adjust by S12PLR
				//  So, must make an adjustment for S12ElecCoolingPower/S12ElecCoolFullLoadPower
				//  when subtracting off S1ElecCoolingPower
				if ( S12ElecCoolFullLoadPower > 0.0 ) {
					DXCoil( DXCoilNum ).ElecCoolingPower = S1RuntimeFraction * S1ElecCoolingPower + S12RuntimeFraction * ( S12ElecCoolingPower - S1ElecCoolingPower * S12ElecCoolingPower / S12ElecCoolFullLoadPower );
				} else {
					DXCoil( DXCoilNum ).ElecCoolingPower = 0.0;
				}

				DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction = S1RuntimeFraction;

				AirMassFlow = DXCoil( DXCoilNum ).InletAirMassFlowRate;
				DXCoil( DXCoilNum ).TotalCoolingEnergyRate = AirMassFlow * ( DXCoil( DXCoilNum ).InletAirEnthalpy - DXCoil( DXCoilNum ).OutletAirEnthalpy );
				MinAirHumRat = min( DXCoil( DXCoilNum ).InletAirHumRat, DXCoil( DXCoilNum ).OutletAirHumRat );
				DXCoil( DXCoilNum ).SensCoolingEnergyRate = AirMassFlow * ( PsyHFnTdbW( DXCoil( DXCoilNum ).InletAirTemp, MinAirHumRat ) - PsyHFnTdbW( DXCoil( DXCoilNum ).OutletAirTemp, MinAirHumRat ) );
				//  Don't let sensible capacity be greater than total capacity
				if ( DXCoil( DXCoilNum ).SensCoolingEnergyRate > DXCoil( DXCoilNum ).TotalCoolingEnergyRate ) {
					DXCoil( DXCoilNum ).SensCoolingEnergyRate = DXCoil( DXCoilNum ).TotalCoolingEnergyRate;
				}

				DXCoil( DXCoilNum ).LatCoolingEnergyRate = DXCoil( DXCoilNum ).TotalCoolingEnergyRate - DXCoil( DXCoilNum ).SensCoolingEnergyRate;

				DXCoil( DXCoilNum ).EvapWaterConsumpRate = ( 1.0 - S12RuntimeFraction ) * S1EvapWaterConsumpRate + S12RuntimeFraction * S12EvapWaterConsumpRate;
				DXCoil( DXCoilNum ).EvapCondPumpElecPower = ( 1.0 - S12RuntimeFraction ) * S1EvapCondPumpElecPower + S12RuntimeFraction * S12EvapCondPumpElecPower;

				// Stage 1 runtime sets the crankcase heater power
				DXCoil( DXCoilNum ).CrankcaseHeaterPower = S1CrankcaseHeaterPower;

				DXCoilOutletTemp( DXCoilNum ) = DXCoil( DXCoilNum ).OutletAirTemp;
				DXCoilOutletHumRat( DXCoilNum ) = DXCoil( DXCoilNum ).OutletAirHumRat;

				//     calculate average full load outlet conditions for second stage operation
				DXCoilFullLoadOutAirTemp( DXCoilNum ) = ( 1.0 - S2PLR ) * S1FFullLoadOutAirTemp + S2PLR * DXCoilFullLoadOutAirTemp( DXCoilNum );
				DXCoilFullLoadOutAirHumRat( DXCoilNum ) = ( 1.0 - S2PLR ) * S1FullLoadOutAirHumRat + S2PLR * DXCoilFullLoadOutAirHumRat( DXCoilNum );

			} // End if stage 2 is operating

			//   set the part load ratio and heat reclaim capacity for use by desuperheater heating coils
			DXCoil( DXCoilNum ).PartLoadRatio = S1PLR;
			DXCoilPartLoadRatio( DXCoilNum ) = S1PLR;

			//   Calculation for heat reclaim needs to be corrected to use compressor power (not including condenser fan power)
			HeatReclaimDXCoil( DXCoilNum ).AvailCapacity = DXCoil( DXCoilNum ).TotalCoolingEnergyRate + DXCoil( DXCoilNum ).ElecCoolingPower;

			DXCoil( DXCoilNum ).CoolingCoilStg2RuntimeFrac = S12RuntimeFraction;

			//   Calculate basin heater power
			CalcBasinHeaterPowerForMultiModeDXCoil( DXCoilNum, DehumidMode );

		} else {
			ShowSevereError( "Error detected in DX Coil=" + CompName );
			ShowContinueError( "Invalid DX Coil Type=" + DXCoil( DXCoilNum ).DXCoilType );
			ShowFatalError( "Preceding condition causes termination." );

		}}

		// Update the unit outlet nodes
		UpdateDXCoil( DXCoilNum );

		// Report the result of the simulation
		ReportDXCoil( DXCoilNum );

	}

	void
	GetDXCoils()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   May 2000
		//       MODIFIED       Aug 2000, Don Shirey, Sept 2000, Feb/Oct 2001, Sept 2003, Jan/July 2004
		//                      Feb 2005, M. J. Witte, GARD Analytics, Inc., Add new coil type COIL:DX:MultiMode:CoolingEmpirical:
		//                      May 2005, Rich Raustad, FSEC, Added COIL:DX:HeatPumpWaterHeater
		//                      Jun 2007, L. Gu, FSEC, Added new coil type COIL:DX:MULTISPEED:COOLING and COIL:DX:MULTISPEED:HEATING
		//                      Apr 2010, Chandan Sharma, FSEC, added basin heater inputs
		//                      Jul 2015, RP Zhang, XF Pang, LBNL, Added new coil type for VRF-FluidTemperatureControl Model
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Obtains input data for DX coils and stores it in DX coil data structure

		// METHODOLOGY EMPLOYED:
		// Uses "Get" routines to read in data.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::GetObjectItemNum;
		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using InputProcessor::GetObjectDefMaxArgs;
		using CurveManager::GetCurveIndex;
		using CurveManager::GetCurveType;
		using CurveManager::CurveValue;
		using CurveManager::SetCurveOutputMinMaxValues;
		using BranchNodeConnections::TestCompSet;
		using NodeInputManager::GetOnlySingleNode;
		using DataSizing::AutoSize;
		using General::TrimSigDigits;
		using WaterManager::SetupTankDemandComponent;
		using WaterManager::SetupTankSupplyComponent;
		using OutAirNodeManager::CheckOutAirNodeNumber;
		using ScheduleManager::GetScheduleIndex;
		using DataGlobals::AnyEnergyManagementSystemInModel;
		using DataGlobals::emsCallFromComponentGetInput;
		using EMSManager::ManageEMS;
		using GlobalNames::VerifyUniqueCoilName;
		using DataHeatBalance::IntGainTypeOf_SecCoolingDXCoilSingleSpeed;
		using DataHeatBalance::IntGainTypeOf_SecHeatingDXCoilSingleSpeed;
		using DataHeatBalance::IntGainTypeOf_SecCoolingDXCoilTwoSpeed;
		using DataHeatBalance::IntGainTypeOf_SecCoolingDXCoilMultiSpeed;
		using DataHeatBalance::IntGainTypeOf_SecHeatingDXCoilMultiSpeed;
		using InputProcessor::FindItemInList;
		using DataHeatBalance::Zone;


		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetDXCoils: " ); // include trailing blank space

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int DXCoilIndex; // loop index
		int DXCoilNum; // current DX coil number
		int NumAlphas; // Number of alphas in input
		int NumNumbers; // Number of numeric items in input
		Array1D_string Alphas2; // Alpha input items for object
		Array1D< Real64 > Numbers2; // Numeric input items for object
		Array1D_string cAlphaFields2; // Alpha field names
		Array1D_string cNumericFields2; // Numeric field names
		Array1D_bool lAlphaBlanks2; // Logical array, alpha field input BLANK = .TRUE.
		Array1D_bool lNumericBlanks2; // Logical array, numeric field input BLANK = .TRUE.
		int NumAlphas2; // Number of alphas in input for performance object
		int NumNumbers2; // Number of numeric items in input for performance object
		int IOStatus; // Input status returned from GetObjectItem
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		static bool ErrorsFound( false ); // Set to true if errors in input, fatal at end of routine
		int DXHPWaterHeaterCoilNum; // Loop index for 1,NumDXHeatPumpWaterHeaterCoils
		int CapacityStageNum; // Loop index for 1,Number of capacity stages
		int DehumidModeNum; // Loop index for 1,Number of enhanced dehumidification modes
		int PerfModeNum; // Performance mode index
		int PerfObjectNum; // Item number for performance object
		int AlphaIndex; // Index for current alpha field
		std::string CurrentModuleObject; // Object type for getting and error messages
		std::string PerfObjectType; // Performance object type for getting and error messages
		std::string PerfObjectName; // Performance object name for getting and error messages
		Real64 InletAirTemp; // Used to pass proper inlet air temp to HPWH DX coil performance curves
		Real64 InletWaterTemp; // Used to pass proper inlet water temp to HPWH DX coil performance curves
		Real64 HeatCapFTemp; // Used to verify HPWH DX coil heating capacity (function of temp) performance curve
		Real64 HeatCOPFTemp; // Used to verify HPWH DX coil heating COP (function of temp) performance curve
		Real64 HeatCapFAirFlow; // Used to verify HPWH DX coil heating capacity (function of air flow) performance curve
		Real64 HeatCOPFAirFlow; // Used to verify HPWH DX coil heating COP (function of air flow) performance curve
		Real64 HeatCapFWaterFlow; // Used to verify HPWH DX coil heating capacity (function of water flow) performance curve
		Real64 HeatCOPFWaterFlow; // Used to verify HPWH DX coil heating COP (function of water flow) performance curve
		int I; // Index of speeds
		Real64 CurveVal; // Used to verify modifier curves equal 1 at rated conditions
		Array1D_string Alphas; // Alpha input items for object
		Array1D_string cAlphaFields; // Alpha field names
		Array1D_string cNumericFields; // Numeric field names
		Array1D< Real64 > Numbers; // Numeric input items for object
		Array1D_bool lAlphaBlanks; // Logical array, alpha field input BLANK = .TRUE.
		Array1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.
		static int MaxNumbers( 0 ); // Maximum number of numeric input fields
		static int MaxAlphas( 0 ); // Maximum number of alpha input fields
		static int TotalArgs( 0 ); // Total number of alpha and numeric arguments (max) for a
		//   certain object in the input file
		Real64 MinCurveVal; // used for testing PLF curve output
		Real64 MinCurvePLR; // used for testing PLF curve output
		Real64 MaxCurveVal; // used for testing PLF curve output
		Real64 MaxCurvePLR; // used for testing PLF curve output
		Real64 CurveInput; // index used for testing PLF curve output

		bool errFlag;

		// find number of each type of DX coil and calculate the total number
		NumDoe2DXCoils = GetNumObjectsFound( "Coil:Cooling:DX:SingleSpeed" );
		NumDXHeatingCoils = GetNumObjectsFound( "Coil:Heating:DX:SingleSpeed" );
		NumDXMulSpeedCoils = GetNumObjectsFound( "Coil:Cooling:DX:TwoSpeed" );
		NumDXMulModeCoils = GetNumObjectsFound( "Coil:Cooling:DX:TwoStageWithHumidityControlMode" );
		NumDXHeatPumpWaterHeaterPumpedCoils = GetNumObjectsFound( cAllCoilTypes( CoilDX_HeatPumpWaterHeaterPumped ) );
		NumDXHeatPumpWaterHeaterWrappedCoils = GetNumObjectsFound( cAllCoilTypes( CoilDX_HeatPumpWaterHeaterWrapped ) );
		NumDXMulSpeedCoolCoils = GetNumObjectsFound( "Coil:Cooling:DX:MultiSpeed" );
		NumDXMulSpeedHeatCoils = GetNumObjectsFound( "Coil:Heating:DX:MultiSpeed" );
		NumVRFCoolingCoils = GetNumObjectsFound( cAllCoilTypes( CoilVRF_Cooling ) );
		NumVRFHeatingCoils = GetNumObjectsFound( cAllCoilTypes( CoilVRF_Heating ) );
		NumVRFCoolingFluidTCtrlCoils = GetNumObjectsFound( cAllCoilTypes( CoilVRF_FluidTCtrl_Cooling ) );
		NumVRFHeatingFluidTCtrlCoils = GetNumObjectsFound( cAllCoilTypes( CoilVRF_FluidTCtrl_Heating ) );

		NumDXCoils = NumDoe2DXCoils + NumDXHeatingCoils + NumDXMulSpeedCoils + NumDXMulModeCoils + NumDXHeatPumpWaterHeaterPumpedCoils + NumDXHeatPumpWaterHeaterWrappedCoils + NumDXMulSpeedCoolCoils + NumDXMulSpeedHeatCoils + NumVRFCoolingCoils + NumVRFHeatingCoils + NumVRFCoolingFluidTCtrlCoils + NumVRFHeatingFluidTCtrlCoils;

		// Determine max number of alpha and numeric arguments for all objects being read, in order to allocate local arrays
		GetObjectDefMaxArgs( "Coil:Cooling:DX:SingleSpeed", TotalArgs, NumAlphas, NumNumbers );
		MaxNumbers = NumNumbers;
		MaxAlphas = NumAlphas;
		GetObjectDefMaxArgs( "Coil:Heating:DX:SingleSpeed", TotalArgs, NumAlphas, NumNumbers );
		MaxNumbers = max( MaxNumbers, NumNumbers );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		GetObjectDefMaxArgs( "Coil:Cooling:DX:TwoSpeed", TotalArgs, NumAlphas, NumNumbers );
		MaxNumbers = max( MaxNumbers, NumNumbers );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		GetObjectDefMaxArgs( "Coil:Cooling:DX:TwoStageWithHumidityControlMode", TotalArgs, NumAlphas, NumNumbers );
		MaxNumbers = max( MaxNumbers, NumNumbers );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		GetObjectDefMaxArgs( cAllCoilTypes( CoilDX_HeatPumpWaterHeaterPumped ), TotalArgs, NumAlphas, NumNumbers );
		MaxNumbers = max( MaxNumbers, NumNumbers );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		GetObjectDefMaxArgs( cAllCoilTypes( CoilDX_HeatPumpWaterHeaterWrapped ), TotalArgs, NumAlphas, NumNumbers );
		MaxNumbers = max( MaxNumbers, NumNumbers );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		GetObjectDefMaxArgs( "Coil:Cooling:DX:MultiSpeed", TotalArgs, NumAlphas, NumNumbers );
		MaxNumbers = max( MaxNumbers, NumNumbers );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		GetObjectDefMaxArgs( "Coil:Heating:DX:MultiSpeed", TotalArgs, NumAlphas, NumNumbers );
		MaxNumbers = max( MaxNumbers, NumNumbers );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		GetObjectDefMaxArgs( cAllCoilTypes( CoilVRF_Cooling ), TotalArgs, NumAlphas, NumNumbers );
		MaxNumbers = max( MaxNumbers, NumNumbers );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		GetObjectDefMaxArgs( cAllCoilTypes( CoilVRF_Heating ), TotalArgs, NumAlphas, NumNumbers );
		MaxNumbers = max( MaxNumbers, NumNumbers );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		GetObjectDefMaxArgs( cAllCoilTypes( CoilVRF_FluidTCtrl_Cooling ), TotalArgs, NumAlphas, NumNumbers );
		MaxNumbers = max( MaxNumbers, NumNumbers );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		GetObjectDefMaxArgs( cAllCoilTypes( CoilVRF_FluidTCtrl_Heating ), TotalArgs, NumAlphas, NumNumbers );
		MaxNumbers = max( MaxNumbers, NumNumbers );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		GetObjectDefMaxArgs( "CoilPerformance:DX:Cooling", TotalArgs, NumAlphas, NumNumbers );
		MaxNumbers = max( MaxNumbers, NumNumbers );
		MaxAlphas = max( MaxAlphas, NumAlphas );

		Alphas.allocate( MaxAlphas );
		cAlphaFields.allocate( MaxAlphas );
		cNumericFields.allocate( MaxNumbers );
		Numbers.dimension( MaxNumbers, 0.0 );
		lAlphaBlanks.dimension( MaxAlphas, true );
		lNumericBlanks.dimension( MaxNumbers, true );

		Alphas2.allocate( MaxAlphas );
		cAlphaFields2.allocate( MaxAlphas );
		cNumericFields2.allocate( MaxNumbers );
		Numbers2.dimension( MaxNumbers, 0.0 );
		lAlphaBlanks2.dimension( MaxAlphas, true );
		lNumericBlanks2.dimension( MaxNumbers, true );

		// allocate the data structure

		// Derived types
		DXCoil.allocate( NumDXCoils );
		DXCoilNumericFields.allocate( NumDXCoils );
		HeatReclaimDXCoil.allocate( NumDXCoils );
		CheckEquipName.dimension( NumDXCoils, true );

		// Module level variable arrays
		DXCoilOutletTemp.allocate( NumDXCoils );
		DXCoilOutletHumRat.allocate( NumDXCoils );
		DXCoilPartLoadRatio.allocate( NumDXCoils );
		DXCoilFanOpMode.allocate( NumDXCoils );
		DXCoilFullLoadOutAirTemp.allocate( NumDXCoils );
		DXCoilFullLoadOutAirHumRat.allocate( NumDXCoils );
		DXCoilTotalCooling.allocate( NumDXCoils );
		DXCoilTotalHeating.allocate( NumDXCoils );
		DXCoilCoolInletAirWBTemp.allocate( NumDXCoils );
		DXCoilHeatInletAirDBTemp.allocate( NumDXCoils );
		DXCoilHeatInletAirWBTemp.allocate( NumDXCoils );

		// initialize the module level arrays
		DXCoilOutletTemp = 0.0;
		DXCoilOutletHumRat = 0.0;
		DXCoilPartLoadRatio = 0.0;
		DXCoilFanOpMode = 0;
		DXCoilFullLoadOutAirTemp = 0.0;
		DXCoilFullLoadOutAirHumRat = 0.0;

		// initialize the coil counter
		DXCoilNum = 0;

		// Loop over the Doe2 DX Coils and get & load the data
		CurrentModuleObject = "Coil:Cooling:DX:SingleSpeed";
		for ( DXCoilIndex = 1; DXCoilIndex <= NumDoe2DXCoils; ++DXCoilIndex ) {

			GetObjectItem( CurrentModuleObject, DXCoilIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			++DXCoilNum;
			// allocate single performance mode for numeric field strings used for sizing routine
			DXCoilNumericFields ( DXCoilNum ).PerfMode.allocate ( 1 );
			DXCoilNumericFields( DXCoilNum ).PerfMode( 1 ).FieldNames.allocate ( MaxNumbers );
			DXCoilNumericFields ( DXCoilNum ).PerfMode ( 1 ).FieldNames = cNumericFields;
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), DXCoil, DXCoilNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			VerifyUniqueCoilName( CurrentModuleObject, Alphas( 1 ), errFlag, CurrentModuleObject + " Name" );
			if ( errFlag ) {
				ErrorsFound = true;
			}
			DXCoil( DXCoilNum ).Name = Alphas( 1 );
			// Initialize DataHeatBalance heat reclaim variable name for use by heat reclaim coils
			HeatReclaimDXCoil( DXCoilNum ).Name = DXCoil( DXCoilNum ).Name;
			HeatReclaimDXCoil( DXCoilNum ).SourceType = CurrentModuleObject;
			DXCoil( DXCoilNum ).DXCoilType = CurrentModuleObject;
			DXCoil( DXCoilNum ).DXCoilType_Num = CoilDX_CoolingSingleSpeed;
			DXCoil( DXCoilNum ).Schedule = Alphas( 2 );
			if ( lAlphaBlanks( 2 ) ) {
				DXCoil( DXCoilNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				DXCoil( DXCoilNum ).SchedPtr = GetScheduleIndex( Alphas( 2 ) ); // convert schedule name to pointer
				if ( DXCoil( DXCoilNum ).SchedPtr == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "..." + cAlphaFields( 2 ) + "=\"" + Alphas( 2 ) + "\"." );
					ErrorsFound = true;
				}
			}
			DXCoil( DXCoilNum ).RatedTotCap( 1 ) = Numbers( 1 );
			DXCoil( DXCoilNum ).RatedSHR( 1 ) = Numbers( 2 );
			DXCoil( DXCoilNum ).RatedCOP( 1 ) = Numbers( 3 );
			if ( DXCoil( DXCoilNum ).RatedCOP( 1 ) <= 0.0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 3 ) + " must be > 0.0, entered value=[" + TrimSigDigits( Numbers( 3 ), 2 ) + "]." );
				ErrorsFound = true;
			}

			DXCoil( DXCoilNum ).RatedAirVolFlowRate( 1 ) = Numbers( 4 );
			DXCoil( DXCoilNum ).FanPowerPerEvapAirFlowRate( 1 ) = Numbers( 5 );

			DXCoil( DXCoilNum ).AirInNode = GetOnlySingleNode( Alphas( 3 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

			DXCoil( DXCoilNum ).AirOutNode = GetOnlySingleNode( Alphas( 4 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			TestCompSet( CurrentModuleObject, Alphas( 1 ), Alphas( 3 ), Alphas( 4 ), "Air Nodes" );

			DXCoil( DXCoilNum ).CCapFTemp( 1 ) = GetCurveIndex( Alphas( 5 ) ); // convert curve name to number
			if ( DXCoil( DXCoilNum ).CCapFTemp( 1 ) == 0 ) {
				if ( lAlphaBlanks( 5 ) ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", missing" );
					ShowContinueError( "...required " + cAlphaFields( 5 ) + " is blank." );
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 5 ) + "=\"" + Alphas( 5 ) + "\"." );
				}
				ErrorsFound = true;
			} else {
				// Verify Curve Object, only legal type is BiQuadratic
				{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).CCapFTemp( 1 ) ) );

				if ( SELECT_CASE_var == "BIQUADRATIC" ) {
					DXCoil( DXCoilNum ).TotCapTempModFacCurveType( 1 ) = BiQuadratic;

				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...illegal " + cAlphaFields( 5 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).CCapFTemp( 1 ) ) );
					ShowContinueError( "Curve type must be Biquadratic." );
					ErrorsFound = true;
				}}
			}

			DXCoil( DXCoilNum ).CCapFFlow( 1 ) = GetCurveIndex( Alphas( 6 ) ); // convert curve name to number
			if ( DXCoil( DXCoilNum ).CCapFFlow( 1 ) == 0 ) {
				if ( lAlphaBlanks( 6 ) ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", missing" );
					ShowContinueError( "...required " + cAlphaFields( 6 ) + " is blank." );
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 6 ) + "=\"" + Alphas( 6 ) + "\"." );
				}
				ErrorsFound = true;
			} else {
				// Verify Curve Object, only legal type is Quadratic
				{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).CCapFFlow( 1 ) ) );
				if ( SELECT_CASE_var == "QUADRATIC" ) {

				} else if ( SELECT_CASE_var == "CUBIC" ) {

				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...illegal " + cAlphaFields( 6 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).CCapFFlow( 1 ) ) );
					ShowContinueError( "Curve type must be Quadratic or Cubic." );
					ErrorsFound = true;
				}}
			}

			DXCoil( DXCoilNum ).EIRFTemp( 1 ) = GetCurveIndex( Alphas( 7 ) ); // convert curve name to number
			if ( DXCoil( DXCoilNum ).EIRFTemp( 1 ) == 0 ) {
				if ( lAlphaBlanks( 7 ) ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", missing" );
					ShowContinueError( "...required " + cAlphaFields( 7 ) + " is blank." );
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 7 ) + "=\"" + Alphas( 7 ) + "\"." );
				}
				ErrorsFound = true;
			} else {
				// Verify Curve Object, only legal type is BiQuadratic
				{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).EIRFTemp( 1 ) ) );

				if ( SELECT_CASE_var == "BIQUADRATIC" ) {
					DXCoil( DXCoilNum ).EIRTempModFacCurveType( 1 ) = BiQuadratic;

				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...illegal " + cAlphaFields( 7 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).EIRFTemp( 1 ) ) );
					ShowContinueError( "Curve type must be Biquadratic." );
					ErrorsFound = true;
				}}
			}

			DXCoil( DXCoilNum ).EIRFFlow( 1 ) = GetCurveIndex( Alphas( 8 ) ); // convert curve name to number
			if ( DXCoil( DXCoilNum ).EIRFFlow( 1 ) == 0 ) {
				if ( lAlphaBlanks( 8 ) ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", missing" );
					ShowContinueError( "...required " + cAlphaFields( 8 ) + " is blank." );
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 8 ) + "=\"" + Alphas( 8 ) + "\"." );
				}
				ErrorsFound = true;
			} else {
				// Verify Curve Object, only legal type is Quadratic
				{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).EIRFFlow( 1 ) ) );

				if ( SELECT_CASE_var == "QUADRATIC" ) {

				} else if ( SELECT_CASE_var == "CUBIC" ) {

				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...illegal " + cAlphaFields( 8 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).EIRFFlow( 1 ) ) );
					ShowContinueError( "Curve type must be Quadratic or Cubic." );
					ErrorsFound = true;
				}}
			}

			DXCoil( DXCoilNum ).PLFFPLR( 1 ) = GetCurveIndex( Alphas( 9 ) ); // convert curve name to number
			if ( DXCoil( DXCoilNum ).PLFFPLR( 1 ) == 0 ) {
				if ( lAlphaBlanks( 9 ) ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", missing" );
					ShowContinueError( "...required " + cAlphaFields( 9 ) + " is blank." );
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 9 ) + "=\"" + Alphas( 9 ) + "\"." );
				}
				ErrorsFound = true;
			} else {
				// Verify Curve Object, only legal types are Quadratic or Cubic
				{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).PLFFPLR( 1 ) ) );

				if ( SELECT_CASE_var == "QUADRATIC" ) {

				} else if ( SELECT_CASE_var == "CUBIC" ) {

				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...illegal " + cAlphaFields( 9 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).PLFFPLR( 1 ) ) );
					ShowContinueError( "Curve type must be Quadratic or Cubic." );
					ErrorsFound = true;
				}}

				if ( ! ErrorsFound ) {
					//     Test PLF curve minimum and maximum. Cap if less than 0.7 or greater than 1.0.
					MinCurveVal = 999.0;
					MaxCurveVal = -999.0;
					CurveInput = 0.0;
					while ( CurveInput <= 1.0 ) {
						CurveVal = CurveValue( DXCoil( DXCoilNum ).PLFFPLR( 1 ), CurveInput );
						if ( CurveVal < MinCurveVal ) {
							MinCurveVal = CurveVal;
							MinCurvePLR = CurveInput;
						}
						if ( CurveVal > MaxCurveVal ) {
							MaxCurveVal = CurveVal;
							MaxCurvePLR = CurveInput;
						}
						CurveInput += 0.01;
					}
					if ( MinCurveVal < 0.7 ) {
						ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "..." + cAlphaFields( 9 ) + "=\"" + Alphas( 9 ) + "\" has out of range values." );
						ShowContinueError( "...Curve minimum must be >= 0.7, curve min at PLR = " + TrimSigDigits( MinCurvePLR, 2 ) + " is " + TrimSigDigits( MinCurveVal, 3 ) );
						ShowContinueError( "...Setting curve minimum to 0.7 and simulation continues." );
						SetCurveOutputMinMaxValues( DXCoil( DXCoilNum ).PLFFPLR( 1 ), ErrorsFound, 0.7, _ );
					}

					if ( MaxCurveVal > 1.0 ) {
						ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "..." + cAlphaFields( 9 ) + " = " + Alphas( 9 ) + " has out of range value." );
						ShowContinueError( "...Curve maximum must be <= 1.0, curve max at PLR = " + TrimSigDigits( MaxCurvePLR, 2 ) + " is " + TrimSigDigits( MaxCurveVal, 3 ) );
						ShowContinueError( "...Setting curve maximum to 1.0 and simulation continues." );
						SetCurveOutputMinMaxValues( DXCoil( DXCoilNum ).PLFFPLR( 1 ), ErrorsFound, _, 1.0 );
					}

				}
			}

			DXCoil( DXCoilNum ).Twet_Rated( 1 ) = Numbers( 6 );
			DXCoil( DXCoilNum ).Gamma_Rated( 1 ) = Numbers( 7 );
			DXCoil( DXCoilNum ).MaxONOFFCyclesperHour( 1 ) = Numbers( 8 );
			DXCoil( DXCoilNum ).LatentCapacityTimeConstant( 1 ) = Numbers( 9 );

			// Numbers (6) through (9) must all be greater than zero to use the latent capacity degradation model
			if ( ( Numbers( 6 ) > 0.0 || Numbers( 7 ) > 0.0 || Numbers( 8 ) > 0.0 || Numbers( 9 ) > 0.0 ) && ( Numbers( 6 ) <= 0.0 || Numbers( 7 ) <= 0.0 || Numbers( 8 ) <= 0.0 || Numbers( 9 ) <= 0.0 ) ) {
				ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\":" );
				ShowContinueError( "...At least one of the four input parameters for the latent capacity degradation model" );
				ShowContinueError( "...is set to zero. Therefore, the latent degradation model will not be used for this simulation." );
			}

			// outdoor condenser node
			if ( lAlphaBlanks( 10 ) ) {
				DXCoil( DXCoilNum ).CondenserInletNodeNum( 1 ) = 0;
			} else {
				DXCoil( DXCoilNum ).CondenserInletNodeNum( 1 ) = GetOnlySingleNode( Alphas( 10 ), ErrorsFound, CurrentModuleObject, DXCoil( DXCoilNum ).Name, NodeType_Air, NodeConnectionType_OutsideAirReference, 1, ObjectIsNotParent );

				if ( ! CheckOutAirNodeNumber( DXCoil( DXCoilNum ).CondenserInletNodeNum( 1 ) ) ) {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", may be invalid" );
					ShowContinueError( cAlphaFields( 10 ) + "=\"" + Alphas( 10 ) + "\", node does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node." );
					ShowContinueError( "This node needs to be included in an air system or the coil model will not be valid, and the simulation continues" );
				}
			}

			if ( ( SameString( Alphas( 11 ), "AirCooled" ) ) || lAlphaBlanks( 11 ) ) {
				DXCoil( DXCoilNum ).CondenserType( 1 ) = AirCooled;
			} else if ( SameString( Alphas( 11 ), "EvaporativelyCooled" ) ) {
				DXCoil( DXCoilNum ).CondenserType( 1 ) = EvapCooled;
				DXCoil( DXCoilNum ).ReportEvapCondVars = true;
			} else {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cAlphaFields( 11 ) + "=\"" + Alphas( 11 ) + "\":" );
				ShowContinueError( "...must be AirCooled or EvaporativelyCooled." );
				ErrorsFound = true;
			}

			DXCoil( DXCoilNum ).EvapCondEffect( 1 ) = Numbers( 10 );
			if ( DXCoil( DXCoilNum ).EvapCondEffect( 1 ) < 0.0 || DXCoil( DXCoilNum ).EvapCondEffect( 1 ) > 1.0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 10 ) + " cannot be < 0.0 or > 1.0." );
				ShowContinueError( "...entered value=[" + TrimSigDigits( Numbers( 10 ), 2 ) + "]." );
				ErrorsFound = true;
			}

			DXCoil( DXCoilNum ).EvapCondAirFlow( 1 ) = Numbers( 11 );
			if ( DXCoil( DXCoilNum ).EvapCondAirFlow( 1 ) < 0.0 && DXCoil( DXCoilNum ).EvapCondAirFlow( 1 ) != AutoSize ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 11 ) + " cannot be < 0.0." );
				ShowContinueError( "...entered value=[" + TrimSigDigits( Numbers( 11 ), 2 ) + "]." );
				ErrorsFound = true;
			}

			DXCoil( DXCoilNum ).EvapCondPumpElecNomPower( 1 ) = Numbers( 12 );
			if ( DXCoil( DXCoilNum ).EvapCondPumpElecNomPower( 1 ) < 0.0 && DXCoil( DXCoilNum ).EvapCondPumpElecNomPower( 1 ) != AutoSize ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 12 ) + " cannot be < 0.0." );
				ShowContinueError( "...entered value=[" + TrimSigDigits( Numbers( 12 ), 2 ) + "]." );
				ErrorsFound = true;
			}

			//Set crankcase heater capacity
			DXCoil( DXCoilNum ).CrankcaseHeaterCapacity = Numbers( 13 );
			if ( DXCoil( DXCoilNum ).CrankcaseHeaterCapacity < 0.0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 13 ) + " cannot be < 0.0." );
				ShowContinueError( "...entered value=[" + TrimSigDigits( Numbers( 13 ), 2 ) + "]." );
				ErrorsFound = true;
			}

			//Set crankcase heater cutout temperature
			DXCoil( DXCoilNum ).MaxOATCrankcaseHeater = Numbers( 14 );

			if ( DXCoil( DXCoilNum ).RatedCOP( 1 ) > 0.0 ) {
				DXCoil( DXCoilNum ).RatedEIR( 1 ) = 1.0 / DXCoil( DXCoilNum ).RatedCOP( 1 );
			}

			// Get Water System tank connections
			//  A12, \field Name of Water Storage Tank for Supply
			DXCoil( DXCoilNum ).EvapWaterSupplyName = Alphas( 12 );
			if ( lAlphaBlanks( 12 ) ) {
				DXCoil( DXCoilNum ).EvapWaterSupplyMode = WaterSupplyFromMains;
			} else {
				DXCoil( DXCoilNum ).EvapWaterSupplyMode = WaterSupplyFromTank;
				SetupTankDemandComponent( DXCoil( DXCoilNum ).Name, CurrentModuleObject, DXCoil( DXCoilNum ).EvapWaterSupplyName, ErrorsFound, DXCoil( DXCoilNum ).EvapWaterSupTankID, DXCoil( DXCoilNum ).EvapWaterTankDemandARRID );
			}

			//A13; \field Name of Water Storage Tank for Condensate Collection
			DXCoil( DXCoilNum ).CondensateCollectName = Alphas( 13 );
			if ( lAlphaBlanks( 13 ) ) {
				DXCoil( DXCoilNum ).CondensateCollectMode = CondensateDiscarded;
			} else {
				DXCoil( DXCoilNum ).CondensateCollectMode = CondensateToTank;
				SetupTankSupplyComponent( DXCoil( DXCoilNum ).Name, CurrentModuleObject, DXCoil( DXCoilNum ).CondensateCollectName, ErrorsFound, DXCoil( DXCoilNum ).CondensateTankID, DXCoil( DXCoilNum ).CondensateTankSupplyARRID );
			}

			//   Basin heater power as a function of temperature must be greater than or equal to 0
			DXCoil( DXCoilNum ).BasinHeaterPowerFTempDiff = Numbers( 15 );
			if ( Numbers( 15 ) < 0.0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 15 ) + " must be >= 0.0." );
				ShowContinueError( "...entered value=[" + TrimSigDigits( Numbers( 15 ), 2 ) + "]." );
				ErrorsFound = true;
			}

			DXCoil( DXCoilNum ).BasinHeaterSetPointTemp = Numbers( 16 );
			if ( DXCoil( DXCoilNum ).BasinHeaterPowerFTempDiff > 0.0 ) {
				if ( NumNumbers < 16 ) {
					DXCoil( DXCoilNum ).BasinHeaterSetPointTemp = 2.0;
				}
				if ( DXCoil( DXCoilNum ).BasinHeaterSetPointTemp < 2.0 ) {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", freeze possible" );
					ShowContinueError( "..." + cNumericFields( 16 ) + " is < 2 {C}. Freezing could occur." );
					ShowContinueError( "...entered value=[" + TrimSigDigits( Numbers( 16 ), 2 ) + "]." );
				}
			}

			if ( ! lAlphaBlanks( 14 ) ) {
				DXCoil( DXCoilNum ).BasinHeaterSchedulePtr = GetScheduleIndex( Alphas( 14 ) );
				if ( DXCoil( DXCoilNum ).BasinHeaterSchedulePtr == 0 ) {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 14 ) + "=\"" + Alphas( 14 ) + "\"." );
					ShowContinueError( "Basin heater will be available to operate throughout the simulation." );
				}
			}

			if ( ! lAlphaBlanks( 15 ) && NumAlphas > 14 ) {
				DXCoil( DXCoilNum ).SHRFTemp( 1 ) = GetCurveIndex( Alphas( 15 ) ); // convert curve name to number
				if ( DXCoil( DXCoilNum ).SHRFTemp( 1 ) == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 15 ) + "=\"" + Alphas( 15 ) + "\"." );
				} else {
					// Verify Curve Object, only legal type is BiQuadratic
					{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).SHRFTemp( 1 ) ) );
					if ( SELECT_CASE_var == "BIQUADRATIC" ) {
						DXCoil( DXCoilNum ).SHRFTempCurveType( 1 ) = BiQuadratic;
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( 15 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).SHRFTemp( 1 ) ) );
						ShowContinueError( "Curve type must be Biquadratic." );
						ErrorsFound = true;
					}}
				}
			}

			if ( ! lAlphaBlanks( 16 ) && NumAlphas > 15 ) {
				DXCoil( DXCoilNum ).SHRFFlow( 1 ) = GetCurveIndex( Alphas( 16 ) ); // convert curve name to number
				if ( DXCoil( DXCoilNum ).SHRFTemp( 1 ) == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 16 ) + "=\"" + Alphas( 16 ) + "\"." );
				} else {
					// Verify Curve Object, only legal type is BiQuadratic
					{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).SHRFFlow( 1 ) ) );
					if ( ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) ) {

					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( 16 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).SHRFFlow( 1 ) ) );
						ShowContinueError( "Curve type must be quadratic or cubic." );
						ErrorsFound = true;
					}}
				}
			}

			if ( DXCoil( DXCoilNum ).SHRFTemp( 1 ) > 0 && DXCoil( DXCoilNum ).SHRFFlow( 1 ) > 0 ) {
				DXCoil( DXCoilNum ).UserSHRCurveExists = true;
			}
			// get User Input flag for ASHRAE Standard 127 Standard Ratings Reporting
			if ( lAlphaBlanks( 17 ) ) {
				DXCoil( DXCoilNum ).ASHRAE127StdRprt = false;
			} else {
				if ( Alphas( 17 ) == "YES" || Alphas( 17 ) == "Yes" ) {
					DXCoil( DXCoilNum ).ASHRAE127StdRprt = true;
				} else {
					DXCoil( DXCoilNum ).ASHRAE127StdRprt = false;
				}
			}
			// A18; \field Zone Name for Condenser Placement
			if ( !lAlphaBlanks( 18 ) && NumAlphas > 17 ) {
				DXCoil( DXCoilNum ).SecZonePtr = FindItemInList( Alphas( 18 ), Zone );
				if ( DXCoil( DXCoilNum ).SecZonePtr > 0 ) {
					SetupZoneInternalGain( DXCoil( DXCoilNum ).SecZonePtr, "Coil:Cooling:DX:SingleSpeed", DXCoil( DXCoilNum ).Name, IntGainTypeOf_SecCoolingDXCoilSingleSpeed, DXCoil( DXCoilNum ).SecCoilSensibleHeatGainRate );
					DXCoil( DXCoilNum ).IsSecondaryDXCoilInZone = true;
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 18 ) + "=\"" + Alphas( 18 ) + "\"." );
				}
			}

		} // end of the Doe2 DX coil loop

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in getting " + CurrentModuleObject + " input. Preceding condition(s) causes termination." );
		}

		// Loop over the Multimode DX Coils and get & load the data
		CurrentModuleObject = "Coil:Cooling:DX:TwoStageWithHumidityControlMode";
		for ( DXCoilIndex = 1; DXCoilIndex <= NumDXMulModeCoils; ++DXCoilIndex ) {

			GetObjectItem( CurrentModuleObject, DXCoilIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			++DXCoilNum;
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), DXCoil, DXCoilNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			VerifyUniqueCoilName( CurrentModuleObject, Alphas( 1 ), errFlag, CurrentModuleObject + " Name" );
			if ( errFlag ) {
				ErrorsFound = true;
			}
			DXCoil( DXCoilNum ).Name = Alphas( 1 );
			// Initialize DataHeatBalance heat reclaim variable name for use by heat reclaim coils
			HeatReclaimDXCoil( DXCoilNum ).Name = DXCoil( DXCoilNum ).Name;
			HeatReclaimDXCoil( DXCoilNum ).SourceType = CurrentModuleObject;
			DXCoil( DXCoilNum ).DXCoilType = CurrentModuleObject;
			DXCoil( DXCoilNum ).DXCoilType_Num = CoilDX_CoolingTwoStageWHumControl;
			DXCoil( DXCoilNum ).Schedule = Alphas( 2 );
			if ( lAlphaBlanks( 2 ) ) {
				DXCoil( DXCoilNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				DXCoil( DXCoilNum ).SchedPtr = GetScheduleIndex( Alphas( 2 ) ); // convert schedule name to pointer
				if ( DXCoil( DXCoilNum ).SchedPtr == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "..." + cAlphaFields( 2 ) + "=\"" + Alphas( 2 ) + "\"." );
					ErrorsFound = true;
				}
			}

			DXCoil( DXCoilNum ).AirInNode = GetOnlySingleNode( Alphas( 3 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

			DXCoil( DXCoilNum ).AirOutNode = GetOnlySingleNode( Alphas( 4 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			TestCompSet( CurrentModuleObject, Alphas( 1 ), Alphas( 3 ), Alphas( 4 ), "Air Nodes" );

			//Set crankcase heater capacity
			DXCoil( DXCoilNum ).CrankcaseHeaterCapacity = Numbers( 1 );
			if ( DXCoil( DXCoilNum ).CrankcaseHeaterCapacity < 0.0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 1 ) + " must be >= 0.0, entered value=[" + TrimSigDigits( Numbers( 1 ), 2 ) + "]." );
				ErrorsFound = true;
			}

			//Set crankcase heater cutout temperature
			DXCoil( DXCoilNum ).MaxOATCrankcaseHeater = Numbers( 2 );

			//  Number of capacity stages
			DXCoil( DXCoilNum ).NumCapacityStages = Numbers( 3 );
			//  Check if requested number of capacity stages exceeds limits
			if ( ( DXCoil( DXCoilNum ).NumCapacityStages > MaxCapacityStages ) || ( DXCoil( DXCoilNum ).NumCapacityStages < 1 ) ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "...illegal " + cNumericFields( 3 ) + " = " + TrimSigDigits( DXCoil( DXCoilNum ).NumCapacityStages ) );
				ShowContinueError( "...Valid range is 1 to " + TrimSigDigits( MaxCapacityStages ) );
				ErrorsFound = true;
			}

			//  Number of enhanced dehumidification modes
			DXCoil( DXCoilNum ).NumDehumidModes = Numbers( 4 );
			//  Check if requested number of enhanced dehumidification modes exceeds limits
			if ( ( DXCoil( DXCoilNum ).NumDehumidModes > MaxDehumidModes ) || ( DXCoil( DXCoilNum ).NumDehumidModes < 0 ) ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "...illegal " + cNumericFields( 4 ) + " = " + TrimSigDigits( DXCoil( DXCoilNum ).NumDehumidModes ) );
				ShowContinueError( "...Valid range is 0 to " + TrimSigDigits( MaxDehumidModes ) );
				ErrorsFound = true;
			}

			//  Set starting alpha index for coil performance inputs
			AlphaIndex = 5;
			// allocate performance modes for numeric field strings used for sizing routine
			DXCoilNumericFields ( DXCoilNum ).PerfMode.allocate ( DXCoil( DXCoilNum ).NumDehumidModes * 2 + DXCoil( DXCoilNum ).NumCapacityStages * 2 ); // not sure this math is correct, ask MW

			//  Loop through capacity stages and dehumidification modes
			for ( DehumidModeNum = 0; DehumidModeNum <= DXCoil( DXCoilNum ).NumDehumidModes; ++DehumidModeNum ) {
				for ( CapacityStageNum = 1; CapacityStageNum <= DXCoil( DXCoilNum ).NumCapacityStages; ++CapacityStageNum ) {
					//  Check if sufficient number of fields entered
					if ( ( AlphaIndex + 1 ) > NumAlphas ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...not enough remaining fields for specified Number of Operating Modes." );
						ShowContinueError( "...Need additional Coil Performance Object Type and Coil Performance Object Name fields." );
						ErrorsFound = true;
					} else {
						PerfObjectType = Alphas( AlphaIndex );
						PerfObjectName = Alphas( AlphaIndex + 1 );
						PerfModeNum = DehumidModeNum * 2 + CapacityStageNum;
						DXCoil( DXCoilNum ).CoilPerformanceType( PerfModeNum ) = PerfObjectType;
						if ( SameString( PerfObjectType, "CoilPerformance:DX:Cooling" ) ) {
							DXCoil( DXCoilNum ).CoilPerformanceType_Num( PerfModeNum ) = CoilPerfDX_CoolBypassEmpirical;
						} else {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
							ShowContinueError( "...illegal " + cAlphaFields( AlphaIndex ) + "=\"" + PerfObjectType + "\"." );
							ShowContinueError( "Must be \"CoilPerformance:DX:Cooling\"." );
							ErrorsFound = true;
						}
						DXCoil( DXCoilNum ).CoilPerformanceName( PerfModeNum ) = PerfObjectName;
						// Get for CoilPerformance object
						PerfObjectNum = GetObjectItemNum( PerfObjectType, PerfObjectName );
						if ( PerfObjectNum > 0 ) {

							GetObjectItem( PerfObjectType, PerfObjectNum, Alphas2, NumAlphas2, Numbers2, NumNumbers2, IOStatus, lNumericBlanks2, lAlphaBlanks2, cAlphaFields2, cNumericFields2 );

							// allocate performance mode numeric field strings used for sizing routine
							DXCoilNumericFields( DXCoilNum ).PerfMode( PerfModeNum ).FieldNames.allocate ( NumNumbers2 ); // use MaxNumbers here??
							DXCoilNumericFields ( DXCoilNum ).PerfMode ( PerfModeNum ).FieldNames = cNumericFields2;

							DXCoil( DXCoilNum ).RatedTotCap( PerfModeNum ) = Numbers2( 1 );
							DXCoil( DXCoilNum ).RatedSHR( PerfModeNum ) = Numbers2( 2 );
							DXCoil( DXCoilNum ).RatedCOP( PerfModeNum ) = Numbers2( 3 );
							// Rated flow is immediately adjusted for bypass fraction if not autosized
							DXCoil( DXCoilNum ).BypassedFlowFrac( PerfModeNum ) = Numbers2( 5 );
							DXCoil( DXCoilNum ).RatedAirVolFlowRate( PerfModeNum ) = Numbers2( 4 );
							if ( DXCoil( DXCoilNum ).RatedAirVolFlowRate( PerfModeNum ) != AutoSize ) {
								DXCoil( DXCoilNum ).RatedAirVolFlowRate( PerfModeNum ) *= ( 1.0 - DXCoil( DXCoilNum ).BypassedFlowFrac( PerfModeNum ) );
							}

							DXCoil( DXCoilNum ).CCapFTemp( PerfModeNum ) = GetCurveIndex( Alphas2( 2 ) ); // convert curve name to number
							if ( DXCoil( DXCoilNum ).CCapFTemp( PerfModeNum ) == 0 ) {
								if ( lAlphaBlanks2( 2 ) ) {
									ShowSevereError( RoutineName + PerfObjectType + "=\"" + PerfObjectName + "\", invalid" );
									ShowContinueError( "...required " + cAlphaFields2( 2 ) + " is blank." );
								} else {
									ShowSevereError( RoutineName + PerfObjectType + "=\"" + PerfObjectName + "\", invalid" );
									ShowContinueError( "...not found " + cAlphaFields2( 2 ) + "=\"" + Alphas2( 2 ) + "\"." );
								}
								ErrorsFound = true;
							} else {
								// Verify Curve Object, only legal type is BiQuadratic
								{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).CCapFTemp( PerfModeNum ) ) );

								if ( SELECT_CASE_var == "BIQUADRATIC" ) {
									DXCoil( DXCoilNum ).TotCapTempModFacCurveType( PerfModeNum ) = BiQuadratic;

								} else {
									ShowSevereError( RoutineName + PerfObjectType + "=\"" + PerfObjectName + "\", invalid" );
									ShowContinueError( "...illegal " + cAlphaFields2( 2 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).CCapFTemp( PerfModeNum ) ) );
									ShowContinueError( "Curve type must be BiQuadratic." );
									ErrorsFound = true;
								}}
							}

							DXCoil( DXCoilNum ).CCapFFlow( PerfModeNum ) = GetCurveIndex( Alphas2( 3 ) ); // convert curve name to number
							if ( DXCoil( DXCoilNum ).CCapFFlow( PerfModeNum ) == 0 ) {
								if ( lAlphaBlanks2( 3 ) ) {
									ShowSevereError( RoutineName + PerfObjectType + "=\"" + PerfObjectName + "\", invalid" );
									ShowContinueError( "...required " + cAlphaFields2( 3 ) + " is blank." );
								} else {
									ShowSevereError( RoutineName + PerfObjectType + "=\"" + PerfObjectName + "\", invalid" );
									ShowContinueError( "...not found " + cAlphaFields2( 3 ) + "=\"" + Alphas2( 3 ) + "\"." );
								}
								ErrorsFound = true;
							} else {
								// Verify Curve Object, only legal type is Quadratic
								{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).CCapFFlow( PerfModeNum ) ) );

								if ( SELECT_CASE_var == "QUADRATIC" ) {

								} else if ( SELECT_CASE_var == "CUBIC" ) {

								} else {
									ShowSevereError( RoutineName + PerfObjectType + "=\"" + PerfObjectName + "\", invalid" );
									ShowContinueError( "...illegal " + cAlphaFields2( 3 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).CCapFFlow( PerfModeNum ) ) );
									ShowContinueError( "Curve type must be Quadratic or Cubic." );
									ErrorsFound = true;
								}}
							}

							DXCoil( DXCoilNum ).EIRFTemp( PerfModeNum ) = GetCurveIndex( Alphas2( 4 ) ); // convert curve name to number
							if ( DXCoil( DXCoilNum ).EIRFTemp( PerfModeNum ) == 0 ) {
								if ( lAlphaBlanks2( 4 ) ) {
									ShowSevereError( RoutineName + PerfObjectType + "=\"" + PerfObjectName + "\", invalid" );
									ShowContinueError( "...required " + cAlphaFields2( 4 ) + " is blank." );
								} else {
									ShowSevereError( RoutineName + PerfObjectType + "=\"" + PerfObjectName + "\", invalid" );
									ShowContinueError( "...not found " + cAlphaFields2( 4 ) + "=\"" + Alphas2( 4 ) + "\"." );
								}
								ErrorsFound = true;
							} else {
								// Verify Curve Object, only legal type is BiQuadratic
								{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).EIRFTemp( PerfModeNum ) ) );

								if ( SELECT_CASE_var == "BIQUADRATIC" ) {
									DXCoil( DXCoilNum ).EIRTempModFacCurveType( PerfModeNum ) = BiQuadratic;

								} else {
									ShowSevereError( RoutineName + PerfObjectType + "=\"" + PerfObjectName + "\", invalid" );
									ShowContinueError( "...illegal " + cAlphaFields2( 4 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).EIRFTemp( PerfModeNum ) ) );
									ShowContinueError( "Curve type must be BiQuadratic." );
									ErrorsFound = true;
								}}
							}

							DXCoil( DXCoilNum ).EIRFFlow( PerfModeNum ) = GetCurveIndex( Alphas2( 5 ) ); // convert curve name to number
							if ( DXCoil( DXCoilNum ).EIRFFlow( PerfModeNum ) == 0 ) {
								if ( lAlphaBlanks2( 5 ) ) {
									ShowSevereError( RoutineName + PerfObjectType + "=\"" + PerfObjectName + "\", invalid" );
									ShowContinueError( "...required " + cAlphaFields2( 5 ) + " is blank." );
								} else {
									ShowSevereError( RoutineName + PerfObjectType + "=\"" + PerfObjectName + "\", invalid" );
									ShowContinueError( "...not found " + cAlphaFields2( 5 ) + "=\"" + Alphas2( 5 ) + "\"." );
								}
								ErrorsFound = true;
							} else {
								// Verify Curve Object, only legal type is Quadratic
								{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).EIRFFlow( PerfModeNum ) ) );

								if ( SELECT_CASE_var == "QUADRATIC" ) {

								} else if ( SELECT_CASE_var == "CUBIC" ) {

								} else {
									ShowSevereError( RoutineName + PerfObjectType + "=\"" + PerfObjectName + "\", invalid" );
									ShowContinueError( "...illegal " + cAlphaFields2( 5 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).EIRFFlow( PerfModeNum ) ) );
									ShowContinueError( "Curve type must be Quadratic or Cubic." );
									ErrorsFound = true;
								}}
							}

							DXCoil( DXCoilNum ).PLFFPLR( PerfModeNum ) = GetCurveIndex( Alphas2( 6 ) ); // convert curve name to number
							if ( DXCoil( DXCoilNum ).PLFFPLR( PerfModeNum ) == 0 ) {
								if ( lAlphaBlanks2( 6 ) ) {
									ShowSevereError( RoutineName + PerfObjectType + "=\"" + PerfObjectName + "\", invalid" );
									ShowContinueError( "...required " + cAlphaFields2( 6 ) + " is blank." );
								} else {
									ShowSevereError( RoutineName + PerfObjectType + "=\"" + PerfObjectName + "\", invalid" );
									ShowContinueError( "...not found " + cAlphaFields2( 6 ) + "=\"" + Alphas2( 6 ) + "\"." );
								}
								ErrorsFound = true;
							} else {
								// Verify Curve Object, only legal types are Quadratic or Cubic
								{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).PLFFPLR( PerfModeNum ) ) );

								if ( SELECT_CASE_var == "QUADRATIC" ) {

								} else if ( SELECT_CASE_var == "CUBIC" ) {

								} else {
									ShowSevereError( RoutineName + PerfObjectType + "=\"" + PerfObjectName + "\", invalid" );
									ShowContinueError( "...illegal " + cAlphaFields2( 6 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).PLFFPLR( PerfModeNum ) ) );
									ShowContinueError( "Curve type must be Quadratic or Cubic." );
									ErrorsFound = true;
								}}

								if ( ! ErrorsFound ) {
									//             Test PLF curve minimum and maximum. Cap if less than 0.7 or greater than 1.0.
									MinCurveVal = 999.0;
									MaxCurveVal = -999.0;
									CurveInput = 0.0;
									while ( CurveInput <= 1.0 ) {
										CurveVal = CurveValue( DXCoil( DXCoilNum ).PLFFPLR( PerfModeNum ), CurveInput );
										if ( CurveVal < MinCurveVal ) {
											MinCurveVal = CurveVal;
											MinCurvePLR = CurveInput;
										}
										if ( CurveVal > MaxCurveVal ) {
											MaxCurveVal = CurveVal;
											MaxCurvePLR = CurveInput;
										}
										CurveInput += 0.01;
									}
									if ( MinCurveVal < 0.7 ) {
										ShowWarningError( RoutineName + PerfObjectType + "=\"" + PerfObjectName + "\", invalid" );
										ShowContinueError( "..." + cAlphaFields2( 6 ) + " = " + Alphas2( 6 ) + " has out of range value." );
										ShowContinueError( "...Curve minimum must be >= 0.7, curve min at PLR = " + TrimSigDigits( MinCurvePLR, 2 ) + " is " + TrimSigDigits( MinCurveVal, 3 ) );
										ShowContinueError( "...Setting curve minimum to 0.7 and simulation continues." );
										SetCurveOutputMinMaxValues( DXCoil( DXCoilNum ).PLFFPLR( PerfModeNum ), ErrorsFound, 0.7, _ );
									}

									if ( MaxCurveVal > 1.0 ) {
										ShowWarningError( RoutineName + PerfObjectType + "=\"" + PerfObjectName + "\", invalid" );
										ShowContinueError( "..." + cAlphaFields2( 6 ) + " = " + Alphas2( 6 ) + " has out of range value." );
										ShowContinueError( "...Curve maximum must be <= 1.0, curve max at PLR = " + TrimSigDigits( MaxCurvePLR, 2 ) + " is " + TrimSigDigits( MaxCurveVal, 3 ) );
										ShowContinueError( "...Setting curve maximum to 1.0 and simulation continues." );
										SetCurveOutputMinMaxValues( DXCoil( DXCoilNum ).PLFFPLR( PerfModeNum ), ErrorsFound, _, 1.0 );
									}

								}
							}

							DXCoil( DXCoilNum ).Twet_Rated( PerfModeNum ) = Numbers2( 6 );
							DXCoil( DXCoilNum ).Gamma_Rated( PerfModeNum ) = Numbers2( 7 );
							DXCoil( DXCoilNum ).MaxONOFFCyclesperHour( PerfModeNum ) = Numbers2( 8 );
							DXCoil( DXCoilNum ).LatentCapacityTimeConstant( PerfModeNum ) = Numbers2( 9 );
							// Numbers2 (6) through (9) must all be greater than zero to use the latent capacity degradation model
							if ( ( Numbers2( 6 ) > 0.0 || Numbers2( 7 ) > 0.0 || Numbers2( 8 ) > 0.0 || Numbers2( 9 ) > 0.0 ) && ( Numbers2( 6 ) <= 0.0 || Numbers2( 7 ) <= 0.0 || Numbers2( 8 ) <= 0.0 || Numbers2( 9 ) <= 0.0 ) ) {
								ShowWarningError( RoutineName + PerfObjectType + "=\"" + PerfObjectName + "\":" );
								ShowContinueError( "...At least one of the four input parameters for the latent capacity degradation model" );
								ShowContinueError( "...is set to zero. Therefore, the latent degradation model will not be used for this simulation." );
							}

							// outdoor condenser node
							if ( lAlphaBlanks2( 7 ) ) {
								DXCoil( DXCoilNum ).CondenserInletNodeNum( PerfModeNum ) = 0;
							} else {
								DXCoil( DXCoilNum ).CondenserInletNodeNum( PerfModeNum ) = GetOnlySingleNode( Alphas2( 7 ), ErrorsFound, PerfObjectType, PerfObjectName, NodeType_Air, NodeConnectionType_OutsideAirReference, 1, ObjectIsNotParent );
								if ( ! CheckOutAirNodeNumber( DXCoil( DXCoilNum ).CondenserInletNodeNum( PerfModeNum ) ) ) {
									ShowWarningError( RoutineName + PerfObjectType + "=\"" + PerfObjectName + "\":" );
									ShowContinueError( "may not be valid " + cAlphaFields2( 7 ) + "=\"" + Alphas2( 7 ) + "\"." );
									ShowContinueError( "node does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node." );
									ShowContinueError( "This node needs to be included in an air system or the coil model will not be valid, and the simulation continues" );
								}
							}
							if ( ( SameString( Alphas2( 8 ), "AirCooled" ) ) || lAlphaBlanks2( 8 ) ) {
								DXCoil( DXCoilNum ).CondenserType( PerfModeNum ) = AirCooled;
							} else if ( SameString( Alphas2( 8 ), "EvaporativelyCooled" ) ) {
								DXCoil( DXCoilNum ).CondenserType( PerfModeNum ) = EvapCooled;
								DXCoil( DXCoilNum ).ReportEvapCondVars = true;
							} else {
								ShowSevereError( RoutineName + PerfObjectType + "=\"" + PerfObjectName + "\", invalid" );
								ShowContinueError( "..." + cAlphaFields2( 8 ) + "=\"" + Alphas2( 8 ) + "\":" );
								ShowContinueError( "...must be AirCooled or EvaporativelyCooled." );
								ErrorsFound = true;
							}

							DXCoil( DXCoilNum ).EvapCondEffect( PerfModeNum ) = Numbers2( 10 );
							if ( DXCoil( DXCoilNum ).EvapCondEffect( PerfModeNum ) < 0.0 || DXCoil( DXCoilNum ).EvapCondEffect( PerfModeNum ) > 1.0 ) {
								ShowSevereError( RoutineName + PerfObjectType + "=\"" + PerfObjectName + "\", invalid" );
								ShowContinueError( "..." + cNumericFields2( 10 ) + " cannot be < 0.0 or > 1.0." );
								ShowContinueError( "...entered value=[" + TrimSigDigits( Numbers2( 10 ), 2 ) + "]." );
								ErrorsFound = true;
							}

							DXCoil( DXCoilNum ).EvapCondAirFlow( PerfModeNum ) = Numbers2( 11 );
							if ( DXCoil( DXCoilNum ).EvapCondAirFlow( PerfModeNum ) < 0.0 && DXCoil( DXCoilNum ).EvapCondAirFlow( PerfModeNum ) != AutoSize ) {
								ShowSevereError( RoutineName + PerfObjectType + "=\"" + PerfObjectName + "\", invalid" );
								ShowContinueError( "..." + cNumericFields2( 11 ) + " cannot be < 0.0." );
								ShowContinueError( "...entered value=[" + TrimSigDigits( Numbers2( 11 ), 2 ) + "]." );
								ErrorsFound = true;
							}

							DXCoil( DXCoilNum ).EvapCondPumpElecNomPower( PerfModeNum ) = Numbers2( 12 );
							if ( DXCoil( DXCoilNum ).EvapCondPumpElecNomPower( PerfModeNum ) < 0.0 && DXCoil( DXCoilNum ).EvapCondAirFlow( PerfModeNum ) != AutoSize ) {
								ShowSevereError( RoutineName + PerfObjectType + "=\"" + PerfObjectName + "\", invalid" );
								ShowContinueError( "..." + cNumericFields2( 12 ) + " cannot be less than zero." );
								ShowContinueError( "...entered value=[" + TrimSigDigits( Numbers2( 12 ), 2 ) + "]." );
								ErrorsFound = true;
							}

							DXCoil( DXCoilNum ).RatedEIR( PerfModeNum ) = 1.0 / DXCoil( DXCoilNum ).RatedCOP( PerfModeNum );

							// read in user specified SHR modifer curves
							if ( ! lAlphaBlanks2( 9 ) && NumAlphas2 > 8 ) {
								DXCoil( DXCoilNum ).SHRFTemp( PerfModeNum ) = GetCurveIndex( Alphas2( 9 ) ); // convert curve name to number
								if ( DXCoil( DXCoilNum ).SHRFTemp( PerfModeNum ) == 0 ) {
									ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
									ShowContinueError( "...not found " + cAlphaFields2( 9 ) + "=\"" + Alphas2( 9 ) + "\"." );
								} else {
									// Verify Curve Object, only legal type is BiQuadratic
									{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).SHRFTemp( PerfModeNum ) ) );
									if ( SELECT_CASE_var == "BIQUADRATIC" ) {
										DXCoil( DXCoilNum ).SHRFTempCurveType( PerfModeNum ) = BiQuadratic;
									} else {
										ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
										ShowContinueError( "...illegal " + cAlphaFields2( 9 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).SHRFTemp( PerfModeNum ) ) );
										ShowContinueError( "Curve type must be Biquadratic." );
										ErrorsFound = true;
									}}
								}
							}

							if ( ! lAlphaBlanks2( 10 ) && NumAlphas2 > 9 ) {
								DXCoil( DXCoilNum ).SHRFFlow( PerfModeNum ) = GetCurveIndex( Alphas2( 10 ) ); // convert curve name to number
								if ( DXCoil( DXCoilNum ).SHRFTemp( PerfModeNum ) == 0 ) {
									ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
									ShowContinueError( "...not found " + cAlphaFields2( 10 ) + "=\"" + Alphas2( 10 ) + "\"." );
								} else {
									// Verify Curve Object, only legal type is BiQuadratic
									{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).SHRFFlow( PerfModeNum ) ) );
									if ( ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) ) {

									} else {
										ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
										ShowContinueError( "...illegal " + cAlphaFields2( 10 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).SHRFFlow( PerfModeNum ) ) );
										ShowContinueError( "Curve type must be quadratic or cubic." );
										ErrorsFound = true;
									}}
								}
							}
							if ( DXCoil( DXCoilNum ).SHRFTemp( PerfModeNum ) > 0 && DXCoil( DXCoilNum ).SHRFFlow( PerfModeNum ) > 0 ) {
								DXCoil( DXCoilNum ).UserSHRCurveExists = true;
							} else {
								DXCoil( DXCoilNum ).UserSHRCurveExists = false;
							}

						} else { // invalid performance object
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
							ShowContinueError( "... not found " + PerfObjectType + "=\"" + PerfObjectName + "\"." );
							ErrorsFound = true;
						} // end of valid performance object check
						AlphaIndex += 2;
					} // end of sufficient number of fields entered check
				} // End of multimode DX capacity stages loop
				// Warn if inputs entered for unused capacity stages
				for ( CapacityStageNum = ( DXCoil( DXCoilNum ).NumCapacityStages + 1 ); CapacityStageNum <= MaxCapacityStages; ++CapacityStageNum ) {
					if ( ( AlphaIndex <= NumAlphas ) && ( ( ! Alphas( AlphaIndex ).empty() ) || ( ! Alphas( AlphaIndex + 1 ).empty() ) ) ) {
						ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\":" );
						ShowContinueError( "...Capacity Stage " + TrimSigDigits( CapacityStageNum ) + " not active. Therefore," + cAlphaFields( AlphaIndex ) );
						ShowContinueError( "... and " + cAlphaFields( AlphaIndex + 1 ) + " fields will be ignored." );
					}
					AlphaIndex += 2;
				} // End of unused capacity stages loop
			} // End of multimode DX dehumidification modes loop

			//  ! Warn if excess fields entered
			//  IF (NumAlphas .GE. AlphaIndex .and. ANY(Alphas(AlphaIndex:) /= BlankString)) THEN
			//    CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(DXCoil(DXCoilNum)%Name)//'":')
			//    CALL ShowContinueError('...too many remaining fields for specified Capacity Stages and Dehumidification Modes.')
			//    CALL ShowContinueError('...Excess Coil Performance Object Type and Coil Performance Object Name fields will be ignored.')
			//  ENDIF

			// Get Water System tank connections
			//  A13, \field Name of Water Storage Tank for Supply
			DXCoil( DXCoilNum ).EvapWaterSupplyName = Alphas( 13 );
			if ( lAlphaBlanks( 13 ) ) {
				DXCoil( DXCoilNum ).EvapWaterSupplyMode = WaterSupplyFromMains;
			} else {
				DXCoil( DXCoilNum ).EvapWaterSupplyMode = WaterSupplyFromTank;
				SetupTankDemandComponent( DXCoil( DXCoilNum ).Name, CurrentModuleObject, DXCoil( DXCoilNum ).EvapWaterSupplyName, ErrorsFound, DXCoil( DXCoilNum ).EvapWaterSupTankID, DXCoil( DXCoilNum ).EvapWaterTankDemandARRID );
			}

			//A14; \field Name of Water Storage Tank for Condensate Collection
			DXCoil( DXCoilNum ).CondensateCollectName = Alphas( 14 );
			if ( lAlphaBlanks( 14 ) ) {
				DXCoil( DXCoilNum ).CondensateCollectMode = CondensateDiscarded;
			} else {
				DXCoil( DXCoilNum ).CondensateCollectMode = CondensateToTank;
				SetupTankSupplyComponent( DXCoil( DXCoilNum ).Name, CurrentModuleObject, DXCoil( DXCoilNum ).CondensateCollectName, ErrorsFound, DXCoil( DXCoilNum ).CondensateTankID, DXCoil( DXCoilNum ).CondensateTankSupplyARRID );
			}

			//Basin heater power as a function of temperature must be greater than or equal to 0
			DXCoil( DXCoilNum ).BasinHeaterPowerFTempDiff = Numbers( 5 );
			if ( Numbers( 5 ) < 0.0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 5 ) + " must be >= 0." );
				ShowContinueError( "...entered value=[" + TrimSigDigits( Numbers( 5 ), 2 ) + "]." );
				ErrorsFound = true;
			}

			DXCoil( DXCoilNum ).BasinHeaterSetPointTemp = Numbers( 6 );
			if ( DXCoil( DXCoilNum ).BasinHeaterPowerFTempDiff > 0.0 ) {
				if ( NumNumbers < 6 ) {
					DXCoil( DXCoilNum ).BasinHeaterSetPointTemp = 2.0;
				}
				if ( DXCoil( DXCoilNum ).BasinHeaterSetPointTemp < 2.0 ) {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", freeze possible" );
					ShowContinueError( "..." + cNumericFields( 6 ) + " is < 2 {C}. Freezing could occur." );
					ShowContinueError( "...entered value=[" + TrimSigDigits( Numbers( 6 ), 2 ) + "]." );
				}
			}

			if ( ! lAlphaBlanks( 15 ) ) {
				DXCoil( DXCoilNum ).BasinHeaterSchedulePtr = GetScheduleIndex( Alphas( 15 ) );
				if ( DXCoil( DXCoilNum ).BasinHeaterSchedulePtr == 0 ) {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 15 ) + "=\"" + Alphas( 15 ) + "\"." );
					ShowContinueError( "Basin heater will be available to operate throughout the simulation." );
				}
			}

		} // end of the Multimode DX coil loop

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in getting " + CurrentModuleObject + " input.  Preceding condition(s) causes termination." );
		}

		//************* Read Heat Pump (DX Heating Coil) Input **********
		CurrentModuleObject = "Coil:Heating:DX:SingleSpeed";
		for ( DXCoilIndex = 1; DXCoilIndex <= NumDXHeatingCoils; ++DXCoilIndex ) {

			++DXCoilNum;

			GetObjectItem( CurrentModuleObject, DXCoilIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			// allocate single performance mode for numeric field strings used for sizing routine
			DXCoilNumericFields ( DXCoilNum ).PerfMode.allocate ( 1 );
			DXCoilNumericFields( DXCoilNum ).PerfMode( 1 ).FieldNames.allocate ( MaxNumbers );
			DXCoilNumericFields ( DXCoilNum ).PerfMode ( 1 ).FieldNames = cNumericFields;

			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), DXCoil, DXCoilNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			VerifyUniqueCoilName( CurrentModuleObject, Alphas( 1 ), errFlag, CurrentModuleObject + " Name" );
			if ( errFlag ) {
				ErrorsFound = true;
			}
			DXCoil( DXCoilNum ).Name = Alphas( 1 );
			DXCoil( DXCoilNum ).DXCoilType = CurrentModuleObject;
			DXCoil( DXCoilNum ).DXCoilType_Num = CoilDX_HeatingEmpirical;
			DXCoil( DXCoilNum ).Schedule = Alphas( 2 );
			if ( lAlphaBlanks( 2 ) ) {
				DXCoil( DXCoilNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				DXCoil( DXCoilNum ).SchedPtr = GetScheduleIndex( Alphas( 2 ) ); // convert schedule name to pointer
				if ( DXCoil( DXCoilNum ).SchedPtr == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "..." + cAlphaFields( 2 ) + "=\"" + Alphas( 2 ) + "\"." );
					ErrorsFound = true;
				}
			}

			DXCoil( DXCoilNum ).AirInNode = GetOnlySingleNode( Alphas( 3 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

			DXCoil( DXCoilNum ).AirOutNode = GetOnlySingleNode( Alphas( 4 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			TestCompSet( CurrentModuleObject, Alphas( 1 ), Alphas( 3 ), Alphas( 4 ), "Air Nodes" );

			DXCoil( DXCoilNum ).CCapFTemp( 1 ) = GetCurveIndex( Alphas( 5 ) ); // convert curve name to number
			if ( DXCoil( DXCoilNum ).CCapFTemp( 1 ) == 0 ) {
				if ( lAlphaBlanks( 5 ) ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", missing" );
					ShowContinueError( "...required " + cAlphaFields( 5 ) + " is blank." );
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 5 ) + "=\"" + Alphas( 5 ) + "\"." );
				}
				ErrorsFound = true;
			} else {
				// only legal types are Quadratic, BiQuadratic and Cubic
				{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).CCapFTemp( 1 ) ) );

				if ( SELECT_CASE_var == "QUADRATIC" ) {
					DXCoil( DXCoilNum ).TotCapTempModFacCurveType( 1 ) = Quadratic;

				} else if ( SELECT_CASE_var == "BIQUADRATIC" ) {
					DXCoil( DXCoilNum ).TotCapTempModFacCurveType( 1 ) = BiQuadratic;

				} else if ( SELECT_CASE_var == "CUBIC" ) {
					DXCoil( DXCoilNum ).TotCapTempModFacCurveType( 1 ) = Cubic;

				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...illegal " + cAlphaFields( 5 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).CCapFTemp( 1 ) ) );
					ShowContinueError( "Curve type must be Biquadratic, Quadratic or Cubic." );
					ErrorsFound = true;
				}}
			}

			DXCoil( DXCoilNum ).CCapFFlow( 1 ) = GetCurveIndex( Alphas( 6 ) ); // convert curve name to number
			if ( DXCoil( DXCoilNum ).CCapFFlow( 1 ) == 0 ) {
				if ( lAlphaBlanks( 6 ) ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", missing" );
					ShowContinueError( "...required " + cAlphaFields( 6 ) + " is blank." );
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 6 ) + "=\"" + Alphas( 6 ) + "\"." );
				}
				ErrorsFound = true;
			} else {
				// Verify Curve Object, only legal type is Quadratic
				{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).CCapFFlow( 1 ) ) );

				if ( SELECT_CASE_var == "QUADRATIC" ) {

				} else if ( SELECT_CASE_var == "CUBIC" ) {

				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...illegal " + cAlphaFields( 6 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).CCapFFlow( 1 ) ) );
					ShowContinueError( "Curve type must be Quadratic or Cubic." );
					ErrorsFound = true;
				}}
			}

			DXCoil( DXCoilNum ).EIRFTemp( 1 ) = GetCurveIndex( Alphas( 7 ) ); // convert curve name to number
			if ( DXCoil( DXCoilNum ).EIRFTemp( 1 ) == 0 ) {
				if ( lAlphaBlanks( 7 ) ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", missing" );
					ShowContinueError( "...required " + cAlphaFields( 7 ) + " is blank." );
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 7 ) + "=\"" + Alphas( 7 ) + "\"." );
				}
				ErrorsFound = true;
			} else {
				// only legal types are Quadratic, BiQuadratic and Cubic
				{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).EIRFTemp( 1 ) ) );

				if ( SELECT_CASE_var == "QUADRATIC" ) {
					DXCoil( DXCoilNum ).EIRTempModFacCurveType( 1 ) = Quadratic;

				} else if ( SELECT_CASE_var == "BIQUADRATIC" ) {
					DXCoil( DXCoilNum ).EIRTempModFacCurveType( 1 ) = BiQuadratic;

				} else if ( SELECT_CASE_var == "CUBIC" ) {
					DXCoil( DXCoilNum ).EIRTempModFacCurveType( 1 ) = Cubic;

				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...illegal " + cAlphaFields( 7 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).EIRFTemp( 1 ) ) );
					ShowContinueError( "Curve type must be Biquadratic, Quadratic or Cubic." );
					ErrorsFound = true;
				}}
			}

			DXCoil( DXCoilNum ).EIRFFlow( 1 ) = GetCurveIndex( Alphas( 8 ) ); // convert curve name to number
			if ( DXCoil( DXCoilNum ).EIRFFlow( 1 ) == 0 ) {
				if ( lAlphaBlanks( 8 ) ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", missing" );
					ShowContinueError( "...required " + cAlphaFields( 8 ) + " is blank." );
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 8 ) + "=\"" + Alphas( 8 ) + "\"." );
				}
				ErrorsFound = true;
			} else {
				// Verify Curve Object, only legal type is Quadratic or Cubic
				{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).EIRFFlow( 1 ) ) );

				if ( SELECT_CASE_var == "QUADRATIC" ) {

				} else if ( SELECT_CASE_var == "CUBIC" ) {

				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...illegal " + cAlphaFields( 8 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).EIRFFlow( 1 ) ) );
					ShowContinueError( "Curve type must be Quadratic or Cubic." );
					ErrorsFound = true;
				}}
			}

			DXCoil( DXCoilNum ).PLFFPLR( 1 ) = GetCurveIndex( Alphas( 9 ) ); // convert curve name to number
			if ( DXCoil( DXCoilNum ).PLFFPLR( 1 ) == 0 ) {
				if ( lAlphaBlanks( 9 ) ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", missing" );
					ShowContinueError( "...required " + cAlphaFields( 9 ) + " is blank." );
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 9 ) + "=\"" + Alphas( 9 ) + "\"." );
				}
				ErrorsFound = true;
			} else {
				// Verify Curve Object, only legal types are Quadratic or Cubic
				{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).PLFFPLR( 1 ) ) );

				if ( SELECT_CASE_var == "QUADRATIC" ) {

				} else if ( SELECT_CASE_var == "CUBIC" ) {

				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...illegal " + cAlphaFields( 9 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).PLFFPLR( 1 ) ) );
					ShowContinueError( "Curve type must be Quadratic or Cubic." );
					ErrorsFound = true;
				}}

				if ( ! ErrorsFound ) {
					//     Test PLF curve minimum and maximum. Cap if less than 0.7 or greater than 1.0.
					MinCurveVal = 999.0;
					MaxCurveVal = -999.0;
					CurveInput = 0.0;
					while ( CurveInput <= 1.0 ) {
						CurveVal = CurveValue( DXCoil( DXCoilNum ).PLFFPLR( 1 ), CurveInput );
						if ( CurveVal < MinCurveVal ) {
							MinCurveVal = CurveVal;
							MinCurvePLR = CurveInput;
						}
						if ( CurveVal > MaxCurveVal ) {
							MaxCurveVal = CurveVal;
							MaxCurvePLR = CurveInput;
						}
						CurveInput += 0.01;
					}
					if ( MinCurveVal < 0.7 ) {
						ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "..." + cAlphaFields( 9 ) + " = " + Alphas( 9 ) + " has out of range value." );
						ShowContinueError( "...Curve minimum must be >= 0.7, curve min at PLR = " + TrimSigDigits( MinCurvePLR, 2 ) + " is " + TrimSigDigits( MinCurveVal, 3 ) );
						ShowContinueError( "...Setting curve minimum to 0.7 and simulation continues." );
						SetCurveOutputMinMaxValues( DXCoil( DXCoilNum ).PLFFPLR( 1 ), ErrorsFound, 0.7, _ );
					}

					if ( MaxCurveVal > 1.0 ) {
						ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "..." + cAlphaFields( 9 ) + " = " + Alphas( 9 ) + " has out of range value." );
						ShowContinueError( "...Curve maximum must be <= 1.0, curve max at PLR = " + TrimSigDigits( MaxCurvePLR, 2 ) + " is " + TrimSigDigits( MaxCurveVal, 3 ) );
						ShowContinueError( "...Setting curve maximum to 1.0 and simulation continues." );
						SetCurveOutputMinMaxValues( DXCoil( DXCoilNum ).PLFFPLR( 1 ), ErrorsFound, _, 1.0 );
					}

				}

			}

			// Only required for reverse cycle heat pumps
			DXCoil( DXCoilNum ).DefrostEIRFT = GetCurveIndex( Alphas( 10 ) ); // convert curve name to number
			if ( SameString( Alphas( 11 ), "ReverseCycle" ) && SameString( Alphas( 12 ), "OnDemand" ) ) {
				if ( DXCoil( DXCoilNum ).DefrostEIRFT == 0 ) {
					if ( lAlphaBlanks( 10 ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", missing" );
						ShowContinueError( "...required " + cAlphaFields( 10 ) + " is blank." );
						ShowContinueError( "...field is required because " + cAlphaFields( 11 ) + " is \"ReverseCycle\" and " + cAlphaFields( 12 ) + " is \"OnDemand\"." );
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...not found " + cAlphaFields( 10 ) + "=\"" + Alphas( 10 ) + "\"." );
					}
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal type is BiQuadratic
					{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).DefrostEIRFT ) );

					if ( SELECT_CASE_var == "BIQUADRATIC" ) {

					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( 10 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).DefrostEIRFT ) );
						ShowContinueError( "Curve type must be BiQuadratic." );
						ErrorsFound = true;
					}}
				}
			}

			if ( SameString( Alphas( 11 ), "ReverseCycle" ) ) DXCoil( DXCoilNum ).DefrostStrategy = ReverseCycle;
			if ( SameString( Alphas( 11 ), "Resistive" ) ) DXCoil( DXCoilNum ).DefrostStrategy = Resistive;
			if ( DXCoil( DXCoilNum ).DefrostStrategy == 0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "...illegal " + cAlphaFields( 11 ) + "=\"" + Alphas( 11 ) + "\"." );
				ShowContinueError( "...valid values for this field are ReverseCycle or Resistive." );
				ErrorsFound = true;
			}

			if ( SameString( Alphas( 12 ), "Timed" ) ) DXCoil( DXCoilNum ).DefrostControl = Timed;
			if ( SameString( Alphas( 12 ), "OnDemand" ) ) DXCoil( DXCoilNum ).DefrostControl = OnDemand;
			if ( DXCoil( DXCoilNum ).DefrostControl == 0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "...illegal " + cAlphaFields( 12 ) + "=\"" + Alphas( 12 ) + "\"." );
				ShowContinueError( "...valid values for this field are Timed or OnDemand." );
				ErrorsFound = true;
			}

			DXCoil( DXCoilNum ).RatedSHR( 1 ) = 1.0;
			DXCoil( DXCoilNum ).RatedTotCap( 1 ) = Numbers( 1 );
			DXCoil( DXCoilNum ).RatedCOP( 1 ) = Numbers( 2 );
			DXCoil( DXCoilNum ).RatedAirVolFlowRate( 1 ) = Numbers( 3 );
			DXCoil( DXCoilNum ).FanPowerPerEvapAirFlowRate( 1 ) = Numbers( 4 );

			//Set minimum OAT for heat pump compressor operation
			DXCoil( DXCoilNum ).MinOATCompressor = Numbers( 5 );

			DXCoil( DXCoilNum ).OATempCompressorOn = Numbers( 6 );

			if ( lNumericBlanks( 6 ) || lNumericBlanks( 5 ) ) {
				DXCoil( DXCoilNum ).OATempCompressorOnOffBlank = true;
			} else {
				DXCoil( DXCoilNum ).OATempCompressorOnOffBlank = false;
			}

			if ( DXCoil( DXCoilNum ).OATempCompressorOn < DXCoil( DXCoilNum ).MinOATCompressor ) DXCoil( DXCoilNum ).OATempCompressorOn = DXCoil( DXCoilNum ).MinOATCompressor;

			//Set maximum outdoor temp for defrost to occur
			DXCoil( DXCoilNum ).MaxOATDefrost = Numbers( 7 );

			//Set crankcase heater capacity
			DXCoil( DXCoilNum ).CrankcaseHeaterCapacity = Numbers( 8 );
			if ( DXCoil( DXCoilNum ).CrankcaseHeaterCapacity < 0.0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 8 ) + " cannot be < 0.0." );
				ShowContinueError( "...entered value=[" + TrimSigDigits( Numbers( 8 ), 2 ) + "]." );
				ErrorsFound = true;
			}

			//Set crankcase heater cutout temperature
			DXCoil( DXCoilNum ).MaxOATCrankcaseHeater = Numbers( 9 );

			//Set defrost time period
			DXCoil( DXCoilNum ).DefrostTime = Numbers( 10 );
			if ( DXCoil( DXCoilNum ).DefrostTime == 0.0 && DXCoil( DXCoilNum ).DefrostControl == Timed ) {
				ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", " );
				ShowContinueError( "..." + cNumericFields( 10 ) + " = 0.0 for defrost control = TIMED." );
			}

			//Set defrost capacity (for resistive defrost)
			DXCoil( DXCoilNum ).DefrostCapacity = Numbers( 11 );
			if ( DXCoil( DXCoilNum ).DefrostCapacity == 0.0 && DXCoil( DXCoilNum ).DefrostStrategy == Resistive ) {
				ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", " );
				ShowContinueError( "..." + cNumericFields( 11 ) + " = 0.0 for defrost strategy = RESISTIVE." );
			}

			//Set Region number for calculating HSPF
			DXCoil( DXCoilNum ).RegionNum = Numbers( 12 );

			if ( lNumericBlanks( 12 ) ) {
				DXCoil( DXCoilNum ).RegionNum = 4;
			}

			DXCoil( DXCoilNum ).RatedEIR( 1 ) = 1.0 / DXCoil( DXCoilNum ).RatedCOP( 1 );

			//A13 is optional evaporator node name
			if ( lAlphaBlanks( 13 ) ) {
				DXCoil( DXCoilNum ).CondenserInletNodeNum( 1 ) = 0;
			} else {
				DXCoil( DXCoilNum ).CondenserInletNodeNum( 1 ) = GetOnlySingleNode( Alphas( 13 ), ErrorsFound, CurrentModuleObject, DXCoil( DXCoilNum ).Name, NodeType_Air, NodeConnectionType_OutsideAirReference, 1, ObjectIsNotParent );
				// warn if not an outdoor node, but allow
				if ( ! CheckOutAirNodeNumber( DXCoil( DXCoilNum ).CondenserInletNodeNum( 1 ) ) ) {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", may be invalid" );
					ShowContinueError( cAlphaFields( 13 ) + "=\"" + Alphas( 13 ) + "\", node does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node." );
					ShowContinueError( "This node needs to be included in an air system or the coil model will not be valid, and the simulation continues" );
				}
			}

			//A14, \field Zone Name for Evaporator Placement
			if ( !lAlphaBlanks( 14 ) && NumAlphas > 13 ) {
				DXCoil( DXCoilNum ).SecZonePtr = FindItemInList( Alphas( 14 ), Zone );
				if ( DXCoil( DXCoilNum ).SecZonePtr > 0 ) {
					SetupZoneInternalGain( DXCoil( DXCoilNum ).SecZonePtr, "Coil:Heating:DX:SingleSpeed", DXCoil( DXCoilNum ).Name, IntGainTypeOf_SecHeatingDXCoilSingleSpeed, DXCoil( DXCoilNum ).SecCoilSensibleHeatRemovalRate, _, _, DXCoil( DXCoilNum ).SecCoilLatentHeatRemovalRate );
					DXCoil( DXCoilNum ).IsSecondaryDXCoilInZone = true;
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 14 ) + "=\"" + Alphas( 14 ) + "\"." );
				}
			}
			if ( DXCoil( DXCoilNum ).SecZonePtr > 0 ) {
				//N13, \field Secondary Coil Air Flow Rate
				if ( !lNumericBlanks( 13 ) ) {
					DXCoil( DXCoilNum ).SecCoilAirFlow = Numbers( 13 );
				}
				//N14, \field Secondary Coil Fan Flow Scaling Factor
				if ( !lNumericBlanks( 14 ) ) {
					DXCoil( DXCoilNum ).SecCoilAirFlowScalingFactor = Numbers( 14 );
				}
				//N15, \field Nominal Sensible Heat Ratio of Secondary Coil
				if ( !lNumericBlanks( 15 ) ) {
					DXCoil( DXCoilNum ).SecCoilRatedSHR = Numbers( 15 );
				} else {
					DXCoil( DXCoilNum ).SecCoilRatedSHR = 1.0;
				}
				//A15, \field Sensible Heat Ratio Modifier Function of Temperature Curve Name
				if ( !lAlphaBlanks( 15 ) ) {
					DXCoil( DXCoilNum ).SecCoilSHRFT = GetCurveIndex( Alphas( 15 ) );
					if ( DXCoil( DXCoilNum ).SecCoilSHRFT == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...not found " + cAlphaFields( 15 ) + "=\"" + Alphas( 15 ) + "\"." );
					}
				}
				//A16; \field Sensible Heat Ratio Function of Flow Fraction Curve Name
				if ( !lAlphaBlanks( 16 ) ) {
					DXCoil( DXCoilNum ).SecCoilSHRFF = GetCurveIndex( Alphas( 16 ) );
					if ( DXCoil( DXCoilNum ).SecCoilSHRFF == 0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...not found " + cAlphaFields( 16 ) + "=\"" + Alphas( 16 ) + "\"." );
					}
				}
			}

		} // end of the DX heating coil loop

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in getting " + CurrentModuleObject + " input. Preceding condition(s) causes termination." );
		}

		CurrentModuleObject = "Coil:Cooling:DX:TwoSpeed";
		for ( DXCoilIndex = 1; DXCoilIndex <= NumDXMulSpeedCoils; ++DXCoilIndex ) {

			++DXCoilNum;

			GetObjectItem( CurrentModuleObject, DXCoilIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			// allocate single performance mode for numeric field strings used for sizing routine
			DXCoilNumericFields ( DXCoilNum ).PerfMode.allocate ( 1 );
			DXCoilNumericFields( DXCoilNum ).PerfMode( 1 ).FieldNames.allocate ( MaxNumbers );
			DXCoilNumericFields ( DXCoilNum ).PerfMode ( 1 ).FieldNames = cNumericFields;
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), DXCoil, DXCoilNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			VerifyUniqueCoilName( CurrentModuleObject, Alphas( 1 ), errFlag, CurrentModuleObject + " Name" );
			if ( errFlag ) {
				ErrorsFound = true;
			}
			DXCoil( DXCoilNum ).Name = Alphas( 1 );
			// Initialize DataHeatBalance heat reclaim variable name for use by heat reclaim coils
			HeatReclaimDXCoil( DXCoilNum ).Name = DXCoil( DXCoilNum ).Name;
			HeatReclaimDXCoil( DXCoilNum ).SourceType = CurrentModuleObject;
			DXCoil( DXCoilNum ).DXCoilType = CurrentModuleObject;
			DXCoil( DXCoilNum ).DXCoilType_Num = CoilDX_CoolingTwoSpeed;
			DXCoil( DXCoilNum ).Schedule = Alphas( 2 );
			if ( lAlphaBlanks( 2 ) ) {
				DXCoil( DXCoilNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				DXCoil( DXCoilNum ).SchedPtr = GetScheduleIndex( Alphas( 2 ) ); // convert schedule name to pointer
				if ( DXCoil( DXCoilNum ).SchedPtr == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "..." + cAlphaFields( 2 ) + "=\"" + Alphas( 2 ) + "\"." );
					ErrorsFound = true;
				}
			}
			DXCoil( DXCoilNum ).RatedTotCap( 1 ) = Numbers( 1 );
			DXCoil( DXCoilNum ).RatedSHR( 1 ) = Numbers( 2 );
			DXCoil( DXCoilNum ).RatedCOP( 1 ) = Numbers( 3 );
			DXCoil( DXCoilNum ).RatedAirVolFlowRate( 1 ) = Numbers( 4 );
			if ( ! lNumericBlanks( 5 ) ) {
				DXCoil( DXCoilNum ).InternalStaticPressureDrop = Numbers( 5 );
				DXCoil( DXCoilNum ).RateWithInternalStaticAndFanObject = true;
			} else {
				DXCoil( DXCoilNum ).InternalStaticPressureDrop = -999.0;
				DXCoil( DXCoilNum ).RateWithInternalStaticAndFanObject = false;
			}

			DXCoil( DXCoilNum ).AirInNode = GetOnlySingleNode( Alphas( 3 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

			DXCoil( DXCoilNum ).AirOutNode = GetOnlySingleNode( Alphas( 4 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			TestCompSet( CurrentModuleObject, Alphas( 1 ), Alphas( 3 ), Alphas( 4 ), "Air Nodes" );

			DXCoil( DXCoilNum ).CCapFTemp( 1 ) = GetCurveIndex( Alphas( 5 ) ); // convert curve name to number
			if ( DXCoil( DXCoilNum ).CCapFTemp( 1 ) == 0 ) {
				if ( lAlphaBlanks( 5 ) ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", missing" );
					ShowContinueError( "...required " + cAlphaFields( 5 ) + " is blank." );
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 5 ) + "=\"" + Alphas( 5 ) + "\"." );
				}
				ErrorsFound = true;
			} else {
				// Verify Curve Object, only legal type is BiQuadratic
				{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).CCapFTemp( 1 ) ) );

				if ( SELECT_CASE_var == "BIQUADRATIC" ) {
					DXCoil( DXCoilNum ).TotCapTempModFacCurveType( 1 ) = BiQuadratic;

				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...illegal " + cAlphaFields( 5 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).CCapFTemp( 1 ) ) );
					ShowContinueError( "Curve type must be BiQuadratic." );
					ErrorsFound = true;
				}}
			}

			DXCoil( DXCoilNum ).CCapFFlow( 1 ) = GetCurveIndex( Alphas( 6 ) ); // convert curve name to number
			if ( DXCoil( DXCoilNum ).CCapFFlow( 1 ) == 0 ) {
				if ( lAlphaBlanks( 6 ) ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", missing" );
					ShowContinueError( "...required " + cAlphaFields( 6 ) + " is blank." );
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 6 ) + "=\"" + Alphas( 6 ) + "\"." );
				}
				ErrorsFound = true;
			} else {
				// Verify Curve Object, only legal type is Quadratic
				{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).CCapFFlow( 1 ) ) );

				if ( SELECT_CASE_var == "QUADRATIC" ) {

				} else if ( SELECT_CASE_var == "CUBIC" ) {

				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...illegal " + cAlphaFields( 6 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).CCapFFlow( 1 ) ) );
					ShowContinueError( "Curve type must be Quadratic or Cubic." );
					ErrorsFound = true;
				}}
			}

			DXCoil( DXCoilNum ).EIRFTemp( 1 ) = GetCurveIndex( Alphas( 7 ) ); // convert curve name to number
			if ( DXCoil( DXCoilNum ).EIRFTemp( 1 ) == 0 ) {
				if ( lAlphaBlanks( 7 ) ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", missing" );
					ShowContinueError( "...required " + cAlphaFields( 7 ) + " is blank." );
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 7 ) + "=\"" + Alphas( 7 ) + "\"." );
				}
				ErrorsFound = true;
			} else {
				// Verify Curve Object, only legal type is BiQuadratic
				{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).EIRFTemp( 1 ) ) );

				if ( SELECT_CASE_var == "BIQUADRATIC" ) {
					DXCoil( DXCoilNum ).EIRTempModFacCurveType( 1 ) = BiQuadratic;

				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...illegal " + cAlphaFields( 7 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).EIRFTemp( 1 ) ) );
					ShowContinueError( "Curve type must be BiQuadratic." );
					ErrorsFound = true;
				}}
			}

			DXCoil( DXCoilNum ).EIRFFlow( 1 ) = GetCurveIndex( Alphas( 8 ) ); // convert curve name to number
			if ( DXCoil( DXCoilNum ).EIRFFlow( 1 ) == 0 ) {
				if ( lAlphaBlanks( 8 ) ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", missing" );
					ShowContinueError( "...required " + cAlphaFields( 8 ) + " is blank." );
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 8 ) + "=\"" + Alphas( 8 ) + "\"." );
				}
				ErrorsFound = true;
			} else {
				// Verify Curve Object, only legal type is Quadratic
				{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).EIRFFlow( 1 ) ) );

				if ( SELECT_CASE_var == "QUADRATIC" ) {

				} else if ( SELECT_CASE_var == "CUBIC" ) {

				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...illegal " + cAlphaFields( 8 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).EIRFFlow( 1 ) ) );
					ShowContinueError( "Curve type must be Quadratic or Cubic." );
					ErrorsFound = true;
				}}
			}

			DXCoil( DXCoilNum ).PLFFPLR( 1 ) = GetCurveIndex( Alphas( 9 ) ); // convert curve name to number
			if ( DXCoil( DXCoilNum ).PLFFPLR( 1 ) == 0 ) {
				if ( lAlphaBlanks( 9 ) ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", missing" );
					ShowContinueError( "...required " + cAlphaFields( 9 ) + " is blank." );
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 9 ) + "=\"" + Alphas( 9 ) + "\"." );
				}
				ErrorsFound = true;
			} else {
				// Verify Curve Object, only legal types are Quadratic or Cubic
				{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).PLFFPLR( 1 ) ) );

				if ( SELECT_CASE_var == "QUADRATIC" ) {

				} else if ( SELECT_CASE_var == "CUBIC" ) {

				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...illegal " + cAlphaFields( 9 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).PLFFPLR( 1 ) ) );
					ShowContinueError( "Curve type must be Quadratic or Cubic." );
					ErrorsFound = true;
				}}

				if ( ! ErrorsFound ) {
					//     Test PLF curve minimum and maximum. Cap if less than 0.7 or greater than 1.0.
					MinCurveVal = 999.0;
					MaxCurveVal = -999.0;
					CurveInput = 0.0;
					while ( CurveInput <= 1.0 ) {
						CurveVal = CurveValue( DXCoil( DXCoilNum ).PLFFPLR( 1 ), CurveInput );
						if ( CurveVal < MinCurveVal ) {
							MinCurveVal = CurveVal;
							MinCurvePLR = CurveInput;
						}
						if ( CurveVal > MaxCurveVal ) {
							MaxCurveVal = CurveVal;
							MaxCurvePLR = CurveInput;
						}
						CurveInput += 0.01;
					}
					if ( MinCurveVal < 0.7 ) {
						ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "..." + cAlphaFields( 9 ) + " = " + Alphas( 9 ) + " has out of range value." );
						ShowContinueError( "...Curve minimum must be >= 0.7, curve min at PLR = " + TrimSigDigits( MinCurvePLR, 2 ) + " is " + TrimSigDigits( MinCurveVal, 3 ) );
						ShowContinueError( "...Setting curve minimum to 0.7 and simulation continues." );
						SetCurveOutputMinMaxValues( DXCoil( DXCoilNum ).PLFFPLR( 1 ), ErrorsFound, 0.7, _ );
					}

					if ( MaxCurveVal > 1.0 ) {
						ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "..." + cAlphaFields( 9 ) + " = " + Alphas( 9 ) + " has out of range value." );
						ShowContinueError( "...Curve maximum must be <= 1.0, curve max at PLR = " + TrimSigDigits( MaxCurvePLR, 2 ) + " is " + TrimSigDigits( MaxCurveVal, 3 ) );
						ShowContinueError( "...Setting curve maximum to 1.0 and simulation continues." );
						SetCurveOutputMinMaxValues( DXCoil( DXCoilNum ).PLFFPLR( 1 ), ErrorsFound, _, 1.0 );
					}

				}

			}

			DXCoil( DXCoilNum ).RatedEIR( 1 ) = 1.0 / DXCoil( DXCoilNum ).RatedCOP( 1 );

			DXCoil( DXCoilNum ).RatedTotCap2 = Numbers( 6 );
			DXCoil( DXCoilNum ).RatedSHR2 = Numbers( 7 );
			DXCoil( DXCoilNum ).RatedCOP2 = Numbers( 8 );
			DXCoil( DXCoilNum ).RatedAirVolFlowRate2 = Numbers( 9 );

			DXCoil( DXCoilNum ).CCapFTemp2 = GetCurveIndex( Alphas( 10 ) ); // convert curve name to number
			if ( DXCoil( DXCoilNum ).CCapFTemp2 == 0 ) {
				if ( lAlphaBlanks( 10 ) ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", missing" );
					ShowContinueError( "...required " + cAlphaFields( 10 ) + " is blank." );
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 10 ) + "=\"" + Alphas( 10 ) + "\"." );
				}
				ErrorsFound = true;
			} else {
				// Verify Curve Object, only legal type is BiQuadratic
				{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).CCapFTemp2 ) );

				if ( SELECT_CASE_var == "BIQUADRATIC" ) {
					DXCoil( DXCoilNum ).TotCapTempModFacCurveType( 2 ) = BiQuadratic;

				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...illegal " + cAlphaFields( 10 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).CCapFTemp2 ) );
					ShowContinueError( "Curve type must be BiQuadratic." );
					ErrorsFound = true;
				}}
			}

			DXCoil( DXCoilNum ).EIRFTemp2 = GetCurveIndex( Alphas( 11 ) ); // convert curve name to number
			if ( DXCoil( DXCoilNum ).EIRFTemp2 == 0 ) {
				if ( lAlphaBlanks( 11 ) ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", missing" );
					ShowContinueError( "...required " + cAlphaFields( 11 ) + " is blank." );
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 11 ) + "=\"" + Alphas( 11 ) + "\"." );
				}
				ErrorsFound = true;
			} else {
				// Verify Curve Object, only legal type is BiQuadratic
				{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).EIRFTemp2 ) );

				if ( SELECT_CASE_var == "BIQUADRATIC" ) {
					DXCoil( DXCoilNum ).EIRTempModFacCurveType( 2 ) = BiQuadratic;

				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...illegal " + cAlphaFields( 11 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).EIRFTemp2 ) );
					ShowContinueError( "Curve type must be BiQuadratic." );
					ErrorsFound = true;
				}}
			}

			// outdoor condenser node
			if ( lAlphaBlanks( 12 ) ) {
				DXCoil( DXCoilNum ).CondenserInletNodeNum( 1 ) = 0;
			} else {
				DXCoil( DXCoilNum ).CondenserInletNodeNum( 1 ) = GetOnlySingleNode( Alphas( 12 ), ErrorsFound, CurrentModuleObject, DXCoil( DXCoilNum ).Name, NodeType_Air, NodeConnectionType_OutsideAirReference, 1, ObjectIsNotParent );
				if ( ! CheckOutAirNodeNumber( DXCoil( DXCoilNum ).CondenserInletNodeNum( 1 ) ) ) {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", may be invalid" );
					ShowContinueError( cAlphaFields( 12 ) + "=\"" + Alphas( 12 ) + "\", node does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node." );
					ShowContinueError( "This node needs to be included in an air system or the coil model will not be valid, and the simulation continues" );
				}
			}

			if ( ( SameString( Alphas( 13 ), "AirCooled" ) ) || lAlphaBlanks( 13 ) ) {
				DXCoil( DXCoilNum ).CondenserType( 1 ) = AirCooled;
			} else if ( SameString( Alphas( 13 ), "EvaporativelyCooled" ) ) {
				DXCoil( DXCoilNum ).CondenserType( 1 ) = EvapCooled;
				DXCoil( DXCoilNum ).ReportEvapCondVars = true;
			} else {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cAlphaFields( 13 ) + "=\"" + Alphas( 13 ) + "\":" );
				ShowContinueError( "...must be AirCooled or EvaporativelyCooled." );
				ErrorsFound = true;
			}

			DXCoil( DXCoilNum ).EvapCondEffect( 1 ) = Numbers( 10 );
			if ( DXCoil( DXCoilNum ).EvapCondEffect( 1 ) < 0.0 || DXCoil( DXCoilNum ).EvapCondEffect( 1 ) > 1.0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 9 ) + " cannot be < 0.0 or > 1.0." );
				ShowContinueError( "...entered value=[" + TrimSigDigits( Numbers( 10 ), 2 ) + "]." );
				ErrorsFound = true;
			}

			DXCoil( DXCoilNum ).EvapCondAirFlow( 1 ) = Numbers( 11 );
			if ( DXCoil( DXCoilNum ).EvapCondAirFlow( 1 ) < 0.0 && DXCoil( DXCoilNum ).EvapCondAirFlow( 1 ) != AutoSize ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 10 ) + " cannot be < 0.0." );
				ShowContinueError( "...entered value=[" + TrimSigDigits( Numbers( 11 ), 2 ) + "]." );
				ErrorsFound = true;
			}

			DXCoil( DXCoilNum ).EvapCondPumpElecNomPower( 1 ) = Numbers( 12 );
			if ( DXCoil( DXCoilNum ).EvapCondPumpElecNomPower( 1 ) < 0.0 && DXCoil( DXCoilNum ).EvapCondPumpElecNomPower( 1 ) != AutoSize ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 11 ) + " cannot be < 0.0." );
				ShowContinueError( "...entered value=[" + TrimSigDigits( Numbers( 12 ), 2 ) + "]." );
				ErrorsFound = true;
			}

			DXCoil( DXCoilNum ).EvapCondEffect2 = Numbers( 13 );
			if ( DXCoil( DXCoilNum ).EvapCondEffect2 < 0.0 || DXCoil( DXCoilNum ).EvapCondEffect2 > 1.0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 12 ) + " cannot be cannot be < 0.0 or > 1.0." );
				ShowContinueError( "...entered value=[" + TrimSigDigits( Numbers( 13 ), 2 ) + "]." );
				ErrorsFound = true;
			}

			DXCoil( DXCoilNum ).EvapCondAirFlow2 = Numbers( 14 );
			if ( DXCoil( DXCoilNum ).EvapCondAirFlow2 < 0.0 && DXCoil( DXCoilNum ).EvapCondAirFlow2 != AutoSize ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 13 ) + " cannot be < 0.0." );
				ShowContinueError( "...entered value=[" + TrimSigDigits( Numbers( 14 ), 2 ) + "]." );
				ErrorsFound = true;
			}

			DXCoil( DXCoilNum ).EvapCondPumpElecNomPower2 = Numbers( 15 );
			if ( DXCoil( DXCoilNum ).EvapCondPumpElecNomPower2 < 0.0 && DXCoil( DXCoilNum ).EvapCondPumpElecNomPower2 != AutoSize ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 14 ) + " cannot be < 0.0." );
				ShowContinueError( "...entered value=[" + TrimSigDigits( Numbers( 15 ), 2 ) + "]." );
				ErrorsFound = true;
			}

			DXCoil( DXCoilNum ).RatedEIR2 = 1.0 / DXCoil( DXCoilNum ).RatedCOP2;

			// Get Water System tank connections
			//  A14, \field Name of Water Storage Tank for Supply
			DXCoil( DXCoilNum ).EvapWaterSupplyName = Alphas( 14 );
			if ( lAlphaBlanks( 14 ) ) {
				DXCoil( DXCoilNum ).EvapWaterSupplyMode = WaterSupplyFromMains;
			} else {
				DXCoil( DXCoilNum ).EvapWaterSupplyMode = WaterSupplyFromTank;
				SetupTankDemandComponent( DXCoil( DXCoilNum ).Name, CurrentModuleObject, DXCoil( DXCoilNum ).EvapWaterSupplyName, ErrorsFound, DXCoil( DXCoilNum ).EvapWaterSupTankID, DXCoil( DXCoilNum ).EvapWaterTankDemandARRID );
			}

			//A15; \field Name of Water Storage Tank for Condensate Collection
			DXCoil( DXCoilNum ).CondensateCollectName = Alphas( 15 );
			if ( lAlphaBlanks( 15 ) ) {
				DXCoil( DXCoilNum ).CondensateCollectMode = CondensateDiscarded;
			} else {
				DXCoil( DXCoilNum ).CondensateCollectMode = CondensateToTank;
				SetupTankSupplyComponent( DXCoil( DXCoilNum ).Name, CurrentModuleObject, DXCoil( DXCoilNum ).CondensateCollectName, ErrorsFound, DXCoil( DXCoilNum ).CondensateTankID, DXCoil( DXCoilNum ).CondensateTankSupplyARRID );
			}

			// Basin heater power as a function of temperature must be greater than or equal to 0
			DXCoil( DXCoilNum ).BasinHeaterPowerFTempDiff = Numbers( 16 );
			if ( Numbers( 16 ) < 0.0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 15 ) + " must be >= 0.0." );
				ShowContinueError( "...entered value=[" + TrimSigDigits( Numbers( 16 ), 2 ) + "]." );
				ErrorsFound = true;
			}

			DXCoil( DXCoilNum ).BasinHeaterSetPointTemp = Numbers( 17 );
			if ( DXCoil( DXCoilNum ).BasinHeaterPowerFTempDiff > 0.0 ) {
				if ( NumNumbers < 17 ) {
					DXCoil( DXCoilNum ).BasinHeaterSetPointTemp = 2.0;
				}
				if ( DXCoil( DXCoilNum ).BasinHeaterSetPointTemp < 2.0 ) {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", freeze possible" );
					ShowContinueError( "..." + cNumericFields( 16 ) + " is < 2 {C}. Freezing could occur." );
					ShowContinueError( "...entered value=[" + TrimSigDigits( Numbers( 17 ), 2 ) + "]." );
				}
			}

			if ( ! lAlphaBlanks( 16 ) ) {
				DXCoil( DXCoilNum ).BasinHeaterSchedulePtr = GetScheduleIndex( Alphas( 16 ) );
				if ( DXCoil( DXCoilNum ).BasinHeaterSchedulePtr == 0 ) {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 16 ) + "=\"" + Alphas( 16 ) + "\"." );
					ShowContinueError( "Basin heater will be available to operate throughout the simulation." );
				}
			}

			if ( ! lAlphaBlanks( 17 ) && NumAlphas > 16 ) {
				DXCoil( DXCoilNum ).SHRFTemp( 1 ) = GetCurveIndex( Alphas( 17 ) ); // convert curve name to number
				//DXCoil(DXCoilNum)%SHRFTemp2 = DXCoil(DXCoilNum)%SHRFTemp(1)
				if ( DXCoil( DXCoilNum ).SHRFTemp( 1 ) == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 17 ) + "=\"" + Alphas( 17 ) + "\"." );
				} else {
					// Verify Curve Object, only legal type is BiQuadratic
					{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).SHRFTemp( 1 ) ) );
					if ( SELECT_CASE_var == "BIQUADRATIC" ) {
						DXCoil( DXCoilNum ).SHRFTempCurveType( 1 ) = BiQuadratic;
						DXCoil( DXCoilNum ).SHRFTempCurveType2 = DXCoil( DXCoilNum ).SHRFTempCurveType( 1 );
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( 17 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).SHRFTemp( 1 ) ) );
						ShowContinueError( "Curve type must be Biquadratic." );
						ErrorsFound = true;
					}}
				}
			}

			if ( ! lAlphaBlanks( 18 ) && NumAlphas > 17 ) {
				DXCoil( DXCoilNum ).SHRFFlow( 1 ) = GetCurveIndex( Alphas( 18 ) ); // convert curve name to number
				//DXCoil(DXCoilNum)%SHRFFlow2 = DXCoil(DXCoilNum)%SHRFFlow(1)
				if ( DXCoil( DXCoilNum ).SHRFTemp( 1 ) == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 18 ) + "=\"" + Alphas( 18 ) + "\"." );
				} else {
					// Verify Curve Object, only legal type is BiQuadratic
					{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).SHRFFlow( 1 ) ) );
					if ( ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) ) {

					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( 18 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).SHRFFlow( 1 ) ) );
						ShowContinueError( "Curve type must be quadratic or cubic." );
						ErrorsFound = true;
					}}
				}
			}

			if ( ! lAlphaBlanks( 19 ) && NumAlphas > 18 ) {
				DXCoil( DXCoilNum ).SHRFTemp2 = GetCurveIndex( Alphas( 19 ) ); // convert curve name to number
				if ( DXCoil( DXCoilNum ).SHRFTemp2 == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 19 ) + "=\"" + Alphas( 19 ) + "\"." );
				} else {
					// Verify Curve Object, only legal type is BiQuadratic
					{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).SHRFTemp2 ) );
					if ( SELECT_CASE_var == "BIQUADRATIC" ) {
						DXCoil( DXCoilNum ).SHRFTempCurveType2 = BiQuadratic;
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( 19 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).SHRFTemp2 ) );
						ShowContinueError( "Curve type must be Biquadratic." );
						ErrorsFound = true;
					}}
				}
			}

			if ( ! lAlphaBlanks( 20 ) && NumAlphas > 19 ) {
				DXCoil( DXCoilNum ).SHRFFlow2 = GetCurveIndex( Alphas( 20 ) ); // convert curve name to number
				if ( DXCoil( DXCoilNum ).SHRFTemp2 == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 20 ) + "=\"" + Alphas( 20 ) + "\"." );
				} else {
					// Verify Curve Object, only legal type is BiQuadratic
					{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).SHRFFlow2 ) );
					if ( ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) ) {

					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( 20 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).SHRFFlow2 ) );
						ShowContinueError( "Curve type must be quadratic or cubic." );
						ErrorsFound = true;
					}}
				}
			}
			if ( DXCoil( DXCoilNum ).SHRFTemp( 1 ) > 0 && DXCoil( DXCoilNum ).SHRFFlow( 1 ) > 0 && DXCoil( DXCoilNum ).SHRFTemp2 > 0 && DXCoil( DXCoilNum ).SHRFFlow2 > 0 ) {
				DXCoil( DXCoilNum ).UserSHRCurveExists = true;
			} else {
				DXCoil( DXCoilNum ).UserSHRCurveExists = false;
			}
			// A21; \field Zone Name for Condenser Placement
			if ( !lAlphaBlanks( 21 ) && NumAlphas > 20 ) {
				DXCoil( DXCoilNum ).SecZonePtr = FindItemInList( Alphas( 21 ), Zone );
				if ( DXCoil( DXCoilNum ).SecZonePtr > 0 ) {
					SetupZoneInternalGain( DXCoil( DXCoilNum ).SecZonePtr, "Coil:Cooling:DX:TwoSpeed", DXCoil( DXCoilNum ).Name, IntGainTypeOf_SecCoolingDXCoilTwoSpeed, DXCoil( DXCoilNum ).SecCoilSensibleHeatGainRate );
					DXCoil( DXCoilNum ).IsSecondaryDXCoilInZone = true;
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 21 ) + "=\"" + Alphas( 21 ) + "\"." );
				}
			}

		}

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in getting " + CurrentModuleObject + " input.  Preceding condition(s) causes termination." );
		}

		// Loop over the Pumped DX Water Heater Coils and get & load the data
		CurrentModuleObject = cAllCoilTypes( CoilDX_HeatPumpWaterHeaterPumped );
		for ( DXHPWaterHeaterCoilNum = 1; DXHPWaterHeaterCoilNum <= NumDXHeatPumpWaterHeaterPumpedCoils; ++DXHPWaterHeaterCoilNum ) {

			GetObjectItem( CurrentModuleObject, DXHPWaterHeaterCoilNum, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			++DXCoilNum;

			// allocate single performance mode for numeric field strings used for sizing routine
			DXCoilNumericFields ( DXCoilNum ).PerfMode.allocate ( 1 );
			DXCoilNumericFields( DXCoilNum ).PerfMode( 1 ).FieldNames.allocate ( MaxNumbers );
			DXCoilNumericFields ( DXCoilNum ).PerfMode ( 1 ).FieldNames = cNumericFields;

			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), DXCoil, DXCoilNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			VerifyUniqueCoilName( CurrentModuleObject, Alphas( 1 ), errFlag, CurrentModuleObject + " Name" );
			if ( errFlag ) {
				ErrorsFound = true;
			}
			DXCoil( DXCoilNum ).Name = Alphas( 1 );
			DXCoil( DXCoilNum ).DXCoilType = CurrentModuleObject;
			DXCoil( DXCoilNum ).DXCoilType_Num = CoilDX_HeatPumpWaterHeaterPumped;
			DXCoil( DXCoilNum ).SchedPtr = 0; // heat pump water heater DX coil has no schedule

			// Store the HPWH DX coil heating capacity in RatedTotCap2. After backing off pump and fan heat,
			// move to RatedTotCap() for use by DX coil
			DXCoil( DXCoilNum ).RatedTotCap2 = Numbers( 1 );
			if ( DXCoil( DXCoilNum ).RatedTotCap2 <= 0.0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 1 ) + " must be > 0.0, entered value=[" + TrimSigDigits( Numbers( 1 ), 2 ) + "]." );
				ErrorsFound = true;
			}

			DXCoil( DXCoilNum ).RatedCOP( 1 ) = Numbers( 2 );
			if ( DXCoil( DXCoilNum ).RatedCOP( 1 ) <= 0.0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 2 ) + " must be > 0.0, entered value=[" + TrimSigDigits( Numbers( 2 ), 2 ) + "]." );
				ErrorsFound = true;
			}

			DXCoil( DXCoilNum ).RatedSHR( 1 ) = Numbers( 3 );
			if ( DXCoil( DXCoilNum ).RatedSHR( 1 ) <= 0.0 || DXCoil( DXCoilNum ).RatedSHR( 1 ) > 1.0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 3 ) + " must be > 0 and <= 1.  entered value=[" + TrimSigDigits( Numbers( 3 ), 3 ) + "]." );

				ErrorsFound = true;
			}

			DXCoil( DXCoilNum ).RatedInletDBTemp = Numbers( 4 );
			if ( DXCoil( DXCoilNum ).RatedInletDBTemp <= 5.0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 4 ) + " must be > 5 {C}.  entered value=[" + TrimSigDigits( Numbers( 4 ), 1 ) + "]." );
				ErrorsFound = true;
			}

			DXCoil( DXCoilNum ).RatedInletWBTemp = Numbers( 5 );
			if ( DXCoil( DXCoilNum ).RatedInletWBTemp <= 5.0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 5 ) + " must be > 5 {C}.  entered value=[" + TrimSigDigits( Numbers( 5 ), 1 ) + "]." );
				ErrorsFound = true;
			}

			DXCoil( DXCoilNum ).RatedInletWaterTemp = Numbers( 6 );
			if ( DXCoil( DXCoilNum ).RatedInletWaterTemp <= 25.0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 6 ) + " must be > 25 {C}.  entered value=[" + TrimSigDigits( Numbers( 6 ), 1 ) + "]." );
				ErrorsFound = true;
			}

			DXCoil( DXCoilNum ).RatedAirVolFlowRate( 1 ) = Numbers( 7 );
			if ( DXCoil( DXCoilNum ).RatedAirVolFlowRate( 1 ) != AutoCalculate ) {
				if ( DXCoil( DXCoilNum ).RatedAirVolFlowRate( 1 ) <= 0.0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "..." + cNumericFields( 7 ) + " must be > 0.0.  entered value=[" + TrimSigDigits( Numbers( 7 ), 3 ) + "]." );
					ErrorsFound = true;
				}
			}

			DXCoil( DXCoilNum ).RatedHPWHCondWaterFlow = Numbers( 8 );
			// move to init
			if ( DXCoil( DXCoilNum ).RatedHPWHCondWaterFlow != AutoCalculate ) {
				if ( DXCoil( DXCoilNum ).RatedHPWHCondWaterFlow <= 0.0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "..." + cNumericFields( 8 ) + " must be > 0.0  entered value=[" + TrimSigDigits( Numbers( 8 ), 3 ) + "]." );
					ErrorsFound = true;
				}
				//   check the range of flow rate to be >= 1 gpm/ton and <= 5 gpm/ton
				if ( DXCoil( DXCoilNum ).RatedHPWHCondWaterFlow / DXCoil( DXCoilNum ).RatedTotCap2 < 1.79405e-8 || DXCoil( DXCoilNum ).RatedHPWHCondWaterFlow / DXCoil( DXCoilNum ).RatedTotCap2 > 8.97024e-8 ) {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", outside range" );
					ShowContinueError( "..." + cNumericFields( 8 ) + " per watt of " + cNumericFields( 1 ) + " is outside the recommended range of >= 1.79405E-8 m3/s/W (0.083 gpm/MBH) and <= 8.97024E-8 m3/s/W (0.417 gpm/MBH)." );
					ShowContinueError( "...Entered Flow rate per watt = [" + TrimSigDigits( ( DXCoil( DXCoilNum ).RatedHPWHCondWaterFlow / DXCoil( DXCoilNum ).RatedTotCap2 ), 10 ) + "]." );
				}
			}

			if ( SameString( Alphas( 2 ), "Yes" ) || SameString( Alphas( 2 ), "No" ) ) {
				//  initialized to TRUE on allocate
				if ( SameString( Alphas( 2 ), "No" ) ) DXCoil( DXCoilNum ).FanPowerIncludedInCOP = false;
			} else {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( ",,,invalid choice for " + cAlphaFields( 2 ) + ".  Entered choice = " + Alphas( 2 ) );
				ShowContinueError( "Valid choices are Yes or No." );
				ErrorsFound = true;
			}

			if ( SameString( Alphas( 3 ), "Yes" ) || SameString( Alphas( 3 ), "No" ) ) {
				//  initialized to FALSE on allocate
				if ( SameString( Alphas( 3 ), "Yes" ) ) DXCoil( DXCoilNum ).CondPumpPowerInCOP = true;
			} else {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( ",,,invalid choice for " + cAlphaFields( 3 ) + ".  Entered choice = " + Alphas( 3 ) );
				ShowContinueError( "Valid choices are Yes or No." );
				ErrorsFound = true;
			}

			if ( SameString( Alphas( 4 ), "Yes" ) || SameString( Alphas( 4 ), "No" ) ) {
				//  initialized to FALSE on allocate
				if ( SameString( Alphas( 4 ), "Yes" ) ) DXCoil( DXCoilNum ).CondPumpHeatInCapacity = true;
			} else {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( ",,,invalid choice for " + cAlphaFields( 4 ) + ".  Entered choice = " + Alphas( 4 ) );
				ShowContinueError( "Valid choices are Yes or No." );
				ErrorsFound = true;
			}

			DXCoil( DXCoilNum ).HPWHCondPumpElecNomPower = Numbers( 9 );
			if ( DXCoil( DXCoilNum ).HPWHCondPumpElecNomPower < 0.0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 9 ) + " must be >= 0.0  entered value=[" + TrimSigDigits( Numbers( 9 ), 3 ) + "]." );
				ErrorsFound = true;
			}

			DXCoil( DXCoilNum ).HPWHCondPumpFracToWater = Numbers( 10 );
			if ( DXCoil( DXCoilNum ).HPWHCondPumpFracToWater <= 0.0 || DXCoil( DXCoilNum ).HPWHCondPumpFracToWater > 1.0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 10 ) + " must be >= 0 and <= 1.  entered value=[" + TrimSigDigits( Numbers( 10 ), 3 ) + "]." );
				ErrorsFound = true;
			}

			//Air nodes
			DXCoil( DXCoilNum ).AirInNode = GetOnlySingleNode( Alphas( 5 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

			DXCoil( DXCoilNum ).AirOutNode = GetOnlySingleNode( Alphas( 6 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			TestCompSet( CurrentModuleObject, Alphas( 1 ), Alphas( 5 ), Alphas( 6 ), "Air Nodes" );

			//Check if the air inlet node is OA node, to justify whether the coil is placed in zone or not
			DXCoil( DXCoilNum ).IsDXCoilInZone = ! CheckOutAirNodeNumber( DXCoil( DXCoilNum ).AirInNode );

			//Water nodes
			DXCoil( DXCoilNum ).WaterInNode = GetOnlySingleNode( Alphas( 7 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Water, NodeConnectionType_Inlet, 2, ObjectIsNotParent );

			DXCoil( DXCoilNum ).WaterOutNode = GetOnlySingleNode( Alphas( 8 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Water, NodeConnectionType_Outlet, 2, ObjectIsNotParent );

			TestCompSet( CurrentModuleObject, Alphas( 1 ), Alphas( 7 ), Alphas( 8 ), "Water Nodes" );

			DXCoil( DXCoilNum ).CrankcaseHeaterCapacity = Numbers( 11 );
			if ( DXCoil( DXCoilNum ).CrankcaseHeaterCapacity < 0.0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 11 ) + " must be >= 0.0  entered value=[" + TrimSigDigits( Numbers( 11 ), 1 ) + "]." );
				ErrorsFound = true;
			}

			DXCoil( DXCoilNum ).MaxOATCrankcaseHeater = Numbers( 12 );
			if ( DXCoil( DXCoilNum ).MaxOATCrankcaseHeater < 0.0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 12 ) + " must be >= 0 {C}.  entered value=[" + TrimSigDigits( Numbers( 12 ), 1 ) + "]." );
				ErrorsFound = true;
			}

			if ( SameString( Alphas( 9 ), "DryBulbTemperature" ) ) {
				DXCoil( DXCoilNum ).InletAirTemperatureType = DryBulbIndicator;
			} else if ( SameString( Alphas( 9 ), "WetBulbTemperature" ) ) {
				DXCoil( DXCoilNum ).InletAirTemperatureType = WetBulbIndicator;
			} else {
				//   wrong temperature type selection
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cAlphaFields( 9 ) + " must be DryBulbTemperature or WetBulbTemperature." );
				ShowContinueError( "...entered value=\"" + Alphas( 9 ) + "\"." );
				ErrorsFound = true;
			}

			// set rated inlet air temperature for curve object verification
			if ( DXCoil( DXCoilNum ).InletAirTemperatureType == WetBulbIndicator ) {
				InletAirTemp = DXCoil( DXCoilNum ).RatedInletWBTemp;
			} else {
				InletAirTemp = DXCoil( DXCoilNum ).RatedInletDBTemp;
			}
			// set rated water temperature for curve object verification
			InletWaterTemp = DXCoil( DXCoilNum ).RatedInletWaterTemp;

			if ( ! lAlphaBlanks( 10 ) ) {
				DXCoil( DXCoilNum ).HCapFTemp = GetCurveIndex( Alphas( 10 ) );
				if ( DXCoil( DXCoilNum ).HCapFTemp == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 10 ) + "=\"" + Alphas( 10 ) + "\"." );
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal types are BiQuadratic or Cubic
					{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).HCapFTemp ) );

					if ( SELECT_CASE_var == "BIQUADRATIC" ) {
						DXCoil( DXCoilNum ).HCapFTempCurveType = BiQuadratic;
						HeatCapFTemp = CurveValue( DXCoil( DXCoilNum ).HCapFTemp, InletAirTemp, InletWaterTemp );

					} else if ( SELECT_CASE_var == "CUBIC" ) {
						DXCoil( DXCoilNum ).HCapFTempCurveType = Cubic;
						HeatCapFTemp = CurveValue( DXCoil( DXCoilNum ).HCapFTemp, InletAirTemp );

					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( 10 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).HCapFTemp ) );
						ShowContinueError( "Curve type must be BiQuadratic or Cubic." );
						ErrorsFound = true;
						HeatCapFTemp = 1.0;
					}}

					if ( std::abs( HeatCapFTemp - 1.0 ) > 0.05 ) {
						ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\":" );
						// could remove name from the field for output
						ShowContinueError( "...The " + cAlphaFields( 10 ) + " should be normalized to 1.0 at the rating point." );
						ShowContinueError( "...Curve output at the rating point = " + TrimSigDigits( HeatCapFTemp, 3 ) );
						ShowContinueError( "...The simulation continues using the user-specified curve." );
					}

				}
			}

			if ( ! lAlphaBlanks( 11 ) ) {
				DXCoil( DXCoilNum ).HCapFAirFlow = GetCurveIndex( Alphas( 11 ) );
				if ( DXCoil( DXCoilNum ).HCapFAirFlow == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 11 ) + "=\"" + Alphas( 11 ) + "\"." );
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal types are Cubic or Quadratic
					{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).HCapFAirFlow ) );

					if ( SELECT_CASE_var == "CUBIC" ) {
						HeatCapFAirFlow = CurveValue( DXCoil( DXCoilNum ).HCapFAirFlow, 1.0 );

					} else if ( SELECT_CASE_var == "QUADRATIC" ) {
						HeatCapFAirFlow = CurveValue( DXCoil( DXCoilNum ).HCapFAirFlow, 1.0 );

					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( 11 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).HCapFAirFlow ) );
						ShowContinueError( "Curve type must be Quadratic or Cubic." );
						ErrorsFound = true;
						HeatCapFAirFlow = 1.0;
					}}

					if ( std::abs( HeatCapFAirFlow - 1.0 ) > 0.05 ) {
						ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\":" );
						// could remove name from the field for output
						ShowContinueError( "...The " + cAlphaFields( 11 ) + " should be normalized to 1.0 at the rating point." );
						ShowContinueError( "...Curve output at an air flow fraction of 1 = " + TrimSigDigits( HeatCapFAirFlow, 3 ) );
						ShowContinueError( "...The simulation continues using the user-specified curve." );
					}

				}
			}

			if ( ! lAlphaBlanks( 12 ) ) {
				DXCoil( DXCoilNum ).HCapFWaterFlow = GetCurveIndex( Alphas( 12 ) );
				if ( DXCoil( DXCoilNum ).HCapFWaterFlow == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 12 ) + "=\"" + Alphas( 12 ) + "\"." );
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal types are Cubic or Quadratic
					{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).HCapFWaterFlow ) );

					if ( SELECT_CASE_var == "CUBIC" ) {
						HeatCapFWaterFlow = CurveValue( DXCoil( DXCoilNum ).HCapFWaterFlow, 1.0 );

					} else if ( SELECT_CASE_var == "QUADRATIC" ) {
						HeatCapFWaterFlow = CurveValue( DXCoil( DXCoilNum ).HCapFWaterFlow, 1.0 );

					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( 12 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).HCapFWaterFlow ) );
						ShowContinueError( "Curve type must be Quadratic or Cubic." );
						ErrorsFound = true;
						HeatCapFWaterFlow = 1.0;
					}}

					if ( std::abs( HeatCapFWaterFlow - 1.0 ) > 0.05 ) {
						ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\":" );
						// could remove name from the field for output
						ShowContinueError( "...The " + cAlphaFields( 11 ) + " should be normalized to 1.0 at the rating point." );
						ShowContinueError( "...Curve output at an air flow fraction of 1 = " + TrimSigDigits( HeatCapFWaterFlow, 3 ) );
						ShowContinueError( "...The simulation continues using the user-specified curve." );
					}

				}
			}

			if ( ! lAlphaBlanks( 13 ) ) {
				DXCoil( DXCoilNum ).HCOPFTemp = GetCurveIndex( Alphas( 13 ) );
				if ( DXCoil( DXCoilNum ).HCOPFTemp == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 13 ) + "=\"" + Alphas( 13 ) + "\"." );
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal types are BiQuadratic or Cubic
					{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).HCOPFTemp ) );

					if ( SELECT_CASE_var == "BIQUADRATIC" ) {
						DXCoil( DXCoilNum ).HCOPFTempCurveType = BiQuadratic;
						HeatCOPFTemp = CurveValue( DXCoil( DXCoilNum ).HCOPFTemp, InletAirTemp, InletWaterTemp );

					} else if ( SELECT_CASE_var == "CUBIC" ) {
						DXCoil( DXCoilNum ).HCOPFTempCurveType = Cubic;
						HeatCOPFTemp = CurveValue( DXCoil( DXCoilNum ).HCOPFTemp, InletAirTemp );

					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( 13 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).HCOPFTemp ) );
						ShowContinueError( "Curve type must be BiQuadratic or Cubic." );
						ErrorsFound = true;
						HeatCOPFTemp = 1.0;
					}}

					if ( std::abs( HeatCOPFTemp - 1.0 ) > 0.05 ) {
						ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\":" );
						// could remove name from the field for output
						ShowContinueError( "...The " + cAlphaFields( 13 ) + " should be normalized to 1.0 at the rating point." );
						ShowContinueError( "...Curve output at an air flow fraction of 1 = " + TrimSigDigits( HeatCOPFTemp, 3 ) );
						ShowContinueError( "...The simulation continues using the user-specified curve." );
					}

				}
			}

			if ( ! lAlphaBlanks( 14 ) ) {
				DXCoil( DXCoilNum ).HCOPFAirFlow = GetCurveIndex( Alphas( 14 ) );
				if ( DXCoil( DXCoilNum ).HCOPFAirFlow == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 14 ) + "=\"" + Alphas( 14 ) + "\"." );
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal types are Cubic or Quadratic
					{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).HCOPFAirFlow ) );

					if ( SELECT_CASE_var == "CUBIC" ) {
						HeatCOPFAirFlow = CurveValue( DXCoil( DXCoilNum ).HCOPFAirFlow, 1.0 );

					} else if ( SELECT_CASE_var == "QUADRATIC" ) {
						HeatCOPFAirFlow = CurveValue( DXCoil( DXCoilNum ).HCOPFAirFlow, 1.0 );

					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( 14 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).HCOPFAirFlow ) );
						ShowContinueError( "Curve type must be Quadratic or Cubic." );
						ErrorsFound = true;
						HeatCOPFAirFlow = 1.0;
					}}

					if ( std::abs( HeatCOPFAirFlow - 1.0 ) > 0.05 ) {
						ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\":" );
						// could remove name from the field for output
						ShowContinueError( "...The " + cAlphaFields( 14 ) + " should be normalized to 1.0 at the rating point." );
						ShowContinueError( "...Curve output at an air flow fraction of 1 = " + TrimSigDigits( HeatCOPFAirFlow, 3 ) );
						ShowContinueError( "...The simulation continues using the user-specified curve." );
					}

				}
			}

			if ( ! lAlphaBlanks( 15 ) ) {
				DXCoil( DXCoilNum ).HCOPFWaterFlow = GetCurveIndex( Alphas( 15 ) );
				if ( DXCoil( DXCoilNum ).HCOPFWaterFlow == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 15 ) + "=\"" + Alphas( 15 ) + "\"." );
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal types are Cubic or Quadratic
					{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).HCOPFWaterFlow ) );

					if ( SELECT_CASE_var == "CUBIC" ) {
						HeatCOPFWaterFlow = CurveValue( DXCoil( DXCoilNum ).HCOPFWaterFlow, 1.0 );

					} else if ( SELECT_CASE_var == "QUADRATIC" ) {
						HeatCOPFWaterFlow = CurveValue( DXCoil( DXCoilNum ).HCOPFWaterFlow, 1.0 );

					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( 15 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).HCOPFWaterFlow ) );
						ShowContinueError( "Curve type must be Quadratic or Cubic." );
						ErrorsFound = true;
						HeatCOPFWaterFlow = 1.0;
					}}

					if ( std::abs( HeatCOPFWaterFlow - 1.0 ) > 0.05 ) {
						ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\":" );
						// could remove name from the field for output
						ShowContinueError( "...The " + cAlphaFields( 15 ) + " should be normalized to 1.0 at the rating point." );
						ShowContinueError( "...Curve output at a water flow fraction of 1 = " + TrimSigDigits( HeatCOPFWaterFlow, 3 ) );
						ShowContinueError( "...The simulation continues using the user-specified curve." );
					}

				}
			}

			if ( ! lAlphaBlanks( 16 ) ) {
				DXCoil( DXCoilNum ).PLFFPLR( 1 ) = GetCurveIndex( Alphas( 16 ) );
				if ( DXCoil( DXCoilNum ).PLFFPLR( 1 ) == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 16 ) + "=\"" + Alphas( 16 ) + "\"." );
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal types are Cubic or Quadratic
					{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).PLFFPLR( 1 ) ) );

					if ( SELECT_CASE_var == "CUBIC" ) {

					} else if ( SELECT_CASE_var == "QUADRATIC" ) {

					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( 16 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).PLFFPLR( 1 ) ) );
						ShowContinueError( "Curve type must be Quadratic or Cubic." );
						ErrorsFound = true;
					}}

					if ( ! ErrorsFound ) {
						//       Test PLF curve minimum and maximum. Cap if less than 0.7 or greater than 1.0.
						MinCurveVal = 999.0;
						MaxCurveVal = -999.0;
						CurveInput = 0.0;
						while ( CurveInput <= 1.0 ) {
							CurveVal = CurveValue( DXCoil( DXCoilNum ).PLFFPLR( 1 ), CurveInput );
							if ( CurveVal < MinCurveVal ) {
								MinCurveVal = CurveVal;
								MinCurvePLR = CurveInput;
							}
							if ( CurveVal > MaxCurveVal ) {
								MaxCurveVal = CurveVal;
								MaxCurvePLR = CurveInput;
							}
							CurveInput += 0.01;
						}
						if ( MinCurveVal < 0.7 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
							ShowContinueError( "..." + cAlphaFields( 16 ) + " = " + Alphas( 16 ) + " has out of range value." );
							ShowContinueError( "...Curve minimum must be >= 0.7, curve min at PLR = " + TrimSigDigits( MinCurvePLR, 2 ) + " is " + TrimSigDigits( MinCurveVal, 3 ) );
							ShowContinueError( "...Setting curve minimum to 0.7 and simulation continues." );
							SetCurveOutputMinMaxValues( DXCoil( DXCoilNum ).PLFFPLR( 1 ), ErrorsFound, 0.7, _ );
						}

						if ( MaxCurveVal > 1.0 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
							ShowContinueError( "..." + cAlphaFields( 16 ) + " = " + Alphas( 16 ) + " has out of range value." );
							ShowContinueError( "...Curve maximum must be <= 1.0, curve max at PLR = " + TrimSigDigits( MaxCurvePLR, 2 ) + " is " + TrimSigDigits( MaxCurveVal, 3 ) );
							ShowContinueError( "...Setting curve maximum to 1.0 and simulation continues." );
							SetCurveOutputMinMaxValues( DXCoil( DXCoilNum ).PLFFPLR( 1 ), ErrorsFound, _, 1.0 );
						}

					}

				}
			}

			// assume compressor resides at the inlet to the DX Coil
			DXCoil( DXCoilNum ).CondenserInletNodeNum( 1 ) = DXCoil( DXCoilNum ).AirInNode;

			// set condenser type as HPWH
			DXCoil( DXCoilNum ).CondenserType( 1 ) = WaterHeater;

		} // end of the DX water heater coil loop

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in getting " + CurrentModuleObject + " input. Preceding condition(s) causes termination." );
		}
		// Loop over the Wrapped DX Water Heater Coils and get & load the data
		CurrentModuleObject = cAllCoilTypes( CoilDX_HeatPumpWaterHeaterWrapped );
		for ( DXHPWaterHeaterCoilNum = 1; DXHPWaterHeaterCoilNum <= NumDXHeatPumpWaterHeaterWrappedCoils; ++DXHPWaterHeaterCoilNum ) {

			GetObjectItem( CurrentModuleObject, DXHPWaterHeaterCoilNum, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			++DXCoilNum;

			// allocate single performance mode for numeric field strings used for sizing routine
			DXCoilNumericFields ( DXCoilNum ).PerfMode.allocate ( 1 );
			DXCoilNumericFields( DXCoilNum ).PerfMode( 1 ).FieldNames.allocate ( MaxNumbers );
			DXCoilNumericFields ( DXCoilNum ).PerfMode ( 1 ).FieldNames = cNumericFields;

			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), DXCoil, DXCoilNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			VerifyUniqueCoilName( CurrentModuleObject, Alphas( 1 ), errFlag, CurrentModuleObject + " Name" );
			if ( errFlag ) {
				ErrorsFound = true;
			}
			DXCoil( DXCoilNum ).Name = Alphas( 1 );
			DXCoil( DXCoilNum ).DXCoilType = CurrentModuleObject;
			DXCoil( DXCoilNum ).DXCoilType_Num = CoilDX_HeatPumpWaterHeaterWrapped;
			DXCoil( DXCoilNum ).SchedPtr = 0; // heat pump water heater DX coil has no schedule

			// Store the HPWH DX coil heating capacity in RatedTotCap2. After backing off pump and fan heat,
			// move to RatedTotCap() for use by DX coil
			DXCoil( DXCoilNum ).RatedTotCap2 = Numbers( 1 );
			if ( DXCoil( DXCoilNum ).RatedTotCap2 <= 0.0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 1 ) + " must be > 0.0, entered value=[" + TrimSigDigits( Numbers( 1 ), 2 ) + "]." );
				ErrorsFound = true;
			}

			DXCoil( DXCoilNum ).RatedCOP( 1 ) = Numbers( 2 );
			if ( DXCoil( DXCoilNum ).RatedCOP( 1 ) <= 0.0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 2 ) + " must be > 0.0, entered value=[" + TrimSigDigits( Numbers( 2 ), 2 ) + "]." );
				ErrorsFound = true;
			}

			DXCoil( DXCoilNum ).RatedSHR( 1 ) = Numbers( 3 );
			if ( DXCoil( DXCoilNum ).RatedSHR( 1 ) <= 0.0 || DXCoil( DXCoilNum ).RatedSHR( 1 ) > 1.0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 3 ) + " must be > 0 and <= 1.  entered value=[" + TrimSigDigits( Numbers( 3 ), 3 ) + "]." );

				ErrorsFound = true;
			}

			DXCoil( DXCoilNum ).RatedInletDBTemp = Numbers( 4 );
			if ( DXCoil( DXCoilNum ).RatedInletDBTemp <= 5.0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 4 ) + " must be > 5 {C}.  entered value=[" + TrimSigDigits( Numbers( 4 ), 1 ) + "]." );
				ErrorsFound = true;
			}

			DXCoil( DXCoilNum ).RatedInletWBTemp = Numbers( 5 );
			if ( DXCoil( DXCoilNum ).RatedInletWBTemp <= 5.0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 5 ) + " must be > 5 {C}.  entered value=[" + TrimSigDigits( Numbers( 5 ), 1 ) + "]." );
				ErrorsFound = true;
			}

			DXCoil( DXCoilNum ).RatedInletWaterTemp = Numbers( 6 );
			if ( DXCoil( DXCoilNum ).RatedInletWaterTemp <= 25.0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 6 ) + " must be > 25 {C}.  entered value=[" + TrimSigDigits( Numbers( 6 ), 1 ) + "]." );
				ErrorsFound = true;
			}

			DXCoil( DXCoilNum ).RatedAirVolFlowRate( 1 ) = Numbers( 7 );
			if ( DXCoil( DXCoilNum ).RatedAirVolFlowRate( 1 ) != AutoCalculate ) {
				if ( DXCoil( DXCoilNum ).RatedAirVolFlowRate( 1 ) <= 0.0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "..." + cNumericFields( 7 ) + " must be > 0.0.  entered value=[" + TrimSigDigits( Numbers( 7 ), 3 ) + "]." );
					ErrorsFound = true;
				}
			}

			if ( SameString( Alphas( 2 ), "Yes" ) || SameString( Alphas( 2 ), "No" ) ) {
				//  initialized to TRUE on allocate
				if ( SameString( Alphas( 2 ), "No" ) ) DXCoil( DXCoilNum ).FanPowerIncludedInCOP = false;
			} else {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( ",,,invalid choice for " + cAlphaFields( 2 ) + ".  Entered choice = " + Alphas( 2 ) );
				ShowContinueError( "Valid choices are Yes or No." );
				ErrorsFound = true;
			}

			//Air nodes
			DXCoil( DXCoilNum ).AirInNode = GetOnlySingleNode( Alphas( 3 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

			DXCoil( DXCoilNum ).AirOutNode = GetOnlySingleNode( Alphas( 4 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			TestCompSet( CurrentModuleObject, Alphas( 1 ), Alphas( 3 ), Alphas( 4 ), "Air Nodes" );

			//Check if the air inlet node is OA node, to justify whether the coil is placed in zone or not
			DXCoil( DXCoilNum ).IsDXCoilInZone = ! CheckOutAirNodeNumber( DXCoil( DXCoilNum ).AirInNode );

			std::string const DummyCondenserInletName( "DUMMY CONDENSER INLET " + DXCoil( DXCoilNum ).Name );
			std::string const DummyCondenserOutletName( "DUMMY CONDENSER OUTLET " + DXCoil( DXCoilNum ).Name );
			DXCoil( DXCoilNum ).WaterInNode = GetOnlySingleNode( DummyCondenserInletName, ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Water, NodeConnectionType_Inlet, 2, ObjectIsNotParent );

			DXCoil( DXCoilNum ).WaterOutNode = GetOnlySingleNode( DummyCondenserOutletName, ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Water, NodeConnectionType_Outlet, 2, ObjectIsNotParent );

			TestCompSet( CurrentModuleObject, Alphas( 1 ), DummyCondenserInletName, DummyCondenserOutletName, "Water Nodes" );

			DXCoil( DXCoilNum ).CrankcaseHeaterCapacity = Numbers( 8 );
			if ( DXCoil( DXCoilNum ).CrankcaseHeaterCapacity < 0.0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 8 ) + " must be >= 0.0  entered value=[" + TrimSigDigits( Numbers( 8 ), 1 ) + "]." );
				ErrorsFound = true;
			}

			DXCoil( DXCoilNum ).MaxOATCrankcaseHeater = Numbers( 9 );
			if ( DXCoil( DXCoilNum ).MaxOATCrankcaseHeater < 0.0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 9 ) + " must be >= 0 {C}.  entered value=[" + TrimSigDigits( Numbers( 9 ), 1 ) + "]." );
				ErrorsFound = true;
			}

			if ( SameString( Alphas( 5 ), "DryBulbTemperature" ) ) {
				DXCoil( DXCoilNum ).InletAirTemperatureType = DryBulbIndicator;
			} else if ( SameString( Alphas( 5 ), "WetBulbTemperature" ) ) {
				DXCoil( DXCoilNum ).InletAirTemperatureType = WetBulbIndicator;
			} else {
				//   wrong temperature type selection
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cAlphaFields( 5 ) + " must be DryBulbTemperature or WetBulbTemperature." );
				ShowContinueError( "...entered value=\"" + Alphas( 5 ) + "\"." );
				ErrorsFound = true;
			}

			// set rated inlet air temperature for curve object verification
			if ( DXCoil( DXCoilNum ).InletAirTemperatureType == WetBulbIndicator ) {
				InletAirTemp = DXCoil( DXCoilNum ).RatedInletWBTemp;
			} else {
				InletAirTemp = DXCoil( DXCoilNum ).RatedInletDBTemp;
			}
			// set rated water temperature for curve object verification
			InletWaterTemp = DXCoil( DXCoilNum ).RatedInletWaterTemp;

			if ( ! lAlphaBlanks( 6 ) ) {
				DXCoil( DXCoilNum ).HCapFTemp = GetCurveIndex( Alphas( 6 ) );
				if ( DXCoil( DXCoilNum ).HCapFTemp == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 6 ) + "=\"" + Alphas( 6 ) + "\"." );
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal types are BiQuadratic or Cubic
					{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).HCapFTemp ) );

						if ( SELECT_CASE_var == "BIQUADRATIC" ) {
							DXCoil( DXCoilNum ).HCapFTempCurveType = BiQuadratic;
							HeatCapFTemp = CurveValue( DXCoil( DXCoilNum ).HCapFTemp, InletAirTemp, InletWaterTemp );

						} else if ( SELECT_CASE_var == "CUBIC" ) {
							DXCoil( DXCoilNum ).HCapFTempCurveType = Cubic;
							HeatCapFTemp = CurveValue( DXCoil( DXCoilNum ).HCapFTemp, InletAirTemp );

						} else {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
							ShowContinueError( "...illegal " + cAlphaFields( 6 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).HCapFTemp ) );
							ShowContinueError( "Curve type must be BiQuadratic or Cubic." );
							ErrorsFound = true;
							HeatCapFTemp = 1.0;
						}}

					if ( std::abs( HeatCapFTemp - 1.0 ) > 0.05 ) {
						ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\":" );
						// could remove name from the field for output
						ShowContinueError( "...The " + cAlphaFields( 6 ) + " should be normalized to 1.0 at the rating point." );
						ShowContinueError( "...Curve output at the rating point = " + TrimSigDigits( HeatCapFTemp, 3 ) );
						ShowContinueError( "...The simulation continues using the user-specified curve." );
					}

				}
			}

			if ( ! lAlphaBlanks( 7 ) ) {
				DXCoil( DXCoilNum ).HCapFAirFlow = GetCurveIndex( Alphas( 7 ) );
				if ( DXCoil( DXCoilNum ).HCapFAirFlow == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 7 ) + "=\"" + Alphas( 7 ) + "\"." );
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal types are Cubic or Quadratic
					{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).HCapFAirFlow ) );

						if ( SELECT_CASE_var == "CUBIC" ) {
							HeatCapFAirFlow = CurveValue( DXCoil( DXCoilNum ).HCapFAirFlow, 1.0 );

						} else if ( SELECT_CASE_var == "QUADRATIC" ) {
							HeatCapFAirFlow = CurveValue( DXCoil( DXCoilNum ).HCapFAirFlow, 1.0 );

						} else {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
							ShowContinueError( "...illegal " + cAlphaFields( 7 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).HCapFAirFlow ) );
							ShowContinueError( "Curve type must be Quadratic or Cubic." );
							ErrorsFound = true;
							HeatCapFAirFlow = 1.0;
						}}

					if ( std::abs( HeatCapFAirFlow - 1.0 ) > 0.05 ) {
						ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\":" );
						// could remove name from the field for output
						ShowContinueError( "...The " + cAlphaFields( 7 ) + " should be normalized to 1.0 at the rating point." );
						ShowContinueError( "...Curve output at an air flow fraction of 1 = " + TrimSigDigits( HeatCapFAirFlow, 3 ) );
						ShowContinueError( "...The simulation continues using the user-specified curve." );
					}

				}
			}

			if ( ! lAlphaBlanks( 8 ) ) {
				DXCoil( DXCoilNum ).HCOPFTemp = GetCurveIndex( Alphas( 8 ) );
				if ( DXCoil( DXCoilNum ).HCOPFTemp == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 8 ) + "=\"" + Alphas( 8 ) + "\"." );
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal types are BiQuadratic or Cubic
					{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).HCOPFTemp ) );

						if ( SELECT_CASE_var == "BIQUADRATIC" ) {
							DXCoil( DXCoilNum ).HCOPFTempCurveType = BiQuadratic;
							HeatCOPFTemp = CurveValue( DXCoil( DXCoilNum ).HCOPFTemp, InletAirTemp, InletWaterTemp );

						} else if ( SELECT_CASE_var == "CUBIC" ) {
							DXCoil( DXCoilNum ).HCOPFTempCurveType = Cubic;
							HeatCOPFTemp = CurveValue( DXCoil( DXCoilNum ).HCOPFTemp, InletAirTemp );

						} else {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
							ShowContinueError( "...illegal " + cAlphaFields( 8 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).HCOPFTemp ) );
							ShowContinueError( "Curve type must be BiQuadratic or Cubic." );
							ErrorsFound = true;
							HeatCOPFTemp = 1.0;
						}}

					if ( std::abs( HeatCOPFTemp - 1.0 ) > 0.05 ) {
						ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\":" );
						// could remove name from the field for output
						ShowContinueError( "...The " + cAlphaFields( 8 ) + " should be normalized to 1.0 at the rating point." );
						ShowContinueError( "...Curve output at an air flow fraction of 1 = " + TrimSigDigits( HeatCOPFTemp, 3 ) );
						ShowContinueError( "...The simulation continues using the user-specified curve." );
					}

				}
			}

			if ( ! lAlphaBlanks( 9 ) ) {
				DXCoil( DXCoilNum ).HCOPFAirFlow = GetCurveIndex( Alphas( 9 ) );
				if ( DXCoil( DXCoilNum ).HCOPFAirFlow == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 9 ) + "=\"" + Alphas( 9 ) + "\"." );
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal types are Cubic or Quadratic
					{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).HCOPFAirFlow ) );

						if ( SELECT_CASE_var == "CUBIC" ) {
							HeatCOPFAirFlow = CurveValue( DXCoil( DXCoilNum ).HCOPFAirFlow, 1.0 );

						} else if ( SELECT_CASE_var == "QUADRATIC" ) {
							HeatCOPFAirFlow = CurveValue( DXCoil( DXCoilNum ).HCOPFAirFlow, 1.0 );

						} else {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
							ShowContinueError( "...illegal " + cAlphaFields( 9 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).HCOPFAirFlow ) );
							ShowContinueError( "Curve type must be Quadratic or Cubic." );
							ErrorsFound = true;
							HeatCOPFAirFlow = 1.0;
						}}

					if ( std::abs( HeatCOPFAirFlow - 1.0 ) > 0.05 ) {
						ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\":" );
						// could remove name from the field for output
						ShowContinueError( "...The " + cAlphaFields( 9 ) + " should be normalized to 1.0 at the rating point." );
						ShowContinueError( "...Curve output at an air flow fraction of 1 = " + TrimSigDigits( HeatCOPFAirFlow, 3 ) );
						ShowContinueError( "...The simulation continues using the user-specified curve." );
					}

				}
			}

			if ( ! lAlphaBlanks( 10 ) ) {
				DXCoil( DXCoilNum ).PLFFPLR( 1 ) = GetCurveIndex( Alphas( 10 ) );
				if ( DXCoil( DXCoilNum ).PLFFPLR( 1 ) == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 10 ) + "=\"" + Alphas( 10 ) + "\"." );
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal types are Cubic or Quadratic
					{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).PLFFPLR( 1 ) ) );

						if ( SELECT_CASE_var == "CUBIC" ) {

						} else if ( SELECT_CASE_var == "QUADRATIC" ) {

						} else {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
							ShowContinueError( "...illegal " + cAlphaFields( 10 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).PLFFPLR( 1 ) ) );
							ShowContinueError( "Curve type must be Quadratic or Cubic." );
							ErrorsFound = true;
						}}

					if ( ! ErrorsFound ) {
						//       Test PLF curve minimum and maximum. Cap if less than 0.7 or greater than 1.0.
						MinCurveVal = 999.0;
						MaxCurveVal = -999.0;
						CurveInput = 0.0;
						while ( CurveInput <= 1.0 ) {
							CurveVal = CurveValue( DXCoil( DXCoilNum ).PLFFPLR( 1 ), CurveInput );
							if ( CurveVal < MinCurveVal ) {
								MinCurveVal = CurveVal;
								MinCurvePLR = CurveInput;
							}
							if ( CurveVal > MaxCurveVal ) {
								MaxCurveVal = CurveVal;
								MaxCurvePLR = CurveInput;
							}
							CurveInput += 0.01;
						}
						if ( MinCurveVal < 0.7 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
							ShowContinueError( "..." + cAlphaFields( 10 ) + " = " + Alphas( 10 ) + " has out of range value." );
							ShowContinueError( "...Curve minimum must be >= 0.7, curve min at PLR = " + TrimSigDigits( MinCurvePLR, 2 ) + " is " + TrimSigDigits( MinCurveVal, 3 ) );
							ShowContinueError( "...Setting curve minimum to 0.7 and simulation continues." );
							SetCurveOutputMinMaxValues( DXCoil( DXCoilNum ).PLFFPLR( 1 ), ErrorsFound, 0.7, _ );
						}

						if ( MaxCurveVal > 1.0 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
							ShowContinueError( "..." + cAlphaFields( 10 ) + " = " + Alphas( 10 ) + " has out of range value." );
							ShowContinueError( "...Curve maximum must be <= 1.0, curve max at PLR = " + TrimSigDigits( MaxCurvePLR, 2 ) + " is " + TrimSigDigits( MaxCurveVal, 3 ) );
							ShowContinueError( "...Setting curve maximum to 1.0 and simulation continues." );
							SetCurveOutputMinMaxValues( DXCoil( DXCoilNum ).PLFFPLR( 1 ), ErrorsFound, _, 1.0 );
						}

					}

				}
			}

			// assume compressor resides at the inlet to the DX Coil
			DXCoil( DXCoilNum ).CondenserInletNodeNum( 1 ) = DXCoil( DXCoilNum ).AirInNode;

			// set condenser type as HPWH
			DXCoil( DXCoilNum ).CondenserType( 1 ) = WaterHeater;

		} // end of the DX water heater wrapped coil loop

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in getting " + CurrentModuleObject + " input. Preceding condition(s) causes termination." );
		}

		// DX Multispeed cooling coil
		CurrentModuleObject = "Coil:Cooling:DX:MultiSpeed";
		for ( DXCoilIndex = 1; DXCoilIndex <= NumDXMulSpeedCoolCoils; ++DXCoilIndex ) {

			++DXCoilNum;

			GetObjectItem( CurrentModuleObject, DXCoilIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			// allocate single performance mode for numeric field strings used for sizing routine (all fields are in this object)
			DXCoilNumericFields ( DXCoilNum ).PerfMode.allocate ( 1 );
			DXCoilNumericFields( DXCoilNum ).PerfMode( 1 ).FieldNames.allocate ( MaxNumbers );
			DXCoilNumericFields ( DXCoilNum ).PerfMode ( 1 ).FieldNames = cNumericFields;

			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), DXCoil, DXCoilNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			VerifyUniqueCoilName( CurrentModuleObject, Alphas( 1 ), errFlag, CurrentModuleObject + " Name" );
			if ( errFlag ) {
				ErrorsFound = true;
			}
			DXCoil( DXCoilNum ).Name = Alphas( 1 );
			// Initialize DataHeatBalance heat reclaim variable name for use by heat reclaim coils
			HeatReclaimDXCoil( DXCoilNum ).Name = DXCoil( DXCoilNum ).Name;
			HeatReclaimDXCoil( DXCoilNum ).SourceType = CurrentModuleObject;
			DXCoil( DXCoilNum ).DXCoilType = CurrentModuleObject;
			DXCoil( DXCoilNum ).DXCoilType_Num = CoilDX_MultiSpeedCooling;
			DXCoil( DXCoilNum ).Schedule = Alphas( 2 );
			if ( lAlphaBlanks( 2 ) ) {
				DXCoil( DXCoilNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				DXCoil( DXCoilNum ).SchedPtr = GetScheduleIndex( Alphas( 2 ) ); // convert schedule name to pointer
				if ( DXCoil( DXCoilNum ).SchedPtr == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "..." + cAlphaFields( 2 ) + "=\"" + Alphas( 2 ) + "\"." );
					ErrorsFound = true;
				}
			}

			DXCoil( DXCoilNum ).AirInNode = GetOnlySingleNode( Alphas( 3 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

			DXCoil( DXCoilNum ).AirOutNode = GetOnlySingleNode( Alphas( 4 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			TestCompSet( CurrentModuleObject, Alphas( 1 ), Alphas( 3 ), Alphas( 4 ), "Air Nodes" );

			// outdoor condenser node
			if ( lAlphaBlanks( 5 ) ) {
				DXCoil( DXCoilNum ).CondenserInletNodeNum( 1 ) = 0;
			} else {
				DXCoil( DXCoilNum ).CondenserInletNodeNum( 1 ) = GetOnlySingleNode( Alphas( 5 ), ErrorsFound, CurrentModuleObject, DXCoil( DXCoilNum ).Name, NodeType_Air, NodeConnectionType_OutsideAirReference, 1, ObjectIsNotParent );
				if ( ! CheckOutAirNodeNumber( DXCoil( DXCoilNum ).CondenserInletNodeNum( 1 ) ) ) {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", may be invalid" );
					ShowContinueError( cAlphaFields( 5 ) + "=\"" + Alphas( 5 ) + "\", node does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node." );
					ShowContinueError( "This node needs to be included in an air system or the coil model will not be valid, and the simulation continues" );
				}
			}

			if ( ( SameString( Alphas( 6 ), "AirCooled" ) ) || lAlphaBlanks( 6 ) ) {
				DXCoil( DXCoilNum ).CondenserType( 1 ) = AirCooled;
			} else if ( SameString( Alphas( 6 ), "EvaporativelyCooled" ) ) {
				DXCoil( DXCoilNum ).CondenserType( 1 ) = EvapCooled;
				DXCoil( DXCoilNum ).ReportEvapCondVars = true;
			} else {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cAlphaFields( 6 ) + "=\"" + Alphas( 6 ) + "\":" );
				ShowContinueError( "...must be AirCooled or EvaporativelyCooled." );
				ErrorsFound = true;
			}

			// Get Water System tank connections
			//  A8, \field Name of Water Storage Tank for Supply
			DXCoil( DXCoilNum ).EvapWaterSupplyName = Alphas( 7 );
			if ( lAlphaBlanks( 7 ) ) {
				DXCoil( DXCoilNum ).EvapWaterSupplyMode = WaterSupplyFromMains;
			} else {
				DXCoil( DXCoilNum ).EvapWaterSupplyMode = WaterSupplyFromTank;
				SetupTankDemandComponent( DXCoil( DXCoilNum ).Name, CurrentModuleObject, DXCoil( DXCoilNum ).EvapWaterSupplyName, ErrorsFound, DXCoil( DXCoilNum ).EvapWaterSupTankID, DXCoil( DXCoilNum ).EvapWaterTankDemandARRID );
			}

			//A9; \field Name of Water Storage Tank for Condensate Collection
			DXCoil( DXCoilNum ).CondensateCollectName = Alphas( 8 );
			if ( lAlphaBlanks( 8 ) ) {
				DXCoil( DXCoilNum ).CondensateCollectMode = CondensateDiscarded;
			} else {
				DXCoil( DXCoilNum ).CondensateCollectMode = CondensateToTank;
				SetupTankSupplyComponent( DXCoil( DXCoilNum ).Name, CurrentModuleObject, DXCoil( DXCoilNum ).CondensateCollectName, ErrorsFound, DXCoil( DXCoilNum ).CondensateTankID, DXCoil( DXCoilNum ).CondensateTankSupplyARRID );
			}

			//Set crankcase heater capacity
			DXCoil( DXCoilNum ).CrankcaseHeaterCapacity = Numbers( 1 );
			if ( DXCoil( DXCoilNum ).CrankcaseHeaterCapacity < 0.0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 1 ) + " cannot be < 0.0." );
				ShowContinueError( "...entered value=[" + TrimSigDigits( Numbers( 1 ), 2 ) + "]." );
				ErrorsFound = true;
			}

			//Set crankcase heater cutout temperature
			DXCoil( DXCoilNum ).MaxOATCrankcaseHeater = Numbers( 2 );

			if ( SameString( Alphas( 9 ), "Yes" ) ) {
				DXCoil( DXCoilNum ).PLRImpact = true;
			} else if ( SameString( Alphas( 9 ), "No" ) ) {
				DXCoil( DXCoilNum ).PLRImpact = false;
			} else {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( ",,,invalid choice for " + cAlphaFields( 9 ) + ".  Entered choice = " + Alphas( 9 ) );
				ShowContinueError( "The allowed choices are Yes or No." );
				ErrorsFound = true;
			}

			if ( SameString( Alphas( 10 ), "Yes" ) ) {
				DXCoil( DXCoilNum ).LatentImpact = true;
			} else if ( SameString( Alphas( 10 ), "No" ) ) {
				DXCoil( DXCoilNum ).LatentImpact = false;
			} else {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( ",,,invalid choice for " + cAlphaFields( 10 ) + ".  Entered choice = " + Alphas( 10 ) );
				ShowContinueError( "The allowed choices are Yes or No." );
				ErrorsFound = true;
			}

			//   Basin heater power as a function of temperature must be greater than or equal to 0
			DXCoil( DXCoilNum ).BasinHeaterPowerFTempDiff = Numbers( 3 );
			if ( Numbers( 3 ) < 0.0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 3 ) + " must be >= 0.0, entered value=[" + TrimSigDigits( Numbers( 3 ), 3 ) + "]." );
				ErrorsFound = true;
			}

			DXCoil( DXCoilNum ).BasinHeaterSetPointTemp = Numbers( 4 );
			if ( DXCoil( DXCoilNum ).BasinHeaterPowerFTempDiff > 0.0 ) {
				if ( NumNumbers < 4 ) {
					DXCoil( DXCoilNum ).BasinHeaterSetPointTemp = 2.0;
				}
				if ( DXCoil( DXCoilNum ).BasinHeaterSetPointTemp < 2.0 ) {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", freeze possible" );
					ShowContinueError( "..." + cNumericFields( 4 ) + " is less than 2 {C}. Freezing could occur." );
					ShowContinueError( "...entered value=[" + TrimSigDigits( Numbers( 4 ), 2 ) + "]." );
				}
			}

			if ( ! lAlphaBlanks( 11 ) ) {
				DXCoil( DXCoilNum ).BasinHeaterSchedulePtr = GetScheduleIndex( Alphas( 11 ) );
				if ( DXCoil( DXCoilNum ).BasinHeaterSchedulePtr == 0 ) {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 11 ) + "=\"" + Alphas( 11 ) + "\"." );
					ShowContinueError( "Basin heater will be available to operate throughout the simulation." );
				}
			}

			//A12; \field Fuel type
			if ( SameString( Alphas( 12 ), "Electricity" ) ) {
				DXCoil( DXCoilNum ).FuelType = FuelTypeElectricity;
			} else if ( SameString( Alphas( 12 ), "NaturalGas" ) ) {
				DXCoil( DXCoilNum ).FuelType = FuelTypeNaturalGas;
			} else if ( SameString( Alphas( 12 ), "PropaneGas" ) ) {
				DXCoil( DXCoilNum ).FuelType = FuelTypePropaneGas;
			} else if ( SameString( Alphas( 12 ), "Diesel" ) ) {
				DXCoil( DXCoilNum ).FuelType = FuelTypeDiesel;
			} else if ( SameString( Alphas( 12 ), "Gasoline" ) ) {
				DXCoil( DXCoilNum ).FuelType = FuelTypeGasoline;
			} else if ( SameString( Alphas( 12 ), "FuelOil#1" ) ) {
				DXCoil( DXCoilNum ).FuelType = FuelTypeFuelOil1;
			} else if ( SameString( Alphas( 12 ), "FuelOil#2" ) ) {
				DXCoil( DXCoilNum ).FuelType = FuelTypeFuelOil2;
			} else if ( SameString( Alphas( 12 ), "OtherFuel1" ) ) {
				DXCoil( DXCoilNum ).FuelType = FuelTypeOtherFuel1;
			} else if ( SameString( Alphas( 12 ), "OtherFuel2" ) ) {
				DXCoil( DXCoilNum ).FuelType = FuelTypeOtherFuel2;
			} else {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( ",,,invalid choice for " + cAlphaFields( 12 ) + ".  Entered choice = " + Alphas( 12 ) );
				ShowContinueError( "Valid choices are Electricity, NaturalGas, PropaneGas, Diesel, Gasoline, FuelOil#1, FuelOil#2, OtherFuel1 or OtherFuel2" );
				ErrorsFound = true;
			}

			DXCoil( DXCoilNum ).NumOfSpeeds = Numbers( 5 ); // Number of speeds
			if ( DXCoil( DXCoilNum ).NumOfSpeeds < 2 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 5 ) + " must be >= 2. entered number is " + TrimSigDigits( Numbers( 5 ), 0 ) );
				ErrorsFound = true;
			}

			// Allocate arrays based on the number of speeds
			DXCoil( DXCoilNum ).MSErrIndex.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
			DXCoil( DXCoilNum ).MSErrIndex = 0;
			DXCoil( DXCoilNum ).MSRatedTotCap.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
			DXCoil( DXCoilNum ).MSRatedSHR.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
			DXCoil( DXCoilNum ).MSRatedCOP.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
			DXCoil( DXCoilNum ).MSRatedAirVolFlowRate.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
			DXCoil( DXCoilNum ).MSRatedAirMassFlowRate.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
			DXCoil( DXCoilNum ).MSCCapFTemp.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
			DXCoil( DXCoilNum ).MSCCapFFlow.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
			DXCoil( DXCoilNum ).MSEIRFTemp.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
			DXCoil( DXCoilNum ).MSEIRFFlow.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
			DXCoil( DXCoilNum ).MSWasteHeat.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
			DXCoil( DXCoilNum ).MSEvapCondEffect.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
			DXCoil( DXCoilNum ).MSEvapCondAirFlow.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
			DXCoil( DXCoilNum ).MSEvapCondPumpElecNomPower.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
			DXCoil( DXCoilNum ).MSRatedCBF.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
			DXCoil( DXCoilNum ).MSWasteHeatFrac.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
			DXCoil( DXCoilNum ).MSPLFFPLR.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
			DXCoil( DXCoilNum ).MSTwet_Rated.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
			DXCoil( DXCoilNum ).MSGamma_Rated.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
			DXCoil( DXCoilNum ).MSMaxONOFFCyclesperHour.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
			DXCoil( DXCoilNum ).MSLatentCapacityTimeConstant.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
			DXCoil( DXCoilNum ).MSFanPowerPerEvapAirFlowRate.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );

			for ( I = 1; I <= DXCoil( DXCoilNum ).NumOfSpeeds; ++I ) {
				DXCoil( DXCoilNum ).MSRatedTotCap( I ) = Numbers( 6 + ( I - 1 ) * 13 );
				DXCoil( DXCoilNum ).MSRatedSHR( I ) = Numbers( 7 + ( I - 1 ) * 13 );
				DXCoil( DXCoilNum ).MSRatedCOP( I ) = Numbers( 8 + ( I - 1 ) * 13 );
				DXCoil( DXCoilNum ).MSRatedAirVolFlowRate( I ) = Numbers( 9 + ( I - 1 ) * 13 );
				DXCoil( DXCoilNum ).MSFanPowerPerEvapAirFlowRate( I ) = Numbers( 10 + ( I - 1 ) * 13 );

				DXCoil( DXCoilNum ).MSCCapFTemp( I ) = GetCurveIndex( Alphas( 13 + ( I - 1 ) * 6 ) ); // convert curve name to number
				if ( DXCoil( DXCoilNum ).MSCCapFTemp( I ) == 0 ) {
					if ( lAlphaBlanks( 13 + ( I - 1 ) * 6 ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", missing" );
						ShowContinueError( "...required " + cAlphaFields( 13 + ( I - 1 ) * 6 ) + " is blank." );
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...not found " + cAlphaFields( 13 + ( I - 1 ) * 6 ) + "=\"" + Alphas( 13 + ( I - 1 ) * 6 ) + "\"." );
					}
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal type is BiQuadratic
					{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).MSCCapFTemp( I ) ) );

					if ( SELECT_CASE_var == "BIQUADRATIC" ) {
						CurveVal = CurveValue( DXCoil( DXCoilNum ).MSCCapFTemp( I ), RatedInletWetBulbTemp, RatedOutdoorAirTemp );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( 13 + ( I - 1 ) * 6 ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( 13 + ( I - 1 ) * 6 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).MSCCapFTemp( I ) ) );
						ShowContinueError( "Curve type must be BiQuadratic." );
						ErrorsFound = true;
					}}
				}

				DXCoil( DXCoilNum ).MSCCapFFlow( I ) = GetCurveIndex( Alphas( 14 + ( I - 1 ) * 6 ) ); // convert curve name to number
				if ( DXCoil( DXCoilNum ).MSCCapFFlow( I ) == 0 ) {
					if ( lAlphaBlanks( 14 + ( I - 1 ) * 6 ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", missing" );
						ShowContinueError( "...required " + cAlphaFields( 14 + ( I - 1 ) * 6 ) + " is blank." );
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...not found " + cAlphaFields( 14 + ( I - 1 ) * 6 ) + "=\"" + Alphas( 14 + ( I - 1 ) * 6 ) + "\"." );
					}
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal type is Quadratic
					{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).MSCCapFFlow( I ) ) );

					if ( SELECT_CASE_var == "QUADRATIC" ) {
						CurveVal = CurveValue( DXCoil( DXCoilNum ).MSCCapFFlow( I ), 1.0 );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( 14 + ( I - 1 ) * 6 ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else if ( SELECT_CASE_var == "CUBIC" ) {
						CurveVal = CurveValue( DXCoil( DXCoilNum ).MSCCapFFlow( I ), 1.0 );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( 14 + ( I - 1 ) * 6 ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( 14 + ( I - 1 ) * 6 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).MSCCapFFlow( I ) ) );
						ShowContinueError( "Curve type must be Quadratic or Cubic." );
						ErrorsFound = true;
					}}
				}

				DXCoil( DXCoilNum ).MSEIRFTemp( I ) = GetCurveIndex( Alphas( 15 + ( I - 1 ) * 6 ) ); // convert curve name to number
				if ( DXCoil( DXCoilNum ).MSEIRFTemp( I ) == 0 ) {
					if ( lAlphaBlanks( 15 + ( I - 1 ) * 6 ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", missing" );
						ShowContinueError( "...required " + cAlphaFields( 15 + ( I - 1 ) * 6 ) + " is blank." );
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...not found " + cAlphaFields( 15 + ( I - 1 ) * 6 ) + "=\"" + Alphas( 15 + ( I - 1 ) * 6 ) + "\"." );
					}
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal type is BiQuadratic
					{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).MSEIRFTemp( I ) ) );

					if ( SELECT_CASE_var == "BIQUADRATIC" ) {
						CurveVal = CurveValue( DXCoil( DXCoilNum ).MSEIRFTemp( I ), RatedInletWetBulbTemp, RatedOutdoorAirTemp );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( 15 + ( I - 1 ) * 6 ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( 15 + ( I - 1 ) * 6 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).MSEIRFTemp( 1 ) ) );
						ShowContinueError( "Curve type must be BiQuadratic." );
						ErrorsFound = true;
					}}
				}

				DXCoil( DXCoilNum ).MSEIRFFlow( I ) = GetCurveIndex( Alphas( 16 + ( I - 1 ) * 6 ) ); // convert curve name to number
				if ( DXCoil( DXCoilNum ).MSEIRFFlow( I ) == 0 ) {
					if ( lAlphaBlanks( 16 + ( I - 1 ) * 6 ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", missing" );
						ShowContinueError( "...required " + cAlphaFields( 16 + ( I - 1 ) * 6 ) + " is blank." );
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...not found " + cAlphaFields( 16 + ( I - 1 ) * 6 ) + "=\"" + Alphas( 16 + ( I - 1 ) * 6 ) + "\"." );
					}
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal type is Quadratic
					{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).MSEIRFFlow( I ) ) );

					if ( SELECT_CASE_var == "QUADRATIC" ) {
						CurveVal = CurveValue( DXCoil( DXCoilNum ).MSEIRFFlow( I ), 1.0 );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( 16 + ( I - 1 ) * 6 ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else if ( SELECT_CASE_var == "CUBIC" ) {
						CurveVal = CurveValue( DXCoil( DXCoilNum ).MSEIRFFlow( I ), 1.0 );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( 16 + ( I - 1 ) * 6 ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( 16 + ( I - 1 ) * 6 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).MSEIRFFlow( I ) ) );
						ShowContinueError( "Curve type must be Quadratic or Cubic." );
						ErrorsFound = true;
					}}
				}

				DXCoil( DXCoilNum ).MSPLFFPLR( I ) = GetCurveIndex( Alphas( 17 + ( I - 1 ) * 6 ) ); // convert curve name to number
				if ( DXCoil( DXCoilNum ).MSPLFFPLR( I ) == 0 ) {
					if ( lAlphaBlanks( 17 + ( I - 1 ) * 6 ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", missing" );
						ShowContinueError( "...required " + cAlphaFields( 17 + ( I - 1 ) * 6 ) + " is blank." );
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...not found " + cAlphaFields( 17 + ( I - 1 ) * 6 ) + "=\"" + Alphas( 16 + ( I - 1 ) * 6 ) + "\"." );
					}
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal types are Quadratic or Cubic
					{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).MSPLFFPLR( I ) ) );

					if ( SELECT_CASE_var == "QUADRATIC" ) {

					} else if ( SELECT_CASE_var == "CUBIC" ) {

					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( 17 + ( I - 1 ) * 6 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).MSPLFFPLR( I ) ) );
						ShowContinueError( "Curve type must be Quadratic or Cubic." );
						ErrorsFound = true;
					}}

					if ( ! ErrorsFound ) {
						//       Test PLF curve minimum and maximum. Cap if less than 0.7 or greater than 1.0.
						MinCurveVal = 999.0;
						MaxCurveVal = -999.0;
						CurveInput = 0.0;
						while ( CurveInput <= 1.0 ) {
							CurveVal = CurveValue( DXCoil( DXCoilNum ).MSPLFFPLR( I ), CurveInput );
							if ( CurveVal < MinCurveVal ) {
								MinCurveVal = CurveVal;
								MinCurvePLR = CurveInput;
							}
							if ( CurveVal > MaxCurveVal ) {
								MaxCurveVal = CurveVal;
								MaxCurvePLR = CurveInput;
							}
							CurveInput += 0.01;
						}
						if ( MinCurveVal < 0.7 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
							ShowContinueError( "..." + cAlphaFields2( 17 + ( I - 1 ) * 6 ) + " = " + Alphas2( 17 + ( I - 1 ) * 6 ) + " has out of range value." );
							ShowContinueError( "...Curve minimum must be >= 0.7, curve min at PLR = " + TrimSigDigits( MinCurvePLR, 2 ) + " is " + TrimSigDigits( MinCurveVal, 3 ) );
							ShowContinueError( "...Setting curve minimum to 0.7 and simulation continues." );
							SetCurveOutputMinMaxValues( DXCoil( DXCoilNum ).PLFFPLR( PerfModeNum ), ErrorsFound, 0.7, _ );
						}

						if ( MaxCurveVal > 1.0 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
							ShowContinueError( "..." + cAlphaFields2( 17 + ( I - 1 ) * 6 ) + " = " + Alphas2( 17 + ( I - 1 ) * 6 ) + " has out of range value." );
							ShowContinueError( "...Curve maximum must be <= 1.0, curve max at PLR = " + TrimSigDigits( MaxCurvePLR, 2 ) + " is " + TrimSigDigits( MaxCurveVal, 3 ) );
							ShowContinueError( "...Setting curve maximum to 1.0 and simulation continues." );
							SetCurveOutputMinMaxValues( DXCoil( DXCoilNum ).MSPLFFPLR( I ), ErrorsFound, _, 1.0 );
						}

					}

				}

				// read data for latent degradation
				DXCoil( DXCoilNum ).MSTwet_Rated( I ) = Numbers( 11 + ( I - 1 ) * 13 );
				if ( DXCoil( DXCoilNum ).MSTwet_Rated( I ) < 0.0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "..." + cNumericFields( 11 + ( I - 1 ) * 13 ) + " cannot be < 0.0, entered value=[" + TrimSigDigits( DXCoil( DXCoilNum ).MSTwet_Rated( I ), 4 ) + "]." );
					ErrorsFound = true;
				}
				DXCoil( DXCoilNum ).MSGamma_Rated( I ) = Numbers( 12 + ( I - 1 ) * 13 );
				if ( DXCoil( DXCoilNum ).MSGamma_Rated( I ) < 0.0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "..." + cNumericFields( 12 + ( I - 1 ) * 13 ) + " cannot be < 0.0, entered value=[" + TrimSigDigits( DXCoil( DXCoilNum ).MSGamma_Rated( I ), 4 ) + "]." );
					ErrorsFound = true;
				}
				DXCoil( DXCoilNum ).MSMaxONOFFCyclesperHour( I ) = Numbers( 13 + ( I - 1 ) * 13 );
				if ( DXCoil( DXCoilNum ).Gamma_Rated( I ) < 0.0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "..." + cNumericFields( 13 + ( I - 1 ) * 13 ) + " cannot be < 0.0, entered value=[" + TrimSigDigits( DXCoil( DXCoilNum ).MSMaxONOFFCyclesperHour( I ), 2 ) + "]." );
					ErrorsFound = true;
				}
				DXCoil( DXCoilNum ).MSLatentCapacityTimeConstant( I ) = Numbers( 14 + ( I - 1 ) * 13 );
				if ( DXCoil( DXCoilNum ).Gamma_Rated( I ) < 0.0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "..." + cNumericFields( 14 + ( I - 1 ) * 13 ) + " cannot be < 0.0, entered value=[" + TrimSigDigits( DXCoil( DXCoilNum ).MSLatentCapacityTimeConstant( I ), 2 ) + "]." );
					ErrorsFound = true;
				}

				DXCoil( DXCoilNum ).MSWasteHeatFrac( I ) = Numbers( 15 + ( I - 1 ) * 13 );

				// Read waste heat modifier curve name
				DXCoil( DXCoilNum ).MSWasteHeat( I ) = GetCurveIndex( Alphas( 18 + ( I - 1 ) * 6 ) ); // convert curve name to number
				if ( DXCoil( DXCoilNum ).FuelType != FuelTypeElectricity ) {
					if ( DXCoil( DXCoilNum ).MSWasteHeat( I ) > 0 ) {
						// Verify Curve Object, only legal types are BiQuadratic
						{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).MSWasteHeat( I ) ) );

						if ( SELECT_CASE_var == "BIQUADRATIC" ) {
							CurveVal = CurveValue( DXCoil( DXCoilNum ).MSWasteHeat( I ), RatedOutdoorAirTemp, RatedInletAirTemp );
							if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
								ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", curve values" );
								ShowContinueError( "..." + cAlphaFields( 18 + ( I - 1 ) * 6 ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
								ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
							}

						} else {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
							ShowContinueError( "...illegal " + cAlphaFields( 18 + ( I - 1 ) * 6 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).MSWasteHeat( I ) ) );
							ShowContinueError( "Curve type must be BiQuadratic." );
							ErrorsFound = true;
						}}
					}
				}

				DXCoil( DXCoilNum ).MSEvapCondEffect( I ) = Numbers( 16 + ( I - 1 ) * 13 );
				if ( DXCoil( DXCoilNum ).MSEvapCondEffect( I ) < 0.0 || DXCoil( DXCoilNum ).MSEvapCondEffect( I ) > 1.0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "..." + cNumericFields( 16 + ( I - 1 ) * 13 ) + " cannot be < 0.0 or > 1.0, entered value=[" + TrimSigDigits( Numbers( 16 + ( I - 1 ) * 13 ), 3 ) + "]." );
					ErrorsFound = true;
				}

				DXCoil( DXCoilNum ).MSEvapCondAirFlow( I ) = Numbers( 17 + ( I - 1 ) * 13 );
				if ( DXCoil( DXCoilNum ).MSEvapCondAirFlow( I ) < 0.0 && DXCoil( DXCoilNum ).MSEvapCondAirFlow( I ) != AutoSize ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "..." + cNumericFields( 17 + ( I - 1 ) * 13 ) + " cannot be < 0.0, entered value=[" + TrimSigDigits( Numbers( 17 + ( I - 1 ) * 13 ), 3 ) + "]." );
					ErrorsFound = true;
				}

				DXCoil( DXCoilNum ).MSEvapCondPumpElecNomPower( I ) = Numbers( 18 + ( I - 1 ) * 13 );
				if ( DXCoil( DXCoilNum ).MSEvapCondPumpElecNomPower( I ) < 0.0 && DXCoil( DXCoilNum ).MSEvapCondPumpElecNomPower( I ) != AutoSize ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "..." + cNumericFields( 18 + ( I - 1 ) * 13 ) + " cannot be < 0.0, entered value=[" + TrimSigDigits( Numbers( 18 + ( I - 1 ) * 13 ), 3 ) + "]." );
					ErrorsFound = true;
				}

			}
			//A37; \field Zone Name for Condenser Placement
			if ( !lAlphaBlanks( 37 ) && NumAlphas > 36 ) {
				DXCoil( DXCoilNum ).SecZonePtr = FindItemInList( Alphas( 37 ), Zone );
				if ( DXCoil( DXCoilNum ).SecZonePtr > 0 ) {
					SetupZoneInternalGain( DXCoil( DXCoilNum ).SecZonePtr, "Coil:Cooling:DX:MultiSpeed", DXCoil( DXCoilNum ).Name, IntGainTypeOf_SecCoolingDXCoilMultiSpeed, DXCoil( DXCoilNum ).SecCoilSensibleHeatGainRate );
					DXCoil( DXCoilNum ).IsSecondaryDXCoilInZone = true;
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 37 ) + "=\"" + Alphas( 37 ) + "\"." );
				}
			}
		}

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in getting " + CurrentModuleObject + " input. Preceding condition(s) causes termination." );
		}

		// DX multispeed heating coil
		CurrentModuleObject = "Coil:Heating:DX:MultiSpeed";
		for ( DXCoilIndex = 1; DXCoilIndex <= NumDXMulSpeedHeatCoils; ++DXCoilIndex ) {

			++DXCoilNum;

			GetObjectItem( CurrentModuleObject, DXCoilIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			// *** will have to circle back to this one to fix since the multispeed coil has all fields in this coil object ***
			// allocate single performance mode for numeric field strings used for sizing routine
			DXCoilNumericFields ( DXCoilNum ).PerfMode.allocate ( 1 );
			DXCoilNumericFields( DXCoilNum ).PerfMode( 1 ).FieldNames.allocate ( MaxNumbers );
			DXCoilNumericFields ( DXCoilNum ).PerfMode ( 1 ).FieldNames = cNumericFields;

			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), DXCoil, DXCoilNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			VerifyUniqueCoilName( CurrentModuleObject, Alphas( 1 ), errFlag, CurrentModuleObject + " Name" );
			if ( errFlag ) {
				ErrorsFound = true;
			}
			DXCoil( DXCoilNum ).Name = Alphas( 1 );
			// Initialize DataHeatBalance heat reclaim variable name for use by heat reclaim coils
			HeatReclaimDXCoil( DXCoilNum ).Name = DXCoil( DXCoilNum ).Name;
			HeatReclaimDXCoil( DXCoilNum ).SourceType = CurrentModuleObject;
			DXCoil( DXCoilNum ).DXCoilType = CurrentModuleObject;
			DXCoil( DXCoilNum ).DXCoilType_Num = CoilDX_MultiSpeedHeating;
			DXCoil( DXCoilNum ).Schedule = Alphas( 2 );
			if ( lAlphaBlanks( 2 ) ) {
				DXCoil( DXCoilNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				DXCoil( DXCoilNum ).SchedPtr = GetScheduleIndex( Alphas( 2 ) ); // convert schedule name to pointer
				if ( DXCoil( DXCoilNum ).SchedPtr == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "..." + cAlphaFields( 2 ) + "=\"" + Alphas( 2 ) + "\"." );
					ErrorsFound = true;
				}
			}

			DXCoil( DXCoilNum ).AirInNode = GetOnlySingleNode( Alphas( 3 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

			DXCoil( DXCoilNum ).AirOutNode = GetOnlySingleNode( Alphas( 4 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			TestCompSet( CurrentModuleObject, Alphas( 1 ), Alphas( 3 ), Alphas( 4 ), "Air Nodes" );

			//Set minimum OAT for heat pump compressor operation
			DXCoil( DXCoilNum ).MinOATCompressor = Numbers( 1 );

			// set Minimum Outdoor Dry-Bulb Temperature for Compressor Operation
			DXCoil( DXCoilNum ).OATempCompressorOn = Numbers( 2 );
			//Set crankcase heater capacity
			DXCoil( DXCoilNum ).CrankcaseHeaterCapacity = Numbers( 3 );
			if ( DXCoil( DXCoilNum ).CrankcaseHeaterCapacity < 0.0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 3 ) + " cannot be < 0.0, entered value=[" + TrimSigDigits( Numbers( 3 ), 2 ) + "]." );
				ErrorsFound = true;
			}

			//Set crankcase heater cutout temperature
			DXCoil( DXCoilNum ).MaxOATCrankcaseHeater = Numbers( 4 );

			// Only required for reverse cycle heat pumps
			DXCoil( DXCoilNum ).DefrostEIRFT = GetCurveIndex( Alphas( 5 ) ); // convert curve name to number
			if ( SameString( Alphas( 6 ), "ReverseCycle" ) ) {
				if ( DXCoil( DXCoilNum ).DefrostEIRFT == 0 ) {
					if ( lAlphaBlanks( 5 ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", missing" );
						ShowContinueError( "...required " + cAlphaFields( 5 ) + " is blank." );
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...not found " + cAlphaFields( 5 ) + "=\"" + Alphas( 5 ) + "\"." );
					}
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal type is BiQuadratic
					{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).DefrostEIRFT ) );
					if ( SELECT_CASE_var == "BIQUADRATIC" ) {
						CurveVal = CurveValue( DXCoil( DXCoilNum ).DefrostEIRFT, RatedInletWetBulbTempHeat, RatedOutdoorAirTempHeat );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( 5 ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( 5 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).DefrostEIRFT ) );
						ShowContinueError( "Curve type must be BiQuadratic." );
						ErrorsFound = true;
					}}
				}
			}

			if ( SameString( Alphas( 6 ), "ReverseCycle" ) ) DXCoil( DXCoilNum ).DefrostStrategy = ReverseCycle;
			if ( SameString( Alphas( 6 ), "Resistive" ) ) DXCoil( DXCoilNum ).DefrostStrategy = Resistive;
			if ( DXCoil( DXCoilNum ).DefrostStrategy == 0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "...illegal " + cAlphaFields( 6 ) + "=\"" + Alphas( 6 ) + "\"." );
				ShowContinueError( "...valid values for this field are ReverseCycle or Resistive." );
				ErrorsFound = true;
			}

			if ( SameString( Alphas( 7 ), "Timed" ) ) DXCoil( DXCoilNum ).DefrostControl = Timed;
			if ( SameString( Alphas( 7 ), "OnDemand" ) ) DXCoil( DXCoilNum ).DefrostControl = OnDemand;
			if ( DXCoil( DXCoilNum ).DefrostControl == 0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "...illegal " + cAlphaFields( 7 ) + "=\"" + Alphas( 7 ) + "\"." );
				ShowContinueError( "...valid values for this field are Timed or OnDemand." );
				ErrorsFound = true;
			}

			//Set maximum outdoor temp for defrost to occur
			DXCoil( DXCoilNum ).MaxOATDefrost = Numbers( 5 );

			//Set defrost time period
			DXCoil( DXCoilNum ).DefrostTime = Numbers( 6 );
			if ( DXCoil( DXCoilNum ).DefrostTime == 0.0 && DXCoil( DXCoilNum ).DefrostControl == Timed ) {
				ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", " );
				ShowContinueError( "..." + cNumericFields( 5 ) + " = 0.0 for defrost control = TIMED." );
			}

			//Set defrost capacity (for resistive defrost)
			DXCoil( DXCoilNum ).DefrostCapacity = Numbers( 7 );
			if ( DXCoil( DXCoilNum ).DefrostCapacity == 0.0 && DXCoil( DXCoilNum ).DefrostStrategy == OnDemand ) {
				ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", " );
				ShowContinueError( "..." + cNumericFields( 7 ) + " = 0.0 for defrost strategy = RESISTIVE." );
			}

			if ( SameString( Alphas( 8 ), "Yes" ) ) {
				DXCoil( DXCoilNum ).PLRImpact = true;
			} else if ( SameString( Alphas( 8 ), "No" ) ) {
				DXCoil( DXCoilNum ).PLRImpact = false;
			} else {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( ",,,invalid choice for " + cAlphaFields( 8 ) + ".  Entered choice = " + Alphas( 8 ) );
				ShowContinueError( "The allowed choices are Yes or No." );
				ErrorsFound = true;
			}

			//A10; \field Fuel type
			if ( SameString( Alphas( 9 ), "Electricity" ) ) {
				DXCoil( DXCoilNum ).FuelType = FuelTypeElectricity;
			} else if ( SameString( Alphas( 9 ), "NaturalGas" ) ) {
				DXCoil( DXCoilNum ).FuelType = FuelTypeNaturalGas;
			} else if ( SameString( Alphas( 9 ), "PropaneGas" ) ) {
				DXCoil( DXCoilNum ).FuelType = FuelTypePropaneGas;
			} else if ( SameString( Alphas( 9 ), "Diesel" ) ) {
				DXCoil( DXCoilNum ).FuelType = FuelTypeDiesel;
			} else if ( SameString( Alphas( 9 ), "Gasoline" ) ) {
				DXCoil( DXCoilNum ).FuelType = FuelTypeGasoline;
			} else if ( SameString( Alphas( 9 ), "FuelOil#1" ) ) {
				DXCoil( DXCoilNum ).FuelType = FuelTypeFuelOil1;
			} else if ( SameString( Alphas( 9 ), "FuelOil#2" ) ) {
				DXCoil( DXCoilNum ).FuelType = FuelTypeFuelOil2;
			} else if ( SameString( Alphas( 9 ), "OtherFuel1" ) ) {
				DXCoil( DXCoilNum ).FuelType = FuelTypeOtherFuel1;
			} else if ( SameString( Alphas( 9 ), "OtherFuel2" ) ) {
				DXCoil( DXCoilNum ).FuelType = FuelTypeOtherFuel2;
			} else {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( ",,,invalid choice for " + cAlphaFields( 9 ) + ".  Entered choice = " + Alphas( 9 ) );
				ShowContinueError( "Valid choices are Electricity, NaturalGas, PropaneGas, Diesel, Gasoline, FuelOil#1, FuelOil#2, OtherFuel1 or OtherFuel2" );
				ErrorsFound = true;
			}

			DXCoil( DXCoilNum ).RegionNum = Numbers( 8 ); // Region Number for HSPF Calc
			DXCoil( DXCoilNum ).NumOfSpeeds = Numbers( 9 ); // Number of speeds
			if ( DXCoil( DXCoilNum ).NumOfSpeeds < 2 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "..." + cNumericFields( 9 ) + " must be >= 2. entered number is " + TrimSigDigits( Numbers( 9 ), 0 ) );
				ErrorsFound = true;
			}

			// Allocate arrays based on the number of speeds
			DXCoil( DXCoilNum ).MSErrIndex.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
			DXCoil( DXCoilNum ).MSErrIndex = 0;
			DXCoil( DXCoilNum ).MSRatedTotCap.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
			DXCoil( DXCoilNum ).MSRatedCOP.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
			DXCoil( DXCoilNum ).MSRatedAirVolFlowRate.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
			DXCoil( DXCoilNum ).MSRatedAirMassFlowRate.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
			DXCoil( DXCoilNum ).MSCCapFTemp.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
			DXCoil( DXCoilNum ).MSCCapFFlow.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
			DXCoil( DXCoilNum ).MSEIRFTemp.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
			DXCoil( DXCoilNum ).MSEIRFFlow.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
			DXCoil( DXCoilNum ).MSWasteHeat.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
			DXCoil( DXCoilNum ).MSPLFFPLR.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
			DXCoil( DXCoilNum ).MSRatedCBF.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
			DXCoil( DXCoilNum ).MSWasteHeatFrac.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
			DXCoil( DXCoilNum ).MSTotCapTempModFacCurveType.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
			DXCoil( DXCoilNum ).MSEIRTempModFacCurveType.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
			DXCoil( DXCoilNum ).MSFanPowerPerEvapAirFlowRate.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
			DXCoil( DXCoilNum ).MSSecCoilSHRFT.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
			DXCoil( DXCoilNum ).MSSecCoilSHRFF.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
			DXCoil( DXCoilNum ).MSSecCoilAirFlow.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
			DXCoil( DXCoilNum ).MSSecCoilAirFlowScalingFactor.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
			DXCoil( DXCoilNum ).MSSecCoilRatedSHR.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );

			DXCoil( DXCoilNum ).RatedSHR( 1 ) = 1.0;

			for ( I = 1; I <= DXCoil( DXCoilNum ).NumOfSpeeds; ++I ) {

				DXCoil( DXCoilNum ).MSRatedTotCap( I ) = Numbers( 10 + ( I - 1 ) * 5 );
				DXCoil( DXCoilNum ).MSRatedCOP( I ) = Numbers( 11 + ( I - 1 ) * 5 );
				DXCoil( DXCoilNum ).MSRatedAirVolFlowRate( I ) = Numbers( 12 + ( I - 1 ) * 5 );
				DXCoil( DXCoilNum ).MSFanPowerPerEvapAirFlowRate( I ) = Numbers( 13 + ( I - 1 ) * 5 );
				DXCoil( DXCoilNum ).MSWasteHeatFrac( I ) = Numbers( 14 + ( I - 1 ) * 5 );

				DXCoil( DXCoilNum ).MSCCapFTemp( I ) = GetCurveIndex( Alphas( 10 + ( I - 1 ) * 6 ) ); // convert curve name to number
				if ( DXCoil( DXCoilNum ).MSCCapFTemp( I ) == 0 ) {
					ShowSevereError( CurrentModuleObject + ", \"" + DXCoil( DXCoilNum ).Name + "\" " + cAlphaFields( 10 + ( I - 1 ) * 6 ) + " not found:" + Alphas( 10 + ( I - 1 ) * 6 ) );
					ErrorsFound = true;
				} else {
					// only legal types are Quadratic, BiQuadratic and Cubic
					{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).MSCCapFTemp( I ) ) );

					if ( SELECT_CASE_var == "QUADRATIC" ) {
						DXCoil( DXCoilNum ).MSTotCapTempModFacCurveType( I ) = Quadratic;
						CurveVal = CurveValue( DXCoil( DXCoilNum ).MSCCapFTemp( I ), RatedOutdoorAirTempHeat );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( 10 + ( I - 1 ) * 6 ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else if ( SELECT_CASE_var == "BIQUADRATIC" ) {
						DXCoil( DXCoilNum ).MSTotCapTempModFacCurveType( I ) = BiQuadratic;
						CurveVal = CurveValue( DXCoil( DXCoilNum ).MSCCapFTemp( I ), RatedInletAirTempHeat, RatedOutdoorAirTempHeat );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( 10 + ( I - 1 ) * 6 ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else if ( SELECT_CASE_var == "CUBIC" ) {
						DXCoil( DXCoilNum ).MSTotCapTempModFacCurveType( I ) = Cubic;
						CurveVal = CurveValue( DXCoil( DXCoilNum ).MSCCapFTemp( I ), RatedOutdoorAirTempHeat );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( 10 + ( I - 1 ) * 6 ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( 10 + ( I - 1 ) * 6 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).MSCCapFTemp( I ) ) );
						ShowContinueError( "Curve type must be BiQuadratic, Quadratic or Cubic." );
						ErrorsFound = true;
					}}
				}

				DXCoil( DXCoilNum ).MSCCapFFlow( I ) = GetCurveIndex( Alphas( 11 + ( I - 1 ) * 6 ) ); // convert curve name to number
				if ( DXCoil( DXCoilNum ).MSCCapFFlow( I ) == 0 ) {
					if ( lAlphaBlanks( 11 + ( I - 1 ) * 6 ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", missing" );
						ShowContinueError( "...required " + cAlphaFields( 11 + ( I - 1 ) * 6 ) + " is blank." );
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...not found " + cAlphaFields( 11 + ( I - 1 ) * 6 ) + "=\"" + Alphas( 11 + ( I - 1 ) * 6 ) + "\"." );
					}
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal type is Quadratic
					{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).MSCCapFFlow( I ) ) );

					if ( SELECT_CASE_var == "QUADRATIC" ) {
						CurveVal = CurveValue( DXCoil( DXCoilNum ).MSCCapFFlow( I ), 1.0 );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( 11 + ( I - 1 ) * 6 ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else if ( SELECT_CASE_var == "CUBIC" ) {
						CurveVal = CurveValue( DXCoil( DXCoilNum ).MSCCapFFlow( I ), 1.0 );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( 11 + ( I - 1 ) * 6 ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( 11 + ( I - 1 ) * 6 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).MSCCapFFlow( I ) ) );
						ShowContinueError( "Curve type must be Quadratic or Cubic." );
						ErrorsFound = true;
					}}
				}

				DXCoil( DXCoilNum ).MSEIRFTemp( I ) = GetCurveIndex( Alphas( 12 + ( I - 1 ) * 6 ) ); // convert curve name to number
				if ( DXCoil( DXCoilNum ).MSEIRFTemp( I ) == 0 ) {
					if ( lAlphaBlanks( 12 + ( I - 1 ) * 6 ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", missing" );
						ShowContinueError( "...required " + cAlphaFields( 12 + ( I - 1 ) * 6 ) + " is blank." );
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...not found " + cAlphaFields( 12 + ( I - 1 ) * 6 ) + "=\"" + Alphas( 15 + ( I - 1 ) * 6 ) + "\"." );
					}
					ErrorsFound = true;
				} else {
					// only legal types are Quadratic, BiQuadratic and Cubic
					{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).MSEIRFTemp( I ) ) );

					if ( SELECT_CASE_var == "QUADRATIC" ) {
						DXCoil( DXCoilNum ).MSEIRTempModFacCurveType( I ) = Quadratic;
						CurveVal = CurveValue( DXCoil( DXCoilNum ).MSEIRFTemp( I ), RatedOutdoorAirTempHeat );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( 12 + ( I - 1 ) * 6 ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else if ( SELECT_CASE_var == "BIQUADRATIC" ) {
						DXCoil( DXCoilNum ).MSEIRTempModFacCurveType( I ) = BiQuadratic;
						CurveVal = CurveValue( DXCoil( DXCoilNum ).MSEIRFTemp( I ), RatedInletAirTempHeat, RatedOutdoorAirTempHeat );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( 12 + ( I - 1 ) * 6 ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else if ( SELECT_CASE_var == "CUBIC" ) {
						DXCoil( DXCoilNum ).MSEIRTempModFacCurveType( I ) = Cubic;
						CurveVal = CurveValue( DXCoil( DXCoilNum ).MSEIRFTemp( I ), RatedOutdoorAirTempHeat );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( 12 + ( I - 1 ) * 6 ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( 12 + ( I - 1 ) * 6 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).MSEIRFTemp( I ) ) );
						ShowContinueError( "Curve type must be BiQuadratic, Quadratic or Cubic." );
						ErrorsFound = true;
					}}
				}

				DXCoil( DXCoilNum ).MSEIRFFlow( I ) = GetCurveIndex( Alphas( 13 + ( I - 1 ) * 6 ) ); // convert curve name to number
				if ( DXCoil( DXCoilNum ).MSEIRFFlow( I ) == 0 ) {
					if ( lAlphaBlanks( 13 + ( I - 1 ) * 6 ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", missing" );
						ShowContinueError( "...required " + cAlphaFields( 13 + ( I - 1 ) * 6 ) + " is blank." );
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...not found " + cAlphaFields( 13 + ( I - 1 ) * 6 ) + "=\"" + Alphas( 13 + ( I - 1 ) * 6 ) + "\"." );
					}
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal type is Quadratic
					{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).MSEIRFFlow( I ) ) );

					if ( SELECT_CASE_var == "QUADRATIC" ) {
						CurveVal = CurveValue( DXCoil( DXCoilNum ).MSEIRFFlow( I ), 1.0 );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( 13 + ( I - 1 ) * 6 ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else if ( SELECT_CASE_var == "CUBIC" ) {
						CurveVal = CurveValue( DXCoil( DXCoilNum ).MSEIRFFlow( I ), 1.0 );
						if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", curve values" );
							ShowContinueError( "..." + cAlphaFields( 13 + ( I - 1 ) * 6 ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
							ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
						}

					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( 13 + ( I - 1 ) * 6 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).MSEIRFFlow( I ) ) );
						ShowContinueError( "Curve type must be Quadratic or Cubic." );
						ErrorsFound = true;
					}}
				}

				DXCoil( DXCoilNum ).MSPLFFPLR( I ) = GetCurveIndex( Alphas( 14 + ( I - 1 ) * 6 ) ); // convert curve name to number
				if ( DXCoil( DXCoilNum ).MSPLFFPLR( I ) == 0 ) {
					if ( lAlphaBlanks( 14 + ( I - 1 ) * 6 ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", missing" );
						ShowContinueError( "...required " + cAlphaFields( 14 + ( I - 1 ) * 6 ) + " is blank." );
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...not found " + cAlphaFields( 14 + ( I - 1 ) * 6 ) + "=\"" + Alphas( 14 + ( I - 1 ) * 6 ) + "\"." );
					}
					ErrorsFound = true;
				} else {
					// Verify Curve Object, only legal types are Quadratic or Cubic
					{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).MSPLFFPLR( I ) ) );

					if ( SELECT_CASE_var == "QUADRATIC" ) {

					} else if ( SELECT_CASE_var == "CUBIC" ) {

					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( 14 + ( I - 1 ) * 6 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).MSPLFFPLR( I ) ) );
						ShowContinueError( "Curve type must be Quadratic or Cubic." );
						ErrorsFound = true;
					}}

					if ( !ErrorsFound ) {
						//       Test PLF curve minimum and maximum. Cap if less than 0.7 or greater than 1.0.
						MinCurveVal = 999.0;
						MaxCurveVal = -999.0;
						CurveInput = 0.0;
						while ( CurveInput <= 1.0 ) {
							CurveVal = CurveValue( DXCoil( DXCoilNum ).MSPLFFPLR( I ), CurveInput );
							if ( CurveVal < MinCurveVal ) {
								MinCurveVal = CurveVal;
								MinCurvePLR = CurveInput;
							}
							if ( CurveVal > MaxCurveVal ) {
								MaxCurveVal = CurveVal;
								MaxCurvePLR = CurveInput;
							}
							CurveInput += 0.01;
						}
						if ( MinCurveVal < 0.7 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
							ShowContinueError( "..." + cAlphaFields( 14 + ( I - 1 ) * 6 ) + " = " + Alphas( 14 + ( I - 1 ) * 6 ) + " has out of range value." );
							ShowContinueError( "...Curve minimum must be >= 0.7, curve min at PLR = " + TrimSigDigits( MinCurvePLR, 2 ) + " is " + TrimSigDigits( MinCurveVal, 3 ) );
							ShowContinueError( "...Setting curve minimum to 0.7 and simulation continues." );
							SetCurveOutputMinMaxValues( DXCoil( DXCoilNum ).PLFFPLR( 1 ), ErrorsFound, 0.7, _ );
							SetCurveOutputMinMaxValues( DXCoil( DXCoilNum ).MSPLFFPLR( I ), ErrorsFound, 0.7, _ );
						}

						if ( MaxCurveVal > 1.0 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
							ShowContinueError( "..." + cAlphaFields( 14 + ( I - 1 ) * 6 ) + " = " + Alphas( 14 + ( I - 1 ) * 6 ) + " has out of range value." );
							ShowContinueError( "...Curve maximum must be <= 1.0, curve max at PLR = " + TrimSigDigits( MaxCurvePLR, 2 ) + " is " + TrimSigDigits( MaxCurveVal, 3 ) );
							ShowContinueError( "...Setting curve maximum to 1.0 and simulation continues." );
							SetCurveOutputMinMaxValues( DXCoil( DXCoilNum ).MSPLFFPLR( I ), ErrorsFound, _, 1.0 );
						}

					}

				}

				// Read waste heat modifier curve name
				DXCoil( DXCoilNum ).MSWasteHeat( I ) = GetCurveIndex( Alphas( 15 + ( I - 1 ) * 6 ) ); // convert curve name to number
				if ( DXCoil( DXCoilNum ).FuelType != FuelTypeElectricity ) {
					if ( DXCoil( DXCoilNum ).MSWasteHeat( I ) > 0 ) {
						// Verify Curve Object, only legal types are BiQuadratic
						{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).MSWasteHeat( I ) ) );

						if ( SELECT_CASE_var == "BIQUADRATIC" ) {
							CurveVal = CurveValue( DXCoil( DXCoilNum ).MSWasteHeat( I ), RatedOutdoorAirTempHeat, RatedInletAirTempHeat );
							if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
								ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", curve values" );
								ShowContinueError( "..." + cAlphaFields( 15 + ( I - 1 ) * 6 ) + " output is not equal to 1.0 (+ or - 10%) at rated conditions." );
								ShowContinueError( "...Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
							}

						} else {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
							ShowContinueError( "...illegal " + cAlphaFields( 15 + ( I - 1 ) * 6 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).MSWasteHeat( I ) ) );
							ShowContinueError( "Curve type must be BiQuadratic." );
							ErrorsFound = true;
						}}
					}

				}
			}
			//A34; \field Zone Name for Condenser Placement
			if ( !lAlphaBlanks( 34 ) && NumAlphas > 33 ) {
				DXCoil( DXCoilNum ).SecZonePtr = FindItemInList( Alphas( 34 ), Zone );
				if ( DXCoil( DXCoilNum ).SecZonePtr > 0 ) {
					SetupZoneInternalGain( DXCoil( DXCoilNum ).SecZonePtr, "Coil:Heating:DX:MultiSpeed", DXCoil( DXCoilNum ).Name, IntGainTypeOf_SecHeatingDXCoilMultiSpeed, DXCoil( DXCoilNum ).SecCoilSensibleHeatRemovalRate, _, _, DXCoil( DXCoilNum ).SecCoilLatentHeatRemovalRate );
					DXCoil( DXCoilNum ).IsSecondaryDXCoilInZone = true;
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 34 ) + "=\"" + Alphas( 34 ) + "\"." );
				}
			}
			if ( DXCoil( DXCoilNum ).SecZonePtr > 0 ) {
				for ( I = 1; I <= DXCoil( DXCoilNum ).NumOfSpeeds; ++I ) {
					DXCoil( DXCoilNum ).MSSecCoilAirFlow( I ) = Numbers( 30 + ( I - 1 ) * 3 );
					DXCoil( DXCoilNum ).MSSecCoilAirFlowScalingFactor( I ) = Numbers( 31 + ( I - 1 ) * 3 );
					DXCoil( DXCoilNum ).MSSecCoilRatedSHR( I ) = Numbers( 32 + ( I - 1 ) * 3 );
					// Read SHR modifier curve function of temperature
					if ( !lAlphaBlanks( 35 + ( I - 1 ) * 2 ) ) {
						DXCoil( DXCoilNum ).MSSecCoilSHRFT( I ) = GetCurveIndex( Alphas( 35 + ( I - 1 ) * 2 ) ); // convert curve name to number
						if ( DXCoil( DXCoilNum ).MSSecCoilSHRFT( I ) == 0 ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
							ShowContinueError( "...not found " + cAlphaFields( 35 + ( I - 1 ) * 2 ) + "=\"" + Alphas( 35 + ( I - 1 ) * 2 ) + "\"." );
						}
					}
					// Read SHR modifier curve function of flow fraction
					if ( !lAlphaBlanks( 36 + ( I - 1 ) * 2 ) ) {
						DXCoil( DXCoilNum ).MSSecCoilSHRFF( I ) = GetCurveIndex( Alphas( 36 + ( I - 1 ) * 2 ) ); // convert curve name to number
						if ( DXCoil( DXCoilNum ).MSSecCoilSHRFF( I ) == 0 ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
							ShowContinueError( "...not found " + cAlphaFields( 36 + ( I - 1 ) * 2 ) + "=\"" + Alphas( 36 + ( I - 1 ) * 2 ) + "\"." );
						}
					}
				}
			}
		}

		// Loop over the VRF Cooling Coils and get & load the data
		CurrentModuleObject = cAllCoilTypes( CoilVRF_Cooling );
		for ( DXCoilIndex = 1; DXCoilIndex <= NumVRFCoolingCoils; ++DXCoilIndex ) {

			GetObjectItem( CurrentModuleObject, DXCoilIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			++DXCoilNum;

			// allocate single performance mode for numeric field strings used for sizing routine
			DXCoilNumericFields ( DXCoilNum ).PerfMode.allocate ( 1 );
			DXCoilNumericFields( DXCoilNum ).PerfMode( 1 ).FieldNames.allocate ( MaxNumbers );
			DXCoilNumericFields ( DXCoilNum ).PerfMode ( 1 ).FieldNames = cNumericFields;

			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), DXCoil, DXCoilNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			VerifyUniqueCoilName( CurrentModuleObject, Alphas( 1 ), errFlag, CurrentModuleObject + " Name" );
			if ( errFlag ) {
				ErrorsFound = true;
			}
			DXCoil( DXCoilNum ).Name = Alphas( 1 );
			DXCoil( DXCoilNum ).DXCoilType = CurrentModuleObject;
			DXCoil( DXCoilNum ).DXCoilType_Num = CoilVRF_Cooling;
			DXCoil( DXCoilNum ).Schedule = Alphas( 2 );
			if ( lAlphaBlanks( 2 ) ) {
				DXCoil( DXCoilNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				DXCoil( DXCoilNum ).SchedPtr = GetScheduleIndex( Alphas( 2 ) ); // convert schedule name to pointer
				if ( DXCoil( DXCoilNum ).SchedPtr == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "..." + cAlphaFields( 2 ) + "=\"" + Alphas( 2 ) + "\"." );
					ErrorsFound = true;
				}
			}
			DXCoil( DXCoilNum ).RatedTotCap( 1 ) = Numbers( 1 );
			DXCoil( DXCoilNum ).RatedSHR( 1 ) = Numbers( 2 );
			DXCoil( DXCoilNum ).RatedAirVolFlowRate( 1 ) = Numbers( 3 );

			DXCoil( DXCoilNum ).CCapFTemp( 1 ) = GetCurveIndex( Alphas( 3 ) );
			// Verify Curve Object, only legal type is Linear, Quadratic, Cubic, or BiQuadratic
			{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).CCapFTemp( 1 ) ) );

			if ( SELECT_CASE_var == "LINEAR" ) {
				DXCoil( DXCoilNum ).TotCapTempModFacCurveType( 1 ) = Linear;

			} else if ( SELECT_CASE_var == "QUADRATIC" ) {
				DXCoil( DXCoilNum ).TotCapTempModFacCurveType( 1 ) = Quadratic;

			} else if ( SELECT_CASE_var == "CUBIC" ) {
				DXCoil( DXCoilNum ).TotCapTempModFacCurveType( 1 ) = Cubic;

			} else if ( SELECT_CASE_var == "BIQUADRATIC" ) {
				DXCoil( DXCoilNum ).TotCapTempModFacCurveType( 1 ) = BiQuadratic;

			} else {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
				ShowContinueError( "...illegal " + cAlphaFields( 3 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).CCapFTemp( 1 ) ) );
				ShowContinueError( "... Curve type must be Linear, Quadratic, Cubic, or BiQuadratic." );
				ErrorsFound = true;
			}}

			DXCoil( DXCoilNum ).CCapFFlow( 1 ) = GetCurveIndex( Alphas( 4 ) ); // convert curve name to number
			if ( DXCoil( DXCoilNum ).CCapFFlow( 1 ) == 0 ) {
				if ( lAlphaBlanks( 4 ) ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", missing" );
					ShowContinueError( "...required " + cAlphaFields( 4 ) + " is blank." );
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 4 ) + "=\"" + Alphas( 4 ) + "\"." );
				}
				ErrorsFound = true;
			} else {
				// Verify Curve Object, only legal type is Linear, Quadratic or Cubic
				{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).CCapFFlow( 1 ) ) );

				if ( ( SELECT_CASE_var == "LINEAR" ) || ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) ) {

				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...illegal " + cAlphaFields( 4 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).CCapFFlow( 1 ) ) );
					ShowContinueError( "... Curve type must be Linear, Quadratic or Cubic." );
					ErrorsFound = true;
				}}
			}

			DXCoil( DXCoilNum ).AirInNode = GetOnlySingleNode( Alphas( 5 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

			DXCoil( DXCoilNum ).AirOutNode = GetOnlySingleNode( Alphas( 6 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			TestCompSet( CurrentModuleObject, Alphas( 1 ), Alphas( 5 ), Alphas( 6 ), "Air Nodes" );

			DXCoil( DXCoilNum ).CondensateCollectName = Alphas( 7 );
			if ( lAlphaBlanks( 7 ) ) {
				DXCoil( DXCoilNum ).CondensateCollectMode = CondensateDiscarded;
			} else {
				DXCoil( DXCoilNum ).CondensateCollectMode = CondensateToTank;
				SetupTankSupplyComponent( DXCoil( DXCoilNum ).Name, CurrentModuleObject, DXCoil( DXCoilNum ).CondensateCollectName, ErrorsFound, DXCoil( DXCoilNum ).CondensateTankID, DXCoil( DXCoilNum ).CondensateTankSupplyARRID );
			}

		}

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in getting " + CurrentModuleObject + " input. Preceding condition(s) causes termination." );
		}

		// Loop over the VRF Heating Coils and get & load the data
		CurrentModuleObject = cAllCoilTypes( CoilVRF_Heating );
		for ( DXCoilIndex = 1; DXCoilIndex <= NumVRFHeatingCoils; ++DXCoilIndex ) {

			GetObjectItem( CurrentModuleObject, DXCoilIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			++DXCoilNum;

			// allocate single performance mode for numeric field strings used for sizing routine
			DXCoilNumericFields ( DXCoilNum ).PerfMode.allocate ( 1 );
			DXCoilNumericFields( DXCoilNum ).PerfMode( 1 ).FieldNames.allocate ( MaxNumbers );
			DXCoilNumericFields ( DXCoilNum ).PerfMode ( 1 ).FieldNames = cNumericFields;

			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), DXCoil, DXCoilNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			VerifyUniqueCoilName( CurrentModuleObject, Alphas( 1 ), errFlag, CurrentModuleObject + " Name" );
			if ( errFlag ) {
				ErrorsFound = true;
			}
			DXCoil( DXCoilNum ).Name = Alphas( 1 );
			DXCoil( DXCoilNum ).DXCoilType = CurrentModuleObject;
			DXCoil( DXCoilNum ).DXCoilType_Num = CoilVRF_Heating;
			DXCoil( DXCoilNum ).Schedule = Alphas( 2 );
			if ( lAlphaBlanks( 2 ) ) {
				DXCoil( DXCoilNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				DXCoil( DXCoilNum ).SchedPtr = GetScheduleIndex( Alphas( 2 ) ); // convert schedule name to pointer
				if ( DXCoil( DXCoilNum ).SchedPtr == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "..." + cAlphaFields( 2 ) + "=\"" + Alphas( 2 ) + "\"." );
					ErrorsFound = true;
				}
			}
			DXCoil( DXCoilNum ).RatedTotCap( 1 ) = Numbers( 1 );
			DXCoil( DXCoilNum ).RatedAirVolFlowRate( 1 ) = Numbers( 2 );

			DXCoil( DXCoilNum ).AirInNode = GetOnlySingleNode( Alphas( 3 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

			DXCoil( DXCoilNum ).AirOutNode = GetOnlySingleNode( Alphas( 4 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			TestCompSet( CurrentModuleObject, Alphas( 1 ), Alphas( 3 ), Alphas( 4 ), "Air Nodes" );

			DXCoil( DXCoilNum ).CCapFTemp = GetCurveIndex( Alphas( 5 ) );
			if ( DXCoil( DXCoilNum ).CCapFTemp( 1 ) == 0 ) {
				if ( lAlphaBlanks( 5 ) ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", missing" );
					ShowContinueError( "...required " + cAlphaFields( 5 ) + " is blank." );
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 5 ) + "=\"" + Alphas( 5 ) + "\"." );
				}
				ErrorsFound = true;
			} else {
				// Verify Curve Object, only legal type is Quadratic
				{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).CCapFTemp( 1 ) ) );

				if ( SELECT_CASE_var == "LINEAR" ) {
					DXCoil( DXCoilNum ).TotCapTempModFacCurveType( 1 ) = Linear;
				} else if ( SELECT_CASE_var == "QUADRATIC" ) {
					DXCoil( DXCoilNum ).TotCapTempModFacCurveType( 1 ) = Quadratic;
				} else if ( SELECT_CASE_var == "CUBIC" ) {
					DXCoil( DXCoilNum ).TotCapTempModFacCurveType( 1 ) = Cubic;
				} else if ( SELECT_CASE_var == "BIQUADRATIC" ) {
					DXCoil( DXCoilNum ).TotCapTempModFacCurveType( 1 ) = BiQuadratic;
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...illegal " + cAlphaFields( 5 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).CCapFTemp( 1 ) ) );
					ShowContinueError( "... Curve type must be Linear, Quadratic, Cubic or BiQuadratic." );
					ErrorsFound = true;
				}}
			}

			DXCoil( DXCoilNum ).CCapFFlow( 1 ) = GetCurveIndex( Alphas( 6 ) ); // convert curve name to number
			if ( DXCoil( DXCoilNum ).CCapFFlow( 1 ) == 0 ) {
				if ( lAlphaBlanks( 6 ) ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", missing" );
					ShowContinueError( "...required " + cAlphaFields( 6 ) + " is blank." );
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 6 ) + "=\"" + Alphas( 6 ) + "\"." );
				}
				ErrorsFound = true;
			} else {
				// Verify Curve Object, only legal type is Quadratic
				{ auto const SELECT_CASE_var( GetCurveType( DXCoil( DXCoilNum ).CCapFFlow( 1 ) ) );

				if ( ( SELECT_CASE_var == "LINEAR" ) || ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) ) {

				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...illegal " + cAlphaFields( 5 ) + " type for this object = " + GetCurveType( DXCoil( DXCoilNum ).CCapFFlow( 1 ) ) );
					ShowContinueError( "... Curve type must be linear, Quadratic or Cubic." );
					ErrorsFound = true;
				}}
			}

		}

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in getting " + CurrentModuleObject + " input. Preceding condition(s) causes termination." );
		}

		// Loop over the VRF Cooling Coils for VRF FluidTCtrl Model_zrp 2015
		CurrentModuleObject = cAllCoilTypes( CoilVRF_FluidTCtrl_Cooling );
		for ( DXCoilIndex = 1; DXCoilIndex <= NumVRFCoolingFluidTCtrlCoils; ++DXCoilIndex ) {

			GetObjectItem( CurrentModuleObject, DXCoilIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			++DXCoilNum;

			// allocate single performance mode for numeric field strings used for sizing routine
			DXCoilNumericFields( DXCoilNum ).PerfMode.allocate ( 1 );
			DXCoilNumericFields( DXCoilNum ).PerfMode( 1 ).FieldNames.allocate ( MaxNumbers );
			DXCoilNumericFields( DXCoilNum ).PerfMode( 1 ).FieldNames = cNumericFields;

			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), DXCoil, DXCoilNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			VerifyUniqueCoilName( CurrentModuleObject, Alphas( 1 ), errFlag, CurrentModuleObject + " Name" );
			if ( errFlag ) {
				ErrorsFound = true;
			}
			DXCoil( DXCoilNum ).Name = Alphas( 1 );
			DXCoil( DXCoilNum ).DXCoilType = CurrentModuleObject;
			DXCoil( DXCoilNum ).DXCoilType_Num = CoilVRF_FluidTCtrl_Cooling;
			DXCoil( DXCoilNum ).Schedule = Alphas( 2 );
			if ( lAlphaBlanks( 2 ) ) {
				DXCoil( DXCoilNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				DXCoil( DXCoilNum ).SchedPtr = GetScheduleIndex( Alphas( 2 ) ); // convert schedule name to pointer
				if ( DXCoil( DXCoilNum ).SchedPtr == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "..." + cAlphaFields( 2 ) + "=\"" + Alphas( 2 ) + "\"." );
					ErrorsFound = true;
				}
			}

			DXCoil( DXCoilNum ).AirInNode = GetOnlySingleNode( Alphas( 3 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			DXCoil( DXCoilNum ).AirOutNode = GetOnlySingleNode( Alphas( 4 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			TestCompSet( CurrentModuleObject, Alphas( 1 ), Alphas( 3 ), Alphas( 4 ), "Air Nodes" );

			DXCoil( DXCoilNum ).RatedTotCap( 1 ) = Numbers( 1 );
			DXCoil( DXCoilNum ).RatedSHR( 1 ) = Numbers( 2 );
			DXCoil( DXCoilNum ).SH = Numbers( 3 );

			int indexSHCurve = GetCurveIndex( Alphas( 5 ) ); // convert curve name to index number
			// Verify curve name and type
			if ( indexSHCurve == 0 ) {
				if ( lAlphaBlanks( 5 ) ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", missing" );
					ShowContinueError( "...required " + cAlphaFields( 5 ) + " is blank." );
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 5 ) + "=\"" + Alphas( 5 ) + "\"." );
				}
				ErrorsFound = true;
			} else {
				{ auto const SELECT_CASE_var( GetCurveType( indexSHCurve ) );

					if ( SELECT_CASE_var == "QUADRATIC" ) {
						DXCoil( DXCoilNum ).C1Te = EnergyPlus::CurveManager::PerfCurve( indexSHCurve ).Coeff1;
						DXCoil( DXCoilNum ).C2Te = EnergyPlus::CurveManager::PerfCurve( indexSHCurve ).Coeff2;
						DXCoil( DXCoilNum ).C3Te = EnergyPlus::CurveManager::PerfCurve( indexSHCurve ).Coeff3;

					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( 5 ) + " type for this object = " + GetCurveType( indexSHCurve ) );
						ShowContinueError( "... Curve type must be Quadratic." );
						ErrorsFound = true;
					}
				}
			}

			DXCoil( DXCoilNum ).CondensateCollectName = Alphas( 6 );
			if ( lAlphaBlanks( 6 ) ) {
				DXCoil( DXCoilNum ).CondensateCollectMode = CondensateDiscarded;
			} else {
				DXCoil( DXCoilNum ).CondensateCollectMode = CondensateToTank;
				SetupTankSupplyComponent( DXCoil( DXCoilNum ).Name, CurrentModuleObject, DXCoil( DXCoilNum ).CondensateCollectName, ErrorsFound, DXCoil( DXCoilNum ).CondensateTankID, DXCoil( DXCoilNum ).CondensateTankSupplyARRID );
			}

		}

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in getting " + CurrentModuleObject + " input. Preceding condition(s) causes termination." );
		}

		// Loop over the VRF Heating Coils for VRF FluidTCtrl Model_zrp 2015
		CurrentModuleObject = cAllCoilTypes( CoilVRF_FluidTCtrl_Heating );
		for ( DXCoilIndex = 1; DXCoilIndex <= NumVRFHeatingFluidTCtrlCoils; ++DXCoilIndex ) {

			GetObjectItem( CurrentModuleObject, DXCoilIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			++DXCoilNum;

			// allocate single performance mode for numeric field strings used for sizing routine
			DXCoilNumericFields( DXCoilNum ).PerfMode.allocate ( 1 );
			DXCoilNumericFields( DXCoilNum ).PerfMode( 1 ).FieldNames.allocate( MaxNumbers );
			DXCoilNumericFields( DXCoilNum ).PerfMode( 1 ).FieldNames = cNumericFields;

			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), DXCoil, DXCoilNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			VerifyUniqueCoilName( CurrentModuleObject, Alphas( 1 ), errFlag, CurrentModuleObject + " Name" );
			if ( errFlag ) {
				ErrorsFound = true;
			}
			DXCoil( DXCoilNum ).Name = Alphas( 1 );
			DXCoil( DXCoilNum ).DXCoilType = CurrentModuleObject;
			DXCoil( DXCoilNum ).DXCoilType_Num = CoilVRF_FluidTCtrl_Heating;
			DXCoil( DXCoilNum ).Schedule = Alphas( 2 );
			if ( lAlphaBlanks( 2 ) ) {
				DXCoil( DXCoilNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				DXCoil( DXCoilNum ).SchedPtr = GetScheduleIndex( Alphas( 2 ) ); // convert schedule name to pointer
				if ( DXCoil( DXCoilNum ).SchedPtr == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "..." + cAlphaFields( 2 ) + "=\"" + Alphas( 2 ) + "\"." );
					ErrorsFound = true;
				}
			}

			DXCoil( DXCoilNum ).AirInNode = GetOnlySingleNode( Alphas( 3 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			DXCoil( DXCoilNum ).AirOutNode = GetOnlySingleNode( Alphas( 4 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			TestCompSet( CurrentModuleObject, Alphas( 1 ), Alphas( 3 ), Alphas( 4 ), "Air Nodes" );

			DXCoil( DXCoilNum ).RatedTotCap( 1 ) = Numbers( 1 );
			DXCoil( DXCoilNum ).SC = Numbers( 2 );

			int indexSCCurve = GetCurveIndex( Alphas( 5 ) ); // convert curve name to index number
			// Verify curve name and type
			if ( indexSCCurve == 0 ) {
				if ( lAlphaBlanks( 5 ) ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", missing" );
					ShowContinueError( "...required " + cAlphaFields( 5 ) + " is blank." );
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
					ShowContinueError( "...not found " + cAlphaFields( 5 ) + "=\"" + Alphas( 5 ) + "\"." );
				}
				ErrorsFound = true;
			} else {
				{ auto const SELECT_CASE_var( GetCurveType( indexSCCurve ) );

					if ( SELECT_CASE_var == "QUADRATIC" ) {
						DXCoil( DXCoilNum ).C1Tc = EnergyPlus::CurveManager::PerfCurve( indexSCCurve ).Coeff1;
						DXCoil( DXCoilNum ).C2Tc = EnergyPlus::CurveManager::PerfCurve( indexSCCurve ).Coeff2;
						DXCoil( DXCoilNum ).C3Tc = EnergyPlus::CurveManager::PerfCurve( indexSCCurve ).Coeff3;

					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + DXCoil( DXCoilNum ).Name + "\", invalid" );
						ShowContinueError( "...illegal " + cAlphaFields( 5 ) + " type for this object = " + GetCurveType( indexSCCurve ) );
						ShowContinueError( "... Curve type must be Quadratic." );
						ErrorsFound = true;
					}
				}
			}
		}

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in getting " + CurrentModuleObject + " input. Preceding condition(s) causes termination." );
		}

		for ( DXCoilNum = 1; DXCoilNum <= NumDXCoils; ++DXCoilNum ) {

			DXCoilData & Coil = DXCoil( DXCoilNum );

			if ( Coil.DXCoilType_Num == CoilDX_CoolingSingleSpeed || Coil.DXCoilType_Num == CoilDX_CoolingTwoStageWHumControl ) {
				// Setup Report Variables for Cooling Equipment
				// CurrentModuleObject='Coil:Cooling:DX:SingleSpeed/Coil:Cooling:DX:TwoStageWithHumidityControlMode'
				SetupOutputVariable( "Cooling Coil Total Cooling Rate [W]", Coil.TotalCoolingEnergyRate, "System", "Average", Coil.Name );
				SetupOutputVariable( "Cooling Coil Total Cooling Energy [J]", Coil.TotalCoolingEnergy, "System", "Sum", Coil.Name, _, "ENERGYTRANSFER", "COOLINGCOILS", _, "System" );
				SetupOutputVariable( "Cooling Coil Sensible Cooling Rate [W]", Coil.SensCoolingEnergyRate, "System", "Average", Coil.Name );
				SetupOutputVariable( "Cooling Coil Sensible Cooling Energy [J]", Coil.SensCoolingEnergy, "System", "Sum", Coil.Name );
				SetupOutputVariable( "Cooling Coil Latent Cooling Rate [W]", Coil.LatCoolingEnergyRate, "System", "Average", Coil.Name );
				SetupOutputVariable( "Cooling Coil Latent Cooling Energy [J]", Coil.LatCoolingEnergy, "System", "Sum", Coil.Name );
				SetupOutputVariable( "Cooling Coil Electric Power [W]", Coil.ElecCoolingPower, "System", "Average", Coil.Name );
				SetupOutputVariable( "Cooling Coil Electric Energy [J]", Coil.ElecCoolingConsumption, "System", "Sum", Coil.Name, _, "Electric", "COOLING", _, "System" );
				SetupOutputVariable( "Cooling Coil Runtime Fraction []", Coil.CoolingCoilRuntimeFraction, "System", "Average", Coil.Name );
				if ( Coil.IsSecondaryDXCoilInZone ) {
					SetupOutputVariable( "Secondary Coil Heat Rejection Rate [W]", Coil.SecCoilSensibleHeatGainRate, "System", "Average", Coil.Name );
				}

				// do we report these even if no storage tank?
				if ( Coil.CondensateCollectMode == CondensateToTank ) {
					SetupOutputVariable( "Cooling Coil Condensate Volume Flow Rate [m3/s]", Coil.CondensateVdot, "System", "Average", Coil.Name );
					SetupOutputVariable( "Cooling Coil Condensate Volume [m3]", Coil.CondensateVol, "System", "Sum", Coil.Name, _, "OnSiteWater", "Condensate", _, "System" );
				}

				// Moved to Init
				//  IF (Coil%ReportCoolingCoilCrankcasePower) THEN
				//    CALL SetupOutputVariable('DX Cooling Coil Crankcase Heater Power [W]',Coil%CrankcaseHeaterPower,'System',&
				//                             'Average',Coil%Name)
				//    CALL SetupOutputVariable('Cooling Coil Crankcase Heater Electric Energy [J]',Coil%CrankcaseHeaterConsumption,&
				//                             'System','Sum',Coil%Name, &
				//                              ResourceTypeKey='Electric',EndUseKey='COOLING',GroupKey='System')
				//  END IF

				if ( Coil.ReportEvapCondVars ) {
					SetupOutputVariable( "Cooling Coil Condenser Inlet Temperature [C]", Coil.CondInletTemp, "System", "Average", Coil.Name );
					SetupOutputVariable( "Cooling Coil Evaporative Condenser Water Volume [m3]", Coil.EvapWaterConsump, "System", "Sum", Coil.Name, _, "Water", "Cooling", _, "System" );
					SetupOutputVariable( "Cooling Coil Evaporative Condenser Mains Supply Water Volume [m3]", Coil.EvapWaterConsump, "System", "Sum", Coil.Name, _, "MainsWater", "Cooling", _, "System" );
					SetupOutputVariable( "Cooling Coil Evaporative Condenser Pump Electric Power [W]", Coil.EvapCondPumpElecPower, "System", "Average", Coil.Name );
					SetupOutputVariable( "Cooling Coil Evaporative Condenser Pump Electric Energy [J]", Coil.EvapCondPumpElecConsumption, "System", "Sum", Coil.Name, _, "Electric", "COOLING", _, "System" );
					if ( Coil.BasinHeaterPowerFTempDiff > 0.0 ) {
						SetupOutputVariable( "Cooling Coil Basin Heater Electric Power [W]", Coil.BasinHeaterPower, "System", "Average", Coil.Name );
						SetupOutputVariable( "Cooling Coil Basin Heater Electric Energy [J]", Coil.BasinHeaterConsumption, "System", "Sum", Coil.Name, _, "Electric", "COOLING", _, "System" );
					}
				}

				if ( Coil.DXCoilType_Num == CoilDX_CoolingTwoStageWHumControl ) {
					// Setup Report Variables for Cooling Equipment
					// CurrentModuleObject='Cooling:DX:TwoStageWithHumidityControlMode'
					SetupOutputVariable( "Cooling Coil Stage 2 Runtime Fraction []", Coil.CoolingCoilStg2RuntimeFrac, "System", "Average", Coil.Name );
					SetupOutputVariable( "Cooling Coil Dehumidification Mode []", Coil.DehumidificationMode, "System", "Average", Coil.Name );
				}

			}

			else if ( Coil.DXCoilType_Num == CoilDX_HeatingEmpirical ) {
				// Setup Report Variables for Heating Equipment
				// CurrentModuleObject='Coil:Heating:DX:SingleSpeed'
				SetupOutputVariable( "Heating Coil Total Heating Rate [W]", Coil.TotalHeatingEnergyRate, "System", "Average", Coil.Name );
				SetupOutputVariable( "Heating Coil Total Heating Energy [J]", Coil.TotalHeatingEnergy, "System", "Sum", Coil.Name, _, "ENERGYTRANSFER", "HEATINGCOILS", _, "System" );
				SetupOutputVariable( "Heating Coil Electric Power [W]", Coil.ElecHeatingPower, "System", "Average", Coil.Name );
				SetupOutputVariable( "Heating Coil Electric Energy [J]", Coil.ElecHeatingConsumption, "System", "Sum", Coil.Name, _, "Electric", "HEATING", _, "System" );
				SetupOutputVariable( "Heating Coil Defrost Electric Power [W]", Coil.DefrostPower, "System", "Average", Coil.Name );
				SetupOutputVariable( "Heating Coil Defrost Electric Energy [J]", Coil.DefrostConsumption, "System", "Sum", Coil.Name, _, "Electric", "HEATING", _, "System" );
				SetupOutputVariable( "Heating Coil Crankcase Heater Electric Power [W]", Coil.CrankcaseHeaterPower, "System", "Average", Coil.Name );
				SetupOutputVariable( "Heating Coil Crankcase Heater Electric Energy [J]", Coil.CrankcaseHeaterConsumption, "System", "Sum", Coil.Name, _, "Electric", "HEATING", _, "System" );
				SetupOutputVariable( "Heating Coil Runtime Fraction []", Coil.HeatingCoilRuntimeFraction, "System", "Average", Coil.Name );
				if ( Coil.IsSecondaryDXCoilInZone ) {
					SetupOutputVariable( "Secondary Coil Total Heat Removal Rate [W]", Coil.SecCoilTotalHeatRemovalRate, "System", "Average", Coil.Name );
					SetupOutputVariable( "Secondary Coil Sensible Heat Removal Rate [W]", Coil.SecCoilSensibleHeatRemovalRate, "System", "Average", Coil.Name );
					SetupOutputVariable( "Secondary Coil Latent Heat Removal Rate [W]", Coil.SecCoilLatentHeatRemovalRate, "System", "Average", Coil.Name );
					SetupOutputVariable( "Secondary Coil Sensible Heat Ratio []", Coil.SecCoilSHR, "System", "Average", Coil.Name );
					SetupOutputVariable( "Secondary Coil Compressor Part Load Ratio []", Coil.CompressorPartLoadRatio, "System", "Average", Coil.Name );
				}
			}

			else if ( Coil.DXCoilType_Num == CoilDX_CoolingTwoSpeed ) {
				// Setup Report Variables for Cooling Equipment
				// CurrentModuleObject='Coil:Cooling:DX:TwoSpeed'
				SetupOutputVariable( "Cooling Coil Total Cooling Rate [W]", Coil.TotalCoolingEnergyRate, "System", "Average", Coil.Name );
				SetupOutputVariable( "Cooling Coil Total Cooling Energy [J]", Coil.TotalCoolingEnergy, "System", "Sum", Coil.Name, _, "ENERGYTRANSFER", "COOLINGCOILS", _, "System" );
				SetupOutputVariable( "Cooling Coil Sensible Cooling Rate [W]", Coil.SensCoolingEnergyRate, "System", "Average", Coil.Name );
				SetupOutputVariable( "Cooling Coil Sensible Cooling Energy [J]", Coil.SensCoolingEnergy, "System", "Sum", Coil.Name );
				SetupOutputVariable( "Cooling Coil Latent Cooling Rate [W]", Coil.LatCoolingEnergyRate, "System", "Average", Coil.Name );
				SetupOutputVariable( "Cooling Coil Latent Cooling Energy [J]", Coil.LatCoolingEnergy, "System", "Sum", Coil.Name );
				SetupOutputVariable( "Cooling Coil Electric Power [W]", Coil.ElecCoolingPower, "System", "Average", Coil.Name );
				SetupOutputVariable( "Cooling Coil Electric Energy [J]", Coil.ElecCoolingConsumption, "System", "Sum", Coil.Name, _, "Electric", "COOLING", _, "System" );
				SetupOutputVariable( "Cooling Coil Runtime Fraction []", Coil.CoolingCoilRuntimeFraction, "System", "Average", Coil.Name );
				if ( Coil.IsSecondaryDXCoilInZone ) {
					SetupOutputVariable( "Secondary Coil Heat Rejection Rate [W]", Coil.SecCoilSensibleHeatGainRate, "System", "Average", Coil.Name );
				}

				if ( Coil.ReportEvapCondVars ) {
					SetupOutputVariable( "Cooling Coil Condenser Inlet Temperature [C]", Coil.CondInletTemp, "System", "Average", Coil.Name );
					SetupOutputVariable( "Cooling Coil Evaporative Condenser Water Volume [m3]", Coil.EvapWaterConsump, "System", "Sum", Coil.Name, _, "Water", "Cooling", _, "System" );
					SetupOutputVariable( "Cooling Coil Evaporative Condenser Mains Supply Water Volume [m3]", Coil.EvapWaterConsump, "System", "Sum", Coil.Name, _, "MainsWater", "Cooling", _, "System" );
					SetupOutputVariable( "Cooling Coil Evaporative Condenser Pump Electric Power [W]", Coil.EvapCondPumpElecPower, "System", "Average", Coil.Name );
					SetupOutputVariable( "Cooling Coil Evaporative Condenser Pump Electric Energy [J]", Coil.EvapCondPumpElecConsumption, "System", "Sum", Coil.Name, _, "Electric", "COOLING", _, "System" );
					if ( Coil.BasinHeaterPowerFTempDiff > 0.0 ) {
						SetupOutputVariable( "Cooling Coil Basin Heater Electric Power [W]", Coil.BasinHeaterPower, "System", "Average", Coil.Name );
						SetupOutputVariable( "Cooling Coil Basin Heater Electric Energy [J]", Coil.BasinHeaterConsumption, "System", "Sum", Coil.Name, _, "Electric", "COOLING", _, "System" );
					}

				}

			}

			else if ( Coil.DXCoilType_Num == CoilDX_HeatPumpWaterHeaterPumped || Coil.DXCoilType_Num == CoilDX_HeatPumpWaterHeaterWrapped ) {
				// Setup Report Variables for Cooling Equipment
				// CurrentModuleObject='Coil:WaterHeating:AirToWaterHeatPump:Pumped'
				// or 'Coil:WaterHeating:AirToWaterHeatPump:Wrapped'
				SetupOutputVariable( "Cooling Coil Total Cooling Rate [W]", Coil.TotalCoolingEnergyRate, "System", "Average", Coil.Name );

				if ( Coil.IsDXCoilInZone ){
					SetupOutputVariable( "Cooling Coil Total Cooling Energy [J]", Coil.TotalCoolingEnergy, "System", "Sum", Coil.Name, _, "ENERGYTRANSFER", "COOLINGCOILS", _, "System" );
				} else {
					SetupOutputVariable( "Cooling Coil Total Cooling Energy [J]", Coil.TotalCoolingEnergy, "System", "Sum", Coil.Name );
				}

				SetupOutputVariable( "Cooling Coil Sensible Cooling Rate [W]", Coil.SensCoolingEnergyRate, "System", "Average", Coil.Name );
				SetupOutputVariable( "Cooling Coil Sensible Cooling Energy [J]", Coil.SensCoolingEnergy, "System", "Sum", Coil.Name );
				SetupOutputVariable( "Cooling Coil Latent Cooling Rate [W]", Coil.LatCoolingEnergyRate, "System", "Average", Coil.Name );
				SetupOutputVariable( "Cooling Coil Latent Cooling Energy [J]", Coil.LatCoolingEnergy, "System", "Sum", Coil.Name );
				SetupOutputVariable( "Cooling Coil Runtime Fraction []", Coil.CoolingCoilRuntimeFraction, "System", "Average", Coil.Name );

				if ( Coil.ReportCoolingCoilCrankcasePower ) {
					SetupOutputVariable( "Cooling Coil Crankcase Heater Electric Power [W]", Coil.CrankcaseHeaterPower, "System", "Average", Coil.Name );
					SetupOutputVariable( "Cooling Coil Crankcase Heater Electric Energy [J]", Coil.CrankcaseHeaterConsumption, "System", "Sum", Coil.Name, _, "Electric", "DHW", _, "Plant" );
				}

				// new report variables for a HP water heater DX coil
				SetupOutputVariable( "Cooling Coil Total Water Heating Rate [W]", Coil.TotalHeatingEnergyRate, "System", "Average", Coil.Name );
				SetupOutputVariable( "Cooling Coil Total Water Heating Energy [J]", Coil.TotalHeatingEnergy, "System", "Sum", Coil.Name ); //, &
				//                           ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HEATING',GroupKey='Plant')
				SetupOutputVariable( "Cooling Coil Water Heating Electric Power [W]", Coil.ElecWaterHeatingPower, "System", "Average", Coil.Name );
				SetupOutputVariable( "Cooling Coil Water Heating Electric Energy [J]", Coil.ElecWaterHeatingConsumption, "System", "Sum", Coil.Name, _, "Electric", "DHW", _, "Plant" );
			}

			else if ( Coil.DXCoilType_Num == CoilDX_MultiSpeedCooling ) {
				// Setup Report Variables for Cooling Equipment:
				// CurrentModuleObject='Coil:Cooling:DX:MultiSpeed'
				SetupOutputVariable( "Cooling Coil Total Cooling Rate [W]", Coil.TotalCoolingEnergyRate, "System", "Average", Coil.Name );
				SetupOutputVariable( "Cooling Coil Total Cooling Energy [J]", Coil.TotalCoolingEnergy, "System", "Sum", Coil.Name, _, "ENERGYTRANSFER", "COOLINGCOILS", _, "System" );
				SetupOutputVariable( "Cooling Coil Sensible Cooling Rate [W]", Coil.SensCoolingEnergyRate, "System", "Average", Coil.Name );
				SetupOutputVariable( "Cooling Coil Sensible Cooling Energy [J]", Coil.SensCoolingEnergy, "System", "Sum", Coil.Name );
				SetupOutputVariable( "Cooling Coil Latent Cooling Rate [W]", Coil.LatCoolingEnergyRate, "System", "Average", Coil.Name );
				SetupOutputVariable( "Cooling Coil Latent Cooling Energy [J]", Coil.LatCoolingEnergy, "System", "Sum", Coil.Name );
				SetupOutputVariable( "Cooling Coil Electric Power [W]", Coil.ElecCoolingPower, "System", "Average", Coil.Name );
				SetupOutputVariable( "Cooling Coil Electric Energy [J]", Coil.ElecCoolingConsumption, "System", "Sum", Coil.Name, _, "Electric", "COOLING", _, "System" );

				if ( Coil.FuelType != FuelTypeElectricity ) {
					SetupOutputVariable( "Cooling Coil " + cValidOutputFuelTypes( Coil.FuelType ) + " Rate [W]", Coil.FuelUsed, "System", "Average", Coil.Name );
					SetupOutputVariable( "Cooling Coil " + cValidOutputFuelTypes( Coil.FuelType ) + " Energy [J]", Coil.FuelConsumed, "System", "Sum", Coil.Name, _, cValidOutputFuelTypes( Coil.FuelType ), "COOLING", _, "System" );
				}

				SetupOutputVariable( "Cooling Coil Runtime Fraction []", Coil.CoolingCoilRuntimeFraction, "System", "Average", Coil.Name );

				if ( Coil.ReportEvapCondVars ) {
					SetupOutputVariable( "Cooling Coil Condenser Inlet Temperature [C]", Coil.CondInletTemp, "System", "Average", Coil.Name );
					SetupOutputVariable( "Cooling Coil Evaporative Condenser Water Volume [m3]", Coil.EvapWaterConsump, "System", "Sum", Coil.Name, _, "Water", "Cooling", _, "System" );
					SetupOutputVariable( "Cooling Coil Evaporative Condenser Mains Supply Water Volume [m3]", Coil.EvapWaterConsump, "System", "Sum", Coil.Name, _, "MainsWater", "Cooling", _, "System" );
					SetupOutputVariable( "Cooling Coil Evaporative Condenser Pump Electric Power [W]", Coil.EvapCondPumpElecPower, "System", "Average", Coil.Name );
					SetupOutputVariable( "Cooling Coil Evaporative Condenser Pump Electric Energy [J]", Coil.EvapCondPumpElecConsumption, "System", "Sum", Coil.Name, _, "Electric", "COOLING", _, "System" );
					if ( Coil.BasinHeaterPowerFTempDiff > 0.0 ) {
						SetupOutputVariable( "Cooling Coil Basin Heater Electric Power [W]", Coil.BasinHeaterPower, "System", "Average", Coil.Name );
						SetupOutputVariable( "Cooling Coil Basin Heater Electric Energy [J]", Coil.BasinHeaterConsumption, "System", "Sum", Coil.Name, _, "Electric", "COOLING", _, "System" );
					}
				}
				if ( Coil.IsSecondaryDXCoilInZone ) {
					SetupOutputVariable( "Secondary Coil Heat Rejection Rate [W]", Coil.SecCoilSensibleHeatGainRate, "System", "Average", Coil.Name );
				}

			}

			else if ( Coil.DXCoilType_Num == CoilDX_MultiSpeedHeating ) {
				// Setup Report Variables for Heating Equipment:
				// CurrentModuleObject='Coil:Heating:DX:MultiSpeed'
				SetupOutputVariable( "Heating Coil Total Heating Rate [W]", Coil.TotalHeatingEnergyRate, "System", "Average", Coil.Name );
				SetupOutputVariable( "Heating Coil Total Heating Energy [J]", Coil.TotalHeatingEnergy, "System", "Sum", Coil.Name, _, "ENERGYTRANSFER", "HEATINGCOILS", _, "System" );
				SetupOutputVariable( "Heating Coil Electric Power [W]", Coil.ElecHeatingPower, "System", "Average", Coil.Name );
				SetupOutputVariable( "Heating Coil Electric Energy [J]", Coil.ElecHeatingConsumption, "System", "Sum", Coil.Name, _, "Electric", "HEATING", _, "System" );

				if ( Coil.FuelType != FuelTypeElectricity ) {
					SetupOutputVariable( "Heating Coil " + cValidOutputFuelTypes( Coil.FuelType ) + " Rate [W]", Coil.FuelUsed, "System", "Average", Coil.Name );
					SetupOutputVariable( "Heating Coil " + cValidOutputFuelTypes( Coil.FuelType ) + " Energy [J]", Coil.FuelConsumed, "System", "Sum", Coil.Name, _, cValidOutputFuelTypes( Coil.FuelType ), "HEATING", _, "System" );
				}

				if ( Coil.FuelType != FuelTypeElectricity && Coil.DefrostStrategy == ReverseCycle ) {
					SetupOutputVariable( "Heating Coil Defrost " + cValidOutputFuelTypes( Coil.FuelType ) + " Rate [W]", Coil.DefrostPower, "System", "Average", Coil.Name );
					SetupOutputVariable( "Heating Coil Defrost " + cValidOutputFuelTypes( Coil.FuelType ) + " Energy [J]", Coil.DefrostConsumption, "System", "Sum", Coil.Name, _, cValidOutputFuelTypes( Coil.FuelType ), "HEATING", _, "System" );
				} else {
					SetupOutputVariable( "Heating Coil Defrost Electric Power [W]", Coil.DefrostPower, "System", "Average", Coil.Name );
					SetupOutputVariable( "Heating Coil Defrost Electric Energy [J]", Coil.DefrostConsumption, "System", "Sum", Coil.Name, _, "Electric", "HEATING", _, "System" );
				}

				SetupOutputVariable( "Heating Coil Crankcase Heater Electric Power [W]", Coil.CrankcaseHeaterPower, "System", "Average", Coil.Name );
				SetupOutputVariable( "Heating Coil Crankcase Heater Electric Energy [J]", Coil.CrankcaseHeaterConsumption, "System", "Sum", Coil.Name, _, "Electric", "HEATING", _, "System" );
				SetupOutputVariable( "Heating Coil Runtime Fraction []", Coil.HeatingCoilRuntimeFraction, "System", "Average", Coil.Name );

				if ( Coil.IsSecondaryDXCoilInZone ) {
					SetupOutputVariable( "Secondary Coil Total Heat Removal Rate [W]", Coil.SecCoilTotalHeatRemovalRate, "System", "Average", Coil.Name );
					SetupOutputVariable( "Secondary Coil Sensible Heat Removal Rate [W]", Coil.SecCoilSensibleHeatRemovalRate, "System", "Average", Coil.Name );
					SetupOutputVariable( "Secondary Coil Latent Heat Removal Rate [W]", Coil.SecCoilLatentHeatRemovalRate, "System", "Average", Coil.Name );
					SetupOutputVariable( "Secondary Coil Sensible Heat Ratio []", Coil.SecCoilSHR, "System", "Average", Coil.Name );
				}
			}

			// VRF cooling coil report variables
			else if ( Coil.DXCoilType_Num == CoilVRF_Cooling ) {
				// Setup Report Variables for Cooling Equipment:
				// CurrentModuleObject='Coil:Cooling:DX:VariableRefrigerantFlow
				SetupOutputVariable( "Cooling Coil Total Cooling Rate [W]", Coil.TotalCoolingEnergyRate, "System", "Average", Coil.Name );
				SetupOutputVariable( "Cooling Coil Total Cooling Energy [J]", Coil.TotalCoolingEnergy, "System", "Sum", Coil.Name, _, "ENERGYTRANSFER", "COOLINGCOILS", _, "System" );
				SetupOutputVariable( "Cooling Coil Sensible Cooling Rate [W]", Coil.SensCoolingEnergyRate, "System", "Average", Coil.Name );
				SetupOutputVariable( "Cooling Coil Sensible Cooling Energy [J]", Coil.SensCoolingEnergy, "System", "Sum", Coil.Name );
				SetupOutputVariable( "Cooling Coil Latent Cooling Rate [W]", Coil.LatCoolingEnergyRate, "System", "Average", Coil.Name );
				SetupOutputVariable( "Cooling Coil Latent Cooling Energy [J]", Coil.LatCoolingEnergy, "System", "Sum", Coil.Name );
				SetupOutputVariable( "Cooling Coil Runtime Fraction []", Coil.CoolingCoilRuntimeFraction, "System", "Average", Coil.Name );
				if ( Coil.CondensateCollectMode == CondensateToTank ) {
					SetupOutputVariable( "Cooling Coil Condensate Volume Flow Rate [m3/s]", Coil.CondensateVdot, "System", "Average", Coil.Name );
					SetupOutputVariable( "Cooling Coil Condensate Volume [m3]", Coil.CondensateVol, "System", "Sum", Coil.Name, _, "OnSiteWater", "Condensate", _, "System" );
				}
			}

			// VRF heating coil report variables
			else if ( Coil.DXCoilType_Num == CoilVRF_Heating ) {
				// Setup Report Variables for Heating Equipment:
				// CurrentModuleObject='Coil:Heating:DX:VariableRefrigerantFlow
				SetupOutputVariable( "Heating Coil Total Heating Rate [W]", Coil.TotalHeatingEnergyRate, "System", "Average", Coil.Name );
				SetupOutputVariable( "Heating Coil Total Heating Energy [J]", Coil.TotalHeatingEnergy, "System", "Sum", Coil.Name, _, "ENERGYTRANSFER", "HEATINGCOILS", _, "System" );
				SetupOutputVariable( "Heating Coil Runtime Fraction []", Coil.HeatingCoilRuntimeFraction, "System", "Average", Coil.Name );
			}

			// VRF cooling coil for FluidTCtrl, report variables
			else if ( Coil.DXCoilType_Num == CoilVRF_FluidTCtrl_Cooling ) {
				// Setup Report Variables for Cooling Equipment:
				// CurrentModuleObject='Coil:Cooling:DX:VariableRefrigerantFlow:FluidTemperatureControl
				SetupOutputVariable( "Cooling Coil Total Cooling Rate [W]", Coil.TotalCoolingEnergyRate, "System", "Average", Coil.Name );
				SetupOutputVariable( "Cooling Coil Total Cooling Energy [J]", Coil.TotalCoolingEnergy, "System", "Sum", Coil.Name, _, "ENERGYTRANSFER", "COOLINGCOILS", _, "System" );
				SetupOutputVariable( "Cooling Coil Sensible Cooling Rate [W]", Coil.SensCoolingEnergyRate, "System", "Average", Coil.Name );
				SetupOutputVariable( "Cooling Coil Sensible Cooling Energy [J]", Coil.SensCoolingEnergy, "System", "Sum", Coil.Name );
				SetupOutputVariable( "Cooling Coil Latent Cooling Rate [W]", Coil.LatCoolingEnergyRate, "System", "Average", Coil.Name );
				SetupOutputVariable( "Cooling Coil Latent Cooling Energy [J]", Coil.LatCoolingEnergy, "System", "Sum", Coil.Name );
				SetupOutputVariable( "Cooling Coil Runtime Fraction []", Coil.CoolingCoilRuntimeFraction, "System", "Average", Coil.Name );
				// Followings for VRF_FluidTCtrl Only
				SetupOutputVariable( "Cooling Coil VRF Evaporating Temperature [C]", Coil.EvaporatingTemp, "System", "Average", Coil.Name );
				SetupOutputVariable( "Cooling Coil VRF Super Heating Degrees [C]", Coil.ActualSH, "System", "Average", Coil.Name );

				if ( Coil.CondensateCollectMode == CondensateToTank ) {
					SetupOutputVariable( "Cooling Coil Condensate Volume Flow Rate [m3/s]", Coil.CondensateVdot, "System", "Average", Coil.Name );
					SetupOutputVariable( "Cooling Coil Condensate Volume [m3]", Coil.CondensateVol, "System", "Sum", Coil.Name, _, "OnSiteWater", "Condensate", _, "System" );
				}
			}

			// VRF heating coil for FluidTCtrl, report variables
			else if ( Coil.DXCoilType_Num == CoilVRF_FluidTCtrl_Heating )  {
				// Setup Report Variables for Heating Equipment:
				// CurrentModuleObject='Coil:Heating:DX:VariableRefrigerantFlow:FluidTemperatureControl
				SetupOutputVariable( "Heating Coil Total Heating Rate [W]", Coil.TotalHeatingEnergyRate, "System", "Average", Coil.Name );
				SetupOutputVariable( "Heating Coil Total Heating Energy [J]", Coil.TotalHeatingEnergy, "System", "Sum", Coil.Name, _, "ENERGYTRANSFER", "HEATINGCOILS", _, "System" );
				SetupOutputVariable( "Heating Coil Runtime Fraction []", Coil.HeatingCoilRuntimeFraction, "System", "Average", Coil.Name );
				// Followings for VRF_FluidTCtrl Only
				SetupOutputVariable( "Heating Coil VRF Condensing Temperature [C]", Coil.CondensingTemp, "System", "Average", Coil.Name );
				SetupOutputVariable( "Heating Coil VRF Subcooling Degrees [C]", Coil.ActualSC, "System", "Average", Coil.Name );
			}

		}


		if ( AnyEnergyManagementSystemInModel ) {
			// setup EMS sizing actuators for single speed DX
			for ( DXCoilNum = 1; DXCoilNum <= NumDoe2DXCoils; ++DXCoilNum ) {
				SetupEMSActuator( "Coil:Cooling:DX:SingleSpeed", DXCoil( DXCoilNum ).Name, "Autosized Rated Air Flow Rate", "[m3/s]", DXCoil( DXCoilNum ).RatedAirVolFlowRateEMSOverrideON( 1 ), DXCoil( DXCoilNum ).RatedAirVolFlowRateEMSOverrideValue( 1 ) );

				SetupEMSActuator( "Coil:Cooling:DX:SingleSpeed", DXCoil( DXCoilNum ).Name, "Autosized Rated Sensible Heat Ratio", "[W/W]", DXCoil( DXCoilNum ).RatedSHREMSOverrideOn( 1 ), DXCoil( DXCoilNum ).RatedSHREMSOverrideValue( 1 ) );

				SetupEMSActuator( "Coil:Cooling:DX:SingleSpeed", DXCoil( DXCoilNum ).Name, "Autosized Rated Total Cooling Capacity", "[W]", DXCoil( DXCoilNum ).RatedTotCapEMSOverrideOn( 1 ), DXCoil( DXCoilNum ).RatedTotCapEMSOverrideValue( 1 ) );
			}

		}
		Alphas.deallocate();
		cAlphaFields.deallocate();
		cNumericFields.deallocate();
		Numbers.deallocate();
		lAlphaBlanks.deallocate();
		lNumericBlanks.deallocate();

		Alphas2.deallocate();
		cAlphaFields2.deallocate();
		cNumericFields2.deallocate();
		Numbers2.deallocate();
		lAlphaBlanks2.deallocate();
		lNumericBlanks2.deallocate();
		ManageEMS( emsCallFromComponentGetInput );

	}

	void
	InitDXCoil( int const DXCoilNum ) // number of the current DX coil unit being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   May 2000
		//                      Feb 2005, M. J. Witte, GARD Analytics, Inc. Add new coil type COIL:DX:MultiMode:CoolingEmpirical:
		//                      Jul 2005, R. Raustad, FSEC. Add new coil type COIL:DX:HEATPUMPWATERHEATER
		//                      Jun 2007, L. Gu, FSEC. Add new coil type COIL:DX:MULTISPEED:COOLING and HEATING
		//                      Aug 2015, R. Zhang, LBNL. Add new coil types for VRF_FluidTCtrl
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for initializations of DX Coil Components.

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger initializations.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHVACGlobals::FanElecPower;
		using DataAirLoop::AirLoopInputsFilled;
		using General::TrimSigDigits;
		using ReportSizingManager::ReportSizingOutput;
		using DataHeatBalFanSys::ZoneAirHumRat;
		using DataHeatBalFanSys::ZT;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static Real64 SmallDifferenceTest( 0.00000001 );
		static std::string const RoutineName( "InitDXCoil" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static Array1D_bool MyEnvrnFlag; // One time environment flag
		static Array1D_bool MySizeFlag; // One time sizing flag
		static bool CrankcaseHeaterReportVarFlag( true ); // One time flag used to report crankcase heater power for non-HP coils
		Real64 RatedHeatPumpIndoorAirTemp; // Indoor dry-bulb temperature to heat pump evaporator at rated conditions [C]
		Real64 RatedHeatPumpIndoorHumRat; // Inlet humidity ratio to heat pump evaporator at rated conditions [kgWater/kgDryAir]
		Real64 RatedVolFlowPerRatedTotCap; // Rated Air Volume Flow Rate divided by Rated Total Capacity [m3/s-W)
		Real64 HPInletAirHumRat; // Rated inlet air humidity ratio for heat pump water heater [kgWater/kgDryAir]
		static bool ErrorsFound( false ); // TRUE when errors found
		int CapacityStageNum; // Loop index for 1,Number of capacity stages
		int DehumidModeNum; // Loop index for 1,Number of enhanced dehumidification modes
		int Mode; // Performance mode for MultiMode DX coil; Always 1 for other coil types
		int DXCoilNumTemp; // Counter for crankcase heater report variable DO loop
		int AirInletNode; // Air inlet node number
		int SpeedNum; // Speed number for multispeed coils

		if ( MyOneTimeFlag ) {
			// initialize the environment and sizing flags
			MyEnvrnFlag.allocate( NumDXCoils );
			MySizeFlag.allocate( NumDXCoils );
			MyEnvrnFlag = true;
			MySizeFlag = true;
			MyOneTimeFlag = false;

		}
		// if "ISHundredPercentDOASDXCoil" =.TRUE., then set coil as 100% DOAS dx coil
		if ( DXCoil( DXCoilNum ).ISHundredPercentDOASDXCoil ) {
			DXCT = 2;
		} else {
			DXCT = 1;
		}

		if ( ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_HeatPumpWaterHeaterPumped || DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_HeatPumpWaterHeaterWrapped ) && MyEnvrnFlag( DXCoilNum ) ) {

			SizeDXCoil( DXCoilNum );

			RatedVolFlowPerRatedTotCap = DXCoil( DXCoilNum ).RatedAirVolFlowRate( 1 ) / DXCoil( DXCoilNum ).RatedTotCap2;
			if ( ( ( MinRatedVolFlowPerRatedTotCap( DXCT ) - RatedVolFlowPerRatedTotCap ) > SmallDifferenceTest ) || ( ( RatedVolFlowPerRatedTotCap - MaxHeatVolFlowPerRatedTotCap( DXCT ) ) > SmallDifferenceTest ) ) {
				ShowSevereError( DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + "\": Rated air volume flow rate per watt of rated total water heating capacity is out of range" );
				ShowContinueError( "Min Rated Vol Flow Per Watt=[" + TrimSigDigits( MinRatedVolFlowPerRatedTotCap( DXCT ), 3 ) + "], Rated Vol Flow Per Watt=[" + TrimSigDigits( RatedVolFlowPerRatedTotCap, 3 ) + "], Max Rated Vol Flow Per Watt=[" + TrimSigDigits( MaxHeatVolFlowPerRatedTotCap( DXCT ), 3 ) + "]. See Input-Output Reference Manual for valid range." );
			}
			HPInletAirHumRat = PsyWFnTdbTwbPb( DXCoil( DXCoilNum ).RatedInletDBTemp, DXCoil( DXCoilNum ).RatedInletWBTemp, StdPressureSeaLevel, RoutineName );
			HPWHInletDBTemp = DXCoil( DXCoilNum ).RatedInletDBTemp;
			HPWHInletWBTemp = DXCoil( DXCoilNum ).RatedInletWBTemp;
			DXCoil( DXCoilNum ).RatedAirMassFlowRate( 1 ) = DXCoil( DXCoilNum ).RatedAirVolFlowRate( 1 ) * PsyRhoAirFnPbTdbW( StdBaroPress, DXCoil( DXCoilNum ).RatedInletDBTemp, HPInletAirHumRat, RoutineName );
			//   get rated coil bypass factor excluding fan heat
			FanElecPower = 0.0;
			//   call CalcHPWHDXCoil to determine DXCoil%RatedTotCap(1) for rated CBF calculation below
			CalcHPWHDXCoil( DXCoilNum, 1.0 );
			if ( MySizeFlag( DXCoilNum ) ) {
				SizeDXCoil( DXCoilNum );
				MySizeFlag( DXCoilNum ) = false;
			}

			Real64 const RatedAirMassFlowRateSeaLevel = DXCoil( DXCoilNum ).RatedAirVolFlowRate( 1 ) * PsyRhoAirFnPbTdbW( StdPressureSeaLevel, DXCoil( DXCoilNum ).RatedInletDBTemp, HPInletAirHumRat, RoutineName );
			DXCoil( DXCoilNum ).RatedCBF( 1 ) = CalcCBF( DXCoil( DXCoilNum ).DXCoilType, DXCoil( DXCoilNum ).Name, DXCoil( DXCoilNum ).RatedInletDBTemp, HPInletAirHumRat, DXCoil( DXCoilNum ).RatedTotCap( 1 ), RatedAirMassFlowRateSeaLevel, DXCoil( DXCoilNum ).RatedSHR( 1 ), true, StdPressureSeaLevel );
			MyEnvrnFlag( DXCoilNum ) = false;
		}

		if ( ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_MultiSpeedCooling || DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_MultiSpeedHeating ) && MyEnvrnFlag( DXCoilNum ) ) {
			if ( DXCoil( DXCoilNum ).FuelType != FuelTypeElectricity ) {
				if (DXCoil( DXCoilNum ).MSHPHeatRecActive ) {
					for ( SpeedNum = 1; SpeedNum <= DXCoil( DXCoilNum ).NumOfSpeeds; ++SpeedNum ) {
						if ( DXCoil( DXCoilNum ).MSWasteHeat( SpeedNum ) == 0 ) {
							ShowWarningError( "GetDXCoils:" + DXCoil( DXCoilNum ).Name + ". The value of Waste Heat Function of Temperature Curve is assumed to be 1. Simulation continues. " );
							break;
						}
					}
				}
			}
			MyEnvrnFlag( DXCoilNum ) = false;
		}

		// Find the companion upstream coil (DX cooling coil) that is used with DX heating coils (HP AC units only)
		if ( DXCoil( DXCoilNum ).FindCompanionUpStreamCoil ) {
			if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_HeatingEmpirical || DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_MultiSpeedHeating ) {
				DXCoil( DXCoilNum ).CompanionUpstreamDXCoil = GetHPCoolingCoilIndex( DXCoil( DXCoilNum ).DXCoilType, DXCoil( DXCoilNum ).Name, DXCoilNum );
				if ( DXCoil( DXCoilNum ).CompanionUpstreamDXCoil > 0 ) {
					DXCoil( DXCoil( DXCoilNum ).CompanionUpstreamDXCoil ).ReportCoolingCoilCrankcasePower = false;
					DXCoil( DXCoilNum ).FindCompanionUpStreamCoil = false;
					//       Copy condenser node number from DX cooling coil when used with a companion DX heating coil
					for ( Mode = 1; Mode <= MaxModes; ++Mode ) {
						DXCoil( DXCoilNum ).CondenserInletNodeNum( Mode ) = DXCoil( DXCoil( DXCoilNum ).CompanionUpstreamDXCoil ).CondenserInletNodeNum( Mode );
					}
				}
			} else {
				DXCoil( DXCoilNum ).FindCompanionUpStreamCoil = false;
			}
		} //IF(DXCoil(DXCoilNum)%FindCompanionUpStreamCoil)THEN

		// CR7308 - Wait for zone and air loop equipment to be simulated, then print out report variables
		if ( CrankcaseHeaterReportVarFlag ) {
			if ( AirLoopInputsFilled ) {
				//     Set report variables for DX cooling coils that will have a crankcase heater (all DX coils not used in a HP AC unit)
				for ( DXCoilNumTemp = 1; DXCoilNumTemp <= NumDoe2DXCoils + NumDXMulModeCoils; ++DXCoilNumTemp ) {
					if ( DXCoil( DXCoilNumTemp ).ReportCoolingCoilCrankcasePower ) {
						SetupOutputVariable( "Cooling Coil Crankcase Heater Electric Power [W]", DXCoil( DXCoilNumTemp ).CrankcaseHeaterPower, "System", "Average", DXCoil( DXCoilNumTemp ).Name );
						SetupOutputVariable( "Cooling Coil Crankcase Heater Electric Energy [J]", DXCoil( DXCoilNumTemp ).CrankcaseHeaterConsumption, "System", "Sum", DXCoil( DXCoilNumTemp ).Name, _, "Electric", "COOLING", _, "Plant" );
						DXCoil( DXCoilNumTemp ).ReportCoolingCoilCrankcasePower = false;
					}
				}
				CrankcaseHeaterReportVarFlag = false;
			} //(AirLoopInputsFilled)THEN
		} //(CrankcaseHeaterReportVarFlag)THEN

		// Find the companion upstream coil (DX cooling coil) that is used with DX heating coils (Multispeed HP units only)
		if ( DXCoil( DXCoilNum ).FindCompanionUpStreamCoil ) {
			if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_MultiSpeedHeating ) {
				DXCoil( DXCoilNum ).CompanionUpstreamDXCoil = GetHPCoolingCoilIndex( DXCoil( DXCoilNum ).DXCoilType, DXCoil( DXCoilNum ).Name, DXCoilNum );
				if ( DXCoil( DXCoilNum ).CompanionUpstreamDXCoil > 0 ) {
					DXCoil( DXCoil( DXCoilNum ).CompanionUpstreamDXCoil ).ReportCoolingCoilCrankcasePower = false;
					DXCoil( DXCoilNum ).FindCompanionUpStreamCoil = false;
				}
			} else {
				DXCoil( DXCoilNum ).FindCompanionUpStreamCoil = false;
			}
		} //IF(DXCoil(DXCoilNum)%FindCompanionUpStreamCoil)THEN

		if ( ! SysSizingCalc && MySizeFlag( DXCoilNum ) ) {
			// for each coil, do the sizing once.
			SizeDXCoil( DXCoilNum );
			MySizeFlag( DXCoilNum ) = false;

			if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_CoolingSingleSpeed || DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_CoolingTwoSpeed || DXCoil( DXCoilNum ).DXCoilType_Num == CoilVRF_Cooling || DXCoil( DXCoilNum ).DXCoilType_Num == CoilVRF_FluidTCtrl_Cooling) {

				Mode = 1;
				// Check for zero capacity or zero max flow rate
				if ( DXCoil( DXCoilNum ).RatedTotCap( Mode ) <= 0.0 ) {
					ShowSevereError( "Sizing: " + DXCoil( DXCoilNum ).DXCoilType + ' ' + DXCoil( DXCoilNum ).Name + " has zero rated total capacity" );
					ErrorsFound = true;
				}
				if ( DXCoil( DXCoilNum ).RatedAirVolFlowRate( Mode ) <= 0.0 ) {
					ShowSevereError( "Sizing: " + DXCoil( DXCoilNum ).DXCoilType + ' ' + DXCoil( DXCoilNum ).Name + " has zero rated air flow rate" );
					ErrorsFound = true;
				}
				if ( ErrorsFound ) {
					ShowFatalError( "Preceding condition causes termination." );
				}
				// Check for valid range of (Rated Air Volume Flow Rate / Rated Total Capacity)
				RatedVolFlowPerRatedTotCap = DXCoil( DXCoilNum ).RatedAirVolFlowRate( Mode ) / DXCoil( DXCoilNum ).RatedTotCap( Mode );
				if ( ( ( MinRatedVolFlowPerRatedTotCap( DXCT ) - RatedVolFlowPerRatedTotCap ) > SmallDifferenceTest ) || ( ( RatedVolFlowPerRatedTotCap - MaxRatedVolFlowPerRatedTotCap( DXCT ) ) > SmallDifferenceTest ) ) {
					ShowSevereError( "Sizing: " + DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + "\": Rated air volume flow rate per watt of rated total cooling capacity is out of range." );
					ShowContinueError( "Min Rated Vol Flow Per Watt=[" + TrimSigDigits( MinRatedVolFlowPerRatedTotCap( DXCT ), 3 ) + "], Rated Vol Flow Per Watt=[" + TrimSigDigits( RatedVolFlowPerRatedTotCap, 3 ) + "], Max Rated Vol Flow Per Watt=[" + TrimSigDigits( MaxRatedVolFlowPerRatedTotCap( DXCT ), 3 ) + "]. See Input Output Reference Manual for valid range." );
				}
				DXCoil( DXCoilNum ).RatedAirMassFlowRate( Mode ) = DXCoil( DXCoilNum ).RatedAirVolFlowRate( Mode ) * PsyRhoAirFnPbTdbW( StdBaroPress, RatedInletAirTemp, RatedInletAirHumRat, RoutineName );
				// get high speed rated coil bypass factor
				DXCoil( DXCoilNum ).RatedCBF( Mode ) = CalcCBF( DXCoil( DXCoilNum ).DXCoilType, DXCoil( DXCoilNum ).Name, RatedInletAirTemp, RatedInletAirHumRat, DXCoil( DXCoilNum ).RatedTotCap( Mode ), DXCoil( DXCoilNum ).RatedAirMassFlowRate( Mode ), DXCoil( DXCoilNum ).RatedSHR( Mode ) );

			}

			if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_CoolingTwoStageWHumControl ) {
				for ( DehumidModeNum = 0; DehumidModeNum <= DXCoil( DXCoilNum ).NumDehumidModes; ++DehumidModeNum ) {
					for ( CapacityStageNum = 1; CapacityStageNum <= DXCoil( DXCoilNum ).NumCapacityStages; ++CapacityStageNum ) {
						Mode = DehumidModeNum * 2 + CapacityStageNum;
						// Check for zero capacity or zero max flow rate
						if ( DXCoil( DXCoilNum ).RatedTotCap( Mode ) <= 0.0 ) {
							ShowSevereError( "Sizing: " + DXCoil( DXCoilNum ).DXCoilType + ' ' + DXCoil( DXCoilNum ).Name + " has zero rated total capacity" );
							ShowContinueError( "for CoilPerformance:DX:Cooling mode: " + DXCoil( DXCoilNum ).CoilPerformanceName( Mode ) );
							ErrorsFound = true;
						}
						if ( DXCoil( DXCoilNum ).RatedAirVolFlowRate( Mode ) <= 0.0 ) {
							ShowSevereError( "Sizing: " + DXCoil( DXCoilNum ).DXCoilType + ' ' + DXCoil( DXCoilNum ).Name + " has zero rated air flow rate" );
							ShowContinueError( "for CoilPerformance:DX:Cooling mode: " + DXCoil( DXCoilNum ).CoilPerformanceName( Mode ) );
							ErrorsFound = true;
						}
						if ( ErrorsFound ) {
							ShowFatalError( "Preceding condition causes termination." );
						}
						// Check for valid range of (Rated Air Volume Flow Rate / Rated Total Capacity)
						RatedVolFlowPerRatedTotCap = DXCoil( DXCoilNum ).RatedAirVolFlowRate( Mode ) / DXCoil( DXCoilNum ).RatedTotCap( Mode );
						if ( ( ( MinRatedVolFlowPerRatedTotCap( DXCT ) - RatedVolFlowPerRatedTotCap ) > SmallDifferenceTest ) || ( ( RatedVolFlowPerRatedTotCap - MaxRatedVolFlowPerRatedTotCap( DXCT ) ) > SmallDifferenceTest ) ) {
							ShowSevereError( "Sizing: " + DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + "\": Rated air volume flow rate per watt of rated total cooling capacity is out of range." );
							ShowContinueError( "Min Rated Vol Flow Per Watt=[" + TrimSigDigits( MinRatedVolFlowPerRatedTotCap( DXCT ), 3 ) + "], Rated Vol Flow Per Watt=[" + TrimSigDigits( RatedVolFlowPerRatedTotCap, 3 ) + "], Max Rated Vol Flow Per Watt=[" + TrimSigDigits( MaxRatedVolFlowPerRatedTotCap( DXCT ), 3 ) + "]. See Input Output Reference Manual for valid range." );
							ShowContinueError( "for CoilPerformance:DX:Cooling mode: " + DXCoil( DXCoilNum ).CoilPerformanceName( Mode ) );
						}
						DXCoil( DXCoilNum ).RatedAirMassFlowRate( Mode ) = DXCoil( DXCoilNum ).RatedAirVolFlowRate( Mode ) * PsyRhoAirFnPbTdbW( StdBaroPress, RatedInletAirTemp, RatedInletAirHumRat, RoutineName );
						// get rated coil bypass factor
						DXCoil( DXCoilNum ).RatedCBF( Mode ) = CalcCBF( DXCoil( DXCoilNum ).CoilPerformanceType( Mode ), DXCoil( DXCoilNum ).CoilPerformanceName( Mode ), RatedInletAirTemp, RatedInletAirHumRat, DXCoil( DXCoilNum ).RatedTotCap( Mode ), DXCoil( DXCoilNum ).RatedAirMassFlowRate( Mode ), DXCoil( DXCoilNum ).RatedSHR( Mode ) );
					} // End capacity stages loop
				} // End dehumidification modes loop

			}

			if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_HeatingEmpirical || DXCoil( DXCoilNum ).DXCoilType_Num == CoilVRF_Heating || DXCoil( DXCoilNum ).DXCoilType_Num == CoilVRF_FluidTCtrl_Heating ) {

				Mode = 1;
				if ( DXCoil( DXCoilNum ).RatedTotCap( Mode ) <= 0.0 ) {
					ShowSevereError( "Sizing: " + DXCoil( DXCoilNum ).DXCoilType + ' ' + DXCoil( DXCoilNum ).Name + " has zero rated total capacity" );
					ErrorsFound = true;
				}
				if ( DXCoil( DXCoilNum ).RatedAirVolFlowRate( Mode ) <= 0.0 ) {
					ShowSevereError( "Sizing: " + DXCoil( DXCoilNum ).DXCoilType + ' ' + DXCoil( DXCoilNum ).Name + " has zero rated air flow rate" );
					ErrorsFound = true;
				}
				if ( ErrorsFound ) {
					ShowFatalError( "Preceding condition causes termination." );
				}
				RatedHeatPumpIndoorAirTemp = 21.11; // 21.11C or 70F
				RatedHeatPumpIndoorHumRat = 0.00881; // Humidity ratio corresponding to 70F dry bulb/60F wet bulb
				DXCoil( DXCoilNum ).RatedAirMassFlowRate( Mode ) = DXCoil( DXCoilNum ).RatedAirVolFlowRate( Mode ) * PsyRhoAirFnPbTdbW( StdBaroPress, RatedHeatPumpIndoorAirTemp, RatedHeatPumpIndoorHumRat, RoutineName );
				// Check for valid range of (Rated Air Volume Flow Rate / Rated Total Capacity)
				RatedVolFlowPerRatedTotCap = DXCoil( DXCoilNum ).RatedAirVolFlowRate( Mode ) / DXCoil( DXCoilNum ).RatedTotCap( Mode );
				if ( ( ( MinRatedVolFlowPerRatedTotCap( DXCT ) - RatedVolFlowPerRatedTotCap ) > SmallDifferenceTest ) || ( ( RatedVolFlowPerRatedTotCap - MaxRatedVolFlowPerRatedTotCap( DXCT ) ) > SmallDifferenceTest ) ) {
					ShowSevereError( "Sizing: " + DXCoil( DXCoilNum ).DXCoilType + ' ' + DXCoil( DXCoilNum ).Name + ": Rated air volume flow rate per watt of rated total heating capacity is out of range." );
					ShowContinueError( "Min Rated Vol Flow Per Watt=[" + TrimSigDigits( MinRatedVolFlowPerRatedTotCap( DXCT ), 3 ) + "], Rated Vol Flow Per Watt=[" + TrimSigDigits( RatedVolFlowPerRatedTotCap, 3 ) + "], Max Rated Vol Flow Per Watt=[" + TrimSigDigits( MaxRatedVolFlowPerRatedTotCap( DXCT ), 3 ) + "]. See Input-Output Reference Manual for valid range." );
				}

			}

			if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_CoolingTwoSpeed ) {
				// Check for valid range of (Rated Air Volume Flow Rate / Rated Total Capacity)
				RatedVolFlowPerRatedTotCap = DXCoil( DXCoilNum ).RatedAirVolFlowRate2 / DXCoil( DXCoilNum ).RatedTotCap2;
				if ( ( ( MinRatedVolFlowPerRatedTotCap( DXCT ) - RatedVolFlowPerRatedTotCap ) > SmallDifferenceTest ) || ( ( RatedVolFlowPerRatedTotCap - MaxRatedVolFlowPerRatedTotCap( DXCT ) ) > SmallDifferenceTest ) ) {
					ShowSevereError( "Coil:Cooling:DX:TwoSpeed \"" + DXCoil( DXCoilNum ).Name + "\": At low speed rated air volume flow rate per watt of rated total cooling capacity is out of range." );
					ShowContinueError( "Min Rated Vol Flow Per Watt=[" + TrimSigDigits( MinRatedVolFlowPerRatedTotCap( DXCT ), 3 ) + "], Rated Vol Flow Per Watt=[" + TrimSigDigits( RatedVolFlowPerRatedTotCap, 3 ) + "], Max Rated Vol Flow Per Watt=[" + TrimSigDigits( MaxRatedVolFlowPerRatedTotCap( DXCT ), 3 ) + "]. See Input-Output Reference Manual for valid range." );
				}

				DXCoil( DXCoilNum ).RatedAirMassFlowRate2 = DXCoil( DXCoilNum ).RatedAirVolFlowRate2 * PsyRhoAirFnPbTdbW( StdBaroPress, RatedInletAirTemp, RatedInletAirHumRat, RoutineName );
				// get low speed rated coil bypass factor
				DXCoil( DXCoilNum ).RatedCBF2 = CalcCBF( DXCoil( DXCoilNum ).DXCoilType, DXCoil( DXCoilNum ).Name, RatedInletAirTemp, RatedInletAirHumRat, DXCoil( DXCoilNum ).RatedTotCap2, DXCoil( DXCoilNum ).RatedAirMassFlowRate2, DXCoil( DXCoilNum ).RatedSHR2 );

				// call for standard ratings for two-speeed DX coil
				if ( DXCoil( DXCoilNum ).CondenserType( 1 ) == AirCooled ) {
					CalcTwoSpeedDXCoilStandardRating( DXCoilNum );
				}
			}

			// Multispeed Cooling
			if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_MultiSpeedCooling ) {
				for ( Mode = 1; Mode <= DXCoil( DXCoilNum ).NumOfSpeeds; ++Mode ) {
					// Check for zero capacity or zero max flow rate
					if ( DXCoil( DXCoilNum ).MSRatedTotCap( Mode ) <= 0.0 ) {
						ShowSevereError( "Sizing: " + DXCoil( DXCoilNum ).DXCoilType + ' ' + DXCoil( DXCoilNum ).Name + " has zero rated total capacity at speed " + TrimSigDigits( Mode ) );
						ErrorsFound = true;
					}
					if ( DXCoil( DXCoilNum ).MSRatedAirVolFlowRate( Mode ) <= 0.0 ) {
						ShowSevereError( "Sizing: " + DXCoil( DXCoilNum ).DXCoilType + ' ' + DXCoil( DXCoilNum ).Name + " has zero rated air flow rate at speed " + TrimSigDigits( Mode ) );
						ErrorsFound = true;
					}
					if ( ErrorsFound ) {
						ShowFatalError( "Preceding condition causes termination." );
					}
					// Check for valid range of (Rated Air Volume Flow Rate / Rated Total Capacity)
					RatedVolFlowPerRatedTotCap = DXCoil( DXCoilNum ).MSRatedAirVolFlowRate( Mode ) / DXCoil( DXCoilNum ).MSRatedTotCap( Mode );
					if ( ( ( MinRatedVolFlowPerRatedTotCap( DXCT ) - RatedVolFlowPerRatedTotCap ) > SmallDifferenceTest ) || ( ( RatedVolFlowPerRatedTotCap - MaxRatedVolFlowPerRatedTotCap( DXCT ) ) > SmallDifferenceTest ) ) {
						ShowSevereError( "Sizing: " + DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + "\": Rated air volume flow rate per watt of rated total cooling capacity is out of range at speed " + TrimSigDigits( Mode ) );
						ShowContinueError( "Min Rated Vol Flow Per Watt=[" + TrimSigDigits( MinRatedVolFlowPerRatedTotCap( DXCT ), 3 ) + "], Rated Vol Flow Per Watt=[" + TrimSigDigits( RatedVolFlowPerRatedTotCap, 3 ) + "], Max Rated Vol Flow Per Watt=[" + TrimSigDigits( MaxRatedVolFlowPerRatedTotCap( DXCT ), 3 ) + "]. See Input Output Reference Manual for valid range." );
					}
					DXCoil( DXCoilNum ).MSRatedAirMassFlowRate( Mode ) = DXCoil( DXCoilNum ).MSRatedAirVolFlowRate( Mode ) * PsyRhoAirFnPbTdbW( StdBaroPress, RatedInletAirTemp, RatedInletAirHumRat, RoutineName );
					// get high speed rated coil bypass factor
					DXCoil( DXCoilNum ).MSRatedCBF( Mode ) = CalcCBF( DXCoil( DXCoilNum ).DXCoilType, DXCoil( DXCoilNum ).Name, RatedInletAirTemp, RatedInletAirHumRat, DXCoil( DXCoilNum ).MSRatedTotCap( Mode ), DXCoil( DXCoilNum ).MSRatedAirMassFlowRate( Mode ), DXCoil( DXCoilNum ).MSRatedSHR( Mode ) );
				}
			}

			// Multispeed Heating
			if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_MultiSpeedHeating ) {
				RatedHeatPumpIndoorAirTemp = 21.11; // 21.11C or 70F
				RatedHeatPumpIndoorHumRat = 0.00881; // Humidity ratio corresponding to 70F dry bulb/60F wet bulb
				for ( Mode = 1; Mode <= DXCoil( DXCoilNum ).NumOfSpeeds; ++Mode ) {

					DXCoil( DXCoilNum ).MSRatedAirMassFlowRate( Mode ) = DXCoil( DXCoilNum ).MSRatedAirVolFlowRate( Mode ) * PsyRhoAirFnPbTdbW( StdBaroPress, RatedHeatPumpIndoorAirTemp, RatedHeatPumpIndoorHumRat, RoutineName );
					// Check for valid range of (Rated Air Volume Flow Rate / Rated Total Capacity)
					RatedVolFlowPerRatedTotCap = DXCoil( DXCoilNum ).MSRatedAirVolFlowRate( Mode ) / DXCoil( DXCoilNum ).MSRatedTotCap( Mode );
					if ( ( ( MinRatedVolFlowPerRatedTotCap( DXCT ) - RatedVolFlowPerRatedTotCap ) > SmallDifferenceTest ) || ( ( RatedVolFlowPerRatedTotCap - MaxRatedVolFlowPerRatedTotCap( DXCT ) ) > SmallDifferenceTest ) ) {
						ShowSevereError( "Coil:Heating:DX:MultiSpeed " + DXCoil( DXCoilNum ).Name + ": Rated air volume flow rate per watt of rated total heating capacity is out of range at speed " + TrimSigDigits( Mode ) );
						ShowContinueError( "Min Rated Vol Flow Per Watt=[" + TrimSigDigits( MinRatedVolFlowPerRatedTotCap( DXCT ), 3 ) + "], Rated Vol Flow Per Watt=[" + TrimSigDigits( RatedVolFlowPerRatedTotCap, 3 ) + "], Max Rated Vol Flow Per Watt=[" + TrimSigDigits( MaxRatedVolFlowPerRatedTotCap( DXCT ), 3 ) + "]. See Input Output Reference Manual for valid range." );
					}
				}
			}

		}

		AirInletNode = DXCoil( DXCoilNum ).AirInNode;

		// Each iteration, load the coil data structure with the inlet conditions

		DXCoil( DXCoilNum ).InletAirMassFlowRate = Node( AirInletNode ).MassFlowRate;
		DXCoil( DXCoilNum ).InletAirMassFlowRateMax = max( Node( AirInletNode ).MassFlowRateMax, Node( AirInletNode ).MassFlowRate );
		DXCoil( DXCoilNum ).InletAirTemp = Node( AirInletNode ).Temp;
		DXCoil( DXCoilNum ).InletAirHumRat = Node( AirInletNode ).HumRat;
		DXCoil( DXCoilNum ).InletAirEnthalpy = Node( AirInletNode ).Enthalpy;
		//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
		//  DXCoil(DXCoilNum)%InletAirPressure        = Node(AirInletNode)%Press

		if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_HeatingEmpirical || DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_MultiSpeedHeating ) {
			if ( DXCoil( DXCoilNum ).IsSecondaryDXCoilInZone ) {
				DXCoil( DXCoilNum ).EvapInletWetBulb = PsyTwbFnTdbWPb( ZT( DXCoil( DXCoilNum ).SecZonePtr ), ZoneAirHumRat( DXCoil( DXCoilNum ).SecZonePtr ), OutBaroPress, RoutineName );
			}
		}

		if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_HeatPumpWaterHeaterPumped || DXCoil(DXCoilNum).DXCoilType_Num == CoilDX_HeatPumpWaterHeaterWrapped ) {
			DXCoil( DXCoilNum ).TotalHeatingEnergyRate = 0.0;
			DXCoil( DXCoilNum ).ElecWaterHeatingPower = 0.0;
			//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
			//  DXCoil(DXCoilNum)%InletAirPressure         = StdBaroPress

			//   HPWH's that use an inlet air temperature schedule also need to have a valid barometric pressure
			//   The DX Coil used in HPWH's does not know if it is using a scheduled inlet temperature so check the node pressure
			if ( DXCoil( DXCoilNum ).CondenserInletNodeNum( 1 ) > 0 ) {
				if ( Node( DXCoil( DXCoilNum ).CondenserInletNodeNum( 1 ) ).Press == 0.0 ) {
					Node( DXCoil( DXCoilNum ).CondenserInletNodeNum( 1 ) ).Press = StdBaroPress;
				}
			}

		}
		DXCoil( DXCoilNum ).BasinHeaterPower = 0.0;

		if ( DXCoil( DXCoilNum ).IsSecondaryDXCoilInZone ) {
			DXCoil( DXCoilNum ).CompressorPartLoadRatio = 0.0;
			DXCoil( DXCoilNum ).SecCoilSensibleHeatGainRate = 0.0;
			DXCoil( DXCoilNum ).SecCoilTotalHeatRemovalRate = 0.0;
			DXCoil( DXCoilNum ).SecCoilSensibleHeatRemovalRate = 0.0;
			DXCoil( DXCoilNum ).SecCoilLatentHeatRemovalRate = 0.0;
		}

	}

	void
	SizeDXCoil( int const DXCoilNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   January 2002
		//                      Feb 2005, M. J. Witte, GARD Analytics, Inc. Add new coil type COIL:DX:MultiMode:CoolingEmpirical.
		//                      Jul 2005, R. Raustad, FSEC. Add new coil type COIL:DX:HEATPUMPWATERHEATER
		//                      Jun 2007, L. Gu, FSEC. Add new coil type COIL:DX:MULTISPEED:COOLING and HEATING
		//                      Jan 2011, B. Griffithn, NREL. add EMS overrides for autosized fields
		//                      Aug 2013, D. Kang. add component sizing table entries
		//                      May 2014, R. Raustad, FSEC. moved sizing calculations to common routine
		//                      Aug 2015, R. Zhang, LBNL. Add new coil types for VRF_FluidTCtrl
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing DX Coil components for which nominal capacity and air flow rate
		// have not been specified in the input.

		// METHODOLOGY EMPLOYED:
		// Obtains cooling capacities and air flow rates from the zone or system sizing arrays.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSizing;
		using CurveManager::CurveValue;
		using General::RoundSigDigits;
		using General::TrimSigDigits;
		using ReportSizingManager::ReportSizingOutput;
		using ReportSizingManager::RequestSizing;
		using namespace OutputReportPredefined;
		using DataAirSystems::PrimaryAirSystem;
		using StandardRatings::CalcDXCoilStandardRating;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "SizeDXCoil" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 rhoair;
		Real64 MixTemp;
		Real64 MixHumRat;
		Real64 MixEnth;
		Real64 MixWetBulb;
		Real64 SupTemp;
		Real64 SupHumRat;
		Real64 SupEnth;
		Real64 OutTemp;
		Real64 OutAirFrac;
		Real64 CoilInTemp;
		Real64 VolFlowRate;
		Real64 CoolCapAtPeak;
		Real64 TotCapTempModFac;
		Real64 RatedVolFlowPerRatedTotCap; // Rated Air Volume Flow Rate divided by Rated Total Capacity[m3/s-W)
		int TimeStepNumAtMax;
		int DDNum;
		int CapacityStageNum; // Loop index for 1,Number of capacity stages
		int DehumidModeNum; // Loop index for 1,Number of enhanced dehumidification modes
		int Mode; // Operating mode for MultiMode DX coil; Always 1 for other coil types
		int NumOfSpeedCompanion; // Number of speed for a companion cooling coil (Multispeed HO heating coil only
		std::string equipName;
		Real64 RatedAirVolFlowRateDes; // Design rated air volume flow for reporting
		Real64 RatedAirVolFlowRateUser; // Hard-sized rated air volume flow for reporting
		Real64 RatedAirVolFlowRate2Des; // Design rated low speed air volume flow for reporting
		Real64 RatedAirVolFlowRate2User; // Hard-sized rated low speed air volume flow for reporting
		Real64 RatedTotCapDes; // Design rated total capacity for reproting
		Real64 RatedTotCapUser; // Hard-sized rated total capacity for reproting
		Real64 RatedTotCap2Des; // Design rated low speed total capacity for reproting
		Real64 RatedTotCap2User; // Hard-sized rated low speed total capacity for reproting
		Real64 RatedSHRDes; // Design ratd SHR for reporting
		Real64 RatedSHRUser; // Hard-sized ratd SHR for reporting
		Real64 RatedSHR2Des; // Design ratd low speed SHR for reporting
		Real64 RatedSHR2User; // Hard-sized ratd low speed SHR for reporting
		Real64 EvapCondAirFlowDes; // Design evaporative condenser air flow for reporting
		Real64 EvapCondAirFlowUser; // Hard-sized evaporative condenser air flow for reporting
		Real64 EvapCondAirFlow2Des; // Design low speed evaporative condenser air flow for reporting
		Real64 EvapCondAirFlow2User; // Hard-sized low speed evaporative condenser air flow for reporting
		Real64 EvapCondPumpElecNomPowerDes; // Design evaporative condenser pump rated power consumption for reporting
		Real64 EvapCondPumpElecNomPowerUser; // Hard-sized evaporative condenser pump rated power consumption for reporting
		Real64 EvapCondPumpElecNomPower2Des; // Design low speed condenser pump rated power consumption for reporting
		Real64 EvapCondPumpElecNomPower2User; // Hard-sized low speed condenser pump rated power consumption for reporting
		Real64 DefrostCapacityDes; // Design defrost heater capacity for reporting
		Real64 DefrostCapacityUser; // Hard-sized defrost heater capacity for reporting
		Real64 MSRatedAirVolFlowRateDes; // Design multispeed rated air volume flow rate for reporting
		Real64 MSRatedAirVolFlowRateUser; // Hard-sized multispeed rated air volume flow rate for reporting
		Real64 MSRatedTotCapDes; // Design multispeed rated total capacity for reporting
		Real64 MSRatedTotCapUser; // Hard-sized multispeed rated total capacity for reporting
		Real64 MSRatedSHRDes; // Design multispeed rated SHR for reporting
		Real64 MSRatedSHRUser; // Hard-sized multispeed rated SHR for reporting
		Real64 MSEvapCondAirFlowDes; // Design evaporative condenser air flow for reporting
		Real64 MSEvapCondAirFlowUser; // Hard-sized evaporative condenser air flow for reporting
		Real64 MSEvapCondAirFlow2Des; // Design low speed evaporative condenser air flow for reporting
		Real64 MSEvapCondAirFlow2User; // Hard-sized low speed evaporative condenser air flow for reporting
		Real64 MSEvapCondPumpElecNomPowerDes; // Design evaporative condenser pump rated power consumption for reporting
		Real64 MSEvapCondPumpElecNomPowerUser; // Hard-sized evaporative condenser pump rated power consumption for reporting
		Real64 MSEvapCondPumpElecNomPower2Des; // Design low speed condenser pump rated power consumption for reporting
		Real64 MSEvapCondPumpElecNomPower2User; // Hard-sized low speed condenser pump rated power consumption for reporting
		Real64 MSDefrostCapacityDes; // Design defrost heater capacity for reporting
		Real64 MSDefrostCapacityUser; // Hard-sized defrost heater capacity for reporting
		bool HardSizeNoDesRun; // Indicator to a hard-sized field with no design sizing data
		bool IsAutoSize; // Indicator to autosize for reporting
		bool IsCoolCoilCapAutoSize; // Indicator to cooling capacity autosize for reporting
		bool SizingDesRunThisAirSys; // true if a particular air system had a Sizing:System object and system sizing done
		bool SizingDesRunThisZone; // true if a particular zone had a Sizing:Zone object and zone sizing was done
		std::string CompName; // component name
		std::string	CompType; // component type
		std::string SizingString; // input field sizing description (e.g., Nominal Capacity)
		bool bPRINT = true; // TRUE if sizing is reported to output (eio)
		Real64 TempSize; // autosized value of coil input field
		int FieldNum = 2; // IDD numeric field number where input field description is found
		int SizingMethod; // Integer representation of sizing method (e.g., CoolingAirflowSizing, HeatingCapacitySizing, etc.)
		bool PrintFlag; // TRUE when sizing information is reported in the eio file
		bool SizeSecDXCoil; // if true do sizing calculation for secondary coil
		Real64 SecCoilAirFlowDes; // Design secondary DX coil air flow for reporting
		Real64 SecCoilAirFlowUser; // Hard-sized secondary DX coil air flow for reporting

		// Initiate all reporting variables
		if ( SysSizingRunDone || ZoneSizingRunDone ) {
			HardSizeNoDesRun = false;
		} else {
			HardSizeNoDesRun = true;
		}

		if ( CurSysNum > 0 ) {
			CheckThisAirSystemForSizing( CurSysNum, SizingDesRunThisAirSys );
		} else {
			SizingDesRunThisAirSys = false;
		}
		if ( CurZoneEqNum > 0 ) {
			CheckThisZoneForSizing( CurZoneEqNum, SizingDesRunThisZone );
		} else {
			SizingDesRunThisZone = false;
		}

		IsAutoSize = false;
		IsCoolCoilCapAutoSize = false;
		SizeSecDXCoil = false;
		RatedAirVolFlowRateDes = 0.0;
		RatedAirVolFlowRateUser = 0.0;
		RatedAirVolFlowRate2Des = 0.0;
		RatedAirVolFlowRate2User = 0.0;
		RatedTotCapDes = 0.0;
		RatedTotCapUser = 0.0;
		RatedTotCap2Des = 0.0;
		RatedTotCap2User = 0.0;
		RatedSHRDes = 0.0;
		RatedSHRUser = 0.0;
		RatedSHR2Des = 0.0;
		RatedSHR2User = 0.0;
		EvapCondAirFlowDes = 0.0;
		EvapCondAirFlowUser = 0.0;
		EvapCondAirFlow2Des = 0.0;
		EvapCondAirFlow2User = 0.0;
		EvapCondPumpElecNomPowerDes = 0.0;
		EvapCondPumpElecNomPowerUser = 0.0;
		EvapCondPumpElecNomPower2Des = 0.0;
		EvapCondPumpElecNomPower2User = 0.0;
		DefrostCapacityDes = 0.0;
		DefrostCapacityUser = 0.0;
		MSRatedAirVolFlowRateDes = 0.0;
		MSRatedAirVolFlowRateUser = 0.0;
		MSRatedTotCapDes = 0.0;
		MSRatedTotCapUser = 0.0;
		MSRatedSHRDes = 0.0;
		MSRatedSHRUser = 0.0;
		MSEvapCondAirFlowDes = 0.0;
		MSEvapCondAirFlowUser = 0.0;
		MSEvapCondAirFlow2Des = 0.0;
		MSEvapCondAirFlow2User = 0.0;
		MSEvapCondPumpElecNomPowerDes = 0.0;
		MSEvapCondPumpElecNomPowerUser = 0.0;
		MSEvapCondPumpElecNomPower2Des = 0.0;
		MSEvapCondPumpElecNomPower2User = 0.0;
		MSDefrostCapacityDes = 0.0;
		MSDefrostCapacityUser = 0.0;
		SecCoilAirFlowDes = 0.0;
		SecCoilAirFlowUser = 0.0;

		//  EXTERNAL ReportSizingOutput

		// NOTE: we are sizing COIL:DX:HeatingEmpirical on the COOLING load. Thus the cooling and
		// and heating capacities of a DX heat pump system will be identical. In real life the AHRI
		// heating and cooling capacities are close but not identical.
		for ( DehumidModeNum = 0; DehumidModeNum <= DXCoil( DXCoilNum ).NumDehumidModes; ++DehumidModeNum ) {
			for ( CapacityStageNum = 1; CapacityStageNum <= DXCoil( DXCoilNum ).NumCapacityStages; ++CapacityStageNum ) {
				Mode = DehumidModeNum * 2 + CapacityStageNum;
				if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_HeatPumpWaterHeaterPumped || DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_HeatPumpWaterHeaterWrapped ) {
					if ( DXCoil( DXCoilNum ).RatedAirVolFlowRate( 1 ) == AutoCalculate ) {
						// report autocalculated sizing
						PrintFlag = true;
						CompName = DXCoil( DXCoilNum ).Name;
						CompType = DXCoil( DXCoilNum ).DXCoilType;
						FieldNum = 7;
						SizingString = DXCoilNumericFields( DXCoilNum ).PerfMode( Mode ).FieldNames( FieldNum ) + " [m3/s]";
						SizingMethod = AutoCalculateSizing;
						// DXCoil( DXCoilNum ).RatedAirVolFlowRate( 1 ) = DXCoil( DXCoilNum ).RatedTotCap2 * 0.00005035
						DataConstantUsedForSizing = DXCoil( DXCoilNum ).RatedTotCap2;
						DataFractionUsedForSizing = 0.00005035;
						TempSize = AutoSize;
						RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
						DXCoil( DXCoilNum ).RatedAirVolFlowRate( 1 ) = TempSize;
						PrintFlag = false;
						DataConstantUsedForSizing = 0.0;
						DataFractionUsedForSizing = 0.0;
					}

					if ( DXCoil( DXCoilNum ).RatedHPWHCondWaterFlow == AutoCalculate ) {
						// report autocalculated sizing
						PrintFlag = true;
						CompName = DXCoil( DXCoilNum ).Name;
						CompType = DXCoil( DXCoilNum ).DXCoilType;
						FieldNum = 8;
						SizingString = DXCoilNumericFields( DXCoilNum ).PerfMode( Mode ).FieldNames( FieldNum ) + " [m3/s]";
						SizingMethod = AutoCalculateSizing;
						// DXCoil( DXCoilNum ).RatedAirVolFlowRate( 1 ) = DXCoil( DXCoilNum ).RatedTotCap2 * 0.00000004487
						DataConstantUsedForSizing = DXCoil( DXCoilNum ).RatedTotCap2;
						DataFractionUsedForSizing = 0.00000004487;
						TempSize = AutoSize;
						RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
						DXCoil( DXCoilNum ).RatedHPWHCondWaterFlow = TempSize;
						PrintFlag = false;
						DataConstantUsedForSizing = 0.0;
						DataFractionUsedForSizing = 0.0;
					}
				} else {
					PrintFlag = true;
					FieldNum = 0;
					if( DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_CoolingTwoStageWHumControl ) {
						SizingMethod = CoolingAirflowSizing;
						CompName = DXCoil( DXCoilNum ).Name + ":" + DXCoil( DXCoilNum ).CoilPerformanceName( Mode );
						FieldNum = 4;
						DataBypassFrac = DXCoil ( DXCoilNum ).BypassedFlowFrac ( Mode );
					} else if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_HeatingEmpirical ) {
						SizingMethod = HeatingAirflowSizing;
						CompName = DXCoil( DXCoilNum ).Name;
						FieldNum = 3;
						// doesn't look like this is needed for air flow sizing, only for heating capacity sizing
						DataCoolCoilCap = DXCoolCap; // pass global variable used only for heat pumps (i.e., DX cooling and heating coils)
						if ( ( DXCoil( DXCoilNum ).IsSecondaryDXCoilInZone ) && ( DXCoil( DXCoilNum ).CondenserType( 1 ) == AirCooled ) ) { // seconday DX coil in secondary zone is specified
							SizeSecDXCoil = true;
						}
					} else if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilVRF_Heating ) {
						SizingMethod = HeatingAirflowSizing;
						CompName = DXCoil( DXCoilNum ).Name;
						FieldNum = 2;
					} else if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilVRF_Cooling ) {
						SizingMethod = CoolingAirflowSizing;
						CompName = DXCoil( DXCoilNum ).Name;
						FieldNum = 3;
					} else if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilVRF_FluidTCtrl_Heating ) {
						SizingMethod = HeatingAirflowSizing;
						CompName = DXCoil( DXCoilNum ).Name;
					} else if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilVRF_FluidTCtrl_Cooling ) {
						SizingMethod = CoolingAirflowSizing;
						CompName = DXCoil( DXCoilNum ).Name;
					} else if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_MultiSpeedCooling ) {
						SizingMethod = CoolingAirflowSizing;
						CompName = DXCoil( DXCoilNum ).Name;
						PrintFlag = false;
					} else if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_MultiSpeedHeating ) {
						SizingMethod = HeatingAirflowSizing;
						CompName = DXCoil( DXCoilNum ).Name;
						PrintFlag = false;
					} else {
						SizingMethod = CoolingAirflowSizing;
						CompName = DXCoil( DXCoilNum ).Name;
						FieldNum = 4;
					}

					TempSize = DXCoil( DXCoilNum ).RatedAirVolFlowRate( Mode );
					if( FieldNum > 0 ){
						SizingString = DXCoilNumericFields( DXCoilNum ).PerfMode( Mode ).FieldNames( FieldNum ) + " [m3/s]";
					} else {
						SizingString = "Rated Air Flow Rate [m3/s]";
					}
					CompType = DXCoil( DXCoilNum ).DXCoilType;
					DataIsDXCoil = true;
					DataEMSOverrideON = DXCoil ( DXCoilNum ).RatedAirVolFlowRateEMSOverrideON ( Mode );
					DataEMSOverride = DXCoil( DXCoilNum ).RatedAirVolFlowRateEMSOverrideValue( Mode );
					RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
					DXCoil ( DXCoilNum ).RatedAirVolFlowRate ( Mode ) = TempSize;
					DataIsDXCoil = false;
					DataEMSOverrideON = false;
					DataEMSOverride = 0.0;
					DataBypassFrac = 0.0;
				}

				DataFlowUsedForSizing = DXCoil ( DXCoilNum ).RatedAirVolFlowRate ( Mode );
				// get autosized air flow for capacity calc's if capacity is not autosized
				// *** RAR this if block is a last minute addition to correct capacity reporting when not autosized and a sizing run is done. Test suite was not run with this code included. ***
				// The question here is if the autosized air flow rate or the user specified air flow rate should be used to calculate capacity
				// removing this for now until more is known
//				if ( DXCoil( DXCoilNum ).RatedTotCap( Mode ) != AutoSize && ( ( SysSizingRunDone && CurSysNum > 0 ) || ( ZoneSizingRunDone && CurZoneEqNum > 0 ) ) ) {
//					if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_CoolingTwoStageWHumControl ) {
//						SizingMethod = CoolingAirflowSizing;
//						DataBypassFrac = DXCoil ( DXCoilNum ).BypassedFlowFrac ( Mode );
//					} else if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_HeatingEmpirical ) {
//						SizingMethod = HeatingAirflowSizing;
////						DataCoolCoilCap = DXCoolCap; // pass global variable used only for heat pumps (i.e., DX cooling and heating coils)
//					} else if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilVRF_Heating ) {
//						SizingMethod = HeatingAirflowSizing;
//					} else if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilVRF_Cooling ) {
//						SizingMethod = CoolingAirflowSizing;
//					} else {
//						SizingMethod = CoolingAirflowSizing;
//					}
//					CompName = DXCoil( DXCoilNum ).Name;
//					TempSize = AutoSize;
//					SizingString.clear(); // don't care
//					CompType = DXCoil( DXCoilNum ).DXCoilType;
//					DataIsDXCoil = true;
//					DataEMSOverrideON = DXCoil ( DXCoilNum ).RatedAirVolFlowRateEMSOverrideON ( Mode );
//					DataEMSOverride = DXCoil( DXCoilNum ).RatedAirVolFlowRateEMSOverrideValue( Mode );
//					RequestSizing( CompType, CompName, SizingMethod, trim(SizingString ), TempSize, false , RoutineName );
//					DataFlowUsedForSizing = TempSize;
//					DataIsDXCoil = false; // don't need this and next 2, they are just overwritten below. Delete on next pass so testing will show problems if any.
//					DataEMSOverrideON = false;
//					DataEMSOverride = 0.0;
//					DataBypassFrac = 0.0;
//				}
				PrintFlag = true;
				DataTotCapCurveIndex = DXCoil( DXCoilNum ).CCapFTemp( Mode );
				if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_CoolingTwoStageWHumControl ) {
					SizingMethod = CoolingCapacitySizing;
					CompName = DXCoil( DXCoilNum ).Name + ":" + DXCoil( DXCoilNum ).CoilPerformanceName( Mode );
					FieldNum = 1;
					TempSize = DXCoil( DXCoilNum ).RatedTotCap( Mode );
					SizingString = DXCoilNumericFields( DXCoilNum ).PerfMode( Mode ).FieldNames( FieldNum ) + " [W]";
				} else if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_HeatingEmpirical || DXCoil( DXCoilNum ).DXCoilType_Num == CoilVRF_Heating || DXCoil( DXCoilNum ).DXCoilType_Num == CoilVRF_FluidTCtrl_Heating ) {
					SizingMethod = HeatingCapacitySizing;
					CompName = DXCoil( DXCoilNum ).Name;
					FieldNum = 1;
					TempSize = DXCoil( DXCoilNum ).RatedTotCap( Mode );
					SizingString = DXCoilNumericFields( DXCoilNum ).PerfMode( Mode ).FieldNames( FieldNum ) + " [W]";
					DataCoolCoilCap = DXCoolCap;
				} else if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_HeatPumpWaterHeaterPumped || DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_HeatPumpWaterHeaterWrapped ) {
					SizingMethod = CoolingCapacitySizing;
					CompName = DXCoil( DXCoilNum ).Name;
					FieldNum = 1;
					TempSize = DXCoil( DXCoilNum ).RatedTotCap( Mode );
					SizingString = DXCoilNumericFields( DXCoilNum ).PerfMode( Mode ).FieldNames( FieldNum ) + " [W]";
					PrintFlag = false;
				} else if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilVRF_FluidTCtrl_Cooling ) {
					SizingMethod = CoolingCapacitySizing;
					CompName = DXCoil( DXCoilNum ).Name;
					FieldNum = 1;
					TempSize = DXCoil( DXCoilNum ).RatedTotCap( Mode );
					SizingString = DXCoilNumericFields( DXCoilNum ).PerfMode( Mode ).FieldNames( FieldNum ) + " [W]";
					CoilInTemp = ZoneSizingRunDone ? FinalZoneSizing ( CurZoneEqNum ).DesCoolCoilInTemp : 26;
					CalcVRFCoilCapModFac( 0, _, CompName, CoilInTemp, _, _, _, DataTotCapCurveValue);
				} else if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_MultiSpeedCooling ) {
					SizingMethod = CoolingCapacitySizing;
					CompName = DXCoil( DXCoilNum ).Name;
					FieldNum = 6 + ( DXCoil( DXCoilNum ).NumOfSpeeds - 1 ) * 13;
					DataTotCapCurveIndex = DXCoil ( DXCoilNum ).MSCCapFTemp ( Mode );
					TempSize = DXCoil( DXCoilNum ).MSRatedTotCap( Mode );
					PrintFlag = false;
					SizingString = DXCoilNumericFields( DXCoilNum ).PerfMode( Mode ).FieldNames( FieldNum ) + " [W]";
				} else if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_MultiSpeedHeating ) {
					SizingMethod = HeatingCapacitySizing;
					CompName = DXCoil( DXCoilNum ).Name;
					FieldNum = 10 + ( DXCoil( DXCoilNum ).NumOfSpeeds - 1 ) * 5;
					DataTotCapCurveIndex = DXCoil ( DXCoilNum ).MSCCapFTemp ( Mode );
					TempSize = DXCoil( DXCoilNum ).MSRatedTotCap( Mode );
					PrintFlag = false;
					SizingString = DXCoilNumericFields( DXCoilNum ).PerfMode( Mode ).FieldNames( FieldNum ) + " [W]";
				} else {
					SizingMethod = CoolingCapacitySizing;
					CompName = DXCoil( DXCoilNum ).Name;
					FieldNum = 1;
					TempSize = DXCoil( DXCoilNum ).RatedTotCap( Mode );
					SizingString = DXCoilNumericFields( DXCoilNum ).PerfMode( Mode ).FieldNames( FieldNum ) + " [W]";
				}
				CompType = DXCoil( DXCoilNum ).DXCoilType;
				DataIsDXCoil = true;
				DataEMSOverrideON = DXCoil( DXCoilNum ).RatedTotCapEMSOverrideOn( Mode );
				DataEMSOverride = DXCoil( DXCoilNum ).RatedTotCapEMSOverrideValue( Mode );
				RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
				DXCoil( DXCoilNum ).RatedTotCap ( Mode ) = TempSize;
				DataIsDXCoil = false;
				DataFlowUsedForSizing = 0.0;
				DataCoolCoilCap = 0.0;
				DataTotCapCurveIndex = 0;
				DataEMSOverrideON = false;
				DataEMSOverride = 0.0;
				DataConstantUsedForSizing = 0.0;
				DataFractionUsedForSizing = 0.0;
				DataTotCapCurveValue = 0.0;

				// Cooling coil capacity
				if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_CoolingSingleSpeed || DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_CoolingTwoSpeed || DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_CoolingTwoStageWHumControl || DXCoil( DXCoilNum ).DXCoilType_Num == CoilVRF_Cooling || DXCoil( DXCoilNum ).DXCoilType_Num == CoilVRF_FluidTCtrl_Cooling ) {
					DXCoolCap = DXCoil( DXCoilNum ).RatedTotCap( Mode );
				}

				// Sizing DX cooling coil SHR
				if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_CoolingSingleSpeed || DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_CoolingTwoSpeed || DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_CoolingTwoStageWHumControl || DXCoil( DXCoilNum ).DXCoilType_Num == CoilVRF_Cooling || DXCoil( DXCoilNum ).DXCoilType_Num == CoilVRF_FluidTCtrl_Cooling ) {

					if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_CoolingTwoStageWHumControl ) {
						CompName = DXCoil( DXCoilNum ).Name + ":" + DXCoil( DXCoilNum ).CoilPerformanceName( Mode );
					} else {
						CompName = DXCoil( DXCoilNum ).Name;
					}
					SizingMethod = CoolingSHRSizing;
					CompType = DXCoil( DXCoilNum ).DXCoilType;
					FieldNum = 2;
					TempSize = DXCoil( DXCoilNum ).RatedSHR( Mode );
					SizingString = DXCoilNumericFields( DXCoilNum ).PerfMode( Mode ).FieldNames( FieldNum );
					DataFlowUsedForSizing = DXCoil ( DXCoilNum ).RatedAirVolFlowRate ( Mode );
					DataCapacityUsedForSizing = DXCoil ( DXCoilNum ).RatedTotCap ( Mode );
					DataEMSOverrideON = DXCoil( DXCoilNum ).RatedSHREMSOverrideOn( Mode );
					DataEMSOverride = DXCoil( DXCoilNum ).RatedSHREMSOverrideValue( Mode );
					RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, bPRINT, RoutineName );
					DXCoil ( DXCoilNum ).RatedSHR ( Mode ) = TempSize;
					DataFlowUsedForSizing = 0.0;
					DataCapacityUsedForSizing = 0.0;
					DataEMSOverrideON = false;
					DataEMSOverride = 0.0;

				} // End of Rated SHR

				// Sizing evaporator condenser air flow
				if ( DXCoil( DXCoilNum ).CondenserType( 1 ) == EvapCooled && DXCoil( DXCoilNum ).EvapCondAirFlow( Mode ) != 0.0 && ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_CoolingSingleSpeed || DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_CoolingTwoSpeed || DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_CoolingTwoStageWHumControl ) ) {

					if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_CoolingTwoStageWHumControl ) {
						CompName = DXCoil( DXCoilNum ).Name + ":" + DXCoil( DXCoilNum ).CoilPerformanceName( Mode );
						FieldNum = 11;
						SizingString = DXCoilNumericFields( DXCoilNum ).PerfMode( Mode ).FieldNames( FieldNum ) + " [m3/s]";
					} else {
						CompName = DXCoil( DXCoilNum ).Name;
						FieldNum = 11;
						SizingString = DXCoilNumericFields( DXCoilNum ).PerfMode( Mode ).FieldNames( FieldNum ) + " [m3/s]";
					}
					SizingMethod = AutoCalculateSizing;
					CompType = DXCoil( DXCoilNum ).DXCoilType;
//					// Auto size condenser air flow to Total Capacity * 0.000114 m3/s/w (850 cfm/ton)
					DataConstantUsedForSizing = DXCoil( DXCoilNum ).RatedTotCap( Mode );
					DataFractionUsedForSizing = 0.000114;
					TempSize = DXCoil( DXCoilNum ).EvapCondAirFlow( Mode );
					RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, bPRINT, RoutineName );
					DXCoil( DXCoilNum ).EvapCondAirFlow( Mode ) = TempSize;
					DataConstantUsedForSizing = 0.0;
					DataFractionUsedForSizing = 0.0;
				}

				if ( SizeSecDXCoil ) { // autosize secondary coil air flow rate for AirCooled condenser type
					IsAutoSize = false;
					if ( DXCoil( DXCoilNum ).SecCoilAirFlow == AutoSize ) {
						IsAutoSize = true;
					}
					// Auto size Primary Coil Air Flow * Secondary Coil Scaling Factor
					SecCoilAirFlowDes = DXCoil( DXCoilNum ).RatedAirVolFlowRate( 1 ) * DXCoil( DXCoilNum ).SecCoilAirFlowScalingFactor;
					if ( IsAutoSize ) {
						DXCoil( DXCoilNum ).SecCoilAirFlow = SecCoilAirFlowDes;
						ReportSizingOutput( DXCoil( DXCoilNum ).DXCoilType, DXCoil( DXCoilNum ).Name, "Design Size Secondary Coil Air Flow Rate [m3/s]", SecCoilAirFlowDes );
					} else {
						if ( DXCoil( DXCoilNum ).SecCoilAirFlow > 0.0 && SecCoilAirFlowDes > 0.0 && !HardSizeNoDesRun ) {
							SecCoilAirFlowUser = DXCoil( DXCoilNum ).SecCoilAirFlow;
							ReportSizingOutput( DXCoil( DXCoilNum ).DXCoilType, DXCoil( DXCoilNum ).Name, "Design Size Secondary Coil Air Flow Rate [m3/s]", SecCoilAirFlowDes, "User-Specified Secondary Coil Air Flow Rate [m3/s]", SecCoilAirFlowUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( SecCoilAirFlowDes - SecCoilAirFlowUser ) / SecCoilAirFlowUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeDxCoil: Potential issue with equipment sizing for " + DXCoil( DXCoilNum ).DXCoilType + ' ' + DXCoil( DXCoilNum ).Name );
									ShowContinueError( "User-Specified Secondary Coil Air Flow Rate of " + RoundSigDigits( SecCoilAirFlowUser, 5 ) + " [m3/s]" );
									ShowContinueError( "differs from Design Size Secondary Coil Air Flow Rate of " + RoundSigDigits( SecCoilAirFlowDes, 5 ) + " [m3/s]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
					}
				}

				// Sizing evaporative condenser air flow 2
				PrintFlag = true;
				if ( DXCoil( DXCoilNum ).CondenserType( 1 ) == EvapCooled && DXCoil( DXCoilNum ).EvapCondAirFlow2 != 0.0 && DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_CoolingTwoSpeed ) {
					CompName = DXCoil( DXCoilNum ).Name;
					FieldNum = 14;
					SizingString = DXCoilNumericFields( DXCoilNum ).PerfMode( Mode ).FieldNames( FieldNum ) + " [m3/s]";
					SizingMethod = AutoCalculateSizing;
					CompType = DXCoil( DXCoilNum ).DXCoilType;
					// Auto size low speed condenser air flow to 1/3 Total Capacity * 0.000114 m3/s/w (850 cfm/ton)
					DataConstantUsedForSizing = DXCoil( DXCoilNum ).RatedTotCap( Mode );
					DataFractionUsedForSizing = 0.000114 * 0.3333;
					TempSize = DXCoil( DXCoilNum ).EvapCondAirFlow2;
					RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
					DXCoil( DXCoilNum ).EvapCondAirFlow2 = TempSize;
					DataConstantUsedForSizing = 0.0;
					DataFractionUsedForSizing = 0.0;
				}

				// Sizing evaporative condenser pump electric nominal power
				if ( DXCoil( DXCoilNum ).CondenserType( 1 ) == EvapCooled && DXCoil( DXCoilNum ).EvapCondPumpElecNomPower( Mode ) != 0.0 && (DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_CoolingSingleSpeed || DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_CoolingTwoSpeed || DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_CoolingTwoStageWHumControl ) ) {

					if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_CoolingTwoStageWHumControl ) {
						CompName = DXCoil( DXCoilNum ).Name + ":" + DXCoil( DXCoilNum ).CoilPerformanceName( Mode );
						FieldNum = 12;
						SizingString = DXCoilNumericFields( DXCoilNum ).PerfMode( Mode ).FieldNames( FieldNum ) + " [W]";
					} else {
						CompName = DXCoil( DXCoilNum ).Name;
						FieldNum = 12;
						SizingString = DXCoilNumericFields( DXCoilNum ).PerfMode( Mode ).FieldNames( FieldNum ) + " [W]";
					}
					SizingMethod = AutoCalculateSizing;
					CompType = DXCoil( DXCoilNum ).DXCoilType;
					// Auto size high speed evap condenser pump power to Total Capacity * 0.004266 w/w (15 w/ton)
					DataConstantUsedForSizing = DXCoil( DXCoilNum ).RatedTotCap( Mode );
					DataFractionUsedForSizing = 0.004266;
					TempSize = DXCoil( DXCoilNum ).EvapCondPumpElecNomPower( Mode );
					RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
					DXCoil( DXCoilNum ).EvapCondPumpElecNomPower( Mode ) = TempSize;
					DataConstantUsedForSizing = 0.0;
					DataFractionUsedForSizing = 0.0;
				}

				// Sizing low speed evaporative condenser pump electric nominal power
				if ( DXCoil( DXCoilNum ).CondenserType( 1 ) == EvapCooled && DXCoil( DXCoilNum ).EvapCondPumpElecNomPower2 != 0.0 && DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_CoolingTwoSpeed ) {
					CompName = DXCoil( DXCoilNum ).Name;
					FieldNum = 15;
					SizingString = DXCoilNumericFields( DXCoilNum ).PerfMode( Mode ).FieldNames( FieldNum ) + " [W]";
					SizingMethod = AutoCalculateSizing;
					CompType = DXCoil( DXCoilNum ).DXCoilType;
					//Auto size low speed evap condenser pump power to 1/3 Total Capacity * 0.004266 w/w (15 w/ton)
					DataConstantUsedForSizing = DXCoil( DXCoilNum ).RatedTotCap( Mode );
					DataFractionUsedForSizing = 0.004266 * 0.3333;
					TempSize = DXCoil( DXCoilNum ).EvapCondPumpElecNomPower2;
					RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
					DXCoil( DXCoilNum ).EvapCondPumpElecNomPower2 = TempSize;
					DataConstantUsedForSizing = 0.0;
					DataFractionUsedForSizing = 0.0;
				}

//				// Sizing rated low speed air flow rate
				if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_CoolingTwoSpeed ) {
					CompName = DXCoil( DXCoilNum ).Name;
					FieldNum = 9;
					SizingString = DXCoilNumericFields( DXCoilNum ).PerfMode( Mode ).FieldNames( FieldNum ) + " [m3/s]";
					SizingMethod = AutoCalculateSizing;
					CompType = DXCoil( DXCoilNum ).DXCoilType;
					//Auto size low speed air flow rate to 1/3 high speed air flow rate
					DataConstantUsedForSizing = DXCoil( DXCoilNum ).RatedAirVolFlowRate( Mode );
					DataFractionUsedForSizing = 0.3333;
					TempSize = DXCoil( DXCoilNum ).RatedAirVolFlowRate2;
					RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
					DXCoil( DXCoilNum ).RatedAirVolFlowRate2 = TempSize;
					DataConstantUsedForSizing = 0.0;
					DataFractionUsedForSizing = 0.0;
				}

//				// Sizing rated low speed total cooling capacity
				if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_CoolingTwoSpeed ) {
					CompName = DXCoil( DXCoilNum ).Name;
					FieldNum = 6;
					SizingString = DXCoilNumericFields( DXCoilNum ).PerfMode( Mode ).FieldNames( FieldNum ) + " [W]";
					SizingMethod = AutoCalculateSizing;
					CompType = DXCoil( DXCoilNum ).DXCoilType;
					//Auto size low speed capacity to 1/3 high speed capacity
					DataConstantUsedForSizing = DXCoil( DXCoilNum ).RatedTotCap( Mode );
					DataFractionUsedForSizing = 0.3333;
					TempSize = DXCoil( DXCoilNum ).RatedTotCap2;
					RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
					DXCoil( DXCoilNum ).RatedTotCap2 = TempSize;
					DataConstantUsedForSizing = 0.0;
					DataFractionUsedForSizing = 0.0;
				}

				if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_CoolingTwoSpeed ) {
					if ( DXCoil( DXCoilNum ).EvapCondAirFlow2 > DXCoil( DXCoilNum ).EvapCondAirFlow( Mode ) ) {
						ShowSevereError( "SizeDXCoil: " + DXCoil( DXCoilNum ).DXCoilType + ' ' + DXCoil( DXCoilNum ).Name + ", Evaporative Condenser low speed air flow must be less than or equal to high speed air flow." );
						ShowContinueError( "Instead, " + RoundSigDigits( DXCoil( DXCoilNum ).EvapCondAirFlow2, 2 ) + " > " + RoundSigDigits( DXCoil( DXCoilNum ).EvapCondAirFlow( Mode ), 2 ) );
						ShowFatalError( "Preceding conditions cause termination." );
					}

					if ( DXCoil( DXCoilNum ).EvapCondPumpElecNomPower2 > DXCoil( DXCoilNum ).EvapCondPumpElecNomPower( Mode ) ) {
						ShowSevereError( "SizeDXCoil: " + DXCoil( DXCoilNum ).DXCoilType + ' ' + DXCoil( DXCoilNum ).Name + ", Evaporative Condenser low speed pump power must be less than or equal to high speed pump power." );
						ShowContinueError( "Instead, " + RoundSigDigits( DXCoil( DXCoilNum ).EvapCondPumpElecNomPower2, 2 ) + " > " + RoundSigDigits( DXCoil( DXCoilNum ).EvapCondPumpElecNomPower( Mode ), 2 ) );
						ShowFatalError( "Preceding conditions cause termination." );
					}

					if ( DXCoil( DXCoilNum ).RatedTotCap2 > DXCoil( DXCoilNum ).RatedTotCap( Mode ) ) {
						ShowSevereError( "SizeDXCoil: " + DXCoil( DXCoilNum ).DXCoilType + ' ' + DXCoil( DXCoilNum ).Name + ", Rated Total Cooling Capacity, Low Speed must be less than or equal to Rated Total Cooling Capacity, High Speed." );
						ShowContinueError( "Instead, " + RoundSigDigits( DXCoil( DXCoilNum ).RatedTotCap2, 2 ) + " > " + RoundSigDigits( DXCoil( DXCoilNum ).RatedTotCap( Mode ), 2 ) );
						ShowFatalError( "Preceding conditions cause termination." );
					}

					if ( DXCoil( DXCoilNum ).RatedAirVolFlowRate2 > DXCoil( DXCoilNum ).RatedAirVolFlowRate( Mode ) ) {
						ShowFatalError( "SizeDXCoil: " + DXCoil( DXCoilNum ).DXCoilType + ' ' + DXCoil( DXCoilNum ).Name + ", Rated Air Volume Flow Rate, low speed must be less than or equal to Rated Air Volume Flow Rate, high speed." );
						ShowContinueError( "Instead, " + RoundSigDigits( DXCoil( DXCoilNum ).RatedAirVolFlowRate2, 2 ) + " > " + RoundSigDigits( DXCoil( DXCoilNum ).RatedAirVolFlowRate( Mode ), 2 ) );
						ShowFatalError( "Preceding conditions cause termination." );
					}
				}

//				// Sizing rated low speed SHR2
				if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_CoolingTwoSpeed ) {
					CompName = DXCoil( DXCoilNum ).Name;
					FieldNum = 7;
					SizingString = DXCoilNumericFields( DXCoilNum ).PerfMode( Mode ).FieldNames( FieldNum );
					SizingMethod = AutoCalculateSizing;
					CompType = DXCoil( DXCoilNum ).DXCoilType;
					//Auto size low speed SHR to be the same as high speed SHR
					DataConstantUsedForSizing = DXCoil( DXCoilNum ).RatedSHR( Mode );
					DataFractionUsedForSizing = 1.0;
					TempSize = DXCoil( DXCoilNum ).RatedSHR2;
					RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
					DXCoil( DXCoilNum ).RatedSHR2 = TempSize;
					DataConstantUsedForSizing = 0.0;
					DataFractionUsedForSizing = 0.0;
				}

//				// Sizing resistive defrost heater capacity
				if ( DXCoil( DXCoilNum ).DXCoilType_Num != CoilVRF_Heating && DXCoil( DXCoilNum ).DXCoilType_Num != CoilVRF_FluidTCtrl_Heating && DXCoil( DXCoilNum ).DXCoilType_Num != CoilDX_MultiSpeedHeating) {
					//IF (DXCoil(DXCoilNum)%DXCoilType_Num == CoilDX_MultiSpeedHeating .OR. &
					//    DXCoil(DXCoilNum)%DXCoilType_Num == Coil_HeatingAirToAirVariableSpeed) THEN
					if ( DXCoil( DXCoilNum ).DefrostStrategy == Resistive ) {
						CompName = DXCoil( DXCoilNum ).Name;
						FieldNum = 11;
						SizingString = DXCoilNumericFields( DXCoilNum ).PerfMode( Mode ).FieldNames( FieldNum ) + " [W]";
						SizingMethod = AutoCalculateSizing;
						CompType = DXCoil( DXCoilNum ).DXCoilType;
						//Auto size low speed capacity to 1/3 high speed capacity
						DataConstantUsedForSizing = DXCoolCap;
						DataFractionUsedForSizing = 1.0;
						TempSize = DXCoil( DXCoilNum ).DefrostCapacity;
						RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
						DXCoil( DXCoilNum ).DefrostCapacity = TempSize;
						DataConstantUsedForSizing = 0.0;
						DataFractionUsedForSizing = 0.0;
					} else {
						DXCoil( DXCoilNum ).DefrostCapacity = 0.0;
					}
				}

			} // End capacity stages loop
		} // End dehumidification modes loop

		// Autosizing for multispeed cooling coil
		if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_MultiSpeedCooling ) {
			// flow rate auto size
			for ( Mode = DXCoil( DXCoilNum ).NumOfSpeeds; Mode >= 1; --Mode ) {
				// Sizing multispeed air volume flow rate
				IsAutoSize = false;
				if ( DXCoil( DXCoilNum ).MSRatedAirVolFlowRate( Mode ) == AutoSize ) {
					IsAutoSize = true;
				}

				if ( Mode == DXCoil( DXCoilNum ).NumOfSpeeds ) {
					CompName = DXCoil( DXCoilNum ).Name;
					FieldNum = 9 + ( Mode - 1) * 13;
					SizingString = DXCoilNumericFields( DXCoilNum ).PerfMode( 1 ).FieldNames( FieldNum ) + " [m3/s]";
					SizingMethod = CoolingAirflowSizing;
					CompType = DXCoil( DXCoilNum ).DXCoilType;
					TempSize = DXCoil( DXCoilNum ).MSRatedAirVolFlowRate( Mode );
					DataIsDXCoil = true;
					DataEMSOverrideON = DXCoil ( DXCoilNum ).RatedAirVolFlowRateEMSOverrideON ( Mode );
					DataEMSOverride = DXCoil( DXCoilNum ).RatedAirVolFlowRateEMSOverrideValue( Mode );
					RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
					DXCoil( DXCoilNum ).MSRatedAirVolFlowRate( Mode ) = TempSize;
					DataIsDXCoil = false;
					DataEMSOverrideON = false;
					DataEMSOverride = 0.0;
				} else {
					MSRatedAirVolFlowRateDes = DXCoil( DXCoilNum ).MSRatedAirVolFlowRate( DXCoil( DXCoilNum ).NumOfSpeeds ) * Mode / DXCoil( DXCoilNum ).NumOfSpeeds;
					CompName = DXCoil( DXCoilNum ).Name;
					FieldNum = 9 + ( Mode - 1) * 13;
					SizingString = DXCoilNumericFields( DXCoilNum ).PerfMode( 1 ).FieldNames( FieldNum ) + " [m3/s]";
					SizingMethod = AutoCalculateSizing;
					CompType = DXCoil( DXCoilNum ).DXCoilType;
					//Auto size low speed capacity to 1/3 high speed capacity
					DataConstantUsedForSizing = DXCoil( DXCoilNum ).MSRatedAirVolFlowRate( DXCoil( DXCoilNum ).NumOfSpeeds );
					DataFractionUsedForSizing = (float)Mode / DXCoil( DXCoilNum ).NumOfSpeeds;
					TempSize = DXCoil( DXCoilNum ).MSRatedAirVolFlowRate( Mode );
					RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, bPRINT, RoutineName );
					DXCoil( DXCoilNum ).MSRatedAirVolFlowRate( Mode ) = TempSize;
					DataConstantUsedForSizing = 0.0;
					DataFractionUsedForSizing = 0.0;
				}
			}

			// Ensure flow rate at lower speed must be lower or equal to the flow rate at higher speed. Otherwise, a severe error is isssued.
			for ( Mode = 1; Mode <= DXCoil( DXCoilNum ).NumOfSpeeds - 1; ++Mode ) {
				if ( DXCoil( DXCoilNum ).MSRatedAirVolFlowRate( Mode ) > DXCoil( DXCoilNum ).MSRatedAirVolFlowRate( Mode + 1 ) ) {
					ShowWarningError( "SizeDXCoil: " + DXCoil( DXCoilNum ).DXCoilType + ' ' + DXCoil( DXCoilNum ).Name + ", Speed " + TrimSigDigits( Mode ) + " Rated Air Flow Rate must be less than or equal to Speed " + TrimSigDigits( Mode + 1 ) + " Rated Air Flow Rate." );
					ShowContinueError( "Instead, " + RoundSigDigits( DXCoil( DXCoilNum ).MSRatedAirVolFlowRate( Mode ), 2 ) + " > " + RoundSigDigits( DXCoil( DXCoilNum ).MSRatedAirVolFlowRate( Mode + 1 ), 2 ) );
					ShowFatalError( "Preceding conditions cause termination." );
				}
			}

			// Sizing multispeed rated total capacity
			for ( Mode = DXCoil( DXCoilNum ).NumOfSpeeds; Mode >= 1; --Mode ) {
				IsAutoSize = false;
				if ( DXCoil( DXCoilNum ).MSRatedTotCap( Mode ) == AutoSize ) {
					IsAutoSize = true;
				}
				if ( Mode == DXCoil( DXCoilNum ).NumOfSpeeds ) {
					if ( CurSysNum > 0 ) {
						if ( SizingDesRunThisAirSys ) HardSizeNoDesRun = false;
						if ( ! IsAutoSize && ! SizingDesRunThisAirSys ) {
							HardSizeNoDesRun = true;
							if ( DXCoil( DXCoilNum ).MSRatedTotCap( Mode ) > 0.0 ) {
								ReportSizingOutput( DXCoil( DXCoilNum ).DXCoilType, DXCoil( DXCoilNum ).Name, "Speed " + TrimSigDigits( Mode ) + " User-Specified Total Cooling Capacity [W]", DXCoil( DXCoilNum ).MSRatedTotCap( Mode ) );
							}
						} else { // autosize or hard-sized with system sizing data
							CheckSysSizing( DXCoil( DXCoilNum ).DXCoilType, DXCoil( DXCoilNum ).Name );
							VolFlowRate = DXCoil( DXCoilNum ).MSRatedAirVolFlowRate( Mode );
							if ( VolFlowRate >= SmallAirVolFlow ) {
								if ( CurOASysNum > 0 ) { // coil is in the OA stream
									MixTemp = FinalSysSizing( CurSysNum ).OutTempAtCoolPeak;
									MixHumRat = FinalSysSizing( CurSysNum ).OutHumRatAtCoolPeak;
									SupTemp = FinalSysSizing( CurSysNum ).PrecoolTemp;
									SupHumRat = FinalSysSizing( CurSysNum ).PrecoolHumRat;
								} else { // coil is on the main air loop
									SupTemp = FinalSysSizing( CurSysNum ).CoolSupTemp;
									SupHumRat = FinalSysSizing( CurSysNum ).CoolSupHumRat;
									if ( PrimaryAirSystem( CurSysNum ).NumOACoolCoils == 0 ) { // there is no precooling of the OA stream
										MixTemp = FinalSysSizing( CurSysNum ).MixTempAtCoolPeak;
										MixHumRat = FinalSysSizing( CurSysNum ).MixHumRatAtCoolPeak;
									} else { // there is precooling of OA stream
										if ( VolFlowRate > 0.0 ) {
											OutAirFrac = FinalSysSizing( CurSysNum ).DesOutAirVolFlow / VolFlowRate;
										} else {
											OutAirFrac = 1.0;
										}
										OutAirFrac = min( 1.0, max( 0.0, OutAirFrac ) );
										MixTemp = OutAirFrac * FinalSysSizing( CurSysNum ).PrecoolTemp + ( 1.0 - OutAirFrac ) * FinalSysSizing( CurSysNum ).RetTempAtCoolPeak;
										MixHumRat = OutAirFrac * FinalSysSizing( CurSysNum ).PrecoolHumRat + ( 1.0 - OutAirFrac ) * FinalSysSizing( CurSysNum ).RetHumRatAtCoolPeak;
									}
								}
								OutTemp = FinalSysSizing( CurSysNum ).OutTempAtCoolPeak;
								rhoair = PsyRhoAirFnPbTdbW( StdBaroPress, MixTemp, MixHumRat, RoutineName );
								MixEnth = PsyHFnTdbW( MixTemp, MixHumRat );
								MixWetBulb = PsyTwbFnTdbWPb( MixTemp, MixHumRat, StdBaroPress, RoutineName );
								SupEnth = PsyHFnTdbW( SupTemp, SupHumRat );
								TotCapTempModFac = CurveValue( DXCoil( DXCoilNum ).MSCCapFTemp( Mode ), MixWetBulb, OutTemp );
								CoolCapAtPeak = max( 0.0, ( rhoair * VolFlowRate * ( MixEnth - SupEnth ) ) );
								if ( TotCapTempModFac > 0.0 ) {
									MSRatedTotCapDes = CoolCapAtPeak / TotCapTempModFac;
								} else {
									MSRatedTotCapDes = CoolCapAtPeak;
								}
								if ( UnitarySysEqSizing( CurSysNum ).CoolingCapacity ) { // override capacity if parent speicifies size
									MSRatedTotCapDes = UnitarySysEqSizing ( CurSysNum ).DesCoolingLoad;
								}
								if ( MSRatedTotCapDes > 0.0 ) {
									RatedVolFlowPerRatedTotCap = DXCoil( DXCoilNum ).MSRatedAirVolFlowRate( Mode ) / MSRatedTotCapDes;
								} else {
									RatedVolFlowPerRatedTotCap = 0.0;
								}
								// check capacity to make sure design volume flow per total capacity is within range
								if ( RatedVolFlowPerRatedTotCap < MinRatedVolFlowPerRatedTotCap( DXCT ) ) {
									if ( DisplayExtraWarnings ) {
										ShowWarningError( "SizeDXCoil: " + DXCoil( DXCoilNum ).DXCoilType + ' ' + DXCoil( DXCoilNum ).Name );
										ShowContinueError( "...Rated Total Cooling Capacity will be limited by the minimum rated volume flow per rated total capacity ratio." );
										ShowContinueError( "...DX coil speed = " + TrimSigDigits( Mode ) );
										ShowContinueError( "...DX coil volume flow rate (m3/s) = " + TrimSigDigits( DXCoil( DXCoilNum ).RatedAirVolFlowRate( Mode ), 6 ) );
										ShowContinueError( "...Requested capacity (W) = " + TrimSigDigits( MSRatedTotCapDes, 3 ) );
										ShowContinueError( "...Requested flow/capacity ratio (m3/s/W) = " + TrimSigDigits( RatedVolFlowPerRatedTotCap, 3 ) );
										ShowContinueError( "...Minimum flow/capacity ratio (m3/s/W) = " + TrimSigDigits( MinRatedVolFlowPerRatedTotCap( DXCT ), 3 ) );
									}
									MSRatedTotCapDes = DXCoil( DXCoilNum ).MSRatedAirVolFlowRate( Mode ) / MinRatedVolFlowPerRatedTotCap( DXCT );

									if ( DisplayExtraWarnings ) {
										ShowContinueError( "...Adjusted capacity (W) = " + TrimSigDigits( MSRatedTotCapDes, 3 ) );
									}
								} else if ( RatedVolFlowPerRatedTotCap > MaxRatedVolFlowPerRatedTotCap( DXCT ) ) {
									if ( DisplayExtraWarnings ) {
										ShowWarningError( "SizeDXCoil: " + DXCoil( DXCoilNum ).DXCoilType + ' ' + DXCoil( DXCoilNum ).Name );
										ShowContinueError( "...Rated Total Cooling Capacity will be limited by the maximum rated volume flow per rated total capacity ratio." );
										ShowContinueError( "...DX coil speed = " + TrimSigDigits( Mode ) );
										ShowContinueError( "...DX coil volume flow rate (m3/s) = " + TrimSigDigits( DXCoil( DXCoilNum ).RatedAirVolFlowRate( Mode ), 6 ) );
										ShowContinueError( "...Requested capacity (W) = " + TrimSigDigits( MSRatedTotCapDes, 3 ) );
										ShowContinueError( "...Requested flow/capacity ratio (m3/s/W) = " + TrimSigDigits( RatedVolFlowPerRatedTotCap, 3 ) );
										ShowContinueError( "...Maximum flow/capacity ratio (m3/s/W) = " + TrimSigDigits( MaxRatedVolFlowPerRatedTotCap( DXCT ), 3 ) );
									}
									MSRatedTotCapDes = DXCoil( DXCoilNum ).MSRatedAirVolFlowRate( Mode ) / MaxRatedVolFlowPerRatedTotCap( DXCT );
									if ( DisplayExtraWarnings ) {
										ShowContinueError( "...Adjusted capacity (W) = " + TrimSigDigits( MSRatedTotCapDes, 3 ) );
									}
								}
							} else {
								MSRatedTotCapDes = 0.0;
							}
						}
					} else if ( CurZoneEqNum > 0 ) {
						if ( SizingDesRunThisZone ) HardSizeNoDesRun = false;
						if ( ! IsAutoSize && ! SizingDesRunThisZone ) {
							HardSizeNoDesRun = true;
							if ( DXCoil( DXCoilNum ).MSRatedTotCap( Mode ) > 0.0 ) {
								ReportSizingOutput( DXCoil( DXCoilNum ).DXCoilType, DXCoil( DXCoilNum ).Name, "Speed " + TrimSigDigits( Mode ) + " User-Specified Total Cooling Capacity [W]", DXCoil( DXCoilNum ).MSRatedTotCap( Mode ) );
							}
						} else { // autosize or hard-sized with system sizing data
							CheckZoneSizing( DXCoil( DXCoilNum ).DXCoilType, DXCoil( DXCoilNum ).Name );
							VolFlowRate = DXCoil( DXCoilNum ).MSRatedAirVolFlowRate( Mode );
							if ( VolFlowRate >= SmallAirVolFlow ) {
								MixTemp = FinalZoneSizing( CurZoneEqNum ).DesCoolCoilInTemp;
								MixHumRat = FinalZoneSizing( CurZoneEqNum ).DesCoolCoilInHumRat;
								SupTemp = FinalZoneSizing( CurZoneEqNum ).CoolDesTemp;
								SupHumRat = FinalZoneSizing( CurZoneEqNum ).CoolDesHumRat;
								TimeStepNumAtMax = FinalZoneSizing( CurZoneEqNum ).TimeStepNumAtCoolMax;
								DDNum = FinalZoneSizing( CurZoneEqNum ).CoolDDNum;
								if ( DDNum > 0 && TimeStepNumAtMax > 0 ) {
									OutTemp = DesDayWeath( DDNum ).Temp( TimeStepNumAtMax );
								} else {
									OutTemp = 0.0;
								}
								rhoair = PsyRhoAirFnPbTdbW( StdBaroPress, MixTemp, MixHumRat, RoutineName );
								MixEnth = PsyHFnTdbW( MixTemp, MixHumRat );
								MixWetBulb = PsyTwbFnTdbWPb( MixTemp, MixHumRat, StdBaroPress, RoutineName );
								SupEnth = PsyHFnTdbW( SupTemp, SupHumRat );
								TotCapTempModFac = CurveValue( DXCoil( DXCoilNum ).MSCCapFTemp( Mode ), MixWetBulb, OutTemp );
								CoolCapAtPeak = max( 0.0, ( rhoair * VolFlowRate * ( MixEnth - SupEnth ) ) );
								if ( TotCapTempModFac > 0.0 ) {
									MSRatedTotCapDes = CoolCapAtPeak / TotCapTempModFac;
								} else {
									MSRatedTotCapDes = CoolCapAtPeak;
								}
								if ( ZoneEqSizing( CurZoneEqNum ).CoolingCapacity ) { // override capacity if parent speicifies size
									MSRatedTotCapDes = ZoneEqSizing ( CurZoneEqNum ).DesCoolingLoad;
								}
								if ( MSRatedTotCapDes > 0.0 ) {
									RatedVolFlowPerRatedTotCap = DXCoil( DXCoilNum ).MSRatedAirVolFlowRate( Mode ) / MSRatedTotCapDes;
								} else {
									RatedVolFlowPerRatedTotCap = 0.0;
								}
								// check capacity to make sure design volume flow per total capacity is within range
								if ( RatedVolFlowPerRatedTotCap < MinRatedVolFlowPerRatedTotCap( DXCT ) ) {
									if ( DisplayExtraWarnings ) {
										ShowWarningError( "SizeDXCoil: " + DXCoil( DXCoilNum ).DXCoilType + ' ' + DXCoil( DXCoilNum ).Name );
										ShowContinueError( "...Rated Total Cooling Capacity will be limited by the minimum rated volume flow per rated total capacity ratio." );
										ShowContinueError( "...DX coil speed = " + TrimSigDigits( Mode ) );
										ShowContinueError( "...DX coil volume flow rate (m3/s) = " + TrimSigDigits( DXCoil( DXCoilNum ).RatedAirVolFlowRate( Mode ), 6 ) );
										ShowContinueError( "...Requested capacity (W) = " + TrimSigDigits( MSRatedTotCapDes, 3 ) );
										ShowContinueError( "...Requested flow/capacity ratio (m3/s/W) = " + TrimSigDigits( RatedVolFlowPerRatedTotCap, 3 ) );
										ShowContinueError( "...Minimum flow/capacity ratio (m3/s/W) = " + TrimSigDigits( MinRatedVolFlowPerRatedTotCap( DXCT ), 3 ) );
									}
									MSRatedTotCapDes = DXCoil( DXCoilNum ).MSRatedAirVolFlowRate( Mode ) / MinRatedVolFlowPerRatedTotCap( DXCT );
									if ( DisplayExtraWarnings ) {
										ShowContinueError( "...Adjusted capacity (W) = " + TrimSigDigits( MSRatedTotCapDes, 3 ) );
									}
								} else if ( RatedVolFlowPerRatedTotCap > MaxRatedVolFlowPerRatedTotCap( DXCT ) ) {
									if ( DisplayExtraWarnings ) {
										ShowWarningError( "SizeDXCoil: " + DXCoil( DXCoilNum ).DXCoilType + ' ' + DXCoil( DXCoilNum ).Name );
										ShowContinueError( "...Rated Total Cooling Capacity will be limited by the maximum rated volume flow per rated total capacity ratio." );
										ShowContinueError( "...DX coil speed = " + TrimSigDigits( Mode ) );
										ShowContinueError( "...DX coil volume flow rate (m3/s) = " + TrimSigDigits( DXCoil( DXCoilNum ).RatedAirVolFlowRate( Mode ), 6 ) );
										ShowContinueError( "...Requested capacity (W) = " + TrimSigDigits( MSRatedTotCapDes, 3 ) );
										ShowContinueError( "...Requested flow/capacity ratio (m3/s/W) = " + TrimSigDigits( RatedVolFlowPerRatedTotCap, 3 ) );
										ShowContinueError( "...Maximum flow/capacity ratio (m3/s/W) = " + TrimSigDigits( MaxRatedVolFlowPerRatedTotCap( DXCT ), 3 ) );
									}
									MSRatedTotCapDes = DXCoil( DXCoilNum ).MSRatedAirVolFlowRate( Mode ) / MaxRatedVolFlowPerRatedTotCap( DXCT );
									if ( DisplayExtraWarnings ) {
										ShowContinueError( "...Adjusted capacity (W) = " + TrimSigDigits( MSRatedTotCapDes, 3 ) );
									}
								}
							} else {
								MSRatedTotCapDes = 0.0;
							}
						}
					}
				} else {
					MSRatedTotCapDes = DXCoil( DXCoilNum ).MSRatedTotCap( DXCoil( DXCoilNum ).NumOfSpeeds ) * Mode / DXCoil( DXCoilNum ).NumOfSpeeds;
				}
				if ( ! HardSizeNoDesRun ) {
					if ( IsAutoSize ) {
						DXCoil( DXCoilNum ).MSRatedTotCap( Mode ) = MSRatedTotCapDes;
						ReportSizingOutput( DXCoil( DXCoilNum ).DXCoilType, DXCoil( DXCoilNum ).Name, "Speed " + TrimSigDigits( Mode ) + " Design Size Rated Total Cooling Capacity [W]", MSRatedTotCapDes );
					} else {
						if ( DXCoil( DXCoilNum ).MSRatedTotCap( Mode ) > 0.0 && MSRatedTotCapDes > 0.0 && ! HardSizeNoDesRun ) {
							MSRatedTotCapUser = DXCoil( DXCoilNum ).MSRatedTotCap( Mode );
							ReportSizingOutput( DXCoil( DXCoilNum ).DXCoilType, DXCoil( DXCoilNum ).Name, "Speed " + TrimSigDigits( Mode ) + " Design Size Rated Total Cooling Capacity [W]", MSRatedTotCapDes, "Speed " + TrimSigDigits( Mode ) + " User-Specified Rated Total Cooling Capacity [W]", MSRatedTotCapUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( MSRatedTotCapDes - MSRatedTotCapUser ) / MSRatedTotCapUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeDxCoil: Potential issue with equipment sizing for " + DXCoil( DXCoilNum ).DXCoilType + ' ' + DXCoil( DXCoilNum ).Name );
									ShowContinueError( "User-Specified Rated Total Cooling Capacity of " + RoundSigDigits( MSRatedTotCapUser, 2 ) + " [W]" );
									ShowContinueError( "differs from Design Size Rated Totla Cooling Capacity of " + RoundSigDigits( MSRatedTotCapDes, 2 ) + " [W]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
					}
				}
			}

			// Ensure capacity at lower speed must be lower or equal to the capacity at higher speed.
			for ( Mode = 1; Mode <= DXCoil( DXCoilNum ).NumOfSpeeds - 1; ++Mode ) {
				if ( DXCoil( DXCoilNum ).MSRatedTotCap( Mode ) > DXCoil( DXCoilNum ).MSRatedTotCap( Mode + 1 ) ) {
					ShowWarningError( "SizeDXCoil: " + DXCoil( DXCoilNum ).DXCoilType + ' ' + DXCoil( DXCoilNum ).Name + ", Speed " + TrimSigDigits( Mode ) + " Rated Total Cooling Capacity must be less than or equal to Speed " + TrimSigDigits( Mode + 1 ) + " Rated Total Cooling Capacity." );
					ShowContinueError( "Instead, " + RoundSigDigits( DXCoil( DXCoilNum ).MSRatedTotCap( Mode ), 2 ) + " > " + RoundSigDigits( DXCoil( DXCoilNum ).MSRatedTotCap( Mode + 1 ), 2 ) );
					ShowFatalError( "Preceding conditions cause termination." );
				}
			}

			// Rated SHR
			for ( Mode = DXCoil( DXCoilNum ).NumOfSpeeds; Mode >= 1; --Mode ) {
				IsAutoSize = false;
				if ( DXCoil( DXCoilNum ).MSRatedSHR( Mode ) == AutoSize ) {
					IsAutoSize = true;
				}
				if ( Mode == DXCoil( DXCoilNum ).NumOfSpeeds ) {
					if ( CurSysNum > 0 ) {
						if ( SizingDesRunThisAirSys ) HardSizeNoDesRun = false;
						if ( ! IsAutoSize && ! SizingDesRunThisAirSys ) {
							HardSizeNoDesRun = true;
							if ( DXCoil( DXCoilNum ).MSRatedSHR( Mode ) > 0.0 ) {
								ReportSizingOutput( DXCoil( DXCoilNum ).DXCoilType, DXCoil( DXCoilNum ).Name, "Speed " + TrimSigDigits( Mode ) + " User-Specified Rated Sensible Heat Ratio", DXCoil( DXCoilNum ).MSRatedSHR( Mode ) );
							}
						} else { // autosize or hard-sized with system sizing data
							CheckSysSizing( DXCoil( DXCoilNum ).DXCoilType, DXCoil( DXCoilNum ).Name );
						}
					} else if ( CurZoneEqNum > 0 ) {
						if ( SizingDesRunThisZone ) HardSizeNoDesRun = false;
						if ( ! IsAutoSize && ! SizingDesRunThisZone ) {
							HardSizeNoDesRun = true;
							if ( DXCoil( DXCoilNum ).MSRatedSHR( Mode ) > 0.0 ) {
								ReportSizingOutput( DXCoil( DXCoilNum ).DXCoilType, DXCoil( DXCoilNum ).Name, "Speed " + TrimSigDigits( Mode ) + " User-Specified Rated Sensible Heat Ratio", DXCoil( DXCoilNum ).MSRatedSHR( Mode ) );
							}
						} else { // autosize or hard-sized with system sizing data
							CheckZoneSizing( DXCoil( DXCoilNum ).DXCoilType, DXCoil( DXCoilNum ).Name );
						}
					}
					if ( DXCoil( DXCoilNum ).MSRatedAirVolFlowRate( Mode ) >= SmallAirVolFlow && DXCoil( DXCoilNum ).MSRatedTotCap( Mode ) > 0.0 ) {
						// For autosizing the rated SHR, we set a minimum SHR of 0.676 and a maximum of 0.798. The min SHR occurs occurs at the
						// minimum flow / capacity ratio = MinRatedVolFlowPerRatedTotCap = 0.00004027 [m3/s / W] = 300 [cfm/ton].
						// The max SHR occurs at maximum flow / capacity ratio = MaxRatedVolFlowPerRatedTotCap = 0.00006041 [m3/s / W] = 450 [cfm/ton].
						// For flow / capacity ratios between the min and max we linearly interpolate between min and max SHR. Thus rated SHR is a
						// linear function of the rated flow / capacity ratio. This linear function (see below) is the result of a regression
						// of flow/capacity ratio vs SHR for several actual coils.
						RatedVolFlowPerRatedTotCap = DXCoil( DXCoilNum ).MSRatedAirVolFlowRate( Mode ) / DXCoil( DXCoilNum ).MSRatedTotCap( Mode );
						if ( RatedVolFlowPerRatedTotCap > MaxRatedVolFlowPerRatedTotCap( DXCT ) ) {
							MSRatedSHRDes = 0.431 + 6086.0 * MaxRatedVolFlowPerRatedTotCap( DXCT );
						} else if ( RatedVolFlowPerRatedTotCap < MinRatedVolFlowPerRatedTotCap( DXCT ) ) {
							MSRatedSHRDes = 0.431 + 6086.0 * MinRatedVolFlowPerRatedTotCap( DXCT );
						} else {
							MSRatedSHRDes = 0.431 + 6086.0 * RatedVolFlowPerRatedTotCap;
						}
					} else {
						MSRatedSHRDes = 1.0;
					}
				} else {
					MSRatedSHRDes = DXCoil( DXCoilNum ).MSRatedSHR( Mode + 1 );
				}

				if ( ! HardSizeNoDesRun ) {
					if ( IsAutoSize ) {
						DXCoil( DXCoilNum ).MSRatedSHR( Mode ) = MSRatedSHRDes;
						ReportSizingOutput( DXCoil( DXCoilNum ).DXCoilType, DXCoil( DXCoilNum ).Name, "Speed " + TrimSigDigits( Mode ) + " Design Size Rated Sensible Heat Ratio", MSRatedSHRDes );
					} else {
						if ( DXCoil( DXCoilNum ).MSRatedSHR( Mode ) > 0.0 && MSRatedSHRDes > 0.0 && ! HardSizeNoDesRun ) {
							MSRatedSHRUser = DXCoil( DXCoilNum ).MSRatedSHR( Mode );
							ReportSizingOutput( DXCoil( DXCoilNum ).DXCoilType, DXCoil( DXCoilNum ).Name, "Speed " + TrimSigDigits( Mode ) + " Design Size Rated Sensible Heat Ratio", MSRatedSHRDes, "Speed " + TrimSigDigits( Mode ) + " User-Specified Rated Sensible Heat Ratio", MSRatedSHRUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( MSRatedSHRDes - MSRatedSHRUser ) / MSRatedSHRUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeDxCoil: Potential issue with equipment sizing for " + DXCoil( DXCoilNum ).DXCoilType + ' ' + DXCoil( DXCoilNum ).Name );
									ShowContinueError( "User-Specified Rated Sensible Heat Ratio of " + RoundSigDigits( MSRatedSHRUser, 3 ) );
									ShowContinueError( "differs from Design Size Rated Sensible Heat Ratio of " + RoundSigDigits( MSRatedSHRDes, 3 ) );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
					}
				}
			}

			// Rated Evapovative condenser airflow rates
			for ( Mode = 1; Mode <= DXCoil( DXCoilNum ).NumOfSpeeds; ++Mode ) {
				IsAutoSize = false;
				if ( DXCoil( DXCoilNum ).MSEvapCondAirFlow( Mode ) == AutoSize ) {
					IsAutoSize = true;
				}
				// Auto size condenser air flow to Total Capacity * 0.000114 m3/s/w (850 cfm/ton)
				MSEvapCondAirFlowDes = DXCoil( DXCoilNum ).MSRatedTotCap( Mode ) * 0.000114;

				if ( IsAutoSize ) {
					DXCoil( DXCoilNum ).MSEvapCondAirFlow( Mode ) = MSEvapCondAirFlowDes;
					ReportSizingOutput( DXCoil( DXCoilNum ).DXCoilType, DXCoil( DXCoilNum ).Name, "Speed " + TrimSigDigits( Mode ) + " Design Size Evaporative Condenser Air Flow Rate [m3/s]", MSEvapCondAirFlowDes );
				} else {
					if ( DXCoil( DXCoilNum ).MSEvapCondAirFlow( Mode ) > 0.0 && MSEvapCondAirFlowDes > 0.0 && ! HardSizeNoDesRun ) {
						MSEvapCondAirFlowUser = DXCoil( DXCoilNum ).MSEvapCondAirFlow( Mode );
						ReportSizingOutput( DXCoil( DXCoilNum ).DXCoilType, DXCoil( DXCoilNum ).Name, "Speed " + TrimSigDigits( Mode ) + " Design Size Evaporative Condenser Air Flow Rate [m3/s]", MSEvapCondAirFlowDes, "Speed " + TrimSigDigits( Mode ) + " User-Specified Evaporative Condenser Air Flow Rate [m3/s]", MSEvapCondAirFlowUser );
						if ( DisplayExtraWarnings ) {
							if ( ( std::abs( MSEvapCondAirFlowDes - MSEvapCondAirFlowUser ) / MSEvapCondAirFlowUser ) > AutoVsHardSizingThreshold ) {
								ShowMessage( "SizeDxCoil: Potential issue with equipment sizing for " + DXCoil( DXCoilNum ).DXCoilType + ' ' + DXCoil( DXCoilNum ).Name );
								ShowContinueError( "User-Specified Evaporative Condenser Air Flow Rate of " + RoundSigDigits( MSEvapCondAirFlowUser, 5 ) + " [m3/s]" );
								ShowContinueError( "differs from Design Size Evaporative Condenser Air Flow Rate of " + RoundSigDigits( MSEvapCondAirFlowDes, 5 ) + " [m3/s]" );
								ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
								ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
							}
						}
					}
				}
			}

			// Ensure evaporative condesner airflow rate at lower speed must be lower or equal to one at higher speed.
			for ( Mode = 1; Mode <= DXCoil( DXCoilNum ).NumOfSpeeds - 1; ++Mode ) {
				if ( DXCoil( DXCoilNum ).MSEvapCondAirFlow( Mode ) > DXCoil( DXCoilNum ).MSEvapCondAirFlow( Mode + 1 ) ) {
					ShowWarningError( "SizeDXCoil: " + DXCoil( DXCoilNum ).DXCoilType + ' ' + DXCoil( DXCoilNum ).Name + ", Speed " + TrimSigDigits( Mode ) + " Evaporative Condenser Air Flow Rate must be less than or equal to Speed " + TrimSigDigits( Mode + 1 ) + " Evaporative Condenser Air Flow Rate." );
					ShowContinueError( "Instead, " + RoundSigDigits( DXCoil( DXCoilNum ).MSEvapCondAirFlow( Mode ), 2 ) + " > " + RoundSigDigits( DXCoil( DXCoilNum ).MSEvapCondAirFlow( Mode + 1 ), 2 ) );
					ShowFatalError( "Preceding conditions cause termination." );
				}
			}

			// Sizing multispeed rated evapovative condenser pump power
			for ( Mode = 1; Mode <= DXCoil( DXCoilNum ).NumOfSpeeds; ++Mode ) {
				IsAutoSize = false;
				if ( DXCoil( DXCoilNum ).MSEvapCondPumpElecNomPower( Mode ) == AutoSize ) {
					IsAutoSize = true;
				}
				// Auto size low speed evap condenser pump power to 1/3 Total Capacity * 0.004266 w/w (15 w/ton)
				MSEvapCondPumpElecNomPowerDes = DXCoil( DXCoilNum ).MSRatedTotCap( Mode ) * 0.004266;
				// Design Size data is always available
				if ( IsAutoSize ) {
					DXCoil( DXCoilNum ).MSEvapCondPumpElecNomPower( Mode ) = MSEvapCondPumpElecNomPowerDes;
					ReportSizingOutput( DXCoil( DXCoilNum ).DXCoilType, DXCoil( DXCoilNum ).Name, "Speed " + TrimSigDigits( Mode ) + " Design Size Rated Evaporative Condenser Pump Power Consumption [W]", MSEvapCondPumpElecNomPowerDes );
				} else {
					if ( DXCoil( DXCoilNum ).MSEvapCondPumpElecNomPower( Mode ) > 0.0 && MSEvapCondPumpElecNomPowerDes > 0.0 && ! HardSizeNoDesRun ) {
						MSEvapCondPumpElecNomPowerUser = DXCoil( DXCoilNum ).MSEvapCondPumpElecNomPower( Mode );
						ReportSizingOutput( DXCoil( DXCoilNum ).DXCoilType, DXCoil( DXCoilNum ).Name, "Speed " + TrimSigDigits( Mode ) + " Design Size Rated Evaporative Condenser Pump Power Consumption [W]", MSEvapCondPumpElecNomPowerDes, "Speed " + TrimSigDigits( Mode ) + " User-Specified Rated Evaporative Condenser Pump Power Consumption [W]", MSEvapCondPumpElecNomPowerUser );
						if ( DisplayExtraWarnings ) {
							if ( ( std::abs( MSEvapCondPumpElecNomPowerDes - MSEvapCondPumpElecNomPowerUser ) / MSEvapCondPumpElecNomPowerUser ) > AutoVsHardSizingThreshold ) {
								ShowMessage( "SizeDxCoil: Potential issue with equipment sizing for " + DXCoil( DXCoilNum ).DXCoilType + ' ' + DXCoil( DXCoilNum ).Name );
								ShowContinueError( "User-Specified Evaporative Condenser Pump Rated Power Consumption of " + RoundSigDigits( MSEvapCondPumpElecNomPowerUser, 2 ) + " [W]" );
								ShowContinueError( "differs from Design Size Evaporative Condenser Pump Rated Power Consumption of " + RoundSigDigits( MSEvapCondPumpElecNomPowerDes, 2 ) + " [W]" );
								ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
								ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
							}
						}
					}
				}
			}

			// Ensure evaporative condesner pump power at lower speed must be lower or equal to one at higher speed.
			for ( Mode = 1; Mode <= DXCoil( DXCoilNum ).NumOfSpeeds - 1; ++Mode ) {
				if ( DXCoil( DXCoilNum ).MSEvapCondPumpElecNomPower( Mode ) > DXCoil( DXCoilNum ).MSEvapCondPumpElecNomPower( Mode + 1 ) ) {
					ShowWarningError( "SizeDXCoil: " + DXCoil( DXCoilNum ).DXCoilType + ' ' + DXCoil( DXCoilNum ).Name + ", Speed " + TrimSigDigits( Mode ) + " Rated Evaporative Condenser Pump Power Consumption must be less than or equal to Speed " + TrimSigDigits( Mode + 1 ) + " Rated Evaporative Condenser Pump Power Consumption." );
					ShowContinueError( "Instead, " + RoundSigDigits( DXCoil( DXCoilNum ).MSEvapCondPumpElecNomPower( Mode ), 2 ) + " > " + RoundSigDigits( DXCoil( DXCoilNum ).MSEvapCondPumpElecNomPower( Mode + 1 ), 2 ) );
					ShowFatalError( "Preceding conditions cause termination." );
				}
			}
		}

		// Autosizing for multispeed heating coil
		if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_MultiSpeedHeating ) {
			// flow rate auto size
			for ( Mode = DXCoil( DXCoilNum ).NumOfSpeeds; Mode >= 1; --Mode ) {
				IsAutoSize = false;
				if ( DXCoil( DXCoilNum ).MSRatedAirVolFlowRate( Mode ) == AutoSize ) {
					IsAutoSize = true;
				}
				// Sizing rated air flow rate
				if ( Mode == DXCoil( DXCoilNum ).NumOfSpeeds ) {
					if ( CurSysNum > 0 ) {
						if ( SizingDesRunThisAirSys ) HardSizeNoDesRun = false;
						if ( ! IsAutoSize && ! SizingDesRunThisAirSys ) {
							HardSizeNoDesRun = true;
							if ( DXCoil( DXCoilNum ).MSRatedAirVolFlowRate( Mode ) > 0.0 ) {
								ReportSizingOutput( DXCoil( DXCoilNum ).DXCoilType, DXCoil( DXCoilNum ).Name, "Speed " + TrimSigDigits( Mode ) + " User-Specified Rated Air Flow Rate [m3/s]", DXCoil( DXCoilNum ).MSRatedAirVolFlowRate( Mode ) );
							}
						} else {
							CheckSysSizing( DXCoil( DXCoilNum ).DXCoilType, DXCoil( DXCoilNum ).Name );
							if ( CurOASysNum > 0 ) {
								MSRatedAirVolFlowRateDes = FinalSysSizing( CurSysNum ).DesOutAirVolFlow;
							} else {
								MSRatedAirVolFlowRateDes = FinalSysSizing( CurSysNum ).DesMainVolFlow;
							}
						}
					} else if ( CurZoneEqNum > 0 ) {
						if ( ! IsAutoSize && ! SizingDesRunThisZone ) {
							HardSizeNoDesRun = true;
							if ( DXCoil( DXCoilNum ).MSRatedAirVolFlowRate( Mode ) > 0.0 ) {
								ReportSizingOutput( DXCoil( DXCoilNum ).DXCoilType, DXCoil( DXCoilNum ).Name, "Speed " + TrimSigDigits( Mode ) + " User-Specified Rated Air Flow Rate [m3/s]", DXCoil( DXCoilNum ).MSRatedAirVolFlowRate( Mode ) );
							}
						} else {
							CheckZoneSizing( DXCoil( DXCoilNum ).DXCoilType, DXCoil( DXCoilNum ).Name );
							MSRatedAirVolFlowRateDes = max( FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow, FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow );
						}
					}
					if ( MSRatedAirVolFlowRateDes < SmallAirVolFlow ) {
						MSRatedAirVolFlowRateDes = 0.0;
					}
				} else {
					MSRatedAirVolFlowRateDes = DXCoil( DXCoilNum ).MSRatedAirVolFlowRate( DXCoil( DXCoilNum ).NumOfSpeeds ) * Mode / DXCoil( DXCoilNum ).NumOfSpeeds;
				}
				if ( ! HardSizeNoDesRun ) {
					if ( IsAutoSize ) {
						DXCoil( DXCoilNum ).MSRatedAirVolFlowRate( Mode ) = MSRatedAirVolFlowRateDes;
						ReportSizingOutput( DXCoil( DXCoilNum ).DXCoilType, DXCoil( DXCoilNum ).Name, "Speed " + TrimSigDigits( Mode ) + " Design Size Rated Air Flow Rate [m3/s]", MSRatedAirVolFlowRateDes );
					} else {
						if ( DXCoil( DXCoilNum ).MSRatedAirVolFlowRate( Mode ) > 0.0 && MSRatedAirVolFlowRateDes > 0.0 && ! HardSizeNoDesRun ) {
							MSRatedAirVolFlowRateUser = DXCoil( DXCoilNum ).MSRatedAirVolFlowRate( Mode );
							ReportSizingOutput( DXCoil( DXCoilNum ).DXCoilType, DXCoil( DXCoilNum ).Name, "Speed " + TrimSigDigits( Mode ) + " Design Size Rated Air Flow Rate [m3/s]", MSRatedAirVolFlowRateDes, "Speed " + TrimSigDigits( Mode ) + " User-Specified Rated Air Flow Rate [m3/s]", MSRatedAirVolFlowRateUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( MSRatedAirVolFlowRateDes - MSRatedAirVolFlowRateUser ) / MSRatedAirVolFlowRateUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeDxCoil: Potential issue with equipment sizing for " + DXCoil( DXCoilNum ).DXCoilType + ' ' + DXCoil( DXCoilNum ).Name );
									ShowContinueError( "User-Specified Rated Air Volume Flow Rate of " + RoundSigDigits( MSRatedAirVolFlowRateUser, 5 ) + " [m3/s]" );
									ShowContinueError( "differs from Design Size Rated Air Volume Flow Rate of " + RoundSigDigits( MSRatedAirVolFlowRateDes, 5 ) + " [m3/s]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
					}
				}
			}

			// Ensure flow rate at lower speed must be lower or equal to the flow rate at higher speed. Otherwise, a severe error is isssued.
			for ( Mode = 1; Mode <= DXCoil( DXCoilNum ).NumOfSpeeds - 1; ++Mode ) {
				if ( DXCoil( DXCoilNum ).MSRatedAirVolFlowRate( Mode ) > DXCoil( DXCoilNum ).MSRatedAirVolFlowRate( Mode + 1 ) ) {
					ShowWarningError( "SizeDXCoil: " + DXCoil( DXCoilNum ).DXCoilType + ' ' + DXCoil( DXCoilNum ).Name + ", Speed " + TrimSigDigits( Mode ) + " Rated Air Flow Rate must be less than or equal to Speed " + TrimSigDigits( Mode + 1 ) + " Rated Air Flow Rate." );
					ShowContinueError( "Instead, " + RoundSigDigits( DXCoil( DXCoilNum ).MSRatedAirVolFlowRate( Mode ), 2 ) + " > " + RoundSigDigits( DXCoil( DXCoilNum ).MSRatedAirVolFlowRate( Mode + 1 ), 2 ) );
					ShowFatalError( "Preceding conditions cause termination." );
				}
			}
			// Rated Secondary Coil Airflow Rates for AirCooled condenser type
			if ( DXCoil( DXCoilNum ).IsSecondaryDXCoilInZone ) {
				for ( Mode = DXCoil( DXCoilNum ).NumOfSpeeds; Mode >= 1; --Mode ) {
					IsAutoSize = false;
					if ( DXCoil( DXCoilNum ).MSSecCoilAirFlow( Mode ) == AutoSize ) {
						IsAutoSize = true;
					}
					// Auto size Primary Coil air flow * Secondary Coil Scaling Factor
					SecCoilAirFlowDes = DXCoil( DXCoilNum ).MSRatedAirVolFlowRate( Mode ) * DXCoil( DXCoilNum ).MSSecCoilAirFlowScalingFactor( Mode );
					if ( IsAutoSize ) {
						DXCoil( DXCoilNum ).MSSecCoilAirFlow( Mode ) = SecCoilAirFlowDes;
						ReportSizingOutput( DXCoil( DXCoilNum ).DXCoilType, DXCoil( DXCoilNum ).Name, "Speed " + TrimSigDigits( Mode ) + " Design Size Secondary Coil Air Flow Rate [m3/s]", SecCoilAirFlowDes );
					} else {
						if ( DXCoil( DXCoilNum ).MSSecCoilAirFlow( Mode ) > 0.0 && SecCoilAirFlowDes > 0.0 && !HardSizeNoDesRun ) {
							SecCoilAirFlowUser = DXCoil( DXCoilNum ).MSSecCoilAirFlow( Mode );
							ReportSizingOutput( DXCoil( DXCoilNum ).DXCoilType, DXCoil( DXCoilNum ).Name, "Speed " + TrimSigDigits( Mode ) + " Design Size Secondary Coil Air Flow Rate [m3/s]", SecCoilAirFlowDes, "Speed " + TrimSigDigits( Mode ) + " User-Specified Secondary Coil Air Flow Rate [m3/s]", SecCoilAirFlowUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( SecCoilAirFlowDes - SecCoilAirFlowUser ) / SecCoilAirFlowUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeDxCoil: Potential issue with equipment sizing for " + DXCoil( DXCoilNum ).DXCoilType + ' ' + DXCoil( DXCoilNum ).Name );
									ShowContinueError( "User-Specified Secondary Coil Air Flow Rate of " + RoundSigDigits( SecCoilAirFlowUser, 5 ) + " [m3/s]" );
									ShowContinueError( "differs from Design Size Secondary Coil Air Flow Rate of " + RoundSigDigits( SecCoilAirFlowDes, 5 ) + " [m3/s]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
					}
				}
			}

			// Sizing rated total heating capacity
			for ( Mode = DXCoil( DXCoilNum ).NumOfSpeeds; Mode >= 1; --Mode ) {
				IsAutoSize = false;
				if ( DXCoil( DXCoilNum ).MSRatedTotCap( Mode ) == AutoSize ) {
					IsAutoSize = true;
				}
				if ( Mode == DXCoil( DXCoilNum ).NumOfSpeeds ) {
					// Heating capacity is assumed to be equal to the cooling capacity
					NumOfSpeedCompanion = DXCoil( DXCoil( DXCoilNum ).CompanionUpstreamDXCoil ).NumOfSpeeds;
					MSRatedTotCapDes = DXCoil( DXCoil( DXCoilNum ).CompanionUpstreamDXCoil ).MSRatedTotCap( NumOfSpeedCompanion );
				} else {
					MSRatedTotCapDes = DXCoil( DXCoilNum ).MSRatedTotCap( DXCoil( DXCoilNum ).NumOfSpeeds ) * Mode / DXCoil( DXCoilNum ).NumOfSpeeds;
				}
				if ( IsAutoSize ) {
					DXCoil( DXCoilNum ).MSRatedTotCap( Mode ) = MSRatedTotCapDes;
					ReportSizingOutput( DXCoil( DXCoilNum ).DXCoilType, DXCoil( DXCoilNum ).Name, "Speed " + TrimSigDigits( Mode ) + " Design Size Rated Total Heating Capacity [W]", MSRatedTotCapDes );
				} else {
					if ( DXCoil( DXCoilNum ).MSRatedTotCap( Mode ) > 0.0 && MSRatedTotCapDes > 0.0 && ! HardSizeNoDesRun ) {
						MSRatedTotCapUser = DXCoil( DXCoilNum ).MSRatedTotCap( Mode );
						ReportSizingOutput( DXCoil( DXCoilNum ).DXCoilType, DXCoil( DXCoilNum ).Name, "Speed " + TrimSigDigits( Mode ) + " Design Size Rated Total Heating Capacity [W]", MSRatedTotCapDes, "Speed " + TrimSigDigits( Mode ) + " User-Specified Rated Total Heating Capacity [W]", MSRatedTotCapUser );
						if ( DisplayExtraWarnings ) {
							if ( ( std::abs( MSRatedTotCapDes - MSRatedTotCapUser ) / MSRatedTotCapUser ) > AutoVsHardSizingThreshold ) {
								ShowMessage( "SizeDxCoil: Potential issue with equipment sizing for " + DXCoil( DXCoilNum ).DXCoilType + ' ' + DXCoil( DXCoilNum ).Name );
								ShowContinueError( "User-Specified Rated Total Heating Capacity of " + RoundSigDigits( MSRatedTotCapUser, 2 ) + " [W]" );
								ShowContinueError( "differs from Design Size Rated Total Heating Capacity of " + RoundSigDigits( MSRatedTotCapDes, 2 ) + " [W]" );
								ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
								ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
							}
						}
					}
				}
			}

			// Ensure capacity at lower speed must be lower or equal to the capacity at higher speed.
			for ( Mode = 1; Mode <= DXCoil( DXCoilNum ).NumOfSpeeds - 1; ++Mode ) {
				if ( DXCoil( DXCoilNum ).MSRatedTotCap( Mode ) > DXCoil( DXCoilNum ).MSRatedTotCap( Mode + 1 ) ) {
					ShowWarningError( "SizeDXCoil: " + DXCoil( DXCoilNum ).DXCoilType + ' ' + DXCoil( DXCoilNum ).Name + ", Speed " + TrimSigDigits( Mode ) + " Rated Total Heating Capacity must be less than or equal to Speed " + TrimSigDigits( Mode + 1 ) + " Rated Total Heating Capacity." );
					ShowContinueError( "Instead, " + RoundSigDigits( DXCoil( DXCoilNum ).MSRatedTotCap( Mode ), 2 ) + " > " + RoundSigDigits( DXCoil( DXCoilNum ).MSRatedTotCap( Mode + 1 ), 2 ) );
					ShowFatalError( "Preceding conditions cause termination." );
				}
			}

			// Resistive Defrost Heater Capacity = capacity at the first stage
			// Sizing defrost heater capacity
			IsAutoSize = false;
			if ( DXCoil( DXCoilNum ).DefrostCapacity == AutoSize ) {
				IsAutoSize = true;
			}
			if ( DXCoil( DXCoilNum ).DefrostStrategy == Resistive ) {
				DefrostCapacityDes = DXCoil( DXCoilNum ).MSRatedTotCap( 1 );
			} else {
				DefrostCapacityDes = 0.0;
			}
			if ( IsAutoSize ) {
				DXCoil( DXCoilNum ).DefrostCapacity = DefrostCapacityDes;
				ReportSizingOutput( DXCoil( DXCoilNum ).DXCoilType, DXCoil( DXCoilNum ).Name, "Design Size Resistive Defrost Heater Capacity", DefrostCapacityDes );
			} else {
				if ( DXCoil( DXCoilNum ).DefrostCapacity > 0.0 && DefrostCapacityDes > 0.0 && ! HardSizeNoDesRun ) {
					DefrostCapacityUser = DXCoil( DXCoilNum ).DefrostCapacity;
					ReportSizingOutput( DXCoil( DXCoilNum ).DXCoilType, DXCoil( DXCoilNum ).Name, "Design Size Resistive Defrost Heater Capacity", DefrostCapacityDes, "User-Specified Resistive Defrost Heater Capacity", DefrostCapacityUser );
					if ( DisplayExtraWarnings ) {
						if ( ( std::abs( DefrostCapacityDes - DefrostCapacityUser ) / DefrostCapacityUser ) > AutoVsHardSizingThreshold ) {
							ShowWarningMessage( "SizeDxCoil: Potential issue with equipment sizing for " + DXCoil( DXCoilNum ).DXCoilType + ' ' + DXCoil( DXCoilNum ).Name );
							ShowContinueError( "User-Specified Resistive Defrost Heater Capacity of " + RoundSigDigits( DefrostCapacityUser, 2 ) + "[W]" );
							ShowContinueError( "differs from Design Size Resistive Defrost Heater Capacity of " + RoundSigDigits( DefrostCapacityDes, 2 ) + "[W]" );
							ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
							ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
						}
					}
				}
			}
		}

		// Call routine that computes AHRI certified rating for single-speed DX Coils
		if ( ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_CoolingSingleSpeed && DXCoil( DXCoilNum ).CondenserType( 1 ) == AirCooled ) || DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_HeatingEmpirical ) {
			CalcDXCoilStandardRating( DXCoil( DXCoilNum ).Name, DXCoil( DXCoilNum ).DXCoilType, DXCoil( DXCoilNum ).DXCoilType_Num, 1, DXCoil( DXCoilNum ).RatedTotCap( 1 ), DXCoil( DXCoilNum ).RatedCOP( 1 ), DXCoil( DXCoilNum ).CCapFFlow( 1 ), DXCoil( DXCoilNum ).CCapFTemp( 1 ), DXCoil( DXCoilNum ).EIRFFlow( 1 ), DXCoil( DXCoilNum ).EIRFTemp( 1 ), DXCoil( DXCoilNum ).PLFFPLR( 1 ), DXCoil( DXCoilNum ).RatedAirVolFlowRate( 1 ), DXCoil( DXCoilNum ).FanPowerPerEvapAirFlowRate( 1 ), DXCoil( DXCoilNum ).RegionNum, DXCoil( DXCoilNum ).MinOATCompressor, DXCoil( DXCoilNum ).OATempCompressorOn, DXCoil( DXCoilNum ).OATempCompressorOnOffBlank, DXCoil( DXCoilNum ).DefrostControl, DXCoil( DXCoilNum ).ASHRAE127StdRprt );
		}
		// Call routine that computes AHRI certified rating for multi-speed DX cooling Coils
		if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_MultiSpeedCooling || DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_MultiSpeedHeating ) {
			CalcDXCoilStandardRating( DXCoil( DXCoilNum ).Name, DXCoil( DXCoilNum ).DXCoilType, DXCoil( DXCoilNum ).DXCoilType_Num, DXCoil( DXCoilNum ).NumOfSpeeds, DXCoil( DXCoilNum ).MSRatedTotCap, DXCoil( DXCoilNum ).MSRatedCOP, DXCoil( DXCoilNum ).MSCCapFFlow, DXCoil( DXCoilNum ).MSCCapFTemp, DXCoil( DXCoilNum ).MSEIRFFlow, DXCoil( DXCoilNum ).MSEIRFTemp, DXCoil( DXCoilNum ).MSPLFFPLR, DXCoil( DXCoilNum ).MSRatedAirVolFlowRate, DXCoil( DXCoilNum ).MSFanPowerPerEvapAirFlowRate, DXCoil( DXCoilNum ).RegionNum, DXCoil( DXCoilNum ).MinOATCompressor, DXCoil( DXCoilNum ).OATempCompressorOn, DXCoil( DXCoilNum ).OATempCompressorOnOffBlank, DXCoil( DXCoilNum ).DefrostControl );
		}

		//create predefined report entries
		equipName = DXCoil( DXCoilNum ).Name;
		// put tables for cooling and heating separate
		{ auto const SELECT_CASE_var( DXCoil( DXCoilNum ).DXCoilType_Num );
		if ( ( SELECT_CASE_var == CoilDX_CoolingSingleSpeed ) || ( SELECT_CASE_var == CoilDX_CoolingTwoSpeed ) || ( SELECT_CASE_var == CoilDX_CoolingTwoStageWHumControl ) || ( SELECT_CASE_var == CoilDX_MultiSpeedCooling ) ) {
			PreDefTableEntry( pdchCoolCoilType, equipName, DXCoil( DXCoilNum ).DXCoilType );
			if ( DXCoil( DXCoilNum ).NumOfSpeeds == 0 ) {
				if ( DXCoil( DXCoilNum ).NumCapacityStages == 1 ) {
					PreDefTableEntry( pdchCoolCoilTotCap, equipName, DXCoil( DXCoilNum ).RatedTotCap( 1 ) );
					PreDefTableEntry( pdchCoolCoilSensCap, equipName, DXCoil( DXCoilNum ).RatedTotCap( 1 ) * DXCoil( DXCoilNum ).RatedSHR( 1 ) );
					PreDefTableEntry( pdchCoolCoilLatCap, equipName, DXCoil( DXCoilNum ).RatedTotCap( 1 ) - DXCoil( DXCoilNum ).RatedTotCap( 1 ) * DXCoil( DXCoilNum ).RatedSHR( 1 ) );
					PreDefTableEntry( pdchCoolCoilSHR, equipName, DXCoil( DXCoilNum ).RatedSHR( 1 ) );
					PreDefTableEntry( pdchCoolCoilNomEff, equipName, DXCoil( DXCoilNum ).RatedCOP( 1 ) );
				} else {
					PreDefTableEntry( pdchCoolCoilTotCap, equipName, DXCoil( DXCoilNum ).RatedTotCap( 2 ) );
					PreDefTableEntry( pdchCoolCoilSensCap, equipName, DXCoil( DXCoilNum ).RatedTotCap( 2 ) * DXCoil( DXCoilNum ).RatedSHR( 2 ) );
					PreDefTableEntry( pdchCoolCoilLatCap, equipName, DXCoil( DXCoilNum ).RatedTotCap( 2 ) - DXCoil( DXCoilNum ).RatedTotCap( 2 ) * DXCoil( DXCoilNum ).RatedSHR( 2 ) );
					PreDefTableEntry( pdchCoolCoilSHR, equipName, DXCoil( DXCoilNum ).RatedSHR( 2 ) );
					PreDefTableEntry( pdchCoolCoilNomEff, equipName, DXCoil( DXCoilNum ).RatedCOP( 2 ) );
				}
			} else {
				for ( Mode = 1; Mode <= DXCoil( DXCoilNum ).NumOfSpeeds; ++Mode ) {
					PreDefTableEntry( pdchCoolCoilTotCap, equipName, DXCoil( DXCoilNum ).MSRatedTotCap( Mode ) );
					PreDefTableEntry( pdchCoolCoilSensCap, equipName, DXCoil( DXCoilNum ).MSRatedTotCap( Mode ) * DXCoil( DXCoilNum ).MSRatedSHR( Mode ) );
					PreDefTableEntry( pdchCoolCoilLatCap, equipName, DXCoil( DXCoilNum ).MSRatedTotCap( Mode ) - DXCoil( DXCoilNum ).MSRatedTotCap( Mode ) * DXCoil( DXCoilNum ).MSRatedSHR( Mode ) );
					PreDefTableEntry( pdchCoolCoilSHR, equipName, DXCoil( DXCoilNum ).MSRatedSHR( Mode ) );
					PreDefTableEntry( pdchCoolCoilNomEff, equipName, DXCoil( DXCoilNum ).MSRatedCOP( Mode ) );
				}
			}
			addFootNoteSubTable( pdstCoolCoil, "Nominal values are gross at rated conditions, i.e., the supply air fan heat and electric power NOT accounted for." );

		} else if ( ( SELECT_CASE_var == CoilDX_HeatingEmpirical ) || ( SELECT_CASE_var == CoilDX_MultiSpeedHeating ) || ( SELECT_CASE_var == CoilDX_HeatPumpWaterHeaterPumped ) || ( SELECT_CASE_var == CoilDX_HeatPumpWaterHeaterWrapped ) ) {
			PreDefTableEntry( pdchHeatCoilType, equipName, DXCoil( DXCoilNum ).DXCoilType );
			if ( DXCoil( DXCoilNum ).NumOfSpeeds == 0 ) {
				if ( DXCoil( DXCoilNum ).NumCapacityStages == 1 ) {
					PreDefTableEntry( pdchHeatCoilNomCap, equipName, DXCoil( DXCoilNum ).RatedTotCap( 1 ) );
					PreDefTableEntry( pdchHeatCoilNomEff, equipName, DXCoil( DXCoilNum ).RatedCOP( 1 ) );
				} else {
					PreDefTableEntry( pdchHeatCoilNomCap, equipName, DXCoil( DXCoilNum ).RatedTotCap( 2 ) );
					PreDefTableEntry( pdchHeatCoilNomEff, equipName, DXCoil( DXCoilNum ).RatedCOP( 2 ) );
				}
			} else {
				for ( Mode = 1; Mode <= DXCoil( DXCoilNum ).NumOfSpeeds; ++Mode ) {
					PreDefTableEntry( pdchHeatCoilNomCap, equipName, DXCoil( DXCoilNum ).MSRatedTotCap( Mode ) );
					PreDefTableEntry( pdchHeatCoilNomEff, equipName, DXCoil( DXCoilNum ).MSRatedCOP( Mode ) );
				}
			}
			addFootNoteSubTable( pdstHeatCoil, "Nominal values are gross at rated conditions, i.e., the supply air fan heat and electric power NOT accounted for." );
		}}

	}

	void
	CalcHPWHDXCoil(
		int const DXCoilNum, // the number of the DX coil to be simulated
		Real64 const PartLoadRatio // sensible water heating load / full load sensible water heating capacity
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   May 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the gross cooling capacity of a heat pump water heater evaporator and
		// heating capacity of the condenser coil given the rated heating capacity and COP.

		// METHODOLOGY EMPLOYED:
		// The routine requires the user to enter the total heating capacity and COP for the
		// heat pump water heater along with logicals defining if fan and condenser pump are included.
		// Since manufacturer's can rate their HPWH equipment with or without including condenser
		// pump heat, this information is required to accurately determine the condenser's leaving
		// water temperature. In addition, knowledge of the fan heat is required to back into
		// a compressor COP.

		// Using/Aliasing
		using CurveManager::CurveValue;
		using General::TrimSigDigits;
		using DataHVACGlobals::FanElecPower;
		using DataHVACGlobals::HPWHInletDBTemp;
		using DataHVACGlobals::HPWHInletWBTemp;
		using DataHVACGlobals::DXCoilTotalCapacity;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcHPWHDXCoil" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 RatedHeatingCapacity; // Water heating rated capacity with or without condenser water pump heat (W)
		Real64 RatedHeatingCOP; // Water heating rated COP with or without evap fan and cond water pump heat (W/W)
		Real64 OperatingHeatingCapacity; // Water heating operating capacity including the impact of capacity and COP curves (W)
		Real64 OperatingHeatingCOP; // Water heating operating COP including the impact of capacity and COP curves (W/W)
		Real64 OperatingHeatingPower; // Water heating operating Power (W)
		Real64 CompressorPower; // Power consumed by compressor only (W)

		Real64 TotalTankHeatingCapacity; // Water heating capacity corrected for condenser water pump heat (W)
		Real64 TankHeatingCOP; // Water heating COP corrected for fan and condenser water pump power (W/W)
		// (these previous 2 variables also include the impact of capacity and COP curves)
		Real64 EvapCoolingCapacity; // Air cooling capacity corrected for evap fan and cond water pump heat (W)
		Real64 InletWaterTemp; // Condenser water inlet temperature (C)
		Real64 OutletWaterTemp; // Condenser water outlet temperature (C)
		Real64 EvapInletMassFlowRate; // Evaporator air inlet mass flow rate (m3/s)
		Real64 CondInletMassFlowRate; // Condenser water inlet mass flow rate (m3/s)
		Real64 CpWater; // Specific heat of condenser inlet water (J/Kg/k)
		Real64 InletAirTemp; // HPWH inlet air temperature (dry-bulb or wet-bulb) (C)
		Real64 HeatCapFTemp; // Output of HPWH Heating Capacity as a Function of Temperature curve
		Real64 HeatCapFAirFlow; // Output of HPWH Heating Capacity as a Function of Air Flow Rate Ratio curve
		Real64 HeatCapFWaterFlow; // Output of HPWH Heating Capacity as a Function of Water Flow Rate Ratio curve
		Real64 HeatCOPFTemp; // Output of HPWH COP as a Function of Temperature curve
		Real64 HeatCOPFAirFlow; // Output of HPWH COP as a Function of Air Flow Rate Ratio curve
		Real64 HeatCOPFWaterFlow; // Output of HPWH COP as a Function of Water Flow Rate Ratio curve
		Real64 AirFlowRateRatio; // Ratio of evaporator inlet air mass flow rate to rated mass flow rate
		Real64 WaterFlowRateRatio; // Ratio of evaporator inlet water mass flow rate to rated mass flow rate
		Real64 PartLoadFraction; // Output of Part Load Fraction as a Function of Part Load Ratio curve
		Real64 PumpHeatToWater; // Amount of pump heat attributed to heating water
		Real64 HPRTF; // Heat pump run time fraction

		// References to Coil and Node struct
		DXCoilData & Coil = DXCoil( DXCoilNum );
		NodeData & AirInletNode = Node( Coil.AirInNode );
		NodeData & WaterInletNode = Node( Coil.WaterInNode );
		NodeData & WaterOutletNode = Node( Coil.WaterOutNode );

		// If heat pump water heater is OFF, set outlet to inlet and RETURN
		// Also set the heating energy rate to zero
		if ( PartLoadRatio == 0.0 ) {
			WaterOutletNode = WaterInletNode;
			Coil.TotalHeatingEnergyRate = 0.0;
			return;
		} else {
			RatedHeatingCapacity = Coil.RatedTotCap2;
			RatedHeatingCOP = Coil.RatedCOP( 1 );
			InletWaterTemp = WaterInletNode.Temp;
			CondInletMassFlowRate = WaterInletNode.MassFlowRate / PartLoadRatio;
			EvapInletMassFlowRate = AirInletNode.MassFlowRate / PartLoadRatio;
			CpWater = CPHW( InletWaterTemp );
			CompressorPower = 0.0;
			OperatingHeatingPower = 0.0;
			TankHeatingCOP = 0.0;
		}

		// determine inlet air temperature type for curve objects
		if ( Coil.InletAirTemperatureType == WetBulbIndicator ) {
			InletAirTemp = HPWHInletWBTemp;
		} else {
			InletAirTemp = HPWHInletDBTemp;
		}

		// get output of Heating Capacity and Heating COP curves (curves default to 1 if user has not specified curve name)
		if ( Coil.HCapFTemp > 0 ) {
			if ( Coil.HCapFTempCurveType == Cubic ) {
				HeatCapFTemp = CurveValue( Coil.HCapFTemp, InletAirTemp );
			} else {
				HeatCapFTemp = CurveValue( Coil.HCapFTemp, InletAirTemp, InletWaterTemp );
			}
			//   Warn user if curve output goes negative
			if ( HeatCapFTemp < 0.0 ) {
				if ( Coil.HCapFTempErrorIndex == 0 ) {
					ShowWarningMessage( Coil.DXCoilType + " \"" + Coil.Name + "\":" );
					ShowContinueError( " HPWH Heating Capacity Modifier curve (function of temperature) output is negative (" + TrimSigDigits( HeatCapFTemp, 3 ) + ")." );
					if ( Coil.HCapFTempCurveType == BiQuadratic ) {
						ShowContinueError( " Negative value occurs using an inlet air temperature of " + TrimSigDigits( InletAirTemp, 1 ) + " and an inlet water temperature of " + TrimSigDigits( InletWaterTemp, 1 ) + '.' );
					} else {
						ShowContinueError( " Negative value occurs using an inlet air temperature of " + TrimSigDigits( InletAirTemp, 1 ) + '.' );
					}
					ShowContinueErrorTimeStamp( " Resetting curve output to zero and continuing simulation." );
				}
				ShowRecurringWarningErrorAtEnd( Coil.DXCoilType + " \"" + Coil.Name + "\": HPWH Heating Capacity Modifier curve (function of temperature) output is negative warning continues...", Coil.HCapFTempErrorIndex, HeatCapFTemp, HeatCapFTemp, _, "[C]", "[C]" );
				HeatCapFTemp = 0.0;
			}
		} else {
			HeatCapFTemp = 1.0;
		}

		if ( Coil.HCOPFTemp > 0 ) {
			if ( Coil.HCOPFTempCurveType == Cubic ) {
				HeatCOPFTemp = CurveValue( Coil.HCOPFTemp, InletAirTemp );
			} else {
				HeatCOPFTemp = CurveValue( Coil.HCOPFTemp, InletAirTemp, InletWaterTemp );
			}
			//   Warn user if curve output goes negative
			if ( HeatCOPFTemp < 0.0 ) {
				if ( Coil.HCOPFTempErrorIndex == 0 ) {
					ShowWarningMessage( Coil.DXCoilType + " \"" + Coil.Name + "\":" );
					ShowContinueError( " HPWH Heating COP Modifier curve (function of temperature) output is negative (" + TrimSigDigits( HeatCOPFTemp, 3 ) + ")." );
					if ( Coil.HCOPFTempCurveType == BiQuadratic ) {
						ShowContinueError( " Negative value occurs using an inlet air temperature of " + TrimSigDigits( InletAirTemp, 1 ) + " and an inlet water temperature of " + TrimSigDigits( InletWaterTemp, 1 ) + '.' );
					} else {
						ShowContinueError( " Negative value occurs using an inlet air temperature of " + TrimSigDigits( InletAirTemp, 1 ) + '.' );
					}
					ShowContinueErrorTimeStamp( " Resetting curve output to zero and continuing simulation." );
				}
				ShowRecurringWarningErrorAtEnd( Coil.DXCoilType + " \"" + Coil.Name + "\": HPWH Heating COP Modifier curve (function of temperature) output is negative warning continues...", Coil.HCOPFTempErrorIndex, HeatCOPFTemp, HeatCOPFTemp, _, "[C]", "[C]" );
				HeatCOPFTemp = 0.0;
			}
		} else {
			HeatCOPFTemp = 1.0;
		}

		if ( Coil.HCapFAirFlow > 0 ) {
			AirFlowRateRatio = EvapInletMassFlowRate / ( Coil.RatedAirMassFlowRate( 1 ) );
			HeatCapFAirFlow = CurveValue( Coil.HCapFAirFlow, AirFlowRateRatio );
			//   Warn user if curve output goes negative
			if ( HeatCapFAirFlow < 0.0 ) {
				if ( Coil.HCapFAirFlowErrorIndex == 0 ) {
					ShowWarningMessage( Coil.DXCoilType + " \"" + Coil.Name + "\":" );
					ShowContinueError( " HPWH Heating Capacity Modifier curve (function of air flow fraction) output is negative (" + TrimSigDigits( HeatCapFAirFlow, 3 ) + ")." );
					ShowContinueError( " Negative value occurs using an air flow fraction of " + TrimSigDigits( AirFlowRateRatio, 3 ) + '.' );
					ShowContinueErrorTimeStamp( " Resetting curve output to zero and continuing simulation." );
				}
				ShowRecurringWarningErrorAtEnd( Coil.DXCoilType + " \"" + Coil.Name + "\": HPWH Heating Capacity Modifier curve (function of air flow fraction) output is negative warning continues...", Coil.HCapFAirFlowErrorIndex, HeatCapFAirFlow, HeatCapFAirFlow );
				HeatCapFAirFlow = 0.0;
			}
		} else {
			HeatCapFAirFlow = 1.0;
		}

		if ( Coil.HCOPFAirFlow > 0 ) {
			AirFlowRateRatio = EvapInletMassFlowRate / ( Coil.RatedAirMassFlowRate( 1 ) );
			HeatCOPFAirFlow = CurveValue( Coil.HCOPFAirFlow, AirFlowRateRatio );
			//   Warn user if curve output goes negative
			if ( HeatCOPFAirFlow < 0.0 ) {
				if ( Coil.HCOPFAirFlowErrorIndex == 0 ) {
					ShowWarningMessage( Coil.DXCoilType + " \"" + Coil.Name + "\":" );
					ShowContinueError( " HPWH Heating COP Modifier curve (function of air flow fraction) output is negative (" + TrimSigDigits( HeatCOPFAirFlow, 3 ) + ")." );
					ShowContinueError( " Negative value occurs using an air flow fraction of " + TrimSigDigits( AirFlowRateRatio, 3 ) + '.' );
					ShowContinueErrorTimeStamp( " Resetting curve output to zero and continuing simulation." );
				}
				ShowRecurringWarningErrorAtEnd( Coil.DXCoilType + " \"" + Coil.Name + "\": HPWH Heating COP Modifier curve (function of air flow fraction) output is negative warning continues...", Coil.HCOPFAirFlowErrorIndex, HeatCOPFAirFlow, HeatCOPFAirFlow );
				HeatCOPFAirFlow = 0.0;
			}
		} else {
			HeatCOPFAirFlow = 1.0;
		}

		if ( Coil.HCapFWaterFlow > 0 ) {
			WaterFlowRateRatio = CondInletMassFlowRate / ( Coil.RatedHPWHCondWaterFlow * RhoH2O( InletWaterTemp ) );
			HeatCapFWaterFlow = CurveValue( Coil.HCapFWaterFlow, WaterFlowRateRatio );
			//   Warn user if curve output goes negative
			if ( HeatCapFWaterFlow < 0.0 ) {
				if ( Coil.HCapFWaterFlowErrorIndex == 0 ) {
					ShowWarningMessage( Coil.DXCoilType + " \"" + Coil.Name + "\":" );
					ShowContinueError( " HPWH Heating Capacity Modifier curve (function of water flow fraction) output is negative (" + TrimSigDigits( HeatCapFWaterFlow, 3 ) + ")." );
					ShowContinueError( " Negative value occurs using a water flow fraction of " + TrimSigDigits( WaterFlowRateRatio, 3 ) + '.' );
					ShowContinueErrorTimeStamp( " Resetting curve output to zero and continuing simulation." );
				}
				ShowRecurringWarningErrorAtEnd( Coil.DXCoilType + " \"" + Coil.Name + "\": HPWH Heating Capacity Modifier curve (function of water flow fraction) output is negative warning continues...", Coil.HCapFWaterFlowErrorIndex, HeatCapFWaterFlow, HeatCapFWaterFlow );
				HeatCapFWaterFlow = 0.0;
			}
		} else {
			HeatCapFWaterFlow = 1.0;
		}

		if ( Coil.HCOPFWaterFlow > 0 ) {
			WaterFlowRateRatio = CondInletMassFlowRate / ( Coil.RatedHPWHCondWaterFlow * RhoH2O( InletWaterTemp ) );
			HeatCOPFWaterFlow = CurveValue( Coil.HCOPFWaterFlow, WaterFlowRateRatio );
			//   Warn user if curve output goes negative
			if ( HeatCOPFWaterFlow < 0.0 ) {
				if ( Coil.HCOPFWaterFlowErrorIndex == 0 ) {
					ShowWarningMessage( Coil.DXCoilType + " \"" + Coil.Name + "\":" );
					ShowContinueError( " HPWH Heating COP Modifier curve (function of water flow fraction) output is negative (" + TrimSigDigits( HeatCOPFWaterFlow, 3 ) + ")." );
					ShowContinueError( " Negative value occurs using a water flow fraction of " + TrimSigDigits( WaterFlowRateRatio, 3 ) + '.' );
					ShowContinueErrorTimeStamp( " Resetting curve output to zero and continuing simulation." );
				}
				ShowRecurringWarningErrorAtEnd( Coil.DXCoilType + " \"" + Coil.Name + "\": HPWH Heating COP Modifier curve (function of water flow fraction) output is negative warning continues...", Coil.HCOPFWaterFlowErrorIndex, HeatCOPFWaterFlow, HeatCOPFWaterFlow );
				HeatCOPFWaterFlow = 0.0;
			}
		} else {
			HeatCOPFWaterFlow = 1.0;
		}

		// adjust Heating Capacity and COP for off-design conditions
		OperatingHeatingCapacity = RatedHeatingCapacity * HeatCapFTemp * HeatCapFAirFlow * HeatCapFWaterFlow;
		OperatingHeatingCOP = RatedHeatingCOP * HeatCOPFTemp * HeatCOPFAirFlow * HeatCOPFWaterFlow;

		if ( OperatingHeatingCOP > 0.0 ) OperatingHeatingPower = OperatingHeatingCapacity / OperatingHeatingCOP;

		PumpHeatToWater = Coil.HPWHCondPumpElecNomPower * Coil.HPWHCondPumpFracToWater;
		TankHeatingCOP = OperatingHeatingCOP;

		// account for pump heat if not included in total water heating capacity
		if ( Coil.CondPumpHeatInCapacity ) {
			TotalTankHeatingCapacity = OperatingHeatingCapacity;
		} else {
			TotalTankHeatingCapacity = OperatingHeatingCapacity + PumpHeatToWater;
		}

		// find part load fraction to calculate RTF
		if ( Coil.PLFFPLR( 1 ) > 0 ) {
			PartLoadFraction = max( 0.7, CurveValue( Coil.PLFFPLR( 1 ), PartLoadRatio ) );
		} else {
			PartLoadFraction = 1.0;
		}

		HPRTF = min( 1.0, ( PartLoadRatio / PartLoadFraction ) );

		// calculate evaporator total cooling capacity
		if ( HPRTF > 0.0 ) {
			if ( Coil.FanPowerIncludedInCOP ) {
				if ( Coil.CondPumpPowerInCOP ) {
					//       make sure fan power is full load fan power
					CompressorPower = OperatingHeatingPower - FanElecPower / HPRTF - Coil.HPWHCondPumpElecNomPower;
					if ( OperatingHeatingPower > 0.0 ) TankHeatingCOP = TotalTankHeatingCapacity / OperatingHeatingPower;
				} else {
					CompressorPower = OperatingHeatingPower - FanElecPower / HPRTF;
					if ( ( OperatingHeatingPower + Coil.HPWHCondPumpElecNomPower ) > 0.0 ) TankHeatingCOP = TotalTankHeatingCapacity / ( OperatingHeatingPower + Coil.HPWHCondPumpElecNomPower );
				}
			} else {
				if ( Coil.CondPumpPowerInCOP ) {
					//       make sure fan power is full load fan power
					CompressorPower = OperatingHeatingPower - Coil.HPWHCondPumpElecNomPower;
					if ( ( OperatingHeatingPower + FanElecPower / HPRTF ) > 0.0 ) TankHeatingCOP = TotalTankHeatingCapacity / ( OperatingHeatingPower + FanElecPower / HPRTF );
				} else {
					CompressorPower = OperatingHeatingPower;
					if ( ( OperatingHeatingPower + FanElecPower / HPRTF + Coil.HPWHCondPumpElecNomPower ) > 0.0 ) TankHeatingCOP = TotalTankHeatingCapacity / ( OperatingHeatingPower + FanElecPower / HPRTF + Coil.HPWHCondPumpElecNomPower );
				}
			}
		}

		if ( Coil.CondPumpHeatInCapacity ) {
			EvapCoolingCapacity = TotalTankHeatingCapacity - PumpHeatToWater - CompressorPower;
		} else {
			EvapCoolingCapacity = TotalTankHeatingCapacity - CompressorPower;
		}

		// set evaporator total cooling capacity prior to CalcDOE2DXCoil subroutine
		Coil.RatedTotCap( 1 ) = EvapCoolingCapacity;

		// determine condenser water inlet/outlet condition at full capacity
		if ( CondInletMassFlowRate == 0.0 ) {
			OutletWaterTemp = InletWaterTemp;
		} else {
			OutletWaterTemp = InletWaterTemp + TotalTankHeatingCapacity / ( CpWater * CondInletMassFlowRate );
		}

		WaterOutletNode.Temp = OutletWaterTemp;

		WaterOutletNode.MassFlowRate = WaterInletNode.MassFlowRate;

		// send heating capacity and COP to water heater module for standards rating calculation
		// total heating capacity including condenser pump
		HPWHHeatingCapacity = TotalTankHeatingCapacity;
		// total heating COP including compressor, fan, and condenser pump
		HPWHHeatingCOP = TankHeatingCOP;

		// send DX coil total cooling capacity to HPWH for reporting
		DXCoilTotalCapacity = EvapCoolingCapacity;

		Coil.TotalHeatingEnergyRate = TotalTankHeatingCapacity * PartLoadRatio;

		// calculate total compressor plus condenser pump power, fan power reported in fan module
		Coil.ElecWaterHeatingPower = ( CompressorPower + Coil.HPWHCondPumpElecNomPower ) * HPRTF;

	}

	void
	CalcDoe2DXCoil(
		int const DXCoilNum, // the number of the DX coil to be simulated
		int const CompOp, // compressor operation; 1=on, 0=off
		bool const FirstHVACIteration, // true if this is the first iteration of HVAC
		Real64 const PartLoadRatio, // sensible cooling load / full load sensible cooling capacity
		int const FanOpMode, // Allows parent object to control fan operation
		Optional_int_const PerfMode, // Performance mode for MultiMode DX coil; Always 1 for other coil types
		Optional< Real64 const > OnOffAirFlowRatio, // ratio of compressor on airflow to compressor off airflow
		Optional< Real64 const > CoolingHeatingPLR // used for cycling fan RH control
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   May 2000
		//       MODIFIED       Shirey, Feb/October 2001, Feb/Mar 2004
		//                      Feb 2005 M. J. Witte, GARD Analytics, Inc.
		//                        Add new coil type COIL:DX:MultiMode:CoolingEmpirical:
		//                      April 2010 Chandan Sharma, FSEC, Added basin heater
		//       RE-ENGINEERED  Don Shirey, Aug/Sept 2000

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the air-side performance and electrical energy use of a direct-
		// expansion, air-cooled cooling unit.

		// METHODOLOGY EMPLOYED:
		// This routine simulates the performance of air-cooled DX cooling equipment.
		// The routine requires the user to enter the total cooling capacity, sensible heat ratio,
		// and COP for the unit at ARI 210/240 rating conditions (26.67C [80F] dry-bulb, 19.44C [67F]
		// wet-bulb air entering the cooling coil, 35C [95F] dry-bulb air entering the outdoor
		// condenser. Since different manufacturer's rate their equipment at different air flow rates,
		// the supply air flow rate corresponding to the rated capacities and rated COP must also be
		// entered (should be between 300 cfm/ton and 450 cfm/ton). The rated information entered by
		// the user should NOT include the thermal or electrical impacts of the supply air fan, as
		// this is addressed by another module.

		// With the rated performance data entered by the user, the model employs some of the
		// DOE-2.1E curve fits to adjust the capacity and efficiency of the unit as a function
		// of entering air temperatures and supply air flow rate (actual vs rated flow). The model
		// does NOT employ the exact same methodology to calculate performance as DOE-2, although
		// some of the DOE-2 curve fits are employed by this model.

		// The model checks for coil dryout conditions, and adjusts the calculated performance
		// appropriately.

		// REFERENCES:
		// ASHRAE HVAC 2 Toolkit page 4-81.
		// Henderson, H.I. Jr., K. Rengarajan and D.B. Shirey, III. 1992.The impact of comfort
		// control on air conditioner energy use in humid climates. ASHRAE Transactions 98(2):
		// 104-113.
		// Henderson, H.I. Jr., Danny Parker and Y.J. Huang. 2000.Improving DOE-2's RESYS routine:
		// User Defined Functions to Provide More Accurate Part Load Energy Use and Humidity
		// Predictions. Proceedings of ACEEE Conference.

		// Using/Aliasing
		using CurveManager::CurveValue;
		using DataGlobals::CurrentTime;
		using DataHVACGlobals::HPWHCrankcaseDBTemp;
		using DataHVACGlobals::TimeStepSys;
		using DataHVACGlobals::SysTimeElapsed;
		using General::TrimSigDigits;
		using General::RoundSigDigits;
		using General::CreateSysTimeIntervalString;
		using DataWater::WaterStorage;
		using DataHeatBalance::Zone;
		using DataHeatBalFanSys::ZoneAirHumRat;
		using DataHeatBalFanSys::ZT;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcDoe2DXCoil: " );
		static std::string const calcDoe2DXCoil( "CalcDoe2DXCoil" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 AirMassFlow; // dry air mass flow rate through coil [kg/s] (adjusted for bypass if any)
		Real64 AirMassFlowRatio; // Ratio of actual air mass flow to rated air mass flow (adjusted for bypass if any)
		Real64 AirVolumeFlowRate; // Air volume flow rate across the cooling coil [m3/s] (adjusted for bypass if any)
		// (average flow if cycling fan, full flow if constant fan)
		Real64 VolFlowperRatedTotCap; // Air volume flow rate divided by rated total cooling capacity [m3/s-W] (adjusted for bypass)
		Real64 BypassFlowFraction; // Fraction of total flow which is bypassed around the cooling coil
		Real64 TotCap; // gross total cooling capacity at off-rated conditions [W]
		Real64 TotCapTempModFac; // Total capacity modifier (function of entering wetbulb, outside drybulb)
		Real64 TotCapFlowModFac; // Total capacity modifier (function of actual supply air flow vs rated flow)
		Real64 InletAirWetBulbC; // wetbulb temperature of inlet air [C]
		Real64 InletAirDryBulbTemp; // inlet air dry bulb temperature [C]
		Real64 InletAirEnthalpy; // inlet air enthalpy [J/kg]
		Real64 InletAirHumRat; // inlet air humidity ratio [kg/kg]
		Real64 InletAirHumRatTemp; // inlet air humidity ratio used in ADP/BF loop [kg/kg]
		//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
		//REAL(r64) :: InletAirPressure      ! inlet air pressure [Pa]
		Real64 RatedCBF; // coil bypass factor at rated conditions
		Real64 SHR; // Sensible Heat Ratio (sensible/total) of the cooling coil
		Real64 CBF; // coil bypass factor at off rated conditions
		Real64 A0; // NTU * air mass flow rate, used in CBF calculation
		Real64 hDelta; // Change in air enthalpy across the cooling coil [J/kg]
		Real64 hADP; // Apparatus dew point enthalpy [J/kg]
		Real64 hTinwADP; // Enthalpy at inlet dry-bulb and wADP [J/kg]
		Real64 hTinwout; // Enthalpy at inlet dry-bulb and outlet humidity ratio [J/kg]
		Real64 tADP; // Apparatus dew point temperature [C]
		Real64 wADP; // Apparatus dew point humidity ratio [kg/kg]
		Real64 FullLoadOutAirEnth; // outlet full load enthalpy [J/kg]
		Real64 FullLoadOutAirHumRat; // outlet humidity ratio at full load
		Real64 FullLoadOutAirTemp; // outlet air temperature at full load [C]
		Real64 EIRTempModFac; // EIR modifier (function of entering wetbulb, outside drybulb)
		Real64 EIRFlowModFac; // EIR modifier (function of actual supply air flow vs rated flow)
		Real64 EIR; // EIR at part load and off rated conditions
		Real64 PLF; // Part load factor, accounts for thermal lag at compressor startup, used in power calculation
		Real64 QLatActual; // operating latent capacity of DX coil
		Real64 QLatRated; // Rated latent capacity of DX coil
		Real64 SHRUnadjusted; // SHR prior to latent degradation effective SHR calculation
		int Counter; // Counter for dry evaporator iterations
		int MaxIter; // Maximum number of iterations for dry evaporator calculations
		Real64 RF; // Relaxation factor for dry evaporator iterations
		Real64 Tolerance; // Error tolerance for dry evaporator iterations
		Real64 werror; // Deviation of humidity ratio in dry evaporator iteration loop
		Real64 CondInletTemp; // Condenser inlet temperature (C). Outdoor dry-bulb temp for air-cooled condenser.
		// Outdoor Wetbulb +(1 - effectiveness)*(outdoor drybulb - outdoor wetbulb) for evap condenser.
		Real64 CondInletHumRat; // Condenser inlet humidity ratio (kg/kg). Zero for air-cooled condenser.
		// For evap condenser, its the humidity ratio of the air leaving the evap cooling pads.
		Real64 CondAirMassFlow; // Condenser air mass flow rate [kg/s]
		Real64 RhoAir; // Density of air [kg/m3]
		Real64 RhoWater; // Density of water [kg/m3]
		Real64 CrankcaseHeatingPower; // power due to crankcase heater
		static Real64 CompAmbTemp( 0.0 ); // Ambient temperature at compressor
		Real64 AirFlowRatio; // ratio of compressor on airflow to average timestep airflow
		// used when constant fan mode yields different air flow rates when compressor is ON and OFF
		// (e.g. Packaged Terminal Heat Pump)
		Real64 OutdoorDryBulb; // Outdoor dry-bulb temperature at condenser (C)
		Real64 OutdoorWetBulb; // Outdoor wet-bulb temperature at condenser (C)
		Real64 OutdoorHumRat; // Outdoor humidity ratio at condenser (kg/kg)
		Real64 OutdoorPressure; // Outdoor barometric pressure at condenser (Pa)

		static Real64 CurrentEndTime( 0.0 ); // end time of time step for current simulation time step
		static Real64 MinAirHumRat( 0.0 ); // minimum of the inlet air humidity ratio and the outlet air humidity ratio
		//CHARACTER(len=6) :: OutputChar = ' '     ! character string for warning messages
		//INTEGER,SAVE     :: ErrCount3=0    ! Counter used to minimize the occurrence of output warnings
		//INTEGER,SAVE     :: ErrCount4=0    ! Counter used to minimize the occurrence of output warnings
		//CHARACTER(len=6) :: CharPLR        ! used in warning messages
		//CHARACTER(len=6) :: CharPLF        ! used in warning messages
		int Mode; // Performance mode for Multimode DX coil; Always 1 for other coil types
		//CHARACTER(len=MaxNameLength) :: MinVol      ! character string used for error messages
		//CHARACTER(len=MaxNameLength) :: MaxVol      ! character string used for error messages
		//CHARACTER(len=MaxNameLength) :: VolFlowChar ! character string used for error messages
		Real64 OutletAirTemp; // Supply air temperature (average value if constant fan, full output if cycling fan)
		Real64 OutletAirHumRat; // Supply air humidity ratio (average value if constant fan, full output if cycling fan)
		Real64 OutletAirEnthalpy; // Supply air enthalpy (average value if constant fan, full output if cycling fan)
		Real64 ADiff; // Used for exponential
		Real64 DXcoolToHeatPLRRatio; // ratio of cooling PLR to heating PLR, used for cycling fan RH control
		Real64 HeatRTF; // heating coil part-load ratio, used for cycling fan RH control
		Real64 HeatingCoilPLF; // heating coil PLF (function of PLR), used for cycling fan RH control

		// If Performance mode not present, then set to 1.  Used only by Multimode/Multispeed DX coil (otherwise mode = 1)
		if ( present( PerfMode ) ) {
			Mode = PerfMode;
		} else {
			Mode = 1;
		}

		// If AirFlowRatio not present, then set to 1. Used only by DX coils with different air flow
		// during cooling and when no cooling is required (constant fan, fan speed changes)
		if ( present( OnOffAirFlowRatio ) ) {
			AirFlowRatio = OnOffAirFlowRatio;
		} else {
			AirFlowRatio = 1.0;
		}

		// If CoolingHeatingPLR not present, then set to 1. Used for cycling fan systems where
		// heating PLR is greater than cooling PLR, otherwise CoolingHeatingPLR = 1.
		if ( present( CoolingHeatingPLR ) ) {
			DXcoolToHeatPLRRatio = CoolingHeatingPLR;
		} else {
			DXcoolToHeatPLRRatio = 1.0;
		}

		MaxIter = 30;
		RF = 0.4;
		Counter = 0;
		Tolerance = 0.01;
		CondInletTemp = 0.0;
		CondInletHumRat = 0.0;
		BypassFlowFraction = DXCoil( DXCoilNum ).BypassedFlowFrac( Mode );
		AirMassFlow = DXCoil( DXCoilNum ).InletAirMassFlowRate * ( 1.0 - BypassFlowFraction );
		InletAirDryBulbTemp = DXCoil( DXCoilNum ).InletAirTemp;
		InletAirEnthalpy = DXCoil( DXCoilNum ).InletAirEnthalpy;
		InletAirHumRat = DXCoil( DXCoilNum ).InletAirHumRat;
		//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
		//InletAirPressure    = DXCoil(DXCoilNum)%InletAirPressure
		HeatReclaimDXCoil( DXCoilNum ).AvailCapacity = 0.0;
		DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction = 0.0;
		DXCoil( DXCoilNum ).PartLoadRatio = 0.0;
		DXCoil( DXCoilNum ).BasinHeaterPower = 0.0;

		if ( DXCoil( DXCoilNum ).CondenserType( Mode ) != WaterHeater ) {
			if ( DXCoil( DXCoilNum ).CondenserInletNodeNum( Mode ) != 0 ) {
				OutdoorPressure = Node( DXCoil( DXCoilNum ).CondenserInletNodeNum( Mode ) ).Press;
				// If node is not connected to anything, pressure = default, use weather data
				if ( OutdoorPressure == DefaultNodeValues.Press ) {
					OutdoorDryBulb = OutDryBulbTemp;
					OutdoorHumRat = OutHumRat;
					OutdoorPressure = OutBaroPress;
					OutdoorWetBulb = OutWetBulbTemp;
				} else {
					OutdoorDryBulb = Node( DXCoil( DXCoilNum ).CondenserInletNodeNum( Mode ) ).Temp;
					OutdoorHumRat = Node( DXCoil( DXCoilNum ).CondenserInletNodeNum( Mode ) ).HumRat;
					// this should use Node%WetBulbTemp or a PSYC function, not OAWB
					OutdoorWetBulb = Node( DXCoil( DXCoilNum ).CondenserInletNodeNum( Mode ) ).OutAirWetBulb;
				}
			} else {
				OutdoorDryBulb = OutDryBulbTemp;
				OutdoorHumRat = OutHumRat;
				OutdoorPressure = OutBaroPress;
				OutdoorWetBulb = OutWetBulbTemp;
			}
			if ( DXCoil( DXCoilNum ).IsSecondaryDXCoilInZone ) {
				OutdoorDryBulb = ZT( DXCoil( DXCoilNum ).SecZonePtr );
				OutdoorHumRat = ZoneAirHumRat( DXCoil( DXCoilNum ).SecZonePtr );
				OutdoorWetBulb = DXCoil( DXCoilNum ).EvapInletWetBulb;
				OutdoorPressure = OutBaroPress;
			}
		} else {
			if ( DXCoil( DXCoilNum ).CondenserInletNodeNum( Mode ) != 0 ) {
				OutdoorPressure = Node( DXCoil( DXCoilNum ).CondenserInletNodeNum( Mode ) ).Press;
				// If node is not connected to anything, pressure = default, use weather data
				if ( OutdoorPressure == DefaultNodeValues.Press ) OutdoorPressure = OutBaroPress; // node not connected
			} else {
				OutdoorPressure = OutBaroPress;
			}
		}

		if ( DXCoil( DXCoilNum ).CondenserType( Mode ) == AirCooled ) {
			CondInletTemp = OutdoorDryBulb; // Outdoor dry-bulb temp
			CompAmbTemp = OutdoorDryBulb;
			if ( DXCoil( DXCoilNum ).IsSecondaryDXCoilInZone ) {
				CondInletTemp = ZT( DXCoil( DXCoilNum ).SecZonePtr );
				CompAmbTemp = CondInletTemp;
				OutdoorDryBulb = CondInletTemp;
				OutdoorHumRat = ZoneAirHumRat( DXCoil( DXCoilNum ).SecZonePtr );
				OutdoorWetBulb = DXCoil( DXCoilNum ).EvapInletWetBulb;
				OutdoorPressure = OutBaroPress;
			}
		} else if ( DXCoil( DXCoilNum ).CondenserType( Mode ) == EvapCooled ) {
			RhoAir = PsyRhoAirFnPbTdbW( OutdoorPressure, OutdoorDryBulb, OutdoorHumRat );
			CondAirMassFlow = RhoAir * DXCoil( DXCoilNum ).EvapCondAirFlow( Mode );
			// (Outdoor wet-bulb temp from DataEnvironment) + (1.0-EvapCondEffectiveness) * (drybulb - wetbulb)
			CondInletTemp = OutdoorWetBulb + ( OutdoorDryBulb - OutdoorWetBulb ) * ( 1.0 - DXCoil( DXCoilNum ).EvapCondEffect( Mode ) );
			CondInletHumRat = PsyWFnTdbTwbPb( CondInletTemp, OutdoorWetBulb, OutdoorPressure );
			CompAmbTemp = CondInletTemp;
		} else if ( DXCoil( DXCoilNum ).CondenserType( Mode ) == WaterHeater ) {
			CompAmbTemp = HPWHCrankcaseDBTemp; // Temperature at HP water heater compressor
		}

		// Initialize crankcase heater, operates below OAT defined in input deck for HP DX cooling coil
		// If used in a heat pump, the value of MaxOAT in the heating coil overrides that in the cooling coil (in GetInput)
		if ( CompAmbTemp < DXCoil( DXCoilNum ).MaxOATCrankcaseHeater ) {
			CrankcaseHeatingPower = DXCoil( DXCoilNum ).CrankcaseHeaterCapacity;
		} else {
			CrankcaseHeatingPower = 0.0;
		}

		// calculate end time of current time step to determine if error messages should be printed
		CurrentEndTime = CurrentTime + SysTimeElapsed;

		//   Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
		//   Wait for next time step to print warnings. If simulation iterates, print out
		//   the warning for the last iteration only. Must wait for next time step to accomplish this.
		//   If a warning occurs and the simulation down shifts, the warning is not valid.
		if ( DXCoil( DXCoilNum ).PrintLowAmbMessage ) { // .AND. &
			if ( CurrentEndTime > DXCoil( DXCoilNum ).CurrentEndTimeLast && TimeStepSys >= DXCoil( DXCoilNum ).TimeStepSysLast ) {
				if ( DXCoil( DXCoilNum ).LowAmbErrIndex == 0 ) {
					ShowWarningMessage( RoutineName + DXCoil( DXCoilNum ).LowAmbBuffer1 );
					ShowContinueError( DXCoil( DXCoilNum ).LowAmbBuffer2 );
					ShowContinueError( "... Operation at low ambient temperatures may require special performance curves." );
				}
				if ( DXCoil( DXCoilNum ).CondenserType( Mode ) == AirCooled ) {
					ShowRecurringWarningErrorAtEnd( RoutineName + DXCoil( DXCoilNum ).DXCoilType + "=\"" + DXCoil( DXCoilNum ).Name + "\" - Low condenser dry-bulb temperature error continues...", DXCoil( DXCoilNum ).LowAmbErrIndex, DXCoil( DXCoilNum ).LowTempLast, DXCoil( DXCoilNum ).LowTempLast, _, "[C]", "[C]" );
				} else {
					ShowRecurringWarningErrorAtEnd( RoutineName + DXCoil( DXCoilNum ).DXCoilType + "=\"" + DXCoil( DXCoilNum ).Name + "\" - Low condenser wet-bulb temperature error continues...", DXCoil( DXCoilNum ).LowAmbErrIndex, DXCoil( DXCoilNum ).LowTempLast, DXCoil( DXCoilNum ).LowTempLast, _, "[C]", "[C]" );
				}
			}
		}

		if ( DXCoil( DXCoilNum ).PrintLowOutTempMessage ) {
			if ( CurrentEndTime > DXCoil( DXCoilNum ).CurrentEndTimeLast && TimeStepSys >= DXCoil( DXCoilNum ).TimeStepSysLast ) {
				if ( DXCoil( DXCoilNum ).LowOutletTempIndex == 0 ) {
					ShowWarningMessage( RoutineName + DXCoil( DXCoilNum ).LowOutTempBuffer1 );
					ShowContinueError( DXCoil( DXCoilNum ).LowOutTempBuffer2 );
					ShowContinueError( "... Possible reasons for low outlet air dry-bulb temperatures are: This DX coil" );
					ShowContinueError( "   1) may have a low inlet air dry-bulb temperature. Inlet air temperature = " + TrimSigDigits( DXCoil( DXCoilNum ).FullLoadInletAirTempLast, 3 ) + " C." );
					ShowContinueError( "   2) may have a low air flow rate per watt of cooling capacity. Check inputs." );
					ShowContinueError( "   3) is used as part of a HX assisted cooling coil which uses a high sensible effectiveness. Check inputs." );
				}
				ShowRecurringWarningErrorAtEnd( RoutineName + DXCoil( DXCoilNum ).DXCoilType + "=\"" + DXCoil( DXCoilNum ).Name + "\" - Full load outlet temperature indicates a possibility of frost/freeze error continues. Outlet air temperature statistics follow:", DXCoil( DXCoilNum ).LowOutletTempIndex, DXCoil( DXCoilNum ).FullLoadOutAirTempLast, DXCoil( DXCoilNum ).FullLoadOutAirTempLast );
			}
		}

		// save last system time step and last end time of current time step (used to determine if warning is valid)
		DXCoil( DXCoilNum ).TimeStepSysLast = TimeStepSys;
		DXCoil( DXCoilNum ).CurrentEndTimeLast = CurrentEndTime;
		DXCoil( DXCoilNum ).PrintLowAmbMessage = false;
		DXCoil( DXCoilNum ).PrintLowOutTempMessage = false;

		if ( ( AirMassFlow > 0.0 ) && ( GetCurrentScheduleValue( DXCoil( DXCoilNum ).SchedPtr ) > 0.0 || DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_HeatPumpWaterHeaterPumped || DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_HeatPumpWaterHeaterWrapped ) && ( PartLoadRatio > 0.0 ) && ( CompOp == On ) ) { // for cycling fan, reset mass flow to full on rate
			if ( FanOpMode == CycFanCycCoil ) {
				AirMassFlow /= ( PartLoadRatio / DXcoolToHeatPLRRatio );
			} else if ( FanOpMode == ContFanCycCoil && DXCoil( DXCoilNum ).DXCoilType_Num != CoilDX_CoolingTwoSpeed ) {
				AirMassFlow *= AirFlowRatio;
			} else {
				AirMassFlow = DXCoil( DXCoilNum ).RatedAirMassFlowRate( Mode );
			}

			// Check for valid air volume flow per rated total cooling capacity (200 - 500 cfm/ton)

			// for some reason there are diff's when using coil inlet air pressure
			// these lines (more to follow) are commented out for the time being

			InletAirWetBulbC = PsyTwbFnTdbWPb( InletAirDryBulbTemp, InletAirHumRat, OutdoorPressure );
			AirVolumeFlowRate = AirMassFlow / PsyRhoAirFnPbTdbW( OutdoorPressure, InletAirDryBulbTemp, InletAirHumRat );
			//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
			//  InletAirWetBulbC = PsyTwbFnTdbWPb(InletAirDryBulbTemp,InletAirHumRat,InletAirPressure)
			//  AirVolumeFlowRate = AirMassFlow/ PsyRhoAirFnPbTdbW(InletAirPressure,InletAirDryBulbTemp, InletAirHumRat)
			if ( DXCoil( DXCoilNum ).RatedTotCap( Mode ) <= 0.0 ) {
				ShowFatalError( RoutineName + DXCoil( DXCoilNum ).DXCoilType + "=\"" + DXCoil( DXCoilNum ).Name + "\" - Rated total cooling capacity is zero or less." );
			}
			if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_HeatPumpWaterHeaterPumped || DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_HeatPumpWaterHeaterWrapped ) {
				VolFlowperRatedTotCap = AirVolumeFlowRate / DXCoil( DXCoilNum ).RatedTotCap2;
			} else {
				VolFlowperRatedTotCap = AirVolumeFlowRate / DXCoil( DXCoilNum ).RatedTotCap( Mode );
			}
			if ( ! FirstHVACIteration && ! WarmupFlag && DXCoil( DXCoilNum ).DXCoilType_Num != CoilDX_HeatPumpWaterHeaterPumped && DXCoil( DXCoilNum ).DXCoilType_Num != CoilDX_HeatPumpWaterHeaterWrapped && ( ( VolFlowperRatedTotCap < MinOperVolFlowPerRatedTotCap( DXCT ) ) || ( VolFlowperRatedTotCap > MaxCoolVolFlowPerRatedTotCap( DXCT ) ) ) ) {
				if ( DXCoil( DXCoilNum ).ErrIndex1 == 0 ) {
					ShowWarningMessage( RoutineName + DXCoil( DXCoilNum ).DXCoilType + "=\"" + DXCoil( DXCoilNum ).Name + "\" - Air volume flow rate per watt of rated total cooling capacity is out of range at " + RoundSigDigits( VolFlowperRatedTotCap, 3 ) + " m3/s/W." );
					ShowContinueErrorTimeStamp( "" );
					ShowContinueError( "Expected range for VolumeFlowPerRatedTotalCapacity=[" + RoundSigDigits( MinOperVolFlowPerRatedTotCap( DXCT ), 3 ) + "--" + RoundSigDigits( MaxCoolVolFlowPerRatedTotCap( DXCT ), 3 ) + ']' );
					ShowContinueError( "Possible causes include inconsistent air flow rates in system components," );
					ShowContinueError( "or variable air volume [VAV] system using incorrect coil type." );
				}
				ShowRecurringWarningErrorAtEnd( RoutineName + DXCoil( DXCoilNum ).DXCoilType + "=\"" + DXCoil( DXCoilNum ).Name + "\" - Air volume flow rate per watt of rated total cooling capacity is out of range error continues...", DXCoil( DXCoilNum ).ErrIndex1, VolFlowperRatedTotCap, VolFlowperRatedTotCap );
			} else if ( ! WarmupFlag && DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_HeatPumpWaterHeaterPumped && DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_HeatPumpWaterHeaterWrapped && ( ( VolFlowperRatedTotCap < MinOperVolFlowPerRatedTotCap( DXCT ) ) || ( VolFlowperRatedTotCap > MaxHeatVolFlowPerRatedTotCap( DXCT ) ) ) ) {
				if ( DXCoil( DXCoilNum ).ErrIndex1 == 0 ) {
					ShowWarningMessage( RoutineName + DXCoil( DXCoilNum ).DXCoilType + "=\"" + DXCoil( DXCoilNum ).Name + "\" - Air volume flow rate per watt of rated total water heating capacity is out of range at " + RoundSigDigits( VolFlowperRatedTotCap, 2 ) + " m3/s/W." );
					ShowContinueErrorTimeStamp( "" );
					ShowContinueError( "Expected range for VolumeFlowPerRatedTotalCapacity=[" + RoundSigDigits( MinOperVolFlowPerRatedTotCap( DXCT ), 3 ) + "--" + RoundSigDigits( MaxHeatVolFlowPerRatedTotCap( DXCT ), 3 ) + ']' );
					ShowContinueError( "Possible causes may be that the parent object is calling for an actual supply air flow rate that is much higher or lower than the DX coil rated supply air flow rate." );
				}
				ShowRecurringWarningErrorAtEnd( RoutineName + DXCoil( DXCoilNum ).DXCoilType + "=\"" + DXCoil( DXCoilNum ).Name + "\" - Air volume flow rate per watt of rated total water heating capacity is out of range error continues...", DXCoil( DXCoilNum ).ErrIndex1, VolFlowperRatedTotCap, VolFlowperRatedTotCap );
			}
			//    Adjust coil bypass factor for actual air flow rate. Use relation CBF = exp(-NTU) where
			//    NTU = A0/(m*cp). Relationship models the cooling coil as a heat exchanger with Cmin/Cmax = 0.

			RatedCBF = DXCoil( DXCoilNum ).RatedCBF( Mode );
			if ( RatedCBF > 0.0 ) {
				A0 = -std::log( RatedCBF ) * DXCoil( DXCoilNum ).RatedAirMassFlowRate( Mode );
			} else {
				A0 = 0.0;
			}
			ADiff = -A0 / AirMassFlow;
			if ( ADiff >= EXP_LowerLimit ) {
				CBF = std::exp( ADiff );
			} else {
				CBF = 0.0;
			}

			//   check boundary for low ambient temperature and post warnings to individual DX coil buffers to print at end of time step
			if ( DXCoil( DXCoilNum ).CondenserType( Mode ) == AirCooled ) {
				if ( OutdoorDryBulb < 0.0 && ! WarmupFlag ) { //Same threshold as for air-cooled electric chiller
					DXCoil( DXCoilNum ).PrintLowAmbMessage = true;
					DXCoil( DXCoilNum ).LowTempLast = OutdoorDryBulb;
					if ( DXCoil( DXCoilNum ).LowAmbErrIndex == 0 ) {
						DXCoil( DXCoilNum ).LowAmbBuffer1 = DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + "\" - Air-cooled condenser inlet dry-bulb temperature below 0 C. Outdoor dry-bulb temperature = " + RoundSigDigits( OutdoorDryBulb, 2 );
						DXCoil( DXCoilNum ).LowAmbBuffer2 = " ... Occurrence info = " + EnvironmentName + ", " + CurMnDy + ' ' + CreateSysTimeIntervalString();
					}
				}
			} else if ( DXCoil( DXCoilNum ).CondenserType( Mode ) == EvapCooled ) {
				if ( OutdoorWetBulb < 10.0 && ! WarmupFlag ) { //Same threshold as for evap-cooled electric chiller
					DXCoil( DXCoilNum ).PrintLowAmbMessage = true;
					DXCoil( DXCoilNum ).LowTempLast = OutdoorWetBulb;
					if ( DXCoil( DXCoilNum ).LowAmbErrIndex == 0 ) {
						DXCoil( DXCoilNum ).LowAmbBuffer1 = DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + "\" - Evap-cooled condenser inlet wet-bulb temperature below 10 C. Outdoor wet-bulb temperature = " + RoundSigDigits( OutdoorWetBulb, 2 );
						DXCoil( DXCoilNum ).LowAmbBuffer2 = " ... Occurrence info = " + EnvironmentName + ", " + CurMnDy + ' ' + CreateSysTimeIntervalString();
					}
				}
			}

			//  Get total capacity modifying factor (function of temperature) for off-rated conditions
			//  InletAirHumRat may be modified in this ADP/BF loop, use temporary varible for calculations
			InletAirHumRatTemp = InletAirHumRat;
			AirMassFlowRatio = AirMassFlow / DXCoil( DXCoilNum ).RatedAirMassFlowRate( Mode );
			while ( true ) {
				if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_HeatPumpWaterHeaterPumped || DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_HeatPumpWaterHeaterWrapped ) {
					// Coil:DX:HeatPumpWaterHeater does not have total cooling capacity as a function of temp or flow curve
					TotCapTempModFac = 1.0;
					TotCapFlowModFac = 1.0;
				} else {
					if ( DXCoil( DXCoilNum ).TotCapTempModFacCurveType( Mode ) == BiQuadratic ) {
						TotCapTempModFac = CurveValue( DXCoil( DXCoilNum ).CCapFTemp( Mode ), InletAirWetBulbC, CondInletTemp );
					} else {
						TotCapTempModFac = CurveValue( DXCoil( DXCoilNum ).CCapFTemp( Mode ), CondInletTemp );
					}

					//    Warn user if curve output goes negative
					if ( TotCapTempModFac < 0.0 ) {
						if ( DXCoil( DXCoilNum ).CCapFTempErrorIndex == 0 ) {
							ShowWarningMessage( RoutineName + DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + "\":" );
							ShowContinueError( " Total Cooling Capacity Modifier curve (function of temperature) output is negative (" + TrimSigDigits( TotCapTempModFac, 3 ) + ")." );
							if ( DXCoil( DXCoilNum ).TotCapTempModFacCurveType( Mode ) == BiQuadratic ) {
								ShowContinueError( " Negative value occurs using a condenser inlet air temperature of " + TrimSigDigits( CondInletTemp, 1 ) + " and an inlet air wet-bulb temperature of " + TrimSigDigits( InletAirWetBulbC, 1 ) + '.' );
							} else {
								ShowContinueError( " Negative value occurs using a condenser inlet air temperature of " + TrimSigDigits( CondInletTemp, 1 ) + '.' );
							}
							if ( Mode > 1 ) {
								ShowContinueError( " Negative output results from stage " + TrimSigDigits( Mode ) + " compressor operation." );
							}
							ShowContinueErrorTimeStamp( " Resetting curve output to zero and continuing simulation." );
						}
						ShowRecurringWarningErrorAtEnd( RoutineName + DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + "\": Total Cooling Capacity Modifier curve (function of temperature) output is negative warning continues...", DXCoil( DXCoilNum ).CCapFTempErrorIndex, TotCapTempModFac, TotCapTempModFac );
						TotCapTempModFac = 0.0;
					}

					//    Get total capacity modifying factor (function of mass flow) for off-rated conditions
					TotCapFlowModFac = CurveValue( DXCoil( DXCoilNum ).CCapFFlow( Mode ), AirMassFlowRatio );
					//    Warn user if curve output goes negative
					if ( TotCapFlowModFac < 0.0 ) {
						if ( DXCoil( DXCoilNum ).CCapFFlowErrorIndex == 0 ) {
							ShowWarningMessage( RoutineName + DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + "\":" );
							ShowContinueError( " Total Cooling Capacity Modifier curve (function of flow fraction) output is negative (" + TrimSigDigits( TotCapFlowModFac, 3 ) + ")." );
							ShowContinueError( " Negative value occurs using an air flow fraction of " + TrimSigDigits( AirMassFlowRatio, 3 ) + '.' );
							ShowContinueErrorTimeStamp( " Resetting curve output to zero and continuing simulation." );
							if ( Mode > 1 ) {
								ShowContinueError( " Negative output results from stage " + TrimSigDigits( Mode ) + " compressor operation." );
							}
						}
						ShowRecurringWarningErrorAtEnd( RoutineName + DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + "\": Total Cooling Capacity Modifier curve (function of flow fraction) output is negative warning continues...", DXCoil( DXCoilNum ).CCapFFlowErrorIndex, TotCapFlowModFac, TotCapFlowModFac );
						TotCapFlowModFac = 0.0;
					}
				}
				TotCap = DXCoil( DXCoilNum ).RatedTotCap( Mode ) * TotCapFlowModFac * TotCapTempModFac;
				// if user specified SHR modifier curves are available calculate the SHR as follows:
				if ( DXCoil( DXCoilNum ).UserSHRCurveExists ) {
					SHR = CalcSHRUserDefinedCurves( InletAirDryBulbTemp, InletAirWetBulbC, AirMassFlowRatio, DXCoil( DXCoilNum ).SHRFTemp( Mode ), DXCoil( DXCoilNum ).SHRFFlow( Mode ), DXCoil( DXCoilNum ).RatedSHR( Mode ) );
					hDelta = TotCap / AirMassFlow;
					break;
				} else {
					// Calculate apparatus dew point conditions using TotCap and CBF
					hDelta = TotCap / AirMassFlow;
					hADP = InletAirEnthalpy - hDelta / ( 1.0 - CBF );
					tADP = PsyTsatFnHPb( hADP, OutdoorPressure, calcDoe2DXCoil );
					//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
					//  tADP = PsyTsatFnHPb(hADP,InletAirPressure)
					wADP = PsyWFnTdbH( tADP, hADP, calcDoe2DXCoil );
					hTinwADP = PsyHFnTdbW( InletAirDryBulbTemp, wADP );
					if ( ( InletAirEnthalpy - hADP ) > 1.e-10 ) {
						SHR = min( ( hTinwADP - hADP ) / ( InletAirEnthalpy - hADP ), 1.0 );
					} else {
						SHR = 1.0;
					}
					// Check for dry evaporator conditions (win < wadp)
					if ( wADP > InletAirHumRatTemp || ( Counter >= 1 && Counter < MaxIter ) ) {
						if ( InletAirHumRatTemp == 0.0 ) InletAirHumRatTemp = 0.00001;
						werror = ( InletAirHumRatTemp - wADP ) / InletAirHumRatTemp;
						// Increase InletAirHumRatTemp at constant InletAirTemp to find coil dry-out point. Then use the
						// capacity at the dry-out point to determine exiting conditions from coil. This is required
						// since the TotCapTempModFac doesn't work properly with dry-coil conditions.
						InletAirHumRatTemp = RF * wADP + ( 1.0 - RF ) * InletAirHumRatTemp;
						InletAirWetBulbC = PsyTwbFnTdbWPb( InletAirDryBulbTemp, InletAirHumRatTemp, OutdoorPressure );
						//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
						//  InletAirWetBulbC = PsyTwbFnTdbWPb(InletAirDryBulbTemp,InletAirHumRatTemp,InletAirPressure)
						++Counter;
						if ( std::abs( werror ) > Tolerance ) continue; // Recalculate with modified inlet conditions
						break;
					} else {
						break;
					}
				}
			} // end of DO iteration loop

			if ( DXCoil( DXCoilNum ).PLFFPLR( Mode ) > 0 ) {
				PLF = CurveValue( DXCoil( DXCoilNum ).PLFFPLR( Mode ), PartLoadRatio ); // Calculate part-load factor
			} else {
				PLF = 1.0;
			}

			if ( PLF < 0.7 ) {
				if ( DXCoil( DXCoilNum ).ErrIndex2 == 0 ) {
					if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_HeatPumpWaterHeaterPumped || DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_HeatPumpWaterHeaterWrapped ) {
						ShowWarningMessage( RoutineName + DXCoil( DXCoilNum ).DXCoilType + "=\"" + DXCoil( DXCoilNum ).Name + "\", PLF curve value" );
						ShowContinueError( "The PLF curve value = " + TrimSigDigits( PLF, 3 ) + " for part-load ratio = " + TrimSigDigits( PartLoadRatio, 3 ) );
						ShowContinueErrorTimeStamp( "PLF curve values must be >= 0.7. PLF has been reset to 0.7 and simulation is continuing." );
						ShowContinueError( "Check the IO reference manual for PLF curve guidance [" + DXCoil( DXCoilNum ).DXCoilType + "]." );
					} else {
						ShowWarningMessage( RoutineName + DXCoil( DXCoilNum ).DXCoilType + "=\"" + DXCoil( DXCoilNum ).Name + "\", PLF curve value" );
						ShowContinueError( "The PLF curve value = " + TrimSigDigits( PLF, 3 ) + " for part-load ratio = " + TrimSigDigits( PartLoadRatio, 3 ) );
						ShowContinueErrorTimeStamp( "PLF curve values must be >= 0.7. PLF has been reset to 0.7 and simulation is continuing." );
						ShowContinueError( "Check the IO reference manual for PLF curve guidance [" + DXCoil( DXCoilNum ).DXCoilType + "]." );
					}
				}
				if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_HeatPumpWaterHeaterPumped || DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_HeatPumpWaterHeaterWrapped ) {
					ShowRecurringWarningErrorAtEnd( DXCoil( DXCoilNum ).Name + ", " + DXCoil( DXCoilNum ).DXCoilType + " PLF curve < 0.7 warning continues...", DXCoil( DXCoilNum ).ErrIndex2, PLF, PLF );
				} else {
					ShowRecurringWarningErrorAtEnd( DXCoil( DXCoilNum ).Name + ", " + DXCoil( DXCoilNum ).DXCoilType + " PLF curve < 0.7 warning continues...", DXCoil( DXCoilNum ).ErrIndex2, PLF, PLF );
				}
				PLF = 0.7;
			}

			DXCoil( DXCoilNum ).PartLoadRatio = PartLoadRatio;
			DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction = PartLoadRatio / PLF;
			if ( DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction > 1.0 && std::abs( DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction - 1.0 ) > 0.001 ) {
				if ( DXCoil( DXCoilNum ).ErrIndex3 == 0 ) {
					if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_HeatPumpWaterHeaterPumped || DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_HeatPumpWaterHeaterWrapped ) {
						ShowWarningMessage( RoutineName + DXCoil( DXCoilNum ).DXCoilType + "=\"" + DXCoil( DXCoilNum ).Name + "\", runtime fraction" );
						ShowWarningMessage( "The runtime fraction exceeded 1.0. [" + RoundSigDigits( DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction, 4 ) + "]." );
						ShowContinueError( "Runtime fraction reset to 1 and the simulation will continue." );
						ShowContinueError( "Check the IO reference manual for PLF curve guidance [" + DXCoil( DXCoilNum ).DXCoilType + "]." );
						ShowContinueErrorTimeStamp( "" );
					} else {
						ShowWarningMessage( RoutineName + DXCoil( DXCoilNum ).DXCoilType + "=\"" + DXCoil( DXCoilNum ).Name + "\", runtime fraction" );
						ShowWarningMessage( "The runtime fraction exceeded 1.0. [" + RoundSigDigits( DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction, 4 ) + "]." );
						ShowContinueError( "Runtime fraction reset to 1 and the simulation will continue." );
						ShowContinueError( "Check the IO reference manual for PLF curve guidance [" + DXCoil( DXCoilNum ).DXCoilType + "]." );
						ShowContinueErrorTimeStamp( "" );
					}
				}
				if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_HeatPumpWaterHeaterPumped || DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_HeatPumpWaterHeaterWrapped ) {
					ShowRecurringWarningErrorAtEnd( DXCoil( DXCoilNum ).Name + ", " + DXCoil( DXCoilNum ).DXCoilType + " runtime fraction > 1.0 warning continues...", DXCoil( DXCoilNum ).ErrIndex3, DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction, DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction );
				} else {
					ShowRecurringWarningErrorAtEnd( DXCoil( DXCoilNum ).Name + ", " + DXCoil( DXCoilNum ).DXCoilType + " runtime fraction > 1.0 warning continues...", DXCoil( DXCoilNum ).ErrIndex3, DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction, DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction );
				}
				DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction = 1.0; // Reset coil runtime fraction to 1.0
			} else if ( DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction > 1.0 ) {
				DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction = 1.0; // Reset coil runtime fraction to 1.0
			}

			// If cycling fan, send coil part-load fraction to on/off fan via HVACDataGlobals
			if ( FanOpMode == CycFanCycCoil ) OnOffFanPartLoadFraction = PLF;

			//  Calculate full load output conditions
			if ( DXCoil( DXCoilNum ).UserSHRCurveExists ) {
				FullLoadOutAirEnth = InletAirEnthalpy - hDelta;
				if ( SHR < 1.0 ) {
					hTinwout = InletAirEnthalpy - ( 1.0 - SHR ) * hDelta;
					FullLoadOutAirHumRat = PsyWFnTdbH( InletAirDryBulbTemp, hTinwout );
					if ( FullLoadOutAirHumRat <= 0.0 ) {
						FullLoadOutAirHumRat = min( DryCoilOutletHumRatioMin, InletAirHumRat );
					}
				} else {
					SHR = 1.0;
					FullLoadOutAirHumRat = InletAirHumRat;
				}
			} else {
				if ( SHR > 1.0 || Counter > 0 ) SHR = 1.0;
				FullLoadOutAirEnth = InletAirEnthalpy - TotCap / AirMassFlow;
				hTinwout = InletAirEnthalpy - ( 1.0 - SHR ) * hDelta;
				if ( SHR < 1.0 ) {
					FullLoadOutAirHumRat = PsyWFnTdbH( InletAirDryBulbTemp, hTinwout );
				} else {
					FullLoadOutAirHumRat = InletAirHumRat;
				}
			}
			FullLoadOutAirTemp = PsyTdbFnHW( FullLoadOutAirEnth, FullLoadOutAirHumRat );

			// Check for saturation error and modify temperature at constant enthalpy
			if ( FullLoadOutAirTemp < PsyTsatFnHPb( FullLoadOutAirEnth, OutdoorPressure ) ) {
				FullLoadOutAirTemp = PsyTsatFnHPb( FullLoadOutAirEnth, OutdoorPressure );
				//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
				//   IF(FullLoadOutAirTemp .LT. PsyTsatFnHPb(FullLoadOutAirEnth,InletAirPressure)) THEN
				//    FullLoadOutAirTemp = PsyTsatFnHPb(FullLoadOutAirEnth,InletAirPressure)
				FullLoadOutAirHumRat = PsyWFnTdbH( FullLoadOutAirTemp, FullLoadOutAirEnth );
			}

			// Store actual outlet conditions when DX coil is ON for use in heat recovery module
			DXCoilFullLoadOutAirTemp( DXCoilNum ) = FullLoadOutAirTemp;
			DXCoilFullLoadOutAirHumRat( DXCoilNum ) = FullLoadOutAirHumRat;

			// Add warning message for cold cooling coil (FullLoadOutAirTemp < 2 C)
			if ( FullLoadOutAirTemp < 2.0 && ! FirstHVACIteration && ! WarmupFlag ) {
				DXCoil( DXCoilNum ).PrintLowOutTempMessage = true;
				DXCoil( DXCoilNum ).FullLoadOutAirTempLast = FullLoadOutAirTemp;
				if ( DXCoil( DXCoilNum ).LowOutletTempIndex == 0 ) {
					DXCoil( DXCoilNum ).FullLoadInletAirTempLast = InletAirDryBulbTemp;
					DXCoil( DXCoilNum ).LowOutTempBuffer1 = DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + "\" - Full load outlet air dry-bulb temperature < 2C. This indicates the possibility of coil frost/freeze. Outlet temperature = " + RoundSigDigits( FullLoadOutAirTemp, 2 ) + " C.";
					DXCoil( DXCoilNum ).LowOutTempBuffer2 = " ...Occurrence info = " + EnvironmentName + ", " + CurMnDy + ' ' + CreateSysTimeIntervalString();
				}
			}

			//  If constant fan with cycling compressor, call function to determine "effective SHR"
			//  which includes the part-load degradation on latent capacity
			if ( FanOpMode == ContFanCycCoil ) {
				QLatRated = DXCoil( DXCoilNum ).RatedTotCap( Mode ) * ( 1.0 - DXCoil( DXCoilNum ).RatedSHR( Mode ) );
				QLatActual = TotCap * ( 1.0 - SHR );
				SHRUnadjusted = SHR;
				SHR = CalcEffectiveSHR( DXCoilNum, SHR, DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction, QLatRated, QLatActual, InletAirDryBulbTemp, InletAirWetBulbC, Mode );
				// For multimode coil, if stage-2 operation (modes 2 or 4), adjust Stage1&2 SHR to account for
				// Stage 1 operating at full load, so there is no degradation for that portion
				// Use the stage 1 bypass fraction to allocate
				if ( Mode == 2 || Mode == 4 ) {
					SHR = SHRUnadjusted * ( 1.0 - DXCoil( DXCoilNum ).BypassedFlowFrac( Mode - 1 ) ) + SHR * DXCoil( DXCoilNum ).BypassedFlowFrac( Mode - 1 );
				}

				//  Calculate full load output conditions
				if ( DXCoil( DXCoilNum ).UserSHRCurveExists ) {
					FullLoadOutAirEnth = InletAirEnthalpy - hDelta;
					if ( SHR < 1.0 ) {
						hTinwout = InletAirEnthalpy - ( 1.0 - SHR ) * hDelta;
						FullLoadOutAirHumRat = PsyWFnTdbH( InletAirDryBulbTemp, hTinwout );
						if ( FullLoadOutAirHumRat <= 0.0 ) {
							FullLoadOutAirHumRat = min( DryCoilOutletHumRatioMin, InletAirHumRat );
						}
					} else {
						SHR = 1.0;
						FullLoadOutAirHumRat = InletAirHumRat;
					}
				} else {
					if ( SHR > 1.0 || Counter > 0 ) SHR = 1.0;
					FullLoadOutAirEnth = InletAirEnthalpy - TotCap / AirMassFlow;
					hTinwout = InletAirEnthalpy - ( 1.0 - SHR ) * hDelta;
					if ( SHR < 1.0 ) {
						FullLoadOutAirHumRat = PsyWFnTdbH( InletAirDryBulbTemp, hTinwout );
					} else {
						FullLoadOutAirHumRat = InletAirHumRat;
					}
				}
				FullLoadOutAirTemp = PsyTdbFnHW( FullLoadOutAirEnth, FullLoadOutAirHumRat );

				// apply latent degradation model to cycling fan when RH control is desired and heating coil operates
				// longer than the cooling coil. DXcoolToHeatPLRRatio = Cooling coil PLR / Heating coil PLR.
			} else if ( FanOpMode == CycFanCycCoil ) {
				if ( DXcoolToHeatPLRRatio < 1.0 ) {
					QLatRated = DXCoil( DXCoilNum ).RatedTotCap( Mode ) * ( 1.0 - DXCoil( DXCoilNum ).RatedSHR( Mode ) );
					QLatActual = TotCap * ( 1.0 - SHR );
					HeatRTF = PartLoadRatio / DXcoolToHeatPLRRatio;
					if ( DXCoil( DXCoilNum ).HeatingCoilPLFCurvePTR > 0 ) {
						HeatingCoilPLF = CurveValue( DXCoil( DXCoilNum ).HeatingCoilPLFCurvePTR, HeatRTF );
						if ( HeatingCoilPLF > 0 ) HeatRTF /= HeatingCoilPLF;
					}
					SHRUnadjusted = SHR;
					SHR = CalcEffectiveSHR( DXCoilNum, SHR, DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction, QLatRated, QLatActual, InletAirDryBulbTemp, InletAirWetBulbC, Mode, HeatRTF );
					//   Calculate full load output conditions
					if ( DXCoil( DXCoilNum ).UserSHRCurveExists ) {
						FullLoadOutAirEnth = InletAirEnthalpy - hDelta;
						if ( SHR < 1.0 ) {
							hTinwout = InletAirEnthalpy - ( 1.0 - SHR ) * hDelta;
							FullLoadOutAirHumRat = PsyWFnTdbH( InletAirDryBulbTemp, hTinwout );
							if ( FullLoadOutAirHumRat <= 0.0 ) {
								FullLoadOutAirHumRat = min( DryCoilOutletHumRatioMin, InletAirHumRat );
							}
						} else {
							SHR = 1.0;
							FullLoadOutAirHumRat = InletAirHumRat;
						}
					} else {
						if ( SHR > 1.0 || Counter > 0 ) SHR = 1.0;
						FullLoadOutAirEnth = InletAirEnthalpy - TotCap / AirMassFlow;
						hTinwout = InletAirEnthalpy - ( 1.0 - SHR ) * hDelta;
						if ( SHR < 1.0 ) {
							FullLoadOutAirHumRat = PsyWFnTdbH( InletAirDryBulbTemp, hTinwout );
						} else {
							FullLoadOutAirHumRat = InletAirHumRat;
						}
					}
					FullLoadOutAirTemp = PsyTdbFnHW( FullLoadOutAirEnth, FullLoadOutAirHumRat );
				}

			}

			//  Calculate actual outlet conditions for the input part load ratio
			//  Actual outlet conditions are "average" for time step

			// For multimode coil, if stage-2 operation (modes 2 or 4), return "full load" outlet conditions
			if ( ( ( FanOpMode == ContFanCycCoil ) && ( Mode == 1 ) ) || ( Mode == 3 ) ) {
				// Continuous fan, cycling compressor
				OutletAirEnthalpy = ( ( PartLoadRatio * AirFlowRatio ) * FullLoadOutAirEnth + ( 1.0 - ( PartLoadRatio * AirFlowRatio ) ) * InletAirEnthalpy );
				OutletAirHumRat = ( ( PartLoadRatio * AirFlowRatio ) * FullLoadOutAirHumRat + ( 1.0 - ( PartLoadRatio * AirFlowRatio ) ) * InletAirHumRat );
				OutletAirTemp = PsyTdbFnHW( OutletAirEnthalpy, OutletAirHumRat );
			} else {
				// Default to cycling fan, cycling compressor
				// Also return this result for stage 2 operation of multimode coil
				// Cycling fan typically provides full outlet conditions. When RH control is used, account for additional
				// heating run time by using cooing/heating ratio the same as constant fan (otherwise PLRRatio = 1).
				OutletAirEnthalpy = FullLoadOutAirEnth * DXcoolToHeatPLRRatio + InletAirEnthalpy * ( 1.0 - DXcoolToHeatPLRRatio );
				OutletAirHumRat = FullLoadOutAirHumRat * DXcoolToHeatPLRRatio + InletAirHumRat * ( 1.0 - DXcoolToHeatPLRRatio );
				OutletAirTemp = FullLoadOutAirTemp * DXcoolToHeatPLRRatio + InletAirDryBulbTemp * ( 1.0 - DXcoolToHeatPLRRatio );
			}

			// Check for saturation error and modify temperature at constant enthalpy
			if ( OutletAirTemp < PsyTsatFnHPb( OutletAirEnthalpy, OutdoorPressure, calcDoe2DXCoil ) ) {
				OutletAirTemp = PsyTsatFnHPb( OutletAirEnthalpy, OutdoorPressure );
				//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
				//   IF(OutletAirTemp .LT. PsyTsatFnHPb(OutletAirEnthalpy,InletAirPressure)) THEN
				//    OutletAirTemp = PsyTsatFnHPb(OutletAirEnthalpy,InletAirPressure)
				OutletAirHumRat = PsyWFnTdbH( OutletAirTemp, OutletAirEnthalpy );
			}

			// Mix with air that was bypassed around coil, if any
			if ( BypassFlowFraction > 0.0 ) {
				OutletAirEnthalpy = ( 1.0 - BypassFlowFraction ) * OutletAirEnthalpy + BypassFlowFraction * InletAirEnthalpy;
				OutletAirHumRat = ( 1.0 - BypassFlowFraction ) * OutletAirHumRat + BypassFlowFraction * InletAirHumRat;
				OutletAirTemp = PsyTdbFnHW( OutletAirEnthalpy, OutletAirHumRat );
				// Check for saturation error and modify temperature at constant enthalpy
				if ( OutletAirTemp < PsyTsatFnHPb( OutletAirEnthalpy, OutdoorPressure ) ) {
					OutletAirTemp = PsyTsatFnHPb( OutletAirEnthalpy, OutdoorPressure );
					//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
					//     IF(OutletAirTemp .LT. PsyTsatFnHPb(OutletAirEnthalpy,InletAirPressure)) THEN
					//       OutletAirTemp = PsyTsatFnHPb(OutletAirEnthalpy,InletAirPressure)
					OutletAirHumRat = PsyWFnTdbH( OutletAirTemp, OutletAirEnthalpy );
				}
			}

			// Calculate electricity consumed. First, get EIR modifying factors for off-rated conditions
			if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_HeatPumpWaterHeaterPumped || DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_HeatPumpWaterHeaterWrapped ) {
				//   Coil:DX:HeatPumpWaterHeater does not have EIR temp or flow curves
				EIRTempModFac = 1.0;
				EIRFlowModFac = 1.0;
			} else {
				EIRTempModFac = CurveValue( DXCoil( DXCoilNum ).EIRFTemp( Mode ), InletAirWetBulbC, CondInletTemp );

				//   Warn user if curve output goes negative
				if ( EIRTempModFac < 0.0 ) {
					if ( DXCoil( DXCoilNum ).EIRFTempErrorIndex == 0 ) {
						ShowWarningMessage( RoutineName + DXCoil( DXCoilNum ).DXCoilType + "=\"" + DXCoil( DXCoilNum ).Name + "\":" );
						ShowContinueError( " Energy Input Ratio Modifier curve (function of temperature) output is negative (" + TrimSigDigits( EIRTempModFac, 3 ) + ")." );
						if ( DXCoil( DXCoilNum ).EIRTempModFacCurveType( Mode ) == BiQuadratic ) {
							ShowContinueError( " Negative value occurs using a condenser inlet air temperature of " + TrimSigDigits( CondInletTemp, 1 ) + " and an inlet air wet-bulb temperature of " + TrimSigDigits( InletAirWetBulbC, 1 ) + '.' );
						} else {
							ShowContinueError( " Negative value occurs using a condenser inlet air temperature of " + TrimSigDigits( CondInletTemp, 1 ) + '.' );
						}
						if ( Mode > 1 ) {
							ShowContinueError( " Negative output results from stage " + TrimSigDigits( Mode ) + " compressor operation." );
						}
						ShowContinueErrorTimeStamp( " Resetting curve output to zero and continuing simulation." );
					}
					ShowRecurringWarningErrorAtEnd( RoutineName + DXCoil( DXCoilNum ).DXCoilType + "=\"" + DXCoil( DXCoilNum ).Name + "\": Energy Input Ratio Modifier curve (function of temperature) output is negative warning continues...", DXCoil( DXCoilNum ).EIRFTempErrorIndex, EIRTempModFac, EIRTempModFac );
					EIRTempModFac = 0.0;
				}

				EIRFlowModFac = CurveValue( DXCoil( DXCoilNum ).EIRFFlow( Mode ), AirMassFlowRatio );

				//   Warn user if curve output goes negative
				if ( EIRFlowModFac < 0.0 ) {
					if ( DXCoil( DXCoilNum ).EIRFFlowErrorIndex == 0 ) {
						ShowWarningMessage( RoutineName + DXCoil( DXCoilNum ).DXCoilType + "=\"" + DXCoil( DXCoilNum ).Name + "\":" );
						ShowContinueError( " Energy Input Ratio Modifier curve (function of flow fraction) output is negative (" + TrimSigDigits( EIRFlowModFac, 3 ) + ")." );
						ShowContinueError( " Negative value occurs using an air flow fraction of " + TrimSigDigits( AirMassFlowRatio, 3 ) + '.' );
						ShowContinueErrorTimeStamp( " Resetting curve output to zero and continuing simulation." );
						if ( Mode > 1 ) {
							ShowContinueError( " Negative output results from stage " + TrimSigDigits( Mode ) + " compressor operation." );
						}
					}
					ShowRecurringWarningErrorAtEnd( RoutineName + DXCoil( DXCoilNum ).DXCoilType + "=\"" + DXCoil( DXCoilNum ).Name + "\": Energy Input Ratio Modifier curve (function of flow fraction) output is negative warning continues...", DXCoil( DXCoilNum ).EIRFFlowErrorIndex, EIRFlowModFac, EIRFlowModFac );
					EIRFlowModFac = 0.0;
				}
			}

			EIR = DXCoil( DXCoilNum ).RatedEIR( Mode ) * EIRFlowModFac * EIRTempModFac;

			// For multimode coil, if stage-2 operation (Modes 2 or 4), return "full load" power adjusted for PLF
			if ( Mode == 1 || Mode == 3 ) {
				DXCoil( DXCoilNum ).ElecCoolingPower = TotCap * EIR * DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction;
			} else {
				DXCoil( DXCoilNum ).ElecCoolingPower = TotCap * EIR * DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction / PartLoadRatio;
			}

			// Reset AirMassFlow to inlet node air mass flow for final total, sensible and latent calculations
			// since AirMassFlow might have been modified above (in this subroutine):
			//     IF (FanOpMode .EQ. CycFanCycCoil) AirMassFlow = AirMassFlow / PartLoadRatio
			// For multimode coil, this should be full flow including bypassed fraction
			AirMassFlow = DXCoil( DXCoilNum ).InletAirMassFlowRate;
			DXCoil( DXCoilNum ).TotalCoolingEnergyRate = AirMassFlow * ( InletAirEnthalpy - OutletAirEnthalpy );

			// Set DataHeatGlobal heat reclaim variable for use by heat reclaim coil (part load ratio is accounted for)
			// Calculation for heat reclaim needs to be corrected to use compressor power (not including condenser fan power)
			HeatReclaimDXCoil( DXCoilNum ).AvailCapacity = DXCoil( DXCoilNum ).TotalCoolingEnergyRate + DXCoil( DXCoilNum ).ElecCoolingPower;

			MinAirHumRat = min( InletAirHumRat, OutletAirHumRat );
			DXCoil( DXCoilNum ).SensCoolingEnergyRate = AirMassFlow * ( PsyHFnTdbW( InletAirDryBulbTemp, MinAirHumRat ) - PsyHFnTdbW( OutletAirTemp, MinAirHumRat ) );
			//  Don't let sensible capacity be greater than total capacity
			if ( DXCoil( DXCoilNum ).SensCoolingEnergyRate > DXCoil( DXCoilNum ).TotalCoolingEnergyRate ) {
				DXCoil( DXCoilNum ).SensCoolingEnergyRate = DXCoil( DXCoilNum ).TotalCoolingEnergyRate;
			}
			DXCoil( DXCoilNum ).LatCoolingEnergyRate = DXCoil( DXCoilNum ).TotalCoolingEnergyRate - DXCoil( DXCoilNum ).SensCoolingEnergyRate;

			// Calculate crankcase heater power using the runtime fraction for this DX cooling coil only if there is no companion DX coil.
			// Else use the largest runtime fraction of this DX cooling coil and the companion DX heating coil.
			if ( DXCoil( DXCoilNum ).CompanionUpstreamDXCoil == 0 ) {
				DXCoil( DXCoilNum ).CrankcaseHeaterPower = CrankcaseHeatingPower * ( 1.0 - DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction );
			} else {
				DXCoil( DXCoilNum ).CrankcaseHeaterPower = CrankcaseHeatingPower * ( 1.0 - max( DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction, DXCoil( DXCoil( DXCoilNum ).CompanionUpstreamDXCoil ).HeatingCoilRuntimeFraction ) );
			}

			if ( DXCoil( DXCoilNum ).CondenserType( Mode ) == EvapCooled ) {
				//******************
				//             WATER CONSUMPTION IN m3 OF WATER FOR DIRECT
				//             H2O [m3/sec] = Delta W[KgH2O/Kg air]*Mass Flow Air[Kg air]
				//                                /RhoWater [kg H2O/m3 H2O]
				//******************
				RhoWater = RhoH2O( OutdoorDryBulb );
				DXCoil( DXCoilNum ).EvapWaterConsumpRate = ( CondInletHumRat - OutdoorHumRat ) * CondAirMassFlow / RhoWater * DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction;
				DXCoil( DXCoilNum ).EvapCondPumpElecPower = DXCoil( DXCoilNum ).EvapCondPumpElecNomPower( Mode ) * DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction;
				// Calculate basin heater power
				CalcBasinHeaterPower( DXCoil( DXCoilNum ).BasinHeaterPowerFTempDiff, DXCoil( DXCoilNum ).BasinHeaterSchedulePtr, DXCoil( DXCoilNum ).BasinHeaterSetPointTemp, DXCoil( DXCoilNum ).BasinHeaterPower );
				if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_CoolingSingleSpeed ) {
					DXCoil( DXCoilNum ).BasinHeaterPower *= ( 1.0 - DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction );
				}
			}

			DXCoil( DXCoilNum ).OutletAirTemp = OutletAirTemp;
			DXCoil( DXCoilNum ).OutletAirHumRat = OutletAirHumRat;
			DXCoil( DXCoilNum ).OutletAirEnthalpy = OutletAirEnthalpy;

		} else {

			// DX coil is off; just pass through conditions
			DXCoil( DXCoilNum ).OutletAirEnthalpy = DXCoil( DXCoilNum ).InletAirEnthalpy;
			DXCoil( DXCoilNum ).OutletAirHumRat = DXCoil( DXCoilNum ).InletAirHumRat;
			DXCoil( DXCoilNum ).OutletAirTemp = DXCoil( DXCoilNum ).InletAirTemp;

			DXCoil( DXCoilNum ).ElecCoolingPower = 0.0;
			DXCoil( DXCoilNum ).TotalCoolingEnergyRate = 0.0;
			DXCoil( DXCoilNum ).SensCoolingEnergyRate = 0.0;
			DXCoil( DXCoilNum ).LatCoolingEnergyRate = 0.0;
			DXCoil( DXCoilNum ).EvapCondPumpElecPower = 0.0;
			DXCoil( DXCoilNum ).EvapWaterConsumpRate = 0.0;

			// Reset globals when DX coil is OFF for use in heat recovery module
			DXCoilFullLoadOutAirTemp( DXCoilNum ) = 0.0;
			DXCoilFullLoadOutAirHumRat( DXCoilNum ) = 0.0;

			// Calculate crankcase heater power using the runtime fraction for this DX cooling coil (here DXCoolingCoilRTF=0) if
			// there is no companion DX coil, or the runtime fraction of the companion DX heating coil (here DXHeatingCoilRTF>=0).
			if ( DXCoil( DXCoilNum ).CompanionUpstreamDXCoil == 0 ) {
				DXCoil( DXCoilNum ).CrankcaseHeaterPower = CrankcaseHeatingPower;
			} else {
				DXCoil( DXCoilNum ).CrankcaseHeaterPower = CrankcaseHeatingPower * ( 1.0 - DXCoil( DXCoil( DXCoilNum ).CompanionUpstreamDXCoil ).HeatingCoilRuntimeFraction );
			}

			// Calculate basin heater power
			if ( DXCoil( DXCoilNum ).DXCoilType_Num == CoilDX_CoolingTwoStageWHumControl ) {
				if ( any_eq( DXCoil( DXCoilNum ).CondenserType, EvapCooled ) ) {
					CalcBasinHeaterPower( DXCoil( DXCoilNum ).BasinHeaterPowerFTempDiff, DXCoil( DXCoilNum ).BasinHeaterSchedulePtr, DXCoil( DXCoilNum ).BasinHeaterSetPointTemp, DXCoil( DXCoilNum ).BasinHeaterPower );
				}
			} else {
				if ( DXCoil( DXCoilNum ).CondenserType( Mode ) == EvapCooled ) {
					CalcBasinHeaterPower( DXCoil( DXCoilNum ).BasinHeaterPowerFTempDiff, DXCoil( DXCoilNum ).BasinHeaterSchedulePtr, DXCoil( DXCoilNum ).BasinHeaterSetPointTemp, DXCoil( DXCoilNum ).BasinHeaterPower );
				}
			}

		} // end of on/off if - else

		//set water system demand request (if needed)
		if ( DXCoil( DXCoilNum ).EvapWaterSupplyMode == WaterSupplyFromTank ) {
			WaterStorage( DXCoil( DXCoilNum ).EvapWaterSupTankID ).VdotRequestDemand( DXCoil( DXCoilNum ).EvapWaterTankDemandARRID ) = DXCoil( DXCoilNum ).EvapWaterConsumpRate;
		}

		DXCoilOutletTemp( DXCoilNum ) = DXCoil( DXCoilNum ).OutletAirTemp;
		DXCoilOutletHumRat( DXCoilNum ) = DXCoil( DXCoilNum ).OutletAirHumRat;
		DXCoilPartLoadRatio( DXCoilNum ) = DXCoil( DXCoilNum ).PartLoadRatio;
		DXCoilFanOpMode( DXCoilNum ) = FanOpMode;
		DXCoil( DXCoilNum ).CondInletTemp = CondInletTemp;

		// calc secondary coil if specified
		if ( DXCoil( DXCoilNum ).IsSecondaryDXCoilInZone ) {
			CalcSecondaryDXCoils( DXCoilNum );
		}

	}

	void
	CalcVRFCoolingCoil(
		int const DXCoilNum, // the number of the DX coil to be simulated
		int const CompOp, // compressor operation; 1=on, 0=off
		bool const FirstHVACIteration, // true if this is the first iteration of HVAC
		Real64 const PartLoadRatio, // sensible cooling load / full load sensible cooling capacity
		int const FanOpMode, // Allows parent object to control fan operation
		Real64 const CompCycRatio, // cycling ratio of VRF condenser
		Optional_int_const PerfMode, // Performance mode for MultiMode DX coil; Always 1 for other coil types
		Optional< Real64 const > OnOffAirFlowRatio, // ratio of compressor on airflow to compressor off airflow
		Optional< Real64 const > MaxCoolCap // maximum capacity of DX coil
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   August 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the air-side performance of a direct-expansion, air-cooled
		// VRF terminal unit cooling coil.
		// A new subroutine was created in case this DX coil model is significantly
		// different from the existing CalcDoe2DXCoil subroutine. The VRF heating coil
		// uses the existing DX heating coil subroutine (CalcDXHeatingCoil).

		// METHODOLOGY EMPLOYED:
		// This routine simulates the performance of a variable refrigerant flow cooling coil.
		// The routine requires the user to enter the total cooling capacity and sensible heat ratio.
		// Since different manufacturer's rate their equipment at different air flow rates,
		// the supply air flow rate corresponding to the rated capacities must also be
		// entered (should be between 300 cfm/ton and 450 cfm/ton). The rated information entered by
		// the user should NOT include the thermal or electrical impacts of the supply air fan, as
		// this is addressed by another module.

		// With the rated performance data entered by the user, the model employs some of the
		// DOE-2.1E curve fits to adjust the capacity and efficiency of the unit as a function
		// of entering air temperatures and supply air flow rate (actual vs rated flow). The model
		// does NOT employ the exact same methodology to calculate performance as DOE-2.
		// This VRF cooling coil model adjusts the rated total cooling capacity by the CAPFT
		// and CAP funciton of flow curve/model currently used by the existing DX coil model.
		// The part-load ratio is then applied to the total operating capacity to find the capacity
		// required to meet the load. This VRF model then uses the ADP/bypass method to find the
		// SHR and resulting outlet conditions given that total capacity (or delta H).

		// The model checks for coil dryout conditions, and adjusts the calculated performance
		// appropriately.

		// REFERENCES:
		// na

		// Using/Aliasing
		using CurveManager::CurveValue;
		using DataGlobals::CurrentTime;
		using DataHVACGlobals::HPWHCrankcaseDBTemp;
		using DataHVACGlobals::TimeStepSys;
		using DataHVACGlobals::SysTimeElapsed;
		using General::TrimSigDigits;
		using General::RoundSigDigits;
		using General::CreateSysTimeIntervalString;
		using DataWater::WaterStorage;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		//  REAL(r64), INTENT(IN), OPTIONAL :: CoolingHeatingPLR   ! used for cycling fan RH control

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcVRFCoolingCoil" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 AirMassFlow; // dry air mass flow rate through coil [kg/s] (adjusted for bypass if any)
		Real64 AirMassFlowRatio; // Ratio of actual air mass flow to rated air mass flow (adjusted for bypass if any)
		Real64 AirVolumeFlowRate; // Air volume flow rate across the cooling coil [m3/s] (adjusted for bypass if any)
		// (average flow if cycling fan, full flow if constant fan)
		Real64 VolFlowperRatedTotCap; // Air volume flow rate divided by rated total cooling capacity [m3/s-W] (adjusted for bypass)
		Real64 TotCap; // gross total cooling capacity at off-rated conditions [W]
		Real64 TotCapTempModFac; // Total capacity modifier (function of entering wetbulb, outside drybulb)
		Real64 TotCapFlowModFac; // Total capacity modifier (function of actual supply air flow vs rated flow)
		Real64 InletAirWetBulbC; // wetbulb temperature of inlet air [C]
		Real64 InletAirDryBulbTemp; // inlet air dry bulb temperature [C]
		Real64 InletAirEnthalpy; // inlet air enthalpy [J/kg]
		Real64 InletAirHumRat; // inlet air humidity ratio [kg/kg]
		Real64 InletAirHumRatTemp; // inlet air humidity ratio used in ADP/BF loop [kg/kg]
		//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
		//REAL(r64) :: InletAirPressure      ! inlet air pressure [Pa]
		Real64 RatedCBF; // coil bypass factor at rated conditions
		Real64 SHR; // Sensible Heat Ratio (sensible/total) of the cooling coil
		Real64 CBF; // coil bypass factor at off rated conditions
		Real64 A0; // NTU * air mass flow rate, used in CBF calculation
		Real64 hDelta; // Change in air enthalpy across the cooling coil [J/kg]
		Real64 hADP; // Apparatus dew point enthalpy [J/kg]
		Real64 hTinwADP; // Enthalpy at inlet dry-bulb and wADP [J/kg]
		Real64 hTinwout; // Enthalpy at inlet dry-bulb and outlet humidity ratio [J/kg]
		Real64 tADP; // Apparatus dew point temperature [C]
		Real64 wADP; // Apparatus dew point humidity ratio [kg/kg]
		Real64 FullLoadOutAirEnth; // outlet full load enthalpy [J/kg]
		Real64 FullLoadOutAirHumRat; // outlet humidity ratio at full load
		Real64 FullLoadOutAirTemp; // outlet air temperature at full load [C]
		Real64 PLF; // Part load factor, accounts for thermal lag at compressor startup, used in power calculation
		Real64 QLatActual; // operating latent capacity of DX coil
		Real64 QLatRated; // Rated latent capacity of DX coil
		Real64 SHRUnadjusted; // SHR prior to latent degradation effective SHR calculation
		int Counter; // Counter for dry evaporator iterations
		int MaxIter; // Maximum number of iterations for dry evaporator calculations
		Real64 RF; // Relaxation factor for dry evaporator iterations
		Real64 Tolerance; // Error tolerance for dry evaporator iterations
		Real64 werror; // Deviation of humidity ratio in dry evaporator iteration loop
		Real64 CondInletTemp; // Condenser inlet temperature (C). Outdoor dry-bulb temp for air-cooled condenser.
		// Outdoor Wetbulb +(1 - effectiveness)*(outdoor drybulb - outdoor wetbulb) for evap condenser.
		Real64 CondInletHumRat; // Condenser inlet humidity ratio (kg/kg). Zero for air-cooled condenser.
		// For evap condenser, its the humidity ratio of the air leaving the evap cooling pads.
		Real64 CondAirMassFlow; // Condenser air mass flow rate [kg/s]
		Real64 RhoAir; // Density of air [kg/m3]
		Real64 CrankcaseHeatingPower; // power due to crankcase heater
		static Real64 CompAmbTemp( 0.0 ); // Ambient temperature at compressor
		Real64 AirFlowRatio; // ratio of compressor on airflow to average timestep airflow
		// used when constant fan mode yields different air flow rates when compressor is ON and OFF
		// (e.g. Packaged Terminal Heat Pump)
		Real64 OutdoorDryBulb; // Outdoor dry-bulb temperature at condenser (C)
		Real64 OutdoorWetBulb; // Outdoor wet-bulb temperature at condenser (C)
		Real64 OutdoorHumRat; // Outdoor humidity ratio at condenser (kg/kg)
		Real64 OutdoorPressure; // Outdoor barometric pressure at condenser (Pa)

		static Real64 CurrentEndTime( 0.0 ); // end time of time step for current simulation time step
		static Real64 MinAirHumRat( 0.0 ); // minimum of the inlet air humidity ratio and the outlet air humidity ratio
		int Mode; // Performance mode for Multimode DX coil; Always 1 for other coil types
		Real64 OutletAirTemp; // Supply air temperature (average value if constant fan, full output if cycling fan)
		Real64 OutletAirHumRat; // Supply air humidity ratio (average value if constant fan, full output if cycling fan)
		Real64 OutletAirEnthalpy; // Supply air enthalpy (average value if constant fan, full output if cycling fan)
		Real64 ADiff; // Used for exponential

		// If Performance mode not present, then set to 1.  Used only by Multimode/Multispeed DX coil (otherwise mode = 1)
		if ( present( PerfMode ) ) {
			Mode = PerfMode;
		} else {
			Mode = 1;
		}

		// If AirFlowRatio not present, then set to 1. Used only by DX coils with different air flow
		// during cooling and when no cooling is required (constant fan, fan speed changes)
		if ( present( OnOffAirFlowRatio ) ) {
			AirFlowRatio = OnOffAirFlowRatio;
		} else {
			AirFlowRatio = 1.0;
		}

		MaxIter = 30;
		RF = 0.4;
		Counter = 0;
		Tolerance = 0.01;
		CondInletTemp = 0.0;
		CondInletHumRat = 0.0;
		AirMassFlow = DXCoil( DXCoilNum ).InletAirMassFlowRate;
		InletAirDryBulbTemp = DXCoil( DXCoilNum ).InletAirTemp;
		InletAirEnthalpy = DXCoil( DXCoilNum ).InletAirEnthalpy;
		InletAirHumRat = DXCoil( DXCoilNum ).InletAirHumRat;
		//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
		//InletAirPressure    = DXCoil(DXCoilNum)%InletAirPressure
		HeatReclaimDXCoil( DXCoilNum ).AvailCapacity = 0.0;
		DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction = 0.0;
		DXCoil( DXCoilNum ).PartLoadRatio = 0.0;
		DXCoil( DXCoilNum ).BasinHeaterPower = 0.0;

		if ( DXCoil( DXCoilNum ).CondenserInletNodeNum( Mode ) != 0 ) {
			OutdoorDryBulb = Node( DXCoil( DXCoilNum ).CondenserInletNodeNum( Mode ) ).Temp;
			if ( DXCoil( DXCoilNum ).CondenserType( Mode ) == WaterCooled ) {
				OutdoorHumRat = OutHumRat;
				OutdoorPressure = OutBaroPress;
				OutdoorWetBulb = OutWetBulbTemp;
			} else {
				OutdoorPressure = Node( DXCoil( DXCoilNum ).CondenserInletNodeNum( Mode ) ).Press;
				// If node is not connected to anything, pressure = default, use weather data
				if ( OutdoorPressure == DefaultNodeValues.Press ) {
					OutdoorDryBulb = OutDryBulbTemp;
					OutdoorHumRat = OutHumRat;
					OutdoorPressure = OutBaroPress;
					OutdoorWetBulb = OutWetBulbTemp;
				} else {
					OutdoorHumRat = Node( DXCoil( DXCoilNum ).CondenserInletNodeNum( Mode ) ).HumRat;
					// this should use Node%WetBulbTemp or a PSYC function, not OAWB
					OutdoorWetBulb = Node( DXCoil( DXCoilNum ).CondenserInletNodeNum( Mode ) ).OutAirWetBulb;
				}
			}
		} else {
			OutdoorDryBulb = OutDryBulbTemp;
			OutdoorHumRat = OutHumRat;
			OutdoorPressure = OutBaroPress;
			OutdoorWetBulb = OutWetBulbTemp;
		}

		if ( DXCoil( DXCoilNum ).CondenserType( Mode ) == EvapCooled ) {
			RhoAir = PsyRhoAirFnPbTdbW( OutdoorPressure, OutdoorDryBulb, OutdoorHumRat );
			CondAirMassFlow = RhoAir * DXCoil( DXCoilNum ).EvapCondAirFlow( Mode );
			// (Outdoor wet-bulb temp from DataEnvironment) + (1.0-EvapCondEffectiveness) * (drybulb - wetbulb)
			CondInletTemp = OutdoorWetBulb + ( OutdoorDryBulb - OutdoorWetBulb ) * ( 1.0 - DXCoil( DXCoilNum ).EvapCondEffect( Mode ) );
			CondInletHumRat = PsyWFnTdbTwbPb( CondInletTemp, OutdoorWetBulb, OutdoorPressure );
			CompAmbTemp = OutdoorDryBulb;
		} else { // for air or water-cooled, inlet temp is stored in OutdoorDryBulb temp
			CondInletTemp = OutdoorDryBulb; // Outdoor dry-bulb temp or water inlet temp
			if ( DXCoil( DXCoilNum ).CondenserType( Mode ) == WaterCooled ) {
				CompAmbTemp = OutDryBulbTemp; // for crankcase heater use actual outdoor temp for water-cooled
			} else {
				CompAmbTemp = OutdoorDryBulb;
			}
		}

		// Initialize crankcase heater, operates below OAT defined in input deck for HP DX cooling coil
		// If used in a heat pump, the value of MaxOAT in the heating coil overrides that in the cooling coil (in GetInput)
		if ( CompAmbTemp < DXCoil( DXCoilNum ).MaxOATCrankcaseHeater ) {
			CrankcaseHeatingPower = DXCoil( DXCoilNum ).CrankcaseHeaterCapacity;
		} else {
			CrankcaseHeatingPower = 0.0;
		}

		// calculate end time of current time step to determine if error messages should be printed
		CurrentEndTime = CurrentTime + SysTimeElapsed;

		//   Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
		//   Wait for next time step to print warnings. If simulation iterates, print out
		//   the warning for the last iteration only. Must wait for next time step to accomplish this.
		//   If a warning occurs and the simulation down shifts, the warning is not valid.
		if ( DXCoil( DXCoilNum ).PrintLowAmbMessage ) { // .AND. &
			if ( CurrentEndTime > DXCoil( DXCoilNum ).CurrentEndTimeLast && TimeStepSys >= DXCoil( DXCoilNum ).TimeStepSysLast ) {
				if ( DXCoil( DXCoilNum ).LowAmbErrIndex == 0 ) {
					ShowWarningMessage( DXCoil( DXCoilNum ).LowAmbBuffer1 );
					ShowContinueError( DXCoil( DXCoilNum ).LowAmbBuffer2 );
					ShowContinueError( "... Operation at low inlet temperatures may require special performance curves." );
				}
				ShowRecurringWarningErrorAtEnd( DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + "\" - Low condenser inlet temperature error continues...", DXCoil( DXCoilNum ).LowAmbErrIndex, DXCoil( DXCoilNum ).LowTempLast, DXCoil( DXCoilNum ).LowTempLast, _, "[C]", "[C]" );
			}
		}

		if ( DXCoil( DXCoilNum ).PrintHighAmbMessage ) { // .AND. &
			if ( CurrentEndTime > DXCoil( DXCoilNum ).CurrentEndTimeLast && TimeStepSys >= DXCoil( DXCoilNum ).TimeStepSysLast ) {
				if ( DXCoil( DXCoilNum ).HighAmbErrIndex == 0 ) {
					ShowWarningMessage( DXCoil( DXCoilNum ).HighAmbBuffer1 );
					ShowContinueError( DXCoil( DXCoilNum ).HighAmbBuffer2 );
					ShowContinueError( "... Operation at high inlet temperatures may require special performance curves." );
				}
				ShowRecurringWarningErrorAtEnd( DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + "\" - High condenser inlet temperature error continues...", DXCoil( DXCoilNum ).HighAmbErrIndex, DXCoil( DXCoilNum ).HighTempLast, DXCoil( DXCoilNum ).HighTempLast, _, "[C]", "[C]" );
			}
		}

		if ( DXCoil( DXCoilNum ).PrintLowOutTempMessage ) {
			if ( CurrentEndTime > DXCoil( DXCoilNum ).CurrentEndTimeLast && TimeStepSys >= DXCoil( DXCoilNum ).TimeStepSysLast ) {
				if ( DXCoil( DXCoilNum ).LowOutletTempIndex == 0 ) {
					ShowWarningMessage( DXCoil( DXCoilNum ).LowOutTempBuffer1 );
					ShowContinueError( DXCoil( DXCoilNum ).LowOutTempBuffer2 );
					ShowContinueError( "... Possible reasons for low outlet air dry-bulb temperatures are: This DX coil" );
					ShowContinueError( "   1) may have a low inlet air dry-bulb temperature. Inlet air temperature = " + TrimSigDigits( DXCoil( DXCoilNum ).FullLoadInletAirTempLast, 3 ) + " C." );
					ShowContinueError( "   2) may have a low air flow rate per watt of cooling capacity. Check inputs." );
					ShowContinueError( "   3) is used as part of a HX assisted cooling coil which uses a high sensible effectiveness. Check inputs." );
				}
				ShowRecurringWarningErrorAtEnd( DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + "\" - Full load outlet temperature indicates a possibility of frost/freeze error continues. Outlet air temperature statistics follow:", DXCoil( DXCoilNum ).LowOutletTempIndex, DXCoil( DXCoilNum ).FullLoadOutAirTempLast, DXCoil( DXCoilNum ).FullLoadOutAirTempLast );
			}
		}

		// save last system time step and last end time of current time step (used to determine if warning is valid)
		DXCoil( DXCoilNum ).TimeStepSysLast = TimeStepSys;
		DXCoil( DXCoilNum ).CurrentEndTimeLast = CurrentEndTime;
		DXCoil( DXCoilNum ).PrintLowAmbMessage = false;
		DXCoil( DXCoilNum ).PrintLowOutTempMessage = false;

		if ( ( AirMassFlow > 0.0 ) && ( GetCurrentScheduleValue( DXCoil( DXCoilNum ).SchedPtr ) > 0.0 ) && ( PartLoadRatio > 0.0 ) && ( CompOp == On ) ) { // for cycling fan, reset mass flow to full on rate
			if ( FanOpMode == CycFanCycCoil ) {
				AirMassFlow /= PartLoadRatio;
			} else if ( FanOpMode == ContFanCycCoil ) {
				AirMassFlow *= AirFlowRatio;
			} else {
				AirMassFlow = DXCoil( DXCoilNum ).RatedAirMassFlowRate( Mode );
			}

			// Check for valid air volume flow per rated total cooling capacity (200 - 500 cfm/ton)

			// for some reason there are diff's when using coil inlet air pressure
			// these lines (more to follow) are commented out for the time being

			InletAirWetBulbC = PsyTwbFnTdbWPb( InletAirDryBulbTemp, InletAirHumRat, OutdoorPressure );
			AirVolumeFlowRate = AirMassFlow / PsyRhoAirFnPbTdbW( OutdoorPressure, InletAirDryBulbTemp, InletAirHumRat );
			VolFlowperRatedTotCap = AirVolumeFlowRate / DXCoil( DXCoilNum ).RatedTotCap( Mode );

			if ( DXCoil( DXCoilNum ).RatedTotCap( Mode ) <= 0.0 ) {
				ShowFatalError( DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + "\" - Rated total cooling capacity is zero or less." );
			}

			if ( ! FirstHVACIteration && ! WarmupFlag && ( ( VolFlowperRatedTotCap < MinOperVolFlowPerRatedTotCap( DXCT ) ) || ( VolFlowperRatedTotCap > MaxCoolVolFlowPerRatedTotCap( DXCT ) ) ) ) {
				if ( DXCoil( DXCoilNum ).ErrIndex1 == 0 ) {
					ShowWarningMessage( DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + "\" - Air volume flow rate per watt of rated total cooling capacity is out of range at " + RoundSigDigits( VolFlowperRatedTotCap, 3 ) + " m3/s/W." );
					ShowContinueErrorTimeStamp( "" );
					ShowContinueError( "...Expected range for VolumeFlowPerRatedTotalCapacity=[" + RoundSigDigits( MinOperVolFlowPerRatedTotCap( DXCT ), 3 ) + "--" + RoundSigDigits( MaxCoolVolFlowPerRatedTotCap( DXCT ), 3 ) + ']' );
					ShowContinueError( "...Possible causes include inconsistent air flow rates in system components," );
					ShowContinueError( "...or mixing manual inputs with autosize inputs. Also check the following values and calculations." );
					ShowContinueError( "...Volume Flow Rate per Rated Total Capacity = Volume Flow Rate / Rated Total Capacity" );
					ShowContinueError( "...Volume Flow Rate = Air Mass Flow Rate / Air Density" );
					ShowContinueError( "...Data used for calculations:" );
					ShowContinueError( "...Rated Total Capacity = " + RoundSigDigits( DXCoil( DXCoilNum ).RatedTotCap( Mode ), 2 ) + " W." );
					ShowContinueError( "...Volume Flow Rate = Air Mass Flow Rate / Air Density" );
					ShowContinueError( "...Volume Flow Rate   = " + RoundSigDigits( AirVolumeFlowRate, 8 ) + " m3/s." );
					ShowContinueError( "...Air Mass Flow Rate = " + RoundSigDigits( AirMassFlow, 8 ) + " kg/s." );
					ShowContinueError( "...Air Density        = " + RoundSigDigits( PsyRhoAirFnPbTdbW( OutdoorPressure, InletAirDryBulbTemp, InletAirHumRat ), 8 ) + " kg/m3." );
					ShowContinueError( "...Data used for air density calculation:" );
					ShowContinueError( "...Outdoor Air Pressure     = " + RoundSigDigits( OutdoorPressure, 3 ) + " Pa." );
					ShowContinueError( "...Inlet Air Dry-Bulb Temp  = " + RoundSigDigits( InletAirDryBulbTemp, 3 ) + " C." );
					ShowContinueError( "...Inlet Air Humidity Ratio = " + RoundSigDigits( InletAirHumRat, 8 ) + " kgWater/kgDryAir." );

				}
				ShowRecurringWarningErrorAtEnd( DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + "\" - Air volume flow rate per watt of rated total cooling capacity is out of range error continues...", DXCoil( DXCoilNum ).ErrIndex1, VolFlowperRatedTotCap, VolFlowperRatedTotCap );
			}
			//    Adjust coil bypass factor for actual air flow rate. Use relation CBF = exp(-NTU) where
			//    NTU = A0/(m*cp). Relationship models the cooling coil as a heat exchanger with Cmin/Cmax = 0.

			RatedCBF = DXCoil( DXCoilNum ).RatedCBF( Mode );
			if ( RatedCBF > 0.0 ) {
				A0 = -std::log( RatedCBF ) * DXCoil( DXCoilNum ).RatedAirMassFlowRate( Mode );
			} else {
				A0 = 0.0;
			}
			ADiff = -A0 / AirMassFlow;
			if ( ADiff >= EXP_LowerLimit ) {
				CBF = std::exp( ADiff );
			} else {
				CBF = 0.0;
			}

			// check boundary for low ambient temperature and post warnings to individual DX coil buffers to print at end of time step
			if ( OutdoorDryBulb < DXCoil( DXCoilNum ).MinOATCompressor && ! WarmupFlag ) {
				DXCoil( DXCoilNum ).PrintLowAmbMessage = true;
				DXCoil( DXCoilNum ).LowTempLast = OutdoorDryBulb;
				if ( DXCoil( DXCoilNum ).LowAmbErrIndex == 0 ) {
					DXCoil( DXCoilNum ).LowAmbBuffer1 = DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + "\" - Condenser inlet temperature below " + RoundSigDigits( DXCoil( DXCoilNum ).MinOATCompressor, 2 ) + " C. Condenser inlet temperature = " + RoundSigDigits( OutdoorDryBulb, 2 );
					DXCoil( DXCoilNum ).LowAmbBuffer2 = " ... Occurrence info = " + EnvironmentName + ", " + CurMnDy + ' ' + CreateSysTimeIntervalString();
				}
			}

			// check boundary for high ambient temperature and post warnings to individual DX coil buffers to print at end of time step
			if ( OutdoorDryBulb > DXCoil( DXCoilNum ).MaxOATCompressor && ! WarmupFlag ) {
				DXCoil( DXCoilNum ).PrintHighAmbMessage = true;
				DXCoil( DXCoilNum ).HighTempLast = OutdoorDryBulb;
				if ( DXCoil( DXCoilNum ).HighAmbErrIndex == 0 ) {
					DXCoil( DXCoilNum ).HighAmbBuffer1 = DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + "\" - Condenser inlet temperature above " + RoundSigDigits( DXCoil( DXCoilNum ).MaxOATCompressor, 2 ) + " C. Condenser temperature = " + RoundSigDigits( OutdoorDryBulb, 2 );
					DXCoil( DXCoilNum ).HighAmbBuffer2 = " ... Occurrence info = " + EnvironmentName + ", " + CurMnDy + ' ' + CreateSysTimeIntervalString();
				}
			}

			//  Get total capacity modifying factor (function of temperature) for off-rated conditions
			//  InletAirHumRat may be modified in this ADP/BF loop, use temporary varible for calculations
			InletAirHumRatTemp = InletAirHumRat;
			// No need to differentiate between curve types, single-independent curve will just use first variable
			// (as long as the first independent variable is the same for both curve types)
Label50: ;
			TotCapTempModFac = CurveValue( DXCoil( DXCoilNum ).CCapFTemp( Mode ), InletAirWetBulbC, CondInletTemp );

			//  Warn user if curve output goes negative
			if ( TotCapTempModFac < 0.0 ) {
				if ( DXCoil( DXCoilNum ).CCapFTempErrorIndex == 0 ) {
					ShowWarningMessage( DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + "\":" );
					ShowContinueError( " Total Cooling Capacity Modifier curve (function of temperature) output is negative (" + TrimSigDigits( TotCapTempModFac, 3 ) + ")." );
					ShowContinueError( " Negative value occurs using a condenser inlet temperature of " + TrimSigDigits( CondInletTemp, 1 ) + " and an inlet air wet-bulb temperature of " + TrimSigDigits( InletAirWetBulbC, 1 ) + '.' );
					if ( Mode > 1 ) {
						ShowContinueError( " Negative output results from stage " + TrimSigDigits( Mode ) + " compressor operation." );
					}
					ShowContinueErrorTimeStamp( " Resetting curve output to zero and continuing simulation." );
				}
				ShowRecurringWarningErrorAtEnd( DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + "\": Total Cooling Capacity Modifier curve (function of temperature) output is negative warning continues...", DXCoil( DXCoilNum ).CCapFTempErrorIndex, TotCapTempModFac, TotCapTempModFac );
				TotCapTempModFac = 0.0;
			}

			//  Get total capacity modifying factor (function of mass flow) for off-rated conditions
			AirMassFlowRatio = AirMassFlow / DXCoil( DXCoilNum ).RatedAirMassFlowRate( Mode );
			TotCapFlowModFac = CurveValue( DXCoil( DXCoilNum ).CCapFFlow( Mode ), AirMassFlowRatio );

			//  Warn user if curve output goes negative
			if ( TotCapFlowModFac < 0.0 ) {
				if ( DXCoil( DXCoilNum ).CCapFFlowErrorIndex == 0 ) {
					ShowWarningMessage( DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + "\":" );
					ShowContinueError( " Total Cooling Capacity Modifier curve (function of flow fraction) output is negative (" + TrimSigDigits( TotCapFlowModFac, 3 ) + ")." );
					ShowContinueError( " Negative value occurs using an air flow fraction of " + TrimSigDigits( AirMassFlowRatio, 3 ) + '.' );
					ShowContinueErrorTimeStamp( " Resetting curve output to zero and continuing simulation." );
					if ( Mode > 1 ) {
						ShowContinueError( " Negative output results from stage " + TrimSigDigits( Mode ) + " compressor operation." );
					}
				}
				ShowRecurringWarningErrorAtEnd( DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + "\": Total Cooling Capacity Modifier curve (function of flow fraction) output is negative warning continues...", DXCoil( DXCoilNum ).CCapFFlowErrorIndex, TotCapFlowModFac, TotCapFlowModFac );
				TotCapFlowModFac = 0.0;
			}

			if ( present( MaxCoolCap ) ) {
				TotCap = min( MaxCoolCap, DXCoil( DXCoilNum ).RatedTotCap( Mode ) * TotCapFlowModFac * TotCapTempModFac );
			} else {
				TotCap = DXCoil( DXCoilNum ).RatedTotCap( Mode ) * TotCapFlowModFac * TotCapTempModFac;
			}

			TotCap *= PartLoadRatio;

			// Calculate apparatus dew point conditions using TotCap and CBF
			hDelta = TotCap / AirMassFlow;
			// there is an issue here with using CBF to calculate the ADP enthalpy.
			// at low loads the bypass factor increases significantly.
			hADP = InletAirEnthalpy - hDelta / ( 1.0 - CBF );
			tADP = PsyTsatFnHPb( hADP, OutdoorPressure, RoutineName );
			//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
			//  tADP = PsyTsatFnHPb(hADP,InletAirPressure)
			wADP = min( InletAirHumRat, PsyWFnTdbH( tADP, hADP, RoutineName ) );
			hTinwADP = PsyHFnTdbW( InletAirDryBulbTemp, wADP );
			if ( ( InletAirEnthalpy - hADP ) > 1.e-10 ) {
				SHR = min( ( hTinwADP - hADP ) / ( InletAirEnthalpy - hADP ), 1.0 );
			} else {
				SHR = 1.0;
			}
			// Check for dry evaporator conditions (win < wadp)
			if ( wADP > InletAirHumRatTemp || ( Counter >= 1 && Counter < MaxIter ) ) {
				if ( InletAirHumRatTemp == 0.0 ) InletAirHumRatTemp = 0.00001;
				werror = ( InletAirHumRatTemp - wADP ) / InletAirHumRatTemp;
				// Increase InletAirHumRatTemp at constant InletAirTemp to find coil dry-out point. Then use the
				// capacity at the dry-out point to determine exiting conditions from coil. This is required
				// since the TotCapTempModFac doesn't work properly with dry-coil conditions.
				InletAirHumRatTemp = RF * wADP + ( 1.0 - RF ) * InletAirHumRatTemp;
				InletAirWetBulbC = PsyTwbFnTdbWPb( InletAirDryBulbTemp, InletAirHumRatTemp, OutdoorPressure );
				//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
				//     InletAirWetBulbC = PsyTwbFnTdbWPb(InletAirDryBulbTemp,InletAirHumRatTemp,InletAirPressure)
				++Counter;
				if ( std::abs( werror ) > Tolerance ) goto Label50; // Recalculate with modified inlet conditions

			}

			if ( DXCoil( DXCoilNum ).PLFFPLR( Mode ) > 0 && CompCycRatio < 1.0 ) {
				PLF = CurveValue( DXCoil( DXCoilNum ).PLFFPLR( Mode ), CompCycRatio ); // Calculate part-load factor
			} else {
				PLF = 1.0;
			}

			if ( PLF < 0.7 ) {
				if ( DXCoil( DXCoilNum ).ErrIndex2 == 0 ) {
					ShowWarningMessage( "The PLF curve value for the DX cooling coil " + DXCoil( DXCoilNum ).Name + " =" + RoundSigDigits( PLF, 3 ) + " for part-load ratio =" + RoundSigDigits( PartLoadRatio, 3 ) );
					ShowContinueErrorTimeStamp( "PLF curve values must be >= 0.7. PLF has been reset to 0.7 and simulation is continuing." );
					ShowContinueError( "Check the IO reference manual for PLF curve guidance [Coil:Cooling:DX:SingleSpeed]." );
				}
				ShowRecurringWarningErrorAtEnd( DXCoil( DXCoilNum ).Name + ", DX cooling coil PLF curve < 0.7 warning continues...", DXCoil( DXCoilNum ).ErrIndex2, PLF, PLF );
				PLF = 0.7;
			}

			DXCoil( DXCoilNum ).PartLoadRatio = PartLoadRatio;
			DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction = CompCycRatio / PLF;
			if ( DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction > 1.0 && std::abs( DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction - 1.0 ) > 0.001 ) {
				if ( DXCoil( DXCoilNum ).ErrIndex3 == 0 ) {
					ShowWarningMessage( "The runtime fraction for DX cooling coil " + DXCoil( DXCoilNum ).Name + " exceeded 1.0. [" + RoundSigDigits( DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction, 4 ) + "]." );
					ShowContinueError( "Runtime fraction reset to 1 and the simulation will continue." );
					ShowContinueError( "Check the IO reference manual for PLF curve guidance [Coil:Cooling:DX:SingleSpeed]." );
					ShowContinueErrorTimeStamp( "" );
				}
				ShowRecurringWarningErrorAtEnd( DXCoil( DXCoilNum ).Name + ", DX cooling coil runtime fraction > 1.0 warning continues...", DXCoil( DXCoilNum ).ErrIndex3, DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction, DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction );
				DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction = 1.0; // Reset coil runtime fraction to 1.0
			} else if ( DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction > 1.0 ) {
				DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction = 1.0; // Reset coil runtime fraction to 1.0
			}

			// If cycling fan, send coil part-load fraction to on/off fan via HVACDataGlobals
			if ( FanOpMode == CycFanCycCoil ) OnOffFanPartLoadFraction = PLF;

			//  Calculate full load output conditions
			if ( SHR > 1.0 || Counter > 0 ) SHR = 1.0;
			FullLoadOutAirEnth = InletAirEnthalpy - TotCap / AirMassFlow;
			hTinwout = InletAirEnthalpy - ( 1.0 - SHR ) * hDelta;
			if ( SHR < 1.0 ) {
				FullLoadOutAirHumRat = PsyWFnTdbH( InletAirDryBulbTemp, hTinwout );
			} else {
				FullLoadOutAirHumRat = InletAirHumRat;
			}
			FullLoadOutAirTemp = PsyTdbFnHW( FullLoadOutAirEnth, FullLoadOutAirHumRat );

			// Check for saturation error and modify temperature at constant enthalpy
			if ( FullLoadOutAirTemp < PsyTsatFnHPb( FullLoadOutAirEnth, OutdoorPressure ) ) {
				FullLoadOutAirTemp = PsyTsatFnHPb( FullLoadOutAirEnth, OutdoorPressure );
				//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
				//   IF(FullLoadOutAirTemp .LT. PsyTsatFnHPb(FullLoadOutAirEnth,InletAirPressure)) THEN
				//    FullLoadOutAirTemp = PsyTsatFnHPb(FullLoadOutAirEnth,InletAirPressure)
				FullLoadOutAirHumRat = PsyWFnTdbH( FullLoadOutAirTemp, FullLoadOutAirEnth );
			}

			// Store actual outlet conditions when DX coil is ON for use in heat recovery module
			DXCoilFullLoadOutAirTemp( DXCoilNum ) = FullLoadOutAirTemp;
			DXCoilFullLoadOutAirHumRat( DXCoilNum ) = FullLoadOutAirHumRat;

			// Add warning message for cold cooling coil (FullLoadOutAirTemp < 2 C)
			if ( FullLoadOutAirTemp < 2.0 && ! FirstHVACIteration && ! WarmupFlag ) {
				DXCoil( DXCoilNum ).PrintLowOutTempMessage = true;
				DXCoil( DXCoilNum ).FullLoadOutAirTempLast = FullLoadOutAirTemp;
				if ( DXCoil( DXCoilNum ).LowOutletTempIndex == 0 ) {
					DXCoil( DXCoilNum ).FullLoadInletAirTempLast = InletAirDryBulbTemp;
					DXCoil( DXCoilNum ).LowOutTempBuffer1 = DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + "\" - Full load outlet air dry-bulb temperature < 2C. This indicates the possibility of coil frost/freeze. Outlet temperature = " + RoundSigDigits( FullLoadOutAirTemp, 2 ) + " C.";
					DXCoil( DXCoilNum ).LowOutTempBuffer2 = " ...Occurrence info = " + EnvironmentName + ", " + CurMnDy + ' ' + CreateSysTimeIntervalString();
				}
			}

			//  If constant fan with cycling compressor, call function to determine "effective SHR"
			//  which includes the part-load degradation on latent capacity
			if ( FanOpMode == ContFanCycCoil && CompCycRatio < 1.0 ) {
				QLatRated = DXCoil( DXCoilNum ).RatedTotCap( Mode ) * ( 1.0 - DXCoil( DXCoilNum ).RatedSHR( Mode ) );
				QLatActual = TotCap * ( 1.0 - SHR );
				SHRUnadjusted = SHR;
				SHR = CalcEffectiveSHR( DXCoilNum, SHR, DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction, QLatRated, QLatActual, InletAirDryBulbTemp, InletAirWetBulbC, Mode );

				//  Calculate full load output conditions
				if ( SHR > 1.0 || Counter > 0 ) SHR = 1.0;
				FullLoadOutAirEnth = InletAirEnthalpy - TotCap / AirMassFlow;
				hTinwout = InletAirEnthalpy - ( 1.0 - SHR ) * hDelta;
				if ( SHR < 1.0 ) {
					FullLoadOutAirHumRat = PsyWFnTdbH( InletAirDryBulbTemp, hTinwout );
				} else {
					FullLoadOutAirHumRat = InletAirHumRat;
				}
				FullLoadOutAirTemp = PsyTdbFnHW( FullLoadOutAirEnth, FullLoadOutAirHumRat );

			}

			//  Calculate actual outlet conditions for the input part load ratio
			//  Actual outlet conditions are "average" for time step when compressor cycles

			if ( FanOpMode == ContFanCycCoil && CompCycRatio < 1.0 ) {
				// Continuous fan, cycling compressor
				OutletAirEnthalpy = ( ( PartLoadRatio * AirFlowRatio ) * FullLoadOutAirEnth + ( 1.0 - ( PartLoadRatio * AirFlowRatio ) ) * InletAirEnthalpy );
				OutletAirHumRat = ( ( PartLoadRatio * AirFlowRatio ) * FullLoadOutAirHumRat + ( 1.0 - ( PartLoadRatio * AirFlowRatio ) ) * InletAirHumRat );
				OutletAirTemp = PsyTdbFnHW( OutletAirEnthalpy, OutletAirHumRat );
			} else {
				// Default to cycling fan, cycling compressor
				OutletAirEnthalpy = FullLoadOutAirEnth;
				OutletAirHumRat = FullLoadOutAirHumRat;
				OutletAirTemp = FullLoadOutAirTemp;
			}

			// Check for saturation error and modify temperature at constant enthalpy
			if ( OutletAirTemp < PsyTsatFnHPb( OutletAirEnthalpy, OutdoorPressure, RoutineName ) ) {
				OutletAirTemp = PsyTsatFnHPb( OutletAirEnthalpy, OutdoorPressure );
				//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
				//   IF(OutletAirTemp .LT. PsyTsatFnHPb(OutletAirEnthalpy,InletAirPressure)) THEN
				//    OutletAirTemp = PsyTsatFnHPb(OutletAirEnthalpy,InletAirPressure)
				OutletAirHumRat = PsyWFnTdbH( OutletAirTemp, OutletAirEnthalpy );
			}

			// Reset AirMassFlow to inlet node air mass flow for final total, sensible and latent calculations
			// since AirMassFlow might have been modified above (in this subroutine):
			//     IF (FanOpMode .EQ. CycFanCycCoil) AirMassFlow = AirMassFlow / PartLoadRatio
			// For multimode coil, this should be full flow including bypassed fraction
			AirMassFlow = DXCoil( DXCoilNum ).InletAirMassFlowRate;
			DXCoil( DXCoilNum ).TotalCoolingEnergyRate = AirMassFlow * ( InletAirEnthalpy - OutletAirEnthalpy );

			//! Set DataHeatGlobal heat reclaim variable for use by heat reclaim coil (part load ratio is accounted for)
			//! Calculation for heat reclaim needs to be corrected to use compressor power (not including condenser fan power)
			//  HeatReclaimDXCoil(DXCoilNum)%AvailCapacity = DXCoil(DXCoilNum)%TotalCoolingEnergyRate + DXCoil(DXCoilNum)%ElecCoolingPower

			MinAirHumRat = min( InletAirHumRat, OutletAirHumRat );
			DXCoil( DXCoilNum ).SensCoolingEnergyRate = AirMassFlow * ( PsyHFnTdbW( InletAirDryBulbTemp, MinAirHumRat ) - PsyHFnTdbW( OutletAirTemp, MinAirHumRat ) );
			//  Don't let sensible capacity be greater than total capacity
			if ( DXCoil( DXCoilNum ).SensCoolingEnergyRate > DXCoil( DXCoilNum ).TotalCoolingEnergyRate ) {
				DXCoil( DXCoilNum ).SensCoolingEnergyRate = DXCoil( DXCoilNum ).TotalCoolingEnergyRate;
			}
			DXCoil( DXCoilNum ).LatCoolingEnergyRate = DXCoil( DXCoilNum ).TotalCoolingEnergyRate - DXCoil( DXCoilNum ).SensCoolingEnergyRate;
			DXCoil( DXCoilNum ).OutletAirTemp = OutletAirTemp;
			DXCoil( DXCoilNum ).OutletAirHumRat = OutletAirHumRat;
			DXCoil( DXCoilNum ).OutletAirEnthalpy = OutletAirEnthalpy;

		} else {

			// DX coil is off; just pass through conditions
			DXCoil( DXCoilNum ).OutletAirEnthalpy = DXCoil( DXCoilNum ).InletAirEnthalpy;
			DXCoil( DXCoilNum ).OutletAirHumRat = DXCoil( DXCoilNum ).InletAirHumRat;
			DXCoil( DXCoilNum ).OutletAirTemp = DXCoil( DXCoilNum ).InletAirTemp;

			DXCoil( DXCoilNum ).ElecCoolingPower = 0.0;
			DXCoil( DXCoilNum ).TotalCoolingEnergyRate = 0.0;
			DXCoil( DXCoilNum ).SensCoolingEnergyRate = 0.0;
			DXCoil( DXCoilNum ).LatCoolingEnergyRate = 0.0;
			DXCoil( DXCoilNum ).EvapCondPumpElecPower = 0.0;
			DXCoil( DXCoilNum ).EvapWaterConsumpRate = 0.0;

			// Reset globals when DX coil is OFF for use in heat recovery module
			DXCoilFullLoadOutAirTemp( DXCoilNum ) = 0.0;
			DXCoilFullLoadOutAirHumRat( DXCoilNum ) = 0.0;

		} // end of on/off if - else

		//set water system demand request (if needed)
		if ( DXCoil( DXCoilNum ).EvapWaterSupplyMode == WaterSupplyFromTank ) {
			WaterStorage( DXCoil( DXCoilNum ).EvapWaterSupTankID ).VdotRequestDemand( DXCoil( DXCoilNum ).EvapWaterTankDemandARRID ) = DXCoil( DXCoilNum ).EvapWaterConsumpRate;
		}

		DXCoilOutletTemp( DXCoilNum ) = DXCoil( DXCoilNum ).OutletAirTemp;
		DXCoilOutletHumRat( DXCoilNum ) = DXCoil( DXCoilNum ).OutletAirHumRat;
		DXCoilPartLoadRatio( DXCoilNum ) = DXCoil( DXCoilNum ).PartLoadRatio;
		DXCoilFanOpMode( DXCoilNum ) = FanOpMode;
		DXCoil( DXCoilNum ).CondInletTemp = CondInletTemp;
		DXCoilTotalCooling( DXCoilNum ) = DXCoil( DXCoilNum ).TotalCoolingEnergyRate;
		DXCoilCoolInletAirWBTemp( DXCoilNum ) = PsyTwbFnTdbWPb( InletAirDryBulbTemp, InletAirHumRat, OutdoorPressure );

	}

	void
	CalcDXHeatingCoil(
		int const DXCoilNum, // the number of the DX heating coil to be simulated
		Real64 const PartLoadRatio, // sensible cooling load / full load sensible cooling capacity
		int const FanOpMode, // Allows parent object to control fan mode
		Optional< Real64 const > OnOffAirFlowRatio, // ratio of compressor on airflow to compressor off airflow
		Optional< Real64 const > MaxHeatCap // maximum allowed heating capacity
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   October 2001
		//       MODIFIED       Raustad/Shirey Mar 2004
		//                      Kenneth Tang 2004 (Sensitivity of TotCapTempModFac & EIRTempModFac  to indoor dry bulb temp)
		//                      Feb 2005 M. J. Witte, GARD Analytics, Inc.
		//                        Add new coil type COIL:DX:MultiMode:CoolingEmpirical:
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the air-side heating performance and electrical heating energy
		// use of a direct-expansion, air-cooled heat pump unit.

		// METHODOLOGY EMPLOYED:
		// This routine simulates the performance of air-cooled DX heating equipment.
		// The routine requires the user to enter the total heating capacity
		// and COP for the unit at ARI 210/240 rating conditions (21.11C [70F] dry-bulb,
		// 15.55C [60F] wet-bulb air entering the heating coil, 8.33C [47F] dry-bulb,
		// 6.11C [43F] wet-bulb air entering the outdoor condenser. Since different
		// manufacturer's rate their equipment at different air flow rates, the supply
		// air flow rate corresponding to the rated capacities and rated COP must also
		// be entered (should be between 300 cfm/ton and 450 cfm/ton). The rated information
		// entered by the user should NOT include the thermal or electrical impacts of the
		// supply air fan, as this is addressed by another module.

		// With the rated performance data entered by the user, the model employs some of the
		// DOE-2.1E curve fits to adjust the capacity and efficiency of the unit as a function
		// of outdoor air temperatures and supply air flow rate (actual vs rated flow). The
		// model does NOT employ the exact same methodology to calculate performance as DOE-2,
		// although some of the DOE-2 curve fits are employed by this model.

		// REFERENCES:
		// Winkelmann, F.C., Birdsall, B.E., Buhl W.F., Ellington, K.L., Erdem, A.E. 1993.
		// DOE-2 Supplement Version 2.1E.  Energy and Environment Division, Larwence Berkely
		// Laboratory.
		// Henderson, H.I. Jr., Y.J. Huang and Danny Parker. 1999. Residential Equipment Part
		// Load Curves for Use in DOE-2.  Environmental Energy Technologies Division, Ernest
		// Orlando Lawrence Berkeley National Laboratory.

		// Using/Aliasing
		using CurveManager::CurveValue;
		using General::RoundSigDigits;
		using DataHeatBalFanSys::ZoneAirHumRat;
		using DataHeatBalFanSys::ZT;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcDXHeatingCoil" );
		static std::string const RoutineNameFullLoad( "CalcDXHeatingCoil:fullload" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 AirMassFlow; // dry air mass flow rate through coil [kg/s]
		Real64 AirMassFlowRatio; // Ratio of actual air mass flow to rated air mass flow
		Real64 AirVolumeFlowRate; // Air volume flow rate across the cooling coil [m3/s]
		Real64 VolFlowperRatedTotCap; // Air volume flow rate divided by rated total cooling capacity [m3/s-W]
		Real64 TotCap; // gross total cooling capacity at off-rated conditions [W]
		Real64 TotCapAdj; // adjusted total cooling capacity at off-rated conditions [W]
		Real64 TotCapTempModFac; // Total capacity modifier (function of entering drybulb, outside drybulb) depending
		// on the type of curve
		Real64 TotCapFlowModFac; // Total capacity modifier (function of actual supply air flow vs rated flow)
		Real64 InletAirDryBulbTemp; // inlet air dry bulb temperature [C]
		Real64 InletAirWetBulbC; // wetbulb temperature of inlet air [C]
		Real64 InletAirEnthalpy; // inlet air enthalpy [J/kg]
		Real64 InletAirHumRat; // inlet air humidity ratio [kg/kg]
		//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
		//REAL(r64)     :: InletAirPressure            ! inlet air pressure [Pa]
		Real64 FullLoadOutAirEnth; // outlet full load enthalpy [J/kg]
		Real64 FullLoadOutAirHumRat; // outlet humidity ratio at full load
		Real64 FullLoadOutAirTemp; // outlet air temperature at full load [C]
		Real64 FullLoadOutAirRH; // outlet air relative humidity at full load
		Real64 EIRTempModFac( 0.0 ); // EIR modifier (function of entering drybulb, outside drybulb) depending on the
		// type of curve
		Real64 DefrostEIRTempModFac; // EIR modifier for defrost (function of entering wetbulb, outside drybulb)
		Real64 EIRFlowModFac; // EIR modifier (function of actual supply air flow vs rated flow)
		Real64 EIR; // EIR at part load and off rated conditions
		Real64 PLF; // Part load factor, accounts for thermal lag at compressor startup
		Real64 PLRHeating; // PartLoadRatio in heating
		Real64 OutdoorCoilT; // Outdoor coil temperature (C)
		Real64 OutdoorCoildw; // Outdoor coil delta w assuming coil temp of OutdoorCoilT (kg/kg)
		Real64 FractionalDefrostTime; // Fraction of time step system is in defrost
		Real64 HeatingCapacityMultiplier; // Multiplier for heating capacity when system is in defrost
		Real64 InputPowerMultiplier; // Multiplier for power when system is in defrost
		Real64 LoadDueToDefrost; // Additional load due to defrost
		Real64 CrankcaseHeatingPower; // power due to crankcase heater
		Real64 OutdoorDryBulb; // Outdoor dry-bulb temperature at condenser (C)
		Real64 OutdoorWetBulb; // Outdoor wet-bulb temperature at condenser (C)
		Real64 OutdoorHumRat; // Outdoor humidity ratio at condenser (kg/kg)
		Real64 OutdoorPressure; // Outdoor barometric pressure at condenser (Pa)
		static int Mode( 1 ); // Performance mode for MultiMode DX coil; Always 1 for other coil types
		Real64 AirFlowRatio; // Ratio of compressor on airflow to average timestep airflow
		Real64 OutletAirTemp; // Supply air temperature (average value if constant fan, full output if cycling fan)
		Real64 OutletAirHumRat; // Supply air humidity ratio (average value if constant fan, full output if cycling fan)
		Real64 OutletAirEnthalpy; // Supply air enthalpy (average value if constant fan, full output if cycling fan)

		if ( present( OnOffAirFlowRatio ) ) {
			AirFlowRatio = OnOffAirFlowRatio;
		} else {
			AirFlowRatio = 1.0;
		}

		// Get condenser outdoor node info from DX Heating Coil
		if ( DXCoil( DXCoilNum ).CondenserInletNodeNum( 1 ) != 0 ) {
			OutdoorDryBulb = Node( DXCoil( DXCoilNum ).CondenserInletNodeNum( 1 ) ).Temp;
			if ( DXCoil( DXCoilNum ).CondenserType( Mode ) == WaterCooled ) {
				OutdoorHumRat = OutHumRat;
				OutdoorPressure = OutBaroPress;
				OutdoorWetBulb = OutWetBulbTemp;
			} else {
				OutdoorPressure = Node( DXCoil( DXCoilNum ).CondenserInletNodeNum( 1 ) ).Press;
				// If node is not connected to anything, pressure = default, use weather data
				if ( OutdoorPressure == DefaultNodeValues.Press ) {
					OutdoorDryBulb = OutDryBulbTemp;
					OutdoorHumRat = OutHumRat;
					OutdoorPressure = OutBaroPress;
					OutdoorWetBulb = OutWetBulbTemp;
				} else {
					OutdoorHumRat = Node( DXCoil( DXCoilNum ).CondenserInletNodeNum( 1 ) ).HumRat;
					// this should use Node%WetBulbTemp or a PSYC function, not OAWB
					OutdoorWetBulb = Node( DXCoil( DXCoilNum ).CondenserInletNodeNum( 1 ) ).OutAirWetBulb;
				}
				if ( DXCoil( DXCoilNum ).IsSecondaryDXCoilInZone ) {
					OutdoorDryBulb = ZT( DXCoil( DXCoilNum ).SecZonePtr );
					OutdoorHumRat = ZoneAirHumRat( DXCoil( DXCoilNum ).SecZonePtr );
					OutdoorWetBulb = DXCoil( DXCoilNum ).EvapInletWetBulb;
					OutdoorPressure = OutBaroPress;
				}
			}
		} else if ( DXCoil( DXCoilNum ).IsSecondaryDXCoilInZone ) {
			OutdoorDryBulb = ZT( DXCoil( DXCoilNum ).SecZonePtr );
			OutdoorHumRat = ZoneAirHumRat( DXCoil( DXCoilNum ).SecZonePtr );
			OutdoorWetBulb = DXCoil( DXCoilNum ).EvapInletWetBulb;
			OutdoorPressure = OutBaroPress;
		} else {
			OutdoorDryBulb = OutDryBulbTemp;
			OutdoorHumRat = OutHumRat;
			OutdoorPressure = OutBaroPress;
			OutdoorWetBulb = OutWetBulbTemp;
		}

		AirMassFlow = DXCoil( DXCoilNum ).InletAirMassFlowRate;
		InletAirDryBulbTemp = DXCoil( DXCoilNum ).InletAirTemp;
		InletAirEnthalpy = DXCoil( DXCoilNum ).InletAirEnthalpy;
		InletAirHumRat = DXCoil( DXCoilNum ).InletAirHumRat;
		//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
		//InletAirPressure = DXCoil(DXCoilNum)%InletAirPressure
		//InletAirWetBulbC = PsyTwbFnTdbWPb(InletAirDryBulbTemp,InletAirHumRat,InletAirPressure)
		InletAirWetBulbC = PsyTwbFnTdbWPb( InletAirDryBulbTemp, InletAirHumRat, OutdoorPressure );
		PLRHeating = 0.0;
		DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction = 0.0;
		// Initialize crankcase heater, operates below OAT defined in input deck for HP DX heating coil
		if ( OutdoorDryBulb < DXCoil( DXCoilNum ).MaxOATCrankcaseHeater ) {
			CrankcaseHeatingPower = DXCoil( DXCoilNum ).CrankcaseHeaterCapacity;
		} else {
			CrankcaseHeatingPower = 0.0;
		}

		if ( ( AirMassFlow > 0.0 ) && ( GetCurrentScheduleValue( DXCoil( DXCoilNum ).SchedPtr ) > 0.0 ) && ( PartLoadRatio > 0.0 ) && OutdoorDryBulb > DXCoil( DXCoilNum ).MinOATCompressor ) {
			// for cycling fan, reset mass flow to full on rate
			if ( FanOpMode == CycFanCycCoil ) AirMassFlow /= PartLoadRatio;
			if ( FanOpMode == ContFanCycCoil ) AirMassFlow *= AirFlowRatio;
			// Check for valid air volume flow per rated total cooling capacity (200 - 600 cfm/ton)
			AirVolumeFlowRate = AirMassFlow / PsyRhoAirFnPbTdbW( OutdoorPressure, InletAirDryBulbTemp, InletAirHumRat );
			//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
			//  AirVolumeFlowRate = AirMassFlow/PsyRhoAirFnPbTdbW(InletAirPressure,InletAirDryBulbTemp, InletAirHumRat)
			VolFlowperRatedTotCap = AirVolumeFlowRate / DXCoil( DXCoilNum ).RatedTotCap( Mode );

			if ( ( VolFlowperRatedTotCap < MinOperVolFlowPerRatedTotCap( DXCT ) ) || ( VolFlowperRatedTotCap > MaxHeatVolFlowPerRatedTotCap( DXCT ) ) ) {
				if ( DXCoil( DXCoilNum ).ErrIndex1 == 0 ) {
					ShowWarningMessage( DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + "\" - Air volume flow rate per watt of rated total heating capacity is out of range at " + RoundSigDigits( VolFlowperRatedTotCap, 3 ) + " m3/s/W." );
					ShowContinueErrorTimeStamp( "" );
					ShowContinueError( "Expected range for VolumeFlowPerRatedTotalCapacity=[" + RoundSigDigits( MinOperVolFlowPerRatedTotCap( DXCT ), 3 ) + "--" + RoundSigDigits( MaxHeatVolFlowPerRatedTotCap( DXCT ), 3 ) + ']' );
					ShowContinueError( "Possible causes include inconsistent air flow rates in system components or" );
					ShowContinueError( "inconsistent supply air fan operation modes in coil and unitary system objects." );
				}
				ShowRecurringWarningErrorAtEnd( DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + "\" - Air volume flow rate per watt of rated total heating capacity is out of range error continues...", DXCoil( DXCoilNum ).ErrIndex1, VolFlowperRatedTotCap, VolFlowperRatedTotCap );
			}

			// Get total capacity modifying factor (function of temperature) for off-rated conditions
			// Model was extended to accept bi-quadratic curves. This allows sensitivity of the heating capacity
			// to the entering dry-bulb temperature as well as the outside dry-bulb temperature. User is
			// advised to use the bi-quaratic curve if sufficient manufacturer data is available.
			if ( DXCoil( DXCoilNum ).TotCapTempModFacCurveType( Mode ) == BiQuadratic ) {
				{ auto const SELECT_CASE_var( DXCoil( DXCoilNum ).HeatingPerformanceOATType );
				if ( SELECT_CASE_var == DryBulbIndicator ) {
					TotCapTempModFac = CurveValue( DXCoil( DXCoilNum ).CCapFTemp( Mode ), InletAirDryBulbTemp, OutdoorDryBulb );
				} else if ( SELECT_CASE_var == WetBulbIndicator ) {
					TotCapTempModFac = CurveValue( DXCoil( DXCoilNum ).CCapFTemp( Mode ), InletAirDryBulbTemp, OutdoorWetBulb );
				} else {
					TotCapTempModFac = 1.0;
				}}
			} else {
				{ auto const SELECT_CASE_var( DXCoil( DXCoilNum ).HeatingPerformanceOATType );
				if ( SELECT_CASE_var == DryBulbIndicator ) {
					if ( DXCoil( DXCoilNum ).DXCoilType_Num != CoilVRF_Heating && DXCoil( DXCoilNum ).DXCoilType_Num != CoilVRF_FluidTCtrl_Heating ) {
						TotCapTempModFac = CurveValue( DXCoil( DXCoilNum ).CCapFTemp( Mode ), OutdoorDryBulb );
					} else {
						TotCapTempModFac = CurveValue( DXCoil( DXCoilNum ).CCapFTemp( Mode ), InletAirDryBulbTemp );
					}
				} else if ( SELECT_CASE_var == WetBulbIndicator ) {
					if ( DXCoil( DXCoilNum ).DXCoilType_Num != CoilVRF_Heating && DXCoil( DXCoilNum ).DXCoilType_Num != CoilVRF_FluidTCtrl_Heating ) {
						TotCapTempModFac = CurveValue( DXCoil( DXCoilNum ).CCapFTemp( Mode ), OutdoorWetBulb );
					} else {
						TotCapTempModFac = CurveValue( DXCoil( DXCoilNum ).CCapFTemp( Mode ), InletAirDryBulbTemp );
					}
				} else {
					TotCapTempModFac = 1.0;
				}}
			}

			//  Get total capacity modifying factor (function of mass flow) for off-rated conditions
			AirMassFlowRatio = AirMassFlow / DXCoil( DXCoilNum ).RatedAirMassFlowRate( Mode );
			TotCapFlowModFac = CurveValue( DXCoil( DXCoilNum ).CCapFFlow( Mode ), AirMassFlowRatio );

			// Calculate total heating capacity for off-rated conditions
			TotCap = DXCoil( DXCoilNum ).RatedTotCap( Mode ) * TotCapFlowModFac * TotCapTempModFac;

			// Calculating adjustment factors for defrost
			// Calculate delta w through outdoor coil by assuming a coil temp of 0.82*DBT-9.7(F) per DOE2.1E
			OutdoorCoilT = 0.82 * OutdoorDryBulb - 8.589;
			OutdoorCoildw = max( 1.0e-6, ( OutdoorHumRat - PsyWFnTdpPb( OutdoorCoilT, OutdoorPressure ) ) );

			// Initializing defrost adjustment factors
			LoadDueToDefrost = 0.0;
			HeatingCapacityMultiplier = 1.0;
			FractionalDefrostTime = 0.0;
			InputPowerMultiplier = 1.0;

			// Check outdoor temperature to determine of defrost is active
			if( OutdoorDryBulb <= DXCoil( DXCoilNum ).MaxOATDefrost && DXCoil( DXCoilNum ).CondenserType( Mode ) != WaterCooled ) {
				// Calculate defrost adjustment factors depending on defrost control type
				if ( DXCoil( DXCoilNum ).DefrostControl == Timed ) {
					FractionalDefrostTime = DXCoil( DXCoilNum ).DefrostTime;
					if ( FractionalDefrostTime > 0.0 ) {
						HeatingCapacityMultiplier = 0.909 - 107.33 * OutdoorCoildw;
						InputPowerMultiplier = 0.90 - 36.45 * OutdoorCoildw;
					}
				} else { //else defrost control is on-demand
					FractionalDefrostTime = 1.0 / ( 1.0 + 0.01446 / OutdoorCoildw );
					HeatingCapacityMultiplier = 0.875 * ( 1.0 - FractionalDefrostTime );
					InputPowerMultiplier = 0.954 * ( 1.0 - FractionalDefrostTime );
				}

				if ( FractionalDefrostTime > 0.0 ) {
					// Calculate defrost adjustment factors depending on defrost control strategy
					if ( DXCoil( DXCoilNum ).DefrostStrategy == ReverseCycle ) {
						LoadDueToDefrost = ( 0.01 * FractionalDefrostTime ) * ( 7.222 - OutdoorDryBulb ) * ( DXCoil( DXCoilNum ).RatedTotCap( Mode ) / 1.01667 );
						DefrostEIRTempModFac = CurveValue( DXCoil( DXCoilNum ).DefrostEIRFT, max( 15.555, InletAirWetBulbC ), max( 15.555, OutdoorDryBulb ) );
						DXCoil( DXCoilNum ).DefrostPower = DefrostEIRTempModFac * ( DXCoil( DXCoilNum ).RatedTotCap( Mode ) / 1.01667 ) * FractionalDefrostTime;
					} else { // Defrost strategy is resistive
						DXCoil( DXCoilNum ).DefrostPower = DXCoil( DXCoilNum ).DefrostCapacity * FractionalDefrostTime;
					}
				} else { // Defrost is not active because (FractionalDefrostTime .EQ. 0.0)
					DXCoil( DXCoilNum ).DefrostPower = 0.0;
				}
			}

			// Modify total heating capacity based on defrost heating capacity multiplier
			// MaxHeatCap passed from parent object VRF Condenser and is used to limit capacity of TU's to that available from condenser
			if ( present( MaxHeatCap ) ) {
				TotCapAdj = min( MaxHeatCap, TotCap * HeatingCapacityMultiplier );
				TotCap = min( MaxHeatCap, TotCap );
			} else {
				TotCapAdj = TotCap * HeatingCapacityMultiplier;
			}

			// Calculate full load outlet conditions
			FullLoadOutAirEnth = InletAirEnthalpy + TotCapAdj / AirMassFlow;
			FullLoadOutAirHumRat = InletAirHumRat;
			FullLoadOutAirTemp = PsyTdbFnHW( FullLoadOutAirEnth, FullLoadOutAirHumRat );
			FullLoadOutAirRH = PsyRhFnTdbWPb( FullLoadOutAirTemp, FullLoadOutAirHumRat, OutdoorPressure, RoutineNameFullLoad );
			//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
			//  FullLoadOutAirRH = PsyRhFnTdbWPb(FullLoadOutAirTemp,FullLoadOutAirHumRat,InletAirPressure)
			if ( FullLoadOutAirRH > 1.0 ) { // Limit to saturated conditions at FullLoadOutAirEnth
				FullLoadOutAirTemp = PsyTsatFnHPb( FullLoadOutAirEnth, OutdoorPressure );
				//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
				//    FullLoadOutAirTemp = PsyTsatFnHPb(FullLoadOutAirEnth,InletAirPressure)
				FullLoadOutAirHumRat = PsyWFnTdbH( FullLoadOutAirTemp, FullLoadOutAirEnth );
			}

			// Calculate actual outlet conditions for the input part load ratio
			// Actual outlet conditions are "average" for time step
			if ( FanOpMode == ContFanCycCoil ) {
				// continuous fan, cycling compressor
				OutletAirEnthalpy = ( ( PartLoadRatio * AirFlowRatio ) * FullLoadOutAirEnth + ( 1.0 - ( PartLoadRatio * AirFlowRatio ) ) * InletAirEnthalpy );
				OutletAirHumRat = ( PartLoadRatio * FullLoadOutAirHumRat + ( 1.0 - PartLoadRatio ) * InletAirHumRat );
				OutletAirTemp = PsyTdbFnHW( OutletAirEnthalpy, OutletAirHumRat );
			} else {
				// default to cycling fan, cycling compressor
				OutletAirEnthalpy = FullLoadOutAirEnth;
				OutletAirHumRat = FullLoadOutAirHumRat;
				OutletAirTemp = FullLoadOutAirTemp;
			}
			// Calculate electricity consumed. First, get EIR modifying factors for off-rated conditions
			// Model was extended to accept bi-quadratic curves. This allows sensitivity of the EIR
			// to the entering dry-bulb temperature as well as the outside dry-bulb temperature. User is
			// advised to use the bi-quaratic curve if sufficient manufacturer data is available.
			if ( DXCoil( DXCoilNum ).DXCoilType_Num != CoilVRF_Heating && DXCoil( DXCoilNum ).DXCoilType_Num != CoilVRF_FluidTCtrl_Heating ) {
				if ( ( DXCoil( DXCoilNum ).EIRTempModFacCurveType( 1 ) == Quadratic ) || ( DXCoil( DXCoilNum ).EIRTempModFacCurveType( 1 ) == Cubic ) ) {
					EIRTempModFac = CurveValue( DXCoil( DXCoilNum ).EIRFTemp( Mode ), OutdoorDryBulb );
				} else if ( DXCoil( DXCoilNum ).EIRTempModFacCurveType( 1 ) == BiQuadratic ) {
					EIRTempModFac = CurveValue( DXCoil( DXCoilNum ).EIRFTemp( Mode ), InletAirDryBulbTemp, OutdoorDryBulb );
				} else {
					assert( false );
				}
				EIRFlowModFac = CurveValue( DXCoil( DXCoilNum ).EIRFFlow( Mode ), AirMassFlowRatio );
			} else {
				EIRTempModFac = 1.0;
				EIRFlowModFac = 1.0;
			}
			EIR = DXCoil( DXCoilNum ).RatedEIR( Mode ) * EIRTempModFac * EIRFlowModFac;
			// Calculate modified PartLoadRatio due to defrost (reverse-cycle defrost only)
			PLRHeating = min( 1.0, ( PartLoadRatio + ( LoadDueToDefrost * PartLoadRatio ) / TotCapAdj ) );
			if ( DXCoil( DXCoilNum ).DXCoilType_Num != CoilVRF_Heating ) {
				PLF = CurveValue( DXCoil( DXCoilNum ).PLFFPLR( Mode ), PLRHeating ); // Calculate part-load factor
			} else {
				PLF = 1.0;
			}

			if ( PLF < 0.7 ) {
				if ( DXCoil( DXCoilNum ).PLRErrIndex == 0 ) {
					ShowWarningMessage( "The PLF curve value for DX heating coil " + DXCoil( DXCoilNum ).Name + " =" + RoundSigDigits( PLF, 2 ) + " for part-load ratio =" + RoundSigDigits( PLRHeating, 2 ) );
					ShowContinueError( "PLF curve values must be >= 0.7. PLF has been reset to 0.7 and simulation is continuing." );
					ShowContinueError( "Check the IO reference manual for PLF curve guidance [Coil:Heating:DX:SingleSpeed]." );
					ShowContinueErrorTimeStamp( "" );
				}
				ShowRecurringWarningErrorAtEnd( "DX heating coil PLF curve < 0.7 warning continues... ", DXCoil( DXCoilNum ).PLRErrIndex, PLF, PLF );
				PLF = 0.7;
			}

			DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction = ( PLRHeating / PLF );
			if ( DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction > 1.0 && std::abs( DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction - 1.0 ) > 0.001 ) {
				if ( DXCoil( DXCoilNum ).ErrIndex4 == 0 ) {
					ShowWarningMessage( "The runtime fraction for DX heating coil " + DXCoil( DXCoilNum ).Name + " exceeded 1.0. [" + RoundSigDigits( DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction, 4 ) + "]." );
					ShowContinueError( "Runtime fraction is set to 1.0 and the simulation continues..." );
					ShowContinueError( "Check the IO reference manual for PLF curve guidance [Coil:Heating:DX:SingleSpeed]." );
					ShowContinueErrorTimeStamp( "" );
				}
				ShowRecurringWarningErrorAtEnd( DXCoil( DXCoilNum ).Name + ", DX heating coil runtime fraction > 1.0 warning continues...", DXCoil( DXCoilNum ).ErrIndex4, DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction, DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction );
				DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction = 1.0; // Reset coil runtime fraction to 1.0
			} else if ( DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction > 1.0 ) {
				DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction = 1.0; // Reset coil runtime fraction to 1.0
			}
			// if cycling fan, send coil part-load fraction to on/off fan via HVACDataGlobals
			if ( FanOpMode == CycFanCycCoil ) OnOffFanPartLoadFraction = PLF;
			DXCoil( DXCoilNum ).ElecHeatingPower = TotCap * EIR * DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction * InputPowerMultiplier;

			// Calculate crankcase heater power using the runtime fraction for this DX heating coil only if there is no companion DX coil.
			// Else use the largest runtime fraction of this DX heating coil and the companion DX cooling coil.

			if ( DXCoil( DXCoilNum ).CompanionUpstreamDXCoil == 0 ) {
				DXCoil( DXCoilNum ).CrankcaseHeaterPower = CrankcaseHeatingPower * ( 1.0 - DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction );
			} else {
				DXCoil( DXCoilNum ).CrankcaseHeaterPower = CrankcaseHeatingPower * ( 1.0 - max( DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction, DXCoil( DXCoil( DXCoilNum ).CompanionUpstreamDXCoil ).CoolingCoilRuntimeFraction ) );
			}

			AirMassFlow = DXCoil( DXCoilNum ).InletAirMassFlowRate;
			DXCoil( DXCoilNum ).TotalHeatingEnergyRate = AirMassFlow * ( OutletAirEnthalpy - InletAirEnthalpy );
			// Adjust defrost power to correct for DOE-2 bug where defrost power is constant regardless of compressor runtime fraction
			// Defrosts happen based on compressor run time (frost buildup on outdoor coil), not total elapsed time.
			DXCoil( DXCoilNum ).DefrostPower *= DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction;

			DXCoil( DXCoilNum ).OutletAirTemp = OutletAirTemp;
			DXCoil( DXCoilNum ).OutletAirHumRat = OutletAirHumRat;
			DXCoil( DXCoilNum ).OutletAirEnthalpy = OutletAirEnthalpy;
			DXCoil( DXCoilNum ).CompressorPartLoadRatio = PartLoadRatio;

		} else {

			// DX coil is off; just pass through conditions
			DXCoil( DXCoilNum ).OutletAirEnthalpy = DXCoil( DXCoilNum ).InletAirEnthalpy;
			DXCoil( DXCoilNum ).OutletAirHumRat = DXCoil( DXCoilNum ).InletAirHumRat;
			DXCoil( DXCoilNum ).OutletAirTemp = DXCoil( DXCoilNum ).InletAirTemp;

			DXCoil( DXCoilNum ).ElecHeatingPower = 0.0;
			DXCoil( DXCoilNum ).TotalHeatingEnergyRate = 0.0;
			DXCoil( DXCoilNum ).DefrostPower = 0.0;

			// Calculate crankcase heater power using the runtime fraction for this DX heating coil (here DXHeatingCoilRTF=0) if
			// there is no companion DX coil, or the runtime fraction of the companion DX cooling coil (here DXCoolingCoilRTF>=0).
			if ( DXCoil( DXCoilNum ).CompanionUpstreamDXCoil == 0 ) {
				DXCoil( DXCoilNum ).CrankcaseHeaterPower = CrankcaseHeatingPower;
			} else {
				DXCoil( DXCoilNum ).CrankcaseHeaterPower = CrankcaseHeatingPower * ( 1.0 - DXCoil( DXCoil( DXCoilNum ).CompanionUpstreamDXCoil ).CoolingCoilRuntimeFraction );
			}
			DXCoil( DXCoilNum ).CompressorPartLoadRatio = 0.0;

		} // end of on/off if - else

		DXCoilOutletTemp( DXCoilNum ) = DXCoil( DXCoilNum ).OutletAirTemp;
		DXCoilOutletHumRat( DXCoilNum ) = DXCoil( DXCoilNum ).OutletAirHumRat;
		DXCoilFanOpMode( DXCoilNum ) = FanOpMode;
		DXCoilPartLoadRatio( DXCoilNum ) = PLRHeating;
		DXCoilTotalHeating( DXCoilNum ) = DXCoil( DXCoilNum ).TotalHeatingEnergyRate;
		DXCoilHeatInletAirDBTemp( DXCoilNum ) = InletAirDryBulbTemp;
		DXCoilHeatInletAirWBTemp( DXCoilNum ) = InletAirWetBulbC;

		// calc secondary coil if specified
		if ( DXCoil( DXCoilNum ).IsSecondaryDXCoilInZone ) {
			CalcSecondaryDXCoils( DXCoilNum );
		}

	}

	void
	CalcMultiSpeedDXCoil(
		int const DXCoilNum, // the number of the DX heating coil to be simulated
		Real64 const SpeedRatio, // = (CompressorSpeed - CompressorSpeedMin) / (CompressorSpeedMax - CompressorSpeedMin)
		Real64 const CycRatio, // cycling part load ratio
		Optional_bool_const ForceOn
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   September 2002
		//       MODIFIED       Raustad/Shirey, Feb 2004
		//                      Feb 2005 M. J. Witte, GARD Analytics, Inc.
		//                        Add new coil type COIL:DX:MultiMode:CoolingEmpirical:
		//                      April 2010, Chandan sharma, FSEC, added basin heater
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the air-side performance and electrical energy use of a direct-
		// expansion, air-cooled cooling unit with a 2 speed or variable speed compressor.

		// METHODOLOGY EMPLOYED:
		// Uses the same methodology as the single speed DX unit model (SUBROUTINE CalcDoe2DXCoil).
		// In addition it assumes that the unit performance is obtained by interpolating between
		// the performance at high speed and that at low speed. If the output needed is below
		// that produced at low speed, the compressor cycles between off and low speed.

		// Using/Aliasing
		using CurveManager::CurveValue;
		using DataWater::WaterStorage;
		using DataHeatBalFanSys::ZoneAirHumRat;
		using DataHeatBalFanSys::ZT;
		//USE ScheduleManager, ONLY: GetCurrentScheduleValue

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// SpeedRatio varies between 1.0 (maximum speed) and 0.0 (minimum speed)

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcMultiSpeedDXCoil" );
		static std::string const RoutineNameHighSpeedOutlet( "CalcMultiSpeedDXCoil:highspeedoutlet" );
		static std::string const RoutineNameLowSpeedOutlet( "CalcMultiSpeedDXCoil:lowspeedoutlet" );
		static std::string const RoutineNameNewDewPointConditions( "CalcMultiSpeedDXCoil:newdewpointconditions" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 AirMassFlow; // dry air mass flow rate through coil [kg/s]
		Real64 AirMassFlowRatio; // Ratio of max air mass flow to rated air mass flow
		Real64 InletAirWetBulbC; // wetbulb temperature of inlet air [C]
		Real64 InletAirDryBulbTemp; // inlet air dry bulb temperature [C]
		Real64 InletAirEnthalpy; // inlet air enthalpy [J/kg]
		Real64 InletAirHumRat; // inlet air humidity ratio [kg/kg]
		//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
		//REAL(r64) :: InletAirPressure    ! inlet air pressure [Pa]
		Real64 OutletAirDryBulbTemp; // outlet air dry bulb temperature [C]
		Real64 OutletAirEnthalpy; // outlet air enthalpy [J/kg]
		Real64 OutletAirHumRat; // outlet air humidity ratio [kg/kg]
		// REAL(r64) :: OutletAirRH         ! outlet air relative humudity [fraction]
		Real64 OutletAirDryBulbTempSat; // outlet air dry bulb temp at saturation at the outlet enthalpy [C]
		Real64 LSOutletAirDryBulbTemp; // low speed outlet air dry bulb temperature [C]
		Real64 LSOutletAirEnthalpy; // low speed outlet air enthalpy [J/kg]
		Real64 LSOutletAirHumRat; // low speed outlet air humidity ratio [kg/kg]
		Real64 LSOutletAirRH; // low speed outlet air relative humudity [fraction]
		Real64 hDelta; // Change in air enthalpy across the cooling coil [J/kg]
		Real64 hTinwout; // Enthalpy at inlet dry-bulb and outlet humidity ratio [J/kg]
		Real64 hADP; // Apparatus dew point enthalpy [J/kg]
		Real64 tADP; // Apparatus dew point temperature [C]
		Real64 wADP; // Apparatus dew point humidity ratio [kg/kg]
		Real64 hTinwADP; // Enthalpy at inlet dry-bulb and wADP [J/kg]
		Real64 RatedCBFHS; // coil bypass factor at rated conditions (high speed)
		Real64 CBFHS; // coil bypass factor at max flow (high speed)
		Real64 TotCapHS; // total capacity at high speed [W]
		Real64 SHRHS; // sensible heat ratio at high speed
		Real64 TotCapLS; // total capacity at low speed [W]
		Real64 SHRLS; // sensible heat ratio at low speed
		Real64 EIRTempModFacHS; // EIR modifier (function of entering wetbulb, outside drybulb) (high speed)
		Real64 EIRFlowModFacHS; // EIR modifier (function of actual supply air flow vs rated flow) (high speed)
		Real64 EIRHS; // EIR at off rated conditions (high speed)
		Real64 EIRTempModFacLS; // EIR modifier (function of entering wetbulb, outside drybulb) (low speed)
		Real64 EIRLS; // EIR at off rated conditions (low speed)
		Real64 TotCap; // total capacity at current speed [W]
		Real64 SHR; // sensible heat ratio at current speed
		Real64 EIR; // EIR at current speed
		Real64 AirMassFlowNom; // speed ratio weighted average of high and low speed air mass flow rates [kg/s]
		Real64 CBFNom; // coil bypass factor corresponding to AirMassFlowNom and SpeedRatio
		Real64 CBF; // CBFNom adjusted for actual air mass flow rate
		Real64 PLF; // Part load factor, accounts for thermal lag at compressor startup, used in
		// power calculation
		Real64 CondInletTemp; // Condenser inlet temperature (C). Outdoor dry-bulb temp for air-cooled condenser.
		// Outdoor Wetbulb +(1 - effectiveness)*(outdoor drybulb - outdoor wetbulb) for evap condenser.
		Real64 CondInletHumRat; // Condenser inlet humidity ratio (kg/kg). Zero for air-cooled condenser.
		// For evap condenser, its the humidity ratio of the air leaving the evap cooling pads.
		Real64 RhoAir; // Density of air [kg/m3]
		Real64 RhoWater; // Density of water [kg/m3]
		Real64 CondAirMassFlow; // Condenser air mass flow rate [kg/s]
		Real64 EvapCondPumpElecPower; // Evaporative condenser electric pump power [W]
		static Real64 MinAirHumRat( 0.0 ); // minimum of the inlet air humidity ratio and the outlet air humidity ratio
		static int Mode( 1 ); // Performance mode for MultiMode DX coil; Always 1 for other coil types
		Real64 OutdoorDryBulb; // Outdoor dry-bulb temperature at condenser (C)
		Real64 OutdoorWetBulb; // Outdoor wet-bulb temperature at condenser (C)
		Real64 OutdoorHumRat; // Outdoor humidity ratio at condenser (kg/kg)
		Real64 OutdoorPressure; // Outdoor barometric pressure at condenser (Pa)
		bool LocalForceOn;
		Real64 AirMassFlowRatio2; // Ratio of low speed air mass flow to rated air mass flow

		if ( present( ForceOn ) ) {
			LocalForceOn = true;
		} else {
			LocalForceOn = false;
		}

		if ( DXCoil( DXCoilNum ).CondenserInletNodeNum( Mode ) != 0 ) {
			OutdoorPressure = Node( DXCoil( DXCoilNum ).CondenserInletNodeNum( Mode ) ).Press;
			// If node is not connected to anything, pressure = default, use weather data
			if ( OutdoorPressure == DefaultNodeValues.Press ) {
				OutdoorDryBulb = OutDryBulbTemp;
				OutdoorHumRat = OutHumRat;
				OutdoorPressure = OutBaroPress;
				OutdoorWetBulb = OutWetBulbTemp;
			} else {
				OutdoorDryBulb = Node( DXCoil( DXCoilNum ).CondenserInletNodeNum( Mode ) ).Temp;
				OutdoorHumRat = Node( DXCoil( DXCoilNum ).CondenserInletNodeNum( Mode ) ).HumRat;
				OutdoorWetBulb = PsyTwbFnTdbWPb( OutdoorDryBulb, OutdoorHumRat, OutdoorPressure );
			}
			if ( DXCoil( DXCoilNum ).IsSecondaryDXCoilInZone ) {
				OutdoorDryBulb = ZT( DXCoil( DXCoilNum ).SecZonePtr );
				OutdoorHumRat = ZoneAirHumRat( DXCoil( DXCoilNum ).SecZonePtr );
				OutdoorWetBulb = DXCoil( DXCoilNum ).EvapInletWetBulb;
				OutdoorPressure = OutBaroPress;
			}
		} else if ( DXCoil( DXCoilNum ).IsSecondaryDXCoilInZone ) {
			OutdoorDryBulb = ZT( DXCoil( DXCoilNum ).SecZonePtr );
			OutdoorHumRat = ZoneAirHumRat( DXCoil( DXCoilNum ).SecZonePtr );
			OutdoorWetBulb = DXCoil( DXCoilNum ).EvapInletWetBulb;
			OutdoorPressure = OutBaroPress;
		} else {
			OutdoorDryBulb = OutDryBulbTemp;
			OutdoorHumRat = OutHumRat;
			OutdoorPressure = OutBaroPress;
			OutdoorWetBulb = OutWetBulbTemp;
		}

		AirMassFlow = DXCoil( DXCoilNum ).InletAirMassFlowRate;
		AirMassFlowRatio = DXCoil( DXCoilNum ).InletAirMassFlowRateMax / DXCoil( DXCoilNum ).RatedAirMassFlowRate( Mode );
		DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction = 0.0;
		InletAirDryBulbTemp = DXCoil( DXCoilNum ).InletAirTemp;
		InletAirEnthalpy = DXCoil( DXCoilNum ).InletAirEnthalpy;
		InletAirHumRat = DXCoil( DXCoilNum ).InletAirHumRat;
		AirMassFlowRatio2 = 1.0; // DXCoil(DXCoilNum)%RatedAirMassFlowRate2 / DXCoil(DXCoilNum)%RatedAirMassFlowRate(Mode)
		//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
		//InletAirPressure = DXCoil(DXCoilNum)%InletAirPressure
		//InletAirWetBulbC = PsyTwbFnTdbWPb(InletAirDryBulbTemp,InletAirHumRat,InletAirPressure)
		InletAirWetBulbC = PsyTwbFnTdbWPb( InletAirDryBulbTemp, InletAirHumRat, OutdoorPressure );
		if ( DXCoil( DXCoilNum ).CondenserType( Mode ) == AirCooled ) {
			CondInletTemp = OutdoorDryBulb; // Outdoor dry-bulb temp
		} else if ( DXCoil( DXCoilNum ).CondenserType( Mode ) == EvapCooled ) {
			// Outdoor wet-bulb temp from DataEnvironment + (1.0-EvapCondEffectiveness) * (drybulb - wetbulb)
			CondInletTemp = OutdoorWetBulb + ( OutdoorDryBulb - OutdoorWetBulb ) * ( 1.0 - DXCoil( DXCoilNum ).EvapCondEffect( Mode ) );
			CondInletHumRat = PsyWFnTdbTwbPb( CondInletTemp, OutdoorWetBulb, OutdoorPressure );
		}

		if ( ( AirMassFlow > 0.0 ) && ( ( GetCurrentScheduleValue( DXCoil( DXCoilNum ).SchedPtr ) > 0.0 ) || ( LocalForceOn ) ) && ( SpeedRatio > 0.0 || CycRatio > 0.0 ) ) {

			RhoAir = PsyRhoAirFnPbTdbW( OutdoorPressure, OutdoorDryBulb, OutdoorHumRat );
			if ( SpeedRatio > 0.0 ) {
				// Adjust high speed coil bypass factor for actual maximum air flow rate.
				RatedCBFHS = DXCoil( DXCoilNum ).RatedCBF( Mode );
				CBFHS = AdjustCBF( RatedCBFHS, DXCoil( DXCoilNum ).RatedAirMassFlowRate( Mode ), DXCoil( DXCoilNum ).InletAirMassFlowRateMax );
				// get high speed total capacity and SHR at current conditions
				CalcTotCapSHR( InletAirDryBulbTemp, InletAirHumRat, InletAirEnthalpy, InletAirWetBulbC, AirMassFlowRatio, DXCoil( DXCoilNum ).InletAirMassFlowRateMax, DXCoil( DXCoilNum ).RatedTotCap( Mode ), CBFHS, DXCoil( DXCoilNum ).CCapFTemp( Mode ), DXCoil( DXCoilNum ).CCapFFlow( Mode ), TotCapHS, SHRHS, CondInletTemp, OutdoorPressure );
				//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
				//                       CondInletTemp, Node(DXCoil(DXCoilNum)%AirInNode)%Press)
				// get the high speed SHR from user specified SHR modifier curves
				if ( DXCoil( DXCoilNum ).UserSHRCurveExists ) {
					SHRHS = CalcSHRUserDefinedCurves( InletAirDryBulbTemp, InletAirWetBulbC, AirMassFlowRatio, DXCoil( DXCoilNum ).SHRFTemp( Mode ), DXCoil( DXCoilNum ).SHRFFlow( Mode ), DXCoil( DXCoilNum ).RatedSHR( Mode ) );
				}
				// get low speed total capacity and SHR at current conditions
				CalcTotCapSHR( InletAirDryBulbTemp, InletAirHumRat, InletAirEnthalpy, InletAirWetBulbC, 1.0, DXCoil( DXCoilNum ).RatedAirMassFlowRate2, DXCoil( DXCoilNum ).RatedTotCap2, DXCoil( DXCoilNum ).RatedCBF2, DXCoil( DXCoilNum ).CCapFTemp2, DXCoil( DXCoilNum ).CCapFFlow( Mode ), TotCapLS, SHRLS, CondInletTemp, OutdoorPressure );
				//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
				//                       Node(DXCoil(DXCoilNum)%AirInNode)%Press)
				// get the low speed SHR from user specified SHR modifier curves
				if ( DXCoil( DXCoilNum ).UserSHRCurveExists ) {
					SHRLS = CalcSHRUserDefinedCurves( InletAirDryBulbTemp, InletAirWetBulbC, AirMassFlowRatio2, DXCoil( DXCoilNum ).SHRFTemp2, DXCoil( DXCoilNum ).SHRFFlow2, DXCoil( DXCoilNum ).RatedSHR2 );
				}
				// get high speed EIR at current conditions
				EIRTempModFacHS = CurveValue( DXCoil( DXCoilNum ).EIRFTemp( Mode ), InletAirWetBulbC, CondInletTemp );
				EIRFlowModFacHS = CurveValue( DXCoil( DXCoilNum ).EIRFFlow( Mode ), AirMassFlowRatio );
				EIRHS = DXCoil( DXCoilNum ).RatedEIR( Mode ) * EIRFlowModFacHS * EIRTempModFacHS;
				// get low speed EIR at current conditions
				//    EIRTempModFacLS = CurveValue(DXCoil(DXCoilNum)%EIRFTemp(Mode),InletAirWetBulbC,CondInletTemp)
				//    CR7307 changed EIRTempModFacLS calculation to that shown below.
				EIRTempModFacLS = CurveValue( DXCoil( DXCoilNum ).EIRFTemp2, InletAirWetBulbC, CondInletTemp );
				EIRLS = DXCoil( DXCoilNum ).RatedEIR2 * EIRTempModFacLS;

				// get current total capacity, SHR, EIR
				if ( SpeedRatio >= 1.0 ) {
					TotCap = TotCapHS;
					SHR = SHRHS;
					EIR = EIRHS;
					CBFNom = CBFHS;
					AirMassFlowNom = DXCoil( DXCoilNum ).InletAirMassFlowRateMax;
					CondAirMassFlow = RhoAir * DXCoil( DXCoilNum ).EvapCondAirFlow( Mode );
					EvapCondPumpElecPower = DXCoil( DXCoilNum ).EvapCondPumpElecNomPower( Mode );
				} else {
					TotCap = SpeedRatio * TotCapHS + ( 1.0 - SpeedRatio ) * TotCapLS;
					EIR = SpeedRatio * EIRHS + ( 1.0 - SpeedRatio ) * EIRLS;
					CBFNom = SpeedRatio * CBFHS + ( 1.0 - SpeedRatio ) * DXCoil( DXCoilNum ).RatedCBF2;
					AirMassFlowNom = SpeedRatio * DXCoil( DXCoilNum ).InletAirMassFlowRateMax + ( 1.0 - SpeedRatio ) * DXCoil( DXCoilNum ).RatedAirMassFlowRate2;
					CondAirMassFlow = RhoAir * ( SpeedRatio * DXCoil( DXCoilNum ).EvapCondAirFlow( Mode ) + ( 1.0 - SpeedRatio ) * DXCoil( DXCoilNum ).EvapCondAirFlow2 );
					EvapCondPumpElecPower = SpeedRatio * DXCoil( DXCoilNum ).EvapCondPumpElecNomPower( Mode ) + ( 1.0 - SpeedRatio ) * DXCoil( DXCoilNum ).EvapCondPumpElecNomPower2;
				}
				hDelta = TotCap / AirMassFlow;
				if ( DXCoil( DXCoilNum ).UserSHRCurveExists ) {
					if ( SpeedRatio >= 1.0 ) {
						SHR = SHRHS;
					} else {
						SHR = min( SpeedRatio * SHRHS + ( 1.0 - SpeedRatio ) * SHRLS, 1.0 );
					}
					OutletAirEnthalpy = InletAirEnthalpy - hDelta;
					if ( SHR < 1.0 ) {
						hTinwout = InletAirEnthalpy - ( 1.0 - SHR ) * hDelta;
						OutletAirHumRat = PsyWFnTdbH( InletAirDryBulbTemp, hTinwout );
						if ( OutletAirHumRat <= 0.0 ) {
							OutletAirHumRat = min( DryCoilOutletHumRatioMin, InletAirHumRat );
						}
					} else {
						SHR = 1.0;
						OutletAirHumRat = InletAirHumRat;
					}
					OutletAirDryBulbTemp = PsyTdbFnHW( OutletAirEnthalpy, OutletAirHumRat );
					OutletAirDryBulbTempSat = PsyTdpFnWPb( OutletAirHumRat, OutdoorPressure, RoutineNameHighSpeedOutlet );
					if ( OutletAirDryBulbTempSat > OutletAirDryBulbTemp ) {
						OutletAirDryBulbTemp = OutletAirDryBulbTempSat;
						OutletAirHumRat = PsyWFnTdbH( OutletAirDryBulbTemp, OutletAirEnthalpy, RoutineNameHighSpeedOutlet );
					}
					//LSOutletAirRH = PsyRhFnTdbWPb(OutletAirDryBulbTemp,OutletAirHumRat,OutdoorPressure,'CalcMultiSpeedDXCoil:highspeedoutlet')
				} else {
					// Adjust CBF for off-nominal flow
					CBF = AdjustCBF( CBFNom, AirMassFlowNom, AirMassFlow );
					// Calculate new apparatus dew point conditions
					hADP = InletAirEnthalpy - hDelta / ( 1.0 - CBF );
					tADP = PsyTsatFnHPb( hADP, OutdoorPressure );
					//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
					//    tADP = PsyTsatFnHPb(hADP,InletAirPressure)
					wADP = PsyWFnTdbH( tADP, hADP );
					hTinwADP = PsyHFnTdbW( InletAirDryBulbTemp, wADP );
					// get corresponding SHR
					if ( ( InletAirEnthalpy - hADP ) > 1.e-10 ) {
						SHR = min( ( hTinwADP - hADP ) / ( InletAirEnthalpy - hADP ), 1.0 );
					} else {
						SHR = 1.0;
					}

					//cr8918    SHR = MIN((hTinwADP-hADP)/(InletAirEnthalpy-hADP),1.0d0)
					OutletAirEnthalpy = InletAirEnthalpy - hDelta;
					// get outlet conditions
					hTinwout = InletAirEnthalpy - ( 1.0 - SHR ) * hDelta;
					OutletAirHumRat = PsyWFnTdbH( InletAirDryBulbTemp, hTinwout );

					OutletAirDryBulbTemp = PsyTdbFnHW( OutletAirEnthalpy, OutletAirHumRat );
					// OutletAirRH = PsyRhFnTdbWPb(OutletAirDryBulbTemp,OutletAirHumRat,OutBaroPress)
					// IF (OutletAirRH >= 1.0d0) THEN  ! Limit to saturated conditions at OutletAirEnthalpy
					//   OutletAirDryBulbTemp = PsyTsatFnHPb(OutletAirEnthalpy,OutBaroPress)
					//    OutletAirHumRat  = PsyWFnTdbH(OutletAirDryBulbTemp,OutletAirEnthalpy)
					//  END IF
					OutletAirDryBulbTempSat = PsyTsatFnHPb( OutletAirEnthalpy, OutdoorPressure );
					//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
					//    OutletAirDryBulbTempSat = PsyTsatFnHPb(OutletAirEnthalpy,InletAirPressure)
					if ( OutletAirDryBulbTemp < OutletAirDryBulbTempSat ) { // Limit to saturated conditions at OutletAirEnthalpy
						OutletAirDryBulbTemp = OutletAirDryBulbTempSat;
						OutletAirHumRat = PsyWFnTdbH( OutletAirDryBulbTemp, OutletAirEnthalpy );
					}
				}
				// calculate cooling rate and electrical power
				DXCoil( DXCoilNum ).TotalCoolingEnergyRate = AirMassFlow * ( InletAirEnthalpy - OutletAirEnthalpy );
				MinAirHumRat = min( InletAirHumRat, OutletAirHumRat );
				DXCoil( DXCoilNum ).SensCoolingEnergyRate = AirMassFlow * ( PsyHFnTdbW( InletAirDryBulbTemp, MinAirHumRat ) - PsyHFnTdbW( OutletAirDryBulbTemp, MinAirHumRat ) );
				// Don't let sensible capacity be greater than total capacity
				if ( DXCoil( DXCoilNum ).SensCoolingEnergyRate > DXCoil( DXCoilNum ).TotalCoolingEnergyRate ) {
					DXCoil( DXCoilNum ).SensCoolingEnergyRate = DXCoil( DXCoilNum ).TotalCoolingEnergyRate;
				}
				DXCoil( DXCoilNum ).LatCoolingEnergyRate = DXCoil( DXCoilNum ).TotalCoolingEnergyRate - DXCoil( DXCoilNum ).SensCoolingEnergyRate;
				DXCoil( DXCoilNum ).ElecCoolingPower = TotCap * EIR;
				//   Calculation for heat reclaim needs to be corrected to use compressor power (not including condenser fan power)
				HeatReclaimDXCoil( DXCoilNum ).AvailCapacity = DXCoil( DXCoilNum ).TotalCoolingEnergyRate + DXCoil( DXCoilNum ).ElecCoolingPower;
				DXCoil( DXCoilNum ).PartLoadRatio = 1.0;
				DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction = 1.0;

				DXCoil( DXCoilNum ).OutletAirEnthalpy = OutletAirEnthalpy;
				DXCoil( DXCoilNum ).OutletAirHumRat = OutletAirHumRat;
				DXCoil( DXCoilNum ).OutletAirTemp = OutletAirDryBulbTemp;

			} else if ( CycRatio > 0.0 ) {

				if ( DXCoil( DXCoilNum ).CondenserType( Mode ) == EvapCooled ) {
					// Outdoor wet-bulb temp from DataEnvironment + (1.0-EvapCondEffectiveness) * (drybulb - wetbulb)
					CondInletTemp = OutdoorWetBulb + ( OutdoorDryBulb - OutdoorWetBulb ) * ( 1.0 - DXCoil( DXCoilNum ).EvapCondEffect2 );
					CondInletHumRat = PsyWFnTdbTwbPb( CondInletTemp, OutdoorWetBulb, OutdoorPressure );
				}

				// Adjust low speed coil bypass factor for actual flow rate.
				// CBF = AdjustCBF(DXCoil(DXCoilNum)%RatedCBF2,DXCoil(DXCoilNum)%RatedAirMassFlowRate2,AirMassFlow)
				// get low speed total capacity and SHR at current conditions
				CalcTotCapSHR( InletAirDryBulbTemp, InletAirHumRat, InletAirEnthalpy, InletAirWetBulbC, 1.0, DXCoil( DXCoilNum ).RatedAirMassFlowRate2, DXCoil( DXCoilNum ).RatedTotCap2, DXCoil( DXCoilNum ).RatedCBF2, DXCoil( DXCoilNum ).CCapFTemp2, DXCoil( DXCoilNum ).CCapFFlow( Mode ), TotCapLS, SHRLS, CondInletTemp, OutdoorPressure );
				// get the low speed SHR from user specified SHR modifier curves
				if ( DXCoil( DXCoilNum ).UserSHRCurveExists ) {
					SHRLS = CalcSHRUserDefinedCurves( InletAirDryBulbTemp, InletAirWetBulbC, 1.0, DXCoil( DXCoilNum ).SHRFTemp2, DXCoil( DXCoilNum ).SHRFFlow2, DXCoil( DXCoilNum ).RatedSHR2 );
				}
				//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
				//                       Node(DXCoil(DXCoilNum)%AirInNode)%Press)
				hDelta = TotCapLS / AirMassFlow;
				if ( DXCoil( DXCoilNum ).UserSHRCurveExists ) {
					SHR = SHRLS;
					LSOutletAirEnthalpy = InletAirEnthalpy - hDelta;
					if ( SHR < 1.0 ) {
						hTinwout = InletAirEnthalpy - ( 1.0 - SHR ) * hDelta;
						LSOutletAirHumRat = PsyWFnTdbH( InletAirDryBulbTemp, hTinwout );
						if ( LSOutletAirHumRat <= 0.0 ) {
							LSOutletAirHumRat = min( DryCoilOutletHumRatioMin, InletAirHumRat );
						}
					} else {
						SHR = 1.0;
						LSOutletAirHumRat = InletAirHumRat;
					}
					LSOutletAirDryBulbTemp = PsyTdbFnHW( LSOutletAirEnthalpy, LSOutletAirHumRat );
					OutletAirDryBulbTempSat = PsyTdpFnWPb( LSOutletAirHumRat, OutdoorPressure, RoutineNameLowSpeedOutlet );
					if ( OutletAirDryBulbTempSat > LSOutletAirDryBulbTemp ) {
						LSOutletAirDryBulbTemp = OutletAirDryBulbTempSat;
						LSOutletAirHumRat = PsyWFnTdbH( LSOutletAirDryBulbTemp, LSOutletAirEnthalpy, RoutineNameLowSpeedOutlet );
					}
					LSOutletAirRH = PsyRhFnTdbWPb( LSOutletAirDryBulbTemp, LSOutletAirHumRat, OutdoorPressure, RoutineNameLowSpeedOutlet );
				} else {
					// Adjust CBF for off-nominal flow
					CBF = AdjustCBF( DXCoil( DXCoilNum ).RatedCBF2, DXCoil( DXCoilNum ).RatedAirMassFlowRate2, AirMassFlow );
					// Calculate new apparatus dew point conditions
					hADP = InletAirEnthalpy - hDelta / ( 1.0 - CBF );
					tADP = PsyTsatFnHPb( hADP, OutdoorPressure, RoutineNameNewDewPointConditions );
					//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
					//    tADP = PsyTsatFnHPb(hADP,InletAirPressure)
					wADP = PsyWFnTdbH( tADP, hADP, RoutineNameNewDewPointConditions );
					hTinwADP = PsyHFnTdbW( InletAirDryBulbTemp, wADP );
					// get corresponding SHR
					if ( ( InletAirEnthalpy - hADP ) > 1.e-10 ) {
						SHR = min( ( hTinwADP - hADP ) / ( InletAirEnthalpy - hADP ), 1.0 );
					} else {
						SHR = 1.0;
					}
					//cr8918    SHR = MIN((hTinwADP-hADP)/(InletAirEnthalpy-hADP),1.0d0)
					// get low speed outlet conditions
					LSOutletAirEnthalpy = InletAirEnthalpy - hDelta;
					hTinwout = InletAirEnthalpy - ( 1.0 - SHR ) * hDelta;
					LSOutletAirHumRat = PsyWFnTdbH( InletAirDryBulbTemp, hTinwout );
					LSOutletAirDryBulbTemp = PsyTdbFnHW( LSOutletAirEnthalpy, LSOutletAirHumRat );
					LSOutletAirRH = PsyRhFnTdbWPb( LSOutletAirDryBulbTemp, LSOutletAirHumRat, OutdoorPressure, RoutineNameLowSpeedOutlet );
					OutletAirDryBulbTempSat = PsyTsatFnHPb( LSOutletAirEnthalpy, OutdoorPressure, RoutineNameLowSpeedOutlet );
					//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
					//    LSOutletAirRH = PsyRhFnTdbWPb(LSOutletAirDryBulbTemp,LSOutletAirHumRat,InletAirPressure)
					//    OutletAirDryBulbTempSat = PsyTsatFnHPb(LSOutletAirEnthalpy,InletAirPressure)
					if ( LSOutletAirDryBulbTemp < OutletAirDryBulbTempSat ) { // Limit to saturated conditions at OutletAirEnthalpy
						LSOutletAirDryBulbTemp = OutletAirDryBulbTempSat;
						LSOutletAirHumRat = PsyWFnTdbH( LSOutletAirDryBulbTemp, LSOutletAirEnthalpy, RoutineNameLowSpeedOutlet );
					}
				}
				// outlet conditions are average of inlet and low speed weighted by CycRatio
				OutletAirEnthalpy = CycRatio * LSOutletAirEnthalpy + ( 1.0 - CycRatio ) * InletAirEnthalpy;
				OutletAirHumRat = CycRatio * LSOutletAirHumRat + ( 1.0 - CycRatio ) * InletAirHumRat;
				OutletAirDryBulbTemp = PsyTdbFnHW( OutletAirEnthalpy, OutletAirHumRat );
				// get low speed EIR at current conditions
				//    EIRTempModFacLS = CurveValue(DXCoil(DXCoilNum)%EIRFTemp(Mode),InletAirWetBulbC,CondInletTemp)
				//    CR7307 changed EIRTempModFacLS calculation to that shown below.
				EIRTempModFacLS = CurveValue( DXCoil( DXCoilNum ).EIRFTemp2, InletAirWetBulbC, CondInletTemp );
				EIRLS = DXCoil( DXCoilNum ).RatedEIR2 * EIRTempModFacLS;
				// get the part load factor that will account for cycling losses
				PLF = CurveValue( DXCoil( DXCoilNum ).PLFFPLR( Mode ), CycRatio );
				if ( PLF < 0.7 ) {
					PLF = 0.7;
				}
				// calculate the run time fraction
				DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction = CycRatio / PLF;
				DXCoil( DXCoilNum ).PartLoadRatio = CycRatio;
				if ( DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction > 1.0 ) {
					DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction = 1.0; // Reset coil runtime fraction to 1.0
				}
				// get the eletrical power consumption
				DXCoil( DXCoilNum ).ElecCoolingPower = TotCapLS * EIRLS * DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction;
				// calculate cooling output power
				DXCoil( DXCoilNum ).TotalCoolingEnergyRate = AirMassFlow * ( InletAirEnthalpy - OutletAirEnthalpy );
				//   Calculation for heat reclaim needs to be corrected to use compressor power (not including condenser fan power)
				HeatReclaimDXCoil( DXCoilNum ).AvailCapacity = DXCoil( DXCoilNum ).TotalCoolingEnergyRate + DXCoil( DXCoilNum ).ElecCoolingPower;
				MinAirHumRat = min( InletAirHumRat, OutletAirHumRat );
				DXCoil( DXCoilNum ).SensCoolingEnergyRate = AirMassFlow * ( PsyHFnTdbW( InletAirDryBulbTemp, MinAirHumRat ) - PsyHFnTdbW( OutletAirDryBulbTemp, MinAirHumRat ) );
				// Don't let sensible capacity be greater than total capacity
				if ( DXCoil( DXCoilNum ).SensCoolingEnergyRate > DXCoil( DXCoilNum ).TotalCoolingEnergyRate ) {
					DXCoil( DXCoilNum ).SensCoolingEnergyRate = DXCoil( DXCoilNum ).TotalCoolingEnergyRate;
				}
				DXCoil( DXCoilNum ).LatCoolingEnergyRate = DXCoil( DXCoilNum ).TotalCoolingEnergyRate - DXCoil( DXCoilNum ).SensCoolingEnergyRate;
				DXCoil( DXCoilNum ).OutletAirEnthalpy = OutletAirEnthalpy;
				DXCoil( DXCoilNum ).OutletAirHumRat = OutletAirHumRat;
				DXCoil( DXCoilNum ).OutletAirTemp = OutletAirDryBulbTemp;
				CondAirMassFlow = RhoAir * DXCoil( DXCoilNum ).EvapCondAirFlow2 * DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction;
				EvapCondPumpElecPower = DXCoil( DXCoilNum ).EvapCondPumpElecNomPower2 * DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction;

			}

			if ( DXCoil( DXCoilNum ).CondenserType( Mode ) == EvapCooled ) {
				//******************
				//             WATER CONSUMPTION IN m3 OF WATER FOR DIRECT
				//             H2O [m3/sec] = Delta W[KgH2O/Kg air]*Mass Flow Air[Kg air]
				//                                /RhoWater [kg H2O/m3 H2O]
				//******************
				RhoWater = RhoH2O( OutdoorDryBulb );
				DXCoil( DXCoilNum ).EvapWaterConsumpRate = ( CondInletHumRat - OutdoorHumRat ) * CondAirMassFlow / RhoWater;
				DXCoil( DXCoilNum ).EvapCondPumpElecPower = EvapCondPumpElecPower;
				//set water system demand request (if needed)
				if ( DXCoil( DXCoilNum ).EvapWaterSupplyMode == WaterSupplyFromTank ) {

					WaterStorage( DXCoil( DXCoilNum ).EvapWaterSupTankID ).VdotRequestDemand( DXCoil( DXCoilNum ).EvapWaterTankDemandARRID ) = DXCoil( DXCoilNum ).EvapWaterConsumpRate;
				}

				// Calculate basin heater power
				CalcBasinHeaterPower( DXCoil( DXCoilNum ).BasinHeaterPowerFTempDiff, DXCoil( DXCoilNum ).BasinHeaterSchedulePtr, DXCoil( DXCoilNum ).BasinHeaterSetPointTemp, DXCoil( DXCoilNum ).BasinHeaterPower );
				DXCoil( DXCoilNum ).BasinHeaterPower *= ( 1.0 - DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction );
			}

		} else {

			// DX coil is off; just pass through conditions
			DXCoil( DXCoilNum ).OutletAirEnthalpy = DXCoil( DXCoilNum ).InletAirEnthalpy;
			DXCoil( DXCoilNum ).OutletAirHumRat = DXCoil( DXCoilNum ).InletAirHumRat;
			DXCoil( DXCoilNum ).OutletAirTemp = DXCoil( DXCoilNum ).InletAirTemp;

			DXCoil( DXCoilNum ).ElecCoolingPower = 0.0;
			DXCoil( DXCoilNum ).TotalCoolingEnergyRate = 0.0;
			DXCoil( DXCoilNum ).SensCoolingEnergyRate = 0.0;
			DXCoil( DXCoilNum ).LatCoolingEnergyRate = 0.0;
			DXCoil( DXCoilNum ).EvapCondPumpElecPower = 0.0;
			DXCoil( DXCoilNum ).EvapWaterConsumpRate = 0.0;

			// Calculate basin heater power
			if ( DXCoil( DXCoilNum ).CondenserType( Mode ) == EvapCooled ) {
				CalcBasinHeaterPower( DXCoil( DXCoilNum ).BasinHeaterPowerFTempDiff, DXCoil( DXCoilNum ).BasinHeaterSchedulePtr, DXCoil( DXCoilNum ).BasinHeaterSetPointTemp, DXCoil( DXCoilNum ).BasinHeaterPower );
			}
		}

		DXCoilOutletTemp( DXCoilNum ) = DXCoil( DXCoilNum ).OutletAirTemp;
		DXCoilOutletHumRat( DXCoilNum ) = DXCoil( DXCoilNum ).OutletAirHumRat;
		DXCoil( DXCoilNum ).CondInletTemp = CondInletTemp; // Save condenser inlet temp in the data structure

		// calc secondary coil if specified
		if ( DXCoil( DXCoilNum ).IsSecondaryDXCoilInZone ) {
			CalcSecondaryDXCoils( DXCoilNum );
		}

	}

	void
	CalcBasinHeaterPowerForMultiModeDXCoil(
		int const DXCoilNum, // Index of coil being simulated
		int const DehumidMode // Dehumidification mode (0=normal, 1=enhanced)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Chandan Sharma, FSEC
		//       DATE WRITTEN   May 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// To calculate the basin heater power for multi mode DX cooling coil

		// METHODOLOGY EMPLOYED:
		// The methodology employed is as follows:
		// 1) If the number of capacity stages is equal to 1 and the CondenserType for stage 1
		//    is EvapCooled, then the basin heater power is calculated for (1-runtimefractionstage1) of DX coil
		// 2) If the number of capacity stages is greater than 1, then
		//    a) If the CondenserType for stage 1 is EvapCooled, then the basin heater power is calculated for
		//       (1-runtimefractionofstage1) of DX coil
		//    b) Elseif the CondenserType for stage 2 is EvapCooled, then the basin heater power is calculated for
		//       (1-runtimefractionofstage2) of DX coil

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int PerfMode; // Performance mode for MultiMode DX coil; Always 1 for other coil types
		// 1-2=normal mode: 1=stage 1 only, 2=stage 1&2
		// 3-4=enhanced dehumidification mode: 3=stage 1 only, 4=stage 1&2

		if ( DXCoil( DXCoilNum ).NumCapacityStages == 1 ) {
			DXCoil( DXCoilNum ).BasinHeaterPower *= ( 1.0 - DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction );
		} else {
			PerfMode = DehumidMode * 2 + 1;
			if ( DXCoil( DXCoilNum ).CondenserType( PerfMode ) == EvapCooled ) {
				DXCoil( DXCoilNum ).BasinHeaterPower *= ( 1.0 - DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction );
			} else if ( DXCoil( DXCoilNum ).CondenserType( PerfMode + 1 ) == EvapCooled ) {
				CalcBasinHeaterPower( DXCoil( DXCoilNum ).BasinHeaterPowerFTempDiff, DXCoil( DXCoilNum ).BasinHeaterSchedulePtr, DXCoil( DXCoilNum ).BasinHeaterSetPointTemp, DXCoil( DXCoilNum ).BasinHeaterPower );
				DXCoil( DXCoilNum ).BasinHeaterPower *= ( 1.0 - DXCoil( DXCoilNum ).CoolingCoilStg2RuntimeFrac );
			}
		}

	}

	Real64
	AdjustCBF(
		Real64 const CBFNom, // nominal coil bypass factor
		Real64 const AirMassFlowRateNom, // nominal air mass flow rate [kg/s]
		Real64 const AirMassFlowRate // actual air mass flow rate [kg/s]
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Fred Buhl using Don Shirey's code
		//       DATE WRITTEN   September 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		//    Adjust coil bypass factor for actual air flow rate.

		// METHODOLOGY EMPLOYED:
		// Uses relation CBF = exp(-NTU) whereNTU = A0/(m*cp). Relationship models the cooling coil
		// as a heat exchanger with Cmin/Cmax = 0.
		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Real64 CBFAdj; // the result - the adjusted coil bypass factor

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 A0; // intermediate variable
		Real64 ADiff; // intermediate variable

		if ( CBFNom > 0.0 ) {
			A0 = -std::log( CBFNom ) * AirMassFlowRateNom;
		} else {
			A0 = 0.0;
		}
		ADiff = -A0 / AirMassFlowRate;
		if ( ADiff >= EXP_LowerLimit ) {
			CBFAdj = std::exp( ADiff );
		} else {
			CBFAdj = 0.0;
		}

		return CBFAdj;
	}

	Real64
	CalcCBF(
		std::string const & UnitType,
		std::string const & UnitName,
		Real64 const InletAirTemp, // inlet air temperature [C]
		Real64 const InletAirHumRat, // inlet air humidity ratio [kg water / kg dry air]
		Real64 const TotCap, // total cooling  capacity [Watts]
		Real64 const AirMassFlowRate, // the air mass flow rate at the given capacity [kg/s]
		Real64 const SHR, // sensible heat ratio at the given capacity and flow rate
		bool const PrintFlag, // flag used to print warnings if desired
		Real64 const BaroPress // Barometric pressure [Pa], defaulted to StdBaroPress in header
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Fred Buhl using Don Shirey's code
		//       DATE WRITTEN   September 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculate the coil bypass factor for a coil given the total capacity at the entering conditions,
		// air mass flow rate at the entering conditions, and the sensible heat ratio (SHR) at the
		// entering conditions.

		// METHODOLOGY EMPLOYED:
		// calculate SlopeRated (deltahumrat/deltaT) using rated unit information provided by
		// user. Then hunt along saturation curve of psychrometric chart until the slope of the line
		// between the saturation point and rated inlet air humidity ratio and T is the same as SlopeRated.
		// When the slopes are equal, then we have located the apparatus dewpoint of the coil at rated
		// conditions. From this information, coil bypass factor is calculated.

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::RoundSigDigits;

		// Return value
		Real64 CBF( 0.0 ); // the result - the coil bypass factor

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcCBF" );
		static Real64 SmallDifferenceTest( 0.00000001 );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 InletAirEnthalpy; // Enthalpy of inlet air to evaporator at given conditions [J/kg]
		Real64 DeltaH( 0.0 ); // Enthalpy drop across evaporator at given conditions [J/kg]
		Real64 DeltaT( 0.0 ); // Temperature drop across evaporator at given conditions [C]
		Real64 DeltaHumRat( 0.0 ); // Humidity ratio drop across evaporator at given conditions [kg/kg]
		Real64 OutletAirTemp( InletAirTemp ); // Outlet dry-bulb temperature from evaporator at given conditions [C]
		Real64 OutletAirEnthalpy; // Enthalpy of outlet air at given conditions [J/kg]
		Real64 OutletAirHumRat( InletAirHumRat ); // Outlet humidity ratio from evaporator at given conditions [kg/kg]
		Real64 OutletAirRH; // relative humidity of the outlet air
		Real64 Error; // Error term used in given coil bypass factor (CBF) calculations
		Real64 ErrorLast; // Error term, from previous iteration
		int Iter; // Iteration loop counter in CBF calculations
		int IterMax( 50 ); // Maximum number of iterations in CBF calculations
		Real64 ADPTemp; // Apparatus dewpoint temperature used in CBF calculations [C]
		Real64 ADPHumRat; // Apparatus dewpoint humidity used in CBF calculations [kg/kg]
		Real64 ADPEnthalpy; // Air enthalpy at apparatus dew point [J/kg]
		Real64 DeltaADPTemp; // Change in Apparatus Dew Point used in CBF calculations [C]
		Real64 SlopeAtConds( 0.0 ); // Slope (DeltaHumRat/DeltaT) at given conditions
		Real64 Slope( 0.0 ); // Calculated Slope used while hunting for Tadp
		Real64 Tolerance; // Convergence tolerance for CBF calculations
		Real64 HTinHumRatOut; // Air enthalpy at inlet air temp and outlet air humidity ratio [J/kg]
		static bool CBFErrors( false ); // Set to true if errors in CBF calculation, fatal at end of routine

		DeltaH = TotCap / AirMassFlowRate;
		InletAirEnthalpy = PsyHFnTdbW( InletAirTemp, InletAirHumRat );
		HTinHumRatOut = InletAirEnthalpy - ( 1.0 - SHR ) * DeltaH;
		OutletAirHumRat = PsyWFnTdbH( InletAirTemp, HTinHumRatOut );
		DeltaHumRat = InletAirHumRat - OutletAirHumRat;
		OutletAirEnthalpy = InletAirEnthalpy - DeltaH;
		OutletAirTemp = PsyTdbFnHW( OutletAirEnthalpy, OutletAirHumRat );
		//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
		//  Pressure will have to be pass into this subroutine to fix this one
		OutletAirRH = PsyRhFnTdbWPb( OutletAirTemp, OutletAirHumRat, BaroPress, RoutineName );
		if ( OutletAirRH >= 1.0 && PrintFlag ) {
			ShowSevereError( "For object = " + UnitType + ", name = \"" + UnitName + "\"" );
			ShowContinueError( "Calculated outlet air relative humidity greater than 1. The combination of" );
			ShowContinueError( "rated air volume flow rate, total cooling capacity and sensible heat ratio yields coil exiting" );
			ShowContinueError( "air conditions above the saturation curve. Possible fixes are to reduce the rated total cooling" );
			ShowContinueError( "capacity, increase the rated air volume flow rate, or reduce the rated sensible heat ratio for this coil." );
			ShowContinueError( "If autosizing, it is recommended that all three of these values be autosized." );
			ShowContinueError( "...Inputs used for calculating cooling coil bypass factor." );
			ShowContinueError( "...Inlet Air Temperature     = " + RoundSigDigits( InletAirTemp, 2 ) + " C" );
			ShowContinueError( "...Outlet Air Temperature    = " + RoundSigDigits( OutletAirTemp, 2 ) + " C" );
			ShowContinueError( "...Inlet Air Humidity Ratio  = " + RoundSigDigits( InletAirHumRat, 6 ) + " kgWater/kgDryAir" );
			ShowContinueError( "...Outlet Air Humidity Ratio = " + RoundSigDigits( OutletAirHumRat, 6 ) + " kgWater/kgDryAir" );
			ShowContinueError( "...Total Cooling Capacity used in calculation = " + RoundSigDigits( TotCap, 2 ) + " W" );
			ShowContinueError( "...Air Mass Flow Rate used in calculation     = " + RoundSigDigits( AirMassFlowRate, 6 ) + " kg/s" );
			ShowContinueError( "...Air Volume Flow Rate used in calculation   = " + RoundSigDigits( AirMassFlowRate / PsyRhoAirFnPbTdbW( BaroPress, InletAirTemp, InletAirHumRat, RoutineName ), 6 ) + " m3/s" );
			if ( TotCap > 0.0 ) {
				if ( ( ( MinRatedVolFlowPerRatedTotCap( DXCT ) - AirMassFlowRate / PsyRhoAirFnPbTdbW( BaroPress, InletAirTemp, InletAirHumRat, RoutineName ) / TotCap ) > SmallDifferenceTest ) || ( ( AirMassFlowRate / PsyRhoAirFnPbTdbW( BaroPress, InletAirTemp, InletAirHumRat, RoutineName ) / TotCap - MaxRatedVolFlowPerRatedTotCap( DXCT ) ) > SmallDifferenceTest ) ) {
					ShowContinueError( "...Air Volume Flow Rate per Watt of Rated Cooling Capacity is also out of bounds at = " + RoundSigDigits( AirMassFlowRate / PsyRhoAirFnPbTdbW( BaroPress, InletAirTemp, InletAirHumRat, RoutineName ) / TotCap, 7 ) + " m3/s/W" );
				}
			}
			ShowContinueErrorTimeStamp( "" );
			ShowFatalError( "Check and revise the input data for this coil before rerunning the simulation." );
		}
		DeltaT = InletAirTemp - OutletAirTemp;
		if ( DeltaT <= 0.0 ) {
			ShowSevereError( "For object = " + UnitType + ", name = \"" + UnitName + "\"" );
			ShowContinueError( "Calculated coil delta T is less than or equal to 0. The combination of" );
			ShowContinueError( "rated air volume flow rate, total cooling capacity and sensible heat ratio yields coil exiting" );
			ShowContinueError( "air conditions that are not reasonable. Possible fixes are to adjust the rated total cooling" );
			ShowContinueError( "capacity, rated air volume flow rate, or rated sensible heat ratio for this coil." );
			ShowContinueError( "If autosizing, it is recommended that all three of these values be autosized." );
			ShowContinueError( "...Inputs used for calculating cooling coil bypass factor." );
			ShowContinueError( "...Inlet Air Temperature     = " + RoundSigDigits( InletAirTemp, 2 ) + " C" );
			ShowContinueError( "...Outlet Air Temperature    = " + RoundSigDigits( OutletAirTemp, 2 ) + " C" );
			ShowContinueError( "...Inlet Air Humidity Ratio  = " + RoundSigDigits( InletAirHumRat, 6 ) + " kgWater/kgDryAir" );
			ShowContinueError( "...Outlet Air Humidity Ratio = " + RoundSigDigits( OutletAirHumRat, 6 ) + " kgWater/kgDryAir" );
			ShowContinueError( "...Total Cooling Capacity used in calculation = " + RoundSigDigits( TotCap, 2 ) + " W" );
			ShowContinueError( "...Air Mass Flow Rate used in calculation     = " + RoundSigDigits( AirMassFlowRate, 6 ) + " kg/s" );
			ShowContinueError( "...Air Volume Flow Rate used in calculation   = " + RoundSigDigits( AirMassFlowRate / PsyRhoAirFnPbTdbW( BaroPress, InletAirTemp, InletAirHumRat, RoutineName ), 6 ) + " m3/s" );
			if ( TotCap > 0.0 ) {
				if ( ( ( MinRatedVolFlowPerRatedTotCap( DXCT ) - AirMassFlowRate / PsyRhoAirFnPbTdbW( BaroPress, InletAirTemp, InletAirHumRat, RoutineName ) / TotCap ) > SmallDifferenceTest ) || ( ( AirMassFlowRate / PsyRhoAirFnPbTdbW( BaroPress, InletAirTemp, InletAirHumRat, RoutineName ) / TotCap - MaxRatedVolFlowPerRatedTotCap( DXCT ) ) > SmallDifferenceTest ) ) {
					ShowContinueError( "...Air Volume Flow Rate per Watt of Rated Cooling Capacity is also out of bounds at = " + RoundSigDigits( AirMassFlowRate / PsyRhoAirFnPbTdbW( BaroPress, InletAirTemp, InletAirHumRat, RoutineName ) / TotCap, 7 ) + " m3/s/W" );
				}
			}
			ShowContinueErrorTimeStamp( "" );
			ShowFatalError( "Check and revise the input data for this coil before rerunning the simulation." );
		}
		// Calculate slope at given conditions
		if ( DeltaT > 0.0 ) SlopeAtConds = DeltaHumRat / DeltaT;

		//  IF (SlopeAtConds .le. .0000001d0 .or. OutletAirHumRat .le. 0.0d0) THEN
		if ( SlopeAtConds < 0.0 || OutletAirHumRat <= 0.0 ) {
			//   Invalid conditions, slope can't be less than zero (SHR > 1) or
			//   outlet air humidity ratio can't be less than zero.
			ShowSevereError( UnitType + " \"" + UnitName + "\"" );
			ShowContinueError( "...Invalid slope or outlet air condition when calculating cooling coil bypass factor." );
			ShowContinueError( "...Slope = " + RoundSigDigits( SlopeAtConds, 8 ) );
			ShowContinueError( "...Inlet Air Temperature     = " + RoundSigDigits( InletAirTemp, 2 ) + " C" );
			ShowContinueError( "...Outlet Air Temperature    = " + RoundSigDigits( OutletAirTemp, 2 ) + " C" );
			ShowContinueError( "...Inlet Air Humidity Ratio  = " + RoundSigDigits( InletAirHumRat, 6 ) + " kgWater/kgDryAir" );
			ShowContinueError( "...Outlet Air Humidity Ratio = " + RoundSigDigits( OutletAirHumRat, 6 ) + " kgWater/kgDryAir" );
			ShowContinueError( "...Total Cooling Capacity used in calculation = " + RoundSigDigits( TotCap, 2 ) + " W" );
			ShowContinueError( "...Air Mass Flow Rate used in calculation     = " + RoundSigDigits( AirMassFlowRate, 6 ) + " kg/s" );
			ShowContinueError( "...Air Volume Flow Rate used in calculation   = " + RoundSigDigits( AirMassFlowRate / PsyRhoAirFnPbTdbW( BaroPress, InletAirTemp, InletAirHumRat, RoutineName ), 6 ) + " m3/s" );
			if ( TotCap > 0.0 ) {
				if ( ( ( MinRatedVolFlowPerRatedTotCap( DXCT ) - AirMassFlowRate / PsyRhoAirFnPbTdbW( BaroPress, InletAirTemp, InletAirHumRat, RoutineName ) / TotCap ) > SmallDifferenceTest ) || ( ( AirMassFlowRate / PsyRhoAirFnPbTdbW( BaroPress, InletAirTemp, InletAirHumRat, RoutineName ) / TotCap - MaxRatedVolFlowPerRatedTotCap( DXCT ) ) > SmallDifferenceTest ) ) {
					ShowContinueError( "...Air Volume Flow Rate per Watt of Rated Cooling Capacity is also out of bounds at = " + RoundSigDigits( AirMassFlowRate / PsyRhoAirFnPbTdbW( BaroPress, InletAirTemp, InletAirHumRat, RoutineName ) / TotCap, 7 ) + " m3/s/W" );
				}
			}
			ShowContinueErrorTimeStamp( "" );
			CBFErrors = true;
			CBF = 0.0; //? Added: Is this what should be returned
		} else {

			//   First guess for Tadp is outlet air dew point
			//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
			//  Pressure will have to be pass into this subroutine to fix this one
			ADPTemp = PsyTdpFnWPb( OutletAirHumRat, BaroPress );

			Tolerance = 1.0; // initial conditions for iteration
			ErrorLast = 100.0;
			Iter = 0;
			DeltaADPTemp = 5.0;
			while ( ( Iter <= IterMax ) && ( Tolerance > 0.001 ) ) {
				//     Do for IterMax iterations or until the error gets below .1%
				if ( Iter > 0 ) ADPTemp += DeltaADPTemp;
				++Iter;

				//     Find new slope using guessed Tadp

				//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
				//  Pressure will have to be pass into this subroutine to fix this one
				ADPHumRat = PsyWFnTdpPb( ADPTemp, BaroPress );
				Slope = ( InletAirHumRat - ADPHumRat ) / ( InletAirTemp - ADPTemp );

				//     check for convergence (slopes are equal to within error tolerance)

				Error = ( Slope - SlopeAtConds ) / SlopeAtConds;
				if ( ( Error > 0.0 ) && ( ErrorLast < 0.0 ) ) DeltaADPTemp = -DeltaADPTemp / 2.0;
				if ( ( Error < 0.0 ) && ( ErrorLast > 0.0 ) ) DeltaADPTemp = -DeltaADPTemp / 2.0;
				ErrorLast = Error;

				Tolerance = std::abs( Error );

			}

			//   Calculate Bypass Factor from Enthalpies

			InletAirEnthalpy = PsyHFnTdbW( InletAirTemp, InletAirHumRat );
			OutletAirEnthalpy = PsyHFnTdbW( OutletAirTemp, OutletAirHumRat );
			ADPEnthalpy = PsyHFnTdbW( ADPTemp, ADPHumRat );
			CBF = min( 1.0, ( OutletAirEnthalpy - ADPEnthalpy ) / ( InletAirEnthalpy - ADPEnthalpy ) );
			if ( Iter > IterMax && PrintFlag ) {
				ShowSevereError( UnitType + " \"" + UnitName + "\" -- coil bypass factor calculation did not converge after max iterations." );
				ShowContinueError( "The RatedSHR of [" + RoundSigDigits( SHR, 3 ) + "], entered by the user or autosized (see *.eio file)," );
				ShowContinueError( "may be causing this. The line defined by the coil rated inlet air conditions" );
				ShowContinueError( "(26.7C drybulb and 19.4C wetbulb) and the RatedSHR (i.e., slope of the line) must intersect" );
				ShowContinueError( "the saturation curve of the psychrometric chart. If the RatedSHR is too low, then this" );
				ShowContinueError( "intersection may not occur and the coil bypass factor calculation will not converge." );
				ShowContinueError( "If autosizing the SHR, recheck the design supply air humidity ratio and design supply air" );
				ShowContinueError( "temperature values in the Sizing:System and Sizing:Zone objects. In general, the temperatures" );
				ShowContinueError( "and humidity ratios specified in these two objects should be the same for each system" );
				ShowContinueError( "and the zones that it serves." );
				ShowContinueErrorTimeStamp( "" );
				CBFErrors = true; // Didn't converge within MaxIter iterations
			}
			if ( CBF < 0.0 && PrintFlag ) {
				ShowSevereError( UnitType + " \"" + UnitName + "\" -- negative coil bypass factor calculated." );
				ShowContinueErrorTimeStamp( "" );
				CBFErrors = true; // Negative CBF not valid
			}
		}

		// Show fatal error for specific coil that caused a CBF error
		if ( CBFErrors ) {
			ShowFatalError( UnitType + " \"" + UnitName + "\" Errors found in calculating coil bypass factors" );
		}

		return CBF;
	}

	Real64
	ValidateADP(
		std::string const & UnitType, // component name
		std::string const & UnitName, // component type
		Real64 const RatedInletAirTemp, // coil inlet air temperature [C]
		Real64 const RatedInletAirHumRat, // coil inlet air humidity ratio [kg/kg]
		Real64 const TotCap, // coil total capacity [W]
		Real64 const AirMassFlow, // coil air mass flow rate [kg/s]
		Real64 const InitialSHR, // coil sensible heat ratio []
		std::string const & CallingRoutine // function name calling this routine
	)
	{

		// FUNCTION INFORMATION:
		//    AUTHOR         Richard Raustad, FSEC
		//    DATE WRITTEN   December 2015
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		//    Validates that the calcualted bypass factor represents valid SHR based on total capacity and air mass flow rate.

		// METHODOLOGY EMPLOYED:
		//    With model parameters autosized by the user, the SHR is selected based on an empirical model.
		//    This can sometimes lead to an SHR that does not cross the saturation curve, or one that crosses excessively where
		//    the coil outlet air conditions exceed the saturation curve (i.e., RH > 1).
		//    This function checks to see if the ADP based on coil delta T and calculated bypass factor and the ADP based
		//    on coil delta W and calculated bypass factor land on the saturation curve at the same place within a tolerance.
		//    The result is passed back to the sizing routine as the new value for SHR.
		//    If the SHR is not autosized, this routine will still adjust the design SHR appropriately, however, the hard-sized SHR will not change.
		// REFERENCES:

		// USE STATEMENTS:
		//    na

		// Return value
		Real64 SHR( 0.0 ); // the result - the adjusted design SHR

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 CBF_calculated( 0.0 ); // coil bypass factor based on TotCap, AirFlow, SHR, and BaroPress
		Real64 InletAirEnthalpy( 0.0 ); // Enthalpy of inlet air to evaporator at given conditions [J/kg]
		Real64 DeltaH( 0.0 ); // Enthalpy drop across evaporator at given conditions [J/kg]
		Real64 HTinHumRatOut( 0.0 ); // Air enthalpy at inlet air temp and outlet air humidity ratio [J/kg]
		Real64 DeltaHumRat( 0.0 ); // Humidity ratio drop across evaporator at given conditions [kg/kg]
		Real64 OutletAirTemp( 0.0 ); // Outlet dry-bulb temperature from evaporator at given conditions [C]
		Real64 OutletAirEnthalpy( 0.0 ); // Enthalpy of outlet air at given conditions [J/kg]
		Real64 OutletAirHumRat( 0.0 ); // Outlet humidity ratio from evaporator at given conditions [kg/kg]
		Real64 OutletAirRH( 0.0 ); // relative humidity of the outlet air
		Real64 CalcADPTemp( 0.0 ); // actual ADP temperature based on bypass factor [C]
		Real64 CalcADPHumRat( 0.0 ); // actual ADP humidity ratio based on bypass factor [kg/kg]
		Real64 CalcADPTempFnHR( 0.0 ); // actual ADP temperature as a function of humidity ratio based on bypass factor [C]
		Real64 ADPerror( 0.0 ); // difference between ADP function of temperature and humidity ratio [deltaC]
		bool bStillValidating( true ); // while loop flag
		bool bNoReporting( false ); // don't report specific warnings in calcCBF while iterating on result
		bool bReversePerturb( false ); // identifies when SHR is being lowered based on outlet air RH

		SHR = InitialSHR;
		while ( bStillValidating ) {
			CBF_calculated = max( 0.0, CalcCBF( UnitType, UnitName, RatedInletAirTemp, RatedInletAirHumRat, TotCap, AirMassFlow, SHR, bNoReporting ) );
			DeltaH = TotCap / AirMassFlow;
			InletAirEnthalpy = PsyHFnTdbW( RatedInletAirTemp, RatedInletAirHumRat );
			HTinHumRatOut = InletAirEnthalpy - ( 1.0 - SHR ) * DeltaH;
			OutletAirHumRat = PsyWFnTdbH( RatedInletAirTemp, HTinHumRatOut, CallingRoutine );
			DeltaHumRat = RatedInletAirHumRat - OutletAirHumRat;
			OutletAirEnthalpy = InletAirEnthalpy - DeltaH;
			OutletAirTemp = PsyTdbFnHW( OutletAirEnthalpy, OutletAirHumRat );
			OutletAirRH = PsyRhFnTdbWPb( OutletAirTemp, OutletAirHumRat, StdBaroPress, CallingRoutine );
			if ( CBF_calculated < 1 ) {
				CalcADPTemp = RatedInletAirTemp - ( ( RatedInletAirTemp - OutletAirTemp ) / ( 1 - CBF_calculated ) );
				CalcADPHumRat = RatedInletAirHumRat - ( ( DeltaHumRat ) / ( 1 - CBF_calculated ) );
				CalcADPTempFnHR = PsyTdpFnWPb( CalcADPHumRat, StdBaroPress, CallingRoutine );
				ADPerror = CalcADPTemp - CalcADPTempFnHR;
			} else {
				ADPerror = 0; // might be able to check for RH >= 1 and reduce SHR, need defect file for that since can't create one
			}

			if ( std::abs( ADPerror )  > 0.012 ) {
				if ( OutletAirRH >= 1.0 ) { // if RH > 1, reduce SHR until it crosses the saturation curve
					SHR -= 0.001;
					bReversePerturb = true;
					if( SHR < 0.5 ) bStillValidating = false; // have to stop somewhere, this is lower than the lower limit of SHR empirical model (see ReportSizingManager SizingType == CoolingSHRSizing)
				} else {
					if ( bReversePerturb ) {
						bStillValidating = false; // stop iterating once SHR causes ADP to cross back under saturation curve, take what you get
					} else {
						SHR += 0.001; // increase SHR slowly until ADP temps are resolved
					}
				}
				if ( SHR > 0.8 ) bStillValidating = false; // have to stop somewhere, this is the upper limit of SHR empirical model (see ReportSizingManager SizingType == CoolingSHRSizing)
			} else {
				bStillValidating = false; // ADP temps are close enough. Normal input files hit this on first pass
			}

		}

		return SHR;

	}

	Real64
	CalcEffectiveSHR(
		int const DXCoilNum, // Index number for cooling coil
		Real64 const SHRss, // Steady-state sensible heat ratio
		Real64 const RTF, // Compressor run-time fraction
		Real64 const QLatRated, // Rated latent capacity
		Real64 const QLatActual, // Actual latent capacity
		Real64 const EnteringDB, // Entering air dry-bulb temperature
		Real64 const EnteringWB, // Entering air wet-bulb temperature
		Optional_int_const Mode, // Performance mode for MultiMode DX coil; Always 1 for other coil types
		Optional< Real64 const > HeatingRTF // Used to recalculate Toff for cycling fan systems
	)
	{

		// FUNCTION INFORMATION:
		//    AUTHOR         Richard Raustad, FSEC
		//    DATE WRITTEN   September 2003
		//                   Feb 2005 M. J. Witte, GARD Analytics, Inc.
		//                    Add new coil type COIL:DX:MultiMode:CoolingEmpirical:
		//                   Nov 2008 R. Raustad, FSEC
		//                    Modified to allow latent degradation with cycling fan
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		//    Adjust sensible heat ratio to account for degradation of DX coil latent
		//    capacity at part-load (cycling) conditions.

		// METHODOLOGY EMPLOYED:
		//    With model parameters entered by the user, the part-load latent performance
		//    of a DX cooling coil is determined for a constant air flow system with
		//    a cooling coil that cycles on/off. The model calculates the time
		//    required for condensate to begin falling from the cooling coil.
		//    Runtimes greater than this are integrated to a "part-load" latent
		//    capacity which is used to determine the "part-load" sensible heat ratio.
		//    See reference below for additional details (linear decay model, Eq. 8b).
		// REFERENCES:
		//   "A Model to Predict the Latent Capacity of Air Conditioners and
		//    Heat Pumps at Part-Load Conditions with Constant Fan Operation"
		//    1996 ASHRAE Transactions, Volume 102, Part 1, Pp. 266 - 274,
		//    Hugh I. Henderson, Jr., P.E., Kannan Rengarajan, P.E.

		// USE STATEMENTS:
		//    na

		// Return value
		Real64 SHReff; // Effective sensible heat ratio, includes degradation due to cycling effects

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 Twet; // Nominal time for condensate to begin leaving the coil's condensate drain line
		//   at the current operating conditions (sec)
		Real64 Gamma; // Initial moisture evaporation rate divided by steady-state AC latent capacity
		//   at the current operating conditions
		Real64 Twet_Rated; // Twet at rated conditions (coil air flow rate and air temperatures), sec
		Real64 Gamma_Rated; // Gamma at rated conditions (coil air flow rate and air temperatures)
		Real64 Twet_max; // Maximum allowed value for Twet
		Real64 Nmax; // Maximum ON/OFF cycles per hour for the compressor (cycles/hr)
		Real64 Tcl; // Time constant for latent capacity to reach steady state after startup (sec)
		Real64 Ton; // Coil on time (sec)
		Real64 Toff; // Coil off time (sec)
		Real64 Toffa; // Actual coil off time (sec). Equations valid for Toff <= (2.0 * Twet/Gamma)
		Real64 aa; // Intermediate variable
		Real64 To1; // Intermediate variable (first guess at To). To = time to the start of moisture removal
		Real64 To2; // Intermediate variable (second guess at To). To = time to the start of moisture removal
		Real64 Error; // Error for iteration (DO) loop
		Real64 LHRmult; // Latent Heat Ratio (LHR) multiplier. The effective latent heat ratio LHR = (1-SHRss)*LHRmult
		Real64 Ton_heating;
		Real64 Toff_heating;

		if ( DXCoil( DXCoilNum ).DXCoilType_Num != CoilDX_MultiSpeedCooling ) {
			Twet_Rated = DXCoil( DXCoilNum ).Twet_Rated( Mode );
			Gamma_Rated = DXCoil( DXCoilNum ).Gamma_Rated( Mode );
			Nmax = DXCoil( DXCoilNum ).MaxONOFFCyclesperHour( Mode );
			Tcl = DXCoil( DXCoilNum ).LatentCapacityTimeConstant( Mode );
		} else {
			Twet_Rated = DXCoil( DXCoilNum ).MSTwet_Rated( Mode );
			Gamma_Rated = DXCoil( DXCoilNum ).MSGamma_Rated( Mode );
			Nmax = DXCoil( DXCoilNum ).MSMaxONOFFCyclesperHour( Mode );
			Tcl = DXCoil( DXCoilNum ).MSLatentCapacityTimeConstant( Mode );
		}

		//  No moisture evaporation (latent degradation) occurs for runtime fraction of 1.0
		//  All latent degradation model parameters cause divide by 0.0 if not greater than 0.0
		//  Latent degradation model parameters initialize to 0.0 meaning no evaporation model used.
		if ( RTF >= 1.0 || Twet_Rated <= 0.0 || Gamma_Rated <= 0.0 || Nmax <= 0.0 || Tcl <= 0.0 ) {
			SHReff = SHRss;
			return SHReff;
		}

		Twet_max = 9999.0; // high limit for Twet

		//  Calculate the model parameters at the actual operating conditions
		Twet = min( Twet_Rated * QLatRated / ( QLatActual + 1.e-10 ), Twet_max );
		Gamma = Gamma_Rated * QLatRated * ( EnteringDB - EnteringWB ) / ( ( 26.7 - 19.4 ) * QLatActual + 1.e-10 );

		//  Calculate the compressor on and off times using a converntional thermostat curve
		Ton = 3600.0 / ( 4.0 * Nmax * ( 1.0 - RTF ) ); // duration of cooling coil on-cycle (sec)
		Toff = 3600.0 / ( 4.0 * Nmax * RTF ); // duration of cooling coil off-cycle (sec)

		//  Cap Toff to meet the equation restriction
		if ( Gamma > 0.0 ) {
			Toffa = min( Toff, 2.0 * Twet / Gamma );
		} else {
			Toffa = Toff;
		}

		//  Need to include the reheat coil operation to account for actual fan run time. E+ uses a
		//  separate heating coil for heating and reheat (to separate the heating and reheat loads)
		//  and real world applications would use a single heating coil for both purposes, the actual
		//  fan operation is based on HeatingPLR + ReheatPLR. For cycling fan RH control, latent
		//  degradation only occurs when a heating load exists, in this case the reheat load is
		//  equal to and oposite in magnitude to the cooling coil sensible output but the reheat
		//  coil is not always active. This additional fan run time has not been accounted for at this time.
		//  Recalculate Toff for cycling fan systems when heating is active
		if ( present( HeatingRTF ) ) {
			if ( HeatingRTF < 1.0 && HeatingRTF > RTF ) {
				Ton_heating = 3600.0 / ( 4.0 * Nmax * ( 1.0 - HeatingRTF ) );
				Toff_heating = 3600.0 / ( 4.0 * Nmax * HeatingRTF );
				//    add additional heating coil operation during cooling coil off cycle (due to cycling rate difference of coils)
				Ton_heating += max( 0.0, min( Ton_heating, ( Ton + Toffa ) - ( Ton_heating + Toff_heating ) ) );
				Toffa = min( Toffa, Ton_heating - Ton );
			}
		}

		//  Use sucessive substitution to solve for To
		aa = ( Gamma * Toffa ) - ( 0.25 / Twet ) * pow_2( Gamma ) * pow_2( Toffa );
		To1 = aa + Tcl;
		Error = 1.0;
		while ( Error > 0.001 ) {
			To2 = aa - Tcl * ( std::exp( -To1 / Tcl ) - 1.0 );
			Error = std::abs( ( To2 - To1 ) / To1 );
			To1 = To2;
		}

		//  Adjust Sensible Heat Ratio (SHR) using Latent Heat Ratio (LHR) multiplier
		//  Floating underflow errors occur when -Ton/Tcl is a large negative number.
		//  Cap lower limit at -700 to avoid the underflow errors.
		aa = std::exp( max( -700.0, - Ton / Tcl ) );
		//  Calculate latent heat ratio multiplier
		LHRmult = max( ( ( Ton - To2 ) / ( Ton + Tcl * ( aa - 1.0 ) ) ), 0.0 );

		//  Calculate part-load or "effective" sensible heat ratio
		SHReff = 1.0 - ( 1.0 - SHRss ) * LHRmult;

		if ( SHReff < SHRss ) SHReff = SHRss; // Effective SHR can be less than the steady-state SHR
		if ( SHReff > 1.0 ) SHReff = 1.0; // Effective sensible heat ratio can't be greater than 1.0

		return SHReff;

	}

	void
	CalcTotCapSHR(
		Real64 const InletDryBulb, // inlet air dry bulb temperature [C]
		Real64 const InletHumRat, // inlet air humidity ratio [kg water / kg dry air]
		Real64 const InletEnthalpy, // inlet air specific enthalpy [J/kg]
		Real64 const InletWetBulb, // inlet air wet bulb temperature [C]
		Real64 const AirMassFlowRatio, // Ratio of actual air mass flow to nominal air mass flow
		Real64 const AirMassFlow, // actual mass flow for capacity and SHR calculation
		Real64 const TotCapNom, // nominal total capacity [W]
		Real64 const CBF, // coil bypass factor
		int const CCapFTemp, // capacity modifier curve index, function of entering wetbulb
		int const CCapFFlow, // capacity modifier curve, function of actual flow vs rated flow
		Real64 & TotCap, // total capacity at the given conditions [W]
		Real64 & SHR, // sensible heat ratio at the given conditions
		Real64 const CondInletTemp, // Condenser inlet temperature [C]
		Real64 const Pressure // air pressure [Pa]
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl using Don Shirey's code
		//       DATE WRITTEN   September 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates total capacity and sensible heat ratio of a DX coil at the specified conditions

		// METHODOLOGY EMPLOYED:
		// With the rated performance data entered by the user, the model employs some of the
		// DOE-2.1E curve fits to adjust the capacity and SHR of the unit as a function
		// of entering air temperatures and supply air flow rate (actual vs rated flow). The model
		// does NOT employ the exact same methodology to calculate performance as DOE-2, although
		// some of the DOE-2 curve fits are employed by this model.

		// The model checks for coil dryout conditions, and adjusts the calculated performance
		// appropriately.

		// REFERENCES:
		// ASHRAE HVAC 2 Toolkit page 4-81.
		// Henderson, H.I. Jr., K. Rengarajan and D.B. Shirey, III. 1992.The impact of comfort
		// control on air conditioner energy use in humid climates. ASHRAE Transactions 98(2):
		// 104-113.
		// Henderson, H.I. Jr., Danny Parker and Y.J. Huang. 2000.Improving DOE-2's RESYS routine:
		// User Defined Functions to Provide More Accurate Part Load Energy Use and Humidity
		// Predictions. Proceedings of ACEEE Conference.

		// Using/Aliasing
		using CurveManager::CurveValue;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// and outside drybulb

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcTotCapSHR" );
		int const MaxIter( 30 ); // Maximum number of iterations for dry evaporator calculations
		Real64 const RF( 0.4 ); // Relaxation factor for dry evaporator iterations
		Real64 const Tolerance( 0.01 ); // Error tolerance for dry evaporator iterations
		Real64 const MinHumRatCalc( 0.00001 ); // Error tolerance for dry evaporator iterations

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 InletWetBulbCalc; // calculated inlet wetbulb temperature used for finding dry coil point [C]
		Real64 InletHumRatCalc; // calculated inlet humidity ratio used for finding dry coil point [kg water / kg dry air]
		Real64 TotCapTempModFac; // Total capacity modifier (function of entering wetbulb, outside drybulb)
		Real64 TotCapFlowModFac; // Total capacity modifier (function of actual supply air flow vs nominal flow)
		Real64 hDelta; // Change in air enthalpy across the cooling coil [J/kg]
		Real64 hADP; // Apparatus dew point enthalpy [J/kg]
		Real64 tADP; // Apparatus dew point temperature [C]
		Real64 wADP; // Apparatus dew point humidity ratio [kg/kg]
		Real64 hTinwADP; // Enthalpy at inlet dry-bulb and wADP [J/kg]
		Real64 SHRCalc; // temporary calculated value of SHR
		Real64 TotCapCalc; // temporary calculated value of total capacity [W]
		int Counter; // Counter for dry evaporator iterations
		Real64 werror; // Deviation of humidity ratio in dry evaporator iteration loop

		//  MaxIter = 30
		//  RF = 0.4d0
		Counter = 0;
		//  Tolerance = 0.01d0
		werror = 0.0;

		InletWetBulbCalc = InletWetBulb;
		InletHumRatCalc = InletHumRat;

		//  DO WHILE (ABS(werror) .gt. Tolerance .OR. Counter == 0)
		//   Get capacity modifying factor (function of inlet wetbulb & outside drybulb) for off-rated conditions
		while ( true ) {
			TotCapTempModFac = CurveValue( CCapFTemp, InletWetBulbCalc, CondInletTemp );
			//   Get capacity modifying factor (function of mass flow) for off-rated conditions
			TotCapFlowModFac = CurveValue( CCapFFlow, AirMassFlowRatio );
			//   Get total capacity
			TotCapCalc = TotCapNom * TotCapFlowModFac * TotCapTempModFac;

			//   Calculate apparatus dew point conditions using TotCap and CBF
			hDelta = TotCapCalc / AirMassFlow;
			hADP = InletEnthalpy - hDelta / ( 1.0 - CBF );
			tADP = PsyTsatFnHPb( hADP, Pressure );
			wADP = PsyWFnTdbH( tADP, hADP );
			hTinwADP = PsyHFnTdbW( InletDryBulb, wADP );
			if ( ( InletEnthalpy - hADP ) > 1.e-10 ) {
				SHRCalc = min( ( hTinwADP - hADP ) / ( InletEnthalpy - hADP ), 1.0 );
			} else {
				SHRCalc = 1.0;
			}
			//   Check for dry evaporator conditions (win < wadp)
			if ( wADP > InletHumRatCalc || ( Counter >= 1 && Counter < MaxIter ) ) {
				if ( InletHumRatCalc == 0.0 ) InletHumRatCalc = MinHumRatCalc;
				//      InletHumRatCalc=MAX(InletHumRatCalc,MinHumRatCalc)  ! proposed.

				werror = ( InletHumRatCalc - wADP ) / InletHumRatCalc;
				//     Increase InletHumRatCalc at constant inlet air temp to find coil dry-out point. Then use the
				//     capacity at the dry-out point to determine exiting conditions from coil. This is required
				//     since the TotCapTempModFac doesn't work properly with dry-coil conditions.
				InletHumRatCalc = RF * wADP + ( 1.0 - RF ) * InletHumRatCalc;
				InletWetBulbCalc = PsyTwbFnTdbWPb( InletDryBulb, InletHumRatCalc, Pressure );
				++Counter;
				if ( std::abs( werror ) > Tolerance ) continue; // Recalculate with modified inlet conditions
				break; // conditions are satisfied
			} else {
				break; // conditions are satisfied
			}
		}

		// END DO

		//  Calculate full load output conditions
		if ( SHRCalc > 1.0 || Counter > 0 ) SHRCalc = 1.0;

		SHR = SHRCalc;
		TotCap = TotCapCalc;

	}

	void
	CalcMultiSpeedDXCoilCooling(
		int const DXCoilNum, // the number of the DX heating coil to be simulated
		Real64 const SpeedRatio, // = (CompressorSpeed - CompressorSpeedMin) / (CompressorSpeedMax - CompressorSpeedMin)
		Real64 const CycRatio, // cycling part load ratio
		int const SpeedNum, // Speed number
		int const FanOpMode, // Sets fan control to CycFanCycCoil or ContFanCycCoil
		int const CompOp, // Compressor on/off; 1=on, 0=off
		int const SingleMode // Single mode operation Yes/No; 1=Yes, 0=No
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Lixing Gu, FSEC
		//       DATE WRITTEN   June 2007
		//       MODIFIED       April 2010, Chandan sharma, FSEC, added basin heater
		//       RE-ENGINEERED  Revised based on CalcMultiSpeedDXCoil

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the air-side performance and electrical energy use of a direct-
		// expansion, air-cooled cooling unit with a multispeed compressor.

		// METHODOLOGY EMPLOYED:
		// Uses the same methodology as the single speed DX unit model (SUBROUTINE CalcDoe2DXCoil).
		// In addition it assumes that the unit performance is obtained by interpolating between
		// the performance at high speed and that at low speed. If the output needed is below
		// that produced at low speed, the compressor cycles between off and low speed.

		// Using/Aliasing
		using CurveManager::CurveValue;
		using DataWater::WaterStorage;
		using DataHVACGlobals::MSHPMassFlowRateLow;
		using DataHVACGlobals::MSHPMassFlowRateHigh;
		using DataHVACGlobals::MSHPWasteHeat;
		using General::TrimSigDigits;
		using General::RoundSigDigits;
		using DataHeatBalFanSys::ZoneAirHumRat;
		using DataHeatBalFanSys::ZT;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// SpeedRatio varies between 1.0 (maximum speed) and 0.0 (minimum speed)

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcMultiSpeedDXCoilCooling" );
		static std::string const RoutineNameHighSpeed( "CalcMultiSpeedDXCoilCooling:highspeed" );
		static std::string const RoutineNameHighSpeedAlt( "CalcMultiSpeedDXCoilCooling highspeed" );
		static std::string const RoutineNameHighSpeedOutlet( "CalcMultiSpeedDXCoilCooling:highspeedoutlet" );
		static std::string const RoutineNameLowSpeedOutlet( "CalcMultiSpeedDXCoilCooling:lowspeedoutlet" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 AirMassFlow; // dry air mass flow rate through coil [kg/s]
		Real64 InletAirWetBulbC; // wetbulb temperature of inlet air [C]
		Real64 InletAirDryBulbTemp; // inlet air dry bulb temperature [C]
		Real64 InletAirEnthalpy; // inlet air enthalpy [J/kg]
		Real64 InletAirHumRat; // inlet air humidity ratio [kg/kg]
		//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
		//REAL(r64)   :: InletAirPressure    ! inlet air pressure [Pa]
		Real64 OutletAirDryBulbTemp; // outlet air dry bulb temperature [C]
		Real64 OutletAirEnthalpy; // outlet air enthalpy [J/kg]
		Real64 OutletAirHumRat; // outlet air humidity ratio [kg/kg]
		//REAL(r64)   :: OutletAirRH         ! outlet air relative humudity [fraction]
		Real64 OutletAirDryBulbTempSat; // outlet air dry bulb temp at saturation at the outlet enthalpy [C]
		Real64 LSOutletAirDryBulbTemp; // low speed outlet air dry bulb temperature [C]
		Real64 LSOutletAirEnthalpy; // low speed outlet air enthalpy [J/kg]
		Real64 LSOutletAirHumRat; // low speed outlet air humidity ratio [kg/kg]
		Real64 LSOutletAirRH; // low speed outlet air relative humudity [fraction]
		Real64 HSOutletAirDryBulbTemp; // hihg speed outlet air dry bulb temperature [C]
		Real64 HSOutletAirEnthalpy; // high speed outlet air enthalpy [J/kg]
		Real64 HSOutletAirHumRat; // high speed outlet air humidity ratio [kg/kg]
		Real64 HSOutletAirRH; // high speed outlet air relative humudity [fraction]
		Real64 hDelta; // Change in air enthalpy across the cooling coil [J/kg]
		Real64 hTinwout; // Enthalpy at inlet dry-bulb and outlet humidity ratio [J/kg]
		Real64 hADP; // Apparatus dew point enthalpy [J/kg]
		Real64 tADP; // Apparatus dew point temperature [C]
		Real64 wADP; // Apparatus dew point humidity ratio [kg/kg]
		Real64 hTinwADP; // Enthalpy at inlet dry-bulb and wADP [J/kg]
		Real64 RatedCBFHS; // coil bypass factor at rated conditions (high speed)
		Real64 CBFHS; // coil bypass factor at max flow (high speed)
		Real64 RatedCBFLS; // coil bypass factor at rated conditions (low speed)
		Real64 CBFLS; // coil bypass factor at max flow (low speed)
		Real64 TotCapHS; // total capacity at high speed [W]
		Real64 SHRHS; // sensible heat ratio at high speed
		Real64 TotCapLS; // total capacity at low speed [W]
		Real64 SHRLS; // sensible heat ratio at low speed
		Real64 EIRTempModFacHS; // EIR modifier (function of entering wetbulb, outside drybulb) (high speed)
		Real64 EIRFlowModFacHS; // EIR modifier (function of actual supply air flow vs rated flow) (high speed)
		Real64 EIRHS; // EIR at off rated conditions (high speed)
		Real64 EIRTempModFacLS; // EIR modifier (function of entering wetbulb, outside drybulb) (low speed)
		Real64 EIRFlowModFacLS; // EIR modifier (function of actual supply air flow vs rated flow) (low speed)
		Real64 EIRLS; // EIR at off rated conditions (low speed)
		Real64 SHR; // sensible heat ratio at current speed
		Real64 EIR; // EIR at current speed
		Real64 CBF; // CBFNom adjusted for actual air mass flow rate
		Real64 PLF; // Part load factor, accounts for thermal lag at compressor startup, used in
		// power calculation
		Real64 CondInletTemp; // Condenser inlet temperature (C). Outdoor dry-bulb temp for air-cooled condenser.
		// Outdoor Wetbulb +(1 - effectiveness)*(outdoor drybulb - outdoor wetbulb) for evap condenser.
		Real64 CondInletHumRat; // Condenser inlet humidity ratio (kg/kg). Zero for air-cooled condenser.
		// For evap condenser, its the humidity ratio of the air leaving the evap cooling pads.
		Real64 RhoAir; // Density of air [kg/m3]
		Real64 RhoWater; // Density of water [kg/m3]
		Real64 CondAirMassFlow; // Condenser air mass flow rate [kg/s]
		Real64 EvapCondPumpElecPower; // Evaporative condenser electric pump power [W]
		static Real64 MinAirHumRat( 0.0 ); // minimum of the inlet air humidity ratio and the outlet air humidity ratio
		static int DXMode( 1 ); // Performance mode for MultiMode DX coil; Always 1 for other coil types
		Real64 OutdoorDryBulb; // Outdoor dry-bulb temperature at condenser (C)
		Real64 OutdoorWetBulb; // Outdoor wet-bulb temperature at condenser (C)
		Real64 OutdoorHumRat; // Outdoor humidity ratio at condenser (kg/kg)
		Real64 OutdoorPressure; // Outdoor barometric pressure at condenser (Pa)
		int SpeedNumHS; // High speed number
		int SpeedNumLS; // Low speed number
		Real64 SHRUnadjusted; // Temp SHR
		Real64 QLatRated; // Qlatent at rated conditions of indoor(TDB,TWB)=(26.7C,19.4C)
		Real64 QLatActual; // Qlatent at actual operating conditions
		Real64 AirMassFlowRatioLS; // airflow ratio at low speed
		Real64 AirMassFlowRatioHS; // airflow ratio at high speed
		Real64 WasteHeatLS; // Waste heat at low speed
		Real64 WasteHeatHS; // Waste heat at high speed
		Real64 LSElecCoolingPower; // low speed power [W]
		Real64 HSElecCoolingPower; // high speed power [W]
		Real64 CrankcaseHeatingPower; // Power due to crank case heater
		Real64 Hfg;
		Real64 AirVolumeFlowRate; // Air volume flow rate across the heating coil
		Real64 VolFlowperRatedTotCap; // Air volume flow rate divided by rated total heating capacity

		if ( DXCoil( DXCoilNum ).CondenserInletNodeNum( DXMode ) != 0 ) {
			OutdoorPressure = Node( DXCoil( DXCoilNum ).CondenserInletNodeNum( DXMode ) ).Press;
			// If node is not connected to anything, pressure = default, use weather data
			if ( OutdoorPressure == DefaultNodeValues.Press ) {
				OutdoorDryBulb = OutDryBulbTemp;
				OutdoorHumRat = OutHumRat;
				OutdoorPressure = OutBaroPress;
				OutdoorWetBulb = OutWetBulbTemp;
			} else {
				OutdoorDryBulb = Node( DXCoil( DXCoilNum ).CondenserInletNodeNum( DXMode ) ).Temp;
				OutdoorHumRat = Node( DXCoil( DXCoilNum ).CondenserInletNodeNum( DXMode ) ).HumRat;
				OutdoorWetBulb = PsyTwbFnTdbWPb( OutdoorDryBulb, OutdoorHumRat, OutdoorPressure, RoutineName );
			}
			if ( DXCoil( DXCoilNum ).IsSecondaryDXCoilInZone ) {
				OutdoorDryBulb = ZT( DXCoil( DXCoilNum ).SecZonePtr );
				OutdoorHumRat = ZoneAirHumRat( DXCoil( DXCoilNum ).SecZonePtr );
				OutdoorWetBulb = DXCoil( DXCoilNum ).EvapInletWetBulb;
				OutdoorPressure = OutBaroPress;
			}
		} else if ( DXCoil( DXCoilNum ).IsSecondaryDXCoilInZone ) {
			OutdoorDryBulb = ZT( DXCoil( DXCoilNum ).SecZonePtr );
			OutdoorHumRat = ZoneAirHumRat( DXCoil( DXCoilNum ).SecZonePtr );
			OutdoorWetBulb = DXCoil( DXCoilNum ).EvapInletWetBulb;
			OutdoorPressure = OutBaroPress;
		} else {
			OutdoorDryBulb = OutDryBulbTemp;
			OutdoorHumRat = OutHumRat;
			OutdoorPressure = OutBaroPress;
			OutdoorWetBulb = OutWetBulbTemp;
		}

		if ( SpeedNum > 1 ) {
			SpeedNumLS = SpeedNum - 1;
			SpeedNumHS = SpeedNum;
			if ( SpeedNum > DXCoil( DXCoilNum ).NumOfSpeeds ) {
				SpeedNumLS = DXCoil( DXCoilNum ).NumOfSpeeds - 1;
				SpeedNumHS = DXCoil( DXCoilNum ).NumOfSpeeds;
			}
		} else {
			SpeedNumLS = 1;
			SpeedNumHS = 1;
		}

		MSHPWasteHeat = 0.0;
		AirMassFlow = DXCoil( DXCoilNum ).InletAirMassFlowRate;
		AirMassFlowRatioLS = MSHPMassFlowRateLow / DXCoil( DXCoilNum ).MSRatedAirMassFlowRate( SpeedNumLS );
		AirMassFlowRatioHS = MSHPMassFlowRateHigh / DXCoil( DXCoilNum ).MSRatedAirMassFlowRate( SpeedNumHS );
		if ( ( AirMassFlow > 0.0 ) && ( CycRatio > 0.0 ) && ( ( MSHPMassFlowRateLow == 0.0 ) || ( MSHPMassFlowRateHigh == 0.0 ) ) ) {
			ShowSevereError( "CalcMultiSpeedDXCoilCooling: " + DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + " Developer error - inconsistent airflow rates." );
			ShowContinueError( "When AirMassFlow > 0.0 and CycRatio > 0.0, then MSHPMassFlowRateLow and MSHPMassFlowRateHigh must also be > 0.0" );
			ShowContinueErrorTimeStamp( "" );
			ShowContinueError( "AirMassFlow = " + RoundSigDigits( AirMassFlow, 3 ) + ",CycRatio = " + RoundSigDigits( CycRatio, 3 ) + ", MSHPMassFlowRateLow = " + RoundSigDigits( MSHPMassFlowRateLow, 3 ) + ", MSHPMassFlowRateHigh = " + RoundSigDigits( MSHPMassFlowRateHigh, 3 ) );
			ShowFatalError( "Preceding condition(s) causes termination." );
		} else if ( CycRatio > 1.0 || SpeedRatio > 1.0 ) {
			ShowSevereError( "CalcMultiSpeedDXCoilCooling: " + DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + " Developer error - inconsistent speed ratios." );
			ShowContinueError( "CycRatio and SpeedRatio must be between 0.0 and 1.0" );
			ShowContinueErrorTimeStamp( "" );
			ShowContinueError( "CycRatio=" + RoundSigDigits( CycRatio, 1 ) + ", SpeedRatio = " + RoundSigDigits( SpeedRatio, 1 ) );
			ShowFatalError( "Preceding condition(s) causes termination." );
		}

		DXCoil( DXCoilNum ).PartLoadRatio = 0.0;
		HeatReclaimDXCoil( DXCoilNum ).AvailCapacity = 0.0;
		DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction = 0.0;
		InletAirDryBulbTemp = DXCoil( DXCoilNum ).InletAirTemp;
		InletAirEnthalpy = DXCoil( DXCoilNum ).InletAirEnthalpy;
		InletAirHumRat = DXCoil( DXCoilNum ).InletAirHumRat;
		//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
		//InletAirPressure = DXCoil(DXCoilNum)%InletAirPressure
		//InletAirWetBulbC = PsyTwbFnTdbWPb(InletAirDryBulbTemp,InletAirHumRat,InletAirPressure)
		InletAirWetBulbC = PsyTwbFnTdbWPb( InletAirDryBulbTemp, InletAirHumRat, OutdoorPressure, RoutineName );
		if ( DXCoil( DXCoilNum ).CondenserType( DXMode ) == AirCooled ) {
			CondInletTemp = OutdoorDryBulb; // Outdoor dry-bulb temp
		} else if ( DXCoil( DXCoilNum ).CondenserType( DXMode ) == EvapCooled ) {
			// Outdoor wet-bulb temp from DataEnvironment + (1.0-EvapCondEffectiveness) * (drybulb - wetbulb)
			CondInletTemp = OutdoorWetBulb + ( OutdoorDryBulb - OutdoorWetBulb ) * ( 1.0 - DXCoil( DXCoilNum ).MSEvapCondEffect( SpeedNumHS ) );
			CondInletHumRat = PsyWFnTdbTwbPb( CondInletTemp, OutdoorWetBulb, OutdoorPressure, RoutineName );
		}
		if ( OutdoorDryBulb < DXCoil( DXCoilNum ).MaxOATCrankcaseHeater ) {
			CrankcaseHeatingPower = DXCoil( DXCoilNum ).CrankcaseHeaterCapacity;
		} else {
			CrankcaseHeatingPower = 0.0;
		}

		if ( ( AirMassFlow > 0.0 ) && ( GetCurrentScheduleValue( DXCoil( DXCoilNum ).SchedPtr ) > 0.0 ) && ( ( SpeedRatio > 0.0 && SingleMode == 0 ) || CycRatio > 0.0 ) && ( CompOp == On ) ) {

			RhoAir = PsyRhoAirFnPbTdbW( OutdoorPressure, OutdoorDryBulb, OutdoorHumRat, RoutineName );
			if ( SpeedNum > 1 && SingleMode == 0 ) {

				// Check for valid air volume flow per rated total cooling capacity (200 - 500 cfm/ton) at low speed
				AirVolumeFlowRate = MSHPMassFlowRateLow / PsyRhoAirFnPbTdbW( OutdoorPressure, InletAirDryBulbTemp, InletAirHumRat, RoutineName );
				//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
				//  AirVolumeFlowRate = AirMassFlow/PsyRhoAirFnPbTdbW(InletAirPressure,InletAirDryBulbTemp, InletAirHumRat)
				VolFlowperRatedTotCap = AirVolumeFlowRate / DXCoil( DXCoilNum ).MSRatedTotCap( SpeedNumLS );
				if ( ( VolFlowperRatedTotCap < MinOperVolFlowPerRatedTotCap( DXCT ) ) || ( VolFlowperRatedTotCap > MaxCoolVolFlowPerRatedTotCap( DXCT ) ) ) {
					if ( DXCoil( DXCoilNum ).MSErrIndex( SpeedNumLS ) == 0 ) {
						ShowWarningMessage( DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + "\" - Air volume flow rate per watt of rated total cooling capacity is out of range at speed " + TrimSigDigits( SpeedNumLS ) + '.' );
						ShowContinueErrorTimeStamp( "" );
						ShowContinueError( "Expected range for VolumeFlowPerRatedTotalCapacity=[" + RoundSigDigits( MinOperVolFlowPerRatedTotCap( DXCT ), 3 ) + "--" + RoundSigDigits( MaxCoolVolFlowPerRatedTotCap( DXCT ), 3 ) + "] Current value is " + RoundSigDigits( VolFlowperRatedTotCap, 3 ) + " m3/s/W" );
						ShowContinueError( "Possible causes include inconsistent air flow rates in system components or" );
						ShowContinueError( "inconsistent supply air fan operation modes in coil and unitary system objects." );
					}
					ShowRecurringWarningErrorAtEnd( DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + "\" - Air volume flow rate per watt of rated total cooling capacity is out of range at speed " + TrimSigDigits( SpeedNumLS ) + " error continues...", DXCoil( DXCoilNum ).MSErrIndex( SpeedNumLS ), VolFlowperRatedTotCap, VolFlowperRatedTotCap );
				}

				// Check for valid air volume flow per rated total cooling capacity (200 - 500 cfm/ton) at high speed
				AirVolumeFlowRate = MSHPMassFlowRateHigh / PsyRhoAirFnPbTdbW( OutdoorPressure, InletAirDryBulbTemp, InletAirHumRat, RoutineName );
				//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
				//  AirVolumeFlowRate = AirMassFlow/PsyRhoAirFnPbTdbW(InletAirPressure,InletAirDryBulbTemp, InletAirHumRat)
				VolFlowperRatedTotCap = AirVolumeFlowRate / DXCoil( DXCoilNum ).MSRatedTotCap( SpeedNumHS );
				if ( ( VolFlowperRatedTotCap < MinOperVolFlowPerRatedTotCap( DXCT ) ) || ( VolFlowperRatedTotCap > MaxCoolVolFlowPerRatedTotCap( DXCT ) ) ) {
					if ( DXCoil( DXCoilNum ).MSErrIndex( SpeedNumHS ) == 0 ) {
						ShowWarningMessage( DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + "\" - Air volume flow rate per watt of rated total cooling capacity is out of range at speed " + TrimSigDigits( SpeedNumHS ) + '.' );
						ShowContinueErrorTimeStamp( "" );
						ShowContinueError( "Expected range for VolumeFlowPerRatedTotalCapacity=[" + RoundSigDigits( MinOperVolFlowPerRatedTotCap( DXCT ), 3 ) + "--" + RoundSigDigits( MaxCoolVolFlowPerRatedTotCap( DXCT ), 3 ) + "] Current value is " + RoundSigDigits( VolFlowperRatedTotCap, 3 ) + " m3/s/W" );
						ShowContinueError( "Possible causes include inconsistent air flow rates in system components or" );
						ShowContinueError( "inconsistent supply air fan operation modes in coil and unitary system objects." );
					}
					ShowRecurringWarningErrorAtEnd( DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + "\" - Air volume flow rate per watt of rated total cooling capacity is out of range at speed " + TrimSigDigits( SpeedNumHS ) + " error continues...", DXCoil( DXCoilNum ).MSErrIndex( SpeedNumHS ), VolFlowperRatedTotCap, VolFlowperRatedTotCap );
				}

				// Adjust high speed coil bypass factor for actual maximum air flow rate.
				RatedCBFHS = DXCoil( DXCoilNum ).MSRatedCBF( SpeedNumHS );
				CBFHS = AdjustCBF( RatedCBFHS, DXCoil( DXCoilNum ).MSRatedAirMassFlowRate( SpeedNumHS ), MSHPMassFlowRateHigh );
				RatedCBFLS = DXCoil( DXCoilNum ).MSRatedCBF( SpeedNumLS );
				CBFLS = AdjustCBF( RatedCBFLS, DXCoil( DXCoilNum ).MSRatedAirMassFlowRate( SpeedNumLS ), MSHPMassFlowRateLow );
				// get low speed total capacity and SHR at current conditions
				CalcTotCapSHR( InletAirDryBulbTemp, InletAirHumRat, InletAirEnthalpy, InletAirWetBulbC, AirMassFlowRatioLS, MSHPMassFlowRateLow, DXCoil( DXCoilNum ).MSRatedTotCap( SpeedNumLS ), CBFLS, DXCoil( DXCoilNum ).MSCCapFTemp( SpeedNumLS ), DXCoil( DXCoilNum ).MSCCapFFlow( SpeedNumLS ), TotCapLS, SHRLS, CondInletTemp, OutdoorPressure );
				// get low speed outlet conditions
				hDelta = TotCapLS / MSHPMassFlowRateLow;
				// Calculate new apparatus dew point conditions
				hADP = InletAirEnthalpy - hDelta / ( 1.0 - CBFLS );
				tADP = PsyTsatFnHPb( hADP, OutdoorPressure, RoutineNameHighSpeedAlt );
				//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
				//  tADP = PsyTsatFnHPb(hADP,InletAirPressure)
				wADP = PsyWFnTdbH( tADP, hADP, RoutineName );
				hTinwADP = PsyHFnTdbW( InletAirDryBulbTemp, wADP );
				// get corresponding SHR
				if ( ( InletAirEnthalpy - hADP ) > 1.e-10 ) {
					SHRLS = min( ( hTinwADP - hADP ) / ( InletAirEnthalpy - hADP ), 1.0 );
				} else {
					SHRLS = 1.0;
				}
				//cr8918    SHRLS = MIN((hTinwADP-hADP)/(InletAirEnthalpy-hADP),1.0d0)
				// get low speed outlet conditions
				LSOutletAirEnthalpy = InletAirEnthalpy - hDelta;
				hTinwout = InletAirEnthalpy - ( 1.0 - SHRLS ) * hDelta;
				LSOutletAirHumRat = PsyWFnTdbH( InletAirDryBulbTemp, hTinwout, RoutineName );
				LSOutletAirDryBulbTemp = PsyTdbFnHW( LSOutletAirEnthalpy, LSOutletAirHumRat );
				LSOutletAirRH = PsyRhFnTdbWPb( LSOutletAirDryBulbTemp, LSOutletAirHumRat, OutdoorPressure, RoutineNameHighSpeed );
				OutletAirDryBulbTempSat = PsyTsatFnHPb( LSOutletAirEnthalpy, OutdoorPressure, RoutineName );
				//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
				//    LSOutletAirRH = PsyRhFnTdbWPb(LSOutletAirDryBulbTemp,LSOutletAirHumRat,InletAirPressure)
				//    OutletAirDryBulbTempSat = PsyTsatFnHPb(LSOutletAirEnthalpy,InletAirPressure)
				if ( LSOutletAirDryBulbTemp < OutletAirDryBulbTempSat ) { // Limit to saturated conditions at OutletAirEnthalpy
					LSOutletAirDryBulbTemp = OutletAirDryBulbTempSat;
					LSOutletAirHumRat = PsyWFnTdbH( LSOutletAirDryBulbTemp, LSOutletAirEnthalpy, RoutineName );
				}

				// get high speed total capacity and SHR at current conditions

				CalcTotCapSHR( InletAirDryBulbTemp, InletAirHumRat, InletAirEnthalpy, InletAirWetBulbC, AirMassFlowRatioHS, MSHPMassFlowRateHigh, DXCoil( DXCoilNum ).MSRatedTotCap( SpeedNumHS ), CBFHS, DXCoil( DXCoilNum ).MSCCapFTemp( SpeedNumHS ), DXCoil( DXCoilNum ).MSCCapFFlow( SpeedNumHS ), TotCapHS, SHRHS, CondInletTemp, OutdoorPressure );
				hDelta = TotCapHS / MSHPMassFlowRateHigh;
				// Calculate new apparatus dew point conditions
				hADP = InletAirEnthalpy - hDelta / ( 1.0 - CBFHS );
				tADP = PsyTsatFnHPb( hADP, OutdoorPressure, RoutineName );
				//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
				//  tADP = PsyTsatFnHPb(hADP,InletAirPressure)
				wADP = PsyWFnTdbH( tADP, hADP, RoutineName );
				hTinwADP = PsyHFnTdbW( InletAirDryBulbTemp, wADP );
				// get corresponding SHR
				if ( ( InletAirEnthalpy - hADP ) > 1.e-10 ) {
					SHRHS = min( ( hTinwADP - hADP ) / ( InletAirEnthalpy - hADP ), 1.0 );
				} else {
					SHRHS = 1.0;
				}
				//cr8918    SHRHS = MIN((hTinwADP-hADP)/(InletAirEnthalpy-hADP),1.0d0)
				// get the part load factor that will account for cycling losses
				PLF = CurveValue( DXCoil( DXCoilNum ).MSPLFFPLR( SpeedNumHS ), SpeedRatio );
				if ( PLF < 0.7 ) {
					PLF = 0.7;
				}
				// calculate the run time fraction
				DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction = SpeedRatio / PLF;
				DXCoil( DXCoilNum ).PartLoadRatio = SpeedRatio;

				if ( DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction > 1.0 ) {
					DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction = 1.0; // Reset coil runtime fraction to 1.0
				}

				// get high speed outlet conditions
				HSOutletAirEnthalpy = InletAirEnthalpy - hDelta;
				hTinwout = InletAirEnthalpy - ( 1.0 - SHRHS ) * hDelta;
				HSOutletAirHumRat = PsyWFnTdbH( InletAirDryBulbTemp, hTinwout, RoutineName );
				HSOutletAirDryBulbTemp = PsyTdbFnHW( HSOutletAirEnthalpy, HSOutletAirHumRat );
				HSOutletAirRH = PsyRhFnTdbWPb( HSOutletAirDryBulbTemp, HSOutletAirHumRat, OutdoorPressure, RoutineNameHighSpeedOutlet );
				OutletAirDryBulbTempSat = PsyTsatFnHPb( HSOutletAirEnthalpy, OutdoorPressure, RoutineName );
				//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
				//    LSOutletAirRH = PsyRhFnTdbWPb(LSOutletAirDryBulbTemp,LSOutletAirHumRat,InletAirPressure)
				//    OutletAirDryBulbTempSat = PsyTsatFnHPb(LSOutletAirEnthalpy,InletAirPressure)
				if ( HSOutletAirDryBulbTemp < OutletAirDryBulbTempSat ) { // Limit to saturated conditions at OutletAirEnthalpy
					HSOutletAirDryBulbTemp = OutletAirDryBulbTempSat;
					HSOutletAirHumRat = PsyWFnTdbH( HSOutletAirDryBulbTemp, HSOutletAirEnthalpy, RoutineName );
				}

				//  If constant fan with cycling compressor, call function to determine "effective SHR"
				//  which includes the part-load degradation on latent capacity
				if ( DXCoil( DXCoilNum ).LatentImpact && FanOpMode == ContFanCycCoil && SpeedRatio > 0.0 ) {
					QLatRated = DXCoil( DXCoilNum ).MSRatedTotCap( SpeedNumHS ) * ( 1.0 - DXCoil( DXCoilNum ).MSRatedSHR( SpeedNumHS ) );
					QLatActual = TotCapHS * ( 1.0 - SHRHS );
					SHRUnadjusted = SHRHS;
					SHR = CalcEffectiveSHR( DXCoilNum, SHRHS, DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction, QLatRated, QLatActual, InletAirDryBulbTemp, InletAirWetBulbC, SpeedNumHS );
					// Calculate full load output conditions
					if ( SHR > 1.0 ) SHR = 1.0;
					hTinwout = InletAirEnthalpy - ( 1.0 - SHR ) * hDelta;
					if ( SHR < 1.0 ) {
						HSOutletAirHumRat = PsyWFnTdbH( InletAirDryBulbTemp, hTinwout, RoutineName );
					} else {
						HSOutletAirHumRat = InletAirHumRat;
					}
					HSOutletAirDryBulbTemp = PsyTdbFnHW( HSOutletAirEnthalpy, HSOutletAirHumRat );
				}

				// get high speed EIR at current conditions
				EIRTempModFacHS = CurveValue( DXCoil( DXCoilNum ).MSEIRFTemp( SpeedNumHS ), InletAirWetBulbC, CondInletTemp );
				EIRFlowModFacHS = CurveValue( DXCoil( DXCoilNum ).MSEIRFFlow( SpeedNumHS ), AirMassFlowRatioHS );
				EIRHS = 1.0 / DXCoil( DXCoilNum ).MSRatedCOP( SpeedNumHS ) * EIRFlowModFacHS * EIRTempModFacHS;
				// get low speed EIR at current conditions
				EIRTempModFacLS = CurveValue( DXCoil( DXCoilNum ).MSEIRFTemp( SpeedNumLS ), InletAirWetBulbC, CondInletTemp );
				EIRFlowModFacLS = CurveValue( DXCoil( DXCoilNum ).MSEIRFFlow( SpeedNumLS ), AirMassFlowRatioLS );
				EIRLS = 1.0 / DXCoil( DXCoilNum ).MSRatedCOP( SpeedNumLS ) * EIRTempModFacLS * EIRFlowModFacLS;

				// get current total capacity, SHR, EIR
				if ( SpeedRatio >= 1.0 ) {
					SHR = SHRHS;
					EIR = EIRHS;
					CondAirMassFlow = RhoAir * DXCoil( DXCoilNum ).MSEvapCondAirFlow( SpeedNumHS );
					EvapCondPumpElecPower = DXCoil( DXCoilNum ).MSEvapCondPumpElecNomPower( SpeedNumHS );
				} else {
					EIR = SpeedRatio * EIRHS + ( 1.0 - SpeedRatio ) * EIRLS;
					SHR = SpeedRatio * SHRHS + ( 1.0 - SpeedRatio ) * SHRLS;
					CondAirMassFlow = RhoAir * ( SpeedRatio * DXCoil( DXCoilNum ).MSEvapCondAirFlow( SpeedNumHS ) + ( 1.0 - SpeedRatio ) * DXCoil( DXCoilNum ).MSEvapCondAirFlow( SpeedNumLS ) );
					EvapCondPumpElecPower = SpeedRatio * DXCoil( DXCoilNum ).MSEvapCondPumpElecNomPower( SpeedNumHS ) + ( 1.0 - SpeedRatio ) * DXCoil( DXCoilNum ).MSEvapCondPumpElecNomPower( SpeedNumLS );
				}
				// Outlet calculation
				DXCoil( DXCoilNum ).TotalCoolingEnergyRate = MSHPMassFlowRateHigh * ( InletAirEnthalpy - HSOutletAirEnthalpy ) * SpeedRatio + MSHPMassFlowRateLow * ( InletAirEnthalpy - LSOutletAirEnthalpy ) * ( 1.0 - SpeedRatio );
				// Average outlet enthalpy
				OutletAirEnthalpy = InletAirEnthalpy - DXCoil( DXCoilNum ).TotalCoolingEnergyRate / DXCoil( DXCoilNum ).InletAirMassFlowRate;
				MinAirHumRat = min( InletAirHumRat, SpeedRatio * HSOutletAirHumRat + ( 1.0 - SpeedRatio ) * LSOutletAirHumRat );
				DXCoil( DXCoilNum ).SensCoolingEnergyRate = MSHPMassFlowRateHigh * ( PsyHFnTdbW( InletAirDryBulbTemp, MinAirHumRat ) - PsyHFnTdbW( HSOutletAirDryBulbTemp, MinAirHumRat ) ) * SpeedRatio + MSHPMassFlowRateLow * ( PsyHFnTdbW( InletAirDryBulbTemp, MinAirHumRat ) - PsyHFnTdbW( LSOutletAirDryBulbTemp, MinAirHumRat ) ) * ( 1.0 - SpeedRatio );
				if ( DXCoil( DXCoilNum ).SensCoolingEnergyRate > DXCoil( DXCoilNum ).TotalCoolingEnergyRate ) {
					DXCoil( DXCoilNum ).SensCoolingEnergyRate = DXCoil( DXCoilNum ).TotalCoolingEnergyRate;
				}
				DXCoil( DXCoilNum ).LatCoolingEnergyRate = DXCoil( DXCoilNum ).TotalCoolingEnergyRate - DXCoil( DXCoilNum ).SensCoolingEnergyRate;

				if ( FanOpMode == CycFanCycCoil ) OnOffFanPartLoadFraction = 1.0;
				// Update outlet conditions
				if ( SpeedRatio == 0.0 && FanOpMode == CycFanCycCoil ) {
					OutletAirEnthalpy = LSOutletAirEnthalpy;
					OutletAirHumRat = LSOutletAirHumRat;
					OutletAirDryBulbTemp = LSOutletAirDryBulbTemp;
				} else if ( SpeedRatio >= 1.0 && FanOpMode == CycFanCycCoil ) {
					OutletAirEnthalpy = HSOutletAirEnthalpy;
					OutletAirHumRat = HSOutletAirHumRat;
					OutletAirDryBulbTemp = HSOutletAirDryBulbTemp;
				} else {
					Hfg = PsyHfgAirFnWTdb( MinAirHumRat, HSOutletAirDryBulbTemp * SpeedRatio + ( 1.0 - SpeedRatio ) * LSOutletAirDryBulbTemp );
					// Average outlet HR
					OutletAirHumRat = InletAirHumRat - DXCoil( DXCoilNum ).LatCoolingEnergyRate / Hfg / DXCoil( DXCoilNum ).InletAirMassFlowRate;
					OutletAirDryBulbTemp = PsyTdbFnHW( OutletAirEnthalpy, OutletAirHumRat );
					if ( OutletAirDryBulbTemp < OutletAirDryBulbTempSat ) { // Limit to saturated conditions at OutletAirEnthalpy
						OutletAirDryBulbTemp = OutletAirDryBulbTempSat;
						OutletAirHumRat = PsyWFnTdbH( OutletAirDryBulbTemp, OutletAirEnthalpy, RoutineName );
						MinAirHumRat = min( InletAirHumRat, OutletAirHumRat );
						DXCoil( DXCoilNum ).SensCoolingEnergyRate = AirMassFlow * ( PsyHFnTdbW( InletAirDryBulbTemp, MinAirHumRat ) - PsyHFnTdbW( OutletAirDryBulbTemp, MinAirHumRat ) );
						if ( DXCoil( DXCoilNum ).SensCoolingEnergyRate > DXCoil( DXCoilNum ).TotalCoolingEnergyRate ) {
							DXCoil( DXCoilNum ).SensCoolingEnergyRate = DXCoil( DXCoilNum ).TotalCoolingEnergyRate;
						}
						DXCoil( DXCoilNum ).LatCoolingEnergyRate = DXCoil( DXCoilNum ).TotalCoolingEnergyRate - DXCoil( DXCoilNum ).SensCoolingEnergyRate;
					}
				}

				LSElecCoolingPower = TotCapLS * EIRLS;
				HSElecCoolingPower = TotCapHS * EIRHS;

				// Power calculation
				if ( ! DXCoil( DXCoilNum ).PLRImpact ) {
					DXCoil( DXCoilNum ).ElecCoolingPower = SpeedRatio * HSElecCoolingPower + ( 1.0 - SpeedRatio ) * LSElecCoolingPower;
				} else {
					DXCoil( DXCoilNum ).ElecCoolingPower = DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction * HSElecCoolingPower + ( 1.0 - DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction ) * LSElecCoolingPower;
				}
				//   Calculation for heat reclaim needs to be corrected to use compressor power (not including condenser fan power)
				HeatReclaimDXCoil( DXCoilNum ).AvailCapacity = DXCoil( DXCoilNum ).TotalCoolingEnergyRate + DXCoil( DXCoilNum ).ElecCoolingPower;

				// Waste heat calculation
				if ( DXCoil( DXCoilNum ).MSHPHeatRecActive ) {
					if ( DXCoil( DXCoilNum ).MSWasteHeat( SpeedNumLS ) == 0 ) {
						WasteHeatLS = DXCoil( DXCoilNum ).MSWasteHeatFrac( SpeedNumLS );
					} else {
						WasteHeatLS = CurveValue( DXCoil( DXCoilNum ).MSWasteHeat( SpeedNumLS ), OutdoorDryBulb, InletAirDryBulbTemp ) * DXCoil( DXCoilNum ).MSWasteHeatFrac( SpeedNumLS );
					}
					if ( DXCoil( DXCoilNum ).MSWasteHeat( SpeedNumHS ) == 0 ) {
						WasteHeatHS = DXCoil( DXCoilNum ).MSWasteHeatFrac( SpeedNumHS );
					} else {
						WasteHeatHS = CurveValue( DXCoil( DXCoilNum ).MSWasteHeat( SpeedNumHS ), OutdoorDryBulb, InletAirDryBulbTemp ) * DXCoil( DXCoilNum ).MSWasteHeatFrac( SpeedNumHS );
					}
					MSHPWasteHeat = ( SpeedRatio * WasteHeatHS + ( 1.0 - SpeedRatio ) * WasteHeatLS ) * DXCoil( DXCoilNum ).ElecCoolingPower;
				}

				// Energy use for other fuel types
				if ( DXCoil( DXCoilNum ).FuelType != FuelTypeElectricity ) {
					DXCoil( DXCoilNum ).FuelUsed = DXCoil( DXCoilNum ).ElecCoolingPower;
					DXCoil( DXCoilNum ).ElecCoolingPower = 0.0;
				}

				DXCoil( DXCoilNum ).OutletAirEnthalpy = OutletAirEnthalpy;
				DXCoil( DXCoilNum ).OutletAirHumRat = OutletAirHumRat;
				DXCoil( DXCoilNum ).OutletAirTemp = OutletAirDryBulbTemp;
				DXCoil( DXCoilNum ).CrankcaseHeaterPower = 0.0;

			} else if ( CycRatio > 0.0 || ( CycRatio > 0.0 && SingleMode == 1 ) ) {

				if ( FanOpMode == CycFanCycCoil ) AirMassFlow /= CycRatio;
				if ( FanOpMode == ContFanCycCoil ) AirMassFlow = MSHPMassFlowRateHigh;

				// Check for valid air volume flow per rated total cooling capacity (200 - 500 cfm/ton) at low speed
				AirVolumeFlowRate = MSHPMassFlowRateHigh / PsyRhoAirFnPbTdbW( OutdoorPressure, InletAirDryBulbTemp, InletAirHumRat, RoutineName );
				//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
				//  AirVolumeFlowRate = AirMassFlow/PsyRhoAirFnPbTdbW(InletAirPressure,InletAirDryBulbTemp, InletAirHumRat)
				VolFlowperRatedTotCap = AirVolumeFlowRate / DXCoil( DXCoilNum ).MSRatedTotCap( SpeedNum );
				if ( ( VolFlowperRatedTotCap < MinOperVolFlowPerRatedTotCap( DXCT ) ) || ( VolFlowperRatedTotCap > MaxCoolVolFlowPerRatedTotCap( DXCT ) ) ) {
					if ( DXCoil( DXCoilNum ).MSErrIndex( SpeedNum ) == 0 ) {
						ShowWarningMessage( DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + "\" - Air volume flow rate per watt of rated total cooling capacity is out of range at speed " + TrimSigDigits( SpeedNum ) + '.' );
						ShowContinueErrorTimeStamp( "" );
						ShowContinueError( "Expected range for VolumeFlowPerRatedTotalCapacity=[" + RoundSigDigits( MinOperVolFlowPerRatedTotCap( DXCT ), 3 ) + "--" + RoundSigDigits( MaxCoolVolFlowPerRatedTotCap( DXCT ), 3 ) + "] Current value is " + RoundSigDigits( VolFlowperRatedTotCap, 3 ) + " m3/s/W" );
						ShowContinueError( "Possible causes include inconsistent air flow rates in system components or" );
						ShowContinueError( "inconsistent supply air fan operation modes in coil and unitary system objects." );
					}
					ShowRecurringWarningErrorAtEnd( DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + "\" - Air volume flow rate per watt of rated total cooling capacity is out of range at speed " + TrimSigDigits( SpeedNumHS ) + " error continues...", DXCoil( DXCoilNum ).MSErrIndex( SpeedNumHS ), VolFlowperRatedTotCap, VolFlowperRatedTotCap );
				}

				if ( DXCoil( DXCoilNum ).CondenserType( SpeedNum ) == EvapCooled ) {
					// Outdoor wet-bulb temp from DataEnvironment + (1.0-EvapCondEffectiveness) * (drybulb - wetbulb)
					CondInletTemp = OutdoorWetBulb + ( OutdoorDryBulb - OutdoorWetBulb ) * ( 1.0 - DXCoil( DXCoilNum ).MSEvapCondEffect( SpeedNum ) );
					CondInletHumRat = PsyWFnTdbTwbPb( CondInletTemp, OutdoorWetBulb, OutdoorPressure, RoutineName );
				}

				RatedCBFLS = DXCoil( DXCoilNum ).MSRatedCBF( SpeedNum );
				CBFLS = AdjustCBF( RatedCBFLS, DXCoil( DXCoilNum ).MSRatedAirMassFlowRate( SpeedNum ), MSHPMassFlowRateHigh );

				// Adjust low speed coil bypass factor for actual flow rate.
				// CBF = AdjustCBF(DXCoil(DXCoilNum)%RatedCBF2,DXCoil(DXCoilNum)%RatedAirMassFlowRate2,AirMassFlow)
				// get low speed total capacity and SHR at current conditions
				CalcTotCapSHR( InletAirDryBulbTemp, InletAirHumRat, InletAirEnthalpy, InletAirWetBulbC, AirMassFlowRatioLS, MSHPMassFlowRateHigh, DXCoil( DXCoilNum ).MSRatedTotCap( SpeedNum ), CBFLS, DXCoil( DXCoilNum ).MSCCapFTemp( SpeedNum ), DXCoil( DXCoilNum ).MSCCapFFlow( SpeedNum ), TotCapLS, SHRLS, CondInletTemp, OutdoorPressure );
				//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
				//  Node(DXCoil(DXCoilNum)%AirInNode)%Press)
				hDelta = TotCapLS / AirMassFlow;
				// Adjust CBF for off-nominal flow
				CBF = AdjustCBF( DXCoil( DXCoilNum ).MSRatedCBF( SpeedNum ), DXCoil( DXCoilNum ).MSRatedAirMassFlowRate( SpeedNum ), AirMassFlow );
				// Calculate new apparatus dew point conditions
				hADP = InletAirEnthalpy - hDelta / ( 1.0 - CBF );
				tADP = PsyTsatFnHPb( hADP, OutdoorPressure, RoutineName );
				//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
				//  tADP = PsyTsatFnHPb(hADP,InletAirPressure)
				wADP = PsyWFnTdbH( tADP, hADP, RoutineName );
				hTinwADP = PsyHFnTdbW( InletAirDryBulbTemp, wADP );
				// get corresponding SHR
				if ( ( InletAirEnthalpy - hADP ) > 1.e-10 ) {
					SHR = min( ( hTinwADP - hADP ) / ( InletAirEnthalpy - hADP ), 1.0 );
				} else {
					SHR = 1.0;
				}
				//cr8918    SHR = MIN((hTinwADP-hADP)/(InletAirEnthalpy-hADP),1.0d0)

				// get the part load factor that will account for cycling losses
				PLF = CurveValue( DXCoil( DXCoilNum ).MSPLFFPLR( SpeedNum ), CycRatio );
				if ( FanOpMode == CycFanCycCoil && CycRatio == 1.0 && PLF != 1.0 ) {
					if ( DXCoil( DXCoilNum ).PLFErrIndex == 0 ) {
						ShowWarningMessage( "The PLF curve value for DX cooling coil " + DXCoil( DXCoilNum ).Name + " =" + RoundSigDigits( PLF, 2 ) + " for part-load ratio = 1" );
						ShowContinueError( "PLF curve value must be = 1.0 and has been reset to 1.0. Simulation is continuing." );
						ShowContinueErrorTimeStamp( "" );
					}
					ShowRecurringWarningErrorAtEnd( DXCoil( DXCoilNum ).Name + "\": DX cooling coil PLF curve value <> 1.0 warning continues...", DXCoil( DXCoilNum ).PLFErrIndex, PLF, PLF );
					PLF = 1.0;
				}

				if ( PLF < 0.7 ) {
					PLF = 0.7;
				}
				// calculate the run time fraction
				DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction = CycRatio / PLF;
				DXCoil( DXCoilNum ).PartLoadRatio = CycRatio;

				if ( DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction > 1.0 ) {
					DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction = 1.0; // Reset coil runtime fraction to 1.0
				}

				// get low speed outlet conditions
				LSOutletAirEnthalpy = InletAirEnthalpy - hDelta;
				hTinwout = InletAirEnthalpy - ( 1.0 - SHR ) * hDelta;
				LSOutletAirHumRat = PsyWFnTdbH( InletAirDryBulbTemp, hTinwout, RoutineName );
				LSOutletAirDryBulbTemp = PsyTdbFnHW( LSOutletAirEnthalpy, LSOutletAirHumRat );
				LSOutletAirRH = PsyRhFnTdbWPb( LSOutletAirDryBulbTemp, LSOutletAirHumRat, OutdoorPressure, RoutineNameLowSpeedOutlet );
				OutletAirDryBulbTempSat = PsyTsatFnHPb( LSOutletAirEnthalpy, OutdoorPressure, RoutineName );
				//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
				//    LSOutletAirRH = PsyRhFnTdbWPb(LSOutletAirDryBulbTemp,LSOutletAirHumRat,InletAirPressure)
				//    OutletAirDryBulbTempSat = PsyTsatFnHPb(LSOutletAirEnthalpy,InletAirPressure)
				if ( LSOutletAirDryBulbTemp < OutletAirDryBulbTempSat ) { // Limit to saturated conditions at OutletAirEnthalpy
					LSOutletAirDryBulbTemp = OutletAirDryBulbTempSat;
					LSOutletAirHumRat = PsyWFnTdbH( LSOutletAirDryBulbTemp, LSOutletAirEnthalpy, RoutineName );
				}

				//  If constant fan with cycling compressor, call function to determine "effective SHR"
				//  which includes the part-load degradation on latent capacity
				if ( FanOpMode == ContFanCycCoil ) {
					QLatRated = DXCoil( DXCoilNum ).MSRatedTotCap( SpeedNum ) * ( 1.0 - DXCoil( DXCoilNum ).MSRatedSHR( SpeedNum ) );
					QLatActual = TotCapLS * ( 1.0 - SHR );
					SHRUnadjusted = SHR;
					SHR = CalcEffectiveSHR( DXCoilNum, SHR, DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction, QLatRated, QLatActual, InletAirDryBulbTemp, InletAirWetBulbC, SpeedNum );
					// Calculate full load output conditions
					if ( SHR > 1.0 ) SHR = 1.0;
					hTinwout = InletAirEnthalpy - ( 1.0 - SHR ) * hDelta;
					if ( SHR < 1.0 ) {
						LSOutletAirHumRat = PsyWFnTdbH( InletAirDryBulbTemp, hTinwout, RoutineName );
					} else {
						LSOutletAirHumRat = InletAirHumRat;
					}
					LSOutletAirDryBulbTemp = PsyTdbFnHW( LSOutletAirEnthalpy, LSOutletAirHumRat );
				}

				if ( FanOpMode == CycFanCycCoil ) OnOffFanPartLoadFraction = PLF;
				// outlet conditions are average of inlet and low speed weighted by CycRatio
				OutletAirEnthalpy = LSOutletAirEnthalpy;
				OutletAirHumRat = LSOutletAirHumRat;
				OutletAirDryBulbTemp = LSOutletAirDryBulbTemp;
				// get low speed EIR at current conditions
				EIRTempModFacLS = CurveValue( DXCoil( DXCoilNum ).MSEIRFTemp( SpeedNum ), InletAirWetBulbC, CondInletTemp );
				EIRFlowModFacLS = CurveValue( DXCoil( DXCoilNum ).MSEIRFFlow( SpeedNum ), AirMassFlowRatioLS );
				EIRLS = 1.0 / DXCoil( DXCoilNum ).MSRatedCOP( SpeedNum ) * EIRTempModFacLS * EIRFlowModFacLS;

				// get the eletrical power consumption
				DXCoil( DXCoilNum ).ElecCoolingPower = TotCapLS * EIRLS * DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction;
				// calculate cooling output power
				//    AirMassFlow = DXCoil(DXCoilNum)%InletAirMassFlowRate
				DXCoil( DXCoilNum ).TotalCoolingEnergyRate = AirMassFlow * ( InletAirEnthalpy - LSOutletAirEnthalpy ) * CycRatio;
				if ( FanOpMode == ContFanCycCoil ) {
					OutletAirEnthalpy = InletAirEnthalpy - DXCoil( DXCoilNum ).TotalCoolingEnergyRate / DXCoil( DXCoilNum ).InletAirMassFlowRate;
					MinAirHumRat = min( InletAirHumRat, LSOutletAirHumRat );
					DXCoil( DXCoilNum ).SensCoolingEnergyRate = AirMassFlow * ( PsyHFnTdbW( InletAirDryBulbTemp, MinAirHumRat ) - PsyHFnTdbW( LSOutletAirDryBulbTemp, MinAirHumRat ) ) * CycRatio;
					if ( DXCoil( DXCoilNum ).SensCoolingEnergyRate > DXCoil( DXCoilNum ).TotalCoolingEnergyRate ) {
						DXCoil( DXCoilNum ).SensCoolingEnergyRate = DXCoil( DXCoilNum ).TotalCoolingEnergyRate;
					}
					DXCoil( DXCoilNum ).LatCoolingEnergyRate = DXCoil( DXCoilNum ).TotalCoolingEnergyRate - DXCoil( DXCoilNum ).SensCoolingEnergyRate;
					// Calculate avarage outlet conditions
					Hfg = PsyHfgAirFnWTdb( MinAirHumRat, OutletAirDryBulbTemp * CycRatio + ( 1.0 - CycRatio ) * InletAirDryBulbTemp );
					OutletAirHumRat = InletAirHumRat - DXCoil( DXCoilNum ).LatCoolingEnergyRate / Hfg / DXCoil( DXCoilNum ).InletAirMassFlowRate;
					OutletAirDryBulbTemp = PsyTdbFnHW( OutletAirEnthalpy, OutletAirHumRat );
					if ( OutletAirDryBulbTemp < OutletAirDryBulbTempSat ) { // Limit to saturated conditions at OutletAirEnthalpy
						OutletAirDryBulbTemp = OutletAirDryBulbTempSat;
						OutletAirHumRat = PsyWFnTdbH( OutletAirDryBulbTemp, OutletAirEnthalpy, RoutineName );
						MinAirHumRat = min( InletAirHumRat, OutletAirHumRat );
						DXCoil( DXCoilNum ).SensCoolingEnergyRate = DXCoil( DXCoilNum ).InletAirMassFlowRate * ( PsyHFnTdbW( InletAirDryBulbTemp, MinAirHumRat ) - PsyHFnTdbW( OutletAirDryBulbTemp, MinAirHumRat ) );
						if ( DXCoil( DXCoilNum ).SensCoolingEnergyRate > DXCoil( DXCoilNum ).TotalCoolingEnergyRate ) {
							DXCoil( DXCoilNum ).SensCoolingEnergyRate = DXCoil( DXCoilNum ).TotalCoolingEnergyRate;
						}
						DXCoil( DXCoilNum ).LatCoolingEnergyRate = DXCoil( DXCoilNum ).TotalCoolingEnergyRate - DXCoil( DXCoilNum ).SensCoolingEnergyRate;
					}
				} else {
					MinAirHumRat = min( InletAirHumRat, OutletAirHumRat );
					DXCoil( DXCoilNum ).SensCoolingEnergyRate = AirMassFlow * ( PsyHFnTdbW( InletAirDryBulbTemp, MinAirHumRat ) - PsyHFnTdbW( LSOutletAirDryBulbTemp, MinAirHumRat ) ) * CycRatio;
					// Don't let sensible capacity be greater than total capacity
					if ( DXCoil( DXCoilNum ).SensCoolingEnergyRate > DXCoil( DXCoilNum ).TotalCoolingEnergyRate ) {
						DXCoil( DXCoilNum ).SensCoolingEnergyRate = DXCoil( DXCoilNum ).TotalCoolingEnergyRate;
					}
					DXCoil( DXCoilNum ).LatCoolingEnergyRate = DXCoil( DXCoilNum ).TotalCoolingEnergyRate - DXCoil( DXCoilNum ).SensCoolingEnergyRate;
				}
				//   Calculation for heat reclaim needs to be corrected to use compressor power (not including condenser fan power)
				HeatReclaimDXCoil( DXCoilNum ).AvailCapacity = DXCoil( DXCoilNum ).TotalCoolingEnergyRate + DXCoil( DXCoilNum ).ElecCoolingPower;
				DXCoil( DXCoilNum ).OutletAirEnthalpy = OutletAirEnthalpy;
				DXCoil( DXCoilNum ).OutletAirHumRat = OutletAirHumRat;
				DXCoil( DXCoilNum ).OutletAirTemp = OutletAirDryBulbTemp;
				CondAirMassFlow = RhoAir * DXCoil( DXCoilNum ).MSEvapCondAirFlow( SpeedNum ) * DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction;
				EvapCondPumpElecPower = DXCoil( DXCoilNum ).MSEvapCondPumpElecNomPower( SpeedNum ) * DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction;

				// Waste heat
				if ( DXCoil( DXCoilNum ).MSHPHeatRecActive ) {
					if ( DXCoil( DXCoilNum ).MSWasteHeat( SpeedNum ) == 0 ) {
						MSHPWasteHeat = DXCoil( DXCoilNum ).MSWasteHeatFrac( SpeedNum ) * DXCoil( DXCoilNum ).ElecCoolingPower;
					} else {
						MSHPWasteHeat = CurveValue( DXCoil( DXCoilNum ).MSWasteHeat( SpeedNum ), OutdoorDryBulb, InletAirDryBulbTemp ) * DXCoil( DXCoilNum ).MSWasteHeatFrac( SpeedNum ) * DXCoil( DXCoilNum ).ElecCoolingPower;
					}
				}
				// Energy use for other fuel types
				if ( DXCoil( DXCoilNum ).FuelType != FuelTypeElectricity ) {
					DXCoil( DXCoilNum ).FuelUsed = DXCoil( DXCoilNum ).ElecCoolingPower;
					DXCoil( DXCoilNum ).ElecCoolingPower = 0.0;
				}

				if ( DXCoil( DXCoilNum ).CompanionUpstreamDXCoil == 0 ) {
					DXCoil( DXCoilNum ).CrankcaseHeaterPower = CrankcaseHeatingPower * ( 1.0 - DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction );
				} else {
					DXCoil( DXCoilNum ).CrankcaseHeaterPower = CrankcaseHeatingPower * ( 1.0 - max( DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction, DXCoil( DXCoil( DXCoilNum ).CompanionUpstreamDXCoil ).HeatingCoilRuntimeFraction ) );
				}

			}

			if ( DXCoil( DXCoilNum ).CondenserType( DXMode ) == EvapCooled ) {
				//******************
				//             WATER CONSUMPTION IN m3 OF WATER FOR DIRECT
				//             H2O [m3/sec] = Delta W[KgH2O/Kg air]*Mass Flow Air[Kg air]
				//                                /RhoWater [kg H2O/m3 H2O]
				//******************
				RhoWater = RhoH2O( OutdoorDryBulb );
				DXCoil( DXCoilNum ).EvapWaterConsumpRate = ( CondInletHumRat - OutdoorHumRat ) * CondAirMassFlow / RhoWater;
				DXCoil( DXCoilNum ).EvapCondPumpElecPower = EvapCondPumpElecPower;
				//set water system demand request (if needed)
				if ( DXCoil( DXCoilNum ).EvapWaterSupplyMode == WaterSupplyFromTank ) {
					WaterStorage( DXCoil( DXCoilNum ).EvapWaterSupTankID ).VdotRequestDemand( DXCoil( DXCoilNum ).EvapWaterTankDemandARRID ) = DXCoil( DXCoilNum ).EvapWaterConsumpRate;
				}

				// Calculate basin heater power
				CalcBasinHeaterPower( DXCoil( DXCoilNum ).BasinHeaterPowerFTempDiff, DXCoil( DXCoilNum ).BasinHeaterSchedulePtr, DXCoil( DXCoilNum ).BasinHeaterSetPointTemp, DXCoil( DXCoilNum ).BasinHeaterPower );
				DXCoil( DXCoilNum ).BasinHeaterPower *= ( 1.0 - DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction );
			}

		} else {

			// DX coil is off; just pass through conditions
			DXCoil( DXCoilNum ).OutletAirEnthalpy = DXCoil( DXCoilNum ).InletAirEnthalpy;
			DXCoil( DXCoilNum ).OutletAirHumRat = DXCoil( DXCoilNum ).InletAirHumRat;
			DXCoil( DXCoilNum ).OutletAirTemp = DXCoil( DXCoilNum ).InletAirTemp;

			DXCoil( DXCoilNum ).FuelUsed = 0.0;
			DXCoil( DXCoilNum ).ElecCoolingPower = 0.0;
			DXCoil( DXCoilNum ).TotalCoolingEnergyRate = 0.0;
			DXCoil( DXCoilNum ).SensCoolingEnergyRate = 0.0;
			DXCoil( DXCoilNum ).LatCoolingEnergyRate = 0.0;
			DXCoil( DXCoilNum ).EvapCondPumpElecPower = 0.0;
			DXCoil( DXCoilNum ).EvapWaterConsumpRate = 0.0;

			if ( DXCoil( DXCoilNum ).CompanionUpstreamDXCoil == 0 ) {
				DXCoil( DXCoilNum ).CrankcaseHeaterPower = CrankcaseHeatingPower;
			} else {
				DXCoil( DXCoilNum ).CrankcaseHeaterPower = CrankcaseHeatingPower * ( 1.0 - DXCoil( DXCoil( DXCoilNum ).CompanionUpstreamDXCoil ).HeatingCoilRuntimeFraction );
			}

			// Calculate basin heater power
			if ( DXCoil( DXCoilNum ).CondenserType( DXMode ) == EvapCooled ) {
				CalcBasinHeaterPower( DXCoil( DXCoilNum ).BasinHeaterPowerFTempDiff, DXCoil( DXCoilNum ).BasinHeaterSchedulePtr, DXCoil( DXCoilNum ).BasinHeaterSetPointTemp, DXCoil( DXCoilNum ).BasinHeaterPower );
			}
		}

		DXCoilOutletTemp( DXCoilNum ) = DXCoil( DXCoilNum ).OutletAirTemp;
		DXCoilOutletHumRat( DXCoilNum ) = DXCoil( DXCoilNum ).OutletAirHumRat;
		DXCoilPartLoadRatio( DXCoilNum ) = DXCoil( DXCoilNum ).PartLoadRatio;
		DXCoilFanOpMode( DXCoilNum ) = FanOpMode;
		DXCoil( DXCoilNum ).CondInletTemp = CondInletTemp; // Save condenser inlet temp in the data structure

		// calc secondary coil if specified
		if ( DXCoil( DXCoilNum ).IsSecondaryDXCoilInZone ) {
			CalcSecondaryDXCoils( DXCoilNum );
		}

	}

	void
	CalcMultiSpeedDXCoilHeating(
		int const DXCoilNum, // the number of the DX heating coil to be simulated
		Real64 const SpeedRatio, // = (CompressorSpeed - CompressorSpeedMin) / (CompressorSpeedMax - CompressorSpeedMin)
		Real64 const CycRatio, // cycling part load ratio
		int const SpeedNum, // Speed number
		int const FanOpMode, // Fan operation mode
		int const SingleMode // Single mode operation Yes/No; 1=Yes, 0=No
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Lixing Gu, FSEC
		//       DATE WRITTEN   June 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  Revised based on CalcDXHeatingCoil

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the air-side performance and electrical energy use of a direct-
		// expansion, air-cooled cooling unit with a multispeed compressor.

		// METHODOLOGY EMPLOYED:
		// Uses the same methodology as the single speed DX heating unit model (SUBROUTINE CalcDXHeatingCoil).
		// In addition it assumes that the unit performance is obtained by interpolating between
		// the performance at high speed and that at low speed. If the output needed is below
		// that produced at low speed, the compressor cycles between off and low speed.

		// Using/Aliasing
		using CurveManager::CurveValue;
		using General::TrimSigDigits;
		using General::RoundSigDigits;
		using DataWater::WaterStorage;
		using DataHVACGlobals::MSHPMassFlowRateLow;
		using DataHVACGlobals::MSHPMassFlowRateHigh;
		using DataHVACGlobals::MSHPWasteHeat;
		using DataHeatBalFanSys::ZoneAirHumRat;
		using DataHeatBalFanSys::ZT;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// SpeedRatio varies between 1.0 (maximum speed) and 0.0 (minimum speed)

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcMultiSpeedDXCoilHeating" );
		static std::string const RoutineNameAverageLoad( "CalcMultiSpeedDXCoilHeating:Averageload" );
		static std::string const RoutineNameFullLoad( "CalcMultiSpeedDXCoilHeating:fullload" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 AirMassFlow; // dry air mass flow rate through coil [kg/s]
		Real64 InletAirWetBulbC; // wetbulb temperature of inlet air [C]
		Real64 InletAirDryBulbTemp; // inlet air dry bulb temperature [C]
		Real64 InletAirEnthalpy; // inlet air enthalpy [J/kg]
		Real64 InletAirHumRat; // inlet air humidity ratio [kg/kg]
		Real64 OutletAirEnthalpy; // outlet air enthalpy [J/kg]
		Real64 OutletAirHumRat; // outlet air humidity ratio [kg/kg]
		Real64 TotCapHS; // total capacity at high speed [W]
		Real64 TotCapLS; // total capacity at low speed [W]
		Real64 TotCapHSAdj; // total adjusted capacity at high speed [W]
		Real64 TotCapLSAdj; // total adjusted capacity at low speed [W]
		Real64 EIRHS; // EIR at off rated conditions (high speed)
		Real64 EIRLS; // EIR at off rated conditions (low speed)
		Real64 TotCap; // total capacity at current speed [W]
		Real64 TotCapAdj; // total adjusted capacity at current speed [W]
		Real64 EIR; // EIR at current speed
		Real64 PLF; // Part load factor, accounts for thermal lag at compressor startup, used in
		// power calculation
		Real64 OutdoorDryBulb; // Outdoor dry-bulb temperature at condenser (C)
		Real64 OutdoorHumRat; // Outdoor humidity ratio at condenser (kg/kg)
		Real64 OutdoorPressure; // Outdoor barometric pressure at condenser (Pa)
		int SpeedNumHS; // High speed number
		int SpeedNumLS; // Low speed number
		Real64 AirMassFlowRatioLS; // airflow ratio at low speed
		Real64 AirMassFlowRatioHS; // airflow ratio at high speed
		Real64 AirFlowRatio; // Airflow ratio
		Real64 PLRHeating; // Part load ratio in heating
		Real64 CrankcaseHeatingPower; // Power due to crank case heater
		Real64 AirVolumeFlowRate; // Air volume flow rate across the heating coil
		Real64 VolFlowperRatedTotCap; // Air volume flow rate divided by rated total heating capacity
		Real64 TotCapTempModFac( 0.0 ); // Total capacity modifier as a function ot temperature
		Real64 TotCapFlowModFac; // Total capacity modifier as a function of flow ratio
		Real64 OutdoorCoilT; // Outdoor coil temperature
		Real64 OutdoorCoildw; // Outdoor coil delta w assuming coil temperature of OutdoorCoilT
		Real64 LoadDueToDefrost; // Additonal load due to defrost
		Real64 LoadDueToDefrostLS; // Additonal load due to defrost at low speed
		Real64 LoadDueToDefrostHS; // Additonal load due to defrost at high speed
		Real64 HeatingCapacityMultiplier; // Multiplier for heating capacity when system is in defrost
		Real64 FractionalDefrostTime; // Fraction of time step when system is in defrost
		Real64 InputPowerMultiplier; // Multiplier for poer when system is in defrost
		Real64 DefrostEIRTempModFac; // EIR modifier for defrost
		Real64 FullLoadOutAirEnth; // Outlet full load enthalpy
		Real64 FullLoadOutAirHumRat; // Outlet humidity ratio at full load
		Real64 FullLoadOutAirTemp; // Outlet temperature at full load
		Real64 FullLoadOutAirRH; // Outler relative humidity at full load
		Real64 OutletAirTemp; // Supply ari temperature
		Real64 EIRTempModFac( 0.0 ); // EIR modifier as a function of temperature
		Real64 EIRFlowModFac; // EIR modifier as a function of airflow ratio
		Real64 WasteHeatLS; // Waste heat at low speed
		Real64 WasteHeatHS; // Waste heat at high speed
		Real64 LSFullLoadOutAirEnth; // Outlet full load enthalpy at low speed
		Real64 HSFullLoadOutAirEnth; // Outlet full load enthalpy at high speed
		Real64 LSElecHeatingPower; // Full load power at low speed
		Real64 HSElecHeatingPower; // Full load power at high speed
		Real64 DefrostPowerLS; // Defrost power at low speed [W]
		Real64 DefrostPowerHS; // Defrost power at high speed [W]

		//Autodesk:Uninit Initialize variables used uninitialized
		FullLoadOutAirEnth = 0.0; //Autodesk:Uninit Force default initialization

		// FLOW
		if ( SpeedNum > 1 ) {
			SpeedNumLS = SpeedNum - 1;
			SpeedNumHS = SpeedNum;
			if ( SpeedNum > DXCoil( DXCoilNum ).NumOfSpeeds ) {
				SpeedNumLS = DXCoil( DXCoilNum ).NumOfSpeeds - 1;
				SpeedNumHS = DXCoil( DXCoilNum ).NumOfSpeeds;
			}
		} else {
			SpeedNumLS = 1;
			SpeedNumHS = 1;
		}

		AirMassFlow = DXCoil( DXCoilNum ).InletAirMassFlowRate;
		AirMassFlowRatioLS = MSHPMassFlowRateLow / DXCoil( DXCoilNum ).MSRatedAirMassFlowRate( SpeedNumLS );
		AirMassFlowRatioHS = MSHPMassFlowRateHigh / DXCoil( DXCoilNum ).MSRatedAirMassFlowRate( SpeedNumHS );
		if ( ( AirMassFlow > 0.0 ) && ( CycRatio > 0.0 ) && ( ( MSHPMassFlowRateLow == 0.0 ) || ( MSHPMassFlowRateHigh == 0.0 ) ) ) {
			ShowSevereError( "CalcMultiSpeedDXCoilHeating: " + DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + " Developer error - inconsistent airflow rates." );
			ShowContinueError( "When AirMassFlow > 0.0 and CycRatio > 0.0, then MSHPMassFlowRateLow and MSHPMassFlowRateHigh must also be > 0.0" );
			ShowContinueErrorTimeStamp( "" );
			ShowContinueError( "AirMassFlow=" + RoundSigDigits( AirMassFlow, 3 ) + ",CycRatio=" + RoundSigDigits( CycRatio, 3 ) + ", MSHPMassFlowRateLow=" + RoundSigDigits( MSHPMassFlowRateLow, 3 ) + ", MSHPMassFlowRateHigh=" + RoundSigDigits( MSHPMassFlowRateHigh, 3 ) );
			ShowFatalError( "Preceding condition(s) causes termination." );
		} else if ( CycRatio > 1.0 || SpeedRatio > 1.0 ) {
			ShowSevereError( "CalcMultiSpeedDXCoilHeating: " + DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + " Developer error - inconsistent speed ratios." );
			ShowContinueError( "CycRatio and SpeedRatio must be between 0.0 and 1.0" );
			ShowContinueErrorTimeStamp( "" );
			ShowContinueError( "CycRatio=" + RoundSigDigits( CycRatio, 1 ) + ", SpeedRatio = " + RoundSigDigits( SpeedRatio, 1 ) );
			ShowFatalError( "Preceding condition(s) causes termination." );
		}

		AirFlowRatio = 1.0;
		if ( DXCoil( DXCoilNum ).CompanionUpstreamDXCoil == 0 ) MSHPWasteHeat = 0.0;

		// Get condenser outdoor node info from DX Heating Coil
		if ( DXCoil( DXCoilNum ).CondenserInletNodeNum( 1 ) != 0 ) {
			OutdoorPressure = Node( DXCoil( DXCoilNum ).CondenserInletNodeNum( 1 ) ).Press;
			// If node is not connected to anything, pressure = default, use weather data
			if ( OutdoorPressure == DefaultNodeValues.Press ) {
				OutdoorDryBulb = OutDryBulbTemp;
				OutdoorHumRat = OutHumRat;
				OutdoorPressure = OutBaroPress;
			} else {
				OutdoorDryBulb = Node( DXCoil( DXCoilNum ).CondenserInletNodeNum( 1 ) ).Temp;
				OutdoorHumRat = Node( DXCoil( DXCoilNum ).CondenserInletNodeNum( 1 ) ).HumRat;
			}
			if ( DXCoil( DXCoilNum ).IsSecondaryDXCoilInZone ) {
				OutdoorDryBulb = ZT( DXCoil( DXCoilNum ).SecZonePtr );
				OutdoorHumRat = ZoneAirHumRat( DXCoil( DXCoilNum ).SecZonePtr );
				//OutdoorWetBulb = DXCoil( DXCoilNum ).EvapInletWetBulb;
				OutdoorPressure = OutBaroPress;
			}
		} else if ( DXCoil( DXCoilNum ).IsSecondaryDXCoilInZone ) {
			OutdoorDryBulb = ZT( DXCoil( DXCoilNum ).SecZonePtr );
			OutdoorHumRat = ZoneAirHumRat( DXCoil( DXCoilNum ).SecZonePtr );
			//OutdoorWetBulb = DXCoil( DXCoilNum ).EvapInletWetBulb;
			OutdoorPressure = OutBaroPress;
		} else {
			OutdoorDryBulb = OutDryBulbTemp;
			OutdoorHumRat = OutHumRat;
			OutdoorPressure = OutBaroPress;
		}

		InletAirDryBulbTemp = DXCoil( DXCoilNum ).InletAirTemp;
		InletAirEnthalpy = DXCoil( DXCoilNum ).InletAirEnthalpy;
		InletAirHumRat = DXCoil( DXCoilNum ).InletAirHumRat;
		//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
		//InletAirPressure = DXCoil(DXCoilNum)%InletAirPressure
		//InletAirWetBulbC = PsyTwbFnTdbWPb(InletAirDryBulbTemp,InletAirHumRat,InletAirPressure)
		InletAirWetBulbC = PsyTwbFnTdbWPb( InletAirDryBulbTemp, InletAirHumRat, OutdoorPressure, RoutineName );
		PLRHeating = 0.0;
		DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction = 0.0;
		// Initialize crankcase heater, operates below OAT defined in input deck for HP DX heating coil
		if ( OutdoorDryBulb < DXCoil( DXCoilNum ).MaxOATCrankcaseHeater ) {
			CrankcaseHeatingPower = DXCoil( DXCoilNum ).CrankcaseHeaterCapacity;
		} else {
			CrankcaseHeatingPower = 0.0;
		}
		DXCoil( DXCoilNum ).PartLoadRatio = 0.0;
		HeatReclaimDXCoil( DXCoilNum ).AvailCapacity = 0.0;

		if ( ( AirMassFlow > 0.0 ) && ( GetCurrentScheduleValue( DXCoil( DXCoilNum ).SchedPtr ) > 0.0 ) && ( ( CycRatio > 0.0 ) || ( SpeedRatio > 0.0 && SingleMode == 0 ) ) && OutdoorDryBulb > DXCoil( DXCoilNum ).MinOATCompressor ) {

			if ( SpeedNum > 1 && SingleMode == 0 ) {

				// Check for valid air volume flow per rated total cooling capacity (200 - 600 cfm/ton) at low speed
				AirVolumeFlowRate = MSHPMassFlowRateLow / PsyRhoAirFnPbTdbW( OutdoorPressure, InletAirDryBulbTemp, InletAirHumRat, RoutineName );
				//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
				//  AirVolumeFlowRate = AirMassFlow/PsyRhoAirFnPbTdbW(InletAirPressure,InletAirDryBulbTemp, InletAirHumRat)
				VolFlowperRatedTotCap = AirVolumeFlowRate / DXCoil( DXCoilNum ).MSRatedTotCap( SpeedNumLS );
				if ( ( VolFlowperRatedTotCap < MinOperVolFlowPerRatedTotCap( DXCT ) ) || ( VolFlowperRatedTotCap > MaxHeatVolFlowPerRatedTotCap( DXCT ) ) ) {
					if ( DXCoil( DXCoilNum ).MSErrIndex( SpeedNumLS ) == 0 ) {
						ShowWarningMessage( DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + "\" - Air volume flow rate per watt of rated total heating capacity is out of range at speed " + TrimSigDigits( SpeedNumLS ) + '.' );
						ShowContinueErrorTimeStamp( "" );
						ShowContinueError( "Expected range for VolumeFlowPerRatedTotalCapacity=[" + RoundSigDigits( MinOperVolFlowPerRatedTotCap( DXCT ), 3 ) + "--" + RoundSigDigits( MaxHeatVolFlowPerRatedTotCap( DXCT ), 3 ) + "] Current value is " + RoundSigDigits( VolFlowperRatedTotCap, 3 ) + " m3/s/W" );
						ShowContinueError( "Possible causes include inconsistent air flow rates in system components or" );
						ShowContinueError( "inconsistent supply air fan operation modes in coil and unitary system objects." );
					}
					ShowRecurringWarningErrorAtEnd( DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + "\" - Air volume flow rate per watt of rated total heating capacity is out of range at speed " + TrimSigDigits( SpeedNumLS ) + " error continues...", DXCoil( DXCoilNum ).MSErrIndex( SpeedNumLS ), VolFlowperRatedTotCap, VolFlowperRatedTotCap );
				}

				// Check for valid air volume flow per rated total cooling capacity (200 - 600 cfm/ton) at high speed
				AirVolumeFlowRate = MSHPMassFlowRateHigh / PsyRhoAirFnPbTdbW( OutdoorPressure, InletAirDryBulbTemp, InletAirHumRat, RoutineName );
				//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
				//  AirVolumeFlowRate = AirMassFlow/PsyRhoAirFnPbTdbW(InletAirPressure,InletAirDryBulbTemp, InletAirHumRat)
				VolFlowperRatedTotCap = AirVolumeFlowRate / DXCoil( DXCoilNum ).MSRatedTotCap( SpeedNumHS );
				if ( ( VolFlowperRatedTotCap < MinOperVolFlowPerRatedTotCap( DXCT ) ) || ( VolFlowperRatedTotCap > MaxHeatVolFlowPerRatedTotCap( DXCT ) ) ) {
					if ( DXCoil( DXCoilNum ).MSErrIndex( SpeedNumHS ) == 0 ) {
						ShowWarningMessage( DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + "\" - Air volume flow rate per watt of rated total heating capacity is out of range at speed " + TrimSigDigits( SpeedNumHS ) + '.' );
						ShowContinueErrorTimeStamp( "" );
						ShowContinueError( "Expected range for VolumeFlowPerRatedTotalCapacity=[" + RoundSigDigits( MinOperVolFlowPerRatedTotCap( DXCT ), 3 ) + "--" + RoundSigDigits( MaxHeatVolFlowPerRatedTotCap( DXCT ), 3 ) + "] Current value is " + RoundSigDigits( VolFlowperRatedTotCap, 3 ) + " m3/s/W" );
						ShowContinueError( "Possible causes include inconsistent air flow rates in system components or" );
						ShowContinueError( "inconsistent supply air fan operation modes in coil and unitary system objects." );
					}
					ShowRecurringWarningErrorAtEnd( DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + "\" - Air volume flow rate per watt of rated total heating capacity is out of range at speed " + TrimSigDigits( SpeedNumHS ) + " error continues...", DXCoil( DXCoilNum ).MSErrIndex( SpeedNumHS ), VolFlowperRatedTotCap, VolFlowperRatedTotCap );
				}

				// Get total capacity modifying factor (function of temperature) for off-rated conditions
				// Model was extended to accept bi-quadratic curves. This allows sensitivity of the heating capacity
				// to the entering dry-bulb temperature as well as the outside dry-bulb temperature. User is
				// advised to use the bi-quaratic curve if sufficient manufacturer data is available.
				// Low speed
				if ( ( DXCoil( DXCoilNum ).MSTotCapTempModFacCurveType( SpeedNumLS ) == Quadratic ) || ( DXCoil( DXCoilNum ).MSTotCapTempModFacCurveType( SpeedNumLS ) == Cubic ) ) {
					TotCapTempModFac = CurveValue( DXCoil( DXCoilNum ).MSCCapFTemp( SpeedNumLS ), OutdoorDryBulb );
				} else if ( DXCoil( DXCoilNum ).MSTotCapTempModFacCurveType( SpeedNumLS ) == BiQuadratic ) {
					TotCapTempModFac = CurveValue( DXCoil( DXCoilNum ).MSCCapFTemp( SpeedNumLS ), InletAirDryBulbTemp, OutdoorDryBulb );
				} else {
					assert( false );
				}
				//  Get total capacity modifying factor (function of mass flow) for off-rated conditions
				TotCapFlowModFac = CurveValue( DXCoil( DXCoilNum ).MSCCapFFlow( SpeedNumLS ), AirMassFlowRatioLS );
				// Calculate total heating capacity for off-rated conditions
				TotCapLS = DXCoil( DXCoilNum ).MSRatedTotCap( SpeedNumLS ) * TotCapFlowModFac * TotCapTempModFac;
				// High speed
				if ( ( DXCoil( DXCoilNum ).MSTotCapTempModFacCurveType( SpeedNumHS ) == Quadratic ) || ( DXCoil( DXCoilNum ).MSTotCapTempModFacCurveType( SpeedNumHS ) == Cubic ) ) {
					TotCapTempModFac = CurveValue( DXCoil( DXCoilNum ).MSCCapFTemp( SpeedNumHS ), OutdoorDryBulb );
				} else if ( DXCoil( DXCoilNum ).MSTotCapTempModFacCurveType( SpeedNumHS ) == BiQuadratic ) {
					TotCapTempModFac = CurveValue( DXCoil( DXCoilNum ).MSCCapFTemp( SpeedNumHS ), InletAirDryBulbTemp, OutdoorDryBulb );
				} else {
					assert( false );
				}
				//  Get total capacity modifying factor (function of mass flow) for off-rated conditions
				TotCapFlowModFac = CurveValue( DXCoil( DXCoilNum ).MSCCapFFlow( SpeedNumHS ), AirMassFlowRatioHS );
				// Calculate total heating capacity for off-rated conditions
				TotCapHS = DXCoil( DXCoilNum ).MSRatedTotCap( SpeedNumHS ) * TotCapFlowModFac * TotCapTempModFac;
				// Calculate electricity consumed. First, get EIR modifying factors for off-rated conditions
				// Model was extended to accept bi-quadratic curves. This allows sensitivity of the EIR
				// to the entering dry-bulb temperature as well as the outside dry-bulb temperature. User is
				// advised to use the bi-quaratic curve if sufficient manufacturer data is available.
				// Low Speed
				if ( ( DXCoil( DXCoilNum ).MSEIRTempModFacCurveType( SpeedNumLS ) == Quadratic ) || ( DXCoil( DXCoilNum ).MSEIRTempModFacCurveType( SpeedNumLS ) == Cubic ) ) {
					EIRTempModFac = CurveValue( DXCoil( DXCoilNum ).MSEIRFTemp( SpeedNumLS ), OutdoorDryBulb );
				} else if ( DXCoil( DXCoilNum ).MSEIRTempModFacCurveType( SpeedNumLS ) == BiQuadratic ) {
					EIRTempModFac = CurveValue( DXCoil( DXCoilNum ).MSEIRFTemp( SpeedNumLS ), InletAirDryBulbTemp, OutdoorDryBulb );
				} else {
					assert( false );
				}
				EIRFlowModFac = CurveValue( DXCoil( DXCoilNum ).MSEIRFFlow( SpeedNumLS ), AirMassFlowRatioLS );
				EIRLS = 1.0 / DXCoil( DXCoilNum ).MSRatedCOP( SpeedNumLS ) * EIRTempModFac * EIRFlowModFac;
				// High Speed
				if ( ( DXCoil( DXCoilNum ).MSEIRTempModFacCurveType( SpeedNumHS ) == Quadratic ) || ( DXCoil( DXCoilNum ).MSEIRTempModFacCurveType( SpeedNumHS ) == Cubic ) ) {
					EIRTempModFac = CurveValue( DXCoil( DXCoilNum ).MSEIRFTemp( SpeedNumHS ), OutdoorDryBulb );
				} else if ( DXCoil( DXCoilNum ).MSEIRTempModFacCurveType( SpeedNumHS ) == BiQuadratic ) {
					EIRTempModFac = CurveValue( DXCoil( DXCoilNum ).MSEIRFTemp( SpeedNumHS ), InletAirDryBulbTemp, OutdoorDryBulb );
				}
				EIRFlowModFac = CurveValue( DXCoil( DXCoilNum ).MSEIRFFlow( SpeedNumHS ), AirMassFlowRatioHS );
				EIRHS = 1.0 / DXCoil( DXCoilNum ).MSRatedCOP( SpeedNumHS ) * EIRTempModFac * EIRFlowModFac;

				// Calculating adjustment factors for defrost
				// Calculate delta w through outdoor coil by assuming a coil temp of 0.82*DBT-9.7(F) per DOE2.1E
				OutdoorCoilT = 0.82 * OutdoorDryBulb - 8.589;
				OutdoorCoildw = max( 1.0e-6, ( OutdoorHumRat - PsyWFnTdpPb( OutdoorCoilT, OutdoorPressure, RoutineName ) ) );

				// Initializing defrost adjustment factors
				LoadDueToDefrostLS = 0.0;
				LoadDueToDefrostHS = 0.0;
				HeatingCapacityMultiplier = 1.0;
				FractionalDefrostTime = 0.0;
				InputPowerMultiplier = 1.0;
				DefrostPowerLS = 0.0;
				DefrostPowerHS = 0.0;

				// Check outdoor temperature to determine of defrost is active
				if ( OutdoorDryBulb <= DXCoil( DXCoilNum ).MaxOATDefrost ) {
					// Calculate defrost adjustment factors depending on defrost control type
					if ( DXCoil( DXCoilNum ).DefrostControl == Timed ) {
						FractionalDefrostTime = DXCoil( DXCoilNum ).DefrostTime;
						if ( FractionalDefrostTime > 0.0 ) {
							HeatingCapacityMultiplier = 0.909 - 107.33 * OutdoorCoildw;
							InputPowerMultiplier = 0.90 - 36.45 * OutdoorCoildw;
						}
					} else { //else defrost control is on-demand
						FractionalDefrostTime = 1.0 / ( 1.0 + 0.01446 / OutdoorCoildw );
						HeatingCapacityMultiplier = 0.875 * ( 1.0 - FractionalDefrostTime );
						InputPowerMultiplier = 0.954 * ( 1.0 - FractionalDefrostTime );
					}

					if ( FractionalDefrostTime > 0.0 ) {
						// Calculate defrost adjustment factors depending on defrost control strategy
						if ( DXCoil( DXCoilNum ).DefrostStrategy == ReverseCycle && DXCoil( DXCoilNum ).DefrostControl == OnDemand ) {
							DefrostEIRTempModFac = CurveValue( DXCoil( DXCoilNum ).DefrostEIRFT, max( 15.555, InletAirWetBulbC ), max( 15.555, OutdoorDryBulb ) );
							LoadDueToDefrostLS = ( 0.01 * FractionalDefrostTime ) * ( 7.222 - OutdoorDryBulb ) * ( DXCoil( DXCoilNum ).MSRatedTotCap( SpeedNumLS ) / 1.01667 );
							DefrostPowerLS = DefrostEIRTempModFac * ( DXCoil( DXCoilNum ).MSRatedTotCap( SpeedNumLS ) / 1.01667 ) * FractionalDefrostTime;
							LoadDueToDefrostHS = ( 0.01 * FractionalDefrostTime ) * ( 7.222 - OutdoorDryBulb ) * ( DXCoil( DXCoilNum ).MSRatedTotCap( SpeedNumHS ) / 1.01667 );
							DefrostPowerHS = DefrostEIRTempModFac * ( DXCoil( DXCoilNum ).MSRatedTotCap( SpeedNumHS ) / 1.01667 ) * FractionalDefrostTime;
						} else { // Defrost strategy is resistive
							DXCoil( DXCoilNum ).DefrostPower = DXCoil( DXCoilNum ).DefrostCapacity * FractionalDefrostTime;
						}
					} else { // Defrost is not active because (OutDryBulbTemp .GT. DXCoil(DXCoilNum)%MaxOATDefrost)
						DXCoil( DXCoilNum ).DefrostPower = 0.0;
					}
				}

				TotCapLSAdj = TotCapLS * HeatingCapacityMultiplier;
				TotCapHSAdj = TotCapHS * HeatingCapacityMultiplier;

				// Calculate modified PartLoadRatio due to defrost (reverse-cycle defrost only)
				PLRHeating = min( 1.0, ( SpeedRatio + LoadDueToDefrostHS / TotCapHSAdj ) );
				PLF = CurveValue( DXCoil( DXCoilNum ).MSPLFFPLR( SpeedNumHS ), PLRHeating ); // Calculate part-load factor

				if ( PLF < 0.7 ) {
					if ( DXCoil( DXCoilNum ).PLRErrIndex == 0 ) {
						ShowWarningMessage( "The PLF curve value at high speed for DX multispeed heating coil " + DXCoil( DXCoilNum ).Name + " =" + RoundSigDigits( PLF, 2 ) + " for part-load ratio =" + RoundSigDigits( PLRHeating, 2 ) );
						ShowContinueError( "PLF curve values must be >= 0.7. PLF has been reset to 0.7 and simulation is continuing." );
						ShowContinueError( "Check the IO reference manual for PLF curve guidance [Coil:Heating:DX:MultiSpeed]." );
						ShowContinueErrorTimeStamp( "" );
					}
					ShowRecurringWarningErrorAtEnd( "DX heating coil PLF curve < 0.7 warning continues... ", DXCoil( DXCoilNum ).PLRErrIndex, PLF, PLF );
					PLF = 0.7;
				}

				DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction = ( PLRHeating / PLF );
				if ( DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction > 1.0 && std::abs( DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction - 1.0 ) > 0.001 ) {
					if ( DXCoil( DXCoilNum ).ErrIndex4 == 0 ) {
						ShowWarningMessage( "The runtime fraction at high speed for DX multispeed heating coil " + DXCoil( DXCoilNum ).Name + " exceeded 1.0. [" + RoundSigDigits( DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction, 4 ) + "]." );
						ShowContinueError( "Runtime fraction is set to 1.0 and the simulation continues..." );
						ShowContinueError( "Check the IO reference manual for PLF curve guidance [Coil:Heating:DX:SingleSpeed]." );
						ShowContinueErrorTimeStamp( "" );
					}
					ShowRecurringWarningErrorAtEnd( DXCoil( DXCoilNum ).Name + ", DX heating coil runtime fraction > 1.0 warning continues...", DXCoil( DXCoilNum ).ErrIndex4, DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction, DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction );
					DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction = 1.0; // Reset coil runtime fraction to 1.0
				} else if ( DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction > 1.0 ) {
					DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction = 1.0; // Reset coil runtime fraction to 1.0
				}

				// Get full load output and power
				LSFullLoadOutAirEnth = InletAirEnthalpy + TotCapLSAdj / MSHPMassFlowRateLow;
				HSFullLoadOutAirEnth = InletAirEnthalpy + TotCapHSAdj / MSHPMassFlowRateHigh;
				LSElecHeatingPower = TotCapLS * EIRLS * InputPowerMultiplier;
				HSElecHeatingPower = TotCapHS * EIRHS * InputPowerMultiplier;
				OutletAirHumRat = InletAirHumRat;

				// if cycling fan, send coil part-load fraction to on/off fan via HVACDataGlobals
				if ( FanOpMode == CycFanCycCoil ) OnOffFanPartLoadFraction = 1.0;

				// Power calculation
				if ( ! DXCoil( DXCoilNum ).PLRImpact ) {
					DXCoil( DXCoilNum ).ElecHeatingPower = SpeedRatio * HSElecHeatingPower + ( 1.0 - SpeedRatio ) * LSElecHeatingPower;
				} else {
					DXCoil( DXCoilNum ).ElecHeatingPower = DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction * HSElecHeatingPower + ( 1.0 - DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction ) * LSElecHeatingPower;
				}

				DXCoil( DXCoilNum ).TotalHeatingEnergyRate = MSHPMassFlowRateHigh * ( HSFullLoadOutAirEnth - InletAirEnthalpy ) * SpeedRatio + MSHPMassFlowRateLow * ( LSFullLoadOutAirEnth - InletAirEnthalpy ) * ( 1.0 - SpeedRatio );
				OutletAirEnthalpy = InletAirEnthalpy + DXCoil( DXCoilNum ).TotalHeatingEnergyRate / DXCoil( DXCoilNum ).InletAirMassFlowRate;
				OutletAirTemp = PsyTdbFnHW( OutletAirEnthalpy, OutletAirHumRat );
				FullLoadOutAirRH = PsyRhFnTdbWPb( OutletAirTemp, OutletAirHumRat, OutdoorPressure, RoutineNameAverageLoad );
				if ( FullLoadOutAirRH > 1.0 ) { // Limit to saturated conditions at FullLoadOutAirEnth
					OutletAirTemp = PsyTsatFnHPb( FullLoadOutAirEnth, OutdoorPressure, RoutineName ); //Autodesk:Uninit FullLoadOutAirEnth was possibly uninitialized
					OutletAirHumRat = PsyWFnTdbH( OutletAirTemp, FullLoadOutAirEnth, RoutineName ); //Autodesk:Uninit FullLoadOutAirEnth was possibly uninitialized
				}

				// Waste heat calculation
				if ( DXCoil( DXCoilNum ).MSHPHeatRecActive ) {
					if ( DXCoil( DXCoilNum ).MSWasteHeat( SpeedNumLS ) == 0 ) {
						WasteHeatLS = DXCoil( DXCoilNum ).MSWasteHeatFrac( SpeedNumLS );
					} else {
						WasteHeatLS = CurveValue( DXCoil( DXCoilNum ).MSWasteHeat( SpeedNumLS ), OutdoorDryBulb, InletAirDryBulbTemp ) * DXCoil( DXCoilNum ).MSWasteHeatFrac( SpeedNumLS );
					}
					if ( DXCoil( DXCoilNum ).MSWasteHeat( SpeedNumHS ) == 0 ) {
						WasteHeatHS = DXCoil( DXCoilNum ).MSWasteHeatFrac( SpeedNumHS );
					} else {
						WasteHeatHS = CurveValue( DXCoil( DXCoilNum ).MSWasteHeat( SpeedNumHS ), OutdoorDryBulb, InletAirDryBulbTemp ) * DXCoil( DXCoilNum ).MSWasteHeatFrac( SpeedNumHS );
					}
					MSHPWasteHeat = ( SpeedRatio * WasteHeatHS + ( 1.0 - SpeedRatio ) * WasteHeatLS ) * DXCoil( DXCoilNum ).ElecHeatingPower;
				}
				if ( DXCoil( DXCoilNum ).FuelType != FuelTypeElectricity ) {

					DXCoil( DXCoilNum ).FuelUsed = DXCoil( DXCoilNum ).ElecHeatingPower;
					DXCoil( DXCoilNum ).ElecHeatingPower = 0.0;
				}

				// Adjust defrost power to correct for DOE-2 bug where defrost power is constant regardless of compressor runtime fraction
				// Defrosts happen based on compressor run time (frost buildup on outdoor coil), not total elapsed time.
				if ( DXCoil( DXCoilNum ).DefrostStrategy == ReverseCycle ) {
					if ( ! DXCoil( DXCoilNum ).PLRImpact ) {
						DXCoil( DXCoilNum ).DefrostPower = DefrostPowerHS * SpeedRatio + DefrostPowerLS * ( 1.0 - SpeedRatio );
					} else {
						DXCoil( DXCoilNum ).DefrostPower = DefrostPowerHS * DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction + DefrostPowerLS * ( 1.0 - DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction );
					}
				}
				DXCoil( DXCoilNum ).OutletAirTemp = OutletAirTemp;
				DXCoil( DXCoilNum ).OutletAirHumRat = OutletAirHumRat;
				DXCoil( DXCoilNum ).OutletAirEnthalpy = OutletAirEnthalpy;
				DXCoil( DXCoilNum ).CrankcaseHeaterPower = 0.0;

				// Stage 1
			}  else if ( CycRatio > 0.0 || ( CycRatio > 0.0 && SingleMode == 1 ) ) {

				// for cycling fan, reset mass flow to full on rate
				if ( FanOpMode == CycFanCycCoil ) AirMassFlow /= CycRatio;
				if ( FanOpMode == ContFanCycCoil ) AirMassFlow = MSHPMassFlowRateHigh;
				// Check for valid air volume flow per rated total cooling capacity (200 - 600 cfm/ton)
				AirVolumeFlowRate = AirMassFlow / PsyRhoAirFnPbTdbW( OutdoorPressure, InletAirDryBulbTemp, InletAirHumRat, RoutineName );
				//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
				//  AirVolumeFlowRate = AirMassFlow/PsyRhoAirFnPbTdbW(InletAirPressure,InletAirDryBulbTemp, InletAirHumRat)
				VolFlowperRatedTotCap = AirVolumeFlowRate / DXCoil( DXCoilNum ).MSRatedTotCap( SpeedNum );

				if ( ( VolFlowperRatedTotCap < MinOperVolFlowPerRatedTotCap( DXCT ) ) || ( VolFlowperRatedTotCap > MaxHeatVolFlowPerRatedTotCap( DXCT ) ) ) {
					if ( DXCoil( DXCoilNum ).ErrIndex1 == 0 ) {
						ShowWarningMessage( DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + "\" - Air volume flow rate per watt of rated total heating capacity is out of range at speed 1." );
						ShowContinueErrorTimeStamp( "" );
						ShowContinueError( "Expected range for VolumeFlowPerRatedTotalCapacity=[" + RoundSigDigits( MinOperVolFlowPerRatedTotCap( DXCT ), 3 ) + "--" + RoundSigDigits( MaxHeatVolFlowPerRatedTotCap( DXCT ), 3 ) + "] Current value is " + RoundSigDigits( VolFlowperRatedTotCap, 3 ) + " m3/s/W" );
						ShowContinueError( "Possible causes include inconsistent air flow rates in system components or" );
						ShowContinueError( "inconsistent supply air fan operation modes in coil and unitary system objects." );
					}
					ShowRecurringWarningErrorAtEnd( DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + "\" - Air volume flow rate per watt of rated total heating capacity is out of range error continues at speed 1...", DXCoil( DXCoilNum ).ErrIndex1, VolFlowperRatedTotCap, VolFlowperRatedTotCap );
				}

				// Get total capacity modifying factor (function of temperature) for off-rated conditions
				// Model was extended to accept bi-quadratic curves. This allows sensitivity of the heating capacity
				// to the entering dry-bulb temperature as well as the outside dry-bulb temperature. User is
				// advised to use the bi-quaratic curve if sufficient manufacturer data is available.
				if ( ( DXCoil( DXCoilNum ).MSTotCapTempModFacCurveType( SpeedNum ) == Quadratic ) || ( DXCoil( DXCoilNum ).MSTotCapTempModFacCurveType( SpeedNum ) == Cubic ) ) {
					TotCapTempModFac = CurveValue( DXCoil( DXCoilNum ).MSCCapFTemp( SpeedNum ), OutdoorDryBulb );
				} else if ( DXCoil( DXCoilNum ).MSTotCapTempModFacCurveType( SpeedNum ) == BiQuadratic ) {
					TotCapTempModFac = CurveValue( DXCoil( DXCoilNum ).MSCCapFTemp( SpeedNum ), InletAirDryBulbTemp, OutdoorDryBulb );
				}

				//  Get total capacity modifying factor (function of mass flow) for off-rated conditions
				//    AirMassFlowRatio = AirMassFlow/DXCoil(DXCoilNum)%MSRatedAirMassFlowRate(SpeedNumLS)
				//    TotCapFlowModFac = CurveValue(DXCoil(DXCoilNum)%MSCCapFFlow(SpeedNumLS),AirMassFlowRatio)
				TotCapFlowModFac = CurveValue( DXCoil( DXCoilNum ).MSCCapFFlow( SpeedNum ), AirMassFlowRatioLS );
				// Calculate total heating capacity for off-rated conditions
				TotCap = DXCoil( DXCoilNum ).MSRatedTotCap( SpeedNum ) * TotCapFlowModFac * TotCapTempModFac;

				// Calculating adjustment factors for defrost
				// Calculate delta w through outdoor coil by assuming a coil temp of 0.82*DBT-9.7(F) per DOE2.1E
				OutdoorCoilT = 0.82 * OutdoorDryBulb - 8.589;
				OutdoorCoildw = max( 1.0e-6, ( OutdoorHumRat - PsyWFnTdpPb( OutdoorCoilT, OutdoorPressure, RoutineName ) ) );

				// Initializing defrost adjustment factors
				LoadDueToDefrost = 0.0;
				HeatingCapacityMultiplier = 1.0;
				FractionalDefrostTime = 0.0;
				InputPowerMultiplier = 1.0;

				// Check outdoor temperature to determine of defrost is active
				if ( OutdoorDryBulb <= DXCoil( DXCoilNum ).MaxOATDefrost ) {
					// Calculate defrost adjustment factors depending on defrost control type
					if ( DXCoil( DXCoilNum ).DefrostControl == Timed ) {
						FractionalDefrostTime = DXCoil( DXCoilNum ).DefrostTime;
						if ( FractionalDefrostTime > 0.0 ) {
							HeatingCapacityMultiplier = 0.909 - 107.33 * OutdoorCoildw;
							InputPowerMultiplier = 0.90 - 36.45 * OutdoorCoildw;
						}
					} else { //else defrost control is on-demand
						FractionalDefrostTime = 1.0 / ( 1.0 + 0.01446 / OutdoorCoildw );
						HeatingCapacityMultiplier = 0.875 * ( 1.0 - FractionalDefrostTime );
						InputPowerMultiplier = 0.954 * ( 1.0 - FractionalDefrostTime );
					}

					if ( FractionalDefrostTime > 0.0 ) {
						// Calculate defrost adjustment factors depending on defrost control strategy
						if ( DXCoil( DXCoilNum ).DefrostStrategy == ReverseCycle && DXCoil( DXCoilNum ).DefrostControl == OnDemand ) {
							LoadDueToDefrost = ( 0.01 * FractionalDefrostTime ) * ( 7.222 - OutdoorDryBulb ) * ( DXCoil( DXCoilNum ).MSRatedTotCap( 1 ) / 1.01667 );
							DefrostEIRTempModFac = CurveValue( DXCoil( DXCoilNum ).DefrostEIRFT, max( 15.555, InletAirWetBulbC ), max( 15.555, OutdoorDryBulb ) );
							DXCoil( DXCoilNum ).DefrostPower = DefrostEIRTempModFac * ( DXCoil( DXCoilNum ).MSRatedTotCap( 1 ) / 1.01667 ) * FractionalDefrostTime;
						} else { // Defrost strategy is resistive
							DXCoil( DXCoilNum ).DefrostPower = DXCoil( DXCoilNum ).DefrostCapacity * FractionalDefrostTime;
						}
					} else { // Defrost is not active because (OutDryBulbTemp .GT. DXCoil(DXCoilNum)%MaxOATDefrost)
						DXCoil( DXCoilNum ).DefrostPower = 0.0;
					}
				}

				// Modify total heating capacity based on defrost heating capacity multiplier
				TotCapAdj = TotCap * HeatingCapacityMultiplier;

				// Calculate full load outlet conditions
				FullLoadOutAirEnth = InletAirEnthalpy + TotCapAdj / AirMassFlow;
				FullLoadOutAirHumRat = InletAirHumRat;
				FullLoadOutAirTemp = PsyTdbFnHW( FullLoadOutAirEnth, FullLoadOutAirHumRat );
				FullLoadOutAirRH = PsyRhFnTdbWPb( FullLoadOutAirTemp, FullLoadOutAirHumRat, OutdoorPressure, RoutineNameFullLoad );
				//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
				//  FullLoadOutAirRH = PsyRhFnTdbWPb(FullLoadOutAirTemp,FullLoadOutAirHumRat,InletAirPressure)
				if ( FullLoadOutAirRH > 1.0 ) { // Limit to saturated conditions at FullLoadOutAirEnth
					FullLoadOutAirTemp = PsyTsatFnHPb( FullLoadOutAirEnth, OutdoorPressure, RoutineName );
					//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
					//  FullLoadOutAirTemp = PsyTsatFnHPb(FullLoadOutAirEnth,InletAirPressure)
					FullLoadOutAirHumRat = PsyWFnTdbH( FullLoadOutAirTemp, FullLoadOutAirEnth, RoutineName );
				}

				// Set outlet conditions from the full load calculation
				OutletAirEnthalpy = FullLoadOutAirEnth;
				OutletAirHumRat = FullLoadOutAirHumRat;
				OutletAirTemp = FullLoadOutAirTemp;
				// Calculate electricity consumed. First, get EIR modifying factors for off-rated conditions
				// Model was extended to accept bi-quadratic curves. This allows sensitivity of the EIR
				// to the entering dry-bulb temperature as well as the outside dry-bulb temperature. User is
				// advised to use the bi-quadratic curve if sufficient manufacturer data is available.
				if ( ( DXCoil( DXCoilNum ).MSEIRTempModFacCurveType( 1 ) == Quadratic ) || ( DXCoil( DXCoilNum ).MSEIRTempModFacCurveType( 1 ) == Cubic ) ) {
					EIRTempModFac = CurveValue( DXCoil( DXCoilNum ).MSEIRFTemp( 1 ), OutdoorDryBulb );
				} else if ( DXCoil( DXCoilNum ).MSEIRTempModFacCurveType( 1 ) == BiQuadratic ) {
					EIRTempModFac = CurveValue( DXCoil( DXCoilNum ).MSEIRFTemp( 1 ), InletAirDryBulbTemp, OutdoorDryBulb );
				}
				EIRFlowModFac = CurveValue( DXCoil( DXCoilNum ).MSEIRFFlow( 1 ), AirMassFlowRatioLS );
				EIR = 1.0 / DXCoil( DXCoilNum ).MSRatedCOP( 1 ) * EIRTempModFac * EIRFlowModFac;
				// Calculate modified PartLoadRatio due to defrost (reverse-cycle defrost only)
				PLRHeating = min( 1.0, ( CycRatio + LoadDueToDefrost / TotCapAdj ) );
				PLF = CurveValue( DXCoil( DXCoilNum ).MSPLFFPLR( 1 ), PLRHeating ); // Calculate part-load factor
				if ( FanOpMode == CycFanCycCoil && CycRatio == 1.0 && PLF != 1.0 ) {
					if ( DXCoil( DXCoilNum ).PLFErrIndex == 0 ) {
						ShowWarningMessage( "The PLF curve value for DX heating coil " + DXCoil( DXCoilNum ).Name + " =" + RoundSigDigits( PLF, 2 ) + " for part-load ratio = 1" );
						ShowContinueError( "PLF curve value must be = 1.0 and has been reset to 1.0. Simulation is continuing." );
						ShowContinueErrorTimeStamp( "" );
					}
					ShowRecurringWarningErrorAtEnd( DXCoil( DXCoilNum ).Name + "\": DX heating coil PLF curve value <> 1.0 warning continues...", DXCoil( DXCoilNum ).PLFErrIndex, PLF, PLF );
					PLF = 1.0;
				}

				if ( PLF < 0.7 ) {
					if ( DXCoil( DXCoilNum ).PLRErrIndex == 0 ) {
						ShowWarningMessage( "The PLF curve value for DX heating coil " + DXCoil( DXCoilNum ).Name + " =" + RoundSigDigits( PLF, 2 ) + " for part-load ratio =" + RoundSigDigits( PLRHeating, 2 ) );
						ShowContinueError( "PLF curve values must be >= 0.7. PLF has been reset to 0.7 and simulation is continuing." );
						ShowContinueError( "Check the IO reference manual for PLF curve guidance [Coil:Heating:DX:SingleSpeed]." );
						ShowContinueErrorTimeStamp( "" );
					}
					ShowRecurringWarningErrorAtEnd( "DX heating coil PLF curve < 0.7 warning continues... ", DXCoil( DXCoilNum ).PLRErrIndex, PLF, PLF );
					PLF = 0.7;
				}

				DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction = ( PLRHeating / PLF );
				if ( DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction > 1.0 && std::abs( DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction - 1.0 ) > 0.001 ) {
					if ( DXCoil( DXCoilNum ).ErrIndex4 == 0 ) {
						ShowWarningMessage( "The runtime fraction for DX heating coil " + DXCoil( DXCoilNum ).Name + " exceeded 1.0. [" + RoundSigDigits( DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction, 4 ) + "]." );
						ShowContinueError( "Runtime fraction is set to 1.0 and the simulation continues..." );
						ShowContinueError( "Check the IO reference manual for PLF curve guidance [Coil:Heating:DX:SingleSpeed]." );
						ShowContinueErrorTimeStamp( "" );
					}
					ShowRecurringWarningErrorAtEnd( DXCoil( DXCoilNum ).Name + ", DX heating coil runtime fraction > 1.0 warning continues...", DXCoil( DXCoilNum ).ErrIndex4, DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction, DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction );
					DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction = 1.0; // Reset coil runtime fraction to 1.0
				} else if ( DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction > 1.0 ) {
					DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction = 1.0; // Reset coil runtime fraction to 1.0
				}
				// if cycling fan, send coil part-load fraction to on/off fan via HVACDataGlobals
				if ( FanOpMode == CycFanCycCoil ) OnOffFanPartLoadFraction = PLF;
				DXCoil( DXCoilNum ).ElecHeatingPower = TotCap * EIR * DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction * InputPowerMultiplier;

				// Calculate crankcase heater power using the runtime fraction for this DX heating coil only if there is no companion DX coil.
				// Else use the largest runtime fraction of this DX heating coil and the companion DX cooling coil.

				if ( DXCoil( DXCoilNum ).CompanionUpstreamDXCoil == 0 ) {
					DXCoil( DXCoilNum ).CrankcaseHeaterPower = CrankcaseHeatingPower * ( 1.0 - DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction );
				} else {
					DXCoil( DXCoilNum ).CrankcaseHeaterPower = CrankcaseHeatingPower * ( 1.0 - max( DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction, DXCoil( DXCoil( DXCoilNum ).CompanionUpstreamDXCoil ).CoolingCoilRuntimeFraction ) );
				}

				DXCoil( DXCoilNum ).TotalHeatingEnergyRate = AirMassFlow * ( FullLoadOutAirEnth - InletAirEnthalpy ) * CycRatio;
				if ( FanOpMode == ContFanCycCoil ) {
					OutletAirEnthalpy = InletAirEnthalpy + DXCoil( DXCoilNum ).TotalHeatingEnergyRate / DXCoil( DXCoilNum ).InletAirMassFlowRate;
					OutletAirTemp = PsyTdbFnHW( OutletAirEnthalpy, OutletAirHumRat );
				}
				if ( DXCoil( DXCoilNum ).MSHPHeatRecActive ) {
					if ( DXCoil( DXCoilNum ).MSWasteHeat( SpeedNum ) == 0 ) {
						MSHPWasteHeat = DXCoil( DXCoilNum ).MSWasteHeatFrac( SpeedNum ) * DXCoil( DXCoilNum ).ElecHeatingPower;
					} else {
						MSHPWasteHeat = CurveValue( DXCoil( DXCoilNum ).MSWasteHeat( SpeedNum ), OutdoorDryBulb, InletAirDryBulbTemp ) * DXCoil( DXCoilNum ).MSWasteHeatFrac( SpeedNum ) * DXCoil( DXCoilNum ).ElecHeatingPower;
					}
				}
				if ( DXCoil( DXCoilNum ).FuelType != FuelTypeElectricity ) {

					DXCoil( DXCoilNum ).FuelUsed = DXCoil( DXCoilNum ).ElecHeatingPower;
					DXCoil( DXCoilNum ).ElecHeatingPower = 0.0;
				}
				// Adjust defrost power to correct for DOE-2 bug where defrost power is constant regardless of compressor runtime fraction
				// Defrosts happen based on compressor run time (frost buildup on outdoor coil), not total elapsed time.
				DXCoil( DXCoilNum ).DefrostPower *= DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction;

				DXCoil( DXCoilNum ).OutletAirTemp = OutletAirTemp;
				DXCoil( DXCoilNum ).OutletAirHumRat = OutletAirHumRat;
				DXCoil( DXCoilNum ).OutletAirEnthalpy = OutletAirEnthalpy;
			}

		} else {

			// DX coil is off; just pass through conditions
			DXCoil( DXCoilNum ).OutletAirEnthalpy = DXCoil( DXCoilNum ).InletAirEnthalpy;
			DXCoil( DXCoilNum ).OutletAirHumRat = DXCoil( DXCoilNum ).InletAirHumRat;
			DXCoil( DXCoilNum ).OutletAirTemp = DXCoil( DXCoilNum ).InletAirTemp;

			DXCoil( DXCoilNum ).ElecHeatingPower = 0.0;
			DXCoil( DXCoilNum ).FuelUsed = 0.0;
			DXCoil( DXCoilNum ).TotalHeatingEnergyRate = 0.0;
			DXCoil( DXCoilNum ).DefrostPower = 0.0;

			// Calculate crankcase heater power using the runtime fraction for this DX heating coil (here DXHeatingCoilRTF=0) if
			// there is no companion DX coil, or the runtime fraction of the companion DX cooling coil (here DXCoolingCoilRTF>=0).
			if ( DXCoil( DXCoilNum ).CompanionUpstreamDXCoil == 0 ) {
				DXCoil( DXCoilNum ).CrankcaseHeaterPower = CrankcaseHeatingPower;
			} else {
				DXCoil( DXCoilNum ).CrankcaseHeaterPower = CrankcaseHeatingPower * ( 1.0 - DXCoil( DXCoil( DXCoilNum ).CompanionUpstreamDXCoil ).CoolingCoilRuntimeFraction );
			}

		} // end of on/off if - else

		DXCoilOutletTemp( DXCoilNum ) = DXCoil( DXCoilNum ).OutletAirTemp;
		DXCoilOutletHumRat( DXCoilNum ) = DXCoil( DXCoilNum ).OutletAirHumRat;
		DXCoil( DXCoilNum ).PartLoadRatio = PLRHeating;
		DXCoilFanOpMode( DXCoilNum ) = FanOpMode;
		DXCoilPartLoadRatio( DXCoilNum ) = PLRHeating;
		DXCoil( DXCoilNum ).MSSpeedNumLS = SpeedNumLS;
		DXCoil( DXCoilNum ).MSSpeedNumHS = SpeedNumHS;
		DXCoil( DXCoilNum ).MSSpeedRatio = SpeedRatio;
		DXCoil( DXCoilNum ).MSCycRatio = CycRatio;

		// calc secondary coil if specified
		if ( DXCoil( DXCoilNum ).IsSecondaryDXCoilInZone ) {
			CalcSecondaryDXCoils( DXCoilNum );
		}

	}

	void
	UpdateDXCoil( int const DXCoilNum ) // number of the current fan coil unit being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   May 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for passing results to the outlet air node.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataContaminantBalance::Contaminant;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int AirOutletNode; // air outlet node number
		int AirInletNode; // air inlet node number

		AirOutletNode = DXCoil( DXCoilNum ).AirOutNode;
		AirInletNode = DXCoil( DXCoilNum ).AirInNode;
		// changed outputs
		Node( AirOutletNode ).Enthalpy = DXCoil( DXCoilNum ).OutletAirEnthalpy;
		Node( AirOutletNode ).Temp = DXCoil( DXCoilNum ).OutletAirTemp;
		Node( AirOutletNode ).HumRat = DXCoil( DXCoilNum ).OutletAirHumRat;
		Node( AirOutletNode ).MassFlowRate = DXCoil( DXCoilNum ).InletAirMassFlowRate;
		// pass through outputs
		Node( AirOutletNode ).Quality = Node( AirInletNode ).Quality;
		Node( AirOutletNode ).Press = Node( AirInletNode ).Press;
		Node( AirOutletNode ).MassFlowRateMin = Node( AirInletNode ).MassFlowRateMin;
		Node( AirOutletNode ).MassFlowRateMax = Node( AirInletNode ).MassFlowRateMax;
		Node( AirOutletNode ).MassFlowRateMinAvail = Node( AirInletNode ).MassFlowRateMinAvail;
		Node( AirOutletNode ).MassFlowRateMaxAvail = Node( AirInletNode ).MassFlowRateMaxAvail;

		if ( Contaminant.CO2Simulation ) {
			Node( AirOutletNode ).CO2 = Node( AirInletNode ).CO2;
		}
		if ( Contaminant.GenericContamSimulation ) {
			Node( AirOutletNode ).GenContam = Node( AirInletNode ).GenContam;
		}

	}

	void
	ReportDXCoil( int const DXCoilNum ) // number of the current fan coil unit being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   May 2000
		//       MODIFIED       Richard Raustad/Don Shirey Oct 2001, Feb 2004
		//                      Feb 2005 M. J. Witte, GARD Analytics, Inc.
		//                        Always update evap value to support new coil type COIL:DX:MultiMode:CoolingEmpirical:
		//                      Lixing Gu. Jan. 5, 2007, pass information to the AirflowNetwork model
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Fills some of the report variables for the DX coils

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHVACGlobals::TimeStepSys;
		using DataHVACGlobals::DXElecCoolingPower;
		using DataHVACGlobals::DXElecHeatingPower;
		using Psychrometrics::RhoH2O;
		using DataWater::WaterStorage;
		using DataAirLoop::LoopDXCoilRTF;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 RhoWater;
		Real64 Tavg;
		Real64 SpecHumOut;
		Real64 SpecHumIn;
		Real64 ReportingConstant; // Number of seconds per HVAC system time step, to convert from W (J/s) to J

		ReportingConstant = TimeStepSys * SecInHour;

		{ auto const SELECT_CASE_var( DXCoil( DXCoilNum ).DXCoilType_Num );

		if ( ( SELECT_CASE_var == CoilDX_HeatingEmpirical ) || ( SELECT_CASE_var == CoilVRF_Heating ) || ( SELECT_CASE_var == CoilVRF_FluidTCtrl_Heating ) ) {
			DXCoil( DXCoilNum ).TotalHeatingEnergy = DXCoil( DXCoilNum ).TotalHeatingEnergyRate * ReportingConstant;
			DXCoil( DXCoilNum ).ElecHeatingConsumption = DXCoil( DXCoilNum ).ElecHeatingPower * ReportingConstant;
			DXCoil( DXCoilNum ).DefrostConsumption = DXCoil( DXCoilNum ).DefrostPower * ReportingConstant;
			DXCoil( DXCoilNum ).CrankcaseHeaterConsumption = DXCoil( DXCoilNum ).CrankcaseHeaterPower * ReportingConstant;
			DXElecHeatingPower = DXCoil( DXCoilNum ).ElecHeatingPower + DXCoil( DXCoilNum ).CrankcaseHeaterPower;
		} else if ( SELECT_CASE_var == CoilDX_MultiSpeedHeating ) {
			DXCoil( DXCoilNum ).TotalHeatingEnergy = DXCoil( DXCoilNum ).TotalHeatingEnergyRate * ReportingConstant;
			if ( DXCoil( DXCoilNum ).FuelType == FuelTypeElectricity ) {
				DXCoil( DXCoilNum ).ElecHeatingConsumption = DXCoil( DXCoilNum ).ElecHeatingPower * ReportingConstant;
			} else {
				DXCoil( DXCoilNum ).FuelConsumed = DXCoil( DXCoilNum ).FuelUsed * ReportingConstant;
			}
			DXCoil( DXCoilNum ).DefrostConsumption = DXCoil( DXCoilNum ).DefrostPower * ReportingConstant;
			DXCoil( DXCoilNum ).CrankcaseHeaterConsumption = DXCoil( DXCoilNum ).CrankcaseHeaterPower * ReportingConstant;
			DXElecHeatingPower = DXCoil( DXCoilNum ).ElecHeatingPower + DXCoil( DXCoilNum ).CrankcaseHeaterPower;
		} else if ( SELECT_CASE_var == CoilDX_MultiSpeedCooling ) {
			DXCoil( DXCoilNum ).TotalCoolingEnergy = DXCoil( DXCoilNum ).TotalCoolingEnergyRate * ReportingConstant;
			DXCoil( DXCoilNum ).SensCoolingEnergy = DXCoil( DXCoilNum ).SensCoolingEnergyRate * ReportingConstant;
			DXCoil( DXCoilNum ).LatCoolingEnergy = DXCoil( DXCoilNum ).TotalCoolingEnergy - DXCoil( DXCoilNum ).SensCoolingEnergy;
			DXCoil( DXCoilNum ).CrankcaseHeaterConsumption = DXCoil( DXCoilNum ).CrankcaseHeaterPower * ReportingConstant;
			DXElecCoolingPower = DXCoil( DXCoilNum ).ElecCoolingPower;
			DXCoil( DXCoilNum ).EvapCondPumpElecConsumption = DXCoil( DXCoilNum ).EvapCondPumpElecPower * ReportingConstant;
			DXCoil( DXCoilNum ).EvapWaterConsump = DXCoil( DXCoilNum ).EvapWaterConsumpRate * ReportingConstant;
			if ( DXCoil( DXCoilNum ).FuelType == FuelTypeElectricity ) {
				DXCoil( DXCoilNum ).ElecCoolingConsumption = DXCoil( DXCoilNum ).ElecCoolingPower * ReportingConstant;
			} else {
				DXCoil( DXCoilNum ).FuelConsumed = DXCoil( DXCoilNum ).FuelUsed * ReportingConstant;
			}
			if ( any_eq( DXCoil( DXCoilNum ).CondenserType, EvapCooled ) ) {
				DXCoil( DXCoilNum ).BasinHeaterConsumption = DXCoil( DXCoilNum ).BasinHeaterPower * ReportingConstant;
			}
		} else if ( SELECT_CASE_var == CoilDX_HeatPumpWaterHeaterPumped || SELECT_CASE_var == CoilDX_HeatPumpWaterHeaterWrapped ) {
			// water heating energy for HP water heater DX Coil condenser
			DXCoil( DXCoilNum ).TotalHeatingEnergy = DXCoil( DXCoilNum ).TotalHeatingEnergyRate * ReportingConstant;
			// water heating power for HP water heater
			DXCoil( DXCoilNum ).ElecWaterHeatingConsumption = DXCoil( DXCoilNum ).ElecWaterHeatingPower * ReportingConstant;
			// other usual DX cooling coil outputs
			DXCoil( DXCoilNum ).TotalCoolingEnergy = DXCoil( DXCoilNum ).TotalCoolingEnergyRate * ReportingConstant;
			DXCoil( DXCoilNum ).SensCoolingEnergy = DXCoil( DXCoilNum ).SensCoolingEnergyRate * ReportingConstant;
			DXCoil( DXCoilNum ).LatCoolingEnergy = DXCoil( DXCoilNum ).TotalCoolingEnergy - DXCoil( DXCoilNum ).SensCoolingEnergy;
			DXCoil( DXCoilNum ).ElecCoolingConsumption = DXCoil( DXCoilNum ).ElecCoolingPower * ReportingConstant;
			DXCoil( DXCoilNum ).CrankcaseHeaterConsumption = DXCoil( DXCoilNum ).CrankcaseHeaterPower * ReportingConstant;
			// DXElecCoolingPower global is only used for air-to-air cooling and heating coils
			DXElecCoolingPower = 0.0;
		} else {
			DXCoil( DXCoilNum ).TotalCoolingEnergy = DXCoil( DXCoilNum ).TotalCoolingEnergyRate * ReportingConstant;
			DXCoil( DXCoilNum ).SensCoolingEnergy = DXCoil( DXCoilNum ).SensCoolingEnergyRate * ReportingConstant;
			DXCoil( DXCoilNum ).LatCoolingEnergy = DXCoil( DXCoilNum ).TotalCoolingEnergy - DXCoil( DXCoilNum ).SensCoolingEnergy;
			DXCoil( DXCoilNum ).ElecCoolingConsumption = DXCoil( DXCoilNum ).ElecCoolingPower * ReportingConstant;
			DXCoil( DXCoilNum ).CrankcaseHeaterConsumption = DXCoil( DXCoilNum ).CrankcaseHeaterPower * ReportingConstant;
			DXElecCoolingPower = DXCoil( DXCoilNum ).ElecCoolingPower;
			DXCoil( DXCoilNum ).EvapCondPumpElecConsumption = DXCoil( DXCoilNum ).EvapCondPumpElecPower * ReportingConstant;
			DXCoil( DXCoilNum ).EvapWaterConsump = DXCoil( DXCoilNum ).EvapWaterConsumpRate * ReportingConstant;
			if ( any_eq( DXCoil( DXCoilNum ).CondenserType, EvapCooled ) ) {
				DXCoil( DXCoilNum ).BasinHeaterConsumption = DXCoil( DXCoilNum ).BasinHeaterPower * ReportingConstant;
			}
		}}

		if ( DXCoil( DXCoilNum ).CondensateCollectMode == CondensateToTank ) {
			// calculate and report condensation rates  (how much water extracted from the air stream)
			// water flow of water in m3/s for water system interactions
			//  put here to catch all types of DX coils
			Tavg = ( DXCoil( DXCoilNum ).InletAirTemp - DXCoil( DXCoilNum ).OutletAirTemp ) / 2.0;
			RhoWater = RhoH2O( Tavg );
			// CR9155 Remove specific humidity calculations
			SpecHumIn = DXCoil( DXCoilNum ).InletAirHumRat;
			SpecHumOut = DXCoil( DXCoilNum ).OutletAirHumRat;
			//  mdot * del HumRat / rho water
			DXCoil( DXCoilNum ).CondensateVdot = max( 0.0, ( DXCoil( DXCoilNum ).InletAirMassFlowRate * ( SpecHumIn - SpecHumOut ) / RhoWater ) );
			DXCoil( DXCoilNum ).CondensateVol = DXCoil( DXCoilNum ).CondensateVdot * ReportingConstant;

			WaterStorage( DXCoil( DXCoilNum ).CondensateTankID ).VdotAvailSupply( DXCoil( DXCoilNum ).CondensateTankSupplyARRID ) = DXCoil( DXCoilNum ).CondensateVdot;
			WaterStorage( DXCoil( DXCoilNum ).CondensateTankID ).TwaterSupply( DXCoil( DXCoilNum ).CondensateTankSupplyARRID ) = DXCoil( DXCoilNum ).OutletAirTemp;
		}

		LoopDXCoilRTF = max( DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction, DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction );

	}

	void
	CalcTwoSpeedDXCoilStandardRating( int const DXCoilNum )
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith, (Derived from CalcDXCoilStandardRating by Bereket Nigusse & Chandan Sharma)
		//       DATE WRITTEN   July 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate the following
		//                 (1) Standard Rated (net) Cooling Capacity
		//                 (2) Energy Efficiency Ratio (EER),
		//                 (3) Integrated Energy Efficiency Ratio (IEER)

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// ANSI/AHRI Standard 340/360-2007, Peformance Rating of Commercial and Industrial Unitary Air-Conditioning and
		//  Heat Pump Equipment, Air-Conditioning, Heating, and Refrigeration Institute, Arlingtion VA.

		// USE STATEMENTS:

		// Using/Aliasing
		using CurveManager::CurveValue;
		using Fans::GetFanPower;
		using Fans::GetFanInletNode;
		using Fans::GetFanOutletNode;
		using Fans::SimulateFanComponents;
		using DataEnvironment::OutBaroPress;
		using General::SolveRegulaFalsi;
		using General::RoundSigDigits;
		using namespace OutputReportPredefined;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// AHRI Standard 340/360-2007 Peformance Rating of Commercial and Industrial Unitary Air-Conditioning and Heat Pump Equipment
		Real64 const CoolingCoilInletAirWetBulbTempRated( 19.4 ); // 19.44C (67F)
		Real64 const CoolingCoilInletAirDryBulbTempRated( 26.7 );
		Real64 const OutdoorUnitInletAirDryBulbTempRated( 35.0 ); // 35.00C (95F)
		static Array1D< Real64 > const OutdoorUnitInletAirDryBulbTempPLTestPoint( 3, { 27.5, 20.0, 18.3 } );
		static Array1D< Real64 > const NetCapacityFactorPLTestPoint( 3, { 0.75, 0.50, 0.25 } );
		Real64 const ConvFromSIToIP( 3.412141633 ); // Conversion from SI to IP [3.412 Btu/hr-W]

		Real64 const AirMassFlowRatioRated( 1.0 ); // AHRI test is at the design flow rate
		// and hence AirMassFlowRatio is 1.0

		Real64 const DefaultFanPowerPerEvapAirFlowRate( 773.3 ); // 365 W/1000 scfm or 773.3 W/(m3/s). The AHRI standard
		// specifies a nominal/default fan electric power consumption per rated air
		// volume flow rate to account for indoor fan electric power consumption
		// when the standard tests are conducted on units that do not have an
		// indoor air circulting fan. Used if user doesn't enter a specific value.
		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static std::string const RoutineName( "CalcTwoSpeedDXCoilStandardRating" );

		static Real64 NetCoolingCapRated( 0.0 ); // Net Cooling Coil capacity at Rated conditions, accounting for supply fan heat [W]
		static Real64 EER( 0.0 ); // Energy Efficiency Ratio in SI [W/W]
		static Real64 IEER( 0.0 ); // Integerated Energy Efficiency Ratio in SI [W/W]
		static Real64 TotCapTempModFac( 0.0 ); // Total capacity modifier (function of entering wetbulb, outside drybulb) [-]
		static Real64 TotCapFlowModFac( 0.0 ); // Total capacity modifier (function of actual supply air flow vs rated flow) [-]
		static Real64 EIRTempModFac( 0.0 ); // EIR modifier (function of entering wetbulb, outside drybulb) [-]
		static Real64 EIRFlowModFac( 0.0 ); // EIR modifier (function of actual supply air flow vs rated flow) [-]
		Real64 EIR;
		Real64 TotalElecPowerRated;
		Array1D< Real64 > EER_TestPoint_SI( 4 ); // 1 = A, 2 = B, 3= C, 4= D
		Array1D< Real64 > EER_TestPoint_IP( 4 ); // 1 = A, 2 = B, 3= C, 4= D
		Array1D< Real64 > NetCapacity_TestPoint( 4 ); // 1 = A, 2 = B, 3= C, 4= D
		Array1D< Real64 > NetPower_TestPoint( 4 ); // 1 = A, 2 = B, 3= C, 4= D
		Array1D< Real64 > SupAirMdot_TestPoint( 4 ); // 1 = A, 2 = B, 3= C, 4= D

		static Real64 TempDryBulb_Leaving_Apoint( 0.0 );

		Real64 HighSpeedNetCoolingCap;
		Real64 LowSpeedNetCoolingCap;

		Real64 PartLoadAirMassFlowRate;
		Real64 AirMassFlowRatio;
		static Real64 AccuracyTolerance( 0.2 ); // tolerance in AHRI 340/360 Table 6 note 1
		static int MaximumIterations( 500 );
		int SolverFlag;
		Array1D< Real64 > Par( 12 ); // Parameter array passed to solver
		Real64 EIR_HighSpeed;
		Real64 EIR_LowSpeed;
		int FanInletNode;
		int FanOutletNode;
		int Iter;
		Real64 ExternalStatic;
		Real64 FanStaticPressureRise;
		static bool ErrorsFound( false );
		Real64 FanHeatCorrection;
		Real64 FanPowerCorrection;
		Real64 FanPowerPerEvapAirFlowRate;
		Real64 SpeedRatio;
		Real64 CycRatio;
		Real64 TargetNetCapacity;
		Real64 SupplyAirHumRat;
		Real64 SupplyAirRho;
		Real64 SupplyAirVolFlowRate;
		Real64 HighSpeedTotCoolingCap;
		Real64 LowSpeedTotCoolingCap;
		Real64 TotCoolingCap;
		Real64 NetCoolingCap;
		Real64 PLF;
		Real64 RunTimeFraction;
		Real64 LowerBoundMassFlowRate;
		static bool OneTimeEIOHeaderWrite( true );
		int PartLoadTestPoint;
		int countStaticInputs;
		int index;

		// Formats
		static gio::Fmt Format_890( "('! <VAV DX Cooling Coil Standard Rating Information>, DX Coil Type, DX Coil Name, Fan Type, Fan Name, ','Standard Net Cooling Capacity {W}, Standard Net Cooling Capacity {Btu/h}, IEER {Btu/W-h}, ','COP 100% Capacity {W/W}, COP 75% Capacity {W/W}, COP 50% Capacity {W/W}, COP 25% Capacity {W/W}, ','EER 100% Capacity {Btu/W-h}, EER 75% Capacity {Btu/W-h}, EER 50% Capacity {Btu/W-h}, EER 25% Capacity {Btu/W-h}, ','Supply Air Flow 100% {kg/s}, Supply Air Flow 75% {kg/s},Supply Air Flow 50% {kg/s},Supply Air Flow 25% {kg/s}')" );
		static gio::Fmt Format_891( "(' VAV DX Cooling Coil Standard Rating Information, ',A,',',A,',',A,',',A,',',A,',',A,',',A,',',A,',',A,',',A,',',A,',',A,',',A,',',A,',',A,',',A,',',A,',',A,',',A,',',A)" );

		// Get fan index and name if not already available
		if ( DXCoil( DXCoilNum ).SupplyFanIndex == 0 ) GetFanIndexForTwoSpeedCoil( DXCoilNum, DXCoil( DXCoilNum ).SupplyFanIndex, DXCoil( DXCoilNum ).SupplyFanName );
		if ( DXCoil( DXCoilNum ).SupplyFanIndex == 0 ) { // didn't find VAV fan, do not rate this coil
			DXCoil( DXCoilNum ).RateWithInternalStaticAndFanObject = false;
			ShowWarningError( "CalcTwoSpeedDXCoilStandardRating: Did not find a variable air volume fan associated with DX coil named = \"" + DXCoil( DXCoilNum ).Name + "\". Standard Ratings will not be calculated." );
			return;
		}

		// CALL CheckCurveLimitsForStandardRatings(

		// Calculate the Indoor fan electric power consumption.  The electric power consumption is estimated
		// using either user supplied or AHRI default value for fan power per air volume flow rate
		if ( DXCoil( DXCoilNum ).RateWithInternalStaticAndFanObject ) {

			TotCapFlowModFac = CurveValue( DXCoil( DXCoilNum ).CCapFFlow( 1 ), AirMassFlowRatioRated );
			TotCapTempModFac = CurveValue( DXCoil( DXCoilNum ).CCapFTemp( 1 ), CoolingCoilInletAirWetBulbTempRated, OutdoorUnitInletAirDryBulbTempRated );
			for ( Iter = 1; Iter <= 4; ++Iter ) { // iterative solution in the event that net capacity is near a threshold for external static
				//Obtain external static pressure from Table 5 in ANSI/AHRI Std. 340/360-2007
				if ( NetCoolingCapRated <= 21000.0 ) {
					ExternalStatic = 50.0;
				} else if ( 21000.0 < NetCoolingCapRated && NetCoolingCapRated <= 30800.0 ) {
					ExternalStatic = 60.0;
				} else if ( 30800.0 < NetCoolingCapRated && NetCoolingCapRated <= 39300.0 ) {
					ExternalStatic = 70.0;
				} else if ( 39300.0 < NetCoolingCapRated && NetCoolingCapRated <= 61500.0 ) {
					ExternalStatic = 90.0;
				} else if ( 61500.0 < NetCoolingCapRated && NetCoolingCapRated <= 82100.0 ) {
					ExternalStatic = 100.0;
				} else if ( 82100.0 < NetCoolingCapRated && NetCoolingCapRated <= 103000.0 ) {
					ExternalStatic = 110.0;
				} else if ( 103000.0 < NetCoolingCapRated && NetCoolingCapRated <= 117000.0 ) {
					ExternalStatic = 140.0;
				} else if ( 117000.0 < NetCoolingCapRated && NetCoolingCapRated <= 147000.0 ) {
					ExternalStatic = 160.0;
				} else if ( 147000.0 < NetCoolingCapRated ) {
					ExternalStatic = 190.0;
				}
				FanStaticPressureRise = ExternalStatic + DXCoil( DXCoilNum ).InternalStaticPressureDrop;

				FanInletNode = GetFanInletNode( "FAN:VARIABLEVOLUME", DXCoil( DXCoilNum ).SupplyFanName, ErrorsFound );
				FanOutletNode = GetFanOutletNode( "FAN:VARIABLEVOLUME", DXCoil( DXCoilNum ).SupplyFanName, ErrorsFound );
				// set node state variables in preparation for fan model.
				Node( FanInletNode ).MassFlowRate = DXCoil( DXCoilNum ).RatedAirMassFlowRate( 1 );
				Node( FanOutletNode ).MassFlowRate = DXCoil( DXCoilNum ).RatedAirMassFlowRate( 1 );
				Node( FanInletNode ).Temp = CoolingCoilInletAirDryBulbTempRated;
				Node( FanInletNode ).HumRat = PsyWFnTdbTwbPb( CoolingCoilInletAirDryBulbTempRated, CoolingCoilInletAirWetBulbTempRated, OutBaroPress, RoutineName );
				Node( FanInletNode ).Enthalpy = PsyHFnTdbW( CoolingCoilInletAirDryBulbTempRated, Node( FanInletNode ).HumRat );

				SimulateFanComponents( DXCoil( DXCoilNum ).SupplyFanName, true, DXCoil( DXCoilNum ).SupplyFanIndex, _, true, false, FanStaticPressureRise );
				FanHeatCorrection = Node( FanOutletNode ).Enthalpy - Node( FanInletNode ).Enthalpy;
				GetFanPower( DXCoil( DXCoilNum ).SupplyFanIndex, FanPowerCorrection );

				NetCoolingCapRated = DXCoil( DXCoilNum ).RatedTotCap( 1 ) * TotCapTempModFac * TotCapFlowModFac - FanHeatCorrection;
			}

		} else {
			FanPowerPerEvapAirFlowRate = DefaultFanPowerPerEvapAirFlowRate;
			FanPowerCorrection = DefaultFanPowerPerEvapAirFlowRate * DXCoil( DXCoilNum ).RatedAirVolFlowRate( 1 );
			FanHeatCorrection = DefaultFanPowerPerEvapAirFlowRate * DXCoil( DXCoilNum ).RatedAirVolFlowRate( 1 );
			TotCapFlowModFac = CurveValue( DXCoil( DXCoilNum ).CCapFFlow( 1 ), AirMassFlowRatioRated );
			TotCapTempModFac = CurveValue( DXCoil( DXCoilNum ).CCapFTemp( 1 ), CoolingCoilInletAirWetBulbTempRated, OutdoorUnitInletAirDryBulbTempRated );
			NetCoolingCapRated = DXCoil( DXCoilNum ).RatedTotCap( 1 ) * TotCapTempModFac * TotCapFlowModFac - FanHeatCorrection;
		}

		SupAirMdot_TestPoint( 1 ) = DXCoil( DXCoilNum ).RatedAirMassFlowRate( 1 );

		// Calculate Energy Efficiency Ratio (EER) at (19.44C WB and 35.0C DB ), ANSI/AHRI Std. 340/360
		EIRTempModFac = CurveValue( DXCoil( DXCoilNum ).EIRFTemp( 1 ), CoolingCoilInletAirWetBulbTempRated, OutdoorUnitInletAirDryBulbTempRated );
		EIRFlowModFac = CurveValue( DXCoil( DXCoilNum ).EIRFFlow( 1 ), AirMassFlowRatioRated );
		if ( DXCoil( DXCoilNum ).RatedCOP( 1 ) > 0.0 ) {
			// RatedCOP <= 0.0 is trapped in GetInput, but keep this as "safety"
			EIR = EIRTempModFac * EIRFlowModFac / DXCoil( DXCoilNum ).RatedCOP( 1 );
		} else {
			EIR = 0.0;
		}
		TotalElecPowerRated = EIR * ( DXCoil( DXCoilNum ).RatedTotCap( 1 ) * TotCapTempModFac * TotCapFlowModFac ) + FanPowerCorrection;

		if ( TotalElecPowerRated > 0.0 ) {
			EER = NetCoolingCapRated / TotalElecPowerRated;
		} else {
			EER = 0.0;
		}

		// IEER - A point 100 % net capacity
		EER_TestPoint_SI( 1 ) = EER;
		EER_TestPoint_IP( 1 ) = EER * ConvFromSIToIP;

		// find coil leaving drybulb at point A, with full rated air flow rate.
		// init coil
		DXCoil( DXCoilNum ).InletAirMassFlowRate = DXCoil( DXCoilNum ).RatedAirMassFlowRate( 1 );
		DXCoil( DXCoilNum ).InletAirMassFlowRateMax = DXCoil( DXCoilNum ).RatedAirMassFlowRate( 1 );
		DXCoil( DXCoilNum ).InletAirTemp = 26.7;
		DXCoil( DXCoilNum ).InletAirHumRat = PsyWFnTdbTwbPb( 26.7, 19.4, OutBaroPress, RoutineName );
		DXCoil( DXCoilNum ).InletAirEnthalpy = PsyHFnTdbW( 26.7, DXCoil( DXCoilNum ).InletAirHumRat );

		if ( DXCoil( DXCoilNum ).CondenserInletNodeNum( 1 ) != 0 ) {
			Node( DXCoil( DXCoilNum ).CondenserInletNodeNum( 1 ) ).Temp = OutdoorUnitInletAirDryBulbTempRated;
		} else {
			OutDryBulbTemp = OutdoorUnitInletAirDryBulbTempRated;
		}
		SpeedRatio = 1.0;
		CycRatio = 1.0;
		CalcMultiSpeedDXCoil( DXCoilNum, SpeedRatio, CycRatio, true );
		TempDryBulb_Leaving_Apoint = DXCoilOutletTemp( DXCoilNum ); // store result

		// IEER - part load test points ***************************************************
		for ( PartLoadTestPoint = 1; PartLoadTestPoint <= 3; ++PartLoadTestPoint ) {
			// determine minimum unloading capacity fraction at point B conditions.
			if ( DXCoil( DXCoilNum ).CondenserInletNodeNum( 1 ) != 0 ) {
				Node( DXCoil( DXCoilNum ).CondenserInletNodeNum( 1 ) ).Temp = OutdoorUnitInletAirDryBulbTempPLTestPoint( PartLoadTestPoint );
			} else {
				OutDryBulbTemp = OutdoorUnitInletAirDryBulbTempPLTestPoint( PartLoadTestPoint );
			}

			TargetNetCapacity = NetCapacityFactorPLTestPoint( PartLoadTestPoint ) * NetCoolingCapRated;
			Par( 1 ) = double( DXCoilNum );
			Par( 2 ) = TempDryBulb_Leaving_Apoint;
			Par( 3 ) = TargetNetCapacity;
			Par( 4 ) = OutdoorUnitInletAirDryBulbTempPLTestPoint( PartLoadTestPoint );
			Par( 5 ) = CoolingCoilInletAirWetBulbTempRated;
			Par( 6 ) = CoolingCoilInletAirDryBulbTempRated;
			Par( 7 ) = NetCoolingCapRated;
			if ( DXCoil( DXCoilNum ).RateWithInternalStaticAndFanObject ) {
				Par( 8 ) = 0.0;
				Par( 9 ) = double( FanInletNode );
				Par( 10 ) = double( FanOutletNode );
				Par( 11 ) = ExternalStatic;
				Par( 12 ) = double( DXCoil( DXCoilNum ).SupplyFanIndex );
			} else {
				Par( 8 ) = FanPowerPerEvapAirFlowRate;
				Par( 9 ) = 0.0;
				Par( 10 ) = 0.0;
				Par( 11 ) = 0.0;
				Par( 12 ) = 0.0;
			}

			LowerBoundMassFlowRate = 0.01 * DXCoil( DXCoilNum ).RatedAirMassFlowRate( 1 );

			SolveRegulaFalsi( AccuracyTolerance, MaximumIterations, SolverFlag, PartLoadAirMassFlowRate, CalcTwoSpeedDXCoilIEERResidual, LowerBoundMassFlowRate, DXCoil( DXCoilNum ).RatedAirMassFlowRate( 1 ), Par );

			if ( SolverFlag == -1 ) {

				ShowWarningError( "CalcTwoSpeedDXCoilStandardRating: air flow rate solver failed. Iteration limit exceeded " );

				SupAirMdot_TestPoint( 1 + PartLoadTestPoint ) = -999.0;
				EER_TestPoint_SI( 1 + PartLoadTestPoint ) = -999.0;
				EER_TestPoint_IP( 1 + PartLoadTestPoint ) = -999.0;
				NetCapacity_TestPoint( 1 + PartLoadTestPoint ) = -999.0;
				NetPower_TestPoint( 1 + PartLoadTestPoint ) = -999.0;

			} else if ( SolverFlag == -2 ) {
				ShowWarningError( "CalcTwoSpeedDXCoilStandardRating: air flow rate solver failed. root not bounded " );

				SupAirMdot_TestPoint( 1 + PartLoadTestPoint ) = -999.0;
				EER_TestPoint_SI( 1 + PartLoadTestPoint ) = -999.0;
				EER_TestPoint_IP( 1 + PartLoadTestPoint ) = -999.0;
				NetCapacity_TestPoint( 1 + PartLoadTestPoint ) = -999.0;
				NetPower_TestPoint( 1 + PartLoadTestPoint ) = -999.0;
			} else {
				// now we have the supply air flow rate
				SupAirMdot_TestPoint( 1 + PartLoadTestPoint ) = PartLoadAirMassFlowRate;
				AirMassFlowRatio = PartLoadAirMassFlowRate / DXCoil( DXCoilNum ).RatedAirMassFlowRate( 1 );
				SupplyAirHumRat = PsyWFnTdbTwbPb( CoolingCoilInletAirDryBulbTempRated, CoolingCoilInletAirWetBulbTempRated, OutBaroPress, RoutineName );
				SupplyAirRho = PsyRhoAirFnPbTdbW( OutBaroPress, CoolingCoilInletAirDryBulbTempRated, SupplyAirHumRat, RoutineName );
				SupplyAirVolFlowRate = PartLoadAirMassFlowRate / SupplyAirRho;

				if ( DXCoil( DXCoilNum ).RateWithInternalStaticAndFanObject ) {
					FanStaticPressureRise = DXCoil( DXCoilNum ).InternalStaticPressureDrop + ( ExternalStatic * pow_2( AirMassFlowRatio ) );
					Node( FanInletNode ).MassFlowRate = PartLoadAirMassFlowRate;
					Node( FanInletNode ).Temp = CoolingCoilInletAirDryBulbTempRated;
					Node( FanInletNode ).HumRat = SupplyAirHumRat;
					Node( FanInletNode ).Enthalpy = PsyHFnTdbW( CoolingCoilInletAirDryBulbTempRated, SupplyAirHumRat );
					SimulateFanComponents( DXCoil( DXCoilNum ).SupplyFanName, true, DXCoil( DXCoilNum ).SupplyFanIndex, _, true, false, FanStaticPressureRise );
					FanHeatCorrection = Node( FanOutletNode ).Enthalpy - Node( FanInletNode ).Enthalpy;
					GetFanPower( DXCoil( DXCoilNum ).SupplyFanIndex, FanPowerCorrection );

				} else {
					FanPowerCorrection = FanPowerPerEvapAirFlowRate * PartLoadAirMassFlowRate;
					FanHeatCorrection = FanPowerPerEvapAirFlowRate * PartLoadAirMassFlowRate;
				}

				TotCapFlowModFac = CurveValue( DXCoil( DXCoilNum ).CCapFFlow( 1 ), AirMassFlowRatio );
				TotCapTempModFac = CurveValue( DXCoil( DXCoilNum ).CCapFTemp( 1 ), CoolingCoilInletAirWetBulbTempRated, OutdoorUnitInletAirDryBulbTempPLTestPoint( PartLoadTestPoint ) );
				HighSpeedTotCoolingCap = DXCoil( DXCoilNum ).RatedTotCap( 1 ) * TotCapTempModFac * TotCapFlowModFac;
				HighSpeedNetCoolingCap = HighSpeedTotCoolingCap - FanHeatCorrection;

				EIRTempModFac = CurveValue( DXCoil( DXCoilNum ).EIRFTemp( 1 ), CoolingCoilInletAirWetBulbTempRated, OutdoorUnitInletAirDryBulbTempPLTestPoint( PartLoadTestPoint ) );
				EIRFlowModFac = CurveValue( DXCoil( DXCoilNum ).EIRFFlow( 1 ), AirMassFlowRatio );
				if ( DXCoil( DXCoilNum ).RatedCOP( 1 ) > 0.0 ) {
					// RatedCOP <= 0.0 is trapped in GetInput, but keep this as "safety"
					EIR_HighSpeed = EIRTempModFac * EIRFlowModFac / DXCoil( DXCoilNum ).RatedCOP( 1 );
				} else {
					EIR = 0.0;
				}

				TotCapFlowModFac = CurveValue( DXCoil( DXCoilNum ).CCapFTemp2, AirMassFlowRatio );
				TotCapTempModFac = CurveValue( DXCoil( DXCoilNum ).CCapFTemp2, CoolingCoilInletAirWetBulbTempRated, OutdoorUnitInletAirDryBulbTempPLTestPoint( PartLoadTestPoint ) );
				LowSpeedTotCoolingCap = DXCoil( DXCoilNum ).RatedTotCap2 * TotCapTempModFac * TotCapFlowModFac;
				LowSpeedNetCoolingCap = LowSpeedTotCoolingCap - FanHeatCorrection;

				EIRTempModFac = CurveValue( DXCoil( DXCoilNum ).EIRFTemp2, CoolingCoilInletAirWetBulbTempRated, OutdoorUnitInletAirDryBulbTempPLTestPoint( PartLoadTestPoint ) );
				EIRFlowModFac = CurveValue( DXCoil( DXCoilNum ).EIRFFlow( 1 ), AirMassFlowRatio );
				if ( DXCoil( DXCoilNum ).RatedCOP2 > 0.0 ) {
					// RatedCOP <= 0.0 is trapped in GetInput, but keep this as "safety"
					EIR_LowSpeed = EIRTempModFac * EIRFlowModFac / DXCoil( DXCoilNum ).RatedCOP2;
				} else {
					EIR_LowSpeed = 0.0;
				}

				if ( LowSpeedNetCoolingCap <= TargetNetCapacity ) {
					CycRatio = 1.0;
					SpeedRatio = ( TargetNetCapacity - LowSpeedNetCoolingCap ) / ( HighSpeedNetCoolingCap - LowSpeedNetCoolingCap );
					TotCoolingCap = HighSpeedTotCoolingCap * SpeedRatio + LowSpeedTotCoolingCap * ( 1.0 - SpeedRatio );
					NetCoolingCap = TotCoolingCap - FanHeatCorrection;
					EIR = EIR_HighSpeed * SpeedRatio + EIR_LowSpeed * ( 1.0 - SpeedRatio );
					TotalElecPowerRated = TotCoolingCap * EIR + FanPowerCorrection;
					EER_TestPoint_SI( 1 + PartLoadTestPoint ) = NetCoolingCap / TotalElecPowerRated;
					EER_TestPoint_IP( 1 + PartLoadTestPoint ) = EER_TestPoint_SI( 1 + PartLoadTestPoint ) * ConvFromSIToIP;
					NetCapacity_TestPoint( 1 + PartLoadTestPoint ) = NetCoolingCap;
					NetPower_TestPoint( 1 + PartLoadTestPoint ) = TotalElecPowerRated;
				} else { // minimum unloading limit exceeded without cycling, so cycle
					SpeedRatio = 0.0;
					CycRatio = TargetNetCapacity / LowSpeedNetCoolingCap;
					PLF = CurveValue( DXCoil( DXCoilNum ).PLFFPLR( 1 ), CycRatio );
					if ( PLF < 0.7 ) {
						PLF = 0.7;
					}
					RunTimeFraction = CycRatio / PLF;
					RunTimeFraction = min( RunTimeFraction, 1.0 );
					TotCoolingCap = LowSpeedTotCoolingCap * RunTimeFraction;
					NetCoolingCap = TotCoolingCap - FanHeatCorrection;
					TotalElecPowerRated = LowSpeedTotCoolingCap * EIR_LowSpeed * RunTimeFraction + FanPowerCorrection;
					EER_TestPoint_SI( 1 + PartLoadTestPoint ) = NetCoolingCap / TotalElecPowerRated;
					EER_TestPoint_IP( 1 + PartLoadTestPoint ) = EER_TestPoint_SI( 1 + PartLoadTestPoint ) * ConvFromSIToIP;
					NetCapacity_TestPoint( 1 + PartLoadTestPoint ) = NetCoolingCap;
					NetPower_TestPoint( 1 + PartLoadTestPoint ) = TotalElecPowerRated;

				}

			}
		} // loop over 3 part load test points

		IEER = ( 0.02 * EER_TestPoint_IP( 1 ) ) + ( 0.617 * EER_TestPoint_IP( 2 ) ) + ( 0.238 * EER_TestPoint_IP( 3 ) ) + ( 0.125 * EER_TestPoint_IP( 4 ) );

		// begin output
		if ( OneTimeEIOHeaderWrite ) {
			gio::write( OutputFileInits, Format_890 );
			OneTimeEIOHeaderWrite = false;
			pdstVAVDXCoolCoil = newPreDefSubTable( pdrEquip, "VAV DX Cooling Standard Rating Details" );
			pdchVAVDXCoolCoilType = newPreDefColumn( pdstVAVDXCoolCoil, "DX Cooling Coil Type" );
			pdchVAVDXFanName = newPreDefColumn( pdstVAVDXCoolCoil, "Assocated Fan" );
			pdchVAVDXCoolCoilNetCapSI = newPreDefColumn( pdstVAVDXCoolCoil, "Net Cooling Capacity [W]" );
			pdchVAVDXCoolCoilCOP = newPreDefColumn( pdstVAVDXCoolCoil, "COP [W/W]" );
			pdchVAVDXCoolCoilEERIP = newPreDefColumn( pdstVAVDXCoolCoil, "EER [Btu/W-h]" );
			pdchVAVDXCoolCoilIEERIP = newPreDefColumn( pdstVAVDXCoolCoil, "IEER [Btu/W-h]" );
			pdchVAVDXCoolCoilMdotA = newPreDefColumn( pdstVAVDXCoolCoil, "Supply Air Flow 100% [kg/s]" );
			pdchVAVDXCoolCoilCOP_B = newPreDefColumn( pdstVAVDXCoolCoil, "COP 75% Capacity [W/W]" );
			pdchVAVDXCoolCoilEER_B_IP = newPreDefColumn( pdstVAVDXCoolCoil, "EER 75% Capacity [Btu/W-h]" );
			pdchVAVDXCoolCoilMdotB = newPreDefColumn( pdstVAVDXCoolCoil, "Supply Air Flow 75% [kg/s]" );
			pdchVAVDXCoolCoilCOP_C = newPreDefColumn( pdstVAVDXCoolCoil, "COP 50% Capacity [W/W]" );
			pdchVAVDXCoolCoilEER_C_IP = newPreDefColumn( pdstVAVDXCoolCoil, "EER 50% Capacity [Btu/W-h]" );
			pdchVAVDXCoolCoilMdotC = newPreDefColumn( pdstVAVDXCoolCoil, "Supply Air Flow 50% [kg/s]" );
			pdchVAVDXCoolCoilCOP_D = newPreDefColumn( pdstVAVDXCoolCoil, "COP 25% Capacity [W/W]" );
			pdchVAVDXCoolCoilEER_D_IP = newPreDefColumn( pdstVAVDXCoolCoil, "EER 25% Capacity [Btu/W-h]" );
			pdchVAVDXCoolCoilMdotD = newPreDefColumn( pdstVAVDXCoolCoil, "Supply Air Flow 25% [kg/s]" );

			// determine footnote content
			countStaticInputs = 0;
			for ( index = 1; index <= NumDXCoils; ++index ) {

				if ( DXCoil( index ).RateWithInternalStaticAndFanObject && DXCoil( index ).DXCoilType_Num == CoilDX_CoolingTwoSpeed ) {
					++countStaticInputs;
				}
			}

			if ( countStaticInputs == NumDXMulSpeedCoils ) {
				addFootNoteSubTable( pdstVAVDXCoolCoil, "Packaged VAV unit ratings per ANSI/AHRI Standard 340/360-2007 with Addenda 1 and 2" );
			} else if ( countStaticInputs == 0 ) {
				addFootNoteSubTable( pdstVAVDXCoolCoil, "Indoor-coil-only unit ratings per ANSI/AHRI Standard 340/360-2007 with Addenda 1 and 2, with supply fan specific power at 365 {W/1000cfm} (773.3 {W/(m3/s)})" );
			} else { // both
				addFootNoteSubTable( pdstVAVDXCoolCoil, "Packaged VAV unit ratings per ANSI/AHRI Standard 340/360-2007 with Addenda 1 and 2, indoor-coil-only units with supply fan specific power at 365 {W/1000cfm} (773.3 {W/(m3/s)})" );
			}

		}

		if ( DXCoil( DXCoilNum ).RateWithInternalStaticAndFanObject ) {
			gio::write( OutputFileInits, Format_891 ) << "Coil:Cooling:DX:TwoSpeed" << DXCoil( DXCoilNum ).Name << "Fan:VariableVolume" << DXCoil( DXCoilNum ).SupplyFanName << RoundSigDigits( NetCoolingCapRated, 2 ) << RoundSigDigits( ( NetCoolingCapRated * ConvFromSIToIP ), 2 ) << RoundSigDigits( IEER, 2 ) << RoundSigDigits( EER_TestPoint_SI( 1 ), 2 ) << RoundSigDigits( EER_TestPoint_SI( 2 ), 2 ) << RoundSigDigits( EER_TestPoint_SI( 3 ), 2 ) << RoundSigDigits( EER_TestPoint_SI( 4 ), 2 ) << RoundSigDigits( EER_TestPoint_IP( 1 ), 2 ) << RoundSigDigits( EER_TestPoint_IP( 2 ), 2 ) << RoundSigDigits( EER_TestPoint_IP( 3 ), 2 ) << RoundSigDigits( EER_TestPoint_IP( 4 ), 2 ) << RoundSigDigits( SupAirMdot_TestPoint( 1 ), 4 ) << RoundSigDigits( SupAirMdot_TestPoint( 2 ), 4 ) << RoundSigDigits( SupAirMdot_TestPoint( 3 ), 4 ) << RoundSigDigits( SupAirMdot_TestPoint( 4 ), 4 );
		} else {
			gio::write( OutputFileInits, Format_891 ) << "Coil:Cooling:DX:TwoSpeed" << DXCoil( DXCoilNum ).Name << "N/A" << "N/A" << RoundSigDigits( NetCoolingCapRated, 2 ) << RoundSigDigits( ( NetCoolingCapRated * ConvFromSIToIP ), 2 ) << RoundSigDigits( IEER, 2 ) << RoundSigDigits( EER_TestPoint_SI( 1 ), 2 ) << RoundSigDigits( EER_TestPoint_SI( 2 ), 2 ) << RoundSigDigits( EER_TestPoint_SI( 3 ), 2 ) << RoundSigDigits( EER_TestPoint_SI( 4 ), 2 ) << RoundSigDigits( EER_TestPoint_IP( 1 ), 2 ) << RoundSigDigits( EER_TestPoint_IP( 2 ), 2 ) << RoundSigDigits( EER_TestPoint_IP( 3 ), 2 ) << RoundSigDigits( EER_TestPoint_IP( 4 ), 2 ) << RoundSigDigits( SupAirMdot_TestPoint( 1 ), 4 ) << RoundSigDigits( SupAirMdot_TestPoint( 2 ), 4 ) << RoundSigDigits( SupAirMdot_TestPoint( 3 ), 4 ) << RoundSigDigits( SupAirMdot_TestPoint( 4 ), 4 );
		}

		PreDefTableEntry( pdchDXCoolCoilType, DXCoil( DXCoilNum ).Name, "Coil:Cooling:DX:TwoSpeed" );
		PreDefTableEntry( pdchDXCoolCoilNetCapSI, DXCoil( DXCoilNum ).Name, RoundSigDigits( NetCoolingCapRated, 1 ) );
		PreDefTableEntry( pdchDXCoolCoilCOP, DXCoil( DXCoilNum ).Name, RoundSigDigits( EER_TestPoint_SI( 1 ), 2 ) );
		PreDefTableEntry( pdchDXCoolCoilEERIP, DXCoil( DXCoilNum ).Name, RoundSigDigits( EER_TestPoint_IP( 1 ), 2 ) );
		PreDefTableEntry( pdchDXCoolCoilIEERIP, DXCoil( DXCoilNum ).Name, RoundSigDigits( IEER, 2 ) );
		PreDefTableEntry( pdchDXCoolCoilSEERIP, DXCoil( DXCoilNum ).Name, "N/A" );
		addFootNoteSubTable( pdstDXCoolCoil, "ANSI/AHRI ratings include supply fan" );

		PreDefTableEntry( pdchVAVDXCoolCoilType, DXCoil( DXCoilNum ).Name, "Coil:Cooling:DX:TwoSpeed" );
		if ( DXCoil( DXCoilNum ).RateWithInternalStaticAndFanObject ) {
			PreDefTableEntry( pdchVAVDXFanName, DXCoil( DXCoilNum ).Name, DXCoil( DXCoilNum ).SupplyFanName );
		} else {
			PreDefTableEntry( pdchVAVDXFanName, DXCoil( DXCoilNum ).Name, "None" );
		}
		PreDefTableEntry( pdchVAVDXCoolCoilNetCapSI, DXCoil( DXCoilNum ).Name, RoundSigDigits( NetCoolingCapRated, 2 ) );
		PreDefTableEntry( pdchVAVDXCoolCoilCOP, DXCoil( DXCoilNum ).Name, RoundSigDigits( EER_TestPoint_SI( 1 ), 2 ) );
		PreDefTableEntry( pdchVAVDXCoolCoilIEERIP, DXCoil( DXCoilNum ).Name, RoundSigDigits( IEER, 2 ) );
		PreDefTableEntry( pdchVAVDXCoolCoilEERIP, DXCoil( DXCoilNum ).Name, RoundSigDigits( EER_TestPoint_IP( 1 ), 2 ) );
		PreDefTableEntry( pdchVAVDXCoolCoilMdotA, DXCoil( DXCoilNum ).Name, RoundSigDigits( SupAirMdot_TestPoint( 1 ), 4 ) );
		PreDefTableEntry( pdchVAVDXCoolCoilCOP_B, DXCoil( DXCoilNum ).Name, RoundSigDigits( EER_TestPoint_SI( 2 ), 2 ) );
		PreDefTableEntry( pdchVAVDXCoolCoilEER_B_IP, DXCoil( DXCoilNum ).Name, RoundSigDigits( EER_TestPoint_IP( 2 ), 2 ) );
		PreDefTableEntry( pdchVAVDXCoolCoilMdotB, DXCoil( DXCoilNum ).Name, RoundSigDigits( SupAirMdot_TestPoint( 2 ), 4 ) );
		PreDefTableEntry( pdchVAVDXCoolCoilCOP_C, DXCoil( DXCoilNum ).Name, RoundSigDigits( EER_TestPoint_SI( 3 ), 2 ) );
		PreDefTableEntry( pdchVAVDXCoolCoilEER_C_IP, DXCoil( DXCoilNum ).Name, RoundSigDigits( EER_TestPoint_IP( 3 ), 2 ) );
		PreDefTableEntry( pdchVAVDXCoolCoilMdotC, DXCoil( DXCoilNum ).Name, RoundSigDigits( SupAirMdot_TestPoint( 3 ), 4 ) );
		PreDefTableEntry( pdchVAVDXCoolCoilCOP_D, DXCoil( DXCoilNum ).Name, RoundSigDigits( EER_TestPoint_SI( 4 ), 2 ) );
		PreDefTableEntry( pdchVAVDXCoolCoilEER_D_IP, DXCoil( DXCoilNum ).Name, RoundSigDigits( EER_TestPoint_IP( 4 ), 2 ) );
		PreDefTableEntry( pdchVAVDXCoolCoilMdotD, DXCoil( DXCoilNum ).Name, RoundSigDigits( SupAirMdot_TestPoint( 4 ), 4 ) );

	}

	void
	GetFanIndexForTwoSpeedCoil(
		int const CoolingCoilIndex,
		int & SupplyFanIndex,
		std::string & SupplyFanName
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         <author>
		//       DATE WRITTEN   <date_written>
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine looks up the given TwoSpeed DX coil and returns the companion supply fan index

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using InputProcessor::SameString;
		using DataAirSystems::PrimaryAirSystem;
		using Fans::GetFanIndex;

		using DataHVACGlobals::NumPrimaryAirSys;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// SUBROUTINE PARAMETER DEFINITIONS:
		int const DXSystem( 14 ); // must match SimAirServingZones.cc (not public)
		int const Fan_Simple_VAV( 3 ); // must match SimAirServingZones.cc (not public)
		int const UnitarySystem( 19 ); // must match SimAirServingZones.cc (not public)

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int FoundBranch;
		int FoundAirSysNum;
		int AirSysNum;
		int BranchNum;
		int CompNum;
		static bool ErrorsFound( false );

		FoundBranch = 0;
		FoundAirSysNum = 0;
		SupplyFanIndex = 0;
		SupplyFanName = "n/a";
		for ( AirSysNum = 1; AirSysNum <= NumPrimaryAirSys; ++AirSysNum ) {

			for ( BranchNum = 1; BranchNum <= PrimaryAirSystem( AirSysNum ).NumBranches; ++BranchNum ) {

				for ( CompNum = 1; CompNum <= PrimaryAirSystem( AirSysNum ).Branch( BranchNum ).TotalComponents; ++CompNum ) {

					if ( PrimaryAirSystem( AirSysNum ).Branch( BranchNum ).Comp( CompNum ).CompType_Num == DXSystem ) {

						if ( SameString( PrimaryAirSystem( AirSysNum ).Branch( BranchNum ).Comp( CompNum ).Name, DXCoil( CoolingCoilIndex ).CoilSystemName ) ) {
							FoundBranch = BranchNum;
							FoundAirSysNum = AirSysNum;
							break;
						}
						// these are specified in SimAirServingZones and need to be moved to a Data* file. UnitarySystem=19
					} else if ( PrimaryAirSystem( AirSysNum ).Branch( BranchNum ).Comp( CompNum ).CompType_Num == UnitarySystem ) {
						FoundBranch = BranchNum;
						FoundAirSysNum = AirSysNum;
						break;
					}
				}

				if ( FoundBranch > 0 && FoundAirSysNum > 0 ) {
					for ( CompNum = 1; CompNum <= PrimaryAirSystem( FoundAirSysNum ).Branch( FoundBranch ).TotalComponents; ++CompNum ) {
						if ( PrimaryAirSystem( FoundAirSysNum ).Branch( FoundBranch ).Comp( CompNum ).CompType_Num == Fan_Simple_VAV ) {
							SupplyFanName = PrimaryAirSystem( FoundAirSysNum ).Branch( FoundBranch ).Comp( CompNum ).Name;
							GetFanIndex( SupplyFanName, SupplyFanIndex, ErrorsFound );
							break;
							// these are specified in SimAirServingZones and need to be moved to a Data* file. UnitarySystem=19
						} else if ( PrimaryAirSystem( FoundAirSysNum ).Branch( FoundBranch ).Comp( CompNum ).CompType_Num == UnitarySystem ) {
							// fan may not be specified in a unitary system object, keep looking
							// Unitary System will "set" the fan index to the DX coil if contained within the HVAC system
							if ( DXCoil( CoolingCoilIndex ).SupplyFanIndex > 0 ) break;
						}
					}
				}
			}
		}

	}

	Real64
	CalcTwoSpeedDXCoilIEERResidual(
		Real64 const SupplyAirMassFlowRate, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   July 2012
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (desired outlet temp - actual outlet temp)
		// Two Speed DX Coil rating for VAV, output depends on the supply air flow rate which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Calls CalcMultiSpeedDXCoil to get outlet temperature at the given supply flow rate and SpeedRatio
		// and calculates the residual as defined above

		// REFERENCES:

		// Using/Aliasing
		using DataEnvironment::OutBaroPress;
		using Fans::SimulateFanComponents;
		using CurveManager::CurveValue;

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// par(2) = desired air outlet temperature [C]
		// par(3) = speed ratio
		// par(4) = cycling Ratio
		// par(5) = supply air fan operating mode (ContFanCycCoil)

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		static std::string const RoutineName( "CalcTwoSpeedDXCoilIEERResidual" );
		int DXCoilNum; // index of this coil
		Real64 OutletAirTemp; // outlet air temperature [C]
		Real64 TargetCoilLeavingDryBulb;
		Real64 OutdoorUnitInletDryBulb;
		Real64 IndoorUnitInletDryBulb;
		Real64 IndoorUnitInletWetBulb;
		Real64 AirMassFlowRatio;
		Real64 SpeedRatio;
		Real64 CycRatio;
		Real64 SupplyAirRho;
		Real64 SupplyAirHumRat;
		Real64 NetCoolingCapRated;
		Real64 TargetNetCapacity;
		Real64 FanPowerPerEvapAirFlowRate;
		int FanInletNodeNum;
		int FanOutletNodeNum;
		int FanIndex;
		Real64 FanExternalStaticFull;
		Real64 SupplyAirVolFlowRate;
		Real64 FanStaticPressureRise;
		Real64 FanHeatCorrection;
		Real64 TotCapFlowModFac;
		Real64 TotCapTempModFac;
		Real64 HighSpeedNetCoolingCap;
		Real64 LowSpeedNetCoolingCap;

		DXCoilNum = int( Par( 1 ) );
		TargetCoilLeavingDryBulb = Par( 2 );
		TargetNetCapacity = Par( 3 );
		OutdoorUnitInletDryBulb = Par( 4 );
		IndoorUnitInletWetBulb = Par( 5 );
		IndoorUnitInletDryBulb = Par( 6 );
		NetCoolingCapRated = Par( 7 );
		FanPowerPerEvapAirFlowRate = Par( 8 );
		FanInletNodeNum = int( Par( 9 ) );
		FanOutletNodeNum = int( Par( 10 ) );
		FanExternalStaticFull = Par( 11 );
		FanIndex = int( Par( 12 ) );

		if ( DXCoil( DXCoilNum ).RatedAirMassFlowRate( 1 ) > 0.0 ) {
			AirMassFlowRatio = SupplyAirMassFlowRate / DXCoil( DXCoilNum ).RatedAirMassFlowRate( 1 );
		} else {
			AirMassFlowRatio = 0.0;
		}
		SupplyAirHumRat = PsyWFnTdbTwbPb( IndoorUnitInletDryBulb, IndoorUnitInletWetBulb, OutBaroPress, RoutineName );
		SupplyAirRho = PsyRhoAirFnPbTdbW( OutBaroPress, IndoorUnitInletDryBulb, SupplyAirHumRat, RoutineName );

		SupplyAirVolFlowRate = SupplyAirMassFlowRate / SupplyAirRho;

		if ( DXCoil( DXCoilNum ).RateWithInternalStaticAndFanObject ) {
			// modify external static per AHRI 340/360, Table 6, note 1.
			FanStaticPressureRise = DXCoil( DXCoilNum ).InternalStaticPressureDrop + ( FanExternalStaticFull * pow_2( AirMassFlowRatio ) );
			Node( FanInletNodeNum ).MassFlowRate = SupplyAirMassFlowRate;
			Node( FanOutletNodeNum ).MassFlowRate = SupplyAirMassFlowRate;
			Node( FanInletNodeNum ).Temp = IndoorUnitInletDryBulb;
			Node( FanInletNodeNum ).HumRat = PsyWFnTdbTwbPb( IndoorUnitInletDryBulb, IndoorUnitInletWetBulb, OutBaroPress, RoutineName );
			Node( FanInletNodeNum ).Enthalpy = PsyHFnTdbW( IndoorUnitInletDryBulb, Node( FanInletNodeNum ).HumRat );
			SimulateFanComponents( DXCoil( DXCoilNum ).SupplyFanName, true, DXCoil( DXCoilNum ).SupplyFanIndex, _, true, false, FanStaticPressureRise );
			FanHeatCorrection = Node( FanOutletNodeNum ).Enthalpy - Node( FanInletNodeNum ).Enthalpy;

		} else {

			FanHeatCorrection = FanPowerPerEvapAirFlowRate * SupplyAirVolFlowRate;

		}

		TotCapFlowModFac = CurveValue( DXCoil( DXCoilNum ).CCapFFlow( 1 ), AirMassFlowRatio );
		TotCapTempModFac = CurveValue( DXCoil( DXCoilNum ).CCapFTemp( 1 ), IndoorUnitInletWetBulb, OutdoorUnitInletDryBulb );
		HighSpeedNetCoolingCap = DXCoil( DXCoilNum ).RatedTotCap( 1 ) * TotCapTempModFac * TotCapFlowModFac - FanHeatCorrection;

		TotCapFlowModFac = CurveValue( DXCoil( DXCoilNum ).CCapFFlow( 1 ), AirMassFlowRatio );
		TotCapTempModFac = CurveValue( DXCoil( DXCoilNum ).CCapFTemp2, IndoorUnitInletWetBulb, OutdoorUnitInletDryBulb );
		LowSpeedNetCoolingCap = DXCoil( DXCoilNum ).RatedTotCap2 * TotCapTempModFac * TotCapFlowModFac - FanHeatCorrection;

		if ( LowSpeedNetCoolingCap <= TargetNetCapacity ) {
			CycRatio = 1.0;
			SpeedRatio = ( TargetNetCapacity - LowSpeedNetCoolingCap ) / ( HighSpeedNetCoolingCap - LowSpeedNetCoolingCap );
		} else { // minimum unloading limit exceeded for no cycling
			SpeedRatio = 0.0;
			CycRatio = TargetNetCapacity / LowSpeedNetCoolingCap;
		}

		DXCoil( DXCoilNum ).InletAirMassFlowRate = SupplyAirMassFlowRate;

		CalcMultiSpeedDXCoil( DXCoilNum, SpeedRatio, CycRatio, true );
		OutletAirTemp = DXCoilOutletTemp( DXCoilNum );
		Residuum = TargetCoilLeavingDryBulb - OutletAirTemp;

		return Residuum;
	}

	// ======================  Utility routines ======================================

	void
	GetDXCoilIndex(
		std::string const & DXCoilName,
		int & DXCoilIndex,
		bool & ErrorsFound,
		Optional_string_const ThisObjectType,
		Optional_bool_const SuppressWarning
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   March 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine sets an index for a given DX Coil -- issues error message if that
		// DX Coil is not a legal DX Coil.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na
		if ( GetCoilsInputFlag ) {
			GetDXCoils();
			GetCoilsInputFlag = false;
		}

		DXCoilIndex = FindItemInList( DXCoilName, DXCoil );
		if ( DXCoilIndex == 0 ) {
			if ( present( SuppressWarning ) ) {
				//     No warning printed if only searching for the existence of a DX Coil
			} else {
				if ( present( ThisObjectType ) ) {
					ShowSevereError( ThisObjectType + ", GetDXCoilIndex: DX Coil not found=" + DXCoilName );
				} else {
					ShowSevereError( "GetDXCoilIndex: DX Coil not found=" + DXCoilName );
				}
			}
			ErrorsFound = true;
		}

	}

	Real64
	GetCoilCapacity(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   February 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the coil capacity for the given coil and returns it.  If
		// incorrect coil type or name is given, ErrorsFound is returned as true and capacity is returned
		// as negative.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItem;
		using InputProcessor::SameString;

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

		// Obtains and Allocates DXCoils
		if ( GetCoilsInputFlag ) {
			GetDXCoils();
			GetCoilsInputFlag = false;
		}

		if ( SameString( CoilType, "Coil:Heating:DX:SingleSpeed" ) || SameString( CoilType, "Coil:Cooling:DX:SingleSpeed" ) ) {
			WhichCoil = FindItem( CoilName, DXCoil );
			if ( WhichCoil != 0 ) {
				CoilCapacity = DXCoil( WhichCoil ).RatedTotCap( 1 );
			}
		} else if ( SameString( CoilType, "Coil:Cooling:DX:TwoStageWithHumidityControlMode" ) ) {
			WhichCoil = FindItem( CoilName, DXCoil );
			if ( WhichCoil != 0 ) {
				CoilCapacity = DXCoil( WhichCoil ).RatedTotCap( DXCoil( WhichCoil ).NumCapacityStages );
			}
		} else if ( SameString( CoilType, "Coil:Cooling:DX:TwoSpeed" ) ) {
			WhichCoil = FindItem( CoilName, DXCoil );
			if ( WhichCoil != 0 ) {
				CoilCapacity = DXCoil( WhichCoil ).RatedTotCap( 1 );
			}
		} else if ( SameString( CoilType, "Coil:Cooling:DX:MultiSpeed" ) ) {
			WhichCoil = FindItem( CoilName, DXCoil );
			if ( WhichCoil != 0 ) {
				CoilCapacity = DXCoil( WhichCoil ).MSRatedTotCap( DXCoil( WhichCoil ).NumOfSpeeds );
			}
		} else {
			WhichCoil = 0;
		}

		if ( WhichCoil == 0 ) {
			ShowSevereError( "GetCoilCapacity: Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"" );
			ShowContinueError( "... returning capacity as -1000." );
			ErrorsFound = true;
			CoilCapacity = -1000.0;
		}

		return CoilCapacity;

	}

	Real64
	GetCoilCapacityByIndexType(
		int const CoilIndex, // must match coil index for the coil type
		int const CoilType_Num, // must match coil types in this module
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   October 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the coil capacity for the given coil and returns it.  If
		// incorrect coil index or type is given, ErrorsFound is returned as true and capacity is returned
		// as negative.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItem;
		using InputProcessor::SameString;

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
		// na

		// Obtains and Allocates DXCoils
		if ( GetCoilsInputFlag ) {
			GetDXCoils();
			GetCoilsInputFlag = false;
		}

		if ( CoilIndex == 0 ) {
			ShowSevereError( "GetCoilCapacityByIndexType: Invalid index passed = 0" );
			ShowContinueError( "... returning capacity as -1000." );
			ErrorsFound = true;
			CoilCapacity = -1000.0;
			return CoilCapacity;
		}

		if ( CoilType_Num != DXCoil( CoilIndex ).DXCoilType_Num ) {
			ShowSevereError( "GetCoilCapacityByIndexType: Index passed does not match DX Coil type passed." );
			ShowContinueError( "... returning capacity as -1000." );
			ErrorsFound = true;
			CoilCapacity = -1000.0;
		} else {
			{ auto const SELECT_CASE_var( DXCoil( CoilIndex ).DXCoilType_Num );
			if ( ( SELECT_CASE_var == CoilDX_MultiSpeedCooling ) || ( SELECT_CASE_var == CoilDX_MultiSpeedHeating ) ) {
				CoilCapacity = DXCoil( CoilIndex ).MSRatedTotCap( DXCoil( CoilIndex ).NumOfSpeeds );
			} else {
				CoilCapacity = DXCoil( CoilIndex ).RatedTotCap( DXCoil( CoilIndex ).NumCapacityStages );
			}}
		}

		return CoilCapacity;

	}

	int
	GetCoilTypeNum(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound, // set to true if problem
		Optional_bool_const PrintWarning // prints warning when true
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         R. Raustad - FSEC
		//       DATE WRITTEN   August 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the integerized coil type for the given coil and returns it.  If
		// incorrect coil type or name is given, ErrorsFound is returned as true and capacity is returned
		// as negative.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Return value
		int TypeNum; // returned integerized type of matched coil

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
		bool PrintMessage;

		// Obtains and Allocates DXCoils
		if ( GetCoilsInputFlag ) {
			GetDXCoils();
			GetCoilsInputFlag = false;
		}

		if ( present( PrintWarning ) ) {
			PrintMessage = PrintWarning;
		} else {
			PrintMessage = true;
		}

		WhichCoil = FindItemInList( CoilName, DXCoil );
		if ( WhichCoil != 0 ) {
			TypeNum = DXCoil( WhichCoil ).DXCoilType_Num;
		} else {
			if ( PrintMessage ) {
				ShowSevereError( "GetCoilTypeNum: Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"" );
			}
			ErrorsFound = true;
			TypeNum = 0;
		}

		return TypeNum;

	}

	Real64
	GetMinOATCompressor(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   February 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the the min oat for the heating coil compressor and returns it.  If
		// incorrect coil type or name is given, ErrorsFound is returned as true and value is returned
		// as negative.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItem;
		using InputProcessor::SameString;

		// Return value
		Real64 MinOAT; // returned min oa temperature of matched coil

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

		// Obtains and Allocates DXCoils
		if ( GetCoilsInputFlag ) {
			GetDXCoils();
			GetCoilsInputFlag = false;
		}

		if ( SameString( CoilType, "Coil:Heating:DX:SingleSpeed" ) || SameString( CoilType, "Coil:Heating:DX:MultiSpeed" ) ) {
			WhichCoil = FindItem( CoilName, DXCoil );
			if ( WhichCoil != 0 ) {
				MinOAT = DXCoil( WhichCoil ).MinOATCompressor;
			}
		} else {
			WhichCoil = 0;
		}

		if ( WhichCoil == 0 ) {
			ShowSevereError( "GetMinOATCompressor: Could not find CoilType=\"" + CoilType + "\" with Name=\"" + CoilName + "\"" );
			ShowContinueError( "... returning Min OAT as -1000." );
			ErrorsFound = true;
			MinOAT = -1000.0;
		}

		return MinOAT;

	}

	int
	GetCoilInletNode(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   February 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the given coil and returns the inlet node number.  If
		// incorrect coil type or name is given, ErrorsFound is returned as true and node number is returned
		// as zero.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Return value
		int NodeNumber; // returned node number of matched coil

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

		// Obtains and Allocates DXCoils
		if ( GetCoilsInputFlag ) {
			GetDXCoils();
			GetCoilsInputFlag = false;
		}

		WhichCoil = FindItemInList( CoilName, DXCoil );
		if ( WhichCoil != 0 ) {
			NodeNumber = DXCoil( WhichCoil ).AirInNode;
		} else {
			ShowSevereError( "GetCoilInletNode: Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"" );
			ErrorsFound = true;
			NodeNumber = 0;
		}

		return NodeNumber;

	}

	int
	GetCoilOutletNode(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   February 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the given coil and returns the inlet node number.  If
		// incorrect coil type or name is given, ErrorsFound is returned as true and node number is returned
		// as zero.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Return value
		int NodeNumber; // returned node number of matched coil

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

		// Obtains and Allocates DXCoils
		if ( GetCoilsInputFlag ) {
			GetDXCoils();
			GetCoilsInputFlag = false;
		}

		WhichCoil = FindItemInList( CoilName, DXCoil );
		if ( WhichCoil != 0 ) {
			NodeNumber = DXCoil( WhichCoil ).AirOutNode;
		} else {
			ShowSevereError( "GetCoilOutletNode: Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\" when accessing coil outlet node number." );
			ErrorsFound = true;
			NodeNumber = 0;
		}

		return NodeNumber;

	}

	int
	GetCoilCondenserInletNode(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         R. Raustad
		//       DATE WRITTEN   January 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the given coil and returns the condenser inlet node.  If
		// incorrect coil type or name is given, ErrorsFound is returned as true.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Return value
		int CondNode; // returned condenser node number of matched coil

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

		// Obtains and Allocates DXCoils
		if ( GetCoilsInputFlag ) {
			GetDXCoils();
			GetCoilsInputFlag = false;
		}

		WhichCoil = FindItemInList( CoilName, DXCoil );
		if ( WhichCoil != 0 ) {
			CondNode = DXCoil( WhichCoil ).CondenserInletNodeNum( 1 );
		} else {
			ShowSevereError( "GetCoilCondenserInletNode: Invalid DX Coil, Type= \"" + CoilType + "\" Name=\"" + CoilName + "\"" );
			ErrorsFound = true;
			CondNode = 0;
		}

		return CondNode;

	}

	Real64
	GetDXCoilBypassedFlowFrac(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         R. Raustad
		//       DATE WRITTEN   June 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the given coil and returns the bypassed air flow fraction.
		// Bypassed air flow fraction can only be greater than 0 for multimode DX cooling coils and is typical for 1st stage
		// If incorrect coil type or name is given, ErrorsFound is returned as true.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Return value
		Real64 BypassFraction; // returned bypass air fraction of matched coil

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

		// Obtains and Allocates DXCoils
		if ( GetCoilsInputFlag ) {
			GetDXCoils();
			GetCoilsInputFlag = false;
		}

		WhichCoil = FindItemInList( CoilName, DXCoil );
		if ( WhichCoil != 0 ) {
			BypassFraction = DXCoil( WhichCoil ).BypassedFlowFrac( 1 );
		} else {
			ShowSevereError( "GetDXCoilBypassedFlowFrac: Invalid DX Coil Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"" );
			ErrorsFound = true;
			BypassFraction = 0.0;
		}

		return BypassFraction;

	}

	int
	GetHPCoolingCoilIndex(
		std::string const & HeatingCoilType, // Type of DX heating coil used in HP
		std::string const & HeatingCoilName, // Name of DX heating coil used in HP
		int const HeatingCoilIndex // Index of DX heating coil used in HP
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         R. Raustad
		//       DATE WRITTEN   February 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the given DX heating coil and returns the companion DX cooling coil.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using InputProcessor::SameString;
		using DataBranchNodeConnections::CompSets;
		using DataBranchNodeConnections::NumCompSets;

		// Return value
		int DXCoolingCoilIndex; // Index of HP DX cooling coil returned from this function

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int WhichComp; // DO loop counter to find correct comp set
		int WhichCompanionComp; // DO loop counter to find companion coil comp set
		int WhichHXAssistedComp; // DO loop counter when DX coil is used in a HX assisted cooling coil
		std::string CompSetsParentType; // Parent object type which uses DX heating coil pass into this function
		std::string CompSetsParentName; // Parent object name which uses DX heating coil pass into this function
		std::string HXCompSetsParentType; // Used when DX cooling coil is a child of a HX assisted cooling coil
		std::string HXCompSetsParentName; // Used when DX cooling coil is a child of a HX assisted cooling coil

		DXCoolingCoilIndex = 0;

		for ( WhichComp = 1; WhichComp <= NumCompSets; ++WhichComp ) {
			if ( ! SameString( HeatingCoilType, CompSets( WhichComp ).CType ) || ! SameString( HeatingCoilName, CompSets( WhichComp ).CName ) ) continue;
			CompSetsParentType = CompSets( WhichComp ).ParentCType;
			CompSetsParentName = CompSets( WhichComp ).ParentCName;
			if ( SameString( CompSetsParentType, "AirLoopHVAC:UnitaryHeatPump:AirToAir" ) || SameString( CompSetsParentType, "ZoneHVAC:PackagedTerminalHeatPump" ) || SameString( CompSetsParentType, "AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed" ) || SameString( CompSetsParentType, "AirLoopHVAC:UnitaryHeatCool:VAVChangeoverBypass" ) || SameString( CompSetsParentType, "AirLoopHVAC:UnitarySystem" ) ) {
				//       Search for DX cooling coils
				for ( WhichCompanionComp = 1; WhichCompanionComp <= NumCompSets; ++WhichCompanionComp ) {
					if ( ! SameString( CompSets( WhichCompanionComp ).ParentCName, CompSetsParentName ) || ! SameString( CompSets( WhichCompanionComp ).CType, "Coil:Cooling:DX:SingleSpeed" ) ) continue;
					DXCoolingCoilIndex = FindItemInList( CompSets( WhichCompanionComp ).CName, DXCoil );
					break;
				}
				for ( WhichCompanionComp = 1; WhichCompanionComp <= NumCompSets; ++WhichCompanionComp ) {
					if ( ! SameString( CompSets( WhichCompanionComp ).ParentCName, CompSetsParentName ) || ! SameString( CompSets( WhichCompanionComp ).CType, "Coil:Cooling:DX:MultiSpeed" ) ) continue;
					DXCoolingCoilIndex = FindItemInList( CompSets( WhichCompanionComp ).CName, DXCoil );
					break;
				}
				//       Search for Heat Exchanger Assisted DX cooling coils
				if ( DXCoolingCoilIndex == 0 ) {
					for ( WhichHXAssistedComp = 1; WhichHXAssistedComp <= NumCompSets; ++WhichHXAssistedComp ) {
						if ( ! SameString( CompSets( WhichHXAssistedComp ).ParentCName, CompSetsParentName ) || ! SameString( CompSets( WhichHXAssistedComp ).CType, "CoilSystem:Cooling:DX:HeatExchangerAssisted" ) ) continue;
						HXCompSetsParentType = CompSets( WhichHXAssistedComp ).CType;
						HXCompSetsParentName = CompSets( WhichHXAssistedComp ).CName;
						for ( WhichCompanionComp = 1; WhichCompanionComp <= NumCompSets; ++WhichCompanionComp ) {
							if ( ! SameString( CompSets( WhichCompanionComp ).ParentCName, HXCompSetsParentName ) || ! SameString( CompSets( WhichCompanionComp ).CType, "Coil:Cooling:DX:SingleSpeed" ) ) continue;
							DXCoolingCoilIndex = FindItemInList( CompSets( WhichCompanionComp ).CName, DXCoil );
							break;
						}
						break;
					}
				}
			} else {
				//     ErrorFound, Coil:Heating:DX:SingleSpeed is used in wrong type of parent object (should never get here)
				ShowSevereError( "Configuration error in " + CompSetsParentType + " \"" + CompSetsParentName + "\"" );
				ShowContinueError( "DX heating coil not allowed in this configuration." );
				ShowFatalError( "Preceding condition(s) causes termination." );
			}
			break;
		}

		// Check and warn user is crankcase heater power or max OAT for crankcase heater differs in DX cooling and heating coils
		if ( DXCoolingCoilIndex > 0 ) {
			if ( DXCoil( DXCoolingCoilIndex ).CrankcaseHeaterCapacity != 0.0 ) {
				if ( DXCoil( DXCoolingCoilIndex ).CrankcaseHeaterCapacity != DXCoil( HeatingCoilIndex ).CrankcaseHeaterCapacity || DXCoil( DXCoolingCoilIndex ).MaxOATCrankcaseHeater != DXCoil( HeatingCoilIndex ).MaxOATCrankcaseHeater ) {
					ShowWarningError( "Crankcase heater capacity or max outdoor temp for crankcase heater operation specified in" );
					ShowContinueError( "Coil:Cooling:DX:SingleSpeed = " + DXCoil( DXCoolingCoilIndex ).Name );
					ShowContinueError( "is different than that specified in Coil:Heating:DX:SingleSpeed = " + HeatingCoilName + '.' );
					ShowContinueError( "Both of these DX coils are part of " + CompSetsParentType + " = " + CompSetsParentName + '.' );
					ShowContinueError( "The value specified in the DX heating coil will be used and the simulation continues..." );
				}
			}
		}

		return DXCoolingCoilIndex;

	}

	int
	GetDXCoilNumberOfSpeeds(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         L. Gu
		//       DATE WRITTEN   July 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the given coil and returns the number of speeds for multispeed coils.
		// If incorrect coil type or name is given, ErrorsFound is returned as true.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Return value
		int NumberOfSpeeds; // returned the number of speed of matched coil

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

		// Obtains and Allocates DXCoils
		if ( GetCoilsInputFlag ) {
			GetDXCoils();
			GetCoilsInputFlag = false;
		}

		WhichCoil = FindItemInList( CoilName, DXCoil );
		if ( WhichCoil != 0 ) {
			NumberOfSpeeds = DXCoil( WhichCoil ).NumOfSpeeds;
		} else {
			ShowSevereError( "GetDXCoilNumberOfSpeeds: Invalid DX Coil Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"" );
			ErrorsFound = true;
			NumberOfSpeeds = 0;
		}

		return NumberOfSpeeds;

	}

	int
	GetDXCoilAvailSchPtr(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound, // set to true if problem
		Optional_int_const CoilIndex // Coil index number
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   January 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the given coil and returns the availability schedule index.  If
		// incorrect coil type or name is given, ErrorsFound is returned as true and schedule index is returned
		// as -1.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Return value
		int SchPtr; // returned availabiltiy schedule of matched coil

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

		// Obtains and Allocates DXCoils
		if ( GetCoilsInputFlag ) {
			GetDXCoils();
			GetCoilsInputFlag = false;
		}

		if ( present( CoilIndex ) ) {
			if ( CoilIndex == 0 ) {
				ShowSevereError( "GetDXCoilAvailSchPtr: Invalid index passed = 0" );
				ShowContinueError( "... returning DXCoilAvailSchPtr as -1." );
				ErrorsFound = true;
				SchPtr = -1;
				return SchPtr;
			} else {
				WhichCoil = CoilIndex;
			}
		} else {
			WhichCoil = FindItemInList( CoilName, DXCoil );
		}
		if ( WhichCoil != 0 ) {
			SchPtr = DXCoil( WhichCoil ).SchedPtr;
		} else {
			if ( ! present( CoilIndex ) ) {
				ShowSevereError( "GetDXCoilAvailSch: Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\" when accessing coil availability schedule index." );
			}
			ErrorsFound = true;
			SchPtr = -1;
		}

		return SchPtr;

	}

	Real64
	GetDXCoilAirFlow(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   January 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the given coil and returns the availability schedule index.  If
		// incorrect coil type or name is given, ErrorsFound is returned as true and schedule index is returned
		// as -1.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Return value
		Real64 AirFlow; // returned coil air flow rate

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

		// Obtains and Allocates DXCoils
		if ( GetCoilsInputFlag ) {
			GetDXCoils();
			GetCoilsInputFlag = false;
		}

		WhichCoil = FindItemInList( CoilName, DXCoil );
		if ( WhichCoil != 0 ) {
			{ auto const SELECT_CASE_var( DXCoil( WhichCoil ).DXCoilType_Num );
			if ( ( SELECT_CASE_var == CoilDX_CoolingSingleSpeed ) || ( SELECT_CASE_var == CoilDX_CoolingTwoSpeed ) || ( SELECT_CASE_var == CoilDX_HeatingEmpirical ) || ( SELECT_CASE_var == CoilDX_CoolingTwoStageWHumControl ) ) {
				AirFlow = DXCoil( WhichCoil ).RatedAirVolFlowRate( 1 );
			} else if ( ( SELECT_CASE_var == CoilDX_MultiSpeedCooling ) || ( SELECT_CASE_var == CoilDX_MultiSpeedHeating ) ) {
				AirFlow = DXCoil( WhichCoil ).MSRatedAirVolFlowRate( 1 );
			} else {
				ShowSevereError( "GetDXCoilAirFlow: Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\" when accessing coil air flow rate." );
				ErrorsFound = true;
				AirFlow = -1.0;
			}}
		} else {
			ShowSevereError( "GetDXCoilAirFlow: Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\" when accessing coil air flow rate." );
			ErrorsFound = true;
			AirFlow = -1.0;
		}

		return AirFlow;

	}

	int
	GetDXCoilCapFTCurveIndex(
		int const CoilIndex, // coil index pointer
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   August 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the given coil and returns the CapFT schedule index.  If
		// incorrect coil index is given, ErrorsFound is returned as true and schedule index is returned
		// as -1.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Return value
		int CapFTCurveIndex; // returned coil CapFT curve index

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

		// Obtains and Allocates DXCoils
		if ( GetCoilsInputFlag ) {
			GetDXCoils();
			GetCoilsInputFlag = false;
		}

		if ( CoilIndex != 0 ) {
			{ auto const SELECT_CASE_var( DXCoil( CoilIndex ).DXCoilType_Num );
			if ( ( SELECT_CASE_var == CoilDX_CoolingSingleSpeed ) || ( SELECT_CASE_var == CoilDX_CoolingTwoSpeed ) || ( SELECT_CASE_var == CoilDX_HeatingEmpirical ) || ( SELECT_CASE_var == CoilDX_CoolingTwoStageWHumControl ) ) {
				CapFTCurveIndex = DXCoil( CoilIndex ).CCapFTemp( 1 );
			} else if ( ( SELECT_CASE_var == CoilDX_MultiSpeedCooling ) || ( SELECT_CASE_var == CoilDX_MultiSpeedHeating ) ) {
				CapFTCurveIndex = DXCoil( CoilIndex ).MSCCapFTemp( 1 );
			} else {
				//        CALL ShowSevereError('GetDXCoilCapFTCurveIndex: Could not find Coil, Type="'// &
				//             TRIM(cAllCoilTypes(DXCoil(CoilIndex)%DXCoilType_Num))//'" Name="'//TRIM(DXCoil(CoilIndex)%Name)//  &
				//              '" when accessing coil capacity as a function of temperture curve.')
				ErrorsFound = true;
				CapFTCurveIndex = 0;
			}}
		} else {
			//    CALL ShowSevereError('GetDXCoilCapFTCurveIndex: Could not find Coil, Index = 0'// &
			//          ' when accessing coil air flow rate.')
			ErrorsFound = true;
			CapFTCurveIndex = 0;
		}

		return CapFTCurveIndex;

	}

	void
	SetDXCoolingCoilData(
		int const DXCoilNum, // Number of DX Cooling Coil
		bool & ErrorsFound, // Set to true if certain errors found
		Optional_int HeatingCoilPLFCurvePTR, // Parameter equivalent of heating coil PLR curve index
		Optional_int CondenserType, // Parameter equivalent of condenser type parameter
		Optional_int CondenserInletNodeNum, // Parameter equivalent of condenser inlet node number
		Optional< Real64 > MaxOATCrankcaseHeater, // Parameter equivalent of condenser Max OAT for Crank Case Heater temp
		Optional< Real64 > MinOATCooling, // Parameter equivalent of condenser Min OAT for compressor cooling operation
		Optional< Real64 > MaxOATCooling, // Parameter equivalent of condenser Max OAT for compressor cooling operation
		Optional< Real64 > MinOATHeating, // Parameter equivalent of condenser Min OAT for compressor heating operation
		Optional< Real64 > MaxOATHeating, // Parameter equivalent of condenser Max OAT for compressor heating operation
		Optional_int HeatingPerformanceOATType, // Parameter equivalent to condenser entering air temp type (1-db, 2=wb)
		Optional_int DefrostStrategy,
		Optional_int DefrostControl,
		Optional_int DefrostEIRPtr,
		Optional< Real64 > DefrostFraction,
		Optional< Real64 > DefrostCapacity,
		Optional< Real64 > MaxOATDefrost,
		Optional_bool CoolingCoilPresent,
		Optional_bool HeatingCoilPresent,
		Optional< Real64 > HeatSizeRatio,
		Optional< Real64 > TotCap,
		Optional_int SupplyFanIndex,
		Optional_string SupplyFanName
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   December 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine was designed to allow the DX coil to access information from a gas or
		// electric heating coil when these coils are each used in a parent object.
		// Also, this is an illustration of setting Data from an outside source.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::TrimSigDigits;

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

		// Obtains and Allocates DXCoils
		if ( GetCoilsInputFlag ) {
			GetDXCoils();
			GetCoilsInputFlag = false;
		}

		if ( DXCoilNum <= 0 || DXCoilNum > NumDXCoils ) {
			ShowSevereError( "SetDXCoolingCoilData: called with DX Cooling Coil Number out of range=" + TrimSigDigits( DXCoilNum ) + " should be >0 and <" + TrimSigDigits( NumDXCoils ) );
			ErrorsFound = true;
			return;
		}

		if ( present( HeatingCoilPLFCurvePTR ) ) {
			DXCoil( DXCoilNum ).HeatingCoilPLFCurvePTR = HeatingCoilPLFCurvePTR;
		}

		if ( present( CondenserType ) ) {
			DXCoil( DXCoilNum ).CondenserType = CondenserType;
		}

		if ( present( CondenserInletNodeNum ) ) {
			DXCoil( DXCoilNum ).CondenserInletNodeNum( 1 ) = CondenserInletNodeNum;
		}

		if ( present( MaxOATCrankcaseHeater ) ) {
			DXCoil( DXCoilNum ).MaxOATCrankcaseHeater = MaxOATCrankcaseHeater;
		}

		if ( present( MaxOATCooling ) ) {
			DXCoil( DXCoilNum ).MaxOATCompressor = MaxOATCooling;
		}

		if ( present( MaxOATHeating ) ) {
			DXCoil( DXCoilNum ).MaxOATCompressor = MaxOATHeating;
		}

		if ( present( MinOATCooling ) ) {
			DXCoil( DXCoilNum ).MinOATCompressor = MinOATCooling;
		}

		if ( present( MinOATHeating ) ) {
			DXCoil( DXCoilNum ).MinOATCompressor = MinOATHeating;
		}

		if ( present( HeatingPerformanceOATType ) ) {
			DXCoil( DXCoilNum ).HeatingPerformanceOATType = HeatingPerformanceOATType;
		}

		if ( present( DefrostStrategy ) ) {
			DXCoil( DXCoilNum ).DefrostStrategy = DefrostStrategy;
		}

		if ( present( DefrostControl ) ) {
			DXCoil( DXCoilNum ).DefrostControl = DefrostControl;
		}

		if ( present( DefrostEIRPtr ) ) {
			DXCoil( DXCoilNum ).DefrostEIRFT = DefrostEIRPtr;
		}

		if ( present( DefrostFraction ) ) {
			DXCoil( DXCoilNum ).DefrostTime = DefrostFraction;
		}

		if ( present( DefrostCapacity ) ) {
			DXCoil( DXCoilNum ).DefrostCapacity = DefrostCapacity;
		}

		if ( present( MaxOATDefrost ) ) {
			DXCoil( DXCoilNum ).MaxOATDefrost = MaxOATDefrost;
		}

		if ( present( CoolingCoilPresent ) ) {
			DXCoil( DXCoilNum ).CoolingCoilPresent = CoolingCoilPresent;
		}

		if ( present( HeatingCoilPresent ) ) {
			DXCoil( DXCoilNum ).HeatingCoilPresent = HeatingCoilPresent;
		}

		if ( present( HeatSizeRatio ) ) {
			DXCoil( DXCoilNum ).HeatSizeRatio = HeatSizeRatio;
		}

		if ( present( TotCap ) ) {
			DXCoil( DXCoilNum ).RatedTotCap( 1 ) = TotCap;
		}

		if ( present( SupplyFanIndex ) ) {
			DXCoil( DXCoilNum ).SupplyFanIndex = SupplyFanIndex;
		}

		if ( present( SupplyFanName ) ) {
			DXCoil( DXCoilNum ).SupplyFanName = SupplyFanName;
		}

	}

	void
	SetCoilSystemHeatingDXFlag(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName // must match coil names for the coil type
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   Jan. 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// inform DX heating coil that is is part of a CoilSystem:Heating:DX
		// and therefore it need not find its companion cooling coil

		// METHODOLOGY EMPLOYED:
		// set value of logical flag FindCompanionUpStreamCoil to true

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int WhichCoil;

		// Obtains and Allocates DXCoils
		if ( GetCoilsInputFlag ) {
			GetDXCoils();
			GetCoilsInputFlag = false;
		}

		WhichCoil = FindItemInList( CoilName, DXCoil );
		if ( WhichCoil != 0 ) {
			DXCoil( WhichCoil ).FindCompanionUpStreamCoil = false;
		} else {
			ShowSevereError( "SetCoilSystemHeatingDXFlag: Could not find Coil, Type=\"" + CoilType + "\"Name=\"" + CoilName + "\"" );

		}

	}

	void
	SetCoilSystemCoolingData(
		std::string const & CoilName, // must match coil names for the coil type
		std::string const & CoilSystemName
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   July 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// inform the child DX coil what the name of its parent is.

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int WhichCoil;

		if ( GetCoilsInputFlag ) {
			GetDXCoils();
			GetCoilsInputFlag = false;
		}

		WhichCoil = FindItemInList( CoilName, DXCoil );
		if ( WhichCoil != 0 ) {
			DXCoil( WhichCoil ).CoilSystemName = CoilSystemName;
		} else {
			ShowSevereError( "SetCoilSystemCoolingData: Could not find Coil \"Name=\"" + CoilName + "\"" );
		}

	}

	Real64
	CalcSHRUserDefinedCurves(
		Real64 const InletDryBulb, // inlet air dry bulb temperature [C]
		Real64 const InletWetBulb, // inlet air wet bulb temperature [C]
		Real64 const AirMassFlowRatio, // ratio of actual air mass flow to rated air mass flow
		int const SHRFTempCurveIndex, // SHR modifier curve index
		int const SHRFFlowCurveIndex, // SHR modifier curve index
		Real64 const SHRRated // rated sensible heat ratio, user input
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Bereket Nigusse, FSEC
		//       DATE WRITTEN   December 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		//    Returns the oprating sensible heat ratio for a given Rated SHR abd coil entering
		//    air DBT and WBT, and supply air mass flow fraction.

		// METHODOLOGY EMPLOYED:
		//    Model uses user specified rated SHR, and SHR modifying curves for temperature and flow
		//    fraction.  The curves adjust the rated SHR based on biquadratic curve for temperatures
		//    and quadratic function for supply air mass flow ratio (actual vs rated).
		//    The biquadratic and quadratic curves are normalized caurves generated from manufacturer's
		//    performance data

		// REFERENCES: na

		// Using/Aliasing
		using CurveManager::CurveValue;

		// Return value
		Real64 SHRopr; // operating SHR, corrected for Temp and Flow Fraction

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		static std::string const RoutineName( "CalcSHRUserDefinedCurves" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na
		// DERIVED TYPE DEFINITIONS
		// na
		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 SHRTempModFac; // Sensible Heat Ratio modifier (function of entering wetbulb, entering drybulb)
		Real64 SHRFlowModFac; // Sensible Heat Ratio modifier (function of actual vs rated flow)

		//   Get SHR modifying factor (function of inlet wetbulb & drybulb) for off-rated conditions
		if ( SHRFTempCurveIndex == 0 ) {
			SHRTempModFac = 1.0;
		} else {
			SHRTempModFac = CurveValue( SHRFTempCurveIndex, InletWetBulb, InletDryBulb );
			if ( SHRTempModFac < 0.0 ) {
				SHRTempModFac = 0.0;
			}
		}
		//   Get SHR modifying factor (function of mass flow ratio) for off-rated conditions
		if ( SHRFFlowCurveIndex == 0 ) {
			SHRFlowModFac = 1.0;
		} else {
			SHRFlowModFac = CurveValue( SHRFFlowCurveIndex, AirMassFlowRatio );
			if ( SHRFlowModFac < 0.0 ) {
				SHRFlowModFac = 0.0;
			}
		}
		//  Calculate "operating" sensible heat ratio
		SHRopr = SHRRated * SHRTempModFac * SHRFlowModFac;

		if ( SHRopr < 0.0 ) SHRopr = 0.0; // SHR cannot be less than zero
		if ( SHRopr > 1.0 ) SHRopr = 1.0; // SHR cannot be greater than 1.0

		return SHRopr;
	}

	void
	SetDXCoilTypeData( std::string const & CoilName ) // must match coil names for the coil type
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Nigusse
		//       DATE WRITTEN   January 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// inform the child DX coil if the DX cooling coil is for 100% DOAS application.

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int WhichCoil;

		if ( GetCoilsInputFlag ) {
			GetDXCoils();
			GetCoilsInputFlag = false;
		}

		WhichCoil = FindItemInList( CoilName, DXCoil );
		if ( WhichCoil != 0 ) {
			DXCoil( WhichCoil ).ISHundredPercentDOASDXCoil = true;
		} else {
			//DXCoil(WhichCoil)%ISHundredPercentDOASDXCoil = .FALSE. //Autodesk:BoundsViolation DXCoil(0): DXCoil is not allocated with a 0 element: Commented out
			ShowSevereError( "SetDXCoilTypeData: Could not find Coil \"Name=\"" + CoilName + "\"" );
		}

	}

	void
	CalcSecondaryDXCoils(
		int const DXCoilNum
		) {

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Nigusse
		//       DATE WRITTEN   February 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates secondary zone heat gain from secondary DX coils placed in a zone.

		// METHODOLOGY EMPLOYED:
		// Energy balance:
		//  (1) Condenser placed in a zone, the zone total (sensible) heat
		//      gain rate is given Qcond = QEvap + WcompPluscondFanPower
		//  (2) Evaporator placed in a zone, the zone total heat removal
		//      rate is given Qevap = Qcond - WcompPluscondFanPower
		//      Furthermore, the evaporator total heat removal is split into
		//      latent and sensible components using user specified SHR

		// REFERENCES:
		// na

		// Using/Aliasing
		using CurveManager::CurveValue;
		using DataHVACGlobals::TimeStepSys;
		using DataLoopNode::Node;
		using DataHeatBalFanSys::ZoneAirHumRat;
		using DataHeatBalFanSys::ZT;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcSecondaryDXCoils" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 CondInletDryBulb; // condenser entering air dry-bulb temperature (C)
		Real64 EvapAirMassFlow; // Condenser air mass flow rate [kg/s]
		Real64 EvapInletDryBulb; // evaporator inlet air drybulb [C]
		Real64 EvapInletHumRat; // evaporator inlet air humidity ratio [kg/kg]
		Real64 EvapInletWetBulb; // evaporator inlet air wetbulb [C]
		Real64 EvapInletEnthalpy; // evaporator inlet air enthalpy [J/kg]
		Real64 FullLoadOutAirEnth; // evaporator outlet full load enthalpy [J/kg]
		Real64 FullLoadOutAirHumRat; // evaporator outlet humidity ratio at full load
		Real64 FullLoadOutAirTemp; // evaporator outlet air temperature at full load [C]
		Real64 hTinwout; // Enthalpy at inlet dry-bulb and outlet humidity ratio [J/kg]
		Real64 SHR( 0 ); // sensible heat ratio
		Real64 RhoAir; // secondary coil entering air density [kg/m3]
		Real64 PartLoadRatio( 0 ); // primary coil part-load ratio [-]
		Real64 SecCoilRatedSHR; // secondary DX coil nominal or rated sensible heat ratio
		Real64 SecCoilFlowFraction; // secondary coil flow fraction, is 1.0 for single speed machine
		Real64 TotalHeatRemovalRate; // secondary coil total heat removal rate
		Real64 TotalHeatRejectionRate; // secondary coil total heat rejection rate
		int SecCoilSHRFT; // index of the SHR modifier curve for temperature of a secondary DX coil
		int SecCoilSHRFF; // index of the sHR modifier curve for flow fraction of a secondary DX coil
		int MSSpeedNumLS; // current low speed number of multspeed HP
		int MSSpeedNumHS; // current high speed number of multspeed HP
		Real64 MSSpeedRatio; // current speed ratio of multspeed HP
		Real64 MSCycRatio; // current cycling ratio of multspeed HP
		Real64 SHRHighSpeed; // sensible heat ratio at high speed
		Real64 SHRLowSpeed; // sensible heat ratio at low speed

		EvapAirMassFlow = 0.0;

		if ( DXCoil( DXCoilNum ).IsSecondaryDXCoilInZone ) {
			// Select the correct unit type
			{ auto const SELECT_CASE_var( DXCoil( DXCoilNum ).DXCoilType_Num );
			if ( ( SELECT_CASE_var == CoilDX_CoolingSingleSpeed ) || ( SELECT_CASE_var == CoilDX_CoolingTwoSpeed ) || ( SELECT_CASE_var == CoilDX_MultiSpeedCooling ) ) {
				// total sensible heat gain of the secondary zone from the secondary coil (condenser)
				if ( DXCoil( DXCoilNum ).ElecCoolingPower > 0.0 ) {
					TotalHeatRejectionRate = DXCoil( DXCoilNum ).TotalCoolingEnergyRate + DXCoil( DXCoilNum ).ElecCoolingPower;
				} else {
					TotalHeatRejectionRate = 0.0;
					return;
				}
				DXCoil( DXCoilNum ).SecCoilSensibleHeatGainRate = TotalHeatRejectionRate;
			} else if ( SELECT_CASE_var == CoilDX_HeatingEmpirical ) {
				// evaporator coil in the secondary zone
				if ( DXCoil( DXCoilNum ).ElecHeatingPower > 0.0 ) {
					TotalHeatRemovalRate = max( 0.0, DXCoil( DXCoilNum ).TotalHeatingEnergyRate - DXCoil( DXCoilNum ).ElecHeatingPower );
				} else {
					TotalHeatRemovalRate = 0.0;
					DXCoil( DXCoilNum ).SecCoilSHR = 0.0;
					return;
				}
				DXCoil( DXCoilNum ).SecCoilTotalHeatRemovalRate = -TotalHeatRemovalRate; // +DXCoil( DXCoilNum ).DefrostPower;
				EvapInletDryBulb = ZT( DXCoil( DXCoilNum ).SecZonePtr );
				EvapInletHumRat = ZoneAirHumRat( DXCoil( DXCoilNum ).SecZonePtr );
				RhoAir = PsyRhoAirFnPbTdbW( OutBaroPress, EvapInletDryBulb, EvapInletHumRat );
				EvapAirMassFlow = RhoAir * DXCoil( DXCoilNum ).SecCoilAirFlow;;
				PartLoadRatio = DXCoil( DXCoilNum ).CompressorPartLoadRatio;
				SecCoilRatedSHR = DXCoil( DXCoilNum ).SecCoilRatedSHR;
				if ( ( EvapAirMassFlow > SmallMassFlow ) && ( PartLoadRatio > 0.0 ) && ( EvapInletDryBulb > DXCoil( DXCoilNum ).MinOATCompressor ) ) { // coil is running
					SecCoilFlowFraction = 1.0;  // for single speed DX coil the secondary coil (condenser) flow fraction is 1.0
					CondInletDryBulb = Node( DXCoil( DXCoilNum ).AirInNode ).Temp;
					EvapInletWetBulb = PsyTwbFnTdbWPb( EvapInletDryBulb, EvapInletHumRat, OutBaroPress, RoutineName );
					EvapInletEnthalpy = PsyHFnTdbW( EvapInletDryBulb, EvapInletHumRat );
					SecCoilSHRFT = DXCoil( DXCoilNum ).SecCoilSHRFT;
					SecCoilSHRFF = DXCoil( DXCoilNum ).SecCoilSHRFF;
					// determine the current SHR
					SHR = CalcSecondaryDXCoilsSHR( DXCoilNum, EvapAirMassFlow, TotalHeatRemovalRate, PartLoadRatio, SecCoilRatedSHR, EvapInletDryBulb, EvapInletHumRat, EvapInletWetBulb, EvapInletEnthalpy, CondInletDryBulb, SecCoilFlowFraction, SecCoilSHRFT, SecCoilSHRFF );
					// Calculate full load output conditions
					FullLoadOutAirEnth = EvapInletEnthalpy - ( TotalHeatRemovalRate / PartLoadRatio ) / EvapAirMassFlow;
					hTinwout = EvapInletEnthalpy - ( 1.0 - SHR ) * ( ( TotalHeatRemovalRate / PartLoadRatio ) / EvapAirMassFlow );
					FullLoadOutAirHumRat = PsyWFnTdbH( EvapInletDryBulb, hTinwout, RoutineName, true );
					FullLoadOutAirTemp = PsyTdbFnHW( FullLoadOutAirEnth, FullLoadOutAirHumRat );
					// when the air outlet temperature falls below the saturation temperature, it is reset to saturation temperature
					if ( FullLoadOutAirTemp < PsyTsatFnHPb( FullLoadOutAirEnth, OutBaroPress, RoutineName ) ) {
						FullLoadOutAirTemp = PsyTsatFnHPb( FullLoadOutAirEnth, OutBaroPress, RoutineName );
						FullLoadOutAirHumRat = PsyWFnTdbH( FullLoadOutAirTemp, FullLoadOutAirEnth, RoutineName );
						// Adjust SHR for the new outlet condition that balances energy
						hTinwout = PsyHFnTdbW( EvapInletDryBulb, FullLoadOutAirHumRat );
						SHR = 1.0 - ( EvapInletEnthalpy - hTinwout ) / ( ( TotalHeatRemovalRate / PartLoadRatio ) / EvapAirMassFlow );
						SHR = min( SHR, 1.0 );
					}
					// calculate the sensible and latent zone heat removal (extraction) rate by the secondary coil
					DXCoil( DXCoilNum ).SecCoilSensibleHeatRemovalRate = DXCoil( DXCoilNum ).SecCoilTotalHeatRemovalRate * SHR;
					DXCoil( DXCoilNum ).SecCoilLatentHeatRemovalRate = DXCoil( DXCoilNum ).SecCoilTotalHeatRemovalRate - DXCoil( DXCoilNum ).SecCoilSensibleHeatRemovalRate;
				} else {
					//DX coil is off;
					DXCoil( DXCoilNum ).SecCoilTotalHeatRemovalRate = 0.0;
					DXCoil( DXCoilNum ).SecCoilSensibleHeatRemovalRate = 0.0;
					DXCoil( DXCoilNum ).SecCoilLatentHeatRemovalRate = 0.0;
					SHR = 0.0; // SHR is set to zero if the coil is off
				}
				DXCoil( DXCoilNum ).SecCoilSHR = SHR;

			} else if ( SELECT_CASE_var == CoilDX_MultiSpeedHeating ) {
				EvapInletDryBulb = ZT( DXCoil( DXCoilNum ).SecZonePtr );
				EvapInletHumRat = ZoneAirHumRat( DXCoil( DXCoilNum ).SecZonePtr );
				RhoAir = PsyRhoAirFnPbTdbW( OutBaroPress, EvapInletDryBulb, EvapInletHumRat );
				MSSpeedRatio = DXCoil( DXCoilNum ).MSSpeedRatio;
				MSCycRatio = DXCoil( DXCoilNum ).MSCycRatio;
				MSSpeedNumHS = DXCoil( DXCoilNum ).MSSpeedNumHS;
				MSSpeedNumLS = DXCoil( DXCoilNum ).MSSpeedNumLS;
				if ( MSSpeedRatio > 0.0 ) {
					EvapAirMassFlow = RhoAir * ( DXCoil( DXCoilNum ).MSSecCoilAirFlow( MSSpeedNumHS ) * MSSpeedRatio + DXCoil( DXCoilNum ).MSSecCoilAirFlow( MSSpeedNumLS ) * ( 1.0 - MSSpeedRatio) );
				} else if ( MSCycRatio > 0.0 ) {
					EvapAirMassFlow = RhoAir * DXCoil( DXCoilNum ).MSSecCoilAirFlow( MSSpeedNumLS );
				}
				if ( DXCoil( DXCoilNum ).ElecHeatingPower > 0.0 ) {
					TotalHeatRemovalRate = max( 0.0, DXCoil( DXCoilNum ).TotalHeatingEnergyRate - DXCoil( DXCoilNum ).ElecHeatingPower );
				} else {
					TotalHeatRemovalRate = 0.0;
					return;
				}
				DXCoil( DXCoilNum ).SecCoilTotalHeatRemovalRate = -TotalHeatRemovalRate; // +DXCoil( DXCoilNum ).DefrostPower;
				if ( ( EvapAirMassFlow > SmallMassFlow ) && ( MSSpeedRatio > 0.0 || MSCycRatio > 0.0 ) && ( EvapInletDryBulb > DXCoil( DXCoilNum ).MinOATCompressor ) ) { // coil is running
					SecCoilFlowFraction = 1.0;  // for single speed DX coil the secondary coil (condenser) flow fraction is 1.0
					CondInletDryBulb = Node( DXCoil( DXCoilNum ).AirInNode ).Temp;
					EvapInletWetBulb = PsyTwbFnTdbWPb( EvapInletDryBulb, EvapInletHumRat, OutBaroPress, RoutineName );
					EvapInletEnthalpy = PsyHFnTdbW( EvapInletDryBulb, EvapInletHumRat );
					// determine the current SHR
					if ( MSSpeedRatio > 0.0 ) {
						// calculate SHR for the higher speed
						PartLoadRatio = 1.0;
						SecCoilFlowFraction = 1.0;
						SecCoilSHRFT = DXCoil( DXCoilNum ).MSSecCoilSHRFT( MSSpeedNumHS );
						SecCoilSHRFF = DXCoil( DXCoilNum ).MSSecCoilSHRFF( MSSpeedNumHS );
						SecCoilRatedSHR = DXCoil( DXCoilNum ).MSSecCoilRatedSHR( MSSpeedNumHS );
						SHRHighSpeed = CalcSecondaryDXCoilsSHR(DXCoilNum, EvapAirMassFlow, TotalHeatRemovalRate, PartLoadRatio, SecCoilRatedSHR, EvapInletDryBulb, EvapInletHumRat, EvapInletWetBulb, EvapInletEnthalpy, CondInletDryBulb, SecCoilFlowFraction, SecCoilSHRFT, SecCoilSHRFF);
						// calculate SHR for the lower speed
						SecCoilSHRFT = DXCoil( DXCoilNum ).MSSecCoilSHRFT( MSSpeedNumLS );
						SecCoilSHRFF = DXCoil( DXCoilNum ).MSSecCoilSHRFF( MSSpeedNumLS );
						SecCoilRatedSHR = DXCoil( DXCoilNum ).MSSecCoilRatedSHR( MSSpeedNumLS );
						SHRLowSpeed = CalcSecondaryDXCoilsSHR(DXCoilNum, EvapAirMassFlow, TotalHeatRemovalRate, PartLoadRatio, SecCoilRatedSHR, EvapInletDryBulb, EvapInletHumRat, EvapInletWetBulb, EvapInletEnthalpy, CondInletDryBulb, SecCoilFlowFraction, SecCoilSHRFT, SecCoilSHRFF);
						SHR = SHRHighSpeed * MSSpeedRatio + SHRLowSpeed * ( 1.0 - MSSpeedRatio );

					} else if ( MSCycRatio > 0.0 ) {
						// calculate SHR for the lower speed
						PartLoadRatio = MSCycRatio;
						SecCoilSHRFT = DXCoil( DXCoilNum ).MSSecCoilSHRFT( MSSpeedNumLS );
						SecCoilSHRFF = DXCoil( DXCoilNum ).MSSecCoilSHRFF( MSSpeedNumLS );
						SecCoilRatedSHR = DXCoil( DXCoilNum ).MSSecCoilRatedSHR( MSSpeedNumLS );
						SecCoilFlowFraction = 1.0;
						SHRLowSpeed = CalcSecondaryDXCoilsSHR( DXCoilNum, EvapAirMassFlow, TotalHeatRemovalRate, MSCycRatio, SecCoilRatedSHR, EvapInletDryBulb, EvapInletHumRat, EvapInletWetBulb, EvapInletEnthalpy, CondInletDryBulb, SecCoilFlowFraction, SecCoilSHRFT, SecCoilSHRFF );
						SHR = SHRLowSpeed;
					}
					// Calculate full load output conditions
					FullLoadOutAirEnth = EvapInletEnthalpy - ( TotalHeatRemovalRate / PartLoadRatio ) / EvapAirMassFlow;
					hTinwout = EvapInletEnthalpy - ( 1.0 - SHR ) * ( ( TotalHeatRemovalRate / PartLoadRatio ) / EvapAirMassFlow );
					FullLoadOutAirHumRat = PsyWFnTdbH( EvapInletDryBulb, hTinwout, RoutineName, true );
					FullLoadOutAirTemp = PsyTdbFnHW( FullLoadOutAirEnth, FullLoadOutAirHumRat );
					// when the air outlet temperature falls below the saturation temperature, it is reset to saturation temperature
					if ( FullLoadOutAirTemp < PsyTsatFnHPb( FullLoadOutAirEnth, OutBaroPress, RoutineName ) ) {
						FullLoadOutAirTemp = PsyTsatFnHPb( FullLoadOutAirEnth, OutBaroPress, RoutineName );
						FullLoadOutAirHumRat = PsyWFnTdbH( FullLoadOutAirTemp, FullLoadOutAirEnth, RoutineName );
						// Adjust SHR for the new outlet condition that balances energy
						hTinwout = PsyHFnTdbW( EvapInletDryBulb, FullLoadOutAirHumRat );
						SHR = 1.0 - ( EvapInletEnthalpy - hTinwout ) / ( TotalHeatRemovalRate / PartLoadRatio ) / EvapAirMassFlow;
						SHR = min( SHR, 1.0 );
					}
					// calculate the sensible and latent zone heat removal (extraction) rate by the secondary coil
					DXCoil( DXCoilNum ).SecCoilSensibleHeatRemovalRate = DXCoil( DXCoilNum ).SecCoilTotalHeatRemovalRate * SHR;
					DXCoil( DXCoilNum ).SecCoilLatentHeatRemovalRate = DXCoil( DXCoilNum ).SecCoilTotalHeatRemovalRate - DXCoil( DXCoilNum ).SecCoilSensibleHeatRemovalRate;
				} else {
					//DX coil is off;
					DXCoil( DXCoilNum ).SecCoilTotalHeatRemovalRate = 0.0;
					DXCoil( DXCoilNum ).SecCoilSensibleHeatRemovalRate = 0.0;
					DXCoil( DXCoilNum ).SecCoilLatentHeatRemovalRate = 0.0;
					SHR = 0.0; // SHR is set to rated value if the coil is off
				}
				DXCoil( DXCoilNum ).SecCoilSHR = SHR;

			}} // end of { auto const SELECT_CASE_var( DXCoil( DXCoilNum ).DXCoilType_Num );

		} else {
			DXCoil( DXCoilNum ).SecCoilSensibleHeatGainRate = 0.0;
			DXCoil( DXCoilNum ).SecCoilTotalHeatRemovalRate = 0.0;
			DXCoil( DXCoilNum ).SecCoilSensibleHeatRemovalRate = 0.0;
			DXCoil( DXCoilNum ).SecCoilLatentHeatRemovalRate = 0.0;
		}
	}

	Real64
	CalcSecondaryDXCoilsSHR(
		int const EP_UNUSED( DXCoilNum ),
		Real64 const EvapAirMassFlow,
		Real64 const TotalHeatRemovalRate,
		Real64 const PartLoadRatio,
		Real64 const SecCoilRatedSHR,
		Real64 const EvapInletDryBulb,
		Real64 const EvapInletHumRat,
		Real64 const EvapInletWetBulb,
		Real64 const EvapInletEnthalpy,
		Real64 const CondInletDryBulb,
		Real64 const SecCoilFlowFraction,
		int const SecCoilSHRFT,
		int const SecCoilSHRFF
		) {

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Nigusse
		//       DATE WRITTEN   February 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates secondary coil (evaporator) sensible heat ratio.

		// METHODOLOGY EMPLOYED:
		// Energy balance:
		//  (1) checks if the seconday coil operation is dry and calculates appliavle SHR.
		//  (2) determines SHR from user specified rated SHR values and SHR modifier curves for
		//      temperature and flor fraction.
		//  (3) if secondary coil operates dry then the larger of the user SHR value and dry
		//      coil operation SHR is selected.

		// REFERENCES:
		// na

		// Using/Aliasing
		using CurveManager::CurveValue;
		using DataHVACGlobals::TimeStepSys;
		using DataLoopNode::Node;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const MaxIter( 30 );
		Real64 const RelaxationFactor( 0.4 );
		Real64 const Tolerance( 0.1 );
		Real64 const DryCoilTestEvapInletHumRatReset( 0.00001 );
		static std::string const RoutineName( "CalcSecondaryDXCoilsSHR" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 DryCoilTestEvapInletHumRat; // evaporator coil inlet humidity ratio test for dry coil
		Real64 DryCoilTestEvapInletWetBulb; // evaporator coil inlet dry bulb temperature test for dry coil
		Real64 FullLoadOutAirEnth; // evaporator outlet full load enthalpy [J/kg]
		Real64 FullLoadOutAirTemp; // evaporator outlet air temperature at full load [C]
		Real64 hTinwADP; // enthaly of air at secondary coil entering temperature and Humidity ratio at ADP
		Real64 SHRadp; // Sensible heat ratio
		Real64 hADP; // enthaly of air at secondary coil at ADP
		Real64 tADP; // dry bulb temperature of air at secondary coil at ADP
		Real64 wADP; // humidity ratio of air at secondary coil at ADP
		Real64 HumRatError; // humidity ratio error
		bool CoilMightBeDry; // TRUE means the secondary DX coi runs dry
		int Counter; // iteration counter
		bool Converged; // convergence flag
		Real64 SHR; // current time step sensible heat ratio of secondary coil

		CoilMightBeDry = false;
		FullLoadOutAirEnth = EvapInletEnthalpy - ( TotalHeatRemovalRate / PartLoadRatio ) / EvapAirMassFlow;
		FullLoadOutAirTemp = PsyTdbFnHW( FullLoadOutAirEnth, EvapInletHumRat );
		if ( FullLoadOutAirTemp > PsyTsatFnHPb( FullLoadOutAirEnth, OutBaroPress, RoutineName ) ) {
			CoilMightBeDry = true;
			// find wADP, humidity ratio at apparatus dewpoint and inlet hum rat that would have dry coil
			DryCoilTestEvapInletHumRat = EvapInletHumRat;
			DryCoilTestEvapInletWetBulb = EvapInletWetBulb;
			Counter = 0;
			Converged = false;
			while ( !Converged ) {
				// assumes coil bypass factor (CBF) = 0.0
				hADP = EvapInletEnthalpy - ( TotalHeatRemovalRate / PartLoadRatio ) / EvapAirMassFlow;
				tADP = PsyTsatFnHPb( hADP, OutBaroPress, RoutineName );
				wADP = min( EvapInletHumRat, PsyWFnTdbH( tADP, hADP, RoutineName ) );
				hTinwADP = PsyHFnTdbW( EvapInletDryBulb, wADP );
				if ( ( EvapInletEnthalpy - hADP ) > 1.e-10 ) {
					SHRadp = min( ( hTinwADP - hADP ) / ( EvapInletEnthalpy - hADP ), 1.0 );
				} else {
					SHRadp = 1.0;
				}
				if ( ( wADP > DryCoilTestEvapInletHumRat ) || ( Counter >= 1 && Counter < MaxIter ) ) {
					if ( DryCoilTestEvapInletHumRat <= 0.0 ) DryCoilTestEvapInletHumRat = DryCoilTestEvapInletHumRatReset;
					HumRatError = ( DryCoilTestEvapInletHumRat - wADP ) / DryCoilTestEvapInletHumRat;
					DryCoilTestEvapInletHumRat = RelaxationFactor * wADP + ( 1.0 - RelaxationFactor ) * DryCoilTestEvapInletHumRat;
					DryCoilTestEvapInletWetBulb = PsyTwbFnTdbWPb( EvapInletDryBulb, DryCoilTestEvapInletHumRat, OutBaroPress, RoutineName );
					++Counter;
					if ( std::abs( HumRatError ) <= Tolerance ) {
						Converged = true;
					} else {
						Converged = false;
					}
				} else {
					Converged = true;
				}
			}
		}
		// determine SHR from user specified nominal value and SHR modifier curves
		SHR = CalcSHRUserDefinedCurves( CondInletDryBulb, EvapInletWetBulb, SecCoilFlowFraction, SecCoilSHRFT, SecCoilSHRFF, SecCoilRatedSHR );
		if ( CoilMightBeDry ) {
			if ( ( EvapInletHumRat < DryCoilTestEvapInletHumRat ) && ( SHRadp > SHR ) ) { // coil is dry for sure
				SHR = 1.0;
			} else if ( SHRadp > SHR ) {
				SHR = SHRadp;
			}
		}
		return SHR;
	}

	void
	CalcVRFCoolingCoil_FluidTCtrl(
		int const DXCoilNum, // the number of the DX coil to be simulated
		int const CompOp, // compressor operation; 1=on, 0=off
		bool const FirstHVACIteration, // true if this is the first iteration of HVAC
		Real64 const PartLoadRatio, // sensible cooling load / full load sensible cooling capacity
		int const FanOpMode, // Allows parent object to control fan operation
		Real64 const CompCycRatio, // cycling ratio of VRF condenser
		Optional_int_const PerfMode, // Performance mode for MultiMode DX coil; Always 1 for other coil types
		Optional< Real64 const > OnOffAirFlowRatio // ratio of compressor on airflow to compressor off airflow
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Xiufeng Pang, LBNL
		//       DATE WRITTEN   Jan 2013
		//       MODIFIED       Nov 2015, RP Zhang, LBNL
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// 		Calculates the air-side performance of a direct-expansion, air-cooled
		// 		VRF terminal unit cooling coil, for the VRF_FluidTCtrl model.

		// METHODOLOGY EMPLOYED:
		// 		This subroutine is derived from CalcVRFCoolingCoil, and implements the new VRF model for FluidTCtrl.

		// REFERENCES:
		// na

		// Using/Aliasing
		using CurveManager::CurveValue;
		using DataGlobals::CurrentTime;
		using DataHVACGlobals::HPWHCrankcaseDBTemp;
		using DataHVACGlobals::TimeStepSys;
		using DataHVACGlobals::SysTimeElapsed;
		using DataWater::WaterStorage;
		using General::TrimSigDigits;
		using General::RoundSigDigits;
		using General::CreateSysTimeIntervalString;
		using HVACVariableRefrigerantFlow::OACompOnMassFlow;
		using HVACVariableRefrigerantFlow::OACompOffMassFlow;
		using namespace DataZoneEnergyDemands;
		using namespace HVACVariableRefrigerantFlow;

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcVRFCoolingCoil_FluidTCtrl" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		Real64 AirMassFlow; // dry air mass flow rate through coil [kg/s] (adjusted for bypass if any)
		Real64 AirVolumeFlowRate; // Air volume flow rate across the cooling coil [m3/s] (adjusted for bypass if any)
		// (average flow if cycling fan, full flow if constant fan)
		Real64 VolFlowperRatedTotCap; // Air volume flow rate divided by rated total cooling capacity [m3/s-W] (adjusted for bypass)
		Real64 TotCap; // gross total cooling capacity at off-rated conditions [W]
		Real64 InletAirDryBulbTemp; // inlet air dry bulb temperature [C]
		Real64 InletAirEnthalpy; // inlet air enthalpy [J/kg]
		Real64 InletAirHumRat; // inlet air humidity ratio [kg/kg]
		Real64 InletAirHumRatTemp; // inlet air humidity ratio used in ADP/BF loop [kg/kg]
		//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
		Real64 RatedCBF; // coil bypass factor at rated conditions
		Real64 SHR; // Sensible Heat Ratio (sensible/total) of the cooling coil
		Real64 CBF; // coil bypass factor at off rated conditions
		Real64 A0; // NTU * air mass flow rate, used in CBF calculation
		Real64 hDelta; // Change in air enthalpy across the cooling coil [J/kg]
		Real64 hADP; // Apparatus dew point enthalpy [J/kg]
		Real64 hTinwADP; // Enthalpy at inlet dry-bulb and wADP [J/kg]
		Real64 PLF; // Part load factor, accounts for thermal lag at compressor startup, used in power calculation
		Real64 CondInletTemp; // Condenser inlet temperature (C). Outdoor dry-bulb temp for air-cooled condenser.
		// Outdoor Wetbulb +(1 - effectiveness)*(outdoor drybulb - outdoor wetbulb) for evap condenser.
		Real64 CondInletHumRat; // Condenser inlet humidity ratio (kg/kg). Zero for air-cooled condenser.
		// For evap condenser, its the humidity ratio of the air leaving the evap cooling pads.
		Real64 CondAirMassFlow; // Condenser air mass flow rate [kg/s]
		Real64 RhoAir; // Density of air [kg/m3]
		Real64 CrankcaseHeatingPower; // power due to crankcase heater
		static Real64 CompAmbTemp( 0.0 ); // Ambient temperature at compressor
		Real64 AirFlowRatio; // ratio of compressor on airflow to average timestep airflow
		// used when constant fan mode yields different air flow rates when compressor is ON and OFF
		// (e.g. Packaged Terminal Heat Pump)
		Real64 OutdoorDryBulb; // Outdoor dry-bulb temperature at condenser (C)
		Real64 OutdoorWetBulb; // Outdoor wet-bulb temperature at condenser (C)
		Real64 OutdoorHumRat; // Outdoor humidity ratio at condenser (kg/kg)
		Real64 OutdoorPressure; // Outdoor barometric pressure at condenser (Pa)
		Real64 tADP; // Apparatus dew point temperature [C]
		Real64 wADP; // Apparatus dew point humidity ratio [kg/kg]

		static Real64 CurrentEndTime( 0.0 ); // end time of time step for current simulation time step
		static Real64 MinAirHumRat( 0.0 ); // minimum of the inlet air humidity ratio and the outlet air humidity ratio
		int Mode; // Performance mode for Multimode DX coil; Always 1 for other coil types
		Real64 OutletAirTemp; // Supply air temperature (average value if constant fan, full output if cycling fan)
		Real64 OutletAirHumRat; // Supply air humidity ratio (average value if constant fan, full output if cycling fan)
		Real64 OutletAirEnthalpy; // Supply air enthalpy (average value if constant fan, full output if cycling fan)
		Real64 ADiff; // Used for exponential

		// Followings for VRF FluidTCtrl Only
		Real64 QCoilReq; // Coil load (W)
		Real64 FanSpdRatio; // Fan speed ratio
		Real64 AirMassFlowMin; // Min air mass flow rate due to OA requirement [kg/s]
		Real64 ActualSH; // Super heating degrees (C)
		Real64 ActualSC; // Sub cooling degrees (C)

		// If Performance mode not present, then set to 1.  Used only by Multimode/Multispeed DX coil (otherwise mode = 1)
		if ( present( PerfMode ) ) {
			Mode = PerfMode;
		} else {
			Mode = 1;
		}

		// If AirFlowRatio not present, then set to 1. Used only by DX coils with different air flow
		// during cooling and when no cooling is required (constant fan, fan speed changes)
		if ( present( OnOffAirFlowRatio ) ) {
			AirFlowRatio = OnOffAirFlowRatio;
		} else {
			AirFlowRatio = 1.0;
		}

		// Initialize coil air side parameters
		CondInletTemp = 0.0;
		CondInletHumRat = 0.0;
		AirMassFlow = DXCoil( DXCoilNum ).InletAirMassFlowRate;
		InletAirDryBulbTemp = DXCoil( DXCoilNum ).InletAirTemp;
		InletAirEnthalpy = DXCoil( DXCoilNum ).InletAirEnthalpy;
		InletAirHumRat = DXCoil( DXCoilNum ).InletAirHumRat;
		HeatReclaimDXCoil( DXCoilNum ).AvailCapacity = 0.0;
		DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction = 0.0;
		DXCoil( DXCoilNum ).PartLoadRatio = 0.0;
		DXCoil( DXCoilNum ).BasinHeaterPower = 0.0;
		DXCoil( DXCoilNum ).EvaporatingTemp = VRF( DXCoil( DXCoilNum ).VRFOUPtr ).IUEvaporatingTemp;

		if ( DXCoil( DXCoilNum ).CondenserInletNodeNum( Mode ) != 0 ) {
			OutdoorDryBulb = Node( DXCoil( DXCoilNum ).CondenserInletNodeNum( Mode ) ).Temp;
			if ( DXCoil( DXCoilNum ).CondenserType( Mode ) == WaterCooled ) {
				OutdoorHumRat = OutHumRat;
				OutdoorPressure = OutBaroPress;
				OutdoorWetBulb = OutWetBulbTemp;
			} else {
				OutdoorPressure = Node( DXCoil( DXCoilNum ).CondenserInletNodeNum( Mode ) ).Press;
				// If node is not connected to anything, pressure = default, use weather data
				if ( OutdoorPressure == DefaultNodeValues.Press ) {
					OutdoorDryBulb = OutDryBulbTemp;
					OutdoorHumRat = OutHumRat;
					OutdoorPressure = OutBaroPress;
					OutdoorWetBulb = OutWetBulbTemp;
				} else {
					OutdoorHumRat = Node( DXCoil( DXCoilNum ).CondenserInletNodeNum( Mode ) ).HumRat;
					OutdoorWetBulb = Node( DXCoil( DXCoilNum ).CondenserInletNodeNum( Mode ) ).OutAirWetBulb;
				}
			}
		} else {
			OutdoorDryBulb = OutDryBulbTemp;
			OutdoorHumRat = OutHumRat;
			OutdoorPressure = OutBaroPress;
			OutdoorWetBulb = OutWetBulbTemp;
		}

		if ( DXCoil( DXCoilNum ).CondenserType( Mode ) == EvapCooled ) {
			RhoAir = PsyRhoAirFnPbTdbW( OutdoorPressure, OutdoorDryBulb, OutdoorHumRat );
			CondAirMassFlow = RhoAir * DXCoil( DXCoilNum ).EvapCondAirFlow( Mode );
			// (Outdoor wet-bulb temp from DataEnvironment) + (1.0-EvapCondEffectiveness) * (drybulb - wetbulb)
			CondInletTemp = OutdoorWetBulb + ( OutdoorDryBulb - OutdoorWetBulb ) * ( 1.0 - DXCoil( DXCoilNum ).EvapCondEffect( Mode ) );
			CondInletHumRat = PsyWFnTdbTwbPb( CondInletTemp, OutdoorWetBulb, OutdoorPressure );
			CompAmbTemp = OutdoorDryBulb;
		} else { // for air or water-cooled, inlet temp is stored in OutdoorDryBulb temp
			CondInletTemp = OutdoorDryBulb; // Outdoor dry-bulb temp or water inlet temp
			if ( DXCoil( DXCoilNum ).CondenserType( Mode ) == WaterCooled ) {
				CompAmbTemp = OutDryBulbTemp; // for crankcase heater use actual outdoor temp for water-cooled
			} else {
				CompAmbTemp = OutdoorDryBulb;
			}
		}

		// Initialize crankcase heater, operates below OAT defined in input deck for HP DX cooling coil
		// If used in a heat pump, the value of MaxOAT in the heating coil overrides that in the cooling coil (in GetInput)
		if ( CompAmbTemp < DXCoil( DXCoilNum ).MaxOATCrankcaseHeater ) {
			CrankcaseHeatingPower = DXCoil( DXCoilNum ).CrankcaseHeaterCapacity;
		} else {
			CrankcaseHeatingPower = 0.0;
		}

		// calculate end time of current time step to determine if error messages should be printed
		CurrentEndTime = CurrentTime + SysTimeElapsed;

		//   Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
		//   Wait for next time step to print warnings. If simulation iterates, print out
		//   the warning for the last iteration only. Must wait for next time step to accomplish this.
		//   If a warning occurs and the simulation down shifts, the warning is not valid.
		if ( DXCoil( DXCoilNum ).PrintLowAmbMessage ) { // .AND. &
			if ( CurrentEndTime > DXCoil( DXCoilNum ).CurrentEndTimeLast && TimeStepSys >= DXCoil( DXCoilNum ).TimeStepSysLast ) {
				if ( DXCoil( DXCoilNum ).LowAmbErrIndex == 0 ) {
					ShowWarningMessage( DXCoil( DXCoilNum ).LowAmbBuffer1 );
					ShowContinueError( DXCoil( DXCoilNum ).LowAmbBuffer2 );
					ShowContinueError( "... Operation at low inlet temperatures may require special performance curves." );
				}
				ShowRecurringWarningErrorAtEnd( DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + "\" - Low condenser inlet temperature error continues...", DXCoil( DXCoilNum ).LowAmbErrIndex, DXCoil( DXCoilNum ).LowTempLast, DXCoil( DXCoilNum ).LowTempLast, _, "[C]", "[C]" );
			}
		}

		if ( DXCoil( DXCoilNum ).PrintHighAmbMessage ) { // .AND. &
			if ( CurrentEndTime > DXCoil( DXCoilNum ).CurrentEndTimeLast && TimeStepSys >= DXCoil( DXCoilNum ).TimeStepSysLast ) {
				if ( DXCoil( DXCoilNum ).HighAmbErrIndex == 0 ) {
					ShowWarningMessage( DXCoil( DXCoilNum ).HighAmbBuffer1 );
					ShowContinueError( DXCoil( DXCoilNum ).HighAmbBuffer2 );
					ShowContinueError( "... Operation at high inlet temperatures may require special performance curves." );
				}
				ShowRecurringWarningErrorAtEnd( DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + "\" - High condenser inlet temperature error continues...", DXCoil( DXCoilNum ).HighAmbErrIndex, DXCoil( DXCoilNum ).HighTempLast, DXCoil( DXCoilNum ).HighTempLast, _, "[C]", "[C]" );
			}
		}

		if ( DXCoil( DXCoilNum ).PrintLowOutTempMessage ) {
			if ( CurrentEndTime > DXCoil( DXCoilNum ).CurrentEndTimeLast && TimeStepSys >= DXCoil( DXCoilNum ).TimeStepSysLast ) {
				if ( DXCoil( DXCoilNum ).LowOutletTempIndex == 0 ) {
					ShowWarningMessage( DXCoil( DXCoilNum ).LowOutTempBuffer1 );
					ShowContinueError( DXCoil( DXCoilNum ).LowOutTempBuffer2 );
					ShowContinueError( "... Possible reasons for low outlet air dry-bulb temperatures are: This DX coil" );
					ShowContinueError( "   1) may have a low inlet air dry-bulb temperature. Inlet air temperature = " + TrimSigDigits( DXCoil( DXCoilNum ).FullLoadInletAirTempLast, 3 ) + " C." );
					ShowContinueError( "   2) may have a low air flow rate per watt of cooling capacity. Check inputs." );
					ShowContinueError( "   3) is used as part of a HX assisted cooling coil which uses a high sensible effectiveness. Check inputs." );
				}
				ShowRecurringWarningErrorAtEnd( DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + "\" - Full load outlet temperature indicates a possibility of frost/freeze error continues. Outlet air temperature statistics follow:", DXCoil( DXCoilNum ).LowOutletTempIndex, DXCoil( DXCoilNum ).FullLoadOutAirTempLast, DXCoil( DXCoilNum ).FullLoadOutAirTempLast );
			}
		}

		// save last system time step and last end time of current time step (used to determine if warning is valid)
		DXCoil( DXCoilNum ).TimeStepSysLast = TimeStepSys;
		DXCoil( DXCoilNum ).CurrentEndTimeLast = CurrentEndTime;
		DXCoil( DXCoilNum ).PrintLowAmbMessage = false;
		DXCoil( DXCoilNum ).PrintLowOutTempMessage = false;

		if ( ( AirMassFlow > 0.0 ) && ( GetCurrentScheduleValue( DXCoil( DXCoilNum ).SchedPtr ) > 0.0 ) && ( PartLoadRatio > 0.0 ) && ( CompOp == On ) ) { // for cycling fan, reset mass flow to full on rate

			if ( DXCoil( DXCoilNum ).RatedTotCap( Mode ) <= 0.0 ) {
				ShowFatalError( DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + "\" - Rated total cooling capacity is zero or less." );
			}

			TotCap = DXCoil( DXCoilNum ).RatedTotCap( Mode );
			QCoilReq = -PartLoadRatio * TotCap;
			if( PartLoadRatio == 0.0 ){
				AirMassFlowMin = OACompOffMassFlow;
			} else {
				AirMassFlowMin = OACompOnMassFlow;
			}

			// Call ControlVRFIUCoil to calculate: (1) FanSpdRatio, (2) coil inlet/outlet conditions, and (3) SH/SC
			ControlVRFIUCoil( DXCoilNum, QCoilReq, DXCoil( DXCoilNum ).InletAirTemp, DXCoil( DXCoilNum ).InletAirHumRat, DXCoil( DXCoilNum ).EvaporatingTemp, AirMassFlowMin, FanSpdRatio, OutletAirHumRat, OutletAirTemp, OutletAirEnthalpy, ActualSH, ActualSC );
			AirMassFlow = FanSpdRatio * DXCoil( DXCoilNum ).RatedAirMassFlowRate( Mode );

			AirVolumeFlowRate = AirMassFlow / PsyRhoAirFnPbTdbW( OutdoorPressure, InletAirDryBulbTemp, InletAirHumRat );
			VolFlowperRatedTotCap = AirVolumeFlowRate / DXCoil( DXCoilNum ).RatedTotCap( Mode );
			// VolFlowperRatedTotCap was checked at the initialization step
			// No need to check VolFlowperRatedTotCap at the simulation
			// New VRF_FluidTCtrl model implements VAV fan which can vary air flow rate during simulation

			RatedCBF = DXCoil( DXCoilNum ).RatedCBF( Mode );
			if ( RatedCBF > 0.0 ) {
				A0 = -std::log( RatedCBF ) * DXCoil( DXCoilNum ).RatedAirMassFlowRate( Mode );
			} else {
				A0 = 0.0;
			}
			ADiff = -A0 / AirMassFlow;
			if ( ADiff >= EXP_LowerLimit ) {
				CBF = std::exp( ADiff );
			} else {
				CBF = 0.0;
			}

			// check boundary for low ambient temperature and post warnings to individual DX coil buffers to print at end of time step
			if ( OutdoorDryBulb < DXCoil( DXCoilNum ).MinOATCompressor && ! WarmupFlag ) {
				DXCoil( DXCoilNum ).PrintLowAmbMessage = true;
				DXCoil( DXCoilNum ).LowTempLast = OutdoorDryBulb;
				if ( DXCoil( DXCoilNum ).LowAmbErrIndex == 0 ) {
					DXCoil( DXCoilNum ).LowAmbBuffer1 = DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + "\" - Condenser inlet temperature below " + RoundSigDigits( DXCoil( DXCoilNum ).MinOATCompressor, 2 ) + " C. Condenser inlet temperature = " + RoundSigDigits( OutdoorDryBulb, 2 );
					DXCoil( DXCoilNum ).LowAmbBuffer2 = " ... Occurrence info = " + EnvironmentName + ", " + CurMnDy + " " + CreateSysTimeIntervalString();
				}
			}

			// check boundary for high ambient temperature and post warnings to individual DX coil buffers to print at end of time step
			if ( OutdoorDryBulb > DXCoil( DXCoilNum ).MaxOATCompressor && ! WarmupFlag ) {
				DXCoil( DXCoilNum ).PrintHighAmbMessage = true;
				DXCoil( DXCoilNum ).HighTempLast = OutdoorDryBulb;
				if ( DXCoil( DXCoilNum ).HighAmbErrIndex == 0 ) {
					DXCoil( DXCoilNum ).HighAmbBuffer1 = DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + "\" - Condenser inlet temperature above " + RoundSigDigits( DXCoil( DXCoilNum ).MaxOATCompressor, 2 ) + " C. Condenser temperature = " + RoundSigDigits( OutdoorDryBulb, 2 );
					DXCoil( DXCoilNum ).HighAmbBuffer2 = " ... Occurrence info = " + EnvironmentName + ", " + CurMnDy + " " + CreateSysTimeIntervalString();
				}
			}

			//  Get total capacity modifying factor (function of temperature) for off-rated conditions
			//  InletAirHumRat may be modified in this ADP/BF loop, use temporary varible for calculations
			InletAirHumRatTemp = InletAirHumRat;

			// Calculate apparatus dew point conditions using TotCap and CBF
			hDelta = TotCap / AirMassFlow;
			// there is an issue here with using CBF to calculate the ADP enthalpy.
			// at low loads the bypass factor increases significantly.
			hADP = InletAirEnthalpy - hDelta / ( 1.0 - CBF );
			tADP = PsyTsatFnHPb( hADP, OutdoorPressure, RoutineName );
			//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
			//  tADP = PsyTsatFnHPb(hADP,InletAirPressure)
			wADP = min( InletAirHumRat, PsyWFnTdbH( tADP, hADP, RoutineName ) );
			hTinwADP = PsyHFnTdbW( InletAirDryBulbTemp, wADP );
			if ( ( InletAirEnthalpy - hADP ) > 1.e-10 ) {
				SHR = min( ( hTinwADP - hADP ) / ( InletAirEnthalpy - hADP ), 1.0 );
			} else {
				SHR = 1.0;
			}

			if ( DXCoil( DXCoilNum ).PLFFPLR( Mode ) > 0 && CompCycRatio < 1.0 ) {
				PLF = CurveValue( DXCoil( DXCoilNum ).PLFFPLR( Mode ), CompCycRatio ); // Calculate part-load factor
			} else {
				PLF = 1.0;
			}

			if ( PLF < 0.7 ) {
				if ( DXCoil( DXCoilNum ).ErrIndex2 == 0 ) {
					ShowWarningMessage( "The PLF curve value for the DX cooling coil " + DXCoil( DXCoilNum ).Name + " =" + RoundSigDigits( PLF, 3 ) + " for part-load ratio =" + RoundSigDigits( PartLoadRatio, 3 ) );
					ShowContinueErrorTimeStamp( "PLF curve values must be >= 0.7. PLF has been reset to 0.7 and simulation is continuing." );
					ShowContinueError( "Check the IO reference manual for PLF curve guidance [Coil:Cooling:DX:SingleSpeed]." );
				}
				ShowRecurringWarningErrorAtEnd( DXCoil( DXCoilNum ).Name + ", DX cooling coil PLF curve < 0.7 warning continues...", DXCoil( DXCoilNum ).ErrIndex2, PLF, PLF );
				PLF = 0.7;
			}

			DXCoil( DXCoilNum ).PartLoadRatio = PartLoadRatio;
			DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction = CompCycRatio / PLF;
			if ( DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction > 1.0 && std::abs( DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction - 1.0 ) > 0.001 ) {
				if ( DXCoil( DXCoilNum ).ErrIndex3 == 0 ) {
					ShowWarningMessage( "The runtime fraction for DX cooling coil " + DXCoil( DXCoilNum ).Name + " exceeded 1.0. [" + RoundSigDigits( DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction, 4 ) + "]." );
					ShowContinueError( "Runtime fraction reset to 1 and the simulation will continue." );
					ShowContinueError( "Check the IO reference manual for PLF curve guidance [Coil:Cooling:DX:SingleSpeed]." );
					ShowContinueErrorTimeStamp( "" );
				}
				ShowRecurringWarningErrorAtEnd( DXCoil( DXCoilNum ).Name + ", DX cooling coil runtime fraction > 1.0 warning continues...", DXCoil( DXCoilNum ).ErrIndex3, DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction, DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction );
				DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction = 1.0; // Reset coil runtime fraction to 1.0
			} else if ( DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction > 1.0 ) {
				DXCoil( DXCoilNum ).CoolingCoilRuntimeFraction = 1.0; // Reset coil runtime fraction to 1.0
			}

			// If cycling fan, send coil part-load fraction to on/off fan via HVACDataGlobals
			if ( FanOpMode == CycFanCycCoil ) OnOffFanPartLoadFraction = PLF;

			// Check for saturation error and modify temperature at constant enthalpy
			if ( OutletAirTemp < PsyTsatFnHPb( OutletAirEnthalpy, OutdoorPressure ) ) {
				OutletAirTemp = PsyTsatFnHPb( OutletAirEnthalpy, OutdoorPressure );
				//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
				//   IF(FullLoadOutAirTemp .LT. PsyTsatFnHPb(FullLoadOutAirEnth,InletAirPressure)) THEN
				//    FullLoadOutAirTemp = PsyTsatFnHPb(FullLoadOutAirEnth,InletAirPressure)
				OutletAirHumRat = PsyWFnTdbH( OutletAirTemp, OutletAirEnthalpy );
			}

			// Store actual outlet conditions when DX coil is ON for use in heat recovery module
			DXCoilFullLoadOutAirTemp( DXCoilNum ) = OutletAirTemp;
			DXCoilFullLoadOutAirHumRat( DXCoilNum ) = OutletAirHumRat;

			// Add warning message for cold cooling coil (OutletAirTemp < 2 C)
			if ( OutletAirTemp < 2.0 && ! FirstHVACIteration && ! WarmupFlag ) {
				DXCoil( DXCoilNum ).PrintLowOutTempMessage = true;
				DXCoil( DXCoilNum ).FullLoadOutAirTempLast = OutletAirTemp;
				if ( DXCoil( DXCoilNum ).LowOutletTempIndex == 0 ) {
					DXCoil( DXCoilNum ).FullLoadInletAirTempLast = InletAirDryBulbTemp;
					DXCoil( DXCoilNum ).LowOutTempBuffer1 = DXCoil( DXCoilNum ).DXCoilType + " \"" + DXCoil( DXCoilNum ).Name + "\" - Full load outlet air dry-bulb temperature < 2C. This indicates the possibility of coil frost/freeze. Outlet temperature = " + RoundSigDigits( OutletAirTemp, 2 ) + " C.";
					DXCoil( DXCoilNum ).LowOutTempBuffer2 = " ...Occurrence info = " + EnvironmentName + ", " + CurMnDy + " " + CreateSysTimeIntervalString();
				}
			}

			// Coil total cooling
			DXCoil( DXCoilNum ).TotalCoolingEnergyRate = AirMassFlow * ( InletAirEnthalpy - OutletAirEnthalpy );

			// Coil sensible cooling
			MinAirHumRat = min( InletAirHumRat, OutletAirHumRat );
			DXCoil( DXCoilNum ).SensCoolingEnergyRate = AirMassFlow * 1005.0 * ( InletAirDryBulbTemp - OutletAirTemp );
			//  Don't let sensible capacity be greater than total capacity
			if ( DXCoil( DXCoilNum ).SensCoolingEnergyRate > DXCoil( DXCoilNum ).TotalCoolingEnergyRate ) {
				DXCoil( DXCoilNum ).SensCoolingEnergyRate = DXCoil( DXCoilNum ).TotalCoolingEnergyRate;
			}

			// Coil latent cooling
			DXCoil( DXCoilNum ).LatCoolingEnergyRate = DXCoil( DXCoilNum ).TotalCoolingEnergyRate - DXCoil( DXCoilNum ).SensCoolingEnergyRate;

			// Coil outlet conditions
			DXCoil( DXCoilNum ).OutletAirTemp = OutletAirTemp;
			DXCoil( DXCoilNum ).OutletAirHumRat = OutletAirHumRat;
			DXCoil( DXCoilNum ).OutletAirEnthalpy = OutletAirEnthalpy;

			// Coil SH/SC
			DXCoil( DXCoilNum ).ActualSH = ActualSH;
			DXCoil( DXCoilNum ).ActualSC = ActualSC;

		} else {
		// DX coil is off; just pass through conditions

			DXCoil( DXCoilNum ).OutletAirEnthalpy = DXCoil( DXCoilNum ).InletAirEnthalpy;
			DXCoil( DXCoilNum ).OutletAirHumRat = DXCoil( DXCoilNum ).InletAirHumRat;
			DXCoil( DXCoilNum ).OutletAirTemp = DXCoil( DXCoilNum ).InletAirTemp;

			DXCoil( DXCoilNum ).ElecCoolingPower = 0.0;
			DXCoil( DXCoilNum ).TotalCoolingEnergyRate = 0.0;
			DXCoil( DXCoilNum ).SensCoolingEnergyRate = 0.0;
			DXCoil( DXCoilNum ).LatCoolingEnergyRate = 0.0;
			DXCoil( DXCoilNum ).EvapCondPumpElecPower = 0.0;
			DXCoil( DXCoilNum ).EvapWaterConsumpRate = 0.0;

			DXCoil( DXCoilNum ).ActualSH = 999.0;
			DXCoil( DXCoilNum ).ActualSC = 999.0;

			// Reset globals when DX coil is OFF for use in heat recovery module
			DXCoilFullLoadOutAirTemp( DXCoilNum ) = 0.0;
			DXCoilFullLoadOutAirHumRat( DXCoilNum ) = 0.0;

		} // end of on/off

		//set water system demand request (if needed)
		if ( DXCoil( DXCoilNum ).EvapWaterSupplyMode == WaterSupplyFromTank ) {
			WaterStorage( DXCoil( DXCoilNum ).EvapWaterSupTankID ).VdotRequestDemand( DXCoil( DXCoilNum ).EvapWaterTankDemandARRID ) = DXCoil( DXCoilNum ).EvapWaterConsumpRate;
		}

		DXCoilOutletTemp( DXCoilNum ) = DXCoil( DXCoilNum ).OutletAirTemp;
		DXCoilOutletHumRat( DXCoilNum ) = DXCoil( DXCoilNum ).OutletAirHumRat;
		DXCoilPartLoadRatio( DXCoilNum ) = DXCoil( DXCoilNum ).PartLoadRatio;
		DXCoilFanOpMode( DXCoilNum ) = FanOpMode;
		DXCoil( DXCoilNum ).CondInletTemp = CondInletTemp;
		DXCoilTotalCooling( DXCoilNum ) = DXCoil( DXCoilNum ).TotalCoolingEnergyRate;
		DXCoilCoolInletAirWBTemp( DXCoilNum ) = PsyTwbFnTdbWPb( InletAirDryBulbTemp, InletAirHumRat, OutdoorPressure );

	}

	void
	CalcVRFHeatingCoil_FluidTCtrl(
		int const CompOp, // compressor operation; 1=on, 0=off
		int const DXCoilNum, // the number of the DX heating coil to be simulated
		Real64 const PartLoadRatio, // sensible cooling load / full load sensible cooling capacity
		int const FanOpMode, // Allows parent object to control fan mode
		Optional< Real64 const > OnOffAirFlowRatio, // ratio of compressor on airflow to compressor off airflow
		Optional< Real64 const > MaxHeatCap // maximum allowed heating capacity
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Xiufeng Pang (XP), LBNL
		//       DATE WRITTEN   Mar 2013
		//       MODIFIED       Nov 2015, RP Zhang, LBNL
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// 		Calculates the air-side performance of a direct-expansion, air-cooled
		// 		VRF terminal unit heating coil, for the new VRF model.

		// METHODOLOGY EMPLOYED:
		// 		This subroutine is derived from CalcVRFCoolingCoil, and implements the new VRF model for FluidTCtrl.

		// REFERENCES:
		// na

		// Using/Aliasing
		using CurveManager::CurveValue;
		using General::RoundSigDigits;
		using HVACVariableRefrigerantFlow::OACompOnMassFlow;
		using HVACVariableRefrigerantFlow::OACompOffMassFlow;
		using namespace DataZoneEnergyDemands;
		using namespace HVACVariableRefrigerantFlow;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		static std::string const RoutineNameFullLoad( "CalcVRFHeatingCoil_FluidTCtrl:fullload" );

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 AirMassFlow; // dry air mass flow rate through coil [kg/s]
		Real64 AirMassFlowRatio; // Ratio of actual air mass flow to rated air mass flow
		Real64 AirVolumeFlowRate; // Air volume flow rate across the cooling coil [m3/s]
		Real64 VolFlowperRatedTotCap; // Air volume flow rate divided by rated total cooling capacity [m3/s-W]
		Real64 TotCap; // gross total cooling capacity at off-rated conditions [W]
		Real64 TotCapAdj; // adjusted total cooling capacity at off-rated conditions [W]
		// on the type of curve
		Real64 InletAirDryBulbTemp; // inlet air dry bulb temperature [C]
		Real64 InletAirWetBulbC; // wetbulb temperature of inlet air [C]
		Real64 InletAirEnthalpy; // inlet air enthalpy [J/kg]
		Real64 InletAirHumRat; // inlet air humidity ratio [kg/kg]
		Real64 FullLoadOutAirEnth; // outlet full load enthalpy [J/kg]
		Real64 FullLoadOutAirHumRat; // outlet humidity ratio at full load
		Real64 FullLoadOutAirTemp; // outlet air temperature at full load [C]
		Real64 FullLoadOutAirRH; // outlet air relative humidity at full load
		Real64 EIRTempModFac( 0.0 ); // EIR modifier (function of entering drybulb, outside drybulb) depending on the
		//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
		// type of curve
		// Real64 DefrostEIRTempModFac; // EIR modifier for defrost (function of entering wetbulb, outside drybulb)
		Real64 EIRFlowModFac; // EIR modifier (function of actual supply air flow vs rated flow)
		Real64 EIR; // EIR at part load and off rated conditions
		Real64 PLF; // Part load factor, accounts for thermal lag at compressor startup
		Real64 PLRHeating; // PartLoadRatio in heating
		Real64 OutdoorCoilT; // Outdoor coil temperature (C)
		Real64 OutdoorCoildw; // Outdoor coil delta w assuming coil temp of OutdoorCoilT (kg/kg)
		Real64 FractionalDefrostTime; // Fraction of time step system is in defrost
		Real64 HeatingCapacityMultiplier; // Multiplier for heating capacity when system is in defrost
		Real64 InputPowerMultiplier; // Multiplier for power when system is in defrost
		Real64 LoadDueToDefrost; // Additional load due to defrost
		Real64 CrankcaseHeatingPower; // power due to crankcase heater
		Real64 OutdoorDryBulb; // Outdoor dry-bulb temperature at condenser (C)
		Real64 OutdoorWetBulb; // Outdoor wet-bulb temperature at condenser (C)
		Real64 OutdoorHumRat; // Outdoor humidity ratio at condenser (kg/kg)
		Real64 OutdoorPressure; // Outdoor barometric pressure at condenser (Pa)
		static int Mode( 1 ); // Performance mode for MultiMode DX coil. Always 1 for other coil types
		Real64 AirFlowRatio; // Ratio of compressor on airflow to average timestep airflow
		Real64 OutletAirTemp; // Supply air temperature (average value if constant fan, full output if cycling fan)
		Real64 OutletAirHumRat; // Supply air humidity ratio (average value if constant fan, full output if cycling fan)
		Real64 OutletAirEnthalpy; // Supply air enthalpy (average value if constant fan, full output if cycling fan)

		// Followings for VRF FluidTCtrl Only
		Real64 QCoilReq; // Coil load (W)
		Real64 FanSpdRatio; // Fan Speed Ratio
		Real64 AirMassFlowMin; // Min air mass flow rate due to OA requirement [kg/s]
		Real64 ActualSH; // Actual Super Heating
		Real64 ActualSC; // Actual Sub Cooling

		if ( present( OnOffAirFlowRatio ) ) {
			AirFlowRatio = OnOffAirFlowRatio;
		} else {
			AirFlowRatio = 1.0;
		}

		//Air cooled condenser
		OutdoorDryBulb  = OutDryBulbTemp;
		OutdoorWetBulb  = OutWetBulbTemp;
		OutdoorHumRat   = OutHumRat;
		OutdoorPressure = OutBaroPress;

		AirMassFlow = DXCoil( DXCoilNum ).InletAirMassFlowRate;
		InletAirDryBulbTemp = DXCoil( DXCoilNum ).InletAirTemp;
		InletAirEnthalpy = DXCoil( DXCoilNum ).InletAirEnthalpy;
		InletAirHumRat = DXCoil( DXCoilNum ).InletAirHumRat;
		InletAirWetBulbC = PsyTwbFnTdbWPb( InletAirDryBulbTemp, InletAirHumRat, OutdoorPressure );
		PLRHeating = 0.0;
		DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction = 0.0;
		DXCoil( DXCoilNum ).CondensingTemp = VRF( DXCoil( DXCoilNum ).VRFOUPtr ).IUCondensingTemp;

		// Initialize crankcase heater, operates below OAT defined in input deck for HP DX heating coil
		if ( OutdoorDryBulb < DXCoil( DXCoilNum ).MaxOATCrankcaseHeater ) {
			CrankcaseHeatingPower = DXCoil( DXCoilNum ).CrankcaseHeaterCapacity;
		} else {
			CrankcaseHeatingPower = 0.0;
		}

		if ( ( AirMassFlow > 0.0 ) && ( CompOp == On ) &&
			( GetCurrentScheduleValue( DXCoil( DXCoilNum ).SchedPtr ) > 0.0 ) &&
			( PartLoadRatio > 0.0 ) && ( OutdoorDryBulb > DXCoil( DXCoilNum ).MinOATCompressor ) ) {

			TotCap = DXCoil( DXCoilNum ).RatedTotCap( Mode );
			QCoilReq = PartLoadRatio * TotCap;
			if( PartLoadRatio == 0.0 ){
				AirMassFlowMin = OACompOffMassFlow;
			} else {
				AirMassFlowMin = OACompOnMassFlow;
			}

			// Call ControlVRFIUCoil to calculate: (1) FanSpdRatio, (2) coil inlet/outlet conditions, and (3) SH/SC
			ControlVRFIUCoil( DXCoilNum, QCoilReq, DXCoil( DXCoilNum ).InletAirTemp, DXCoil( DXCoilNum ).InletAirHumRat, DXCoil( DXCoilNum ).CondensingTemp, AirMassFlowMin, FanSpdRatio, OutletAirHumRat, OutletAirTemp, OutletAirEnthalpy, ActualSH, ActualSC );
			AirMassFlow = FanSpdRatio * DXCoil( DXCoilNum ).RatedAirMassFlowRate( Mode );


			AirVolumeFlowRate = AirMassFlow / PsyRhoAirFnPbTdbW( OutdoorPressure, InletAirDryBulbTemp, InletAirHumRat );
			// Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
			VolFlowperRatedTotCap = AirVolumeFlowRate / DXCoil( DXCoilNum ).RatedTotCap( Mode );
			// VolFlowperRatedTotCap was checked at the initialization step
			// No need to check VolFlowperRatedTotCap at the simulation
			// New VRF_FluidTCtrl model implements VAV fan which can vary air flow rate during simulation

			AirMassFlowRatio = AirMassFlow / DXCoil( DXCoilNum ).RatedAirMassFlowRate( Mode );

			// Calculating adjustment factors for defrost
			// Calculate delta w through outdoor coil by assuming a coil temp of 0.82*DBT-9.7(F) per DOE2.1E
			OutdoorCoilT = 0.82 * OutdoorDryBulb - 8.589;
			OutdoorCoildw = max( 1.0e-6, ( OutdoorHumRat - PsyWFnTdpPb( OutdoorCoilT, OutdoorPressure ) ) );

			// Initializing defrost adjustment factors
			LoadDueToDefrost = 0.0;
			HeatingCapacityMultiplier = 1.0;
			FractionalDefrostTime = 0.0;
			InputPowerMultiplier = 1.0;

			// Modify total heating capacity based on defrost heating capacity multiplier
			// MaxHeatCap passed from parent object VRF Condenser and is used to limit capacity of TU's to that available from condenser
			if ( present( MaxHeatCap ) ) {
				TotCapAdj = min( MaxHeatCap, TotCap * HeatingCapacityMultiplier );
				TotCap = min( MaxHeatCap, TotCap );
			} else {
				TotCapAdj = TotCap * HeatingCapacityMultiplier;
			}

			// Calculate full load outlet conditions
			FullLoadOutAirEnth = InletAirEnthalpy + TotCapAdj / AirMassFlow;
			FullLoadOutAirHumRat = InletAirHumRat;
			FullLoadOutAirTemp = PsyTdbFnHW( FullLoadOutAirEnth, FullLoadOutAirHumRat );
			FullLoadOutAirRH = PsyRhFnTdbWPb( FullLoadOutAirTemp, FullLoadOutAirHumRat, OutdoorPressure, RoutineNameFullLoad );
			//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
			//  FullLoadOutAirRH = PsyRhFnTdbWPb(FullLoadOutAirTemp,FullLoadOutAirHumRat,InletAirPressure)
			if ( FullLoadOutAirRH > 1.0 ) { // Limit to saturated conditions at FullLoadOutAirEnth
				FullLoadOutAirTemp = PsyTsatFnHPb( FullLoadOutAirEnth, OutdoorPressure );
				//  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
				//    FullLoadOutAirTemp = PsyTsatFnHPb(FullLoadOutAirEnth,InletAirPressure)
				FullLoadOutAirHumRat = PsyWFnTdbH( FullLoadOutAirTemp, FullLoadOutAirEnth );
			}

			// Calculate electricity consumed. First, get EIR modifying factors for off-rated conditions
			// Model was extended to accept bi-quadratic curves. This allows sensitivity of the EIR
			// to the entering dry-bulb temperature as well as the outside dry-bulb temperature. User is
			// advised to use the bi-quaratic curve if sufficient manufacturer data is available.
			if ( DXCoil( DXCoilNum ).DXCoilType_Num != CoilVRF_Heating && DXCoil( DXCoilNum ).DXCoilType_Num != CoilVRF_FluidTCtrl_Heating ) {
				if ( ( DXCoil( DXCoilNum ).EIRTempModFacCurveType( 1 ) == Quadratic ) || ( DXCoil( DXCoilNum ).EIRTempModFacCurveType( 1 ) == Cubic ) ) {
					EIRTempModFac = CurveValue( DXCoil( DXCoilNum ).EIRFTemp( Mode ), OutdoorDryBulb );
				} else if ( DXCoil( DXCoilNum ).EIRTempModFacCurveType( 1 ) == BiQuadratic ) {
					EIRTempModFac = CurveValue( DXCoil( DXCoilNum ).EIRFTemp( Mode ), InletAirDryBulbTemp, OutdoorDryBulb );
				} else {
					assert( false );
				}
				EIRFlowModFac = CurveValue( DXCoil( DXCoilNum ).EIRFFlow( Mode ), AirMassFlowRatio );
			} else {
				EIRTempModFac = 1.0;
				EIRFlowModFac = 1.0;
			}
			EIR = DXCoil( DXCoilNum ).RatedEIR( Mode ) * EIRTempModFac * EIRFlowModFac;

			// Calculate PLRHeating: modified PartLoadRatio due to defrost ( reverse-cycle defrost only )
			PLRHeating = min( 1.0, ( PartLoadRatio + LoadDueToDefrost / TotCap ) );
			if ( DXCoil( DXCoilNum ).DXCoilType_Num != CoilVRF_Heating && DXCoil( DXCoilNum ).DXCoilType_Num != CoilVRF_FluidTCtrl_Heating ) {
				PLF = CurveValue( DXCoil( DXCoilNum ).PLFFPLR( Mode ), PLRHeating ); // Calculate part-load factor
			} else {
				PLF = 1.0;
			}

			if ( PLF < 0.7 ) {
				if ( DXCoil( DXCoilNum ).PLRErrIndex == 0 ) {
					ShowWarningMessage( "The PLF curve value for DX heating coil " + DXCoil( DXCoilNum ).Name + " =" + RoundSigDigits( PLF, 2 ) + " for part-load ratio =" + RoundSigDigits( PLRHeating, 2 ) );
					ShowContinueError( "PLF curve values must be >= 0.7. PLF has been reset to 0.7 and simulation is continuing." );
					ShowContinueError( "Check the IO reference manual for PLF curve guidance [Coil:Heating:DX:SingleSpeed]." );
					ShowContinueErrorTimeStamp( "" );
				}
				ShowRecurringWarningErrorAtEnd( "DX heating coil PLF curve < 0.7 warning continues... ", DXCoil( DXCoilNum ).PLRErrIndex, PLF, PLF );
				PLF = 0.7;
			}

			DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction = ( PLRHeating / PLF );
			if ( DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction > 1.0 && std::abs( DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction - 1.0 ) > 0.001 ) {
				if ( DXCoil( DXCoilNum ).ErrIndex4 == 0 ) {
					ShowWarningMessage( "The runtime fraction for DX heating coil " + DXCoil( DXCoilNum ).Name + " exceeded 1.0. [" + RoundSigDigits( DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction, 4 ) + "]." );
					ShowContinueError( "Runtime fraction is set to 1.0 and the simulation continues..." );
					ShowContinueError( "Check the IO reference manual for PLF curve guidance [Coil:Heating:DX:SingleSpeed]." );
					ShowContinueErrorTimeStamp( "" );
				}
				ShowRecurringWarningErrorAtEnd( DXCoil( DXCoilNum ).Name + ", DX heating coil runtime fraction > 1.0 warning continues...", DXCoil( DXCoilNum ).ErrIndex4, DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction, DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction );
				DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction = 1.0; // Reset coil runtime fraction to 1.0
			} else if ( DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction > 1.0 ) {
				DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction = 1.0; // Reset coil runtime fraction to 1.0
			}

			// if cycling fan, send coil part-load fraction to on / off fan via HVACDataGlobals
			if ( FanOpMode == CycFanCycCoil ) OnOffFanPartLoadFraction = PLF;
			DXCoil( DXCoilNum ).ElecHeatingPower = TotCap * EIR * DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction * InputPowerMultiplier;

			// Calculate crankcase heater power using the runtime fraction for this DX heating coil only if there is no companion DX coil.
			// Else use the largest runtime fraction of this DX heating coil and the companion DX cooling coil.
			if ( DXCoil( DXCoilNum ).CompanionUpstreamDXCoil == 0 ) {
				DXCoil( DXCoilNum ).CrankcaseHeaterPower = CrankcaseHeatingPower * ( 1.0 - DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction );
			} else {
				DXCoil( DXCoilNum ).CrankcaseHeaterPower = CrankcaseHeatingPower * ( 1.0 - max( DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction, DXCoil( DXCoil( DXCoilNum ).CompanionUpstreamDXCoil ).CoolingCoilRuntimeFraction ) );
			}

			DXCoil( DXCoilNum ).OutletAirTemp = OutletAirTemp;
			DXCoil( DXCoilNum ).OutletAirHumRat = OutletAirHumRat;
			DXCoil( DXCoilNum ).OutletAirEnthalpy = OutletAirEnthalpy;
			DXCoil( DXCoilNum ).CompressorPartLoadRatio = PartLoadRatio;
			DXCoil( DXCoilNum ).ActualSH = ActualSH;
			DXCoil( DXCoilNum ).ActualSC = ActualSC;
			DXCoil( DXCoilNum ).TotalHeatingEnergyRate = AirMassFlow * ( OutletAirEnthalpy - InletAirEnthalpy );
			DXCoil( DXCoilNum ).DefrostPower = DXCoil( DXCoilNum ).DefrostPower * DXCoil( DXCoilNum ).HeatingCoilRuntimeFraction;

		} else {
		// DX coil is off; just pass through conditions

			DXCoil( DXCoilNum ).OutletAirEnthalpy = DXCoil( DXCoilNum ).InletAirEnthalpy;
			DXCoil( DXCoilNum ).OutletAirHumRat = DXCoil( DXCoilNum ).InletAirHumRat;
			DXCoil( DXCoilNum ).OutletAirTemp = DXCoil( DXCoilNum ).InletAirTemp;

			DXCoil( DXCoilNum ).ElecHeatingPower = 0.0;
			DXCoil( DXCoilNum ).TotalHeatingEnergyRate = 0.0;
			DXCoil( DXCoilNum ).DefrostPower = 0.0;

			// Calculate crankcase heater power using the runtime fraction for this DX heating coil (here DXHeatingCoilRTF=0) if
			// there is no companion DX coil, or the runtime fraction of the companion DX cooling coil (here DXCoolingCoilRTF>=0).
			if ( DXCoil( DXCoilNum ).CompanionUpstreamDXCoil == 0 ) {
				DXCoil( DXCoilNum ).CrankcaseHeaterPower = CrankcaseHeatingPower;
			} else {
				DXCoil( DXCoilNum ).CrankcaseHeaterPower = CrankcaseHeatingPower * ( 1.0 - DXCoil( DXCoil( DXCoilNum ).CompanionUpstreamDXCoil ).CoolingCoilRuntimeFraction );
			}
			DXCoil( DXCoilNum ).CompressorPartLoadRatio = 0.0;

			DXCoil( DXCoilNum ).ActualSH = 999.0;
			DXCoil( DXCoilNum ).ActualSC = 999.0;
		} // end of on/off if - else

		DXCoilOutletTemp( DXCoilNum ) = DXCoil( DXCoilNum ).OutletAirTemp;
		DXCoilOutletHumRat( DXCoilNum ) = DXCoil( DXCoilNum ).OutletAirHumRat;
		DXCoilFanOpMode( DXCoilNum ) = FanOpMode;
		DXCoilPartLoadRatio( DXCoilNum ) = PLRHeating;
		DXCoilTotalHeating( DXCoilNum ) = DXCoil( DXCoilNum ).TotalHeatingEnergyRate;
		DXCoilHeatInletAirDBTemp( DXCoilNum ) = InletAirDryBulbTemp;
		DXCoilHeatInletAirWBTemp( DXCoilNum ) = InletAirWetBulbC;

		// calc secondary coil if specified
		if ( DXCoil( DXCoilNum ).IsSecondaryDXCoilInZone ) {
			CalcSecondaryDXCoils( DXCoilNum );
		}
	}

	void
	ControlVRFIUCoil(
		int const CoilIndex,  // index to VRFTU coil
		Real64 const QCoil,   // coil load
		Real64 const Tin, // inlet air temperature
		Real64 const Win, // inlet air humidity ratio
		Real64 const TeTc,    // evaporating or condensing temperature
		Real64 const OAMassFlow,  // mass flow rate of outdoor air
		Real64 & FanSpdRatio, // fan speed ratio: actual flow rate / rated flow rate
		Real64 & Wout,    // outlet air humidity ratio
		Real64 & Tout, // outlet air temperature
		Real64 & Hout, // outlet air enthalpy
		Real64 & SHact,   // actual SH
		Real64 & SCact    // actual SC
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Xiufeng Pang, LBNL
		//       DATE WRITTEN   Feb 2013
		//       MODIFIED       Nov 2015, RP Zhang, LBNL
		//
		//       RE-ENGINEERED  na
		//
		// PURPOSE OF THIS SUBROUTINE:
		//       Analyze the VRF Indoor Unit operations given coil loads.
		//       Calculated parameters include: (1) Fan Speed Ratio (2) SH/SC, (3) Coil Outlet conditions
		//
		// METHODOLOGY EMPLOYED:
		//       A new physics based VRF model applicable for Fluid Temperature Control.
		//
		// REFERENCES:
		//       na
		//
		// USE STATEMENTS:
		using namespace DataZoneEnergyDemands;
		using General::SolveRegulaFalsi;
		using Psychrometrics::PsyHFnTdbW;

		// SUBROUTINE PARAMETER DEFINITIONS:
		//
		// INTERFACE BLOCK SPECIFICATIONS
		// na
		//
		// DERIVED TYPE DEFINITIONS
		// na
		//
		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Array1D< Real64 > Par( 11 ); // Parameter array for SolveRegulaFalsi
		int MaxIter( 500 ); // Max iteration numbers (-)
		int SolFla; // Solving flag for SolveRegulaFalsi (-)
		int const FlagCoolMode( 0 ); // Flag for cooling mode
		int const FlagHeatMode( 1 ); // Flag for heating mode
		Real64 BF; // Bypass factor (-)
		Real64 C1Tevap; // Coefficient for indoor unit coil evaporating temperature curve (-)
		Real64 C2Tevap; // Coefficient for indoor unit coil evaporating temperature curve (-)
		Real64 C3Tevap; // Coefficient for indoor unit coil evaporating temperature curve (-)
		Real64 C1Tcond; // Coefficient for indoor unit coil condensing temperature curve (-)
		Real64 C2Tcond; // Coefficient for indoor unit coil condensing temperature curve (-)
		Real64 C3Tcond; // Coefficient for indoor unit coil condensing temperature curve (-)
		Real64 CoilOnOffRatio; // coil on/off ratio: time coil is on divided by total time
		Real64 deltaT; // Difference between evaporating/condensing temperature and coil surface temperature (C)
		Real64 FanSpdRatioMin; // Min fan speed ratio, below which the cycling will be activated (-)
		Real64 FanSpdRatioMax; // Max fan speed ratio (-)
		Real64 Garate; // Nominal air mass flow rate (m3/s)
		Real64 MaxSH; // Max super heating degrees (C)
		Real64 MaxSC; // Max subcooling degrees (C)
		Real64 QinSenMin1; //Coil capacity at minimum fan speed, corresponding to real SH (W)
		Real64 QinSenMin2; //Coil capacity at minimum fan speed, corresponding to corresponds maximum SH (W)
		Real64 QinSenPerFlowRate; //Coil capacity per air mass flow rate(W-s/kg)
		Real64 QCoilSenCoolingLoad; // Coil sensible cooling load (W)
		Real64 QCoilSenHeatingLoad; // Coil sensible heating load (W)
		Real64 Ratio1; // Fan speed ratio (-)
		Real64 RHsat; // Relative humidity of the air at saturated condition(-)
		Real64 SH; // Super heating degrees (C)
		Real64 SC; // Subcooling degrees (C)
		Real64 Ts_1; // Air temperature at the coil surface, corresponding to SH (C)
		Real64 Ts_2; // Air temperature at the coil surface, corresponding to MaxSH (C)
		Real64 To_1; // Air temperature at the coil outlet, corresponding to SH (C)
		Real64 To_2; // Air temperature at the coil outlet, corresponding to MaxSH (C)
		Real64 Ts; // Air temperature at the coil surface (C)
		Real64 Ws; // Air humidity ratio at the coil surface (kg/kg)

		RHsat = 0.98; // Saturated RH
		MaxSH = 15;
		MaxSC = 20;
		Garate = DXCoil( CoilIndex ).RatedAirMassFlowRate( 1 );
		FanSpdRatioMin = min( max( OAMassFlow / Garate, 0.65 ), 1.0 ); // ensure that coil flow rate is higher than OA flow rate

		if ( QCoil == 0 ) {
			//No Heating or Cooling
			FanSpdRatio = OAMassFlow / Garate;
			CoilOnOffRatio = 0.0;

			SHact = 999.0;
			SCact = 999.0;
			Tout = Tin;
			Hout = PsyHFnTdbW( Tin, Win );
			Wout = Win;

		} else if ( QCoil < 0 ) {
			//Cooling Mode

			// Obtain coil cooling loads
			QCoilSenCoolingLoad = -QCoil;

			// Coefficients describing coil performance
			SH = DXCoil( CoilIndex ).SH;
			C1Tevap = DXCoil( CoilIndex ).C1Te;
			C2Tevap = DXCoil( CoilIndex ).C2Te;
			C3Tevap = DXCoil( CoilIndex ).C3Te;
			BF = 0.0592;

			// Coil sensible heat transfer minimum value
			CalcVRFCoilSenCap( FlagCoolMode, CoilIndex, Tin, TeTc, SH, BF, QinSenPerFlowRate, Ts_1 );
			To_1 = Tin - QinSenPerFlowRate / 1005;
			QinSenMin1 = FanSpdRatioMin * Garate * QinSenPerFlowRate; // Corresponds real SH

			CalcVRFCoilSenCap( FlagCoolMode, CoilIndex, Tin, TeTc, MaxSH, BF, QinSenPerFlowRate, Ts_2 );
			To_2 = Tin - QinSenPerFlowRate / 1005;
			QinSenMin2 = FanSpdRatioMin * Garate * QinSenPerFlowRate; // Corresponds maximum SH

			if ( QCoilSenCoolingLoad > QinSenMin1 ) {
				// Increase fan speed to meet room sensible load; SH is not updated

				Par( 1 ) = QCoilSenCoolingLoad;
				Par( 2 ) = Ts_1;
				Par( 3 ) = Tin;
				Par( 4 ) = Garate;
				Par( 5 ) = BF;

				FanSpdRatioMax = 1.0;
				SolveRegulaFalsi( 1.0e-3, MaxIter, SolFla, Ratio1, FanSpdResidualCool, FanSpdRatioMin, FanSpdRatioMax, Par );
				if ( SolFla < 0 ) Ratio1 = FanSpdRatioMax; // over capacity
				FanSpdRatio = Ratio1;
				CoilOnOffRatio = 1.0;

				Tout = To_1; // Since SH is not updated
				Ws = PsyWFnTdbRhPb( Ts_1, RHsat, OutBaroPress, "ControlVRFIUCoil" );
				if ( Ws < Win ) {
					Wout = Win - ( Win - Ws ) * ( 1 - BF );
				} else {
					Wout = Win;
				}
				Hout = PsyHFnTdbW( Tout, Wout );
				SCact = 999.0;
				SHact = SH;

			} else {
				// Low load modification algorithm
				// Need to increase SH to further reduce coil capacity
				// May further implement coil cycling control if SC modification is not enough

				FanSpdRatio = FanSpdRatioMin;

				CoilOnOffRatio = 1.0;

				Tout = Tin - QCoilSenCoolingLoad / 1005.0 / FanSpdRatioMin / Garate;
				Ts = Tin - ( Tin - Tout ) / ( 1 - BF );
				deltaT = Ts - TeTc;

				// Update SH
				if ( C3Tevap <= 0.0 ) {
					if ( C2Tevap > 0.0 ) {
						SHact = ( deltaT - C1Tevap ) / C2Tevap;
					} else {
						SHact = 998.0;
					}
				} else {
					SHact = ( -C2Tevap + sqrt( pow_2( C2Tevap ) - 4 * C3Tevap * ( C1Tevap - deltaT ) ) ) / 2 / C3Tevap;
				}

				Ws = PsyWFnTdbRhPb( Ts, RHsat, OutBaroPress, "ControlVRFIUCoil" );
				if ( Ws < Win ) {
					Wout = Win - ( Win - Ws ) * ( 1 - BF );
				} else {
					Wout = Win;
				}

				if ( SHact > MaxSH ) {
					// Further implement On/Off Control
					SHact = MaxSH;
					CoilOnOffRatio = QCoilSenCoolingLoad / QinSenMin2;

					Ts = Ts_2;
					Ws = PsyWFnTdbRhPb( Ts, RHsat, OutBaroPress, "ControlVRFIUCoil" );
					if ( Ws < Win ) {
						Wout = Win - ( Win - Ws ) * ( 1 - BF );
					} else {
						Wout = Win;
					}

					//outlet air temperature and humidity ratio is time-weighted
					Tout = CoilOnOffRatio * To_2 + ( 1 - CoilOnOffRatio ) * Tin;
					Wout = CoilOnOffRatio * Wout + ( 1 - CoilOnOffRatio ) * Win;
				}

				Hout = PsyHFnTdbW( Tout, Wout );
				SCact = 999.0;
			}

		} else if ( QCoil > 0 ) {
			//Heating Mode

			// Obtain zonal heating loads
			QCoilSenHeatingLoad = QCoil;

			// Coefficients describing coil performance
			SC = DXCoil( CoilIndex ).SC;
			C1Tcond = DXCoil( CoilIndex ).C1Tc;
			C2Tcond = DXCoil( CoilIndex ).C2Tc;
			C3Tcond = DXCoil( CoilIndex ).C3Tc;

			BF = 0.136;

			// Coil sensible heat transfer minimum value
			CalcVRFCoilSenCap( FlagHeatMode, CoilIndex, Tin, TeTc, SC, BF, QinSenPerFlowRate, Ts_1 );
			To_1 = QinSenPerFlowRate / 1005 + Tin;
			QinSenMin1 = FanSpdRatioMin * Garate * QinSenPerFlowRate; // Corresponds real SH

			CalcVRFCoilSenCap( FlagHeatMode, CoilIndex, Tin, TeTc, MaxSC, BF, QinSenPerFlowRate, Ts_2 );
			To_2 = QinSenPerFlowRate / 1005 + Tin;
			QinSenMin2 = FanSpdRatioMin * Garate * QinSenPerFlowRate; // Corresponds maximum SH

			if ( QCoilSenHeatingLoad > QinSenMin1 ) {
				// Modulate fan speed to meet room sensible load; SC is not updated

				Par( 1 ) = QCoilSenHeatingLoad;
				Par( 2 ) = Ts_1;
				Par( 3 ) = Tin;
				Par( 4 ) = Garate;
				Par( 5 ) = BF;

				FanSpdRatioMax = 1.0;
				SolveRegulaFalsi( 1.0e-3, MaxIter, SolFla, Ratio1, FanSpdResidualHeat, FanSpdRatioMin, FanSpdRatioMax, Par );
				// this will likely cause problems eventually, -1 and -2 mean different things
				if ( SolFla < 0 ) Ratio1 = FanSpdRatioMax; // over capacity
				FanSpdRatio = Ratio1;
				CoilOnOffRatio = 1.0;

				Tout = Tin + ( Ts_1 - Tin ) * ( 1 - BF );
				Wout = Win;
				Hout = PsyHFnTdbW( Tout, Wout );
				SHact = 999.0;
				SCact = SC;

			} else {
				// Low load modification algorithm
				// Need to increase SC to further reduce coil heating capacity
				// May further implement coil cycling control if SC modification is not enough

				FanSpdRatio = FanSpdRatioMin;
				CoilOnOffRatio = 1.0;

				Tout = Tin + QCoilSenHeatingLoad / 1005.0 / FanSpdRatio / Garate;
				Ts = Tin + ( Tout - Tin ) / ( 1 - BF );
				deltaT = TeTc - Ts;

				// Update SC
				if ( C3Tcond <= 0.0 ) {
					if ( C2Tcond > 0.0 ) {
						SCact = ( deltaT - C1Tcond ) / C2Tcond;
					} else {
						SCact = 998.0;
					}
				} else {
					SCact = ( -C2Tcond + sqrt( pow_2( C2Tcond ) - 4 * C3Tcond * ( C1Tcond - deltaT ) ) ) / 2 / C3Tcond;
				}

				if ( SCact > MaxSC ) {
					// Implement On/Off Control
					SCact = MaxSC;
					CoilOnOffRatio = QCoilSenHeatingLoad / QinSenMin2;
					//outlet air temperature is time-weighted
					Tout = CoilOnOffRatio * To_2 + ( 1 - CoilOnOffRatio ) * Tin;
				}

				Wout = Win;
				Hout = PsyHFnTdbW( Tout, Wout );
				SHact = 999.0;
			}

		}

	}

	void
	CalcVRFCoilSenCap(
		int const OperationMode, // mode 0 for cooling, 1 for heating
		int const CoilNum,  // index to VRFTU cooling or heating coil
		Real64 const Tinlet,// dry bulb temperature of air entering the coil
		Real64 const TeTc,  // evaporating or condensing temperature
		Real64 const SHSC,  // SH at cooling /SC at heating
		Real64 const BF,    // Bypass factor
		Real64 & Q_sen,     // VRF coil sensible capacity per air mass flow rate
		Real64 & T_coil_surf// Air temperature at coil surface
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rongpeng Zhang, LBNL
		//       DATE WRITTEN   Jul 2015
		//       MODIFIED       na
		//
		//       RE-ENGINEERED  na
		//
		// PURPOSE OF THIS SUBROUTINE:
		//        Calculate the VRF coil sensible capacity per air mass flow rate, given:
		//        (1) refrigerant temperature (Te or Tc), (2) SH or SC, and (3) inlet air temperature.
		//
		// METHODOLOGY EMPLOYED:
		//        A new physics based VRF model appliable for Fluid Temperature Control.
		//
		// REFERENCES:
		//        na
		//
		// USE STATEMENTS:

		int const FlagCoolMode( 0 ); // Flag for cooling mode
		int const FlagHeatMode( 1 ); // Flag for heating mode
		Real64 C1Tevap;    // Coefficient for indoor unit coil evaporating temperature curve (-)
		Real64 C2Tevap;    // Coefficient for indoor unit coil evaporating temperature curve (-)
		Real64 C3Tevap;    // Coefficient for indoor unit coil evaporating temperature curve (-)
		Real64 C1Tcond;    // Coefficient for indoor unit coil condensing temperature curve (-)
		Real64 C2Tcond;    // Coefficient for indoor unit coil condensing temperature curve (-)
		Real64 C3Tcond;    // Coefficient for indoor unit coil condensing temperature curve (-)
		Real64 deltaT;     // Difference between Te/Tc and coil surface temperature (C)
		Real64 SH;         // Super heating at cooling mode(C)
		Real64 SC;         // Subcooling at heating mode (C)
		Real64 T_coil_in;  // Air temperature at coil inlet (C)
		Real64 T_coil_out; // Air temperature at coil outlet (C)

		if ( OperationMode == FlagCoolMode ) {
			//Cooling: OperationMode 0

			C1Tevap = DXCoil( CoilNum ).C1Te;
			C2Tevap = DXCoil( CoilNum ).C2Te;
			C3Tevap = DXCoil( CoilNum ).C3Te;
			SH = SHSC;
			T_coil_in = Tinlet;

			// Coil surface temperature
			deltaT = C3Tevap * SH * SH + C2Tevap * SH + C1Tevap;
			T_coil_surf = TeTc + deltaT;

			// Outlet air temperature
			T_coil_out = T_coil_in - ( T_coil_in - T_coil_surf ) * ( 1 - BF );

			// Coil sensilbe heat transfer per mass flow rate
			Q_sen = max( 1005 * ( T_coil_in - T_coil_out ), 0.0 );

		} else if ( OperationMode == FlagHeatMode ) {
			//Heating: OperationMode 1

			C1Tcond = DXCoil( CoilNum ).C1Tc;
			C2Tcond = DXCoil( CoilNum ).C2Tc;
			C3Tcond = DXCoil( CoilNum ).C3Tc;
			SC = SHSC;
			T_coil_in = Tinlet;

			// Coil surface temperature
			deltaT = C3Tcond * SC * SC + C2Tcond * SC + C1Tcond;
			T_coil_surf = TeTc - deltaT;

			// Coil outlet air temperature
			T_coil_out = T_coil_in + ( T_coil_surf - T_coil_in ) * ( 1 - BF );

			// Coil sensilbe heat transfer_minimum value
			Q_sen = max( 1005 * ( T_coil_out - T_coil_in ), 0.0 );

		}

	}

	void
	CalcVRFCoilCapModFac(
		int const OperationMode, // mode 0 for cooling, 1 for heating
		Optional< int const > CoilIndex,  // index to VRFTU cooling or heating coil
		Optional< std::string > CoilName, // name of VRFTU cooling or heating coil
		Real64 const Tinlet,// dry bulb temperature of air entering the coil
		Optional< Real64 const > TeTc,  // evaporating or condensing temperature
		Optional< Real64 const > SHSC,  // SH at cooling /SC at heating
		Optional< Real64 const > BF,    // Bypass factor
		Real64 & CapModFac // Coil capacity modification factor
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rongpeng Zhang, LBNL
		//       DATE WRITTEN   Jul 2015
		//       MODIFIED       na
		//
		//       RE-ENGINEERED  na
		//
		// PURPOSE OF THIS SUBROUTINE:
		//        Calculate the VRF coil capacity modification factor, which is the ratio of
		//        the capacity at real conditions and that at rated conditions.
		//        This is used for the coil sizing subroutine.
		//
		// METHODOLOGY EMPLOYED:
		//        A new physics based VRF model applicable for Fluid Temperature Control.
		//
		// REFERENCES:
		//        na
		//
		// USE STATEMENTS:

		bool ErrorsFound( false );       // Flag for errors
		int const FlagCoolMode( 0 );     // Flag for cooling mode
		int const FlagHeatMode( 1 );     // Flag for heating mode
		Real64 const BFC_rate( 0.0592 ); // Bypass factor at cooling mode (-)
		Real64 const BFH_rate( 0.1360 ); // Bypass factor at heating mode (-)
		Real64 const SH_rate( 3 );       // Super heating at cooling mode, default 3(C)
		Real64 const SC_rate( 5 );       // Subcooling at heating mode, default 5 (C)
		Real64 const Te_rate( 6 );       // Evaporating temperature at cooling mode, default 6 (C)
		Real64 const Tc_rate( 44 );      // Condensing temperature at heating mode, default 44 (C)
		int CoilNum;       // index to VRFTU cooling or heating coil
		Real64 BF_real;    // Bypass factor (-)
		Real64 SHSC_real;  // Super heating or Subcooling (C)
		Real64 TeTc_real;  // Evaporating temperature or condensing temperature (C)
		Real64 Ts;         // Air temperature at coil surface (C)
		Real64 Q_real;     // Coil capacity at given condition (W)
		Real64 Q_rate;     // Coil capacity at rated condition (W)

		if ( present( CoilIndex )){
			CoilNum = CoilIndex;
		} else {
			GetDXCoilIndex( CoilName, CoilNum, ErrorsFound );
		}

		if ( OperationMode == FlagCoolMode ) {
			//Cooling: OperationMode 0

			if ( present( BF ) ) {
				BF_real = BF;
			} else {
				BF_real = BFC_rate;
			}
			if ( present( TeTc ) ) {
				TeTc_real = TeTc;
			} else {
				TeTc_real = Te_rate;
			}
			if ( present( SHSC ) ) {
				SHSC_real = SHSC;
			} else {
				SHSC_real = SH_rate;
			}

			// Coil capacity at rated conditions
			CalcVRFCoilSenCap( FlagCoolMode, CoilNum, 24, Te_rate, SH_rate, BFC_rate, Q_rate, Ts );

			// Coil capacity at given conditions
			CalcVRFCoilSenCap( FlagCoolMode, CoilNum, Tinlet, TeTc_real, SHSC_real, BF_real, Q_real, Ts );

			if ( Q_rate > 0 ){
				CapModFac = Q_real / Q_rate;
			} else {
				CapModFac = 1.0;
			}

		} else if ( OperationMode == FlagHeatMode ) {
			//Heating: OperationMode 1

			if ( present( BF ) ) {
				BF_real = BF;
			} else {
				BF_real = BFH_rate;
			}
			if ( present( TeTc ) ) {
				TeTc_real = TeTc;
			} else {
				TeTc_real = Tc_rate;
			}
			if ( present( SHSC ) ) {
				SHSC_real = SHSC;
			} else {
				SHSC_real = SC_rate;
			}

			// Coil capacity at rated conditions
			CalcVRFCoilSenCap( FlagHeatMode, CoilNum, 20, Tc_rate, SC_rate, BFH_rate, Q_rate, Ts );

			// Coil capacity at given conditions
			CalcVRFCoilSenCap( FlagHeatMode, CoilNum, Tinlet, TeTc_real, SHSC_real, BF_real, Q_real, Ts );

			if ( Q_rate > 0 ){
				CapModFac = Q_real / Q_rate;
			} else {
				CapModFac = 1.0;
			}
		}

	}

	Real64
	FanSpdResidualCool(
		Real64 const FanSpdRto, // indoor unit fan speed ratio
		Array1< Real64 > const & Par // parameters
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Xiufeng Pang (XP)
		//       DATE WRITTEN   Mar 2013
		//       MODIFIED       Nov 2015, RP Zhang, LBNL
		//       RE-ENGINEERED
		//
		// PURPOSE OF THIS FUNCTION:
		//       Calculates residual function (desired zone cooling load - actual coil cooling capacity)
		//       This is used to modify the fan speed to adjust the coil cooling capacity to match
		//       the zone cooling load.
		//
		// METHODOLOGY EMPLOYED:
		//
		// REFERENCES:
		// na
		//
		// USE STATEMENTS:
		// na

		//FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 BF; // Bypass factor (-)
		Real64 FanSpdResidualCool; // Modified fan speed ratio to meet actual zone load (-)
		Real64 Garate; // Nominal air mass flow rate (m3/s)
		Real64 TcoilIn; // Air temperature at indoor coil inlet (C)
		Real64 Th2; // Air temperature at the coil surface (C)
		Real64 TotCap; // Cooling capacity of the coil (W)
		Real64 Tout; // Air temperature at the indoor unit outlet (C)
		Real64 ZnSenLoad;  // Zone sensible cooling load (W)

		ZnSenLoad = Par( 1 );
		Th2 = Par( 2 );
		TcoilIn = Par( 3 );
		Garate = Par( 4 );
		BF = Par( 5 );
		// +-100 W minimum zone load?
		if ( std::abs( ZnSenLoad ) < 100.0 ) ZnSenLoad = sign( 100.0, ZnSenLoad );

		Tout = TcoilIn - ( TcoilIn - Th2 ) * ( 1 - BF );
		TotCap = FanSpdRto * Garate * 1005.0 * ( TcoilIn - Tout );
		FanSpdResidualCool = (TotCap - ZnSenLoad ) / ZnSenLoad;

		return FanSpdResidualCool;
	}

	Real64
	FanSpdResidualHeat(
		Real64 const FanSpdRto, // indoor unit fan speed ratio
		Array1< Real64 > const & Par        // parameters
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Xiufeng Pang (XP)
		//       DATE WRITTEN   Mar 2013
		//       MODIFIED       Nov 2015, RP Zhang, LBNL
		//       RE-ENGINEERED
		//
		// PURPOSE OF THIS FUNCTION:
		//       Calculates residual function (desired zone heating load - actual heating coil capacity)
		//       This is used to modify the fan speed to adjust the coil heating capacity to match
		//       the zone heating load.
		//
		// METHODOLOGY EMPLOYED:
		//
		// REFERENCES:
		// na
		//
		// USE STATEMENTS:
		// na

		Real64 BF; // Bypass factor (-)
		Real64 FanSpdResidualHeat; // Modified fan speed ratio to meet actual zone load (-)
		Real64 Garate; // Nominal air mass flow rate (m3/s)
		Real64 TcoilIn; // Air temperature at indoor coil inlet (C)
		Real64 Th2; // Air temperature at the coil surface (C)
		Real64 TotCap; // Heating capacity of the coil (W)
		Real64 Tout; // Air temperature at the indoor unit outlet (C)
		Real64 ZnSenLoad; // Zone sensible heating load (W)

		ZnSenLoad = Par( 1 );
		Th2 = Par( 2 );
		TcoilIn = Par( 3 );
		Garate = Par( 4 );
		BF = Par( 5 );
		// +-100 W minimum zone load?
		if ( std::abs( ZnSenLoad ) < 100.0 ) ZnSenLoad = sign( 100.0, ZnSenLoad );

		Tout = TcoilIn + ( Th2 - TcoilIn )  *  ( 1 - BF );
		TotCap = FanSpdRto  *  Garate  *  1005.0 * ( Tout - TcoilIn );
		FanSpdResidualHeat = ( TotCap - ZnSenLoad ) / ZnSenLoad;

		return FanSpdResidualHeat;
	}

	void
	SetMSHPDXCoilHeatRecoveryFlag( int const DXCoilNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         L. Gu
		//       DATE WRITTEN   Sep. 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Set the heat recovery flag true when the parent object requests heat recovery.

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		if ( DXCoil( DXCoilNum ).FuelType != FuelTypeElectricity ) {
			DXCoil( DXCoilNum ).MSHPHeatRecActive = true;
		}

	}

	// Clears the global data in DXCoils.
	// Needed for unit tests, should not be normally called.
	void
	clear_state()
	{

		CurDXCoilNum = 0;
		NumDXCoils = 0;
		HPWHHeatingCapacity = 0.0;
		HPWHHeatingCOP = 0.0;
		NumVRFHeatingCoils = 0;
		NumVRFCoolingCoils = 0;
		NumDXHeatingCoils = 0;
		NumDoe2DXCoils = 0;
		NumDXHeatPumpWaterHeaterPumpedCoils = 0;
		NumDXHeatPumpWaterHeaterWrappedCoils = 0;
		NumDXMulSpeedCoils = 0;
		NumDXMulModeCoils = 0;
		NumDXMulSpeedCoolCoils = 0;
		NumDXMulSpeedHeatCoils = 0;

		GetCoilsInputFlag = true;
		MyOneTimeFlag = true;

		DXCoil.deallocate();
		DXCoilNumericFields.deallocate();
		DXCoilOutletTemp.deallocate();
		DXCoilOutletHumRat.deallocate();
		DXCoilPartLoadRatio.deallocate();
		DXCoilFanOpMode.deallocate();
		DXCoilFullLoadOutAirTemp.deallocate();
		DXCoilFullLoadOutAirHumRat.deallocate();
		DXCoilTotalCooling.deallocate();
		DXCoilTotalHeating.deallocate();
		DXCoilCoolInletAirWBTemp.deallocate();
		DXCoilHeatInletAirDBTemp.deallocate();
		DXCoilHeatInletAirWBTemp.deallocate();
		CheckEquipName.deallocate();

	}

} // DXCoils

} // EnergyPlus
