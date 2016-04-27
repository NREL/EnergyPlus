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

// EnergyPlus::HVACVariableRefrigerantFlow unit tests

// Google test headers
#include <gtest/gtest.h>

// C++ Headers
#include <cassert>
#include <cmath>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <BranchInputManager.hh>
#include <CurveManager.hh>
#include <DataAirLoop.hh>
#include <DataAirSystems.hh>
#include <DataEnvironment.hh>
#include <DataGlobals.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHVACGlobals.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataSizing.hh>
#include <DataZoneEquipment.hh>
#include <DataZoneEnergyDemands.hh>
#include <DXCoils.hh>
#include <FluidProperties.hh>
#include <Fans.hh>
#include <GlobalNames.hh>
#include <HeatBalanceManager.hh>
#include <HVACVariableRefrigerantFlow.hh>
#include <OutputReportPredefined.hh>
#include <PlantManager.hh>
#include <Psychrometrics.hh>
#include <ScheduleManager.hh>
#include <SizingManager.hh>

using namespace EnergyPlus;
using namespace DXCoils;
using namespace EnergyPlus::BranchInputManager;
using namespace EnergyPlus::CurveManager;
using namespace EnergyPlus::DataAirLoop;
using namespace EnergyPlus::DataAirSystems;
using namespace EnergyPlus::DataEnvironment;
using namespace EnergyPlus::DataGlobals;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::DataHeatBalFanSys;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::DataPlant;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::DataZoneEnergyDemands;
using namespace EnergyPlus::FluidProperties;
using namespace EnergyPlus::DXCoils;
using namespace EnergyPlus::Fans;
using namespace EnergyPlus::HeatBalanceManager;
using namespace EnergyPlus::HVACVariableRefrigerantFlow;
using namespace EnergyPlus::GlobalNames;
using namespace EnergyPlus::OutputReportPredefined;
using namespace EnergyPlus::PlantManager;
using namespace EnergyPlus::Psychrometrics;
using namespace EnergyPlus::ScheduleManager;
using namespace EnergyPlus::SizingManager;

namespace EnergyPlus {

	TEST_F( EnergyPlusFixture, VRFFluidTCtrlGetCoilInput )
	{
		// PURPOSE OF THE TEST:
		//   IDF Read in for the new coil type: Coil:Cooling:DX:VariableRefrigerantFlow:FluidTemperatureControl

		std::string const idf_objects = delimited_string( {
			" Coil:Cooling:DX:VariableRefrigerantFlow:FluidTemperatureControl,  ",
			" 	 TU1 VRF DX Cooling Coil, !- Name							   ",
			" 	 VRFAvailSched,           !- Availability Schedule Name		   ",
			" 	 TU1 VRF DX CCoil Inlet Node,  !- Coil Air Inlet Node		   ",
			" 	 TU1 VRF DX CCoil Outlet Node, !- Coil Air Outlet Node		   ",
			" 	 2200,                    !- Rated Total Cooling Capacity {W}   ",
			" 	 0.865,                   !- Rated Sensible Heat Ratio		   ",
			" 	 3,                       !- Indoor Unit Reference Superheating ",
			" 	 IUEvapTempCurve,         !- Indoor Unit Evaporating Temperature",
			" 	 ;                        !- Name of Water Storage Tank for Cond",
			" Curve:Quadratic,												   ",
			"     IUEvapTempCurve,         !- Name							   ",
			"     0,                       !- Coefficient1 Const				   ",
			"     0.80404,                 !- Coefficient2 x					   ",
			"     0,                       !- Coefficient3 x**2				   ",
			"     0,                       !- Minimum Value of x				   ",
			"     15,                      !- Maximum Value of x				   ",
			"     ,                        !- Minimum Curve Outp				   ",
			"     ,                        !- Maximum Curve Outp				   ",
			"     Dimensionless,           !- Input Unit Type fo				   ",
			"     Dimensionless;           !- Output Unit Type				   "
		} );

		ASSERT_FALSE( process_idf( idf_objects ) );

		// Run the method
		GetDXCoils( );

		// Check the results
		ASSERT_EQ( 1, NumDXCoils );
		EXPECT_EQ( DXCoil( 1 ).DXCoilType_Num, 33 );
		EXPECT_EQ( DXCoil( 1 ).RatedTotCap( 1 ), 2200 );
		EXPECT_EQ( DXCoil( 1 ).RatedSHR( 1 ), 0.865 );
		EXPECT_EQ( DXCoil( 1 ).C1Te, 0 );
		EXPECT_EQ( DXCoil( 1 ).C2Te, 0.80404 );
		EXPECT_EQ( DXCoil( 1 ).C3Te, 0 );
		EXPECT_EQ( DXCoil( 1 ).SH, 3 );

	}

	TEST_F( EnergyPlusFixture, HVACVariableRefrigerantFlow_VRF_FluidTCtrl_CompResidual )
	{
		// PURPOSE OF THIS SUBROUTINE:
		//  Calculates residual function ((VRV terminal unit cooling output - Zone sensible cooling load)

		using namespace CurveManager;

		int CurveNum = 1;
		int NumPar;
		double Te = -2.796; // Outdoor unit evaporating temperature
		double Tdis = 40.093;
		double CondHeat = 1864.44;
		Array1D< Real64 > Par;

		// Allocate
		NumCurves = 1; //CurveManager::NumCurves
		PerfCurve.allocate( NumCurves );

		NumPar = 3;
		Par.allocate( NumPar );

		// Inputs: curve parameters
		Par( 1 ) = Tdis;
		Par( 2 ) = CondHeat;
		Par( 3 ) = CurveNum;

		// Inputs: parameters
		PerfCurve( CurveNum ).CurveType = CurveManager::BiQuadratic;
		PerfCurve( CurveNum ).ObjectType = CurveType_BiQuadratic;
		PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;
		PerfCurve( CurveNum ).Coeff1 = 724.71125; // Coefficient1 Constant
		PerfCurve( CurveNum ).Coeff2 = -21.867868; // Coefficient2 x
		PerfCurve( CurveNum ).Coeff3 = 0.52480042; // Coefficient3 x**2
		PerfCurve( CurveNum ).Coeff4 = -17.043566; // Coefficient4 y
		PerfCurve( CurveNum ).Coeff5 = -.40346383; // Coefficient5 y**2
		PerfCurve( CurveNum ).Coeff6 = 0.29573589; // Coefficient6 x*y
		PerfCurve( CurveNum ).Var1Min = 15; // Minimum Value of x
		PerfCurve( CurveNum ).Var1Max = 65; // Maximum Value of x
		PerfCurve( CurveNum ).Var2Min = -30; // Minimum Value of y
		PerfCurve( CurveNum ).Var2Max = 15; // Maximum Value of y

		// Run and Check
		double CompResidual = HVACVariableRefrigerantFlow::CompResidual_FluidTCtrl( Te, Par );
		EXPECT_NEAR( 1.652, CompResidual, 0.005 );

		// Clean up
		PerfCurve.deallocate();
		Par.deallocate();

	}

	TEST_F( EnergyPlusFixture, HVACVariableRefrigerantFlow_VRF_FluidTCtrl_FanSpdResidualCool )
	{
		// PURPOSE OF THIS TEST:
		//   Test the method FanSpdResidualCool.

		using namespace DXCoils;

		int NumPar;
		double FanSpdRto;
		double ZnSenLoad;
		double Th2;
		double TairInlet;
		double Garate;
		double BF;
		Array1D< Real64 > Par;

		// Allocate
		NumPar = 6;
		Par.allocate( 6 );

		// Inputs:
		FanSpdRto = 0.5;
		ZnSenLoad = 2716.62;
		Th2 = 17.41212;
		TairInlet = 25.55534;
		Garate = 0.20664;
		BF = 0.0592;
		Par( 1 ) = ZnSenLoad;
		Par( 2 ) = Th2;
		Par( 3 ) = TairInlet;
		Par( 4 ) = Garate;
		Par( 5 ) = BF;

		// Run and Check
		double FanSpdResidual = FanSpdResidualCool( FanSpdRto, Par );
		EXPECT_NEAR( -0.707, FanSpdResidual, 0.0005 );

		// Clean up
		Par.deallocate();

	}

	TEST_F( EnergyPlusFixture, HVACVariableRefrigerantFlow_VRF_FluidTCtrl_FanSpdResidualHeat )
	{
		// PURPOSE OF THIS TEST:
		//   Test the method FanSpdResidualHeat.

		using namespace DXCoils;

		int NumPar;
		double FanSpdRto;
		double ZnSenLoad;
		double Th2;
		double TairInlet;
		double Garate;
		double BF;
		Array1D< Real64 > Par;

		// Allocate
		NumPar = 6;
		Par.allocate( 6 );

		// Inputs:
		FanSpdRto = 0.5;
		ZnSenLoad = 4241.66;
		Th2 = 41.221;
		TairInlet = 20.236;
		Garate = 0.21136;
		BF = 0.1360;
		Par( 1 ) = ZnSenLoad;
		Par( 2 ) = Th2;
		Par( 3 ) = TairInlet;
		Par( 4 ) = Garate;
		Par( 5 ) = BF;

		// Run and Check
		double FanSpdResidual = FanSpdResidualHeat( FanSpdRto, Par );
		EXPECT_NEAR( -0.5459, FanSpdResidual, 0.0005 );

		// Clean up
		Par.deallocate();

	}

	TEST_F( EnergyPlusFixture, HVACVariableRefrigerantFlow_VRF_FluidTCtrl_CalcVRFIUAirFlow )
	{
		// PURPOSE OF THIS TEST:
		//   Test the method CalcVRFIUAirFlow, which analyzes the VRF Indoor Unit operations given zonal loads.
		//   Calculated parameters includie: (1) Fan Speed Ratio, (2) SH/SC Degrees, and (3) Coil Inlet/Outlet conditions

		using namespace DXCoils;
		using namespace DataZoneEnergyDemands;
		using namespace EnergyPlus::Psychrometrics;
		using DataEnvironment::OutBaroPress;

		int ZoneIndex;  // index to zone where the VRF Terminal Unit resides
		int CoolCoilIndex;  // index to VRFTU cooling coil
		int HeatCoilIndex;  // index to VRFTU heating coil
		int Mode;       // mode 0 for cooling, 1 for heating, 2 for neither cooling nor heating
		Real64 Temp;    // evaporating or condensing temperature
		Real64 FanSpdRatio; // fan speed ratio
		Real64 Wout;    // outlet air humidity ratio
		Real64 Toutlet; // outlet air temperature
		Real64 Houtlet; // outlet air enthalpy
		Real64 SHact;   // actual SH
		Real64 SCact;   // actual SC

		// Allocate
		int NumCoils = 2;
		DXCoil.allocate( NumCoils );
		int NumZones = 2;
		ZoneSysEnergyDemand.allocate( NumZones );

		// Common Inputs
		CoolCoilIndex = 1;
		HeatCoilIndex = 2;
		FanSpdRatio = 0;
		Wout = 1;
		OutBaroPress = 101570;
		InitializePsychRoutines();

		DXCoil( CoolCoilIndex ).C1Te = 0;
		DXCoil( CoolCoilIndex ).C2Te = 0.804;
		DXCoil( CoolCoilIndex ).C3Te = 0;
		DXCoil( CoolCoilIndex ).SH = 3.00;
		DXCoil( CoolCoilIndex ).SupplyFanIndex = 0;
		DXCoil( HeatCoilIndex ).C1Tc = -1.905;
		DXCoil( HeatCoilIndex ).C2Tc = 0.4333;
		DXCoil( HeatCoilIndex ).C3Tc = 0.0207;
		DXCoil( HeatCoilIndex ).SC = 5.00;
		DXCoil( HeatCoilIndex ).SupplyFanIndex = 0;

		// Run and Check for Cooling Mode
		Mode = 0;
		Temp = 15;
		ZoneIndex = 1;

		ZoneSysEnergyDemand( ZoneIndex ).OutputRequiredToCoolingSP = -2716.6229;
		ZoneSysEnergyDemand( ZoneIndex ).OutputRequiredToHeatingSP = -45507.8487;

		DXCoil( CoolCoilIndex ).RatedAirMassFlowRate( 1 ) = 0.2066;
		DXCoil( CoolCoilIndex ).InletAirTemp = 25.5553;
		DXCoil( CoolCoilIndex ).InletAirHumRat = 8.4682e-3;
		DXCoil( CoolCoilIndex ).InletAirEnthalpy = 47259.78;

		ControlVRFIUCoil( CoolCoilIndex, ZoneSysEnergyDemand( ZoneIndex ).OutputRequiredToCoolingSP, 25.5553, 8.4682e-3, Temp, 0, FanSpdRatio, Wout, Toutlet, Houtlet, SHact, SCact );
		EXPECT_NEAR( Toutlet, 17.89, 0.01 );
		EXPECT_NEAR( Houtlet, 39440, 1 );
		EXPECT_NEAR( SHact, 3.00, 0.01 );

		// Run and Check for Heating Mode
		Mode = 1;
		Temp = 42;
		ZoneIndex = 2;

		ZoneSysEnergyDemand( ZoneIndex ).OutputRequiredToCoolingSP = 43167.2628;
		ZoneSysEnergyDemand( ZoneIndex ).OutputRequiredToHeatingSP = 4241.66099;

		DXCoil( HeatCoilIndex ).RatedAirMassFlowRate( 1 ) = 0.21136;
		DXCoil( HeatCoilIndex ).InletAirTemp = 20.2362;
		DXCoil( HeatCoilIndex ).InletAirHumRat = 4.1053e-3;
		DXCoil( HeatCoilIndex ).InletAirEnthalpy = 30755.6253;

		ControlVRFIUCoil( HeatCoilIndex, ZoneSysEnergyDemand( ZoneIndex ).OutputRequiredToHeatingSP, 20.2362, 4.1053e-3, Temp, 0, FanSpdRatio, Wout, Toutlet, Houtlet, SHact, SCact );
		EXPECT_NEAR( Toutlet, 38.37, 0.01 );
		EXPECT_NEAR( Houtlet, 49113, 1 );
		EXPECT_NEAR( SCact, 5.00, 0.01 );

		// Clean up
		ZoneSysEnergyDemand.deallocate();
	}

	TEST_F( EnergyPlusFixture, HVACVariableRefrigerantFlow_VRF_FluidTCtrl_CalcVRFIUTeTc )
	{
		// PURPOSE OF THIS TEST:
		//   Test the method CalcVRFIUTeTc_FluidTCtrl, which determines the VRF evaporating temperature at
		//   cooling mode and the condensing temperature at heating mode.

		using namespace HVACVariableRefrigerantFlow;

		// Allocate
		int NumVRFCondenser = 1;
		VRF.allocate( NumVRFCondenser );
		int NumTUList = 1;
		TerminalUnitList.allocate( NumTUList );

		// Common Inputs
		int IndexVRFCondenser = 1;
		int IndexTUList = 1;

		TerminalUnitList( IndexTUList ).NumTUInList = 2;

		VRF( IndexVRFCondenser ).ZoneTUListPtr = 1;
		VRF( IndexVRFCondenser ).AlgorithmIUCtrl = 0;
		VRF( IndexVRFCondenser ).EvapTempFixed = 3;
		VRF( IndexVRFCondenser ).CondTempFixed = 5;

		// Run and Check
		CalcVRFIUTeTc_FluidTCtrl( IndexVRFCondenser );

		EXPECT_EQ( VRF( IndexVRFCondenser ).IUEvaporatingTemp, 3 );
		EXPECT_EQ( VRF( IndexVRFCondenser ).IUCondensingTemp, 5 );

		// Clean up
		VRF.deallocate();
		TerminalUnitList.deallocate();
	}

	TEST_F( EnergyPlusFixture, VRFTest_SysCurve ) {

		bool ErrorsFound( false );        // function returns true on error
		bool FirstHVACIteration( true );  // simulate the first pass through HVAC simulation, use false for next iteration
		int VRFCond( 1 );                 // index to VRF condenser
		int VRFTUNum( 1 );                // index to VRF terminal unit
		int EquipPtr( 1 );                // index to equipment list
		int CurZoneNum( 1 );              // index to zone
		int ZoneInletAirNode( 0 );        // zone inlet node number
		Real64 DefrostWatts( 0.0 );       // calculation of VRF defrost power [W]
		Real64 SysOutputProvided( 0.0 );  // function returns sensible capacity [W]
		Real64 LatOutputProvided( 0.0 );  // function returns latent capacity [W]

		std::string const idf_objects = delimited_string( {
			"Version,8.3;",
			" ",
			"AirConditioner:VariableRefrigerantFlow,",
			"  VRF Heat Pump,           !- Heat Pump Name",
			"  VRFCondAvailSched,       !- Availability Schedule Name",
			"  autosize,                !- Gross Rated Total Cooling Capacity {W}",
			"  3.2917,                  !- Gross Rated Cooling COP {W/W}",
			"  -5,                      !- Minimum Outdoor Temperature in Cooling Mode {C}",
			"  43,                      !- Maximum Outdoor Temperature in Cooling Mode {C}",
			"  VRFCoolCapFT,            !- Cooling Capacity Ratio Modifier Function of Low Temperature Curve Name",
			"  VRFCoolCapFTBoundary,    !- Cooling Capacity Ratio Boundary Curve Name",
			"  VRFCoolCapFTHi,          !- Cooling Capacity Ratio Modifier Function of High Temperature Curve Name",
			"  VRFCoolEIRFT,            !- Cooling Energy Input Ratio Modifier Function of Low Temperature Curve Name",
			"  VRFCoolEIRFTBoundary,    !- Cooling Energy Input Ratio Boundary Curve Name",
			"  VRFCoolEIRFTHi,          !- Cooling Energy Input Ratio Modifier Function of High Temperature Curve Name",
			"  CoolingEIRLowPLR,        !- Cooling Energy Input Ratio Modifier Function of Low Part-Load Ratio Curve Name",
			"  CoolingEIRHiPLR,         !- Cooling Energy Input Ratio Modifier Function of High Part-Load Ratio Curve Name",
			"  CoolingCombRatio,        !- Cooling Combination Ratio Correction Factor Curve Name",
			"  VRFCPLFFPLR,             !- Cooling Part-Load Fraction Correlation Curve Name",
			"  autosize,                !- Gross Rated Heating Capacity {W}",
			"  ,                        !- Rated Heating Capacity Sizing Ratio {W/W}",
			"  3.5484,                  !- Gross Rated Heating COP {W/W}",
			"  -20,                     !- Minimum Outdoor Temperature in Heating Mode {C}",
			"  20,                      !- Maximum Outdoor Temperature in Heating Mode {C}",
			"  VRFHeatCapFT,            !- Heating Capacity Ratio Modifier Function of Low Temperature Curve Name",
			"  VRFHeatCapFTBoundary,    !- Heating Capacity Ratio Boundary Curve Name",
			"  VRFHeatCapFTHi,          !- Heating Capacity Ratio Modifier Function of High Temperature Curve Name",
			"  VRFHeatEIRFT,            !- Heating Energy Input Ratio Modifier Function of Low Temperature Curve Name",
			"  VRFHeatEIRFTBoundary,    !- Heating Energy Input Ratio Boundary Curve Name",
			"  VRFHeatEIRFTHi,          !- Heating Energy Input Ratio Modifier Function of High Temperature Curve Name",
			"  WetBulbTemperature,      !- Heating Performance Curve Outdoor Temperature Type",
			"  HeatingEIRLowPLR,        !- Heating Energy Input Ratio Modifier Function of Low Part-Load Ratio Curve Name",
			"  HeatingEIRHiPLR,         !- Heating Energy Input Ratio Modifier Function of High Part-Load Ratio Curve Name",
			"  HeatingCombRatio,        !- Heating Combination Ratio Correction Factor Curve Name",
			"  VRFCPLFFPLR,             !- Heating Part-Load Fraction Correlation Curve Name",
			"  0.25,                    !- Minimum Heat Pump Part-Load Ratio {dimensionless}",
			"  SPACE1-1,                !- Zone Name for Master Thermostat Location",
			"  LoadPriority,            !- Master Thermostat Priority Control Type",
			"  ,                        !- Thermostat Priority Schedule Name",
			"  VRF Heat Pump TU List,   !- Zone Terminal Unit List Name",
			"  No,                      !- Heat Pump Waste Heat Recovery",
			"  30,                      !- Equivalent Piping Length used for Piping Correction Factor in Cooling Mode {m}",
			"  10,                      !- Vertical Height used for Piping Correction Factor {m}",
			"  CoolingLengthCorrectionFactor,  !- Piping Correction Factor for Length in Cooling Mode Curve Name",
			"  -0.000386,               !- Piping Correction Factor for Height in Cooling Mode Coefficient {1/m}",
			"  30,                      !- Equivalent Piping Length used for Piping Correction Factor in Heating Mode {m}",
			"  ,                        !- Piping Correction Factor for Length in Heating Mode Curve Name",
			"  ,                        !- Piping Correction Factor for Height in Heating Mode Coefficient {1/m}",
			"  15,                      !- Crankcase Heater Power per Compressor {W}",
			"  3,                       !- Number of Compressors {dimensionless}",
			"  0.33,                    !- Ratio of Compressor Size to Total Compressor Capacity {W/W}",
			"  7,                       !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater {C}",
			"  ReverseCycle,            !- Defrost Strategy",
			"  Timed,                   !- Defrost Control",
			"  DefrostEIRSched,         !- Defrost Energy Input Ratio Modifier Function of Temperature Curve Name",
			"  ,                        !- Defrost Time Period Fraction {dimensionless}",
			"  autosize,                !- Resistive Defrost Heater Capacity {W}",
			"  7,                       !- Maximum Outdoor Dry-bulb Temperature for Defrost Operation {C}",
			"  AirCooled,               !- Condenser Type",
			"  MyVRFOANode,             !- Condenser Inlet Node Name",
			"  ,                        !- Condenser Outlet Node Name",
			"  ,                        !- Water Condenser Volume Flow Rate {m3/s}",
			"  ,                        !- Evaporative Condenser Effectiveness {dimensionless}",
			"  ,                        !- Evaporative Condenser Air Flow Rate {m3/s}",
			"  0,                       !- Evaporative Condenser Pump Rated Power Consumption {W}",
			"  ,                        !- Supply Water Storage Tank Name",
			"  0,                       !- Basin Heater Capacity {W/K}",
			"  ,                        !- Basin Heater Setpoint Temperature {C}",
			"  ,                        !- Basin Heater Operating Schedule Name",
			"  Electricity;             !- Fuel Type",
			" ",
			"Zone,",
			"  SPACE1-1,                !- Name",
			"  0,                       !- Direction of Relative North {deg}",
			"  0,                       !- X Origin {m}",
			"  0,                       !- Y Origin {m}",
			"  0,                       !- Z Origin {m}",
			"  1,                       !- Type",
			"  1,                       !- Multiplier",
			"  2.438400269,             !- Ceiling Height {m}",
			"  239.247360229;           !- Volume {m3}",
			" ",
			"ZoneHVAC:EquipmentConnections,",
			"  SPACE1-1,                !- Zone Name",
			"  SPACE1-1 Eq,             !- Zone Conditioning Equipment List Name",
			"  TU1 Outlet Node,         !- Zone Air Inlet Node or NodeList Name",
			"  TU1 Inlet Node,          !- Zone Air Exhaust Node or NodeList Name",
			"  SPACE1-1 Node,           !- Zone Air Node Name",
			"  SPACE1-1 Out Node;       !- Zone Return Air Node Name", // not used anywhere else in the example file
			" ",
			"ZoneHVAC:EquipmentList,",
			"  SPACE1-1 Eq,             !- Name",
			"  ZoneHVAC:TerminalUnit:VariableRefrigerantFlow,  !- Zone Equipment 1 Object Type",
			"  TU1,                     !- Zone Equipment 1 Name",
			"  1,                       !- Zone Equipment 1 Cooling Sequence",
			"  1;                       !- Zone Equipment 1 Heating or No-Load Sequence",
			" ",
			"ZoneTerminalUnitList,",
			"  VRF Heat Pump TU List,    !- Zone Terminal Unit List Name",
			"  TU1;                      !- Zone Terminal Unit Name 1",
			" ",
			"ZoneHVAC:TerminalUnit:VariableRefrigerantFlow,",
			"  TU1,                      !- Zone Terminal Unit Name",
			"  VRFAvailSched,            !- Terminal Unit Availability Schedule",
			"  TU1 Inlet Node,           !- Terminal Unit Air Inlet Node Name",
			"  TU1 Outlet Node,          !- Terminal Unit Air Outlet Node Name",
			"  autosize,                 !- Supply Air Flow Rate During Cooling Operation {m3/s}",
			"  autosize,                 !- Supply Air Flow Rate When No Cooling is Needed {m3/s}",
			"  autosize,                 !- Supply Air Flow Rate During Heating Operation {m3/s}",
			"  autosize,                 !- Supply Air Flow Rate When No Heating is Needed {m3/s}",
			"  autosize,                 !- Outdoor Air Flow Rate During Cooling Operation {m3/s}",
			"  autosize,                 !- Outdoor Air Flow Rate During Heating Operation {m3/s}",
			"  autosize,                 !- Outdoor Air Flow Rate When No Cooling or Heating is Needed {m3/s}",
			"  VRFFanSchedule,           !- Supply Air Fan Operating Mode Schedule Name",
			"  drawthrough,              !- Supply Air Fan Placement",
			"  Fan:OnOff,                !- Supply Air Fan Object Type",
			"  TU1 VRF Supply Fan,       !- Supply Air Fan Object Name",
			"  OutdoorAir:Mixer,         !- Outside Air Mixer Object Type",
			"  TU1 OA Mixer,             !- Outside Air Mixer Object Name",
			"  COIL:Cooling:DX:VariableRefrigerantFlow,  !- Cooling Coil Object Type",
			"  TU1 VRF DX Cooling Coil,  !- Cooling Coil Object Name",
			"  COIL:Heating:DX:VariableRefrigerantFlow,  !- Heating Coil Object Type",
			"  TU1 VRF DX Heating Coil,  !- Heating Coil Object Name",
			"  30,                       !- Zone Terminal Unit On Parasitic Electric Energy Use {W}",
			"  20;                       !- Zone Terminal Unit Off Parasitic Electric Energy Use{ W }",
			" ",
			"Fan:OnOff,",
			"  TU1 VRF Supply Fan,       !- Name",
			"  VRFAvailSched,            !- Availability Schedule Name",
			"  0.7,                      !- Fan Total Efficiency",
			"  600.0,                    !- Pressure Rise{ Pa }",
			"  autosize,                 !- Maximum Flow Rate{ m3 / s }",
			"  0.9,                      !- Motor Efficiency",
			"  1.0,                      !- Motor In Airstream Fraction",
			"  TU1 VRF DX HCoil Outlet Node, !- Air Inlet Node Name",
			"  TU1 Outlet Node;          !- Air Outlet Node Name",
			" ",
			"OutdoorAir:Mixer,",
			"  TU1 OA Mixer,             !- Name",
			"  TU1 VRF DX CCoil Inlet Node, !- Mixed Air Node Name",
			"  Outside Air Inlet Node 1, !- Outdoor Air Stream Node Name",
			"  Relief Air Outlet Node 1, !- Relief Air Stream Node Name",
			"  TU1 Inlet Node;           !- Return Air Stream Node Name",
			" ",
			"OutdoorAir:NodeList,",
			"  OutsideAirInletNodes;     !- Node or NodeList Name 1",
			" ",
			"NodeList,",
			"  OutsideAirInletNodes, !- Name",
			"  Outside Air Inlet Node 1, !- Node 1 Name",
			"  MyVRFOANode;  !- Node 1 Name",
			" ",
			"COIL:Cooling:DX:VariableRefrigerantFlow,",
			"  TU1 VRF DX Cooling Coil, !- Name",
			"  VRFAvailSched,           !- Availability Schedule Name",
			"  autosize,                !- Gross Rated Total Cooling Capacity {W}",
			"  autosize,                !- Gross Rated Sensible Heat Ratio",
			"  autosize,                !- Rated Air Flow Rate {m3/s}",
			"  VRFTUCoolCapFT,          !- Cooling Capacity Ratio Modifier Function of Temperature Curve Name",
			"  VRFACCoolCapFFF,         !- Cooling Capacity Modifier Curve Function of Flow Fraction Name",
			"  TU1 VRF DX CCoil Inlet Node,  !- Coil Air Inlet Node",
			"  TU1 VRF DX CCoil Outlet Node,  !- Coil Air Outlet Node",
			"  ;                        !- Name of Water Storage Tank for Condensate Collection",
			" ",
			"COIL:Heating:DX:VariableRefrigerantFlow,",
			"  TU1 VRF DX Heating Coil, !- Name",
			"  VRFAvailSched,           !- Availability Schedule",
			"  autosize,                !- Gross Rated Heating Capacity {W}",
			"  autosize,                !- Rated Air Flow Rate {m3/s}",
			"  TU1 VRF DX CCoil Outlet Node,  !- Coil Air Inlet Node",
			"  TU1 VRF DX HCoil Outlet Node,  !- Coil Air Outlet Node",
			"  VRFTUHeatCapFT,          !- Heating Capacity Ratio Modifier Function of Temperature Curve Name",
			"  VRFACCoolCapFFF;         !- Heating Capacity Modifier Function of Flow Fraction Curve Name",
			" ",
			"ScheduleTypeLimits,",
			"  Any Number;              !- Name",
			" ",
			"Schedule:Compact,",
			"  VRFAvailSched,           !- Name",
			"  Any Number,              !- Schedule Type Limits Name",
			"  Through: 3/31,           !- Field 1",
			"  For: AllDays,            !- Field 2",
			"  Until: 24:00,1.0,        !- Field 3",
			"  Through: 9/30,           !- Field 4",
			"  For: WeekDays,           !- Field 5",
			"  Until: 7:00,1.0,         !- Field 6",
			"  Until: 17:00,1.0,        !- Field 7",
			"  Until: 24:00,1.0,        !- Field 8",
			"  For: SummerDesignDay WinterDesignDay, !- Field 9",
			"  Until: 24:00,1.0,        !- Field 10",
			"  For: AllOtherDays,       !- Field 11",
			"  Until: 24:00,1.0,        !- Field 12",
			"  Through: 12/31,          !- Field 13",
			"  For: AllDays,            !- Field 14",
			"  Until: 24:00,1.0;        !- Field 15",
			" ",
			"Schedule:Compact,",
			"  VRFCondAvailSched,       !- Name",
			"  Any Number,              !- Schedule Type Limits Name",
			"  Through: 12/31,          !- Field 1",
			"  For: AllDays,            !- Field 2",
			"  Until: 12:00,1.0,        !- Field 3",
			"  Until: 13:00,1.0,        !- Field 4",
			"  Until: 24:00,1.0;        !- Field 5",
			" ",
			"Schedule:Compact,",
			"  VRFFanSchedule,          !- Name",
			"  Any Number,              !- Schedule Type Limits Name",
			"  Through: 12/31,          !- Field 1",
			"  For: AllDays,            !- Field 2",
			"  Until: 7:00,1.0,         !- Field 3",
			"  Until: 18:00,1.0,        !- Field 5",
			"  Until: 24:00,1.0;        !- Field 7",
			" ",
			"Curve:Biquadratic,",
			"  DefrostEIRSched,         !- Name",
			"  1.0,                     !- Coefficient1 Constant",
			"  0.0,                     !- Coefficient2 x",
			"  0.0,                     !- Coefficient3 x**2",
			"  0.0,                     !- Coefficient4 y",
			"  0.0,                     !- Coefficient5 y**2",
			"  0.0,                     !- Coefficient6 x*y",
			"  15,                      !- Minimum Value of x",
			"  24,                      !- Maximum Value of x",
			"  -5,                      !- Minimum Value of y",
			"  23,                      !- Maximum Value of y",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature,             !- Input Unit Type for Y",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Biquadratic,",
			"  VRFCoolCapFT,            !- Name",
			"  0.576882692,             !- Coefficient1 Constant",
			"  0.017447952,             !- Coefficient2 x",
			"  0.000583269,             !- Coefficient3 x**2",
			"  -1.76324E-06,            !- Coefficient4 y",
			"  -7.474E-09,              !- Coefficient5 y**2",
			"  -1.30413E-07,            !- Coefficient6 x*y",
			"  15,                      !- Minimum Value of x",
			"  24,                      !- Maximum Value of x",
			"  -5,                      !- Minimum Value of y",
			"  23,                      !- Maximum Value of y",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature,             !- Input Unit Type for Y",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Cubic,",
			"  VRFCoolCapFTBoundary,    !- Name",
			"  25.73473775,             !- Coefficient1 Constant",
			"  -0.03150043,             !- Coefficient2 x",
			"  -0.01416595,             !- Coefficient3 x**2",
			"  0,                       !- Coefficient4 x**3",
			"  11,                      !- Minimum Value of x",
			"  30,                      !- Maximum Value of x",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature;             !- Output Unit Type",
			" ",
			"Curve:Biquadratic,",
			"  VRFCoolCapFTHi,          !- Name",
			"  0.6867358,               !- Coefficient1 Constant",
			"  0.0207631,               !- Coefficient2 x",
			"  0.0005447,               !- Coefficient3 x**2",
			"  -0.0016218,              !- Coefficient4 y",
			"  -4.259E-07,              !- Coefficient5 y**2",
			"  -0.0003392,              !- Coefficient6 x*y",
			"  15,                      !- Minimum Value of x",
			"  24,                      !- Maximum Value of x",
			"  16,                      !- Minimum Value of y",
			"  43,                      !- Maximum Value of y",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature,             !- Input Unit Type for Y",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Biquadratic,",
			"  VRFCoolEIRFT,            !- Name",
			"  0.989010541,             !- Coefficient1 Constant",
			"  -0.02347967,             !- Coefficient2 x",
			"  0.000199711,             !- Coefficient3 x**2",
			"  0.005968336,             !- Coefficient4 y",
			"  -1.0289E-07,             !- Coefficient5 y**2",
			"  -0.00015686,             !- Coefficient6 x*y",
			"  15,                      !- Minimum Value of x",
			"  24,                      !- Maximum Value of x",
			"  -5,                      !- Minimum Value of y",
			"  23,                      !- Maximum Value of y",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature,             !- Input Unit Type for Y",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Cubic,",
			"  VRFCoolEIRFTBoundary,    !- Name",
			"  25.73473775,             !- Coefficient1 Constant",
			"  -0.03150043,             !- Coefficient2 x",
			"  -0.01416595,             !- Coefficient3 x**2",
			"  0,                       !- Coefficient4 x**3",
			"  15,                      !- Minimum Value of x",
			"  24,                      !- Maximum Value of x",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature;             !- Output Unit Type",
			" ",
			"Curve:Biquadratic,",
			"  VRFCoolEIRFTHi,          !- Name",
			"  0.14351470,              !- Coefficient1 Constant",
			"  0.01860035,              !- Coefficient2 x",
			"  -0.0003954,              !- Coefficient3 x**2",
			"  0.02485219,              !- Coefficient4 y",
			"  0.00016329,              !- Coefficient5 y**2",
			"  -0.0006244,              !- Coefficient6 x*y",
			"  15,                      !- Minimum Value of x",
			"  24,                      !- Maximum Value of x",
			"  16,                      !- Minimum Value of y",
			"  43,                      !- Maximum Value of y",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature,             !- Input Unit Type for Y",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Cubic,",
			"  CoolingEIRLowPLR,        !- Name",
			"  0.4628123,               !- Coefficient1 Constant",
			"  -1.0402406,              !- Coefficient2 x",
			"  2.17490997,              !- Coefficient3 x**2",
			"  -0.5974817,              !- Coefficient4 x**3",
			"  0,                       !- Minimum Value of x",
			"  1,                       !- Maximum Value of x",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature;             !- Output Unit Type",
			" ",
			"Curve:Quadratic,",
			"  CoolingEIRHiPLR,         !- Name",
			"  1.0,                     !- Coefficient1 Constant",
			"  0.0,                     !- Coefficient2 x",
			"  0.0,                     !- Coefficient3 x**2",
			"  1.0,                     !- Minimum Value of x",
			"  1.5,                     !- Maximum Value of x",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Dimensionless,           !- Input Unit Type for X",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Linear,",
			"  CoolingCombRatio,        !- Name",
			"  0.618055,                !- Coefficient1 Constant",
			"  0.381945,                !- Coefficient2 x",
			"  1.0,                     !- Minimum Value of x",
			"  1.5,                     !- Maximum Value of x",
			"  1.0,                     !- Minimum Curve Output",
			"  1.2,                     !- Maximum Curve Output",
			"  Dimensionless,           !- Input Unit Type for X",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"CURVE:QUADRATIC,",
			"  VRFCPLFFPLR,             !- Name",
			"  0.85,                    !- Coefficient1 Constant",
			"  0.15,                    !- Coefficient2 x",
			"  0.0,                     !- Coefficient3 x**2",
			"  0.0,                     !- Minimum Value of x",
			"  1.0,                     !- Maximum Value of x",
			"  0.85,                    !- Minimum Curve Output",
			"  1.0,                     !- Maximum Curve Output",
			"  Dimensionless,           !- Input Unit Type for X",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Biquadratic,",
			"  VRFHeatCapFT,            !- Name",
			"  1.014599599,             !- Coefficient1 Constant",
			"  -0.002506703,            !- Coefficient2 x",
			"  -0.000141599,            !- Coefficient3 x**2",
			"  0.026931595,             !- Coefficient4 y",
			"  1.83538E-06,             !- Coefficient5 y**2",
			"  -0.000358147,            !- Coefficient6 x*y",
			"  15,                      !- Minimum Value of x",
			"  27,                      !- Maximum Value of x",
			"  -20,                     !- Minimum Value of y",
			"  15,                      !- Maximum Value of y",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature,             !- Input Unit Type for Y",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Cubic,",
			"  VRFHeatCapFTBoundary,    !- Name",
			"  -7.6000882,              !- Coefficient1 Constant",
			"  3.05090016,              !- Coefficient2 x",
			"  -0.1162844,              !- Coefficient3 x**2",
			"  0.0,                     !- Coefficient4 x**3",
			"  15,                      !- Minimum Value of x",
			"  27,                      !- Maximum Value of x",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature;             !- Output Unit Type",
			" ",
			"Curve:Biquadratic,",
			"  VRFHeatCapFTHi,          !- Name",
			"  1.161134821,             !- Coefficient1 Constant",
			"  0.027478868,             !- Coefficient2 x",
			"  -0.00168795,             !- Coefficient3 x**2",
			"  0.001783378,             !- Coefficient4 y",
			"  2.03208E-06,             !- Coefficient5 y**2",
			"  -6.8969E-05,             !- Coefficient6 x*y",
			"  15,                      !- Minimum Value of x",
			"  27,                      !- Maximum Value of x",
			"  -10,                     !- Minimum Value of y",
			"  15,                      !- Maximum Value of y",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature,             !- Input Unit Type for Y",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Biquadratic,",
			"  VRFHeatEIRFT,            !- Name",
			"  0.87465501,              !- Coefficient1 Constant",
			"  -0.01319754,             !- Coefficient2 x",
			"  0.00110307,              !- Coefficient3 x**2",
			"  -0.0133118,              !- Coefficient4 y",
			"  0.00089017,              !- Coefficient5 y**2",
			"  -0.00012766,             !- Coefficient6 x*y",
			"  15,                      !- Minimum Value of x",
			"  27,                      !- Maximum Value of x",
			"  -20,                     !- Minimum Value of y",
			"  12,                      !- Maximum Value of y",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature,             !- Input Unit Type for Y",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Cubic,",
			"  VRFHeatEIRFTBoundary,    !- Name",
			"  -7.6000882,              !- Coefficient1 Constant",
			"  3.05090016,              !- Coefficient2 x",
			"  -0.1162844,              !- Coefficient3 x**2",
			"  0.0,                     !- Coefficient4 x**3",
			"  15,                      !- Minimum Value of x",
			"  27,                      !- Maximum Value of x",
			"  -20,                     !- Minimum Curve Output",
			"  15,                      !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature;             !- Output Unit Type",
			" ",
			"Curve:Biquadratic,",
			"  VRFHeatEIRFTHi,          !- Name",
			"  2.504005146,             !- Coefficient1 Constant",
			"  -0.05736767,             !- Coefficient2 x",
			"  4.07336E-05,             !- Coefficient3 x**2",
			"  -0.12959669,             !- Coefficient4 y",
			"  0.00135839,              !- Coefficient5 y**2",
			"  0.00317047,              !- Coefficient6 x*y",
			"  15,                      !- Minimum Value of x",
			"  27,                      !- Maximum Value of x",
			"  -10,                     !- Minimum Value of y",
			"  15,                      !- Maximum Value of y",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature,             !- Input Unit Type for Y",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Cubic,",
			"  HeatingEIRLowPLR,        !- Name",
			"  0.1400093,               !- Coefficient1 Constant",
			"  0.6415002,               !- Coefficient2 x",
			"  0.1339047,               !- Coefficient3 x**2",
			"  0.0845859,               !- Coefficient4 x**3",
			"  0,                       !- Minimum Value of x",
			"  1,                       !- Maximum Value of x",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Dimensionless,           !- Input Unit Type for X",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Quadratic,",
			"  HeatingEIRHiPLR,         !- Name",
			"  2.4294355,               !- Coefficient1 Constant",
			"  -2.235887,               !- Coefficient2 x",
			"  0.8064516,               !- Coefficient3 x**2",
			"  1.0,                     !- Minimum Value of x",
			"  1.5,                     !- Maximum Value of x",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Dimensionless,           !- Input Unit Type for X",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Linear,",
			"  HeatingCombRatio,        !- Name",
			"  0.96034,                 !- Coefficient1 Constant",
			"  0.03966,                 !- Coefficient2 x",
			"  1.0,                     !- Minimum Value of x",
			"  1.5,                     !- Maximum Value of x",
			"  1.0,                     !- Minimum Curve Output",
			"  1.023,                   !- Maximum Curve Output",
			"  Dimensionless,           !- Input Unit Type for X",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Biquadratic,",
			"  CoolingLengthCorrectionFactor,  !- Name",
			"  1.0693794,               !- Coefficient1 Constant",
			"  -0.0014951,              !- Coefficient2 x",
			"  2.56E-06,                !- Coefficient3 x**2",
			"  -0.1151104,              !- Coefficient4 y",
			"  0.0511169,               !- Coefficient5 y**2",
			"  -0.0004369,              !- Coefficient6 x*y",
			"  8,                       !- Minimum Value of x",
			"  175,                     !- Maximum Value of x",
			"  0.5,                     !- Minimum Value of y",
			"  1.5,                     !- Maximum Value of y",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature,             !- Input Unit Type for Y",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Cubic,",
			"  VRFTUCoolCapFT,          !- Name",
			"  0.504547273506488,       !- Coefficient1 Constant",
			"  0.0288891279198444,      !- Coefficient2 x",
			"  -0.000010819418650677,   !- Coefficient3 x**2",
			"  0.0000101359395177008,   !- Coefficient4 x**3",
			"  0.0,                     !- Minimum Value of x",
			"  50.0,                    !- Maximum Value of x",
			"  0.5,                     !- Minimum Curve Output",
			"  1.5,                     !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Quadratic,",
			"  VRFACCoolCapFFF,         !- Name",
			"  0.8,                     !- Coefficient1 Constant",
			"  0.2,                     !- Coefficient2 x",
			"  0.0,                     !- Coefficient3 x**2",
			"  0.5,                     !- Minimum Value of x",
			"  1.5;                     !- Maximum Value of x",
			" ",
			"Curve:Cubic,",
			"  VRFTUHeatCapFT,          !- Name",
			"  -0.390708928227928,      !- Coefficient1 Constant",
			"  0.261815023760162,       !- Coefficient2 x",
			"  -0.0130431603151873,     !- Coefficient3 x**2",
			"  0.000178131745997821,    !- Coefficient4 x**3",
			"  0.0,                     !- Minimum Value of x",
			"  50.0,                    !- Maximum Value of x",
			"  0.5,                     !- Minimum Curve Output",
			"  1.5,                     !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Dimensionless;           !- Output Unit Type",
		} );

		ASSERT_FALSE( process_idf( idf_objects ) );

		DataGlobals::BeginEnvrnFlag = true;
		DataSizing::CurZoneEqNum = 1;
		DataEnvironment::OutBaroPress = 101325; // sea level
		DataZoneEquipment::ZoneEquipInputsFilled = true; // denotes zone equipment has been read in
		StdRhoAir = PsyRhoAirFnPbTdbW( DataEnvironment::OutBaroPress, 20.0, 0.0 );
		ZoneEqSizing.allocate( 1 );
		ZoneSizingRunDone = true;
		ZoneEqSizing( CurZoneEqNum ).DesignSizeFromParent = false;
		ZoneEqSizing( CurZoneEqNum ).SizingMethod.allocate( 25 );
		ZoneEqSizing( CurZoneEqNum ).SizingMethod( DataHVACGlobals::SystemAirflowSizing ) = DataSizing::SupplyAirFlowRate;

		FinalZoneSizing.allocate( 1 );
		FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow = 0.566337; // 400 cfm * 3 tons = 1200 cfm
		FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow = 0.566337;

		ZoneSysEnergyDemand.allocate( 1 );

		ProcessScheduleInput(); // read schedules
		GetCurveInput(); // read curves
		GetZoneData( ErrorsFound ); // read zone data
		EXPECT_FALSE( ErrorsFound );

		DXCoils::GetCoilsInputFlag = true; // remove this when clear_state gets added to DXCoils
		GlobalNames::NumCoils = 0; // remove this when clear_state gets added to GlobalNames
		GlobalNames::CoilNames.deallocate(); // remove this when clear_state gets added to GlobalNames

		GetZoneEquipmentData(); // read equipment list and connections
		ZoneInletAirNode = GetVRFTUZoneInletAirNode( VRFTUNum ); // trigger GetVRFInput by calling a mining function

		Schedule( VRF( VRFCond ).SchedPtr ).CurrentValue = 1.0; // enable the VRF condenser
		Schedule( VRFTU( VRFTUNum ).SchedPtr ).CurrentValue = 1.0; // enable the terminal unit
		Schedule( VRFTU( VRFTUNum ).FanAvailSchedPtr ).CurrentValue = 1.0; // turn on fan
		Schedule( VRFTU( VRFTUNum ).FanOpModeSchedPtr ).CurrentValue = 0.0; // set cycling fan operating mode

		// Test coil sizing

		ZoneSysEnergyDemand( CurZoneNum ).RemainingOutputRequired = 0.0; // set load = 0
		ZoneSysEnergyDemand( CurZoneNum ).RemainingOutputReqToCoolSP = 0.0;
		ZoneSysEnergyDemand( CurZoneNum ).RemainingOutputReqToHeatSP = 0.0;

		FinalZoneSizing( CurZoneEqNum ).ZoneRetTempAtCoolPeak = 26.66667;
		FinalZoneSizing( CurZoneEqNum ).ZoneHumRatAtCoolPeak = 0.01117049470250416; // AHRI condition at 80 F db / 67 F wb
		Node( VRF( VRFCond ).CondenserNodeNum ).Temp = 35.0; // AHRI condition at 95 F db
		Node( VRFTU( VRFTUNum ).VRFTUOAMixerOANodeNum ).Temp = 35.0; // AHRI condition at 95 F db
		Node( VRFTU( VRFTUNum ).VRFTUOAMixerOANodeNum ).HumRat = 0.01; // don't care
		FinalZoneSizing( CurZoneEqNum ).CoolDDNum = 1;
		FinalZoneSizing( CurZoneEqNum ).TimeStepNumAtCoolMax = 1;
		DesDayWeath.allocate( 1 );
		DesDayWeath( 1 ).Temp.allocate( 1 );
		DesDayWeath( FinalZoneSizing( CurZoneEqNum ).CoolDDNum ).Temp( FinalZoneSizing( CurZoneEqNum ).TimeStepNumAtCoolMax ) = 35.0;

		FinalZoneSizing( CurZoneEqNum ).CoolDesTemp = 13.1; // 55.58 F
		FinalZoneSizing( CurZoneEqNum ).CoolDesHumRat = 0.009297628698818194; // humrat at 12.77777 C db / 12.6 C wb

		SimulateVRF( VRFTU( VRFTUNum ).Name, CurZoneNum, FirstHVACIteration, SysOutputProvided, LatOutputProvided, ZoneEquipList( CurZoneEqNum ).EquipIndex( EquipPtr ) );

		ASSERT_EQ( 1, NumVRFCond );
		ASSERT_EQ( ZoneInletAirNode, ZoneEquipConfig( VRFTU( VRFTUNum ).ZoneNum ).InletNode( 1 ) ); // only 1 inlet node specified above in ZoneHVAC:EquipmentConnections
		ASSERT_EQ( 1.0, VRF( VRFCond ).CoolingCombinationRatio );
		ASSERT_EQ( 10749.071979211991, VRF( VRFCond ).CoolingCapacity );
		ASSERT_EQ( 10749.071979211991, VRF( VRFCond ).HeatingCapacity );
		EXPECT_EQ( 0.0, VRF( VRFCond ).DefrostPower );

		// test defrost operation Issue #4950 - Reverse cycle with timed defrost = 0

		// set OA node temperatures for heating where defrost should be active
		Node( VRF( VRFCond ).CondenserNodeNum ).Temp = VRF( VRFCond ).MaxOATDefrost - 1.0;
		Node( VRF( VRFCond ).CondenserNodeNum ).HumRat = 0.005;
		Node( VRFTU( VRFTUNum ).VRFTUOAMixerOANodeNum ).Temp = Node( VRF( VRFCond ).CondenserNodeNum ).Temp;

		// set zone load to heating
		ZoneSysEnergyDemand( CurZoneNum ).RemainingOutputRequired = VRF( VRFCond ).HeatingCapacity; // set load equal to the VRF heating capacity
		ZoneSysEnergyDemand( CurZoneNum ).RemainingOutputReqToCoolSP = VRF( VRFCond ).HeatingCapacity + 1000.0; // simulates a dual Tstat with load to cooling SP > load to heating SP
		ZoneSysEnergyDemand( CurZoneNum ).RemainingOutputReqToHeatSP = VRF( VRFCond ).HeatingCapacity;


		SimulateVRF( VRFTU( VRFTUNum ).Name, CurZoneNum, FirstHVACIteration, SysOutputProvided, LatOutputProvided, ZoneEquipList( CurZoneEqNum ).EquipIndex( EquipPtr ) );

		ASSERT_TRUE( VRF( VRFCond ).DefrostPower > 0.0 ); // defrost power should be greater than 0
		DefrostWatts = VRF( VRFCond ).VRFCondRTF * ( VRF( VRFCond ).HeatingCapacity / 1.01667 ) * VRF( VRFCond ).DefrostFraction;
		ASSERT_EQ( DefrostWatts, VRF( VRFCond ).DefrostPower ); // defrost power calculation check

		// test other ThermostatPriority control types
		ZoneThermostatSetPointHi.allocate( 1 );
		ZoneThermostatSetPointHi = 24.0;
		ZoneThermostatSetPointLo.allocate( 1 );
		ZoneThermostatSetPointLo = 21.0;
		TempControlType.allocate( 1 );
		TempControlType = 4;
		ZT.allocate( 1 );
		ZT = 25.0;

		ZoneSysEnergyDemand( CurZoneNum ).RemainingOutputRequired = -VRF( VRFCond ).CoolingCapacity; // set load equal to the VRF cooling capacity
		ZoneSysEnergyDemand( CurZoneNum ).RemainingOutputReqToCoolSP = -VRF( VRFCond ).CoolingCapacity;
		ZoneSysEnergyDemand( CurZoneNum ).RemainingOutputReqToHeatSP = -VRF( VRFCond ).CoolingCapacity - 1000.0;

		Node( VRF( VRFCond ).CondenserNodeNum ).Temp = 35.0; // AHRI condition at 95 F db
		Node( VRFTU( VRFTUNum ).VRFTUInletNodeNum ).Temp = 25.0;
		Node( VRFTU( VRFTUNum ).VRFTUOAMixerOANodeNum ).Temp = 35.0; // AHRI condition at 95 F db
		Node( VRFTU( VRFTUNum ).VRFTUOAMixerOANodeNum ).HumRat = 0.01; // don't care

		VRF( VRFCond ).MasterZonePtr = 0;
		VRF( VRFCond ).MasterZoneTUIndex = 0;
		VRF( VRFCond ).ThermostatPriority = ThermostatOffsetPriority;
		SimulateVRF( VRFTU( VRFTUNum ).Name, CurZoneNum, FirstHVACIteration, SysOutputProvided, LatOutputProvided, ZoneEquipList( CurZoneEqNum ).EquipIndex( EquipPtr ) );
		EXPECT_NEAR( SysOutputProvided, ZoneSysEnergyDemand( CurZoneNum ).RemainingOutputRequired, 5.0 ); // system output should be less than 0

	}

	TEST_F( EnergyPlusFixture, VRFTest_SysCurve_GetInputFailers ) {
		// Author: R. Raustad, FSEC

		bool ErrorsFound( false );        // function returns true on error
		int VRFTUNum( 1 );                // index to VRF terminal unit

		std::string const idf_objects = delimited_string( {
			"Version,8.3;",
			" ",
			"AirConditioner:VariableRefrigerantFlow,",
			"  VRF Heat Pump,           !- Heat Pump Name",
			"  VRFCondAvailSched,       !- Availability Schedule Name",
			"  autosize,                !- Gross Rated Total Cooling Capacity {W}",
			"  3.2917,                  !- Gross Rated Cooling COP {W/W}",
			"  -5,                      !- Minimum Outdoor Temperature in Cooling Mode {C}",
			"  43,                      !- Maximum Outdoor Temperature in Cooling Mode {C}",
			"  VRFCoolCapFT,            !- Cooling Capacity Ratio Modifier Function of Low Temperature Curve Name",
			"  VRFCoolCapFTBoundary,    !- Cooling Capacity Ratio Boundary Curve Name",
			"  VRFCoolCapFTHi,          !- Cooling Capacity Ratio Modifier Function of High Temperature Curve Name",
			"  VRFCoolEIRFT,            !- Cooling Energy Input Ratio Modifier Function of Low Temperature Curve Name",
			"  VRFCoolEIRFTBoundary,    !- Cooling Energy Input Ratio Boundary Curve Name",
			"  VRFCoolEIRFTHi,          !- Cooling Energy Input Ratio Modifier Function of High Temperature Curve Name",
			"  CoolingEIRLowPLR,        !- Cooling Energy Input Ratio Modifier Function of Low Part-Load Ratio Curve Name",
			"  CoolingEIRHiPLR,         !- Cooling Energy Input Ratio Modifier Function of High Part-Load Ratio Curve Name",
			"  CoolingCombRatio,        !- Cooling Combination Ratio Correction Factor Curve Name",
			"  VRFCPLFFPLR,             !- Cooling Part-Load Fraction Correlation Curve Name",
			"  autosize,                !- Gross Rated Heating Capacity {W}",
			"  ,                        !- Rated Heating Capacity Sizing Ratio {W/W}",
			"  3.5484,                  !- Gross Rated Heating COP {W/W}",
			"  -20,                     !- Minimum Outdoor Temperature in Heating Mode {C}",
			"  20,                      !- Maximum Outdoor Temperature in Heating Mode {C}",
			"  VRFHeatCapFT,            !- Heating Capacity Ratio Modifier Function of Low Temperature Curve Name",
			"  VRFHeatCapFTBoundary,    !- Heating Capacity Ratio Boundary Curve Name",
			"  VRFHeatCapFTHi,          !- Heating Capacity Ratio Modifier Function of High Temperature Curve Name",
			"  VRFHeatEIRFT,            !- Heating Energy Input Ratio Modifier Function of Low Temperature Curve Name",
			"  VRFHeatEIRFTBoundary,    !- Heating Energy Input Ratio Boundary Curve Name",
			"  VRFHeatEIRFTHi,          !- Heating Energy Input Ratio Modifier Function of High Temperature Curve Name",
			"  WetBulbTemperature,      !- Heating Performance Curve Outdoor Temperature Type",
			"  HeatingEIRLowPLR,        !- Heating Energy Input Ratio Modifier Function of Low Part-Load Ratio Curve Name",
			"  HeatingEIRHiPLR,         !- Heating Energy Input Ratio Modifier Function of High Part-Load Ratio Curve Name",
			"  HeatingCombRatio,        !- Heating Combination Ratio Correction Factor Curve Name",
			"  VRFCPLFFPLR,             !- Heating Part-Load Fraction Correlation Curve Name",
			"  0.25,                    !- Minimum Heat Pump Part-Load Ratio {dimensionless}",
			"  SPACE1-1,                !- Zone Name for Master Thermostat Location",
			"  LoadPriority,            !- Master Thermostat Priority Control Type",
			"  ,                        !- Thermostat Priority Schedule Name",
			"  VRF Heat Pump TU List,   !- Zone Terminal Unit List Name",
			"  No,                      !- Heat Pump Waste Heat Recovery",
			"  30,                      !- Equivalent Piping Length used for Piping Correction Factor in Cooling Mode {m}",
			"  10,                      !- Vertical Height used for Piping Correction Factor {m}",
			"  CoolingLengthCorrectionFactor,  !- Piping Correction Factor for Length in Cooling Mode Curve Name",
			"  -0.000386,               !- Piping Correction Factor for Height in Cooling Mode Coefficient {1/m}",
			"  30,                      !- Equivalent Piping Length used for Piping Correction Factor in Heating Mode {m}",
			"  ,                        !- Piping Correction Factor for Length in Heating Mode Curve Name",
			"  ,                        !- Piping Correction Factor for Height in Heating Mode Coefficient {1/m}",
			"  15,                      !- Crankcase Heater Power per Compressor {W}",
			"  3,                       !- Number of Compressors {dimensionless}",
			"  0.33,                    !- Ratio of Compressor Size to Total Compressor Capacity {W/W}",
			"  7,                       !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater {C}",
			"  ReverseCycle,            !- Defrost Strategy",
			"  Timed,                   !- Defrost Control",
			"  DefrostEIRSched,         !- Defrost Energy Input Ratio Modifier Function of Temperature Curve Name",
			"  ,                        !- Defrost Time Period Fraction {dimensionless}",
			"  autosize,                !- Resistive Defrost Heater Capacity {W}",
			"  7,                       !- Maximum Outdoor Dry-bulb Temperature for Defrost Operation {C}",
			"  AirCooled,               !- Condenser Type",
			"  MyVRFOANode,             !- Condenser Inlet Node Name",
			"  ,                        !- Condenser Outlet Node Name",
			"  ,                        !- Water Condenser Volume Flow Rate {m3/s}",
			"  ,                        !- Evaporative Condenser Effectiveness {dimensionless}",
			"  ,                        !- Evaporative Condenser Air Flow Rate {m3/s}",
			"  0,                       !- Evaporative Condenser Pump Rated Power Consumption {W}",
			"  ,                        !- Supply Water Storage Tank Name",
			"  0,                       !- Basin Heater Capacity {W/K}",
			"  ,                        !- Basin Heater Setpoint Temperature {C}",
			"  ,                        !- Basin Heater Operating Schedule Name",
			"  Electricity;             !- Fuel Type",
			" ",
			"Zone,",
			"  SPACE1-1,                !- Name",
			"  0,                       !- Direction of Relative North {deg}",
			"  0,                       !- X Origin {m}",
			"  0,                       !- Y Origin {m}",
			"  0,                       !- Z Origin {m}",
			"  1,                       !- Type",
			"  1,                       !- Multiplier",
			"  2.438400269,             !- Ceiling Height {m}",
			"  239.247360229;           !- Volume {m3}",
			" ",
			"ZoneHVAC:EquipmentConnections,",
			"  SPACE1-1,                !- Zone Name",
			"  SPACE1-1 Eq,             !- Zone Conditioning Equipment List Name",
			"  TU1 Outlet Node,         !- Zone Air Inlet Node or NodeList Name",
			"  TU1 Inlet Node,          !- Zone Air Exhaust Node or NodeList Name",
			"  SPACE1-1 Node,           !- Zone Air Node Name",
			"  SPACE1-1 Out Node;       !- Zone Return Air Node Name", // not used anywhere else in the example file
			" ",
			"ZoneHVAC:EquipmentList,",
			"  SPACE1-1 Eq,             !- Name",
			"  ZoneHVAC:TerminalUnit:VariableRefrigerantFlow,  !- Zone Equipment 1 Object Type",
			"  TU1,                     !- Zone Equipment 1 Name",
			"  1,                       !- Zone Equipment 1 Cooling Sequence",
			"  1;                       !- Zone Equipment 1 Heating or No-Load Sequence",
			" ",
			"ZoneTerminalUnitList,",
			"  VRF Heat Pump TU List,    !- Zone Terminal Unit List Name",
			"  TU 1;                     !- Zone Terminal Unit Name 1", // different terminal unit name than specified in ZoneHVAC:TerminalUnit:VariableRefrigerantFlow
			" ",
			"ZoneHVAC:TerminalUnit:VariableRefrigerantFlow,",
			"  TU1,                      !- Zone Terminal Unit Name",
			"  VRFAvailSched,            !- Terminal Unit Availability Schedule",
			"  TU1 Inlet Node,           !- Terminal Unit Air Inlet Node Name",
			"  TU1 Outlet Node,          !- Terminal Unit Air Outlet Node Name",
			"  autosize,                 !- Supply Air Flow Rate During Cooling Operation {m3/s}",
			"  autosize,                 !- Supply Air Flow Rate When No Cooling is Needed {m3/s}",
			"  autosize,                 !- Supply Air Flow Rate During Heating Operation {m3/s}",
			"  autosize,                 !- Supply Air Flow Rate When No Heating is Needed {m3/s}",
			"  autosize,                 !- Outdoor Air Flow Rate During Cooling Operation {m3/s}",
			"  autosize,                 !- Outdoor Air Flow Rate During Heating Operation {m3/s}",
			"  autosize,                 !- Outdoor Air Flow Rate When No Cooling or Heating is Needed {m3/s}",
			"  VRFFanSchedule,           !- Supply Air Fan Operating Mode Schedule Name",
			"  drawthrough,              !- Supply Air Fan Placement",
			"  Fan:ConstantVolume,       !- Supply Air Fan Object Type",
			"  TU1 VRF Supply Fan,       !- Supply Air Fan Object Name",
			"  OutdoorAir:Mixer,         !- Outside Air Mixer Object Type",
			"  TU1 OA Mixer,             !- Outside Air Mixer Object Name",
			"  COIL:Cooling:DX:VariableRefrigerantFlow,  !- Cooling Coil Object Type",
			"  TU1 VRF DX Cooling Coil,  !- Cooling Coil Object Name",
			"  COIL:Heating:DX:VariableRefrigerantFlow,  !- Heating Coil Object Type",
			"  TU1 VRF DX Heating Coil,  !- Heating Coil Object Name",
			"  30,                       !- Zone Terminal Unit On Parasitic Electric Energy Use {W}",
			"  20;                       !- Zone Terminal Unit Off Parasitic Electric Energy Use{ W }",
			" ",
			"Fan:ConstantVolume,",
			"  TU1 VRF Supply Fan,       !- Name",
			"  VRFAvailSched,            !- Availability Schedule Name",
			"  0.7,                      !- Fan Total Efficiency",
			"  600.0,                    !- Pressure Rise{ Pa }",
			"  autosize,                 !- Maximum Flow Rate{ m3 / s }",
			"  0.9,                      !- Motor Efficiency",
			"  1.0,                      !- Motor In Airstream Fraction",
			"  TU1 VRF DX HCoil Outlet Node, !- Air Inlet Node Name",
			"  TU1 Outlet Node;          !- Air Outlet Node Name",
			" ",
			"OutdoorAir:Mixer,",
			"  TU1 OA Mixer,             !- Name",
			"  TU1 VRF DX CCoil Inlet Node, !- Mixed Air Node Name",
			"  Outside Air Inlet Node 1, !- Outdoor Air Stream Node Name",
			"  Relief Air Outlet Node 1, !- Relief Air Stream Node Name",
			"  TU1 Inlet Node;           !- Return Air Stream Node Name",
			" ",
			"OutdoorAir:NodeList,",
			"  OutsideAirInletNodes;     !- Node or NodeList Name 1",
			" ",
			"NodeList,",
			"  OutsideAirInletNodes, !- Name",
			"  Outside Air Inlet Node 1, !- Node 1 Name",
			"  MyVRFOANode;  !- Node 1 Name",
			" ",
			"COIL:Cooling:DX:VariableRefrigerantFlow,",
			"  TU1 VRF DX Cooling Coil, !- Name",
			"  VRFAvailSched,           !- Availability Schedule Name",
			"  autosize,                !- Gross Rated Total Cooling Capacity {W}",
			"  autosize,                !- Gross Rated Sensible Heat Ratio",
			"  autosize,                !- Rated Air Flow Rate {m3/s}",
			"  VRFTUCoolCapFT,          !- Cooling Capacity Ratio Modifier Function of Temperature Curve Name",
			"  VRFACCoolCapFFF,         !- Cooling Capacity Modifier Curve Function of Flow Fraction Name",
			"  TU1 VRF DX CCoil Inlet Node,  !- Coil Air Inlet Node",
			"  TU1 VRF DX CCoil Outlet Node,  !- Coil Air Outlet Node",
			"  ;                        !- Name of Water Storage Tank for Condensate Collection",
			" ",
			"COIL:Heating:DX:VariableRefrigerantFlow,",
			"  TU1 VRF DX Heating Coil, !- Name",
			"  VRFAvailSched,           !- Availability Schedule",
			"  autosize,                !- Gross Rated Heating Capacity {W}",
			"  autosize,                !- Rated Air Flow Rate {m3/s}",
			"  TU1 VRF DX CCoil Outlet Node,  !- Coil Air Inlet Node",
			"  TU1 VRF DX HCoil Outlet Node,  !- Coil Air Outlet Node",
			"  VRFTUHeatCapFT,          !- Heating Capacity Ratio Modifier Function of Temperature Curve Name",
			"  VRFACCoolCapFFF;         !- Heating Capacity Modifier Function of Flow Fraction Curve Name",
			" ",
			"ScheduleTypeLimits,",
			"  Any Number;              !- Name",
			" ",
			"Schedule:Compact,",
			"  VRFAvailSched,           !- Name",
			"  Any Number,              !- Schedule Type Limits Name",
			"  Through: 3/31,           !- Field 1",
			"  For: AllDays,            !- Field 2",
			"  Until: 24:00,1.0,        !- Field 3",
			"  Through: 9/30,           !- Field 4",
			"  For: WeekDays,           !- Field 5",
			"  Until: 7:00,1.0,         !- Field 6",
			"  Until: 17:00,1.0,        !- Field 7",
			"  Until: 24:00,1.0,        !- Field 8",
			"  For: SummerDesignDay WinterDesignDay, !- Field 9",
			"  Until: 24:00,1.0,        !- Field 10",
			"  For: AllOtherDays,       !- Field 11",
			"  Until: 24:00,1.0,        !- Field 12",
			"  Through: 12/31,          !- Field 13",
			"  For: AllDays,            !- Field 14",
			"  Until: 24:00,1.0;        !- Field 15",
			" ",
			"Schedule:Compact,",
			"  VRFCondAvailSched,       !- Name",
			"  Any Number,              !- Schedule Type Limits Name",
			"  Through: 12/31,          !- Field 1",
			"  For: AllDays,            !- Field 2",
			"  Until: 12:00,1.0,        !- Field 3",
			"  Until: 13:00,1.0,        !- Field 4",
			"  Until: 24:00,1.0;        !- Field 5",
			" ",
			"Schedule:Compact,",
			"  VRFFanSchedule,          !- Name",
			"  Any Number,              !- Schedule Type Limits Name",
			"  Through: 12/31,          !- Field 1",
			"  For: AllDays,            !- Field 2",
			"  Until: 7:00,1.0,         !- Field 3",
			"  Until: 18:00,1.0,        !- Field 5",
			"  Until: 24:00,1.0;        !- Field 7",
			" ",
			"Curve:Biquadratic,",
			"  DefrostEIRSched,         !- Name",
			"  1.0,                     !- Coefficient1 Constant",
			"  0.0,                     !- Coefficient2 x",
			"  0.0,                     !- Coefficient3 x**2",
			"  0.0,                     !- Coefficient4 y",
			"  0.0,                     !- Coefficient5 y**2",
			"  0.0,                     !- Coefficient6 x*y",
			"  15,                      !- Minimum Value of x",
			"  24,                      !- Maximum Value of x",
			"  -5,                      !- Minimum Value of y",
			"  23,                      !- Maximum Value of y",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature,             !- Input Unit Type for Y",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Biquadratic,",
			"  VRFCoolCapFT,            !- Name",
			"  0.576882692,             !- Coefficient1 Constant",
			"  0.017447952,             !- Coefficient2 x",
			"  0.000583269,             !- Coefficient3 x**2",
			"  -1.76324E-06,            !- Coefficient4 y",
			"  -7.474E-09,              !- Coefficient5 y**2",
			"  -1.30413E-07,            !- Coefficient6 x*y",
			"  15,                      !- Minimum Value of x",
			"  24,                      !- Maximum Value of x",
			"  -5,                      !- Minimum Value of y",
			"  23,                      !- Maximum Value of y",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature,             !- Input Unit Type for Y",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Cubic,",
			"  VRFCoolCapFTBoundary,    !- Name",
			"  25.73473775,             !- Coefficient1 Constant",
			"  -0.03150043,             !- Coefficient2 x",
			"  -0.01416595,             !- Coefficient3 x**2",
			"  0,                       !- Coefficient4 x**3",
			"  11,                      !- Minimum Value of x",
			"  30,                      !- Maximum Value of x",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature;             !- Output Unit Type",
			" ",
			"Curve:Biquadratic,",
			"  VRFCoolCapFTHi,          !- Name",
			"  0.6867358,               !- Coefficient1 Constant",
			"  0.0207631,               !- Coefficient2 x",
			"  0.0005447,               !- Coefficient3 x**2",
			"  -0.0016218,              !- Coefficient4 y",
			"  -4.259E-07,              !- Coefficient5 y**2",
			"  -0.0003392,              !- Coefficient6 x*y",
			"  15,                      !- Minimum Value of x",
			"  24,                      !- Maximum Value of x",
			"  16,                      !- Minimum Value of y",
			"  43,                      !- Maximum Value of y",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature,             !- Input Unit Type for Y",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Biquadratic,",
			"  VRFCoolEIRFT,            !- Name",
			"  0.989010541,             !- Coefficient1 Constant",
			"  -0.02347967,             !- Coefficient2 x",
			"  0.000199711,             !- Coefficient3 x**2",
			"  0.005968336,             !- Coefficient4 y",
			"  -1.0289E-07,             !- Coefficient5 y**2",
			"  -0.00015686,             !- Coefficient6 x*y",
			"  15,                      !- Minimum Value of x",
			"  24,                      !- Maximum Value of x",
			"  -5,                      !- Minimum Value of y",
			"  23,                      !- Maximum Value of y",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature,             !- Input Unit Type for Y",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Cubic,",
			"  VRFCoolEIRFTBoundary,    !- Name",
			"  25.73473775,             !- Coefficient1 Constant",
			"  -0.03150043,             !- Coefficient2 x",
			"  -0.01416595,             !- Coefficient3 x**2",
			"  0,                       !- Coefficient4 x**3",
			"  15,                      !- Minimum Value of x",
			"  24,                      !- Maximum Value of x",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature;             !- Output Unit Type",
			" ",
			"Curve:Biquadratic,",
			"  VRFCoolEIRFTHi,          !- Name",
			"  0.14351470,              !- Coefficient1 Constant",
			"  0.01860035,              !- Coefficient2 x",
			"  -0.0003954,              !- Coefficient3 x**2",
			"  0.02485219,              !- Coefficient4 y",
			"  0.00016329,              !- Coefficient5 y**2",
			"  -0.0006244,              !- Coefficient6 x*y",
			"  15,                      !- Minimum Value of x",
			"  24,                      !- Maximum Value of x",
			"  16,                      !- Minimum Value of y",
			"  43,                      !- Maximum Value of y",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature,             !- Input Unit Type for Y",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Cubic,",
			"  CoolingEIRLowPLR,        !- Name",
			"  0.4628123,               !- Coefficient1 Constant",
			"  -1.0402406,              !- Coefficient2 x",
			"  2.17490997,              !- Coefficient3 x**2",
			"  -0.5974817,              !- Coefficient4 x**3",
			"  0,                       !- Minimum Value of x",
			"  1,                       !- Maximum Value of x",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature;             !- Output Unit Type",
			" ",
			"Curve:Quadratic,",
			"  CoolingEIRHiPLR,         !- Name",
			"  1.0,                     !- Coefficient1 Constant",
			"  0.0,                     !- Coefficient2 x",
			"  0.0,                     !- Coefficient3 x**2",
			"  1.0,                     !- Minimum Value of x",
			"  1.5,                     !- Maximum Value of x",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Dimensionless,           !- Input Unit Type for X",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Linear,",
			"  CoolingCombRatio,        !- Name",
			"  0.618055,                !- Coefficient1 Constant",
			"  0.381945,                !- Coefficient2 x",
			"  1.0,                     !- Minimum Value of x",
			"  1.5,                     !- Maximum Value of x",
			"  1.0,                     !- Minimum Curve Output",
			"  1.2,                     !- Maximum Curve Output",
			"  Dimensionless,           !- Input Unit Type for X",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"CURVE:QUADRATIC,",
			"  VRFCPLFFPLR,             !- Name",
			"  0.85,                    !- Coefficient1 Constant",
			"  0.15,                    !- Coefficient2 x",
			"  0.0,                     !- Coefficient3 x**2",
			"  0.0,                     !- Minimum Value of x",
			"  1.0,                     !- Maximum Value of x",
			"  0.85,                    !- Minimum Curve Output",
			"  1.0,                     !- Maximum Curve Output",
			"  Dimensionless,           !- Input Unit Type for X",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Biquadratic,",
			"  VRFHeatCapFT,            !- Name",
			"  1.014599599,             !- Coefficient1 Constant",
			"  -0.002506703,            !- Coefficient2 x",
			"  -0.000141599,            !- Coefficient3 x**2",
			"  0.026931595,             !- Coefficient4 y",
			"  1.83538E-06,             !- Coefficient5 y**2",
			"  -0.000358147,            !- Coefficient6 x*y",
			"  15,                      !- Minimum Value of x",
			"  27,                      !- Maximum Value of x",
			"  -20,                     !- Minimum Value of y",
			"  15,                      !- Maximum Value of y",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature,             !- Input Unit Type for Y",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Cubic,",
			"  VRFHeatCapFTBoundary,    !- Name",
			"  -7.6000882,              !- Coefficient1 Constant",
			"  3.05090016,              !- Coefficient2 x",
			"  -0.1162844,              !- Coefficient3 x**2",
			"  0.0,                     !- Coefficient4 x**3",
			"  15,                      !- Minimum Value of x",
			"  27,                      !- Maximum Value of x",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature;             !- Output Unit Type",
			" ",
			"Curve:Biquadratic,",
			"  VRFHeatCapFTHi,          !- Name",
			"  1.161134821,             !- Coefficient1 Constant",
			"  0.027478868,             !- Coefficient2 x",
			"  -0.00168795,             !- Coefficient3 x**2",
			"  0.001783378,             !- Coefficient4 y",
			"  2.03208E-06,             !- Coefficient5 y**2",
			"  -6.8969E-05,             !- Coefficient6 x*y",
			"  15,                      !- Minimum Value of x",
			"  27,                      !- Maximum Value of x",
			"  -10,                     !- Minimum Value of y",
			"  15,                      !- Maximum Value of y",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature,             !- Input Unit Type for Y",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Biquadratic,",
			"  VRFHeatEIRFT,            !- Name",
			"  0.87465501,              !- Coefficient1 Constant",
			"  -0.01319754,             !- Coefficient2 x",
			"  0.00110307,              !- Coefficient3 x**2",
			"  -0.0133118,              !- Coefficient4 y",
			"  0.00089017,              !- Coefficient5 y**2",
			"  -0.00012766,             !- Coefficient6 x*y",
			"  15,                      !- Minimum Value of x",
			"  27,                      !- Maximum Value of x",
			"  -20,                     !- Minimum Value of y",
			"  12,                      !- Maximum Value of y",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature,             !- Input Unit Type for Y",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Cubic,",
			"  VRFHeatEIRFTBoundary,    !- Name",
			"  -7.6000882,              !- Coefficient1 Constant",
			"  3.05090016,              !- Coefficient2 x",
			"  -0.1162844,              !- Coefficient3 x**2",
			"  0.0,                     !- Coefficient4 x**3",
			"  15,                      !- Minimum Value of x",
			"  27,                      !- Maximum Value of x",
			"  -20,                     !- Minimum Curve Output",
			"  15,                      !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature;             !- Output Unit Type",
			" ",
			"Curve:Biquadratic,",
			"  VRFHeatEIRFTHi,          !- Name",
			"  2.504005146,             !- Coefficient1 Constant",
			"  -0.05736767,             !- Coefficient2 x",
			"  4.07336E-05,             !- Coefficient3 x**2",
			"  -0.12959669,             !- Coefficient4 y",
			"  0.00135839,              !- Coefficient5 y**2",
			"  0.00317047,              !- Coefficient6 x*y",
			"  15,                      !- Minimum Value of x",
			"  27,                      !- Maximum Value of x",
			"  -10,                     !- Minimum Value of y",
			"  15,                      !- Maximum Value of y",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature,             !- Input Unit Type for Y",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Cubic,",
			"  HeatingEIRLowPLR,        !- Name",
			"  0.1400093,               !- Coefficient1 Constant",
			"  0.6415002,               !- Coefficient2 x",
			"  0.1339047,               !- Coefficient3 x**2",
			"  0.0845859,               !- Coefficient4 x**3",
			"  0,                       !- Minimum Value of x",
			"  1,                       !- Maximum Value of x",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Dimensionless,           !- Input Unit Type for X",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Quadratic,",
			"  HeatingEIRHiPLR,         !- Name",
			"  2.4294355,               !- Coefficient1 Constant",
			"  -2.235887,               !- Coefficient2 x",
			"  0.8064516,               !- Coefficient3 x**2",
			"  1.0,                     !- Minimum Value of x",
			"  1.5,                     !- Maximum Value of x",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Dimensionless,           !- Input Unit Type for X",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Linear,",
			"  HeatingCombRatio,        !- Name",
			"  0.96034,                 !- Coefficient1 Constant",
			"  0.03966,                 !- Coefficient2 x",
			"  1.0,                     !- Minimum Value of x",
			"  1.5,                     !- Maximum Value of x",
			"  1.0,                     !- Minimum Curve Output",
			"  1.023,                   !- Maximum Curve Output",
			"  Dimensionless,           !- Input Unit Type for X",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Biquadratic,",
			"  CoolingLengthCorrectionFactor,  !- Name",
			"  1.0693794,               !- Coefficient1 Constant",
			"  -0.0014951,              !- Coefficient2 x",
			"  2.56E-06,                !- Coefficient3 x**2",
			"  -0.1151104,              !- Coefficient4 y",
			"  0.0511169,               !- Coefficient5 y**2",
			"  -0.0004369,              !- Coefficient6 x*y",
			"  8,                       !- Minimum Value of x",
			"  175,                     !- Maximum Value of x",
			"  0.5,                     !- Minimum Value of y",
			"  1.5,                     !- Maximum Value of y",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature,             !- Input Unit Type for Y",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Cubic,",
			"  VRFTUCoolCapFT,          !- Name",
			"  0.504547273506488,       !- Coefficient1 Constant",
			"  0.0288891279198444,      !- Coefficient2 x",
			"  -0.000010819418650677,   !- Coefficient3 x**2",
			"  0.0000101359395177008,   !- Coefficient4 x**3",
			"  0.0,                     !- Minimum Value of x",
			"  50.0,                    !- Maximum Value of x",
			"  0.5,                     !- Minimum Curve Output",
			"  1.5,                     !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Quadratic,",
			"  VRFACCoolCapFFF,         !- Name",
			"  0.8,                     !- Coefficient1 Constant",
			"  0.2,                     !- Coefficient2 x",
			"  0.0,                     !- Coefficient3 x**2",
			"  0.5,                     !- Minimum Value of x",
			"  1.5;                     !- Maximum Value of x",
			" ",
			"Curve:Cubic,",
			"  VRFTUHeatCapFT,          !- Name",
			"  -0.390708928227928,      !- Coefficient1 Constant",
			"  0.261815023760162,       !- Coefficient2 x",
			"  -0.0130431603151873,     !- Coefficient3 x**2",
			"  0.000178131745997821,    !- Coefficient4 x**3",
			"  0.0,                     !- Minimum Value of x",
			"  50.0,                    !- Maximum Value of x",
			"  0.5,                     !- Minimum Curve Output",
			"  1.5,                     !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Dimensionless;           !- Output Unit Type",
		} );

		ASSERT_FALSE( process_idf( idf_objects ) );

		DataGlobals::BeginEnvrnFlag = true;
		DataSizing::CurZoneEqNum = 1;
		DataEnvironment::OutBaroPress = 101325; // sea level
		DataZoneEquipment::ZoneEquipInputsFilled = true; // denotes zone equipment has been read in
		StdRhoAir = PsyRhoAirFnPbTdbW( DataEnvironment::OutBaroPress, 20.0, 0.0 );
		ZoneEqSizing.allocate( 1 );
		ZoneSizingRunDone = true;
		ZoneEqSizing( CurZoneEqNum ).DesignSizeFromParent = false;
		ZoneEqSizing( CurZoneEqNum ).SizingMethod.allocate( 25 );
		ZoneEqSizing( CurZoneEqNum ).SizingMethod( DataHVACGlobals::SystemAirflowSizing ) = DataSizing::SupplyAirFlowRate;

		FinalZoneSizing.allocate( 1 );
		FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow = 0.566337; // 400 cfm * 3 tons = 1200 cfm
		FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow = 0.566337;

		ZoneSysEnergyDemand.allocate( 1 );

		ProcessScheduleInput(); // read schedules
		GetCurveInput(); // read curves
		GetZoneData( ErrorsFound ); // read zone data
		EXPECT_FALSE( ErrorsFound );

		GetZoneEquipmentData(); // read equipment list and connections
		GetVRFInputData( ErrorsFound );
		EXPECT_TRUE( ErrorsFound );
		EXPECT_EQ( 0, VRFTU( VRFTUNum ).VRFSysNum );
		EXPECT_EQ( 0, VRFTU( VRFTUNum ).ZoneNum );
		EXPECT_EQ( 0, VRFTU( VRFTUNum ).TUListIndex );
		EXPECT_EQ( 0, VRFTU( VRFTUNum ).IndexToTUInTUList );

		// clean up
		ZoneSysEnergyDemand.deallocate();

	}

	TEST_F( EnergyPlusFixture, VRFTest_SysCurve_WaterCooled ) {

		static std::string const RoutineName( "VRFTest_WaterCooled" );
		bool ErrorsFound( false );        // function returns true on error
		bool FirstHVACIteration( true );  // simulate the first pass through HVAC simulation, use false for next iteration
		int VRFCond( 1 );                 // index to VRF condenser
		int VRFTUNum( 1 );                // index to VRF terminal unit
		int EquipPtr( 1 );                // index to equipment list
		int CurZoneNum( 1 );              // index to zone
		int ZoneInletAirNode( 0 );        // zone inlet node number
		Real64 SysOutputProvided( 0.0 );  // function returns sensible capacity [W]
		Real64 LatOutputProvided( 0.0 );  // function returns latent capacity [W]
		Real64 CurLoad( 0.0 );
		Real64 MaxLoad( 0.0 );
		Real64 MinLoad( 0.0 );
		Real64 OptLoad( 0.0 );
		int LoopNum( 0 );
		Real64 rho( 0.0 );
		Real64 Cp( 0.0 );
		Real64 CondVolFlowRate( 0.0 );


		std::string const idf_objects = delimited_string( {
			"Version,8.3;",
			" BUILDING, VRFTest_SysCurve_WaterCooled, 0.0, Suburbs, .04, .4, FullExterior, 25, 6;",
			" ",
			"AirConditioner:VariableRefrigerantFlow,",
			"  VRF Water Cooled HP,     !- Heat Pump Name",
			"  VRFCondAvailSched,       !- Availability Schedule Name",
			"  autosize,                !- Gross Rated Total Cooling Capacity {W}",
			"  3.2917,                  !- Gross Rated Cooling COP {W/W}",
			"  -5,                      !- Minimum Outdoor Temperature in Cooling Mode {C}",
			"  43,                      !- Maximum Outdoor Temperature in Cooling Mode {C}",
			"  VRFCoolCapFT,            !- Cooling Capacity Ratio Modifier Function of Low Temperature Curve Name",
			"  VRFCoolCapFTBoundary,    !- Cooling Capacity Ratio Boundary Curve Name",
			"  VRFCoolCapFTHi,          !- Cooling Capacity Ratio Modifier Function of High Temperature Curve Name",
			"  VRFCoolEIRFT,            !- Cooling Energy Input Ratio Modifier Function of Low Temperature Curve Name",
			"  VRFCoolEIRFTBoundary,    !- Cooling Energy Input Ratio Boundary Curve Name",
			"  VRFCoolEIRFTHi,          !- Cooling Energy Input Ratio Modifier Function of High Temperature Curve Name",
			"  CoolingEIRLowPLR,        !- Cooling Energy Input Ratio Modifier Function of Low Part-Load Ratio Curve Name",
			"  CoolingEIRHiPLR,         !- Cooling Energy Input Ratio Modifier Function of High Part-Load Ratio Curve Name",
			"  CoolingCombRatio,        !- Cooling Combination Ratio Correction Factor Curve Name",
			"  VRFCPLFFPLR,             !- Cooling Part-Load Fraction Correlation Curve Name",
			"  autosize,                !- Gross Rated Heating Capacity {W}",
			"  ,                        !- Rated Heating Capacity Sizing Ratio {W/W}",
			"  3.5484,                  !- Gross Rated Heating COP {W/W}",
			"  -20,                     !- Minimum Outdoor Temperature in Heating Mode {C}",
			"  20,                      !- Maximum Outdoor Temperature in Heating Mode {C}",
			"  VRFHeatCapFT,            !- Heating Capacity Ratio Modifier Function of Low Temperature Curve Name",
			"  VRFHeatCapFTBoundary,    !- Heating Capacity Ratio Boundary Curve Name",
			"  VRFHeatCapFTHi,          !- Heating Capacity Ratio Modifier Function of High Temperature Curve Name",
			"  VRFHeatEIRFT,            !- Heating Energy Input Ratio Modifier Function of Low Temperature Curve Name",
			"  VRFHeatEIRFTBoundary,    !- Heating Energy Input Ratio Boundary Curve Name",
			"  VRFHeatEIRFTHi,          !- Heating Energy Input Ratio Modifier Function of High Temperature Curve Name",
			"  WetBulbTemperature,      !- Heating Performance Curve Outdoor Temperature Type",
			"  HeatingEIRLowPLR,        !- Heating Energy Input Ratio Modifier Function of Low Part-Load Ratio Curve Name",
			"  HeatingEIRHiPLR,         !- Heating Energy Input Ratio Modifier Function of High Part-Load Ratio Curve Name",
			"  HeatingCombRatio,        !- Heating Combination Ratio Correction Factor Curve Name",
			"  VRFCPLFFPLR,             !- Heating Part-Load Fraction Correlation Curve Name",
			"  0.25,                    !- Minimum Heat Pump Part-Load Ratio {dimensionless}",
			"  SPACE1-1,                !- Zone Name for Master Thermostat Location",
			"  LoadPriority,            !- Master Thermostat Priority Control Type",
			"  ,                        !- Thermostat Priority Schedule Name",
			"  VRF Heat Pump TU List,   !- Zone Terminal Unit List Name",
			"  No,                      !- Heat Pump Waste Heat Recovery",
			"  30,                      !- Equivalent Piping Length used for Piping Correction Factor in Cooling Mode {m}",
			"  10,                      !- Vertical Height used for Piping Correction Factor {m}",
			"  CoolingLengthCorrectionFactor,  !- Piping Correction Factor for Length in Cooling Mode Curve Name",
			"  -0.000386,               !- Piping Correction Factor for Height in Cooling Mode Coefficient {1/m}",
			"  30,                      !- Equivalent Piping Length used for Piping Correction Factor in Heating Mode {m}",
			"  ,                        !- Piping Correction Factor for Length in Heating Mode Curve Name",
			"  ,                        !- Piping Correction Factor for Height in Heating Mode Coefficient {1/m}",
			"  15,                      !- Crankcase Heater Power per Compressor {W}",
			"  3,                       !- Number of Compressors {dimensionless}",
			"  0.33,                    !- Ratio of Compressor Size to Total Compressor Capacity {W/W}",
			"  7,                       !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater {C}",
			"  ReverseCycle,            !- Defrost Strategy",
			"  Timed,                   !- Defrost Control",
			"  DefrostEIRSched,         !- Defrost Energy Input Ratio Modifier Function of Temperature Curve Name",
			"  ,                        !- Defrost Time Period Fraction {dimensionless}",
			"  autosize,                !- Resistive Defrost Heater Capacity {W}",
			"  7,                       !- Maximum Outdoor Dry-bulb Temperature for Defrost Operation {C}",
			"  WaterCooled,             !- Condenser Type",
			"  VRF Water Inlet Node,    !- Condenser Inlet Node Name",
			"  VRF Water Outlet Node,   !- Condenser Outlet Node Name",
			"  autosize,                !- Water Condenser Volume Flow Rate {m3/s}",
			"  ,                        !- Evaporative Condenser Effectiveness {dimensionless}",
			"  ,                        !- Evaporative Condenser Air Flow Rate {m3/s}",
			"  0,                       !- Evaporative Condenser Pump Rated Power Consumption {W}",
			"  ,                        !- Supply Water Storage Tank Name",
			"  0,                       !- Basin Heater Capacity {W/K}",
			"  ,                        !- Basin Heater Setpoint Temperature {C}",
			"  ,                        !- Basin Heater Operating Schedule Name",
			"  Electricity;             !- Fuel Type",
			" ",
			"Zone,",
			"  SPACE1-1,                !- Name",
			"  0,                       !- Direction of Relative North {deg}",
			"  0,                       !- X Origin {m}",
			"  0,                       !- Y Origin {m}",
			"  0,                       !- Z Origin {m}",
			"  1,                       !- Type",
			"  1,                       !- Multiplier",
			"  2.438400269,             !- Ceiling Height {m}",
			"  239.247360229;           !- Volume {m3}",
			" ",
			"ZoneHVAC:EquipmentConnections,",
			"  SPACE1-1,                !- Zone Name",
			"  SPACE1-1 Eq,             !- Zone Conditioning Equipment List Name",
			"  TU1 Outlet Node,         !- Zone Air Inlet Node or NodeList Name",
			"  TU1 Inlet Node,          !- Zone Air Exhaust Node or NodeList Name",
			"  SPACE1-1 Node,           !- Zone Air Node Name",
			"  SPACE1-1 Out Node;       !- Zone Return Air Node Name", // not used anywhere else in the example file
			" ",
			"ZoneHVAC:EquipmentList,",
			"  SPACE1-1 Eq,             !- Name",
			"  ZoneHVAC:TerminalUnit:VariableRefrigerantFlow,  !- Zone Equipment 1 Object Type",
			"  TU1,                     !- Zone Equipment 1 Name",
			"  1,                       !- Zone Equipment 1 Cooling Sequence",
			"  1;                       !- Zone Equipment 1 Heating or No-Load Sequence",
			" ",
			"ZoneTerminalUnitList,",
			"  VRF Heat Pump TU List,    !- Zone Terminal Unit List Name",
			"  TU1;                      !- Zone Terminal Unit Name 1",
			" ",
			"ZoneHVAC:TerminalUnit:VariableRefrigerantFlow,",
			"  TU1,                      !- Zone Terminal Unit Name",
			"  VRFAvailSched,            !- Terminal Unit Availability Schedule",
			"  TU1 Inlet Node,           !- Terminal Unit Air Inlet Node Name",
			"  TU1 Outlet Node,          !- Terminal Unit Air Outlet Node Name",
			"  autosize,                 !- Supply Air Flow Rate During Cooling Operation {m3/s}",
			"  autosize,                 !- Supply Air Flow Rate When No Cooling is Needed {m3/s}",
			"  autosize,                 !- Supply Air Flow Rate During Heating Operation {m3/s}",
			"  autosize,                 !- Supply Air Flow Rate When No Heating is Needed {m3/s}",
			"  autosize,                 !- Outdoor Air Flow Rate During Cooling Operation {m3/s}",
			"  autosize,                 !- Outdoor Air Flow Rate During Heating Operation {m3/s}",
			"  autosize,                 !- Outdoor Air Flow Rate When No Cooling or Heating is Needed {m3/s}",
			"  VRFWaterFanSchedule,      !- Supply Air Fan Operating Mode Schedule Name",
			"  drawthrough,              !- Supply Air Fan Placement",
			"  Fan:OnOff,                !- Supply Air Fan Object Type",
			"  TU1 VRF Supply Fan,       !- Supply Air Fan Object Name",
			"  OutdoorAir:Mixer,         !- Outside Air Mixer Object Type",
			"  TU1 OA Mixer,             !- Outside Air Mixer Object Name",
			"  COIL:Cooling:DX:VariableRefrigerantFlow,  !- Cooling Coil Object Type",
			"  TU1 VRF DX Cooling Coil,  !- Cooling Coil Object Name",
			"  COIL:Heating:DX:VariableRefrigerantFlow,  !- Heating Coil Object Type",
			"  TU1 VRF DX Heating Coil,  !- Heating Coil Object Name",
			"  30,                       !- Zone Terminal Unit On Parasitic Electric Energy Use {W}",
			"  20;                       !- Zone Terminal Unit Off Parasitic Electric Energy Use{ W }",
			" ",
			"Fan:OnOff,",
			"  TU1 VRF Supply Fan,       !- Name",
			"  VRFAvailSched,            !- Availability Schedule Name",
			"  0.7,                      !- Fan Total Efficiency",
			"  600.0,                    !- Pressure Rise{ Pa }",
			"  autosize,                 !- Maximum Flow Rate{ m3 / s }",
			"  0.9,                      !- Motor Efficiency",
			"  1.0,                      !- Motor In Airstream Fraction",
			"  TU1 VRF DX HCoil Outlet Node, !- Air Inlet Node Name",
			"  TU1 Outlet Node;          !- Air Outlet Node Name",
			" ",
			"OutdoorAir:Mixer,",
			"  TU1 OA Mixer,             !- Name",
			"  TU1 VRF DX CCoil Inlet Node, !- Mixed Air Node Name",
			"  Outside Air Inlet Node 1, !- Outdoor Air Stream Node Name",
			"  Relief Air Outlet Node 1, !- Relief Air Stream Node Name",
			"  TU1 Inlet Node;           !- Return Air Stream Node Name",
			" ",
			"OutdoorAir:NodeList,",
			"  OutsideAirInletNodes;     !- Node or NodeList Name 1",
			" ",
			"NodeList,",
			"  OutsideAirInletNodes, !- Name",
			"  Outside Air Inlet Node 1, !- Node 1 Name",
			"  MyVRFOANode;  !- Node 1 Name",
			" ",
			"COIL:Cooling:DX:VariableRefrigerantFlow,",
			"  TU1 VRF DX Cooling Coil, !- Name",
			"  VRFAvailSched,           !- Availability Schedule Name",
			"  autosize,                !- Gross Rated Total Cooling Capacity {W}",
			"  autosize,                !- Gross Rated Sensible Heat Ratio",
			"  autosize,                !- Rated Air Flow Rate {m3/s}",
			"  VRFTUCoolCapFT,          !- Cooling Capacity Ratio Modifier Function of Temperature Curve Name",
			"  VRFACCoolCapFFF,         !- Cooling Capacity Modifier Curve Function of Flow Fraction Name",
			"  TU1 VRF DX CCoil Inlet Node,  !- Coil Air Inlet Node",
			"  TU1 VRF DX CCoil Outlet Node,  !- Coil Air Outlet Node",
			"  ;                        !- Name of Water Storage Tank for Condensate Collection",
			" ",
			"COIL:Heating:DX:VariableRefrigerantFlow,",
			"  TU1 VRF DX Heating Coil, !- Name",
			"  VRFAvailSched,           !- Availability Schedule",
			"  autosize,                !- Gross Rated Heating Capacity {W}",
			"  autosize,                !- Rated Air Flow Rate {m3/s}",
			"  TU1 VRF DX CCoil Outlet Node,  !- Coil Air Inlet Node",
			"  TU1 VRF DX HCoil Outlet Node,  !- Coil Air Outlet Node",
			"  VRFTUHeatCapFT,          !- Heating Capacity Ratio Modifier Function of Temperature Curve Name",
			"  VRFACCoolCapFFF;         !- Heating Capacity Modifier Function of Flow Fraction Curve Name",
			" ",
			"ScheduleTypeLimits,",
			"  Any Number;              !- Name",
			" ",
			"Schedule:Compact,",
			"  VRFAvailSched,           !- Name",
			"  Any Number,              !- Schedule Type Limits Name",
			"  Through: 3/31,           !- Field 1",
			"  For: AllDays,            !- Field 2",
			"  Until: 24:00,1.0,        !- Field 3",
			"  Through: 9/30,           !- Field 4",
			"  For: WeekDays,           !- Field 5",
			"  Until: 7:00,1.0,         !- Field 6",
			"  Until: 17:00,1.0,        !- Field 7",
			"  Until: 24:00,1.0,        !- Field 8",
			"  For: SummerDesignDay WinterDesignDay, !- Field 9",
			"  Until: 24:00,1.0,        !- Field 10",
			"  For: AllOtherDays,       !- Field 11",
			"  Until: 24:00,1.0,        !- Field 12",
			"  Through: 12/31,          !- Field 13",
			"  For: AllDays,            !- Field 14",
			"  Until: 24:00,1.0;        !- Field 15",
			" ",
			"Schedule:Compact,",
			"  VRFCondAvailSched,       !- Name",
			"  Any Number,              !- Schedule Type Limits Name",
			"  Through: 12/31,          !- Field 1",
			"  For: AllDays,            !- Field 2",
			"  Until: 12:00,1.0,        !- Field 3",
			"  Until: 13:00,1.0,        !- Field 4",
			"  Until: 24:00,1.0;        !- Field 5",
			" ",
			"Schedule:Compact,",
			"  VRFWaterFanSchedule,     !- Name",
			"  Any Number,              !- Schedule Type Limits Name",
			"  Through: 12/31,          !- Field 1",
			"  For: AllDays,            !- Field 2",
			"  Until: 24:00, 1.0;       !- Field 7",
			" ",
			"Curve:Biquadratic,",
			"  DefrostEIRSched,         !- Name",
			"  1.0,                     !- Coefficient1 Constant",
			"  0.0,                     !- Coefficient2 x",
			"  0.0,                     !- Coefficient3 x**2",
			"  0.0,                     !- Coefficient4 y",
			"  0.0,                     !- Coefficient5 y**2",
			"  0.0,                     !- Coefficient6 x*y",
			"  15,                      !- Minimum Value of x",
			"  24,                      !- Maximum Value of x",
			"  -5,                      !- Minimum Value of y",
			"  23,                      !- Maximum Value of y",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature,             !- Input Unit Type for Y",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Biquadratic,",
			"  VRFCoolCapFT,            !- Name",
			"  0.576882692,             !- Coefficient1 Constant",
			"  0.017447952,             !- Coefficient2 x",
			"  0.000583269,             !- Coefficient3 x**2",
			"  -1.76324E-06,            !- Coefficient4 y",
			"  -7.474E-09,              !- Coefficient5 y**2",
			"  -1.30413E-07,            !- Coefficient6 x*y",
			"  15,                      !- Minimum Value of x",
			"  24,                      !- Maximum Value of x",
			"  -5,                      !- Minimum Value of y",
			"  23,                      !- Maximum Value of y",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature,             !- Input Unit Type for Y",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Cubic,",
			"  VRFCoolCapFTBoundary,    !- Name",
			"  25.73473775,             !- Coefficient1 Constant",
			"  -0.03150043,             !- Coefficient2 x",
			"  -0.01416595,             !- Coefficient3 x**2",
			"  0,                       !- Coefficient4 x**3",
			"  11,                      !- Minimum Value of x",
			"  30,                      !- Maximum Value of x",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature;             !- Output Unit Type",
			" ",
			"Curve:Biquadratic,",
			"  VRFCoolCapFTHi,          !- Name",
			"  0.6867358,               !- Coefficient1 Constant",
			"  0.0207631,               !- Coefficient2 x",
			"  0.0005447,               !- Coefficient3 x**2",
			"  -0.0016218,              !- Coefficient4 y",
			"  -4.259E-07,              !- Coefficient5 y**2",
			"  -0.0003392,              !- Coefficient6 x*y",
			"  15,                      !- Minimum Value of x",
			"  24,                      !- Maximum Value of x",
			"  16,                      !- Minimum Value of y",
			"  43,                      !- Maximum Value of y",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature,             !- Input Unit Type for Y",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Biquadratic,",
			"  VRFCoolEIRFT,            !- Name",
			"  0.989010541,             !- Coefficient1 Constant",
			"  -0.02347967,             !- Coefficient2 x",
			"  0.000199711,             !- Coefficient3 x**2",
			"  0.005968336,             !- Coefficient4 y",
			"  -1.0289E-07,             !- Coefficient5 y**2",
			"  -0.00015686,             !- Coefficient6 x*y",
			"  15,                      !- Minimum Value of x",
			"  24,                      !- Maximum Value of x",
			"  -5,                      !- Minimum Value of y",
			"  23,                      !- Maximum Value of y",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature,             !- Input Unit Type for Y",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Cubic,",
			"  VRFCoolEIRFTBoundary,    !- Name",
			"  25.73473775,             !- Coefficient1 Constant",
			"  -0.03150043,             !- Coefficient2 x",
			"  -0.01416595,             !- Coefficient3 x**2",
			"  0,                       !- Coefficient4 x**3",
			"  15,                      !- Minimum Value of x",
			"  24,                      !- Maximum Value of x",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature;             !- Output Unit Type",
			" ",
			"Curve:Biquadratic,",
			"  VRFCoolEIRFTHi,          !- Name",
			"  0.14351470,              !- Coefficient1 Constant",
			"  0.01860035,              !- Coefficient2 x",
			"  -0.0003954,              !- Coefficient3 x**2",
			"  0.02485219,              !- Coefficient4 y",
			"  0.00016329,              !- Coefficient5 y**2",
			"  -0.0006244,              !- Coefficient6 x*y",
			"  15,                      !- Minimum Value of x",
			"  24,                      !- Maximum Value of x",
			"  16,                      !- Minimum Value of y",
			"  43,                      !- Maximum Value of y",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature,             !- Input Unit Type for Y",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Cubic,",
			"  CoolingEIRLowPLR,        !- Name",
			"  0.4628123,               !- Coefficient1 Constant",
			"  -1.0402406,              !- Coefficient2 x",
			"  2.17490997,              !- Coefficient3 x**2",
			"  -0.5974817,              !- Coefficient4 x**3",
			"  0,                       !- Minimum Value of x",
			"  1,                       !- Maximum Value of x",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature;             !- Output Unit Type",
			" ",
			"Curve:Quadratic,",
			"  CoolingEIRHiPLR,         !- Name",
			"  1.0,                     !- Coefficient1 Constant",
			"  0.0,                     !- Coefficient2 x",
			"  0.0,                     !- Coefficient3 x**2",
			"  1.0,                     !- Minimum Value of x",
			"  1.5,                     !- Maximum Value of x",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Dimensionless,           !- Input Unit Type for X",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Linear,",
			"  CoolingCombRatio,        !- Name",
			"  0.618055,                !- Coefficient1 Constant",
			"  0.381945,                !- Coefficient2 x",
			"  1.0,                     !- Minimum Value of x",
			"  1.5,                     !- Maximum Value of x",
			"  1.0,                     !- Minimum Curve Output",
			"  1.2,                     !- Maximum Curve Output",
			"  Dimensionless,           !- Input Unit Type for X",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"CURVE:QUADRATIC,",
			"  VRFCPLFFPLR,             !- Name",
			"  0.85,                    !- Coefficient1 Constant",
			"  0.15,                    !- Coefficient2 x",
			"  0.0,                     !- Coefficient3 x**2",
			"  0.0,                     !- Minimum Value of x",
			"  1.0,                     !- Maximum Value of x",
			"  0.85,                    !- Minimum Curve Output",
			"  1.0,                     !- Maximum Curve Output",
			"  Dimensionless,           !- Input Unit Type for X",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Biquadratic,",
			"  VRFHeatCapFT,            !- Name",
			"  1.014599599,             !- Coefficient1 Constant",
			"  -0.002506703,            !- Coefficient2 x",
			"  -0.000141599,            !- Coefficient3 x**2",
			"  0.026931595,             !- Coefficient4 y",
			"  1.83538E-06,             !- Coefficient5 y**2",
			"  -0.000358147,            !- Coefficient6 x*y",
			"  15,                      !- Minimum Value of x",
			"  27,                      !- Maximum Value of x",
			"  -20,                     !- Minimum Value of y",
			"  15,                      !- Maximum Value of y",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature,             !- Input Unit Type for Y",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Cubic,",
			"  VRFHeatCapFTBoundary,    !- Name",
			"  -7.6000882,              !- Coefficient1 Constant",
			"  3.05090016,              !- Coefficient2 x",
			"  -0.1162844,              !- Coefficient3 x**2",
			"  0.0,                     !- Coefficient4 x**3",
			"  15,                      !- Minimum Value of x",
			"  27,                      !- Maximum Value of x",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature;             !- Output Unit Type",
			" ",
			"Curve:Biquadratic,",
			"  VRFHeatCapFTHi,          !- Name",
			"  1.161134821,             !- Coefficient1 Constant",
			"  0.027478868,             !- Coefficient2 x",
			"  -0.00168795,             !- Coefficient3 x**2",
			"  0.001783378,             !- Coefficient4 y",
			"  2.03208E-06,             !- Coefficient5 y**2",
			"  -6.8969E-05,             !- Coefficient6 x*y",
			"  15,                      !- Minimum Value of x",
			"  27,                      !- Maximum Value of x",
			"  -10,                     !- Minimum Value of y",
			"  15,                      !- Maximum Value of y",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature,             !- Input Unit Type for Y",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Biquadratic,",
			"  VRFHeatEIRFT,            !- Name",
			"  0.87465501,              !- Coefficient1 Constant",
			"  -0.01319754,             !- Coefficient2 x",
			"  0.00110307,              !- Coefficient3 x**2",
			"  -0.0133118,              !- Coefficient4 y",
			"  0.00089017,              !- Coefficient5 y**2",
			"  -0.00012766,             !- Coefficient6 x*y",
			"  15,                      !- Minimum Value of x",
			"  27,                      !- Maximum Value of x",
			"  -20,                     !- Minimum Value of y",
			"  12,                      !- Maximum Value of y",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature,             !- Input Unit Type for Y",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Cubic,",
			"  VRFHeatEIRFTBoundary,    !- Name",
			"  -7.6000882,              !- Coefficient1 Constant",
			"  3.05090016,              !- Coefficient2 x",
			"  -0.1162844,              !- Coefficient3 x**2",
			"  0.0,                     !- Coefficient4 x**3",
			"  15,                      !- Minimum Value of x",
			"  27,                      !- Maximum Value of x",
			"  -20,                     !- Minimum Curve Output",
			"  15,                      !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature;             !- Output Unit Type",
			" ",
			"Curve:Biquadratic,",
			"  VRFHeatEIRFTHi,          !- Name",
			"  2.504005146,             !- Coefficient1 Constant",
			"  -0.05736767,             !- Coefficient2 x",
			"  4.07336E-05,             !- Coefficient3 x**2",
			"  -0.12959669,             !- Coefficient4 y",
			"  0.00135839,              !- Coefficient5 y**2",
			"  0.00317047,              !- Coefficient6 x*y",
			"  15,                      !- Minimum Value of x",
			"  27,                      !- Maximum Value of x",
			"  -10,                     !- Minimum Value of y",
			"  15,                      !- Maximum Value of y",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature,             !- Input Unit Type for Y",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Cubic,",
			"  HeatingEIRLowPLR,        !- Name",
			"  0.1400093,               !- Coefficient1 Constant",
			"  0.6415002,               !- Coefficient2 x",
			"  0.1339047,               !- Coefficient3 x**2",
			"  0.0845859,               !- Coefficient4 x**3",
			"  0,                       !- Minimum Value of x",
			"  1,                       !- Maximum Value of x",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Dimensionless,           !- Input Unit Type for X",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Quadratic,",
			"  HeatingEIRHiPLR,         !- Name",
			"  2.4294355,               !- Coefficient1 Constant",
			"  -2.235887,               !- Coefficient2 x",
			"  0.8064516,               !- Coefficient3 x**2",
			"  1.0,                     !- Minimum Value of x",
			"  1.5,                     !- Maximum Value of x",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Dimensionless,           !- Input Unit Type for X",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Linear,",
			"  HeatingCombRatio,        !- Name",
			"  0.96034,                 !- Coefficient1 Constant",
			"  0.03966,                 !- Coefficient2 x",
			"  1.0,                     !- Minimum Value of x",
			"  1.5,                     !- Maximum Value of x",
			"  1.0,                     !- Minimum Curve Output",
			"  1.023,                   !- Maximum Curve Output",
			"  Dimensionless,           !- Input Unit Type for X",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Biquadratic,",
			"  CoolingLengthCorrectionFactor,  !- Name",
			"  1.0693794,               !- Coefficient1 Constant",
			"  -0.0014951,              !- Coefficient2 x",
			"  2.56E-06,                !- Coefficient3 x**2",
			"  -0.1151104,              !- Coefficient4 y",
			"  0.0511169,               !- Coefficient5 y**2",
			"  -0.0004369,              !- Coefficient6 x*y",
			"  8,                       !- Minimum Value of x",
			"  175,                     !- Maximum Value of x",
			"  0.5,                     !- Minimum Value of y",
			"  1.5,                     !- Maximum Value of y",
			"  ,                        !- Minimum Curve Output",
			"  ,                        !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Temperature,             !- Input Unit Type for Y",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Cubic,",
			"  VRFTUCoolCapFT,          !- Name",
			"  0.504547273506488,       !- Coefficient1 Constant",
			"  0.0288891279198444,      !- Coefficient2 x",
			"  -0.000010819418650677,   !- Coefficient3 x**2",
			"  0.0000101359395177008,   !- Coefficient4 x**3",
			"  0.0,                     !- Minimum Value of x",
			"  50.0,                    !- Maximum Value of x",
			"  0.5,                     !- Minimum Curve Output",
			"  1.5,                     !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"Curve:Quadratic,",
			"  VRFACCoolCapFFF,         !- Name",
			"  0.8,                     !- Coefficient1 Constant",
			"  0.2,                     !- Coefficient2 x",
			"  0.0,                     !- Coefficient3 x**2",
			"  0.5,                     !- Minimum Value of x",
			"  1.5;                     !- Maximum Value of x",
			" ",
			"Curve:Cubic,",
			"  VRFTUHeatCapFT,          !- Name",
			"  -0.390708928227928,      !- Coefficient1 Constant",
			"  0.261815023760162,       !- Coefficient2 x",
			"  -0.0130431603151873,     !- Coefficient3 x**2",
			"  0.000178131745997821,    !- Coefficient4 x**3",
			"  0.0,                     !- Minimum Value of x",
			"  50.0,                    !- Maximum Value of x",
			"  0.5,                     !- Minimum Curve Output",
			"  1.5,                     !- Maximum Curve Output",
			"  Temperature,             !- Input Unit Type for X",
			"  Dimensionless;           !- Output Unit Type",
			" ",
			"PlantLoop,",
			"  Main Loop, !- Name",
			"  WATER, !- Fluid Type",
			", !- User Defined Fluid Type",
			"Main Loop Operation, !- Plant Equipment Operation Scheme Name",
			"Supply Outlet Node, !- Loop Temperature Setpoint Node Name",
			"100, !- Maximum Loop Temperature{ C }",
			"3, !- Minimum Loop Temperature{ C }",
			"0.003, !- Maximum Loop Flow Rate{ m3 / s }",
			"0, !- Minimum Loop Flow Rate{ m3 / s }",
			"autocalculate, !- Plant Loop Volume{ m3 }",
			"Supply Inlet Node, !- Plant Side Inlet Node Name",
			"Supply Outlet Node, !- Plant Side Outlet Node Name",
			"Supply Branches, !- Plant Side Branch List Name",
			"Supply Connectors, !- Plant Side Connector List Name",
			"Demand Inlet Node, !- Demand Side Inlet Node Name",
			"Demand Outlet Node, !- Demand Side Outlet Node Name",
			"Demand Branches, !- Demand Side Branch List Name",
			"Demand Connectors, !- Demand Side Connector List Name",
			"OPTIMAL;                 !- Load Distribution Scheme",
			" ",
			"Sizing:Plant,",
			"  Main Loop, !- Plant or Condenser Loop Name",
			"  heating, !- Loop Type",
			"  82., !- Design Loop Exit Temperature{ C }",
			"  11, !- Loop Design Temperature Difference{ deltaC }",
			"  Coincident, !- Sizing Option",
			"  1, !- Zone Timesteps in Averaging Window",
			"  None;                    !- Coincident Sizing Factor Mode",
			" ",
			"SetpointManager:Scheduled,",
			"  Main Loop Setpoint Manager, !- Name",
			"  Temperature, !- Control Variable",
			"  Main Loop Temp Sch, !- Schedule Name",
			"  Main Loop Setpoint Node List;  !- Setpoint Node or NodeList Name",
			" ",
			"NodeList,",
			"  Main Loop Setpoint Node List, !- Name",
			"  Supply Outlet Node;      !- Node 1 Name",
			" ",
			"PlantEquipmentOperationSchemes,",
			"  Main Loop Operation, !- Name",
			"  PlantEquipmentOperation:HeatingLoad, !- Control Scheme 1 Object Type",
			"  Purchased Only, !- Control Scheme 1 Name",
			"  AlwaysOnSchedule;        !- Control Scheme 1 Schedule Name",
			" ",
			"PlantEquipmentOperation:HeatingLoad,",
			"  Purchased Only, !- Name",
			"  0, !- Load Range 1 Lower Limit{ W }",
			"  10000000, !- Load Range 1 Upper Limit{ W }",
			"  Heating Plant;           !- Range 1 Equipment List Name",
			" ",
			"PlantEquipmentList,",
			"  Heating Plant, !- Name",
			"  DistrictHeating, !- Equipment 1 Object Type",
			"  Purchased Heating;       !- Equipment 1 Name",
			" ",
			"BranchList,",
			"  Supply Branches, !- Name",
			"  Supply Inlet Branch, !- Branch 1 Name",
			"  Heating Branch, !- Branch 2 Name",
			"  Supply Outlet Branch;    !- Branch 3 Name",
			" ",
			"ConnectorList,",
			"  Supply Connectors, !- Name",
			"  Connector:Splitter, !- Connector 1 Object Type",
			"  Supply Splitter, !- Connector 1 Name",
			"  Connector:Mixer, !- Connector 2 Object Type",
			"  Supply Mixer;            !- Connector 2 Name",
			" ",
			"Connector:Splitter,",
			"  Supply Splitter, !- Name",
			"  Supply Inlet Branch, !- Inlet Branch Name",
			"  Heating Branch;          !- Outlet Branch 1 Name",
			" ",
			"Connector:Mixer,",
			"  Supply Mixer, !- Name",
			"  Supply Outlet Branch, !- Outlet Branch Name",
			"  Heating Branch;          !- Inlet Branch 1 Name",
			" ",
			"Branch,",
			"  Supply Inlet Branch, !- Name",
			" 0, !- Maximum Flow Rate{ m3 / s }",
			"  , !- Pressure Drop Curve Name",
			"  Pump:VariableSpeed, !- Component 1 Object Type",
			"  Pump, !- Component 1 Name",
			"  Supply Inlet Node, !- Component 1 Inlet Node Name",
			"  Supply Pump-Heating Node, !- Component 1 Outlet Node Name",
			"  ACTIVE;                  !- Component 1 Branch Control Type",
			" ",
			"Pump:VariableSpeed,",
			"  Pump, !- Name",
			"  Supply Inlet Node, !- Inlet Node Name",
			"  Supply Pump-Heating Node, !- Outlet Node Name",
			"  0.005, !- Rated Flow Rate{ m3 / s }",
			"  300000, !- Rated Pump Head{ Pa }",
			"  2250, !- Rated Power Consumption{ W }",
			"  0.87, !- Motor Efficiency",
			"  0.0, !- Fraction of Motor Inefficiencies to Fluid Stream",
			"  0, !- Coefficient 1 of the Part Load Performance Curve",
			"  1, !- Coefficient 2 of the Part Load Performance Curve",
			"  0, !- Coefficient 3 of the Part Load Performance Curve",
			"  0, !- Coefficient 4 of the Part Load Performance Curve",
			"  0, !- Minimum Flow Rate{ m3 / s }",
			"  INTERMITTENT;            !- Pump Control Type",
			" ",
			"Branch,",
			"  Heating Branch, !- Name",
			"  0, !- Maximum Flow Rate{ m3 / s }",
			"  , !- Pressure Drop Curve Name",
			"  DistrictHeating, !- Component 1 Object Type",
			"  Purchased Heating, !- Component 1 Name",
			"  Supply Heating Inlet Node, !- Component 1 Inlet Node Name",
			"  Supply Heating Outlet Node, !- Component 1 Outlet Node Name",
			"  ACTIVE;                  !- Component 1 Branch Control Type",
			" ",
			"DistrictHeating,",
			"  Purchased Heating, !- Name",
			"  Supply Heating Inlet Node, !- Hot Water Inlet Node Name",
			"  Supply Heating Outlet Node, !- Hot Water Outlet Node Name",
			"  1000000;                 !- Nominal Capacity{ W }",
			" ",
			"Branch,",
			"  Supply Outlet Branch, !- Name",
			"  0, !- Maximum Flow Rate{ m3 / s }",
			"  , !- Pressure Drop Curve Name",
			"  Pipe:Adiabatic, !- Component 1 Object Type",
			"  Supply Outlet Pipe, !- Component 1 Name",
			"  Supply Heating-Pipe Node, !- Component 1 Inlet Node Name",
			"  Supply Outlet Node, !- Component 1 Outlet Node Name",
			"  PASSIVE;                 !- Component 1 Branch Control Type",
			" ",
			"Pipe:Adiabatic,",
			"  Supply Outlet Pipe, !- Name",
			"  Supply Heating-Pipe Node, !- Inlet Node Name",
			"  Supply Outlet Node;      !- Outlet Node Name",
			" ",
			"BranchList,",
			"  Demand Branches, !- Name",
			"  Demand Inlet Branch, !- Branch 1 Name",
			"  Load Profile Branch 1, !- Branch 2 Name",
			"  Demand Outlet Branch;    !- Branch 3 Name",
			" ",
			"ConnectorList,",
			"  Demand Connectors, !- Name",
			"  Connector:Splitter, !- Connector 1 Object Type",
			"  Demand Splitter, !- Connector 1 Name",
			"  Connector:Mixer, !- Connector 2 Object Type",
			"  Demand Mixer;            !- Connector 2 Name",
			" ",
			"Connector:Splitter,",
			"  Demand Splitter, !- Name",
			"  Demand Inlet Branch, !- Inlet Branch Name",
			"  Load Profile Branch 1;   !- Outlet Branch 1 Name",
			" ",
			"Connector:Mixer,",
			"  Demand Mixer, !- Name",
			"  Demand Outlet Branch, !- Outlet Branch Name",
			"  Load Profile Branch 1;   !- Inlet Branch 1 Name",
			" ",
			"Branch,",
			"  Demand Inlet Branch, !- Name",
			"  0, !- Maximum Flow Rate{ m3/s}",
			"  , !- Pressure Drop Curve Name",
			"  Pipe:Adiabatic, !- Component 1 Object Type",
			"  Demand Inlet Pipe, !- Component 1 Name",
			"  Demand Inlet Node, !- Component 1 Inlet Node Name",
			"  VRF Water Inlet Node, !- Component 1 Outlet Node Name",
			"  PASSIVE;                 !- Component 1 Branch Control Type",
			" ",
			"Pipe:Adiabatic,",
			"  Demand Inlet Pipe, !- Name",
			"  Demand Inlet Node, !- Inlet Node Name",
			"  VRF Water Inlet Node;  !- Outlet Node Name",
			" ",
			"Branch,",
			"  Load Profile Branch 1, !- Name",
			"  0, !- Maximum Flow Rate{m3/s}",
			"  , !- Pressure Drop Curve Name",
			"  AirConditioner:VariableRefrigerantFlow, !- Component 1 Object Type",
			"  VRF Water Cooled HP, !- Component 1 Name",
			"  VRF Water Inlet Node, !- Component 1 Inlet Node Name",
			"  VRF Water Outlet Node, !- Component 1 Outlet Node Name",
			"  ACTIVE;                  !- Component 1 Branch Control Type",
			" ",
			"Branch,",
			"  Demand Outlet Branch, !- Name",
			"  0, !- Maximum Flow Rate{ m3 / s }",
			"  , !- Pressure Drop Curve Name",
			"  Pipe:Adiabatic, !- Component 1 Object Type",
			"  Demand Outlet Pipe, !- Component 1 Name",
			"  VRF Water Outlet Node, !- Component 1 Inlet Node Name",
			"  Demand Outlet Node, !- Component 1 Outlet Node Name",
			"  PASSIVE;                 !- Component 1 Branch Control Type",
			" ",
			"Pipe:Adiabatic,",
			"  Demand Outlet Pipe, !- Name",
			"  VRF Water Outlet Node, !- Inlet Node Name",
			"  Demand Outlet Node;      !- Outlet Node Name",
			" ",
			"ScheduleTypeLimits,",
			"  On/Off, !- Name",
			"  0, !- Lower Limit Value",
			"  1, !- Upper Limit Value",
			"  DISCRETE;                !- Numeric Type",
			" ",
			"Schedule:Compact,",
			"  Main Loop Temp Sch, !- Name",
			"  Any Number, !- Schedule Type Limits Name",
			"  THROUGH: 12/31, !- Field 1",
			"  FOR: AllDays, !- Field 2",
			"  UNTIL: 24:00, 60.0;       !- Field 3",
			" ",
			"Schedule:Compact,",
			"  AlwaysOnSchedule, !- Name",
			"  On/Off, !- Schedule Type Limits Name",
			"  THROUGH: 12/31, !- Field 1",
			"  FOR: AllDays, !- Field 2",
			"  UNTIL: 24:00, 1;          !- Field 3",
			" ",
			"Schedule:Compact,",
			"  Load Profile 1 Load Schedule, !- Name",
			"  Any Number, !- Schedule Type Limits Name",
			"  THROUGH: 12/31, !- Field 1",
			"  FOR: AllDays, !- Field 2",
			"  UNTIL: 4:00, 8000, !- Field 3",
			"  UNTIL: 8:00, 6000, !- Field 5",
			"  UNTIL: 9:00, 0, !- Field 7",
			"  UNTIL: 12:00, 6000, !- Field 9",
			"  UNTIL: 24:00, 10000;      !- Field 11",
			" ",
			"Schedule:Compact,",
			"  Load Profile 1 Flow Frac Schedule, !- Name",
			"  Any Number, !- Schedule Type Limits Name",
			"  THROUGH: 12/31, !- Field 1",
			"  FOR: AllDays, !- Field 2",
			"  UNTIL: 24:00, 1.0;        !- Field 3",
		} );

		ASSERT_FALSE( process_idf( idf_objects ) );

		DataGlobals::BeginEnvrnFlag = true;
		DataSizing::CurZoneEqNum = 1;
		DataEnvironment::OutBaroPress = 101325; // sea level
		DataZoneEquipment::ZoneEquipInputsFilled = true; // denotes zone equipment has been read in
		DataEnvironment::StdRhoAir = PsyRhoAirFnPbTdbW( DataEnvironment::OutBaroPress, 20.0, 0.0 );
		DataSizing::ZoneEqSizing.allocate( 1 );
		DataSizing::ZoneSizingRunDone = true;
		DataSizing::ZoneEqSizing( CurZoneEqNum ).DesignSizeFromParent = false;
		DataSizing::ZoneEqSizing( CurZoneEqNum ).SizingMethod.allocate( 25 );
		DataSizing::ZoneEqSizing( CurZoneEqNum ).SizingMethod( DataHVACGlobals::SystemAirflowSizing ) = DataSizing::SupplyAirFlowRate;

		DataSizing::FinalZoneSizing.allocate( 1 );
		DataSizing::FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow = 0.566337; // 400 cfm * 3 tons = 1200 cfm
		DataSizing::FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow = 0.566337;

		DataZoneEnergyDemands::ZoneSysEnergyDemand.allocate( 1 );

		Array2D< Real64 > DummyArray; // Sky temperature
		DataGlobals::NumOfTimeStepInHour = 4;
		DataGlobals::MinutesPerTimeStep = 60 / DataGlobals::NumOfTimeStepInHour;
		DummyArray.allocate( DataGlobals::NumOfTimeStepInHour, 24 );
		DummyArray = 0.0;
		ScheduleManager::GetScheduleValuesForDay( 1, DummyArray, 58, 3 );

		CurveManager::GetCurveInput(); // read curves
		HeatBalanceManager::GetZoneData( ErrorsFound ); // read zone data
		EXPECT_FALSE( ErrorsFound );

		DataZoneEquipment::GetZoneEquipmentData(); // read equipment list and connections

		BranchInputManager::ManageBranchInput();
			// Get plant loop data
		PlantManager::GetPlantLoopData();
		PlantManager::GetPlantInput();

		HVACVariableRefrigerantFlow::MyEnvrnFlag = true;
		ZoneInletAirNode = GetVRFTUZoneInletAirNode( VRFTUNum ); // trigger GetVRFInput by calling a mining function

		Schedule( VRF( VRFCond ).SchedPtr ).CurrentValue = 1.0; // enable the VRF condenser
		Schedule( VRFTU( VRFTUNum ).SchedPtr ).CurrentValue = 1.0; // enable the terminal unit
		Schedule( VRFTU( VRFTUNum ).FanAvailSchedPtr ).CurrentValue = 1.0; // turn on fan
		Schedule( VRFTU( VRFTUNum ).FanOpModeSchedPtr ).CurrentValue = 0.0; // set cycling fan operating mode

		// Test coil sizing

		DataZoneEnergyDemands::ZoneSysEnergyDemand( CurZoneNum ).RemainingOutputRequired = 0.0; // set load = 0
		DataZoneEnergyDemands::ZoneSysEnergyDemand( CurZoneNum ).RemainingOutputReqToCoolSP = 0.0;
		DataZoneEnergyDemands::ZoneSysEnergyDemand( CurZoneNum ).RemainingOutputReqToHeatSP = 0.0;

		DataSizing::FinalZoneSizing( CurZoneEqNum ).ZoneRetTempAtCoolPeak = 26.66667;
		DataSizing::FinalZoneSizing( CurZoneEqNum ).ZoneHumRatAtCoolPeak = 0.01117049470250416; // AHRI condition at 80 F db / 67 F wb
		DataLoopNode::Node( VRF( VRFCond ).CondenserNodeNum ).Temp = 35.0; // AHRI condition at 95 F db
		DataLoopNode::Node( VRFTU( VRFTUNum ).VRFTUOAMixerOANodeNum ).Temp = 35.0; // AHRI condition at 95 F db
		DataLoopNode::Node( VRFTU( VRFTUNum ).VRFTUOAMixerOANodeNum ).HumRat = 0.01; // don't care
		DataSizing::FinalZoneSizing( CurZoneEqNum ).CoolDDNum = 1;
		DataSizing::FinalZoneSizing( CurZoneEqNum ).TimeStepNumAtCoolMax = 1;
		DataSizing::DesDayWeath.allocate( 1 );
		DataSizing::DesDayWeath( 1 ).Temp.allocate( 1 );
		DataSizing::DesDayWeath( FinalZoneSizing( CurZoneEqNum ).CoolDDNum ).Temp( FinalZoneSizing( CurZoneEqNum ).TimeStepNumAtCoolMax ) = 35.0;

		DataSizing::FinalZoneSizing( CurZoneEqNum ).CoolDesTemp = 13.1; // 55.58 F
		DataSizing::FinalZoneSizing( CurZoneEqNum ).CoolDesHumRat = 0.009297628698818194; // humrat at 12.77777 C db / 12.6 C wb

		SizingManager::GetPlantSizingInput();
		PlantManager::InitOneTimePlantSizingInfo( 1 );
		PlantManager::SizePlantLoop( 1, true );
		PlantManager::InitLoopEquip = true;
		// call air-side VRF
		SimulateVRF( VRFTU( VRFTUNum ).Name, CurZoneNum, FirstHVACIteration, SysOutputProvided, LatOutputProvided, ZoneEquipList( CurZoneEqNum ).EquipIndex( EquipPtr ) );
		// call plant-side VRF
		SimVRFCondenserPlant( SimPlantEquipTypes( VRF( VRFCond ).VRFPlantTypeOfNum ), VRF( VRFCond ).VRFPlantTypeOfNum, VRF( VRFCond ).Name, VRFCond, FirstHVACIteration, InitLoopEquip, CurLoad, MaxLoad, MinLoad, OptLoad, LoopNum );

		DataZoneEnergyDemands::ZoneSysEnergyDemand( CurZoneNum ).RemainingOutputRequired = -1000.0; // set cooling load
		DataZoneEnergyDemands::ZoneSysEnergyDemand( CurZoneNum ).RemainingOutputReqToCoolSP = -1000.0;
		DataZoneEnergyDemands::ZoneSysEnergyDemand( CurZoneNum ).RemainingOutputReqToHeatSP = -2000.0;

		BeginEnvrnFlag = true;
		DataLoopNode::Node( VRFTU( VRFTUNum ).VRFTUInletNodeNum ).Temp = 24.0;
		DataLoopNode::Node( VRFTU( VRFTUNum ).VRFTUInletNodeNum ).HumRat = 0.0093;
		DataLoopNode::Node( VRFTU( VRFTUNum ).VRFTUInletNodeNum ).Enthalpy = 47794.1;
		DataEnvironment::OutDryBulbTemp = 35.0;
		DataEnvironment::OutHumRat = 0.017767; // 50% RH
		DataEnvironment::OutBaroPress = 101325.0;
		DataEnvironment::OutWetBulbTemp = 26.045;
		SimulateVRF( VRFTU( VRFTUNum ).Name, CurZoneNum, FirstHVACIteration, SysOutputProvided, LatOutputProvided, ZoneEquipList( CurZoneEqNum ).EquipIndex( EquipPtr ) );
		EXPECT_TRUE( VRF( VRFCond ).VRFCondPLR > 0.0 );
		EXPECT_NEAR( SysOutputProvided, ZoneSysEnergyDemand( CurZoneNum ).RemainingOutputReqToCoolSP, 1.0 );

		rho = GetDensityGlycol( PlantLoop( VRF( VRFCond ).SourceLoopNum ).FluidName, PlantSizData( 1 ).ExitTemp, PlantLoop( VRF( VRFCond ).SourceLoopNum ).FluidIndex, RoutineName );
		Cp = GetSpecificHeatGlycol( PlantLoop( VRF( VRFCond ).SourceLoopNum ).FluidName, PlantSizData( 1 ).ExitTemp, PlantLoop( VRF( VRFCond ).SourceLoopNum ).FluidIndex, RoutineName );
		CondVolFlowRate = max( VRF( VRFCond ).CoolingCapacity, VRF( VRFCond ).HeatingCapacity ) / ( PlantSizData( 1 ).DeltaT * Cp * rho );

		EXPECT_DOUBLE_EQ( CondVolFlowRate, VRF( VRFCond ).WaterCondVolFlowRate );

		rho = GetDensityGlycol( PlantLoop( VRF( VRFCond ).SourceLoopNum ).FluidName, InitConvTemp, PlantLoop( VRF( VRFCond ).SourceLoopNum ).FluidIndex, RoutineName );
		EXPECT_DOUBLE_EQ( VRF( VRFCond ).WaterCondenserDesignMassFlow, ( VRF( VRFCond ).WaterCondVolFlowRate * rho ) );

		// set zone load to heating
		DataZoneEnergyDemands::ZoneSysEnergyDemand( CurZoneNum ).RemainingOutputRequired = VRF( VRFCond ).HeatingCapacity; // set load equal to the VRF heating capacity
		DataZoneEnergyDemands::ZoneSysEnergyDemand( CurZoneNum ).RemainingOutputReqToCoolSP = VRF( VRFCond ).HeatingCapacity + 1000.0; // simulates a dual Tstat with load to cooling SP > load to heating SP
		DataZoneEnergyDemands::ZoneSysEnergyDemand( CurZoneNum ).RemainingOutputReqToHeatSP = VRF( VRFCond ).HeatingCapacity;

		DataLoopNode::Node( VRF( VRFCond ).CondenserNodeNum ).Temp = 7.0; // water inlet temperature
		DataLoopNode::Node( VRFTU( VRFTUNum ).VRFTUInletNodeNum ).Temp = 20.0; // TU inlet air temp
		DataLoopNode::Node( VRFTU( VRFTUNum ).VRFTUInletNodeNum ).HumRat = 0.0056; // TU inlet air humrat
		DataLoopNode::Node( VRFTU( VRFTUNum ).VRFTUInletNodeNum ).Enthalpy = 34823.5; // TU inlet air enthalpy
		DataEnvironment::OutDryBulbTemp = 5.0;
		DataEnvironment::OutHumRat = 0.00269; //50% RH
		DataEnvironment::OutBaroPress = 101325.0;
		DataEnvironment::OutWetBulbTemp = 1.34678;
		SimulateVRF( VRFTU( VRFTUNum ).Name, CurZoneNum, FirstHVACIteration, SysOutputProvided, LatOutputProvided, ZoneEquipList( CurZoneEqNum ).EquipIndex( EquipPtr ) );

		EXPECT_TRUE( VRF( VRFCond ).VRFCondPLR > 0.0 );
		EXPECT_NEAR( SysOutputProvided, ZoneSysEnergyDemand( CurZoneNum ).RemainingOutputReqToHeatSP, 1.0 );

		ASSERT_EQ( VRF( VRFCond ).WaterCondenserDesignMassFlow, Node( VRF( VRFCond ).CondenserNodeNum ).MassFlowRate ); // Condenser flow rate should be set for active cooling
		ASSERT_EQ( VRF( VRFCond ).WaterCondenserDesignMassFlow, Node( VRF( VRFCond ).CondenserOutletNodeNum ).MassFlowRate ); // outlet node should also be set

		// clean up
		ZoneSysEnergyDemand.deallocate();
	}


	TEST_F( EnergyPlusFixture, VRFTest_NoLoad_OAMassFlowRateTest ) {

		//static std::string const RoutineName( "VRFTest_NoLoadOAFlowTest" );
		bool ErrorsFound( false );        // function returns true on error
		bool FirstHVACIteration( true );  // simulate the first pass through HVAC simulation, use false for next iteration
		int VRFTUNum( 1 );                // index to VRF terminal unit
		int CurZoneNum( 1 );              // index to zone
		int ZoneInletAirNode( 0 );        // zone inlet node number
		int OutsideAirNode( 0 );          // VRFTU Outside air inlet node
		Real64 AverageOAMassFlow( 0.0 );  // VRFTU Outside air mass flow rate 
		int ZoneNum( 1 );                 // current zone index
		Real64 QZnReq( 0.0 );             // current zone load to set point
		Real64 PartLoadRatio( 0.0 );      // unit part load ratio
		Real64 OnOffAirFlowRatio( 1.0 );  // ratio of compressor ON airflow to average airflow over timestep

		std::string const idf_objects = delimited_string( {

			"  Version,8.4;",

			"  ScheduleTypeLimits,",
			"    Fraction,                !- Name",
			"    0.0,                     !- Lower Limit Value",
			"    1.0,                     !- Upper Limit Value",
			"    CONTINUOUS;              !- Numeric Type",

			"  Schedule:Compact,",
			"    AlwaysOn,                !- Name",
			"    Fraction,                !- Schedule Type Limits Name",
			"    Through: 12/31,          !- Field 1",
			"    For: AllDays,            !- Field 2",
			"    Until: 24:00,1;          !- Field 3",

			"  ZoneHVAC:TerminalUnit:VariableRefrigerantFlow,",
			"    Level1:Office1 VRF Indoor Unit,  !- Zone Terminal Unit Name",
			"    AlwaysOn,                !- Terminal Unit Availability Schedule",
			"    Level1:Office1 VRF Indoor Unit Return,  !- Terminal Unit Air Inlet Node Name",
			"    Level1:Office1 VRF Indoor Unit Supply Inlet,  !- Terminal Unit Air Outlet Node Name",
			"    0.111000,                !- Cooling Supply Air Flow Rate {m3/s}",
			"    0.056000,                !- No Cooling Supply Air Flow Rate {m3/s}",
			"    0.111000,                !- Heating Supply Air Flow Rate {m3/s}",
			"    0.056000,                !- No Heating Supply Air Flow Rate {m3/s}",
			"    0.028000,                !- Cooling Outdoor Air Flow Rate {m3/s}",
			"    0.028000,                !- Heating Outdoor Air Flow Rate {m3/s}",
			"    0.014000,                !- No Load Outdoor Air Flow Rate {m3/s}",
			"    AlwaysOn,                !- Supply Air Fan Operating Mode Schedule Name",
			"    BlowThrough,             !- Supply Air Fan Placement",
			"    Fan:ConstantVolume,      !- Supply Air Fan Object Type",
			"    Level1:Office1 VRF Indoor Unit Supply Fan,  !- Supply Air Fan Object Name",
			"    OutdoorAir:Mixer,        !- Outside Air Mixer Object Type",
			"    Level1:Office1 VRF Indoor Unit Outdoor Air Mixer,  !- Outside Air Mixer Object Name",
			"    Coil:Cooling:DX:VariableRefrigerantFlow,  !- Cooling Coil Object Type",
			"    Level1:Office1 VRF Indoor Unit DX Cooling Coil,  !- Cooling Coil Object Name",
			"    Coil:Heating:DX:VariableRefrigerantFlow,  !- Heating Coil Object Type",
			"    Level1:Office1 VRF Indoor Unit DX Heating Coil,  !- Heating Coil Object Name",
			"    30.0000,                 !- Zone Terminal Unit On Parasitic Electric Energy Use {W}",
			"    20.0000;                 !- Zone Terminal Unit Off Parasitic Electric Energy Use {W}",

			"  ZoneHVAC:EquipmentList,",
			"    Level1:Office1 Equipment,!- Name",
			"    ZoneHVAC:TerminalUnit:VariableRefrigerantFlow,  !- Zone Equipment 1 Object Type",
			"    Level1:Office1 VRF Indoor Unit,  !- Zone Equipment 1 Name",
			"    1,                       !- Zone Equipment 1 Cooling Sequence",
			"    1;                       !- Zone Equipment 1 Heating or No-Load Sequence",

			"  ZoneHVAC:EquipmentConnections,",
			"    Level1:Office1,          !- Zone Name",
			"    Level1:Office1 Equipment,!- Zone Conditioning Equipment List Name",
			"    Level1:Office1 Air Inlet Node List,  !- Zone Air Inlet Node or NodeList Name",
			"    Level1:Office1 Air Exhaust Node List,  !- Zone Air Exhaust Node or NodeList Name",
			"    Level1:Office1 Zone Air Node,  !- Zone Air Node Name",
			"    Level1:Office1 Return Outlet;  !- Zone Return Air Node Name",

			"  Zone,",
			"    Level1:Office1,          !- Name",
			"    0,                       !- Direction of Relative North {deg}",
			"    0,                       !- X Origin {m}",
			"    0,                       !- Y Origin {m}",
			"    0,                       !- Z Origin {m}",
			"    1,                       !- Type",
			"    1,                       !- Multiplier",
			"    ,                        !- Ceiling Height {m}",
			"    68.2413,                 !- Volume {m3}",
			"    19.4975,                 !- Floor Area {m2}",
			"    TARP;                    !- Zone Inside Convection Algorithm",

			"  Fan:ConstantVolume,",
			"    Level1:Office1 VRF Indoor Unit Supply Fan,  !- Name",
			"    AlwaysOn,                 !- Availability Schedule Name",
			"    0.70,                    !- Fan Total Efficiency",
			"    100.00,                  !- Pressure Rise {Pa}",
			"    0.111000,                !- Maximum Flow Rate {m3/s}",
			"    0.90,                    !- Motor Efficiency",
			"    1.00,                    !- Motor In Airstream Fraction",
			"    Level1:Office1 VRF Indoor Unit Mixed Air Outlet,  !- Air Inlet Node Name",
			"    Level1:Office1 VRF Indoor Unit Supply Fan Outlet,  !- Air Outlet Node Name",
			"    General;                 !- End-Use Subcategory",

			"  Coil:Cooling:DX:VariableRefrigerantFlow,",
			"    Level1:Office1 VRF Indoor Unit DX Cooling Coil,  !- Name",
			"    AlwaysOn,                 !- Availability Schedule Name",
			"    4000.0,                  !- Gross Rated Total Cooling Capacity {W}",
			"    0.75,                    !- Gross Rated Sensible Heat Ratio",
			"    0.111000,                !- Rated Air Flow Rate {m3/s}",
			"    VRFTUCoolCapFT,          !- Cooling Capacity Ratio Modifier Function of Temperature Curve Name",
			"    VRFACCoolCapFFF,         !- Cooling Capacity Modifier Curve Function of Flow Fraction Name",
			"    Level1:Office1 VRF Indoor Unit Supply Fan Outlet,  !- Coil Air Inlet Node",
			"    Level1:Office1 VRF Indoor Unit DX Cooling Coil Outlet;  !- Coil Air Outlet Node",

			"  Coil:Heating:DX:VariableRefrigerantFlow,",
			"    Level1:Office1 VRF Indoor Unit DX Heating Coil,  !- Name",
			"    AlwaysOn,                !- Availability Schedule",
			"    4000.0,                  !- Gross Rated Heating Capacity {W}",
			"    0.111000,                !- Rated Air Flow Rate {m3/s}",
			"    Level1:Office1 VRF Indoor Unit DX Cooling Coil Outlet,  !- Coil Air Inlet Node",
			"    Level1:Office1 VRF Indoor Unit Supply Inlet,  !- Coil Air Outlet Node",
			"    VRFTUHeatCapFT,          !- Heating Capacity Ratio Modifier Function of Temperature Curve Name",
			"    VRFACCoolCapFFF;         !- Heating Capacity Modifier Function of Flow Fraction Curve Name",

			"  AirConditioner:VariableRefrigerantFlow,",
			"    VRF Outdoor Unit,        !- Heat Pump Name",
			"    AlwaysOn,                !- Availability Schedule Name",
			"    7040.0,                  !- Gross Rated Total Cooling Capacity {W}",
			"    3.3,                     !- Gross Rated Cooling COP {W/W}",
			"    -6,                      !- Minimum Outdoor Temperature in Cooling Mode {C}",
			"    43,                      !- Maximum Outdoor Temperature in Cooling Mode {C}",
			"    VRFCoolCapFT,            !- Cooling Capacity Ratio Modifier Function of Low Temperature Curve Name",
			"    VRFCoolCapFTBoundary,    !- Cooling Capacity Ratio Boundary Curve Name",
			"    VRFCoolCapFTHi,          !- Cooling Capacity Ratio Modifier Function of High Temperature Curve Name",
			"    VRFCoolEIRFT,            !- Cooling Energy Input Ratio Modifier Function of Low Temperature Curve Name",
			"    VRFCoolEIRFTBoundary,    !- Cooling Energy Input Ratio Boundary Curve Name",
			"    VRFCoolEIRFTHi,          !- Cooling Energy Input Ratio Modifier Function of High Temperature Curve Name",
			"    CoolingEIRLowPLR,        !- Cooling Energy Input Ratio Modifier Function of Low Part-Load Ratio Curve Name",
			"    CoolingEIRHiPLR,         !- Cooling Energy Input Ratio Modifier Function of High Part-Load Ratio Curve Name",
			"    CoolingCombRatio,        !- Cooling Combination Ratio Correction Factor Curve Name",
			"    VRFCPLFFPLR,             !- Cooling Part-Load Fraction Correlation Curve Name",
			"    autosize,                !- Gross Rated Heating Capacity {W}",
			"    1,                       !- Rated Heating Capacity Sizing Ratio {W/W}",
			"    3.4,                     !- Gross Rated Heating COP {W/W}",
			"    -20,                     !- Minimum Outdoor Temperature in Heating Mode {C}",
			"    40,                      !- Maximum Outdoor Temperature in Heating Mode {C}",
			"    VRFHeatCapFT,            !- Heating Capacity Ratio Modifier Function of Low Temperature Curve Name",
			"    VRFHeatCapFTBoundary,    !- Heating Capacity Ratio Boundary Curve Name",
			"    VRFHeatCapFTHi,          !- Heating Capacity Ratio Modifier Function of High Temperature Curve Name",
			"    VRFHeatEIRFT,            !- Heating Energy Input Ratio Modifier Function of Low Temperature Curve Name",
			"    VRFHeatEIRFTBoundary,    !- Heating Energy Input Ratio Boundary Curve Name",
			"    VRFHeatEIRFTHi,          !- Heating Energy Input Ratio Modifier Function of High Temperature Curve Name",
			"    WetBulbTemperature,      !- Heating Performance Curve Outdoor Temperature Type",
			"    HeatingEIRLowPLR,        !- Heating Energy Input Ratio Modifier Function of Low Part-Load Ratio Curve Name",
			"    HeatingEIRHiPLR,         !- Heating Energy Input Ratio Modifier Function of High Part-Load Ratio Curve Name",
			"    HeatingCombRatio,        !- Heating Combination Ratio Correction Factor Curve Name",
			"    VRFCPLFFPLR,             !- Heating Part-Load Fraction Correlation Curve Name",
			"    0.15,                    !- Minimum Heat Pump Part-Load Ratio {dimensionless}",
			"    <Select zone>,           !- Zone Name for Master Thermostat Location",
			"    LoadPriority,            !- Master Thermostat Priority Control Type",
			"    ,                        !- Thermostat Priority Schedule Name",
			"    VRF Outdoor Unit Zone List,  !- Zone Terminal Unit List Name",
			"    No,                      !- Heat Pump Waste Heat Recovery",
			"    50,                      !- Equivalent Piping Length used for Piping Correction Factor in Cooling Mode {m}",
			"    15,                      !- Vertical Height used for Piping Correction Factor {m}",
			"    CoolingLengthCorrectionFactor,  !- Piping Correction Factor for Length in Cooling Mode Curve Name",
			"    -.000386,                !- Piping Correction Factor for Height in Cooling Mode Coefficient {1/m}",
			"    50,                      !- Equivalent Piping Length used for Piping Correction Factor in Heating Mode {m}",
			"    VRF Piping Correction Factor for Length in Heating Mode,  !- Piping Correction Factor for Length in Heating Mode Curve Name",
			"    0,                       !- Piping Correction Factor for Height in Heating Mode Coefficient {1/m}",
			"    15,                      !- Crankcase Heater Power per Compressor {W}",
			"    2,                       !- Number of Compressors {dimensionless}",
			"    0.5,                     !- Ratio of Compressor Size to Total Compressor Capacity {W/W}",
			"    5,                       !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater {C}",
			"    Resistive,               !- Defrost Strategy",
			"    Timed,                   !- Defrost Control",
			"    DXHtgCoilDefrostEIRFT,   !- Defrost Energy Input Ratio Modifier Function of Temperature Curve Name",
			"    0.058333,                !- Defrost Time Period Fraction {dimensionless}",
			"    autosize,                !- Resistive Defrost Heater Capacity {W}",
			"    5,                       !- Maximum Outdoor Dry-bulb Temperature for Defrost Operation {C}",
			"    AirCooled,               !- Condenser Type",
			"    VRF Outdoor Unit Outdoor Air Node,  !- Condenser Inlet Node Name",
			"    ,                        !- Condenser Outlet Node Name",
			"    autosize,                !- Water Condenser Volume Flow Rate {m3/s}",
			"    0.9,                     !- Evaporative Condenser Effectiveness {dimensionless}",
			"    autosize,                !- Evaporative Condenser Air Flow Rate {m3/s}",
			"    autosize,                !- Evaporative Condenser Pump Rated Power Consumption {W}",
			"    ,                        !- Supply Water Storage Tank Name",
			"    0,                       !- Basin Heater Capacity {W/K}",
			"    2,                       !- Basin Heater Setpoint Temperature {C}",
			"    AlwaysOn,                 !- Basin Heater Operating Schedule Name",
			"    Electricity,             !- Fuel Type",
			"    -10,                     !- Minimum Outdoor Temperature in Heat Recovery Mode {C}",
			"    40,                      !- Maximum Outdoor Temperature in Heat Recovery Mode {C}",
			"    VRF Heat Recovery Cooling Capacity Modifier,  !- Heat Recovery Cooling Capacity Modifier Curve Name",
			"    0.5,                     !- Initial Heat Recovery Cooling Capacity Fraction {W/W}",
			"    0.15,                    !- Heat Recovery Cooling Capacity Time Constant {hr}",
			"    VRF Heat Recovery Cooling Energy Modifier,  !- Heat Recovery Cooling Energy Modifier Curve Name",
			"    1,                       !- Initial Heat Recovery Cooling Energy Fraction {W/W}",
			"    0,                       !- Heat Recovery Cooling Energy Time Constant {hr}",
			"    VRF Heat Recovery Heating Capacity Modifier,  !- Heat Recovery Heating Capacity Modifier Curve Name",
			"    1,                       !- Initial Heat Recovery Heating Capacity Fraction {W/W}",
			"    0.15,                    !- Heat Recovery Heating Capacity Time Constant {hr}",
			"    VRF Heat Recovery Heating Energy Modifier,  !- Heat Recovery Heating Energy Modifier Curve Name",
			"    1,                       !- Initial Heat Recovery Heating Energy Fraction {W/W}",
			"    0;                       !- Heat Recovery Heating Energy Time Constant {hr}",

			"  ZoneTerminalUnitList,",
			"    VRF Outdoor Unit Zone List,  !- Zone Terminal Unit List Name",
			"    Level1:Office1 VRF Indoor Unit;  !- Zone Terminal Unit Name 1",

			"  OutdoorAir:Mixer,",
			"    Level1:Office1 VRF Indoor Unit Outdoor Air Mixer,  !- Name",
			"    Level1:Office1 VRF Indoor Unit Mixed Air Outlet,  !- Mixed Air Node Name",
			"    Level1:Office1 VRF Indoor Unit Outdoor Air Node Name,  !- Outdoor Air Stream Node Name",
			"    Level1:Office1 VRF Indoor Unit Air Relief Node Name,  !- Relief Air Stream Node Name",
			"    Level1:Office1 VRF Indoor Unit Return;  !- Return Air Stream Node Name",

			"  NodeList,",
			"    Level1:Office1 Air Inlet Node List,  !- Name",
			"    Level1:Office1 VRF Indoor Unit Supply Inlet;  !- Node 1 Name",

			"  NodeList,",
			"    Level1:Office1 Air Exhaust Node List,  !- Name",
			"    Level1:Office1 VRF Indoor Unit Return;  !- Node 1 Name",

			"  OutdoorAir:NodeList,",
			"    VRF Outdoor Unit Outdoor Air Node;  !- Node or NodeList Name 1",

			"  OutdoorAir:NodeList,",
			"    Level1:Office1 VRF Indoor Unit Outdoor Air Node Name;  !- Node or NodeList Name 1",

			"  Curve:Linear,",
			"    CoolingCombRatio,        !- Name",
			"    0.618055,                !- Coefficient1 Constant",
			"    0.381945,                !- Coefficient2 x",
			"    1.0,                     !- Minimum Value of x",
			"    1.5;                     !- Maximum Value of x",

			"  Curve:Linear,",
			"    HeatingCombRatio,        !- Name",
			"    0.96034,                 !- Coefficient1 Constant",
			"    0.03966,                 !- Coefficient2 x",
			"    1.0,                     !- Minimum Value of x",
			"    1.5;                     !- Maximum Value of x",

			"  Curve:Quadratic,",
			"    VRFACCoolCapFFF,         !- Name",
			"    0.8,                     !- Coefficient1 Constant",
			"    0.2,                     !- Coefficient2 x",
			"    0.0,                     !- Coefficient3 x**2",
			"    0.5,                     !- Minimum Value of x",
			"    1.5;                     !- Maximum Value of x",

			"  Curve:Quadratic,",
			"    CoolingEIRHiPLR,         !- Name",
			"    1.0,                     !- Coefficient1 Constant",
			"    0.0,                     !- Coefficient2 x",
			"    0.0,                     !- Coefficient3 x**2",
			"    1.0,                     !- Minimum Value of x",
			"    1.5;                     !- Maximum Value of x",

			"  Curve:Quadratic,",
			"    VRFCPLFFPLR,             !- Name",
			"    0.85,                    !- Coefficient1 Constant",
			"    0.15,                    !- Coefficient2 x",
			"    0.0,                     !- Coefficient3 x**2",
			"    0.0,                     !- Minimum Value of x",
			"    1.0;                     !- Maximum Value of x",

			"  Curve:Quadratic,",
			"    HeatingEIRHiPLR,         !- Name",
			"    2.4294355,               !- Coefficient1 Constant",
			"    -2.235887,               !- Coefficient2 x",
			"    0.8064516,               !- Coefficient3 x**2",
			"    1.0,                     !- Minimum Value of x",
			"    1.5;                     !- Maximum Value of x",

			"  Curve:Cubic,",
			"    DefaultFanEffRatioCurve, !- Name",
			"    0.33856828,              !- Coefficient1 Constant",
			"    1.72644131,              !- Coefficient2 x",
			"    -1.49280132,             !- Coefficient3 x**2",
			"    0.42776208,              !- Coefficient4 x**3",
			"    0.5,                     !- Minimum Value of x",
			"    1.5,                     !- Maximum Value of x",
			"    0.3,                     !- Minimum Curve Output",
			"    1.0;                     !- Maximum Curve Output",

			"  Curve:Cubic,",
			"    VRFTUCoolCapFT,          !- Name",
			"    0.504547273506488,       !- Coefficient1 Constant",
			"    0.0288891279198444,      !- Coefficient2 x",
			"    -0.000010819418650677,   !- Coefficient3 x**2",
			"    0.0000101359395177008,   !- Coefficient4 x**3",
			"    0.0,                     !- Minimum Value of x",
			"    50.0,                    !- Maximum Value of x",
			"    0.5,                     !- Minimum Curve Output",
			"    1.5,                     !- Maximum Curve Output",
			"    Temperature,             !- Input Unit Type for X",
			"    Dimensionless;           !- Output Unit Type",

			"  Curve:Cubic,",
			"    VRFTUHeatCapFT,          !- Name",
			"    -0.390708928227928,      !- Coefficient1 Constant",
			"    0.261815023760162,       !- Coefficient2 x",
			"    -0.0130431603151873,     !- Coefficient3 x**2",
			"    0.000178131745997821,    !- Coefficient4 x**3",
			"    0.0,                     !- Minimum Value of x",
			"    50.0,                    !- Maximum Value of x",
			"    0.5,                     !- Minimum Curve Output",
			"    1.5,                     !- Maximum Curve Output",
			"    Temperature,             !- Input Unit Type for X",
			"    Dimensionless;           !- Output Unit Type",

			"  Curve:Cubic,",
			"    VRFCoolCapFTBoundary,    !- Name",
			"    25.73473775,             !- Coefficient1 Constant",
			"    -0.03150043,             !- Coefficient2 x",
			"    -0.01416595,             !- Coefficient3 x**2",
			"    0,                       !- Coefficient4 x**3",
			"    11,                      !- Minimum Value of x",
			"    30,                      !- Maximum Value of x",
			"    ,                        !- Minimum Curve Output",
			"    ,                        !- Maximum Curve Output",
			"    Temperature,             !- Input Unit Type for X",
			"    Temperature;             !- Output Unit Type",

			"  Curve:Cubic,",
			"    VRFCoolEIRFTBoundary,    !- Name",
			"    25.73473775,             !- Coefficient1 Constant",
			"    -0.03150043,             !- Coefficient2 x",
			"    -0.01416595,             !- Coefficient3 x**2",
			"    0,                       !- Coefficient4 x**3",
			"    15,                      !- Minimum Value of x",
			"    24,                      !- Maximum Value of x",
			"    ,                        !- Minimum Curve Output",
			"    ,                        !- Maximum Curve Output",
			"    Temperature,             !- Input Unit Type for X",
			"    Temperature;             !- Output Unit Type",

			"  Curve:Cubic,",
			"    CoolingEIRLowPLR,        !- Name",
			"    0.4628123,               !- Coefficient1 Constant",
			"    -1.0402406,              !- Coefficient2 x",
			"    2.17490997,              !- Coefficient3 x**2",
			"    -0.5974817,              !- Coefficient4 x**3",
			"    0,                       !- Minimum Value of x",
			"    1,                       !- Maximum Value of x",
			"    ,                        !- Minimum Curve Output",
			"    ,                        !- Maximum Curve Output",
			"    Temperature,             !- Input Unit Type for X",
			"    Temperature;             !- Output Unit Type",

			"  Curve:Cubic,",
			"    VRFHeatCapFTBoundary,    !- Name",
			"    -7.6000882,              !- Coefficient1 Constant",
			"    3.05090016,              !- Coefficient2 x",
			"    -0.1162844,              !- Coefficient3 x**2",
			"    0.0,                     !- Coefficient4 x**3",
			"    15,                      !- Minimum Value of x",
			"    27,                      !- Maximum Value of x",
			"    ,                        !- Minimum Curve Output",
			"    ,                        !- Maximum Curve Output",
			"    Temperature,             !- Input Unit Type for X",
			"    Temperature;             !- Output Unit Type",

			"  Curve:Cubic,",
			"    VRFHeatEIRFTBoundary,    !- Name",
			"    -7.6000882,              !- Coefficient1 Constant",
			"    3.05090016,              !- Coefficient2 x",
			"    -0.1162844,              !- Coefficient3 x**2",
			"    0.0,                     !- Coefficient4 x**3",
			"    15,                      !- Minimum Value of x",
			"    27,                      !- Maximum Value of x",
			"    -20,                     !- Minimum Curve Output",
			"    15,                      !- Maximum Curve Output",
			"    Temperature,             !- Input Unit Type for X",
			"    Temperature;             !- Output Unit Type",

			"  Curve:Cubic,",
			"    HeatingEIRLowPLR,        !- Name",
			"    0.1400093,               !- Coefficient1 Constant",
			"    0.6415002,               !- Coefficient2 x",
			"    0.1339047,               !- Coefficient3 x**2",
			"    0.0845859,               !- Coefficient4 x**3",
			"    0,                       !- Minimum Value of x",
			"    1,                       !- Maximum Value of x",
			"    ,                        !- Minimum Curve Output",
			"    ,                        !- Maximum Curve Output",
			"    Dimensionless,           !- Input Unit Type for X",
			"    Dimensionless;           !- Output Unit Type",

			"  Curve:Exponent,",
			"    DefaultFanPowerRatioCurve,  !- Name",
			"    0,                       !- Coefficient1 Constant",
			"    1,                       !- Coefficient2 Constant",
			"    3,                       !- Coefficient3 Constant",
			"    0,                       !- Minimum Value of x",
			"    1.5,                     !- Maximum Value of x",
			"    0.01,                    !- Minimum Curve Output",
			"    1.5;                     !- Maximum Curve Output",

			"  Curve:Biquadratic,",
			"    DXHtgCoilDefrostEIRFT,   !- Name",
			"    1.0,                     !- Coefficient1 Constant",
			"    0.0,                     !- Coefficient2 x",
			"    0.0,                     !- Coefficient3 x**2",
			"    0.0,                     !- Coefficient4 y",
			"    0,                       !- Coefficient5 y**2",
			"    0,                       !- Coefficient6 x*y",
			"    0.0,                     !- Minimum Value of x",
			"    50.0,                    !- Maximum Value of x",
			"    0.0,                     !- Minimum Value of y",
			"    50.0,                    !- Maximum Value of y",
			"    ,                        !- Minimum Curve Output",
			"    ,                        !- Maximum Curve Output",
			"    Temperature,             !- Input Unit Type for X",
			"    Temperature,             !- Input Unit Type for Y",
			"    Dimensionless;           !- Output Unit Type",

			"  Curve:Biquadratic,",
			"    VRFCoolCapFT,            !- Name",
			"    0.576882692,             !- Coefficient1 Constant",
			"    0.017447952,             !- Coefficient2 x",
			"    0.000583269,             !- Coefficient3 x**2",
			"    -1.76324E-06,            !- Coefficient4 y",
			"    -7.474E-09,              !- Coefficient5 y**2",
			"    -1.30413E-07,            !- Coefficient6 x*y",
			"    15,                      !- Minimum Value of x",
			"    24,                      !- Maximum Value of x",
			"    -5,                      !- Minimum Value of y",
			"    23,                      !- Maximum Value of y",
			"    ,                        !- Minimum Curve Output",
			"    ,                        !- Maximum Curve Output",
			"    Temperature,             !- Input Unit Type for X",
			"    Temperature,             !- Input Unit Type for Y",
			"    Dimensionless;           !- Output Unit Type",

			"  Curve:Biquadratic,",
			"    VRFCoolCapFTHi,          !- Name",
			"    0.6867358,               !- Coefficient1 Constant",
			"    0.0207631,               !- Coefficient2 x",
			"    0.0005447,               !- Coefficient3 x**2",
			"    -0.0016218,              !- Coefficient4 y",
			"    -4.259E-07,              !- Coefficient5 y**2",
			"    -0.0003392,              !- Coefficient6 x*y",
			"    15,                      !- Minimum Value of x",
			"    24,                      !- Maximum Value of x",
			"    16,                      !- Minimum Value of y",
			"    43,                      !- Maximum Value of y",
			"    ,                        !- Minimum Curve Output",
			"    ,                        !- Maximum Curve Output",
			"    Temperature,             !- Input Unit Type for X",
			"    Temperature,             !- Input Unit Type for Y",
			"    Dimensionless;           !- Output Unit Type",

			"  Curve:Biquadratic,",
			"    VRFCoolEIRFT,            !- Name",
			"    0.989010541,             !- Coefficient1 Constant",
			"    -0.02347967,             !- Coefficient2 x",
			"    0.000199711,             !- Coefficient3 x**2",
			"    0.005968336,             !- Coefficient4 y",
			"    -1.0289E-07,             !- Coefficient5 y**2",
			"    -0.00015686,             !- Coefficient6 x*y",
			"    15,                      !- Minimum Value of x",
			"    24,                      !- Maximum Value of x",
			"    -5,                      !- Minimum Value of y",
			"    23,                      !- Maximum Value of y",
			"    ,                        !- Minimum Curve Output",
			"    ,                        !- Maximum Curve Output",
			"    Temperature,             !- Input Unit Type for X",
			"    Temperature,             !- Input Unit Type for Y",
			"    Dimensionless;           !- Output Unit Type",

			"  Curve:Biquadratic,",
			"    VRFCoolEIRFTHi,          !- Name",
			"    0.14351470,              !- Coefficient1 Constant",
			"    0.01860035,              !- Coefficient2 x",
			"    -0.0003954,              !- Coefficient3 x**2",
			"    0.02485219,              !- Coefficient4 y",
			"    0.00016329,              !- Coefficient5 y**2",
			"    -0.0006244,              !- Coefficient6 x*y",
			"    15,                      !- Minimum Value of x",
			"    24,                      !- Maximum Value of x",
			"    16,                      !- Minimum Value of y",
			"    43,                      !- Maximum Value of y",
			"    ,                        !- Minimum Curve Output",
			"    ,                        !- Maximum Curve Output",
			"    Temperature,             !- Input Unit Type for X",
			"    Temperature,             !- Input Unit Type for Y",
			"    Dimensionless;           !- Output Unit Type",

			"  Curve:Biquadratic,",
			"    VRFHeatCapFT,            !- Name",
			"    1.014599599,             !- Coefficient1 Constant",
			"    -0.002506703,            !- Coefficient2 x",
			"    -0.000141599,            !- Coefficient3 x**2",
			"    0.026931595,             !- Coefficient4 y",
			"    1.83538E-06,             !- Coefficient5 y**2",
			"    -0.000358147,            !- Coefficient6 x*y",
			"    15,                      !- Minimum Value of x",
			"    27,                      !- Maximum Value of x",
			"    -20,                     !- Minimum Value of y",
			"    15,                      !- Maximum Value of y",
			"    ,                        !- Minimum Curve Output",
			"    ,                        !- Maximum Curve Output",
			"    Temperature,             !- Input Unit Type for X",
			"    Temperature,             !- Input Unit Type for Y",
			"    Dimensionless;           !- Output Unit Type",

			"  Curve:Biquadratic,",
			"    VRFHeatCapFTHi,          !- Name",
			"    1.161134821,             !- Coefficient1 Constant",
			"    0.027478868,             !- Coefficient2 x",
			"    -0.00168795,             !- Coefficient3 x**2",
			"    0.001783378,             !- Coefficient4 y",
			"    2.03208E-06,             !- Coefficient5 y**2",
			"    -6.8969E-05,             !- Coefficient6 x*y",
			"    15,                      !- Minimum Value of x",
			"    27,                      !- Maximum Value of x",
			"    -10,                     !- Minimum Value of y",
			"    15,                      !- Maximum Value of y",
			"    ,                        !- Minimum Curve Output",
			"    ,                        !- Maximum Curve Output",
			"    Temperature,             !- Input Unit Type for X",
			"    Temperature,             !- Input Unit Type for Y",
			"    Dimensionless;           !- Output Unit Type",

			"  Curve:Biquadratic,",
			"    VRFHeatEIRFT,            !- Name",
			"    0.87465501,              !- Coefficient1 Constant",
			"    -0.01319754,             !- Coefficient2 x",
			"    0.00110307,              !- Coefficient3 x**2",
			"    -0.0133118,              !- Coefficient4 y",
			"    0.00089017,              !- Coefficient5 y**2",
			"    -0.00012766,             !- Coefficient6 x*y",
			"    15,                      !- Minimum Value of x",
			"    27,                      !- Maximum Value of x",
			"    -20,                     !- Minimum Value of y",
			"    12,                      !- Maximum Value of y",
			"    ,                        !- Minimum Curve Output",
			"    ,                        !- Maximum Curve Output",
			"    Temperature,             !- Input Unit Type for X",
			"    Temperature,             !- Input Unit Type for Y",
			"    Dimensionless;           !- Output Unit Type",

			"  Curve:Biquadratic,",
			"    VRFHeatEIRFTHi,          !- Name",
			"    2.504005146,             !- Coefficient1 Constant",
			"    -0.05736767,             !- Coefficient2 x",
			"    4.07336E-05,             !- Coefficient3 x**2",
			"    -0.12959669,             !- Coefficient4 y",
			"    0.00135839,              !- Coefficient5 y**2",
			"    0.00317047,              !- Coefficient6 x*y",
			"    15,                      !- Minimum Value of x",
			"    27,                      !- Maximum Value of x",
			"    -10,                     !- Minimum Value of y",
			"    15,                      !- Maximum Value of y",
			"    ,                        !- Minimum Curve Output",
			"    ,                        !- Maximum Curve Output",
			"    Temperature,             !- Input Unit Type for X",
			"    Temperature,             !- Input Unit Type for Y",
			"    Dimensionless;           !- Output Unit Type",

			"  Curve:Biquadratic,",
			"    CoolingLengthCorrectionFactor,  !- Name",
			"    1.0693794,               !- Coefficient1 Constant",
			"    -0.0014951,              !- Coefficient2 x",
			"    2.56E-06,                !- Coefficient3 x**2",
			"    -0.1151104,              !- Coefficient4 y",
			"    0.0511169,               !- Coefficient5 y**2",
			"    -0.0004369,              !- Coefficient6 x*y",
			"    8,                       !- Minimum Value of x",
			"    175,                     !- Maximum Value of x",
			"    0.5,                     !- Minimum Value of y",
			"    1.5,                     !- Maximum Value of y",
			"    ,                        !- Minimum Curve Output",
			"    ,                        !- Maximum Curve Output",
			"    Temperature,             !- Input Unit Type for X",
			"    Temperature,             !- Input Unit Type for Y",
			"    Dimensionless;           !- Output Unit Type",

			"  Curve:Biquadratic,",
			"    VRF Piping Correction Factor for Length in Heating Mode,  !- Name",
			"    0.989916,                !- Coefficient1 Constant",
			"    0.001961,                !- Coefficient2 x",
			"    -.000036,                !- Coefficient3 x**2",
			"    0,                       !- Coefficient4 y",
			"    0,                       !- Coefficient5 y**2",
			"    0,                       !- Coefficient6 x*y",
			"    7,                       !- Minimum Value of x",
			"    106.5,                   !- Maximum Value of x",
			"    1,                       !- Minimum Value of y",
			"    1,                       !- Maximum Value of y",
			"    ,                        !- Minimum Curve Output",
			"    ,                        !- Maximum Curve Output",
			"    Distance,                !- Input Unit Type for X",
			"    Dimensionless,           !- Input Unit Type for Y",
			"    Dimensionless;           !- Output Unit Type",

			"  Curve:Biquadratic,",
			"    VRF Heat Recovery Cooling Capacity Modifier,  !- Name",
			"    0.9,                     !- Coefficient1 Constant",
			"    0,                       !- Coefficient2 x",
			"    0,                       !- Coefficient3 x**2",
			"    0,                       !- Coefficient4 y",
			"    0,                       !- Coefficient5 y**2",
			"    0,                       !- Coefficient6 x*y",
			"    -100,                    !- Minimum Value of x",
			"    100,                     !- Maximum Value of x",
			"    -100,                    !- Minimum Value of y",
			"    100,                     !- Maximum Value of y",
			"    ,                        !- Minimum Curve Output",
			"    ,                        !- Maximum Curve Output",
			"    Temperature,             !- Input Unit Type for X",
			"    Temperature,             !- Input Unit Type for Y",
			"    Dimensionless;           !- Output Unit Type",

			"  Curve:Biquadratic,",
			"    VRF Heat Recovery Cooling Energy Modifier,  !- Name",
			"    1.1,                     !- Coefficient1 Constant",
			"    0,                       !- Coefficient2 x",
			"    0,                       !- Coefficient3 x**2",
			"    0,                       !- Coefficient4 y",
			"    0,                       !- Coefficient5 y**2",
			"    0,                       !- Coefficient6 x*y",
			"    -100,                    !- Minimum Value of x",
			"    100,                     !- Maximum Value of x",
			"    -100,                    !- Minimum Value of y",
			"    100,                     !- Maximum Value of y",
			"    ,                        !- Minimum Curve Output",
			"    ,                        !- Maximum Curve Output",
			"    Temperature,             !- Input Unit Type for X",
			"    Temperature,             !- Input Unit Type for Y",
			"    Dimensionless;           !- Output Unit Type",

			"  Curve:Biquadratic,",
			"    VRF Heat Recovery Heating Capacity Modifier,  !- Name",
			"    0.9,                     !- Coefficient1 Constant",
			"    0,                       !- Coefficient2 x",
			"    0,                       !- Coefficient3 x**2",
			"    0,                       !- Coefficient4 y",
			"    0,                       !- Coefficient5 y**2",
			"    0,                       !- Coefficient6 x*y",
			"    -100,                    !- Minimum Value of x",
			"    100,                     !- Maximum Value of x",
			"    -100,                    !- Minimum Value of y",
			"    100,                     !- Maximum Value of y",
			"    ,                        !- Minimum Curve Output",
			"    ,                        !- Maximum Curve Output",
			"    Temperature,             !- Input Unit Type for X",
			"    Temperature,             !- Input Unit Type for Y",
			"    Dimensionless;           !- Output Unit Type",

			"  Curve:Biquadratic,",
			"    VRF Heat Recovery Heating Energy Modifier,  !- Name",
			"    1.1,                     !- Coefficient1 Constant",
			"    0,                       !- Coefficient2 x",
			"    0,                       !- Coefficient3 x**2",
			"    0,                       !- Coefficient4 y",
			"    0,                       !- Coefficient5 y**2",
			"    0,                       !- Coefficient6 x*y",
			"    -100,                    !- Minimum Value of x",
			"    100,                     !- Maximum Value of x",
			"    -100,                    !- Minimum Value of y",
			"    100,                     !- Maximum Value of y",
			"    ,                        !- Minimum Curve Output",
			"    ,                        !- Maximum Curve Output",
			"    Temperature,             !- Input Unit Type for X",
			"    Temperature,             !- Input Unit Type for Y",
			"    Dimensionless;           !- Output Unit Type",

		} );

		ASSERT_FALSE( process_idf( idf_objects ) );

		DataGlobals::BeginEnvrnFlag = true;
		DataSizing::CurZoneEqNum = 1;
		DataEnvironment::OutBaroPress = 101325; // sea level
		DataZoneEquipment::ZoneEquipInputsFilled = true; // denotes zone equipment has been read in
		DataEnvironment::StdRhoAir = PsyRhoAirFnPbTdbW( DataEnvironment::OutBaroPress, 20.0, 0.0 );
		DataGlobals::SysSizingCalc = true;
		DataGlobals::NumOfTimeStepInHour = 1;
		DataGlobals::MinutesPerTimeStep = 60;

		CurveManager::GetCurveInput(); // read curves
		HeatBalanceManager::GetZoneData( ErrorsFound ); // read zone data
		EXPECT_FALSE( ErrorsFound );

		DataZoneEquipment::GetZoneEquipmentData(); // read equipment list and connections
		HVACVariableRefrigerantFlow::MyEnvrnFlag = true;
		ZoneInletAirNode = GetVRFTUZoneInletAirNode( VRFTUNum ); // trigger GetVRFInput by calling a mining function
		OutsideAirNode = VRFTU( VRFTUNum ).VRFTUOAMixerOANodeNum; // outside air air inlet node num
		DataZoneEnergyDemands::ZoneSysEnergyDemand.allocate( 1 );
		DataZoneEnergyDemands::ZoneSysEnergyDemand( CurZoneNum ).RemainingOutputRequired = 0.0;  // No load 
		DataZoneEnergyDemands::ZoneSysEnergyDemand( CurZoneNum ).RemainingOutputReqToCoolSP = 0.0;  // No load 
		DataZoneEnergyDemands::ZoneSysEnergyDemand( CurZoneNum ).RemainingOutputReqToHeatSP = 0.0;  // No load 
		QZnReq = DataZoneEnergyDemands::ZoneSysEnergyDemand( CurZoneNum ).RemainingOutputRequired;  // No load 
		// Initialize terminal unit
		Schedule( VRFTU( VRFTUNum ).FanOpModeSchedPtr ).CurrentValue = 1.0; // set continuous fan operating mode
		InitVRF( VRFTUNum, ZoneNum, FirstHVACIteration, OnOffAirFlowRatio, QZnReq ); // Initialize all VRFTU related parameters
		ASSERT_EQ( VRFTU( VRFTUNum ).OpMode, DataHVACGlobals::ContFanCycCoil ); // continuous fan cycling coil operating mode
		// Set average OA flow rate when there in no load for cont. fan cyc. coil operating mode
		SetAverageAirFlow( VRFTUNum, PartLoadRatio, OnOffAirFlowRatio );
		AverageOAMassFlow = DataEnvironment::StdRhoAir * VRFTU( VRFTUNum ).NoCoolHeatOutAirVolFlow;
		EXPECT_EQ( AverageOAMassFlow, Node( OutsideAirNode ).MassFlowRate );

		// clean up
		ZoneSysEnergyDemand.deallocate();
	}

}
