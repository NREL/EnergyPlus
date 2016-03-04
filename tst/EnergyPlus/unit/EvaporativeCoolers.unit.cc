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

// EnergyPlus::Evaporative Cooler Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobalConstants.hh>
#include <EnergyPlus/EvaporativeCoolers.hh>
#include <EnergyPlus/Psychrometrics.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace EnergyPlus::CurveManager;
using namespace EnergyPlus::DataEnvironment;
using namespace EnergyPlus::Psychrometrics;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::DataAirSystems;
using namespace EnergyPlus::EvaporativeCoolers;
using EnergyPlus::DataGlobalConstants::iEvapCoolerInDirectRDDSpecial;

namespace EnergyPlus {

	TEST_F( EnergyPlusFixture, EvapCoolers_SecondaryAirOutletCondition ) {

		EvapCond.allocate( 1 );
		int const EvapCoolNum( 1 );
		EvapCond( EvapCoolNum ).SecInletEnthalpy = 42000.0;

		// set up arguments
		int OperatingMode( EvaporativeCoolers::None );
		Real64 AirMassFlowSec( 0.0 );
		Real64 const EDBTSec( 20.0 );
		Real64 const EWBTSec( 15.0 );
		Real64 const EHumRatSec( 0.0085 );
		Real64 QHXTotal( 0.0 );
		Real64 QHXLatent( 0.0 );

		// make the call for zero secondary air flow rate
		CalcSecondaryAirOutletCondition(
			EvapCoolNum,
			OperatingMode,
			AirMassFlowSec,
			EDBTSec,
			EWBTSec,
			EHumRatSec,
			QHXTotal,
			QHXLatent
			);

		// check outputs for evap cooler set off
		EXPECT_DOUBLE_EQ( EvapCond( EvapCoolNum ).SecOutletEnthalpy, EvapCond( EvapCoolNum ).SecInletEnthalpy );
		EXPECT_DOUBLE_EQ( 0.0, QHXLatent );

		// dry operating mode and non zero secondary air flow rate
		OperatingMode = EvaporativeCoolers::DryFull;
		AirMassFlowSec = 2.0;
		QHXTotal = 10206.410750000941;

		InitializePsychRoutines();

		// make the call for dry operating mode
		CalcSecondaryAirOutletCondition(
			EvapCoolNum,
			OperatingMode,
			AirMassFlowSec,
			EDBTSec,
			EWBTSec,
			EHumRatSec,
			QHXTotal,
			QHXLatent
			);

		// check outputs for dry operating condition
		EXPECT_DOUBLE_EQ( 25.0, EvapCond( EvapCoolNum ).SecOutletTemp );
		EXPECT_DOUBLE_EQ( 0.0, QHXLatent );


		// wet operating mode and non zero secondary air flow rate
		OperatingMode = EvaporativeCoolers::WetFull;
		AirMassFlowSec = 2.0;
		QHXTotal = 10206.410750000941;

		// make the call for wet operating condition
		CalcSecondaryAirOutletCondition(
			EvapCoolNum,
			OperatingMode,
			AirMassFlowSec,
			EDBTSec,
			EWBTSec,
			EHumRatSec,
			QHXTotal,
			QHXLatent
			);

		// check outputs for wet operating condition
		EXPECT_DOUBLE_EQ( 20.0, EvapCond( EvapCoolNum ).SecOutletTemp );
		EXPECT_DOUBLE_EQ( 47103.205375000471, EvapCond( EvapCoolNum ).SecOutletEnthalpy );
		EXPECT_DOUBLE_EQ( QHXTotal, QHXLatent );

		EvapCond.deallocate();

	}

	TEST_F( EnergyPlusFixture, EvapCoolers_IndEvapCoolerOutletTemp ) {

		int const EvapCoolNum( 1 );
		EvapCond.allocate( EvapCoolNum );

		OutBaroPress = 101325.0;
		EvapCond( EvapCoolNum ).InletMassFlowRate = 1.0;
		EvapCond( EvapCoolNum ).InletTemp = 24.0;
		EvapCond( EvapCoolNum ).InletHumRat = 0.013;
		EvapCond( EvapCoolNum ).DryCoilMaxEfficiency = 0.8;

		// set up arguments
		int DryOrWetOperatingMode( EvaporativeCoolers::DryFull );
		Real64 const AirMassFlowSec( 1.0 );
		Real64 const EDBTSec( 14.0 );
		Real64 const EWBTSec( 11.0 );
		Real64 const EHumRatSec( 0.0075 );

		// testing full capacity in dry operating mode
		CalcIndirectRDDEvapCoolerOutletTemp(
			EvapCoolNum,
			DryOrWetOperatingMode,
			AirMassFlowSec,
			EDBTSec,
			EWBTSec,
			EHumRatSec );

		EXPECT_DOUBLE_EQ( 16.0, EvapCond( EvapCoolNum ).OutletTemp );

		// testing full capacity in wet operating mode
		DryOrWetOperatingMode = EvaporativeCoolers::WetFull;
		EvapCond( EvapCoolNum ).WetCoilMaxEfficiency = 0.75;

		CalcIndirectRDDEvapCoolerOutletTemp(
			EvapCoolNum,
			DryOrWetOperatingMode,
			AirMassFlowSec,
			EDBTSec,
			EWBTSec,
			EHumRatSec );

		EXPECT_DOUBLE_EQ( 14.25, EvapCond( EvapCoolNum ).OutletTemp );

		EvapCond.deallocate();

	}

	TEST_F( EnergyPlusFixture, EvapCoolers_IndEvapCoolerPower ) {

		//using CurveManager::Quadratic;
		int const EvapCoolNum( 1 );
		int CurveNum( 1) ;
		int DryWetMode( EvaporativeCoolers::DryFull );
		Real64 FlowRatio( 1.0 );

		EvapCond.allocate( EvapCoolNum );
		EvapCond( EvapCoolNum ).IndirectFanPower = 200.0;
		EvapCond( EvapCoolNum ).IndirectRecircPumpPower = 100.0;

		// set up arguments
		EvapCond( EvapCoolNum ).FanPowerModifierCurveIndex = CurveNum;

		NumCurves = 1;
		PerfCurve.allocate( 1 );
		PerfCurve( CurveNum ).CurveType = Quadratic;
		PerfCurve( CurveNum ).ObjectType = CurveType_Quadratic;
		PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;
		PerfCurve( CurveNum ).Coeff1 = 0.0;
		PerfCurve( CurveNum ).Coeff2 = 1.0;
		PerfCurve( CurveNum ).Coeff3 = 0.0;
		PerfCurve( CurveNum ).Coeff4 = 0.0;
		PerfCurve( CurveNum ).Coeff5 = 0.0;
		PerfCurve( CurveNum ).Coeff6 = 0.0;
		PerfCurve( CurveNum ).Var1Min = 0.0;
		PerfCurve( CurveNum ).Var1Max = 1.0;
		PerfCurve( CurveNum ).Var2Min = 0;
		PerfCurve( CurveNum ).Var2Max = 0;

		// make the call for dry full load operating condition
		EvapCond( EvapCoolNum ).EvapCoolerPower = IndEvapCoolerPower(
			EvapCoolNum,
			DryWetMode,
			FlowRatio );

		// check outputs for dry full load operating condition
		EXPECT_EQ( 200.0, EvapCond( EvapCoolNum ).EvapCoolerPower );

		// set up arguments for wet modulated operating condition
		DryWetMode = WetModulated;
		FlowRatio = 0.5;
		EvapCond( EvapCoolNum ).PartLoadFract = 0.5;

		// make the call for wet modulated operating condition
		EvapCond( EvapCoolNum ).EvapCoolerPower = IndEvapCoolerPower(
			EvapCoolNum,
			DryWetMode,
			FlowRatio );

		// check outputs for wet modulated operating condition
		EXPECT_EQ( 150.0, EvapCond( EvapCoolNum ).EvapCoolerPower );

		EvapCond.deallocate();
		PerfCurve.deallocate();
	}

	TEST_F( EnergyPlusFixture, EvapCoolers_SizeIndEvapCoolerTest ) {

		int const EvapCoolNum( 1 );
		Real64 PrimaryAirDesignFlow( 0.0 );
		Real64 SecondaryAirDesignFlow( 0.0 );

		DataSizing::SysSizingRunDone = true;
		DataSizing::NumSysSizInput = 1;
		DataSizing::SysSizInput.allocate( 1 );
		DataSizing::SysSizInput( 1 ).AirLoopNum = 1;

		// allocate
		DataSizing::CurSysNum = 1;
		FinalSysSizing.allocate( CurSysNum );
		DataSizing::FinalSysSizing( 1 ).DesMainVolFlow = 1.0;
		DataSizing::FinalSysSizing( 1 ).DesOutAirVolFlow = 0.4;
		PrimaryAirSystem.allocate( CurSysNum );
		PrimaryAirSystem( CurSysNum ).Branch.allocate( 1 );
		PrimaryAirSystem( CurSysNum ).Branch( 1 ).Comp.allocate( 1 );
		DataAirSystems::PrimaryAirSystem( 1 ).Branch( 1 ).Comp( 1 ).Name = "INDRDD EVAP COOLER";

		// Set Primary Air Data
		PrimaryAirSystem( CurSysNum ).NumBranches = 1;
		PrimaryAirSystem( CurSysNum ).Branch( 1 ).TotalComponents = 1;

		std::string const idf_objects = delimited_string( {
			"	Version,8.3;",
			"	EvaporativeCooler:Indirect:ResearchSpecial,",
			"	IndRDD Evap Cooler,  !- Name",
			"	ALWAYS_ON,			 !- Availability Schedule Name",
			"	0.750,				 !- Cooler Wetbulb Design Effectiveness",
			"	,					 !- Wetbulb Effectiveness Flow Ratio Modifier Curve Name",
			"	,					 !- Cooler Drybulb Design Effectiveness",
			"	,					 !- Drybulb Effectiveness Flow Ratio Modifier Curve Name",
			"	30.0,				 !- Recirculating Water Pump Design Power { W }",
			"	,					 !- Water Pump Power Sizing Factor",
			"	,					 !- Water Pump Power Modifier Curve Name",
			"	autosize,			 !- Secondary Air Design Flow Rate { m3 / s }",
			"	1.2,				 !- Secondary Air Flow Sizing Factor",
			"	autosize,			 !- Secondary Air Fan Design Power",
			"	207.6,				 !- Secondary Air Fan Sizing Specific Power",
			"	,					 !- Secondary Fan Power Modifier Curve Name",
			"	PriAir Inlet Node,	 !- Primary Air Inlet Node Name",
			"	PriAir Outlet Node,	 !- Primary Air Outlet Node Name",
			"	autosize,			 !- Primary Air Design Air Flow Rate",
			"	0.90,				 !- Dewpoint Effectiveness Factor",
			"	SecAir Inlet Node,   !- Secondary Air Inlet Node Name",
			"	SecAir Outlet Node,  !- Secondary Air Outlet Node Name",
			"	PriAir Outlet Node,	 !- Sensor Node Name",
			"	,					 !- Relief Air Inlet Node Name",
			"	,					 !- Water Supply Storage Tank Name",
			"	0.0,				 !- Drift Loss Fraction",
			"	3;                   !- Blowdown Concentration Ratio",
		} );

		ASSERT_FALSE( process_idf( idf_objects ) );

		GetEvapInput();

		// Set Parameters for Evap Cooler on Main Air Loop System
		PrimaryAirSystem( CurSysNum ).Branch( 1 ).Comp( 1 ).Name = EvapCond( EvapCoolNum ).EvapCoolerName;
		EvapCond( EvapCoolNum ).VolFlowRate = DataSizing::AutoSize;
		EvapCond( EvapCoolNum ).IndirectVolFlowRate = DataSizing::AutoSize;
		FinalSysSizing( CurSysNum ).DesMainVolFlow = 1.0;
		FinalSysSizing( CurSysNum ).DesOutAirVolFlow = 0.2;
		PrimaryAirDesignFlow = FinalSysSizing( CurSysNum ).DesMainVolFlow;
		SecondaryAirDesignFlow = PrimaryAirDesignFlow * EvapCond( EvapCoolNum ).IndirectVolFlowScalingFactor;

		// Test Indirect Evaporative Cooler Primary/Secondary Air Design Flow Rate on Main Air Loop
		SizeEvapCooler( EvapCoolNum );
		EXPECT_EQ( PrimaryAirDesignFlow, EvapCond( EvapCoolNum ).VolFlowRate );
		EXPECT_EQ( SecondaryAirDesignFlow, EvapCond( EvapCoolNum ).IndirectVolFlowRate );

		// Set Parameters for Evap Cooler on OA System
		EvapCond( EvapCoolNum ).EvapCoolerName = "EvapCool On OA System",
		EvapCond( EvapCoolNum ).VolFlowRate = DataSizing::AutoSize;
		EvapCond( EvapCoolNum ).IndirectVolFlowRate = DataSizing::AutoSize;
		FinalSysSizing( CurSysNum ).DesMainVolFlow = 1.0;
		FinalSysSizing( CurSysNum ).DesOutAirVolFlow = 0.2;
		PrimaryAirDesignFlow = FinalSysSizing( CurSysNum ).DesOutAirVolFlow;
		SecondaryAirDesignFlow = max( PrimaryAirDesignFlow, 0.5 * FinalSysSizing( CurSysNum ).DesMainVolFlow );
		SecondaryAirDesignFlow = SecondaryAirDesignFlow * EvapCond( EvapCoolNum ).IndirectVolFlowScalingFactor;

		// Test Indirect Evaporative Cooler Primary/Secondary Air Design Flow Rate on OA System
		SizeEvapCooler( EvapCoolNum );
		EXPECT_EQ( 0.5, EvapCond( EvapCoolNum ).VolFlowRate );
		EXPECT_EQ( SecondaryAirDesignFlow, EvapCond( EvapCoolNum ).IndirectVolFlowRate );

		EvapCond.deallocate();
		PrimaryAirSystem.deallocate();
		FinalSysSizing.deallocate();
	}

	TEST_F( EnergyPlusFixture, EvapCoolers_SizeDirEvapCoolerTest ) {

		int const EvapCoolNum( 1 );
		Real64 PrimaryAirDesignFlow( 0.0 );
		Real64 RecirWaterPumpDesignPower( 0.0 );

		DataSizing::SysSizingRunDone = true;
		DataSizing::NumSysSizInput = 1;
		DataSizing::SysSizInput.allocate( 1 );
		DataSizing::SysSizInput( 1 ).AirLoopNum = 1;


		DataSizing::CurSysNum = 1;
		FinalSysSizing.allocate( CurSysNum );
		DataSizing::FinalSysSizing( 1 ).DesMainVolFlow = 1.0;
		DataSizing::FinalSysSizing( 1 ).DesOutAirVolFlow = 0.4;
		PrimaryAirSystem.allocate( CurSysNum );
		PrimaryAirSystem( CurSysNum ).Branch.allocate( 1 );
		PrimaryAirSystem( CurSysNum ).Branch( 1 ).Comp.allocate( 1 );
		DataAirSystems::PrimaryAirSystem( 1 ).Branch( 1 ).Comp( 1 ).Name = "DIRECTEVAPCOOLER";

		// Set Primary Air Data
		PrimaryAirSystem( CurSysNum ).NumBranches = 1;
		PrimaryAirSystem( CurSysNum ).Branch( 1 ).TotalComponents = 1;

		std::string const idf_objects = delimited_string( {
		"	Version,8.3;",
		"	EvaporativeCooler:Direct:ResearchSpecial,",
		"	DirectEvapCooler,    !- Name",
		"	ALWAYS_ON,			 !- Availability Schedule Name",
		"	0.7,				 !- Cooler Design Effectiveness",
		"	,					 !- Effectiveness Flow Ratio Modifier Curve Name",
		"	autosize,			 !- Primary Air Design Flow Rate",
		"	autosize,			 !- Recirculating Water Pump Power Consumption { W }",
		"	55.0,				 !- Water Pump Power Sizing Factor",
		"	,					 !- Water Pump Power Modifier Curve Name",
		"	Fan Outlet Node,     !- Air Inlet Node Name",
		"	Zone Inlet Node,	 !- Air Outlet Node Name",
		"	Zone Inlet Node,	 !- Sensor Node Name",
		"	,					 !- Water Supply Storage Tank Name",
		"	0.0,				 !- Drift Loss Fraction",
		"	3;                   !- Blowdown Concentration Ratio",
		} );

		ASSERT_FALSE( process_idf( idf_objects ) );

		GetEvapInput();
		// check autosized input fields from idf snippet read
		EXPECT_EQ( DataSizing::AutoSize, EvapCond( EvapCoolNum ).VolFlowRate );
		EXPECT_EQ( DataSizing::AutoSize, EvapCond( EvapCoolNum ).RecircPumpPower );

		PrimaryAirSystem( CurSysNum ).Branch( 1 ).Comp( 1 ).Name = EvapCond( EvapCoolNum ).EvapCoolerName;
		FinalSysSizing( CurSysNum ).DesMainVolFlow = 0.50;
		PrimaryAirDesignFlow = FinalSysSizing( CurSysNum ).DesMainVolFlow;
		RecirWaterPumpDesignPower = PrimaryAirDesignFlow * EvapCond( EvapCoolNum ).RecircPumpSizingFactor;

		// Test Direct Evaporative Cooler Primary Air Design Flow Rate sizing
		SizeEvapCooler( 1 );
		EXPECT_EQ( PrimaryAirDesignFlow, EvapCond( EvapCoolNum ).VolFlowRate );

		EXPECT_EQ( RecirWaterPumpDesignPower, EvapCond( EvapCoolNum ).RecircPumpPower );

		EvapCond.deallocate();
		PrimaryAirSystem.deallocate();
		FinalSysSizing.deallocate();
	}

	TEST_F( EnergyPlusFixture, EvaporativeCoolers_CalcSecondaryAirOutletCondition )
	{

		EvaporativeCoolers::EvapCond.allocate( 1 );
		int const EvapCoolNum( 1 );
		EvaporativeCoolers::EvapCond( EvapCoolNum ).SecInletEnthalpy = 42000.0;

		// set up arguments
		int OperatingMode( EvaporativeCoolers::None );
		Real64 AirMassFlowSec( 0.0 );
		Real64 const EDBTSec( 20.0 );
		Real64 const EWBTSec( 15.0 );
		Real64 const EHumRatSec( 0.0085 );
		Real64 QHXTotal( 0.0 );
		Real64 QHXLatent( 0.0 );

		// make the call for zero secondary air flow rate
		EvaporativeCoolers::CalcSecondaryAirOutletCondition(
			EvapCoolNum,
			OperatingMode,
			AirMassFlowSec,
			EDBTSec,
			EWBTSec,
			EHumRatSec,
			QHXTotal,
			QHXLatent
		);

		// check outputs for evap cooler set off
		EXPECT_DOUBLE_EQ( EvaporativeCoolers::EvapCond( EvapCoolNum ).SecOutletEnthalpy, EvaporativeCoolers::EvapCond( EvapCoolNum ).SecInletEnthalpy );
		EXPECT_DOUBLE_EQ( 0.0, QHXLatent );


		// dry operating mode and non zero secondary air flow rate
		OperatingMode = EvaporativeCoolers::DryFull;
		AirMassFlowSec = 2.0;
		QHXTotal = 10206.410750000941;

		InitializePsychRoutines();

		// make the call for dry operating mode
		EvaporativeCoolers::CalcSecondaryAirOutletCondition(
			EvapCoolNum,
			OperatingMode,
			AirMassFlowSec,
			EDBTSec,
			EWBTSec,
			EHumRatSec,
			QHXTotal,
			QHXLatent
			);

		// check outputs for dry operating condition
		EXPECT_DOUBLE_EQ( 25.0, EvaporativeCoolers::EvapCond( EvapCoolNum ).SecOutletTemp );
		EXPECT_DOUBLE_EQ( 0.0, QHXLatent );


		// wet operating mode and non zero secondary air flow rate
		OperatingMode = EvaporativeCoolers::WetFull;
		AirMassFlowSec = 2.0;
		QHXTotal = 10206.410750000941;

		// make the call for wet operating condition
		EvaporativeCoolers::CalcSecondaryAirOutletCondition(
			EvapCoolNum,
			OperatingMode,
			AirMassFlowSec,
			EDBTSec,
			EWBTSec,
			EHumRatSec,
			QHXTotal,
			QHXLatent
			);

		// check outputs for wet operating condition
		EXPECT_DOUBLE_EQ( 20.0, EvaporativeCoolers::EvapCond( EvapCoolNum ).SecOutletTemp );
		EXPECT_DOUBLE_EQ( 47103.205375000471, EvaporativeCoolers::EvapCond( EvapCoolNum ).SecOutletEnthalpy );
		EXPECT_DOUBLE_EQ( QHXTotal, QHXLatent );

		EvaporativeCoolers::EvapCond.deallocate();

	}

	TEST_F( EnergyPlusFixture, EvaporativeCoolers_CalcIndirectRDDEvapCoolerOutletTemp )
	{

		OutBaroPress = 101325.0;
		EvaporativeCoolers::EvapCond.allocate( 1 );
		int const EvapCoolNum( 1 );
		EvaporativeCoolers::EvapCond( EvapCoolNum ).InletMassFlowRate = 1.0;
		EvaporativeCoolers::EvapCond( EvapCoolNum ).InletTemp = 24.0;
		EvaporativeCoolers::EvapCond( EvapCoolNum ).InletHumRat = 0.013;
		EvaporativeCoolers::EvapCond( EvapCoolNum ).DryCoilMaxEfficiency = 0.8;

		// set up arguments
		int DryOrWetOperatingMode( EvaporativeCoolers::DryFull );
		Real64 const AirMassFlowSec( 1.0 );
		Real64 const EDBTSec( 14.0 );
		Real64 const EWBTSec( 11.0 );
		Real64 const EHumRatSec( 0.0075 );

		// testing full capacity in dry operating mode
		EvaporativeCoolers::CalcIndirectRDDEvapCoolerOutletTemp(
		EvapCoolNum,
		DryOrWetOperatingMode,
		AirMassFlowSec,
		EDBTSec,
		EWBTSec,
		EHumRatSec );

		EXPECT_DOUBLE_EQ( 16.0, EvaporativeCoolers::EvapCond( EvapCoolNum ).OutletTemp );

		// testing full capacity in wet operating mode
		DryOrWetOperatingMode = EvaporativeCoolers::WetFull;
		EvaporativeCoolers::EvapCond( EvapCoolNum ).WetCoilMaxEfficiency = 0.75;

		EvaporativeCoolers::CalcIndirectRDDEvapCoolerOutletTemp(
			EvapCoolNum,
			DryOrWetOperatingMode,
			AirMassFlowSec,
			EDBTSec,
			EWBTSec,
			EHumRatSec );

		EXPECT_DOUBLE_EQ( 14.25, EvaporativeCoolers::EvapCond( EvapCoolNum ).OutletTemp );

		EvaporativeCoolers::EvapCond.deallocate();

	}

	TEST_F( EnergyPlusFixture, EvaporativeCoolers_IndEvapCoolerPower )
	{

		ShowMessage( "Begin Test: EvaporativeCoolers, IndEvapCoolerPower" );

		using CurveManager::Quadratic;

		int CurveNum;

		EvaporativeCoolers::EvapCond.allocate( 1 );
		int const EvapCoolNum( 1 );
		EvaporativeCoolers::EvapCond( EvapCoolNum ).IndirectFanPower = 200.0;
		EvaporativeCoolers::EvapCond( EvapCoolNum ).IndirectRecircPumpPower = 100.0;

		// set up arguments
		int DryWetMode( EvaporativeCoolers::DryFull );
		Real64 FlowRatio( 1.0 );

		CurveNum = 1;
		EvaporativeCoolers::EvapCond( EvapCoolNum ).FanPowerModifierCurveIndex = CurveNum;

		NumCurves = 1;
		PerfCurve.allocate( 1 );
		PerfCurve( CurveNum ).CurveType = Quadratic;
		PerfCurve( CurveNum ).ObjectType = CurveType_Quadratic;
		PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;
		PerfCurve( CurveNum ).Coeff1 = 0.0;
		PerfCurve( CurveNum ).Coeff2 = 1.0;
		PerfCurve( CurveNum ).Coeff3 = 0.0;
		PerfCurve( CurveNum ).Coeff4 = 0.0;
		PerfCurve( CurveNum ).Coeff5 = 0.0;
		PerfCurve( CurveNum ).Coeff6 = 0.0;
		PerfCurve( CurveNum ).Var1Min = 0.0;
		PerfCurve( CurveNum ).Var1Max = 1.0;
		PerfCurve( CurveNum ).Var2Min = 0;
		PerfCurve( CurveNum ).Var2Max = 0;

		// make the call for dry full load operating condition
		EvaporativeCoolers::EvapCond( EvapCoolNum ).EvapCoolerPower = EvaporativeCoolers::IndEvapCoolerPower(
			EvapCoolNum,
			DryWetMode,
			FlowRatio );

		// check outputs for dry full load operating condition
		EXPECT_EQ( 200.0, EvaporativeCoolers::EvapCond( EvapCoolNum ).EvapCoolerPower );

		// set up arguments for wet modulated operating condition
		DryWetMode = EvaporativeCoolers::WetModulated;
		FlowRatio = 0.5;
		EvaporativeCoolers::EvapCond( EvapCoolNum ).PartLoadFract = 0.5;

		// make the call for wet modulated operating condition
		EvaporativeCoolers::EvapCond( EvapCoolNum ).EvapCoolerPower = EvaporativeCoolers::IndEvapCoolerPower(
		EvapCoolNum,
		DryWetMode,
		FlowRatio );

		// check outputs for wet modulated operating condition
		EXPECT_EQ( 150.0, EvaporativeCoolers::EvapCond( EvapCoolNum ).EvapCoolerPower );

		EvaporativeCoolers::EvapCond.deallocate();
		PerfCurve.deallocate();
	}

	TEST_F( EnergyPlusFixture, EvaporativeCoolers_SizeEvapCooler )
	{

		// one-time setup of evap cooler instance
		int const EvapCoolNum( 1 );
		EvaporativeCoolers::EvapCond.allocate( EvapCoolNum );
		auto & thisEvapCooler = EvaporativeCoolers::EvapCond( EvapCoolNum );

		// set up sizing stuff
		DataSizing::SysSizingRunDone = true;
		DataSizing::ZoneSizingRunDone = false;
		DataSizing::CurSysNum = 1;
		DataSizing::NumSysSizInput = 1;
		DataSizing::SysSizInput.allocate( 1 );
		DataSizing::SysSizInput( 1 ).AirLoopNum = 1;
		DataAirSystems::PrimaryAirSystem.allocate( 1 );
		DataAirSystems::PrimaryAirSystem( 1 ).NumBranches = 1;
		DataAirSystems::PrimaryAirSystem( 1 ).Branch.allocate( 1 );
		DataAirSystems::PrimaryAirSystem( 1 ).Branch( 1 ).TotalComponents = 1;
		DataAirSystems::PrimaryAirSystem( 1 ).Branch( 1 ).Comp.allocate( 1 );
		DataAirSystems::PrimaryAirSystem( 1 ).Branch( 1 ).Comp( 1 ).Name = "MyEvapCooler";
		thisEvapCooler.EvapCoolerName = "MyEvapCooler";
		DataSizing::FinalSysSizing.allocate(1);
		DataSizing::FinalSysSizing( 1 ).DesMainVolFlow = 1.0;
		DataSizing::FinalSysSizing( 1 ).DesOutAirVolFlow = 0.4;

		// set up the structure to size the flow rates for an RDDSpecial
		thisEvapCooler.EvapCoolerType = DataGlobalConstants::iEvapCoolerInDirectRDDSpecial;
		thisEvapCooler.VolFlowRate = DataSizing::AutoSize;
		thisEvapCooler.PadArea = 0.0;
		thisEvapCooler.PadDepth = 0.0;
		thisEvapCooler.IndirectPadArea = 0.0;
		thisEvapCooler.IndirectPadDepth = 0.0;
		thisEvapCooler.IndirectVolFlowRate = DataSizing::AutoSize;
		thisEvapCooler.IndirectVolFlowScalingFactor = 0.3;

		// make the call for sizing the flow rates
		EvaporativeCoolers::SizeEvapCooler(EvapCoolNum);
		EXPECT_NEAR(0.3, thisEvapCooler.IndirectVolFlowRate, 0.0001);
		EXPECT_NEAR(1.0, thisEvapCooler.VolFlowRate, 0.0001);

		// now let's try to size some of the pad properties
		thisEvapCooler.EvapCoolerType = DataGlobalConstants::iEvapCoolerDirectCELDEKPAD;
		thisEvapCooler.VolFlowRate = 1.0;
		thisEvapCooler.PadArea = DataSizing::AutoSize;
		thisEvapCooler.PadDepth = DataSizing::AutoSize;
		thisEvapCooler.IndirectPadArea = 0.0;
		thisEvapCooler.IndirectPadDepth = 0.0;
		thisEvapCooler.IndirectVolFlowRate = 1.0;

		// make the call for sizing the pad properties
		EvaporativeCoolers::SizeEvapCooler(EvapCoolNum);
		EXPECT_NEAR(0.333333, thisEvapCooler.PadArea, 0.0001);
		EXPECT_NEAR(0.17382, thisEvapCooler.PadDepth, 0.0001);

		// now let's try 'not' finding it on the air loop; thus it is in the OA path
		DataAirSystems::PrimaryAirSystem( 1 ).Branch( 1 ).Comp( 1 ).Name = "NOT-MyEvapCooler";

		// set up the structure to size the flow rates for an indirect celdekpad
		thisEvapCooler.EvapCoolerType = DataGlobalConstants::iEvapCoolerInDirectCELDEKPAD;
		thisEvapCooler.VolFlowRate = DataSizing::AutoSize;
		thisEvapCooler.PadArea = 0.0;
		thisEvapCooler.PadDepth = 0.0;
		thisEvapCooler.IndirectPadArea = 0.0;
		thisEvapCooler.IndirectPadDepth = 0.0;
		thisEvapCooler.IndirectVolFlowRate = DataSizing::AutoSize;
		thisEvapCooler.IndirectVolFlowScalingFactor = 0.3;

		// make the call for sizing the flow rates
		EvaporativeCoolers::SizeEvapCooler(EvapCoolNum);
		EXPECT_NEAR(0.5, thisEvapCooler.IndirectVolFlowRate, 0.0001);
		EXPECT_NEAR(0.5, thisEvapCooler.VolFlowRate, 0.0001);

		// clean up
		EvaporativeCoolers::EvapCond.deallocate();
		DataSizing::FinalSysSizing.deallocate();
		DataAirSystems::PrimaryAirSystem.deallocate();
		DataSizing::SysSizInput.deallocate();

	}

	TEST_F( EnergyPlusFixture, DefaultAutosizeIndEvapCoolerTest ) {

		int const EvapCoolNum( 1 );
		Real64 PrimaryAirDesignFlow( 0.0 );
		Real64 SecondaryAirDesignFlow( 0.0 );
		Real64 SecondaryFanPower( 0.0 );
		Real64 RecirculatingWaterPumpPower( 0.0 );

		DataSizing::SysSizingRunDone = true;
		DataSizing::NumSysSizInput = 1;
		DataSizing::SysSizInput.allocate( 1 );
		DataSizing::SysSizInput( 1 ).AirLoopNum = 1;

		// allocate
		DataSizing::CurSysNum = 1;
		FinalSysSizing.allocate( CurSysNum );
		DataSizing::FinalSysSizing( 1 ).DesMainVolFlow = 1.0;
		DataSizing::FinalSysSizing( 1 ).DesOutAirVolFlow = 0.4;
		PrimaryAirSystem.allocate( CurSysNum );
		PrimaryAirSystem( CurSysNum ).Branch.allocate( 1 );
		PrimaryAirSystem( CurSysNum ).Branch( 1 ).Comp.allocate( 1 );
		DataAirSystems::PrimaryAirSystem( 1 ).Branch( 1 ).Comp( 1 ).Name = "INDRDD EVAP COOLER";

		// Set Primary Air Data
		PrimaryAirSystem( CurSysNum ).NumBranches = 1;
		PrimaryAirSystem( CurSysNum ).Branch( 1 ).TotalComponents = 1;

		std::string const idf_objects = delimited_string( {
			"	Version,8.4;",
			"	EvaporativeCooler:Indirect:ResearchSpecial,",
			"	IndRDD Evap Cooler,  !- Name",
			"	ALWAYS_ON,			 !- Availability Schedule Name",
			"	0.750,				 !- Cooler Wetbulb Design Effectiveness",
			"	,					 !- Wetbulb Effectiveness Flow Ratio Modifier Curve Name",
			"	,					 !- Cooler Drybulb Design Effectiveness",
			"	,					 !- Drybulb Effectiveness Flow Ratio Modifier Curve Name",
			"	,   				 !- Recirculating Water Pump Design Power { W }",
			"	,					 !- Water Pump Power Sizing Factor",
			"	,					 !- Water Pump Power Modifier Curve Name",
			"	,        			 !- Secondary Air Design Flow Rate { m3 / s }",
			"	1.2,				 !- Secondary Air Flow Sizing Factor",
			"	,        			 !- Secondary Air Fan Design Power",
			"	207.6,				 !- Secondary Air Fan Sizing Specific Power",
			"	,					 !- Secondary Fan Power Modifier Curve Name",
			"	PriAir Inlet Node,	 !- Primary Air Inlet Node Name",
			"	PriAir Outlet Node,	 !- Primary Air Outlet Node Name",
			"	,       			 !- Primary Air Design Air Flow Rate",
			"	0.90,				 !- Dewpoint Effectiveness Factor",
			"	SecAir Inlet Node,   !- Secondary Air Inlet Node Name",
			"	SecAir Outlet Node,  !- Secondary Air Outlet Node Name",
			"	PriAir Outlet Node,	 !- Sensor Node Name",
			"	,					 !- Relief Air Inlet Node Name",
			"	,					 !- Water Supply Storage Tank Name",
			"	0.0,				 !- Drift Loss Fraction",
			"	3;                   !- Blowdown Concentration Ratio",
		} );

		ASSERT_FALSE( process_idf( idf_objects ) );

		GetEvapInput();

		// check blank autosizable input fields default to autosize
		EXPECT_EQ( DataSizing::AutoSize, EvapCond( EvapCoolNum ).VolFlowRate );
		EXPECT_EQ( DataSizing::AutoSize, EvapCond( EvapCoolNum ).IndirectVolFlowRate );
		EXPECT_EQ( DataSizing::AutoSize, EvapCond( EvapCoolNum ).IndirectFanPower );
		EXPECT_EQ( DataSizing::AutoSize, EvapCond( EvapCoolNum ).IndirectRecircPumpPower );

		// Set Parameters for Evap Cooler on Main Air Loop System
		PrimaryAirSystem( CurSysNum ).Branch( 1 ).Comp( 1 ).Name = EvapCond( EvapCoolNum ).EvapCoolerName;
		FinalSysSizing( CurSysNum ).DesMainVolFlow = 1.0;
		FinalSysSizing( CurSysNum ).DesOutAirVolFlow = 0.2;
		PrimaryAirDesignFlow = FinalSysSizing( CurSysNum ).DesMainVolFlow;
		SecondaryAirDesignFlow = PrimaryAirDesignFlow * EvapCond( EvapCoolNum ).IndirectVolFlowScalingFactor;

		// Test Indirect Evaporative Cooler Primary/Secondary Air Design Flow Rate on Main Air Loop
		SizeEvapCooler( EvapCoolNum );
		EXPECT_EQ( PrimaryAirDesignFlow, EvapCond( EvapCoolNum ).VolFlowRate );
		EXPECT_EQ( SecondaryAirDesignFlow, EvapCond( EvapCoolNum ).IndirectVolFlowRate );
		// Test Secondary Fan Power and reciculating water pump power
		SecondaryFanPower = SecondaryAirDesignFlow * EvapCond( EvapCoolNum ).FanSizingSpecificPower;
		RecirculatingWaterPumpPower = SecondaryAirDesignFlow * EvapCond( EvapCoolNum ).RecircPumpSizingFactor;

		//SizeEvapCooler( EvapCoolNum );
		EXPECT_EQ( SecondaryFanPower, EvapCond( EvapCoolNum ).IndirectFanPower );
		EXPECT_EQ( RecirculatingWaterPumpPower, EvapCond( EvapCoolNum ).IndirectRecircPumpPower );

		EvapCond.deallocate();
		PrimaryAirSystem.deallocate();
		FinalSysSizing.deallocate();
	}

	TEST_F( EnergyPlusFixture, DefaultAutosizeDirEvapCoolerTest ) {

		int const EvapCoolNum( 1 );
		Real64 PrimaryAirDesignFlow( 0.0 );
		Real64 RecirWaterPumpDesignPower( 0.0 );

		DataSizing::SysSizingRunDone = true;
		DataSizing::NumSysSizInput = 1;
		DataSizing::SysSizInput.allocate( 1 );
		DataSizing::SysSizInput( 1 ).AirLoopNum = 1;

		DataSizing::CurSysNum = 1;
		FinalSysSizing.allocate( CurSysNum );
		DataSizing::FinalSysSizing( 1 ).DesMainVolFlow = 1.0;
		DataSizing::FinalSysSizing( 1 ).DesOutAirVolFlow = 0.4;
		PrimaryAirSystem.allocate( CurSysNum );
		PrimaryAirSystem( CurSysNum ).Branch.allocate( 1 );
		PrimaryAirSystem( CurSysNum ).Branch( 1 ).Comp.allocate( 1 );
		DataAirSystems::PrimaryAirSystem( 1 ).Branch( 1 ).Comp( 1 ).Name = "DIRECTEVAPCOOLER";

		// Set Primary Air Data
		PrimaryAirSystem( CurSysNum ).NumBranches = 1;
		PrimaryAirSystem( CurSysNum ).Branch( 1 ).TotalComponents = 1;

		std::string const idf_objects = delimited_string( {
			"	Version,8.4;",
			"	EvaporativeCooler:Direct:ResearchSpecial,",
			"	DirectEvapCooler,    !- Name",
			"	ALWAYS_ON,			 !- Availability Schedule Name",
			"	0.7,				 !- Cooler Design Effectiveness",
			"	,					 !- Effectiveness Flow Ratio Modifier Curve Name",
			"	,          			 !- Primary Air Design Flow Rate",
			"	,               	 !- Recirculating Water Pump Power Consumption { W }",
			"	55.0,				 !- Water Pump Power Sizing Factor",
			"	,					 !- Water Pump Power Modifier Curve Name",
			"	Fan Outlet Node,     !- Air Inlet Node Name",
			"	Zone Inlet Node,	 !- Air Outlet Node Name",
			"	Zone Inlet Node,	 !- Sensor Node Name",
			"	,					 !- Water Supply Storage Tank Name",
			"	0.0,				 !- Drift Loss Fraction",
			"	3;                   !- Blowdown Concentration Ratio",
		} );

		ASSERT_FALSE( process_idf( idf_objects ) );

		GetEvapInput();
		// check blank autosizable input fields default to autosize
		EXPECT_EQ( DataSizing::AutoSize, EvapCond( EvapCoolNum ).VolFlowRate );
		EXPECT_EQ( DataSizing::AutoSize, EvapCond( EvapCoolNum ).RecircPumpPower );

		// do local sizing calculations
		PrimaryAirSystem( CurSysNum ).Branch( 1 ).Comp( 1 ).Name = EvapCond( EvapCoolNum ).EvapCoolerName;
		FinalSysSizing( CurSysNum ).DesMainVolFlow = 0.50;
		PrimaryAirDesignFlow = FinalSysSizing( CurSysNum ).DesMainVolFlow;
		RecirWaterPumpDesignPower = PrimaryAirDesignFlow * EvapCond( EvapCoolNum ).RecircPumpSizingFactor;

		// Test Direct Evaporative Cooler Primary Air Design Flow Rate sizing
		SizeEvapCooler( 1 );
		EXPECT_EQ( PrimaryAirDesignFlow, EvapCond( EvapCoolNum ).VolFlowRate );
		EXPECT_EQ( RecirWaterPumpDesignPower, EvapCond( EvapCoolNum ).RecircPumpPower );

		EvapCond.deallocate();
		PrimaryAirSystem.deallocate();
		FinalSysSizing.deallocate();
	}

}
