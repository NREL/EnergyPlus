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

// EnergyPlus::Standalone ERV Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

#include "Fixtures/EnergyPlusFixture.hh"
#include "Fixtures/SQLiteFixture.hh"

// EnergyPlus Headers
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/HVACFan.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ReportCoilSelection.hh>
#include <EnergyPlus/ReportSizingManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/Plant/Loop.hh>
#include <EnergyPlus/Plant/LoopSide.hh>

using namespace EnergyPlus;
using namespace ObjexxFCL;

TEST_F( EnergyPlusFixture, ReportCoilSelection_ChWCoil )
{
	std::string coil1Name ( "Coil 1" ); // user-defined name of the coil
	std::string coil1Type ( "Coil:Cooling:Water" ); // idf input object class name of coil
	int chWInletNodeNum = 9;
	int chWOutletNodeNum = 15;

	EnergyPlus::DataPlant::TotNumLoops = 1;
	DataPlant::PlantLoop.allocate( 1 );
	DataPlant::PlantLoop( 1 ).Name = "Chilled Water Loop";
	DataPlant::PlantLoop( 1 ).FluidName = "Water";
	DataPlant::PlantLoop( 1 ).FluidIndex = 1;
	DataPlant::PlantLoop( 1 ).MaxMassFlowRate = 0.1;
	DataPlant::PlantLoop( 1 ).LoopSide.allocate( 2 );
	DataPlant::PlantLoop( 1 ).LoopSide( 1 ).Branch.allocate( 1 );
	DataPlant::PlantLoop( 1 ).LoopSide( 1 ).TotalBranches = 1;
	DataPlant::PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp.allocate( 1 );
	DataPlant::PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).TotalComponents = 1;
	DataPlant::PlantLoop( 1 ).LoopSide( 2 ).Branch.allocate( 1 );
	DataPlant::PlantLoop( 1 ).LoopSide( 2 ).TotalBranches = 1;
	DataPlant::PlantLoop( 1 ).LoopSide( 2 ).Branch( 1 ).Comp.allocate( 1 );
	DataPlant::PlantLoop( 1 ).LoopSide( 2 ).Branch( 1 ).TotalComponents = 1;

	DataPlant::PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = 0;
	DataPlant::PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumOut = 0;
	DataPlant::PlantLoop( 1 ).LoopSide( 2 ).Branch( 1 ).Comp( 1 ).NodeNumIn = chWInletNodeNum;
	DataPlant::PlantLoop( 1 ).LoopSide( 2 ).Branch( 1 ).Comp( 1 ).NodeNumOut = chWOutletNodeNum;

	Real64 airVdot ( 0.052 ); // air flow rate in m3/s
	bool isAutoSized ( false ); // true if autosized
	coilSelectionReportObj->setCoilAirFlow( coil1Name, coil1Type, airVdot, isAutoSized );
	auto & c1 ( coilSelectionReportObj->coilSelectionDataObjs[ 0 ] );
	EXPECT_EQ( coil1Name, c1->coilName_ );
	EXPECT_EQ( coil1Type, c1->coilObjName );
	EXPECT_EQ( airVdot, c1->coilDesVolFlow );
	EXPECT_EQ( isAutoSized, c1->volFlowIsAutosized );

	int loopNum = 1;
	Real64 waterVdot = 0.05;
	// First with no plant sizing objects defined
	isAutoSized = false; // true if autosized
	coilSelectionReportObj->setCoilWaterFlowNodeNums( coil1Name, coil1Type, waterVdot,  isAutoSized, chWInletNodeNum, chWOutletNodeNum, loopNum );
	EXPECT_EQ( -999, c1->pltSizNum );
	EXPECT_EQ( loopNum, c1->waterLoopNum );
	EXPECT_EQ( DataPlant::PlantLoop( 1 ).Name, c1->plantLoopName );
	EXPECT_EQ( -999, c1->rhoFluid );
	EXPECT_EQ( -999, c1->cpFluid );
	EXPECT_EQ( -999, c1->coilDesWaterMassFlow );
	EXPECT_EQ( "No", c1->coilWaterFlowAutoMsg );

	// Exercise report writing with mostly defaults
	coilSelectionReportObj->finishCoilSummaryReportTable();

	// Use the other form for coil 2
	std::string coil2Name ( "Coil 2" ); // user-defined name of the coil
	std::string coil2Type ( "Coil:Cooling:Water" ); // idf input object class name of coil
	int pltSizNum = -999;
	coilSelectionReportObj->setCoilWaterFlowPltSizNum( coil2Name, coil2Type, waterVdot,  isAutoSized, pltSizNum, loopNum );
	auto & c2 ( coilSelectionReportObj->coilSelectionDataObjs[ 1 ] );
	EXPECT_EQ( -999, c2->pltSizNum );
	EXPECT_EQ( loopNum, c2->waterLoopNum );
	EXPECT_EQ( DataPlant::PlantLoop( 1 ).Name, c2->plantLoopName );
	EXPECT_EQ( -999, c2->rhoFluid );
	EXPECT_EQ( -999, c2->cpFluid );
	EXPECT_EQ( -999, c2->coilDesWaterMassFlow );
	EXPECT_EQ( "No", c2->coilWaterFlowAutoMsg );

	// Now add a plant sizing object
	DataSizing::NumPltSizInput = 1;
	DataSizing::PlantSizData.allocate( 1 );
	DataSizing::PlantSizData( 1 ).PlantLoopName = "Chilled Water Loop";
	isAutoSized = true; // true if autosized
	coilSelectionReportObj->setCoilWaterFlowNodeNums( coil1Name, coil1Type, waterVdot,  isAutoSized, chWInletNodeNum, chWOutletNodeNum, loopNum );
	auto & c1b ( coilSelectionReportObj->coilSelectionDataObjs[ 0 ] );
	EXPECT_EQ( 1, c1b->pltSizNum );
	EXPECT_EQ( loopNum, c1b->waterLoopNum );
	EXPECT_EQ( DataPlant::PlantLoop( 1 ).Name, c1b->plantLoopName );
	EXPECT_NEAR( 999.9, c1b->rhoFluid, 0.1 );
	EXPECT_NEAR( 4197.9, c1b->cpFluid, 0.1 );
	Real64 expFlow = waterVdot * c1b->rhoFluid;
	EXPECT_NEAR( expFlow, c1b->coilDesWaterMassFlow, 0.01 );
	EXPECT_EQ( "Yes", c1b->coilWaterFlowAutoMsg );

	Real64 uA = 1000.00;
	Real64 sizingCap = 500.0;
	int curSysNum = 1;
	int curZoneEqNum = 0;
	isAutoSized = true; // true if autosized
	DataAirSystems::PrimaryAirSystem.allocate( 1 );
	DataAirLoop::AirToZoneNodeInfo.allocate( 1 );
	DataAirLoop::AirToZoneNodeInfo( 1 ).NumZonesHeated = 2;
	DataAirLoop::AirToZoneNodeInfo( 1 ).HeatCtrlZoneNums.allocate( DataAirLoop::AirToZoneNodeInfo(1).NumZonesHeated );
	DataAirLoop::AirToZoneNodeInfo( 1 ).HeatCtrlZoneNums( 1 ) = 2;
	DataAirLoop::AirToZoneNodeInfo( 1 ).HeatCtrlZoneNums( 2 ) = 3;
	DataGlobals::NumOfZones = 3;
	DataHeatBalance::Zone.allocate( DataGlobals::NumOfZones );
	DataHeatBalance::Zone( 1 ).Name = "Zone 1";
	DataHeatBalance::Zone( 2 ).Name = "Zone 2";
	DataHeatBalance::Zone( 3 ).Name = "Zone 3";

	// This triggers doAirLoopSetUp
	coilSelectionReportObj->setCoilUA( coil2Name, coil2Type, uA, sizingCap, isAutoSized, curSysNum, curZoneEqNum );
	EXPECT_EQ( uA, c2->coilUA );
	EXPECT_EQ( sizingCap, c2->coilTotCapAtPeak );
	EXPECT_EQ( curSysNum, c2->airloopNum );
	EXPECT_EQ( curZoneEqNum, c2->zoneEqNum );

	// This coil serves zones 2 and 3 - rmLatentAtPeak is summed for all applicable zones
	Real64 zoneCoolingLatentLoad = 1000.0;
	int zoneNum = 1;
	coilSelectionReportObj->setZoneLatentLoadCoolingIdealPeak( zoneNum, zoneCoolingLatentLoad );
	EXPECT_EQ( 0.0, c2->rmLatentAtPeak );
	zoneNum = 2;
	coilSelectionReportObj->setZoneLatentLoadCoolingIdealPeak( zoneNum, zoneCoolingLatentLoad );
	EXPECT_EQ( 1000.0, c2->rmLatentAtPeak );
	zoneNum = 3;
	coilSelectionReportObj->setZoneLatentLoadCoolingIdealPeak( zoneNum, zoneCoolingLatentLoad );
	EXPECT_EQ( 2000.0, c2->rmLatentAtPeak );

	// Add a heating coil
	std::string coil3Name ( "Coil 3" ); // user-defined name of the coil
	std::string coil3Type ( "Coil:Heating:Electric" ); // idf input object class name of coil
	uA = -999.0;
	sizingCap = 500.0;
	curSysNum = 1;
	curZoneEqNum = 0;
	isAutoSized = false; // true if autosized
	// This triggers doAirLoopSetUp
	coilSelectionReportObj->setCoilUA( coil3Name, coil3Type, uA, sizingCap, isAutoSized, curSysNum, curZoneEqNum );
	auto & c3 ( coilSelectionReportObj->coilSelectionDataObjs[ 2 ] );
	EXPECT_EQ( uA, c3->coilUA );
	EXPECT_EQ( sizingCap, c3->coilTotCapAtPeak );
	EXPECT_EQ( curSysNum, c3->airloopNum );
	EXPECT_EQ( curZoneEqNum, c3->zoneEqNum );

	// This coil serves zones 2 and 3 - rmLatentAtPeak is summed for all applicable zones
	Real64 zoneHeatingLatentLoad = 100.0;
	zoneNum = 1;
	coilSelectionReportObj->setZoneLatentLoadHeatingIdealPeak( zoneNum, zoneHeatingLatentLoad );
	EXPECT_EQ( 0.0, c3->rmLatentAtPeak );
	zoneNum = 2;
	coilSelectionReportObj->setZoneLatentLoadHeatingIdealPeak( zoneNum, zoneHeatingLatentLoad );
	EXPECT_EQ( 100.0, c3->rmLatentAtPeak );
	zoneNum = 3;
	coilSelectionReportObj->setZoneLatentLoadHeatingIdealPeak( zoneNum, zoneHeatingLatentLoad );
	EXPECT_EQ( 200.0, c3->rmLatentAtPeak );

	// Exercise report writing again
	coilSelectionReportObj->finishCoilSummaryReportTable();
}

TEST_F( EnergyPlusFixture, ReportCoilSelection_SteamCoil )
{
	std::string coil1Name ( "Coil 1" ); // user-defined name of the coil
	std::string coil1Type ( "Coil:Heating:Steam" ); // idf input object class name of coil
	int wInletNodeNum = 9;
	int wOutletNodeNum = 15;

	EnergyPlus::DataPlant::TotNumLoops = 1;
	DataPlant::PlantLoop.allocate( 1 );
	DataPlant::PlantLoop( 1 ).Name = "Steam Loop";
	DataPlant::PlantLoop( 1 ).FluidName = "Steam";
	DataPlant::PlantLoop( 1 ).FluidIndex = 1;
	DataPlant::PlantLoop( 1 ).MaxMassFlowRate = 0.1;
	DataPlant::PlantLoop( 1 ).LoopSide.allocate( 2 );
	DataPlant::PlantLoop( 1 ).LoopSide( 1 ).Branch.allocate( 1 );
	DataPlant::PlantLoop( 1 ).LoopSide( 1 ).TotalBranches = 1;
	DataPlant::PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp.allocate( 1 );
	DataPlant::PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).TotalComponents = 1;
	DataPlant::PlantLoop( 1 ).LoopSide( 2 ).Branch.allocate( 1 );
	DataPlant::PlantLoop( 1 ).LoopSide( 2 ).TotalBranches = 1;
	DataPlant::PlantLoop( 1 ).LoopSide( 2 ).Branch( 1 ).Comp.allocate( 1 );
	DataPlant::PlantLoop( 1 ).LoopSide( 2 ).Branch( 1 ).TotalComponents = 1;

	DataPlant::PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = 0;
	DataPlant::PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumOut = 0;
	DataPlant::PlantLoop( 1 ).LoopSide( 2 ).Branch( 1 ).Comp( 1 ).NodeNumIn = wInletNodeNum;
	DataPlant::PlantLoop( 1 ).LoopSide( 2 ).Branch( 1 ).Comp( 1 ).NodeNumOut = wOutletNodeNum;

	Real64 airVdot ( 0.052 ); // air flow rate in m3/s
	bool isAutoSized ( false ); // true if autosized
	coilSelectionReportObj->setCoilAirFlow( coil1Name, coil1Type, airVdot, isAutoSized );
	auto & c1 ( coilSelectionReportObj->coilSelectionDataObjs[ 0 ] );
	EXPECT_EQ( coil1Name, c1->coilName_ );
	EXPECT_EQ( coil1Type, c1->coilObjName );
	EXPECT_EQ( airVdot, c1->coilDesVolFlow );
	EXPECT_EQ( isAutoSized, c1->volFlowIsAutosized );

	int loopNum = 1;
	Real64 waterVdot = 0.05;
	// First with no plant sizing objects defined
	isAutoSized = false; // true if autosized
	coilSelectionReportObj->setCoilWaterFlowNodeNums( coil1Name, coil1Type, waterVdot,  isAutoSized, wInletNodeNum, wOutletNodeNum, loopNum );
	EXPECT_EQ( -999, c1->pltSizNum );
	EXPECT_EQ( loopNum, c1->waterLoopNum );
	EXPECT_EQ( DataPlant::PlantLoop( 1 ).Name, c1->plantLoopName );
	EXPECT_EQ( -999, c1->rhoFluid );
	EXPECT_EQ( -999, c1->cpFluid );
	EXPECT_EQ( -999, c1->coilDesWaterMassFlow );
	EXPECT_EQ( "No", c1->coilWaterFlowAutoMsg );

	// Now add a plant sizing object
	DataSizing::NumPltSizInput = 1;
	DataSizing::PlantSizData.allocate( 1 );
	DataSizing::PlantSizData( 1 ).PlantLoopName = "Steam Loop";
	DataSizing::PlantSizData( 1 ).LoopType = DataSizing::SteamLoop;
	isAutoSized = true; // true if autosized
	coilSelectionReportObj->setCoilWaterFlowNodeNums( coil1Name, coil1Type, waterVdot,  isAutoSized, wInletNodeNum, wOutletNodeNum, loopNum );
	auto & c1b ( coilSelectionReportObj->coilSelectionDataObjs[ 0 ] );
	EXPECT_EQ( 1, c1b->pltSizNum );
	EXPECT_EQ( loopNum, c1b->waterLoopNum );
	EXPECT_EQ( DataPlant::PlantLoop( 1 ).Name, c1b->plantLoopName );
	EXPECT_NEAR( 0.6, c1b->rhoFluid, 0.01 );
	EXPECT_NEAR( 4216.0, c1b->cpFluid, 0.1 );
	Real64 expFlow = waterVdot * c1b->rhoFluid;
	EXPECT_NEAR( expFlow, c1b->coilDesWaterMassFlow, 0.01 );
	EXPECT_EQ( "Yes", c1b->coilWaterFlowAutoMsg );

	// Exercise report writing again
	coilSelectionReportObj->finishCoilSummaryReportTable();
}

TEST_F( EnergyPlusFixture, ReportCoilSelection_ZoneEqCoil )
{
	std::string coil1Name ( "Coil 1" ); // user-defined name of the coil
	std::string coil1Type ( "Coil:Heating:Fuel" ); // idf input object class name of coil

	DataGlobals::NumOfZones = 3;
	DataHeatBalance::Zone.allocate( DataGlobals::NumOfZones );
	DataHeatBalance::Zone( 1 ).Name = "Zone 1";
	DataHeatBalance::Zone( 2 ).Name = "Zone 2";
	DataHeatBalance::Zone( 3 ).Name = "Zone 3";

	int curSysNum = 0;
	int curZoneEqNum = 2;
	DataZoneEquipment::ZoneEquipList.allocate( 3 );
	DataZoneEquipment::ZoneEquipList( curZoneEqNum ).NumOfEquipTypes = 2;
	DataZoneEquipment::ZoneEquipList( curZoneEqNum ).EquipName.allocate( 2 );
	DataZoneEquipment::ZoneEquipList( curZoneEqNum ).EquipType.allocate( 2 );
	DataZoneEquipment::ZoneEquipList( curZoneEqNum ).EquipType_Num.allocate( 2 );
	DataZoneEquipment::ZoneEquipList( curZoneEqNum ).EquipName( 1 )= "Zone 2 Fan Coil";
	DataZoneEquipment::ZoneEquipList( curZoneEqNum ).EquipType( 1 )= "ZoneHVAC:FourPipeFanCoil";
	DataZoneEquipment::ZoneEquipList( curZoneEqNum ).EquipType_Num( 1 ) = DataHVACGlobals::ZoneEquipTypeOf_FourPipeFanCoil;
	DataZoneEquipment::ZoneEquipList( curZoneEqNum ).EquipName( 2 )= "Zone 2 Unit Heater";
	DataZoneEquipment::ZoneEquipList( curZoneEqNum ).EquipType( 2 )= "ZoneHVAC:UnitHeater";
	DataZoneEquipment::ZoneEquipList( curZoneEqNum ).EquipType_Num( 2 ) = DataHVACGlobals::ZoneEquipTypeOf_UnitVentilator;

	Real64 totGrossCap = 500.0;
	Real64 sensGrossCap = 500.0;
	Real64 airFlowRate = 0.11;
	Real64 waterFlowRate = 0.0;

	coilSelectionReportObj->setCoilFinalSizes(  coil1Name, coil1Type, totGrossCap, sensGrossCap, airFlowRate, waterFlowRate );
	auto & c1 ( coilSelectionReportObj->coilSelectionDataObjs[ 0 ] );
	EXPECT_EQ( totGrossCap, c1->coilTotCapFinal );
	EXPECT_EQ( sensGrossCap, c1->coilSensCapFinal );
	EXPECT_EQ( airFlowRate, c1->coilRefAirVolFlowFinal );
	EXPECT_EQ( waterFlowRate, c1->coilRefWaterVolFlowFinal );

	Real64 RatedCoilTotCap = 400.0;
	Real64 RatedCoilSensCap = 399.0;
	Real64 RatedAirMassFlow = 0.001;
	Real64 RatedCoilInDb = -999.0;
	Real64 RatedCoilInHumRat = -999.0;
	Real64 RatedCoilInWb = 20.0;
	Real64 RatedCoilOutDb = -999.0;
	Real64 RatedCoilOutHumRat = -999.0;
	Real64 RatedCoilOutWb = 30.0;
	Real64 RatedCoilOadbRef = 24.0;
	Real64 RatedCoilOawbRef = 16.0;
	Real64 RatedCoilBpFactor = 0.2;
	Real64 RatedCoilEff = 0.8;

	// First without setting coil inlet/outlet conditions
	coilSelectionReportObj->setRatedCoilConditions( coil1Name, coil1Type, RatedCoilTotCap, RatedCoilSensCap, RatedAirMassFlow, RatedCoilInDb, RatedCoilInHumRat, RatedCoilInWb, 
		RatedCoilOutDb, RatedCoilOutHumRat, RatedCoilOutWb, RatedCoilOadbRef, RatedCoilOawbRef, RatedCoilBpFactor, RatedCoilEff );

	EXPECT_EQ( RatedCoilTotCap, c1->coilRatedTotCap );
	EXPECT_EQ( RatedCoilSensCap, c1->coilRatedSensCap );
	EXPECT_EQ( RatedAirMassFlow, c1->ratedAirMassFlow );
	EXPECT_EQ( RatedCoilInDb, c1->ratedCoilInDb );
	EXPECT_EQ( RatedCoilInWb, c1->ratedCoilInWb );
	EXPECT_EQ( RatedCoilInHumRat, c1->ratedCoilInHumRat );
	EXPECT_EQ( -999.0, c1->ratedCoilInEnth );
	EXPECT_EQ( RatedCoilOutDb, c1->ratedCoilOutDb );
	EXPECT_EQ( RatedCoilOutWb, c1->ratedCoilOutWb );
	EXPECT_EQ( RatedCoilOutHumRat, c1->ratedCoilOutHumRat );
	EXPECT_EQ( -999.0, c1->ratedCoilOutEnth );
	EXPECT_EQ( RatedCoilEff, c1->ratedCoilEff );
	EXPECT_EQ( RatedCoilBpFactor, c1->ratedCoilBpFactor );
	EXPECT_EQ( RatedCoilOadbRef, c1->ratedCoilOadbRef );
	EXPECT_EQ( RatedCoilOawbRef, c1->ratedCoilOawbRef );

	// again with setting coil inlet/outlet conditions
	RatedCoilInDb = 23.0;
	RatedCoilInHumRat = 0.008;
	RatedCoilOutDb = 40.0;
	RatedCoilOutHumRat = 0.009;
	coilSelectionReportObj->setRatedCoilConditions( coil1Name, coil1Type, RatedCoilTotCap, RatedCoilSensCap, RatedAirMassFlow, RatedCoilInDb, RatedCoilInHumRat, RatedCoilInWb, 
		RatedCoilOutDb, RatedCoilOutHumRat, RatedCoilOutWb, RatedCoilOadbRef, RatedCoilOawbRef, RatedCoilBpFactor, RatedCoilEff );
	EXPECT_EQ( RatedCoilInDb, c1->ratedCoilInDb );
	EXPECT_EQ( RatedCoilInHumRat, c1->ratedCoilInHumRat );
	EXPECT_NEAR( 43460.9, c1->ratedCoilInEnth, 0.1 );
	EXPECT_EQ( RatedCoilOutDb, c1->ratedCoilOutDb );
	EXPECT_EQ( RatedCoilOutHumRat, c1->ratedCoilOutHumRat );
	EXPECT_NEAR( 63371.3, c1->ratedCoilOutEnth, 0.1 );

	Real64 entAirDryBulbTemp = 24.0;
	coilSelectionReportObj->setCoilEntAirTemp( coil1Name, coil1Type, entAirDryBulbTemp, curSysNum, curZoneEqNum );
	EXPECT_EQ( entAirDryBulbTemp, c1->coilDesEntTemp );
	EXPECT_EQ( curSysNum, c1->airloopNum );
	EXPECT_EQ( curZoneEqNum, c1->zoneEqNum );

	Real64 entAirHumRat = 0.004;
	coilSelectionReportObj->setCoilEntAirHumRat( coil1Name, coil1Type, entAirHumRat );
	EXPECT_EQ( entAirHumRat, c1->coilDesEntHumRat );

	Real64 entWaterTemp = 60.0;
	coilSelectionReportObj->setCoilEntWaterTemp( coil1Name, coil1Type, entWaterTemp );
	EXPECT_EQ( entWaterTemp, c1->coilDesWaterEntTemp );

	Real64 lvgWaterTemp = 50.0;
	coilSelectionReportObj->setCoilLvgWaterTemp( coil1Name, coil1Type, lvgWaterTemp );
	EXPECT_EQ( lvgWaterTemp, c1->coilDesWaterLvgTemp );

	Real64 CoilWaterDeltaT = 50.0;
	coilSelectionReportObj->setCoilWaterDeltaT( coil1Name, coil1Type, CoilWaterDeltaT );
	EXPECT_EQ( CoilWaterDeltaT, c1->coilDesWaterTempDiff );

	Real64 lvgAirDryBulbTemp = 12.0;
	coilSelectionReportObj->setCoilLvgAirTemp( coil1Name, coil1Type, lvgAirDryBulbTemp );
	EXPECT_EQ( lvgAirDryBulbTemp, c1->coilDesLvgTemp );

	Real64 lvgAirHumRat = 0.006;
	coilSelectionReportObj->setCoilLvgAirHumRat( coil1Name, coil1Type, lvgAirHumRat );
	EXPECT_EQ( lvgAirHumRat, c1->coilDesLvgHumRat );

	int zoneNum = 1;
	Real64 zoneCoolingLatentLoad = 1234.0;
	coilSelectionReportObj->setZoneLatentLoadCoolingIdealPeak( zoneNum, zoneCoolingLatentLoad );
	// Expect zero because it's a heating coil
	EXPECT_EQ( 0.0, c1->rmLatentAtPeak );

	Real64 zoneHeatingLatentLoad = 4321.0;
	coilSelectionReportObj->setZoneLatentLoadHeatingIdealPeak( zoneNum, zoneHeatingLatentLoad );
	// Expect zero because doZoneEqSetup isn't currently executed
	EXPECT_EQ( 0.0, c1->rmLatentAtPeak );

	// Exercise report writing again
	coilSelectionReportObj->finishCoilSummaryReportTable();
}
