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
	//DataGlobals::InitConvTemp = 20.0;
	DataPlant::PlantLoop( 1 ).FluidIndex = 1;
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

	//void
	//finishCoilSummaryReportTable();

	//void
	//setCoilFinalSizes(  
	//	std::string const & coilName, // user-defined name of the coil
	//	std::string const & coilObjName , //  coil object name, e.g., Coil:Cooling:Water
	//	Real64 const totGrossCap, // total capacity [W]
	//	Real64 const sensGrossCap,// sensible capacity [W]
	//	Real64 const airFlowRate, // design or reference or rated air flow rate [m3/s]
	//	Real64 const waterFlowRate //design or reference or rated water flow rate [m3/s]
	//);
	//
	//void
	//setRatedCoilConditions(
	//	std::string const & coilName,    // ! user-defined name of the coil
	//	std::string const & coilObjName , //  coil object name, e.g., Coil:Cooling:Water
	//	Real64 const RatedCoilTotCap,    // ! rated coil total capacity [W]
	//	Real64 const RatedCoilSensCap,    // rated coil sensible capacity [W]
	//	Real64 const RatedAirMassFlow,    // rated coil design air mass flow rate [m3/s]
	//	Real64 const RatedCoilInDb,       // rated coil inlet air dry bulb at time of peak [C]
	//	Real64 const RatedCoilInHumRat,   // rated coil inlet air humidity ratio [kgWater/kgDryAir]
	//	Real64 const RatedCoilInWb,       // rated coil inlet air wet bulb [C]
	//	Real64 const RatedCoilOutDb,      // rated coil outlet air dry bulb [C]
	//	Real64 const RatedCoilOutHumRat,  // rated coil outlet air humidity ratio, [kgWater/kgDryAir]
	//	Real64 const RatedCoilOutWb,      // rated coil outlet air wet bulb [C]

	//	Real64 const RatedCoilOadbRef,    // rated DX coil outside air dry bulb reference [C]
	//	Real64 const RatedCoilOawbRef,    // rated DX coil outside air wet bulb reference [C]
	//	Real64 const RatedCoilBpFactor,   // rated coil bypass factor
	//	Real64 const RatedCoilEff        // rated coil effectiveness
	//);

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

	//void
	//setCoilWaterFlowPltSizNum(  
	//	std::string const & coilName, // user-defined name of the coil
	//	std::string const & coilType, // idf input object class name of coil
	//	Real64 const waterVdot, // water flow rate in m3/s
	//	bool const isAutoSized, // true if water flow was autosized
	//	int const DataPltSizNum, // plant sizing structure index
	//	int const DataWaterLoopNum // plant loop structure index
	//);

	//void
	//setCoilEntAirTemp(
	//	std::string const & coilName, // user-defined name of the coil
	//	std::string const & coilType, // idf input object class name of coil
	//	Real64 const entAirDryBulbTemp, // ideal loads sizing result for air entering coil drybulb temp (C)
	//	int const curSysNum, // airloop system number index, if non zero
	//	int const curZoneEqNum // zone equipment list index, if non-zero
	//);

	//void
	//setCoilEntAirHumRat(
	//	std::string const & coilName, // user-defined name of the coil
	//	std::string const & coilType, // idf input object class name of coil
	//	Real64 const entAirHumRat
	//);

	//void
	//setCoilEntWaterTemp(
	//	std::string const & coilName, // user-defined name of the coil
	//	std::string const & coilType, // idf input object class name of coil
	//	Real64 const entWaterTemp     // degree C
	//);

	//void
	//setCoilLvgWaterTemp(
	//	std::string const & coilName, // user-defined name of the coil
	//	std::string const & coilType, // idf input object class name of coil
	//	Real64 const lvgWaterTemp     // degree C
	//);
	//
	//void
	//setCoilWaterDeltaT(
	//	std::string const & coilName, // user-defined name of the coil
	//	std::string const & coilType, // idf input object class name of coil
	//	Real64 const CoilWaterDeltaT // degree C temperature difference used to size coil
	//);
	//
	//void
	//setCoilLvgAirTemp(
	//	std::string const & coilName, // user-defined name of the coil
	//	std::string const & coilType, // idf input object class name of coil
	//	Real64 const lvgAirDryBulbTemp // air temperature leaving coil {C}
	//);

	//void
	//setCoilLvgAirHumRat(
	//	std::string const & coilName, // user-defined name of the coil
	//	std::string const & coilType, // idf input object class name of coil
	//	Real64 const lvgAirHumRat // 
	//);

	//void
	//setCoilCoolingCapacity(
	//	std::string const & coilName, // user-defined name of the coil
	//	std::string const & coilType, // idf input object class name of coil
	//	Real64 const totalCoolingCap, // {W} coil cooling capacity
	//	bool const isAutoSize, // true if value was autosized
	//	int const curSysNum, // airloop system number index, if non zero
	//	int const curZoneEqNum, // zone equipment list index, if non-zero
	//	int const curOASysNum, // OA system equipment list index, if non-zero
	//	Real64 const fanCoolLoad, // {W} fan load used in ideal loads coil sizing
	//	Real64 const coilCapFunTempFac, // {W} curve result for modification factor for capacity as a function of temperature
	//	Real64 const DXFlowPerCapMinRatio, // non dimensional ratio, capacity adjustment ratio min
	//	Real64 const DXFlowPerCapMaxRatio // non dimensional ratio, capacity adjustment ratio max
	//);

	//void
	//setCoilHeatingCapacity(
	//	std::string const & coilName, // user-defined name of the coil
	//	std::string const & coilType, // idf input object class name of coil
	//	Real64 const totalHeatingCap, // {W} coil Heating capacity
	//	bool const isAutoSize, // true if value was autosized
	//	int const curSysNum, // airloop system number index, if non zero
	//	int const curZoneEqNum, // zone equipment list index, if non-zero
	//	int const curOASysNum, // OA system equipment list index, if non-zero
	//	Real64 const fanHeatGain, // {W} fan load used in ideal loads coil sizing
	//	Real64 const coilCapFunTempFac, // {W} curve result for modification factor for capacity as a function of temperature
	//	Real64 const DXFlowPerCapMinRatio, // non dimensional ratio, capacity adjustment ratio min
	//	Real64 const DXFlowPerCapMaxRatio // non dimensional ratio, capacity adjustment ratio max
	//);

	//void
	//setCoilWaterCoolingCapacity(
	//	std::string const & coilName, // user-defined name of the coil
	//	std::string const & coilType, // idf input object class name of coil
	//	Real64 const totalCoolingCap, // {W} coil cooling capacity
	//	bool const isAutoSize, // true if value was autosized
	//	int const inletNodeNum, // coil chw inlet node num
	//	int const outletNodeNum, // coil chw outlet node num
	//	int const dataWaterLoopNum // plant loop structure index
	//);

	//void
	//setCoilWaterHeaterCapacityNodeNums(
	//	std::string const & coilName, // user-defined name of the coil
	//	std::string const & coilType, // idf input object class name of coil
	//	Real64 const totalHeatingCap, // {W} coil Heating capacity
	//	bool const isAutoSize, // true if value was autosized
	//	int const inletNodeNum, // coil chw inlet node num
	//	int const outletNodeNum, // coil chw outlet node num
	//	int const dataWaterLoopNum // plant loop structure index
	//);

	//void
	//setCoilWaterHeaterCapacityPltSizNum(
	//	std::string const & coilName, // user-defined name of the coil
	//	std::string const & coilType, // idf input object class name of coil
	//	Real64 const totalHeatingCap, // {W} coil Heating capacity
	//	bool const isAutoSize, // true if value was autosized
	//	int const dataPltSizNum, // plant sizing structure index
	//	int const dataWaterLoopNum // plant loop structure index
	//);

	//void
	//setCoilUA(
	//	std::string const & coilName, // user-defined name of the coil
	//	std::string const & coilType, // idf input object class name of coil
	//	Real64 const UAvalue, // [W/k] UA value for coil, 
	//	Real64 const dataCapacityUsedForSizing, // [W] sizing global
	//	bool const isAutoSize, // true if value was autosized
	//	int const curSysNum, // airloop system number index, if non zero
	//	int const curZoneEqNum // zone equipment list index, if non-zero
	//);

	//void
	//setCoilReheatMultiplier(
	//	std::string const & coilName, // user-defined name of the coil
	//	std::string const & coilType, // idf input object class name of coil
	//	Real64 const multiplierReheatLoad
	//);

	//void
	//setCoilSupplyFanInfo(
	//	std::string const & coilName, // user-defined name of the coil
	//	std::string const & coilType, // idf input object class name of coil
	//	std::string const & fanName,
	//	DataAirSystems::fanModelTypeEnum const & fanEnumType,
	//	int const & fanIndex
	//);

	//std::string 
	//getTimeText(
	//	int const timeStepAtPeak
	//);

	//bool
	//isCompTypeFan(
	//	std::string const & compType // string component type, input object class name
	//);

	//bool 
	//isCompTypeCoil(
	//	std::string const & compType // string component type, input object class name
	//);

	//void
	//setZoneLatentLoadCoolingIdealPeak(
	//	int const zoneIndex,
	//	Real64 const zoneCoolingLatentLoad
	//);
	//
	//void
	//setZoneLatentLoadHeatingIdealPeak(
	//	int const zoneIndex,
	//	Real64 const zoneHeatingLatentLoad
	//);
}
