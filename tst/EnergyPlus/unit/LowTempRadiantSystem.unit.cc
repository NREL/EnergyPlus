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

// EnergyPlus::Low Temperature Radiant Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/LowTempRadiantSystem.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <ObjexxFCL/gio.hh>


using namespace EnergyPlus;
using namespace EnergyPlus::LowTempRadiantSystem;
using namespace ObjexxFCL;
using namespace EnergyPlus::DataHeatBalance;
using namespace DataGlobals;
using namespace EnergyPlus::DataPlant;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::FluidProperties;


class LowTempRadiantSystemTest : public EnergyPlusFixture
{
public:
	int RadSysNum;
	int SystemType;
	Real64 ExpectedResult1;
	Real64 ExpectedResult2;
	Real64 ExpectedResult3;
	Real64 const CpWater = 4180.0; // For estimating the expected result
	Real64 const RhoWater = 1000.0; // For estimating the expected result

protected:
	virtual void SetUp() {
		EnergyPlusFixture::SetUp();  // Sets up the base fixture first.

		ElecRadSys.allocate( 1 );
		HydrRadSys.allocate( 1 );
		CFloRadSys.allocate( 1 );
		CalcFinalZoneSizing.allocate( 1 );
		ZoneEqSizing.allocate( 1 );
		Zone.allocate( 1 );
		CurZoneEqNum = 1;
		ZoneEqSizing( CurZoneEqNum ).SizingMethod.allocate( 25 );
		ZoneSizingRunDone = true;

		CurSysNum = 0;
		RadSysNum = 1;
		SystemType = ElectricSystem;
		ElecRadSysNumericFields.allocate( 1 );
		ElecRadSysNumericFields( RadSysNum ).FieldNames.allocate( 1 );
		HydronicRadiantSysNumericFields.allocate( 1 );
		HydronicRadiantSysNumericFields( RadSysNum ).FieldNames.allocate( 15 );
		HydrRadSys( RadSysNum ).NumCircuits.allocate( 1 );
		CFloRadSys( RadSysNum ).NumCircuits.allocate( 1 );
		// set up plant loop
		TotNumLoops = 2;
		PlantLoop.allocate( TotNumLoops );
		PlantSizData.allocate( TotNumLoops );
		NumPltSizInput = TotNumLoops;

		for ( int loopindex = 1; loopindex <= TotNumLoops; ++loopindex ) {
			auto & loop( PlantLoop( loopindex ) );
			loop.LoopSide.allocate( 2 );
			auto & loopside( PlantLoop( loopindex ).LoopSide( 1 ) );
			loopside.TotalBranches = 1;
			loopside.Branch.allocate( 1 );
			auto & loopsidebranch( PlantLoop( loopindex ).LoopSide( 1 ).Branch( 1 ) );
			loopsidebranch.TotalComponents = 1;
			loopsidebranch.Comp.allocate( 1 );
		}
		PlantLoop( 1 ).Name = "Hot Water Loop";
		PlantLoop( 1 ).FluidName = "WATER";
		PlantLoop( 1 ).FluidIndex = 1;

		PlantLoop( 2 ).Name = "Chilled Water Loop";
		PlantLoop( 2 ).FluidName = "WATER";
		PlantLoop( 2 ).FluidIndex = 1;

		PlantSizData( 1 ).PlantLoopName = "Hot Water Loop";
		PlantSizData( 1 ).ExitTemp = 80.0;
		PlantSizData( 1 ).DeltaT = 10.0;

		PlantSizData( 2 ).PlantLoopName = "Chilled Water Loop";
		PlantSizData( 2 ).ExitTemp = 6.0;
		PlantSizData( 2 ).DeltaT = 5.0;

		ExpectedResult1 = 0.0;
		ExpectedResult2 = 0.0;
		ExpectedResult3 = 0.0;
	}

	virtual void TearDown() {
		EnergyPlusFixture::TearDown();  // Remember to tear down the base fixture after cleaning up derived fixture!
	}
};

TEST_F( LowTempRadiantSystemTest, SizeLowTempRadiantElectric )
{
	SystemType = ElectricSystem;
	ElecRadSys( RadSysNum ).Name = "LowTempElectric 1";
	ElecRadSys( RadSysNum ).ZonePtr = 1;
	ElecRadSysNumericFields( RadSysNum ).FieldNames( 1 ) = "Heating Design Capacity";

	//Electric - HeatingDesignCapacity method
	ElecRadSys( RadSysNum ).MaxElecPower = AutoSize;
	ElecRadSys( RadSysNum ).HeatingCapMethod = HeatingDesignCapacity;
	ElecRadSys( RadSysNum ).ScaledHeatingCapacity = AutoSize;
	CalcFinalZoneSizing( CurZoneEqNum ).DesHeatLoad = 1000.0;
	CalcFinalZoneSizing( CurZoneEqNum ).HeatSizingFactor = 1.2;
	SizeLowTempRadiantSystem( RadSysNum, SystemType );
	EXPECT_NEAR( 1200.0, ElecRadSys( RadSysNum ).MaxElecPower, 0.1 );

	//Electric - CapacityPerFloorArea method - hold until scalable sizing issue is resolved
	ElecRadSys( RadSysNum ).MaxElecPower = AutoSize;
	ElecRadSys( RadSysNum ).HeatingCapMethod = CapacityPerFloorArea;
	ElecRadSys( RadSysNum ).ScaledHeatingCapacity = 1.5;
	Zone( 1 ).FloorArea = 500.0;
	SizeLowTempRadiantSystem( RadSysNum, SystemType );
	EXPECT_NEAR( 750.0, ElecRadSys( RadSysNum ).MaxElecPower, 0.1 );

	//Electric - FractionOfAutosizedHeatingCapacity method - hold until scalable sizing issue is resolved
	ElecRadSys( RadSysNum ).MaxElecPower = AutoSize;
	ElecRadSys( RadSysNum ).HeatingCapMethod = FractionOfAutosizedHeatingCapacity;
	ElecRadSys( RadSysNum ).ScaledHeatingCapacity = 10.0;
	CalcFinalZoneSizing( CurZoneEqNum ).DesHeatLoad = 800.0;
	CalcFinalZoneSizing( CurZoneEqNum ).HeatSizingFactor = 1.1;
	SizeLowTempRadiantSystem( RadSysNum, SystemType );
	EXPECT_NEAR( 8800.0, ElecRadSys( RadSysNum ).MaxElecPower, 0.1 );
}

TEST_F( LowTempRadiantSystemTest, SizeLowTempRadiantVariableFlow )
{
	SystemType = HydronicSystem;
	HydrRadSys( RadSysNum ).Name = "LowTempVarFlow 1";
	HydrRadSys( RadSysNum ).ZonePtr = 1;
	HydronicRadiantSysNumericFields( RadSysNum ).FieldNames( 3 ) = "Heating Design Capacity";
	HydronicRadiantSysNumericFields( RadSysNum ).FieldNames( 8 ) = "Cooling Design Capacity";

	HydrRadSys( RadSysNum ).HotWaterInNode = 1;
	HydrRadSys( RadSysNum ).HotWaterOutNode = 2;
	HydrRadSys( RadSysNum ).HWLoopNum = 1;
	PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = HydrRadSys( RadSysNum ).HotWaterInNode;

	HydrRadSys( RadSysNum ).ColdWaterInNode = 3;
	HydrRadSys( RadSysNum ).ColdWaterOutNode = 4;
	HydrRadSys( RadSysNum ).CWLoopNum = 2;
	PlantLoop( 2 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = HydrRadSys( RadSysNum ).ColdWaterInNode;

	//Hydronic - HeatingDesignCapacity/CoolingDesignCapacity method
	HydrRadSys( RadSysNum ).WaterVolFlowMaxHeat = AutoSize;
	HydrRadSys( RadSysNum ).HeatingCapMethod = HeatingDesignCapacity;
	HydrRadSys( RadSysNum ).ScaledHeatingCapacity = AutoSize;
	CalcFinalZoneSizing( CurZoneEqNum ).DesHeatLoad = 1000.0;
	CalcFinalZoneSizing( CurZoneEqNum ).HeatSizingFactor = 1.2;
	ExpectedResult1 = CalcFinalZoneSizing( CurZoneEqNum ).DesHeatLoad * CalcFinalZoneSizing( CurZoneEqNum ).HeatSizingFactor;
	ExpectedResult1 = ExpectedResult1 / ( PlantSizData( 1 ).DeltaT * RhoWater * CpWater );

	HydrRadSys( RadSysNum ).WaterVolFlowMaxCool = AutoSize;
	HydrRadSys( RadSysNum ).CoolingCapMethod = CoolingDesignCapacity;
	HydrRadSys( RadSysNum ).ScaledCoolingCapacity = AutoSize;
	CalcFinalZoneSizing( CurZoneEqNum ).DesCoolLoad = 2000.0;
	CalcFinalZoneSizing( CurZoneEqNum ).CoolSizingFactor = 1.1;
	ExpectedResult2 = CalcFinalZoneSizing( CurZoneEqNum ).DesCoolLoad * CalcFinalZoneSizing( CurZoneEqNum ).CoolSizingFactor;
	ExpectedResult2 = ExpectedResult2 / ( PlantSizData( 2 ).DeltaT * RhoWater * CpWater );

	HydrRadSys( RadSysNum ).NumCircCalcMethod = 0;
	HydrRadSys( RadSysNum ).NumOfSurfaces = 1;
	HydrRadSys( RadSysNum ).TubeLength = AutoSize;
	HydrRadSys( RadSysNum ).TotalSurfaceArea = 1500.0;
	ExpectedResult3 = HydrRadSys( RadSysNum ).TotalSurfaceArea / 0.15;

	SizeLowTempRadiantSystem( RadSysNum, SystemType );
	EXPECT_NEAR( ExpectedResult1, HydrRadSys( RadSysNum ).WaterVolFlowMaxHeat, 0.1 );
	EXPECT_NEAR( ExpectedResult2, HydrRadSys( RadSysNum ).WaterVolFlowMaxCool, 0.1 );
	EXPECT_NEAR( ExpectedResult3, HydrRadSys( RadSysNum ).TubeLength, 0.1 );

	//Hydronic - CapacityPerFloorArea method
	HydrRadSys( RadSysNum ).WaterVolFlowMaxHeat = AutoSize;
	HydrRadSys( RadSysNum ).HeatingCapMethod = CapacityPerFloorArea;
	HydrRadSys( RadSysNum ).ScaledHeatingCapacity = 10.0;
	Zone( 1 ).FloorArea = 500.0;
	ExpectedResult1 = HydrRadSys( RadSysNum ).ScaledHeatingCapacity * Zone( 1 ).FloorArea;
	ExpectedResult1 = ExpectedResult1 / ( PlantSizData( 1 ).DeltaT * RhoWater * CpWater );

	HydrRadSys( RadSysNum ).WaterVolFlowMaxCool = AutoSize;
	HydrRadSys( RadSysNum ).CoolingCapMethod = CapacityPerFloorArea;
	HydrRadSys( RadSysNum ).ScaledCoolingCapacity = 20.0;
	ExpectedResult2 = HydrRadSys( RadSysNum ).ScaledCoolingCapacity * Zone( 1 ).FloorArea;
	ExpectedResult2 = ExpectedResult2 / ( PlantSizData( 2 ).DeltaT * RhoWater * CpWater );

	SizeLowTempRadiantSystem( RadSysNum, SystemType );
	EXPECT_NEAR( ExpectedResult1, HydrRadSys( RadSysNum ).WaterVolFlowMaxHeat, 0.1 );
	EXPECT_NEAR( ExpectedResult2, HydrRadSys( RadSysNum ).WaterVolFlowMaxCool, 0.1 );

	//Hydronic - FractionOfAutosizedHeating/CoolingCapacity method
	HydrRadSys( RadSysNum ).WaterVolFlowMaxHeat = AutoSize;
	HydrRadSys( RadSysNum ).HeatingCapMethod = FractionOfAutosizedHeatingCapacity;
	HydrRadSys( RadSysNum ).ScaledHeatingCapacity = 1.2;
	CalcFinalZoneSizing( CurZoneEqNum ).DesHeatLoad = 800.0;
	CalcFinalZoneSizing( CurZoneEqNum ).HeatSizingFactor = 1.1;
	ExpectedResult1 = HydrRadSys( RadSysNum ).ScaledHeatingCapacity * CalcFinalZoneSizing( CurZoneEqNum ).DesHeatLoad * CalcFinalZoneSizing( CurZoneEqNum ).HeatSizingFactor;
	ExpectedResult1 = ExpectedResult1 / ( PlantSizData( 1 ).DeltaT * RhoWater * CpWater );

	HydrRadSys( RadSysNum ).WaterVolFlowMaxCool = AutoSize;
	HydrRadSys( RadSysNum ).CoolingCapMethod = FractionOfAutosizedCoolingCapacity;
	HydrRadSys( RadSysNum ).ScaledCoolingCapacity = 1.5;
	CalcFinalZoneSizing( CurZoneEqNum ).DesCoolLoad = 1000.0;
	CalcFinalZoneSizing( CurZoneEqNum ).CoolSizingFactor = 1.2;
	ExpectedResult2 = HydrRadSys( RadSysNum ).ScaledCoolingCapacity * CalcFinalZoneSizing( CurZoneEqNum ).DesCoolLoad * CalcFinalZoneSizing( CurZoneEqNum ).CoolSizingFactor;
	ExpectedResult2 = ExpectedResult2 / ( PlantSizData( 2 ).DeltaT * RhoWater * CpWater );

	SizeLowTempRadiantSystem( RadSysNum, SystemType );
	EXPECT_NEAR( ExpectedResult1, HydrRadSys( RadSysNum ).WaterVolFlowMaxHeat, 0.1 );
	EXPECT_NEAR( ExpectedResult2, HydrRadSys( RadSysNum ).WaterVolFlowMaxCool, 0.1 );
}

TEST_F( LowTempRadiantSystemTest, SizeCapacityLowTempRadiantVariableFlow )
{
	SystemType = HydronicSystem;
	HydrRadSys( RadSysNum ).Name = "LowTempVarFlow 1";
	HydrRadSys( RadSysNum ).ZonePtr = 1;
	HydronicRadiantSysNumericFields( RadSysNum ).FieldNames( 3 ) = "Heating Design Capacity";
	HydronicRadiantSysNumericFields( RadSysNum ).FieldNames( 8 ) = "Cooling Design Capacity";

	HydrRadSys( RadSysNum ).HotWaterInNode = 1;
	HydrRadSys( RadSysNum ).HotWaterOutNode = 2;
	HydrRadSys( RadSysNum ).HWLoopNum = 1;
	PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = HydrRadSys( RadSysNum ).HotWaterInNode;

	HydrRadSys( RadSysNum ).ColdWaterInNode = 3;
	HydrRadSys( RadSysNum ).ColdWaterOutNode = 4;
	HydrRadSys( RadSysNum ).CWLoopNum = 2;
	PlantLoop( 2 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = HydrRadSys( RadSysNum ).ColdWaterInNode;

	//Hydronic - HeatingDesignCapacity/CoolingDesignCapacity Autosize Method
	HydrRadSys( RadSysNum ).HeatingCapMethod = HeatingDesignCapacity;
	HydrRadSys( RadSysNum ).ScaledHeatingCapacity = AutoSize;
	CalcFinalZoneSizing( CurZoneEqNum ).DesHeatLoad = 1000.0;
	CalcFinalZoneSizing( CurZoneEqNum ).HeatSizingFactor = 1.2;
	ExpectedResult1 = CalcFinalZoneSizing( CurZoneEqNum ).DesHeatLoad * CalcFinalZoneSizing( CurZoneEqNum ).HeatSizingFactor;

	HydrRadSys( RadSysNum ).CoolingCapMethod = CoolingDesignCapacity;
	HydrRadSys( RadSysNum ).ScaledCoolingCapacity = AutoSize;
	CalcFinalZoneSizing( CurZoneEqNum ).DesCoolLoad = 2000.0;
	CalcFinalZoneSizing( CurZoneEqNum ).CoolSizingFactor = 1.1;
	ExpectedResult2 = CalcFinalZoneSizing( CurZoneEqNum ).DesCoolLoad * CalcFinalZoneSizing( CurZoneEqNum ).CoolSizingFactor;

	SizeLowTempRadiantSystem( RadSysNum, SystemType );
	EXPECT_NEAR( ExpectedResult1, HydrRadSys( RadSysNum ).ScaledHeatingCapacity, 0.1 );
	EXPECT_NEAR( ExpectedResult2, HydrRadSys( RadSysNum ).ScaledCoolingCapacity, 0.1 );

	//Hydronic - CapacityPerFloorArea Capacity Sizing Method
	Zone( 1 ).FloorArea = 50.0;
	HydrRadSys( RadSysNum ).HeatingCapMethod = CapacityPerFloorArea;
	HydrRadSys( RadSysNum ).ScaledHeatingCapacity = 200.0;
	ExpectedResult1 = HydrRadSys( RadSysNum ).ScaledHeatingCapacity * Zone( 1 ).FloorArea;

	HydrRadSys( RadSysNum ).CoolingCapMethod = CapacityPerFloorArea;
	HydrRadSys( RadSysNum ).ScaledCoolingCapacity = 250.0;
	ExpectedResult2 = HydrRadSys( RadSysNum ).ScaledCoolingCapacity * Zone( 1 ).FloorArea;

	SizeLowTempRadiantSystem( RadSysNum, SystemType );
	EXPECT_NEAR( ExpectedResult1, HydrRadSys( RadSysNum ).ScaledHeatingCapacity, 0.1 );
	EXPECT_NEAR( ExpectedResult2, HydrRadSys( RadSysNum ).ScaledCoolingCapacity, 0.1 );

	//Hydronic - FractionOfAutosizedHeating/CoolingCapacity Sizing Method
	HydrRadSys( RadSysNum ).HeatingCapMethod = FractionOfAutosizedHeatingCapacity;
	HydrRadSys( RadSysNum ).ScaledHeatingCapacity = 1.2;
	CalcFinalZoneSizing( CurZoneEqNum ).DesHeatLoad = 800.0;
	CalcFinalZoneSizing( CurZoneEqNum ).HeatSizingFactor = 1.1;
	ExpectedResult1 = HydrRadSys( RadSysNum ).ScaledHeatingCapacity * CalcFinalZoneSizing( CurZoneEqNum ).DesHeatLoad * CalcFinalZoneSizing( CurZoneEqNum ).HeatSizingFactor;

	HydrRadSys( RadSysNum ).CoolingCapMethod = FractionOfAutosizedCoolingCapacity;
	HydrRadSys( RadSysNum ).ScaledCoolingCapacity = 1.5;
	CalcFinalZoneSizing( CurZoneEqNum ).DesCoolLoad = 1000.0;
	CalcFinalZoneSizing( CurZoneEqNum ).CoolSizingFactor = 1.2;
	ExpectedResult2 = HydrRadSys( RadSysNum ).ScaledCoolingCapacity * CalcFinalZoneSizing( CurZoneEqNum ).DesCoolLoad * CalcFinalZoneSizing( CurZoneEqNum ).CoolSizingFactor;

	SizeLowTempRadiantSystem( RadSysNum, SystemType );
	EXPECT_NEAR( ExpectedResult1, HydrRadSys( RadSysNum ).ScaledHeatingCapacity, 0.1 );
	EXPECT_NEAR( ExpectedResult2, HydrRadSys( RadSysNum ).ScaledCoolingCapacity, 0.1 );
}

TEST_F( LowTempRadiantSystemTest, SizeLowTempRadiantConstantFlow )
{
	SystemType = ConstantFlowSystem;
	CFloRadSys( RadSysNum ).Name = "LowTempConstantFlow 1";
	CFloRadSys( RadSysNum ).ZonePtr = 1;
	HydronicRadiantSysNumericFields( RadSysNum ).FieldNames( 2 ) = "Rated Flow Rate";
	HydronicRadiantSysNumericFields( RadSysNum ).FieldNames( 3 ) = "Total length of pipe embedded in surface";

	CFloRadSys( RadSysNum ).HotWaterInNode = 1;
	CFloRadSys( RadSysNum ).HotWaterOutNode = 2;
	CFloRadSys( RadSysNum ).HWLoopNum = 1;
	PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = CFloRadSys( RadSysNum ).HotWaterInNode;

	CFloRadSys( RadSysNum ).ColdWaterInNode = 3;
	CFloRadSys( RadSysNum ).ColdWaterOutNode = 4;
	CFloRadSys( RadSysNum ).CWLoopNum = 2;
	PlantLoop( 2 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = CFloRadSys( RadSysNum ).ColdWaterInNode;

	//Hydronic - Hot water volume flow rate autosize
	CFloRadSys( RadSysNum ).ColdWaterInNode = 0;
	CFloRadSys( RadSysNum ).ColdWaterOutNode = 0;
	CFloRadSys( RadSysNum ).WaterVolFlowMax = AutoSize;
	CalcFinalZoneSizing( CurZoneEqNum ).DesHeatLoad = 1000.0;
	CalcFinalZoneSizing( CurZoneEqNum ).HeatSizingFactor = 1.2;
	ExpectedResult1 = CalcFinalZoneSizing( CurZoneEqNum ).DesHeatLoad * CalcFinalZoneSizing( CurZoneEqNum ).HeatSizingFactor;
	ExpectedResult1 = ExpectedResult1 / ( PlantSizData( 1 ).DeltaT * RhoWater * CpWater );

	SizeLowTempRadiantSystem( RadSysNum, SystemType );
	EXPECT_NEAR( ExpectedResult1, CFloRadSys( RadSysNum ).WaterVolFlowMax, 0.001 );

	//Hydronic - cold water volume flow rate autosize
	CFloRadSys( RadSysNum ).HotWaterInNode = 0;
	CFloRadSys( RadSysNum ).HotWaterOutNode = 0;
	CFloRadSys( RadSysNum ).ColdWaterInNode = 3;
	CFloRadSys( RadSysNum ).ColdWaterOutNode = 4;
	CFloRadSys( RadSysNum ).WaterVolFlowMax = AutoSize;
	CalcFinalZoneSizing( CurZoneEqNum ).DesCoolLoad = 2000.0;
	CalcFinalZoneSizing( CurZoneEqNum ).CoolSizingFactor = 1.1;
	ExpectedResult2 = CalcFinalZoneSizing( CurZoneEqNum ).DesCoolLoad * CalcFinalZoneSizing( CurZoneEqNum ).CoolSizingFactor;
	ExpectedResult2 = ExpectedResult2 / ( PlantSizData( 2 ).DeltaT * RhoWater * CpWater );

	SizeLowTempRadiantSystem( RadSysNum, SystemType );
	EXPECT_NEAR( ExpectedResult2, CFloRadSys( RadSysNum ).WaterVolFlowMax, 0.001 );

	//Hydronic - maximum water volume flow rate autosize
	CFloRadSys( RadSysNum ).WaterVolFlowMax = AutoSize;
	CFloRadSys( RadSysNum ).HotWaterInNode = 1;
	CFloRadSys( RadSysNum ).HotWaterOutNode = 2;
	CFloRadSys( RadSysNum ).ColdWaterInNode = 3;
	CFloRadSys( RadSysNum ).ColdWaterOutNode = 4;

	//Hydronic - embeded tube length autosize
	CFloRadSys( RadSysNum ).NumCircCalcMethod = 0;
	CFloRadSys( RadSysNum ).NumOfSurfaces = 1;
	CFloRadSys( RadSysNum ).TubeLength = AutoSize;
	CFloRadSys( RadSysNum ).TotalSurfaceArea = 150.0;
	ExpectedResult3 = CFloRadSys( RadSysNum ).TotalSurfaceArea / 0.15;

	SizeLowTempRadiantSystem( RadSysNum, SystemType );
	EXPECT_NEAR( std::max( ExpectedResult1, ExpectedResult2), CFloRadSys( RadSysNum ).WaterVolFlowMax, 0.001 );
	EXPECT_NEAR( ExpectedResult3, CFloRadSys( RadSysNum ).TubeLength, 0.1 );
}
