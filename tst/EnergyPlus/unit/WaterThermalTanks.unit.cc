// EnergyPlus::WaterThermalTank Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <WaterThermalTanks.hh>
#include <EnergyPlus/UtilityRoutines.hh>

using namespace EnergyPlus;

TEST( HeatPumpWaterHeaterTests, TestQsourceCalcs )
{
	ShowMessage( "Begin Test: HeatPumpWaterHeaterTests, TestQsourceCalcs" );
	Real64 DeltaT = 0.0;
	Real64 const SourceInletTemp = 62.0;
	Real64 const Cp = 4178.; // water, J/(kg * K)
	Real64 const SetPointTemp = 60.0;
	Real64 const SourceMassFlowRateOrig = 0.378529822165; // water, 6 gal/min
	Real64 SourceMassFlowRate = SourceMassFlowRateOrig;
	Real64 Qheatpump = 0.0;
	Real64 Qsource = 0.0;
	
	// Mixed Tank
	
	// Test case without HPWH
	WaterThermalTanks::CalcMixedTankSourceSideHeatTransferRate(DeltaT, SourceInletTemp, Cp, SetPointTemp, SourceMassFlowRate, Qheatpump, Qsource);
	// Qsource is non zero and calculated relative to the tank setpoint.
	EXPECT_DOUBLE_EQ(SourceMassFlowRate * Cp * (SourceInletTemp - SetPointTemp), Qsource);
	// Qheatpump is zero
	EXPECT_DOUBLE_EQ(Qheatpump, 0.0);
	// SourceMassFlowRate is unchanged
	EXPECT_DOUBLE_EQ(SourceMassFlowRateOrig, SourceMassFlowRate);
	
	// Test case with HPWH
	DeltaT = 5.0;
	WaterThermalTanks::CalcMixedTankSourceSideHeatTransferRate(DeltaT, SourceInletTemp, Cp, SetPointTemp, SourceMassFlowRate, Qheatpump, Qsource);
	// Qsource is Qheatpump
	EXPECT_DOUBLE_EQ(Qsource, Qheatpump);
	// Qheatpump is the heat transfer rate from the input DeltaT
	EXPECT_DOUBLE_EQ(SourceMassFlowRateOrig * Cp * DeltaT, Qheatpump);
	// SourceMassFlowRate is zero
	EXPECT_DOUBLE_EQ(SourceMassFlowRate, 0.0);
	
	// Stratified Tank
	SourceMassFlowRate = SourceMassFlowRateOrig;
	Real64 const NodeTemp = 58.0;
	
	WaterThermalTanks::StratifiedNodeData StratNode;
	StratNode.Temp = 58.0;
	StratNode.HPWHWrappedCondenserHeatingFrac = 0.5;
	
	// Test case without HPWH
	Qheatpump = 0.0;
	Qsource = WaterThermalTanks::CalcStratifiedTankSourceSideHeatTransferRate(Qheatpump, SourceInletTemp, Cp, SourceMassFlowRate, StratNode);
	EXPECT_DOUBLE_EQ(Qsource, SourceMassFlowRate * Cp * (SourceInletTemp - NodeTemp));
	
	// Test case with Pumped HPWH
	Qheatpump = 100.0;
	Qsource = WaterThermalTanks::CalcStratifiedTankSourceSideHeatTransferRate(Qheatpump, SourceInletTemp, Cp, SourceMassFlowRate, StratNode);
	EXPECT_DOUBLE_EQ(Qsource, Qheatpump);
	
	// Test case with Wrapped HPWH
	SourceMassFlowRate = 0.0;
	Qsource = WaterThermalTanks::CalcStratifiedTankSourceSideHeatTransferRate(Qheatpump, SourceInletTemp, Cp, SourceMassFlowRate, StratNode);
	EXPECT_DOUBLE_EQ(Qsource, Qheatpump * StratNode.HPWHWrappedCondenserHeatingFrac );
	
}

TEST( WaterThermalTankData, GetDeadBandTemp )
{

	ShowMessage( "Begin Test: WaterThermalTankData, GetDeadBandTemp" );
	WaterThermalTanks::WaterThermalTankData thisTank;
	thisTank.SetPointTemp = 10;
	thisTank.DeadBandDeltaTemp = 1;

	// first the hot water tank
	thisTank.IsChilledWaterTank = false;
	EXPECT_DOUBLE_EQ( 9.0,  thisTank.getDeadBandTemp() );

	// then the chilled water tank
	thisTank.IsChilledWaterTank = true;
	EXPECT_DOUBLE_EQ( 11.0, thisTank.getDeadBandTemp() );

}
