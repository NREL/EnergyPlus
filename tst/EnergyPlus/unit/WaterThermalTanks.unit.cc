// EnergyPlus::WaterThermalTank Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <WaterThermalTanks.hh>

using namespace EnergyPlus;

bool FloatEqualTest(Real64 val1, Real64 val2, Real64 SmallNum = 0.00000001) {
	using namespace std;
	return ( abs(val1 - val2) < SmallNum );
}

TEST( HeatPumpWaterHeaterTests, TestQsourceCalcs )
{
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
	
	// Test case without HPWH
	DeltaT = 0.0;
	Qsource = WaterThermalTanks::CalcStratifiedTankSourceSideHeatTransferRate(DeltaT, SourceInletTemp, Cp, SourceMassFlowRate, NodeTemp);
	EXPECT_DOUBLE_EQ(Qsource, SourceMassFlowRate * Cp * (SourceInletTemp - NodeTemp));
	
	// Test case with HPWH
	DeltaT = 5.0;
	Qsource = WaterThermalTanks::CalcStratifiedTankSourceSideHeatTransferRate(DeltaT, SourceInletTemp, Cp, SourceMassFlowRate, NodeTemp);
	EXPECT_DOUBLE_EQ(Qsource, SourceMassFlowRate * Cp * DeltaT);
	
}
