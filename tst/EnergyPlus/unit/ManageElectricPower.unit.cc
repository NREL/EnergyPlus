// EnergyPlus::ExteriorEnergyUse Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/ExteriorEnergyUse.hh>
#include <EnergyPlus/ManageElectricPower.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::ManageElectricPower;
using namespace EnergyPlus::CurveManager;
using namespace ObjexxFCL;
using namespace DataGlobals;

TEST( ManageElectricPowerTest, BatteryDischargeTest )
{
	ShowMessage( "Begin Test: ManageElectricPowerTest, BatteryDischargeTest" );

	NumCurves = 1;
	PerfCurve.allocate( NumCurves );
	PerfCurve( 1 ).CurveType = CurveType_RectangularHyperbola1;
	PerfCurve( 1 ).InterpolationType = EvaluateCurveToLimits;
	PerfCurve( 1 ).Coeff1 = 0.0899;
	PerfCurve( 1 ).Coeff2 = -98.24;
	PerfCurve( 1 ).Coeff3 = -.0082;
	int CurveNum1 = 1;
	Real64 k = 0.5874;
	Real64 c = 0.37;
	Real64 qmax = 86.1;
	Real64 E0c = 12.6;
	Real64 InternalR = 0.054;

	Real64 I0 = 0.159;
	Real64 T0 = 537.9;
	Real64 Volt = 12.59;
	Real64 Pw = 2.0;
	Real64 q0 = 60.2;

	EXPECT_TRUE( determineCurrentForBatteryDischarge( I0, T0, Volt, Pw, q0, CurveNum1, k, c, qmax, E0c, InternalR ) );

	I0 = -222.7;
	T0 = -0.145;
	Volt = 24.54;
	Pw = 48000;
	q0 = 0;

	EXPECT_FALSE( determineCurrentForBatteryDischarge( I0, T0, Volt, Pw, q0, CurveNum1, k, c, qmax, E0c, InternalR ) );

	PerfCurve.deallocate();
}

TEST( ManageElectricPowerTest, UpdateLoadCenterRecords )
{
	ShowMessage( "Begin Test: ManageElectricPowerTest, UpdateLoadCenterRecords" );

	NumLoadCenters = 1;
	int LoadCenterNum( 1 );
	ElecLoadCenter.allocate( NumLoadCenters );
	ElecLoadCenter( LoadCenterNum ).OperationScheme = 0;
	ElecLoadCenter( LoadCenterNum ).DemandMeterPtr = 0;
	ElecLoadCenter( LoadCenterNum ).NumGenerators = 2;
	ElecLoadCenter( LoadCenterNum ).ElecGen.allocate( ElecLoadCenter( LoadCenterNum ).NumGenerators );
	ElecLoadCenter( LoadCenterNum ).TrackSchedPtr = 0;
	ElecLoadCenter( LoadCenterNum ).ElectricityProd = 0.0;
	ElecLoadCenter( LoadCenterNum ).ElectProdRate = 0.0;
	ElecLoadCenter( LoadCenterNum ).ThermalProd = 0.0;
	ElecLoadCenter( LoadCenterNum ).ThermalProdRate = 0.0;


	// Case 1 ACBuss - Generators 1000+2000=3000, thermal 500+750=1250
	ElecLoadCenter( LoadCenterNum ).BussType = ( ACBuss );
	ElecLoadCenter( LoadCenterNum ).ElecGen( 1 ).ElectProdRate = 1000.0;
	ElecLoadCenter( LoadCenterNum ).ElecGen( 2 ).ElectProdRate = 2000.0;
	ElecLoadCenter( LoadCenterNum ).ElecGen( 1 ).ElectricityProd = 1000.0*3600.0;
	ElecLoadCenter( LoadCenterNum ).ElecGen( 2 ).ElectricityProd = 2000.0*3600.0;
	ElecLoadCenter( LoadCenterNum ).ElecGen( 1 ).ThermalProdRate = 500.0;
	ElecLoadCenter( LoadCenterNum ).ElecGen( 2 ).ThermalProdRate = 750.0;
	ElecLoadCenter( LoadCenterNum ).ElecGen( 1 ).ThermalProd = 500.0*3600.0;
	ElecLoadCenter( LoadCenterNum ).ElecGen( 2 ).ThermalProd = 750.0*3600.0;
	UpdateLoadCenterRecords( LoadCenterNum );

	EXPECT_NEAR( ElecLoadCenter( LoadCenterNum ).ElectProdRate , 3000.0, 0.1);
	EXPECT_NEAR( ElecLoadCenter( LoadCenterNum ).ElectricityProd, 3000.0*3600.0, 0.1 );
	EXPECT_NEAR( ElecLoadCenter( LoadCenterNum ).ThermalProdRate, 1250.0, 0.1 );
	EXPECT_NEAR( ElecLoadCenter( LoadCenterNum ).ThermalProd, 1250.0*3600.0, 0.1 );

	// reset
	ElecLoadCenter( LoadCenterNum ).ElectricityProd = 0.0;
	ElecLoadCenter( LoadCenterNum ).ElectProdRate = 0.0;

	// Case 2 ACBussStorage - Generators 1000+2000=3000, Storage 200-150=50
	ElecLoadCenter( LoadCenterNum ).BussType = ( ACBussStorage );
	NumElecStorageDevices = 1;
	int StorageNum( 1 );
	ElecLoadCenter( LoadCenterNum ).StorageModelNum = StorageNum;
	ElecStorage.allocate( NumElecStorageDevices );
	ElecStorage( StorageNum ).DrawnPower = 200.0;
	ElecStorage( StorageNum ).StoredPower = 150.0;
	ElecStorage( StorageNum ).DrawnEnergy = 200.0*3600.0;
	ElecStorage( StorageNum ).StoredEnergy = 150.0*3600.0;
	UpdateLoadCenterRecords( LoadCenterNum );

	EXPECT_NEAR( ElecLoadCenter( LoadCenterNum ).ElectProdRate, 3050.0, 0.1 );
	EXPECT_NEAR( ElecLoadCenter( LoadCenterNum ).ElectricityProd, 3050.0*3600.0, 0.1 );

	// reset
	ElecLoadCenter( LoadCenterNum ).ElectricityProd = 0.0;
	ElecLoadCenter( LoadCenterNum ).ElectProdRate = 0.0;

	// Case 3 DCBussInverter   Inverter = 5000,
	ElecLoadCenter( LoadCenterNum ).BussType = ( DCBussInverter );
	NumInverters = 1;
	int InverterNum( 1 );
	ElecLoadCenter( LoadCenterNum ).InverterModelNum = InverterNum;
	Inverter.allocate( NumInverters );
	Inverter( InverterNum ).ACPowerOut = 5000.0;
	Inverter( InverterNum ).ACEnergyOut = 5000.0*3600.0;
	UpdateLoadCenterRecords( LoadCenterNum );

	EXPECT_NEAR( ElecLoadCenter( LoadCenterNum ).ElectProdRate, 5000.0, 0.1 );
	EXPECT_NEAR( ElecLoadCenter( LoadCenterNum ).ElectricityProd, 5000.0*3600.0, 0.1 );

	// reset
	ElecLoadCenter( LoadCenterNum ).ElectricityProd = 0.0;
	ElecLoadCenter( LoadCenterNum ).ElectProdRate = 0.0;

	// Case 4 DCBussInverterDCStorage    Inverter = 5000,
	ElecLoadCenter( LoadCenterNum ).BussType = ( DCBussInverterDCStorage );
	UpdateLoadCenterRecords( LoadCenterNum );

	EXPECT_NEAR( ElecLoadCenter( LoadCenterNum ).ElectProdRate, 5000.0, 0.1 );
	EXPECT_NEAR( ElecLoadCenter( LoadCenterNum ).ElectricityProd, 5000.0*3600.0, 0.1 );

	// reset
	ElecLoadCenter( LoadCenterNum ).ElectricityProd = 0.0;
	ElecLoadCenter( LoadCenterNum ).ElectProdRate = 0.0;
	ElecLoadCenter( LoadCenterNum ).ThermalProdRate = 0.0;
	ElecLoadCenter( LoadCenterNum ).ThermalProd = 0.0;

	// Case 5 DCBussInverterACStorage     Inverter = 5000, , Storage 200-150=50, thermal should still be same as Case 1
	ElecLoadCenter( LoadCenterNum ).BussType = ( DCBussInverterACStorage );
	UpdateLoadCenterRecords( LoadCenterNum );

	EXPECT_NEAR( ElecLoadCenter( LoadCenterNum ).ElectProdRate, 5050.0, 0.1 );
	EXPECT_NEAR( ElecLoadCenter( LoadCenterNum ).ElectricityProd, 5050.0*3600.0, 0.1 );
	EXPECT_NEAR( ElecLoadCenter( LoadCenterNum ).ThermalProdRate, 1250.0, 0.1 );
	EXPECT_NEAR( ElecLoadCenter( LoadCenterNum ).ThermalProd, 1250.0*3600.0, 0.1 );

	ElecStorage.deallocate( );
	ElecLoadCenter.deallocate( );
}
