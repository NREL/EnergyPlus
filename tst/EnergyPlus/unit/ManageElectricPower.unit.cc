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

// EnergyPlus::ExteriorEnergyUse Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/ExteriorEnergyUse.hh>
#include <EnergyPlus/ManageElectricPower.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace EnergyPlus::ManageElectricPower;
using namespace EnergyPlus::CurveManager;
using namespace ObjexxFCL;
using namespace DataGlobals;

TEST_F( EnergyPlusFixture, ManageElectricPowerTest_BatteryDischargeTest )
{

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

TEST_F( EnergyPlusFixture, ManageElectricPowerTest_UpdateLoadCenterRecords )
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

	ElecStorage.deallocate();
	ElecLoadCenter.deallocate();
}
