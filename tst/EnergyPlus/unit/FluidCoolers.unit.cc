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

// EnergyPlus::FluidCoolers Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

#include "Fixtures/EnergyPlusFixture.hh"

// EnergyPlus Headers
#include <EnergyPlus/FluidCoolers.hh>
#include <DataSizing.hh>
#include <EnergyPlus/UtilityRoutines.hh>


using namespace EnergyPlus;
using namespace EnergyPlus::FluidCoolers;
using namespace DataGlobals;
using namespace EnergyPlus::DataSizing;
using namespace ObjexxFCL;

TEST_F( EnergyPlusFixture, TwoSpeedFluidCoolerInput_Test1 )
{

	using DataSizing::AutoSize;
	int StringArraySize = 20;
	Array1D_string cNumericFieldNames;
	cNumericFieldNames.allocate( StringArraySize );
	Array1D_string cAlphaFieldNames;
	cAlphaFieldNames.allocate( StringArraySize );
	Array1D_string AlphArray;
	AlphArray.allocate( StringArraySize );
	for ( int i = 1; i <= StringArraySize; ++i ) {
		cAlphaFieldNames( i ) = "AlphaField";
		cNumericFieldNames( i ) = "NumerField";
		AlphArray( i ) = "FieldValues";
	}
	std::string const cCurrentModuleObject( "FluidCooler:TwoSpeed" );
	int FluidCoolerNum( 1 );
	SimpleFluidCooler.allocate( FluidCoolerNum );

	SimpleFluidCooler( FluidCoolerNum ).Name = "Test";
	SimpleFluidCooler( FluidCoolerNum ).FluidCoolerMassFlowRateMultiplier = 2.5;
	SimpleFluidCooler( FluidCoolerNum ).PerformanceInputMethod_Num = PIM_NominalCapacity;
	SimpleFluidCooler( FluidCoolerNum ).WaterInletNodeNum = 1;
	SimpleFluidCooler( FluidCoolerNum ).WaterOutletNodeNum = 1;
	SimpleFluidCooler( FluidCoolerNum ).FluidCoolerNominalCapacity = 50000;
	SimpleFluidCooler( FluidCoolerNum ).DesignEnteringWaterTemp = 52;
	SimpleFluidCooler( FluidCoolerNum ).DesignEnteringAirTemp = 35;
	SimpleFluidCooler( FluidCoolerNum ).DesignEnteringAirWetBulbTemp = 25;
	SimpleFluidCooler( FluidCoolerNum ).DesignWaterFlowRate = AutoSize;
	SimpleFluidCooler( FluidCoolerNum ).DesignWaterFlowRateWasAutoSized = true;
	SimpleFluidCooler( FluidCoolerNum ).HighSpeedAirFlowRate = AutoSize;
	SimpleFluidCooler( FluidCoolerNum ).HighSpeedAirFlowRateWasAutoSized = true;
	SimpleFluidCooler( FluidCoolerNum ).HighSpeedFanPower = AutoSize;
	SimpleFluidCooler( FluidCoolerNum ).HighSpeedFanPowerWasAutoSized = true;
	SimpleFluidCooler( FluidCoolerNum ).LowSpeedAirFlowRate = AutoSize;
	SimpleFluidCooler( FluidCoolerNum ).LowSpeedAirFlowRateWasAutoSized = true;
	SimpleFluidCooler( FluidCoolerNum ).LowSpeedFanPower = AutoSize;
	SimpleFluidCooler( FluidCoolerNum ).LowSpeedFanPowerWasAutoSized = true;
	SimpleFluidCooler( FluidCoolerNum ).FluidCoolerLowSpeedNomCap = 30000;


	AlphArray( 4 ) = "NominalCapacity";
	SimpleFluidCooler( FluidCoolerNum ).HighSpeedFluidCoolerUA = 0;
	SimpleFluidCooler( FluidCoolerNum ).LowSpeedFluidCoolerUA = 0;
	SimpleFluidCooler( 1 ).DesignEnteringWaterTemp = 50;
	bool testResult = TestFluidCoolerTwoSpeedInputForDesign( cCurrentModuleObject, AlphArray, cNumericFieldNames, cAlphaFieldNames, FluidCoolerNum );
	EXPECT_FALSE( testResult ); // no error message triggered

	SimpleFluidCooler( 1 ).DesignEnteringWaterTemp = -10;
	testResult = TestFluidCoolerTwoSpeedInputForDesign( cCurrentModuleObject, AlphArray, cNumericFieldNames, cAlphaFieldNames, FluidCoolerNum );
	EXPECT_TRUE( testResult ); // error message triggered

	SimpleFluidCooler( 1 ).DesignEnteringWaterTemp = 50;
	SimpleFluidCooler( 1 ).FluidCoolerLowSpeedNomCap = AutoSize;
	SimpleFluidCooler( 1 ).FluidCoolerLowSpeedNomCapWasAutoSized = true;
	testResult = TestFluidCoolerTwoSpeedInputForDesign( cCurrentModuleObject, AlphArray, cNumericFieldNames, cAlphaFieldNames, FluidCoolerNum );
	EXPECT_FALSE( testResult ); // no error message triggered

	SimpleFluidCooler( 1 ).FluidCoolerLowSpeedNomCap = 0; // this should trigger the original error condition
	SimpleFluidCooler( 1 ).FluidCoolerLowSpeedNomCapWasAutoSized = false;
	testResult = TestFluidCoolerTwoSpeedInputForDesign( cCurrentModuleObject, AlphArray, cNumericFieldNames, cAlphaFieldNames, FluidCoolerNum );
	EXPECT_TRUE( testResult ); // error message triggered

	SimpleFluidCooler.deallocate();
}

TEST_F( EnergyPlusFixture, TwoSpeedFluidCoolerInput_Test2 ) {

	using DataSizing::AutoSize;
	int StringArraySize = 20;
	Array1D_string cNumericFieldNames;
	cNumericFieldNames.allocate( StringArraySize );
	Array1D_string cAlphaFieldNames;
	cAlphaFieldNames.allocate( StringArraySize );
	Array1D_string AlphArray;
	AlphArray.allocate( StringArraySize );
	for ( int i = 1; i <= StringArraySize; ++i ) {
		cAlphaFieldNames( i ) = "AlphaField";
		cNumericFieldNames( i ) = "NumerField";
		AlphArray( i ) = "FieldValues";
	}
	std::string const cCurrentModuleObject( "FluidCooler:TwoSpeed" );
	int FluidCoolerNum( 1 );
	bool ErrrorsFound( false );
	SimpleFluidCooler.allocate( FluidCoolerNum );

	SimpleFluidCooler( FluidCoolerNum ).Name = "Test";
	SimpleFluidCooler( FluidCoolerNum ).FluidCoolerMassFlowRateMultiplier = 1.0;
	SimpleFluidCooler( FluidCoolerNum ).PerformanceInputMethod_Num = PIM_UFactor;
	SimpleFluidCooler( FluidCoolerNum ).DesignEnteringWaterTemp = 52;
	SimpleFluidCooler( FluidCoolerNum ).DesignEnteringAirTemp = 35;
	SimpleFluidCooler( FluidCoolerNum ).DesignEnteringAirWetBulbTemp = 25;
	SimpleFluidCooler( FluidCoolerNum ).DesignWaterFlowRate = AutoSize;
	SimpleFluidCooler( FluidCoolerNum ).DesignWaterFlowRateWasAutoSized = true;
	SimpleFluidCooler( FluidCoolerNum ).HighSpeedAirFlowRate = AutoSize;
	SimpleFluidCooler( FluidCoolerNum ).HighSpeedAirFlowRateWasAutoSized = true;
	SimpleFluidCooler( FluidCoolerNum ).HighSpeedFanPower = AutoSize;
	SimpleFluidCooler( FluidCoolerNum ).HighSpeedFanPowerWasAutoSized = true;
	SimpleFluidCooler( FluidCoolerNum ).LowSpeedAirFlowRate = AutoSize;
	SimpleFluidCooler( FluidCoolerNum ).LowSpeedAirFlowRateWasAutoSized = true;
	SimpleFluidCooler( FluidCoolerNum ).LowSpeedFanPower = AutoSize;
	SimpleFluidCooler( FluidCoolerNum ).LowSpeedFanPowerWasAutoSized = true;
	SimpleFluidCooler( FluidCoolerNum ).FluidCoolerLowSpeedNomCap = 30000;
	SimpleFluidCooler( FluidCoolerNum ).LowSpeedFluidCoolerUA = AutoSize;
	SimpleFluidCooler( FluidCoolerNum ).LowSpeedFluidCoolerUAWasAutoSized = true;

	AlphArray( 4 ) = "UFactorTimesAreaAndDesignWaterFlowRate";
	SimpleFluidCooler( FluidCoolerNum ).HighSpeedFluidCoolerUA = AutoSize;
	SimpleFluidCooler( FluidCoolerNum ).HighSpeedFluidCoolerUAWasAutoSized = false;
	bool testResult = TestFluidCoolerTwoSpeedInputForDesign( cCurrentModuleObject, AlphArray, cNumericFieldNames, cAlphaFieldNames, FluidCoolerNum );
	EXPECT_TRUE( testResult ); // error message triggered

	ErrrorsFound = false;
	SimpleFluidCooler( FluidCoolerNum ).HighSpeedFluidCoolerUA = AutoSize;
	SimpleFluidCooler( FluidCoolerNum ).HighSpeedFluidCoolerUAWasAutoSized = true;
	testResult = TestFluidCoolerTwoSpeedInputForDesign( cCurrentModuleObject, AlphArray, cNumericFieldNames, cAlphaFieldNames, FluidCoolerNum );
	EXPECT_FALSE( testResult ); // no error message triggered

	SimpleFluidCooler.deallocate();
	cNumericFieldNames.deallocate();
	cAlphaFieldNames.deallocate();
	AlphArray.deallocate();
}


TEST_F( EnergyPlusFixture, SingleSpeedFluidCoolerInput_Test3 )
{
	using DataSizing::AutoSize;
	int StringArraySize = 20;
	Array1D_string cNumericFieldNames;
	cNumericFieldNames.allocate( StringArraySize );
	Array1D_string cAlphaFieldNames;
	cAlphaFieldNames.allocate( StringArraySize );
	Array1D_string AlphArray;
	AlphArray.allocate( StringArraySize );
	for ( int i = 1; i <= StringArraySize; ++i ) {
		cAlphaFieldNames( i ) = "AlphaField";
		cNumericFieldNames( i ) = "NumerField";
		AlphArray( i ) = "FieldValues";
	}
	std::string const cCurrentModuleObject( "FluidCooler:SingleSpeed" );
	int FluidCoolerNum( 1 );
	SimpleFluidCooler.allocate( FluidCoolerNum );

	SimpleFluidCooler( FluidCoolerNum ).Name = "Test";
	SimpleFluidCooler( FluidCoolerNum ).FluidCoolerMassFlowRateMultiplier = 2.5;
	SimpleFluidCooler( FluidCoolerNum ).PerformanceInputMethod_Num = PIM_UFactor;
	SimpleFluidCooler( FluidCoolerNum ).WaterInletNodeNum = 1;
	SimpleFluidCooler( FluidCoolerNum ).WaterOutletNodeNum = 1;
	SimpleFluidCooler( FluidCoolerNum ).FluidCoolerNominalCapacity = 50000;
	SimpleFluidCooler( FluidCoolerNum ).DesignEnteringWaterTemp = 52;
	SimpleFluidCooler( FluidCoolerNum ).DesignEnteringAirTemp = 35;
	SimpleFluidCooler( FluidCoolerNum ).DesignEnteringAirWetBulbTemp = 25;
	SimpleFluidCooler( FluidCoolerNum ).HighSpeedAirFlowRate = AutoSize;
	SimpleFluidCooler( FluidCoolerNum ).HighSpeedAirFlowRateWasAutoSized = true;
	SimpleFluidCooler( FluidCoolerNum ).HighSpeedFanPower = AutoSize;
	SimpleFluidCooler( FluidCoolerNum ).HighSpeedFanPowerWasAutoSized = true;
	SimpleFluidCooler( FluidCoolerNum ).HighSpeedFluidCoolerUA = AutoSize;
	SimpleFluidCooler( FluidCoolerNum ).HighSpeedFluidCoolerUAWasAutoSized = true;

	AlphArray( 4 ) = "UFactorTimesAreaAndDesignWaterFlowRate";
	SimpleFluidCooler( FluidCoolerNum ).DesignWaterFlowRateWasAutoSized = true;
	SimpleFluidCooler( FluidCoolerNum ).DesignWaterFlowRate = 1;
	bool testResult = TestFluidCoolerSingleSpeedInputForDesign( cCurrentModuleObject, AlphArray, cNumericFieldNames, cAlphaFieldNames, FluidCoolerNum );
	EXPECT_FALSE( testResult ); // no error message triggered

	SimpleFluidCooler( FluidCoolerNum ).DesignWaterFlowRateWasAutoSized = true;
	SimpleFluidCooler( FluidCoolerNum ).DesignWaterFlowRate = 0;
	testResult = TestFluidCoolerSingleSpeedInputForDesign( cCurrentModuleObject, AlphArray, cNumericFieldNames, cAlphaFieldNames, FluidCoolerNum );
	EXPECT_FALSE( testResult ); // no error message triggered

	SimpleFluidCooler( FluidCoolerNum ).DesignWaterFlowRateWasAutoSized = false;
	SimpleFluidCooler( FluidCoolerNum ).DesignWaterFlowRate = 1;
	testResult = TestFluidCoolerSingleSpeedInputForDesign( cCurrentModuleObject, AlphArray, cNumericFieldNames, cAlphaFieldNames, FluidCoolerNum );
	EXPECT_FALSE( testResult ); // no error message triggered

	SimpleFluidCooler( FluidCoolerNum ).DesignWaterFlowRateWasAutoSized = false;
	SimpleFluidCooler( FluidCoolerNum ).DesignWaterFlowRate = 0;
	testResult = TestFluidCoolerSingleSpeedInputForDesign( cCurrentModuleObject, AlphArray, cNumericFieldNames, cAlphaFieldNames, FluidCoolerNum );
	EXPECT_TRUE( testResult ); // error message triggered

}
