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

// EnergyPlus::DXCoils unit tests
// Secondary DX cooling and heating coil heat rejection and removal rate
// from a secondary zone and SHR calculation

// Google test headers
#include <gtest/gtest.h>

// C++ Headers
#include <cassert>
#include <cmath>
#include <string>

// EnergyPlus Headers
#include <DXCoils.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHVACGlobals.hh>
#include <DataLoopNode.hh>
#include <Psychrometrics.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace DXCoils;
using namespace DataHVACGlobals;
using DataLoopNode::Node;
using DataEnvironment::OutBaroPress;
using Psychrometrics::PsyRhoAirFnPbTdbW;
using Psychrometrics::PsyTwbFnTdbWPb;
using Psychrometrics::PsyHFnTdbW;
using Psychrometrics::InitializePsychRoutines;
using DataHeatBalFanSys::ZoneAirHumRat;
using DataHeatBalFanSys::ZT;

TEST_F( EnergyPlusFixture, SecondaryDXCoolingCoilSingleSpeed_Test1 ) {
	// tests secondary DX coil calculation of single speed DX system or heat pump
	int DXCoilNum;

	NumDXCoils = 1;
	DXCoilNum = 1;
	DXCoil.allocate( NumDXCoils );
	DXCoil( DXCoilNum ).IsSecondaryDXCoilInZone = true;
	DXCoil( DXCoilNum ).DXCoilType_Num = CoilDX_CoolingSingleSpeed;
	DXCoil( DXCoilNum ).TotalCoolingEnergyRate = 5000.0;
	DXCoil( DXCoilNum ).ElecCoolingPower = 500.0;
	DXCoil( DXCoilNum ).SecCoilSensibleHeatGainRate = 0.0;

	CalcSecondaryDXCoils( DXCoilNum );
	EXPECT_DOUBLE_EQ( 5500.0, DXCoil( DXCoilNum ).SecCoilSensibleHeatGainRate );

	// cleanup
	DXCoil.deallocate();
}
TEST_F( EnergyPlusFixture, SecondaryDXCoolingCoilTwoSpeed_Test2 ) {

	// tests secondary DX coil calculation of two speed DX cooling system
	int DXCoilNum;

	NumDXCoils = 1;
	DXCoilNum = 1;
	DXCoil.allocate( NumDXCoils );
	DXCoil( DXCoilNum ).IsSecondaryDXCoilInZone = true;
	DXCoil( DXCoilNum ).DXCoilType_Num = CoilDX_CoolingTwoSpeed;
	DXCoil( DXCoilNum ).TotalCoolingEnergyRate = 5000.0;
	DXCoil( DXCoilNum ).ElecCoolingPower = 500.0;
	DXCoil( DXCoilNum ).SecCoilSensibleHeatGainRate = 0.0;

	CalcSecondaryDXCoils( DXCoilNum );
	EXPECT_DOUBLE_EQ( 5500.0, DXCoil( DXCoilNum ).SecCoilSensibleHeatGainRate );

	// cleanup
	DXCoil.deallocate();
}
TEST_F( EnergyPlusFixture, SecondaryDXCoolingCoilMultiSpeed_Test3 ) {

	// tests secondary DX coil calculation of multi speed heat pump
	int DXCoilNum;

	NumDXCoils = 1;
	DXCoilNum = 1;
	DXCoil.allocate( NumDXCoils );
	DXCoil( DXCoilNum ).IsSecondaryDXCoilInZone = true;
	DXCoil( DXCoilNum ).DXCoilType_Num = CoilDX_MultiSpeedCooling;
	DXCoil( DXCoilNum ).TotalCoolingEnergyRate = 5000.0;
	DXCoil( DXCoilNum ).ElecCoolingPower = 500.0;
	DXCoil( DXCoilNum ).SecCoilSensibleHeatGainRate = 0.0;

	CalcSecondaryDXCoils( DXCoilNum );
	EXPECT_DOUBLE_EQ( 5500.0, DXCoil( DXCoilNum ).SecCoilSensibleHeatGainRate );

	// cleanup
	DXCoil.deallocate();

}
TEST_F( EnergyPlusFixture, SecondaryDXHeatingCoilSingleSpeed_Test4 ) {
	// tests secondary DX coil calculation of single speed heat pump
	int DXCoilNum;

	NumDXCoils = 1;
	DXCoilNum = 1;
	DXCoil.allocate( NumDXCoils );
	DXCoil( DXCoilNum ).IsSecondaryDXCoilInZone = true;
	DXCoil( DXCoilNum ).DXCoilType_Num = CoilDX_HeatingEmpirical;
	DXCoil( DXCoilNum ).MinOATCompressor = -5.0;
	DXCoil( DXCoilNum ).TotalHeatingEnergyRate = 5500.0;
	DXCoil( DXCoilNum ).ElecHeatingPower = 500.0;
	DXCoil( DXCoilNum ).SecCoilTotalHeatRemovalRate = 0.0;
	DXCoil( DXCoilNum ).SecCoilSensibleHeatRemovalRate = 0.0;
	DXCoil( DXCoilNum ).SecCoilLatentHeatRemovalRate = 0.0;

	DXCoil( DXCoilNum ).SecZonePtr = 1;
	Node.allocate( 2 );
	ZT.allocate( 1 );
	ZoneAirHumRat.allocate( 1 );
	ZT( 1 ) = 10.0;
	ZoneAirHumRat( 1 ) = 0.003;
	DXCoil( DXCoilNum ).SecCoilAirFlow = 1.0;
	DXCoil( DXCoilNum ).CompressorPartLoadRatio = 1.0;
	DXCoil( DXCoilNum ).SecCoilRatedSHR = 1.0;

	OutBaroPress = 101325.0;
	DXCoil( DXCoilNum ).AirInNode = 2;
	Node( DXCoil( DXCoilNum ).AirInNode ).Temp = 20.0;
	InitializePsychRoutines();

	CalcSecondaryDXCoils( DXCoilNum );
	EXPECT_DOUBLE_EQ( -5000.0, DXCoil( DXCoilNum ).SecCoilTotalHeatRemovalRate );
	EXPECT_DOUBLE_EQ( 1.0, DXCoil( DXCoilNum ).SecCoilSHR );


	//// set up arguments
	Real64 const EvapAirMassFlow = 1.2;
	Real64 const TotalHeatRemovalRate = 5500.0;
	Real64 const PartLoadRatio = 1.0;
	Real64 const SecCoilRatedSHR = 1.0;
	Real64 const EvapInletDryBulb = 10.0;
	Real64 const EvapInletHumRat = 0.003;
	Real64 const EvapInletWetBulb = 4.5;
	Real64 const EvapInletEnthalpy = 17607.0;
	Real64 const CondInletDryBulb = 20.0;
	Real64 const SecCoilFlowFraction = 1.0;
	int const SecCoilSHRFT = 0;
	int const SecCoilSHRFF = 0;

	// output variable
	Real64 SHRTest;

	// make the call
	SHRTest = CalcSecondaryDXCoilsSHR(
	DXCoilNum,
	EvapAirMassFlow,
	TotalHeatRemovalRate,
	PartLoadRatio,
	SecCoilRatedSHR,
	EvapInletDryBulb,
	EvapInletHumRat,
	EvapInletWetBulb,
	EvapInletEnthalpy,
	CondInletDryBulb,
	SecCoilFlowFraction,
	SecCoilSHRFT,
	SecCoilSHRFF );

	EXPECT_DOUBLE_EQ( 1.0, SHRTest );

	// cleanup
	DXCoil.deallocate();
	Node.deallocate();
}
TEST_F( EnergyPlusFixture, SecondaryDXHeatingCoilMultiSpeed_Test5 ) {

	// tests secondary DX coil calculation of multi speed heat pump
	int DXCoilNum;

	NumDXCoils = 1;
	DXCoilNum = 1;
	DXCoil.allocate( NumDXCoils );
	DXCoil( DXCoilNum ).NumOfSpeeds = 2;
	DXCoil( DXCoilNum ).MSSecCoilAirFlow.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
	DXCoil( DXCoilNum ).MSSecCoilRatedSHR.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
	DXCoil( DXCoilNum ).MSSecCoilSHRFT.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
	DXCoil( DXCoilNum ).MSSecCoilSHRFF.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );

	DXCoil( DXCoilNum ).IsSecondaryDXCoilInZone = true;
	DXCoil( DXCoilNum ).DXCoilType_Num = CoilDX_MultiSpeedHeating;
	DXCoil( DXCoilNum ).MinOATCompressor = -5.0;
	DXCoil( DXCoilNum ).TotalHeatingEnergyRate = 5500.0;
	DXCoil( DXCoilNum ).ElecHeatingPower = 500.0;
	DXCoil( DXCoilNum ).SecCoilTotalHeatRemovalRate = 0.0;
	DXCoil( DXCoilNum ).SecCoilSensibleHeatRemovalRate = 0.0;
	DXCoil( DXCoilNum ).SecCoilLatentHeatRemovalRate = 0.0;

	DXCoil( DXCoilNum ).SecZonePtr = 1;
	Node.allocate( 2 );
	ZT.allocate( 1 );
	ZoneAirHumRat.allocate( 1 );
	ZT( 1 ) = 10.0;
	ZoneAirHumRat( 1 ) = 0.003;
	DXCoil( DXCoilNum ).MSSecCoilAirFlow( 1 ) = 1.0;
	DXCoil( DXCoilNum ).MSSecCoilAirFlow( 2 ) = 1.0;
	DXCoil( DXCoilNum ).MSSecCoilSHRFT( 1 ) = 0;
	DXCoil( DXCoilNum ).MSSecCoilSHRFF( 1 ) = 0;
	DXCoil( DXCoilNum ).MSSecCoilSHRFT( 2 ) = 0;
	DXCoil( DXCoilNum ).MSSecCoilSHRFF( 2 ) = 0;
	DXCoil( DXCoilNum ).MSSecCoilRatedSHR( 1 ) = 1.0;
	DXCoil( DXCoilNum ).MSSecCoilRatedSHR( 2 ) = 1.0;

	DXCoil( DXCoilNum ).MSSpeedRatio = 0;
	DXCoil( DXCoilNum ).MSCycRatio = 1;
	DXCoil( DXCoilNum ).MSSpeedNumHS = 1;
	DXCoil( DXCoilNum ).MSSpeedNumLS = 1;

	OutBaroPress = 101325.0;
	DXCoil( DXCoilNum ).AirInNode = 2;
	Node( DXCoil( DXCoilNum ).AirInNode ).Temp = 20.0;
	InitializePsychRoutines();

	CalcSecondaryDXCoils( DXCoilNum );
	EXPECT_DOUBLE_EQ( -5000.0, DXCoil( DXCoilNum ).SecCoilTotalHeatRemovalRate );
	EXPECT_DOUBLE_EQ( 1.0, DXCoil( DXCoilNum ).SecCoilSHR );

	// cleanup
	DXCoil( DXCoilNum ).MSSecCoilAirFlow.deallocate();
	DXCoil( DXCoilNum ).MSSecCoilRatedSHR.deallocate();
	DXCoil( DXCoilNum ).MSSecCoilSHRFT.deallocate();
	DXCoil( DXCoilNum ).MSSecCoilSHRFF.deallocate();
	DXCoil.deallocate();
	Node.deallocate();
}
