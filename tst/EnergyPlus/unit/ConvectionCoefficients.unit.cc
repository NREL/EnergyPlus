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

// EnergyPlus::ConvectionCoefficients unit tests

// Google test headers
#include <gtest/gtest.h>

// C++ Headers

// EnergyPlus Headers
#include <ConvectionCoefficients.hh>
#include <EnergyPlus/UtilityRoutines.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace ConvectionCoefficients;

TEST_F( EnergyPlusFixture, ConvectionCoefficientsTest_ConvectionCofficients )
{

	Real64 DeltaTemp; // [C] temperature difference between surface and air
	Real64 Height; // [m] characteristic size
	Real64 SurfTemp; // [C] surface temperature
	Real64 SupplyAirTemp; // [C] temperature of supply air into zone
	Real64 AirChangeRate; // [ACH] [1/hour] supply air ACH for zone
	int ZoneNum; // index of zone for messaging
	Real64 Hc;

	DeltaTemp = 1.0; 
	Height = 2.0; 
	SurfTemp = 23.0; 
	SupplyAirTemp = 35.0;
	AirChangeRate = 2.0;
	ZoneNum = 1;

	Hc = CalcBeausoleilMorrisonMixedAssistedWall( DeltaTemp, Height, SurfTemp, SupplyAirTemp, AirChangeRate, ZoneNum ); 
	EXPECT_NEAR( -1.19516, Hc, 0.0001 );

	Hc = CalcBeausoleilMorrisonMixedOpposingWall( DeltaTemp, Height, SurfTemp, SupplyAirTemp, AirChangeRate, ZoneNum );
	EXPECT_NEAR( 1.8378, Hc, 0.0001 );

	Hc = CalcBeausoleilMorrisonMixedStableFloor( DeltaTemp, Height, SurfTemp, SupplyAirTemp, AirChangeRate, ZoneNum );
	EXPECT_NEAR( -4.3290, Hc, 0.0001 );

	Hc = CalcBeausoleilMorrisonMixedUnstableFloor( DeltaTemp, Height, SurfTemp, SupplyAirTemp, AirChangeRate, ZoneNum );
	EXPECT_NEAR( -4.24778, Hc, 0.0001 );

	Hc = CalcBeausoleilMorrisonMixedStableCeiling( DeltaTemp, Height, SurfTemp, SupplyAirTemp, AirChangeRate, ZoneNum );
	EXPECT_NEAR( -8.11959, Hc, 0.0001 );

	Hc = CalcBeausoleilMorrisonMixedUnstableCeiling( DeltaTemp, Height, SurfTemp, SupplyAirTemp, AirChangeRate, ZoneNum );
	EXPECT_NEAR( -8.09685, Hc, 0.0001 );

}


