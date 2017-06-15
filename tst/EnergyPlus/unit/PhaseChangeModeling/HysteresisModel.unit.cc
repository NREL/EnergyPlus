// EnergyPlus, Copyright (c) 1996-2017, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
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

// Google Test Headers
#include <gtest/gtest.h>
#include <EnergyPlus/PhaseChangeModeling/HysteresisModel.hh>

using namespace EnergyPlus;

// A new one of these is created for each test
class HysteresisTest : public testing::Test
{
public:
	HysteresisPhaseChange::HysteresisPhaseChange ModelA;
	virtual void SetUp()
	{
		this->ModelA.name = "PCM Name";
		this->ModelA.totalLatentHeat = 25000;  // J/kg ?
		this->ModelA.specificHeatLiquid = 25000;  // J/kgK
		this->ModelA.deltaTempMeltingHigh = 1.0;  // deltaC
		this->ModelA.peakTempMelting = 27;  // degC
		this->ModelA.deltaTempMeltingLow = 1.0;  // deltaC
		this->ModelA.specificHeatSolid = 20000;  // J/kgK
		this->ModelA.deltaTempFreezingHigh = 1.0;  // deltaC
		this->ModelA.peakTempFreezing = 23;  // degC
		this->ModelA.deltaTempFreezingLow = 1.0;  // deltaC
		this->ModelA.specHeatTransition = ( this->ModelA.specificHeatSolid + this->ModelA.specificHeatLiquid ) / 2.0;
		this->ModelA.CpOld = this->ModelA.specificHeatSolid;
	}

	virtual void TearDown()
	{
	}
};

TEST_F(HysteresisTest, StraightUpCurve)
{
	Real64 phaseChangeTempReverse = 999;

	Real64 prevTempTD = 20;
	Real64 updatedTempTDT = 21;
	int prevPhaseChangeState = HysteresisPhaseChange::PhaseChangeStates::CRYSTALLIZED;
	int phaseChangeState = HysteresisPhaseChange::PhaseChangeStates::CRYSTALLIZED;

	// calculate a new specific heat value, moving from 20 to 21
	Real64 newSpecificHeat = this->ModelA.getCurrentSpecificHeat(prevTempTD, updatedTempTDT, phaseChangeTempReverse, prevPhaseChangeState, phaseChangeState);

	// validate the return value
	EXPECT_NEAR( 20000, newSpecificHeat, 1.0 );

	// each call to the getCurrentSpecificHeat will return a new phase change state, so assert it and then store it
	EXPECT_EQ( 2, phaseChangeState );
	prevPhaseChangeState = phaseChangeState;

	// also store the previous temp for convenience and move updatedTemp to a new state
	prevTempTD = updatedTempTDT;

	// and repeat with different expectations as we move through the curve
	updatedTempTDT += 1;  // now 22
	newSpecificHeat = this->ModelA.getCurrentSpecificHeat(prevTempTD, updatedTempTDT, phaseChangeTempReverse, prevPhaseChangeState, phaseChangeState);
	EXPECT_NEAR( 20001, newSpecificHeat, 1.0 );
	EXPECT_EQ( 2, phaseChangeState );  // CRYSTALLIZED

	prevPhaseChangeState = phaseChangeState;
	prevTempTD = updatedTempTDT;
	updatedTempTDT += 1;  // now 23
	newSpecificHeat = this->ModelA.getCurrentSpecificHeat(prevTempTD, updatedTempTDT, phaseChangeTempReverse, prevPhaseChangeState, phaseChangeState);
	EXPECT_NEAR( 20008, newSpecificHeat, 1.0 );
	EXPECT_EQ( 2, phaseChangeState );  // CRYSTALLIZED

	prevPhaseChangeState = phaseChangeState;
	prevTempTD = updatedTempTDT;
	updatedTempTDT += 1;  // now 23
	newSpecificHeat = this->ModelA.getCurrentSpecificHeat(prevTempTD, updatedTempTDT, phaseChangeTempReverse, prevPhaseChangeState, phaseChangeState);
	EXPECT_NEAR( 20061, newSpecificHeat, 1.0 );
	EXPECT_EQ( 2, phaseChangeState );  // CRYSTALLIZED

	prevPhaseChangeState = phaseChangeState;
	prevTempTD = updatedTempTDT;
	updatedTempTDT += 1;  // now 24
	newSpecificHeat = this->ModelA.getCurrentSpecificHeat(prevTempTD, updatedTempTDT, phaseChangeTempReverse, prevPhaseChangeState, phaseChangeState);
	EXPECT_NEAR( 20457, newSpecificHeat, 1.0 );
	EXPECT_EQ( 2, phaseChangeState );  // CRYSTALLIZED

	prevPhaseChangeState = phaseChangeState;
	prevTempTD = updatedTempTDT;
	updatedTempTDT += 1;  // now 25
	newSpecificHeat = this->ModelA.getCurrentSpecificHeat(prevTempTD, updatedTempTDT, phaseChangeTempReverse, prevPhaseChangeState, phaseChangeState);
	EXPECT_NEAR( 23383, newSpecificHeat, 1.0 );
	EXPECT_EQ( -1, phaseChangeState );  // MELTING

	prevPhaseChangeState = phaseChangeState;
	prevTempTD = updatedTempTDT;
	updatedTempTDT += 1;  // now 26
	newSpecificHeat = this->ModelA.getCurrentSpecificHeat(prevTempTD, updatedTempTDT, phaseChangeTempReverse, prevPhaseChangeState, phaseChangeState);
	EXPECT_NEAR( 30808, newSpecificHeat, 1.0 );
	EXPECT_EQ( -1, phaseChangeState );  // MELTING

	prevPhaseChangeState = phaseChangeState;
	prevTempTD = updatedTempTDT;
	updatedTempTDT += 1;  // now 27
	newSpecificHeat = this->ModelA.getCurrentSpecificHeat(prevTempTD, updatedTempTDT, phaseChangeTempReverse, prevPhaseChangeState, phaseChangeState);
	EXPECT_NEAR( 28383, newSpecificHeat, 1.0 );
	EXPECT_EQ( -1, phaseChangeState );  // MELTING

	prevPhaseChangeState = phaseChangeState;
	prevTempTD = updatedTempTDT;
	updatedTempTDT += 1;  // now 28
	newSpecificHeat = this->ModelA.getCurrentSpecificHeat(prevTempTD, updatedTempTDT, phaseChangeTempReverse, prevPhaseChangeState, phaseChangeState);
	EXPECT_NEAR( 25457, newSpecificHeat, 1.0 );
	EXPECT_EQ( -2, phaseChangeState );  // LIQUID

	prevPhaseChangeState = phaseChangeState;
	prevTempTD = updatedTempTDT;
	updatedTempTDT += 1;  // now 29
	newSpecificHeat = this->ModelA.getCurrentSpecificHeat(prevTempTD, updatedTempTDT, phaseChangeTempReverse, prevPhaseChangeState, phaseChangeState);
	EXPECT_NEAR( 25061, newSpecificHeat, 1.0 );
	EXPECT_EQ( -2, phaseChangeState );  // LIQUID

	prevPhaseChangeState = phaseChangeState;
	prevTempTD = updatedTempTDT;
	updatedTempTDT += 1;  // now 30
	newSpecificHeat = this->ModelA.getCurrentSpecificHeat(prevTempTD, updatedTempTDT, phaseChangeTempReverse, prevPhaseChangeState, phaseChangeState);
	EXPECT_NEAR( 25008, newSpecificHeat, 1.0 );
	EXPECT_EQ( -2, phaseChangeState );  // LIQUID

	prevPhaseChangeState = phaseChangeState;
	prevTempTD = updatedTempTDT;
	updatedTempTDT += 1;  // now 31
	newSpecificHeat = this->ModelA.getCurrentSpecificHeat(prevTempTD, updatedTempTDT, phaseChangeTempReverse, prevPhaseChangeState, phaseChangeState);
	EXPECT_NEAR( 25001, newSpecificHeat, 1.0 );
	EXPECT_EQ( -2, phaseChangeState );  // LIQUID

	prevPhaseChangeState = phaseChangeState;
	prevTempTD = updatedTempTDT;
	updatedTempTDT += 1;  // now 32
	newSpecificHeat = this->ModelA.getCurrentSpecificHeat(prevTempTD, updatedTempTDT, phaseChangeTempReverse, prevPhaseChangeState, phaseChangeState);
	EXPECT_NEAR( 25000, newSpecificHeat, 1.0 );
	EXPECT_EQ( -2, phaseChangeState );  // LIQUID
}


TEST_F(HysteresisTest, StraightDownCurve)
{
	Real64 phaseChangeTempReverse = 999;

	Real64 prevTempTD = 32;
	Real64 updatedTempTDT = 31;
	int prevPhaseChangeState = HysteresisPhaseChange::PhaseChangeStates::LIQUID;
	int phaseChangeState = HysteresisPhaseChange::PhaseChangeStates::LIQUID;

	// calculate a new specific heat value, moving from 32 to 31
	Real64 newSpecificHeat = this->ModelA.getCurrentSpecificHeat(prevTempTD, updatedTempTDT, phaseChangeTempReverse, prevPhaseChangeState, phaseChangeState);

	// validate the return value
	EXPECT_NEAR( 25000, newSpecificHeat, 1.0 );

	// each call to the getCurrentSpecificHeat will return a new phase change state, so assert it and then store it
	EXPECT_EQ( -2, phaseChangeState );  // LIQUID
	prevPhaseChangeState = phaseChangeState;

	// also store the previous temp for convenience and move updatedTemp to a new state
	prevTempTD = updatedTempTDT;

	// and repeat with different expectations as we move through the curve
	updatedTempTDT -= 1;  // now 30
	newSpecificHeat = this->ModelA.getCurrentSpecificHeat(prevTempTD, updatedTempTDT, phaseChangeTempReverse, prevPhaseChangeState, phaseChangeState);
	EXPECT_NEAR( 25000, newSpecificHeat, 1.0 );
	EXPECT_EQ( -2, phaseChangeState );  // LIQUID

	prevPhaseChangeState = phaseChangeState;
	prevTempTD = updatedTempTDT;
	updatedTempTDT -= 1;  // now 29
	newSpecificHeat = this->ModelA.getCurrentSpecificHeat(prevTempTD, updatedTempTDT, phaseChangeTempReverse, prevPhaseChangeState, phaseChangeState);
	EXPECT_NEAR( 25000, newSpecificHeat, 1.0 );
	EXPECT_EQ( -2, phaseChangeState );  // LIQUID

	prevPhaseChangeState = phaseChangeState;
	prevTempTD = updatedTempTDT;
	updatedTempTDT -= 1;  // now 28
	newSpecificHeat = this->ModelA.getCurrentSpecificHeat(prevTempTD, updatedTempTDT, phaseChangeTempReverse, prevPhaseChangeState, phaseChangeState);
	EXPECT_NEAR( 25001, newSpecificHeat, 1.0 );
	EXPECT_EQ( -2, phaseChangeState );  // LIQUID

	prevPhaseChangeState = phaseChangeState;
	prevTempTD = updatedTempTDT;
	updatedTempTDT -= 1;  // now 27
	newSpecificHeat = this->ModelA.getCurrentSpecificHeat(prevTempTD, updatedTempTDT, phaseChangeTempReverse, prevPhaseChangeState, phaseChangeState);
	EXPECT_NEAR( 25008, newSpecificHeat, 1.0 );
	EXPECT_EQ( -2, phaseChangeState );  // LIQUID

	prevPhaseChangeState = phaseChangeState;
	prevTempTD = updatedTempTDT;
	updatedTempTDT -= 1;  // now 26
	newSpecificHeat = this->ModelA.getCurrentSpecificHeat(prevTempTD, updatedTempTDT, phaseChangeTempReverse, prevPhaseChangeState, phaseChangeState);
	EXPECT_NEAR( 25061, newSpecificHeat, 1.0 );
	EXPECT_EQ( -2, phaseChangeState );  // LIQUID

	prevPhaseChangeState = phaseChangeState;
	prevTempTD = updatedTempTDT;
	updatedTempTDT -= 1;  // now 25
	newSpecificHeat = this->ModelA.getCurrentSpecificHeat(prevTempTD, updatedTempTDT, phaseChangeTempReverse, prevPhaseChangeState, phaseChangeState);
	EXPECT_NEAR( 25457, newSpecificHeat, 1.0 );
	EXPECT_EQ( -2, phaseChangeState );  // LIQUID

	prevPhaseChangeState = phaseChangeState;
	prevTempTD = updatedTempTDT;
	updatedTempTDT -= 1;  // now 24
	newSpecificHeat = this->ModelA.getCurrentSpecificHeat(prevTempTD, updatedTempTDT, phaseChangeTempReverse, prevPhaseChangeState, phaseChangeState);
	EXPECT_NEAR( 28383, newSpecificHeat, 1.0 );
	EXPECT_EQ( 1, phaseChangeState );  // FREEZING

	prevPhaseChangeState = phaseChangeState;
	prevTempTD = updatedTempTDT;
	updatedTempTDT -= 1;  // now 23
	newSpecificHeat = this->ModelA.getCurrentSpecificHeat(prevTempTD, updatedTempTDT, phaseChangeTempReverse, prevPhaseChangeState, phaseChangeState);
	EXPECT_NEAR( 35808, newSpecificHeat, 1.0 );
	EXPECT_EQ( 1, phaseChangeState );  // FREEZING

	prevPhaseChangeState = phaseChangeState;
	prevTempTD = updatedTempTDT;
	updatedTempTDT -= 1;  // now 22
	newSpecificHeat = this->ModelA.getCurrentSpecificHeat(prevTempTD, updatedTempTDT, phaseChangeTempReverse, prevPhaseChangeState, phaseChangeState);
	EXPECT_NEAR( 23383, newSpecificHeat, 1.0 );
	EXPECT_EQ( 1, phaseChangeState );  // FREEZING

	prevPhaseChangeState = phaseChangeState;
	prevTempTD = updatedTempTDT;
	updatedTempTDT -= 1;  // now 21
	newSpecificHeat = this->ModelA.getCurrentSpecificHeat(prevTempTD, updatedTempTDT, phaseChangeTempReverse, prevPhaseChangeState, phaseChangeState);
	EXPECT_NEAR( 20457, newSpecificHeat, 1.0 );
	EXPECT_EQ( 2, phaseChangeState );  // CRYSTALLIZED

	prevPhaseChangeState = phaseChangeState;
	prevTempTD = updatedTempTDT;
	updatedTempTDT -= 1;  // now 20
	newSpecificHeat = this->ModelA.getCurrentSpecificHeat(prevTempTD, updatedTempTDT, phaseChangeTempReverse, prevPhaseChangeState, phaseChangeState);
	EXPECT_NEAR( 20061, newSpecificHeat, 1.0 );
	EXPECT_EQ( 2, phaseChangeState );  // CRYSTALLIZED

	prevPhaseChangeState = phaseChangeState;
	prevTempTD = updatedTempTDT;
	updatedTempTDT -= 1;  // now 19
	newSpecificHeat = this->ModelA.getCurrentSpecificHeat(prevTempTD, updatedTempTDT, phaseChangeTempReverse, prevPhaseChangeState, phaseChangeState);
	EXPECT_NEAR( 20008, newSpecificHeat, 1.0 );
	EXPECT_EQ( 2, phaseChangeState );  // CRYSTALLIZED

	prevPhaseChangeState = phaseChangeState;
	prevTempTD = updatedTempTDT;
	updatedTempTDT -= 1;  // now 18
	newSpecificHeat = this->ModelA.getCurrentSpecificHeat(prevTempTD, updatedTempTDT, phaseChangeTempReverse, prevPhaseChangeState, phaseChangeState);
	EXPECT_NEAR( 20001, newSpecificHeat, 1.0 );
	EXPECT_EQ( 2, phaseChangeState );  // CRYSTALLIZED

	prevPhaseChangeState = phaseChangeState;
	prevTempTD = updatedTempTDT;
	updatedTempTDT -= 1;  // now 17
	newSpecificHeat = this->ModelA.getCurrentSpecificHeat(prevTempTD, updatedTempTDT, phaseChangeTempReverse, prevPhaseChangeState, phaseChangeState);
	EXPECT_NEAR( 20000, newSpecificHeat, 1.0 );
	EXPECT_EQ( 2, phaseChangeState );  // CRYSTALLIZED
}
