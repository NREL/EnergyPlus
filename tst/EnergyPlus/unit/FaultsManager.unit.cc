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

// EnergyPlus::FaultManager unit tests
// Fouling Air Filter

// Google test headers
#include <gtest/gtest.h>

// C++ Headers
#include <cassert>
#include <cmath>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <CurveManager.hh>
#include <Fans.hh>
#include <FaultsManager.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace FaultsManager;
using namespace CurveManager;
using namespace Fans;

TEST_F( EnergyPlusFixture, FaultsManager_FaultFoulingAirFilters_CheckFaultyAirFilterFanCurve )
{
	// PURPOSE OF THIS SUBROUTINE:
	// To check whether the fan curve specified in the FaultModel:Fouling:AirFilter object
	// covers the rated operational point of the corresponding fan
	// Return true if the curve covers the fan rated operational point

	int CurveNum;
	int FanNum;
	bool TestRestult;

	// Allocate
	NumCurves = 1;
	PerfCurve.allocate( NumCurves );

	NumFans = 2;
	Fan.allocate( NumFans );

	// Inputs: fan curve
	CurveNum = 1;
	PerfCurve( CurveNum ).CurveType = Cubic;
	PerfCurve( CurveNum ).ObjectType = CurveType_Cubic;
	PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;
	PerfCurve( CurveNum ).Coeff1 = 1151.1;
	PerfCurve( CurveNum ).Coeff2 = 13.509;
	PerfCurve( CurveNum ).Coeff3 = -0.9105;
	PerfCurve( CurveNum ).Coeff4 = -0.0129;
	PerfCurve( CurveNum ).Coeff5 = 0.0;
	PerfCurve( CurveNum ).Coeff6 = 0.0;
	PerfCurve( CurveNum ).Var1Min = 7.0;
	PerfCurve( CurveNum ).Var1Max = 21.0;

	// Inputs: fans
	FanNum = 1;
	Fan( FanNum ).FanName = "Fan_1";
	Fan( FanNum ).FanType = "Fan:VariableVolume";
	Fan( FanNum ).MaxAirFlowRate = 18.194;
	Fan( FanNum ).DeltaPress = 1017.59;

	FanNum = 2;
	Fan( FanNum ).FanName = "Fan_2";
	Fan( FanNum ).FanType = "Fan:VariableVolume";
	Fan( FanNum ).MaxAirFlowRate = 18.194;
	Fan( FanNum ).DeltaPress = 1017.59 * 1.2;

	// Run and Check
	// (1)The rated operational point of Fan_1 falls on the fan curve
	TestRestult = CheckFaultyAirFilterFanCurve( "Fan_1", CurveNum );
	EXPECT_TRUE( TestRestult );
	// (2)The rated operational point of Fan_2 does not fall on the fan curve
	TestRestult = CheckFaultyAirFilterFanCurve( "Fan_2", CurveNum );
	EXPECT_FALSE( TestRestult );

	// Clean up
	PerfCurve.deallocate();
	Fan.deallocate();

}

TEST_F( EnergyPlusFixture, FaultsManager_FaultFoulingAirFilters_CalFaultyFanAirFlowReduction )
{
	// PURPOSE OF THIS SUBROUTINE:
	// Calculate the decrease of the fan air flow rate, given the fan curve
	// and the increase of fan pressure rise due to fouling air filters

	int CurveNum;
	int FanNum;
	double FanDesignFlowRateDec;
	double FanFaultyDeltaPressInc = 0.10; // Increase by 10%

	// Allocate
	NumCurves = 1;
	PerfCurve.allocate( NumCurves );

	NumFans = 1;
	Fan.allocate( NumFans );

	// Inputs: fan curve
	CurveNum = 1;
	PerfCurve( CurveNum ).CurveType = Cubic;
	PerfCurve( CurveNum ).ObjectType = CurveType_Cubic;
	PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;
	PerfCurve( CurveNum ).Coeff1 = 1151.1;
	PerfCurve( CurveNum ).Coeff2 = 13.509;
	PerfCurve( CurveNum ).Coeff3 = -0.9105;
	PerfCurve( CurveNum ).Coeff4 = -0.0129;
	PerfCurve( CurveNum ).Coeff5 = 0.0;
	PerfCurve( CurveNum ).Coeff6 = 0.0;
	PerfCurve( CurveNum ).Var1Min = 7.0;
	PerfCurve( CurveNum ).Var1Max = 21.0;

	// Inputs: fans
	FanNum = 1;
	Fan( FanNum ).FanName = "Fan_1";
	Fan( FanNum ).FanType = "Fan:VariableVolume";
	Fan( FanNum ).MaxAirFlowRate = 18.194;
	Fan( FanNum ).DeltaPress = 1017.59;

	// Run and Check
    FanDesignFlowRateDec = CalFaultyFanAirFlowReduction( Fan( FanNum ).FanName, Fan( FanNum ).MaxAirFlowRate, Fan( FanNum ).DeltaPress,
                           FanFaultyDeltaPressInc * Fan( FanNum ).DeltaPress, CurveNum );

	EXPECT_NEAR( 3.845, FanDesignFlowRateDec, 0.005 );

	// Clean up
	PerfCurve.deallocate();
	Fan.deallocate();

}
