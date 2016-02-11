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

// EnergyPlus::Standalone ERV Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/DataAirflowNetwork.hh>
#include <EnergyPlus/AirflowNetworkBalanceManager.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace EnergyPlus::DataAirflowNetwork;
using namespace EnergyPlus::AirflowNetworkBalanceManager;
using namespace ObjexxFCL;
using namespace EnergyPlus::DataEnvironment;
using DataHeatBalance::Zone;
using DataHeatBalFanSys::MAT;
using DataHeatBalance::MRT;
using namespace CurveManager;
using DataHeatBalance::ZoneIntGain;
using DataHeatBalFanSys::TempControlType;
using DataHeatBalFanSys::ZoneThermostatSetPointLo;
using DataHeatBalFanSys::ZoneThermostatSetPointHi;

TEST_F( EnergyPlusFixture, AdvancedAFNTest_Test1 )
{

	int AirflowNetworkNumOfOccuVentCtrls;
	Real64 TimeOpenElapsed;
	Real64 TimeCloseElapsed;
	int OpenStatus;
	int OpenProbStatus;
	int CloseProbStatus;
	int CurveNum;

	AirflowNetworkNumOfOccuVentCtrls = 1;
	OccupantVentilationControl.allocate( AirflowNetworkNumOfOccuVentCtrls );
	OccupantVentilationControl( 1 ).MinOpeningTime = 4;
	OccupantVentilationControl( 1 ).MinClosingTime = 4;
	OccupantVentilationControl( 1 ).MinTimeControlOnly = true;

	TimeOpenElapsed = 3.0;
	TimeCloseElapsed = 0.0;
	OccupantVentilationControl( 1 ).calc( 1, 2, 0, TimeOpenElapsed, TimeCloseElapsed, OpenStatus, OpenProbStatus, CloseProbStatus );
	EXPECT_EQ( 1, OpenStatus );

	TimeOpenElapsed = 5.0;
	OccupantVentilationControl( 1 ).calc( 1, 2, 0, TimeOpenElapsed, TimeCloseElapsed, OpenStatus, OpenProbStatus, CloseProbStatus );
	EXPECT_EQ( 0, OpenStatus );

	TimeOpenElapsed = 0.0;
	TimeCloseElapsed = 3.0;
	OccupantVentilationControl( 1 ).calc( 1, 2, 0, TimeOpenElapsed, TimeCloseElapsed, OpenStatus, OpenProbStatus, CloseProbStatus );
	EXPECT_EQ( 2, OpenStatus );

	TimeOpenElapsed = 0.0;
	TimeCloseElapsed = 5.0;
	OccupantVentilationControl( 1 ).calc( 1, 2, 0, TimeOpenElapsed, TimeCloseElapsed, OpenStatus, OpenProbStatus, CloseProbStatus );
	EXPECT_EQ( 0, OpenStatus );

	OutDryBulbTemp = 15.0;
	Zone.allocate( 1 );
	MAT.allocate( 1 );
	MRT.allocate( 1 );
	MAT( 1 ) = 22.0;
	MRT( 1 ) = 22.0;

	TimeOpenElapsed = 5.0;
	TimeCloseElapsed = 0.0;
	OccupantVentilationControl( 1 ).MinTimeControlOnly = false;
	OccupantVentilationControl( 1 ).ComfortBouPoint = 10.0;
	OccupantVentilationControl( 1 ).ComfortLowTempCurveNum = 1;
	OccupantVentilationControl( 1 ).ComfortHighTempCurveNum = 2;

	NumCurves = 2;
	PerfCurve.allocate( NumCurves );

	CurveNum = 1;
	PerfCurve( CurveNum ).CurveType = Quadratic;
	PerfCurve( CurveNum ).ObjectType = CurveType_Quadratic;
	PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;
	PerfCurve( CurveNum ).Coeff1 = 21.2;
	PerfCurve( CurveNum ).Coeff2 = 0.09;
	PerfCurve( CurveNum ).Coeff3 = 0.0;
	PerfCurve( CurveNum ).Coeff4 = 0.0;
	PerfCurve( CurveNum ).Coeff5 = 0.0;
	PerfCurve( CurveNum ).Coeff6 = 0.0;
	PerfCurve( CurveNum ).Var1Min = -50.0;
	PerfCurve( CurveNum ).Var1Max = 10.0;
	PerfCurve( CurveNum ).Var2Min = 0.0;
	PerfCurve( CurveNum ).Var2Max = 2.0;

	CurveNum = 2;
	PerfCurve( CurveNum ).CurveType = Quadratic;
	PerfCurve( CurveNum ).ObjectType = CurveType_Quadratic;
	PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;
	PerfCurve( CurveNum ).Coeff1 = 18.8;
	PerfCurve( CurveNum ).Coeff2 = 0.33;
	PerfCurve( CurveNum ).Coeff3 = 0.0;
	PerfCurve( CurveNum ).Coeff4 = 0.0;
	PerfCurve( CurveNum ).Coeff5 = 0.0;
	PerfCurve( CurveNum ).Coeff6 = 0.0;
	PerfCurve( CurveNum ).Var1Min = 10.0;
	PerfCurve( CurveNum ).Var1Max = 50.0;
	PerfCurve( CurveNum ).Var2Min = 0.0;
	PerfCurve( CurveNum ).Var2Max = 2.0;

	OccupantVentilationControl( 1 ).calc( 1, 2, 0, TimeOpenElapsed, TimeCloseElapsed, OpenStatus, OpenProbStatus, CloseProbStatus );
	EXPECT_EQ( 0, OpenProbStatus );
	EXPECT_EQ( 1, CloseProbStatus );

	MAT( 1 ) = 26.0;
	MRT( 1 ) = 26.0;
	OccupantVentilationControl( 1 ).calc( 1, 2, 0, TimeOpenElapsed, TimeCloseElapsed, OpenStatus, OpenProbStatus, CloseProbStatus );
	EXPECT_EQ( 2, OpenProbStatus );
	EXPECT_EQ( 0, CloseProbStatus );

	TimeOpenElapsed = 0.0;
	TimeCloseElapsed = 5.0;
	ZoneIntGain.allocate( 1 );
	ZoneIntGain( 1 ).NOFOCC = 0.5;
	TempControlType.allocate( 1 );
	TempControlType( 1 ) = 0;
	ZoneThermostatSetPointLo.allocate( 1 );
	ZoneThermostatSetPointHi.allocate( 1 );

	OccupantVentilationControl( 1 ).calc( 1, 2, 0, TimeOpenElapsed, TimeCloseElapsed, OpenStatus, OpenProbStatus, CloseProbStatus );
	EXPECT_EQ( 1, OpenProbStatus );
	EXPECT_EQ( 0, CloseProbStatus );

	TempControlType( 1 ) = 4;
	ZoneThermostatSetPointLo( 1 ) = 22.0;
	ZoneThermostatSetPointHi( 1 ) = 28.0;
	OccupantVentilationControl( 1 ).calc( 1, 2, 0, TimeOpenElapsed, TimeCloseElapsed, OpenStatus, OpenProbStatus, CloseProbStatus );
	EXPECT_EQ( 1, OpenProbStatus );
	EXPECT_EQ( 0, CloseProbStatus );

}
