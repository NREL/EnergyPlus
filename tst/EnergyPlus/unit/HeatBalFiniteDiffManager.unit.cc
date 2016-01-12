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

// EnergyPlus::HeatBalFiniteDiffManager Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/HeatBalFiniteDiffManager.hh>

using namespace EnergyPlus::HeatBalFiniteDiffManager;

namespace EnergyPlus {

	TEST_F( EnergyPlusFixture, HeatBalFiniteDiffManager_CalcNodeHeatFluxTest)
	{

		int const numNodes( 4 );
		int nodeNum( 0 );
		SurfaceFD.allocate( 1 );
		int const SurfNum( 1 );
		SurfaceFD( SurfNum ).QDreport.allocate( numNodes + 1 );
		SurfaceFD( SurfNum ).TDpriortimestep.allocate( numNodes + 1 );
		SurfaceFD( SurfNum ).TDT.allocate( numNodes + 1 );
		SurfaceFD( SurfNum ).CpDelXRhoS1.allocate( numNodes + 1 );
		SurfaceFD( SurfNum ).CpDelXRhoS2.allocate( numNodes + 1 );
		DataHeatBalSurface::OpaqSurfInsFaceConductionFlux.allocate( 1 );
		DataGlobals::TimeStepZoneSec = 600.0;

		Real64 expectedResult1( 0.0 );
		Real64 expectedResult2( 0.0 );
		Real64 expectedResult3( 0.0 );
		Real64 expectedResult4( 0.0 );
		Real64 expectedResult5( 0.0 );

		// Steady-state case
		DataHeatBalSurface::OpaqSurfInsFaceConductionFlux( SurfNum ) = 100.0;
		nodeNum = 1;
		SurfaceFD( SurfNum ).TDpriortimestep( nodeNum ) = 20.0;
		SurfaceFD( SurfNum ).TDT( nodeNum ) = 20.0;
		SurfaceFD( SurfNum ).CpDelXRhoS1( nodeNum ) = 1000.0;
		SurfaceFD( SurfNum ).CpDelXRhoS2( nodeNum ) = 2000.0;
		expectedResult1 = DataHeatBalSurface::OpaqSurfInsFaceConductionFlux( SurfNum );

		nodeNum = 2;
		SurfaceFD( SurfNum ).TDpriortimestep( nodeNum ) = 22.0;
		SurfaceFD( SurfNum ).TDT( nodeNum ) = 22.0;
		SurfaceFD( SurfNum ).CpDelXRhoS1( nodeNum ) = 1000.0;
		SurfaceFD( SurfNum ).CpDelXRhoS2( nodeNum ) = 2000.0;
		expectedResult2 = DataHeatBalSurface::OpaqSurfInsFaceConductionFlux( SurfNum );

		nodeNum = 3;
		SurfaceFD( SurfNum ).TDpriortimestep( nodeNum ) = 23.0;
		SurfaceFD( SurfNum ).TDT( nodeNum ) = 23.0;
		SurfaceFD( SurfNum ).CpDelXRhoS1( nodeNum ) = 1000.0;
		SurfaceFD( SurfNum ).CpDelXRhoS2( nodeNum ) = 2000.0;
		expectedResult3 = DataHeatBalSurface::OpaqSurfInsFaceConductionFlux( SurfNum );

		nodeNum = 4;
		SurfaceFD( SurfNum ).TDpriortimestep( nodeNum ) = 26.0;
		SurfaceFD( SurfNum ).TDT( nodeNum ) = 26.0;
		SurfaceFD( SurfNum ).CpDelXRhoS1( nodeNum ) = 1000.0;
		SurfaceFD( SurfNum ).CpDelXRhoS2( nodeNum ) = 2000.0;
		expectedResult4 = DataHeatBalSurface::OpaqSurfInsFaceConductionFlux( SurfNum );

		nodeNum = 5;
		SurfaceFD( SurfNum ).TDpriortimestep( nodeNum ) = 27.0;
		SurfaceFD( SurfNum ).TDT( nodeNum ) = 27.0;
		SurfaceFD( SurfNum ).CpDelXRhoS1( nodeNum ) = 1000.0;
		SurfaceFD( SurfNum ).CpDelXRhoS2( nodeNum ) = 2000.0;
		expectedResult5 = DataHeatBalSurface::OpaqSurfInsFaceConductionFlux( SurfNum );

		CalcNodeHeatFlux( SurfNum, numNodes );
		EXPECT_NEAR( SurfaceFD( SurfNum ).QDreport( 1 ), expectedResult1 , 0.0001 );
		EXPECT_NEAR( SurfaceFD( SurfNum ).QDreport( 2 ), expectedResult2 , 0.0001 );
		EXPECT_NEAR( SurfaceFD( SurfNum ).QDreport( 3 ), expectedResult3 , 0.0001 );
		EXPECT_NEAR( SurfaceFD( SurfNum ).QDreport( 4 ), expectedResult4 , 0.0001 );
		EXPECT_NEAR( SurfaceFD( SurfNum ).QDreport( 5 ), expectedResult5 , 0.0001 );

		// Reset
		SurfaceFD( SurfNum ).QDreport = 0.0;
		expectedResult1 = 0.0;
		expectedResult2 = 0.0;
		expectedResult3 = 0.0;
		expectedResult4 = 0.0;
		expectedResult5 = 0.0;

		// Unsteady-state case
		DataGlobals::TimeStepZoneSec = 600.0;
		DataHeatBalSurface::OpaqSurfInsFaceConductionFlux( SurfNum ) = -200.0;

		nodeNum = 5;
		SurfaceFD( SurfNum ).TDpriortimestep( nodeNum ) = 27.5;
		SurfaceFD( SurfNum ).TDT( nodeNum ) = 27.0;
		SurfaceFD( SurfNum ).CpDelXRhoS1( nodeNum ) = 0.0;
		SurfaceFD( SurfNum ).CpDelXRhoS2( nodeNum ) = 0.0;
		expectedResult5 = DataHeatBalSurface::OpaqSurfInsFaceConductionFlux( SurfNum );

		nodeNum = 4;
		SurfaceFD( SurfNum ).TDpriortimestep( nodeNum ) = 26.0;
		SurfaceFD( SurfNum ).TDT( nodeNum ) = 26.0;
		SurfaceFD( SurfNum ).CpDelXRhoS1( nodeNum ) = 0.0;
		SurfaceFD( SurfNum ).CpDelXRhoS2( nodeNum ) = 2000.0;
		expectedResult4 = expectedResult5; // r-layer with zero heat capacity, so flux passes through

		nodeNum = 3;
		SurfaceFD( SurfNum ).TDpriortimestep( nodeNum ) = 23.0;
		SurfaceFD( SurfNum ).TDT( nodeNum ) = 23.0;
		SurfaceFD( SurfNum ).CpDelXRhoS1( nodeNum ) = 1000.0;
		SurfaceFD( SurfNum ).CpDelXRhoS2( nodeNum ) = 2000.0;
		expectedResult3 = expectedResult4; // no change in temperature at nodes 4 and 3, so flux passes through

		nodeNum = 2;
		SurfaceFD( SurfNum ).TDpriortimestep( nodeNum ) = 22.2;
		SurfaceFD( SurfNum ).TDT( nodeNum ) = 22.0;
		SurfaceFD( SurfNum ).CpDelXRhoS1( nodeNum ) = 1000.0;
		SurfaceFD( SurfNum ).CpDelXRhoS2( nodeNum ) = 2000.0;
		expectedResult2 = expectedResult3 + ( SurfaceFD( SurfNum ).TDT( nodeNum ) - SurfaceFD( SurfNum ).TDpriortimestep( nodeNum ) ) * SurfaceFD( SurfNum ).CpDelXRhoS2( nodeNum ) / DataGlobals::TimeStepZoneSec;

		nodeNum = 1;
		SurfaceFD( SurfNum ).TDpriortimestep( nodeNum ) = 20.1;
		SurfaceFD( SurfNum ).TDT( nodeNum ) = 20.0;
		SurfaceFD( SurfNum ).CpDelXRhoS1( nodeNum ) = 1000.0;
		SurfaceFD( SurfNum ).CpDelXRhoS2( nodeNum ) = 2000.0;
		expectedResult1 = expectedResult2 + ( SurfaceFD( SurfNum ).TDT( nodeNum + 1 ) - SurfaceFD( SurfNum ).TDpriortimestep( nodeNum + 1 ) ) * SurfaceFD( SurfNum ).CpDelXRhoS1( nodeNum + 1 ) / DataGlobals::TimeStepZoneSec;
		expectedResult1 = expectedResult1 + ( SurfaceFD( SurfNum ).TDT( nodeNum ) - SurfaceFD( SurfNum ).TDpriortimestep( nodeNum ) ) * SurfaceFD( SurfNum ).CpDelXRhoS2( nodeNum ) / DataGlobals::TimeStepZoneSec;

		CalcNodeHeatFlux( SurfNum, numNodes );
		EXPECT_NEAR( SurfaceFD( SurfNum ).QDreport( 1 ), expectedResult1, 0.0001 );
		EXPECT_NEAR( SurfaceFD( SurfNum ).QDreport( 2 ), expectedResult2, 0.0001 );
		EXPECT_NEAR( SurfaceFD( SurfNum ).QDreport( 3 ), expectedResult3, 0.0001 );
		EXPECT_NEAR( SurfaceFD( SurfNum ).QDreport( 4 ), expectedResult4, 0.0001 );
		EXPECT_NEAR( SurfaceFD( SurfNum ).QDreport( 5 ), expectedResult5, 0.0001 );

	}

}
