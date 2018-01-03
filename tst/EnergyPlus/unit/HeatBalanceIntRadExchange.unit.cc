// EnergyPlus, Copyright (c) 1996-2017, The Board of Trustees of the University of Illinois,
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy), Oak Ridge
// National Laboratory, managed by UT-Battelle, Alliance for Sustainable Energy, LLC, and other
// contributors. All rights reserved.
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

// EnergyPlus::HeatBalFiniteDiffManager Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/HeatBalanceIntRadExchange.hh>

using namespace EnergyPlus::HeatBalanceIntRadExchange;

namespace EnergyPlus {

	TEST_F( EnergyPlusFixture, HeatBalanceIntRadExchange_FixViewFactorsTest)
	{

		int N; // NUMBER OF SURFACES
		Array1D< Real64 > A; // AREA VECTOR- ASSUMED,BE N ELEMENTS LONG
		Array2D< Real64 > F; // APPROXIMATE DIRECT VIEW FACTOR MATRIX (N X N)
		int ZoneNum; // Zone number being fixed
		Real64 OriginalCheckValue; // check of SUM(F) - N
		Real64 FixedCheckValue; // check after fixed of SUM(F) - N
		Real64 FinalCheckValue; // the one to go with
		int NumIterations; // number of iterations to fixed
		Real64 RowSum; // RowSum of Fixed

		N = 3;

		A.allocate( N );
		F.allocate( N, N );

		A( 1 ) = 1.0;
		A( 2 ) = 1.0;
		A( 3 ) = 1.0;

		F( 1, 1 ) = 0.0;
		F( 1, 2 ) = 0.5;
		F( 1, 3 ) = 0.5;
		F( 2, 1 ) = 0.5;
		F( 2, 2 ) = 0.0;
		F( 2, 3 ) = 0.5;
		F( 3, 1 ) = 0.5;
		F( 3, 2 ) = 0.5;
		F( 3, 3 ) = 0.0;

		ZoneNum = 1;

		DataHeatBalance::Zone.allocate( ZoneNum );
		DataHeatBalance::Zone( ZoneNum ).Name = "Test";

		FixViewFactors( N, A, F, ZoneNum, OriginalCheckValue, FixedCheckValue, FinalCheckValue, NumIterations, RowSum );

		std::string const error_string = delimited_string( {
			"   ** Warning ** Surfaces in Zone=\"Test\" do not define an enclosure.",
			"   **   ~~~   ** Number of surfaces <= 3, view factors are set to force reciprocity but may not fulfill completeness.",
			"   **   ~~~   ** Reciprocity means that radiant exchange between two surfaces will match and not lead to an energy loss.",
			"   **   ~~~   ** Completeness means that all of the view factors between a surface and the other surfaces in a zone add up to unity.",
			"   **   ~~~   ** So, when there are three or less surfaces in a zone, EnergyPlus will make sure there are no losses of energy but",
			"   **   ~~~   ** it will not exchange the full amount of radiation with the rest of the zone as it would if there was a completed enclosure.",
		} );

		EXPECT_TRUE( compare_err_stream( error_string, true ) );

		// Tests for correction of view factors based on GitHub Issue #5772

		A( 1 ) = 20.0;
		A( 2 ) = 180.0;
		A( 3 ) = 180.0;
		F( 1, 1 ) = 0.0;
		F( 1, 2 ) = 0.5;
		F( 1, 3 ) = 0.5;
		F( 2, 1 ) = 0.1;
		F( 2, 2 ) = 0.0;
		F( 2, 3 ) = 0.9;
		F( 3, 1 ) = 0.1;
		F( 3, 2 ) = 0.9;
		F( 3, 3 ) = 0.0;

		FixViewFactors( N, A, F, ZoneNum, OriginalCheckValue, FixedCheckValue, FinalCheckValue, NumIterations, RowSum );
		EXPECT_NEAR( F( 1, 2 ), 0.07986, 0.001 );
		EXPECT_NEAR( F( 2, 1 ), 0.71875, 0.001 );
		EXPECT_NEAR( F( 3, 2 ), 0.28125, 0.001 );

		A( 1 ) = 100.0;
		A( 2 ) = 100.0;
		A( 3 ) = 200.0;
		F( 1, 1 ) = 0.0;
		F( 1, 2 ) = 1.0/3.0;
		F( 1, 3 ) = 2.0/3.0;
		F( 2, 1 ) = 1.0/3.0;
		F( 2, 2 ) = 0.0;
		F( 2, 3 ) = 2.0/3.0;
		F( 3, 1 ) = 0.5;
		F( 3, 2 ) = 0.5;
		F( 3, 3 ) = 0.0;

		FixViewFactors( N, A, F, ZoneNum, OriginalCheckValue, FixedCheckValue, FinalCheckValue, NumIterations, RowSum );
		EXPECT_NEAR( F( 1, 2 ), 0.181818, 0.001 );
		EXPECT_NEAR( F( 2, 3 ), 0.25, 0.001 );
		EXPECT_NEAR( F( 3, 2 ), 0.5, 0.001 );

		A( 1 ) = 100.0;
		A( 2 ) = 150.0;
		A( 3 ) = 200.0;
		F( 1, 1 ) = 0.0;
		F( 1, 2 ) = 150.0/350.0;
		F( 1, 3 ) = 200.0/350.0;
		F( 2, 1 ) = 1.0/3.0;
		F( 2, 2 ) = 0.0;
		F( 2, 3 ) = 2.0/3.0;
		F( 3, 1 ) = 0.4;
		F( 3, 2 ) = 0.6;
		F( 3, 3 ) = 0.0;

		FixViewFactors( N, A, F, ZoneNum, OriginalCheckValue, FixedCheckValue, FinalCheckValue, NumIterations, RowSum );
		EXPECT_NEAR( F( 1, 2 ), 0.21466, 0.001 );
		EXPECT_NEAR( F( 1, 3 ), 0.25445, 0.001 );
		EXPECT_NEAR( F( 2, 1 ), 0.32199, 0.001 );
		EXPECT_NEAR( F( 2, 3 ), 0.36832, 0.001 );
		EXPECT_NEAR( F( 3, 1 ), 0.50890, 0.001 );
		EXPECT_NEAR( F( 3, 2 ), 0.49110, 0.001 );

		A.deallocate();
		F.deallocate();

	}

	TEST_F( EnergyPlusFixture, HeatBalanceIntRadExchange_UpdateMovableInsulationFlagTest)
	{

		bool DidMIChange;
		int SurfNum;

		DataHeatBalance::Construct.allocate( 1 );
		DataHeatBalance::Material.allocate( 1 );
		DataSurfaces::Surface.allocate( 1 );

		SurfNum = 1;
		DataSurfaces::Surface( 1 ).MaterialMovInsulInt = 1;
		DataSurfaces::Surface( 1 ).MovInsulIntPresent = false;
		DataSurfaces::Surface( 1 ).MovInsulIntPresentPrevTS = false;
		DataSurfaces::Surface( 1 ).Construction = 1;
		DataSurfaces::Surface( 1 ).MaterialMovInsulInt = 1;
		DataHeatBalance::Construct( 1 ).InsideAbsorpThermal = 0.9;
		DataHeatBalance::Material( 1 ).AbsorpThermal = 0.5;
		DataHeatBalance::Material( 1 ).Resistance = 1.25;
		DataSurfaces::Surface( 1 ).SchedMovInsulInt = -1;
		DataHeatBalance::Material( 1 ).AbsorpSolar = 0.25;

		// Test 1: Movable insulation present but wasn't in previous time step, also movable insulation emissivity different than base construction
		//         This should result in a true value from the algorithm which will cause interior radiant exchange matrices to be recalculated
		HeatBalanceIntRadExchange::UpdateMovableInsulationFlag( DidMIChange, SurfNum );
		EXPECT_TRUE( DidMIChange );

		// Test 2: Movable insulation present and was also present in previous time step.  This should result in a false value since nothing has changed.
		DataSurfaces::Surface( 1 ).MovInsulIntPresentPrevTS = true;
		HeatBalanceIntRadExchange::UpdateMovableInsulationFlag( DidMIChange, SurfNum );
		EXPECT_TRUE( !DidMIChange );

		// Test 2: Movable insulation present but wasn't in previous time step.  However, the emissivity of the movable insulation and that of the
  		// 		   construction are the same so nothing has actually changed.  This should result in a false value.
		DataSurfaces::Surface( 1 ).MovInsulIntPresentPrevTS = false;
		DataHeatBalance::Material( 1 ).AbsorpThermal = DataHeatBalance::Construct( 1 ).InsideAbsorpThermal;
		HeatBalanceIntRadExchange::UpdateMovableInsulationFlag( DidMIChange, SurfNum );
		EXPECT_TRUE( !DidMIChange );

	}

}
