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
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/HeatBalanceIntRadExchange.hh>

using namespace EnergyPlus::HeatBalanceIntRadExchange;

namespace EnergyPlus {

	TEST_F( EnergyPlusFixture, HeatBalanceIntRadExchange_FixViewFactorsTest)
	{

		int N; // NUMBER OF SURFACES
		Array1D< Real64 > A; // AREA VECTOR- ASSUMED,BE N ELEMENTS LONG
		Array2D< Real64 > F; // APPROXIMATE DIRECT VIEW FACTOR MATRIX (N X N)
		int ZoneNum; // Zone number being fixe
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

	}

}
