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

// EnergyPlus::DataVectorTypes::Vector Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/DataVectorTypes.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/TarcogShading.hh>
#include "TARCOGParams.hh"

using namespace EnergyPlus::TarcogShading;
using namespace EnergyPlus::TARCOGParams;

TEST( WindowLayerEffectiveMultipliers, VenetianBlind_0_deg )
{
	EnergyPlus::ShowMessage( "Begin Test: Effective openness, venetian blind 0 deg slat." );
	{
		int const nlayer = 1;
		Real64 const width = 1;
		Real64 const height = 1;
		int nperr = 0;
		std::string ErrorMessage;
		Array1D< Real64 > Atop_eff( nlayer, 0.0 );
		Array1D< Real64 > Abot_eff( nlayer, 0.0 );
		Array1D< Real64 > Al_eff( nlayer, 0.0 );
		Array1D< Real64 > Ar_eff( nlayer, 0.0 );
		Array1D< Real64 > Ah_eff( nlayer, 0.0 );
		Array1D< Real64 > EffectiveOpenness( nlayer, 0.0 );
		Array1A< Real64 > const Atop = { 0.1 };
		Array1A< Real64 > const Abot = { 0.1 };
		Array1A< Real64 > const Al = { 0 };
		Array1A< Real64 > const Ar = { 0 };
		Array1A< Real64 > const Ah = { 0.2 };
		Array1A_int const LayerType = { VENETBLIND };
		Array1A< Real64 > const SlatAngle = { 0 };
		Array1A< Real64 > gap = {};

		updateEffectiveMultipliers( nlayer, width, height, Atop, Abot, Al, Ar, Ah, Atop_eff, Abot_eff, Al_eff,
			                            Ar_eff, Ah_eff, LayerType, SlatAngle, gap, nperr, ErrorMessage );

		EXPECT_EQ( nperr, 0 );
		EXPECT_EQ( Al_eff( 1 ), 0 );
		EXPECT_EQ( Ar_eff( 1 ), 0 );
		EXPECT_EQ( Atop_eff( 1 ), 0.1 );
		EXPECT_EQ( Abot_eff( 1 ), 0.1 );
		EXPECT_EQ( Ah_eff( 1 ), 0.006818 );

	}
}

TEST( WindowLayerEffectiveMultipliers, VenetianBlind_45_deg )
{
	EnergyPlus::ShowMessage( "Begin Test: Effective openness, venetian blind 45 deg slat." );
	{
		int const nlayer = 1;
		Real64 const width = 1;
		Real64 const height = 1;
		int nperr = 0;
		std::string ErrorMessage;
		Array1D< Real64 > Atop_eff( nlayer, 0.0 );
		Array1D< Real64 > Abot_eff( nlayer, 0.0 );
		Array1D< Real64 > Al_eff( nlayer, 0.0 );
		Array1D< Real64 > Ar_eff( nlayer, 0.0 );
		Array1D< Real64 > Ah_eff( nlayer, 0.0 );
		Array1D< Real64 > EffectiveOpenness( nlayer, 0.0 );
		Array1A< Real64 > const Atop = { 0.1 };
		Array1A< Real64 > const Abot = { 0.1 };
		Array1A< Real64 > const Al = { 0 };
		Array1A< Real64 > const Ar = { 0 };
		Array1A< Real64 > const Ah = { 0.2 };
		Array1A_int const LayerType = { VENETBLIND };
		Array1A< Real64 > const SlatAngle = { 45 };
		Array1A< Real64 > gap = {};

		updateEffectiveMultipliers( nlayer, width, height, Atop, Abot, Al, Ar, Ah, Atop_eff, Abot_eff, Al_eff,
			                            Ar_eff, Ah_eff, LayerType, SlatAngle, gap, nperr, ErrorMessage );

		EXPECT_EQ( nperr, 0 );
		EXPECT_EQ( Al_eff( 1 ), 0 );
		EXPECT_EQ( Ar_eff( 1 ), 0 );
		EXPECT_EQ( Atop_eff( 1 ), 0.1 );
		EXPECT_EQ( Abot_eff( 1 ), 0.1 );
		EXPECT_EQ( Ah_eff( 1 ), 0.007655 );

	}
}

TEST( WindowLayerEffectiveMultipliers, OtherShades )
{
	EnergyPlus::ShowMessage( "Begin Test: Effective openness, other shades." );
	{
		int const nlayer = 1;
		Real64 const width = 1;
		Real64 const height = 1;
		int nperr = 0;
		std::string ErrorMessage;
		Array1D< Real64 > Atop_eff( nlayer, 0.0 );
		Array1D< Real64 > Abot_eff( nlayer, 0.0 );
		Array1D< Real64 > Al_eff( nlayer, 0.0 );
		Array1D< Real64 > Ar_eff( nlayer, 0.0 );
		Array1D< Real64 > Ah_eff( nlayer, 0.0 );
		Array1D< Real64 > EffectiveOpenness( nlayer, 0.0 );
		Array1A< Real64 > const Atop = { 0.1 };
		Array1A< Real64 > const Abot = { 0.1 };
		Array1A< Real64 > const Al = { 0 };
		Array1A< Real64 > const Ar = { 0 };
		Array1A< Real64 > const Ah = { 0.2 };
		Array1A_int const LayerType = { DIFFSHADE };
		Array1A< Real64 > const SlatAngle = { 0 };
		Array1A< Real64 > gap = {};

		updateEffectiveMultipliers( nlayer, width, height, Atop, Abot, Al, Ar, Ah, Atop_eff, Abot_eff, Al_eff,
			                            Ar_eff, Ah_eff, LayerType, SlatAngle, gap, nperr, ErrorMessage );

		EXPECT_EQ( nperr, 0 );
		EXPECT_EQ( Al_eff( 1 ), 0 );
		EXPECT_EQ( Ar_eff( 1 ), 0 );
		EXPECT_EQ( Atop_eff( 1 ), 0.05 );
		EXPECT_EQ( Abot_eff( 1 ), 0.05 );
		EXPECT_EQ( Ah_eff( 1 ), 0.008792 );

	}
}