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

// EnergyPlus::WindowManager unit tests

// C++ Headers
#include <iostream>

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <WindowEquivalentLayer.hh>
#include <WindowManager.hh>
#include <DataWindowEquivalentLayer.hh>
#include <ConvectionCoefficients.hh>
#include <DataEnvironment.hh>
#include <DataGlobals.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHeatBalSurface.hh>
#include <DataSurfaces.hh>
#include <ElectricPowerServiceManager.hh>
#include <HeatBalanceManager.hh>
#include <HeatBalanceIntRadExchange.hh>
#include <HeatBalanceSurfaceManager.hh>
#include <Psychrometrics.hh>
#include <SolarShading.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace EnergyPlus::WindowEquivalentLayer;

TEST_F(EnergyPlusFixture, WindowEquivalentLayerGetInput )
{

	bool ErrorsFound( false );

	std::string const idf_objects = delimited_string({
		"Version,8.7;",

		"  Construction:WindowEquivalentLayer,",
		"  CLR CLR VB,                !- Name",
		"  GLZCLR,                    !- Outside Layer",
		"  Air GAP SealedOut 20mm,    !- Layer 2",
		"  GLZCLR,                    !- Layer 3",
		"  Air GAP SealedIndoor 20mm, !- Layer 4",
		"  VBU8D6+45SW1;              !- Layer 5",
		 
		"WindowMaterial:Glazing:EquivalentLayer,",
		"  GLZCLR,                    !-  Name",
		"  SpectralAverage,           !-  Optical Data Type",
		"  ,                          !-  Window Glass Spectral Data Set Name",
		"  0.83,                      !-  Front Side Beam-Beam Solar Transmittance",
		"  0.83,                      !-  Back Side Beam-Beam Solar Transmittance",
		"  0.08,                      !-  Front Side Beam-Beam Solar Reflectance",
		"  0.08,                      !-  Back Side Beam-Beam Solar Reflectance",
		"  0.0,                       !-  Front Side Beam-Beam Visible Transmittance",
		"  0.0,                       !-  Back Side Beam-Beam Visible Transmittance",
		"  0.0,                       !-  Front Side Beam-Beam Visible Reflectance",
		"  0.0,                       !-  Back Side Beam-Beam Visible Reflectance",
		"  0.0,                       !-  Front Side Beam-Diffuse Solar Transmittance",
		"  0.0,                       !-  Back Side Beam-Diffuse Solar Transmittance",
		"  0.0,                       !-  Front Side Beam-Diffuse Solar Reflectance",
		"  0.0,                       !-  Back Side Beam-Diffuse Solar Reflectance",
		"  0.0,                       !-  Front Side Beam-Diffuse Visible Transmittance",
		"  0.0,                       !-  Back Side Beam-Diffuse Visible Transmittance",
		"  0.0,                       !-  Front Side Beam-Diffuse Visible Reflectance",
		"  0.0,                       !-  Back Side Beam-Diffuse Visible Reflectance",
		"  0.76,                      !-  Diffuse-Diffuse Solar Transmittance",
		"  0.14,                      !-  Front Side Diffuse-Diffuse Solar Reflectance",
		"  0.14,                      !-  Back Side Diffuse-Diffuse Solar Reflectance",
		"  0.0,                       !-  Diffuse-Diffuse Visible Transmittance",
		"  0.0,                       !-  Front Side Diffuse-Diffuse Visible Reflectance",
		"  0.0,                       !-  Back Side Diffuse-Diffuse Visible Reflectance",
		"  0.0,                       !-  Infrared Transmittance (front and back)",
		"  0.84,                      !-  Front Side Infrared Emissivity",
		"  0.84;                      !-  Back Side Infrared Emissivity",
		  
		"WindowMaterial:Blind:EquivalentLayer,",
		"  VBU8D6+45SW1,           ! - Name",
		"  Horizontal,             ! - Slat Orientation",
		"  0.025,                  ! - Slat Width",
		"  0.025,                  ! - Slat Separation",
		"  0.0,                    ! - Slat Crown",
		"  45.0,                   ! - Slat Angle",
		"  0.0,                    ! - Front Side Slat Beam-Diffuse Solar Transmittance",
		"  0.0,                    ! - Back Side Slat Beam-Diffuse Solar Transmittance",
		"  0.0,                    ! - Front Side Slat Beam-Diffuse Solar Reflectance",
		"  0.0,                    ! - Back Side Slat Beam-Diffuse Solar Reflectance",
		"  0.0,                    ! - Front Side Slat Beam-Diffuse Visible Solar Transmittance",
		"  0.0,                    ! - Back Side Slat Beam-Diffuse Visible Solar Transmittance",
		"  0.0,                    ! - Front Side Slat Beam-Diffuse Visible Solar Reflectance",
		"  0.0,                    ! - Back Side Slat Beam-Diffuse Visible Solar Reflectance",
		"  0.0,                    ! - Slat Diffuse-Diffuse Solar Transmittance",
		"  0.80,                   ! - Front Side Slat Diffuse-Diffuse Solar Reflectance",
		"  0.60,                   ! - Back Side Slat Diffuse-Diffuse Solar Reflectance",
		"  0.0,                    ! - Slat Diffuse-Diffuse Visible Transmittance",
		"  0.0,                    ! - Front Side Slat Diffuse-Diffuse Visible Reflectance",
		"  0.0,                    ! - Back Side Slat Diffuse-Diffuse Visible Reflectance",
		"  0.0,                    ! - Slat Infrared Transmittance",
		"  0.90,                   ! - Front Side Slat Infrared Emissivity",
		"  0.90,                   ! - Back Side Slat Infrared Emissivity",
		"  BlockBeamSolar;         ! - Slat Angle Control",
		
		" WindowMaterial:Gap:EquivalentLayer,",
		"  Air GAP SealedOut 20mm,    !- Name",
		"  Air,                       !- Gas Type",
		"  0.0200,                    !- Thickness",
		"  Sealed;                    !- Gap Vent Type",
		 
		" WindowMaterial:Gap:EquivalentLayer,",
		"  Air GAP SealedIndoor 20mm, !- Name",
		"  Air,                       !- Gas Type",
		"  0.020,                     !- Thickness",
		"  Sealed;                    !- Gap Vent Type ",
	});

	ASSERT_FALSE(process_idf(idf_objects));

	HeatBalanceManager::GetMaterialData( ErrorsFound );
	HeatBalanceManager::GetConstructData( ErrorsFound );

	int VBMatNum( 0 );
	for ( int i = 1; i <= 4; i++ ) {
		if ( DataHeatBalance::Material( i ).Group == DataHeatBalance::BlindEquivalentLayer ) {
			VBMatNum = i;
			break;
		}
	}
	EXPECT_EQ( 1, DataHeatBalance::TotBlindsEQL );
	EXPECT_EQ( DataHeatBalance::Material( VBMatNum ).Group, DataHeatBalance::BlindEquivalentLayer );
	EXPECT_EQ( DataHeatBalance::Material( VBMatNum ).SlatAngleType, WindowEquivalentLayer::lscVBNOBM );
	
	int ConstrNum = 1;
	int EQLNum = 0;
	InitEquivalentLayerWindowCalculations();
	EQLNum = DataHeatBalance::Construct( ConstrNum ).EQLConsPtr;
	SetEquivalentLayerWindowProperties( EQLNum );
	EXPECT_EQ( CFS( EQLNum ).L( CFS( EQLNum ).VBLayerPtr ).CNTRL, WindowEquivalentLayer::lscVBNOBM );
	EXPECT_EQ( CFS( EQLNum ).L( CFS( EQLNum ).VBLayerPtr ).CNTRL, WindowEquivalentLayer::lscVBNOBM );


}


