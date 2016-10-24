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

// EnergyPlus::HeatBalanceSurfaceManager Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/HeatBalanceSurfaceManager.hh>
#include <EnergyPlus/ScheduleManager.hh>

using namespace EnergyPlus::HeatBalanceSurfaceManager;

namespace EnergyPlus {

	TEST_F( EnergyPlusFixture, HeatBalanceSurfaceManager_CalcOutsideSurfTemp)
	{

		int SurfNum; // Surface number DO loop counter
		int ZoneNum; // Zone number the current surface is attached to
		int ConstrNum; // Construction index for the current surface
		Real64 HMovInsul; // "Convection" coefficient of movable insulation
		Real64 TempExt; // Exterior temperature boundary condition
		bool ErrorFlag; // Movable insulation error flag

		SurfNum = 1;
		ZoneNum = 1;
		ConstrNum = 1;
		HMovInsul = 1.0;
		TempExt = 23.0;
		ErrorFlag = false;
		
		DataHeatBalance::Construct.allocate( ConstrNum );
		DataHeatBalance::Construct( ConstrNum ).Name = "TestConstruct";
		DataHeatBalance::Construct( ConstrNum ).CTFCross( 0 ) = 0.0;
		DataHeatBalance::Construct( ConstrNum ).CTFOutside( 0 ) = 1.0;
		DataHeatBalance::Construct( ConstrNum ).SourceSinkPresent = true;
		DataHeatBalance::Material.allocate( 1 );
		DataHeatBalance::Material( 1 ).Name = "TestMaterial";
		
		
		DataHeatBalSurface::HcExtSurf.allocate( SurfNum );
		DataHeatBalSurface::HcExtSurf( SurfNum ) = 1.0;
		DataHeatBalSurface::HAirExtSurf.allocate( SurfNum );
		DataHeatBalSurface::HAirExtSurf( SurfNum ) = 1.0;
		DataHeatBalSurface::HSkyExtSurf.allocate( SurfNum );
		DataHeatBalSurface::HSkyExtSurf( SurfNum ) = 1.0;
		DataHeatBalSurface::HGrdExtSurf.allocate( SurfNum );
		DataHeatBalSurface::HGrdExtSurf( SurfNum ) = 1.0;
		
		DataHeatBalSurface::CTFConstOutPart.allocate( SurfNum );
		DataHeatBalSurface::CTFConstOutPart( SurfNum ) = 1.0;
		DataHeatBalSurface::QRadSWOutAbs.allocate( SurfNum );
		DataHeatBalSurface::QRadSWOutAbs( SurfNum ) = 1.0;
		DataHeatBalSurface::TempSurfIn.allocate( SurfNum );
		DataHeatBalSurface::TempSurfIn( SurfNum ) = 1.0;
		DataHeatBalSurface::QRadSWOutMvIns.allocate( SurfNum );
		DataHeatBalSurface::QRadSWOutMvIns( SurfNum ) = 1.0;
		
		DataHeatBalSurface::TH.allocate(2,2,1);
		DataSurfaces::Surface.allocate( SurfNum );
		DataSurfaces::Surface( SurfNum ).Class = 1;
		DataSurfaces::Surface( SurfNum ).Area = 10.0;
		DataSurfaces::Surface( SurfNum ).MaterialMovInsulExt = 1;
		
		DataEnvironment::SkyTemp = 23.0;
		DataEnvironment::OutDryBulbTemp	= 23.0;

		DataHeatBalSurface::QdotRadOutRep.allocate( SurfNum );
		DataHeatBalSurface::QdotRadOutRepPerArea.allocate( SurfNum );
		DataHeatBalSurface::QRadOutReport.allocate( SurfNum );
		DataGlobals::TimeStepZoneSec = 900.0;
		
		CalcOutsideSurfTemp( SurfNum, ZoneNum, ConstrNum, HMovInsul, TempExt, ErrorFlag );

		std::string const error_string = delimited_string( {
			"   ** Severe  ** Exterior movable insulation is not valid with embedded sources/sinks",
			"   **   ~~~   ** Construction TestConstruct contains an internal source or sink but also uses",
			"   **   ~~~   ** exterior movable insulation TestMaterial for a surface with that construction.",
			"   **   ~~~   ** This is not currently allowed because the heat balance equations do not currently accommodate this combination.",
		} );

		EXPECT_TRUE( ErrorFlag );
		EXPECT_TRUE( compare_err_stream( error_string, true ) );

	}

	TEST_F( EnergyPlusFixture, HeatBalanceSurfaceManager_TestSurfTempCalcHeatBalanceInsideSurf )
	{

		Real64 surfTemp;
		DataSurfaces::SurfaceData testSurface;
		DataHeatBalance::ZoneData testZone;
		int cntWarmupSurfTemp = 0;
		testSurface.Name = "TestSurface";
		testZone.Name = "TestZone";
		testZone.InternalHeatGains = 2.5;
		testZone.NominalInfilVent = 0.5;
		testZone.NominalMixing = 0.7;

		// no error
		surfTemp = 26;
		DataGlobals::WarmupFlag = true;
		testSurface.LowTempErrCount = 0;
		testSurface.HighTempErrCount = 0;
		testZone.TempOutOfBoundsReported = true;
		testZone.FloorArea = 1000;
		testZone.IsControlled = true;
		TestSurfTempCalcHeatBalanceInsideSurf( surfTemp, testSurface, testZone, cntWarmupSurfTemp );
		EXPECT_TRUE( compare_err_stream( "", true ) );

		// to hot - first time
		surfTemp = 201;
		DataGlobals::WarmupFlag = false;
		testSurface.LowTempErrCount = 0;
		testSurface.HighTempErrCount = 0;
		testZone.TempOutOfBoundsReported = false;
		testZone.FloorArea = 1000;
		testZone.IsControlled = true;
		TestSurfTempCalcHeatBalanceInsideSurf( surfTemp, testSurface, testZone, cntWarmupSurfTemp );
		std::string const error_string01 = delimited_string( {
			"   ** Severe  ** Temperature (high) out of bounds (201.00] for zone=\"TestZone\", for surface=\"TestSurface\"",
			"   **   ~~~   **  Environment=, at Simulation time= 00:00 - 00:00",
			"   **   ~~~   ** Zone=\"TestZone\", Diagnostic Details:",
			"   **   ~~~   ** ...Internal Heat Gain [2.500E-003] W/m2",
			"   **   ~~~   ** ...Infiltration/Ventilation [0.500] m3/s",
			"   **   ~~~   ** ...Mixing/Cross Mixing [0.700] m3/s",
			"   **   ~~~   ** ...Zone is part of HVAC controlled system."
		} );
		EXPECT_TRUE( compare_err_stream( error_string01, true ) );
		EXPECT_TRUE( testZone.TempOutOfBoundsReported );

		// to hot - subsequent times
		surfTemp = 201;
		DataGlobals::WarmupFlag = false;
		testSurface.LowTempErrCount = 0;
		testSurface.HighTempErrCount = 0;
		testZone.TempOutOfBoundsReported = true;
		testZone.FloorArea = 1000;
		testZone.IsControlled = true;
		TestSurfTempCalcHeatBalanceInsideSurf( surfTemp, testSurface, testZone, cntWarmupSurfTemp );
		std::string const error_string02 = delimited_string( {
			"   ** Severe  ** Temperature (high) out of bounds (201.00] for zone=\"TestZone\", for surface=\"TestSurface\"",
			"   **   ~~~   **  Environment=, at Simulation time= 00:00 - 00:00",
		} );
		EXPECT_TRUE( compare_err_stream( error_string02, true ) );
		EXPECT_TRUE( testZone.TempOutOfBoundsReported );


		// to cold - first time
		surfTemp = -101;
		DataGlobals::WarmupFlag = false;
		testSurface.LowTempErrCount = 0;
		testSurface.HighTempErrCount = 0;
		testZone.TempOutOfBoundsReported = false;
		testZone.FloorArea = 1000;
		testZone.IsControlled = true;
		TestSurfTempCalcHeatBalanceInsideSurf( surfTemp, testSurface, testZone, cntWarmupSurfTemp );
		std::string const error_string03 = delimited_string( {
			"   ** Severe  ** Temperature (low) out of bounds [-101.00] for zone=\"TestZone\", for surface=\"TestSurface\"",
			"   **   ~~~   **  Environment=, at Simulation time= 00:00 - 00:00",
			"   **   ~~~   ** Zone=\"TestZone\", Diagnostic Details:",
			"   **   ~~~   ** ...Internal Heat Gain [2.500E-003] W/m2",
			"   **   ~~~   ** ...Infiltration/Ventilation [0.500] m3/s",
			"   **   ~~~   ** ...Mixing/Cross Mixing [0.700] m3/s",
			"   **   ~~~   ** ...Zone is part of HVAC controlled system."
		} );
		EXPECT_TRUE( compare_err_stream( error_string03, true ) );
		EXPECT_TRUE( testZone.TempOutOfBoundsReported );

		// to cold - subsequent times
		surfTemp = -101;
		DataGlobals::WarmupFlag = false;
		testSurface.LowTempErrCount = 0;
		testSurface.HighTempErrCount = 0;
		testZone.TempOutOfBoundsReported = true;
		testZone.FloorArea = 1000;
		testZone.IsControlled = true;
		TestSurfTempCalcHeatBalanceInsideSurf( surfTemp, testSurface, testZone, cntWarmupSurfTemp );
		std::string const error_string04 = delimited_string( {
			"   ** Severe  ** Temperature (low) out of bounds [-101.00] for zone=\"TestZone\", for surface=\"TestSurface\"",
			"   **   ~~~   **  Environment=, at Simulation time= 00:00 - 00:00"
		} );
		EXPECT_TRUE( compare_err_stream( error_string04, true ) );
		EXPECT_TRUE( testZone.TempOutOfBoundsReported );

	}

	TEST_F( EnergyPlusFixture, HeatBalanceSurfaceManager_ComputeIntThermalAbsorpFactors)
	{

		DataSurfaces::TotSurfaces = 1;
		DataGlobals::NumOfZones = 1;
		DataHeatBalance::TotMaterials = 1;
		DataHeatBalance::TotConstructs = 1;
		
		DataHeatBalance::Zone.allocate( DataGlobals::NumOfZones );
		DataSurfaces::Surface.allocate(DataSurfaces::TotSurfaces);
		DataSurfaces::SurfaceWindow.allocate(DataSurfaces::TotSurfaces);
		DataHeatBalance::Construct.allocate(DataHeatBalance::TotConstructs);
		DataHeatBalance::Material.allocate(DataHeatBalance::TotMaterials);

		DataSurfaces::Surface( 1 ).HeatTransSurf = true;
		DataSurfaces::Surface( 1 ).Construction = 1;
		DataSurfaces::SurfaceWindow( 1 ).ShadingFlag = 0;
		DataHeatBalance::Construct( 1 ).InsideAbsorpThermal = 0.9;
		DataHeatBalance::Construct( 1 ).TransDiff = 0.0;
		DataSurfaces::Surface( 1 ).MaterialMovInsulInt = 1;
		DataHeatBalance::Material( 1 ).AbsorpThermal = 0.2;
		DataHeatBalance::Material( 1 ).AbsorpSolar = 0.5;
		
		DataGlobals::NumOfZones = 0; // Reset this to skip part of the code in the unit tested routine
		
		DataSurfaces::Surface( 1 ).SchedMovInsulInt = -1;	// According to schedule manager protocol, an index of -1 returns a 1.0 value for the schedule
		DataHeatBalance::Material( 1 ).Resistance = 1.25;

		ComputeIntThermalAbsorpFactors();
		
		EXPECT_EQ( 0.2, DataHeatBalance::ITABSF( 1 ) );
		
	}
	
	TEST_F( EnergyPlusFixture, HeatBalanceSurfaceManager_UpdateFinalThermalHistories)
	{
		DataSurfaces::TotSurfaces = 1;
		DataGlobals::NumOfZones = 1;
		DataHeatBalance::TotConstructs = 1;
		DataHeatBalance::Zone.allocate( DataGlobals::NumOfZones );
		DataSurfaces::Surface.allocate(DataSurfaces::TotSurfaces);
		DataSurfaces::SurfaceWindow.allocate(DataSurfaces::TotSurfaces);
		DataHeatBalance::Construct.allocate(DataHeatBalance::TotConstructs);
		DataHeatBalance::AnyConstructInternalSourceInInput = true;

		AllocateSurfaceHeatBalArrays(); // allocates a host of variables related to CTF calculations
		
		DataSurfaces::Surface( 1 ).Class = DataSurfaces::SurfaceClass_Wall;
		DataSurfaces::Surface( 1 ).HeatTransSurf = true;
		DataSurfaces::Surface( 1 ).HeatTransferAlgorithm = DataSurfaces::HeatTransferModel_CTF;
		DataSurfaces::Surface( 1 ).ExtBoundCond = 1;
		DataSurfaces::Surface( 1 ).Construction = 1;
		
		DataHeatBalance::Construct( 1 ).NumCTFTerms = 2;
		DataHeatBalance::Construct( 1 ).SourceSinkPresent = true;
		DataHeatBalance::Construct( 1 ).NumHistories = 1;
		DataHeatBalance::Construct( 1 ).CTFTUserOut( 0 ) = 0.5;
		DataHeatBalance::Construct( 1 ).CTFTUserIn( 0 ) = 0.25;
		DataHeatBalance::Construct( 1 ).CTFTUserSource( 0 ) = 0.25;
		
		DataHeatBalSurface::SUMH( 1 ) = 0;
		DataHeatBalSurface::TH( 1, 1, 1 ) = 20.0;
		DataHeatBalSurface::TempSurfIn( 1 ) = 10.0;
		
		DataHeatBalFanSys::CTFTuserConstPart( 1 ) = 0.0;

		UpdateThermalHistories(); // First check to see if it is calculating the user location temperature properly
		
		EXPECT_EQ( 12.5, DataHeatBalSurface::TempUserLoc( 1 ) );
		EXPECT_EQ( 0.0, DataHeatBalSurface::TuserHist( 1, 3 ) );
		
		UpdateThermalHistories();

		EXPECT_EQ( 12.5, DataHeatBalSurface::TuserHist( 1, 3 ) ); // Now check to see that it is shifting the temperature history properly
		
	}
	
}
