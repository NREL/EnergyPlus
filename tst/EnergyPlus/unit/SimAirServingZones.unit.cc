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

// EnergyPlus::SimAirServingZones Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// C++ Headers
#include <cassert>
#include <cmath>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <DataAirSystems.hh>
#include <DataSizing.hh>
#include <SimAirServingZones.hh>
#include <MixedAir.hh>
#include <UtilityRoutines.hh>

using namespace EnergyPlus;
using namespace DataAirSystems;
using namespace DataSizing;
using namespace ObjexxFCL;
using namespace SimAirServingZones;

namespace EnergyPlus {

	TEST( SimAirServingZones, ReheatCoilSizing )
	{
		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine test the GetHeatingSATempForSizing & GetHeatingSATempHumRatForSizing methods, 
		// which are designed to get the proper reheat coil inlet temperature/humidity ratio for sizing
		// depending on the system configurations
	
		ShowMessage( "Begin Test: SimAirServingZones, ReheatCoilSizing" );
				
		int NumPrimaryAirSys = 4; // total number of air loops
		int AirLoopNum; // index of air loops
		int CtrlZoneNum; // index of zones
		
		// Allocate
		CalcSysSizing.allocate( NumPrimaryAirSys );
		FinalSysSizing.allocate( NumPrimaryAirSys );
		FinalZoneSizing.allocate( NumPrimaryAirSys );
		PrimaryAirSystem.allocate( NumPrimaryAirSys );
		
		// Inputs: system configurations: 
		// 	(1) Central heating coils exist
		// 	(2) No central heating coils, but preheating coils exist
		// 	(3) No central heating coils, but OA heat-exchangers exist
		// 	(4) No central heating coils; No preheating coils or OA heat-exchangers

		PrimaryAirSystem( 1 ).CentralHeatCoilExists = true;
		PrimaryAirSystem( 2 ).CentralHeatCoilExists = false;
		PrimaryAirSystem( 3 ).CentralHeatCoilExists = false;
		PrimaryAirSystem( 4 ).CentralHeatCoilExists = false;
		
		PrimaryAirSystem( 1 ).NumOAHeatCoils = 0;
		PrimaryAirSystem( 2 ).NumOAHeatCoils = 1;
		PrimaryAirSystem( 3 ).NumOAHeatCoils = 0;
		PrimaryAirSystem( 4 ).NumOAHeatCoils = 0;
		
		PrimaryAirSystem( 1 ).NumOAHXs = 0;
		PrimaryAirSystem( 2 ).NumOAHXs = 0;
		PrimaryAirSystem( 3 ).NumOAHXs = 1;
		PrimaryAirSystem( 4 ).NumOAHXs = 0;
		
		// Inputs: sizing parameters
		for ( AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum ) {
			FinalSysSizing( AirLoopNum ).DesOutAirVolFlow = 0.25;
			FinalSysSizing( AirLoopNum ).DesHeatVolFlow = 0.50; 
			
			FinalSysSizing( AirLoopNum ).PreheatTemp = 7;
			FinalSysSizing( AirLoopNum ).HeatRetTemp = 22; 
			FinalSysSizing( AirLoopNum ).HeatMixTemp = 10;
			CalcSysSizing( AirLoopNum ).HeatSupTemp = 17;
			
			FinalSysSizing( AirLoopNum ).PreheatHumRat = 0.003;
			FinalSysSizing( AirLoopNum ).HeatRetHumRat = 0.008; 
			FinalSysSizing( AirLoopNum ).HeatMixHumRat = 0.004;
			CalcSysSizing( AirLoopNum ).HeatSupHumRat = 0.006;

		}
		
		// Run 
		for ( AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum ) {
			CtrlZoneNum = AirLoopNum;

			FinalZoneSizing( CtrlZoneNum ).DesHeatCoilInTempTU = GetHeatingSATempForSizing( AirLoopNum );
			FinalZoneSizing( CtrlZoneNum ).DesHeatCoilInHumRatTU = GetHeatingSATempHumRatForSizing( AirLoopNum );
		}
		
		// Check
		EXPECT_EQ( 17.0, FinalZoneSizing( 1 ).DesHeatCoilInTempTU );
		EXPECT_NEAR( 14.5, FinalZoneSizing( 2 ).DesHeatCoilInTempTU, 0.05 );
		EXPECT_NEAR( 14.5, FinalZoneSizing( 3 ).DesHeatCoilInTempTU, 0.05 );
		EXPECT_EQ( 10.0, FinalZoneSizing( 4 ).DesHeatCoilInTempTU );
		EXPECT_EQ( 0.006, FinalZoneSizing( 1 ).DesHeatCoilInHumRatTU );
		EXPECT_EQ( 0.0055, FinalZoneSizing( 2 ).DesHeatCoilInHumRatTU );
		EXPECT_EQ( 0.0055, FinalZoneSizing( 3 ).DesHeatCoilInHumRatTU );
		EXPECT_EQ( 0.004, FinalZoneSizing( 4 ).DesHeatCoilInHumRatTU );
		
		// Clean up
		CalcSysSizing.deallocate( );
		FinalSysSizing.deallocate( );
		FinalZoneSizing.deallocate( );
		PrimaryAirSystem.deallocate( ); 

	}

}
