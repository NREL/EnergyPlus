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

// EnergyPlus::ZonePlenum Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/ZonePlenum.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <DataContaminantBalance.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace ObjexxFCL;
using namespace DataGlobals;
using namespace EnergyPlus::ZonePlenum;
using namespace EnergyPlus::DataLoopNode;
using DataContaminantBalance::Contaminant;


TEST_F( EnergyPlusFixture, ZonePlenum_InitAirZoneReturnPlenumTest )
{
	BeginEnvrnFlag = false;
	Contaminant.CO2Simulation = true;
	Contaminant.GenericContamSimulation = true;

	NumZoneReturnPlenums = 1;
	ZoneRetPlenCond.allocate( NumZoneReturnPlenums );
	int ZonePlenumNum = 1;
	ZoneRetPlenCond( ZonePlenumNum ).NumInletNodes = 0; // To avoid initializing extra zone equip config and ADU data
	ZoneRetPlenCond( ZonePlenumNum ).NumInducedNodes = 2;
	ZoneRetPlenCond( ZonePlenumNum ).InletNode.allocate(1); // Needed for the Update routine
	ZoneRetPlenCond( ZonePlenumNum ).InducedNode.allocate( ZoneRetPlenCond( ZonePlenumNum ).NumInducedNodes );
	ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRate.allocate( ZoneRetPlenCond( ZonePlenumNum ).NumInducedNodes );
	ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRateMaxAvail.allocate( ZoneRetPlenCond( ZonePlenumNum ).NumInducedNodes );
	ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRateMinAvail.allocate( ZoneRetPlenCond( ZonePlenumNum ).NumInducedNodes );
	ZoneRetPlenCond( ZonePlenumNum ).InducedTemp.allocate( ZoneRetPlenCond( ZonePlenumNum ).NumInducedNodes );
	ZoneRetPlenCond( ZonePlenumNum ).InducedHumRat.allocate( ZoneRetPlenCond( ZonePlenumNum ).NumInducedNodes );
	ZoneRetPlenCond( ZonePlenumNum ).InducedEnthalpy.allocate( ZoneRetPlenCond( ZonePlenumNum ).NumInducedNodes );
	ZoneRetPlenCond( ZonePlenumNum ).InducedPressure.allocate( ZoneRetPlenCond( ZonePlenumNum ).NumInducedNodes );
	ZoneRetPlenCond( ZonePlenumNum ).InducedCO2.allocate( ZoneRetPlenCond( ZonePlenumNum ).NumInducedNodes );
	ZoneRetPlenCond( ZonePlenumNum ).InducedGenContam.allocate( ZoneRetPlenCond( ZonePlenumNum ).NumInducedNodes );
	ZoneRetPlenCond( ZonePlenumNum ).InletNode(1) = 1;
	ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRate = 0.0;
	ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRateMaxAvail = 0.0;
	ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRateMinAvail = 0.0;
	ZoneRetPlenCond( ZonePlenumNum ).InducedTemp = 0.0;
	ZoneRetPlenCond( ZonePlenumNum ).InducedHumRat = 0.0;
	ZoneRetPlenCond( ZonePlenumNum ).InducedEnthalpy = 0.0;
	ZoneRetPlenCond( ZonePlenumNum ).InducedPressure = 0.0;
	ZoneRetPlenCond( ZonePlenumNum ).InducedCO2 = 0.0;
	ZoneRetPlenCond( ZonePlenumNum ).InducedGenContam = 0.0;

	Node.allocate( 4 ); // One node per plenum plus total of NumInducedNodes for all plenums)
	int ZoneNodeNum = 1;
	ZoneRetPlenCond( ZonePlenumNum ).ZoneNodeNum = ZoneNodeNum;
	Node( ZoneNodeNum ).Temp = 24.2;
	Node( ZoneNodeNum ).HumRat= 0.0003;
	Node( ZoneNodeNum ).Enthalpy = 40000.0;
	Node( ZoneNodeNum ).Press = 99000.0;
	Node( ZoneNodeNum ).CO2 = 950.0;
	Node( ZoneNodeNum ).GenContam = 100.0;
	ZoneRetPlenCond( ZonePlenumNum ).OutletPressure = 99000.0;

	int InducedNodeIndex = 1;
	int InducedNodeNum = 2;
	ZoneRetPlenCond( ZonePlenumNum ).InducedNode( InducedNodeIndex ) = InducedNodeNum;
	Node( InducedNodeNum ).MassFlowRate = 0.20;
	Node( InducedNodeNum ).MassFlowRateMaxAvail = 0.25;
	Node( InducedNodeNum ).MassFlowRateMinAvail = 0.10;

	InducedNodeIndex = 2;
	InducedNodeNum = 3;
	ZoneRetPlenCond( ZonePlenumNum ).InducedNode( InducedNodeIndex ) = InducedNodeNum;
	Node( InducedNodeNum ).MassFlowRate = 0.40;
	Node( InducedNodeNum ).MassFlowRateMaxAvail = 0.50;
	Node( InducedNodeNum ).MassFlowRateMinAvail = 0.22;

	ZoneRetPlenCond( ZonePlenumNum ).OutletNode = 4;

	InitAirZoneReturnPlenum( ZonePlenumNum );
	UpdateAirZoneReturnPlenum( ZonePlenumNum );

	EXPECT_EQ( Node( ZoneRetPlenCond( ZonePlenumNum ).ZoneNodeNum ).CO2, Node(ZoneRetPlenCond( ZonePlenumNum ).OutletNode ).CO2 );
	EXPECT_EQ( Node( ZoneRetPlenCond( ZonePlenumNum ).ZoneNodeNum ).CO2, Node(ZoneRetPlenCond( ZonePlenumNum ).OutletNode ).CO2 );

	for ( InducedNodeIndex = 1; InducedNodeIndex <= ZoneRetPlenCond( ZonePlenumNum ).NumInducedNodes; ++InducedNodeIndex ) {
		InducedNodeNum = ZoneRetPlenCond( ZonePlenumNum ).InducedNode( InducedNodeIndex );
		EXPECT_EQ( Node( InducedNodeNum ).MassFlowRate, ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRate( InducedNodeIndex ) );
		EXPECT_EQ( Node( InducedNodeNum ).MassFlowRateMaxAvail, ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRateMaxAvail( InducedNodeIndex ) );
		EXPECT_EQ( Node( InducedNodeNum ).MassFlowRateMinAvail, ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRateMinAvail( InducedNodeIndex ) );
		EXPECT_EQ( Node( ZoneNodeNum ).Temp, ZoneRetPlenCond( ZonePlenumNum ).InducedTemp( InducedNodeIndex ) );
		EXPECT_EQ( Node( ZoneNodeNum ).HumRat, ZoneRetPlenCond( ZonePlenumNum ).InducedHumRat( InducedNodeIndex ) );
		EXPECT_EQ( Node( ZoneNodeNum ).Enthalpy, ZoneRetPlenCond( ZonePlenumNum ).InducedEnthalpy( InducedNodeIndex ) );
		EXPECT_EQ( Node( ZoneNodeNum ).Press, ZoneRetPlenCond( ZonePlenumNum ).InducedPressure( InducedNodeIndex ) );
		EXPECT_EQ( Node( ZoneNodeNum ).CO2, ZoneRetPlenCond( ZonePlenumNum ).InducedCO2( InducedNodeIndex ) );
		EXPECT_EQ( Node( ZoneNodeNum ).GenContam, ZoneRetPlenCond( ZonePlenumNum ).InducedGenContam( InducedNodeIndex ) );
		EXPECT_EQ( Node( ZoneNodeNum ).Temp, ZoneRetPlenCond( ZonePlenumNum ).ZoneTemp );
		EXPECT_EQ( Node( ZoneNodeNum ).HumRat, ZoneRetPlenCond( ZonePlenumNum ).ZoneHumRat );
		EXPECT_EQ( Node( ZoneNodeNum ).Enthalpy, ZoneRetPlenCond( ZonePlenumNum ).ZoneEnthalpy );
	}

	// Deallocate everything
	ZoneRetPlenCond( ZonePlenumNum ).InletNode.deallocate();
	ZoneRetPlenCond( ZonePlenumNum ).InducedNode.deallocate();
	ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRate.deallocate();
	ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRateMaxAvail.deallocate();
	ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRateMinAvail.deallocate();
	ZoneRetPlenCond( ZonePlenumNum ).InducedTemp.deallocate();
	ZoneRetPlenCond( ZonePlenumNum ).InducedHumRat.deallocate();
	ZoneRetPlenCond( ZonePlenumNum ).InducedEnthalpy.deallocate();
	ZoneRetPlenCond( ZonePlenumNum ).InducedPressure.deallocate();
	ZoneRetPlenCond( ZonePlenumNum ).InducedCO2.deallocate();
	ZoneRetPlenCond( ZonePlenumNum ).InducedGenContam.deallocate();
	ZoneRetPlenCond.deallocate();
	Node.deallocate();

}
