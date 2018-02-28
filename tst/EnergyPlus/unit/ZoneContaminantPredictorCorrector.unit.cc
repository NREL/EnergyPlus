// EnergyPlus, Copyright (c) 1996-2018, The Board of Trustees of the University of Illinois,
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

// EnergyPlus::ZoneTempPredictorCorrector Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

#include "Fixtures/EnergyPlusFixture.hh"

// EnergyPlus Headers
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataZoneControls.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/ZonePlenum.hh>
#include <EnergyPlus/ZoneTempPredictorCorrector.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataAirflowNetwork.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/SimulationManager.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/WeatherManager.hh>
#include <EnergyPlus/ZoneContaminantPredictorCorrector.hh>
#include <EnergyPlus/DataContaminantBalance.hh>

using namespace EnergyPlus;
using namespace ObjexxFCL;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::DataHeatBalFanSys;
using namespace DataGlobals;
using namespace DataStringGlobals;
using namespace EnergyPlus::DataZoneControls;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::DataZoneEnergyDemands;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::HeatBalanceManager;
using namespace EnergyPlus::ZonePlenum;
using namespace EnergyPlus::ZoneTempPredictorCorrector;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::DataSurfaces;
using namespace EnergyPlus::DataEnvironment;
using namespace EnergyPlus::DataAirflowNetwork;
using namespace EnergyPlus::Psychrometrics;
using namespace SimulationManager;
using namespace EnergyPlus::ZoneContaminantPredictorCorrector;

TEST_F( EnergyPlusFixture, ZoneContaminantPredictorCorrector_AddMDotOATest )
{

	ShortenTimeStepSys = false;
	UseZoneTimeStepHistory = false;

	ZoneAirHumRat.allocate( 1 );
	ZT.allocate( 1 );
	MixingMassFlowZone.allocate( 1 );

	DataGlobals::NumOfZones = 1;
	DataContaminantBalance::Contaminant.CO2Simulation = true;
	DataContaminantBalance::Contaminant.GenericContamSimulation = true;

	DataContaminantBalance::AZ.allocate( 1 );
	DataContaminantBalance::BZ.allocate( 1 );
	DataContaminantBalance::CZ.allocate( 1 );
	DataContaminantBalance::AZGC.allocate( 1 );
	DataContaminantBalance::BZGC.allocate( 1 );
	DataContaminantBalance::CZGC.allocate( 1 );

	DataContaminantBalance::CO2ZoneTimeMinus1Temp.allocate( 1 );
	DataContaminantBalance::CO2ZoneTimeMinus2Temp.allocate( 1 );
	DataContaminantBalance::CO2ZoneTimeMinus3Temp.allocate( 1 );
	DataContaminantBalance::DSCO2ZoneTimeMinus1.allocate( 1 );
	DataContaminantBalance::DSCO2ZoneTimeMinus2.allocate( 1 );
	DataContaminantBalance::DSCO2ZoneTimeMinus3.allocate( 1 );
	DataContaminantBalance::GCZoneTimeMinus1Temp.allocate( 1 );
	DataContaminantBalance::GCZoneTimeMinus2Temp.allocate( 1 );
	DataContaminantBalance::GCZoneTimeMinus3Temp.allocate( 1 );
	DataContaminantBalance::DSGCZoneTimeMinus1.allocate( 1 );
	DataContaminantBalance::DSGCZoneTimeMinus2.allocate( 1 );
	DataContaminantBalance::DSGCZoneTimeMinus3.allocate( 1 );

	DataContaminantBalance::MixingMassFlowCO2.allocate( 1 );
	DataContaminantBalance::MixingMassFlowGC.allocate( 1 );
	DataContaminantBalance::ZoneAirCO2Temp.allocate( 1 );
	DataContaminantBalance::ZoneCO21.allocate( 1 );
	DataContaminantBalance::ZoneAirCO2.allocate( 1 );
	DataContaminantBalance::ZoneAirGCTemp.allocate( 1 );
	DataContaminantBalance::ZoneGC1.allocate( 1 );
	DataContaminantBalance::ZoneAirGC.allocate( 1 );
	DataContaminantBalance::ZoneCO2SetPoint.allocate( 1 );
	DataContaminantBalance::CO2PredictedRate.allocate( 1 );
	DataContaminantBalance::GCPredictedRate.allocate( 1 );
	DataContaminantBalance::ContaminantControlledZone.allocate( 1 );
	DataContaminantBalance::ZoneGCSetPoint.allocate( 1 );

	DataContaminantBalance::ZoneAirDensityCO.allocate( 1 );
	DataContaminantBalance::ZoneCO2Gain.allocate( 1 );
	DataContaminantBalance::ZoneGCGain.allocate( 1 );
	DataContaminantBalance::ZoneCO2Gain( 1 ) = 0.0001;
	DataContaminantBalance::ZoneGCGain( 1 ) = 0.0000001;
	DataContaminantBalance::MixingMassFlowCO2( 1 ) = 0.0;
	DataContaminantBalance::MixingMassFlowGC( 1 ) = 0.0;
	
	DataContaminantBalance::DSCO2ZoneTimeMinus1( 1 ) = 200.0;
	DataContaminantBalance::DSCO2ZoneTimeMinus2( 1 ) = 200.0;
	DataContaminantBalance::DSCO2ZoneTimeMinus3( 1 ) = 200.0;
	DataContaminantBalance::OutdoorCO2 = 400.0;
	DataContaminantBalance::OutdoorGC = 0.001;
	DataContaminantBalance::ZoneCO21( 1 ) = DataContaminantBalance::OutdoorCO2;
	DataContaminantBalance::ZoneGC1( 1 ) = DataContaminantBalance::OutdoorGC;
	DataContaminantBalance::ZoneCO2SetPoint( 1 ) = 450.0; 
	DataContaminantBalance::ZoneAirCO2( 1 ) = DataContaminantBalance::ZoneCO21( 1 );
	DataContaminantBalance::ZoneAirGC( 1 ) = DataContaminantBalance::ZoneGC1( 1 );

	Real64 PriorTimeStep;

	TimeStepSys = 15.0 / 60.0; // System timestep in hours
	PriorTimeStep = TimeStepSys;

	ZoneEquipConfig.allocate( 1 );
	ZoneEquipConfig( 1 ).ZoneName = "Zone 1";
	ZoneEquipConfig( 1 ).ActualZoneNum = 1;

	ZoneEquipConfig( 1 ).NumInletNodes = 2; 
	ZoneEquipConfig( 1 ).InletNode.allocate( 2 );
	ZoneEquipConfig( 1 ).InletNode( 1 ) = 1;
	ZoneEquipConfig( 1 ).InletNode( 2 ) = 2;
	ZoneEquipConfig( 1 ).NumExhaustNodes = 1;
	ZoneEquipConfig( 1 ).ExhaustNode.allocate( 1 );
	ZoneEquipConfig( 1 ).ExhaustNode( 1 ) = 3;
	ZoneEquipConfig( 1 ).NumReturnNodes = 1;
	ZoneEquipConfig( 1 ).ReturnNode.allocate( 1 );
	ZoneEquipConfig( 1 ).ReturnNode( 1 ) = 4;

	Node.allocate( 5 );

	Zone.allocate( 1 );
	Zone( 1 ).Name = ZoneEquipConfig( 1 ).ZoneName;
	Zone( 1 ).ZoneEqNum = 1;
	ZoneEqSizing.allocate( 1 );
	CurZoneEqNum = 1;
	Zone( 1 ).Multiplier = 1.0;
	Zone( 1 ).Volume = 1000.0;
	Zone( 1 ).SystemZoneNodeNumber = 5;
	Zone( 1 ).ZoneVolCapMultpMoist = 1.0;
	OutBaroPress = 101325.0;

	NumZoneReturnPlenums = 0;
	NumZoneSupplyPlenums = 0;

	OAMFL.allocate( 1 );
	VAMFL.allocate( 1 );
	EAMFL.allocate( 1 );
	CTMFL.allocate( 1 );
	MDotOA.allocate( 1 );
	MDotOA( 1 ) = 0.001;
	ScheduleManager::Schedule.allocate( 1 );

	ScheduleManager::Schedule( 1 ).CurrentValue = 1.0;

	SimulateAirflowNetwork = 0;

	ZoneAirSolutionAlgo = UseEulerMethod;

	Node( 1 ).MassFlowRate = 0.01; // Zone inlet node 1
	Node( 1 ).HumRat = 0.008;
	Node( 2 ).MassFlowRate = 0.02; // Zone inlet node 2
	Node( 2 ).HumRat = 0.008;
	ZoneEquipConfig( 1 ).ZoneExhBalanced = 0.0;
	Node( 3 ).MassFlowRate = 0.00; // Zone exhaust node 1
	ZoneEquipConfig( 1 ).ZoneExh = Node( 3 ).MassFlowRate;
	Node( 3 ).HumRat = 0.008;
	Node( 4 ).MassFlowRate = 0.03; // Zone return node
	Node( 4 ).HumRat = 0.000;
	Node( 5 ).HumRat = 0.000;
	OAMFL( 1 ) = 0.0;
	VAMFL( 1 ) = 0.0;
	EAMFL( 1 ) = 0.0;
	CTMFL( 1 ) = 0.0;
	ZoneAirHumRat( 1 ) = 0.008;
	ZT( 1 ) = 24.0;
	MixingMassFlowZone( 1 ) = 0.0;

	DataContaminantBalance::CO2PredictedRate.allocate( 1 );
	DataContaminantBalance::ZoneSysContDemand.allocate( 1 );
	DataContaminantBalance::NumContControlledZones = 1;

	DataContaminantBalance::ContaminantControlledZone.allocate( 1 );

	DataContaminantBalance::ContaminantControlledZone( 1 ).AvaiSchedPtr = 1;
	DataContaminantBalance::ContaminantControlledZone( 1 ).ActualZoneNum = 1;
	DataContaminantBalance::ContaminantControlledZone( 1 ).NumOfZones = 1;
	DataContaminantBalance::ZoneGCSetPoint( 1 ) = 0.0025;

	PredictZoneContaminants( ShortenTimeStepSys, UseZoneTimeStepHistory, PriorTimeStep );
	EXPECT_NEAR( 1.041692180, DataContaminantBalance::CO2PredictedRate( 1 ), 0.00001 );
	EXPECT_NEAR( 76.89754831, DataContaminantBalance::GCPredictedRate( 1 ), 0.00001 );
	

	CorrectZoneContaminants( ShortenTimeStepSys, UseZoneTimeStepHistory, PriorTimeStep );
	EXPECT_NEAR( 490.0, Node( 5 ).CO2, 0.00001 );
	EXPECT_NEAR( 0.0909999, Node( 5 ).GenContam, 0.00001 );

	DataContaminantBalance::Contaminant.CO2Simulation = false;
	DataContaminantBalance::Contaminant.GenericContamSimulation = false;

	// Deallocate everything
	ZoneAirHumRat.deallocate( );
	ZT.deallocate( );
	MixingMassFlowZone.deallocate( );
	ZoneEquipConfig( 1 ).InletNode.deallocate();
	ZoneEquipConfig( 1 ).ExhaustNode.deallocate();
	ZoneEquipConfig.deallocate();
	Node.deallocate();
	Zone.deallocate();
	ZoneEqSizing.deallocate();
	OAMFL.deallocate();
	VAMFL.deallocate();
	EAMFL.deallocate();
	CTMFL.deallocate();
	MDotOA.deallocate();
	DataContaminantBalance::AZ.deallocate( );
	DataContaminantBalance::BZ.deallocate( );
	DataContaminantBalance::CZ.deallocate( );
	DataContaminantBalance::AZGC.deallocate( );
	DataContaminantBalance::BZGC.deallocate( );
	DataContaminantBalance::CZGC.deallocate( );
	DataContaminantBalance::CO2ZoneTimeMinus1Temp.deallocate( );
	DataContaminantBalance::CO2ZoneTimeMinus2Temp.deallocate( );
	DataContaminantBalance::CO2ZoneTimeMinus3Temp.deallocate( );
	DataContaminantBalance::DSCO2ZoneTimeMinus1.deallocate( );
	DataContaminantBalance::DSCO2ZoneTimeMinus2.deallocate( );
	DataContaminantBalance::DSCO2ZoneTimeMinus3.deallocate( );
	DataContaminantBalance::GCZoneTimeMinus1Temp.deallocate( );
	DataContaminantBalance::GCZoneTimeMinus2Temp.deallocate( );
	DataContaminantBalance::GCZoneTimeMinus3Temp.deallocate( );
	DataContaminantBalance::DSGCZoneTimeMinus1.deallocate( );
	DataContaminantBalance::DSGCZoneTimeMinus2.deallocate( );
	DataContaminantBalance::DSGCZoneTimeMinus3.deallocate( );
	DataContaminantBalance::ZoneAirDensityCO.deallocate( );
	DataContaminantBalance::ZoneCO2Gain.deallocate( );
	DataContaminantBalance::ZoneGCGain.deallocate( );
	DataContaminantBalance::MixingMassFlowCO2.deallocate( );
	DataContaminantBalance::MixingMassFlowGC.deallocate( );
	DataContaminantBalance::ZoneAirCO2Temp.deallocate( );
	DataContaminantBalance::ZoneCO21.deallocate( );
	DataContaminantBalance::ZoneAirCO2.deallocate( );
	DataContaminantBalance::ZoneAirGCTemp.deallocate( );
	DataContaminantBalance::ZoneGC1.deallocate( );
	DataContaminantBalance::ZoneAirGC.deallocate( );
	DataContaminantBalance::CO2PredictedRate.deallocate( );
	DataContaminantBalance::GCPredictedRate.deallocate( );
	DataContaminantBalance::ZoneSysContDemand.deallocate( );
	DataContaminantBalance::ContaminantControlledZone.deallocate( );
	DataContaminantBalance::ZoneGCSetPoint.deallocate( );
	ScheduleManager::Schedule.deallocate( );

}


