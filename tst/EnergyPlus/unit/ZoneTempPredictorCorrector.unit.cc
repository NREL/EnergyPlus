// EnergyPlus::ZoneTempPredictorCorrector Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/ZonePlenum.hh>
#include <EnergyPlus/ZoneTempPredictorCorrector.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataAirflowNetwork.hh>
#include <EnergyPlus/Psychrometrics.hh>

using namespace EnergyPlus;
using namespace ObjexxFCL;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::DataHeatBalFanSys;
using namespace DataGlobals;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::ZonePlenum;
using namespace EnergyPlus::ZoneTempPredictorCorrector;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::DataSurfaces;
using namespace EnergyPlus::DataEnvironment;
using namespace EnergyPlus::DataAirflowNetwork;
using namespace EnergyPlus::Psychrometrics;


TEST( ZoneTempPredictorCorrector, CorrectZoneHumRatTest )
{
	ShowMessage( "Begin Test: ZoneTempPredictorCorrector, CorrectZoneHumRatTest" );

	InitializePsychRoutines();

	TimeStepSys = 15.0 / 60.0; // System timestep in hours

	ZoneEquipConfig.allocate( 1 );
	ZoneEquipConfig( 1 ).ZoneName = "Zone 1";
	ZoneEquipConfig( 1 ).ActualZoneNum = 1;
	std::vector< int > controlledZoneEquipConfigNums;
	controlledZoneEquipConfigNums.push_back( 1 );

	ZoneEquipConfig( 1 ).NumInletNodes = 2;
	ZoneEquipConfig( 1 ).InletNode.allocate( 2 );
	ZoneEquipConfig( 1 ).InletNode( 1 ) = 1;
	ZoneEquipConfig( 1 ).InletNode( 2 ) = 2;
	ZoneEquipConfig( 1 ).NumExhaustNodes = 1;
	ZoneEquipConfig( 1 ).ExhaustNode.allocate( 1 );
	ZoneEquipConfig( 1 ).ExhaustNode( 1 ) = 3;
	ZoneEquipConfig( 1 ).ReturnAirNode = 4;

	Node.allocate( 5 );

	Zone.allocate( 1 );
	Zone( 1 ).Name = ZoneEquipConfig( 1 ).ZoneName;
	ZoneEqSizing.allocate( 1 );
	CurZoneEqNum = 1;
	Zone( 1 ).Multiplier = 1.0;
	Zone( 1 ).Volume = 1000.0;
	Zone( 1 ).SystemZoneNodeNumber = 5;
	ZoneVolCapMultpMoist = 1.0;
	ZoneLatentGain.allocate( 1 );
	ZoneLatentGain( 1 ) = 0.0;
	SumLatentHTRadSys.allocate( 1 );
	SumLatentHTRadSys( 1 ) = 0.0;
	SumLatentPool.allocate( 1 );
	SumLatentPool( 1 ) = 0.0;
	OutBaroPress = 101325.0;
	ZT.allocate( 1 ); // Zone temperature C
	ZT( 1 ) = 24.0;
	ZoneAirHumRat.allocate( 1 );

	Zone( 1 ).SurfaceFirst = 1;
	Zone( 1 ).SurfaceLast = 2;
	Surface.allocate( 2);

	NumZoneReturnPlenums = 0;
	NumZoneSupplyPlenums = 0;

	OAMFL.allocate( 1 );
	VAMFL.allocate( 1 );
	EAMFL.allocate( 1 );
	CTMFL.allocate( 1 );

	SumHmARaW.allocate( 1 );
	SumHmARa.allocate( 1 );
	MixingMassFlowXHumRat.allocate( 1 );
	MixingMassFlowZone.allocate( 1 );
	SimulateAirflowNetwork = 0;
	MDotOA.allocate( 1 );

	ZoneAirSolutionAlgo = UseEulerMethod;
	ZoneAirHumRatTemp.allocate( 1 );
	ZoneW1.allocate( 1 );


// Case 1 - All flows at the same humrat
	ZoneW1( 1 ) = 0.008;
	Node( 1 ).MassFlowRate = 0.01; // Zone inlet node 1
	Node( 1 ).HumRat = 0.008;
	Node( 2 ).MassFlowRate = 0.02; // Zone inlet node 2
	Node( 2 ).HumRat = 0.008;
	ZoneEquipConfig( 1 ).ZoneExhBalanced = 0.0;
	Node( 3 ).MassFlowRate = 0.00; // Zone exhaust node 1
	Node( 3 ).HumRat = ZoneW1( 1 );
	Node( 4 ).MassFlowRate = 0.03; // Zone return node
	Node( 4 ).HumRat = 0.000;
	Node( 5 ).HumRat = 0.000;
	ZoneAirHumRat( 1 ) = 0.008;
	OAMFL( 1 ) = 0.0;
	VAMFL( 1 ) = 0.0;
	EAMFL( 1 ) = 0.0;
	CTMFL( 1 ) = 0.0;
	OutHumRat = 0.008;
	MixingMassFlowXHumRat( 1 ) = 0.0;
	MixingMassFlowZone( 1 ) = 0.0;
	MDotOA( 1 ) = 0.0;

	CorrectZoneHumRat( 1, controlledZoneEquipConfigNums );
	EXPECT_EQ( 0.008, Node( 5 ).HumRat );

	// Case 2 - Unbalanced exhaust flow
	ZoneW1( 1 ) = 0.008;
	Node( 1 ).MassFlowRate = 0.01; // Zone inlet node 1
	Node( 1 ).HumRat = 0.008;
	Node( 2 ).MassFlowRate = 0.02; // Zone inlet node 2
	Node( 2 ).HumRat = 0.008;
	ZoneEquipConfig( 1 ).ZoneExhBalanced = 0.0;
	Node( 3 ).MassFlowRate = 0.02; // Zone exhaust node 1
	Node( 3 ).HumRat = ZoneW1( 1 );
	Node( 4 ).MassFlowRate = 0.01; // Zone return node
	Node( 4 ).HumRat = ZoneW1( 1 );
	Node( 5 ).HumRat = 0.000;
	ZoneAirHumRat( 1 ) = 0.008;
	OAMFL( 1 ) = 0.0;
	VAMFL( 1 ) = 0.0;
	EAMFL( 1 ) = 0.0;
	CTMFL( 1 ) = 0.0;
	OutHumRat = 0.004;
	MixingMassFlowXHumRat( 1 ) = 0.0;
	MixingMassFlowZone( 1 ) = 0.0;
	MDotOA( 1 ) = 0.0;

	CorrectZoneHumRat( 1, controlledZoneEquipConfigNums );
	EXPECT_EQ( 0.008, Node( 5 ).HumRat );

	// Case 3 - Balanced exhaust flow with proper source flow from mixing
	ZoneW1( 1 ) = 0.008;
	Node( 1 ).MassFlowRate = 0.01; // Zone inlet node 1
	Node( 1 ).HumRat = 0.008;
	Node( 2 ).MassFlowRate = 0.02; // Zone inlet node 2
	Node( 2 ).HumRat = 0.008;
	ZoneEquipConfig( 1 ).ZoneExhBalanced = 0.02;
	Node( 3 ).MassFlowRate = 0.02; // Zone exhaust node 1
	Node( 3 ).HumRat = ZoneW1( 1 );
	Node( 4 ).MassFlowRate = 0.03; // Zone return node
	Node( 4 ).HumRat = ZoneW1( 1 );
	Node( 5 ).HumRat = 0.000;
	ZoneAirHumRat( 1 ) = 0.008;
	OAMFL( 1 ) = 0.0;
	VAMFL( 1 ) = 0.0;
	EAMFL( 1 ) = 0.0;
	CTMFL( 1 ) = 0.0;
	OutHumRat = 0.004;
	MixingMassFlowXHumRat( 1 ) = 0.02 * 0.008;
	MixingMassFlowZone( 1 ) = 0.02;
	MDotOA( 1 ) = 0.0;

	CorrectZoneHumRat( 1, controlledZoneEquipConfigNums );
	EXPECT_EQ( 0.008, Node( 5 ).HumRat );

	// Case 4 - Balanced exhaust flow without source flow from mixing
	ZoneW1( 1 ) = 0.008;
	Node( 1 ).MassFlowRate = 0.01; // Zone inlet node 1
	Node( 1 ).HumRat = 0.008;
	Node( 2 ).MassFlowRate = 0.02; // Zone inlet node 2
	Node( 2 ).HumRat = 0.008;
	ZoneEquipConfig( 1 ).ZoneExhBalanced = 0.02;
	Node( 3 ).MassFlowRate = 0.02; // Zone exhaust node 1
	Node( 3 ).HumRat = ZoneW1( 1 );
	Node( 4 ).MassFlowRate = 0.01; // Zone return node
	Node( 4 ).HumRat = ZoneW1( 1 );
	Node( 5 ).HumRat = 0.000;
	ZoneAirHumRat( 1 ) = 0.008;
	OAMFL( 1 ) = 0.0;
	VAMFL( 1 ) = 0.0;
	EAMFL( 1 ) = 0.0;
	CTMFL( 1 ) = 0.0;
	OutHumRat = 0.004;
	MixingMassFlowXHumRat( 1 ) = 0.0;
	MixingMassFlowZone( 1 ) = 0.0;
	MDotOA( 1 ) = 0.0;

	CorrectZoneHumRat( 1, controlledZoneEquipConfigNums );
	EXPECT_FALSE( (0.008 == Node( 5 ).HumRat) );

	// Deallocate everything
	ZoneEquipConfig( 1 ).InletNode.deallocate();
	ZoneEquipConfig( 1 ).ExhaustNode.deallocate();
	ZoneEquipConfig.deallocate();
	Node.deallocate();
	Zone.deallocate();
	ZoneLatentGain.deallocate();
	ZoneEqSizing.deallocate();
	SumLatentHTRadSys.deallocate();
	SumLatentPool.deallocate();
	ZT.deallocate(); // Zone temperature C
	ZoneAirHumRat.deallocate();
	Surface.deallocate();
	OAMFL.deallocate();
	VAMFL.deallocate();
	EAMFL.deallocate();
	CTMFL.deallocate();
	SumHmARaW.deallocate();
	SumHmARa.deallocate();
	MixingMassFlowXHumRat.deallocate();
	MixingMassFlowZone.deallocate();
	MDotOA.deallocate();
	ZoneAirHumRatTemp.deallocate();
	ZoneW1.deallocate();

}
