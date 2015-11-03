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
#include <SimAirServingZones.hh>
#include <BranchInputManager.hh>
#include <DataAirLoop.hh>
#include <DataAirSystems.hh>
#include <DataContaminantBalance.hh>
#include <DataConvergParams.hh>
#include <DataDefineEquip.hh>
#include <DataEnvironment.hh>
#include <DataGlobals.hh>
#include <DataHeatBalance.hh>
#include <DataHVACControllers.hh>
#include <DataHVACGlobals.hh>
#include <DataLoopNode.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DataSystemVariables.hh>
#include <DataZoneEquipment.hh>
#include <DesiccantDehumidifiers.hh>
#include <EMSManager.hh>
#include <EvaporativeCoolers.hh>
#include <Fans.hh>
#include <Furnaces.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <HeatingCoils.hh>
#include <HeatRecovery.hh>
#include <Humidifiers.hh>
#include <HVACControllers.hh>
#include <HVACDuct.hh>
#include <HVACDXHeatPumpSystem.hh>
#include <HVACDXSystem.hh>
#include <HVACHXAssistedCoolingCoil.hh>
#include <HVACInterfaceManager.hh>
#include <HVACMultiSpeedHeatPump.hh>
#include <HVACUnitaryBypassVAV.hh>
#include <HVACUnitarySystem.hh>
#include <InputProcessor.hh>
#include <MixedAir.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <OutputReportPredefined.hh>
#include <Psychrometrics.hh>
#include <ReportSizingManager.hh>
#include <SimAirServingZones.hh>
#include <SplitterComponent.hh>
#include <SteamCoils.hh>
#include <SystemAvailabilityManager.hh>
#include <UserDefinedComponents.hh>
#include <UtilityRoutines.hh>
#include <WaterCoils.hh>
#include <ZonePlenum.hh>

using namespace EnergyPlus;
using namespace DataPrecisionGlobals;
using namespace DataLoopNode;
using namespace DataAirLoop;
using namespace DataGlobals;
using namespace DataHVACGlobals;
using namespace DataSizing;
using namespace DataZoneEquipment;
using namespace DataAirSystems;
using namespace SimAirServingZones;
using DataEnvironment::TotDesDays;
using DataEnvironment::CurEnvirNum;
using DataEnvironment::EnvironmentName;
using DataEnvironment::CurMnDy;
using DataEnvironment::TotRunDesPersDays;

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
		EXPECT_EQ( 14.5, FinalZoneSizing( 2 ).DesHeatCoilInTempTU );
		EXPECT_EQ( 14.5, FinalZoneSizing( 3 ).DesHeatCoilInTempTU );
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
