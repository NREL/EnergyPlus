// EnergyPlus::ThermalComfort Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <EnergyPlus/ThermalComfort.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataRoomAirModel.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::ThermalComfort;
using namespace EnergyPlus::DataGlobals;
using namespace EnergyPlus::DataHVACGlobals;
using namespace ObjexxFCL;

using DataZoneEnergyDemands::ZoneSysEnergyDemand;
using DataHeatBalFanSys::TempControlType;
using DataRoomAirModel::AirModel;
using DataRoomAirModel::RoomAirModel_Mixing;
using DataHeatBalFanSys::ZTAV;
using DataHeatBalFanSys::ZoneThermostatSetPointLo;
using DataHeatBalFanSys::ZoneThermostatSetPointHi;


TEST( ThermalComfort, CalcIfSetPointMetTest1 )
{
	ShowMessage( "Begin Test: ThermalComfort, CalcIfSetPointMetTest1" );

	NumOfZones = 1;
	ZoneSysEnergyDemand.allocate( NumOfZones );
	ThermalComfortSetPoint.allocate( NumOfZones );
	TempControlType.allocate( 1 );
	AirModel.allocate( NumOfZones );
	AirModel( 1 ).AirModelType = RoomAirModel_Mixing;
	ZTAV.allocate( NumOfZones );
	ZoneThermostatSetPointLo.allocate( NumOfZones );
	ZoneThermostatSetPointHi.allocate( NumOfZones );
	TimeStepZone = 0.25;
	ThermalComfortInASH55.allocate( NumOfZones );
	ThermalComfortInASH55( 1 ).ZoneIsOccupied = true;

	// SingleHeatingSetPoint thermostat

	TempControlType( 1 ) = SingleHeatingSetPoint;

	//heating
	ZTAV( 1 ) = 21.1; // 70F
	ZoneThermostatSetPointLo( 1 ) = 22.2; // 72F
	ZoneSysEnergyDemand( 1 ).TotalOutputRequired = 500.0; // must be greater than zero
	CalcIfSetPointMet();
	EXPECT_EQ( TimeStepZone, ThermalComfortSetPoint( 1 ).notMetHeating );
	EXPECT_EQ( TimeStepZone, ThermalComfortSetPoint( 1 ).notMetHeatingOccupied );
	EXPECT_EQ( 0., ThermalComfortSetPoint( 1 ).notMetCooling );
	EXPECT_EQ( 0., ThermalComfortSetPoint( 1 ).notMetCoolingOccupied );

	//cooling
	ZTAV( 1 ) = 25.0; // 77F
	ZoneThermostatSetPointHi( 1 ) = 23.9; // 75F
	ZoneSysEnergyDemand( 1 ).TotalOutputRequired = -500.0; // must be less than zero
	CalcIfSetPointMet();
	EXPECT_EQ( 0, ThermalComfortSetPoint( 1 ).notMetHeating );
	EXPECT_EQ( 0, ThermalComfortSetPoint( 1 ).notMetHeatingOccupied );
	EXPECT_EQ( 0., ThermalComfortSetPoint( 1 ).notMetCooling );
	EXPECT_EQ( 0., ThermalComfortSetPoint( 1 ).notMetCoolingOccupied );

	// SingleCoolingSetPoint thermostat

	TempControlType( 1 ) = SingleCoolingSetPoint;

	//heating
	ZTAV( 1 ) = 21.1; // 70F
	ZoneThermostatSetPointLo( 1 ) = 22.2; // 72F
	ZoneSysEnergyDemand( 1 ).TotalOutputRequired = 500.0; // must be greater than zero
	CalcIfSetPointMet();
	EXPECT_EQ( 0, ThermalComfortSetPoint( 1 ).notMetHeating );
	EXPECT_EQ( 0, ThermalComfortSetPoint( 1 ).notMetHeatingOccupied );
	EXPECT_EQ( 0., ThermalComfortSetPoint( 1 ).notMetCooling );
	EXPECT_EQ( 0., ThermalComfortSetPoint( 1 ).notMetCoolingOccupied );

	//cooling
	ZTAV( 1 ) = 25.0; // 77F
	ZoneThermostatSetPointHi( 1 ) = 23.9; // 75F
	ZoneSysEnergyDemand( 1 ).TotalOutputRequired = -500.0; // must be less than zero
	CalcIfSetPointMet();
	EXPECT_EQ( 0, ThermalComfortSetPoint( 1 ).notMetHeating );
	EXPECT_EQ( 0, ThermalComfortSetPoint( 1 ).notMetHeatingOccupied );
	EXPECT_EQ( TimeStepZone, ThermalComfortSetPoint( 1 ).notMetCooling );
	EXPECT_EQ( TimeStepZone, ThermalComfortSetPoint( 1 ).notMetCoolingOccupied );

	// SingleHeatCoolSetPoint thermostat

	TempControlType( 1 ) = SingleHeatCoolSetPoint;

	//heating
	ZTAV( 1 ) = 21.1; // 70F
	ZoneThermostatSetPointLo( 1 ) = 22.2; // 72F
	ZoneSysEnergyDemand( 1 ).TotalOutputRequired = 500.0; // must be greater than zero
	CalcIfSetPointMet();
	EXPECT_EQ( TimeStepZone, ThermalComfortSetPoint( 1 ).notMetHeating );
	EXPECT_EQ( TimeStepZone, ThermalComfortSetPoint( 1 ).notMetHeatingOccupied );
	EXPECT_EQ( 0., ThermalComfortSetPoint( 1 ).notMetCooling );
	EXPECT_EQ( 0., ThermalComfortSetPoint( 1 ).notMetCoolingOccupied );

	//cooling
	ZTAV( 1 ) = 25.0; // 77F
	ZoneThermostatSetPointHi( 1 ) = 23.9; // 75F
	ZoneSysEnergyDemand( 1 ).TotalOutputRequired = -500.0; // must be less than zero
	CalcIfSetPointMet();
	EXPECT_EQ( 0, ThermalComfortSetPoint( 1 ).notMetHeating );
	EXPECT_EQ( 0, ThermalComfortSetPoint( 1 ).notMetHeatingOccupied );
	EXPECT_EQ( TimeStepZone, ThermalComfortSetPoint( 1 ).notMetCooling );
	EXPECT_EQ( TimeStepZone, ThermalComfortSetPoint( 1 ).notMetCoolingOccupied );

	// DualSetPointWithDeadBand thermostat

	TempControlType( 1 ) = DualSetPointWithDeadBand;

	//heating
	ZTAV( 1 ) = 21.1; // 70F
	ZoneThermostatSetPointLo( 1 ) = 22.2; // 72F
	ZoneSysEnergyDemand( 1 ).TotalOutputRequired = 500.0; // must be greater than zero
	CalcIfSetPointMet();
	EXPECT_EQ( TimeStepZone, ThermalComfortSetPoint( 1 ).notMetHeating );
	EXPECT_EQ( TimeStepZone, ThermalComfortSetPoint( 1 ).notMetHeatingOccupied );
	EXPECT_EQ( 0., ThermalComfortSetPoint( 1 ).notMetCooling );
	EXPECT_EQ( 0., ThermalComfortSetPoint( 1 ).notMetCoolingOccupied );

	//cooling
	ZTAV( 1 ) = 25.0; // 77F
	ZoneThermostatSetPointHi( 1 ) = 23.9; // 75F
	ZoneSysEnergyDemand( 1 ).TotalOutputRequired = -500.0; // must be less than zero
	CalcIfSetPointMet();
	EXPECT_EQ( 0, ThermalComfortSetPoint( 1 ).notMetHeating );
	EXPECT_EQ( 0, ThermalComfortSetPoint( 1 ).notMetHeatingOccupied );
	EXPECT_EQ( TimeStepZone, ThermalComfortSetPoint( 1 ).notMetCooling );
	EXPECT_EQ( TimeStepZone, ThermalComfortSetPoint( 1 ).notMetCoolingOccupied );

	//// clean up
	NumOfZones = 0;
	ZoneSysEnergyDemand.deallocate( );
	ThermalComfortSetPoint.deallocate( );
	TempControlType.deallocate( );
	AirModel.deallocate( );
	ZTAV.deallocate( );
	ZoneThermostatSetPointLo.deallocate( );
	ZoneThermostatSetPointHi.deallocate( );
	TimeStepZone = 0.;
	ThermalComfortInASH55.deallocate( );
}




