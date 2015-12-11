// EnergyPlus::AirflowNetworkBalanceManager unit tests

// Google test headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <DataAirflowNetwork.hh>
#include <AirflowNetworkBalanceManager.hh>
#include <DataSurfaces.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataGlobals.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace AirflowNetworkBalanceManager;
using namespace DataAirflowNetwork;
using namespace DataSurfaces;
using namespace DataHeatBalance;
using namespace DataGlobals;

namespace EnergyPlus {

	TEST( AirflowNetworkBalanceManagerTest, TestOtherSideCoefficients )
	{

		ShowMessage( "Begin Test: AirflowNetworkBalanceManagerTest, TestOtherSideCoefficients" );

		int i = 2;

		AirflowNetworkNumOfExtSurfaces = 2;
		AirflowNetworkNumOfSurfaces = 2;

		MultizoneSurfaceData.allocate( i );
		Surface.allocate( i );
		Surface( 1 ).ExtBoundCond = -2;
		Surface( 2 ).ExtBoundCond = -2;
		Surface( 1 ).ExtWind = true;
		Surface( 2 ).ExtWind = true;
		Surface( 1 ).Tilt = 90.0;
		Surface( 2 ).Tilt = 90.0;
		Surface( 1 ).Azimuth = 0.0;
		Surface( 2 ).Azimuth = 180.0;

		MultizoneSurfaceData( 1 ).SurfNum = 1;
		MultizoneSurfaceData( 2 ).SurfNum = 2;

		CalcWindPressureCoeffs( );
		EXPECT_EQ( 1, MultizoneSurfaceData( 1 ).NodeNums( 2 ) );
		EXPECT_EQ( 2, MultizoneSurfaceData( 2 ).NodeNums( 2 ) );
		EXPECT_EQ( 1, MultizoneExternalNodeData( 1 ).CPVNum );
		EXPECT_EQ( 3, MultizoneExternalNodeData( 2 ).CPVNum );

		MultizoneSurfaceData.deallocate( );
		MultizoneExternalNodeData.deallocate( );
		Surface.deallocate( );
	}

	TEST_F( EnergyPlusFixture, TestZoneVentingSch ) {

		// Unit test for #5021

		Zone.allocate( 1 );
		Zone( 1 ).Name = "SALA DE AULA";

		Surface.allocate( 2 );
		Surface( 1 ).Name = "WINDOW AULA 1";
		Surface( 1 ).Zone = 1;
		Surface( 1 ).ZoneName = "SALA DE AULA";
		Surface( 1 ).Azimuth = 0.0;
		Surface( 1 ).ExtBoundCond = 0;
		Surface( 1 ).HeatTransSurf = true;
		Surface( 1 ).Tilt = 90.0;
		Surface( 2 ).Name = "WINDOW AULA 2";
		Surface( 2 ).Zone = 1;
		Surface( 2 ).ZoneName = "SALA DE AULA";
		Surface( 2 ).Azimuth = 180.0;
		Surface( 2 ).ExtBoundCond = 0;
		Surface( 2 ).HeatTransSurf = true;
		Surface( 2 ).Tilt = 90.0;

		SurfaceWindow.allocate( 2 );
		SurfaceWindow( 1 ).OriginalClass = 11;
		SurfaceWindow( 2 ).OriginalClass = 11;
		NumOfZones = 1;

		std::string const idf_objects = delimited_string( {
			"Version,8.3;",
			"Schedule:Constant,OnSch,,1.0;",
			"Schedule:Constant,Aula people sched,,0.0;",
			"Schedule:Constant,Sempre 21,,21.0;",
			"AirflowNetwork:SimulationControl,",
			"  NaturalVentilation, !- Name",
			"  MultizoneWithoutDistribution, !- AirflowNetwork Control",
			"  SurfaceAverageCalculation, !- Wind Pressure Coefficient Type",
			"  , !- AirflowNetwork Wind Pressure Coefficient Array Name",
			"  , !- Height Selection for Local Wind Pressure Calculation",
			"  LOWRISE, !- Building Type",
			"  1000, !- Maximum Number of Iterations{ dimensionless }",
			"  LinearInitializationMethod, !- Initialization Type",
			"  0.0001, !- Relative Airflow Convergence Tolerance{ dimensionless }",
			"  0.0001, !- Absolute Airflow Convergence Tolerance{ kg / s }",
			"  -0.5, !- Convergence Acceleration Limit{ dimensionless }",
			"  90, !- Azimuth Angle of Long Axis of Building{ deg }",
			"  0.36;                    !- Ratio of Building Width Along Short Axis to Width Along Long Axis",
			"AirflowNetwork:MultiZone:Zone,",
			"  sala de aula, !- Zone Name",
			"  Temperature, !- Ventilation Control Mode",
			"  Sempre 21, !- Ventilation Control Zone Temperature Setpoint Schedule Name",
			"  1, !- Minimum Venting Open Factor{ dimensionless }",
			"  , !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor{ deltaC }",
			"  100, !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor{ deltaC }",
			"  , !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor{ deltaJ / kg }",
			"  300000, !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor{ deltaJ / kg }",
			"  Aula people sched, !- Venting Availability Schedule Name",
			"  Standard;                !- Single Sided Wind Pressure Coefficient Algorithm",
			"AirflowNetwork:MultiZone:Surface,",
			"  window aula 1, !- Surface Name",
			"  Simple Window, !- Leakage Component Name",
			"  , !- External Node Name",
			"  1, !- Window / Door Opening Factor, or Crack Factor{ dimensionless }",
			"  ZoneLevel, !- Ventilation Control Mode",
			"  , !- Ventilation Control Zone Temperature Setpoint Schedule Name",
			"  , !- Minimum Venting Open Factor{ dimensionless }",
			"  , !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor{ deltaC }",
			"  100, !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor{ deltaC }",
			"  , !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor{ deltaJ / kg }",
			"  300000, !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor{ deltaJ / kg }",
			"  Aula people sched;       !- Venting Availability Schedule Name",
			"AirflowNetwork:MultiZone:Surface,",
			"  window aula 2, !- Surface Name",
			"  Simple Window, !- Leakage Component Name",
			"  , !- External Node Name",
			"  1, !- Window / Door Opening Factor, or Crack Factor{ dimensionless }",
			"  Temperature, !- Ventilation Control Mode",
			"  Sempre 21, !- Ventilation Control Zone Temperature Setpoint Schedule Name",
			"  1, !- Minimum Venting Open Factor{ dimensionless }",
			"  , !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor{ deltaC }",
			"  100, !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor{ deltaC }",
			"  , !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor{ deltaJ / kg }",
			"  300000, !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor{ deltaJ / kg }",
			"  Aula people sched;       !- Venting Availability Schedule Name",
			"AirflowNetwork:MultiZone:Component:SimpleOpening,",
			"  Simple Window, !- Name",
			"  0.0010, !- Air Mass Flow Coefficient When Opening is Closed{ kg / s - m }",
			"  0.65, !- Air Mass Flow Exponent When Opening is Closed{ dimensionless }",
			"  0.01, !- Minimum Density Difference for Two - Way Flow{ kg / m3 }",
			"  0.78;                    !- Discharge Coefficient{ dimensionless }",
		} );

		ASSERT_FALSE( process_idf( idf_objects ) );

		GetAirflowNetworkInput( );

		EXPECT_EQ( 2, MultizoneZoneData( 1 ).VentingSchNum );

		Zone.deallocate( );
		Surface.deallocate( );
		SurfaceWindow.deallocate( );

	}
}