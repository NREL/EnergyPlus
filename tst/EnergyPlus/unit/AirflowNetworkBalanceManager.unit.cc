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

	TEST_F( EnergyPlusFixture, AirflowNetworkBalanceManagerTest_TestOtherSideCoefficients )
	{

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

		CalcWindPressureCoeffs();
		EXPECT_EQ( 1, MultizoneSurfaceData( 1 ).NodeNums( 2 ) );
		EXPECT_EQ( 2, MultizoneSurfaceData( 2 ).NodeNums( 2 ) );
		EXPECT_EQ( 1, MultizoneExternalNodeData( 1 ).CPVNum );
		EXPECT_EQ( 3, MultizoneExternalNodeData( 2 ).CPVNum );

		MultizoneSurfaceData.deallocate();
		MultizoneExternalNodeData.deallocate();
		Surface.deallocate();
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

		GetAirflowNetworkInput();

		EXPECT_EQ( 2, MultizoneZoneData( 1 ).VentingSchNum );

		Zone.deallocate();
		Surface.deallocate();
		SurfaceWindow.deallocate();

	}

	TEST_F( EnergyPlusFixture, AirflowNetworkBalanceManager_TestTriangulerWindowWarning ) {

		// Unit test for #5384

		Zone.allocate( 1 );
		Zone( 1 ).Name = "WEST_ZONE";

		Surface.allocate( 3 );
		Surface( 1 ).Name = "SURFACE_1";
		Surface( 1 ).Zone = 1;
		Surface( 1 ).ZoneName = "WEST_ZONE";
		Surface( 1 ).Azimuth = 0.0;
		Surface( 1 ).ExtBoundCond = 0;
		Surface( 1 ).HeatTransSurf = true;
		Surface( 1 ).Tilt = 90.0;
		Surface( 1 ).Sides = 4;
		Surface( 2 ).Name = "SURFACE_2";
		Surface( 2 ).Zone = 1;
		Surface( 2 ).ZoneName = "WEST_ZONE";
		Surface( 2 ).Azimuth = 180.0;
		Surface( 2 ).ExtBoundCond = 0;
		Surface( 2 ).HeatTransSurf = true;
		Surface( 2 ).Tilt = 90.0;
		Surface( 2 ).Sides = 4;
		Surface( 3 ).Name = "WINDOW1";
		Surface( 3 ).Zone = 1;
		Surface( 3 ).ZoneName = "WEST_ZONE";
		Surface( 3 ).Azimuth = 180.0;
		Surface( 3 ).ExtBoundCond = 0;
		Surface( 3 ).HeatTransSurf = true;
		Surface( 3 ).Tilt = 90.0;
		Surface( 3 ).Sides = 3;

		SurfaceWindow.allocate( 3 );
		SurfaceWindow( 1 ).OriginalClass = 11;
		SurfaceWindow( 2 ).OriginalClass = 11;
		SurfaceWindow( 3 ).OriginalClass = 11;
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
			"  WEST_ZONE, !- Zone Name",
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
			"  Surface_1, !- Surface Name",
			"  CR-1, !- Leakage Component Name",
			"  , !- External Node Name",
			"  1; !- Window / Door Opening Factor, or Crack Factor{ dimensionless }",
			"AirflowNetwork:MultiZone:Surface,",
			"  Surface_2, !- Surface Name",
			"  CR-1, !- Leakage Component Name",
			"  , !- External Node Name",
			"  1; !- Window / Door Opening Factor, or Crack Factor{ dimensionless }",
			"AirflowNetwork:MultiZone:Surface,",
			"  Window1, !- Surface Name",
			"  Simple Window, !- Leakage Component Name",
			"  , !- External Node Name",
			"  1; !- Window / Door Opening Factor, or Crack Factor{ dimensionless }",
			"AirflowNetwork:MultiZone:Component:SimpleOpening,",
			"  Simple Window, !- Name",
			"  0.0010, !- Air Mass Flow Coefficient When Opening is Closed{ kg / s - m }",
			"  0.65, !- Air Mass Flow Exponent When Opening is Closed{ dimensionless }",
			"  0.01, !- Minimum Density Difference for Two - Way Flow{ kg / m3 }",
			"  0.78;                    !- Discharge Coefficient{ dimensionless }",
			"AirflowNetwork:MultiZone:ReferenceCrackConditions,",
			"  ReferenceCrackConditions, !- Name",
			"  20.0, !- Reference Temperature{ C }",
			"  101320, !- Reference Barometric Pressure{ Pa }",
			"  0.005;                   !- Reference Humidity Ratio{ kgWater / kgDryAir }",
			"AirflowNetwork:MultiZone:Surface:Crack,",
			"  CR-1, !- Name",
			"  0.01, !- Air Mass Flow Coefficient at Reference Conditions{ kg / s }",
			"  0.667, !- Air Mass Flow Exponent{ dimensionless }",
			"  ReferenceCrackConditions; !- Reference Crack Conditions",
		} );

		ASSERT_FALSE( process_idf( idf_objects ) );

		GetAirflowNetworkInput( );
		std::string const error_string = delimited_string( {
			"   ** Warning ** GetAirflowNetworkInput: AirflowNetwork:MultiZone:Surface=\"WINDOW1\".",
			"   **   ~~~   ** The opening is a Triangular subsurface. A rectangular subsurface will be used with effective width and height.",
		} );

		EXPECT_TRUE( compare_err_stream( error_string, true ) );

		AirflowNetworkNodeData.deallocate( );
		AirflowNetworkCompData.deallocate( );
		MultizoneExternalNodeData.deallocate( );
		Zone.deallocate( );
		Surface.deallocate( );
		SurfaceWindow.deallocate( );

	}

}
