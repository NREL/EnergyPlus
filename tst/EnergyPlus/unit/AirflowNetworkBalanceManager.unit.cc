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
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/SurfaceGeometry.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/ScheduleManager.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace AirflowNetworkBalanceManager;
using namespace DataAirflowNetwork;
using namespace DataSurfaces;
using namespace DataHeatBalance;
using namespace DataGlobals;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::ScheduleManager;

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

	TEST_F( EnergyPlusFixture, TestAFNPressureStat ) {

		// Unit test for a new feature of PressureStat
		int i;


		std::string const idf_objects = delimited_string( {
			"Version,8.4;",
			"  Building,",
			"    Small Office with AirflowNetwork model,  !- Name",
			"    0,                       !- North Axis {deg}",
			"    Suburbs,                 !- Terrain",
			"    0.001,                   !- Loads Convergence Tolerance Value",
			"    0.0050000,               !- Temperature Convergence Tolerance Value {deltaC}",
			"    FullInteriorAndExterior, !- Solar Distribution",
			"    25,                      !- Maximum Number of Warmup Days",
			"    6;                       !- Minimum Number of Warmup Days",

			"  Timestep,6;",

			"  SurfaceConvectionAlgorithm:Inside,TARP;",

			"  SurfaceConvectionAlgorithm:Outside,DOE-2;",

			"  HeatBalanceAlgorithm,ConductionTransferFunction;",

			"  Schedule:Constant,FanAndCoilAvailSched,1.0;",
			"  Schedule:Constant,On,1.0;",
			"  Schedule:Constant,WindowVentSched,21.0;",
			"  Schedule:Constant,VentingSched,0.0;",

			"  Site:GroundTemperature:BuildingSurface,20.03,20.03,20.13,20.30,20.43,20.52,20.62,20.77,20.78,20.55,20.44,20.20;",

			"  Material,",
			"    A1 - 1 IN STUCCO,        !- Name",
			"    Smooth,                  !- Roughness",
			"    2.5389841E-02,           !- Thickness {m}",
			"    0.6918309,               !- Conductivity {W/m-K}",
			"    1858.142,                !- Density {kg/m3}",
			"    836.8000,                !- Specific Heat {J/kg-K}",
			"    0.9000000,               !- Thermal Absorptance",
			"    0.9200000,               !- Solar Absorptance",
			"    0.9200000;               !- Visible Absorptance",

			"  Material,",
			"    C4 - 4 IN COMMON BRICK,  !- Name",
			"    Rough,                   !- Roughness",
			"    0.1014984,               !- Thickness {m}",
			"    0.7264224,               !- Conductivity {W/m-K}",
			"    1922.216,                !- Density {kg/m3}",
			"    836.8000,                !- Specific Heat {J/kg-K}",
			"    0.9000000,               !- Thermal Absorptance",
			"    0.7600000,               !- Solar Absorptance",
			"    0.7600000;               !- Visible Absorptance",

			"  Material,",
			"    E1 - 3 / 4 IN PLASTER OR GYP BOARD,  !- Name",
			"    Smooth,                  !- Roughness",
			"    1.9050000E-02,           !- Thickness {m}",
			"    0.7264224,               !- Conductivity {W/m-K}",
			"    1601.846,                !- Density {kg/m3}",
			"    836.8000,                !- Specific Heat {J/kg-K}",
			"    0.9000000,               !- Thermal Absorptance",
			"    0.9200000,               !- Solar Absorptance",
			"    0.9200000;               !- Visible Absorptance",

			"  Material,",
			"    C6 - 8 IN CLAY TILE,     !- Name",
			"    Smooth,                  !- Roughness",
			"    0.2033016,               !- Thickness {m}",
			"    0.5707605,               !- Conductivity {W/m-K}",
			"    1121.292,                !- Density {kg/m3}",
			"    836.8000,                !- Specific Heat {J/kg-K}",
			"    0.9000000,               !- Thermal Absorptance",
			"    0.8200000,               !- Solar Absorptance",
			"    0.8200000;               !- Visible Absorptance",

			"  Material,",
			"    C10 - 8 IN HW CONCRETE,  !- Name",
			"    MediumRough,             !- Roughness",
			"    0.2033016,               !- Thickness {m}",
			"    1.729577,                !- Conductivity {W/m-K}",
			"    2242.585,                !- Density {kg/m3}",
			"    836.8000,                !- Specific Heat {J/kg-K}",
			"    0.9000000,               !- Thermal Absorptance",
			"    0.6500000,               !- Solar Absorptance",
			"    0.6500000;               !- Visible Absorptance",

			"  Material,",
			"    E2 - 1 / 2 IN SLAG OR STONE,  !- Name",
			"    Rough,                   !- Roughness",
			"    1.2710161E-02,           !- Thickness {m}",
			"    1.435549,                !- Conductivity {W/m-K}",
			"    881.0155,                !- Density {kg/m3}",
			"    1673.600,                !- Specific Heat {J/kg-K}",
			"    0.9000000,               !- Thermal Absorptance",
			"    0.5500000,               !- Solar Absorptance",
			"    0.5500000;               !- Visible Absorptance",

			"  Material,",
			"    E3 - 3 / 8 IN FELT AND MEMBRANE,  !- Name",
			"    Rough,                   !- Roughness",
			"    9.5402403E-03,           !- Thickness {m}",
			"    0.1902535,               !- Conductivity {W/m-K}",
			"    1121.292,                !- Density {kg/m3}",
			"    1673.600,                !- Specific Heat {J/kg-K}",
			"    0.9000000,               !- Thermal Absorptance",
			"    0.7500000,               !- Solar Absorptance",
			"    0.7500000;               !- Visible Absorptance",

			"  Material,",
			"    B5 - 1 IN DENSE INSULATION,  !- Name",
			"    VeryRough,               !- Roughness",
			"    2.5389841E-02,           !- Thickness {m}",
			"    4.3239430E-02,           !- Conductivity {W/m-K}",
			"    91.30524,                !- Density {kg/m3}",
			"    836.8000,                !- Specific Heat {J/kg-K}",
			"    0.9000000,               !- Thermal Absorptance",
			"    0.5000000,               !- Solar Absorptance",
			"    0.5000000;               !- Visible Absorptance",

			"  Material,",
			"    C12 - 2 IN HW CONCRETE,  !- Name",
			"    MediumRough,             !- Roughness",
			"    5.0901599E-02,           !- Thickness {m}",
			"    1.729577,                !- Conductivity {W/m-K}",
			"    2242.585,                !- Density {kg/m3}",
			"    836.8000,                !- Specific Heat {J/kg-K}",
			"    0.9000000,               !- Thermal Absorptance",
			"    0.6500000,               !- Solar Absorptance",
			"    0.6500000;               !- Visible Absorptance",

			"  Material,",
			"    1.375in-Solid-Core,      !- Name",
			"    Smooth,                  !- Roughness",
			"    3.4925E-02,              !- Thickness {m}",
			"    0.1525000,               !- Conductivity {W/m-K}",
			"    614.5000,                !- Density {kg/m3}",
			"    1630.0000,               !- Specific Heat {J/kg-K}",
			"    0.9000000,               !- Thermal Absorptance",
			"    0.9200000,               !- Solar Absorptance",
			"    0.9200000;               !- Visible Absorptance",

			"  WindowMaterial:Glazing,",
			"    SPECTRAL GLASS INNER PANE,  !- Name",
			"    Spectral,                !- Optical Data Type",
			"    TestSpectralDataSet,     !- Window Glass Spectral Data Set Name",
			"    0.0099,                  !- Thickness {m}",
			"    0.0,                     !- Solar Transmittance at Normal Incidence",
			"    0.0,                     !- Front Side Solar Reflectance at Normal Incidence",
			"    0.0,                     !- Back Side Solar Reflectance at Normal Incidence",
			"    0.0,                     !- Visible Transmittance at Normal Incidence",
			"    0.0,                     !- Front Side Visible Reflectance at Normal Incidence",
			"    0.0,                     !- Back Side Visible Reflectance at Normal Incidence",
			"    0.0,                     !- Infrared Transmittance at Normal Incidence",
			"    0.84,                    !- Front Side Infrared Hemispherical Emissivity",
			"    0.84,                    !- Back Side Infrared Hemispherical Emissivity",
			"    0.798;                   !- Conductivity {W/m-K}",

			"  WindowMaterial:Glazing,",
			"    ELECTRO GLASS LIGHT STATE,  !- Name",
			"    SpectralAverage,         !- Optical Data Type",
			"    ,                        !- Window Glass Spectral Data Set Name",
			"    0.006,                   !- Thickness {m}",
			"    0.814,                   !- Solar Transmittance at Normal Incidence",
			"    0.086,                   !- Front Side Solar Reflectance at Normal Incidence",
			"    0.086,                   !- Back Side Solar Reflectance at Normal Incidence",
			"    0.847,                   !- Visible Transmittance at Normal Incidence",
			"    0.099,                   !- Front Side Visible Reflectance at Normal Incidence",
			"    0.099,                   !- Back Side Visible Reflectance at Normal Incidence",
			"    0.0,                     !- Infrared Transmittance at Normal Incidence",
			"    0.84,                    !- Front Side Infrared Hemispherical Emissivity",
			"    0.84,                    !- Back Side Infrared Hemispherical Emissivity",
			"    0.9;                     !- Conductivity {W/m-K}",

			"  WindowMaterial:Glazing,",
			"    ELECTRO GLASS DARK STATE,!- Name",
			"    SpectralAverage,         !- Optical Data Type",
			"    ,                        !- Window Glass Spectral Data Set Name",
			"    0.006,                   !- Thickness {m}",
			"    0.111,                   !- Solar Transmittance at Normal Incidence",
			"    0.179,                   !- Front Side Solar Reflectance at Normal Incidence",
			"    0.179,                   !- Back Side Solar Reflectance at Normal Incidence",
			"    0.128,                   !- Visible Transmittance at Normal Incidence",
			"    0.081,                   !- Front Side Visible Reflectance at Normal Incidence",
			"    0.081,                   !- Back Side Visible Reflectance at Normal Incidence",
			"    0.0,                     !- Infrared Transmittance at Normal Incidence",
			"    0.84,                    !- Front Side Infrared Hemispherical Emissivity",
			"    0.84,                    !- Back Side Infrared Hemispherical Emissivity",
			"    0.9;                     !- Conductivity {W/m-K}",

			"  WindowMaterial:Gas,",
			"    WinAirGap,               !- Name",
			"    AIR,                     !- Gas Type",
			"    0.013;                   !- Thickness {m}",

			"  MaterialProperty:GlazingSpectralData,",
			"    TestSpectralDataSet,     !- Name",
			"    0.300,0.000,0.045,0.045,",
			"    0.310,0.000,0.044,0.044,",
			"    0.320,0.000,0.044,0.044,",
			"    0.330,0.000,0.042,0.042,",
			"    0.340,0.000,0.041,0.041,",
			"    0.350,0.000,0.040,0.040,",
			"    0.360,0.009,0.040,0.040,",
			"    0.370,0.120,0.040,0.040,",
			"    0.380,0.492,0.051,0.051,",
			"    0.390,0.782,0.070,0.070,",
			"    0.400,0.856,0.075,0.075,",
			"    0.410,0.858,0.075,0.075,",
			"    0.420,0.858,0.075,0.075,",
			"    0.430,0.860,0.075,0.075,",
			"    0.440,0.861,0.075,0.075,",
			"    0.450,0.871,0.075,0.075,",
			"    0.460,0.880,0.076,0.076,",
			"    0.470,0.883,0.075,0.075,",
			"    0.480,0.887,0.076,0.076,",
			"    0.490,0.890,0.075,0.075,",
			"    0.500,0.890,0.075,0.075,",
			"    0.510,0.891,0.075,0.075,",
			"    0.520,0.887,0.075,0.075,",
			"    0.530,0.890,0.075,0.075,",
			"    0.540,0.883,0.074,0.074,",
			"    0.550,0.888,0.074,0.074,",
			"    0.560,0.882,0.074,0.074,",
			"    0.570,0.881,0.074,0.074,",
			"    0.580,0.865,0.071,0.071,",
			"    0.590,0.858,0.070,0.070,",
			"    0.600,0.865,0.070,0.070,",
			"    0.610,0.856,0.070,0.070,",
			"    0.620,0.845,0.070,0.070,",
			"    0.630,0.837,0.070,0.070,",
			"    0.640,0.827,0.069,0.069,",
			"    0.650,0.820,0.067,0.067,",
			"    0.660,0.807,0.067,0.067,",
			"    0.670,0.798,0.065,0.065,",
			"    0.680,0.791,0.065,0.065,",
			"    0.690,0.781,0.065,0.065,",
			"    0.700,0.768,0.064,0.064,",
			"    0.710,0.761,0.064,0.064,",
			"    0.720,0.744,0.062,0.062,",
			"    0.730,0.713,0.064,0.064,",
			"    0.740,0.703,0.062,0.062,",
			"    0.750,0.694,0.061,0.061,",
			"    0.760,0.685,0.061,0.061,",
			"    0.770,0.675,0.060,0.060,",
			"    0.780,0.667,0.060,0.060,",
			"    0.790,0.655,0.060,0.060,",
			"    0.800,0.646,0.059,0.059,",
			"    0.810,0.638,0.059,0.059,",
			"    0.820,0.629,0.057,0.057,",
			"    0.830,0.623,0.057,0.057,",
			"    0.840,0.614,0.056,0.056,",
			"    0.850,0.608,0.056,0.056,",
			"    0.860,0.601,0.055,0.055,",
			"    0.870,0.597,0.054,0.054,",
			"    0.880,0.592,0.054,0.054,",
			"    0.890,0.587,0.054,0.054,",
			"    0.900,0.582,0.055,0.055,",
			"    0.950,0.568,0.051,0.051,",
			"    1.000,0.562,0.051,0.051,",
			"    1.050,0.556,0.050,0.050,",
			"    1.100,0.563,0.051,0.051,",
			"    1.150,0.556,0.050,0.050,",
			"    1.200,0.547,0.050,0.050,",
			"    1.250,0.577,0.051,0.051,",
			"    1.300,0.598,0.054,0.054,",
			"    1.350,0.608,0.055,0.055,",
			"    1.400,0.603,0.052,0.052,",
			"    1.450,0.614,0.055,0.055,",
			"    1.500,0.648,0.057,0.057,",
			"    1.550,0.680,0.059,0.059,",
			"    1.600,0.699,0.060,0.060,",
			"    1.650,0.706,0.060,0.060,",
			"    1.700,0.570,0.051,0.051,",
			"    1.750,0.585,0.051,0.051,",
			"    1.800,0.637,0.055,0.055,",
			"    1.850,0.655,0.057,0.057,",
			"    1.900,0.637,0.057,0.057,",
			"    1.950,0.634,0.057,0.057,",
			"    2.000,0.634,0.057,0.057,",
			"    2.050,0.586,0.052,0.052,",
			"    2.100,0.588,0.054,0.054,",
			"    2.150,0.597,0.054,0.054,",
			"    2.200,0.576,0.051,0.051,",
			"    2.250,0.404,0.045,0.045,",
			"    2.300,0.179,0.037,0.037,",
			"    2.350,0.219,0.037,0.037,",
			"    2.400,0.240,0.039,0.039,",
			"    2.450,0.200,0.040,0.040,",
			"    2.500,0.214,0.039,0.039;",

			"  Construction,",
			"    EXTWALL80,               !- Name",
			"    A1 - 1 IN STUCCO,        !- Outside Layer",
			"    C4 - 4 IN COMMON BRICK,  !- Layer 2",
			"    E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 3",

			"  Construction,",
			"    PARTITION06,             !- Name",
			"    E1 - 3 / 4 IN PLASTER OR GYP BOARD,  !- Outside Layer",
			"    C6 - 8 IN CLAY TILE,     !- Layer 2",
			"    E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 3",

			"  Construction,",
			"    FLOOR SLAB 8 IN,         !- Name",
			"    C10 - 8 IN HW CONCRETE;  !- Outside Layer",

			"  Construction,",
			"    ROOF34,                  !- Name",
			"    E2 - 1 / 2 IN SLAG OR STONE,  !- Outside Layer",
			"    E3 - 3 / 8 IN FELT AND MEMBRANE,  !- Layer 2",
			"    C12 - 2 IN HW CONCRETE;  !- Layer 3",

			"  Construction,",
			"    CEILING:ZONE,            !- Name",
			"    B5 - 1 IN DENSE INSULATION,  !- Outside Layer",
			"    E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 2",

			"  Construction,",
			"    CEILING:ATTIC,           !- Name",
			"    E1 - 3 / 4 IN PLASTER OR GYP BOARD,  !- Outside Layer",
			"    B5 - 1 IN DENSE INSULATION;  !- Layer 2",

			"  Construction,",
			"    ELECTRO-CON-LIGHT,       !- Name",
			"    ELECTRO GLASS LIGHT STATE,  !- Outside Layer",
			"    WinAirGap,               !- Layer 2",
			"    SPECTRAL GLASS INNER PANE;  !- Layer 3",

			"  Construction,",
			"    ELECTRO-CON-DARK,        !- Name",
			"    ELECTRO GLASS DARK STATE,!- Outside Layer",
			"    WinAirGap,               !- Layer 2",
			"    SPECTRAL GLASS INNER PANE;  !- Layer 3",

			"  Construction,",
			"    DOOR-CON,                !- Name",
			"    1.375in-Solid-Core;      !- Outside Layer",


			"  Zone,",
			"    West Zone,               !- Name",
			"    0,                       !- Direction of Relative North {deg}",
			"    0,                       !- X Origin {m}",
			"    0,                       !- Y Origin {m}",
			"    0,                       !- Z Origin {m}",
			"    1,                       !- Type",
			"    1,                       !- Multiplier",
			"    autocalculate,           !- Ceiling Height {m}",
			"    autocalculate;           !- Volume {m3}",

			"  Zone,",
			"    EAST ZONE,               !- Name",
			"    0,                       !- Direction of Relative North {deg}",
			"    0,                       !- X Origin {m}",
			"    0,                       !- Y Origin {m}",
			"    0,                       !- Z Origin {m}",
			"    1,                       !- Type",
			"    1,                       !- Multiplier",
			"    autocalculate,           !- Ceiling Height {m}",
			"    autocalculate;           !- Volume {m3}",

			"  Zone,",
			"    NORTH ZONE,              !- Name",
			"    0,                       !- Direction of Relative North {deg}",
			"    0,                       !- X Origin {m}",
			"    0,                       !- Y Origin {m}",
			"    0,                       !- Z Origin {m}",
			"    1,                       !- Type",
			"    1,                       !- Multiplier",
			"    autocalculate,           !- Ceiling Height {m}",
			"    autocalculate;           !- Volume {m3}",
			"  Zone,",
			"    ATTIC ZONE,              !- Name",
			"    0,                       !- Direction of Relative North {deg}",
			"    0,                       !- X Origin {m}",
			"    0,                       !- Y Origin {m}",
			"    0,                       !- Z Origin {m}",
			"    1,                       !- Type",
			"    1,                       !- Multiplier",
			"    autocalculate,           !- Ceiling Height {m}",
			"    autocalculate;           !- Volume {m3}",

			"  GlobalGeometryRules,",
			"    UpperLeftCorner,         !- Starting Vertex Position",
			"    CounterClockWise,        !- Vertex Entry Direction",
			"    World;                   !- Coordinate System",

			"  BuildingSurface:Detailed,",
			"    Zn001:Wall001,           !- Name",
			"    Wall,                    !- Surface Type",
			"    EXTWALL80,               !- Construction Name",
			"    West Zone,               !- Zone Name",
			"    Outdoors,                !- Outside Boundary Condition",
			"    ,                        !- Outside Boundary Condition Object",
			"    SunExposed,              !- Sun Exposure",
			"    WindExposed,             !- Wind Exposure",
			"    0.5000000,               !- View Factor to Ground",
			"    4,                       !- Number of Vertices",
			"    0,0,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
			"    0,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
			"    6.096000,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
			"    6.096000,0,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

			"  BuildingSurface:Detailed,",
			"    Zn001:Wall002,           !- Name",
			"    Wall,                    !- Surface Type",
			"    EXTWALL80,               !- Construction Name",
			"    West Zone,               !- Zone Name",
			"    Outdoors,                !- Outside Boundary Condition",
			"    ,                        !- Outside Boundary Condition Object",
			"    SunExposed,              !- Sun Exposure",
			"    WindExposed,             !- Wind Exposure",
			"    0.5000000,               !- View Factor to Ground",
			"    4,                       !- Number of Vertices",
			"    0,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
			"    0,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
			"    0,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
			"    0,0,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

			"  BuildingSurface:Detailed,",
			"    Zn001:Wall003,           !- Name",
			"    Wall,                    !- Surface Type",
			"    PARTITION06,             !- Construction Name",
			"    West Zone,               !- Zone Name",
			"    Surface,                 !- Outside Boundary Condition",
			"    Zn003:Wall004,           !- Outside Boundary Condition Object",
			"    NoSun,                   !- Sun Exposure",
			"    NoWind,                  !- Wind Exposure",
			"    0.5000000,               !- View Factor to Ground",
			"    4,                       !- Number of Vertices",
			"    6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
			"    6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
			"    0,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
			"    0,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

			"  BuildingSurface:Detailed,",
			"    Zn001:Wall004,           !- Name",
			"    Wall,                    !- Surface Type",
			"    PARTITION06,             !- Construction Name",
			"    West Zone,               !- Zone Name",
			"    Surface,                 !- Outside Boundary Condition",
			"    Zn002:Wall004,           !- Outside Boundary Condition Object",
			"    NoSun,                   !- Sun Exposure",
			"    NoWind,                  !- Wind Exposure",
			"    0.5000000,               !- View Factor to Ground",
			"    4,                       !- Number of Vertices",
			"    6.096000,0,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
			"    6.096000,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
			"    6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
			"    6.096000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

			"  BuildingSurface:Detailed,",
			"    Zn001:Flr001,            !- Name",
			"    Floor,                   !- Surface Type",
			"    FLOOR SLAB 8 IN,         !- Construction Name",
			"    West Zone,               !- Zone Name",
			"    Surface,                 !- Outside Boundary Condition",
			"    Zn001:Flr001,            !- Outside Boundary Condition Object",
			"    NoSun,                   !- Sun Exposure",
			"    NoWind,                  !- Wind Exposure",
			"    1.000000,                !- View Factor to Ground",
			"    4,                       !- Number of Vertices",
			"    0,0,0,  !- X,Y,Z ==> Vertex 1 {m}",
			"    0,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
			"    6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
			"    6.096000,0,0;  !- X,Y,Z ==> Vertex 4 {m}",

			"  BuildingSurface:Detailed,",
			"    Zn001:Ceil001,           !- Name",
			"    CEILING,                 !- Surface Type",
			"    CEILING:ZONE,            !- Construction Name",
			"    West Zone,               !- Zone Name",
			"    Surface,                 !- Outside Boundary Condition",
			"    Zn004:Flr001,            !- Outside Boundary Condition Object",
			"    NoSun,                   !- Sun Exposure",
			"    NoWind,                  !- Wind Exposure",
			"    0,                       !- View Factor to Ground",
			"    4,                       !- Number of Vertices",
			"    0,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
			"    0,0,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
			"    6.096000,0,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
			"    6.096000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

			"  BuildingSurface:Detailed,",
			"    Zn002:Wall001,           !- Name",
			"    Wall,                    !- Surface Type",
			"    EXTWALL80,               !- Construction Name",
			"    EAST ZONE,               !- Zone Name",
			"    Outdoors,                !- Outside Boundary Condition",
			"    ,                        !- Outside Boundary Condition Object",
			"    SunExposed,              !- Sun Exposure",
			"    WindExposed,             !- Wind Exposure",
			"    0.5000000,               !- View Factor to Ground",
			"    4,                       !- Number of Vertices",
			"    12.19200,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
			"    12.19200,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
			"    9.144000,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
			"    9.144000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

			"  BuildingSurface:Detailed,",
			"    Zn002:Wall002,           !- Name",
			"    Wall,                    !- Surface Type",
			"    EXTWALL80,               !- Construction Name",
			"    EAST ZONE,               !- Zone Name",
			"    Outdoors,                !- Outside Boundary Condition",
			"    ,                        !- Outside Boundary Condition Object",
			"    SunExposed,              !- Sun Exposure",
			"    WindExposed,             !- Wind Exposure",
			"    0.5000000,               !- View Factor to Ground",
			"    4,                       !- Number of Vertices",
			"    6.096000,0,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
			"    6.096000,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
			"    12.19200,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
			"    12.19200,0,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

			"  BuildingSurface:Detailed,",
			"    Zn002:Wall003,           !- Name",
			"    Wall,                    !- Surface Type",
			"    EXTWALL80,               !- Construction Name",
			"    EAST ZONE,               !- Zone Name",
			"    Outdoors,                !- Outside Boundary Condition",
			"    ,                        !- Outside Boundary Condition Object",
			"    SunExposed,              !- Sun Exposure",
			"    WindExposed,             !- Wind Exposure",
			"    0.5000000,               !- View Factor to Ground",
			"    4,                       !- Number of Vertices",
			"    12.19200,0,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
			"    12.19200,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
			"    12.19200,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
			"    12.19200,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

			"  BuildingSurface:Detailed,",
			"    Zn002:Wall004,           !- Name",
			"    Wall,                    !- Surface Type",
			"    PARTITION06,             !- Construction Name",
			"    EAST ZONE,               !- Zone Name",
			"    Surface,                 !- Outside Boundary Condition",
			"    Zn001:Wall004,           !- Outside Boundary Condition Object",
			"    NoSun,                   !- Sun Exposure",
			"    NoWind,                  !- Wind Exposure",
			"    0.5000000,               !- View Factor to Ground",
			"    4,                       !- Number of Vertices",
			"    6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
			"    6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
			"    6.096000,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
			"    6.096000,0,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

			"  BuildingSurface:Detailed,",
			"    Zn002:Wall005,           !- Name",
			"    Wall,                    !- Surface Type",
			"    PARTITION06,             !- Construction Name",
			"    EAST ZONE,               !- Zone Name",
			"    Surface,                 !- Outside Boundary Condition",
			"    Zn003:Wall005,           !- Outside Boundary Condition Object",
			"    NoSun,                   !- Sun Exposure",
			"    NoWind,                  !- Wind Exposure",
			"    0.5000000,               !- View Factor to Ground",
			"    4,                       !- Number of Vertices",
			"    9.144000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
			"    9.144000,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
			"    6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
			"    6.096000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

			"  BuildingSurface:Detailed,",
			"    Zn002:Flr001,            !- Name",
			"    Floor,                   !- Surface Type",
			"    FLOOR SLAB 8 IN,         !- Construction Name",
			"    EAST ZONE,               !- Zone Name",
			"    Surface,                 !- Outside Boundary Condition",
			"    Zn002:Flr001,            !- Outside Boundary Condition Object",
			"    NoSun,                   !- Sun Exposure",
			"    NoWind,                  !- Wind Exposure",
			"    1.000000,                !- View Factor to Ground",
			"    4,                       !- Number of Vertices",
			"    6.096000,0,0,  !- X,Y,Z ==> Vertex 1 {m}",
			"    6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
			"    12.19200,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
			"    12.19200,0,0;  !- X,Y,Z ==> Vertex 4 {m}",

			"  BuildingSurface:Detailed,",
			"    Zn002:Ceil001,           !- Name",
			"    CEILING,                 !- Surface Type",
			"    CEILING:ZONE,            !- Construction Name",
			"    EAST ZONE,               !- Zone Name",
			"    Surface,                 !- Outside Boundary Condition",
			"    Zn004:Flr002,            !- Outside Boundary Condition Object",
			"    NoSun,                   !- Sun Exposure",
			"    NoWind,                  !- Wind Exposure",
			"    0,                       !- View Factor to Ground",
			"    4,                       !- Number of Vertices",
			"    6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
			"    6.096000,0,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
			"    12.19200,0,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
			"    12.19200,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

			"  BuildingSurface:Detailed,",
			"    Zn003:Wall001,           !- Name",
			"    Wall,                    !- Surface Type",
			"    EXTWALL80,               !- Construction Name",
			"    NORTH ZONE,              !- Zone Name",
			"    Outdoors,                !- Outside Boundary Condition",
			"    ,                        !- Outside Boundary Condition Object",
			"    SunExposed,              !- Sun Exposure",
			"    WindExposed,             !- Wind Exposure",
			"    0.5000000,               !- View Factor to Ground",
			"    4,                       !- Number of Vertices",
			"    0,12.19200,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
			"    0,12.19200,0,  !- X,Y,Z ==> Vertex 2 {m}",
			"    0,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
			"    0,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

			"  BuildingSurface:Detailed,",
			"    Zn003:Wall002,           !- Name",
			"    Wall,                    !- Surface Type",
			"    EXTWALL80,               !- Construction Name",
			"    NORTH ZONE,              !- Zone Name",
			"    Outdoors,                !- Outside Boundary Condition",
			"    ,                        !- Outside Boundary Condition Object",
			"    SunExposed,              !- Sun Exposure",
			"    WindExposed,             !- Wind Exposure",
			"    0.5000000,               !- View Factor to Ground",
			"    4,                       !- Number of Vertices",
			"    9.144000,12.19200,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
			"    9.144000,12.19200,0,  !- X,Y,Z ==> Vertex 2 {m}",
			"    0,12.19200,0,  !- X,Y,Z ==> Vertex 3 {m}",
			"    0,12.19200,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

			"  BuildingSurface:Detailed,",
			"    Zn003:Wall003,           !- Name",
			"    Wall,                    !- Surface Type",
			"    EXTWALL80,               !- Construction Name",
			"    NORTH ZONE,              !- Zone Name",
			"    Outdoors,                !- Outside Boundary Condition",
			"    ,                        !- Outside Boundary Condition Object",
			"    SunExposed,              !- Sun Exposure",
			"    WindExposed,             !- Wind Exposure",
			"    0.5000000,               !- View Factor to Ground",
			"    4,                       !- Number of Vertices",
			"    9.144000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
			"    9.144000,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
			"    9.144000,12.19200,0,  !- X,Y,Z ==> Vertex 3 {m}",
			"    9.144000,12.19200,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

			"  BuildingSurface:Detailed,",
			"    Zn003:Wall004,           !- Name",
			"    Wall,                    !- Surface Type",
			"    PARTITION06,             !- Construction Name",
			"    NORTH ZONE,              !- Zone Name",
			"    Surface,                 !- Outside Boundary Condition",
			"    Zn001:Wall003,           !- Outside Boundary Condition Object",
			"    NoSun,                   !- Sun Exposure",
			"    NoWind,                  !- Wind Exposure",
			"    0.5000000,               !- View Factor to Ground",
			"    4,                       !- Number of Vertices",
			"    0,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
			"    0,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
			"    6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
			"    6.096000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

			"  BuildingSurface:Detailed,",
			"    Zn003:Wall005,           !- Name",
			"    Wall,                    !- Surface Type",
			"    PARTITION06,             !- Construction Name",
			"    NORTH ZONE,              !- Zone Name",
			"    Surface,                 !- Outside Boundary Condition",
			"    Zn002:Wall005,           !- Outside Boundary Condition Object",
			"    NoSun,                   !- Sun Exposure",
			"    NoWind,                  !- Wind Exposure",
			"    0.5000000,               !- View Factor to Ground",
			"    4,                       !- Number of Vertices",
			"    6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
			"    6.096000,6.096000,0,  !- X,Y,Z ==> Vertex 2 {m}",
			"    9.144000,6.096000,0,  !- X,Y,Z ==> Vertex 3 {m}",
			"    9.144000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

			"  BuildingSurface:Detailed,",
			"    Zn003:Flr001,            !- Name",
			"    Floor,                   !- Surface Type",
			"    FLOOR SLAB 8 IN,         !- Construction Name",
			"    NORTH ZONE,              !- Zone Name",
			"    Surface,                 !- Outside Boundary Condition",
			"    Zn003:Flr001,            !- Outside Boundary Condition Object",
			"    NoSun,                   !- Sun Exposure",
			"    NoWind,                  !- Wind Exposure",
			"    1.000000,                !- View Factor to Ground",
			"    4,                       !- Number of Vertices",
			"    0,6.096000,0,  !- X,Y,Z ==> Vertex 1 {m}",
			"    0,12.19200,0,  !- X,Y,Z ==> Vertex 2 {m}",
			"    9.144000,12.19200,0,  !- X,Y,Z ==> Vertex 3 {m}",
			"    9.144000,6.096000,0;  !- X,Y,Z ==> Vertex 4 {m}",

			"  BuildingSurface:Detailed,",
			"    Zn003:Ceil001,           !- Name",
			"    CEILING,                 !- Surface Type",
			"    CEILING:ZONE,            !- Construction Name",
			"    NORTH ZONE,              !- Zone Name",
			"    Surface,                 !- Outside Boundary Condition",
			"    Zn004:Flr003,            !- Outside Boundary Condition Object",
			"    NoSun,                   !- Sun Exposure",
			"    NoWind,                  !- Wind Exposure",
			"    0,                       !- View Factor to Ground",
			"    4,                       !- Number of Vertices",
			"    0,12.19200,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
			"    0,6.096000,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
			"    9.144000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
			"    9.144000,12.19200,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

			"  BuildingSurface:Detailed,",
			"    Zn004:Wall001,           !- Name",
			"    Wall,                    !- Surface Type",
			"    EXTWALL80,               !- Construction Name",
			"    ATTIC ZONE,              !- Zone Name",
			"    Outdoors,                !- Outside Boundary Condition",
			"    ,                        !- Outside Boundary Condition Object",
			"    SunExposed,              !- Sun Exposure",
			"    WindExposed,             !- Wind Exposure",
			"    0.5000000,               !- View Factor to Ground",
			"    4,                       !- Number of Vertices",
			"    0,0,3.962400,  !- X,Y,Z ==> Vertex 1 {m}",
			"    0,0,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
			"    6.096000,0,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
			"    6.096000,0,3.962400;  !- X,Y,Z ==> Vertex 4 {m}",

			"  BuildingSurface:Detailed,",
			"    Zn004:Wall002,           !- Name",
			"    Wall,                    !- Surface Type",
			"    EXTWALL80,               !- Construction Name",
			"    ATTIC ZONE,              !- Zone Name",
			"    Outdoors,                !- Outside Boundary Condition",
			"    ,                        !- Outside Boundary Condition Object",
			"    SunExposed,              !- Sun Exposure",
			"    WindExposed,             !- Wind Exposure",
			"    0.5000000,               !- View Factor to Ground",
			"    4,                       !- Number of Vertices",
			"    6.096000,0,3.962400,  !- X,Y,Z ==> Vertex 1 {m}",
			"    6.096000,0,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
			"    12.19200,0,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
			"    12.19200,0,3.962400;  !- X,Y,Z ==> Vertex 4 {m}",

			"  BuildingSurface:Detailed,",
			"    Zn004:Wall003,           !- Name",
			"    Wall,                    !- Surface Type",
			"    EXTWALL80,               !- Construction Name",
			"    ATTIC ZONE,              !- Zone Name",
			"    Outdoors,                !- Outside Boundary Condition",
			"    ,                        !- Outside Boundary Condition Object",
			"    SunExposed,              !- Sun Exposure",
			"    WindExposed,             !- Wind Exposure",
			"    0.5000000,               !- View Factor to Ground",
			"    4,                       !- Number of Vertices",
			"    12.19200,0,3.962400,  !- X,Y,Z ==> Vertex 1 {m}",
			"    12.19200,0,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
			"    12.19200,6.096000,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
			"    12.19200,6.096000,3.962400;  !- X,Y,Z ==> Vertex 4 {m}",

			"  BuildingSurface:Detailed,",
			"    Zn004:Wall004,           !- Name",
			"    Wall,                    !- Surface Type",
			"    EXTWALL80,               !- Construction Name",
			"    ATTIC ZONE,              !- Zone Name",
			"    Outdoors,                !- Outside Boundary Condition",
			"    ,                        !- Outside Boundary Condition Object",
			"    SunExposed,              !- Sun Exposure",
			"    WindExposed,             !- Wind Exposure",
			"    0.5000000,               !- View Factor to Ground",
			"    4,                       !- Number of Vertices",
			"    12.19200,6.096000,3.962400,  !- X,Y,Z ==> Vertex 1 {m}",
			"    12.19200,6.096000,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
			"    9.144000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
			"    9.144000,6.096000,3.962400;  !- X,Y,Z ==> Vertex 4 {m}",

			"  BuildingSurface:Detailed,",
			"    Zn004:Wall005,           !- Name",
			"    Wall,                    !- Surface Type",
			"    EXTWALL80,               !- Construction Name",
			"    ATTIC ZONE,              !- Zone Name",
			"    Outdoors,                !- Outside Boundary Condition",
			"    ,                        !- Outside Boundary Condition Object",
			"    SunExposed,              !- Sun Exposure",
			"    WindExposed,             !- Wind Exposure",
			"    0.5000000,               !- View Factor to Ground",
			"    4,                       !- Number of Vertices",
			"    9.144000,6.096000,3.962400,  !- X,Y,Z ==> Vertex 1 {m}",
			"    9.144000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
			"    9.144000,12.19200,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
			"    9.144000,12.19200,3.962400;  !- X,Y,Z ==> Vertex 4 {m}",

			"  BuildingSurface:Detailed,",
			"    Zn004:Wall006,           !- Name",
			"    Wall,                    !- Surface Type",
			"    EXTWALL80,               !- Construction Name",
			"    ATTIC ZONE,              !- Zone Name",
			"    Outdoors,                !- Outside Boundary Condition",
			"    ,                        !- Outside Boundary Condition Object",
			"    SunExposed,              !- Sun Exposure",
			"    WindExposed,             !- Wind Exposure",
			"    0.5000000,               !- View Factor to Ground",
			"    4,                       !- Number of Vertices",
			"    9.144000,12.19200,3.962400,  !- X,Y,Z ==> Vertex 1 {m}",
			"    9.144000,12.19200,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
			"    0,12.19200,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
			"    0,12.19200,3.962400;  !- X,Y,Z ==> Vertex 4 {m}",

			"  BuildingSurface:Detailed,",
			"    Zn004:Wall007,           !- Name",
			"    Wall,                    !- Surface Type",
			"    EXTWALL80,               !- Construction Name",
			"    ATTIC ZONE,              !- Zone Name",
			"    Outdoors,                !- Outside Boundary Condition",
			"    ,                        !- Outside Boundary Condition Object",
			"    SunExposed,              !- Sun Exposure",
			"    WindExposed,             !- Wind Exposure",
			"    0.5000000,               !- View Factor to Ground",
			"    4,                       !- Number of Vertices",
			"    0,12.19200,3.962400,  !- X,Y,Z ==> Vertex 1 {m}",
			"    0,12.19200,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
			"    0,6.096000,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
			"    0,6.096000,3.962400;  !- X,Y,Z ==> Vertex 4 {m}",

			"  BuildingSurface:Detailed,",
			"    Zn004:Wall008,           !- Name",
			"    Wall,                    !- Surface Type",
			"    EXTWALL80,               !- Construction Name",
			"    ATTIC ZONE,              !- Zone Name",
			"    Outdoors,                !- Outside Boundary Condition",
			"    ,                        !- Outside Boundary Condition Object",
			"    SunExposed,              !- Sun Exposure",
			"    WindExposed,             !- Wind Exposure",
			"    0.5000000,               !- View Factor to Ground",
			"    4,                       !- Number of Vertices",
			"    0,6.096000,3.962400,  !- X,Y,Z ==> Vertex 1 {m}",
			"    0,6.096000,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
			"    0,0,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
			"    0,0,3.962400;  !- X,Y,Z ==> Vertex 4 {m}",

			"  BuildingSurface:Detailed,",
			"    Zn004:Roof001,           !- Name",
			"    Roof,                    !- Surface Type",
			"    ROOF34,                  !- Construction Name",
			"    ATTIC ZONE,              !- Zone Name",
			"    Outdoors,                !- Outside Boundary Condition",
			"    ,                        !- Outside Boundary Condition Object",
			"    SunExposed,              !- Sun Exposure",
			"    WindExposed,             !- Wind Exposure",
			"    0,                       !- View Factor to Ground",
			"    4,                       !- Number of Vertices",
			"    0,6.096000,3.962400,  !- X,Y,Z ==> Vertex 1 {m}",
			"    0,0,3.962400,  !- X,Y,Z ==> Vertex 2 {m}",
			"    6.096000,0,3.962400,  !- X,Y,Z ==> Vertex 3 {m}",
			"    6.096000,6.096000,3.962400;  !- X,Y,Z ==> Vertex 4 {m}",

			"  BuildingSurface:Detailed,",
			"    Zn004:Roof002,           !- Name",
			"    Roof,                    !- Surface Type",
			"    ROOF34,                  !- Construction Name",
			"    ATTIC ZONE,              !- Zone Name",
			"    Outdoors,                !- Outside Boundary Condition",
			"    ,                        !- Outside Boundary Condition Object",
			"    SunExposed,              !- Sun Exposure",
			"    WindExposed,             !- Wind Exposure",
			"    0,                       !- View Factor to Ground",
			"    4,                       !- Number of Vertices",
			"    6.096000,6.096000,3.962400,  !- X,Y,Z ==> Vertex 1 {m}",
			"    6.096000,0,3.962400,  !- X,Y,Z ==> Vertex 2 {m}",
			"    12.19200,0,3.962400,  !- X,Y,Z ==> Vertex 3 {m}",
			"    12.19200,6.096000,3.962400;  !- X,Y,Z ==> Vertex 4 {m}",

			"  BuildingSurface:Detailed,",
			"    Zn004:Roof003,           !- Name",
			"    Roof,                    !- Surface Type",
			"    ROOF34,                  !- Construction Name",
			"    ATTIC ZONE,              !- Zone Name",
			"    Outdoors,                !- Outside Boundary Condition",
			"    ,                        !- Outside Boundary Condition Object",
			"    SunExposed,              !- Sun Exposure",
			"    WindExposed,             !- Wind Exposure",
			"    0,                       !- View Factor to Ground",
			"    4,                       !- Number of Vertices",
			"    0,12.19200,3.962400,  !- X,Y,Z ==> Vertex 1 {m}",
			"    0,6.096000,3.962400,  !- X,Y,Z ==> Vertex 2 {m}",
			"    9.144000,6.096000,3.962400,  !- X,Y,Z ==> Vertex 3 {m}",
			"    9.144000,12.19200,3.962400;  !- X,Y,Z ==> Vertex 4 {m}",

			"  BuildingSurface:Detailed,",
			"    Zn004:Flr001,            !- Name",
			"    Floor,                   !- Surface Type",
			"    CEILING:ATTIC,           !- Construction Name",
			"    ATTIC ZONE,              !- Zone Name",
			"    Surface,                 !- Outside Boundary Condition",
			"    Zn001:Ceil001,           !- Outside Boundary Condition Object",
			"    NoSun,                   !- Sun Exposure",
			"    NoWind,                  !- Wind Exposure",
			"    1.000000,                !- View Factor to Ground",
			"    4,                       !- Number of Vertices",
			"    0,0,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
			"    0,6.096000,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
			"    6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
			"    6.096000,0,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

			"  BuildingSurface:Detailed,",
			"    Zn004:Flr002,            !- Name",
			"    Floor,                   !- Surface Type",
			"    CEILING:ATTIC,           !- Construction Name",
			"    ATTIC ZONE,              !- Zone Name",
			"    Surface,                 !- Outside Boundary Condition",
			"    Zn002:Ceil001,           !- Outside Boundary Condition Object",
			"    NoSun,                   !- Sun Exposure",
			"    NoWind,                  !- Wind Exposure",
			"    1.000000,                !- View Factor to Ground",
			"    4,                       !- Number of Vertices",
			"    6.096000,0,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
			"    6.096000,6.096000,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
			"    12.19200,6.096000,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
			"    12.19200,0,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

			"  BuildingSurface:Detailed,",
			"    Zn004:Flr003,            !- Name",
			"    Floor,                   !- Surface Type",
			"    CEILING:ATTIC,           !- Construction Name",
			"    ATTIC ZONE,              !- Zone Name",
			"    Surface,                 !- Outside Boundary Condition",
			"    Zn003:Ceil001,           !- Outside Boundary Condition Object",
			"    NoSun,                   !- Sun Exposure",
			"    NoWind,                  !- Wind Exposure",
			"    1.000000,                !- View Factor to Ground",
			"    4,                       !- Number of Vertices",
			"    0,6.096000,3.048000,  !- X,Y,Z ==> Vertex 1 {m}",
			"    0,12.19200,3.048000,  !- X,Y,Z ==> Vertex 2 {m}",
			"    9.144000,12.19200,3.048000,  !- X,Y,Z ==> Vertex 3 {m}",
			"    9.144000,6.096000,3.048000;  !- X,Y,Z ==> Vertex 4 {m}",

			"  FenestrationSurface:Detailed,",
			"    Zn001:Wall001:Win001,    !- Name",
			"    Window,                  !- Surface Type",
			"    ELECTRO-CON-LIGHT,       !- Construction Name",
			"    Zn001:Wall001,           !- Building Surface Name",
			"    ,                        !- Outside Boundary Condition Object",
			"    0.5000000,               !- View Factor to Ground",
			"    ,                        !- Shading Control Name",
			"    ,                        !- Frame and Divider Name",
			"    1.0,                     !- Multiplier",
			"    4,                       !- Number of Vertices",
			"    0.548000,0,2.5000,  !- X,Y,Z ==> Vertex 1 {m}",
			"    0.548000,0,0.5000,  !- X,Y,Z ==> Vertex 2 {m}",
			"    5.548000,0,0.5000,  !- X,Y,Z ==> Vertex 3 {m}",
			"    5.548000,0,2.5000;  !- X,Y,Z ==> Vertex 4 {m}",

			"  FenestrationSurface:Detailed,",
			"    Zn001:Wall003:Door001,   !- Name",
			"    DOOR,                    !- Surface Type",
			"    DOOR-CON,                !- Construction Name",
			"    Zn001:Wall003,           !- Building Surface Name",
			"    Zn003:Wall004:Door001,   !- Outside Boundary Condition Object",
			"    0.5000000,               !- View Factor to Ground",
			"    ,                        !- Shading Control Name",
			"    ,                        !- Frame and Divider Name",
			"    1.0,                     !- Multiplier",
			"    4,                       !- Number of Vertices",
			"    3.500,6.096000,2.0,  !- X,Y,Z ==> Vertex 1 {m}",
			"    3.500,6.096000,0.0,  !- X,Y,Z ==> Vertex 2 {m}",
			"    2.500,6.096000,0.0,  !- X,Y,Z ==> Vertex 3 {m}",
			"    2.500,6.096000,2.0;  !- X,Y,Z ==> Vertex 4 {m}",

			"  FenestrationSurface:Detailed,",
			"    Zn003:Wall002:Win001,    !- Name",
			"    Window,                  !- Surface Type",
			"    ELECTRO-CON-LIGHT,       !- Construction Name",
			"    Zn003:Wall002,           !- Building Surface Name",
			"    ,                        !- Outside Boundary Condition Object",
			"    0.5000000,               !- View Factor to Ground",
			"    ,                        !- Shading Control Name",
			"    ,                        !- Frame and Divider Name",
			"    1.0,                     !- Multiplier",
			"    4,                       !- Number of Vertices",
			"    5.548000,12.19200,2.5000,  !- X,Y,Z ==> Vertex 1 {m}",
			"    5.548000,12.19200,0.5000,  !- X,Y,Z ==> Vertex 2 {m}",
			"    0.548000,12.19200,0.5000,  !- X,Y,Z ==> Vertex 3 {m}",
			"    0.548000,12.19200,2.5000;  !- X,Y,Z ==> Vertex 4 {m}",

			"  FenestrationSurface:Detailed,",
			"    Zn003:Wall004:Door001,   !- Name",
			"    DOOR,                    !- Surface Type",
			"    DOOR-CON,                !- Construction Name",
			"    Zn003:Wall004,           !- Building Surface Name",
			"    Zn001:Wall003:Door001,   !- Outside Boundary Condition Object",
			"    0.5000000,               !- View Factor to Ground",
			"    ,                        !- Shading Control Name",
			"    ,                        !- Frame and Divider Name",
			"    1.0,                     !- Multiplier",
			"    4,                       !- Number of Vertices",
			"    2.500,6.096000,2.0,  !- X,Y,Z ==> Vertex 1 {m}",
			"    2.500,6.096000,0.0,  !- X,Y,Z ==> Vertex 2 {m}",
			"    3.500,6.096000,0.0,  !- X,Y,Z ==> Vertex 3 {m}",
			"    3.500,6.096000,2.0;  !- X,Y,Z ==> Vertex 4 {m}",

			"  AirflowNetwork:SimulationControl,",
			"    AriflowNetwork_All,      !- Name",
			"    MultizoneWithDistribution,  !- AirflowNetwork Control",
			"    INPUT,                   !- Wind Pressure Coefficient Type",
			"    Every 30 Degrees,        !- AirflowNetwork Wind Pressure Coefficient Array Name",
			"    ExternalNode,            !- Height Selection for Local Wind Pressure Calculation",
			"    LOWRISE,                 !- Building Type",
			"    500,                     !- Maximum Number of Iterations {dimensionless}",
			"    ZeroNodePressures,       !- Initialization Type",
			"    1.0E-05,                 !- Relative Airflow Convergence Tolerance {dimensionless}",
			"    1.0E-06,                 !- Absolute Airflow Convergence Tolerance {kg/s}",
			"    -0.5,                    !- Convergence Acceleration Limit {dimensionless}",
			"    0.0,                     !- Azimuth Angle of Long Axis of Building {deg}",
			"    1.0;                     !- Ratio of Building Width Along Short Axis to Width Along Long Axis",

			"  AirflowNetwork:MultiZone:Zone,",
			"    West Zone,               !- Zone Name",
			"    Temperature,             !- Ventilation Control Mode",
			"    WindowVentSched,         !- Ventilation Control Zone Temperature Setpoint Schedule Name",
			"    0.3,                     !- Minimum Venting Open Factor {dimensionless}",
			"    5.0,                     !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor {deltaC}",
			"    10.0,                    !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor {deltaC}",
			"    0.0,                     !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor {deltaJ/kg}",
			"    300000.0,                !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor {deltaJ/kg}",
			"    VentingSched;            !- Venting Availability Schedule Name",

			"  AirflowNetwork:MultiZone:Zone,",
			"    EAST ZONE,               !- Zone Name",
			"    NoVent,                  !- Ventilation Control Mode",
			"    ,                        !- Ventilation Control Zone Temperature Setpoint Schedule Name",
			"    1.0,                     !- Minimum Venting Open Factor {dimensionless}",
			"    0.0,                     !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor {deltaC}",
			"    100.0,                   !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor {deltaC}",
			"    0.0,                     !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor {deltaJ/kg}",
			"    300000.0;                !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor {deltaJ/kg}",

			"  AirflowNetwork:MultiZone:Zone,",
			"    NORTH ZONE,              !- Zone Name",
			"    Temperature,             !- Ventilation Control Mode",
			"    WindowVentSched,         !- Ventilation Control Zone Temperature Setpoint Schedule Name",
			"    1.0,                     !- Minimum Venting Open Factor {dimensionless}",
			"    0.0,                     !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor {deltaC}",
			"    100.0,                   !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor {deltaC}",
			"    0.0,                     !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor {deltaJ/kg}",
			"    300000.0,                !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor {deltaJ/kg}",
			"    VentingSched;            !- Venting Availability Schedule Name",

			"  AirflowNetwork:MultiZone:Zone,",
			"    ATTIC ZONE,              !- Zone Name",
			"    NoVent,                  !- Ventilation Control Mode",
			"    ,                        !- Ventilation Control Zone Temperature Setpoint Schedule Name",
			"    1.0,                     !- Minimum Venting Open Factor {dimensionless}",
			"    0.0,                     !- Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor {deltaC}",
			"    100.0,                   !- Indoor and Outdoor Temperature Difference Upper Limit for Minimum Venting Open Factor {deltaC}",
			"    0.0,                     !- Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor {deltaJ/kg}",
			"    300000.0;                !- Indoor and Outdoor Enthalpy Difference Upper Limit for Minimum Venting Open Factor {deltaJ/kg}",

			"  AirflowNetwork:MultiZone:Surface,",
			"    Zn001:Wall001,           !- Surface Name",
			"    ELA-1,                   !- Leakage Component Name",
			"    SFacade,                 !- External Node Name",
			"    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

			"  AirflowNetwork:MultiZone:Surface,",
			"    Zn001:Wall001:Win001,    !- Surface Name",
			"    CRcri,                   !- Leakage Component Name",
			"    SFacade,                 !- External Node Name",
			"    0.5;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

			"  AirflowNetwork:MultiZone:Surface,",
			"    Zn001:Wall002,           !- Surface Name",
			"    CR-1,                    !- Leakage Component Name",
			"    WFacade,                 !- External Node Name",
			"    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

			"  AirflowNetwork:MultiZone:Surface,",
			"    Zn001:Wall003,           !- Surface Name",
			"    CRcri,                   !- Leakage Component Name",
			"    ,                        !- External Node Name",
			"    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

			"  AirflowNetwork:MultiZone:Surface,",
			"    Zn001:Wall003:Door001,   !- Surface Name",
			"    CRcri,                   !- Leakage Component Name",
			"    ,                        !- External Node Name",
			"    0.5;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

			"  AirflowNetwork:MultiZone:Surface,",
			"    Zn001:Wall004,           !- Surface Name",
			"    CRcri,                   !- Leakage Component Name",
			"    ,                        !- External Node Name",
			"    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

			"  AirflowNetwork:MultiZone:Surface,",
			"    Zn001:Ceil001,           !- Surface Name",
			"    CRcri,                   !- Leakage Component Name",
			"    ,                        !- External Node Name",
			"    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

			"  AirflowNetwork:MultiZone:Surface,",
			"    Zn002:Wall002,           !- Surface Name",
			"    CR-1,                    !- Leakage Component Name",
			"    SFacade,                 !- External Node Name",
			"    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

			"  AirflowNetwork:MultiZone:Surface,",
			"    Zn002:Wall003,           !- Surface Name",
			"    CR-1,                    !- Leakage Component Name",
			"    EFacade,                 !- External Node Name",
			"    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

			"  AirflowNetwork:MultiZone:Surface,",
			"    Zn002:Wall005,           !- Surface Name",
			"    CRcri,                   !- Leakage Component Name",
			"    ,                        !- External Node Name",
			"    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

			"  AirflowNetwork:MultiZone:Surface,",
			"    Zn002:Ceil001,           !- Surface Name",
			"    CRcri,                   !- Leakage Component Name",
			"    ,                        !- External Node Name",
			"    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

			"  AirflowNetwork:MultiZone:Surface,",
			"    Zn003:Wall001,           !- Surface Name",
			"    CR-1,                    !- Leakage Component Name",
			"    WFacade,                 !- External Node Name",
			"    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

			"  AirflowNetwork:MultiZone:Surface,",
			"    Zn003:Wall002,           !- Surface Name",
			"    CR-1,                    !- Leakage Component Name",
			"    NFacade,                 !- External Node Name",
			"    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

			"  AirflowNetwork:MultiZone:Surface,",
			"    Zn003:Wall002:Win001,    !- Surface Name",
			"    CRcri,                   !- Leakage Component Name",
			"    NFacade,                 !- External Node Name",
			"    0.5;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

			"  AirflowNetwork:MultiZone:Surface,",
			"    Zn003:Wall003,           !- Surface Name",
			"    Zone3 Exhaust Fan,       !- Leakage Component Name",
			"    EFacade,                 !- External Node Name",
			"    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

			"  AirflowNetwork:MultiZone:Surface,",
			"    Zn003:Ceil001,           !- Surface Name",
			"    CRcri,                   !- Leakage Component Name",
			"    ,                        !- External Node Name",
			"    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

			"  AirflowNetwork:MultiZone:Surface,",
			"    Zn004:Roof001,           !- Surface Name",
			"    CR-1,                    !- Leakage Component Name",
			"    Horizontal,              !- External Node Name",
			"    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

			"  AirflowNetwork:MultiZone:Surface,",
			"    Zn004:Roof002,           !- Surface Name",
			"    CR-1,                    !- Leakage Component Name",
			"    Horizontal,              !- External Node Name",
			"    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

			"  AirflowNetwork:MultiZone:Surface,",
			"    Zn004:Roof003,           !- Surface Name",
			"    CR-1,                    !- Leakage Component Name",
			"    Horizontal,              !- External Node Name",
			"    1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}",

			"  AirflowNetwork:MultiZone:ReferenceCrackConditions,",
			"    ReferenceCrackConditions,!- Name",
			"    20.0,                    !- Reference Temperature {C}",
			"    101325,                  !- Reference Barometric Pressure {Pa}",
			"    0.0;                     !- Reference Humidity Ratio {kgWater/kgDryAir}",

			"  AirflowNetwork:MultiZone:Surface:Crack,",
			"    CR-1,                    !- Name",
			"    0.01,                    !- Air Mass Flow Coefficient at Reference Conditions {kg/s}",
			"    0.667;                   !- Air Mass Flow Exponent {dimensionless}",

			"  AirflowNetwork:MultiZone:Surface:Crack,",
			"    CRcri,                   !- Name",
			"    0.05,                    !- Air Mass Flow Coefficient at Reference Conditions {kg/s}",
			"    0.667;                   !- Air Mass Flow Exponent {dimensionless}",

			"  AirflowNetwork:MultiZone:Component:ZoneExhaustFan,",
			"    Zone3 Exhaust Fan,       !- Name",
			"    0.01,                    !- Air Mass Flow Coefficient When the Zone Exhaust Fan is Off at Reference Conditions {kg/s}",
			"    0.667;                   !- Air Mass Flow Exponent When the Zone Exhaust Fan is Off {dimensionless}",

			"  AirflowNetwork:MultiZone:Surface:EffectiveLeakageArea,",
			"    ELA-1,                   !- Name",
			"    0.007,                   !- Effective Leakage Area {m2}",
			"    1.0,                     !- Discharge Coefficient {dimensionless}",
			"    4.0,                     !- Reference Pressure Difference {Pa}",
			"    0.667;                   !- Air Mass Flow Exponent {dimensionless}",

			"  AirflowNetwork:MultiZone:ExternalNode,",
			"    NFacade,                 !- Name",
			"    1.524,                   !- External Node Height {m}",
			"    NFacade_WPCValue;        !- Wind Pressure Coefficient Values Object Name",

			"  AirflowNetwork:MultiZone:ExternalNode,",
			"    EFacade,                 !- Name",
			"    1.524,                   !- External Node Height {m}",
			"    EFacade_WPCValue;        !- Wind Pressure Coefficient Values Object Name",

			"  AirflowNetwork:MultiZone:ExternalNode,",
			"    SFacade,                 !- Name",
			"    1.524,                   !- External Node Height {m}",
			"    SFacade_WPCValue;        !- Wind Pressure Coefficient Values Object Name",

			"  AirflowNetwork:MultiZone:ExternalNode,",
			"    WFacade,                 !- Name",
			"    1.524,                   !- External Node Height {m}",
			"    WFacade_WPCValue;        !- Wind Pressure Coefficient Values Object Name",

			"  AirflowNetwork:MultiZone:ExternalNode,",
			"    Horizontal,              !- Name",
			"    3.028,                   !- External Node Height {m}",
			"    Horizontal_WPCValue;     !- Wind Pressure Coefficient Values Object Name",

			"  AirflowNetwork:MultiZone:WindPressureCoefficientArray,",
			"    Every 30 Degrees,        !- Name",
			"    0,                       !- Wind Direction 1 {deg}",
			"    30,                      !- Wind Direction 2 {deg}",
			"    60,                      !- Wind Direction 3 {deg}",
			"    90,                      !- Wind Direction 4 {deg}",
			"    120,                     !- Wind Direction 5 {deg}",
			"    150,                     !- Wind Direction 6 {deg}",
			"    180,                     !- Wind Direction 7 {deg}",
			"    210,                     !- Wind Direction 8 {deg}",
			"    240,                     !- Wind Direction 9 {deg}",
			"    270,                     !- Wind Direction 10 {deg}",
			"    300,                     !- Wind Direction 11 {deg}",
			"    330;                     !- Wind Direction 12 {deg}",

			"  AirflowNetwork:MultiZone:WindPressureCoefficientValues,",
			"    NFacade_WPCValue,        !- Name",
			"    Every 30 Degrees,        !- AirflowNetwork:MultiZone:WindPressureCoefficientArray Name",
			"    0.60,                    !- Wind Pressure Coefficient Value 1 {dimensionless}",
			"    0.48,                    !- Wind Pressure Coefficient Value 2 {dimensionless}",
			"    0.04,                    !- Wind Pressure Coefficient Value 3 {dimensionless}",
			"    -0.56,                   !- Wind Pressure Coefficient Value 4 {dimensionless}",
			"    -0.56,                   !- Wind Pressure Coefficient Value 5 {dimensionless}",
			"    -0.42,                   !- Wind Pressure Coefficient Value 6 {dimensionless}",
			"    -0.37,                   !- Wind Pressure Coefficient Value 7 {dimensionless}",
			"    -0.42,                   !- Wind Pressure Coefficient Value 8 {dimensionless}",
			"    -0.56,                   !- Wind Pressure Coefficient Value 9 {dimensionless}",
			"    -0.56,                   !- Wind Pressure Coefficient Value 10 {dimensionless}",
			"    0.04,                    !- Wind Pressure Coefficient Value 11 {dimensionless}",
			"    0.48;                    !- Wind Pressure Coefficient Value 12 {dimensionless}",

			"  AirflowNetwork:MultiZone:WindPressureCoefficientValues,",
			"    EFacade_WPCValue,        !- Name",
			"    Every 30 Degrees,        !- AirflowNetwork:MultiZone:WindPressureCoefficientArray Name",
			"    -0.56,                   !- Wind Pressure Coefficient Value 1 {dimensionless}",
			"    0.04,                    !- Wind Pressure Coefficient Value 2 {dimensionless}",
			"    0.48,                    !- Wind Pressure Coefficient Value 3 {dimensionless}",
			"    0.60,                    !- Wind Pressure Coefficient Value 4 {dimensionless}",
			"    0.48,                    !- Wind Pressure Coefficient Value 5 {dimensionless}",
			"    0.04,                    !- Wind Pressure Coefficient Value 6 {dimensionless}",
			"    -0.56,                   !- Wind Pressure Coefficient Value 7 {dimensionless}",
			"    -0.56,                   !- Wind Pressure Coefficient Value 8 {dimensionless}",
			"    -0.42,                   !- Wind Pressure Coefficient Value 9 {dimensionless}",
			"    -0.37,                   !- Wind Pressure Coefficient Value 10 {dimensionless}",
			"    -0.42,                   !- Wind Pressure Coefficient Value 11 {dimensionless}",
			"    -0.56;                   !- Wind Pressure Coefficient Value 12 {dimensionless}",

			"  AirflowNetwork:MultiZone:WindPressureCoefficientValues,",
			"    SFacade_WPCValue,        !- Name",
			"    Every 30 Degrees,        !- AirflowNetwork:MultiZone:WindPressureCoefficientArray Name",
			"    -0.37,                   !- Wind Pressure Coefficient Value 1 {dimensionless}",
			"    -0.42,                   !- Wind Pressure Coefficient Value 2 {dimensionless}",
			"    -0.56,                   !- Wind Pressure Coefficient Value 3 {dimensionless}",
			"    -0.56,                   !- Wind Pressure Coefficient Value 4 {dimensionless}",
			"    0.04,                    !- Wind Pressure Coefficient Value 5 {dimensionless}",
			"    0.48,                    !- Wind Pressure Coefficient Value 6 {dimensionless}",
			"    0.60,                    !- Wind Pressure Coefficient Value 7 {dimensionless}",
			"    0.48,                    !- Wind Pressure Coefficient Value 8 {dimensionless}",
			"    0.04,                    !- Wind Pressure Coefficient Value 9 {dimensionless}",
			"    -0.56,                   !- Wind Pressure Coefficient Value 10 {dimensionless}",
			"    -0.56,                   !- Wind Pressure Coefficient Value 11 {dimensionless}",
			"    -0.42;                   !- Wind Pressure Coefficient Value 12 {dimensionless}",

			"  AirflowNetwork:MultiZone:WindPressureCoefficientValues,",
			"    WFacade_WPCValue,        !- Name",
			"    Every 30 Degrees,        !- AirflowNetwork:MultiZone:WindPressureCoefficientArray Name",
			"    -0.56,                   !- Wind Pressure Coefficient Value 1 {dimensionless}",
			"    -0.56,                   !- Wind Pressure Coefficient Value 2 {dimensionless}",
			"    -0.42,                   !- Wind Pressure Coefficient Value 3 {dimensionless}",
			"    -0.37,                   !- Wind Pressure Coefficient Value 4 {dimensionless}",
			"    -0.42,                   !- Wind Pressure Coefficient Value 5 {dimensionless}",
			"    -0.56,                   !- Wind Pressure Coefficient Value 6 {dimensionless}",
			"    -0.56,                   !- Wind Pressure Coefficient Value 7 {dimensionless}",
			"    0.04,                    !- Wind Pressure Coefficient Value 8 {dimensionless}",
			"    0.48,                    !- Wind Pressure Coefficient Value 9 {dimensionless}",
			"    0.60,                    !- Wind Pressure Coefficient Value 10 {dimensionless}",
			"    0.48,                    !- Wind Pressure Coefficient Value 11 {dimensionless}",
			"    0.04;                    !- Wind Pressure Coefficient Value 12 {dimensionless}",

			"  AirflowNetwork:MultiZone:WindPressureCoefficientValues,",
			"    Horizontal_WPCValue,     !- Name",
			"    Every 30 Degrees,        !- AirflowNetwork:MultiZone:WindPressureCoefficientArray Name",
			"    0.00,                    !- Wind Pressure Coefficient Value 1 {dimensionless}",
			"    0.00,                    !- Wind Pressure Coefficient Value 2 {dimensionless}",
			"    0.00,                    !- Wind Pressure Coefficient Value 3 {dimensionless}",
			"    0.00,                    !- Wind Pressure Coefficient Value 4 {dimensionless}",
			"    0.00,                    !- Wind Pressure Coefficient Value 5 {dimensionless}",
			"    0.00,                    !- Wind Pressure Coefficient Value 6 {dimensionless}",
			"    0.00,                    !- Wind Pressure Coefficient Value 7 {dimensionless}",
			"    0.00,                    !- Wind Pressure Coefficient Value 8 {dimensionless}",
			"    0.00,                    !- Wind Pressure Coefficient Value 9 {dimensionless}",
			"    0.00,                    !- Wind Pressure Coefficient Value 10 {dimensionless}",
			"    0.00,                    !- Wind Pressure Coefficient Value 11 {dimensionless}",
			"    0.00;                    !- Wind Pressure Coefficient Value 12 {dimensionless}",

			"  AirflowNetwork:Distribution:Node,",
			"    EquipmentInletNode,      !- Name",
			"    Zone Equipment Inlet Node,  !- Component Name or Node Name",
			"    Other,                   !- Component Object Type or Node Type",
			"    3.0;                     !- Node Height {m}",

			"  AirflowNetwork:Distribution:Node,",
			"    EquipmentOutletNode,     !- Name",
			"    ,                        !- Component Name or Node Name",
			"    Other,                   !- Component Object Type or Node Type",
			"    3.0;                     !- Node Height {m}",

			"  AirflowNetwork:Distribution:Node,",
			"    SupplyMainNode,          !- Name",
			"    ,                        !- Component Name or Node Name",
			"    Other,                   !- Component Object Type or Node Type",
			"    3.0;                     !- Node Height {m}",

			"  AirflowNetwork:Distribution:Node,",
			"    MainSplitterNode,        !- Name",
			"    ,                        !- Component Name or Node Name",
			"    AirLoopHVAC:ZoneSplitter,!- Component Object Type or Node Type",
			"    3.0;                     !- Node Height {m}",

			"  AirflowNetwork:Distribution:Node,",
			"    Zone1SupplyNode,         !- Name",
			"    ,                        !- Component Name or Node Name",
			"    Other,                   !- Component Object Type or Node Type",
			"    3.0;                     !- Node Height {m}",

			"  AirflowNetwork:Distribution:Node,",
			"    ReheatInlet1Node,        !- Name",
			"    Zone 1 Reheat Air Inlet Node,  !- Component Name or Node Name",
			"    Other,                   !- Component Object Type or Node Type",
			"    3.0;                     !- Node Height {m}",

			"  AirflowNetwork:Distribution:Node,",
			"    Zone1SupplyRegisterNode, !- Name",
			"    Zone 1 Reheat Air Outlet Node,  !- Component Name or Node Name",
			"    Other,                   !- Component Object Type or Node Type",
			"    3.0;                     !- Node Height {m}",

			"  AirflowNetwork:Distribution:Node,",
			"    Zone1OutletNode,         !- Name",
			"    Zone 1 Outlet Node,      !- Component Name or Node Name",
			"    Other,                   !- Component Object Type or Node Type",
			"    3.0;                     !- Node Height {m}",

			"  AirflowNetwork:Distribution:Node,",
			"    Zone2SupplyNode,         !- Name",
			"    ,                        !- Component Name or Node Name",
			"    Other,                   !- Component Object Type or Node Type",
			"    3.0;                     !- Node Height {m}",

			"  AirflowNetwork:Distribution:Node,",
			"    ReheatInlet2Node,        !- Name",
			"    Zone 2 Reheat Air Inlet Node,  !- Component Name or Node Name",
			"    Other,                   !- Component Object Type or Node Type",
			"    3.0;                     !- Node Height {m}",

			"  AirflowNetwork:Distribution:Node,",
			"    Zone2SupplyRegisterNode, !- Name",
			"    Zone 2 Reheat Air Outlet Node,  !- Component Name or Node Name",
			"    Other,                   !- Component Object Type or Node Type",
			"    3.0;                     !- Node Height {m}",

			"  AirflowNetwork:Distribution:Node,",
			"    Zone2OutletNode,         !- Name",
			"    Zone 2 Outlet Node,      !- Component Name or Node Name",
			"    Other,                   !- Component Object Type or Node Type",
			"    3.0;                     !- Node Height {m}",

			"  AirflowNetwork:Distribution:Node,",
			"    Zone3SupplyNode,         !- Name",
			"    ,                        !- Component Name or Node Name",
			"    Other,                   !- Component Object Type or Node Type",
			"    3.0;                     !- Node Height {m}",

			"  AirflowNetwork:Distribution:Node,",
			"    Zone3SupplyRegisterNode, !- Name",
			"    Zone 3 Inlet Node,       !- Component Name or Node Name",
			"    ,                        !- Component Object Type or Node Type",
			"    3.0;                     !- Node Height {m}",

			"  AirflowNetwork:Distribution:Node,",
			"    Zone3OutletNode,         !- Name",
			"    Zone 3 Outlet Node,      !- Component Name or Node Name",
			"    Other,                   !- Component Object Type or Node Type",
			"    3.0;                     !- Node Height {m}",

			"  AirflowNetwork:Distribution:Node,",
			"    Zone1ReturnNode,         !- Name",
			"    ,                        !- Component Name or Node Name",
			"    Other,                   !- Component Object Type or Node Type",
			"    3.0;                     !- Node Height {m}",

			"  AirflowNetwork:Distribution:Node,",
			"    Zone2ReturnNode,         !- Name",
			"    ,                        !- Component Name or Node Name",
			"    Other,                   !- Component Object Type or Node Type",
			"    3.0;                     !- Node Height {m}",

			"  AirflowNetwork:Distribution:Node,",
			"    Zone3ReturnNode,         !- Name",
			"    ,                        !- Component Name or Node Name",
			"    Other,                   !- Component Object Type or Node Type",
			"    3.0;                     !- Node Height {m}",

			"  AirflowNetwork:Distribution:Node,",
			"    MainMixerNode,           !- Name",
			"    ,                        !- Component Name or Node Name",
			"    AirLoopHVAC:ZoneMixer,   !- Component Object Type or Node Type",
			"    3.0;                     !- Node Height {m}",

			"  AirflowNetwork:Distribution:Node,",
			"    MainReturnNode,          !- Name",
			"    Return Air Mixer Outlet, !- Component Name or Node Name",
			"    Other,                   !- Component Object Type or Node Type",
			"    3.0;                     !- Node Height {m}",

			"  AirflowNetwork:Distribution:Node,",
			"    MainInletNode,           !- Name",
			"    Air Loop Inlet Node,     !- Component Name or Node Name",
			"    Other,                   !- Component Object Type or Node Type",
			"    3.0;                     !- Node Height {m}",

			"  AirflowNetwork:Distribution:Node,",
			"    OA System Node,          !- Name",
			"    ,                        !- Component Name or Node Name",
			"    AirLoopHVAC:OutdoorAirSystem,  !- Component Object Type or Node Type",
			"    3.0;                     !- Node Height {m}",

			"  AirflowNetwork:Distribution:Node,",
			"    OA Inlet Node,           !- Name",
			"    Outside Air Inlet Node,  !- Component Name or Node Name",
			"    OAMixerOutdoorAirStreamNode,  !- Component Object Type or Node Type",
			"    1.5;                     !- Node Height {m}",

			"  AirflowNetwork:Distribution:Node,",
			"    FanInletNode,            !- Name",
			"    Mixed Air Node,          !- Component Name or Node Name",
			"    Other,                   !- Component Object Type or Node Type",
			"    3.0;                     !- Node Height {m}",

			"  AirflowNetwork:Distribution:Node,",
			"    FanOutletNode,           !- Name",
			"    Cooling Coil Air Inlet Node,  !- Component Name or Node Name",
			"    Other,                   !- Component Object Type or Node Type",
			"    3.0;                     !- Node Height {m}",

			"  AirflowNetwork:Distribution:Node,",
			"    HeatingInletNode,        !- Name",
			"    Heating Coil Air Inlet Node,  !- Component Name or Node Name",
			"    Other,                   !- Component Object Type or Node Type",
			"    3.0;                     !- Node Height {m}",

			"  AirflowNetwork:Distribution:Node,",
			"    HeatingOutletNode,       !- Name",
			"    Air Loop Outlet Node,    !- Component Name or Node Name",
			"    Other,                   !- Component Object Type or Node Type",
			"    3.0;                     !- Node Height {m}",

		    "  AirflowNetwork:Distribution:Component:Leak,",
			"    MainSupplyLeak,          !- Name",
			"    0.0025,                  !- Air Mass Flow Coefficient {kg/s}",
			"    0.65;                    !- Air Mass Flow Exponent {dimensionless}",

			"  AirflowNetwork:Distribution:Component:ConstantPressureDrop,",
			"    SupplyCPDComp,           !- Name",
			"    1.0;                     !- Pressure Difference Across the Component {Pa}",

			"  AirflowNetwork:Distribution:Component:LeakageRatio,",
			"    ZoneSupplyELR1,          !- Name",
			"    0.01,                    !- Effective Leakage Ratio {dimensionless}",
			"    1.9,                     !- Maximum Flow Rate {m3/s}",
			"    59.0,                    !- Reference Pressure Difference {Pa}",
			"    0.65;                    !- Air Mass Flow Exponent {dimensionless}",

			"  AirflowNetwork:Distribution:Component:LeakageRatio,",
			"    ZoneSupplyELR2,          !- Name",
			"    0.01,                    !- Effective Leakage Ratio {dimensionless}",
			"    1.9,                     !- Maximum Flow Rate {m3/s}",
			"    59.0,                    !- Reference Pressure Difference {Pa}",
			"    0.65;                    !- Air Mass Flow Exponent {dimensionless}",

			"  AirflowNetwork:Distribution:Component:LeakageRatio,",
			"    ZoneSupplyELR3,          !- Name",
			"    0.01,                    !- Effective Leakage Ratio {dimensionless}",
			"    1.9,                     !- Maximum Flow Rate {m3/s}",
			"    59.0,                    !- Reference Pressure Difference {Pa}",
			"    0.65;                    !- Air Mass Flow Exponent {dimensionless}",

			"  AirflowNetwork:Distribution:Component:LeakageRatio,",
			"    ReturnLeakELR1,          !- Name",
			"    0.03,                    !- Effective Leakage Ratio {dimensionless}",
			"    1.9,                     !- Maximum Flow Rate {m3/s}",
			"    41.0,                    !- Reference Pressure Difference {Pa}",
			"    0.65;                    !- Air Mass Flow Exponent {dimensionless}",

			"  AirflowNetwork:Distribution:Component:LeakageRatio,",
			"    ReturnLeakELR2,          !- Name",
			"    0.03,                    !- Effective Leakage Ratio {dimensionless}",
			"    1.9,                     !- Maximum Flow Rate {m3/s}",
			"    40.0,                    !- Reference Pressure Difference {Pa}",
			"    0.65;                    !- Air Mass Flow Exponent {dimensionless}",

			"  AirflowNetwork:Distribution:Component:LeakageRatio,",
			"    ReturnLeakELR3,          !- Name",
			"    0.04,                    !- Effective Leakage Ratio {dimensionless}",
			"    1.9,                     !- Maximum Flow Rate {m3/s}",
			"    43.0,                    !- Reference Pressure Difference {Pa}",
			"    0.65;                    !- Air Mass Flow Exponent {dimensionless}",

			"  AirflowNetwork:Distribution:Component:Duct,",
			"    MainTruck1,              !- Name",
			"    3.0,                     !- Duct Length {m}",
			"    0.6,                     !- Hydraulic Diameter {m}",
			"    0.2827,                  !- Cross Section Area {m2}",
			"    0.0009,                  !- Surface Roughness {m}",
			"    0.01,                    !- Coefficient for Local Dynamic Loss Due to Fitting {dimensionless}",
			"    0.772,                   !- Overall Heat Transmittance Coefficient (U-Factor) from Air to Air {W/m2-K}",
			"    0.0001;                  !- Overall Moisture Transmittance Coefficient from Air to Air {kg/m2}",

			"  AirflowNetwork:Distribution:Component:Duct,",
			"    MainTruck2,              !- Name",
			"    4.0,                     !- Duct Length {m}",
			"    0.6,                     !- Hydraulic Diameter {m}",
			"    0.2827,                  !- Cross Section Area {m2}",
			"    0.0009,                  !- Surface Roughness {m}",
			"    0.01,                    !- Coefficient for Local Dynamic Loss Due to Fitting {dimensionless}",
			"    0.772,                   !- Overall Heat Transmittance Coefficient (U-Factor) from Air to Air {W/m2-K}",
			"    0.0001;                  !- Overall Moisture Transmittance Coefficient from Air to Air {kg/m2}",

			"  AirflowNetwork:Distribution:Component:Duct,",
			"    Zone1Supply,             !- Name",
			"    5.0,                     !- Duct Length {m}",
			"    0.4,                     !- Hydraulic Diameter {m}",
			"    0.1256,                  !- Cross Section Area {m2}",
			"    0.0009,                  !- Surface Roughness {m}",
			"    1.00,                    !- Coefficient for Local Dynamic Loss Due to Fitting {dimensionless}",
			"    0.772,                   !- Overall Heat Transmittance Coefficient (U-Factor) from Air to Air {W/m2-K}",
			"    0.0001;                  !- Overall Moisture Transmittance Coefficient from Air to Air {kg/m2}",

			"  AirflowNetwork:Distribution:Component:Duct,",
			"    Zone2Supply,             !- Name",
			"    4.0,                     !- Duct Length {m}",
			"    0.39,                    !- Hydraulic Diameter {m}",
			"    0.1195,                  !- Cross Section Area {m2}",
			"    0.0009,                  !- Surface Roughness {m}",
			"    2.5,                     !- Coefficient for Local Dynamic Loss Due to Fitting {dimensionless}",
			"    0.772,                   !- Overall Heat Transmittance Coefficient (U-Factor) from Air to Air {W/m2-K}",
			"    0.0001;                  !- Overall Moisture Transmittance Coefficient from Air to Air {kg/m2}",

			"  AirflowNetwork:Distribution:Component:Duct,",
			"    Zone3Supply,             !- Name",
			"    4.0,                     !- Duct Length {m}",
			"    0.44,                    !- Hydraulic Diameter {m}",
			"    0.1521,                  !- Cross Section Area {m2}",
			"    0.0009,                  !- Surface Roughness {m}",
			"    1.0,                     !- Coefficient for Local Dynamic Loss Due to Fitting {dimensionless}",
			"    0.772,                   !- Overall Heat Transmittance Coefficient (U-Factor) from Air to Air {W/m2-K}",
			"    0.0001;                  !- Overall Moisture Transmittance Coefficient from Air to Air {kg/m2}",

			"  AirflowNetwork:Distribution:Component:Duct,",
			"    Zone1Return,             !- Name",
			"    4.0,                     !- Duct Length {m}",
			"    0.50,                    !- Hydraulic Diameter {m}",
			"    0.1963,                  !- Cross Section Area {m2}",
			"    0.0009,                  !- Surface Roughness {m}",
			"    1.0,                     !- Coefficient for Local Dynamic Loss Due to Fitting {dimensionless}",
			"    0.772,                   !- Overall Heat Transmittance Coefficient (U-Factor) from Air to Air {W/m2-K}",
			"    0.0001;                  !- Overall Moisture Transmittance Coefficient from Air to Air {kg/m2}",

			"  AirflowNetwork:Distribution:Component:Duct,",
			"    Zone2Return,             !- Name",
			"    4.0,                     !- Duct Length {m}",
			"    0.48,                    !- Hydraulic Diameter {m}",
			"    0.1809,                  !- Cross Section Area {m2}",
			"    0.0009,                  !- Surface Roughness {m}",
			"    1.0,                     !- Coefficient for Local Dynamic Loss Due to Fitting {dimensionless}",
			"    0.772,                   !- Overall Heat Transmittance Coefficient (U-Factor) from Air to Air {W/m2-K}",
			"    0.0001;                  !- Overall Moisture Transmittance Coefficient from Air to Air {kg/m2}",

			"  AirflowNetwork:Distribution:Component:Duct,",
			"    Zone3Return,             !- Name",
			"    4.0,                     !- Duct Length {m}",
			"    0.55,                    !- Hydraulic Diameter {m}",
			"    0.2376,                  !- Cross Section Area {m2}",
			"    0.0009,                  !- Surface Roughness {m}",
			"    1.0,                     !- Coefficient for Local Dynamic Loss Due to Fitting {dimensionless}",
			"    0.772,                   !- Overall Heat Transmittance Coefficient (U-Factor) from Air to Air {W/m2-K}",
			"    0.0001;                  !- Overall Moisture Transmittance Coefficient from Air to Air {kg/m2}",

			"  AirflowNetwork:Distribution:Component:Duct,",
			"    ZoneConnectionDuct,      !- Name",
			"    0.1,                     !- Duct Length {m}",
			"    1.0,                     !- Hydraulic Diameter {m}",
			"    0.7854,                  !- Cross Section Area {m2}",
			"    0.0001,                  !- Surface Roughness {m}",
			"    30.00,                   !- Coefficient for Local Dynamic Loss Due to Fitting {dimensionless}",
			"    0.001,                   !- Overall Heat Transmittance Coefficient (U-Factor) from Air to Air {W/m2-K}",
			"    0.0001;                  !- Overall Moisture Transmittance Coefficient from Air to Air {kg/m2}",

			"  AirflowNetwork:Distribution:Component:Duct,",
			"    MixerConnectionDuct,     !- Name",
			"    0.1,                     !- Duct Length {m}",
			"    1.0,                     !- Hydraulic Diameter {m}",
			"    0.7854,                  !- Cross Section Area {m2}",
			"    0.0001,                  !- Surface Roughness {m}",
			"    1.00,                    !- Coefficient for Local Dynamic Loss Due to Fitting {dimensionless}",
			"    0.001,                   !- Overall Heat Transmittance Coefficient (U-Factor) from Air to Air {W/m2-K}",
			"    0.0001;                  !- Overall Moisture Transmittance Coefficient from Air to Air {kg/m2}",


			"  AirflowNetwork:Distribution:Component:Duct,",
			"    AirLoopReturn,           !- Name",
			"    0.1,                     !- Duct Length {m}",
			"    1.0,                     !- Hydraulic Diameter {m}",
			"    0.7854,                  !- Cross Section Area {m2}",
			"    0.0001,                  !- Surface Roughness {m}",
			"    1.00,                    !- Coefficient for Local Dynamic Loss Due to Fitting {dimensionless}",
			"    0.001,                   !- Overall Heat Transmittance Coefficient (U-Factor) from Air to Air {W/m2-K}",
			"    0.0001;                  !- Overall Moisture Transmittance Coefficient from Air to Air {kg/m2}",


			"  AirflowNetwork:Distribution:Component:Duct,",
			"    AirLoopSupply,           !- Name",
			"    0.1,                     !- Duct Length {m}",
			"    1.0,                     !- Hydraulic Diameter {m}",
			"    0.7854,                  !- Cross Section Area {m2}",
			"    0.0001,                  !- Surface Roughness {m}",
			"    1.00,                    !- Coefficient for Local Dynamic Loss Due to Fitting {dimensionless}",
			"    0.001,                   !- Overall Heat Transmittance Coefficient (U-Factor) from Air to Air {W/m2-K}",
			"    0.0001;                  !- Overall Moisture Transmittance Coefficient from Air to Air {kg/m2}",

			"  AirflowNetwork:Distribution:Component:Fan,",
			"    Supply Fan 1,            !- Fan Name",
			"    Fan:ConstantVolume;      !- Supply Fan Object Type",

			"  AirflowNetwork:Distribution:Component:Coil,",
			"    ACDXCoil 1,              !- Coil Name",
			"    Coil:Cooling:DX:SingleSpeed,  !- Coil Object Type",
			"    0.1,                     !- Air Path Length {m}",
			"    1.00;                    !- Air Path Hydraulic Diameter {m}",

			"  AirflowNetwork:Distribution:Component:Coil,",
			"    Main Heating Coil 1,     !- Coil Name",
			"    Coil:Heating:Gas,        !- Coil Object Type",
			"    0.1,                     !- Air Path Length {m}",
			"    1.00;                    !- Air Path Hydraulic Diameter {m}",

			"  AirflowNetwork:Distribution:Component:TerminalUnit,",
			"    Reheat Zone 1,           !- Terminal Unit Name",
			"    AirTerminal:SingleDuct:ConstantVolume:Reheat,  !- Terminal Unit Object Type",
			"    0.1,                     !- Air Path Length {m}",
			"    0.44;                    !- Air Path Hydraulic Diameter {m}",

			"  AirflowNetwork:Distribution:Component:TerminalUnit,",
			"    Reheat Zone 2,           !- Terminal Unit Name",
			"    AirTerminal:SingleDuct:ConstantVolume:Reheat,  !- Terminal Unit Object Type",
			"    0.1,                     !- Air Path Length {m}",
			"    0.44;                    !- Air Path Hydraulic Diameter {m}",

			"  AirflowNetwork:Distribution:Component:Leak,",
			"    OAFlow,          !- Name",
			"    0.025,                  !- Air Mass Flow Coefficient {kg/s}",
			"    0.65;                    !- Air Mass Flow Exponent {dimensionless}",

			"  AirflowNetwork:Distribution:Component:Leak,",
			"    OAFlow1,          !- Name",
			"    0.025,                  !- Air Mass Flow Coefficient {kg/s}",
			"    0.65;                    !- Air Mass Flow Exponent {dimensionless}",

			"  AirflowNetwork:Distribution:Linkage,",
			"    Main Link 1,             !- Name",
			"    EquipmentInletNode,      !- Node 1 Name",
			"    EquipmentOutletNode,     !- Node 2 Name",
			"    MainTruck1,              !- Component Name",
			"    Attic Zone;              !- Thermal Zone Name",

			"  AirflowNetwork:Distribution:Linkage,",
			"    Main CDP Link,           !- Name",
			"    EquipmentOutletNode,     !- Node 1 Name",
			"    SupplyMainNode,          !- Node 2 Name",
			"    SupplyCPDComp;           !- Component Name",

			"  AirflowNetwork:Distribution:Linkage,",
			"    Main Link 2,             !- Name",
			"    SupplyMainNode,          !- Node 1 Name",
			"    MainSplitterNode,        !- Node 2 Name",
			"    MainTruck2,              !- Component Name",
			"    Attic Zone;              !- Thermal Zone Name",

			"  AirflowNetwork:Distribution:Linkage,",
			"    Zone1Supply1Link,        !- Name",
			"    MainSplitterNode,        !- Node 1 Name",
			"    Zone1SupplyNode,         !- Node 2 Name",
			"    Zone1Supply,             !- Component Name",
			"    Attic Zone;              !- Thermal Zone Name",

			"  AirflowNetwork:Distribution:Linkage,",
			"    Zone1Supply2Link,        !- Name",
			"    Zone1SupplyNode,         !- Node 1 Name",
			"    ReheatInlet1Node,        !- Node 2 Name",
			"    Zone1Supply,             !- Component Name",
			"    Attic Zone;              !- Thermal Zone Name",

			"  AirflowNetwork:Distribution:Linkage,",
			"    Zone1ReheatCoilLink,     !- Name",
			"    ReheatInlet1Node,        !- Node 1 Name",
			"    Zone1SupplyRegisterNode, !- Node 2 Name",
			"    Reheat Zone 1;           !- Component Name",

			"  AirflowNetwork:Distribution:Linkage,",
			"    Zone1SupplyConnectionLink,  !- Name",
			"    Zone1SupplyRegisterNode, !- Node 1 Name",
			"    West Zone,               !- Node 2 Name",
			"    ZoneConnectionDuct;      !- Component Name",

			"  AirflowNetwork:Distribution:Linkage,",
			"    Zone1ReturnConnectionLink,  !- Name",
			"    West Zone,               !- Node 1 Name",
			"    Zone1OutletNode,         !- Node 2 Name",
			"    ZoneConnectionDuct;      !- Component Name",

			"  AirflowNetwork:Distribution:Linkage,",
			"    Zone2Supply1Link,        !- Name",
			"    MainSplitterNode,        !- Node 1 Name",
			"    Zone2SupplyNode,         !- Node 2 Name",
			"    Zone2Supply,             !- Component Name",
			"    Attic Zone;              !- Thermal Zone Name",

			"  AirflowNetwork:Distribution:Linkage,",
			"    Zone2Supply2Link,        !- Name",
			"    Zone2SupplyNode,         !- Node 1 Name",
			"    ReheatInlet2Node,        !- Node 2 Name",
			"    Zone2Supply,             !- Component Name",
			"    Attic Zone;              !- Thermal Zone Name",

			"  AirflowNetwork:Distribution:Linkage,",
			"    Zone2ReheatCoilLink,     !- Name",
			"    ReheatInlet2Node,        !- Node 1 Name",
			"    Zone2SupplyRegisterNode, !- Node 2 Name",
			"    Reheat Zone 2;           !- Component Name",

			"  AirflowNetwork:Distribution:Linkage,",
			"    Zone2SupplyConnectionLink,  !- Name",
			"    Zone2SupplyRegisterNode, !- Node 1 Name",
			"    EAST ZONE,               !- Node 2 Name",
			"    ZoneConnectionDuct;      !- Component Name",

			"  AirflowNetwork:Distribution:Linkage,",
			"    Zone2returnConnectionLink,  !- Name",
			"    EAST ZONE,               !- Node 1 Name",
			"    Zone2OutletNode,         !- Node 2 Name",
			"    ZoneConnectionDuct;      !- Component Name",

			"  AirflowNetwork:Distribution:Linkage,",
			"    Zone3Supply1Link,        !- Name",
			"    MainSplitterNode,        !- Node 1 Name",
			"    Zone3SupplyNode,         !- Node 2 Name",
			"    Zone3Supply,             !- Component Name",
			"    Attic Zone;              !- Thermal Zone Name",

			"  AirflowNetwork:Distribution:Linkage,",
			"    Zone3SupplyLink,         !- Name",
			"    Zone3SupplyNode,         !- Node 1 Name",
			"    Zone3SupplyRegisterNode, !- Node 2 Name",
			"    Zone3Supply,             !- Component Name",
			"    Attic Zone;              !- Thermal Zone Name",

			"  AirflowNetwork:Distribution:Linkage,",
			"    Zone3SupplyConnectionLink,  !- Name",
			"    Zone3SupplyRegisterNode, !- Node 1 Name",
			"    NORTH ZONE,              !- Node 2 Name",
			"    ZoneConnectionDuct;      !- Component Name",

			"  AirflowNetwork:Distribution:Linkage,",
			"    Zone3ReturnConnectionLink,  !- Name",
			"    NORTH ZONE,              !- Node 1 Name",
			"    Zone3OutletNode,         !- Node 2 Name",
			"    ZoneConnectionDuct;      !- Component Name",

			"  AirflowNetwork:Distribution:Linkage,",
			"    Zone1Return1Link,        !- Name",
			"    Zone1OutletNode,         !- Node 1 Name",
			"    Zone1ReturnNode,         !- Node 2 Name",
			"    Zone1Return,             !- Component Name",
			"    Attic Zone;              !- Thermal Zone Name",

			"  AirflowNetwork:Distribution:Linkage,",
			"    Zone1Return2Link,        !- Name",
			"    Zone1ReturnNode,         !- Node 1 Name",
			"    MainMixerNode,           !- Node 2 Name",
			"    Zone1Return,             !- Component Name",
			"    Attic Zone;              !- Thermal Zone Name",

			"  AirflowNetwork:Distribution:Linkage,",
			"    Zone2Return1Link,        !- Name",
			"    Zone2OutletNode,         !- Node 1 Name",
			"    Zone2ReturnNode,         !- Node 2 Name",
			"    Zone2Return,             !- Component Name",
			"    Attic Zone;              !- Thermal Zone Name",

			"  AirflowNetwork:Distribution:Linkage,",
			"    Zone2Return2Link,        !- Name",
			"    Zone2ReturnNode,         !- Node 1 Name",
			"    MainMixerNode,           !- Node 2 Name",
			"    Zone2Return,             !- Component Name",
			"    Attic Zone;              !- Thermal Zone Name",

			"  AirflowNetwork:Distribution:Linkage,",
			"    Zone3Return1Link,        !- Name",
			"    Zone3OutletNode,         !- Node 1 Name",
			"    Zone3ReturnNode,         !- Node 2 Name",
			"    Zone3Return,             !- Component Name",
			"    Attic Zone;              !- Thermal Zone Name",

			"  AirflowNetwork:Distribution:Linkage,",
			"    Zone3Return2Link,        !- Name",
			"    Zone3ReturnNode,         !- Node 1 Name",
			"    MainMixerNode,           !- Node 2 Name",
			"    Zone3Return,             !- Component Name",
			"    Attic Zone;              !- Thermal Zone Name",

			"  AirflowNetwork:Distribution:Linkage,",
			"    ReturnMixerLink,         !- Name",
			"    MainMixerNode,           !- Node 1 Name",
			"    MainReturnNode,          !- Node 2 Name",
			"    MixerConnectionDuct,     !- Component Name",
			"    Attic Zone;              !- Thermal Zone Name",

			"  AirflowNetwork:Distribution:Linkage,",
			"    AirLoopReturnLink,       !- Name",
			"    MainReturnNode,          !- Node 1 Name",
			"    MainInletNode,           !- Node 2 Name",
			"    AirLoopReturn;           !- Component Name",

			"  AirflowNetwork:Distribution:Linkage,",
			"    OASystemInletLink,       !- Name",
			"    MainInletNode,           !- Node 1 Name",
			"    OA System Node,          !- Node 2 Name",
			"    ZoneConnectionDuct;      !- Component Name",

			"  AirflowNetwork:Distribution:Linkage,",
			"    OASystemFanLink,         !- Name",
			"    OA Inlet Node,           !- Node 1 Name",
			"    OA System Node,          !- Node 2 Name",
			"    OA Fan;                  !- Component Name",

			"  AirflowNetwork:Distribution:Linkage,",
			"    OASystemReliefLink,      !- Name",
			"    OA System Node,          !- Node 1 Name",
			"    OA Inlet Node,           !- Node 2 Name",
			"    Relief Fan;              !- Component Name",

			"  AirflowNetwork:Distribution:Linkage,",
			"    OAMixerOutletLink,       !- Name",
			"    OA System Node,          !- Node 1 Name",
			"    FanInletNode,            !- Node 2 Name",
			"    ZoneConnectionDuct;      !- Component Name",

			"  AirflowNetwork:Distribution:Linkage,",
			"    SupplyFanLink,           !- Name",
			"    FanInletNode,            !- Node 1 Name",
			"    FanOutletNode,           !- Node 2 Name",
			"    Supply Fan 1;            !- Component Name",

			"  AirflowNetwork:Distribution:Linkage,",
			"    CoolingCoilLink,         !- Name",
			"    FanOutletNode,           !- Node 1 Name",
			"    HeatingInletNode,        !- Node 2 Name",
			"    ACDXCoil 1;              !- Component Name",

			"  AirflowNetwork:Distribution:Linkage,",
			"    HeatingCoilLink,         !- Name",
			"    HeatingInletNode,        !- Node 1 Name",
			"    HeatingOutletNode,       !- Node 2 Name",
			"    Main Heating Coil 1;     !- Component Name",

			"  AirflowNetwork:Distribution:Linkage,",
			"    EquipmentAirLoopLink,    !- Name",
			"    HeatingOutletNode,       !- Node 1 Name",
			"    EquipmentInletNode,      !- Node 2 Name",
			"    AirLoopSupply;           !- Component Name",

			"  AirflowNetwork:Distribution:Linkage,",
			"    Zone1ReturnLeakLink,     !- Name",
			"    Zone1ReturnNode,         !- Node 1 Name",
			"    OA Inlet Node,           !- Node 2 Name",
			"    ReturnLeakELR1;          !- Component Name",

			"  AirflowNetwork:Distribution:Linkage,",
			"    MainSupplyLeakLink,      !- Name",
			"    SupplyMainNode,          !- Node 1 Name",
			"    ATTIC ZONE,              !- Node 2 Name",
			"    MainSupplyLeak;          !- Component Name",

			"  AirflowNetwork:Distribution:Linkage,",
			"    Zone1SupplyLeakLink,     !- Name",
			"    Zone1SupplyNode,         !- Node 1 Name",
			"    ATTIC ZONE,              !- Node 2 Name",
			"    ZoneSupplyELR1;          !- Component Name",

			"  AirflowNetwork:Distribution:Linkage,",
			"    Zone2ReturnLeakLink,     !- Name",
			"    Zone2ReturnNode,         !- Node 1 Name",
			"    OA Inlet Node,           !- Node 2 Name",
			"    ReturnLeakELR2;          !- Component Name",

			"  AirflowNetwork:Distribution:Linkage,",
			"    Zone3ReturnLeakLink,     !- Name",
			"    Zone3ReturnNode,         !- Node 1 Name",
			"    OA Inlet Node,           !- Node 2 Name",
			"    ReturnLeakELR3;          !- Component Name",

			"  AirflowNetwork:Distribution:Linkage,",
			"    Zone2SupplyLeakLink,     !- Name",
			"    Zone2SupplyNode,         !- Node 1 Name",
			"    ATTIC ZONE,              !- Node 2 Name",
			"    ZoneSupplyELR2;          !- Component Name",

			"  AirflowNetwork:Distribution:Linkage,",
			"    Zone3SupplyLeakLink,     !- Name",
			"    Zone3SupplyNode,         !- Node 1 Name",
			"    ATTIC ZONE,              !- Node 2 Name",
			"    ZoneSupplyELR3;          !- Component Name",

			"  AirflowNetwork:ZoneControl:PressureController,",
			"    PressureController,      !- Name",
			"    NORTH ZONE,              !- Zone Name",
			"    AirflowNetwork:Distribution:Component:ReliefAirFlow, !- Control Object Type",
			"    Relief Fan,              !- Control Name",
			"    ,                        !- Pressure Control Availability Schedule Name",
			"    Pressure Setpoint Schedule; !- Pressure Setpoint Schedule Name",

			"  Schedule:Compact,",
			"    Pressure Setpoint Schedule,   !- Name",
			"    Any Number,              !- Schedule Type Limits Name",
			"    Through: 3/31,           !- Field 1",
			"    For: AllDays,            !- Field 2",
			"    Until: 24:00,0.5,        !- Field 3",
			"    Through: 9/30,           !- Field 4",
			"    For: AllDays,            !- Field 5",
			"    Until: 24:00,3.5,        !- Field 6",
			"    Through: 12/31,          !- Field 7",
			"    For: AllDays,            !- Field 8",
			"    Until: 24:00,0.5;        !- Field 9",

			"  AirflowNetwork:Distribution:Component:OutdoorAirFlow,",
			"    OA Fan,                  !- Name",
			"    0.001,                   !- Air Mass Flow Coefficient When the Zone Exhaust Fan is Off at Reference Conditions {kg/s}",
			"    0.667;                   !- Air Mass Flow Exponent When the Zone Exhaust Fan is Off {dimensionless}",

			"  AirflowNetwork:Distribution:Component:ReliefAirFlow,",
			"    Relief Fan,              !- Name",
			"    0.001,                   !- Air Mass Flow Coefficient When the Zone Exhaust Fan is Off at Reference Conditions {kg/s}",
			"    0.667;                   !- Air Mass Flow Exponent When the Zone Exhaust Fan is Off {dimensionless}",

			"  AirLoopHVAC,",
			"    Typical Terminal Reheat 1,  !- Name",
			"    ,                        !- Controller List Name",
			"    Reheat System 1 Avail List,  !- Availability Manager List Name",
			"    1.9,                     !- Design Supply Air Flow Rate {m3/s}",
			"    Air Loop Branches,       !- Branch List Name",
			"    ,                        !- Connector List Name",
			"    Air Loop Inlet Node,     !- Supply Side Inlet Node Name",
			"    Return Air Mixer Outlet, !- Demand Side Outlet Node Name",
			"    Zone Equipment Inlet Node,  !- Demand Side Inlet Node Names",
			"    Air Loop Outlet Node;    !- Supply Side Outlet Node Names",

			"  Fan:ZoneExhaust,",
			"    Zone3 Exhaust Fan,       !- Name",
			"    On,    !- Availability Schedule Name",
			"    0.7,                     !- Fan Total Efficiency",
			"    500,                     !- Pressure Rise {Pa}",
			"    0.01,                    !- Maximum Flow Rate {m3/s}",
			"    Zone3 Exhaust Node,      !- Air Inlet Node Name",
			"    Zone3 Exhaust Fan Outlet Node,  !- Air Outlet Node Name",
			"    Zone Exhaust;            !- End-Use Subcategory",

			"  Fan:ConstantVolume,",
			"    Supply Fan 1,            !- Name",
			"    FanAndCoilAvailSched,    !- Availability Schedule Name",
			"    0.7,                     !- Fan Total Efficiency",
			"    600.0,                   !- Pressure Rise {Pa}",
			"    1.9,                     !- Maximum Flow Rate {m3/s}",
			"    0.9,                     !- Motor Efficiency",
			"    1.0,                     !- Motor In Airstream Fraction",
			"    Mixed Air Node,          !- Air Inlet Node Name",
			"    Cooling Coil Air Inlet Node;  !- Air Outlet Node Name",

		} );

		ASSERT_FALSE( process_idf( idf_objects ) );

		bool ErrorsFound = false;
		HeatBalanceManager::GetProjectControlData( ErrorsFound );
		EXPECT_FALSE( ErrorsFound );
		HeatBalanceManager::GetZoneData( ErrorsFound );
		EXPECT_FALSE( ErrorsFound );
		HeatBalanceManager::GetWindowGlassSpectralData( ErrorsFound );
		EXPECT_FALSE( ErrorsFound );
		HeatBalanceManager::GetMaterialData( ErrorsFound );
		EXPECT_FALSE( ErrorsFound );
		HeatBalanceManager::GetConstructData( ErrorsFound );
		EXPECT_FALSE( ErrorsFound );

		SurfaceGeometry::GetGeometryParameters( ErrorsFound );
		EXPECT_FALSE( ErrorsFound );

		SurfaceGeometry::CosBldgRotAppGonly = 1.0;
		SurfaceGeometry::SinBldgRotAppGonly = 0.0;
		SurfaceGeometry::GetSurfaceData( ErrorsFound );
		EXPECT_FALSE( ErrorsFound );

		GetAirflowNetworkInput( );

		Real64 PresssureSet = 0.5;
		Schedule( 1 ).CurrentValue = PresssureSet; // Pressure setpoint
		Schedule( 2 ).CurrentValue = 1.0; // set availability and fan schedule to 1
		Schedule( 3 ).CurrentValue = 1.0; // On
		Schedule( 4 ).CurrentValue = 25.55; // WindowVentSched
		Schedule( 5 ).CurrentValue = 1.0; // VentingSched

		AirflowNetworkFanActivated = true;
		DataEnvironment::OutDryBulbTemp = -17.29025;
		DataEnvironment::OutHumRat = 0.0008389;
		DataEnvironment::OutBaroPress = 99063.0;
		DataEnvironment::WindSpeed = 4.9;
		DataEnvironment::WindDir = 270.0;

		for ( i = 1; i <= 36; ++i ) {
			AirflowNetworkNodeSimu( i ).TZ = 23.0;
			AirflowNetworkNodeSimu( i ).WZ = 0.0008400;
			if ( ( i > 4 && i < 10 ) || i == 32) {
				AirflowNetworkNodeSimu( i ).TZ = DataEnvironment::OutDryBulbTempAt( AirflowNetworkNodeData( i ).NodeHeight );
				AirflowNetworkNodeSimu( i ).WZ = DataEnvironment::OutHumRat;
			}
		}

		Node.allocate( 10 );
		if ( MultizoneCompExhaustFanData( 1 ).InletNode == 0 ) {
			MultizoneCompExhaustFanData( 1 ).InletNode = 3;
		}
		Node( MultizoneCompExhaustFanData( 1 ).InletNode ).MassFlowRate = 0.1005046;

		if ( DisSysCompCVFData( 1 ).InletNode == 0 ) {
			DisSysCompCVFData( 1 ).InletNode = 1;
		}
		Node( DisSysCompCVFData( 1 ).InletNode ).MassFlowRate = 2.23418088;

		if ( DisSysCompOutdoorAirData( 1 ).InletNode == 0 ) {
			DisSysCompOutdoorAirData( 1 ).InletNode = 5;
			DisSysCompOutdoorAirData( 1 ).OutletNode = 6;
		}
		Node( DisSysCompOutdoorAirData( 1 ).InletNode ).MassFlowRate = 0.5095108;
		Node( DisSysCompOutdoorAirData( 1 ).OutletNode ).MassFlowRate = 0.5095108;

		if ( DisSysCompReliefAirData( 1 ).InletNode == 0 ) {
			DisSysCompReliefAirData( 1 ).InletNode = 6;
			DisSysCompReliefAirData( 1 ).OutletNode = 5;
		}

		CalcAirflowNetworkAirBalance( );

		EXPECT_NEAR( PresssureSet, AirflowNetworkNodeSimu( 3 ).PZ, 0.0001 );
		EXPECT_NEAR( 0.00255337, ReliefMassFlowRate, 0.0001 );

		Node.deallocate( );
	
	}
}
