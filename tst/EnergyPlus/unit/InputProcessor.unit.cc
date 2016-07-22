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

// EnergyPlus::InputProcessor Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/DataOutputs.hh>
#include <EnergyPlus/InputProcessor.hh>
#include <EnergyPlus/SortAndStringUtilities.hh>

#include "Fixtures/InputProcessorFixture.hh"

#include <tuple>
#include <map>

namespace EnergyPlus {

    TEST_F( InputProcessorFixture, decode_encode_1 ) {
        auto const idf = delimited_string({
                                                  "Building,",
                                                  "  Ref Bldg Medium Office New2004_v1.3_5.0,",
                                                  "  0.000000,",
                                                  "  City,",
                                                  "  0.040000,",
                                                  "  0.200000,",
                                                  "  FullInteriorAndExterior,",
                                                  "  25.000000,",
                                                  "  6.000000;",
                                                  "",
                                                  "BuildingSurface:Detailed,",
                                                  "  Zn009:Flr001,",
                                                  "  Floor,",
                                                  "  FLOOR38,",
                                                  "  SCWINDOW,",
                                                  "  Surface,",
                                                  "  Zn009:Flr001,",
                                                  "  NoSun,",
                                                  "  NoWind,",
                                                  "  1.000000,",
                                                  "  4.000000,",
                                                  "  10.000000,",
                                                  "  0.000000,",
                                                  "  0.000000,",
                                                  "  0.000000,",
                                                  "  0.000000,",
                                                  "  0.000000,",
                                                  "  0.000000,",
                                                  "  10.000000,",
                                                  "  0.000000,",
                                                  "  10.000000,",
                                                  "  10.000000,",
                                                  "  0.000000;",
                                                  "",
                                                  "GlobalGeometryRules,",
                                                  "  UpperLeftCorner,",
                                                  "  Counterclockwise,",
                                                  "  Relative,",
                                                  "  Relative,",
                                                  "  Relative;",
                                                  ""
                                          });
        ASSERT_TRUE(process_idf(idf));
        std::string encoded = InputProcessor::idf_parser.encode(InputProcessor::jdf, InputProcessor::schema);
        EXPECT_EQ(idf, encoded);
    }


    TEST_F( InputProcessorFixture, decode_encode_2 ) {
        auto const idf(delimited_string({
                                                "Zone,",
                                                "  Core_mid,",
                                                "  0.000000,",
                                                "  0.000000,",
                                                "  0.000000,",
                                                "  0.000000,",
                                                "  1.000000,",
                                                "  1.000000,",
                                                "  ,",
                                                "  ,",
                                                "  autocalculate,",
                                                "  ,",
                                                "  ,",
                                                "  Yes;",
                                        }));

        auto const expected(delimited_string({
                                                     "Building,",
                                                     "  Bldg,",
                                                     "  0.000000,",
                                                     "  Suburbs,",
                                                     "  0.040000,",
                                                     "  0.400000,",
                                                     "  FullExterior,",
                                                     "  25.000000,",
                                                     "  6.000000;",
                                                     "",
                                                     "GlobalGeometryRules,",
                                                     "  UpperLeftCorner,",
                                                     "  Counterclockwise,",
                                                     "  Relative,",
                                                     "  Relative,",
                                                     "  Relative;"
                                                     "",
                                                     "",
													                           "Zone,",
                                                     "  Core_mid,",
                                                     "  0.000000,",
                                                     "  0.000000,",
                                                     "  0.000000,",
                                                     "  0.000000,",
                                                     "  1.000000,",
                                                     "  1.000000,",
                                                     "  ,",
                                                     "  ,",
                                                     "  Autocalculate,",
                                                     "  ,",
                                                     "  ,",
                                                     "  Yes;",
                                                     ""
                                             }));

        ASSERT_TRUE(process_idf(idf));
        std::string encoded = InputProcessor::idf_parser.encode(InputProcessor::jdf, InputProcessor::schema);
        EXPECT_EQ(expected, encoded);
    }

    TEST_F( InputProcessorFixture, parse_empty_fields) {
        std::string const idf( delimited_string( {
                                                         "  Building,",
                                                         "    Ref Bldg Medium Office New2004_v1.3_5.0,  !- Name",
                                                         "    ,                  !- North Axis {deg}",
                                                         "    ,                    !- Terrain",
                                                         "    ,                  !- Loads Convergence Tolerance Value",
                                                         "    0.2000,                  !- Temperature Convergence Tolerance Value {deltaC}",
                                                         "    , !- Solar Distribution",
                                                         "    25,                      !- Maximum Number of Warmup Days",
                                                         "    6;",
                                                 } ) );

        json expected = {
                {
                        "Building",
                        {
                                {
                                        "Ref Bldg Medium Office New2004_v1.3_5.0",
                                        {
                                                {"north_axis", ""},
                                                {"terrain", ""},
                                                {"loads_convergence_tolerance_value", ""},
                                                {"temperature_convergence_tolerance_value", 0.2000},
                                                {"solar_distribution", ""},
                                                {"maximum_number_of_warmup_days", 25},
                                                {"minimum_number_of_warmup_days", 6}
                                        }
                                }
                        }
                }
        };

        ASSERT_TRUE( process_idf(idf) );
        json tmp;
        for (auto it = expected.begin(); it != expected.end(); ++it) {
            ASSERT_NO_THROW(tmp = InputProcessor::jdf[it.key()]);
            for (auto it_in = it.value().begin(); it_in != it.value().end(); ++it_in) {
                ASSERT_NO_THROW(tmp = InputProcessor::jdf[it.key()][it_in.key()]);
                for (auto it_in_in = it_in.value().begin(); it_in_in != it_in.value().end(); ++it_in_in) {
                    ASSERT_NO_THROW(tmp = InputProcessor::jdf[it.key()][it_in.key()][it_in_in.key()]);
                    EXPECT_EQ(tmp.dump(), it_in_in.value().dump());
                }
            }
        }
    }


    TEST_F( InputProcessorFixture, parse_idf_and_validate_two_non_extensible_objects ) {
        std::string const idf( delimited_string( {
                                                         "  Building,",
                                                         "    Ref Bldg Medium Office New2004_v1.3_5.0,  !- Name",
                                                         "    0.0000,                  !- North Axis {deg}",
                                                         "    City,                    !- Terrain",
                                                         "    0.0400123456789123,                  !- Loads Convergence Tolerance Value",
                                                         "    0.2000,                  !- Temperature Convergence Tolerance Value {deltaC}",
                                                         "    FullInteriorAndExterior, !- Solar Distribution",
                                                         "    25,                      !- Maximum Number of Warmup Days",
                                                         "    6;",
                                                         "",
                                                         "  Building,",
                                                         "    Another Building Name,  !- Name",
                                                         "    0.0000,                  !- North Axis {deg}",
                                                         "    City,                    !- Terrain",
                                                         "    0.0400,                  !- Loads Convergence Tolerance Value",
                                                         "    0.2000,                  !- Temperature Convergence Tolerance Value {deltaC}",
                                                         "    FullInteriorAndExterior, !- Solar Distribution",
                                                         "    25,                      !- Maximum Number of Warmup Days",
                                                         "    6;",
                                                 } ) );

		json expected =
				{
						{
								"Building",
								{
										{
												"Ref Bldg Medium Office New2004_v1.3_5.0",
												{
														{"north_axis", 0.0000},
														{"terrain", "City"},
														{"loads_convergence_tolerance_value", 0.0400123456789123},
														{"temperature_convergence_tolerance_value", 0.2000},
														{"solar_distribution", "FullInteriorAndExterior"},
														{"maximum_number_of_warmup_days", 25},
														{"minimum_number_of_warmup_days", 6}
												},
										},
										{
												"Another Building Name",
												{
														{"north_axis", 0.0000},
														{"terrain", "City"},
														{"loads_convergence_tolerance_value", 0.0400},
														{"temperature_convergence_tolerance_value", 0.2000},
														{"solar_distribution", "FullInteriorAndExterior"},
														{"maximum_number_of_warmup_days", 25},
														{"minimum_number_of_warmup_days", 6}
												},
										}
								}
						},
						{
								"GlobalGeometryRules",
								{
										{
												"",
												{
														{"starting_vertex_position", "UpperLeftCorner"},
														{"vertex_entry_direction", "Counterclockwise"},
														{"coordinate_system", "Relative"},
														{"daylighting_reference_point_coordinate_system", "Relative"},
														{"rectangular_surface_coordinate_system", "Relative"}
												}
										}
								}
						}
				};

        ASSERT_TRUE(process_idf(idf));

        json tmp;
        for (auto it = expected.begin(); it != expected.end(); ++it) {
            ASSERT_NO_THROW(tmp = InputProcessor::jdf[it.key()]);
            for (auto it_in = it.value().begin(); it_in != it.value().end(); ++it_in) {
                ASSERT_NO_THROW(tmp = InputProcessor::jdf[it.key()][it_in.key()]);
                for (auto it_in_in = it_in.value().begin(); it_in_in != it_in.value().end(); ++it_in_in) {
                    ASSERT_NO_THROW(tmp = InputProcessor::jdf[it.key()][it_in.key()][it_in_in.key()]);
                    EXPECT_EQ(tmp.dump(), it_in_in.value().dump());
                }
            }
        }
        EXPECT_EQ(InputProcessor::state.errors.size(), 0);
    }


    TEST_F(InputProcessorFixture, parse_idf_extensible_blank_extensibles) {
        size_t index = 0;
        bool success = true;

        std::string const idf(delimited_string(
                {
                        "BuildingSurface:Detailed,",
                        "Zn009:Flr001,            !- Name",
                        "    Floor,                   !- Surface Type",
                        "    FLOOR38,                 !- Construction Name",
                        "    SCWINDOW,                !- Zone Name",
                        "    Surface,                 !- Outside Boundary Condition",
                        "    Zn009:Flr001,            !- Outside Boundary Condition Object",
                        "    NoSun,                   !- Sun Exposure",
                        "    NoWind,                  !- Wind Exposure",
                        "    1.000000,                !- View Factor to Ground",
                        "    4,                       !- Number of Vertices",
                        "    "",10,0,  !- X,Y,Z ==> Vertex 1 {m}",
                        "    0.000000,,0,  !- X,Y,Z ==> Vertex 2 {m}",
                        "    0.000000,10.00000,0,  !- X,Y,Z ==> Vertex 3 {m}",
                        "    "",10.00000,"";  !- X,Y,Z ==> Vertex 4 {m}"
                }));

        json expected =
                {
                        {
								"BuildingSurface:Detailed",
                                {
                                        {"Zn009:Flr001",
                                                {
                                                        {"surface_type", "Floor"},
                                                        {"construction_name", "FLOOR38"},
                                                        {"zone_name", "SCWINDOW"},
                                                        {"outside_boundary_condition", "Surface"},
                                                        {"outside_boundary_condition_object", "Zn009:Flr001"},
                                                        {"sun_exposure", "NoSun"},
                                                        {"wind_exposure", "NoWind"},
                                                        {"view_factor_to_ground", 1.000000},
                                                        {"number_of_vertices", 4},
                                                        {"extensions", {
                                                                               {
                                                                                       {"vertex_x_coordinate", ""},
                                                                                       {"vertex_y_coordinate", 10},
                                                                                       {"vertex_z_coordinate", 0}
                                                                               },
                                                                               {
                                                                                       {"vertex_x_coordinate", 0.0},
                                                                                       {"vertex_y_coordinate", ""},
                                                                                       {"vertex_z_coordinate", 0}
                                                                               },
                                                                               {
                                                                                       {"vertex_x_coordinate", 0.0},
                                                                                       {"vertex_y_coordinate", 10},
                                                                                       {"vertex_z_coordinate", 0}
                                                                               },
                                                                               {
                                                                                       {"vertex_x_coordinate", ""},
                                                                                       {"vertex_y_coordinate", 10},
                                                                                       {"vertex_z_coordinate", ""}
                                                                               }
                                                                       }
                                                        }
                                                }
                                        }
                                }
                        },
						{
								"GlobalGeometryRules",
								{
										{
												"",
												{
														{"starting_vertex_position", "UpperLeftCorner"},
														{"vertex_entry_direction", "Counterclockwise"},
														{"coordinate_system", "Relative"},
														{"daylighting_reference_point_coordinate_system", "Relative"},
														{"rectangular_surface_coordinate_system", "Relative"}
												}
										}
								}
						},
						{
								"Building",
								{
										{
												"Bldg",
												{
														{"north_axis", 0.0},
														{"terrain", "Suburbs"},
														{"loads_convergence_tolerance_value", 0.04},
														{"temperature_convergence_tolerance_value", 0.4000},
														{"solar_distribution", "FullExterior"},
														{"maximum_number_of_warmup_days", 25},
														{"minimum_number_of_warmup_days", 6}
												}
										}
								}
						}
                };

		ASSERT_TRUE( process_idf( idf ) );
        json tmp;
        for (auto it = expected.begin(); it != expected.end(); ++it) {
            ASSERT_NO_THROW(tmp = InputProcessor::jdf[it.key()]);
            for (auto it_in = it.value().begin(); it_in != it.value().end(); ++it_in) {
                ASSERT_NO_THROW(tmp = InputProcessor::jdf[it.key()][it_in.key()]);
                for (auto it_in_in = it_in.value().begin(); it_in_in != it_in.value().end(); ++it_in_in) {
                    ASSERT_NO_THROW(tmp = InputProcessor::jdf[it.key()][it_in.key()][it_in_in.key()]);
                    if (!tmp.is_array()) {
                        EXPECT_EQ(tmp.dump(), it_in_in.value().dump());
                    } else {
                        for (int i = 0; i < it_in_in.value().size(); i++) {
                            for (auto it_ext = it_in_in.value()[i].begin(); it_ext != it_in_in.value()[i].end(); ++it_ext) {
                                ASSERT_NO_THROW(tmp = InputProcessor::jdf[it.key()][it_in.key()][it_in_in.key()][i][it_ext.key()]);
                                EXPECT_EQ(tmp.dump(), it_ext.value().dump());
                            }
                        }
                    }
                }
            }
        }
    }


    TEST_F(InputProcessorFixture, parse_idf_and_validate_extensible) {
        size_t index = 0;
        bool success = true;

        std::string const idf(delimited_string(
                {
                        "BuildingSurface:Detailed,",
                        "Zn009:Flr001,            !- Name",
                        "    Floor,                   !- Surface Type",
                        "    FLOOR38,                 !- Construction Name",
                        "    SCWINDOW,                !- Zone Name",
                        "    Surface,                 !- Outside Boundary Condition",
                        "    Zn009:Flr001,            !- Outside Boundary Condition Object",
                        "    NoSun,                   !- Sun Exposure",
                        "    NoWind,                  !- Wind Exposure",
                        "    1.000000,                !- View Factor to Ground",
                        "    4,                       !- Number of Vertices",
                        "    10.00000,0.000000,0,  !- X,Y,Z ==> Vertex 1 {m}",
                        "    0.000000,0.000000,0,  !- X,Y,Z ==> Vertex 2 {m}",
                        "    0.000000,10.00000,0,  !- X,Y,Z ==> Vertex 3 {m}",
                        "    10.00000,10.00000,0;  !- X,Y,Z ==> Vertex 4 {m}"
                }));

        json expected =
                {
                        {"BuildingSurface:Detailed",
                                {
                                        {"Zn009:Flr001",
                                                {
                                                        {"surface_type", "Floor"},
                                                        {"construction_name", "FLOOR38"},
                                                        {"zone_name", "SCWINDOW"},
                                                        {"outside_boundary_condition", "Surface"},
                                                        {"outside_boundary_condition_object", "Zn009:Flr001"},
                                                        {"sun_exposure", "NoSun"},
                                                        {"wind_exposure", "NoWind"},
                                                        {"view_factor_to_ground", 1.000000},
                                                        {"number_of_vertices", 4},
                                                        {"extensions", {
                                                                               {
                                                                                       {"vertex_x_coordinate", 10},
                                                                                       {"vertex_y_coordinate", 0.0},
                                                                                       {"vertex_z_coordinate", 0}
                                                                               },
                                                                               {
                                                                                       {"vertex_x_coordinate", 0.0},
                                                                                       {"vertex_y_coordinate", 0.0},
                                                                                       {"vertex_z_coordinate", 0}
                                                                               },
                                                                               {
                                                                                       {"vertex_x_coordinate", 0.0},
                                                                                       {"vertex_y_coordinate", 10},
                                                                                       {"vertex_z_coordinate", 0}
                                                                               },
                                                                               {
                                                                                       {"vertex_x_coordinate", 10},
                                                                                       {"vertex_y_coordinate", 10},
                                                                                       {"vertex_z_coordinate", 0}
                                                                               }
                                                                       }
                                                        }
                                                }
                                        }
                                }
                        }
                };

		ASSERT_TRUE( process_idf( idf ) );
        json tmp;
        for (auto it = expected.begin(); it != expected.end(); ++it) {
            ASSERT_NO_THROW(tmp = InputProcessor::jdf[it.key()]);
            for (auto it_in = it.value().begin(); it_in != it.value().end(); ++it_in) {
                ASSERT_NO_THROW(tmp = InputProcessor::jdf[it.key()][it_in.key()]);
                for (auto it_in_in = it_in.value().begin(); it_in_in != it_in.value().end(); ++it_in_in) {
                    ASSERT_NO_THROW(tmp = InputProcessor::jdf[it.key()][it_in.key()][it_in_in.key()]);
                    if (!tmp.is_array()) {
                        EXPECT_EQ(tmp.dump(), it_in_in.value().dump());
                    } else {
                        for (int i = 0; i < it_in_in.value().size(); i++) {
                            for (auto it_ext = it_in_in.value()[i].begin(); it_ext != it_in_in.value()[i].end(); ++it_ext) {
                                ASSERT_NO_THROW(tmp = InputProcessor::jdf[it.key()][it_in.key()][it_in_in.key()][i][it_ext.key()]);
                                EXPECT_EQ(tmp.dump(), it_ext.value().dump());
                            }
                        }
                    }
                }
            }
        }
        json::parse(InputProcessor::jdf.dump(2), EnergyPlusFixture::call_back);
        EXPECT_EQ(InputProcessor::state.errors.size() + InputProcessor::state.warnings.size(), 0);
        EXPECT_TRUE(success);
    }

    TEST_F(InputProcessorFixture, parse_idf_and_validate_two_extensible_objects) {
        size_t index = 0;
        bool success = true;

        std::string const idf(delimited_string(
                {
                        "BuildingSurface:Detailed,",
                        "Zn009:Flr001,            !- Name",
                        "    Floor,                   !- Surface Type",
                        "    FLOOR38,                 !- Construction Name",
                        "    SCWINDOW,                !- Zone Name",
                        "    Surface,                 !- Outside Boundary Condition",
                        "    Zn009:Flr001,            !- Outside Boundary Condition Object",
                        "    NoSun,                   !- Sun Exposure",
                        "    NoWind,                  !- Wind Exposure",
                        "    1.000000,                !- View Factor to Ground",
                        "    4,                       !- Number of Vertices",
                        "    10.00000,0.000000,0,  !- X,Y,Z ==> Vertex 1 {m}",
                        "    0.000000,0.000000,0,  !- X,Y,Z ==> Vertex 2 {m}",
                        "    0.000000,10.00000,0,  !- X,Y,Z ==> Vertex 3 {m}",
                        "    10.00000,10.00000,0;  !- X,Y,Z ==> Vertex 4 {m}",
                        "",
                        "BuildingSurface:Detailed,",
                        "Some Surface Name,            !- Name",
                        "    Floor,                   !- Surface Type",
                        "    FLOOR38,                 !- Construction Name",
                        "    SCWINDOW,                !- Zone Name",
                        "    Surface,                 !- Outside Boundary Condition",
                        "    Zn009:Flr001,            !- Outside Boundary Condition Object",
                        "    NoSun,                   !- Sun Exposure",
                        "    NoWind,                  !- Wind Exposure",
                        "    1.000000,                !- View Factor to Ground",
                        "    4,                       !- Number of Vertices",
                        "    10.00000,0.000000,0,  !- X,Y,Z ==> Vertex 1 {m}",
                        "    0.000000,0.000000,0,  !- X,Y,Z ==> Vertex 2 {m}",
                        "    0.000000,10.00000,0,  !- X,Y,Z ==> Vertex 3 {m}",
                        "    10.00000,10.00000,0;  !- X,Y,Z ==> Vertex 4 {m}"
                }));

        json expected =
                {
                        {
								"BuildingSurface:Detailed",
                                {
                                        {"Zn009:Flr001",
                                                {
                                                        {"surface_type", "Floor"},
                                                        {"construction_name", "FLOOR38"},
                                                        {"zone_name", "SCWINDOW"},
                                                        {"outside_boundary_condition", "Surface"},
                                                        {"outside_boundary_condition_object", "Zn009:Flr001"},
                                                        {"sun_exposure", "NoSun"},
                                                        {"wind_exposure", "NoWind"},
                                                        {"view_factor_to_ground", 1.000000},
                                                        {"number_of_vertices", 4},
                                                        {"extensions", {
                                                                               {
                                                                                       {"vertex_x_coordinate", 10},
                                                                                       {"vertex_y_coordinate", 0.0},
                                                                                       {"vertex_z_coordinate", 0}
                                                                               },
                                                                               {
                                                                                       {"vertex_x_coordinate", 0.0},
                                                                                       {"vertex_y_coordinate", 0.0},
                                                                                       {"vertex_z_coordinate", 0}
                                                                               },
                                                                               {
                                                                                       {"vertex_x_coordinate", 0.0},
                                                                                       {"vertex_y_coordinate", 10},
                                                                                       {"vertex_z_coordinate", 0}
                                                                               },
                                                                               {
                                                                                       {"vertex_x_coordinate", 10},
                                                                                       {"vertex_y_coordinate", 10},
                                                                                       {"vertex_z_coordinate", 0}
                                                                               }
                                                                       }
                                                        }
                                                }
                                        },
                                        {"Some Surface Name",
                                                {
                                                        {"surface_type", "Floor"},
                                                        {"construction_name", "FLOOR38"},
                                                        {"zone_name", "SCWINDOW"},
                                                        {"outside_boundary_condition", "Surface"},
                                                        {"outside_boundary_condition_object", "Zn009:Flr001"},
                                                        {"sun_exposure", "NoSun"},
                                                        {"wind_exposure", "NoWind"},
                                                        {"view_factor_to_ground", 1.000000},
                                                        {"number_of_vertices", 4},
                                                        {"extensions", {
                                                                               {
                                                                                       {"vertex_x_coordinate", 10},
                                                                                       {"vertex_y_coordinate", 0.0},
                                                                                       {"vertex_z_coordinate", 0}
                                                                               },
                                                                               {
                                                                                       {"vertex_x_coordinate", 0.0},
                                                                                       {"vertex_y_coordinate", 0.0},
                                                                                       {"vertex_z_coordinate", 0}
                                                                               },
                                                                               {
                                                                                       {"vertex_x_coordinate", 0.0},
                                                                                       {"vertex_y_coordinate", 10},
                                                                                       {"vertex_z_coordinate", 0}
                                                                               },
                                                                               {
                                                                                       {"vertex_x_coordinate", 10},
                                                                                       {"vertex_y_coordinate", 10},
                                                                                       {"vertex_z_coordinate", 0}
                                                                               }
                                                                       }
                                                        }
                                                }
                                        }
                                }
                        },
						{
								"GlobalGeometryRules",
								{
										{
												"",
												{
														{"starting_vertex_position", "UpperLeftCorner"},
														{"vertex_entry_direction", "Counterclockwise"},
														{"coordinate_system", "Relative"},
														{"daylighting_reference_point_coordinate_system", "Relative"},
														{"rectangular_surface_coordinate_system", "Relative"}
												}
										}
								}
						},
						{
								"Building",
								{
										{
												"Bldg",
												{
														{"north_axis", 0.0},
														{"terrain", "Suburbs"},
														{"loads_convergence_tolerance_value", 0.04},
														{"temperature_convergence_tolerance_value", 0.4000},
														{"solar_distribution", "FullExterior"},
														{"maximum_number_of_warmup_days", 25},
														{"minimum_number_of_warmup_days", 6}
												}
										}
								}
						}
                };

		ASSERT_TRUE( process_idf( idf ) );
        json tmp;
        for (auto it = expected.begin(); it != expected.end(); ++it) {
            ASSERT_NO_THROW(tmp = InputProcessor::jdf[it.key()]);
            for (auto it_in = it.value().begin(); it_in != it.value().end(); ++it_in) {
                ASSERT_NO_THROW(tmp = InputProcessor::jdf[it.key()][it_in.key()]);
                for (auto it_in_in = it_in.value().begin(); it_in_in != it_in.value().end(); ++it_in_in) {
                    ASSERT_NO_THROW(tmp = InputProcessor::jdf[it.key()][it_in.key()][it_in_in.key()]);
                    if (!tmp.is_array()) {
                        EXPECT_EQ(tmp.dump(), it_in_in.value().dump());
                    } else {
                        for (int i = 0; i < it_in_in.value().size(); i++) {
                            for (auto it_ext = it_in_in.value()[i].begin(); it_ext != it_in_in.value()[i].end(); ++it_ext) {
                                ASSERT_NO_THROW(tmp = InputProcessor::jdf[it.key()][it_in.key()][it_in_in.key()][i][it_ext.key()]);
                                EXPECT_EQ(tmp.dump(), it_ext.value().dump());
                            }
                        }
                    }
                }
            }
        }

        json::parse(InputProcessor::jdf.dump(2), EnergyPlusFixture::call_back);
        EXPECT_EQ(InputProcessor::state.errors.size() + InputProcessor::state.warnings.size(), 0);
    }


	TEST_F(InputProcessorFixture, validate_two_extensible_objects_and_one_non_extensible_object) {
		std::string const idf(delimited_string(
				{
						"BuildingSurface:Detailed,",
						"Zn009:Flr001,            !- Name",
						"    Floor,                   !- Surface Type",
						"    FLOOR38,                 !- Construction Name",
						"    SCWINDOW,                !- Zone Name",
						"    Surface,                 !- Outside Boundary Condition",
						"    Zn009:Flr001,            !- Outside Boundary Condition Object",
						"    NoSun,                   !- Sun Exposure",
						"    NoWind,                  !- Wind Exposure",
						"    1.000000,                !- View Factor to Ground",
						"    4,                       !- Number of Vertices",
						"    10.00000,0.000000,0,  !- X,Y,Z ==> Vertex 1 {m}",
						"    0.000000,0.000000,0,  !- X,Y,Z ==> Vertex 2 {m}",
						"    0.000000,10.00000,0,  !- X,Y,Z ==> Vertex 3 {m}",
						"    10.00000,10.00000,0;  !- X,Y,Z ==> Vertex 4 {m}",
						"",
						"BuildingSurface:Detailed,",
						"Building Surface Name,            !- Name",
						"    Floor,                   !- Surface Type",
						"    FLOOR38,                 !- Construction Name",
						"    SCWINDOW,                !- Zone Name",
						"    Surface,                 !- Outside Boundary Condition",
						"    Zn009:Flr001,            !- Outside Boundary Condition Object",
						"    NoSun,                   !- Sun Exposure",
						"    NoWind,                  !- Wind Exposure",
						"    1.000000,                !- View Factor to Ground",
						"    4,                       !- Number of Vertices",
						"    10.00000,0.000000,0,  !- X,Y,Z ==> Vertex 1 {m}",
						"    0.000000,0.000000,0,  !- X,Y,Z ==> Vertex 2 {m}",
						"    0.000000,10.00000,0,  !- X,Y,Z ==> Vertex 3 {m}",
						"    10.00000,10.00000,0;  !- X,Y,Z ==> Vertex 4 {m}",
						"",
						"  Building,",
						"    Ref Bldg Medium Office New2004_v1.3_5.0,  !- Name",
						"    0.0000,                  !- North Axis {deg}",
						"    City,                    !- Terrain",
						"    0.0400,                  !- Loads Convergence Tolerance Value",
						"    0.2000,                  !- Temperature Convergence Tolerance Value {deltaC}",
						"    FullInteriorAndExterior, !- Solar Distribution",
						"    25,                      !- Maximum Number of Warmup Days",
						"    6;",
				}));

		ASSERT_TRUE( process_idf( idf ) );
		EXPECT_EQ(InputProcessor::state.errors.size() + InputProcessor::state.warnings.size(), 0);
	}


	TEST_F( InputProcessorFixture, parse_idf ) {
		std::string const idf( delimited_string( {
														 "  Building,",
														 "    Ref Bldg Medium Office New2004_v1.3_5.0,  !- Name",
														 "    0.0000,                  !- North Axis {deg}",
														 "    City,                    !- Terrain",
														 "    0.0400,                  !- Loads Convergence Tolerance Value",
														 "    0.2000,                  !- Temperature Convergence Tolerance Value {deltaC}",
														 "    FullInteriorAndExterior, !- Solar Distribution",
														 "    25,                      !- Maximum Number of Warmup Days",
														 "    6;",
												 } ) );

		json expected = {
				{
						"Building",
						{
								{
										"Ref Bldg Medium Office New2004_v1.3_5.0",
										{
												{"north_axis", 0.0000},
												{"terrain", "City"},
												{"loads_convergence_tolerance_value", 0.0400},
												{"temperature_convergence_tolerance_value", 0.2000},
												{"solar_distribution", "FullInteriorAndExterior"},
												{"maximum_number_of_warmup_days", 25},
												{"minimum_number_of_warmup_days", 6}
										},
								}
						}
				}
		};

		ASSERT_TRUE( process_idf( idf ) );

		json tmp;
		for (auto it = expected.begin(); it != expected.end(); ++it) {
			ASSERT_NO_THROW(tmp = InputProcessor::jdf[it.key()]);
			for (auto it_in = it.value().begin(); it_in != it.value().end(); ++it_in) {
				ASSERT_NO_THROW(tmp = InputProcessor::jdf[it.key()][it_in.key()]);
				for (auto it_in_in = it_in.value().begin(); it_in_in != it_in.value().end(); ++it_in_in) {
					ASSERT_NO_THROW(tmp = InputProcessor::jdf[it.key()][it_in.key()][it_in_in.key()]);
					EXPECT_EQ(tmp.dump(), it_in_in.value().dump());
				}
			}
		}
	}


	TEST_F( InputProcessorFixture, parse_idf_two_objects ) {
		std::string const idf( delimited_string( {
																 "  Building,",
																 "    Ref Bldg Medium Office New2004_v1.3_5.0,  !- Name",
																 "    0.0000,                  !- North Axis {deg}",
																 "    City,                    !- Terrain",
																 "    0.0400,                  !- Loads Convergence Tolerance Value",
																 "    0.2000,                  !- Temperature Convergence Tolerance Value {deltaC}",
																 "    FullInteriorAndExterior, !- Solar Distribution",
																 "    20,                      !- Maximum Number of Warmup Days",
																 "    6;",
																 "",
																 "  Building,",
																 "    Random Building Name 3,  !- Name",
																 "    0.0000,                  !- North Axis {deg}",
																 "    City,                    !- Terrain",
																 "    0.0400,                  !- Loads Convergence Tolerance Value",
																 "    0.2000,                  !- Temperature Convergence Tolerance Value {deltaC}",
																 "    FullInteriorAndExterior, !- Solar Distribution",
																 "    20,                      !- Maximum Number of Warmup Days",
																 "    6;",
														 } ) );

		json expected = {
				{
						"Building",
						{
								{
										"Ref Bldg Medium Office New2004_v1.3_5.0",
										{
												{"north_axis", 0.0000},
												{"terrain", "City"},
												{"loads_convergence_tolerance_value", 0.0400},
												{"temperature_convergence_tolerance_value", 0.2000},
												{"solar_distribution", "FullInteriorAndExterior"},
												{"maximum_number_of_warmup_days", 20},
												{"minimum_number_of_warmup_days", 6}
										},
								},
								{
										"Random Building Name 3",
										{
												{"north_axis", 0.0000},
												{"terrain", "City"},
												{"loads_convergence_tolerance_value", 0.0400},
												{"temperature_convergence_tolerance_value", 0.2000},
												{"solar_distribution", "FullInteriorAndExterior"},
												{"maximum_number_of_warmup_days", 20},
												{"minimum_number_of_warmup_days", 6}
										}
								}
						}
				}
		};

		ASSERT_TRUE( process_idf( idf ) );

		json tmp;
		for (auto it = expected.begin(); it != expected.end(); ++it) {
			ASSERT_NO_THROW(tmp = InputProcessor::jdf[it.key()]);
			for (auto it_in = it.value().begin(); it_in != it.value().end(); ++it_in) {
				ASSERT_NO_THROW(tmp = InputProcessor::jdf[it.key()][it_in.key()]);
				for (auto it_in_in = it_in.value().begin(); it_in_in != it_in.value().end(); ++it_in_in) {
					ASSERT_NO_THROW(tmp = InputProcessor::jdf[it.key()][it_in.key()][it_in_in.key()]);
					EXPECT_EQ(tmp.dump(), it_in_in.value().dump());
				}
			}
		}
	}


	TEST_F(InputProcessorFixture, parse_idf_extensibles) {
		std::string const idf(delimited_string(
				{
						"BuildingSurface:Detailed,",
						"Zn009:Flr001,            !- Name",
						"    Floor,                   !- Surface Type",
						"    FLOOR38,                 !- Construction Name",
						"    SCWINDOW,                !- Zone Name",
						"    Surface,                 !- Outside Boundary Condition",
						"    Zn009:Flr001,            !- Outside Boundary Condition Object",
						"    NoSun,                   !- Sun Exposure",
						"    NoWind,                  !- Wind Exposure",
						"    1.000000,                !- View Factor to Ground",
						"    4,                       !- Number of Vertices",
						"    10.00000,0.000000,0,  !- X,Y,Z ==> Vertex 1 {m}",
						"    0.000000,0.000000,0,  !- X,Y,Z ==> Vertex 2 {m}",
						"    0.000000,10.00000,0,  !- X,Y,Z ==> Vertex 3 {m}",
						"    10.00000,10.00000,0;  !- X,Y,Z ==> Vertex 4 {m}"
				}));

		json expected =
				{
						{"BuildingSurface:Detailed",
								{
										{"Zn009:Flr001",
												{
														{"surface_type", "Floor"},
														{"construction_name", "FLOOR38"},
														{"zone_name", "SCWINDOW"},
														{"outside_boundary_condition", "Surface"},
														{"outside_boundary_condition_object", "Zn009:Flr001"},
														{"sun_exposure", "NoSun"},
														{"wind_exposure", "NoWind"},
														{"view_factor_to_ground", 1.000000},
														{"number_of_vertices", 4},
														{"extensions", {
																			   {
																					   {"vertex_x_coordinate", 10},
																					   {"vertex_y_coordinate", 0.0},
																					   {"vertex_z_coordinate", 0}
																			   },
																			   {
																					   {"vertex_x_coordinate", 0.0},
																					   {"vertex_y_coordinate", 0.0},
																					   {"vertex_z_coordinate", 0}
																			   },
																			   {
																					   {"vertex_x_coordinate", 0.0},
																					   {"vertex_y_coordinate", 10},
																					   {"vertex_z_coordinate", 0}
																			   },
																			   {
																					   {"vertex_x_coordinate", 10},
																					   {"vertex_y_coordinate", 10},
																					   {"vertex_z_coordinate", 0}
																			   }
																	   }
														}
												}
										}
								}
						}
				};

		ASSERT_TRUE( process_idf( idf ) );
		json tmp;
		for (auto it = expected.begin(); it != expected.end(); ++it) {
			ASSERT_NO_THROW(tmp = InputProcessor::jdf[it.key()]);
			for (auto it_in = it.value().begin(); it_in != it.value().end(); ++it_in) {
				ASSERT_NO_THROW(tmp = InputProcessor::jdf[it.key()][it_in.key()]);
				for (auto it_in_in = it_in.value().begin(); it_in_in != it_in.value().end(); ++it_in_in) {
					ASSERT_NO_THROW(tmp = InputProcessor::jdf[it.key()][it_in.key()][it_in_in.key()]);
					if (!tmp.is_array()) {
						EXPECT_EQ(tmp.dump(), it_in_in.value().dump());
					} else {
						for (int i = 0; i < it_in_in.value().size(); i++) {
							for (auto it_ext = it_in_in.value()[i].begin(); it_ext != it_in_in.value()[i].end(); ++it_ext) {
								ASSERT_NO_THROW(tmp = InputProcessor::jdf[it.key()][it_in.key()][it_in_in.key()][i][it_ext.key()]);
								EXPECT_EQ(tmp.dump(), it_ext.value().dump());
							}
						}
					}
				}
			}
		}
	}


	TEST_F(InputProcessorFixture, parse_idf_extensibles_two_objects) {
		std::string const idf(delimited_string(
				{
						"BuildingSurface:Detailed,",
						"Zn009:Flr001,            !- Name",
						"    Floor,                   !- Surface Type",
						"    FLOOR38,                 !- Construction Name",
						"    SCWINDOW,                !- Zone Name",
						"    Surface,                 !- Outside Boundary Condition",
						"    Zn009:Flr001,            !- Outside Boundary Condition Object",
						"    NoSun,                   !- Sun Exposure",
						"    NoWind,                  !- Wind Exposure",
						"    1.000000,                !- View Factor to Ground",
						"    4,                       !- Number of Vertices",
						"    10.00000,0.000000,0,  !- X,Y,Z ==> Vertex 1 {m}",
						"    0.000000,0.000000,0,  !- X,Y,Z ==> Vertex 2 {m}",
						"    0.000000,10.00000,0,  !- X,Y,Z ==> Vertex 3 {m}",
						"    10.00000,10.00000,0;  !- X,Y,Z ==> Vertex 4 {m}",
						"",
						"BuildingSurface:Detailed,",
						"Building Surface Name,            !- Name",
						"    Floor,                   !- Surface Type",
						"    FLOOR38,                 !- Construction Name",
						"    SCWINDOW,                !- Zone Name",
						"    Surface,                 !- Outside Boundary Condition",
						"    Zn009:Flr001,            !- Outside Boundary Condition Object",
						"    NoSun,                   !- Sun Exposure",
						"    NoWind,                  !- Wind Exposure",
						"    1.000000,                !- View Factor to Ground",
						"    4,                       !- Number of Vertices",
						"    10.00000,0.000000,0,  !- X,Y,Z ==> Vertex 1 {m}",
						"    0.000000,0.000000,0,  !- X,Y,Z ==> Vertex 2 {m}",
						"    0.000000,10.00000,0,  !- X,Y,Z ==> Vertex 3 {m}",
						"    10.00000,10.00000,0;  !- X,Y,Z ==> Vertex 4 {m}"
				}));

		json expected =
				{
						{"BuildingSurface:Detailed",
								{
										{"Zn009:Flr001",
												{
														{"surface_type", "Floor"},
														{"construction_name", "FLOOR38"},
														{"zone_name", "SCWINDOW"},
														{"outside_boundary_condition", "Surface"},
														{"outside_boundary_condition_object", "Zn009:Flr001"},
														{"sun_exposure", "NoSun"},
														{"wind_exposure", "NoWind"},
														{"view_factor_to_ground", 1.000000},
														{"number_of_vertices", 4},
														{"extensions", {
																			   {
																					   {"vertex_x_coordinate", 10},
																					   {"vertex_y_coordinate", 0.0},
																					   {"vertex_z_coordinate", 0}
																			   },
																			   {
																					   {"vertex_x_coordinate", 0.0},
																					   {"vertex_y_coordinate", 0.0},
																					   {"vertex_z_coordinate", 0}
																			   },
																			   {
																					   {"vertex_x_coordinate", 0.0},
																					   {"vertex_y_coordinate", 10},
																					   {"vertex_z_coordinate", 0}
																			   },
																			   {
																					   {"vertex_x_coordinate", 10},
																					   {"vertex_y_coordinate", 10},
																					   {"vertex_z_coordinate", 0}
																			   }
																	   }
														}
												}
										},
										{"Building Surface Name",
												{
														{"surface_type", "Floor"},
														{"construction_name", "FLOOR38"},
														{"zone_name", "SCWINDOW"},
														{"outside_boundary_condition", "Surface"},
														{"outside_boundary_condition_object", "Zn009:Flr001"},
														{"sun_exposure", "NoSun"},
														{"wind_exposure", "NoWind"},
														{"view_factor_to_ground", 1.000000},
														{"number_of_vertices", 4},
														{"extensions", {
																			   {
																					   {"vertex_x_coordinate", 10},
																					   {"vertex_y_coordinate", 0.0},
																					   {"vertex_z_coordinate", 0}
																			   },
																			   {
																					   {"vertex_x_coordinate", 0.0},
																					   {"vertex_y_coordinate", 0.0},
																					   {"vertex_z_coordinate", 0}
																			   },
																			   {
																					   {"vertex_x_coordinate", 0.0},
																					   {"vertex_y_coordinate", 10},
																					   {"vertex_z_coordinate", 0}
																			   },
																			   {
																					   {"vertex_x_coordinate", 10},
																					   {"vertex_y_coordinate", 10},
																					   {"vertex_z_coordinate", 0}
																			   }
																	   }
														}
												}
										}
								}
						}
				};

		ASSERT_TRUE( process_idf( idf ) );
		json tmp;
		for (auto it = expected.begin(); it != expected.end(); ++it) {
			ASSERT_NO_THROW(tmp = InputProcessor::jdf[it.key()]);
			for (auto it_in = it.value().begin(); it_in != it.value().end(); ++it_in) {
				ASSERT_NO_THROW(tmp = InputProcessor::jdf[it.key()][it_in.key()]);
				for (auto it_in_in = it_in.value().begin(); it_in_in != it_in.value().end(); ++it_in_in) {
					ASSERT_NO_THROW(tmp = InputProcessor::jdf[it.key()][it_in.key()][it_in_in.key()]);
					if (!tmp.is_array()) {
						EXPECT_EQ(tmp.dump(), it_in_in.value().dump());
					} else {
						for (int i = 0; i < it_in_in.value().size(); i++) {
							for (auto it_ext = it_in_in.value()[i].begin(); it_ext != it_in_in.value()[i].end(); ++it_ext) {
								ASSERT_NO_THROW(tmp = InputProcessor::jdf[it.key()][it_in.key()][it_in_in.key()][i][it_ext.key()]);
								EXPECT_EQ(tmp.dump(), it_ext.value().dump());
							}
						}
					}
				}
			}
		}
	}


	TEST_F(InputProcessorFixture, validate_jdf_parametric_template) {
		std::string const idf(delimited_string(
				{
						"Parametric:Logic,",
						"Main,                    !- Name",
						"PARAMETER $bldgArea,     !- Parametric Logic Line 1",
						"PARAMETER $depth,        !- Parametric Logic Line 2",
						"PARAMETER $width,        !- Parametric Logic Line 3",
						"PARAMETER $height,       !- Parametric Logic Line 4",
						"$bldgArea = 300.0,       !- Parametric Logic Line 5",
						"$depth = SQRT($bldgArea / $aspectRatio),  !- Parametric Logic Line 6",
						"$width = $depth * $aspectRatio,  !- Parametric Logic Line 7",
						"$height = 4.0;           !- Parametric Logic Line 8"
						"",
						"HVACTemplate:Thermostat,",
						"All Zones,               !- Name",
						"Htg-SetP-Sch,            !- Heating Setpoint Schedule Name",
						",                        !- Constant Heating Setpoint {C}",
						"Clg-SetP-Sch,            !- Cooling Setpoint Schedule Name",
						";                        !- Constant Cooling Setpoint {C}"
				}));
		ASSERT_TRUE( process_idf( idf ) );
		json::parse(InputProcessor::jdf.dump(2), EnergyPlusFixture::call_back);
		EXPECT_EQ(InputProcessor::state.errors.size() + InputProcessor::state.warnings.size(), 4);
		if (InputProcessor::state.errors.size() >= 4) {
			EXPECT_NE(InputProcessor::state.errors[0].find("You must run the ExpandObjects program for \"HVACTemplate:Thermostat\" at line"), std::string::npos);
			EXPECT_NE(InputProcessor::state.errors[1].find("In object \"HVACTemplate:Thermostat\""), std::string::npos);
			EXPECT_NE(InputProcessor::state.errors[1].find("type needs to be string"), std::string::npos);
			EXPECT_NE(InputProcessor::state.errors[2].find("In object \"HVACTemplate:Thermostat\""), std::string::npos);
			EXPECT_NE(InputProcessor::state.errors[2].find("type needs to be string"), std::string::npos);
			EXPECT_NE(InputProcessor::state.errors[3].find("You must run Parametric Preprocesor for \"Parametric:Logic\" at line"), std::string::npos);
		}
	}


	TEST_F(InputProcessorFixture, non_existent_keys) {
		json root =
				{
						{"BuildingSurface:Detailed",
								{
										{"Zn009:Flr001",
												{
														{"surface_type", "Floor"},
														{"construction_name", "FLOOR38"},
														{"zone_name", "SCWINDOW"},
														{"outside_boundary_condition", "Surface"},
														{"outside_boundary_condition_object", "Zn009:Flr001"},
														{"non_existent_field_1", "NoSun"},
														{"wind_exposure", "NoWind"},
														{"non_existent_field_2", 1.000000},
														{"number_of_vertices", 4},
														{"extensions", {
																			   {
																					   {"vertex_x_coordinate", 10},
																					   {"vertex_y_coordinate", 10},
																					   {"vertex_z_coordinate", 0}
																			   }
																	   }
														}
												}
										}
								}
						}
				};

		json::parse(root.dump(2), EnergyPlusFixture::call_back);
		EXPECT_EQ(InputProcessor::state.errors.size() + InputProcessor::state.warnings.size(), 2);
		if (InputProcessor::state.errors.size() >= 2) {
			EXPECT_NE(InputProcessor::state.errors[0].find("Key \"non_existent_field_1\" in object \"BuildingSurface:Detailed\" at line"), std::string::npos);
			EXPECT_NE(InputProcessor::state.errors[1].find("Key \"non_existent_field_2\" in object \"BuildingSurface:Detailed\" at line"), std::string::npos);
		}
	}

	TEST_F(InputProcessorFixture, required_fields_required_extensibles_and_missing_enum) {
		json root =
				{
						{"BuildingSurface:Detailed",
								{
										{"Zn009:Flr001",
												{
														{"surface_type", "value that doesn't exist in the enum"},
														{"zone_name", "SCWINDOW"},
														{"outside_boundary_condition", "Surface"},
														{"outside_boundary_condition_object", "Zn009:Flr001"},
														{"sun_exposure", "NoSun"},
														{"wind_exposure", "NoWind"},
														{"view_factor_to_ground", 1.000000},
														{"number_of_vertices", 4},
														{"extensions", {
																			   {
																					   {"vertex_z_coordinate", 0}
																			   },
																			   {
																					   {"vertex_x_coordinate", 10},
																					   {"vertex_y_coordinate", 10},
																					   {"vertex_z_coordinate", 0}
																			   }
																	   }
														}
												}
										}
								}
						}
				};

		json::parse(root.dump(2), EnergyPlusFixture::call_back);
		EXPECT_EQ(InputProcessor::state.errors.size() + InputProcessor::state.warnings.size(), 4);
		if (InputProcessor::state.errors.size() >= 4) {
			EXPECT_NE(InputProcessor::state.errors[0].find("Required extensible field \"vertex_y_coordinate\" in object \"BuildingSurface:Detailed\" ending at line"), std::string::npos);
			EXPECT_NE(InputProcessor::state.errors[1].find("Required extensible field \"vertex_x_coordinate\" in object \"BuildingSurface:Detailed\" ending at line"), std::string::npos);
			EXPECT_NE(InputProcessor::state.errors[2].find("In object \"BuildingSurface:Detailed\" at line"), std::string::npos);
			EXPECT_NE(InputProcessor::state.errors[2].find("value that doesn't exist in the enum\" was not found in the enum"), std::string::npos);
			EXPECT_NE(InputProcessor::state.errors[3].find("Required field \"construction_name\" in object \"BuildingSurface:Detailed\" ending at line"), std::string::npos);
		}
	}


	TEST_F(InputProcessorFixture, min_and_max_validation) {
		json root =
				{
						{"BuildingSurface:Detailed",
								{
										{"Zn009:Flr001",
												{
														{"surface_type", "Floor"},
														{"construction_name", "super_official_construction_name"},
														{"zone_name", "SCWINDOW"},
														{"outside_boundary_condition", "Surface"},
														{"outside_boundary_condition_object", "Zn009:Flr001"},
														{"sun_exposure", "NoSun"},
														{"wind_exposure", "NoWind"},
														{"view_factor_to_ground", -987.654321},
														{"number_of_vertices", -98765.4321},
														{"extensions", {
																			   {
																					   {"vertex_x_coordinate", "definitely not a number"},
																					   {"vertex_y_coordinate", 10},
																					   {"vertex_z_coordinate", 0}
																			   }
																	   }
														}
												}
										}
								}
						},
						{"Building",
								{
										{
										 "Ref Bldg Medium Office New2004_v1.3_5.0",
												{
														{"north_axis", 0.0000},
														{"terrain", "City"},
														{"loads_convergence_tolerance_value", 0.0},
														{"temperature_convergence_tolerance_value", 0.2000},
														{"solar_distribution", "FullInteriorAndExterior"},
														{"maximum_number_of_warmup_days", 20},
														{"minimum_number_of_warmup_days", 0}
												},
										},
								}
						}
				};
		json::parse(root.dump(2), EnergyPlusFixture::call_back);
		EXPECT_EQ(InputProcessor::state.errors.size() + InputProcessor::state.warnings.size(), 5);
		if (InputProcessor::state.errors.size() >= 5) {
			EXPECT_NE(InputProcessor::state.errors[0].find("Value \"0.000000\" parsed at line"), std::string::npos);
			EXPECT_NE(InputProcessor::state.errors[0].find("is less than or equal to the exclusive minimum"), std::string::npos);
			EXPECT_NE(InputProcessor::state.errors[1].find("Value \"0.000000\" parsed at line"), std::string::npos);
			EXPECT_NE(InputProcessor::state.errors[1].find("is less than or equal to the exclusive minimum"), std::string::npos);
			EXPECT_NE(InputProcessor::state.errors[2].find("In object \"BuildingSurface:Detailed\", at line"), std::string::npos);
			EXPECT_NE(InputProcessor::state.errors[2].find("type needs to be string"), std::string::npos);
			EXPECT_NE(InputProcessor::state.errors[3].find("Value \"-98765.432100\" parsed at line"), std::string::npos);
			EXPECT_NE(InputProcessor::state.errors[3].find("less than the minimum"), std::string::npos);
			EXPECT_NE(InputProcessor::state.errors[4].find("Value \"-987.654321\" parsed at line"), std::string::npos);
			EXPECT_NE(InputProcessor::state.errors[4].find("less than the minimum"), std::string::npos);
//			EXPECT_NE(InputProcessor::state.errors[5].find("Required object \"GlobalGeometryRules\" was not provided"), std::string::npos);
		}
	}


	TEST_F( InputProcessorFixture, eat_whitespace ) {
		size_t index = 0;
		InputProcessor::idf_parser.eat_whitespace( "    test", index );
		EXPECT_EQ( 4ul, index );

		index = 0;
		InputProcessor::idf_parser.eat_whitespace( "t   test", index );
		EXPECT_EQ( 0ul, index );
	}


	TEST_F( InputProcessorFixture, eat_comment ) {
		size_t index = 0;
		InputProcessor::idf_parser.eat_comment( "!- North Axis {deg}\n", index );
		EXPECT_EQ( 20ul, index );

		index = 0;
		InputProcessor::idf_parser.eat_comment( "                    !- Terrain\n", index );
		EXPECT_EQ( 31ul, index );

		index = 0;
		InputProcessor::idf_parser.eat_comment( "  !- Name\n    0.0000", index );
		EXPECT_EQ( 10ul, index );

		index = 0;
		InputProcessor::idf_parser.eat_comment( "  !- Name\n\r    0.0000", index );
		EXPECT_EQ( 10ul, index );
	}


	TEST_F( InputProcessorFixture, parse_string ) {
		size_t index = 0;
		bool success = true;
		std::string output_string;

		output_string = InputProcessor::idf_parser.parse_string( "test_string", index, success );
		EXPECT_EQ( "test_string", output_string );
		EXPECT_EQ( 11ul, index );
		EXPECT_TRUE( success );

		index = 0;
		success = true;
		output_string = InputProcessor::idf_parser.parse_string( "-1234.1234", index, success );
		EXPECT_EQ( "-1234.1234", output_string );
		EXPECT_EQ( 10ul, index );
		EXPECT_TRUE( success );

		index = 0;
		success = true;
		output_string = InputProcessor::idf_parser.parse_string( R"(\b\t/\\\";)", index, success );
		EXPECT_EQ( "\b\t/\\\"", output_string );
		EXPECT_EQ( 9ul, index );
		EXPECT_TRUE( success );

		index = 0;
		success = true;
		output_string = InputProcessor::idf_parser.parse_string( R"(test \n string)", index, success );
		EXPECT_EQ( "", output_string );
		EXPECT_EQ( 7ul, index );
		EXPECT_FALSE( success );

		index = 0;
		success = true;
		output_string = InputProcessor::idf_parser.parse_string( R"(! this is a comment \n)", index, success );
		EXPECT_EQ( "", output_string );
		EXPECT_EQ( 0ul, index );
		EXPECT_TRUE( success );
	}

	TEST_F (InputProcessorFixture, parse_value) {
		size_t index = 0;
		bool success = true;
		json rv;

		rv = InputProcessor::idf_parser.parse_value("11th of April,", index, success, InputProcessor::schema["properties"]);
		EXPECT_EQ(13ul, index);
		EXPECT_EQ("11th of April", rv.get<std::string>());

		index = 0;
		rv = InputProcessor::idf_parser.parse_value("11.201,", index, success, InputProcessor::schema["properties"]);
		EXPECT_EQ(6ul, index);
		EXPECT_EQ(11.201, rv.get<double>());

		index = 0;
		rv = InputProcessor::idf_parser.parse_value("11.201 of April,", index, success, InputProcessor::schema["properties"]);
		EXPECT_EQ(15ul, index);
		EXPECT_EQ("11.201 of April", rv.get<std::string>());

		index = 0;
		EXPECT_NO_THROW(rv = InputProcessor::idf_parser.parse_value("4Ee5,", index, success, InputProcessor::schema["properties"]));
		EXPECT_EQ(4ul, index);
		EXPECT_EQ("4Ee5", rv.get<std::string>());
	}


	TEST_F( InputProcessorFixture, parse_number) {
		size_t index = 0;
		bool success = true;
		json output;

		output = InputProcessor::idf_parser.parse_number("4.5,", index, success);
		EXPECT_EQ(4.5, output.get<double>());
		EXPECT_EQ(3ul, index);
		EXPECT_TRUE(success);

		index = 0;
		output = InputProcessor::idf_parser.parse_number("0.53;", index, success);
		EXPECT_EQ(0.53, output.get<double>());
		EXPECT_EQ(4ul, index);
		EXPECT_TRUE(success);

		index = 0;
		output = InputProcessor::idf_parser.parse_number("1.53  ;", index, success);
		EXPECT_EQ(1.53, output.get<double>());
		EXPECT_EQ(4ul, index);
		EXPECT_TRUE(success);

		index = 0;
		output = InputProcessor::idf_parser.parse_number(" 1.53  ;", index, success);
		EXPECT_EQ(1.53, output.get<double>());
		EXPECT_EQ(5ul, index);
		EXPECT_TRUE(success);

		index = 0;
		output = InputProcessor::idf_parser.parse_number("2.510035e5;", index, success);
		EXPECT_EQ(251003.5, output.get<double>());
		EXPECT_EQ(10ul, index);
		EXPECT_TRUE(success);

		index = 0;
		output = InputProcessor::idf_parser.parse_number("2.510035e-05;", index, success);
		EXPECT_EQ(0.00002510035, output.get<double>());
		EXPECT_EQ(12ul, index);
		EXPECT_TRUE(success);

		index = 0;
		output = InputProcessor::idf_parser.parse_number("1.0E-05;", index, success);
		EXPECT_EQ(0.000010, output.get<double>());
		EXPECT_EQ(7ul, index);
		EXPECT_TRUE(success);

		index = 0;
		output = InputProcessor::idf_parser.parse_number("11th of April,", index, success);
		EXPECT_TRUE(output.is_null());
		EXPECT_EQ(0ul, index);
		EXPECT_FALSE(success);

		index = 0;
		EXPECT_NO_THROW(output = InputProcessor::idf_parser.parse_number("-+4,", index, success));
		EXPECT_TRUE(output.is_null());
		EXPECT_EQ(0ul, index);
		EXPECT_FALSE(success);

		index = 0;
		EXPECT_NO_THROW(output = InputProcessor::idf_parser.parse_number("4..0,", index, success));
		EXPECT_TRUE(output.is_null());
		EXPECT_EQ(0ul, index);
		EXPECT_FALSE(success);

		index = 0;
		EXPECT_NO_THROW(output = InputProcessor::idf_parser.parse_number("++4,", index, success));
		EXPECT_TRUE(output.is_null());
		EXPECT_EQ(0ul, index);
		EXPECT_FALSE(success);

		index = 0;
		EXPECT_NO_THROW(output = InputProcessor::idf_parser.parse_number("--4,", index, success));
		EXPECT_TRUE(output.is_null());
		EXPECT_EQ(0ul, index);
		EXPECT_FALSE(success);

		index = 0;
		EXPECT_NO_THROW(output = InputProcessor::idf_parser.parse_number("4++,", index, success));
		EXPECT_TRUE(output.is_null());
		EXPECT_EQ(0ul, index);
		EXPECT_FALSE(success);

		index = 0;
		EXPECT_NO_THROW(output = InputProcessor::idf_parser.parse_number("4--,", index, success));
		EXPECT_TRUE(output.is_null());
		EXPECT_EQ(0ul, index);
		EXPECT_FALSE(success);

		index = 0;
		EXPECT_NO_THROW(output = InputProcessor::idf_parser.parse_number("4ee5,", index, success));
		EXPECT_TRUE(output.is_null());
		EXPECT_EQ(0ul, index);
		EXPECT_FALSE(success);

		index = 0;
		EXPECT_NO_THROW(output = InputProcessor::idf_parser.parse_number("4EE5,", index, success));
		EXPECT_TRUE(output.is_null());
		EXPECT_EQ(0ul, index);
		EXPECT_FALSE(success);

		index = 0;
		EXPECT_NO_THROW(output = InputProcessor::idf_parser.parse_number("4eE5,", index, success));
		EXPECT_TRUE(output.is_null());
		EXPECT_EQ(0ul, index);
		EXPECT_FALSE(success);

		index = 0;
		EXPECT_NO_THROW(output = InputProcessor::idf_parser.parse_number("4Ee5,", index, success));
		EXPECT_TRUE(output.is_null());
		EXPECT_EQ(0ul, index);
		EXPECT_FALSE(success);
	}


	TEST_F( InputProcessorFixture, look_ahead ) {
		std::string const test_input( "B , ! t ; `" );
		size_t index = 0;
		IdfParser::Token token = InputProcessor::idf_parser.look_ahead( test_input, index );
		EXPECT_EQ( 0ul, index );
		EXPECT_EQ( IdfParser::Token::STRING, token );
		index = 2;
		token = InputProcessor::idf_parser.look_ahead( test_input, index );
		EXPECT_EQ( 2ul, index );
		EXPECT_EQ( IdfParser::Token::COMMA, token );
		index = 3;
		token = InputProcessor::idf_parser.look_ahead( test_input, index );
		EXPECT_EQ( 3ul, index );
		EXPECT_EQ( IdfParser::Token::EXCLAMATION, token );
		index = 5;
		token = InputProcessor::idf_parser.look_ahead( test_input, index );
		EXPECT_EQ( 5ul, index );
		EXPECT_EQ( IdfParser::Token::STRING, token );
		index = 7;
		token = InputProcessor::idf_parser.look_ahead( test_input, index );
		EXPECT_EQ( 7ul, index );
		EXPECT_EQ( IdfParser::Token::SEMICOLON, token );
		index = 9;
		token = InputProcessor::idf_parser.look_ahead( test_input, index );
		EXPECT_EQ( 9ul, index );
		EXPECT_EQ( IdfParser::Token::NONE, token );
		index = test_input.size();
		token = InputProcessor::idf_parser.look_ahead( test_input, index );
		EXPECT_EQ( test_input.size(), index );
		EXPECT_EQ( IdfParser::Token::END, token );
	}

	TEST_F( InputProcessorFixture, next_token ) {
		size_t index = 0;

		std::string const test_input( "B , ! t ; `" );
		IdfParser::Token token = InputProcessor::idf_parser.next_token( test_input, index );
		EXPECT_EQ( 1ul, index );
		EXPECT_EQ( IdfParser::Token::STRING, token );
		token = InputProcessor::idf_parser.next_token( test_input, index );
		EXPECT_EQ( 3ul, index );
		EXPECT_EQ( IdfParser::Token::COMMA, token );
		token = InputProcessor::idf_parser.next_token( test_input, index );
		EXPECT_EQ( 5ul, index );
		EXPECT_EQ( IdfParser::Token::EXCLAMATION, token );
		token = InputProcessor::idf_parser.next_token( test_input, index );
		EXPECT_EQ( 7ul, index );
		EXPECT_EQ( IdfParser::Token::STRING, token );
		token = InputProcessor::idf_parser.next_token( test_input, index );
		EXPECT_EQ( 9ul, index );
		EXPECT_EQ( IdfParser::Token::SEMICOLON, token );
		token = InputProcessor::idf_parser.next_token( test_input, index );
		EXPECT_EQ( 10ul, index );
		EXPECT_EQ( IdfParser::Token::NONE, token );
		index = test_input.size();
		token = InputProcessor::idf_parser.next_token( test_input, index );
		EXPECT_EQ( test_input.size() , index );
		EXPECT_EQ( IdfParser::Token::END, token );
	}

	TEST_F( InputProcessorFixture, getObjectItem_json1 )
    {
        std::string const idf_objects = delimited_string({
                                                                 "Version,8.3;",
                                                                 "Output:SQLite,SimpleAndTabular;",
                                                         });

        ASSERT_TRUE( process_idf( idf_objects ) );
        std::string const CurrentModuleObject = "Output:SQLite";

        int NumSQLite = InputProcessor::GetNumObjectsFound( CurrentModuleObject );
        ASSERT_EQ( 1, NumSQLite );

        int TotalArgs = 0;
        int NumAlphas = 0;
        int NumNumbers = 0;

        InputProcessor::GetObjectDefMaxArgs( CurrentModuleObject, TotalArgs, NumAlphas, NumNumbers );

        int IOStatus = 0;
        Array1D_string Alphas( NumAlphas );
        Array1D< Real64 > Numbers( NumNumbers, 0.0 );
        Array1D_bool lNumericBlanks( NumNumbers, true );
        Array1D_bool lAlphaBlanks( NumAlphas, true );
        Array1D_string cAlphaFields( NumAlphas );
        Array1D_string cNumericFields( NumNumbers );

        InputProcessor::GetObjectItem( CurrentModuleObject, NumSQLite, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

        EXPECT_TRUE( compare_containers( std::vector< std::string >( { "SIMPLEANDTABULAR" } ), Alphas ) );
        EXPECT_TRUE( compare_containers( std::vector< std::string >( { "option_type" } ), cAlphaFields ) );
        EXPECT_TRUE( compare_containers( std::vector< std::string >( { } ), cNumericFields ) );
        EXPECT_TRUE( compare_containers( std::vector< bool >( { } ), lNumericBlanks ) );
        EXPECT_TRUE( compare_containers( std::vector< bool >( { false } ), lAlphaBlanks ) );
        EXPECT_TRUE( compare_containers( std::vector< Real64 >( { } ), Numbers ) );
        EXPECT_EQ( 1, NumAlphas );
        EXPECT_EQ( 0, NumNumbers );
        EXPECT_EQ( 1, IOStatus );

    }


    TEST_F( InputProcessorFixture, getObjectItem_json2 ) {
        std::string const idf_objects = delimited_string({
                                                                 "Version,8.3;",
                                                                 "Humidifier:Steam:Gas,",
                                                                 "  Main Gas Humidifier,     !- Name",
                                                                 "  ,                        !- Availability Schedule Name",
                                                                 "  autosize,                !- Rated Capacity {m3/s}",
                                                                 "  autosize,                !- Rated Gas Use Rate {W}",
                                                                 "  0.80,                    !- Thermal Efficiency {-} ",
                                                                 "  ThermalEfficiencyFPLR,   !- Thermal Efficiency Modifier Curve Name",
                                                                 "  0,                       !- Rated Fan Power {W}",
                                                                 "  0,                       !- Auxiliary Electric Power {W}",
                                                                 "  Mixed Air Node 1,        !- Air Inlet Node Name",
                                                                 "  Main Humidifier Outlet Node,  !- Air Outlet Node Name",
                                                                 "  ;                        !- Water Storage Tank Name",
                                                         });
		ASSERT_TRUE( process_idf ( idf_objects ) );
        std::string const CurrentModuleObject = "Humidifier:Steam:Gas";

        int NumGasSteamHums = InputProcessor::GetNumObjectsFound( CurrentModuleObject );
        ASSERT_EQ( 1, NumGasSteamHums );

        int TotalArgs = 0;
        int NumAlphas = 0;
        int NumNumbers = 0;

        InputProcessor::GetObjectDefMaxArgs( CurrentModuleObject, TotalArgs, NumAlphas, NumNumbers );
        int IOStatus = 0;
        Array1D_string Alphas( NumAlphas );
        Array1D< Real64 > Numbers( NumNumbers, 0.0 );
        Array1D_bool lNumericBlanks( NumNumbers, true );
        Array1D_bool lAlphaBlanks( NumAlphas, true );
        Array1D_string cAlphaFields( NumAlphas );
        Array1D_string cNumericFields( NumNumbers );
        InputProcessor::GetObjectItem( CurrentModuleObject, NumGasSteamHums, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

        EXPECT_TRUE( compare_containers( std::vector< std::string >( { "MAIN GAS HUMIDIFIER", "", "THERMALEFFICIENCYFPLR", "MIXED AIR NODE 1", "MAIN HUMIDIFIER OUTLET NODE", "", "" } ), Alphas ) );
        EXPECT_TRUE( compare_containers( std::vector< std::string >( { "name", "availability_schedule_name", "thermal_efficiency_modifier_curve_name", "air_inlet_node_name", "air_outlet_node_name", "water_storage_tank_name", "inlet_water_temperature_option" } ), cAlphaFields ) );
        EXPECT_TRUE( compare_containers( std::vector< std::string >( { "rated_capacity", "rated_gas_use_rate", "thermal_efficiency", "rated_fan_power", "auxiliary_electric_power" } ), cNumericFields ) );
        EXPECT_TRUE( compare_containers( std::vector< bool >( { false, false, false, false, false } ), lNumericBlanks ) );
        EXPECT_TRUE( compare_containers( std::vector< bool >( { false, true, false, false, false, true, true } ), lAlphaBlanks ) );
        EXPECT_TRUE( compare_containers( std::vector< Real64 >( { -99999, -99999, 0.80, 0.0, 0.0 } ), Numbers ) );
//        EXPECT_EQ( 6, NumAlphas ); // TODO: Should be 6, why is it 7? Might be due to name field
		EXPECT_EQ( 6, NumAlphas );
        EXPECT_EQ( 5, NumNumbers );
        EXPECT_EQ( 1, IOStatus );
    }

    TEST_F( InputProcessorFixture, getObjectItem_json3 ) {
        std::string const idf_objects = delimited_string({
                                                                 "Version,8.3;",
                                                                 "  BuildingSurface:Detailed,",
                                                                 "    Zn001:Wall001,           !- Name",
                                                                 "    Wall,                    !- Surface Type",
                                                                 "    R13WALL,                 !- Construction Name",
                                                                 "    Main Zone,               !- Zone Name",
                                                                 "    Outdoors,                !- Outside Boundary Condition",
                                                                 "    ,                        !- Outside Boundary Condition Object",
                                                                 "    SunExposed,              !- Sun Exposure",
                                                                 "    WindExposed,             !- Wind Exposure",
                                                                 "    0.5000000,               !- View Factor to Ground",
                                                                 "    4,                       !- Number of Vertices",
                                                                 "    0,0,4.572000,  !- X,Y,Z ==> Vertex 1 {m}",
                                                                 "    0,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
                                                                 "    15.24000,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
                                                                 "    15.24000,0,4.572000;  !- X,Y,Z ==> Vertex 4 {m}",
                                                         });

		ASSERT_TRUE( process_idf( idf_objects ) );
        std::string const CurrentModuleObject = "BuildingSurface:Detailed";

        int numBuildingSurfaceDetailed = InputProcessor::GetNumObjectsFound(CurrentModuleObject);
        ASSERT_EQ(1, numBuildingSurfaceDetailed);

        int TotalArgs = 0;
        int NumAlphas = 0;
        int NumNumbers = 0;

        InputProcessor::GetObjectDefMaxArgs(CurrentModuleObject, TotalArgs, NumAlphas, NumNumbers);
        int IOStatus = 0;
        Array1D_string Alphas(NumAlphas);
        Array1D<Real64> Numbers(NumNumbers, 0.0);
        Array1D_bool lNumericBlanks(NumNumbers, true);
        Array1D_bool lAlphaBlanks(NumAlphas, true);
        Array1D_string cAlphaFields(NumAlphas);
        Array1D_string cNumericFields(NumNumbers);
        InputProcessor::GetObjectItem(CurrentModuleObject, numBuildingSurfaceDetailed, Alphas, NumAlphas, Numbers, NumNumbers,
                                      IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields);

        EXPECT_TRUE(compare_containers(std::vector<std::string>({"ZN001:WALL001", "WALL", "R13WALL", "MAIN ZONE", "OUTDOORS", "", "SUNEXPOSED", "WINDEXPOSED" }), Alphas));
        EXPECT_TRUE(compare_containers(std::vector<bool>({false, false, false, false, false, false, false, false, false, false, false, false, false, false }), lNumericBlanks));
        EXPECT_TRUE(compare_containers(std::vector<bool>({false, false, false, false, false, true, false, false }), lAlphaBlanks));
        EXPECT_TRUE(compare_containers(std::vector<Real64>({0.5, 4, 0, 0, 4.572, 0, 0, 0, 15.24, 0, 0, 15.24, 0, 4.572 }), Numbers));
        EXPECT_EQ(8, NumAlphas);
        EXPECT_EQ(14, NumNumbers);
        EXPECT_EQ(1, IOStatus);
    }

	TEST_F( InputProcessorFixture, getObjectItem_empty_fields_with_no_defaults )
	{
		std::string const idf_objects = delimited_string({
																 "Version,8.3;",
																 " Curve:Biquadratic,",
																 "  HPACCOOLEIRFT Speed, !- Name",
																 "  0.632475E+00, !- Coefficient1 Constant",
																 "  -0.121321E-01, !- Coefficient2 x",
																 "  0.507773E-03, !- Coefficient3 x**2",
																 "  0.155377E-01, !- Coefficient4 y",
																 "  0.272840E-03, !- Coefficient5 y**2",
																 "  -0.679201E-03, !- Coefficient6 x*y",
																 "  12.77778, !- Minimum Value of x",
																 "  23.88889, !- Maximum Value of x",
																 "  23.88889, !- Minimum Value of y",
																 "  46.11111, !- Maximum Value of y",
																 "  , !- Minimum Curve Output",
																 "  , !- Maximum Curve Output",
																 "  , !- Input Unit Type for X",
																 "  Temperature, !- Input Unit Type for Y",
																 "  Dimensionless;           !- Output Unit Type",
														 });
		// expect 0's to be inserted in for min Curve Output and Max Curve Output and expect true to be their respective NumBlanks value, they are missing fields and have no default
		// expect Dimensionless to be inserted for Input Unit Type for X, blank field with a default. Expect true for it's alphaBlank value

		ASSERT_TRUE( process_idf( idf_objects ) );

		std::string const CurrentModuleObject = "Curve:Biquadratic";

		int num_curve_biquadratic_objects = InputProcessor::GetNumObjectsFound( CurrentModuleObject );
		ASSERT_EQ( 1, num_curve_biquadratic_objects );

		int TotalArgs = 0;
		int NumAlphas = 0;
		int NumNumbers = 0;

        InputProcessor::GetObjectDefMaxArgs( CurrentModuleObject, TotalArgs, NumAlphas, NumNumbers );

		int IOStatus = 0;
		Array1D_string Alphas( NumAlphas );
		Array1D< Real64 > Numbers( NumNumbers, 0.0 );
		Array1D_bool lNumericBlanks( NumNumbers, true );
		Array1D_bool lAlphaBlanks( NumAlphas, true );
		Array1D_string cAlphaFields( NumAlphas );
		Array1D_string cNumericFields( NumNumbers );

        InputProcessor::GetObjectItem( CurrentModuleObject, num_curve_biquadratic_objects, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );


		EXPECT_EQ( 4, NumAlphas );
		EXPECT_TRUE( compare_containers( std::vector< std::string >( { "HPACCOOLEIRFT SPEED", "DIMENSIONLESS", "TEMPERATURE", "DIMENSIONLESS", } ), Alphas ) );
		EXPECT_TRUE( compare_containers( std::vector< std::string >( { "name", "input_unit_type_for_x", "input_unit_type_for_y", "output_unit_type", } ), cAlphaFields ) );
		EXPECT_TRUE( compare_containers( std::vector< bool >( { false, true, false, false } ), lAlphaBlanks ) );

		EXPECT_EQ( 12, NumNumbers );
		EXPECT_TRUE( compare_containers( std::vector< std::string >( { "coefficient1_constant", "coefficient2_x", "coefficient3_x_2", "coefficient4_y", "coefficient5_y_2", "coefficient6_x_y",
																	   "minimum_value_of_x", "maximum_value_of_x", "minimum_value_of_y", "maximum_value_of_y", "minimum_curve_output", "maximum_curve_output" } ), cNumericFields ) );
		EXPECT_TRUE( compare_containers( std::vector< Real64 >( { 0.632475E+00, -0.121321E-01 , 0.507773E-03, 0.155377E-01 , 0.272840E-03,
																  -0.679201E-03, 12.77778, 23.88889, 23.88889, 46.11111, 0, 0, } ), Numbers ) );
		EXPECT_TRUE( compare_containers( std::vector< bool >( { false, false, false, false, false, false, false, false, false, false, true, true } ), lNumericBlanks ) );

		EXPECT_EQ( 1, IOStatus );

	}

	TEST_F( InputProcessorFixture, getObjectItem_truncated_obj_pulled_up_semicolon )
	{
		std::string const idf_objects = delimited_string({
																 "Version,8.3;",
																 " Curve:Biquadratic,",
																 "  HPACCOOLEIRFT Speed, !- Name",
																 "  0.632475E+00, !- Coefficient1 Constant",
																 "  -0.121321E-01, !- Coefficient2 x",
																 "  0.507773E-03, !- Coefficient3 x**2",
																 "  0.155377E-01, !- Coefficient4 y",
																 "  0.272840E-03, !- Coefficient5 y**2",
																 "  -0.679201E-03, !- Coefficient6 x*y",
																 "  12.77778, !- Minimum Value of x",
																 "  23.88889, !- Maximum Value of x",
																 "  23.88889, !- Minimum Value of y",
																 "  46.11111; !- Maximum Value of y",
														 });
		// expect 0's to be inserted in for min Curve Output and Max Curve Output and expect true to be their respective NumBlanks value, they are missing fields and have no default
		// expect "" to be inserted for the missing alpha fields due to the truncation, blank field with a default. Expect true for it's alphaBlank value

		ASSERT_TRUE( process_idf( idf_objects ) );

		std::string const CurrentModuleObject = "Curve:Biquadratic";

		int num_curve_biquadratic_objects = InputProcessor::GetNumObjectsFound( CurrentModuleObject );
		ASSERT_EQ( 1, num_curve_biquadratic_objects );

		int TotalArgs = 0;
		int NumAlphas = 0;
		int NumNumbers = 0;

        InputProcessor::GetObjectDefMaxArgs( CurrentModuleObject, TotalArgs, NumAlphas, NumNumbers );

		int IOStatus = 0;
		Array1D_string Alphas( NumAlphas );
		Array1D< Real64 > Numbers( NumNumbers, 0.0 );
		Array1D_bool lNumericBlanks( NumNumbers, true );
		Array1D_bool lAlphaBlanks( NumAlphas, true );
		Array1D_string cAlphaFields( NumAlphas );
		Array1D_string cNumericFields( NumNumbers );

        InputProcessor::GetObjectItem( CurrentModuleObject, num_curve_biquadratic_objects, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );


		EXPECT_EQ( 1, NumAlphas );
		EXPECT_TRUE( compare_containers( std::vector< std::string >( { "HPACCOOLEIRFT SPEED", "", "", "" } ), Alphas ) );
		EXPECT_TRUE( compare_containers( std::vector< std::string >( { "name", "input_unit_type_for_x", "input_unit_type_for_y", "output_unit_type", } ), cAlphaFields ) );
		EXPECT_TRUE( compare_containers( std::vector< bool >( { false, true, true, true } ), lAlphaBlanks ) );

		EXPECT_EQ( 10, NumNumbers );
		EXPECT_TRUE( compare_containers( std::vector< std::string >( { "coefficient1_constant", "coefficient2_x", "coefficient3_x_2", "coefficient4_y", "coefficient5_y_2", "coefficient6_x_y",
																	   "minimum_value_of_x", "maximum_value_of_x", "minimum_value_of_y", "maximum_value_of_y", "minimum_curve_output", "maximum_curve_output" } ), cNumericFields ) );
		EXPECT_TRUE( compare_containers( std::vector< Real64 >( { 0.632475E+00, -0.121321E-01 , 0.507773E-03, 0.155377E-01 , 0.272840E-03,
																  -0.679201E-03, 12.77778, 23.88889, 23.88889, 46.11111, 0, 0 } ), Numbers ) );
		EXPECT_TRUE( compare_containers( std::vector< bool >( { false, false, false, false, false, false, false, false, false, false, true, true } ), lNumericBlanks ) );

		EXPECT_EQ( 1, IOStatus );

	}

	TEST_F( InputProcessorFixture, getObjectItem_missing_numerics_with_defaults_and_autosize )
	{
		std::string const idf_objects = delimited_string({
																 "Version,8.3;",
																 "Humidifier:Steam:Gas,",
																 "  Main Gas Humidifier,     !- Name",
																 "  ,                        !- Availability Schedule Name",
																 "  ,                !- Rated Capacity {m3/s}",
																 "  autosize,                !- Rated Gas Use Rate {W}",
																 "  ,                    !- Thermal Efficiency {-} ",
																 "  ThermalEfficiencyFPLR,   !- Thermal Efficiency Modifier Curve Name",
																 "  0,                       !- Rated Fan Power {W}",
																 "  ,                       !- Auxiliary Electric Power {W}",
																 "  Mixed Air Node 1,        !- Air Inlet Node Name",
																 "  Main Humidifier Outlet Node,  !- Air Outlet Node Name",
																 "  ,                        !- Water Storage Tank Name",
																 "  ;                        !- InletWaterTemperatureOption",
														 });

		// Expect the alpha field Inlet Water Temp to be filled in with it's default value
		// Expect Rated Capacity to be filled in with ZERO, not with the autosize value of -99999. Expect
		// Auxiliary Electric Power to be filled in with .80 (it's default value)

		ASSERT_TRUE( process_idf( idf_objects ) );

		std::string const CurrentModuleObject = "Humidifier:Steam:Gas";

		int NumGasSteamHums = InputProcessor::GetNumObjectsFound( CurrentModuleObject );
		ASSERT_EQ( 1, NumGasSteamHums );

		int TotalArgs = 0;
		int NumAlphas = 0;
		int NumNumbers = 0;

        InputProcessor::GetObjectDefMaxArgs( CurrentModuleObject, TotalArgs, NumAlphas, NumNumbers );

		int IOStatus = 0;
		Array1D_string Alphas( NumAlphas );
		Array1D< Real64 > Numbers( NumNumbers, 0.0 );
		Array1D_bool lNumericBlanks( NumNumbers, true );
		Array1D_bool lAlphaBlanks( NumAlphas, true );
		Array1D_string cAlphaFields( NumAlphas );
		Array1D_string cNumericFields( NumNumbers );

        InputProcessor::GetObjectItem( CurrentModuleObject, NumGasSteamHums, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

		EXPECT_EQ( 7, NumAlphas );
		EXPECT_TRUE( compare_containers( std::vector< std::string >( { "MAIN GAS HUMIDIFIER", "", "THERMALEFFICIENCYFPLR", "MIXED AIR NODE 1", "MAIN HUMIDIFIER OUTLET NODE", "", "FIXEDINLETWATERTEMPERATURE" } ), Alphas ) );
		EXPECT_TRUE( compare_containers( std::vector< std::string >( { "name", "availability_schedule_name", "thermal_efficiency_modifier_curve_name", "air_inlet_node_name", "air_outlet_node_name", "water_storage_tank_name", "inlet_water_temperature_option" } ), cAlphaFields ) );
		EXPECT_TRUE( compare_containers( std::vector< bool >( { false, true, false, false, false, true, true } ), lAlphaBlanks ) );

		EXPECT_EQ( 5, NumNumbers );
		EXPECT_TRUE( compare_containers( std::vector< std::string >( { "rated_capacity", "rated_gas_use_rate", "thermal_efficiency", "rated_fan_power", "auxiliary_electric_power" } ), cNumericFields ) );
		EXPECT_TRUE( compare_containers( std::vector< bool >( { true, false, true, false, true } ), lNumericBlanks ) );
		EXPECT_TRUE( compare_containers( std::vector< Real64 >( { 0, -99999, 0.80, 0.0, 0.0 } ), Numbers ) );
		EXPECT_EQ( 1, IOStatus );
	}

	TEST_F( InputProcessorFixture, getObjectItem_truncated_autosize_fields )
	{
		std::string const idf_objects = delimited_string({
																 "Version,8.3;",
																 "Humidifier:Steam:Gas,",
																 "  Main Gas Humidifier,     !- Name",
																 "  ,                        !- Availability Schedule Name",
																 "  autosize;                !- Rated Capacity {m3/s}",
														 });

		// Expect Rated Capacity to be filled in with the autosize value of -99999. Expect everything else to be empty string and 0

		ASSERT_TRUE( process_idf( idf_objects ) );

		std::string const CurrentModuleObject = "Humidifier:Steam:Gas";

		int NumGasSteamHums = InputProcessor::GetNumObjectsFound( CurrentModuleObject );
		ASSERT_EQ( 1, NumGasSteamHums );

		int TotalArgs = 0;
		int NumAlphas = 0;
		int NumNumbers = 0;

		InputProcessor::GetObjectDefMaxArgs( CurrentModuleObject, TotalArgs, NumAlphas, NumNumbers );

		int IOStatus = 0;
		Array1D_string Alphas( NumAlphas );
		Array1D< Real64 > Numbers( NumNumbers, 0.0 );
		Array1D_bool lNumericBlanks( NumNumbers, true );
		Array1D_bool lAlphaBlanks( NumAlphas, true );
		Array1D_string cAlphaFields( NumAlphas );
		Array1D_string cNumericFields( NumNumbers );

		InputProcessor::GetObjectItem( CurrentModuleObject, NumGasSteamHums, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

		EXPECT_EQ( 2, NumAlphas );
		EXPECT_TRUE( compare_containers( std::vector< std::string >( { "MAIN GAS HUMIDIFIER", "", "", "", "", "", "" } ), Alphas ) );
		EXPECT_TRUE( compare_containers( std::vector< std::string >( { "name", "availability_schedule_name", "thermal_efficiency_modifier_curve_name", "air_inlet_node_name", "air_outlet_node_name", "water_storage_tank_name", "inlet_water_temperature_option" } ), cAlphaFields ) );
		EXPECT_TRUE( compare_containers( std::vector< bool >( { false, true, true, true, true, true, true } ), lAlphaBlanks ) );

		EXPECT_EQ( 1, NumNumbers );
		EXPECT_TRUE( compare_containers( std::vector< std::string >( { "rated_capacity", "rated_gas_use_rate", "thermal_efficiency", "rated_fan_power", "auxiliary_electric_power" } ), cNumericFields ) );
		EXPECT_TRUE( compare_containers( std::vector< bool >( { false, true, true, true, true } ), lNumericBlanks ) );
		EXPECT_TRUE( compare_containers( std::vector< Real64 >( { -99999, 0, 0, 0, 0 } ), Numbers ) );
		EXPECT_EQ( 1, IOStatus );
	}

	TEST_F( InputProcessorFixture, getObjectItem_unitary_system_input )
	{
		std::string const idf_objects = delimited_string({
																 "AirLoopHVAC:UnitarySystem,",
																 "  GasHeat DXAC Furnace 1, !- Name",
																 "  Load,                   !- Control Type",
																 "  East Zone,              !- Controlling Zone or Thermostat Location",
																 "  None,                   !- Dehumidification Control Type",
																 "  FanAndCoilAvailSched,   !- Availability Schedule Name",
																 "  Zone Exhaust Node,         !- Air Inlet Node Name",
																 "  Zone 2 Inlet Node,   !- Air Outlet Node Name",
																 "  Fan:OnOff,              !- Supply Fan Object Type",
																 "  Supply Fan 1,           !- Supply Fan Name",
																 "  BlowThrough,            !- Fan Placement",
																 "  ContinuousFanSchedule,  !- Supply Air Fan Operating Mode Schedule Name",
																 "  Coil:Heating:Gas,       !- Heating Coil Object Type",
																 "  Furnace Heating Coil 1, !- Heating Coil Name",
																 "  ,                       !- DX Heating Coil Sizing Ratio",
																 "  Coil:Cooling:DX:VariableSpeed, !- Cooling Coil Object Type",
																 "  Furnace ACDXCoil 1,     !- Cooling Coil Name",
																 "  ,                       !- Use DOAS DX Cooling Coil",
																 "  ,                       !- DOAS DX Cooling Coil Leaving Minimum Air Temperature{ C }",
																 "  ,                       !- Latent Load Control",
																 "  Coil:Heating:Gas,       !- Supplemental Heating Coil Object Type",
																 "  Humidistat Reheat Coil 1, !- Supplemental Heating Coil Name",
																 "  SupplyAirFlowRate,      !- Supply Air Flow Rate Method During Cooling Operation",
																 "  1.6,                    !- Supply Air Flow Rate During Cooling Operation{ m3/s }",
																 "  ,                       !- Supply Air Flow Rate Per Floor Area During Cooling Operation{ m3/s-m2 }",
																 "  ,                       !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
																 "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
																 "  SupplyAirFlowRate,      !- Supply air Flow Rate Method During Heating Operation",
																 "  1.6,                    !- Supply Air Flow Rate During Heating Operation{ m3/s }",
																 "  ,                       !- Supply Air Flow Rate Per Floor Area during Heating Operation{ m3/s-m2 }",
																 "  ,                       !- Fraction of Autosized Design Heating Supply Air Flow Rate",
																 "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
																 "  SupplyAirFlowRate,      !- Supply Air Flow Rate Method When No Cooling or Heating is Required",
																 "  1.6,                    !- Supply Air Flow Rate When No Cooling or Heating is Required{ m3/s }",
																 "  ,                       !- Supply Air Flow Rate Per Floor Area When No Cooling or Heating is Required{ m3/s-m2 }",
																 "  ,                       !- Fraction of Autosized Design Cooling Supply Air Flow Rate",
																 "  ,                       !- Fraction of Autosized Design Heating Supply Air Flow Rate",
																 "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Cooling Operation{ m3/s-W }",
																 "  ,                       !- Design Supply Air Flow Rate Per Unit of Capacity During Heating Operation{ m3/s-W }",
																 "  80;                     !- Maximum Supply Air Temperature{ C }",
														 });

		ASSERT_TRUE( process_idf( idf_objects ) );

		std::string const CurrentModuleObject = "AirLoopHVAC:UnitarySystem";

		int num_unitary_systems = InputProcessor::GetNumObjectsFound( CurrentModuleObject );
		ASSERT_EQ( 1,  num_unitary_systems );

		int TotalArgs = 0;
		int NumAlphas = 0;
		int NumNumbers = 0;

		InputProcessor::GetObjectDefMaxArgs( CurrentModuleObject, TotalArgs, NumAlphas, NumNumbers );

		int IOStatus = 0;
		Array1D_string Alphas( NumAlphas );
		Array1D< Real64 > Numbers( NumNumbers, 0.0 );
		Array1D_bool lNumericBlanks( NumNumbers, true );
		Array1D_bool lAlphaBlanks( NumAlphas, true );
		Array1D_string cAlphaFields( NumAlphas );
		Array1D_string cNumericFields( NumNumbers );

		InputProcessor::GetObjectItem( CurrentModuleObject, num_unitary_systems, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

		EXPECT_EQ( 22, NumAlphas );
		EXPECT_TRUE( compare_containers( std::vector< std::string >( { "GASHEAT DXAC FURNACE 1", "LOAD", "EAST ZONE", "NONE", "FANANDCOILAVAILSCHED", "ZONE EXHAUST NODE", "ZONE 2 INLET NODE",
																	   "FAN:ONOFF", "SUPPLY FAN 1", "BLOWTHROUGH", "CONTINUOUSFANSCHEDULE", "COIL:HEATING:GAS", "FURNACE HEATING COIL 1",
																	   "COIL:COOLING:DX:VARIABLESPEED", "FURNACE ACDXCOIL 1", "NO", "SENSIBLEONLYLOADCONTROL", "COIL:HEATING:GAS",
																	   "HUMIDISTAT REHEAT COIL 1", "SUPPLYAIRFLOWRATE", "SUPPLYAIRFLOWRATE", "SUPPLYAIRFLOWRATE", "", "", "", "", ""} ), Alphas ) );
		EXPECT_TRUE( compare_containers( std::vector< bool >( { false, false, false, false, false, false, false, false, false, false,
																false, false, false, false, false, true, true, false, false, false,
																false, false, true, true, true, true, true } ), lAlphaBlanks ) );

		EXPECT_EQ( 17, NumNumbers );
		EXPECT_TRUE( compare_containers( std::vector< bool >( { true, true, false, true, true, true, false, true, true, true,
																false, true, true, true, true, true, false, true, true, true,
																true, true, true, true, true, true } ), lNumericBlanks ) );
		EXPECT_TRUE( compare_containers( std::vector< Real64 >( { 1, 2, 1.6, 0, 0, 0, 1.6, 0, 0, 0,
																  1.6, 0, 0, 0, 0, 0, 80, 0, 0, 0,
																  0, 0, 0, 0, 0, 0 } ), Numbers ) );
		EXPECT_EQ( 1, IOStatus );
	}

	TEST_F( InputProcessorFixture, getObjectItem_test_numbers_as_strings)
	{
		std::string const idf_objects = delimited_string({
																 "  ZoneHVAC:EquipmentConnections,",
																 "    401,                     !- Zone Name",
																 "    Z401 terminal list,      !- Zone Conditioning Equipment List Name",
																 "    Z401 zone inlet,         !- Zone Air Inlet Node or NodeList Name",
																 "    ,                        !- Zone Air Exhaust Node or NodeList Name",
																 "    Z401 air node,           !- Zone Air Node Name",
																 "    Z401 outlet node;        !- Zone Return Air Node Name",
														 });

		ASSERT_TRUE( process_idf( idf_objects ) );

		std::string const CurrentModuleObject = "ZoneHVAC:EquipmentConnections";

		int num_eq_connections = InputProcessor::GetNumObjectsFound( CurrentModuleObject );
		ASSERT_EQ( 1,  num_eq_connections );

		int TotalArgs = 0;
		int NumAlphas = 0;
		int NumNumbers = 0;

		InputProcessor::GetObjectDefMaxArgs( CurrentModuleObject, TotalArgs, NumAlphas, NumNumbers );

		int IOStatus = 0;
		Array1D_string Alphas( NumAlphas );
		Array1D< Real64 > Numbers( NumNumbers, 0.0 );
		Array1D_bool lNumericBlanks( NumNumbers, true );
		Array1D_bool lAlphaBlanks( NumAlphas, true );
		Array1D_string cAlphaFields( NumAlphas );
		Array1D_string cNumericFields( NumNumbers );

		InputProcessor::GetObjectItem( CurrentModuleObject, num_eq_connections, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

		EXPECT_EQ( 6, NumAlphas );
		EXPECT_TRUE( compare_containers( std::vector< std::string >( { "401", "Z401 TERMINAL LIST", "Z401 ZONE INLET", "",
																	   "Z401 AIR NODE", "Z401 OUTLET NODE", "", "" } ), Alphas ) );
		EXPECT_TRUE( compare_containers( std::vector< bool >( { false, false, false, true, false, false, true, true } ), lAlphaBlanks ) );

		EXPECT_EQ( 0, NumNumbers );
		EXPECT_TRUE( compare_containers( std::vector< Real64 >( { } ), Numbers ) );
		EXPECT_TRUE( compare_containers( std::vector< bool >( { } ), lNumericBlanks ) );
		EXPECT_EQ( 1, IOStatus );
	}

	TEST_F( InputProcessorFixture, getObjectItem_test_zone_input)
	{
		std::string const idf_objects = delimited_string({
																 "Zone,",
																 "  EAST ZONE,              !- Name",
																 "  0,                      !- Direction of Relative North{ deg }",
																 "  0,                      !- X Origin{ m }",
																 "  0,                      !- Y Origin{ m }",
																 "  0,                      !- Z Origin{ m }",
																 "  1,                      !- Type",
																 "  1,                      !- Multiplier",
																 "  autocalculate,          !- Ceiling Height{ m }",
																 "  autocalculate;          !- Volume{ m3 }",
														 });

		ASSERT_TRUE( process_idf( idf_objects ) );

		std::string const CurrentModuleObject = "Zone";

		int num_zones = InputProcessor::GetNumObjectsFound( CurrentModuleObject );
		ASSERT_EQ( 1,  num_zones );

		int TotalArgs = 0;
		int NumAlphas = 0;
		int NumNumbers = 0;

		InputProcessor::GetObjectDefMaxArgs( CurrentModuleObject, TotalArgs, NumAlphas, NumNumbers );

		int IOStatus = 0;
		Array1D_string Alphas( NumAlphas );
		Array1D< Real64 > Numbers( NumNumbers, 0.0 );
		Array1D_bool lNumericBlanks( NumNumbers, true );
		Array1D_bool lAlphaBlanks( NumAlphas, true );
		Array1D_string cAlphaFields( NumAlphas );
		Array1D_string cNumericFields( NumNumbers );

		InputProcessor::GetObjectItem( CurrentModuleObject, num_zones, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

		EXPECT_EQ( 1, NumAlphas );
		EXPECT_TRUE( compare_containers( std::vector< std::string >( { "EAST ZONE", "", "", ""} ), Alphas ) );
		EXPECT_TRUE( compare_containers( std::vector< bool >( { false, true, true, true } ), lAlphaBlanks ) );

		EXPECT_EQ( 8, NumNumbers );
		EXPECT_TRUE( compare_containers( std::vector< bool >( { false, false, false, false, false, false, false, false, true } ), lNumericBlanks ) );
		EXPECT_TRUE( compare_containers( std::vector< Real64 >( { 0, 0, 0, 0, 1, 1, -99999, -99999, 0 } ), Numbers ) );
		EXPECT_EQ( 1, IOStatus );
	}

	TEST_F( InputProcessorFixture, getObjectItem_zone_HVAC_input)
	{
		std::string const idf_objects = delimited_string({
																 "ZoneHVAC:EquipmentConnections,",
																 "EAST ZONE,                 !- Zone Name",
																 "  Zone2Equipment,          !- Zone Conditioning Equipment List Name",
																 "  Zone 2 Inlet Node,       !- Zone Air Inlet Node or NodeList Name",
																 "  Zone Exhaust Node,       !- Zone Air Exhaust Node or NodeList Name",
																 "  Zone 2 Node,             !- Zone Air Node Name",
																 "  Zone 2 Outlet Node;      !- Zone Return Air Node Name",
																 "  ",
																 "ZoneHVAC:EquipmentList,",
																 "  Zone2Equipment,          !- Name",
																 "  AirLoopHVAC:UnitarySystem, !- Zone Equipment 1 Object Type",
																 "  GasHeat DXAC Furnace 1,          !- Zone Equipment 1 Name",
																 "  1,                       !- Zone Equipment 1 Cooling Sequence",
																 "  1;                       !- Zone Equipment 1 Heating or No - Load Sequence",
														 });

		ASSERT_TRUE( process_idf( idf_objects ) );

		std::string CurrentModuleObject = "ZoneHVAC:EquipmentConnections";

		int num_equipment_connections = InputProcessor::GetNumObjectsFound( CurrentModuleObject );
		ASSERT_EQ( 1,  num_equipment_connections );

		int TotalArgs = 0;
		int NumAlphas = 0;
		int NumNumbers = 0;

		InputProcessor::GetObjectDefMaxArgs( CurrentModuleObject, TotalArgs, NumAlphas, NumNumbers );

		int IOStatus = 0;
		Array1D_string Alphas( NumAlphas );
		Array1D< Real64 > Numbers( NumNumbers, 0.0 );
		Array1D_bool lNumericBlanks( NumNumbers, true );
		Array1D_bool lAlphaBlanks( NumAlphas, true );
		Array1D_string cAlphaFields( NumAlphas );
		Array1D_string cNumericFields( NumNumbers );

		InputProcessor::GetObjectItem( CurrentModuleObject, num_equipment_connections, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

		EXPECT_EQ( 6, NumAlphas );
		EXPECT_TRUE( compare_containers( std::vector< std::string >( { "EAST ZONE", "ZONE2EQUIPMENT", "ZONE 2 INLET NODE", "ZONE EXHAUST NODE",
																	   "ZONE 2 NODE", "ZONE 2 OUTLET NODE", "", ""} ), Alphas ) );
		EXPECT_TRUE( compare_containers( std::vector< bool >( { false, false, false, false, false, false, true, true } ), lAlphaBlanks ) );

		EXPECT_EQ( 0, NumNumbers );
		EXPECT_TRUE( compare_containers( std::vector< bool >( { } ), lNumericBlanks ) );
		EXPECT_TRUE( compare_containers( std::vector< Real64 >( { } ), Numbers ) );

		CurrentModuleObject = "ZoneHVAC:EquipmentList";

		int num_equipment_lists = InputProcessor::GetNumObjectsFound( CurrentModuleObject );
		ASSERT_EQ( 1,  num_equipment_lists );

		int TotalArgs2 = 0;
		int NumAlphas2 = 0;
		int NumNumbers2 = 0;

		InputProcessor::GetObjectDefMaxArgs( CurrentModuleObject, TotalArgs2, NumAlphas2, NumNumbers2 );

		Array1D_string Alphas2( NumAlphas2 );
		Array1D< Real64 > Numbers2( NumNumbers2, 0.0 );
		Array1D_bool lNumericBlanks2( NumNumbers2, true );
		Array1D_bool lAlphaBlanks2( NumAlphas2, true );
		Array1D_string cAlphaFields2( NumAlphas2 );
		Array1D_string cNumericFields2( NumNumbers2 );

		InputProcessor::GetObjectItem( CurrentModuleObject, num_equipment_lists, Alphas2, NumAlphas2, Numbers2, NumNumbers2, IOStatus, lNumericBlanks2, lAlphaBlanks2, cAlphaFields2, cNumericFields2 );

		EXPECT_EQ( 3, NumAlphas2 );
		EXPECT_TRUE( compare_containers( std::vector< std::string >( { "ZONE2EQUIPMENT", "AIRLOOPHVAC:UNITARYSYSTEM", "GASHEAT DXAC FURNACE 1" } ), Alphas2 ) );
		EXPECT_TRUE( compare_containers( std::vector< bool >( { false, false, false } ), lAlphaBlanks2 ) );

		EXPECT_EQ( 2, NumNumbers2 );
		EXPECT_TRUE( compare_containers( std::vector< bool >( { false, false } ), lNumericBlanks2 ) );
		EXPECT_TRUE( compare_containers( std::vector< Real64 >( { 1, 1 } ), Numbers2 ) );
		EXPECT_EQ( 1, IOStatus );
	}

	TEST_F( InputProcessorFixture, getObjectItem_coil_heating_gas)
	{
		std::string const idf_objects = delimited_string({
																 "Coil:Heating:Gas,",
																 "  Furnace Heating Coil 1, !- Name",
																 "  FanAndCoilAvailSched,   !- Availability Schedule Name",
																 "  0.8,                    !- Gas Burner Efficiency",
																 "  32000,                  !- Nominal Capacity{ W }",
																 "  Heating Coil Air Inlet Node, !- Air Inlet Node Name",
																 "  Reheat Coil Air Inlet Node;  !- Air Outlet Node Name",
																 "  ",
																 "Coil:Heating:Gas,",
																 "  Humidistat Reheat Coil 1, !- Name",
																 "  FanAndCoilAvailSched, !- Availability Schedule Name",
																 "  0.8, !- Gas Burner Efficiency",
																 "  32000, !- Nominal Capacity{ W }",
																 "  Reheat Coil Air Inlet Node, !- Air Inlet Node Name",
																 "  Zone 2 Inlet Node;    !- Air Outlet Node Name",
														 });

		ASSERT_TRUE( process_idf( idf_objects ) );

		std::string const CurrentModuleObject = "Coil:Heating:Gas";

		int num_coil_heating_gas = InputProcessor::GetNumObjectsFound( CurrentModuleObject );
		ASSERT_EQ( 2,  num_coil_heating_gas );

		int TotalArgs = 0;
		int NumAlphas = 0;
		int NumNumbers = 0;

		InputProcessor::GetObjectDefMaxArgs( CurrentModuleObject, TotalArgs, NumAlphas, NumNumbers );

		int IOStatus = 0;
		Array1D_string Alphas( NumAlphas );
		Array1D< Real64 > Numbers( NumNumbers, 0.0 );
		Array1D_bool lNumericBlanks( NumNumbers, true );
		Array1D_bool lAlphaBlanks( NumAlphas, true );
		Array1D_string cAlphaFields( NumAlphas );
		Array1D_string cNumericFields( NumNumbers );

		InputProcessor::GetObjectItem( CurrentModuleObject, 1, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

		EXPECT_EQ( 4, NumAlphas );
		EXPECT_TRUE( compare_containers( std::vector< std::string >( { "FURNACE HEATING COIL 1", "FANANDCOILAVAILSCHED", "HEATING COIL AIR INLET NODE",
																	   "REHEAT COIL AIR INLET NODE", "", "" } ), Alphas ) );
		EXPECT_TRUE( compare_containers( std::vector< bool >( { false, false, false, false, true, true } ), lAlphaBlanks ) );

		EXPECT_EQ( 2, NumNumbers );
		EXPECT_TRUE( compare_containers( std::vector< bool >( { false, false, true, true } ), lNumericBlanks ) );
		EXPECT_TRUE( compare_containers( std::vector< Real64 >( { 0.8, 32000, 0, 0 } ), Numbers ) );

		int TotalArgs2 = 0;
		int NumAlphas2 = 0;
		int NumNumbers2 = 0;

		InputProcessor::GetObjectDefMaxArgs( CurrentModuleObject, TotalArgs2, NumAlphas2, NumNumbers2 );

		Array1D_string Alphas2( NumAlphas2 );
		Array1D< Real64 > Numbers2( NumNumbers2, 0.0 );
		Array1D_bool lNumericBlanks2( NumNumbers2, true );
		Array1D_bool lAlphaBlanks2( NumAlphas2, true );
		Array1D_string cAlphaFields2( NumAlphas2 );
		Array1D_string cNumericFields2( NumNumbers2 );

		InputProcessor::GetObjectItem( CurrentModuleObject, 2, Alphas2, NumAlphas2, Numbers2, NumNumbers2, IOStatus, lNumericBlanks2, lAlphaBlanks2, cAlphaFields2, cNumericFields2 );

		EXPECT_EQ( 4, NumAlphas2 );
		EXPECT_TRUE( compare_containers( std::vector< std::string >( { "HUMIDISTAT REHEAT COIL 1", "FANANDCOILAVAILSCHED", "REHEAT COIL AIR INLET NODE",
																	   "ZONE 2 INLET NODE", "", "" } ), Alphas2 ) );
		EXPECT_TRUE( compare_containers( std::vector< bool >( { false, false, false, false, true, true } ), lAlphaBlanks2 ) );

		EXPECT_EQ( 2, NumNumbers2 );
		EXPECT_TRUE( compare_containers( std::vector< bool >( { false, false, true, true } ), lNumericBlanks2 ) );
		EXPECT_TRUE( compare_containers( std::vector< Real64 >( { 0.8, 32000, 0, 0 } ), Numbers2 ) );

		EXPECT_EQ( 1, IOStatus );
	}

	TEST_F( InputProcessorFixture, getObjectItem_schedule_objects)
	{
		std::string const idf_objects = delimited_string({
																 "ScheduleTypeLimits,",
																 "  Any Number;             !- Name",
																 "  ",
																 "Schedule:Compact,",
																 "  FanAndCoilAvailSched,   !- Name",
																 "  Any Number,             !- Schedule Type Limits Name",
																 "  Through: 12/31,         !- Field 1",
																 "  For: AllDays,           !- Field 2",
																 "  Until: 24:00, 1.0;      !- Field 3",
																 "  ",
																 "Schedule:Compact,",
																 "  ContinuousFanSchedule,  !- Name",
																 "  Any Number,             !- Schedule Type Limits Name",
																 "  Through: 12/31,         !- Field 1",
																 "  For: AllDays,           !- Field 2",
																 "  Until: 24:00, 1.0;      !- Field 3",
														 });

		ASSERT_TRUE( process_idf( idf_objects ) );

		std::string CurrentModuleObject = "ScheduleTypeLimits";

		int num_schedule_type_limits = InputProcessor::GetNumObjectsFound( CurrentModuleObject );
		ASSERT_EQ( 1,  num_schedule_type_limits );

		int TotalArgs = 0;
		int NumAlphas = 0;
		int NumNumbers = 0;

		InputProcessor::GetObjectDefMaxArgs( CurrentModuleObject, TotalArgs, NumAlphas, NumNumbers );

		int IOStatus = 0;
		Array1D_string Alphas( NumAlphas );
		Array1D< Real64 > Numbers( NumNumbers, 0.0 );
		Array1D_bool lNumericBlanks( NumNumbers, true );
		Array1D_bool lAlphaBlanks( NumAlphas, true );
		Array1D_string cAlphaFields( NumAlphas );
		Array1D_string cNumericFields( NumNumbers );

		InputProcessor::GetObjectItem( CurrentModuleObject, num_schedule_type_limits, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

		EXPECT_EQ( 1, NumAlphas );
		EXPECT_TRUE( compare_containers( std::vector< std::string >( { "ANY NUMBER", "", "" } ), Alphas ) );
		EXPECT_TRUE( compare_containers( std::vector< bool >( { false, true, true } ), lAlphaBlanks ) );

		EXPECT_EQ( 0, NumNumbers );
		EXPECT_TRUE( compare_containers( std::vector< Real64 >( { 0, 0 } ), Numbers ) );
		EXPECT_TRUE( compare_containers( std::vector< bool >( { true, true } ), lNumericBlanks ) );


		CurrentModuleObject = "Schedule:Compact";

		int num_schedule_compact = InputProcessor::GetNumObjectsFound( CurrentModuleObject );
		ASSERT_EQ( 2,  num_schedule_compact );

		TotalArgs = 0;
		NumAlphas = 0;
		NumNumbers = 0;

		InputProcessor::GetObjectDefMaxArgs( CurrentModuleObject, TotalArgs, NumAlphas, NumNumbers );

		Array1D_string Alphas2( NumAlphas );
		Array1D< Real64 > Numbers2( NumNumbers, 0.0 );
		Array1D_bool lNumericBlanks2( NumNumbers, true );
		Array1D_bool lAlphaBlanks2( NumAlphas, true );
		Array1D_string cAlphaFields2( NumAlphas );
		Array1D_string cNumericFields2( NumNumbers );

		InputProcessor::GetObjectItem( CurrentModuleObject, 2, Alphas2, NumAlphas, Numbers2, NumNumbers, IOStatus, lNumericBlanks2, lAlphaBlanks2, cAlphaFields2, cNumericFields2 );

		// Container size is 4500 here!
		EXPECT_EQ( 6, NumAlphas );
		EXPECT_TRUE( compare_containers( std::vector< std::string >( { "FANANDCOILAVAILSCHED", "ANY NUMBER", "THROUGH: 12/31", "FOR: ALLDAYS", "UNTIL: 24:00", "1.000000" } ), Alphas2 ) );
		EXPECT_TRUE( compare_containers( std::vector< bool >( { false, false, false, false, false, false } ), lAlphaBlanks2 ) );

		EXPECT_EQ( 0, NumNumbers );
		EXPECT_TRUE( compare_containers( std::vector< Real64 >( { } ), Numbers2 ) );
		EXPECT_TRUE( compare_containers( std::vector< bool >( { } ), lNumericBlanks2 ) );

		EXPECT_EQ( 1, IOStatus );
	}

	TEST_F( InputProcessorFixture, getObjectItem_fan_on_off)
	{
		std::string const idf_objects = delimited_string({
																 "Fan:OnOff,",
																 "  Supply Fan 1,           !- Name",
																 "  FanAndCoilAvailSched,   !- Availability Schedule Name",
																 "  0.7,                    !- Fan Total Efficiency",
																 "  600.0,                  !- Pressure Rise{ Pa }",
																 "  1.6,                    !- Maximum Flow Rate{ m3 / s }",
																 "  0.9,                    !- Motor Efficiency",
																 "  1.0,                    !- Motor In Airstream Fraction",
																 "  Zone Exhaust Node,      !- Air Inlet Node Name",
																 "  DX Cooling Coil Air Inlet Node;  !- Air Outlet Node Name",
																 "  ",
														 });

		ASSERT_TRUE( process_idf( idf_objects ) );

		std::string const CurrentModuleObject = "Fan:OnOff";

		int num_fans = InputProcessor::GetNumObjectsFound( CurrentModuleObject );
		ASSERT_EQ( 1,  num_fans );

		int TotalArgs = 0;
		int NumAlphas = 0;
		int NumNumbers = 0;

		InputProcessor::GetObjectDefMaxArgs( CurrentModuleObject, TotalArgs, NumAlphas, NumNumbers );

		int IOStatus = 0;
		Array1D_string Alphas( NumAlphas );
		Array1D< Real64 > Numbers( NumNumbers, 0.0 );
		Array1D_bool lNumericBlanks( NumNumbers, true );
		Array1D_bool lAlphaBlanks( NumAlphas, true );
		Array1D_string cAlphaFields( NumAlphas );
		Array1D_string cNumericFields( NumNumbers );

		InputProcessor::GetObjectItem( CurrentModuleObject, num_fans, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

		EXPECT_EQ( 4, NumAlphas );
		EXPECT_TRUE( compare_containers( std::vector< std::string >( { "SUPPLY FAN 1", "FANANDCOILAVAILSCHED", "ZONE EXHAUST NODE", "DX COOLING COIL AIR INLET NODE",
																	   "", "", "" } ), Alphas ) );
		EXPECT_TRUE( compare_containers( std::vector< bool >( { false, false, false, false, true, true, true } ), lAlphaBlanks ) );

		EXPECT_EQ( 5, NumNumbers );
		EXPECT_TRUE( compare_containers( std::vector< Real64 >( { 0.7, 600, 1.6, 0.9, 1.0 } ), Numbers ) );
		EXPECT_TRUE( compare_containers( std::vector< bool >( { false, false, false, false, false } ), lNumericBlanks ) );
		EXPECT_EQ( 1, IOStatus );
	}

	TEST_F( InputProcessorFixture, getObjectItem_curve_quadratic)
	{
		std::string const idf_objects = delimited_string({
																 "Curve:Quadratic,",
																 "  CoolCapFFF,       !- Name",
																 "  0.8,                    !- Coefficient1 Constant",
																 "  0.2,                    !- Coefficient2 x",
																 "  0.0,                    !- Coefficient3 x**2",
																 "  0.5,                    !- Minimum Value of x",
																 "  1.5;                    !- Maximum Value of x",
																 "  ",
																 "Curve:Quadratic,",
																 "  COOLEIRFFF,           !- Name",
																 "  1.1552,                 !- Coefficient1 Constant",
																 "  -0.1808,                !- Coefficient2 x",
																 "  0.0256,                 !- Coefficient3 x**2",
																 "  0.5,                    !- Minimum Value of x",
																 "  1.5;                    !- Maximum Value of x",
																 "  ",
																 "Curve:Quadratic,",
																 "  PLFFPLR,          !- Name",
																 "  0.85,                   !- Coefficient1 Constant",
																 "  0.15,                   !- Coefficient2 x",
																 "  0.0,                    !- Coefficient3 x**2",
																 "  0.0,                    !- Minimum Value of x",
																 "  1.0;                    !- Maximum Value of x",
														 });

		ASSERT_TRUE( process_idf( idf_objects ) );

		std::string const CurrentModuleObject = "Curve:Quadratic";

		int num_curve_quad = InputProcessor::GetNumObjectsFound( CurrentModuleObject );
		ASSERT_EQ( 3,  num_curve_quad );

		int TotalArgs = 0;
		int NumAlphas = 0;
		int NumNumbers = 0;

		InputProcessor::GetObjectDefMaxArgs( CurrentModuleObject, TotalArgs, NumAlphas, NumNumbers );

		int IOStatus = 0;
		Array1D_string Alphas( NumAlphas );
		Array1D< Real64 > Numbers( NumNumbers, 0.0 );
		Array1D_bool lNumericBlanks( NumNumbers, true );
		Array1D_bool lAlphaBlanks( NumAlphas, true );
		Array1D_string cAlphaFields( NumAlphas );
		Array1D_string cNumericFields( NumNumbers );

		InputProcessor::GetObjectItem( CurrentModuleObject, 2, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

		EXPECT_EQ( 1, NumAlphas );
		EXPECT_TRUE( compare_containers( std::vector< std::string >( { "COOLCAPFFF", "", "" } ), Alphas ) );
		EXPECT_TRUE( compare_containers( std::vector< bool >( { false, true, true } ), lAlphaBlanks ) );

		EXPECT_EQ( 5, NumNumbers );
		EXPECT_TRUE( compare_containers( std::vector< bool >( { false, false, false, false, false, true, true } ), lNumericBlanks ) );
		EXPECT_TRUE( compare_containers( std::vector< Real64 >( { 0.8, 0.2, 0, 0.5, 1.5, 0, 0 } ), Numbers ) );

		int TotalArgs2 = 0;
		int NumAlphas2 = 0;
		int NumNumbers2 = 0;

		InputProcessor::GetObjectDefMaxArgs( CurrentModuleObject, TotalArgs2, NumAlphas2, NumNumbers2 );

		Array1D_string Alphas2( NumAlphas2 );
		Array1D< Real64 > Numbers2( NumNumbers2, 0.0 );
		Array1D_bool lNumericBlanks2( NumNumbers2, true );
		Array1D_bool lAlphaBlanks2( NumAlphas2, true );
		Array1D_string cAlphaFields2( NumAlphas2 );
		Array1D_string cNumericFields2( NumNumbers2 );

		InputProcessor::GetObjectItem( CurrentModuleObject, 1, Alphas2, NumAlphas2, Numbers2, NumNumbers2, IOStatus, lNumericBlanks2, lAlphaBlanks2, cAlphaFields2, cNumericFields2 );

		EXPECT_EQ( 1, NumAlphas2 );
		EXPECT_TRUE( compare_containers( std::vector< std::string >( { "COOLEIRFFF", "", "" } ), Alphas2 ) );
		EXPECT_TRUE( compare_containers( std::vector< bool >( { false, true, true } ), lAlphaBlanks2 ) );

		EXPECT_EQ( 5, NumNumbers2 );
		EXPECT_TRUE( compare_containers( std::vector< bool >( { false, false, false, false, false, true, true } ), lNumericBlanks2 ) );
		EXPECT_TRUE( compare_containers( std::vector< Real64 >( { 1.1552, -0.1808, 0.0256, 0.5, 1.5, 0, 0 } ), Numbers2 ) );

		int TotalArgs3 = 0;
		int NumAlphas3 = 0;
		int NumNumbers3 = 0;

		InputProcessor::GetObjectDefMaxArgs( CurrentModuleObject, TotalArgs3, NumAlphas3, NumNumbers3 );

		Array1D_string Alphas3( NumAlphas3 );
		Array1D< Real64 > Numbers3( NumNumbers3, 0.0 );
		Array1D_bool lNumericBlanks3( NumNumbers3, true );
		Array1D_bool lAlphaBlanks3( NumAlphas3, true );
		Array1D_string cAlphaFields3( NumAlphas3 );
		Array1D_string cNumericFields3( NumNumbers3 );

		InputProcessor::GetObjectItem( CurrentModuleObject, 3, Alphas3, NumAlphas3, Numbers3, NumNumbers3, IOStatus, lNumericBlanks3, lAlphaBlanks3, cAlphaFields3, cNumericFields3 );

		EXPECT_EQ( 1, NumAlphas3 );
		EXPECT_TRUE( compare_containers( std::vector< std::string >( { "PLFFPLR", "", "" } ), Alphas3 ) );
		EXPECT_TRUE( compare_containers( std::vector< bool >( { false, true, true } ), lAlphaBlanks3 ) );

		EXPECT_EQ( 5, NumNumbers3 );
		EXPECT_TRUE( compare_containers( std::vector< bool >( { false, false, false, false, false, true, true } ), lNumericBlanks3 ) );
		EXPECT_TRUE( compare_containers( std::vector< Real64 >( { 0.85, 0.15, 0, 0.0, 1, 0, 0 } ), Numbers3 ) );
		EXPECT_EQ( 1, IOStatus );
	}

	TEST_F( InputProcessorFixture, getObjectItem_coil_cooling_dx_variable_speed)
	{
		std::string const idf_objects = delimited_string({
																 "Coil:Cooling:DX:VariableSpeed,",
																 "  Furnace ACDXCoil 1, !- Name",
																 "  DX Cooling Coil Air Inlet Node, !- Air Inlet Node Name",
																 "  Heating Coil Air Inlet Node, !- Air Outlet Node Name",
																 "  10.0, !- Number of Speeds{ dimensionless }",
																 "  10.0, !- Nominal Speed Level{ dimensionless }",
																 "  32000.0, !- Gross Rated Total Cooling Capacity At Selected Nominal Speed Level{ w }",
																 "  1.6, !- Rated Air Flow Rate At Selected Nominal Speed Level{ m3 / s }",
																 "  0.0, !- Nominal Time for Condensate to Begin Leaving the Coil{ s }",
																 "  0.0, !- Initial Moisture Evaporation Rate Divided by Steady - State AC Latent Capacity{ dimensionless }",
																 "  PLFFPLR, !- Energy Part Load Fraction Curve Name",
																 "  , !- Condenser Air Inlet Node Name",
																 "  AirCooled, !- Condenser Type",
																 "  , !- Evaporative Condenser Pump Rated Power Consumption{ W }",
																 "  200.0, !- Crankcase Heater Capacity{ W }",
																 "  10.0, !- Maximum Outdoor Dry - Bulb Temperature for Crankcase Heater Operation{ C }",
																 "  , !- Supply Water Storage Tank Name",
																 "  , !- Condensate Collection Water Storage Tank Name",
																 "  , !- Basin Heater Capacity{ W / K }",
																 "  , !- Basin Heater Setpoint Temperature{ C }",
																 "  , !- Basin Heater Operating Schedule Name",
																 "  1524.1, !- Speed 1 Reference Unit Gross Rated Total Cooling Capacity{ w }",
																 "  0.75, !- Speed 1 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
																 "  4.0, !- Speed 1 Reference Unit Gross Rated Cooling COP{ dimensionless }",
																 "  0.1359072, !- Speed 1 Reference Unit Rated Air Flow Rate{ m3 / s }",
																 "  0.26, !- Speed 1 Reference Unit Rated Condenser Air Flow Rate{ m3 / s }",
																 "  , !- Speed 1 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
																 "  CoolCapFT, !- Speed 1 Total Cooling Capacity Function of Temperature Curve Name",
																 "  CoolCapFFF, !- Speed 1 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
																 "  COOLEIRFT, !- Speed 1 Energy Input Ratio Function of Temperature Curve Name",
																 "  COOLEIRFFF, !- Speed 1 Energy Input Ratio Function of Air Flow Fraction Curve Name",
																 "  1877.9, !- Speed 2 Reference Unit Gross Rated Total Cooling Capacity{ w }",
																 "  0.75, !- Speed 2 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
																 "  4.0, !- Speed 2 Reference Unit Gross Rated Cooling COP{ dimensionless }",
																 "  0.151008, !- Speed 2 Reference Unit Rated Air Flow Rate{ m3 / s }",
																 "  0.30, !- Speed 2 Reference Unit Rated Condenser Air Flow Rate{ m3 / s }",
																 "  , !- Speed 2 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
																 "  CoolCapFT, !- Speed 2 Total Cooling Capacity Function of Temperature Curve Name",
																 "  CoolCapFFF, !- Speed 2 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
																 "  COOLEIRFT, !- Speed 2 Energy Input Ratio Function of Temperature Curve Name",
																 "  COOLEIRFFF, !- Speed 2 Energy Input Ratio Function of Air Flow Fraction Curve Name",
																 "  2226.6, !- Speed 3 Reference Unit Gross Rated Total Cooling Capacity{ w }",
																 "  0.75, !- Speed 3 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
																 "  4.0, !- Speed 3 Reference Unit Gross Rated Cooling COP{ dimensionless }",
																 "  0.1661088, !- Speed 3 Reference Unit Rated Air Flow Rate{ m3 / s }",
																 "  0.33, !- Speed 3 Reference Unit Rated Condenser Air Flow Rate{ m3 / s }",
																 "  , !- Speed 3 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
																 "  CoolCapFT, !- Speed 3 Total Cooling Capacity Function of Temperature Curve Name",
																 "  CoolCapFFF, !- Speed 3 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
																 "  COOLEIRFT, !- Speed 3 Energy Input Ratio Function of Temperature Curve Name",
																 "  COOLEIRFFF, !- Speed 3 Energy Input Ratio Function of Air Flow Fraction Curve Name",
																 "  2911.3, !- Speed 4 Reference Unit Gross Rated Total Cooling Capacity{ w }",
																 "  0.75, !- Speed 4 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
																 "  4.0, !- Speed 4 Reference Unit Gross Rated Cooling COP{ dimensionless }",
																 "  0.1963104, !- Speed 4 Reference Unit Rated Air Flow Rate{ m3 / s }",
																 "  0.38, !- Speed 4 Reference Unit Rated Condenser Air Flow Rate{ m3 / s }",
																 "  , !- Speed 4 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
																 "  CoolCapFT, !- Speed 4 Total Cooling Capacity Function of Temperature Curve Name",
																 "  CoolCapFFF, !- Speed 4 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
																 "  COOLEIRFT, !- Speed 4 Energy Input Ratio Function of Temperature Curve Name",
																 "  COOLEIRFFF, !- Speed 4 Energy Input Ratio Function of Air Flow Fraction Curve Name",
																 "  3581.7, !- Speed 5 Reference Unit Gross Rated Total Cooling Capacity{ w }",
																 "  0.75, !- Speed 5 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
																 "  4.0, !- Speed 5 Reference Unit Gross Rated Cooling COP{ dimensionless }",
																 "  0.226512, !- Speed 5 Reference Unit Rated Air Flow Rate{ m3 / s }",
																 "  0.44, !- Speed 5 Reference Unit Rated Condenser Air Flow Rate{ m3 / s }",
																 "  , !- Speed 5 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
																 "  CoolCapFT, !- Speed 5 Total Cooling Capacity Function of Temperature Curve Name",
																 "  CoolCapFFF, !- Speed 5 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
																 "  COOLEIRFT, !- Speed 5 Energy Input Ratio Function of Temperature Curve Name",
																 "  COOLEIRFFF, !- Speed 5 Energy Input Ratio Function of Air Flow Fraction Curve Name",
																 "  4239.5, !- Speed 6 Reference Unit Gross Rated Total Cooling Capacity{ w }",
																 "  0.75, !- Speed 6 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
																 "  4.0, !- Speed 6 Reference Unit Gross Rated Cooling COP{ dimensionless }",
																 "  0.2567136, !- Speed 6 Reference Unit Rated Air Flow Rate{ m3 / s }",
																 "  0.50, !- Speed 6 Reference Unit Condenser Air Flow Rate{ m3 / s }",
																 "  , !- Speed 6 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
																 "  CoolCapFT, !- Speed 6 Total Cooling Capacity Function of Temperature Curve Name",
																 "  CoolCapFFF, !- Speed 6 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
																 "  COOLEIRFT, !- Speed 6 Energy Input Ratio Function of Temperature Curve Name",
																 "  COOLEIRFFF, !- Speed 6 Energy Input Ratio Function of Air Flow Fraction Curve Name",
																 "  4885.7, !- Speed 7 Reference Unit Gross Rated Total Cooling Capacity{ w }",
																 "  0.75, !- Speed 7 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
																 "  4.0, !- Speed 7 Reference Unit Gross Rated Cooling COP{ dimensionless }",
																 "  0.2869152, !- Speed 7 Reference Unit Rated Air Flow Rate{ m3 / s }",
																 "  0.57, !- Speed 7 Reference Unit Condenser Flow Rate{ m3 / s }",
																 "  , !- Speed 7 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
																 "  CoolCapFT, !- Speed 7 Total Cooling Capacity Function of Temperature Curve Name",
																 "  CoolCapFFF, !- Speed 7 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
																 "  COOLEIRFT, !- Speed 7 Energy Input Ratio Function of Temperature Curve Name",
																 "  COOLEIRFFF, !- Speed 7 Energy Input Ratio Function of Air Flow Fraction Curve Name",
																 "  5520.7, !- Speed 8 Reference Unit Gross Rated Total Cooling Capacity{ w }",
																 "  0.75, !- Speed 8 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
																 "  4.0, !- Speed 8 Reference Unit Gross Rated Cooling COP{ dimensionless }",
																 "  0.3171168, !- Speed 8 Reference Unit Rated Air Flow Rate{ m3 / s }",
																 "  0.63, !- Speed 8 Reference Unit Condenser Air Flow Rate{ m3 / s }",
																 "  , !- Speed 8 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
																 "  CoolCapFT, !- Speed 8 Total Cooling Capacity Function of Temperature Curve Name",
																 "  CoolCapFFF, !- Speed 8 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
																 "  COOLEIRFT, !- Speed 8 Energy Input Ratio Function of Temperature Curve Name",
																 "  COOLEIRFFF, !- Speed 8 Energy Input Ratio Function of Air Flow Fraction Curve Name",
																 "  6144.8, !- Speed 9 Reference Unit Gross Rated Total Cooling Capacity{ w }",
																 "  0.75, !- Speed 9 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
																 "  4.0, !- Speed 9 Reference Unit Gross Rated Cooling COP{ dimensionless }",
																 "  0.3473184, !- Speed 9 Reference Unit Rated Air Flow Rate{ m3 / s }",
																 "  0.69, !- Speed 9 Reference Unit Condenser Air Flow Rate{ m3 / s }",
																 "  , !- Speed 9 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
																 "  CoolCapFT, !- Speed 9 Total Cooling Capacity Function of Temperature Curve Name",
																 "  CoolCapFFF, !- Speed 9 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
																 "  COOLEIRFT, !- Speed 9 Energy Input Ratio Function of Temperature Curve Name",
																 "  COOLEIRFFF, !- Speed 9 Energy Input Ratio Function of Air Flow Fraction Curve Name",
																 "  6758.0, !- Speed 10 Reference Unit Gross Rated Total Cooling Capacity{ w }",
																 "  0.75, !- Speed 10 Reference Unit Gross Rated Sensible Heat Ratio{ dimensionless }",
																 "  4.0, !- Speed 10 Reference Unit Gross Rated Cooling COP{ dimensionless }",
																 "  0.37752, !- Speed 10 Reference Unit Rated Air Flow Rate{ m3 / s }",
																 "  0.74, !- Speed 10 Reference Unit Condenser Air Flow Rate{ m3 / s }",
																 "  , !- Speed 10 Reference Unit Rated Pad Effectiveness of Evap Precooling{ dimensionless }",
																 "  CoolCapFT, !- Speed 10 Total Cooling Capacity Function of Temperature Curve Name",
																 "  CoolCapFFF, !- Speed 10 Total Cooling Capacity Function of Air Flow Fraction Curve Name",
																 "  COOLEIRFT, !- Speed 10 Energy Input Ratio Function of Temperature Curve Name",
																 "  COOLEIRFFF;          !- Speed 10 Energy Input Ratio Function of Air Flow Fraction Curve Name",
														 });

		ASSERT_TRUE( process_idf( idf_objects ) );

		std::string const CurrentModuleObject = "Coil:Cooling:DX:VariableSpeed";

		int num_coils = InputProcessor::GetNumObjectsFound( CurrentModuleObject );
		ASSERT_EQ( 1,  num_coils );

		int TotalArgs = 0;
		int NumAlphas = 0;
		int NumNumbers = 0;

		InputProcessor::GetObjectDefMaxArgs( CurrentModuleObject, TotalArgs, NumAlphas, NumNumbers );

		int IOStatus = 0;
		Array1D_string Alphas( NumAlphas );
		Array1D< Real64 > Numbers( NumNumbers, 0.0 );
		Array1D_bool lNumericBlanks( NumNumbers, true );
		Array1D_bool lAlphaBlanks( NumAlphas, true );
		Array1D_string cAlphaFields( NumAlphas );
		Array1D_string cNumericFields( NumNumbers );

		InputProcessor::GetObjectItem( CurrentModuleObject, num_coils, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

		EXPECT_EQ( 49, NumAlphas );
		EXPECT_TRUE( compare_containers( std::vector< std::string >( { "FURNACE ACDXCOIL 1", "DX COOLING COIL AIR INLET NODE", "HEATING COIL AIR INLET NODE", "PLFFPLR", "",
																	   "AIRCOOLED", "", "", "", "COOLCAPFT",
																	   "COOLCAPFFF", "COOLEIRFT", "COOLEIRFFF", "COOLCAPFT", "COOLCAPFFF",
																	   "COOLEIRFT", "COOLEIRFFF", "COOLCAPFT", "COOLCAPFFF", "COOLEIRFT",
																	   "COOLEIRFFF", "COOLCAPFT", "COOLCAPFFF", "COOLEIRFT", "COOLEIRFFF",
																	   "COOLCAPFT", "COOLCAPFFF", "COOLEIRFT", "COOLEIRFFF", "COOLCAPFT",
																	   "COOLCAPFFF", "COOLEIRFT", "COOLEIRFFF", "COOLCAPFT", "COOLCAPFFF",
																	   "COOLEIRFT", "COOLEIRFFF", "COOLCAPFT", "COOLCAPFFF", "COOLEIRFT",
																	   "COOLEIRFFF", "COOLCAPFT", "COOLCAPFFF", "COOLEIRFT", "COOLEIRFFF",
																	   "COOLCAPFT", "COOLCAPFFF", "COOLEIRFT", "COOLEIRFFF" } ), Alphas ) );
		EXPECT_TRUE( compare_containers( std::vector< bool >( { false, false, false, false, true, false, true, true, true, false,
																false, false, false, false, false, false, false, false, false, false,
																false, false, false, false, false, false, false, false, false, false,
																false, false, false, false, false, false, false, false, false, false,
																false, false, false, false, false, false, false, false, false } ), lAlphaBlanks ) );

		EXPECT_EQ( 71, NumNumbers );
		EXPECT_TRUE( compare_containers( std::vector< Real64 >( { 10.0, 10.0, 32000, 1.6, 0, 0, 0, 200, 10.0, 0,
																  2, 1524.1, .75, 4, 0.1359072, 0.26, 0, 1877.9, 0.75, 4.0,
																  0.151008, 0.30, 0, 2226.6, .75, 4.0, 0.1661088, 0.33, 0, 2911.3,
																  0.75, 4.0, 0.1963104, 0.38, 0, 3581.7, 0.75, 4.0, 0.226512, 0.44,
																  0, 4239.5, 0.75, 4.0, 0.2567136, 0.5, 0, 4885.7, 0.75, 4.0,
																  0.2869152, 0.57, 0, 5520.7, 0.75, 4.0, 0.31711680, 0.63, 0, 6144.8,
																  .75, 4.0, 0.3473184, 0.69, 0, 6758.0, 0.75, 4.0, 0.37752, 0.74, 0 } ), Numbers ) );
		EXPECT_TRUE( compare_containers( std::vector< bool >( { false, false, false, false, false, false, true, false, false, true, true,
																false, false, false, false, false, true,
																false, false, false, false, false, true,
																false, false, false, false, false, true,
																false, false, false, false, false, true,
																false, false, false, false, false, true,
																false, false, false, false, false, true,
																false, false, false, false, false, true,
																false, false, false, false, false, true,
																false, false, false, false, false, true,
																false, false, false, false, false, true } ), lNumericBlanks ) );
		EXPECT_EQ( 1, IOStatus );
	}

	TEST_F( InputProcessorFixture, getObjectItem_curve_biquadratic)
	{
		std::string const idf_objects = delimited_string({
																 "Curve:Biquadratic,",
																 "  CoolCapFT,        !- Name",
																 "  0.942587793,            !- Coefficient1 Constant",
																 "  0.009543347,            !- Coefficient2 x",
																 "  0.000683770,            !- Coefficient3 x**2",
																 "  -0.011042676,           !- Coefficient4 y",
																 "  0.000005249,            !- Coefficient5 y**2",
																 "  -0.000009720,           !- Coefficient6 x*y",
																 "  12.77778,               !- Minimum Value of x",
																 "  23.88889,               !- Maximum Value of x",
																 "  18.0,                   !- Minimum Value of y",
																 "  46.11111,               !- Maximum Value of y",
																 "  ,                       !- Minimum Curve Output",
																 "  ,                       !- Maximum Curve Output",
																 "  Temperature,            !- Input Unit Type for X",
																 "  Temperature,            !- Input Unit Type for Y",
																 "  Dimensionless;          !- Output Unit Type",
																 "  ",
																 "Curve:Biquadratic,",
																 "  COOLEIRFT,            !- Name",
																 "  0.342414409,            !- Coefficient1 Constant",
																 "  0.034885008,            !- Coefficient2 x",
																 "  -0.000623700,           !- Coefficient3 x**2",
																 "  0.004977216,            !- Coefficient4 y",
																 "  0.000437951,            !- Coefficient5 y**2",
																 "  -0.000728028,           !- Coefficient6 x*y",
																 "  12.77778,               !- Minimum Value of x",
																 "  23.88889,               !- Maximum Value of x",
																 "  18.0,                   !- Minimum Value of y",
																 "  46.11111,               !- Maximum Value of y",
																 "  ,                       !- Minimum Curve Output",
																 "  ,                       !- Maximum Curve Output",
																 "  Temperature,            !- Input Unit Type for X",
																 "  Temperature,            !- Input Unit Type for Y",
																 "  Dimensionless;          !- Output Unit Type",
														 });

		ASSERT_TRUE( process_idf( idf_objects ) );

		std::string const CurrentModuleObject = "Curve:Biquadratic";

		int num_curve_biquad = InputProcessor::GetNumObjectsFound( CurrentModuleObject );
		ASSERT_EQ( 2,  num_curve_biquad );

		int TotalArgs = 0;
		int NumAlphas = 0;
		int NumNumbers = 0;

		InputProcessor::GetObjectDefMaxArgs( CurrentModuleObject, TotalArgs, NumAlphas, NumNumbers );

		int IOStatus = 0;
		Array1D_string Alphas( NumAlphas );
		Array1D< Real64 > Numbers( NumNumbers, 0.0 );
		Array1D_bool lNumericBlanks( NumNumbers, true );
		Array1D_bool lAlphaBlanks( NumAlphas, true );
		Array1D_string cAlphaFields( NumAlphas );
		Array1D_string cNumericFields( NumNumbers );

		InputProcessor::GetObjectItem( CurrentModuleObject, 2, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

		EXPECT_EQ( 4, NumAlphas );
		EXPECT_TRUE( compare_containers( std::vector< std::string >( { "COOLCAPFT", "TEMPERATURE", "TEMPERATURE",
																	   "DIMENSIONLESS" } ), Alphas ) );
		EXPECT_TRUE( compare_containers( std::vector< bool >( { false, false, false, false } ), lAlphaBlanks ) );

		EXPECT_EQ( 12, NumNumbers );
		EXPECT_TRUE( compare_containers( std::vector< Real64 >( { 0.942587793, 0.009543347, 0.000683770, -0.011042676, 0.000005249,
																  -0.000009720, 12.77778, 23.88889, 18.0, 46.11111,
																  0, 0 } ), Numbers ) );
		EXPECT_TRUE( compare_containers( std::vector< bool >( { false, false, false, false, false, false,
																false, false, false, false, true, true } ), lNumericBlanks ) );


		int TotalArgs2 = 0;
		int NumAlphas2 = 0;
		int NumNumbers2 = 0;

		InputProcessor::GetObjectDefMaxArgs( CurrentModuleObject, TotalArgs2, NumAlphas2, NumNumbers2 );

		Array1D_string Alphas2( NumAlphas2 );
		Array1D< Real64 > Numbers2( NumNumbers2, 0.0 );
		Array1D_bool lNumericBlanks2( NumNumbers2, true );
		Array1D_bool lAlphaBlanks2( NumAlphas2, true );
		Array1D_string cAlphaFields2( NumAlphas2 );
		Array1D_string cNumericFields2( NumNumbers2 );

		InputProcessor::GetObjectItem( CurrentModuleObject, 1, Alphas2, NumAlphas2, Numbers2, NumNumbers2, IOStatus, lNumericBlanks2, lAlphaBlanks2, cAlphaFields2, cNumericFields2 );

		EXPECT_EQ( 4, NumAlphas2 );
		EXPECT_TRUE( compare_containers( std::vector< std::string >( { "COOLEIRFT", "TEMPERATURE", "TEMPERATURE",
																	   "DIMENSIONLESS" } ), Alphas2 ) );
		EXPECT_TRUE( compare_containers( std::vector< bool >( { false, false, false, false } ), lAlphaBlanks2 ) );

		EXPECT_EQ( 12, NumNumbers2 );
		EXPECT_TRUE( compare_containers( std::vector< Real64 >( { 0.342414409, 0.034885008, -0.000623700, 0.004977216, 0.000437951,
																  -0.000728028, 12.77778, 23.88889, 18.0, 46.11111,
																  0, 0 } ), Numbers2 ) );
		EXPECT_TRUE( compare_containers( std::vector< bool >( { false, false, false, false, false, false,
																false, false, false, false, true, true } ), lNumericBlanks2 ) );

		EXPECT_EQ( 1, IOStatus );
	}

	TEST_F( InputProcessorFixture, getObjectItem_curve_biquadratic2)
	{
		std::string const idf_objects = delimited_string({
																 "Curve:Biquadratic,",
																 "  HPACCoolCapFT Speed 1, !- Name",
																 "  1, !- Coefficient1 Constant",
																 "  0, !- Coefficient2 x",
																 "  0, !- Coefficient3 x**2",
																 "  0, !- Coefficient4 y",
																 "  0, !- Coefficient5 y**2",
																 "  0, !- Coefficient6 x*y",
																 "  0, !- Minimum Value of x",
																 "  0, !- Maximum Value of x",
																 "  0, !- Minimum Value of y",
																 "  46.11111, !- Maximum Value of y",
																 "  , !- Minimum Curve Output",
																 "  , !- Maximum Curve Output",
																 "  Temperature, !- Input Unit Type for X",
																 "  Temperature, !- Input Unit Type for Y",
																 "  Dimensionless;           !- Output Unit Type",
														 });

		ASSERT_TRUE( process_idf( idf_objects ) );

		std::string const CurrentModuleObject = "Curve:Biquadratic";

		int num_curve_biquad = InputProcessor::GetNumObjectsFound( CurrentModuleObject );
		ASSERT_EQ( 1,  num_curve_biquad );

		int TotalArgs = 0;
		int NumAlphas = 0;
		int NumNumbers = 0;

		InputProcessor::GetObjectDefMaxArgs( CurrentModuleObject, TotalArgs, NumAlphas, NumNumbers );

		int IOStatus = 0;
		Array1D_string Alphas( NumAlphas );
		Array1D< Real64 > Numbers( NumNumbers, 0.0 );
		Array1D_bool lNumericBlanks( NumNumbers, true );
		Array1D_bool lAlphaBlanks( NumAlphas, true );
		Array1D_string cAlphaFields( NumAlphas );
		Array1D_string cNumericFields( NumNumbers );

		InputProcessor::GetObjectItem( CurrentModuleObject, num_curve_biquad, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

		EXPECT_EQ( 4, NumAlphas );
		EXPECT_TRUE( compare_containers( std::vector< std::string >( { "HPACCOOLCAPFT SPEED 1", "TEMPERATURE", "TEMPERATURE",
																	   "DIMENSIONLESS" } ), Alphas ) );
		EXPECT_TRUE( compare_containers( std::vector< bool >( { false, false, false, false } ), lAlphaBlanks ) );

		EXPECT_EQ( 12, NumNumbers );
		EXPECT_TRUE( compare_containers( std::vector< Real64 >( { 1, 0, 0, 0, 0,
																  0, 0, 0, 0, 46.11111,
																  0, 0 } ), Numbers ) );
		EXPECT_TRUE( compare_containers( std::vector< bool >( { false, false, false, false, false, false,
																false, false, false, false, true, true } ), lNumericBlanks ) );
		EXPECT_EQ( 1, IOStatus );
	}

/*
   TEST_F( InputProcessorFixture, processIDF_json )
   {
      const json schema = initialize();
      InputProcessor IP;
      IdfParser idf_parser(schema);
      State state(schema);

      std::string const idf = delimited_string({
                                                             "Version,",
                                                             "8.3;",
                                                             "SimulationControl, NO, NO, NO, YES, YES;",
                                                       });

      json::parser_callback_t cb = [&state](int depth, json::parse_event_t event, json &parsed,
                                            unsigned line_num, unsigned line_index) -> bool {
        state.traverse(event, parsed, line_num, line_index);
        return true;
      };

      IP.jdf = idf_parser.decode(idf, schema);
      json::parse(IP.jdf.dump(2), cb);

      EXPECT_EQ(2, state.errors + state.warnings);

      // auto index = FindItemInSortedList( version_name, ListOfObjects, NumObjectDefs );
      // if ( index != 0 ) index = iListOfObjects( index );
      // index = ObjectStartRecord( index );
      // EXPECT_EQ( 1, index );

      json &loc = IP.jdf["properties"]["Version"];

      // EXPECT_EQ( "Version", IDFRecords( index ).Name );
      EXPECT_EQ(1, loc['alphas'].size());  // EXPECT_EQ( 1, IDFRecords( index ).NumAlphas )
      EXPECT_EQ(0, loc['numerics'].size());  // EXPECT_EQ( 0, IDFRecords( index ).NumNumbers );
      EXPECT_EQ(1, )  // EXPECT_EQ( 1, IDFRecords( index ).ObjectDefPtr );
      EXPECT_TRUE( compare_containers( std::vector< std::string >( { "8.5" } ), IDFRecords( index ).Alphas ) );
      EXPECT_TRUE( compare_containers( std::vector< bool >( { false } ), IDFRecords( index ).AlphBlank ) );
      EXPECT_TRUE( compare_containers( std::vector< Real64 >( {} ), IDFRecords( index ).Numbers ) );
      EXPECT_TRUE( compare_containers( std::vector< bool >( {} ), IDFRecords( index ).NumBlank ) );

      std::string const simulation_control_name( "SIMULATIONCONTROL" );

      index = FindItemInSortedList( simulation_control_name, ListOfObjects, NumObjectDefs );
      if ( index != 0 ) index = iListOfObjects( index );

      index = ObjectStartRecord( index );

      ASSERT_EQ( 2, index );

      EXPECT_EQ( simulation_control_name, IDFRecords( index ).Name );
      EXPECT_EQ( 5, IDFRecords( index ).NumAlphas );
      EXPECT_EQ( 0, IDFRecords( index ).NumNumbers );
      EXPECT_EQ( 2, IDFRecords( index ).ObjectDefPtr );
      EXPECT_TRUE( compare_containers( std::vector< std::string >( { "NO", "NO", "NO", "YES", "YES" } ), IDFRecords( index ).Alphas ) );
      EXPECT_TRUE( compare_containers( std::vector< bool >( { false, false, false, false, false } ), IDFRecords( index ).AlphBlank ) );
      EXPECT_TRUE( compare_containers( std::vector< Real64 >( {} ), IDFRecords( index ).Numbers ) );
      EXPECT_TRUE( compare_containers( std::vector< bool >( {} ), IDFRecords( index ).NumBlank ) );

   }
   */
}
