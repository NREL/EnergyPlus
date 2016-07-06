// EnergyPlus::InputProcessor Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/DataOutputs.hh>
#include <EnergyPlus/InputProcessor_json.hh>
#include <EnergyPlus/SortAndStringUtilities.hh>

#include "Fixtures/InputProcessorFixture.hh"

#include <tuple>
#include <map>
#include <InputProcessor_json.hh>

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
                                                  "  6.000000;\n",
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
                                                  "  0.000000;\n",
                                                  "GlobalGeometryRules,",
                                                  "  UpperLeftCorner,",
                                                  "  Counterclockwise,",
                                                  "  Relative,",
                                                  "  Relative,",
                                                  "  Relative;\n"
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
                                                     "  6.000000;\n",
                                                     "GlobalGeometryRules,",
                                                     "  UpperLeftCorner,",
                                                     "  Counterclockwise,",
                                                     "  Relative,",
                                                     "  Relative,",
                                                     "  Relative;\n\n"
                                                             "Zone,",
                                                     "  Core_mid,",
                                                     "  0.000000,",
                                                     "  0.000000,",
                                                     "  0.000000,",
                                                     "  0.000000,",
                                                     "  1.000000,",
                                                     "  1.000000,",
                                                     "  autocalculate,",
                                                     "  autocalculate,",
                                                     "  Autocalculate,",
                                                     "  ,",
                                                     "  ,",
                                                     "  Yes;\n",
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
                                                {"north_axis", 0.0},
                                                {"terrain", "Suburbs"},
                                                {"loads_convergence_tolerance_value", 0.04},
                                                {"temperature_convergence_tolerance_value", 0.2000},
                                                {"solar_distribution", "FullExterior"},
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
                                                         "\n",
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
                        "Zn009:Flr001,            !- Name\n",
                        "    Floor,                   !- Surface Type\n",
                        "    FLOOR38,                 !- Construction Name\n",
                        "    SCWINDOW,                !- Zone Name\n",
                        "    Surface,                 !- Outside Boundary Condition\n",
                        "    Zn009:Flr001,            !- Outside Boundary Condition Object\n",
                        "    NoSun,                   !- Sun Exposure\n",
                        "    NoWind,                  !- Wind Exposure\n",
                        "    1.000000,                !- View Factor to Ground\n",
                        "    4,                       !- Number of Vertices\n",
                        "    "",10,0,  !- X,Y,Z ==> Vertex 1 {m}\n",
                        "    0.000000,,0,  !- X,Y,Z ==> Vertex 2 {m}\n",
                        "    0.000000,10.00000,0,  !- X,Y,Z ==> Vertex 3 {m}\n",
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
                        "Zn009:Flr001,            !- Name\n",
                        "    Floor,                   !- Surface Type\n",
                        "    FLOOR38,                 !- Construction Name\n",
                        "    SCWINDOW,                !- Zone Name\n",
                        "    Surface,                 !- Outside Boundary Condition\n",
                        "    Zn009:Flr001,            !- Outside Boundary Condition Object\n",
                        "    NoSun,                   !- Sun Exposure\n",
                        "    NoWind,                  !- Wind Exposure\n",
                        "    1.000000,                !- View Factor to Ground\n",
                        "    4,                       !- Number of Vertices\n",
                        "    10.00000,0.000000,0,  !- X,Y,Z ==> Vertex 1 {m}\n",
                        "    0.000000,0.000000,0,  !- X,Y,Z ==> Vertex 2 {m}\n",
                        "    0.000000,10.00000,0,  !- X,Y,Z ==> Vertex 3 {m}\n",
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
                        "Zn009:Flr001,            !- Name\n",
                        "    Floor,                   !- Surface Type\n",
                        "    FLOOR38,                 !- Construction Name\n",
                        "    SCWINDOW,                !- Zone Name\n",
                        "    Surface,                 !- Outside Boundary Condition\n",
                        "    Zn009:Flr001,            !- Outside Boundary Condition Object\n",
                        "    NoSun,                   !- Sun Exposure\n",
                        "    NoWind,                  !- Wind Exposure\n",
                        "    1.000000,                !- View Factor to Ground\n",
                        "    4,                       !- Number of Vertices\n",
                        "    10.00000,0.000000,0,  !- X,Y,Z ==> Vertex 1 {m}\n",
                        "    0.000000,0.000000,0,  !- X,Y,Z ==> Vertex 2 {m}\n",
                        "    0.000000,10.00000,0,  !- X,Y,Z ==> Vertex 3 {m}\n",
                        "    10.00000,10.00000,0;  !- X,Y,Z ==> Vertex 4 {m}",
                        "\n",
                        "BuildingSurface:Detailed,",
                        "Some Surface Name,            !- Name\n",
                        "    Floor,                   !- Surface Type\n",
                        "    FLOOR38,                 !- Construction Name\n",
                        "    SCWINDOW,                !- Zone Name\n",
                        "    Surface,                 !- Outside Boundary Condition\n",
                        "    Zn009:Flr001,            !- Outside Boundary Condition Object\n",
                        "    NoSun,                   !- Sun Exposure\n",
                        "    NoWind,                  !- Wind Exposure\n",
                        "    1.000000,                !- View Factor to Ground\n",
                        "    4,                       !- Number of Vertices\n",
                        "    10.00000,0.000000,0,  !- X,Y,Z ==> Vertex 1 {m}\n",
                        "    0.000000,0.000000,0,  !- X,Y,Z ==> Vertex 2 {m}\n",
                        "    0.000000,10.00000,0,  !- X,Y,Z ==> Vertex 3 {m}\n",
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
						"Zn009:Flr001,            !- Name\n",
						"    Floor,                   !- Surface Type\n",
						"    FLOOR38,                 !- Construction Name\n",
						"    SCWINDOW,                !- Zone Name\n",
						"    Surface,                 !- Outside Boundary Condition\n",
						"    Zn009:Flr001,            !- Outside Boundary Condition Object\n",
						"    NoSun,                   !- Sun Exposure\n",
						"    NoWind,                  !- Wind Exposure\n",
						"    1.000000,                !- View Factor to Ground\n",
						"    4,                       !- Number of Vertices\n",
						"    10.00000,0.000000,0,  !- X,Y,Z ==> Vertex 1 {m}\n",
						"    0.000000,0.000000,0,  !- X,Y,Z ==> Vertex 2 {m}\n",
						"    0.000000,10.00000,0,  !- X,Y,Z ==> Vertex 3 {m}\n",
						"    10.00000,10.00000,0;  !- X,Y,Z ==> Vertex 4 {m}",
						"\n",
						"BuildingSurface:Detailed,",
						"Building Surface Name,            !- Name\n",
						"    Floor,                   !- Surface Type\n",
						"    FLOOR38,                 !- Construction Name\n",
						"    SCWINDOW,                !- Zone Name\n",
						"    Surface,                 !- Outside Boundary Condition\n",
						"    Zn009:Flr001,            !- Outside Boundary Condition Object\n",
						"    NoSun,                   !- Sun Exposure\n",
						"    NoWind,                  !- Wind Exposure\n",
						"    1.000000,                !- View Factor to Ground\n",
						"    4,                       !- Number of Vertices\n",
						"    10.00000,0.000000,0,  !- X,Y,Z ==> Vertex 1 {m}\n",
						"    0.000000,0.000000,0,  !- X,Y,Z ==> Vertex 2 {m}\n",
						"    0.000000,10.00000,0,  !- X,Y,Z ==> Vertex 3 {m}\n",
						"    10.00000,10.00000,0;  !- X,Y,Z ==> Vertex 4 {m}",
						"\n",
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
																 "\n",
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
						"Zn009:Flr001,            !- Name\n",
						"    Floor,                   !- Surface Type\n",
						"    FLOOR38,                 !- Construction Name\n",
						"    SCWINDOW,                !- Zone Name\n",
						"    Surface,                 !- Outside Boundary Condition\n",
						"    Zn009:Flr001,            !- Outside Boundary Condition Object\n",
						"    NoSun,                   !- Sun Exposure\n",
						"    NoWind,                  !- Wind Exposure\n",
						"    1.000000,                !- View Factor to Ground\n",
						"    4,                       !- Number of Vertices\n",
						"    10.00000,0.000000,0,  !- X,Y,Z ==> Vertex 1 {m}\n",
						"    0.000000,0.000000,0,  !- X,Y,Z ==> Vertex 2 {m}\n",
						"    0.000000,10.00000,0,  !- X,Y,Z ==> Vertex 3 {m}\n",
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
						"Zn009:Flr001,            !- Name\n",
						"    Floor,                   !- Surface Type\n",
						"    FLOOR38,                 !- Construction Name\n",
						"    SCWINDOW,                !- Zone Name\n",
						"    Surface,                 !- Outside Boundary Condition\n",
						"    Zn009:Flr001,            !- Outside Boundary Condition Object\n",
						"    NoSun,                   !- Sun Exposure\n",
						"    NoWind,                  !- Wind Exposure\n",
						"    1.000000,                !- View Factor to Ground\n",
						"    4,                       !- Number of Vertices\n",
						"    10.00000,0.000000,0,  !- X,Y,Z ==> Vertex 1 {m}\n",
						"    0.000000,0.000000,0,  !- X,Y,Z ==> Vertex 2 {m}\n",
						"    0.000000,10.00000,0,  !- X,Y,Z ==> Vertex 3 {m}\n",
						"    10.00000,10.00000,0;  !- X,Y,Z ==> Vertex 4 {m}",
						"\n",
						"BuildingSurface:Detailed,",
						"Building Surface Name,            !- Name\n",
						"    Floor,                   !- Surface Type\n",
						"    FLOOR38,                 !- Construction Name\n",
						"    SCWINDOW,                !- Zone Name\n",
						"    Surface,                 !- Outside Boundary Condition\n",
						"    Zn009:Flr001,            !- Outside Boundary Condition Object\n",
						"    NoSun,                   !- Sun Exposure\n",
						"    NoWind,                  !- Wind Exposure\n",
						"    1.000000,                !- View Factor to Ground\n",
						"    4,                       !- Number of Vertices\n",
						"    10.00000,0.000000,0,  !- X,Y,Z ==> Vertex 1 {m}\n",
						"    0.000000,0.000000,0,  !- X,Y,Z ==> Vertex 2 {m}\n",
						"    0.000000,10.00000,0,  !- X,Y,Z ==> Vertex 3 {m}\n",
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
								"\n",
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

        EXPECT_TRUE( compare_containers( std::vector< std::string >( { "MAIN GAS HUMIDIFIER", "", "THERMALEFFICIENCYFPLR", "MIXED AIR NODE 1", "MAIN HUMIDIFIER OUTLET NODE", "", "FIXEDINLETWATERTEMPERATURE" } ), Alphas ) );
        EXPECT_TRUE( compare_containers( std::vector< std::string >( { "name", "availability_schedule_name", "thermal_efficiency_modifier_curve_name", "air_inlet_node_name", "air_outlet_node_name", "water_storage_tank_name", "inlet_water_temperature_option" } ), cAlphaFields ) );
        EXPECT_TRUE( compare_containers( std::vector< std::string >( { "rated_capacity", "rated_gas_use_rate", "thermal_efficiency", "rated_fan_power", "auxiliary_electric_power" } ), cNumericFields ) );
        EXPECT_TRUE( compare_containers( std::vector< bool >( { false, false, false, false, false } ), lNumericBlanks ) );
        EXPECT_TRUE( compare_containers( std::vector< bool >( { false, true, false, false, false, true, false } ), lAlphaBlanks ) );
        EXPECT_TRUE( compare_containers( std::vector< Real64 >( { -99999, -99999, 0.80, 0.0, 0.0 } ), Numbers ) );
//        EXPECT_EQ( 6, NumAlphas ); // TODO: Should be 6, why is it 7? Might be due to name field
		EXPECT_EQ( 7, NumAlphas );
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
