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

// const json initialize() {
//        std::ifstream ifs("idd/FULL_SCHEMA_modified", std::ifstream::in);
//       ASSERT_TRUE(ifs.is_open());
//       const json schema = json::parse(ifs);
//       return schema;
// }

namespace EnergyPlus {


    TEST_F( InputProcessorFixture, getObjectItem_json1 )
    {
      std::string const idf_objects = delimited_string({
        "Version,8.3;",
        "Output:SQLite,SimpleAndTabular;",
      });

      // ASSERT_FALSE( process_idf( idf_objects ) );
			InputProcessor IP;
			std::ifstream ifs("FULL_SCHEMA_modified.json", std::ifstream::in);
			ASSERT_TRUE( ifs.is_open() );
			IP.schema = json::parse(ifs);
			IP.idf_parser.initialize(IP.schema);
//      json test = IP.idf_parser.decode(idf_objects, IP.schema);
			InputProcessor::jdf = IP.idf_parser.decode(idf_objects, IP.schema);

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

      InputProcessor IP;
      std::ifstream ifs("FULL_SCHEMA_modified.json", std::ifstream::in);
      ASSERT_TRUE( ifs.is_open() );
      IP.schema = json::parse(ifs);
      IP.idf_parser.initialize(IP.schema);
//      json test = IP.idf_parser.decode(idf_objects, IP.schema);
      InputProcessor::jdf = IP.idf_parser.decode(idf_objects, IP.schema);


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
      EXPECT_EQ( 6, NumAlphas ); // TODO: Should be 6, why is it 7? Might be due to name field
//		  EXPECT_EQ( 7, NumAlphas );
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

     InputProcessor IP;
     std::ifstream ifs("FULL_SCHEMA_modified.json", std::ifstream::in);
     ASSERT_TRUE(ifs.is_open());
     IP.schema = json::parse(ifs);
     IP.idf_parser.initialize(IP.schema);
//      json test = IP.idf_parser.decode(idf_objects, IP.schema);
     InputProcessor::jdf = IP.idf_parser.decode(idf_objects, IP.schema);

//     std::ofstream debug_ofs("test_json_dump.json", std::ofstream::out);
//     debug_ofs << InputProcessor::jdf.dump(4) << std::endl;

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
