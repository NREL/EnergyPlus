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
#include <nlohmann/json.hpp>

namespace EnergyPlus {
   TEST_F( InputProcessorFixture, processIDF_json )
   {
      std::ifstream ifs("idd/FULL_SCHEMA_modified", std::ifstream::in);
      ASSERT_TRUE(ifs.is_open());
      const json schema = json::parse(ifs);
      InputProcessor IP;


      std::string const idf_objects = delimited_string({
                                                             "Version,",
                                                             "8.3;",
                                                             "SimulationControl, NO, NO, NO, YES, YES;",
                                                       });
//      ASSERT_FALSE( process_idf( idf_objects, true, false ) );
//      EXPECT_FALSE( OverallErrorFlag );
//      EXPECT_FALSE( has_cout_output() );
//      EXPECT_FALSE( has_cerr_output() );

      IP.idf_parser();

      std::string const version_name( "VERSION" );

      auto index = FindItemInSortedList( version_name, ListOfObjects, NumObjectDefs );
      if ( index != 0 ) index = iListOfObjects( index );

      index = ObjectStartRecord( index );

      EXPECT_EQ( 1, index );

      EXPECT_EQ( version_name, IDFRecords( index ).Name );
      EXPECT_EQ( 1, IDFRecords( index ).NumAlphas );
      EXPECT_EQ( 0, IDFRecords( index ).NumNumbers );
      EXPECT_EQ( 1, IDFRecords( index ).ObjectDefPtr );
      EXPECT_TRUE( compare_containers( std::vector< std::string >( { "8.3" } ), IDFRecords( index ).Alphas ) );
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
}