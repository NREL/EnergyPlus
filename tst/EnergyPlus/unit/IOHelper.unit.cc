// EnergyPlusFixture Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"

namespace EnergyPlus {

	typedef EnergyPlusFixture IOHelperFixture;
	typedef EnergyPlusFixture IOHelperDeathTestFixture;

	TEST_F( IOHelperFixture, compareIDF )
	{
		std::string const idf_objects = delimited_string({
			"Version,8.3;",
			"	 BUILDING, Bldg2, 0.0, Suburbs, .04, .4, FullExterior, 25, 6;",
			"Output:SQLite,SimpleAndTabular;",
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

		ASSERT_FALSE( process_idf( idf_objects ) );

		EXPECT_FALSE( compare_idf( "VERSION", 1, 0, 1, { "8.3" }, { false }, {}, {} ) );

		EXPECT_FALSE( compare_idf( "OUTPUT:SQLITE", 1, 0, 739, { "SIMPLEANDTABULAR" }, { false }, {}, {} ) );

		EXPECT_FALSE( compare_idf( 
			"BUILDINGSURFACE:DETAILED", 
			8, 
			14, 
			98, 
			{ "ZN001:WALL001", "WALL", "R13WALL", "MAIN ZONE", "OUTDOORS", "", "SUNEXPOSED", "WINDEXPOSED" }, 
			{ false, false, false, false, false, true, false, false }, 
			{ 0.5, 4, 0, 0, 4.572, 0, 0, 0, 15.24, 0, 0, 15.24, 0, 4.572 }, 
			{ false, false, false, false, false, false, false, false, false, false, false, false, false, false }
		) );

	}

	TEST_F( IOHelperFixture, processIDD_Full_IDD )
	{
		using namespace InputProcessor;
		std::string const idd_objects;

		bool errors_found = false;

		ASSERT_FALSE( IOHelper::process_idd( idd_objects, errors_found ) );

		EXPECT_EQ( 745, NumObjectDefs );
		ASSERT_EQ( static_cast<unsigned long>( NumObjectDefs ), ListOfObjects.size() );

		std::string const name( "OUTPUT:SQLITE" );

		auto index = FindItemInSortedList( name, ListOfObjects, NumObjectDefs );
		if ( index != 0 ) index = iListOfObjects( index );

		EXPECT_EQ( name, ObjectDef( index ).Name );
		EXPECT_EQ( 1, ObjectDef( index ).NumParams );
		EXPECT_EQ( 1, ObjectDef( index ).NumAlpha );
		EXPECT_EQ( 0, ObjectDef( index ).NumNumeric );
		EXPECT_EQ( 0, ObjectDef( index ).MinNumFields );
		EXPECT_FALSE( ObjectDef( index ).NameAlpha1 );
		EXPECT_TRUE( ObjectDef( index ).UniqueObject );
		EXPECT_FALSE( ObjectDef( index ).RequiredObject );
		EXPECT_FALSE( ObjectDef( index ).ExtensibleObject );
		EXPECT_EQ( 0, ObjectDef( index ).ExtensibleNum );
		EXPECT_EQ( 0, ObjectDef( index ).LastExtendAlpha );
		EXPECT_EQ( 0, ObjectDef( index ).LastExtendNum );
		EXPECT_EQ( 0, ObjectDef( index ).ObsPtr );
		EXPECT_TRUE( compare_containers< std::vector< bool > >( { true } , ObjectDef( index ).AlphaOrNumeric ) );
		EXPECT_TRUE( compare_containers< std::vector< bool > >( { false } , ObjectDef( index ).ReqField ) );
		EXPECT_TRUE( compare_containers< std::vector< bool > >( { false } , ObjectDef( index ).AlphRetainCase ) );
		EXPECT_TRUE( compare_containers< std::vector< std::string > >( { "Option Type" }, ObjectDef( index ).AlphFieldChks ) );
		EXPECT_TRUE( compare_containers< std::vector< std::string > >( { "" }, ObjectDef( index ).AlphFieldDefs ) );
		// EXPECT_TRUE( compare_containers< Array1D< RangeCheckDef > >( { RangeCheckDef() }, ObjectDef( index ).NumRangeChks ) );
		EXPECT_EQ( 0, ObjectDef( index ).NumFound );

	}

	TEST_F( IOHelperFixture, processIDD )
	{
		using namespace InputProcessor;
		std::string const idd_objects = delimited_string({
			"Output:SQLite,",
			"       \\memo Output from EnergyPlus can be written to an SQLite format file.",
			"       \\unique-object",
			"  A1 ; \\field Option Type",
			"       \\type choice",
			"       \\key Simple",
			"       \\key SimpleAndTabular"
		});

		bool errors_found = false;

		ASSERT_FALSE( IOHelper::process_idd( idd_objects, errors_found ) );

		EXPECT_EQ( 1, NumObjectDefs );

		EXPECT_EQ( "OUTPUT:SQLITE", ObjectDef( 1 ).Name );
		EXPECT_EQ( 1, ObjectDef( 1 ).NumParams );
		EXPECT_EQ( 1, ObjectDef( 1 ).NumAlpha );
		EXPECT_EQ( 0, ObjectDef( 1 ).NumNumeric );
		EXPECT_EQ( 0, ObjectDef( 1 ).MinNumFields );
		EXPECT_FALSE( ObjectDef( 1 ).NameAlpha1 );
		EXPECT_TRUE( ObjectDef( 1 ).UniqueObject );
		EXPECT_FALSE( ObjectDef( 1 ).RequiredObject );
		EXPECT_FALSE( ObjectDef( 1 ).ExtensibleObject );
		EXPECT_EQ( 0, ObjectDef( 1 ).ExtensibleNum );
		EXPECT_EQ( 0, ObjectDef( 1 ).LastExtendAlpha );
		EXPECT_EQ( 0, ObjectDef( 1 ).LastExtendNum );
		EXPECT_EQ( 0, ObjectDef( 1 ).ObsPtr );
		EXPECT_TRUE( compare_containers< std::vector< bool > >( { true } , ObjectDef( 1 ).AlphaOrNumeric ) );
		EXPECT_TRUE( compare_containers< std::vector< bool > >( { false } , ObjectDef( 1 ).ReqField ) );
		EXPECT_TRUE( compare_containers< std::vector< bool > >( { false } , ObjectDef( 1 ).AlphRetainCase ) );
		EXPECT_TRUE( compare_containers< std::vector< std::string > >( { "Option Type" }, ObjectDef( 1 ).AlphFieldChks ) );
		EXPECT_TRUE( compare_containers< std::vector< std::string > >( { "" }, ObjectDef( 1 ).AlphFieldDefs ) );
		// EXPECT_TRUE( compare_containers< Array1D< RangeCheckDef > >( { RangeCheckDef() }, ObjectDef( 1 ).NumRangeChks ) );
		EXPECT_EQ( 0, ObjectDef( 1 ).NumFound );

	}

	TEST_F( IOHelperFixture, processIDF )
	{
		using namespace InputProcessor;
		std::string const idf_objects = delimited_string({
			"Version,",
			"8.3;",
			"Output:SQLite,",
			"SimpleAndTabular;"
		});

		ASSERT_FALSE( process_idf( idf_objects, false ) );

		EXPECT_FALSE( OverallErrorFlag );

		EXPECT_FALSE( has_cout_output() );
		EXPECT_FALSE( has_cerr_output() );

		std::string const version_name( "VERSION" );

		auto index = FindItemInSortedList( version_name, ListOfObjects, NumObjectDefs );
		if ( index != 0 ) index = iListOfObjects( index );

		index = ObjectStartRecord( index );

		EXPECT_EQ( 1, index );

		EXPECT_EQ( version_name, IDFRecords( index ).Name );
		EXPECT_EQ( 1, IDFRecords( index ).NumAlphas );
		EXPECT_EQ( 0, IDFRecords( index ).NumNumbers );
		EXPECT_EQ( 1, IDFRecords( index ).ObjectDefPtr );
		EXPECT_TRUE( compare_containers< std::vector< std::string > >( { "8.3" }, IDFRecords( index ).Alphas ) );
		EXPECT_TRUE( compare_containers< std::vector< bool > >( { false }, IDFRecords( index ).AlphBlank ) );
		EXPECT_TRUE( compare_containers< std::vector< Real64 > >( {}, IDFRecords( index ).Numbers ) );
		EXPECT_TRUE( compare_containers< std::vector< bool > >( {}, IDFRecords( index ).NumBlank ) );

		std::string const sqlite_name( "OUTPUT:SQLITE" );

		index = FindItemInSortedList( sqlite_name, ListOfObjects, NumObjectDefs );
		if ( index != 0 ) index = iListOfObjects( index );

		index = ObjectStartRecord( index );

		EXPECT_EQ( 2, index );

		EXPECT_EQ( sqlite_name, IDFRecords( index ).Name );
		EXPECT_EQ( 1, IDFRecords( index ).NumAlphas );
		EXPECT_EQ( 0, IDFRecords( index ).NumNumbers );
		EXPECT_EQ( 739, IDFRecords( index ).ObjectDefPtr );
		EXPECT_TRUE( compare_containers< std::vector< std::string > >( { "SIMPLEANDTABULAR" }, IDFRecords( index ).Alphas ) );
		EXPECT_TRUE( compare_containers< std::vector< bool > >( { false }, IDFRecords( index ).AlphBlank ) );
		EXPECT_TRUE( compare_containers< std::vector< Real64 > >( {}, IDFRecords( index ).Numbers ) );
		EXPECT_TRUE( compare_containers< std::vector< bool > >( {}, IDFRecords( index ).NumBlank ) );

	}

	TEST_F( IOHelperFixture, processIDF_Cached )
	{
		using namespace InputProcessor;
		std::string const idf_objects = delimited_string({
			"Version,8.3;",
			"Output:SQLite,SimpleAndTabular;",
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

		ASSERT_FALSE( process_idf( idf_objects ) );

		EXPECT_FALSE( OverallErrorFlag );

		EXPECT_FALSE( has_cout_output() );
		EXPECT_FALSE( has_cerr_output() );

		std::string const version_name( "VERSION" );

		auto index = FindItemInSortedList( version_name, ListOfObjects, NumObjectDefs );
		if ( index != 0 ) index = iListOfObjects( index );

		index = ObjectStartRecord( index );

		EXPECT_EQ( 1, index );

		EXPECT_EQ( version_name, IDFRecords( index ).Name );
		EXPECT_EQ( 1, IDFRecords( index ).NumAlphas );
		EXPECT_EQ( 0, IDFRecords( index ).NumNumbers );
		EXPECT_EQ( 1, IDFRecords( index ).ObjectDefPtr );
		EXPECT_TRUE( compare_containers< std::vector< std::string > >( { "8.3" }, IDFRecords( index ).Alphas ) );
		EXPECT_TRUE( compare_containers< std::vector< bool > >( { false }, IDFRecords( index ).AlphBlank ) );
		EXPECT_TRUE( compare_containers< std::vector< Real64 > >( {}, IDFRecords( index ).Numbers ) );
		EXPECT_TRUE( compare_containers< std::vector< bool > >( {}, IDFRecords( index ).NumBlank ) );

		std::string const sqlite_name( "OUTPUT:SQLITE" );

		index = FindItemInSortedList( sqlite_name, ListOfObjects, NumObjectDefs );
		if ( index != 0 ) index = iListOfObjects( index );

		index = ObjectStartRecord( index );

		EXPECT_EQ( 2, index );

		EXPECT_EQ( sqlite_name, IDFRecords( index ).Name );
		EXPECT_EQ( 1, IDFRecords( index ).NumAlphas );
		EXPECT_EQ( 0, IDFRecords( index ).NumNumbers );
		EXPECT_EQ( 739, IDFRecords( index ).ObjectDefPtr );
		EXPECT_TRUE( compare_containers< std::vector< std::string > >( { "SIMPLEANDTABULAR" }, IDFRecords( index ).Alphas ) );
		EXPECT_TRUE( compare_containers< std::vector< bool > >( { false }, IDFRecords( index ).AlphBlank ) );
		EXPECT_TRUE( compare_containers< std::vector< Real64 > >( {}, IDFRecords( index ).Numbers ) );
		EXPECT_TRUE( compare_containers< std::vector< bool > >( {}, IDFRecords( index ).NumBlank ) );

		std::string const building_surface_detailed_name( "BUILDINGSURFACE:DETAILED" );

		index = FindItemInSortedList( building_surface_detailed_name, ListOfObjects, NumObjectDefs );
		if ( index != 0 ) index = iListOfObjects( index );

		index = ObjectStartRecord( index );

		EXPECT_EQ( 3, index );

		EXPECT_EQ( building_surface_detailed_name, IDFRecords( index ).Name );
		EXPECT_EQ( 8, IDFRecords( index ).NumAlphas );
		EXPECT_EQ( 14, IDFRecords( index ).NumNumbers );
		EXPECT_EQ( 98, IDFRecords( index ).ObjectDefPtr );
		EXPECT_TRUE( compare_containers< std::vector< std::string > >( { "ZN001:WALL001", "WALL", "R13WALL", "MAIN ZONE", "OUTDOORS", "", "SUNEXPOSED", "WINDEXPOSED" }, IDFRecords( index ).Alphas ) );
		EXPECT_TRUE( compare_containers< std::vector< bool > >( { false, false, false, false, false, true, false, false }, IDFRecords( index ).AlphBlank ) );
		EXPECT_TRUE( compare_containers< std::vector< Real64 > >( { 0.5, 4, 0, 0, 4.572, 0, 0, 0, 15.24, 0, 0, 15.24, 0, 4.572 }, IDFRecords( index ).Numbers ) );
		EXPECT_TRUE( compare_containers< std::vector< bool > >( { false, false, false, false, false, false, false, false, false, false, false, false, false, false }, IDFRecords( index ).NumBlank ) );

	}

}
