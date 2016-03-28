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

	namespace InputProcessor {

		TEST_F( InputProcessorFixture, compareIDF )
		{
			std::string const idf_objects = delimited_string({
				"Version,8.3;",
				"	 BUILDING, Bldg2, 0.0, Suburbs, .04, .4, FullExterior, 25, 6;",
				"SimulationControl, NO, NO, NO, YES, YES;",
				"  Schedule:Compact,",
				"    ACTIVITY_SCH,            !- Name",
				"    Any Number,              !- Schedule Type Limits Name",
				"    Through: 12/31,          !- Field 1",
				"    For: AllDays,            !- Field 2",
				"    Until: 24:00,120.;       !- Field 3",
				"  Site:Location,",
				"    USA IL-CHICAGO-OHARE,    !- Name",
				"    41.77,                   !- Latitude {deg}",
				"    -87.75,                  !- Longitude {deg}",
				"    -6.00,                   !- Time Zone {hr}",
				"    190;                     !- Elevation {m}",
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

			EXPECT_TRUE( compare_idf( "VERSION", 1, 0, { "8.3" }, { false }, {}, {} ) );

			EXPECT_TRUE( compare_idf( "SIMULATIONCONTROL", 5, 0, { "NO", "NO", "NO", "YES", "YES" }, { false, false, false, false, false }, {}, {} ) );

			EXPECT_TRUE( compare_idf( "SCHEDULE:COMPACT", 6, 0, { "ACTIVITY_SCH", "ANY NUMBER", "THROUGH: 12/31", "FOR: ALLDAYS", "UNTIL: 24:00", "120." }, { false, false, false, false, false, false }, { }, { } ) );

			EXPECT_TRUE( compare_idf( "SITE:LOCATION", 1, 4, { "USA IL-CHICAGO-OHARE" }, { false }, { 41.77, -87.75, -6.0, 190 }, { false, false, false, false } ) );

			EXPECT_TRUE( compare_idf(
				"BUILDINGSURFACE:DETAILED",
				8,
				14,
				{ "ZN001:WALL001", "WALL", "R13WALL", "MAIN ZONE", "OUTDOORS", "", "SUNEXPOSED", "WINDEXPOSED" },
				{ false, false, false, false, false, true, false, false },
				{ 0.5, 4, 0, 0, 4.572, 0, 0, 0, 15.24, 0, 0, 15.24, 0, 4.572 },
				{ false, false, false, false, false, false, false, false, false, false, false, false, false, false }
			) );

		}

		TEST_F(InputProcessorFixture, MisleadingIDDWarningTest)
		{
			bool errors_found = false;

			std::string const idf_contents = delimited_string({
				"Timestep, 4;"
			});

			ASSERT_FALSE( process_idf( idf_contents, errors_found ) );
			EXPECT_TRUE( compare_err_stream( "", true ) );

			std::string saveMatchVersion = DataStringGlobals::MatchVersion;
			DataStringGlobals::MatchVersion = "2.0";
			ASSERT_FALSE(process_idf(idf_contents, errors_found ));

			std::string const error_string = delimited_string( {
				"   ** Severe  ** IP: Possible incorrect IDD File",
				"   **   ~~~   ** " + DataStringGlobals::IDDVerString + " not the same as expected =\"2.0\""
			} );
			EXPECT_TRUE( compare_err_stream( error_string, true ) );

			DataStringGlobals::MatchVersion = saveMatchVersion;
		}

		TEST_F( InputProcessorFixture, processIDD_Full_IDD )
		{
			using namespace InputProcessor;
			std::string const idd_objects;

			bool errors_found = false;

			ASSERT_FALSE( process_idd( idd_objects, errors_found ) );

			// EXPECT_EQ( 745, NumObjectDefs ) << "If not equal, probably added or removed IDD object (change expected value).";
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
			EXPECT_TRUE( compare_containers( std::vector< bool >( { true } ), ObjectDef( index ).AlphaOrNumeric ) );
			EXPECT_TRUE( compare_containers( std::vector< bool >( { false } ), ObjectDef( index ).ReqField ) );
			EXPECT_TRUE( compare_containers( std::vector< bool >( { false } ), ObjectDef( index ).AlphRetainCase ) );
			EXPECT_TRUE( compare_containers( std::vector< std::string >( { "Option Type" } ), ObjectDef( index ).AlphFieldChks ) );
			EXPECT_TRUE( compare_containers( std::vector< std::string >( { "" } ), ObjectDef( index ).AlphFieldDefs ) );
			// EXPECT_TRUE( compare_containers( Array1D< RangeCheckDef >( { RangeCheckDef() } ), ObjectDef( index ).NumRangeChks ) );
			EXPECT_EQ( 0, ObjectDef( index ).NumFound );

		}

		TEST_F( InputProcessorFixture, processIDD )
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

			ASSERT_FALSE( process_idd( idd_objects, errors_found ) );

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
			EXPECT_TRUE( compare_containers( std::vector< bool >( { true } ), ObjectDef( 1 ).AlphaOrNumeric ) );
			EXPECT_TRUE( compare_containers( std::vector< bool >( { false } ), ObjectDef( 1 ).ReqField ) );
			EXPECT_TRUE( compare_containers( std::vector< bool >( { false } ), ObjectDef( 1 ).AlphRetainCase ) );
			EXPECT_TRUE( compare_containers( std::vector< std::string >( { "Option Type" } ), ObjectDef( 1 ).AlphFieldChks ) );
			EXPECT_TRUE( compare_containers( std::vector< std::string >( { "" } ), ObjectDef( 1 ).AlphFieldDefs ) );
			// EXPECT_TRUE( compare_containers( Array1D< RangeCheckDef >( { RangeCheckDef() } ), ObjectDef( 1 ).NumRangeChks ) );
			EXPECT_EQ( 0, ObjectDef( 1 ).NumFound );

		}

		TEST_F( InputProcessorFixture, processIDF )
		{
			using namespace InputProcessor;
			std::string const idf_objects = delimited_string({
				"Version,",
				"8.3;",
				"SimulationControl, NO, NO, NO, YES, YES;",
			});

			ASSERT_FALSE( process_idf( idf_objects, true, false ) );

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

		TEST_F( InputProcessorFixture, processIDF_Cached )
		{
			using namespace InputProcessor;
			std::string const idf_objects = delimited_string({
				"Version,8.3;",
				"SimulationControl, NO, NO, NO, YES, YES;",
				"  Schedule:Compact,",
				"    ACTIVITY_SCH,            !- Name",
				"    Any Number,              !- Schedule Type Limits Name",
				"    Through: 12/31,          !- Field 1",
				"    For: AllDays,            !- Field 2",
				"    Until: 24:00,120.;       !- Field 3",
				"  Site:Location,",
				"    USA IL-CHICAGO-OHARE,    !- Name",
				"    41.77,                   !- Latitude {deg}",
				"    -87.75,                  !- Longitude {deg}",
				"    -6.00,                   !- Time Zone {hr}",
				"    190;                     !- Elevation {m}",
			});

			ASSERT_FALSE( process_idf( idf_objects ) );

			EXPECT_FALSE( OverallErrorFlag );

			EXPECT_FALSE( has_cout_output() );
			EXPECT_FALSE( has_cerr_output() );

			std::string const version_name( "VERSION" );

			auto index = FindItemInSortedList( version_name, ListOfObjects, NumObjectDefs );
			if ( index != 0 ) index = iListOfObjects( index );

			index = ObjectStartRecord( index );

			ASSERT_EQ( 1, index );

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

			std::string const schedule_compact_name( "SCHEDULE:COMPACT" );

			index = FindItemInSortedList( schedule_compact_name, ListOfObjects, NumObjectDefs );
			if ( index != 0 ) index = iListOfObjects( index );

			index = ObjectStartRecord( index );

			ASSERT_EQ( 3, index );

			EXPECT_EQ( schedule_compact_name, IDFRecords( index ).Name );
			EXPECT_EQ( 6, IDFRecords( index ).NumAlphas );
			EXPECT_EQ( 0, IDFRecords( index ).NumNumbers );
			EXPECT_TRUE( compare_containers( std::vector< std::string >( { "ACTIVITY_SCH", "ANY NUMBER", "THROUGH: 12/31", "FOR: ALLDAYS", "UNTIL: 24:00", "120." } ), IDFRecords( index ).Alphas ) );
			EXPECT_TRUE( compare_containers( std::vector< bool >( { false, false, false, false, false, false } ), IDFRecords( index ).AlphBlank ) );
			EXPECT_TRUE( compare_containers( std::vector< Real64 >( { } ), IDFRecords( index ).Numbers ) );
			EXPECT_TRUE( compare_containers( std::vector< bool >( { } ), IDFRecords( index ).NumBlank ) );

			std::string const site_location_name( "SITE:LOCATION" );

			index = FindItemInSortedList( site_location_name, ListOfObjects, NumObjectDefs );
			if ( index != 0 ) index = iListOfObjects( index );

			index = ObjectStartRecord( index );

			ASSERT_EQ( 4, index );

			EXPECT_EQ( site_location_name, IDFRecords( index ).Name );
			EXPECT_EQ( 1, IDFRecords( index ).NumAlphas );
			EXPECT_EQ( 4, IDFRecords( index ).NumNumbers );
			EXPECT_TRUE( compare_containers( std::vector< std::string >( { "USA IL-CHICAGO-OHARE" } ), IDFRecords( index ).Alphas ) );
			EXPECT_TRUE( compare_containers( std::vector< bool >( { false } ), IDFRecords( index ).AlphBlank ) );
			EXPECT_TRUE( compare_containers( std::vector< Real64 >( { 41.77, -87.75, -6.0, 190 } ), IDFRecords( index ).Numbers ) );
			EXPECT_TRUE( compare_containers( std::vector< bool >( { false, false, false, false } ), IDFRecords( index ).NumBlank ) );

		}

		TEST_F( InputProcessorFixture, getObjectItem1 )
		{
			std::string const idf_objects = delimited_string({
				"Version,8.3;",
				"Output:SQLite,SimpleAndTabular;",
			});

			ASSERT_FALSE( process_idf( idf_objects ) );

			std::string const CurrentModuleObject = "Output:SQLite";

			int NumSQLite = GetNumObjectsFound( CurrentModuleObject );
			ASSERT_EQ( 1, NumSQLite );

			int TotalArgs = 0;
			int NumAlphas = 0;
			int NumNumbers = 0;

			GetObjectDefMaxArgs( CurrentModuleObject, TotalArgs, NumAlphas, NumNumbers );

			int IOStatus = 0;
			Array1D_string Alphas( NumAlphas );
			Array1D< Real64 > Numbers( NumNumbers, 0.0 );
			Array1D_bool lNumericBlanks( NumAlphas, true );
			Array1D_bool lAlphaBlanks( NumAlphas, true );
			Array1D_string cAlphaFields( NumAlphas );
			Array1D_string cNumericFields( NumNumbers );

			GetObjectItem( CurrentModuleObject, NumSQLite, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			EXPECT_TRUE( compare_containers( std::vector< std::string >( { "SIMPLEANDTABULAR" } ), Alphas ) );
			EXPECT_TRUE( compare_containers( std::vector< std::string >( { "Option Type" } ), cAlphaFields ) );
			EXPECT_TRUE( compare_containers( std::vector< std::string >( { } ), cNumericFields ) );
			EXPECT_TRUE( compare_containers( std::vector< bool >( { true } ), lNumericBlanks ) );
			EXPECT_TRUE( compare_containers( std::vector< bool >( { false } ), lAlphaBlanks ) );
			EXPECT_TRUE( compare_containers( std::vector< Real64 >( { } ), Numbers ) );
			EXPECT_EQ( 1, NumAlphas );
			EXPECT_EQ( 0, NumNumbers );
			EXPECT_EQ( 1, IOStatus );

		}

		TEST_F( InputProcessorFixture, getObjectItem2 )
		{
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

			ASSERT_FALSE( process_idf( idf_objects ) );

			std::string const CurrentModuleObject = "Humidifier:Steam:Gas";

			int NumGasSteamHums = GetNumObjectsFound( CurrentModuleObject );
			ASSERT_EQ( 1, NumGasSteamHums );

			int TotalArgs = 0;
			int NumAlphas = 0;
			int NumNumbers = 0;

			GetObjectDefMaxArgs( CurrentModuleObject, TotalArgs, NumAlphas, NumNumbers );

			int IOStatus = 0;
			Array1D_string Alphas( NumAlphas );
			Array1D< Real64 > Numbers( NumNumbers, 0.0 );
			Array1D_bool lNumericBlanks( NumAlphas, true );
			Array1D_bool lAlphaBlanks( NumAlphas, true );
			Array1D_string cAlphaFields( NumAlphas );
			Array1D_string cNumericFields( NumNumbers );

			GetObjectItem( CurrentModuleObject, NumGasSteamHums, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			EXPECT_TRUE( compare_containers( std::vector< std::string >( { "MAIN GAS HUMIDIFIER", "", "THERMALEFFICIENCYFPLR", "MIXED AIR NODE 1", "MAIN HUMIDIFIER OUTLET NODE", "", "" } ), Alphas ) );
			EXPECT_TRUE( compare_containers( std::vector< std::string >( { "Name", "Availability Schedule Name", "Thermal Efficiency Modifier Curve Name", "Air Inlet Node Name", "Air Outlet Node Name", "Water Storage Tank Name", "Inlet Water Temperature Option" } ), cAlphaFields ) );
			EXPECT_TRUE( compare_containers( std::vector< std::string >( { "Rated Capacity", "Rated Gas Use Rate", "Thermal Efficiency", "Rated Fan Power", "Auxiliary Electric Power" } ), cNumericFields ) );
			EXPECT_TRUE( compare_containers( std::vector< bool >( { false, false, false, false, false, true, true } ), lNumericBlanks ) );
			EXPECT_TRUE( compare_containers( std::vector< bool >( { false, true, false, false, false, true, true } ), lAlphaBlanks ) );
			EXPECT_TRUE( compare_containers( std::vector< Real64 >( { -99999, -99999, 0.80, 0.0, 0.0 } ), Numbers ) );
			EXPECT_EQ( 6, NumAlphas );
			EXPECT_EQ( 5, NumNumbers );
			EXPECT_EQ( 1, IOStatus );

		}

		TEST_F( InputProcessorFixture, findItemInSortedList )
		{
			InputProcessor::ListOfObjects = Array1D_string ({
				"OUTPUT:METER",
				"OUTPUT:METER:CUMULATIVE",
				"OUTPUT:METER:CUMULATIVE:METERFILEONLY",
				"OUTPUT:METER:METERFILEONLY",
				"OUTPUT:SQLITE",
				"OUTPUT:VARIABLE"
			});

			InputProcessor::NumObjectDefs = ListOfObjects.size();

			iListOfObjects.allocate( NumObjectDefs );
			SortAndStringUtilities::SetupAndSort( ListOfObjects, iListOfObjects );

			auto index1 = FindItemInSortedList( "OUTPUT:METER", ListOfObjects, NumObjectDefs );
			auto index2 = FindItemInSortedList( "OUTPUT:METER:CUMULATIVE", ListOfObjects, NumObjectDefs );
			auto index3 = FindItemInSortedList( "OUTPUT:METER:CUMULATIVE:METERFILEONLY", ListOfObjects, NumObjectDefs );
			auto index4 = FindItemInSortedList( "OUTPUT:METER:METERFILEONLY", ListOfObjects, NumObjectDefs );
			auto index5 = FindItemInSortedList( "OUTPUT:SQLITE", ListOfObjects, NumObjectDefs );
			auto index6 = FindItemInSortedList( "OUTPUT:VARIABLE", ListOfObjects, NumObjectDefs );


			EXPECT_EQ( 1, index1 );
			EXPECT_EQ( 2, index2 );
			EXPECT_EQ( 3, index3 );
			EXPECT_EQ( 4, index4 );
			EXPECT_EQ( 5, index5 );
			EXPECT_EQ( 6, index6 );

			EXPECT_EQ( 1, iListOfObjects( index1 ) );
			EXPECT_EQ( 2, iListOfObjects( index2 ) );
			EXPECT_EQ( 3, iListOfObjects( index3 ) );
			EXPECT_EQ( 4, iListOfObjects( index4 ) );
			EXPECT_EQ( 5, iListOfObjects( index5 ) );
			EXPECT_EQ( 6, iListOfObjects( index6 ) );

		}

		TEST_F( InputProcessorFixture, findItemInSortedList_unsorted )
		{
			InputProcessor::ListOfObjects = Array1D_string ({
				"OUTPUT:VARIABLE",
				"OUTPUT:METER",
				"OUTPUT:METER:METERFILEONLY",
				"OUTPUT:METER:CUMULATIVE",
				"OUTPUT:METER:CUMULATIVE:METERFILEONLY",
				"OUTPUT:SQLITE"
			});

			InputProcessor::NumObjectDefs = ListOfObjects.size();

			iListOfObjects.allocate( NumObjectDefs );
			SortAndStringUtilities::SetupAndSort( ListOfObjects, iListOfObjects );

			auto index1 = FindItemInSortedList( "OUTPUT:METER", ListOfObjects, NumObjectDefs );
			auto index2 = FindItemInSortedList( "OUTPUT:METER:CUMULATIVE", ListOfObjects, NumObjectDefs );
			auto index3 = FindItemInSortedList( "OUTPUT:METER:CUMULATIVE:METERFILEONLY", ListOfObjects, NumObjectDefs );
			auto index4 = FindItemInSortedList( "OUTPUT:METER:METERFILEONLY", ListOfObjects, NumObjectDefs );
			auto index5 = FindItemInSortedList( "OUTPUT:SQLITE", ListOfObjects, NumObjectDefs );
			auto index6 = FindItemInSortedList( "OUTPUT:VARIABLE", ListOfObjects, NumObjectDefs );


			EXPECT_EQ( 1, index1 );
			EXPECT_EQ( 2, index2 );
			EXPECT_EQ( 3, index3 );
			EXPECT_EQ( 4, index4 );
			EXPECT_EQ( 5, index5 );
			EXPECT_EQ( 6, index6 );

			EXPECT_EQ( 2, iListOfObjects( index1 ) );
			EXPECT_EQ( 4, iListOfObjects( index2 ) );
			EXPECT_EQ( 5, iListOfObjects( index3 ) );
			EXPECT_EQ( 3, iListOfObjects( index4 ) );
			EXPECT_EQ( 6, iListOfObjects( index5 ) );
			EXPECT_EQ( 1, iListOfObjects( index6 ) );

		}

		TEST_F( InputProcessorFixture, findItemInList )
		{
			InputProcessor::ListOfObjects = Array1D_string ({
				"OUTPUT:VARIABLE",
				"OUTPUT:METER",
				"OUTPUT:METER:METERFILEONLY",
				"OUTPUT:METER:CUMULATIVE",
				"OUTPUT:METER:CUMULATIVE:METERFILEONLY",
				"OUTPUT:SQLITE"
			});

			InputProcessor::NumObjectDefs = ListOfObjects.size();

			EXPECT_EQ( 2, FindItemInList( "OUTPUT:METER", ListOfObjects, NumObjectDefs ) );
			EXPECT_EQ( 4, FindItemInList( "OUTPUT:METER:CUMULATIVE", ListOfObjects, NumObjectDefs ) );
			EXPECT_EQ( 5, FindItemInList( "OUTPUT:METER:CUMULATIVE:METERFILEONLY", ListOfObjects, NumObjectDefs ) );
			EXPECT_EQ( 3, FindItemInList( "OUTPUT:METER:METERFILEONLY", ListOfObjects, NumObjectDefs ) );
			EXPECT_EQ( 6, FindItemInList( "OUTPUT:SQLITE", ListOfObjects, NumObjectDefs ) );
			EXPECT_EQ( 1, FindItemInList( "OUTPUT:VARIABLE", ListOfObjects, NumObjectDefs ) );

		}

		TEST_F( InputProcessorFixture, addObjectDefandParse )
		{
			std::string const idd_objects =
				"Output:SQLite,\n"
				"       \\memo Output from EnergyPlus can be written to an SQLite format file.\n"
				"       \\unique-object\n"
				"  A1 ; \field Option Type\n"
				"       \type choice\n"
				"       \\key Simple\n"
				"       \\key SimpleAndTabular\n";

			auto idd_stream = std::unique_ptr<std::stringstream>( new std::stringstream( idd_objects ) );

			MaxSectionDefs = SectionDefAllocInc;
			MaxObjectDefs = ObjectDefAllocInc;

			SectionDef.allocate( MaxSectionDefs );
			ObjectDef.allocate( MaxObjectDefs );

			NumObjectDefs = 0;
			NumSectionDefs = 0;

			ProcessingIDD = true;

			bool errors_found( false );
			bool end_of_file( false );
			bool blank_line( false );
			std::string::size_type current_pos;

			ReadInputLine( *idd_stream, current_pos, blank_line, end_of_file );
			current_pos = scan( InputLine, ",;" );
			AddObjectDefandParse( *idd_stream, InputLine.substr( 0, current_pos ), current_pos, end_of_file, errors_found );

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
			EXPECT_TRUE( compare_containers( std::vector< bool >( { true } ), ObjectDef( 1 ).AlphaOrNumeric ) );
			EXPECT_TRUE( compare_containers( std::vector< bool >( { false } ), ObjectDef( 1 ).ReqField ) );
			EXPECT_TRUE( compare_containers( std::vector< bool >( { false } ), ObjectDef( 1 ).AlphRetainCase ) );
			EXPECT_TRUE( compare_containers( std::vector< std::string >( { "" } ), ObjectDef( 1 ).AlphFieldChks ) );
			EXPECT_TRUE( compare_containers( std::vector< std::string >( { "" } ), ObjectDef( 1 ).AlphFieldDefs ) );
			// EXPECT_TRUE( compare_containers( Array1D< RangeCheckDef >( { RangeCheckDef() } ), ObjectDef( 1 ).NumRangeChks ) );
			EXPECT_EQ( 0, ObjectDef( 1 ).NumFound );

		}

		TEST_F( InputProcessorFixture, addSectionDef )
		{
			std::string const idd_objects = delimited_string({
				"Lead Input;",
				"Simulation Data;",
			});

			auto idd_stream = std::unique_ptr<std::stringstream>( new std::stringstream( idd_objects ) );

			MaxSectionDefs = SectionDefAllocInc;
			SectionDef.allocate( MaxSectionDefs );
			NumSectionDefs = 0;

			ProcessingIDD = true;

			bool errors_found( false );
			bool end_of_file( false );
			bool blank_line( false );
			std::string::size_type current_pos;

			while ( ! end_of_file ) {
				ReadInputLine( *idd_stream, current_pos, blank_line, end_of_file );
				if ( blank_line || end_of_file ) continue;
				current_pos = scan( InputLine, ",;" );
				if ( InputLine[ current_pos ] == ';' ) {
					AddSectionDef( InputLine.substr( 0, current_pos ), errors_found );
					EXPECT_FALSE( errors_found );
				}
			}

			ASSERT_EQ( 2, NumSectionDefs );

			EXPECT_EQ( "LEAD INPUT", SectionDef( 1 ).Name );
			EXPECT_EQ( 0, SectionDef( 1 ).NumFound );

			EXPECT_EQ( "SIMULATION DATA", SectionDef( 2 ).Name );
			EXPECT_EQ( 0, SectionDef( 2 ).NumFound );

		}

		TEST_F( InputProcessorFixture, validateObjectandParse )
		{
			std::string const idf_objects = delimited_string({
				"Version,",
				"8.3;",
				"SimulationControl,",
				"NO, NO, NO, YES, YES;"
			});

			NumIDFRecords = 2;
			IDFRecords.allocate( 2 );

			auto idf_stream = std::unique_ptr<std::stringstream>( new std::stringstream( idf_objects ) );

			EnergyPlusFixture::use_cached_idd();

			NumLines = 0;

			MaxIDFRecords = ObjectsIDFAllocInc;
			NumIDFRecords = 0;
			MaxIDFSections = SectionsIDFAllocInc;
			NumIDFSections = 0;

			SectionsOnFile.allocate( MaxIDFSections );
			IDFRecords.allocate( MaxIDFRecords );
			LineItem.Numbers.allocate( MaxNumericArgsFound );
			LineItem.NumBlank.allocate( MaxNumericArgsFound );
			LineItem.Alphas.allocate( MaxAlphaArgsFound );
			LineItem.AlphBlank.allocate( MaxAlphaArgsFound );

			bool end_of_file( false );
			bool blank_line( false );
			std::string::size_type current_pos;

			while ( ! end_of_file ) {
				ReadInputLine( *idf_stream, current_pos, blank_line, end_of_file );
				if ( blank_line || end_of_file ) continue;
				current_pos = scan( InputLine, ",;" );
				ValidateObjectandParse( *idf_stream, InputLine.substr( 0, current_pos ), current_pos, end_of_file );
			}

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

		TEST_F( InputProcessorFixture, validateSection )
		{
			std::string const idf_objects = delimited_string({
				"Simulation Data;",
				"report variable dictionary;",
				"Lead Input;",
				"End Lead Input;",
				"SimulationControl;", // object used like section
				"Building;", // object used like section
			});

			NumIDFRecords = 2;
			IDFRecords.allocate( 2 );

			auto idf_stream = std::unique_ptr<std::stringstream>( new std::stringstream( idf_objects ) );

			EnergyPlusFixture::use_cached_idd();

			NumLines = 0;

			MaxIDFRecords = ObjectsIDFAllocInc;
			NumIDFRecords = 0;
			MaxIDFSections = SectionsIDFAllocInc;
			NumIDFSections = 0;

			SectionsOnFile.allocate( MaxIDFSections );
			IDFRecords.allocate( MaxIDFRecords );
			LineItem.Numbers.allocate( MaxNumericArgsFound );
			LineItem.NumBlank.allocate( MaxNumericArgsFound );
			LineItem.Alphas.allocate( MaxAlphaArgsFound );
			LineItem.AlphBlank.allocate( MaxAlphaArgsFound );

			bool errors_found( false );
			bool end_of_file( false );
			bool blank_line( false );
			std::string::size_type current_pos;

			while ( ! end_of_file ) {
				ReadInputLine( *idf_stream, current_pos, blank_line, end_of_file );
				if ( blank_line || end_of_file ) continue;
				current_pos = scan( InputLine, ",;" );
				if ( InputLine[ current_pos ] == ';' ) {
					ValidateSection( InputLine.substr( 0, current_pos ), errors_found );
					EXPECT_FALSE( errors_found );
				}
			}

			std::string const simulation_control_name( "SIMULATIONCONTROL" );

			auto index = FindItemInSortedList( simulation_control_name, ListOfObjects, NumObjectDefs );
			if ( index != 0 ) index = iListOfObjects( index );

			index = ObjectStartRecord( index );

			ASSERT_EQ( 1, index );

			EXPECT_EQ( simulation_control_name, IDFRecords( index ).Name );
			EXPECT_EQ( 5, IDFRecords( index ).NumAlphas );
			EXPECT_EQ( 0, IDFRecords( index ).NumNumbers );
			EXPECT_EQ( 2, IDFRecords( index ).ObjectDefPtr );
			EXPECT_TRUE( compare_containers( std::vector< std::string >( { "NO", "NO", "NO", "YES", "YES" } ), IDFRecords( index ).Alphas ) );
			EXPECT_TRUE( compare_containers( std::vector< bool >( { false, false, false, false, false } ), IDFRecords( index ).AlphBlank ) );
			EXPECT_TRUE( compare_containers( std::vector< Real64 >( {} ), IDFRecords( index ).Numbers ) );
			EXPECT_TRUE( compare_containers( std::vector< bool >( {} ), IDFRecords( index ).NumBlank ) );

			EXPECT_EQ( 1, ObjectDef( 2 ).NumFound );

			std::string const building_name( "BUILDING" );

			index = FindItemInSortedList( building_name, ListOfObjects, NumObjectDefs );
			if ( index != 0 ) index = iListOfObjects( index );

			index = ObjectStartRecord( index );

			ASSERT_EQ( 2, index );

			EXPECT_EQ( building_name, IDFRecords( index ).Name );
			EXPECT_EQ( 3, IDFRecords( index ).NumAlphas );
			EXPECT_EQ( 5, IDFRecords( index ).NumNumbers );
			EXPECT_EQ( 3, IDFRecords( index ).ObjectDefPtr );
			EXPECT_TRUE( compare_containers( std::vector< std::string >( { "NONE", "SUBURBS", "FULLEXTERIOR" } ), IDFRecords( index ).Alphas ) );
			EXPECT_TRUE( compare_containers( std::vector< bool >( { false, false, false } ), IDFRecords( index ).AlphBlank ) );
			EXPECT_TRUE( compare_containers( std::vector< Real64 >( { 0.0, 0.04, 0.4, 25, 6 } ), IDFRecords( index ).Numbers ) );
			EXPECT_TRUE( compare_containers( std::vector< bool >( { true, true, true, true, true } ), IDFRecords( index ).NumBlank ) );

			EXPECT_EQ( 1, ObjectDef( 3 ).NumFound );

			ASSERT_EQ( 5, NumSectionDefs );

			EXPECT_EQ( "SIMULATION DATA", SectionDef( 2 ).Name );
			EXPECT_EQ( 1, SectionDef( 2 ).NumFound );

			EXPECT_EQ( "REPORT VARIABLE DICTIONARY", SectionDef( 3 ).Name );
			EXPECT_EQ( 1, SectionDef( 3 ).NumFound );

			EXPECT_EQ( "LEAD INPUT", SectionDef( 1 ).Name );
			EXPECT_EQ( 1, SectionDef( 1 ).NumFound );

			EXPECT_EQ( "SIMULATIONCONTROL", SectionDef( 4 ).Name );
			EXPECT_EQ( 1, SectionDef( 4 ).NumFound );

			EXPECT_EQ( "BUILDING", SectionDef( 5 ).Name );
			EXPECT_EQ( 1, SectionDef( 5 ).NumFound );

			ASSERT_EQ( 5, NumIDFSections );

			EXPECT_EQ( "SIMULATION DATA", SectionsOnFile( 1 ).Name );
			EXPECT_EQ( 0, SectionsOnFile( 1 ).FirstRecord );
			EXPECT_EQ( 0, SectionsOnFile( 1 ).LastRecord );
			EXPECT_EQ( 0, SectionsOnFile( 1 ).FirstLineNo );

			EXPECT_EQ( "REPORT VARIABLE DICTIONARY", SectionsOnFile( 2 ).Name );
			EXPECT_EQ( 0, SectionsOnFile( 2 ).FirstRecord );
			EXPECT_EQ( 0, SectionsOnFile( 2 ).LastRecord );
			EXPECT_EQ( 0, SectionsOnFile( 2 ).FirstLineNo );

			EXPECT_EQ( "LEAD INPUT", SectionsOnFile( 3 ).Name );
			EXPECT_EQ( 0, SectionsOnFile( 3 ).FirstRecord );
			EXPECT_EQ( 0, SectionsOnFile( 3 ).LastRecord );
			EXPECT_EQ( 0, SectionsOnFile( 3 ).FirstLineNo );

			EXPECT_EQ( "SIMULATIONCONTROL", SectionsOnFile( 4 ).Name );
			EXPECT_EQ( 1, SectionsOnFile( 4 ).FirstRecord );
			EXPECT_EQ( 0, SectionsOnFile( 4 ).LastRecord );
			EXPECT_EQ( 0, SectionsOnFile( 4 ).FirstLineNo );

			EXPECT_EQ( "BUILDING", SectionsOnFile( 5 ).Name );
			EXPECT_EQ( 2, SectionsOnFile( 5 ).FirstRecord );
			EXPECT_EQ( 0, SectionsOnFile( 5 ).LastRecord );
			EXPECT_EQ( 0, SectionsOnFile( 5 ).FirstLineNo );

		}

		TEST_F( InputProcessorFixture, processSection )
		{
			std::string const idf_objects = delimited_string({
				"Simulation Data;",
				"report variable dictionary;",
				" Lead Input;",
				"  SimulationControl,",
				"    No,                      !- Do Zone Sizing Calculation",
				"    No,                      !- Do System Sizing Calculation",
				"    No,                      !- Do Plant Sizing Calculation",
				"    Yes,                     !- Run Simulation for Sizing Periods",
				"    No;                      !- Run Simulation for Weather File Run Periods",
				" End Lead Input;",
			});

			NumIDFRecords = 1;
			IDFRecords.allocate( 1 );

			auto idf_stream = std::unique_ptr<std::stringstream>( new std::stringstream( idf_objects ) );

			EnergyPlusFixture::use_cached_idd();

			NumLines = 0;

			MaxIDFRecords = ObjectsIDFAllocInc;
			NumIDFRecords = 0;
			MaxIDFSections = SectionsIDFAllocInc;
			NumIDFSections = 0;

			SectionsOnFile.allocate( MaxIDFSections );
			IDFRecords.allocate( MaxIDFRecords );
			LineItem.Numbers.allocate( MaxNumericArgsFound );
			LineItem.NumBlank.allocate( MaxNumericArgsFound );
			LineItem.Alphas.allocate( MaxAlphaArgsFound );
			LineItem.AlphBlank.allocate( MaxAlphaArgsFound );

			bool errors_found( false );
			bool end_of_file( false );
			bool blank_line( false );
			std::string::size_type current_pos;

			while ( ! end_of_file ) {
				ReadInputLine( *idf_stream, current_pos, blank_line, end_of_file );
				if ( blank_line || end_of_file ) continue;
				current_pos = scan( InputLine, ",;" );
				if ( InputLine[ current_pos ] == ';' ) {
					ValidateSection( InputLine.substr( 0, current_pos ), errors_found );
					EXPECT_FALSE( errors_found );
				} else {
					ValidateObjectandParse( *idf_stream, InputLine.substr( 0, current_pos ), current_pos, end_of_file );
				}
			}

			std::string const simulation_control_name( "SIMULATIONCONTROL" );

			auto index = FindItemInSortedList( simulation_control_name, ListOfObjects, NumObjectDefs );
			if ( index != 0 ) index = iListOfObjects( index );

			index = ObjectStartRecord( index );

			ASSERT_EQ( 1, index );

			EXPECT_EQ( simulation_control_name, IDFRecords( index ).Name );
			EXPECT_EQ( 5, IDFRecords( index ).NumAlphas );
			EXPECT_EQ( 0, IDFRecords( index ).NumNumbers );
			EXPECT_EQ( 2, IDFRecords( index ).ObjectDefPtr );
			EXPECT_TRUE( compare_containers( std::vector< std::string >( { "NO", "NO", "NO", "YES", "NO" } ), IDFRecords( index ).Alphas ) );
			EXPECT_TRUE( compare_containers( std::vector< bool >( { false, false, false, false, false } ), IDFRecords( index ).AlphBlank ) );
			EXPECT_TRUE( compare_containers( std::vector< Real64 >( {} ), IDFRecords( index ).Numbers ) );
			EXPECT_TRUE( compare_containers( std::vector< bool >( {} ), IDFRecords( index ).NumBlank ) );

			ASSERT_EQ( 3, NumSectionDefs );

			EXPECT_EQ( "SIMULATION DATA", SectionDef( 2 ).Name );
			EXPECT_EQ( 1, SectionDef( 2 ).NumFound );

			EXPECT_EQ( "REPORT VARIABLE DICTIONARY", SectionDef( 3 ).Name );
			EXPECT_EQ( 1, SectionDef( 3 ).NumFound );

			EXPECT_EQ( "LEAD INPUT", SectionDef( 1 ).Name );
			EXPECT_EQ( 1, SectionDef( 1 ).NumFound );

			ASSERT_EQ( 3, NumIDFSections );

			EXPECT_EQ( "SIMULATION DATA", SectionsOnFile( 1 ).Name );
			EXPECT_EQ( 0, SectionsOnFile( 1 ).FirstRecord );
			EXPECT_EQ( 0, SectionsOnFile( 1 ).FirstLineNo );

			EXPECT_EQ( "REPORT VARIABLE DICTIONARY", SectionsOnFile( 2 ).Name );
			EXPECT_EQ( 0, SectionsOnFile( 2 ).FirstRecord );
			EXPECT_EQ( 0, SectionsOnFile( 2 ).FirstLineNo );

			EXPECT_EQ( "LEAD INPUT", SectionsOnFile( 3 ).Name );
			EXPECT_EQ( 0, SectionsOnFile( 3 ).FirstRecord );
			EXPECT_EQ( 0, SectionsOnFile( 3 ).FirstLineNo );
		}

		TEST_F( InputProcessorFixture, getNumObjectsFound )
		{
			std::string const idf_objects = delimited_string({
				"Version,8.3;",
				"Output:SQLite,SimpleAndTabular;",
				"Output:Meter:MeterFileOnly,Electricity:Facility,timestep;",
				"Output:Meter:MeterFileOnly,Electricity:Facility,hourly;",
				"Output:Meter:MeterFileOnly,Electricity:Facility,daily;",
				"Output:Meter:MeterFileOnly,Electricity:Facility,monthly;",
				"Output:Meter:MeterFileOnly,Electricity:Facility,runperiod;",
			});

			ASSERT_FALSE( process_idf( idf_objects ) );

			EXPECT_EQ( 1, GetNumObjectsFound( "VERSION" ) );
			EXPECT_EQ( 1, GetNumObjectsFound( "OUTPUT:SQLITE" ) );
			EXPECT_EQ( 5, GetNumObjectsFound( "OUTPUT:METER:METERFILEONLY" ) );
			EXPECT_EQ( 0, GetNumObjectsFound( "OUTPUT:VARIABLE" ) );

			EXPECT_FALSE( has_cout_output() );
			EXPECT_FALSE( has_cerr_output() );

		}

		TEST_F( InputProcessorFixture, addRecordToOutputVariableStructure )
		{
			using namespace DataOutputs;

			AddRecordToOutputVariableStructure( "*", "ZONE AIR SYSTEM SENSIBLE COOLING RATE" );
			AddRecordToOutputVariableStructure( "*", "SITE OUTDOOR AIR DRYBULB TEMPERATURE" );
			AddRecordToOutputVariableStructure( "*", "SITE OUTDOOR AIR WETBULB TEMPERATURE" );
			AddRecordToOutputVariableStructure( "*", "ZONE TOTAL INTERNAL LATENT GAIN ENERGY" );
			AddRecordToOutputVariableStructure( "*", "ZONE TOTAL INTERNAL LATENT GAIN RATE" );

			EXPECT_EQ( "*", OutputVariablesForSimulation( 1 ).Key );
			EXPECT_EQ( "ZONE AIR SYSTEM SENSIBLE COOLING RATE", OutputVariablesForSimulation( 1 ).VarName );
			EXPECT_EQ( 0, OutputVariablesForSimulation( 1 ).Previous );
			EXPECT_EQ( 0, OutputVariablesForSimulation( 1 ).Next );

			EXPECT_EQ( "*", OutputVariablesForSimulation( 2 ).Key );
			EXPECT_EQ( "SITE OUTDOOR AIR DRYBULB TEMPERATURE", OutputVariablesForSimulation( 2 ).VarName );
			EXPECT_EQ( 0, OutputVariablesForSimulation( 2 ).Previous );
			EXPECT_EQ( 0, OutputVariablesForSimulation( 2 ).Next );

			EXPECT_EQ( "*", OutputVariablesForSimulation( 3 ).Key );
			EXPECT_EQ( "SITE OUTDOOR AIR WETBULB TEMPERATURE", OutputVariablesForSimulation( 3 ).VarName );
			EXPECT_EQ( 0, OutputVariablesForSimulation( 3 ).Previous );
			EXPECT_EQ( 0, OutputVariablesForSimulation( 3 ).Next );

			EXPECT_EQ( "*", OutputVariablesForSimulation( 4 ).Key );
			EXPECT_EQ( "ZONE TOTAL INTERNAL LATENT GAIN ENERGY", OutputVariablesForSimulation( 4 ).VarName );
			EXPECT_EQ( 0, OutputVariablesForSimulation( 4 ).Previous );
			EXPECT_EQ( 0, OutputVariablesForSimulation( 4 ).Next );

			EXPECT_EQ( "*", OutputVariablesForSimulation( 5 ).Key );
			EXPECT_EQ( "ZONE TOTAL INTERNAL LATENT GAIN RATE", OutputVariablesForSimulation( 5 ).VarName );
			EXPECT_EQ( 0, OutputVariablesForSimulation( 5 ).Previous );
			EXPECT_EQ( 0, OutputVariablesForSimulation( 5 ).Next );

		}

		TEST_F( InputProcessorFixture, addVariablesForMonthlyReport )
		{
			auto const results_map = std::map< std::string, std::vector< std::tuple< std::string, std::string, int, int > > >({
				{
					"ZONECOOLINGSUMMARYMONTHLY",
					{
						std::make_tuple( "*", "ZONE AIR SYSTEM SENSIBLE COOLING RATE", 0, 0 ),
						std::make_tuple( "*", "SITE OUTDOOR AIR DRYBULB TEMPERATURE", 0, 0 ),
						std::make_tuple( "*", "SITE OUTDOOR AIR WETBULB TEMPERATURE", 0, 0 ),
						std::make_tuple( "*", "ZONE TOTAL INTERNAL LATENT GAIN ENERGY", 0, 0 ),
						std::make_tuple( "*", "ZONE TOTAL INTERNAL LATENT GAIN RATE", 0, 0 ),
					}
				},
				{
					"ZONEHEATINGSUMMARYMONTHLY",
					{
						std::make_tuple( "*", "ZONE AIR SYSTEM SENSIBLE HEATING ENERGY", 0, 0 ),
						std::make_tuple( "*", "ZONE AIR SYSTEM SENSIBLE HEATING RATE", 0, 0 ),
						std::make_tuple( "*", "SITE OUTDOOR AIR DRYBULB TEMPERATURE", 0, 0 ),
					}
				},
				{
					"ZONEELECTRICSUMMARYMONTHLY",
					{
						std::make_tuple( "*", "ZONE LIGHTS ELECTRIC ENERGY", 0, 0 ),
						std::make_tuple( "*", "ZONE ELECTRIC EQUIPMENT ELECTRIC ENERGY", 0, 0 ),
					}
				},
				{
					"SPACEGAINSMONTHLY",
					{
						std::make_tuple( "*", "ZONE PEOPLE TOTAL HEATING ENERGY", 0, 0 ),
						std::make_tuple( "*", "ZONE LIGHTS TOTAL HEATING ENERGY", 0, 0 ),
						std::make_tuple( "*", "ZONE ELECTRIC EQUIPMENT TOTAL HEATING ENERGY", 0, 0 ),
						std::make_tuple( "*", "ZONE GAS EQUIPMENT TOTAL HEATING ENERGY", 0, 0 ),
						std::make_tuple( "*", "ZONE HOT WATER EQUIPMENT TOTAL HEATING ENERGY", 0, 0 ),
						std::make_tuple( "*", "ZONE STEAM EQUIPMENT TOTAL HEATING ENERGY", 0, 0 ),
						std::make_tuple( "*", "ZONE OTHER EQUIPMENT TOTAL HEATING ENERGY", 0, 0 ),
						std::make_tuple( "*", "ZONE INFILTRATION SENSIBLE HEAT GAIN ENERGY", 0, 0 ),
						std::make_tuple( "*", "ZONE INFILTRATION SENSIBLE HEAT LOSS ENERGY", 0, 0 ),
					}
				},
				{
					"PEAKSPACEGAINSMONTHLY",
					{
						std::make_tuple( "*", "ZONE PEOPLE TOTAL HEATING ENERGY", 0, 0 ),
						std::make_tuple( "*", "ZONE LIGHTS TOTAL HEATING ENERGY", 0, 0 ),
						std::make_tuple( "*", "ZONE ELECTRIC EQUIPMENT TOTAL HEATING ENERGY", 0, 0 ),
						std::make_tuple( "*", "ZONE GAS EQUIPMENT TOTAL HEATING ENERGY", 0, 0 ),
						std::make_tuple( "*", "ZONE HOT WATER EQUIPMENT TOTAL HEATING ENERGY", 0, 0 ),
						std::make_tuple( "*", "ZONE STEAM EQUIPMENT TOTAL HEATING ENERGY", 0, 0 ),
						std::make_tuple( "*", "ZONE OTHER EQUIPMENT TOTAL HEATING ENERGY", 0, 0 ),
						std::make_tuple( "*", "ZONE INFILTRATION SENSIBLE HEAT GAIN ENERGY", 0, 0 ),
						std::make_tuple( "*", "ZONE INFILTRATION SENSIBLE HEAT LOSS ENERGY", 0, 0 ),
					}
				},
				{
					"SPACEGAINCOMPONENTSATCOOLINGPEAKMONTHLY",
					{
						std::make_tuple( "*", "ZONE AIR SYSTEM SENSIBLE COOLING RATE", 0, 0 ),
						std::make_tuple( "*", "ZONE PEOPLE TOTAL HEATING ENERGY", 0, 0 ),
						std::make_tuple( "*", "ZONE LIGHTS TOTAL HEATING ENERGY", 0, 0 ),
						std::make_tuple( "*", "ZONE ELECTRIC EQUIPMENT TOTAL HEATING ENERGY", 0, 0 ),
						std::make_tuple( "*", "ZONE GAS EQUIPMENT TOTAL HEATING ENERGY", 0, 0 ),
						std::make_tuple( "*", "ZONE HOT WATER EQUIPMENT TOTAL HEATING ENERGY", 0, 0 ),
						std::make_tuple( "*", "ZONE STEAM EQUIPMENT TOTAL HEATING ENERGY", 0, 0 ),
						std::make_tuple( "*", "ZONE OTHER EQUIPMENT TOTAL HEATING ENERGY", 0, 0 ),
						std::make_tuple( "*", "ZONE INFILTRATION SENSIBLE HEAT GAIN ENERGY", 0, 0 ),
						std::make_tuple( "*", "ZONE INFILTRATION SENSIBLE HEAT LOSS ENERGY", 0, 0 ),
					}
				},
				{
					"SETPOINTSNOTMETWITHTEMPERATURESMONTHLY",
					{
						std::make_tuple( "*", "ZONE HEATING SETPOINT NOT MET TIME", 0, 0 ),
						std::make_tuple( "*", "ZONE MEAN AIR TEMPERATURE", 0, 0 ),
						std::make_tuple( "*", "ZONE HEATING SETPOINT NOT MET WHILE OCCUPIED TIME", 0, 0 ),
						std::make_tuple( "*", "ZONE COOLING SETPOINT NOT MET TIME", 0, 0 ),
						std::make_tuple( "*", "ZONE COOLING SETPOINT NOT MET WHILE OCCUPIED TIME", 0, 0 ),
					}
				},
				{
					"COMFORTREPORTSIMPLE55MONTHLY",
					{
						std::make_tuple( "*", "ZONE THERMAL COMFORT ASHRAE 55 SIMPLE MODEL SUMMER CLOTHES NOT COMFORTABLE TIME", 0, 0 ),
						std::make_tuple( "*", "ZONE MEAN AIR TEMPERATURE", 0, 0 ),
						std::make_tuple( "*", "ZONE THERMAL COMFORT ASHRAE 55 SIMPLE MODEL WINTER CLOTHES NOT COMFORTABLE TIME", 0, 0 ),
						std::make_tuple( "*", "ZONE THERMAL COMFORT ASHRAE 55 SIMPLE MODEL SUMMER OR WINTER CLOTHES NOT COMFORTABLE TIME", 0, 0 ),
					}
				},
				{
					"UNGLAZEDTRANSPIREDSOLARCOLLECTORSUMMARYMONTHLY",
					{
						std::make_tuple( "*", "SOLAR COLLECTOR SYSTEM EFFICIENCY", 0, 0 ),
						std::make_tuple( "*", "SOLAR COLLECTOR OUTSIDE FACE SUCTION VELOCITY", 0, 0 ),
						std::make_tuple( "*", "SOLAR COLLECTOR SENSIBLE HEATING RATE", 0, 0 ),
					}
				},
				{
					"OCCUPANTCOMFORTDATASUMMARYMONTHLY",
					{
						std::make_tuple( "*", "PEOPLE OCCUPANT COUNT", 0, 0 ),
						std::make_tuple( "*", "PEOPLE AIR TEMPERATURE", 0, 0 ),
						std::make_tuple( "*", "PEOPLE AIR RELATIVE HUMIDITY", 0, 0 ),
						std::make_tuple( "*", "ZONE THERMAL COMFORT FANGER MODEL PMV", 0, 0 ),
						std::make_tuple( "*", "ZONE THERMAL COMFORT FANGER MODEL PPD", 0, 0 ),
					}
				},
				{
					"CHILLERREPORTMONTHLY",
					{
						std::make_tuple( "*", "CHILLER ELECTRIC ENERGY", 0, 0 ),
						std::make_tuple( "*", "CHILLER ELECTRIC POWER", 0, 0 ),
						std::make_tuple( "*", "CHILLER EVAPORATOR COOLING ENERGY", 0, 0 ),
						std::make_tuple( "*", "CHILLER CONDENSER HEAT TRANSFER ENERGY", 0, 0 ),
						std::make_tuple( "*", "CHILLER COP", 0, 0 ),
					}
				},
				{
					"TOWERREPORTMONTHLY",
					{
						std::make_tuple( "*", "COOLING TOWER FAN ELECTRIC ENERGY", 0, 0 ),
						std::make_tuple( "*", "COOLING TOWER FAN ELECTRIC POWER", 0, 0 ),
						std::make_tuple( "*", "COOLING TOWER HEAT TRANSFER RATE", 0, 0 ),
						std::make_tuple( "*", "COOLING TOWER INLET TEMPERATURE", 0, 0 ),
						std::make_tuple( "*", "COOLING TOWER OUTLET TEMPERATURE", 0, 0 ),
						std::make_tuple( "*", "COOLING TOWER MASS FLOW RATE", 0, 0 ),
					}
				},
				{
					"BOILERREPORTMONTHLY",
					{
						std::make_tuple( "*", "BOILER HEATING ENERGY", 0, 0 ),
						std::make_tuple( "*", "BOILER GAS CONSUMPTION", 0, 0 ),
						std::make_tuple( "*", "BOILER HEATING RATE", 0, 0 ),
						std::make_tuple( "*", "BOILER GAS CONSUMPTION RATE", 0, 0 ),
						std::make_tuple( "*", "BOILER INLET TEMPERATURE", 0, 0 ),
						std::make_tuple( "*", "BOILER OUTLET TEMPERATURE", 0, 0 ),
						std::make_tuple( "*", "BOILER MASS FLOW RATE", 0, 0 ),
						std::make_tuple( "*", "BOILER ANCILLARY ELECTRIC POWER", 0, 0 ),
					}
				},
				{
					"DXREPORTMONTHLY",
					{
						std::make_tuple( "*", "COOLING COIL TOTAL COOLING ENERGY", 0, 0 ),
						std::make_tuple( "*", "COOLING COIL ELECTRIC ENERGY", 0, 0 ),
						std::make_tuple( "*", "COOLING COIL SENSIBLE COOLING ENERGY", 0, 0 ),
						std::make_tuple( "*", "COOLING COIL LATENT COOLING ENERGY", 0, 0 ),
						std::make_tuple( "*", "COOLING COIL CRANKCASE HEATER ELECTRIC ENERGY", 0, 0 ),
						std::make_tuple( "*", "COOLING COIL RUNTIME FRACTION", 0, 0 ),
						std::make_tuple( "*", "COOLING COIL TOTAL COOLING RATE", 0, 0 ),
						std::make_tuple( "*", "COOLING COIL SENSIBLE COOLING RATE", 0, 0 ),
						std::make_tuple( "*", "COOLING COIL LATENT COOLING RATE", 0, 0 ),
						std::make_tuple( "*", "COOLING COIL ELECTRIC POWER", 0, 0 ),
						std::make_tuple( "*", "COOLING COIL CRANKCASE HEATER ELECTRIC POWER", 0, 0 ),
					}
				},
				{
					"WINDOWREPORTMONTHLY",
					{
						std::make_tuple( "*", "SURFACE WINDOW TRANSMITTED SOLAR RADIATION RATE", 0, 0 ),
						std::make_tuple( "*", "SURFACE WINDOW TRANSMITTED BEAM SOLAR RADIATION RATE", 0, 0 ),
						std::make_tuple( "*", "SURFACE WINDOW TRANSMITTED DIFFUSE SOLAR RADIATION RATE", 0, 0 ),
						std::make_tuple( "*", "SURFACE WINDOW HEAT GAIN RATE", 0, 0 ),
						std::make_tuple( "*", "SURFACE WINDOW HEAT LOSS RATE", 0, 0 ),
						std::make_tuple( "*", "SURFACE WINDOW INSIDE FACE GLAZING CONDENSATION STATUS", 0, 0 ),
						std::make_tuple( "*", "SURFACE SHADING DEVICE IS ON TIME FRACTION", 0, 0 ),
						std::make_tuple( "*", "SURFACE STORM WINDOW ON OFF STATUS", 0, 0 ),
					}
				},
				{
					"WINDOWENERGYREPORTMONTHLY",
					{
						std::make_tuple( "*", "SURFACE WINDOW TRANSMITTED SOLAR RADIATION ENERGY", 0, 0 ),
						std::make_tuple( "*", "SURFACE WINDOW TRANSMITTED BEAM SOLAR RADIATION ENERGY", 0, 0 ),
						std::make_tuple( "*", "SURFACE WINDOW TRANSMITTED DIFFUSE SOLAR RADIATION ENERGY", 0, 0 ),
						std::make_tuple( "*", "SURFACE WINDOW HEAT GAIN ENERGY", 0, 0 ),
						std::make_tuple( "*", "SURFACE WINDOW HEAT LOSS ENERGY", 0, 0 ),
					}
				},
				{
					"WINDOWZONESUMMARYMONTHLY",
					{
						std::make_tuple( "*", "ZONE WINDOWS TOTAL HEAT GAIN RATE", 0, 0 ),
						std::make_tuple( "*", "ZONE WINDOWS TOTAL HEAT LOSS RATE", 0, 0 ),
						std::make_tuple( "*", "ZONE WINDOWS TOTAL TRANSMITTED SOLAR RADIATION RATE", 0, 0 ),
						std::make_tuple( "*", "ZONE EXTERIOR WINDOWS TOTAL TRANSMITTED BEAM SOLAR RADIATION RATE", 0, 0 ),
						std::make_tuple( "*", "ZONE EXTERIOR WINDOWS TOTAL TRANSMITTED DIFFUSE SOLAR RADIATION RATE", 0, 0 ),
						std::make_tuple( "*", "ZONE INTERIOR WINDOWS TOTAL TRANSMITTED DIFFUSE SOLAR RADIATION RATE", 0, 0 ),
						std::make_tuple( "*", "ZONE INTERIOR WINDOWS TOTAL TRANSMITTED BEAM SOLAR RADIATION RATE", 0, 0 ),
					}
				},
				{
					"WINDOWENERGYZONESUMMARYMONTHLY",
					{
						std::make_tuple( "*", "ZONE WINDOWS TOTAL HEAT GAIN ENERGY", 0, 0 ),
						std::make_tuple( "*", "ZONE WINDOWS TOTAL HEAT LOSS ENERGY", 0, 0 ),
						std::make_tuple( "*", "ZONE WINDOWS TOTAL TRANSMITTED SOLAR RADIATION ENERGY", 0, 0 ),
						std::make_tuple( "*", "ZONE EXTERIOR WINDOWS TOTAL TRANSMITTED BEAM SOLAR RADIATION ENERGY", 0, 0 ),
						std::make_tuple( "*", "ZONE EXTERIOR WINDOWS TOTAL TRANSMITTED DIFFUSE SOLAR RADIATION ENERGY", 0, 0 ),
						std::make_tuple( "*", "ZONE INTERIOR WINDOWS TOTAL TRANSMITTED DIFFUSE SOLAR RADIATION ENERGY", 0, 0 ),
						std::make_tuple( "*", "ZONE INTERIOR WINDOWS TOTAL TRANSMITTED BEAM SOLAR RADIATION ENERGY", 0, 0 ),
					}
				},
				{
					"AVERAGEOUTDOORCONDITIONSMONTHLY",
					{
						std::make_tuple( "*", "SITE OUTDOOR AIR DRYBULB TEMPERATURE", 0, 0 ),
						std::make_tuple( "*", "SITE OUTDOOR AIR WETBULB TEMPERATURE", 0, 0 ),
						std::make_tuple( "*", "SITE OUTDOOR AIR DEWPOINT TEMPERATURE", 0, 0 ),
						std::make_tuple( "*", "SITE WIND SPEED", 0, 0 ),
						std::make_tuple( "*", "SITE SKY TEMPERATURE", 0, 0 ),
						std::make_tuple( "*", "SITE DIFFUSE SOLAR RADIATION RATE PER AREA", 0, 0 ),
						std::make_tuple( "*", "SITE DIRECT SOLAR RADIATION RATE PER AREA", 0, 0 ),
						std::make_tuple( "*", "SITE RAIN STATUS", 0, 0 ),
					}
				},
				{
					"OUTDOORCONDITIONSMAXIMUMDRYBULBMONTHLY",
					{
						std::make_tuple( "*", "SITE OUTDOOR AIR DRYBULB TEMPERATURE", 0, 0 ),
						std::make_tuple( "*", "SITE OUTDOOR AIR WETBULB TEMPERATURE", 0, 0 ),
						std::make_tuple( "*", "SITE OUTDOOR AIR DEWPOINT TEMPERATURE", 0, 0 ),
						std::make_tuple( "*", "SITE WIND SPEED", 0, 0 ),
						std::make_tuple( "*", "SITE SKY TEMPERATURE", 0, 0 ),
						std::make_tuple( "*", "SITE DIFFUSE SOLAR RADIATION RATE PER AREA", 0, 0 ),
						std::make_tuple( "*", "SITE DIRECT SOLAR RADIATION RATE PER AREA", 0, 0 ),
					}
				},
				{
					"OUTDOORCONDITIONSMINIMUMDRYBULBMONTHLY",
					{
						std::make_tuple( "*", "SITE OUTDOOR AIR DRYBULB TEMPERATURE", 0, 0 ),
						std::make_tuple( "*", "SITE OUTDOOR AIR WETBULB TEMPERATURE", 0, 0 ),
						std::make_tuple( "*", "SITE OUTDOOR AIR DEWPOINT TEMPERATURE", 0, 0 ),
						std::make_tuple( "*", "SITE WIND SPEED", 0, 0 ),
						std::make_tuple( "*", "SITE SKY TEMPERATURE", 0, 0 ),
						std::make_tuple( "*", "SITE DIFFUSE SOLAR RADIATION RATE PER AREA", 0, 0 ),
						std::make_tuple( "*", "SITE DIRECT SOLAR RADIATION RATE PER AREA", 0, 0 ),
					}
				},
				{
					"OUTDOORCONDITIONSMAXIMUMWETBULBMONTHLY",
					{
						std::make_tuple( "*", "SITE OUTDOOR AIR WETBULB TEMPERATURE", 0, 0 ),
						std::make_tuple( "*", "SITE OUTDOOR AIR DRYBULB TEMPERATURE", 0, 0 ),
						std::make_tuple( "*", "SITE OUTDOOR AIR DEWPOINT TEMPERATURE", 0, 0 ),
						std::make_tuple( "*", "SITE WIND SPEED", 0, 0 ),
						std::make_tuple( "*", "SITE SKY TEMPERATURE", 0, 0 ),
						std::make_tuple( "*", "SITE DIFFUSE SOLAR RADIATION RATE PER AREA", 0, 0 ),
						std::make_tuple( "*", "SITE DIRECT SOLAR RADIATION RATE PER AREA", 0, 0 ),
					}
				},
				{
					"OUTDOORCONDITIONSMAXIMUMDEWPOINTMONTHLY",
					{
						std::make_tuple( "*", "SITE OUTDOOR AIR DEWPOINT TEMPERATURE", 0, 0 ),
						std::make_tuple( "*", "SITE OUTDOOR AIR DRYBULB TEMPERATURE", 0, 0 ),
						std::make_tuple( "*", "SITE OUTDOOR AIR WETBULB TEMPERATURE", 0, 0 ),
						std::make_tuple( "*", "SITE WIND SPEED", 0, 0 ),
						std::make_tuple( "*", "SITE SKY TEMPERATURE", 0, 0 ),
						std::make_tuple( "*", "SITE DIFFUSE SOLAR RADIATION RATE PER AREA", 0, 0 ),
						std::make_tuple( "*", "SITE DIRECT SOLAR RADIATION RATE PER AREA", 0, 0 ),
					}
				},
				{
					"OUTDOORGROUNDCONDITIONSMONTHLY",
					{
						std::make_tuple( "*", "SITE GROUND TEMPERATURE", 0, 0 ),
						std::make_tuple( "*", "SITE SURFACE GROUND TEMPERATURE", 0, 0 ),
						std::make_tuple( "*", "SITE DEEP GROUND TEMPERATURE", 0, 0 ),
						std::make_tuple( "*", "SITE MAINS WATER TEMPERATURE", 0, 0 ),
						std::make_tuple( "*", "SITE GROUND REFLECTED SOLAR RADIATION RATE PER AREA", 0, 0 ),
						std::make_tuple( "*", "SITE SNOW ON GROUND STATUS", 0, 0 ),
					}
				},
				{
					"WINDOWACREPORTMONTHLY",
					{
						std::make_tuple( "*", "ZONE WINDOW AIR CONDITIONER TOTAL COOLING ENERGY", 0, 0 ),
						std::make_tuple( "*", "ZONE WINDOW AIR CONDITIONER ELECTRIC ENERGY", 0, 0 ),
						std::make_tuple( "*", "ZONE WINDOW AIR CONDITIONER SENSIBLE COOLING ENERGY", 0, 0 ),
						std::make_tuple( "*", "ZONE WINDOW AIR CONDITIONER LATENT COOLING ENERGY", 0, 0 ),
						std::make_tuple( "*", "ZONE WINDOW AIR CONDITIONER TOTAL COOLING RATE", 0, 0 ),
						std::make_tuple( "*", "ZONE WINDOW AIR CONDITIONER SENSIBLE COOLING RATE", 0, 0 ),
						std::make_tuple( "*", "ZONE WINDOW AIR CONDITIONER LATENT COOLING RATE", 0, 0 ),
						std::make_tuple( "*", "ZONE WINDOW AIR CONDITIONER ELECTRIC POWER", 0, 0 ),
					}
				},
				{
					"WATERHEATERREPORTMONTHLY",
					{
						std::make_tuple( "*", "WATER HEATER TOTAL DEMAND HEAT TRANSFER ENERGY", 0, 0 ),
						std::make_tuple( "*", "WATER HEATER USE SIDE HEAT TRANSFER ENERGY", 0, 0 ),
						std::make_tuple( "*", "WATER HEATER BURNER HEATING ENERGY", 0, 0 ),
						std::make_tuple( "*", "WATER HEATER GAS CONSUMPTION", 0, 0 ),
						std::make_tuple( "*", "WATER HEATER LOSS DEMAND ENERGY", 0, 0 ),
						std::make_tuple( "*", "WATER HEATER HEAT LOSS ENERGY", 0, 0 ),
						std::make_tuple( "*", "WATER HEATER TANK TEMPERATURE", 0, 0 ),
						std::make_tuple( "*", "WATER HEATER HEAT RECOVERY SUPPLY ENERGY", 0, 0 ),
						std::make_tuple( "*", "WATER HEATER SOURCE ENERGY", 0, 0 ),
					}
				},
				{
					"GENERATORREPORTMONTHLY",
					{
						std::make_tuple( "*", "GENERATOR PRODUCED ELECTRIC ENERGY", 0, 0 ),
						std::make_tuple( "*", "GENERATOR DIESEL CONSUMPTION", 0, 0 ),
						std::make_tuple( "*", "GENERATOR GAS CONSUMPTION", 0, 0 ),
						std::make_tuple( "*", "GENERATOR TOTAL HEAT RECOVERY", 0, 0 ),
						std::make_tuple( "*", "GENERATOR JACKET HEAT RECOVERY ENERGY", 0, 0 ),
						std::make_tuple( "*", "GENERATOR LUBE HEAT RECOVERY", 0, 0 ),
						std::make_tuple( "*", "GENERATOR EXHAUST HEAT RECOVERY ENERGY", 0, 0 ),
						std::make_tuple( "*", "GENERATOR EXHAUST AIR TEMPERATURE", 0, 0 ),
					}
				},
				{
					"DAYLIGHTINGREPORTMONTHLY",
					{
						std::make_tuple( "*", "SITE EXTERIOR BEAM NORMAL ILLUMINANCE", 0, 0 ),
						std::make_tuple( "*", "DAYLIGHTING LIGHTING POWER MULTIPLIER", 0, 0 ),
						std::make_tuple( "*", "DAYLIGHTING REFERENCE POINT 1 ILLUMINANCE", 0, 0 ),
						std::make_tuple( "*", "DAYLIGHTING REFERENCE POINT 1 GLARE INDEX", 0, 0 ),
						std::make_tuple( "*", "DAYLIGHTING REFERENCE POINT 1 GLARE INDEX SETPOINT EXCEEDED TIME", 0, 0 ),
						std::make_tuple( "*", "DAYLIGHTING REFERENCE POINT 1 DAYLIGHT ILLUMINANCE SETPOINT EXCEEDED TIME", 0, 0 ),
						std::make_tuple( "*", "DAYLIGHTING REFERENCE POINT 2 ILLUMINANCE", 0, 0 ),
						std::make_tuple( "*", "DAYLIGHTING REFERENCE POINT 2 GLARE INDEX", 0, 0 ),
						std::make_tuple( "*", "DAYLIGHTING REFERENCE POINT 2 GLARE INDEX SETPOINT EXCEEDED TIME", 0, 0 ),
						std::make_tuple( "*", "DAYLIGHTING REFERENCE POINT 2 DAYLIGHT ILLUMINANCE SETPOINT EXCEEDED TIME", 0, 0 ),
					}
				},
				{
					"COILREPORTMONTHLY",
					{
						std::make_tuple( "*", "HEATING COIL HEATING ENERGY", 0, 0 ),
						std::make_tuple( "*", "HEATING COIL HEATING RATE", 0, 0 ),
						std::make_tuple( "*", "COOLING COIL SENSIBLE COOLING ENERGY", 0, 0 ),
						std::make_tuple( "*", "COOLING COIL TOTAL COOLING ENERGY", 0, 0 ),
						std::make_tuple( "*", "COOLING COIL TOTAL COOLING RATE", 0, 0 ),
						std::make_tuple( "*", "COOLING COIL SENSIBLE COOLING RATE", 0, 0 ),
						std::make_tuple( "*", "COOLING COIL WETTED AREA FRACTION", 0, 0 ),
					}
				},
				{
					"PLANTLOOPDEMANDREPORTMONTHLY",
					{
						std::make_tuple( "*", "PLANT SUPPLY SIDE COOLING DEMAND RATE", 0, 0 ),
						std::make_tuple( "*", "PLANT SUPPLY SIDE HEATING DEMAND RATE", 0, 0 ),
					}
				},
				{
					"FANREPORTMONTHLY",
					{
						std::make_tuple( "*", "FAN ELECTRIC ENERGY", 0, 0 ),
						std::make_tuple( "*", "FAN RISE IN AIR TEMPERATURE", 0, 0 ),
						std::make_tuple( "*", "FAN ELECTRIC POWER", 0, 0 ),
					}
				},
				{
					"PUMPREPORTMONTHLY",
					{
						std::make_tuple( "*", "PUMP ELECTRIC ENERGY", 0, 0 ),
						std::make_tuple( "*", "PUMP FLUID HEAT GAIN ENERGY", 0, 0 ),
						std::make_tuple( "*", "PUMP ELECTRIC POWER", 0, 0 ),
						std::make_tuple( "*", "PUMP SHAFT POWER", 0, 0 ),
						std::make_tuple( "*", "PUMP FLUID HEAT GAIN RATE", 0, 0 ),
						std::make_tuple( "*", "PUMP OUTLET TEMPERATURE", 0, 0 ),
						std::make_tuple( "*", "PUMP MASS FLOW RATE", 0, 0 ),
					}
				},
				{
					"CONDLOOPDEMANDREPORTMONTHLY",
					{
						std::make_tuple( "*", "PLANT SUPPLY SIDE COOLING DEMAND RATE", 0, 0 ),
						std::make_tuple( "*", "PLANT SUPPLY SIDE HEATING DEMAND RATE", 0, 0 ),
						std::make_tuple( "*", "PLANT SUPPLY SIDE INLET TEMPERATURE", 0, 0 ),
						std::make_tuple( "*", "PLANT SUPPLY SIDE OUTLET TEMPERATURE", 0, 0 ),
					}
				},
				{
					"ZONETEMPERATUREOSCILLATIONREPORTMONTHLY",
					{
						std::make_tuple( "*", "ZONE OSCILLATING TEMPERATURES TIME", 0, 0 ),
						std::make_tuple( "*", "ZONE PEOPLE OCCUPANT COUNT", 0, 0 ),
					}
				},
				{
					"AIRLOOPSYSTEMENERGYANDWATERUSEMONTHLY",
					{
						std::make_tuple( "*", "AIR SYSTEM HOT WATER ENERGY", 0, 0 ),
						std::make_tuple( "*", "AIR SYSTEM STEAM ENERGY", 0, 0 ),
						std::make_tuple( "*", "AIR SYSTEM CHILLED WATER ENERGY", 0, 0 ),
						std::make_tuple( "*", "AIR SYSTEM ELECTRIC ENERGY", 0, 0 ),
						std::make_tuple( "*", "AIR SYSTEM GAS ENERGY", 0, 0 ),
						std::make_tuple( "*", "AIR SYSTEM WATER VOLUME", 0, 0 ),
					}
				},
				{
					"AIRLOOPSYSTEMCOMPONENTLOADSMONTHLY",
					{
						std::make_tuple( "*", "AIR SYSTEM FAN AIR HEATING ENERGY", 0, 0 ),
						std::make_tuple( "*", "AIR SYSTEM COOLING COIL TOTAL COOLING ENERGY", 0, 0 ),
						std::make_tuple( "*", "AIR SYSTEM HEATING COIL TOTAL HEATING ENERGY", 0, 0 ),
						std::make_tuple( "*", "AIR SYSTEM HEAT EXCHANGER TOTAL HEATING ENERGY", 0, 0 ),
						std::make_tuple( "*", "AIR SYSTEM HEAT EXCHANGER TOTAL COOLING ENERGY", 0, 0 ),
						std::make_tuple( "*", "AIR SYSTEM HUMIDIFIER TOTAL HEATING ENERGY", 0, 0 ),
						std::make_tuple( "*", "AIR SYSTEM EVAPORATIVE COOLER TOTAL COOLING ENERGY", 0, 0 ),
						std::make_tuple( "*", "AIR SYSTEM DESICCANT DEHUMIDIFIER TOTAL COOLING ENERGY", 0, 0 ),
					}
				},
				{
					"AIRLOOPSYSTEMCOMPONENTENERGYUSEMONTHLY",
					{
						std::make_tuple( "*", "AIR SYSTEM FAN ELECTRIC ENERGY", 0, 0 ),
						std::make_tuple( "*", "AIR SYSTEM HEATING COIL HOT WATER ENERGY", 0, 0 ),
						std::make_tuple( "*", "AIR SYSTEM COOLING COIL CHILLED WATER ENERGY", 0, 0 ),
						std::make_tuple( "*", "AIR SYSTEM DX HEATING COIL ELECTRIC ENERGY", 0, 0 ),
						std::make_tuple( "*", "AIR SYSTEM DX COOLING COIL ELECTRIC ENERGY", 0, 0 ),
						std::make_tuple( "*", "AIR SYSTEM HEATING COIL ELECTRIC ENERGY", 0, 0 ),
						std::make_tuple( "*", "AIR SYSTEM HEATING COIL GAS ENERGY", 0, 0 ),
						std::make_tuple( "*", "AIR SYSTEM HEATING COIL STEAM ENERGY", 0, 0 ),
						std::make_tuple( "*", "AIR SYSTEM HUMIDIFIER ELECTRIC ENERGY", 0, 0 ),
						std::make_tuple( "*", "AIR SYSTEM EVAPORATIVE COOLER ELECTRIC ENERGY", 0, 0 ),
						std::make_tuple( "*", "AIR SYSTEM DESICCANT DEHUMIDIFIER ELECTRIC ENERGY", 0, 0 ),
					}
				},
				{
					"MECHANICALVENTILATIONLOADSMONTHLY",
					{
						std::make_tuple( "*", "ZONE MECHANICAL VENTILATION NO LOAD HEAT REMOVAL ENERGY", 0, 0 ),
						std::make_tuple( "*", "ZONE MECHANICAL VENTILATION COOLING LOAD INCREASE ENERGY", 0, 0 ),
						std::make_tuple( "*", "ZONE MECHANICAL VENTILATION COOLING LOAD INCREASE DUE TO OVERHEATING ENERGY", 0, 0 ),
						std::make_tuple( "*", "ZONE MECHANICAL VENTILATION COOLING LOAD DECREASE ENERGY", 0, 0 ),
						std::make_tuple( "*", "ZONE MECHANICAL VENTILATION NO LOAD HEAT ADDITION ENERGY", 0, 0 ),
						std::make_tuple( "*", "ZONE MECHANICAL VENTILATION HEATING LOAD INCREASE ENERGY", 0, 0 ),
						std::make_tuple( "*", "ZONE MECHANICAL VENTILATION HEATING LOAD INCREASE DUE TO OVERCOOLING ENERGY", 0, 0 ),
						std::make_tuple( "*", "ZONE MECHANICAL VENTILATION HEATING LOAD DECREASE ENERGY", 0, 0 ),
						std::make_tuple( "*", "ZONE MECHANICAL VENTILATION AIR CHANGES PER HOUR", 0, 0 ),
					}
				}
			});

			for ( auto const result : results_map ) {
				int index = 1;
				DataOutputs::OutputVariablesForSimulation.allocate( 10000 );
				AddVariablesForMonthlyReport( result.first );
				for ( auto const result_tuple : result.second ) {
					EXPECT_EQ( std::get<0>( result_tuple ), DataOutputs::OutputVariablesForSimulation( index ).Key );
					EXPECT_EQ( std::get<1>( result_tuple ), DataOutputs::OutputVariablesForSimulation( index ).VarName );
					EXPECT_EQ( std::get<2>( result_tuple ), DataOutputs::OutputVariablesForSimulation( index ).Previous );
					EXPECT_EQ( std::get<3>( result_tuple ), DataOutputs::OutputVariablesForSimulation( index ).Next );
					++index;
				}
				DataOutputs::OutputVariablesForSimulation.deallocate();
				DataOutputs::NumConsideredOutputVariables = 0;
			}

		}

		TEST_F( InputProcessorFixture, preScanReportingVariables )
		{
			std::string const idf_objects = delimited_string({
				"Output:Variable,*,Site Outdoor Air Drybulb Temperature,timestep;",
				"Output:Variable,,Site Outdoor Air Drybulb Temperature,hourly;",
			});

			ASSERT_FALSE( process_idf( idf_objects ) );

			// reset global variable since process_idf() calls PreScanReportingVariables()
			DataOutputs::NumConsideredOutputVariables = 0;

			PreScanReportingVariables();

			EXPECT_FALSE( has_cout_output() );
			EXPECT_FALSE( has_cerr_output() );

			EXPECT_EQ( "*", DataOutputs::OutputVariablesForSimulation( 1 ).Key );
			EXPECT_EQ( "SITE OUTDOOR AIR DRYBULB TEMPERATURE", DataOutputs::OutputVariablesForSimulation( 1 ).VarName );
			EXPECT_EQ( 0, DataOutputs::OutputVariablesForSimulation( 1 ).Previous );
			EXPECT_EQ( 0, DataOutputs::OutputVariablesForSimulation( 1 ).Next );

		}

		TEST_F( InputProcessorFixture, initSecretObjects )
		{
			InitSecretObjects();

			EXPECT_EQ( 5, NumSecretObjects );

			EXPECT_EQ( "SKY RADIANCE DISTRIBUTION", RepObjects( 1 ).OldName );
			EXPECT_TRUE( RepObjects( 1 ).Deleted );

			EXPECT_EQ( "SURFACE:SHADING:DETACHED", RepObjects( 2 ).OldName );
			EXPECT_EQ( "Shading:Site:Detailed", RepObjects( 2 ).NewName );

			EXPECT_EQ( "AIRFLOW MODEL", RepObjects( 3 ).OldName );
			EXPECT_TRUE( RepObjects( 3 ).Deleted );

			EXPECT_EQ( "AIRFLOWNETWORK:MULTIZONE:SITEWINDCONDITIONS", RepObjects( 4 ).OldName );
			EXPECT_TRUE( RepObjects( 4 ).Deleted );

			EXPECT_EQ( "OUTPUT:REPORTS", RepObjects( 5 ).OldName );
			EXPECT_EQ( "various - depends on fields", RepObjects( 5 ).NewName );
			EXPECT_TRUE( RepObjects( 5 ).Deleted );
			EXPECT_TRUE( RepObjects( 5 ).TransitionDefer );
		}

	}

}
