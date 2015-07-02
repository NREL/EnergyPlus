// EnergyPlus::InputProcessor Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/InputProcessor.hh>
#include <EnergyPlus/DataSystemVariables.hh>
#include <EnergyPlus/SortAndStringUtilities.hh>
#include <EnergyPlus/FileSystem.hh>

#include <fstream>

using namespace EnergyPlus;
using namespace EnergyPlus::InputProcessor;
using namespace ObjexxFCL;
// using namespace DataGlobals;

namespace EnergyPlus {

class InputProcessorFixture : public EnergyPlusFixture
{
protected:
	virtual void SetUp() {
		EnergyPlusFixture::SetUp();  // Sets up the base fixture first.
	}

	virtual void TearDown() {
		if ( NumObjectDefs > 0 && ObjectDef.size() > 0 ) {
			ObjectDef( NumObjectDefs ).AlphaOrNumeric.deallocate();
			ObjectDef( NumObjectDefs ).NumRangeChks.deallocate();
			ObjectDef( NumObjectDefs ).AlphFieldChks.deallocate();
			ObjectDef( NumObjectDefs ).AlphFieldDefs.deallocate();
			ObjectDef( NumObjectDefs ).ReqField.deallocate();
			ObjectDef( NumObjectDefs ).AlphRetainCase.deallocate();
		}
		ObjectDef.deallocate();
		NumObjectDefs = 0;
		ObsoleteObjectsRepNames.deallocate();

		ListOfObjects.deallocate();
		iListOfObjects.deallocate();

		SectionsOnFile.deallocate();
		IDFRecords.deallocate();
		// LineItem.Numbers.deallocate( MaxNumericArgsFound );
		// LineItem.NumBlank.deallocate( MaxNumericArgsFound );
		// LineItem.Alphas.deallocate( MaxAlphaArgsFound );
		// LineItem.AlphBlank.deallocate( MaxAlphaArgsFound );
		MaxNumericArgsFound = 0;
		MaxAlphaArgsFound = 0;

		EnergyPlusFixture::TearDown();  // Remember to tear down the base fixture after cleaning up derived fixture!
	}

	bool processIDD( std::string const & idd, bool & errors_found ) {
		std::unique_ptr< std::istream > idd_stream;
		if( !idd.empty() ) {
			idd_stream = std::unique_ptr<std::istringstream>( new std::istringstream( idd ) );
		} else {
			auto const exeDirectory = FileSystem::getParentDirectoryPath( FileSystem::getAbsolutePath( FileSystem::getProgramPath() ) );
			auto const idd_location = exeDirectory + "Energy+.idd";

			idd_stream = std::unique_ptr<std::ifstream>( new std::ifstream( idd_location, std::ios_base::in | std::ios_base::binary ) );

			// static std::vector< std::string > const possible_idd_locations( { "Energy+.idd", "Products/Energy+.idd", "../Energy+.idd" } );
			// for( auto const & idd_location : possible_idd_locations ) {
			// 	idd_stream = std::unique_ptr<std::ifstream>( new std::ifstream( idd_location, std::ios_base::in | std::ios_base::binary ) );
			// 	if ( idd_stream->good() ) {
			// 		break;
			// 	} else {
			// 		continue;
			// 	}
			// }
		}

		if ( ! idd_stream->good() ) {
			errors_found = true;
			return errors_found;
		}

		ProcessingIDD = true;
		DataSystemVariables::SortedIDD = true;
		ProcessDataDicFile( *idd_stream, errors_found );
		ProcessingIDD = false;

		if( !errors_found ) {
			ListOfObjects.allocate( NumObjectDefs );
			ListOfObjects = ObjectDef( {1,NumObjectDefs} ).Name();
			if ( DataSystemVariables::SortedIDD ) {
				iListOfObjects.allocate( NumObjectDefs );
				SortAndStringUtilities::SetupAndSort( ListOfObjects, iListOfObjects );
			}
		}

		return errors_found;
	}

	void processIDF( std::string const & idf ) {
		auto idf_stream = std::unique_ptr<std::stringstream>( new std::stringstream );
		*idf_stream << idf << std::endl;

		ProcessingIDD = false;
		ProcessInputDataFile( *idf_stream );
	}
};

TEST_F( InputProcessorFixture, findItemInSortedList )
{
	ShowMessage( "Begin Test: InputProcessorFixture, findItemInSortedList" );

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
	ShowMessage( "Begin Test: InputProcessorFixture, findItemInSortedList" );

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
	ShowMessage( "Begin Test: InputProcessorFixture, findItemInList" );

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

TEST_F( InputProcessorFixture, processDataDicFile_FullIDD )
{
	std::string const idd_objects;

	bool errors_found = false;

	ASSERT_FALSE( processIDD( idd_objects, errors_found ) );

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
	// EXPECT_EQ( 1, ObjectDef( index ).AlphaOrNumeric );
	// EXPECT_EQ( 1, ObjectDef( index ).ReqField );
	// EXPECT_EQ( 1, ObjectDef( index ).AlphRetainCase );
	// EXPECT_EQ( 1, ObjectDef( index ).AlphFieldChks );
	// EXPECT_EQ( 1, ObjectDef( index ).AlphFieldDefs );
	// EXPECT_EQ( 1, ObjectDef( index ).NumRangeChks );
	EXPECT_EQ( 0, ObjectDef( index ).NumFound );

}

TEST_F( InputProcessorFixture, processDataDicFile )
{
	std::string const idd_objects = delimitedString({
		"Output:SQLite,",
		"       \\memo Output from EnergyPlus can be written to an SQLite format file.",
		"       \\unique-object",
		"  A1 ; \\field Option Type",
		"       \\type choice",
		"       \\key Simple",
		"       \\key SimpleAndTabular"
	});

	bool errors_found = false;

	ASSERT_FALSE( processIDD( idd_objects, errors_found ) );

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
	// EXPECT_EQ( 1, ObjectDef( 1 ).AlphaOrNumeric );
	// EXPECT_EQ( 1, ObjectDef( 1 ).ReqField );
	// EXPECT_EQ( 1, ObjectDef( 1 ).AlphRetainCase );
	// EXPECT_EQ( 1, ObjectDef( 1 ).AlphFieldChks );
	// EXPECT_EQ( 1, ObjectDef( 1 ).AlphFieldDefs );
	// EXPECT_EQ( 1, ObjectDef( 1 ).NumRangeChks );
	EXPECT_EQ( 0, ObjectDef( 1 ).NumFound );

}

TEST_F( InputProcessorFixture, addObjectDefandParse )
{
	ShowMessage( "Begin Test: InputProcessorFixture, addObjectDefandParse" );

	ProcessingIDD = true;
	std::string const idd_objects = 
		"Output:SQLite,\n"
		"       \\memo Output from EnergyPlus can be written to an SQLite format file.\n"
		"       \\unique-object\n"
		"  A1 ; \field Option Type\n"
		"       \type choice\n"
		"       \\key Simple\n"
		"       \\key SimpleAndTabular\n";

	auto idd_stream = std::unique_ptr<std::stringstream>( new std::stringstream( idd_objects ) );

	std::string const proposed_object = "Output:SQLite";

	auto current_pos = idd_objects.find_first_of( ',' );

	EXPECT_EQ( 13ul, current_pos );

	bool end_of_file = false;
	bool errors_found = false;

	ObjectDef.allocate( 100 );

	AddObjectDefandParse( *idd_stream, proposed_object, current_pos, end_of_file, errors_found );

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
	// EXPECT_EQ( Array1D_bool( { false } ) , ObjectDef( 1 ).AlphaOrNumeric );
	// EXPECT_EQ( Array1D_bool( { false } ) , ObjectDef( 1 ).ReqField );
	// EXPECT_EQ( Array1D_bool( { false } ) , ObjectDef( 1 ).AlphRetainCase );
	// EXPECT_EQ( Array1D_string( { "" } ), ObjectDef( 1 ).AlphFieldChks );
	// EXPECT_EQ( Array1D_string( { "" } ), ObjectDef( 1 ).AlphFieldDefs );
	// EXPECT_EQ( Array1< RangeCheckDef >( RangeCheckDef() ), ObjectDef( 1 ).NumRangeChks );
	EXPECT_EQ( 0, ObjectDef( 1 ).NumFound );

	ProcessingIDD = false;
}

// TEST_F( InputProcessorFixture, getNumObjectsFound )
// {
// 	ShowMessage( "Begin Test: InputProcessorFixture, getNumObjectsFound" );

// 	InputProcessor::ListOfObjects = Array1D_string ({
// 		"OUTPUT:METER",
// 		"OUTPUT:METER:CUMULATIVE",
// 		"OUTPUT:METER:CUMULATIVE:METERFILEONLY",
// 		"OUTPUT:METER:METERFILEONLY",
// 		"OUTPUT:SQLITE",
// 		"OUTPUT:VARIABLE"
// 	});
// 	InputProcessor::NumObjectDefs = ListOfObjects.size();

// 	InputProcessor::ObjectDef = Array1D< ObjectsDefinition > ({
// 		ObjectsDefinition( "Unused", 4, 4, 0, 2, false, false, false, false, 0, 0, 0, 0, Array1_bool({}), Array1_bool({}), Array1_bool({}), Array1_string({}), Array1_string({}), Array1< RangeCheckDef >({}), 2 )
// 	});

// // 	int const NumParams, // Number of parameters to be processed for each object
// // 	int const NumAlpha, // Number of Alpha elements in the object
// // 	int const NumNumeric, // Number of Numeric elements in the object
// // 	int const MinNumFields, // Minimum number of fields to be passed to the Get routines
// // 	bool const NameAlpha1, // True if the first alpha appears to "name" the object for error messages
// // 	bool const UniqueObject, // True if this object has been designated \unique-object
// // 	bool const RequiredObject, // True if this object has been designated \required-object
// // 	bool const ExtensibleObject, // True if this object has been designated \extensible
// // 	int const ExtensibleNum, // how many fields to extend
// // 	int const LastExtendAlpha, // Count for extended alpha fields
// // 	int const LastExtendNum, // Count for extended numeric fields
// // 	int const ObsPtr, // If > 0, object is obsolete and this is the
// // 	Array1_bool const & AlphaOrNumeric, // Positionally, whether the argument
// // 	Array1_bool const & ReqField, // True for required fields
// // 	Array1_bool const & AlphRetainCase, // true if retaincase is set for this field (alpha fields only)
// // 	Array1_string const & AlphFieldChks, // Field names for alphas
// // 	Array1_string const & AlphFieldDefs, // Defaults for alphas
// // 	Array1< RangeCheckDef > const & NumRangeChks, // Used to range check and default numeric fields
// // 	int const NumFound // Number of this object found in IDF

// // 	NumParams	int	4	4
// // NumAlpha	int	4	4
// // NumNumeric	int	0	0
// // MinNumFields	int	2	2
// // NameAlpha1	bool	false	false
// // UniqueObject	bool	false	false
// // RequiredObject	bool	false	false
// // ExtensibleObject	bool	false	false
// // ExtensibleNum	int	0	0
// // LastExtendAlpha	int	0	0
// // LastExtendNum	int	0	0
// // ObsPtr	int	0	0
// // AlphaOrNumeric	ObjexxFCL::Array1D_bool		
// // ReqField	ObjexxFCL::Array1D_bool		
// // AlphRetainCase	ObjexxFCL::Array1D_bool		
// // AlphFieldChks	ObjexxFCL::Array1D_string		
// // AlphFieldDefs	ObjexxFCL::Array1D_string		
// // NumRangeChks	ObjexxFCL::Array1D<EnergyPlus::InputProcessor::RangeCheckDef>		
// // NumFound	int	2	2

// 	EXPECT_EQ( 1ul, ObjectDef.size() );

// 	// ListOfObjects.allocate( NumObjectDefs );
// 	// ListOfObjects = ObjectDef( {1,NumObjectDefs} ).Name();

// 	// GetNumObjectsFound( "" );

// 	InputProcessor::NumObjectDefs = 0;
// 	InputProcessor::ListOfObjects.deallocate();
// 	InputProcessor::ObjectDef.deallocate();
// }

}
