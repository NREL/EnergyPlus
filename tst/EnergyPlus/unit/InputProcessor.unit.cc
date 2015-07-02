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
#include <typeinfo>

using namespace EnergyPlus;
using namespace EnergyPlus::InputProcessor;
using namespace ObjexxFCL;

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
		SectionDef.deallocate();
		SectionsOnFile.deallocate();
		ObjectStartRecord.deallocate();
		ObjectGotCount.deallocate();
		ObsoleteObjectsRepNames.deallocate();
		ListOfSections.deallocate();
		ListOfObjects.deallocate();
		iListOfObjects.deallocate();
		IDFRecordsGotten.deallocate();
		IDFRecords.deallocate();
		RepObjects.deallocate();
		LineItem.Numbers.deallocate();
		LineItem.NumBlank.deallocate();
		LineItem.Alphas.deallocate();
		LineItem.AlphBlank.deallocate();

		NumObjectDefs = 0;
		NumSectionDefs = 0;
		MaxObjectDefs = 0;
		MaxSectionDefs = 0;
		NumLines = 0;
		MaxIDFRecords = 0;
		NumIDFRecords = 0;
		MaxIDFSections = 0;
		NumIDFSections = 0;
		EchoInputFile = 0;
		InputLineLength = 0;
		MaxAlphaArgsFound = 0;
		MaxNumericArgsFound = 0;
		NumAlphaArgsFound = 0;
		NumNumericArgsFound = 0;
		MaxAlphaIDFArgsFound = 0;
		MaxNumericIDFArgsFound = 0;
		MaxAlphaIDFDefArgsFound = 0;
		MaxNumericIDFDefArgsFound = 0;
		NumOutOfRangeErrorsFound = 0;
		NumBlankReqFieldFound = 0;
		NumMiscErrorsFound = 0;
		MinimumNumberOfFields = 0;
		NumObsoleteObjects = 0;
		TotalAuditErrors = 0;
		NumSecretObjects = 0;
		ProcessingIDD = false;
		echo_stream = nullptr;

		InputLine = std::string();
		CurrentFieldName = std::string();
		ReplacementName = std::string();

		OverallErrorFlag = false;
		EchoInputLine = true;
		ReportRangeCheckErrors = true;
		FieldSet = false;
		RequiredField = false;
		RetainCaseFlag = false;
		ObsoleteObject = false;
		RequiredObject = false;
		UniqueObject = false;
		ExtensibleObject = false;
		ExtensibleNumFields = 0;

		EnergyPlusFixture::TearDown();  // Remember to tear down the base fixture after cleaning up derived fixture!
	}

	template < typename T >
	bool compareVectors( T const & correct, T const & to_check ) {
		auto const correct_size = correct.size();
		if ( correct_size != to_check.size() ) return false;
		if ( correct_size > 0 ) {
			if ( typeid( correct[ 0 ] ) != typeid( to_check[ 0 ] ) ) return false;
		}
		bool is_correct = true;
		for ( size_t i = 0; i < correct_size; ++i )
		{
			is_correct = ( correct[ i ] == to_check[ i ] );
			EXPECT_EQ( correct[ i ], to_check[ i ] ) << "Array index: " << i;
		}
		return is_correct;
	}

	bool processIDD( std::string const & idd, bool & errors_found ) {
		std::unique_ptr< std::istream > idd_stream;
		if( !idd.empty() ) {
			idd_stream = std::unique_ptr<std::istringstream>( new std::istringstream( idd ) );
		} else {
			auto const exeDirectory = FileSystem::getParentDirectoryPath( FileSystem::getAbsolutePath( FileSystem::getProgramPath() ) );
			auto const idd_location = exeDirectory + "Energy+.idd";

			EXPECT_TRUE( FileSystem::fileExists( idd_location ) ) << 
				"Energy+.idd does not exist at search location." << std::endl << "IDD search location: \"" << idd_location << "\""; 

			idd_stream = std::unique_ptr<std::ifstream>( new std::ifstream( idd_location, std::ios_base::in | std::ios_base::binary ) );
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

		ObjectStartRecord.dimension( NumObjectDefs, 0 );
		ObjectGotCount.dimension( NumObjectDefs, 0 );

		return errors_found;
	}

	void processIDF( std::string const & idf ) {
		auto idf_stream = std::unique_ptr<std::stringstream>( new std::stringstream );
		*idf_stream << idf << std::endl;

		ProcessingIDD = false;
		NumLines = 0;
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

TEST_F( InputProcessorFixture, processIDD_FullIDD )
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
	EXPECT_TRUE( compareVectors< Array1D_bool >( { true } , ObjectDef( index ).AlphaOrNumeric ) );
	EXPECT_TRUE( compareVectors< Array1D_bool >( { false } , ObjectDef( index ).ReqField ) );
	EXPECT_TRUE( compareVectors< Array1D_bool >( { false } , ObjectDef( index ).AlphRetainCase ) );
	EXPECT_TRUE( compareVectors< Array1D_string >( { "Option Type" }, ObjectDef( index ).AlphFieldChks ) );
	EXPECT_TRUE( compareVectors< Array1D_string >( { "" }, ObjectDef( index ).AlphFieldDefs ) );
	// EXPECT_TRUE( compareVectors< Array1D< RangeCheckDef > >( { RangeCheckDef() }, ObjectDef( index ).NumRangeChks ) );
	EXPECT_EQ( 0, ObjectDef( index ).NumFound );

}

TEST_F( InputProcessorFixture, processIDD )
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
	EXPECT_TRUE( compareVectors< Array1D_bool >( { true } , ObjectDef( 1 ).AlphaOrNumeric ) );
	EXPECT_TRUE( compareVectors< Array1D_bool >( { false } , ObjectDef( 1 ).ReqField ) );
	EXPECT_TRUE( compareVectors< Array1D_bool >( { false } , ObjectDef( 1 ).AlphRetainCase ) );
	EXPECT_TRUE( compareVectors< Array1D_string >( { "Option Type" }, ObjectDef( 1 ).AlphFieldChks ) );
	EXPECT_TRUE( compareVectors< Array1D_string >( { "" }, ObjectDef( 1 ).AlphFieldDefs ) );
	// EXPECT_TRUE( compareVectors< Array1D< RangeCheckDef > >( { RangeCheckDef() }, ObjectDef( 1 ).NumRangeChks ) );
	EXPECT_EQ( 0, ObjectDef( 1 ).NumFound );

}

TEST_F( InputProcessorFixture, addObjectDefandParse )
{
	ShowMessage( "Begin Test: InputProcessorFixture, addObjectDefandParse" );

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
	EXPECT_TRUE( compareVectors< Array1D_bool >( { true } , ObjectDef( 1 ).AlphaOrNumeric ) );
	EXPECT_TRUE( compareVectors< Array1D_bool >( { false } , ObjectDef( 1 ).ReqField ) );
	EXPECT_TRUE( compareVectors< Array1D_bool >( { false } , ObjectDef( 1 ).AlphRetainCase ) );
	EXPECT_TRUE( compareVectors< Array1D_string >( { "" }, ObjectDef( 1 ).AlphFieldChks ) );
	EXPECT_TRUE( compareVectors< Array1D_string >( { "" }, ObjectDef( 1 ).AlphFieldDefs ) );
	// EXPECT_TRUE( compareVectors< Array1D< RangeCheckDef > >( { RangeCheckDef() }, ObjectDef( 1 ).NumRangeChks ) );
	EXPECT_EQ( 0, ObjectDef( 1 ).NumFound );

}

TEST_F( InputProcessorFixture, validateObjectandParse )
{
	ShowMessage( "Begin Test: InputProcessorFixture, validateObjectandParse" );

	std::string const idf_objects = delimitedString({
		"Version,",
		"8.3;",
		"Output:SQLite,",
		"SimpleAndTabular;"
	});

	NumIDFRecords = 2;
	IDFRecords.allocate( 2 );

	auto idf_stream = std::unique_ptr<std::stringstream>( new std::stringstream( idf_objects ) );

	std::string const idd_objects = delimitedString({
		"Version,",
		"      \\memo Specifies the EnergyPlus version of the IDF file.",
		"      \\unique-object",
		"      \\format singleLine",
		"  A1 ; \\field Version Identifier",
		"      \\required-field",
		"      \\default 8.3",
		"Output:SQLite,",
		"       \\memo Output from EnergyPlus can be written to an SQLite format file.",
		"       \\unique-object",
		"  A1 ; \\field Option Type",
		"       \\type choice",
		"       \\key Simple",
		"       \\key SimpleAndTabular"
	});

	bool errors_found = false;

	ASSERT_FALSE( processIDD( idd_objects, errors_found ) ) << "Error processing IDD.";

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

	EXPECT_FALSE( hasCoutOutput() );
	EXPECT_FALSE( hasCerrOutput() );

	std::string const version_name( "VERSION" );

	auto index = FindItemInSortedList( version_name, ListOfObjects, NumObjectDefs );
	if ( index != 0 ) index = iListOfObjects( index );

	index = ObjectStartRecord( index );

	EXPECT_EQ( 1, index );

	EXPECT_EQ( version_name, IDFRecords( index ).Name );
	EXPECT_EQ( 1, IDFRecords( index ).NumAlphas );
	EXPECT_EQ( 0, IDFRecords( index ).NumNumbers );
	EXPECT_EQ( 1, IDFRecords( index ).ObjectDefPtr );
	EXPECT_TRUE( compareVectors< Array1D_string >( { "8.3" }, IDFRecords( index ).Alphas ) );
	EXPECT_TRUE( compareVectors< Array1D_bool >( { false }, IDFRecords( index ).AlphBlank ) );
	EXPECT_TRUE( compareVectors< Array1D< Real64 > >( {}, IDFRecords( index ).Numbers ) );
	EXPECT_TRUE( compareVectors< Array1D_bool >( {}, IDFRecords( index ).NumBlank ) );

	std::string const sqlite_name( "OUTPUT:SQLITE" );

	index = FindItemInSortedList( sqlite_name, ListOfObjects, NumObjectDefs );
	if ( index != 0 ) index = iListOfObjects( index );

	index = ObjectStartRecord( index );

	EXPECT_EQ( 2, index );

	EXPECT_EQ( sqlite_name, IDFRecords( index ).Name );
	EXPECT_EQ( 1, IDFRecords( index ).NumAlphas );
	EXPECT_EQ( 0, IDFRecords( index ).NumNumbers );
	EXPECT_EQ( 2, IDFRecords( index ).ObjectDefPtr );
	EXPECT_TRUE( compareVectors< Array1D_string >( { "SIMPLEANDTABULAR" }, IDFRecords( index ).Alphas ) );
	EXPECT_TRUE( compareVectors< Array1D_bool >( { false }, IDFRecords( index ).AlphBlank ) );
	EXPECT_TRUE( compareVectors< Array1D< Real64 > >( {}, IDFRecords( index ).Numbers ) );
	EXPECT_TRUE( compareVectors< Array1D_bool >( {}, IDFRecords( index ).NumBlank ) );

}

TEST_F( InputProcessorFixture, processIDF_FullIDD )
{
	std::string const idd_objects;

	bool errors_found = false;

	ASSERT_FALSE( processIDD( idd_objects, errors_found ) );

	std::string const idf_objects = delimitedString({
		"Version,",
		"8.3;",
		"Output:SQLite,",
		"SimpleAndTabular;"
	});

	processIDF( idf_objects );

	EXPECT_FALSE( OverallErrorFlag );

	EXPECT_FALSE( hasCoutOutput() );
	EXPECT_FALSE( hasCerrOutput() );

	std::string const version_name( "VERSION" );

	auto index = FindItemInSortedList( version_name, ListOfObjects, NumObjectDefs );
	if ( index != 0 ) index = iListOfObjects( index );

	index = ObjectStartRecord( index );

	EXPECT_EQ( 1, index );

	EXPECT_EQ( version_name, IDFRecords( index ).Name );
	EXPECT_EQ( 1, IDFRecords( index ).NumAlphas );
	EXPECT_EQ( 0, IDFRecords( index ).NumNumbers );
	EXPECT_EQ( 1, IDFRecords( index ).ObjectDefPtr );
	EXPECT_TRUE( compareVectors< Array1D_string >( { "8.3" }, IDFRecords( index ).Alphas ) );
	EXPECT_TRUE( compareVectors< Array1D_bool >( { false }, IDFRecords( index ).AlphBlank ) );
	EXPECT_TRUE( compareVectors< Array1D< Real64 > >( {}, IDFRecords( index ).Numbers ) );
	EXPECT_TRUE( compareVectors< Array1D_bool >( {}, IDFRecords( index ).NumBlank ) );

	std::string const sqlite_name( "OUTPUT:SQLITE" );

	index = FindItemInSortedList( sqlite_name, ListOfObjects, NumObjectDefs );
	if ( index != 0 ) index = iListOfObjects( index );

	index = ObjectStartRecord( index );

	EXPECT_EQ( 2, index );

	EXPECT_EQ( sqlite_name, IDFRecords( index ).Name );
	EXPECT_EQ( 1, IDFRecords( index ).NumAlphas );
	EXPECT_EQ( 0, IDFRecords( index ).NumNumbers );
	EXPECT_EQ( 739, IDFRecords( index ).ObjectDefPtr );
	EXPECT_TRUE( compareVectors< Array1D_string >( { "SIMPLEANDTABULAR" }, IDFRecords( index ).Alphas ) );
	EXPECT_TRUE( compareVectors< Array1D_bool >( { false }, IDFRecords( index ).AlphBlank ) );
	EXPECT_TRUE( compareVectors< Array1D< Real64 > >( {}, IDFRecords( index ).Numbers ) );
	EXPECT_TRUE( compareVectors< Array1D_bool >( {}, IDFRecords( index ).NumBlank ) );

}

TEST_F( InputProcessorFixture, processIDF )
{
	std::string const idd_objects = delimitedString({
		"Version,",
		"      \\memo Specifies the EnergyPlus version of the IDF file.",
		"      \\unique-object",
		"      \\format singleLine",
		"  A1 ; \\field Version Identifier",
		"      \\required-field",
		"      \\default 8.3",
		"Output:SQLite,",
		"       \\memo Output from EnergyPlus can be written to an SQLite format file.",
		"       \\unique-object",
		"  A1 ; \\field Option Type",
		"       \\type choice",
		"       \\key Simple",
		"       \\key SimpleAndTabular"
	});

	bool errors_found = false;

	ASSERT_FALSE( processIDD( idd_objects, errors_found ) ) << "Error processing IDD.";

	std::string const idf_objects = delimitedString({
		"Version,",
		"8.3;",
		"Output:SQLite,",
		"SimpleAndTabular;"
	});

	processIDF( idf_objects );

	EXPECT_FALSE( OverallErrorFlag );

	EXPECT_FALSE( hasCoutOutput() );
	EXPECT_FALSE( hasCerrOutput() );

	std::string const version_name( "VERSION" );

	auto index = FindItemInSortedList( version_name, ListOfObjects, NumObjectDefs );
	if ( index != 0 ) index = iListOfObjects( index );

	index = ObjectStartRecord( index );

	EXPECT_EQ( 1, index );

	EXPECT_EQ( version_name, IDFRecords( index ).Name );
	EXPECT_EQ( 1, IDFRecords( index ).NumAlphas );
	EXPECT_EQ( 0, IDFRecords( index ).NumNumbers );
	EXPECT_EQ( 1, IDFRecords( index ).ObjectDefPtr );
	EXPECT_TRUE( compareVectors< Array1D_string >( { "8.3" }, IDFRecords( index ).Alphas ) );
	EXPECT_TRUE( compareVectors< Array1D_bool >( { false }, IDFRecords( index ).AlphBlank ) );
	EXPECT_TRUE( compareVectors< Array1D< Real64 > >( {}, IDFRecords( index ).Numbers ) );
	EXPECT_TRUE( compareVectors< Array1D_bool >( {}, IDFRecords( index ).NumBlank ) );

	std::string const sqlite_name( "OUTPUT:SQLITE" );

	index = FindItemInSortedList( sqlite_name, ListOfObjects, NumObjectDefs );
	if ( index != 0 ) index = iListOfObjects( index );

	index = ObjectStartRecord( index );

	EXPECT_EQ( 2, index );

	EXPECT_EQ( sqlite_name, IDFRecords( index ).Name );
	EXPECT_EQ( 1, IDFRecords( index ).NumAlphas );
	EXPECT_EQ( 0, IDFRecords( index ).NumNumbers );
	EXPECT_EQ( 2, IDFRecords( index ).ObjectDefPtr );
	EXPECT_TRUE( compareVectors< Array1D_string >( { "SIMPLEANDTABULAR" }, IDFRecords( index ).Alphas ) );
	EXPECT_TRUE( compareVectors< Array1D_bool >( { false }, IDFRecords( index ).AlphBlank ) );
	EXPECT_TRUE( compareVectors< Array1D< Real64 > >( {}, IDFRecords( index ).Numbers ) );
	EXPECT_TRUE( compareVectors< Array1D_bool >( {}, IDFRecords( index ).NumBlank ) );

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
