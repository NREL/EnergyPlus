// IdfParser Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/IdfParserFixture.hh"

namespace EnergyPlus {

	TEST_F( IdfParserFixture, decode ) {
		std::string const test_object( delimited_string( {
			"Version,8.3;",
			"  Building,",
			"    Ref Bldg Medium Office New2004_v1.3_5.0,  !- Name",
			"    0.0000,                  !- North Axis {deg}",
			"    City,                    !- Terrain",
			"    0.0400,                  !- Loads Convergence Tolerance Value",
			"    0.2000,                  !- Temperature Convergence Tolerance Value {deltaC}",
			"    FullInteriorAndExterior, !- Solar Distribution",
			"    25,                      !- Maximum Number of Warmup Days",
			"    6;  ",
		} ) );

		auto const output = IdfParser::decode( test_object );

		EXPECT_EQ( std::vector< std::vector< std::string > >({ { "Version", "8.3" }, { "Building", "Ref Bldg Medium Office New2004_v1.3_5.0", "0.0000", "City", "0.0400", "0.2000", "FullInteriorAndExterior", "25", "6" } }), output );
	}

	TEST_F( IdfParserFixture, decode_success ) {
		bool success = true;
		std::string const test_object( delimited_string( {
			"Version,8.3;",
			"  Building,",
			"    Ref Bldg Medium Office New2004_v1.3_5.0,  !- Name",
			"    0.0000,                  !- North Axis {deg}",
			"    City,                    !- Terrain",
			"    0.0400,                  !- Loads Convergence Tolerance Value",
			"    0.2000,                  !- Temperature Convergence Tolerance Value {deltaC}",
			"    FullInteriorAndExterior, !- Solar Distribution",
			"    25,                      !- Maximum Number of Warmup Days",
			"    6;  ",
		} ) );

		auto const output = IdfParser::decode( test_object, success );

		EXPECT_EQ( std::vector< std::vector< std::string > >({ { "Version", "8.3" }, { "Building", "Ref Bldg Medium Office New2004_v1.3_5.0", "0.0000", "City", "0.0400", "0.2000", "FullInteriorAndExterior", "25", "6" } }), output );
		EXPECT_TRUE( success );
	}

	TEST_F( IdfParserFixture, parse_idf ) {
		size_t index = 0;
		bool success = true;
		std::string const test_object( delimited_string( {
			"Version,8.3;",
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

		auto const output = parse_idf( test_object, index, success );

		EXPECT_EQ( std::vector< std::vector< std::string > >({ { "Version", "8.3" }, { "Building", "Ref Bldg Medium Office New2004_v1.3_5.0", "0.0000", "City", "0.0400", "0.2000", "FullInteriorAndExterior", "25", "6" } }), output );
	#ifdef _WIN32
		// Windows has \r\n for line ending so it needs to be 2 back
		EXPECT_EQ( test_object.size() - 2, index );
	#else
		EXPECT_EQ( test_object.size() - 1, index );
	#endif
		EXPECT_TRUE( success );
	}

	TEST_F( IdfParserFixture, parse_object ) {
		size_t index = 0;
		bool success = true;
		std::string const test_object( delimited_string( {
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

		auto const output_vector = parse_object( test_object, index, success );
		EXPECT_EQ( std::vector< std::string >({ "Building", "Ref Bldg Medium Office New2004_v1.3_5.0", "0.0000", "City", "0.0400", "0.2000", "FullInteriorAndExterior", "25", "6" }), output_vector );
	#ifdef _WIN32
		// Windows has \r\n for line ending so it needs to be 2 back
		EXPECT_EQ( test_object.size() - 2, index );
	#else
		EXPECT_EQ( test_object.size() - 1, index );
	#endif
		EXPECT_TRUE( success );
	}

	TEST_F( IdfParserFixture, eat_whitespace ) {
		size_t index = 0;
		eat_whitespace( "    test", index );
		EXPECT_EQ( 4ul, index );

		index = 0;
		eat_whitespace( "t   test", index );
		EXPECT_EQ( 0ul, index );
	}

	TEST_F( IdfParserFixture, eat_comment ) {
		size_t index = 0;
		eat_comment( "!- North Axis {deg}\n", index );
		EXPECT_EQ( 20ul, index );

		index = 0;
		eat_comment( "                    !- Terrain\n", index );
		EXPECT_EQ( 31ul, index );

		index = 0;
		eat_comment( "  !- Name\n    0.0000", index );
		EXPECT_EQ( 10ul, index );

		index = 0;
		eat_comment( "  !- Name\n\r    0.0000", index );
		EXPECT_EQ( 10ul, index );
	}

	TEST_F( IdfParserFixture, parse_string ) {
		size_t index = 0;
		bool success = true;
		std::string output_string;

		output_string = parse_string( "test_string", index, success );
		EXPECT_EQ( "test_string", output_string );
		EXPECT_EQ( 11ul, index );
		EXPECT_TRUE( success );

		index = 0;
		success = true;
		output_string = parse_string( "test string", index, success );
		EXPECT_EQ( "test string", output_string );
		EXPECT_EQ( 11ul, index );
		EXPECT_TRUE( success );

		index = 0;
		success = true;
		output_string = parse_string( "-1234.1234", index, success );
		EXPECT_EQ( "-1234.1234", output_string );
		EXPECT_EQ( 10ul, index );
		EXPECT_TRUE( success );

		index = 0;
		success = true;
		output_string = parse_string( R"(\b\t/\\\";)", index, success );
		EXPECT_EQ( "\b\t/\\\"", output_string );
		EXPECT_EQ( 9ul, index );
		EXPECT_TRUE( success );

		index = 0;
		success = true;
		output_string = parse_string( R"(test \n string)", index, success );
		EXPECT_EQ( "", output_string );
		EXPECT_EQ( 7ul, index );
		EXPECT_FALSE( success );

		index = 0;
		success = true;
		output_string = parse_string( R"(! this is a comment \n)", index, success );
		EXPECT_EQ( "", output_string );
		EXPECT_EQ( 0ul, index );
		EXPECT_TRUE( success );
	}

	TEST_F( IdfParserFixture, parse_value ) {
		size_t index = 0;
		bool success = true;
		std::string output_string;

		output_string = parse_value( "test_string", index, success );
		EXPECT_EQ( "test_string", output_string );
		EXPECT_EQ( 11ul, index );
		EXPECT_TRUE( success );

		index = 0;
		success = true;
		output_string = parse_value( ", test_string", index, success );
		EXPECT_EQ( "", output_string );
		EXPECT_EQ( 0ul, index );
		EXPECT_FALSE( success );

		index = 0;
		success = true;
		output_string = parse_value( R"(test \n string)", index, success );
		EXPECT_EQ( "", output_string );
		EXPECT_EQ( 7ul, index );
		EXPECT_FALSE( success );

		index = 0;
		success = true;
		output_string = parse_value( "; test_string", index, success );
		EXPECT_EQ( "", output_string );
		EXPECT_EQ( 0ul, index );
		EXPECT_FALSE( success );

		index = 0;
		success = true;
		output_string = parse_value( "! test_string", index, success );
		EXPECT_EQ( "", output_string );
		EXPECT_EQ( 0ul, index );
		EXPECT_FALSE( success );
	}

	TEST_F( IdfParserFixture, look_ahead ) {
		std::string const test_input( "B , ! t ; `" );
		size_t index = 0;
		IdfParser::Token token = look_ahead( test_input, index );
		EXPECT_EQ( 0ul, index );
		EXPECT_EQ( IdfParser::Token::STRING, token );
		index = 2;
		token = look_ahead( test_input, index );
		EXPECT_EQ( 2ul, index );
		EXPECT_EQ( IdfParser::Token::COMMA, token );
		index = 3;
		token = look_ahead( test_input, index );
		EXPECT_EQ( 3ul, index );
		EXPECT_EQ( IdfParser::Token::EXCLAMATION, token );
		index = 5;
		token = look_ahead( test_input, index );
		EXPECT_EQ( 5ul, index );
		EXPECT_EQ( IdfParser::Token::STRING, token );
		index = 7;
		token = look_ahead( test_input, index );
		EXPECT_EQ( 7ul, index );
		EXPECT_EQ( IdfParser::Token::SEMICOLON, token );
		index = 9;
		token = look_ahead( test_input, index );
		EXPECT_EQ( 9ul, index );
		EXPECT_EQ( IdfParser::Token::NONE, token );
		index = test_input.size();
		token = look_ahead( test_input, index );
		EXPECT_EQ( test_input.size(), index );
		EXPECT_EQ( IdfParser::Token::END, token );

	}

	TEST_F( IdfParserFixture, next_token ) {
		size_t index = 0;

		std::string const test_input( "B , ! t ; `" );
		IdfParser::Token token = next_token( test_input, index );
		EXPECT_EQ( 1ul, index );
		EXPECT_EQ( IdfParser::Token::STRING, token );
		token = next_token( test_input, index );
		EXPECT_EQ( 3ul, index );
		EXPECT_EQ( IdfParser::Token::COMMA, token );
		token = next_token( test_input, index );
		EXPECT_EQ( 5ul, index );
		EXPECT_EQ( IdfParser::Token::EXCLAMATION, token );
		token = next_token( test_input, index );
		EXPECT_EQ( 7ul, index );
		EXPECT_EQ( IdfParser::Token::STRING, token );
		token = next_token( test_input, index );
		EXPECT_EQ( 9ul, index );
		EXPECT_EQ( IdfParser::Token::SEMICOLON, token );
		token = next_token( test_input, index );
		EXPECT_EQ( 10ul, index );
		EXPECT_EQ( IdfParser::Token::NONE, token );
		index = test_input.size();
		token = next_token( test_input, index );
		EXPECT_EQ( test_input.size() , index );
		EXPECT_EQ( IdfParser::Token::END, token );

	}

}
