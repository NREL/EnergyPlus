#ifndef IdfParserFixture_hh_INCLUDED
#define IdfParserFixture_hh_INCLUDED

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "EnergyPlusFixture.hh"

namespace EnergyPlus {

	class IdfParserFixture : public EnergyPlusFixture
	{
	protected:
		static void SetUpTestCase() { }
		static void TearDownTestCase() { }

		virtual void SetUp() {
			EnergyPlusFixture::SetUp();  // Sets up the base fixture first.
		}

		virtual void TearDown() {
			EnergyPlusFixture::TearDown();  // Remember to tear down the base fixture after cleaning up derived fixture!
		}

		void eat_whitespace( std::string const & idf, size_t & index ) {
			IdfParser parser;
			parser.eat_whitespace( idf, index );
		}

		void eat_comment( std::string const & idf, size_t & index ) {
			IdfParser parser;
			parser.eat_comment( idf, index );
		}

		std::string parse_string( std::string const & idf, size_t & index, bool & success) {
			IdfParser parser;
			return parser.parse_string( idf, index, success );
		}

		std::string parse_value( std::string const & idf, size_t & index, bool & success) {
			IdfParser parser;
			return parser.parse_value( idf, index, success );
		}

		size_t look_ahead( std::string const & idf, size_t index)
		{
			IdfParser parser;
			return parser.look_ahead( idf, index );
		}

		size_t next_token( std::string const & idf, size_t & index)
		{
			IdfParser parser;
			return parser.next_token( idf, index );
		}

		std::vector< std::vector< std::string > > parse_array( std::string const & idf, size_t & index, bool & success ) {
			IdfParser parser;
			return parser.parse_array( idf, index, success );
		}

		std::vector< std::string > parse_object( std::string const & idf, size_t & index, bool & success ) {
			IdfParser parser;
			return parser.parse_object( idf, index, success );
		}

		std::vector< std::vector< std::string > > decode( std::string const & idf ) {
			IdfParser parser;
			return parser.decode( idf );
		}

		std::vector< std::vector< std::string > > decode( std::string const & idf, bool & success ) {
			IdfParser parser;
			return parser.decode( idf, success );
		}
		
	};

	typedef IdfParserFixture IdfParserDeathTestFixture;
}

#endif
