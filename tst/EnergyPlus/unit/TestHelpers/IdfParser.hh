#ifndef IdfParser_hh_INCLUDED
#define IdfParser_hh_INCLUDED

#include <string>
#include <vector>

namespace EnergyPlus {

	// Recursive descent parser for IDF format. This using a vector< vector< string > > approach to store objects. Thus each object is stored
	// in the outer vector and a vector of each field is stored in the inner vector. This is a naive data structure for now since it works for
	// the original purpose. Could use a map, unordered_map, a new struct, or the IDFRecords data structure in the future.
	// This should not be used outside of the EnergyPlusFixture.
	// This parser was created to facilite searching an IDF snippet before it is run through the E+ InputProcessor.
	class IdfParser
	{
	public:
		static std::vector< std::vector< std::string > > decode( std::string const & idf );

		static std::vector< std::vector< std::string > > decode( std::string const & idf, bool & success );
		
		enum class Token : size_t { NONE = 0, END = 1, EXCLAMATION = 2, COMMA = 3, SEMICOLON = 4, STRING = 5 };

	private:
		friend class IdfParserFixture;
		
		static std::vector< std::vector< std::string > > parse_idf( std::string const & idf, size_t & index, bool & success );

		static std::vector< std::string > parse_object( std::string const & idf, size_t & index, bool & success );

		static std::string parse_value( std::string const & idf, size_t & index, bool & success);

		static std::string parse_string( std::string const & idf, size_t & index, bool & success);

		static void eat_whitespace( std::string const & idf, size_t & index);

		static void eat_comment( std::string const & idf, size_t & index);

		static Token look_ahead( std::string const & idf, size_t index);

		static Token next_token( std::string const & idf, size_t & index);
	};

}

#endif
