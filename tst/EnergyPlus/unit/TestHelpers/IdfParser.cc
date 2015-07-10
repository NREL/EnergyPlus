#include "IdfParser.hh"

namespace EnergyPlus {

	std::vector< std::vector< std::string > > IdfParser::decode( std::string const & idf )
	{
		bool success = true;
		return decode( idf, success );
	}

	std::vector< std::vector< std::string > > IdfParser::decode( std::string const & idf, bool & success )
	{
		success = true;
		if ( idf.empty() ) return std::vector< std::vector< std::string > >();

		size_t index = 0;
		return parse_idf( idf, index, success );
	}

	std::vector< std::vector< std::string > > IdfParser::parse_idf( std::string const & idf, size_t & index, bool & success )
	{
		std::vector< std::vector< std::string > > obj;
		Token token;

		bool done = false;
		while ( !done ) {
			token = look_ahead( idf, index );
			if ( token == Token::END ) {
				break;
			} else if ( token == Token::NONE ) {
				success = false;
				return std::vector< std::vector< std::string > >();
			} else if ( token == Token::EXCLAMATION ) {
				eat_comment( idf, index );
			} else {

				auto const array = parse_object( idf, index, success );
				if ( ! array.empty() ) obj.push_back( array );
			}
		}

		return obj;
	}

	std::vector< std::string > IdfParser::parse_object( std::string const & idf, size_t & index, bool & success )
	{
		std::vector< std::string > array;
		Token token;

		bool done = false;
		while ( !done ) {
			token = look_ahead( idf, index );
			if ( token == Token::NONE ) {
				success = false;
				return std::vector< std::string >();
			} else if ( token == Token::COMMA ) {
				next_token( idf, index );
			} else if ( token == Token::SEMICOLON ) {
				next_token( idf, index );
				break;
			} else if ( token == Token::EXCLAMATION ) {
				eat_comment( idf, index );
			} else {
				std::string const value = parse_value( idf, index, success );
				if ( !success ) {
					return std::vector< std::string >();
				}
				array.push_back( value );
			}
		}

		return array;
	}

	std::string IdfParser::parse_value( std::string const & idf, size_t & index, bool & success)
	{
		switch ( look_ahead( idf, index ) ) {
			case Token::STRING:
				return parse_string( idf, index, success );
			case Token::NONE: case Token::END: case Token::EXCLAMATION: 
			case Token::COMMA: case Token::SEMICOLON:
				break;
		}

		success = false;
		return std::string();
	}

	std::string IdfParser::parse_string( std::string const & idf, size_t & index, bool & success)
	{
		eat_whitespace( idf, index );

		std::string s;
		char c;

		auto const idf_size = idf.size();

		bool complete = false;
		while ( !complete ) {
			if ( index == idf_size ) {
				complete = true;
				break;
			}

			c = idf[ index++ ];
			if ( c == ',' ) {
				complete = true;
				index--;
				break;
			} else if ( c == ';' ) {
				complete = true;
				index--;
				break;
			} else if ( c == '!' ) {
				complete = true;
				index--;
				break;
			} else if ( c == '\\' ) {
				if ( index == idf_size ) break;
				c = idf[ index++ ];
				if ( c == '"' ) {
					s += '"';
				} else if ( c == '\\' ) {
					s += '\\';
				} else if ( c == '/' ) {
					s += '/';
				} else if ( c == 'b' ) {
					s += '\b';
				} else if ( c == 't' ) {
					s += '\t';
				} else if ( c == 'n' ) {
					complete = false;
					break;
				} else if ( c == 'r' ) {
					complete = false;
					break;
				}
			} else {
				s += c;
			}
		}

		if ( !complete ) {
			success = false;
			return std::string();
		}

		return s;
	}

	void IdfParser::eat_whitespace( std::string const & idf, size_t & index)
	{
		for (; index < idf.size(); index++) {
			switch( idf[ index ] ) {
				case ' ': case '\n': case '\r': case '\t':
					continue;
				default:
					return;
			}
		}
	}

	void IdfParser::eat_comment( std::string const & idf, size_t & index)
	{
		auto const idf_size = idf.size();
		while ( true ) {
			if ( index == idf_size ) break;
			if ( idf[ index++ ] == '\n' ) break;
		}
	}

	IdfParser::Token IdfParser::look_ahead( std::string const & idf, size_t index)
	{
		size_t save_index = index;
		return next_token( idf, save_index );
	}

	IdfParser::Token IdfParser::next_token( std::string const & idf, size_t & index)
	{
		eat_whitespace( idf, index );

		if ( index == idf.size() ) {
			return Token::END;
		}

		char const c = idf[ index ];
		index++;
		switch (c) {
			case '!':
				return Token::EXCLAMATION;
			case ',':
				return Token::COMMA;
			case ';':
				return Token::SEMICOLON;
			default:
				static std::string const search_chars( "-:.#/\\[]{}_@$%^&*()|+=<>?'\"~" );
				if ( isalnum( c ) || ( std::string::npos != search_chars.find_first_of( c ) ) ) {
					return Token::STRING;
				}
				break;
		}
		index--;
		return Token::NONE;
	}

}
