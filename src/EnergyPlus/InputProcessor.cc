// EnergyPlus, Copyright (c) 1996-2017, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
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
//     similar designation, without the U.S. Department of Energy's prior written consent.
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

// C++ Headers
#include <algorithm>
#include <istream>
#include <iostream>
#include <unordered_set>

// ObjexxFCL Headers
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/Array1S.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <InputProcessor.hh>
#include <DataIPShortCuts.hh>
#include <DataOutputs.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DataStringGlobals.hh>
#include <DataSystemVariables.hh>
#include <DisplayRoutines.hh>
#include <SortAndStringUtilities.hh>
#include <FileSystem.hh>
#include <milo/dtoa.hpp>
#include <milo/itoa.hpp>

typedef std::unordered_map < std::string, std::string > UnorderedObjectTypeMap;
typedef std::unordered_map < std::string, std::pair < json::const_iterator, std::vector <json::const_iterator> > > UnorderedObjectCacheMap;
typedef std::map < const json::object_t * const, std::pair < std::string, std::string > > UnorderedUnusedObjectMap;

namespace EnergyPlus {
	// initialization of class static variables
	json InputProcessor::jdf = json();
	json InputProcessor::schema = json();
	IdfParser InputProcessor::idf_parser = IdfParser();
	State InputProcessor::state = State();
	char InputProcessor::s[] = { 0 };
	std::ostream * InputProcessor::echo_stream = nullptr;

	UnorderedObjectTypeMap InputProcessor::case_insensitive_object_map = UnorderedObjectTypeMap();
	UnorderedObjectCacheMap InputProcessor::jdd_jdf_cache_map = UnorderedObjectCacheMap();
	UnorderedUnusedObjectMap InputProcessor::unused_inputs = UnorderedUnusedObjectMap();
}

bool icompare( std::string const & s1, std::string const & s2 ) {
	return s1.length() == s2.length() &&
		std::equal(
			s1.begin(),
			s1.end(),
			s2.begin(),
			[ ]( unsigned char c1, unsigned char c2 ) { return ::tolower( c1 ) == ::tolower( c2 );}
		);
}


json IdfParser::decode( std::string const & idf, json const & schema ) {
	bool success = true;
	return decode( idf, schema, success );
}

json IdfParser::decode( std::string const & idf, json const & schema, bool & success ) {
	success = true;
	if ( idf.empty() ) {
		success = false;
		return nullptr;
	}

	size_t index = 0;
	return parse_idf( idf, index, success, schema );
}

std::string IdfParser::encode( json const & root, json const & schema ) {
	std::string end_of_field( "," + EnergyPlus::DataStringGlobals::NL + "  " );
	std::string end_of_object( ";" + EnergyPlus::DataStringGlobals::NL + EnergyPlus::DataStringGlobals::NL );

	std::string encoded, extension_key;

	for ( auto obj = root.begin(); obj != root.end(); ++obj ) {
		const auto & legacy_idd = schema[ "properties" ][ obj.key() ][ "legacy_idd" ];
		const auto & legacy_idd_field = legacy_idd[ "fields" ];
		auto key = legacy_idd.find( "extension" );
		if ( key != legacy_idd.end() ) {
			extension_key = key.value();
		}
		for ( auto obj_in = obj.value().begin(); obj_in != obj.value().end(); ++obj_in ) {
			encoded += obj.key();
			size_t skipped_fields = 0;
			for ( size_t i = 0; i < legacy_idd_field.size(); i++ ) {
				std::string const & entry = legacy_idd_field[ i ];
				if ( obj_in.value().find( entry ) == obj_in.value().end() ) {
					if ( entry == "name" ) encoded += end_of_field + obj_in.key();
					else skipped_fields++;
					continue;
				}
				for ( size_t j = 0; j < skipped_fields; j++ ) encoded += "," + EnergyPlus::DataStringGlobals::NL + "  ";
				skipped_fields = 0;
				encoded += end_of_field;
				auto const & val = obj_in.value()[ entry ];
				if ( val.is_string() ) {
					encoded += val.get < std::string >();
				} else {
					dtoa( val.get < double >(), s );
					encoded += s;
				}
			}

			if ( obj_in.value().find( extension_key ) == obj_in.value().end() ) {
				encoded += end_of_object;
				continue;
			}

			auto & extensions = obj_in.value()[ extension_key ];
			for ( size_t extension_i = 0; extension_i < extensions.size(); extension_i++ ) {
				auto const & cur_extension_obj = extensions[ extension_i ];
				auto const & extensible = schema[ "properties" ][ obj.key() ][ "legacy_idd" ][ "extensibles" ];
				for ( size_t i = 0; i < extensible.size(); i++ ) {
					std::string const & tmp = extensible[ i ];
					if ( cur_extension_obj.find( tmp ) == cur_extension_obj.end() ) {
						skipped_fields++;
						continue;
					}
					for ( size_t j = 0; j < skipped_fields; j++ ) encoded += end_of_field;
					skipped_fields = 0;
					encoded += end_of_field;
					if ( cur_extension_obj[ tmp ].is_string() ) {
						encoded += cur_extension_obj[ tmp ];
					} else {
						dtoa( cur_extension_obj[ tmp ].get < double >(), s );
						encoded += s;
					}
				}
			}
			encoded += end_of_object;
		}
	}
	return encoded;
}

json IdfParser::parse_idf( std::string const & idf, size_t & index, bool & success, json const & schema ) {
	json root;
	Token token;
	auto const & schema_properties = schema[ "properties" ];

	while ( true ) {
		token = look_ahead( idf, index );
		if ( token == Token::END ) {
			break;
		} else if ( token == Token::NONE ) {
			success = false;
			return root;
		} else if ( token == Token::EXCLAMATION ) {
			eat_comment( idf, index );
		} else {
			std::string obj_name = parse_string( idf, index, success );

			auto const converted = EnergyPlus::InputProcessor::ConvertInsensitiveObjectType( obj_name );
			if ( converted.first ) {
				obj_name = converted.second;
			} else {
				print_out_line_error( idf, false );
				while ( token != Token::SEMICOLON && token != Token::END ) token = next_token( idf, index );
				continue;
			}

			json const & obj_loc = schema_properties[ obj_name ];
			json const & legacy_idd = obj_loc[ "legacy_idd" ];
			json obj = parse_object( idf, index, success, legacy_idd, obj_loc );
			if ( !success ) print_out_line_error( idf, true );
			u64toa( root[ obj_name ].size() + 1, s );
			std::string name = obj_name + " " + s;
			if ( !obj.is_null() ) {
				auto const & name_iter = obj.find( "name" );
				if ( name_iter != obj.end() ) {
					name = name_iter.value();
					obj.erase( name_iter );
					if ( root[ obj_name ].find( name ) != root[ obj_name ].end() ) {
						// hacky but needed to warn if there are duplicate names in parsed IDF
						if ( obj_name != "RunPeriod" ) {
							EnergyPlus::ShowWarningMessage("Duplicate name found. name: \"" + name + "\"");
						}
					}
				}
			}

			root[ obj_name ][ name ] = std::move( obj );
		}
	}

	return root;
}

json IdfParser::parse_object( std::string const & idf, size_t & index, bool & success,
							  json const & legacy_idd, json const & schema_obj_loc ) {
	json root = json::object();
	json extensible = json::object();
	json array_of_extensions = json::array();
	Token token;
	std::string extension_key;
	size_t legacy_idd_index = 0, extensible_index = 0;
	success = true;
	bool was_value_parsed = false;
	auto const & legacy_idd_fields_array = legacy_idd[ "fields" ];
	auto const & legacy_idd_extensibles_iter = legacy_idd.find( "extensibles" );

	auto const & schema_patternProperties = schema_obj_loc[ "patternProperties" ];
	auto const & schema_dot_star = schema_patternProperties[ ".*" ];
	auto const & schema_obj_props = schema_dot_star[ "properties" ];
	auto key = legacy_idd.find( "extension" );

	json const * schema_obj_extensions = nullptr;
	if ( legacy_idd_extensibles_iter != legacy_idd.end() ) {
		extension_key = key.value();
		schema_obj_extensions = & schema_obj_props[ extension_key ][ "items" ][ "properties" ];
	}

	auto const & found_min_fields = schema_obj_loc.find( "min_fields" );

	index += 1;

	while ( true ) {
		token = look_ahead( idf, index );
		if ( token == Token::NONE ) {
			success = false;
			return root;
		} else if ( token == Token::END ) {
			return root;
		} else if ( token == Token::COMMA || token == Token::SEMICOLON ) {
			if ( !was_value_parsed ) {
				int ext_size = 0;
				if ( legacy_idd_index < legacy_idd_fields_array.size() ) {
					std::string const & field_name = legacy_idd_fields_array[ legacy_idd_index ];
					root[ field_name ] = "";
				} else {
					auto const & legacy_idd_extensibles_array = legacy_idd_extensibles_iter.value();
					ext_size = static_cast<int>( legacy_idd_extensibles_array.size() );
					std::string const & field_name = legacy_idd_extensibles_array[ extensible_index % ext_size ];
					extensible_index++;
					extensible[ field_name ] = "";
				}
				if ( ext_size && extensible_index % ext_size == 0 ) {
					array_of_extensions.push_back( extensible );
					extensible.clear();
				}
			}
			legacy_idd_index++;
			was_value_parsed = false;
			next_token( idf, index );
			if ( token == Token::SEMICOLON ) {
				size_t min_fields = 0;
				if ( found_min_fields != schema_obj_loc.end() ) {
					min_fields = found_min_fields.value();
				}
				for (; legacy_idd_index < min_fields; legacy_idd_index++) {
					std::string const & field_name = legacy_idd_fields_array[ legacy_idd_index ];
					root[ field_name ] = "";
				}
				if ( extensible.size() ) {
					array_of_extensions.push_back( extensible );
					extensible.clear();
				}
				break;
			}
		} else if ( token == Token::EXCLAMATION ) {
			eat_comment( idf, index );
		} else if ( legacy_idd_index >= legacy_idd_fields_array.size() ) {
			if ( legacy_idd_extensibles_iter == legacy_idd.end() ) {
				success = false;
				return root;
			}
			auto const & legacy_idd_extensibles_array = legacy_idd_extensibles_iter.value();
			auto const size = legacy_idd_extensibles_array.size();
			std::string const & field_name = legacy_idd_extensibles_array[ extensible_index % size ];
			auto const val = parse_value( idf, index, success, schema_obj_extensions->at( field_name ) );
			extensible[ field_name ] = std::move( val );
			was_value_parsed = true;
			extensible_index++;
			if ( extensible_index && extensible_index % size == 0 ) {
				array_of_extensions.push_back( extensible );
				extensible.clear();
			}
		} else {
			was_value_parsed = true;
			std::string const & field = legacy_idd_fields_array[ legacy_idd_index ];
			auto const & find_field_iter = schema_obj_props.find( field );
			if ( find_field_iter == schema_obj_props.end() ) {
				if ( field == "name" ) {
					root[ field ] = parse_string( idf, index, success );
				} else {
					u64toa( cur_line_num, s );
					EnergyPlus::ShowWarningMessage( "Field " + field + " was not found at line " + s );
				}
			} else {
				auto const val = parse_value( idf, index, success, find_field_iter.value() );
				root[ field ] = std::move( val );
			}
			if ( !success ) return root;
		}
	}
	if ( array_of_extensions.size() ) {
		root[ extension_key ] = std::move( array_of_extensions );
		array_of_extensions = nullptr;
	}
	return root;
}

json IdfParser::parse_number( std::string const & idf, size_t & index, bool & success ) {
	size_t save_i = index;
	eat_whitespace( idf, save_i );
	bool is_double = false, is_sign = false, is_scientific = false;
	std::string num_str, numeric = "-+.eE0123456789";

	while ( numeric.find_first_of( idf[ save_i ] ) != std::string::npos ) {
		num_str += idf[ save_i ];
		if ( idf[ save_i ] == '.' ) {
			if ( is_double ) {
				success = false;
				return nullptr;
			}
			is_double = true;
		} else if ( idf[ save_i ] == '-' || idf[ save_i ] == '+' ) {
			if ( is_sign && !is_scientific ) {
				success = false;
				return nullptr;
			}
			is_sign = true;
		} else if ( idf[ save_i ] == 'e' || idf[ save_i ] == 'E' ) {
			if ( is_scientific ) {
				success = false;
				return nullptr;
			}
			is_scientific = true;
			is_double = true;
		}
		save_i++;
	}


	assert( !num_str.empty() );

	if ( num_str[ num_str.size() - 1 ] == 'e' || num_str[ num_str.size() - 1 ] == 'E' ) {
		success = false;
		return nullptr;
	}

	Token token = look_ahead( idf, save_i );
	if ( token != Token::SEMICOLON && token != Token::COMMA ) {
		success = false;
		return nullptr;
	}
	json val;
	if ( is_double ) {
		try {
			auto const double_val = stod(num_str, nullptr );
			val = double_val;
		} catch ( std::exception e ) {
			auto const double_val = stold( num_str, nullptr );
			val = double_val;
		}
	} else {
		try {
			auto const int_val = stoi( num_str, nullptr );
			val = int_val;
		} catch ( std::exception e ) {
			auto const int_val = stoll( num_str, nullptr );
			val = int_val;
		}
	}
	index = save_i;
	return val;
}

json IdfParser::parse_value( std::string const & idf, size_t & index, bool & success, json const & field_loc ) {
	auto const & field_type = field_loc.find("type");
	if ( field_type != field_loc.end() ) {
		if ( field_type.value() == "number" || field_type.value() == "integer" ) {
			return parse_number( idf, index, success );
		} else {
			auto const parsed_string = parse_string( idf, index, success );
			auto const & enum_it = field_loc.find( "enum" );
			if ( enum_it == field_loc.end() ) return parsed_string;
			for ( auto const & s : enum_it.value() ) {
				auto const & str = s.get < std::string >();
				if ( icompare( str, parsed_string ) ) {
					return str;
				}
			}
			return parsed_string;
		}
	} else {
		switch (look_ahead(idf, index)) {
			case Token::STRING: {
				auto const parsed_string = parse_string(idf, index, success);
				auto const & enum_it = field_loc.find( "enum" );
				if (enum_it != field_loc.end()) {
					for ( auto const & s : enum_it.value() ) {
						auto const & str = s.get < std::string >();
						if ( icompare( str, parsed_string ) ) {
							return str;
						}
					}
				} else if ( icompare( parsed_string, "Autosize" ) || icompare( parsed_string, "Autocalculate" ) ) {
					auto const & default_it = field_loc.find( "default" );
					// The following is hacky because it abuses knowing the consistent generated structure
					// in the future this might not hold true for the array indexes.
					if ( default_it != field_loc.end() ) {
						return field_loc[ "anyOf" ][ 1 ][ "enum" ][ 1 ];
					} else {
						return field_loc[ "anyOf" ][ 1 ][ "enum" ][ 0 ];
					}
				}
				return parsed_string;
			}
			case Token::NUMBER: {
				size_t save_line_index = index_into_cur_line;
				size_t save_line_num = cur_line_num;
				json value = parse_number(idf, index, success);
				if ( !success ) {
					cur_line_num = save_line_num;
					index_into_cur_line = save_line_index;
					success = true;
					return parse_string(idf, index, success);
				}
				return value;
			}
			case Token::NONE:
			case Token::END:
			case Token::EXCLAMATION:
			case Token::COMMA:
			case Token::SEMICOLON:
				break;
		}
		success = false;
		return nullptr;
	}
}

std::string IdfParser::parse_string( std::string const & idf, size_t & index, bool & success ) {
	eat_whitespace( idf, index );

	std::string s;
	char c;

	bool complete = false;
	while ( !complete ) {
		if ( index == idf.size() ) {
			complete = true;
			break;
		}

		c = idf[ index ];
		increment_both_index( index, index_into_cur_line );
		if ( c == ',' ) {
			complete = true;
			decrement_both_index( index, index_into_cur_line );
			break;
		} else if ( c == ';' ) {
			complete = true;
			decrement_both_index( index, index_into_cur_line );
			break;
		} else if ( c == '!' ) {
			complete = true;
			decrement_both_index( index, index_into_cur_line );
			break;
		} else if ( c == '\\' ) {
			if ( index == idf.size() ) break;
			char next_c = idf[ index ];
			increment_both_index( index, index_into_cur_line );
			if ( next_c == '"' ) {
				s += '"';
			} else if ( next_c == '\\' ) {
				s += '\\';
			} else if ( next_c == '/' ) {
				s += '/';
			} else if ( next_c == 'b' ) {
				s += '\b';
			} else if ( next_c == 't' ) {
				s += '\t';
			} else if ( next_c == 'n' ) {
				complete = false;
				break;
			} else if ( next_c == 'r' ) {
				complete = false;
				break;
			} else {
				s += c;
				s += next_c;
			}
		} else {
			s += c;
		}
	}

	if ( !complete ) {
		success = false;
		return std::string();
	}
	return rtrim( s );
}

void IdfParser::increment_both_index( size_t & index, size_t & line_index ) {
	index++;
	line_index++;
}

void IdfParser::decrement_both_index( size_t & index, size_t & line_index ) {
	index--;
	line_index--;
}

void IdfParser::print_out_line_error( std::string const & idf, bool obj_found ) {
	std::string line;
	if ( obj_found ) EnergyPlus::ShowWarningError( "error: \"extra field(s)\" " );
	else EnergyPlus::ShowWarningError( "error: \"obj not found in schema\" " );
	EnergyPlus::ShowWarningError( "at line number " + std::to_string( cur_line_num )
								  + " (index " + std::to_string(index_into_cur_line)  + ")\nLine:\n" );
	while ( idf[ beginning_of_line_index++ ] != '\n' ) line += idf[ beginning_of_line_index ];
	EnergyPlus::ShowWarningError( line );
}

void IdfParser::eat_whitespace( std::string const & idf, size_t & index ) {
	while ( index < idf.size() ) {
		switch ( idf[ index ] ) {
			case ' ':
			case '\r':
			case '\t':
				increment_both_index( index, index_into_cur_line );
				continue;
			case '\n':
				increment_both_index( index, cur_line_num );
				beginning_of_line_index = index;
				index_into_cur_line = 0;
				continue;
			default:
				return;
		}
	}
}

void IdfParser::eat_comment( std::string const & idf, size_t & index ) {
	while ( true ) {
		if ( index == idf.size() ) break;
		if ( idf[ index ] == '\n' ) {
			increment_both_index( index, cur_line_num );
			index_into_cur_line = 0;
			beginning_of_line_index = index;
			break;
		}
		increment_both_index( index, index_into_cur_line );
	}
}

IdfParser::Token IdfParser::look_ahead( std::string const & idf, size_t index ) {
	size_t save_index = index;
	size_t save_line_num = cur_line_num;
	size_t save_line_index = index_into_cur_line;
	Token token = next_token( idf, save_index );
	cur_line_num = save_line_num;
	index_into_cur_line = save_line_index;
	return token;
}

IdfParser::Token IdfParser::next_token( std::string const & idf, size_t & index ) {
	eat_whitespace( idf, index );

	if ( index == idf.size() ) {
		return Token::END;
	}

	char const c = idf[ index ];
	increment_both_index( index, index_into_cur_line );
	switch ( c ) {
		case '!':
			return Token::EXCLAMATION;
		case ',':
			return Token::COMMA;
		case ';':
			return Token::SEMICOLON;
		default:
			static std::string const search_chars( "-:.#/\\[]{}_@$%^&*()|+=<>?'\"~" );
			static std::string const numeric( ".-+0123456789" );
			if ( numeric.find_first_of( c ) != std::string::npos ) {
				return Token::NUMBER;
			} else if ( isalnum( c ) || ( std::string::npos != search_chars.find_first_of( c ) ) ) {
				return Token::STRING;
			}
			break;
	}
	decrement_both_index( index, index_into_cur_line );
	return Token::NONE;
}

void State::initialize( json const * parsed_schema ) {
	stack.clear();
	schema = parsed_schema;
	stack.push_back( schema );
	json const & loc = stack.back()->at( "required" );
	for ( auto & s : loc ) root_required.emplace( s.get < std::string >(), false );
}

void State::add_error( ErrorType err, double val, unsigned line_num, unsigned line_index ) {
	std::string str = "Out of Range: Value \"" + std::to_string( val ) + "\" parsed at line " +
					  std::to_string( line_num ) + " (index " + std::to_string( line_index ) + ")";
	if ( err == ErrorType::Maximum ) {
		errors.push_back( str + " exceeds maximum" );
	} else if ( err == ErrorType::ExclusiveMaximum ) {
		errors.push_back( str + " exceeds or equals exclusive maximum" );
	} else if ( err == ErrorType::Minimum ) {
		errors.push_back( str + " is less than the minimum" );
	} else if ( err == ErrorType::ExclusiveMinimum ) {
		errors.push_back( str + " is less than or equal to the exclusive minimum" );
	}
}

int State::print_errors() {
	if ( warnings.size() ) EnergyPlus::ShowWarningError("Number of validation warnings: " + std::to_string(warnings.size()));
	for ( auto const & s : warnings ) EnergyPlus::ShowContinueError( s );
	if ( errors.size() ) EnergyPlus::ShowSevereError("Number of validation errors: " + std::to_string(errors.size()));
	for ( auto const & s : errors ) EnergyPlus::ShowContinueError( s );
	return static_cast<int> ( errors.size() );
}

std::vector < std::string > const & State::validation_errors() {
	return errors;
}

std::vector < std::string > const & State::validation_warnings() {
	return warnings;
}

void State::traverse( json::parse_event_t & event, json & parsed, unsigned line_num, unsigned line_index ) {
	switch ( event ) {
		case json::parse_event_t::object_start: {
			if ( is_in_extensibles || stack.back()->find( "patternProperties" ) == stack.back()->end() ) {
				if ( stack.back()->find( "properties" ) != stack.back()->end() )
					stack.push_back( & stack.back()->at( "properties" ) );
			} else {
				stack.push_back( & stack.back()->at( "patternProperties" )[ ".*" ] );
				if ( stack.back()->find( "required" ) != stack.back()->end() ) {
					auto & loc = stack.back()->at( "required" );
					obj_required.clear();
					for ( auto & s : loc ) obj_required.emplace( s.get < std::string >(), false );
				}
			}
			last_seen_event = event;
			break;
		}

		case json::parse_event_t::value: {
			validate( parsed, line_num, line_index );
			if ( does_key_exist ) stack.pop_back();
			does_key_exist = true;
			last_seen_event = event;
			break;
		}

		case json::parse_event_t::key: {
			std::string const & key = parsed;
			prev_line_index = line_index;
			prev_key_len = ( unsigned ) key.size() + 3;
			if ( need_new_object_name ) {
				cur_obj_name = key;
				cur_obj_count = 0;
				need_new_object_name = false;
				if ( cur_obj_name.find( "Parametric:" ) != std::string::npos ) {
					u64toa( line_num + 1, s );
					errors.push_back( "You must run Parametric Preprocessor for \"" + cur_obj_name + "\" at line " + s );
				} else if ( cur_obj_name.find( "Template" ) != std::string::npos ) {
					u64toa( line_num + 1, s );
					errors.push_back( "You must run the ExpandObjects program for \"" + cur_obj_name + "\" at line " + s );
				}
			}

			if ( stack.back()->find( "properties" ) == stack.back()->end() ) {
				if ( stack.back()->find( key ) != stack.back()->end() ) {
					stack.push_back( & stack.back()->at( key ) );
				} else {
					u64toa( line_num, s );
					u64toa( line_index, s2 );
					errors.push_back( "Key \"" + key + "\" in object \"" + cur_obj_name + "\" at line "
									  + s2 + " (index " + s + ") not found in schema" );
					does_key_exist = false;
				}
			}

			if ( !is_in_extensibles ) {
				auto req = obj_required.find( key );
				if ( req != obj_required.end() ) {
					req->second = true; // required field is now accounted for, for this specific object
				}
				req = root_required.find( key );
				if ( req != root_required.end() ) {
					req->second = true; // root_required field is now accounted for
				}
			} else {
				auto req = extensible_required.find( key );
				if ( req != extensible_required.end() ) req->second = true;
			}

			last_seen_event = event;
			break;
		}

		case json::parse_event_t::array_start: {
			stack.push_back( & stack.back()->at( "items" ) );
			if ( stack.back()->find( "required" ) != stack.back()->end() ) {
				auto & loc = stack.back()->at( "required" );
				extensible_required.clear();
				for ( auto & s : loc ) extensible_required.emplace( s.get < std::string >(), false );
			}
			is_in_extensibles = true;
			last_seen_event = event;
			break;
		}

		case json::parse_event_t::array_end: {
			stack.pop_back();
			stack.pop_back();
			is_in_extensibles = false;
			last_seen_event = event;
			break;
		}

		case json::parse_event_t::object_end: {
			if ( is_in_extensibles ) {
				for ( auto & it : extensible_required ) {
					if ( !it.second ) {
						u64toa( line_num, s );
						u64toa( line_index, s2 );
						errors.push_back(
						"Required extensible field \"" + it.first + "\" in object \"" + cur_obj_name
						+ "\" ending at line " + s2 + " (index " + s + ") was not provided" );
					}
					it.second = false;
				}
			} else if ( last_seen_event != json::parse_event_t::object_end ) {
				cur_obj_count++;
				for ( auto & it : obj_required ) {
					if ( !it.second ) {
						u64toa( line_num, s );
						u64toa( line_index, s2 );
						errors.push_back(
						"Required field \"" + it.first + "\" in object \"" + cur_obj_name
						+ "\" ending at line " + s2 + " (index " + s + ") was not provided" );
					}
					it.second = false;
				}
			} else { // must be at the very end of an object now
				const auto * loc = stack.back();
				if ( loc->find( "minProperties" ) != loc->end() &&
					 cur_obj_count < loc->at( "minProperties" ).get < unsigned >() ) {
					u64toa( line_num, s );
					errors.push_back(
					"minProperties for object \"" + cur_obj_name + "\" at line " + s + " was not met" );
				}
				if ( loc->find( "maxProperties" ) != loc->end() &&
					 cur_obj_count > loc->at( "maxProperties" ).get < unsigned >() ) {
					u64toa( line_num, s );
					errors.push_back(
					"maxProperties for object \"" + cur_obj_name + "\" at line " + s + " was exceeded" );
				}
				obj_required.clear();
				extensible_required.clear();
				need_new_object_name = true;
				stack.pop_back();
			}
			stack.pop_back();
			last_seen_event = event;
			break;
		}
	}
	if ( !stack.size() ) {
		for ( auto & it: root_required ) {
			if ( !it.second ) {
				errors.push_back( "Required object \"" + it.first + "\" was not provided in input file" );
			}
		}
	}
}

void State::validate( json & parsed, unsigned line_num, unsigned EP_UNUSED( line_index ) ) {
	auto const * loc = stack.back();

	if ( loc->find( "enum" ) != loc->end() ) {
		size_t i;
		auto const & enum_array = loc->at( "enum" );
		auto const enum_array_size = enum_array.size();
		if ( parsed.is_string() ) {
			auto const & parsed_string = parsed.get < std::string >();
			for ( i = 0; i < enum_array_size; i++ ) {
				auto const & enum_string = enum_array[ i ].get< std::string >();
				if ( icompare( enum_string, parsed_string ) ) break;
			}
			if ( i == enum_array_size ) {
				u64toa( line_num, s );
				errors.push_back( "In object \"" + cur_obj_name + "\" at line " + s
								  + ": \"" + parsed_string + "\" was not found in the enum" );
			}
		} else {
			int const parsed_int = parsed.get < int >();
			for ( i = 0; i < enum_array_size; i++ ) {
				auto const & enum_int = enum_array[ i ].get< int >();
				if ( enum_int == parsed_int ) break;
			}
			if ( i == enum_array_size ) {
				i64toa( parsed_int, s );
				u64toa( line_num, s2 );
				errors.push_back( "In object \"" + cur_obj_name + "\" at line " + s
								  + ": \"" + s2 + "\" was not found in the enum" );
			}
		}
	} else if ( parsed.is_number() ) {
		double const val = parsed.get < double >();
		auto const found_anyOf = loc->find( "anyOf" );
		if ( found_anyOf != loc->end() ) {
			loc = & found_anyOf->at( 0 );
		}
		auto const found_min = loc->find( "minimum" );
		if ( found_min != loc->end() ) {
			double const min_val = found_min->get < double >();
			if ( loc->find( "exclusiveMinimum" ) != loc->end() && val <= min_val ) {
				add_error( State::ErrorType::ExclusiveMinimum, val, line_num, prev_line_index + prev_key_len );
			} else if ( val < min_val ) {
				add_error( State::ErrorType::Minimum, val, line_num, prev_line_index + prev_key_len );
			}
		}
		auto const found_max = loc->find( "maximum" );
		if ( found_max != loc->end() ) {
			double const max_val = found_max->get < double >();
			if ( loc->find( "exclusiveMaximum" ) != loc->end() && val >= max_val ) {
				add_error( State::ErrorType::ExclusiveMaximum, val, line_num, prev_line_index + prev_key_len );
			} else if ( val > max_val ) {
				add_error( State::ErrorType::Maximum, val, line_num, prev_line_index + prev_key_len );
			}
		}
		auto const found_type = loc->find( "type" );
		if ( found_type != loc->end() && found_type.value() != "number" ) {
			dtoa( val, s );
			u64toa( line_num, s2 );
			warnings.push_back( "In object \"" + cur_obj_name + "\" at line " + s
								+ ", type == " + loc->at( "type" ).get < std::string >()
								+ " but parsed value = " + s2 );
		}
	} else if ( parsed.is_string() ) {
		auto const found_anyOf = loc->find( "anyOf" );
		if ( found_anyOf != loc->end() ) {
			size_t i;
			for ( i = 0; i < found_anyOf->size(); i++ ) {
				auto const & any_of_check = found_anyOf->at( i );
				auto const found_type = any_of_check.find( "type" );
				if ( found_type != any_of_check.end() && *found_type == "string" ) break;
			}
			if ( i == found_anyOf->size() ) {
				u64toa( line_num, s );
				warnings.push_back( "type == string was not found in anyOf in object \"" + cur_obj_name
									+ "\" at line " + s );
			}
			return;
		}
		auto const found_type = loc->find( "type" );
		auto const & parsed_string = parsed.get< std::string >();
		if ( found_type != loc->end() && *found_type != "string" && ! parsed_string.empty() ) {
			u64toa( line_num, s );
			errors.push_back( "In object \"" + cur_obj_name + "\", at line " + s + ": type needs to be string" );
		}
	}
}


namespace EnergyPlus {
// Module containing the input processor routines

// MODULE INFORMATION:
//       AUTHOR         Linda K. Lawrie
//       DATE WRITTEN   August 1997
//       MODIFIED       na
//       RE-ENGINEERED  na

// PURPOSE OF THIS MODULE:
// To provide the capabilities of reading the input data dictionary
// and input file and supplying the simulation routines with the data
// contained therein.

// METHODOLOGY EMPLOYED:

// REFERENCES:
// The input syntax is designed to allow for future flexibility without
// necessitating massive (or any) changes to this code.  Two files are
// used as key elements: (1) the input data dictionary will specify the
// sections and objects that will be allowed in the actual simulation
// input file and (2) the simulation input data file will be processed
// with the data therein being supplied to the actual simulation routines.

// OTHER NOTES:

// USE STATEMENTS:
// Use statements for data only modules
// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataStringGlobals;
	using DataGlobals::MaxNameLength;
	using DataGlobals::AutoCalculate;
	using DataGlobals::rTinyValue;
	using DataGlobals::DisplayAllWarnings;
	using DataGlobals::DisplayUnusedObjects;
	using DataGlobals::CacheIPErrorFile;
	using DataGlobals::DoingInputProcessing;
	using DataSizing::AutoSize;
	using namespace DataIPShortCuts;
	using DataSystemVariables::SortedIDD;
	using DataSystemVariables::iASCII_CR;
	using DataSystemVariables::iUnicode_end;
	using DataGlobals::DisplayInputInAudit;

	static std::string const BlankString;
	static gio::Fmt fmtLD( "*" );
	static gio::Fmt fmtA( "(A)" );

	int EchoInputFile( 0 ); // Unit number of the file echoing the IDD and input records (eplusout.audit)

// Functions

// Clears the global data in InputProcessor.
// Needed for unit tests, should not be normally called.
	void
	InputProcessor::clear_state() {
		state = State();
		idf_parser = IdfParser();
		jdf.clear();
		jdd_jdf_cache_map.clear();
		EchoInputFile = 0;
		echo_stream = nullptr;
	}

	std::vector < std::string > const & InputProcessor::validation_errors() {
		return state.validation_errors();
	}

	std::vector < std::string > const & InputProcessor::validation_warnings() {
		return state.validation_warnings();
	}

	std::pair< bool, std::string >
	InputProcessor::ConvertInsensitiveObjectType( std::string const & objectType ) {
		auto tmp_umit = EnergyPlus::InputProcessor::case_insensitive_object_map.find( MakeUPPERCase( objectType ) );
		if ( tmp_umit != EnergyPlus::InputProcessor::case_insensitive_object_map.end() ) {
			return std::make_pair( true, tmp_umit->second );
		}
		return std::make_pair( false, "" );
	}

	void
	InputProcessor::InitFiles() {
		int write_stat;
//		int read_stat;
//		bool FileExists;

		EchoInputFile = GetNewUnitNumber();
		{
			IOFlags flags;
			flags.ACTION( "write" );
			gio::open( EchoInputFile, outputAuditFileName, flags );
			write_stat = flags.ios();
		}
		if ( write_stat != 0 ) {
			DisplayString( "Could not open (write) " + outputAuditFileName + " ." );
			ShowFatalError( "ProcessInput: Could not open file " + outputAuditFileName + " for output (write)." );
		}
		echo_stream = gio::out_stream( EchoInputFile );

//		{
//			IOFlags flags;
//			gio::inquire( outputIperrFileName, flags );
//			FileExists = flags.exists();
//		}
//		if ( FileExists ) {
//			CacheIPErrorFile = GetNewUnitNumber();
//			{
//				IOFlags flags;
//				flags.ACTION( "read" );
//				gio::open( CacheIPErrorFile, outputIperrFileName, flags );
//				read_stat = flags.ios();
//			}
//			if ( read_stat != 0 ) {
//				ShowFatalError( "EnergyPlus: Could not open file " + outputIperrFileName + " for input (read)." );
//			}
//			{
//				IOFlags flags;
//				flags.DISPOSE( "delete" );
//				gio::close( CacheIPErrorFile, flags );
//			}
//		}
//		CacheIPErrorFile = GetNewUnitNumber();
//		{
//			IOFlags flags;
//			flags.ACTION( "write" );
//			gio::open( CacheIPErrorFile, outputIperrFileName, flags );
//			write_stat = flags.ios();
//		}
//		if ( write_stat != 0 ) {
//			DisplayString( "Could not open (write) " + outputIperrFileName );
//			ShowFatalError( "ProcessInput: Could not open file " + outputIperrFileName + " for output (write)." );
//		}
	}

	void
	InputProcessor::InitializeMaps() {
		unused_inputs.clear();
		jdd_jdf_cache_map.clear();
		jdd_jdf_cache_map.reserve( jdf.size() );
		auto const & schema_properties = schema.at( "properties" );

		for ( auto jdf_iter = jdf.begin(); jdf_iter != jdf.end(); ++jdf_iter ) {
			auto const & objects = jdf_iter.value();
			std::vector < json::const_iterator > jdf_obj_iterators_vec;
			jdf_obj_iterators_vec.reserve( objects.size() );
			for ( auto jdf_obj_iter = objects.begin(); jdf_obj_iter != objects.end(); ++jdf_obj_iter ) {
				jdf_obj_iterators_vec.emplace_back( jdf_obj_iter );
				auto const * const obj_ptr = jdf_obj_iter.value().get_ptr< const json::object_t * const >();
				unused_inputs.emplace( obj_ptr, std::make_pair( jdf_iter.key(), jdf_obj_iter.key() ) );
			}
			auto const & schema_iter = schema_properties.find( jdf_iter.key() );
			jdd_jdf_cache_map.emplace( schema_iter.key(), std::make_pair( schema_iter, std::move( jdf_obj_iterators_vec ) ) );
		}
	}

	void
	InputProcessor::ProcessInput() {
		std::ifstream jdd_stream( inputJddFileName , std::ifstream::in);
		if ( !jdd_stream.is_open() ) {
			ShowFatalError( "JDD file path " + inputJddFileName + " not found" );
			return;
		}
		InputProcessor::schema = json::parse(jdd_stream);

		const json & loc = InputProcessor::schema[ "properties" ];
		case_insensitive_object_map.reserve( loc.size() );
		for ( auto it = loc.begin(); it != loc.end(); ++it ) {
			std::string key = it.key();
			for ( char & c : key ) c = toupper( c );
			case_insensitive_object_map.emplace( std::move( key ), it.key() );
		}

		std::ifstream input_stream( inputFileName , std::ifstream::in | std::ios::ate);
		if ( !input_stream.is_open() ) {
			ShowFatalError( "Input file path " + inputFileName + " not found" );
			return;
		}

		// TODO: Check which file read approach works properly on windows
		// std::string lines;
		// std::string line;
		// while (std::getline(infile, line))
		// {
		// 	lines.append(line + NL);
		// }
		std::ifstream::pos_type size = input_stream.tellg();
		char *memblock = new char[(size_t) size + 1];
		input_stream.seekg(0, std::ios::beg);
		input_stream.read(memblock, size);
		memblock[size] = '\0';
		input_stream.close();
		std::string input_file = memblock;
		delete[] memblock;

		if ( ! DataGlobals::isJDF ) {
			json const input_file_json = InputProcessor::idf_parser.decode( input_file, InputProcessor::schema );
			if ( DataGlobals::outputJDFConversion ) {
				input_file = input_file_json.dump( 4 );
				std::string convertedIDF( outputDirPathName + inputFileNameOnly + ".jdf" );
				FileSystem::makeNativePath( convertedIDF );
				std::ofstream convertedFS( convertedIDF, std::ofstream::out );
				convertedFS << input_file << std::endl;
			} else {
				input_file = input_file_json.dump( 4 );
			}
		}

		InputProcessor::state.initialize( & InputProcessor::schema );

		json::parser_callback_t cb = []( int EP_UNUSED( depth ), json::parse_event_t event, json &parsed, unsigned line_num, unsigned line_index ) -> bool {
			InputProcessor::state.traverse( event, parsed, line_num, line_index );
			return true;
		};

		InputProcessor::jdf = json::parse( input_file, cb );

		if ( DataGlobals::isJDF && DataGlobals::outputJDFConversion ) {
			std::string const encoded = InputProcessor::idf_parser.encode( InputProcessor::jdf, InputProcessor::schema );
			std::string convertedJDF( outputDirPathName + inputFileNameOnly + ".idf" );
			FileSystem::makeNativePath( convertedJDF );
			std::ofstream convertedFS( convertedJDF, std::ofstream::out );
			convertedFS << encoded << std::endl;
		}

		auto const num_errors = InputProcessor::state.print_errors();
		if ( num_errors ) {
			ShowFatalError( "Errors occurred on processing input file. Preceding condition(s) cause termination." );
		}

		InitializeMaps();

		int MaxArgs = 0;
		int MaxAlpha = 0;
		int MaxNumeric = 0;
		GetMaxSchemaArgs( MaxArgs, MaxAlpha, MaxNumeric );

		DataIPShortCuts::cAlphaFieldNames.allocate( MaxAlpha );
		DataIPShortCuts::cAlphaArgs.allocate( MaxAlpha );
		DataIPShortCuts::lAlphaFieldBlanks.dimension( MaxAlpha, false );
		DataIPShortCuts::cNumericFieldNames.allocate( MaxNumeric );
		DataIPShortCuts::rNumericArgs.dimension( MaxNumeric, 0.0 );
		DataIPShortCuts::lNumericFieldBlanks.dimension( MaxNumeric, false );
	}

	int
	InputProcessor::GetNumSectionsFound( std::string const & SectionWord ) {
		// PURPOSE OF THIS SUBROUTINE:
		// This function returns the number of a particular section (in input data file)
		// found in the current run.  If it can't find the section in list
		// of sections, a -1 will be returned.

		// METHODOLOGY EMPLOYED:
		// Look up section in list of sections.  If there, return the
		// number of sections of that kind found in the current input.  If not, return -1.
		auto const & SectionWord_iter = jdf.find( SectionWord );
		if ( SectionWord_iter == jdf.end() ) return -1;
		return static_cast <int> ( SectionWord_iter.value().size() );
	}


	int
	InputProcessor::GetNumObjectsFound( std::string const & ObjectWord ) {

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   September 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This function returns the number of objects (in input data file)
		// found in the current run.  If it can't find the object in list
		// of objects, a 0 will be returned.

		// METHODOLOGY EMPLOYED:
		// Look up object in list of objects.  If there, return the
		// number of objects found in the current input.  If not, return 0.

		auto const & find_obj = jdf.find( ObjectWord );

		if ( find_obj == jdf.end() ) {
			auto tmp_umit = case_insensitive_object_map.find( MakeUPPERCase( ObjectWord ) );
			if ( tmp_umit == case_insensitive_object_map.end()
				 || jdf.find( tmp_umit->second ) == jdf.end() ) {
				return 0;
			}
			return static_cast< int >( jdf[ tmp_umit->second ].size() );
		} else {
			return static_cast< int >( find_obj.value().size() );
		}

		if ( schema[ "properties" ].find( ObjectWord ) == schema[ "properties" ].end() ) {
			auto tmp_umit = case_insensitive_object_map.find( MakeUPPERCase( ObjectWord ) );
			if ( tmp_umit == case_insensitive_object_map.end() ) {
				ShowWarningError( "Requested Object not found in Definitions: " + ObjectWord );
			}
		}
		return 0;
	}

	void
	InputProcessor::GetObjectItem(
		std::string const & Object,
		int const Number,
		Array1S_string Alphas,
		int & NumAlphas,
		Array1S < Real64 > Numbers,
		int & NumNumbers,
		int & Status,
		Optional < Array1D_bool > NumBlank,
		Optional < Array1D_bool > AlphaBlank,
		Optional < Array1D_string > AlphaFieldNames,
		Optional < Array1D_string > NumericFieldNames
	) {
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   September 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine gets the 'number' 'object' from the IDFRecord data structure.

		auto find_iterators = jdd_jdf_cache_map.find( Object );
		if ( find_iterators == jdd_jdf_cache_map.end() ) {
			auto const tmp_umit = case_insensitive_object_map.find( MakeUPPERCase( Object ) );
			if ( tmp_umit == case_insensitive_object_map.end()
				 || jdf.find( tmp_umit->second ) == jdf.end() ) {
				return;
			}
			find_iterators = jdd_jdf_cache_map.find( tmp_umit->second );
		}

		NumAlphas = 0;
		NumNumbers = 0;
		Status = -1;
		auto const & is_AlphaBlank = present(AlphaBlank);
		auto const & is_AlphaFieldNames = present(AlphaFieldNames);
		auto const & is_NumBlank = present(NumBlank);
		auto const & is_NumericFieldNames = present(NumericFieldNames);

		auto const & jdf_it = find_iterators->second.second.at( Number - 1 );
		auto const & jdd_it = find_iterators->second.first;
		auto const & jdd_it_val = jdd_it.value();

		auto const * const obj_ptr = jdf_it.value().get_ptr< const json::object_t * const >();
		auto const find_unused = unused_inputs.find( obj_ptr );
		if ( find_unused != unused_inputs.end() ) {
			unused_inputs.erase( find_unused );
		}

		// Locations in JSON schema relating to normal fields
		auto const & schema_obj_props = jdd_it_val[ "patternProperties" ][ ".*" ][ "properties" ];

		// Locations in JSON schema storing the positional aspects from the IDD format, legacy prefixed
		auto const & legacy_idd = jdd_it_val[ "legacy_idd" ];
		auto const & legacy_idd_alphas = legacy_idd[ "alphas" ];
		auto const & legacy_idd_numerics = legacy_idd[ "numerics" ];
		auto const & schema_name_field = jdd_it_val.find( "name" );

		auto key = legacy_idd.find("extension");
		std::string extension_key;
		if ( key != legacy_idd.end() ) {
			extension_key = key.value();
		}

		Alphas = "";
		Numbers = 0;

		auto const & obj = jdf_it;
		auto const & obj_val = obj.value();
		auto const & legacy_idd_alphas_fields = legacy_idd_alphas[ "fields" ];
		for ( size_t i = 0; i < legacy_idd_alphas_fields.size(); ++i ) {
			std::string const & field = legacy_idd_alphas_fields[ i ];
			if ( field == "name" && schema_name_field != jdd_it_val.end() ) {
				auto const & name_iter = schema_name_field.value();
				if ( name_iter.find( "retaincase" ) != name_iter.end() ) {
					Alphas( i + 1 ) = obj.key();
				} else {
					Alphas( i + 1 ) = MakeUPPERCase( obj.key() );
				}
				if ( is_AlphaBlank ) AlphaBlank()( i + 1 ) = obj.key().empty();
				if ( is_AlphaFieldNames ) AlphaFieldNames()( i + 1 ) = field;
				NumAlphas++;
				continue;
			}
<<<<<<< HEAD
			auto it = obj_val.find( field );
			if ( it != obj_val.end() ) {
				if ( it.value().is_string() ) {
					std::string val;
					auto const & schema_field_obj = schema_obj_props[ field ];
					auto const & find_default = schema_field_obj.find( "default" );
					if ( it.value().get < std::string >().empty() &&
						 find_default != schema_field_obj.end() ) {
						auto const & default_val = find_default.value();
						if ( default_val.is_string() ) {
							val = default_val.get < std::string >();
						} else {
							dtoa( default_val.get < double >(), s );
							val = s;
						}
						if ( is_AlphaBlank ) AlphaBlank()( i + 1 ) = true;
					} else {
						val = it.value().get < std::string >();
						if ( is_AlphaBlank ) AlphaBlank()( i + 1 ) = val.empty();
					}
					if ( schema_field_obj.find("retaincase") != schema_field_obj.end() ) {
						Alphas( i + 1 ) = val;
					} else {
						Alphas( i + 1 ) = MakeUPPERCase( val );
=======

			if ( Pos == std::string::npos ) {
				// must be time to read another line
				ReadInputLine( idd_stream, CurPos, BlankLine, EndofFile );
				if ( BlankLine || EndofFile ) continue;
			} else {
				if ( InputLine[ CurPos + Pos ] == ';' ) {
					EndofObjectDef = true;
				}
				CurPos += Pos;
			}

		}

		// Reached end of object def but there may still be more \ lines to parse....
		// Goes until next object is encountered ("not blankline") or end of idd file
		// If last object is not numeric, then exit immediately....
		BlankLine = true;
		while ( BlankLine && ! EndofFile ) {
			// It's a numeric object as last one...
			ReadInputLine( idd_stream, CurPos, BlankLine, EndofFile, MinMax, WhichMinMax, MinMaxString, Value, Default, AlphDefaultString, AutoSize, AutoCalculate, RetainCaseFlag, ErrorsFoundFlag );
			if ( MinMax ) {
				NumRangeChecks( ObjectDef( NumObjectDefs ).NumNumeric ).MinMaxChk = true;
				NumRangeChecks( ObjectDef( NumObjectDefs ).NumNumeric ).FieldNumber = Count;
				if ( WhichMinMax <= 2 ) { //=0 (none/invalid), =1 \min, =2 \min>, =3 \max, =4 \max<
					NumRangeChecks( ObjectDef( NumObjectDefs ).NumNumeric ).WhichMinMax( 1 ) = WhichMinMax;
					NumRangeChecks( ObjectDef( NumObjectDefs ).NumNumeric ).MinMaxString( 1 ) = MinMaxString;
					NumRangeChecks( ObjectDef( NumObjectDefs ).NumNumeric ).MinMaxValue( 1 ) = Value;
				} else {
					NumRangeChecks( ObjectDef( NumObjectDefs ).NumNumeric ).WhichMinMax( 2 ) = WhichMinMax;
					NumRangeChecks( ObjectDef( NumObjectDefs ).NumNumeric ).MinMaxString( 2 ) = MinMaxString;
					NumRangeChecks( ObjectDef( NumObjectDefs ).NumNumeric ).MinMaxValue( 2 ) = Value;
				}
			}
			if ( Default && ! AlphaOrNumeric( Count ) ) {
				NumRangeChecks( ObjectDef( NumObjectDefs ).NumNumeric ).DefaultChk = true;
				NumRangeChecks( ObjectDef( NumObjectDefs ).NumNumeric ).Default = Value;
				if ( AlphDefaultString == "AUTOSIZE" ) NumRangeChecks( ObjectDef( NumObjectDefs ).NumNumeric ).DefAutoSize = true;
				if ( AlphDefaultString == "AUTOCALCULATE" ) NumRangeChecks( ObjectDef( NumObjectDefs ).NumNumeric ).DefAutoCalculate = true;
			} else if ( Default && AlphaOrNumeric( Count ) ) {
				AlphFieldDefaults( ObjectDef( NumObjectDefs ).NumAlpha ) = AlphDefaultString;
			}
			if ( AutoSize ) {
				NumRangeChecks( ObjectDef( NumObjectDefs ).NumNumeric ).AutoSizable = true;
				NumRangeChecks( ObjectDef( NumObjectDefs ).NumNumeric ).AutoSizeValue = Value;
			}
			if ( AutoCalculate ) {
				NumRangeChecks( ObjectDef( NumObjectDefs ).NumNumeric ).AutoCalculatable = true;
				NumRangeChecks( ObjectDef( NumObjectDefs ).NumNumeric ).AutoCalculateValue = Value;
			}
			if ( ErrorsFoundFlag ) {
				errFlag = true;
				ErrorsFoundFlag = false;
			}
		}

		if ( ! BlankLine ) {
			Backspace( idd_stream );
			EchoInputLine = false;
		}

		if ( RequiredField ) {
			RequiredFields( Count ) = true;
			MinimumNumberOfFields = max( Count, MinimumNumberOfFields );
		}
		if ( RetainCaseFlag ) {
			AlphRetainCase( Count ) = true;
		}

		ObjectDef( NumObjectDefs ).NumParams = Count; // Also the total of ObjectDef(..)%NumAlpha+ObjectDef(..)%NumNumeric
		ObjectDef( NumObjectDefs ).MinNumFields = MinimumNumberOfFields;
		if ( ObsoleteObject ) {
			ObsoleteObjectsRepNames.redimension( ++NumObsoleteObjects );
			ObsoleteObjectsRepNames( NumObsoleteObjects ) = ReplacementName;
			ObjectDef( NumObjectDefs ).ObsPtr = NumObsoleteObjects;
		}
		if ( RequiredObject ) {
			ObjectDef( NumObjectDefs ).RequiredObject = true;
		}
		if ( UniqueObject ) {
			ObjectDef( NumObjectDefs ).UniqueObject = true;
		}
		if ( ExtensibleObject ) {
			ObjectDef( NumObjectDefs ).ExtensibleObject = true;
			ObjectDef( NumObjectDefs ).ExtensibleNum = ExtensibleNumFields;
		}

		NumAlphaArgsFound += ObjectDef( NumObjectDefs ).NumAlpha;
		MaxAlphaArgsFound = max( MaxAlphaArgsFound, ObjectDef( NumObjectDefs ).NumAlpha );
		NumNumericArgsFound += ObjectDef( NumObjectDefs ).NumNumeric;
		MaxNumericArgsFound = max( MaxNumericArgsFound, ObjectDef( NumObjectDefs ).NumNumeric );
		ObjectDef( NumObjectDefs ).AlphaOrNumeric.allocate( Count );
		ObjectDef( NumObjectDefs ).AlphaOrNumeric = AlphaOrNumeric( {1,Count} );
		ObjectDef( NumObjectDefs ).AlphRetainCase.allocate( Count );
		ObjectDef( NumObjectDefs ).AlphRetainCase = AlphRetainCase( {1,Count} );
		PrevCount = Count;
		ObjectDef( NumObjectDefs ).NumRangeChks.allocate( ObjectDef( NumObjectDefs ).NumNumeric );
		if ( ObjectDef( NumObjectDefs ).NumNumeric > 0 ) {
			ObjectDef( NumObjectDefs ).NumRangeChks = NumRangeChecks( {1,ObjectDef( NumObjectDefs ).NumNumeric} );
		}
		PrevSizeNumNumeric = ObjectDef( NumObjectDefs ).NumNumeric; //used to clear only portion of NumRangeChecks array
		ObjectDef( NumObjectDefs ).AlphFieldChks.allocate( ObjectDef( NumObjectDefs ).NumAlpha );
		if ( ObjectDef( NumObjectDefs ).NumAlpha > 0 ) {
			ObjectDef( NumObjectDefs ).AlphFieldChks = AlphFieldChecks( {1,ObjectDef( NumObjectDefs ).NumAlpha} );
		}
		ObjectDef( NumObjectDefs ).AlphFieldDefs.allocate( ObjectDef( NumObjectDefs ).NumAlpha );
		if ( ObjectDef( NumObjectDefs ).NumAlpha > 0 ) {
			ObjectDef( NumObjectDefs ).AlphFieldDefs = AlphFieldDefaults( {1,ObjectDef( NumObjectDefs ).NumAlpha} );
		}
		PrevSizeNumAlpha = ObjectDef( NumObjectDefs ).NumAlpha;
		ObjectDef( NumObjectDefs ).ReqField.allocate( Count );
		ObjectDef( NumObjectDefs ).ReqField = RequiredFields( {1,Count} );
		for ( Count = 1; Count <= ObjectDef( NumObjectDefs ).NumNumeric; ++Count ) {
			if ( ObjectDef( NumObjectDefs ).NumRangeChks( Count ).MinMaxChk ) {
				// Checking MinMax Range (min vs. max and vice versa)
				MinMaxError = false;
				// check min against max
				if ( ObjectDef( NumObjectDefs ).NumRangeChks( Count ).WhichMinMax( 1 ) == 1 ) {
					// min
					Value = ObjectDef( NumObjectDefs ).NumRangeChks( Count ).MinMaxValue( 1 );
					if ( ObjectDef( NumObjectDefs ).NumRangeChks( Count ).WhichMinMax( 2 ) == 3 ) {
						if ( Value > ObjectDef( NumObjectDefs ).NumRangeChks( Count ).MinMaxValue( 2 ) ) MinMaxError = true;
					} else if ( ObjectDef( NumObjectDefs ).NumRangeChks( Count ).WhichMinMax( 2 ) == 4 ) {
						if ( Value == ObjectDef( NumObjectDefs ).NumRangeChks( Count ).MinMaxValue( 2 ) ) MinMaxError = true;
					}
				} else if ( ObjectDef( NumObjectDefs ).NumRangeChks( Count ).WhichMinMax( 1 ) == 2 ) {
					// min>
					Value = ObjectDef( NumObjectDefs ).NumRangeChks( Count ).MinMaxValue( 1 ) + rTinyValue; // infintesimally bigger than min
					if ( ObjectDef( NumObjectDefs ).NumRangeChks( Count ).WhichMinMax( 2 ) == 3 ) {
						if ( Value > ObjectDef( NumObjectDefs ).NumRangeChks( Count ).MinMaxValue( 2 ) ) MinMaxError = true;
					} else if ( ObjectDef( NumObjectDefs ).NumRangeChks( Count ).WhichMinMax( 2 ) == 4 ) {
						if ( Value == ObjectDef( NumObjectDefs ).NumRangeChks( Count ).MinMaxValue( 2 ) ) MinMaxError = true;
					}
				}
				// check max against min
				if ( ObjectDef( NumObjectDefs ).NumRangeChks( Count ).WhichMinMax( 2 ) == 3 ) {
					// max
					Value = ObjectDef( NumObjectDefs ).NumRangeChks( Count ).MinMaxValue( 2 );
					// Check max value against min
					if ( ObjectDef( NumObjectDefs ).NumRangeChks( Count ).WhichMinMax( 1 ) == 1 ) {
						if ( Value < ObjectDef( NumObjectDefs ).NumRangeChks( Count ).MinMaxValue( 1 ) ) MinMaxError = true;
					} else if ( ObjectDef( NumObjectDefs ).NumRangeChks( Count ).WhichMinMax( 1 ) == 2 ) {
						if ( Value == ObjectDef( NumObjectDefs ).NumRangeChks( Count ).MinMaxValue( 1 ) ) MinMaxError = true;
					}
				} else if ( ObjectDef( NumObjectDefs ).NumRangeChks( Count ).WhichMinMax( 2 ) == 4 ) {
					// max<
					Value = ObjectDef( NumObjectDefs ).NumRangeChks( Count ).MinMaxValue( 2 ) - rTinyValue; // infintesimally bigger than min
					if ( ObjectDef( NumObjectDefs ).NumRangeChks( Count ).WhichMinMax( 1 ) == 1 ) {
						if ( Value < ObjectDef( NumObjectDefs ).NumRangeChks( Count ).MinMaxValue( 1 ) ) MinMaxError = true;
					} else if ( ObjectDef( NumObjectDefs ).NumRangeChks( Count ).WhichMinMax( 1 ) == 2 ) {
						if ( Value == ObjectDef( NumObjectDefs ).NumRangeChks( Count ).MinMaxValue( 1 ) ) MinMaxError = true;
					}
				}
				// check if error condition
				if ( MinMaxError ) {
					//  Error stated min is not in range with stated max
					MinMaxString = IPTrimSigDigits( ObjectDef( NumObjectDefs ).NumRangeChks( Count ).FieldNumber );
					ShowSevereError( "IP: IDD: Field #" + MinMaxString + " conflict in Min/Max specifications/values, in class=" + ObjectDef( NumObjectDefs ).Name, EchoInputFile );
					errFlag = true;
				}
			}
			if ( ObjectDef( NumObjectDefs ).NumRangeChks( Count ).DefaultChk ) {
				// Check Default against MinMaxRange
				//  Don't check when default is autosize...
				if ( ObjectDef( NumObjectDefs ).NumRangeChks( Count ).AutoSizable && ObjectDef( NumObjectDefs ).NumRangeChks( Count ).DefAutoSize ) continue;
				if ( ObjectDef( NumObjectDefs ).NumRangeChks( Count ).AutoCalculatable && ObjectDef( NumObjectDefs ).NumRangeChks( Count ).DefAutoCalculate ) continue;
				MinMaxError = false;
				Value = ObjectDef( NumObjectDefs ).NumRangeChks( Count ).Default;
				if ( ObjectDef( NumObjectDefs ).NumRangeChks( Count ).WhichMinMax( 1 ) == 1 ) {
					if ( Value < ObjectDef( NumObjectDefs ).NumRangeChks( Count ).MinMaxValue( 1 ) ) MinMaxError = true;
				} else if ( ObjectDef( NumObjectDefs ).NumRangeChks( Count ).WhichMinMax( 1 ) == 2 ) {
					if ( Value <= ObjectDef( NumObjectDefs ).NumRangeChks( Count ).MinMaxValue( 1 ) ) MinMaxError = true;
				}
				if ( ObjectDef( NumObjectDefs ).NumRangeChks( Count ).WhichMinMax( 2 ) == 3 ) {
					if ( Value > ObjectDef( NumObjectDefs ).NumRangeChks( Count ).MinMaxValue( 2 ) ) MinMaxError = true;
				} else if ( ObjectDef( NumObjectDefs ).NumRangeChks( Count ).WhichMinMax( 2 ) == 4 ) {
					if ( Value >= ObjectDef( NumObjectDefs ).NumRangeChks( Count ).MinMaxValue( 2 ) ) MinMaxError = true;
				}
				if ( MinMaxError ) {
					//  Error stated default is not in min/max range
					MinMaxString = IPTrimSigDigits( ObjectDef( NumObjectDefs ).NumRangeChks( Count ).FieldNumber );
					ShowSevereError( "IP: IDD: Field #" + MinMaxString + " default is invalid for Min/Max values, in class=" + ObjectDef( NumObjectDefs ).Name, EchoInputFile );
					errFlag = true;
				}
			}
		}

		if ( errFlag ) {
			ShowContinueError( "IP: Errors occurred in ObjectDefinition for Class=" + ObjectDef( NumObjectDefs ).Name + ", Object not available for IDF processing.", EchoInputFile );
			ObjectDef( NumObjectDefs ).AlphaOrNumeric.deallocate();
			ObjectDef( NumObjectDefs ).NumRangeChks.deallocate();
			ObjectDef( NumObjectDefs ).AlphFieldChks.deallocate();
			ObjectDef( NumObjectDefs ).AlphFieldDefs.deallocate();
			ObjectDef( NumObjectDefs ).ReqField.deallocate();
			ObjectDef( NumObjectDefs ).AlphRetainCase.deallocate();
			--NumObjectDefs;
			ErrorsFound = true;
		}

	}

	void
	ProcessInputDataFile( std::istream & idf_stream )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   August 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine processes input data file for EnergyPlus.  Each "record" is
		// parsed into the LineItem data structure and, if okay, put into the
		// IDFRecords data structure.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		bool EndofFile( false );
		bool BlankLine( false );
		std::string::size_type Pos;

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

		// Check file
		cross_platform_get_line( idf_stream, InputLine );
		std::string::size_type const len_line( InputLine.length() );
		if ( idf_stream && ( len_line > 0 ) ) {
			if ( int( InputLine[ len_line - 1 ] ) == iUnicode_end ) {
				ShowSevereError( "ProcessInput: \"in.idf\" appears to be a Unicode or binary file." );
				ShowContinueError( "...This file cannot be read by this program. Please save as PC or Unix file and try again" );
				ShowFatalError( "Program terminates due to previous condition." );
			}
		}
		if ( idf_stream ) idf_stream.seekg( 0, std::ios::beg );

		while ( ! EndofFile ) {
			ReadInputLine( idf_stream, Pos, BlankLine, EndofFile );
			if ( BlankLine || EndofFile ) continue;
			Pos = scan( InputLine, ",;" );
			if ( Pos != std::string::npos ) {
				if ( InputLine[ Pos ] == ';' ) {
					ValidateSection( InputLine.substr( 0, Pos ), NumLines );
					if ( NumIDFSections == MaxIDFSections ) {
						SectionsOnFile.redimension( MaxIDFSections += SectionsIDFAllocInc );
					}
				} else {
					ValidateObjectandParse( idf_stream, InputLine.substr( 0, Pos ), Pos, EndofFile );
					if ( NumIDFRecords == MaxIDFRecords ) {
						IDFRecords.redimension( MaxIDFRecords += ObjectsIDFAllocInc );
					}
				}
			} else { // Error condition, no , or ; on first line
				ShowMessage( "IP: IDF Line~" + IPTrimSigDigits( NumLines ) + ' ' + InputLine );
				ShowSevereError( ", or ; expected on this line", EchoInputFile );
			}
		}

		//   IF (NumIDFSections > 0) THEN
		//     SectionsOnFile(NumIDFSections)%LastRecord=NumIDFRecords
		//   ENDIF

		if ( NumIDFRecords > 0 ) {
			for ( int i = 1; i <= NumObjectDefs; ++i ) {
				if ( ObjectDef( i ).RequiredObject && ObjectDef( i ).NumFound == 0 ) {
					ShowSevereError( "IP: No items found for Required Object=" + ObjectDef( i ).Name );
					++NumMiscErrorsFound;
				}
			}
		}

	}

	void
	ValidateSection(
		std::string const & ProposedSection,
		int const LineNo
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   September 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine validates the section from the input data file
		// with the list of objects from the data dictionary file.

		// METHODOLOGY EMPLOYED:
		// A "squeezed" string is formed and checked against the list of
		// sections.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		std::string SqueezedSection;
		int Found;
		int OFound;

		SqueezedSection = MakeUPPERCase( stripped( ProposedSection ) );
		if ( len( stripped( ProposedSection ) ) > static_cast< std::string::size_type >( MaxSectionNameLength ) ) {
			ShowWarningError( "IP: Section length exceeds maximum, will be truncated=" + ProposedSection, EchoInputFile );
			ShowContinueError( "Will be processed as Section=" + SqueezedSection, EchoInputFile );
		}
		if ( ! has_prefix( SqueezedSection, "END" ) ) {
			Found = FindItemInList( SqueezedSection, SectionDef );
			if ( Found == 0 ) {
				// Make sure this Section not an object name
				if ( SortedIDD ) {
					OFound = FindItemInSortedList( SqueezedSection, ListOfObjects, NumObjectDefs );
					if ( OFound != 0 ) OFound = iListOfObjects( OFound );
				} else {
					OFound = FindItemInList( SqueezedSection, ListOfObjects, NumObjectDefs );
				}
				if ( OFound != 0 ) {
					AddRecordFromSection( OFound );
				} else if ( NumSectionDefs == MaxSectionDefs ) {
					SectionDef.redimension( MaxSectionDefs += SectionDefAllocInc );
				}
				++NumSectionDefs;
				SectionDef( NumSectionDefs ).Name = SqueezedSection;
				SectionDef( NumSectionDefs ).NumFound = 1;
				// Add to "Sections on file" if appropriate
				if ( ! ProcessingIDD ) {
					++NumIDFSections;
					SectionsOnFile( NumIDFSections ).Name = SqueezedSection;
					SectionsOnFile( NumIDFSections ).FirstRecord = NumIDFRecords;
					SectionsOnFile( NumIDFSections ).FirstLineNo = LineNo;
				}
			} else {
				//      IF (NumIDFSections > 0) THEN
				//        SectionsOnFile(NumIDFSections)%LastRecord=NumIDFRecords
				//      ENDIF
				++SectionDef( Found ).NumFound;
				if ( ! ProcessingIDD ) {
					++NumIDFSections;
					SectionsOnFile( NumIDFSections ).Name = SqueezedSection;
					SectionsOnFile( NumIDFSections ).FirstRecord = NumIDFRecords;
					SectionsOnFile( NumIDFSections ).FirstLineNo = LineNo;
				}
			}
		} else { // End ...
			if ( ! ProcessingIDD ) {
				SqueezedSection.erase( 0, 3 );
				strip( SqueezedSection );
				for ( Found = NumIDFSections; Found >= 1; --Found ) {
					if ( ! SameString( SectionsOnFile( Found ).Name, SqueezedSection ) ) continue;
					SectionsOnFile( Found ).LastRecord = NumIDFRecords;
				}
			}
		}

	}

	void
	ValidateObjectandParse(
		std::istream & idf_stream,
		std::string const & ProposedObject,
		std::string::size_type & CurPos,
		bool & EndofFile
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   September 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine validates the proposed object from the IDF and then
		// parses it, putting it into the internal InputProcessor Data structure.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const dimLineBuf( 10 );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		std::string SqueezedObject;
		std::string SqueezedArg;
		int Found;
		int NumArg;
		int NumArgExpected;
		int NumAlpha;
		int NumNumeric;
		std::string::size_type Pos;
		bool EndofObject;
		bool BlankLine;
		static bool errFlag( false );
		std::string::size_type LenLeft;
		int Count;
		std::string FieldString;
		std::string FieldNameString;
		std::string Message;
		std::string cStartLine;
		std::string cStartName;
		static Array1D_string LineBuf( dimLineBuf );
		static int StartLine;
		static int NumConxLines;
		static int CurLines;
		static int CurQPtr;

		std::string String;
		bool IDidntMeanIt;
		bool TestingObject;
		bool TransitionDefer;
		int TFound;
		std::string::size_type NextChr;
		std::string String1;

		SqueezedObject = MakeUPPERCase( stripped( ProposedObject ) );
		if ( len( SqueezedObject ) > static_cast< std::string::size_type >( MaxObjectNameLength ) ) {
			ShowWarningError( "IP: Object name length exceeds maximum, will be truncated=" + stripped( ProposedObject ), EchoInputFile );
			ShowContinueError( "Will be processed as Object=" + SqueezedObject, EchoInputFile );
		}
		IDidntMeanIt = false;

		TestingObject = true;
		TransitionDefer = false;
		while ( TestingObject ) {
			errFlag = false;
			IDidntMeanIt = false;
			if ( SortedIDD ) {
				Found = FindItemInSortedList( SqueezedObject, ListOfObjects, NumObjectDefs );
				if ( Found != 0 ) Found = iListOfObjects( Found );
			} else {
				Found = FindItemInList( SqueezedObject, ListOfObjects, NumObjectDefs );
			}
			if ( Found != 0 ) {
				if ( ObjectDef( Found ).ObsPtr > 0 ) {
					TFound = FindItemInList( SqueezedObject, RepObjects, &SecretObjects::OldName );
					if ( TFound != 0 ) {
						if ( RepObjects( TFound ).Transitioned ) {
							if ( ! RepObjects( TFound ).Used ) ShowWarningError( "IP: Objects=\"" + stripped( ProposedObject ) + "\" are being transitioned to this object=\"" + RepObjects( TFound ).NewName + "\"" );
							RepObjects( TFound ).Used = true;
							if ( SortedIDD ) {
								Found = FindItemInSortedList( SqueezedObject, ListOfObjects, NumObjectDefs );
								if ( Found != 0 ) Found = iListOfObjects( Found );
							} else {
								Found = FindItemInList( SqueezedObject, ListOfObjects, NumObjectDefs );
							}
						} else if ( RepObjects( TFound ).TransitionDefer ) {
							if ( ! RepObjects( TFound ).Used ) ShowWarningError( "IP: Objects=\"" + stripped( ProposedObject ) + "\" are being transitioned to this object=\"" + RepObjects( TFound ).NewName + "\"" );
							RepObjects( TFound ).Used = true;
							if ( SortedIDD ) {
								Found = FindItemInSortedList( SqueezedObject, ListOfObjects, NumObjectDefs );
								if ( Found != 0 ) Found = iListOfObjects( Found );
							} else {
								Found = FindItemInList( SqueezedObject, ListOfObjects, NumObjectDefs );
							}
							TransitionDefer = true;
						} else {
							Found = 0; // being handled differently for this obsolete object
						}
					}
				}
			}

			TestingObject = false;
			if ( Found == 0 ) {
				// Check to see if it's a "secret" object
				Found = FindItemInList( SqueezedObject, RepObjects, &SecretObjects::OldName );
				if ( Found == 0 ) {
					ShowSevereError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Did not find \"" + stripped( ProposedObject ) + "\" in list of Objects", EchoInputFile );
					// Will need to parse to next ;
					errFlag = true;
				} else if ( RepObjects( Found ).Deleted ) {
					if ( ! RepObjects( Found ).Used ) {
						ShowWarningError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Objects=\"" + stripped( ProposedObject ) + "\" have been deleted from the IDD.  Will be ignored." );
						RepObjects( Found ).Used = true;
					}
					IDidntMeanIt = true;
					errFlag = true;
					Found = 0;
				} else if ( RepObjects( Found ).TransitionDefer ) {

				} else { // This name is replaced with something else
					if ( ! RepObjects( Found ).Used ) {
						if ( ! RepObjects( Found ).Transitioned ) {
							ShowWarningError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Objects=\"" + stripped( ProposedObject ) + "\" are being replaced with this object=\"" + RepObjects( Found ).NewName + "\"" );
							RepObjects( Found ).Used = true;
							SqueezedObject = RepObjects( Found ).NewName;
							TestingObject = true;
						} else {
							ShowWarningError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Objects=\"" + stripped( ProposedObject ) + "\" are being transitioned to this object=\"" + RepObjects( Found ).NewName + "\"" );
							RepObjects( Found ).Used = true;
							if ( SortedIDD ) {
								Found = FindItemInSortedList( SqueezedObject, ListOfObjects, NumObjectDefs );
								if ( Found != 0 ) Found = iListOfObjects( Found );
							} else {
								Found = FindItemInList( SqueezedObject, ListOfObjects, NumObjectDefs );
							}
						}
					} else if ( ! RepObjects( Found ).Transitioned ) {
						SqueezedObject = RepObjects( Found ).NewName;
						TestingObject = true;
					} else {
						if ( SortedIDD ) {
							Found = FindItemInSortedList( SqueezedObject, ListOfObjects, NumObjectDefs );
							if ( Found != 0 ) Found = iListOfObjects( Found );
						} else {
							Found = FindItemInList( SqueezedObject, ListOfObjects, NumObjectDefs );
						}
					}
				}
			} else {

				// Start Parsing the Object according to definition

				errFlag = false;
				LineItem.Name = SqueezedObject;
				LineItem.Alphas = BlankString;
				LineItem.AlphBlank = false;
				LineItem.NumAlphas = 0;
				LineItem.Numbers = 0.0;
				LineItem.NumNumbers = 0;
				LineItem.NumBlank = false;
				LineItem.ObjectDefPtr = Found;
				NumArgExpected = ObjectDef( Found ).NumParams;
				++ObjectDef( Found ).NumFound;
				if ( ObjectDef( Found ).UniqueObject && ObjectDef( Found ).NumFound > 1 ) {
					ShowSevereError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Multiple occurrences of Unique Object=" + stripped( ProposedObject ) );
					++NumMiscErrorsFound;
				}
				if ( ObjectDef( Found ).ObsPtr > 0 ) {
					TFound = FindItemInList( SqueezedObject, RepObjects, &SecretObjects::OldName );
					if ( TFound == 0 ) {
						ShowWarningError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Obsolete object=" + stripped( ProposedObject ) + ", encountered.  Should be replaced with new object=" + ObsoleteObjectsRepNames( ObjectDef( Found ).ObsPtr ) );
					} else if ( ! RepObjects( TFound ).Used && RepObjects( TFound ).Transitioned ) {
						ShowWarningError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Objects=\"" + stripped( ProposedObject ) + "\" are being transitioned to this object=\"" + RepObjects( TFound ).NewName + "\"" );
						RepObjects( TFound ).Used = true;
					}
				}
			}
		}

		NumArg = 0;
		NumAlpha = 0;
		NumNumeric = 0;
		EndofObject = false;
		++CurPos;

		//  Keep context buffer in case of errors
		LineBuf = BlankString;
		NumConxLines = 0;
		StartLine = NumLines;
		cStartLine = InputLine.substr( 0, 300 );
		cStartName = BlankString;
		NumConxLines = 0;
		CurLines = NumLines;
		CurQPtr = 0;

		while ( ! EndofFile && ! EndofObject ) {
			if ( CurLines != NumLines ) {
				NumConxLines = min( NumConxLines + 1, dimLineBuf );
				++CurQPtr;
				if ( CurQPtr == 1 && is_blank( cStartName ) && not_blank( InputLine ) ) {
					if ( Found > 0 ) {
						if ( ObjectDef( Found ).NameAlpha1 ) {
							Pos = index( InputLine, ',' );
							if ( Pos != std::string::npos ) {
								cStartName = stripped( InputLine.substr( 0, Pos ) );
							}
						}
					}
				}
				if ( CurQPtr > dimLineBuf ) CurQPtr = 1;
				LineBuf( CurQPtr ) = InputLine;
				CurLines = NumLines;
			}
			if ( CurPos < static_cast< std::string::size_type >( InputLineLength ) ) {
				Pos = scan( InputLine.substr( CurPos, InputLineLength - CurPos ), ",;" );
				if ( Pos == std::string::npos ) {
					if ( InputLine[ InputLineLength - 1 ] == '!' ) {
						LenLeft = len_trim( InputLine.substr( CurPos, InputLineLength - CurPos - 1 ) );
					} else {
						LenLeft = len_trim( InputLine.substr( CurPos, InputLineLength - CurPos ) );
					}
					if ( LenLeft == 0 ) {
						CurPos = InputLineLength;
						continue;
					} else {
						if ( InputLine[ InputLineLength - 1 ] == '!' ) {
							Pos = InputLineLength - CurPos;
							DumpCurrentLineBuffer( StartLine, cStartLine, cStartName, NumLines, NumConxLines, LineBuf, CurQPtr );
							ShowWarningError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Comma being inserted after:\"" + InputLine.substr( CurPos, InputLineLength - CurPos - 1 ) + "\" in Object=" + SqueezedObject, EchoInputFile );
						} else {
							Pos = InputLineLength - CurPos + 1;
							DumpCurrentLineBuffer( StartLine, cStartLine, cStartName, NumLines, NumConxLines, LineBuf, CurQPtr );
							ShowWarningError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Comma being inserted after:\"" + InputLine.substr( CurPos, InputLineLength - CurPos ) + "\" in Object=" + SqueezedObject, EchoInputFile );
						}
					}
				}
			} else {
				ReadInputLine( idf_stream, CurPos, BlankLine, EndofFile );
				continue;
			}
			if ( Pos != std::string::npos ) {
				if ( ! errFlag ) {
					if ( Pos > 0 ) {
						SqueezedArg = MakeUPPERCase( stripped( InputLine.substr( CurPos, Pos ) ) );
						if ( len( stripped( InputLine.substr( CurPos, Pos ) ) ) > static_cast< std::string::size_type >( MaxAlphaArgLength ) ) {
							DumpCurrentLineBuffer( StartLine, cStartLine, cStartName, NumLines, NumConxLines, LineBuf, CurQPtr );
							ShowWarningError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Alpha Argument length exceeds maximum, will be truncated=" + InputLine.substr( CurPos, Pos - 1 ), EchoInputFile );
							ShowContinueError( "Will be processed as Alpha=" + SqueezedArg, EchoInputFile );
						}
					} else {
						SqueezedArg = BlankString;
					}
					if ( NumArg == NumArgExpected && ! ObjectDef( Found ).ExtensibleObject ) {
						DumpCurrentLineBuffer( StartLine, cStartLine, cStartName, NumLines, NumConxLines, LineBuf, CurQPtr );
						ShowSevereError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Error detected for Object=" + ObjectDef( Found ).Name, EchoInputFile );
						ShowContinueError( " Maximum arguments reached for this object, trying to process ->" + SqueezedArg + "<-", EchoInputFile );
						errFlag = true;
					} else {
						if ( NumArg == NumArgExpected && ObjectDef( Found ).ExtensibleObject ) {
							ExtendObjectDefinition( Found, NumArgExpected );
						}
						++NumArg;
						if ( ObjectDef( Found ).AlphaOrNumeric( NumArg ) ) {
							if ( NumAlpha == ObjectDef( Found ).NumAlpha ) {
								DumpCurrentLineBuffer( StartLine, cStartLine, cStartName, NumLines, NumConxLines, LineBuf, CurQPtr );
								ShowSevereError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Error detected for Object=" + ObjectDef( Found ).Name, EchoInputFile );
								ShowContinueError( " Too many Alphas for this object, trying to process ->" + SqueezedArg + "<-", EchoInputFile );
								errFlag = true;
							} else {
								++NumAlpha;
								LineItem.NumAlphas = NumAlpha;
								if ( ObjectDef( Found ).AlphRetainCase( NumArg ) ) {
									SqueezedArg = InputLine.substr( CurPos, Pos );
									strip( SqueezedArg );
								}
								if ( ! SqueezedArg.empty() ) {
									LineItem.Alphas( NumAlpha ) = SqueezedArg;
								} else if ( ObjectDef( Found ).ReqField( NumArg ) ) { // Blank Argument
									if ( ! ObjectDef( Found ).AlphFieldDefs( NumAlpha ).empty() ) {
										LineItem.Alphas( NumAlpha ) = ObjectDef( Found ).AlphFieldDefs( NumAlpha );
									} else {
										if ( ObjectDef( Found ).NameAlpha1 && NumAlpha != 1 ) {
											DumpCurrentLineBuffer( StartLine, cStartLine, cStartName, NumLines, NumConxLines, LineBuf, CurQPtr );
											ShowSevereError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Error detected in Object=" + ObjectDef( Found ).Name + ", name=" + LineItem.Alphas( 1 ), EchoInputFile );
										} else {
											DumpCurrentLineBuffer( StartLine, cStartLine, cStartName, NumLines, NumConxLines, LineBuf, CurQPtr );
											ShowSevereError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Error detected in Object=" + ObjectDef( Found ).Name, EchoInputFile );
										}
										ShowContinueError( "Field [" + ObjectDef( Found ).AlphFieldChks( NumAlpha ) + "] is required but was blank", EchoInputFile );
										++NumBlankReqFieldFound;
									}
								} else {
									LineItem.AlphBlank( NumAlpha ) = true;
									if ( ! ObjectDef( Found ).AlphFieldDefs( NumAlpha ).empty() ) {
										LineItem.Alphas( NumAlpha ) = ObjectDef( Found ).AlphFieldDefs( NumAlpha );
									}
								}
							}
						} else {
							if ( NumNumeric == ObjectDef( Found ).NumNumeric ) {
								DumpCurrentLineBuffer( StartLine, cStartLine, cStartName, NumLines, NumConxLines, LineBuf, CurQPtr );
								ShowSevereError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Error detected for Object=" + ObjectDef( Found ).Name, EchoInputFile );
								ShowContinueError( " Too many Numbers for this object, trying to process ->" + SqueezedArg + "<-", EchoInputFile );
								errFlag = true;
							} else {
								++NumNumeric;
								LineItem.NumNumbers = NumNumeric;
								if ( ! SqueezedArg.empty() ) {
									if ( ! ObjectDef( Found ).NumRangeChks( NumNumeric ).AutoSizable && ! ObjectDef( Found ).NumRangeChks( NumNumeric ).AutoCalculatable ) {
										LineItem.Numbers( NumNumeric ) = ProcessNumber( SqueezedArg, errFlag );
									} else if ( SqueezedArg == "AUTOSIZE" ) {
										LineItem.Numbers( NumNumeric ) = ObjectDef( Found ).NumRangeChks( NumNumeric ).AutoSizeValue;
									} else if ( SqueezedArg == "AUTOCALCULATE" ) {
										LineItem.Numbers( NumNumeric ) = ObjectDef( Found ).NumRangeChks( NumNumeric ).AutoCalculateValue;
									} else {
										LineItem.Numbers( NumNumeric ) = ProcessNumber( SqueezedArg, errFlag );
									}
								} else { // numeric arg is blank.
									if ( ObjectDef( Found ).NumRangeChks( NumNumeric ).DefaultChk ) { // blank arg has default
										if ( ! ObjectDef( Found ).NumRangeChks( NumNumeric ).DefAutoSize && ! ObjectDef( Found ).NumRangeChks( NumNumeric ).AutoCalculatable ) {
											LineItem.Numbers( NumNumeric ) = ObjectDef( Found ).NumRangeChks( NumNumeric ).Default;
											LineItem.NumBlank( NumNumeric ) = true;
										} else if ( ObjectDef( Found ).NumRangeChks( NumNumeric ).DefAutoSize ) {
											LineItem.Numbers( NumNumeric ) = ObjectDef( Found ).NumRangeChks( NumNumeric ).AutoSizeValue;
											LineItem.NumBlank( NumNumeric ) = true;
										} else if ( ObjectDef( Found ).NumRangeChks( NumNumeric ).DefAutoCalculate ) {
											LineItem.Numbers( NumNumeric ) = ObjectDef( Found ).NumRangeChks( NumNumeric ).AutoCalculateValue;
											LineItem.NumBlank( NumNumeric ) = true;
										}
										errFlag = false;
									} else { // blank arg does not have default
										if ( ObjectDef( Found ).ReqField( NumArg ) ) { // arg is required
											if ( ObjectDef( Found ).NameAlpha1 ) { // object has name field - more context for error
												DumpCurrentLineBuffer( StartLine, cStartLine, cStartName, NumLines, NumConxLines, LineBuf, CurQPtr );
												ShowSevereError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Error detected in Object=" + ObjectDef( Found ).Name + ", name=" + LineItem.Alphas( 1 ), EchoInputFile );
												errFlag = true;
											} else { // object does not have name field
												DumpCurrentLineBuffer( StartLine, cStartLine, cStartName, NumLines, NumConxLines, LineBuf, CurQPtr );
												ShowSevereError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Error detected in Object=" + ObjectDef( Found ).Name, EchoInputFile );
												errFlag = true;
											}
											ShowContinueError( "Field [" + ObjectDef( Found ).NumRangeChks( NumNumeric ).FieldName + "] is required but was blank", EchoInputFile );
											++NumBlankReqFieldFound;
										}
										LineItem.Numbers( NumNumeric ) = 0.0;
										LineItem.NumBlank( NumNumeric ) = true;
									}
								}
								if ( errFlag ) {
									if ( SqueezedArg[ 0 ] != '=' ) { // argument does not start with "=" (parametric)
										FieldString = IPTrimSigDigits( NumNumeric );
										FieldNameString = ObjectDef( Found ).NumRangeChks( NumNumeric ).FieldName;
										if ( ! FieldNameString.empty() ) {
											Message = "Invalid Number in Numeric Field#" + FieldString + " (" + FieldNameString + "), value=" + SqueezedArg;
										} else { // Field Name not recorded
											Message = "Invalid Number in Numeric Field#" + FieldString + ", value=" + SqueezedArg;
										}
										Message += ", in " + ObjectDef( Found ).Name;
										if ( ObjectDef( Found ).NameAlpha1 ) {
											Message += "=" + LineItem.Alphas( 1 );
										}
										DumpCurrentLineBuffer( StartLine, cStartLine, cStartName, NumLines, NumConxLines, LineBuf, CurQPtr );
										ShowSevereError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + ' ' + Message, EchoInputFile );
									} else { // parametric in Numeric field
										errFlag = false;
									}
								}
							}
						}
					}
				}

				if ( InputLine[ CurPos + Pos ] == ';' ) {
					EndofObject = true;
					// Check if more characters on line -- and if first is a comment character
					if ( not_blank( InputLine.substr( CurPos + Pos + 1 ) ) ) {
						NextChr = FindNonSpace( InputLine.substr( CurPos + Pos + 1 ) );
						if ( ( NextChr == std::string::npos ) || ( InputLine[ CurPos + Pos + 1 + NextChr ] != '!' ) ) {
							DumpCurrentLineBuffer( StartLine, cStartLine, cStartName, NumLines, NumConxLines, LineBuf, CurQPtr );
							ShowWarningError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " End of Object=\"" + ObjectDef( Found ).Name + "\" reached, but next part of line not comment.", EchoInputFile );
							ShowContinueError( "Final line above shows line that contains error." );
						}
					}
				}
				CurPos += Pos + 1;
			}

		}

		// Store to IDFRecord Data Structure, errFlag is true if there was an error
		// Check out MinimumNumberOfFields
		if ( ! errFlag && ! IDidntMeanIt ) {
			if ( NumArg < ObjectDef( Found ).MinNumFields ) {
				if ( ObjectDef( Found ).NameAlpha1 ) {
					ShowAuditErrorMessage( " ** Warning ** ", "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Object=" + ObjectDef( Found ).Name + ", name=" + LineItem.Alphas( 1 ) + ", entered with less than minimum number of fields." );
				} else {
					ShowAuditErrorMessage( " ** Warning ** ", "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Object=" + ObjectDef( Found ).Name + ", entered with less than minimum number of fields." );
				}
				ShowAuditErrorMessage( " **   ~~~   ** ", "Attempting fill to minimum." );
				NumAlpha = 0;
				NumNumeric = 0;
				if ( ObjectDef( Found ).MinNumFields > ObjectDef( Found ).NumParams ) {
					String = IPTrimSigDigits( ObjectDef( Found ).MinNumFields );
					String1 = IPTrimSigDigits( ObjectDef( Found ).NumParams );
					ShowSevereError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Object \\min-fields > number of fields specified, Object=" + ObjectDef( Found ).Name );
					ShowContinueError( "..\\min-fields=" + String + ", total number of fields in object definition=" + String1 );
					errFlag = true;
				} else {
					for ( Count = 1; Count <= ObjectDef( Found ).MinNumFields; ++Count ) {
						if ( ObjectDef( Found ).AlphaOrNumeric( Count ) ) {
							++NumAlpha;
							if ( NumAlpha <= LineItem.NumAlphas ) continue;
							++LineItem.NumAlphas;
							if ( ! ObjectDef( Found ).AlphFieldDefs( LineItem.NumAlphas ).empty() ) {
								LineItem.Alphas( LineItem.NumAlphas ) = ObjectDef( Found ).AlphFieldDefs( LineItem.NumAlphas );
								ShowAuditErrorMessage( " **   Add   ** ", ObjectDef( Found ).AlphFieldDefs( LineItem.NumAlphas ) + "   ! field=>" + ObjectDef( Found ).AlphFieldChks( NumAlpha ) );
							} else if ( ObjectDef( Found ).ReqField( Count ) ) {
								if ( ObjectDef( Found ).NameAlpha1 ) {
									ShowSevereError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Object=" + ObjectDef( Found ).Name + ", name=" + LineItem.Alphas( 1 ) + ", Required Field=[" + ObjectDef( Found ).AlphFieldChks( NumAlpha ) + "] was blank.", EchoInputFile );
								} else {
									ShowSevereError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Object=" + ObjectDef( Found ).Name + ", Required Field=[" + ObjectDef( Found ).AlphFieldChks( NumAlpha ) + "] was blank.", EchoInputFile );
								}
								errFlag = true;
							} else {
								LineItem.Alphas( LineItem.NumAlphas ).clear();
								LineItem.AlphBlank( LineItem.NumAlphas ) = true;
								ShowAuditErrorMessage( " **   Add   ** ", "<blank field>   ! field=>" + ObjectDef( Found ).AlphFieldChks( NumAlpha ) );
							}
						} else {
							++NumNumeric;
							if ( NumNumeric <= LineItem.NumNumbers ) continue;
							++LineItem.NumNumbers;
							LineItem.NumBlank( NumNumeric ) = true;
							if ( ObjectDef( Found ).NumRangeChks( NumNumeric ).DefaultChk ) {
								if ( ! ObjectDef( Found ).NumRangeChks( NumNumeric ).DefAutoSize && ! ObjectDef( Found ).NumRangeChks( NumNumeric ).DefAutoCalculate ) {
									LineItem.Numbers( NumNumeric ) = ObjectDef( Found ).NumRangeChks( NumNumeric ).Default;
									gio::write( String, fmtLD ) << ObjectDef( Found ).NumRangeChks( NumNumeric ).Default;
									strip( String );
									ShowAuditErrorMessage( " **   Add   ** ", String + "   ! field=>" + ObjectDef( Found ).NumRangeChks( NumNumeric ).FieldName );
								} else if ( ObjectDef( Found ).NumRangeChks( NumNumeric ).DefAutoSize ) {
									LineItem.Numbers( NumNumeric ) = ObjectDef( Found ).NumRangeChks( NumNumeric ).AutoSizeValue;
									ShowAuditErrorMessage( " **   Add   ** ", "autosize    ! field=>" + ObjectDef( Found ).NumRangeChks( NumNumeric ).FieldName );
								} else if ( ObjectDef( Found ).NumRangeChks( NumNumeric ).DefAutoCalculate ) {
									LineItem.Numbers( NumNumeric ) = ObjectDef( Found ).NumRangeChks( NumNumeric ).AutoCalculateValue;
									ShowAuditErrorMessage( " **   Add   ** ", "autocalculate    ! field=>" + ObjectDef( Found ).NumRangeChks( NumNumeric ).FieldName );
								}
							} else if ( ObjectDef( Found ).ReqField( Count ) ) {
								if ( ObjectDef( Found ).NameAlpha1 ) {
									ShowSevereError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Object=" + ObjectDef( Found ).Name + ", name=" + LineItem.Alphas( 1 ) + ", Required Field=[" + ObjectDef( Found ).NumRangeChks( NumNumeric ).FieldName + "] was blank.", EchoInputFile );
								} else {
									ShowSevereError( "IP: IDF line~" + IPTrimSigDigits( NumLines ) + " Object=" + ObjectDef( Found ).Name + ", Required Field=[" + ObjectDef( Found ).NumRangeChks( NumNumeric ).FieldName + "] was blank.", EchoInputFile );
								}
								errFlag = true;
							} else {
								LineItem.Numbers( NumNumeric ) = 0.0;
								LineItem.NumBlank( NumNumeric ) = true;
								ShowAuditErrorMessage( " **   Add   ** ", "<blank field>   ! field=>" + ObjectDef( Found ).NumRangeChks( NumNumeric ).FieldName );
							}
						}
					}
				}
			}
		}

		if ( ! errFlag && ! IDidntMeanIt ) {
			if ( TransitionDefer ) {
				MakeTransition( Found );
			}
			++NumIDFRecords;
			if ( ObjectStartRecord( Found ) == 0 ) ObjectStartRecord( Found ) = NumIDFRecords;
			MaxAlphaIDFArgsFound = max( MaxAlphaIDFArgsFound, LineItem.NumAlphas );
			MaxNumericIDFArgsFound = max( MaxNumericIDFArgsFound, LineItem.NumNumbers );
			MaxAlphaIDFDefArgsFound = max( MaxAlphaIDFDefArgsFound, ObjectDef( Found ).NumAlpha );
			MaxNumericIDFDefArgsFound = max( MaxNumericIDFDefArgsFound, ObjectDef( Found ).NumNumeric );
			IDFRecords( NumIDFRecords ).Name = LineItem.Name;
			IDFRecords( NumIDFRecords ).NumNumbers = LineItem.NumNumbers;
			IDFRecords( NumIDFRecords ).NumAlphas = LineItem.NumAlphas;
			IDFRecords( NumIDFRecords ).ObjectDefPtr = LineItem.ObjectDefPtr;
			IDFRecords( NumIDFRecords ).Alphas.allocate( LineItem.NumAlphas );
			IDFRecords( NumIDFRecords ).Alphas = LineItem.Alphas( {1,LineItem.NumAlphas} );
			IDFRecords( NumIDFRecords ).AlphBlank.allocate( LineItem.NumAlphas );
			IDFRecords( NumIDFRecords ).AlphBlank = LineItem.AlphBlank( {1,LineItem.NumAlphas} );
			IDFRecords( NumIDFRecords ).Numbers.allocate( LineItem.NumNumbers );
			IDFRecords( NumIDFRecords ).Numbers = LineItem.Numbers( {1,LineItem.NumNumbers} );
			IDFRecords( NumIDFRecords ).NumBlank.allocate( LineItem.NumNumbers );
			IDFRecords( NumIDFRecords ).NumBlank = LineItem.NumBlank( {1,LineItem.NumNumbers} );
			if ( LineItem.NumNumbers > 0 ) {
				for ( Count = 1; Count <= LineItem.NumNumbers; ++Count ) {
					if ( ObjectDef( Found ).NumRangeChks( Count ).MinMaxChk && ! LineItem.NumBlank( Count ) ) {
						InternalRangeCheck( LineItem.Numbers( Count ), Count, Found, LineItem.Alphas( 1 ), ObjectDef( Found ).NumRangeChks( Count ).AutoSizable, ObjectDef( Found ).NumRangeChks( Count ).AutoCalculatable );
					}
				}
			}
		} else if ( ! IDidntMeanIt ) {
			OverallErrorFlag = true;
		}

	}

	void
	ValidateSectionsInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   September 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine uses the data structure that is set up during
		// IDF processing and makes sure that record pointers are accurate.
		// They could be inaccurate if a 'section' is input without any
		// 'objects' following.  The invalidity will show itself in the
		// values of the FirstRecord and Last Record pointer.
		// If FirstRecord>LastRecord, then no records (Objects) have been
		// written to the SIDF file for that Section.

		// METHODOLOGY EMPLOYED:
		// Scan the SectionsOnFile data structure and look for invalid
		// FirstRecord,LastRecord items.  Reset those items to -1.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		for ( int Count = 1; Count <= NumIDFSections; ++Count ) {
			if ( SectionsOnFile( Count ).FirstRecord > SectionsOnFile( Count ).LastRecord ) {
				gio::write( EchoInputFile, fmtLD ) << " Section " << Count << ' ' << SectionsOnFile( Count ).Name << " had no object records";
				SectionsOnFile( Count ).FirstRecord = -1;
				SectionsOnFile( Count ).LastRecord = -1;
			}
		}

	}

	int
	GetNumSectionsFound( std::string const & SectionWord )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   September 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This function returns the number of a particular section (in input data file)
		// found in the current run.  If it can't find the section in list
		// of sections, a -1 will be returned.

		// METHODOLOGY EMPLOYED:
		// Look up section in list of sections.  If there, return the
		// number of sections of that kind found in the current input.  If not, return
		// -1.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		int GetNumSectionsFound;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int Found = FindItemInList( MakeUPPERCase( SectionWord ), ListOfSections, NumSectionDefs );
		if ( Found == 0 ) {
			//    CALL ShowFatalError('Requested Section not found in Definitions: '//TRIM(SectionWord))
			GetNumSectionsFound = 0;
		} else {
			GetNumSectionsFound = SectionDef( Found ).NumFound;
		}

		return GetNumSectionsFound;

	}

	int
	GetNumSectionsinInput()
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   September 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This function returns the number of sections in the entire input data file
		// of the current run.

		// METHODOLOGY EMPLOYED:
		// Return value of NumIDFSections.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		return NumIDFSections;

	}

	void
	GetListofSectionsinInput(
		Array1S_string SectionList,
		int & NuminList
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   September 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine returns the list of sections as they occurred
		// in the Input Data File (IDF).

		// METHODOLOGY EMPLOYED:
		// Look up object in list of objects.  If there, return the
		// number of objects found in the current input.  If not, return
		// -1.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int const MaxAllowedOut = min( NumIDFSections, isize( SectionList ) );
		if ( MaxAllowedOut != NumIDFSections ) {
			ShowWarningError( "More in list than allowed in passed array - (GetListofSectionsinInput)" );
		}
		NuminList = MaxAllowedOut;
		for ( int i = 1; i <= MaxAllowedOut; ++i ) SectionList( i ) = SectionsOnFile( i ).Name;

	}

	int
	GetNumObjectsFound( std::string const & ObjectWord )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   September 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This function returns the number of objects (in input data file)
		// found in the current run.  If it can't find the object in list
		// of objects, a 0 will be returned.

		// METHODOLOGY EMPLOYED:
		// Look up object in list of objects.  If there, return the
		// number of objects found in the current input.  If not, return 0.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		int GetNumObjectsFound;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Found;

		if ( SortedIDD ) {
			Found = FindItemInSortedList( MakeUPPERCase( ObjectWord ), ListOfObjects, NumObjectDefs );
			if ( Found != 0 ) Found = iListOfObjects( Found );
		} else {
			Found = FindItemInList( MakeUPPERCase( ObjectWord ), ListOfObjects, NumObjectDefs );
		}

		if ( Found != 0 ) {
			GetNumObjectsFound = ObjectDef( Found ).NumFound;
		} else {
			GetNumObjectsFound = 0;
			ShowWarningError( "Requested Object not found in Definitions: " + ObjectWord );
		}

		return GetNumObjectsFound;

	}

	void
	GetRecordLocations(
		int const Which,
		int & FirstRecord,
		int & LastRecord
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   September 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine returns the record location values (which will be
		// passed to 'GetObjectItem') for a section from the list of inputted
		// sections (sequential).

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		FirstRecord = SectionsOnFile( Which ).FirstRecord;
		LastRecord = SectionsOnFile( Which ).LastRecord;

	}

	void
	GetObjectItem(
		std::string const & Object,
		int const Number,
		Array1S_string Alphas,
		int & NumAlphas,
		Array1S< Real64 > Numbers,
		int & NumNumbers,
		int & Status,
		Optional< Array1_bool > NumBlank,
		Optional< Array1_bool > AlphaBlank,
		Optional< Array1_string > AlphaFieldNames,
		Optional< Array1_string > NumericFieldNames
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   September 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine gets the 'number' 'object' from the IDFRecord data structure.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Count;
		int LoopIndex;
		std::string ObjectWord;
		std::string UCObject;
		//////////// hoisted into namespace ////////////
		// static Array1D_string AlphaArgs;
		// static Array1D< Real64 > NumberArgs;
		// static Array1D_bool AlphaArgsBlank;
		// static Array1D_bool NumberArgsBlank;
		////////////////////////////////////////////////
		int MaxAlphas;
		int MaxNumbers;
		int Found;
		int StartRecord;
		std::string cfld1;
		std::string cfld2;
		bool GoodItem;

		//Autodesk:Uninit Initialize variables used uninitialized
		NumAlphas = 0; //Autodesk:Uninit Force default initialization
		NumNumbers = 0; //Autodesk:Uninit Force default initialization

		MaxAlphas = isize( Alphas, 1 );
		MaxNumbers = isize( Numbers, 1 );
		GoodItem = false;

		if ( ! allocated( AlphaArgs ) ) {
			if ( NumObjectDefs == 0 ) {
				ProcessInput();
			}
			AlphaArgs.allocate( MaxAlphaArgsFound );
			NumberArgs.allocate( MaxNumericArgsFound );
			NumberArgsBlank.allocate( MaxNumericArgsFound );
			AlphaArgsBlank.allocate( MaxAlphaArgsFound );
		}

		Count = 0;
		Status = -1;
		UCObject = MakeUPPERCase( Object );
		if ( SortedIDD ) {
			Found = FindItemInSortedList( UCObject, ListOfObjects, NumObjectDefs );
			if ( Found != 0 ) Found = iListOfObjects( Found );
		} else {
			Found = FindItemInList( UCObject, ListOfObjects, NumObjectDefs );
		}
		if ( Found == 0 ) { //  This is more of a developer problem
			ShowFatalError( "IP: GetObjectItem: Requested object=" + UCObject + ", not found in Object Definitions -- incorrect IDD attached." );
		}

		if ( ObjectDef( Found ).NumAlpha > 0 ) {
			if ( ObjectDef( Found ).NumAlpha > MaxAlphas ) {
				cfld1 = IPTrimSigDigits( ObjectDef( Found ).NumAlpha );
				cfld2 = IPTrimSigDigits( MaxAlphas );
				ShowFatalError( "IP: GetObjectItem: " + Object + ", Number of ObjectDef Alpha Args [" + cfld1 + "] > Size of AlphaArg array [" + cfld2 + "]." );
			}
			Alphas( {1,ObjectDef( Found ).NumAlpha} ) = BlankString;
		}
		if ( ObjectDef( Found ).NumNumeric > 0 ) {
			if ( ObjectDef( Found ).NumNumeric > MaxNumbers ) {
				cfld1 = IPTrimSigDigits( ObjectDef( Found ).NumNumeric );
				cfld2 = IPTrimSigDigits( MaxNumbers );
				ShowFatalError( "IP: GetObjectItem: " + Object + ", Number of ObjectDef Numeric Args [" + cfld1 + "] > Size of NumericArg array [" + cfld2 + "]." );
			}
			Numbers( {1,ObjectDef( Found ).NumNumeric} ) = 0.0;
		}

		StartRecord = ObjectStartRecord( Found );
		if ( StartRecord == 0 ) {
			ShowWarningError( "IP: GetObjectItem: Requested object=" + UCObject + ", not found in IDF." );
			Status = -1;
			StartRecord = NumIDFRecords + 1;
		}

		if ( ObjectGotCount( Found ) == 0 ) {
			gio::write( EchoInputFile, fmtLD ) << "Getting object=" << UCObject;
		}
		++ObjectGotCount( Found );

		for ( LoopIndex = StartRecord; LoopIndex <= NumIDFRecords; ++LoopIndex ) {
			if ( IDFRecords( LoopIndex ).Name == UCObject ) {
				++Count;
				if ( Count == Number ) {
					IDFRecordsGotten( LoopIndex ) = true; // only object level "gets" recorded
					// Read this one
					GetObjectItemfromFile( LoopIndex, ObjectWord, NumAlphas, NumNumbers, AlphaArgs, NumberArgs, AlphaArgsBlank, NumberArgsBlank );
					if ( NumAlphas > MaxAlphas || NumNumbers > MaxNumbers ) {
						ShowFatalError( "IP: GetObjectItem: Too many actual arguments for those expected on Object: " + ObjectWord, EchoInputFile );
					}
					NumAlphas = min( MaxAlphas, NumAlphas );
					NumNumbers = min( MaxNumbers, NumNumbers );
					GoodItem = true;
					if ( NumAlphas > 0 ) {
						Alphas( {1,NumAlphas} ) = AlphaArgs( {1,NumAlphas} );
					}
					if ( NumNumbers > 0 ) {
						Numbers( {1,NumNumbers} ) = NumberArgs( {1,NumNumbers} );
					}
					if ( present( NumBlank ) ) {
						NumBlank = true;
						if ( NumNumbers > 0 ) NumBlank()( {1,NumNumbers} ) = NumberArgsBlank( {1,NumNumbers} );
					}
					if ( present( AlphaBlank ) ) {
						AlphaBlank = true;
						if ( NumAlphas > 0 ) AlphaBlank()( {1,NumAlphas} ) = AlphaArgsBlank( {1,NumAlphas} );
					}
					if ( present( AlphaFieldNames ) ) {
						AlphaFieldNames()( {1,ObjectDef( Found ).NumAlpha} ) = ObjectDef( Found ).AlphFieldChks( {1,ObjectDef( Found ).NumAlpha} );
					}
					if ( present( NumericFieldNames ) ) {
						for ( int i = 1, e = ObjectDef( Found ).NumNumeric; i <= e; ++i ) NumericFieldNames()( i ) = ObjectDef( Found ).NumRangeChks( i ).FieldName;
					}
					Status = 1;
					break;
				}
			}
		}

#ifdef IDDTEST
		// This checks various principles of the IDD (e.g. required fields, defaults) to see what happens in the GetInput
		// This can only work for "good" objects. (Found=object def)
		// exempt certain objects
		if ( SameString( ObjectDef( Found ).Name, "Schedule:Compact" ) ) return;
		if ( has_prefixi( ObjectDef( Found ).Name, "Schedule" ) ) return;
		if ( SameString( ObjectDef( Found ).Name, "Construction" ) ) return;
		if ( SameString( ObjectDef( Found ).Name, "Construction:InternalSource" ) ) return;
		if ( SameString( ObjectDef( Found ).Name, "BuildingSurface:Detailed" ) ) return;
		if ( SameString( ObjectDef( Found ).Name, "Wall:Detailed" ) ) return;
		if ( SameString( ObjectDef( Found ).Name, "RoofCeiling:Detailed" ) ) return;
		if ( SameString( ObjectDef( Found ).Name, "Floor:Detailed" ) ) return;
		if ( SameString( ObjectDef( Found ).Name, "FenestrationSurface:Detailed" ) ) return;
		if ( SameString( ObjectDef( Found ).Name, "SizingPeriod:DesignDay" ) ) return;
		if ( SameString( ObjectDef( Found ).Name, "Branch" ) ) return;

		if ( GoodItem ) {
			NAfld = 0;
			NNfld = 0;
			for ( LoopIndex = 1; LoopIndex <= NumAlphas + NumNumbers; ++LoopIndex ) {
				if ( LoopIndex > ObjectDef( Found ).MinNumFields ) break;
				if ( ObjectDef( Found ).AlphaOrNumeric( LoopIndex ) ) {
					++NAfld;
					if ( ! ObjectDef( Found ).ReqField( LoopIndex ) ) {
						if ( AlphaArgsBlank( NAfld ) ) Alphas( NAfld ) = ObjectDef( Found ).AlphFieldDefs( NAfld );
						if ( present( AlphaBlank ) ) {
							if ( is_blank( Alphas( NAfld ) ) ) {
								AlphaBlank()( NAfld ) = true;
							} else {
								AlphaBlank()( NAfld ) = false;
							}
						}
					}
				} else {
					++NNfld;
					if ( ! ObjectDef( Found ).ReqField( LoopIndex ) ) {
						Numbers( NNfld ) = ObjectDef( Found ).NumRangeChks( NNfld ).Default;
						if ( ObjectDef( Found ).NumRangeChks( NNfld ).DefAutoSize ) Numbers( NNfld ) = ObjectDef( Found ).NumRangeChks( NNfld ).AutoSizeValue;
						if ( ObjectDef( Found ).NumRangeChks( NNfld ).AutoCalculatable ) Numbers( NNfld ) = ObjectDef( Found ).NumRangeChks( NNfld ).AutoCalculateValue;

						if ( present( NumBlank ) ) {
							if ( Numbers( NNfld ) == ObjectDef( Found ).NumRangeChks( NNfld ).Default ) {
								NumBlank()( NNfld ) = true;
							} else {
								NumBlank()( NNfld ) = false;
							}
						}
					}
				}
			}
		}
#endif

	}

	int
	GetObjectItemNum(
		std::string const & ObjType, // Object Type (ref: IDD Objects)
		std::string const & ObjName // Name of the object type
	)
	{

		// SUBROUTINE INFORMATION
		//             AUTHOR:  Fred Buhl
		//       DATE WRITTEN:  Jan 1998
		//           MODIFIED:  Lawrie, September 1999. Take advantage of internal
		//                      InputProcessor structures to speed search.
		//      RE-ENGINEERED:  This is new code, not reengineered

		// PURPOSE OF THIS SUBROUTINE:
		// Get the occurrence number of an object of type ObjType and name ObjName

		// METHODOLOGY EMPLOYED:
		// Use internal IDF record structure for each object occurrence
		// and compare the name with ObjName.

		// REFERENCES:
		// na

		// Return value
		int GetObjectItemNum;

		// Locals
		// SUBROUTINE ARGUMENTS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK DEFINITIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DEFINITIONS
		int NumObjOfType; // Total number of Object Type in IDF
		int ObjNum; // Loop index variable
		int ItemNum; // Item number for Object Name
		int Found; // Indicator for Object Type in list of Valid Objects
		std::string UCObjType; // Upper Case for ObjType
		bool ItemFound; // Set to true if item found
		bool ObjectFound; // Set to true if object found
		int StartRecord; // Start record for objects

		ItemNum = 0;
		ItemFound = false;
		ObjectFound = false;
		UCObjType = MakeUPPERCase( ObjType );
		if ( SortedIDD ) {
			Found = FindItemInSortedList( UCObjType, ListOfObjects, NumObjectDefs );
			if ( Found != 0 ) Found = iListOfObjects( Found );
		} else {
			Found = FindItemInList( UCObjType, ListOfObjects, NumObjectDefs );
		}

		if ( Found != 0 ) {

			ObjectFound = true;
			NumObjOfType = ObjectDef( Found ).NumFound;
			ItemNum = 0;
			StartRecord = ObjectStartRecord( Found );

			if ( StartRecord > 0 ) {
				for ( ObjNum = StartRecord; ObjNum <= NumIDFRecords; ++ObjNum ) {
					if ( IDFRecords( ObjNum ).Name != UCObjType ) continue;
					++ItemNum;
					if ( ItemNum > NumObjOfType ) break;
					if ( IDFRecords( ObjNum ).Alphas( 1 ) == ObjName ) {
						ItemFound = true;
						break;
					}
				}
			}
		}

		if ( ObjectFound ) {
			if ( ! ItemFound ) ItemNum = 0;
		} else {
			ItemNum = -1; // if object not found, then flag it
		}

		GetObjectItemNum = ItemNum;

		return GetObjectItemNum;

	}

	void
	TellMeHowManyObjectItemArgs(
		std::string const & Object,
		int const Number,
		int & NumAlpha,
		int & NumNumbers,
		int & Status
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   September 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine returns the number of arguments (alpha and numeric) for
		// the referenced 'number' Object.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Count;
		int LoopIndex;
		std::string ObjectWord;

		Count = 0;
		Status = -1;
		for ( LoopIndex = 1; LoopIndex <= NumIDFRecords; ++LoopIndex ) {
			if ( SameString( IDFRecords( LoopIndex ).Name, Object ) ) {
				++Count;
				if ( Count == Number ) {
					// Read this one
					GetObjectItemfromFile( LoopIndex, ObjectWord, NumAlpha, NumNumbers );
					Status = 1;
					break;
				}
			}
		}

	}

	void
	GetObjectItemfromFile(
		int const Which,
		std::string & ObjectWord,
		int & NumAlpha,
		int & NumNumeric,
		Optional< Array1S_string > AlphaArgs,
		Optional< Array1S< Real64 > > NumericArgs,
		Optional< Array1S_bool > AlphaBlanks,
		Optional< Array1S_bool > NumericBlanks
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   September 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine "gets" the object instance from the data structure.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using
		using DataStringGlobals::NL;

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// Object Data
		LineDefinition xLineItem; // Description of current record

		if ( Which > 0 && Which <= NumIDFRecords ) {
			xLineItem = IDFRecords( Which );
			ObjectWord = xLineItem.Name;
			NumAlpha = xLineItem.NumAlphas;
			NumNumeric = xLineItem.NumNumbers;
			if ( present( AlphaArgs ) ) {
				if ( NumAlpha >= 1 ) {
					AlphaArgs()( {1,NumAlpha} ) = xLineItem.Alphas( {1,NumAlpha} );
				}
			}
			if ( present( AlphaBlanks ) ) {
				if ( NumAlpha >= 1 ) {
					AlphaBlanks()( {1,NumAlpha} ) = xLineItem.AlphBlank( {1,NumAlpha} );
				}
			}
			if ( present( NumericArgs ) ) {
				if ( NumNumeric >= 1 ) {
					NumericArgs()( {1,NumNumeric} ) = xLineItem.Numbers( {1,NumNumeric} );
				}
			}
			if ( present( NumericBlanks ) ) {
				if ( NumNumeric >= 1 ) {
					NumericBlanks()( {1,NumNumeric} ) = xLineItem.NumBlank( {1,NumNumeric} );
				}
			}
		} else {
			if ( echo_stream ) *echo_stream << " Requested Record " << Which << " not in range, 1 -- " << NumIDFRecords << NL;
		}

	}

	// Utility Functions/Routines for Module

	void
	ReadInputLine(
		std::istream & in_stream,
		std::string::size_type & CurPos,
		bool & BlankLine,
		bool & EndofFile
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   September 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine reads a line in the specified file and checks for end of file

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using
		using DataStringGlobals::NL;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		char const SPC( ' ' );
		char const TAB( '\t' );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ReadStat;
		std::string::size_type Pos;
		std::string::size_type Slash;
		std::string::size_type NSpace;
		bool errFlag;

		errFlag = false;
		{ IOFlags flags; cross_platform_get_line( in_stream, InputLine ); flags.set_status( in_stream ); ReadStat = flags.ios(); }

		if ( ReadStat != 0 ) InputLine.clear();
		std::replace( InputLine.begin(), InputLine.end(), TAB, SPC ); // Replace tabs with spaces

		BlankLine = false;
		CurPos = 0;
		if ( ReadStat < 0 ) {
			EndofFile = true;
		} else {
			if ( EchoInputLine ) {
				++NumLines;
				if ( DisplayInputInAudit ) {
					if ( echo_stream ) *echo_stream << std::setw( 7 ) << NumLines << ' ' << InputLine << NL;
				}
			}
			EchoInputLine = true;
			InputLineLength = static_cast< int >( len_trim( InputLine ) );
			if ( InputLineLength == 0 ) {
				BlankLine = true;
			}
			Pos = ( ProcessingIDD ? InputLine.find_first_of( "!\\" ) : InputLine.find( '!' ) ); // 4/30/09 remove ~
			if ( Pos != std::string::npos ) {
				Slash = ( ProcessingIDD ? InputLine.find( '\\' ) : std::string::npos );
				InputLineLength = static_cast< int >( Pos + 1 );
				if ( Pos > 0 ) {
					if ( is_blank( InputLine.substr( 0, Pos ) ) ) {
						BlankLine = true;
					}
				} else {
					BlankLine = true;
				}
				if ( ( Slash != std::string::npos ) && ( Pos == Slash ) ) {
					std::string UCInputLine( InputLine, Pos );
					uppercase( UCInputLine ); // With this many comparisons probably faster to uppercase once vs many c-i comparisons
					FieldSet = false;
					switch ( UCInputLine.length() > 1 ? UCInputLine[ 1 ] : ' ' ) { //Performance Switch to reduce expensive string operations
					case 'E':
						if ( has_prefix( UCInputLine, "\\EXTENSIBLE" ) ) { // Extensible arg
							ExtensibleObject = true;
							if ( UCInputLine[ 11 ] != ':' ) {
								ShowFatalError( "IP: IDD Line=" + IPTrimSigDigits( NumLines ) + " Illegal definition for extensible object, should be \"\\extensible:<num>\"", EchoInputFile );
							} else { // process number
								std::string const number_str( UCInputLine.substr( 12 ) );
								NSpace = scan( number_str, " !" );
								if ( NSpace != std::string::npos ) {
									ExtensibleNumFields = int( ProcessNumber( number_str.substr( 0, NSpace ), errFlag ) );
									if ( errFlag ) {
										ShowSevereError( "IP: IDD Line=" + IPTrimSigDigits( NumLines ) + " Illegal Number for \\extensible:<num>", EchoInputFile );
									}
								} else {
									ExtensibleNumFields = 0.0;
									errFlag = false;
								}
							}
						}
						break;
					case 'F':
						if ( has_prefix( UCInputLine, "\\FIELD" ) ) {
							// Capture Field Name
							CurrentFieldName = InputLine.substr( Slash + 6 );
							strip( CurrentFieldName );
							FieldSet = true;
						}
						break;
					case 'M':
						if ( has_prefix( UCInputLine, "\\MIN-FIELDS" ) ) { // Min-Fields arg
							//RequiredField = true;
							NSpace = FindNonSpace( UCInputLine.substr( 11 ) );
							if ( NSpace == std::string::npos ) {
								ShowSevereError( "IP: IDD Line=" + IPTrimSigDigits( NumLines ) + " Need number for \\Min-Fields", EchoInputFile );
								errFlag = true;
								MinimumNumberOfFields = 0;
							} else {
								std::string const number_str( trimmed( UCInputLine.substr( 11 + NSpace ) ) );
								NSpace = scan( number_str, " !" );
								if ( NSpace == std::string::npos ) {
									MinimumNumberOfFields = int( ProcessNumber( number_str, errFlag ) );
								} else {
									MinimumNumberOfFields = int( ProcessNumber( number_str.substr( 0, NSpace ), errFlag ) );
								}
								if ( errFlag ) {
									ShowSevereError( "IP: IDD Line=" + IPTrimSigDigits( NumLines ) + " Illegal Number for \\Min-Fields", EchoInputFile );
								}
							}
						}
						break;
					case 'O':
						if ( has_prefix( UCInputLine, "\\OBSOLETE" ) ) { // Obsolete arg
							NSpace = index( UCInputLine.substr( 9 ), "=>" );
							if ( NSpace == std::string::npos ) {
								ShowSevereError( "IP: IDD Line=" + IPTrimSigDigits( NumLines ) + " Need replacement object for \\Obsolete objects", EchoInputFile );
								errFlag = true;
							} else {
								std::string const name_str( InputLine.substr( Pos + 9 + NSpace + 2 ) );
								ReplacementName = trimmed( name_str );
								ObsoleteObject = true;
							}
						}
						break;
					case 'R':
						if ( has_prefix( UCInputLine, "\\REQUIRED-FIELD" ) ) { // Required-field arg
							RequiredField = true;
						} else if ( has_prefix( UCInputLine, "\\REQUIRED-OBJECT" ) ) { // Required-object arg
							RequiredObject = true;
//						} else if ( has_prefix( UCInputLine, "\\RETAINCASE" ) ) {
//							//RetainCase = true; // Arg not present
						}
						break;
					case 'U':
						if ( has_prefix( UCInputLine, "\\UNIQUE-OBJECT" ) ) { // Unique-object arg
							UniqueObject = true;
						}
						break;
					}
				}
			}
		}

	}

	void
	ReadInputLine(
		std::istream & in_stream,
		std::string::size_type & CurPos,
		bool & BlankLine,
		bool & EndofFile,
		bool & MinMax,
		int & WhichMinMax, // =0 (none/invalid), =1 \min, =2 \min>, =3 \max, =4 \max<
		std::string & MinMaxString,
		Real64 & Value,
		bool & Default,
		std::string & DefString,
		bool & AutoSizable,
		bool & AutoCalculatable,
		bool & RetainCase,
		bool & ErrorsFound
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   September 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine reads a line in the specified file and checks for end of file

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using
		using DataStringGlobals::NL;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		char const SPC( ' ' );
		char const TAB( '\t' );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ReadStat;
		std::string::size_type Pos;
		std::string::size_type Slash;
		std::string::size_type P1;
		std::string::size_type NSpace;
		bool errFlag;
		int ErrLevel;

		errFlag = false;
		{ IOFlags flags; cross_platform_get_line( in_stream, InputLine ); flags.set_status( in_stream ); ReadStat = flags.ios(); }

		if ( ReadStat != 0 ) InputLine.clear();
		std::replace( InputLine.begin(), InputLine.end(), TAB, SPC ); // Replace tabs with spaces

		BlankLine = false;
		CurPos = 0;
		if ( ReadStat < 0 ) {
			EndofFile = true;
		} else {
			if ( EchoInputLine ) {
				++NumLines;
				if ( DisplayInputInAudit ) {
					if ( echo_stream ) *echo_stream << std::setw( 7 ) << NumLines << ' ' << InputLine << NL;
				}
			}
			EchoInputLine = true;
			InputLineLength = static_cast< int >( len_trim( InputLine ) );
			if ( InputLineLength == 0 ) {
				BlankLine = true;
			}
			Pos = ( ProcessingIDD ? InputLine.find_first_of( "!\\" ) : InputLine.find( '!' ) ); // 4/30/09 remove ~
			if ( Pos != std::string::npos ) {
				Slash = ( ProcessingIDD ? InputLine.find( '\\' ) : std::string::npos );
				InputLineLength = static_cast< int >( Pos + 1 );
				if ( Pos > 0 ) {
					if ( is_blank( InputLine.substr( 0, Pos ) ) ) {
						BlankLine = true;
					}
				} else {
					BlankLine = true;
				}
				if ( ( Slash != std::string::npos ) && ( Pos == Slash ) ) {
					std::string UCInputLine( InputLine, Pos );
					uppercase( UCInputLine ); // With this many comparisons probably faster to uppercase once vs many c-i comparisons
					FieldSet = false;
					MinMax = false;
					Default = false;
					AutoSizable = false;
					AutoCalculatable = false;
					switch ( UCInputLine.length() > 1 ? UCInputLine[ 1 ] : ' ' ) { //Performance Switch to reduce expensive string operations
					case 'A':
						if ( has_prefix( UCInputLine, "\\AUTOS" ) ) { // AutoSizable arg
							AutoSizable = true;
							ProcessMinMaxDefLine( UCInputLine, WhichMinMax, MinMaxString, Value, DefString, ErrLevel );
							if ( ErrLevel > 0 ) {
								ShowSevereError( "IP: IDD Line=" + IPTrimSigDigits( NumLines ) + " Error in Autosize designation -- invalid number=" + UCInputLine, EchoInputFile );
								errFlag = true;
							}
						} else if ( has_prefix( UCInputLine, "\\AUTOC" ) ) { // AutoCalculatable arg
							AutoCalculatable = true;
							ProcessMinMaxDefLine( UCInputLine, WhichMinMax, MinMaxString, Value, DefString, ErrLevel );
							if ( ErrLevel > 0 ) {
								ShowSevereError( "IP: IDD Line=" + IPTrimSigDigits( NumLines ) + " Error in Autocalculate designation -- invalid number=" + UCInputLine, EchoInputFile );
								errFlag = true;
							}
						}
						break;
					case 'D':
						if ( has_prefix( UCInputLine, "\\DEFAULT" ) ) { // Default arg
							// WhichMinMax, MinMaxString not filled here
							Default = true;
							ProcessMinMaxDefLine( InputLine.substr( Pos ), WhichMinMax, MinMaxString, Value, DefString, ErrLevel );
							if ( ( ! RetainCase ) && ( ! DefString.empty() ) ) uppercase( DefString );
							if ( ErrLevel > 1 ) {
								ShowContinueError( "Blank Default Field Encountered", EchoInputFile );
								errFlag = true;
							}
						}
						break;
					case 'E':
						if ( has_prefix( UCInputLine, "\\EXTENSIBLE" ) ) { // Extensible arg
							ExtensibleObject = true;
							if ( UCInputLine[ 11 ] != ':' ) {
								ShowFatalError( "IP: IDD Line=" + IPTrimSigDigits( NumLines ) + " Illegal definition for extensible object, should be \"\\extensible:<num>\"", EchoInputFile );
							} else { // process number
								std::string const number_str( UCInputLine.substr( 12 ) );
								NSpace = scan( number_str, " !" );
								if ( NSpace != std::string::npos ) {
									ExtensibleNumFields = int( ProcessNumber( number_str.substr( 0, NSpace ), errFlag ) );
									if ( errFlag ) {
										ShowSevereError( "IP: IDD Line=" + IPTrimSigDigits( NumLines ) + " Illegal Number for \\extensible:<num>", EchoInputFile );
									}
								} else {
									ExtensibleNumFields = 0.0;
									errFlag = false;
								}
							}
						}
						break;
					case 'F':
						if ( has_prefix( UCInputLine, "\\FIELD" ) ) {
							// Capture Field Name
							CurrentFieldName = InputLine.substr( Slash + 6 );
							strip( CurrentFieldName );
							P1 = scan( CurrentFieldName, '!' );
							if ( P1 != std::string::npos ) CurrentFieldName.erase( P1 );
							FieldSet = true;
						}
						break;
					case 'M':
						if ( has_prefix( UCInputLine, "\\MIN-FIELDS" ) ) { // Min-Fields arg
							//RequiredField = true;
							NSpace = FindNonSpace( UCInputLine.substr( 11 ) );
							if ( NSpace == std::string::npos ) {
								ShowSevereError( "IP: IDD Line=" + IPTrimSigDigits( NumLines ) + " Need number for \\Min-Fields", EchoInputFile );
								errFlag = true;
								MinimumNumberOfFields = 0;
							} else {
								std::string const number_str( UCInputLine.substr( 11 + NSpace ) );
								NSpace = scan( number_str, " !" );
								if ( NSpace == std::string::npos ) {
									MinimumNumberOfFields = int( ProcessNumber( number_str, errFlag ) );
								} else {
									MinimumNumberOfFields = int( ProcessNumber( number_str.substr( 0, NSpace ), errFlag ) );
								}
								if ( errFlag ) {
									ShowSevereError( "IP: IDD Line=" + IPTrimSigDigits( NumLines ) + " Illegal Number for \\Min-Fields", EchoInputFile );
								}
							}
						} else if ( has_prefix( UCInputLine, "\\MINIMUM" ) || has_prefix( UCInputLine, "\\MAXIMUM" ) ) { // Min/Max args
							MinMax = true;
							ProcessMinMaxDefLine( UCInputLine, WhichMinMax, MinMaxString, Value, DefString, ErrLevel );
							if ( ErrLevel > 0 ) {
								ShowSevereError( "IP: IDD Line=" + IPTrimSigDigits( NumLines ) + " Error in Minimum/Maximum designation -- invalid number=" + UCInputLine, EchoInputFile );
								errFlag = true;
							}
						}
						break;
					case 'O':
						if ( has_prefix( UCInputLine, "\\OBSOLETE" ) ) { // Obsolete arg
							NSpace = index( UCInputLine.substr( 9 ), "=>" );
							if ( NSpace == std::string::npos ) {
								ShowSevereError( "IP: IDD Line=" + IPTrimSigDigits( NumLines ) + " Need replacement object for \\Obsolete objects", EchoInputFile );
								errFlag = true;
							} else {
								std::string const name_str( InputLine.substr( Pos + 9 + NSpace + 2 ) );
								NSpace = scan( name_str, '!' );
								if ( NSpace == std::string::npos ) {
									ReplacementName = trimmed( name_str );
								} else {
									ReplacementName = trimmed( name_str.substr( 0, NSpace ) );
								}
								ObsoleteObject = true;
							}
						}
						break;
					case 'R':
						if ( has_prefix( UCInputLine, "\\REQUIRED-FIELD" ) ) { // Required-field arg
							RequiredField = true;
						} else if ( has_prefix( UCInputLine, "\\REQUIRED-OBJECT" ) ) { // Required-object arg
							RequiredObject = true;
						} else if ( has_prefix( UCInputLine, "\\RETAINCASE" ) ) {
							RetainCase = true;
						}
						break;
					case 'U':
						if ( has_prefix( UCInputLine, "\\UNIQUE-OBJECT" ) ) { // Unique-object arg
							UniqueObject = true;
						}
						break;
					}
				}
			}
		}
		if ( errFlag ) {
			ErrorsFound = true;
		}

	}

	void
	ExtendObjectDefinition(
		int const ObjectNum, // Number of the object definition to be extended.
		int & NumNewArgsLimit // Number of the parameters after extension
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   Sep 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine expands the object definition according to the extensible "rules" entered
		// by the developer.  The developer should enter the number of fields to be duplicated.
		// See References section for examples.

		// METHODOLOGY EMPLOYED:
		// The routine determines the type of the fields to be added (A or N) and reallocates the
		// appropriate arrays in the object definition structure.

		// REFERENCES:
		// Extensible objects have a \extensible:<num> specification
		// \extensible:3 -- the last 3 fields are "extended"
		// Works on this part of the definition:
		//   INTEGER :: NumParams                       =0   ! Number of parameters to be processed for each object
		//   INTEGER :: NumAlpha                        =0   ! Number of Alpha elements in the object
		//   INTEGER :: NumNumeric                      =0   ! Number of Numeric elements in the object
		//   LOGICAL(1), ALLOCATABLE, DIMENSION(:) :: AlphaOrNumeric ! Positionally, whether the argument
		//                                                           ! is alpha (true) or numeric (false)
		//   LOGICAL(1), ALLOCATABLE, DIMENSION(:) :: ReqField ! True for required fields
		//   LOGICAL(1), ALLOCATABLE, DIMENSION(:) :: AlphRetainCase ! true if retaincase is set for this field (alpha fields only)
		//   CHARACTER(len=MaxNameLength+40),  &
		//               ALLOCATABLE, DIMENSION(:) :: AlphFieldChks ! Field names for alphas
		//   CHARACTER(len=MaxNameLength),  &
		//               ALLOCATABLE, DIMENSION(:) :: AlphFieldDefs ! Defaults for alphas
		//   TYPE(RangeCheckDef), ALLOCATABLE, DIMENSION(:) :: NumRangeChks  ! Used to range check and default numeric fields
		//   INTEGER :: LastExtendAlpha                 =0   ! Count for extended alpha fields
		//   INTEGER :: LastExtendNum                   =0   ! Count for extended numeric fields

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const NewAlloc( 1000 ); // number of new items to allocate (* number of fields)

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumAlphaField;
		int NumNumericField;
		int NumNewAlphas;
		int NumNewNumerics;
		int NumNewParams;
		int NumExtendFields;
		int NumParams;
		int Count;
		//  LOGICAL :: MaxArgsChanged
		std::string charout;
		static std::string CurObject;

		gio::write( EchoInputFile, fmtA ) << "Attempting to auto-extend object=" + ObjectDef( ObjectNum ).Name;
		if ( CurObject != ObjectDef( ObjectNum ).Name ) {
			DisplayString( "Auto-extending object=\"" + ObjectDef( ObjectNum ).Name + "\", input processing may be slow." );
			CurObject = ObjectDef( ObjectNum ).Name;
		}

		NumAlphaField = 0;
		NumNumericField = 0;
		NumParams = ObjectDef( ObjectNum ).NumParams;
		Count = NumParams - ObjectDef( ObjectNum ).ExtensibleNum + 1;
		//  MaxArgsChanged=.FALSE.

		Array1D_bool AorN( ObjectDef( ObjectNum ).ExtensibleNum, false );
		for ( int Loop = Count, Item = 1; Loop <= NumParams; ++Loop, ++Item ) {
			bool const AON_Loop( ObjectDef( ObjectNum ).AlphaOrNumeric( Loop ) );
			if ( AON_Loop ) {
				++NumAlphaField;
			} else {
				++NumNumericField;
			}
			AorN( Item ) = AON_Loop;
		}
		NumNewAlphas = NumAlphaField * NewAlloc;
		NumNewNumerics = NumNumericField * NewAlloc;
		NumNewParams = NumParams + NumNewAlphas + NumNewNumerics;
		NumExtendFields = NumAlphaField + NumNumericField;
		ObjectDef( ObjectNum ).AlphaOrNumeric.redimension( NumNewParams, false );
		for ( int Loop = NumParams + 1; Loop <= NumNewParams; Loop += NumExtendFields ) {
			ObjectDef( ObjectNum ).AlphaOrNumeric( {Loop,Loop+NumExtendFields-1} ) = AorN;
		}
		AorN.deallocate(); // done with this object AorN array.

		// required fields -- can't be extended and required.
		ObjectDef( ObjectNum ).ReqField.redimension( NumNewParams, false );

		ObjectDef( ObjectNum ).AlphRetainCase.redimension( NumNewParams, false );

		if ( NumAlphaField > 0 ) {
			ObjectDef( ObjectNum ).AlphFieldChks.redimension( ObjectDef( ObjectNum ).NumAlpha + NumNewAlphas );
			for ( int Loop = ObjectDef( ObjectNum ).NumAlpha + 1, Loop_end = ObjectDef( ObjectNum ).NumAlpha + NumNewAlphas; Loop <= Loop_end; ++Loop ) {
				++ObjectDef( ObjectNum ).LastExtendAlpha;
				charout = IPTrimSigDigits( ObjectDef( ObjectNum ).LastExtendAlpha );
				ObjectDef( ObjectNum ).AlphFieldChks( Loop ) = "Extended Alpha Field " + charout;
			}

			ObjectDef( ObjectNum ).AlphFieldDefs.redimension( ObjectDef( ObjectNum ).NumAlpha + NumNewAlphas );

			if ( ObjectDef( ObjectNum ).NumAlpha + NumNewAlphas > MaxAlphaArgsFound ) {
				// must redimension LineItem args
				LineItem.Alphas.redimension( ObjectDef( ObjectNum ).NumAlpha + NumNewAlphas );

				LineItem.AlphBlank.redimension( ObjectDef( ObjectNum ).NumAlpha + NumNewAlphas, true );

				MaxAlphaArgsFound = ObjectDef( ObjectNum ).NumAlpha + NumNewAlphas;
				//      MaxArgsChanged=.TRUE.
			}

		}

		if ( NumNumericField > 0 ) {
			ObjectDef( ObjectNum ).NumRangeChks.redimension( ObjectDef( ObjectNum ).NumNumeric + NumNewNumerics );
			for ( int Loop = ObjectDef( ObjectNum ).NumNumeric + 1, Loop_end = ObjectDef( ObjectNum ).NumNumeric + NumNewNumerics; Loop <= Loop_end; ++Loop ) {
				ObjectDef( ObjectNum ).NumRangeChks( Loop ).FieldNumber = Loop;
				++ObjectDef( ObjectNum ).LastExtendNum;
				charout = IPTrimSigDigits( ObjectDef( ObjectNum ).LastExtendNum );
				ObjectDef( ObjectNum ).NumRangeChks( Loop ).FieldName = "Extended Numeric Field " + charout;
			}

			if ( ObjectDef( ObjectNum ).NumNumeric + NumNewNumerics > MaxNumericArgsFound ) {
				// must redimension LineItem args
				LineItem.Numbers.redimension( ObjectDef( ObjectNum ).NumNumeric + NumNewNumerics, 0.0 );

				LineItem.NumBlank.redimension( ObjectDef( ObjectNum ).NumNumeric + NumNewNumerics, true );

				MaxNumericArgsFound = ObjectDef( ObjectNum ).NumNumeric + NumNewNumerics;
				//      MaxArgsChanged=.TRUE.
			}

		}

		ObjectDef( ObjectNum ).NumParams = NumNewParams;
		NumNewArgsLimit = NumNewParams;
		ObjectDef( ObjectNum ).NumAlpha += NumNewAlphas;
		ObjectDef( ObjectNum ).NumNumeric += NumNewNumerics;

	}

	Real64
	ProcessNumber(
		std::string const & String,
		bool & ErrorFlag
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   September 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function processes a string that should be numeric and
		// returns the real value of the string.

		// METHODOLOGY EMPLOYED:
		// FUNCTION ProcessNumber translates the argument (a string)
		// into a real number.  The string should consist of all
		// numeric characters (except a decimal point).  Numerics
		// with exponentiation (i.e. 1.2345E+03) are allowed but if
		// it is not a valid number an error message along with the
		// string causing the error is printed out and 0.0 is returned
		// as the value.

		// REFERENCES:
		// List directed Fortran input/output.

		// USE STATEMENTS:
		// na

		// Return value

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const ValidNumerics( "0123456789.+-EeDd" ); // This had a trailing tab character: Not sure why

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:


		Real64 rProcessNumber = 0.0;
		//  Make sure the string has all what we think numerics should have
		std::string const PString( stripped( String ) );
		std::string::size_type const StringLen( PString.length() );
		ErrorFlag = false;
		if ( StringLen == 0 ) return rProcessNumber;
		int IoStatus( 0 );
		if ( PString.find_first_not_of( ValidNumerics ) == std::string::npos ) {
			{ IOFlags flags; gio::read( PString, fmtLD, flags ) >> rProcessNumber; IoStatus = flags.ios(); }
			ErrorFlag = false;
		} else {
			rProcessNumber = 0.0;
			ErrorFlag = true;
		}
		if ( IoStatus != 0 ) {
			rProcessNumber = 0.0;
			ErrorFlag = true;
		}

		return rProcessNumber;

	}

	void
	ProcessMinMaxDefLine(
		std::string const & partLine, // part of input line starting \min or \max  Not uppercase if \default
		int & WhichMinMax, // =0 (none/invalid), =1 \min, =2 \min>, =3 \max, =4 \max<
		std::string & MinMaxString,
		Real64 & Value,
		std::string & DefaultString,
		int & ErrLevel
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   July 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine processes the IDD lines that start with
		// \minimum or \maximum and set up the parameters so that it can
		// be automatically checked.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// IDD Statements.
		//  \minimum         Minimum that includes the following value
		//  i.e. min >=
		//  \minimum>        Minimum that must be > than the following value
		//  \maximum         Maximum that includes the following value
		//  i.e. max <=
		//  \maximum<        Maximum that must be < than the following value
		//  \default         Default for field (when field is blank)

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		ErrLevel = 0;
		std::string::size_type Pos = scan( partLine, ' ' );

		{ auto const maxMinDefLine( uppercased( partLine.substr( 0, 4 ) ) );

		if ( maxMinDefLine == "\\MIN" ) {
			WhichMinMax = 1;
			if ( has( partLine, '>' ) ) {
				Pos = scan( partLine, '>' ) + 1;
				WhichMinMax = 2;
			}
			if ( WhichMinMax == 1 ) {
				MinMaxString = ">=";
			} else {
				MinMaxString = ">";
			}

		} else if ( maxMinDefLine == "\\MAX" ) {
			WhichMinMax = 3;
			if ( has( partLine, '<' ) ) {
				Pos = scan( partLine, '<' ) + 1;
				WhichMinMax = 4;
			}
			if ( WhichMinMax == 3 ) {
				MinMaxString = "<=";
			} else {
				MinMaxString = "<";
			}

		} else if ( maxMinDefLine == "\\DEF" ) {
			WhichMinMax = 5;
			MinMaxString = BlankString;

		} else if ( maxMinDefLine == "\\AUT" ) {
			WhichMinMax = 6;
			MinMaxString = BlankString;

		} else {
			WhichMinMax = 0; // invalid field
			MinMaxString = BlankString;
			Value = -999999.0;

		}}

		if ( WhichMinMax != 0 ) {
			if ( Pos == std::string::npos ) Pos = partLine.length(); // So that NSpace=npos
			std::string::size_type NSpace = FindNonSpace( partLine.substr( Pos ) );
			if ( NSpace == std::string::npos ) {
				if ( WhichMinMax != 6 ) { // Only autosize/autocalculate can't have argument
					ShowSevereError( "IP: IDD Line=" + IPTrimSigDigits( NumLines ) + "Min/Max/Default field cannot be blank -- must have value", EchoInputFile );
					ErrLevel = 2;
				} else if ( has_prefix( partLine, "\\AUTOS" ) ) {
					Value = DefAutoSizeValue;
				} else if ( has_prefix( partLine, "\\AUTOC" ) ) {
					Value = DefAutoCalculateValue;
				}
			} else {
				Pos += NSpace;
				NSpace = scan( partLine.substr( Pos ), " !" );
				MinMaxString += partLine.substr( Pos, NSpace );
				bool errFlag;
				Value = ProcessNumber( partLine.substr( Pos, NSpace ), errFlag );
				if ( errFlag ) ErrLevel = 1;
				NSpace = scan( partLine.substr( Pos ), '!' );
				if ( NSpace != std::string::npos ) {
					DefaultString = partLine.substr( Pos, NSpace - 1 );
				} else {
					DefaultString = partLine.substr( Pos );
				}
				strip( DefaultString );
				if ( DefaultString == BlankString ) {
					if ( WhichMinMax == 6 ) {
						if ( has_prefix( partLine, "\\AUTOS" ) ) {
							Value = DefAutoSizeValue;
						} else {
							Value = DefAutoCalculateValue;
						}
					} else {
						ShowSevereError( "IP: IDD Line=" + IPTrimSigDigits( NumLines ) + "Min/Max/Default field cannot be blank -- must have value", EchoInputFile );
						ErrLevel = 2;
					}
				}
			}
		}

	}

	int
	FindItemInList(
		std::string const & String,
		Array1_string const & ListOfItems,
		int const NumItems
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   September 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up a string in a similar list of
		// items and returns the index of the item in the list, if
		// found.  This routine is not case insensitive and doesn't need
		// for most inputs -- they are automatically turned to UPPERCASE.
		// If you need case insensitivity use FindItem.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		for ( int Count = 1; Count <= NumItems; ++Count ) {
			if ( String == ListOfItems( Count ) ) return Count;
		}
		return 0; // Not found
	}

	int
	FindItemInList(
		std::string const & String,
		Array1S_string const ListOfItems,
		int const NumItems
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   September 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up a string in a similar list of
		// items and returns the index of the item in the list, if
		// found.  This routine is not case insensitive and doesn't need
		// for most inputs -- they are automatically turned to UPPERCASE.
		// If you need case insensitivity use FindItem.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		for ( int Count = 1; Count <= NumItems; ++Count ) {
			if ( String == ListOfItems( Count ) ) return Count;
		}
		return 0; // Not found
	}

	int
	FindItemInSortedList(
		std::string const & String,
		Array1S_string const ListOfItems,
		int const NumItems
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   September 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up a string in a similar list of
		// items and returns the index of the item in the list, if
		// found.  This routine is case insensitive.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int Probe( 0 );
		int LBnd( 0 );
		int UBnd( NumItems + 1 );
		bool Found( false );
		while ( ( ! Found ) || ( Probe != 0 ) ) {
			Probe = ( UBnd - LBnd ) / 2;
			if ( Probe == 0 ) break;
			Probe += LBnd;
			if ( equali( String, ListOfItems( Probe ) ) ) {
				Found = true;
				break;
			} else if ( lessthani( String, ListOfItems( Probe ) ) ) {
				UBnd = Probe;
			} else {
				LBnd = Probe;
			}
		}
		return Probe;
	}

	int
	FindItem(
		std::string const & String,
		Array1D_string const & ListOfItems,
		int const NumItems
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   April 1999
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up a string in a similar list of
		// items and returns the index of the item in the list, if
		// found.  This routine is case insensitive.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int FindItem = FindItemInList( String, ListOfItems, NumItems );
		if ( FindItem != 0 ) return FindItem;

		for ( int Count = 1; Count <= NumItems; ++Count ) {
			if ( equali( String, ListOfItems( Count ) ) ) return Count;
		}
		return 0; // Not found
	}

	int
	FindItem(
		std::string const & String,
		Array1S_string const ListOfItems,
		int const NumItems
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   April 1999
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up a string in a similar list of
		// items and returns the index of the item in the list, if
		// found.  This routine is case insensitive.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int FindItem = FindItemInList( String, ListOfItems, NumItems );
		if ( FindItem != 0 ) return FindItem;

		for ( int Count = 1; Count <= NumItems; ++Count ) {
			if ( equali( String, ListOfItems( Count ) ) ) return Count;
		}
		return 0; // Not found
	}

	std::string
	MakeUPPERCase( std::string const & InputString )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   September 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This function returns the Upper Case representation of the InputString.

		// METHODOLOGY EMPLOYED:
		// Uses the Intrinsic SCAN function to scan the lowercase representation of
		// characters (DataStringGlobals) for each character in the given string.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:
		// MaxInputLineLength because of PowerStation Compiler
		// otherwise could say (CHARACTER(len=LEN(InputString))

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:

		std::string ResultString( InputString );

		for ( std::string::size_type i = 0, e = len( InputString ); i < e; ++i ) {
			int const curCharVal = int( InputString[ i ] );
			if ( ( 97 <= curCharVal && curCharVal <= 122 ) || ( 224 <= curCharVal && curCharVal <= 255 ) ) { // lowercase ASCII and accented characters
				ResultString[ i ] = char( curCharVal - 32 );
			}
		}

		return ResultString;

	}

	void
	VerifyName(
		std::string const & NameToVerify,
		Array1D_string const & NamesList,
		int const NumOfNames,
		bool & ErrorFound,
		bool & IsBlank,
		std::string const & StringToDisplay
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   February 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine verifys that a new name can be added to the
		// list of names for this item (i.e., that there isn't one of that
		// name already and that this name is not blank).

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Found;

		ErrorFound = false;
		if ( NumOfNames > 0 ) {
			Found = FindItem( NameToVerify, NamesList, NumOfNames );
			if ( Found != 0 ) {
				ShowSevereError( StringToDisplay + ", duplicate name=" + NameToVerify );
				ErrorFound = true;
			}
		}

		if ( NameToVerify.empty() ) {
			ShowSevereError( StringToDisplay + ", cannot be blank" );
			ErrorFound = true;
			IsBlank = true;
		} else {
			IsBlank = false;
		}

	}

	void
	VerifyName(
		std::string const & NameToVerify,
		Array1S_string const NamesList,
		int const NumOfNames,
		bool & ErrorFound,
		bool & IsBlank,
		std::string const & StringToDisplay
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   February 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine verifys that a new name can be added to the
		// list of names for this item (i.e., that there isn't one of that
		// name already and that this name is not blank).

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Found;

		ErrorFound = false;
		if ( NumOfNames > 0 ) {
			Found = FindItem( NameToVerify, NamesList, NumOfNames );
			if ( Found != 0 ) {
				ShowSevereError( StringToDisplay + ", duplicate name=" + NameToVerify );
				ErrorFound = true;
			}
		}

		if ( NameToVerify.empty() ) {
			ShowSevereError( StringToDisplay + ", cannot be blank" );
			ErrorFound = true;
			IsBlank = true;
		} else {
			IsBlank = false;
		}

	}

	void
	RangeCheck(
		bool & ErrorsFound, // Set to true if error detected
		std::string const & WhatFieldString, // Descriptive field for string
		std::string const & WhatObjectString, // Descriptive field for object, Zone Name, etc.
		std::string const & ErrorLevel, // 'Warning','Severe','Fatal')
		Optional_string_const LowerBoundString, // String for error message, if applicable
		Optional_bool_const LowerBoundCondition, // Condition for error condition, if applicable
		Optional_string_const UpperBoundString, // String for error message, if applicable
		Optional_bool_const UpperBoundCondition, // Condition for error condition, if applicable
		Optional_string_const ValueString, // Value with digits if to be displayed with error
		Optional_string_const WhatObjectName // ObjectName -- used for error messages
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   July 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is a general purpose "range check" routine for GetInput routines.
		// Using the standard "ErrorsFound" logical, this routine can produce a reasonable
		// error message to describe the situation in addition to setting the ErrorsFound variable
		// to true.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		std::string ErrorString; // Uppercase representation of ErrorLevel
		std::string Message1;
		std::string Message2;

		bool Error = false;
		if ( present( UpperBoundCondition ) ) {
			if ( ! UpperBoundCondition ) Error = true;
		}
		if ( present( LowerBoundCondition ) ) {
			if ( ! LowerBoundCondition ) Error = true;
		}

		if ( Error ) {
			ConvertCaseToUpper( ErrorLevel, ErrorString );
			Message1 = WhatObjectString;
			if ( present( WhatObjectName ) ) Message1 += "=\"" + WhatObjectName + "\", out of range data";
			Message2 = "Out of range value field=" + WhatFieldString;
			if ( present( ValueString ) ) Message2 += ", Value=[" + ValueString + ']';
			Message2 += ", range={";
			if ( present( LowerBoundString ) ) Message2 += LowerBoundString;
			if ( present( LowerBoundString ) && present( UpperBoundString ) ) {
				Message2 += " and " + UpperBoundString;
			} else if ( present( UpperBoundString ) ) {
				Message2 += UpperBoundString;
			}
			Message2 += "}";

			{ auto const errorCheck( ErrorString[ 0 ] );

			if ( ( errorCheck == 'W' ) || ( errorCheck == 'w' ) ) {
				ShowWarningError( Message1 );
				ShowContinueError( Message2 );

			} else if ( ( errorCheck == 'S' ) || ( errorCheck == 's' ) ) {
				ShowSevereError( Message1 );
				ShowContinueError( Message2 );
				ErrorsFound = true;

			} else if ( ( errorCheck == 'F' ) || ( errorCheck == 'f' ) ) {
				ShowSevereError( Message1 );
				ShowContinueError( Message2 );
				ShowFatalError( "Program terminates due to preceding condition(s)." );

			} else {
				ShowSevereError( Message1 );
				ShowContinueError( Message2 );
				ErrorsFound = true;

			}}

		}

	}

	void
	InternalRangeCheck(
		Real64 const Value,
		int const FieldNumber,
		int const WhichObject,
		std::string const & PossibleAlpha,
		bool const AutoSizable,
		bool const AutoCalculatable
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   July 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is an internal range check that checks fields which have
		// the \min and/or \max values set for appropriate values.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		std::string FieldString;
		std::string FieldNameString;
		std::string ValueString;
		std::string Message;

		bool Error = false;
		if ( ObjectDef( WhichObject ).NumRangeChks( FieldNumber ).WhichMinMax( 1 ) == 1 ) {
			if ( Value < ObjectDef( WhichObject ).NumRangeChks( FieldNumber ).MinMaxValue( 1 ) ) Error = true;
		} else if ( ObjectDef( WhichObject ).NumRangeChks( FieldNumber ).WhichMinMax( 1 ) == 2 ) {
			if ( Value <= ObjectDef( WhichObject ).NumRangeChks( FieldNumber ).MinMaxValue( 1 ) ) Error = true;
		}
		if ( ObjectDef( WhichObject ).NumRangeChks( FieldNumber ).WhichMinMax( 2 ) == 3 ) {
			if ( Value > ObjectDef( WhichObject ).NumRangeChks( FieldNumber ).MinMaxValue( 2 ) ) Error = true;
		} else if ( ObjectDef( WhichObject ).NumRangeChks( FieldNumber ).WhichMinMax( 2 ) == 4 ) {
			if ( Value >= ObjectDef( WhichObject ).NumRangeChks( FieldNumber ).MinMaxValue( 2 ) ) Error = true;
		}

		if ( Error ) {
			if ( ! ( AutoSizable && Value == ObjectDef( WhichObject ).NumRangeChks( FieldNumber ).AutoSizeValue ) && ! ( AutoCalculatable && Value == ObjectDef( WhichObject ).NumRangeChks( FieldNumber ).AutoCalculateValue ) ) {
				++NumOutOfRangeErrorsFound;
				if ( ReportRangeCheckErrors ) {
					FieldString = IPTrimSigDigits( FieldNumber );
					FieldNameString = ObjectDef( WhichObject ).NumRangeChks( FieldNumber ).FieldName;
					gio::write( ValueString, "(F20.5)" ) << Value;
					strip( ValueString );
					if ( ! FieldNameString.empty() ) {
						Message = "Out of range value Numeric Field#" + FieldString + " (" + FieldNameString + "), value=" + ValueString + ", range={";
					} else { // Field Name not recorded
						Message = "Out of range value Numeric Field#" + FieldString + ", value=" + ValueString + ", range={";
>>>>>>> NREL/develop
					}
				} else {
					if ( it.value().is_number_integer() ) {
						i64toa( it.value().get < int >(), s );
					} else {
						dtoa( it.value().get < double >(), s );
					}
					Alphas( i + 1 ) = s;
					if ( is_AlphaBlank ) AlphaBlank()( i + 1 ) = false;
				}
				NumAlphas++;
			} else {
				Alphas( i + 1 ) = "";
				if ( is_AlphaBlank ) AlphaBlank()( i + 1 ) = true;
			}
			if ( is_AlphaFieldNames ) AlphaFieldNames()( i + 1 ) = field;
		}

		auto const & legacy_idd_alphas_extension_iter = legacy_idd_alphas.find( "extensions" );
		if ( legacy_idd_alphas_extension_iter != legacy_idd_alphas.end() ) {
			auto const & legacy_idd_alphas_extensions = legacy_idd_alphas_extension_iter.value();
			auto const & jdf_extensions_array = obj.value()[ extension_key];
			auto const & schema_extension_fields = schema_obj_props[ extension_key ][ "items" ][ "properties" ];
			int alphas_index = static_cast <int> ( legacy_idd_alphas_fields.size() );

			for ( auto it = jdf_extensions_array.begin(); it != jdf_extensions_array.end(); ++it ) {
				auto const & jdf_extension_obj = it.value();

				for ( size_t i = 0; i < legacy_idd_alphas_extensions.size(); i++ ) {
					std::string const & field_name = legacy_idd_alphas_extensions[ i ];
					auto const & jdf_obj_field_iter = jdf_extension_obj.find( field_name );

					if ( jdf_obj_field_iter != jdf_extension_obj.end() ) {
						auto const & jdf_field_val = jdf_obj_field_iter.value();
						if ( jdf_field_val.is_string() ) {
							auto const & schema_field = schema_extension_fields[ field_name ];
							std::string val = jdf_field_val;
							auto const & tmp_find_default_iter = schema_field.find( "default" );
							if ( val.empty() and tmp_find_default_iter != schema_field.end() ) {
								auto const & field_default_val = tmp_find_default_iter.value();
								if ( field_default_val.is_string() ) {
									val = field_default_val.get < std::string >();
								} else {
									if ( field_default_val.is_number_integer() ) {
										i64toa( field_default_val.get < int >(), s );
									} else {
										dtoa( field_default_val.get < double >(), s );
									}
									val = s;
								}
								if ( is_AlphaBlank ) AlphaBlank()( alphas_index + 1 ) = true;
							} else {
								if ( is_AlphaBlank ) AlphaBlank()( alphas_index + 1 ) = val.empty();
							}
							if ( schema_field.find("retaincase") != schema_field.end() ) {
								Alphas( alphas_index + 1 ) = val;
							} else {
								Alphas( alphas_index + 1 ) = MakeUPPERCase( val );
							}
						} else {
							if ( jdf_field_val.is_number_integer() ) {
								i64toa( jdf_field_val.get < int >(), s );
							} else {
								dtoa( jdf_field_val.get < double >(), s );
							}
							Alphas( alphas_index + 1 ) = s;
							if ( is_AlphaBlank ) AlphaBlank()( alphas_index + 1 ) = false;
						}
						NumAlphas++;
					} else {
						Alphas( alphas_index + 1 ) = "";
						if ( is_AlphaBlank ) AlphaBlank()( alphas_index + 1 ) = true;
					}
					if ( is_AlphaFieldNames ) AlphaFieldNames()( alphas_index + 1 ) = field_name;
					alphas_index++;
				}
			}
		}

		auto const & legacy_idd_numerics_fields = legacy_idd_numerics[ "fields" ];
		for ( size_t i = 0; i < legacy_idd_numerics_fields.size(); ++i ) {
			std::string const & field = legacy_idd_numerics_fields[ i ];
			auto it = obj.value().find( field );
			if ( it != obj.value().end() ) {
				if ( !it.value().is_string() ) {
					Numbers( i + 1 ) = it.value().get < double >();
					if ( is_NumBlank ) NumBlank()( i + 1 ) = false;
				} else {
					if ( it.value().get < std::string >().empty() ) {
						auto const & schema_obj = schema_obj_props[ field ];
						auto const & schema_default_iter = schema_obj.find( "default" );
						if ( schema_default_iter != schema_obj.end() ) {
							auto const & schema_default_val = schema_default_iter.value();
							if ( schema_default_val.is_string() ) Numbers( i + 1 ) = -99999;
							else Numbers( i + 1 ) = schema_default_val.get < double >();
						} else {
							Numbers( i + 1 ) = 0;
						}
					} else {
						Numbers( i + 1 ) = -99999; // autosize and autocalculate
					}
					if ( is_NumBlank ) NumBlank()( i + 1 ) = it.value().get < std::string >().empty();
				}
				NumNumbers++;
			} else {
				Numbers( i + 1 ) = 0;
				if ( is_NumBlank )
					NumBlank()( i + 1 ) = true;
			}
			if ( is_NumericFieldNames ) NumericFieldNames()( i + 1 ) = field;
		}

		auto const & legacy_idd_numerics_extension_iter = legacy_idd_numerics.find( "extensions" );
		if ( legacy_idd_numerics_extension_iter != legacy_idd_numerics.end() ) {
			auto const & legacy_idd_numerics_extensions = legacy_idd_numerics_extension_iter.value();
			auto const & schema_extension_fields = schema_obj_props[ extension_key ][ "items" ][ "properties" ];
			auto const & jdf_extensions_array = obj.value()[ extension_key ];
			int numerics_index = static_cast <int> ( legacy_idd_numerics_fields.size() );

			for ( auto it = jdf_extensions_array.begin(); it != jdf_extensions_array.end(); ++it ) {
				auto const & jdf_extension_obj = it.value();

				for ( size_t i = 0; i < legacy_idd_numerics_extensions.size(); i++ ) {
					std::string const & field = legacy_idd_numerics_extensions[ i ];
					auto const & jdf_extension_field_iter = jdf_extension_obj.find( field );

					if ( jdf_extension_field_iter != jdf_extension_obj.end() ) {
						auto const & val = jdf_extension_field_iter.value();
						if ( !val.is_string() ) {
							if ( val.is_number_integer() ) {
								Numbers( numerics_index + 1 ) = val.get < int >();
							} else {
								Numbers( numerics_index + 1 ) = val.get < double >();
							}
							if ( is_NumBlank ) NumBlank()( numerics_index + 1 ) = false;
						} else {
							if ( val.get < std::string >().empty() ) {
								auto const & schema_obj = schema_extension_fields[ field ];
								auto const & default_iter = schema_obj.find( "default" );
								if ( default_iter != schema_obj.end() ) {
									auto const & default_val = default_iter.value();
									if ( default_val.is_string() ) {
										Numbers( numerics_index + 1 ) = -99999;
									} else if ( default_val.is_number_integer() ) {
										Numbers( numerics_index + 1 ) = default_val.get < int >();
									} else {
										Numbers( numerics_index + 1 ) = default_val.get < double >();
									}
								} else {
									Numbers( numerics_index + 1 ) = 0;
								}
							} else { // autosize and autocalculate
								Numbers( numerics_index + 1 ) = -99999;
							}
							if ( is_NumBlank ) NumBlank()( numerics_index + 1 ) = val.get < std::string >().empty();
						}
						NumNumbers++;
					} else {
						auto const & schema_field = schema_extension_fields[ field ];
						auto const & schema_field_default_iter = schema_field.find( "default" );
						if ( schema_field_default_iter != schema_field.end() ) {
							auto const & schema_field_default_val = schema_field_default_iter.value();
							if ( schema_field_default_val.is_number_integer() ) {
								Numbers( numerics_index + 1 ) = schema_field_default_val.get < int >();
							} else if ( schema_field_default_val.is_number_float() ) {
								Numbers( numerics_index + 1 ) = schema_field_default_val.get < double >();
							} else {
								if ( it.value().get < std::string >().empty() ) {
									Numbers( numerics_index + 1 ) = 0;
								} else {
									Numbers( numerics_index + 1 ) = -99999; // autosize and autocalculate
								}
							}
						} else {
							Numbers( numerics_index + 1 ) = 0;
						}
						if ( is_NumBlank ) NumBlank()( numerics_index + 1 ) = true;
					}
					if ( is_NumericFieldNames ) NumericFieldNames()( numerics_index + 1 ) = field;
					numerics_index++;
				}
			}
		}

		Status = 1;
	}

	int
	InputProcessor::GetObjectItemNum(
	std::string const & ObjType, // Object Type (ref: IDD Objects)
	std::string const & ObjName // Name of the object type
	) {
		// PURPOSE OF THIS SUBROUTINE:
		// Get the occurrence number of an object of type ObjType and name ObjName

		json * obj;
		auto obj_iter = jdf.find( ObjType );
		if ( obj_iter == jdf.end() || obj_iter.value().find( ObjName ) == obj_iter.value().end() ) {
			auto tmp_umit = case_insensitive_object_map.find( MakeUPPERCase( ObjType ) );
			if ( tmp_umit == case_insensitive_object_map.end() ) {
				return -1;
			}
			obj = &jdf[ tmp_umit->second ];
		} else {
			obj = &( obj_iter.value() );
		}

		int object_item_num = 1;
		bool found = false;
		auto const upperObjName = MakeUPPERCase( ObjName );
		for ( auto it = obj->begin(); it != obj->end(); ++it ) {
			if ( MakeUPPERCase( it.key() ) == upperObjName ) {
				found = true;
				break;
			}
			object_item_num++;
		}

		if ( !found ) {
			// ShowWarningError("Didn't find name, need to probably use search key");
			return -1;
		}
		return object_item_num;
		}


	int
	InputProcessor::GetObjectItemNum(
			std::string const & ObjType, // Object Type (ref: IDD Objects)
			std::string const & NameTypeVal, // Object "name" field type ( used as search key )
			std::string const & ObjName // Name of the object type
	) {
		// PURPOSE OF THIS SUBROUTINE:
		// Get the occurrence number of an object of type ObjType and name ObjName

		json * obj;
		auto obj_iter = jdf.find( ObjType );
		if ( jdf.find( ObjType ) == jdf.end() || obj_iter.value().find( ObjName ) == obj_iter.value().end() ) {
			auto tmp_umit = case_insensitive_object_map.find( MakeUPPERCase( ObjType ) );
			if ( tmp_umit == case_insensitive_object_map.end() ) {
				return -1;
			}
			obj = &jdf[ tmp_umit->second ];
		} else {
			obj = &( obj_iter.value() );
		}

		int object_item_num = 1;
		bool found = false;
		auto const upperObjName = MakeUPPERCase( ObjName );
		for ( auto it = obj->begin(); it != obj->end(); ++it ) {
			auto it2 = it.value().find(NameTypeVal);

			if ( ( it2 != it.value().end() ) && ( MakeUPPERCase( it2.value() ) == upperObjName ) ) {
				found = true;
				break;
			}
			object_item_num++;
		}


		if ( !found ) {
			// ShowWarningError("Didn't find name, need to probably use search key");
			return -1;
		}
		return object_item_num;
	}

	Real64
	InputProcessor::ProcessNumber(
		std::string const & String,
		bool & ErrorFlag
	) {

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   September 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function processes a string that should be numeric and
		// returns the real value of the string.

		// METHODOLOGY EMPLOYED:
		// FUNCTION ProcessNumber translates the argument (a string)
		// into a real number.  The string should consist of all
		// numeric characters (except a decimal point).  Numerics
		// with exponentiation (i.e. 1.2345E+03) are allowed but if
		// it is not a valid number an error message along with the
		// string causing the error is printed out and 0.0 is returned
		// as the value.

		// REFERENCES:
		// List directed Fortran input/output.

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const ValidNumerics( "0123456789.+-EeDd" );

		Real64 rProcessNumber = 0.0;
		//  Make sure the string has all what we think numerics should have
		std::string const PString( stripped( String ) );
		std::string::size_type const StringLen( PString.length() );
		ErrorFlag = false;
		if ( StringLen == 0 ) return rProcessNumber;
		int IoStatus( 0 );
		if ( PString.find_first_not_of( ValidNumerics ) == std::string::npos ) {
			{
				IOFlags flags;
				gio::read( PString, fmtLD, flags ) >> rProcessNumber;
				IoStatus = flags.ios();
			}
			ErrorFlag = false;
		} else {
			rProcessNumber = 0.0;
			ErrorFlag = true;
		}
		if ( IoStatus != 0 ) {
			rProcessNumber = 0.0;
			ErrorFlag = true;
		}

		return rProcessNumber;

	}

	int
	InputProcessor::FindItemInList(
		std::string const & String,
		Array1_string const & ListOfItems,
		int const NumItems
	) {

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   September 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up a string in a similar list of
		// items and returns the index of the item in the list, if
		// found.  This routine is not case insensitive and doesn't need
		// for most inputs -- they are automatically turned to UPPERCASE.
		// If you need case insensitivity use FindItem.

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		for ( int Count = 1; Count <= NumItems; ++Count ) {
			if ( String == ListOfItems( Count ) ) return Count;
		}
		return 0; // Not found
	}

	int
	InputProcessor::FindItemInList(
		std::string const & String,
		Array1S_string const ListOfItems,
		int const NumItems
	) {

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   September 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up a string in a similar list of
		// items and returns the index of the item in the list, if
		// found.  This routine is not case insensitive and doesn't need
		// for most inputs -- they are automatically turned to UPPERCASE.
		// If you need case insensitivity use FindItem.

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		for ( int Count = 1; Count <= NumItems; ++Count ) {
			if ( String == ListOfItems( Count ) ) return Count;
		}
		return 0; // Not found
	}


	int
	InputProcessor::FindItemInSortedList(
		std::string const & String,
		Array1S_string const ListOfItems,
		int const NumItems
	) {

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   September 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up a string in a similar list of
		// items and returns the index of the item in the list, if
		// found.  This routine is case insensitive.

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int Probe( 0 );
		int LBnd( 0 );
		int UBnd( NumItems + 1 );
		bool Found( false );
		while ( ( !Found ) || ( Probe != 0 ) ) {
			Probe = ( UBnd - LBnd ) / 2;
			if ( Probe == 0 ) break;
			Probe += LBnd;
			if ( equali( String, ListOfItems( Probe ) ) ) {
				Found = true;
				break;
			} else if ( lessthani( String, ListOfItems( Probe ) ) ) {
				UBnd = Probe;
			} else {
				LBnd = Probe;
			}
		}
		return Probe;
	}

	int
	InputProcessor::FindItem(
		std::string const & String,
		Array1D_string const & ListOfItems,
		int const NumItems
	) {

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   April 1999
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up a string in a similar list of
		// items and returns the index of the item in the list, if
		// found.  This routine is case insensitive.

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int FindItem = FindItemInList( String, ListOfItems, NumItems );
		if ( FindItem != 0 ) return FindItem;

		for ( int Count = 1; Count <= NumItems; ++Count ) {
			if ( equali( String, ListOfItems( Count ) ) ) return Count;
		}
		return 0; // Not found
	}

	int
	InputProcessor::FindItem(
		std::string const & String,
		Array1S_string const ListOfItems,
		int const NumItems
	) {

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   April 1999
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up a string in a similar list of
		// items and returns the index of the item in the list, if
		// found.  This routine is case insensitive.

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int FindItem = FindItemInList( String, ListOfItems, NumItems );
		if ( FindItem != 0 ) return FindItem;

		for ( int Count = 1; Count <= NumItems; ++Count ) {
			if ( equali( String, ListOfItems( Count ) ) ) return Count;
		}
		return 0; // Not found
	}

	std::string
	InputProcessor::MakeUPPERCase( std::string const & InputString ) {

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   September 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This function returns the Upper Case representation of the InputString.

		// METHODOLOGY EMPLOYED:
		// Uses the Intrinsic SCAN function to scan the lowercase representation of
		// characters (DataStringGlobals) for each character in the given string.

		// FUNCTION LOCAL VARIABLE DECLARATIONS:

		std::string ResultString( InputString );

		for ( std::string::size_type i = 0, e = len( InputString ); i < e; ++i ) {
			int const curCharVal = int( InputString[ i ] );
			if ( ( 97 <= curCharVal && curCharVal <= 122 ) ||
				 ( 224 <= curCharVal && curCharVal <= 255 ) ) { // lowercase ASCII and accented characters
				ResultString[ i ] = char( curCharVal - 32 );
			}
		}

		return ResultString;

	}

	void
	InputProcessor::VerifyName(
		std::string const & NameToVerify,
		Array1D_string const & NamesList,
		int const NumOfNames,
		bool & ErrorFound,
		bool & IsBlank,
		std::string const & StringToDisplay
	) {

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   February 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine verifys that a new name can be added to the
		// list of names for this item (i.e., that there isn't one of that
		// name already and that this name is not blank).

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Found;

		ErrorFound = false;
		if ( NumOfNames > 0 ) {
			Found = FindItem( NameToVerify, NamesList, NumOfNames );
			if ( Found != 0 ) {
				ShowSevereError( StringToDisplay + ", duplicate name=" + NameToVerify );
				ErrorFound = true;
			}
		}

		if ( NameToVerify.empty() ) {
			ShowSevereError( StringToDisplay + ", cannot be blank" );
			ErrorFound = true;
			IsBlank = true;
		} else {
			IsBlank = false;
		}

	}

	void
	InputProcessor::VerifyName(
		std::string const & NameToVerify,
		Array1S_string const NamesList,
		int const NumOfNames,
		bool & ErrorFound,
		bool & IsBlank,
		std::string const & StringToDisplay
	) {

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   February 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine verifys that a new name can be added to the
		// list of names for this item (i.e., that there isn't one of that
		// name already and that this name is not blank).

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Found;

		ErrorFound = false;
		if ( NumOfNames > 0 ) {
			Found = FindItem( NameToVerify, NamesList, NumOfNames );
			if ( Found != 0 ) {
				ShowSevereError( StringToDisplay + ", duplicate name=" + NameToVerify );
				ErrorFound = true;
			}
		}

		if ( NameToVerify.empty() ) {
			ShowSevereError( StringToDisplay + ", cannot be blank" );
			ErrorFound = true;
			IsBlank = true;
		} else {
			IsBlank = false;
		}

	}

	bool
	EnergyPlus::InputProcessor::IsNameEmpty (
		std::string & NameToVerify,
		std::string const & StringToDisplay,
		bool & ErrorFound
	){
		if ( NameToVerify.empty() ) {
			ShowSevereError(StringToDisplay + " Name, cannot be blank");
			ErrorFound = true;
			NameToVerify = "xxxxx";
			return true;
		}
		return false;
	}

	void
	InputProcessor::RangeCheck(
		bool & ErrorsFound, // Set to true if error detected
		std::string const & WhatFieldString, // Descriptive field for string
		std::string const & WhatObjectString, // Descriptive field for object, Zone Name, etc.
		std::string const & ErrorLevel, // 'Warning','Severe','Fatal')
		Optional_string_const LowerBoundString, // String for error message, if applicable
		Optional_bool_const LowerBoundCondition, // Condition for error condition, if applicable
		Optional_string_const UpperBoundString, // String for error message, if applicable
		Optional_bool_const UpperBoundCondition, // Condition for error condition, if applicable
		Optional_string_const ValueString, // Value with digits if to be displayed with error
		Optional_string_const WhatObjectName // ObjectName -- used for error messages
	) {

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   July 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is a general purpose "range check" routine for GetInput routines.
		// Using the standard "ErrorsFound" logical, this routine can produce a reasonable
		// error message to describe the situation in addition to setting the ErrorsFound variable
		// to true.

		std::string ErrorString; // Uppercase representation of ErrorLevel
		std::string Message1;
		std::string Message2;

		bool Error = false;
		if ( present( UpperBoundCondition ) ) {
			if ( !UpperBoundCondition ) Error = true;
		}
		if ( present( LowerBoundCondition ) ) {
			if ( !LowerBoundCondition ) Error = true;
		}

		if ( Error ) {
			ConvertCaseToUpper( ErrorLevel, ErrorString );
			Message1 = WhatObjectString;
			if ( present( WhatObjectName ) ) Message1 += "=\"" + WhatObjectName + "\", out of range data";
			Message2 = "Out of range value field=" + WhatFieldString;
			if ( present( ValueString ) ) Message2 += ", Value=[" + ValueString + ']';
			Message2 += ", range={";
			if ( present( LowerBoundString ) ) Message2 += LowerBoundString;
			if ( present( LowerBoundString ) && present( UpperBoundString ) ) {
				Message2 += " and " + UpperBoundString;
			} else if ( present( UpperBoundString ) ) {
				Message2 += UpperBoundString;
			}
			Message2 += "}";

			{
				auto const errorCheck( ErrorString[ 0 ] );

				if ( ( errorCheck == 'W' ) || ( errorCheck == 'w' ) ) {
					ShowWarningError( Message1 );
					ShowContinueError( Message2 );

				} else if ( ( errorCheck == 'S' ) || ( errorCheck == 's' ) ) {
					ShowSevereError( Message1 );
					ShowContinueError( Message2 );
					ErrorsFound = true;

				} else if ( ( errorCheck == 'F' ) || ( errorCheck == 'f' ) ) {
					ShowSevereError( Message1 );
					ShowContinueError( Message2 );
					ShowFatalError( "Program terminates due to preceding condition(s)." );

				} else {
					ShowSevereError( Message1 );
					ShowContinueError( Message2 );
					ErrorsFound = true;
				}
			}
		}
	}

	void
	InputProcessor::GetMaxSchemaArgs(
		int & NumArgs,
		int & NumAlpha,
		int & NumNumeric
	) {
		NumArgs = 0;
		NumAlpha = 0;
		NumNumeric = 0;
		std::string extension_key;
		auto const & schema_properties = schema.at( "properties" );

		for ( json::iterator object = jdf.begin(); object != jdf.end(); ++object ) {
			int num_alpha = 0;
			int num_numeric = 0;

			const json & legacy_idd = schema_properties.at( object.key() ).at( "legacy_idd" );
			auto key = legacy_idd.find("extension");
			if ( key != legacy_idd.end() ) {
				extension_key = key.value();
			}

			size_t max_size = 0;
			for ( auto const & obj : object.value() ) {
				auto const & find_extensions = obj.find( extension_key );
				if ( find_extensions != obj.end() ) {
					auto const size = find_extensions.value().size();
					if ( size > max_size ) max_size = size;
				}
			}

			auto const & find_alphas = legacy_idd.find( "alphas" );
			if ( find_alphas != legacy_idd.end() ) {
				json const & alphas = find_alphas.value();
				auto const & find_fields = alphas.find( "fields" );
				if ( find_fields != alphas.end() ) {
					num_alpha += find_fields.value().size();
				}
				if ( alphas.find( "extensions" ) != alphas.end() ) {
					num_alpha += alphas[ "extensions" ].size() * max_size;
				}
			}
			if ( legacy_idd.find( "numerics" ) != legacy_idd.end() ) {
				json const & numerics = legacy_idd[ "numerics" ];
				if ( numerics.find( "fields" ) != numerics.end() ) {
					num_numeric += numerics[ "fields" ].size();
				}
				if ( numerics.find( "extensions" ) != numerics.end() ) {
					num_numeric += numerics[ "extensions" ].size() * max_size;
				}
			}
			if ( num_alpha > NumAlpha ) NumAlpha = num_alpha;
			if ( num_numeric > NumNumeric ) NumNumeric = num_numeric;
		}

		NumArgs = NumAlpha + NumNumeric;
	}

	void
	InputProcessor::GetObjectDefMaxArgs(
		std::string const & ObjectWord, // Object for definition
		int & NumArgs, // How many arguments (max) this Object can have
		int & NumAlpha, // How many Alpha arguments (max) this Object can have
		int & NumNumeric // How many Numeric arguments (max) this Object can have
	) {
		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine returns maximum argument limits (total, alphas, numerics) of an Object from the IDD.
		// These dimensions (not sure what one can use the total for) can be used to dynamically dimension the
		// arrays in the GetInput routines.

		NumArgs = 0;
		NumAlpha = 0;
		NumNumeric = 0;
		json * object;
		if ( schema[ "properties" ].find( ObjectWord ) == schema[ "properties" ].end() ) {
			auto tmp_umit = case_insensitive_object_map.find( MakeUPPERCase( ObjectWord ) );
			if ( tmp_umit == case_insensitive_object_map.end() ) {
				ShowSevereError(
				"GetObjectDefMaxArgs: Did not find object=\"" + ObjectWord + "\" in list of objects." );
				return;
			}
			object = &schema[ "properties" ][ tmp_umit->second ];
		} else {
			object = &schema[ "properties" ][ ObjectWord ];
		}
		const json & legacy_idd = object->at( "legacy_idd" );

		json * objects;
		if ( jdf.find( ObjectWord ) == jdf.end() ) {
			auto tmp_umit = case_insensitive_object_map.find( MakeUPPERCase( ObjectWord ) );
			if ( tmp_umit == case_insensitive_object_map.end() ) {
				ShowSevereError(
				"GetObjectDefMaxArgs: Did not find object=\"" + ObjectWord + "\" in list of objects." );
				return;
			}
			objects = &jdf[ tmp_umit->second ];
		} else {
			objects = &jdf[ ObjectWord ];
		}

		size_t max_size = 0;

		std::string extension_key;
		auto key = legacy_idd.find("extension");
		if ( key != legacy_idd.end() ) {
			extension_key = key.value();
		}

		for ( auto const obj : *objects ) {
			if ( obj.find( extension_key ) != obj.end() ) {
				auto const size = obj[ extension_key ].size();
				if ( size > max_size ) max_size = size;
			}
		}

		if ( legacy_idd.find( "alphas" ) != legacy_idd.end() ) {
			json const alphas = legacy_idd[ "alphas" ];
			if ( alphas.find( "fields" ) != alphas.end() ) {
				NumAlpha += alphas[ "fields" ].size();
			}
			if ( alphas.find( "extensions" ) != alphas.end() ) {
				NumAlpha += alphas[ "extensions" ].size() * max_size;
			}
		}
		if ( legacy_idd.find( "numerics" ) != legacy_idd.end() ) {
			json const numerics = legacy_idd[ "numerics" ];
			if ( numerics.find( "fields" ) != numerics.end() ) {
				NumNumeric += numerics[ "fields" ].size();
			}
			if ( numerics.find( "extensions" ) != numerics.end() ) {
				NumNumeric += numerics[ "extensions" ].size() * max_size;
			}
		}
		NumArgs = NumAlpha + NumNumeric;
	}

	void
	InputProcessor::ReportOrphanRecordObjects()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   August 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  Mark Adams, Oct 2016

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine reports "orphan" objects that are in the input but were
		// not "gotten" during the simulation.

		std::unordered_set< std::string > unused_object_types;
		unused_object_types.reserve( unused_inputs.size() );

		if ( unused_inputs.size() && DisplayUnusedObjects ) {
			ShowWarningError( "The following lines are \"Unused Objects\".  These objects are in the input" );
			ShowContinueError( " file but are never obtained by the simulation and therefore are NOT used." );
			if ( ! DisplayAllWarnings ) {
				ShowContinueError( " Only the first unused named object of an object class is shown.  Use Output:Diagnostics,DisplayAllWarnings; to see all." );
			} else {
				ShowContinueError( " Each unused object is shown." );
			}
			ShowContinueError( " See InputOutputReference document for more details." );
		}

		bool first_iteration = true;
		for ( auto it = unused_inputs.begin(); it != unused_inputs.end(); ++it ) {
			auto const & object_type = it->second.first;
			auto const & name = it->second.second;

			// there are some orphans that we are deeming as special, in that they should be warned in detail even if !DisplayUnusedObjects and !DisplayAllWarnings
			if ( has_prefix( object_type, "ZoneHVAC:" ) ) {
				ShowSevereError( "Orphaned ZoneHVAC object found.  This was object never referenced in the input, and was not used." );
				ShowContinueError( " -- Object type: " + object_type );
				ShowContinueError( " -- Object name: " + name );
			}

			if ( ! DisplayUnusedObjects ) continue;

			if ( ! DisplayAllWarnings ) {
				auto found_type = unused_object_types.find( object_type );
				if ( found_type != unused_object_types.end() ) {
					// only show first unused named object of an object class
					continue;
				} else {
					unused_object_types.emplace( object_type );
				}
			}

			if ( first_iteration ) {
				if ( ! name.empty() ) {
					ShowMessage( "Object=" + object_type + '=' + name );
				} else {
					ShowMessage( "Object=" + object_type );
				}
				first_iteration = false;
			} else {
				if ( ! name.empty() ) {
					ShowContinueError( "Object=" + object_type + '=' + name );
				} else {
					ShowContinueError( "Object=" + object_type );
				}
			}
		}

		if ( unused_inputs.size() && ! DisplayUnusedObjects ) {
			u64toa( unused_inputs.size(), s );
			ShowMessage( "There are " + std::string( s ) + " unused objects in input." );
			ShowMessage( "Use Output:Diagnostics,DisplayUnusedObjects; to see them." );
		}

	}

	void
	InputProcessor::PreProcessorCheck( bool & PreP_Fatal ) // True if a preprocessor flags a fatal error
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   August 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine checks for existance of "Preprocessor Message" object and
		// performs appropriate action.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// Preprocessor Message,
		//    \memo This object does not come from a user input.  This is generated by a pre-processor
		//    \memo so that various conditions can be gracefully passed on by the InputProcessor.
		//    A1,        \field preprocessor name
		//    A2,        \field error severity
		//               \note Depending on type, InputProcessor may terminate the program.
		//               \type choice
		//               \key warning
		//               \key severe
		//               \key fatal
		//    A3,        \field message line 1
		//    A4,        \field message line 2
		//    A5,        \field message line 3
		//    A6,        \field message line 4
		//    A7,        \field message line 5
		//    A8,        \field message line 6
		//    A9,        \field message line 7
		//    A10,       \field message line 8
		//    A11,       \field message line 9
		//    A12;       \field message line 10

		// Using/Aliasing
		using namespace DataIPShortCuts;

		int NumAlphas; // Used to retrieve names from IDF
		int NumNumbers; // Used to retrieve rNumericArgs from IDF
		int IOStat; // Could be used in the Get Routines, not currently checked
		int NumParams; // Total Number of Parameters in 'Output:PreprocessorMessage' Object
		int NumPrePM; // Number of Preprocessor Message objects in IDF
		int CountP;
		int CountM;
		std::string Multiples;

		cCurrentModuleObject = "Output:PreprocessorMessage";
		NumPrePM = InputProcessor::GetNumObjectsFound( cCurrentModuleObject );
		if ( NumPrePM > 0 ) {
			GetObjectDefMaxArgs( cCurrentModuleObject, NumParams, NumAlphas, NumNumbers );
			cAlphaArgs( { 1, NumAlphas } ) = BlankString;
			for ( CountP = 1; CountP <= NumPrePM; ++CountP ) {
				InputProcessor::GetObjectItem( cCurrentModuleObject, CountP, cAlphaArgs, NumAlphas, rNumericArgs,
											   NumNumbers, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks,
											   cAlphaFieldNames, cNumericFieldNames );
				if ( cAlphaArgs( 1 ).empty() ) cAlphaArgs( 1 ) = "Unknown";
				if ( NumAlphas > 3 ) {
					Multiples = "s";
				} else {
					Multiples = BlankString;
				}
				if ( cAlphaArgs( 2 ).empty() ) cAlphaArgs( 2 ) = "Unknown";
				{
					auto const errorType( uppercased( cAlphaArgs( 2 ) ) );
					if ( errorType == "INFORMATION" ) {
						ShowMessage( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) +
									 "\" has the following Information message" + Multiples + ':' );
					} else if ( errorType == "WARNING" ) {
						ShowWarningError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) +
										  "\" has the following Warning condition" + Multiples + ':' );
					} else if ( errorType == "SEVERE" ) {
						ShowSevereError(
						cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) +
						"\" has the following Severe condition" +
						Multiples + ':' );
					} else if ( errorType == "FATAL" ) {
						ShowSevereError(
						cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) +
						"\" has the following Fatal condition" +
						Multiples + ':' );
						PreP_Fatal = true;
					} else {
						ShowSevereError(
						cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" has the following " +
						cAlphaArgs( 2 ) +
						" condition" + Multiples + ':' );
					}
				}
				CountM = 3;
				if ( CountM > NumAlphas ) {
					ShowContinueError( cCurrentModuleObject + " was blank.  Check " + cAlphaArgs( 1 ) +
									   " audit trail or error file for possible reasons." );
				}
				while ( CountM <= NumAlphas ) {
					if ( len( cAlphaArgs( CountM ) ) == MaxNameLength ) {
						ShowContinueError( cAlphaArgs( CountM ) + cAlphaArgs( CountM + 1 ) );
						CountM += 2;
					} else {
						ShowContinueError( cAlphaArgs( CountM ) );
						++CountM;
					}
				}
			}
		}

	}

	void
	InputProcessor::PreScanReportingVariables() {
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   July 2010

		// PURPOSE OF THIS SUBROUTINE:
		// This routine scans the input records and determines which output variables
		// are actually being requested for the run so that the OutputProcessor will only
		// consider those variables for output.  (At this time, all metered variables are
		// allowed to pass through).

		// METHODOLOGY EMPLOYED:
		// Uses internal records and structures.
		// Looks at:
		// Output:Variable
		// Meter:Custom
		// Meter:CustomDecrement
		// Meter:CustomDifference
		// Output:Table:Monthly
		// Output:Table:TimeBins
		// Output:Table:SummaryReports
		// EnergyManagementSystem:Sensor
		// EnergyManagementSystem:OutputVariable

		// Using/Aliasing
		using namespace DataOutputs;

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const OutputVariable( "Output:Variable" );
		static std::string const MeterCustom( "Meter:Custom" );
		static std::string const MeterCustomDecrement( "Meter:CustomDecrement" );
//		static std::string const MeterCustomDifference( "METER:CUSTOMDIFFERENCE" );
		static std::string const OutputTableMonthly( "Output:Table:Monthly" );
		static std::string const OutputTableAnnual( "Output:Table:Annual" );
		static std::string const OutputTableTimeBins( "Output:Table:TimeBins" );
		static std::string const OutputTableSummaries( "Output:Table:SummaryReports" );
		static std::string const EMSSensor( "EnergyManagementSystem:Sensor" );
		static std::string const EMSOutputVariable( "EnergyManagementSystem:OutputVariable" );

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		std::string extension_key;
		OutputVariablesForSimulation.reserve( 1024 );
		MaxConsideredOutputVariables = 10000;

		// Output Variable
		auto jdf_objects = jdf.find( OutputVariable );
		if ( jdf_objects != jdf.end() ) {
			auto const & jdf_object = jdf_objects.value();
			for ( auto obj = jdf_object.begin(); obj != jdf_object.end(); ++obj ) {
				json const & fields = obj.value();
				if ( !fields.at( "key_value" ).empty() ) {
					InputProcessor::AddRecordToOutputVariableStructure( fields.at( "key_value" ),
																		fields.at( "variable_name" ) );
				} else {
					InputProcessor::AddRecordToOutputVariableStructure( "*", fields.at( "variable_name" ) );
				}
			}
		}

		jdf_objects = jdf.find( MeterCustom );
		if ( jdf_objects != jdf.end() ) {
			auto const & jdf_object = jdf_objects.value();
			auto const & legacy_idd = schema[ "properties" ][ MeterCustom ][ "legacy_idd" ];
			auto key = legacy_idd.find("extension");
			if ( key != legacy_idd.end() ) {
				extension_key = key.value();
			}
			for ( auto obj = jdf_object.begin(); obj != jdf_object.end(); ++obj ) {
				json const & fields = obj.value();
				for ( auto const & extensions : fields[ extension_key ] ) {
					if ( !obj.key().empty() ) {
						InputProcessor::AddRecordToOutputVariableStructure( extensions.at( "key_name" ),
																			extensions.at(
																			"output_variable_or_meter_name" ) );
					} else {
						InputProcessor::AddRecordToOutputVariableStructure( "*", extensions.at(
						"output_variable_or_meter_name" ) );
					}
				}
			}
		}

		jdf_objects = jdf.find( MeterCustomDecrement );
		if ( jdf_objects != jdf.end() ) {
			auto const & jdf_object = jdf_objects.value();
			auto const & legacy_idd = schema[ "properties" ][ MeterCustomDecrement ][ "legacy_idd" ];
			auto key = legacy_idd.find("extension");
			if ( key != legacy_idd.end() ) {
				extension_key = key.value();
			}
			for ( auto obj = jdf_object.begin(); obj != jdf_object.end(); ++obj ) {
				json const & fields = obj.value();
				for ( auto const & extensions : fields[ extension_key ] ) {
					if ( !obj.key().empty() ) {
						InputProcessor::AddRecordToOutputVariableStructure( extensions.at( "key_name" ),
																			extensions.at(
																			"output_variable_or_meter_name" ) );
					} else {
						InputProcessor::AddRecordToOutputVariableStructure( "*", extensions.at(
						"output_variable_or_meter_name" ) );
					}
				}
			}
		}

		jdf_objects = jdf.find( EMSSensor );
		if ( jdf_objects != jdf.end() ) {
			auto const & jdf_object = jdf_objects.value();
			for ( auto obj = jdf_object.begin(); obj != jdf_object.end(); ++obj ) {
				json const & fields = obj.value();
				if ( !fields.at( "output_variable_or_output_meter_index_key_name" ).empty() ) {
					InputProcessor::AddRecordToOutputVariableStructure(
					fields.at( "output_variable_or_output_meter_index_key_name" ),
					fields.at( "output_variable_or_output_meter_name" ) );
				} else {
					InputProcessor::AddRecordToOutputVariableStructure( "*", fields.at(
					"output_variable_or_output_meter_name" ) );
				}
			}
		}

		jdf_objects = jdf.find( EMSOutputVariable );
		if ( jdf_objects != jdf.end() ) {
			auto const & jdf_object = jdf_objects.value();
			for ( auto obj = jdf_object.begin(); obj != jdf_object.end(); ++obj ) {
				InputProcessor::AddRecordToOutputVariableStructure( "*", obj.key() );
			}
		}

		jdf_objects = jdf.find( OutputTableTimeBins );
		if ( jdf_objects != jdf.end() ) {
			auto const & jdf_object = jdf_objects.value();
			for ( auto obj = jdf_object.begin(); obj != jdf_object.end(); ++obj ) {
				json const & fields = obj.value();
				if ( !obj.key().empty() ) {
					InputProcessor::AddRecordToOutputVariableStructure( obj.key(), fields.at( "key_value" ) );
				} else {
					InputProcessor::AddRecordToOutputVariableStructure( "*", fields.at( "key_value" ) );
				}
			}
		}

		jdf_objects = jdf.find( OutputTableMonthly );
		if ( jdf_objects != jdf.end() ) {
			auto const & jdf_object = jdf_objects.value();
			auto const & legacy_idd = schema[ "properties" ][ OutputTableMonthly ][ "legacy_idd" ];
			auto key = legacy_idd.find("extension");
			if ( key != legacy_idd.end() ) {
				extension_key = key.value();
			}
			for ( auto obj = jdf_object.begin(); obj != jdf_object.end(); ++obj ) {
				json const & fields = obj.value();
				for ( auto const & extensions : fields[ extension_key ] ) {
					InputProcessor::AddRecordToOutputVariableStructure( "*",
																		extensions.at( "variable_or_meter_name" ) );
				}
			}
		}

		jdf_objects = jdf.find( OutputTableAnnual );
		if ( jdf_objects != jdf.end() ) {
			auto const & jdf_object = jdf_objects.value();
			auto const & legacy_idd = schema[ "properties" ][ OutputTableAnnual ][ "legacy_idd" ];
			auto key = legacy_idd.find("extension");
			if ( key != legacy_idd.end() ) {
				extension_key = key.value();
			}
			for ( auto obj = jdf_object.begin(); obj != jdf_object.end(); ++obj ) {
				json const & fields = obj.value();
				for ( auto const & extensions : fields[ extension_key ] ) {
					InputProcessor::AddRecordToOutputVariableStructure( "*", extensions.at(
					"variable_or_meter_or_ems_variable_or_field_name" ) );
				}
			}
		}

		jdf_objects = jdf.find( OutputTableSummaries );
		if ( jdf_objects != jdf.end() ) {
			auto const & jdf_object = jdf_objects.value();
			auto const & legacy_idd = schema[ "properties" ][ OutputTableSummaries ][ "legacy_idd" ];
			auto key = legacy_idd.find("extension");
			if ( key != legacy_idd.end() ) {
				extension_key = key.value();
			}
			for ( auto obj = jdf_object.begin(); obj != jdf_object.end(); ++obj ) {
				json const & fields = obj.value();
				for ( auto const & extensions : fields[ extension_key ] ) {
					auto const report_name = MakeUPPERCase( extensions.at( "report_name" ) );
					if ( report_name == "ALLMONTHLY" || report_name == "ALLSUMMARYANDMONTHLY" ) {
						for ( int i = 1; i <= NumMonthlyReports; ++i ) {
							InputProcessor::AddVariablesForMonthlyReport( MonthlyNamedReports( i ) );
						}
					} else {
						InputProcessor::AddVariablesForMonthlyReport( report_name );
					}
				}
			}
		}

	}

	void
	InputProcessor::AddVariablesForMonthlyReport( std::string const & reportName ) {

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   July 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine adds specific variables to the Output Variables for Simulation
		// Structure. Note that only non-metered variables need to be added here.  Metered
		// variables are automatically included in the minimized output variable structure.

		if ( reportName == "ZONECOOLINGSUMMARYMONTHLY" ) {
			AddRecordToOutputVariableStructure( "*", "ZONE AIR SYSTEM SENSIBLE COOLING RATE" );
			AddRecordToOutputVariableStructure( "*", "SITE OUTDOOR AIR DRYBULB TEMPERATURE" );
			AddRecordToOutputVariableStructure( "*", "SITE OUTDOOR AIR WETBULB TEMPERATURE" );
			AddRecordToOutputVariableStructure( "*", "ZONE TOTAL INTERNAL LATENT GAIN ENERGY" );
			AddRecordToOutputVariableStructure( "*", "ZONE TOTAL INTERNAL LATENT GAIN RATE" );

		} else if ( reportName == "ZONEHEATINGSUMMARYMONTHLY" ) {
			AddRecordToOutputVariableStructure( "*", "ZONE AIR SYSTEM SENSIBLE HEATING ENERGY" ); // on meter
			AddRecordToOutputVariableStructure( "*", "ZONE AIR SYSTEM SENSIBLE HEATING RATE" );
			AddRecordToOutputVariableStructure( "*", "SITE OUTDOOR AIR DRYBULB TEMPERATURE" );

		} else if ( reportName == "ZONEELECTRICSUMMARYMONTHLY" ) {
			AddRecordToOutputVariableStructure( "*", "ZONE LIGHTS ELECTRIC ENERGY" );
			AddRecordToOutputVariableStructure( "*", "ZONE ELECTRIC EQUIPMENT ELECTRIC ENERGY" );

		} else if ( reportName == "SPACEGAINSMONTHLY" ) {
			AddRecordToOutputVariableStructure( "*", "ZONE PEOPLE TOTAL HEATING ENERGY" );
			AddRecordToOutputVariableStructure( "*", "ZONE LIGHTS TOTAL HEATING ENERGY" );
			AddRecordToOutputVariableStructure( "*", "ZONE ELECTRIC EQUIPMENT TOTAL HEATING ENERGY" );
			AddRecordToOutputVariableStructure( "*", "ZONE GAS EQUIPMENT TOTAL HEATING ENERGY" );
			AddRecordToOutputVariableStructure( "*", "ZONE HOT WATER EQUIPMENT TOTAL HEATING ENERGY" );
			AddRecordToOutputVariableStructure( "*", "ZONE STEAM EQUIPMENT TOTAL HEATING ENERGY" );
			AddRecordToOutputVariableStructure( "*", "ZONE OTHER EQUIPMENT TOTAL HEATING ENERGY" );
			AddRecordToOutputVariableStructure( "*", "ZONE INFILTRATION SENSIBLE HEAT GAIN ENERGY" );
			AddRecordToOutputVariableStructure( "*", "ZONE INFILTRATION SENSIBLE HEAT LOSS ENERGY" );

		} else if ( reportName == "PEAKSPACEGAINSMONTHLY" ) {
			AddRecordToOutputVariableStructure( "*", "ZONE PEOPLE TOTAL HEATING ENERGY" );
			AddRecordToOutputVariableStructure( "*", "ZONE LIGHTS TOTAL HEATING ENERGY" );
			AddRecordToOutputVariableStructure( "*", "ZONE ELECTRIC EQUIPMENT TOTAL HEATING ENERGY" );
			AddRecordToOutputVariableStructure( "*", "ZONE GAS EQUIPMENT TOTAL HEATING ENERGY" );
			AddRecordToOutputVariableStructure( "*", "ZONE HOT WATER EQUIPMENT TOTAL HEATING ENERGY" );
			AddRecordToOutputVariableStructure( "*", "ZONE STEAM EQUIPMENT TOTAL HEATING ENERGY" );
			AddRecordToOutputVariableStructure( "*", "ZONE OTHER EQUIPMENT TOTAL HEATING ENERGY" );
			AddRecordToOutputVariableStructure( "*", "ZONE INFILTRATION SENSIBLE HEAT GAIN ENERGY" );
			AddRecordToOutputVariableStructure( "*", "ZONE INFILTRATION SENSIBLE HEAT LOSS ENERGY" );

		} else if ( reportName == "SPACEGAINCOMPONENTSATCOOLINGPEAKMONTHLY" ) {
			AddRecordToOutputVariableStructure( "*", "ZONE AIR SYSTEM SENSIBLE COOLING RATE" );
			AddRecordToOutputVariableStructure( "*", "ZONE PEOPLE TOTAL HEATING ENERGY" );
			AddRecordToOutputVariableStructure( "*", "ZONE LIGHTS TOTAL HEATING ENERGY" );
			AddRecordToOutputVariableStructure( "*", "ZONE ELECTRIC EQUIPMENT TOTAL HEATING ENERGY" );
			AddRecordToOutputVariableStructure( "*", "ZONE GAS EQUIPMENT TOTAL HEATING ENERGY" );
			AddRecordToOutputVariableStructure( "*", "ZONE HOT WATER EQUIPMENT TOTAL HEATING ENERGY" );
			AddRecordToOutputVariableStructure( "*", "ZONE STEAM EQUIPMENT TOTAL HEATING ENERGY" );
			AddRecordToOutputVariableStructure( "*", "ZONE OTHER EQUIPMENT TOTAL HEATING ENERGY" );
			AddRecordToOutputVariableStructure( "*", "ZONE INFILTRATION SENSIBLE HEAT GAIN ENERGY" );
			AddRecordToOutputVariableStructure( "*", "ZONE INFILTRATION SENSIBLE HEAT LOSS ENERGY" );

		} else if ( reportName == "SETPOINTSNOTMETWITHTEMPERATURESMONTHLY" ) {
			AddRecordToOutputVariableStructure( "*", "ZONE HEATING SETPOINT NOT MET TIME" );
			AddRecordToOutputVariableStructure( "*", "ZONE MEAN AIR TEMPERATURE" );
			AddRecordToOutputVariableStructure( "*", "ZONE HEATING SETPOINT NOT MET WHILE OCCUPIED TIME" );
			AddRecordToOutputVariableStructure( "*", "ZONE COOLING SETPOINT NOT MET TIME" );
			AddRecordToOutputVariableStructure( "*", "ZONE COOLING SETPOINT NOT MET WHILE OCCUPIED TIME" );

		} else if ( reportName == "COMFORTREPORTSIMPLE55MONTHLY" ) {
			AddRecordToOutputVariableStructure( "*",
												"ZONE THERMAL COMFORT ASHRAE 55 SIMPLE MODEL SUMMER CLOTHES NOT COMFORTABLE TIME" );
			AddRecordToOutputVariableStructure( "*", "ZONE MEAN AIR TEMPERATURE" );
			AddRecordToOutputVariableStructure( "*",
												"ZONE THERMAL COMFORT ASHRAE 55 SIMPLE MODEL WINTER CLOTHES NOT COMFORTABLE TIME" );
			AddRecordToOutputVariableStructure( "*",
												"ZONE THERMAL COMFORT ASHRAE 55 SIMPLE MODEL SUMMER OR WINTER CLOTHES NOT COMFORTABLE TIME" );

		} else if ( reportName == "UNGLAZEDTRANSPIREDSOLARCOLLECTORSUMMARYMONTHLY" ) {
			AddRecordToOutputVariableStructure( "*", "SOLAR COLLECTOR SYSTEM EFFICIENCY" );
			AddRecordToOutputVariableStructure( "*", "SOLAR COLLECTOR OUTSIDE FACE SUCTION VELOCITY" );
			AddRecordToOutputVariableStructure( "*", "SOLAR COLLECTOR SENSIBLE HEATING RATE" );

		} else if ( reportName == "OCCUPANTCOMFORTDATASUMMARYMONTHLY" ) {
			AddRecordToOutputVariableStructure( "*", "PEOPLE OCCUPANT COUNT" );
			AddRecordToOutputVariableStructure( "*", "PEOPLE AIR TEMPERATURE" );
			AddRecordToOutputVariableStructure( "*", "PEOPLE AIR RELATIVE HUMIDITY" );
			AddRecordToOutputVariableStructure( "*", "ZONE THERMAL COMFORT FANGER MODEL PMV" );
			AddRecordToOutputVariableStructure( "*", "ZONE THERMAL COMFORT FANGER MODEL PPD" );

		} else if ( reportName == "CHILLERREPORTMONTHLY" ) {
			AddRecordToOutputVariableStructure( "*", "CHILLER ELECTRIC ENERGY" ); // on meter
			AddRecordToOutputVariableStructure( "*", "CHILLER ELECTRIC POWER" );
			AddRecordToOutputVariableStructure( "*", "CHILLER EVAPORATOR COOLING ENERGY" ); // on meter
			AddRecordToOutputVariableStructure( "*", "CHILLER CONDENSER HEAT TRANSFER ENERGY" ); // on meter
			AddRecordToOutputVariableStructure( "*", "CHILLER COP" );

		} else if ( reportName == "TOWERREPORTMONTHLY" ) {
			AddRecordToOutputVariableStructure( "*", "COOLING TOWER FAN ELECTRIC ENERGY" ); // on meter
			AddRecordToOutputVariableStructure( "*", "COOLING TOWER FAN ELECTRIC POWER" );
			AddRecordToOutputVariableStructure( "*", "COOLING TOWER HEAT TRANSFER RATE" );
			AddRecordToOutputVariableStructure( "*", "COOLING TOWER INLET TEMPERATURE" );
			AddRecordToOutputVariableStructure( "*", "COOLING TOWER OUTLET TEMPERATURE" );
			AddRecordToOutputVariableStructure( "*", "COOLING TOWER MASS FLOW RATE" );

		} else if ( reportName == "BOILERREPORTMONTHLY" ) {
			AddRecordToOutputVariableStructure( "*", "BOILER HEATING ENERGY" ); // on meter
			AddRecordToOutputVariableStructure( "*", "BOILER GAS CONSUMPTION" ); // on meter
			AddRecordToOutputVariableStructure( "*", "BOILER HEATING ENERGY" ); // on meter
			AddRecordToOutputVariableStructure( "*", "BOILER HEATING RATE" );
			AddRecordToOutputVariableStructure( "*", "BOILER GAS CONSUMPTION RATE" );
			AddRecordToOutputVariableStructure( "*", "BOILER INLET TEMPERATURE" );
			AddRecordToOutputVariableStructure( "*", "BOILER OUTLET TEMPERATURE" );
			AddRecordToOutputVariableStructure( "*", "BOILER MASS FLOW RATE" );
			AddRecordToOutputVariableStructure( "*", "BOILER ANCILLARY ELECTRIC POWER" );

		} else if ( reportName == "DXREPORTMONTHLY" ) {
			AddRecordToOutputVariableStructure( "*", "COOLING COIL TOTAL COOLING ENERGY" ); // on meter
			AddRecordToOutputVariableStructure( "*", "COOLING COIL ELECTRIC ENERGY" ); // on meter
			AddRecordToOutputVariableStructure( "*", "COOLING COIL SENSIBLE COOLING ENERGY" );
			AddRecordToOutputVariableStructure( "*", "COOLING COIL LATENT COOLING ENERGY" );
			AddRecordToOutputVariableStructure( "*", "COOLING COIL CRANKCASE HEATER ELECTRIC ENERGY" );
			AddRecordToOutputVariableStructure( "*", "COOLING COIL RUNTIME FRACTION" );
			AddRecordToOutputVariableStructure( "*", "COOLING COIL TOTAL COOLING RATE" );
			AddRecordToOutputVariableStructure( "*", "COOLING COIL SENSIBLE COOLING RATE" );
			AddRecordToOutputVariableStructure( "*", "COOLING COIL LATENT COOLING RATE" );
			AddRecordToOutputVariableStructure( "*", "COOLING COIL ELECTRIC POWER" );
			AddRecordToOutputVariableStructure( "*", "COOLING COIL CRANKCASE HEATER ELECTRIC POWER" );

		} else if ( reportName == "WINDOWREPORTMONTHLY" ) {
			AddRecordToOutputVariableStructure( "*", "SURFACE WINDOW TRANSMITTED SOLAR RADIATION RATE" );
			AddRecordToOutputVariableStructure( "*", "SURFACE WINDOW TRANSMITTED BEAM SOLAR RADIATION RATE" );
			AddRecordToOutputVariableStructure( "*", "SURFACE WINDOW TRANSMITTED DIFFUSE SOLAR RADIATION RATE" );
			AddRecordToOutputVariableStructure( "*", "SURFACE WINDOW HEAT GAIN RATE" );
			AddRecordToOutputVariableStructure( "*", "SURFACE WINDOW HEAT LOSS RATE" );
			AddRecordToOutputVariableStructure( "*", "SURFACE WINDOW INSIDE FACE GLAZING CONDENSATION STATUS" );
			AddRecordToOutputVariableStructure( "*", "SURFACE SHADING DEVICE IS ON TIME FRACTION" );
			AddRecordToOutputVariableStructure( "*", "SURFACE STORM WINDOW ON OFF STATUS" );

		} else if ( reportName == "WINDOWENERGYREPORTMONTHLY" ) {
			AddRecordToOutputVariableStructure( "*", "SURFACE WINDOW TRANSMITTED SOLAR RADIATION ENERGY" );
			AddRecordToOutputVariableStructure( "*", "SURFACE WINDOW TRANSMITTED BEAM SOLAR RADIATION ENERGY" );
			AddRecordToOutputVariableStructure( "*", "SURFACE WINDOW TRANSMITTED DIFFUSE SOLAR RADIATION ENERGY" );
			AddRecordToOutputVariableStructure( "*", "SURFACE WINDOW HEAT GAIN ENERGY" );
			AddRecordToOutputVariableStructure( "*", "SURFACE WINDOW HEAT LOSS ENERGY" );

		} else if ( reportName == "WINDOWZONESUMMARYMONTHLY" ) {
			AddRecordToOutputVariableStructure( "*", "ZONE WINDOWS TOTAL HEAT GAIN RATE" );
			AddRecordToOutputVariableStructure( "*", "ZONE WINDOWS TOTAL HEAT LOSS RATE" );
			AddRecordToOutputVariableStructure( "*", "ZONE WINDOWS TOTAL TRANSMITTED SOLAR RADIATION RATE" );
			AddRecordToOutputVariableStructure( "*",
												"ZONE EXTERIOR WINDOWS TOTAL TRANSMITTED BEAM SOLAR RADIATION RATE" );
			AddRecordToOutputVariableStructure( "*",
												"ZONE EXTERIOR WINDOWS TOTAL TRANSMITTED DIFFUSE SOLAR RADIATION RATE" );
			AddRecordToOutputVariableStructure( "*",
												"ZONE INTERIOR WINDOWS TOTAL TRANSMITTED DIFFUSE SOLAR RADIATION RATE" );
			AddRecordToOutputVariableStructure( "*",
												"ZONE INTERIOR WINDOWS TOTAL TRANSMITTED BEAM SOLAR RADIATION RATE" );

		} else if ( reportName == "WINDOWENERGYZONESUMMARYMONTHLY" ) {
			AddRecordToOutputVariableStructure( "*", "ZONE WINDOWS TOTAL HEAT GAIN ENERGY" );
			AddRecordToOutputVariableStructure( "*", "ZONE WINDOWS TOTAL HEAT LOSS ENERGY" );
			AddRecordToOutputVariableStructure( "*", "ZONE WINDOWS TOTAL TRANSMITTED SOLAR RADIATION ENERGY" );
			AddRecordToOutputVariableStructure( "*",
												"ZONE EXTERIOR WINDOWS TOTAL TRANSMITTED BEAM SOLAR RADIATION ENERGY" );
			AddRecordToOutputVariableStructure( "*",
												"ZONE EXTERIOR WINDOWS TOTAL TRANSMITTED DIFFUSE SOLAR RADIATION ENERGY" );
			AddRecordToOutputVariableStructure( "*",
												"ZONE INTERIOR WINDOWS TOTAL TRANSMITTED DIFFUSE SOLAR RADIATION ENERGY" );
			AddRecordToOutputVariableStructure( "*",
												"ZONE INTERIOR WINDOWS TOTAL TRANSMITTED BEAM SOLAR RADIATION ENERGY" );

		} else if ( reportName == "AVERAGEOUTDOORCONDITIONSMONTHLY" ) {
			AddRecordToOutputVariableStructure( "*", "SITE OUTDOOR AIR DRYBULB TEMPERATURE" );
			AddRecordToOutputVariableStructure( "*", "SITE OUTDOOR AIR WETBULB TEMPERATURE" );
			AddRecordToOutputVariableStructure( "*", "SITE OUTDOOR AIR DEWPOINT TEMPERATURE" );
			AddRecordToOutputVariableStructure( "*", "SITE WIND SPEED" );
			AddRecordToOutputVariableStructure( "*", "SITE SKY TEMPERATURE" );
			AddRecordToOutputVariableStructure( "*", "SITE DIFFUSE SOLAR RADIATION RATE PER AREA" );
			AddRecordToOutputVariableStructure( "*", "SITE DIRECT SOLAR RADIATION RATE PER AREA" );
			AddRecordToOutputVariableStructure( "*", "SITE RAIN STATUS" );

		} else if ( reportName == "OUTDOORCONDITIONSMAXIMUMDRYBULBMONTHLY" ) {
			AddRecordToOutputVariableStructure( "*", "SITE OUTDOOR AIR DRYBULB TEMPERATURE" );
			AddRecordToOutputVariableStructure( "*", "SITE OUTDOOR AIR WETBULB TEMPERATURE" );
			AddRecordToOutputVariableStructure( "*", "SITE OUTDOOR AIR DEWPOINT TEMPERATURE" );
			AddRecordToOutputVariableStructure( "*", "SITE WIND SPEED" );
			AddRecordToOutputVariableStructure( "*", "SITE SKY TEMPERATURE" );
			AddRecordToOutputVariableStructure( "*", "SITE DIFFUSE SOLAR RADIATION RATE PER AREA" );
			AddRecordToOutputVariableStructure( "*", "SITE DIRECT SOLAR RADIATION RATE PER AREA" );

		} else if ( reportName == "OUTDOORCONDITIONSMINIMUMDRYBULBMONTHLY" ) {
			AddRecordToOutputVariableStructure( "*", "SITE OUTDOOR AIR DRYBULB TEMPERATURE" );
			AddRecordToOutputVariableStructure( "*", "SITE OUTDOOR AIR WETBULB TEMPERATURE" );
			AddRecordToOutputVariableStructure( "*", "SITE OUTDOOR AIR DEWPOINT TEMPERATURE" );
			AddRecordToOutputVariableStructure( "*", "SITE WIND SPEED" );
			AddRecordToOutputVariableStructure( "*", "SITE SKY TEMPERATURE" );
			AddRecordToOutputVariableStructure( "*", "SITE DIFFUSE SOLAR RADIATION RATE PER AREA" );
			AddRecordToOutputVariableStructure( "*", "SITE DIRECT SOLAR RADIATION RATE PER AREA" );

		} else if ( reportName == "OUTDOORCONDITIONSMAXIMUMWETBULBMONTHLY" ) {
			AddRecordToOutputVariableStructure( "*", "SITE OUTDOOR AIR WETBULB TEMPERATURE" );
			AddRecordToOutputVariableStructure( "*", "SITE OUTDOOR AIR DRYBULB TEMPERATURE" );
			AddRecordToOutputVariableStructure( "*", "SITE OUTDOOR AIR DEWPOINT TEMPERATURE" );
			AddRecordToOutputVariableStructure( "*", "SITE WIND SPEED" );
			AddRecordToOutputVariableStructure( "*", "SITE SKY TEMPERATURE" );
			AddRecordToOutputVariableStructure( "*", "SITE DIFFUSE SOLAR RADIATION RATE PER AREA" );
			AddRecordToOutputVariableStructure( "*", "SITE DIRECT SOLAR RADIATION RATE PER AREA" );

		} else if ( reportName == "OUTDOORCONDITIONSMAXIMUMDEWPOINTMONTHLY" ) {
			AddRecordToOutputVariableStructure( "*", "SITE OUTDOOR AIR DEWPOINT TEMPERATURE" );
			AddRecordToOutputVariableStructure( "*", "SITE OUTDOOR AIR DRYBULB TEMPERATURE" );
			AddRecordToOutputVariableStructure( "*", "SITE OUTDOOR AIR WETBULB TEMPERATURE" );
			AddRecordToOutputVariableStructure( "*", "SITE WIND SPEED" );
			AddRecordToOutputVariableStructure( "*", "SITE SKY TEMPERATURE" );
			AddRecordToOutputVariableStructure( "*", "SITE DIFFUSE SOLAR RADIATION RATE PER AREA" );
			AddRecordToOutputVariableStructure( "*", "SITE DIRECT SOLAR RADIATION RATE PER AREA" );

		} else if ( reportName == "OUTDOORGROUNDCONDITIONSMONTHLY" ) {
			AddRecordToOutputVariableStructure( "*", "SITE GROUND TEMPERATURE" );
			AddRecordToOutputVariableStructure( "*", "SITE SURFACE GROUND TEMPERATURE" );
			AddRecordToOutputVariableStructure( "*", "SITE DEEP GROUND TEMPERATURE" );
			AddRecordToOutputVariableStructure( "*", "SITE MAINS WATER TEMPERATURE" );
			AddRecordToOutputVariableStructure( "*", "SITE GROUND REFLECTED SOLAR RADIATION RATE PER AREA" );
			AddRecordToOutputVariableStructure( "*", "SITE SNOW ON GROUND STATUS" );

		} else if ( reportName == "WINDOWACREPORTMONTHLY" ) {
			AddRecordToOutputVariableStructure( "*", "ZONE WINDOW AIR CONDITIONER TOTAL COOLING ENERGY" );
			AddRecordToOutputVariableStructure( "*", "ZONE WINDOW AIR CONDITIONER ELECTRIC ENERGY" );
			AddRecordToOutputVariableStructure( "*", "ZONE WINDOW AIR CONDITIONER TOTAL COOLING ENERGY" );
			AddRecordToOutputVariableStructure( "*", "ZONE WINDOW AIR CONDITIONER SENSIBLE COOLING ENERGY" );
			AddRecordToOutputVariableStructure( "*", "ZONE WINDOW AIR CONDITIONER LATENT COOLING ENERGY" );
			AddRecordToOutputVariableStructure( "*", "ZONE WINDOW AIR CONDITIONER TOTAL COOLING RATE" );
			AddRecordToOutputVariableStructure( "*", "ZONE WINDOW AIR CONDITIONER SENSIBLE COOLING RATE" );
			AddRecordToOutputVariableStructure( "*", "ZONE WINDOW AIR CONDITIONER LATENT COOLING RATE" );
			AddRecordToOutputVariableStructure( "*", "ZONE WINDOW AIR CONDITIONER ELECTRIC POWER" );

		} else if ( reportName == "WATERHEATERREPORTMONTHLY" ) {
			AddRecordToOutputVariableStructure( "*", "WATER HEATER TOTAL DEMAND HEAT TRANSFER ENERGY" );
			AddRecordToOutputVariableStructure( "*", "WATER HEATER USE SIDE HEAT TRANSFER ENERGY" );
			AddRecordToOutputVariableStructure( "*", "WATER HEATER BURNER HEATING ENERGY" );
			AddRecordToOutputVariableStructure( "*", "WATER HEATER GAS CONSUMPTION" );
			AddRecordToOutputVariableStructure( "*", "WATER HEATER TOTAL DEMAND HEAT TRANSFER ENERGY" );
			AddRecordToOutputVariableStructure( "*", "WATER HEATER LOSS DEMAND ENERGY" );
			AddRecordToOutputVariableStructure( "*", "WATER HEATER HEAT LOSS ENERGY" );
			AddRecordToOutputVariableStructure( "*", "WATER HEATER TANK TEMPERATURE" );
			AddRecordToOutputVariableStructure( "*", "WATER HEATER HEAT RECOVERY SUPPLY ENERGY" );
			AddRecordToOutputVariableStructure( "*", "WATER HEATER SOURCE ENERGY" );

		} else if ( reportName == "GENERATORREPORTMONTHLY" ) {
			AddRecordToOutputVariableStructure( "*", "GENERATOR PRODUCED ELECTRIC ENERGY" );
			AddRecordToOutputVariableStructure( "*", "GENERATOR DIESEL CONSUMPTION" );
			AddRecordToOutputVariableStructure( "*", "GENERATOR GAS CONSUMPTION" );
			AddRecordToOutputVariableStructure( "*", "GENERATOR PRODUCED ELECTRIC ENERGY" );
			AddRecordToOutputVariableStructure( "*", "GENERATOR TOTAL HEAT RECOVERY" );
			AddRecordToOutputVariableStructure( "*", "GENERATOR JACKET HEAT RECOVERY ENERGY" );
			AddRecordToOutputVariableStructure( "*", "GENERATOR LUBE HEAT RECOVERY" );
			AddRecordToOutputVariableStructure( "*", "GENERATOR EXHAUST HEAT RECOVERY ENERGY" );
			AddRecordToOutputVariableStructure( "*", "GENERATOR EXHAUST AIR TEMPERATURE" );

		} else if ( reportName == "DAYLIGHTINGREPORTMONTHLY" ) {
			AddRecordToOutputVariableStructure( "*", "SITE EXTERIOR BEAM NORMAL ILLUMINANCE" );
			AddRecordToOutputVariableStructure( "*", "DAYLIGHTING LIGHTING POWER MULTIPLIER" );
			AddRecordToOutputVariableStructure( "*", "DAYLIGHTING LIGHTING POWER MULTIPLIER" );
			AddRecordToOutputVariableStructure( "*", "DAYLIGHTING REFERENCE POINT 1 ILLUMINANCE" );
			AddRecordToOutputVariableStructure( "*", "DAYLIGHTING REFERENCE POINT 1 GLARE INDEX" );
			AddRecordToOutputVariableStructure( "*",
												"DAYLIGHTING REFERENCE POINT 1 GLARE INDEX SETPOINT EXCEEDED TIME" );
			AddRecordToOutputVariableStructure( "*",
												"DAYLIGHTING REFERENCE POINT 1 DAYLIGHT ILLUMINANCE SETPOINT EXCEEDED TIME" );
			AddRecordToOutputVariableStructure( "*", "DAYLIGHTING REFERENCE POINT 2 ILLUMINANCE" );
			AddRecordToOutputVariableStructure( "*", "DAYLIGHTING REFERENCE POINT 2 GLARE INDEX" );
			AddRecordToOutputVariableStructure( "*",
												"DAYLIGHTING REFERENCE POINT 2 GLARE INDEX SETPOINT EXCEEDED TIME" );
			AddRecordToOutputVariableStructure( "*",
												"DAYLIGHTING REFERENCE POINT 2 DAYLIGHT ILLUMINANCE SETPOINT EXCEEDED TIME" );

		} else if ( reportName == "COILREPORTMONTHLY" ) {
			AddRecordToOutputVariableStructure( "*", "HEATING COIL HEATING ENERGY" );
			AddRecordToOutputVariableStructure( "*", "HEATING COIL HEATING RATE" );
			AddRecordToOutputVariableStructure( "*", "COOLING COIL SENSIBLE COOLING ENERGY" );
			AddRecordToOutputVariableStructure( "*", "COOLING COIL TOTAL COOLING ENERGY" );
			AddRecordToOutputVariableStructure( "*", "COOLING COIL TOTAL COOLING RATE" );
			AddRecordToOutputVariableStructure( "*", "COOLING COIL SENSIBLE COOLING RATE" );
			AddRecordToOutputVariableStructure( "*", "COOLING COIL WETTED AREA FRACTION" );

		} else if ( reportName == "PLANTLOOPDEMANDREPORTMONTHLY" ) {
			AddRecordToOutputVariableStructure( "*", "PLANT SUPPLY SIDE COOLING DEMAND RATE" );
			AddRecordToOutputVariableStructure( "*", "PLANT SUPPLY SIDE HEATING DEMAND RATE" );

		} else if ( reportName == "FANREPORTMONTHLY" ) {
			AddRecordToOutputVariableStructure( "*", "FAN ELECTRIC ENERGY" );
			AddRecordToOutputVariableStructure( "*", "FAN RISE IN AIR TEMPERATURE" );
			AddRecordToOutputVariableStructure( "*", "FAN ELECTRIC POWER" );

		} else if ( reportName == "PUMPREPORTMONTHLY" ) {
			AddRecordToOutputVariableStructure( "*", "PUMP ELECTRIC ENERGY" );
			AddRecordToOutputVariableStructure( "*", "PUMP FLUID HEAT GAIN ENERGY" );
			AddRecordToOutputVariableStructure( "*", "PUMP ELECTRIC POWER" );
			AddRecordToOutputVariableStructure( "*", "PUMP SHAFT POWER" );
			AddRecordToOutputVariableStructure( "*", "PUMP FLUID HEAT GAIN RATE" );
			AddRecordToOutputVariableStructure( "*", "PUMP OUTLET TEMPERATURE" );
			AddRecordToOutputVariableStructure( "*", "PUMP MASS FLOW RATE" );

		} else if ( reportName == "CONDLOOPDEMANDREPORTMONTHLY" ) {
			AddRecordToOutputVariableStructure( "*", "PLANT SUPPLY SIDE COOLING DEMAND RATE" );
			AddRecordToOutputVariableStructure( "*", "PLANT SUPPLY SIDE HEATING DEMAND RATE" );
			AddRecordToOutputVariableStructure( "*", "PLANT SUPPLY SIDE INLET TEMPERATURE" );
			AddRecordToOutputVariableStructure( "*", "PLANT SUPPLY SIDE OUTLET TEMPERATURE" );

		} else if ( reportName == "ZONETEMPERATUREOSCILLATIONREPORTMONTHLY" ) {
			AddRecordToOutputVariableStructure( "*", "ZONE OSCILLATING TEMPERATURES TIME" );
			AddRecordToOutputVariableStructure( "*", "ZONE PEOPLE OCCUPANT COUNT" );

		} else if ( reportName == "AIRLOOPSYSTEMENERGYANDWATERUSEMONTHLY" ) {
			AddRecordToOutputVariableStructure( "*", "AIR SYSTEM HOT WATER ENERGY" );
			AddRecordToOutputVariableStructure( "*", "AIR SYSTEM STEAM ENERGY" );
			AddRecordToOutputVariableStructure( "*", "AIR SYSTEM CHILLED WATER ENERGY" );
			AddRecordToOutputVariableStructure( "*", "AIR SYSTEM ELECTRIC ENERGY" );
			AddRecordToOutputVariableStructure( "*", "AIR SYSTEM GAS ENERGY" );
			AddRecordToOutputVariableStructure( "*", "AIR SYSTEM WATER VOLUME" );

		} else if ( reportName == "AIRLOOPSYSTEMCOMPONENTLOADSMONTHLY" ) {
			AddRecordToOutputVariableStructure( "*", "AIR SYSTEM FAN AIR HEATING ENERGY" );
			AddRecordToOutputVariableStructure( "*", "AIR SYSTEM COOLING COIL TOTAL COOLING ENERGY" );
			AddRecordToOutputVariableStructure( "*", "AIR SYSTEM HEATING COIL TOTAL HEATING ENERGY" );
			AddRecordToOutputVariableStructure( "*", "AIR SYSTEM HEAT EXCHANGER TOTAL HEATING ENERGY" );
			AddRecordToOutputVariableStructure( "*", "AIR SYSTEM HEAT EXCHANGER TOTAL COOLING ENERGY" );
			AddRecordToOutputVariableStructure( "*", "AIR SYSTEM HUMIDIFIER TOTAL HEATING ENERGY" );
			AddRecordToOutputVariableStructure( "*", "AIR SYSTEM EVAPORATIVE COOLER TOTAL COOLING ENERGY" );
			AddRecordToOutputVariableStructure( "*", "AIR SYSTEM DESICCANT DEHUMIDIFIER TOTAL COOLING ENERGY" );

		} else if ( reportName == "AIRLOOPSYSTEMCOMPONENTENERGYUSEMONTHLY" ) {
			AddRecordToOutputVariableStructure( "*", "AIR SYSTEM FAN ELECTRIC ENERGY" );
			AddRecordToOutputVariableStructure( "*", "AIR SYSTEM HEATING COIL HOT WATER ENERGY" );
			AddRecordToOutputVariableStructure( "*", "AIR SYSTEM COOLING COIL CHILLED WATER ENERGY" );
			AddRecordToOutputVariableStructure( "*", "AIR SYSTEM DX HEATING COIL ELECTRIC ENERGY" );
			AddRecordToOutputVariableStructure( "*", "AIR SYSTEM DX COOLING COIL ELECTRIC ENERGY" );
			AddRecordToOutputVariableStructure( "*", "AIR SYSTEM HEATING COIL ELECTRIC ENERGY" );
			AddRecordToOutputVariableStructure( "*", "AIR SYSTEM HEATING COIL GAS ENERGY" );
			AddRecordToOutputVariableStructure( "*", "AIR SYSTEM HEATING COIL STEAM ENERGY" );
			AddRecordToOutputVariableStructure( "*", "AIR SYSTEM HUMIDIFIER ELECTRIC ENERGY" );
			AddRecordToOutputVariableStructure( "*", "AIR SYSTEM EVAPORATIVE COOLER ELECTRIC ENERGY" );
			AddRecordToOutputVariableStructure( "*", "AIR SYSTEM DESICCANT DEHUMIDIFIER ELECTRIC ENERGY" );

		} else if ( reportName == "MECHANICALVENTILATIONLOADSMONTHLY" ) {
			AddRecordToOutputVariableStructure( "*", "ZONE MECHANICAL VENTILATION NO LOAD HEAT REMOVAL ENERGY" );
			AddRecordToOutputVariableStructure( "*", "ZONE MECHANICAL VENTILATION COOLING LOAD INCREASE ENERGY" );
			AddRecordToOutputVariableStructure( "*",
												"ZONE MECHANICAL VENTILATION COOLING LOAD INCREASE DUE TO OVERHEATING ENERGY" );
			AddRecordToOutputVariableStructure( "*", "ZONE MECHANICAL VENTILATION COOLING LOAD DECREASE ENERGY" );
			AddRecordToOutputVariableStructure( "*", "ZONE MECHANICAL VENTILATION NO LOAD HEAT ADDITION ENERGY" );
			AddRecordToOutputVariableStructure( "*", "ZONE MECHANICAL VENTILATION HEATING LOAD INCREASE ENERGY" );
			AddRecordToOutputVariableStructure( "*",
												"ZONE MECHANICAL VENTILATION HEATING LOAD INCREASE DUE TO OVERCOOLING ENERGY" );
			AddRecordToOutputVariableStructure( "*", "ZONE MECHANICAL VENTILATION HEATING LOAD DECREASE ENERGY" );
			AddRecordToOutputVariableStructure( "*", "ZONE MECHANICAL VENTILATION AIR CHANGES PER HOUR" );

		} else {

		}

	}

	void
	InputProcessor::AddRecordToOutputVariableStructure(
		std::string const & KeyValue,
		std::string const & VariableName
	) {
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   July 2010
		//       MODIFIED       March 2017
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine adds a new record (if necessary) to the Output Variable
		// reporting structure.  DataOutputs, OutputVariablesForSimulation

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		std::string::size_type vnameLen; // if < length, there were units on the line/name

		std::string::size_type const rbpos = index( VariableName, '[' );
		if ( rbpos == std::string::npos ) {
			vnameLen = len_trim( VariableName );
		} else {
			vnameLen = len_trim( VariableName.substr( 0, rbpos ) );
		}
		std::string const VarName( VariableName.substr( 0, vnameLen ) );

		auto const found = DataOutputs::OutputVariablesForSimulation.find( VarName );
		if ( found == DataOutputs::OutputVariablesForSimulation.end() ) {
			std::unordered_map< std::string, DataOutputs::OutputReportingVariables > data;
			data.reserve( 32 );
			data.emplace( KeyValue, DataOutputs::OutputReportingVariables( KeyValue, VarName ) );
			DataOutputs::OutputVariablesForSimulation.emplace( VarName, std::move( data ) );
		} else {
			found->second.emplace( KeyValue, DataOutputs::OutputReportingVariables( KeyValue, VarName ) );
		}
		DataOutputs::NumConsideredOutputVariables++;
	}

//void
//ShowAuditErrorMessage(
//		std::string const & Severity, // if blank, does not add to sum
//		std::string const & ErrorMessage
//		)
//{
//
//	// SUBROUTINE INFORMATION:
//	//       AUTHOR         Linda K. Lawrie
//	//       DATE WRITTEN   March 2003
//	//       MODIFIED       na
//	//       RE-ENGINEERED  na
//
//	// PURPOSE OF THIS SUBROUTINE:
//	// This subroutine is just for messages that will be displayed on the audit trail
//	// (echo of the input file).  Errors are counted and a summary is displayed after
//	// finishing the scan of the input file.
//
//	// SUBROUTINE PARAMETER DEFINITIONS:
//	static gio::Fmt ErrorFormat( "(2X,A)" );
//
//	if ( ! Severity.empty() ) {
//		++TotalAuditErrors;
//		gio::write( EchoInputFile, ErrorFormat ) << Severity + ErrorMessage;
//	} else {
//		gio::write( EchoInputFile, ErrorFormat ) << " ************* " + ErrorMessage;
//	}
//
//}

	std::string
	InputProcessor::IPTrimSigDigits( int const IntegerValue ) {

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   March 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function accepts a number as parameter as well as the number of
		// significant digits after the decimal point to report and returns a string
		// that is appropriate.

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		std::string String; // Working string

		gio::write( String, fmtLD ) << IntegerValue;
		return stripped( String );

	}
}
