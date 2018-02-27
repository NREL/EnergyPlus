// EnergyPlus, Copyright (c) 1996-2018, The Board of Trustees of the University of Illinois,
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy), Oak Ridge
// National Laboratory, managed by UT-Battelle, Alliance for Sustainable Energy, LLC, and other
// contributors. All rights reserved.
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

// ObjexxFCL Headers

// EnergyPlus Headers
#include <InputProcessing/InputValidation.hh>
#include <milo/dtoa.hpp>
#include <milo/itoa.hpp>

using json = nlohmann::json;

#ifndef UNUSED
#define UNUSED( expr )
#endif

void State::initialize( json const * parsed_schema ) {
	stack.clear();
	schema = parsed_schema;
	stack.push_back( schema );
	json const & loc = stack.back()->at( "required" );
	for ( auto & s : loc ) root_required.emplace( s.get < std::string >(), false );
}

void State::addError( ErrorType err, double val, unsigned line_num, unsigned line_index ) {
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

std::vector < std::string > const & State::validationErrors() {
	return errors;
}

std::vector < std::string > const & State::validationWarnings() {
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

void State::validate( json & parsed, unsigned line_num, unsigned UNUSED( line_index ) ) {
	auto const * loc = stack.back();

	if ( loc->find( "enum" ) != loc->end() ) {
		size_t i;
		auto const & enum_array = loc->at( "enum" );
		auto const enum_array_size = enum_array.size();
		if ( parsed.is_string() ) {
			auto const & parsed_string = parsed.get < std::string >();
			for ( i = 0; i < enum_array_size; i++ ) {
				auto const & enum_string = enum_array[ i ].get< std::string >();
				// TODO: Was case-insensitive and may need to change back
				if ( enum_string == parsed_string ) break;
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
				addError( State::ErrorType::ExclusiveMinimum, val, line_num, prev_line_index + prev_key_len );
			} else if ( val < min_val ) {
				addError( State::ErrorType::Minimum, val, line_num, prev_line_index + prev_key_len );
			}
		}
		auto const found_max = loc->find( "maximum" );
		if ( found_max != loc->end() ) {
			double const max_val = found_max->get < double >();
			if ( loc->find( "exclusiveMaximum" ) != loc->end() && val >= max_val ) {
				addError( State::ErrorType::ExclusiveMaximum, val, line_num, prev_line_index + prev_key_len );
			} else if ( val > max_val ) {
				addError( State::ErrorType::Maximum, val, line_num, prev_line_index + prev_key_len );
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
