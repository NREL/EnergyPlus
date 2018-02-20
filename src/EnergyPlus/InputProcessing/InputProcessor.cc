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
#include <algorithm>
#include <istream>
#include <iostream>
#include <fstream>
#include <unordered_set>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1S.hh>

// EnergyPlus Headers
#include <InputProcessing/InputProcessor.hh>
#include <InputProcessing/IdfParser.hh>
#include <InputProcessing/InputValidation.hh>
#include <InputProcessing/DataStorage.hh>
#include <InputProcessing/EmbeddedEpJSONSchema.hh>
#include <DataIPShortCuts.hh>
#include <DataOutputs.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DataStringGlobals.hh>
#include <DataSystemVariables.hh>
#include <DisplayRoutines.hh>
#include <SortAndStringUtilities.hh>
#include <FileSystem.hh>
#include <UtilityRoutines.hh>
#include <milo/dtoa.hpp>
#include <milo/itoa.hpp>

namespace EnergyPlus {
// Module containing the input processor routines

// MODULE INFORMATION:
//       AUTHOR         Linda K. Lawrie
//       DATE WRITTEN   August 1997
//       MODIFIED       na
//       RE-ENGINEERED  Mark Adams 2017

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

	static std::string const BlankString;

	using json = nlohmann::json;

	std::unique_ptr< InputProcessor > inputProcessor = nullptr;

	InputProcessor::InputProcessor() :
		idf_parser( std::unique_ptr< IdfParser >( new IdfParser() ) ),
		state( std::unique_ptr< State >( new State() ) ),
		data( std::unique_ptr< DataStorage >( new DataStorage() ) )
	{
		auto const embeddedEpJSONSchema = EmbeddedEpJSONSchema::embeddedEpJSONSchema();
		schema = json::from_cbor( embeddedEpJSONSchema.first, embeddedEpJSONSchema.second );

		const json & loc = schema[ "properties" ];
		caseInsensitiveObjectMap.reserve( loc.size() );
		for ( auto it = loc.begin(); it != loc.end(); ++it ) {
			caseInsensitiveObjectMap.emplace( convertToUpper( it.key() ), it.key() );
		}

		state->initialize( & schema );

		const auto state_ptr = state.get();
		callback = [ state_ptr ]( int EP_UNUSED( depth ), json::parse_event_t event, json &parsed ) -> bool {
			state_ptr->traverse( event, parsed, 999999999, 999999999 );
			return true;
		};
	}

	std::unique_ptr< InputProcessor > InputProcessor::factory()
	{
		auto ret = std::unique_ptr< InputProcessor >( new InputProcessor() );
		return ret;
	}

	json const &
	InputProcessor::getFields( std::string const & objectType, std::string const & objectName )
	{
		auto const it = epJSON.find( objectType );
		if ( it == epJSON.end() ) {
			ShowFatalError( "ObjectType (" + objectType + ") requested was not found in input" );
		}
		auto const & objs = it.value();
		auto const it2 = objs.find( objectName );
		if ( it2 == objs.end() ) {
			// HACK: this is not ideal and should be removed once everything is case sensitive internally
			for ( auto it3 = objs.begin(); it3 != objs.end(); ++it3 ) {
				if ( UtilityRoutines::MakeUPPERCase( it3.key() ) == objectName ) {
					return it3.value();
				}
			}
			ShowFatalError( "Name \"" + objectName + "\" requested was not found in input for ObjectType (" + objectType + ")"  );
		}
		return it2.value();
	}

	json const &
	InputProcessor::getFields( std::string const & objectType )
	{
		static const std::string blankString;
		auto const it = epJSON.find( objectType );
		if ( it == epJSON.end() ) {
			ShowFatalError( "ObjectType (" + objectType + ") requested was not found in input" );
		}
		auto const & objs = it.value();
		auto const it2 = objs.find( blankString );
		if ( it2 == objs.end() ) {
			ShowFatalError( "Name \"\" requested was not found in input for ObjectType (" + objectType + ")"  );
		}
		return it2.value();
	}

// Functions

	void
	InputProcessor::clear_state()
	{
		idf_parser = std::unique_ptr< IdfParser >( new IdfParser() );
		data = std::unique_ptr< DataStorage >( new DataStorage() );
		epJSON = json::object();
		objectCacheMap.clear();
		unusedInputs.clear();

		state = std::unique_ptr< State >( new State() );
		state->initialize( & schema );
		const auto state_ptr = state.get();
		callback = [ state_ptr ]( int EP_UNUSED( depth ), json::parse_event_t event, json &parsed ) -> bool {
			state_ptr->traverse( event, parsed, 999999999, 999999999 );
			return true;
		};
	}

	std::vector < std::string > const & InputProcessor::validationErrors() {
		return state->validationErrors();
	}

	std::vector < std::string > const & InputProcessor::validationWarnings() {
		return state->validationWarnings();
	}

	std::pair< bool, std::string >
	InputProcessor::convertInsensitiveObjectType( std::string const & objectType ) {
		auto tmp_umit = caseInsensitiveObjectMap.find( convertToUpper( objectType ) );
		if ( tmp_umit != caseInsensitiveObjectMap.end() ) {
			return std::make_pair( true, tmp_umit->second );
		}
		return std::make_pair( false, "" );
	}

	void
	InputProcessor::initializeMaps() {
		unusedInputs.clear();
		objectCacheMap.clear();
		objectCacheMap.reserve( epJSON.size() );
		auto const & schema_properties = schema.at( "properties" );

		for ( auto epJSON_iter = epJSON.begin(); epJSON_iter != epJSON.end(); ++epJSON_iter ) {
			auto const & objects = epJSON_iter.value();
			auto const & objectType = epJSON_iter.key();
			ObjectCache objectCache;
			objectCache.inputObjectIterators.reserve( objects.size() );
			for ( auto epJSON_obj_iter = objects.begin(); epJSON_obj_iter != objects.end(); ++epJSON_obj_iter ) {
				objectCache.inputObjectIterators.emplace_back( epJSON_obj_iter );
				auto const * const obj_ptr = epJSON_obj_iter.value().get_ptr< const json::object_t * const >();
				unusedInputs.emplace( obj_ptr, ObjectInfo( objectType, epJSON_obj_iter.key() ) );
			}
			auto const schema_iter = schema_properties.find( objectType );
			objectCache.schemaIterator = schema_iter;
			objectCacheMap.emplace( schema_iter.key(), objectCache );
		}
	}

	void
	InputProcessor::processInput() {
		std::ifstream input_stream( DataStringGlobals::inputFileName , std::ifstream::in );
		if ( !input_stream.is_open() ) {
			ShowFatalError( "Input file path " + DataStringGlobals::inputFileName + " not found" );
			return;
		}

		std::string input_file;
		std::string line;
		while (std::getline(input_stream, line))
		{
			input_file.append(line + DataStringGlobals::NL);
		}
		// For some reason this does not work properly on Windows. This will be faster so should investigate in future.
		// std::ifstream::pos_type size = input_stream.tellg();
		// char *memblock = new char[(size_t) size + 1];
		// input_stream.seekg(0, std::ios::beg);
		// input_stream.read(memblock, size);
		// memblock[size] = '\0';
		// input_stream.close();
		// std::string input_file = memblock;
		// delete[] memblock;

		// Potential C approach to reading file
		// std::vector<char> v;
		// if (FILE *fp = fopen("filename", "r"))
		// {
		// 	char buf[1024];
		// 	while (size_t len = fread(buf, 1, sizeof(buf), fp))
		// 		v.insert(v.end(), buf, buf + len);
		// 	fclose(fp);
		// }

		if (input_file.empty()) {
			ShowFatalError("Failed to read input file: " + DataStringGlobals::inputFileName);
			return;
		}

		if ( ! DataGlobals::isEpJSON ) {
			json const input_file_json = idf_parser->decode( input_file, schema );
			if ( DataGlobals::outputEpJSONConversion ) {
				input_file = input_file_json.dump( 4 );
				std::string convertedIDF( DataStringGlobals::outputDirPathName + DataStringGlobals::inputFileNameOnly + ".epJSON" );
				FileSystem::makeNativePath( convertedIDF );
				std::ofstream convertedFS( convertedIDF, std::ofstream::out );
				convertedFS << input_file << std::endl;
			} else {
				input_file = input_file_json.dump( 4 );
			}
		}

		epJSON = json::parse( input_file );

		if ( DataGlobals::isEpJSON && DataGlobals::outputEpJSONConversion ) {
			std::string const encoded = idf_parser->encode( epJSON, schema );
			std::string convertedEpJSON( DataStringGlobals::outputDirPathName + DataStringGlobals::inputFileNameOnly + ".idf" );
			FileSystem::makeNativePath( convertedEpJSON );
			std::ofstream convertedFS( convertedEpJSON, std::ofstream::out );
			convertedFS << encoded << std::endl;
		}

		auto const num_errors = state->validationErrors().size();
		// TODO: Need to add error outputting for idf parsing and validation
		if ( num_errors ) {
			ShowFatalError( "Errors occurred on processing input file. Preceding condition(s) cause termination." );
		}

		initializeMaps();

		int MaxArgs = 0;
		int MaxAlpha = 0;
		int MaxNumeric = 0;
		getMaxSchemaArgs( MaxArgs, MaxAlpha, MaxNumeric );

		DataIPShortCuts::cAlphaFieldNames.allocate( MaxAlpha );
		DataIPShortCuts::cAlphaArgs.allocate( MaxAlpha );
		DataIPShortCuts::lAlphaFieldBlanks.dimension( MaxAlpha, false );
		DataIPShortCuts::cNumericFieldNames.allocate( MaxNumeric );
		DataIPShortCuts::rNumericArgs.dimension( MaxNumeric, 0.0 );
		DataIPShortCuts::lNumericFieldBlanks.dimension( MaxNumeric, false );
	}

	int
	InputProcessor::getNumSectionsFound( std::string const & SectionWord ) {
		// PURPOSE OF THIS SUBROUTINE:
		// This function returns the number of a particular section (in input data file)
		// found in the current run.  If it can't find the section in list
		// of sections, a -1 will be returned.

		// METHODOLOGY EMPLOYED:
		// Look up section in list of sections.  If there, return the
		// number of sections of that kind found in the current input.  If not, return -1.

		auto const & SectionWord_iter = epJSON.find( SectionWord );
		if ( SectionWord_iter == epJSON.end() ) return -1;
		return static_cast< int >( SectionWord_iter.value().size() );
	}


	int
	InputProcessor::getNumObjectsFound( std::string const & ObjectWord ) {

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   September 1997
		//       MODIFIED       Mark Adams
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This function returns the number of objects (in input data file)
		// found in the current run.  If it can't find the object in list
		// of objects, a 0 will be returned.

		// METHODOLOGY EMPLOYED:
		// Look up object in list of objects.  If there, return the
		// number of objects found in the current input.  If not, return 0.

		auto const & find_obj = epJSON.find( ObjectWord );

		if ( find_obj == epJSON.end() ) {
			auto tmp_umit = caseInsensitiveObjectMap.find( convertToUpper( ObjectWord ) );
			if ( tmp_umit == caseInsensitiveObjectMap.end() || epJSON.find( tmp_umit->second ) == epJSON.end() ) {
				return 0;
			}
			return static_cast< int >( epJSON[ tmp_umit->second ].size() );
		} else {
			return static_cast< int >( find_obj.value().size() );
		}

		if ( schema[ "properties" ].find( ObjectWord ) == schema[ "properties" ].end() ) {
			auto tmp_umit = caseInsensitiveObjectMap.find( convertToUpper( ObjectWord ) );
			if ( tmp_umit == caseInsensitiveObjectMap.end() ) {
				ShowWarningError( "Requested Object not found in Definitions: " + ObjectWord );
			}
		}
		return 0;
	}

	void
	InputProcessor::getObjectItem(
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

		int adjustedNumber = getJSONObjNum( Object, Number ); // if incoming input is idf, then use idf object order

		auto find_iterators = objectCacheMap.find( Object );
		// auto sorted_iterators = find_iterators;


		if ( find_iterators == objectCacheMap.end() ) {
			auto const tmp_umit = caseInsensitiveObjectMap.find( convertToUpper( Object ) );
			if ( tmp_umit == caseInsensitiveObjectMap.end() || epJSON.find( tmp_umit->second ) == epJSON.end() ) {
				return;
			}
			find_iterators = objectCacheMap.find( tmp_umit->second );
		}

		NumAlphas = 0;
		NumNumbers = 0;
		Status = -1;
		auto const & is_AlphaBlank = present(AlphaBlank);
		auto const & is_AlphaFieldNames = present(AlphaFieldNames);
		auto const & is_NumBlank = present(NumBlank);
		auto const & is_NumericFieldNames = present(NumericFieldNames);

		auto const & epJSON_it = find_iterators->second.inputObjectIterators.at( adjustedNumber - 1 );
		auto const & epJSON_schema_it = find_iterators->second.schemaIterator;
		auto const & epJSON_schema_it_val = epJSON_schema_it.value();

		auto const * const obj_ptr = epJSON_it.value().get_ptr< const json::object_t * const >();
		auto const find_unused = unusedInputs.find( obj_ptr );
		if ( find_unused != unusedInputs.end() ) {
			unusedInputs.erase( find_unused );
		}

		// Locations in JSON schema relating to normal fields
		auto const & schema_obj_props = epJSON_schema_it_val[ "patternProperties" ][ ".*" ][ "properties" ];

		// Locations in JSON schema storing the positional aspects from the IDD format, legacy prefixed
		auto const & legacy_idd = epJSON_schema_it_val[ "legacy_idd" ];
		auto const & legacy_idd_field_names = legacy_idd[ "field_names" ];
		auto const & legacy_idd_alphas = legacy_idd[ "alphas" ];
		auto const & legacy_idd_numerics = legacy_idd[ "numerics" ];
		auto const & schema_name_field = epJSON_schema_it_val.find( "name" );

		auto key = legacy_idd.find("extension");
		std::string extension_key;
		if ( key != legacy_idd.end() ) {
			extension_key = key.value();
		}

		Alphas = "";
		Numbers = 0;
		if ( is_NumBlank ) {
			NumBlank() = true;
		}
		if ( is_AlphaBlank ) {
			AlphaBlank() = true;
		}
		if ( is_AlphaFieldNames ) {
			AlphaFieldNames() = "";
		}
		if ( is_NumericFieldNames ) {
			NumericFieldNames() = "";
		}

		auto const & obj = epJSON_it;
		auto const & obj_val = obj.value();
		auto const & legacy_idd_alphas_fields = legacy_idd_alphas[ "fields" ];
		// int idfObjCount = obj_val[ "idfOrder"];
		for ( size_t i = 0; i < legacy_idd_alphas_fields.size(); ++i ) {
			std::string const & field = legacy_idd_alphas_fields[ i ];
			if ( field == "name" && schema_name_field != epJSON_schema_it_val.end() ) {
				auto const & name_iter = schema_name_field.value();
				if ( name_iter.find( "retaincase" ) != name_iter.end() ) {
					Alphas( i + 1 ) = obj.key();
				} else {
					Alphas( i + 1 ) = UtilityRoutines::MakeUPPERCase( obj.key() );
					// Alphas( i + 1 ) = obj.key();
				}
				if ( is_AlphaBlank ) AlphaBlank()( i + 1 ) = obj.key().empty();
				if ( is_AlphaFieldNames ) AlphaFieldNames()( i + 1 ) = (DataGlobals::isEpJSON) ? field : legacy_idd_field_names[field].get<std::string>();
				NumAlphas++;
				continue;
			}
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
						// Alphas( i + 1 ) = val;
						Alphas( i + 1 ) = UtilityRoutines::MakeUPPERCase( val );
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
			auto const epJSON_extensions_array_itr = obj.value().find( extension_key );
			if ( epJSON_extensions_array_itr != obj.value().end() ) {
				auto const & legacy_idd_alphas_extensions = legacy_idd_alphas_extension_iter.value();
				auto const & epJSON_extensions_array = epJSON_extensions_array_itr.value();
				auto const & schema_extension_fields = schema_obj_props[ extension_key ][ "items" ][ "properties" ];
				int alphas_index = static_cast <int> ( legacy_idd_alphas_fields.size() );

				for ( auto it = epJSON_extensions_array.begin(); it != epJSON_extensions_array.end(); ++it ) {
					auto const & epJSON_extension_obj = it.value();

					for ( size_t i = 0; i < legacy_idd_alphas_extensions.size(); i++ ) {
						std::string const & field_name = legacy_idd_alphas_extensions[ i ];
						auto const & epJSON_obj_field_iter = epJSON_extension_obj.find( field_name );

						if ( epJSON_obj_field_iter != epJSON_extension_obj.end() ) {
							auto const & epJSON_field_val = epJSON_obj_field_iter.value();
							if ( epJSON_field_val.is_string() ) {
								auto const & schema_field = schema_extension_fields[ field_name ];
								std::string val = epJSON_field_val;
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
									// Alphas( alphas_index + 1 ) = val;
									Alphas( alphas_index + 1 ) = UtilityRoutines::MakeUPPERCase( val );
								}
							} else {
								if ( epJSON_field_val.is_number_integer() ) {
									i64toa( epJSON_field_val.get < int >(), s );
								} else {
									dtoa( epJSON_field_val.get < double >(), s );
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
			if ( is_NumericFieldNames ) NumericFieldNames()( i + 1 ) = (DataGlobals::isEpJSON) ? field : legacy_idd_field_names[field].get<std::string>();
		}

		auto const legacy_idd_numerics_extension_iter = legacy_idd_numerics.find( "extensions" );
		auto const epJSON_extensions_iter = obj.value().find( extension_key );
		if ( legacy_idd_numerics_extension_iter != legacy_idd_numerics.end() && epJSON_extensions_iter != obj.value().end() ) {
			auto const & legacy_idd_numerics_extensions = legacy_idd_numerics_extension_iter.value();
			auto const & schema_extension_fields = schema_obj_props[ extension_key ][ "items" ][ "properties" ];
			// auto const & epJSON_extensions_array = obj.value()[ extension_key ];
			auto const & epJSON_extensions_array = epJSON_extensions_iter.value();
			int numerics_index = static_cast <int> ( legacy_idd_numerics_fields.size() );

			for ( auto it = epJSON_extensions_array.begin(); it != epJSON_extensions_array.end(); ++it ) {
				auto const & epJSON_extension_obj = it.value();

				for ( size_t i = 0; i < legacy_idd_numerics_extensions.size(); i++ ) {
					std::string const & field = legacy_idd_numerics_extensions[ i ];
					auto const & epJSON_extension_field_iter = epJSON_extension_obj.find( field );

					if ( epJSON_extension_field_iter != epJSON_extension_obj.end() ) {
						auto const & val = epJSON_extension_field_iter.value();
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
	InputProcessor::getIDFObjNum(
		std::string const & Object,
		int const Number
	)
	{
		// Given the number (index) of an object in JSON order, return it's number in original idf order

		// Only applicable if the incoming file was idf
		int idfOrderNumber = Number;
		if ( DataGlobals::isEpJSON ) return idfOrderNumber;

		json * obj;
		auto obj_iter = epJSON.find( Object );
		if ( obj_iter == epJSON.end() ) {
			auto tmp_umit = caseInsensitiveObjectMap.find( convertToUpper( Object ) );
			if ( tmp_umit == caseInsensitiveObjectMap.end() ) {
				return idfOrderNumber;
			}
			obj = &epJSON[ tmp_umit->second ];
		} else {
			obj = &( obj_iter.value() );
		}

		std::vector< int > idfObjNums;
		std::vector< int > idfObjNumsSorted;

		// get list of saved object numbers from idf processing
		for ( auto it = obj->begin(); it != obj->end(); ++it ) {
			int objNum = it.value()[ "idfOrder" ];
			idfObjNums.emplace_back( objNum );
		}

		idfObjNumsSorted = idfObjNums;
		std::sort( idfObjNumsSorted.begin(), idfObjNumsSorted.end() );

		// find matching object number in unsorted list
		int targetIdfObjNum = idfObjNums[ Number - 1 ];
		for ( size_t i = 1; i <= idfObjNums.size(); ++i ) {
			if (idfObjNumsSorted[ i - 1 ] == targetIdfObjNum ) {
				idfOrderNumber = i;
				break;
			}
		}
		return idfOrderNumber;
	}

	int
	InputProcessor::getJSONObjNum(
		std::string const & Object,
		int const Number
	)
	{
		// Given the number (index) of an object in original idf order, return it's number in JSON order

		// Only applicable if the incoming file was idf
		int jSONOrderNumber = Number;
		if ( DataGlobals::isEpJSON ) return jSONOrderNumber;

		json * obj;
		auto obj_iter = epJSON.find( Object );
		if ( obj_iter == epJSON.end() ) {
			auto tmp_umit = caseInsensitiveObjectMap.find( convertToUpper( Object ) );
			if ( tmp_umit == caseInsensitiveObjectMap.end() ) {
				return jSONOrderNumber;
			}
			obj = &epJSON[ tmp_umit->second ];
		} else {
			obj = &( obj_iter.value() );
		}

		std::vector< int > idfObjNums;
		std::vector< int > idfObjNumsSorted;

		// get list of saved object numbers from idf processing
		for ( auto it = obj->begin(); it != obj->end(); ++it ) {
			int objNum = it.value()[ "idfOrder" ];
			idfObjNums.emplace_back( objNum );
		}

		idfObjNumsSorted = idfObjNums;
		std::sort( idfObjNumsSorted.begin(), idfObjNumsSorted.end() );

		// find matching object number in unsorted list
		int targetIdfObjNum = idfObjNumsSorted[ Number - 1 ];
		for ( size_t i = 1; i <= idfObjNums.size(); ++i ) {
			if (idfObjNums[ i - 1 ] == targetIdfObjNum ) {
				jSONOrderNumber = i;
				break;
			}
		}
		return jSONOrderNumber;
	}

	int
	InputProcessor::getObjectItemNum(
		std::string const & ObjType, // Object Type (ref: IDD Objects)
		std::string const & ObjName // Name of the object type
	) {
		// PURPOSE OF THIS SUBROUTINE:
		// Get the occurrence number of an object of type ObjType and name ObjName

		json * obj;
		auto obj_iter = epJSON.find( ObjType );
		if ( obj_iter == epJSON.end() || obj_iter.value().find( ObjName ) == obj_iter.value().end() ) {
			auto tmp_umit = caseInsensitiveObjectMap.find( convertToUpper( ObjType ) );
			if ( tmp_umit == caseInsensitiveObjectMap.end() ) {
				return -1;
			}
			obj = &epJSON[ tmp_umit->second ];
		} else {
			obj = &( obj_iter.value() );
		}

		int object_item_num = 1;
		bool found = false;
		auto const upperObjName = UtilityRoutines::MakeUPPERCase( ObjName );
		for ( auto it = obj->begin(); it != obj->end(); ++it ) {
			if ( UtilityRoutines::MakeUPPERCase( it.key() ) == upperObjName ) {
				found = true;
				break;
			}
			object_item_num++;
		}

		if ( !found ) {
			// ShowWarningError("Didn't find name, need to probably use search key");
			return -1;
		}
		return 	getIDFObjNum( ObjType, object_item_num ); // if incoming input is idf, then return idf object order
	}


	int
	InputProcessor::getObjectItemNum(
		std::string const & ObjType, // Object Type (ref: IDD Objects)
		std::string const & NameTypeVal, // Object "name" field type ( used as search key )
		std::string const & ObjName // Name of the object type
	) {
		// PURPOSE OF THIS SUBROUTINE:
		// Get the occurrence number of an object of type ObjType and name ObjName

		json * obj;
		auto obj_iter = epJSON.find( ObjType );
		if ( epJSON.find( ObjType ) == epJSON.end() || obj_iter.value().find( ObjName ) == obj_iter.value().end() ) {
			auto tmp_umit = caseInsensitiveObjectMap.find( convertToUpper( ObjType ) );
			if ( tmp_umit == caseInsensitiveObjectMap.end() ) {
				return -1;
			}
			obj = &epJSON[ tmp_umit->second ];
		} else {
			obj = &( obj_iter.value() );
		}

		int object_item_num = 1;
		bool found = false;
		auto const upperObjName = UtilityRoutines::MakeUPPERCase( ObjName );
		for ( auto it = obj->begin(); it != obj->end(); ++it ) {
			auto it2 = it.value().find(NameTypeVal);

			if ( ( it2 != it.value().end() ) && ( UtilityRoutines::MakeUPPERCase( it2.value() ) == upperObjName ) ) {
				found = true;
				break;
			}
			object_item_num++;
		}


		if ( !found ) {
			// ShowWarningError("Didn't find name, need to probably use search key");
			return -1;
		}
		return 	getIDFObjNum( ObjType, object_item_num ); // if incoming input is idf, then return idf object order
	}

	void
	InputProcessor::rangeCheck(
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
	InputProcessor::getMaxSchemaArgs(
		int & NumArgs,
		int & NumAlpha,
		int & NumNumeric
	) {
		NumArgs = 0;
		NumAlpha = 0;
		NumNumeric = 0;
		std::string extension_key;
		auto const & schema_properties = schema.at( "properties" );

		for ( json::iterator object = epJSON.begin(); object != epJSON.end(); ++object ) {
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
	InputProcessor::getObjectDefMaxArgs(
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
			auto tmp_umit = caseInsensitiveObjectMap.find( convertToUpper( ObjectWord ) );
			if ( tmp_umit == caseInsensitiveObjectMap.end() ) {
				ShowSevereError( "getObjectDefMaxArgs: Did not find object=\"" + ObjectWord + "\" in list of objects." );
				return;
			}
			object = &schema[ "properties" ][ tmp_umit->second ];
		} else {
			object = &schema[ "properties" ][ ObjectWord ];
		}
		const json & legacy_idd = object->at( "legacy_idd" );

		json * objects;
		if ( epJSON.find( ObjectWord ) == epJSON.end() ) {
			auto tmp_umit = caseInsensitiveObjectMap.find( convertToUpper( ObjectWord ) );
			if ( tmp_umit == caseInsensitiveObjectMap.end() ) {
				ShowSevereError( "getObjectDefMaxArgs: Did not find object=\"" + ObjectWord + "\" in list of objects." );
				return;
			}
			objects = &epJSON[ tmp_umit->second ];
		} else {
			objects = &epJSON[ ObjectWord ];
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
	InputProcessor::reportOrphanRecordObjects()
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
		unused_object_types.reserve( unusedInputs.size() );

		if ( unusedInputs.size() && DataGlobals::DisplayUnusedObjects ) {
			ShowWarningError( "The following lines are \"Unused Objects\".  These objects are in the input" );
			ShowContinueError( " file but are never obtained by the simulation and therefore are NOT used." );
			if ( ! DataGlobals::DisplayAllWarnings ) {
				ShowContinueError( " Only the first unused named object of an object class is shown.  Use Output:Diagnostics,DisplayAllWarnings; to see all." );
			} else {
				ShowContinueError( " Each unused object is shown." );
			}
			ShowContinueError( " See InputOutputReference document for more details." );
		}

		bool first_iteration = true;
		for ( auto it = unusedInputs.begin(); it != unusedInputs.end(); ++it ) {
			auto const & object_type = it->second.objectType;
			auto const & name = it->second.objectName;

			// there are some orphans that we are deeming as special, in that they should be warned in detail even if !DisplayUnusedObjects and !DisplayAllWarnings
			if ( has_prefix( object_type, "ZoneHVAC:" ) ) {
				ShowSevereError( "Orphaned ZoneHVAC object found.  This was object never referenced in the input, and was not used." );
				ShowContinueError( " -- Object type: " + object_type );
				ShowContinueError( " -- Object name: " + name );
			}

			if ( ! DataGlobals::DisplayUnusedObjects ) continue;

			if ( ! DataGlobals::DisplayAllWarnings ) {
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

		if ( unusedInputs.size() && ! DataGlobals::DisplayUnusedObjects ) {
			u64toa( unusedInputs.size(), s );
			ShowMessage( "There are " + std::string( s ) + " unused objects in input." );
			ShowMessage( "Use Output:Diagnostics,DisplayUnusedObjects; to see them." );
		}

	}

	void
	InputProcessor::preProcessorCheck( bool & PreP_Fatal ) // True if a preprocessor flags a fatal error
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

		int NumAlphas; // Used to retrieve names from IDF
		int NumNumbers; // Used to retrieve rNumericArgs from IDF
		int IOStat; // Could be used in the Get Routines, not currently checked
		int NumParams; // Total Number of Parameters in 'Output:PreprocessorMessage' Object
		int NumPrePM; // Number of Preprocessor Message objects in IDF
		int CountP;
		int CountM;
		std::string Multiples;

		DataIPShortCuts::cCurrentModuleObject = "Output:PreprocessorMessage";
		NumPrePM = getNumObjectsFound( DataIPShortCuts::cCurrentModuleObject );
		if ( NumPrePM > 0 ) {
			getObjectDefMaxArgs( DataIPShortCuts::cCurrentModuleObject, NumParams, NumAlphas, NumNumbers );
			DataIPShortCuts::cAlphaArgs( { 1, NumAlphas } ) = BlankString;
			for ( CountP = 1; CountP <= NumPrePM; ++CountP ) {
				getObjectItem( DataIPShortCuts::cCurrentModuleObject, CountP, DataIPShortCuts::cAlphaArgs, NumAlphas,
												DataIPShortCuts::rNumericArgs, NumNumbers, IOStat, DataIPShortCuts::lNumericFieldBlanks,
												DataIPShortCuts::lAlphaFieldBlanks, DataIPShortCuts::cAlphaFieldNames,
												DataIPShortCuts::cNumericFieldNames );
				if ( DataIPShortCuts::cAlphaArgs( 1 ).empty() ) DataIPShortCuts::cAlphaArgs( 1 ) = "Unknown";
				if ( NumAlphas > 3 ) {
					Multiples = "s";
				} else {
					Multiples = BlankString;
				}
				if ( DataIPShortCuts::cAlphaArgs( 2 ).empty() ) DataIPShortCuts::cAlphaArgs( 2 ) = "Unknown";
				{
					auto const errorType( uppercased( DataIPShortCuts::cAlphaArgs( 2 ) ) );
					if ( errorType == "INFORMATION" ) {
						ShowMessage( DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) +
									 "\" has the following Information message" + Multiples + ':' );
					} else if ( errorType == "WARNING" ) {
						ShowWarningError( DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) +
										  "\" has the following Warning condition" + Multiples + ':' );
					} else if ( errorType == "SEVERE" ) {
						ShowSevereError(
						DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) +
						"\" has the following Severe condition" +
						Multiples + ':' );
					} else if ( errorType == "FATAL" ) {
						ShowSevereError(
						DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) +
						"\" has the following Fatal condition" +
						Multiples + ':' );
						PreP_Fatal = true;
					} else {
						ShowSevereError(
						DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\" has the following " +
						DataIPShortCuts::cAlphaArgs( 2 ) +
						" condition" + Multiples + ':' );
					}
				}
				CountM = 3;
				if ( CountM > NumAlphas ) {
					ShowContinueError( DataIPShortCuts::cCurrentModuleObject + " was blank.  Check " + DataIPShortCuts::cAlphaArgs( 1 ) +
									   " audit trail or error file for possible reasons." );
				}
				while ( CountM <= NumAlphas ) {
					if ( len( DataIPShortCuts::cAlphaArgs( CountM ) ) == DataGlobals::MaxNameLength ) {
						ShowContinueError( DataIPShortCuts::cAlphaArgs( CountM ) + DataIPShortCuts::cAlphaArgs( CountM + 1 ) );
						CountM += 2;
					} else {
						ShowContinueError( DataIPShortCuts::cAlphaArgs( CountM ) );
						++CountM;
					}
				}
			}
		}

	}

	void
	InputProcessor::preScanReportingVariables() {
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
		DataOutputs::OutputVariablesForSimulation.reserve( 1024 );
		DataOutputs::MaxConsideredOutputVariables = 10000;

		// Output Variable
		auto epJSON_objects = epJSON.find( OutputVariable );
		if ( epJSON_objects != epJSON.end() ) {
			auto const & epJSON_object = epJSON_objects.value();
			for ( auto obj = epJSON_object.begin(); obj != epJSON_object.end(); ++obj ) {
				json const & fields = obj.value();
				if ( !fields.at( "key_value" ).empty() ) {
					addRecordToOutputVariableStructure( fields.at( "key_value" ),
																		fields.at( "variable_name" ) );
				} else {
					addRecordToOutputVariableStructure( "*", fields.at( "variable_name" ) );
				}
			}
		}

		epJSON_objects = epJSON.find( MeterCustom );
		if ( epJSON_objects != epJSON.end() ) {
			auto const & epJSON_object = epJSON_objects.value();
			auto const & legacy_idd = schema[ "properties" ][ MeterCustom ][ "legacy_idd" ];
			auto key = legacy_idd.find("extension");
			if ( key != legacy_idd.end() ) {
				extension_key = key.value();
			}
			for ( auto obj = epJSON_object.begin(); obj != epJSON_object.end(); ++obj ) {
				json const & fields = obj.value();
				for ( auto const & extensions : fields[ extension_key ] ) {
					if ( !obj.key().empty() ) {
						addRecordToOutputVariableStructure( extensions.at( "key_name" ),
																			extensions.at(
																			"output_variable_or_meter_name" ) );
					} else {
						addRecordToOutputVariableStructure( "*", extensions.at(
						"output_variable_or_meter_name" ) );
					}
				}
			}
		}

		epJSON_objects = epJSON.find( MeterCustomDecrement );
		if ( epJSON_objects != epJSON.end() ) {
			auto const & epJSON_object = epJSON_objects.value();
			auto const & legacy_idd = schema[ "properties" ][ MeterCustomDecrement ][ "legacy_idd" ];
			auto key = legacy_idd.find("extension");
			if ( key != legacy_idd.end() ) {
				extension_key = key.value();
			}
			for ( auto obj = epJSON_object.begin(); obj != epJSON_object.end(); ++obj ) {
				json const & fields = obj.value();
				for ( auto const & extensions : fields[ extension_key ] ) {
					if ( !obj.key().empty() ) {
						addRecordToOutputVariableStructure( extensions.at( "key_name" ),
																			extensions.at(
																			"output_variable_or_meter_name" ) );
					} else {
						addRecordToOutputVariableStructure( "*", extensions.at(
						"output_variable_or_meter_name" ) );
					}
				}
			}
		}

		epJSON_objects = epJSON.find( EMSSensor );
		if ( epJSON_objects != epJSON.end() ) {
			auto const & epJSON_object = epJSON_objects.value();
			for ( auto obj = epJSON_object.begin(); obj != epJSON_object.end(); ++obj ) {
				json const & fields = obj.value();
				if ( !fields.at( "output_variable_or_output_meter_index_key_name" ).empty() ) {
					addRecordToOutputVariableStructure(
					fields.at( "output_variable_or_output_meter_index_key_name" ),
					fields.at( "output_variable_or_output_meter_name" ) );
				} else {
					addRecordToOutputVariableStructure( "*", fields.at(
					"output_variable_or_output_meter_name" ) );
				}
			}
		}

		epJSON_objects = epJSON.find( EMSOutputVariable );
		if ( epJSON_objects != epJSON.end() ) {
			auto const & epJSON_object = epJSON_objects.value();
			for ( auto obj = epJSON_object.begin(); obj != epJSON_object.end(); ++obj ) {
				addRecordToOutputVariableStructure( "*", obj.key() );
			}
		}

		epJSON_objects = epJSON.find( OutputTableTimeBins );
		if ( epJSON_objects != epJSON.end() ) {
			auto const & epJSON_object = epJSON_objects.value();
			for ( auto obj = epJSON_object.begin(); obj != epJSON_object.end(); ++obj ) {
				json const & fields = obj.value();
				if ( !obj.key().empty() ) {
					addRecordToOutputVariableStructure( obj.key(), fields.at( "key_value" ) );
				} else {
					addRecordToOutputVariableStructure( "*", fields.at( "key_value" ) );
				}
			}
		}

		epJSON_objects = epJSON.find( OutputTableMonthly );
		if ( epJSON_objects != epJSON.end() ) {
			auto const & epJSON_object = epJSON_objects.value();
			auto const & legacy_idd = schema[ "properties" ][ OutputTableMonthly ][ "legacy_idd" ];
			auto key = legacy_idd.find("extension");
			if ( key != legacy_idd.end() ) {
				extension_key = key.value();
			}
			for ( auto obj = epJSON_object.begin(); obj != epJSON_object.end(); ++obj ) {
				json const & fields = obj.value();
				for ( auto const & extensions : fields[ extension_key ] ) {
					addRecordToOutputVariableStructure( "*",
																		extensions.at( "variable_or_meter_name" ) );
				}
			}
		}

		epJSON_objects = epJSON.find( OutputTableAnnual );
		if ( epJSON_objects != epJSON.end() ) {
			auto const & epJSON_object = epJSON_objects.value();
			auto const & legacy_idd = schema[ "properties" ][ OutputTableAnnual ][ "legacy_idd" ];
			auto key = legacy_idd.find("extension");
			if ( key != legacy_idd.end() ) {
				extension_key = key.value();
			}
			for ( auto obj = epJSON_object.begin(); obj != epJSON_object.end(); ++obj ) {
				json const & fields = obj.value();
				for ( auto const & extensions : fields[ extension_key ] ) {
					addRecordToOutputVariableStructure( "*", extensions.at(
					"variable_or_meter_or_ems_variable_or_field_name" ) );
				}
			}
		}

		epJSON_objects = epJSON.find( OutputTableSummaries );
		if ( epJSON_objects != epJSON.end() ) {
			auto const & epJSON_object = epJSON_objects.value();
			auto const & legacy_idd = schema[ "properties" ][ OutputTableSummaries ][ "legacy_idd" ];
			auto key = legacy_idd.find("extension");
			if ( key != legacy_idd.end() ) {
				extension_key = key.value();
			}
			for ( auto obj = epJSON_object.begin(); obj != epJSON_object.end(); ++obj ) {
				json const & fields = obj.value();
				for ( auto const & extensions : fields[ extension_key ] ) {
					auto const report_name = UtilityRoutines::MakeUPPERCase( extensions.at( "report_name" ) );
					if ( report_name == "ALLMONTHLY" || report_name == "ALLSUMMARYANDMONTHLY" ) {
						for ( int i = 1; i <= DataOutputs::NumMonthlyReports; ++i ) {
							addVariablesForMonthlyReport( DataOutputs::MonthlyNamedReports( i ) );
						}
					} else {
						addVariablesForMonthlyReport( report_name );
					}
				}
			}
		}

	}

	void
	InputProcessor::addVariablesForMonthlyReport( std::string const & reportName ) {

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
			addRecordToOutputVariableStructure( "*", "ZONE AIR SYSTEM SENSIBLE COOLING RATE" );
			addRecordToOutputVariableStructure( "*", "SITE OUTDOOR AIR DRYBULB TEMPERATURE" );
			addRecordToOutputVariableStructure( "*", "SITE OUTDOOR AIR WETBULB TEMPERATURE" );
			addRecordToOutputVariableStructure( "*", "ZONE TOTAL INTERNAL LATENT GAIN ENERGY" );
			addRecordToOutputVariableStructure( "*", "ZONE TOTAL INTERNAL LATENT GAIN RATE" );

		} else if ( reportName == "ZONEHEATINGSUMMARYMONTHLY" ) {
			addRecordToOutputVariableStructure( "*", "ZONE AIR SYSTEM SENSIBLE HEATING ENERGY" ); // on meter
			addRecordToOutputVariableStructure( "*", "ZONE AIR SYSTEM SENSIBLE HEATING RATE" );
			addRecordToOutputVariableStructure( "*", "SITE OUTDOOR AIR DRYBULB TEMPERATURE" );

		} else if ( reportName == "ZONEELECTRICSUMMARYMONTHLY" ) {
			addRecordToOutputVariableStructure( "*", "ZONE LIGHTS ELECTRIC ENERGY" );
			addRecordToOutputVariableStructure( "*", "ZONE ELECTRIC EQUIPMENT ELECTRIC ENERGY" );

		} else if ( reportName == "SPACEGAINSMONTHLY" ) {
			addRecordToOutputVariableStructure( "*", "ZONE PEOPLE TOTAL HEATING ENERGY" );
			addRecordToOutputVariableStructure( "*", "ZONE LIGHTS TOTAL HEATING ENERGY" );
			addRecordToOutputVariableStructure( "*", "ZONE ELECTRIC EQUIPMENT TOTAL HEATING ENERGY" );
			addRecordToOutputVariableStructure( "*", "ZONE GAS EQUIPMENT TOTAL HEATING ENERGY" );
			addRecordToOutputVariableStructure( "*", "ZONE HOT WATER EQUIPMENT TOTAL HEATING ENERGY" );
			addRecordToOutputVariableStructure( "*", "ZONE STEAM EQUIPMENT TOTAL HEATING ENERGY" );
			addRecordToOutputVariableStructure( "*", "ZONE OTHER EQUIPMENT TOTAL HEATING ENERGY" );
			addRecordToOutputVariableStructure( "*", "ZONE INFILTRATION SENSIBLE HEAT GAIN ENERGY" );
			addRecordToOutputVariableStructure( "*", "ZONE INFILTRATION SENSIBLE HEAT LOSS ENERGY" );

		} else if ( reportName == "PEAKSPACEGAINSMONTHLY" ) {
			addRecordToOutputVariableStructure( "*", "ZONE PEOPLE TOTAL HEATING ENERGY" );
			addRecordToOutputVariableStructure( "*", "ZONE LIGHTS TOTAL HEATING ENERGY" );
			addRecordToOutputVariableStructure( "*", "ZONE ELECTRIC EQUIPMENT TOTAL HEATING ENERGY" );
			addRecordToOutputVariableStructure( "*", "ZONE GAS EQUIPMENT TOTAL HEATING ENERGY" );
			addRecordToOutputVariableStructure( "*", "ZONE HOT WATER EQUIPMENT TOTAL HEATING ENERGY" );
			addRecordToOutputVariableStructure( "*", "ZONE STEAM EQUIPMENT TOTAL HEATING ENERGY" );
			addRecordToOutputVariableStructure( "*", "ZONE OTHER EQUIPMENT TOTAL HEATING ENERGY" );
			addRecordToOutputVariableStructure( "*", "ZONE INFILTRATION SENSIBLE HEAT GAIN ENERGY" );
			addRecordToOutputVariableStructure( "*", "ZONE INFILTRATION SENSIBLE HEAT LOSS ENERGY" );

		} else if ( reportName == "SPACEGAINCOMPONENTSATCOOLINGPEAKMONTHLY" ) {
			addRecordToOutputVariableStructure( "*", "ZONE AIR SYSTEM SENSIBLE COOLING RATE" );
			addRecordToOutputVariableStructure( "*", "ZONE PEOPLE TOTAL HEATING ENERGY" );
			addRecordToOutputVariableStructure( "*", "ZONE LIGHTS TOTAL HEATING ENERGY" );
			addRecordToOutputVariableStructure( "*", "ZONE ELECTRIC EQUIPMENT TOTAL HEATING ENERGY" );
			addRecordToOutputVariableStructure( "*", "ZONE GAS EQUIPMENT TOTAL HEATING ENERGY" );
			addRecordToOutputVariableStructure( "*", "ZONE HOT WATER EQUIPMENT TOTAL HEATING ENERGY" );
			addRecordToOutputVariableStructure( "*", "ZONE STEAM EQUIPMENT TOTAL HEATING ENERGY" );
			addRecordToOutputVariableStructure( "*", "ZONE OTHER EQUIPMENT TOTAL HEATING ENERGY" );
			addRecordToOutputVariableStructure( "*", "ZONE INFILTRATION SENSIBLE HEAT GAIN ENERGY" );
			addRecordToOutputVariableStructure( "*", "ZONE INFILTRATION SENSIBLE HEAT LOSS ENERGY" );

		} else if ( reportName == "SETPOINTSNOTMETWITHTEMPERATURESMONTHLY" ) {
			addRecordToOutputVariableStructure( "*", "ZONE HEATING SETPOINT NOT MET TIME" );
			addRecordToOutputVariableStructure( "*", "ZONE MEAN AIR TEMPERATURE" );
			addRecordToOutputVariableStructure( "*", "ZONE HEATING SETPOINT NOT MET WHILE OCCUPIED TIME" );
			addRecordToOutputVariableStructure( "*", "ZONE COOLING SETPOINT NOT MET TIME" );
			addRecordToOutputVariableStructure( "*", "ZONE COOLING SETPOINT NOT MET WHILE OCCUPIED TIME" );

		} else if ( reportName == "COMFORTREPORTSIMPLE55MONTHLY" ) {
			addRecordToOutputVariableStructure( "*",
												"ZONE THERMAL COMFORT ASHRAE 55 SIMPLE MODEL SUMMER CLOTHES NOT COMFORTABLE TIME" );
			addRecordToOutputVariableStructure( "*", "ZONE MEAN AIR TEMPERATURE" );
			addRecordToOutputVariableStructure( "*",
												"ZONE THERMAL COMFORT ASHRAE 55 SIMPLE MODEL WINTER CLOTHES NOT COMFORTABLE TIME" );
			addRecordToOutputVariableStructure( "*",
												"ZONE THERMAL COMFORT ASHRAE 55 SIMPLE MODEL SUMMER OR WINTER CLOTHES NOT COMFORTABLE TIME" );

		} else if ( reportName == "UNGLAZEDTRANSPIREDSOLARCOLLECTORSUMMARYMONTHLY" ) {
			addRecordToOutputVariableStructure( "*", "SOLAR COLLECTOR SYSTEM EFFICIENCY" );
			addRecordToOutputVariableStructure( "*", "SOLAR COLLECTOR OUTSIDE FACE SUCTION VELOCITY" );
			addRecordToOutputVariableStructure( "*", "SOLAR COLLECTOR SENSIBLE HEATING RATE" );

		} else if ( reportName == "OCCUPANTCOMFORTDATASUMMARYMONTHLY" ) {
			addRecordToOutputVariableStructure( "*", "PEOPLE OCCUPANT COUNT" );
			addRecordToOutputVariableStructure( "*", "PEOPLE AIR TEMPERATURE" );
			addRecordToOutputVariableStructure( "*", "PEOPLE AIR RELATIVE HUMIDITY" );
			addRecordToOutputVariableStructure( "*", "ZONE THERMAL COMFORT FANGER MODEL PMV" );
			addRecordToOutputVariableStructure( "*", "ZONE THERMAL COMFORT FANGER MODEL PPD" );

		} else if ( reportName == "CHILLERREPORTMONTHLY" ) {
			addRecordToOutputVariableStructure( "*", "CHILLER ELECTRIC ENERGY" ); // on meter
			addRecordToOutputVariableStructure( "*", "CHILLER ELECTRIC POWER" );
			addRecordToOutputVariableStructure( "*", "CHILLER EVAPORATOR COOLING ENERGY" ); // on meter
			addRecordToOutputVariableStructure( "*", "CHILLER CONDENSER HEAT TRANSFER ENERGY" ); // on meter
			addRecordToOutputVariableStructure( "*", "CHILLER COP" );

		} else if ( reportName == "TOWERREPORTMONTHLY" ) {
			addRecordToOutputVariableStructure( "*", "COOLING TOWER FAN ELECTRIC ENERGY" ); // on meter
			addRecordToOutputVariableStructure( "*", "COOLING TOWER FAN ELECTRIC POWER" );
			addRecordToOutputVariableStructure( "*", "COOLING TOWER HEAT TRANSFER RATE" );
			addRecordToOutputVariableStructure( "*", "COOLING TOWER INLET TEMPERATURE" );
			addRecordToOutputVariableStructure( "*", "COOLING TOWER OUTLET TEMPERATURE" );
			addRecordToOutputVariableStructure( "*", "COOLING TOWER MASS FLOW RATE" );

		} else if ( reportName == "BOILERREPORTMONTHLY" ) {
			addRecordToOutputVariableStructure( "*", "BOILER HEATING ENERGY" ); // on meter
			addRecordToOutputVariableStructure( "*", "BOILER GAS CONSUMPTION" ); // on meter
			addRecordToOutputVariableStructure( "*", "BOILER HEATING ENERGY" ); // on meter
			addRecordToOutputVariableStructure( "*", "BOILER HEATING RATE" );
			addRecordToOutputVariableStructure( "*", "BOILER GAS CONSUMPTION RATE" );
			addRecordToOutputVariableStructure( "*", "BOILER INLET TEMPERATURE" );
			addRecordToOutputVariableStructure( "*", "BOILER OUTLET TEMPERATURE" );
			addRecordToOutputVariableStructure( "*", "BOILER MASS FLOW RATE" );
			addRecordToOutputVariableStructure( "*", "BOILER ANCILLARY ELECTRIC POWER" );

		} else if ( reportName == "DXREPORTMONTHLY" ) {
			addRecordToOutputVariableStructure( "*", "COOLING COIL TOTAL COOLING ENERGY" ); // on meter
			addRecordToOutputVariableStructure( "*", "COOLING COIL ELECTRIC ENERGY" ); // on meter
			addRecordToOutputVariableStructure( "*", "COOLING COIL SENSIBLE COOLING ENERGY" );
			addRecordToOutputVariableStructure( "*", "COOLING COIL LATENT COOLING ENERGY" );
			addRecordToOutputVariableStructure( "*", "COOLING COIL CRANKCASE HEATER ELECTRIC ENERGY" );
			addRecordToOutputVariableStructure( "*", "COOLING COIL RUNTIME FRACTION" );
			addRecordToOutputVariableStructure( "*", "COOLING COIL TOTAL COOLING RATE" );
			addRecordToOutputVariableStructure( "*", "COOLING COIL SENSIBLE COOLING RATE" );
			addRecordToOutputVariableStructure( "*", "COOLING COIL LATENT COOLING RATE" );
			addRecordToOutputVariableStructure( "*", "COOLING COIL ELECTRIC POWER" );
			addRecordToOutputVariableStructure( "*", "COOLING COIL CRANKCASE HEATER ELECTRIC POWER" );

		} else if ( reportName == "WINDOWREPORTMONTHLY" ) {
			addRecordToOutputVariableStructure( "*", "SURFACE WINDOW TRANSMITTED SOLAR RADIATION RATE" );
			addRecordToOutputVariableStructure( "*", "SURFACE WINDOW TRANSMITTED BEAM SOLAR RADIATION RATE" );
			addRecordToOutputVariableStructure( "*", "SURFACE WINDOW TRANSMITTED DIFFUSE SOLAR RADIATION RATE" );
			addRecordToOutputVariableStructure( "*", "SURFACE WINDOW HEAT GAIN RATE" );
			addRecordToOutputVariableStructure( "*", "SURFACE WINDOW HEAT LOSS RATE" );
			addRecordToOutputVariableStructure( "*", "SURFACE WINDOW INSIDE FACE GLAZING CONDENSATION STATUS" );
			addRecordToOutputVariableStructure( "*", "SURFACE SHADING DEVICE IS ON TIME FRACTION" );
			addRecordToOutputVariableStructure( "*", "SURFACE STORM WINDOW ON OFF STATUS" );

		} else if ( reportName == "WINDOWENERGYREPORTMONTHLY" ) {
			addRecordToOutputVariableStructure( "*", "SURFACE WINDOW TRANSMITTED SOLAR RADIATION ENERGY" );
			addRecordToOutputVariableStructure( "*", "SURFACE WINDOW TRANSMITTED BEAM SOLAR RADIATION ENERGY" );
			addRecordToOutputVariableStructure( "*", "SURFACE WINDOW TRANSMITTED DIFFUSE SOLAR RADIATION ENERGY" );
			addRecordToOutputVariableStructure( "*", "SURFACE WINDOW HEAT GAIN ENERGY" );
			addRecordToOutputVariableStructure( "*", "SURFACE WINDOW HEAT LOSS ENERGY" );

		} else if ( reportName == "WINDOWZONESUMMARYMONTHLY" ) {
			addRecordToOutputVariableStructure( "*", "ZONE WINDOWS TOTAL HEAT GAIN RATE" );
			addRecordToOutputVariableStructure( "*", "ZONE WINDOWS TOTAL HEAT LOSS RATE" );
			addRecordToOutputVariableStructure( "*", "ZONE WINDOWS TOTAL TRANSMITTED SOLAR RADIATION RATE" );
			addRecordToOutputVariableStructure( "*",
												"ZONE EXTERIOR WINDOWS TOTAL TRANSMITTED BEAM SOLAR RADIATION RATE" );
			addRecordToOutputVariableStructure( "*",
												"ZONE EXTERIOR WINDOWS TOTAL TRANSMITTED DIFFUSE SOLAR RADIATION RATE" );
			addRecordToOutputVariableStructure( "*",
												"ZONE INTERIOR WINDOWS TOTAL TRANSMITTED DIFFUSE SOLAR RADIATION RATE" );
			addRecordToOutputVariableStructure( "*",
												"ZONE INTERIOR WINDOWS TOTAL TRANSMITTED BEAM SOLAR RADIATION RATE" );

		} else if ( reportName == "WINDOWENERGYZONESUMMARYMONTHLY" ) {
			addRecordToOutputVariableStructure( "*", "ZONE WINDOWS TOTAL HEAT GAIN ENERGY" );
			addRecordToOutputVariableStructure( "*", "ZONE WINDOWS TOTAL HEAT LOSS ENERGY" );
			addRecordToOutputVariableStructure( "*", "ZONE WINDOWS TOTAL TRANSMITTED SOLAR RADIATION ENERGY" );
			addRecordToOutputVariableStructure( "*",
												"ZONE EXTERIOR WINDOWS TOTAL TRANSMITTED BEAM SOLAR RADIATION ENERGY" );
			addRecordToOutputVariableStructure( "*",
												"ZONE EXTERIOR WINDOWS TOTAL TRANSMITTED DIFFUSE SOLAR RADIATION ENERGY" );
			addRecordToOutputVariableStructure( "*",
												"ZONE INTERIOR WINDOWS TOTAL TRANSMITTED DIFFUSE SOLAR RADIATION ENERGY" );
			addRecordToOutputVariableStructure( "*",
												"ZONE INTERIOR WINDOWS TOTAL TRANSMITTED BEAM SOLAR RADIATION ENERGY" );

		} else if ( reportName == "AVERAGEOUTDOORCONDITIONSMONTHLY" ) {
			addRecordToOutputVariableStructure( "*", "SITE OUTDOOR AIR DRYBULB TEMPERATURE" );
			addRecordToOutputVariableStructure( "*", "SITE OUTDOOR AIR WETBULB TEMPERATURE" );
			addRecordToOutputVariableStructure( "*", "SITE OUTDOOR AIR DEWPOINT TEMPERATURE" );
			addRecordToOutputVariableStructure( "*", "SITE WIND SPEED" );
			addRecordToOutputVariableStructure( "*", "SITE SKY TEMPERATURE" );
			addRecordToOutputVariableStructure( "*", "SITE DIFFUSE SOLAR RADIATION RATE PER AREA" );
			addRecordToOutputVariableStructure( "*", "SITE DIRECT SOLAR RADIATION RATE PER AREA" );
			addRecordToOutputVariableStructure( "*", "SITE RAIN STATUS" );

		} else if ( reportName == "OUTDOORCONDITIONSMAXIMUMDRYBULBMONTHLY" ) {
			addRecordToOutputVariableStructure( "*", "SITE OUTDOOR AIR DRYBULB TEMPERATURE" );
			addRecordToOutputVariableStructure( "*", "SITE OUTDOOR AIR WETBULB TEMPERATURE" );
			addRecordToOutputVariableStructure( "*", "SITE OUTDOOR AIR DEWPOINT TEMPERATURE" );
			addRecordToOutputVariableStructure( "*", "SITE WIND SPEED" );
			addRecordToOutputVariableStructure( "*", "SITE SKY TEMPERATURE" );
			addRecordToOutputVariableStructure( "*", "SITE DIFFUSE SOLAR RADIATION RATE PER AREA" );
			addRecordToOutputVariableStructure( "*", "SITE DIRECT SOLAR RADIATION RATE PER AREA" );

		} else if ( reportName == "OUTDOORCONDITIONSMINIMUMDRYBULBMONTHLY" ) {
			addRecordToOutputVariableStructure( "*", "SITE OUTDOOR AIR DRYBULB TEMPERATURE" );
			addRecordToOutputVariableStructure( "*", "SITE OUTDOOR AIR WETBULB TEMPERATURE" );
			addRecordToOutputVariableStructure( "*", "SITE OUTDOOR AIR DEWPOINT TEMPERATURE" );
			addRecordToOutputVariableStructure( "*", "SITE WIND SPEED" );
			addRecordToOutputVariableStructure( "*", "SITE SKY TEMPERATURE" );
			addRecordToOutputVariableStructure( "*", "SITE DIFFUSE SOLAR RADIATION RATE PER AREA" );
			addRecordToOutputVariableStructure( "*", "SITE DIRECT SOLAR RADIATION RATE PER AREA" );

		} else if ( reportName == "OUTDOORCONDITIONSMAXIMUMWETBULBMONTHLY" ) {
			addRecordToOutputVariableStructure( "*", "SITE OUTDOOR AIR WETBULB TEMPERATURE" );
			addRecordToOutputVariableStructure( "*", "SITE OUTDOOR AIR DRYBULB TEMPERATURE" );
			addRecordToOutputVariableStructure( "*", "SITE OUTDOOR AIR DEWPOINT TEMPERATURE" );
			addRecordToOutputVariableStructure( "*", "SITE WIND SPEED" );
			addRecordToOutputVariableStructure( "*", "SITE SKY TEMPERATURE" );
			addRecordToOutputVariableStructure( "*", "SITE DIFFUSE SOLAR RADIATION RATE PER AREA" );
			addRecordToOutputVariableStructure( "*", "SITE DIRECT SOLAR RADIATION RATE PER AREA" );

		} else if ( reportName == "OUTDOORCONDITIONSMAXIMUMDEWPOINTMONTHLY" ) {
			addRecordToOutputVariableStructure( "*", "SITE OUTDOOR AIR DEWPOINT TEMPERATURE" );
			addRecordToOutputVariableStructure( "*", "SITE OUTDOOR AIR DRYBULB TEMPERATURE" );
			addRecordToOutputVariableStructure( "*", "SITE OUTDOOR AIR WETBULB TEMPERATURE" );
			addRecordToOutputVariableStructure( "*", "SITE WIND SPEED" );
			addRecordToOutputVariableStructure( "*", "SITE SKY TEMPERATURE" );
			addRecordToOutputVariableStructure( "*", "SITE DIFFUSE SOLAR RADIATION RATE PER AREA" );
			addRecordToOutputVariableStructure( "*", "SITE DIRECT SOLAR RADIATION RATE PER AREA" );

		} else if ( reportName == "OUTDOORGROUNDCONDITIONSMONTHLY" ) {
			addRecordToOutputVariableStructure( "*", "SITE GROUND TEMPERATURE" );
			addRecordToOutputVariableStructure( "*", "SITE SURFACE GROUND TEMPERATURE" );
			addRecordToOutputVariableStructure( "*", "SITE DEEP GROUND TEMPERATURE" );
			addRecordToOutputVariableStructure( "*", "SITE MAINS WATER TEMPERATURE" );
			addRecordToOutputVariableStructure( "*", "SITE GROUND REFLECTED SOLAR RADIATION RATE PER AREA" );
			addRecordToOutputVariableStructure( "*", "SITE SNOW ON GROUND STATUS" );

		} else if ( reportName == "WINDOWACREPORTMONTHLY" ) {
			addRecordToOutputVariableStructure( "*", "ZONE WINDOW AIR CONDITIONER TOTAL COOLING ENERGY" );
			addRecordToOutputVariableStructure( "*", "ZONE WINDOW AIR CONDITIONER ELECTRIC ENERGY" );
			addRecordToOutputVariableStructure( "*", "ZONE WINDOW AIR CONDITIONER TOTAL COOLING ENERGY" );
			addRecordToOutputVariableStructure( "*", "ZONE WINDOW AIR CONDITIONER SENSIBLE COOLING ENERGY" );
			addRecordToOutputVariableStructure( "*", "ZONE WINDOW AIR CONDITIONER LATENT COOLING ENERGY" );
			addRecordToOutputVariableStructure( "*", "ZONE WINDOW AIR CONDITIONER TOTAL COOLING RATE" );
			addRecordToOutputVariableStructure( "*", "ZONE WINDOW AIR CONDITIONER SENSIBLE COOLING RATE" );
			addRecordToOutputVariableStructure( "*", "ZONE WINDOW AIR CONDITIONER LATENT COOLING RATE" );
			addRecordToOutputVariableStructure( "*", "ZONE WINDOW AIR CONDITIONER ELECTRIC POWER" );

		} else if ( reportName == "WATERHEATERREPORTMONTHLY" ) {
			addRecordToOutputVariableStructure( "*", "WATER HEATER TOTAL DEMAND HEAT TRANSFER ENERGY" );
			addRecordToOutputVariableStructure( "*", "WATER HEATER USE SIDE HEAT TRANSFER ENERGY" );
			addRecordToOutputVariableStructure( "*", "WATER HEATER BURNER HEATING ENERGY" );
			addRecordToOutputVariableStructure( "*", "WATER HEATER GAS CONSUMPTION" );
			addRecordToOutputVariableStructure( "*", "WATER HEATER TOTAL DEMAND HEAT TRANSFER ENERGY" );
			addRecordToOutputVariableStructure( "*", "WATER HEATER LOSS DEMAND ENERGY" );
			addRecordToOutputVariableStructure( "*", "WATER HEATER HEAT LOSS ENERGY" );
			addRecordToOutputVariableStructure( "*", "WATER HEATER TANK TEMPERATURE" );
			addRecordToOutputVariableStructure( "*", "WATER HEATER HEAT RECOVERY SUPPLY ENERGY" );
			addRecordToOutputVariableStructure( "*", "WATER HEATER SOURCE ENERGY" );

		} else if ( reportName == "GENERATORREPORTMONTHLY" ) {
			addRecordToOutputVariableStructure( "*", "GENERATOR PRODUCED ELECTRIC ENERGY" );
			addRecordToOutputVariableStructure( "*", "GENERATOR DIESEL CONSUMPTION" );
			addRecordToOutputVariableStructure( "*", "GENERATOR GAS CONSUMPTION" );
			addRecordToOutputVariableStructure( "*", "GENERATOR PRODUCED ELECTRIC ENERGY" );
			addRecordToOutputVariableStructure( "*", "GENERATOR TOTAL HEAT RECOVERY" );
			addRecordToOutputVariableStructure( "*", "GENERATOR JACKET HEAT RECOVERY ENERGY" );
			addRecordToOutputVariableStructure( "*", "GENERATOR LUBE HEAT RECOVERY" );
			addRecordToOutputVariableStructure( "*", "GENERATOR EXHAUST HEAT RECOVERY ENERGY" );
			addRecordToOutputVariableStructure( "*", "GENERATOR EXHAUST AIR TEMPERATURE" );

		} else if ( reportName == "DAYLIGHTINGREPORTMONTHLY" ) {
			addRecordToOutputVariableStructure( "*", "SITE EXTERIOR BEAM NORMAL ILLUMINANCE" );
			addRecordToOutputVariableStructure( "*", "DAYLIGHTING LIGHTING POWER MULTIPLIER" );
			addRecordToOutputVariableStructure( "*", "DAYLIGHTING LIGHTING POWER MULTIPLIER" );
			addRecordToOutputVariableStructure( "*", "DAYLIGHTING REFERENCE POINT 1 ILLUMINANCE" );
			addRecordToOutputVariableStructure( "*", "DAYLIGHTING REFERENCE POINT 1 GLARE INDEX" );
			addRecordToOutputVariableStructure( "*",
												"DAYLIGHTING REFERENCE POINT 1 GLARE INDEX SETPOINT EXCEEDED TIME" );
			addRecordToOutputVariableStructure( "*",
												"DAYLIGHTING REFERENCE POINT 1 DAYLIGHT ILLUMINANCE SETPOINT EXCEEDED TIME" );
			addRecordToOutputVariableStructure( "*", "DAYLIGHTING REFERENCE POINT 2 ILLUMINANCE" );
			addRecordToOutputVariableStructure( "*", "DAYLIGHTING REFERENCE POINT 2 GLARE INDEX" );
			addRecordToOutputVariableStructure( "*",
												"DAYLIGHTING REFERENCE POINT 2 GLARE INDEX SETPOINT EXCEEDED TIME" );
			addRecordToOutputVariableStructure( "*",
												"DAYLIGHTING REFERENCE POINT 2 DAYLIGHT ILLUMINANCE SETPOINT EXCEEDED TIME" );

		} else if ( reportName == "COILREPORTMONTHLY" ) {
			addRecordToOutputVariableStructure( "*", "HEATING COIL HEATING ENERGY" );
			addRecordToOutputVariableStructure( "*", "HEATING COIL HEATING RATE" );
			addRecordToOutputVariableStructure( "*", "COOLING COIL SENSIBLE COOLING ENERGY" );
			addRecordToOutputVariableStructure( "*", "COOLING COIL TOTAL COOLING ENERGY" );
			addRecordToOutputVariableStructure( "*", "COOLING COIL TOTAL COOLING RATE" );
			addRecordToOutputVariableStructure( "*", "COOLING COIL SENSIBLE COOLING RATE" );
			addRecordToOutputVariableStructure( "*", "COOLING COIL WETTED AREA FRACTION" );

		} else if ( reportName == "PLANTLOOPDEMANDREPORTMONTHLY" ) {
			addRecordToOutputVariableStructure( "*", "PLANT SUPPLY SIDE COOLING DEMAND RATE" );
			addRecordToOutputVariableStructure( "*", "PLANT SUPPLY SIDE HEATING DEMAND RATE" );

		} else if ( reportName == "FANREPORTMONTHLY" ) {
			addRecordToOutputVariableStructure( "*", "FAN ELECTRIC ENERGY" );
			addRecordToOutputVariableStructure( "*", "FAN RISE IN AIR TEMPERATURE" );
			addRecordToOutputVariableStructure( "*", "FAN ELECTRIC POWER" );

		} else if ( reportName == "PUMPREPORTMONTHLY" ) {
			addRecordToOutputVariableStructure( "*", "PUMP ELECTRIC ENERGY" );
			addRecordToOutputVariableStructure( "*", "PUMP FLUID HEAT GAIN ENERGY" );
			addRecordToOutputVariableStructure( "*", "PUMP ELECTRIC POWER" );
			addRecordToOutputVariableStructure( "*", "PUMP SHAFT POWER" );
			addRecordToOutputVariableStructure( "*", "PUMP FLUID HEAT GAIN RATE" );
			addRecordToOutputVariableStructure( "*", "PUMP OUTLET TEMPERATURE" );
			addRecordToOutputVariableStructure( "*", "PUMP MASS FLOW RATE" );

		} else if ( reportName == "CONDLOOPDEMANDREPORTMONTHLY" ) {
			addRecordToOutputVariableStructure( "*", "PLANT SUPPLY SIDE COOLING DEMAND RATE" );
			addRecordToOutputVariableStructure( "*", "PLANT SUPPLY SIDE HEATING DEMAND RATE" );
			addRecordToOutputVariableStructure( "*", "PLANT SUPPLY SIDE INLET TEMPERATURE" );
			addRecordToOutputVariableStructure( "*", "PLANT SUPPLY SIDE OUTLET TEMPERATURE" );

		} else if ( reportName == "ZONETEMPERATUREOSCILLATIONREPORTMONTHLY" ) {
			addRecordToOutputVariableStructure( "*", "ZONE OSCILLATING TEMPERATURES TIME" );
			addRecordToOutputVariableStructure( "*", "ZONE PEOPLE OCCUPANT COUNT" );

		} else if ( reportName == "AIRLOOPSYSTEMENERGYANDWATERUSEMONTHLY" ) {
			addRecordToOutputVariableStructure( "*", "AIR SYSTEM HOT WATER ENERGY" );
			addRecordToOutputVariableStructure( "*", "AIR SYSTEM STEAM ENERGY" );
			addRecordToOutputVariableStructure( "*", "AIR SYSTEM CHILLED WATER ENERGY" );
			addRecordToOutputVariableStructure( "*", "AIR SYSTEM ELECTRIC ENERGY" );
			addRecordToOutputVariableStructure( "*", "AIR SYSTEM GAS ENERGY" );
			addRecordToOutputVariableStructure( "*", "AIR SYSTEM WATER VOLUME" );

		} else if ( reportName == "AIRLOOPSYSTEMCOMPONENTLOADSMONTHLY" ) {
			addRecordToOutputVariableStructure( "*", "AIR SYSTEM FAN AIR HEATING ENERGY" );
			addRecordToOutputVariableStructure( "*", "AIR SYSTEM COOLING COIL TOTAL COOLING ENERGY" );
			addRecordToOutputVariableStructure( "*", "AIR SYSTEM HEATING COIL TOTAL HEATING ENERGY" );
			addRecordToOutputVariableStructure( "*", "AIR SYSTEM HEAT EXCHANGER TOTAL HEATING ENERGY" );
			addRecordToOutputVariableStructure( "*", "AIR SYSTEM HEAT EXCHANGER TOTAL COOLING ENERGY" );
			addRecordToOutputVariableStructure( "*", "AIR SYSTEM HUMIDIFIER TOTAL HEATING ENERGY" );
			addRecordToOutputVariableStructure( "*", "AIR SYSTEM EVAPORATIVE COOLER TOTAL COOLING ENERGY" );
			addRecordToOutputVariableStructure( "*", "AIR SYSTEM DESICCANT DEHUMIDIFIER TOTAL COOLING ENERGY" );

		} else if ( reportName == "AIRLOOPSYSTEMCOMPONENTENERGYUSEMONTHLY" ) {
			addRecordToOutputVariableStructure( "*", "AIR SYSTEM FAN ELECTRIC ENERGY" );
			addRecordToOutputVariableStructure( "*", "AIR SYSTEM HEATING COIL HOT WATER ENERGY" );
			addRecordToOutputVariableStructure( "*", "AIR SYSTEM COOLING COIL CHILLED WATER ENERGY" );
			addRecordToOutputVariableStructure( "*", "AIR SYSTEM DX HEATING COIL ELECTRIC ENERGY" );
			addRecordToOutputVariableStructure( "*", "AIR SYSTEM DX COOLING COIL ELECTRIC ENERGY" );
			addRecordToOutputVariableStructure( "*", "AIR SYSTEM HEATING COIL ELECTRIC ENERGY" );
			addRecordToOutputVariableStructure( "*", "AIR SYSTEM HEATING COIL GAS ENERGY" );
			addRecordToOutputVariableStructure( "*", "AIR SYSTEM HEATING COIL STEAM ENERGY" );
			addRecordToOutputVariableStructure( "*", "AIR SYSTEM HUMIDIFIER ELECTRIC ENERGY" );
			addRecordToOutputVariableStructure( "*", "AIR SYSTEM EVAPORATIVE COOLER ELECTRIC ENERGY" );
			addRecordToOutputVariableStructure( "*", "AIR SYSTEM DESICCANT DEHUMIDIFIER ELECTRIC ENERGY" );

		} else if ( reportName == "MECHANICALVENTILATIONLOADSMONTHLY" ) {
			addRecordToOutputVariableStructure( "*", "ZONE MECHANICAL VENTILATION NO LOAD HEAT REMOVAL ENERGY" );
			addRecordToOutputVariableStructure( "*", "ZONE MECHANICAL VENTILATION COOLING LOAD INCREASE ENERGY" );
			addRecordToOutputVariableStructure( "*",
												"ZONE MECHANICAL VENTILATION COOLING LOAD INCREASE DUE TO OVERHEATING ENERGY" );
			addRecordToOutputVariableStructure( "*", "ZONE MECHANICAL VENTILATION COOLING LOAD DECREASE ENERGY" );
			addRecordToOutputVariableStructure( "*", "ZONE MECHANICAL VENTILATION NO LOAD HEAT ADDITION ENERGY" );
			addRecordToOutputVariableStructure( "*", "ZONE MECHANICAL VENTILATION HEATING LOAD INCREASE ENERGY" );
			addRecordToOutputVariableStructure( "*",
												"ZONE MECHANICAL VENTILATION HEATING LOAD INCREASE DUE TO OVERCOOLING ENERGY" );
			addRecordToOutputVariableStructure( "*", "ZONE MECHANICAL VENTILATION HEATING LOAD DECREASE ENERGY" );
			addRecordToOutputVariableStructure( "*", "ZONE MECHANICAL VENTILATION AIR CHANGES PER HOUR" );

		} else {

		}

	}

	void
	InputProcessor::addRecordToOutputVariableStructure(
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

}
