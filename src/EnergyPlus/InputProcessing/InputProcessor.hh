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

#ifndef InputProcessor_hh_INCLUDED
#define InputProcessor_hh_INCLUDED

// C++ Headers
#include <string>
#include <vector>
#include <unordered_map>
#include <map>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1S.fwd.hh>
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

#include <nlohmann/json.hpp>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

	class IdfParser;
	class State;

	class InputProcessor {
	public:
		using json = nlohmann::json;

		friend class EnergyPlusFixture;
		friend class InputProcessorFixture;

		static json::parser_callback_t callback;

		// Clears the global data in InputProcessor.
		// Needed for unit tests, should not be normally called.
		static
		void
		clear_state();

		static
		std::pair< bool, std::string >
		ConvertInsensitiveObjectType( std::string const & objectType );

		static
		void
		InitializeMaps();

		static
		void
		ProcessInput();

		static
		int
		GetNumSectionsFound( std::string const & SectionWord );

		static
		int
		GetNumObjectsFound( std::string const & ObjectWord );

		static
		void
		GetObjectItem(
			std::string const & Object,
			int const Number,
			Array1S_string Alphas,
			int & NumAlphas,
			Array1S< Real64 > Numbers,
			int & NumNumbers,
			int & Status,
			Optional< Array1D_bool > NumBlank = _,
			Optional< Array1D_bool > AlphaBlank = _,
			Optional< Array1D_string > AlphaFieldNames = _,
			Optional< Array1D_string > NumericFieldNames = _
		);


		static
		int
		GetObjectItemNum(
			std::string const & ObjType, // Object Type (ref: IDD Objects)
			std::string const & ObjName // Name of the object type
		);

		static
		int
		GetObjectItemNum(
			std::string const & ObjType, // Object Type (ref: IDD Objects)
			std::string const & NameTypeVal, // Object "name" field type ( used as search key )
			std::string const & ObjName // Name of the object type
		);

		static
		void
		RangeCheck(
			bool & ErrorsFound, // Set to true if error detected
			std::string const & WhatFieldString, // Descriptive field for string
			std::string const & WhatObjectString, // Descriptive field for object, Zone Name, etc.
			std::string const & ErrorLevel, // 'Warning','Severe','Fatal')
			Optional_string_const LowerBoundString = _, // String for error message, if applicable
			Optional_bool_const LowerBoundCondition = _, // Condition for error condition, if applicable
			Optional_string_const UpperBoundString = _, // String for error message, if applicable
			Optional_bool_const UpperBoundCondition = _, // Condition for error condition, if applicable
			Optional_string_const ValueString = _, // Value with digits if to be displayed with error
			Optional_string_const WhatObjectName = _ // ObjectName -- used for error messages
		);

		static
		void
		GetMaxSchemaArgs(
			int & NumArgs,
			int & NumAlpha,
			int & NumNumeric
		);

		static
		void
		GetObjectDefMaxArgs(
			std::string const & ObjectWord, // Object for definition
			int & NumArgs, // How many arguments (max) this Object can have
			int & NumAlpha, // How many Alpha arguments (max) this Object can have
			int & NumNumeric // How many Numeric arguments (max) this Object can have
		);

		static
		void
		PreProcessorCheck( bool & PreP_Fatal ); // True if a preprocessor flags a fatal error

		static
		void
		PreScanReportingVariables();

		static
		void
		AddVariablesForMonthlyReport( std::string const & reportName );

		static
		void
		AddRecordToOutputVariableStructure(
			std::string const & KeyValue,
			std::string const & VariableName
		);

		static
		void
		ReAllocateAndPreserveOutputVariablesForSimulation();

		static
		void
		ReportOrphanRecordObjects();

		struct ObjectInfo
		{
			ObjectInfo() = default;

			ObjectInfo( std::string const & objectType, std::string const & objectName )
			:
			objectType( objectType ),
			objectName( objectName )
			{}

			ObjectInfo( std::string && objectType, std::string && objectName )
			:
			objectType( objectType ),
			objectName( objectName )
			{}

			std::string objectType = "";
			std::string objectName = "";
		};

		struct ObjectCache
		{
			ObjectCache() = default;

			ObjectCache( json::const_iterator const & schemaIterator, std::vector< json::const_iterator > const & inputObjectIterators )
			:
			schemaIterator( schemaIterator ),
			inputObjectIterators( inputObjectIterators )
			{}

			ObjectCache( json::const_iterator && schemaIterator, std::vector< json::const_iterator > && inputObjectIterators )
			:
			schemaIterator( schemaIterator ),
			inputObjectIterators( inputObjectIterators )
			{}

			json::const_iterator schemaIterator;
			std::vector< json::const_iterator > inputObjectIterators;
		};

	private:
		static
		std::vector < std::string > const &
		validationErrors();

		static
		std::vector < std::string > const &
		validationWarnings();

		using UnorderedObjectTypeMap = std::unordered_map < std::string, std::string >;
		using UnorderedObjectCacheMap = std::unordered_map< std::string, ObjectCache >;
		using UnorderedUnusedObjectMap = std::map< const json::object_t * const, ObjectInfo >;

		static IdfParser idf_parser;
		static json schema;
		static json jdf;
		static State state;
		static UnorderedObjectTypeMap caseInsensitiveObjectMap;
		static UnorderedObjectCacheMap objectCacheMap;
		static UnorderedUnusedObjectMap unusedInputs;
		static char s[ 129 ];

	}; // InputProcessor
}

#endif
