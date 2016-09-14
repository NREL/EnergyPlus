#ifndef InputProcessor_hh_INCLUDED
#define InputProcessor_hh_INCLUDED

// C++ Headers
#include <iosfwd>
#include <type_traits>
#include <memory>
#include <string>
#include <vector>
#include <unordered_map>
#include <fstream>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1S.fwd.hh>
#include <ObjexxFCL/MArray1.fwd.hh>
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.fwd.hh>
#include <ObjexxFCL/string.functions.hh>

#include <nlohmann/json.hpp>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>
#include <UtilityRoutines.hh>

using json = nlohmann::json;

class IdfParser {
public:
	void initialize( json const & schema ) {
		if ( schema.is_null() ) return;
		const json & loc = schema[ "properties" ];
		for ( auto it = loc.begin(); it != loc.end(); ++it ) {
			std::string key = it.key();
			for ( char & c : key ) c = toupper( c );
			case_insensitive_keys[ key ] = it.key();
		}
	}

	std::unordered_map < std::string, std::string > case_insensitive_keys;

	json decode( std::string const & idf, json const & schema );

	json decode( std::string const & idf, json const & schema, bool & success );

	std::string encode( json const & root, json const & schema );

	enum class Token : size_t {
		NONE = 0, END = 1, EXCLAMATION = 2, COMMA = 3, SEMICOLON = 4, STRING = 5, NUMBER = 6
	};

	json parse_idf( std::string const & idf, size_t & index, bool & success, json const & schema );

	json parse_object( std::string const & idf, size_t & index, bool & success, json const & schema_loc,
	                   json const & obj_loc );

	void add_missing_field_value( std::string const & field_name, json & root, json & extensible, json const & obj_loc,
	                              json const & loc, int legacy_idd_index );

	json parse_value( std::string const & idf, size_t & index, bool & success, json const & field_loc );

	json parse_number( std::string const & idf, size_t & index, bool & success );

	std::string parse_string( std::string const & idf, size_t & index, bool & success );

	void eat_whitespace( std::string const & idf, size_t & index );

	void eat_comment( std::string const & idf, size_t & index );

	void print_out_line_error( std::string const & idf, bool obj_found );

	void increment_both_index( size_t & index, size_t & line_index );

	void decrement_both_index( size_t & index, size_t & line_index );

	Token look_ahead( std::string const & idf, size_t index );

	Token next_token( std::string const & idf, size_t & index );

	bool icompare( std::string const & s1, std::string const & s2 ) {
		if ( s1.length() == s2.length() ) {
			return std::equal( s2.begin(),
			                   s2.end(),
			                   s1.begin(),
			                   [ ]( unsigned char c1, unsigned char c2 ) { return tolower( c1 ) == tolower( c2 ); } );
		}
		return false;
	}

	inline std::string rtrim( std::string & s ) {
		if ( s.size() == 0 ) return std::string();
		for ( size_t i = s.size() - 1; i > 0; i-- ) {
			if ( s[ i ] != ' ' ) break;
			s.erase( i );
		}
		return s;
	}

	std::string read_from_file( std::string const & input_file_name ) {
		std::ifstream in( input_file_name, std::ios::in | std::ios::binary );
		if ( in ) {
			return ( std::string( ( std::istreambuf_iterator < char >( in ) ), std::istreambuf_iterator < char >() ) );
		}
		throw ( errno );
	}

private:
	size_t cur_line_num = 1;
	size_t index_into_cur_line = 0;
	size_t beginning_of_line_index = 0;
	char s[ 129 ];
};

class State {
	json const * schema;
	std::vector < json const * > stack;
	std::unordered_map < std::string, bool > obj_required, extensible_required, root_required;
	// this design decision was made because
	// the choice was between sorting a vector for binary searching or log time object lookup in a map
	std::string cur_obj_name = "";

	unsigned prev_line_index = 0, prev_key_len = 0;
	unsigned cur_obj_count = 0;
	bool is_in_extensibles = false, does_key_exist = true, need_new_object_name = true;
	json::parse_event_t last_seen_event = json::parse_event_t::object_start;
	char s[ 129 ];
	char s2[ 129 ];

public:
	void initialize( json const * parsed_schema );

	void traverse( json::parse_event_t & event, json & parsed, unsigned line_num, unsigned line_index );

	void validate( json & parsed, unsigned line_num, unsigned line_index );

	std::vector < std::string > errors, warnings;

	int print_errors() {
		if ( warnings.size() ) EnergyPlus::ShowContinueError("Warnings: " + std::to_string(errors.size()));
		for ( auto const & s: warnings ) EnergyPlus::ShowContinueError( s );
		if ( errors.size() ) EnergyPlus::ShowWarningError("Errors: " + std::to_string(errors.size()));
		for ( auto const & s : errors ) EnergyPlus::ShowWarningError( s );
		return errors.size() + warnings.size();
	}

	bool icompare( std::string const & s1, std::string const & s2 ) {
		if ( s1.length() == s2.length() ) {
			return std::equal( s2.begin(),
			                   s2.end(),
			                   s1.begin(),
			                   [ ]( unsigned char c1, unsigned char c2 ) { return tolower( c1 ) == tolower( c2 ); } );
		}
		return false;
	}

	void add_error( std::string err, double val, unsigned line_num, unsigned line_index ) {
		std::string str = "Value \"" + std::to_string( val ) + "\" parsed at line " + std::to_string( line_num )
		                  + " (index " + std::to_string( line_index ) + ")";
		if ( err == "max" ) {
			errors.push_back( str + " exceeds maximum" );
		} else if ( err == "exmax" ) {
			errors.push_back( str + " exceeds or equals exclusive maximum" );
		} else if ( err == "min" ) {
			errors.push_back( str + " is less than the minimum" );
		} else if ( err == "exmin" ) {
			errors.push_back( str + " is less than or equal to the exclusive minimum" );
		}
	}
};

namespace EnergyPlus {

	class InputProcessor {
	private:
		static char s[ 129 ];

	public:
		static IdfParser idf_parser;
		static State state;
		static json schema;
		static json jdf;
		static std::ostream * echo_stream;

		template < class T >
		struct is_shared_ptr : std::false_type {};
		template < class T >
		struct is_shared_ptr < std::shared_ptr < T > > : std::true_type {};

		// Clears the global data in InputProcessor.
		// Needed for unit tests, should not be normally called.
		static void
		clear_state();

		static
		void
		InitFiles();

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
			Array1S < Real64 > Numbers,
			int & NumNumbers,
			int & Status,
			Optional < Array1D_bool > NumBlank = _,
			Optional < Array1D_bool > AlphaBlank = _,
			Optional < Array1D_string > AlphaFieldNames = _,
			Optional < Array1D_string > NumericFieldNames = _
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
		Real64
		ProcessNumber(
			std::string const & String,
			bool & ErrorFlag
		);

		static
		int
		FindItemInList(
			std::string const & String,
			Array1_string const & ListOfItems,
			int const NumItems
		);

		static
		inline
		int
		FindItemInList(
			std::string const & String,
			Array1_string const & ListOfItems
		) {
			return FindItemInList( String, ListOfItems, ListOfItems.isize() );
		}

		static
		int
		FindItemInList(
			std::string const & String,
			Array1S_string const ListOfItems,
			int const NumItems
		);

		static
		inline
		int
		FindItemInList(
			std::string const & String,
			Array1S_string const ListOfItems
		) {
			return FindItemInList( String, ListOfItems, ListOfItems.isize() );
		}

		template < typename A >
		static
		inline
		int
		FindItemInList(
			std::string const & String,
			MArray1 < A, std::string > const & ListOfItems,
			int const NumItems
		) {
			for ( int Count = 1; Count <= NumItems; ++Count ) {
				if ( String == ListOfItems( Count ) ) return Count;
			}
			return 0; // Not found
		}

		template < typename A >
		static
		inline
		int
		FindItemInList(
			std::string const & String,
			MArray1 < A, std::string > const & ListOfItems
		) {
			return FindItemInList( String, ListOfItems, ListOfItems.isize() );
		}

		template < typename Container, class = typename std::enable_if < !std::is_same < typename Container::value_type, std::string >::value >::type >
		// Container needs and operator[i] and elements need Name
		static
		inline
		int
		FindItemInList(
			std::string const & String,
			Container const & ListOfItems,
			int const NumItems
		) {
			for ( typename Container::size_type i = 0, e = NumItems; i < e; ++i ) {
				if ( String == ListOfItems[ i ].Name ) return int( i + 1 ); // 1-based return index
			}
			return 0; // Not found
		}

		template < typename Container, class = typename std::enable_if < !std::is_same < typename Container::value_type, std::string >::value >::type >
		// Container needs isize() and operator[i] and elements need Name
		static
		inline
		int
		FindItemInList(
			std::string const & String,
			Container const & ListOfItems
		) {
			return FindItemInList( String, ListOfItems, ListOfItems.isize() );
		}

		template < typename Container, class = typename std::enable_if < !std::is_same < typename Container::value_type, std::string >::value >::type >
		// Container needs operator[i] and value_type
		static
		inline
		int
		FindItemInList(
			std::string const & String,
			Container const & ListOfItems,
			std::string Container::value_type::*name_p,
			int const NumItems
		) {
			for ( typename Container::size_type i = 0, e = NumItems; i < e; ++i ) {
				if ( String == ListOfItems[ i ].*name_p ) return int( i + 1 ); // 1-based return index
			}
			return 0; // Not found
		}

		template < typename Container, class = typename std::enable_if < !std::is_same < typename Container::value_type, std::string >::value >::type >
		// Container needs isize() and operator[i] and value_type
		static
		inline
		int
		FindItemInList(
			std::string const & String,
			Container const & ListOfItems,
			std::string Container::value_type::*name_p
		) {
			return FindItemInList( String, ListOfItems, name_p, ListOfItems.isize() );
		}

		static
		int
		FindItemInSortedList(
			std::string const & String,
			Array1S_string const ListOfItems,
			int const NumItems
		);

		inline
		static
		int
		FindItemInSortedList(
			std::string const & String,
			Array1S_string const ListOfItems
		) {
			return FindItemInSortedList( String, ListOfItems, ListOfItems.isize() );
		}

		template < typename A >
		static
		inline
		int
		FindItemInSortedList(
			std::string const & String,
			MArray1 < A, std::string > const & ListOfItems,
			int const NumItems
		) {
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

		template < typename A >
		static
		inline
		int
		FindItemInSortedList(
			std::string const & String,
			MArray1 < A, std::string > const & ListOfItems
		) {
			return FindItemInSortedList( String, ListOfItems, ListOfItems.isize() );
		}

		template < typename InputIterator >
		static
		inline
		int
		FindItem(
			InputIterator first,
			InputIterator last,
			std::string const & str,
			std::false_type
		) {
			using valueType = typename std::iterator_traits < InputIterator >::value_type;
			//static_assert( std::is_convertible< decltype( std::declval< valueType >() ), Named >::value, "Iterator value must inherit from class Named" );

			auto const it = std::find_if( first, last, [ &str ]( const valueType & s ) { return s.name == str; } );
			if ( it != last ) return it - first + 1; // 1-based return index

			auto const it2 = std::find_if( first, last,
			                               [ &str ]( const valueType & s ) { return equali( s.name, str ); } );
			if ( it2 != last ) return it2 - first + 1; // 1-based return index

			return 0; // Not found
		}

		template < typename InputIterator >
		static
		inline
		int
		FindItem(
			InputIterator first,
			InputIterator last,
			std::string const & str,
			std::true_type
		) {
			using valueType = typename std::iterator_traits < InputIterator >::value_type;
			//static_assert( std::is_convertible< decltype( *std::declval< valueType >() ), Named >::value, "Iterator value must inherit from class Named" );

			auto const it = std::find_if( first, last, [ &str ]( const valueType & s ) { return s->name == str; } );
			if ( it != last ) return it - first + 1; // 1-based return index

			auto const it2 = std::find_if( first, last,
			                               [ &str ]( const valueType & s ) { return equali( s->name, str ); } );
			if ( it2 != last ) return it2 - first + 1; // 1-based return index

			return 0; // Not found
		}

		template < typename InputIterator >
		static
		inline
		int
		FindItem(
			InputIterator first,
			InputIterator last,
			std::string const & str
		) {
			return FindItem( first, last, str,
			                 is_shared_ptr < typename std::iterator_traits < InputIterator >::value_type >{ } );
		}

		static
		int
		FindItem(
			std::string const & String,
			Array1D_string const & ListOfItems,
			int const NumItems
		);

		inline
		static
		int
		FindItem(
			std::string const & String,
			Array1D_string const & ListOfItems
		) {
			return FindItem( String, ListOfItems, ListOfItems.isize() );
		}

		static
		int
		FindItem(
			std::string const & String,
			Array1S_string const ListOfItems,
			int const NumItems
		);

		inline
		static
		int
		FindItem(
			std::string const & String,
			Array1S_string const ListOfItems
		) {
			return FindItem( String, ListOfItems, ListOfItems.isize() );
		}

		template < typename A >
		static
		inline
		int
		FindItem(
			std::string const & String,
			MArray1 < A, std::string > const & ListOfItems,
			int const NumItems
		) {
			int const item_number( FindItemInList( String, ListOfItems, NumItems ) );
			if ( item_number != 0 ) return item_number;
			for ( int Count = 1; Count <= NumItems; ++Count ) {
				if ( equali( String, ListOfItems( Count ) ) ) return Count;
			}
			return 0; // Not found
		}

		template < typename A >
		static
		inline
		int
		FindItem(
			std::string const & String,
			MArray1 < A, std::string > const & ListOfItems
		) {
			return FindItem( String, ListOfItems, ListOfItems.isize() );
		}

		template < typename Container, class = typename std::enable_if < !std::is_same < typename Container::value_type, std::string >::value >::type >
		// Container needs size() and operator[i] and elements need Name
		static
		inline
		int
		FindItem(
			std::string const & String,
			Container const & ListOfItems,
			int const NumItems
		) {
			int const item_number( FindItemInList( String, ListOfItems, NumItems ) );
			if ( item_number != 0 ) return item_number;
			for ( typename Container::size_type i = 0, e = NumItems; i < e; ++i ) {
				if ( equali( String, ListOfItems[ i ].Name ) ) return i + 1; // 1-based return index
			}
			return 0; // Not found
		}

		template < typename Container, class = typename std::enable_if < !std::is_same < typename Container::value_type, std::string >::value >::type >
		// Container needs size() and operator[i] and elements need Name
		static
		inline
		int
		FindItem(
			std::string const & String,
			Container const & ListOfItems
		) {
			return FindItem( String, ListOfItems, ListOfItems.isize() );
		}

		template < typename Container, class = typename std::enable_if < !std::is_same < typename Container::value_type, std::string >::value >::type >
		// Container needs size() and operator[i] and value_type
		static
		inline
		int
		FindItem(
			std::string const & String,
			Container const & ListOfItems,
			std::string Container::value_type::*name_p,
			int const NumItems
		) {
			int const item_number( FindItemInList( String, ListOfItems, name_p, NumItems ) );
			if ( item_number != 0 ) return item_number;
			for ( typename Container::size_type i = 0, e = NumItems; i < e; ++i ) {
				if ( equali( String, ListOfItems[ i ].*name_p ) ) return i + 1; // 1-based return index
			}
			return 0; // Not found
		}

		template < typename Container, class = typename std::enable_if < !std::is_same < typename Container::value_type, std::string >::value >::type >
		// Container needs size() and operator[i] and value_type
		static
		inline
		int
		FindItem(
			std::string const & String,
			Container const & ListOfItems,
			std::string Container::value_type::*name_p
		) {
			return FindItem( String, ListOfItems, name_p, ListOfItems.isize() );
		}

		static
		std::string
		MakeUPPERCase( std::string const & InputString ); // Input String

		std::string
		deAllCaps( std::string const & );


		typedef char const * c_cstring;

		inline
		static
		bool
		SameString( std::string const & s, std::string const & t ) {
			// case insensitive comparison
			return equali( s, t );
		}

		inline
		static
		bool
		SameString( std::string const & s, c_cstring const & t ) {
			// case insensitive comparison
			return equali( s, t );
		}

		inline
		static
		bool
		SameString( c_cstring const & s, std::string const & t ) {
			// case insensitive comparison
			return equali( s, t );
		}

		inline
		static
		bool
		SameString( c_cstring const & s, c_cstring const & t ) {
			// case insensitive comparison
			return equali( s, t );
		}

		template < typename InputIterator >
		static
		inline
		void
		VerifyName(
			InputIterator first,
			InputIterator last,
			std::string const & NameToVerify,
			bool & ErrorFound,
			bool & IsBlank,
			std::string const & StringToDisplay
		) {
			// IsBlank = false;
			// ErrorFound = false;
			// if ( NameToVerify.empty() ) {
			// 	ShowSevereError( StringToDisplay + ", cannot be blank" );
			// 	ErrorFound = true;
			// 	IsBlank = true;
			// 	return;
			// }
			// int Found = FindItem( first, last, NameToVerify );
			// if ( Found != 0 ) {
			// 	ShowSevereError( StringToDisplay + ", duplicate name=" + NameToVerify );
			// 	ErrorFound = true;
			// }
		}

		static
		void
		VerifyName(
			std::string const & NameToVerify,
			Array1D_string const & NamesList,
			int const NumOfNames,
			bool & ErrorFound,
			bool & IsBlank,
			std::string const & StringToDisplay
		);

		static
		void
		VerifyName(
			std::string const & NameToVerify,
			Array1S_string const NamesList,
			int const NumOfNames,
			bool & ErrorFound,
			bool & IsBlank,
			std::string const & StringToDisplay
		);

		template < typename A >
		static
		inline
		void
		VerifyName(
			std::string const & NameToVerify,
			MArray1 < A, std::string > const & NamesList,
			int const NumOfNames,
			bool & ErrorFound,
			bool & IsBlank,
			std::string const & StringToDisplay
		) { // Overload for member arrays: Implemented here to avoid copy to Array_string to forward to other VerifyName
			// ErrorFound = false;
			// if ( NumOfNames > 0 ) {
			// 	int const Found = FindItem( NameToVerify, NamesList,
			// 	                            NumOfNames ); // Calls FindItem overload that accepts member arrays
			// 	if ( Found != 0 ) {
			// 		ShowSevereError( StringToDisplay + ", duplicate name=" + NameToVerify );
			// 		ErrorFound = true;
			// 	}
			// }

			// if ( NameToVerify.empty() ) {
			// 	ShowSevereError( StringToDisplay + ", cannot be blank" );
			// 	ErrorFound = true;
			// 	IsBlank = true;
			// } else {
			// 	IsBlank = false;
			// }
		}

		template < typename Container, class = typename std::enable_if < !std::is_same < typename Container::value_type, std::string >::value >::type >
		// Container needs size() and operator[i] and elements need Name
		static
		inline
		void
		VerifyName(
			std::string const & NameToVerify,
			Container const & NamesList,
			int const NumOfNames,
			bool & ErrorFound,
			bool & IsBlank,
			std::string const & StringToDisplay
		) {
			// ErrorFound = false;
			// if ( NumOfNames > 0 ) {
			// 	int const Found = FindItem( NameToVerify, NamesList,
			// 	                            NumOfNames ); // Calls FindItem overload that accepts member arrays
			// 	if ( Found != 0 ) {
			// 		ShowSevereError( StringToDisplay + ", duplicate name=" + NameToVerify );
			// 		ErrorFound = true;
			// 	}
			// }

			// if ( NameToVerify.empty() ) {
			// 	ShowSevereError( StringToDisplay + ", cannot be blank" );
			// 	ErrorFound = true;
			// 	IsBlank = true;
			// } else {
			// 	IsBlank = false;
			// }
		}

		template < typename Container, class = typename std::enable_if < !std::is_same < typename Container::value_type, std::string >::value >::type >
		// Container needs size() and operator[i] and value_type
		static
		inline
		void
		VerifyName(
			std::string const & NameToVerify,
			Container const & NamesList,
			std::string Container::value_type::*name_p,
			int const NumOfNames,
			bool & ErrorFound,
			bool & IsBlank,
			std::string const & StringToDisplay
		) {
			// ErrorFound = false;
			// if ( NumOfNames > 0 ) {
			// 	int const Found = FindItem( NameToVerify, NamesList, name_p, NumOfNames );
			// 	if ( Found != 0 ) {
			// 		ShowSevereError( StringToDisplay + ", duplicate name=" + NameToVerify );
			// 		ErrorFound = true;
			// 	}
			// }

			// if ( NameToVerify.empty() ) {
			// 	ShowSevereError( StringToDisplay + ", cannot be blank" );
			// 	ErrorFound = true;
			// 	IsBlank = true;
			// } else {
			// 	IsBlank = false;
			// }
		}

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

//	void
//	TurnOnReportRangeCheckErrors();

//	void
//	TurnOffReportRangeCheckErrors();

		static
		int
		GetNumRangeCheckErrorsFound();

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

//	void
//	ReportOrphanRecordObjects();

		void
		PreProcessorCheck( bool & PreP_Fatal ); // True if a preprocessor flags a fatal error

//	void
//	CompactObjectsCheck();

//	void
//	ParametricObjectsCheck();

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

//	void
//	ShowAuditErrorMessage(
//		std::string const & Severity, // if blank, does not add to sum
//		std::string const & ErrorMessage
//	);

		std::string
		IPTrimSigDigits( int const IntegerValue );

//		void
//		ReportOrphanRecordObjects();

	}; // InputProcessor
}

#endif
