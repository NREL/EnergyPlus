#ifndef InputProcessor_hh_INCLUDED
#define InputProcessor_hh_INCLUDED

// C++ Headers
#include <iosfwd>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array1S.hh>
#include <ObjexxFCL/Optional.hh>
#include <ObjexxFCL/string.functions.hh>
#include <ObjexxFCL/Vector2.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace InputProcessor {

	// Using/Aliasing

	// Data
	//MODULE PARAMETER DEFINITIONS
	extern int const ObjectDefAllocInc; // Starting number of Objects allowed in IDD as well as the increment
	// when max is reached
	extern int const ANArgsDefAllocInc; // The increment when max total args is reached
	extern int const SectionDefAllocInc; // Starting number of Sections allowed in IDD as well as the increment
	// when max is reached
	extern int const SectionsIDFAllocInc; // Initial number of Sections allowed in IDF as well as the increment
	// when max is reached
	extern int const ObjectsIDFAllocInc; // Initial number of Objects allowed in IDF as well as the increment
	// when max is reached
	extern std::string::size_type const MaxObjectNameLength; // Maximum number of characters in an Object Name
	extern std::string::size_type const MaxSectionNameLength; // Maximum number of characters in a Section Name
	extern std::string::size_type const MaxAlphaArgLength; // Maximum number of characters in an Alpha Argument
	extern std::string::size_type const MaxInputLineLength; // Maximum number of characters in an input line (in.idf, energy+.idd)
	extern std::string::size_type const MaxFieldNameLength; // Maximum number of characters in a field name string
	extern std::string const Blank;
	extern Real64 const DefAutoSizeValue;
	extern Real64 const DefAutoCalculateValue;

	// DERIVED TYPE DEFINITIONS

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// MODULE VARIABLE DECLARATIONS:

	//Integer Variables for the Module
	extern int NumObjectDefs; // Count of number of object definitions found in the IDD
	extern int NumSectionDefs; // Count of number of section defintions found in the IDD
	extern int MaxObjectDefs; // Current "max" object defs (IDD), when reached will be reallocated and new Max set
	extern int MaxSectionDefs; // Current "max" section defs (IDD), when reached will be reallocated and new Max set
	extern int NumLines; // Count of number of lines in IDF
	extern int MaxIDFRecords; // Current "max" IDF records (lines), when reached will be reallocated and new Max set
	extern int NumIDFRecords; // Count of number of IDF records
	extern int MaxIDFSections; // Current "max" IDF sections (lines), when reached will be reallocated and new Max set
	extern int NumIDFSections; // Count of number of IDF records
	extern int EchoInputFile; // Unit number of the file echoing the IDD and input records (eplusout.audit)
	extern int InputLineLength; // Actual input line length or position of comment character
	extern int MaxAlphaArgsFound; // Count of max alpha args found in the IDD
	extern int MaxNumericArgsFound; // Count of max numeric args found in the IDD
	extern int NumAlphaArgsFound; // Count of max alpha args found in the IDD
	extern int NumNumericArgsFound; // Count of max numeric args found in the IDD
	extern int MaxAlphaIDFArgsFound; // Count of max alpha args found in the IDF
	extern int MaxNumericIDFArgsFound; // Count of max numeric args found in the IDF
	extern int MaxAlphaIDFDefArgsFound; // Count of max alpha args found in the IDF
	extern int MaxNumericIDFDefArgsFound; // Count of max numeric args found in the IDF
	extern int NumOutOfRangeErrorsFound; // Count of number of "out of range" errors found
	extern int NumBlankReqFieldFound; // Count of number of blank required field errors found
	extern int NumMiscErrorsFound; // Count of other errors found
	extern int MinimumNumberOfFields; // When ReadLine discovers a "minimum" number of fields for an object, this variable is set
	extern int NumObsoleteObjects; // Number of \obsolete objects
	extern int TotalAuditErrors; // Counting some warnings that go onto only the audit file
	extern int NumSecretObjects; // Number of objects in "Secret Mode"
	extern bool ProcessingIDD; // True when processing IDD, false when processing IDF
	extern std::ostream * echo_stream; // Internal stream used for input file echoing (used for performance)

	//Real Variables for Module
	//na

	//Character Variables for Module
	extern std::string InputLine;
	extern Array1D_string ListOfSections;
	extern Array1D_string ListOfObjects;
	extern Array1D_int iListOfObjects;
	extern Array1D_int ObjectGotCount;
	extern Array1D_int ObjectStartRecord;
	extern std::string CurrentFieldName; // Current Field Name (IDD)
	extern Array1D_string ObsoleteObjectsRepNames; // Array of Replacement names for Obsolete objects
	extern std::string ReplacementName;

	//Logical Variables for Module
	extern bool OverallErrorFlag; // If errors found during parse of IDF, will fatal at end
	extern bool EchoInputLine; // Usually True, if the IDD is backspaced, then is set to false, then back to true
	extern bool ReportRangeCheckErrors; // Module level reporting logical, can be turned off from outside the module (and then
	// must be turned back on.
	extern bool FieldSet; // Set to true when ReadInputLine has just scanned a "field"
	extern bool RequiredField; // Set to true when ReadInputLine has determined that this field is required
	extern bool RetainCaseFlag; // Set to true when ReadInputLine has determined that this field should retain case
	extern bool ObsoleteObject; // Set to true when ReadInputLine has an obsolete object
	extern bool RequiredObject; // Set to true when ReadInputLine has a required object
	extern bool UniqueObject; // Set to true when ReadInputLine has a unique object
	extern bool ExtensibleObject; // Set to true when ReadInputLine has an extensible object
	extern int ExtensibleNumFields; // set to number when ReadInputLine has an extensible object
	extern Array1D_bool IDFRecordsGotten; // Denotes that this record has been "gotten" from the IDF

	//Derived Types Variables

	// Types

	struct RangeCheckDef
	{
		// Members
		bool MinMaxChk; // true when Min/Max has been added
		int FieldNumber; // which field number this is
		std::string FieldName; // Name of the field
		Vector2_string MinMaxString; // appropriate Min/Max Strings
		Vector2< Real64 > MinMaxValue; // appropriate Min/Max Values
		Vector2_int WhichMinMax; // =0 (none/invalid), =1 \min, =2 \min>, =3 \max, =4 \max<
		bool DefaultChk; // true when default has been entered
		Real64 Default; // Default value
		bool DefAutoSize; // Default value is "autosize"
		bool AutoSizable; // True if this field can be autosized
		Real64 AutoSizeValue; // Value to return for autosize field
		bool DefAutoCalculate; // Default value is "autocalculate"
		bool AutoCalculatable; // True if this field can be autocalculated
		Real64 AutoCalculateValue; // Value to return for autocalculate field

		// Default Constructor
		RangeCheckDef() :
			MinMaxChk( false ),
			FieldNumber( 0 ),
			MinMaxValue( 0.0 ),
			WhichMinMax( 0 ),
			DefaultChk( false ),
			Default( 0.0 ),
			DefAutoSize( false ),
			AutoSizable( false ),
			AutoSizeValue( 0.0 ),
			DefAutoCalculate( false ),
			AutoCalculatable( false ),
			AutoCalculateValue( 0.0 )
		{}

		// Member Constructor
		RangeCheckDef(
			bool const MinMaxChk, // true when Min/Max has been added
			int const FieldNumber, // which field number this is
			std::string const & FieldName, // Name of the field
			Vector2_string const & MinMaxString, // appropriate Min/Max Strings
			Vector2< Real64 > const & MinMaxValue, // appropriate Min/Max Values
			Vector2_int const & WhichMinMax, // =0 (none/invalid), =1 \min, =2 \min>, =3 \max, =4 \max<
			bool const DefaultChk, // true when default has been entered
			Real64 const Default, // Default value
			bool const DefAutoSize, // Default value is "autosize"
			bool const AutoSizable, // True if this field can be autosized
			Real64 const AutoSizeValue, // Value to return for autosize field
			bool const DefAutoCalculate, // Default value is "autocalculate"
			bool const AutoCalculatable, // True if this field can be autocalculated
			Real64 const AutoCalculateValue // Value to return for autocalculate field
		) :
			MinMaxChk( MinMaxChk ),
			FieldNumber( FieldNumber ),
			FieldName( FieldName ),
			MinMaxString( MinMaxString ),
			MinMaxValue( MinMaxValue ),
			WhichMinMax( WhichMinMax ),
			DefaultChk( DefaultChk ),
			Default( Default ),
			DefAutoSize( DefAutoSize ),
			AutoSizable( AutoSizable ),
			AutoSizeValue( AutoSizeValue ),
			DefAutoCalculate( DefAutoCalculate ),
			AutoCalculatable( AutoCalculatable ),
			AutoCalculateValue( AutoCalculateValue )
		{}

	};

	struct ObjectsDefinition
	{
		// Members
		std::string Name; // Name of the Object
		int NumParams; // Number of parameters to be processed for each object
		int NumAlpha; // Number of Alpha elements in the object
		int NumNumeric; // Number of Numeric elements in the object
		int MinNumFields; // Minimum number of fields to be passed to the Get routines
		bool NameAlpha1; // True if the first alpha appears to "name" the object for error messages
		bool UniqueObject; // True if this object has been designated \unique-object
		bool RequiredObject; // True if this object has been designated \required-object
		bool ExtensibleObject; // True if this object has been designated \extensible
		int ExtensibleNum; // how many fields to extend
		int LastExtendAlpha; // Count for extended alpha fields
		int LastExtendNum; // Count for extended numeric fields
		int ObsPtr; // If > 0, object is obsolete and this is the
		// Pointer to ObsoleteObjectRepNames Array for replacement object
		Array1D_bool AlphaOrNumeric; // Positionally, whether the argument
		// is alpha (true) or numeric (false)
		Array1D_bool ReqField; // True for required fields
		Array1D_bool AlphRetainCase; // true if retaincase is set for this field (alpha fields only)
		Array1D_string AlphFieldChks; // Field names for alphas
		Array1D_string AlphFieldDefs; // Defaults for alphas
		Array1D< RangeCheckDef > NumRangeChks; // Used to range check and default numeric fields
		int NumFound; // Number of this object found in IDF

		// Default Constructor
		ObjectsDefinition() :
			NumParams( 0 ),
			NumAlpha( 0 ),
			NumNumeric( 0 ),
			MinNumFields( 0 ),
			NameAlpha1( false ),
			UniqueObject( false ),
			RequiredObject( false ),
			ExtensibleObject( false ),
			ExtensibleNum( 0 ),
			LastExtendAlpha( 0 ),
			LastExtendNum( 0 ),
			ObsPtr( 0 ),
			NumFound( 0 )
		{}

		// Member Constructor
		ObjectsDefinition(
			std::string const & EP_UNUSED( Name ), // Name of the Object
			int const NumParams, // Number of parameters to be processed for each object
			int const NumAlpha, // Number of Alpha elements in the object
			int const NumNumeric, // Number of Numeric elements in the object
			int const MinNumFields, // Minimum number of fields to be passed to the Get routines
			bool const NameAlpha1, // True if the first alpha appears to "name" the object for error messages
			bool const UniqueObject, // True if this object has been designated \unique-object
			bool const RequiredObject, // True if this object has been designated \required-object
			bool const ExtensibleObject, // True if this object has been designated \extensible
			int const ExtensibleNum, // how many fields to extend
			int const LastExtendAlpha, // Count for extended alpha fields
			int const LastExtendNum, // Count for extended numeric fields
			int const ObsPtr, // If > 0, object is obsolete and this is the
			Array1_bool const & AlphaOrNumeric, // Positionally, whether the argument
			Array1_bool const & ReqField, // True for required fields
			Array1_bool const & AlphRetainCase, // true if retaincase is set for this field (alpha fields only)
			Array1_string const & AlphFieldChks, // Field names for alphas
			Array1_string const & AlphFieldDefs, // Defaults for alphas
			Array1< RangeCheckDef > const & NumRangeChks, // Used to range check and default numeric fields
			int const NumFound // Number of this object found in IDF
		) :
			NumParams( NumParams ),
			NumAlpha( NumAlpha ),
			NumNumeric( NumNumeric ),
			MinNumFields( MinNumFields ),
			NameAlpha1( NameAlpha1 ),
			UniqueObject( UniqueObject ),
			RequiredObject( RequiredObject ),
			ExtensibleObject( ExtensibleObject ),
			ExtensibleNum( ExtensibleNum ),
			LastExtendAlpha( LastExtendAlpha ),
			LastExtendNum( LastExtendNum ),
			ObsPtr( ObsPtr ),
			AlphaOrNumeric( AlphaOrNumeric ),
			ReqField( ReqField ),
			AlphRetainCase( AlphRetainCase ),
			AlphFieldChks( AlphFieldChks ),
			AlphFieldDefs( AlphFieldDefs ),
			NumRangeChks( NumRangeChks ),
			NumFound( NumFound )
		{}

	};

	struct SectionsDefinition
	{
		// Members
		std::string Name; // Name of the Section
		int NumFound; // Number of this object found in IDF

		// Default Constructor
		SectionsDefinition() :
			NumFound( 0 )
		{}

		// Member Constructor
		SectionsDefinition(
			std::string const & Name, // Name of the Section
			int const NumFound // Number of this object found in IDF
		) :
			Name( Name ),
			NumFound( NumFound )
		{}

	};

	struct FileSectionsDefinition
	{
		// Members
		std::string Name; // Name of this section
		int FirstRecord; // Record number of first object in section
		int FirstLineNo; // Record number of first object in section
		int LastRecord; // Record number of last object in section

		// Default Constructor
		FileSectionsDefinition() :
			FirstRecord( 0 ),
			FirstLineNo( 0 ),
			LastRecord( 0 )
		{}

		// Member Constructor
		FileSectionsDefinition(
			std::string const & Name, // Name of this section
			int const FirstRecord, // Record number of first object in section
			int const FirstLineNo, // Record number of first object in section
			int const LastRecord // Record number of last object in section
		) :
			Name( Name ),
			FirstRecord( FirstRecord ),
			FirstLineNo( FirstLineNo ),
			LastRecord( LastRecord )
		{}

	};

	struct LineDefinition // Will be saved for each "object" input
	{
		// Members
		// The arrays (Alphas, Numbers) will be dimensioned to be
		// the size expected from the definition.
		std::string Name; // Object name for this record
		int NumAlphas; // Number of alphas on this record
		int NumNumbers; // Number of numbers on this record
		int ObjectDefPtr; // Which Object Def is this
		Array1D_string Alphas; // Storage for the alphas
		Array1D_bool AlphBlank; // Set to true if this field was blank on input
		Array1D< Real64 > Numbers; // Storage for the numbers
		Array1D_bool NumBlank; // Set to true if this field was blank on input

		// Default Constructor
		LineDefinition() :
			NumAlphas( 0 ),
			NumNumbers( 0 ),
			ObjectDefPtr( 0 )
		{}

		// Member Constructor
		LineDefinition(
			std::string const & EP_UNUSED( Name ), // Object name for this record
			int const NumAlphas, // Number of alphas on this record
			int const NumNumbers, // Number of numbers on this record
			int const ObjectDefPtr, // Which Object Def is this
			Array1_string const & Alphas, // Storage for the alphas
			Array1_bool const & AlphBlank, // Set to true if this field was blank on input
			Array1< Real64 > const & Numbers, // Storage for the numbers
			Array1_bool const & NumBlank // Set to true if this field was blank on input
		) :
			NumAlphas( NumAlphas ),
			NumNumbers( NumNumbers ),
			ObjectDefPtr( ObjectDefPtr ),
			Alphas( Alphas ),
			AlphBlank( AlphBlank ),
			Numbers( Numbers ),
			NumBlank( NumBlank )
		{}

	};

	struct SecretObjects
	{
		// Members
		std::string OldName; // Old Object Name
		std::string NewName; // New Object Name if applicable
		bool Deleted; // true if this (old name) was deleted
		bool Used; // true when used (and reported) in this input file
		bool Transitioned; // true if old name will be transitioned to new object within IP
		bool TransitionDefer; // true if old name will be transitioned to new object within IP

		// Default Constructor
		SecretObjects() :
			Deleted( false ),
			Used( false ),
			Transitioned( false ),
			TransitionDefer( false )
		{}

		// Member Constructor
		SecretObjects(
			std::string const & OldName, // Old Object Name
			std::string const & NewName, // New Object Name if applicable
			bool const Deleted, // true if this (old name) was deleted
			bool const Used, // true when used (and reported) in this input file
			bool const Transitioned, // true if old name will be transitioned to new object within IP
			bool const TransitionDefer // true if old name will be transitioned to new object within IP
		) :
			OldName( OldName ),
			NewName( NewName ),
			Deleted( Deleted ),
			Used( Used ),
			Transitioned( Transitioned ),
			TransitionDefer( TransitionDefer )
		{}

	};

	// Object Data
	extern Array1D< ObjectsDefinition > ObjectDef; // Contains all the Valid Objects on the IDD
	extern Array1D< SectionsDefinition > SectionDef; // Contains all the Valid Sections on the IDD
	extern Array1D< FileSectionsDefinition > SectionsOnFile; // lists the sections on file (IDF)
	extern LineDefinition LineItem; // Description of current record
	extern Array1D< LineDefinition > IDFRecords; // All the objects read from the IDF
	extern Array1D< SecretObjects > RepObjects; // Secret Objects that could replace old ones

	// Functions

	// Clears the global data in InputProcessor.
	// Needed for unit tests, should not be normally called.
	void
	clear_state();

	void
	ProcessInput();

	void
	ProcessDataDicFile(
		std::istream & idd_stream,
		bool & ErrorsFound // set to true if any errors flagged during IDD processing
	);

	void
	AddSectionDef(
		std::string const & ProposedSection, // Proposed Section to be added
		bool & ErrorsFound // set to true if errors found here
	);

	void
	AddObjectDefandParse(
		std::istream & idd_stream,
		std::string const & ProposedObject, // Proposed Object to Add
		std::string::size_type & CurPos, // Current position (initially at first ',') of InputLine
		bool & EndofFile, // End of File marker
		bool & ErrorsFound // set to true if errors found here
	);

	void
	ProcessInputDataFile( std::istream & idf_stream );

	void
	ValidateSection(
		std::string const & ProposedSection,
		int const LineNo
	);

	void
	ValidateObjectandParse(
		std::istream & idf_stream,
		std::string const & ProposedObject,
		std::string::size_type & CurPos,
		bool & EndofFile
	);

	void
	ValidateSectionsInput();

	int
	GetNumSectionsFound( std::string const & SectionWord );

	int
	GetNumSectionsinInput();

	void
	GetListofSectionsinInput(
		Array1S_string SectionList,
		int & NuminList
	);

	int
	GetNumObjectsFound( std::string const & ObjectWord );

	void
	GetRecordLocations(
		int const Which,
		int & FirstRecord,
		int & LastRecord
	);

	void
	GetObjectItem(
		std::string const & Object,
		int const Number,
		Array1S_string Alphas,
		int & NumAlphas,
		Array1S< Real64 > Numbers,
		int & NumNumbers,
		int & Status,
		Optional< Array1_bool > NumBlank = _,
		Optional< Array1_bool > AlphaBlank = _,
		Optional< Array1_string > AlphaFieldNames = _,
		Optional< Array1_string > NumericFieldNames = _
	);

	int
	GetObjectItemNum(
		std::string const & ObjType, // Object Type (ref: IDD Objects)
		std::string const & ObjName // Name of the object type
	);

	void
	TellMeHowManyObjectItemArgs(
		std::string const & Object,
		int const Number,
		int & NumAlpha,
		int & NumNumbers,
		int & Status
	);

	void
	GetObjectItemfromFile(
		int const Which,
		std::string & ObjectWord,
		int & NumAlpha,
		int & NumNumeric,
		Optional< Array1S_string > AlphaArgs = _,
		Optional< Array1S< Real64 > > NumericArgs = _,
		Optional< Array1S_bool > AlphaBlanks = _,
		Optional< Array1S_bool > NumericBlanks = _
	);

	// Utility Functions/Routines for Module

	void
	ReadInputLine(
		std::istream & in_stream,
		std::string::size_type & CurPos,
		bool & BlankLine,
		bool & EndofFile
	);

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
	);

	void
	ExtendObjectDefinition(
		int const ObjectNum, // Number of the object definition to be extended.
		int & NumNewArgsLimit // Number of the parameters after extension
	);

	Real64
	ProcessNumber(
		std::string const & String,
		bool & ErrorFlag
	);

	void
	ProcessMinMaxDefLine(
		std::string const & partLine, // part of input line starting \min or \max
		int & WhichMinMax, // =0 (none/invalid), =1 \min, =2 \min>, =3 \max, =4 \max<
		std::string & MinMaxString,
		Real64 & Value,
		std::string & DefaultString,
		int & ErrLevel
	);

	int
	FindItemInList(
		std::string const & String,
		Array1S_string const ListOfItems,
		int const NumItems
	);

	template< typename A >
	inline
	int
	FindItemInList(
		std::string const & String,
		MArray1< A, std::string > const & ListOfItems,
		int const NumItems
	)
	{
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
	);

	template< typename A >
	inline
	int
	FindItemInSortedList(
		std::string const & String,
		MArray1< A, std::string > const & ListOfItems,
		int const NumItems
	)
	{
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
		Array1S_string const ListOfItems,
		int const NumItems
	);

	template< typename A >
	inline
	int
	FindItem(
		std::string const & String,
		MArray1< A, std::string > const & ListOfItems,
		int const NumItems
	)
	{
		int const item_number( FindItemInList( String, ListOfItems, NumItems ) );
		if ( item_number != 0 ) return item_number;
		for ( int Count = 1; Count <= NumItems; ++Count ) {
			if ( equali( String, ListOfItems( Count ) ) ) return Count;
		}
		return 0; // Not found
	}

	std::string
	MakeUPPERCase( std::string const & InputString ); // Input String

	typedef char const * c_cstring;

	inline
	bool
	SameString( std::string const & s, std::string const & t )
	{
		// case insensitive comparison
		return equali( s, t );
	}

	inline
	bool
	SameString( std::string const & s, c_cstring const & t )
	{
		// case insensitive comparison
		return equali( s, t );
	}

	inline
	bool
	SameString( c_cstring const & s, std::string const & t )
	{
		// case insensitive comparison
		return equali( s, t );
	}

	inline
	bool
	SameString( c_cstring const & s, c_cstring const & t )
	{
		// case insensitive comparison
		return equali( s, t );
	}

	void
	VerifyName(
		std::string const & NameToVerify,
		Array1S_string const NamesList,
		int const NumOfNames,
		bool & ErrorFound,
		bool & IsBlank,
		std::string const & StringToDisplay
	);

	template< typename A >
	inline
	void
	VerifyName(
		std::string const & NameToVerify,
		MArray1< A, std::string > const & NamesList,
		int const NumOfNames,
		bool & ErrorFound,
		bool & IsBlank,
		std::string const & StringToDisplay
	)
	{ // Overload for member arrays: Implemented here to avoid copy to Array_string to forward to other VerifyName
		int Found;

		ErrorFound = false;
		if ( NumOfNames > 0 ) {
			Found = FindItem( NameToVerify, NamesList, NumOfNames ); // Calls FindItem overload that accepts member arrays
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
		Optional_string_const LowerBoundString = _, // String for error message, if applicable
		Optional_bool_const LowerBoundCondition = _, // Condition for error condition, if applicable
		Optional_string_const UpperBoundString = _, // String for error message, if applicable
		Optional_bool_const UpperBoundCondition = _, // Condition for error condition, if applicable
		Optional_string_const ValueString = _, // Value with digits if to be displayed with error
		Optional_string_const WhatObjectName = _ // ObjectName -- used for error messages
	);

	void
	InternalRangeCheck(
		Real64 const Value,
		int const FieldNumber,
		int const WhichObject,
		std::string const & PossibleAlpha,
		bool const AutoSizable,
		bool const AutoCalculatable
	);

	void
	TurnOnReportRangeCheckErrors();

	void
	TurnOffReportRangeCheckErrors();

	int
	GetNumRangeCheckErrorsFound();

	//==============================================================================
	// The following routines allow access to the definition lines of the IDD and
	// thus can be used to "report" on expected arguments for the Input Processor.

	int
	GetNumObjectsInIDD();

	void
	GetListOfObjectsInIDD(
		Array1S_string ObjectNames, // List of Object Names (from IDD)
		int & Number // Number in List
	);

	void
	GetObjectDefInIDD(
		std::string const & ObjectWord, // Object for definition
		int & NumArgs, // How many arguments (max) this Object can have
		Array1S_bool AlphaOrNumeric, // Array designating Alpha (true) or Numeric (false) for each
		Array1S_bool RequiredFields, // Array designating RequiredFields (true) for each argument
		int & MinNumFields // Minimum Number of Fields to be returned to Get routines
	);

	void
	GetObjectDefMaxArgs(
		std::string const & ObjectWord, // Object for definition
		int & NumArgs, // How many arguments (max) this Object can have
		int & NumAlpha, // How many Alpha arguments (max) this Object can have
		int & NumNumeric // How many Numeric arguments (max) this Object can have
	);

	void
	GetIDFRecordsStats(
		int & iNumberOfRecords, // Number of IDF Records
		int & iNumberOfDefaultedFields, // Number of defaulted fields in IDF
		int & iTotalFieldsWithDefaults, // Total number of fields that could be defaulted
		int & iNumberOfAutoSizedFields, // Number of autosized fields in IDF
		int & iTotalAutoSizableFields, // Total number of autosizeable fields
		int & iNumberOfAutoCalcedFields, // Total number of autocalculate fields
		int & iTotalAutoCalculatableFields // Total number of autocalculatable fields
	);

	void
	ReportOrphanRecordObjects();

	void
	InitSecretObjects();

	void
	MakeTransition( int & ObjPtr ); // Pointer to Object Definition

	void
	AddRecordFromSection( int const Which ); // Which object was matched

	void
	PreProcessorCheck( bool & PreP_Fatal ); // True if a preprocessor flags a fatal error

	void
	CompactObjectsCheck();

	void
	ParametricObjectsCheck();

	void
	PreScanReportingVariables();

	void
	AddVariablesForMonthlyReport( std::string const & reportName );

	int
	FindFirstRecord( std::string const & UCObjType );

	int
	FindNextRecord(
		std::string const & UCObjType,
		int const StartPointer
	);

	void
	AddRecordToOutputVariableStructure(
		std::string const & KeyValue,
		std::string const & VariableName
	);

	void
	ReAllocateAndPreserveOutputVariablesForSimulation();

	void
	DumpCurrentLineBuffer(
		int const StartLine,
		std::string const & cStartLine,
		std::string const & cStartName,
		int const CurLine,
		int const NumConxLines,
		Array1S_string const LineBuf,
		int const CurQPtr
	);

	void
	ShowAuditErrorMessage(
		std::string const & Severity, // if blank, does not add to sum
		std::string const & ErrorMessage
	);

	std::string
	IPTrimSigDigits( int const IntegerValue );

	//     NOTICE

	//     Copyright (c) 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.

	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in main.cc.

	//     NOTICE: The U.S. Government is granted for itself and others acting on its
	//     behalf a paid-up, nonexclusive, irrevocable, worldwide license in this data to
	//     reproduce, prepare derivative works, and perform publicly and display publicly.
	//     Beginning five (5) years after permission to assert copyright is granted,
	//     subject to two possible five year renewals, the U.S. Government is granted for
	//     itself and others acting on its behalf a paid-up, non-exclusive, irrevocable
	//     worldwide license in this data to reproduce, prepare derivative works,
	//     distribute copies to the public, perform publicly and display publicly, and to
	//     permit others to do so.

	//     TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.

} // InputProcessor

} // EnergyPlus

#endif
