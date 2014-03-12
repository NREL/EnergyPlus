#ifndef InputProcessor_hh_INCLUDED
#define InputProcessor_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>
#include <ObjexxFCL/FArray1S.hh>
#include <ObjexxFCL/Fstring.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace InputProcessor {

	// Using/Aliasing
	using DataGlobals::MaxNameLength;

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
	extern int const MaxObjectNameLength; // Maximum number of characters in an Object Name
	extern int const MaxSectionNameLength; // Maximum number of characters in a Section Name
	extern int const MaxAlphaArgLength; // Maximum number of characters in an Alpha Argument
	extern int const MaxInputLineLength; // Maximum number of characters in an input line (in.idf, energy+.idd)
	extern int const MaxFieldNameLength; // Maximum number of characters in a field name string
	extern Fstring const Blank;
	extern Fstring const AlphaNum; // Valid indicators for Alpha or Numeric fields (A or N)
	extern Fstring const fmta;
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
	extern int IDDFile; // Unit number for reading IDD (Energy+.idd)
	extern int IDFFile; // Unit number for reading IDF (in.idf)
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

	//Real Variables for Module
	//na

	//Character Variables for Module
	extern Fstring InputLine; // Each line can be up to MaxInputLineLength characters long
	extern FArray1D_Fstring ListOfSections;
	extern FArray1D_Fstring ListOfObjects;
	extern FArray1D_int iListOfObjects;
	extern FArray1D_int ObjectGotCount;
	extern FArray1D_int ObjectStartRecord;
	extern Fstring CurrentFieldName; // Current Field Name (IDD)
	extern FArray1D_Fstring ObsoleteObjectsRepNames; // Array of Replacement names for Obsolete objects
	extern Fstring ReplacementName;

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
	extern bool StripCR; // If true, strip last character (<cr> off each schedule:file line)
	extern int ExtensibleNumFields; // set to number when ReadInputLine has an extensible object
	extern FArray1D_bool IDFRecordsGotten; // Denotes that this record has been "gotten" from the IDF

	//Derived Types Variables

	// Types

	struct RangeCheckDef
	{
		// Members
		bool MinMaxChk; // true when Min/Max has been added
		int FieldNumber; // which field number this is
		Fstring FieldName; // Name of the field
		FArray1D_Fstring MinMaxString; // appropriate Min/Max Strings
		FArray1D< Real64 > MinMaxValue; // appropriate Min/Max Values
		FArray1D_int WhichMinMax; // =0 (none/invalid), =1 \min, =2 \min>, =3 \max, =4 \max<
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
			FieldName( MaxFieldNameLength, Blank ),
			MinMaxString( 2, sFstring( 32 ), Blank ),
			MinMaxValue( 2, 0.0 ),
			WhichMinMax( 2, 0 ),
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
			Fstring const & FieldName, // Name of the field
			FArray1_Fstring const & MinMaxString, // appropriate Min/Max Strings
			FArray1< Real64 > const & MinMaxValue, // appropriate Min/Max Values
			FArray1_int const & WhichMinMax, // =0 (none/invalid), =1 \min, =2 \min>, =3 \max, =4 \max<
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
			FieldName( MaxFieldNameLength, FieldName ),
			MinMaxString( 2, sFstring( 32 ), MinMaxString ),
			MinMaxValue( 2, MinMaxValue ),
			WhichMinMax( 2, WhichMinMax ),
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
		Fstring Name; // Name of the Object
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
		FArray1D_bool AlphaOrNumeric; // Positionally, whether the argument
		// is alpha (true) or numeric (false)
		FArray1D_bool ReqField; // True for required fields
		FArray1D_bool AlphRetainCase; // true if retaincase is set for this field (alpha fields only)
		FArray1D_Fstring AlphFieldChks; // Field names for alphas
		FArray1D_Fstring AlphFieldDefs; // Defaults for alphas
		FArray1D< RangeCheckDef > NumRangeChks; // Used to range check and default numeric fields
		int NumFound; // Number of this object found in IDF

		// Default Constructor
		ObjectsDefinition() :
			Name( MaxObjectNameLength, Blank ),
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
			AlphFieldChks( sFstring( MaxFieldNameLength ) ),
			AlphFieldDefs( sFstring( MaxNameLength ) ),
			NumFound( 0 )
		{}

		// Member Constructor
		ObjectsDefinition(
			Fstring const & Name, // Name of the Object
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
			FArray1_bool const & AlphaOrNumeric, // Positionally, whether the argument
			FArray1_bool const & ReqField, // True for required fields
			FArray1_bool const & AlphRetainCase, // true if retaincase is set for this field (alpha fields only)
			FArray1_Fstring const & AlphFieldChks, // Field names for alphas
			FArray1_Fstring const & AlphFieldDefs, // Defaults for alphas
			FArray1< RangeCheckDef > const & NumRangeChks, // Used to range check and default numeric fields
			int const NumFound // Number of this object found in IDF
		) :
			Name( MaxObjectNameLength, Name ),
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
		Fstring Name; // Name of the Section
		int NumFound; // Number of this object found in IDF

		// Default Constructor
		SectionsDefinition() :
			Name( MaxSectionNameLength, Blank ),
			NumFound( 0 )
		{}

		// Member Constructor
		SectionsDefinition(
			Fstring const & Name, // Name of the Section
			int const NumFound // Number of this object found in IDF
		) :
			Name( MaxSectionNameLength, Name ),
			NumFound( NumFound )
		{}

	};

	struct FileSectionsDefinition
	{
		// Members
		Fstring Name; // Name of this section
		int FirstRecord; // Record number of first object in section
		int FirstLineNo; // Record number of first object in section
		int LastRecord; // Record number of last object in section

		// Default Constructor
		FileSectionsDefinition() :
			Name( MaxSectionNameLength, Blank ),
			FirstRecord( 0 ),
			FirstLineNo( 0 ),
			LastRecord( 0 )
		{}

		// Member Constructor
		FileSectionsDefinition(
			Fstring const & Name, // Name of this section
			int const FirstRecord, // Record number of first object in section
			int const FirstLineNo, // Record number of first object in section
			int const LastRecord // Record number of last object in section
		) :
			Name( MaxSectionNameLength, Name ),
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
		Fstring Name; // Object name for this record
		int NumAlphas; // Number of alphas on this record
		int NumNumbers; // Number of numbers on this record
		int ObjectDefPtr; // Which Object Def is this
		FArray1D_Fstring Alphas; // Storage for the alphas
		FArray1D_bool AlphBlank; // Set to true if this field was blank on input
		FArray1D< Real64 > Numbers; // Storage for the numbers
		FArray1D_bool NumBlank; // Set to true if this field was blank on input

		// Default Constructor
		LineDefinition() :
			Name( MaxObjectNameLength, Blank ),
			NumAlphas( 0 ),
			NumNumbers( 0 ),
			ObjectDefPtr( 0 ),
			Alphas( sFstring( MaxAlphaArgLength ) )
		{}

		// Member Constructor
		LineDefinition(
			Fstring const & Name, // Object name for this record
			int const NumAlphas, // Number of alphas on this record
			int const NumNumbers, // Number of numbers on this record
			int const ObjectDefPtr, // Which Object Def is this
			FArray1_Fstring const & Alphas, // Storage for the alphas
			FArray1_bool const & AlphBlank, // Set to true if this field was blank on input
			FArray1< Real64 > const & Numbers, // Storage for the numbers
			FArray1_bool const & NumBlank // Set to true if this field was blank on input
		) :
			Name( MaxObjectNameLength, Name ),
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
		Fstring OldName; // Old Object Name
		Fstring NewName; // New Object Name if applicable
		bool Deleted; // true if this (old name) was deleted
		bool Used; // true when used (and reported) in this input file
		bool Transitioned; // true if old name will be transitioned to new object within IP
		bool TransitionDefer; // true if old name will be transitioned to new object within IP

		// Default Constructor
		SecretObjects() :
			OldName( MaxObjectNameLength, Blank ),
			NewName( MaxObjectNameLength, Blank ),
			Deleted( false ),
			Used( false ),
			Transitioned( false ),
			TransitionDefer( false )
		{}

		// Member Constructor
		SecretObjects(
			Fstring const & OldName, // Old Object Name
			Fstring const & NewName, // New Object Name if applicable
			bool const Deleted, // true if this (old name) was deleted
			bool const Used, // true when used (and reported) in this input file
			bool const Transitioned, // true if old name will be transitioned to new object within IP
			bool const TransitionDefer // true if old name will be transitioned to new object within IP
		) :
			OldName( MaxObjectNameLength, OldName ),
			NewName( MaxObjectNameLength, NewName ),
			Deleted( Deleted ),
			Used( Used ),
			Transitioned( Transitioned ),
			TransitionDefer( TransitionDefer )
		{}

	};

	// Object Data
	extern FArray1D< ObjectsDefinition > ObjectDef; // Contains all the Valid Objects on the IDD
	extern FArray1D< SectionsDefinition > SectionDef; // Contains all the Valid Sections on the IDD
	extern FArray1D< FileSectionsDefinition > SectionsOnFile; // lists the sections on file (IDF)
	extern LineDefinition LineItem; // Description of current record
	extern FArray1D< LineDefinition > IDFRecords; // All the objects read from the IDF
	extern FArray1D< SecretObjects > RepObjects; // Secret Objects that could replace old ones

	// Functions

	void
	ProcessInput();

	void
	ProcessDataDicFile( bool & ErrorsFound ); // set to true if any errors flagged during IDD processing

	void
	AddSectionDef(
		Fstring const & ProposedSection, // Proposed Section to be added
		bool & ErrorsFound // set to true if errors found here
	);

	void
	AddObjectDefandParse(
		Fstring const & ProposedObject, // Proposed Object to Add
		int & CurPos, // Current position (initially at first ',') of InputLine
		bool & EndofFile, // End of File marker
		bool & ErrorsFound // set to true if errors found here
	);

	void
	ProcessInputDataFile();

	void
	ValidateSection(
		Fstring const & ProposedSection,
		int const LineNo
	);

	void
	ValidateObjectandParse(
		Fstring const & ProposedObject,
		int & CurPos,
		bool & EndofFile
	);

	void
	ValidateSectionsInput();

	int
	GetNumSectionsFound( Fstring const & SectionWord );

	int
	GetNumSectionsinInput();

	void
	GetListofSectionsinInput(
		FArray1S_Fstring SectionList,
		int & NuminList
	);

	int
	GetNumObjectsFound( Fstring const & ObjectWord );

	void
	GetRecordLocations(
		int const Which,
		int & FirstRecord,
		int & LastRecord
	);

	void
	GetObjectItem(
		Fstring const & Object,
		int const Number,
		FArray1S_Fstring Alphas,
		int & NumAlphas,
		FArray1S< Real64 > Numbers,
		int & NumNumbers,
		int & Status,
		Optional< FArray1_bool > NumBlank = _,
		Optional< FArray1_bool > AlphaBlank = _,
		Optional< FArray1_Fstring > AlphaFieldNames = _,
		Optional< FArray1_Fstring > NumericFieldNames = _
	);

	int
	GetObjectItemNum(
		Fstring const & ObjType, // Object Type (ref: IDD Objects)
		Fstring const & ObjName // Name of the object type
	);

	void
	TellMeHowManyObjectItemArgs(
		Fstring const & Object,
		int const Number,
		int & NumAlpha,
		int & NumNumbers,
		int & Status
	);

	void
	GetObjectItemfromFile(
		int const Which,
		Fstring & ObjectWord,
		int & NumAlpha,
		int & NumNumeric,
		Optional< FArray1S_Fstring > AlphaArgs = _,
		Optional< FArray1S< Real64 > > NumericArgs = _,
		Optional< FArray1S_bool > AlphaBlanks = _,
		Optional< FArray1S_bool > NumericBlanks = _
	);

	// Utility Functions/Routines for Module

	void
	ReadInputLine(
		int const UnitNumber,
		int & CurPos,
		bool & BlankLine,
		int & InputLineLength,
		bool & EndofFile,
		Optional_bool MinMax = _,
		Optional_int WhichMinMax = _, // =0 (none/invalid), =1 \min, =2 \min>, =3 \max, =4 \max< //Autodesk:OPTIONAL Used without PRESENT check
		Optional_Fstring MinMaxString = _, //Autodesk:OPTIONAL Used without PRESENT check
		Optional< Real64 > Value = _, //Autodesk:OPTIONAL Used without PRESENT check
		Optional_bool Default = _,
		Optional_Fstring DefString = _, //Autodesk:OPTIONAL Used without PRESENT check
		Optional_bool AutoSizable = _,
		Optional_bool AutoCalculatable = _,
		Optional_bool RetainCase = _, //Autodesk:OPTIONAL Used without PRESENT check
		Optional_bool ErrorsFound = _ //Autodesk:OPTIONAL Used without PRESENT check
	);

	void
	ExtendObjectDefinition(
		int const ObjectNum, // Number of the object definition to be extended.
		int & NumNewArgsLimit // Number of the parameters after extension
	);

	Real64
	ProcessNumber(
		Fstring const & String,
		bool & ErrorFlag
	);

	void
	ProcessMinMaxDefLine(
		Fstring const & UCInputLine, // part of input line starting \min or \max
		int & WhichMinMax, // =0 (none/invalid), =1 \min, =2 \min>, =3 \max, =4 \max<
		Fstring & MinMaxString,
		Real64 & Value,
		Fstring & DefaultString,
		int & ErrLevel
	);

	int
	FindItemInList(
		Fstring const & String,
		FArray1S_Fstring const ListOfItems,
		int const NumItems
	);

	template< typename A >
	inline
	int
	FindItemInList(
		Fstring const & String,
		MArray1< A, Fstring > const & ListOfItems,
		int const NumItems
	)
	{
		return FindItemInList( String, FArray1D_Fstring( ListOfItems ), NumItems );
	}

	int
	FindItemInSortedList(
		Fstring const & String,
		FArray1S_Fstring const ListOfItems,
		int const NumItems
	);

	template< typename A >
	inline
	int
	FindItemInSortedList(
		Fstring const & String,
		MArray1< A, Fstring > const & ListOfItems,
		int const NumItems
	)
	{
		return FindItemInSortedList( String, FArray1D_Fstring( ListOfItems ), NumItems );
	}

	int
	FindItem(
		Fstring const & String,
		FArray1S_Fstring const ListOfItems,
		int const NumItems
	);

	template< typename A >
	inline
	int
	FindItem(
		Fstring const & String,
		MArray1< A, Fstring > const & ListOfItems,
		int const NumItems
	)
	{
		return FindItem( String, FArray1D_Fstring( ListOfItems ), NumItems );
	}

	Fstring
	MakeUPPERCase( Fstring const & InputString ); // Input String

	bool
	SameString(
		Fstring const & TestString1, // First String to Test
		Fstring const & TestString2 // Second String to Test
	);

	void
	VerifyName(
		Fstring const & NameToVerify,
		FArray1S_Fstring const NamesList,
		int const NumOfNames,
		bool & ErrorFound,
		bool & IsBlank,
		Fstring const & StringToDisplay
	);

	template< typename A >
	inline
	void
	VerifyName(
		Fstring const & NameToVerify,
		MArray1< A, Fstring > const & NamesList,
		int const NumOfNames,
		bool & ErrorFound,
		bool & IsBlank,
		Fstring const & StringToDisplay
	)
	{
		VerifyName( NameToVerify, FArray1D_Fstring( NamesList ), NumOfNames, ErrorFound, IsBlank, StringToDisplay );
	}

	void
	RangeCheck(
		bool & ErrorsFound, // Set to true if error detected
		Fstring const & WhatFieldString, // Descriptive field for string
		Fstring const & WhatObjectString, // Descriptive field for object, Zone Name, etc.
		Fstring const & ErrorLevel, // 'Warning','Severe','Fatal')
		Optional_Fstring_const LowerBoundString = _, // String for error message, if applicable
		Optional_bool_const LowerBoundCondition = _, // Condition for error condition, if applicable
		Optional_Fstring_const UpperBoundString = _, // String for error message, if applicable
		Optional_bool_const UpperBoundCondition = _, // Condition for error condition, if applicable
		Optional_Fstring_const ValueString = _, // Value with digits if to be displayed with error
		Optional_Fstring_const WhatObjectName = _ // ObjectName -- used for error messages
	);

	void
	InternalRangeCheck(
		Real64 const Value,
		int const FieldNumber,
		int const WhichObject,
		Fstring const & PossibleAlpha,
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
		FArray1S_Fstring ObjectNames, // List of Object Names (from IDD)
		int & Number // Number in List
	);

	void
	GetObjectDefInIDD(
		Fstring const & ObjectWord, // Object for definition
		int & NumArgs, // How many arguments (max) this Object can have
		FArray1S_bool AlphaOrNumeric, // Array designating Alpha (true) or Numeric (false) for each
		FArray1S_bool RequiredFields, // Array designating RequiredFields (true) for each argument
		int & MinNumFields // Minimum Number of Fields to be returned to Get routines
	);

	void
	GetObjectDefMaxArgs(
		Fstring const & ObjectWord, // Object for definition
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
	AddVariablesForMonthlyReport( Fstring const & reportName );

	int
	FindFirstRecord( Fstring const & UCObjType );

	int
	FindNextRecord(
		Fstring const & UCObjType,
		int const StartPointer
	);

	void
	AddRecordToOutputVariableStructure(
		Fstring const & KeyValue,
		Fstring const & VariableName
	);

	void
	ReAllocateAndPreserveOutputVariablesForSimulation();

	void
	DumpCurrentLineBuffer(
		int const StartLine,
		Fstring const & cStartLine,
		Fstring const & cStartName,
		int const CurLine,
		int const NumConxLines,
		FArray1S_Fstring const LineBuf,
		int const CurQPtr
	);

	void
	ShowAuditErrorMessage(
		Fstring const & Severity, // if blank, does not add to sum
		Fstring const & ErrorMessage
	);

	Fstring
	IPTrimSigDigits( int const IntegerValue );

	//     NOTICE

	//     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.

	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in EnergyPlus.f90.

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
