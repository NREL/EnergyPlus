#ifndef OutputReportTabular_hh_INCLUDED
#define OutputReportTabular_hh_INCLUDED

// C++ Headers
#include <fstream>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array1S.hh>
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Array2S.hh>
#include <ObjexxFCL/Array3D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace OutputReportTabular {

	// Using/Aliasing

	// Data
	//MODULE PARAMETER DEFINITIONS:

	extern int const MaxHeaderLength;
	extern int const MaxNoteLength;

	extern int const aggTypeSumOrAvg;
	extern int const aggTypeMaximum;
	extern int const aggTypeMinimum;
	extern int const aggTypeValueWhenMaxMin;
	extern int const aggTypeHoursZero;
	extern int const aggTypeHoursNonZero;
	extern int const aggTypeHoursPositive;
	extern int const aggTypeHoursNonPositive;
	extern int const aggTypeHoursNegative;
	extern int const aggTypeHoursNonNegative;
	extern int const aggTypeSumOrAverageHoursShown;
	extern int const aggTypeMaximumDuringHoursShown;
	extern int const aggTypeMinimumDuringHoursShown;

	extern int const tableStyleComma;
	extern int const tableStyleTab;
	extern int const tableStyleFixed;
	extern int const tableStyleHTML;
	extern int const tableStyleXML;

	extern int const unitsStyleNone; // no change to any units
	extern int const unitsStyleJtoKWH;
	extern int const unitsStyleJtoMJ;
	extern int const unitsStyleJtoGJ;
	extern int const unitsStyleInchPound;
	extern int const unitsStyleNotFound;

	extern int const isAverage;
	extern int const isSum;

	extern int const stepTypeZone;
	extern int const stepTypeHVAC;

	// BEPS Report Related Variables
	// From Report:Table:Predefined - BEPS
	extern int const numResourceTypes;
	extern int const numSourceTypes;

	//MODULE VARIABLE DECLARATIONS:

	// The Binned table type is different and only references one variable and its structure is very
	// different from the others so it is has its own type.

	// arrays for time binned results

	extern int OutputTableBinnedCount;
	extern int BinResultsTableCount;
	extern int BinResultsIntervalCount;

	extern int const numNamedMonthly;
	// These reports are detailed/named in routine InitializePredefinedMonthlyTitles

	extern int MonthlyInputCount;
	extern int sizeMonthlyInput;
	extern int MonthlyFieldSetInputCount;
	extern int sizeMonthlyFieldSetInput;
	extern int MonthlyTablesCount;
	extern int MonthlyColumnsCount;
	extern Array1D_bool IsMonthGathered; // shown as true for any month used

	extern int TOCEntriesCount;
	extern int TOCEntriesSize;

	extern int UnitConvSize;

	extern bool WriteTabularFiles;

	// Allow up to five output files to be created
	extern int const maxNumStyles;

	// From Report:Table:Style
	extern int unitsStyle; // see list of parameters
	extern int numStyles;
	extern std::ofstream csv_stream; // CSV table stream
	extern std::ofstream tab_stream; // Tab table stream
	extern std::ofstream fix_stream; // Fixed table stream
	extern std::ofstream htm_stream; // HTML table stream
	extern std::ofstream xml_stream; // XML table stream
	extern Array1D< std::ofstream * > TabularOutputFile; // Table stream array
	extern Array1D_string del; // the delimiter to use
	extern Array1D_int TableStyle; // see list of parameters

	extern Real64 timeInYear;

	// Flags for predefined tabular reports
	extern bool displayTabularBEPS;
	extern bool displayLEEDSummary;
	extern bool displayTabularCompCosts; // added BTG 5/6/04 for component cost summary
	extern bool displayTabularVeriSum; // added JG 2006-06-28 for input verification and summary report
	extern bool displayComponentSizing;
	extern bool displaySurfaceShadowing;
	extern bool displayDemandEndUse;
	extern bool displayAdaptiveComfort;
	extern bool displaySourceEnergyEndUseSummary;
	extern bool displayZoneComponentLoadSummary;

	// BEPS Report Related Variables
	// From Report:Table:Predefined - BEPS
	// arrays that hold the meter numbers that are initialized at get input
	extern Array1D_int meterNumTotalsBEPS;
	extern Array1D_int meterNumTotalsSource;
	extern Array1D_bool fuelfactorsused;
	extern Array1D_bool ffUsed;
	extern Array1D< Real64 > SourceFactors;
	extern Array1D_bool ffSchedUsed;
	extern Array1D_int ffSchedIndex;
	extern Array2D_int meterNumEndUseBEPS;
	extern Array3D_int meterNumEndUseSubBEPS;
	// arrays that hold the names of the resource and end uses
	extern Array1D_string resourceTypeNames;
	extern Array1D_string sourceTypeNames;
	extern Array1D_string endUseNames;
	// arrays that hold the actual values for the year
	extern Array1D< Real64 > gatherTotalsBEPS;
	extern Array1D< Real64 > gatherTotalsBySourceBEPS;
	extern Array1D< Real64 > gatherTotalsSource;
	extern Array1D< Real64 > gatherTotalsBySource;
	extern Array2D< Real64 > gatherEndUseBEPS;
	extern Array2D< Real64 > gatherEndUseBySourceBEPS;
	extern Array3D< Real64 > gatherEndUseSubBEPS;
	// arrays the hold the demand values
	extern Array1D< Real64 > gatherDemandTotal;
	extern Array2D< Real64 > gatherDemandEndUse;
	extern Array3D< Real64 > gatherDemandEndUseSub;
	extern Array1D_int gatherDemandTimeStamp;
	// to keep track of hours for the BEPS report gathering
	extern Real64 gatherElapsedTimeBEPS;
	// for normalization of results
	extern Real64 buildingGrossFloorArea;
	extern Real64 buildingConditionedFloorArea;
	// keep track if schedules are used in fuel factors
	extern bool fuelFactorSchedulesUsed;
	// for electic load components on BEPS report
	extern int meterNumPowerFuelFireGen;
	extern Real64 gatherPowerFuelFireGen;
	extern int meterNumPowerPV;
	extern Real64 gatherPowerPV;
	extern int meterNumPowerWind;
	extern Real64 gatherPowerWind;
	extern Real64 OverallNetEnergyFromStorage;
	extern int meterNumPowerHTGeothermal;
	extern Real64 gatherPowerHTGeothermal;
	extern int meterNumElecProduced;
	extern Real64 gatherElecProduced;
	extern int meterNumElecPurchased;
	extern Real64 gatherElecPurchased;
	extern int meterNumElecSurplusSold;
	extern Real64 gatherElecSurplusSold;
	// for on site thermal source components on BEPS report
	extern int meterNumWaterHeatRecovery;
	extern Real64 gatherWaterHeatRecovery;
	extern int meterNumAirHeatRecoveryCool;
	extern Real64 gatherAirHeatRecoveryCool;
	extern int meterNumAirHeatRecoveryHeat;
	extern Real64 gatherAirHeatRecoveryHeat;
	extern int meterNumHeatHTGeothermal;
	extern Real64 gatherHeatHTGeothermal;
	extern int meterNumHeatSolarWater;
	extern Real64 gatherHeatSolarWater;
	extern int meterNumHeatSolarAir;
	extern Real64 gatherHeatSolarAir;
	// for on site water components on BEPS report
	extern int meterNumRainWater;
	extern Real64 gatherRainWater;
	extern int meterNumCondensate;
	extern Real64 gatherCondensate;
	extern int meterNumGroundwater;
	extern Real64 gatherWellwater;
	extern int meterNumMains;
	extern Real64 gatherMains;
	extern int meterNumWaterEndUseTotal;
	extern Real64 gatherWaterEndUseTotal;
	// for source energy conversion factors on BEPS report
	extern Real64 sourceFactorElectric;
	extern Real64 sourceFactorNaturalGas;
	extern Real64 efficiencyDistrictCooling;
	extern Real64 efficiencyDistrictHeating;
	extern Real64 sourceFactorSteam;
	extern Real64 sourceFactorGasoline;
	extern Real64 sourceFactorDiesel;
	extern Real64 sourceFactorCoal;
	extern Real64 sourceFactorFuelOil1;
	extern Real64 sourceFactorFuelOil2;
	extern Real64 sourceFactorPropane;
	extern Real64 sourceFactorOtherFuel1;
	extern Real64 sourceFactorOtherFuel2;

	extern Array1D_int td;
	//(1)   Current year
	//(2)   Current month
	//(3)   Current day
	//(4)   Time difference with respect to UTC in minutes (0-59)
	//(5)   Hour of the day (0-23)
	//(6)   Minutes (0-59)
	//(7)   Seconds (0-59)
	//(8)   Milliseconds (0-999)

	// Design day name storage
	extern Array1D_string DesignDayName;
	extern int DesignDayCount;

	//arrays related to pulse and load component reporting
	extern Array2D< Real64 > radiantPulseUsed;
	extern Array2D_int radiantPulseTimestep;
	extern Array2D< Real64 > radiantPulseReceived;
	extern Array3D< Real64 > loadConvectedNormal;
	extern Array3D< Real64 > loadConvectedWithPulse;
	extern Array3D< Real64 > netSurfRadSeq;
	extern Array2D< Real64 > decayCurveCool;
	extern Array2D< Real64 > decayCurveHeat;
	extern Array3D< Real64 > ITABSFseq; // used for determining the radiant fraction on each surface
	extern Array3D< Real64 > TMULTseq; // used for determining the radiant fraction on each surface

	extern Array3D< Real64 > peopleInstantSeq;
	extern Array3D< Real64 > peopleLatentSeq;
	extern Array3D< Real64 > peopleRadSeq;
	extern Array3D< Real64 > peopleDelaySeq;

	extern Array3D< Real64 > lightInstantSeq;
	extern Array3D< Real64 > lightRetAirSeq;
	extern Array3D< Real64 > lightLWRadSeq; // long wave thermal radiation
	extern Array3D< Real64 > lightSWRadSeq; // short wave visible radiation
	extern Array3D< Real64 > lightDelaySeq;

	extern Array3D< Real64 > equipInstantSeq;
	extern Array3D< Real64 > equipLatentSeq;
	extern Array3D< Real64 > equipRadSeq;
	extern Array3D< Real64 > equipDelaySeq;

	extern Array3D< Real64 > refrigInstantSeq;
	extern Array3D< Real64 > refrigRetAirSeq;
	extern Array3D< Real64 > refrigLatentSeq;

	extern Array3D< Real64 > waterUseInstantSeq;
	extern Array3D< Real64 > waterUseLatentSeq;

	extern Array3D< Real64 > hvacLossInstantSeq;
	extern Array3D< Real64 > hvacLossRadSeq;
	extern Array3D< Real64 > hvacLossDelaySeq;

	extern Array3D< Real64 > powerGenInstantSeq;
	extern Array3D< Real64 > powerGenRadSeq;
	extern Array3D< Real64 > powerGenDelaySeq;

	extern Array3D< Real64 > infilInstantSeq;
	extern Array3D< Real64 > infilLatentSeq;

	extern Array3D< Real64 > zoneVentInstantSeq;
	extern Array3D< Real64 > zoneVentLatentSeq;

	extern Array3D< Real64 > interZoneMixInstantSeq;
	extern Array3D< Real64 > interZoneMixLatentSeq;

	extern Array3D< Real64 > feneCondInstantSeq;
	//REAL(r64), DIMENSION(:,:,:),ALLOCATABLE,PUBLIC  :: feneSolarInstantSeq
	extern Array3D< Real64 > feneSolarRadSeq;
	extern Array3D< Real64 > feneSolarDelaySeq;

	extern Array3D< Real64 > surfDelaySeq;

	extern int maxUniqueKeyCount;

	// for the XML report must keep track fo the active sub-table name and report set by other routines
	extern std::string activeSubTableName;
	extern std::string activeReportNameNoSpace;
	extern std::string activeReportName;
	extern std::string activeForName;
	extern std::string prevReportName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE PrimaryPlantLoops
	//PRIVATE      DateToStr

	// Types

	struct OutputTableBinnedType
	{
		// Members
		std::string keyValue; // the key value (usually an asterisk to indicate all variables
		std::string varOrMeter; // the name of the variable or meter
		Real64 intervalStart; // The lowest value for the intervals being binned into.
		Real64 intervalSize; // The size of the bins starting with Interval start.
		int intervalCount; // The number of bins used. The number of hours below the start of
		// the lowest bin and above the value of the last bin are also shown.
		int resIndex; // result index - pointer to BinResults array
		int numTables;
		int typeOfVar; // 0=not found, 1=integer, 2=real, 3=meter
		int avgSum; // Variable  is Averaged=1 or Summed=2
		int stepType; // Variable time step is Zone=1 or HVAC=2
		std::string units; // the units string, may be blank
		std::string ScheduleName; // the name of the schedule
		int scheduleIndex; // index to the schedule specified - if no schedule use zero

		// Default Constructor
		OutputTableBinnedType() :
			intervalStart( 0.0 ),
			intervalSize( 0.0 ),
			intervalCount( 0 ),
			resIndex( 0 ),
			numTables( 0 ),
			typeOfVar( 0 ),
			avgSum( 0 ),
			stepType( 0 ),
			scheduleIndex( 0 )
		{}

		// Member Constructor
		OutputTableBinnedType(
			std::string const & keyValue, // the key value (usually an asterisk to indicate all variables
			std::string const & varOrMeter, // the name of the variable or meter
			Real64 const intervalStart, // The lowest value for the intervals being binned into.
			Real64 const intervalSize, // The size of the bins starting with Interval start.
			int const intervalCount, // The number of bins used. The number of hours below the start of
			int const resIndex, // result index - pointer to BinResults array
			int const numTables,
			int const typeOfVar, // 0=not found, 1=integer, 2=real, 3=meter
			int const avgSum, // Variable  is Averaged=1 or Summed=2
			int const stepType, // Variable time step is Zone=1 or HVAC=2
			std::string const & units, // the units string, may be blank
			std::string const & ScheduleName, // the name of the schedule
			int const scheduleIndex // index to the schedule specified - if no schedule use zero
		) :
			keyValue( keyValue ),
			varOrMeter( varOrMeter ),
			intervalStart( intervalStart ),
			intervalSize( intervalSize ),
			intervalCount( intervalCount ),
			resIndex( resIndex ),
			numTables( numTables ),
			typeOfVar( typeOfVar ),
			avgSum( avgSum ),
			stepType( stepType ),
			units( units ),
			ScheduleName( ScheduleName ),
			scheduleIndex( scheduleIndex )
		{}

	};

	struct BinResultsType
	{
		// Members
		Array1D< Real64 > mnth; // monthly bins
		Array1D< Real64 > hrly; // hourly bins

		// Default Constructor
		BinResultsType() :
			mnth( 12, 0.0 ),
			hrly( 24, 0.0 )
		{}

		// Member Constructor
		BinResultsType(
			Array1< Real64 > const & mnth, // monthly bins
			Array1< Real64 > const & hrly // hourly bins
		) :
			mnth( 12, mnth ),
			hrly( 24, hrly )
		{}

	};

	struct BinObjVarIDType
	{
		// Members
		std::string namesOfObj; // name of the object
		int varMeterNum; // variable or meter number

		// Default Constructor
		BinObjVarIDType() :
			varMeterNum( 0 )
		{}

		// Member Constructor
		BinObjVarIDType(
			std::string const & namesOfObj, // name of the object
			int const varMeterNum // variable or meter number
		) :
			namesOfObj( namesOfObj ),
			varMeterNum( varMeterNum )
		{}

	};

	struct BinStatisticsType
	{
		// Members
		Real64 sum; // sum of the variable
		Real64 sum2; // sum of the variable squared
		int n; // number of items in sum
		Real64 minimum; // minimum value
		Real64 maximum; // maximum value

		// Default Constructor
		BinStatisticsType() :
			sum( 0.0 ),
			sum2( 0.0 ),
			n( 0 ),
			minimum( 0.0 ),
			maximum( 0.0 )
		{}

		// Member Constructor
		BinStatisticsType(
			Real64 const sum, // sum of the variable
			Real64 const sum2, // sum of the variable squared
			int const n, // number of items in sum
			Real64 const minimum, // minimum value
			Real64 const maximum // maximum value
		) :
			sum( sum ),
			sum2( sum2 ),
			n( n ),
			minimum( minimum ),
			maximum( maximum )
		{}

	};

	struct NamedMonthlyType
	{
		// Members
		std::string title; // report title
		bool show; // if report should be shown

		// Default Constructor
		NamedMonthlyType() :
			show( false )
		{}

		// Member Constructor
		NamedMonthlyType(
			std::string const & title, // report title
			bool const show // if report should be shown
		) :
			title( title ),
			show( show )
		{}

	};

	struct MonthlyInputType
	{
		// Members
		std::string name; // identifier
		int numFieldSet; // number of monthly field sets
		int firstFieldSet; // pointer to the first field set
		int numTables; // number of tables
		int firstTable; // pointer to the first table
		int showDigits; // the number of digits to be shown

		// Default Constructor
		MonthlyInputType() :
			numFieldSet( 0 ),
			firstFieldSet( 0 ),
			numTables( 0 ),
			firstTable( 0 ),
			showDigits( 0 )
		{}

		// Member Constructor
		MonthlyInputType(
			std::string const & name, // identifier
			int const numFieldSet, // number of monthly field sets
			int const firstFieldSet, // pointer to the first field set
			int const numTables, // number of tables
			int const firstTable, // pointer to the first table
			int const showDigits // the number of digits to be shown
		) :
			name( name ),
			numFieldSet( numFieldSet ),
			firstFieldSet( firstFieldSet ),
			numTables( numTables ),
			firstTable( firstTable ),
			showDigits( showDigits )
		{}

	};

	struct MonthlyFieldSetInputType
	{
		// Members
		std::string variMeter; // the name of the variable or meter
		std::string colHead; // the column header to use instead of the variable name (only for predefined)
		int aggregate; // the type of aggregation for the variable (see aggType parameters)
		std::string varUnits; // Units sting, may be blank
		std::string variMeterUpper; // the name of the variable or meter uppercased
		int typeOfVar; // 0=not found, 1=integer, 2=real, 3=meter
		int keyCount; // noel
		int varAvgSum; // Variable  is Averaged=1 or Summed=2
		int varStepType; // Variable time step is Zone=1 or HVAC=2
		Array1D_string NamesOfKeys; // keyNames !noel
		Array1D_int IndexesForKeyVar; // keyVarIndexes !noel

		// Default Constructor
		MonthlyFieldSetInputType() :
			aggregate( 0 ),
			typeOfVar( 0 ),
			keyCount( 0 ),
			varAvgSum( 1 ),
			varStepType( 1 )
		{}

		// Member Constructor
		MonthlyFieldSetInputType(
			std::string const & variMeter, // the name of the variable or meter
			std::string const & colHead, // the column header to use instead of the variable name (only for predefined)
			int const aggregate, // the type of aggregation for the variable (see aggType parameters)
			std::string const & varUnits, // Units sting, may be blank
			std::string const & variMeterUpper, // the name of the variable or meter uppercased
			int const typeOfVar, // 0=not found, 1=integer, 2=real, 3=meter
			int const keyCount, // noel
			int const varAvgSum, // Variable  is Averaged=1 or Summed=2
			int const varStepType, // Variable time step is Zone=1 or HVAC=2
			Array1_string const & NamesOfKeys, // keyNames !noel
			Array1_int const & IndexesForKeyVar // keyVarIndexes !noel
		) :
			variMeter( variMeter ),
			colHead( colHead ),
			aggregate( aggregate ),
			varUnits( varUnits ),
			variMeterUpper( variMeterUpper ),
			typeOfVar( typeOfVar ),
			keyCount( keyCount ),
			varAvgSum( varAvgSum ),
			varStepType( varStepType ),
			NamesOfKeys( NamesOfKeys ),
			IndexesForKeyVar( IndexesForKeyVar )
		{}

	};

	struct MonthlyTablesType
	{
		// Members
		std::string keyValue; // the key value - the object names that result in the variable
		int firstColumn; // pointer to the monthly column array for the first item
		int numColumns; // number of columns for the table

		// Default Constructor
		MonthlyTablesType() :
			firstColumn( 0 ),
			numColumns( 0 )
		{}

		// Member Constructor
		MonthlyTablesType(
			std::string const & keyValue, // the key value - the object names that result in the variable
			int const firstColumn, // pointer to the monthly column array for the first item
			int const numColumns // number of columns for the table
		) :
			keyValue( keyValue ),
			firstColumn( firstColumn ),
			numColumns( numColumns )
		{}

	};

	struct MonthlyColumnsType
	{
		// Members
		std::string varName; // name of variable
		std::string colHead; // column header (not used for user defined monthly)
		int varNum; // variable or meter number
		int typeOfVar; // 0=not found, 1=integer, 2=real, 3=meter
		int avgSum; // Variable  is Averaged=1 or Summed=2
		int stepType; // Variable time step is Zone=1 or HVAC=2
		std::string units; // the units string, may be blank
		int aggType; // index to the type of aggregation (see list of parameters)
		Array1D< Real64 > reslt; // monthly results
		Array1D< Real64 > duration; // the time during which results are summed for use in averages
		Array1D_int timeStamp; // encoded timestamp of max or min
		Real64 aggForStep; // holds the aggregation for the HVAC time steps when smaller than
		// the zone timestep

		// Default Constructor
		MonthlyColumnsType() :
			varNum( 0 ),
			typeOfVar( 0 ),
			avgSum( 0 ),
			stepType( 0 ),
			aggType( 0 ),
			reslt( 12, 0.0 ),
			duration( 12, 0.0 ),
			timeStamp( 12, 0 ),
			aggForStep( 0.0 )
		{}

		// Member Constructor
		MonthlyColumnsType(
			std::string const & varName, // name of variable
			std::string const & colHead, // column header (not used for user defined monthly)
			int const varNum, // variable or meter number
			int const typeOfVar, // 0=not found, 1=integer, 2=real, 3=meter
			int const avgSum, // Variable  is Averaged=1 or Summed=2
			int const stepType, // Variable time step is Zone=1 or HVAC=2
			std::string const & units, // the units string, may be blank
			int const aggType, // index to the type of aggregation (see list of parameters)
			Array1< Real64 > const & reslt, // monthly results
			Array1< Real64 > const & duration, // the time during which results are summed for use in averages
			Array1_int const & timeStamp, // encoded timestamp of max or min
			Real64 const aggForStep // holds the aggregation for the HVAC time steps when smaller than
		) :
			varName( varName ),
			colHead( colHead ),
			varNum( varNum ),
			typeOfVar( typeOfVar ),
			avgSum( avgSum ),
			stepType( stepType ),
			units( units ),
			aggType( aggType ),
			reslt( 12, reslt ),
			duration( 12, duration ),
			timeStamp( 12, timeStamp ),
			aggForStep( aggForStep )
		{}

	};

	struct TOCEntriesType
	{
		// Members
		std::string reportName; // the name of the individual report
		std::string sectionName; // the name of the section containing individual reports
		bool isWritten; // flag if the entry has been written to TOC

		// Default Constructor
		TOCEntriesType() :
			isWritten( false )
		{}

		// Member Constructor
		TOCEntriesType(
			std::string const & reportName, // the name of the individual report
			std::string const & sectionName, // the name of the section containing individual reports
			bool const isWritten // flag if the entry has been written to TOC
		) :
			reportName( reportName ),
			sectionName( sectionName ),
			isWritten( isWritten )
		{}

	};

	struct UnitConvType
	{
		// Members
		std::string siName; // the name abbreviation or symbol of the SI units
		std::string ipName; // the name abbreviation or symbol of the IP units
		Real64 mult; // the multiplier used to convert from SI to IP in IP = (SI * mult) + offset
		Real64 offset; // the offset used to convert from SI to IP in IP = (SI * mult) + offset
		std::string hint; // the string used when multiple SI units match
		bool several; // several different options for the SI unit to be converted into IP
		bool is_default; // if part of a set of "several" this should be used as default

		// Default Constructor
		UnitConvType() :
			mult( 1.0 ),
			offset( 0.0 ),
			several( false ),
			is_default( false )
		{}

		// Member Constructor
		UnitConvType(
			std::string const & siName, // the name abbreviation or symbol of the SI units
			std::string const & ipName, // the name abbreviation or symbol of the IP units
			Real64 const mult, // the multiplier used to convert from SI to IP in IP = (SI * mult) + offset
			Real64 const offset, // the offset used to convert from SI to IP in IP = (SI * mult) + offset
			std::string const & hint, // the string used when multiple SI units match
			bool const several, // several different options for the SI unit to be converted into IP
			bool const is_default // if part of a set of "several" this should be used as default
		) :
			siName( siName ),
			ipName( ipName ),
			mult( mult ),
			offset( offset ),
			hint( hint ),
			several( several ),
			is_default( is_default )
		{}

	};

	// Object Data
	extern Array1D< OutputTableBinnedType > OutputTableBinned;
	extern Array2D< BinResultsType > BinResults; // table number, number of intervals
	extern Array1D< BinResultsType > BinResultsBelow; // time below the lowest defined bin
	extern Array1D< BinResultsType > BinResultsAbove; // time above the highest defined bin
	extern Array1D< BinObjVarIDType > BinObjVarID;
	extern Array1D< BinStatisticsType > BinStatistics;
	extern Array1D< NamedMonthlyType > namedMonthly; // for predefined monthly report titles
	extern Array1D< MonthlyFieldSetInputType > MonthlyFieldSetInput;
	extern Array1D< MonthlyInputType > MonthlyInput;
	extern Array1D< MonthlyTablesType > MonthlyTables;
	extern Array1D< MonthlyColumnsType > MonthlyColumns;
	extern Array1D< TOCEntriesType > TOCEntries;
	extern Array1D< UnitConvType > UnitConv;

	// Functions

	void
	UpdateTabularReports( int const IndexTypeKey ); // What kind of data to update (Zone, HVAC)

	//======================================================================================================================
	//======================================================================================================================

	//    GET INPUT ROUTINES

	//======================================================================================================================
	//======================================================================================================================

	void
	GetInputTabularMonthly();

	int
	AddMonthlyReport(
		std::string const & inReportName,
		int const inNumDigitsShown
	);

	void
	AddMonthlyFieldSetInput(
		int const inMonthReport,
		std::string const & inVariMeter,
		std::string const & inColHead,
		int const inAggregate
	);

	void
	InitializeTabularMonthly();

	void
	GetInputTabularTimeBins();

	bool
	warningAboutKeyNotFound( int foundIndex, int inObjIndex, std::string const & moduleName );

	void
	GetInputTabularStyle();

	int
	SetUnitsStyleFromString( std::string const & unitStringIn );


	void
	GetInputTabularPredefined();

	bool
	isCompLoadRepReq();

	void
	InitializePredefinedMonthlyTitles();

	void
	CreatePredefinedMonthlyReports();

	void
	GetInputFuelAndPollutionFactors();

	//======================================================================================================================
	//======================================================================================================================

	//    OTHER INITIALIZATION ROUTINES

	//======================================================================================================================
	//======================================================================================================================

	void
	OpenOutputTabularFile();

	void
	CloseOutputTabularFile();

	void
	WriteTableOfContents();

	//======================================================================================================================
	//======================================================================================================================

	//    GATHER DATA EACH TIME STEP ROUTINES

	//======================================================================================================================
	//======================================================================================================================

	void
	GatherBinResultsForTimestep( int const IndexTypeKey ); // What kind of data to update (Zone, HVAC)

	void
	GatherMonthlyResultsForTimestep( int const IndexTypeKey ); // What kind of data to update (Zone, HVAC)

	void
	GatherBEPSResultsForTimestep( int const IndexTypeKey ); // What kind of data to update (Zone, HVAC)

	void
	GatherSourceEnergyEndUseResultsForTimestep( int const IndexTypeKey ); // What kind of data to update (Zone, HVAC)

	void
	GatherPeakDemandForTimestep( int const IndexTypeKey ); // What kind of data to update (Zone, HVAC)

	void
	GatherHeatGainReport( int const IndexTypeKey ); // What kind of data to update (Zone, HVAC)

	//======================================================================================================================
	//======================================================================================================================

	//    WRITE OUTPUT FILE ROUTINES

	//======================================================================================================================
	//======================================================================================================================

	void
	WriteTabularReports();

	void
	FillWeatherPredefinedEntries();

	std::string
	GetColumnUsingTabs(
		std::string const & inString, // Input String
		int const colNum // Column number
	);

	void
	FillRemainingPredefinedEntries();

	void
	WriteMonthlyTables();

	void
	WriteTimeBinTables();

	void
	WriteBEPSTable();

	std::string
	ResourceWarningMessage(std::string resource);

	Real64
	WaterConversionFunct(Real64 WaterTotal, Real64 ConversionFactor);

	void
	WriteSourceEnergyEndUseSummary();

	void
	WriteDemandEndUseSummary();

	void
	WriteCompCostTable();

	void
	WriteVeriSumTable();

	void
	WriteAdaptiveComfortTable();

	void
	WritePredefinedTables();

	void
	WriteComponentSizing();

	void
	WriteSurfaceShadowing();

	void
	AddTOCZoneLoadComponentTable();

	void
	AllocateLoadComponentArrays();

	void
	DeallocateLoadComponentArrays();

	void
	ComputeLoadComponentDecayCurve();

	void
	GatherComponentLoadsSurface();

	void
	GatherComponentLoadsHVAC();

	void
	ComputeDelayedComponents();

	void
	WriteZoneLoadComponentTable();

	void
	WriteReportHeaders(
		std::string const & reportName,
		std::string const & objectName,
		int const averageOrSum
	);

	void
	WriteSubtitle( std::string const & subtitle );

	void
	WriteTextLine(
		std::string const & lineOfText,
		Optional_bool_const isBold = _
	);

	void
	WriteTable(
		Array2S_string const body, // row,column
		Array1S_string const rowLabels,
		Array1S_string const columnLabels,
		Array1S_int widthColumn,
		Optional_bool_const transposeXML = _,
		Optional_string_const footnoteText = _
	);

	std::string
	MakeAnchorName(
		std::string const & reportString,
		std::string const & objectString
	);

	std::string
	InsertCurrencySymbol(
		std::string const & inString, // Input String
		bool const isHTML // True if an HTML string
	);

	std::string
	ConvertToElementTag( std::string const & inString ); // Input String

	std::string
	ConvertToEscaped( std::string const & inString ); // Input String

	void
	DetermineBuildingFloorArea();

	//======================================================================================================================
	//======================================================================================================================

	//    ROUTINES RELATED TO IF VALUE IS IN A RANGE

	//======================================================================================================================
	//======================================================================================================================

	bool
	isInTriangle(
		Real64 const qx,
		Real64 const qy,
		Real64 const x1,
		Real64 const y1,
		Real64 const x2,
		Real64 const y2,
		Real64 const x3,
		Real64 const y3
	);

	bool
	isInQuadrilateral(
		Real64 const qx,
		Real64 const qy,
		Real64 const ax,
		Real64 const ay,
		Real64 const bx,
		Real64 const by,
		Real64 const cx,
		Real64 const cy,
		Real64 const dx,
		Real64 const dy
	);

	//======================================================================================================================
	//======================================================================================================================

	//    SUPPORT ROUTINES

	//======================================================================================================================
	//======================================================================================================================

	std::string
	RealToStr(
		Real64 const RealIn,
		int const numDigits
	);

	std::string
	IntToStr( int const intIn );

	Real64
	StrToReal( std::string const & stringIn );

	std::string
	DateToString( int const codedDate ); // word containing encoded month, day, hour, minute

	void
	AddTOCEntry(
		std::string const & nameSection,
		std::string const & nameReport
	);

	void
	SetupUnitConversions();

	std::string
	GetUnitSubString( std::string const & inString ); // Input String

	void
	LookupSItoIP(
		std::string const & stringInWithSI,
		int & unitConvIndex,
		std::string & stringOutWithIP
	);

	Real64
	ConvertIP(
		int const unitConvIndex,
		Real64 const SIvalue
	);

	Real64
	ConvertIPdelta(
		int const unitConvIndex,
		Real64 const SIvalue
	);

	void
	GetUnitConversion(
		int const unitConvIndex,
		Real64 & multiplier,
		Real64 & offset,
		std::string & IPunit
	);

	Real64
	getSpecificUnitMultiplier(
		std::string const & SIunit,
		std::string const & IPunit
	);

	Real64
	getSpecificUnitDivider(
		std::string const & SIunit,
		std::string const & IPunit
	);

	Real64
	getSpecificUnitIndex(
		std::string const & SIunit,
		std::string const & IPunit
	);

	//     NOTICE

	//     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of
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

} // OutputReportTabular

} // EnergyPlus

#endif
