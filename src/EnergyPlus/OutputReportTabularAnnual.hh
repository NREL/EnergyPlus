#ifndef OutputReportTabularAnnual_hh_INCLUDED
#define OutputReportTabularAnnual_hh_INCLUDED

// C++ Headers
#include <string>
#include <vector>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {


	class NamedAnnual
	{
	public:

	private:
		// Members
		std::string title; // report title
		bool show; // if report should be shown

		// Default Constructor
		NamedAnnual() :
			show( false )
		{}

		// Member Constructor
		NamedAnnual(
			std::string const & title, // report title
			bool const show // if report should be shown
			) :
			title( title ),
			show( show )
		{}

	};

	class AnnualInput
	{
	public:

	private:
		// Members
		std::string name; // identifier
		std::string reportGroupName;
		std::string filter;
		std::string scheduleName;
		int scheduleNum;
		int numFieldSet; // number of monthly field sets
		int firstFieldSet; // pointer to the first field set
		int numTables; // number of tables
		int firstTable; // pointer to the first table

		// Default Constructor
		AnnualInput() :
			numFieldSet( 0 ),
			firstFieldSet( 0 ),
			numTables( 0 ),
			firstTable( 0 )
		{}

		// Member Constructor
		AnnualInput(
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
			firstTable( firstTable )
		{}

	};

	enum AggregationKind {
		sumOrAvg,
		maximum,
		minimum,
		hoursNonZero,
		hoursZero,
		hoursPositive,
		hoursNonPositive,
		hoursNegative,
		hoursNonNegative,
		hoursInTenPercentBins,
		hoursInTenBinsMinToMax,
		hoursInTenBinsZeroToMax,
		hoursInTenBinsMinToZero,
		hoursInTenBinsPlusMinusTwoStdDev,
		hoursInTenBinsPlusMinusThreeStdDev,
		noAggregation,
		valueWhenMaxMin,
		sumOrAverageHoursShown,
		maximumDuringHoursShown,
		minimumDuringHoursShown,
	};

	class AnnualFieldSetInput
	{
	public:

	private:
		// Members
		std::string variMeter; // the name of the variable or meter
		std::string colHead; // the column header to use instead of the variable name (only for predefined)
		AggregationKind aggregate; // the type of aggregation for the variable (see aggType parameters)
		std::string varUnits; // Units sting, may be blank
		std::string variMeterUpper; // the name of the variable or meter uppercased
		int typeOfVar; // 0=not found, 1=integer, 2=real, 3=meter
		int keyCount; // noel
		int varAvgSum; // Variable  is Averaged=1 or Summed=2
		int varStepType; // Variable time step is Zone=1 or HVAC=2
		std::vector< std::string > NamesOfKeys; // keyNames !noel
		std::vector< int > IndexesForKeyVar; // keyVarIndexes !noel

		// Default Constructor
		AnnualFieldSetInput() :
			aggregate( sumOrAvg ),
			typeOfVar( 0 ),
			keyCount( 0 ),
			varAvgSum( 1 ),
			varStepType( 1 )
		{}

		// Member Constructor
		AnnualFieldSetInput(
			std::string const & variMeter, // the name of the variable or meter
			std::string const & colHead, // the column header to use instead of the variable name (only for predefined)
			AggregationKind aggregate, // the type of aggregation for the variable (see aggType parameters)
			std::string const & varUnits, // Units sting, may be blank
			std::string const & variMeterUpper, // the name of the variable or meter uppercased
			int const typeOfVar, // 0=not found, 1=integer, 2=real, 3=meter
			int const keyCount, // noel
			int const varAvgSum, // Variable  is Averaged=1 or Summed=2
			int const varStepType, // Variable time step is Zone=1 or HVAC=2
			std::vector< std::string > const & NamesOfKeys, // keyNames !noel
			std::vector< int > const & IndexesForKeyVar // keyVarIndexes !noel
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

	class AnnualTables
	{
	public:

	private:
		// Members
		std::string keyValue; // the key value - the object names that result in the variable
		int firstColumn; // pointer to the monthly column array for the first item
		int numColumns; // number of columns for the table

		// Default Constructor
		AnnualTables() :
			firstColumn( 0 ),
			numColumns( 0 )
		{}

		// Member Constructor
		AnnualTables(
			std::string const & keyValue, // the key value - the object names that result in the variable
			int const firstColumn, // pointer to the monthly column array for the first item
			int const numColumns // number of columns for the table
			) :
			keyValue( keyValue ),
			firstColumn( firstColumn ),
			numColumns( numColumns )
		{}

	};

	class AnnualColumns
	{
	public:

	private:
		// Members
		std::string varName; // name of variable
		std::string colHead; // column header (not used for user defined monthly)
		int varNum; // variable or meter number
		int typeOfVar; // 0=not found, 1=integer, 2=real, 3=meter
		int avgSum; // Variable  is Averaged=1 or Summed=2
		int stepType; // Variable time step is Zone=1 or HVAC=2
		std::string units; // the units string, may be blank
		AggregationKind aggType; // index to the type of aggregation (see list of parameters)
		std::vector< Real64 > reslt; // monthly results
		std::vector< Real64 > duration; // the time during which results are summed for use in averages
		std::vector< int > timeStamp; // encoded timestamp of max or min
		Real64 aggForStep; // holds the aggregation for the HVAC time steps when smaller than
		int showDigits; // the number of digits to be shown
		// the zone timestep

		// Default Constructor
		AnnualColumns() :
			varNum( 0 ),
			typeOfVar( 0 ),
			avgSum( 0 ),
			stepType( 0 ),
			aggType( sumOrAvg ),
			reslt( 12, 0.0 ),
			duration( 12, 0.0 ),
			timeStamp( 12, 0 ),
			aggForStep( 0.0 ),
			showDigits( 0 )
		{}

		// Member Constructor
		AnnualColumns(
			std::string const & varName, // name of variable
			std::string const & colHead, // column header (not used for user defined monthly)
			int const varNum, // variable or meter number
			int const typeOfVar, // 0=not found, 1=integer, 2=real, 3=meter
			int const avgSum, // Variable  is Averaged=1 or Summed=2
			int const stepType, // Variable time step is Zone=1 or HVAC=2
			std::string const & units, // the units string, may be blank
			AggregationKind aggType, // index to the type of aggregation (see list of parameters)
			std::vector< Real64 > const & reslt, // monthly results
			std::vector< Real64 > const & duration, // the time during which results are summed for use in averages
			std::vector< int > const & timeStamp, // encoded timestamp of max or min
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
			reslt( reslt ),
			duration( duration ),
			timeStamp( timeStamp ),
			aggForStep( aggForStep ),
			showDigits( showDigits )
		{}

	};


		//     NOTICE

		//     Copyright (c) 1996-2015 The Board of Trustees of the University of Illinois
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

} // EnergyPlus


#endif