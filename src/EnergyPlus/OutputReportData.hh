#ifndef OutputReportData_hh_INCLUDED
#define OutputReportData_hh_INCLUDED

// C++ Headers
#include <string>
#include <vector>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array1S.hh>
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Array2S.hh>
#include <ObjexxFCL/Array3D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

	class AnnualFieldSet
	{
	public:

		enum  AggregationKind {
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


		// default constructor
		AnnualFieldSet():
			m_variMeter( "" ),
			m_colHead(""),
			m_aggregate( sumOrAvg ),
			m_varUnits(""),
			m_typeOfVar(0),
			m_keyCount(0),
			m_varAvgSum(0),
			m_bottomBinValue( 0 ),
			m_topBinValue( 0 )
		{}

		// constructor
		AnnualFieldSet( std::string varName, AnnualFieldSet::AggregationKind kindOfAggregation, int numDigitsShown );

		struct AnnualCell
		{
			int indexesForKeyVar; // keyVarIndexes for each namesOfKeys
			Real64 result; // annual results
			Real64 duration; // the time during which results are summed for use in averages
			int timeStamp; // encoded timestamp of max or min
			std::vector<Real64> deferredResults; //used for the binned cases when size of bins is being calculated later
			std::vector<Real64> deferredElapsed; //the elapsed time for each result 
			Real64 m_timeAboveTopBin;
			Real64 m_timeBelowBottomBin;
			std::vector<Real64> m_timeInBin; // amount of time in each bin (usually 10 bins)
		};

		int
		getVariableKeyCountandTypeFromFldSt( int &typeVar, int &avgSumVar, int &stepTypeVar, std::string &unitsVar );

		void
		getVariableKeysFromFldSt( int &typeVar, int keyCount, std::vector<std::string> &namesOfKeys, std::vector<int>  &indexesForKeyVar );

		std::string m_variMeter; // the name of the variable or meter
		std::string m_colHead; // the column header to use instead of the variable name (only for predefined)
		AggregationKind m_aggregate; // the type of aggregation for the variable (see aggType parameters)
		int m_showDigits; // the number of digits to be shown
		std::string m_varUnits; // Units sting, may be blank
		int m_typeOfVar; // 0=not found, 1=integer, 2=real, 3=meter
		int m_keyCount; 
		int m_varAvgSum; // Variable  is Averaged=1 or Summed=2
		int m_varStepType; // Variable time step is Zone=1 or HVAC=2
		std::vector <std::string> m_namesOfKeys; // stored version of name of keys from getVariableKeys
		std::vector <int> m_indexesForKeyVar; // stored version of name of keys from getVariableKeys
		std::vector<AnnualCell> m_cell; // for each row contains the results and details for one cell of the table
		Real64 m_bottomBinValue; // the bottom of the binning for a column
		Real64 m_topBinValue; // the top of the binning for a column
		Real64 m_timeAboveTopBinTotal;
		Real64 m_timeBelowBottomBinTotal;
		std::vector<Real64> m_timeInBinTotal; // amount of time in each bin (usually 10 bins)
	};

} // EnergyPlus


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



#endif


