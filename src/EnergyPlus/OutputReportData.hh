// EnergyPlus, Copyright (c) 1996-2016, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
//
// If you have questions about your rights to use or distribute this software, please contact
// Berkeley Lab's Innovation & Partnerships Office at IPO@lbl.gov.
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
//     similar designation, without Lawrence Berkeley National Laboratory's prior written consent.
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
//
// You are under no obligation whatsoever to provide any bug fixes, patches, or upgrades to the
// features, functionality or performance of the source code ("Enhancements") to anyone; however,
// if you choose to make your Enhancements available either publicly, or directly to Lawrence
// Berkeley National Laboratory, without imposing a separate written license agreement for such
// Enhancements, then you hereby grant the following license: a non-exclusive, royalty-free
// perpetual license to install, use, modify, prepare derivative works, incorporate into other
// computer software, distribute, and sublicense such enhancements or derivative works thereof,
// in binary and source code form.

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



#endif


