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

// C++ Headers
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <EconomicLifeCycleCost.hh>
#include <DataCostEstimate.hh>
#include <DataGlobalConstants.hh>
#include <DataIPShortCuts.hh>
#include <DataPrecisionGlobals.hh>
#include <DisplayRoutines.hh>
#include <EconomicTariff.hh>
#include <InputProcessor.hh>
#include <OutputReportTabular.hh>
#include <SQLiteProcedures.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace EconomicLifeCycleCost {

	// Module containing the routines dealing with the EconomicLifeCycleCost

	// MODULE INFORMATION:
	//       AUTHOR         Jason Glazer of GARD Analytics, Inc.
	//       DATE WRITTEN   May 2010
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	//   To compute life-cycle cost measures such as present value based
	//   on input provided by the user as well as calculated energy costs.

	// METHODOLOGY EMPLOYED:
	//   Uses NIST Handbook 135 "Life-Cycle Costing Manual for the Federaml
	//   Energy Management Program" for most computations.

	// REFERENCES:
	//   To compute the net present value for all costs entered in the
	//   LifeCycleCosts objects, the algorithms from NIST Handbook 135
	//   "Life-Cycle Costing Manual for the Federal Energy Management
	//   Program" will be used as the primary source. Supplemental sources
	//   of algorithms will be derived from ASTM E833-09a "Standard
	//   Terminology of Building Economics", ASTM E917-05 "Standard
	//   Practice for Measuring Life-Cycle Cost of Buildings and Building
	//   Systems", and "Engineering Economic Analysis, Ninth Edition", by
	//   Donald Newnan, Ted Eschenback, and Jerome Lavelle.

	// OTHER NOTES:
	// na

	// Using/Aliasing
	using namespace DataGlobalConstants;
	using namespace DataPrecisionGlobals;
	using namespace InputProcessor;
	using namespace DataCostEstimate;
	using namespace DataIPShortCuts;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	int const disConvBeginOfYear( 1 );
	int const disConvMidYear( 2 );
	int const disConvEndOfYear( 3 );

	int const inflAppConstantDollar( 1 );
	int const inflAppCurrentDollar( 2 );

	// ModifiedAcceleratedCostRecoverySystem or Straight Line
	int const depMethMACRS3( 1 );
	int const depMethMACRS5( 2 );
	int const depMethMACRS7( 3 );
	int const depMethMACRS10( 4 );
	int const depMethMACRS15( 5 );
	int const depMethMACRS20( 6 );
	int const depMethStraight27( 7 );
	int const depMethStraight31( 8 );
	int const depMethStraight39( 9 );
	int const depMethStraight40( 10 );
	int const depMethNone( 11 );

	int const costCatMaintenance( 1 );
	int const costCatRepair( 2 );
	int const costCatOperation( 3 );
	int const costCatReplacement( 4 );
	int const costCatMinorOverhaul( 5 );
	int const costCatMajorOverhaul( 6 );
	int const costCatOtherOperational( 7 );
	int const costCatConstruction( 8 );
	int const costCatSalvage( 9 );
	int const costCatOtherCapital( 10 );
	int const costCatWater( 11 );
	int const costCatEnergy( 12 );
	int const costCatTotEnergy( 13 );
	int const costCatTotOper( 14 );
	int const costCatTotCaptl( 15 );
	int const costCatTotGrand( 16 );

	int const countOfCostCat( 16 ); // count of the number of cost categories

	// The NIST supplement includes UPV* factors for
	//   Electricity
	//   Natural gas
	//   Distillate oil
	//   Liquified petroleum gas
	//   Residual oil
	//   Coal

	int const startServicePeriod( 1 );
	int const startBasePeriod( 2 );

	// DERIVED TYPE DEFINITIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:

	// related to LifeCycleCost:Parameters
	bool LCCparamPresent( false ); // If a LifeCycleCost:Parameters object is present
	std::string LCCname; // Name
	int discountConvension( disConvEndOfYear ); // Discounting Convention
	int inflationApproach( inflAppConstantDollar ); // Inflation Approach
	Real64 realDiscountRate( 0.0 ); // Real Discount Rate
	Real64 nominalDiscountRate( 0.0 ); // Nominal Discount Rate
	Real64 inflation( 0.0 ); // Inflation
	int baseDateMonth( 0 ); // Base Date Month (1=Jan, 12=Dec)
	int baseDateYear( 0 ); // Base Date Year  1900-2100
	int serviceDateMonth( 0 ); // Service Date Month (1=Jan, 12=Dec)
	int serviceDateYear( 0 ); // Service Date Year 1900-2100
	int lengthStudyYears( 0 ); // Length of Study Period in Years
	int lengthStudyTotalMonths( 0 ); // Length of Study expressed in months (years x 12)
	Real64 taxRate( 0.0 ); // Tax rate
	int depreciationMethod( depMethNone ); // Depreciation Method
	// derived
	int lastDateMonth( 0 ); // Last Date Month (the month before the base date month)
	int lastDateYear( 0 ); // Last Date Year (base date year + length of study period in years)

	int numRecurringCosts( 0 );

	int numNonrecurringCost( 0 );

	int numUsePriceEscalation( 0 );

	int numUseAdjustment( 0 );

	int numCashFlow;
	int const skRecurring( 1 );
	int const skNonrecurring( 2 );
	int const skResource( 3 );
	int const skSum( 4 );
	int const pvkEnergy( 1 );
	int const pvkNonEnergy( 2 );
	int const pvkNotComputed( 3 );
	int numResourcesUsed;

	//present value factors
	Array1D< Real64 > SPV;
	Array2D< Real64 > energySPV; // yearly equivalent to FEMP UPV* values

	//arrays related to computing after tax cashflow and present value
	Array1D< Real64 > DepreciatedCapital;
	Array1D< Real64 > TaxableIncome;
	Array1D< Real64 > Taxes;
	Array1D< Real64 > AfterTaxCashFlow;
	Array1D< Real64 > AfterTaxPresentValue;

	Array1D_string const MonthNames( 12, { "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December" } );

	// SUBROUTINE SPECIFICATIONS FOR MODULE <module_name>:

	// Object Data
	Array1D< RecurringCostsType > RecurringCosts;
	Array1D< NonrecurringCostType > NonrecurringCost;
	Array1D< UsePriceEscalationType > UsePriceEscalation;
	Array1D< UseAdjustmentType > UseAdjustment;
	Array1D< CashFlowType > CashFlow;

	// Functions

	void
	GetInputForLifeCycleCost()
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   May 2010
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//    Read the input file for "LifeCycleCost:Parameters" object.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// Using/Aliasing
		using OutputReportTabular::AddTOCEntry;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool GetLifeCycleCostInput( true );

		if ( GetLifeCycleCostInput ) {
			GetInputLifeCycleCostParameters();
			GetInputLifeCycleCostRecurringCosts();
			GetInputLifeCycleCostNonrecurringCost();
			GetInputLifeCycleCostUsePriceEscalation();
			GetInputLifeCycleCostUseAdjustment();
			if ( LCCparamPresent ) {
				AddTOCEntry( "Life-Cycle Cost Report", "Entire Facility" );
			}
			GetLifeCycleCostInput = false;
		}
	}

	void
	ComputeLifeCycleCostAndReport()
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   May 2010
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//    Perform the life cycle cost computations and write report.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		if ( LCCparamPresent ) {
			DisplayString( "Computing Life Cycle Costs and Reporting" );
			ExpressAsCashFlows();
			ComputePresentValue();
			ComputeTaxAndDepreciation();
			WriteTabularLifeCycleCostReport();
		}
	}

	//======================================================================================================================
	//======================================================================================================================

	//    GET INPUT ROUTINES

	//======================================================================================================================
	//======================================================================================================================

	void
	GetInputLifeCycleCostParameters()
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   May 2010
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//    Read the input file for "LifeCycleCost:Parameters" object.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int jFld;
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		Array1D_string AlphaArray( 100 ); // character string data
		Array1D< Real64 > NumArray( 100 ); // numeric data
		int IOStat; // IO Status when calling get input subroutine
		std::string CurrentModuleObject; // for ease in renaming.
		int NumObj; // count of objects

		CurrentModuleObject = "LifeCycleCost:Parameters";
		NumObj = GetNumObjectsFound( CurrentModuleObject );

		if ( NumObj == 0 ) {
			LCCparamPresent = false;
		} else if ( NumObj == 1 ) {
			LCCparamPresent = true;
			GetObjectItem( CurrentModuleObject, 1, AlphaArray, NumAlphas, NumArray, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			//check to make sure none of the values are another life cycle cost object
			for ( jFld = 1; jFld <= NumAlphas; ++jFld ) {
				if ( hasi( AlphaArray( jFld ), "LifeCycleCost:" ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + AlphaArray( 1 ) + " a field was found containing LifeCycleCost: which may indicate a missing comma." );
				}
			}
			// start to extract values from input array into appropriate fields
			//  A1,  \field Name
			//       \required-field
			//       \type alpha
			LCCname = AlphaArray( 1 );
			//  A2, \field Discounting Convention
			//      \type choice
			//      \key EndOfYear
			//      \key MidYear
			//      \key BeginningOfYear
			//      \default EndOfYear
			if ( SameString( AlphaArray( 2 ), "EndOfYear" ) ) {
				discountConvension = disConvEndOfYear;
			} else if ( SameString( AlphaArray( 2 ), "MidYear" ) ) {
				discountConvension = disConvMidYear;
			} else if ( SameString( AlphaArray( 2 ), "BeginningOfYear" ) ) {
				discountConvension = disConvBeginOfYear;
			} else {
				discountConvension = disConvEndOfYear;
				ShowWarningError( CurrentModuleObject + ": Invalid " + cAlphaFieldNames( 2 ) + "=\"" + AlphaArray( 2 ) + "\". EndOfYear will be used." );
			}
			// A3,  \field Inflation Approach
			//      \type choice
			//      \key ConstantDollar
			//      \key CurrentDollar
			//      \default ConstantDollar
			if ( SameString( AlphaArray( 3 ), "ConstantDollar" ) ) {
				inflationApproach = inflAppConstantDollar;
			} else if ( SameString( AlphaArray( 3 ), "CurrentDollar" ) ) {
				inflationApproach = inflAppCurrentDollar;
			} else {
				inflationApproach = inflAppConstantDollar;
				ShowWarningError( CurrentModuleObject + ": Invalid " + cAlphaFieldNames( 3 ) + "=\"" + AlphaArray( 3 ) + "\". ConstantDollar will be used." );
			}
			// N1,  \field Real Discount Rate
			//      \type real
			realDiscountRate = NumArray( 1 );
			if ( ( inflationApproach == inflAppConstantDollar ) && lNumericFieldBlanks( 1 ) ) {
				ShowWarningError( CurrentModuleObject + ": Invalid for field " + cNumericFieldNames( 1 ) + " to be blank when ConstantDollar analysis is be used." );
			}
			if ( ( realDiscountRate > 0.30 ) || ( realDiscountRate < -0.30 ) ) {
				ShowWarningError( CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames( 1 ) + ".  This value is the decimal value not a percentage so most values are between 0.02 and 0.15. " );
			}
			// N2,  \field Nominal Discount Rate
			//      \type real
			nominalDiscountRate = NumArray( 2 );
			if ( ( inflationApproach == inflAppCurrentDollar ) && lNumericFieldBlanks( 2 ) ) {
				ShowWarningError( CurrentModuleObject + ": Invalid for field " + cNumericFieldNames( 2 ) + " to be blank when CurrentDollar analysis is be used." );
			}
			if ( ( nominalDiscountRate > 0.30 ) || ( nominalDiscountRate < -0.30 ) ) {
				ShowWarningError( CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames( 2 ) + ".  This value is the decimal value not a percentage so most values are between 0.02 and 0.15. " );
			}
			// N3,  \field Inflation
			//      \type real
			inflation = NumArray( 3 );
			if ( ( inflationApproach == inflAppConstantDollar ) && ( ! lNumericFieldBlanks( 3 ) ) ) {
				ShowWarningError( CurrentModuleObject + ": Invalid for field " + cNumericFieldNames( 3 ) + " contain a value when ConstantDollar analysis is be used." );
			}
			if ( ( inflation > 0.30 ) || ( inflation < -0.30 ) ) {
				ShowWarningError( CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames( 3 ) + ".  This value is the decimal value not a percentage so most values are between 0.02 and 0.15. " );
			}
			// A4,  \field Base Date Month
			//      \type choice
			//      \key January
			//      \key February
			//      \key March
			//      \key April
			//      \key May
			//      \key June
			//      \key July
			//      \key August
			//      \key September
			//      \key October
			//      \key November
			//      \key December
			//      \default January
			baseDateMonth = MonthToMonthNumber( AlphaArray( 4 ), 1 );
			// N4,  \field Base Date Year
			//      \type integer
			//      \minimum 1900
			//      \maximum 2100
			baseDateYear = int( NumArray( 4 ) );
			if ( baseDateYear > 2100 ) {
				ShowWarningError( CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames( 4 ) + ".  Value greater than 2100 yet it is representing a year. " );
			}
			if ( baseDateYear < 1900 ) {
				ShowWarningError( CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames( 4 ) + ".  Value less than 1900 yet it is representing a year. " );
			}
			// A5,  \field Service Date Month
			//      \type choice
			//      \key January
			//      \key February
			//      \key March
			//      \key April
			//      \key May
			//      \key June
			//      \key July
			//      \key August
			//      \key September
			//      \key October
			//      \key November
			//      \key December
			//      \default January
			serviceDateMonth = MonthToMonthNumber( AlphaArray( 5 ), 1 );
			// N5,  \field Service Date Year
			//      \type integer
			//      \minimum 1900
			//      \maximum 2100
			serviceDateYear = int( NumArray( 5 ) );
			if ( serviceDateYear > 2100 ) {
				ShowWarningError( CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames( 5 ) + ".  Value greater than 2100 yet it is representing a year. " );
			}
			if ( serviceDateYear < 1900 ) {
				ShowWarningError( CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames( 5 ) + ".  Value less than 1900 yet it is representing a year. " );
			}
			// N6,  \field Length of Study Period in Years
			//      \type integer
			//      \minimum 1
			//      \maximum 100
			lengthStudyYears = int( NumArray( 6 ) );
			if ( lengthStudyYears > 100 ) {
				ShowWarningError( CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames( 6 ) + ".  A value greater than 100 is not reasonable for an economic evaluation. " );
			}
			if ( lengthStudyYears < 1 ) {
				ShowWarningError( CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames( 6 ) + ".  A value less than 1 is not reasonable for an economic evaluation. " );
			}
			lengthStudyTotalMonths = lengthStudyYears * 12;
			// N7, \field Tax rate
			//      \type real
			//      \minimum 0.0
			taxRate = NumArray( 7 );
			if ( taxRate < 0.0 && ( ! lNumericFieldBlanks( 7 ) ) ) {
				ShowWarningError( CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames( 10 ) + ".  A value less than 0 is not reasonable for a tax rate. " );
			}
			// A6;  \field Depreciation Method
			//      \type choice
			//      \key ModifiedAcceleratedCostRecoverySystem-3year
			//      \key ModifiedAcceleratedCostRecoverySystem-5year
			//      \key ModifiedAcceleratedCostRecoverySystem-7year
			//      \key ModifiedAcceleratedCostRecoverySystem-10year
			//      \key ModifiedAcceleratedCostRecoverySystem-15year
			//      \key ModifiedAcceleratedCostRecoverySystem-20year
			//      \key StraightLine-27year
			//      \key StraightLine-31year
			//      \key StraightLine-39year
			//      \key StraightLine-40year
			//      \key None
			//      \default None
			if ( SameString( AlphaArray( 6 ), "ModifiedAcceleratedCostRecoverySystem-3year" ) ) {
				depreciationMethod = depMethMACRS3;
			} else if ( SameString( AlphaArray( 6 ), "ModifiedAcceleratedCostRecoverySystem-5year" ) ) {
				depreciationMethod = depMethMACRS5;
			} else if ( SameString( AlphaArray( 6 ), "ModifiedAcceleratedCostRecoverySystem-7year" ) ) {
				depreciationMethod = depMethMACRS7;
			} else if ( SameString( AlphaArray( 6 ), "ModifiedAcceleratedCostRecoverySystem-10year" ) ) {
				depreciationMethod = depMethMACRS10;
			} else if ( SameString( AlphaArray( 6 ), "ModifiedAcceleratedCostRecoverySystem-15year" ) ) {
				depreciationMethod = depMethMACRS15;
			} else if ( SameString( AlphaArray( 6 ), "ModifiedAcceleratedCostRecoverySystem-20year" ) ) {
				depreciationMethod = depMethMACRS20;
			} else if ( SameString( AlphaArray( 6 ), "StraightLine-27year" ) ) {
				depreciationMethod = depMethStraight27;
			} else if ( SameString( AlphaArray( 6 ), "StraightLine-31year" ) ) {
				depreciationMethod = depMethStraight31;
			} else if ( SameString( AlphaArray( 6 ), "StraightLine-39year" ) ) {
				depreciationMethod = depMethStraight39;
			} else if ( SameString( AlphaArray( 6 ), "StraightLine-40year" ) ) {
				depreciationMethod = depMethStraight40;
			} else if ( SameString( AlphaArray( 6 ), "None" ) ) {
				depreciationMethod = depMethNone;
			} else if ( lAlphaFieldBlanks( 6 ) ) {
				depreciationMethod = depMethNone;
				ShowWarningError( CurrentModuleObject + ": The input field " + cAlphaFieldNames( 6 ) + "is blank. \"None\" will be used." );
			} else {
				depreciationMethod = depMethNone;
				ShowWarningError( CurrentModuleObject + ": Invalid " + cAlphaFieldNames( 6 ) + "=\"" + AlphaArray( 6 ) + "\". \"None\" will be used." );
			}
			// compute derived variables
			lastDateMonth = baseDateMonth - 1; //same month of the year for first and last month
			if ( lastDateMonth == 0 ) lastDateMonth = 12;
			lastDateYear = baseDateYear + lengthStudyYears - 1;
		} else {
			ShowWarningError( CurrentModuleObject + ": Only one instance of this object is allowed. No life-cycle cost reports will be generated." );
			LCCparamPresent = false;
		}
	}

	void
	GetInputLifeCycleCostRecurringCosts()
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   May 2010
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//    Read the input file for "LifeCycleCost:RecurringCosts" object.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int iInObj; // loop index variable for reading in objects
		int jFld;
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		Array1D_string AlphaArray( 100 ); // character string data
		Array1D< Real64 > NumArray( 100 ); // numeric data
		int IOStat; // IO Status when calling get input subroutine
		std::string CurrentModuleObject; // for ease in renaming.

		if ( ! LCCparamPresent ) return;
		CurrentModuleObject = "LifeCycleCost:RecurringCosts";
		numRecurringCosts = GetNumObjectsFound( CurrentModuleObject );
		RecurringCosts.allocate( numRecurringCosts );
		for ( iInObj = 1; iInObj <= numRecurringCosts; ++iInObj ) {
			GetObjectItem( CurrentModuleObject, iInObj, AlphaArray, NumAlphas, NumArray, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			//check to make sure none of the values are another life cycle cost object
			for ( jFld = 1; jFld <= NumAlphas; ++jFld ) {
				if ( hasi( AlphaArray( jFld ), "LifeCycleCost:" ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + AlphaArray( 1 ) + " a field was found containing LifeCycleCost: which may indicate a missing comma." );
				}
			}
			// start to extract values from input array into appropriate fields
			//   A1,  \field Name
			//        \required-field
			//        \type alpha
			RecurringCosts( iInObj ).name = AlphaArray( 1 );
			//   A2,  \field Category
			//        \type choice
			//        \key Maintenance
			//        \key Repair
			//        \key Operation
			//        \key Replacement
			//        \key MinorOverhaul
			//        \key MajorOverhaul
			//        \key OtherOperational
			//        \default Maintenance
			if ( SameString( AlphaArray( 2 ), "Maintenance" ) ) {
				RecurringCosts( iInObj ).category = costCatMaintenance;
			} else if ( SameString( AlphaArray( 2 ), "Repair" ) ) {
				RecurringCosts( iInObj ).category = costCatRepair;
			} else if ( SameString( AlphaArray( 2 ), "Operation" ) ) {
				RecurringCosts( iInObj ).category = costCatOperation;
			} else if ( SameString( AlphaArray( 2 ), "Replacement" ) ) {
				RecurringCosts( iInObj ).category = costCatReplacement;
			} else if ( SameString( AlphaArray( 2 ), "MinorOverhaul" ) ) {
				RecurringCosts( iInObj ).category = costCatMinorOverhaul;
			} else if ( SameString( AlphaArray( 2 ), "MajorOverhaul" ) ) {
				RecurringCosts( iInObj ).category = costCatMajorOverhaul;
			} else if ( SameString( AlphaArray( 2 ), "OtherOperational" ) ) {
				RecurringCosts( iInObj ).category = costCatOtherOperational;
			} else {
				RecurringCosts( iInObj ).category = costCatMaintenance;
				ShowWarningError( CurrentModuleObject + ": Invalid " + cAlphaFieldNames( 2 ) + "=\"" + AlphaArray( 2 ) + "\". The category of Maintenance will be used." );
			}
			//   N1,  \field Cost
			//        \type real
			RecurringCosts( iInObj ).cost = NumArray( 1 );
			//   A3,  \field Start of Costs
			//        \type choice
			//        \key ServicePeriod
			//        \key BasePeriod
			//        \default ServicePeriod
			if ( SameString( AlphaArray( 3 ), "ServicePeriod" ) ) {
				RecurringCosts( iInObj ).startOfCosts = startServicePeriod;
			} else if ( SameString( AlphaArray( 3 ), "BasePeriod" ) ) {
				RecurringCosts( iInObj ).startOfCosts = startBasePeriod;
			} else {
				RecurringCosts( iInObj ).startOfCosts = startServicePeriod;
				ShowWarningError( CurrentModuleObject + ": Invalid " + cAlphaFieldNames( 3 ) + "=\"" + AlphaArray( 3 ) + "\". The start of the service period will be used." );
			}
			//   N2,  \field Years from Start
			//        \type integer
			//        \minimum 0
			//        \maximum 100
			RecurringCosts( iInObj ).yearsFromStart = int( NumArray( 2 ) );
			if ( RecurringCosts( iInObj ).yearsFromStart > 100 ) {
				ShowWarningError( CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames( 2 ) + ".  This value is the number of years from the start so a value greater than 100 is not reasonable for an economic evaluation. " );
			}
			if ( RecurringCosts( iInObj ).yearsFromStart < 0 ) {
				ShowWarningError( CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames( 2 ) + ".  This value is the number of years from the start so a value less than 0 is not reasonable for an economic evaluation. " );
			}
			//   N3,  \field Months from Start
			//        \type integer
			//        \minimum 0
			//        \maximum 1200
			RecurringCosts( iInObj ).monthsFromStart = int( NumArray( 3 ) );
			if ( RecurringCosts( iInObj ).monthsFromStart > 1200 ) {
				ShowWarningError( CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames( 3 ) + ".  This value is the number of months from the start so a value greater than 1200 is not reasonable for an economic evaluation. " );
			}
			if ( RecurringCosts( iInObj ).monthsFromStart < 0 ) {
				ShowWarningError( CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames( 3 ) + ".  This value is the number of months from the start so a value less than 0 is not reasonable for an economic evaluation. " );
			}
			//   N4,  \field Repeat Period Years
			//        \type integer
			//        \minimum 1
			//        \maximum 100
			RecurringCosts( iInObj ).repeatPeriodYears = int( NumArray( 4 ) );
			if ( RecurringCosts( iInObj ).repeatPeriodYears > 100 ) {
				ShowWarningError( CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames( 4 ) + ".  This value is the number of years between occurances of the cost so a value greater than 100 is not reasonable for an economic evaluation. " );
			}
			if ( RecurringCosts( iInObj ).repeatPeriodYears < 1 ) {
				ShowWarningError( CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames( 4 ) + ".  This value is the number of years between occurances of the cost so a value less than 1 is not reasonable for an economic evaluation. " );
			}
			//   N5,  \field Repeat Period Months
			//        \type integer
			//        \minimum 0
			//        \maximum 1200
			RecurringCosts( iInObj ).repeatPeriodMonths = int( NumArray( 5 ) );
			if ( RecurringCosts( iInObj ).repeatPeriodMonths > 1200 ) {
				ShowWarningError( CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames( 5 ) + ".  This value is the number of months between occurances of the cost so a value greater than 1200 is not reasonable for an economic evaluation. " );
			}
			if ( RecurringCosts( iInObj ).repeatPeriodMonths < 0 ) {
				ShowWarningError( CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames( 5 ) + ".  This value is the number of months between occurances of the cost so a value less than 0 is not reasonable for an economic evaluation. " );
			}
			if ( ( RecurringCosts( iInObj ).repeatPeriodMonths == 0 ) && ( RecurringCosts( iInObj ).repeatPeriodYears == 0 ) ) {
				ShowWarningError( CurrentModuleObject + ": Invalid value in fields " + cNumericFieldNames( 5 ) + " and " + cNumericFieldNames( 4 ) + ".  The repeat period must not be zero months and zero years. " );
			}
			//   N6;  \field Annual escalation rate
			//        \type real
			RecurringCosts( iInObj ).annualEscalationRate = int( NumArray( 6 ) );
			if ( RecurringCosts( iInObj ).annualEscalationRate > 0.30 ) {
				ShowWarningError( CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames( 6 ) + ".  This value is the decimal value for the annual escalation so most values are between 0.02 and 0.15. " );
			}
			if ( RecurringCosts( iInObj ).annualEscalationRate < -0.30 ) {
				ShowWarningError( CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames( 6 ) + ".  This value is the decimal value for the annual escalation so most values are between 0.02 and 0.15. " );
			}
			// express the years and months fields in total months
			RecurringCosts( iInObj ).totalMonthsFromStart = RecurringCosts( iInObj ).yearsFromStart * 12 + RecurringCosts( iInObj ).monthsFromStart;
			RecurringCosts( iInObj ).totalRepeatPeriodMonths = RecurringCosts( iInObj ).repeatPeriodYears * 12 + RecurringCosts( iInObj ).repeatPeriodMonths;
		}
	}

	void
	GetInputLifeCycleCostNonrecurringCost()
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   May 2010
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//    Read the input file for "LifeCycleCost:NonrecurringCost" object.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int iInObj; // loop index variable for reading in objects
		int jFld;
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		Array1D_string AlphaArray( 100 ); // character string data
		Array1D< Real64 > NumArray( 100 ); // numeric data
		int IOStat; // IO Status when calling get input subroutine
		std::string CurrentModuleObject; // for ease in renaming.
		int numComponentCostLineItems; // number of ComponentCost:LineItem objects

		if ( ! LCCparamPresent ) return;
		CurrentModuleObject = "LifeCycleCost:NonrecurringCost";
		numNonrecurringCost = GetNumObjectsFound( CurrentModuleObject );
		numComponentCostLineItems = GetNumObjectsFound( "ComponentCost:LineItem" );
		if ( numComponentCostLineItems > 0 ) { //leave room for component cost total
			NonrecurringCost.allocate( numNonrecurringCost + 1 ); //add a place for CostEstimate total
		} else {
			NonrecurringCost.allocate( numNonrecurringCost );
		}
		for ( iInObj = 1; iInObj <= numNonrecurringCost; ++iInObj ) {
			GetObjectItem( CurrentModuleObject, iInObj, AlphaArray, NumAlphas, NumArray, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			//check to make sure none of the values are another life cycle cost object
			for ( jFld = 1; jFld <= NumAlphas; ++jFld ) {
				if ( hasi( AlphaArray( jFld ), "LifeCycleCost:" ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + AlphaArray( 1 ) + " a field was found containing LifeCycleCost: which may indicate a missing comma." );
				}
			}
			// start to extract values from input array into appropriate fields
			// A1,  \field Name
			//      \required-field
			//      \type alpha
			NonrecurringCost( iInObj ).name = AlphaArray( 1 );
			// A2,  \field Category
			//      \type choice
			//      \key Construction
			//      \key Salvage
			//      \key OtherCapital
			//      \default Construction
			if ( SameString( AlphaArray( 2 ), "Construction" ) ) {
				NonrecurringCost( iInObj ).category = costCatConstruction;
			} else if ( SameString( AlphaArray( 2 ), "Salvage" ) ) {
				NonrecurringCost( iInObj ).category = costCatSalvage;
			} else if ( SameString( AlphaArray( 2 ), "OtherCapital" ) ) {
				NonrecurringCost( iInObj ).category = costCatOtherCapital;
			} else {
				NonrecurringCost( iInObj ).category = costCatConstruction;
				ShowWarningError( CurrentModuleObject + ": Invalid " + cAlphaFieldNames( 2 ) + "=\"" + AlphaArray( 2 ) + "\". The category of Construction will be used." );
			}
			// N1,  \field Cost
			//      \type real
			NonrecurringCost( iInObj ).cost = NumArray( 1 );
			// A3,  \field Start of Costs
			//      \type choice
			//      \key ServicePeriod
			//      \key BasePeriod
			//      \default ServicePeriod
			if ( SameString( AlphaArray( 3 ), "ServicePeriod" ) ) {
				NonrecurringCost( iInObj ).startOfCosts = startServicePeriod;
			} else if ( SameString( AlphaArray( 3 ), "BasePeriod" ) ) {
				NonrecurringCost( iInObj ).startOfCosts = startBasePeriod;
			} else {
				NonrecurringCost( iInObj ).startOfCosts = startServicePeriod;
				ShowWarningError( CurrentModuleObject + ": Invalid " + cAlphaFieldNames( 3 ) + "=\"" + AlphaArray( 3 ) + "\". The start of the service period will be used." );
			}
			// N2,  \field Years from Start
			//      \type integer
			//      \minimum 0
			//      \maximum 100
			NonrecurringCost( iInObj ).yearsFromStart = int( NumArray( 2 ) );
			if ( NonrecurringCost( iInObj ).yearsFromStart > 100 ) {
				ShowWarningError( CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames( 2 ) + ".  This value is the number of years from the start so a value greater than 100 is not reasonable for an economic evaluation. " );
			}
			if ( NonrecurringCost( iInObj ).yearsFromStart < 0 ) {
				ShowWarningError( CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames( 2 ) + ".  This value is the number of years from the start so a value less than 0 is not reasonable for an economic evaluation. " );
			}
			//  N3;  \field Months from Start
			//       \type integer
			//       \minimum 0
			//       \maximum 11
			NonrecurringCost( iInObj ).monthsFromStart = int( NumArray( 3 ) );
			if ( NonrecurringCost( iInObj ).monthsFromStart > 1200 ) {
				ShowWarningError( CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames( 3 ) + ".  This value is the number of months from the start so a value greater than 1200 is not reasonable for an economic evaluation. " );
			}
			if ( NonrecurringCost( iInObj ).monthsFromStart < 0 ) {
				ShowWarningError( CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames( 3 ) + ".  This value is the number of months from the start so a value less than 0 is not reasonable for an economic evaluation. " );
			}
			// express the years and months fields in total months
			NonrecurringCost( iInObj ).totalMonthsFromStart = NonrecurringCost( iInObj ).yearsFromStart * 12 + NonrecurringCost( iInObj ).monthsFromStart;
		}
	}

	void
	GetInputLifeCycleCostUsePriceEscalation()
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   May 2010
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//    Read the input file for "LifeCycleCost:UsePriceEscalation" object.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int iInObj; // loop index variable for reading in objects
		int jYear;
		int jFld;
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		Array1D_string AlphaArray( 100 ); // character string data
		Array1D< Real64 > NumArray( 100 ); // numeric data
		int IOStat; // IO Status when calling get input subroutine
		std::string CurrentModuleObject; // for ease in renaming.
		static int escStartYear( 0 );
		static int escNumYears( 0 );
		static int escEndYear( 0 );
		static int earlierEndYear( 0 );
		static int laterStartYear( 0 );
		static int curEsc( 0 );
		static int curFld( 0 );

		if ( ! LCCparamPresent ) return;
		CurrentModuleObject = "LifeCycleCost:UsePriceEscalation";
		numUsePriceEscalation = GetNumObjectsFound( CurrentModuleObject );
		UsePriceEscalation.allocate( numUsePriceEscalation );
		for ( iInObj = 1; iInObj <= numUsePriceEscalation; ++iInObj ) {
			UsePriceEscalation( iInObj ).Escalation.allocate( lengthStudyYears );
		}
		if ( numUsePriceEscalation > 0 ) {
			for ( iInObj = 1; iInObj <= numUsePriceEscalation; ++iInObj ) {
				GetObjectItem( CurrentModuleObject, iInObj, AlphaArray, NumAlphas, NumArray, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				//check to make sure none of the values are another life cycle cost object
				for ( jFld = 1; jFld <= NumAlphas; ++jFld ) {
					if ( hasi( AlphaArray( jFld ), "LifeCycleCost:" ) ) {
						ShowWarningError( "In " + CurrentModuleObject + " named " + AlphaArray( 1 ) + " a field was found containing LifeCycleCost: which may indicate a missing comma." );
					}
				}
				// start to extract values from input array into appropriate fields
				// A1,  \field Name
				//      \required-field
				//      \type alpha
				UsePriceEscalation( iInObj ).name = AlphaArray( 1 );
				//  A2,  \field Resource
				//       \required-field
				//       \type choice
				//       \key Electricity
				//       \key NaturalGas
				//       \key Steam
				//       \key Gasoline
				//       \key Diesel
				//       \key Coal
				//       \key FuelOil#1
				//       \key FuelOil#2
				//       \key Propane
				//       \key Water
				//       \key OtherFuel1
				//       \key OtherFuel2
				UsePriceEscalation( iInObj ).resource = AssignResourceTypeNum( AlphaArray( 2 ) ); //use function from DataGlobalConstants
				if ( NumAlphas > 3 ) {
					ShowWarningError( "In " + CurrentModuleObject + " contains more alpha fields than expected." );
				}
				// N1,  \field Escalation Start Year
				//      \type integer
				//      \minimum 1900
				//      \maximum 2100
				UsePriceEscalation( iInObj ).escalationStartYear = int( NumArray( 1 ) );
				if ( UsePriceEscalation( iInObj ).escalationStartYear > 2100 ) {
					ShowWarningError( CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames( 1 ) + ".  Value greater than 2100 yet it is representing a year. " );
				}
				if ( UsePriceEscalation( iInObj ).escalationStartYear < 1900 ) {
					ShowWarningError( CurrentModuleObject + ": Invalid value in field " + cNumericFieldNames( 1 ) + ".  Value less than 1900 yet it is representing a year. " );
				}
				// A3,  \field Escalation Start Month
				//      \type choice
				//      \key January
				//      \key February
				//      \key March
				//      \key April
				//      \key May
				//      \key June
				//      \key July
				//      \key August
				//      \key September
				//      \key October
				//      \key November
				//      \key December
				//      \default January
				UsePriceEscalation( iInObj ).escalationStartMonth = MonthToMonthNumber( AlphaArray( 3 ), 1 );
				// N2,  \field Year 1 Escalation
				//      \type real
				//      \begin-extensible
				// The array is from the baseDateYear until baseDateYear + lengthStudyYears
				// Set the array to default to 1.0
				for ( jYear = 1; jYear <= lengthStudyYears; ++jYear ) {
					UsePriceEscalation( iInObj ).Escalation( jYear ) = 1.0;
				}
				// Since the years in the UsePriceEscalation may not match up with the baseDateYear and
				// the lenghtStudyYears, need to make adjustments when reading in the values to align
				// with the baseDateYear (the first item in all yearly arrays)
				escStartYear = UsePriceEscalation( iInObj ).escalationStartYear;
				escNumYears = NumNums - 1;
				escEndYear = escStartYear + escNumYears - 1;
				earlierEndYear = min( escEndYear, lastDateYear ); // pick the earlier ending date
				laterStartYear = max( escStartYear, baseDateYear ); //pick the later starting date
				for ( jYear = laterStartYear; jYear <= earlierEndYear; ++jYear ) {
					curFld = 2 + jYear - escStartYear;
					curEsc = 1 + jYear - baseDateYear;
					if ( ( curFld <= NumNums ) && ( curFld >= 1 ) ) {
						if ( ( curEsc <= lengthStudyYears ) && ( curEsc >= 1 ) ) {
							UsePriceEscalation( iInObj ).Escalation( curEsc ) = NumArray( curFld );
						}
					}
				}
			}
		}
	}

	void
	GetInputLifeCycleCostUseAdjustment()
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   May 2010
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//    Read the input file for "LifeCycleCost:UseAdjustment" object.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int iInObj; // loop index variable for reading in objects
		int jFld;
		int jYear;
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		Array1D_string AlphaArray( 100 ); // character string data
		Array1D< Real64 > NumArray( 100 ); // numeric data
		int IOStat; // IO Status when calling get input subroutine
		std::string CurrentModuleObject; // for ease in renaming.
		int numFldsToUse;

		if ( ! LCCparamPresent ) return;
		CurrentModuleObject = "LifeCycleCost:UseAdjustment";
		numUseAdjustment = GetNumObjectsFound( CurrentModuleObject );
		UseAdjustment.allocate( numUseAdjustment );
		for ( iInObj = 1; iInObj <= numUseAdjustment; ++iInObj ) {
			UseAdjustment( iInObj ).Adjustment.allocate( lengthStudyYears );
		}
		if ( numUseAdjustment > 0 ) {
			for ( iInObj = 1; iInObj <= numUseAdjustment; ++iInObj ) {
				GetObjectItem( CurrentModuleObject, iInObj, AlphaArray, NumAlphas, NumArray, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				//check to make sure none of the values are another life cycle cost object
				for ( jFld = 1; jFld <= NumAlphas; ++jFld ) {
					if ( hasi( AlphaArray( jFld ), "LifeCycleCost:" ) ) {
						ShowWarningError( "In " + CurrentModuleObject + " named " + AlphaArray( 1 ) + " a field was found containing LifeCycleCost: which may indicate a missing comma." );
					}
				}
				// start to extract values from input array into appropriate fields
				//  A1,  \field Name
				//       \required-field
				//       \type alpha
				UseAdjustment( iInObj ).name = AlphaArray( 1 );
				//  A2,  \field Resource
				//       \required-field
				//       \type choice
				//       \key Electricity
				//       \key NaturalGas
				//       \key Steam
				//       \key Gasoline
				//       \key Diesel
				//       \key Coal
				//       \key FuelOil#1
				//       \key FuelOil#2
				//       \key Propane
				//       \key Water
				//       \key OtherFuel1
				//       \key OtherFuel2
				UseAdjustment( iInObj ).resource = AssignResourceTypeNum( AlphaArray( 2 ) ); //use function from DataGlobalConstants
				if ( NumAlphas > 2 ) {
					ShowWarningError( "In " + CurrentModuleObject + " contains more alpha fields than expected." );
				}
				//  N1,  \field Year 1 Multiplier
				//       \type real
				//       \begin-extensible
				// Set the array to default to 1.0
				for ( jYear = 1; jYear <= lengthStudyYears; ++jYear ) {
					UseAdjustment( iInObj ).Adjustment( jYear ) = 1.0;
				}
				numFldsToUse = min( NumNums, lengthStudyYears );
				for ( jYear = 1; jYear <= numFldsToUse; ++jYear ) {
					UseAdjustment( iInObj ).Adjustment( jYear ) = NumArray( jYear );
				}
			}
		}
	}

	int
	MonthToMonthNumber(
		std::string const & inMonthString,
		int const inDefaultMonth
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   May 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		//  Convert the string of the name of the month into numbers 1 to 12

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		int MonthToMonthNumber;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:

		if ( SameString( inMonthString, "January" ) ) {
			MonthToMonthNumber = 1;
		} else if ( SameString( inMonthString, "February" ) ) {
			MonthToMonthNumber = 2;
		} else if ( SameString( inMonthString, "March" ) ) {
			MonthToMonthNumber = 3;
		} else if ( SameString( inMonthString, "April" ) ) {
			MonthToMonthNumber = 4;
		} else if ( SameString( inMonthString, "May" ) ) {
			MonthToMonthNumber = 5;
		} else if ( SameString( inMonthString, "June" ) ) {
			MonthToMonthNumber = 6;
		} else if ( SameString( inMonthString, "July" ) ) {
			MonthToMonthNumber = 7;
		} else if ( SameString( inMonthString, "August" ) ) {
			MonthToMonthNumber = 8;
		} else if ( SameString( inMonthString, "September" ) ) {
			MonthToMonthNumber = 9;
		} else if ( SameString( inMonthString, "October" ) ) {
			MonthToMonthNumber = 10;
		} else if ( SameString( inMonthString, "November" ) ) {
			MonthToMonthNumber = 11;
		} else if ( SameString( inMonthString, "December" ) ) {
			MonthToMonthNumber = 12;
		} else {
			MonthToMonthNumber = inDefaultMonth;
		}
		return MonthToMonthNumber;
	}

	//======================================================================================================================
	//======================================================================================================================

	//    COMPUTATION ROUTINES

	//======================================================================================================================
	//======================================================================================================================

	void
	ExpressAsCashFlows()
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   July 2010
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//    Convert all recurring and nonrecurring costs into cash flows
		//    used in calculations and reporting.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Using/Aliasing
		using EconomicTariff::GetMonthlyCostForResource;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int iCashFlow;
		int iResource;
		int jCost;
		int jMonth;
		int jAdj;
		int kYear;
		int offset;
		int month; // number of months since base date
		int firstMonth;
		static int baseMonths1900( 0 ); // number of months since 1900 for base period
		static int serviceMonths1900( 0 ); // number of months since 1900 for service period
		int monthsBaseToService;
		Array2D< Real64 > resourceCosts;
		Array1D< Real64 > curResourceCosts( 12 );
		Array1D_bool resourceCostNotZero;
		Array1D< Real64 > resourceCostAnnual;
		Real64 annualCost;
		int cashFlowCounter;
		int found;
		int curCategory;
		Array1D< Real64 > monthlyInflationFactor;
		Real64 inflationPerMonth;
		int iLoop;

		// compute months from 1900 for base and service period
		baseMonths1900 = ( baseDateYear - 1900 ) * 12 + baseDateMonth;
		serviceMonths1900 = ( serviceDateYear - 1900 ) * 12 + serviceDateMonth;
		monthsBaseToService = serviceMonths1900 - baseMonths1900;
		// if ComponentCost:LineItem exist, the grand total of all costs are another non-recurring cost
		if ( CurntBldg.GrandTotal > 0.0 ) { //from DataCostEstimate and computed in WriteCompCostTable within OutputReportTabular
			++numNonrecurringCost;
			NonrecurringCost( numNonrecurringCost ).name = "Total of ComponentCost:*";
			NonrecurringCost( numNonrecurringCost ).lineItem = "";
			NonrecurringCost( numNonrecurringCost ).category = costCatConstruction;
			NonrecurringCost( numNonrecurringCost ).cost = CurntBldg.GrandTotal;
			NonrecurringCost( numNonrecurringCost ).startOfCosts = startBasePeriod;
			NonrecurringCost( numNonrecurringCost ).yearsFromStart = 0;
			NonrecurringCost( numNonrecurringCost ).monthsFromStart = 0;
			NonrecurringCost( numNonrecurringCost ).totalMonthsFromStart = 0;
		}
		// gather costs from EconomicTariff for each end use
		resourceCosts.allocate( 12, NumOfResourceTypes );
		resourceCostNotZero.allocate( NumOfResourceTypes );
		resourceCostAnnual.allocate( NumOfResourceTypes );
		numResourcesUsed = 0;
		for ( iResource = 1; iResource <= NumOfResourceTypes; ++iResource ) {
			GetMonthlyCostForResource( iResource + ResourceTypeInitialOffset, curResourceCosts );
			annualCost = 0.0;
			for ( jMonth = 1; jMonth <= 12; ++jMonth ) {
				resourceCosts( jMonth, iResource ) = curResourceCosts( jMonth );
				annualCost += resourceCosts( jMonth, iResource );
			}
			if ( annualCost != 0.0 ) {
				++numResourcesUsed;
				resourceCostNotZero( iResource ) = true;
			} else {
				resourceCostNotZero( iResource ) = false;
			}
			resourceCostAnnual( iResource ) = annualCost;
		}
		// pre-compute the inflation factors for each year
		monthlyInflationFactor.allocate( lengthStudyTotalMonths );
		if ( inflationApproach == inflAppConstantDollar ) {
			monthlyInflationFactor = 1.0; //not really used but just in case
		} else if ( inflationApproach == inflAppCurrentDollar ) {
			// to allocate an interest rate (in this case inflation) cannot just use 1/12
			// for the monthly value since it will be slightly wrong. Instead use inverse of
			// formula from Newnan (4-32) which is r = m x (ia + 1)^(1/m) - 1)
			inflationPerMonth = std::pow( inflation + 1.0, 1.0 / 12.0 ) - 1;
			for ( jMonth = 1; jMonth <= lengthStudyTotalMonths; ++jMonth ) {
				monthlyInflationFactor( jMonth ) = std::pow( 1.0 + inflationPerMonth, jMonth - 1 );
			}
		}

		numCashFlow = countOfCostCat + numRecurringCosts + numNonrecurringCost + numResourcesUsed;
		// Cashflow array order:
		//   1 cost categories
		//   2 recurring costs
		//   3 nonrecurring costs
		//   4 resource costs
		CashFlow.allocate( numCashFlow );
		for ( iCashFlow = 1; iCashFlow <= numCashFlow; ++iCashFlow ) {
			CashFlow( iCashFlow ).mnAmount.allocate( lengthStudyTotalMonths );
			CashFlow( iCashFlow ).yrAmount.allocate( lengthStudyYears );
			CashFlow( iCashFlow ).yrPresVal.allocate( lengthStudyYears );
			CashFlow( iCashFlow ).mnAmount = 0.0; //zero all cash flow values
			CashFlow( iCashFlow ).yrAmount = 0.0; //zero all cash flow values
			CashFlow( iCashFlow ).yrPresVal = 0.0; //zero all present values
		}
		// Put nonrecurring costs into cashflows
		offset = countOfCostCat + numRecurringCosts;
		for ( jCost = 1; jCost <= numNonrecurringCost; ++jCost ) {
			CashFlow( offset + jCost ).name = NonrecurringCost( jCost ).name;
			CashFlow( offset + jCost ).SourceKind = skNonrecurring;
			CashFlow( offset + jCost ).Category = NonrecurringCost( jCost ).category;
			CashFlow( offset + jCost ).orginalCost = NonrecurringCost( jCost ).cost;
			CashFlow( offset + jCost ).mnAmount = 0.0;
			if ( NonrecurringCost( jCost ).startOfCosts == startServicePeriod ) {
				month = NonrecurringCost( jCost ).totalMonthsFromStart + monthsBaseToService + 1;
			} else if ( NonrecurringCost( jCost ).startOfCosts == startBasePeriod ) {
				month = NonrecurringCost( jCost ).totalMonthsFromStart + 1;
			}
			if ( ( month >= 1 ) && ( month <= lengthStudyTotalMonths ) ) {
				CashFlow( offset + jCost ).mnAmount( month ) = NonrecurringCost( jCost ).cost * monthlyInflationFactor( month );
			} else {
				ShowWarningError( "For life cycle costing a nonrecurring cost named " + NonrecurringCost( jCost ).name + " contains a cost which is not within the study period." );
			}
		}
		// Put recurring costs into cashflows
		offset = countOfCostCat;
		for ( jCost = 1; jCost <= numRecurringCosts; ++jCost ) {
			CashFlow( offset + jCost ).name = RecurringCosts( jCost ).name;
			CashFlow( offset + jCost ).SourceKind = skRecurring;
			CashFlow( offset + jCost ).Category = RecurringCosts( jCost ).category;
			CashFlow( offset + jCost ).orginalCost = RecurringCosts( jCost ).cost;
			if ( RecurringCosts( jCost ).startOfCosts == startServicePeriod ) {
				firstMonth = RecurringCosts( jCost ).totalMonthsFromStart + monthsBaseToService + 1;
			} else if ( RecurringCosts( jCost ).startOfCosts == startBasePeriod ) {
				firstMonth = RecurringCosts( jCost ).totalMonthsFromStart + 1;
			}
			if ( ( firstMonth >= 1 ) && ( firstMonth <= lengthStudyTotalMonths ) ) {
				month = firstMonth;
				if ( RecurringCosts( jCost ).totalRepeatPeriodMonths >= 1 ) {
					for ( iLoop = 1; iLoop <= 10000; ++iLoop ) { //add a limit to the loop to prevent runaway condition
						CashFlow( offset + jCost ).mnAmount( month ) = RecurringCosts( jCost ).cost * monthlyInflationFactor( month );
						month += RecurringCosts( jCost ).totalRepeatPeriodMonths;
						if ( month > lengthStudyTotalMonths ) break;
					}
				}
			} else {
				ShowWarningError( "For life cycle costing the recurring cost named " + RecurringCosts( jCost ).name + " has the first year of the costs that is not within the study period." );
			}
		}
		// Put resource costs into cashflows
		// the first cash flow for resources should be after the categories, recurring and nonrecurring costs
		cashFlowCounter = countOfCostCat + numRecurringCosts + numNonrecurringCost;
		for ( iResource = 1; iResource <= NumOfResourceTypes; ++iResource ) {
			if ( resourceCostNotZero( iResource ) ) {
				++cashFlowCounter;
				CashFlow( cashFlowCounter ).Category = costCatEnergy;
				CashFlow( cashFlowCounter ).Resource = iResource + ResourceTypeInitialOffset;
				CashFlow( cashFlowCounter ).SourceKind = skResource;
				CashFlow( cashFlowCounter ).name = GetResourceTypeChar( iResource + ResourceTypeInitialOffset );
				if ( cashFlowCounter <= numCashFlow ) {
					//put the monthly energy costs into the cashflow prior to adjustments
					//energy costs (a.k.a. resource costs) start at the start of service and repeat
					//until the end of the study total
					for ( jMonth = 1; jMonth <= 12; ++jMonth ) {
						CashFlow( cashFlowCounter ).mnAmount( monthsBaseToService + jMonth ) = resourceCosts( jMonth, iResource );
					}
					CashFlow( cashFlowCounter ).orginalCost = resourceCostAnnual( iResource );
					for ( jMonth = monthsBaseToService + 13; jMonth <= lengthStudyTotalMonths; ++jMonth ) {
						// use the cost from a year earlier
						CashFlow( cashFlowCounter ).mnAmount( jMonth ) = CashFlow( cashFlowCounter ).mnAmount( jMonth - 12 );
					}
					// add in the impact of inflation
					for ( jMonth = 1; jMonth <= lengthStudyTotalMonths; ++jMonth ) {
						CashFlow( cashFlowCounter ).mnAmount( jMonth ) *= monthlyInflationFactor( jMonth );
					}
					// now factor in adjustments
					// need to find the correct adjustment to use for the current resource
					found = 0;
					for ( jAdj = 1; jAdj <= numUseAdjustment; ++jAdj ) {
						if ( UseAdjustment( jAdj ).resource == iResource + ResourceTypeInitialOffset ) {
							found = jAdj;
							break;
						}
					}
					// if any adjustments were found for that resource apply the multiplier
					if ( found != 0 ) {
						for ( kYear = 1; kYear <= lengthStudyYears; ++kYear ) { //if service period is later than base period then this will go too far
							for ( jMonth = 1; jMonth <= 12; ++jMonth ) {
								month = ( kYear - 1 ) * 12 + jMonth;
								if ( month > lengthStudyTotalMonths ) break;
								CashFlow( cashFlowCounter ).mnAmount( month ) *= UseAdjustment( found ).Adjustment( kYear );
							}
						}
					}
				}
			}
		}
		//put cashflows into categories
		for ( jCost = 1; jCost <= countOfCostCat; ++jCost ) {
			CashFlow( jCost ).Category = jCost; //make each category the type indicated
			CashFlow( jCost ).SourceKind = skSum;
		}
		//add the cashflows by category
		for ( jCost = countOfCostCat + 1; jCost <= numCashFlow; ++jCost ) {
			curCategory = CashFlow( jCost ).Category;
			if ( ( curCategory <= countOfCostCat ) && ( curCategory >= 1 ) ) {
				for ( jMonth = 1; jMonth <= lengthStudyTotalMonths; ++jMonth ) {
					CashFlow( curCategory ).mnAmount( jMonth ) += CashFlow( jCost ).mnAmount( jMonth );
				}
			}
		}
		//create total categories
		for ( jMonth = 1; jMonth <= lengthStudyTotalMonths; ++jMonth ) {
			CashFlow( costCatTotEnergy ).mnAmount( jMonth ) = CashFlow( costCatEnergy ).mnAmount( jMonth );
			CashFlow( costCatTotOper ).mnAmount( jMonth ) = CashFlow( costCatMaintenance ).mnAmount( jMonth ) + CashFlow( costCatRepair ).mnAmount( jMonth ) + CashFlow( costCatOperation ).mnAmount( jMonth ) + CashFlow( costCatReplacement ).mnAmount( jMonth ) + CashFlow( costCatMinorOverhaul ).mnAmount( jMonth ) + CashFlow( costCatMajorOverhaul ).mnAmount( jMonth ) + CashFlow( costCatOtherOperational ).mnAmount( jMonth ) + CashFlow( costCatWater ).mnAmount( jMonth ) + CashFlow( costCatEnergy ).mnAmount( jMonth );
			CashFlow( costCatTotCaptl ).mnAmount( jMonth ) = CashFlow( costCatConstruction ).mnAmount( jMonth ) + CashFlow( costCatSalvage ).mnAmount( jMonth ) + CashFlow( costCatOtherCapital ).mnAmount( jMonth );
			CashFlow( costCatTotGrand ).mnAmount( jMonth ) = CashFlow( costCatTotOper ).mnAmount( jMonth ) + CashFlow( costCatTotCaptl ).mnAmount( jMonth );
		}
		//convert all monthly cashflows into yearly cashflows
		for ( jCost = 1; jCost <= numCashFlow; ++jCost ) {
			for ( kYear = 1; kYear <= lengthStudyYears; ++kYear ) {
				annualCost = 0.0;
				for ( jMonth = 1; jMonth <= 12; ++jMonth ) {
					month = ( kYear - 1 ) * 12 + jMonth;
					if ( month <= lengthStudyTotalMonths ) {
						annualCost += CashFlow( jCost ).mnAmount( month );
					}
				}
				CashFlow( jCost ).yrAmount( kYear ) = annualCost;
			}
		}
	}

	void
	ComputePresentValue()
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   August 2010
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//    For each cashflow, compute the present value

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 totalPV;
		int curCategory;
		int curResource;
		Real64 curDiscountRate;
		int iCashFlow;
		int jYear;
		int kResource;
		int nUsePriceEsc;
		Real64 effectiveYear;

		// identify how each cashflow should be treated
		for ( iCashFlow = 1; iCashFlow <= numCashFlow; ++iCashFlow ) {
			{ auto const SELECT_CASE_var( CashFlow( iCashFlow ).SourceKind );
			if ( SELECT_CASE_var == skResource ) {
				//only for real fuels purchased such as electricity, natural gas, etc..
				if ( ( CashFlow(iCashFlow).Resource >= iRT_Electricity ) && ( CashFlow(iCashFlow).Resource <= iRT_ElectricitySurplusSold ) ) {
					CashFlow( iCashFlow ).pvKind = pvkEnergy;
				} else {
					CashFlow( iCashFlow ).pvKind = pvkNonEnergy;
				}
			} else if ( ( SELECT_CASE_var == skRecurring ) || ( SELECT_CASE_var == skNonrecurring ) ) {
				if ( CashFlow( iCashFlow ).Category == costCatEnergy ) {
					CashFlow( iCashFlow ).pvKind = pvkEnergy;
				} else {
					CashFlow( iCashFlow ).pvKind = pvkNonEnergy;
				}
			} else if ( SELECT_CASE_var == skSum ) {
				CashFlow( iCashFlow ).pvKind = pvkNotComputed;
			} else {
				CashFlow( iCashFlow ).pvKind = pvkNotComputed;
			}}
		}
		// compute the Single Present Value factors based on the discount rate
		SPV.allocate( lengthStudyYears );
		energySPV.allocate( lengthStudyYears, NumOfResourceTypes );
		// Depending if using Constant or Current Dollar analysis
		// use the appropriate discount rate
		if ( inflationApproach == inflAppConstantDollar ) {
			curDiscountRate = realDiscountRate;
		} else if ( inflationApproach == inflAppCurrentDollar ) {
			curDiscountRate = nominalDiscountRate;
		}
		//compute single present values based on real discount rates
		for ( jYear = 1; jYear <= lengthStudyYears; ++jYear ) {
			// NIST 155 D.2.1.1 - Single Present Value (SPV) formula
			{ auto const SELECT_CASE_var( discountConvension );
			if ( SELECT_CASE_var == disConvBeginOfYear ) {
				effectiveYear = double( jYear ) - 1.0;
			} else if ( SELECT_CASE_var == disConvMidYear ) {
				effectiveYear = double( jYear ) - 0.5;
			} else if ( SELECT_CASE_var == disConvEndOfYear ) {
				effectiveYear = double( jYear );
			} else {
			}}
			SPV( jYear ) = 1.0 / std::pow( 1.0 + curDiscountRate, effectiveYear );
		}
		//use SPV as default values for all energy types
		for ( jYear = 1; jYear <= lengthStudyYears; ++jYear ) {
			for ( kResource = 1; kResource <= NumOfResourceTypes; ++kResource ) {
				energySPV( jYear, kResource ) = SPV( jYear );
			}
		}
		//loop through the resources and if they match a UseEscalation use those values instead
		for ( nUsePriceEsc = 1; nUsePriceEsc <= numUsePriceEscalation; ++nUsePriceEsc ) {
			curResource = UsePriceEscalation( nUsePriceEsc ).resource - ResourceTypeInitialOffset;
			if ( ( curResource >= 1 ) && ( curResource < NumOfResourceTypes ) ) {
				for ( jYear = 1; jYear <= lengthStudyYears; ++jYear ) {
					//the following is based on UPV* formula from NIST 135 supplement but is for a single year
					{ auto const SELECT_CASE_var( discountConvension );
					if ( SELECT_CASE_var == disConvBeginOfYear ) {
						effectiveYear = double( jYear ) - 1.0;
					} else if ( SELECT_CASE_var == disConvMidYear ) {
						effectiveYear = double( jYear ) - 0.5;
					} else if ( SELECT_CASE_var == disConvEndOfYear ) {
						effectiveYear = double( jYear );
					} else {
					}}
					energySPV( jYear, curResource ) = UsePriceEscalation( nUsePriceEsc ).Escalation( jYear ) / std::pow( 1.0 + curDiscountRate, effectiveYear );
				}
			}
		}
		for ( iCashFlow = 1; iCashFlow <= numCashFlow; ++iCashFlow ) {
			{ auto const SELECT_CASE_var( CashFlow( iCashFlow ).pvKind );
			if ( SELECT_CASE_var == pvkNonEnergy ) {
				totalPV = 0.0;
				for ( jYear = 1; jYear <= lengthStudyYears; ++jYear ) {
					CashFlow( iCashFlow ).yrPresVal( jYear ) = CashFlow( iCashFlow ).yrAmount( jYear ) * SPV( jYear );
					totalPV += CashFlow( iCashFlow ).yrPresVal( jYear );
				}
				CashFlow( iCashFlow ).presentValue = totalPV;
			} else if ( SELECT_CASE_var == pvkEnergy ) {
				curResource = CashFlow( iCashFlow ).Resource - ResourceTypeInitialOffset;
				if ( ( curResource >= 1 ) && ( curResource < NumOfResourceTypes ) ) {
					totalPV = 0.0;
					for ( jYear = 1; jYear <= lengthStudyYears; ++jYear ) {
						CashFlow( iCashFlow ).yrPresVal( jYear ) = CashFlow( iCashFlow ).yrAmount( jYear ) * energySPV( jYear, curResource );
						totalPV += CashFlow( iCashFlow ).yrPresVal( jYear );
					}
					CashFlow( iCashFlow ).presentValue = totalPV;
				}
			} else if ( SELECT_CASE_var == pvkNotComputed ) {
				// do nothing
			}}
		}
		// sum by category
		for ( int i = 1; i <= countOfCostCat; ++i ) {
			CashFlow(i).presentValue = 0; //initialize value to zero before summing in next for loop
		}
		for (iCashFlow = countOfCostCat + 1; iCashFlow <= numCashFlow; ++iCashFlow) {
			curCategory = CashFlow( iCashFlow ).Category;
			if ( ( curCategory <= countOfCostCat ) && ( curCategory >= 1 ) ) {
				CashFlow( curCategory ).presentValue += CashFlow( iCashFlow ).presentValue;
				for ( jYear = 1; jYear <= lengthStudyYears; ++jYear ) {
					CashFlow( curCategory ).yrPresVal( jYear ) += CashFlow( iCashFlow ).yrPresVal( jYear );
				}
			}
		}
		//create total categories
		CashFlow( costCatTotEnergy ).presentValue = CashFlow( costCatEnergy ).presentValue;
		CashFlow( costCatTotOper ).presentValue = CashFlow( costCatMaintenance ).presentValue + CashFlow( costCatRepair ).presentValue + CashFlow( costCatOperation ).presentValue + CashFlow( costCatReplacement ).presentValue + CashFlow( costCatMinorOverhaul ).presentValue + CashFlow( costCatMajorOverhaul ).presentValue + CashFlow( costCatOtherOperational ).presentValue + CashFlow( costCatWater ).presentValue + CashFlow( costCatEnergy ).presentValue;
		CashFlow( costCatTotCaptl ).presentValue = CashFlow( costCatConstruction ).presentValue + CashFlow( costCatSalvage ).presentValue + CashFlow( costCatOtherCapital ).presentValue;
		CashFlow( costCatTotGrand ).presentValue = CashFlow( costCatTotOper ).presentValue + CashFlow( costCatTotCaptl ).presentValue;
		for ( jYear = 1; jYear <= lengthStudyYears; ++jYear ) {
			CashFlow( costCatTotEnergy ).yrPresVal( jYear ) = CashFlow( costCatEnergy ).yrPresVal( jYear );
			CashFlow( costCatTotOper ).yrPresVal( jYear ) = CashFlow( costCatMaintenance ).yrPresVal( jYear ) + CashFlow( costCatRepair ).yrPresVal( jYear ) + CashFlow( costCatOperation ).yrPresVal( jYear ) + CashFlow( costCatReplacement ).yrPresVal( jYear ) + CashFlow( costCatMinorOverhaul ).yrPresVal( jYear ) + CashFlow( costCatMajorOverhaul ).yrPresVal( jYear ) + CashFlow( costCatOtherOperational ).yrPresVal( jYear ) + CashFlow( costCatWater ).yrPresVal( jYear ) + CashFlow( costCatEnergy ).yrPresVal( jYear );
			CashFlow( costCatTotCaptl ).yrPresVal( jYear ) = CashFlow( costCatConstruction ).yrPresVal( jYear ) + CashFlow( costCatSalvage ).yrPresVal( jYear ) + CashFlow( costCatOtherCapital ).yrPresVal( jYear );
			CashFlow( costCatTotGrand ).yrPresVal( jYear ) = CashFlow( costCatTotOper ).yrPresVal( jYear ) + CashFlow( costCatTotCaptl ).yrPresVal( jYear );
		}

	}

	void
	ComputeTaxAndDepreciation()
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   August 2010
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//    Compute the present value after factoring in taxes
		//    and depreciation.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const SizeDepr( 41 );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Array1D< Real64 > DepreciationPercent( SizeDepr ); // values expressed as percent 5% is 5.0 (based on tables)
		Real64 curCapital;
		int curDepYear;
		int iYear;
		int jYear;

		DepreciatedCapital.allocate( lengthStudyYears );
		TaxableIncome.allocate( lengthStudyYears );
		Taxes.allocate( lengthStudyYears );
		AfterTaxCashFlow.allocate( lengthStudyYears );
		AfterTaxPresentValue.allocate( lengthStudyYears );

		// Depreciation factors are based on IRS Publication 946 for 2009 "How to Depreciate Property"
		// The MACRS valus are based on Modified Accelerated Cost Recovery System GDS for 3, 5, 7, 10 year
		// property are based on 200% depreciation method shown in Appendix A using half year. 15 and 20 are
		// based on 150% (Chart 1). For Straight Line depreciation GDS is used for 27 years (actually 27.5)
		// 31 years (actually 31.5 years) and 39 years using mid month. For 40 years ADS is used (chart 2)
		// Table A-1 is used for 3, 4, 5, 10, 15 and 20 years. Table A-6 is for 27 years. Table A-7 for 31 years.
		// Table A-7a for 39 years. Table A-13 for 40 years. These years are a classification of property
		// and should not be confused with the length of the study. For 27 years, 31 years, 39 years and 40 years
		// the June value was used.
		DepreciationPercent = 0.0; //default all values to zero
		{ auto const SELECT_CASE_var( depreciationMethod );
		if ( SELECT_CASE_var == depMethMACRS3 ) { // IRS Publication 946 for 2009 Table A-1
			DepreciationPercent( 1 ) = 33.33;
			DepreciationPercent( 2 ) = 44.45;
			DepreciationPercent( 3 ) = 14.81;
			DepreciationPercent( 4 ) = 7.41;
		} else if ( SELECT_CASE_var == depMethMACRS5 ) { // IRS Publication 946 for 2009 Table A-1
			DepreciationPercent( 1 ) = 20.0;
			DepreciationPercent( 2 ) = 32.0;
			DepreciationPercent( 3 ) = 19.2;
			DepreciationPercent( 4 ) = 11.52;
			DepreciationPercent( 5 ) = 11.52;
			DepreciationPercent( 6 ) = 5.76;
		} else if ( SELECT_CASE_var == depMethMACRS7 ) { // IRS Publication 946 for 2009 Table A-1
			DepreciationPercent( 1 ) = 14.29;
			DepreciationPercent( 2 ) = 24.49;
			DepreciationPercent( 3 ) = 17.49;
			DepreciationPercent( 4 ) = 12.49;
			DepreciationPercent( 5 ) = 8.93;
			DepreciationPercent( 6 ) = 8.92;
			DepreciationPercent( 7 ) = 8.93;
			DepreciationPercent( 8 ) = 4.46;
		} else if ( SELECT_CASE_var == depMethMACRS10 ) { // IRS Publication 946 for 2009 Table A-1
			DepreciationPercent( 1 ) = 10.0;
			DepreciationPercent( 2 ) = 18.0;
			DepreciationPercent( 3 ) = 14.4;
			DepreciationPercent( 4 ) = 11.52;
			DepreciationPercent( 5 ) = 9.22;
			DepreciationPercent( 6 ) = 7.37;
			DepreciationPercent( 7 ) = 6.55;
			DepreciationPercent( 8 ) = 6.55;
			DepreciationPercent( 9 ) = 6.56;
			DepreciationPercent( 10 ) = 6.55;
			DepreciationPercent( 11 ) = 3.28;
		} else if ( SELECT_CASE_var == depMethMACRS15 ) { // IRS Publication 946 for 2009 Table A-1
			DepreciationPercent( 1 ) = 5.0;
			DepreciationPercent( 2 ) = 9.5;
			DepreciationPercent( 3 ) = 8.55;
			DepreciationPercent( 4 ) = 7.7;
			DepreciationPercent( 5 ) = 6.93;
			DepreciationPercent( 6 ) = 6.23;
			DepreciationPercent( 7 ) = 5.9;
			DepreciationPercent( 8 ) = 5.9;
			DepreciationPercent( 9 ) = 5.91;
			DepreciationPercent( 10 ) = 5.9;
			DepreciationPercent( 11 ) = 5.91;
			DepreciationPercent( 12 ) = 5.9;
			DepreciationPercent( 13 ) = 5.91;
			DepreciationPercent( 14 ) = 5.9;
			DepreciationPercent( 15 ) = 5.91;
			DepreciationPercent( 16 ) = 2.95;
		} else if ( SELECT_CASE_var == depMethMACRS20 ) { // IRS Publication 946 for 2009 Table A-1
			DepreciationPercent( 1 ) = 3.75;
			DepreciationPercent( 2 ) = 7.219;
			DepreciationPercent( 3 ) = 6.677;
			DepreciationPercent( 4 ) = 6.177;
			DepreciationPercent( 5 ) = 5.713;
			DepreciationPercent( 6 ) = 5.285;
			DepreciationPercent( 7 ) = 4.888;
			DepreciationPercent( 8 ) = 4.522;
			DepreciationPercent( 9 ) = 4.462;
			DepreciationPercent( 10 ) = 4.461;
			DepreciationPercent( 11 ) = 4.462;
			DepreciationPercent( 12 ) = 4.461;
			DepreciationPercent( 13 ) = 4.462;
			DepreciationPercent( 14 ) = 4.461;
			DepreciationPercent( 15 ) = 4.462;
			DepreciationPercent( 16 ) = 4.461;
			DepreciationPercent( 17 ) = 4.462;
			DepreciationPercent( 18 ) = 4.461;
			DepreciationPercent( 19 ) = 4.462;
			DepreciationPercent( 20 ) = 4.461;
			DepreciationPercent( 21 ) = 2.231;
		} else if ( SELECT_CASE_var == depMethStraight27 ) { // IRS Publication 946 for 2009 Table A-6 (June)
			DepreciationPercent( 1 ) = 1.97;
			DepreciationPercent( 2 ) = 3.636;
			DepreciationPercent( 3 ) = 3.636;
			DepreciationPercent( 4 ) = 3.636;
			DepreciationPercent( 5 ) = 3.636;
			DepreciationPercent( 6 ) = 3.636;
			DepreciationPercent( 7 ) = 3.636;
			DepreciationPercent( 8 ) = 3.636;
			DepreciationPercent( 9 ) = 3.636;
			DepreciationPercent( 10 ) = 3.637;
			DepreciationPercent( 11 ) = 3.636;
			DepreciationPercent( 12 ) = 3.637;
			DepreciationPercent( 13 ) = 3.636;
			DepreciationPercent( 14 ) = 3.637;
			DepreciationPercent( 15 ) = 3.636;
			DepreciationPercent( 16 ) = 3.637;
			DepreciationPercent( 17 ) = 3.636;
			DepreciationPercent( 18 ) = 3.637;
			DepreciationPercent( 19 ) = 3.636;
			DepreciationPercent( 20 ) = 3.637;
			DepreciationPercent( 21 ) = 3.636;
			DepreciationPercent( 22 ) = 3.637;
			DepreciationPercent( 23 ) = 3.636;
			DepreciationPercent( 24 ) = 3.637;
			DepreciationPercent( 25 ) = 3.636;
			DepreciationPercent( 26 ) = 3.637;
			DepreciationPercent( 27 ) = 3.636;
			DepreciationPercent( 28 ) = 3.485;
		} else if ( SELECT_CASE_var == depMethStraight31 ) { // IRS Publication 946 for 2009 Table A-7 (June)
			DepreciationPercent( 1 ) = 1.72;
			DepreciationPercent( 2 ) = 3.175;
			DepreciationPercent( 3 ) = 3.175;
			DepreciationPercent( 4 ) = 3.175;
			DepreciationPercent( 5 ) = 3.175;
			DepreciationPercent( 6 ) = 3.175;
			DepreciationPercent( 7 ) = 3.175;
			DepreciationPercent( 8 ) = 3.174;
			DepreciationPercent( 9 ) = 3.175;
			DepreciationPercent( 10 ) = 3.174;
			DepreciationPercent( 11 ) = 3.175;
			DepreciationPercent( 12 ) = 3.174;
			DepreciationPercent( 13 ) = 3.175;
			DepreciationPercent( 14 ) = 3.174;
			DepreciationPercent( 15 ) = 3.175;
			DepreciationPercent( 16 ) = 3.174;
			DepreciationPercent( 17 ) = 3.175;
			DepreciationPercent( 18 ) = 3.174;
			DepreciationPercent( 19 ) = 3.175;
			DepreciationPercent( 20 ) = 3.174;
			DepreciationPercent( 21 ) = 3.175;
			DepreciationPercent( 22 ) = 3.174;
			DepreciationPercent( 23 ) = 3.175;
			DepreciationPercent( 24 ) = 3.174;
			DepreciationPercent( 25 ) = 3.175;
			DepreciationPercent( 26 ) = 3.174;
			DepreciationPercent( 27 ) = 3.175;
			DepreciationPercent( 28 ) = 3.174;
			DepreciationPercent( 29 ) = 3.175;
			DepreciationPercent( 30 ) = 3.174;
			DepreciationPercent( 31 ) = 3.175;
			DepreciationPercent( 32 ) = 3.042;
		} else if ( SELECT_CASE_var == depMethStraight39 ) { // IRS Publication 946 for 2009 Table A-7a (June)
			DepreciationPercent( 1 ) = 1.391;
			DepreciationPercent( 2 ) = 2.564;
			DepreciationPercent( 3 ) = 2.564;
			DepreciationPercent( 4 ) = 2.564;
			DepreciationPercent( 5 ) = 2.564;
			DepreciationPercent( 6 ) = 2.564;
			DepreciationPercent( 7 ) = 2.564;
			DepreciationPercent( 8 ) = 2.564;
			DepreciationPercent( 9 ) = 2.564;
			DepreciationPercent( 10 ) = 2.564;
			DepreciationPercent( 11 ) = 2.564;
			DepreciationPercent( 12 ) = 2.564;
			DepreciationPercent( 13 ) = 2.564;
			DepreciationPercent( 14 ) = 2.564;
			DepreciationPercent( 15 ) = 2.564;
			DepreciationPercent( 16 ) = 2.564;
			DepreciationPercent( 17 ) = 2.564;
			DepreciationPercent( 18 ) = 2.564;
			DepreciationPercent( 19 ) = 2.564;
			DepreciationPercent( 20 ) = 2.564;
			DepreciationPercent( 21 ) = 2.564;
			DepreciationPercent( 22 ) = 2.564;
			DepreciationPercent( 23 ) = 2.564;
			DepreciationPercent( 24 ) = 2.564;
			DepreciationPercent( 25 ) = 2.564;
			DepreciationPercent( 26 ) = 2.564;
			DepreciationPercent( 27 ) = 2.564;
			DepreciationPercent( 28 ) = 2.564;
			DepreciationPercent( 29 ) = 2.564;
			DepreciationPercent( 30 ) = 2.564;
			DepreciationPercent( 31 ) = 2.564;
			DepreciationPercent( 32 ) = 2.564;
			DepreciationPercent( 33 ) = 2.564;
			DepreciationPercent( 34 ) = 2.564;
			DepreciationPercent( 35 ) = 2.564;
			DepreciationPercent( 36 ) = 2.564;
			DepreciationPercent( 37 ) = 2.564;
			DepreciationPercent( 38 ) = 2.564;
			DepreciationPercent( 39 ) = 2.564;
			DepreciationPercent( 40 ) = 1.177;
		} else if ( SELECT_CASE_var == depMethStraight40 ) { // IRS Publication 946 for 2009 Table A-13 (June)
			DepreciationPercent( 1 ) = 1.354;
			DepreciationPercent( 2 ) = 2.5;
			DepreciationPercent( 3 ) = 2.5;
			DepreciationPercent( 4 ) = 2.5;
			DepreciationPercent( 5 ) = 2.5;
			DepreciationPercent( 6 ) = 2.5;
			DepreciationPercent( 7 ) = 2.5;
			DepreciationPercent( 8 ) = 2.5;
			DepreciationPercent( 9 ) = 2.5;
			DepreciationPercent( 10 ) = 2.5;
			DepreciationPercent( 11 ) = 2.5;
			DepreciationPercent( 12 ) = 2.5;
			DepreciationPercent( 13 ) = 2.5;
			DepreciationPercent( 14 ) = 2.5;
			DepreciationPercent( 15 ) = 2.5;
			DepreciationPercent( 16 ) = 2.5;
			DepreciationPercent( 17 ) = 2.5;
			DepreciationPercent( 18 ) = 2.5;
			DepreciationPercent( 19 ) = 2.5;
			DepreciationPercent( 20 ) = 2.5;
			DepreciationPercent( 21 ) = 2.5;
			DepreciationPercent( 22 ) = 2.5;
			DepreciationPercent( 23 ) = 2.5;
			DepreciationPercent( 24 ) = 2.5;
			DepreciationPercent( 25 ) = 2.5;
			DepreciationPercent( 26 ) = 2.5;
			DepreciationPercent( 27 ) = 2.5;
			DepreciationPercent( 28 ) = 2.5;
			DepreciationPercent( 29 ) = 2.5;
			DepreciationPercent( 30 ) = 2.5;
			DepreciationPercent( 31 ) = 2.5;
			DepreciationPercent( 32 ) = 2.5;
			DepreciationPercent( 33 ) = 2.5;
			DepreciationPercent( 34 ) = 2.5;
			DepreciationPercent( 35 ) = 2.5;
			DepreciationPercent( 36 ) = 2.5;
			DepreciationPercent( 37 ) = 2.5;
			DepreciationPercent( 38 ) = 2.5;
			DepreciationPercent( 39 ) = 2.5;
			DepreciationPercent( 40 ) = 2.5;
			DepreciationPercent( 41 ) = 1.146;
		}}
		// convert construction costs (not salvage) into depreciation
		DepreciatedCapital = 0.0; // set all years to zero
		for ( iYear = 1; iYear <= lengthStudyYears; ++iYear ) {
			curCapital = CashFlow( costCatConstruction ).yrAmount( iYear ) + CashFlow( costCatOtherCapital ).yrAmount( iYear );
			for ( jYear = 1; jYear <= SizeDepr; ++jYear ) {
				curDepYear = iYear + jYear - 1; //start depreciating with the year that the capital was shown and go to years following
				if ( curDepYear <= lengthStudyYears ) {
					DepreciatedCapital( curDepYear ) += curCapital * ( DepreciationPercent( jYear ) / 100 );
				}
			}
		}
		// Using Newnan pg 3880
		//   before-tax cash flow
		//   depreciation
		//   taxable income (before-tax cash flow - depreciation)
		//   income taxes (taxable income x incremental tax rate)
		//   after-tax cash flow (before-tax cash flow - income taxes)
		for ( iYear = 1; iYear <= lengthStudyYears; ++iYear ) {
			TaxableIncome( iYear ) = CashFlow( costCatTotGrand ).yrAmount( iYear ) - DepreciatedCapital( iYear );
			Taxes( iYear ) = TaxableIncome( iYear ) * taxRate;
			AfterTaxCashFlow( iYear ) = CashFlow( costCatTotGrand ).yrAmount( iYear ) - Taxes( iYear );
			// the present value after taxes is pretax present value minus the present value of the taxes
			AfterTaxPresentValue( iYear ) = CashFlow( costCatTotGrand ).yrPresVal( iYear ) - Taxes( iYear ) * SPV( iYear );
		}
	}

	//======================================================================================================================
	//======================================================================================================================

	//    OUTPUT ROUTINES

	//======================================================================================================================
	//======================================================================================================================

	void
	WriteTabularLifeCycleCostReport()
	{
		// SUBROUTINE INFORMATION:
		//    AUTHOR         Jason Glazer of GARD Analytics, Inc.
		//    DATE WRITTEN   June 2010
		//    MODIFIED       na
		//    RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//    Write the output report related to life-cycle costing
		//    to the tabular output file.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// Using/Aliasing
		using OutputReportTabular::WriteReportHeaders;
		using OutputReportTabular::WriteSubtitle;
		using OutputReportTabular::WriteTable;
		using OutputReportTabular::RealToStr;
		using OutputReportTabular::IntToStr;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// all arrays are in the format: (row, column)
		Array1D_string columnHead;
		Array1D_int columnWidth;
		Array1D_string rowHead;
		Array2D_string tableBody;

		int numColumns;
		int iYear;
		int jObj;
		int kMonth;
		int curCashFlow;
		int numRows;
		int offset;
		int numYears;
		Real64 totalPV;

		if ( LCCparamPresent ) {
			//---------------------------------
			// Life-Cycle Cost Verification and Results Report
			//---------------------------------
			WriteReportHeaders( "Life-Cycle Cost Report", "Entire Facility", 1 );
			//---- Life-Cycle Cost Parameters
			rowHead.allocate( 11 );
			columnHead.allocate( 1 );
			columnWidth.allocate( 1 );
			tableBody.allocate( 1, 11 );
			tableBody = "";
			rowHead( 1 ) = "Name";
			rowHead( 2 ) = "Discounting Convention";
			rowHead( 3 ) = "Inflation Approach";
			rowHead( 4 ) = "Real Discount Rate";
			rowHead( 5 ) = "Nominal Discount Rate";
			rowHead( 6 ) = "Inflation";
			rowHead( 7 ) = "Base Date";
			rowHead( 8 ) = "Service Date";
			rowHead( 9 ) = "Length of Study Period in Years";
			rowHead( 10 ) = "Tax rate";
			rowHead( 11 ) = "Depreciation Method";
			columnHead( 1 ) = "Value";

			tableBody( 1, 1 ) = LCCname;
			if ( discountConvension == disConvEndOfYear ) {
				tableBody( 1, 2 ) = "EndOfYear";
			} else if ( discountConvension == disConvMidYear ) {
				tableBody( 1, 2 ) = "MidYear";
			} else if ( discountConvension == disConvBeginOfYear ) {
				tableBody( 1, 2 ) = "BeginningOfYear";
			}
			if ( inflationApproach == inflAppConstantDollar ) {
				tableBody( 1, 3 ) = "ConstantDollar";
			} else if ( inflationApproach == inflAppCurrentDollar ) {
				tableBody( 1, 3 ) = "CurrentDollar";
			}
			if ( inflationApproach == inflAppConstantDollar ) {
				tableBody( 1, 4 ) = RealToStr( realDiscountRate, 4 );
			} else {
				tableBody( 1, 4 ) = "-- N/A --";
			}
			if ( inflationApproach == inflAppCurrentDollar ) {
				tableBody( 1, 5 ) = RealToStr( nominalDiscountRate, 4 );
			} else {
				tableBody( 1, 5 ) = "-- N/A --";
			}
			if ( inflationApproach == inflAppCurrentDollar ) {
				tableBody( 1, 6 ) = RealToStr( inflation, 4 );
			} else {
				tableBody( 1, 6 ) = "-- N/A --";
			}
			tableBody( 1, 7 ) = MonthNames( baseDateMonth ) + ' ' + IntToStr( baseDateYear );
			tableBody( 1, 8 ) = MonthNames( serviceDateMonth ) + ' ' + IntToStr( serviceDateYear );
			tableBody( 1, 9 ) = IntToStr( lengthStudyYears );
			tableBody( 1, 10 ) = RealToStr( taxRate, 4 );
			{ auto const SELECT_CASE_var( depreciationMethod );
			if ( SELECT_CASE_var == depMethMACRS3 ) {
				tableBody( 1, 11 ) = "ModifiedAcceleratedCostRecoverySystem-3year";
			} else if ( SELECT_CASE_var == depMethMACRS5 ) {
				tableBody( 1, 11 ) = "ModifiedAcceleratedCostRecoverySystem-5year";
			} else if ( SELECT_CASE_var == depMethMACRS7 ) {
				tableBody( 1, 11 ) = "ModifiedAcceleratedCostRecoverySystem-7year";
			} else if ( SELECT_CASE_var == depMethMACRS10 ) {
				tableBody( 1, 11 ) = "ModifiedAcceleratedCostRecoverySystem-10year";
			} else if ( SELECT_CASE_var == depMethMACRS15 ) {
				tableBody( 1, 11 ) = "ModifiedAcceleratedCostRecoverySystem-15year";
			} else if ( SELECT_CASE_var == depMethMACRS20 ) {
				tableBody( 1, 11 ) = "ModifiedAcceleratedCostRecoverySystem-20year";
			} else if ( SELECT_CASE_var == depMethStraight27 ) {
				tableBody( 1, 11 ) = "StraightLine-27year";
			} else if ( SELECT_CASE_var == depMethStraight31 ) {
				tableBody( 1, 11 ) = "StraightLine-31year";
			} else if ( SELECT_CASE_var == depMethStraight39 ) {
				tableBody( 1, 11 ) = "StraightLine-39year";
			} else if ( SELECT_CASE_var == depMethStraight40 ) {
				tableBody( 1, 11 ) = "StraightLine-40year";
			} else if ( SELECT_CASE_var == depMethNone ) {
				tableBody( 1, 11 ) = "None";
			}}
			columnWidth = 14; //array assignment - same for all columns
			WriteSubtitle( "Life-Cycle Cost Parameters" );
			WriteTable( tableBody, rowHead, columnHead, columnWidth );
			if ( sqlite ) {
				sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "Life-Cycle Cost Parameters" );
			}
			columnHead.deallocate();
			rowHead.deallocate();
			columnWidth.deallocate();
			tableBody.deallocate();
			//---- Use Price Escalation
			numColumns = max( 1, numUsePriceEscalation );
			rowHead.allocate( lengthStudyYears + 2 );
			columnHead.allocate( numColumns );
			columnWidth.dimension( numColumns, 14 ); //array assignment - same for all columns
			tableBody.allocate( numColumns, lengthStudyYears + 2 );
			tableBody = "";
			columnHead = "none";
			rowHead( 1 ) = "Resource";
			rowHead( 2 ) = "Start Date";
			for ( iYear = 1; iYear <= lengthStudyYears; ++iYear ) {
				rowHead( iYear + 2 ) = IntToStr( iYear );
			}
			for ( jObj = 1; jObj <= numUsePriceEscalation; ++jObj ) { //loop through objects not columns to add names
				columnHead( jObj ) = UsePriceEscalation( jObj ).name;
				tableBody( jObj, 1 ) = GetResourceTypeChar( UsePriceEscalation( jObj ).resource );
				tableBody( jObj, 2 ) = MonthNames( UsePriceEscalation( jObj ).escalationStartMonth ) + ' ' + IntToStr( UsePriceEscalation( jObj ).escalationStartYear );
			}
			for ( jObj = 1; jObj <= numUsePriceEscalation; ++jObj ) {
				for ( iYear = 1; iYear <= lengthStudyYears; ++iYear ) {
					tableBody( jObj, iYear + 2 ) = RealToStr( UsePriceEscalation( jObj ).Escalation( iYear ), 6 );
				}
			}
			WriteSubtitle( "Use Price Escalation" );
			WriteTable( tableBody, rowHead, columnHead, columnWidth );
			if ( sqlite ) {
				sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "Use Price Escalation" );
			}
			columnHead.deallocate();
			rowHead.deallocate();
			columnWidth.deallocate();
			tableBody.deallocate();
			//---- Use Adjustment
			if ( numUseAdjustment >= 1 ) { //only create table if objects used
				numColumns = max( 1, numUseAdjustment );
				numYears = lengthStudyYears - ( serviceDateYear - baseDateYear );
				rowHead.allocate( numYears + 1 );
				columnHead.allocate( numColumns );
				columnWidth.dimension( numColumns, 14 ); //array assignment - same for all columns
				tableBody.allocate( numColumns, numYears + 1 );
				tableBody = "";
				columnHead = "none";
				rowHead( 1 ) = "";
				for ( iYear = 1; iYear <= numYears; ++iYear ) {
					rowHead( iYear + 1 ) = MonthNames( serviceDateMonth ) + ' ' + IntToStr( serviceDateYear + iYear - 1 );
				}
				for ( jObj = 1; jObj <= numUseAdjustment; ++jObj ) { //loop through objects not columns to add names
					columnHead( jObj ) = UseAdjustment( jObj ).name;
					tableBody( jObj, 1 ) = GetResourceTypeChar( UseAdjustment( jObj ).resource );
				}
				for ( jObj = 1; jObj <= numUseAdjustment; ++jObj ) {
					for ( iYear = 1; iYear <= numYears; ++iYear ) {
						tableBody( jObj, iYear + 1 ) = RealToStr( UseAdjustment( jObj ).Adjustment( iYear ), 6 );
					}
				}
				WriteSubtitle( "Use Adjustment" );
				WriteTable( tableBody, rowHead, columnHead, columnWidth );
				if ( sqlite ) {
					sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "Use Adjustment" );
				}
				columnHead.deallocate();
				rowHead.deallocate();
				columnWidth.deallocate();
				tableBody.deallocate();
			}
			//---- Cash Flow for Recurring and Nonrecurring Costs
			numColumns = max( 1, numRecurringCosts + numNonrecurringCost );
			rowHead.allocate( lengthStudyYears + 1 );
			columnHead.allocate( numColumns );
			columnWidth.dimension( numColumns, 14 ); //array assignment - same for all columns
			tableBody.allocate( numColumns, lengthStudyYears + 1 );
			tableBody = "";
			rowHead( 1 ) = "";
			for ( iYear = 1; iYear <= lengthStudyYears; ++iYear ) {
				rowHead( iYear + 1 ) = MonthNames( baseDateMonth ) + ' ' + IntToStr( baseDateYear + iYear - 1 );
			}
			for ( jObj = 1; jObj <= ( numRecurringCosts + numNonrecurringCost ); ++jObj ) {
				curCashFlow = countOfCostCat + jObj;
				columnHead( jObj ) = CashFlow( curCashFlow ).name;
				{ auto const SELECT_CASE_var( CashFlow( curCashFlow ).SourceKind );
				if ( SELECT_CASE_var == skNonrecurring ) {
					tableBody( jObj, 1 ) = "Nonrecurring";
				} else if ( SELECT_CASE_var == skRecurring ) {
					tableBody( jObj, 1 ) = "Recurring";
				}}
				for ( iYear = 1; iYear <= lengthStudyYears; ++iYear ) {
					tableBody( jObj, iYear + 1 ) = RealToStr( CashFlow( curCashFlow ).yrAmount( iYear ), 2 );
				}
			}
			WriteSubtitle( "Cash Flow for Recurring and Nonrecurring Costs (Without Escalation)" );
			WriteTable( tableBody, rowHead, columnHead, columnWidth );
			if ( sqlite ) {
				sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "Cash Flow for Recurring and Nonrecurring Costs (Without Escalation)" );
			}
			columnHead.deallocate();
			rowHead.deallocate();
			columnWidth.deallocate();
			tableBody.deallocate();
			//---- Energy Cost Cash Flows
			numColumns = max( 1, numResourcesUsed );
			rowHead.allocate( lengthStudyYears );
			columnHead.allocate( numColumns );
			columnWidth.dimension( numColumns, 14 ); //array assignment - same for all columns
			tableBody.allocate( numColumns, lengthStudyYears );
			tableBody = "";
			for ( iYear = 1; iYear <= lengthStudyYears; ++iYear ) {
				rowHead( iYear ) = MonthNames( baseDateMonth ) + ' ' + IntToStr( baseDateYear + iYear - 1 );
			}
			for ( jObj = 1; jObj <= numResourcesUsed; ++jObj ) {
				curCashFlow = countOfCostCat + numRecurringCosts + numNonrecurringCost + jObj;
				columnHead( jObj ) = CashFlow( curCashFlow ).name;
				for ( iYear = 1; iYear <= lengthStudyYears; ++iYear ) {
					tableBody( jObj, iYear ) = RealToStr( CashFlow( curCashFlow ).yrAmount( iYear ), 2 );
				}
			}
			WriteSubtitle( "Energy Cost Cash Flows (Without Escalation)" );
			WriteTable( tableBody, rowHead, columnHead, columnWidth );
			if ( sqlite ) {
				sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "Energy Cost Cash Flows (Without Escalation)" );
			}
			columnHead.deallocate();
			rowHead.deallocate();
			columnWidth.deallocate();
			tableBody.deallocate();
			//---- Capital Cash Flow by Category
			rowHead.allocate( lengthStudyYears );
			columnHead.allocate( 4 );
			columnWidth.allocate( 4 );
			columnWidth = 14; //array assignment - same for all columns
			tableBody.allocate( 4, lengthStudyYears );
			tableBody = "";
			columnHead( 1 ) = "Construction";
			columnHead( 2 ) = "Salvage";
			columnHead( 3 ) = "OtherCapital";
			columnHead( 4 ) = "Total";
			for ( iYear = 1; iYear <= lengthStudyYears; ++iYear ) {
				rowHead( iYear ) = MonthNames( baseDateMonth ) + ' ' + IntToStr( baseDateYear + iYear - 1 );
				tableBody( 1, iYear ) = RealToStr( CashFlow( costCatConstruction ).yrAmount( iYear ), 2 );
				tableBody( 2, iYear ) = RealToStr( CashFlow( costCatSalvage ).yrAmount( iYear ), 2 );
				tableBody( 3, iYear ) = RealToStr( CashFlow( costCatOtherCapital ).yrAmount( iYear ), 2 );
				tableBody( 4, iYear ) = RealToStr( CashFlow( costCatTotCaptl ).yrAmount( iYear ), 2 );
			}
			WriteSubtitle( "Capital Cash Flow by Category (Without Escalation)" );
			WriteTable( tableBody, rowHead, columnHead, columnWidth );
			if ( sqlite ) {
				sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "Capital Cash Flow by Category (Without Escalation)" );
			}
			columnHead.deallocate();
			rowHead.deallocate();
			columnWidth.deallocate();
			tableBody.deallocate();
			//---- Operating Cash Flow by Category
			rowHead.allocate( lengthStudyYears );
			columnHead.allocate( 10 );
			columnWidth.allocate( 10 );
			columnWidth = 14; //array assignment - same for all columns
			tableBody.allocate( 10, lengthStudyYears );
			tableBody = "";
			columnHead( 1 ) = "Energy";
			columnHead( 2 ) = "Water";
			columnHead( 3 ) = "Maintenance";
			columnHead( 4 ) = "Repair";
			columnHead( 5 ) = "Operation";
			columnHead( 6 ) = "Replacement";
			columnHead( 7 ) = "MinorOverhaul";
			columnHead( 8 ) = "MajorOverhaul";
			columnHead( 9 ) = "OtherOperational";
			columnHead( 10 ) = "Total";

			for ( iYear = 1; iYear <= lengthStudyYears; ++iYear ) {
				rowHead( iYear ) = MonthNames( baseDateMonth ) + ' ' + IntToStr( baseDateYear + iYear - 1 );
				tableBody( 1, iYear ) = RealToStr( CashFlow( costCatEnergy ).yrAmount( iYear ), 2 );
				tableBody( 2, iYear ) = RealToStr( CashFlow( costCatWater ).yrAmount( iYear ), 2 );
				tableBody( 3, iYear ) = RealToStr( CashFlow( costCatMaintenance ).yrAmount( iYear ), 2 );
				tableBody( 4, iYear ) = RealToStr( CashFlow( costCatRepair ).yrAmount( iYear ), 2 );
				tableBody( 5, iYear ) = RealToStr( CashFlow( costCatOperation ).yrAmount( iYear ), 2 );
				tableBody( 6, iYear ) = RealToStr( CashFlow( costCatReplacement ).yrAmount( iYear ), 2 );
				tableBody( 7, iYear ) = RealToStr( CashFlow( costCatMinorOverhaul ).yrAmount( iYear ), 2 );
				tableBody( 8, iYear ) = RealToStr( CashFlow( costCatMajorOverhaul ).yrAmount( iYear ), 2 );
				tableBody( 9, iYear ) = RealToStr( CashFlow( costCatOtherOperational ).yrAmount( iYear ), 2 );
				tableBody( 10, iYear ) = RealToStr( CashFlow( costCatTotOper ).yrAmount( iYear ), 2 );
			}
			WriteSubtitle( "Operating Cash Flow by Category (Without Escalation)" );
			WriteTable( tableBody, rowHead, columnHead, columnWidth );
			if ( sqlite ) {
				sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "Operating Cash Flow by Category (Without Escalation)" );
			}
			columnHead.deallocate();
			rowHead.deallocate();
			columnWidth.deallocate();
			tableBody.deallocate();
			//---- DEBUG ONLY - Monthly Cash Flows
			// This table is not usually produced but was used as a debugging aid. The code
			// was kept for future debugging efforts related to cashflows but should generally
			// be commented out.
			//  ALLOCATE(rowHead(lengthStudyTotalMonths))
			//  ALLOCATE(columnHead(numCashFlow))
			//  ALLOCATE(columnWidth(numCashFlow))
			//  ALLOCATE(tableBody(lengthStudyTotalMonths,numCashFlow))
			//  tableBody = ''
			//  columnHead(1) = 'mnt'
			//  columnHead(2) = 'rpr'
			//  columnHead(3) = 'opr'
			//  columnHead(4) = 'repl'
			//  columnHead(5) = 'mOvhl'
			//  columnHead(6) = 'MOvhl'
			//  columnHead(7) = 'oOpr'
			//  columnHead(8) = 'cons'
			//  columnHead(9) = 'slvg'
			//  columnHead(10) = 'oCap'
			//  columnHead(11) = 'H20'
			//  columnHead(12) = 'ene'
			//  columnHead(13) = 'tEne'
			//  columnHead(14) = 'tOpr'
			//  columnHead(15) = 'tCap'
			//  columnHead(16) = 'Totl'
			//  DO jObj = countOfCostCat + 1, numCashFlow
			//    columnHead(jObj) = CashFlow(jObj)%name
			//  END DO
			//  DO kMonth = 1,lengthStudyTotalMonths
			//    rowHead(kMonth) = MonthNames(1 + MOD((kMonth + baseDateMonth - 2),12)) // ' ' // IntToStr(baseDateYear + INT((kMonth - 1) / 12))
			//  END DO
			//  DO kMonth = 1,lengthStudyTotalMonths
			//    DO jObj = 1,numCashFlow
			//      tableBody(kMonth,jObj) = TRIM(RealToStr(CashFlow(jObj)%mnAmount(kMonth),2))
			//    END DO
			//  END DO
			//  columnWidth = 14 !array assignment - same for all columns
			//  CALL WriteSubtitle('DEBUG ONLY - Monthly Cash Flows')
			//  CALL WriteTable(tableBody,rowHead,columnHead,columnWidth)
			//  CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
			//                                      'Life-Cycle Cost Report',&
			//                                      'Entire Facility',&
			//                                      'DEBUG ONLY - Monthly Cash Flows')
			//  DEALLOCATE(columnHead)
			//  DEALLOCATE(rowHead)
			//  DEALLOCATE(columnWidth)
			//  DEALLOCATE(tableBody)
			//---- Monthly Total Cash Flow
			rowHead.allocate( lengthStudyYears );
			columnHead.allocate( 12 );
			columnWidth.allocate( 12 );
			columnWidth = 14; //array assignment - same for all columns
			tableBody.allocate( 12, lengthStudyYears );
			tableBody = "";
			for ( kMonth = 1; kMonth <= 12; ++kMonth ) {
				columnHead( kMonth ) = MonthNames( kMonth );
			}
			for ( iYear = 1; iYear <= lengthStudyYears; ++iYear ) {
				rowHead( iYear ) = IntToStr( baseDateYear + iYear - 1 );
			}
			for ( iYear = 1; iYear <= lengthStudyYears; ++iYear ) {
				for ( kMonth = 1; kMonth <= 12; ++kMonth ) {
					tableBody( kMonth, iYear ) = RealToStr( CashFlow( costCatTotGrand ).mnAmount( ( iYear - 1 ) * 12 + kMonth ), 2 );
				}
			}
			WriteSubtitle( "Monthly Total Cash Flow (Without Escalation)" );
			WriteTable( tableBody, rowHead, columnHead, columnWidth );
			if ( sqlite ) {
				sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "Monthly Total Cash Flow (Without Escalation)" );
			}
			columnHead.deallocate();
			rowHead.deallocate();
			columnWidth.deallocate();
			tableBody.deallocate();
			//---- Present Value for Recurring, Nonrecurring and Energy Costs
			numRows = max( 1, numRecurringCosts + numNonrecurringCost + numResourcesUsed );
			rowHead.allocate( numRows + 1 );
			columnHead.allocate( 5 );
			columnWidth.allocate( 5 );
			columnWidth = 14; //array assignment - same for all columns
			tableBody.allocate( 5, numRows + 1 );
			tableBody = "";
			columnHead( 1 ) = "Category";
			columnHead( 2 ) = "Kind";
			columnHead( 3 ) = "Cost";
			columnHead( 4 ) = "Present Value";
			columnHead( 5 ) = "Present Value Factor";
			totalPV = 0.0;
			rowHead( numRows + 1 ) = "TOTAL";
			for ( jObj = 1; jObj <= ( numRecurringCosts + numNonrecurringCost + numResourcesUsed ); ++jObj ) {
				offset = countOfCostCat;
				rowHead( jObj ) = CashFlow( offset + jObj ).name;
				{ auto const SELECT_CASE_var( CashFlow( offset + jObj ).Category );
				if ( SELECT_CASE_var == costCatMaintenance ) {
					tableBody( 1, jObj ) = "Maintenance";
				} else if ( SELECT_CASE_var == costCatRepair ) {
					tableBody( 1, jObj ) = "Repair";
				} else if ( SELECT_CASE_var == costCatOperation ) {
					tableBody( 1, jObj ) = "Operation";
				} else if ( SELECT_CASE_var == costCatReplacement ) {
					tableBody( 1, jObj ) = "Replacement";
				} else if ( SELECT_CASE_var == costCatMinorOverhaul ) {
					tableBody( 1, jObj ) = "Minor Overhaul";
				} else if ( SELECT_CASE_var == costCatMajorOverhaul ) {
					tableBody( 1, jObj ) = "Major Overhaul";
				} else if ( SELECT_CASE_var == costCatOtherOperational ) {
					tableBody( 1, jObj ) = "Other Operational";
				} else if ( SELECT_CASE_var == costCatConstruction ) {
					tableBody( 1, jObj ) = "Construction";
				} else if ( SELECT_CASE_var == costCatSalvage ) {
					tableBody( 1, jObj ) = "Salvage";
				} else if ( SELECT_CASE_var == costCatOtherCapital ) {
					tableBody( 1, jObj ) = "Other Capital";
				} else if ( SELECT_CASE_var == costCatWater ) {
					tableBody( 1, jObj ) = "Water";
				} else if ( SELECT_CASE_var == costCatEnergy ) {
					tableBody( 1, jObj ) = "Energy";
				} else {
					tableBody( 1, jObj ) = "-";
				}}
				{ auto const SELECT_CASE_var( CashFlow( offset + jObj ).SourceKind );
				if ( SELECT_CASE_var == skNonrecurring ) {
					tableBody( 2, jObj ) = "Nonrecurring";
				} else if ( SELECT_CASE_var == skRecurring ) {
					tableBody( 2, jObj ) = "Recurring";
				} else if ( SELECT_CASE_var == skResource ) {
					tableBody( 2, jObj ) = "Energy Cost";
				} else {
					tableBody( 2, jObj ) = "-";
				}}
				tableBody( 3, jObj ) = RealToStr( CashFlow( offset + jObj ).orginalCost, 2 );
				tableBody( 4, jObj ) = RealToStr( CashFlow( offset + jObj ).presentValue, 2 );
				totalPV += CashFlow( offset + jObj ).presentValue;
				if ( CashFlow( offset + jObj ).orginalCost != 0.0 ) {
					tableBody( 5, jObj ) = RealToStr( CashFlow( offset + jObj ).presentValue / CashFlow( offset + jObj ).orginalCost, 4 );
				} else {
					tableBody( 5, jObj ) = "-";
				}
			}
			tableBody( 4, numRows + 1 ) = RealToStr( totalPV, 2 );
			WriteSubtitle( "Present Value for Recurring, Nonrecurring and Energy Costs (Before Tax)" );
			WriteTable( tableBody, rowHead, columnHead, columnWidth );
			if ( sqlite ) {
				sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "Present Value for Recurring, Nonrecurring and Energy Costs (Before Tax)" );
			}
			columnHead.deallocate();
			rowHead.deallocate();
			columnWidth.deallocate();
			tableBody.deallocate();
			//---- Present Value by Category
			rowHead.allocate( 16 );
			columnHead.allocate( 1 );
			columnWidth.allocate( 1 );
			columnWidth = 14; //array assignment - same for all columns
			tableBody.allocate( 1, 16 );
			tableBody = "";
			rowHead( 1 ) = "Construction";
			rowHead( 2 ) = "Salvage";
			rowHead( 3 ) = "Other Capital";
			rowHead( 4 ) = "Energy";
			rowHead( 5 ) = "Water";
			rowHead( 6 ) = "Maintenance";
			rowHead( 7 ) = "Repair";
			rowHead( 8 ) = "Operation";
			rowHead( 9 ) = "Replacement";
			rowHead( 10 ) = "Minor Overhaul";
			rowHead( 11 ) = "Major Overhaul";
			rowHead( 12 ) = "Other Operational";
			rowHead( 13 ) = "Total Energy";
			rowHead( 14 ) = "Total Operation";
			rowHead( 15 ) = "Total Capital";
			rowHead( 16 ) = "Grand Total";
			columnHead( 1 ) = "Present Value";

			tableBody( 1, 1 ) = RealToStr( CashFlow( costCatConstruction ).presentValue, 2 );
			tableBody( 1, 2 ) = RealToStr( CashFlow( costCatSalvage ).presentValue, 2 );
			tableBody( 1, 3 ) = RealToStr( CashFlow( costCatOtherCapital ).presentValue, 2 );
			tableBody( 1, 4 ) = RealToStr( CashFlow( costCatEnergy ).presentValue, 2 );
			tableBody( 1, 5 ) = RealToStr( CashFlow( costCatWater ).presentValue, 2 );
			tableBody( 1, 6 ) = RealToStr( CashFlow( costCatMaintenance ).presentValue, 2 );
			tableBody( 1, 7 ) = RealToStr( CashFlow( costCatRepair ).presentValue, 2 );
			tableBody( 1, 8 ) = RealToStr( CashFlow( costCatOperation ).presentValue, 2 );
			tableBody( 1, 9 ) = RealToStr( CashFlow( costCatReplacement ).presentValue, 2 );
			tableBody( 1, 10 ) = RealToStr( CashFlow( costCatMinorOverhaul ).presentValue, 2 );
			tableBody( 1, 11 ) = RealToStr( CashFlow( costCatMajorOverhaul ).presentValue, 2 );
			tableBody( 1, 12 ) = RealToStr( CashFlow( costCatOtherOperational ).presentValue, 2 );
			tableBody( 1, 13 ) = RealToStr( CashFlow( costCatTotEnergy ).presentValue, 2 );
			tableBody( 1, 14 ) = RealToStr( CashFlow( costCatTotOper ).presentValue, 2 );
			tableBody( 1, 15 ) = RealToStr( CashFlow( costCatTotCaptl ).presentValue, 2 );
			tableBody( 1, 16 ) = RealToStr( CashFlow( costCatTotGrand ).presentValue, 2 );

			WriteSubtitle( "Present Value by Category" );
			WriteTable( tableBody, rowHead, columnHead, columnWidth );
			if ( sqlite ) {
				sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "Present Value by Category" );
			}
			columnHead.deallocate();
			rowHead.deallocate();
			columnWidth.deallocate();
			tableBody.deallocate();
			//---- Present Value by Year
			rowHead.allocate( lengthStudyYears + 1 );
			columnHead.allocate( 2 );
			columnWidth.allocate( 2 );
			columnWidth = 14; //array assignment - same for all columns
			tableBody.allocate( 2, lengthStudyYears + 1 );
			tableBody = "";
			columnHead( 1 ) = "Total Cost";
			columnHead( 2 ) = "Present Value of Costs";

			totalPV = 0.0;
			for ( iYear = 1; iYear <= lengthStudyYears; ++iYear ) {
				rowHead( iYear ) = MonthNames( baseDateMonth ) + ' ' + IntToStr( baseDateYear + iYear - 1 );
				tableBody( 1, iYear ) = RealToStr( CashFlow( costCatTotGrand ).yrAmount( iYear ), 2 );
				tableBody( 2, iYear ) = RealToStr( CashFlow( costCatTotGrand ).yrPresVal( iYear ), 2 );
				totalPV += CashFlow( costCatTotGrand ).yrPresVal( iYear );
			}

			rowHead( lengthStudyYears + 1 ) = "TOTAL";
			tableBody( 2, lengthStudyYears + 1 ) = RealToStr( totalPV, 2 );

			WriteSubtitle( "Present Value by Year" );
			WriteTable( tableBody, rowHead, columnHead, columnWidth );
			if ( sqlite ) {
				sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "Present Value by Year" );
			}
			columnHead.deallocate();
			rowHead.deallocate();
			columnWidth.deallocate();
			tableBody.deallocate();
			//---- After Tax Estimate
			if ( taxRate != 0.0 ) {
				rowHead.allocate( lengthStudyYears + 1 );
				columnHead.allocate( 5 );
				columnWidth.allocate( 5 );
				columnWidth = 14; //array assignment - same for all columns
				tableBody.allocate( 5, lengthStudyYears + 1 );
				tableBody = "";
				columnHead( 1 ) = "Depreciated Capital";
				columnHead( 2 ) = "Taxable Income";
				columnHead( 3 ) = "Income Taxes";
				columnHead( 4 ) = "After Tax Cash Flow";
				columnHead( 5 ) = "After Tax Present Value";

				totalPV = 0.0;
				for ( iYear = 1; iYear <= lengthStudyYears; ++iYear ) {
					rowHead( iYear ) = MonthNames( baseDateMonth ) + ' ' + IntToStr( baseDateYear + iYear - 1 );
					tableBody( 1, iYear ) = RealToStr( DepreciatedCapital( iYear ), 2 );
					tableBody( 2, iYear ) = RealToStr( TaxableIncome( iYear ), 2 );
					tableBody( 3, iYear ) = RealToStr( Taxes( iYear ), 2 );
					tableBody( 4, iYear ) = RealToStr( AfterTaxCashFlow( iYear ), 2 );
					tableBody( 5, iYear ) = RealToStr( AfterTaxPresentValue( iYear ), 2 );
					totalPV += AfterTaxPresentValue( iYear );
				}

				rowHead( lengthStudyYears + 1 ) = "TOTAL";
				tableBody( 5, lengthStudyYears + 1 ) = RealToStr( totalPV, 2 );

				WriteSubtitle( "After Tax Estimate" );
				WriteTable( tableBody, rowHead, columnHead, columnWidth );
				if ( sqlite ) {
					sqlite->createSQLiteTabularDataRecords( tableBody, rowHead, columnHead, "Life-Cycle Cost Report", "Entire Facility", "After Tax Estimate" );
				}
				columnHead.deallocate();
				rowHead.deallocate();
				columnWidth.deallocate();
				tableBody.deallocate();
			}

		}
	}

} // EconomicLifeCycleCost

} // EnergyPlus
