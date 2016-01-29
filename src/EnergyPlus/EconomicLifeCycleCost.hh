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

#ifndef EconomicLifeCycleCost_hh_INCLUDED
#define EconomicLifeCycleCost_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array2D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace EconomicLifeCycleCost {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:
	extern int const disConvBeginOfYear;
	extern int const disConvMidYear;
	extern int const disConvEndOfYear;

	extern int const inflAppConstantDollar;
	extern int const inflAppCurrentDollar;

	// ModifiedAcceleratedCostRecoverySystem or Straight Line
	extern int const depMethMACRS3;
	extern int const depMethMACRS5;
	extern int const depMethMACRS7;
	extern int const depMethMACRS10;
	extern int const depMethMACRS15;
	extern int const depMethMACRS20;
	extern int const depMethStraight27;
	extern int const depMethStraight31;
	extern int const depMethStraight39;
	extern int const depMethStraight40;
	extern int const depMethNone;

	extern int const costCatMaintenance;
	extern int const costCatRepair;
	extern int const costCatOperation;
	extern int const costCatReplacement;
	extern int const costCatMinorOverhaul;
	extern int const costCatMajorOverhaul;
	extern int const costCatOtherOperational;
	extern int const costCatConstruction;
	extern int const costCatSalvage;
	extern int const costCatOtherCapital;
	extern int const costCatWater;
	extern int const costCatEnergy;
	extern int const costCatTotEnergy;
	extern int const costCatTotOper;
	extern int const costCatTotCaptl;
	extern int const costCatTotGrand;

	extern int const countOfCostCat; // count of the number of cost categories

	// The NIST supplement includes UPV* factors for
	//   Electricity
	//   Natural gas
	//   Distillate oil
	//   Liquified petroleum gas
	//   Residual oil
	//   Coal

	extern int const startServicePeriod;
	extern int const startBasePeriod;

	// DERIVED TYPE DEFINITIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:

	// related to LifeCycleCost:Parameters
	extern bool LCCparamPresent; // If a LifeCycleCost:Parameters object is present
	extern std::string LCCname; // Name
	extern int discountConvension; // Discounting Convention
	extern int inflationApproach; // Inflation Approach
	extern Real64 realDiscountRate; // Real Discount Rate
	extern Real64 nominalDiscountRate; // Nominal Discount Rate
	extern Real64 inflation; // Inflation
	extern int baseDateMonth; // Base Date Month (1=Jan, 12=Dec)
	extern int baseDateYear; // Base Date Year  1900-2100
	extern int serviceDateMonth; // Service Date Month (1=Jan, 12=Dec)
	extern int serviceDateYear; // Service Date Year 1900-2100
	extern int lengthStudyYears; // Length of Study Period in Years
	extern int lengthStudyTotalMonths; // Length of Study expressed in months (years x 12)
	extern Real64 taxRate; // Tax rate
	extern int depreciationMethod; // Depreciation Method
	// derived
	extern int lastDateMonth; // Last Date Month (the month before the base date month)
	extern int lastDateYear; // Last Date Year (base date year + length of study period in years)

	extern int numRecurringCosts;

	extern int numNonrecurringCost;

	extern int numUsePriceEscalation;

	extern int numUseAdjustment;

	extern int numCashFlow;
	extern int const skRecurring;
	extern int const skNonrecurring;
	extern int const skResource;
	extern int const skSum;
	extern int const pvkEnergy;
	extern int const pvkNonEnergy;
	extern int const pvkNotComputed;
	extern int numResourcesUsed;

	//present value factors
	extern Array1D< Real64 > SPV;
	extern Array2D< Real64 > energySPV; // yearly equivalent to FEMP UPV* values

	//arrays related to computing after tax cashflow and present value
	extern Array1D< Real64 > DepreciatedCapital;
	extern Array1D< Real64 > TaxableIncome;
	extern Array1D< Real64 > Taxes;
	extern Array1D< Real64 > AfterTaxCashFlow;
	extern Array1D< Real64 > AfterTaxPresentValue;

	extern Array1D_string const MonthNames;

	// SUBROUTINE SPECIFICATIONS FOR MODULE <module_name>:

	// Types

	struct RecurringCostsType
	{
		// Members
		std::string name; // Name
		std::string lineItem; // Line Item
		int category; // Category
		Real64 cost; // Cost
		int startOfCosts; // Start of Costs
		int yearsFromStart; // Years from Start 0 - 100
		int monthsFromStart; // Months from Start 0 - 11
		int totalMonthsFromStart; // Total months (12 x years) + months
		int repeatPeriodYears; // Repeat Period Years 1 - 100
		int repeatPeriodMonths; // Repeat Period Months 0 - 11
		int totalRepeatPeriodMonths; // Total months (12 x years) + months
		Real64 annualEscalationRate; // Annual escalation rate

		// Default Constructor
		RecurringCostsType() :
			category( costCatMaintenance ),
			startOfCosts( startServicePeriod ),
			yearsFromStart( 0 ),
			monthsFromStart( 0 ),
			totalMonthsFromStart( 0 ),
			repeatPeriodYears( 0 ),
			repeatPeriodMonths( 0 ),
			totalRepeatPeriodMonths( 0 ),
			annualEscalationRate( 0.0 )
		{}

	};

	struct NonrecurringCostType
	{
		// Members
		std::string name; // Name
		std::string lineItem; // Line Item
		int category; // Category
		Real64 cost; // Cost
		int startOfCosts; // Start of Costs
		int yearsFromStart; // Years from Start 0 - 100
		int monthsFromStart; // Months from Start 0 - 11
		int totalMonthsFromStart; // Total months (12 x years) + months

		// Default Constructor
		NonrecurringCostType() :
			category( costCatConstruction ),
			startOfCosts( startServicePeriod ),
			yearsFromStart( 0 ),
			monthsFromStart( 0 ),
			totalMonthsFromStart( 0 )
		{}

	};

	struct UsePriceEscalationType
	{
		// Members
		std::string name; // Name
		int resource; // resource like electricity or natural gas (uses definitions from DataGlobalConstants)
		int escalationStartYear; // Escalation Start Year 1900-2100
		int escalationStartMonth; // Escalation Start Month 1 to 12
		Array1D< Real64 > Escalation; // Escalation by year, first year is baseDateYear
		// last year is baseDateYear + lengthStudyYears - 1

		// Default Constructor
		UsePriceEscalationType() :
			escalationStartYear( 0 ),
			escalationStartMonth( 0 )
		{}

	};

	struct UseAdjustmentType
	{
		// Members
		std::string name; // Name
		int resource; // resource like electricity or natural gas (uses definitions from DataGlobalConstants)
		Array1D< Real64 > Adjustment; // Adjustment by year, first year is baseDateYear
		// last year is baseDateYear + lengthStudyYears - 1

		// Default Constructor
		UseAdjustmentType()
		{}

	};

	struct CashFlowType
	{
		// Members
		std::string name; // Name - just for labeling output - use Category for aggregation
		int SourceKind; // 1=recurring, 2=nonrecurring, 3=resource
		int Resource; // resource like electricity or natural gas (uses definitions from DataGlobalConstants)
		int Category; // uses "costCat" constants above
		Array1D< Real64 > mnAmount; // cashflow dollar amount by month, first year is baseDateYear
		// last year is baseDateYear + lengthStudyYears - 1
		Array1D< Real64 > yrAmount; // cashflow dollar amount by year, first year is baseDateYear
		int pvKind; // kind of present value 1=energy, 2=non-energy,3=not computed but summed
		Real64 presentValue; // total present value for cashflow
		Real64 orginalCost; // original cost from recurring, non-recurring or energy cost
		Array1D< Real64 > yrPresVal; // present value by year, first year is baseDateYear

		// Default Constructor
		CashFlowType() :
			pvKind( 0 )
		{}

	};

	// Object Data
	extern Array1D< RecurringCostsType > RecurringCosts;
	extern Array1D< NonrecurringCostType > NonrecurringCost;
	extern Array1D< UsePriceEscalationType > UsePriceEscalation;
	extern Array1D< UseAdjustmentType > UseAdjustment;
	extern Array1D< CashFlowType > CashFlow;

	// Functions

	void
	GetInputForLifeCycleCost();

	void
	ComputeLifeCycleCostAndReport();

	//======================================================================================================================
	//======================================================================================================================

	//    GET INPUT ROUTINES

	//======================================================================================================================
	//======================================================================================================================

	void
	GetInputLifeCycleCostParameters();

	void
	GetInputLifeCycleCostRecurringCosts();

	void
	GetInputLifeCycleCostNonrecurringCost();

	void
	GetInputLifeCycleCostUsePriceEscalation();

	void
	GetInputLifeCycleCostUseAdjustment();

	int
	MonthToMonthNumber(
		std::string const & inMonthString,
		int const inDefaultMonth
	);

	//======================================================================================================================
	//======================================================================================================================

	//    COMPUTATION ROUTINES

	//======================================================================================================================
	//======================================================================================================================

	void
	ExpressAsCashFlows();

	void
	ComputePresentValue();

	void
	ComputeTaxAndDepreciation();

	//======================================================================================================================
	//======================================================================================================================

	//    OUTPUT ROUTINES

	//======================================================================================================================
	//======================================================================================================================

	void
	WriteTabularLifeCycleCostReport();

} // EconomicLifeCycleCost

} // EnergyPlus

#endif
