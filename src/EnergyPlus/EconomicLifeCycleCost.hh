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

		// Member Constructor
		RecurringCostsType(
			std::string const & name, // Name
			std::string const & lineItem, // Line Item
			int const category, // Category
			Real64 const cost, // Cost
			int const startOfCosts, // Start of Costs
			int const yearsFromStart, // Years from Start 0 - 100
			int const monthsFromStart, // Months from Start 0 - 11
			int const totalMonthsFromStart, // Total months (12 x years) + months
			int const repeatPeriodYears, // Repeat Period Years 1 - 100
			int const repeatPeriodMonths, // Repeat Period Months 0 - 11
			int const totalRepeatPeriodMonths, // Total months (12 x years) + months
			Real64 const annualEscalationRate // Annual escalation rate
		) :
			name( name ),
			lineItem( lineItem ),
			category( category ),
			cost( cost ),
			startOfCosts( startOfCosts ),
			yearsFromStart( yearsFromStart ),
			monthsFromStart( monthsFromStart ),
			totalMonthsFromStart( totalMonthsFromStart ),
			repeatPeriodYears( repeatPeriodYears ),
			repeatPeriodMonths( repeatPeriodMonths ),
			totalRepeatPeriodMonths( totalRepeatPeriodMonths ),
			annualEscalationRate( annualEscalationRate )
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

		// Member Constructor
		NonrecurringCostType(
			std::string const & name, // Name
			std::string const & lineItem, // Line Item
			int const category, // Category
			Real64 const cost, // Cost
			int const startOfCosts, // Start of Costs
			int const yearsFromStart, // Years from Start 0 - 100
			int const monthsFromStart, // Months from Start 0 - 11
			int const totalMonthsFromStart // Total months (12 x years) + months
		) :
			name( name ),
			lineItem( lineItem ),
			category( category ),
			cost( cost ),
			startOfCosts( startOfCosts ),
			yearsFromStart( yearsFromStart ),
			monthsFromStart( monthsFromStart ),
			totalMonthsFromStart( totalMonthsFromStart )
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

		// Member Constructor
		UsePriceEscalationType(
			std::string const & name, // Name
			int const resource, // resource like electricity or natural gas (uses definitions from DataGlobalConstants)
			int const escalationStartYear, // Escalation Start Year 1900-2100
			int const escalationStartMonth, // Escalation Start Month 1 to 12
			Array1< Real64 > const & Escalation // Escalation by year, first year is baseDateYear
		) :
			name( name ),
			resource( resource ),
			escalationStartYear( escalationStartYear ),
			escalationStartMonth( escalationStartMonth ),
			Escalation( Escalation )
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

		// Member Constructor
		UseAdjustmentType(
			std::string const & name, // Name
			int const resource, // resource like electricity or natural gas (uses definitions from DataGlobalConstants)
			Array1< Real64 > const & Adjustment // Adjustment by year, first year is baseDateYear
		) :
			name( name ),
			resource( resource ),
			Adjustment( Adjustment )
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

		// Member Constructor
		CashFlowType(
			std::string const & name, // Name - just for labeling output - use Category for aggregation
			int const SourceKind, // 1=recurring, 2=nonrecurring, 3=resource
			int const Resource, // resource like electricity or natural gas (uses definitions from DataGlobalConstants)
			int const Category, // uses "costCat" constants above
			Array1< Real64 > const & mnAmount, // cashflow dollar amount by month, first year is baseDateYear
			Array1< Real64 > const & yrAmount, // cashflow dollar amount by year, first year is baseDateYear
			int const pvKind, // kind of present value 1=energy, 2=non-energy,3=not computed but summed
			Real64 const presentValue, // total present value for cashflow
			Real64 const orginalCost, // original cost from recurring, non-recurring or energy cost
			Array1< Real64 > const & yrPresVal // present value by year, first year is baseDateYear
		) :
			name( name ),
			SourceKind( SourceKind ),
			Resource( Resource ),
			Category( Category ),
			mnAmount( mnAmount ),
			yrAmount( yrAmount ),
			pvKind( pvKind ),
			presentValue( presentValue ),
			orginalCost( orginalCost ),
			yrPresVal( yrPresVal )
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

} // EconomicLifeCycleCost

} // EnergyPlus

#endif
