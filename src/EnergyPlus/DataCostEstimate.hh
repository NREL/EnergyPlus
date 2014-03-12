#ifndef DataCostEstimate_hh_INCLUDED
#define DataCostEstimate_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>
#include <ObjexxFCL/Fstring.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace DataCostEstimate {

	// Using/Aliasing
	using DataGlobals::MaxNameLength;

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:
	// na

	// DERIVED TYPE DEFINITIONS

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// MODULE VARIABLE DECLARATIONS:

	// CurntBldg holds results for current bldg. cost estimate
	// RefrnceBldg holds user input for comparison.

	extern int NumLineItems; // number of cost estimate line items
	extern bool DoCostEstimate; // set to true if any cost estimating needed

	extern int numMonetaryUnit;
	extern int selectedMonetaryUnit;

	// Types

	struct CostLineItemStruct
	{
		// Members
		Fstring LineName; // object name (needed ?)
		Fstring LineType; // Case statement driver?
		Fstring ParentObjType; // parent reference to IDD object type
		Fstring ParentObjName; // parent instance in IDF
		Fstring ParentObjKey; // end use key for parent object
		int ParentObjIDinList;
		Real64 PerSquareMeter; // cost per square meter
		Real64 PerEach; // cost per each
		Real64 PerKiloWattCap; // cost per kW of nominal capacity
		Real64 PerKWCapPerCOP; // cost per kW of nominal capacity per COP
		Real64 PerCubicMeter; // cost per cubic meter
		Real64 PerCubMeterPerSec; // cost per cubic meter per second
		Real64 PerUAinWattperDelK; // cost per (UA) in Watt/deltaK
		//  REAL(r64)    :: AnnualMaintFract   = 0.0d0 ! cost for annual service and non energy consumables
		//  REAL(r64)    :: MinorOverhallFract = 0.0d0 ! cost for minor overhalls
		//  INTEGER :: MinorOverhallYears = 0   ! year interval for minor overhalls
		//  REAL(r64)    :: MajorOverhallFract = 0.0d0 ! cost for major overhall
		//  INTEGER :: MajorOverhallYears = 0   ! year interval for major overhalls
		//  INTEGER :: LifeYears          = 0.0 ! expected life in years
		//  REAL(r64)    :: ValueAtReplacement = 0.0d0 ! residual value at end of life
		int LineNumber; // number of line item in detail list
		Real64 Qty; // quantity in calculations (can be input)
		Fstring Units; // Reported units
		Real64 ValuePer; // Cost used in final calculation
		Real64 LineSubTotal; // line item total  Qty * ValuePer

		// Default Constructor
		CostLineItemStruct() :
			LineName( MaxNameLength ),
			LineType( MaxNameLength ),
			ParentObjType( MaxNameLength ),
			ParentObjName( MaxNameLength ),
			ParentObjKey( MaxNameLength ),
			ParentObjIDinList( 1 ),
			PerSquareMeter( 0.0 ),
			PerEach( 0.0 ),
			PerKiloWattCap( 0.0 ),
			PerKWCapPerCOP( 0.0 ),
			PerCubicMeter( 0.0 ),
			PerCubMeterPerSec( 0.0 ),
			PerUAinWattperDelK( 0.0 ),
			LineNumber( -1 ),
			Qty( 0.0 ),
			Units( MaxNameLength ),
			ValuePer( 0.0 ),
			LineSubTotal( 0.0 )
		{}

		// Member Constructor
		CostLineItemStruct(
			Fstring const & LineName, // object name (needed ?)
			Fstring const & LineType, // Case statement driver?
			Fstring const & ParentObjType, // parent reference to IDD object type
			Fstring const & ParentObjName, // parent instance in IDF
			Fstring const & ParentObjKey, // end use key for parent object
			int const ParentObjIDinList,
			Real64 const PerSquareMeter, // cost per square meter
			Real64 const PerEach, // cost per each
			Real64 const PerKiloWattCap, // cost per kW of nominal capacity
			Real64 const PerKWCapPerCOP, // cost per kW of nominal capacity per COP
			Real64 const PerCubicMeter, // cost per cubic meter
			Real64 const PerCubMeterPerSec, // cost per cubic meter per second
			Real64 const PerUAinWattperDelK, // cost per (UA) in Watt/deltaK
			int const LineNumber, // number of line item in detail list
			Real64 const Qty, // quantity in calculations (can be input)
			Fstring const & Units, // Reported units
			Real64 const ValuePer, // Cost used in final calculation
			Real64 const LineSubTotal // line item total  Qty * ValuePer
		) :
			LineName( MaxNameLength, LineName ),
			LineType( MaxNameLength, LineType ),
			ParentObjType( MaxNameLength, ParentObjType ),
			ParentObjName( MaxNameLength, ParentObjName ),
			ParentObjKey( MaxNameLength, ParentObjKey ),
			ParentObjIDinList( ParentObjIDinList ),
			PerSquareMeter( PerSquareMeter ),
			PerEach( PerEach ),
			PerKiloWattCap( PerKiloWattCap ),
			PerKWCapPerCOP( PerKWCapPerCOP ),
			PerCubicMeter( PerCubicMeter ),
			PerCubMeterPerSec( PerCubMeterPerSec ),
			PerUAinWattperDelK( PerUAinWattperDelK ),
			LineNumber( LineNumber ),
			Qty( Qty ),
			Units( MaxNameLength, Units ),
			ValuePer( ValuePer ),
			LineSubTotal( LineSubTotal )
		{}

	};

	struct CostAdjustmentStruct
	{
		// Members
		Real64 LineItemTot; // = 0.0 ! holds total from line item cost calculations
		Real64 MiscCostperSqMeter; // = 0.0 ! holds user-defined constant cost model
		Real64 DesignFeeFrac; // = 0.0 ! holds user-defined fraction for design fees
		Real64 ContractorFeeFrac; // = 0.0 ! holds user-defined fraction for contractor fees
		Real64 ContingencyFrac; // = 0.0 ! holds user-defined fraction for contingencies
		Real64 BondCostFrac; // = 0.0 ! holds user-defined fraction for bonding costs
		Real64 CommissioningFrac; // = 0.0 ! holds user-defined fraction for commissioning costs
		Real64 RegionalModifier; // = 1.0 ! holds user-defined multiplier to account for regional diffs
		Real64 GrandTotal; // = 0.0 ! the Grand Total of all line items plus all other costs

		// Default Constructor
		CostAdjustmentStruct()
		{}

		// Member Constructor
		CostAdjustmentStruct(
			Real64 const LineItemTot, // = 0.0 ! holds total from line item cost calculations
			Real64 const MiscCostperSqMeter, // = 0.0 ! holds user-defined constant cost model
			Real64 const DesignFeeFrac, // = 0.0 ! holds user-defined fraction for design fees
			Real64 const ContractorFeeFrac, // = 0.0 ! holds user-defined fraction for contractor fees
			Real64 const ContingencyFrac, // = 0.0 ! holds user-defined fraction for contingencies
			Real64 const BondCostFrac, // = 0.0 ! holds user-defined fraction for bonding costs
			Real64 const CommissioningFrac, // = 0.0 ! holds user-defined fraction for commissioning costs
			Real64 const RegionalModifier, // = 1.0 ! holds user-defined multiplier to account for regional diffs
			Real64 const GrandTotal // = 0.0 ! the Grand Total of all line items plus all other costs
		) :
			LineItemTot( LineItemTot ),
			MiscCostperSqMeter( MiscCostperSqMeter ),
			DesignFeeFrac( DesignFeeFrac ),
			ContractorFeeFrac( ContractorFeeFrac ),
			ContingencyFrac( ContingencyFrac ),
			BondCostFrac( BondCostFrac ),
			CommissioningFrac( CommissioningFrac ),
			RegionalModifier( RegionalModifier ),
			GrandTotal( GrandTotal )
		{}

	};

	struct monetaryUnitType
	{
		// Members
		Fstring code; // ISO code for currency such as USD or EUR
		Fstring txt; // text representation of the currency
		Fstring html; // representation for HTML file - contains unicode references

		// Default Constructor
		monetaryUnitType() :
			code( MaxNameLength ),
			txt( MaxNameLength ),
			html( MaxNameLength )
		{}

		// Member Constructor
		monetaryUnitType(
			Fstring const & code, // ISO code for currency such as USD or EUR
			Fstring const & txt, // text representation of the currency
			Fstring const & html // representation for HTML file - contains unicode references
		) :
			code( MaxNameLength, code ),
			txt( MaxNameLength, txt ),
			html( MaxNameLength, html )
		{}

	};

	// Object Data
	extern FArray1D< CostLineItemStruct > CostLineItem;
	extern CostAdjustmentStruct CurntBldg; // holds total from line item cost calculations | holds user-defined constant cost model | holds user-defined fraction for design fees | holds user-defined fraction for contractor fees | holds user-defined fraction for contingencies | holds user-defined fraction for bonding costs | holds user-defined fraction for commissioning costs | holds user-defined multiplier to account for regional diffs | the Grand Total of all line items plus all other costs
	extern CostAdjustmentStruct RefrncBldg; // holds total from line item cost calculations | holds user-defined constant cost model | holds user-defined fraction for design fees | holds user-defined fraction for contractor fees | holds user-defined fraction for contingencies | holds user-defined fraction for bonding costs | holds user-defined fraction for commissioning costs | holds user-defined multiplier to account for regional diffs | the Grand Total of all line items plus all other costs
	extern FArray1D< monetaryUnitType > monetaryUnit;

} // DataCostEstimate

} // EnergyPlus

#endif
