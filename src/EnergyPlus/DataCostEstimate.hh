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

#ifndef DataCostEstimate_hh_INCLUDED
#define DataCostEstimate_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace DataCostEstimate {

	// Using/Aliasing

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
		std::string LineName; // object name (needed ?)
		std::string LineType; // Case statement driver?
		std::string ParentObjType; // parent reference to IDD object type
		std::string ParentObjName; // parent instance in IDF
		std::string ParentObjKey; // end use key for parent object
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
		std::string Units; // Reported units
		Real64 ValuePer; // Cost used in final calculation
		Real64 LineSubTotal; // line item total  Qty * ValuePer

		// Default Constructor
		CostLineItemStruct() :
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
			ValuePer( 0.0 ),
			LineSubTotal( 0.0 )
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
		std::string code; // ISO code for currency such as USD or EUR
		std::string txt; // text representation of the currency
		std::string html; // representation for HTML file - contains unicode references

		// Default Constructor
		monetaryUnitType()
		{}

	};

	// Object Data
	extern Array1D< CostLineItemStruct > CostLineItem;
	extern CostAdjustmentStruct CurntBldg; // holds total from line item cost calculations | holds user-defined constant cost model | holds user-defined fraction for design fees | holds user-defined fraction for contractor fees | holds user-defined fraction for contingencies | holds user-defined fraction for bonding costs | holds user-defined fraction for commissioning costs | holds user-defined multiplier to account for regional diffs | the Grand Total of all line items plus all other costs
	extern CostAdjustmentStruct RefrncBldg; // holds total from line item cost calculations | holds user-defined constant cost model | holds user-defined fraction for design fees | holds user-defined fraction for contractor fees | holds user-defined fraction for contingencies | holds user-defined fraction for bonding costs | holds user-defined fraction for commissioning costs | holds user-defined multiplier to account for regional diffs | the Grand Total of all line items plus all other costs
	extern Array1D< monetaryUnitType > monetaryUnit;

} // DataCostEstimate

} // EnergyPlus

#endif
