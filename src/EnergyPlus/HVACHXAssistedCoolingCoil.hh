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

#ifndef HVACHXAssistedCoolingCoil_hh_INCLUDED
#define HVACHXAssistedCoolingCoil_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace HVACHXAssistedCoolingCoil {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS
	// Compressor operation
	extern int const On; // normal compressor operation
	extern int const Off; // signal DXCoil that compressor shouldn't run

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	extern int TotalNumHXAssistedCoils; // The total number of HXAssistedCoolingCoil compound objects
	extern Array1D< Real64 > HXAssistedCoilOutletTemp; // Outlet temperature from this compound object
	extern Array1D< Real64 > HXAssistedCoilOutletHumRat; // Outlet humidity ratio from this compound object
	// PUBLIC so others can access this information
	extern bool GetCoilsInputFlag; // Flag to allow input data to be retrieved from idf on first call to this subroutine
	extern Array1D_bool CheckEquipName;

	// Subroutine Specifications for the Module
	// Driver/Manager Routines

	// Get Input routines for module

	// Initialization routines for module

	// Calculation algorithms for the module
	// Update routine to update output node information
	//PRIVATE UpdateHXAssistedCoolingCoil
	// Not required.  All updates done by the individual components
	// (cooling coil and air-to-air heat exchanger)

	// Reporting routines for module
	//PRIVATE ReportHXAssistedCoolingCoil
	// No reporting variables for this compound component

	// Utility routines for module

	// Types

	struct HXAssistedCoilParameters
	{
		// Members
		std::string HXAssistedCoilType; // Type of HXAssistedCoolingCoil
		int HXAssistedCoilType_Num; // Numeric equivalent for hx assisted coil
		std::string Name; // Name of the HXAssistedCoolingCoil
		std::string CoolingCoilType; // Cooling coil type must be DetailedFlatCooling
		//  or Coil:DX:CoolingBypassFactorEmpirical
		int CoolingCoilType_Num; // Numeric Equivalent for cooling coil
		std::string CoolingCoilName; // Cooling coil name
		int CoolingCoilIndex;
		std::string HeatExchangerType; // Heat Exchanger type must be HeatExchanger:AirToAir:FlatPlate,
		// HeatExchanger:AirToAir:SensibleAndLatent or
		// HeatExchanger:Desiccant:BalancedFlow
		int HeatExchangerType_Num; // Numeric Equivalent for heat exchanger
		std::string HeatExchangerName; // Heat Exchanger name
		int HeatExchangerIndex; // Heat Exchanger index
		int HXAssistedCoilInletNodeNum; // Inlet node to HXAssistedCoolingCoil compound object
		int HXAssistedCoilOutletNodeNum; // Outlet node to HXAssistedCoolingCoil compound object
		int HXExhaustAirInletNodeNum; // Inlet node number for air-to-air heat exchanger
		Real64 MassFlowRate; // Mass flow rate through HXAssistedCoolingCoil compound object
		int MaxIterCounter; // used in warning messages
		int MaxIterIndex; // used in warning messages

		// Default Constructor
		HXAssistedCoilParameters() :
			HXAssistedCoilType_Num( 0 ),
			CoolingCoilType_Num( 0 ),
			CoolingCoilIndex( 0 ),
			HeatExchangerType_Num( 0 ),
			HeatExchangerIndex( 0 ),
			HXAssistedCoilInletNodeNum( 0 ),
			HXAssistedCoilOutletNodeNum( 0 ),
			HXExhaustAirInletNodeNum( 0 ),
			MassFlowRate( 0.0 ),
			MaxIterCounter( 0 ),
			MaxIterIndex( 0 )
		{}

	};

	// Object Data
	extern Array1D< HXAssistedCoilParameters > HXAssistedCoil;

	// Functions

	void
	SimHXAssistedCoolingCoil(
		std::string const & HXAssistedCoilName, // Name of HXAssistedCoolingCoil
		bool const FirstHVACIteration, // FirstHVACIteration flag
		int const CompOp, // compressor operation; 1=on, 0=off
		Real64 const PartLoadRatio, // Part load ratio of Coil:DX:CoolingBypassFactorEmpirical
		int & CompIndex,
		int const FanOpMode, // Allows the parent object to control fan operation
		Optional_bool_const HXUnitEnable = _, // flag to enable heat exchanger heat recovery
		Optional< Real64 const > OnOffAFR = _, // Ratio of compressor ON air mass flow rate to AVERAGE over time step
		Optional_bool_const EconomizerFlag = _, // OA sys or air loop economizer status
		Optional< Real64 > QTotOut = _ // the total cooling output of unit
	);

	// Get Input Section of the Module
	//******************************************************************************

	void
	GetHXAssistedCoolingCoilInput();

	// End of Get Input subroutines for this Module
	//******************************************************************************

	// Beginning Initialization Section of the Module
	//******************************************************************************

	void
	InitHXAssistedCoolingCoil( int const HXAssistedCoilNum ); // index for HXAssistedCoolingCoil

	// End Initialization Section of the Module
	//******************************************************************************

	void
	CalcHXAssistedCoolingCoil(
		int const HXAssistedCoilNum, // Index number for HXAssistedCoolingCoil
		bool const FirstHVACIteration, // FirstHVACIteration flag
		int const CompOp, // compressor operation; 1=on, 0=off
		Real64 const PartLoadRatio, // Cooling coil part load ratio
		bool const HXUnitOn, // Flag to enable heat exchanger
		int const FanOpMode, // Allows parent object to control fan operation
		Optional< Real64 const > OnOffAirFlow = _, // Ratio of compressor ON air mass flow to AVERAGE over time step
		Optional_bool_const EconomizerFlag = _ // OA (or airloop) econommizer status
	);

	//        End of Reporting subroutines for the HXAssistedCoil Module
	// *****************************************************************************

	void
	GetHXDXCoilIndex(
		std::string const & HXDXCoilName,
		int & HXDXCoilIndex,
		bool & ErrorsFound,
		Optional_string_const CurrentModuleObject = _
	);

	void
	CheckHXAssistedCoolingCoilSchedule(
		std::string const & CompType, // unused1208
		std::string const & CompName,
		Real64 & Value,
		int & CompIndex
	);

	Real64
	GetCoilCapacity(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetCoilGroupTypeNum(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound, // set to true if problem
		Optional_bool_const PrintWarning = _ // prints warning message if true
	);

	int
	GetCoilObjectTypeNum(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound, // set to true if problem
		Optional_bool_const PrintWarning = _ // prints warning message if true
	);

	int
	GetCoilInletNode(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetCoilWaterInletNode(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetCoilOutletNode(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	std::string
	GetHXDXCoilType(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	std::string
	GetHXDXCoilName(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetActualDXCoilIndex(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	std::string
	GetHXCoilType(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	void
	GetHXCoilTypeAndName(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound, // set to true if problem
		std::string & CoolingCoilType, // returned type of cooling coil
		std::string & CoolingCoilName // returned name of cooling coil
	);

	Real64
	GetCoilMaxWaterFlowRate(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	Real64
	GetHXCoilAirFlowRate(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	bool
	VerifyHeatExchangerParent(
		std::string const & HXType, // must match coil types in this module
		std::string const & HXName // must match coil names for the coil type
	);

	//        End of Utility subroutines for the HXAssistedCoil Module
	// *****************************************************************************

} // HVACHXAssistedCoolingCoil

} // EnergyPlus

#endif
