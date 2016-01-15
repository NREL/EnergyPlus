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

#ifndef HeatPumpWaterToWaterSimple_hh_INCLUDED
#define HeatPumpWaterToWaterSimple_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace HeatPumpWaterToWaterSimple {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS
	extern std::string const HPEqFitHeating;
	extern std::string const HPEqFitHeatingUC;
	extern std::string const HPEqFitCooling;
	extern std::string const HPEqFitCoolingUC;

	// DERIVED TYPE DEFINITIONS

	// Type Description of Heat Pump

	// Output Variables Type definition

	// MODULE VARIABLE DECLARATIONS:
	extern int NumGSHPs; // Number of GSHPs specified in input

	// SUBROUTINE SPECIFICATIONS FOR MODULE

	// Driver/Manager Routines

	// Get Input routines for module

	// Initialization routines for module

	// Computational routines

	// Update routine to check convergence and update nodes

	// Types

	struct GshpSpecs
	{
		// Members
		std::string Name; // user identifier
		std::string WatertoWaterHPType; // Type of WatertoAirHP ie. Heating or Cooling
		int WWHPPlantTypeOfNum; // equipment type num
		bool Available; // need an array of logicals--load identifiers of available equipment
		bool ON; // simulate the machine at it's operating part load ratio
		bool IsOn; // flag that the heat pump is ON during current time step
		bool MustRun; // flag that the heat pump is MUST RUN during current time step
		Real64 SourceSideDesignMassFlow; // Design flow rate (kg/s)
		Real64 LoadSideDesignMassFlow; // Design flow rate (kg/s)
		Real64 RatedLoadVolFlowCool; // Rated Cooling Load Side Volumetric Flow Rate [m3/s]
		Real64 RatedSourceVolFlowCool; // Rated Cooling Source Side Volumetric Flow Rate [m3/s]
		Real64 RatedCapCool; // Rated Cooling Capacity [W]
		Real64 RatedPowerCool; // Rated Cooling Power Consumption[W]
		Real64 CoolCap1; // 1st coefficient of the Cooling capacity performance curve
		Real64 CoolCap2; // 2nd coefficient of the Cooling capacity performance curve
		Real64 CoolCap3; // 3rd coefficient of the Cooling capacity performance curve
		Real64 CoolCap4; // 4th coefficient of the Cooling capacity performance curve
		Real64 CoolCap5; // 5th coefficient of the Cooling capacity performance curve
		Real64 CoolPower1; // 1st coefficient of the Cooling power consumption curve
		Real64 CoolPower2; // 2nd coefficient of the Cooling power consumption curve
		Real64 CoolPower3; // 3rd coefficient of the Cooling power consumption curve
		Real64 CoolPower4; // 4th coefficient of the Cooling power consumption curve
		Real64 CoolPower5; // 5th coefficient of the Cooling power consumption curve
		int CoolCapNegativeCounter; // Counter for number of times cooling capacity curve is <= 0.0
		int CoolCapNegativeIndex; // Index for recurring warning message regarding cooling capacity curve is <= 0.0
		int CoolPowerNegativeCounter; // Counter for number of times cooling power curve is <= 0.0
		int CoolPowerNegativeIndex; // Index for recurring warning message regarding cooling power curve is <= 0.0
		Real64 RatedLoadVolFlowHeat; // Rated Heating Load Side Volumetric Flow Rate [m3/s]
		Real64 RatedSourceVolFlowHeat; // Rated Heating Source Side Volumetric Flow Rate [m3/s]
		Real64 RatedCapHeat; // Rated Heating Capacity [W]
		Real64 RatedPowerHeat; // Rated Heating Compressor Power[W]
		Real64 HeatCap1; // 1st coefficient of the Heating capacity performance curve
		Real64 HeatCap2; // 2nd coefficient of the Heating capacity performance curve
		Real64 HeatCap3; // 3rd coefficient of the Heating capacity performance curve
		Real64 HeatCap4; // 4th coefficient of the Heating capacity performance curve
		Real64 HeatCap5; // 5th coefficient of the Heating capacity performance curve
		Real64 HeatPower1; // 1st coefficient of the Heating power consumption curve
		Real64 HeatPower2; // 2nd coefficient of the Heating power consumption curve
		Real64 HeatPower3; // 3rd coefficient of the Heating power consumption curve
		Real64 HeatPower4; // 4th coefficient of the Heating power consumption curve
		Real64 HeatPower5; // 5th coefficient of the Heating power consumption curve
		int LoadSideInletNodeNum; // Load Side Inlet Node
		int LoadSideOutletNodeNum; // Load Side Outlet Node
		int SourceSideInletNodeNum; // Source Side Inlet Node
		int SourceSideOutletNodeNum; // Source Side Outlet Node
		int HeatCapNegativeCounter; // Counter for number of times heating capacity curve is <= 0.0
		int HeatCapNegativeIndex; // Index for recurring warning message regarding heating capacity curve is <= 0.0
		int HeatPowerNegativeCounter; // Counter for number of times heating power curve is <= 0.0
		int HeatPowerNegativeIndex; // Index for recurring warning message regarding heating power curve is <= 0.0
		//loop topology variables
		int SourceLoopNum; // source side plant loop index number
		int SourceLoopSideNum; // source side plant loop side index
		int SourceBranchNum; // source side plant loop branch index
		int SourceCompNum; // source side plant loop component index
		int LoadLoopNum; // load side plant loop index number
		int LoadLoopSideNum; // load side plant loop side index
		int LoadBranchNum; // load side plant loop branch index
		int LoadCompNum; // load side plant loop component index
		int CondMassFlowIndex; // index for criteria in PullCompInterconnectTrigger

		// Default Constructor
		GshpSpecs() :
			WWHPPlantTypeOfNum( 0 ),
			Available( false ),
			ON( false ),
			IsOn( false ),
			MustRun( false ),
			SourceSideDesignMassFlow( 0.0 ),
			LoadSideDesignMassFlow( 0.0 ),
			RatedLoadVolFlowCool( 0.0 ),
			RatedSourceVolFlowCool( 0.0 ),
			RatedCapCool( 0.0 ),
			RatedPowerCool( 0.0 ),
			CoolCap1( 0.0 ),
			CoolCap2( 0.0 ),
			CoolCap3( 0.0 ),
			CoolCap4( 0.0 ),
			CoolCap5( 0.0 ),
			CoolPower1( 0.0 ),
			CoolPower2( 0.0 ),
			CoolPower3( 0.0 ),
			CoolPower4( 0.0 ),
			CoolPower5( 0.0 ),
			CoolCapNegativeCounter( 0 ),
			CoolCapNegativeIndex( 0 ),
			CoolPowerNegativeCounter( 0 ),
			CoolPowerNegativeIndex( 0 ),
			RatedLoadVolFlowHeat( 0.0 ),
			RatedSourceVolFlowHeat( 0.0 ),
			RatedCapHeat( 0.0 ),
			RatedPowerHeat( 0.0 ),
			HeatCap1( 0.0 ),
			HeatCap2( 0.0 ),
			HeatCap3( 0.0 ),
			HeatCap4( 0.0 ),
			HeatCap5( 0.0 ),
			HeatPower1( 0.0 ),
			HeatPower2( 0.0 ),
			HeatPower3( 0.0 ),
			HeatPower4( 0.0 ),
			HeatPower5( 0.0 ),
			LoadSideInletNodeNum( 0 ),
			LoadSideOutletNodeNum( 0 ),
			SourceSideInletNodeNum( 0 ),
			SourceSideOutletNodeNum( 0 ),
			HeatCapNegativeCounter( 0 ),
			HeatCapNegativeIndex( 0 ),
			HeatPowerNegativeCounter( 0 ),
			HeatPowerNegativeIndex( 0 ),
			SourceLoopNum( 0 ),
			SourceLoopSideNum( 0 ),
			SourceBranchNum( 0 ),
			SourceCompNum( 0 ),
			LoadLoopNum( 0 ),
			LoadLoopSideNum( 0 ),
			LoadBranchNum( 0 ),
			LoadCompNum( 0 ),
			CondMassFlowIndex( 0 )
		{}

	};

	struct ReportVars
	{
		// Members
		Real64 Power; // Power Consumption [W]
		Real64 Energy; // Energy Consumption [J]
		Real64 QLoad; // Load Side Heat Transfer Rate [W]
		Real64 QLoadEnergy; // Load Side Heat Transfer [J]
		Real64 QSource; // Source Side Heat Transfer Rate [W]
		Real64 QSourceEnergy; // Source Side Heat Transfer [J]
		Real64 LoadSideMassFlowRate; // Load side volumetric flow rate m3/s
		Real64 LoadSideInletTemp; // Load Side outlet temperature °C
		Real64 LoadSideOutletTemp; // Load Side outlet temperature °C
		Real64 SourceSideMassFlowRate; // Source side volumetric flow rate m3/s
		Real64 SourceSideInletTemp; // Source Side outlet temperature °C
		Real64 SourceSideOutletTemp; // Source Side outlet temperature °C

		// Default Constructor
		ReportVars() :
			Power( 0.0 ),
			Energy( 0.0 ),
			QLoad( 0.0 ),
			QLoadEnergy( 0.0 ),
			QSource( 0.0 ),
			QSourceEnergy( 0.0 ),
			LoadSideMassFlowRate( 0.0 ),
			LoadSideInletTemp( 0.0 ),
			LoadSideOutletTemp( 0.0 ),
			SourceSideMassFlowRate( 0.0 ),
			SourceSideInletTemp( 0.0 ),
			SourceSideOutletTemp( 0.0 )
		{}

	};

	// Object Data
	extern Array1D< GshpSpecs > GSHP;
	extern Array1D< ReportVars > GSHPReport;

	// Functions
	void
	clear_state();

	void
	SimHPWatertoWaterSimple(
		std::string const & GSHPType, // Type of GSHP
		int const GSHPTypeNum, // Type of GSHP in Plant equipment
		std::string const & GSHPName, // User Specified Name of GSHP
		int & GSHPNum, // Index of Equipment
		bool const FirstHVACIteration,
		bool & InitLoopEquip, // If not zero, calculate the max load for operating conditions
		Real64 const MyLoad, // Loop demand component will meet
		Real64 & MaxCap, // Maximum operating capacity of GSHP [W]
		Real64 & MinCap, // Minimum operating capacity of GSHP [W]
		Real64 & OptCap, // Optimal operating capacity of GSHP [W]
		int const LoopNum // The calling loop number
	);

	void
	GetWatertoWaterHPInput();

	void
	InitWatertoWaterHP(
		int const GSHPTypeNum, // Type of GSHP
		std::string const & GSHPName, // User Specified Name of GSHP
		int const GSHPNum, // GSHP Number
		bool const FirstHVACIteration,
		Real64 const MyLoad // Demand Load
	);

	void
	CalcWatertoWaterHPCooling(
		int const GSHPNum, // GSHP Number
		Real64 const MyLoad // Operating Load
	);

	void
	CalcWatertoWaterHPHeating(
		int const GSHPNum, // GSHP Number
		Real64 const MyLoad // Operating Load
	);

	void
	UpdateGSHPRecords( int const GSHPNum ); // GSHP number

} // HeatPumpWaterToWaterSimple

} // EnergyPlus

#endif
