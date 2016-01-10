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

#ifndef CTElectricGenerator_hh_INCLUDED
#define CTElectricGenerator_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobalConstants.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace CTElectricGenerator {

	// Using/Aliasing
	using DataGlobalConstants::iGeneratorCombTurbine;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// na

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	extern int NumCTGenerators; // number of CT Generators specified in input
	extern bool GetCTInput; // then TRUE, calls subroutine to read input file.

	extern Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE PrimaryPlantLoops

	// Types

	struct CTGeneratorSpecs
	{
		// Members
		std::string Name; // user identifier
		std::string TypeOf; // Type of Generator
		int CompType_Num;
		std::string FuelType; // Type of Fuel - DIESEL, GASOLINE, GAS
		Real64 RatedPowerOutput; // W - design nominal capacity of Generator
		int ElectricCircuitNode; // Electric Circuit Node
		Real64 MinPartLoadRat; // (CT MIN) min allowed operating frac full load
		Real64 MaxPartLoadRat; // (CT MAX) max allowed operating frac full load
		Real64 OptPartLoadRat; // (CT BEST) optimal operating frac full load
		Real64 FuelEnergyUseRate; // (EFUEL) rate of Fuel Energy Required to run COMBUSTION turbine (W)
		Real64 FuelEnergy; // Amount of Fuel Energy Required to run COMBUSTION turbine (J)
		int PLBasedFuelInputCurve; // (FUL1GC) Curve Index for Part Load Ratio Based Fuel Input
		// Coefficients Poly Fit
		int TempBasedFuelInputCurve; // (FUL2GC) Curve Index for Ambient Temperature Based Fuel Input
		// Coeff Poly Fit
		Real64 ExhaustFlow; // (FEX) Exhaust Gas Flow Rate cubic meters per second???
		int ExhaustFlowCurve; // (FEXGC) Curve Index for Exhaust Gas Flow Rate Input Coef Poly Fit
		Real64 ExhaustTemp; // (TEX) Exhaust Gas Temperature in C
		int PLBasedExhaustTempCurve; // (TEX1GC) Curve Index for Part Load Ratio Based Exhaust Temp Input
		// Coeffs Poly Fit
		int TempBasedExhaustTempCurve; // (TEX2GC) Curve Index for Ambient Temperature Based Exhaust Gas Temp to
		// Fuel Energy Input Coeffs Poly Fit
		Real64 QLubeOilRecovered; // (ELUBE) Recovered Lube Oil Energy (W)
		Real64 QExhaustRecovered; // (EEX) Recovered Exhaust heat  (W)
		Real64 QTotalHeatRecovered; // total heat recovered (W)
		Real64 LubeOilEnergyRec; // Recovered Lube Oil Energy (J)
		Real64 ExhaustEnergyRec; // Recovered Exhaust heat  (J)
		Real64 TotalHeatEnergyRec; // total heat recovered (J)
		int QLubeOilRecoveredCurve; // (ELUBEGC) Curve Index for Recoverable Lube Oil heat Input Coef Poly Fit
		Real64 UA; // (UACGC) exhaust gas Heat Exchanger UA
		Array1D< Real64 > UACoef; // Heat Exchanger UA  Coeffs Poly Fit
		Real64 MaxExhaustperCTPower; // MAX EXHAUST FLOW PER W POWER OUTPUT COEFF
		Real64 DesignHeatRecVolFlowRate; // m3/s, Design Water mass flow rate through heat recovery loop
		Real64 DesignHeatRecMassFlowRate; // kg/s, Design Water mass flow rate through heat recovery loop
		Real64 DesignMinExitGasTemp; // Steam Saturation Temperature (C)
		Real64 DesignAirInletTemp; // Design Turbine Air Inlet Temperature (C)
		Real64 ExhaustStackTemp; // turbine exhaust gas temp (C)
		bool HeatRecActive; // true when design max flow rate > 0
		int HeatRecInletNodeNum; // Node number on the heat recovery inlet side of the condenser
		int HeatRecOutletNodeNum; // Node number on the heat recovery outlet side of the condenser
		Real64 HeatRecInletTemp; // Inlet Temperature of the heat recovery fluid
		Real64 HeatRecOutletTemp; // Outlet Temperature of the heat recovery fluid
		Real64 HeatRecMdot; // reporting: Heat Recovery Loop Mass flow rate
		int HRLoopNum; // cooling water plant loop index number, for heat recovery
		int HRLoopSideNum; // cooling water plant loop side index, for heat recovery
		int HRBranchNum; // cooling water plant loop branch index, for heat recovery
		int HRCompNum; // cooling water plant loop component index, for heat recovery
		Real64 FuelMdot; // reporting: Fuel Amount used (kg/s)
		Real64 FuelHeatingValue; // Heating Value for Fuel in (kJ/kg)
		Real64 ElecPowerGenerated; // reporting: power generated (W)
		Real64 ElecEnergyGenerated; // reporting: power generated (W)
		Real64 HeatRecMaxTemp; // Max Temp that can be produced in heat recovery
		int OAInletNode; // optional inlet node index pointer for outdoor air for compustion

		// Default Constructor
		CTGeneratorSpecs() :
			TypeOf( "Generator:CombustionTurbine" ),
			CompType_Num( iGeneratorCombTurbine ),
			RatedPowerOutput( 0.0 ),
			ElectricCircuitNode( 0 ),
			MinPartLoadRat( 0.0 ),
			MaxPartLoadRat( 0.0 ),
			OptPartLoadRat( 0.0 ),
			FuelEnergyUseRate( 0.0 ),
			FuelEnergy( 0.0 ),
			PLBasedFuelInputCurve( 0 ),
			TempBasedFuelInputCurve( 0 ),
			ExhaustFlow( 0.0 ),
			ExhaustFlowCurve( 0 ),
			ExhaustTemp( 0.0 ),
			PLBasedExhaustTempCurve( 0 ),
			TempBasedExhaustTempCurve( 0 ),
			QLubeOilRecovered( 0.0 ),
			QExhaustRecovered( 0.0 ),
			QTotalHeatRecovered( 0.0 ),
			LubeOilEnergyRec( 0.0 ),
			ExhaustEnergyRec( 0.0 ),
			TotalHeatEnergyRec( 0.0 ),
			QLubeOilRecoveredCurve( 0 ),
			UA( 0.0 ),
			UACoef( 2, 0.0 ),
			MaxExhaustperCTPower( 0.0 ),
			DesignHeatRecVolFlowRate( 0.0 ),
			DesignHeatRecMassFlowRate( 0.0 ),
			DesignMinExitGasTemp( 0.0 ),
			DesignAirInletTemp( 0.0 ),
			ExhaustStackTemp( 0.0 ),
			HeatRecActive( false ),
			HeatRecInletNodeNum( 0 ),
			HeatRecOutletNodeNum( 0 ),
			HeatRecInletTemp( 0.0 ),
			HeatRecOutletTemp( 0.0 ),
			HeatRecMdot( 0.0 ),
			HRLoopNum( 0 ),
			HRLoopSideNum( 0 ),
			HRBranchNum( 0 ),
			HRCompNum( 0 ),
			FuelMdot( 0.0 ),
			FuelHeatingValue( 0.0 ),
			ElecPowerGenerated( 0.0 ),
			ElecEnergyGenerated( 0.0 ),
			HeatRecMaxTemp( 0.0 ),
			OAInletNode( 0 )
		{}

	};

	struct ReportVars
	{
		// Members
		Real64 PowerGen; // reporting: power (W)
		Real64 EnergyGen; // reporting: power (W)
		Real64 QTotalHeatRecovered; // reporting: total Heat Recovered (W)
		Real64 QLubeOilRecovered; // reporting: Heat Recovered from Lubricant (W)
		Real64 QExhaustRecovered; // reporting: Heat Recovered from exhaust (W)
		Real64 TotalHeatEnergyRec; // reporting: total Heat Recovered (W)
		Real64 LubeOilEnergyRec; // reporting: Heat Recovered from Lubricant (W)
		Real64 ExhaustEnergyRec; // reporting: Heat Recovered from exhaust (W)
		Real64 FuelEnergyUseRate; // reporting: Fuel Energy use rate (W)
		Real64 FuelEnergy; // reporting: Fuel Energy used (J)
		Real64 FuelMdot; // reporting: Fuel Amount used (kg/s)
		Real64 ExhaustStackTemp; // reporting: Exhaust Stack Temperature (C)
		Real64 HeatRecInletTemp; // reporting: Heat Recovery Loop Inlet Temperature (C)
		Real64 HeatRecOutletTemp; // reporting: Heat Recovery Loop Outlet Temperature (C)
		Real64 HeatRecMdot; // reporting: Heat Recovery Loop Mass flow rate (kg/s)

		// Default Constructor
		ReportVars() :
			PowerGen( 0.0 ),
			EnergyGen( 0.0 ),
			QTotalHeatRecovered( 0.0 ),
			QLubeOilRecovered( 0.0 ),
			QExhaustRecovered( 0.0 ),
			TotalHeatEnergyRec( 0.0 ),
			LubeOilEnergyRec( 0.0 ),
			ExhaustEnergyRec( 0.0 ),
			FuelEnergyUseRate( 0.0 ),
			FuelEnergy( 0.0 ),
			FuelMdot( 0.0 ),
			ExhaustStackTemp( 0.0 ),
			HeatRecInletTemp( 0.0 ),
			HeatRecOutletTemp( 0.0 ),
			HeatRecMdot( 0.0 )
		{}

	};

	// Object Data
	extern Array1D< CTGeneratorSpecs > CTGenerator; // dimension to number of machines
	extern Array1D< ReportVars > CTGeneratorReport;

	// Functions

	void
	SimCTGenerator(
		int const GeneratorType, // type of Generator
		std::string const & GeneratorName, // user specified name of Generator
		int & GeneratorIndex,
		bool const RunFlag, // simulate Generator when TRUE
		Real64 const MyLoad, // generator demand
		bool const FirstHVACIteration
	);

	void
	SimCTPlantHeatRecovery(
		std::string const & CompType, // unused1208
		std::string const & CompName,
		int const CompTypeNum, // unused1208
		int & CompNum,
		bool const RunFlag,
		bool & InitLoopEquip,
		Real64 & MyLoad,
		Real64 & MaxCap,
		Real64 & MinCap,
		Real64 & OptCap,
		bool const FirstHVACIteration // TRUE if First iteration of simulation
	);

	// End CT Generator Module Driver Subroutines
	//******************************************************************************

	// Beginning of CT Generator Module Get Input subroutines
	//******************************************************************************

	void
	GetCTGeneratorInput();

	// End of Get Input subroutines for the CT Generator Module
	//******************************************************************************

	// Beginning of Generator model Subroutines
	// *****************************************************************************

	void
	CalcCTGeneratorModel(
		int const GeneratorNum, // Generator number
		bool const RunFlag, // TRUE when Generator operating
		Real64 const MyLoad, // Generator demand
		bool const FirstHVACIteration
	);

	// End of CT Generator Module Model Subroutines
	// *****************************************************************************

	// Begin CT Generator Module Utility Subroutines
	// *****************************************************************************

	void
	InitCTGenerators(
		int const GeneratorNum, // Generator number
		bool const RunFlag, // TRUE when Generator operating
		Real64 const MyLoad, // Generator demand
		bool const FirstHVACIteration
	);

	// End CT Generator Module Utility Subroutines
	// *****************************************************************************

	// Beginning of Record Keeping subroutines for the CT Generator Module
	// *****************************************************************************

	void
	UpdateCTGeneratorRecords(
		bool const RunFlag, // TRUE if Generator operating
		int const Num // Generator number
	);

	void
	GetCTGeneratorResults(
		int const GeneratorType, // type of Generator
		int const GeneratorIndex,
		Real64 & GeneratorPower, // electrical power
		Real64 & GeneratorEnergy, // electrical energy
		Real64 & ThermalPower, // heat power
		Real64 & ThermalEnergy // heat energy
	);

	// End of Record Keeping subroutines for the CT Generator Module
	// *****************************************************************************

} // CTElectricGenerator

} // EnergyPlus

#endif
