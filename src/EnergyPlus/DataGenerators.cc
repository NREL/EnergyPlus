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

// EnergyPlus Headers
#include <DataGenerators.hh>
#include <DataPrecisionGlobals.hh>

namespace EnergyPlus {

namespace DataGenerators {

	// MODULE INFORMATION:
	//       AUTHOR         B Griffith
	//       DATE WRITTEN   March 2005
	//       MODIFIED
	//       RE-ENGINEERED  July 2006 BG, generalized and added data for ICE/SE model micro CHP

	// PURPOSE OF THIS MODULE:
	// This data-only module is a repository for the variables that relate specifically
	// to the Fuel cell and Micro CHP modeling in EnergyPlus
	//  the data for the older BLAST generators are in those component's modules

	// METHODOLOGY EMPLOYED:

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:
	int const NormalizedCurveMode( 1 ); // mode where efficiency curves are modifier curves
	int const DirectCurveMode( 2 ); // mode where efficiency curves are direct

	int const ConstantRateSkinLoss( 1 ); // fixed rate mode for skin losses
	int const UADTSkinLoss( 2 ); // UAdelta T mode for skin losses
	int const QuadraticFuelNdotSkin( 3 ); // Quadratic function of fuel flow for skin losses

	int const QuadraticFuncofNdot( 1 ); // function of fuel rate mode for air flow
	int const ConstantStoicsAirRat( 2 ); // Constant air ratio in stoics with fuel constituents
	int const QuadraticFuncofPel( 3 ); // function of electric power mode

	int const NoRecoveryOnAirIntake( 101 ); // mode for controlling intake air heat recovery
	int const RecoverBurnInvertBatt( 102 ); // mode for controlling intake air heat recovery
	int const RecoverAuxiliaryBurner( 103 ); // mode for controlling intake air heat recovery
	int const RecoverInverterBatt( 104 ); // mode for controlling intake air heat recovery
	int const RecoverInverter( 105 ); // mode for controlling intake air heat recovery
	int const RecoverBattery( 106 ); // mode for controlling intake air heat recovery

	int const RegularAir( 1 );
	int const UserDefinedConstituents( 2 );

	int const FuelInTempFromNode( 1 );
	int const FuelInTempSchedule( 2 );

	int const WaterInReformMains( 21 );
	int const WaterInReformAirNode( 22 );
	int const WaterInReformWaterNode( 23 );
	int const WaterInReformSchedule( 24 );

	int const InverterEffConstant( 1 );
	int const InverterEffQuadratic( 2 );

	int const FixedEffectiveness( 11 ); // exhaust gas HX modeling mode
	int const LMTDempiricalUAeff( 12 ); // exhaust gas HX modeling mode
	int const LMTDfundementalUAeff( 13 ); // exhaust gas HX modeling mode
	int const Condensing( 14 ); // exhaust gas HX modeling mode

	int const SimpleEffConstraints( 21 ); // electrical storage modeling mode
	int const LeadAcidBatterySaupe( 22 ); // electrical storage modeling mode
	int const LeadAcidBatterManwellMcGowan( 23 ); // electrical storage modeling mode

	int const SurroundingZone( 31 );
	int const AirInletForFC( 32 );

	int const OpModeOff( 1 ); // CHP operating mode OFF
	int const OpModeStandby( 2 ); // CHP operating mode Stand By
	int const OpModeWarmUp( 3 ); // CHP operating mode Warm Up or start up
	int const OpModeNormal( 4 ); // CHP operating mode Normal
	int const OpModeCoolDown( 5 ); // CHP operating mode Cool down or shut down

	int const fuelModeGaseousConstituents( 301 );
	int const fuelModeGenericLiquid( 302 );

	Real64 const MinProductGasTemp( 100.0 ); // Minimum bound on search for product gas temps
	Real64 const MaxProductGasTemp( 2000.0 ); // Maximum bound on search for product gas temps

	int const NISTShomate( 41 );
	int const NASAPolynomial( 42 );

	Real64 const RinKJperMolpK( 0.0083145 ); // R is ideal gas constant (kJ/mol-K)
	Real64 const InitHRTemp( 50.0 ); // Initialization temperature for heat recovery water

	Real64 const ImBalanceTol( 0.00001 ); // used as fraction of electrical power at power module

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:

	int NumFuelConstit( 0 );
	int NumGeneratorFuelSups( 0 );
	int NumFuelCellGenerators( 0 ); // number of SOFC Generators specified in input
	int NumMicroCHPs( 0 );
	int NumMicroCHPParams( 0 ); // number of parameter sets for micro chp
	int NumGensWDynamics( 0 ); // number of dynamics controls for generators

	// Object Data
	Array1D< FCDataStruct > FuelCell; // dimension to number of machines
	Array1D< GasPropertyDataStruct > GasPhaseThermoChemistryData;
	Array1D< GeneratorFuelSupplyDataStruct > FuelSupply; // fuel supply (reused across various)
	Array1D< MicroCHPDataStruct > MicroCHP;
	Array1D< MicroCHPParamsNonNormalized > MicroCHPParamInput; // Used during get input then put into nested
	Array1D< GeneratorDynamicsManagerStruct > GeneratorDynamics;

	void
	clear_state()
	{
		NumFuelConstit = 0;
		NumGeneratorFuelSups = 0;
		NumFuelCellGenerators = 0;
		NumMicroCHPs = 0;
		NumMicroCHPParams = 0;
		NumGensWDynamics = 0;
		FuelCell.deallocate();
		GasPhaseThermoChemistryData.deallocate();
		FuelSupply.deallocate();
		MicroCHP.deallocate();
		MicroCHPParamInput.deallocate();
		GeneratorDynamics.deallocate();
	}

} // DataGenerators

} // EnergyPlus
