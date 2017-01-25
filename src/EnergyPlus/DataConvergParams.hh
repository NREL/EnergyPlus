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

#ifndef DataConvergParams_hh_INCLUDED
#define DataConvergParams_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace DataConvergParams {

	// Using/Aliasing

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:

	// Note: Unless otherwise noted, the tolerance parameters listed below were chosen
	// to represent educated guesses at what the tolerances for individual physical
	// parameters should be.
	extern Real64 const HVACEnthalpyToler; // Tolerance for enthalpy comparisons (in kJ/kgK)
	extern Real64 const HVACFlowRateToler; // Tolerance for mass flow rate convergence (in kg/s) [~20 CFM]
	extern Real64 const HVACFlowRateSlopeToler; // Slope tolerance for mass flow, kg/s/iteration
	extern Real64 const HVACFlowRateOscillationToler; // tolerance for detecting duplicate flow rate in stack
	extern Real64 const HVACHumRatToler; // Tolerance for humidity ratio comparisons (kg water/kg dryair)
	extern Real64 const HVACHumRatSlopeToler; // Slope tolerance for humidity ratio, kg water/kg-dryair/iteration
	extern Real64 const HVACHumRatOscillationToler; // tolerance for detecting duplicate humidity ratio in stack
	extern Real64 const HVACQualityToler; // Tolerance for fluid quality comparisons (dimensionless)
	extern Real64 const HVACPressToler; // Tolerance for pressure comparisons (in Pascals)
	extern Real64 const HVACTemperatureToler; // Tolerance for temperature comparisons (in degrees C or K)
	extern Real64 const HVACTemperatureSlopeToler; // Slope tolerance for Temperature, Deg C/iteration
	extern Real64 const HVACTemperatureOscillationToler; // tolerance for detecting duplicate temps in stack
	extern Real64 const HVACEnergyToler; // Tolerance for Energy comparisons (in Watts W)
	// to be consistent, should be 20.d0 (BG Aug 2012)

	extern Real64 const HVACCpApprox; // Air Cp (20C,0.0Kg/Kg) Only for energy Tolerance Calculation
	// Only used to scale the answer for a more intuitive answer for comparison

	extern Real64 const PlantEnthalpyToler; // Tolerance for enthalpy comparisons (in kJ/kgK)
	extern Real64 const PlantFlowRateToler; // Tolerance for mass flow rate convergence (in kg/s) [~2 CFM]
	extern Real64 const PlantLowFlowRateToler; // // Tolerance for low flow rate used for determining when
	//plant pumps can be shut down
	extern Real64 const PlantFlowRateOscillationToler;
	extern Real64 const PlantFlowRateSlopeToler; // Slope tolerance for mass flow, kg/s/iteration

	extern Real64 const PlantPressToler; // Tolerance for pressure comparisons (in Pascals)
	extern Real64 const PlantTemperatureToler; // Tolerance for temperature comparisons (in degrees C or K)
	extern Real64 const PlantTemperatureSlopeToler; // Slope tolerance for Temperature, Deg C/iteration
	extern Real64 const PlantTemperatureOscillationToler; // tolerance for detecting duplicate temps in stack

	extern Real64 const PlantEnergyToler; // Tolerance for Energy comparisons (in Watts W)

	extern Real64 const PlantCpApprox; // Approximate Cp used in Interface manager for
	// Energy Tolerance Calculation, used to scale the answer
	// for a more intuitive answer for comparison
	extern Real64 const PlantFlowFlowRateToler; // Tolerance for mass flow rate convergence (in kg/s)

	extern int const ConvergLogStackDepth;
	extern Array1D< Real64 > const ConvergLogStackARR;
	extern Real64 const sum_ConvergLogStackARR;
	extern Real64 const square_sum_ConvergLogStackARR;
	extern Real64 const sum_square_ConvergLogStackARR;

	extern int const CalledFromAirSystemDemandSide;
	extern int const CalledFromAirSystemSupplySideDeck1;
	extern int const CalledFromAirSystemSupplySideDeck2;
	// DERIVED TYPE DEFINITIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:

	extern int AirLoopConvergFail;

	extern Real64 MinTimeStepSys; // =1 minute
	extern Real64 MinTimeStepTol; // = min allowable for ABS(1.-TimeStepSys/(MinTimeStepSys))
	extern Real64 MaxZoneTempDiff; // 0.3 C = (1% OF 300 C) = max allowable difference between
	//   zone air temp at Time=T and Time=T-1
	extern Real64 MinSysTimeRemaining; // = 1 second
	extern int MaxIter; // maximum number of iterations allowed

	extern int MaxPlantSubIterations; // Iteration Max for Plant Simulation sub iterations
	extern int MinPlantSubIterations; // Iteration Min for Plant Simulation sub iterations

	// Types

	struct HVACNodeConvergLogStruct
	{
		// Members
		int NodeNum;
		Array1D< Real64 > HumidityRatio;
		Array1D< Real64 > MassFlowRate;
		Array1D< Real64 > Temperature;

		// Default Constructor
		HVACNodeConvergLogStruct() :
			NodeNum( 0 ),
			HumidityRatio( ConvergLogStackDepth ),
			MassFlowRate( ConvergLogStackDepth ),
			Temperature( ConvergLogStackDepth )
		{}

	};

	struct HVACZoneInletConvergenceStruct
	{
		// Members
		std::string ZoneName;
		int NumInletNodes; // number of inlet nodes for zone
		Array1D< HVACNodeConvergLogStruct > InletNode;

		// Default Constructor
		HVACZoneInletConvergenceStruct() :
			NumInletNodes( 0 )
		{}

	};

	struct HVACAirLoopIterationConvergenceStruct
	{
		// Members
		Array1D_bool HVACMassFlowNotConverged; // Flag to show mass flow convergence
		Array1D< Real64 > HVACFlowDemandToSupplyTolValue; // Queue of convergence "results"
		Array1D< Real64 > HVACFlowSupplyDeck1ToDemandTolValue; // Queue of convergence "results"
		Array1D< Real64 > HVACFlowSupplyDeck2ToDemandTolValue; // Queue of convergence "results"
		Array1D_bool HVACHumRatNotConverged; // Flag to show humidity ratio convergence   or failure
		Array1D< Real64 > HVACHumDemandToSupplyTolValue; // Queue of convergence "results"
		Array1D< Real64 > HVACHumSupplyDeck1ToDemandTolValue; // Queue of convergence "results"
		Array1D< Real64 > HVACHumSupplyDeck2ToDemandTolValue; // Queue of convergence "results"
		Array1D_bool HVACTempNotConverged; // Flag to show temperature convergence  or failure
		Array1D< Real64 > HVACTempDemandToSupplyTolValue; // Queue of convergence "results"
		Array1D< Real64 > HVACTempSupplyDeck1ToDemandTolValue; // Queue of convergence "results"
		Array1D< Real64 > HVACTempSupplyDeck2ToDemandTolValue; // Queue of convergence "results"
		Array1D_bool HVACEnergyNotConverged; // Flag to show energy convergence   or failure
		Array1D< Real64 > HVACEnergyDemandToSupplyTolValue; // Queue of convergence "results"
		Array1D< Real64 > HVACEnergySupplyDeck1ToDemandTolValue; // Queue of convergence "results"
		Array1D< Real64 > HVACEnergySupplyDeck2ToDemandTolValue; // Queue of convergence "results"
		Array1D_bool HVACEnthalpyNotConverged; // Flag to show energy convergence   or failure
		Array1D< Real64 > HVACEnthalpyDemandToSupplyTolValue; // Queue of convergence "results"
		Array1D< Real64 > HVACEnthalpySupplyDeck1ToDemandTolValue; // Queue of convergence "results"
		Array1D< Real64 > HVACEnthalpySupplyDeck2ToDemandTolValue; // Queue of convergence "results"
		Array1D_bool HVACPressureNotConverged; // Flag to show energy convergence   or failure
		Array1D< Real64 > HVACPressureDemandToSupplyTolValue; // Queue of convergence "results"
		Array1D< Real64 > HVACPressureSupplyDeck1ToDemandTolValue; // Queue of convergence "results"
		Array1D< Real64 > HVACPressueSupplyDeck2ToDemandTolValue; // Queue of convergence "results"
		Array1D_bool HVACQualityNotConverged; // Flag to show energy convergence   or failure
		Array1D< Real64 > HVACQualityDemandToSupplyTolValue; // Queue of convergence "results"
		Array1D< Real64 > HVACQualitSupplyDeck1ToDemandTolValue; // Queue of convergence "results"
		Array1D< Real64 > HVACQualitySupplyDeck2ToDemandTolValue; // Queue of convergence "results"

		// Default Constructor
		HVACAirLoopIterationConvergenceStruct() :
			HVACMassFlowNotConverged( 3, false ),
			HVACFlowDemandToSupplyTolValue( ConvergLogStackDepth, 0.0 ),
			HVACFlowSupplyDeck1ToDemandTolValue( ConvergLogStackDepth, 0.0 ),
			HVACFlowSupplyDeck2ToDemandTolValue( ConvergLogStackDepth, 0.0 ),
			HVACHumRatNotConverged( 3, false ),
			HVACHumDemandToSupplyTolValue( ConvergLogStackDepth, 0.0 ),
			HVACHumSupplyDeck1ToDemandTolValue( ConvergLogStackDepth, 0.0 ),
			HVACHumSupplyDeck2ToDemandTolValue( ConvergLogStackDepth, 0.0 ),
			HVACTempNotConverged( 3, false ),
			HVACTempDemandToSupplyTolValue( ConvergLogStackDepth, 0.0 ),
			HVACTempSupplyDeck1ToDemandTolValue( ConvergLogStackDepth, 0.0 ),
			HVACTempSupplyDeck2ToDemandTolValue( ConvergLogStackDepth, 0.0 ),
			HVACEnergyNotConverged( 3, false ),
			HVACEnergyDemandToSupplyTolValue( ConvergLogStackDepth, 0.0 ),
			HVACEnergySupplyDeck1ToDemandTolValue( ConvergLogStackDepth, 0.0 ),
			HVACEnergySupplyDeck2ToDemandTolValue( ConvergLogStackDepth, 0.0 ),
			HVACEnthalpyNotConverged( 3, false ),
			HVACEnthalpyDemandToSupplyTolValue( ConvergLogStackDepth, 0.0 ),
			HVACEnthalpySupplyDeck1ToDemandTolValue( ConvergLogStackDepth, 0.0 ),
			HVACEnthalpySupplyDeck2ToDemandTolValue( ConvergLogStackDepth, 0.0 ),
			HVACPressureNotConverged( 3, false ),
			HVACPressureDemandToSupplyTolValue( ConvergLogStackDepth, 0.0 ),
			HVACPressureSupplyDeck1ToDemandTolValue( ConvergLogStackDepth, 0.0 ),
			HVACPressueSupplyDeck2ToDemandTolValue( ConvergLogStackDepth, 0.0 ),
			HVACQualityNotConverged( 3, false ),
			HVACQualityDemandToSupplyTolValue( ConvergLogStackDepth, 0.0 ),
			HVACQualitSupplyDeck1ToDemandTolValue( ConvergLogStackDepth, 0.0 ),
			HVACQualitySupplyDeck2ToDemandTolValue( ConvergLogStackDepth, 0.0 )
		{}

	};

	struct PlantIterationConvergenceStruct
	{
		// Members
		bool PlantMassFlowNotConverged; // Flag to show mass flow convergence
		Array1D< Real64 > PlantFlowDemandToSupplyTolValue; // Queue of convergence "results"
		Array1D< Real64 > PlantFlowSupplyToDemandTolValue; // Queue of convergence "results"
		bool PlantTempNotConverged; // Flag to show temperature convergence (0) or failure (1)
		Array1D< Real64 > PlantTempDemandToSupplyTolValue; // Queue of convergence "results"
		Array1D< Real64 > PlantTempSupplyToDemandTolValue; // Queue of convergence "results"

		// Default Constructor
		PlantIterationConvergenceStruct() :
			PlantMassFlowNotConverged( false ),
			PlantFlowDemandToSupplyTolValue( ConvergLogStackDepth, 0.0 ),
			PlantFlowSupplyToDemandTolValue( ConvergLogStackDepth, 0.0 ),
			PlantTempNotConverged( false ),
			PlantTempDemandToSupplyTolValue( ConvergLogStackDepth, 0.0 ),
			PlantTempSupplyToDemandTolValue( ConvergLogStackDepth, 0.0 )
		{}

	};

	// Object Data
	extern Array1D< HVACZoneInletConvergenceStruct > ZoneInletConvergence;
	extern Array1D< HVACAirLoopIterationConvergenceStruct > AirLoopConvergence;
	extern Array1D< PlantIterationConvergenceStruct > PlantConvergence;

	void
	clear_state();

} // DataConvergParams

} // EnergyPlus

#endif
