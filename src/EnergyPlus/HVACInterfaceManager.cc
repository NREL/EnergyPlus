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

// EnergyPlus Headers
#include <HVACInterfaceManager.hh>
#include <DataBranchAirLoopPlant.hh>
#include <DataContaminantBalance.hh>
#include <DataConvergParams.hh>
#include <DataGlobals.hh>
#include <DataHVACGlobals.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <FluidProperties.hh>
#include <OutputProcessor.hh>
#include <PlantUtilities.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace HVACInterfaceManager {

	// MODULE INFORMATION:
	//       AUTHOR         Rick Strand
	//       DATE WRITTEN   October 1998
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This module contains one or more routines for checking the convergence
	// of the various HVAC loops and passing information across interface
	// boundaries.

	// METHODOLOGY EMPLOYED:
	// The upper level HVAC managers call the routine(s) contained in this
	// module as a last step.  The node information is passed across the
	// interface boundary and the logical flag is set if the nodes across
	// from each other are not within tolerance.

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	//Common Pipe Recirc Flow Directions
	int const NoRecircFlow( 0 );
	int const PrimaryRecirc( 1 ); // flow from Supply-outlet/Demand-inlet to Supply-inlet/demand-outlet
	int const SecondaryRecirc( 2 ); // flow from Supply-inlet/Demand-oulet to Supply-outlet/demand-inlet

	int const FlowTypeNotSet( 9 );
	int const ConstantFlow( 10 );
	int const VariableFlow( 11 );

	// DERIVED TYPE DEFINITIONS:

	// INTERFACE BLOCK SPECIFICATIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:
	bool CommonPipeSetupFinished( false );

	// SUBROUTINE SPECIFICATIONS FOR MODULE ConductionTransferFunctionCalc

	// Object Data
	Array1D< CommonPipeData > PlantCommonPipe;

	// MODULE SUBROUTINES:

	// Functions

	void
	UpdateHVACInterface(
		int const AirLoopNum, // airloop number for which air loop this is
		int const CalledFrom,
		int const OutletNode, // Node number for the outlet of the side of the loop just simulated
		int const InletNode, // Node number for the inlet of the side that needs the outlet node data
		bool & OutOfToleranceFlag // True when the other side of the loop need to be (re)simulated
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   October 1998
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine manages any generic HVAC loop interface.

		// METHODOLOGY EMPLOYED:
		// This is a simple "forward" interface where all of the properties
		// from the outlet of one side of the loop get transfered directly
		// to the inlet node of the corresponding other side of the loop.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataLoopNode::Node;
		using namespace DataConvergParams;
		using DataContaminantBalance::Contaminant;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static Array1D< Real64 > TmpRealARR( ConvergLogStackDepth ); //Tuned Made static
		Real64 DeltaEnergy;
		// FLOW:

		//Calculate the approximate energy difference across interface for comparison
		DeltaEnergy = HVACCpApprox * ( ( Node( OutletNode ).MassFlowRate * Node( OutletNode ).Temp ) - ( Node( InletNode ).MassFlowRate * Node( InletNode ).Temp ) );

		if ( CalledFrom == CalledFromAirSystemDemandSide ) {

			AirLoopConvergence( AirLoopNum ).HVACMassFlowNotConverged( 1 ) = false;
			AirLoopConvergence( AirLoopNum ).HVACHumRatNotConverged( 1 ) = false;
			AirLoopConvergence( AirLoopNum ).HVACTempNotConverged( 1 ) = false;
			AirLoopConvergence( AirLoopNum ).HVACEnergyNotConverged( 1 ) = false;
			AirLoopConvergence( AirLoopNum ).HVACEnthalpyNotConverged( 1 ) = false;
			AirLoopConvergence( AirLoopNum ).HVACPressureNotConverged( 1 ) = false;

			TmpRealARR = AirLoopConvergence( AirLoopNum ).HVACFlowDemandToSupplyTolValue;
			AirLoopConvergence( AirLoopNum ).HVACFlowDemandToSupplyTolValue( 1 ) = std::abs( Node( OutletNode ).MassFlowRate - Node( InletNode ).MassFlowRate );
			AirLoopConvergence( AirLoopNum ).HVACFlowDemandToSupplyTolValue( {2,ConvergLogStackDepth} ) = TmpRealARR( {1,ConvergLogStackDepth - 1} );
			if ( AirLoopConvergence( AirLoopNum ).HVACFlowDemandToSupplyTolValue( 1 ) > HVACFlowRateToler ) {
				AirLoopConvergence( AirLoopNum ).HVACMassFlowNotConverged( 1 ) = true;
				OutOfToleranceFlag = true; // Something has changed--resimulate the other side of the loop
			}

			TmpRealARR = AirLoopConvergence( AirLoopNum ).HVACHumDemandToSupplyTolValue;
			AirLoopConvergence( AirLoopNum ).HVACHumDemandToSupplyTolValue( 1 ) = std::abs( Node( OutletNode ).HumRat - Node( InletNode ).HumRat );
			AirLoopConvergence( AirLoopNum ).HVACHumDemandToSupplyTolValue( {2,ConvergLogStackDepth} ) = TmpRealARR( {1,ConvergLogStackDepth - 1} );
			if ( AirLoopConvergence( AirLoopNum ).HVACHumDemandToSupplyTolValue( 1 ) > HVACHumRatToler ) {
				AirLoopConvergence( AirLoopNum ).HVACHumRatNotConverged( 1 ) = true;
				OutOfToleranceFlag = true; // Something has changed--resimulate the other side of the loop
			}

			TmpRealARR = AirLoopConvergence( AirLoopNum ).HVACTempDemandToSupplyTolValue;
			AirLoopConvergence( AirLoopNum ).HVACTempDemandToSupplyTolValue( 1 ) = std::abs( Node( OutletNode ).Temp - Node( InletNode ).Temp );
			AirLoopConvergence( AirLoopNum ).HVACTempDemandToSupplyTolValue( {2,ConvergLogStackDepth} ) = TmpRealARR( {1,ConvergLogStackDepth - 1} );
			if ( AirLoopConvergence( AirLoopNum ).HVACTempDemandToSupplyTolValue( 1 ) > HVACTemperatureToler ) {
				AirLoopConvergence( AirLoopNum ).HVACTempNotConverged( 1 ) = true;
				OutOfToleranceFlag = true; // Something has changed--resimulate the other side of the loop
			}

			TmpRealARR = AirLoopConvergence( AirLoopNum ).HVACEnergyDemandToSupplyTolValue;
			AirLoopConvergence( AirLoopNum ).HVACEnergyDemandToSupplyTolValue( 1 ) = std::abs( DeltaEnergy );
			AirLoopConvergence( AirLoopNum ).HVACEnergyDemandToSupplyTolValue( {2,ConvergLogStackDepth} ) = TmpRealARR( {1,ConvergLogStackDepth - 1} );
			if ( std::abs( DeltaEnergy ) > HVACEnergyToler ) {
				AirLoopConvergence( AirLoopNum ).HVACEnergyNotConverged( 1 ) = true;
				OutOfToleranceFlag = true; // Something has changed--resimulate the other side of the loop
			}

			TmpRealARR = AirLoopConvergence( AirLoopNum ).HVACEnthalpyDemandToSupplyTolValue;
			AirLoopConvergence( AirLoopNum ).HVACEnthalpyDemandToSupplyTolValue( 1 ) = std::abs( Node( OutletNode ).Enthalpy - Node( InletNode ).Enthalpy );
			AirLoopConvergence( AirLoopNum ).HVACEnthalpyDemandToSupplyTolValue( {2,ConvergLogStackDepth} ) = TmpRealARR( {1,ConvergLogStackDepth - 1} );
			if ( AirLoopConvergence( AirLoopNum ).HVACEnthalpyDemandToSupplyTolValue( 1 ) > HVACEnthalpyToler ) {
				AirLoopConvergence( AirLoopNum ).HVACEnthalpyNotConverged( 1 ) = true;
				OutOfToleranceFlag = true; // Something has changed--resimulate the other side of the loop
			}

			TmpRealARR = AirLoopConvergence( AirLoopNum ).HVACPressureDemandToSupplyTolValue;
			AirLoopConvergence( AirLoopNum ).HVACPressureDemandToSupplyTolValue( 1 ) = std::abs( Node( OutletNode ).Press - Node( InletNode ).Press );
			AirLoopConvergence( AirLoopNum ).HVACPressureDemandToSupplyTolValue( {2,ConvergLogStackDepth} ) = TmpRealARR( {1,ConvergLogStackDepth - 1} );
			if ( AirLoopConvergence( AirLoopNum ).HVACPressureDemandToSupplyTolValue( 1 ) > HVACPressToler ) {
				AirLoopConvergence( AirLoopNum ).HVACPressureNotConverged( 1 ) = true;
				OutOfToleranceFlag = true; // Something has changed--resimulate the other side of the loop
			}

		} else if ( CalledFrom == CalledFromAirSystemSupplySideDeck1 ) {

			AirLoopConvergence( AirLoopNum ).HVACMassFlowNotConverged( 2 ) = false;
			AirLoopConvergence( AirLoopNum ).HVACHumRatNotConverged( 2 ) = false;
			AirLoopConvergence( AirLoopNum ).HVACTempNotConverged( 2 ) = false;
			AirLoopConvergence( AirLoopNum ).HVACEnergyNotConverged( 2 ) = false;
			AirLoopConvergence( AirLoopNum ).HVACEnthalpyNotConverged( 2 ) = false;
			AirLoopConvergence( AirLoopNum ).HVACPressureNotConverged( 2 ) = false;

			TmpRealARR = AirLoopConvergence( AirLoopNum ).HVACFlowSupplyDeck1ToDemandTolValue;
			AirLoopConvergence( AirLoopNum ).HVACFlowSupplyDeck1ToDemandTolValue( 1 ) = std::abs( Node( OutletNode ).MassFlowRate - Node( InletNode ).MassFlowRate );
			AirLoopConvergence( AirLoopNum ).HVACFlowSupplyDeck1ToDemandTolValue( {2,ConvergLogStackDepth} ) = TmpRealARR( {1,ConvergLogStackDepth - 1} );
			if ( AirLoopConvergence( AirLoopNum ).HVACFlowSupplyDeck1ToDemandTolValue( 1 ) > HVACFlowRateToler ) {
				AirLoopConvergence( AirLoopNum ).HVACMassFlowNotConverged( 2 ) = true;
				OutOfToleranceFlag = true; // Something has changed--resimulate the other side of the loop
			}

			TmpRealARR = AirLoopConvergence( AirLoopNum ).HVACHumSupplyDeck1ToDemandTolValue;
			AirLoopConvergence( AirLoopNum ).HVACHumSupplyDeck1ToDemandTolValue( 1 ) = std::abs( Node( OutletNode ).HumRat - Node( InletNode ).HumRat );
			AirLoopConvergence( AirLoopNum ).HVACHumSupplyDeck1ToDemandTolValue( {2,ConvergLogStackDepth} ) = TmpRealARR( {1,ConvergLogStackDepth - 1} );
			if ( AirLoopConvergence( AirLoopNum ).HVACHumSupplyDeck1ToDemandTolValue( 1 ) > HVACHumRatToler ) {
				AirLoopConvergence( AirLoopNum ).HVACHumRatNotConverged( 2 ) = true;
				OutOfToleranceFlag = true; // Something has changed--resimulate the other side of the loop
			}

			TmpRealARR = AirLoopConvergence( AirLoopNum ).HVACTempSupplyDeck1ToDemandTolValue;
			AirLoopConvergence( AirLoopNum ).HVACTempSupplyDeck1ToDemandTolValue( 1 ) = std::abs( Node( OutletNode ).Temp - Node( InletNode ).Temp );
			AirLoopConvergence( AirLoopNum ).HVACTempSupplyDeck1ToDemandTolValue( {2,ConvergLogStackDepth} ) = TmpRealARR( {1,ConvergLogStackDepth - 1} );
			if ( AirLoopConvergence( AirLoopNum ).HVACTempSupplyDeck1ToDemandTolValue( 1 ) > HVACTemperatureToler ) {
				AirLoopConvergence( AirLoopNum ).HVACTempNotConverged( 2 ) = true;
				OutOfToleranceFlag = true; // Something has changed--resimulate the other side of the loop
			}

			TmpRealARR = AirLoopConvergence( AirLoopNum ).HVACEnergySupplyDeck1ToDemandTolValue;
			AirLoopConvergence( AirLoopNum ).HVACEnergySupplyDeck1ToDemandTolValue( 1 ) = DeltaEnergy;
			AirLoopConvergence( AirLoopNum ).HVACEnergySupplyDeck1ToDemandTolValue( {2,ConvergLogStackDepth} ) = TmpRealARR( {1,ConvergLogStackDepth - 1} );
			if ( std::abs( DeltaEnergy ) > HVACEnergyToler ) {
				AirLoopConvergence( AirLoopNum ).HVACEnergyNotConverged( 2 ) = true;
				OutOfToleranceFlag = true; // Something has changed--resimulate the other side of the loop
			}

			TmpRealARR = AirLoopConvergence( AirLoopNum ).HVACEnthalpySupplyDeck1ToDemandTolValue;
			AirLoopConvergence( AirLoopNum ).HVACEnthalpySupplyDeck1ToDemandTolValue( 1 ) = std::abs( Node( OutletNode ).Enthalpy - Node( InletNode ).Enthalpy );
			AirLoopConvergence( AirLoopNum ).HVACEnthalpySupplyDeck1ToDemandTolValue( {2,ConvergLogStackDepth} ) = TmpRealARR( {1,ConvergLogStackDepth - 1} );
			if ( AirLoopConvergence( AirLoopNum ).HVACEnthalpySupplyDeck1ToDemandTolValue( 1 ) > HVACEnthalpyToler ) {
				AirLoopConvergence( AirLoopNum ).HVACEnthalpyNotConverged( 2 ) = true;
				OutOfToleranceFlag = true; // Something has changed--resimulate the other side of the loop
			}

			TmpRealARR = AirLoopConvergence( AirLoopNum ).HVACPressureSupplyDeck1ToDemandTolValue;
			AirLoopConvergence( AirLoopNum ).HVACPressureSupplyDeck1ToDemandTolValue( 1 ) = std::abs( Node( OutletNode ).Press - Node( InletNode ).Press );
			AirLoopConvergence( AirLoopNum ).HVACPressureSupplyDeck1ToDemandTolValue( {2,ConvergLogStackDepth} ) = TmpRealARR( {1,ConvergLogStackDepth - 1} );
			if ( AirLoopConvergence( AirLoopNum ).HVACPressureSupplyDeck1ToDemandTolValue( 1 ) > HVACPressToler ) {
				AirLoopConvergence( AirLoopNum ).HVACPressureNotConverged( 2 ) = true;
				OutOfToleranceFlag = true; // Something has changed--resimulate the other side of the loop
			}

		} else if ( CalledFrom == CalledFromAirSystemSupplySideDeck2 ) {

			AirLoopConvergence( AirLoopNum ).HVACMassFlowNotConverged( 3 ) = false;
			AirLoopConvergence( AirLoopNum ).HVACHumRatNotConverged( 3 ) = false;
			AirLoopConvergence( AirLoopNum ).HVACTempNotConverged( 3 ) = false;
			AirLoopConvergence( AirLoopNum ).HVACEnergyNotConverged( 3 ) = false;
			AirLoopConvergence( AirLoopNum ).HVACEnthalpyNotConverged( 3 ) = false;
			AirLoopConvergence( AirLoopNum ).HVACPressureNotConverged( 3 ) = false;

			TmpRealARR = AirLoopConvergence( AirLoopNum ).HVACFlowSupplyDeck2ToDemandTolValue;
			AirLoopConvergence( AirLoopNum ).HVACFlowSupplyDeck2ToDemandTolValue( 1 ) = std::abs( Node( OutletNode ).MassFlowRate - Node( InletNode ).MassFlowRate );
			AirLoopConvergence( AirLoopNum ).HVACFlowSupplyDeck2ToDemandTolValue( {2,ConvergLogStackDepth} ) = TmpRealARR( {1,ConvergLogStackDepth - 1} );
			if ( AirLoopConvergence( AirLoopNum ).HVACFlowSupplyDeck2ToDemandTolValue( 1 ) > HVACFlowRateToler ) {
				AirLoopConvergence( AirLoopNum ).HVACMassFlowNotConverged( 3 ) = true;
				OutOfToleranceFlag = true; // Something has changed--resimulate the other side of the loop
			}

			TmpRealARR = AirLoopConvergence( AirLoopNum ).HVACHumSupplyDeck2ToDemandTolValue;
			AirLoopConvergence( AirLoopNum ).HVACHumSupplyDeck2ToDemandTolValue( 1 ) = std::abs( Node( OutletNode ).HumRat - Node( InletNode ).HumRat );
			AirLoopConvergence( AirLoopNum ).HVACHumSupplyDeck2ToDemandTolValue( {2,ConvergLogStackDepth} ) = TmpRealARR( {1,ConvergLogStackDepth - 1} );
			if ( AirLoopConvergence( AirLoopNum ).HVACHumSupplyDeck2ToDemandTolValue( 1 ) > HVACHumRatToler ) {
				AirLoopConvergence( AirLoopNum ).HVACHumRatNotConverged( 3 ) = true;
				OutOfToleranceFlag = true; // Something has changed--resimulate the other side of the loop
			}

			TmpRealARR = AirLoopConvergence( AirLoopNum ).HVACTempSupplyDeck2ToDemandTolValue;
			AirLoopConvergence( AirLoopNum ).HVACTempSupplyDeck2ToDemandTolValue( 1 ) = std::abs( Node( OutletNode ).Temp - Node( InletNode ).Temp );
			AirLoopConvergence( AirLoopNum ).HVACTempSupplyDeck2ToDemandTolValue( {2,ConvergLogStackDepth} ) = TmpRealARR( {1,ConvergLogStackDepth - 1} );
			if ( AirLoopConvergence( AirLoopNum ).HVACTempSupplyDeck2ToDemandTolValue( 1 ) > HVACTemperatureToler ) {
				AirLoopConvergence( AirLoopNum ).HVACTempNotConverged( 3 ) = true;
				OutOfToleranceFlag = true; // Something has changed--resimulate the other side of the loop
			}

			TmpRealARR = AirLoopConvergence( AirLoopNum ).HVACEnergySupplyDeck2ToDemandTolValue;
			AirLoopConvergence( AirLoopNum ).HVACEnergySupplyDeck2ToDemandTolValue( 1 ) = DeltaEnergy;
			AirLoopConvergence( AirLoopNum ).HVACEnergySupplyDeck2ToDemandTolValue( {2,ConvergLogStackDepth} ) = TmpRealARR( {1,ConvergLogStackDepth - 1} );
			if ( std::abs( DeltaEnergy ) > HVACEnergyToler ) {
				AirLoopConvergence( AirLoopNum ).HVACEnergyNotConverged( 3 ) = true;
				OutOfToleranceFlag = true; // Something has changed--resimulate the other side of the loop
			}

			TmpRealARR = AirLoopConvergence( AirLoopNum ).HVACEnthalpySupplyDeck2ToDemandTolValue;
			AirLoopConvergence( AirLoopNum ).HVACEnthalpySupplyDeck2ToDemandTolValue( 1 ) = std::abs( Node( OutletNode ).Enthalpy - Node( InletNode ).Enthalpy );
			AirLoopConvergence( AirLoopNum ).HVACEnthalpySupplyDeck2ToDemandTolValue( {2,ConvergLogStackDepth} ) = TmpRealARR( {1,ConvergLogStackDepth - 1} );
			if ( AirLoopConvergence( AirLoopNum ).HVACEnthalpySupplyDeck2ToDemandTolValue( 1 ) > HVACEnthalpyToler ) {
				AirLoopConvergence( AirLoopNum ).HVACEnthalpyNotConverged( 3 ) = true;
				OutOfToleranceFlag = true; // Something has changed--resimulate the other side of the loop
			}

			TmpRealARR = AirLoopConvergence( AirLoopNum ).HVACPressueSupplyDeck2ToDemandTolValue;
			AirLoopConvergence( AirLoopNum ).HVACPressueSupplyDeck2ToDemandTolValue( 1 ) = std::abs( Node( OutletNode ).Press - Node( InletNode ).Press );
			AirLoopConvergence( AirLoopNum ).HVACPressueSupplyDeck2ToDemandTolValue( {2,ConvergLogStackDepth} ) = TmpRealARR( {1,ConvergLogStackDepth - 1} );
			if ( AirLoopConvergence( AirLoopNum ).HVACPressueSupplyDeck2ToDemandTolValue( 1 ) > HVACPressToler ) {
				AirLoopConvergence( AirLoopNum ).HVACPressureNotConverged( 3 ) = true;
				OutOfToleranceFlag = true; // Something has changed--resimulate the other side of the loop
			}

		}

		// Always update the new inlet conditions
		Node( InletNode ).Temp = Node( OutletNode ).Temp;
		Node( InletNode ).MassFlowRate = Node( OutletNode ).MassFlowRate;
		Node( InletNode ).MassFlowRateMinAvail = Node( OutletNode ).MassFlowRateMinAvail;
		Node( InletNode ).MassFlowRateMaxAvail = Node( OutletNode ).MassFlowRateMaxAvail;
		Node( InletNode ).Quality = Node( OutletNode ).Quality;
		Node( InletNode ).Press = Node( OutletNode ).Press;
		Node( InletNode ).Enthalpy = Node( OutletNode ).Enthalpy;
		Node( InletNode ).HumRat = Node( OutletNode ).HumRat;

		if ( Contaminant.CO2Simulation ) {
			Node( InletNode ).CO2 = Node( OutletNode ).CO2;
		}

		if ( Contaminant.GenericContamSimulation ) {
			Node( InletNode ).GenContam = Node( OutletNode ).GenContam;
		}

	}

	//***************

	// In-Place Right Shift by 1 of Array Elements
	void
	rshift1( Array1< Real64 > & a )
	{
		assert( a.size_bounded() );
		for ( int i = a.u(), e = a.l(); i > e; --i ) {
			a( i ) = a( i - 1 );
		}
	}

	void
	UpdatePlantLoopInterface(
		int const LoopNum, // The 'inlet/outlet node' loop number
		int const ThisLoopSideNum, // The 'outlet node' LoopSide number
		int const ThisLoopSideOutletNode, // Node number for the inlet of the side that needs the outlet node data
		int const OtherLoopSideInletNode, // Node number for the outlet of the side of the loop just simulated
		bool & OutOfToleranceFlag, // True when the other side of the loop need to be (re)simulated
		int const CommonPipeType
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   October 1998
		//       MODIFIED       na
		//       RE-ENGINEERED  Brent Griffith, Sept. 2010
		//       RE-ENGINEERED  Dan Fisher,     Sept. 2010

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine manages any generic HVAC loop interface.

		// METHODOLOGY EMPLOYED:
		// This is a simple "forward" interface where all of the properties
		// from the outlet of one side of the loop get transfered
		// to the inlet node of the corresponding other side of the loop.
		// Temperatures are 'lagged' by loop capacitance (i.e. a 'tank')
		// between the outlet and inlet nodes.
		// the update from the demand side to the supply side always triggers
		// resimulation of the supply side if any state variable (or energy) is
		// out of tolerance.  Remsimulation of the demand side is only triggered if
		// flow or energy are out of tolerance.  This in effect checks flow and
		// ~.25C temperature difference.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataLoopNode::Node;
		using namespace DataConvergParams;
		using DataPlant::PlantLoop;
		using DataPlant::DemandSide;
		using FluidProperties::GetSpecificHeatGlycol;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "UpdatePlantLoopInterface" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 OldTankOutletTemp;
		Real64 OldOtherLoopSideInletMdot;
		Real64 TankOutletTemp;
		Real64 Cp;
		Real64 MixedOutletTemp;
		int ThisLoopSideInletNode;

		// FLOW:

		auto & convergence( PlantConvergence( LoopNum ) );

		//reset out of tolerance flags
		convergence.PlantMassFlowNotConverged = false;
		convergence.PlantTempNotConverged = false;

		//set the LoopSide inlet node
		ThisLoopSideInletNode = PlantLoop( LoopNum ).LoopSide( ThisLoopSideNum ).NodeNumIn;

		//save the inlet node temp for DeltaEnergy check
		OldOtherLoopSideInletMdot = Node( OtherLoopSideInletNode ).MassFlowRate;
		OldTankOutletTemp = Node( OtherLoopSideInletNode ).Temp;

		//calculate the specific heat
		Cp = GetSpecificHeatGlycol( PlantLoop( LoopNum ).FluidName, OldTankOutletTemp, PlantLoop( LoopNum ).FluidIndex, RoutineName );

		//update the enthalpy
		Node( OtherLoopSideInletNode ).Enthalpy = Cp * Node( OtherLoopSideInletNode ).Temp;

		//update the temperatures and flow rates
		auto & flow_demand_to_supply_tol( convergence.PlantFlowDemandToSupplyTolValue );
		auto & flow_supply_to_demand_tol( convergence.PlantFlowSupplyToDemandTolValue );
		if ( CommonPipeType == 1 || CommonPipeType == 2 ) {
			//update the temperature
			UpdateCommonPipe( LoopNum, ThisLoopSideNum, CommonPipeType, MixedOutletTemp );
			Node( OtherLoopSideInletNode ).Temp = MixedOutletTemp;
			TankOutletTemp = MixedOutletTemp;
			if ( ThisLoopSideNum == DemandSide ) {
				rshift1( flow_demand_to_supply_tol );
				flow_demand_to_supply_tol( 1 ) = std::abs( OldOtherLoopSideInletMdot - Node( OtherLoopSideInletNode ).MassFlowRate );
				if ( flow_demand_to_supply_tol( 1 ) > PlantFlowRateToler ) {
					convergence.PlantMassFlowNotConverged = true;
				}
			} else {
				rshift1( flow_supply_to_demand_tol );
				flow_supply_to_demand_tol( 1 ) = std::abs( OldOtherLoopSideInletMdot - Node( OtherLoopSideInletNode ).MassFlowRate );
				if ( flow_supply_to_demand_tol( 1 ) > PlantFlowRateToler ) {
					convergence.PlantMassFlowNotConverged = true;
				}
			}
			//Set the flow rate.  Continuity requires that the flow rates at the half loop inlet and outlet match
			Node( ThisLoopSideInletNode ).MassFlowRate = Node( ThisLoopSideOutletNode ).MassFlowRate;
			//Update this LoopSide inlet node Min/MaxAvail to this LoopSide outlet node Min/MaxAvail
			Node( ThisLoopSideInletNode ).MassFlowRateMinAvail = Node( ThisLoopSideOutletNode ).MassFlowRateMinAvail;
			Node( ThisLoopSideInletNode ).MassFlowRateMaxAvail = Node( ThisLoopSideOutletNode ).MassFlowRateMaxAvail;

		} else { //no common pipe
			UpdateHalfLoopInletTemp( LoopNum, ThisLoopSideNum, TankOutletTemp );
			//update the temperature
			Node( OtherLoopSideInletNode ).Temp = TankOutletTemp;
			//Set the flow tolerance array
			if ( ThisLoopSideNum == DemandSide ) {
				rshift1( flow_demand_to_supply_tol );
				flow_demand_to_supply_tol( 1 ) = std::abs( Node( ThisLoopSideOutletNode ).MassFlowRate - Node( OtherLoopSideInletNode ).MassFlowRate );
				if ( flow_demand_to_supply_tol( 1 ) > PlantFlowRateToler ) {
					convergence.PlantMassFlowNotConverged = true;
				}
			} else {
				rshift1( flow_supply_to_demand_tol );
				flow_supply_to_demand_tol( 1 ) = std::abs( Node( ThisLoopSideOutletNode ).MassFlowRate - Node( OtherLoopSideInletNode ).MassFlowRate );
				if ( flow_supply_to_demand_tol( 1 ) > PlantFlowRateToler ) {
					convergence.PlantMassFlowNotConverged = true;
				}
			}
			//    PlantFlowTolValue(PlantQuePtr)  = ABS(Node(ThisLoopSideOutletNode)%MassFlowRate-Node(OtherLoopSideInletNode)%MassFlowRate)
			//Set the flow rate
			Node( OtherLoopSideInletNode ).MassFlowRate = Node( ThisLoopSideOutletNode ).MassFlowRate;
			//update the MIN/MAX available flow rates
			Node( OtherLoopSideInletNode ).MassFlowRateMinAvail = Node( ThisLoopSideOutletNode ).MassFlowRateMinAvail;
			Node( OtherLoopSideInletNode ).MassFlowRateMaxAvail = Node( ThisLoopSideOutletNode ).MassFlowRateMaxAvail;
			//update Quality.  DSU? Note: This update assumes that STEAM cannot be used with common pipes.
			Node( OtherLoopSideInletNode ).Quality = Node( ThisLoopSideOutletNode ).Quality;
			//pressure update  DSU? Note: This update assumes that PRESSURE SIMULATION cannot be used with common pipes.
			if ( PlantLoop( LoopNum ).HasPressureComponents ) {
				//Don't update pressure, let the pressure simulation handle pressures
			} else {
				//Do update pressure!
				Node( OtherLoopSideInletNode ).Press = Node( ThisLoopSideOutletNode ).Press;
			}
		}

		//temperature
		if ( ThisLoopSideNum == DemandSide ) {
			auto & temp_demand_to_supply_tol( convergence.PlantTempDemandToSupplyTolValue );
			rshift1( temp_demand_to_supply_tol );
			temp_demand_to_supply_tol( 1 ) = std::abs( OldTankOutletTemp - Node( OtherLoopSideInletNode ).Temp );
			if ( temp_demand_to_supply_tol( 1 ) > PlantTemperatureToler ) {
				convergence.PlantTempNotConverged = true;
			}
		} else {
			auto & temp_supply_to_demand_tol( convergence.PlantTempSupplyToDemandTolValue );
			rshift1( temp_supply_to_demand_tol );
			temp_supply_to_demand_tol( 1 ) = std::abs( OldTankOutletTemp - Node( OtherLoopSideInletNode ).Temp );
			if ( temp_supply_to_demand_tol( 1 ) > PlantTemperatureToler ) {
				convergence.PlantTempNotConverged = true;
			}
		}

		//Set out of tolerance flags
		if ( ThisLoopSideNum == DemandSide ) {
			if ( convergence.PlantMassFlowNotConverged || convergence.PlantTempNotConverged ) {
				OutOfToleranceFlag = true;
			}
		} else {
			if ( convergence.PlantMassFlowNotConverged ) {
				OutOfToleranceFlag = true;
			}
		}

	}

	//***************

	void
	UpdateHalfLoopInletTemp(
		int const LoopNum,
		int const TankInletLoopSide,
		Real64 & TankOutletTemp
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   September 2001
		//       MODIFIED       Simon Rees, July 2007
		//                      Brent Griffith, Feb. 2010, add LoopNum arg
		//       RE-ENGINEERED  Brent Griffith, Sept 2010, generalize for both loop sides
		//                                           add pump heat from other loop
		//                      B.Griffith and L.Gu, Oct 2011, solve via analytical soln, use average over timestep

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine calculates the new loop side inlet temperature
		// based on the previous temperature of the mixed tank, mass flow rate and the new
		// outlet temperature on the supply side.  The temperature does not
		// pass directly across because the loop has some capacitance. It is
		// called separately but used for both supply-to-demand, and demand-to-supply

		// METHODOLOGY EMPLOYED:
		// This uses a analytical solution for changes in the
		// fluid loop temperature.  The user defines some volume of fluid
		// for the loop which gets converted to a fixed amount of mass.
		// The loop side inlet node is modeled as the outlet of a fully mixed
		// tank. Note that this routine is called repeatedly to re calculate
		// loop capacitance based on current plant conditions

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHVACGlobals::TimeStepSys;
		using DataHVACGlobals::SysTimeElapsed;
		using DataLoopNode::Node;
		using DataPlant::PlantLoop;
		using DataGlobals::SecInHour;
		using DataGlobals::TimeStepZone;
		using DataGlobals::TimeStep;
		using DataGlobals::HourOfDay;
		using FluidProperties::GetSpecificHeatGlycol;

		// Locals
		// SUBROUTINE ARGUMENTS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const FracTotLoopMass( 0.5 ); // Fraction of total loop mass assigned to the half loop
		static std::string const RoutineName( "UpdateHalfLoopInletTemp" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int TankOutletLoopSide; // inlet loopsidenumber
		int TankInletNode; // inlet loop side outlet node
		int TankOutletNode; // inlet loop side outlet node
		Real64 TankInletTemp; // temporary variable
		Real64 LastTankOutletTemp; // temporary variable
		Real64 Cp; // specific heat
		Real64 TimeElapsed; // temporary value based on current clock time during simulation, fractional hours

		Real64 TimeStepSeconds;
		Real64 MassFlowRate;
		Real64 PumpHeat;
		Real64 ThisTankMass;
		Real64 TankFinalTemp;
		Real64 TankAverageTemp;

		// FLOW:

		//find tank inlet and outlet nodes
		TankOutletLoopSide = 3 - TankInletLoopSide;
		TankInletNode = PlantLoop( LoopNum ).LoopSide( TankInletLoopSide ).NodeNumOut;
		TankOutletNode = PlantLoop( LoopNum ).LoopSide( TankOutletLoopSide ).NodeNumIn;

		TankInletTemp = Node( TankInletNode ).Temp;

		// This needs to be based on time to deal with system downstepping and repeated timesteps
		TimeElapsed = ( HourOfDay - 1 ) + TimeStep * TimeStepZone + SysTimeElapsed;
		if ( PlantLoop( LoopNum ).LoopSide( TankOutletLoopSide ).TimeElapsed != TimeElapsed ) {
			PlantLoop( LoopNum ).LoopSide( TankOutletLoopSide ).LastTempInterfaceTankOutlet = PlantLoop( LoopNum ).LoopSide( TankOutletLoopSide ).TempInterfaceTankOutlet;
			PlantLoop( LoopNum ).LoopSide( TankOutletLoopSide ).TimeElapsed = TimeElapsed;
		}

		LastTankOutletTemp = PlantLoop( LoopNum ).LoopSide( TankOutletLoopSide ).LastTempInterfaceTankOutlet;

		//calculate the specific heat for the capacitance calculation
		Cp = GetSpecificHeatGlycol( PlantLoop( LoopNum ).FluidName, LastTankOutletTemp, PlantLoop( LoopNum ).FluidIndex, RoutineName );
		//set the fraction of loop mass assigned to each half loop outlet capacitance ('tank') calculation

		//calculate new loop inlet temperature.  The calculation is a simple 'tank' (thermal capacitance) calculation that includes:
		//--half of loop mass.  The other half is accounted for at the other half loop interface
		//--pump heat.  Pump heat for a single loop setpoint with pumps only on the supply side is added at the supply side inlet.
		//   Pump heat for a dual setpoint loop is added to each loop side inlet
		//  The previous tank temperature value is used to prevent accumulation of pump heat during iterations while recalculating
		// tank conditions each call.
		// Analytical solution for ODE, formulated for both final tank temp and average tank temp.

		TimeStepSeconds = TimeStepSys * SecInHour;
		MassFlowRate = Node( TankInletNode ).MassFlowRate;
		PumpHeat = PlantLoop( LoopNum ).LoopSide( TankOutletLoopSide ).TotalPumpHeat;
		ThisTankMass = FracTotLoopMass * PlantLoop( LoopNum ).Mass;

		if ( ThisTankMass <= 0.0 ) { // no mass, no plant loop volume
			if ( MassFlowRate > 0.0 ) {
				TankFinalTemp = TankInletTemp + PumpHeat / ( MassFlowRate * Cp );
				TankAverageTemp = ( TankFinalTemp + LastTankOutletTemp ) / 2.0;
			} else {
				TankFinalTemp = LastTankOutletTemp;
				TankAverageTemp = LastTankOutletTemp;
			}

		} else { // tank has mass
			if ( MassFlowRate > 0.0 ) {
				TankFinalTemp = ( LastTankOutletTemp - ( MassFlowRate * Cp * TankInletTemp + PumpHeat ) / ( MassFlowRate * Cp ) ) * std::exp( -( MassFlowRate * Cp ) / ( ThisTankMass * Cp ) * TimeStepSeconds ) + ( MassFlowRate * Cp * TankInletTemp + PumpHeat ) / ( MassFlowRate * Cp );
				TankAverageTemp = ( ( ThisTankMass * Cp ) / ( MassFlowRate * Cp ) * ( LastTankOutletTemp - ( MassFlowRate * Cp * TankInletTemp + PumpHeat ) / ( MassFlowRate * Cp ) ) * ( 1.0 - std::exp( -( MassFlowRate * Cp ) / ( ThisTankMass * Cp ) * TimeStepSeconds ) ) / TimeStepSeconds + ( MassFlowRate * Cp * TankInletTemp + PumpHeat ) / ( MassFlowRate * Cp ) );
			} else {
				TankFinalTemp = PumpHeat / ( ThisTankMass * Cp ) * TimeStepSeconds + LastTankOutletTemp;
				TankAverageTemp = ( TankFinalTemp + LastTankOutletTemp ) / 2.0;
			}
		}

		//update last tank outlet temperature
		PlantLoop( LoopNum ).LoopSide( TankOutletLoopSide ).TempInterfaceTankOutlet = TankFinalTemp;

		// update report variable
		PlantLoop( LoopNum ).LoopSide( TankOutletLoopSide ).LoopSideInlet_TankTemp = TankAverageTemp;

		TankOutletTemp = TankAverageTemp;

	}

	void
	UpdateCommonPipe(
		int const LoopNum,
		int const TankInletLoopSide,
		int const CommonPipeType,
		Real64 & MixedOutletTemp
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   September 2001
		//       MODIFIED       Simon Rees, July 2007
		//                      Brent Griffith, Feb. 2010, add LoopNum arg
		//       RE-ENGINEERED  Brent Griffith, Sept 2010, generalize for both loop sides
		//                                           add pump heat from other loop
		//                      B.Griffith and L.Gu, Oct 2011, solve via analytical soln, use average over timestep

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine calculates the new loop side inlet temperature
		// based on the previous temperature of the mixed tank, mass flow rate and the new
		// outlet temperature on the supply side.  The temperature does not
		// pass directly across because the loop has some capacitance. It is
		// called separately but used for both supply-to-demand, and demand-to-supply

		// METHODOLOGY EMPLOYED:
		// This uses a analytical solution for changes in the
		// fluid loop temperature.  The user defines some volume of fluid
		// for the loop which gets converted to a fixed amount of mass.
		// The loop side inlet node is modeled as the outlet of a fully mixed
		// tank. Note that this routine is called repeatedly to re calculate
		// loop capacitance based on current plant conditions

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHVACGlobals::TimeStepSys;
		using DataHVACGlobals::SysTimeElapsed;
		using DataLoopNode::Node;
		using DataPlant::PlantLoop;
		using DataPlant::CommonPipe_Single;
		using DataPlant::CommonPipe_TwoWay;
		using DataPlant::DemandSide;
		using DataGlobals::SecInHour;
		using DataGlobals::TimeStepZone;
		using DataGlobals::TimeStep;
		using DataGlobals::HourOfDay;
		using FluidProperties::GetSpecificHeatGlycol;

		// Locals
		// SUBROUTINE ARGUMENTS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "UpdateCommonPipe" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int TankOutletLoopSide; // inlet loopsidenumber
		int TankInletNode; // inlet loop side outlet node
		int TankOutletNode; // inlet loop side outlet node
		Real64 TankInletTemp; // temporary variable
		Real64 LastTankOutletTemp; // temporary variable
		Real64 Cp; // specific heat
		Real64 TimeElapsed; // temporary value based on current clock time during simulation, fractional hours

		Real64 FracTotLoopMass; // Fraction of total loop mass assigned to the half loop
		Real64 TimeStepSeconds;
		Real64 MassFlowRate;
		Real64 PumpHeat;
		Real64 ThisTankMass;
		Real64 TankFinalTemp;
		Real64 TankAverageTemp;

		// FLOW:

		//find tank inlet and outlet nodes
		TankOutletLoopSide = 3 - TankInletLoopSide;
		TankInletNode = PlantLoop( LoopNum ).LoopSide( TankInletLoopSide ).NodeNumOut;
		TankOutletNode = PlantLoop( LoopNum ).LoopSide( TankOutletLoopSide ).NodeNumIn;

		TankInletTemp = Node( TankInletNode ).Temp;

		if ( TankInletLoopSide == DemandSide ) {
			// for common pipe loops, assume 75% of plant loop volume is on the demand side
			FracTotLoopMass = 0.25;
		} else {
			FracTotLoopMass = 0.75;
		}

		// This needs to be based on time to deal with system downstepping and repeated timesteps
		TimeElapsed = ( HourOfDay - 1 ) + TimeStep * TimeStepZone + SysTimeElapsed;
		if ( PlantLoop( LoopNum ).LoopSide( TankOutletLoopSide ).TimeElapsed != TimeElapsed ) {
			PlantLoop( LoopNum ).LoopSide( TankOutletLoopSide ).LastTempInterfaceTankOutlet = PlantLoop( LoopNum ).LoopSide( TankOutletLoopSide ).TempInterfaceTankOutlet;
			PlantLoop( LoopNum ).LoopSide( TankOutletLoopSide ).TimeElapsed = TimeElapsed;
		}

		LastTankOutletTemp = PlantLoop( LoopNum ).LoopSide( TankOutletLoopSide ).LastTempInterfaceTankOutlet;

		//calculate the specific heat for the capacitance calculation
		Cp = GetSpecificHeatGlycol( PlantLoop( LoopNum ).FluidName, LastTankOutletTemp, PlantLoop( LoopNum ).FluidIndex, RoutineName );

		//set the fraction of loop mass assigned to each half loop outlet capacitance ('tank') calculation

		//calculate new loop inlet temperature.  The calculation is a simple 'tank' (thermal capacitance) calculation that includes:
		//--half of loop mass.  The other half is accounted for at the other half loop interface
		//--pump heat.  Pump heat for a single loop setpoint with pumps only on the supply side is added at the supply side inlet.
		//Pump heat for a dual setpoint loop is added to each loop side inlet
		//The previous inlet side temp,'ThisLoopSideTankOutletTemp' is used to prevent accumulation of pump heat during iterations.
		//The placement of the 'tank' for common pipes is *after* the outlet node and *before* the flow split or flow mixing.
		//This requires no logical check in the code since for purposes of temperature calculations, it is identical to the
		//no common pipe case.
		// calculation is separated because for common pipe, a different split for mass fraction is applied
		// The pump heat source is swapped around here compared to no common pipe (so pump heat sort stays on its own side).
		TimeStepSeconds = TimeStepSys * SecInHour;
		MassFlowRate = Node( TankInletNode ).MassFlowRate;
		PumpHeat = PlantLoop( LoopNum ).LoopSide( TankInletLoopSide ).TotalPumpHeat;
		ThisTankMass = FracTotLoopMass * PlantLoop( LoopNum ).Mass;

		if ( ThisTankMass <= 0.0 ) { // no mass, no plant loop volume
			if ( MassFlowRate > 0.0 ) {
				TankFinalTemp = TankInletTemp + PumpHeat / ( MassFlowRate * Cp );
				TankAverageTemp = ( TankFinalTemp + LastTankOutletTemp ) / 2.0;
			} else {
				TankFinalTemp = LastTankOutletTemp;
				TankAverageTemp = LastTankOutletTemp;
			}

		} else { // tank has mass
			if ( MassFlowRate > 0.0 ) {
				TankFinalTemp = ( LastTankOutletTemp - ( MassFlowRate * Cp * TankInletTemp + PumpHeat ) / ( MassFlowRate * Cp ) ) * std::exp( -( MassFlowRate * Cp ) / ( ThisTankMass * Cp ) * TimeStepSeconds ) + ( MassFlowRate * Cp * TankInletTemp + PumpHeat ) / ( MassFlowRate * Cp );
				TankAverageTemp = ( ( ThisTankMass * Cp ) / ( MassFlowRate * Cp ) * ( LastTankOutletTemp - ( MassFlowRate * Cp * TankInletTemp + PumpHeat ) / ( MassFlowRate * Cp ) ) * ( 1.0 - std::exp( -( MassFlowRate * Cp ) / ( ThisTankMass * Cp ) * TimeStepSeconds ) ) / TimeStepSeconds + ( MassFlowRate * Cp * TankInletTemp + PumpHeat ) / ( MassFlowRate * Cp ) );
			} else {

				TankFinalTemp = PumpHeat / ( ThisTankMass * Cp ) * TimeStepSeconds + LastTankOutletTemp;
				TankAverageTemp = ( TankFinalTemp + LastTankOutletTemp ) / 2.0;

			}
		}
		//Common Pipe Simulation
		if ( CommonPipeType == CommonPipe_Single ) {
			ManageSingleCommonPipe( LoopNum, TankOutletLoopSide, TankAverageTemp, MixedOutletTemp );
			//2-way (controlled) common pipe simulation
		} else if ( CommonPipeType == CommonPipe_TwoWay ) {

			ManageTwoWayCommonPipe( LoopNum, TankOutletLoopSide, TankAverageTemp );
			MixedOutletTemp = Node( TankOutletNode ).Temp;
		}

		PlantLoop( LoopNum ).LoopSide( TankOutletLoopSide ).TempInterfaceTankOutlet = TankFinalTemp;

		PlantLoop( LoopNum ).LoopSide( TankOutletLoopSide ).LoopSideInlet_TankTemp = TankAverageTemp;

	}

	void
	ManageSingleCommonPipe(
		int const LoopNum, // plant loop number
		int const LoopSide, // plant loop side number
		Real64 const TankOutletTemp, // inlet temperature to the common pipe passed in from the capacitance calculation
		Real64 & MixedOutletTemp // inlet temperature to the common pipe passed in from the capacitance calculation
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Sankaranarayanan K P
		//       DATE WRITTEN   November 2006
		//       MODIFIED       B. Griffith, Jan 2010 clean up setup to allow mixing common pipe modes
		//                      B. Griffith, Mar 2010 add LoopNum arg and simplify
		//       RE-ENGINEERED  D. Fisher, Sept. 2010
		//                      B. Griffitth, Oct 2011, major rewrite for plant upgrade

		// PURPOSE OF THIS SUBROUTINE:
		// To determine the conditions in common pipe viz., the flow flow temperature and direction of flow.

		// METHODOLOGY EMPLOYED:
		// Determine the flow on both sides of the common pipe. Decide if flow is coming into common pipe
		// or going out of common pipe. After that determine which interface calls the subroutine, i.e. if
		// called from "Demand to Supply" interface or "Supply to Demand" interface. Update the node temperatures
		// accordingly.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::BeginEnvrnFlag;
		using DataLoopNode::Node;
		using namespace DataPlant;
		using DataBranchAirLoopPlant::MassFlowTolerance;

		//SUBROUTINE ARGUMENT DEFINITIONS

		// Locals
		// SUBROUTINE LOCAL VARIABLE DECLARATIONS
		static Real64 MdotPri( 0.0 ); // flow rate on primary side kg/s
		static Real64 MdotSec( 0.0 ); // flow rate on secondary side kg/s
		static Real64 MdotPriRCLeg( 0.0 ); // flow rate of primary recirculation thru common pipe kg/s
		static Real64 MdotSecRCLeg( 0.0 ); // flow rate of secondary recirculation thru common pipe kg/s
		static Real64 TempSecInlet( 0.0 ); // temperature at secondary inlet deg C
		static Real64 TempPriInlet( 0.0 ); // temperature at primary inlet deg C
		static Real64 TempPriOutTankOut( 0.0 );
		static Real64 TempSecOutTankOut( 0.0 );
		static int NodeNumPriOut( 0 );
		static int NodeNumSecOut( 0 );
		static int NodeNumPriIn( 0 );
		static int NodeNumSecIn( 0 );
		int CPFlowDir; // flow direction in single common pipe
		static Array1D_bool MyEnvrnFlag;
		static bool OneTimeData( true );
		Real64 CommonPipeTemp;

		//One time call to set up report variables and set common pipe 'type' flag
		if ( OneTimeData ) {
			if ( ! CommonPipeSetupFinished ) SetupCommonPipes();
			MyEnvrnFlag.dimension( TotNumLoops, true );
			OneTimeData = false;
		}

		//fill local node indexes
		NodeNumPriIn = PlantLoop( LoopNum ).LoopSide( SupplySide ).NodeNumIn;
		NodeNumPriOut = PlantLoop( LoopNum ).LoopSide( SupplySide ).NodeNumOut;
		NodeNumSecIn = PlantLoop( LoopNum ).LoopSide( DemandSide ).NodeNumIn;
		NodeNumSecOut = PlantLoop( LoopNum ).LoopSide( DemandSide ).NodeNumOut;

		if ( MyEnvrnFlag( LoopNum ) && BeginEnvrnFlag ) {
			PlantCommonPipe( LoopNum ).Flow = 0.0;
			PlantCommonPipe( LoopNum ).Temp = 0.0;
			PlantCommonPipe( LoopNum ).FlowDir = NoRecircFlow;
			MyEnvrnFlag( LoopNum ) = false;
		}
		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( LoopNum ) = true;
		}

		// every time inits
		MdotSec = Node( NodeNumSecOut ).MassFlowRate;
		MdotPri = Node( NodeNumPriOut ).MassFlowRate;

		if ( LoopSide == SupplySide ) {
			TempSecOutTankOut = TankOutletTemp;
			TempPriOutTankOut = PlantLoop( LoopNum ).LoopSide( DemandSide ).LoopSideInlet_TankTemp;
		} else {
			TempPriOutTankOut = TankOutletTemp;
			TempSecOutTankOut = PlantLoop( LoopNum ).LoopSide( SupplySide ).LoopSideInlet_TankTemp;
		}

		// first do mass balances and find common pipe flow rate and direction
		if ( MdotPri > MdotSec ) {
			MdotPriRCLeg = MdotPri - MdotSec;
			if ( MdotPriRCLeg < MassFlowTolerance ) {
				MdotPriRCLeg = 0.0;
				CPFlowDir = NoRecircFlow;
			} else {
				CPFlowDir = PrimaryRecirc;
			}
			MdotSecRCLeg = 0.0;
			CommonPipeTemp = TempPriOutTankOut;
		} else if ( MdotPri < MdotSec ) {
			MdotSecRCLeg = MdotSec - MdotPri;
			if ( MdotSecRCLeg < MassFlowTolerance ) {
				MdotSecRCLeg = 0.0;
				CPFlowDir = NoRecircFlow;
			} else {
				CPFlowDir = SecondaryRecirc;
			}
			MdotPriRCLeg = 0.0;
			CommonPipeTemp = TempSecOutTankOut;
		} else { // equal
			MdotPriRCLeg = 0.0;
			MdotSecRCLeg = 0.0;
			CPFlowDir = NoRecircFlow;
			CommonPipeTemp = ( TempPriOutTankOut + TempSecOutTankOut ) / 2.0;
		}

		// now calculate inlet temps

		if ( MdotSec > 0.0 ) {
			TempSecInlet = ( MdotPri * TempPriOutTankOut + MdotSecRCLeg * TempSecOutTankOut - MdotPriRCLeg * TempPriOutTankOut ) / ( MdotSec );
		} else {
			TempSecInlet = TempPriOutTankOut;
		}
		if ( MdotPri > 0.0 ) {
			TempPriInlet = ( MdotSec * TempSecOutTankOut + MdotPriRCLeg * TempPriOutTankOut - MdotSecRCLeg * TempSecOutTankOut ) / ( MdotPri );
		} else {
			TempPriInlet = TempSecOutTankOut;
		}

		//Update the Common Pipe Data structure for reporting purposes.
		PlantCommonPipe( LoopNum ).Flow = max( MdotPriRCLeg, MdotSecRCLeg );
		PlantCommonPipe( LoopNum ).Temp = CommonPipeTemp;
		PlantCommonPipe( LoopNum ).FlowDir = CPFlowDir;
		Node( NodeNumSecIn ).Temp = TempSecInlet;
		Node( NodeNumPriIn ).Temp = TempPriInlet;

		if ( LoopSide == SupplySide ) {
			MixedOutletTemp = TempPriInlet;
		} else {
			MixedOutletTemp = TempSecInlet;
		}

	}

	void
	ManageTwoWayCommonPipe(
		int const LoopNum,
		int const LoopSide,
		Real64 const TankOutletTemp
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   June 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  B. Griffith, Oct 2011.  rewrite

		// PURPOSE OF THIS SUBROUTINE:
		// manage two-way common pipe modeling at half-loop interface

		// METHODOLOGY EMPLOYED:
		// calculate mixed temperatures and various flow rates
		// sequential subsitution of system of equations

		// REFERENCES:
		// reimplementation of CheckTwoWayCommonPipeConditions by Sankaranarayanan K P Jan 2007

		// Using/Aliasing
		using DataGlobals::BeginEnvrnFlag;
		using DataPlant::PlantLoop;
		using DataPlant::SupplySide;
		using DataPlant::DemandSide;
		using DataPlant::TotNumLoops;
		using DataPlant::DeltaTempTol;
		using DataBranchAirLoopPlant::MassFlowTolerance;
		using DataLoopNode::Node;
		using PlantUtilities::SetActuatedBranchFlowRate;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const DemandLedPrimaryInletUpdate( 101 );
		int const DemandLedSecondaryInletUpdate( 102 );
		int const SupplyLedPrimaryInletUpdate( 103 );
		int const SupplyLedSecondaryInletUpdate( 104 );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static Array1D_bool MyEnvrnFlag;
		static bool OneTimeData( true );
		int CurCallingCase; // local temporary
		static Real64 MdotPri( 0.0 ); // flow rate on primary side kg/s
		static Real64 MdotSec( 0.0 ); // flow rate on secondary side kg/s
		static Real64 MdotPriToSec( 0.0 ); // flow rate between primary and secondary side kg/s
		static Real64 MdotPriRCLeg( 0.0 ); // flow rate on primary recirculation common pipe kg/s
		static Real64 MdotSecRCLeg( 0.0 ); // flow rate on secondary recirculation common pipe kg/s
		static Real64 TempSecInlet( 0.0 ); // temperature at secondary inlet deg C
		static Real64 TempPriInlet( 0.0 ); // temperature at primary inlet deg C
		static Real64 TempPriOutTankOut( 0.0 );
		static Real64 TempSecOutTankOut( 0.0 );
		static Real64 TempCPPrimaryCntrlSetPoint( 0.0 );
		// REAL(r64)  :: TempCPCntrlCurrent  = 0.0d0
		static Real64 TempCPSecondaryCntrlSetPoint( 0.0 );
		// REAL(r64)  :: TempCPCntrlCurrent  = 0.0d0
		static int NodeNumPriOut( 0 );
		static int NodeNumSecOut( 0 );
		static int NodeNumPriIn( 0 );
		static int NodeNumSecIn( 0 );

		static int MaxIterLimitCaseA( 8 );
		static int MaxIterLimitCaseB( 4 );

		int loop; // interation loop counter
		//  INTEGER    :: loop2

		// one time setups
		if ( OneTimeData ) {
			if ( ! CommonPipeSetupFinished ) SetupCommonPipes();
			MyEnvrnFlag.dimension( TotNumLoops, true );
			OneTimeData = false;
		}

		//fill local node indexes
		NodeNumPriIn = PlantLoop( LoopNum ).LoopSide( SupplySide ).NodeNumIn;
		NodeNumPriOut = PlantLoop( LoopNum ).LoopSide( SupplySide ).NodeNumOut;
		NodeNumSecIn = PlantLoop( LoopNum ).LoopSide( DemandSide ).NodeNumIn;
		NodeNumSecOut = PlantLoop( LoopNum ).LoopSide( DemandSide ).NodeNumOut;

		// begin environment inits
		if ( MyEnvrnFlag( LoopNum ) && BeginEnvrnFlag ) {
			PlantCommonPipe( LoopNum ).PriToSecFlow = 0.0;
			PlantCommonPipe( LoopNum ).SecToPriFlow = 0.0;
			PlantCommonPipe( LoopNum ).PriCPLegFlow = 0.0;
			PlantCommonPipe( LoopNum ).SecCPLegFlow = 0.0;
			MyEnvrnFlag( LoopNum ) = false;
		}

		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( LoopNum ) = true;
		}

		// every time inits
		MdotSec = Node( NodeNumSecOut ).MassFlowRate; // assume known and fixed by demand side operation
		TempCPPrimaryCntrlSetPoint = Node( NodeNumPriIn ).TempSetPoint;
		TempCPSecondaryCntrlSetPoint = Node( NodeNumSecIn ).TempSetPoint;

		// 6 unknowns follow, fill with current values
		MdotPriToSec = PlantCommonPipe( LoopNum ).PriToSecFlow;
		MdotPriRCLeg = PlantCommonPipe( LoopNum ).PriCPLegFlow;
		MdotSecRCLeg = PlantCommonPipe( LoopNum ).SecCPLegFlow;
		TempSecInlet = Node( NodeNumSecIn ).Temp;
		TempPriInlet = Node( NodeNumPriIn ).Temp;
		MdotPri = Node( NodeNumPriOut ).MassFlowRate; //may or may not be an unknown, If variable speed primary side, then unknown

		if ( LoopSide == SupplySide ) {
			TempSecOutTankOut = TankOutletTemp;
			TempPriOutTankOut = PlantLoop( LoopNum ).LoopSide( DemandSide ).LoopSideInlet_TankTemp;
		} else {
			TempPriOutTankOut = TankOutletTemp;
			TempSecOutTankOut = PlantLoop( LoopNum ).LoopSide( SupplySide ).LoopSideInlet_TankTemp;
		}

		// determine current case
		// which side is being updated
		// commonpipe control point is the inlet of one of the half loops
		CurCallingCase = 0;
		if ( LoopSide == SupplySide ) { //update primary inlet
			if ( PlantLoop( LoopNum ).LoopSide( SupplySide ).InletNodeSetPt && ! PlantLoop( LoopNum ).LoopSide( DemandSide ).InletNodeSetPt ) {
				CurCallingCase = SupplyLedPrimaryInletUpdate;

			} else if ( ! PlantLoop( LoopNum ).LoopSide( SupplySide ).InletNodeSetPt && PlantLoop( LoopNum ).LoopSide( DemandSide ).InletNodeSetPt ) {
				CurCallingCase = DemandLedPrimaryInletUpdate;

			}
		} else { // update secondary inlet
			if ( PlantLoop( LoopNum ).LoopSide( SupplySide ).InletNodeSetPt && ! PlantLoop( LoopNum ).LoopSide( DemandSide ).InletNodeSetPt ) {
				CurCallingCase = SupplyLedSecondaryInletUpdate;

			} else if ( ! PlantLoop( LoopNum ).LoopSide( SupplySide ).InletNodeSetPt && PlantLoop( LoopNum ).LoopSide( DemandSide ).InletNodeSetPt ) {
				CurCallingCase = DemandLedSecondaryInletUpdate;

			}
		}

		{ auto const SELECT_CASE_var( CurCallingCase );

		if ( ( SELECT_CASE_var == SupplyLedPrimaryInletUpdate ) || ( SELECT_CASE_var == SupplyLedSecondaryInletUpdate ) ) {
			// CASE A, Primary/Supply Led
			// six equations and six unknowns (although one has a setpoint)
			for ( loop = 1; loop <= MaxIterLimitCaseA; ++loop ) {

				// eq 1
				if ( std::abs( TempSecOutTankOut - TempCPPrimaryCntrlSetPoint ) > DeltaTempTol ) {
					MdotPriToSec = MdotPriRCLeg * ( TempCPPrimaryCntrlSetPoint - TempPriOutTankOut ) / ( TempSecOutTankOut - TempCPPrimaryCntrlSetPoint );
					if ( MdotPriToSec < MassFlowTolerance ) MdotPriToSec = 0.0;
					if ( MdotPriToSec > MdotSec ) MdotPriToSec = MdotSec;
				} else {
					MdotPriToSec = MdotSec; //  what to do (?)
				}
				// eq. 5
				MdotPriRCLeg = MdotPri - MdotPriToSec;
				if ( MdotPriRCLeg < MassFlowTolerance ) MdotPriRCLeg = 0.0;

				// eq. 4
				MdotSecRCLeg = MdotSec - MdotPriToSec;
				if ( MdotSecRCLeg < MassFlowTolerance ) MdotSecRCLeg = 0.0;

				// eq  6
				if ( ( MdotPriToSec + MdotSecRCLeg ) > MassFlowTolerance ) {
					TempSecInlet = ( MdotPriToSec * TempPriOutTankOut + MdotSecRCLeg * TempSecOutTankOut ) / ( MdotPriToSec + MdotSecRCLeg );
				} else {
					TempSecInlet = TempPriOutTankOut;
				}

				// eq. 3
				if ( ( PlantCommonPipe( LoopNum ).SupplySideInletPumpType == VariableFlow ) && ( CurCallingCase == SupplyLedPrimaryInletUpdate ) ) {
					// MdotPri is a variable to be calculated and flow request needs to be made
					if ( std::abs( TempCPPrimaryCntrlSetPoint ) > DeltaTempTol ) {
						//          Do loop2 = 1, MaxIterLimitCaseA
						//            MdotPri = (MdotSec *  TempSecInlet +  MdotPriRCLeg *TempPriOutTankOut - MdotSecRCLeg * TempSecOutTankOut )  &
						//                          /  (TempPriOutTankOut )

						MdotPri = ( MdotPriRCLeg * TempPriOutTankOut + MdotPriToSec * TempSecOutTankOut ) / ( TempCPPrimaryCntrlSetPoint );

						//   ENDDO
						if ( MdotPri < MassFlowTolerance ) MdotPri = 0.0;
					} else {
						MdotPri = MdotSec;
					}
					SetActuatedBranchFlowRate( MdotPri, NodeNumPriIn, LoopNum, SupplySide, 1, false );
				}

				// eq. 2
				if ( ( MdotPriToSec + MdotPriRCLeg ) > MassFlowTolerance ) {
					TempPriInlet = ( MdotPriToSec * TempSecOutTankOut + MdotPriRCLeg * TempPriOutTankOut ) / ( MdotPriToSec + MdotPriRCLeg );
				} else {
					TempPriInlet = TempSecOutTankOut;
				}

			}
		} else if ( ( SELECT_CASE_var == DemandLedPrimaryInletUpdate ) || ( SELECT_CASE_var == DemandLedSecondaryInletUpdate ) ) {
			// case B. Secondary/demand led

			// six equations and six unknowns (although one has a setpoint)
			for ( loop = 1; loop <= MaxIterLimitCaseB; ++loop ) {
				// eq 1,
				if ( std::abs( TempPriOutTankOut - TempSecOutTankOut ) > DeltaTempTol ) {
					MdotPriToSec = MdotSec * ( TempCPSecondaryCntrlSetPoint - TempSecOutTankOut ) / ( TempPriOutTankOut - TempSecOutTankOut );
					if ( MdotPriToSec < MassFlowTolerance ) MdotPriToSec = 0.0;
					if ( MdotPriToSec > MdotSec ) MdotPriToSec = MdotSec;
				} else {
					MdotPriToSec = MdotSec;
				}

				// eq. 2,
				if ( ( MdotPriToSec + MdotPriRCLeg ) > MassFlowTolerance ) {
					TempPriInlet = ( MdotPriToSec * TempSecOutTankOut + MdotPriRCLeg * TempPriOutTankOut ) / ( MdotPriToSec + MdotPriRCLeg );
				} else {
					TempPriInlet = TempSecOutTankOut;
				}

				// eq. 3
				if ( ( PlantCommonPipe( LoopNum ).SupplySideInletPumpType == VariableFlow ) && ( CurCallingCase == DemandLedPrimaryInletUpdate ) ) {
					// MdotPri is a variable to be calculated and flow request made
					if ( std::abs( TempPriOutTankOut - TempPriInlet ) > DeltaTempTol ) {
						MdotPri = MdotSec * ( TempCPSecondaryCntrlSetPoint - TempSecOutTankOut ) / ( TempPriOutTankOut - TempPriInlet );
						if ( MdotPri < MassFlowTolerance ) MdotPri = 0.0;
					} else {
						MdotPri = MdotSec;
					}
					SetActuatedBranchFlowRate( MdotPri, NodeNumPriIn, LoopNum, SupplySide, 1, false );
				}

				// eq. 4
				MdotSecRCLeg = MdotSec - MdotPriToSec;
				if ( MdotSecRCLeg < MassFlowTolerance ) MdotSecRCLeg = 0.0;

				// eq. 5
				MdotPriRCLeg = MdotPri - MdotPriToSec;
				if ( MdotPriRCLeg < MassFlowTolerance ) MdotPriRCLeg = 0.0;

				// eq  6
				if ( ( MdotPriToSec + MdotSecRCLeg ) > MassFlowTolerance ) {
					TempSecInlet = ( MdotPriToSec * TempPriOutTankOut + MdotSecRCLeg * TempSecOutTankOut ) / ( MdotPriToSec + MdotSecRCLeg );
				} else {
					TempSecInlet = TempPriOutTankOut;
				}

			}

		} else {
			//???      CALL ShowFatalError('ManageTwoWayCommonPipe: Calling Case Fall Through')

		}}

		//update
		PlantCommonPipe( LoopNum ).PriToSecFlow = MdotPriToSec;
		PlantCommonPipe( LoopNum ).SecToPriFlow = MdotPriToSec;
		PlantCommonPipe( LoopNum ).PriCPLegFlow = MdotPriRCLeg;
		PlantCommonPipe( LoopNum ).SecCPLegFlow = MdotSecRCLeg;
		Node( NodeNumSecIn ).Temp = TempSecInlet;
		Node( NodeNumPriIn ).Temp = TempPriInlet;

	}

	void
	SetupCommonPipes()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   Jan. 2010
		//       MODIFIED       B. Griffith Oct. 2011
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// collect allocation, outputs, and other set up for common pipes

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataPlant;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int CurLoopNum; // local do loop counter

		PlantCommonPipe.allocate( TotNumLoops );

		for ( CurLoopNum = 1; CurLoopNum <= TotNumLoops; ++CurLoopNum ) {

			// reference to easily lookup the first item once
			auto & first_demand_component_typenum( PlantLoop( CurLoopNum ).LoopSide( DemandSide ).Branch( 1 ).Comp( 1 ).TypeOf_Num );
			auto & first_supply_component_typenum( PlantLoop( CurLoopNum ).LoopSide( SupplySide ).Branch( 1 ).Comp( 1 ).TypeOf_Num );

			{ auto const SELECT_CASE_var( PlantLoop( CurLoopNum ).CommonPipeType );
			if ( SELECT_CASE_var == CommonPipe_No ) {
				PlantCommonPipe( CurLoopNum ).CommonPipeType = CommonPipe_No;

			} else if ( SELECT_CASE_var == CommonPipe_Single ) { //Uncontrolled ('single') common pipe
				PlantCommonPipe( CurLoopNum ).CommonPipeType = CommonPipe_Single;
				SetupOutputVariable( "Plant Common Pipe Mass Flow Rate [Kg/s]", PlantCommonPipe( CurLoopNum ).Flow, "System", "Average", PlantLoop( CurLoopNum ).Name );
				SetupOutputVariable( "Plant Common Pipe Temperature [C]", PlantCommonPipe( CurLoopNum ).Temp, "System", "Average", PlantLoop( CurLoopNum ).Name );
				SetupOutputVariable( "Plant Common Pipe Flow Direction Status []", PlantCommonPipe( CurLoopNum ).FlowDir, "System", "Average", PlantLoop( CurLoopNum ).Name );

				if ( first_supply_component_typenum == TypeOf_PumpVariableSpeed ) {
					// If/when the model supports variable-pumping primary, this can be removed.
					ShowWarningError( "SetupCommonPipes: detected variable speed pump on supply inlet of CommonPipe plant loop" );
					ShowContinueError( "Occurs on plant loop name = " + PlantLoop( CurLoopNum ).Name );
					ShowContinueError( "The common pipe model does not support varying the flow rate on the primary/supply side" );
					ShowContinueError( "The primary/supply side will operate as if constant speed, and the simulation continues" );

				}

			} else if ( SELECT_CASE_var == CommonPipe_TwoWay ) { //Controlled ('two-way') common pipe
				PlantCommonPipe( CurLoopNum ).CommonPipeType = CommonPipe_TwoWay;
				SetupOutputVariable( "Plant Common Pipe Primary Mass Flow Rate [kg/s]", PlantCommonPipe( CurLoopNum ).PriCPLegFlow, "System", "Average", PlantLoop( CurLoopNum ).Name );
				SetupOutputVariable( "Plant Common Pipe Secondary Mass Flow Rate [kg/s]", PlantCommonPipe( CurLoopNum ).SecCPLegFlow, "System", "Average", PlantLoop( CurLoopNum ).Name );
				SetupOutputVariable( "Plant Common Pipe Primary to Secondary Mass Flow Rate [kg/s]", PlantCommonPipe( CurLoopNum ).PriToSecFlow, "System", "Average", PlantLoop( CurLoopNum ).Name );
				SetupOutputVariable( "Plant Common Pipe Secondary to Primary Mass Flow Rate [kg/s]", PlantCommonPipe( CurLoopNum ).SecToPriFlow, "System", "Average", PlantLoop( CurLoopNum ).Name );

				// check type of pump on supply side inlet
				if ( first_supply_component_typenum == TypeOf_PumpConstantSpeed ) {
					PlantCommonPipe( CurLoopNum ).SupplySideInletPumpType = ConstantFlow;
				} else if ( first_supply_component_typenum == TypeOf_PumpVariableSpeed ) {
					PlantCommonPipe( CurLoopNum ).SupplySideInletPumpType = VariableFlow;
					// If/when the model supports variable-pumping primary, this can be removed.
					ShowWarningError( "SetupCommonPipes: detected variable speed pump on supply inlet of TwoWayCommonPipe plant loop" );
					ShowContinueError( "Occurs on plant loop name = " + PlantLoop( CurLoopNum ).Name );
					ShowContinueError( "The common pipe model does not support varying the flow rate on the primary/supply side" );
					ShowContinueError( "The primary/supply side will operate as if constant speed, and the simulation continues" );

				}
				// check type of pump on demand side inlet
				if ( first_demand_component_typenum == TypeOf_PumpConstantSpeed ) {
					PlantCommonPipe( CurLoopNum ).DemandSideInletPumpType = ConstantFlow;
				} else if ( first_demand_component_typenum == TypeOf_PumpVariableSpeed ) {
					PlantCommonPipe( CurLoopNum ).DemandSideInletPumpType = VariableFlow;
				}

			}}
		}

		CommonPipeSetupFinished = true;

	}

} // HVACInterfaceManager

} // EnergyPlus
