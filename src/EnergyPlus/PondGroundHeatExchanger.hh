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

#ifndef PondGroundHeatExchanger_hh_INCLUDED
#define PondGroundHeatExchanger_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace PondGroundHeatExchanger {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS
	extern Real64 const SmallNum; // Very small number to avoid div0 errors
	extern Real64 const StefBoltzmann; // Stefan-Boltzmann constant
	//  REAL(r64), PARAMETER :: KelvinConv    = KelvinConv           ! Conversion from Celsius to Kelvin

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	// utility variables initialized once
	extern int NumOfPondGHEs; // Number of pond ground heat exchangers
	// Utility variables - initialized for each instance of a pond
	extern Real64 InletTemp; // water inlet temperature
	extern Real64 OutletTemp; // water outlet temperature
	extern Real64 FlowRate; // water mass flow rate
	extern Real64 HeatTransRate; // total heat transfer rate, Watts
	extern Real64 PondTemp; // pond temperature
	extern Real64 PastPondTemp; // past pond temperature
	extern Real64 PondArea; // pond surface area
	extern Real64 PondDepth; // pond depth
	extern Real64 TubeInDiameter; // hydronic tube inside diameter
	extern Real64 TubeOutDiameter; // hydronic tube outside diameter
	extern Real64 TubeConductivity; // hydronic tube thermal conductivity
	extern Real64 GrndConductivity; // ground thermal conductivity
	extern Real64 Concentration; // fluid/glycol concentration 0.0-1.0 proportion.
	extern Real64 CircLength; // length of each circuit
	extern int NumCircuits; // number of circuits in total
	extern int InletNodeNum; // inlet node number
	extern int OutletNodeNum; // oulet node number
	extern int WaterIndex; // Fluid index for pond water
	extern bool NoDeepGroundTempObjWarning; // This will cause a warning to be issued if no "deep" ground
	// temperature object was input.
	extern Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE PlantPondGroundHeatExchangers

	// Types

	struct PondGroundHeatExchangerData
	{
		// Members
		// Input data
		std::string Name; // name of pond GHE
		std::string InletNode; // pond inlet fluid node
		std::string OutletNode; // pond outlet fluid node
		Real64 DesignMassFlowRate; // design flow rate of circulating fluid
		Real64 DesignCapacity; // design cooling capacity of pond at
		Real64 Depth; // depth of pond
		Real64 Area; // area of pond
		Real64 TubeInDiameter; // hydronic tube inside diameter
		Real64 TubeOutDiameter; // hydronic tube outside diameter
		Real64 TubeConductivity; // hydronic tube thermal conductivity
		Real64 GrndConductivity; // ground thermal conductivity
		Real64 CircuitLength; // length of each circuit
		Real64 BulkTemperature; // current pond bulk temperature
		Real64 PastBulkTemperature; // past pond bulk temperature
		int NumCircuits; // number of circuits in total
		int InletNodeNum; // inlet node number
		int OutletNodeNum; // oulet node number
		int FrozenErrIndex; // for recurring warnings
		int ConsecutiveFrozen; // count of time steps consecutive frozen
		//loop topology variables
		int LoopNum;
		int LoopSideNum;
		int BranchNum;
		int CompNum;

		// Default Constructor
		PondGroundHeatExchangerData() :
			DesignMassFlowRate( 0.0 ),
			DesignCapacity( 0.0 ),
			Depth( 0.0 ),
			Area( 0.0 ),
			TubeInDiameter( 0.0 ),
			TubeOutDiameter( 0.0 ),
			TubeConductivity( 0.0 ),
			GrndConductivity( 0.0 ),
			CircuitLength( 0.0 ),
			BulkTemperature( 0.0 ),
			PastBulkTemperature( 0.0 ),
			NumCircuits( 0 ),
			InletNodeNum( 0 ),
			OutletNodeNum( 0 ),
			FrozenErrIndex( 0 ),
			ConsecutiveFrozen( 0 ),
			LoopNum( 0 ),
			LoopSideNum( 0 ),
			BranchNum( 0 ),
			CompNum( 0 )
		{}

	};

	struct PondGroundHeatExchangerReport
	{
		// Members
		// Report data
		Real64 InletTemp; // fluid inlet temperature
		Real64 OutletTemp; // fluid outlet temperature
		Real64 MassFlowRate; // fluid mass flow rate
		Real64 PondTemp; // pond bulk temperature
		Real64 HeatTransferRate; // total fluid heat transfer rate, Watts
		Real64 Energy; // cumulative energy, Joules

		// Default Constructor
		PondGroundHeatExchangerReport()
		{}

	};

	// Object Data
	extern Array1D< PondGroundHeatExchangerData > PondGHE;
	extern Array1D< PondGroundHeatExchangerReport > PondGHEReport;

	// Functions

	void
	SimPondGroundHeatExchanger(
		std::string const & CompName, // name of the pond GHE
		int & CompIndex, // index in local derived types
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		bool const RunFlag, // TRUE if equipment turned on by loop operation scheme
		bool & InitLoopEquip,
		Real64 & MaxLoad,
		Real64 & MinLoad,
		Real64 & OptLoad
	);

	//==============================================================================

	void
	GetPondGroundHeatExchanger();

	//==============================================================================

	void
	InitPondGroundHeatExchanger(
		int const PondGHENum, // component number
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		bool const RunFlag // TRUE if equipment scheduled to operate
	);

	//==============================================================================

	void
	CalcPondGroundHeatExchanger( int const PondGHENum ); // Number of the Pond GHE

	//==============================================================================

	Real64
	CalcTotalFLux(
		Real64 const PondBulkTemp, // pond temp for this flux calculation
		int const PondGHENum // Number of the Pond GHE
	);

	//==============================================================================

	Real64
	CalcSolarFlux();

	//==============================================================================

	Real64
	CalcEffectiveness(
		Real64 const InsideTemperature, // Temperature of fluid in pipe circuit, in C
		Real64 const PondTemperature, // Temperature of pond water (i.e. outside the pipe), in C
		Real64 const MassFlowRate, // Mass flow rate, in kg/s
		int const PondGHENum // Number of the Pond GHE
	);

	//==============================================================================

	void
	UpdatePondGroundHeatExchanger( int const PondGHENum ); // Index for the pond

	//==============================================================================

	void
	ReportPondGroundHeatExchanger( int const PondGHENum ); // Index for the pond under consideration

} // PondGroundHeatExchanger

} // EnergyPlus

#endif
