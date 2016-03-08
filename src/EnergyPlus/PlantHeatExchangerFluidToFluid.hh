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

#ifndef PlantHeatExchangerFluidToFluid_hh_INCLUDED
#define PlantHeatExchangerFluidToFluid_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace PlantHeatExchangerFluidToFluid {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:
	extern int const CrossFlowBothUnMixed;
	extern int const CrossFlowBothMixed;
	extern int const CrossFlowSupplyLoopMixedDemandLoopUnMixed;
	extern int const CrossFlowSupplyLoopUnMixedDemandLoopMixed;
	extern int const CounterFlow;
	extern int const ParallelFlow;
	extern int const Ideal;

	extern int const UncontrolledOn;
	extern int const OperationSchemeModulated;
	extern int const OperationSchemeOnOff;
	extern int const HeatingSetPointModulated;
	extern int const HeatingSetPointOnOff;
	extern int const CoolingSetPointModulated;
	extern int const CoolingSetPointOnOff;
	extern int const DualDeadBandSetPointModulated;
	extern int const DualDeadBandSetPointOnOff;
	extern int const CoolingDifferentialOnOff;
	extern int const CoolingSetPointOnOffWithComponentOverride;
	extern int const TrackComponentOnOff;

	extern int const WetBulbTemperature;
	extern int const DryBulbTemperature;
	extern int const LoopTemperature;

	extern int const HeatingSupplySideLoop;
	extern int const CoolingSupplySideLoop;

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	extern std::string ComponentClassName;
	extern int NumberOfPlantFluidHXs;
	extern bool GetInput;
	extern Array1D_bool CheckFluidHXs;

	// SUBROUTINE SPECIFICATIONS FOR MODULE

	// Types

	struct PlantConnectionStruct
	{
		// Members
		int LoopNum; // plant loop connection index
		int LoopSideNum; // plant loop side connection index
		int BranchNum; // plant loop branch connection index
		int CompNum; // plant loop component connection index
		int InletNodeNum; // plant loop inlet node index
		int OutletNodeNum; // plant loop outlet node index
		Real64 MassFlowRateMin; // minimum (hardware) flow rate for component [kg/s]
		Real64 MassFlowRateMax; // maximum (hardware) flow rate for component [kg/s]
		Real64 DesignVolumeFlowRate; // design flow rate [m3/s]
		bool DesignVolumeFlowRateWasAutoSized; // true if design flow rate was autosize on input
		Real64 MyLoad; // current load request of supply equip for op scheme control[W]
		Real64 MinLoad; // reports back size for load dispatch routines [W]
		Real64 MaxLoad; // reports back size for load dispatch [W]
		Real64 OptLoad; // reports back size for load dispatch [W]
		Real64 InletTemp; // current inlet fluid temperature [C]
		Real64 InletMassFlowRate; // current inlet mass flow rate [kg/s]
		Real64 OutletTemp; // componenent outlet temperature [C]

		// Default Constructor
		PlantConnectionStruct() :
			LoopNum( 0 ),
			LoopSideNum( 0 ),
			BranchNum( 0 ),
			CompNum( 0 ),
			InletNodeNum( 0 ),
			OutletNodeNum( 0 ),
			MassFlowRateMin( 0.0 ),
			MassFlowRateMax( 0.0 ),
			DesignVolumeFlowRate( 0.0 ),
			DesignVolumeFlowRateWasAutoSized( false ),
			MyLoad( 0.0 ),
			MinLoad( 0.0 ),
			MaxLoad( 0.0 ),
			OptLoad( 0.0 ),
			InletTemp( 0.0 ),
			InletMassFlowRate( 0.0 ),
			OutletTemp( 0.0 )
		{}

	};

	struct PlantLocatorStruct
	{
		// Members
		int LoopNum; // plant loop connection index
		int LoopSideNum; // plant loop side connection index
		int BranchNum; // plant loop branch connection index
		int CompNum; // plant loop component connection index
		int InletNodeNum; // plant loop inlet node index

		// Default Constructor
		PlantLocatorStruct() :
			LoopNum( 0 ),
			LoopSideNum( 0 ),
			BranchNum( 0 ),
			CompNum( 0 ),
			InletNodeNum( 0 )
		{}

	};

	struct HeatExchangerStruct
	{
		// Members
		std::string Name;
		int AvailSchedNum;
		int HeatExchangeModelType;
		Real64 UA;
		bool UAWasAutoSized; // true is UA was autosized on input
		int ControlMode;
		int SetPointNodeNum;
		Real64 TempControlTol;
		int ControlSignalTemp;
		Real64 MinOperationTemp;
		Real64 MaxOperationTemp;
		PlantConnectionStruct DemandSideLoop; // plant connections and data for the side of HX connected to demand side
		PlantConnectionStruct SupplySideLoop;
		std::string HeatTransferMeteringEndUse;
		std::string ComponentUserName; // user name for control-associated  component
		std::string ComponentClassName; // object class name for control-associated component
		int ComponentTypeOfNum;
		PlantLocatorStruct OtherCompSupplySideLoop;
		PlantLocatorStruct OtherCompDemandSideLoop;
		Real64 SizingFactor;
		Real64 HeatTransferRate;
		Real64 HeatTransferEnergy;
		Real64 Effectiveness;
		Real64 OperationStatus;
		int DmdSideModulatSolvNoConvergeErrorCount;
		int DmdSideModulatSolvNoConvergeErrorIndex;
		int DmdSideModulatSolvFailErrorCount;
		int DmdSideModulatSolvFailErrorIndex;

		// Default Constructor
		HeatExchangerStruct() :
			AvailSchedNum( 0 ),
			HeatExchangeModelType( 0 ),
			UA( 0.0 ),
			UAWasAutoSized( false ),
			ControlMode( 0 ),
			SetPointNodeNum( 0 ),
			TempControlTol( 0.0 ),
			ControlSignalTemp( 0 ),
			MinOperationTemp( -99999.0 ),
			MaxOperationTemp( 99999.0 ),
			ComponentTypeOfNum( 0 ),
			SizingFactor( 1.0 ),
			HeatTransferRate( 0.0 ),
			HeatTransferEnergy( 0.0 ),
			Effectiveness( 0.0 ),
			OperationStatus( 0.0 ),
			DmdSideModulatSolvNoConvergeErrorCount( 0 ),
			DmdSideModulatSolvNoConvergeErrorIndex( 0 ),
			DmdSideModulatSolvFailErrorCount( 0 ),
			DmdSideModulatSolvFailErrorIndex( 0 )
		{}

	};

	// Object Data
	extern Array1D< HeatExchangerStruct > FluidHX;

	// Functions

	void
	SimFluidHeatExchanger(
		int const LoopNum, // plant loop sim call originated from
		int const LoopSideNum, // plant loop side sim call originated from
		std::string const & EquipType, // type of equipment, 'PlantComponent:UserDefined'
		std::string const & EquipName, // user name for component
		int & CompIndex,
		bool & InitLoopEquip,
		Real64 const MyLoad,
		Real64 & MaxCap,
		Real64 & MinCap,
		Real64 & OptCap,
		bool const FirstHVACIteration
	);

	void
	GetFluidHeatExchangerInput();

	void
	InitFluidHeatExchanger(
		int const CompNum,
		int const LoopNum
	);

	void
	SizeFluidHeatExchanger( int const CompNum );

	void
	ControlFluidHeatExchanger(
		int const CompNum,
		int const LoopNum,
		Real64 const MyLoad,
		bool const FirstHVACIteration
	);

	void
	CalcFluidHeatExchanger(
		int const CompNum,
		Real64 const SupSideMdot, // mass flow rate of fluid entering from supply side loop
		Real64 const DmdSideMdot // mass flow rate of fluid entering from demand side loop
	);

	void
	FindHXDemandSideLoopFlow(
		int const CompNum,
		Real64 const TargetSupplySideLoopLeavingTemp,
		int const HXActionMode
	);

	Real64
	HXDemandSideLoopFlowResidual(
		Real64 const DmdSideMassFlowRate,
		Array1< Real64 > const & Par // Par(1) = HX index number
	);

	void
	UpdateFluidHeatExchanger( int const CompNum );

	void
	ReportFluidHeatExchanger( int const CompNum );

} // PlantHeatExchangerFluidToFluid

} // EnergyPlus

#endif
