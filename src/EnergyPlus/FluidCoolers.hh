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

#ifndef FluidCoolers_hh_INCLUDED
#define FluidCoolers_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace FluidCoolers {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:
	extern std::string const cFluidCooler_SingleSpeed;
	extern std::string const cFluidCooler_TwoSpeed;

	extern int const PIM_NominalCapacity;
	extern int const PIM_UFactor;

	extern int const FluidCooler_SingleSpeed;
	extern int const FluidCooler_TwoSpeed;

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	extern int NumSimpleFluidCoolers; // Number of similar fluid coolers

	// The following block of variables are used to carry model results for a fluid cooler instance
	// across sim, update, and report routines.  Simulation manager must be careful
	// in models with multiple fluid coolers.

	extern Real64 InletWaterTemp; // CW temperature at fluid cooler inlet
	extern Real64 OutletWaterTemp; // CW temperature at fluid cooler outlet
	extern int WaterInletNode; // Node number at fluid cooler inlet
	extern int WaterOutletNode; // Node number at fluid cooler outlet
	extern Real64 WaterMassFlowRate; // WaterMassFlowRate through fluid cooler
	//DSU this is plant level stuff now  :: FluidCoolerMassFlowRateMax     = 0.0    ! Max Hardware Mass Flow Rate
	//DSU this is plant level stuff now  :: FluidCoolerMassFlowRateMin     = 0.0    ! Min Hardware Mass Flow Rate
	//DSU this is plant level stuff now  :: LoopMassFlowRateMaxAvail = 0.0    ! Max Loop Mass Flow Rate available
	//DSU this is plant level stuff now  :: LoopMassFlowRateMinAvail = 0.0    ! Min Loop Mass Flow Rate available
	extern Real64 Qactual; // Fluid cooler heat transfer
	extern Real64 FanPower; // Fluid cooler fan power used

	extern Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE CondenserLoopFluidCoolers

	// Driver/Manager Routines

	// Get Input routines for module

	// Initialization routines for module
	// also, calculates UA based on nominal capacity input(s)

	// Update routines to check convergence and update nodes

	// Types

	struct FluidCoolerspecs
	{
		// Members
		std::string Name; // User identifier
		std::string FluidCoolerType; // Type of fluid cooler
		int FluidCoolerType_Num;
		int PerformanceInputMethod_Num;
		bool Available; // need an array of logicals--load identifiers of available equipment
		bool ON; // Simulate the machine at it's operating part load ratio
		Real64 DesignWaterFlowRate; // Design water flow rate through the fluid cooler [m3/s]
		bool DesignWaterFlowRateWasAutoSized; // true if design water rate was autosize on input
		Real64 DesWaterMassFlowRate; // Design water flow rate through the fluid cooler [kg/s]
		Real64 HighSpeedAirFlowRate; // Air flow rate through the fluid cooler at high speed [m3/s]
		bool HighSpeedAirFlowRateWasAutoSized; //true if high speed air rate was autosize on input
		Real64 HighSpeedFanPower; // Fan power at high fan speed [W]
		bool HighSpeedFanPowerWasAutoSized; // true if high fan power was autosize on input
		Real64 HighSpeedFluidCoolerUA; // UA of fluid cooler at high fan speed [W/C]
		bool HighSpeedFluidCoolerUAWasAutoSized; // true if high speed UA was autosized on input
		Real64 LowSpeedAirFlowRate; // Air flow rate through fluid cooler at low speed [m3/s]
		bool LowSpeedAirFlowRateWasAutoSized; // true if low speed air rate was autosize on input
		Real64 LowSpeedAirFlowRateSizingFactor; // sizing factor for low speed air flow rate []
		Real64 LowSpeedFanPower; // Fan power at low fan speed [W]
		bool LowSpeedFanPowerWasAutoSized; // true if low speed fan power set to autosize on input
		Real64 LowSpeedFanPowerSizingFactor; // sizing factor for low speed fan power []
		Real64 LowSpeedFluidCoolerUA; // UA of fluid cooler at low fan speed [W/C]
		bool LowSpeedFluidCoolerUAWasAutoSized; //true if low speed UA set to autosize on input
		Real64 LowSpeedFluidCoolerUASizingFactor; // sizing factor for low speed UA []
		Real64 DesignEnteringWaterTemp; // Entering water temperature at design conditions
		Real64 DesignLeavingWaterTemp; // Entering water temperature at design conditions
		Real64 DesignEnteringAirTemp; // Entering water temperature at design conditions
		Real64 DesignEnteringAirWetBulbTemp; // Entering water temperature at design condition
		Real64 FluidCoolerMassFlowRateMultiplier; // Maximum fluid cooler flow rate is this multiplier * design flow rate
		Real64 FluidCoolerNominalCapacity; // Nominal capacity of the fluid cooler [W] at high speed
		Real64 FluidCoolerLowSpeedNomCap; // Nominal capacity of the fluid cooler [W] at low speed
		bool FluidCoolerLowSpeedNomCapWasAutoSized; // true if previous was set to autosize on input
		Real64 FluidCoolerLowSpeedNomCapSizingFactor; // sizing factor for low speed capacity []
		int WaterInletNodeNum; // Node number on the water inlet side of the fluid cooler
		int WaterOutletNodeNum; // Node number on the water outlet side of the fluid cooler
		int OutdoorAirInletNodeNum; // Node number of outdoor air inlet for the fluid cooler
		int HighMassFlowErrorCount; // Counter when mass flow rate is > Design*FluidCoolerMassFlowRateMultiplier
		int HighMassFlowErrorIndex; // Index for high mass flow recurring error message
		int OutletWaterTempErrorCount; // Counter when outlet water temperature is < minimum allowed temperature
		int OutletWaterTempErrorIndex; // Index for outlet water temperature recurring error message
		int SmallWaterMassFlowErrorCount; // Counter when water mass flow rate is very small
		int SmallWaterMassFlowErrorIndex; // Index for very small water mass flow rate recurring error message
		int WMFRLessThanMinAvailErrCount; // Counter when water mass flow rate is less than minimum available
		int WMFRLessThanMinAvailErrIndex; // Index for water mass flow rate less than minavail recurring message
		int WMFRGreaterThanMaxAvailErrCount; // Counter when water mass flow rate is greater than minimum available
		int WMFRGreaterThanMaxAvailErrIndex; // Index for water mass flow rate > minavail recurring message
		//loop topology variables
		int LoopNum;
		int LoopSideNum;
		int BranchNum;
		int CompNum;

		// Default Constructor
		FluidCoolerspecs() :
			FluidCoolerType_Num( 0 ),
			PerformanceInputMethod_Num( 0 ),
			Available( true ),
			ON( true ),
			DesignWaterFlowRate( 0.0 ),
			DesignWaterFlowRateWasAutoSized( false ),
			DesWaterMassFlowRate( 0.0 ),
			HighSpeedAirFlowRate( 0.0 ),
			HighSpeedAirFlowRateWasAutoSized( false ),
			HighSpeedFanPower( 0.0 ),
			HighSpeedFanPowerWasAutoSized( false ),
			HighSpeedFluidCoolerUA( 0.0 ),
			HighSpeedFluidCoolerUAWasAutoSized( false ),
			LowSpeedAirFlowRate( 0.0 ),
			LowSpeedAirFlowRateWasAutoSized( false ),
			LowSpeedAirFlowRateSizingFactor( 0.0 ),
			LowSpeedFanPower( 0.0 ),
			LowSpeedFanPowerWasAutoSized( false ),
			LowSpeedFanPowerSizingFactor( 0.0 ),
			LowSpeedFluidCoolerUA( 0.0 ),
			LowSpeedFluidCoolerUAWasAutoSized( false ),
			LowSpeedFluidCoolerUASizingFactor( 0.0 ),
			DesignEnteringWaterTemp( 0.0 ),
			DesignLeavingWaterTemp( 0.0 ),
			DesignEnteringAirTemp( 0.0 ),
			DesignEnteringAirWetBulbTemp( 0.0 ),
			FluidCoolerMassFlowRateMultiplier( 0.0 ),
			FluidCoolerNominalCapacity( 0.0 ),
			FluidCoolerLowSpeedNomCap( 0.0 ),
			FluidCoolerLowSpeedNomCapWasAutoSized( false ),
			FluidCoolerLowSpeedNomCapSizingFactor( 0.0 ),
			WaterInletNodeNum( 0 ),
			WaterOutletNodeNum( 0 ),
			OutdoorAirInletNodeNum( 0 ),
			HighMassFlowErrorCount( 0 ),
			HighMassFlowErrorIndex( 0 ),
			OutletWaterTempErrorCount( 0 ),
			OutletWaterTempErrorIndex( 0 ),
			SmallWaterMassFlowErrorCount( 0 ),
			SmallWaterMassFlowErrorIndex( 0 ),
			WMFRLessThanMinAvailErrCount( 0 ),
			WMFRLessThanMinAvailErrIndex( 0 ),
			WMFRGreaterThanMaxAvailErrCount( 0 ),
			WMFRGreaterThanMaxAvailErrIndex( 0 ),
			LoopNum( 0 ),
			LoopSideNum( 0 ),
			BranchNum( 0 ),
			CompNum( 0 )
		{}

	};

	struct FluidCoolerInletConds
	{
		// Members
		Real64 WaterTemp; // Fluid cooler water inlet temperature (C)
		Real64 AirTemp; // Fluid cooler air inlet dry-bulb temperature (C)
		Real64 AirWetBulb; // Fluid cooler air inlet wet-bulb temperature (C)
		Real64 AirPress; // Fluid cooler air barometric pressure
		Real64 AirHumRat; // Fluid cooler air inlet humidity ratio (kg/kg)

		// Default Constructor
		FluidCoolerInletConds() :
			WaterTemp( 0.0 ),
			AirTemp( 0.0 ),
			AirWetBulb( 0.0 ),
			AirPress( 0.0 ),
			AirHumRat( 0.0 )
		{}
	};

	struct ReportVars
	{
		// Members
		Real64 InletWaterTemp; // Fluid cooler inlet water temperature (C)
		Real64 OutletWaterTemp; // Fluid cooler outlet water temperature (C)
		Real64 WaterMassFlowRate; // Fluid cooler water mass flow rate (m3/s)
		Real64 Qactual; // Fluid cooler heat rejection rate (W)
		Real64 FanPower; // Fluid cooler fan power (W)
		Real64 FanEnergy; // Fluid cooler fan energy consumption (J)

		// Default Constructor
		ReportVars() :
			InletWaterTemp( 0.0 ),
			OutletWaterTemp( 0.0 ),
			WaterMassFlowRate( 0.0 ),
			Qactual( 0.0 ),
			FanPower( 0.0 ),
			FanEnergy( 0.0 )
		{}
	};

	// Object Data
	extern Array1D< FluidCoolerspecs > SimpleFluidCooler; // dimension to number of machines
	extern Array1D< FluidCoolerInletConds > SimpleFluidCoolerInlet; // inlet conditions
	extern Array1D< ReportVars > SimpleFluidCoolerReport; // report variables

	// Functions
	bool
	TestFluidCoolerSingleSpeedInputForDesign(
		std::string const & cCurrentModuleObject,
		Array1D<std::string> const &  AlphArray,
		Array1D<std::string> const & cNumericFieldNames,
		Array1D<std::string> const & cAlphaFieldNames,
		int const &	FluidCoolerNum
		);

	bool
	TestFluidCoolerTwoSpeedInputForDesign(
		std::string const & cCurrentModuleObject,
		Array1D<std::string> const &  AlphArray,
		Array1D<std::string> const & cNumericFieldNames,
		Array1D<std::string> const & cAlphaFieldNames,
		int const &	FluidCoolerNum
	);

	void
	SimFluidCoolers(
		std::string & FluidCoolerType,
		std::string & FluidCoolerName,
		int & CompIndex,
		bool & RunFlag,
		bool const InitLoopEquip,
		Real64 & MaxCap,
		Real64 & MinCap,
		Real64 & OptCap
	);

	// End CondenserLoopFluidCoolers Module Driver Subroutines
	//******************************************************************************

	// Beginning of CondenserLoopFluidCoolers Module Get Input subroutines
	//******************************************************************************

	void
	GetFluidCoolerInput();

	// End of Get Input subroutines for the CondenserLoopFluidCoolers Module
	//******************************************************************************

	// Beginning Initialization Section for the CondenserLoopFluidCoolers Module
	//******************************************************************************

	void
	InitSimVars();

	void
	InitFluidCooler(
		int const FluidCoolerNum, // Number of the current fluid cooler being simulated
		bool const RunFlag // TRUE if fluid cooler is ON
	);

	void
	SizeFluidCooler( int const FluidCoolerNum );

	// End Initialization Section for the CondenserLoopFluidCoolers Module
	//******************************************************************************

	// Beginning of the CondenserLoopFluidCoolers Module Simulation Subroutines
	// *****************************************************************************

	void
	SingleSpeedFluidCooler( int & FluidCoolerNum );

	void
	TwoSpeedFluidCooler( int & FluidCoolerNum );

	void
	SimSimpleFluidCooler(
		int const FluidCoolerNum,
		Real64 const WaterMassFlowRate,
		Real64 const AirFlowRate,
		Real64 const UAdesign,
		Real64 & OutletWaterTemp
	);

	Real64
	SimpleFluidCoolerUAResidual(
		Real64 const UA, // UA of fluid cooler
		Array1< Real64 > const & Par // par(1) = design fluid cooler load [W]
	);

	// End of the CondenserLoopFluidCoolers Module Simulation Subroutines
	// *****************************************************************************

	// Beginning of Record Keeping subroutines for the FluidCooler Module
	// *****************************************************************************

	void
	UpdateFluidCooler( int const FluidCoolerNum );

	// End of Record Keeping subroutines for the FluidCooler Module
	// *****************************************************************************

	// Beginning of Reporting subroutines for the FluidCooler Module
	// *****************************************************************************

	void
	ReportFluidCooler(
		bool const RunFlag,
		int const FluidCoolerNum
	);

} // FluidCoolers

} // EnergyPlus

#endif
