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

#ifndef HVACStandAloneERV_hh_INCLUDED
#define HVACStandAloneERV_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace HVACStandAloneERV {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS

	extern int const ControllerSimple;
	extern int const ControllerOutsideAir;
	extern int const ControllerStandAloneERV;

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:

	extern int NumStandAloneERVs; // Total number of stand alone ERVs defined in the idf

	extern Array1D_bool MySizeFlag;
	extern Array1D_bool CheckEquipName;
	extern bool GetERVInputFlag; // First time, input is "gotten"

	// SUBROUTINE SPECIFICATIONS FOR MODULE

	// Driver/Manager Routine

	// Algorithms/Calculation routine for the module

	// Get Input routine for module

	// Sizing routine for the module

	// Initialization routine for module

	// Utility routines for module

	// Types

	struct StandAloneERVData
	{
		// Members
		// input data
		std::string Name; // name of the stand alone ERV unit
		std::string UnitType; // ZoneHVAC:EnergyRecoveryVentilator
		int SchedPtr; // pointer to availability schedule
		std::string HeatExchangerName; // name of the heat exchanger within the ERV unit
		int HeatExchangerIndex; // Pointer to heat exchanger
		int HeatExchangerTypeNum; // Parameter equivalent of HX object type
		int SupplyAirInletNode; // supply air inlet node for the stand alone ERV
		int SupplyAirOutletNode; // supply air outlet node for the stand alone ERV
		std::string SupplyAirFanName; // fan name in the supply air stream of the ERV
		int SupplyAirFanIndex; // index to supply air fan
		int SupplyAirFanSchPtr; // index to supply air fan schedule
		int SupplyAirFanType_Num; // parameter equivalent of fan type
		int ExhaustAirInletNode; // exhaust air inlet node for the stand alone ERV
		int ExhaustAirOutletNode; // exhaust air outlet node for the stand alone ERV
		std::string ExhaustAirFanName; // fan name in exhaust air stream of the ERV
		int ExhaustAirFanIndex; // index to exhaust air fan
		int ExhaustAirFanSchPtr; // index to exhaust air fan schedule
		int ExhaustAirFanType_Num; // paramter equivalent of fan type
		Real64 SupplyAirVolFlow; // volumetric flow rate through the supply side of the ERV
		Real64 ExhaustAirVolFlow; // volumetric flow rate through the exhaust side of the ERV
		std::string ControllerName; // name of the controller for the stand alone ERV
		bool ControllerNameDefined; // controller for the stand alone ERV is defined
		int ControlledZoneNum; // index to controlled zone for stand alone ERV
		int ControllerIndex; // Pointer for updates by routines this module calls.
		Real64 MaxSupAirMassFlow; // air mass flow rate through the supply side of the ERV
		Real64 MaxExhAirMassFlow; // air mass flow rate through the exhaust side of the ERV
		Real64 HighRHOAFlowRatio; // ratio of outside air flow to max outside air flow
		Real64 DesignSAFanVolFlowRate; // SA fan volumetric flow rate
		Real64 DesignEAFanVolFlowRate; // EA fan volumetric flow rate
		Real64 DesignHXVolFlowRate; // HX (heat exchanger) volumetric flow rate
		Real64 DesignSAFanMassFlowRate; // SA fan mass flow rate
		Real64 DesignEAFanMassFlowRate; // EA fan mass flow rate
		Real64 AirVolFlowPerFloorArea; // Air flow rate per unit floor area, used for autosizing
		Real64 AirVolFlowPerOccupant; // Air flow rate per occupant, used for autosizing
		int EconomizerOASchedPtr; // schedule to modify outdoor air
		bool FlowError; // used for one-time warning message for flow imbalance (Init)
		int AvailStatus;
		std::string AvailManagerListName; // Name of an availability manager list object
		// report variables
		Real64 ElecUseRate; // total electric use rate (power) for supply/exhaust fans & generic HX parasitics [W]
		Real64 ElecUseEnergy; // electric energy use for supply fan, exhaust fan, and generic HX parasitics [J]
		Real64 SensCoolingEnergy; // sensible cooling energy delivered by the ERV supply air to the zone [J]
		Real64 SensCoolingRate; // rate of sensible cooling delivered to the zone [W]
		Real64 LatCoolingEnergy; // latent cooling energy delivered by the ERV supply air to the zone [J]
		Real64 LatCoolingRate; // rate of latent cooling delivered to the zone [W]
		Real64 TotCoolingEnergy; // total cooling energy delivered by the ERV supply air to the zone [J]
		Real64 TotCoolingRate; // rate of total cooling delivered to the zone [W]
		Real64 SensHeatingEnergy; // sensible heating energy delivered by the ERV supply air to the zone [J]
		Real64 SensHeatingRate; // rate of sensible heating delivered to the zone [W]
		Real64 LatHeatingEnergy; // latent heating energy delivered by the ERV supply air to the zone [J]
		Real64 LatHeatingRate; // rate of latent heating delivered to the zone [W]
		Real64 TotHeatingEnergy; // total heating energy delivered by the ERV supply air to the zone [J]
		Real64 TotHeatingRate; // rate of total heating delivered to the zone [W]

		// Default Constructor
		StandAloneERVData() :
			SchedPtr( 0 ),
			HeatExchangerIndex( 0 ),
			HeatExchangerTypeNum( 0 ),
			SupplyAirInletNode( 0 ),
			SupplyAirOutletNode( 0 ),
			SupplyAirFanIndex( 0 ),
			SupplyAirFanSchPtr( 0 ),
			SupplyAirFanType_Num( 0 ),
			ExhaustAirInletNode( 0 ),
			ExhaustAirOutletNode( 0 ),
			ExhaustAirFanIndex( 0 ),
			ExhaustAirFanSchPtr( 0 ),
			ExhaustAirFanType_Num( 0 ),
			SupplyAirVolFlow( 0.0 ),
			ExhaustAirVolFlow( 0.0 ),
			ControllerNameDefined( true ),
			ControlledZoneNum( 0 ),
			ControllerIndex( 0 ),
			MaxSupAirMassFlow( 0.0 ),
			MaxExhAirMassFlow( 0.0 ),
			HighRHOAFlowRatio( 1.0 ),
			DesignSAFanVolFlowRate( 0.0 ),
			DesignEAFanVolFlowRate( 0.0 ),
			DesignHXVolFlowRate( 0.0 ),
			DesignSAFanMassFlowRate( 0.0 ),
			DesignEAFanMassFlowRate( 0.0 ),
			AirVolFlowPerFloorArea( 0.0 ),
			AirVolFlowPerOccupant( 0.0 ),
			EconomizerOASchedPtr( 0 ),
			FlowError( true ),
			AvailStatus( 0 ),
			ElecUseRate( 0.0 ),
			ElecUseEnergy( 0.0 ),
			SensCoolingEnergy( 0.0 ),
			SensCoolingRate( 0.0 ),
			LatCoolingEnergy( 0.0 ),
			LatCoolingRate( 0.0 ),
			TotCoolingEnergy( 0.0 ),
			TotCoolingRate( 0.0 ),
			SensHeatingEnergy( 0.0 ),
			SensHeatingRate( 0.0 ),
			LatHeatingEnergy( 0.0 ),
			LatHeatingRate( 0.0 ),
			TotHeatingEnergy( 0.0 ),
			TotHeatingRate( 0.0 )
		{}

	};

	// Object Data
	extern Array1D< StandAloneERVData > StandAloneERV;

	// Functions

	void
	clear_state();

	void
	SimStandAloneERV(
		std::string const & CompName, // name of the Stand Alone ERV unit
		int const ZoneNum, // number of zone being served unused1208
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & SensLoadMet, // net sensible load supplied by the ERV unit to the zone (W)
		Real64 & LatLoadMet, // net latent load supplied by ERV unit to the zone (kg/s),
		int & CompIndex // pointer to correct component
	);

	void
	GetStandAloneERV();

	void
	InitStandAloneERV(
		int const StandAloneERVNum, // number of the current Stand Alone ERV unit being simulated
		int const ZoneNum, // number of zone being served unused1208
		bool const FirstHVACIteration // TRUE if first HVAC iteration
	);

	void
	SizeStandAloneERV( int const StandAloneERVNum );

	void
	CalcStandAloneERV(
		int const StandAloneERVNum, // Unit index in ERV data structure
		bool const FirstHVACIteration, // flag for 1st HVAC iteration in the time step
		Real64 & SensLoadMet, // sensible zone load met by unit (W)
		Real64 & LatentMassLoadMet // latent zone load met by unit (kg/s), dehumid = negative
	);

	void
	ReportStandAloneERV( int const StandAloneERVNum ); // number of the current Stand Alone ERV being simulated

	//        End of Reporting subroutines for the Module

	//        Utility subroutines/functions for the HeatingCoil Module

	Real64
	GetSupplyAirFlowRate(
		std::string const & ERVType, // must be "ZoneHVAC:EnergyRecoveryVentilator"
		std::string const & ERVCtrlName, // must match a controller name in the ERV data structure
		bool & ErrorsFound // set to true if problem
	);

	int
	GetSupplyAirInletNode(
		std::string const & ERVType, // must be "ZoneHVAC:EnergyRecoveryVentilator"
		std::string const & ERVCtrlName, // must match a controller name in the ERV data structure
		bool & ErrorsFound // set to true if problem
	);

	int
	GetExhaustAirInletNode(
		std::string const & ERVType, // must be "ZoneHVAC:EnergyRecoveryVentilator"
		std::string const & ERVCtrlName, // must match a controller name in the ERV data structure
		bool & ErrorsFound // set to true if problem
	);

	int
	GetStandAloneERVOutAirNode( int const StandAloneERVNum );

	int
	GetStandAloneERVZoneInletAirNode( int const StandAloneERVNum );

	int
	GetStandAloneERVReturnAirNode( int const StandAloneERVNum );

} // HVACStandAloneERV

} // EnergyPlus

#endif
