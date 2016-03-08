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

#ifndef CoolTower_hh_INCLUDED
#define CoolTower_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

// (ref: Object: COOLTOWER:SHOWER)

namespace CoolTower {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS
	extern int const WaterSupplyFromMains;
	extern int const WaterSupplyFromTank;
	extern int const WaterFlowSchedule;
	extern int const WindDrivenFlow;

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLES DECLARATIONS:
	extern int NumCoolTowers; // Total cooltower statements in inputs

	// Subroutine Specifications for the Heat Balance Module

	// Types

	struct CoolTowerParams
	{
		// Members
		std::string Name; // The component name
		std::string CompType; // Type of component
		std::string Schedule; // Available schedule
		std::string ZoneName; // Name of zone the component is serving
		std::string PumpSchedName; // Available schedule of the water pump
		int SchedPtr; // Index to schedule
		int ZonePtr; // Point to this zone
		int PumpSchedPtr; // Index to schedule for water pump
		int FlowCtrlType; // Type of cooltower operation
		int CoolTWaterSupplyMode; // Type of water source
		std::string CoolTWaterSupplyName; // Name of water source
		int CoolTWaterSupTankID; // Index to water storage tank
		int CoolTWaterTankDemandARRID; // Index to water storage demand
		Real64 TowerHeight; // Effective cooltower height in m
		Real64 OutletArea; // Outlet area where conditioned air comes in m2
		Real64 OutletVelocity; // Outlet velocity of the cooltower in m/s
		Real64 MaxAirVolFlowRate; // Maximum allowable airflow in m3/s
		Real64 AirMassFlowRate; // Air mass flow rate in kg/s
		Real64 CoolTAirMass; // Air mass in kg
		Real64 MinZoneTemp; // Lower temperature limit to prevent over cooling in C
		Real64 FracWaterLoss; // Fraction of estimated blowdown and drift water
		Real64 FracFlowSched; // Fraction of airflow loss
		Real64 MaxWaterFlowRate; // Maximum limit of water flow rate in m3/s
		Real64 ActualWaterFlowRate; // Actual water mass flow rate in m3/s
		Real64 RatedPumpPower; // Rated power consumption for water pump serving the cooltower in watts
		Real64 SenHeatLoss; // Sensible heat loss in Joules
		Real64 SenHeatPower; // Sensible heat loss rate in watts
		Real64 LatHeatLoss; // Latent heat loss in Joules
		Real64 LatHeatPower; // Latent heat loss rate in watts
		Real64 AirVolFlowRate; // Air flow rate in m3/s
		Real64 AirVolFlowRateStd; // Air flow rate in m3/s at standard conditions
		Real64 CoolTAirVol; // Air volume in m3
		Real64 ActualAirVolFlowRate; // Actual air flow rate in m3/s
		Real64 InletDBTemp; // Outdoor dry bulb temperature in C
		Real64 InletWBTemp; // Outdoor wet bulb temperature in C
		Real64 InletHumRat; // Outdoor humidity ratio
		Real64 OutletTemp; // Dry bulb temperature at cooltower exit in C
		Real64 OutletHumRat; // Humidity ratio at cooltower exit
		Real64 CoolTWaterConsumpRate; // Total water consumption during the processes in m3/s
		Real64 CoolTWaterStarvMakeupRate; // Water provided from the mains (m3/s)
		Real64 CoolTWaterStarvMakeup; // Water provided from the mains
		Real64 CoolTWaterConsump; // Total water consumption in m3
		Real64 PumpElecPower; // Pump power in watts
		Real64 PumpElecConsump; // Pump energy consumption in Joules

		// Default Constructor
		CoolTowerParams() :
			SchedPtr( 0 ),
			ZonePtr( 0 ),
			PumpSchedPtr( 0 ),
			FlowCtrlType( 0 ),
			CoolTWaterSupplyMode( WaterSupplyFromMains ),
			CoolTWaterSupTankID( 0 ),
			CoolTWaterTankDemandARRID( 0 ),
			TowerHeight( 0.0 ),
			OutletArea( 0.0 ),
			OutletVelocity( 0.0 ),
			MaxAirVolFlowRate( 0.0 ),
			AirMassFlowRate( 0.0 ),
			CoolTAirMass( 0.0 ),
			MinZoneTemp( 0.0 ),
			FracWaterLoss( 0.0 ),
			FracFlowSched( 0.0 ),
			MaxWaterFlowRate( 0.0 ),
			ActualWaterFlowRate( 0.0 ),
			RatedPumpPower( 0.0 ),
			SenHeatLoss( 0.0 ),
			SenHeatPower( 0.0 ),
			LatHeatLoss( 0.0 ),
			LatHeatPower( 0.0 ),
			AirVolFlowRate( 0.0 ),
			AirVolFlowRateStd( 0.0 ),
			CoolTAirVol( 0.0 ),
			ActualAirVolFlowRate( 0.0 ),
			InletDBTemp( 0.0 ),
			InletWBTemp( 0.0 ),
			InletHumRat( 0.0 ),
			OutletTemp( 0.0 ),
			OutletHumRat( 0.0 ),
			CoolTWaterConsumpRate( 0.0 ),
			CoolTWaterStarvMakeupRate( 0.0 ),
			CoolTWaterStarvMakeup( 0.0 ),
			CoolTWaterConsump( 0.0 ),
			PumpElecPower( 0.0 ),
			PumpElecConsump( 0.0 )
		{}

	};

	// Object Data
	extern Array1D< CoolTowerParams > CoolTowerSys;

	// Functions

	void
	clear_state();

	void
	ManageCoolTower();

	void
	GetCoolTower();

	void
	CalcCoolTower();

	void
	UpdateCoolTower();

	void
	ReportCoolTower();

	//*****************************************************************************************

} // CoolTower

} // EnergyPlus

#endif
