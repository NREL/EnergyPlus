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

#ifndef ZoneEquipmentManager_hh_INCLUDED
#define ZoneEquipmentManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace ZoneEquipmentManager {

	// Using/Aliasing

	// Data
	//MODULE PARAMETER DEFINITIONS
	// na

	// DERIVED TYPE DEFINITIONS

	//MODULE VARIABLE DECLARATIONS:
	extern Array1D< Real64 > AvgData; // scratch array for storing averaged data
	extern Array1D_int DefaultSimOrder;
	extern int NumOfTimeStepInDay; // number of zone time steps in a day
	extern bool GetZoneEquipmentInputFlag;
	extern bool SizeZoneEquipmentOneTimeFlag;


	//SUBROUTINE SPECIFICATIONS FOR MODULE ZoneEquipmentManager

	// Types

	struct SimulationOrder
	{
		// Members
		std::string EquipType;
		int EquipType_Num;
		std::string EquipName;
		int EquipPtr;
		int CoolingPriority;
		int HeatingPriority;

		// Default Constructor
		SimulationOrder() :
			EquipType_Num( 0 ),
			EquipPtr( 0 ),
			CoolingPriority( 0 ),
			HeatingPriority( 0 )
		{}

	};

	// Object Data
	extern Array1D< SimulationOrder > PrioritySimOrder;

	// Functions
	void
	clear_state();

	void
	ManageZoneEquipment(
		bool const FirstHVACIteration,
		bool & SimZone,
		bool & SimAir
	);

	void
	GetZoneEquipment();

	void
	InitZoneEquipment( bool const FirstHVACIteration ); // unused 1208

	void
	SizeZoneEquipment();

	void
	SetUpZoneSizingArrays();

	void
	RezeroZoneSizingArrays();

	void
	UpdateZoneSizing( int const CallIndicator );

	void
	SimZoneEquipment(
		bool const FirstHVACIteration,
		bool & SimAir
	);

	void
	SetZoneEquipSimOrder(
		int const ControlledZoneNum,
		int const ActualZoneNum
	);

	void
	InitSystemOutputRequired(
		int const ZoneNum,
		Real64 & SysOutputProvided,
		Real64 & LatOutputProvided
	);

	void
	UpdateSystemOutputRequired(
		int const ZoneNum,
		Real64 const SysOutputProvided, // sensible output provided by zone equipment (W)
		Real64 const LatOutputProvided, // latent output provided by zone equipment (kg/s)
		Optional_int_const EquipPriorityNum = _ // index in PrioritySimOrder for this update
	);

	void
	CalcZoneMassBalance();

	void
	CalcAirFlowSimple(
		int const SysTimestepLoop = 0, // System time step index
		bool const AdjustZoneMixingFlowFlag = false // flags to adjust zone mxing mass flow rate
	);

	void
	GetStandAloneERVNodes(int const OutdoorNum); // Zone Air Balance Outdoor index

	void
	CalcZoneMixingFlowRateOfReceivingZone(
		int const ZoneNum,
		Real64 & ZoneMixingAirMassFlowRate
		);

	void
	CalcZoneMixingFlowRateOfSourceZone(int const ZoneNum);

	void
	CalcZoneLeavingConditions();

	void
	UpdateZoneEquipment( bool & SimAir );

	void
	ReportZoneEquipment();

	void
	CalcDOASSupCondsForSizing(
		Real64 OutDB, // outside air temperature [C]
		Real64 OutHR, // outside humidity ratio [kg Water / kg Dry Air]
		int DOASControl, // dedicated outside air control strategy
		Real64 DOASLowTemp, // DOAS low setpoint [C]
		Real64 DOASHighTemp, // DOAS high setpoint [C]
		Real64 W90H, // humidity ratio at DOAS high setpoint temperature and 90% relative humidity [kg Water / kg Dry Air]
		Real64 W90L, // humidity ratio at DOAS low setpoint temperature and 90% relative humidity [kg Water / kg Dry Air]
		Real64 & DOASSupTemp, // DOAS supply temperature [C]
		Real64 & DOASSupHR // DOAS Supply Humidity ratio [kg Water / kg Dry Air]
	);

} // ZoneEquipmentManager

} // EnergyPlus

#endif
