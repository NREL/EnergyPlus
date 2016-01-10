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

#ifndef SteamBaseboardRadiator_hh_INCLUDED
#define SteamBaseboardRadiator_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace SteamBaseboardRadiator {

	// Using/Aliasing

	// Data
	//MODULE PARAMETER DEFINITIONS
	extern std::string const cCMO_BBRadiator_Steam;

	// DERIVED TYPE DEFINITIONS

	//MODULE VARIABLE DECLARATIONS:
	extern int NumSteamBaseboards;
	extern int SteamIndex;

	extern Array1D< Real64 > QBBSteamRadSource; // Need to keep the last value in case we are still iterating
	extern Array1D< Real64 > QBBSteamRadSrcAvg; // Need to keep the last value in case we are still iterating
	extern Array1D< Real64 > ZeroSourceSumHATsurf; // Equal to the SumHATsurf for all the walls in a zone
	// with no source

	// Record keeping variables used to calculate QBBRadSrcAvg locally
	extern Array1D< Real64 > LastQBBSteamRadSrc; // Need to keep the last value in case we are still iterating
	extern Array1D< Real64 > LastSysTimeElapsed; // Need to keep the last value in case we are still iterating
	extern Array1D< Real64 > LastTimeStepSys; // Need to keep the last value in case we are still iterating
	extern Array1D_bool MySizeFlag;
	extern Array1D_bool CheckEquipName;
	extern Array1D_bool SetLoopIndexFlag; // get loop number flag

	//SUBROUTINE SPECIFICATIONS FOR MODULE BaseboardRadiator

	// Types

	struct SteamBaseboardParams
	{
		// Members
		std::string EquipID;
		int EquipType;
		std::string Schedule;
		Array1D_string SurfaceName;
		Array1D_int SurfacePtr;
		int ZonePtr;
		int SchedPtr; // Pointer to the correct schedule
		int SteamInletNode; // Inlet steam baseboard node
		int SteamOutletNode; // Outlet steam baseboard node
		int TotSurfToDistrib; // Total numbers of the surfaces that the radiant heat gets distributed
		int FluidIndex; // Fluid index for FluidProperties (Steam)
		int ControlCompTypeNum;
		int CompErrIndex;
		Real64 DegOfSubcooling; // Temperature differences due to subcooling of the condensate [C]
		Real64 Offset; // Control accuracy
		Real64 SteamMassFlowRate; // Mass flow rate of steam passing through the heater [kg/s]
		Real64 SteamMassFlowRateMax; // Maximum mass flow rate of steam [kg/s]
		Real64 SteamVolFlowRateMax; // Maximum volumetric flow rate of steam [m3/s]
		Real64 SteamOutletTemp; // Outlet steam temperature from the heater [C]
		Real64 SteamInletTemp; // Inlet steam temperature [C]
		Real64 SteamInletEnthalpy; // Enthalpy of the steam delivered from the boiler [J/kg]
		Real64 SteamOutletEnthalpy; // Enthalpy of the steam leaving the heater [J/kg]
		Real64 SteamInletPress; // Pressure of steam at the inlet of the heater [Pa]
		Real64 SteamOutletPress; // Pressure of steam at the outlet of the heater [Pa]
		Real64 SteamInletQuality; // Quality of steam at the inlet of the heater [Pa]
		Real64 SteamOutletQuality; // Quality of steam at the outlet of the heater [Pa]
		Real64 FracRadiant; // User defined fraction for radiant heat addition
		Real64 FracConvect; // Fraction for convective heat addition
		Real64 FracDistribPerson; // Fraction for radiant heat incident on people
		Array1D< Real64 > FracDistribToSurf;
		Real64 TotPower; // Convective system impact rate that the heater actually meets [W]
		Real64 Power; // Maximum heating rate [W]
		Real64 ConvPower; // Convective heating rate [W]
		Real64 RadPower; // Radiant heating rate [W]
		Real64 TotEnergy; // Convective system impact energy [J]
		Real64 Energy; // Maximum heating energy [J]
		Real64 ConvEnergy; // Convective heating energy [J]
		Real64 RadEnergy; // Radiant heating energy [J]
		int LoopNum; // plant loop index
		int LoopSideNum; // plant loop side index
		int BranchNum; // plant loop branch index
		int CompNum; // plant loop component index
		int BBLoadReSimIndex;
		int BBMassFlowReSimIndex;
		int BBInletTempFlowReSimIndex;
		int HeatingCapMethod; // - Method for steam baseboard Radiator system heating capacity scaledsizing calculation (HeatingDesignCapacity, CapacityPerFloorArea, FracOfAutosizedHeatingCapacity)
		Real64 ScaledHeatingCapacity; // -  steam baseboard Radiator system scaled maximum heating capacity {W} or scalable variable of zone HVAC equipment, {-}, or {W/m2}

		// Default Constructor
		SteamBaseboardParams() :
			EquipType( 0 ),
			ZonePtr( 0 ),
			SchedPtr( 0 ),
			SteamInletNode( 0 ),
			SteamOutletNode( 0 ),
			TotSurfToDistrib( 0 ),
			FluidIndex( 0 ),
			ControlCompTypeNum( 0 ),
			CompErrIndex( 0 ),
			DegOfSubcooling( 0.0 ),
			Offset( 0.0 ),
			SteamMassFlowRate( 0.0 ),
			SteamMassFlowRateMax( 0.0 ),
			SteamVolFlowRateMax( 0.0 ),
			SteamOutletTemp( 0.0 ),
			SteamInletTemp( 0.0 ),
			SteamInletEnthalpy( 0.0 ),
			SteamOutletEnthalpy( 0.0 ),
			SteamInletPress( 0.0 ),
			SteamOutletPress( 0.0 ),
			SteamInletQuality( 0.0 ),
			SteamOutletQuality( 0.0 ),
			FracRadiant( 0.0 ),
			FracConvect( 0.0 ),
			FracDistribPerson( 0.0 ),
			TotPower( 0.0 ),
			Power( 0.0 ),
			ConvPower( 0.0 ),
			RadPower( 0.0 ),
			TotEnergy( 0.0 ),
			Energy( 0.0 ),
			ConvEnergy( 0.0 ),
			RadEnergy( 0.0 ),
			LoopNum( 0 ),
			LoopSideNum( 0 ),
			BranchNum( 0 ),
			CompNum( 0 ),
			BBLoadReSimIndex( 0 ),
			BBMassFlowReSimIndex( 0 ),
			BBInletTempFlowReSimIndex( 0 ),
			HeatingCapMethod( 0 ),
			ScaledHeatingCapacity( 0.0 )
		{}

	};

	struct SteamBaseboardNumericFieldData
	{
		// Members
		Array1D_string FieldNames;

		// Default Constructor
		SteamBaseboardNumericFieldData()
		{}

	};

	// Object Data
	extern Array1D< SteamBaseboardParams > SteamBaseboard;
	extern Array1D< SteamBaseboardNumericFieldData > SteamBaseboardNumericFields;

	// Functions

	void
	SimSteamBaseboard(
		std::string const & EquipName,
		int const ActualZoneNum,
		int const ControlledZoneNum,
		bool const FirstHVACIteration,
		Real64 & PowerMet,
		int & CompIndex
	);

	void
	GetSteamBaseboardInput();

	void
	InitSteamBaseboard(
		int const BaseboardNum,
		int const ControlledZoneNumSub,
		bool const FirstHVACIteration
	);

	void
	SizeSteamBaseboard( int const BaseboardNum );

	void
	CalcSteamBaseboard(
		int & BaseboardNum,
		Real64 & LoadMet
	);

	void
	UpdateSteamBaseboard( int const BaseboardNum );

	void
	UpdateBBSteamRadSourceValAvg( bool & SteamBaseboardSysOn ); // .TRUE. if the radiant system has run this zone time step

	void
	DistributeBBSteamRadGains();

	void
	ReportSteamBaseboard( int const BaseboardNum );

	Real64
	SumHATsurf( int const ZoneNum ); // Zone number

	void
	UpdateSteamBaseboardPlantConnection(
		int const BaseboardTypeNum, // type index
		std::string const & BaseboardName, // component name
		int const EquipFlowCtrl, // Flow control mode for the equipment
		int const LoopNum, // Plant loop index for where called from
		int const LoopSide, // Plant loop side index for where called from
		int & CompIndex, // Chiller number pointer
		bool const FirstHVACIteration,
		bool & InitLoopEquip // If not zero, calculate the max load for operating conditions
	);

} // SteamBaseboardRadiator

} // EnergyPlus

#endif
