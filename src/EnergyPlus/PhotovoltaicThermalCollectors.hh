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

#ifndef PhotovoltaicThermalCollectors_hh_INCLUDED
#define PhotovoltaicThermalCollectors_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace PhotovoltaicThermalCollectors {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:
	extern int const SimplePVTmodel;
	extern int const LayerByLayerPVTmodel;

	extern int const ScheduledThermEffic; // mode for thermal efficiency is to use schedule
	extern int const FixedThermEffic; // mode for thermal efficiency is to use fixed value

	extern int const LiquidWorkingFluid;
	extern int const AirWorkingFluid;

	extern int const CalledFromPlantLoopEquipMgr;
	extern int const CalledFromOutsideAirSystem;

	extern Real64 const SimplePVTWaterSizeFactor; // [ m3/s/m2 ] average of collectors in SolarCollectors.idf

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	extern Array1D_bool CheckEquipName;
	extern int NumPVT; // count of all types of PVT in input file
	extern int NumSimplePVTPerform; // count of simple PVT performance objects in input file

	// SUBROUTINE SPECIFICATIONS FOR MODULE:
	// Driver/Manager Routines

	// Utility routines for module
	// these would be public such as:
	//PUBLIC  GetPVTIncidentSolarForInternalPVLayer
	//PUBLIC  GetPVTCellTemp

	// Types

	struct SimplePVTModelStruct
	{
		// Members
		std::string Name;
		Real64 ThermalActiveFract; // fraction of surface area with active thermal collection
		int ThermEfficMode; // setting for how therm effic is determined
		Real64 ThermEffic; // fixed or current Therm efficiency
		int ThermEffSchedNum; // pointer to schedule for therm effic (if any)
		Real64 SurfEmissivity; // surface emittance in long wave IR
		Real64 LastCollectorTemp; // store previous temperature
		Real64 CollectorTemp; // average solar collector temp.

		// Default Constructor
		SimplePVTModelStruct() :
			ThermalActiveFract( 0.0 ),
			ThermEfficMode( 0 ),
			ThermEffic( 0.0 ),
			ThermEffSchedNum( 0 ),
			SurfEmissivity( 0.0 ),
			LastCollectorTemp( 0.0 ),
			CollectorTemp( 0.0 )
		{}
	};

	struct PVTReportStruct
	{
		// Members
		Real64 ThermEfficiency; // Thermal efficiency of solar energy conversion
		Real64 ThermPower; // Heat gain or loss to collector fluid (W)
		Real64 ThermHeatGain; // Heat gain to collector fluid (W)
		Real64 ThermHeatLoss; // Heat loss from collector fluid (W)
		Real64 ThermEnergy; // Energy gained (or lost) to collector fluid (J)
		Real64 MdotWorkFluid; // working fluid mass flow rate (kg/s)
		Real64 TinletWorkFluid; // working fluid inlet temp (C)
		Real64 ToutletWorkFluid; // working fluid outlet temp (C)
		Real64 BypassStatus; // 0 = no bypass, 1=full bypass

		// Default Constructor
		PVTReportStruct() :
			ThermEfficiency( 0.0 ),
			ThermPower( 0.0 ),
			ThermHeatGain( 0.0 ),
			ThermHeatLoss( 0.0 ),
			ThermEnergy( 0.0 ),
			MdotWorkFluid( 0.0 ),
			TinletWorkFluid( 0.0 ),
			ToutletWorkFluid( 0.0 ),
			BypassStatus( 0.0 )
		{}
	};

	struct PVTCollectorStruct
	{
		// Members
		// input
		std::string Name; // Name of PVT collector
		int TypeNum; // Plant Side Connection: 'TypeOf_Num' assigned in DataPlant  !DSU
		int WLoopNum; // Water plant loop index number                      !DSU
		int WLoopSideNum; // Water plant loop side index                        !DSU
		int WLoopBranchNum; // Water plant loop branch index                      !DSU
		int WLoopCompNum; // Water plant loop component index                   !DSU
		bool EnvrnInit; // manage begin environmen inits
		bool SizingInit; // manage when sizing is complete
		std::string PVTModelName; // Name of PVT performance object
		int PVTModelType; // model type indicator, only simple avail now
		int SurfNum; // surface index
		std::string PVname; // named Generator:Photovoltaic object
		int PVnum; // PV index
		bool PVfound; // init, need to delay get input until PV gotten
		// INTEGER                      :: PlantLoopNum       = 0  ! needed for sizing and control
		// INTEGER                      :: PlantLoopSide      = 0  ! needed for sizing, demand vs. supply sided
		SimplePVTModelStruct Simple; // performance data structure.
		int WorkingFluidType;
		int PlantInletNodeNum;
		int PlantOutletNodeNum;
		int HVACInletNodeNum;
		int HVACOutletNodeNum;
		Real64 DesignVolFlowRate;
		bool DesignVolFlowRateWasAutoSized; // true if design volume flow rate was autosize on input
		Real64 MaxMassFlowRate;
		Real64 MassFlowRate; // DSU
		Real64 AreaCol;
		bool BypassDamperOff;
		bool CoolingUseful;
		bool HeatingUseful;
		PVTReportStruct Report;

		// Default Constructor
		PVTCollectorStruct() :
			WLoopNum( 0 ),
			WLoopSideNum( 0 ),
			WLoopBranchNum( 0 ),
			WLoopCompNum( 0 ),
			EnvrnInit( true ),
			SizingInit( true ),
			PVTModelType( 0 ),
			SurfNum( 0 ),
			PVnum( 0 ),
			PVfound( false ),
			WorkingFluidType( 0 ),
			PlantInletNodeNum( 0 ),
			PlantOutletNodeNum( 0 ),
			HVACInletNodeNum( 0 ),
			HVACOutletNodeNum( 0 ),
			DesignVolFlowRate( 0.0 ),
			DesignVolFlowRateWasAutoSized( false ),
			MaxMassFlowRate( 0.0 ),
			MassFlowRate( 0.0 ),
			AreaCol( 0.0 ),
			BypassDamperOff( true ),
			CoolingUseful( false ),
			HeatingUseful( false )
		{}
	};

	// Object Data
	extern Array1D< PVTCollectorStruct > PVT;

	// Functions

	void
	SimPVTcollectors(
		int & PVTnum, // index to PVT array.
		bool const FirstHVACIteration,
		int const CalledFrom,
		Optional_string_const PVTName = _,
		Optional_bool_const InitLoopEquip = _
	);

	void
	GetPVTcollectorsInput();

	void
	InitPVTcollectors(
		int const PVTnum,
		bool const FirstHVACIteration
	);

	void
	SizePVT( int const PVTnum );

	void
	ControlPVTcollector( int const PVTnum );

	void
	CalcPVTcollectors( int const PVTnum );

	void
	UpdatePVTcollectors( int const PVTnum );

	void
	GetPVTThermalPowerProduction(
		int const PVindex, // index of PV generator (not PVT collector)
		Real64 & ThermalPower,
		Real64 & ThermalEnergy
	);

	//=====================  Utility/Other routines for module.
	// Insert as appropriate

} // PhotovoltaicThermalCollectors

} // EnergyPlus

#endif
