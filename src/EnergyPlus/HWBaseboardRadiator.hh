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

#ifndef HWBaseboardRadiator_hh_INCLUDED
#define HWBaseboardRadiator_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace HWBaseboardRadiator {

	// Using/Aliasing

	// Data
	//MODULE PARAMETER DEFINITIONS

	extern std::string const cCMO_BBRadiator_Water;

	// DERIVED TYPE DEFINITIONS

	//MODULE VARIABLE DECLARATIONS:
	extern int NumHWBaseboards;
	extern Array1D< Real64 > QBBRadSource; // Need to keep the last value in case we are still iterating
	extern Array1D< Real64 > QBBRadSrcAvg; // Need to keep the last value in case we are still iterating
	extern Array1D< Real64 > ZeroSourceSumHATsurf; // Equal to the SumHATsurf for all the walls in a zone with no source

	// Record keeping variables used to calculate QBBRadSrcAvg locally
	extern Array1D< Real64 > LastQBBRadSrc; // Need to keep the last value in case we are still iterating
	extern Array1D< Real64 > LastSysTimeElapsed; // Need to keep the last value in case we are still iterating
	extern Array1D< Real64 > LastTimeStepSys; // Need to keep the last value in case we are still iterating
	extern Array1D_bool MySizeFlag;
	extern Array1D_bool CheckEquipName;
	extern Array1D_bool SetLoopIndexFlag; // get loop number flag

	//SUBROUTINE SPECIFICATIONS FOR MODULE BaseboardRadiator

	// Types

	struct HWBaseboardParams
	{
		// Members
		std::string EquipID;
		int EquipType;
		std::string Schedule;
		Array1D_string SurfaceName;
		Array1D_int SurfacePtr;
		int ZonePtr;
		int SchedPtr;
		int WaterInletNode;
		int WaterOutletNode;
		int TotSurfToDistrib;
		int ControlCompTypeNum;
		int CompErrIndex;
		Real64 AirMassFlowRate;
		Real64 AirMassFlowRateStd;
		Real64 WaterTempAvg;
		Real64 RatedCapacity;
		Real64 UA;
		Real64 Offset;
		Real64 WaterMassFlowRate;
		Real64 WaterMassFlowRateMax;
		Real64 WaterMassFlowRateStd;
		Real64 WaterVolFlowRateMax;
		Real64 WaterInletTempStd;
		Real64 WaterInletTemp;
		Real64 WaterInletEnthalpy;
		Real64 WaterOutletTempStd;
		Real64 WaterOutletTemp;
		Real64 WaterOutletEnthalpy;
		Real64 AirInletTempStd;
		Real64 AirInletTemp;
		Real64 AirOutletTemp;
		Real64 AirInletHumRat;
		Real64 AirOutletTempStd;
		Real64 FracRadiant;
		Real64 FracConvect;
		Real64 FracDistribPerson;
		Array1D< Real64 > FracDistribToSurf;
		Real64 TotPower;
		Real64 Power;
		Real64 ConvPower;
		Real64 RadPower;
		Real64 TotEnergy;
		Real64 Energy;
		Real64 ConvEnergy;
		Real64 RadEnergy;
		int LoopNum; // plant loop index
		int LoopSideNum; // plant loop side index
		int BranchNum; // plant loop branch index
		int CompNum; // plant loop component index
		int BBLoadReSimIndex;
		int BBMassFlowReSimIndex;
		int BBInletTempFlowReSimIndex;
		int HeatingCapMethod; // - Method for heating capacity scaledsizing calculation (HeatingDesignCapacity, CapacityPerFloorArea, FracOfAutosizedHeatingCapacity)
		Real64 ScaledHeatingCapacity; // - scaled maximum heating capacity {W} or scalable variable of zone HVAC equipment, {-}, or {W/m2}

		// Default Constructor
		HWBaseboardParams() :
			EquipType( 0 ),
			ZonePtr( 0 ),
			SchedPtr( 0 ),
			WaterInletNode( 0 ),
			WaterOutletNode( 0 ),
			TotSurfToDistrib( 0 ),
			ControlCompTypeNum( 0 ),
			CompErrIndex( 0 ),
			AirMassFlowRate( 0.0 ),
			AirMassFlowRateStd( 0.0 ),
			WaterTempAvg( 0.0 ),
			RatedCapacity( 0.0 ),
			UA( 0.0 ),
			Offset( 0.0 ),
			WaterMassFlowRate( 0.0 ),
			WaterMassFlowRateMax( 0.0 ),
			WaterMassFlowRateStd( 0.0 ),
			WaterVolFlowRateMax( 0.0 ),
			WaterInletTempStd( 0.0 ),
			WaterInletTemp( 0.0 ),
			WaterInletEnthalpy( 0.0 ),
			WaterOutletTempStd( 0.0 ),
			WaterOutletTemp( 0.0 ),
			WaterOutletEnthalpy( 0.0 ),
			AirInletTempStd( 0.0 ),
			AirInletTemp( 0.0 ),
			AirOutletTemp( 0.0 ),
			AirInletHumRat( 0.0 ),
			AirOutletTempStd( 0.0 ),
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

	struct HWBaseboardNumericFieldData
	{
		// Members
		Array1D_string FieldNames;

		// Default Constructor
		HWBaseboardNumericFieldData()
		{}

	};

	// Object Data
	extern Array1D< HWBaseboardParams > HWBaseboard;
	extern Array1D< HWBaseboardNumericFieldData > HWBaseboardNumericFields;

	// Functions

	void
	SimHWBaseboard(
		std::string const & EquipName,
		int const ActualZoneNum,
		int const ControlledZoneNum,
		bool const FirstHVACIteration,
		Real64 & PowerMet,
		int & CompIndex
	);

	void
	GetHWBaseboardInput();

	void
	InitHWBaseboard(
		int const BaseboardNum,
		int const ControlledZoneNumSub,
		bool const FirstHVACIteration
	);

	void
	SizeHWBaseboard( int const BaseboardNum );

	void
	CalcHWBaseboard(
		int & BaseboardNum,
		Real64 & LoadMet
	);

	void
	UpdateHWBaseboard( int const BaseboardNum );

	void
	UpdateBBRadSourceValAvg( bool & HWBaseboardSysOn ); // .TRUE. if the radiant system has run this zone time step

	void
	DistributeBBRadGains();

	void
	ReportHWBaseboard( int const BaseboardNum );

	Real64
	SumHATsurf( int const ZoneNum ); // Zone number

	void
	UpdateHWBaseboardPlantConnection(
		int const BaseboardTypeNum, // type index
		std::string const & BaseboardName, // component name
		int const EquipFlowCtrl, // Flow control mode for the equipment
		int const LoopNum, // Plant loop index for where called from
		int const LoopSide, // Plant loop side index for where called from
		int & CompIndex, // Chiller number pointer
		bool const FirstHVACIteration,
		bool & InitLoopEquip // If not zero, calculate the max load for operating conditions
	);

	//*****************************************************************************************

} // HWBaseboardRadiator

} // EnergyPlus

#endif
