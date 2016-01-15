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

#ifndef WindowAC_hh_INCLUDED
#define WindowAC_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace WindowAC {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS
	extern int const WindowAC_UnitType;
	extern std::string const cWindowAC_UnitType;
	extern Array1D_string const cWindowAC_UnitTypes;

	// Compressor operation
	extern int const On; // normal compressor operation
	extern int const Off; // signal DXCoil that compressor shouldn't run

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:

	extern int NumWindAC;
	extern int NumWindACCyc;
	extern Array1D_bool MySizeFlag;
	extern bool GetWindowACInputFlag; // First time, input is "gotten"
	extern bool CoolingLoad; // defines a cooling load
	extern Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE

	// Types

	struct WindACData
	{
		// Members
		// input data
		std::string Name; // name of unit
		//  CHARACTER(len=MaxNameLength) :: UnitType         =' '  ! type of unit
		int UnitType; // type of unit
		std::string Sched; // availability schedule
		int SchedPtr; // index to schedule
		int FanSchedPtr; // index to fan operating mode schedule
		int FanAvailSchedPtr; // index to fan availability schedule
		Real64 MaxAirVolFlow; // m3/s
		Real64 MaxAirMassFlow; // kg/s
		Real64 OutAirVolFlow; // m3/s
		Real64 OutAirMassFlow; // kg/s
		int AirInNode; // inlet air node number
		int AirOutNode; // outlet air node number
		int OutsideAirNode; // outside air node number
		int AirReliefNode; // relief air node number
		int MixedAirNode; // Mixed Air Node number
		std::string OAMixName; // name of outdoor air mixer
		std::string OAMixType; // type of outdoor air mixer
		int OAMixIndex;
		std::string FanName; // name of fan
		std::string FanType; // type of fan
		int FanType_Num; // index to fan type
		int FanIndex;
		std::string DXCoilName; // name of cooling coil
		std::string DXCoilType; // type of cooling coil,Coil:DX:CoolingBypassFactorEmpirical or
		// 'CoilSystem:Cooling:DX:HeatExchangerAssisted'
		int DXCoilType_Num; // Numeric Equivalent for DXCoil Type
		int DXCoilIndex; // Index to DX cooling coil
		int CoilOutletNodeNum; // Outlet node number of DX cooling coil
		int OpMode; // mode of operation; 1=cycling fan, cycling compressor,
		// 2=continuous fan, cycling compresor
		int FanPlace; // fan placement; 1=blow through, 2=draw through
		int MaxIterIndex1;
		int MaxIterIndex2;
		Real64 ConvergenceTol; // Convergence tolerance, fraction (ZoneLoad - Equip Output)/ZoneLoad
		// Calc data
		Real64 PartLoadFrac; // part load fraction for the unit
		bool EMSOverridePartLoadFrac;
		Real64 EMSValueForPartLoadFrac;
		// Report data
		Real64 TotCoolEnergyRate; // total cooling output [W]
		Real64 TotCoolEnergy; // total cooling output [J]
		Real64 SensCoolEnergyRate; // sensible cooling output [W]
		Real64 SensCoolEnergy; // sensible cooling output [J]
		Real64 LatCoolEnergyRate; // sensible cooling output [W]
		Real64 LatCoolEnergy; // sensible cooling output [J]
		Real64 ElecPower; // electricity consumed [W]
		Real64 ElecConsumption; // electricity consumed [J]
		Real64 FanPartLoadRatio; // fan part-load ratio for time step
		Real64 CompPartLoadRatio; // compressor part-load ratio for time step
		std::string AvailManagerListName; // Name of an availability manager list object
		int AvailStatus;
		int ZonePtr; // pointer to a zone served by a Window AC unit
		int HVACSizingIndex; // index of a HVACSizing object for a Window AC unit

		// Default Constructor
		WindACData() :
			UnitType( 0 ),
			SchedPtr( 0 ),
			FanSchedPtr( 0 ),
			FanAvailSchedPtr( 0 ),
			MaxAirVolFlow( 0.0 ),
			MaxAirMassFlow( 0.0 ),
			OutAirVolFlow( 0.0 ),
			OutAirMassFlow( 0.0 ),
			AirInNode( 0 ),
			AirOutNode( 0 ),
			OutsideAirNode( 0 ),
			AirReliefNode( 0 ),
			MixedAirNode( 0 ),
			OAMixIndex( 0 ),
			FanType_Num( 0 ),
			FanIndex( 0 ),
			DXCoilType_Num( 0 ),
			DXCoilIndex( 0 ),
			CoilOutletNodeNum( 0 ),
			OpMode( 0 ),
			FanPlace( 0 ),
			MaxIterIndex1( 0 ),
			MaxIterIndex2( 0 ),
			ConvergenceTol( 0.0 ),
			PartLoadFrac( 0.0 ),
			EMSOverridePartLoadFrac( false ),
			EMSValueForPartLoadFrac( 0.0 ),
			TotCoolEnergyRate( 0.0 ),
			TotCoolEnergy( 0.0 ),
			SensCoolEnergyRate( 0.0 ),
			SensCoolEnergy( 0.0 ),
			LatCoolEnergyRate( 0.0 ),
			LatCoolEnergy( 0.0 ),
			ElecPower( 0.0 ),
			ElecConsumption( 0.0 ),
			FanPartLoadRatio( 0.0 ),
			CompPartLoadRatio( 0.0 ),
			AvailStatus( 0 ),
			ZonePtr( 0 ),
			HVACSizingIndex( 0 )
		{}

	};

	struct WindACNumericFieldData
	{
		// Members
		Array1D_string FieldNames;

		// Default Constructor
		WindACNumericFieldData()
		{}

	};

	// Object Data
	extern Array1D< WindACData > WindAC;
	extern Array1D< WindACNumericFieldData > WindACNumericFields; // holds window AC numeric input fields character field name

	// Functions

	void
	clear_state();

	void
	SimWindowAC(
		std::string const & CompName, // name of the window AC unit
		int const ZoneNum, // number of zone being served
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & PowerMet, // Sensible power supplied by window AC (W)
		Real64 & LatOutputProvided, // Latent add/removal supplied by window AC (kg/s), dehumid = negative
		int & CompIndex // component index
	);

	void
	GetWindowAC();

	void
	InitWindowAC(
		int const WindACNum, // number of the current window AC unit being simulated
		Real64 & QZnReq, // zone load (modified as needed) (W)
		int const ZoneNum, // index to zone
		bool const FirstHVACIteration // TRUE when first HVAC iteration
	);

	void
	SizeWindowAC( int const WindACNum );

	void
	SimCyclingWindowAC(
		int const WindACNum, // number of the current window AC unit being simulated
		int const ZoneNum, // number of zone being served !unused1208
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & PowerMet, // Sensible power supplied (W)
		Real64 const QZnReq, // Sensible load to be met (W)
		Real64 & LatOutputProvided // Latent power supplied (kg/s), negative = dehumidification
	);

	void
	ReportWindowAC( int const WindACNum ); // number of the current AC unit being simulated

	void
	CalcWindowACOutput(
		int const WindACNum, // Unit index in fan coil array
		bool const FirstHVACIteration, // flag for 1st HVAV iteration in the time step
		int const OpMode, // operating mode: CycFanCycCoil | ContFanCycCoil
		Real64 const PartLoadFrac, // unit part load fraction
		bool const HXUnitOn, // Flag to toggle HX heat recovery as needed
		Real64 & LoadMet // load met by unit (watts)
	);

	void
	ControlCycWindACOutput(
		int const WindACNum, // Unit index in fan coil array
		bool const FirstHVACIteration, // flag for 1st HVAV iteration in the time step
		int const OpMode, // operating mode: CycFanCycCoil | ContFanCycCoil
		Real64 const QZnReq, // cooling output needed by zone [W]
		Real64 & PartLoadFrac, // unit part load fraction
		bool & HXUnitOn // Used to control HX heat recovery as needed
	);

	int
	GetWindowACZoneInletAirNode( int const WindACNum );

	int
	GetWindowACOutAirNode( int const WindACNum );

	int
	GetWindowACReturnAirNode( int const WindACNum );

	int
	GetWindowACMixedAirNode( int const WindACNum );

} // WindowAC

} // EnergyPlus

#endif
