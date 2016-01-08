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

#ifndef OutsideEnergySources_hh_INCLUDED
#define OutsideEnergySources_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace OutsideEnergySources {

	// Using/Aliasing

	// Data
	//MODULE PARAMETER DEFINITIONS
	extern int const EnergyType_DistrictHeating;
	extern int const EnergyType_DistrictCooling;

	// DERIVED TYPE DEFINITIONS

	//MODULE VARIABLE DECLARATIONS:
	extern int NumDistrictUnits;

	// SUBROUTINE SPECIFICATIONS FOR MODULE OutsideEnergySources

	// Types

	struct OutsideEnergySourceSpecs
	{
		// Members
		std::string PlantLoopID; // main plant loop ID
		std::string SecndryLoopID; // secondary chiller loop (cond loop) ID
		std::string ScheduleID; // equipment availability schedule
		std::string Name; // user identifier
		Real64 NomCap; // design nominal capacity of district service
		bool NomCapWasAutoSized; // ture if Nominal Capacity was autosize on input
		int CapFractionSchedNum; // capacity modifier schedule number
		int InletNodeNum; // Node number on the inlet side of the plant
		int OutletNodeNum; // Node number on the inlet side of the plant
		Real64 EnergyTransfer; // cooling energy provided in time step
		Real64 EnergyRate; // cooling power
		int EnergyType; // flag for district heating OR cooling
		int MassFlowReSimIndex;
		//loop topology variables
		int LoopNum;
		int LoopSideNum;
		int BranchNum;
		int CompNum;
		//flags
		bool OneTimeInitFlag;
		bool BeginEnvrnInitFlag;
		bool CheckEquipName;

		// Default Constructor
		OutsideEnergySourceSpecs() :
			NomCap( 0.0 ),
			NomCapWasAutoSized( false ),
			CapFractionSchedNum( 0 ),
			InletNodeNum( 0 ),
			OutletNodeNum( 0 ),
			EnergyTransfer( 0.0 ),
			EnergyRate( 0.0 ),
			EnergyType( 0 ),
			MassFlowReSimIndex( 0 ),
			LoopNum( 0 ),
			LoopSideNum( 0 ),
			BranchNum( 0 ),
			CompNum( 0 ),
			OneTimeInitFlag( true ),
			BeginEnvrnInitFlag( true ),
			CheckEquipName( true )
		{}

	};

	struct ReportVars
	{
		// Members
		Real64 MassFlowRate;
		Real64 InletTemp;
		Real64 OutletTemp;
		Real64 EnergyTransfer;

		// Default Constructor
		ReportVars() :
			MassFlowRate( 0.0 ),
			InletTemp( 0.0 ),
			OutletTemp( 0.0 ),
			EnergyTransfer( 0.0 )
		{}
	};

	// Object Data
	extern Array1D< OutsideEnergySourceSpecs > EnergySource;
	extern Array1D< ReportVars > EnergySourceReport;

	// Functions
	void
	clear_state();

	void
	SimOutsideEnergy(
		std::string const & EnergyType,
		std::string const & EquipName,
		int const EquipFlowCtrl, // Flow control mode for the equipment
		int & CompIndex,
		bool const RunFlag,
		bool const InitLoopEquip,
		Real64 & MyLoad,
		Real64 & MaxCap,
		Real64 & MinCap,
		Real64 & OptCap,
		bool const FirstHVACIteration
	);

	// End OutsideEnergySources Module Driver Subroutines
	//******************************************************************************

	// Beginning of OutsideEnergySources Module Get Input subroutines
	//******************************************************************************

	void
	GetOutsideEnergySourcesInput();

	// End of Get Input subroutines for the OutsideEnergySources Module
	//******************************************************************************

	// Beginning Initialization Section of the OutsideEnergySources Module
	//******************************************************************************

	void
	InitSimVars(
		int const EnergySourceNum, // Which item being initialized
		Real64 & MassFlowRate,
		Real64 & InletTemp,
		Real64 & OutletTemp,
		Real64 const MyLoad
	);

	// End Initialization Section of the OutsideEnergySources Module
	//******************************************************************************

	// Beginning of OutsideEnergySources Module Utility Subroutines
	// *****************************************************************************

	void
	SizeDistrictEnergy(
		int const EnergySourceNum
	);

	void
	SimDistrictEnergy(
		bool const RunFlag,
		int const DistrictEqNum,
		Real64 & MyLoad,
		Real64 const MassFlowRate,
		Real64 const InletTemp,
		Real64 & OutletTemp
	);

	// End of OutsideEnergySources Module Utility Subroutines
	// *****************************************************************************

	// Beginning of Record Keeping subroutines for the OutsideEnergySources Module
	// *****************************************************************************

	void
	UpdateRecords(
		Real64 const MyLoad,
		int const EqNum,
		Real64 const MassFlowRate,
		Real64 const OutletTemp
	);

	// End of Record Keeping subroutines for the OutsideEnergySources Module
	// *****************************************************************************

} // OutsideEnergySources

} // EnergyPlus

#endif
