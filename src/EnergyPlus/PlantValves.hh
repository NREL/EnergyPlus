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

#ifndef PlantValves_hh_INCLUDED
#define PlantValves_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace PlantValves {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// na

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	extern int NumTemperingValves;
	extern Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE <module_name>:

	// Types

	struct TemperValveData
	{
		// Members
		// user input data
		std::string Name; // User identifier
		int PltInletNodeNum; // Node number on the inlet side of the plant
		int PltOutletNodeNum; // Node number on the outlet side of the plant
		int PltStream2NodeNum; // Node number on the outlet side of the second stream
		int PltSetPointNodeNum; // Node number for the setpoint node.
		int PltPumpOutletNodeNum; // node number for the pump outlet (for flow rate)
		// Calculated and from elsewhere
		bool Init; // flag for initializationL true means do the initializations
		Real64 FlowDivFract; // Fraction of flow sent down diversion path
		Real64 Stream2SourceTemp; // Temperature [C] of stream 2 being mixed
		Real64 InletTemp; // Temperature [C] of inlet to valve
		Real64 SetPointTemp; // setpoint Temperatures [C] at control node.
		Real64 MixedMassFlowRate; // Flow rate downstream of mixer [kg/s]
		Real64 DivertedFlowRate; // flow rate through tempering valve's diversion path [kg/s]
		//loop topology variables
		int LoopNum;
		int LoopSideNum;
		int BranchNum;
		int CompNum;

		// Default Constructor
		TemperValveData() :
			PltInletNodeNum( 0 ),
			PltOutletNodeNum( 0 ),
			PltStream2NodeNum( 0 ),
			PltSetPointNodeNum( 0 ),
			PltPumpOutletNodeNum( 0 ),
			Init( true ),
			FlowDivFract( 0.0 ),
			Stream2SourceTemp( 0.0 ),
			InletTemp( 0.0 ),
			SetPointTemp( 0.0 ),
			MixedMassFlowRate( 0.0 ),
			DivertedFlowRate( 0.0 ),
			LoopNum( 0 ),
			LoopSideNum( 0 ),
			BranchNum( 0 ),
			CompNum( 0 )
		{}

	};

	// Object Data
	extern Array1D< TemperValveData > TemperValve; // dimension to No. of TemperingValve objects

	// Functions

	void
	SimPlantValves(
		int const CompTypeNum,
		std::string const & CompName,
		int & CompNum,
		bool const RunFlag, // unused1208
		bool & InitLoopEquip,
		Real64 & MyLoad, // unused1208
		Real64 & MaxCap,
		Real64 & MinCap,
		Real64 & OptCap,
		bool const FirstHVACIteration // TRUE if First iteration of simulation
	);

	void
	GetPlantValvesInput();

	void
	InitPlantValves(
		int const CompTypeNum,
		int const CompNum
	);

	void
	CalcPlantValves(
		int const CompTypeNum,
		int const CompNum
	);

	void
	UpdatePlantValves(
		int const CompTypeNum,
		int const CompNum
	);

	void
	ReportPlantValves();

} // PlantValves

} // EnergyPlus

#endif
