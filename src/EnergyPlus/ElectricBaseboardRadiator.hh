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

#ifndef ElectricBaseboardRadiator_hh_INCLUDED
#define ElectricBaseboardRadiator_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace ElectricBaseboardRadiator {

	// Using/Aliasing

	// Data
	//MODULE PARAMETER DEFINITIONS
	extern int const BaseboardRadiator_Electric;
	extern std::string const cCMO_BBRadiator_Electric;

	// DERIVED TYPE DEFINITIONS

	//MODULE VARIABLE DECLARATIONS:
	extern int NumElecBaseboards;
	extern Array1D< Real64 > QBBElecRadSource; // Need to keep the last value in case we are still iterating
	extern Array1D< Real64 > QBBElecRadSrcAvg; // Need to keep the last value in case we are still iterating
	extern Array1D< Real64 > ZeroSourceSumHATsurf; // Equal to the SumHATsurf for all the walls in a zone with no source
	// Record keeping variables used to calculate QBBRadSrcAvg locally
	extern Array1D< Real64 > LastQBBElecRadSrc; // Need to keep the last value in case we are still iterating
	extern Array1D< Real64 > LastSysTimeElapsed; // Need to keep the last value in case we are still iterating
	extern Array1D< Real64 > LastTimeStepSys; // Need to keep the last value in case we are still iterating
	extern Array1D_bool MySizeFlag;
	extern Array1D_bool CheckEquipName;
	//SUBROUTINE SPECIFICATIONS FOR MODULE BaseboardRadiator

	// Types

	struct ElecBaseboardParams
	{
		// Members
		std::string EquipName;
		int EquipType;
		std::string Schedule;
		Array1D_string SurfaceName;
		Array1D_int SurfacePtr;
		int ZonePtr;
		int SchedPtr;
		int TotSurfToDistrib;
		Real64 NominalCapacity;
		Real64 BaseboardEfficiency;
		Real64 AirInletTemp;
		Real64 AirInletHumRat;
		Real64 AirOutletTemp;
		Real64 ElecUseLoad;
		Real64 ElecUseRate;
		Real64 FracRadiant;
		Real64 FracConvect;
		Real64 FracDistribPerson;
		Real64 TotPower;
		Real64 Power;
		Real64 ConvPower;
		Real64 RadPower;
		Real64 TotEnergy;
		Real64 Energy;
		Real64 ConvEnergy;
		Real64 RadEnergy;
		Array1D< Real64 > FracDistribToSurf;
		int HeatingCapMethod; // - Method for electric baseboard heating capacity scalable sizing calculation
		Real64 ScaledHeatingCapacity; // - electric baseboard scaled maximum heating capacity {W} or scalable variable for sizing in {-}, or {W/m2}

		// Default Constructor
		ElecBaseboardParams() :
			EquipType( 0 ),
			ZonePtr( 0 ),
			SchedPtr( 0 ),
			TotSurfToDistrib( 0 ),
			NominalCapacity( 0.0 ),
			BaseboardEfficiency( 0.0 ),
			AirInletTemp( 0.0 ),
			AirInletHumRat( 0.0 ),
			AirOutletTemp( 0.0 ),
			ElecUseLoad( 0.0 ),
			ElecUseRate( 0.0 ),
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
			HeatingCapMethod( 0 ),
			ScaledHeatingCapacity( 0.0 )
		{}

	};

	struct ElecBaseboardNumericFieldData
	{
		// Members
		Array1D_string FieldNames;

		// Default Constructor
		ElecBaseboardNumericFieldData()
		{}

	};

	// Object Data
	extern Array1D< ElecBaseboardParams > ElecBaseboard;
	extern Array1D< ElecBaseboardNumericFieldData > ElecBaseboardNumericFields;

	// Functions

	void
	SimElecBaseboard(
		std::string const & EquipName,
		int const ActualZoneNum,
		int const ControlledZoneNum,
		bool const FirstHVACIteration,
		Real64 & PowerMet,
		int & CompIndex
	);

	void
	GetElectricBaseboardInput();

	void
	InitElectricBaseboard(
		int const BaseboardNum,
		int const ControlledZoneNumSub,
		bool const FirstHVACIteration
	);

	void
	SizeElectricBaseboard( int const BaseboardNum );

	void
	CalcElectricBaseboard(
		int const BaseboardNum,
		int const ControlledZoneNum
	);

	void
	UpdateElectricBaseboard( int const BaseboardNum );

	void
	UpdateBBElecRadSourceValAvg( bool & ElecBaseboardSysOn ); // .TRUE. if the radiant system has run this zone time step

	void
	DistributeBBElecRadGains();

	void
	ReportElectricBaseboard( int const BaseboardNum );

	Real64
	SumHATsurf( int const ZoneNum ); // Zone number

} // ElectricBaseboardRadiator

} // EnergyPlus

#endif
