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

// EnergyPlus Headers
#include <DataWater.hh>
#include <DataPrecisionGlobals.hh>

namespace EnergyPlus {

namespace DataWater {

	// Module containing the routines dealing with the DataWater

	// MODULE INFORMATION:
	//       AUTHOR         B. Griffith
	//       DATE WRITTEN   August 2006
	//       MODIFIED       D. Sailor -- to add ecoroof irrigation
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This data-only module is a repository for the variables that relate specifically
	// to the management of water in the simulation

	// METHODOLOGY EMPLOYED:
	// <description>

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLI

	// MODULE PARAMETER DEFINITION

	int const ScheduledTankTemp( 101 ); // tank water temperature is user input via schedule
	int const TankZoneThermalCoupled( 102 ); // tank water temperature is modeled using simple UA

	int const RainSchedDesign( 201 ); // mode of Rainfall determination is Scheduled Design
	int const IrrSchedDesign( 202 ); // mode of Irrigation determination is Scheduled Design (DJS -PSU)
	int const IrrSmartSched( 203 ); // mode of irrigation DJS - PSU

	int const ConstantRainLossFactor( 301 );
	int const ScheduledRainLossFactor( 302 );

	int const AmbientTempSchedule( 1 ); // ambient temperature around tank (or HPWH inlet air) is scheduled
	int const AmbientTempZone( 2 ); // tank is located in a zone or HPWH inlet air is zone air only
	int const AmbientTempExterior( 3 ); // tank is located outdoors or HPWH inlet air is outdoor air only

	int const ConstantWaterTable( 401 );
	int const ScheduledWaterTable( 402 );

	int const NoControlLevel( 501 );
	int const MainsFloatValve( 502 );
	int const WellFloatValve( 503 );
	int const WellFloatMainsBackup( 504 );
	int const OtherTankFloatValve( 505 );
	int const TankMainsBackup( 506 );

	int const OverflowDiscarded( 601 );
	int const OverflowToTank( 602 );

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	int NumWaterStorageTanks( 0 ); // number of water Storage tanks in model
	int NumRainCollectors( 0 ); // number of rainfall collectors in model
	int NumGroundWaterWells( 0 ); // number of
	int NumSiteRainFall( 0 );
	int NumIrrigation( 0 ); // DJS PSU Dec 2006 number of irrigation descriptions (1 allowed)
	bool AnyWaterSystemsInModel( false ); // control flag set true if any water systems
	bool WaterSystemGetInputCalled( false ); // set true once input data gotten.
	bool AnyIrrigationInModel( false ); // control flag set true if irrigation input for ecoroof DJS PSU Dec 2006

	// Object Data
	SiteRainFallDataStruct RainFall; // type of rainfall modeling | design annual rain | rain sched id | nominal annual rain | current rate | current amount
	IrrigationDataStruct Irrigation; // type of irrigation modeling | Irrigation schedule id | scheduled amount | actual amount | irrigation threshold
	Array1D< StorageTankDataStruct > WaterStorage;
	Array1D< RainfallCollectorDataStruct > RainCollector;
	Array1D< GroundwaterWellDataStruct > GroundwaterWell;

} // DataWater

} // EnergyPlus
