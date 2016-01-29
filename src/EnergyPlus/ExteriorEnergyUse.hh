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

#ifndef ExteriorEnergyUse_hh_INCLUDED
#define ExteriorEnergyUse_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace ExteriorEnergyUse {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:
	extern int const ElecUse; // Electricity
	extern int const GasUse; // Gas (Natural)
	extern int const WaterUse; // Water
	extern int const CoalUse; // Coal
	extern int const FuelOil1Use; // FuelOil#1
	extern int const FuelOil2Use; // FuelOil#2
	extern int const LPGUse; // PropaneGas
	extern int const GasolineUse; // Gasoline
	extern int const DieselUse; // Diesel
	extern int const SteamUse; // Steam
	extern int const DistrictCoolUse; // Purchased Cooling
	extern int const DistrictHeatUse; // Purchased Heating
	extern int const OtherFuel1Use; // OtherFuel1
	extern int const OtherFuel2Use; // OtherFuel2

	extern int const ScheduleOnly; // exterior lights only on schedule
	extern int const AstroClockOverride; // exterior lights controlled to turn off during day.

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	extern int NumExteriorLights; // Number of Exterior Light Inputs
	extern int NumExteriorEqs; // Number of Exterior Equipment Inputs

	// SUBROUTINE SPECIFICATIONS FOR MODULE <module_name>

	// Clears the global data in ExteriorEnergyUse.
	// Needed for unit tests, should not be normally called.
	void
	clear_state();
	
	// Name Public routines, optionally name Private routines within this module

	// Types

	struct ExteriorLightUsage
	{
		// Members
		std::string Name; // Descriptive name -- will show on reporting
		int SchedPtr; // Can be scheduled
		Real64 DesignLevel; // Consumption in Watts
		Real64 Power; // Power = DesignLevel * ScheduleValue
		Real64 CurrentUse; // Use for this time step
		int ControlMode; // Control mode Schedule Only or Astronomical Clock plus schedule
		bool ManageDemand; // Flag to indicate whether to use demand limiting
		Real64 DemandLimit; // Demand limit set by demand manager [W]
		bool PowerActuatorOn; // EMS flag
		Real64 PowerActuatorValue; // EMS value
		Real64 SumConsumption; // sum of electric consumption [J] for reporting
		Real64 SumTimeNotZeroCons; // sum of time of positive electric consumption [hr]

		// Default Constructor
		ExteriorLightUsage() :
			SchedPtr( 0 ),
			DesignLevel( 0.0 ),
			Power( 0.0 ),
			CurrentUse( 0.0 ),
			ControlMode( 1 ),
			ManageDemand( false ),
			DemandLimit( 0.0 ),
			PowerActuatorOn( false ),
			SumConsumption( 0.0 ),
			SumTimeNotZeroCons( 0.0 )
		{}

	};

	struct ExteriorEquipmentUsage
	{
		// Members
		std::string Name; // Descriptive name -- will show on reporting
		int FuelType;
		int SchedPtr; // Can be scheduled
		Real64 DesignLevel; // Design Consumption (Watts, except for Water Equipment)
		Real64 Power; // Power = DesignLevel * ScheduleValue
		Real64 CurrentUse; // Use for this time step
		bool ManageDemand; // Flag to indicate whether to use demand limiting
		Real64 DemandLimit; // Demand limit set by demand manager [W]

		// Default Constructor
		ExteriorEquipmentUsage() :
			FuelType( 0 ),
			SchedPtr( 0 ),
			DesignLevel( 0.0 ),
			Power( 0.0 ),
			CurrentUse( 0.0 ),
			ManageDemand( false ),
			DemandLimit( 0.0 )
		{}

	};

	// Object Data
	extern Array1D< ExteriorLightUsage > ExteriorLights; // Structure for Exterior Light reporting
	extern Array1D< ExteriorEquipmentUsage > ExteriorEquipment; // Structure for Exterior Equipment Reporting

	// Functions

	void
	ManageExteriorEnergyUse();

	void
	GetExteriorEnergyUseInput();

	void
	ValidateFuelType(
		int & FuelTypeNumber, // Fuel Type to be set in structure.
		std::string const & FuelTypeAlpha, // Fuel Type String
		std::string & FuelTypeString, // Standardized Fuel Type String (for variable naming)
		std::string const & CurrentModuleObject, // object being parsed
		std::string const & CurrentField, // current field being parsed
		std::string const & CurrentName // current object name being parsed
	);

	void
	ReportExteriorEnergyUse();

} // ExteriorEnergyUse

} // EnergyPlus

#endif
