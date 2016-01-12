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
#include <DataPhotovoltaics.hh>
#include <DataPrecisionGlobals.hh>

namespace EnergyPlus {

namespace DataPhotovoltaics {

	// MODULE INFORMATION:
	//       AUTHOR         D. Bradley
	//       DATE WRITTEN   May 2003
	//       MODIFIED       B. Griffith, Dec. 2003, heavy changes, moved derived types here from Photovoltaics.cc
	//                      B. Griffith, Feb 2008, added BIPV and inverter to one-diode model
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This data-only module is a repository for the variables that relate specifically
	// to the native EnergyPlus photovoltaics simulation.

	// METHODOLOGY EMPLOYED:

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:
	std::string const cPVGeneratorObjectName( "Generator:Photovoltaic" );
	std::string const cPVSimplePerfObjectName( "PhotovoltaicPerformance:Simple" );
	std::string const cPVEquiv1DiodePerfObjectName( "PhotovoltaicPerformance:EquivalentOne-Diode" );
	std::string const cPVSandiaPerfObjectName( "PhotovoltaicPerformance:Sandia" );

	int const iNotYetSetPVModel( 0 );
	int const iSimplePVModel( 1001 );
	int const iTRNSYSPVModel( 1002 );
	int const iSandiaPVModel( 1003 );

	int const iNotYetSetCellIntegration( 0 ); // cell temp method not set
	int const iDecoupledCellIntegration( 1 ); // cell temp method based on energy balance
	int const iDecoupledUllebergDynamicCellIntegration( 2 ); // cell temp method based on energy bal with capacity
	int const iSurfaceOutsideFaceCellIntegration( 3 ); // cell temp method based on coupling to E+'s heat balance
	int const iTranspiredCollectorCellIntegration( 4 ); // cell temp method based on coupling to unglazed transpired co
	int const iExteriorVentedCavityCellIntegration( 5 ); // cell temp method based on coupling to nat vent exterior cavi
	int const iPVTSolarCollectorCellIntegration( 6 ); // cell temp method based on coupling to PVT model

	int const FixedEfficiency( 10 ); // simple PV, constant efficiency
	int const ScheduledEfficiency( 11 ); // simpel PV, scheduled efficiency

	int const CrystallineSiPVCells( 1 );
	int const AmorphousSiPVCells( 2 );

	Real64 const MinIrradiance( 0.3 ); // [W/m2] Assume no operation if Ic is below this number (W/m2)
	// DERIVED TYPE DEFINITIONS

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// MODULE VARIABLE DECLARATIONS:
	int NumPVs( 0 ); // count of number of PV generators
	int Num1DiodePVModuleTypes( 0 ); // count for Equivalent one-diode model
	int NumSimplePVModuleTypes( 0 ); // count of number of input objs for simple model
	int NumSNLPVModuleTypes( 0 ); // count of number of input objs for Sandia model

	Real64 ShuntResistance( 0.0 ); // old "RSH" in common block of trnsys code

	// Object Data
	Array1D< PVArrayStruct > PVarray;

	// ___________________________________________________________________________

	//     EnergyPlus V1.2 and beyond include models for photovoltaic calculations called
	//     Generator:Photovoltaic:Simple and Generator:PV:Sandia implemented by the Center for
	//     Buildings and Thermal Systems, National Renewable Energy Laboratory, 1617 Cole Blvd
	//     MS 2722, Golden, CO, 80401
	//     EnergyPlus v1.1.1 and beyond includes model for Photovoltaic calculations, now
	//     referred to as the Generator:PV:Equivalent One-Diode model developed by Thermal Energy
	//     System Specialists, 2916 Marketplace Drive, Suite 104, Madison, WI 53719;
	//     Tel: (608) 274-2577

} // DataPhotovoltaics

} // EnergyPlus
