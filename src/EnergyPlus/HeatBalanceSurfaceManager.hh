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

#ifndef HeatBalanceSurfaceManager_hh_INCLUDED
#define HeatBalanceSurfaceManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace HeatBalanceSurfaceManager {

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// na

	// DERIVED TYPE DEFINITIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:
	// na

	// Subroutine Specifications for the Heat Balance Module
	// Driver Routines

	// Initialization routines for module

	// Algorithms for the module
	// These old external subroutines have been moved into the namespace and are no longer externals
	// CalcHeatBalanceOutsideSurf  ! The heat balance routines are now public because the
	//  CalcHeatBalanceInsideSurf   ! radiant systems need access to them in order to simulate

	// Record Keeping/Utility Routines for Module

	// Reporting routines for module

	// Functions
	void
	clear_state();

	void
	ManageSurfaceHeatBalance();

	// Beginning Initialization Section of the Module
	//******************************************************************************

	void
	InitSurfaceHeatBalance();

	void
	GatherForPredefinedReport();

	void
	AllocateSurfaceHeatBalArrays();

	void
	InitThermalAndFluxHistories();

	void
	InitSolarHeatGains();

	void
	InitIntSolarDistribution();

	void
	ComputeIntThermalAbsorpFactors();

	void
	ComputeIntSWAbsorpFactors();

	void
	ComputeDifSolExcZonesWIZWindows( int const NumberOfZones ); // Number of zones

	void
	InitEMSControlledSurfaceProperties();

	void
	InitEMSControlledConstructions();

	// End Initialization Section of the Module
	//******************************************************************************

	// Begin Algorithm Section of the Module
	//******************************************************************************

	// Beginning of Record Keeping subroutines for the HB Module
	// *****************************************************************************

	void
	UpdateFinalSurfaceHeatBalance();

	void
	UpdateThermalHistories();

	void
	CalculateZoneMRT( Optional_int_const ZoneToResimulate = _ ); // if passed in, then only calculate surfaces that have this zone

	// End of Record Keeping subroutines for the HB Module
	// *****************************************************************************

	// Beginning of Reporting subroutines for the HB Module
	// *****************************************************************************

	void
	ReportSurfaceHeatBalance();

	// End of Reporting subroutines for the HB Module
	// *****************************************************************************



// *****************************************************************************
// *****************************************************************************
// *****************************************************************************
// *****************************************************************************

// Formerly EXTERNAL SUBROUTINES (heavily related to HeatBalanceSurfaceManager) but now moved into namespace HeatBalanceSurfaceManager

void
CalcHeatBalanceOutsideSurf( Optional_int_const ZoneToResimulate = _ ); // if passed in, then only calculate surfaces that have this zone

void
CalcHeatBalanceInsideSurf( Optional_int_const ZoneToResimulate = _ ); // if passed in, then only calculate surfaces that have this zone

void
CalcOutsideSurfTemp(
	int const SurfNum, // Surface number DO loop counter
	int const ZoneNum, // Zone number the current surface is attached to
	int const ConstrNum, // Construction index for the current surface
	Real64 const HMovInsul, // "Convection" coefficient of movable insulation
	Real64 const TempExt // Exterior temperature boundary condition
);

void
CalcExteriorVentedCavity( int const SurfNum ); // index of surface

void
GatherComponentLoadsSurfAbsFact();

} // HeatBalanceSurfaceManager
// *****************************************************************************
// *****************************************************************************
// *****************************************************************************
// *****************************************************************************


} // EnergyPlus

#endif
