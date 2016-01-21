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
#include <DataBSDFWindow.hh>
#include <DataGlobals.hh>
#include <DataPrecisionGlobals.hh>

namespace EnergyPlus {

namespace DataBSDFWindow {

	// Module containing the data definitions dealing with calculating window optical
	// properties from BSDF data

	// MODULE INFORMATION:
	//       AUTHOR         Joseph Klems, Brent Griffith
	//       DATE WRITTEN   August 2011
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// define the data structures to be used in calculating solar
	//  transmittance and absorptance and associated arsenal
	//  of geometry and window state information necessary

	// METHODOLOGY EMPLOYED:
	// Matrix representation of bidirectional transmittance of radiance

	// REFERENCES:
	// to be added--Complex glazing pubs, WINDOW writeup(s)(?)

	// OTHER NOTES:
	// see Joe's draft "Including Non-Specular Fenestrations in EnergyPlus"

	// USE STATEMENTS:
	// <use statements for data only modules>
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataVectorTypes;

	// <use statements for access to subroutines in other modules>

	// Data
	// MODULE PARAMETER DEFINITIONS:

	int const BasisType_WINDOW( 1 );
	int const BasisType_Custom( 2 );

	int const BasisSymmetry_Axisymmetric( 1 );
	int const BasisSymmetry_None( 2 );

	// Thermal calculations for complex fenestration can be used to generate reports for standard cases
	// noCondition is used when performing timestep calculations
	// summerCondtion will override certain parameters so that produced results are matching standard summer WINDOW (software) results
	// winterCondition will override certain parameters so that produced resuls are matching standard winter WINDOW (software) results
	int const noCondition( 0 );
	int const summerCondition( 1 );
	int const winterCondition( 2 );

	// DERIVED TYPE DEFINITIONS:

	// Structure to keep reference points coefficients for different reference points and illuminance maps

	//Allocation of complex fenestration data:  SurfaceWindow(:)%ComplexFen is a structure of type BSDFWindowDescript
	//defined in DataSurfaces.  ComplexWind(:) is an array of type BSDF WindowGeomDescr defined as a module
	//variable in WindowComplexManager

	// MODULE VARIABLE DECLARATIONS:

	int TotComplexFenStates( 0 ); // Number of complex fenestration construction definitions
	int FirstBSDF( 0 ); // Location of first complex fenestration construction definition in Constr array
	int MaxBkSurf( 20 ); // was 20    Maximum number of back surfaces in solar overlap & interior solar distribution
	int TotThermalModels( 0 ); // Number of thermal models
	//calculation
	Array3D< Real64 > SUNCOSTS( 60, 24, 3 ); // Timestep values of solar direction cosines
	Array2D< Real64 > BSDFTempMtrx; // Temporary matrix for holding axisymmetric input

	// Object Data
	Array1D< BSDFWindowGeomDescr > ComplexWind; // Window geometry structure: set in CalcPerSolarBeam/SolarShading

} // DataBSDFWindow

} // EnergyPlus
