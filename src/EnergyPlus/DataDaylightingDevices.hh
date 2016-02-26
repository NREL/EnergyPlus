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

#ifndef DataDaylightingDevices_hh_INCLUDED
#define DataDaylightingDevices_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace DataDaylightingDevices {

	// Using/Aliasing

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:
	extern int const MaxTZones; // Maximum number of transition zones
	extern int const NumOfAngles; // Number of data points on transmittance vs. angle curve

	extern int const VisibleBeam; // Constant for radiation type
	extern int const SolarBeam; // Constant for radiation type
	extern int const SolarAniso; // Constant for radiation type
	extern int const SolarIso; // Constant for radiation type

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE TYPE DECLARATIONS:

	// INTERFACE BLOCK SPECIFICATIONS: na

	// MODULE VARIABLE DECLARATIONS:
	extern int NumOfTDDPipes; // Number of TDD pipes in the input file
	extern int NumOfShelf; // Number of daylighting shelves in the input file

	// Types

	struct TDDPipeData
	{
		// Members
		// Input variables
		std::string Name; // Name of TDD pipe
		int Dome; // Pointer to the dome object
		int Diffuser; // Pointer to the diffuser object
		int Construction; // Pointer to the construction object
		Real64 Diameter; // Pipe diameter
		Real64 TotLength; // Total length of pipe, including exterior
		Real64 Reff; // Effective R value between TDD:DOME and TDD:DIFFUSER
		int NumOfTZones; // Number of transition zone
		Array1D_int TZone; // Pointers to transition zones
		Array1D< Real64 > TZoneLength; // Length of pipe in each transition zone
		// Calculated variables
		Real64 AspectRatio; // Aspect ratio, length / diameter
		Real64 ReflectVis; // Visible reflectance of surface
		Real64 ReflectSol; // Solar reflectance of surface
		Array1D< Real64 > PipeTransVisBeam; // Table of beam visible transmittance vs. cosine angle
		Array1D< Real64 > PipeTransSolBeam; // Table of beam solar transmittance vs. cosine angle
		Real64 TransSolIso; // Diffuse isotropic solar transmittance (constant)
		Real64 TransSolHorizon; // Diffuse horizon solar transmittance (constant)
		Real64 ExtLength; // Exterior exposed length of pipe
		Array1D< Real64 > TZoneHeatGain; // convection gain to transition zones
		// Report variables
		Real64 TransmittedSolar; // Solar transmitted by the TDD [W]
		Real64 PipeAbsorbedSolar; // Solar absorbed in the walls of the pipe [W]
		Real64 HeatGain; // Solar heat gain [W]
		Real64 HeatLoss; // Solar heat loss [W]
		Real64 TransVisBeam; // TDD visible transmittance
		Real64 TransSolBeam; // TDD beam solar transmittance
		Real64 TransVisDiff; // TDD diffuse visible transmittance
		Real64 TransSolDiff; // TDD diffuse solar transmittance

		// Default Constructor
		TDDPipeData() :
			Dome( 0 ),
			Diffuser( 0 ),
			Construction( 0 ),
			Diameter( 0.0 ),
			TotLength( 0.0 ),
			Reff( 0.0 ),
			NumOfTZones( 0 ),
			AspectRatio( 0.0 ),
			ReflectVis( 0.0 ),
			ReflectSol( 0.0 ),
			PipeTransVisBeam( NumOfAngles, 0.0 ),
			PipeTransSolBeam( NumOfAngles, 0.0 ),
			TransSolIso( 0.0 ),
			TransSolHorizon( 0.0 ),
			ExtLength( 0.0 ),
			TransmittedSolar( 0.0 ),
			PipeAbsorbedSolar( 0.0 ),
			HeatGain( 0.0 ),
			HeatLoss( 0.0 ),
			TransVisBeam( 0.0 ),
			TransSolBeam( 0.0 ),
			TransVisDiff( 0.0 ),
			TransSolDiff( 0.0 )
		{}

	};

	struct ShelfData
	{
		// Members
		// Input variables
		std::string Name; // Name of daylighting shelf
		int Window; // Pointer to the window object
		int InSurf; // Pointer to the inside shelf heat transfer surface
		int OutSurf; // Pointer to the outside shelf attached shading surface
		int Construction; // Pointer to the outside shelf construction object
		// Calculated variables
		Real64 OutReflectVis; // Outside shelf visible reflectance
		Real64 OutReflectSol; // Outside shelf solar reflectance
		Real64 ViewFactor; // Outside shelf view factor to window
		// Report variables

		// Default Constructor
		ShelfData() :
			Window( 0 ),
			InSurf( 0 ),
			OutSurf( 0 ),
			Construction( 0 ),
			OutReflectVis( 0.0 ),
			OutReflectSol( 0.0 ),
			ViewFactor( 0.0 )
		{}

	};

	// Object Data
	extern Array1D< TDDPipeData > TDDPipe;
	extern Array1D< ShelfData > Shelf;

} // DataDaylightingDevices

} // EnergyPlus

#endif
