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

#ifndef EarthTube_hh_INCLUDED
#define EarthTube_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace EarthTube {

	// Using/Aliasing

	// Data
	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLES DECLARATIONS:
	extern int TotEarthTube; // Total EarthTube Statements in input
	// Parameters for Ventilation
	extern int const NaturalEarthTube;
	extern int const IntakeEarthTube;
	extern int const ExhaustEarthTube;

	//         Subroutine Specifications for the Heat Balance Module
	// Driver Routines

	// Get Input routines for module

	// Algorithms for the module

	// Reporting routines for module

	// Types

	struct EarthTubeData
	{
		// Members
		int ZonePtr;
		int SchedPtr;
		std::string SchedName;
		Real64 DesignLevel;
		Real64 MinTemperature;
		Real64 MaxTemperature;
		Real64 DelTemperature;
		int FanType;
		Real64 FanPressure;
		Real64 FanEfficiency;
		Real64 FanPower;
		Real64 GroundTempz1z2t; // ground temp between z1 and z2 at time t
		Real64 InsideAirTemp;
		Real64 AirTemp;
		Real64 r1; // Inner Pipe Radius (m)
		Real64 r2; // Pipe Thickness (m)
		Real64 r3; // Distance between Pipe Outer Surface and Undistubed Soil (m)
		Real64 PipeLength; // Entire Pipe Length
		Real64 PipeThermCond; // Pipe Thermal Conductivity
		Real64 z; // Depth under the Ground Surface (m)
		Real64 SoilThermDiff; // Soil Thermal Diffusivity
		Real64 SoilThermCond; // Soil Thermal Conductivity
		Real64 AverSoilSurTemp; // Average Soil Surface Temperature
		Real64 ApmlSoilSurTemp; // Amplitude of Soil Surface Temperature
		int SoilSurPhaseConst; // Phase constant of Soil Surface
		Real64 ConstantTermCoef;
		Real64 TemperatureTermCoef;
		Real64 VelocityTermCoef;
		Real64 VelocitySQTermCoef;

		// Default Constructor
		EarthTubeData() :
			ZonePtr( 0 ),
			SchedPtr( 0 ),
			DesignLevel( 0.0 ),
			MinTemperature( 0.0 ),
			MaxTemperature( 0.0 ),
			DelTemperature( 0.0 ),
			FanType( 0 ),
			FanPressure( 0.0 ),
			FanEfficiency( 0.0 ),
			FanPower( 0.0 ),
			GroundTempz1z2t( 0.0 ),
			InsideAirTemp( 0.0 ),
			AirTemp( 0.0 ),
			r1( 0.0 ),
			r2( 0.0 ),
			r3( 0.0 ),
			PipeLength( 0.0 ),
			PipeThermCond( 0.0 ),
			z( 0.0 ),
			SoilThermDiff( 0.0 ),
			SoilThermCond( 0.0 ),
			ConstantTermCoef( 0.0 ),
			TemperatureTermCoef( 0.0 ),
			VelocityTermCoef( 0.0 ),
			VelocitySQTermCoef( 0.0 )
		{}

	};

	struct EarthTubeZoneReportVars
	{
		// Members
		Real64 EarthTubeHeatLoss; // [J] Heat loss or cooling to zone from air delivered by earth tube
		Real64 EarthTubeHeatLossRate; // [W] Heat loss or cooling rate to zone from air delivered by earth tube
		Real64 EarthTubeHeatGain; // [J] Heat Gain to zone from air delivered by earth tube
		Real64 EarthTubeHeatGainRate; // [W] Heat Gain rate to zone from air delivered by earth tube
		Real64 EarthTubeOATreatmentPower; // [W] rate of heat transfer to/from air.  positive is heating OA to higher temp
		Real64 EarthTubeVolume; // Volume of Air {m3} due to EarthTube
		Real64 EarthTubeVolFlowRate; // Volume flow rate of air (m3/s) due to EarthTube
		Real64 EarthTubeVolFlowRateStd; // Volume flow rate of air (m3/s) due to EarthTube at standard air conditions
		Real64 EarthTubeMass; // Mass of Air {kg} due to EarthTube
		Real64 EarthTubeMassFlowRate; // Mass flow rate of air (kg/s) due to EarthTube
		Real64 EarthTubeFanElec; // [J] Fan Electricity consumed by EarthTube
		Real64 EarthTubeFanElecPower; // [W] Fan Electric power for EarthTube
		Real64 EarthTubeAirTemp; // Air Temp {C} of EarthTube, air leaving tube and entering zone

		// Default Constructor
		EarthTubeZoneReportVars() :
			EarthTubeHeatLoss( 0.0 ),
			EarthTubeHeatLossRate( 0.0 ),
			EarthTubeHeatGain( 0.0 ),
			EarthTubeHeatGainRate( 0.0 ),
			EarthTubeOATreatmentPower( 0.0 ),
			EarthTubeVolume( 0.0 ),
			EarthTubeVolFlowRate( 0.0 ),
			EarthTubeVolFlowRateStd( 0.0 ),
			EarthTubeMass( 0.0 ),
			EarthTubeMassFlowRate( 0.0 ),
			EarthTubeFanElec( 0.0 ),
			EarthTubeFanElecPower( 0.0 ),
			EarthTubeAirTemp( 0.0 )
		{}

	};

	// Object Data
	extern Array1D< EarthTubeData > EarthTubeSys;
	extern Array1D< EarthTubeZoneReportVars > ZnRptET;

	// Functions

	void
	ManageEarthTube();

	void
	GetEarthTube( bool & ErrorsFound ); // If errors found in input

	void
	CalcEarthTube();

	void
	ReportEarthTube();

	//        End of Module Subroutines for EarthTube

	//*****************************************************************************************

} // EarthTube

} // EnergyPlus

#endif
