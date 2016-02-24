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
#include <DataContaminantBalance.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSurfaces.hh>

namespace EnergyPlus {

namespace DataContaminantBalance {

	// MODULE INFORMATION:
	//       AUTHOR         Lixing Gu
	//       DATE WRITTEN   May 2010
	//       MODIFIED       Added generic contaminant in Jan. 2012 by L. Gu
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This module is revised from humidity ratio data straucture and contains the information
	// that is needed to pass from the Contaminant Balance Module.

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::AutoCalculate;
	using DataGlobals::DegToRadians;
	using DataSurfaces::MaxSlatAngs;

	// Data
	// module should be available to other modules and routines.  Thus,
	// all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:

	// MODULE VARIABLE Type DECLARATIONS:

	Array1D< Real64 > ZoneCO2SetPoint;
	Array1D< Real64 > CO2PredictedRate;

	Array1D< Real64 > ZoneCO2Gain; // CO2 gain from each Zone (People, equipment)
	Array1D< Real64 > ZoneCO2GainFromPeople; // CO2 gain from each Zone (From People only)

	// Zone Air Contaminant conditions variables
	Array1D< Real64 > ZoneAirCO2Avg; // AIR CO2 averaged over the zone time step
	Array1D< Real64 > ZoneAirCO2; // AIR CO2
	Array1D< Real64 > CO2ZoneTimeMinus1; // CO2 history terms for 3rd order derivative
	Array1D< Real64 > CO2ZoneTimeMinus2; // Time Minus 2 Zone Time Steps Term
	Array1D< Real64 > CO2ZoneTimeMinus3; // Time Minus 3 Zone Time Steps Term
	Array1D< Real64 > CO2ZoneTimeMinus4; // Time Minus 4 Zone Time Steps Term
	Array1D< Real64 > DSCO2ZoneTimeMinus1; // DownStepped CO2 history terms for 3rd order derivative
	Array1D< Real64 > DSCO2ZoneTimeMinus2; // DownStepped Time Minus 2 Zone Time Steps Term
	Array1D< Real64 > DSCO2ZoneTimeMinus3; // DownStepped Time Minus 3 Zone Time Steps Term
	Array1D< Real64 > DSCO2ZoneTimeMinus4; // DownStepped Time Minus 4 Zone Time Steps Term

	Array1D< Real64 > ZoneAirCO2Temp; // Temp zone air CO2 at time plus 1
	Array1D< Real64 > CO2ZoneTimeMinus1Temp; // Zone air CO2 at previous timestep
	Array1D< Real64 > CO2ZoneTimeMinus2Temp; // Zone air CO2 at timestep T-2
	Array1D< Real64 > CO2ZoneTimeMinus3Temp; // Zone air CO2 at timestep T-3
	Array1D< Real64 > ZoneAirCO2Old; // Last Time Steps Zone AIR Humidity Ratio

	Array1D< Real64 > ZoneCO2MX; // TEMPORARY ZONE CO2 TO TEST CONVERGENCE in Exact and Euler method
	Array1D< Real64 > ZoneCO2M2; // TEMPORARY ZONE CO2 at timestep t-2 in Exact and Euler method
	Array1D< Real64 > ZoneCO21; // Zone CO2 at the previous time step used in Exact and Euler method

	Array1D< Real64 > CONTRAT; // Zone CO2 at the previous time step used in Exact and Euler method

	Array1D< Real64 > MixingMassFlowCO2; // Mixing MASS FLOW * CO2

	int NumContControlledZones( 0 );

	Real64 OutdoorCO2( 0.0 ); // Outdoor CO2 level

	Array1D< Real64 > ZoneAirDensityCO; // Mixing MASS FLOW * CO2
	Array1D< Real64 > AZ;
	Array1D< Real64 > BZ;
	Array1D< Real64 > CZ;

	// Generic contaminant

	Array1D< Real64 > ZoneGCSetPoint;
	Array1D< Real64 > GCPredictedRate;

	Array1D< Real64 > ZoneGCGain; // Generic contaminant gain from each Zone (People, equipment)

	// Zone Air Contaminant conditions variables
	Array1D< Real64 > ZoneAirGCAvg; // AIR generic contaminant averaged over the zone time step
	Array1D< Real64 > ZoneAirGC; // AIR generic contaminant
	Array1D< Real64 > GCZoneTimeMinus1; // Generic contaminant history terms for 3rd order derivative
	Array1D< Real64 > GCZoneTimeMinus2; // Time Minus 2 Zone Time Steps Term
	Array1D< Real64 > GCZoneTimeMinus3; // Time Minus 3 Zone Time Steps Term
	Array1D< Real64 > GCZoneTimeMinus4; // Time Minus 4 Zone Time Steps Term
	Array1D< Real64 > DSGCZoneTimeMinus1; // DownStepped generic contaminant history terms for 3rd order
	// derivative
	Array1D< Real64 > DSGCZoneTimeMinus2; // DownStepped Time Minus 2 Zone Time Steps Term
	Array1D< Real64 > DSGCZoneTimeMinus3; // DownStepped Time Minus 3 Zone Time Steps Term
	Array1D< Real64 > DSGCZoneTimeMinus4; // DownStepped Time Minus 4 Zone Time Steps Term

	Array1D< Real64 > ZoneAirGCTemp; // Temp zone air generic contaminant at time plus 1
	Array1D< Real64 > GCZoneTimeMinus1Temp; // Zone air generic contaminant at previous timestep
	Array1D< Real64 > GCZoneTimeMinus2Temp; // Zone air generic contaminant at timestep T-2
	Array1D< Real64 > GCZoneTimeMinus3Temp; // Zone air generic contaminant at timestep T-3
	Array1D< Real64 > ZoneAirGCOld; // Last Time Steps Zone AIR generic contaminant

	Array1D< Real64 > ZoneGCMX; // TEMPORARY ZONE CO2 TO TEST CONVERGENCE in Exact and Euler method
	Array1D< Real64 > ZoneGCM2; // TEMPORARY ZONE CO2 at timestep t-2 in Exact and Euler method
	Array1D< Real64 > ZoneGC1; // Zone CO2 at the previous time step used in Exact and Euler method

	Array1D< Real64 > CONTRATGC; // Zone generic contaminant at the previous time step used in
	// Exact and Euler method

	Array1D< Real64 > MixingMassFlowGC; // Mixing MASS FLOW * generic contaminant

	Real64 OutdoorGC( 0.0 ); // Outdoor generic contaminant level

	Array1D< Real64 > ZoneAirDensityGC; // Mixing MASS FLOW * generic contaminant
	Array1D< Real64 > AZGC;
	Array1D< Real64 > BZGC;
	Array1D< Real64 > CZGC;

	// Object Data
	Array1D< ZoneSystemContaminantDemandData > ZoneSysContDemand;
	ContaminantData Contaminant; // A logical flag to determine whether any contaminants are simulated or not | CO2 simulation flag | CO2 outdoor level schedule pointer | Generic contaminant simulation flag | Generic contaminant outdoor level schedule pointer
	Array1D< ZoneContControls > ContaminantControlledZone;
	Array1D< ZoneContamGenericDataConstant > ZoneContamGenericConstant;
	Array1D< ZoneContamGenericDataPDriven > ZoneContamGenericPDriven;
	Array1D< ZoneContamGenericDataCutoff > ZoneContamGenericCutoff;
	Array1D< ZoneContamGenericDataDecay > ZoneContamGenericDecay;
	Array1D< ZoneContamGenericDataBLDiff > ZoneContamGenericBLDiff;
	Array1D< ZoneContamGenericDataDVS > ZoneContamGenericDVS;
	Array1D< ZoneContamGenericDataDRS > ZoneContamGenericDRS;

	// INTERFACE BLOCK SPECIFICATIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:

} // DataContaminantBalance

} // EnergyPlus
