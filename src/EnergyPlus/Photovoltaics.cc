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

// C++ Headers
#include <cassert>
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <Photovoltaics.hh>
#include <DataEnvironment.hh>
#include <DataGlobalConstants.hh>
#include <DataGlobals.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHeatBalSurface.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataPhotovoltaics.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSurfaces.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <OutputProcessor.hh>
#include <PhotovoltaicThermalCollectors.hh>
#include <ScheduleManager.hh>
#include <TranspiredCollector.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace Photovoltaics {
	//       MODULE INFORMATION:
	//       AUTHOR         David Bradley
	//       DATE WRITTEN   January 2003
	//       MODIFIED       B. Griffith, dec2003 - Jan2004
	//                      added Sandia PV model loosely based on G. Barker's implementation for TRNSYS type
	//                      added Simple PV efficiency model for early design phases
	//       RE-ENGINEERED  added case statement to allow selecting and mixing between different models
	//                      moved derived types to DataPhotovoltaics
	//                      B. Griffith, Aug. 2008, refactored PV data structures and input objects to
	//                       so that there is one Generator:Photovoltaics object with 3 different model options.

	// PURPOSE OF THIS MODULE:
	// This module collects routines used to simulate the timestep by timestep performance of a
	// photovoltaic arrays.  The user can select between different models by choosing an a model and performance input object
	// Using the input object "PhotovoltaicPerformance:Simple" will lead to modeling the PV system using
	// crude model that just applies a power conversion efficiency factor, much simpler to specify
	// Using the input object "PhotovoltaicPerformance:EquivalentOne-Diode" will lead to modeling the PV system using
	// The PV model used as the basis for this module is Type180 from the HYDROGEMS library developed by
	// Oystein Ulleberg at the IFE Institute for Energy Technology in Norway and also work by Eckstein

	// Using the input object, "PhotovoltaicPerformance:SANDIA"  will lead to modeling a PV array
	//  using models developed by David King, Sandia National lab.  These models appear to provide
	//  improved prediction of PV performance at low radiance and incident angles.

	// METHODOLOGY EMPLOYED: This module contains routines to manage PV system models.
	//  There are two options for what model to use and this duality of modeling approaches is
	//  reflected in there being two groups of routines for each PV model, The original model is
	//  referred to as Equivalent one-diode model and has origins as a TRNSYS type180 from the Hydrogems library
	//  A newer model with more involved input has been developed by Sandia National Lab (SNL) by David King.
	//  The TRNSYS type180 model include the use of numerical routines to minimize a multivariate function

	// REFERENCES:

	// OTHER NOTES: none

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataPhotovoltaics;
	using DataGlobals::BeginSimFlag;
	using DataGlobals::BeginEnvrnFlag;
	using DataGlobals::BeginDayFlag;
	using DataGlobals::EndEnvrnFlag;
	using DataGlobals::SecInHour;
	using DataGlobals::KelvinConv;
	using DataHVACGlobals::TimeStepSys;

	// Data
	//MODULE PARAMETER DEFINITIONS:
	// na

	// DERIVED TYPE DEFINITIONS:
	//   see DataPhotovoltaics.cc

	Array1D_bool CheckEquipName;

	//SUBROUTINE SPECIFICATIONS FOR MODULE Photovoltaics

	// The following subroutines are used for the SIMPLE model

	// The following subroutines and functions are used for only the EQUIVALENT ONE-DIODE model

	// The following subroutines and functions are used for the Sandia model.

	//  OO get set methods for coupling to exterior vented baffle cavity mounting configurations

	// *************

	// Functions

	void
	SimPVGenerator(
		int const EP_UNUSED( GeneratorType ), // type of Generator !unused1208
		std::string const & GeneratorName, // user specified name of Generator
		int & GeneratorIndex,
		bool const RunFlag, // is PV ON or OFF as determined by schedules in ElecLoadCenter
		Real64 const EP_UNUSED( PVLoad ) // electrical load on the PV (not really used... PV models assume "full on" !unused1208
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         David Bradley
		//       DATE WRITTEN   April 2003
		//       MODIFIED       B. Griffith Jan 2004
		//                      B. Griffith Aug. 2008 Rework for new structure
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is in charge of all the rest of the subroutines contained
		// in this module. provides common entry point for all the models

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		//unused0909  USE DataEnvironment, ONLY : EnvironmentName, DayOfYear
		//unused0909  USE DataGlobals, ONLY: BeginEnvrnFlag, EndEnvrnFlag
		using General::TrimSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na)

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int PVnum; // index of unit in PV array for Equivalent one-diode model
		static bool GetInputFlag( true ); // one time get input flag

		//Get PV data from input file
		if ( GetInputFlag ) {
			GetPVInput(); // for all three types of models
			GetInputFlag = false;
		}

		if ( GeneratorIndex == 0 ) {
			PVnum = FindItemInList( GeneratorName, PVarray );
			if ( PVnum == 0 ) {
				ShowFatalError( "SimPhotovoltaicGenerator: Specified PV not one of valid Photovoltaic Generators " + GeneratorName );
			}
			GeneratorIndex = PVnum;
		} else {
			PVnum = GeneratorIndex;
			if ( PVnum > NumPVs || PVnum < 1 ) {
				ShowFatalError( "SimPhotovoltaicGenerator: Invalid GeneratorIndex passed=" + TrimSigDigits( PVnum ) + ", Number of PVs=" + TrimSigDigits( NumPVs ) + ", Generator name=" + GeneratorName );
			}
			if ( CheckEquipName( PVnum ) ) {
				if ( GeneratorName != PVarray( PVnum ).Name ) {
					ShowFatalError( "SimPhotovoltaicGenerator: Invalid GeneratorIndex passed=" + TrimSigDigits( PVnum ) + ", Generator name=" + GeneratorName + ", stored PV Name for that index=" + PVarray( PVnum ).Name );
				}
				CheckEquipName( PVnum ) = false;
			}
		}

		{ auto const SELECT_CASE_var( PVarray( PVnum ).PVModelType ); //SELECT and CALL MODELS based on model type

		if ( SELECT_CASE_var == iSimplePVModel ) {

			CalcSimplePV( PVnum, RunFlag );

		} else if ( SELECT_CASE_var == iTRNSYSPVModel ) {
			// 'PhotovoltaicPeformance:EquivalentOne-Diode' (aka. 5-parameter TRNSYS type 180 model)

			InitTRNSYSPV( PVnum );

			CalcTRNSYSPV( PVnum, RunFlag );

		} else if ( SELECT_CASE_var == iSandiaPVModel ) {
			// 'PhotovoltaicPerformance:Sandia' (aka. King model, Sandia Nat. Labs.)

			CalcSandiaPV( PVnum, RunFlag );

		} else {

			ShowFatalError( "Specified generator model type not found for PV generator = " + GeneratorName );

		}}

		ReportPV( PVnum );

	}

	void
	GetPVGeneratorResults(
		int const EP_UNUSED( GeneratorType ), // type of Generator !unused1208
		int const GeneratorIndex,
		Real64 & GeneratorPower, // electrical power
		Real64 & GeneratorEnergy, // electrical energy
		Real64 & ThermalPower,
		Real64 & ThermalEnergy
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   Aug. 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// provide a "get" method to collect results for individual electic load centers.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// Using/Aliasing
		using PhotovoltaicThermalCollectors::GetPVTThermalPowerProduction;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		GeneratorPower = PVarray( GeneratorIndex ).Report.DCPower;
		GeneratorEnergy = PVarray( GeneratorIndex ).Report.DCEnergy;
		// PVT may add thermal
		if ( PVarray( GeneratorIndex ).CellIntegrationMode == iPVTSolarCollectorCellIntegration ) {
			// get result for thermal power generation
			GetPVTThermalPowerProduction( GeneratorIndex, ThermalPower, ThermalEnergy );
		} else {
			ThermalPower = 0.0;
			ThermalEnergy = 0.0;
		}

	}

	// *************

	void
	GetPVInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         David Bradley
		//       DATE WRITTEN   January 2003
		//       MODIFIED       B.Griffith Dec. 2003 - Jan 2004 added input for Simple and Sandia PV model
		//                      B. Griffith Feb. 2008 - revised input for TRNSYS pv model for BIPV and inverter
		//                      B. Griffith Aug. 2008 - revised input for new organization and naming convention
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine gets the input for the Photovoltaic units saving it in
		// the data structures defined in DataPhotovoltaics.cc.

		// METHODOLOGY EMPLOYED:
		// subroutine structure taken from Beta2 BaseboardRadiator.cc

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::FindItemInList;
		using InputProcessor::SameString;
		using namespace DataIPShortCuts;
		using DataGlobals::KelvinConv;
		//unused0909  USE DataEnvironment, ONLY: Longitude, TimeZoneMeridian
		using DataSurfaces::Surface;
		using namespace DataHeatBalance;
		using ScheduleManager::GetScheduleIndex;
		using TranspiredCollector::GetTranspiredCollectorIndex;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int PVnum; // working variable for do loop through pv arrays
		int SurfNum; // working variable for surface id in Heat Balance domain
		int ModNum; // working variable for do loop through Sandia model parameter input
		int NumAlphas; // Number of PV Array parameter alpha names being passed
		int NumNums; // Number of PV Array numeric parameters are being passed
		int IOStat;
		static bool ErrorsFound( false ); // if errors detected in input
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		int ThisParamObj;
		int dupPtr;

		// Object Data
		Array1D< SimplePVParamsStruct > tmpSimpleModuleParams; // temporary, for processing input data
		Array1D< TRNSYSPVModuleParamsStruct > tmpTNRSYSModuleParams; // temporary, for processing input data
		Array1D< SNLModuleParamsStuct > tmpSNLModuleParams; // temporary, for processing input data

		// count how many photovoltaic arrays of different types are in the .idf
		NumPVs = GetNumObjectsFound( cPVGeneratorObjectName );
		NumSimplePVModuleTypes = GetNumObjectsFound( cPVSimplePerfObjectName );
		Num1DiodePVModuleTypes = GetNumObjectsFound( cPVEquiv1DiodePerfObjectName );
		NumSNLPVModuleTypes = GetNumObjectsFound( cPVSandiaPerfObjectName );

		if ( NumPVs <= 0 ) {
			ShowSevereError( "Did not find any " + cPVGeneratorObjectName );
			return;
		}

		if ( ! allocated( PVarray ) ) PVarray.allocate( NumPVs );
		CheckEquipName.dimension( NumPVs, true );

		cCurrentModuleObject = cPVGeneratorObjectName;
		for ( PVnum = 1; PVnum <= NumPVs; ++PVnum ) {
			GetObjectItem( cCurrentModuleObject, PVnum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), PVarray, PVnum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			PVarray( PVnum ).Name = cAlphaArgs( 1 );

			PVarray( PVnum ).SurfaceName = cAlphaArgs( 2 );
			PVarray( PVnum ).SurfacePtr = FindItemInList( cAlphaArgs( 2 ), Surface );
			// required-surface
			if ( lAlphaFieldBlanks( 2 ) ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 2 ) + " = " + cAlphaArgs( 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
				ShowContinueError( "Surface name cannot be blank" );
				ErrorsFound = true;
			}
			if ( PVarray( PVnum ).SurfacePtr == 0 ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 2 ) + " = " + cAlphaArgs( 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			} else {
				// Found one -- make sure has right parameters for PV
				SurfNum = PVarray( PVnum ).SurfacePtr;
				Surface( SurfNum ).IsPV = true;

				if ( ! Surface( SurfNum ).ExtSolar ) {
					ShowWarningError( "Invalid " + cAlphaFieldNames( 2 ) + " = " + cAlphaArgs( 2 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
					ShowContinueError( "Surface is not exposed to solar, check surface bounday condition" );
				}

				// check surface orientation, warn if upside down
				if ( ( Surface( SurfNum ).Tilt < -95.0 ) || ( Surface( SurfNum ).Tilt > 95.0 ) ) {
					ShowWarningError( "Suspected input problem with " + cAlphaFieldNames( 2 ) + " = " + cAlphaArgs( 2 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
					ShowContinueError( "Surface used for solar collector faces down" );
					ShowContinueError( "Surface tilt angle (degrees from ground outward normal) = " + RoundSigDigits( Surface( SurfNum ).Tilt, 2 ) );
				}
			}

			PVarray( PVnum ).PVModelType = iNotYetSetPVModel;
			if ( SameString( cAlphaArgs( 3 ), cPVSimplePerfObjectName ) ) {
				PVarray( PVnum ).PVModelType = iSimplePVModel;
			} else if ( SameString( cAlphaArgs( 3 ), cPVEquiv1DiodePerfObjectName ) ) {
				PVarray( PVnum ).PVModelType = iTRNSYSPVModel;
			} else if ( SameString( cAlphaArgs( 3 ), cPVSandiaPerfObjectName ) ) {
				PVarray( PVnum ).PVModelType = iSandiaPVModel;
			} else { // throw error, did not find module performance type
				if ( lAlphaFieldBlanks( 3 ) ) {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 3 ) + " = " + cAlphaArgs( 3 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
					ShowContinueError( "Field cannot be blank" );
					ErrorsFound = true;
				} else {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 3 ) + " = " + cAlphaArgs( 3 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
					ShowContinueError( "Did not recognize entry" );
					ErrorsFound = true;
				}
			}
			PVarray( PVnum ).PerfObjName = cAlphaArgs( 4 ); // check later once perf objects are loaded

			PVarray( PVnum ).CellIntegrationMode = iNotYetSetCellIntegration;
			if ( SameString( cAlphaArgs( 5 ), "Decoupled" ) ) {
				PVarray( PVnum ).CellIntegrationMode = iDecoupledCellIntegration;
			} else if ( SameString( cAlphaArgs( 5 ), "DecoupledUllebergDynamic" ) ) {
				PVarray( PVnum ).CellIntegrationMode = iDecoupledUllebergDynamicCellIntegration;
			} else if ( SameString( cAlphaArgs( 5 ), "IntegratedSurfaceOutsideFace" ) ) {
				PVarray( PVnum ).CellIntegrationMode = iSurfaceOutsideFaceCellIntegration;
			} else if ( SameString( cAlphaArgs( 5 ), "IntegratedTranspiredCollector" ) ) {
				PVarray( PVnum ).CellIntegrationMode = iTranspiredCollectorCellIntegration;
			} else if ( SameString( cAlphaArgs( 5 ), "IntegratedExteriorVentedCavity" ) ) {
				PVarray( PVnum ).CellIntegrationMode = iExteriorVentedCavityCellIntegration;
			} else if ( SameString( cAlphaArgs( 5 ), "PhotovoltaicThermalSolarCollector" ) ) {
				PVarray( PVnum ).CellIntegrationMode = iPVTSolarCollectorCellIntegration;
			} else {
				if ( lAlphaFieldBlanks( 5 ) ) {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 5 ) + " = " + cAlphaArgs( 5 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
					ShowContinueError( "Field cannot be blank" );
					ErrorsFound = true;
				} else {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 5 ) + " = " + cAlphaArgs( 5 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
					ShowContinueError( "Did not recognize entry" );
					ErrorsFound = true;
				}
			}

			PVarray( PVnum ).NumSeriesNParall = rNumericArgs( 1 );
			PVarray( PVnum ).NumModNSeries = rNumericArgs( 2 );

		} // main PV array objects

		// search for duplicate PV arrays on integrated heat transfer surfaces, accumulating source terms across arrays is not supported
		for ( PVnum = 1; PVnum <= NumPVs; ++PVnum ) {
			IsNotOK = false;
			{ auto const SELECT_CASE_var( PVarray( PVnum ).CellIntegrationMode );

			if ( ( SELECT_CASE_var == iSurfaceOutsideFaceCellIntegration ) || ( SELECT_CASE_var == iTranspiredCollectorCellIntegration ) || ( SELECT_CASE_var == iExteriorVentedCavityCellIntegration ) ) {
				dupPtr = FindItemInList( PVarray( PVnum ).SurfaceName, PVarray( {PVnum + 1,NumPVs} ), &PVArrayStruct::SurfaceName );
				if ( dupPtr != 0 ) dupPtr += PVnum; // to correct for shortened array in find item
				if ( dupPtr != 0 ) {
					if ( PVarray( dupPtr ).CellIntegrationMode == iSurfaceOutsideFaceCellIntegration ) {
						ShowSevereError( cCurrentModuleObject + ": problem detected with multiple PV arrays." );
						ShowContinueError( "When using IntegratedSurfaceOutsideFace heat transfer mode, only one PV array can be coupled" );
						ShowContinueError( "Both " + PVarray( PVnum ).Name + " and " + PVarray( dupPtr ).Name + " are using surface " + PVarray( PVnum ).SurfaceName );
						ErrorsFound = true;
					} else if ( PVarray( dupPtr ).CellIntegrationMode == iTranspiredCollectorCellIntegration ) {
						ShowSevereError( cCurrentModuleObject + ": problem detected with multiple PV arrays." );
						ShowContinueError( "When using IntegratedTranspiredCollector heat transfer mode, only one PV array can be coupled" );
						ShowContinueError( "Both " + PVarray( PVnum ).Name + " and " + PVarray( dupPtr ).Name + " are using UTSC surface = " + PVarray( PVnum ).SurfaceName );
						ErrorsFound = true;
					} else if ( PVarray( dupPtr ).CellIntegrationMode == iExteriorVentedCavityCellIntegration ) {
						ShowSevereError( cCurrentModuleObject + ": problem detected with multiple PV arrays." );
						ShowContinueError( "When using IntegratedExteriorVentedCavity heat transfer mode, only one PV array can be coupled" );
						ShowContinueError( "Both " + PVarray( PVnum ).Name + " and " + PVarray( dupPtr ).Name + " are using exterior vented surface = " + PVarray( PVnum ).SurfaceName );
						ErrorsFound = true;
					}
				}
			}}
		}

		if ( NumSimplePVModuleTypes > 0 ) {
			tmpSimpleModuleParams.allocate( NumSimplePVModuleTypes );
			cCurrentModuleObject = cPVSimplePerfObjectName;
			for ( ModNum = 1; ModNum <= NumSimplePVModuleTypes; ++ModNum ) {
				GetObjectItem( cCurrentModuleObject, ModNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				IsNotOK = false;
				IsBlank = false;
				VerifyName( cAlphaArgs( 1 ), tmpSimpleModuleParams, ModNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
				if ( IsNotOK ) { //repeat or blank name so don't add
					ErrorsFound = true;
					continue;
				}
				tmpSimpleModuleParams( ModNum ).Name = cAlphaArgs( 1 );
				tmpSimpleModuleParams( ModNum ).ActiveFraction = rNumericArgs( 1 );

				if ( SameString( cAlphaArgs( 2 ), "Fixed" ) ) {
					tmpSimpleModuleParams( ModNum ).EfficencyInputMode = FixedEfficiency;
				} else if ( SameString( cAlphaArgs( 2 ), "Scheduled" ) ) {
					tmpSimpleModuleParams( ModNum ).EfficencyInputMode = ScheduledEfficiency;
				} else {
					if ( lAlphaFieldBlanks( 2 ) ) {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 2 ) + " = " + cAlphaArgs( 2 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
						ShowContinueError( "Field cannot be blank" );
						ErrorsFound = true;
					} else {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 2 ) + " = " + cAlphaArgs( 2 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
						ShowContinueError( "Did not recognize entry" );
						ErrorsFound = true;
					}
				}
				tmpSimpleModuleParams( ModNum ).PVEfficiency = rNumericArgs( 2 );

				tmpSimpleModuleParams( ModNum ).EffSchedPtr = GetScheduleIndex( cAlphaArgs( 3 ) );
				if ( ( tmpSimpleModuleParams( ModNum ).EffSchedPtr == 0 ) && ( tmpSimpleModuleParams( ModNum ).EfficencyInputMode == ScheduledEfficiency ) ) {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 3 ) + " = " + cAlphaArgs( 3 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
					ShowContinueError( "Did not find schedule" );
					ErrorsFound = true;
				}
			}
		}

		if ( Num1DiodePVModuleTypes > 0 ) {
			tmpTNRSYSModuleParams.allocate( Num1DiodePVModuleTypes );
			cCurrentModuleObject = cPVEquiv1DiodePerfObjectName;
			for ( ModNum = 1; ModNum <= Num1DiodePVModuleTypes; ++ModNum ) {
				GetObjectItem( cCurrentModuleObject, ModNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

				IsNotOK = false;
				IsBlank = false;
				VerifyName( cAlphaArgs( 1 ), tmpTNRSYSModuleParams, ModNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
				if ( IsNotOK ) { //repeat or blank name so don't add
					ErrorsFound = true;
					continue;
				}
				tmpTNRSYSModuleParams( ModNum ).Name = cAlphaArgs( 1 );
				if ( SameString( cAlphaArgs( 2 ), "CrystallineSilicon" ) ) {
					tmpTNRSYSModuleParams( ModNum ).CellType = CrystallineSiPVCells;
				} else if ( SameString( cAlphaArgs( 2 ), "AmorphousSilicon" ) ) {
					tmpTNRSYSModuleParams( ModNum ).CellType = AmorphousSiPVCells;
				} else {
					if ( lAlphaFieldBlanks( 2 ) ) {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 2 ) + " = " + cAlphaArgs( 2 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
						ShowContinueError( "Field cannot be blank" );
						ErrorsFound = true;
					} else {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 2 ) + " = " + cAlphaArgs( 2 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
						ShowContinueError( "Did not recognize entry" );
						ErrorsFound = true;
					}
				}

				tmpTNRSYSModuleParams( ModNum ).CellsInSeries = int( rNumericArgs( 1 ) );
				tmpTNRSYSModuleParams( ModNum ).Area = rNumericArgs( 2 );
				tmpTNRSYSModuleParams( ModNum ).TauAlpha = rNumericArgs( 3 );
				tmpTNRSYSModuleParams( ModNum ).SemiConductorBandgap = rNumericArgs( 4 );
				tmpTNRSYSModuleParams( ModNum ).ShuntResistance = rNumericArgs( 5 );
				tmpTNRSYSModuleParams( ModNum ).RefIsc = rNumericArgs( 6 );
				tmpTNRSYSModuleParams( ModNum ).RefVoc = rNumericArgs( 7 );
				tmpTNRSYSModuleParams( ModNum ).RefTemperature = rNumericArgs( 8 ) + KelvinConv;
				tmpTNRSYSModuleParams( ModNum ).RefInsolation = rNumericArgs( 9 );
				tmpTNRSYSModuleParams( ModNum ).Imp = rNumericArgs( 10 );
				tmpTNRSYSModuleParams( ModNum ).Vmp = rNumericArgs( 11 );
				tmpTNRSYSModuleParams( ModNum ).TempCoefIsc = rNumericArgs( 12 );
				tmpTNRSYSModuleParams( ModNum ).TempCoefVoc = rNumericArgs( 13 );
				tmpTNRSYSModuleParams( ModNum ).NOCTAmbTemp = rNumericArgs( 14 ) + KelvinConv;
				tmpTNRSYSModuleParams( ModNum ).NOCTCellTemp = rNumericArgs( 15 ) + KelvinConv;
				tmpTNRSYSModuleParams( ModNum ).NOCTInsolation = rNumericArgs( 16 );
				tmpTNRSYSModuleParams( ModNum ).HeatLossCoef = rNumericArgs( 17 );
				tmpTNRSYSModuleParams( ModNum ).HeatCapacity = rNumericArgs( 18 );

			}
		}

		if ( NumSNLPVModuleTypes > 0 ) {
			tmpSNLModuleParams.allocate( NumSNLPVModuleTypes );
			cCurrentModuleObject = cPVSandiaPerfObjectName;
			for ( ModNum = 1; ModNum <= NumSNLPVModuleTypes; ++ModNum ) {

				GetObjectItem( cCurrentModuleObject, ModNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

				IsNotOK = false;
				IsBlank = false;
				VerifyName( cAlphaArgs( 1 ), tmpSNLModuleParams, &SNLModuleParamsStuct::name, ModNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
				if ( IsNotOK ) { //repeat or blank name so don't add
					ErrorsFound = true;
					continue;
				}

				tmpSNLModuleParams( ModNum ).name = cAlphaArgs( 1 );
				tmpSNLModuleParams( ModNum ).Acoll = rNumericArgs( 1 );
				tmpSNLModuleParams( ModNum ).NcellSer = rNumericArgs( 2 );
				tmpSNLModuleParams( ModNum ).NparSerCells = rNumericArgs( 3 );
				tmpSNLModuleParams( ModNum ).Isc0 = rNumericArgs( 4 );
				tmpSNLModuleParams( ModNum ).Voc0 = rNumericArgs( 5 );
				tmpSNLModuleParams( ModNum ).Imp0 = rNumericArgs( 6 );
				tmpSNLModuleParams( ModNum ).Vmp0 = rNumericArgs( 7 );
				tmpSNLModuleParams( ModNum ).aIsc = rNumericArgs( 8 );
				tmpSNLModuleParams( ModNum ).aImp = rNumericArgs( 9 );
				tmpSNLModuleParams( ModNum ).c_0 = rNumericArgs( 10 );
				tmpSNLModuleParams( ModNum ).c_1 = rNumericArgs( 11 );
				tmpSNLModuleParams( ModNum ).BVoc0 = rNumericArgs( 12 );
				tmpSNLModuleParams( ModNum ).mBVoc = rNumericArgs( 13 );
				tmpSNLModuleParams( ModNum ).BVmp0 = rNumericArgs( 14 );
				tmpSNLModuleParams( ModNum ).mBVmp = rNumericArgs( 15 );
				tmpSNLModuleParams( ModNum ).DiodeFactor = rNumericArgs( 16 );
				tmpSNLModuleParams( ModNum ).c_2 = rNumericArgs( 17 );
				tmpSNLModuleParams( ModNum ).c_3 = rNumericArgs( 18 );
				tmpSNLModuleParams( ModNum ).a_0 = rNumericArgs( 19 );
				tmpSNLModuleParams( ModNum ).a_1 = rNumericArgs( 20 );
				tmpSNLModuleParams( ModNum ).a_2 = rNumericArgs( 21 );
				tmpSNLModuleParams( ModNum ).a_3 = rNumericArgs( 22 );
				tmpSNLModuleParams( ModNum ).a_4 = rNumericArgs( 23 );
				tmpSNLModuleParams( ModNum ).b_0 = rNumericArgs( 24 );
				tmpSNLModuleParams( ModNum ).b_1 = rNumericArgs( 25 );
				tmpSNLModuleParams( ModNum ).b_2 = rNumericArgs( 26 );
				tmpSNLModuleParams( ModNum ).b_3 = rNumericArgs( 27 );
				tmpSNLModuleParams( ModNum ).b_4 = rNumericArgs( 28 );
				tmpSNLModuleParams( ModNum ).b_5 = rNumericArgs( 29 );
				tmpSNLModuleParams( ModNum ).DT0 = rNumericArgs( 30 );
				tmpSNLModuleParams( ModNum ).fd = rNumericArgs( 31 );
				tmpSNLModuleParams( ModNum ).a = rNumericArgs( 32 );
				tmpSNLModuleParams( ModNum ).b = rNumericArgs( 33 );
				tmpSNLModuleParams( ModNum ).c_4 = rNumericArgs( 34 );
				tmpSNLModuleParams( ModNum ).c_5 = rNumericArgs( 35 );
				tmpSNLModuleParams( ModNum ).Ix0 = rNumericArgs( 36 );
				tmpSNLModuleParams( ModNum ).Ixx0 = rNumericArgs( 37 );
				tmpSNLModuleParams( ModNum ).c_6 = rNumericArgs( 38 );
				tmpSNLModuleParams( ModNum ).c_7 = rNumericArgs( 39 );

			}
		}

		// now fill collector performance data into main PV structure
		for ( PVnum = 1; PVnum <= NumPVs; ++PVnum ) {

			{ auto const SELECT_CASE_var( PVarray( PVnum ).PVModelType );

			if ( SELECT_CASE_var == iSimplePVModel ) {

				ThisParamObj = FindItemInList( PVarray( PVnum ).PerfObjName, tmpSimpleModuleParams );
				if ( ThisParamObj > 0 ) {
					PVarray( PVnum ).SimplePVModule = tmpSimpleModuleParams( ThisParamObj ); //entire structure assignment

					// do one-time setups on input data
					PVarray( PVnum ).SimplePVModule.AreaCol = Surface( PVarray( PVnum ).SurfacePtr ).Area * PVarray( PVnum ).SimplePVModule.ActiveFraction;
				} else {
					ShowSevereError( "Invalid PV performance object name of " + PVarray( PVnum ).PerfObjName );
					ShowContinueError( "Entered in " + cPVGeneratorObjectName + " = " + PVarray( PVnum ).Name );
					ErrorsFound = true;
				}

			} else if ( SELECT_CASE_var == iTRNSYSPVModel ) {

				ThisParamObj = FindItemInList( PVarray( PVnum ).PerfObjName, tmpTNRSYSModuleParams );
				if ( ThisParamObj > 0 ) {
					PVarray( PVnum ).TRNSYSPVModule = tmpTNRSYSModuleParams( ThisParamObj ); //entire structure assignment
				} else {
					ShowSevereError( "Invalid PV performance object name of " + PVarray( PVnum ).PerfObjName );
					ShowContinueError( "Entered in " + cPVGeneratorObjectName + " = " + PVarray( PVnum ).Name );
					ErrorsFound = true;
				}

			} else if ( SELECT_CASE_var == iSandiaPVModel ) {

				ThisParamObj = FindItemInList( PVarray( PVnum ).PerfObjName, tmpSNLModuleParams, &SNLModuleParamsStuct::name );
				if ( ThisParamObj > 0 ) {
					PVarray( PVnum ).SNLPVModule = tmpSNLModuleParams( ThisParamObj ); //entire structure assignment
				} else {
					ShowSevereError( "Invalid PV performance object name of " + PVarray( PVnum ).PerfObjName );
					ShowContinueError( "Entered in " + cPVGeneratorObjectName + " = " + PVarray( PVnum ).Name );
					ErrorsFound = true;
				}
			}}

			//set up report variables CurrentModuleObject='Photovoltaics'
			SetupOutputVariable( "Generator Produced DC Electric Power [W]", PVarray( PVnum ).Report.DCPower, "System", "Average", PVarray( PVnum ).Name );
			SetupOutputVariable( "Generator Produced DC Electric Energy [J]", PVarray( PVnum ).Report.DCEnergy, "System", "Sum", PVarray( PVnum ).Name, _, "ElectricityProduced", "Photovoltaics", _, "Plant" );
			SetupOutputVariable( "Generator PV Array Efficiency []", PVarray( PVnum ).Report.ArrayEfficiency, "System", "Average", PVarray( PVnum ).Name );

			// CurrentModuleObject='Equiv1Diode or Sandia Photovoltaics'
			if ( ( PVarray( PVnum ).PVModelType == iTRNSYSPVModel ) || ( PVarray( PVnum ).PVModelType == iSandiaPVModel ) ) {
				SetupOutputVariable( "Generator PV Cell Temperature [C]", PVarray( PVnum ).Report.CellTemp, "System", "Average", PVarray( PVnum ).Name );
				SetupOutputVariable( "Generator PV Short Circuit Current [A]", PVarray( PVnum ).Report.ArrayIsc, "System", "Average", PVarray( PVnum ).Name );
				SetupOutputVariable( "Generator PV Open Circuit Voltage [V]", PVarray( PVnum ).Report.ArrayVoc, "System", "Average", PVarray( PVnum ).Name );
			}

			// do some checks and setup
			if ( PVarray( PVnum ).PVModelType == iSurfaceOutsideFaceCellIntegration ) {
				//check that surface is HeatTransfer and a Construction with Internal Source was used
				if ( ! Surface( PVarray( PVnum ).SurfacePtr ).HeatTransSurf ) {
					ShowSevereError( "Must use a surface with heat transfer for IntegratedSurfaceOutsideFace mode in " + PVarray( PVnum ).Name );
					ErrorsFound = true;
				} else if ( ! Construct( Surface( PVarray( PVnum ).SurfacePtr ).Construction ).SourceSinkPresent ) {
					ShowSevereError( "Must use a surface with internal source construction for IntegratedSurfaceOutsideFace mode in " + PVarray( PVnum ).Name );
					ErrorsFound = true;
				}
			}

			if ( PVarray( PVnum ).CellIntegrationMode == iTranspiredCollectorCellIntegration ) {
				GetTranspiredCollectorIndex( PVarray( PVnum ).SurfacePtr, PVarray( PVnum ).UTSCPtr );
			}

			if ( PVarray( PVnum ).CellIntegrationMode == iExteriorVentedCavityCellIntegration ) {
				GetExtVentedCavityIndex( PVarray( PVnum ).SurfacePtr, PVarray( PVnum ).ExtVentCavPtr );
			}

			if ( PVarray( PVnum ).CellIntegrationMode == iPVTSolarCollectorCellIntegration ) {
				// Call GetPVTmodelIndex( PVarray(PVNum)%SurfacePtr , PVarray(PVNum)%PVTPtr )
			}

		}

		if ( ErrorsFound ) {
			ShowFatalError( "Errors found in getting photovoltaic input" );
		}

	}

	// **************************************

	void
	CalcSimplePV(
		int const thisPV,
		bool const EP_UNUSED( RunFlag ) // unused1208
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   Jan. 2004
		//       MODIFIED       B. Griffith, Aug. 2008
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// calculate the electricity production using a simple PV model

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Using/Aliasing
		using DataHeatBalance::QRadSWOutIncident;
		using ScheduleManager::GetCurrentScheduleValue;
		using DataHVACGlobals::TimeStepSys;
		using DataGlobals::SecInHour;
		using DataSurfaces::Surface;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ThisSurf; // working index ptr to Surface arrays
		Real64 Eff; // working variable for solar electric efficiency
		//unused1208    REAL(r64) :: ArrayEnergy !working variable for PV energy this system time step
		//first get surface index to use as a pointer

		ThisSurf = PVarray( thisPV ).SurfacePtr;

		if ( QRadSWOutIncident( ThisSurf ) > MinIrradiance ) {

			//get efficiency
			{ auto const SELECT_CASE_var( PVarray( thisPV ).SimplePVModule.EfficencyInputMode );

			if ( SELECT_CASE_var == FixedEfficiency ) {

				Eff = PVarray( thisPV ).SimplePVModule.PVEfficiency;

			} else if ( SELECT_CASE_var == ScheduledEfficiency ) { // get from schedule

				Eff = GetCurrentScheduleValue( PVarray( thisPV ).SimplePVModule.EffSchedPtr );
				PVarray( thisPV ).SimplePVModule.PVEfficiency = Eff;

			} else {
				Eff = 0.0; // Suppress uninitialized warning
				ShowSevereError( "caught bad Mode in Generator:Photovoltaic:Simple use FIXED or SCHEDULED efficiency mode" );
			}}

			PVarray( thisPV ).Report.DCPower = PVarray( thisPV ).SimplePVModule.AreaCol * Eff * QRadSWOutIncident( ThisSurf ); // active solar cellsurface net area | solar conversion efficiency | solar incident

			// store sink term in appropriate place for surface heat transfer itegration
			PVarray( thisPV ).SurfaceSink = PVarray( thisPV ).Report.DCPower;

			// array energy, power * timestep
			PVarray( thisPV ).Report.DCEnergy = PVarray( thisPV ).Report.DCPower * ( TimeStepSys * SecInHour );
			PVarray( thisPV ).Report.ArrayEfficiency = Eff;
		} else { //not enough incident solar, zero things out

			PVarray( thisPV ).SurfaceSink = 0.0;
			PVarray( thisPV ).Report.DCEnergy = 0.0;
			PVarray( thisPV ).Report.DCPower = 0.0;
			PVarray( thisPV ).Report.ArrayEfficiency = 0.0;

		}

	}

	void
	ReportPV( int const PVnum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   Jan. 2004
		//       MODIFIED       B. Griffith, Aug. 2008
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// collect statements that assign to variables tied to output variables

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na
		// Using/Aliasing
		using DataHeatBalance::Zone;
		using DataSurfaces::Surface;
		using DataHeatBalFanSys::QPVSysSource;
		using TranspiredCollector::SetUTSCQdotSource;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int thisZone; // working index for zones

		PVarray( PVnum ).Report.DCEnergy = PVarray( PVnum ).Report.DCPower * ( TimeStepSys * SecInHour );

		// add check for multiplier.  if surface is attached to a zone that is on a multiplier
		// then PV production should be multiplied out as well

		if ( Surface( PVarray( PVnum ).SurfacePtr ).Zone != 0 ) { // might need to apply multiplier
			thisZone = Surface( PVarray( PVnum ).SurfacePtr ).Zone;
			PVarray( PVnum ).Report.DCEnergy *= ( Zone( thisZone ).Multiplier * Zone( thisZone ).ListMultiplier );
			PVarray( PVnum ).Report.DCPower *= ( Zone( thisZone ).Multiplier * Zone( thisZone ).ListMultiplier );
		}

		{ auto const SELECT_CASE_var( PVarray( PVnum ).CellIntegrationMode );
		// SurfaceSink is not multiplied...
		if ( SELECT_CASE_var == iSurfaceOutsideFaceCellIntegration ) {
			QPVSysSource( PVarray( PVnum ).SurfacePtr ) = -1.0 * PVarray( PVnum ).SurfaceSink;

		} else if ( SELECT_CASE_var == iTranspiredCollectorCellIntegration ) {
			SetUTSCQdotSource( PVarray( PVnum ).UTSCPtr, - 1.0 * PVarray( PVnum ).SurfaceSink );

		} else if ( SELECT_CASE_var == iExteriorVentedCavityCellIntegration ) {
			SetVentedModuleQdotSource( PVarray( PVnum ).ExtVentCavPtr, - 1.0 * PVarray( PVnum ).SurfaceSink );

		} else if ( SELECT_CASE_var == iPVTSolarCollectorCellIntegration ) {
			// CALL SetPVTQdotSource(PVarray(PVNum)%ExtVentCavPtr,  -1 * PVarray(PVNum)%SurfaceSink )

		}}

	}

	// *************

	void
	CalcSandiaPV(
		int const PVnum, // ptr to current PV system
		bool const RunFlag // controls if generator is scheduled *ON*
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith , (derived from Greg Barker's TRNSYS type101 for SANDIA PV model)
		//       DATE WRITTEN   Jan 2004
		//       MODIFIED       B. Griffith, Aug. 2008 reworked for new, single-PV-generator data structure
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate various PV system peformance indicies at the current timestep

		// METHODOLOGY EMPLOYED:
		//  adapted code from a set of F77 routines by G. Barker that implement the model
		//  This routines works on a single photovoltaic object of the type 'GENERATOR:PV:SANDIA'
		//  Each major model equation has its own function (in this module)

		// REFERENCES:
		// King, David L. . Photovoltaic module and array performance characterization methods for all
		//   system operating conditions. Pro. NREL/SNL Photovoltaics Program Review, AIP Press, Lakewood CO
		//   Sandia National Laboratories

		// Davis, M.W., A.H. Fanney, and B.P. Dougherty. Measured versus predicted performance of Building
		//    integrated photovoltaics. Solar 2002, Sunrise on the Reliable Energy Economy, June 15-19, 2002 Reno, NV

		// Using/Aliasing
		using DataGlobals::DegToRadians;
		using DataEnvironment::Elevation;
		using DataEnvironment::SOLCOS;
		using DataHeatBalance::CosIncidenceAngle;
		using DataHeatBalance::QRadSWOutIncidentBeam;
		using DataHeatBalance::QRadSWOutIncident;
		using DataHeatBalSurface::TempSurfOut;
		using DataSurfaces::Surface;
		using TranspiredCollector::GetUTSCTsColl;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ThisSurf; // working variable for indexing surfaces
		//unused1208    INTEGER :: thisMod  ! working variable for indexing module parameters
		Real64 Ee;

		ThisSurf = PVarray( PVnum ).SurfacePtr;

		//   get input from elsewhere in Energyplus for the current point in the simulation
		PVarray( PVnum ).SNLPVinto.IcBeam = QRadSWOutIncidentBeam( ThisSurf ); //(W/m2)from DataHeatBalance
		PVarray( PVnum ).SNLPVinto.IcDiffuse = QRadSWOutIncident( ThisSurf ) - QRadSWOutIncidentBeam( ThisSurf ); //(W/ m2)(was kJ/hr m2)
		PVarray( PVnum ).SNLPVinto.IncidenceAngle = std::acos( CosIncidenceAngle( ThisSurf ) ) / DegToRadians; // (deg) from dataHeatBalance
		PVarray( PVnum ).SNLPVinto.ZenithAngle = std::acos( SOLCOS( 3 ) ) / DegToRadians; //(degrees),
		PVarray( PVnum ).SNLPVinto.Tamb = Surface( ThisSurf ).OutDryBulbTemp; //(deg. C)
		PVarray( PVnum ).SNLPVinto.WindSpeed = Surface( ThisSurf ).WindSpeed; // (m/s)
		PVarray( PVnum ).SNLPVinto.Altitude = Elevation; // from DataEnvironment via USE

		if ( ( ( PVarray( PVnum ).SNLPVinto.IcBeam + PVarray( PVnum ).SNLPVinto.IcDiffuse ) > MinIrradiance ) && ( RunFlag ) ) {

			// first determine PV cell temperatures depending on model
			{ auto const SELECT_CASE_var( PVarray( PVnum ).CellIntegrationMode );

			if ( SELECT_CASE_var == iDecoupledCellIntegration ) { // Sandia module temperature model for rack mounted PVs
				// Calculate back-of-module temperature:
				PVarray( PVnum ).SNLPVCalc.Tback = SandiaModuleTemperature( PVarray( PVnum ).SNLPVinto.IcBeam, PVarray( PVnum ).SNLPVinto.IcDiffuse, PVarray( PVnum ).SNLPVinto.WindSpeed, PVarray( PVnum ).SNLPVinto.Tamb, PVarray( PVnum ).SNLPVModule.fd, PVarray( PVnum ).SNLPVModule.a, PVarray( PVnum ).SNLPVModule.b );

				// Calculate cell temperature:
				PVarray( PVnum ).SNLPVCalc.Tcell = SandiaTcellFromTmodule( PVarray( PVnum ).SNLPVCalc.Tback, PVarray( PVnum ).SNLPVinto.IcBeam, PVarray( PVnum ).SNLPVinto.IcDiffuse, PVarray( PVnum ).SNLPVModule.fd, PVarray( PVnum ).SNLPVModule.DT0 );

			} else if ( SELECT_CASE_var == iSurfaceOutsideFaceCellIntegration ) {
				// get back-of-module temperature from elsewhere in EnergyPlus
				PVarray( PVnum ).SNLPVCalc.Tback = TempSurfOut( PVarray( PVnum ).SurfacePtr );

				PVarray( PVnum ).SNLPVCalc.Tcell = SandiaTcellFromTmodule( PVarray( PVnum ).SNLPVCalc.Tback, PVarray( PVnum ).SNLPVinto.IcBeam, PVarray( PVnum ).SNLPVinto.IcDiffuse, PVarray( PVnum ).SNLPVModule.fd, PVarray( PVnum ).SNLPVModule.DT0 );

			} else if ( SELECT_CASE_var == iTranspiredCollectorCellIntegration ) {
				GetUTSCTsColl( PVarray( PVnum ).UTSCPtr, PVarray( PVnum ).SNLPVCalc.Tback );

				PVarray( PVnum ).SNLPVCalc.Tcell = SandiaTcellFromTmodule( PVarray( PVnum ).SNLPVCalc.Tback, PVarray( PVnum ).SNLPVinto.IcBeam, PVarray( PVnum ).SNLPVinto.IcDiffuse, PVarray( PVnum ).SNLPVModule.fd, PVarray( PVnum ).SNLPVModule.DT0 );

			} else if ( SELECT_CASE_var == iExteriorVentedCavityCellIntegration ) {
				GetExtVentedCavityTsColl( PVarray( PVnum ).ExtVentCavPtr, PVarray( PVnum ).SNLPVCalc.Tback );

				PVarray( PVnum ).SNLPVCalc.Tcell = SandiaTcellFromTmodule( PVarray( PVnum ).SNLPVCalc.Tback, PVarray( PVnum ).SNLPVinto.IcBeam, PVarray( PVnum ).SNLPVinto.IcDiffuse, PVarray( PVnum ).SNLPVModule.fd, PVarray( PVnum ).SNLPVModule.DT0 );

			} else if ( SELECT_CASE_var == iPVTSolarCollectorCellIntegration ) {
				// add calls to PVT models here

			} else {
				ShowSevereError( "Sandia PV Simulation Temperature Modeling Mode Error in " + PVarray( PVnum ).Name );

			}}

			// Calculate Air Mass function
			PVarray( PVnum ).SNLPVCalc.AMa = AbsoluteAirMass( PVarray( PVnum ).SNLPVinto.ZenithAngle, PVarray( PVnum ).SNLPVinto.Altitude );

			// Calculate F1 polynomial function:
			PVarray( PVnum ).SNLPVCalc.F1 = SandiaF1( PVarray( PVnum ).SNLPVCalc.AMa, PVarray( PVnum ).SNLPVModule.a_0, PVarray( PVnum ).SNLPVModule.a_1, PVarray( PVnum ).SNLPVModule.a_2, PVarray( PVnum ).SNLPVModule.a_3, PVarray( PVnum ).SNLPVModule.a_4 );

			// Calculate F2 polynomial function:
			PVarray( PVnum ).SNLPVCalc.F2 = SandiaF2( PVarray( PVnum ).SNLPVinto.IncidenceAngle, PVarray( PVnum ).SNLPVModule.b_0, PVarray( PVnum ).SNLPVModule.b_1, PVarray( PVnum ).SNLPVModule.b_2, PVarray( PVnum ).SNLPVModule.b_3, PVarray( PVnum ).SNLPVModule.b_4, PVarray( PVnum ).SNLPVModule.b_5 );

			// Calculate short-circuit current function:
			PVarray( PVnum ).SNLPVCalc.Isc = SandiaIsc( PVarray( PVnum ).SNLPVCalc.Tcell, PVarray( PVnum ).SNLPVModule.Isc0, PVarray( PVnum ).SNLPVinto.IcBeam, PVarray( PVnum ).SNLPVinto.IcDiffuse, PVarray( PVnum ).SNLPVCalc.F1, PVarray( PVnum ).SNLPVCalc.F2, PVarray( PVnum ).SNLPVModule.fd, PVarray( PVnum ).SNLPVModule.aIsc );

			// Calculate effective irradiance function:
			Ee = SandiaEffectiveIrradiance( PVarray( PVnum ).SNLPVCalc.Tcell, PVarray( PVnum ).SNLPVCalc.Isc, PVarray( PVnum ).SNLPVModule.Isc0, PVarray( PVnum ).SNLPVModule.aIsc );
			// Calculate Imp function:
			PVarray( PVnum ).SNLPVCalc.Imp = SandiaImp( PVarray( PVnum ).SNLPVCalc.Tcell, Ee, PVarray( PVnum ).SNLPVModule.Imp0, PVarray( PVnum ).SNLPVModule.aImp, PVarray( PVnum ).SNLPVModule.c_0, PVarray( PVnum ).SNLPVModule.c_1 );

			// Calculate Voc function:
			PVarray( PVnum ).SNLPVCalc.Voc = SandiaVoc( PVarray( PVnum ).SNLPVCalc.Tcell, Ee, PVarray( PVnum ).SNLPVModule.Voc0, PVarray( PVnum ).SNLPVModule.NcellSer, PVarray( PVnum ).SNLPVModule.DiodeFactor, PVarray( PVnum ).SNLPVModule.BVoc0, PVarray( PVnum ).SNLPVModule.mBVoc );

			// Calculate Vmp: voltagea at maximum powerpoint
			PVarray( PVnum ).SNLPVCalc.Vmp = SandiaVmp( PVarray( PVnum ).SNLPVCalc.Tcell, Ee, PVarray( PVnum ).SNLPVModule.Vmp0, PVarray( PVnum ).SNLPVModule.NcellSer, PVarray( PVnum ).SNLPVModule.DiodeFactor, PVarray( PVnum ).SNLPVModule.BVmp0, PVarray( PVnum ).SNLPVModule.mBVmp, PVarray( PVnum ).SNLPVModule.c_2, PVarray( PVnum ).SNLPVModule.c_3 );

			// Calculate Ix function:
			PVarray( PVnum ).SNLPVCalc.Ix = SandiaIx( PVarray( PVnum ).SNLPVCalc.Tcell, Ee, PVarray( PVnum ).SNLPVModule.Ix0, PVarray( PVnum ).SNLPVModule.aIsc, PVarray( PVnum ).SNLPVModule.aImp, PVarray( PVnum ).SNLPVModule.c_4, PVarray( PVnum ).SNLPVModule.c_5 );

			// Calculate Vx function:
			PVarray( PVnum ).SNLPVCalc.Vx = PVarray( PVnum ).SNLPVCalc.Voc / 2.0;

			// Calculate Ixx function:
			PVarray( PVnum ).SNLPVCalc.Ixx = SandiaIxx( PVarray( PVnum ).SNLPVCalc.Tcell, Ee, PVarray( PVnum ).SNLPVModule.Ixx0, PVarray( PVnum ).SNLPVModule.aImp, PVarray( PVnum ).SNLPVModule.c_6, PVarray( PVnum ).SNLPVModule.c_7 );
			// Calculate Vxx :
			PVarray( PVnum ).SNLPVCalc.Vxx = 0.5 * ( PVarray( PVnum ).SNLPVCalc.Voc + PVarray( PVnum ).SNLPVCalc.Vmp );

			// Calculate Pmp, single module: power at maximum powerpoint
			PVarray( PVnum ).SNLPVCalc.Pmp = PVarray( PVnum ).SNLPVCalc.Imp * PVarray( PVnum ).SNLPVCalc.Vmp; // W

			// Calculate PV efficiency at maximum power point
			PVarray( PVnum ).SNLPVCalc.EffMax = PVarray( PVnum ).SNLPVCalc.Pmp / ( PVarray( PVnum ).SNLPVinto.IcBeam + PVarray( PVnum ).SNLPVinto.IcDiffuse ) / PVarray( PVnum ).SNLPVModule.Acoll;

			// Scale to NumStrings and NumSeries:
			PVarray( PVnum ).SNLPVCalc.Pmp *= PVarray( PVnum ).NumSeriesNParall * PVarray( PVnum ).NumModNSeries;
			PVarray( PVnum ).SNLPVCalc.Imp *= PVarray( PVnum ).NumModNSeries;
			PVarray( PVnum ).SNLPVCalc.Vmp *= PVarray( PVnum ).NumModNSeries;
			PVarray( PVnum ).SNLPVCalc.Isc *= PVarray( PVnum ).NumSeriesNParall;
			PVarray( PVnum ).SNLPVCalc.Voc *= PVarray( PVnum ).NumModNSeries;
			PVarray( PVnum ).SNLPVCalc.Ix *= PVarray( PVnum ).NumSeriesNParall;
			PVarray( PVnum ).SNLPVCalc.Ixx *= PVarray( PVnum ).NumSeriesNParall;
			PVarray( PVnum ).SNLPVCalc.Vx *= PVarray( PVnum ).NumModNSeries;
			PVarray( PVnum ).SNLPVCalc.Vxx *= PVarray( PVnum ).NumModNSeries;
			PVarray( PVnum ).SNLPVCalc.SurfaceSink = PVarray( PVnum ).SNLPVCalc.Pmp;
		} else { // Ibeam+Idiff < MaxIrradiance or not RunFlag
			// so zero things.
			PVarray( PVnum ).SNLPVCalc.Vmp = 0.0;
			PVarray( PVnum ).SNLPVCalc.Imp = 0.0;
			PVarray( PVnum ).SNLPVCalc.Pmp = 0.0;
			PVarray( PVnum ).SNLPVCalc.EffMax = 0.0;
			PVarray( PVnum ).SNLPVCalc.Isc = 0.0;
			PVarray( PVnum ).SNLPVCalc.Voc = 0.0;
			PVarray( PVnum ).SNLPVCalc.Tcell = PVarray( PVnum ).SNLPVinto.Tamb;
			PVarray( PVnum ).SNLPVCalc.Tback = PVarray( PVnum ).SNLPVinto.Tamb;
			PVarray( PVnum ).SNLPVCalc.AMa = 999.0;
			PVarray( PVnum ).SNLPVCalc.F1 = 0.0;
			PVarray( PVnum ).SNLPVCalc.F2 = 0.0;
			PVarray( PVnum ).SNLPVCalc.Ix = 0.0;
			PVarray( PVnum ).SNLPVCalc.Vx = 0.0;
			PVarray( PVnum ).SNLPVCalc.Ixx = 0.0;
			PVarray( PVnum ).SNLPVCalc.Vxx = 0.0;
			PVarray( PVnum ).SNLPVCalc.SurfaceSink = 0.0;
		} //Ibeam+Idiff > MinIrradiance and runflag

		// update calculations to report variables
		PVarray( PVnum ).Report.DCPower = PVarray( PVnum ).SNLPVCalc.Pmp;
		PVarray( PVnum ).Report.ArrayIsc = PVarray( PVnum ).SNLPVCalc.Isc;
		PVarray( PVnum ).Report.ArrayVoc = PVarray( PVnum ).SNLPVCalc.Voc;
		PVarray( PVnum ).Report.CellTemp = PVarray( PVnum ).SNLPVCalc.Tcell;
		PVarray( PVnum ).Report.ArrayEfficiency = PVarray( PVnum ).SNLPVCalc.EffMax;
		PVarray( PVnum ).SurfaceSink = PVarray( PVnum ).SNLPVCalc.SurfaceSink;

	}

	// ********************
	// begin routines for Equivalent one-diode model by Bradley/Ulleberg

	void
	InitTRNSYSPV( int const PVnum ) // the number of the GENERATOR:PHOTOVOLTAICS (passed in)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         David Bradley
		//       DATE WRITTEN   April 2003
		//       MODIFIED       BG March 2007 reworked for CR7109 (reverse DD testing)
		//                      B. Griffith, Aug. 2008 reworked for new, single-PV-generator data structure
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine initializes the PV arrays during simulation. It performs both start of
		// simulation initializations and start of timestep initializations. The structure of the
		// subroutine was taken from InitBaseboard.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		//  USE DataPhotovoltaics, ONLY:CellTemp,LastCellTemp
		// Using/Aliasing
		using DataHeatBalance::QRadSWOutIncident;
		using DataSurfaces::Surface;
		using DataHVACGlobals::SysTimeElapsed;
		using DataHVACGlobals::TimeStepSys;
		using DataGlobals::TimeStep;
		using DataGlobals::TimeStepZone;
		using DataGlobals::SecInHour;
		using DataGlobals::BeginEnvrnFlag;
		using DataGlobals::HourOfDay;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool MyOneTimeFlag( true );
		static Array1D_bool MyEnvrnFlag;
		Real64 TimeElapsed; // Fraction of the current hour that has elapsed (h)

		// perform the one time initializations
		if ( MyOneTimeFlag ) {
			// initialize the environment and sizing flags
			MyEnvrnFlag.dimension( NumPVs, true );
			MyOneTimeFlag = false;
		}

		// Do the Begin Environment initializations
		if ( BeginEnvrnFlag && MyEnvrnFlag( PVnum ) ) {

			PVarray( PVnum ).TRNSYSPVcalc.CellTempK = Surface( PVarray( PVnum ).SurfacePtr ).OutDryBulbTemp + KelvinConv;
			PVarray( PVnum ).TRNSYSPVcalc.LastCellTempK = Surface( PVarray( PVnum ).SurfacePtr ).OutDryBulbTemp + KelvinConv;
			MyEnvrnFlag( PVnum ) = false;
		}

		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( PVnum ) = true;
		}

		// Do the beginning of every time step initializations
		TimeElapsed = HourOfDay + TimeStep * TimeStepZone + SysTimeElapsed;
		if ( PVarray( PVnum ).TRNSYSPVcalc.TimeElapsed != TimeElapsed ) {
			// The simulation has advanced to the next system timestep.  Save conditions from the end of the previous system
			PVarray( PVnum ).TRNSYSPVcalc.LastCellTempK = PVarray( PVnum ).TRNSYSPVcalc.CellTempK;
			PVarray( PVnum ).TRNSYSPVcalc.TimeElapsed = TimeElapsed;
		}

		if ( any_gt( QRadSWOutIncident, 0.0 ) ) {
			//  Determine the amount of radiation incident on each PV
			PVarray( PVnum ).TRNSYSPVcalc.Insolation = QRadSWOutIncident( PVarray( PVnum ).SurfacePtr ); //[W/m2]
		} else {
			PVarray( PVnum ).TRNSYSPVcalc.Insolation = 0.0;
		}

	}

	// *************

	void
	CalcTRNSYSPV(
		int const PVnum, // BTG added intent
		bool const RunFlag // BTG added intent    !flag tells whether the PV is ON or OFF
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         D. Bradley
		//       DATE WRITTEN   April 2003
		//       MODIFIED       B. Griffith, February 2008-- added support for inverter
		//                      multipliers, and building integrated heat transfer
		//                      B. Griffith, Aug. 2008 reworked for new, single-PV-generator data structure
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine simulates the PV performance.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::SecInHour;
		using DataGlobals::MinutesPerTimeStep;
		using DataSurfaces::Surface;
		//  USE DataPhotovoltaics, ONLY:CellTemp,LastCellTemp
		using DataHeatBalSurface::TempSurfOut;
		using TranspiredCollector::GetUTSCTsColl;
		using DataHeatBalance::Zone;

		// Locals
		// SUBROUTINE FUNCTION DECLARATIONS:

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const EPS( 0.001 );
		Real64 const ERR( 0.001 );
		Real64 const MinInsolation( 30.0 );
		int const KMAX( 100 );
		Real64 const EtaIni( 0.10 ); // initial value of eta

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static Real64 PVTimeStep; // internal timestep (in seconds) for cell temperature mode 3
		Real64 DummyErr;
		Real64 ETA;
		Real64 Tambient;
		Real64 EtaOld;
		Real64 ILRef;
		Real64 AARef;
		Real64 IORef;
		Real64 SeriesResistance;
		Real64 IL;
		Real64 AA;
		Real64 IO;
		Real64 ISCG1;
		Real64 ISC;
		Real64 VOCG1;
		Real64 VOC;
		Real64 VLEFT;
		Real64 VRIGHT;
		Real64 VM;
		Real64 IM;
		Real64 PM;
		Real64 IA;
		Real64 ISCA;
		Real64 VA;
		Real64 VOCA;
		Real64 PA;
		int CC;
		int K;
		Real64 CellTemp( 0.0 ); // cell temperature in Kelvin
		Real64 CellTempC; // cell temperature in degrees C
		static bool firstTime( true );
		//unused1208  INTEGER :: thisZone

		// if the cell temperature mode is 2, convert the timestep to seconds
		if ( firstTime && PVarray( PVnum ).CellIntegrationMode == iDecoupledUllebergDynamicCellIntegration ) {
			PVTimeStep = double( MinutesPerTimeStep ) * 60.0; //Seconds per time step
		}
		firstTime = false;

		// place the shunt resistance into its common block
		ShuntResistance = PVarray( PVnum ).TRNSYSPVModule.ShuntResistance;

		// convert ambient temperature from C to K
		Tambient = Surface( PVarray( PVnum ).SurfacePtr ).OutDryBulbTemp + KelvinConv;

		if ( ( PVarray( PVnum ).TRNSYSPVcalc.Insolation > MinInsolation ) && ( RunFlag ) ) {

			// set initial values for eta iteration loop
			DummyErr = 2.0 * ERR;
			CC = 1;
			EtaOld = EtaIni;

			// Begin DO WHILE loop - until the error tolerance is reached.
			ETA = 0.0;
			while ( DummyErr > ERR ) {

				{ auto const SELECT_CASE_var( PVarray( PVnum ).CellIntegrationMode );
				if ( SELECT_CASE_var == iDecoupledCellIntegration ) {
					//  cell temperature based on energy balance
					PVarray( PVnum ).TRNSYSPVModule.HeatLossCoef = PVarray( PVnum ).TRNSYSPVModule.TauAlpha * PVarray( PVnum ).TRNSYSPVModule.NOCTInsolation / ( PVarray( PVnum ).TRNSYSPVModule.NOCTCellTemp - PVarray( PVnum ).TRNSYSPVModule.NOCTAmbTemp );
					CellTemp = Tambient + ( PVarray( PVnum ).TRNSYSPVcalc.Insolation * PVarray( PVnum ).TRNSYSPVModule.TauAlpha / PVarray( PVnum ).TRNSYSPVModule.HeatLossCoef ) * ( 1.0 - ETA / PVarray( PVnum ).TRNSYSPVModule.TauAlpha );
				} else if ( SELECT_CASE_var == iDecoupledUllebergDynamicCellIntegration ) {
					//  cell temperature based on energy balance with thermal capacity effects
					CellTemp = Tambient + ( PVarray( PVnum ).TRNSYSPVcalc.LastCellTempK - Tambient ) * std::exp( -PVarray( PVnum ).TRNSYSPVModule.HeatLossCoef / PVarray( PVnum ).TRNSYSPVModule.HeatCapacity * PVTimeStep ) + ( PVarray( PVnum ).TRNSYSPVModule.TauAlpha - ETA ) * PVarray( PVnum ).TRNSYSPVcalc.Insolation / PVarray( PVnum ).TRNSYSPVModule.HeatLossCoef * ( 1.0 - std::exp( -PVarray( PVnum ).TRNSYSPVModule.HeatLossCoef / PVarray( PVnum ).TRNSYSPVModule.HeatCapacity * PVTimeStep ) );
				} else if ( SELECT_CASE_var == iSurfaceOutsideFaceCellIntegration ) {
					CellTemp = TempSurfOut( PVarray( PVnum ).SurfacePtr ) + KelvinConv;
				} else if ( SELECT_CASE_var == iTranspiredCollectorCellIntegration ) {
					GetUTSCTsColl( PVarray( PVnum ).UTSCPtr, CellTemp );
					CellTemp += KelvinConv;
				} else if ( SELECT_CASE_var == iExteriorVentedCavityCellIntegration ) {
					GetExtVentedCavityTsColl( PVarray( PVnum ).ExtVentCavPtr, CellTemp );
					CellTemp += KelvinConv;
				} else if ( SELECT_CASE_var == iPVTSolarCollectorCellIntegration ) {
					// get PVT model result for cell temp..
				}}

				//  reference parameters
				ILRef = PVarray( PVnum ).TRNSYSPVModule.RefIsc;
				AARef = ( PVarray( PVnum ).TRNSYSPVModule.TempCoefVoc * PVarray( PVnum ).TRNSYSPVModule.RefTemperature - PVarray( PVnum ).TRNSYSPVModule.RefVoc + PVarray( PVnum ).TRNSYSPVModule.SemiConductorBandgap * PVarray( PVnum ).TRNSYSPVModule.CellsInSeries ) / ( PVarray( PVnum ).TRNSYSPVModule.TempCoefIsc * PVarray( PVnum ).TRNSYSPVModule.RefTemperature / ILRef - 3.0 );
				IORef = ILRef * std::exp( -PVarray( PVnum ).TRNSYSPVModule.RefVoc / AARef );

				//  series resistance
				SeriesResistance = ( AARef * std::log( 1.0 - PVarray( PVnum ).TRNSYSPVModule.Imp / ILRef ) - PVarray( PVnum ).TRNSYSPVModule.Vmp + PVarray( PVnum ).TRNSYSPVModule.RefVoc ) / PVarray( PVnum ).TRNSYSPVModule.Imp;

				//  temperature depencence
				IL = PVarray( PVnum ).TRNSYSPVcalc.Insolation / PVarray( PVnum ).TRNSYSPVModule.RefInsolation * ( ILRef + PVarray( PVnum ).TRNSYSPVModule.TempCoefIsc * ( CellTemp - PVarray( PVnum ).TRNSYSPVModule.RefTemperature ) );
				Real64 const cell_temp_ratio( CellTemp / PVarray( PVnum ).TRNSYSPVModule.RefTemperature );
				AA = AARef * cell_temp_ratio;
				IO = IORef * pow_3( cell_temp_ratio ) * std::exp( PVarray( PVnum ).TRNSYSPVModule.SemiConductorBandgap * PVarray( PVnum ).TRNSYSPVModule.CellsInSeries / AARef * ( 1.0 - PVarray( PVnum ).TRNSYSPVModule.RefTemperature / CellTemp ) );

				//  compute short curcuit current and open circuit voltage

				//   NEWTON --> ISC  (STARTVALUE: ISCG1 - BASED ON IL=ISC)
				ISCG1 = IL;
				NEWTON( ISC, FUN, FI, ISC, constant_zero, IO, IL, SeriesResistance, AA, ISCG1, EPS );

				//   NEWTON --> VOC  (STARTVALUE: VOCG1 - BASED ON IM=0.0)
				VOCG1 = ( std::log( IL / IO ) + 1.0 ) * AA;
				NEWTON( VOC, FUN, FV, constant_zero, VOC, IO, IL, SeriesResistance, AA, VOCG1, EPS );

				//  maximum power point tracking

				//   SEARCH --> VM AT MAXIMUM POWER POINT
				VLEFT = 0.0;
				VRIGHT = VOC;
				SEARCH( VLEFT, VRIGHT, VM, K, IO, IL, SeriesResistance, AA, EPS, KMAX );

				//   POWER --> IM & PM AT MAXIMUM POWER POINT
				POWER( IO, IL, SeriesResistance, AA, EPS, IM, VM, PM );

				// calculate overall PV module efficiency
				ETA = PM / PVarray( PVnum ).TRNSYSPVcalc.Insolation / PVarray( PVnum ).TRNSYSPVModule.Area;
				DummyErr = std::abs( ( ETA - EtaOld ) / EtaOld );
				EtaOld = ETA;
				++CC;

			} // while

		} else {
			// if there is no incident radiation or if the control switch is 'Off'
			{ auto const SELECT_CASE_var( PVarray( PVnum ).CellIntegrationMode );
			if ( SELECT_CASE_var == iDecoupledCellIntegration ) {
				CellTemp = Tambient;
			} else if ( SELECT_CASE_var == iDecoupledUllebergDynamicCellIntegration ) {
				CellTemp = Tambient + ( PVarray( PVnum ).TRNSYSPVcalc.LastCellTempK - Tambient ) * std::exp( -PVarray( PVnum ).TRNSYSPVModule.HeatLossCoef / PVarray( PVnum ).TRNSYSPVModule.HeatCapacity * PVTimeStep );
			} else if ( SELECT_CASE_var == iSurfaceOutsideFaceCellIntegration ) {
				CellTemp = TempSurfOut( PVarray( PVnum ).SurfacePtr ) + KelvinConv;
			} else if ( SELECT_CASE_var == iTranspiredCollectorCellIntegration ) {
				GetUTSCTsColl( PVarray( PVnum ).UTSCPtr, CellTemp );
				CellTemp += KelvinConv;
			} else if ( SELECT_CASE_var == iExteriorVentedCavityCellIntegration ) {
				GetExtVentedCavityTsColl( PVarray( PVnum ).ExtVentCavPtr, CellTemp );
				CellTemp += KelvinConv;
			} else if ( SELECT_CASE_var == iPVTSolarCollectorCellIntegration ) {
				// get PVT model result for cell temp.. //Bug CellTemp not set but used below
			} else {
				assert( false );
			}}

			PVarray( PVnum ).TRNSYSPVcalc.Insolation = 0.0;
			IM = 0.0; //module current
			VM = 0.0; //module voltage
			PM = 0.0; //module power
			ETA = 0.0; //module efficiency
			ISC = 0.0;
			VOC = 0.0;

		}

		// convert cell temperature back to C
		CellTempC = CellTemp - KelvinConv;

		// calculate array based outputs (so far, the outputs are module based
		IA = PVarray( PVnum ).NumSeriesNParall * IM;
		ISCA = PVarray( PVnum ).NumSeriesNParall * ISC;
		VA = PVarray( PVnum ).NumModNSeries * VM;
		VOCA = PVarray( PVnum ).NumModNSeries * VOC;
		PA = IA * VA;

		// Place local variables into the reporting structure
		PVarray( PVnum ).TRNSYSPVcalc.ArrayCurrent = IA;
		PVarray( PVnum ).TRNSYSPVcalc.ArrayVoltage = VA;
		PVarray( PVnum ).TRNSYSPVcalc.ArrayPower = PA;
		PVarray( PVnum ).Report.DCPower = PA;
		PVarray( PVnum ).TRNSYSPVcalc.ArrayEfficiency = ETA;
		PVarray( PVnum ).Report.ArrayEfficiency = ETA;
		PVarray( PVnum ).TRNSYSPVcalc.CellTemp = CellTempC;
		PVarray( PVnum ).Report.CellTemp = CellTempC;
		PVarray( PVnum ).TRNSYSPVcalc.CellTempK = CellTemp;
		PVarray( PVnum ).TRNSYSPVcalc.ArrayIsc = ISCA;
		PVarray( PVnum ).Report.ArrayIsc = ISCA;
		PVarray( PVnum ).TRNSYSPVcalc.ArrayVoc = VOCA;
		PVarray( PVnum ).Report.ArrayVoc = VOCA;
		PVarray( PVnum ).SurfaceSink = PA;

	}

	void
	POWER(
		Real64 const IO, // passed in from CalcPV
		Real64 const IL, // passed in from CalcPV
		Real64 const RSER, // passed in from CalcPV
		Real64 const AA, // passed in from CalcPV
		Real64 const EPS, // passed in from CalcPV
		Real64 & II, // current [A]
		Real64 & VV, // voltage [V]
		Real64 & PP // power [W]
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Ø. Ulleberg, IFE Norway for Hydrogems
		//       DATE WRITTEN   March 2001
		//       MODIFIED       D. Bradley for use with EnergyPlus
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine calculates the power produced by the PV.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE FUNCTION DECLARATIONS:
		// na

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 IG1;

		// NEWTON --> II (STARTVALUE: IG1 BASED ON SIMPLIFIED I(I,V) EQUATION)
		IG1 = IL - IO * std::exp( VV / AA - 1.0 );
		NEWTON( II, FUN, FI, II, VV, IO, IL, RSER, AA, IG1, EPS );
		PP = II * VV;

	}

	void
	NEWTON(
		Real64 & XX,
		std::function< Real64( Real64 const, Real64 const, Real64 const, Real64 const, Real64 const, Real64 const ) > FXX,
		std::function< Real64( Real64 const, Real64 const, Real64 const, Real64 const, Real64 const ) > DER,
		Real64 const & II, //Autodesk Aliased to XX in some calls
		Real64 const & VV, //Autodesk Aliased to XX in some calls
		Real64 const IO,
		Real64 const IL,
		Real64 const RSER,
		Real64 const AA,
		Real64 const XS,
		Real64 const EPS
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Ø. Ulleberg, IFE Norway for Hydrogems
		//       DATE WRITTEN   March 2001
		//       MODIFIED       D. Bradley for use with EnergyPlus
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine uses the Newton-Raphson method to solve a non linear equation with one variable.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int COUNT;
		Real64 ERR;
		Real64 X0;

		COUNT = 0;
		XX = XS;
		ERR = 1.0;
		while ( ( ERR > EPS ) && ( COUNT <= 10 ) ) {
			X0 = XX;
			XX -= FXX( II, VV, IL, IO, RSER, AA ) / DER( II, VV, IO, RSER, AA );
			++COUNT;
			ERR = std::abs( ( XX - X0 ) / X0 );
		}

	}

	void
	SEARCH(
		Real64 & A,
		Real64 & B,
		Real64 & P,
		int & K,
		Real64 & IO,
		Real64 & IL,
		Real64 & RSER,
		Real64 & AA,
		Real64 const EPS,
		int const KMAX
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Ø. Ulleberg, IFE Norway for Hydrogems
		//       DATE WRITTEN   March 2001
		//       MODIFIED       D. Bradley for use with EnergyPlus
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine minimum of an unimodal function with one variable. The algorithm was
		// adapted to find the maximum power point of a PV module. The changes to the original
		// algorithm are the following:
		// 1. a subroutine "POWER" is called in order to calculate the power output of the PV module
		// 2. the negative of the power of the PV module is taken so that the optimum can be found.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		//   /1/ MATHEWS, JOHN H.  NUMERICAL METHODS:  FORTRAN PROGRAMS. 1992, PP 413.
		//   /2/ NUMERICAL METHODS FOR MATHEMATICS, SCIENCE AND ENGINEERING, 2ND EDITION,
		//       PRENTICE HALL, NEW JERSEY, 1992.

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const DELTA( 1.e-3 );
		Real64 const EPSILON( 1.e-3 );
		static Real64 const RONE( ( std::sqrt( 5.0 ) - 1.0 ) / 2.0 );
		static Real64 const RTWO( RONE * RONE );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 C;
		Real64 D;
		Real64 H;
		Real64 YP;
		Real64 YA;
		Real64 YB;
		Real64 YC;
		Real64 YD;
		Real64 IM;
		Real64 PM;

		H = B - A;
		POWER( IO, IL, RSER, AA, EPS, IM, A, PM );
		YA = -1.0 * PM;
		POWER( IO, IL, RSER, AA, EPS, IM, B, PM );
		YB = -1.0 * PM;
		C = A + RTWO * H;
		D = A + RONE * H;
		POWER( IO, IL, RSER, AA, EPS, IM, C, PM );
		YC = -1.0 * PM;
		POWER( IO, IL, RSER, AA, EPS, IM, D, PM );
		YD = -1.0 * PM;
		K = 1;
		while ( std::abs( YB - YA ) > EPSILON || H > DELTA ) {
			if ( YC < YD ) {
				B = D;
				YB = YD;
				D = C;
				YD = YC;
				H = B - A;
				C = A + RTWO * H;
				POWER( IO, IL, RSER, AA, EPS, IM, C, PM );
				YC = -1.0 * PM;
			} else {
				A = C;
				YA = YC;
				C = D;
				YC = YD;
				H = B - A;
				D = A + RONE * H;
				POWER( IO, IL, RSER, AA, EPS, IM, D, PM );
				YD = -1.0 * PM;
			}
			++K;
		}
		if ( K < KMAX ) {
			P = A;
			YP = YA;
			if ( YB < YA ) {
				P = B;
				YP = YB;
			}
			return;
		} else {
			return;
		}
	}

	Real64
	FUN(
		Real64 const II,
		Real64 const VV,
		Real64 const IL,
		Real64 const IO,
		Real64 const RSER,
		Real64 const AA
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Ø. Ulleberg, IFE Norway for Hydrogems
		//       DATE WRITTEN   March 2001
		//       MODIFIED       D. Bradley for EnergyPlus
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// This function is based on the current-voltage characteristic of the PV module and is of the
		// form f(I,V)=0

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::RoundSigDigits;

		// Return value
		Real64 FUN( 0.0 );

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		if ( ( ( VV + II * RSER ) / AA ) < 700.0 ) {
			FUN = II - IL + IO * ( std::exp( ( VV + II * RSER ) / AA ) - 1.0 ) - ( ( VV + II * RSER ) / ShuntResistance );
		} else {
			ShowSevereError( "EquivalentOneDiode Photovoltaic model failed to find maximum power point" );
			ShowContinueError( "Numerical solver failed trying to take exponential of too large a number" );
			ShowContinueError( "Check input data in " + cPVEquiv1DiodePerfObjectName );
			ShowContinueError( "VV (voltage) = " + RoundSigDigits( VV, 5 ) );
			ShowContinueError( "II (current) = " + RoundSigDigits( II, 5 ) );
			ShowFatalError( "FUN: EnergyPlus terminates because of numerical problem in EquivalentOne-Diode PV model" );
		}

		return FUN;
	}

	Real64
	FI(
		Real64 const II,
		Real64 const VV,
		Real64 const IO,
		Real64 const RSER,
		Real64 const AA
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Ø. Ulleberg, IFE Norway for Hydrogems
		//       DATE WRITTEN   March 2001
		//       MODIFIED       D. Bradley for EnergyPlus
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// partial differential of I=I(I,V)

		// METHODOLOGY EMPLOYED:
		// the function is based on the current voltage characteristic of the PV module and is of
		// the form dF(I,V)/dI=0

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::RoundSigDigits;

		// Return value
		Real64 FI( 0.0 );

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na
		if ( ( ( VV + II * RSER ) / AA ) < 700.0 ) {
			FI = 1.0 + IO * std::exp( ( VV + II * RSER ) / AA ) * RSER / AA + ( RSER / ShuntResistance );
		} else {
			ShowSevereError( "EquivalentOneDiode Photovoltaic model failed to find maximum power point" );
			ShowContinueError( "Numerical solver failed trying to take exponential of too large a number" );
			ShowContinueError( "Check input data in " + cPVEquiv1DiodePerfObjectName );
			ShowContinueError( "VV (voltage) = " + RoundSigDigits( VV, 5 ) );
			ShowContinueError( "II (current) = " + RoundSigDigits( II, 5 ) );
			ShowFatalError( "FI: EnergyPlus terminates because of numerical problem in EquivalentOne-Diode PV model" );
		}

		return FI;
	}

	Real64
	FV(
		Real64 const II,
		Real64 const VV,
		Real64 const IO,
		Real64 const RSER,
		Real64 const AA
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Ø. Ulleberg, IFE Norway for Hydrogems
		//       DATE WRITTEN   March 2001
		//       MODIFIED       D. Bradley for EnergyPlus
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// partial differential of V=I(I,V)

		// METHODOLOGY EMPLOYED:
		// the function is based on the current voltage characteristic of the PV module and is of
		// the form dF(I,V)/dV=0

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::RoundSigDigits;

		// Return value
		Real64 FV( 0.0 );

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		if ( ( ( VV + II * RSER ) / AA ) < 700.0 ) {
			FV = IO * std::exp( ( VV + II * RSER ) / AA ) / AA + ( 1.0 / ShuntResistance );
		} else {
			ShowSevereError( "EquivalentOneDiode Photovoltaic model failed to find maximum power point" );
			ShowContinueError( "Numerical solver failed trying to take exponential of too large a number" );
			ShowContinueError( "Check input data in " + cPVEquiv1DiodePerfObjectName );
			ShowContinueError( "VV (voltage) = " + RoundSigDigits( VV, 5 ) );
			ShowContinueError( "II (current) = " + RoundSigDigits( II, 5 ) );
			ShowFatalError( "FI: EnergyPlus terminates because of numerical problem in EquivalentOne-Diode PV model" );
		}

		return FV;
	}

	// End routines for Equivalent One-Diode model as implemented by Bradley
	//************************************************************************

	// Begin supporting routines for Sandia PV model
	// -------------------------------------------------------------------------------

	Real64
	SandiaModuleTemperature(
		Real64 const Ibc, // beam radiation on collector plane, W/m2
		Real64 const Idc, // Diffuse radiation on collector plane, W/m2
		Real64 const Ws, // wind speed, m/s
		Real64 const Ta, // ambient temperature, degC
		Real64 const fd, // fraction of Idc used (empirical constant)
		Real64 const a, // empirical constant
		Real64 const b // empirical constant
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         G. Barker
		//       DATE WRITTEN   unknown
		//       MODIFIED       na
		//       RE-ENGINEERED  B.Griffith December 2003

		// PURPOSE OF THIS FUNCTION:
		// Returns back-of-module temperature, deg C

		// METHODOLOGY EMPLOYED:
		// apply sandia temperature model, This is module temp or back of
		// of the panel.  A seperate correction handles delta T for actual cell

		// REFERENCES:
		// from G. Barker's TRNSYS implementation
		// Equations (10)  in Davis, M.W., A.H. Fanney, B.P. Dougherty. Measured versus
		//   predicted performance of building integrated photovoltaics,
		//   Solar 2002, Sunrise on the Reliable Energy Economy,
		//   June 15-19, 2002, Reno, NV.

		// USE STATEMENTS:
		// na

		// Return value
		Real64 SandiaModuleTemperature;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:

		Real64 E; // total irradiance working variable

		E = Ibc + fd * Idc;

		SandiaModuleTemperature = E * std::exp( a + b * Ws ) + Ta;

		return SandiaModuleTemperature;
	}

	// -------------------------------------------------------------------------------
	// -------------------------------------------------------------------------------

	Real64
	SandiaTcellFromTmodule(
		Real64 const Tm, // module temperature (deg C)
		Real64 const Ibc, // beam radiation on collector plane, W/m2
		Real64 const Idc, // Diffuse radiation on collector plane, W/m2
		Real64 const fd, // fraction of Idc used (empirical constant)
		Real64 const DT0 // (Tc-Tm) at E=1000 W/m2 (empirical constant known as delta T), deg C
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         G. Barker
		//       DATE WRITTEN   unknown
		//       MODIFIED       na
		//       RE-ENGINEERED  B. Griffith Jan 2004 F77 -> f90

		// PURPOSE OF THIS FUNCTION:
		// Returns cell temperature, deg C

		// METHODOLOGY EMPLOYED:
		// This is for the Sandia model method of determining cell temperatures
		// module temperature differs from solar cell temperature
		// because panel temperatures are not uniform

		// REFERENCES:
		//Equations (11) in Davis, M.W., A.H. Fanney, B.P. Dougherty. Measured versus
		//   predicted performance of building integrated photovoltaics,
		//   Solar 2002, Sunrise on the Reliable Energy Economy,
		//   June 15-19, 2002, Reno, NV.

		// USE STATEMENTS:
		// na

		// Return value
		Real64 SandiaTcellFromTmodule;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:

		Real64 E; // total irradiance working variable

		E = Ibc + fd * Idc;

		SandiaTcellFromTmodule = Tm + ( E / 1000.0 ) * DT0;

		return SandiaTcellFromTmodule;
	}

	// -------------------------------------------------------------------------------

	Real64
	SandiaCellTemperature(
		Real64 const Ibc, // beam radiation on collector plane W/m2
		Real64 const Idc, // Diffuse radiation on collector plane W/m2
		Real64 const Ws, // wind speed, m/s
		Real64 const Ta, // ambient temperature, degC
		Real64 const fd, // fraction of Idc used (empirical constant)
		Real64 const a, // empirical constant
		Real64 const b, // empirical constant
		Real64 const DT0 // (Tc-Tm) at E=1000 W/m2 (empirical constant known as dTc), deg C
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         G. Barker
		//       DATE WRITTEN   unknown
		//       MODIFIED
		//       RE-ENGINEERED  B. Griffith, Jan 2004 F77-> f90

		// PURPOSE OF THIS FUNCTION:
		//  Returns cell temperature, deg C
		// METHODOLOGY EMPLOYED:
		// is this even used?  duplicates separate functions above.
		// combines function SandiaTcellFromTmodule with
		//  SandiaModuleTemperature

		// REFERENCES:
		// Equations (10) and (11) in Davis, M.W., A.H. Fanney, B.P. Dougherty. Measured versus
		//   predicted performance of building integrated photovoltaics,
		//   Solar 2002, Sunrise on the Reliable Energy Economy,
		//   June 15-19, 2002, Reno, NV.

		// USE STATEMENTS:
		// na

		// Return value
		Real64 SandiaCellTemperature;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na
		Real64 E; // irradiance working variable
		Real64 Tm;

		E = Ibc + fd * Idc;

		Tm = E * std::exp( a + b * Ws ) + Ta;

		SandiaCellTemperature = Tm + ( E / 1000.0 ) * DT0; // E0=1000.0 W/m2

		return SandiaCellTemperature;
	}

	// -------------------------------------------------------------------------------

	Real64
	SandiaEffectiveIrradiance(
		Real64 const Tc, // cell temperature (deg C)
		Real64 const Isc, // short-circuit current under operating conditions (A)
		Real64 const Isc0, // reference Isc at Tc=25 C, Ic=1000 W/m2 (A)
		Real64 const aIsc // Isc temperature coefficient (degC^-1)
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         G. Barker
		//       DATE WRITTEN   <unknown>
		//       MODIFIED       na
		//       RE-ENGINEERED  B. Griffith Jan 2004, F77 to f90

		// PURPOSE OF THIS FUNCTION:
		// Returns "effective irradiance", used in calculation of Imp, Voc, Ix, Ixx

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Real64 SandiaEffectiveIrradiance;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		SandiaEffectiveIrradiance = Isc / ( 1.0 + aIsc * ( Tc - 25.0 ) ) / Isc0;

		return SandiaEffectiveIrradiance;
	}

	// -------------------------------------------------------------------------------

	Real64
	AbsoluteAirMass(
		Real64 const SolZen, // solar zenith angle (deg)
		Real64 const Altitude // site altitude (m)
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         G. Barker
		//       DATE WRITTEN   <unknown>
		//       MODIFIED       na
		//       RE-ENGINEERED  B. Griffith Jan 2004 F77 -> f90

		// PURPOSE OF THIS FUNCTION:
		// Returns absolute air mass

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::DegToRadians;

		// Return value
		Real64 AbsoluteAirMass;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		if ( SolZen < 89.9 ) {
			Real64 const AM( 1.0 / ( std::cos( SolZen * DegToRadians ) + 0.5057 * std::pow( 96.08 - SolZen, -1.634 ) ) );
			AbsoluteAirMass = std::exp( -0.0001184 * Altitude ) * AM;
		} else {
			AbsoluteAirMass = 999.0;
			// should maybe add a show warning msg.
		}

		return AbsoluteAirMass;
	}

	// -------------------------------------------------------------------------------

	Real64
	SandiaF1(
		Real64 const AMa, // absolute air mass
		Real64 const a0, // empirical constant, module-specific
		Real64 const a1, // empirical constant, module-specific
		Real64 const a2, // empirical constant, module-specific
		Real64 const a3, // empirical constant, module-specific
		Real64 const a4 // empirical constant, module-specific
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         G. Barker
		//       DATE WRITTEN   <unknown>
		//       MODIFIED       na
		//       RE-ENGINEERED  B. Griffit F77-> f90

		// PURPOSE OF THIS FUNCTION:
		// Returns the result of Sandia Air Mass function
		//  "AMa-Function" for solar spectral influence

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// Equation (8) in Davis, M.W., A.H. Fanney, B.P. Dougherty. Measured versus
		//   predicted performance of building integrated photovoltaics,
		//   Solar 2002, Sunrise on the Reliable Energy Economy,
		//   June 15-19, 2002, Reno, NV.

		// USE STATEMENTS:
		// na

		// Return value
		Real64 SandiaF1;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:

		Real64 const F1( a0 + a1 * AMa + a2 * pow_2( AMa ) + a3 * pow_3( AMa ) + a4 * pow_4( AMa ) );

		if ( F1 > 0.0 ) {
			SandiaF1 = F1;
		} else {
			SandiaF1 = 0.0;
		}

		return SandiaF1;
	}

	// -------------------------------------------------------------------------------

	Real64
	SandiaF2(
		Real64 const IncAng, // incidence angle (deg)
		Real64 const b0, // empirical module-specific constants
		Real64 const b1, // empirical module-specific constants
		Real64 const b2, // empirical module-specific constants
		Real64 const b3, // empirical module-specific constants
		Real64 const b4, // empirical module-specific constants
		Real64 const b5 // empirical module-specific constants
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         G. Barker
		//       DATE WRITTEN   <unknown>
		//       MODIFIED       na
		//       RE-ENGINEERED  B. Griffith Jan 2004 F77-> f90

		// PURPOSE OF THIS FUNCTION:
		// C Returns Sandia F2 function

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// Equation (9) in Davis, M.W., A.H. Fanney, B.P. Dougherty. Measured versus
		//   predicted performance of building integrated photovoltaics,
		//   Solar 2002, Sunrise on the Reliable Energy Economy,
		//   June 15-19, 2002, Reno, NV.

		// USE STATEMENTS:
		// na

		// Return value
		Real64 SandiaF2;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 F2; // working variable for function result

		F2 = b0 + b1 * IncAng + b2 * pow_2( IncAng ) + b3 * pow_3( IncAng ) + b4 * pow_4( IncAng ) + b5 * pow_5( IncAng );

		if ( F2 > 0.0 ) {
			SandiaF2 = F2;
		} else {
			SandiaF2 = 0.0;
		}

		return SandiaF2;
	}

	// -------------------------------------------------------------------------------

	Real64
	SandiaImp(
		Real64 const Tc, // cell temperature (degC)
		Real64 const Ee, // effective irradiance (W/m2)
		Real64 const Imp0, // current at MPP at SRC (1000 W/m2, 25 C) (A)
		Real64 const aImp, // Imp temperature coefficient (degC^-1)
		Real64 const C0, // empirical module-specific constants
		Real64 const C1 // empirical module-specific constants
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         G. Barker
		//       DATE WRITTEN   <unknown>
		//       MODIFIED       na
		//       RE-ENGINEERED  B. Griffith F77 -> f90

		// PURPOSE OF THIS FUNCTION:
		// Returns current at maximum power point (A)

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// Equation (3) in Davis, M.W., A.H. Fanney, B.P. Dougherty. Measured versus
		//   predicted performance of building integrated photovoltaics,
		//   Solar 2002, Sunrise on the Reliable Energy Economy,
		//   June 15-19, 2002, Reno, NV.

		// USE STATEMENTS:
		// na

		// Return value
		Real64 SandiaImp;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:

		SandiaImp = Imp0 * ( C0 * Ee + C1 * pow_2( Ee ) ) * ( 1.0 + aImp * ( Tc - 25 ) );
		// why hardwire T0 at 25.0?  can this change? seems okay, fewer args
		return SandiaImp;
	}

	// -------------------------------------------------------------------------------

	Real64
	SandiaIsc(
		Real64 const Tc, // cell temperature (deg C)
		Real64 const Isc0, // Isc at Tc=25 C, Ic=1000 W/m2 (A)
		Real64 const Ibc, // beam radiation on collector plane (W/m2)
		Real64 const Idc, // Diffuse radiation on collector plane (W/m2)
		Real64 const F1, // Sandia F1 function for air mass effects
		Real64 const F2, // Sandia F2 function of incidence angle
		Real64 const fd, // module-specific empirical constant
		Real64 const aIsc // Isc temperature coefficient (degC^-1)
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         G. Barker
		//       DATE WRITTEN   <date_written>
		//       MODIFIED       na
		//       RE-ENGINEERED  B. Griffith Jan 2004 F77 -> f90

		// PURPOSE OF THIS FUNCTION:
		// Returns Short-Circuit Current

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// Equation (1) in Davis, M.W., A.H. Fanney, B.P. Dougherty. Measured versus
		//   predicted performance of building integrated photovoltaics,
		//   Solar 2002, Sunrise on the Reliable Energy Economy,
		//   June 15-19, 2002, Reno, NV.

		// USE STATEMENTS:
		// na

		// Return value
		Real64 SandiaIsc;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		// SandiaIsc=Isc0*((Ibc*F1*F2+fd*Idc)/1000.0)*(1.0+aIsc*(Tc-25.0))
		// Barkers original (above) changed to match publish eq. (1) in reference
		SandiaIsc = Isc0 * F1 * ( ( Ibc * F2 + fd * Idc ) / 1000.0 ) * ( 1.0 + aIsc * ( Tc - 25.0 ) );

		// why hardwire E0 at 1000.0 ?, can this change? seems okay

		return SandiaIsc;
	}

	// -------------------------------------------------------------------------------

	Real64
	SandiaIx(
		Real64 const Tc, // cell temperature (deg C)
		Real64 const Ee, // effective irradiance
		Real64 const Ix0, // Ix at SRC (1000 W/m2, 25 C) (A)
		Real64 const aIsc, // Isc temp coefficient (/C)
		Real64 const aImp, // Imp temp coefficient (/C)
		Real64 const C4, // empirical module-specific constants
		Real64 const C5 // empirical module-specific constants
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         G. Barker
		//       DATE WRITTEN   <unknown>
		//       MODIFIED       na
		//       RE-ENGINEERED  B. Griffith, Jan 2004 F77 -> f90

		// PURPOSE OF THIS FUNCTION:
		// Returns current "Ix" at V=0.5*Voc (A)

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// Equation 9 in King et al. nov 20003

		// USE STATEMENTS:
		// na

		// Return value
		Real64 SandiaIx;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:

		SandiaIx = Ix0 * ( C4 * Ee + C5 * pow_2( Ee ) ) * ( 1.0 + ( ( aIsc + aImp ) / 2.0 * ( Tc - 25.0 ) ) );

		return SandiaIx;
	}

	// -------------------------------------------------------------------------------

	Real64
	SandiaIxx(
		Real64 const Tc, // cell temperature (deg C)
		Real64 const Ee, // effective irradiance (W/m2 ?)
		Real64 const Ixx0, // Ixx at SRC (1000 W/m2, 25 C) (A)
		Real64 const aImp, // Imp temp coefficient (/C)
		Real64 const C6, // empirical module-specific constants
		Real64 const C7 // empirical module-specific constants
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         G. Barker
		//       DATE WRITTEN   <unknown>
		//       MODIFIED       na
		//       RE-ENGINEERED  B. Griffith Jan2004 F77 to f90

		// PURPOSE OF THIS FUNCTION:
		// Returns current "Ix" at V=0.5*(Voc+Vmp) (A)

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// Equation 10 in King et al nov. 2003

		// USE STATEMENTS:
		// na

		// Return value
		Real64 SandiaIxx;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		SandiaIxx = Ixx0 * ( C6 * Ee + C7 * pow_2( Ee ) ) * ( 1.0 + aImp * ( Tc - 25.0 ) );

		return SandiaIxx;
	}

	// -------------------------------------------------------------------------------

	Real64
	SandiaVmp(
		Real64 const Tc, // cell temperature (deg C)
		Real64 const Ee, // effective irradiance
		Real64 const Vmp0, // Vmp at SRC (1000 W/m2, 25 C) (V)
		Real64 const NcellSer, // # cells in series
		Real64 const DiodeFactor, // module-specIFic empirical constant
		Real64 const BVmp0, // Vmp temperature coefficient (V/C)
		Real64 const mBVmp, // change in BVmp with irradiance
		Real64 const C2, // empirical module-specific constants
		Real64 const C3 // empirical module-specific constants
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         G. Barker
		//       DATE WRITTEN   <unknown>
		//       MODIFIED       na
		//       RE-ENGINEERED  B. Griffith, Jan 2004, F77 -> f90

		// PURPOSE OF THIS FUNCTION:
		// Returns Voltage at Max. Power Point (V)

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// Equation 4 in King et al Nov. 2003

		// USE STATEMENTS:
		// na

		// Return value
		Real64 SandiaVmp;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:

		Real64 dTc;
		Real64 BVmpEe;

		if ( Ee > 0.0 ) {
			// following is equation 8 in King et al. nov. 2003
			dTc = DiodeFactor * ( ( 1.38066e-23 * ( Tc + KelvinConv ) ) / 1.60218e-19 );

			BVmpEe = BVmp0 + mBVmp * ( 1.0 - Ee );

			SandiaVmp = Vmp0 + C2 * NcellSer * dTc * std::log( Ee ) + C3 * NcellSer * pow_2( dTc * std::log( Ee ) ) + BVmpEe * ( Tc - 25.0 );
		} else {
			SandiaVmp = 0.0;
		}

		return SandiaVmp;
	}

	// -------------------------------------------------------------------------------

	Real64
	SandiaVoc(
		Real64 const Tc, // cell temperature (deg C)
		Real64 const Ee, // effective irradiance
		Real64 const Voc0, // Voc at SRC (1000 W/m2, 25 C) (V)
		Real64 const NcellSer, // # cells in series
		Real64 const DiodeFactor, // module-specIFic empirical constant
		Real64 const BVoc0, // Voc temperature coefficient (V/C)
		Real64 const mBVoc // change in BVoc with irradiance
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         G Barker
		//       DATE WRITTEN   <unknown>
		//       MODIFIED       na
		//       RE-ENGINEERED  B Griffith Jan 2004 F77 -> f90

		// PURPOSE OF THIS FUNCTION:
		// Returns Open-Circuit Voltage (V)

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Real64 SandiaVoc;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		Real64 dTc; // working variable
		Real64 BVocEe; // working variable

		if ( Ee > 0.0 ) {
			dTc = DiodeFactor * ( ( 1.38066e-23 * ( Tc + KelvinConv ) ) / 1.60218e-19 );
			BVocEe = BVoc0 + mBVoc * ( 1.0 - Ee );

			SandiaVoc = Voc0 + NcellSer * dTc * std::log( Ee ) + BVocEe * ( Tc - 25.0 );
		} else {
			SandiaVoc = 0.0;
		}

		return SandiaVoc;
	}

	void
	SetVentedModuleQdotSource(
		int const VentModNum,
		Real64 const QSource // source term in Watts
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   Janauray 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// object oriented "Set" routine for updating sink term without exposing variables

		// METHODOLOGY EMPLOYED:
		// update derived type with new data , turn power into W/m2

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSurfaces;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		ExtVentedCavity( VentModNum ).QdotSource = QSource / ExtVentedCavity( VentModNum ).ProjArea;

	}

	void
	GetExtVentedCavityIndex(
		int const SurfacePtr,
		int & VentCavIndex
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   January 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// object oriented "Get" routine for establishing correct integer index from outside this module

		// METHODOLOGY EMPLOYED:
		// mine Surface derived type for correct index/number of surface
		// mine  ExtVentedCavity derived type that has the surface.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using DataSurfaces::Surface;
		using DataSurfaces::ExtVentedCavity;
		using DataSurfaces::TotExtVentCav;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS:

		// DERIVED TYPE DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int CavNum; // temporary
		int ThisSurf; // temporary
		int thisCav;
		bool Found;

		if ( SurfacePtr == 0 ) {
			// should be trapped already
			ShowFatalError( "Invalid surface passed to GetExtVentedCavityIndex" );
		}

		CavNum = 0;
		Found = false;
		for ( thisCav = 1; thisCav <= TotExtVentCav; ++thisCav ) {
			for ( ThisSurf = 1; ThisSurf <= ExtVentedCavity( thisCav ).NumSurfs; ++ThisSurf ) {
				if ( SurfacePtr == ExtVentedCavity( thisCav ).SurfPtrs( ThisSurf ) ) {
					Found = true;
					CavNum = thisCav;
				}
			}
		}

		if ( ! Found ) {
			ShowFatalError( "Did not find surface in Exterior Vented Cavity description in GetExtVentedCavityIndex, Surface name = " + Surface( SurfacePtr ).Name );
		} else {

			VentCavIndex = CavNum;

		}

	}

	void
	GetExtVentedCavityTsColl(
		int const VentModNum,
		Real64 & TsColl
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         <author>
		//       DATE WRITTEN   <date_written>
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// object oriented "Get" routine for collector surface temperature

		// METHODOLOGY EMPLOYED:
		// access derived type

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Using/Aliasing
		using DataSurfaces::ExtVentedCavity;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		TsColl = ExtVentedCavity( VentModNum ).Tbaffle;

	}

	// -------------------------------------------------------------------------------

	//     EnergyPlus V1.2 and beyond include models for photovoltaic calculations called
	//     Generator:Photovoltaic:Simple and Generator:PV:Sandia implemented by the Center for
	//     Buildings and Thermal Systems, National Renewable Energy Laboratory, 1617 Cole Blvd
	//     MS 2722, Golden, CO, 80401

	//     EnergyPlus v1.1.1 and beyond includes model for Photovoltaic calculations, now
	//     referred to as the Generator:PV:Equivalent One-Diode model developed by Thermal Energy
	//     System Specialists, 2916 Marketplace Drive, Suite 104, Madison, WI 53719;
	//     Tel: (608) 274-2577

} // Photovoltaics

} // EnergyPlus
