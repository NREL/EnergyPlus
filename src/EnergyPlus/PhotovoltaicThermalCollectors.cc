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
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <PhotovoltaicThermalCollectors.hh>
#include <BranchNodeConnections.hh>
#include <ConvectionCoefficients.hh>
#include <DataAirLoop.hh>
#include <DataAirSystems.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPhotovoltaics.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DataSurfaces.hh>
#include <EMSManager.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <OutputReportPredefined.hh>
#include <PlantUtilities.hh>
#include <Psychrometrics.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace PhotovoltaicThermalCollectors {

	// Module containing the routines dealing with the photovoltaic thermal collectors

	// MODULE INFORMATION:
	//       AUTHOR         Brent. Griffith
	//       DATE WRITTEN   June-August 2008
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// collect models related to PVT or hybrid, photovoltaic - thermal solar collectors

	// METHODOLOGY EMPLOYED:
	// The approach is to have one PVT structure that works with different models.
	//  the PVT modle reuses photovoltaic modeling in Photovoltaics.cc for electricity generation.
	//  the electric load center and "generator" is all accessed thru PV objects and models.
	//  this module is for the thermal portion of PVT.
	//  the first model is a "simple" or "ideal" model useful for sizing, early design, or policy analyses
	//  Simple PV/T model just converts incoming solar to electricity and temperature rise of a working fluid.

	// REFERENCES:

	// OTHER NOTES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataGlobals;
	using DataSurfaces::Surface;
	using DataSurfaces::TotSurfaces;
	using DataSurfaces::SurfSunlitArea;
	using DataSurfaces::SurfSunlitFrac;
	using DataSurfaces::SurfaceClass_Detached_F;
	using DataSurfaces::SurfaceClass_Detached_B;
	using DataSurfaces::SurfaceClass_Shading;
	using namespace DataPhotovoltaics;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	int const SimplePVTmodel( 1001 );
	int const LayerByLayerPVTmodel( 1002 );

	int const ScheduledThermEffic( 15 ); // mode for thermal efficiency is to use schedule
	int const FixedThermEffic( 16 ); // mode for thermal efficiency is to use fixed value

	int const LiquidWorkingFluid( 1 );
	int const AirWorkingFluid( 2 );

	int const CalledFromPlantLoopEquipMgr( 101 );
	int const CalledFromOutsideAirSystem( 102 );

	Real64 const SimplePVTWaterSizeFactor( 1.905e-5 ); // [ m3/s/m2 ] average of collectors in SolarCollectors.idf

	static std::string const BlankString;

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	Array1D_bool CheckEquipName;
	int NumPVT( 0 ); // count of all types of PVT in input file
	int NumSimplePVTPerform( 0 ); // count of simple PVT performance objects in input file

	// SUBROUTINE SPECIFICATIONS FOR MODULE:
	// Driver/Manager Routines

	// Utility routines for module
	// these would be public such as:
	//PUBLIC  GetPVTIncidentSolarForInternalPVLayer
	//PUBLIC  GetPVTCellTemp

	// Object Data
	Array1D< PVTCollectorStruct > PVT;

	// Functions

	void
	SimPVTcollectors(
		int & PVTnum, // index to PVT array.
		bool const FirstHVACIteration,
		int const CalledFrom,
		Optional_string_const PVTName,
		Optional_bool_const InitLoopEquip
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         <author>
		//       DATE WRITTEN   <date_written>
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using General::TrimSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool GetInputFlag( true ); // First time, input is "gotten"

		if ( GetInputFlag ) {
			GetPVTcollectorsInput();
			GetInputFlag = false;
		}

		if ( present( PVTName ) ) {
			if ( PVTnum == 0 ) {
				PVTnum = FindItemInList( PVTName, PVT );
				if ( PVTnum == 0 ) {
					ShowFatalError( "SimPVTcollectors: Unit not found=" + PVTName() );
				}
			} else {
				if ( PVTnum > NumPVT || PVTnum < 1 ) {
					ShowFatalError( "SimPVTcollectors: Invalid PVT index passed = " + TrimSigDigits( PVTnum ) + ", Number of PVT units=" + TrimSigDigits( NumPVT ) + ", Entered Unit name=" + PVTName() );
				}
				if ( CheckEquipName( PVTnum ) ) {
					if ( PVTName != PVT( PVTnum ).Name ) {
						ShowFatalError( "SimPVTcollectors: Invalid PVT index passed = " + TrimSigDigits( PVTnum ) + ", Unit name=" + PVTName() + ", stored name for that index=" + PVT( PVTnum ).Name );
					}
					CheckEquipName( PVTnum ) = false;
				}
			}
		} else {
			if ( PVTnum > NumPVT || PVTnum < 1 ) {
				ShowFatalError( "SimPVTcollectors: Invalid PVT index passed = " + TrimSigDigits( PVTnum ) + ", Number of PVT units=" + TrimSigDigits( NumPVT ) + ", Entered Unit name=" + PVTName() );
			}
		} // compName present

		if ( present( InitLoopEquip ) ) {
			if ( InitLoopEquip ) {
				InitPVTcollectors( PVTnum, FirstHVACIteration );
				SizePVT( PVTnum );
				return;
			}
		}

		//check where called from and what type of collector this is, return if not right for calling order for speed
		if ( ( PVT( PVTnum ).WorkingFluidType == AirWorkingFluid ) && ( CalledFrom == CalledFromPlantLoopEquipMgr ) ) return;
		if ( ( PVT( PVTnum ).WorkingFluidType == LiquidWorkingFluid ) && ( CalledFrom == CalledFromOutsideAirSystem ) ) return;

		InitPVTcollectors( PVTnum, FirstHVACIteration );

		ControlPVTcollector( PVTnum );

		CalcPVTcollectors( PVTnum );

		UpdatePVTcollectors( PVTnum );

	}

	void
	GetPVTcollectorsInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   June 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Get input for PVT objects

		// METHODOLOGY EMPLOYED:
		// usual E+ methods

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using InputProcessor::SameString;
		using InputProcessor::VerifyName;
		using namespace DataIPShortCuts;
		using namespace DataHeatBalance;
		using namespace DataLoopNode;
		using DataEnvironment::StdRhoAir;
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::TestCompSet;
		using ScheduleManager::GetScheduleIndex;
		using DataSizing::AutoSize;
		using General::RoundSigDigits;
		using PlantUtilities::RegisterPlantCompDesignFlow;
		using ReportSizingManager::ReportSizingOutput;
		using namespace DataPlant; // DSU

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
		int Item; // Item to be "gotten"
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		int IOStatus; // Used in GetObjectItem
		static bool ErrorsFound( false ); // Set to true if errors in input, fatal at end of routine
		int SurfNum; // local use only
		int ThisParamObj;
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name

		// Object Data
		Array1D< SimplePVTModelStruct > tmpSimplePVTperf;

		// first load the performance object info into temporary structure
		cCurrentModuleObject = "SolarCollectorPerformance:PhotovoltaicThermal:Simple";
		NumSimplePVTPerform = GetNumObjectsFound( cCurrentModuleObject );
		if ( NumSimplePVTPerform > 0 ) {
			tmpSimplePVTperf.allocate( NumSimplePVTPerform );
			for ( Item = 1; Item <= NumSimplePVTPerform; ++Item ) {
				GetObjectItem( cCurrentModuleObject, Item, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				IsNotOK = false;
				IsBlank = false;
				VerifyName( cAlphaArgs( 1 ), tmpSimplePVTperf, Item - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Names" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) {
						ShowSevereError( "Invalid " + cCurrentModuleObject + ", Name cannot be blank" );
					}
					continue;
				}
				tmpSimplePVTperf( Item ).Name = cAlphaArgs( 1 );
				if ( SameString( cAlphaArgs( 2 ), "Fixed" ) ) {
					tmpSimplePVTperf( Item ).ThermEfficMode = FixedThermEffic;
				} else if ( SameString( cAlphaArgs( 2 ), "Scheduled" ) ) {
					tmpSimplePVTperf( Item ).ThermEfficMode = ScheduledThermEffic;
				} else {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 2 ) + " = " + cAlphaArgs( 2 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}
				tmpSimplePVTperf( Item ).ThermalActiveFract = rNumericArgs( 1 );
				tmpSimplePVTperf( Item ).ThermEffic = rNumericArgs( 2 );

				tmpSimplePVTperf( Item ).ThermEffSchedNum = GetScheduleIndex( cAlphaArgs( 3 ) );
				if ( ( tmpSimplePVTperf( Item ).ThermEffSchedNum == 0 ) && ( tmpSimplePVTperf( Item ).ThermEfficMode == ScheduledThermEffic ) ) {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 3 ) + " = " + cAlphaArgs( 3 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}
				tmpSimplePVTperf( Item ).SurfEmissivity = rNumericArgs( 3 );

			}
		} //NumSimplePVTPerform > 0

		// now get main PVT objects
		cCurrentModuleObject = "SolarCollector:FlatPlate:PhotovoltaicThermal";
		NumPVT = GetNumObjectsFound( cCurrentModuleObject );
		PVT.allocate( NumPVT );
		CheckEquipName.dimension( NumPVT, true );

		for ( Item = 1; Item <= NumPVT; ++Item ) {
			GetObjectItem( cCurrentModuleObject, Item, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			//check name
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), PVT, Item - 1, IsNotOK, IsBlank, cCurrentModuleObject );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) {
					ShowSevereError( "Invalid " + cCurrentModuleObject + ", Name cannot be blank" );
				}
				continue;
			}
			PVT( Item ).Name = cAlphaArgs( 1 );
			PVT( Item ).TypeNum = TypeOf_PVTSolarCollectorFlatPlate; //DSU, assigned in DataPlant

			PVT( Item ).SurfNum = FindItemInList( cAlphaArgs( 2 ), Surface );
			// check surface
			if ( PVT( Item ).SurfNum == 0 ) {
				if ( lAlphaFieldBlanks( 2 ) ) {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 2 ) + " = " + cAlphaArgs( 2 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
					ShowContinueError( "Surface name cannot be blank." );
				} else {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 2 ) + " = " + cAlphaArgs( 2 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
					ShowContinueError( "Surface was not found." );
				}
				ErrorsFound = true;
			} else {
				//     ! Found one -- make sure has right parameters for PVT
				SurfNum = PVT( Item ).SurfNum;

				if ( ! Surface( PVT( Item ).SurfNum ).ExtSolar ) {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 2 ) + " = " + cAlphaArgs( 2 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
					ShowContinueError( "Surface must be exposed to solar." );
					ErrorsFound = true;
				}
				// check surface orientation, warn if upside down
				if ( ( Surface( SurfNum ).Tilt < -95.0 ) || ( Surface( SurfNum ).Tilt > 95.0 ) ) {
					ShowWarningError( "Suspected input problem with " + cAlphaFieldNames( 2 ) + " = " + cAlphaArgs( 2 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
					ShowContinueError( "Surface used for solar collector faces down" );
					ShowContinueError( "Surface tilt angle (degrees from ground outward normal) = " + RoundSigDigits( Surface( SurfNum ).Tilt, 2 ) );
				}

			} // check surface

			if ( lAlphaFieldBlanks( 3 ) ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 3 ) + " = " + cAlphaArgs( 3 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
				ShowContinueError( cAlphaFieldNames( 3 ) + ", name cannot be blank." );
				ErrorsFound = true;
			} else {
				PVT( Item ).PVTModelName = cAlphaArgs( 3 );
				ThisParamObj = FindItemInList( PVT( Item ).PVTModelName, tmpSimplePVTperf );
				if ( ThisParamObj > 0 ) {
					PVT( Item ).Simple = tmpSimplePVTperf( ThisParamObj ); // entire structure assigned
					// do one-time setups on input data
					PVT( Item ).AreaCol = Surface( PVT( Item ).SurfNum ).Area * PVT( Item ).Simple.ThermalActiveFract;
					PVT( Item ).PVTModelType = SimplePVTmodel;
				} else {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 3 ) + " = " + cAlphaArgs( 3 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
					ShowContinueError( cAlphaFieldNames( 3 ) + ", was not found." );
					ErrorsFound = true;
				}

			}
			if ( allocated( PVarray ) ) { // then PV input gotten... but don't expect this to be true.
				PVT( Item ).PVnum = FindItemInList( cAlphaArgs( 4 ), PVarray );
				// check PV
				if ( PVT( Item ).PVnum == 0 ) {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 4 ) + " = " + cAlphaArgs( 4 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				} else {
					PVT( Item ).PVname = cAlphaArgs( 4 );
					PVT( Item ).PVfound = true;
				}
			} else { // no PV or not yet gotten.
				PVT( Item ).PVname = cAlphaArgs( 4 );
				PVT( Item ).PVfound = false;
			}

			if ( SameString( cAlphaArgs( 5 ), "Water" ) ) {
				PVT( Item ).WorkingFluidType = LiquidWorkingFluid;
			} else if ( SameString( cAlphaArgs( 5 ), "Air" ) ) {
				PVT( Item ).WorkingFluidType = AirWorkingFluid;
			} else {
				if ( lAlphaFieldBlanks( 5 ) ) {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 5 ) + " = " + cAlphaArgs( 5 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
					ShowContinueError( cAlphaFieldNames( 5 ) + " field cannot be blank." );
				} else {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 5 ) + " = " + cAlphaArgs( 5 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
				}
				ErrorsFound = true;
			}

			if ( PVT( Item ).WorkingFluidType == LiquidWorkingFluid ) {
				PVT( Item ).PlantInletNodeNum = GetOnlySingleNode( cAlphaArgs( 6 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
				PVT( Item ).PlantOutletNodeNum = GetOnlySingleNode( cAlphaArgs( 7 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

				TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 6 ), cAlphaArgs( 7 ), "Water Nodes" );

				PVT( Item ).WLoopSideNum = DemandSupply_No;

			}

			if ( PVT( Item ).WorkingFluidType == AirWorkingFluid ) {
				PVT( Item ).HVACInletNodeNum = GetOnlySingleNode( cAlphaArgs( 8 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
				PVT( Item ).HVACOutletNodeNum = GetOnlySingleNode( cAlphaArgs( 9 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

				TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 8 ), cAlphaArgs( 9 ), "Air Nodes" );

			}

			PVT( Item ).DesignVolFlowRate = rNumericArgs( 1 );
			PVT( Item ).SizingInit = true;
			if ( PVT( Item ).DesignVolFlowRate == AutoSize ) {
				PVT( Item ).DesignVolFlowRateWasAutoSized = true;
			}
			if ( PVT( Item ).DesignVolFlowRate != AutoSize ) {

				if ( PVT( Item ).WorkingFluidType == LiquidWorkingFluid ) {
					RegisterPlantCompDesignFlow( PVT( Item ).PlantInletNodeNum, PVT( Item ).DesignVolFlowRate );
				} else if ( PVT( Item ).WorkingFluidType == AirWorkingFluid ) {
					PVT( Item ).MaxMassFlowRate = PVT( Item ).DesignVolFlowRate * StdRhoAir;
				}
				PVT( Item ).SizingInit = false;
			}

		}

		for ( Item = 1; Item <= NumPVT; ++Item ) {
			// electrical production reporting under generator:photovoltaic....
			//    only thermal side reported here,

			SetupOutputVariable( "Generator Produced Thermal Rate [W]", PVT( Item ).Report.ThermPower, "System", "Average", PVT( Item ).Name );
			if ( PVT( Item ).WorkingFluidType == LiquidWorkingFluid ) {
				SetupOutputVariable( "Generator Produced Thermal Energy [J]", PVT( Item ).Report.ThermEnergy, "System", "Sum", PVT( Item ).Name, _, "SolarWater", "HeatProduced", _, "Plant" );
			} else if ( PVT( Item ).WorkingFluidType == AirWorkingFluid ) {
				SetupOutputVariable( "Generator Produced Thermal Energy [J]", PVT( Item ).Report.ThermEnergy, "System", "Sum", PVT( Item ).Name, _, "SolarAir", "HeatProduced", _, "System" );
				SetupOutputVariable( "Generator PVT Fluid Bypass Status []", PVT( Item ).Report.BypassStatus, "System", "Average", PVT( Item ).Name );
			}

			SetupOutputVariable( "Generator PVT Fluid Inlet Temperature [C]", PVT( Item ).Report.TinletWorkFluid, "System", "Average", PVT( Item ).Name );
			SetupOutputVariable( "Generator PVT Fluid Outlet Temperature [C]", PVT( Item ).Report.ToutletWorkFluid, "System", "Average", PVT( Item ).Name );
			SetupOutputVariable( "Generator PVT Fluid Mass Flow Rate [kg/s]", PVT( Item ).Report.MdotWorkFluid, "System", "Average", PVT( Item ).Name );
		}

		if ( ErrorsFound ) {
			ShowFatalError( "Errors found in processing input for photovoltaic thermal collectors" );
		}

		if ( allocated( tmpSimplePVTperf ) ) tmpSimplePVTperf.deallocate();

	}

	void
	InitPVTcollectors(
		int const PVTnum,
		bool const FirstHVACIteration
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   June 2008
		//       MODIFIED       B. Griffith, May 2009, EMS setpoint check
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// init for PVT

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::SysSizingCalc;
		using DataGlobals::AnyEnergyManagementSystemInModel;
		using PlantUtilities::RegisterPlantCompDesignFlow;
		using DataLoopNode::Node;
		using DataLoopNode::SensedNodeFlagValue;
		using FluidProperties::GetDensityGlycol;
		using InputProcessor::FindItemInList;
		using DataHVACGlobals::DoSetPointTest;
		using DataHVACGlobals::SetPointErrorFlag;
		using DataHeatBalance::QRadSWOutIncident;
		using General::RoundSigDigits;
		using EMSManager::iTemperatureSetPoint;
		using EMSManager::CheckIfNodeSetPointManagedByEMS;
		using DataPlant::ScanPlantLoopsForObject;
		using DataPlant::PlantLoop;
		using PlantUtilities::SetComponentFlowRate;
		using PlantUtilities::InitComponentNodes;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "InitPVTcollectors" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int InletNode;
		int OutletNode;
		int PVTindex;
		int SurfNum;
		static bool ErrorsFound( false );
		static bool MySetPointCheckFlag( true );
		static bool MyOneTimeFlag( true ); // one time flag
		static Array1D_bool SetLoopIndexFlag; // get loop number flag
		bool errFlag;
		Real64 rho; // local fluid density kg/s
		// FLOW:

		// Do the one time initializations
		if ( MyOneTimeFlag ) {
			SetLoopIndexFlag.dimension( NumPVT, true );
			MyOneTimeFlag = false;
		}

		if ( SetLoopIndexFlag( PVTnum ) ) {
			if ( allocated( PlantLoop ) && ( PVT( PVTnum ).PlantInletNodeNum > 0 ) ) {
				errFlag = false;
				ScanPlantLoopsForObject( PVT( PVTnum ).Name, PVT( PVTnum ).TypeNum, PVT( PVTnum ).WLoopNum, PVT( PVTnum ).WLoopSideNum, PVT( PVTnum ).WLoopBranchNum, PVT( PVTnum ).WLoopCompNum, _, _, _, _, _, errFlag );
				if ( errFlag ) {
					ShowFatalError( "InitPVTcollectors: Program terminated for previous conditions." );
				}
				SetLoopIndexFlag( PVTnum ) = false;
			}
		}

		// finish set up of PV, becaues PV get-input follows PVT's get input.
		if ( ! PVT( PVTnum ).PVfound ) {
			if ( allocated( PVarray ) ) {
				PVT( PVTnum ).PVnum = FindItemInList( PVT( PVTnum ).PVname, PVarray );
				if ( PVT( PVTnum ).PVnum == 0 ) {
					ShowSevereError( "Invalid name for photovoltaic generator = " + PVT( PVTnum ).PVname );
					ShowContinueError( "Entered in flat plate photovoltaic-thermal collector = " + PVT( PVTnum ).Name );
					ErrorsFound = true;
				} else {
					PVT( PVTnum ).PVfound = true;
				}
			} else {
				if ( ( ! BeginEnvrnFlag ) && ( ! FirstHVACIteration ) ) {
					ShowSevereError( "Photovoltaic generators are missing for Photovoltaic Thermal modeling" );
					ShowContinueError( "Needed for flat plate photovoltaic-thermal collector = " + PVT( PVTnum ).Name );
					ErrorsFound = true;
				}
			}
		}

		if ( ! SysSizingCalc && MySetPointCheckFlag && DoSetPointTest ) {
			for ( PVTindex = 1; PVTindex <= NumPVT; ++PVTindex ) {
				if ( PVT( PVTindex ).WorkingFluidType == AirWorkingFluid ) {
					if ( Node( PVT( PVTindex ).HVACOutletNodeNum ).TempSetPoint == SensedNodeFlagValue ) {
						if ( ! AnyEnergyManagementSystemInModel ) {
							ShowSevereError( "Missing temperature setpoint for PVT outlet node  " );
							ShowContinueError( "Add a setpoint manager to outlet node of PVT named " + PVT( PVTindex ).Name );
							SetPointErrorFlag = true;
						} else {
							// need call to EMS to check node
							CheckIfNodeSetPointManagedByEMS( PVT( PVTindex ).HVACOutletNodeNum, iTemperatureSetPoint, SetPointErrorFlag );
							if ( SetPointErrorFlag ) {
								ShowSevereError( "Missing temperature setpoint for PVT outlet node  " );
								ShowContinueError( "Add a setpoint manager to outlet node of PVT named " + PVT( PVTindex ).Name );
								ShowContinueError( "  or use an EMS actuator to establish a setpoint at the outlet node of PVT" );
							}
						}
					}
				}
			}
			MySetPointCheckFlag = false;
		}

		if ( ! SysSizingCalc && PVT( PVTnum ).SizingInit
				&& ( PVT( PVTnum ).WorkingFluidType == AirWorkingFluid ) ) {
			SizePVT( PVTnum );
		}

		{ auto const SELECT_CASE_var( PVT( PVTnum ).WorkingFluidType );
		if ( SELECT_CASE_var == LiquidWorkingFluid ) {
			InletNode = PVT( PVTnum ).PlantInletNodeNum;
			OutletNode = PVT( PVTnum ).PlantOutletNodeNum;
		} else if ( SELECT_CASE_var == AirWorkingFluid ) {
			InletNode = PVT( PVTnum ).HVACInletNodeNum;
			OutletNode = PVT( PVTnum ).HVACOutletNodeNum;
		}}

		if ( BeginEnvrnFlag && PVT( PVTnum ).EnvrnInit ) {

			PVT( PVTnum ).MassFlowRate = 0.0;
			PVT( PVTnum ).BypassDamperOff = true;
			PVT( PVTnum ).CoolingUseful = false;
			PVT( PVTnum ).HeatingUseful = false;
			PVT( PVTnum ).Simple.LastCollectorTemp = 0.0;
			PVT( PVTnum ).Simple.CollectorTemp = 0.0;
			PVT( PVTnum ).Report.ThermEfficiency = 0.0;
			PVT( PVTnum ).Report.ThermPower = 0.0;
			PVT( PVTnum ).Report.ThermHeatGain = 0.0;
			PVT( PVTnum ).Report.ThermHeatLoss = 0.0;
			PVT( PVTnum ).Report.ThermEnergy = 0.0;
			PVT( PVTnum ).Report.MdotWorkFluid = 0.0;
			PVT( PVTnum ).Report.TinletWorkFluid = 0.0;
			PVT( PVTnum ).Report.ToutletWorkFluid = 0.0;
			PVT( PVTnum ).Report.BypassStatus = 0.0;

			{ auto const SELECT_CASE_var( PVT( PVTnum ).WorkingFluidType );

			if ( SELECT_CASE_var == LiquidWorkingFluid ) {

				rho = GetDensityGlycol( PlantLoop( PVT( PVTnum ).WLoopNum ).FluidName, 60.0, PlantLoop( PVT( PVTnum ).WLoopNum ).FluidIndex, RoutineName );

				PVT( PVTnum ).MaxMassFlowRate = PVT( PVTnum ).DesignVolFlowRate * rho;

				InitComponentNodes( 0.0, PVT( PVTnum ).MaxMassFlowRate, InletNode, OutletNode, PVT( PVTnum ).WLoopNum, PVT( PVTnum ).WLoopSideNum, PVT( PVTnum ).WLoopBranchNum, PVT( PVTnum ).WLoopCompNum );

				PVT( PVTnum ).Simple.LastCollectorTemp = 23.0;

			} else if ( SELECT_CASE_var == AirWorkingFluid ) {
				PVT( PVTnum ).Simple.LastCollectorTemp = 23.0;

			}}

			PVT( PVTnum ).EnvrnInit = false;
		}
		if ( ! BeginEnvrnFlag ) PVT( PVTnum ).EnvrnInit = true;

		{ auto const SELECT_CASE_var( PVT( PVTnum ).WorkingFluidType );

		if ( SELECT_CASE_var == LiquidWorkingFluid ) {
			// heating only right now, so control flow requests based on incident solar
			SurfNum = PVT( PVTnum ).SurfNum;
			if ( QRadSWOutIncident( SurfNum ) > MinIrradiance ) {
				//IF (FirstHVACIteration) THEN
				PVT( PVTnum ).MassFlowRate = PVT( PVTnum ).MaxMassFlowRate; //DSU
				//ENDIF
			} else {
				//IF (FirstHVACIteration) THEN
				PVT( PVTnum ).MassFlowRate = 0.0; //DSU
				//ENDIF
			}
			// Should we declare a mass flow rate variable in the data structure instead of using node(outlet)%MassFlowRate ?  DSU
			SetComponentFlowRate( PVT( PVTnum ).MassFlowRate, InletNode, OutletNode, PVT( PVTnum ).WLoopNum, PVT( PVTnum ).WLoopSideNum, PVT( PVTnum ).WLoopBranchNum, PVT( PVTnum ).WLoopCompNum ); //DSU | DSU
		} else if ( SELECT_CASE_var == AirWorkingFluid ) {
			PVT( PVTnum ).MassFlowRate = Node( InletNode ).MassFlowRate;

		}}

	}

	void
	SizePVT( int const PVTnum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   August 2008
		//       MODIFIED       November 2013 Daeho Kang, add component sizing table entries
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing PVT flow rates that
		// have not been specified in the input.

		// METHODOLOGY EMPLOYED:
		// Obtains hot water flow rate from the plant sizing array.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSizing;
		using DataPlant::PlantLoop;
		using DataPlant::SupplySide;
		using DataPlant::DemandSide;
		using DataHVACGlobals::SmallWaterVolFlow;
		using DataHVACGlobals::Main;
		using DataHVACGlobals::Cooling;
		using DataHVACGlobals::Heating;
		using DataHVACGlobals::Other;
		using PlantUtilities::RegisterPlantCompDesignFlow;
		using ReportSizingManager::ReportSizingOutput;
		using namespace OutputReportPredefined;
		using DataEnvironment::StdRhoAir;
		using DataLoopNode::Node;
		using General::RoundSigDigits;
		using DataPlant::PlantFirstSizesOkayToFinalize;
		using DataPlant::PlantFirstSizesOkayToReport;
		using DataPlant::PlantFinalSizesOkayToReport;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int PltSizNum; // Plant Sizing index corresponding to CurLoopNum
		bool ErrorsFound; // If errors detected in input
		//unused1208  CHARACTER(len=MaxNameLength) :: equipName     ! Name of boiler object
		Real64 DesVolFlow;
		Real64 DesMassFlow;
		bool HardSizeNoDesRun; // Indicator to hardsize and no sizing run
		bool SizingDesRunThisAirSys; // true if a particular air system had a Sizing:System object and system sizing done
		Real64 DesignVolFlowRateDes; // Autosize design volume flow for reporting
		Real64 DesignVolFlowRateUser; // Hardsize design volume flow for reporting

		if ( SysSizingRunDone || ZoneSizingRunDone ) {
			HardSizeNoDesRun = false;
		} else {
			HardSizeNoDesRun = true;
		}
		if ( CurSysNum > 0 ) {
			CheckThisAirSystemForSizing( CurSysNum, SizingDesRunThisAirSys );
		} else {
			SizingDesRunThisAirSys = false;
		}

		DesignVolFlowRateDes = 0.0;
		DesignVolFlowRateUser = 0.0;
		PltSizNum = 0;
		ErrorsFound = false;

		if ( PVT( PVTnum ).WorkingFluidType == LiquidWorkingFluid ) {

			if ( ! allocated( PlantSizData ) ) return;
			if ( ! allocated( PlantLoop ) ) return;

			if ( PVT( PVTnum ).WLoopNum > 0 ) {
				PltSizNum = PlantLoop( PVT( PVTnum ).WLoopNum ).PlantSizNum;
			}
			if ( PVT( PVTnum ).WLoopSideNum == SupplySide ) {
				if ( PltSizNum > 0 ) {
					if ( PlantSizData( PltSizNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
						DesVolFlow = PlantSizData( PltSizNum ).DesVolFlowRate;
					} else {
						DesVolFlow = 0.0;
					}
					DesignVolFlowRateDes = DesVolFlow;
				} else {
					if ( PVT( PVTnum ).DesignVolFlowRateWasAutoSized ) {
						if ( PlantFirstSizesOkayToFinalize ) {
							ShowSevereError( "Autosizing of PVT solar collector design flow rate requires a Sizing:Plant object" );
							ShowContinueError( "Occurs in PVT object=" + PVT( PVTnum ).Name );
							ErrorsFound = true;
						}
					} else { // Hardsized
						if ( PlantFinalSizesOkayToReport && PVT( PVTnum ).DesignVolFlowRate > 0.0 ) {
							ReportSizingOutput( "SolarCollector:FlatPlate:PhotovoltaicThermal", PVT( PVTnum ).Name, "User-Specified Design Flow Rate [m3/s]", PVT( PVTnum ).DesignVolFlowRate );
						}
					}
				}
			} else if ( PVT( PVTnum ).WLoopSideNum == DemandSide ) {
				DesignVolFlowRateDes = PVT( PVTnum ).AreaCol * SimplePVTWaterSizeFactor;
			}
			if ( PVT( PVTnum ).DesignVolFlowRateWasAutoSized ) {
				PVT( PVTnum ).DesignVolFlowRate = DesignVolFlowRateDes;
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( "SolarCollector:FlatPlate:PhotovoltaicThermal", PVT( PVTnum ).Name,
						"Design Size Design Flow Rate [m3/s]", DesignVolFlowRateDes );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( "SolarCollector:FlatPlate:PhotovoltaicThermal", PVT( PVTnum ).Name,
						"Initial Design Size Design Flow Rate [m3/s]", DesignVolFlowRateDes );
				}
				RegisterPlantCompDesignFlow( PVT( PVTnum ).PlantInletNodeNum, PVT( PVTnum ).DesignVolFlowRate );

			} else { //Hardsized with sizing data
				if ( PVT( PVTnum ).DesignVolFlowRate > 0.0 && DesignVolFlowRateDes > 0.0 && PlantFinalSizesOkayToReport ) {
					DesignVolFlowRateUser = PVT( PVTnum ).DesignVolFlowRate;
					ReportSizingOutput( "SolarCollector:FlatPlate:PhotovoltaicThermal", PVT( PVTnum ).Name,
						"Design Size Design Flow Rate [m3/s]", DesignVolFlowRateDes,
						"User-Specified Design Flow Rate [m3/s]", DesignVolFlowRateUser );
					if ( DisplayExtraWarnings ) {
						if ( ( std::abs( DesignVolFlowRateDes - DesignVolFlowRateUser ) / DesignVolFlowRateUser ) > AutoVsHardSizingThreshold ) {
							ShowMessage( "SizeSolarCollector: Potential issue with equipment sizing for " + PVT( PVTnum ).Name );
							ShowContinueError( "User-Specified Design Flow Rate of " + RoundSigDigits( DesignVolFlowRateUser, 5 ) + " [W]" );
							ShowContinueError( "differs from Design Size Design Flow Rate of " + RoundSigDigits( DesignVolFlowRateDes, 5 ) + " [W]" );
							ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
							ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
						}
					}
				}
			}
		} //plant component

		if ( PVT( PVTnum ).WorkingFluidType == AirWorkingFluid ) {

			if ( CurSysNum > 0 ) {
				if ( ! PVT( PVTnum ).DesignVolFlowRateWasAutoSized && ! SizingDesRunThisAirSys ) { // Simulation continue
					HardSizeNoDesRun = true;
					if ( PVT( PVTnum ).DesignVolFlowRate > 0.0 ) {
						ReportSizingOutput( "SolarCollector:FlatPlate:PhotovoltaicThermal", PVT( PVTnum ).Name,
							"User-Specified Design Flow Rate [m3/s]", PVT( PVTnum ).DesignVolFlowRate );
					}
				} else {
					CheckSysSizing( "SolarCollector:FlatPlate:PhotovoltaicThermal", PVT( PVTnum ).Name );
					if ( CurOASysNum > 0 ) {
						DesVolFlow = FinalSysSizing( CurSysNum ).DesOutAirVolFlow;
					} else {
						{ auto const SELECT_CASE_var( CurDuctType );
						if ( SELECT_CASE_var == Main ) {
							DesVolFlow = FinalSysSizing( CurSysNum ).SysAirMinFlowRat * FinalSysSizing( CurSysNum ).DesMainVolFlow;
						} else if ( SELECT_CASE_var == Cooling ) {
							DesVolFlow = FinalSysSizing( CurSysNum ).SysAirMinFlowRat * FinalSysSizing( CurSysNum ).DesCoolVolFlow;
						} else if ( SELECT_CASE_var == Heating ) {
							DesVolFlow = FinalSysSizing( CurSysNum ).DesHeatVolFlow;
						} else if ( SELECT_CASE_var == Other ) {
							DesVolFlow = FinalSysSizing( CurSysNum ).DesMainVolFlow;
						} else {
							DesVolFlow = FinalSysSizing( CurSysNum ).DesMainVolFlow;
						}}

					}
					DesMassFlow = StdRhoAir * DesVolFlow;
					DesignVolFlowRateDes = DesVolFlow;
					PVT( PVTnum ).MaxMassFlowRate = DesMassFlow;
				}
				if ( ! HardSizeNoDesRun ) {
					if ( PVT( PVTnum ).DesignVolFlowRateWasAutoSized ) {
						PVT( PVTnum ).DesignVolFlowRate = DesignVolFlowRateDes;
						ReportSizingOutput( "SolarCollector:FlatPlate:PhotovoltaicThermal", PVT( PVTnum ).Name,
								"Design Size Design Flow Rate [m3/s]", DesignVolFlowRateDes );
						PVT( PVTnum ).SizingInit = false;
					} else {
						if ( PVT( PVTnum ).DesignVolFlowRate > 0.0 && DesignVolFlowRateDes > 0.0 ) {
							DesignVolFlowRateUser = PVT( PVTnum ).DesignVolFlowRate;
							ReportSizingOutput( "SolarCollector:FlatPlate:PhotovoltaicThermal", PVT( PVTnum ).Name, "Design Size Design Flow Rate [m3/s]", DesignVolFlowRateDes, "User-Specified Design Flow Rate [m3/s]", DesignVolFlowRateUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( DesignVolFlowRateDes - DesignVolFlowRateUser ) / DesignVolFlowRateUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeSolarCollector: Potential issue with equipment sizing for " + PVT( PVTnum ).Name );
									ShowContinueError( "User-Specified Design Flow Rate of " + RoundSigDigits( DesignVolFlowRateUser, 5 ) + " [W]" );
									ShowContinueError( "differs from Design Size Design Flow Rate of " + RoundSigDigits( DesignVolFlowRateDes, 5 ) + " [W]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
					}
				}
			} else if ( CurZoneEqNum > 0 ) {
				// PVT is not currently for zone equipment, should not come here.
			}

		}

		if ( ErrorsFound ) {
			ShowFatalError( "Preceding sizing errors cause program termination" );
		}

	}

	void
	ControlPVTcollector( int const PVTnum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   August 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// make control decisions for PVT collector

		// METHODOLOGY EMPLOYED:
		// decide if PVT should be in cooling or heat mode and if it should be bypassed or not

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataLoopNode::Node;
		using DataHeatBalance::QRadSWOutIncident;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static int SurfNum( 0 );
		//  INTEGER   :: PlantLoopNum = 0
		//  REAL(r64) :: mdot  = 0.0D0

		SurfNum = PVT( PVTnum ).SurfNum;

		if ( PVT( PVTnum ).WorkingFluidType == AirWorkingFluid ) {

			if ( PVT( PVTnum ).PVTModelType == SimplePVTmodel ) {
				if ( QRadSWOutIncident( SurfNum ) > MinIrradiance ) {
					// is heating wanted?
					//  Outlet node is required to have a setpoint.
					if ( Node( PVT( PVTnum ).HVACOutletNodeNum ).TempSetPoint > Node( PVT( PVTnum ).HVACInletNodeNum ).Temp ) {
						PVT( PVTnum ).HeatingUseful = true;
						PVT( PVTnum ).CoolingUseful = false;
						PVT( PVTnum ).BypassDamperOff = true;
					} else {
						PVT( PVTnum ).HeatingUseful = false;
						PVT( PVTnum ).CoolingUseful = true;
						PVT( PVTnum ).BypassDamperOff = false;
					}
				} else {
					// is cooling wanted?
					if ( Node( PVT( PVTnum ).HVACOutletNodeNum ).TempSetPoint < Node( PVT( PVTnum ).HVACInletNodeNum ).Temp ) {
						PVT( PVTnum ).CoolingUseful = true;
						PVT( PVTnum ).HeatingUseful = false;
						PVT( PVTnum ).BypassDamperOff = true;
					} else {
						PVT( PVTnum ).CoolingUseful = false;
						PVT( PVTnum ).HeatingUseful = true;
						PVT( PVTnum ).BypassDamperOff = false;
					}
				}
			}

		} else if ( PVT( PVTnum ).WorkingFluidType == LiquidWorkingFluid ) {
			//PlantLoopNum = PVT(PVTNum)%PlantLoopNum
			//    mdot   = Node(PVT(PVTNum)%PlantInletNodeNum)%MassFlowRate
			//If (.NOT. Allocated(PlantReport)) RETURN ! this can happen early before plant is setup
			if ( PVT( PVTnum ).PVTModelType == SimplePVTmodel ) {
				if ( QRadSWOutIncident( SurfNum ) > MinIrradiance ) {
					// is heating wanted?

					//  IF (mdot > 0.0D0) THEN
					//  If (PlantReport(PlantLoopNum)%HeatingDemand > 0.0) THEN
					PVT( PVTnum ).HeatingUseful = true;
					//          PVT(PVTnum)%CoolingUseful   = .FALSE.
					PVT( PVTnum ).BypassDamperOff = true;
					//        ELSE
					//          PVT(PVTnum)%HeatingUseful   = .FALSE.
					//          PVT(PVTnum)%CoolingUseful   = .TRUE.
					//          PVT(PVTnum)%BypassDamperOff = .FALSE.
					//        ENDIF

				} else {
					// is cooling wanted?
					//        IF (mdot > 0.0D0) THEN
					//  If (PlantReport(PlantLoopNum)%CoolingDemand > 0.0) THEN
					//          PVT(PVTnum)%CoolingUseful   = .TRUE.
					//          PVT(PVTnum)%HeatingUseful   = .FALSE.
					//          PVT(PVTnum)%BypassDamperOff = .TRUE.
					//        ELSE
					PVT( PVTnum ).CoolingUseful = false;
					//          PVT(PVTnum)%HeatingUseful   = .TRUE.
					PVT( PVTnum ).BypassDamperOff = false;
					//        ENDIF

				}
			}
		}

	}

	void
	CalcPVTcollectors( int const PVTnum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   August 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate PVT collector thermal

		// METHODOLOGY EMPLOYED:
		// Current model is "simple" fixed efficiency and simple night sky balance for cooling

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHeatBalance::VerySmooth;
		using DataHeatBalance::QRadSWOutIncident;
		using Psychrometrics::CPHW;
		using Psychrometrics::PsyCpAirFnWTdb;
		using Psychrometrics::PsyTwbFnTdbWPb;
		using Psychrometrics::PsyTdpFnTdbTwbPb;
		using DataGlobals::SecInHour;
		using DataHVACGlobals::TimeStepSys;
		using DataLoopNode::Node;
		using ScheduleManager::GetCurrentScheduleValue;
		using DataEnvironment::OutDryBulbTemp;
		using DataEnvironment::SkyTemp;
		using DataEnvironment::OutBaroPress;
		using ConvectionCoefficients::InitExteriorConvectionCoeff;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcPVTcollectors" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		static int InletNode( 0 );
		static int OutletNode( 0 );
		static Real64 Eff( 0.0 );
		static int SurfNum( 0 );
		static int RoughSurf( 0 );
		static Real64 HcExt( 0.0 );
		static Real64 HrSky( 0.0 );
		static Real64 HrGround( 0.0 );
		static Real64 HrAir( 0.0 );
		static Real64 Tcollector( 0.0 );
		static Real64 mdot( 0.0 );
		static Real64 Tinlet( 0.0 );
		static Real64 Winlet( 0.0 );
		static Real64 CpInlet( 0.0 );
		static Real64 PotentialOutletTemp( 0.0 );
		static Real64 BypassFraction( 0.0 );
		static Real64 PotentialHeatGain( 0.0 );
		static Real64 WetBulbInlet( 0.0 );
		static Real64 DewPointInlet( 0.0 );

		// flow
		SurfNum = PVT( PVTnum ).SurfNum;
		RoughSurf = VerySmooth;

		{ auto const SELECT_CASE_var( PVT( PVTnum ).WorkingFluidType );
		if ( SELECT_CASE_var == LiquidWorkingFluid ) {
			InletNode = PVT( PVTnum ).PlantInletNodeNum;
			OutletNode = PVT( PVTnum ).PlantOutletNodeNum;
		} else if ( SELECT_CASE_var == AirWorkingFluid ) {
			InletNode = PVT( PVTnum ).HVACInletNodeNum;
			OutletNode = PVT( PVTnum ).HVACOutletNodeNum;
		}}

		mdot = PVT( PVTnum ).MassFlowRate;
		Tinlet = Node( InletNode ).Temp;

		if ( PVT( PVTnum ).PVTModelType == SimplePVTmodel ) {

			if ( PVT( PVTnum ).HeatingUseful && PVT( PVTnum ).BypassDamperOff && ( mdot > 0.0 ) ) {

				{ auto const SELECT_CASE_var( PVT( PVTnum ).Simple.ThermEfficMode );

				if ( SELECT_CASE_var == FixedThermEffic ) {
					Eff = PVT( PVTnum ).Simple.ThermEffic;
				} else if ( SELECT_CASE_var == ScheduledThermEffic ) {
					Eff = GetCurrentScheduleValue( PVT( PVTnum ).Simple.ThermEffSchedNum );
					PVT( PVTnum ).Simple.ThermEffic = Eff;

				}}

				PotentialHeatGain = QRadSWOutIncident( SurfNum ) * Eff * PVT( PVTnum ).AreaCol;

				if ( PVT( PVTnum ).WorkingFluidType == AirWorkingFluid ) {
					Winlet = Node( InletNode ).HumRat;
					CpInlet = PsyCpAirFnWTdb( Winlet, Tinlet );
					if ( mdot * CpInlet > 0.0 ) {
						PotentialOutletTemp = Tinlet + PotentialHeatGain / ( mdot * CpInlet );
					} else {
						PotentialOutletTemp = Tinlet;
					}
					//now compare heating potential to setpoint and figure bypass fraction
					if ( PotentialOutletTemp > Node( PVT( PVTnum ).HVACOutletNodeNum ).TempSetPoint ) { // need to modulate
						if ( Tinlet != PotentialOutletTemp ) {
							BypassFraction = ( Node( PVT( PVTnum ).HVACOutletNodeNum ).TempSetPoint - PotentialOutletTemp ) / ( Tinlet - PotentialOutletTemp );
						} else {
							BypassFraction = 0.0;
						}
						BypassFraction = max( 0.0, BypassFraction );
						PotentialOutletTemp = Node( PVT( PVTnum ).HVACOutletNodeNum ).TempSetPoint;
						PotentialHeatGain = mdot * PsyCpAirFnWTdb( Winlet, Tinlet ) * ( PotentialOutletTemp - Tinlet );

					} else {
						BypassFraction = 0.0;
					}
				} else if ( PVT( PVTnum ).WorkingFluidType == LiquidWorkingFluid ) {
					CpInlet = CPHW( Tinlet );
					if ( mdot * CpInlet != 0.0 ) { // protect divide by zero
						PotentialOutletTemp = Tinlet + PotentialHeatGain / ( mdot * CpInlet );
					} else {
						PotentialOutletTemp = Tinlet;
					}
					BypassFraction = 0.0;

				}

				PVT( PVTnum ).Report.ThermEfficiency = Eff;
				PVT( PVTnum ).Report.ThermHeatGain = PotentialHeatGain;
				PVT( PVTnum ).Report.ThermPower = PVT( PVTnum ).Report.ThermHeatGain;
				PVT( PVTnum ).Report.ThermEnergy = PVT( PVTnum ).Report.ThermPower * TimeStepSys * SecInHour;
				PVT( PVTnum ).Report.ThermHeatLoss = 0.0;
				PVT( PVTnum ).Report.TinletWorkFluid = Tinlet;
				PVT( PVTnum ).Report.MdotWorkFluid = mdot;
				PVT( PVTnum ).Report.ToutletWorkFluid = PotentialOutletTemp;
				PVT( PVTnum ).Report.BypassStatus = BypassFraction;

			} else if ( PVT( PVTnum ).CoolingUseful && PVT( PVTnum ).BypassDamperOff && ( mdot > 0.0 ) ) {
				//calculate cooling using energy balance

				InitExteriorConvectionCoeff( SurfNum, 0.0, RoughSurf, PVT( PVTnum ).Simple.SurfEmissivity, PVT( PVTnum ).Simple.LastCollectorTemp, HcExt, HrSky, HrGround, HrAir );

				if ( PVT( PVTnum ).WorkingFluidType == AirWorkingFluid ) {
					Winlet = Node( InletNode ).HumRat;
					CpInlet = PsyCpAirFnWTdb( Winlet, Tinlet );
					WetBulbInlet = PsyTwbFnTdbWPb( Tinlet, Winlet, OutBaroPress, RoutineName );
					DewPointInlet = PsyTdpFnTdbTwbPb( Tinlet, WetBulbInlet, OutBaroPress, RoutineName );
				} else if ( PVT( PVTnum ).WorkingFluidType == LiquidWorkingFluid ) {
					CpInlet = CPHW( Tinlet );
				}

				Tcollector = ( 2.0 * mdot * CpInlet * Tinlet + PVT( PVTnum ).AreaCol * ( HrGround * OutDryBulbTemp + HrSky * SkyTemp + HrAir * Surface( SurfNum ).OutDryBulbTemp + HcExt * Surface( SurfNum ).OutDryBulbTemp ) ) / ( 2.0 * mdot * CpInlet + PVT( PVTnum ).AreaCol * ( HrGround + HrSky + HrAir + HcExt ) );

				PotentialOutletTemp = 2.0 * Tcollector - Tinlet;
				PVT( PVTnum ).Report.ToutletWorkFluid = PotentialOutletTemp;
				// trap for air not being cooled below its wetbulb.
				if ( PVT( PVTnum ).WorkingFluidType == AirWorkingFluid ) {
					if ( PotentialOutletTemp < DewPointInlet ) {
						//  water removal would be needed.. not going to allow that for now.  limit cooling to dew point and model bypass
						if ( Tinlet != PotentialOutletTemp ) {
							BypassFraction = ( DewPointInlet - PotentialOutletTemp ) / ( Tinlet - PotentialOutletTemp );
						} else {
							BypassFraction = 0.0;
						}
						BypassFraction = max( 0.0, BypassFraction );
						PotentialOutletTemp = DewPointInlet;

					}
				}

				PVT( PVTnum ).Report.MdotWorkFluid = mdot;
				PVT( PVTnum ).Report.TinletWorkFluid = Tinlet;
				PVT( PVTnum ).Report.ToutletWorkFluid = PotentialOutletTemp;
				PVT( PVTnum ).Report.ThermHeatLoss = mdot * CpInlet * ( Tinlet - PVT( PVTnum ).Report.ToutletWorkFluid );
				PVT( PVTnum ).Report.ThermHeatGain = 0.0;
				PVT( PVTnum ).Report.ThermPower = -1.0 * PVT( PVTnum ).Report.ThermHeatLoss;
				PVT( PVTnum ).Report.ThermEnergy = PVT( PVTnum ).Report.ThermPower * TimeStepSys * SecInHour;
				PVT( PVTnum ).Report.ThermEfficiency = 0.0;
				PVT( PVTnum ).Simple.LastCollectorTemp = Tcollector;
				PVT( PVTnum ).Report.BypassStatus = 0.0;

			} else if ( ! PVT( PVTnum ).BypassDamperOff ) { // bypassed, zero things out

				PVT( PVTnum ).Report.TinletWorkFluid = Tinlet;
				PVT( PVTnum ).Report.ToutletWorkFluid = Tinlet;
				PVT( PVTnum ).Report.ThermHeatLoss = 0.0;
				PVT( PVTnum ).Report.ThermHeatGain = 0.0;
				PVT( PVTnum ).Report.ThermPower = 0.0;
				PVT( PVTnum ).Report.ThermEfficiency = 0.0;
				PVT( PVTnum ).Report.ThermEnergy = 0.0;
				PVT( PVTnum ).Report.BypassStatus = 1.0;
				PVT( PVTnum ).Report.MdotWorkFluid = mdot;

			} else {
				PVT( PVTnum ).Report.TinletWorkFluid = Tinlet;
				PVT( PVTnum ).Report.ToutletWorkFluid = Tinlet;
				PVT( PVTnum ).Report.ThermHeatLoss = 0.0;
				PVT( PVTnum ).Report.ThermHeatGain = 0.0;
				PVT( PVTnum ).Report.ThermPower = 0.0;
				PVT( PVTnum ).Report.ThermEfficiency = 0.0;
				PVT( PVTnum ).Report.ThermEnergy = 0.0;
				PVT( PVTnum ).Report.BypassStatus = 1.0;
				PVT( PVTnum ).Report.MdotWorkFluid = mdot;
			}
		}

	}

	void
	UpdatePVTcollectors( int const PVTnum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   August 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataLoopNode::Node;
		using Psychrometrics::PsyHFnTdbW;
		using PlantUtilities::SafeCopyPlantNode;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int InletNode;
		int OutletNode;

		{ auto const SELECT_CASE_var( PVT( PVTnum ).WorkingFluidType );
		if ( SELECT_CASE_var == LiquidWorkingFluid ) {
			InletNode = PVT( PVTnum ).PlantInletNodeNum;
			OutletNode = PVT( PVTnum ).PlantOutletNodeNum;

			SafeCopyPlantNode( InletNode, OutletNode );
			Node( OutletNode ).Temp = PVT( PVTnum ).Report.ToutletWorkFluid;

		} else if ( SELECT_CASE_var == AirWorkingFluid ) {
			InletNode = PVT( PVTnum ).HVACInletNodeNum;
			OutletNode = PVT( PVTnum ).HVACOutletNodeNum;

			// Set the outlet nodes for properties that just pass through & not used
			Node( OutletNode ).Quality = Node( InletNode ).Quality;
			Node( OutletNode ).Press = Node( InletNode ).Press;
			Node( OutletNode ).MassFlowRate = Node( InletNode ).MassFlowRate;
			Node( OutletNode ).MassFlowRateMin = Node( InletNode ).MassFlowRateMin;
			Node( OutletNode ).MassFlowRateMax = Node( InletNode ).MassFlowRateMax;
			Node( OutletNode ).MassFlowRateMinAvail = Node( InletNode ).MassFlowRateMinAvail;
			Node( OutletNode ).MassFlowRateMaxAvail = Node( InletNode ).MassFlowRateMaxAvail;

			// Set outlet node variables that are possibly changed
			Node( OutletNode ).Temp = PVT( PVTnum ).Report.ToutletWorkFluid;
			Node( OutletNode ).HumRat = Node( InletNode ).HumRat; // assumes dewpoint bound on cooling ....
			Node( OutletNode ).Enthalpy = PsyHFnTdbW( PVT( PVTnum ).Report.ToutletWorkFluid, Node( OutletNode ).HumRat );
		}}

	}

	void
	GetPVTThermalPowerProduction(
		int const PVindex, // index of PV generator (not PVT collector)
		Real64 & ThermalPower,
		Real64 & ThermalEnergy
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         <author>
		//       DATE WRITTEN   <date_written>
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static int PVTnum( 0 );
		static int loop( 0 );

		// first find PVT index that is associated with this PV generator
		for ( loop = 1; loop <= NumPVT; ++loop ) {
			if ( ! PVT( loop ).PVfound ) continue;
			if ( PVT( loop ).PVnum == PVindex ) { // we found it
				PVTnum = loop;
			}
		}

		if ( PVTnum > 0 ) {
			ThermalPower = PVT( PVTnum ).Report.ThermPower;
			ThermalEnergy = PVT( PVTnum ).Report.ThermEnergy;
		} else {
			ThermalPower = 0.0;
			ThermalEnergy = 0.0;
		}

	}

	//=====================  Utility/Other routines for module.
	// Insert as appropriate

} // PhotovoltaicThermalCollectors

} // EnergyPlus
