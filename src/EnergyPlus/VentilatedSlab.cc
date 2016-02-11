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
#include <VentilatedSlab.hh>
#include <BranchNodeConnections.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHeatBalSurface.hh>
#include <DataHVACGlobals.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataSizing.hh>
#include <DataSurfaceLists.hh>
#include <DataSurfaces.hh>
#include <DataZoneEnergyDemands.hh>
#include <DataZoneEquipment.hh>
#include <Fans.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <HeatingCoils.hh>
#include <HVACHXAssistedCoolingCoil.hh>
#include <HeatBalanceSurfaceManager.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutAirNodeManager.hh>
#include <OutputProcessor.hh>
#include <PlantUtilities.hh>
#include <Psychrometrics.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <SteamCoils.hh>
#include <UtilityRoutines.hh>
#include <WaterCoils.hh>

namespace EnergyPlus {

namespace VentilatedSlab {

	// Module containing the routines dealing with the Ventilated Slab

	// MODULE INFORMATION:
	//       AUTHOR         Young Tae Chae, Rick Strand
	//       DATE WRITTEN   June 2008
	//       MODIFIED
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// Simulate Ventilated Slab Systems.

	// METHODOLOGY EMPLOYED:
	// Systems are modeled as a collection of components: radiant panel, outside air mixer,
	// fan, heating coil and/or cooling coil plus an integrated control
	// algorithm that adjusts the hot or cold water flow to meet the setpoint
	// condition.  Outside air mixing is handled locally as either fixed percent
	// or as attempting to meet a prescribed mixed air temperature.

	// REFERENCES:
	// ASHRAE Systems and Equipment Handbook (SI), 1996. pp. 31.1-31.3
	// Fred Buhl's fan coil module (FanCoilUnits.cc)

	// OTHER NOTES: none

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataLoopNode;
	using DataGlobals::BeginEnvrnFlag;
	using DataGlobals::BeginDayFlag;
	using DataGlobals::BeginTimeStepFlag;
	using DataGlobals::InitConvTemp;
	using DataGlobals::SysSizingCalc;
	using DataGlobals::WarmupFlag;
	using DataGlobals::DisplayExtraWarnings;

	using DataSurfaces::Surface;
	using DataSurfaces::TotSurfaces;
	using DataHeatBalFanSys::QRadSysSource;
	using DataHVACGlobals::FanElecPower;
	using DataHVACGlobals::SmallAirVolFlow;
	using DataHVACGlobals::ContFanCycCoil;

	// Use statements for access to subroutines in other modules
	using namespace ScheduleManager;
	using namespace Psychrometrics;
	using namespace FluidProperties;

	// Data
	// MODULE PARAMETER DEFINITIONS

	// Module Object
	std::string const cMO_VentilatedSlab( "ZoneHVAC:VentilatedSlab" );
	static std::string const BlankString;

	// Parameters for outside air control types:
	int const Heating_ElectricCoilType( 1 );
	int const Heating_GasCoilType( 2 );
	int const Heating_WaterCoilType( 3 );
	int const Heating_SteamCoilType( 4 );
	int const Cooling_CoilWaterCooling( 1 );
	int const Cooling_CoilDetailedCooling( 2 );
	int const Cooling_CoilHXAssisted( 3 );
	int const VariablePercent( 1 );
	int const FixedTemperature( 2 );
	int const FixedOAControl( 3 );
	int const NotOperating( 0 ); // Parameter for use with OperatingMode variable, set for no heating/cooling
	int const HeatingMode( 1 ); // Parameter for use with OperatingMode variable, set for heating
	int const CoolingMode( 2 ); // Parameter for use with OperatingMode variable, set for cooling
	//Ventilated Slab Configurations
	int const SlabOnly( 1 ); // Air circulate through cores of slab only
	int const SlabAndZone( 2 ); // Circulated Air is introduced to zone
	int const SeriesSlabs( 3 );
	//  Control Types
	int const MATControl( 1 ); // Controls system using mean air temperature
	int const MRTControl( 2 ); // Controls system using mean radiant temperature
	int const OPTControl( 3 ); // Controls system using operative temperature
	int const ODBControl( 4 ); // Controls system using outside air dry-bulb temperature
	int const OWBControl( 5 ); // Controls system using outside air wet-bulb temperature
	int const SURControl( 6 ); // Controls system using surface temperature !Phase2-A
	int const DPTZControl( 7 ); // Controls system using dew-point temperature of zone!Phase2-A

	// coil operation
	int const On( 1 ); // normal coil operation
	int const Off( 0 ); // signal coil shouldn't run
	int const NoneOption( 0 );
	int const BothOption( 1 );
	int const HeatingOption( 2 );
	int const CoolingOption( 3 );
	int OperatingMode( 0 ); // Used to keep track of whether system is in heating or cooling mode

	static std::string const fluidNameSteam( "STEAM" );
	static std::string const fluidNameWater( "WATER" );

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	bool HCoilOn( false ); // TRUE if the heating coil (gas or electric especially) should be running
	int NumOfVentSlabs( 0 ); // Number of ventilated slab in the input file
	Real64 OAMassFlowRate( 0.0 ); // Outside air mass flow rate for the ventilated slab
	Array1D_double QRadSysSrcAvg; // Average source over the time step for a particular radiant surfaceD
	Array1D< Real64 > ZeroSourceSumHATsurf; // Equal to SumHATsurf for all the walls in a zone with no source
	int MaxCloNumOfSurfaces( 0 ); // Used to set allocate size in CalcClo routine
	Real64 QZnReq( 0.0 ); // heating or cooling needed by system [watts]

	// Record keeping variables used to calculate QRadSysSrcAvg locally

	Array1D_double LastQRadSysSrc; // Need to keep the last value in case we are still iterating
	Array1D< Real64 > LastSysTimeElapsed; // Need to keep the last value in case we are still iterating
	Array1D< Real64 > LastTimeStepSys; // Need to keep the last value in case we are still iterating
	Array1D_bool CheckEquipName;

	// Autosizing variables
	Array1D_bool MySizeFlag;

	// SUBROUTINE SPECIFICATIONS FOR MODULE VentilatedSlab
	// PRIVATE UpdateVentilatedSlabValAvg

	// Object Data
	Array1D< VentilatedSlabData > VentSlab;
	Array1D< VentSlabNumericFieldData > VentSlabNumericFields;

	// Functions

	void
	clear_state()
	{
		HCoilOn = false;
		NumOfVentSlabs = 0;
		OAMassFlowRate = 0.0;
		MaxCloNumOfSurfaces = 0;
		QZnReq = 0.0;
		QRadSysSrcAvg.deallocate();
		ZeroSourceSumHATsurf.deallocate();
		LastQRadSysSrc.deallocate();
		LastSysTimeElapsed.deallocate();
		LastTimeStepSys.deallocate();
		CheckEquipName.deallocate();
		MySizeFlag.deallocate();
		VentSlab.deallocate();
		VentSlabNumericFields.deallocate();
	}

	void
	SimVentilatedSlab(
		std::string const & CompName, // name of the fan coil unit
		int const ZoneNum, // number of zone being served
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & PowerMet, // Sensible power supplied (W)
		Real64 & LatOutputProvided, // Latent add/removal supplied by window AC (kg/s), dehumid = negative
		int & CompIndex
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   May 2000
		//       MODIFIED       Don Shirey, Aug 2009 (LatOutputProvided)
		//       RE-ENGINEERED
		// This is re-engineered by Rick Strand and Young T. Chae for Ventilated Slab (June, 2008)

		// PURPOSE OF THIS SUBROUTINE:
		// This is the main driver subroutine for the Ventilated Slab simulation.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using General::TrimSigDigits;
		using DataSizing::ZoneEqVentedSlab;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Item; // index of ventilated slab being simulated
		static bool GetInputFlag( true ); // First time, input is "gotten"

		// FLOW:
		if ( GetInputFlag ) {
			GetVentilatedSlabInput();
			GetInputFlag = false;
		}

		// Find the correct VentilatedSlabInput
		if ( CompIndex == 0 ) {
			Item = FindItemInList( CompName, VentSlab );
			if ( Item == 0 ) {
				ShowFatalError( "SimVentilatedSlab: system not found=" + CompName );
			}
			CompIndex = Item;
		} else {
			Item = CompIndex;
			if ( Item > NumOfVentSlabs || Item < 1 ) {
				ShowFatalError( "SimVentilatedSlab:  Invalid CompIndex passed=" + TrimSigDigits( Item ) + ", Number of Systems=" + TrimSigDigits( NumOfVentSlabs ) + ", Entered System name=" + CompName );
			}
			if ( CheckEquipName( Item ) ) {
				if ( CompName != VentSlab( Item ).Name ) {
					ShowFatalError( "SimVentilatedSlab: Invalid CompIndex passed=" + TrimSigDigits( Item ) + ", System name=" + CompName + ", stored System Name for that index=" + VentSlab( Item ).Name );
				}
				CheckEquipName( Item ) = false;
			}
		}

		ZoneEqVentedSlab = true;

		InitVentilatedSlab( Item, ZoneNum, FirstHVACIteration );

		CalcVentilatedSlab( Item, ZoneNum, FirstHVACIteration, PowerMet, LatOutputProvided );

		UpdateVentilatedSlab( Item, FirstHVACIteration );

		ReportVentilatedSlab( Item );

		ZoneEqVentedSlab = false;
	}

	void
	GetVentilatedSlabInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Young Tae Chae, Rick Strand
		//       DATE WRITTEN   June 2008
		//       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine obtains the input for ventilated slab and sets
		// up the appropriate derived type.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// REFERENCES:
		// Fred Buhl's fan coil module (FanCoilUnits.cc)
		// Kwang Ho Lee's Unit Ventilator Module (UnitVentilator.cc)
		// Rick Strand's Low temperature Radiant system (RadiantSystemLowTemp.cc)

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using InputProcessor::FindItemInList;
		using InputProcessor::GetObjectDefMaxArgs;
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::SetUpCompSets;
		auto & GetWaterCoilMaxFlowRate( WaterCoils::GetCoilMaxWaterFlowRate );
		auto & GetSteamCoilMaxFlowRate( SteamCoils::GetCoilMaxWaterFlowRate );
		auto & GetHXAssistedCoilFlowRate( HVACHXAssistedCoolingCoil::GetCoilMaxWaterFlowRate );
		using HVACHXAssistedCoolingCoil::GetHXCoilTypeAndName;
		using DataGlobals::ScheduleAlwaysOn;
		using DataHeatBalance::Zone;
		using DataHeatBalance::Construct;
		using ScheduleManager::GetScheduleIndex;
		using namespace DataLoopNode;
		using namespace DataSurfaceLists;
		using OutAirNodeManager::CheckAndAddAirNodeNumber;
		using FluidProperties::FindRefrigerant;
		using DataPlant::TypeOf_CoilWaterCooling;
		using DataPlant::TypeOf_CoilWaterDetailedFlatCooling;
		using DataPlant::TypeOf_CoilWaterSimpleHeating;
		using DataPlant::TypeOf_CoilSteamAirHeating;
		using DataSizing::ZoneHVACSizing;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const MeanAirTemperature( "MeanAirTemperature" );
		static std::string const MeanRadiantTemperature( "MeanRadiantTemperature" );
		static std::string const OperativeTemperature( "OperativeTemperature" );
		static std::string const OutsideAirDryBulbTemperature( "OutdoorDryBulbTemperature" );
		static std::string const OutsideAirWetBulbTemperature( "OutdoorWetBulbTemperature" );
		static std::string const SlabSurfaceTemperature( "SurfaceTemperature" );
		static std::string const SlabSurfaceDewPointTemperature( "ZoneAirDewPointTemperature" );
		static std::string const CurrentModuleObject( "ZoneHVAC:VentilatedSlab" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool ErrorsFound( false ); // Set to true if errors in input, fatal at end of routine
		int IOStatus; // Used in GetObjectItem
		bool IsBlank; // TRUE if the name is blank
		bool IsNotOK; // TRUE if there was a problem with a list name
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumArgs; // Unused variable that is part of a subroutine call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		int Item; // Item to be "gotten"
		int BaseNum; // Temporary number for creating RadiantSystemTypes structure
		bool errFlag; // interim error flag
		int SurfListNum; // Index within the SurfList derived type for a surface list name
		//unused0309  INTEGER                         :: NumOfSurfListVB  ! Number of surface lists in the user input file
		int SurfNum; // DO loop counter for surfaces
		bool IsValid; // Set for outside air node check
		Array1D_string cAlphaArgs; // Alpha input items for object
		Array1D_string cAlphaFields; // Alpha field names
		Array1D_string cNumericFields; // Numeric field names
		Array1D< Real64 > rNumericArgs; // Numeric input items for object
		Array1D_bool lAlphaBlanks; // Logical array, alpha field input BLANK = .TRUE.
		Array1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.
		bool SteamMessageNeeded;

		// FLOW:
		// Figure out how many Ventilated Slab Systems there are in the input file

		SteamMessageNeeded = true;
		GetObjectDefMaxArgs( CurrentModuleObject, NumArgs, NumAlphas, NumNumbers );
		cAlphaArgs.allocate( NumAlphas );
		cAlphaFields.allocate( NumAlphas );
		cNumericFields.allocate( NumNumbers );
		rNumericArgs.dimension( NumNumbers, 0.0 );
		lAlphaBlanks.dimension( NumAlphas, true );
		lNumericBlanks.dimension( NumNumbers, true );

		// make sure data is gotten for surface lists
		BaseNum = GetNumberOfSurfListVentSlab();

		NumOfVentSlabs = GetNumObjectsFound( CurrentModuleObject );
		// Allocate the local derived type and do one-time initializations for all parts of it

		VentSlab.allocate( NumOfVentSlabs );
		CheckEquipName.dimension( NumOfVentSlabs, true );
		VentSlabNumericFields.allocate( NumOfVentSlabs );

		for ( Item = 1; Item <= NumOfVentSlabs; ++Item ) { // Begin looping over the entire ventilated slab systems found in the input file...

			GetObjectItem( CurrentModuleObject, Item, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			VentSlabNumericFields( Item ).FieldNames.allocate( NumNumbers );
			VentSlabNumericFields( Item ).FieldNames = cNumericFields;

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), VentSlab, Item - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}

			VentSlab( Item ).Name = cAlphaArgs( 1 );
			VentSlab( Item ).SchedName = cAlphaArgs( 2 );
			if ( lAlphaBlanks( 2 ) ) {
				VentSlab( Item ).SchedPtr = ScheduleAlwaysOn;
			} else {
				VentSlab( Item ).SchedPtr = GetScheduleIndex( cAlphaArgs( 2 ) ); // convert schedule name to pointer
				if ( VentSlab( Item ).SchedPtr == 0 ) {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFields( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\" not found." );
					ErrorsFound = true;
				}
			}

			VentSlab( Item ).ZoneName = cAlphaArgs( 3 );
			VentSlab( Item ).ZonePtr = FindItemInList( cAlphaArgs( 3 ), Zone );
			if ( VentSlab( Item ).ZonePtr == 0 ) {
				if ( lAlphaBlanks( 3 ) ) {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFields( 3 ) + " is required but input is blank." );
				} else {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFields( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\" not found." );
				}
				ErrorsFound = true;
			}

			VentSlab( Item ).SurfListName = cAlphaArgs( 4 );
			SurfListNum = 0;
			//    IF (NumOfSlabLists > 0) SurfListNum = FindItemInList(VentSlab(Item)%SurfListName, SlabList%Name, NumOfSlabLists)
			if ( NumOfSurfListVentSlab > 0 ) SurfListNum = FindItemInList( VentSlab( Item ).SurfListName, SlabList );
			if ( SurfListNum > 0 ) { // Found a valid surface list
				VentSlab( Item ).NumOfSurfaces = SlabList( SurfListNum ).NumOfSurfaces;
				VentSlab( Item ).ZName.allocate( VentSlab( Item ).NumOfSurfaces );
				VentSlab( Item ).ZPtr.allocate( VentSlab( Item ).NumOfSurfaces );
				VentSlab( Item ).SurfaceName.allocate( VentSlab( Item ).NumOfSurfaces );
				VentSlab( Item ).SurfacePtr.allocate( VentSlab( Item ).NumOfSurfaces );
				VentSlab( Item ).CDiameter.allocate( VentSlab( Item ).NumOfSurfaces );
				VentSlab( Item ).CLength.allocate( VentSlab( Item ).NumOfSurfaces );
				VentSlab( Item ).CNumbers.allocate( VentSlab( Item ).NumOfSurfaces );
				VentSlab( Item ).SlabIn.allocate( VentSlab( Item ).NumOfSurfaces );
				VentSlab( Item ).SlabOut.allocate( VentSlab( Item ).NumOfSurfaces );

				MaxCloNumOfSurfaces = max( MaxCloNumOfSurfaces, VentSlab( Item ).NumOfSurfaces );
				for ( SurfNum = 1; SurfNum <= SlabList( SurfListNum ).NumOfSurfaces; ++SurfNum ) {
					VentSlab( Item ).ZName( SurfNum ) = SlabList( SurfListNum ).ZoneName( SurfNum );
					VentSlab( Item ).ZPtr( SurfNum ) = SlabList( SurfListNum ).ZonePtr( SurfNum );
					VentSlab( Item ).SurfaceName( SurfNum ) = SlabList( SurfListNum ).SurfName( SurfNum );
					VentSlab( Item ).SurfacePtr( SurfNum ) = SlabList( SurfListNum ).SurfPtr( SurfNum );
					VentSlab( Item ).CDiameter( SurfNum ) = SlabList( SurfListNum ).CoreDiameter( SurfNum );
					VentSlab( Item ).CLength( SurfNum ) = SlabList( SurfListNum ).CoreLength( SurfNum );
					VentSlab( Item ).CNumbers( SurfNum ) = SlabList( SurfListNum ).CoreNumbers( SurfNum );
					VentSlab( Item ).SlabIn( SurfNum ) = SlabList( SurfListNum ).SlabInNodeName( SurfNum );
					VentSlab( Item ).SlabOut( SurfNum ) = SlabList( SurfListNum ).SlabOutNodeName( SurfNum );
					if ( VentSlab( Item ).SurfacePtr( SurfNum ) != 0 ) {
						Surface( VentSlab( Item ).SurfacePtr( SurfNum ) ).IntConvSurfHasActiveInIt = true;
					}
				}

			} else { // User entered a single surface name rather than a surface list
				VentSlab( Item ).NumOfSurfaces = 1;
				VentSlab( Item ).SurfacePtr.allocate( VentSlab( Item ).NumOfSurfaces );
				VentSlab( Item ).SurfaceName.allocate( VentSlab( Item ).NumOfSurfaces );
				VentSlab( Item ).SurfaceFlowFrac.allocate( VentSlab( Item ).NumOfSurfaces );
				MaxCloNumOfSurfaces = max( MaxCloNumOfSurfaces, VentSlab( Item ).NumOfSurfaces );
				VentSlab( Item ).SurfaceName( 1 ) = VentSlab( Item ).SurfListName;
				VentSlab( Item ).SurfacePtr( 1 ) = FindItemInList( VentSlab( Item ).SurfaceName( 1 ), Surface );
				VentSlab( Item ).SurfaceFlowFrac( 1 ) = 1.0;
				// Error checking for single surfaces
				if ( VentSlab( Item ).SurfacePtr( 1 ) == 0 ) {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFields( 4 ) + "=\"" + cAlphaArgs( 4 ) + "\" not found." );
					ErrorsFound = true;
				} else if ( Surface( VentSlab( Item ).SurfacePtr( 1 ) ).PartOfVentSlabOrRadiantSurface ) {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid Surface" );
					ShowContinueError( cAlphaFields( 4 ) + "=\"" + cAlphaArgs( 4 ) + "\" has been used in another radiant system or ventilated slab." );
					ErrorsFound = true;
				}
				if ( VentSlab( Item ).SurfacePtr( 1 ) != 0 ) {
					Surface( VentSlab( Item ).SurfacePtr( 1 ) ).IntConvSurfHasActiveInIt = true;
					Surface( VentSlab( Item ).SurfacePtr( 1 ) ).PartOfVentSlabOrRadiantSurface = true;
				}
			}

			// Error checking for zones and construction information

			if ( SurfListNum > 0 ) {

				for ( SurfNum = 1; SurfNum <= VentSlab( Item ).NumOfSurfaces; ++SurfNum ) {

					if ( VentSlab( Item ).SurfacePtr( SurfNum ) == 0 ) continue; // invalid surface -- detected earlier
					if ( VentSlab( Item ).ZPtr( SurfNum ) == 0 ) continue; // invalid zone -- detected earlier
					//      IF (Surface(VentSlab(Item)%SurfacePtr(SurfNum))%Zone /= VentSlab(Item)%ZPtr(SurfNum)) THEN
					//        CALL ShowSevereError(TRIM(CurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'" invalid '//   &
					//          'surface="'//TRIM(Surface(VentSlab(Item)%SurfacePtr(SurfNum))%Name)//'".')
					//        CALL ShowContinueError('Surface in Zone='//TRIM(Zone(Surface(VentSlab(Item)%SurfacePtr(SurfNum))%Zone)%Name)//' '// &
					//                         CurrentModuleObject//' in Zone='//TRIM(cAlphaArgs(3)))
					//        ErrorsFound=.TRUE.
					//      END IF
					if ( Surface( VentSlab( Item ).SurfacePtr( SurfNum ) ).Construction == 0 ) continue; // invalid construction, detected earlier
					if ( ! Construct( Surface( VentSlab( Item ).SurfacePtr( SurfNum ) ).Construction ).SourceSinkPresent ) {
						ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid surface=\"" + Surface( VentSlab( Item ).SurfacePtr( SurfNum ) ).Name + "\"." );
						ShowContinueError( "Surface Construction does not have a source/sink, Construction name= \"" + Construct( Surface( VentSlab( Item ).SurfacePtr( SurfNum ) ).Construction ).Name + "\"." );
						ErrorsFound = true;
					}
				}
			} else {
				for ( SurfNum = 1; SurfNum <= VentSlab( Item ).NumOfSurfaces; ++SurfNum ) {
					if ( VentSlab( Item ).SurfacePtr( SurfNum ) == 0 ) continue; // invalid surface -- detected earlier
					if ( VentSlab( Item ).ZonePtr == 0 ) continue; // invalid zone -- detected earlier
					if ( Surface( VentSlab( Item ).SurfacePtr( SurfNum ) ).Zone != VentSlab( Item ).ZonePtr ) {
						ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid surface=\"" + Surface( VentSlab( Item ).SurfacePtr( SurfNum ) ).Name + "\"." );
						ShowContinueError( "Surface in Zone=" + Zone( Surface( VentSlab( Item ).SurfacePtr( SurfNum ) ).Zone ).Name + ' ' + CurrentModuleObject + " in Zone=" + cAlphaArgs( 3 ) );
						ErrorsFound = true;
					}
					if ( Surface( VentSlab( Item ).SurfacePtr( SurfNum ) ).Construction == 0 ) continue; // invalid construction, detected earlier
					if ( ! Construct( Surface( VentSlab( Item ).SurfacePtr( SurfNum ) ).Construction ).SourceSinkPresent ) {
						ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid surface=\"" + Surface( VentSlab( Item ).SurfacePtr( SurfNum ) ).Name + "\"." );
						ShowContinueError( "Surface Construction does not have a source/sink, Construction name= \"" + Construct( Surface( VentSlab( Item ).SurfacePtr( SurfNum ) ).Construction ).Name + "\"." );
						ErrorsFound = true;
					}
				}
			}

			VentSlab( Item ).MaxAirVolFlow = rNumericArgs( 1 );

			// Outside air information:
			VentSlab( Item ).MinOutAirVolFlow = rNumericArgs( 2 );
			VentSlab( Item ).OutAirVolFlow = rNumericArgs( 3 );

			{ auto const SELECT_CASE_var( cAlphaArgs( 5 ) );
			if ( SELECT_CASE_var == "VARIABLEPERCENT" ) {
				VentSlab( Item ).OAControlType = VariablePercent;
				VentSlab( Item ).MaxOASchedName = cAlphaArgs( 6 );
				VentSlab( Item ).MaxOASchedPtr = GetScheduleIndex( cAlphaArgs( 7 ) ); // convert schedule name to pointer
				if ( VentSlab( Item ).MaxOASchedPtr == 0 ) {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFields( 7 ) + "=\"" + cAlphaArgs( 7 ) + "\" not found." );
					ErrorsFound = true;
				} else if ( ! CheckScheduleValueMinMax( VentSlab( Item ).MaxOASchedPtr, ">=0", 0.0, "<=", 1.0 ) ) {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFields( 7 ) + "=\"" + cAlphaArgs( 7 ) + "\" values out of range [0,1]." );
					ErrorsFound = true;
				}
			} else if ( SELECT_CASE_var == "FIXEDAMOUNT" ) {
				VentSlab( Item ).OAControlType = FixedOAControl;
				VentSlab( Item ).MaxOASchedName = cAlphaArgs( 7 );
				VentSlab( Item ).MaxOASchedPtr = GetScheduleIndex( cAlphaArgs( 7 ) ); // convert schedule name to pointer
				if ( VentSlab( Item ).MaxOASchedPtr == 0 ) {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFields( 7 ) + "=\"" + cAlphaArgs( 7 ) + "\" not found." );
					ErrorsFound = true;
				} else if ( ! CheckScheduleValueMinMax( VentSlab( Item ).MaxOASchedPtr, ">=0", 0.0 ) ) {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFields( 7 ) + "=\"" + cAlphaArgs( 7 ) + "\" values out of range (must be >=0)." );
					ErrorsFound = true;
				}
			} else if ( SELECT_CASE_var == "FIXEDTEMPERATURE" ) {
				VentSlab( Item ).OAControlType = FixedTemperature;
				VentSlab( Item ).TempSchedName = cAlphaArgs( 7 );
				VentSlab( Item ).TempSchedPtr = GetScheduleIndex( cAlphaArgs( 7 ) ); // convert schedule name to pointer
				if ( VentSlab( Item ).TempSchedPtr == 0 ) {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFields( 7 ) + "=\"" + cAlphaArgs( 7 ) + "\" not found." );
					ErrorsFound = true;
				}
			} else {
				ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFields( 5 ) + "=\"" + cAlphaArgs( 5 ) + "\"." );
			}}

			VentSlab( Item ).MinOASchedName = cAlphaArgs( 6 );
			VentSlab( Item ).MinOASchedPtr = GetScheduleIndex( cAlphaArgs( 6 ) ); // convert schedule name to pointer
			if ( VentSlab( Item ).MinOASchedPtr == 0 ) {
				ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFields( 6 ) + "=\"" + cAlphaArgs( 6 ) + "\" not found." );
				ErrorsFound = true;
			}

			// System Configuration:
			if ( SameString( cAlphaArgs( 8 ), "SlabOnly" ) ) {
				VentSlab( Item ).SysConfg = SlabOnly;
			} else if ( SameString( cAlphaArgs( 8 ), "SlabAndZone" ) ) {
				VentSlab( Item ).SysConfg = SlabAndZone;
			} else if ( SameString( cAlphaArgs( 8 ), "SeriesSlabs" ) ) {
				VentSlab( Item ).SysConfg = SeriesSlabs;
			} else {
				ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFields( 8 ) + "=\"" + cAlphaArgs( 8 ) + "\"." );
				ShowContinueError( "Control reset to SLAB ONLY Configuration." );
				VentSlab( Item ).SysConfg = SlabOnly;
			}

			// Hollow Core information :
			VentSlab( Item ).CoreDiameter = rNumericArgs( 4 );
			VentSlab( Item ).CoreLength = rNumericArgs( 5 );
			VentSlab( Item ).CoreNumbers = rNumericArgs( 6 );

			if ( SameString( cAlphaArgs( 8 ), "SurfaceListNames" ) ) {
				if ( ! lNumericBlanks( 4 ) ) {
					ShowWarningError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"  Core Diameter is not needed for the series slabs configuration- ignored." );
					ShowContinueError( "...It has been asigned on SlabGroup." );
				}
			}

			if ( SameString( cAlphaArgs( 8 ), "SurfaceListNames" ) ) {
				if ( ! lNumericBlanks( 5 ) ) {
					ShowWarningError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"  Core Length is not needed for the series slabs configuration- ignored." );
					ShowContinueError( "...It has been asigned on SlabGroup." );
				}
			}

			if ( SameString( cAlphaArgs( 8 ), "SurfaceListNames" ) ) {
				if ( ! lNumericBlanks( 6 ) ) {
					ShowWarningError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"  Core Numbers is not needed for the series slabs configuration- ignored." );
					ShowContinueError( "...It has been asigned on SlabGroup." );
				}
			}

			// Process the temperature control type
			if ( SameString( cAlphaArgs( 9 ), OutsideAirDryBulbTemperature ) ) {
				VentSlab( Item ).ControlType = ODBControl;
			} else if ( SameString( cAlphaArgs( 9 ), OutsideAirWetBulbTemperature ) ) {
				VentSlab( Item ).ControlType = OWBControl;
			} else if ( SameString( cAlphaArgs( 9 ), OperativeTemperature ) ) {
				VentSlab( Item ).ControlType = OPTControl;
			} else if ( SameString( cAlphaArgs( 9 ), MeanAirTemperature ) ) {
				VentSlab( Item ).ControlType = MATControl;
			} else if ( SameString( cAlphaArgs( 9 ), MeanRadiantTemperature ) ) {
				VentSlab( Item ).ControlType = MRTControl;
			} else if ( SameString( cAlphaArgs( 9 ), SlabSurfaceTemperature ) ) {
				VentSlab( Item ).ControlType = SURControl;
			} else if ( SameString( cAlphaArgs( 9 ), SlabSurfaceDewPointTemperature ) ) {
				VentSlab( Item ).ControlType = DPTZControl;
			} else {
				ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFields( 9 ) + "=\"" + cAlphaArgs( 9 ) + "\"." );
				ShowContinueError( "Control reset to ODB control." );
				VentSlab( Item ).ControlType = ODBControl;
			}

			// Heating User Input Data For Ventilated Slab Control :

			// High Air Temp :
			VentSlab( Item ).HotAirHiTempSched = cAlphaArgs( 10 );
			VentSlab( Item ).HotAirHiTempSchedPtr = GetScheduleIndex( cAlphaArgs( 10 ) );
			if ( ( VentSlab( Item ).HotAirHiTempSchedPtr == 0 ) && ( ! lAlphaBlanks( 10 ) ) ) {
				ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFields( 10 ) + "=\"" + cAlphaArgs( 10 ) + "\" not found." );
				ErrorsFound = true;
			}

			// Low Air Temp :

			VentSlab( Item ).HotAirLoTempSched = cAlphaArgs( 11 );
			VentSlab( Item ).HotAirLoTempSchedPtr = GetScheduleIndex( cAlphaArgs( 11 ) );
			if ( ( VentSlab( Item ).HotAirLoTempSchedPtr == 0 ) && ( ! lAlphaBlanks( 11 ) ) ) {
				ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFields( 11 ) + "=\"" + cAlphaArgs( 11 ) + "\" not found." );
				ErrorsFound = true;
			}

			VentSlab( Item ).HotCtrlHiTempSched = cAlphaArgs( 12 );
			VentSlab( Item ).HotCtrlHiTempSchedPtr = GetScheduleIndex( cAlphaArgs( 12 ) );
			if ( ( VentSlab( Item ).HotCtrlHiTempSchedPtr == 0 ) && ( ! lAlphaBlanks( 12 ) ) ) {
				ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFields( 12 ) + "=\"" + cAlphaArgs( 12 ) + "\" not found." );
				ErrorsFound = true;
			}

			VentSlab( Item ).HotCtrlLoTempSched = cAlphaArgs( 13 );
			VentSlab( Item ).HotCtrlLoTempSchedPtr = GetScheduleIndex( cAlphaArgs( 13 ) );
			if ( ( VentSlab( Item ).HotCtrlLoTempSchedPtr == 0 ) && ( ! lAlphaBlanks( 13 ) ) ) {
				ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFields( 13 ) + "=\"" + cAlphaArgs( 13 ) + "\" not found." );
				ErrorsFound = true;
			}

			// Cooling User Input Data For Ventilated Slab Control :
			// Cooling High Temp Sch.
			VentSlab( Item ).ColdAirHiTempSched = cAlphaArgs( 13 );
			VentSlab( Item ).ColdAirHiTempSchedPtr = GetScheduleIndex( cAlphaArgs( 14 ) );
			if ( ( VentSlab( Item ).ColdAirHiTempSchedPtr == 0 ) && ( ! lAlphaBlanks( 14 ) ) ) {
				ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFields( 14 ) + "=\"" + cAlphaArgs( 14 ) + "\" not found." );
				ErrorsFound = true;
			}

			// Cooling Low Temp Sch.

			VentSlab( Item ).ColdAirLoTempSched = cAlphaArgs( 15 );
			VentSlab( Item ).ColdAirLoTempSchedPtr = GetScheduleIndex( cAlphaArgs( 15 ) );
			if ( ( VentSlab( Item ).ColdAirLoTempSchedPtr == 0 ) && ( ! lAlphaBlanks( 15 ) ) ) {
				ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFields( 15 ) + "=\"" + cAlphaArgs( 15 ) + "\" not found." );
				ErrorsFound = true;
			}

			// Cooling Control High Sch.

			VentSlab( Item ).ColdCtrlHiTempSched = cAlphaArgs( 16 );
			VentSlab( Item ).ColdCtrlHiTempSchedPtr = GetScheduleIndex( cAlphaArgs( 16 ) );
			if ( ( VentSlab( Item ).ColdCtrlHiTempSchedPtr == 0 ) && ( ! lAlphaBlanks( 16 ) ) ) {
				ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFields( 16 ) + "=\"" + cAlphaArgs( 16 ) + "\" not found." );
				ErrorsFound = true;
			}

			// Cooling Control Low Sch.

			VentSlab( Item ).ColdCtrlLoTempSched = cAlphaArgs( 17 );
			VentSlab( Item ).ColdCtrlLoTempSchedPtr = GetScheduleIndex( cAlphaArgs( 17 ) );
			if ( ( VentSlab( Item ).ColdCtrlLoTempSchedPtr == 0 ) && ( ! lAlphaBlanks( 17 ) ) ) {
				ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFields( 17 ) + "=\"" + cAlphaArgs( 17 ) + "\" not found." );
				ErrorsFound = true;
			}

			// Main air nodes (except outside air node):
			// Refer the Unit Ventilator Air Node note

			// MJW CR7903 - Ventilated slab was not drawing properly in HVAC Diagram svg output
			//  This object is structured differently from other zone equipment in that it functions
			//  as both a parent and non-parent, and it has an implicit OA mixer.  This makes it difficult
			//  to register the nodes in a way that HVAC Diagram can understand and in a way that satisfies
			//  node connection tests.  Here's an explanation of the changes made for this CR:
			//      In general, nodes associated with the ventilated slab system (the overall parent object)
			//         are registered with "-SYSTEM" appended to the object type and object name
			//         This same suffix is also added later when SetUpCompSets is called, for the same reason
			//      In general, nodes associated with the implicit OA mixer object
			//         are registered with "-OA MIXER" appended to the object type and object name
			//      %ReturnAirNode is one inlet to the implicit oa mixer
			//         For SlabOnly and SeriesSlab this node does nothing,
			//             so NodeConnectionType_Internal,ObjectIsNotParent, -OA MIXER
			//         For SlabandZone, this node extracts air from the zone,
			//             so NodeConnectionType_Inlet,ObjectIsNotParent, -OA MIXER
			//         For SlabandZone, this node is also used to associate the whole system with a pair of zone inlet/exhaust nodes,
			//             so it is registered again as NodeConnectionType_Inlet,1,ObjectIsParent, -SYSTEM
			//      %RadInNode is the ultimate air inlet to the slab or series of slabs
			//         For all types of ventilated slab, this is NodeConnectionType_Inlet,ObjectIsNotParent
			//      %OAMixerOutNode is the outlet from the implicit OA mixer
			//         For all types of ventilated slab, this is NodeConnectionType_Outlet,ObjectIsNotParent
			//      %FanOutletNode is the outlet from the explicit fan child object (redundant input, should mine from child)
			//         For all types of ventilated slab, this is NodeConnectionType_Internal,ObjectIsParent
			//      %ZoneAirInNode is applicable only to SlabandZone configuration. It is the node that flows into the zone,
			//         and it is also the outlet from the ventilated slab section, so it must be registered twice
			//         First for the overall system, NodeConnectionType_Outlet,ObjectIsParent, -SYSTEM
			//         Second as the slab outlet, NodeConnectionType_Outlet,ObjectIsNotParent
			//      %OutsideAirNode is the outdoor air inlet to the OA mixer
			//         For all types of ventilated slab, this is NodeConnectionType_Inlet,ObjectIsNotParent, -OA MIXER

			if ( VentSlab( Item ).SysConfg == SlabOnly ) {

				VentSlab( Item ).ReturnAirNode = GetOnlySingleNode( cAlphaArgs( 18 ), ErrorsFound, CurrentModuleObject + "-OA MIXER", cAlphaArgs( 1 ) + "-OA MIXER", NodeType_Air, NodeConnectionType_Internal, 1, ObjectIsNotParent );
				VentSlab( Item ).RadInNode = GetOnlySingleNode( cAlphaArgs( 19 ), ErrorsFound, CurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

				VentSlab( Item ).OAMixerOutNode = GetOnlySingleNode( cAlphaArgs( 23 ), ErrorsFound, CurrentModuleObject + "-OA MIXER", cAlphaArgs( 1 ) + "-OA MIXER", NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
				VentSlab( Item ).FanOutletNode = GetOnlySingleNode( cAlphaArgs( 24 ), ErrorsFound, CurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Internal, 1, ObjectIsParent );

			} else if ( VentSlab( Item ).SysConfg == SeriesSlabs ) {

				VentSlab( Item ).ReturnAirNode = GetOnlySingleNode( cAlphaArgs( 18 ), ErrorsFound, CurrentModuleObject + "-OA MIXER", cAlphaArgs( 1 ) + "-OA MIXER", NodeType_Air, NodeConnectionType_Internal, 1, ObjectIsNotParent );
				VentSlab( Item ).RadInNode = GetOnlySingleNode( cAlphaArgs( 19 ), ErrorsFound, CurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

				VentSlab( Item ).OAMixerOutNode = GetOnlySingleNode( cAlphaArgs( 23 ), ErrorsFound, CurrentModuleObject + "-OA MIXER", cAlphaArgs( 1 ) + "-OA MIXER", NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
				VentSlab( Item ).FanOutletNode = GetOnlySingleNode( cAlphaArgs( 24 ), ErrorsFound, CurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Internal, 1, ObjectIsParent );

			} else if ( VentSlab( Item ).SysConfg == SlabAndZone ) {

				VentSlab( Item ).ReturnAirNode = GetOnlySingleNode( cAlphaArgs( 18 ), ErrorsFound, CurrentModuleObject + "-SYSTEM", cAlphaArgs( 1 ) + "-SYSTEM", NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsParent );
				VentSlab( Item ).ReturnAirNode = GetOnlySingleNode( cAlphaArgs( 18 ), ErrorsFound, CurrentModuleObject + "-OA MIXER", cAlphaArgs( 1 ) + "-OA MIXER", NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
				VentSlab( Item ).RadInNode = GetOnlySingleNode( cAlphaArgs( 19 ), ErrorsFound, CurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
				VentSlab( Item ).OAMixerOutNode = GetOnlySingleNode( cAlphaArgs( 23 ), ErrorsFound, CurrentModuleObject + "-OA MIXER", cAlphaArgs( 1 ) + "-OA MIXER", NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
				VentSlab( Item ).FanOutletNode = GetOnlySingleNode( cAlphaArgs( 24 ), ErrorsFound, CurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Internal, 1, ObjectIsParent );
			}

			if ( VentSlab( Item ).SysConfg == SlabOnly ) {
				if ( ! lAlphaBlanks( 20 ) ) {
					ShowWarningError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" " + cAlphaFields( 20 ) + "=\"" + cAlphaArgs( 20 ) + "\" not needed - ignored." );
					ShowContinueError( "It is used for \"SlabAndZone\" only" );
				}

			} else if ( VentSlab( Item ).SysConfg == SlabAndZone ) {
				if ( lAlphaBlanks( 20 ) ) {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFields( 20 ) + " is blank and must be entered." );
					ErrorsFound = true;
				}

				VentSlab( Item ).ZoneAirInNode = GetOnlySingleNode( cAlphaArgs( 20 ), ErrorsFound, CurrentModuleObject + "-SYSTEM", cAlphaArgs( 1 ) + "-SYSTEM", NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsParent );

				VentSlab( Item ).ZoneAirInNode = GetOnlySingleNode( cAlphaArgs( 20 ), ErrorsFound, CurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			}

			//  Set connection type to 'Inlet', because it now uses an OA node
			VentSlab( Item ).OutsideAirNode = GetOnlySingleNode( cAlphaArgs( 21 ), ErrorsFound, CurrentModuleObject + "-OA MIXER", cAlphaArgs( 1 ) + "-OA MIXER", NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

			if ( ! lAlphaBlanks( 21 ) ) {
				CheckAndAddAirNodeNumber( VentSlab( Item ).OutsideAirNode, IsValid );
				if ( ! IsValid ) {
					ShowWarningError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", Adding OutdoorAir:Node=" + cAlphaArgs( 21 ) );
				}

			}

			VentSlab( Item ).AirReliefNode = GetOnlySingleNode( cAlphaArgs( 22 ), ErrorsFound, CurrentModuleObject + "-OA MIXER", cAlphaArgs( 1 ) + "-OA MIXER", NodeType_Air, NodeConnectionType_ReliefAir, 1, ObjectIsNotParent );

			// Fan information:
			VentSlab( Item ).FanName = cAlphaArgs( 25 );

			if ( VentSlab( Item ).OAControlType == FixedOAControl ) {
				VentSlab( Item ).OutAirVolFlow = VentSlab( Item ).MinOutAirVolFlow;
				VentSlab( Item ).MaxOASchedName = VentSlab( Item ).MinOASchedName;
				VentSlab( Item ).MaxOASchedPtr = GetScheduleIndex( VentSlab( Item ).MinOASchedName );
			}

			// Add fan to component sets array
			SetUpCompSets( CurrentModuleObject + "-SYSTEM", VentSlab( Item ).Name + "-SYSTEM", "UNDEFINED", cAlphaArgs( 25 ), cAlphaArgs( 23 ), cAlphaArgs( 24 ) );

			// Coil options assign

			{ auto const SELECT_CASE_var( cAlphaArgs( 26 ) );
			if ( SELECT_CASE_var == "HEATINGANDCOOLING" ) {
				VentSlab( Item ).CoilOption = BothOption;
			} else if ( SELECT_CASE_var == "HEATING" ) {
				VentSlab( Item ).CoilOption = HeatingOption;
			} else if ( SELECT_CASE_var == "COOLING" ) {
				VentSlab( Item ).CoilOption = CoolingOption;
			} else if ( SELECT_CASE_var == "NONE" ) {
				VentSlab( Item ).CoilOption = NoneOption;
			} else {
				ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFields( 26 ) + "=\"" + cAlphaArgs( 26 ) + "\"." );
				ErrorsFound = true;
			}}

			if ( VentSlab( Item ).CoilOption == BothOption || VentSlab( Item ).CoilOption == HeatingOption ) {
				// Heating coil information:
				//        A27, \field Heating Coil Object Type
				//             \type choice
				//             \key Coil:Heating:Water
				//             \key Coil:Heating:Electric
				//             \key Coil:Heating:Gas
				//             \key Coil:Heating:Steam
				//        A28, \field Heating Coil Name
				//             \type object-list
				//             \object-list HeatingCoilName

				// Heating coil information:
				if ( ! lAlphaBlanks( 28 ) ) {
					VentSlab( Item ).HCoilPresent = true;
					VentSlab( Item ).HCoilTypeCh = cAlphaArgs( 27 );
					errFlag = false;

					{ auto const SELECT_CASE_var( cAlphaArgs( 27 ) );
					if ( SELECT_CASE_var == "COIL:HEATING:WATER" ) {
						VentSlab( Item ).HCoilType = Heating_WaterCoilType;
						VentSlab( Item ).HCoil_PlantTypeNum = TypeOf_CoilWaterSimpleHeating;
					} else if ( SELECT_CASE_var == "COIL:HEATING:STEAM" ) {
						VentSlab( Item ).HCoilType = Heating_SteamCoilType;
						VentSlab( Item ).HCoil_PlantTypeNum = TypeOf_CoilSteamAirHeating;
						VentSlab( Item ).HCoil_FluidIndex = FindRefrigerant( "Steam" );
						if ( VentSlab( Item ).HCoil_FluidIndex == 0 ) {
							ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "Steam Properties not found." );
							if ( SteamMessageNeeded ) ShowContinueError( "Steam Fluid Properties should have been included in the input file." );
							ErrorsFound = true;
							SteamMessageNeeded = false;
						}
					} else if ( SELECT_CASE_var == "COIL:HEATING:ELECTRIC" ) {
						VentSlab( Item ).HCoilType = Heating_ElectricCoilType;
					} else if ( SELECT_CASE_var == "COIL:HEATING:GAS" ) {
						VentSlab( Item ).HCoilType = Heating_GasCoilType;
					} else {
						ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFields( 27 ) + "=\"" + cAlphaArgs( 27 ) + "\"." );
						ErrorsFound = true;
						errFlag = true;
					}}
					if ( ! errFlag ) {
						VentSlab( Item ).HCoilName = cAlphaArgs( 28 );
						ValidateComponent( cAlphaArgs( 27 ), VentSlab( Item ).HCoilName, IsNotOK, CurrentModuleObject );
						if ( IsNotOK ) {
							ShowContinueError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFields( 28 ) + "=\"" + cAlphaArgs( 28 ) + "\"." );
							ShowContinueError( "... not valid for " + cAlphaFields( 27 ) + "=\"" + cAlphaArgs( 27 ) + "\"." );
							ErrorsFound = true;
						}
					}

					VentSlab( Item ).MinVolHotWaterFlow = 0.0;
					VentSlab( Item ).MinVolHotSteamFlow = 0.0;

					// The heating coil control node is necessary for a hot water coil, but not necessary for an
					// electric or gas coil.
					if ( VentSlab( Item ).HCoilType == Heating_GasCoilType || VentSlab( Item ).HCoilType == Heating_ElectricCoilType ) {
						if ( ! lAlphaBlanks( 29 ) ) {
							ShowWarningError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" " + cAlphaFields( 29 ) + "=\"" + cAlphaArgs( 29 ) + "\" not needed - ignored." );
							ShowContinueError( "..It is used for hot water coils only." );
						}
					} else {
						if ( lAlphaBlanks( 29 ) ) {
							ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFields( 29 ) + " is blank and must be entered." );
							ErrorsFound = true;
						}
						VentSlab( Item ).HotControlNode = GetOnlySingleNode( cAlphaArgs( 29 ), ErrorsFound, CurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Actuator, 1, ObjectIsParent );
					}
					VentSlab( Item ).HotControlOffset = 0.001;

					if ( VentSlab( Item ).HCoilType == Heating_WaterCoilType ) {
						VentSlab( Item ).MaxVolHotWaterFlow = GetWaterCoilMaxFlowRate( "Coil:Heating:Water", VentSlab( Item ).HCoilName, ErrorsFound );
						VentSlab( Item ).MaxVolHotSteamFlow = GetWaterCoilMaxFlowRate( "Coil:Heating:Water", VentSlab( Item ).HCoilName, ErrorsFound );
					} else if ( VentSlab( Item ).HCoilType == Heating_SteamCoilType ) {
						VentSlab( Item ).MaxVolHotWaterFlow = GetSteamCoilMaxFlowRate( "Coil:Heating:Steam", VentSlab( Item ).HCoilName, ErrorsFound );
						VentSlab( Item ).MaxVolHotSteamFlow = GetSteamCoilMaxFlowRate( "Coil:Heating:Steam", VentSlab( Item ).HCoilName, ErrorsFound );
					}

				} else { // no heating coil
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" missing heating coil." );
					ShowContinueError( "a heating coil is required for " + cAlphaFields( 26 ) + "=\"" + cAlphaArgs( 26 ) + "\"." );
					ErrorsFound = true;
				}
			}

			if ( VentSlab( Item ).CoilOption == BothOption || VentSlab( Item ).CoilOption == CoolingOption ) {
				// Cooling coil information (if one is present):
				//        A30, \field Cooling Coil Object Type
				//             \type choice
				//             \key Coil:Cooling:Water
				//             \key Coil:Cooling:Water:DetailedGeometry
				//             \key CoilSystem:Cooling:Water:HeatExchangerAssisted
				//        A31, \field Cooling Coil Name
				//             \type object-list
				//             \object-list CoolingCoilsWater
				// Cooling coil information (if one is present):
				if ( ! lAlphaBlanks( 31 ) ) {
					VentSlab( Item ).CCoilPresent = true;
					VentSlab( Item ).CCoilTypeCh = cAlphaArgs( 30 );
					errFlag = false;

					{ auto const SELECT_CASE_var( cAlphaArgs( 30 ) );
					if ( SELECT_CASE_var == "COIL:COOLING:WATER" ) {
						VentSlab( Item ).CCoilType = Cooling_CoilWaterCooling;
						VentSlab( Item ).CCoil_PlantTypeNum = TypeOf_CoilWaterCooling;
						VentSlab( Item ).CCoilPlantName = cAlphaArgs( 31 );
					} else if ( SELECT_CASE_var == "COIL:COOLING:WATER:DETAILEDGEOMETRY" ) {
						VentSlab( Item ).CCoilType = Cooling_CoilDetailedCooling;
						VentSlab( Item ).CCoil_PlantTypeNum = TypeOf_CoilWaterDetailedFlatCooling;
						VentSlab( Item ).CCoilPlantName = cAlphaArgs( 31 );
					} else if ( SELECT_CASE_var == "COILSYSTEM:COOLING:WATER:HEATEXCHANGERASSISTED" ) {
						VentSlab( Item ).CCoilType = Cooling_CoilHXAssisted;
						GetHXCoilTypeAndName( cAlphaArgs( 30 ), cAlphaArgs( 31 ), ErrorsFound, VentSlab( Item ).CCoilPlantType, VentSlab( Item ).CCoilPlantName );
						if ( SameString( VentSlab( Item ).CCoilPlantType, "Coil:Cooling:Water" ) ) {
							VentSlab( Item ).CCoil_PlantTypeNum = TypeOf_CoilWaterCooling;
						} else if ( SameString( VentSlab( Item ).CCoilPlantType, "Coil:Cooling:Water:DetailedGeometry" ) ) {
							VentSlab( Item ).CCoil_PlantTypeNum = TypeOf_CoilWaterDetailedFlatCooling;
						} else {
							ShowSevereError( "GetVentilatedSlabInput: " + CurrentModuleObject + "=\"" + VentSlab( Item ).Name + "\", invalid" );
							ShowContinueError( "For: " + cAlphaFields( 30 ) + "=\"" + cAlphaArgs( 30 ) + "\"." );
							ShowContinueError( "Invalid Coil Type=" + VentSlab( Item ).CCoilPlantType + ", Name=" + VentSlab( Item ).CCoilPlantName );
							ShowContinueError( "must be \"Coil:Cooling:Water\" or \"Coil:Cooling:Water:DetailedGeometry\"" );
							ErrorsFound = true;
						}
					} else {
						ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFields( 29 ) + "=\"" + cAlphaArgs( 29 ) + "\"." );
						ErrorsFound = true;
						errFlag = true;
					}}

					if ( ! errFlag ) {
						VentSlab( Item ).CCoilName = cAlphaArgs( 31 );
						ValidateComponent( cAlphaArgs( 30 ), VentSlab( Item ).CCoilName, IsNotOK, "ZoneHVAC:VentilatedSlab " );
						if ( IsNotOK ) {
							ShowContinueError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFields( 31 ) + "=\"" + cAlphaArgs( 31 ) + "\"." );
							ShowContinueError( "... not valid for " + cAlphaFields( 30 ) + "=\"" + cAlphaArgs( 30 ) + "\"." );
							ErrorsFound = true;
						}
					}

					VentSlab( Item ).MinVolColdWaterFlow = 0.0;

					VentSlab( Item ).ColdControlNode = GetOnlySingleNode( cAlphaArgs( 32 ), ErrorsFound, CurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Actuator, 1, ObjectIsParent );

					if ( lAlphaBlanks( 32 ) ) {
						ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFields( 32 ) + " is blank and must be entered." );
						ErrorsFound = true;
					}

					VentSlab( Item ).ColdControlOffset = 0.001;

					if ( VentSlab( Item ).CCoilType == Cooling_CoilWaterCooling ) {
						VentSlab( Item ).MaxVolColdWaterFlow = GetWaterCoilMaxFlowRate( "Coil:Cooling:Water", VentSlab( Item ).CCoilName, ErrorsFound );
					} else if ( VentSlab( Item ).CCoilType == Cooling_CoilDetailedCooling ) {
						VentSlab( Item ).MaxVolColdWaterFlow = GetWaterCoilMaxFlowRate( "Coil:Cooling:Water:DetailedGeometry", VentSlab( Item ).CCoilName, ErrorsFound );
					} else if ( VentSlab( Item ).CCoilType == Cooling_CoilHXAssisted ) {
						VentSlab( Item ).MaxVolColdWaterFlow = GetHXAssistedCoilFlowRate( "CoilSystem:Cooling:Water:HeatExchangerAssisted", VentSlab( Item ).CCoilName, ErrorsFound );
					}

				} else { // No Cooling Coil
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" missing cooling coil." );
					ShowContinueError( "a cooling coil is required for " + cAlphaFields( 26 ) + "=\"" + cAlphaArgs( 26 ) + "\"." );
					ErrorsFound = true;
				}
			}
			if ( ! lAlphaBlanks( 33 ) ) {
				VentSlab( Item ).AvailManagerListName = cAlphaArgs( 33 );
			}

			VentSlab( Item ).HVACSizingIndex = 0;
			if ( ! lAlphaBlanks( 34 )) {
				VentSlab( Item ).HVACSizingIndex = FindItemInList( cAlphaArgs( 34 ), ZoneHVACSizing );
				if (VentSlab( Item ).HVACSizingIndex == 0) {
					ShowSevereError( cAlphaFields( 34 ) + " = " + cAlphaArgs( 34 ) + " not found." );
					ShowContinueError( "Occurs in " + cMO_VentilatedSlab + " = " + VentSlab( Item ).Name );
					ErrorsFound = true;
				}
			}

			{ auto const SELECT_CASE_var( VentSlab( Item ).CoilOption );
			if ( SELECT_CASE_var == BothOption ) { // 'HeatingAndCooling'
				// Add cooling coil to component sets array when present
				SetUpCompSets( CurrentModuleObject + "-SYSTEM", VentSlab( Item ).Name + "-SYSTEM", cAlphaArgs( 30 ), cAlphaArgs( 31 ), cAlphaArgs( 24 ), "UNDEFINED" );

				// Add heating coil to component sets array when cooling coil present
				SetUpCompSets( CurrentModuleObject + "-SYSTEM", VentSlab( Item ).Name + "-SYSTEM", cAlphaArgs( 27 ), cAlphaArgs( 28 ), "UNDEFINED", cAlphaArgs( 19 ) );

			} else if ( SELECT_CASE_var == HeatingOption ) { // 'Heating'
				// Add heating coil to component sets array when no cooling coil present
				SetUpCompSets( CurrentModuleObject + "-SYSTEM", VentSlab( Item ).Name + "-SYSTEM", cAlphaArgs( 27 ), cAlphaArgs( 28 ), cAlphaArgs( 24 ), cAlphaArgs( 19 ) );

			} else if ( SELECT_CASE_var == CoolingOption ) { // 'Cooling'
				// Add cooling coil to component sets array when no heating coil present
				SetUpCompSets( CurrentModuleObject + "-SYSTEM", VentSlab( Item ).Name + "-SYSTEM", cAlphaArgs( 30 ), cAlphaArgs( 31 ), cAlphaArgs( 24 ), cAlphaArgs( 19 ) );

			} else if ( SELECT_CASE_var == NoneOption ) {

			} else {

			}}

		} // ...loop over all of the ventilated slab found in the input file

		cAlphaArgs.deallocate();
		cAlphaFields.deallocate();
		cNumericFields.deallocate();
		rNumericArgs.deallocate();
		lAlphaBlanks.deallocate();
		lNumericBlanks.deallocate();

		if ( ErrorsFound ) ShowFatalError( CurrentModuleObject + " errors occurred in input.  Program terminates." );

		// Setup Report variables for the VENTILATED SLAB
		for ( Item = 1; Item <= NumOfVentSlabs; ++Item ) {
			//   CALL SetupOutputVariable('Ventilated Slab Direct Heat Loss Rate [W]', &
			//                             VentSlab(Item)%DirectHeatLossRate,'System', &
			//                             'Average', VentSlab(Item)%Name)
			//   CALL SetupOutputVariable('Ventilated Slab Direct Heat Loss [W]',        &
			//                             VentSlab(Item)%DirectHeatLoss,'System', &
			//                             'Sum', VentSlab(Item)%Name)
			//   CALL SetupOutputVariable('Ventilated Slab Direct Heat Gain Rate [W]',        &
			//                             VentSlab(Item)%DirectHeatGainRate,'System', &
			//                            'Average', VentSlab(Item)%Name)
			//   CALL SetupOutputVariable('Ventilated Slab Direct Heat Gain [J]',        &
			//                           VentSlab(Item)%DirectHeatGain,'System', &
			//                             'Sum', VentSlab(Item)%Name)
			SetupOutputVariable( "Zone Ventilated Slab Radiant Heating Rate [W]", VentSlab( Item ).RadHeatingPower, "System", "Average", VentSlab( Item ).Name );
			SetupOutputVariable( "Zone Ventilated Slab Radiant Heating Energy [J]", VentSlab( Item ).RadHeatingEnergy, "System", "Sum", VentSlab( Item ).Name );
			SetupOutputVariable( "Zone Ventilated Slab Radiant Cooling Rate [W]", VentSlab( Item ).RadCoolingPower, "System", "Average", VentSlab( Item ).Name );
			SetupOutputVariable( "Zone Ventilated Slab Radiant Cooling Energy [J]", VentSlab( Item ).RadCoolingEnergy, "System", "Sum", VentSlab( Item ).Name );
			SetupOutputVariable( "Zone Ventilated Slab Coil Heating Rate [W]", VentSlab( Item ).HeatCoilPower, "System", "Average", VentSlab( Item ).Name );
			SetupOutputVariable( "Zone Ventilated Slab Coil Heating Energy [J]", VentSlab( Item ).HeatCoilEnergy, "System", "Sum", VentSlab( Item ).Name );
			SetupOutputVariable( "Zone Ventilated Slab Coil Total Cooling Rate [W]", VentSlab( Item ).TotCoolCoilPower, "System", "Average", VentSlab( Item ).Name );
			SetupOutputVariable( "Zone Ventilated Slab Coil Total Cooling Energy [J]", VentSlab( Item ).TotCoolCoilEnergy, "System", "Sum", VentSlab( Item ).Name );
			SetupOutputVariable( "Zone Ventilated Slab Coil Sensible Cooling Rate [W]", VentSlab( Item ).SensCoolCoilPower, "System", "Average", VentSlab( Item ).Name );
			SetupOutputVariable( "Zone Ventilated Slab Coil Sensible Cooling Energy [J]", VentSlab( Item ).SensCoolCoilEnergy, "System", "Sum", VentSlab( Item ).Name );
			SetupOutputVariable( "Zone Ventilated Slab Coil Latent Cooling Rate [W]", VentSlab( Item ).LateCoolCoilPower, "System", "Average", VentSlab( Item ).Name );
			SetupOutputVariable( "Zone Ventilated Slab Coil Latent Cooling Energy [J]", VentSlab( Item ).LateCoolCoilEnergy, "System", "Sum", VentSlab( Item ).Name );
			SetupOutputVariable( "Zone Ventilated Slab Air Mass Flow Rate [kg/s]", VentSlab( Item ).AirMassFlowRate, "System", "Average", VentSlab( Item ).Name );
			SetupOutputVariable( "Zone Ventilated Slab Fan Electric Power [W]", VentSlab( Item ).ElecFanPower, "System", "Average", VentSlab( Item ).Name );
			//! Note that the ventilated slab fan electric is NOT metered because this value is already metered through the fan component
			SetupOutputVariable( "Zone Ventilated Slab Fan Electric Energy [J]", VentSlab( Item ).ElecFanEnergy, "System", "Sum", VentSlab( Item ).Name );
			SetupOutputVariable( "Zone Ventilated Slab Inlet Air Temperature [C]", VentSlab( Item ).SlabInTemp, "System", "Average", VentSlab( Item ).Name );
			SetupOutputVariable( "Zone Ventilated Slab Outlet Air Temperature [C]", VentSlab( Item ).SlabOutTemp, "System", "Average", VentSlab( Item ).Name );
			SetupOutputVariable( "Zone Ventilated Slab Zone Inlet Air Temperature [C]", VentSlab( Item ).ZoneInletTemp, "System", "Average", VentSlab( Item ).Name );
			SetupOutputVariable( "Zone Ventilated Slab Return Air Temperature [C]", VentSlab( Item ).ReturnAirTemp, "System", "Average", VentSlab( Item ).Name );
			SetupOutputVariable( "Zone Ventilated Slab Fan Outlet Air Temperature [C]", VentSlab( Item ).FanOutletTemp, "System", "Average", VentSlab( Item ).Name );
			SetupOutputVariable( "Zone Ventilated Slab Fan Availability Status []", VentSlab( Item ).AvailStatus, "System", "Average", VentSlab( Item ).Name );

		}

	}

	void
	InitVentilatedSlab(
		int const Item, // index for the current ventilated slab
		int const VentSlabZoneNum, // number of zone being served
		bool const FirstHVACIteration // TRUE if 1st HVAC simulation of system timestep
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Young Tae Chae, Rick Strand
		//       DATE WRITTEN   June 2008
		//       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine initializes all of the data elements which are necessary
		// to simulate a Ventilated Slab.

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger initializations.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataEnvironment::OutBaroPress;
		using DataEnvironment::OutHumRat;
		using DataEnvironment::StdRhoAir;
		using DataGlobals::NumOfZones;
		using DataGlobals::BeginEnvrnFlag;
		using DataGlobals::AnyPlantInModel;
		using DataLoopNode::Node;
		using ScheduleManager::GetCurrentScheduleValue;
		using DataHeatBalFanSys::MAT;
		using DataHeatBalFanSys::ZoneAirHumRat;
		using DataZoneEquipment::ZoneEquipInputsFilled;
		using DataZoneEquipment::CheckZoneEquipmentList;
		using DataZoneEquipment::VentilatedSlab_Num;
		using DataPlant::PlantLoop;
		using DataPlant::ScanPlantLoopsForObject;
		using DataPlant::TypeOf_CoilWaterSimpleHeating;
		using DataPlant::TypeOf_CoilSteamAirHeating;
		using DataPlant::TypeOf_CoilWaterCooling;
		using DataPlant::TypeOf_CoilWaterDetailedFlatCooling;
		using FluidProperties::GetDensityGlycol;
		using PlantUtilities::InitComponentNodes;
		using DataHVACGlobals::ZoneComp;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "InitVentilatedSlab" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//  REAL           :: CurrentFlowSchedule   ! Schedule value for flow fraction in a ventilated slab
		int RadNum; // Number of the radiant system (DO loop counter)
		int RadSurfNum; // Number of the radiant system surface (DO loop counter)
		int SurfNum; // Intermediate variable for keeping track of the surface number
		int ZoneNum; // Intermediate variable for keeping track of the zone number

		int AirRelNode; // relief air node number in Ventilated Slab loop
		int ColdConNode; // cold water control node number in Ventilated Slab loop
		static bool MyOneTimeFlag( true );
		static bool ZoneEquipmentListChecked( false ); // True after the Zone Equipment List has been checked for items
		static Array1D_bool MyEnvrnFlag;
		static Array1D_bool MyPlantScanFlag;
		static Array1D_bool MyZoneEqFlag; // used to set up zone equipment availability managers
		int HotConNode; // hot water control node number in Ventilated Slab loop
		int InNode; // inlet node number in Ventilated Slab loop
		int OutNode; // outlet node number in Ventilated Slab loop
		int OutsideAirNode; // outside air node number in Ventilated Slab loop
		Real64 RhoAir; // air density at InNode
		Real64 TempSteamIn;
		Real64 SteamDensity;
		int ZoneAirInNode;
		int MixOut;
		Real64 rho;
		bool errFlag;
		// FLOW:

		// Do the one time initializations

		if ( MyOneTimeFlag ) {
			MyEnvrnFlag.allocate( NumOfVentSlabs );
			MySizeFlag.allocate( NumOfVentSlabs );
			MyPlantScanFlag.allocate( NumOfVentSlabs );
			MyZoneEqFlag.allocate ( NumOfVentSlabs );
			ZeroSourceSumHATsurf.dimension( NumOfZones, 0.0 );
			QRadSysSrcAvg.dimension( TotSurfaces, 0.0 );
			LastQRadSysSrc.dimension( TotSurfaces, 0.0 );
			LastSysTimeElapsed.dimension( TotSurfaces, 0.0 );
			LastTimeStepSys.dimension( TotSurfaces, 0.0 );

			// Initialize total areas for all radiant systems
			for ( RadNum = 1; RadNum <= NumOfVentSlabs; ++RadNum ) {
				VentSlab( RadNum ).TotalSurfaceArea = 0.0;
				for ( SurfNum = 1; SurfNum <= VentSlab( RadNum ).NumOfSurfaces; ++SurfNum ) {
					VentSlab( RadNum ).TotalSurfaceArea += Surface( VentSlab( RadNum ).SurfacePtr( SurfNum ) ).Area;
				}
			}
			MyEnvrnFlag = true;
			MySizeFlag = true;
			MyPlantScanFlag = true;
			MyZoneEqFlag = true;
			MyOneTimeFlag = false;
		}

		if ( allocated( ZoneComp ) ) {
			if ( MyZoneEqFlag( Item ) ) { // initialize the name of each availability manager list and zone number
				ZoneComp( VentilatedSlab_Num ).ZoneCompAvailMgrs( Item ).AvailManagerListName = VentSlab( Item ).AvailManagerListName;
				ZoneComp( VentilatedSlab_Num ).ZoneCompAvailMgrs( Item ).ZoneNum = VentSlabZoneNum;
				MyZoneEqFlag ( Item ) = false;
			}
			VentSlab( Item ).AvailStatus = ZoneComp( VentilatedSlab_Num ).ZoneCompAvailMgrs( Item ).AvailStatus;
		}

		if ( MyPlantScanFlag( Item ) && allocated( PlantLoop ) ) {
			if ( ( VentSlab( Item ).HCoil_PlantTypeNum == TypeOf_CoilWaterSimpleHeating ) || ( VentSlab( Item ).HCoil_PlantTypeNum == TypeOf_CoilSteamAirHeating ) ) {
				errFlag = false;
				ScanPlantLoopsForObject( VentSlab( Item ).HCoilName, VentSlab( Item ).HCoil_PlantTypeNum, VentSlab( Item ).HWLoopNum, VentSlab( Item ).HWLoopSide, VentSlab( Item ).HWBranchNum, VentSlab( Item ).HWCompNum, _, _, _, _, _, errFlag );
				if ( errFlag ) {
					ShowContinueError( "Reference Unit=\"" + VentSlab( Item ).Name + "\", type=ZoneHVAC:VentilatedSlab" );
					ShowFatalError( "InitVentilatedSlab: Program terminated due to previous condition(s)." );
				}

				VentSlab( Item ).HotCoilOutNodeNum = PlantLoop( VentSlab( Item ).HWLoopNum ).LoopSide( VentSlab( Item ).HWLoopSide ).Branch( VentSlab( Item ).HWBranchNum ).Comp( VentSlab( Item ).HWCompNum ).NodeNumOut;

			}
			if ( ( VentSlab( Item ).CCoil_PlantTypeNum == TypeOf_CoilWaterCooling ) || ( VentSlab( Item ).CCoil_PlantTypeNum == TypeOf_CoilWaterDetailedFlatCooling ) ) {
				errFlag = false;
				ScanPlantLoopsForObject( VentSlab( Item ).CCoilPlantName, VentSlab( Item ).CCoil_PlantTypeNum, VentSlab( Item ).CWLoopNum, VentSlab( Item ).CWLoopSide, VentSlab( Item ).CWBranchNum, VentSlab( Item ).CWCompNum );
				if ( errFlag ) {
					ShowContinueError( "Reference Unit=\"" + VentSlab( Item ).Name + "\", type=ZoneHVAC:VentilatedSlab" );
					ShowFatalError( "InitVentilatedSlab: Program terminated due to previous condition(s)." );
				}
				VentSlab( Item ).ColdCoilOutNodeNum = PlantLoop( VentSlab( Item ).CWLoopNum ).LoopSide( VentSlab( Item ).CWLoopSide ).Branch( VentSlab( Item ).CWBranchNum ).Comp( VentSlab( Item ).CWCompNum ).NodeNumOut;
			} else {
				if ( VentSlab( Item ).CCoilPresent ) ShowFatalError( "InitVentilatedSlab: Unit=" + VentSlab( Item ).Name + ", invalid cooling coil type. Program terminated." );
			}
			MyPlantScanFlag( Item ) = false;
		} else if ( MyPlantScanFlag( Item ) && ! AnyPlantInModel ) {
			MyPlantScanFlag( Item ) = false;
		}

		// need to check all Ventilated Slab units to see if they are on Zone Equipment List or issue warning
		if ( ! ZoneEquipmentListChecked && ZoneEquipInputsFilled ) {
			ZoneEquipmentListChecked = true;
			for ( RadNum = 1; RadNum <= NumOfVentSlabs; ++RadNum ) {
				if ( CheckZoneEquipmentList( cMO_VentilatedSlab, VentSlab( RadNum ).Name ) ) continue;
				ShowSevereError( "InitVentilatedSlab: Ventilated Slab Unit=[" + cMO_VentilatedSlab + ',' + VentSlab( RadNum ).Name + "] is not on any ZoneHVAC:EquipmentList.  It will not be simulated." );
			}
		}

		if ( ! SysSizingCalc && MySizeFlag( Item ) && ! MyPlantScanFlag( Item ) ) {

			SizeVentilatedSlab( Item );

			MySizeFlag( Item ) = false;

		}

		// Do the one time initializations
		if ( BeginEnvrnFlag && MyEnvrnFlag( Item ) && ! MyPlantScanFlag( Item ) ) {

			// Coil Part
			InNode = VentSlab( Item ).ReturnAirNode;
			OutNode = VentSlab( Item ).RadInNode;
			HotConNode = VentSlab( Item ).HotControlNode;
			ColdConNode = VentSlab( Item ).ColdControlNode;
			OutsideAirNode = VentSlab( Item ).OutsideAirNode;
			RhoAir = StdRhoAir;

			// Radiation Panel Part
			ZeroSourceSumHATsurf = 0.0;
			QRadSysSrcAvg = 0.0;
			LastQRadSysSrc = 0.0;
			LastSysTimeElapsed = 0.0;
			LastTimeStepSys = 0.0;
			if ( NumOfVentSlabs > 0 ) {
				for ( auto & e : VentSlab ) {
					e.RadHeatingPower = 0.0;
					e.RadHeatingEnergy = 0.0;
					e.RadCoolingPower = 0.0;
					e.RadCoolingEnergy = 0.0;
				}
			}

			// set the initial Temperature of Return Air

			// set the mass flow rates from the input volume flow rates
			VentSlab( Item ).MaxAirMassFlow = RhoAir * VentSlab( Item ).MaxAirVolFlow;
			VentSlab( Item ).OutAirMassFlow = RhoAir * VentSlab( Item ).OutAirVolFlow;
			VentSlab( Item ).MinOutAirMassFlow = RhoAir * VentSlab( Item ).MinOutAirVolFlow;
			if ( VentSlab( Item ).OutAirMassFlow > VentSlab( Item ).MaxAirMassFlow ) {
				VentSlab( Item ).OutAirMassFlow = VentSlab( Item ).MaxAirMassFlow;
				VentSlab( Item ).MinOutAirMassFlow = VentSlab( Item ).OutAirMassFlow * ( VentSlab( Item ).MinOutAirVolFlow / VentSlab( Item ).OutAirVolFlow );
				ShowWarningError( "Outdoor air mass flow rate higher than unit flow rate, reset to unit flow rate for " + VentSlab( Item ).Name );
			}

			// set the node max and min mass flow rates
			Node( OutsideAirNode ).MassFlowRateMax = VentSlab( Item ).OutAirMassFlow;
			Node( OutsideAirNode ).MassFlowRateMin = 0.0;

			Node( OutNode ).MassFlowRateMax = VentSlab( Item ).MaxAirMassFlow;
			Node( OutNode ).MassFlowRateMin = 0.0;

			Node( InNode ).MassFlowRateMax = VentSlab( Item ).MaxAirMassFlow;
			Node( InNode ).MassFlowRateMin = 0.0;

			if ( VentSlab( Item ).HCoilPresent ) { // Only initialize these if a heating coil is actually present

				if ( VentSlab( Item ).HCoil_PlantTypeNum == TypeOf_CoilWaterSimpleHeating && ! MyPlantScanFlag( Item ) ) {
					rho = GetDensityGlycol( PlantLoop( VentSlab( Item ).HWLoopNum ).FluidName, 60.0, PlantLoop( VentSlab( Item ).HWLoopNum ).FluidIndex, RoutineName );

					VentSlab( Item ).MaxHotWaterFlow = rho * VentSlab( Item ).MaxVolHotWaterFlow;
					VentSlab( Item ).MinHotWaterFlow = rho * VentSlab( Item ).MinVolHotWaterFlow;

					InitComponentNodes( VentSlab( Item ).MinHotWaterFlow, VentSlab( Item ).MaxHotWaterFlow, VentSlab( Item ).HotControlNode, VentSlab( Item ).HotCoilOutNodeNum, VentSlab( Item ).HWLoopNum, VentSlab( Item ).HWLoopSide, VentSlab( Item ).HWBranchNum, VentSlab( Item ).HWCompNum );

				}
				if ( VentSlab( Item ).HCoil_PlantTypeNum == TypeOf_CoilSteamAirHeating && ! MyPlantScanFlag( Item ) ) {
					TempSteamIn = 100.00;
					SteamDensity = GetSatDensityRefrig( fluidNameSteam, TempSteamIn, 1.0, VentSlab( Item ).HCoil_FluidIndex, RoutineName );
					VentSlab( Item ).MaxHotSteamFlow = SteamDensity * VentSlab( Item ).MaxVolHotSteamFlow;
					VentSlab( Item ).MinHotSteamFlow = SteamDensity * VentSlab( Item ).MinVolHotSteamFlow;

					InitComponentNodes( VentSlab( Item ).MinHotSteamFlow, VentSlab( Item ).MaxHotSteamFlow, VentSlab( Item ).HotControlNode, VentSlab( Item ).HotCoilOutNodeNum, VentSlab( Item ).HWLoopNum, VentSlab( Item ).HWLoopSide, VentSlab( Item ).HWBranchNum, VentSlab( Item ).HWCompNum );

				}
			} //(VentSlab(Item)%HCoilPresent)

			if ( VentSlab( Item ).CCoilPresent && ! MyPlantScanFlag( Item ) ) {
				// Only initialize these if a cooling coil is actually present
				if ( ( VentSlab( Item ).CCoil_PlantTypeNum == TypeOf_CoilWaterCooling ) || ( VentSlab( Item ).CCoil_PlantTypeNum == TypeOf_CoilWaterDetailedFlatCooling ) ) {
					rho = GetDensityGlycol( PlantLoop( VentSlab( Item ).CWLoopNum ).FluidName, InitConvTemp, PlantLoop( VentSlab( Item ).CWLoopNum ).FluidIndex, RoutineName );
					VentSlab( Item ).MaxColdWaterFlow = rho * VentSlab( Item ).MaxVolColdWaterFlow;
					VentSlab( Item ).MinColdWaterFlow = rho * VentSlab( Item ).MinVolColdWaterFlow;
					InitComponentNodes( VentSlab( Item ).MinColdWaterFlow, VentSlab( Item ).MaxColdWaterFlow, VentSlab( Item ).ColdControlNode, VentSlab( Item ).ColdCoilOutNodeNum, VentSlab( Item ).CWLoopNum, VentSlab( Item ).CWLoopSide, VentSlab( Item ).CWBranchNum, VentSlab( Item ).CWCompNum );
				}
			}

			MyEnvrnFlag( Item ) = false;

		} // ...end start of environment inits

		if ( ! BeginEnvrnFlag ) {

			MyEnvrnFlag( Item ) = true;
		}

		// These initializations are done every iteration...
		InNode = VentSlab( Item ).ReturnAirNode;
		OutNode = VentSlab( Item ).RadInNode;
		OutsideAirNode = VentSlab( Item ).OutsideAirNode;
		AirRelNode = VentSlab( Item ).AirReliefNode;
		ZoneAirInNode = VentSlab( Item ).ZoneAirInNode;
		MixOut = VentSlab( Item ).OAMixerOutNode;

		// First, set the flow conditions up so that there is flow through the ventilated
		// slab system(this will be shut down if the system is not available or there
		// is no load
		Node( InNode ).MassFlowRate = VentSlab( Item ).MaxAirMassFlow;
		Node( InNode ).MassFlowRateMaxAvail = VentSlab( Item ).MaxAirMassFlow;
		Node( InNode ).MassFlowRateMinAvail = VentSlab( Item ).MaxAirMassFlow;
		Node( OutNode ).MassFlowRate = VentSlab( Item ).MaxAirMassFlow;
		Node( OutNode ).MassFlowRateMaxAvail = VentSlab( Item ).MaxAirMassFlow;
		Node( OutNode ).MassFlowRateMinAvail = VentSlab( Item ).MaxAirMassFlow;
		Node( OutsideAirNode ).MassFlowRate = VentSlab( Item ).OutAirMassFlow;
		Node( OutsideAirNode ).MassFlowRateMaxAvail = VentSlab( Item ).OutAirMassFlow;
		Node( OutsideAirNode ).MassFlowRateMinAvail = VentSlab( Item ).OutAirMassFlow;
		Node( AirRelNode ).MassFlowRate = VentSlab( Item ).OutAirMassFlow;
		Node( AirRelNode ).MassFlowRateMaxAvail = VentSlab( Item ).OutAirMassFlow;
		Node( AirRelNode ).MassFlowRateMinAvail = VentSlab( Item ).OutAirMassFlow;

		// Initialize the relief air (same as inlet conditions to the Ventilated Slab ..
		// Note that mass flow rates will be taken care of later.
		Node( AirRelNode ) = Node( InNode );
		OAMassFlowRate = 0.0;

		// Just in case the system is off and conditions do not get sent through
		// the system for some reason, set the outlet conditions equal to the inlet
		// conditions of the ventilated slab mixer
		Node( OutNode ).Temp = Node( InNode ).Temp;
		Node( OutNode ).Press = Node( InNode ).Press;
		Node( OutNode ).HumRat = Node( InNode ).HumRat;
		Node( OutNode ).Enthalpy = Node( InNode ).Enthalpy;

		// These initializations only need to be done once at the start of the iterations...
		if ( BeginTimeStepFlag && FirstHVACIteration ) {
			// Initialize the outside air conditions...
			Node( OutsideAirNode ).Temp = Node( OutsideAirNode ).OutAirDryBulb;
			Node( OutsideAirNode ).HumRat = OutHumRat;
			Node( OutsideAirNode ).Press = OutBaroPress;

			// The first pass through in a particular time step
			ZoneNum = VentSlab( Item ).ZonePtr;
			ZeroSourceSumHATsurf( ZoneNum ) = SumHATsurf( ZoneNum ); // Set this to figure what part of the load the radiant system meets
			for ( RadSurfNum = 1; RadSurfNum <= VentSlab( Item ).NumOfSurfaces; ++RadSurfNum ) {
				SurfNum = VentSlab( Item ).SurfacePtr( RadSurfNum );
				QRadSysSrcAvg( SurfNum ) = 0.0; // Initialize this variable to zero (radiant system defaults to off)
				LastQRadSysSrc( SurfNum ) = 0.0; // At the start of a time step, reset to zero so average calculation can begin again
				LastSysTimeElapsed( SurfNum ) = 0.0; // At the start of a time step, reset to zero so average calculation can begin again
				LastTimeStepSys( SurfNum ) = 0.0; // At the start of a time step, reset to zero so average calculation can begin again
			}
		}

	}

	void
	SizeVentilatedSlab( int const Item )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Young Tae Chae, Rick Strand
		//       DATE WRITTEN   June 2008
		//       MODIFIED       July 2013 Daeho Kang, add component sizing table entries
		//                      July 2014, B. Nigusse, added scalable sizing
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing Ventilated Slab components for which flow rates have not been
		// specified in the input.

		// METHODOLOGY EMPLOYED:
		// Obtains flow rates from the zone sizing arrays and plant sizing data.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSizing;
		using namespace InputProcessor;
		using WaterCoils::SetCoilDesFlow;
		using WaterCoils::GetCoilWaterInletNode;
		using WaterCoils::GetCoilWaterOutletNode;
		using SteamCoils::GetCoilSteamInletNode;
		using SteamCoils::GetCoilSteamOutletNode;
		using HVACHXAssistedCoolingCoil::GetHXDXCoilName;
		using HVACHXAssistedCoolingCoil::GetHXCoilType;
		//  USE BranchInputManager, ONLY: MyPlantSizingIndex
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSpecificHeatGlycol;
		using DataPlant::PlantLoop;
		using DataPlant::MyPlantSizingIndex;
		using ReportSizingManager::ReportSizingOutput;
		using ReportSizingManager::RequestSizing;
		using General::RoundSigDigits;
		using DataHVACGlobals::SystemAirflowSizing;
		using DataHVACGlobals::CoolingAirflowSizing;
		using DataHVACGlobals::HeatingAirflowSizing;
		using DataHVACGlobals::CoolingCapacitySizing;
		using DataHVACGlobals::HeatingCapacitySizing;
		using DataHeatBalance::Zone;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "SizeVentilatedSlab" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int PltSizHeatNum; // index of plant sizing object for 1st heating loop
		int PltSizCoolNum; // index of plant sizing object for 1st cooling loop
		bool ErrorsFound;
		Real64 DesCoilLoad;
		Real64 TempSteamIn;
		Real64 EnthSteamInDry;
		Real64 EnthSteamOutWet;
		Real64 LatentHeatSteam;
		Real64 SteamDensity;
		static int CoilWaterInletNode( 0 );
		static int CoilWaterOutletNode( 0 );
		static int CoilSteamInletNode( 0 );
		static int CoilSteamOutletNode( 0 );
		std::string CoolingCoilName;
		std::string CoolingCoilType;
		Real64 rho;
		Real64 Cp;
		static int DummyWaterIndex( 1 );
		bool IsAutoSize; // Indicator to autosize
		Real64 MaxAirVolFlowDes; // Autosized maximum air flow for reporting
		Real64 MaxAirVolFlowUser; // Hardsized maximum air flow for reporting
		Real64 OutAirVolFlowDes; // Autosized outdoor air flow for reporting
		Real64 OutAirVolFlowUser; // Hardsized outdoor air flow for reporting
		Real64 MinOutAirVolFlowDes; // Autosized minimum outdoor air flow for reporting
		Real64 MinOutAirVolFlowUser; // Hardsized minimum outdoor air flow for reporting
		Real64 MaxVolHotWaterFlowDes; // Autosized maximum hot water flow for reporting
		Real64 MaxVolHotWaterFlowUser; // Hardsized maximum hot water flow for reporting
		Real64 MaxVolHotSteamFlowDes; // Autosized maximum hot steam flow for reporting
		Real64 MaxVolHotSteamFlowUser; // Hardsized maximum hot steam flow for reporting
		Real64 MaxVolColdWaterFlowDes; // Autosized maximum cold water flow for reporting
		Real64 MaxVolColdWaterFlowUser; // Hardsized maximum cold water flow for reporting
		std::string CompName; // component name
		std::string CompType; // component type
		std::string SizingString; // input field sizing description (e.g., Nominal Capacity)
		Real64 TempSize; // autosized value of coil input field
		int FieldNum = 2; // IDD numeric field number where input field description is found
		int SizingMethod; // Integer representation of sizing method name (e.g., CoolingAirflowSizing, HeatingAirflowSizing, CoolingCapacitySizing, HeatingCapacitySizing, etc.)
		bool PrintFlag; // TRUE when sizing information is reported in the eio file
		int zoneHVACIndex; // index of zoneHVAC equipment sizing specification
		int SAFMethod( 0 ); // supply air flow rate sizing method (SupplyAirFlowRate, FlowPerFloorArea, FractionOfAutosizedCoolingAirflow, FractionOfAutosizedHeatingAirflow ...)
		int CapSizingMethod(0); // capacity sizing methods (HeatingDesignCapacity, CapacityPerFloorArea, FractionOfAutosizedCoolingCapacity, and FractionOfAutosizedHeatingCapacity )
		Real64 CoolingAirVolFlowScalable; // cooling airvolume for rate determined using scalable sizing method
		Real64 HeatingAirVolFlowScalable; // heating airvolume for rate determined using scalable sizing method

		PltSizCoolNum = 0;
		PltSizHeatNum = 0;
		ErrorsFound = false;
		IsAutoSize = false;
		MaxAirVolFlowDes = 0.0;
		MaxAirVolFlowUser = 0.0;
		OutAirVolFlowDes = 0.0;
		OutAirVolFlowUser = 0.0;
		MinOutAirVolFlowDes = 0.0;
		MinOutAirVolFlowUser = 0.0;
		MaxVolHotWaterFlowDes = 0.0;
		MaxVolHotWaterFlowUser = 0.0;
		MaxVolHotSteamFlowDes = 0.0;
		MaxVolHotSteamFlowUser = 0.0;
		MaxVolColdWaterFlowDes = 0.0;
		MaxVolColdWaterFlowUser = 0.0;
		CoolingAirVolFlowScalable = 0.0;
		HeatingAirVolFlowScalable = 0.0;
		DataScalableSizingON = false;
		DataScalableCapSizingON = false;
		CompType = cMO_VentilatedSlab;
		CompName = VentSlab( Item ).Name;
		DataZoneNumber = VentSlab( Item ).ZonePtr;

		if ( VentSlab( Item ).HVACSizingIndex > 0 ) {
			zoneHVACIndex = VentSlab( Item ).HVACSizingIndex;
			// N1 , \field Maximum Supply Air Flow Rate
			FieldNum = 1;
			PrintFlag = true;
			SizingString = VentSlabNumericFields( Item ).FieldNames( FieldNum ) + " [m3/s]";
			if ( ZoneHVACSizing( zoneHVACIndex ).CoolingSAFMethod > 0 ) {
				SizingMethod = CoolingAirflowSizing;
				SAFMethod = ZoneHVACSizing( zoneHVACIndex ).CoolingSAFMethod;
				ZoneEqSizing( CurZoneEqNum ).SizingMethod( SizingMethod ) = SAFMethod;
				if ( SAFMethod == None || SAFMethod == SupplyAirFlowRate || SAFMethod == FlowPerFloorArea || SAFMethod == FractionOfAutosizedCoolingAirflow ) {
					if ( SAFMethod == SupplyAirFlowRate ) {
						if ( ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow > 0.0 ) {
							ZoneEqSizing( CurZoneEqNum ).AirVolFlow = ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow;
							ZoneEqSizing( CurZoneEqNum ).SystemAirFlow = true;
						}
						TempSize = ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow;
					} else if ( SAFMethod == FlowPerFloorArea ) {
						ZoneEqSizing( CurZoneEqNum ).SystemAirFlow = true;
						ZoneEqSizing( CurZoneEqNum ).AirVolFlow = ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow * Zone( DataZoneNumber ).FloorArea;
						TempSize = ZoneEqSizing( CurZoneEqNum ).AirVolFlow;
						DataScalableSizingON = true;
					} else if ( SAFMethod == FractionOfAutosizedCoolingAirflow ) {
						DataFracOfAutosizedCoolingAirflow = ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow;
						TempSize = AutoSize;
						DataScalableSizingON = true;
					} else {
						TempSize = ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow;
					}
					RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
					CoolingAirVolFlowScalable = TempSize;

				} else if ( SAFMethod == FlowPerCoolingCapacity ) {
					SizingMethod = CoolingCapacitySizing;
					TempSize = AutoSize;
					PrintFlag = false;
					DataScalableSizingON = true;
					DataFlowUsedForSizing = FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow;
					RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
					DataAutosizedCoolingCapacity = TempSize;
					DataFlowPerCoolingCapacity = ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow;
					SizingMethod = CoolingAirflowSizing;
					PrintFlag = true;
					TempSize = AutoSize;
					RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
					CoolingAirVolFlowScalable = TempSize;
				}
			}
			if ( ZoneHVACSizing( zoneHVACIndex ).HeatingSAFMethod > 0 )	{
				SizingMethod = HeatingAirflowSizing;
				SAFMethod = ZoneHVACSizing( zoneHVACIndex ).HeatingSAFMethod;
				ZoneEqSizing( CurZoneEqNum ).SizingMethod( SizingMethod ) = SAFMethod;
				if ( SAFMethod == None || SAFMethod == SupplyAirFlowRate || SAFMethod == FlowPerFloorArea || SAFMethod == FractionOfAutosizedHeatingAirflow ) {
					if ( SAFMethod == SupplyAirFlowRate ) {
						if ( ZoneHVACSizing( zoneHVACIndex ).MaxHeatAirVolFlow > 0.0 ) {
							ZoneEqSizing( CurZoneEqNum ).AirVolFlow = ZoneHVACSizing( zoneHVACIndex ).MaxHeatAirVolFlow;
							ZoneEqSizing( CurZoneEqNum ).SystemAirFlow = true;
						}
						TempSize = ZoneHVACSizing( zoneHVACIndex ).MaxHeatAirVolFlow;
					} else if ( SAFMethod == FlowPerFloorArea ) {
						ZoneEqSizing( CurZoneEqNum ).SystemAirFlow = true;
						ZoneEqSizing( CurZoneEqNum ).AirVolFlow = ZoneHVACSizing( zoneHVACIndex ).MaxHeatAirVolFlow * Zone( DataZoneNumber ).FloorArea;
						TempSize = ZoneEqSizing( CurZoneEqNum ).AirVolFlow;
						DataScalableSizingON = true;
					} else if ( SAFMethod == FractionOfAutosizedHeatingAirflow ) {
						DataFracOfAutosizedHeatingAirflow = ZoneHVACSizing( zoneHVACIndex ).MaxHeatAirVolFlow;
						TempSize = AutoSize;
						DataScalableSizingON = true;
					} else {
						TempSize = ZoneHVACSizing( zoneHVACIndex ).MaxHeatAirVolFlow;
					}
					RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
					HeatingAirVolFlowScalable = TempSize;
				} else if ( SAFMethod == FlowPerHeatingCapacity ) {
					SizingMethod = HeatingCapacitySizing;
					TempSize = AutoSize;
					PrintFlag = false;
					DataScalableSizingON = true;
					DataFlowUsedForSizing = FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow;
					RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
					DataAutosizedHeatingCapacity = TempSize;
					DataFlowPerHeatingCapacity = ZoneHVACSizing( zoneHVACIndex ).MaxHeatAirVolFlow;
					SizingMethod = HeatingAirflowSizing;
					PrintFlag = true;
					TempSize = AutoSize;
					RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
					HeatingAirVolFlowScalable = TempSize;
				}
			}
			//DataScalableSizingON = false;
			VentSlab( Item ).MaxAirVolFlow = max( CoolingAirVolFlowScalable, HeatingAirVolFlowScalable );
		} else {
			// no scalble sizing method has been specified. Sizing proceeds using the method
			// specified in the zoneHVAC object
			// N1 , \field Maximum Supply Air Flow Rate
			SizingMethod = SystemAirflowSizing;
			FieldNum = 1;
			PrintFlag = true;
			SizingString = VentSlabNumericFields( Item ).FieldNames( FieldNum ) + " [m3/s]";
			TempSize = VentSlab( Item ).MaxAirVolFlow;
			RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
			VentSlab( Item ).MaxAirVolFlow = TempSize;
		}

		IsAutoSize = false;
		if ( VentSlab( Item ).OutAirVolFlow == AutoSize ) {
			IsAutoSize = true;
		}
		if ( CurZoneEqNum > 0 ) {
			if ( !IsAutoSize && !ZoneSizingRunDone ) {
				if ( VentSlab( Item ).OutAirVolFlow > 0.0 ) {
					ReportSizingOutput( cMO_VentilatedSlab, VentSlab( Item ).Name, "User-Specified Maximum Outdoor Air Flow Rate [m3/s]", VentSlab( Item ).OutAirVolFlow );
				}
			} else { // Autosize or hard-size with sizing run
				CheckZoneSizing( cMO_VentilatedSlab, VentSlab( Item ).Name );
				OutAirVolFlowDes = VentSlab( Item ).MaxAirVolFlow;
				if ( IsAutoSize ) {
					VentSlab( Item ).OutAirVolFlow = OutAirVolFlowDes;
					ReportSizingOutput( cMO_VentilatedSlab, VentSlab( Item ).Name, "Design Size Maximum Outdoor Air Flow Rate [m3/s]", OutAirVolFlowDes );
				} else {
					if ( VentSlab( Item ).OutAirVolFlow > 0.0 && OutAirVolFlowDes > 0.0 ) {
						OutAirVolFlowUser = VentSlab( Item ).OutAirVolFlow;
						ReportSizingOutput( cMO_VentilatedSlab, VentSlab( Item ).Name, "Design Size Maximum Outdoor Air Flow Rate [m3/s]", OutAirVolFlowDes, "User-Specified Maximum Outdoor Air Flow Rate [m3/s]", OutAirVolFlowUser );
						if ( DisplayExtraWarnings ) {
							if ( ( std::abs( OutAirVolFlowDes - OutAirVolFlowUser ) / OutAirVolFlowUser ) > AutoVsHardSizingThreshold ) {
								ShowMessage( "SizeVentilatedSlab: Potential issue with equipment sizing for ZoneHVAC:VentilatedSlab = \"" + VentSlab( Item ).Name + "\"." );
								ShowContinueError( "User-Specified Maximum Outdoor Air Flow Rate of " + RoundSigDigits( OutAirVolFlowUser, 5 ) + " [m3/s]" );
								ShowContinueError( "differs from Design Size Maximum Outdoor Air Flow Rate of " + RoundSigDigits( OutAirVolFlowDes, 5 ) + " [m3/s]" );
								ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
								ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
							}
						}
					}
				}
			}
		}

		IsAutoSize = false;
		if ( VentSlab( Item ).MinOutAirVolFlow == AutoSize ) {
			IsAutoSize = true;
		}
		if ( CurZoneEqNum > 0 ) {
			if ( !IsAutoSize && !ZoneSizingRunDone ) {
				if ( VentSlab( Item ).MinOutAirVolFlow > 0.0 ) {
					ReportSizingOutput( cMO_VentilatedSlab, VentSlab( Item ).Name, "User-Specified Minimum Outdoor Air Flow Rate [m3/s]", VentSlab( Item ).MinOutAirVolFlow );
				}
			} else {
				CheckZoneSizing( cMO_VentilatedSlab, VentSlab( Item ).Name );
				MinOutAirVolFlowDes = min( FinalZoneSizing( CurZoneEqNum ).MinOA, VentSlab( Item ).MaxAirVolFlow );
				if ( MinOutAirVolFlowDes < SmallAirVolFlow ) {
					MinOutAirVolFlowDes = 0.0;
				}
				if ( IsAutoSize ) {
					VentSlab( Item ).MinOutAirVolFlow = MinOutAirVolFlowDes;
					ReportSizingOutput( cMO_VentilatedSlab, VentSlab( Item ).Name, "Design Size Minimum Outdoor Air Flow Rate [m3/s]", MinOutAirVolFlowDes );
				} else { // Hard-size with sizing data
					if ( VentSlab( Item ).MinOutAirVolFlow > 0.0 && MinOutAirVolFlowDes > 0.0 ) {
						MinOutAirVolFlowUser = VentSlab( Item ).MinOutAirVolFlow;
						ReportSizingOutput( cMO_VentilatedSlab, VentSlab( Item ).Name, "Design Size Minimum Outdoor Air Flow Rate [m3/s]", MinOutAirVolFlowDes, "User-Specified Minimum Outdoor Air Flow Rate [m3/s]", MinOutAirVolFlowUser );
						if ( DisplayExtraWarnings ) {
							if ( ( std::abs( MinOutAirVolFlowDes - MinOutAirVolFlowUser ) / MinOutAirVolFlowUser ) > AutoVsHardSizingThreshold ) {
								ShowMessage( "SizeVentilatedSlab: Potential issue with equipment sizing for ZoneHVAC:VentilatedSlab = \"" + VentSlab( Item ).Name + "\"." );
								ShowContinueError( "User-Specified Minimum Outdoor Air Flow Rate of " + RoundSigDigits( MinOutAirVolFlowUser, 5 ) + " [m3/s]" );
								ShowContinueError( "differs from Design Size Minimum Outdoor Air Flow Rate of " + RoundSigDigits( MinOutAirVolFlowDes, 5 ) + " [m3/s]" );
								ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
								ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
							}
						}
					}
				}
			}
		}

		IsAutoSize = false;
		if ( VentSlab( Item ).MaxVolHotWaterFlow == AutoSize ) {
			IsAutoSize = true;
		}
		if ( VentSlab( Item ).HCoilType == Heating_WaterCoilType ) {

			if ( CurZoneEqNum > 0 ) {
				if ( !IsAutoSize && !ZoneSizingRunDone ) {
					if ( VentSlab( Item ).MaxVolHotWaterFlow > 0.0 ) {
						ReportSizingOutput( cMO_VentilatedSlab, VentSlab( Item ).Name, "User-Specified Maximum Hot Water Flow [m3/s]", VentSlab( Item ).MaxVolHotWaterFlow );
					}
				} else { // Autosize or hard-size with sizing run
					CheckZoneSizing( cMO_VentilatedSlab, VentSlab( Item ).Name );

					CoilWaterInletNode = GetCoilWaterInletNode( "Coil:Heating:Water", VentSlab( Item ).HCoilName, ErrorsFound );
					CoilWaterOutletNode = GetCoilWaterOutletNode( "Coil:Heating:Water", VentSlab( Item ).HCoilName, ErrorsFound );
					if ( IsAutoSize ) {
						PltSizHeatNum = MyPlantSizingIndex( "Coil:Heating:Water", VentSlab( Item ).HCoilName, CoilWaterInletNode, CoilWaterOutletNode, ErrorsFound );
						//END IF
						if ( PltSizHeatNum > 0 ) {
							if ( FinalZoneSizing( CurZoneEqNum ).DesHeatMassFlow >= SmallAirVolFlow ) {
								SizingMethod = HeatingCapacitySizing;
								if ( VentSlab( Item ).HVACSizingIndex > 0 ) {
									zoneHVACIndex = VentSlab( Item ).HVACSizingIndex;
									CapSizingMethod = ZoneHVACSizing( zoneHVACIndex ).HeatingCapMethod;
									ZoneEqSizing( CurZoneEqNum ).SizingMethod( SizingMethod ) = CapSizingMethod;
									if ( CapSizingMethod == HeatingDesignCapacity || CapSizingMethod == CapacityPerFloorArea || CapSizingMethod == FractionOfAutosizedHeatingCapacity ) {
										if ( CapSizingMethod == HeatingDesignCapacity ) {
											if ( ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity > 0.0 ) {
												ZoneEqSizing( CurZoneEqNum ).HeatingCapacity = true;
												ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity;
											} else {
												DataFlowUsedForSizing = FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow;
											}
											TempSize = ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity;
										} else if ( CapSizingMethod == CapacityPerFloorArea ) {
											ZoneEqSizing( CurZoneEqNum ).HeatingCapacity = true;
											ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity * Zone( DataZoneNumber ).FloorArea;
											DataScalableCapSizingON = true;
										} else if ( CapSizingMethod == FractionOfAutosizedHeatingCapacity ) {
											DataFracOfAutosizedHeatingCapacity = ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity;
											DataFlowUsedForSizing = FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow;
											TempSize = AutoSize;
											DataScalableCapSizingON = true;
										}
									}
									SizingString = "";
									PrintFlag = false;
									RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
									DesCoilLoad = TempSize;
								} else {
									SizingString = "";
									PrintFlag = false;
									TempSize = AutoSize;
									RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
									DesCoilLoad = TempSize;
								}
								rho = GetDensityGlycol( PlantLoop( VentSlab( Item ).HWLoopNum ).FluidName, 60., PlantLoop( VentSlab( Item ).HWLoopNum ).FluidIndex, RoutineName );
								Cp = GetSpecificHeatGlycol( PlantLoop( VentSlab( Item ).HWLoopNum ).FluidName, 60., PlantLoop( VentSlab( Item ).HWLoopNum ).FluidIndex, RoutineName );
								MaxVolHotWaterFlowDes = DesCoilLoad / ( PlantSizData( PltSizHeatNum ).DeltaT * Cp * rho );
							} else {
								MaxVolHotWaterFlowDes = 0.0;
							}
						} else {
							ShowSevereError( "Autosizing of water flow requires a heating loop Sizing:Plant object" );
							ShowContinueError( "Occurs in " + cMO_VentilatedSlab + " Object=" + VentSlab( Item ).Name );
							ErrorsFound = true;
						}
					}

					if ( IsAutoSize ) {
						VentSlab( Item ).MaxVolHotWaterFlow = MaxVolHotWaterFlowDes;
						ReportSizingOutput( cMO_VentilatedSlab, VentSlab( Item ).Name, "Design Size Maximum Hot Water Flow [m3/s]", MaxVolHotWaterFlowDes );
					} else { // Hard-size with sizing data
						if ( VentSlab( Item ).MaxVolHotWaterFlow > 0.0 && MaxVolHotWaterFlowDes > 0.0 ) {
							MaxVolHotWaterFlowUser = VentSlab( Item ).MaxVolHotWaterFlow;
							ReportSizingOutput( cMO_VentilatedSlab, VentSlab( Item ).Name, "Design Size Maximum Hot Water Flow [m3/s]", MaxVolHotWaterFlowDes, "User-Specified Maximum Hot Water Flow [m3/s]", MaxVolHotWaterFlowUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( MaxVolHotWaterFlowDes - MaxVolHotWaterFlowUser ) / MaxVolHotWaterFlowUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeVentilatedSlab: Potential issue with equipment sizing for ZoneHVAC:VentilatedSlab = \"" + VentSlab( Item ).Name + "\"." );
									ShowContinueError( "User-Specified Maximum Hot Water Flow of " + RoundSigDigits( MaxVolHotWaterFlowUser, 5 ) + " [m3/s]" );
									ShowContinueError( "differs from Design Size Maximum Hot Water Flow of " + RoundSigDigits( MaxVolHotWaterFlowDes, 5 ) + " [m3/s]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
					}
				}
			}
		} else {
			VentSlab( Item ).MaxVolHotWaterFlow = 0.0;
		}

		IsAutoSize = false;
		if ( VentSlab( Item ).MaxVolHotSteamFlow == AutoSize ) {
			IsAutoSize = true;
		}
		if ( VentSlab( Item ).HCoilType == Heating_SteamCoilType ) {

			if ( CurZoneEqNum > 0 ) {
				if ( !IsAutoSize && !ZoneSizingRunDone ) {
					if ( VentSlab( Item ).MaxVolHotSteamFlow > 0.0 ) {
						ReportSizingOutput( cMO_VentilatedSlab, VentSlab( Item ).Name, "User-Specified Maximum Steam Flow [m3/s]", VentSlab( Item ).MaxVolHotSteamFlow );
					}
				} else { // Autosize or hard-size with sizing run
					CheckZoneSizing( "ZoneHVAC:VentilatedSlab", VentSlab( Item ).Name );

					CoilSteamInletNode = GetCoilSteamInletNode( "Coil:Heating:Steam", VentSlab( Item ).HCoilName, ErrorsFound );
					CoilSteamOutletNode = GetCoilSteamOutletNode( "Coil:Heating:Steam", VentSlab( Item ).HCoilName, ErrorsFound );
					if ( IsAutoSize ) {
						PltSizHeatNum = MyPlantSizingIndex( "Coil:Heating:Steam", VentSlab( Item ).HCoilName, CoilSteamInletNode, CoilSteamOutletNode, ErrorsFound );
						if ( PltSizHeatNum > 0 ) {
							if ( FinalZoneSizing( CurZoneEqNum ).DesHeatMassFlow >= SmallAirVolFlow ) {
								SizingMethod = HeatingCapacitySizing;
								if ( VentSlab( Item ).HVACSizingIndex > 0 ) {
									zoneHVACIndex = VentSlab( Item ).HVACSizingIndex;
									CapSizingMethod = ZoneHVACSizing( zoneHVACIndex ).HeatingCapMethod;
									ZoneEqSizing( CurZoneEqNum ).SizingMethod( SizingMethod ) = CapSizingMethod;
									if ( CapSizingMethod == HeatingDesignCapacity || CapSizingMethod == CapacityPerFloorArea || CapSizingMethod == FractionOfAutosizedHeatingCapacity ) {
										if ( CapSizingMethod == HeatingDesignCapacity ) {
											if ( ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity > 0.0 ) {
												ZoneEqSizing( CurZoneEqNum ).HeatingCapacity = true;
												ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity;
											} else {
												DataFlowUsedForSizing = FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow;
											}
											TempSize = ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity;
										} else if ( CapSizingMethod == CapacityPerFloorArea ) {
											ZoneEqSizing( CurZoneEqNum ).HeatingCapacity = true;
											ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity * Zone( DataZoneNumber ).FloorArea;
											DataScalableCapSizingON = true;
										} else if ( CapSizingMethod == FractionOfAutosizedHeatingCapacity ) {
											DataFracOfAutosizedHeatingCapacity = ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity;
											DataFlowUsedForSizing = FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow;
											TempSize = AutoSize;
											DataScalableCapSizingON = true;
										}
									}
									SizingString = "";
									PrintFlag = false;
									RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
									DesCoilLoad = TempSize;
								} else {
									SizingString = "";
									PrintFlag = false;
									TempSize = AutoSize;
									RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
									DesCoilLoad = TempSize;
								}
								TempSteamIn = 100.00;
								EnthSteamInDry = GetSatEnthalpyRefrig( fluidNameSteam, TempSteamIn, 1.0, VentSlab( Item ).HCoil_FluidIndex, RoutineName );
								EnthSteamOutWet = GetSatEnthalpyRefrig( fluidNameSteam, TempSteamIn, 0.0, VentSlab( Item ).HCoil_FluidIndex, RoutineName );
								LatentHeatSteam = EnthSteamInDry - EnthSteamOutWet;
								SteamDensity = GetSatDensityRefrig( fluidNameSteam, TempSteamIn, 1.0, VentSlab( Item ).HCoil_FluidIndex, RoutineName );
								Cp = GetSpecificHeatGlycol( fluidNameWater, 60.0, DummyWaterIndex, RoutineName );
								rho = GetDensityGlycol( fluidNameWater, 60.0, DummyWaterIndex, RoutineName );
								MaxVolHotSteamFlowDes = DesCoilLoad / ( ( PlantSizData( PltSizHeatNum ).DeltaT * Cp * rho ) + SteamDensity * LatentHeatSteam );
							} else {
								MaxVolHotSteamFlowDes = 0.0;
							}
						} else {
							ShowSevereError( "Autosizing of Steam flow requires a heating loop Sizing:Plant object" );
							ShowContinueError( "Occurs in ZoneHVAC:VentilatedSlab Object=" + VentSlab( Item ).Name );
							ErrorsFound = true;
						}
					}
					if ( IsAutoSize ) {
						VentSlab( Item ).MaxVolHotSteamFlow = MaxVolHotSteamFlowDes;
						ReportSizingOutput( cMO_VentilatedSlab, VentSlab( Item ).Name, "Design Size Maximum Steam Flow [m3/s]", MaxVolHotSteamFlowDes );
					} else {
						if ( VentSlab( Item ).MaxVolHotSteamFlow > 0.0 && MaxVolHotSteamFlowDes > 0.0 ) {
							MaxVolHotSteamFlowUser = VentSlab( Item ).MaxVolHotSteamFlow;
							ReportSizingOutput( cMO_VentilatedSlab, VentSlab( Item ).Name, "Design Size Maximum Steam Flow [m3/s]", MaxVolHotSteamFlowDes, "User-Specified Maximum Steam Flow [m3/s]", MaxVolHotSteamFlowUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( MaxVolHotSteamFlowDes - MaxVolHotSteamFlowUser ) / MaxVolHotSteamFlowUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeVentilatedSlab: Potential issue with equipment sizing for ZoneHVAC:VentilatedSlab = \"" + VentSlab( Item ).Name + "\"." );
									ShowContinueError( "User-Specified Maximum Steam Flow of " + RoundSigDigits( MaxVolHotSteamFlowUser, 5 ) + " [m3/s]" );
									ShowContinueError( "differs from Design Size Maximum Steam Flow of " + RoundSigDigits( MaxVolHotSteamFlowDes, 5 ) + " [m3/s]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
					}
				}
			}
		} else {
			VentSlab( Item ).MaxVolHotSteamFlow = 0.0;
		}

		IsAutoSize = false;
		if ( VentSlab( Item ).MaxVolColdWaterFlow == AutoSize ) {
			IsAutoSize = true;
		}
		if ( CurZoneEqNum > 0 ) {
			if ( !IsAutoSize && !ZoneSizingRunDone ) {
				if ( VentSlab( Item ).MaxVolColdWaterFlow > 0.0 ) {
					ReportSizingOutput( cMO_VentilatedSlab, VentSlab( Item ).Name, "User-Specified Maximum Cold Water Flow [m3/s]", VentSlab( Item ).MaxVolColdWaterFlow );
				}
			} else {
				CheckZoneSizing( cMO_VentilatedSlab, VentSlab( Item ).Name );
				if ( VentSlab( Item ).CCoilType == Cooling_CoilHXAssisted ) {
					CoolingCoilName = GetHXDXCoilName( VentSlab( Item ).CCoilTypeCh, VentSlab( Item ).CCoilName, ErrorsFound );
					CoolingCoilType = GetHXCoilType( VentSlab( Item ).CCoilTypeCh, VentSlab( Item ).CCoilName, ErrorsFound );
				} else {
					CoolingCoilName = VentSlab( Item ).CCoilName;
					CoolingCoilType = VentSlab( Item ).CCoilTypeCh;
				}
				CoilWaterInletNode = GetCoilWaterInletNode( CoolingCoilType, CoolingCoilName, ErrorsFound );
				CoilWaterOutletNode = GetCoilWaterOutletNode( CoolingCoilType, CoolingCoilName, ErrorsFound );
				if ( IsAutoSize ) {
					PltSizCoolNum = MyPlantSizingIndex( CoolingCoilType, CoolingCoilName, CoilWaterInletNode, CoilWaterOutletNode, ErrorsFound );
					if ( PltSizCoolNum > 0 ) {
						if ( FinalZoneSizing( CurZoneEqNum ).DesCoolMassFlow >= SmallAirVolFlow ) {
							SizingMethod = CoolingCapacitySizing;
							if ( VentSlab( Item ).HVACSizingIndex > 0 ) {
								zoneHVACIndex = VentSlab( Item ).HVACSizingIndex;
								CapSizingMethod = ZoneHVACSizing( zoneHVACIndex ).CoolingCapMethod;
								ZoneEqSizing( CurZoneEqNum ).SizingMethod( SizingMethod ) = CapSizingMethod;
								if ( CapSizingMethod == CoolingDesignCapacity || CapSizingMethod == CapacityPerFloorArea || CapSizingMethod == FractionOfAutosizedCoolingCapacity ) {
									if ( CapSizingMethod == CoolingDesignCapacity ) {
										if ( ZoneHVACSizing( zoneHVACIndex ).ScaledCoolingCapacity > 0.0 ) {
											ZoneEqSizing( CurZoneEqNum ).CoolingCapacity = true;
											ZoneEqSizing( CurZoneEqNum ).DesCoolingLoad = ZoneHVACSizing( zoneHVACIndex ).ScaledCoolingCapacity;
										} else {
											DataFlowUsedForSizing = FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow;
										}
										TempSize = ZoneHVACSizing( zoneHVACIndex ).ScaledCoolingCapacity;
									} else if ( CapSizingMethod == CapacityPerFloorArea ) {
										ZoneEqSizing( CurZoneEqNum ).CoolingCapacity = true;
										ZoneEqSizing( CurZoneEqNum ).DesCoolingLoad = ZoneHVACSizing( zoneHVACIndex ).ScaledCoolingCapacity * Zone( DataZoneNumber ).FloorArea;
										DataScalableCapSizingON = true;
									} else if ( CapSizingMethod == FractionOfAutosizedCoolingCapacity ) {
										DataFracOfAutosizedHeatingCapacity = ZoneHVACSizing( zoneHVACIndex ).ScaledCoolingCapacity;
										DataFlowUsedForSizing = FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow;
										TempSize = AutoSize;
										DataScalableCapSizingON = true;
									}
								}
								SizingString = "";
								PrintFlag = false;
								RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
								DesCoilLoad = TempSize;
							} else {
								SizingString = "";
								PrintFlag = false;
								TempSize = AutoSize;
								DataFlowUsedForSizing = FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow;
								RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
								DesCoilLoad = TempSize;
							}
							rho = GetDensityGlycol( PlantLoop( VentSlab( Item ).CWLoopNum ).FluidName, 5., PlantLoop( VentSlab( Item ).CWLoopNum ).FluidIndex, RoutineName );
							Cp = GetSpecificHeatGlycol( PlantLoop( VentSlab( Item ).CWLoopNum ).FluidName, 5., PlantLoop( VentSlab( Item ).CWLoopNum ).FluidIndex, RoutineName );
							MaxVolColdWaterFlowDes = DesCoilLoad / ( PlantSizData( PltSizCoolNum ).DeltaT * Cp * rho );
						} else {
							MaxVolColdWaterFlowDes = 0.0;
						}
					} else {
						ShowSevereError( "Autosizing of water flow requires a cooling loop Sizing:Plant object" );
						ShowContinueError( "Occurs in " + cMO_VentilatedSlab + " Object=" + VentSlab( Item ).Name );
						ErrorsFound = true;
					}
				}
				if ( IsAutoSize ) {
					VentSlab( Item ).MaxVolColdWaterFlow = MaxVolColdWaterFlowDes;
					ReportSizingOutput( cMO_VentilatedSlab, VentSlab( Item ).Name, "Design Size Maximum Cold Water Flow [m3/s]", MaxVolColdWaterFlowDes );
				} else {
					if ( VentSlab( Item ).MaxVolColdWaterFlow > 0.0 && MaxVolColdWaterFlowDes > 0.0 ) {
						MaxVolColdWaterFlowUser = VentSlab( Item ).MaxVolColdWaterFlow;
						ReportSizingOutput( cMO_VentilatedSlab, VentSlab( Item ).Name, "Design Size Maximum Cold Water Flow [m3/s]", MaxVolColdWaterFlowDes, "User-Specified Maximum Cold Water Flow [m3/s]", MaxVolColdWaterFlowUser );
						if ( DisplayExtraWarnings ) {
							if ( ( std::abs( MaxVolColdWaterFlowDes - MaxVolColdWaterFlowUser ) / MaxVolColdWaterFlowUser ) > AutoVsHardSizingThreshold ) {
								ShowMessage( "SizeVentilatedSlab: Potential issue with equipment sizing for ZoneHVAC:VentilatedSlab = \"" + VentSlab( Item ).Name + "\"." );
								ShowContinueError( "User-Specified Maximum Cold Water Flow of " + RoundSigDigits( MaxVolColdWaterFlowUser, 5 ) + " [m3/s]" );
								ShowContinueError( "differs from Design Size Maximum Cold Water Flow of " + RoundSigDigits( MaxVolColdWaterFlowDes, 5 ) + " [m3/s]" );
								ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
								ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
							}
						}
					}
				}
			}
		}

		if ( VentSlab( Item ).CCoilType == Cooling_CoilHXAssisted ) {
			CoolingCoilName = GetHXDXCoilName( VentSlab( Item ).CCoilTypeCh, VentSlab( Item ).CCoilName, ErrorsFound );
			CoolingCoilType = GetHXCoilType( VentSlab( Item ).CCoilTypeCh, VentSlab( Item ).CCoilName, ErrorsFound );
		} else {
			CoolingCoilName = VentSlab( Item ).CCoilName;
			CoolingCoilType = VentSlab( Item ).CCoilTypeCh;
		}
		SetCoilDesFlow( CoolingCoilType, CoolingCoilName, VentSlab( Item ).MaxAirVolFlow, ErrorsFound );
		SetCoilDesFlow( VentSlab( Item ).HCoilTypeCh, VentSlab( Item ).HCoilName, VentSlab( Item ).MaxAirVolFlow, ErrorsFound );

		if ( CurZoneEqNum > 0 ) {
			ZoneEqSizing( CurZoneEqNum ).MaxHWVolFlow = VentSlab( Item ).MaxVolHotWaterFlow;
			ZoneEqSizing( CurZoneEqNum ).MaxCWVolFlow = VentSlab( Item ).MaxVolColdWaterFlow;
		}

		if ( ErrorsFound ) {
			ShowFatalError( "Preceding sizing errors cause program termination" );
		}

	}

	void
	CalcVentilatedSlab(
		int & Item, // number of the current ventilated slab being simulated
		int const ZoneNum, // number of zone being served
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & PowerMet, // power supplied (W)
		Real64 & LatOutputProvided // latent capacity supplied (kg/s)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Young Tae Chae, Rick Strand
		//       DATE WRITTEN   June 2008
		//       MODIFIED       Don Shirey, Aug 2009 (LatOutputProvided)
		//                      July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine mainly controls the action of the Ventilated Slab
		// (or more exactly, it controls the amount of outside air brought in)
		// based on the user input for controls and the defined controls
		// algorithms.

		// METHODOLOGY EMPLOYED:
		// Ventilated slab is controlled based on user input and what is happening in the
		// simulation.  There are various cases to consider:
		// 1. OFF: Unit is schedule off or there is no load on it.  All flow
		//    rates are set to zero and the temperatures are set to zone conditions
		//    (except for the outside air inlet).
		// 2. HEATING/VARIABLE PERCENT: The unit is on, there is a heating load,
		//    and variable percent control is specified.  The outside air fraction
		//    is set to the minimum outside air fraction (schedule based) and the
		//    heating coil is activated.
		// 3. HEATING/FIXED TEMPERATURE: The unit is on, there is a heating load,
		//    and fixed temperature control is specified.  The outside air fraction
		//    is varied in an attempt to obtain a mixed air temperature equal to
		//    the user specified temperature (schedule based).  The heating coil
		//    is activated, if necessary.
		// 4. COOLING/NO COIL: The unit is on, there is a cooling load, and no
		//    coil is present or it has been scheduled off.  Set the amount of
		//    outside air based on the control type.  Simulate the "mixing box".
		// 5. COOLING/WITH COIL: The unit is on, there is a cooling load, and
		//    a cooling coil is present and scheduled on.  Tries to use outside
		//    air as best as possible and then calls a cooling coil
		// Note: controls are strictly temperature based and do not factor
		// humidity into the equation (not an enthalpy economy cycle but rather
		// a simple return air economy cycle).  In addition, temperature predictions
		// are not strict energy balances here in the control routine though
		// in the mixing routine an energy balance is preserved.

		// REFERENCES:
		// ASHRAE Systems and Equipment Handbook (SI), 1996. page 31.3

		// USE STATEMENTS:

		// Using/Aliasing
		using namespace DataZoneEnergyDemands;
		using DataEnvironment::OutDryBulbTemp;
		using DataEnvironment::OutWetBulbTemp;
		using DataEnvironment::EnvironmentName;
		using DataEnvironment::CurMnDy;
		using DataEnvironment::OutBaroPress;
		using DataHeatBalance::MRT;
		using DataHeatBalFanSys::MAT;
		using DataHeatBalFanSys::ZoneAirHumRat;
		using DataHVACGlobals::ZoneCompTurnFansOn;
		using DataHVACGlobals::ZoneCompTurnFansOff;
		using DataLoopNode::Node;
		using ScheduleManager::GetCurrentScheduleValue;
		using HeatingCoils::CheckHeatingCoilSchedule;
		using WaterCoils::CheckWaterCoilSchedule;
		using HVACHXAssistedCoolingCoil::CheckHXAssistedCoolingCoilSchedule;
		using SteamCoils::CheckSteamCoilSchedule;
		using General::TrimSigDigits;
		using Fans::SimulateFanComponents; // 12/18
		using DataHeatBalSurface::TH;
		using NodeInputManager::GetOnlySingleNode;

		// Locals
		Real64 QZnReq;

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		Real64 const LowTempDiff( 0.1 ); // Smallest allowed temperature difference for comparisons
		// (below this value the temperatures are assumed equal)
		Real64 const LowOAFracDiff( 0.01 ); // Smallest allowed outside air fraction difference for comparison
		// (below this value the fractions are assumed equal)

		// INTERFACE BLOCK SPECIFICATIONS

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 AirMassFlow; // air mass flow rate [kg/sec]
		int AirRelNode; // outside air relief node
		int ControlNode; // the hot water or cold water inlet node
		int InletNode; // system air inlet node
		int FanOutletNode; // system fan outlet node
		int ZoneAirInNode; // zone supply air node
		Real64 MaxOAFrac; // maximum possible outside air fraction
		Real64 MaxWaterFlow; // maximum water flow for heating or cooling [kg/sec]
		Real64 MinOAFrac; // minimum possible outside air fraction
		Real64 MinWaterFlow; // minimum water flow for heating or cooling [kg/sec]
		int OutletNode; // air outlet node
		int OutsideAirNode; // outside air node
		int MixoutNode; // oa mixer outlet node
		int ReturnAirNode; // return air node
		Real64 QUnitOut; // heating or sens. cooling provided by fan coil unit [watts]
		Real64 LatentOutput; // Latent (moisture) add/removal rate, negative is dehumidification [kg/s]
		Real64 Tdesired; // desired temperature after mixing inlet and outdoor air [degrees C]
		Real64 Tinlet; // temperature of air coming into the ventilated slab [degrees C]
		Real64 Toutdoor; // temperature of outdoor air being introduced into the ventilated slab [degrees C]
		Real64 MaxSteamFlow;
		Real64 MinSteamFlow;
		Real64 RadInTemp; // "Desired" radiant system air inlet temperature [Celsius]**setpoint
		Real64 SetPointTemp; // temperature that will be used to control the radiant system [Celsius]
		Real64 SetPointTempHi; // Current high point in setpoint temperature range
		Real64 SetPointTempLo; // Current low point in setpoint temperature range
		Real64 AirTempHi; // Current high point in water temperature range
		Real64 AirTempLo; // Current low point in water temperature range
		Real64 AirTempHeatHi; // Current high point in water temperature range
		Real64 AirTempCoolLo; // Current low point in water temperature range
		Real64 CpFan; // Intermediate calculational variable for specific heat of air <<NOV9 Updated
		Real64 ZoneRadNum; // number of zone being served *********************
		int RadSurfNum; // DO loop counter for the surfaces that comprise a particular radiant system
		std::string MSlabIn;
		std::string MSlabOut;
		std::string SlabName;
		int MSlabInletNode;
		int MSlabOutletNode;
		static bool ErrorsFound( false ); // Set to true if errors in input, fatal at end of routine
		static std::string const CurrentModuleObject( "ZoneHVAC:VentilatedSlab" );

		{ auto const SELECT_CASE_var( VentSlab( Item ).CoilOption );
		if ( SELECT_CASE_var == BothOption ) {

			{ auto const SELECT_CASE_var1( VentSlab( Item ).HCoilType );

			if ( SELECT_CASE_var1 == Heating_WaterCoilType ) {
				CheckWaterCoilSchedule( "Coil:Heating:Water", VentSlab( Item ).HCoilName, VentSlab( Item ).HCoilSchedValue, VentSlab( Item ).HCoil_Index );
			} else if ( SELECT_CASE_var1 == Heating_SteamCoilType ) {
				CheckSteamCoilSchedule( "Coil:Heating:Steam", VentSlab( Item ).HCoilName, VentSlab( Item ).HCoilSchedValue, VentSlab( Item ).HCoil_Index );
			} else if ( SELECT_CASE_var1 == Heating_ElectricCoilType ) {
				CheckHeatingCoilSchedule( "Coil:Heating:Electric", VentSlab( Item ).HCoilName, VentSlab( Item ).HCoilSchedValue, VentSlab( Item ).HCoil_Index );
			} else if ( SELECT_CASE_var1 == Heating_GasCoilType ) {
				CheckHeatingCoilSchedule( "Coil:Heating:Gas", VentSlab( Item ).HCoilName, VentSlab( Item ).HCoilSchedValue, VentSlab( Item ).HCoil_Index );
			} else {
			}}

			{ auto const SELECT_CASE_var1( VentSlab( Item ).CCoilType );

			if ( SELECT_CASE_var1 == Cooling_CoilWaterCooling ) {
				CheckWaterCoilSchedule( "Coil:Cooling:Water", VentSlab( Item ).CCoilName, VentSlab( Item ).CCoilSchedValue, VentSlab( Item ).CCoil_Index );
			} else if ( SELECT_CASE_var1 == Cooling_CoilDetailedCooling ) {
				CheckWaterCoilSchedule( "Coil:Cooling:Water:DetailedGeometry", VentSlab( Item ).CCoilName, VentSlab( Item ).CCoilSchedValue, VentSlab( Item ).CCoil_Index );
			} else if ( SELECT_CASE_var1 == Cooling_CoilHXAssisted ) {
				CheckHXAssistedCoolingCoilSchedule( "CoilSystem:Cooling:Water:HeatExchangerAssisted", VentSlab( Item ).CCoilName, VentSlab( Item ).CCoilSchedValue, VentSlab( Item ).CCoil_Index );
			} else {
			}}

		} else if ( SELECT_CASE_var == HeatingOption ) {

			{ auto const SELECT_CASE_var1( VentSlab( Item ).HCoilType );

			if ( SELECT_CASE_var1 == Heating_WaterCoilType ) {
				CheckWaterCoilSchedule( "Coil:Heating:Water", VentSlab( Item ).HCoilName, VentSlab( Item ).HCoilSchedValue, VentSlab( Item ).HCoil_Index );
			} else if ( SELECT_CASE_var1 == Heating_SteamCoilType ) {
				CheckSteamCoilSchedule( "Coil:Heating:Steam", VentSlab( Item ).HCoilName, VentSlab( Item ).HCoilSchedValue, VentSlab( Item ).HCoil_Index );
			} else if ( SELECT_CASE_var1 == Heating_ElectricCoilType ) {
				CheckHeatingCoilSchedule( "Coil:Heating:Electric", VentSlab( Item ).HCoilName, VentSlab( Item ).HCoilSchedValue, VentSlab( Item ).HCoil_Index );
			} else if ( SELECT_CASE_var1 == Heating_GasCoilType ) {
				CheckHeatingCoilSchedule( "Coil:Heating:Gas", VentSlab( Item ).HCoilName, VentSlab( Item ).HCoilSchedValue, VentSlab( Item ).HCoil_Index );
			} else {
			}}

		} else if ( SELECT_CASE_var == CoolingOption ) {

			{ auto const SELECT_CASE_var1( VentSlab( Item ).CCoilType );

			if ( SELECT_CASE_var1 == Cooling_CoilWaterCooling ) {
				CheckWaterCoilSchedule( "Coil:Cooling:Water", VentSlab( Item ).CCoilName, VentSlab( Item ).CCoilSchedValue, VentSlab( Item ).CCoil_Index );
			} else if ( SELECT_CASE_var1 == Cooling_CoilDetailedCooling ) {
				CheckWaterCoilSchedule( "Coil:Cooling:Water:DetailedGeometry", VentSlab( Item ).CCoilName, VentSlab( Item ).CCoilSchedValue, VentSlab( Item ).CCoil_Index );
			} else if ( SELECT_CASE_var1 == Cooling_CoilHXAssisted ) {
				CheckHXAssistedCoolingCoilSchedule( "CoilSystem:Cooling:Water:HeatExchangerAssisted", VentSlab( Item ).CCoilName, VentSlab( Item ).CCoilSchedValue, VentSlab( Item ).CCoil_Index );
			} else {

			}}

		} else if ( SELECT_CASE_var == NoneOption ) {

		}}

		// FLOW:

		FanElecPower = 0.0;
		// initialize local variables
		ControlNode = 0;
		QUnitOut = 0.0;
		LatentOutput = 0.0;
		MaxWaterFlow = 0.0;
		MinWaterFlow = 0.0;
		AirMassFlow = 0.0;
		InletNode = VentSlab( Item ).ReturnAirNode;
		OutletNode = VentSlab( Item ).RadInNode;
		FanOutletNode = VentSlab( Item ).FanOutletNode;
		ZoneAirInNode = VentSlab( Item ).ZoneAirInNode;
		OutsideAirNode = VentSlab( Item ).OutsideAirNode;
		AirRelNode = VentSlab( Item ).AirReliefNode;
		MixoutNode = VentSlab( Item ).OAMixerOutNode;
		ReturnAirNode = VentSlab( Item ).ReturnAirNode;
		ZoneRadNum = VentSlab( Item ).ZonePtr;
		RadSurfNum = VentSlab( Item ).NumOfSurfaces;
		Tinlet = Node( InletNode ).Temp;
		Toutdoor = Node( OutsideAirNode ).Temp;

		// Control Type Check
		{ auto const SELECT_CASE_var( VentSlab( Item ).ControlType );
		if ( SELECT_CASE_var == MATControl ) {
			SetPointTemp = MAT( ZoneNum );
		} else if ( SELECT_CASE_var == MRTControl ) {
			SetPointTemp = MRT( ZoneNum );
		} else if ( SELECT_CASE_var == OPTControl ) {
			SetPointTemp = 0.5 * ( MAT( ZoneNum ) + MRT( ZoneNum ) );
		} else if ( SELECT_CASE_var == ODBControl ) {
			SetPointTemp = OutDryBulbTemp;
		} else if ( SELECT_CASE_var == OWBControl ) {
			SetPointTemp = OutWetBulbTemp;
		} else if ( SELECT_CASE_var == SURControl ) {
			SetPointTemp = TH( 2, 1, VentSlab( Item ).SurfacePtr( RadSurfNum ) );
		} else if ( SELECT_CASE_var == DPTZControl ) {
			SetPointTemp = PsyTdpFnWPb( ZoneAirHumRat( VentSlab( Item ).ZonePtr ), OutBaroPress );
		} else { // Should never get here
			SetPointTemp = 0.0; // Suppress uninitialized warning
			ShowSevereError( "Illegal control type in low temperature radiant system: " + VentSlab( Item ).Name );
			ShowFatalError( "Preceding condition causes termination." );
		}}

		// Load Check

		AirTempHeatHi = GetCurrentScheduleValue( VentSlab( Item ).HotCtrlHiTempSchedPtr );
		AirTempCoolLo = GetCurrentScheduleValue( VentSlab( Item ).ColdCtrlLoTempSchedPtr );

		if ( ( ( SetPointTemp >= AirTempHeatHi ) && ( SetPointTemp <= AirTempCoolLo ) ) || ( GetCurrentScheduleValue( VentSlab( Item ).SchedPtr ) <= 0 ) ) {

			// System is off or has no load upon it; set the flow rates to zero and then
			// simulate the components with the no flow conditions
			Node( InletNode ).MassFlowRate = 0.0;
			Node( InletNode ).MassFlowRateMaxAvail = 0.0;
			Node( InletNode ).MassFlowRateMinAvail = 0.0;
			Node( OutletNode ).MassFlowRate = 0.0;
			Node( OutletNode ).MassFlowRateMaxAvail = 0.0;
			Node( OutletNode ).MassFlowRateMinAvail = 0.0;
			Node( OutsideAirNode ).MassFlowRate = 0.0;
			Node( OutsideAirNode ).MassFlowRateMaxAvail = 0.0;
			Node( OutsideAirNode ).MassFlowRateMinAvail = 0.0;
			Node( AirRelNode ).MassFlowRate = 0.0;
			Node( AirRelNode ).MassFlowRateMaxAvail = 0.0;
			Node( AirRelNode ).MassFlowRateMinAvail = 0.0;
			Node( ReturnAirNode ).MassFlowRate = 0.0;
			Node( ReturnAirNode ).MassFlowRateMaxAvail = 0.0;
			Node( ReturnAirNode ).MassFlowRateMinAvail = 0.0;
			Node( MixoutNode ).MassFlowRate = 0.0;
			Node( MixoutNode ).MassFlowRateMaxAvail = 0.0;
			Node( MixoutNode ).MassFlowRateMinAvail = 0.0;
			Node( FanOutletNode ).MassFlowRate = 0.0;
			Node( FanOutletNode ).MassFlowRateMaxAvail = 0.0;
			Node( FanOutletNode ).MassFlowRateMinAvail = 0.0;
			AirMassFlow = 0.0;
			HCoilOn = false;

			// Node condition
			Node( InletNode ).Temp = TH( 2, 1, VentSlab( Item ).SurfacePtr( 1 ) );
			Node( FanOutletNode ).Temp = Node( InletNode ).Temp;
			Node( OutletNode ).Temp = Node( FanOutletNode ).Temp;

			// Node condition
			if ( VentSlab( Item ).SysConfg == SeriesSlabs ) {
				for ( RadSurfNum = 1; RadSurfNum <= VentSlab( Item ).NumOfSurfaces; ++RadSurfNum ) {
					SlabName = VentSlab( Item ).SurfaceName( RadSurfNum );
					MSlabIn = VentSlab( Item ).SlabIn( RadSurfNum );
					MSlabOut = VentSlab( Item ).SlabOut( RadSurfNum );
					VentSlab( Item ).MSlabInNode = GetOnlySingleNode( MSlabIn, ErrorsFound, CurrentModuleObject, SlabName, NodeType_Air, NodeConnectionType_Internal, 1, ObjectIsNotParent );
					VentSlab( Item ).MSlabOutNode = GetOnlySingleNode( MSlabOut, ErrorsFound, CurrentModuleObject, SlabName, NodeType_Air, NodeConnectionType_Internal, 1, ObjectIsNotParent );
					MSlabInletNode = VentSlab( Item ).MSlabInNode;
					MSlabOutletNode = VentSlab( Item ).MSlabOutNode;

					Node( MSlabInletNode ).Temp = Node( InletNode ).Temp;
					Node( MSlabOutletNode ).Temp = Node( MSlabInletNode ).Temp;
				}
			}

			CalcVentilatedSlabComps( Item, FirstHVACIteration, QUnitOut );

		} else { // System On

			if ( SetPointTemp < AirTempHeatHi ) { // HEATING MODE
				OperatingMode = HeatingMode;

				//Check the setpoint and temperature span
				SetPointTempHi = GetCurrentScheduleValue( VentSlab( Item ).HotCtrlHiTempSchedPtr );
				SetPointTempLo = GetCurrentScheduleValue( VentSlab( Item ).HotCtrlLoTempSchedPtr );
				if ( SetPointTempHi < SetPointTempLo ) {
					ShowSevereError( "Heating setpoint temperature mismatch in" + VentSlab( Item ).Name );
					ShowContinueError( "High setpoint temperature is less than low setpoint temperature--check your schedule input" );
					ShowFatalError( "Preceding condition causes termination." );
				}
				AirTempHi = GetCurrentScheduleValue( VentSlab( Item ).HotAirHiTempSchedPtr );
				AirTempLo = GetCurrentScheduleValue( VentSlab( Item ).HotAirLoTempSchedPtr );

				if ( AirTempHi < AirTempLo ) {
					ShowSevereError( "Heating Air temperature mismatch in" + VentSlab( Item ).Name );
					ShowContinueError( "High Air temperature is less than low Air temperature--check your schedule input" );
					ShowFatalError( "Preceding condition causes termination." );
				}

				if ( SetPointTemp >= SetPointTempHi ) {
					// System is above high heating setpoint so we should be able to turn the system off
					RadInTemp = AirTempLo;

				} else if ( SetPointTemp <= SetPointTempLo ) {
					// System is running with its highest inlet temperature
					RadInTemp = AirTempHi;
				} else {
					// Interpolate to obtain the current radiant system inlet temperature
					RadInTemp = AirTempHi - ( AirTempHi - AirTempLo ) * ( SetPointTemp - SetPointTempLo ) / ( SetPointTempHi - SetPointTempLo );
				}

				Node( VentSlab( Item ).RadInNode ).Temp = RadInTemp;

				ControlNode = VentSlab( Item ).HotControlNode;
				MaxWaterFlow = VentSlab( Item ).MaxHotWaterFlow;
				MinWaterFlow = VentSlab( Item ).MinHotWaterFlow;
				MaxSteamFlow = VentSlab( Item ).MaxHotSteamFlow;
				MinSteamFlow = VentSlab( Item ).MinHotSteamFlow;

				// On the first HVAC iteration the system values are given to the controller, but after that
				// the demand limits are in place and there needs to be feedback to the Zone Equipment

				if ( ! FirstHVACIteration && VentSlab( Item ).HCoilType == Heating_WaterCoilType ) {
					MaxWaterFlow = Node( ControlNode ).MassFlowRateMaxAvail;
					MinWaterFlow = Node( ControlNode ).MassFlowRateMinAvail;
				}

				if ( ! FirstHVACIteration && VentSlab( Item ).HCoilType == Heating_SteamCoilType ) {
					MaxSteamFlow = Node( ControlNode ).MassFlowRateMaxAvail;
					MinSteamFlow = Node( ControlNode ).MassFlowRateMinAvail;
				}

				HCoilOn = true;

				if ( Node( OutsideAirNode ).MassFlowRate > 0.0 ) {
					MinOAFrac = GetCurrentScheduleValue( VentSlab( Item ).MinOASchedPtr ) * ( VentSlab( Item ).MinOutAirMassFlow / Node( OutsideAirNode ).MassFlowRate );
				} else {
					MinOAFrac = 0.0;
				}

				MinOAFrac = min( 1.0, max( 0.0, MinOAFrac ) );

				if ( ( ! VentSlab( Item ).HCoilPresent ) || ( VentSlab( Item ).HCoilSchedValue <= 0.0 ) ) {
					// In heating mode, but there is no coil to provide heating.  This is handled
					// differently than if there was a heating coil present.  Fixed temperature
					// will still try to vary the amount of outside air to meet the desired
					// mixed air temperature, while variable percent will go to full ventilation
					// when it is most advantageous.

					// If there are no coil, Slab In Node is assumed to be Fan Outlet Node

					OutletNode = FanOutletNode;

					{ auto const SELECT_CASE_var( VentSlab( Item ).OAControlType );

					if ( SELECT_CASE_var == FixedOAControl ) {
						// In this control type, the outdoor air flow rate is fixed to the maximum value
						// which is equal to the minimum value, regardless of all the other conditions.
						OAMassFlowRate = MinOAFrac * Node( OutsideAirNode ).MassFlowRate;

					} else if ( SELECT_CASE_var == VariablePercent ) {
						// This algorithm is probably a bit simplistic in that it just bounces
						// back and forth between the maximum outside air and the minimum.  In
						// reality, a system *might* vary between the two based on the load in
						// the zone.  This simple flow control might cause some overcooling but
						// chances are that if there is a cooling load and the zone temperature
						// gets above the outside temperature that overcooling won't be significant.
						Tinlet = Node( InletNode ).Temp;
						Toutdoor = Node( OutsideAirNode ).Temp;

						if ( Tinlet >= Toutdoor ) {

							OAMassFlowRate = MinOAFrac * Node( OutsideAirNode ).MassFlowRate;

						} else { // Tinlet < Toutdoor

							MaxOAFrac = GetCurrentScheduleValue( VentSlab( Item ).MaxOASchedPtr );
							OAMassFlowRate = MaxOAFrac * Node( OutsideAirNode ).MassFlowRate;

						}

					} else if ( SELECT_CASE_var == FixedTemperature ) {
						// This is basically the same algorithm as for the heating case...
						Tdesired = GetCurrentScheduleValue( VentSlab( Item ).TempSchedPtr );
						MaxOAFrac = 1.0;

						if ( std::abs( Tinlet - Toutdoor ) <= LowTempDiff ) { // no difference in indoor and outdoor conditions-->set OA to minimum
							OAMassFlowRate = MinOAFrac * Node( OutsideAirNode ).MassFlowRate;
						} else if ( std::abs( MaxOAFrac - MinOAFrac ) <= LowOAFracDiff ) { // no difference in outside air fractions
							OAMassFlowRate = MinOAFrac * Node( OutsideAirNode ).MassFlowRate;
						} else if ( ( ( Tdesired <= Tinlet ) && ( Tdesired >= Toutdoor ) ) || ( ( Tdesired >= Tinlet ) && ( Tdesired <= Toutdoor ) ) ) {
							// Desired temperature is between the inlet and outdoor temperatures
							// so vary the flow rate between no outside air and no recirculation air
							// then applying the maximum and minimum limits the user has scheduled
							// to make sure too much/little outside air is being introduced
							OAMassFlowRate = ( ( Tdesired - Tinlet ) / ( Toutdoor - Tinlet ) ) * Node( InletNode ).MassFlowRate;
							OAMassFlowRate = max( OAMassFlowRate, ( MinOAFrac * Node( OutsideAirNode ).MassFlowRate ) );
							OAMassFlowRate = min( OAMassFlowRate, ( MaxOAFrac * Node( OutsideAirNode ).MassFlowRate ) );
						} else if ( ( Tdesired < Tinlet ) && ( Tdesired < Toutdoor ) ) {
							// Desired temperature is below both the inlet and outdoor temperatures
							// so use whichever flow rate (max or min) that will get closer
							if ( Tinlet < Toutdoor ) { // Tinlet closer to Tdesired so use minimum outside air
								OAMassFlowRate = MinOAFrac * Node( OutsideAirNode ).MassFlowRate;
							} else { // Toutdoor closer to Tdesired so use maximum outside air
								OAMassFlowRate = MaxOAFrac * Node( OutsideAirNode ).MassFlowRate;
							}
						} else if ( ( Tdesired > Tinlet ) && ( Tdesired > Toutdoor ) ) {
							// Desired temperature is above both the inlet and outdoor temperatures
							// so use whichever flow rate (max or min) that will get closer
							if ( Tinlet > Toutdoor ) { // Tinlet closer to Tdesired so use minimum outside air
								OAMassFlowRate = MinOAFrac * Node( OutsideAirNode ).MassFlowRate;
							} else { // Toutdoor closer to Tdesired so use maximum outside air
								OAMassFlowRate = MaxOAFrac * Node( OutsideAirNode ).MassFlowRate;
							}
						} else {
							// It should NEVER get to this point, but just in case...
							ShowFatalError( "Ventilated Slab simulation control: illogical condition for " + VentSlab( Item ).Name );
						}

					}}

					CalcVentilatedSlabComps( Item, FirstHVACIteration, QUnitOut );

				} else { // Heating Coil present

					{ auto const SELECT_CASE_var( VentSlab( Item ).OAControlType );

					if ( SELECT_CASE_var == FixedOAControl ) {
						// In this control type, the outdoor air flow rate is fixed to the maximum value
						// which is equal to the minimum value, regardless of all the other conditions.
						if ( Node( OutsideAirNode ).MassFlowRate > 0.0 ) {
							MaxOAFrac = GetCurrentScheduleValue( VentSlab( Item ).MaxOASchedPtr );
						} else {
							MaxOAFrac = 0.0;
						}
						MaxOAFrac = min( 1.0, max( 0.0, MinOAFrac ) );
						OAMassFlowRate = MaxOAFrac * Node( OutsideAirNode ).MassFlowRate;

					} else if ( SELECT_CASE_var == VariablePercent ) {
						// In heating mode, the ouside air for "variable percent" control
						// is set to the minimum value

						OAMassFlowRate = MinOAFrac * Node( OutsideAirNode ).MassFlowRate;

					} else if ( SELECT_CASE_var == FixedTemperature ) {
						// This is basically the same algorithm as for the heating case...
						Tdesired = GetCurrentScheduleValue( VentSlab( Item ).TempSchedPtr );
						MaxOAFrac = 1.0;

						if ( std::abs( Tinlet - Toutdoor ) <= LowTempDiff ) { // no difference in indoor and outdoor conditions-->set OA to minimum
							OAMassFlowRate = MinOAFrac * Node( OutsideAirNode ).MassFlowRate;
						} else if ( std::abs( MaxOAFrac - MinOAFrac ) <= LowOAFracDiff ) { // no difference in outside air fractions
							OAMassFlowRate = MinOAFrac * Node( OutsideAirNode ).MassFlowRate;
						} else if ( ( ( Tdesired <= Tinlet ) && ( Tdesired >= Toutdoor ) ) || ( ( Tdesired >= Tinlet ) && ( Tdesired <= Toutdoor ) ) ) {
							// Desired temperature is between the inlet and outdoor temperatures
							// so vary the flow rate between no outside air and no recirculation air
							// then applying the maximum and minimum limits the user has scheduled
							// to make sure too much/little outside air is being introduced
							OAMassFlowRate = ( ( Tdesired - Tinlet ) / ( Toutdoor - Tinlet ) ) * Node( InletNode ).MassFlowRate;
							OAMassFlowRate = max( OAMassFlowRate, ( MinOAFrac * Node( OutsideAirNode ).MassFlowRate ) );
							OAMassFlowRate = min( OAMassFlowRate, ( MaxOAFrac * Node( OutsideAirNode ).MassFlowRate ) );
						} else if ( ( Tdesired < Tinlet ) && ( Tdesired < Toutdoor ) ) {
							// Desired temperature is below both the inlet and outdoor temperatures
							// so use whichever flow rate (max or min) that will get closer
							if ( Tinlet < Toutdoor ) { // Tinlet closer to Tdesired so use minimum outside air
								OAMassFlowRate = MinOAFrac * Node( OutsideAirNode ).MassFlowRate;
							} else { // Toutdoor closer to Tdesired so use maximum outside air
								OAMassFlowRate = MaxOAFrac * Node( OutsideAirNode ).MassFlowRate;
							}
						} else if ( ( Tdesired > Tinlet ) && ( Tdesired > Toutdoor ) ) {
							// Desired temperature is above both the inlet and outdoor temperatures
							// so use whichever flow rate (max or min) that will get closer
							if ( Tinlet > Toutdoor ) { // Tinlet closer to Tdesired so use minimum outside air
								OAMassFlowRate = MinOAFrac * Node( OutsideAirNode ).MassFlowRate;
							} else { // Toutdoor closer to Tdesired so use maximum outside air
								OAMassFlowRate = MaxOAFrac * Node( OutsideAirNode ).MassFlowRate;
							}
						} else {
							// It should NEVER get to this point, but just in case...
							ShowFatalError( "Ventilated Slab simulation control: illogical condition for " + VentSlab( Item ).Name );
						}

					}}

					SimVentSlabOAMixer( Item );
					SimulateFanComponents( VentSlab( Item ).FanName, FirstHVACIteration, VentSlab( Item ).Fan_Index, _, ZoneCompTurnFansOn, ZoneCompTurnFansOff );
					CpFan = PsyCpAirFnWTdb( Node( FanOutletNode ).HumRat, Node( FanOutletNode ).Temp );
					QZnReq = ( Node( OutletNode ).MassFlowRate ) * CpFan * ( RadInTemp - Node( FanOutletNode ).Temp );

					// Setup the coil configuration
					{ auto const SELECT_CASE_var( VentSlab( Item ).HCoilType );

					if ( SELECT_CASE_var == Heating_WaterCoilType ) {
						// control water flow to obtain output matching QZnReq

						ControlCompOutput( VentSlab( Item ).Name, cMO_VentilatedSlab, Item, FirstHVACIteration, QZnReq, ControlNode, MaxWaterFlow, MinWaterFlow, 0.001, VentSlab( Item ).ControlCompTypeNum, VentSlab( Item ).CompErrIndex, _, _, _, _, _, VentSlab( Item ).HWLoopNum, VentSlab( Item ).HWLoopSide, VentSlab( Item ).HWBranchNum );

					} else if ( ( SELECT_CASE_var == Heating_GasCoilType ) || ( SELECT_CASE_var == Heating_ElectricCoilType ) || ( SELECT_CASE_var == Heating_SteamCoilType ) ) {

						CalcVentilatedSlabComps( Item, FirstHVACIteration, QUnitOut );

					}}

				} //  Coil/no coil block

			} else if ( SetPointTemp > AirTempCoolLo ) { // Cooling Mode

				OperatingMode = CoolingMode;

				SetPointTempHi = GetCurrentScheduleValue( VentSlab( Item ).ColdCtrlHiTempSchedPtr );
				SetPointTempLo = GetCurrentScheduleValue( VentSlab( Item ).ColdCtrlLoTempSchedPtr );
				if ( SetPointTempHi < SetPointTempLo ) {
					ShowSevereError( "Cooling setpoint temperature mismatch in" + VentSlab( Item ).Name );
					ShowContinueError( "High setpoint temperature is less than low setpoint temperature--check your schedule input" );
					ShowFatalError( "Preceding condition causes termination." );
				}

				AirTempHi = GetCurrentScheduleValue( VentSlab( Item ).ColdAirHiTempSchedPtr );
				AirTempLo = GetCurrentScheduleValue( VentSlab( Item ).ColdAirLoTempSchedPtr );
				if ( AirTempHi < AirTempLo ) {
					ShowSevereError( "Cooling Air temperature mismatch in" + VentSlab( Item ).Name );
					ShowContinueError( "High Air temperature is less than low Air temperature--check your schedule input" );
					ShowFatalError( "Preceding condition causes termination." );
				}

				if ( SetPointTemp <= SetPointTempLo ) {
					// System is below low cooling setpoint so we should be able to turn the system off
					RadInTemp = AirTempHi;
				} else if ( SetPointTemp >= SetPointTempHi ) {
					// System is running with its lowest inlet temperature
					RadInTemp = AirTempLo;
				} else {
					// Interpolate to obtain the current radiant system inlet temperature
					RadInTemp = AirTempHi - ( AirTempHi - AirTempLo ) * ( SetPointTemp - SetPointTempLo ) / ( SetPointTempHi - SetPointTempLo );

				}

				ControlNode = VentSlab( Item ).ColdControlNode;
				MaxWaterFlow = VentSlab( Item ).MaxColdWaterFlow;
				MinWaterFlow = VentSlab( Item ).MinColdWaterFlow;

				//On the first HVAC iteration the system values are given to the controller, but after that
				// the demand limits are in place and there needs to be feedback to the Zone Equipment
				if ( ( ! FirstHVACIteration ) && ( ControlNode > 0 ) && ( VentSlab( Item ).CCoilPresent ) ) {
					MaxWaterFlow = Node( ControlNode ).MassFlowRateMaxAvail;
					MinWaterFlow = Node( ControlNode ).MassFlowRateMinAvail;
				}
				HCoilOn = false;

				if ( Node( OutsideAirNode ).MassFlowRate > 0.0 ) {
					MinOAFrac = GetCurrentScheduleValue( VentSlab( Item ).MinOASchedPtr ) * ( VentSlab( Item ).MinOutAirMassFlow / Node( OutsideAirNode ).MassFlowRate );
				} else {
					MinOAFrac = 0.0;
				}
				MinOAFrac = min( 1.0, max( 0.0, MinOAFrac ) );

				if ( ( ! VentSlab( Item ).CCoilPresent ) || ( VentSlab( Item ).CCoilSchedValue <= 0.0 ) ) {
					// In cooling mode, but there is no coil to provide cooling.  This is handled
					// differently than if there was a cooling coil present.  Fixed temperature
					// will still try to vary the amount of outside air to meet the desired
					// mixed air temperature, while variable percent will go to full ventilation
					// when it is most advantageous.

					// If there are no coil, Slab In Node is assumed to be Fan Outlet Node
					OutletNode = FanOutletNode;

					{ auto const SELECT_CASE_var( VentSlab( Item ).OAControlType );

					if ( SELECT_CASE_var == FixedOAControl ) {
						// In this control type, the outdoor air flow rate is fixed to the maximum value
						// which is equal to the minimum value, regardless of all the other conditions.
						if ( Node( OutsideAirNode ).MassFlowRate > 0.0 ) {
							MaxOAFrac = GetCurrentScheduleValue( VentSlab( Item ).MaxOASchedPtr );
						} else {
							MaxOAFrac = 0.0;
						}
						MaxOAFrac = min( 1.0, max( 0.0, MinOAFrac ) );
						OAMassFlowRate = MaxOAFrac * Node( OutsideAirNode ).MassFlowRate;

					} else if ( SELECT_CASE_var == VariablePercent ) {
						// This algorithm is probably a bit simplistic in that it just bounces
						// back and forth between the maximum outside air and the minimum.  In
						// reality, a system *might* vary between the two based on the load in
						// the zone.  This simple flow control might cause some overcooling but
						// chances are that if there is a cooling load and the zone temperature
						// gets above the outside temperature that overcooling won't be significant.

						Tinlet = Node( InletNode ).Temp;
						Toutdoor = Node( OutsideAirNode ).Temp;

						if ( Tinlet <= Toutdoor ) {

							OAMassFlowRate = MinOAFrac * Node( OutsideAirNode ).MassFlowRate;

						} else { // Tinlet > Toutdoor

							MaxOAFrac = GetCurrentScheduleValue( VentSlab( Item ).MaxOASchedPtr );
							OAMassFlowRate = MaxOAFrac * Node( OutsideAirNode ).MassFlowRate;

						}

					} else if ( SELECT_CASE_var == FixedTemperature ) {
						// This is basically the same algorithm as for the heating case...
						Tdesired = GetCurrentScheduleValue( VentSlab( Item ).TempSchedPtr );
						MaxOAFrac = 1.0;

						if ( std::abs( Tinlet - Toutdoor ) <= LowTempDiff ) { // no difference in indoor and outdoor conditions-->set OA to minimum
							OAMassFlowRate = MinOAFrac * Node( OutsideAirNode ).MassFlowRate;
						} else if ( std::abs( MaxOAFrac - MinOAFrac ) <= LowOAFracDiff ) { // no difference in outside air fractions
							OAMassFlowRate = MinOAFrac * Node( OutsideAirNode ).MassFlowRate;
						} else if ( ( ( Tdesired <= Tinlet ) && ( Tdesired >= Toutdoor ) ) || ( ( Tdesired >= Tinlet ) && ( Tdesired <= Toutdoor ) ) ) {
							// Desired temperature is between the inlet and outdoor temperatures
							// so vary the flow rate between no outside air and no recirculation air
							// then applying the maximum and minimum limits the user has scheduled
							// to make sure too much/little outside air is being introduced
							OAMassFlowRate = ( ( Tdesired - Tinlet ) / ( Toutdoor - Tinlet ) ) * Node( InletNode ).MassFlowRate;
							OAMassFlowRate = max( OAMassFlowRate, ( MinOAFrac * Node( OutsideAirNode ).MassFlowRate ) );
							OAMassFlowRate = min( OAMassFlowRate, ( MaxOAFrac * Node( OutsideAirNode ).MassFlowRate ) );
						} else if ( ( Tdesired < Tinlet ) && ( Tdesired < Toutdoor ) ) {
							// Desired temperature is below both the inlet and outdoor temperatures
							// so use whichever flow rate (max or min) that will get closer
							if ( Tinlet < Toutdoor ) { // Tinlet closer to Tdesired so use minimum outside air
								OAMassFlowRate = MinOAFrac * Node( OutsideAirNode ).MassFlowRate;
							} else { // Toutdoor closer to Tdesired so use maximum outside air
								OAMassFlowRate = MaxOAFrac * Node( OutsideAirNode ).MassFlowRate;
							}
						} else if ( ( Tdesired > Tinlet ) && ( Tdesired > Toutdoor ) ) {
							// Desired temperature is above both the inlet and outdoor temperatures
							// so use whichever flow rate (max or min) that will get closer
							if ( Tinlet > Toutdoor ) { // Tinlet closer to Tdesired so use minimum outside air
								OAMassFlowRate = MinOAFrac * Node( OutsideAirNode ).MassFlowRate;
							} else { // Toutdoor closer to Tdesired so use maximum outside air
								OAMassFlowRate = MaxOAFrac * Node( OutsideAirNode ).MassFlowRate;
							}
						} else {
							// It should NEVER get to this point, but just in case...
							ShowFatalError( cMO_VentilatedSlab + " simulation control: illogical condition for " + VentSlab( Item ).Name );
						}

					}}

					CalcVentilatedSlabComps( Item, FirstHVACIteration, QUnitOut );

				} else {
					// There is a cooling load and there is a cooling coil present (presumably).
					// Variable percent will throttle outside air back to the minimum while
					// fixed temperature will still try to vary the outside air amount to meet
					// the desired mixed air temperature.

					{ auto const SELECT_CASE_var( VentSlab( Item ).OAControlType );

					if ( SELECT_CASE_var == FixedOAControl ) {
						// In this control type, the outdoor air flow rate is fixed to the maximum value
						// which is equal to the minimum value, regardless of all the other conditions.
						if ( Node( OutsideAirNode ).MassFlowRate > 0.0 ) {
							MaxOAFrac = GetCurrentScheduleValue( VentSlab( Item ).MaxOASchedPtr );
						} else {
							MaxOAFrac = 0.0;
						}
						MaxOAFrac = min( 1.0, max( 0.0, MinOAFrac ) );
						OAMassFlowRate = MaxOAFrac * Node( OutsideAirNode ).MassFlowRate;

					} else if ( SELECT_CASE_var == VariablePercent ) {
						// A cooling coil is present so let it try to do the cooling...
						OAMassFlowRate = MinOAFrac * Node( OutsideAirNode ).MassFlowRate;

					} else if ( SELECT_CASE_var == FixedTemperature ) {
						// This is basically the same algorithm as for the heating case...
						Tdesired = GetCurrentScheduleValue( VentSlab( Item ).TempSchedPtr );

						MaxOAFrac = 1.0;

						if ( std::abs( Tinlet - Toutdoor ) <= LowTempDiff ) { // no difference in indoor and outdoor conditions-->set OA to minimum
							OAMassFlowRate = MinOAFrac * Node( OutsideAirNode ).MassFlowRate;
						} else if ( std::abs( MaxOAFrac - MinOAFrac ) <= LowOAFracDiff ) { // no difference in outside air fractions
							OAMassFlowRate = MinOAFrac * Node( OutsideAirNode ).MassFlowRate;
						} else if ( ( ( Tdesired <= Tinlet ) && ( Tdesired >= Toutdoor ) ) || ( ( Tdesired >= Tinlet ) && ( Tdesired <= Toutdoor ) ) ) {
							// Desired temperature is between the inlet and outdoor temperatures
							// so vary the flow rate between no outside air and no recirculation air
							// then applying the maximum and minimum limits the user has scheduled
							// to make sure too much/little outside air is being introduced
							OAMassFlowRate = ( ( Tdesired - Tinlet ) / ( Toutdoor - Tinlet ) ) * Node( InletNode ).MassFlowRate;
							OAMassFlowRate = max( OAMassFlowRate, ( MinOAFrac * Node( OutsideAirNode ).MassFlowRate ) );
							OAMassFlowRate = min( OAMassFlowRate, ( MaxOAFrac * Node( OutsideAirNode ).MassFlowRate ) );
						} else if ( ( Tdesired < Tinlet ) && ( Tdesired < Toutdoor ) ) {
							// Desired temperature is below both the inlet and outdoor temperatures
							// so use whichever flow rate (max or min) that will get closer
							if ( Tinlet < Toutdoor ) { // Tinlet closer to Tdesired so use minimum outside air
								OAMassFlowRate = MinOAFrac * Node( OutsideAirNode ).MassFlowRate;
							} else { // Toutdoor closer to Tdesired so use maximum outside air
								OAMassFlowRate = MaxOAFrac * Node( OutsideAirNode ).MassFlowRate;
							}
						} else if ( ( Tdesired > Tinlet ) && ( Tdesired > Toutdoor ) ) {
							// Desired temperature is above both the inlet and outdoor temperatures
							// so use whichever flow rate (max or min) that will get closer
							if ( Tinlet > Toutdoor ) { // Tinlet closer to Tdesired so use minimum outside air
								OAMassFlowRate = MinOAFrac * Node( OutsideAirNode ).MassFlowRate;
							} else { // Toutdoor closer to Tdesired so use maximum outside air
								OAMassFlowRate = MaxOAFrac * Node( OutsideAirNode ).MassFlowRate;
							}
						} else {
							// It should NEVER get to this point, but just in case...
							ShowFatalError( cMO_VentilatedSlab + " simulation control: illogical condition for " + VentSlab( Item ).Name );
						}

					}}

					// control water flow to obtain output matching Low Setpoint Temperateure
					HCoilOn = false;

					SimVentSlabOAMixer( Item );
					SimulateFanComponents( VentSlab( Item ).FanName, FirstHVACIteration, VentSlab( Item ).Fan_Index, _, ZoneCompTurnFansOn, ZoneCompTurnFansOff );

					CpFan = PsyCpAirFnWTdb( Node( FanOutletNode ).HumRat, Node( FanOutletNode ).Temp );
					QZnReq = ( Node( OutletNode ).MassFlowRate ) * CpFan * ( RadInTemp - Node( FanOutletNode ).Temp );

					ControlCompOutput( VentSlab( Item ).Name, cMO_VentilatedSlab, Item, FirstHVACIteration, QZnReq, ControlNode, MaxWaterFlow, MinWaterFlow, 0.001, VentSlab( Item ).ControlCompTypeNum, VentSlab( Item ).CompErrIndex, _, _, _, _, _, VentSlab( Item ).CWLoopNum, VentSlab( Item ).CWLoopSide, VentSlab( Item ).CWBranchNum );

				}

			} // ...end of HEATING/COOLING IF-THEN block

			CalcVentilatedSlabRadComps( Item, FirstHVACIteration );

		} // ...end of system ON/OFF IF-THEN block

		// Resimulate fans if AirMassFlow is zero and FanElecPower is > 0, indicating that load or condensation controls shut off the ventilated slab in CalcVentilatedSlabRadComps
		AirMassFlow = Node( OutletNode ).MassFlowRate;
		if ( ( AirMassFlow <= 0.0 ) && ( FanElecPower > 0.0 ) ) {
			Node( MixoutNode ).MassFlowRate = 0.0;
			Node( MixoutNode ).MassFlowRateMaxAvail = 0.0;
			Node( MixoutNode ).MassFlowRateMinAvail = 0.0;
			Node( FanOutletNode ).MassFlowRate = 0.0;
			Node( FanOutletNode ).MassFlowRateMaxAvail = 0.0;
			Node( FanOutletNode ).MassFlowRateMinAvail = 0.0;
			SimulateFanComponents( VentSlab( Item ).FanName, FirstHVACIteration, VentSlab( Item ).Fan_Index, _, ZoneCompTurnFansOn, ZoneCompTurnFansOff );
		}

		CalcVentilatedSlabCoilOutput( Item, PowerMet, LatOutputProvided );

	}

	void
	CalcVentilatedSlabComps(
		int const Item, // system index in ventilated slab array
		bool const FirstHVACIteration, // flag for 1st HVAV iteration in the time step
		Real64 & LoadMet // load met by the system (watts)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Young Tae Chae, Rick Strand
		//       DATE WRITTEN   June 2008
		//       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine launches the individual component simulations.
		// This is called either when the system is off to carry null conditions
		// through the system or during control iterations to continue updating
		// what is going on within the unit.

		// METHODOLOGY EMPLOYED:
		// Simply calls the different components in order.  Only slight wrinkles
		// here are that the ventilatd slab system has it's own outside air mixed and
		// that a cooling coil must be present in order to call a cooling coil
		// simulation.  Other than that, the subroutine is very straightforward.

		// REFERENCES:
		// na

		// Using/Aliasing
		using Fans::SimulateFanComponents;
		using HeatingCoils::SimulateHeatingCoilComponents;
		using WaterCoils::SimulateWaterCoilComponents;
		using HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil;
		using SteamCoils::SimulateSteamCoilComponents;
		using DataHVACGlobals::ZoneCompTurnFansOn;
		using DataHVACGlobals::ZoneCompTurnFansOff;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 AirMassFlow; // total mass flow through the system
		Real64 CpAirZn; // specific heat of dry air at zone conditions (zone conditions same as system inlet)
		int HCoilInAirNode; // inlet node number for fan exit/coil inlet
		int InletNode; // system air inlet node
		int OutletNode; // system air outlet node
		//unused0309  INTEGER        :: HCoilOutAirNode
		Real64 QCoilReq; // Heat addition required from an electric/gas heating coil
		Real64 HCoilOutAirTemp;
		Real64 HCoilInAirTemp;
		//unused1208  REAL(r64)           :: RadInTemp       ! Set temperature for "Slab In Node"

		// FLOW:

		SimVentSlabOAMixer( Item );
		SimulateFanComponents( VentSlab( Item ).FanName, FirstHVACIteration, VentSlab( Item ).Fan_Index, _, ZoneCompTurnFansOn, ZoneCompTurnFansOff );
		if ( ( VentSlab( Item ).CCoilPresent ) && ( VentSlab( Item ).CCoilSchedValue >= 0.0 ) ) {
			if ( VentSlab( Item ).CCoilType == Cooling_CoilHXAssisted ) {
				SimHXAssistedCoolingCoil( VentSlab( Item ).CCoilName, FirstHVACIteration, On, 0.0, VentSlab( Item ).CCoil_Index, ContFanCycCoil );
			} else {
				SimulateWaterCoilComponents( VentSlab( Item ).CCoilName, FirstHVACIteration, VentSlab( Item ).CCoil_Index );
			}

		}

		if ( ( VentSlab( Item ).HCoilPresent ) && ( VentSlab( Item ).HCoilSchedValue >= 0.0 ) ) {

			{ auto const SELECT_CASE_var( VentSlab( Item ).HCoilType );

			if ( SELECT_CASE_var == Heating_WaterCoilType ) {

				SimulateWaterCoilComponents( VentSlab( Item ).HCoilName, FirstHVACIteration, VentSlab( Item ).HCoil_Index );

			} else if ( SELECT_CASE_var == Heating_SteamCoilType ) {

				if ( ! HCoilOn ) {
					QCoilReq = 0.0;
				} else {
					HCoilInAirNode = VentSlab( Item ).FanOutletNode;
					CpAirZn = PsyCpAirFnWTdb( Node( HCoilInAirNode ).HumRat, Node( HCoilInAirNode ).Temp );
					QCoilReq = Node( HCoilInAirNode ).MassFlowRate * CpAirZn * ( Node( VentSlab( Item ).RadInNode ).Temp ) - ( Node( HCoilInAirNode ).Temp );
				}

				if ( QCoilReq < 0.0 ) QCoilReq = 0.0; // a heating coil can only heat, not cool

				SimulateSteamCoilComponents( VentSlab( Item ).HCoilName, FirstHVACIteration, VentSlab( Item ).HCoil_Index, QCoilReq );

			} else if ( ( SELECT_CASE_var == Heating_ElectricCoilType ) || ( SELECT_CASE_var == Heating_GasCoilType ) ) {

				if ( ! HCoilOn ) {
					QCoilReq = 0.0;
				} else {
					HCoilInAirTemp = Node( VentSlab( Item ).FanOutletNode ).Temp;
					HCoilOutAirTemp = Node( VentSlab( Item ).RadInNode ).Temp;
					CpAirZn = PsyCpAirFnWTdb( Node( VentSlab( Item ).RadInNode ).HumRat, Node( VentSlab( Item ).RadInNode ).Temp );
					QCoilReq = Node( VentSlab( Item ).FanOutletNode ).MassFlowRate * CpAirZn * ( HCoilOutAirTemp - HCoilInAirTemp );

				}

				if ( QCoilReq < 0.0 ) QCoilReq = 0.0; // a heating coil can only heat, not cool

				SimulateHeatingCoilComponents( VentSlab( Item ).HCoilName, FirstHVACIteration, QCoilReq, VentSlab( Item ).HCoil_Index );

			}}

		}

		InletNode = VentSlab( Item ).FanOutletNode;
		OutletNode = VentSlab( Item ).RadInNode;
		AirMassFlow = Node( OutletNode ).MassFlowRate;

		LoadMet = AirMassFlow * ( PsyHFnTdbW( Node( OutletNode ).Temp, Node( InletNode ).HumRat ) - PsyHFnTdbW( Node( InletNode ).Temp, Node( InletNode ).HumRat ) );

	}


	void
	CalcVentilatedSlabCoilOutput(
		int const Item, // system index in ventilated slab array
		Real64 & PowerMet, // power supplied (W)
		Real64 & LatOutputProvided // latent capacity supplied (kg/s)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Young Tae Chae, Rick Strand
		//       DATE WRITTEN   June 2008
		//       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
		//       RE-ENGINEERED  July 2015, M.J. Witte, Refactored coil output calcs in to this new routine

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine calculates the output from the coils

		// METHODOLOGY EMPLOYED:
		// Calculates the sensible and total enthalpy change from the fan outlet node to the slab inlet node.

		// REFERENCES:
		// na

		// Using/Aliasing
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 AirMassFlow; // total mass flow through the system
		int FanOutletNode; // system fan outlet node
		int OutletNode; // air outlet node
		Real64 SpecHumOut; // Specific humidity ratio of outlet air (kg moisture / kg moist air)
		Real64 SpecHumIn; // Specific humidity ratio of inlet air (kg moisture / kg moist air)
		Real64 QTotUnitOut; // total unit output [watts]
		Real64 QUnitOut; // heating or sens. cooling provided by fan coil unit [watts]

		// FLOW:

		OutletNode = VentSlab( Item ).RadInNode;
		FanOutletNode = VentSlab( Item ).FanOutletNode;
		AirMassFlow = Node( OutletNode ).MassFlowRate;

//		QTotUnitOut = AirMassFlow * ( Node( OutletNode ).Enthalpy - Node( FanOutletNode ).Enthalpy );
		QTotUnitOut = AirMassFlow * ( PsyHFnTdbW( Node( OutletNode ).Temp, Node( OutletNode ).HumRat ) - PsyHFnTdbW( Node( FanOutletNode ).Temp, Node( FanOutletNode ).HumRat ) );
		QUnitOut = AirMassFlow * ( PsyHFnTdbW( Node( OutletNode ).Temp, Node( FanOutletNode ).HumRat ) - PsyHFnTdbW( Node( FanOutletNode ).Temp, Node( FanOutletNode ).HumRat ) );
		// Limit sensible <= total when cooling (which is negative, so use max)
		QUnitOut = max( QUnitOut, QTotUnitOut );

		// Report variables...
		VentSlab( Item ).HeatCoilPower = max( 0.0, QUnitOut );
		VentSlab( Item ).SensCoolCoilPower = std::abs( min( 0.0, QUnitOut ) );
		VentSlab( Item ).TotCoolCoilPower = std::abs( min( 0.0, QTotUnitOut ) );
		VentSlab( Item ).LateCoolCoilPower = VentSlab( Item ).TotCoolCoilPower - VentSlab( Item ).SensCoolCoilPower;
		VentSlab( Item ).ElecFanPower = FanElecPower;
		VentSlab( Item ).AirMassFlowRate = AirMassFlow;

		SpecHumOut = Node( OutletNode ).HumRat;
		SpecHumIn = Node( FanOutletNode ).HumRat;
		LatOutputProvided = AirMassFlow * ( SpecHumOut - SpecHumIn ); // Latent rate (kg/s), dehumid = negative
		PowerMet = QUnitOut;

	}

	void
	CalcVentilatedSlabRadComps(
		int const Item, // System index in ventilated slab array
		bool const EP_UNUSED( FirstHVACIteration ) // flag for 1st HVAV iteration in the time step !unused1208
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Young Tae Chae, Rick Strand
		//       DATE WRITTEN   June 2008
		//       MODIFIED       Sep 2011 LKL/BG - resimulate only zones needing it for Radiant systems
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine launches the individual component simulations.
		// This is called either when the system is off to carry null conditions
		// through the system or during control iterations to continue updating
		// what is going on within the system.

		// METHODOLOGY EMPLOYED:
		// Simply calls the different components in order.  Only slight wrinkles
		// here are that the Ventilated Slab has it's own outside air mixed and
		// that a cooling coil must be present in order to call a cooling coil
		// simulation.  Other than that, the subroutine is very straightforward.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataEnvironment::OutBaroPress;
		using General::RoundSigDigits;
		using Fans::SimulateFanComponents;
		using HeatingCoils::SimulateHeatingCoilComponents;
		using WaterCoils::SimulateWaterCoilComponents;
		using SteamCoils::SimulateSteamCoilComponents;
		using DataHeatBalance::Construct;
		using DataHeatBalance::Zone;
		using DataHeatBalFanSys::RadSysTiHBConstCoef;
		using DataHeatBalFanSys::RadSysTiHBToutCoef;
		using DataHeatBalFanSys::RadSysTiHBQsrcCoef;
		using DataHeatBalFanSys::RadSysToHBConstCoef;
		using DataHeatBalFanSys::RadSysToHBTinCoef;
		using DataHeatBalFanSys::RadSysToHBQsrcCoef;
		using DataHeatBalFanSys::CTFTsrcConstPart;
		using DataHeatBalFanSys::ZoneAirHumRat;
		using DataHeatBalFanSys::MAT;
		using DataHeatBalSurface::TH;
		using DataSurfaces::Surface;
		using NodeInputManager::GetOnlySingleNode;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const CondDeltaTemp( 0.001 ); // How close the surface temperatures can get to the dewpoint temperature
		// of a space before the radiant cooling system shuts off the flow.
		Real64 const ZeroSystemResp( 0.1 ); // Response below which the system response is really zero
		Real64 const TempCheckLimit( 0.1 ); // Maximum allowed temperature difference between outlet temperature calculations
		static std::string const CurrentModuleObject( "ZoneHVAC:VentilatedSlab" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ConstrNum; // Index for construction number in Construct derived type
		Real64 CpAirZn; // Intermediate calculational variable for specific heat of air
		Real64 DewPointTemp; // Dew-point temperature based on the zone air conditions
		Real64 EpsMdotCpAirZn; // Epsilon (heat exchanger terminology) times water mass flow rate times water specific heat
		Real64 Mdot; // Intermediate calculation variable for mass flow rate in a surface within the radiant system
		int RadSurfNum; // DO loop counter for the surfaces that comprise a particular radiant system
		int RadSurfNum2; // DO loop counter for the surfaces that comprise a particular radiant system
		int RadSurfNum3; // DO loop counter for the surfaces that comprise a particular radiant system
		//unused0309  INTEGER  :: RadSurfNum4    ! DO loop counter for the surfaces that comprise a particular radiant system

		int SurfNum; // Index for radiant surface in Surface derived type
		int SurfNum2; // Index for radiant surface in Surface derived type
		//unused0309  INTEGER  :: RadSurfNumNum
		Real64 TotalVentSlabRadPower; // Total heat source/sink to radiant system
		Real64 AirMassFlow; // air mass flow rate in the radiant system, kg/s
		int SlabInNode; // Node number of the air entering the radiant system
		Real64 AirOutletTempCheck; // Radiant system air outlet temperature (calculated from mixing all outlet streams together)
		Real64 AirTempIn; // Temperature of the air entering the radiant system, in C
		int ZoneNum; // number of zone being served
		Real64 ZoneMult; // Zone multiplier for this system
		Real64 Ca; // Coefficients to relate the inlet air temperature to the heat source
		Real64 Cb;
		Real64 Cc;
		Real64 Cd;
		Real64 Ce;
		Real64 Cf;
		Real64 Cg;
		Real64 Ch;
		Real64 Ci;
		Real64 Cj;
		Real64 Ck;
		Real64 Cl;
		// For more info on Ca through Cl, refer Constant Flow Radiant System
		//unused0309  REAL(r64):: CoreNumber
		static Real64 Ckj; // Coefficients for individual surfaces within a radiant system
		static Real64 Cmj;
		static Array1D< Real64 > AirTempOut; // Array of outlet air temperatures for each surface in the radiant system
		int FanOutletNode; // unit air outlet node
		int OAInletNode; // unit air outlet node
		int MixoutNode; // unit air outlet node
		int ReturnAirNode; // discription
		int ZoneAirInNode; // supply air node
		//For Phase 3
		Real64 CNumDS;
		Real64 CLengDS;
		Real64 CDiaDS;
		Real64 FlowFrac;
		//unused0309  REAL(r64)  :: SlabAirOutTemp
		Real64 MSlabAirInTemp;
		static bool ErrorsFound( false ); // Set to true if errors in input, fatal at end of routine

		std::string MSlabIn;
		std::string MSlabOut;
		std::string SlabName;
		int MSlabInletNode;
		int MSlabOutletNode;
		static int CondensationErrorCount( 0 ); // Counts for # times the radiant systems are shutdown due to condensation
		static int EnergyImbalanceErrorCount( 0 ); // Counts for # times a temperature mismatch is found in the energy balance check
		static bool FirstTimeFlag( true ); // for setting size of Ckj, Cmj, AirTempOut arrays

		// FLOW:

		if ( FirstTimeFlag ) {
			AirTempOut.allocate( MaxCloNumOfSurfaces );
			FirstTimeFlag = false;
		}

		Ckj = 0.0;
		Cmj = 0.0;

		SlabInNode = VentSlab( Item ).RadInNode;
		FanOutletNode = VentSlab( Item ).FanOutletNode;
		OAInletNode = VentSlab( Item ).OutsideAirNode;
		MixoutNode = VentSlab( Item ).OAMixerOutNode;
		ReturnAirNode = VentSlab( Item ).ReturnAirNode;
		ZoneAirInNode = VentSlab( Item ).ZoneAirInNode;

		// Set the conditions on the air side inlet
		ZoneNum = VentSlab( Item ).ZonePtr;
		ZoneMult = double( Zone( ZoneNum ).Multiplier * Zone( ZoneNum ).ListMultiplier );
		AirMassFlow = Node( VentSlab( Item ).RadInNode ).MassFlowRate / ZoneMult;

		if ( OperatingMode == HeatingMode ) {

			if ( ( ! VentSlab( Item ).HCoilPresent ) || ( VentSlab( Item ).HCoilSchedValue <= 0.0 ) ) {

				AirTempIn = Node( FanOutletNode ).Temp;
				Node( SlabInNode ).Temp = Node( FanOutletNode ).Temp; // If coil not available or running, then coil in and out temps same

			} else {

				AirTempIn = Node( SlabInNode ).Temp;
			}
		}

		if ( OperatingMode == CoolingMode ) {

			if ( ( ! VentSlab( Item ).CCoilPresent ) || ( VentSlab( Item ).CCoilSchedValue <= 0.0 ) ) {

				AirTempIn = Node( FanOutletNode ).Temp;
				Node( SlabInNode ).Temp = Node( FanOutletNode ).Temp; // If coil not available or running, then coil in and out temps same

			} else {

				AirTempIn = Node( SlabInNode ).Temp;
			}

		}

		if ( AirMassFlow <= 0.0 ) {
			// No flow or below minimum allowed so there is no heat source/sink
			// This is possible with a mismatch between system and plant operation
			// or a slight mismatch between zone and system controls.  This is not
			// necessarily a "problem" so this exception is necessary in the code.

			for ( RadSurfNum = 1; RadSurfNum <= VentSlab( Item ).NumOfSurfaces; ++RadSurfNum ) {
				SurfNum = VentSlab( Item ).SurfacePtr( RadSurfNum );
				QRadSysSource( SurfNum ) = 0.0;
				if ( Surface( SurfNum ).ExtBoundCond > 0 && Surface( SurfNum ).ExtBoundCond != SurfNum ) QRadSysSource( Surface( SurfNum ).ExtBoundCond ) = 0.0; // Also zero the other side of an interzone
			}

			VentSlab( Item ).SlabOutTemp = VentSlab( Item ).SlabInTemp;

			// zero out node flows
			Node( SlabInNode ).MassFlowRate = 0.0;
			Node( FanOutletNode ).MassFlowRate = 0.0;
			Node( OAInletNode ).MassFlowRate = 0.0;
			Node( MixoutNode ).MassFlowRate = 0.0;
			Node( ReturnAirNode ).MassFlowRate = 0.0;
			Node( FanOutletNode ).Temp = Node( SlabInNode ).Temp;
			AirMassFlow = 0.0;
		}

		if ( AirMassFlow > 0.0 ) {

			if ( ( VentSlab( Item ).SysConfg == SlabOnly ) || ( VentSlab( Item ).SysConfg == SlabAndZone ) ) {

				for ( RadSurfNum = 1; RadSurfNum <= VentSlab( Item ).NumOfSurfaces; ++RadSurfNum ) {
					SurfNum = VentSlab( Item ).SurfacePtr( RadSurfNum );
					// Determine the heat exchanger "effectiveness" term
					EpsMdotCpAirZn = CalcVentSlabHXEffectTerm( Item, AirTempIn, AirMassFlow, VentSlab( Item ).SurfaceFlowFrac( RadSurfNum ), VentSlab( Item ).CoreLength, VentSlab( Item ).CoreDiameter, VentSlab( Item ).CoreNumbers );

					// Obtain the heat balance coefficients and calculate the intermediate coefficients
					// linking the inlet air temperature to the heat source/sink to the radiant system.
					// The coefficients are based on the Constant Flow Radiation System.

					ConstrNum = Surface( SurfNum ).Construction;

					Ca = RadSysTiHBConstCoef( SurfNum );
					Cb = RadSysTiHBToutCoef( SurfNum );
					Cc = RadSysTiHBQsrcCoef( SurfNum );

					Cd = RadSysToHBConstCoef( SurfNum );
					Ce = RadSysToHBTinCoef( SurfNum );
					Cf = RadSysToHBQsrcCoef( SurfNum );

					Cg = CTFTsrcConstPart( SurfNum );
					Ch = double( Construct( ConstrNum ).CTFTSourceQ( 0 ) );
					Ci = double( Construct( ConstrNum ).CTFTSourceIn( 0 ) );
					Cj = double( Construct( ConstrNum ).CTFTSourceOut( 0 ) );

					Ck = Cg + ( ( Ci * ( Ca + Cb * Cd ) + Cj * ( Cd + Ce * Ca ) ) / ( 1.0 - Ce * Cb ) );
					Cl = Ch + ( ( Ci * ( Cc + Cb * Cf ) + Cj * ( Cf + Ce * Cc ) ) / ( 1.0 - Ce * Cb ) );

					Mdot = AirMassFlow * VentSlab( Item ).SurfaceFlowFrac( RadSurfNum );
					CpAirZn = PsyCpAirFnWTdb( Node( VentSlab( Item ).RadInNode ).HumRat, Node( VentSlab( Item ).RadInNode ).Temp );

					QRadSysSource( SurfNum ) = VentSlab( Item ).CoreNumbers * EpsMdotCpAirZn * ( AirTempIn - Ck ) / ( 1.0 + ( EpsMdotCpAirZn * Cl / Surface( SurfNum ).Area ) );

					if ( Surface( SurfNum ).ExtBoundCond > 0 && Surface( SurfNum ).ExtBoundCond != SurfNum ) QRadSysSource( Surface( SurfNum ).ExtBoundCond ) = QRadSysSource( SurfNum );
					// Also set the other side of an interzone!
					AirTempOut( RadSurfNum ) = AirTempIn - ( QRadSysSource( SurfNum ) / ( Mdot * CpAirZn ) );

					// "Temperature Comparison" Cut-off:
					// Check to see whether or not the system should really be running.  If
					// QRadSysSource is negative when we are in heating mode or QRadSysSource
					// is positive when we are in cooling mode, then the radiant system will
					// be doing the opposite of its intention.  In this case, the flow rate
					// is set to zero to avoid heating in cooling mode or cooling in heating
					// mode.

					if ( ( ( OperatingMode == HeatingMode ) && ( QRadSysSource( SurfNum ) <= 0.0 ) ) || ( ( OperatingMode == CoolingMode ) && ( QRadSysSource( SurfNum ) >= 0.0 ) ) ) {

						// IF (.not. WarmupFlag) THEN
						//   TempComparisonErrorCount = TempComparisonErrorCount + 1
						//   IF (TempComparisonErrorCount <= NumOfVentSlabs) THEN
						//     CALL ShowWarningError('Radaint Heat exchange is negative in Heating Mode or posive in Cooling Mode')
						//     CALL ShowContinueError('Flow to the following ventilated slab will be shut-off to avoid heating in cooling mode or cooling &
						//                             in heating mode')
						//     CALL ShowContinueError('Ventilated Slab Name = '//TRIM(VentSlab(Item)%Name))
						//     CALL ShowContinueError('All node temperature are reseted at the ventilated slab surface temperature = '// &
						//                            RoundSigDigits(TH(VentSlab(Item)%SurfacePtr(RadSurfNum),1,2),2))
						//     CALL ShowContinueErrorTimeStamp(' ')
						//   ELSE
						//     CALL ShowRecurringWarningErrorAtEnd('Ventilated Slab ['//TRIM(VentSlab(Item)%Name)//  &
						//                  '] Temperature Comparison Error shut-off occurrence continues.',  &
						//                  VentSlab(Item)%CondErrCount)
						//   END IF
						// END IF

						Node( SlabInNode ).MassFlowRate = 0.0;
						Node( FanOutletNode ).MassFlowRate = 0.0;
						Node( OAInletNode ).MassFlowRate = 0.0;
						Node( MixoutNode ).MassFlowRate = 0.0;
						Node( ReturnAirNode ).MassFlowRate = 0.0;
						AirMassFlow = 0.0;

						for ( RadSurfNum2 = 1; RadSurfNum2 <= VentSlab( Item ).NumOfSurfaces; ++RadSurfNum2 ) {
							SurfNum2 = VentSlab( Item ).SurfacePtr( RadSurfNum2 );
							QRadSysSource( SurfNum2 ) = 0.0;
							if ( Surface( SurfNum2 ).ExtBoundCond > 0 && Surface( SurfNum2 ).ExtBoundCond != SurfNum2 ) QRadSysSource( Surface( SurfNum2 ).ExtBoundCond ) = 0.0; // Also zero the other side of an interzone

							if ( VentSlab( Item ).SysConfg == SlabOnly ) {
								//            Node(Returnairnode)%Temp = MAT(Zonenum)
								Node( ReturnAirNode ).Temp = TH( 2, 1, VentSlab( Item ).SurfacePtr( RadSurfNum ) );
								Node( FanOutletNode ).Temp = Node( ReturnAirNode ).Temp;
								Node( SlabInNode ).Temp = Node( FanOutletNode ).Temp;
							} else if ( VentSlab( Item ).SysConfg == SlabAndZone ) {
								Node( ReturnAirNode ).Temp = MAT( ZoneNum );
								Node( SlabInNode ).Temp = Node( ReturnAirNode ).Temp;
								Node( FanOutletNode ).Temp = Node( SlabInNode ).Temp;
								Node( ZoneAirInNode ).Temp = Node( SlabInNode ).Temp;
							}

						}
						break; // outer do loop
					}

					// Condensation Cut-off:
					// Check to see whether there are any surface temperatures within the radiant system that have
					// dropped below the dew-point temperature.  If so, we need to shut off this radiant system.
					// A safety parameter is added (hardwired parameter) to avoid getting too close to condensation
					// conditions.

					if ( OperatingMode == CoolingMode ) {
						DewPointTemp = PsyTdpFnWPb( ZoneAirHumRat( VentSlab( Item ).ZonePtr ), OutBaroPress );
						for ( RadSurfNum2 = 1; RadSurfNum2 <= VentSlab( Item ).NumOfSurfaces; ++RadSurfNum2 ) {
							if ( TH( 2, 1, VentSlab( Item ).SurfacePtr( RadSurfNum2 ) ) < ( DewPointTemp + CondDeltaTemp ) ) {
								// Condensation warning--must shut off radiant system
								Node( SlabInNode ).MassFlowRate = 0.0;
								Node( FanOutletNode ).MassFlowRate = 0.0;
								Node( OAInletNode ).MassFlowRate = 0.0;
								Node( MixoutNode ).MassFlowRate = 0.0;
								Node( ReturnAirNode ).MassFlowRate = 0.0;
								Node( FanOutletNode ).Temp = Node( SlabInNode ).Temp;
								AirMassFlow = 0.0;
								for ( RadSurfNum3 = 1; RadSurfNum3 <= VentSlab( Item ).NumOfSurfaces; ++RadSurfNum3 ) {
									SurfNum2 = VentSlab( Item ).SurfacePtr( RadSurfNum3 );
									QRadSysSource( SurfNum2 ) = 0.0;
									if ( Surface( SurfNum2 ).ExtBoundCond > 0 && Surface( SurfNum2 ).ExtBoundCond != SurfNum2 ) QRadSysSource( Surface( SurfNum2 ).ExtBoundCond ) = 0.0; // Also zero the other side of an interzone
								}
								// Produce a warning message so that user knows the system was shut-off due to potential for condensation
								if ( ! WarmupFlag ) {
									++CondensationErrorCount;

									if ( VentSlab( Item ).CondErrIndex == 0 ) {
										ShowWarningMessage( cMO_VentilatedSlab + " [" + VentSlab( Item ).Name + ']' );
										ShowContinueError( "Surface [" + Surface( VentSlab( Item ).SurfacePtr( RadSurfNum2 ) ).Name + "] temperature below dew-point temperature--potential for condensation exists" );
										ShowContinueError( "Flow to the ventilated slab system will be shut-off to avoid condensation" );
										ShowContinueError( "Predicted radiant system surface temperature = " + RoundSigDigits( TH( 2, 1, VentSlab( Item ).SurfacePtr( RadSurfNum2 ) ), 2 ) );
										ShowContinueError( "Zone dew-point temperature + safety factor delta= " + RoundSigDigits( DewPointTemp + CondDeltaTemp, 2 ) );
										ShowContinueErrorTimeStamp( "" );
									}
									if ( CondensationErrorCount == 1 ) {
										ShowContinueError( "Note that there is a " + RoundSigDigits( CondDeltaTemp, 4 ) + " C safety built-in to the shut-off criteria" );
										ShowContinueError( "Note also that this affects all surfaces that are part of this system" );
									}
									ShowRecurringWarningErrorAtEnd( cMO_VentilatedSlab + " [" + VentSlab( Item ).Name + "] condensation shut-off occurrence continues.", VentSlab( Item ).CondErrIndex, DewPointTemp, DewPointTemp, _, "C", "C" );
								}
								break; // outer do loop
							}
						}
					}
				}

				// Total Radiant Power
				AirOutletTempCheck = 0.0;
				TotalVentSlabRadPower = 0.0;
				for ( RadSurfNum = 1; RadSurfNum <= VentSlab( Item ).NumOfSurfaces; ++RadSurfNum ) {
					SurfNum = VentSlab( Item ).SurfacePtr( RadSurfNum );
					TotalVentSlabRadPower += QRadSysSource( SurfNum );
					AirOutletTempCheck += ( VentSlab( Item ).SurfaceFlowFrac( RadSurfNum ) * AirTempOut( RadSurfNum ) );
				}
				TotalVentSlabRadPower *= ZoneMult;

				// Return Air temp Check
				if ( VentSlab( Item ).SysConfg == SlabOnly ) {
					if ( AirMassFlow > 0.0 ) {
						CpAirZn = PsyCpAirFnWTdb( Node( VentSlab( Item ).RadInNode ).HumRat, Node( VentSlab( Item ).RadInNode ).Temp );
						Node( ReturnAirNode ).Temp = Node( SlabInNode ).Temp - ( TotalVentSlabRadPower / ( AirMassFlow * CpAirZn ) );
						if ( ( std::abs( Node( ReturnAirNode ).Temp - AirOutletTempCheck ) > TempCheckLimit ) && ( std::abs( TotalVentSlabRadPower ) > ZeroSystemResp ) ) {

							if ( ! WarmupFlag ) {
								++EnergyImbalanceErrorCount;
								if ( VentSlab( Item ).EnrgyImbalErrIndex == 0 ) {
									ShowWarningMessage( cMO_VentilatedSlab + " [" + VentSlab( Item ).Name + ']' );
									ShowContinueError( "Ventilated Slab (slab only type) air outlet temperature calculation mismatch." );
									ShowContinueError( "This should not happen as it indicates a potential energy imbalance in the calculations." );
									ShowContinueError( "However, it could also result from improper input for the ventilated slab or" );
									ShowContinueError( "illogical control temperatures.  Check your input for this ventilated slab and" );
									ShowContinueError( "also look at the internal data shown below." );
									ShowContinueError( "Predicted return air temperature [C] from the overall energy balance = " + RoundSigDigits( Node( ReturnAirNode ).Temp, 4 ) );
									ShowContinueError( "Predicted return air temperature [C] from the slab section energy balances = " + RoundSigDigits( AirOutletTempCheck, 4 ) );
									ShowContinueError( "Total energy rate (power) [W] added to the slab = " + RoundSigDigits( TotalVentSlabRadPower, 4 ) );
									ShowContinueErrorTimeStamp( "" );
								}
								ShowRecurringWarningErrorAtEnd( cMO_VentilatedSlab + " [" + VentSlab( Item ).Name + "] temperature calculation mismatch occurrence continues.", VentSlab( Item ).EnrgyImbalErrIndex );
							}

						}
					} else {
						Node( ReturnAirNode ).Temp = Node( SlabInNode ).Temp;
					}
				}

				if ( VentSlab( Item ).SysConfg == SlabAndZone ) {
					if ( AirMassFlow > 0.0 ) {
						Node( ZoneAirInNode ).Temp = Node( SlabInNode ).Temp - ( TotalVentSlabRadPower / ( AirMassFlow * CpAirZn ) );
						if ( ( std::abs( Node( ZoneAirInNode ).Temp - AirOutletTempCheck ) > TempCheckLimit ) && ( std::abs( TotalVentSlabRadPower ) > ZeroSystemResp ) ) {

							if ( ! WarmupFlag ) {
								++EnergyImbalanceErrorCount;
								if ( VentSlab( Item ).EnrgyImbalErrIndex == 0 ) {
									ShowWarningMessage( cMO_VentilatedSlab + " [" + VentSlab( Item ).Name + ']' );
									ShowContinueError( "Ventilated Slab (slab only type) air outlet temperature calculation mismatch." );
									ShowContinueError( "This should not happen as it indicates a potential energy imbalance in the calculations." );
									ShowContinueError( "However, it could also result from improper input for the ventilated slab or" );
									ShowContinueError( "illogical control temperatures.  Check your input for this ventilated slab and" );
									ShowContinueError( "also look at the internal data shown below." );
									ShowContinueError( "Predicted return air temperature [C] from the overall energy balance = " + RoundSigDigits( Node( ReturnAirNode ).Temp, 4 ) );
									ShowContinueError( "Predicted return air temperature [C] from the slab section energy balances = " + RoundSigDigits( AirOutletTempCheck, 4 ) );
									ShowContinueError( "Total energy rate (power) [W] added to the slab = " + RoundSigDigits( TotalVentSlabRadPower, 4 ) );
									ShowContinueErrorTimeStamp( "" );
								}
								ShowRecurringWarningErrorAtEnd( cMO_VentilatedSlab + " [" + VentSlab( Item ).Name + "] temperature calculation mismatch occurrence continues.", VentSlab( Item ).EnrgyImbalErrIndex );
							}

						}
						//       IF ((.NOT. FirstHVACIteration) .AND. &
						//          (ABS(Node(ReturnAirNode)%Temp-MAT(Zonenum)) > VentSlabAirTempToler))THEN
						//          NeedtoIterate = .TRUE.
						//      END IF
						//         Node(ReturnAirNode)%Temp = MAT(Zonenum)
					} else {
						Node( ZoneAirInNode ).Temp = Node( SlabInNode ).Temp;
						Node( ReturnAirNode ).Temp = MAT( ZoneNum );
					}
				}

				// Now that we have the source/sink term, we must redo the heat balances to obtain
				// the new SumHATsurf value for the zone.  Note that the difference between the new
				// SumHATsurf and the value originally calculated by the heat balance with a zero
				// source for all radiant systems in the zone is the load met by the system (approximately).
				HeatBalanceSurfaceManager::CalcHeatBalanceOutsideSurf( ZoneNum );
				HeatBalanceSurfaceManager::CalcHeatBalanceInsideSurf( ZoneNum );

			} //SYSCONFIG. SLABONLY&SLABANDZONE

			if ( VentSlab( Item ).SysConfg == SeriesSlabs ) {

				for ( RadSurfNum = 1; RadSurfNum <= VentSlab( Item ).NumOfSurfaces; ++RadSurfNum ) {

					CNumDS = VentSlab( Item ).CNumbers( RadSurfNum );
					CLengDS = VentSlab( Item ).CLength( RadSurfNum ); // for check
					CDiaDS = VentSlab( Item ).CDiameter( RadSurfNum ); // for check
					FlowFrac = 1.0;

					SurfNum = VentSlab( Item ).SurfacePtr( RadSurfNum );

					// Determine the heat exchanger "effectiveness" term
					EpsMdotCpAirZn = CalcVentSlabHXEffectTerm( Item, AirTempIn, AirMassFlow, FlowFrac, CLengDS, CDiaDS, CNumDS );

					// Obtain the heat balance coefficients and calculate the intermediate coefficients
					// linking the inlet air temperature to the heat source/sink to the radiant system.
					// The coefficients are based on the Constant Flow Radiation System.

					ConstrNum = Surface( SurfNum ).Construction;

					Ca = RadSysTiHBConstCoef( SurfNum );
					Cb = RadSysTiHBToutCoef( SurfNum );
					Cc = RadSysTiHBQsrcCoef( SurfNum );

					Cd = RadSysToHBConstCoef( SurfNum );
					Ce = RadSysToHBTinCoef( SurfNum );
					Cf = RadSysToHBQsrcCoef( SurfNum );

					Cg = CTFTsrcConstPart( SurfNum );
					Ch = double( Construct( ConstrNum ).CTFTSourceQ( 0 ) );
					Ci = double( Construct( ConstrNum ).CTFTSourceIn( 0 ) );
					Cj = double( Construct( ConstrNum ).CTFTSourceOut( 0 ) );

					Ck = Cg + ( ( Ci * ( Ca + Cb * Cd ) + Cj * ( Cd + Ce * Ca ) ) / ( 1.0 - Ce * Cb ) );
					Cl = Ch + ( ( Ci * ( Cc + Cb * Cf ) + Cj * ( Cf + Ce * Cc ) ) / ( 1.0 - Ce * Cb ) );

					Mdot = AirMassFlow * FlowFrac;
					CpAirZn = PsyCpAirFnWTdb( Node( VentSlab( Item ).RadInNode ).HumRat, Node( VentSlab( Item ).RadInNode ).Temp );

					QRadSysSource( SurfNum ) = CNumDS * EpsMdotCpAirZn * ( AirTempIn - Ck ) / ( 1.0 + ( EpsMdotCpAirZn * Cl / Surface( SurfNum ).Area ) );

					if ( Surface( SurfNum ).ExtBoundCond > 0 && Surface( SurfNum ).ExtBoundCond != SurfNum ) QRadSysSource( Surface( SurfNum ).ExtBoundCond ) = QRadSysSource( SurfNum );
					// Also set the other side of an interzone!

					AirTempOut( RadSurfNum ) = AirTempIn - ( QRadSysSource( SurfNum ) / ( Mdot * CpAirZn ) );
					AirTempIn = AirTempOut( RadSurfNum );
					// "Temperature Comparison" Cut-off:
					// Check to see whether or not the system should really be running.  If
					// QRadSysSource is negative when we are in heating mode or QRadSysSource
					// is positive when we are in cooling mode, then the radiant system will
					// be doing the opposite of its intention.  In this case, the flow rate
					// is set to zero to avoid heating in cooling mode or cooling in heating
					// mode.

					if ( RadSurfNum == 1 ) {
						if ( ( ( OperatingMode == HeatingMode ) && ( QRadSysSource( SurfNum ) <= 0.0 ) ) || ( ( OperatingMode == CoolingMode ) && ( QRadSysSource( SurfNum ) >= 0.0 ) ) ) {
							//IF (.not. WarmupFlag) THEN
							//  TempComparisonErrorCount = TempComparisonErrorCount + 1
							//  IF (TempComparisonErrorCount <= NumOfVentSlabs) THEN
							//    CALL ShowWarningError('Radaint Heat exchange is negative in Heating Mode or posive in Cooling Mode')
							//    CALL ShowContinueError('Flow to the following ventilated slab will be shut-off to avoid heating in cooling mode or cooling &
							//                            in heating mode')
							//    CALL ShowContinueError('Ventilated Slab Name = '//TRIM(VentSlab(Item)%Name))
							//    CALL ShowContinueError('Surface Name  = '//TRIM(VentSlab(Item)%SurfaceName(RadSurfNum)))
							//    CALL ShowContinueError('All node temperature are reseted at the surface temperature of control zone = '// &
							//                           RoundSigDigits(TH(VentSlab(Item)%SurfacePtr(1),1,2),2))
							//    CALL ShowContinueErrorTimeStamp(' ')
							//  ELSE
							//    CALL ShowRecurringWarningErrorAtEnd('Ventilated Slab ['//TRIM(VentSlab(Item)%Name)//  &
							//                 ']  shut-off occurrence continues due to temperature comparison error.',  &
							//                 VentSlab(Item)%CondErrCount)
							//  END IF
							//END IF

							Node( SlabInNode ).MassFlowRate = 0.0;
							Node( FanOutletNode ).MassFlowRate = 0.0;
							Node( OAInletNode ).MassFlowRate = 0.0;
							Node( MixoutNode ).MassFlowRate = 0.0;
							Node( ReturnAirNode ).MassFlowRate = 0.0;
							AirMassFlow = 0.0;

							for ( RadSurfNum2 = 1; RadSurfNum2 <= VentSlab( Item ).NumOfSurfaces; ++RadSurfNum2 ) {
								SurfNum2 = VentSlab( Item ).SurfacePtr( RadSurfNum2 );
								QRadSysSource( SurfNum2 ) = 0.0;
								if ( Surface( SurfNum2 ).ExtBoundCond > 0 && Surface( SurfNum2 ).ExtBoundCond != SurfNum2 ) QRadSysSource( Surface( SurfNum2 ).ExtBoundCond ) = 0.0; // Also zero the other side of an interzone
							}
							Node( ReturnAirNode ).Temp = TH( 2, 1, VentSlab( Item ).SurfacePtr( 1 ) );
							Node( FanOutletNode ).Temp = Node( ReturnAirNode ).Temp;
							Node( SlabInNode ).Temp = Node( FanOutletNode ).Temp;
							// Each Internal node is reseted at the surface temperature

							break; // outer do loop
						}
					}
					// Condensation Cut-off:
					// Check to see whether there are any surface temperatures within the radiant system that have
					// dropped below the dew-point temperature.  If so, we need to shut off this radiant system.
					// A safety parameter is added (hardwired parameter) to avoid getting too close to condensation
					// conditions.

					if ( OperatingMode == CoolingMode ) {
						DewPointTemp = PsyTdpFnWPb( ZoneAirHumRat( VentSlab( Item ).ZPtr( RadSurfNum ) ), OutBaroPress );
						for ( RadSurfNum2 = 1; RadSurfNum2 <= VentSlab( Item ).NumOfSurfaces; ++RadSurfNum2 ) {
							if ( TH( 2, 1, VentSlab( Item ).SurfacePtr( RadSurfNum2 ) ) < ( DewPointTemp + CondDeltaTemp ) ) {
								// Condensation warning--must shut off radiant system
								Node( SlabInNode ).MassFlowRate = 0.0;
								Node( FanOutletNode ).MassFlowRate = 0.0;
								Node( OAInletNode ).MassFlowRate = 0.0;
								Node( MixoutNode ).MassFlowRate = 0.0;
								Node( ReturnAirNode ).MassFlowRate = 0.0;
								Node( FanOutletNode ).Temp = Node( SlabInNode ).Temp;
								AirMassFlow = 0.0;
								for ( RadSurfNum3 = 1; RadSurfNum3 <= VentSlab( Item ).NumOfSurfaces; ++RadSurfNum3 ) {
									SurfNum2 = VentSlab( Item ).SurfacePtr( RadSurfNum3 );
									QRadSysSource( SurfNum2 ) = 0.0;
									if ( Surface( SurfNum2 ).ExtBoundCond > 0 && Surface( SurfNum2 ).ExtBoundCond != SurfNum2 ) QRadSysSource( Surface( SurfNum2 ).ExtBoundCond ) = 0.0; // Also zero the other side of an interzone
								}
								// Produce a warning message so that user knows the system was shut-off due to potential for condensation
								if ( ! WarmupFlag ) {
									++CondensationErrorCount;
									if ( VentSlab( Item ).CondErrIndex == 0 ) {
										ShowWarningMessage( cMO_VentilatedSlab + " [" + VentSlab( Item ).Name + ']' );
										ShowContinueError( "Surface [" + Surface( VentSlab( Item ).SurfacePtr( RadSurfNum2 ) ).Name + "] temperature below dew-point temperature--potential for condensation exists" );
										ShowContinueError( "Flow to the ventilated slab system will be shut-off to avoid condensation" );
										ShowContinueError( "Predicted radiant system surface temperature = " + RoundSigDigits( TH( 2, 1, VentSlab( Item ).SurfacePtr( RadSurfNum2 ) ), 2 ) );
										ShowContinueError( "Zone dew-point temperature + safety factor delta= " + RoundSigDigits( DewPointTemp + CondDeltaTemp, 2 ) );
										ShowContinueErrorTimeStamp( "" );
									}
									if ( CondensationErrorCount == 1 ) {
										ShowContinueError( "Note that there is a " + RoundSigDigits( CondDeltaTemp, 4 ) + " C safety built-in to the shut-off criteria" );
										ShowContinueError( "Note also that this affects all surfaces that are part of this system" );
									}
									ShowRecurringWarningErrorAtEnd( cMO_VentilatedSlab + " [" + VentSlab( Item ).Name + "] condensation shut-off occurrence continues.", VentSlab( Item ).CondErrIndex, DewPointTemp, DewPointTemp, _, "C", "C" );
								}
								break; // outer do loop
							}
						}
					}
				}

				// Total Radiant Power
				AirOutletTempCheck = 0.0;
				TotalVentSlabRadPower = 0.0;
				for ( RadSurfNum = 1; RadSurfNum <= VentSlab( Item ).NumOfSurfaces; ++RadSurfNum ) {
					SurfNum = VentSlab( Item ).SurfacePtr( RadSurfNum );
					TotalVentSlabRadPower += QRadSysSource( SurfNum );
					AirOutletTempCheck = AirTempOut( RadSurfNum );
				}
				TotalVentSlabRadPower *= ZoneMult;

				// Intenal Node Temperature Check

				MSlabAirInTemp = Node( SlabInNode ).Temp;

				for ( RadSurfNum = 1; RadSurfNum <= VentSlab( Item ).NumOfSurfaces; ++RadSurfNum ) {
					SlabName = VentSlab( Item ).SurfaceName( RadSurfNum );
					MSlabIn = VentSlab( Item ).SlabIn( RadSurfNum );
					MSlabOut = VentSlab( Item ).SlabOut( RadSurfNum );
					VentSlab( Item ).MSlabInNode = GetOnlySingleNode( MSlabIn, ErrorsFound, CurrentModuleObject, SlabName, NodeType_Air, NodeConnectionType_Internal, 1, ObjectIsNotParent );
					VentSlab( Item ).MSlabOutNode = GetOnlySingleNode( MSlabOut, ErrorsFound, CurrentModuleObject, SlabName, NodeType_Air, NodeConnectionType_Internal, 1, ObjectIsNotParent );
					MSlabInletNode = VentSlab( Item ).MSlabInNode;
					MSlabOutletNode = VentSlab( Item ).MSlabOutNode;
					SurfNum = VentSlab( Item ).SurfacePtr( RadSurfNum );

					if ( AirMassFlow > 0.0 ) {

						CpAirZn = PsyCpAirFnWTdb( Node( VentSlab( Item ).RadInNode ).HumRat, Node( VentSlab( Item ).RadInNode ).Temp );

						Node( MSlabInletNode ).Temp = MSlabAirInTemp;
						Node( MSlabOutletNode ).Temp = Node( MSlabInletNode ).Temp - ( QRadSysSource( SurfNum ) / ( AirMassFlow * CpAirZn ) );
						MSlabAirInTemp = Node( MSlabOutletNode ).Temp;
					} else {
						Node( MSlabInletNode ).Temp = Node( ReturnAirNode ).Temp;
						Node( MSlabOutletNode ).Temp = Node( MSlabInletNode ).Temp;
					}
				}

				// Return Air temp Check
				if ( AirMassFlow > 0.0 ) {

					CpAirZn = PsyCpAirFnWTdb( Node( VentSlab( Item ).RadInNode ).HumRat, Node( VentSlab( Item ).RadInNode ).Temp );
					Node( ReturnAirNode ).Temp = Node( SlabInNode ).Temp - ( TotalVentSlabRadPower / ( AirMassFlow * CpAirZn ) );

					if ( ( std::abs( Node( ReturnAirNode ).Temp - AirOutletTempCheck ) > TempCheckLimit ) && ( std::abs( TotalVentSlabRadPower ) > ZeroSystemResp ) ) { // Return air temperature check did not match calculated temp

						if ( ! WarmupFlag ) {
							++EnergyImbalanceErrorCount;
							if ( VentSlab( Item ).EnrgyImbalErrIndex == 0 ) {
								ShowWarningMessage( cMO_VentilatedSlab + " [" + VentSlab( Item ).Name + ']' );
								ShowContinueError( "Ventilated Slab (slab only type) air outlet temperature calculation mismatch." );
								ShowContinueError( "This should not happen as it indicates a potential energy imbalance in the calculations." );
								ShowContinueError( "However, it could also result from improper input for the ventilated slab or" );
								ShowContinueError( "illogical control temperatures.  Check your input for this ventilated slab and" );
								ShowContinueError( "also look at the internal data shown below." );
								ShowContinueError( "Predicted return air temperature [C] from the overall energy balance = " + RoundSigDigits( Node( ReturnAirNode ).Temp, 4 ) );
								ShowContinueError( "Predicted return air temperature [C] from the slab section energy balances = " + RoundSigDigits( AirOutletTempCheck, 4 ) );
								ShowContinueError( "Total energy rate (power) [W] added to the slab = " + RoundSigDigits( TotalVentSlabRadPower, 4 ) );
								ShowContinueErrorTimeStamp( "" );
							}
							ShowRecurringWarningErrorAtEnd( cMO_VentilatedSlab + " [" + VentSlab( Item ).Name + "] temperature calculation mismatch occurrence continues.", VentSlab( Item ).EnrgyImbalErrIndex );
						}
					}

				} else {
					Node( ReturnAirNode ).Temp = Node( SlabInNode ).Temp;
				}

				// Now that we have the source/sink term, we must redo the heat balances to obtain
				// the new SumHATsurf value for the zone.  Note that the difference between the new
				// SumHATsurf and the value originally calculated by the heat balance with a zero
				// source for all radiant systems in the zone is the load met by the system (approximately).

				HeatBalanceSurfaceManager::CalcHeatBalanceOutsideSurf();
				HeatBalanceSurfaceManager::CalcHeatBalanceInsideSurf();

			} // SeriesSlabs

		} //(AirMassFlow > 0.0d0)

	}

	void
	SimVentSlabOAMixer( int const Item ) // System index in Ventilated Slab array
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   May 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This responsibility of this subroutine is to set the air flow rates
		// through the mixing box portion of the Ventilated Slab and then perform
		// an energy balance to arrive at outlet conditions which then would
		// serve as inlet conditions to the coils (or outlet conditions for
		// the device).  There is some question as to whether this needs to be
		// called every time the coils and fan are called since how the fans and
		// coil operate won't presumable change how the mixer operates.  The
		// method in which this routine is called is slightly cleaner though
		// from a code readability standpoint though less efficient.

		// METHODOLOGY EMPLOYED:
		// The OAMassFlowRate has already been calculated in the main control
		// algorithm.  Use this flow rate to establish all of the other flow
		// rates and perform an energy balance on the mixing of the return and
		// outdoor air streams.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int AirRelNode; // relief air node number in ventilated slab loop
		int InletNode; // inlet node number for ventilated slab loop
		Real64 OAFraction; // Outside air fraction of inlet air
		int OAMixOutNode; // outside air mixer outlet node for ventilated slab loop
		int OutsideAirNode; // outside air node number in ventilated slab loop

		// FLOW:
		AirRelNode = VentSlab( Item ).AirReliefNode;
		InletNode = VentSlab( Item ).ReturnAirNode;
		OAMixOutNode = VentSlab( Item ).OAMixerOutNode;
		OutsideAirNode = VentSlab( Item ).OutsideAirNode;

		// "Resolve" the air flow rates...

		Node( OutsideAirNode ).MassFlowRate = OAMassFlowRate;
		Node( OutsideAirNode ).MassFlowRateMinAvail = OAMassFlowRate;
		Node( OutsideAirNode ).MassFlowRateMaxAvail = OAMassFlowRate;

		Node( AirRelNode ).MassFlowRate = OAMassFlowRate;
		Node( AirRelNode ).MassFlowRateMinAvail = OAMassFlowRate;
		Node( AirRelNode ).MassFlowRateMaxAvail = OAMassFlowRate;

		Node( OAMixOutNode ).MassFlowRate = Node( InletNode ).MassFlowRate;
		Node( OAMixOutNode ).MassFlowRateMinAvail = Node( InletNode ).MassFlowRate;
		Node( OAMixOutNode ).MassFlowRateMaxAvail = Node( InletNode ).MassFlowRate;

		// "Inlet" conditions for InletNode and OutsideAirNode have already
		// been set elsewhere so we just need to set the "outlet" conditions
		Node( AirRelNode ).Temp = Node( InletNode ).Temp;
		Node( AirRelNode ).Press = Node( InletNode ).Press;
		Node( AirRelNode ).HumRat = Node( InletNode ).HumRat;
		Node( AirRelNode ).Enthalpy = Node( InletNode ).Enthalpy;

		if ( Node( InletNode ).MassFlowRate > 0.0 ) {

			OAFraction = Node( OutsideAirNode ).MassFlowRate / Node( InletNode ).MassFlowRate;

		} else {
			OAFraction = 0.0;
		}

		Node( InletNode ).Enthalpy = PsyHFnTdbW( Node( InletNode ).Temp, Node( InletNode ).HumRat );

		// Perform an energy and moisture mass balance on the mixing portion of the OA Mixer of the ventilated slab
		Node( OAMixOutNode ).Enthalpy = OAFraction * Node( OutsideAirNode ).Enthalpy + ( 1.0 - OAFraction ) * Node( InletNode ).Enthalpy;
		Node( OAMixOutNode ).HumRat = OAFraction * Node( OutsideAirNode ).HumRat + ( 1.0 - OAFraction ) * Node( InletNode ).HumRat;

		// Find the other key state points based on calculated conditions
		Node( OAMixOutNode ).Temp = PsyTdbFnHW( Node( OAMixOutNode ).Enthalpy, Node( OAMixOutNode ).HumRat );
		Node( OAMixOutNode ).Press = Node( InletNode ).Press;

	}

	void
	UpdateVentilatedSlab(
		int const Item, // Index for the ventilated slab under consideration within the derived types
		bool const EP_UNUSED( FirstHVACIteration ) // TRUE if 1st HVAC simulation of system timestep !unused1208
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Young Tae Chae, Rick Strand
		//       DATE WRITTEN   November 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine does any updating that needs to be done for low
		// temperature radiant heating and cooling systems.  One of the most
		// important functions of this routine is to update the average heat
		// source/sink for a particular system over the various system time
		// steps that make up the zone time step.  For hydronic systems,
		// this routine must also set the outlet water conditions.

		// METHODOLOGY EMPLOYED:
		// For the source/sink average update, if the system time step elapsed
		// is still what it used to be, then either we are still iterating or
		// we had to go back and shorten the time step.  As a result, we have
		// to subtract out the previous value that we added.  If the system
		// time step elapsed is different, then we just need to add the new
		// values to the running average.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::TimeStepZone;
		using DataHeatBalance::Zone;
		using DataHVACGlobals::TimeStepSys;
		using DataHVACGlobals::SysTimeElapsed;
		using DataLoopNode::Node;
		using DataHeatBalFanSys::MAT;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 CpAppAir; // Specific heat of air
		int RadSurfNum; // DO loop counter for radiant surfaces in the ventilated slab
		int SurfNum; // Surface index number for the current ventilated slab
		int AirInletNode; // Node number for the air side inlet of the ventilated slab
		Real64 TotalHeatSource; // Total heat source or sink for a particular system (sum of all surface source/sinks)
		int TotRadSurfaces; // Total number of radiant surfaces in this system
		Real64 AirMassFlow; // Flow rate of water in the radiant system
		int AirOutletNode; // Node number for the water side outlet of the radiant system
		int FanOutNode; // Node number for the water side outlet of the radiant system
		Real64 ZoneMult; // Zone multiplier
		int ZoneNum; // Zone for this ventilated slab
		int MixOutNode; // Node number for the water side outlet of the radiant system
		int OANode; // Node number for the water side outlet of the radiant system
		Real64 OAFraction; // Outside air fraction of inlet air
		int ZoneInletNode; // Node number for the air side inlet of the ventilated slab
		// FLOW:

		ZoneNum = VentSlab( Item ).ZonePtr;
		TotRadSurfaces = VentSlab( Item ).NumOfSurfaces;
		MixOutNode = VentSlab( Item ).OAMixerOutNode;
		OANode = VentSlab( Item ).OutsideAirNode;
		AirOutletNode = VentSlab( Item ).RadInNode;
		FanOutNode = VentSlab( Item ).FanOutletNode;
		AirMassFlow = Node( AirOutletNode ).MassFlowRate;
		ZoneInletNode = VentSlab( Item ).ZoneAirInNode;
		CpAppAir = PsyCpAirFnWTdb( Node( AirOutletNode ).HumRat, Node( AirOutletNode ).Temp );
		AirInletNode = VentSlab( Item ).ReturnAirNode;

		for ( RadSurfNum = 1; RadSurfNum <= TotRadSurfaces; ++RadSurfNum ) {

			SurfNum = VentSlab( Item ).SurfacePtr( RadSurfNum );

			if ( LastSysTimeElapsed( SurfNum ) == SysTimeElapsed ) {
				// Still iterating or reducing system time step, so subtract old values which were
				// not valid
				QRadSysSrcAvg( SurfNum ) -= LastQRadSysSrc( SurfNum ) * LastTimeStepSys( SurfNum ) / TimeStepZone;
			}

			// Update the running average and the "last" values with the current values of the appropriate variables
			QRadSysSrcAvg( SurfNum ) += QRadSysSource( SurfNum ) * TimeStepSys / TimeStepZone;

			LastQRadSysSrc( SurfNum ) = QRadSysSource( SurfNum );
			LastSysTimeElapsed( SurfNum ) = SysTimeElapsed;
			LastTimeStepSys( SurfNum ) = TimeStepSys;

		}

		// First sum up all of the heat sources/sinks associated with this system
		TotalHeatSource = 0.0;
		for ( RadSurfNum = 1; RadSurfNum <= VentSlab( Item ).NumOfSurfaces; ++RadSurfNum ) {
			SurfNum = VentSlab( Item ).SurfacePtr( RadSurfNum );
			TotalHeatSource += QRadSysSource( SurfNum );
		}
		ZoneNum = VentSlab( Item ).ZonePtr;
		ZoneMult = double( Zone( ZoneNum ).Multiplier * Zone( ZoneNum ).ListMultiplier );
		TotalHeatSource *= ZoneMult;

		// Update the heating side of things

		if ( ( CpAppAir > 0.0 ) && ( AirMassFlow > 0.0 ) ) {

			if ( ( VentSlab( Item ).SysConfg == SlabOnly ) || ( VentSlab( Item ).SysConfg == SeriesSlabs ) ) {
				Node( AirInletNode ) = Node( AirInletNode );
				Node( AirInletNode ).Temp = Node( AirOutletNode ).Temp - TotalHeatSource / AirMassFlow / CpAppAir;
				Node( AirInletNode ).MassFlowRate = Node( AirOutletNode ).MassFlowRate;
				Node( AirInletNode ).HumRat = Node( AirOutletNode ).HumRat;

			} else if ( VentSlab( Item ).SysConfg == SlabAndZone ) {
				Node( ZoneInletNode ) = Node( ZoneInletNode );
				Node( ZoneInletNode ).Temp = Node( AirOutletNode ).Temp - TotalHeatSource / AirMassFlow / CpAppAir;
				Node( ZoneInletNode ).MassFlowRate = Node( AirOutletNode ).MassFlowRate;
				Node( ZoneInletNode ).HumRat = Node( AirOutletNode ).HumRat;
				Node( VentSlab( Item ).ReturnAirNode ).Temp = MAT( ZoneNum );
			}

		} else {
			if ( ( VentSlab( Item ).SysConfg == SlabOnly ) || ( VentSlab( Item ).SysConfg == SeriesSlabs ) ) {
				Node( FanOutNode ) = Node( AirOutletNode );
				QRadSysSource( SurfNum ) = 0.0;

			} else if ( VentSlab( Item ).SysConfg == SlabAndZone ) {
				Node( ZoneInletNode ) = Node( AirInletNode );
				Node( FanOutNode ) = Node( AirOutletNode ); // Fan Resolve
				QRadSysSource( SurfNum ) = 0.0;
			}

		}

		// Resolve mixouttemp

		if ( Node( AirInletNode ).MassFlowRate > 0.0 ) {

			OAFraction = Node( OANode ).MassFlowRate / Node( AirInletNode ).MassFlowRate;

		} else {
			OAFraction = 0.0;
		}

		if ( OAFraction <= 0.0 ) {

			Node( MixOutNode ).HumRat = Node( AirInletNode ).HumRat;
			Node( MixOutNode ).Temp = Node( AirInletNode ).Temp;

		} else {

			Node( MixOutNode ).Enthalpy = OAFraction * Node( OANode ).Enthalpy + ( 1.0 - OAFraction ) * Node( AirInletNode ).Enthalpy;
			Node( MixOutNode ).HumRat = OAFraction * Node( OANode ).HumRat + ( 1.0 - OAFraction ) * Node( AirInletNode ).HumRat;

			Node( MixOutNode ).Temp = PsyTdbFnHW( Node( MixOutNode ).Enthalpy, Node( MixOutNode ).HumRat );

		}

	}

	Real64
	CalcVentSlabHXEffectTerm(
		int const Item, // Index number of radiant system under consideration
		Real64 const Temperature, // Temperature of air entering the radiant system, in C
		Real64 const AirMassFlow, // Mass flow rate of water in the radiant system, in kg/s
		Real64 const FlowFraction, // Mass flow rate fraction for this surface in the radiant system
		Real64 const CoreLength, // Length of tubing in the radiant system, in m
		Real64 const CoreDiameter, // Inside diameter of the tubing in the radiant system, in m
		Real64 const CoreNumbers
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   December 2000
		//       MODIFIED       June 2008 (air properties)
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine calculates the radiant system "heat exchanger"
		// effectiveness term.  This is equal to the mass flow rate of water
		// times the specific heat of water times the effectiveness of
		// the heat exchanger (radiant system "coil").

		// METHODOLOGY EMPLOYED:
		// Assumes that the only real heat transfer term that we have to
		// deal with is the convection from the water to the tube.  The
		// other assumptions are that the tube inside surface temperature
		// is equal to the "source location temperature" and that it is
		// a CONSTANT throughout the radiant system.  This is to make
		// the problem more tractable and to fit with other system assumptions
		// that were made elsewhere in the radiant system model.

		// REFERENCES:
		// Property data for air shown below as parameters taken from
		//   Mills, Heat Transfer, Table A.7.
		// Heat exchanger information also from Incropera and DeWitt.
		// Code based loosely on code from IBLAST program (research version)

		// Using/Aliasing
		using DataGlobals::Pi;

		// Return value
		Real64 CalcVentSlabHXEffectTerm;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const MaxLaminarRe( 2300.0 ); // Maximum Reynolds number for laminar flow
		int const NumOfPropDivisions( 13 );
		Real64 const MaxExpPower( 50.0 ); // Maximum power after which EXP argument would be zero for DP variables
		static Array1D< Real64 > const Temps( NumOfPropDivisions, { 1.85, 6.85, 11.85, 16.85, 21.85, 26.85, 31.85, 36.85, 41.85, 46.85, 51.85, 56.85, 61.85 } ); // Temperature, in C
		static Array1D< Real64 > const Mu( NumOfPropDivisions, { 0.0000088, 0.0000176, 0.00001781, 0.00001802, 0.000018225, 0.00001843, 0.00001865, 0.00001887, 0.00001908, 0.00001929, 0.0000195, 0.00001971, 0.00001992 } ); // Viscosity, in Ns/m2
		static Array1D< Real64 > const Conductivity( NumOfPropDivisions, { 0.01275, 0.0255, 0.0258, 0.0261, 0.0264, 0.0267, 0.02705, 0.0274, 0.02775, 0.0281, 0.0284, 0.0287, 0.01435 } ); // Conductivity, in W/mK
		static Array1D< Real64 > const Pr( NumOfPropDivisions, 0.69 ); // Prandtl number (dimensionless)

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Index;
		Real64 InterpFrac;
		Real64 NuD;
		Real64 ReD;
		Real64 NTU;
		Real64 CpAppAir;
		Real64 Kactual;
		Real64 MUactual;
		Real64 PRactual;
		Real64 SysAirMassFlow; // Specific heat of air

		// FLOW:
		// First find out where we are in the range of temperatures
		Index = 1;
		while ( Index <= NumOfPropDivisions ) {
			if ( Temperature < Temps( Index ) ) break; // DO loop
			++Index;
		}

		// Initialize thermal properties of Air
		if ( Index == 1 ) {
			MUactual = Mu( Index );
			Kactual = Conductivity( Index );
			PRactual = Pr( Index );
		} else if ( Index > NumOfPropDivisions ) {
			Index = NumOfPropDivisions;
			MUactual = Mu( Index );
			Kactual = Conductivity( Index );
			PRactual = Pr( Index );
		} else {
			InterpFrac = ( Temperature - Temps( Index - 1 ) ) / ( Temps( Index ) - Temps( Index - 1 ) );
			MUactual = Mu( Index - 1 ) + InterpFrac * ( Mu( Index ) - Mu( Index - 1 ) );
			Kactual = Conductivity( Index - 1 ) + InterpFrac * ( Conductivity( Index ) - Conductivity( Index - 1 ) );
			PRactual = Pr( Index - 1 ) + InterpFrac * ( Pr( Index ) - Pr( Index - 1 ) );
		}
		// arguments are glycol name, temperature, and concentration
		CpAppAir = PsyCpAirFnWTdb( Node( VentSlab( Item ).RadInNode ).HumRat, Node( VentSlab( Item ).RadInNode ).Temp );
		SysAirMassFlow = AirMassFlow / CoreNumbers;

		// Calculate the Reynold's number from RE=(4*Mdot)/(Pi*Mu*Diameter)
		ReD = 4.0 * SysAirMassFlow * FlowFraction / ( Pi * MUactual * CoreDiameter );

		// Calculate the Nusselt number based on what flow regime one is in
		if ( ReD >= MaxLaminarRe ) { // Turbulent flow --> use Colburn equation

			NuD = 0.023 * std::pow( ReD, 0.8 ) * std::pow( PRactual, 1.0 / 3.0 );

		} else { // Laminar flow --> use constant surface temperature relation

			NuD = 3.66;

		}

		// Calculate the NTU parameter
		// NTU = UA/[(Mdot*Cp)min]
		// where: U = h (convection coefficient) and h = (k)(Nu)/D
		//        A = Pi*D*TubeLength
		NTU = Pi * Kactual * NuD * CoreLength / ( SysAirMassFlow * CpAppAir ); // FlowFraction cancels out here

		// Calculate Epsilon*MassFlowRate*Cp
		if ( NTU > MaxExpPower ) {
			CalcVentSlabHXEffectTerm = FlowFraction * SysAirMassFlow * CpAppAir;
		} else {
			CalcVentSlabHXEffectTerm = ( 1.0 - std::exp( -NTU ) ) * FlowFraction * SysAirMassFlow * CpAppAir;
		}

		return CalcVentSlabHXEffectTerm;

	}

	Real64
	SumHATsurf( int const ZoneNum ) // Zone number
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   July 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function calculates the zone sum of Hc*Area*Tsurf.  It replaces the old SUMHAT.
		// The SumHATsurf code below is also in the CalcZoneSums subroutine in ZoneTempPredictorCorrector
		// and should be updated accordingly.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSurfaces;
		using namespace DataHeatBalance;
		using namespace DataHeatBalSurface;

		// Return value
		Real64 SumHATsurf;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int SurfNum; // Surface number
		Real64 Area; // Effective surface area

		// FLOW:
		SumHATsurf = 0.0;

		for ( SurfNum = Zone( ZoneNum ).SurfaceFirst; SurfNum <= Zone( ZoneNum ).SurfaceLast; ++SurfNum ) {
			if ( ! Surface( SurfNum ).HeatTransSurf ) continue; // Skip non-heat transfer surfaces

			Area = Surface( SurfNum ).Area;

			if ( Surface( SurfNum ).Class == SurfaceClass_Window ) {
				if ( SurfaceWindow( SurfNum ).ShadingFlag == IntShadeOn || SurfaceWindow( SurfNum ).ShadingFlag == IntBlindOn ) {
					// The area is the shade or blind are = sum of the glazing area and the divider area (which is zero if no divider)
					Area += SurfaceWindow( SurfNum ).DividerArea;
				}

				if ( SurfaceWindow( SurfNum ).FrameArea > 0.0 ) {
					// Window frame contribution
					SumHATsurf += HConvIn( SurfNum ) * SurfaceWindow( SurfNum ).FrameArea * ( 1.0 + SurfaceWindow( SurfNum ).ProjCorrFrIn ) * SurfaceWindow( SurfNum ).FrameTempSurfIn;
				}

				if ( SurfaceWindow( SurfNum ).DividerArea > 0.0 && SurfaceWindow( SurfNum ).ShadingFlag != IntShadeOn && SurfaceWindow( SurfNum ).ShadingFlag != IntBlindOn ) {
					// Window divider contribution (only from shade or blind for window with divider and interior shade or blind)
					SumHATsurf += HConvIn( SurfNum ) * SurfaceWindow( SurfNum ).DividerArea * ( 1.0 + 2.0 * SurfaceWindow( SurfNum ).ProjCorrDivIn ) * SurfaceWindow( SurfNum ).DividerTempSurfIn;
				}
			}

			SumHATsurf += HConvIn( SurfNum ) * Area * TempSurfInTmp( SurfNum );
		}

		return SumHATsurf;

	}

	void
	ReportVentilatedSlab( int const Item ) // Index for the ventilated slab under consideration within the derived types
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   November 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine simply produces output for the low temperature radiant system.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::SecInHour;
		using DataHeatBalance::Zone;
		using DataHVACGlobals::TimeStepSys;
		using DataLoopNode::Node;
		using DataSurfaces::Surface;
		//unused-12/12/08  USE FluidProperties, ONLY : GetSpecificHeatGlycol

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int RadSurfNum; // DO loop counter for radiant surfaces in the system
		int SurfNum; // Surface number (index) in Surface derived type
		Real64 TotalVentSlabRadPower; // Total source/sink power for the radiant system (sum of all surfaces of the system)
		Real64 ZoneMult; // Total zone multiplier to apply to the system level variables

		// FLOW:

		// Slab Part
		TotalVentSlabRadPower = 0.0;
		ZoneMult = 1.0;

		for ( RadSurfNum = 1; RadSurfNum <= VentSlab( Item ).NumOfSurfaces; ++RadSurfNum ) {
			SurfNum = VentSlab( Item ).SurfacePtr( RadSurfNum );
			TotalVentSlabRadPower += QRadSysSource( SurfNum );
		}
		ZoneMult = double( Zone( VentSlab( Item ).ZonePtr ).Multiplier * Zone( VentSlab( Item ).ZonePtr ).ListMultiplier );
		TotalVentSlabRadPower *= ZoneMult;
		VentSlab( Item ).RadHeatingPower = 0.0;
		VentSlab( Item ).RadCoolingPower = 0.0;

		if ( TotalVentSlabRadPower >= 0.01 ) {

			VentSlab( Item ).RadHeatingPower = +TotalVentSlabRadPower;
		} else {

			VentSlab( Item ).RadCoolingPower = -TotalVentSlabRadPower;
		}

		VentSlab( Item ).RadHeatingEnergy = VentSlab( Item ).RadHeatingPower * TimeStepSys * SecInHour;
		VentSlab( Item ).RadCoolingEnergy = VentSlab( Item ).RadCoolingPower * TimeStepSys * SecInHour;

		//Coil Part
		VentSlab( Item ).HeatCoilEnergy = VentSlab( Item ).HeatCoilPower * TimeStepSys * SecInHour;
		VentSlab( Item ).SensCoolCoilEnergy = VentSlab( Item ).SensCoolCoilPower * TimeStepSys * SecInHour;
		VentSlab( Item ).LateCoolCoilEnergy = VentSlab( Item ).LateCoolCoilPower * TimeStepSys * SecInHour;
		VentSlab( Item ).TotCoolCoilEnergy = VentSlab( Item ).TotCoolCoilPower * TimeStepSys * SecInHour;
		VentSlab( Item ).ElecFanEnergy = VentSlab( Item ).ElecFanPower * TimeStepSys * SecInHour;

		if ( ( VentSlab( Item ).SysConfg == SlabOnly ) || ( VentSlab( Item ).SysConfg == SeriesSlabs ) ) {
			VentSlab( Item ).SlabInTemp = Node( VentSlab( Item ).RadInNode ).Temp;
			VentSlab( Item ).SlabOutTemp = Node( VentSlab( Item ).ReturnAirNode ).Temp;

		} else if ( VentSlab( Item ).SysConfg == SlabAndZone ) {
			VentSlab( Item ).SlabInTemp = Node( VentSlab( Item ).RadInNode ).Temp;
			VentSlab( Item ).ZoneInletTemp = Node( VentSlab( Item ).ZoneAirInNode ).Temp;
			VentSlab( Item ).SlabOutTemp = Node( VentSlab( Item ).ReturnAirNode ).Temp;
		}

		VentSlab( Item ).ReturnAirTemp = Node( VentSlab( Item ).ReturnAirNode ).Temp;
		VentSlab( Item ).FanOutletTemp = Node( VentSlab( Item ).FanOutletNode ).Temp;

	}

	//*****************************************************************************************

} // VentilatedSlab

} // EnergyPlus
