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
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <LowTempRadiantSystem.hh>
#include <BranchNodeConnections.hh>
#include <DataBranchAirLoopPlant.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHeatBalSurface.hh>
#include <DataHVACGlobals.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DataSurfaceLists.hh>
#include <DataSurfaces.hh>
#include <DataZoneEnergyDemands.hh>
#include <DataZoneEquipment.hh>
#include <EMSManager.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <HeatBalanceSurfaceManager.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <PlantUtilities.hh>
#include <Psychrometrics.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace LowTempRadiantSystem {

	// Module containing the routines dealing with the low temperature radiant systems

	// MODULE INFORMATION:
	//       AUTHOR         Rick Strand
	//       DATE WRITTEN   November 2000
	//       MODIFIED       Rick Strand March 2001 (additional controls, etc.)
	//                      Rick Strand July 2003 (added constant flow hydronic system)
	//                      B. Griffith Sept 2010, plant upgrades, generalize fluid properties
	//                      Rick Strand August 2011 (improved condensation handling)
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// The purpose of this module is to simulate low temperature radiant systems.
	// It is the intention of this module to cover all types of low temperature
	// radiant systems: wall, ceiling, floor, heating, cooling, panels, etc.

	// METHODOLOGY EMPLOYED:
	// Based on work done in IBLAST, this model has been revised for the structure
	// of EnergyPlus.  It is still based on the QTF formulation of heat transfer
	// through building elements with embedded heat sources/sinks.  Note that due
	// to the fact that a radiant system is both a building heat transfer element
	// and a controllable system that some iteration between the system and the
	// surface heat balance routine is necessary.
	// REFERENCES:
	// IBLAST-QTF research program, completed in January 1995 (unreleased)
	// Strand, R.K. 1995. "Heat Source Transfer Functions and Their Application to
	//   Low Temperature Radiant Heating Systems", Ph.D. dissertation, University
	//   of Illinois at Urbana-Champaign, Department of Mechanical and Industrial
	//   Engineering.
	// Seem, J.E. 1986. "Heat Transfer in Buildings", Ph.D. dissertation, University
	//   of Wisconsin-Madison.

	// OTHER NOTES: This module contains three different types of radiant system
	// models: (a) variable flow hydronic heating/cooling radiant system;
	// (b) constant flow, variable controlled temperature heating/cooling radiant
	// system; (c) electric resistance heating radiant system.  Systems (a) and
	// (b) are hydronic systems--one which varies hydronic flow as the key control
	// paramter (a) and one which varies the inlet hydronic temperature while
	// keeping the flow rate through the radiant system constant (b).  In system
	// (b), the injection rate from the main water loop is varied to obtain the
	// proper inlet temperature.

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::BeginTimeStepFlag;
	using DataGlobals::InitConvTemp;
	using DataGlobals::SysSizingCalc;
	using DataGlobals::WarmupFlag;
	using DataGlobals::DisplayExtraWarnings;

	using DataHeatBalance::Material;
	using DataHeatBalance::TotMaterials;
	using DataHeatBalance::MaxLayersInConstruct;
	using DataHeatBalance::QRadThermInAbs;
	using DataHeatBalance::Construct;
	using DataHeatBalance::TotConstructs;
	using DataHeatBalance::RegularMaterial;
	using DataHeatBalance::Air;

	using DataSurfaces::Surface;
	using DataSurfaces::TotSurfaces;
	using DataSurfaces::HeatTransferModel_CTF;
	using DataHeatBalFanSys::QRadSysSource; // Heat source/sink value & temperature for CondFD algo.
	using DataHeatBalFanSys::TCondFDSourceNode;
	using DataHVACGlobals::SmallLoad;

	// Use statements for access to subroutines in other modules
	using Psychrometrics::PsyTdpFnWPb;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// System types:
	int const HydronicSystem( 1 ); // Variable flow hydronic radiant system
	int const ConstantFlowSystem( 2 ); // Constant flow, variable (controlled) temperature radiant system
	int const ElectricSystem( 3 ); // Electric resistance radiant heating system
	std::string const cHydronicSystem( "ZoneHVAC:LowTemperatureRadiant:VariableFlow" );
	std::string const cConstantFlowSystem( "ZoneHVAC:LowTemperatureRadiant:ConstantFlow" );
	std::string const cElectricSystem( "ZoneHVAC:LowTemperatureRadiant:Electric" );
	// Operating modes:
	int const NotOperating( 0 ); // Parameter for use with OperatingMode variable, set for heating
	int const HeatingMode( 1 ); // Parameter for use with OperatingMode variable, set for heating
	int const CoolingMode( 2 ); // Parameter for use with OperatingMode variable, set for cooling
	// Control types:
	int const MATControl( 1 ); // Controls system using mean air temperature
	int const MRTControl( 2 ); // Controls system using mean radiant temperature
	int const OperativeControl( 3 ); // Controls system using operative temperature
	int const ODBControl( 4 ); // Controls system using outside air dry-bulb temperature
	int const OWBControl( 5 ); // Controls system using outside air wet-bulb temperature
	// Condensation control types:
	int const CondCtrlNone( 0 ); // Condensation control--none, so system never shuts down
	int const CondCtrlSimpleOff( 1 ); // Condensation control--simple off, system shuts off when condensation predicted
	int const CondCtrlVariedOff( 2 ); // Condensation control--variable off, system modulates to keep running if possible
	// Number of Circuits per Surface Calculation Method
	int const OneCircuit( 1 ); // there is 1 circuit per surface
	int const CalculateFromLength( 2 ); // The number of circuits is TubeLength*SurfaceFlowFrac / CircuitLength
	std::string const OnePerSurf( "OnePerSurface" );
	std::string const CalcFromLength( "CalculateFromCircuitLength" );
	// Limit temperatures to indicate that a system cannot heat or cannot cool
	Real64 LowTempHeating( -200.0 ); // Used to indicate that a user does not have a heating control temperature
	Real64 HighTempCooling( 200.0 ); // Used to indicate that a user does not have a cooling control temperature

	static std::string const fluidNameWater( "WATER" );
	static std::string const BlankString;

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	// Standard, run-of-the-mill variables...
	int NumOfHydrLowTempRadSys( 0 ); // Number of hydronic low tempererature radiant systems
	int NumOfCFloLowTempRadSys( 0 ); // Number of constant flow (hydronic) low tempererature radiant systems
	int NumOfElecLowTempRadSys( 0 ); // Number of electric low tempererature radiant systems
	int CFloCondIterNum( 0 ); // Number of iterations for a constant flow radiant system--controls variable cond sys ctrl
	int TotalNumOfRadSystems( 0 ); // Total number of low temperature radiant systems
	int OperatingMode( 0 ); // Used to keep track of whether system is in heating or cooling mode
	int MaxCloNumOfSurfaces( 0 ); // Used to set allocate size in CalcClo routine
	bool VarOffCond( false ); // Set to true when in cooling for constant flow system + variable off condensation predicted
	Real64 LoopReqTemp( 0.0 ); // Temperature required at the inlet of the pump (from the loop) to meet control logic
	Array1D< Real64 > QRadSysSrcAvg; // Average source over the time step for a particular radiant surface
	Array1D< Real64 > ZeroSourceSumHATsurf; // Equal to SumHATsurf for all the walls in a zone with no source
	// Record keeping variables used to calculate QRadSysSrcAvg locally
	Array1D< Real64 > LastQRadSysSrc; // Need to keep the last value in case we are still iterating
	Array1D< Real64 > LastSysTimeElapsed; // Need to keep the last value in case we are still iterating
	Array1D< Real64 > LastTimeStepSys; // Need to keep the last value in case we are still iterating
	// Autosizing variables
	Array1D_bool MySizeFlagHydr;
	Array1D_bool MySizeFlagCFlo;
	Array1D_bool MySizeFlagElec;
	Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE LowTempRadiantSystem

	// Object Data
	Array1D< HydronicRadiantSystemData > HydrRadSys;
	Array1D< ConstantFlowRadiantSystemData > CFloRadSys;
	Array1D< ElectricRadiantSystemData > ElecRadSys;
	Array1D< RadSysTypeData > RadSysTypes;
	Array1D< ElecRadSysNumericFieldData > ElecRadSysNumericFields;
	Array1D< HydronicRadiantSysNumericFieldData > HydronicRadiantSysNumericFields;

	// Functions

	void
	clear_state()
	{
		LowTempHeating = -200.0;
		HighTempCooling = 200.0;
		NumOfHydrLowTempRadSys = 0;
		NumOfCFloLowTempRadSys = 0;
		NumOfElecLowTempRadSys = 0;
		CFloCondIterNum = 0;
		TotalNumOfRadSystems = 0;
		OperatingMode = 0;
		MaxCloNumOfSurfaces = 0;
		VarOffCond = false;
		LoopReqTemp = 0.0;
		QRadSysSrcAvg.deallocate();
		ZeroSourceSumHATsurf.deallocate();
		LastQRadSysSrc.deallocate();
		LastSysTimeElapsed.deallocate();
		LastTimeStepSys.deallocate();
		MySizeFlagHydr.deallocate();
		MySizeFlagCFlo.deallocate();
		MySizeFlagElec.deallocate();
		CheckEquipName.deallocate();
		HydrRadSys.deallocate();
		CFloRadSys.deallocate();
		ElecRadSys.deallocate();
		RadSysTypes.deallocate();
		ElecRadSysNumericFields.deallocate();
		HydronicRadiantSysNumericFields.deallocate();
	}

	void
	SimLowTempRadiantSystem(
		std::string const & CompName, // name of the low temperature radiant system
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & LoadMet, // load met by the radiant system, in Watts
		int & CompIndex
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   November 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine needs a description.

		// METHODOLOGY EMPLOYED:
		// Needs description, as appropriate.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using General::TrimSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool GetInputFlag( true ); // First time, input is "gotten"
		int RadSysNum; // Radiant system number/index in local derived types
		int SystemType; // Type of radiant system: hydronic, constant flow, or electric

		// FLOW:
		if ( GetInputFlag ) {
			GetLowTempRadiantSystem();
			GetInputFlag = false;
		}

		//          ! Get the radiant system index and type
		//  RadSysNum = FindItemInList(CompName,HydrRadSys%Name,NumOfHydrLowTempRadSys)
		//  IF (RadSysNum > 0) THEN  ! Found it and it is a hydronic system
		//    SystemType = HydronicSystem
		//  ELSE ! RadSysNum <= 0 so this CompName was not found among the hydronic systems-->check out electric systems
		//    RadSysNum = FindItemInList(CompName,ElecRadSys%Name,NumOfElecLowTempRadSys)
		//    IF (RadSysNum > 0) THEN  ! Found it and it is an electric system
		//      SystemType = ElectricSystem
		//    ELSE    ! RadSysNum <= 0 so this CompName was not found among the hydronic systems-->check out constant flow systems
		//      RadSysNum = FindItemInList(CompName,CFloRadSys%Name,NumOfCFloLowTempRadSys)
		//      IF (RadSysNum > 0) THEN  ! Found it and it is an electric system
		//        SystemType = ConstantFlowSystem
		//      ELSE  ! RadSysNum is still <= 0 so this CompName was not found among either radiant system type-->error
		//        CALL ShowFatalError('SimLowTempRadiantSystem: Radiant System not found = '//TRIM(CompName))
		//      END IF
		//    END IF
		//  END IF

		// Find the correct High Temp Radiant System
		if ( CompIndex == 0 ) {
			RadSysNum = FindItemInList( CompName, RadSysTypes );
			if ( RadSysNum == 0 ) {
				ShowFatalError( "SimLowTempRadiantSystem: Unit not found=" + CompName );
			}
			CompIndex = RadSysNum;
			SystemType = RadSysTypes( RadSysNum ).SystemType;
			{ auto const SELECT_CASE_var( SystemType );
			if ( SELECT_CASE_var == HydronicSystem ) {
				RadSysTypes( RadSysNum ).CompIndex = FindItemInList( CompName, HydrRadSys );
			} else if ( SELECT_CASE_var == ConstantFlowSystem ) {
				RadSysTypes( RadSysNum ).CompIndex = FindItemInList( CompName, CFloRadSys );
			} else if ( SELECT_CASE_var == ElectricSystem ) {
				RadSysTypes( RadSysNum ).CompIndex = FindItemInList( CompName, ElecRadSys );
			}}
		} else {
			RadSysNum = CompIndex;
			SystemType = RadSysTypes( RadSysNum ).SystemType;
			if ( RadSysNum > TotalNumOfRadSystems || RadSysNum < 1 ) {
				ShowFatalError( "SimLowTempRadiantSystem:  Invalid CompIndex passed=" + TrimSigDigits( RadSysNum ) + ", Number of Units=" + TrimSigDigits( TotalNumOfRadSystems ) + ", Entered Unit name=" + CompName );
			}
			if ( CheckEquipName( RadSysNum ) ) {
				if ( CompName != RadSysTypes( RadSysNum ).Name ) {
					ShowFatalError( "SimLowTempRadiantSystem: Invalid CompIndex passed=" + TrimSigDigits( RadSysNum ) + ", Unit name=" + CompName + ", stored Unit Name for that index=" + RadSysTypes( RadSysNum ).Name );
				}
				CheckEquipName( RadSysNum ) = false;
			}
		}

		InitLowTempRadiantSystem( FirstHVACIteration, RadSysTypes( RadSysNum ).CompIndex, SystemType );

		{ auto const SELECT_CASE_var( SystemType );
		if ( SELECT_CASE_var == HydronicSystem ) {
			CalcLowTempHydrRadiantSystem( RadSysTypes( RadSysNum ).CompIndex, LoadMet );
		} else if ( SELECT_CASE_var == ConstantFlowSystem ) {
			CalcLowTempCFloRadiantSystem( RadSysTypes( RadSysNum ).CompIndex, LoadMet );
		} else if ( SELECT_CASE_var == ElectricSystem ) {
			CalcLowTempElecRadiantSystem( RadSysTypes( RadSysNum ).CompIndex, LoadMet );
		} else {
			ShowFatalError( "SimLowTempRadiantSystem: Illegal system type for system " + CompName );
		}}

		UpdateLowTempRadiantSystem( FirstHVACIteration, RadSysTypes( RadSysNum ).CompIndex, SystemType );

		ReportLowTempRadiantSystem( RadSysTypes( RadSysNum ).CompIndex, SystemType );

	}

	void
	GetLowTempRadiantSystem()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   November 2000
		//       MODIFIED       August 2003 (added constant flow system, made input extensible)
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine reads the input for low temperature radiant systems
		// from the user input file.  This will contain all of the information
		// needed to simulate a low temperature radiant system.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// REFERENCES:
		// na

		// Using/Aliasing
		using BranchNodeConnections::TestCompSet;
		using DataGlobals::AnyEnergyManagementSystemInModel;
		using DataGlobals::ScheduleAlwaysOn;
		using DataHeatBalance::Zone;
		using DataHeatBalance::Construct;
		using DataSizing::AutoSize;
		using DataSizing::HeatingDesignCapacity;
		using DataSizing::CapacityPerFloorArea;
		using DataSizing::FractionOfAutosizedHeatingCapacity;
		using DataSizing::CoolingDesignCapacity;
		using DataSizing::FractionOfAutosizedCoolingCapacity;
		using FluidProperties::FindGlycol;
		using General::TrimSigDigits;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using InputProcessor::SameString;
		using InputProcessor::GetObjectDefMaxArgs;
		using InputProcessor::VerifyName;
		using NodeInputManager::GetOnlySingleNode;
		using ScheduleManager::GetScheduleIndex;
		using namespace DataLoopNode;
		using namespace DataSurfaceLists;




		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		//  REAL(r64),        PARAMETER :: FlowFractionTolerance = 0.0001 ! Smallest deviation from unity for the sum of all fractions
		Real64 const MinThrottlingRange( 0.5 ); // Smallest throttling range allowed in degrees Celsius
		static std::string const MeanAirTemperature( "MeanAirTemperature" );
		static std::string const MeanRadiantTemperature( "MeanRadiantTemperature" );
		static std::string const OperativeTemperature( "OperativeTemperature" );
		static std::string const OutsideAirDryBulbTemperature( "OutdoorDryBulbTemperature" );
		static std::string const OutsideAirWetBulbTemperature( "OutdoorWetBulbTemperature" );
		static std::string const RoutineName( "GetLowTempRadiantSystem: " ); // include trailing blank space
		static std::string const Off( "Off" );
		static std::string const SimpleOff( "SimpleOff" );
		static std::string const VariableOff( "VariableOff" );
		int const iHeatCAPMAlphaNum( 5 ); // get input index to Low Temperature Radiant system heating capacity sizing method
		int const iHeatDesignCapacityNumericNum( 1 ); // get input index to Low Temperature Radiant system electric heating capacity
		int const iHeatCapacityPerFloorAreaNumericNum( 2 ); // get input index to Low Temperature Radiant system electric heating capacity per floor area sizing
		int const iHeatFracOfAutosizedCapacityNumericNum( 3 ); //  get input index to Low Temperature Radiant system electric heating capacity sizing as fraction of autozized heating capacity

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		std::string CurrentModuleObject; // for ease in getting objects
		Array1D_string Alphas; // Alpha items for object
		Array1D_string cAlphaFields; // Alpha field names
		Array1D_string cNumericFields; // Numeric field names
		Array1D_bool AssignedAsRadiantSurface; // Set to true when a surface is part of a radiant system
		int CheckSurfNum; // Surface number to check to see if it has already been used by a radiant system
		static bool ErrorsFound( false ); // Set to true if errors in input, fatal at end of routine
		int GlycolIndex; // Index of 'Water' in glycol data structure
		int IOStatus; // Used in GetObjectItem
		int Item; // Item to be "gotten"
		int MaxAlphas; // Maximum number of alphas for these input keywords
		int MaxNumbers; // Maximum number of numbers for these input keywords
		//unused1208  INTEGER    :: NameConflict ! Used to see if a surface name matches the name of a surface list (not allowed)
		Array1D< Real64 > Numbers; // Numeric items for object
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumArgs; // Unused variable that is part of a subroutine call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		//unused1208  REAL(r64)  :: SumOfAllFractions   ! Summation of all of the fractions for splitting flow (must sum to 1)
		int SurfListNum; // Index within the SurfList derived type for a surface list name
		int SurfNum; // DO loop counter for surfaces
		//unused1208  INTEGER    :: ZoneForSurface  ! Zone number that a particular surface is attached to
		int BaseNum; // Temporary number for creating RadiantSystemTypes structure
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		Array1D_bool lAlphaBlanks; // Logical array, alpha field input BLANK = .TRUE.
		Array1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.

		// FLOW:
		// Initializations and allocations
		MaxAlphas = 0;
		MaxNumbers = 0;

		GetObjectDefMaxArgs( "ZoneHVAC:LowTemperatureRadiant:VariableFlow", NumArgs, NumAlphas, NumNumbers );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		MaxNumbers = max( MaxNumbers, NumNumbers );

		GetObjectDefMaxArgs( "ZoneHVAC:LowTemperatureRadiant:ConstantFlow", NumArgs, NumAlphas, NumNumbers );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		MaxNumbers = max( MaxNumbers, NumNumbers );

		GetObjectDefMaxArgs( "ZoneHVAC:LowTemperatureRadiant:Electric", NumArgs, NumAlphas, NumNumbers );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		MaxNumbers = max( MaxNumbers, NumNumbers );

		Alphas.allocate( MaxAlphas );
		Numbers.dimension( MaxNumbers, 0.0 );
		cAlphaFields.allocate( MaxAlphas );
		cNumericFields.allocate( MaxNumbers );
		lAlphaBlanks.dimension( MaxAlphas, true );
		lNumericBlanks.dimension( MaxNumbers, true );

		NumOfHydrLowTempRadSys = GetNumObjectsFound( "ZoneHVAC:LowTemperatureRadiant:VariableFlow" );
		NumOfCFloLowTempRadSys = GetNumObjectsFound( "ZoneHVAC:LowTemperatureRadiant:ConstantFlow" );
		NumOfElecLowTempRadSys = GetNumObjectsFound( "ZoneHVAC:LowTemperatureRadiant:Electric" );

		TotalNumOfRadSystems = NumOfHydrLowTempRadSys + NumOfElecLowTempRadSys + NumOfCFloLowTempRadSys;
		RadSysTypes.allocate( TotalNumOfRadSystems );
		CheckEquipName.dimension( TotalNumOfRadSystems, true );

		HydrRadSys.allocate( NumOfHydrLowTempRadSys );
		if ( NumOfHydrLowTempRadSys > 0 ) {
			GlycolIndex = FindGlycol( fluidNameWater );
			for ( auto & e : HydrRadSys ) e.GlycolIndex = GlycolIndex;
			if ( GlycolIndex == 0 ) {
				ShowSevereError( "Hydronic radiant systems: no water property data found in input" );
				ErrorsFound = true;
			}
		} else {
			for ( auto & e : HydrRadSys ) e.GlycolIndex = 0;
		}

		CFloRadSys.allocate( NumOfCFloLowTempRadSys );
		if ( NumOfCFloLowTempRadSys > 0 ) {
			GlycolIndex = FindGlycol( fluidNameWater );
			for ( auto & e : CFloRadSys ) e.GlycolIndex = GlycolIndex;
			if ( GlycolIndex == 0 ) {
				ShowSevereError( "Constant flow radiant systems: no water property data found in input" );
				ErrorsFound = true;
			}
		} else {
			for ( auto & e : CFloRadSys ) e.GlycolIndex = 0;
		}

		ElecRadSys.allocate( NumOfElecLowTempRadSys );
		ElecRadSysNumericFields.allocate( NumOfElecLowTempRadSys );
		HydronicRadiantSysNumericFields.allocate( NumOfHydrLowTempRadSys );

		// make sure data is gotten for surface lists
		GetNumberOfSurfaceLists();

		// Obtain all of the user data related to hydronic low temperature radiant systems...
		BaseNum = 0;
		CurrentModuleObject = "ZoneHVAC:LowTemperatureRadiant:VariableFlow";
		for ( Item = 1; Item <= NumOfHydrLowTempRadSys; ++Item ) {

			GetObjectItem( CurrentModuleObject, Item, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );


			HydronicRadiantSysNumericFields( Item ).FieldNames.allocate( NumNumbers );
			HydronicRadiantSysNumericFields( Item ).FieldNames = "";
			HydronicRadiantSysNumericFields( Item ).FieldNames = cNumericFields;

			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), RadSysTypes, BaseNum, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			++BaseNum;
			RadSysTypes( BaseNum ).Name = Alphas( 1 );
			RadSysTypes( BaseNum ).SystemType = HydronicSystem;
			// General user input data
			HydrRadSys( Item ).Name = Alphas( 1 );

			HydrRadSys( Item ).SchedName = Alphas( 2 );
			if ( lAlphaBlanks( 2 ) ) {
				HydrRadSys( Item ).SchedPtr = ScheduleAlwaysOn;
			} else {
				HydrRadSys( Item ).SchedPtr = GetScheduleIndex( Alphas( 2 ) );
				if ( HydrRadSys( Item ).SchedPtr == 0 ) {
					ShowSevereError( cAlphaFields( 2 ) + " not found for " + Alphas( 1 ) );
					ShowContinueError( "Missing " + cAlphaFields( 2 ) + " is " + Alphas( 2 ) );
					ErrorsFound = true;
				}
			}

			HydrRadSys( Item ).ZoneName = Alphas( 3 );
			HydrRadSys( Item ).ZonePtr = FindItemInList( Alphas( 3 ), Zone );
			if ( HydrRadSys( Item ).ZonePtr == 0 ) {
				ShowSevereError( RoutineName + "Invalid " + cAlphaFields( 3 ) + " = " + Alphas( 3 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
				ErrorsFound = true;
				//    ELSEIF (Zone(HydrRadSys(Item)%ZonePtr)%Multiplier > 1 .or. Zone(HydrRadSys(Item)%ZonePtr)%ListMultiplier > 1) THEN
				//      CALL ShowSevereError(RoutineName//'Zone Multiplier or Zone List Multipliers cannot be used (i.e., >1)')
				//      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
				//      CALL ShowContinueError('Duplicate the zone or make it larger rather than using multipliers.  Zone Referenced='//  &
				//          TRIM(HydrRadSys(Item)%ZoneName))
				//      ErrorsFound=.TRUE.
			}

			HydrRadSys( Item ).SurfListName = Alphas( 4 );
			SurfListNum = 0;
			if ( NumOfSurfaceLists > 0 ) SurfListNum = FindItemInList( HydrRadSys( Item ).SurfListName, SurfList );
			if ( SurfListNum > 0 ) { // Found a valid surface list
				HydrRadSys( Item ).NumOfSurfaces = SurfList( SurfListNum ).NumOfSurfaces;
				HydrRadSys( Item ).SurfacePtr.allocate( HydrRadSys( Item ).NumOfSurfaces );
				HydrRadSys( Item ).SurfaceName.allocate( HydrRadSys( Item ).NumOfSurfaces );
				HydrRadSys( Item ).SurfaceFlowFrac.allocate( HydrRadSys( Item ).NumOfSurfaces );
				HydrRadSys( Item ).NumCircuits.allocate( HydrRadSys( Item ).NumOfSurfaces );
				for ( SurfNum = 1; SurfNum <= SurfList( SurfListNum ).NumOfSurfaces; ++SurfNum ) {
					HydrRadSys( Item ).SurfacePtr( SurfNum ) = SurfList( SurfListNum ).SurfPtr( SurfNum );
					HydrRadSys( Item ).SurfaceName( SurfNum ) = SurfList( SurfListNum ).SurfName( SurfNum );
					HydrRadSys( Item ).SurfaceFlowFrac( SurfNum ) = SurfList( SurfListNum ).SurfFlowFrac( SurfNum );
					if ( HydrRadSys( Item ).SurfacePtr( SurfNum ) > 0 ) {
						Surface( HydrRadSys( Item ).SurfacePtr( SurfNum ) ).IntConvSurfHasActiveInIt = true;
					}
				}
			} else { // User entered a single surface name rather than a surface list
				HydrRadSys( Item ).NumOfSurfaces = 1;
				HydrRadSys( Item ).SurfacePtr.allocate( HydrRadSys( Item ).NumOfSurfaces );
				HydrRadSys( Item ).SurfaceName.allocate( HydrRadSys( Item ).NumOfSurfaces );
				HydrRadSys( Item ).SurfaceFlowFrac.allocate( HydrRadSys( Item ).NumOfSurfaces );
				HydrRadSys( Item ).NumCircuits.allocate( HydrRadSys( Item ).NumOfSurfaces );
				HydrRadSys( Item ).SurfaceName( 1 ) = HydrRadSys( Item ).SurfListName;
				HydrRadSys( Item ).SurfacePtr( 1 ) = FindItemInList( HydrRadSys( Item ).SurfaceName( 1 ), Surface );
				HydrRadSys( Item ).SurfaceFlowFrac( 1 ) = 1.0;
				HydrRadSys( Item ).NumCircuits( 1 ) = 0.0;
				// Error checking for single surfaces
				if ( HydrRadSys( Item ).SurfacePtr( 1 ) == 0 ) {
					ShowSevereError( RoutineName + "Invalid " + cAlphaFields( 4 ) + " = " + Alphas( 4 ) );
					ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
					ErrorsFound = true;
				} else if ( Surface( HydrRadSys( Item ).SurfacePtr( 1 ) ).PartOfVentSlabOrRadiantSurface ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", Invalid Surface" );
					ShowContinueError( cAlphaFields( 4 ) + "=\"" + Alphas( 4 ) + "\" has been used in another radiant system or ventilated slab." );
					ErrorsFound = true;
				}
				if ( HydrRadSys( Item ).SurfacePtr( 1 ) != 0 ) {
					Surface( HydrRadSys( Item ).SurfacePtr( 1 ) ).IntConvSurfHasActiveInIt = true;
					Surface( HydrRadSys( Item ).SurfacePtr( 1 ) ).PartOfVentSlabOrRadiantSurface = true;
				}
			}

			// Error checking for zones and construction information
			for ( SurfNum = 1; SurfNum <= HydrRadSys( Item ).NumOfSurfaces; ++SurfNum ) {
				if ( HydrRadSys( Item ).SurfacePtr( SurfNum ) == 0 ) continue; // invalid surface -- detected earlier
				if ( Surface( HydrRadSys( Item ).SurfacePtr( SurfNum ) ).Zone != HydrRadSys( Item ).ZonePtr ) {
					ShowSevereError( "Surface referenced in " + CurrentModuleObject + " not in same zone as Radiant System, surface=" + Surface( HydrRadSys( Item ).SurfacePtr( SurfNum ) ).Name );
					ShowContinueError( "Surface in Zone=" + Zone( Surface( HydrRadSys( Item ).SurfacePtr( SurfNum ) ).Zone ).Name + " Hydronic Radiant System in " + cAlphaFields( 3 ) + " = " + Alphas( 3 ) );
					ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
					ErrorsFound = true;
				}
				if ( Surface( HydrRadSys( Item ).SurfacePtr( SurfNum ) ).Construction == 0 ) continue; // Invalid construction -- detected earlier
				if ( ! Construct( Surface( HydrRadSys( Item ).SurfacePtr( SurfNum ) ).Construction ).SourceSinkPresent ) {
					ShowSevereError( "Construction referenced in Hydronic Radiant System Surface does not have a source/sink present" );
					ShowContinueError( "Surface name= " + Surface( HydrRadSys( Item ).SurfacePtr( SurfNum ) ).Name + "  Construction name = " + Construct( Surface( HydrRadSys( Item ).SurfacePtr( SurfNum ) ).Construction ).Name );
					ShowContinueError( "Construction needs to be defined with a \"Construction:InternalSource\" object." );
					ErrorsFound = true;
				}
			}

			HydrRadSys( Item ).TubeDiameter = Numbers( 1 );
			HydrRadSys( Item ).TubeLength = Numbers( 2 );

			// Process the temperature control type
			if ( SameString( Alphas( 5 ), MeanAirTemperature ) ) {
				HydrRadSys( Item ).ControlType = MATControl;
			} else if ( SameString( Alphas( 5 ), MeanRadiantTemperature ) ) {
				HydrRadSys( Item ).ControlType = MRTControl;
			} else if ( SameString( Alphas( 5 ), OperativeTemperature ) ) {
				HydrRadSys( Item ).ControlType = OperativeControl;
			} else if ( SameString( Alphas( 5 ), OutsideAirDryBulbTemperature ) ) {
				HydrRadSys( Item ).ControlType = ODBControl;
			} else if ( SameString( Alphas( 5 ), OutsideAirWetBulbTemperature ) ) {
				HydrRadSys( Item ).ControlType = OWBControl;
			} else {
				ShowWarningError( "Invalid " + cAlphaFields( 5 ) + " =" + Alphas( 5 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
				ShowContinueError( "Control reset to MAT control for this Hydronic Radiant System." );
				HydrRadSys( Item ).ControlType = MATControl;
			}

			// Determine Low Temp Radiant heating design capacity sizing method
			if ( SameString( Alphas( 6 ), "HeatingDesignCapacity" ) ) {
				HydrRadSys( Item ).HeatingCapMethod = HeatingDesignCapacity;
				if ( ! lNumericBlanks( 3 ) ) {
					HydrRadSys( Item ).ScaledHeatingCapacity = Numbers( 3 );
					if (HydrRadSys( Item ).ScaledHeatingCapacity < 0.0 && HydrRadSys( Item ).ScaledHeatingCapacity != AutoSize) {
						ShowSevereError( CurrentModuleObject + " = " + HydrRadSys( Item ).Name );
						ShowContinueError( "Illegal " + cNumericFields( 3 ) + " = " + TrimSigDigits( Numbers( 3 ), 7));
						ErrorsFound = true;
					}
				} else {
					if( ( !lAlphaBlanks( 7 ) ) || ( !lAlphaBlanks( 8 ) ) ) {
						ShowSevereError( CurrentModuleObject + " = " + HydrRadSys( Item ).Name );
						ShowContinueError( "Input for " + cAlphaFields( 6 ) + " = " + Alphas( 6 ) );
						ShowContinueError( "Blank field not allowed for " + cNumericFields( 3 ) );
						ErrorsFound = true;
					}
				}
			} else if ( SameString( Alphas( 6 ), "CapacityPerFloorArea" ) ) {
				HydrRadSys( Item ).HeatingCapMethod = CapacityPerFloorArea;
				if ( ! lNumericBlanks( 4 ) ) {
					HydrRadSys( Item ).ScaledHeatingCapacity = Numbers( 4 );
					if ( HydrRadSys( Item ).ScaledHeatingCapacity <= 0.0 ) {
						ShowSevereError( CurrentModuleObject + " = " + HydrRadSys( Item ).Name );
						ShowContinueError( "Input for " + cAlphaFields( 6 ) + " = " + Alphas( 6 ));
						ShowContinueError( "Illegal " + cNumericFields( 4 ) + " = " + TrimSigDigits( Numbers( 4 ), 7));
						ErrorsFound = true;
					} else if ( HydrRadSys( Item ).ScaledHeatingCapacity == AutoSize) {
						ShowSevereError( CurrentModuleObject + " = " + HydrRadSys( Item ).Name );
						ShowContinueError( "Input for " + cAlphaFields( 6 ) + " = " + Alphas( 6 ) );
						ShowContinueError( "Illegal " + cNumericFields( 4 ) + " = Autosize" );
						ErrorsFound = true;
					}
				} else {
					ShowSevereError( CurrentModuleObject + " = " + HydrRadSys( Item ).Name );
					ShowContinueError( "Input for " + cAlphaFields( 6 ) + " = " + Alphas( 6 ) );
					ShowContinueError( "Blank field not allowed for " + cNumericFields( 4 ) );
					ErrorsFound = true;
				}
			} else if ( SameString( Alphas( 6 ), "FractionOfAutosizedHeatingCapacity" ) ) {
				HydrRadSys( Item ).HeatingCapMethod = FractionOfAutosizedHeatingCapacity;
				if ( ! lNumericBlanks( 5 ) ) {
					HydrRadSys( Item ).ScaledHeatingCapacity = Numbers( 5 );
					if (HydrRadSys( Item ).ScaledHeatingCapacity < 0.0) {
						ShowSevereError( CurrentModuleObject + " = " + HydrRadSys( Item ).Name );
						ShowContinueError( "Illegal " + cNumericFields( 5 ) + " = " + TrimSigDigits( Numbers( 5 ), 7) );
						ErrorsFound = true;
					}
				} else {
					ShowSevereError( CurrentModuleObject + " = " + HydrRadSys( Item ).Name );
					ShowContinueError( "Input for " + cAlphaFields( 6 ) + " = " + Alphas( 6 ) );
					ShowContinueError( "Blank field not allowed for " + cNumericFields( 5 ) );
					ErrorsFound = true;
				}
			} else {
				ShowSevereError( CurrentModuleObject + " = " + HydrRadSys( Item ).Name );
				ShowContinueError( "Illegal " + cAlphaFields( 6 ) + " = " + Alphas( 6 ) );
				ErrorsFound = true;
			}

			// Heating user input data
			HydrRadSys( Item ).WaterVolFlowMaxHeat = Numbers( 6 );

			HydrRadSys( Item ).HotWaterInNode = GetOnlySingleNode( Alphas( 7 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

			HydrRadSys( Item ).HotWaterOutNode = GetOnlySingleNode( Alphas( 8 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			if ( ( ! lAlphaBlanks( 7 ) ) || ( ! lAlphaBlanks( 8 ) ) ) {
				TestCompSet( CurrentModuleObject, Alphas( 1 ), Alphas( 7 ), Alphas( 8 ), "Hot Water Nodes" );
			}

			HydrRadSys( Item ).HotThrottlRange = Numbers( 7 );
			if ( HydrRadSys( Item ).HotThrottlRange < MinThrottlingRange ) {
				ShowWarningError( "ZoneHVAC:LowTemperatureRadiant:VariableFlow: Heating throttling range too small, reset to 0.5" );
				ShowContinueError( "Occurs in Radiant System=" + HydrRadSys( Item ).Name );
				HydrRadSys( Item ).HotThrottlRange = MinThrottlingRange;
			}

			HydrRadSys( Item ).HotSetptSched = Alphas( 9 );
			HydrRadSys( Item ).HotSetptSchedPtr = GetScheduleIndex( Alphas( 9 ) );
			if ( ( HydrRadSys( Item ).HotSetptSchedPtr == 0 ) && ( ! lAlphaBlanks( 9 ) ) ) {
				ShowSevereError( cAlphaFields( 9 ) + " not found: " + Alphas( 9 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
				ErrorsFound = true;
			}

			if ( ( HydrRadSys( Item ).WaterVolFlowMaxHeat == AutoSize ) && ( lAlphaBlanks( 7 ) || lAlphaBlanks( 8 ) || lAlphaBlanks( 9 ) || ( HydrRadSys( Item ).HotWaterInNode <= 0 ) || ( HydrRadSys( Item ).HotWaterOutNode <= 0 ) || ( HydrRadSys( Item ).HotSetptSchedPtr == 0 ) ) ) {
				ShowSevereError( "Hydronic radiant systems may not be autosized without specification of nodes or schedules." );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " (heating input) = " + Alphas( 1 ) );
				ErrorsFound = true;
			}

			// Determine Low Temp Radiant cooling design capacity sizing method
			if ( SameString( Alphas( 10 ), "CoolingDesignCapacity" ) ) {
				HydrRadSys( Item ).CoolingCapMethod = CoolingDesignCapacity;
				if ( ! lNumericBlanks( 8 ) ) {
					HydrRadSys( Item ).ScaledCoolingCapacity = Numbers( 8 );
					if ( HydrRadSys( Item ).ScaledCoolingCapacity < 0.0 && HydrRadSys( Item ).ScaledCoolingCapacity != AutoSize ) {
						ShowSevereError( CurrentModuleObject + " = " + HydrRadSys( Item ).Name );
						ShowContinueError( "Illegal " + cNumericFields( 8 ) + " = " + TrimSigDigits( Numbers( 8 ), 7 ) );
						ErrorsFound = true;
					}
				} else {
					if( ( !lAlphaBlanks( 11 ) ) || ( !lAlphaBlanks( 12 ) ) ) {
						ShowSevereError( CurrentModuleObject + " = " + HydrRadSys( Item ).Name );
						ShowContinueError( "Input for " + cAlphaFields( 10 ) + " = " + Alphas( 10 ) );
						ShowContinueError( "Blank field not allowed for " + cNumericFields( 8 ) );
						ErrorsFound = true;
					}
				}
			} else if ( SameString( Alphas( 10 ), "CapacityPerFloorArea" ) ) {
				HydrRadSys( Item ).CoolingCapMethod = CapacityPerFloorArea;
				if ( ! lNumericBlanks( 9 ) ) {
					HydrRadSys( Item ).ScaledCoolingCapacity = Numbers( 9 );
					if ( HydrRadSys( Item ).CoolingCapMethod <= 0.0) {
						ShowSevereError( CurrentModuleObject + " = " + HydrRadSys( Item ).Name );
						ShowContinueError( "Input for " + cAlphaFields( 10 ) + " = " + Alphas( 10 ) );
						ShowContinueError( "Illegal " + cNumericFields( 9 ) + " = " + TrimSigDigits( Numbers( 9 ), 7 ) );
						ErrorsFound = true;
					} else if ( HydrRadSys( Item ).ScaledCoolingCapacity == AutoSize ) {
						ShowSevereError( CurrentModuleObject + " = " + HydrRadSys( Item ).Name );
						ShowContinueError( "Input for " + cAlphaFields( 10 ) + " = " + Alphas( 10 ) );
						ShowContinueError( "Illegal " + cNumericFields( 9 ) + " = Autosize" );
						ErrorsFound = true;
					}
				} else {
					ShowSevereError( CurrentModuleObject + " = " + HydrRadSys(Item).Name );
					ShowContinueError( "Input for " + cAlphaFields( 10 ) + " = " + Alphas( 10 ) );
					ShowContinueError( "Blank field not allowed for " + cNumericFields( 9 ) );
					ErrorsFound = true;
				}
			} else if (SameString( Alphas( 10 ), "FractionOfAutosizedCoolingCapacity" ) ) {
				HydrRadSys( Item ).CoolingCapMethod = FractionOfAutosizedCoolingCapacity;
				if ( ! lNumericBlanks( 10 ) ) {
					HydrRadSys( Item ).ScaledCoolingCapacity = Numbers( 10 );
					if (HydrRadSys( Item ).ScaledCoolingCapacity < 0.0 ) {
						ShowSevereError( CurrentModuleObject + " = " + HydrRadSys( Item ).Name );
						ShowContinueError( "Illegal " + cNumericFields( 10 ) + " = " + TrimSigDigits( Numbers( 10 ), 7 ) );
						ErrorsFound = true;
					}
				} else {
					ShowSevereError( CurrentModuleObject + " = " + HydrRadSys( Item ).Name );
					ShowContinueError( "Input for " + cAlphaFields( 10 ) + " = " + Alphas( 10 ) );
					ShowContinueError( "Blank field not allowed for " + cNumericFields( 10 ) );
					ErrorsFound = true;
				}
			} else {
				ShowSevereError( CurrentModuleObject + " = " + HydrRadSys( Item ).Name );
				ShowContinueError( "Illegal " + cAlphaFields( 10 ) + " = " + Alphas( 10 ) );
				ErrorsFound = true;
			}

			// Cooling user input data
			HydrRadSys( Item ).WaterVolFlowMaxCool = Numbers( 11 );

			HydrRadSys( Item ).ColdWaterInNode = GetOnlySingleNode( Alphas( 11 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Water, NodeConnectionType_Inlet, 2, ObjectIsNotParent );

			HydrRadSys( Item ).ColdWaterOutNode = GetOnlySingleNode( Alphas( 12 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Water, NodeConnectionType_Outlet, 2, ObjectIsNotParent );

			if ( ( ! lAlphaBlanks( 11 ) ) || ( ! lAlphaBlanks( 12 ) ) ) {
				TestCompSet( CurrentModuleObject, Alphas( 1 ), Alphas( 11 ), Alphas( 12 ), "Chilled Water Nodes" );
			}

			HydrRadSys( Item ).ColdThrottlRange = Numbers( 12 );
			if ( HydrRadSys( Item ).ColdThrottlRange < MinThrottlingRange ) {
				ShowWarningError( "ZoneHVAC:LowTemperatureRadiant:VariableFlow: Cooling throttling range too small, reset to 0.5" );
				ShowContinueError( "Occurs in Radiant System=" + HydrRadSys( Item ).Name );
				HydrRadSys( Item ).ColdThrottlRange = MinThrottlingRange;
			}

			HydrRadSys( Item ).ColdSetptSched = Alphas( 13 );
			HydrRadSys( Item ).ColdSetptSchedPtr = GetScheduleIndex( Alphas( 13 ) );
			if ( ( HydrRadSys( Item ).ColdSetptSchedPtr == 0 ) && ( ! lAlphaBlanks( 13 ) ) ) {
				ShowSevereError( cAlphaFields( 13 ) + " not found: " + Alphas( 13 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
				ErrorsFound = true;
			}

			if ( SameString( Alphas( 14 ), Off ) ) {
				HydrRadSys( Item ).CondCtrlType = CondCtrlNone;
			} else if ( SameString( Alphas( 14 ), SimpleOff ) ) {
				HydrRadSys( Item ).CondCtrlType = CondCtrlSimpleOff;
			} else if ( SameString( Alphas( 14 ), VariableOff ) ) {
				HydrRadSys( Item ).CondCtrlType = CondCtrlVariedOff;
			} else {
				HydrRadSys( Item ).CondCtrlType = CondCtrlSimpleOff;
			}

			HydrRadSys( Item ).CondDewPtDeltaT = Numbers( 13 );

			if ( SameString( Alphas( 15 ), OnePerSurf ) ) {
				HydrRadSys( Item ).NumCircCalcMethod = OneCircuit;
			} else if ( SameString( Alphas( 15 ), CalcFromLength ) ) {
				HydrRadSys( Item ).NumCircCalcMethod = CalculateFromLength;
			} else {
				HydrRadSys( Item ).NumCircCalcMethod = OneCircuit;
			}

			HydrRadSys( Item ).CircLength = Numbers( 14 );

			if ( ( HydrRadSys( Item ).WaterVolFlowMaxCool == AutoSize ) && ( lAlphaBlanks( 11 ) || lAlphaBlanks( 12 ) || lAlphaBlanks( 13 ) || ( HydrRadSys( Item ).ColdWaterInNode <= 0 ) || ( HydrRadSys( Item ).ColdWaterOutNode <= 0 ) || ( HydrRadSys( Item ).ColdSetptSchedPtr == 0 ) ) ) {
				ShowSevereError( "Hydronic radiant systems may not be autosized without specification of nodes or schedules" );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " (cooling input) =" + Alphas( 1 ) );
				ErrorsFound = true;
			}

		}

		// Obtain all of the user data related to constant flow (hydronic) low temperature radiant systems...

		CurrentModuleObject = "ZoneHVAC:LowTemperatureRadiant:ConstantFlow";

		for ( Item = 1; Item <= NumOfCFloLowTempRadSys; ++Item ) {

			GetObjectItem( CurrentModuleObject, Item, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), RadSysTypes, BaseNum, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			++BaseNum;
			RadSysTypes( BaseNum ).Name = Alphas( 1 );
			RadSysTypes( BaseNum ).SystemType = ConstantFlowSystem;
			// General user input data
			CFloRadSys( Item ).Name = Alphas( 1 );

			CFloRadSys( Item ).SchedName = Alphas( 2 );
			if ( lAlphaBlanks( 2 ) ) {
				CFloRadSys( Item ).SchedPtr = ScheduleAlwaysOn;
			} else {
				CFloRadSys( Item ).SchedPtr = GetScheduleIndex( Alphas( 2 ) );
				if ( CFloRadSys( Item ).SchedPtr == 0 ) {
					ShowSevereError( cAlphaFields( 2 ) + " not found for " + Alphas( 1 ) );
					ShowContinueError( "Missing " + cAlphaFields( 2 ) + " is " + Alphas( 2 ) );
					ErrorsFound = true;
				}
			}

			CFloRadSys( Item ).ZoneName = Alphas( 3 );
			CFloRadSys( Item ).ZonePtr = FindItemInList( Alphas( 3 ), Zone );
			if ( CFloRadSys( Item ).ZonePtr == 0 ) {
				ShowSevereError( RoutineName + "Invalid " + cAlphaFields( 3 ) + " = " + Alphas( 3 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
				ErrorsFound = true;
				//    ELSEIF (Zone(CFloRadSys(Item)%ZonePtr)%Multiplier > 1 .or. Zone(CFloRadSys(Item)%ZonePtr)%ListMultiplier > 1) THEN
				//      CALL ShowSevereError(RoutineName//'Zone Multiplier or Zone List Multipliers cannot be used (i.e., >1)')
				//      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
				//      CALL ShowContinueError('Duplicate the zone or make it larger rather than using multipliers.  Zone Referenced='//  &
				//          TRIM(CFloRadSys(Item)%ZoneName))
				//      ErrorsFound=.TRUE.
			}

			CFloRadSys( Item ).SurfListName = Alphas( 4 );
			SurfListNum = 0;
			if ( NumOfSurfaceLists > 0 ) SurfListNum = FindItemInList( CFloRadSys( Item ).SurfListName, SurfList );
			if ( SurfListNum > 0 ) { // Found a valid surface list
				CFloRadSys( Item ).NumOfSurfaces = SurfList( SurfListNum ).NumOfSurfaces;
				CFloRadSys( Item ).SurfacePtr.allocate( CFloRadSys( Item ).NumOfSurfaces );
				CFloRadSys( Item ).SurfaceName.allocate( CFloRadSys( Item ).NumOfSurfaces );
				CFloRadSys( Item ).SurfaceFlowFrac.allocate( CFloRadSys( Item ).NumOfSurfaces );
				CFloRadSys( Item ).NumCircuits.allocate( CFloRadSys( Item ).NumOfSurfaces );
				MaxCloNumOfSurfaces = max( MaxCloNumOfSurfaces, CFloRadSys( Item ).NumOfSurfaces );
				for ( SurfNum = 1; SurfNum <= SurfList( SurfListNum ).NumOfSurfaces; ++SurfNum ) {
					CFloRadSys( Item ).SurfacePtr( SurfNum ) = SurfList( SurfListNum ).SurfPtr( SurfNum );
					CFloRadSys( Item ).SurfaceName( SurfNum ) = SurfList( SurfListNum ).SurfName( SurfNum );
					CFloRadSys( Item ).SurfaceFlowFrac( SurfNum ) = SurfList( SurfListNum ).SurfFlowFrac( SurfNum );
					CFloRadSys( Item ).NumCircuits( SurfNum ) = 0.0;
					if ( CFloRadSys( Item ).SurfacePtr( SurfNum ) != 0 ) {
						Surface( CFloRadSys( Item ).SurfacePtr( SurfNum ) ).IntConvSurfHasActiveInIt = true;
					}
				}
			} else { // User entered a single surface name rather than a surface list
				CFloRadSys( Item ).NumOfSurfaces = 1;
				CFloRadSys( Item ).SurfacePtr.allocate( CFloRadSys( Item ).NumOfSurfaces );
				CFloRadSys( Item ).SurfaceName.allocate( CFloRadSys( Item ).NumOfSurfaces );
				CFloRadSys( Item ).SurfaceFlowFrac.allocate( CFloRadSys( Item ).NumOfSurfaces );
				CFloRadSys( Item ).NumCircuits.allocate( CFloRadSys( Item ).NumOfSurfaces );
				MaxCloNumOfSurfaces = max( MaxCloNumOfSurfaces, CFloRadSys( Item ).NumOfSurfaces );
				CFloRadSys( Item ).SurfaceName( 1 ) = CFloRadSys( Item ).SurfListName;
				CFloRadSys( Item ).SurfacePtr( 1 ) = FindItemInList( CFloRadSys( Item ).SurfaceName( 1 ), Surface );
				CFloRadSys( Item ).SurfaceFlowFrac( 1 ) = 1.0;
				CFloRadSys( Item ).NumCircuits( 1 ) = 0.0;
				// Error checking for single surfaces
				if ( CFloRadSys( Item ).SurfacePtr( 1 ) == 0 ) {
					ShowSevereError( RoutineName + "Invalid " + cAlphaFields( 4 ) + " = " + Alphas( 4 ) );
					ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
					ErrorsFound = true;
				} else if ( Surface( CFloRadSys( Item ).SurfacePtr( 1 ) ).PartOfVentSlabOrRadiantSurface ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", Invalid Surface" );
					ShowContinueError( cAlphaFields( 4 ) + "=\"" + Alphas( 4 ) + "\" has been used in another radiant system or ventilated slab." );
					ErrorsFound = true;
				}
				if ( CFloRadSys( Item ).SurfacePtr( 1 ) != 0 ) {
					Surface( CFloRadSys( Item ).SurfacePtr( 1 ) ).IntConvSurfHasActiveInIt = true;
					Surface( CFloRadSys( Item ).SurfacePtr( 1 ) ).PartOfVentSlabOrRadiantSurface = true;
				}
			}

			// Error checking for zones and construction information
			for ( SurfNum = 1; SurfNum <= CFloRadSys( Item ).NumOfSurfaces; ++SurfNum ) {
				if ( CFloRadSys( Item ).SurfacePtr( SurfNum ) == 0 ) continue; // invalid surface -- detected earlier
				if ( Surface( CFloRadSys( Item ).SurfacePtr( SurfNum ) ).Zone != CFloRadSys( Item ).ZonePtr ) {
					ShowSevereError( "Surface referenced in " + CurrentModuleObject + " not in same zone as Radiant System, surface=" + Surface( CFloRadSys( Item ).SurfacePtr( SurfNum ) ).Name );
					ShowContinueError( "Surface in Zone=" + Zone( Surface( CFloRadSys( Item ).SurfacePtr( SurfNum ) ).Zone ).Name + " Constant Flow Radiant System in " + cAlphaFields( 3 ) + " = " + Alphas( 3 ) );
					ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
					ErrorsFound = true;
				}
				if ( Surface( CFloRadSys( Item ).SurfacePtr( SurfNum ) ).Construction == 0 ) continue; // invalid construction, detected earlier
				if ( ! Construct( Surface( CFloRadSys( Item ).SurfacePtr( SurfNum ) ).Construction ).SourceSinkPresent ) {
					ShowSevereError( "Construction referenced in Constant Flow Radiant System Surface does not have a source/sink" );
					ShowContinueError( "Surface name= " + Surface( CFloRadSys( Item ).SurfacePtr( SurfNum ) ).Name + "  Construction name = " + Construct( Surface( CFloRadSys( Item ).SurfacePtr( SurfNum ) ).Construction ).Name );
					ShowContinueError( "Construction needs to be defined with a \"Construction:InternalSource\" object." );
					ErrorsFound = true;
				}
			}

			CFloRadSys( Item ).TubeDiameter = Numbers( 1 );
			CFloRadSys( Item ).TubeLength = Numbers( 2 );

			// Process the temperature control type
			if ( SameString( Alphas( 5 ), MeanAirTemperature ) ) {
				CFloRadSys( Item ).ControlType = MATControl;
			} else if ( SameString( Alphas( 5 ), MeanRadiantTemperature ) ) {
				CFloRadSys( Item ).ControlType = MRTControl;
			} else if ( SameString( Alphas( 5 ), OperativeTemperature ) ) {
				CFloRadSys( Item ).ControlType = OperativeControl;
			} else if ( SameString( Alphas( 5 ), OutsideAirDryBulbTemperature ) ) {
				CFloRadSys( Item ).ControlType = ODBControl;
			} else if ( SameString( Alphas( 5 ), OutsideAirWetBulbTemperature ) ) {
				CFloRadSys( Item ).ControlType = OWBControl;
			} else {
				ShowWarningError( "Invalid " + cAlphaFields( 5 ) + " =" + Alphas( 5 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
				ShowContinueError( "Control reset to MAT control for this Constant Flow Radiant System." );
				CFloRadSys( Item ).ControlType = MATControl;
			}

			// Process pump input for constant flow (hydronic) radiant system
			CFloRadSys( Item ).WaterVolFlowMax = Numbers( 3 );
			CFloRadSys( Item ).VolFlowSched = Alphas( 6 );
			CFloRadSys( Item ).VolFlowSchedPtr = GetScheduleIndex( Alphas( 6 ) );
			if ( ( CFloRadSys( Item ).VolFlowSchedPtr == 0 ) && ( ! lAlphaBlanks( 6 ) ) ) {
				ShowSevereError( cAlphaFields( 6 ) + " not found: " + Alphas( 6 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
				ErrorsFound = true;
			}
			CFloRadSys( Item ).NomPumpHead = Numbers( 4 );
			CFloRadSys( Item ).NomPowerUse = Numbers( 5 );
			CFloRadSys( Item ).MotorEffic = Numbers( 6 );
			CFloRadSys( Item ).FracMotorLossToFluid = Numbers( 7 );

			// Heating user input data
			CFloRadSys( Item ).HotWaterInNode = GetOnlySingleNode( Alphas( 7 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

			CFloRadSys( Item ).HotWaterOutNode = GetOnlySingleNode( Alphas( 8 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			if ( ( ! lAlphaBlanks( 7 ) ) || ( ! lAlphaBlanks( 8 ) ) ) {
				TestCompSet( CurrentModuleObject, Alphas( 1 ), Alphas( 7 ), Alphas( 8 ), "Hot Water Nodes" );
			}

			CFloRadSys( Item ).HotWaterHiTempSched = Alphas( 9 );
			CFloRadSys( Item ).HotWaterHiTempSchedPtr = GetScheduleIndex( Alphas( 9 ) );
			if ( ( CFloRadSys( Item ).HotWaterHiTempSchedPtr == 0 ) && ( ! lAlphaBlanks( 9 ) ) ) {
				ShowSevereError( cAlphaFields( 9 ) + " not found: " + Alphas( 9 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
				ErrorsFound = true;
			}

			CFloRadSys( Item ).HotWaterLoTempSched = Alphas( 10 );
			CFloRadSys( Item ).HotWaterLoTempSchedPtr = GetScheduleIndex( Alphas( 10 ) );
			if ( ( CFloRadSys( Item ).HotWaterLoTempSchedPtr == 0 ) && ( ! lAlphaBlanks( 10 ) ) ) {
				ShowSevereError( cAlphaFields( 10 ) + " not found: " + Alphas( 10 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
				ErrorsFound = true;
			}

			CFloRadSys( Item ).HotCtrlHiTempSched = Alphas( 11 );
			CFloRadSys( Item ).HotCtrlHiTempSchedPtr = GetScheduleIndex( Alphas( 11 ) );
			if ( ( CFloRadSys( Item ).HotCtrlHiTempSchedPtr == 0 ) && ( ! lAlphaBlanks( 11 ) ) ) {
				ShowSevereError( cAlphaFields( 11 ) + " not found: " + Alphas( 11 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
				ErrorsFound = true;
			}

			CFloRadSys( Item ).HotCtrlLoTempSched = Alphas( 12 );
			CFloRadSys( Item ).HotCtrlLoTempSchedPtr = GetScheduleIndex( Alphas( 12 ) );
			if ( ( CFloRadSys( Item ).HotCtrlLoTempSchedPtr == 0 ) && ( ! lAlphaBlanks( 12 ) ) ) {
				ShowSevereError( cAlphaFields( 12 ) + " not found: " + Alphas( 12 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
				ErrorsFound = true;
			}

			// Cooling user input data
			CFloRadSys( Item ).ColdWaterInNode = GetOnlySingleNode( Alphas( 13 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Water, NodeConnectionType_Inlet, 2, ObjectIsNotParent );

			CFloRadSys( Item ).ColdWaterOutNode = GetOnlySingleNode( Alphas( 14 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Water, NodeConnectionType_Outlet, 2, ObjectIsNotParent );

			if ( ( ! lAlphaBlanks( 13 ) ) || ( ! lAlphaBlanks( 14 ) ) ) {
				TestCompSet( CurrentModuleObject, Alphas( 1 ), Alphas( 13 ), Alphas( 14 ), "Chilled Water Nodes" );
			}

			CFloRadSys( Item ).ColdWaterHiTempSched = Alphas( 15 );
			CFloRadSys( Item ).ColdWaterHiTempSchedPtr = GetScheduleIndex( Alphas( 15 ) );
			if ( ( CFloRadSys( Item ).ColdWaterHiTempSchedPtr == 0 ) && ( ! lAlphaBlanks( 15 ) ) ) {
				ShowSevereError( cAlphaFields( 15 ) + " not found: " + Alphas( 15 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
				ErrorsFound = true;
			}

			CFloRadSys( Item ).ColdWaterLoTempSched = Alphas( 16 );
			CFloRadSys( Item ).ColdWaterLoTempSchedPtr = GetScheduleIndex( Alphas( 16 ) );
			if ( ( CFloRadSys( Item ).ColdWaterLoTempSchedPtr == 0 ) && ( ! lAlphaBlanks( 16 ) ) ) {
				ShowSevereError( cAlphaFields( 16 ) + " not found: " + Alphas( 16 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
				ErrorsFound = true;
			}

			CFloRadSys( Item ).ColdCtrlHiTempSched = Alphas( 17 );
			CFloRadSys( Item ).ColdCtrlHiTempSchedPtr = GetScheduleIndex( Alphas( 17 ) );
			if ( ( CFloRadSys( Item ).ColdCtrlHiTempSchedPtr == 0 ) && ( ! lAlphaBlanks( 17 ) ) ) {
				ShowSevereError( cAlphaFields( 17 ) + " not found: " + Alphas( 17 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
				ErrorsFound = true;
			}

			CFloRadSys( Item ).ColdCtrlLoTempSched = Alphas( 18 );
			CFloRadSys( Item ).ColdCtrlLoTempSchedPtr = GetScheduleIndex( Alphas( 18 ) );
			if ( ( CFloRadSys( Item ).ColdCtrlLoTempSchedPtr == 0 ) && ( ! lAlphaBlanks( 18 ) ) ) {
				ShowSevereError( cAlphaFields( 18 ) + " not found: " + Alphas( 18 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
				ErrorsFound = true;
			}

			if ( SameString( Alphas( 19 ), Off ) ) {
				CFloRadSys( Item ).CondCtrlType = CondCtrlNone;
			} else if ( SameString( Alphas( 19 ), SimpleOff ) ) {
				CFloRadSys( Item ).CondCtrlType = CondCtrlSimpleOff;
			} else if ( SameString( Alphas( 19 ), VariableOff ) ) {
				CFloRadSys( Item ).CondCtrlType = CondCtrlVariedOff;
			} else {
				CFloRadSys( Item ).CondCtrlType = CondCtrlSimpleOff;
			}

			CFloRadSys( Item ).CondDewPtDeltaT = Numbers( 8 );

			if ( SameString( Alphas( 20 ), OnePerSurf ) ) {
				CFloRadSys( Item ).NumCircCalcMethod = OneCircuit;
			} else if ( SameString( Alphas( 20 ), CalcFromLength ) ) {
				CFloRadSys( Item ).NumCircCalcMethod = CalculateFromLength;
			} else {
				CFloRadSys( Item ).NumCircCalcMethod = OneCircuit;
			}

			CFloRadSys( Item ).CircLength = Numbers( 9 );

		}

		// Obtain all of the user data related to electric low temperature radiant systems...
		CurrentModuleObject = "ZoneHVAC:LowTemperatureRadiant:Electric";

		for ( Item = 1; Item <= NumOfElecLowTempRadSys; ++Item ) {

			GetObjectItem( CurrentModuleObject, Item, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );


			ElecRadSysNumericFields( Item ).FieldNames.allocate( NumNumbers );
			ElecRadSysNumericFields( Item ).FieldNames = "";
			ElecRadSysNumericFields( Item ).FieldNames = cNumericFields;

			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), RadSysTypes, BaseNum, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			++BaseNum;
			RadSysTypes( BaseNum ).Name = Alphas( 1 );
			RadSysTypes( BaseNum ).SystemType = ElectricSystem;
			// General user input data
			ElecRadSys( Item ).Name = Alphas( 1 );

			ElecRadSys( Item ).SchedName = Alphas( 2 );
			if ( lAlphaBlanks( 2 ) ) {
				ElecRadSys( Item ).SchedPtr = ScheduleAlwaysOn;
			} else {
				ElecRadSys( Item ).SchedPtr = GetScheduleIndex( Alphas( 2 ) );
				if ( ElecRadSys( Item ).SchedPtr == 0 ) {
					ShowSevereError( cAlphaFields( 2 ) + " not found for" + Alphas( 1 ) );
					ShowContinueError( "Incorrect " + cAlphaFields( 2 ) + " = " + Alphas( 2 ) );
					ErrorsFound = true;
				}
			}

			ElecRadSys( Item ).ZoneName = Alphas( 3 );
			ElecRadSys( Item ).ZonePtr = FindItemInList( Alphas( 3 ), Zone );
			if ( ElecRadSys( Item ).ZonePtr == 0 ) {
				ShowSevereError( RoutineName + "Invalid " + cAlphaFields( 3 ) + " = " + Alphas( 3 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
				ErrorsFound = true;
				//    ELSEIF (Zone(ElecRadSys(Item)%ZonePtr)%Multiplier > 1 .or. Zone(ElecRadSys(Item)%ZonePtr)%ListMultiplier > 1) THEN
				//      CALL ShowSevereError(RoutineName//'Zone Multiplier or Zone List Multipliers cannot be used (i.e., >1)')
				//      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
				//      CALL ShowContinueError('Duplicate the zone or make it larger rather than using multipliers.  Zone Referenced='//  &
				//          TRIM(ElecRadSys(Item)%ZoneName))
				//      ErrorsFound=.TRUE.
			}

			ElecRadSys( Item ).SurfListName = Alphas( 4 );
			SurfListNum = 0;
			if ( NumOfSurfaceLists > 0 ) SurfListNum = FindItemInList( ElecRadSys( Item ).SurfListName, SurfList );
			if ( SurfListNum > 0 ) { // Found a valid surface list
				ElecRadSys( Item ).NumOfSurfaces = SurfList( SurfListNum ).NumOfSurfaces;
				ElecRadSys( Item ).SurfacePtr.allocate( ElecRadSys( Item ).NumOfSurfaces );
				ElecRadSys( Item ).SurfaceName.allocate( ElecRadSys( Item ).NumOfSurfaces );
				ElecRadSys( Item ).SurfacePowerFrac.allocate( ElecRadSys( Item ).NumOfSurfaces );
				for ( SurfNum = 1; SurfNum <= ElecRadSys( SurfListNum ).NumOfSurfaces; ++SurfNum ) {
					ElecRadSys( Item ).SurfacePtr( SurfNum ) = SurfList( SurfListNum ).SurfPtr( SurfNum );
					ElecRadSys( Item ).SurfaceName( SurfNum ) = SurfList( SurfListNum ).SurfName( SurfNum );
					ElecRadSys( Item ).SurfacePowerFrac( SurfNum ) = SurfList( SurfListNum ).SurfFlowFrac( SurfNum );
				}
			} else { // User entered a single surface name rather than a surface list
				ElecRadSys( Item ).NumOfSurfaces = 1;
				ElecRadSys( Item ).SurfacePtr.allocate( ElecRadSys( Item ).NumOfSurfaces );
				ElecRadSys( Item ).SurfaceName.allocate( ElecRadSys( Item ).NumOfSurfaces );
				ElecRadSys( Item ).SurfacePowerFrac.allocate( ElecRadSys( Item ).NumOfSurfaces );
				ElecRadSys( Item ).SurfaceName( 1 ) = ElecRadSys( Item ).SurfListName;
				ElecRadSys( Item ).SurfacePtr( 1 ) = FindItemInList( ElecRadSys( Item ).SurfaceName( 1 ), Surface );
				ElecRadSys( Item ).SurfacePowerFrac( 1 ) = 1.0;
				// Error checking for single surfaces
				if ( ElecRadSys( Item ).SurfacePtr( 1 ) == 0 ) {
					ShowSevereError( RoutineName + "Invalid " + cAlphaFields( 4 ) + " = " + Alphas( 4 ) );
					ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
					ErrorsFound = true;
				} else if ( Surface( ElecRadSys( Item ).SurfacePtr( 1 ) ).PartOfVentSlabOrRadiantSurface ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", Invalid Surface" );
					ShowContinueError( cAlphaFields( 4 ) + "=\"" + Alphas( 4 ) + "\" has been used in another radiant system or ventilated slab." );
					ErrorsFound = true;
				}
				if ( ElecRadSys( Item ).SurfacePtr( 1 ) != 0 ) {
					Surface( ElecRadSys( Item ).SurfacePtr( 1 ) ).PartOfVentSlabOrRadiantSurface = true;
				}
			}

			// Error checking for zones and construction information
			for ( SurfNum = 1; SurfNum <= ElecRadSys( Item ).NumOfSurfaces; ++SurfNum ) {
				if ( ElecRadSys( Item ).SurfacePtr( SurfNum ) == 0 ) continue; // Invalid surface -- detected earlier
				if ( Surface( ElecRadSys( Item ).SurfacePtr( SurfNum ) ).Zone != ElecRadSys( Item ).ZonePtr ) {
					ShowSevereError( "Surface referenced in " + CurrentModuleObject + " not in same zone as Radiant System, surface=" + Surface( ElecRadSys( Item ).SurfacePtr( SurfNum ) ).Name );
					ShowContinueError( "Surface in Zone=" + Zone( Surface( ElecRadSys( Item ).SurfacePtr( SurfNum ) ).Zone ).Name + " Electric Radiant System in " + cAlphaFields( 3 ) + " = " + Alphas( 3 ) );
					ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
					ErrorsFound = true;
				}
				if ( Surface( ElecRadSys( Item ).SurfacePtr( SurfNum ) ).Construction == 0 ) continue; // invalid construction -- detected earlier
				if ( ! Construct( Surface( ElecRadSys( Item ).SurfacePtr( SurfNum ) ).Construction ).SourceSinkPresent ) {
					ShowSevereError( "Construction referenced in Electric Radiant System Surface does not have a source/sink present" );
					ShowContinueError( "Surface name= " + Surface( ElecRadSys( Item ).SurfacePtr( SurfNum ) ).Name + "  Construction name = " + Construct( Surface( ElecRadSys( Item ).SurfacePtr( SurfNum ) ).Construction ).Name );
					ShowContinueError( "Construction needs to be defined with a \"Construction:InternalSource\" object." );
					ErrorsFound = true;
				}
			}

			// Heating user input data
			// Determine Low Temp Radiant heating design capacity sizing method
			if ( SameString( Alphas( iHeatCAPMAlphaNum ), "HeatingDesignCapacity" ) ) {
				ElecRadSys( Item ).HeatingCapMethod = HeatingDesignCapacity;
				if ( !lNumericBlanks( iHeatDesignCapacityNumericNum ) ) {
					ElecRadSys( Item ).ScaledHeatingCapacity = Numbers( iHeatDesignCapacityNumericNum );
					ElecRadSys( Item ).MaxElecPower = ElecRadSys( Item ).ScaledHeatingCapacity;
					if (ElecRadSys( Item ).ScaledHeatingCapacity < 0.0 && ElecRadSys( Item ).ScaledHeatingCapacity != AutoSize) {
						ShowSevereError( CurrentModuleObject + " = " + ElecRadSys( Item ).Name );
						ShowContinueError( "Illegal " + cNumericFields( iHeatDesignCapacityNumericNum ) + " = " + TrimSigDigits( Numbers( iHeatDesignCapacityNumericNum ), 7 ) );
						ErrorsFound = true;
					}
				} else {
					ShowSevereError( CurrentModuleObject + " = " + ElecRadSys( Item ).Name );
					ShowContinueError( "Input for " + cAlphaFields(iHeatCAPMAlphaNum) + " = " + Alphas( iHeatCAPMAlphaNum ) );
					ShowContinueError( "Blank field not allowed for " + cNumericFields( iHeatDesignCapacityNumericNum ) );
					ErrorsFound = true;
				}
			} else if ( SameString( Alphas( iHeatCAPMAlphaNum ), "CapacityPerFloorArea" ) ) {
				ElecRadSys( Item ).HeatingCapMethod = CapacityPerFloorArea;
				if ( !lNumericBlanks( iHeatCapacityPerFloorAreaNumericNum ) ) {
					ElecRadSys( Item ).ScaledHeatingCapacity = Numbers( iHeatCapacityPerFloorAreaNumericNum );
					ElecRadSys( Item ).MaxElecPower = ElecRadSys( Item ).ScaledHeatingCapacity;
					if ( ElecRadSys( Item ).ScaledHeatingCapacity <= 0.0 ) {
						ShowSevereError( CurrentModuleObject + " = " + ElecRadSys( Item ).Name );
						ShowContinueError( "Input for " + cAlphaFields( iHeatCAPMAlphaNum ) + " = " + Alphas( iHeatCAPMAlphaNum ) );
						ShowContinueError( "Illegal " + cNumericFields( iHeatCapacityPerFloorAreaNumericNum ) + " = " + TrimSigDigits( Numbers( iHeatCapacityPerFloorAreaNumericNum ), 7 ) );
						ErrorsFound = true;
					} else if ( ElecRadSys( Item ).ScaledHeatingCapacity == AutoSize ) {
						ShowSevereError( CurrentModuleObject + " = " + ElecRadSys( Item ).Name );
						ShowContinueError( "Input for " + cAlphaFields( iHeatCAPMAlphaNum ) + " = " + Alphas( iHeatCAPMAlphaNum ) );
						ShowContinueError( "Illegal " + cNumericFields( iHeatCapacityPerFloorAreaNumericNum ) + " = Autosize" );
						ErrorsFound = true;
					}
				} else {
					ShowSevereError( CurrentModuleObject + " = " + ElecRadSys( Item ).Name );
					ShowContinueError( "Input for " + cAlphaFields( iHeatCAPMAlphaNum ) + " = " + Alphas( iHeatCAPMAlphaNum ) );
					ShowContinueError( "Blank field not allowed for " + cNumericFields( iHeatCapacityPerFloorAreaNumericNum ) );
					ErrorsFound = true;
				}
			} else if ( SameString( Alphas( iHeatCAPMAlphaNum ), "FractionOfAutosizedHeatingCapacity" ) ) {
				ElecRadSys( Item ).HeatingCapMethod = FractionOfAutosizedHeatingCapacity;
				if ( !lNumericBlanks(iHeatFracOfAutosizedCapacityNumericNum ) ) {
					ElecRadSys( Item ).ScaledHeatingCapacity = Numbers( iHeatFracOfAutosizedCapacityNumericNum );
					ElecRadSys( Item ).MaxElecPower = ElecRadSys( Item ).ScaledHeatingCapacity;
					if ( ElecRadSys( Item ).ScaledHeatingCapacity < 0.0 ) {
						ShowSevereError( CurrentModuleObject + " = " + ElecRadSys( Item ).Name );
						ShowContinueError( "Illegal " + cNumericFields( iHeatFracOfAutosizedCapacityNumericNum ) + " = " + TrimSigDigits( Numbers( iHeatFracOfAutosizedCapacityNumericNum ), 7 ) );
						ErrorsFound = true;
					}
				} else {
					ShowSevereError( CurrentModuleObject + " = " + ElecRadSys( Item ).Name );
					ShowContinueError( "Input for " + cAlphaFields( iHeatCAPMAlphaNum ) + " = " + Alphas( iHeatCAPMAlphaNum ) );
					ShowContinueError( "Blank field not allowed for " + cNumericFields( iHeatFracOfAutosizedCapacityNumericNum ) );
					ErrorsFound = true;
				}
			} else {
				ShowSevereError( CurrentModuleObject + " = " + ElecRadSys( Item ).Name );
				ShowContinueError( "Illegal " + cAlphaFields( iHeatCAPMAlphaNum ) + " = " + Alphas( iHeatCAPMAlphaNum ) );
				ErrorsFound = true;
			}

			// Process the temperature control type
			if ( SameString( Alphas( 6 ), MeanAirTemperature ) ) {
				ElecRadSys( Item ).ControlType = MATControl;
			} else if ( SameString( Alphas( 6 ), MeanRadiantTemperature ) ) {
				ElecRadSys( Item ).ControlType = MRTControl;
			} else if ( SameString( Alphas( 6 ), OperativeTemperature ) ) {
				ElecRadSys( Item ).ControlType = OperativeControl;
			} else if ( SameString( Alphas( 6 ), OutsideAirDryBulbTemperature ) ) {
				ElecRadSys( Item ).ControlType = ODBControl;
			} else if ( SameString( Alphas( 6 ), OutsideAirWetBulbTemperature ) ) {
				ElecRadSys( Item ).ControlType = OWBControl;
			} else {
				ShowWarningError( "Invalid " + cAlphaFields( 6 ) + " = " + Alphas( 6 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
				ShowContinueError( "Control reset to MAT control for this Electric Radiant System." );
				ElecRadSys( Item ).ControlType = MATControl;
			}

			ElecRadSys( Item ).ThrottlRange = Numbers( 4 );
			if ( ElecRadSys( Item ).ThrottlRange < MinThrottlingRange ) {
				ShowWarningError( cNumericFields( 4 ) + " out of range, reset to 0.5" );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
				ElecRadSys( Item ).ThrottlRange = MinThrottlingRange;
			}

			ElecRadSys( Item ).SetptSched = Alphas( 7 );
			ElecRadSys( Item ).SetptSchedPtr = GetScheduleIndex( Alphas( 7 ) );
			if ( ElecRadSys( Item ).SetptSchedPtr == 0 ) {
				if ( lAlphaBlanks( 7 ) ) {
					ShowSevereError( cAlphaFields( 7 ) + " must be input, missing for " + Alphas( 1 ) );
				} else {
					ShowSevereError( cAlphaFields( 7 ) + " not found for " + Alphas( 1 ) );
					ShowContinueError( "Incorrect " + cAlphaFields( 7 ) + " = " + Alphas( 7 ) );
				}
				ErrorsFound = true;
			}

		}

		// Check to see if any surface is included in more than one radiant system.  This is not allowed
		// and thus indicative that there is an error in the input file.  This is to make sure that two
		// different radiant systems are competing for the same surface.  Allowing this to happen would
		// result in lost energy somewhere and the situation really is not physically possible anyway.
		AssignedAsRadiantSurface.dimension( TotSurfaces, false );

		for ( Item = 1; Item <= NumOfHydrLowTempRadSys; ++Item ) {
			for ( SurfNum = 1; SurfNum <= HydrRadSys( Item ).NumOfSurfaces; ++SurfNum ) {
				CheckSurfNum = HydrRadSys( Item ).SurfacePtr( SurfNum );
				if ( CheckSurfNum == 0 ) continue;
				if ( AssignedAsRadiantSurface( CheckSurfNum ) ) {
					ShowSevereError( "Surface " + Surface( CheckSurfNum ).Name + " is referenced by more than one radiant system--this is not allowed" );
					ErrorsFound = true;
				} else {
					AssignedAsRadiantSurface( CheckSurfNum ) = true;
				}
				// Also check the other side of interzone partitions
				if ( ( Surface( CheckSurfNum ).ExtBoundCond > 0 ) && ( Surface( CheckSurfNum ).ExtBoundCond != CheckSurfNum ) ) {
					if ( AssignedAsRadiantSurface( Surface( CheckSurfNum ).ExtBoundCond ) ) {
						ShowSevereError( "Interzone surface " + Surface( Surface( CheckSurfNum ).ExtBoundCond ).Name + " is referenced by more than one radiant system--this is not allowed" );
						ErrorsFound = true;
					} else {
						AssignedAsRadiantSurface( Surface( CheckSurfNum ).ExtBoundCond ) = true;
					}
				}
			}
		}

		for ( Item = 1; Item <= NumOfCFloLowTempRadSys; ++Item ) {
			for ( SurfNum = 1; SurfNum <= CFloRadSys( Item ).NumOfSurfaces; ++SurfNum ) {
				CheckSurfNum = CFloRadSys( Item ).SurfacePtr( SurfNum );
				if ( CheckSurfNum == 0 ) continue;
				if ( AssignedAsRadiantSurface( CheckSurfNum ) ) {
					ShowSevereError( "Surface " + Surface( CheckSurfNum ).Name + " is referenced by more than one radiant system--this is not allowed" );
					ErrorsFound = true;
				} else {
					AssignedAsRadiantSurface( CheckSurfNum ) = true;
				}
				// Also check the other side of interzone partitions
				if ( ( Surface( CheckSurfNum ).ExtBoundCond > 0 ) && ( Surface( CheckSurfNum ).ExtBoundCond != CheckSurfNum ) ) {
					if ( AssignedAsRadiantSurface( Surface( CheckSurfNum ).ExtBoundCond ) ) {
						ShowSevereError( "Interzone surface " + Surface( Surface( CheckSurfNum ).ExtBoundCond ).Name + " is referenced by more than one radiant system--this is not allowed" );
						ErrorsFound = true;
					} else {
						AssignedAsRadiantSurface( Surface( CheckSurfNum ).ExtBoundCond ) = true;
					}
				}
			}
		}

		for ( Item = 1; Item <= NumOfElecLowTempRadSys; ++Item ) {
			for ( SurfNum = 1; SurfNum <= ElecRadSys( Item ).NumOfSurfaces; ++SurfNum ) {
				CheckSurfNum = ElecRadSys( Item ).SurfacePtr( SurfNum );
				if ( CheckSurfNum == 0 ) continue;
				if ( AssignedAsRadiantSurface( CheckSurfNum ) ) {
					ShowSevereError( "Surface " + Surface( CheckSurfNum ).Name + " is referenced by more than one radiant system--this is not allowed" );
					ErrorsFound = true;
				} else {
					AssignedAsRadiantSurface( CheckSurfNum ) = true;
				}
				// Also check the other side of interzone partitions
				if ( ( Surface( CheckSurfNum ).ExtBoundCond > 0 ) && ( Surface( CheckSurfNum ).ExtBoundCond != CheckSurfNum ) ) {
					if ( AssignedAsRadiantSurface( Surface( CheckSurfNum ).ExtBoundCond ) ) {
						ShowSevereError( "Interzone surface " + Surface( Surface( CheckSurfNum ).ExtBoundCond ).Name + " is referenced by more than one radiant system--this is not allowed" );
						ErrorsFound = true;
					} else {
						AssignedAsRadiantSurface( Surface( CheckSurfNum ).ExtBoundCond ) = true;
					}
				}
			}
		}

		AssignedAsRadiantSurface.deallocate();

		//  DEALLOCATE(SurfList)

		Alphas.deallocate();
		Numbers.deallocate();
		cAlphaFields.deallocate();
		cNumericFields.deallocate();
		lAlphaBlanks.deallocate();
		lNumericBlanks.deallocate();

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in input. Preceding conditions cause termination." );
		}

		// Set up the output variables for low temperature radiant systems
		// ZoneHVAC:LowTemperatureRadiant:VariableFlow (HydrRadSys)
		for ( Item = 1; Item <= NumOfHydrLowTempRadSys; ++Item ) {
			SetupOutputVariable( "Zone Radiant HVAC Heating Rate [W]", HydrRadSys( Item ).HeatPower, "System", "Average", HydrRadSys( Item ).Name );
			SetupOutputVariable( "Zone Radiant HVAC Heating Energy [J]", HydrRadSys( Item ).HeatEnergy, "System", "Sum", HydrRadSys( Item ).Name, _, "ENERGYTRANSFER", "HEATINGCOILS", _, "System" );
			SetupOutputVariable( "Zone Radiant HVAC Heating Fluid Energy [J]", HydrRadSys( Item ).HeatEnergy, "System", "Sum", HydrRadSys( Item ).Name, _, "PLANTLOOPHEATINGDEMAND", "HEATINGCOILS", _, "System" );
			SetupOutputVariable( "Zone Radiant HVAC Cooling Rate [W]", HydrRadSys( Item ).CoolPower, "System", "Average", HydrRadSys( Item ).Name );

			SetupOutputVariable( "Zone Radiant HVAC Cooling Energy [J]", HydrRadSys( Item ).CoolEnergy, "System", "Sum", HydrRadSys( Item ).Name, _, "ENERGYTRANSFER", "COOLINGCOILS", _, "System" );
			SetupOutputVariable( "Zone Radiant HVAC Cooling Fluid Energy [J]", HydrRadSys( Item ).CoolEnergy, "System", "Sum", HydrRadSys( Item ).Name, _, "PLANTLOOPCOOLINGDEMAND", "COOLINGCOILS", _, "System" );
			SetupOutputVariable( "Zone Radiant HVAC Mass Flow Rate [kg/s]", HydrRadSys( Item ).WaterMassFlowRate, "System", "Average", HydrRadSys( Item ).Name );
			SetupOutputVariable( "Zone Radiant HVAC Inlet Temperature [C]", HydrRadSys( Item ).WaterInletTemp, "System", "Average", HydrRadSys( Item ).Name );
			SetupOutputVariable( "Zone Radiant HVAC Outlet Temperature [C]", HydrRadSys( Item ).WaterOutletTemp, "System", "Average", HydrRadSys( Item ).Name );
			SetupOutputVariable( "Zone Radiant HVAC Moisture Condensation Time [s]", HydrRadSys( Item ).CondCausedTimeOff, "System", "Sum", HydrRadSys( Item ).Name );
			if ( AnyEnergyManagementSystemInModel ) {
				SetupEMSInternalVariable( "Hydronic Low Temp Radiant Design Water Volume Flow Rate for Heating", HydrRadSys( Item ).Name, "[m3/s]", HydrRadSys( Item ).WaterVolFlowMaxHeat );
				SetupEMSInternalVariable( "Hydronic Low Temp Radiant Design Water Volume Flow Rate for Cooling", HydrRadSys( Item ).Name, "[m3/s]", HydrRadSys( Item ).WaterVolFlowMaxCool );
				SetupEMSActuator( "Hydronic Low Temp Radiant", HydrRadSys( Item ).Name, "Water Mass Flow Rate", "[kg/s]", HydrRadSys( Item ).EMSOverrideOnWaterMdot, HydrRadSys( Item ).EMSWaterMdotOverrideValue );
			}
		}

		// Set up the output variables for low temperature radiant systems
		// ZoneHVAC:LowTemperatureRadiant:ConstantFlow (CFloRadSys)
		for ( Item = 1; Item <= NumOfCFloLowTempRadSys; ++Item ) {
			SetupOutputVariable( "Zone Radiant HVAC Heating Rate [W]", CFloRadSys( Item ).HeatPower, "System", "Average", CFloRadSys( Item ).Name );
			SetupOutputVariable( "Zone Radiant HVAC Heating Energy [J]", CFloRadSys( Item ).HeatEnergy, "System", "Sum", CFloRadSys( Item ).Name, _, "ENERGYTRANSFER", "HEATINGCOILS", _, "System" );
			SetupOutputVariable( "Zone Radiant HVAC Heating Fluid Heat Transfer Energy [J]", CFloRadSys( Item ).HeatEnergy, "System", "Sum", CFloRadSys( Item ).Name, _, "PLANTLOOPHEATINGDEMAND", "HEATINGCOILS", _, "System" );
			SetupOutputVariable( "Zone Radiant HVAC Cooling Rate [W]", CFloRadSys( Item ).CoolPower, "System", "Average", CFloRadSys( Item ).Name );
			SetupOutputVariable( "Zone Radiant HVAC Cooling Energy [J]", CFloRadSys( Item ).CoolEnergy, "System", "Sum", CFloRadSys( Item ).Name, _, "ENERGYTRANSFER", "COOLINGCOILS", _, "System" );
			SetupOutputVariable( "Zone Radiant HVAC Cooling Fluid Heat Transfer Energy [J]", CFloRadSys( Item ).CoolEnergy, "System", "Sum", CFloRadSys( Item ).Name, _, "PLANTLOOPCOOLINGDEMAND", "COOLINGCOILS", _, "System" );
			SetupOutputVariable( "Zone Radiant HVAC Mass Flow Rate [kg/s]", CFloRadSys( Item ).WaterMassFlowRate, "System", "Average", CFloRadSys( Item ).Name );
			SetupOutputVariable( "Zone Radiant HVAC Injection Mass Flow Rate [kg/s]", CFloRadSys( Item ).WaterInjectionRate, "System", "Average", CFloRadSys( Item ).Name );
			SetupOutputVariable( "Zone Radiant HVAC Recirculation Mass Flow Rate [kg/s]", CFloRadSys( Item ).WaterRecircRate, "System", "Average", CFloRadSys( Item ).Name );
			SetupOutputVariable( "Zone Radiant HVAC Inlet Temperature [C]", CFloRadSys( Item ).WaterInletTemp, "System", "Average", CFloRadSys( Item ).Name );
			SetupOutputVariable( "Zone Radiant HVAC Outlet Temperature [C]", CFloRadSys( Item ).WaterOutletTemp, "System", "Average", CFloRadSys( Item ).Name );
			SetupOutputVariable( "Zone Radiant HVAC Pump Inlet Temperature [C]", CFloRadSys( Item ).PumpInletTemp, "System", "Average", CFloRadSys( Item ).Name );
			SetupOutputVariable( "Zone Radiant HVAC Pump Electric Power [W]", CFloRadSys( Item ).PumpPower, "System", "Average", CFloRadSys( Item ).Name );
			SetupOutputVariable( "Zone Radiant HVAC Pump Electric Energy [J]", CFloRadSys( Item ).PumpEnergy, "System", "Sum", CFloRadSys( Item ).Name, _, "Electric", "Pumps", _, "Plant" );
			SetupOutputVariable( "Zone Radiant HVAC Pump Mass Flow Rate [kg/s]", CFloRadSys( Item ).PumpMassFlowRate, "System", "Average", CFloRadSys( Item ).Name );
			SetupOutputVariable( "Zone Radiant HVAC Pump Fluid Heat Gain Rate [W]", CFloRadSys( Item ).PumpHeattoFluid, "System", "Average", CFloRadSys( Item ).Name );
			SetupOutputVariable( "Zone Radiant HVAC Pump Fluid Heat Gain Energy [J]", CFloRadSys( Item ).PumpHeattoFluidEnergy, "System", "Sum", CFloRadSys( Item ).Name );
			SetupOutputVariable( "Zone Radiant HVAC Moisture Condensation Time [s]", CFloRadSys( Item ).CondCausedTimeOff, "System", "Sum", CFloRadSys( Item ).Name );
			if ( AnyEnergyManagementSystemInModel ) {
				SetupEMSInternalVariable( "Constant Flow Low Temp Radiant Design Water Mass Flow Rate", CFloRadSys( Item ).Name, "[m3/s]", CFloRadSys( Item ).WaterVolFlowMax );
				SetupEMSActuator( "Constant Flow Low Temp Radiant", CFloRadSys( Item ).Name, "Water Mass Flow Rate", "[kg/s]", CFloRadSys( Item ).EMSOverrideOnWaterMdot, CFloRadSys( Item ).EMSWaterMdotOverrideValue );
			}
		}

		for ( Item = 1; Item <= NumOfElecLowTempRadSys; ++Item ) {
			// Set up the output variables for low temperature radiant systems
			// ZoneHVAC:LowTemperatureRadiant:Electric (ElecRadSys)
			SetupOutputVariable( "Zone Radiant HVAC Electric Power [W]", ElecRadSys( Item ).ElecPower, "System", "Average", ElecRadSys( Item ).Name );
			SetupOutputVariable( "Zone Radiant HVAC Electric Energy [J]", ElecRadSys( Item ).ElecEnergy, "System", "Sum", ElecRadSys( Item ).Name, _, "ELECTRICITY", "Heating", _, "System" );
			SetupOutputVariable( "Zone Radiant HVAC Heating Rate [W]", ElecRadSys( Item ).HeatPower, "System", "Average", ElecRadSys( Item ).Name );
			SetupOutputVariable( "Zone Radiant HVAC Heating Energy [J]", ElecRadSys( Item ).HeatEnergy, "System", "Sum", ElecRadSys( Item ).Name, _, "ENERGYTRANSFER", "HEATINGCOILS", _, "System" );
		}

	}

	void
	InitLowTempRadiantSystem(
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		int const RadSysNum, // Index for the low temperature radiant system under consideration within the derived types
		int const SystemType // Type of radiant system: hydronic, constant flow, or electric
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   November 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine initializes variables relating to low temperature radiant
		// systems.

		// METHODOLOGY EMPLOYED:
		// Simply initializes whatever needs initializing.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::NumOfZones;
		using DataGlobals::BeginEnvrnFlag;
		using DataGlobals::AnyPlantInModel;
		using ScheduleManager::GetCurrentScheduleValue;
		using DataZoneEquipment::ZoneEquipInputsFilled;
		using DataZoneEquipment::CheckZoneEquipmentList;
		using DataPlant::ScanPlantLoopsForObject;
		using DataPlant::PlantLoop;
		using DataPlant::TypeOf_LowTempRadiant_VarFlow;
		using DataPlant::TypeOf_LowTempRadiant_ConstFlow;
		using PlantUtilities::SetComponentFlowRate;
		using PlantUtilities::InitComponentNodes;
		using FluidProperties::GetDensityGlycol;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const ZeroTol( 0.0000001 ); // Smallest non-zero value allowed
		static std::string const RoutineName( "InitLowTempRadiantSystem" );
		static gio::Fmt fmtF102( "(F10.2)" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 CurrentFlowSchedule; // Schedule value for flow fraction in a constant flow radiant system
		static bool ErrorsFound( false ); // In one-time initializations
		std::string Errout; // Message for errors
		static bool firstTime( true ); // For one-time initializations
		int RadNum; // Number of the radiant system (DO loop counter)
		int RadSurfNum; // Number of the radiant system surface (DO loop counter)
		int SurfNum; // Intermediate variable for keeping track of the surface number
		Real64 TotalEffic; // Intermediate calculation variable for total pump efficiency
		int ZoneNum; // Intermediate variable for keeping track of the zone number
		static Array1D_bool MyEnvrnFlagHydr;
		static Array1D_bool MyEnvrnFlagCFlo;
		static Array1D_bool MyEnvrnFlagElec;
		static bool MyEnvrnFlagGeneral( true );
		static bool ZoneEquipmentListChecked( false ); // True after the Zone Equipment List has been checked for items
		int Loop;
		static bool MyOneTimeFlag( true ); // Initialization flag
		static Array1D_bool MyPlantScanFlagHydr;
		static Array1D_bool MyPlantScanFlagCFlo;
		Real64 mdot; // local fluid mass flow rate
		Real64 rho; // local fluid density
		bool errFlag;
		// FLOW:

		if ( MyOneTimeFlag ) {
			MyEnvrnFlagHydr.allocate( NumOfHydrLowTempRadSys );
			MyEnvrnFlagCFlo.allocate( NumOfCFloLowTempRadSys );
			MyEnvrnFlagElec.allocate( NumOfElecLowTempRadSys );
			MyPlantScanFlagHydr.allocate( NumOfHydrLowTempRadSys );
			MyPlantScanFlagCFlo.allocate( NumOfCFloLowTempRadSys );
			MyPlantScanFlagHydr = true;
			MyPlantScanFlagCFlo = true;
			MyEnvrnFlagHydr = true;
			MyEnvrnFlagCFlo = true;
			MyEnvrnFlagElec = true;
			MyOneTimeFlag = false;
		}

		if ( firstTime ) {

			ZeroSourceSumHATsurf.dimension( NumOfZones, 0.0 );
			QRadSysSrcAvg.dimension( TotSurfaces, 0.0 );
			LastQRadSysSrc.dimension( TotSurfaces, 0.0 );
			LastSysTimeElapsed.dimension( TotSurfaces, 0.0 );
			LastTimeStepSys.dimension( TotSurfaces, 0.0 );
			MySizeFlagHydr.allocate( NumOfHydrLowTempRadSys );
			MySizeFlagCFlo.allocate( NumOfCFloLowTempRadSys );
			MySizeFlagElec.allocate( NumOfElecLowTempRadSys );
			MySizeFlagHydr = true;
			MySizeFlagCFlo = true;
			MySizeFlagElec = true;

			// Initialize total areas for all radiant systems
			for ( RadNum = 1; RadNum <= NumOfHydrLowTempRadSys; ++RadNum ) {
				HydrRadSys( RadNum ).TotalSurfaceArea = 0.0;
				for ( SurfNum = 1; SurfNum <= HydrRadSys( RadNum ).NumOfSurfaces; ++SurfNum ) {
					HydrRadSys( RadNum ).TotalSurfaceArea += Surface( HydrRadSys( RadNum ).SurfacePtr( SurfNum ) ).Area;
				}
			}
			for ( RadNum = 1; RadNum <= NumOfCFloLowTempRadSys; ++RadNum ) {
				CFloRadSys( RadNum ).TotalSurfaceArea = 0.0;
				for ( SurfNum = 1; SurfNum <= CFloRadSys( RadNum ).NumOfSurfaces; ++SurfNum ) {
					CFloRadSys( RadNum ).TotalSurfaceArea += Surface( CFloRadSys( RadNum ).SurfacePtr( SurfNum ) ).Area;
				}
			}
			for ( RadNum = 1; RadNum <= NumOfElecLowTempRadSys; ++RadNum ) {
				ElecRadSys( RadNum ).TotalSurfaceArea = 0.0;
				for ( SurfNum = 1; SurfNum <= ElecRadSys( RadNum ).NumOfSurfaces; ++SurfNum ) {
					ElecRadSys( RadNum ).TotalSurfaceArea += Surface( ElecRadSys( RadNum ).SurfacePtr( SurfNum ) ).Area;
				}
			}

			// Check pump parameters for constant flow hydronic radiant systems
			for ( RadNum = 1; RadNum <= NumOfCFloLowTempRadSys; ++RadNum ) {
				// Calculate the efficiency for each pump: The calculation
				// is based on the PMPSIM code in the ASHRAE Secondary Toolkit
				if ( ( CFloRadSys( RadNum ).NomPowerUse > ZeroTol ) && ( CFloRadSys( RadNum ).MotorEffic > ZeroTol ) ) {
					TotalEffic = CFloRadSys( RadNum ).WaterVolFlowMax * CFloRadSys( RadNum ).NomPumpHead / CFloRadSys( RadNum ).NomPowerUse;
					CFloRadSys( RadNum ).PumpEffic = TotalEffic / CFloRadSys( RadNum ).MotorEffic;
					if ( CFloRadSys( RadNum ).PumpEffic < 0.50 ) {
						gio::write( Errout, fmtF102 ) << CFloRadSys( RadNum ).PumpEffic * 100.0;
						ShowWarningError( "Check input. Calc Pump Efficiency=" + stripped( Errout ) + "% which is less than 50%, for pump in radiant system " + CFloRadSys( RadNum ).Name );
					} else if ( ( CFloRadSys( RadNum ).PumpEffic > 0.95 ) && ( CFloRadSys( RadNum ).PumpEffic <= 1.0 ) ) {
						gio::write( Errout, fmtF102 ) << CFloRadSys( RadNum ).PumpEffic * 100.0;
						ShowWarningError( "Check input.  Calc Pump Efficiency=" + stripped( Errout ) + "% is approaching 100%, for pump in radiant system " + CFloRadSys( RadNum ).Name );
					} else if ( CFloRadSys( RadNum ).PumpEffic > 1.0 ) {
						gio::write( Errout, fmtF102 ) << CFloRadSys( RadNum ).PumpEffic * 100.0;
						ShowSevereError( "Check input.  Calc Pump Efficiency=" + stripped( Errout ) + "% which is bigger than 100%, for pump in radiant system " + CFloRadSys( RadNum ).Name );
						ErrorsFound = true;
					}
				} else {
					ShowSevereError( "Check input.  Pump nominal power and motor efficiency cannot be 0, for pump=" + CFloRadSys( RadNum ).Name );
					ErrorsFound = true;
				}
			}

			firstTime = false;

		}

		if ( SystemType == HydronicSystem ) {
			if ( MyPlantScanFlagHydr( RadSysNum ) && allocated( PlantLoop ) ) {
				errFlag = false;
				if ( HydrRadSys( RadSysNum ).HotWaterInNode > 0 ) {
					ScanPlantLoopsForObject( HydrRadSys( RadSysNum ).Name, TypeOf_LowTempRadiant_VarFlow, HydrRadSys( RadSysNum ).HWLoopNum, HydrRadSys( RadSysNum ).HWLoopSide, HydrRadSys( RadSysNum ).HWBranchNum, HydrRadSys( RadSysNum ).HWCompNum, _, _, _, HydrRadSys( RadSysNum ).HotWaterInNode, _, errFlag );
					if ( errFlag ) {
						ShowFatalError( "InitLowTempRadiantSystem: Program terminated due to previous condition(s)." );
					}
				}
				if ( HydrRadSys( RadSysNum ).ColdWaterInNode > 0 ) {
					ScanPlantLoopsForObject( HydrRadSys( RadSysNum ).Name, TypeOf_LowTempRadiant_VarFlow, HydrRadSys( RadSysNum ).CWLoopNum, HydrRadSys( RadSysNum ).CWLoopSide, HydrRadSys( RadSysNum ).CWBranchNum, HydrRadSys( RadSysNum ).CWCompNum, _, _, _, HydrRadSys( RadSysNum ).ColdWaterInNode, _, errFlag );
					if ( errFlag ) {
						ShowFatalError( "InitLowTempRadiantSystem: Program terminated due to previous condition(s)." );
					}
				}
				MyPlantScanFlagHydr( RadSysNum ) = false;
			} else if ( MyPlantScanFlagHydr( RadSysNum ) && ! AnyPlantInModel ) {
				MyPlantScanFlagHydr( RadSysNum ) = false;
			}
		}

		if ( SystemType == ConstantFlowSystem ) {
			if ( MyPlantScanFlagCFlo( RadSysNum ) && allocated( PlantLoop ) ) {
				errFlag = false;
				if ( CFloRadSys( RadSysNum ).HotWaterInNode > 0 ) {
					ScanPlantLoopsForObject( CFloRadSys( RadSysNum ).Name, TypeOf_LowTempRadiant_ConstFlow, CFloRadSys( RadSysNum ).HWLoopNum, CFloRadSys( RadSysNum ).HWLoopSide, CFloRadSys( RadSysNum ).HWBranchNum, CFloRadSys( RadSysNum ).HWCompNum, _, _, _, CFloRadSys( RadSysNum ).HotWaterInNode, _, errFlag );
					if ( errFlag ) {
						ShowFatalError( "InitLowTempRadiantSystem: Program terminated due to previous condition(s)." );
					}
				}
				if ( CFloRadSys( RadSysNum ).ColdWaterInNode > 0 ) {
					ScanPlantLoopsForObject( CFloRadSys( RadSysNum ).Name, TypeOf_LowTempRadiant_ConstFlow, CFloRadSys( RadSysNum ).CWLoopNum, CFloRadSys( RadSysNum ).CWLoopSide, CFloRadSys( RadSysNum ).CWBranchNum, CFloRadSys( RadSysNum ).CWCompNum, _, _, _, CFloRadSys( RadSysNum ).ColdWaterInNode, _, errFlag );
					if ( errFlag ) {
						ShowFatalError( "InitLowTempRadiantSystem: Program terminated due to previous condition(s)." );
					}
				}
				MyPlantScanFlagCFlo( RadSysNum ) = false;
			} else if ( MyPlantScanFlagCFlo( RadSysNum ) && ! AnyPlantInModel ) {
				MyPlantScanFlagCFlo( RadSysNum ) = false;
			}
		}

		// need to check all units to see if they are on Zone Equipment List or issue warning
		if ( ! ZoneEquipmentListChecked && ZoneEquipInputsFilled ) {
			ZoneEquipmentListChecked = true;
			for ( Loop = 1; Loop <= TotalNumOfRadSystems; ++Loop ) {
				{ auto const SELECT_CASE_var( RadSysTypes( Loop ).SystemType );

				if ( SELECT_CASE_var == HydronicSystem ) {
					if ( CheckZoneEquipmentList( "ZoneHVAC:LowTemperatureRadiant:VariableFlow", RadSysTypes( Loop ).Name ) ) continue;
					ShowSevereError( "InitLowTempRadiantSystem: Unit=[ZoneHVAC:LowTemperatureRadiant:VariableFlow," + RadSysTypes( Loop ).Name + "] is not on any ZoneHVAC:EquipmentList.  It will not be simulated." );
				} else if ( SELECT_CASE_var == ConstantFlowSystem ) {
					if ( CheckZoneEquipmentList( "ZoneHVAC:LowTemperatureRadiant:ConstantFlow", RadSysTypes( Loop ).Name ) ) continue;
					ShowSevereError( "InitLowTempRadiantSystem: Unit=[ZoneHVAC:LowTemperatureRadiant:ConstantFlow," + RadSysTypes( Loop ).Name + "] is not on any ZoneHVAC:EquipmentList.  It will not be simulated." );
				} else if ( SELECT_CASE_var == ElectricSystem ) {
					if ( CheckZoneEquipmentList( "ZoneHVAC:LowTemperatureRadiant:Electric", RadSysTypes( Loop ).Name ) ) continue;
					ShowSevereError( "InitLowTempRadiantSystem: Unit=[ZoneHVAC:LowTemperatureRadiant:Electric," + RadSysTypes( Loop ).Name + "] is not on any ZoneHVAC:EquipmentList.  It will not be simulated." );
				} else { // Illegal system, but checked earlier
				}}
			}
		}

		if ( ! SysSizingCalc && ( SystemType == HydronicSystem ) ) {
			if ( MySizeFlagHydr( RadSysNum ) && ! MyPlantScanFlagHydr( RadSysNum ) ) {
				// for each radiant system do the sizing once.
				SizeLowTempRadiantSystem( RadSysNum, SystemType );
				MySizeFlagHydr( RadSysNum ) = false;

				// Can this system actually do cooling?
				if ( ( HydrRadSys( RadSysNum ).WaterVolFlowMaxCool > 0.0 ) && ( HydrRadSys( RadSysNum ).ColdWaterInNode > 0 ) && ( HydrRadSys( RadSysNum ).ColdWaterOutNode > 0 ) && ( HydrRadSys( RadSysNum ).ColdSetptSchedPtr > 0 ) ) {
					HydrRadSys( RadSysNum ).CoolingSystem = true;
				}

				// Can this system actually do heating?
				if ( ( HydrRadSys( RadSysNum ).WaterVolFlowMaxHeat > 0.0 ) && ( HydrRadSys( RadSysNum ).HotWaterInNode > 0 ) && ( HydrRadSys( RadSysNum ).HotWaterOutNode > 0 ) && ( HydrRadSys( RadSysNum ).HotSetptSchedPtr > 0 ) ) {
					HydrRadSys( RadSysNum ).HeatingSystem = true;
				}

				//set design mass flow rates
				if ( HydrRadSys( RadSysNum ).HotWaterInNode > 0 ) {
					rho = GetDensityGlycol( PlantLoop( HydrRadSys( RadSysNum ).HWLoopNum ).FluidName, 60.0, PlantLoop( HydrRadSys( RadSysNum ).HWLoopNum ).FluidIndex, RoutineName );
					HydrRadSys( RadSysNum ).WaterFlowMaxHeat = rho * HydrRadSys( RadSysNum ).WaterVolFlowMaxHeat;
					InitComponentNodes( 0.0, HydrRadSys( RadSysNum ).WaterFlowMaxHeat, HydrRadSys( RadSysNum ).HotWaterInNode, HydrRadSys( RadSysNum ).HotWaterOutNode, HydrRadSys( RadSysNum ).HWLoopNum, HydrRadSys( RadSysNum ).HWLoopSide, HydrRadSys( RadSysNum ).HWBranchNum, HydrRadSys( RadSysNum ).HWCompNum );
				}
				if ( HydrRadSys( RadSysNum ).ColdWaterInNode > 0 ) {
					rho = GetDensityGlycol( PlantLoop( HydrRadSys( RadSysNum ).CWLoopNum ).FluidName, InitConvTemp, PlantLoop( HydrRadSys( RadSysNum ).CWLoopNum ).FluidIndex, RoutineName );
					HydrRadSys( RadSysNum ).WaterFlowMaxCool = rho * HydrRadSys( RadSysNum ).WaterVolFlowMaxCool;
					InitComponentNodes( 0.0, HydrRadSys( RadSysNum ).WaterFlowMaxCool, HydrRadSys( RadSysNum ).ColdWaterInNode, HydrRadSys( RadSysNum ).ColdWaterOutNode, HydrRadSys( RadSysNum ).CWLoopNum, HydrRadSys( RadSysNum ).CWLoopSide, HydrRadSys( RadSysNum ).CWBranchNum, HydrRadSys( RadSysNum ).CWCompNum );
				}
			}
		}

		if ( ! SysSizingCalc && ( SystemType == ConstantFlowSystem ) ) {
			if ( MySizeFlagCFlo( RadSysNum ) && ! MyPlantScanFlagCFlo( RadSysNum ) ) {
				// for each radiant system do the sizing once.
				SizeLowTempRadiantSystem( RadSysNum, SystemType );

				//set design mass flow rates
				if ( CFloRadSys( RadSysNum ).HotWaterInNode > 0 ) {
					rho = GetDensityGlycol( PlantLoop( CFloRadSys( RadSysNum ).HWLoopNum ).FluidName, 60.0, PlantLoop( CFloRadSys( RadSysNum ).HWLoopNum ).FluidIndex, RoutineName );
					CFloRadSys( RadSysNum ).HotDesignWaterMassFlowRate = rho * CFloRadSys( RadSysNum ).WaterVolFlowMax;
					InitComponentNodes( 0.0, CFloRadSys( RadSysNum ).HotDesignWaterMassFlowRate, CFloRadSys( RadSysNum ).HotWaterInNode, CFloRadSys( RadSysNum ).HotWaterOutNode, CFloRadSys( RadSysNum ).HWLoopNum, CFloRadSys( RadSysNum ).HWLoopSide, CFloRadSys( RadSysNum ).HWBranchNum, CFloRadSys( RadSysNum ).HWCompNum );
				}
				if ( CFloRadSys( RadSysNum ).ColdWaterInNode > 0 ) {
					rho = GetDensityGlycol( PlantLoop( CFloRadSys( RadSysNum ).CWLoopNum ).FluidName, InitConvTemp, PlantLoop( CFloRadSys( RadSysNum ).CWLoopNum ).FluidIndex, RoutineName );
					CFloRadSys( RadSysNum ).ColdDesignWaterMassFlowRate = rho * CFloRadSys( RadSysNum ).WaterVolFlowMax;
					InitComponentNodes( 0.0, CFloRadSys( RadSysNum ).ColdDesignWaterMassFlowRate, CFloRadSys( RadSysNum ).ColdWaterInNode, CFloRadSys( RadSysNum ).ColdWaterOutNode, CFloRadSys( RadSysNum ).CWLoopNum, CFloRadSys( RadSysNum ).CWLoopSide, CFloRadSys( RadSysNum ).CWBranchNum, CFloRadSys( RadSysNum ).CWCompNum );
				}
				MySizeFlagCFlo( RadSysNum ) = false;
			}
		}

		if ( ! SysSizingCalc && ( SystemType == ElectricSystem ) ) {
			if ( MySizeFlagElec( RadSysNum ) ) {
				// for each radiant system do the sizing once.
				SizeLowTempRadiantSystem( RadSysNum, SystemType );
				MySizeFlagElec( RadSysNum ) = false;
			}
		}

		if ( BeginEnvrnFlag && MyEnvrnFlagGeneral ) {
			ZeroSourceSumHATsurf = 0.0;
			QRadSysSrcAvg = 0.0;
			LastQRadSysSrc = 0.0;
			LastSysTimeElapsed = 0.0;
			LastTimeStepSys = 0.0;
			MyEnvrnFlagGeneral = false;
		}
		if ( ! BeginEnvrnFlag ) MyEnvrnFlagGeneral = true;

		if ( SystemType == HydronicSystem ) {
			if ( BeginEnvrnFlag && MyEnvrnFlagHydr( RadSysNum ) ) {
				HydrRadSys( RadSysNum ).HeatPower = 0.0;
				HydrRadSys( RadSysNum ).HeatEnergy = 0.0;
				HydrRadSys( RadSysNum ).CoolPower = 0.0;
				HydrRadSys( RadSysNum ).CoolEnergy = 0.0;
				HydrRadSys( RadSysNum ).WaterInletTemp = 0.0;
				HydrRadSys( RadSysNum ).WaterOutletTemp = 0.0;
				HydrRadSys( RadSysNum ).WaterMassFlowRate = 0.0;

				if ( ! MyPlantScanFlagHydr( RadSysNum ) ) {
					if ( HydrRadSys( RadSysNum ).HotWaterInNode > 0 ) {
						InitComponentNodes( 0.0, HydrRadSys( RadSysNum ).WaterFlowMaxHeat, HydrRadSys( RadSysNum ).HotWaterInNode, HydrRadSys( RadSysNum ).HotWaterOutNode, HydrRadSys( RadSysNum ).HWLoopNum, HydrRadSys( RadSysNum ).HWLoopSide, HydrRadSys( RadSysNum ).HWBranchNum, HydrRadSys( RadSysNum ).HWCompNum );
					}
					if ( HydrRadSys( RadSysNum ).ColdWaterInNode > 0 ) {
						InitComponentNodes( 0.0, HydrRadSys( RadSysNum ).WaterFlowMaxCool, HydrRadSys( RadSysNum ).ColdWaterInNode, HydrRadSys( RadSysNum ).ColdWaterOutNode, HydrRadSys( RadSysNum ).CWLoopNum, HydrRadSys( RadSysNum ).CWLoopSide, HydrRadSys( RadSysNum ).CWBranchNum, HydrRadSys( RadSysNum ).CWCompNum );
					}
				}
				MyEnvrnFlagHydr( RadSysNum ) = false;
			}
		} //NumOfHydrLowTempRadSys > 0
		if ( ! BeginEnvrnFlag && SystemType == HydronicSystem ) MyEnvrnFlagHydr( RadSysNum ) = true;

		if ( SystemType == ConstantFlowSystem ) {
			if ( BeginEnvrnFlag && MyEnvrnFlagCFlo( RadSysNum ) ) {
				CFloRadSys( RadSysNum ).WaterInletTemp = 0.0;
				CFloRadSys( RadSysNum ).WaterOutletTemp = 0.0;
				CFloRadSys( RadSysNum ).PumpInletTemp = 0.0;
				CFloRadSys( RadSysNum ).WaterMassFlowRate = 0.0;
				CFloRadSys( RadSysNum ).WaterInjectionRate = 0.0;
				CFloRadSys( RadSysNum ).WaterRecircRate = 0.0;
				CFloRadSys( RadSysNum ).HeatPower = 0.0;
				CFloRadSys( RadSysNum ).HeatEnergy = 0.0;
				CFloRadSys( RadSysNum ).CoolPower = 0.0;
				CFloRadSys( RadSysNum ).CoolEnergy = 0.0;
				CFloRadSys( RadSysNum ).PumpPower = 0.0;
				CFloRadSys( RadSysNum ).PumpMassFlowRate = 0.0;
				CFloRadSys( RadSysNum ).PumpHeattoFluid = 0.0;

				if ( ! MyPlantScanFlagCFlo( RadSysNum ) ) {
					if ( CFloRadSys( RadSysNum ).HotWaterInNode > 0 ) {
						InitComponentNodes( 0.0, CFloRadSys( RadSysNum ).HotDesignWaterMassFlowRate, CFloRadSys( RadSysNum ).HotWaterInNode, CFloRadSys( RadSysNum ).HotWaterOutNode, CFloRadSys( RadSysNum ).HWLoopNum, CFloRadSys( RadSysNum ).HWLoopSide, CFloRadSys( RadSysNum ).HWBranchNum, CFloRadSys( RadSysNum ).HWCompNum );
					}
					if ( CFloRadSys( RadSysNum ).ColdWaterInNode > 0 ) {
						InitComponentNodes( 0.0, CFloRadSys( RadSysNum ).ColdDesignWaterMassFlowRate, CFloRadSys( RadSysNum ).ColdWaterInNode, CFloRadSys( RadSysNum ).ColdWaterOutNode, CFloRadSys( RadSysNum ).CWLoopNum, CFloRadSys( RadSysNum ).CWLoopSide, CFloRadSys( RadSysNum ).CWBranchNum, CFloRadSys( RadSysNum ).CWCompNum );
					}
				}
				MyEnvrnFlagCFlo( RadSysNum ) = false;
			}
		} // NumOfCFloLowTempRadSys > 0
		if ( ! BeginEnvrnFlag && SystemType == ConstantFlowSystem ) MyEnvrnFlagCFlo( RadSysNum ) = true;

		if ( SystemType == ElectricSystem ) {
			if ( BeginEnvrnFlag && MyEnvrnFlagElec( RadSysNum ) ) {
				ElecRadSys( RadSysNum ).HeatPower = 0.0;
				ElecRadSys( RadSysNum ).HeatEnergy = 0.0;
				ElecRadSys( RadSysNum ).ElecPower = 0.0;
				ElecRadSys( RadSysNum ).ElecEnergy = 0.0;
			}
			MyEnvrnFlagElec( RadSysNum ) = false;
		}
		if ( ! BeginEnvrnFlag && SystemType == ElectricSystem ) MyEnvrnFlagElec( RadSysNum ) = true;

		if ( SystemType == ConstantFlowSystem ) {

			// Can this system actually do heating?
			if ( ( CFloRadSys( RadSysNum ).WaterVolFlowMax > 0.0 ) && ( CFloRadSys( RadSysNum ).HotWaterInNode > 0 ) && ( CFloRadSys( RadSysNum ).HotWaterOutNode > 0 ) && ( CFloRadSys( RadSysNum ).HotWaterHiTempSchedPtr > 0 ) && ( CFloRadSys( RadSysNum ).HotWaterLoTempSchedPtr > 0 ) && ( CFloRadSys( RadSysNum ).HotCtrlHiTempSchedPtr > 0 ) && ( CFloRadSys( RadSysNum ).HotCtrlLoTempSchedPtr > 0 ) ) {
				CFloRadSys( RadSysNum ).HeatingSystem = true;
			}

			// Can this system actually do cooling?
			if ( ( CFloRadSys( RadSysNum ).WaterVolFlowMax > 0.0 ) && ( CFloRadSys( RadSysNum ).ColdWaterInNode > 0 ) && ( CFloRadSys( RadSysNum ).ColdWaterOutNode > 0 ) && ( CFloRadSys( RadSysNum ).ColdWaterHiTempSchedPtr > 0 ) && ( CFloRadSys( RadSysNum ).ColdWaterLoTempSchedPtr > 0 ) && ( CFloRadSys( RadSysNum ).ColdCtrlHiTempSchedPtr > 0 ) && ( CFloRadSys( RadSysNum ).ColdCtrlLoTempSchedPtr > 0 ) ) {
				CFloRadSys( RadSysNum ).CoolingSystem = true;
			}

		}

		if ( BeginTimeStepFlag && FirstHVACIteration ) { // This is the first pass through in a particular time step

			{ auto const SELECT_CASE_var( SystemType );

			if ( SELECT_CASE_var == HydronicSystem ) {

				ZoneNum = HydrRadSys( RadSysNum ).ZonePtr;
				ZeroSourceSumHATsurf( ZoneNum ) = SumHATsurf( ZoneNum ); // Set this to figure what part of the load the radiant system meets
				for ( RadSurfNum = 1; RadSurfNum <= HydrRadSys( RadSysNum ).NumOfSurfaces; ++RadSurfNum ) {
					SurfNum = HydrRadSys( RadSysNum ).SurfacePtr( RadSurfNum );
					QRadSysSrcAvg( SurfNum ) = 0.0; // Initialize this variable to zero (radiant system defaults to off)
					LastQRadSysSrc( SurfNum ) = 0.0; // At the start of a time step, reset to zero so average calculation can begin again
					LastSysTimeElapsed( SurfNum ) = 0.0; // At the start of a time step, reset to zero so average calculation can begin again
					LastTimeStepSys( SurfNum ) = 0.0; // At the start of a time step, reset to zero so average calculation can begin again
				}

			} else if ( SELECT_CASE_var == ConstantFlowSystem ) {

				ZoneNum = CFloRadSys( RadSysNum ).ZonePtr;
				ZeroSourceSumHATsurf( ZoneNum ) = SumHATsurf( ZoneNum ); // Set this to figure what part of the load the radiant system meets
				for ( RadSurfNum = 1; RadSurfNum <= CFloRadSys( RadSysNum ).NumOfSurfaces; ++RadSurfNum ) {
					SurfNum = CFloRadSys( RadSysNum ).SurfacePtr( RadSurfNum );
					QRadSysSrcAvg( SurfNum ) = 0.0; // Initialize this variable to zero (radiant system defaults to off)
					LastQRadSysSrc( SurfNum ) = 0.0; // At the start of a time step, reset to zero so average calculation can begin again
					LastSysTimeElapsed( SurfNum ) = 0.0; // At the start of a time step, reset to zero so average calculation can begin again
					LastTimeStepSys( SurfNum ) = 0.0; // At the start of a time step, reset to zero so average calculation can begin again
				}

			} else if ( SELECT_CASE_var == ElectricSystem ) {

				ZoneNum = ElecRadSys( RadSysNum ).ZonePtr;
				ZeroSourceSumHATsurf( ZoneNum ) = SumHATsurf( ZoneNum ); // Set this to figure what part of the load the radiant system meets
				for ( RadSurfNum = 1; RadSurfNum <= ElecRadSys( RadSysNum ).NumOfSurfaces; ++RadSurfNum ) {
					SurfNum = ElecRadSys( RadSysNum ).SurfacePtr( RadSurfNum );
					QRadSysSrcAvg( SurfNum ) = 0.0; // Initialize this variable to zero (radiant system defaults to off)
					LastQRadSysSrc( SurfNum ) = 0.0; // At the start of a time step, reset to zero so average calculation can begin again
					LastSysTimeElapsed( SurfNum ) = 0.0; // At the start of a time step, reset to zero so average calculation can begin again
					LastTimeStepSys( SurfNum ) = 0.0; // At the start of a time step, reset to zero so average calculation can begin again
				}

			} else {

				ShowSevereError( "Radiant system entered without specification of type: electric, constant flow, or hydronic?" );
				ShowContinueError( "Occurs in Radiant System=" + HydrRadSys( RadSysNum ).Name );
				ShowFatalError( "Preceding condition causes termination." );

			}}

		} // ...for first pass through in a particular time step.

		{ auto const SELECT_CASE_var( SystemType );

		if ( SELECT_CASE_var == HydronicSystem ) {

			// Initialize the appropriate node data
			if ( HydrRadSys( RadSysNum ).HeatingSystem ) {
				mdot = 0.0;
				SetComponentFlowRate( mdot, HydrRadSys( RadSysNum ).HotWaterInNode, HydrRadSys( RadSysNum ).HotWaterOutNode, HydrRadSys( RadSysNum ).HWLoopNum, HydrRadSys( RadSysNum ).HWLoopSide, HydrRadSys( RadSysNum ).HWBranchNum, HydrRadSys( RadSysNum ).HWCompNum );
			}
			if ( HydrRadSys( RadSysNum ).CoolingSystem ) {
				mdot = 0.0;
				SetComponentFlowRate( mdot, HydrRadSys( RadSysNum ).ColdWaterInNode, HydrRadSys( RadSysNum ).ColdWaterOutNode, HydrRadSys( RadSysNum ).CWLoopNum, HydrRadSys( RadSysNum ).CWLoopSide, HydrRadSys( RadSysNum ).CWBranchNum, HydrRadSys( RadSysNum ).CWCompNum );
			}

		} else if ( SELECT_CASE_var == ConstantFlowSystem ) {
			// Initialize the appropriate node data
			if ( CFloRadSys( RadSysNum ).HeatingSystem ) {
				if ( CFloRadSys( RadSysNum ).VolFlowSchedPtr > 0 ) {
					CurrentFlowSchedule = GetCurrentScheduleValue( CFloRadSys( RadSysNum ).VolFlowSchedPtr );
				} else {
					CurrentFlowSchedule = 1.0; // Allow user to avoid putting in a schedule (defaults to constant flow at all times)
				}
				if ( CurrentFlowSchedule > 1.0 ) CurrentFlowSchedule = 1.0; // Do not allow more flow than design maximum
				if ( CurrentFlowSchedule < 0.0 ) CurrentFlowSchedule = 0.0; // Do not allow negative flow

				CFloRadSys( RadSysNum ).WaterMassFlowRate = CFloRadSys( RadSysNum ).HotDesignWaterMassFlowRate * CurrentFlowSchedule;

				if ( CFloRadSys( RadSysNum ).EMSOverrideOnWaterMdot ) CFloRadSys( RadSysNum ).WaterMassFlowRate = CFloRadSys( RadSysNum ).EMSWaterMdotOverrideValue;

				SetComponentFlowRate( CFloRadSys( RadSysNum ).WaterMassFlowRate, CFloRadSys( RadSysNum ).HotWaterInNode, CFloRadSys( RadSysNum ).HotWaterOutNode, CFloRadSys( RadSysNum ).HWLoopNum, CFloRadSys( RadSysNum ).HWLoopSide, CFloRadSys( RadSysNum ).HWBranchNum, CFloRadSys( RadSysNum ).HWCompNum );
			}
			if ( CFloRadSys( RadSysNum ).CoolingSystem ) {
				if ( CFloRadSys( RadSysNum ).VolFlowSchedPtr > 0 ) {
					CurrentFlowSchedule = GetCurrentScheduleValue( CFloRadSys( RadSysNum ).VolFlowSchedPtr );
				} else {
					CurrentFlowSchedule = 1.0; // Allow user to avoid putting in a schedule (defaults to constant flow at all times)
				}
				if ( CurrentFlowSchedule > 1.0 ) CurrentFlowSchedule = 1.0; // Do not allow more flow than design maximum
				if ( CurrentFlowSchedule < 0.0 ) CurrentFlowSchedule = 0.0; // Do not allow negative flow
				CFloRadSys( RadSysNum ).WaterMassFlowRate = CFloRadSys( RadSysNum ).ColdDesignWaterMassFlowRate * CurrentFlowSchedule;

				if ( CFloRadSys( RadSysNum ).EMSOverrideOnWaterMdot ) CFloRadSys( RadSysNum ).WaterMassFlowRate = CFloRadSys( RadSysNum ).EMSWaterMdotOverrideValue;

				SetComponentFlowRate( CFloRadSys( RadSysNum ).WaterMassFlowRate, CFloRadSys( RadSysNum ).ColdWaterInNode, CFloRadSys( RadSysNum ).ColdWaterOutNode, CFloRadSys( RadSysNum ).CWLoopNum, CFloRadSys( RadSysNum ).CWLoopSide, CFloRadSys( RadSysNum ).CWBranchNum, CFloRadSys( RadSysNum ).CWCompNum );
			}

		} else if ( SELECT_CASE_var == ElectricSystem ) {

		} else {

		}}

		OperatingMode = NotOperating; // System is not operating or can't operate; will be reset elsewhere, if necessary

	}

	void
	SizeLowTempRadiantSystem(
		int const RadSysNum, // Index for the low temperature radiant system under consideration within the derived types
		int const SystemType // Type of radiant system: hydronic, constant flow, or electric
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   February 2002
		//       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
		//                      August 2014 Bereket Nigusse, added scalable sizing
		//                      March 2014 Daeho Kang, add constant flow system autosizing
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing low temperature radiant components for which flow rates
		// and tube length or max ekectric power have not been specified in the input

		// METHODOLOGY EMPLOYED:
		// Obtains flow rates from the zone sizing arrays and plant sizing data. Maximum electric
		// power is set to the design heat load. Tube length is calculated by rule-of-thumb from
		// rge surface area.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSizing;
		using PlantUtilities::RegisterPlantCompDesignFlow;
		using ReportSizingManager::RequestSizing;
		using ReportSizingManager::ReportSizingOutput;
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSpecificHeatGlycol;
		using DataPlant::PlantLoop;
		using DataPlant::MyPlantSizingIndex;
		using General::RoundSigDigits;
		using DataHVACGlobals::HeatingCapacitySizing;
		using DataHVACGlobals::CoolingCapacitySizing;
		using DataHVACGlobals::AutoCalculateSizing;
		using DataHeatBalance::Zone;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "SizeLowTempRadiantSystem" );
		static int const OFF = 0;
		static int const ClgHtg = 1;
		static int const ClgOnly = 2;
		static int const HtgOnly = 3;

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int PltSizHeatNum( 0 ); // index of plant sizing object for 1st heating loop
		int PltSizCoolNum( 0 ); // index of plant sizing object for 1st cooling loop
		int SurfNum; // surface index in radiant system data structure
		bool ErrorsFound( false ); // If errors detected in input
		Real64 rho;
		Real64 Cp;
		bool IsAutoSize( false ); // Indicator to autosize
		// Real64 MaxElecPowerDes( 0.0 ); // Design electric power for reproting
		Real64 WaterVolFlowMaxHeatDes( 0.0 ); // Design hot water flow for reproting
		Real64 WaterVolFlowMaxHeatUser( 0.0 ); // User hard-sized hot water flow for
		Real64 WaterVolFlowMaxCoolDes( 0.0 ); // Design chilled water flow for reproting
		Real64 WaterVolFlowMaxCoolUser( 0.0 ); // User hard-sized chilled water flow for reproting
		Real64 TubeLengthDes( 0.0 ); // Design tube length for reproting
		Real64 TubeLengthUser( 0.0 ); // User hard-sized tube length for reproting
		std::string CompName; // component name
		std::string CompType; // component type
		std::string SizingString; // input field sizing description (e.g., Nominal Capacity)
		Real64 TempSize; // autosized value of coil input field
		int FieldNum = 1; // IDD numeric field number where input field description is found
		int SizingMethod; // Integer representation of sizing method name (e.g. CoolingCapacitySizing, HeatingCapacitySizing)
		bool PrintFlag; // TRUE when sizing information is reported in the eio file
		int CapSizingMethod( 0 ); // capacity sizing methods (HeatingDesignCapacity, CapacityPerFloorArea, FractionOfAutosizedCoolingCapacity, and FractionOfAutosizedHeatingCapacity )
		Real64 DesCoilLoad; // design autosized or user specified capacity
		int OpMode( 1 );				// System operating mode
		int HeatNode;			// Hot water inlet node to determine system operating mode
		int CoolNode;			// Chilled water inlet node to determine system operating mode
		Real64 WaterVolFlowMaxDes;		// Design water volume flow rate for reproting
		Real64 WaterVolFlowMaxUser;		// User hard-sized water volume flow rate for reproting

		DesCoilLoad = 0.0;
		DataScalableCapSizingON = false;
		DataFracOfAutosizedHeatingCapacity = 1.0;

		if ( SystemType == ElectricSystem ) {

			if ( ElecRadSys( RadSysNum ).MaxElecPower == AutoSize ) {
				IsAutoSize = true;
			}

			if ( CurZoneEqNum > 0 ) {

				CompType = "ZoneHVAC:LowTemperatureRadiant:Electric";
				CompName = ElecRadSys( RadSysNum ).Name;
				SizingMethod = HeatingCapacitySizing;
				FieldNum = 1;
				PrintFlag = true;
				SizingString = ElecRadSysNumericFields( RadSysNum ).FieldNames( FieldNum ) + " [W]";
				CapSizingMethod = ElecRadSys( RadSysNum ).HeatingCapMethod;
				ZoneEqSizing( CurZoneEqNum ).SizingMethod( SizingMethod ) = CapSizingMethod;

				if ( !IsAutoSize && !ZoneSizingRunDone ) { // simulation continue
					if ( CapSizingMethod == HeatingDesignCapacity && ElecRadSys( RadSysNum ).ScaledHeatingCapacity > 0.0 ) {
						TempSize = ElecRadSys( RadSysNum ).ScaledHeatingCapacity;
						RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
						DesCoilLoad = TempSize;
					} else if ( CapSizingMethod == CapacityPerFloorArea ) {
						DataScalableCapSizingON = true;
						TempSize = ElecRadSys( RadSysNum ).ScaledHeatingCapacity * Zone( ElecRadSys( RadSysNum ).ZonePtr ).FloorArea;
						RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
						DesCoilLoad = TempSize;
						DataScalableCapSizingON = false;
						ElecRadSys( RadSysNum ).MaxElecPower = TempSize;
					} else if ( CapSizingMethod == FractionOfAutosizedHeatingCapacity ) {
						ShowSevereError( RoutineName + ": auto-sizing cannot be done for " + CompType + " = " + ElecRadSys( RadSysNum ).Name + "\"." );
						ShowContinueError( "The \"SimulationControl\" object must have the field \"Do Zone Sizing Calculation\" set to Yes when the Heating Design Capacity Method = \"FractionOfAutosizedHeatingCapacity\"." );
						ErrorsFound = true;
					}
				} else {
					if ( CapSizingMethod == HeatingDesignCapacity || CapSizingMethod == CapacityPerFloorArea || CapSizingMethod == FractionOfAutosizedHeatingCapacity ) {
						if ( CapSizingMethod == HeatingDesignCapacity ) {
							if ( ZoneSizingRunDone ) {
								CheckZoneSizing( CompType, CompName );
								SizingMethod = AutoCalculateSizing;
								DataConstantUsedForSizing = CalcFinalZoneSizing( CurZoneEqNum ).DesHeatLoad;
								DataFractionUsedForSizing = CalcFinalZoneSizing( CurZoneEqNum ).HeatSizingFactor;
							}
							if ( ElecRadSys( RadSysNum ).ScaledHeatingCapacity == AutoSize ) {
								TempSize = AutoSize;
							} else {
								TempSize = ElecRadSys( RadSysNum ).ScaledHeatingCapacity;
							}
						} else if ( CapSizingMethod == CapacityPerFloorArea ) {
							if ( ZoneSizingRunDone ) {
								CheckZoneSizing( CompType, CompName );
								ZoneEqSizing( CurZoneEqNum ).HeatingCapacity = true;
								ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = CalcFinalZoneSizing( CurZoneEqNum ).DesHeatLoad * CalcFinalZoneSizing( CurZoneEqNum ).HeatSizingFactor;
							}
							TempSize = ElecRadSys( RadSysNum ).ScaledHeatingCapacity * Zone( ElecRadSys( RadSysNum ).ZonePtr ).FloorArea;
							DataScalableCapSizingON = true;

						} else if ( CapSizingMethod == FractionOfAutosizedHeatingCapacity ) {
							CheckZoneSizing( CompType, CompName );
							ZoneEqSizing( CurZoneEqNum ).HeatingCapacity = true;
							ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = CalcFinalZoneSizing( CurZoneEqNum ).DesHeatLoad * CalcFinalZoneSizing( CurZoneEqNum ).HeatSizingFactor;
							TempSize = ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad * ElecRadSys( RadSysNum ).ScaledHeatingCapacity;
							DataScalableCapSizingON = true;
						} else {
							TempSize = ElecRadSys( RadSysNum ).ScaledHeatingCapacity;
						}
						RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
						ElecRadSys( RadSysNum ).MaxElecPower = TempSize;
						DataConstantUsedForSizing = 0.0;
						DataFractionUsedForSizing = 0.0;
						DataScalableCapSizingON = false;
					}

				}
			}
		}

		if ( SystemType == HydronicSystem ) {

			CompType = "ZoneHVAC:LowTemperatureRadiant:VariableFlow";
			CompName = HydrRadSys( RadSysNum ).Name;

			IsAutoSize = false;
			if ( HydrRadSys( RadSysNum ).ScaledHeatingCapacity == AutoSize ) {
				IsAutoSize = true;
			}

			if ( CurZoneEqNum > 0 ) {

				SizingMethod = HeatingCapacitySizing;
				FieldNum = 3;
				PrintFlag = true;
				SizingString = HydronicRadiantSysNumericFields( RadSysNum ).FieldNames( FieldNum ) + " [W]";
				CapSizingMethod = HydrRadSys( RadSysNum ).HeatingCapMethod;
				ZoneEqSizing( CurZoneEqNum ).SizingMethod( SizingMethod ) = CapSizingMethod;

				if ( !IsAutoSize && !ZoneSizingRunDone ) { // simulation continue
					if ( CapSizingMethod == HeatingDesignCapacity && HydrRadSys( RadSysNum ).ScaledHeatingCapacity > 0.0 ) {
						TempSize = HydrRadSys( RadSysNum ).ScaledHeatingCapacity;
						RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
						DesCoilLoad = TempSize;
					} else if ( CapSizingMethod == CapacityPerFloorArea ) {
						DataScalableCapSizingON = true;
						TempSize = HydrRadSys( RadSysNum ).ScaledHeatingCapacity * Zone( HydrRadSys( RadSysNum ).ZonePtr ).FloorArea;
						RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
						DesCoilLoad = TempSize;
						DataScalableCapSizingON = false;
					} else if ( CapSizingMethod == FractionOfAutosizedHeatingCapacity ) {
						if ( HydrRadSys( RadSysNum ).WaterVolFlowMaxHeat == AutoSize ) {
							ShowSevereError( RoutineName + ": auto-sizing cannot be done for " + CompType + " = " + HydrRadSys( RadSysNum ).Name + "\"." );
							ShowContinueError( "The \"SimulationControl\" object must have the field \"Do Zone Sizing Calculation\" set to Yes when the Heating Design Capacity Method = \"FractionOfAutosizedHeatingCapacity\"." );
							ErrorsFound = true;
						}
					}
				} else { // Autosize or hard-size with sizing run
					if ( CapSizingMethod == HeatingDesignCapacity || CapSizingMethod == CapacityPerFloorArea || CapSizingMethod == FractionOfAutosizedHeatingCapacity ) {
						if ( CapSizingMethod == HeatingDesignCapacity ) {
							if ( ZoneSizingRunDone ) {
								CheckZoneSizing( CompType, CompName );
								SizingMethod = AutoCalculateSizing;
								DataConstantUsedForSizing = CalcFinalZoneSizing( CurZoneEqNum ).DesHeatLoad;
								DataFractionUsedForSizing = CalcFinalZoneSizing( CurZoneEqNum ).HeatSizingFactor;
							}
							if ( HydrRadSys( RadSysNum ).ScaledHeatingCapacity == AutoSize ) {
								TempSize = AutoSize;
							} else {
								TempSize = HydrRadSys( RadSysNum ).ScaledHeatingCapacity;
							}
						} else if ( CapSizingMethod == CapacityPerFloorArea ) {
							if ( ZoneSizingRunDone ) {
								CheckZoneSizing( CompType, CompName );
								ZoneEqSizing( CurZoneEqNum ).HeatingCapacity = true;
								ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = CalcFinalZoneSizing( CurZoneEqNum ).DesHeatLoad * CalcFinalZoneSizing( CurZoneEqNum ).HeatSizingFactor;
							}
							TempSize = HydrRadSys( RadSysNum ).ScaledHeatingCapacity * Zone( HydrRadSys( RadSysNum ).ZonePtr ).FloorArea;
							DataScalableCapSizingON = true;
						} else if ( CapSizingMethod == FractionOfAutosizedHeatingCapacity ) {
							CheckZoneSizing( CompType, CompName );
							ZoneEqSizing( CurZoneEqNum ).HeatingCapacity = true;
							ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = CalcFinalZoneSizing( CurZoneEqNum ).DesHeatLoad * CalcFinalZoneSizing( CurZoneEqNum ).HeatSizingFactor;
							TempSize = ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad * HydrRadSys( RadSysNum ).ScaledHeatingCapacity;
							DataScalableCapSizingON = true;
						} else {
							TempSize = HydrRadSys( RadSysNum ).ScaledHeatingCapacity;
						}
						RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
						DesCoilLoad = TempSize;
						DataConstantUsedForSizing = 0.0;
						DataFractionUsedForSizing = 0.0;
						DataScalableCapSizingON = false;
					} else {
						DesCoilLoad = 0.0;
					}
				}
				// finally heating capacity is saved in this variable
				HydrRadSys( RadSysNum ).ScaledHeatingCapacity = DesCoilLoad;
			}

			IsAutoSize = false;
			if ( HydrRadSys( RadSysNum ).WaterVolFlowMaxHeat == AutoSize ) {
				IsAutoSize = true;
			}

			if ( CurZoneEqNum > 0 ) {
				if ( ! IsAutoSize && ! ZoneSizingRunDone ) { // simulation continue
					if ( HydrRadSys( RadSysNum ).WaterVolFlowMaxHeat > 0.0 ) {
						ReportSizingOutput( CompType, HydrRadSys( RadSysNum ).Name, "User-Specified Maximum Hot Water Flow [m3/s]", HydrRadSys( RadSysNum ).WaterVolFlowMaxHeat );
					}
				} else { // Autosize or hard-size with sizing run
					if ( HydrRadSys( RadSysNum ).HotWaterInNode > 0 && HydrRadSys( RadSysNum ).HotWaterOutNode > 0 ) {
						PltSizHeatNum = MyPlantSizingIndex( CompType, HydrRadSys( RadSysNum ).Name, HydrRadSys( RadSysNum ).HotWaterInNode, HydrRadSys( RadSysNum ).HotWaterOutNode, ErrorsFound );
						if ( PltSizHeatNum > 0 ) {
							if ( DesCoilLoad >= SmallLoad ) {
								rho = GetDensityGlycol( PlantLoop( HydrRadSys( RadSysNum ).HWLoopNum ).FluidName, 60., PlantLoop( HydrRadSys( RadSysNum ).HWLoopNum ).FluidIndex, RoutineName );
								Cp = GetSpecificHeatGlycol( PlantLoop( HydrRadSys( RadSysNum ).HWLoopNum ).FluidName, 60.0, PlantLoop( HydrRadSys( RadSysNum ).HWLoopNum ).FluidIndex, RoutineName );
								WaterVolFlowMaxHeatDes = DesCoilLoad / ( PlantSizData( PltSizHeatNum ).DeltaT * Cp * rho );
							} else {
								WaterVolFlowMaxHeatDes = 0.0;
							}
						} else {
							ShowSevereError( "Autosizing of water flow requires a heating loop Sizing:Plant object" );
							ShowContinueError( "Occurs in ZoneHVAC:LowTemperatureRadiant:VariableFlow Object=" + HydrRadSys( RadSysNum ).Name );
							ErrorsFound = true;
						}
					}

					if ( IsAutoSize ) {
						HydrRadSys( RadSysNum ).WaterVolFlowMaxHeat = WaterVolFlowMaxHeatDes;
						ReportSizingOutput( CompType, HydrRadSys( RadSysNum ).Name, "Design Size Maximum Hot Water Flow [m3/s]", WaterVolFlowMaxHeatDes );
					} else { // hard-size with sizing data
						if ( HydrRadSys( RadSysNum ).WaterVolFlowMaxHeat > 0.0 && WaterVolFlowMaxHeatDes > 0.0 ) {
							WaterVolFlowMaxHeatUser = HydrRadSys( RadSysNum ).WaterVolFlowMaxHeat;
							ReportSizingOutput( CompType, HydrRadSys( RadSysNum ).Name, "Design Size Maximum Hot Water Flow [m3/s]", WaterVolFlowMaxHeatDes, "User-Specified Maximum Hot Water Flow [m3/s]", WaterVolFlowMaxHeatUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( WaterVolFlowMaxHeatDes - WaterVolFlowMaxHeatUser ) / WaterVolFlowMaxHeatUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeLowTempRadiantSystem: Potential issue with equipment sizing for ZoneHVAC:LowTemperatureRadiant:VariableFlow = \"" + HydrRadSys( RadSysNum ).Name + "\"." );
									ShowContinueError( "User-Specified Maximum Hot Water Flow of " + RoundSigDigits( WaterVolFlowMaxHeatUser, 5 ) + " [m3/s]" );
									ShowContinueError( "differs from Design Size Maximum Hot Water Flow of " + RoundSigDigits( WaterVolFlowMaxHeatDes, 5 ) + " [m3/s]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
					}
				}
			}


			IsAutoSize = false;
			if ( HydrRadSys( RadSysNum ).ScaledCoolingCapacity == AutoSize ) {
				IsAutoSize = true;
			}

			if ( CurZoneEqNum > 0 ) {

				SizingMethod = CoolingCapacitySizing;
				FieldNum = 8;
				PrintFlag = true;
				SizingString = HydronicRadiantSysNumericFields( RadSysNum ).FieldNames( FieldNum ) + " [W]";
				CapSizingMethod = HydrRadSys( RadSysNum ).CoolingCapMethod;
				ZoneEqSizing( CurZoneEqNum ).SizingMethod( SizingMethod ) = CapSizingMethod;

				if ( !IsAutoSize && !ZoneSizingRunDone ) { // simulation continue
					if ( CapSizingMethod == CoolingDesignCapacity && HydrRadSys( RadSysNum ).ScaledCoolingCapacity > 0.0 ) {
						TempSize = HydrRadSys( RadSysNum ).ScaledCoolingCapacity;
						RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
						DesCoilLoad = TempSize;
					} else if ( CapSizingMethod == CapacityPerFloorArea ) {
						DataScalableCapSizingON = true;
						TempSize = HydrRadSys( RadSysNum ).ScaledCoolingCapacity * Zone( HydrRadSys( RadSysNum ).ZonePtr ).FloorArea;
						RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
						DesCoilLoad = TempSize;
						DataScalableCapSizingON = false;
					} else if ( CapSizingMethod == FractionOfAutosizedCoolingCapacity ) {
						if ( HydrRadSys( RadSysNum ).WaterVolFlowMaxCool == AutoSize ) {
							ShowSevereError( RoutineName + ": auto-sizing cannot be done for " + CompType + " = " + HydrRadSys( RadSysNum ).Name + "\"." );
							ShowContinueError( "The \"SimulationControl\" object must have the field \"Do Zone Sizing Calculation\" set to Yes when the Cooling Design Capacity Method = \"FractionOfAutosizedCoolingCapacity\"." );
							ErrorsFound = true;
						}
					}
				} else { // Autosize or hard-size with sizing run
					if ( CapSizingMethod == CoolingDesignCapacity || CapSizingMethod == CapacityPerFloorArea || CapSizingMethod == FractionOfAutosizedCoolingCapacity ) {
						if ( CapSizingMethod == CoolingDesignCapacity ) {
							if ( ZoneSizingRunDone ) {
								CheckZoneSizing( CompType, CompName );
								SizingMethod = AutoCalculateSizing;
								DataConstantUsedForSizing = CalcFinalZoneSizing( CurZoneEqNum ).DesCoolLoad;
								DataFractionUsedForSizing = CalcFinalZoneSizing( CurZoneEqNum ).CoolSizingFactor;
							}
							if ( HydrRadSys( RadSysNum ).ScaledCoolingCapacity == AutoSize ) {
								TempSize = AutoSize;
							} else {
								TempSize = HydrRadSys( RadSysNum ).ScaledCoolingCapacity;
							}
						} else if ( CapSizingMethod == CapacityPerFloorArea ) {
							if ( ZoneSizingRunDone ) {
								CheckZoneSizing( CompType, CompName );
								ZoneEqSizing( CurZoneEqNum ).CoolingCapacity = true;
								ZoneEqSizing( CurZoneEqNum ).DesCoolingLoad = CalcFinalZoneSizing( CurZoneEqNum ).DesCoolLoad * CalcFinalZoneSizing( CurZoneEqNum ).CoolSizingFactor;
							}
							TempSize = HydrRadSys( RadSysNum ).ScaledCoolingCapacity * Zone( HydrRadSys( RadSysNum ).ZonePtr ).FloorArea;
							DataScalableCapSizingON = true;
						} else if ( CapSizingMethod == FractionOfAutosizedCoolingCapacity ) {
							CheckZoneSizing( CompType, CompName );
							ZoneEqSizing( CurZoneEqNum ).CoolingCapacity = true;
							ZoneEqSizing( CurZoneEqNum ).DesCoolingLoad = CalcFinalZoneSizing( CurZoneEqNum ).DesCoolLoad * CalcFinalZoneSizing( CurZoneEqNum ).CoolSizingFactor;
							TempSize = ZoneEqSizing( CurZoneEqNum ).DesCoolingLoad * HydrRadSys( RadSysNum ).ScaledCoolingCapacity;
							DataScalableCapSizingON = true;

						} else {
							TempSize = HydrRadSys( RadSysNum ).ScaledCoolingCapacity;
						}
						RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
						DesCoilLoad = TempSize;
						DataConstantUsedForSizing = 0.0;
						DataFractionUsedForSizing = 0.0;
						DataScalableCapSizingON = false;
					} else {
						DesCoilLoad = 0.0;
					}
				}
				// finally cooling capacity is saved in this variable
				HydrRadSys( RadSysNum ).ScaledCoolingCapacity = DesCoilLoad;
			}

			IsAutoSize = false;
			if ( HydrRadSys( RadSysNum ).WaterVolFlowMaxCool == AutoSize ) {
				IsAutoSize = true;
			}
			if ( CurZoneEqNum > 0 ) {
				if ( ! IsAutoSize && ! ZoneSizingRunDone ) { // simulation continue
					if ( HydrRadSys( RadSysNum ).WaterVolFlowMaxCool > 0.0 ) {
						ReportSizingOutput( CompType, HydrRadSys( RadSysNum ).Name, "User-Specified Maximum Cold Water Flow [m3/s]", HydrRadSys( RadSysNum ).WaterVolFlowMaxCool );
					}
				} else { // Autosize or hard-size with sizing run
					if ( HydrRadSys( RadSysNum ).ColdWaterInNode > 0 && HydrRadSys( RadSysNum ).ColdWaterOutNode > 0 ) {
						PltSizCoolNum = MyPlantSizingIndex( CompType, HydrRadSys( RadSysNum ).Name, HydrRadSys( RadSysNum ).ColdWaterInNode, HydrRadSys( RadSysNum ).ColdWaterOutNode, ErrorsFound );
						if ( PltSizCoolNum > 0 ) {
							if ( DesCoilLoad >= SmallLoad ) {
								rho = GetDensityGlycol( PlantLoop( HydrRadSys( RadSysNum ).CWLoopNum ).FluidName, 5., PlantLoop( HydrRadSys( RadSysNum ).CWLoopNum ).FluidIndex, RoutineName );
								Cp = GetSpecificHeatGlycol( PlantLoop( HydrRadSys( RadSysNum ).CWLoopNum ).FluidName, 5.0, PlantLoop( HydrRadSys( RadSysNum ).CWLoopNum ).FluidIndex, RoutineName );
								WaterVolFlowMaxCoolDes = DesCoilLoad / ( PlantSizData( PltSizCoolNum ).DeltaT * Cp * rho );
							} else {
								WaterVolFlowMaxCoolDes = 0.0;
							}
						} else {
							ShowSevereError( "Autosizing of water flow requires a cooling loop Sizing:Plant object" );
							ShowContinueError( "Occurs in ZoneHVAC:LowTemperatureRadiant:VariableFlow Object=" + HydrRadSys( RadSysNum ).Name );
							ErrorsFound = true;
						}
					}

					if ( IsAutoSize ) {
						HydrRadSys( RadSysNum ).WaterVolFlowMaxCool = WaterVolFlowMaxCoolDes;
						ReportSizingOutput( CompType, HydrRadSys( RadSysNum ).Name, "Design Size Maximum Cold Water Flow [m3/s]", WaterVolFlowMaxCoolDes );
					} else { // hard-size with sizing data
						if ( HydrRadSys( RadSysNum ).WaterVolFlowMaxCool > 0.0 && WaterVolFlowMaxCoolDes > 0.0 ) {
							WaterVolFlowMaxCoolUser = HydrRadSys( RadSysNum ).WaterVolFlowMaxCool;
							ReportSizingOutput( CompType, HydrRadSys( RadSysNum ).Name, "Design Size Maximum Cold Water Flow [m3/s]", WaterVolFlowMaxCoolDes, "User-Specified Maximum Cold Water Flow [m3/s]", WaterVolFlowMaxCoolUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( WaterVolFlowMaxCoolDes - WaterVolFlowMaxCoolUser ) / WaterVolFlowMaxCoolUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeLowTempRadiantSystem: Potential issue with equipment sizing for ZoneHVAC:LowTemperatureRadiant:VariableFlow = \"" + HydrRadSys( RadSysNum ).Name + "\"." );
									ShowContinueError( "User-Specified Maximum Cool Water Flow of " + RoundSigDigits( WaterVolFlowMaxCoolUser, 5 ) + " [m3/s]" );
									ShowContinueError( "differs from Design Size Maximum Cool Water Flow of " + RoundSigDigits( WaterVolFlowMaxCoolDes, 5 ) + " [m3/s]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
					}

				}
			}

			IsAutoSize = false;
			if ( HydrRadSys( RadSysNum ).TubeLength == AutoSize ) {
				IsAutoSize = true;
			}
			if ( CurZoneEqNum > 0 ) {
				if ( ! IsAutoSize && ! ZoneSizingRunDone ) { // simulation continue
					if ( HydrRadSys( RadSysNum ).TubeLength > 0.0 ) {
						ReportSizingOutput( CompType, HydrRadSys( RadSysNum ).Name, "User-Specified Hydronic Tubing Length [m]", HydrRadSys( RadSysNum ).TubeLength );
					}
				} else { // Autosize or hard-size with sizing run
					// assume tube spacing of 15 cm
					// CheckZoneSizing is not required here because the tube length calculation is not dependent on zone sizing calculation results
					TubeLengthDes = HydrRadSys( RadSysNum ).TotalSurfaceArea / 0.15;
					if ( IsAutoSize ) {
						HydrRadSys( RadSysNum ).TubeLength = TubeLengthDes;
						ReportSizingOutput( CompType, HydrRadSys( RadSysNum ).Name, "Design Size Hydronic Tubing Length [m]", TubeLengthDes );
					} else { // hard-size with sizing data
						if ( HydrRadSys( RadSysNum ).TubeLength > 0.0 && TubeLengthDes > 0.0 ) {
							TubeLengthUser = HydrRadSys( RadSysNum ).TubeLength;
							ReportSizingOutput( CompType, HydrRadSys( RadSysNum ).Name, "Design Size Hydronic Tubing Length [m]", TubeLengthDes, "User-Specified Hydronic Tubing Length [m]", TubeLengthUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( TubeLengthDes - TubeLengthUser ) / TubeLengthUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeLowTempRadiantSystem: Potential issue with equipment sizing for ZoneHVAC:LowTemperatureRadiant:VariableFlow = \"" + HydrRadSys( RadSysNum ).Name + "\"." );
									ShowContinueError( "User-Specified Hydronic Tubing Length of " + RoundSigDigits( TubeLengthUser, 5 ) + " [m]" );
									ShowContinueError( "differs from Design Size Hydronic Tubing Length of " + RoundSigDigits( TubeLengthDes, 5 ) + " [m]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
					}
				}
			}

			for ( SurfNum = 1; SurfNum <= HydrRadSys( RadSysNum ).NumOfSurfaces; ++SurfNum ) {
				if ( HydrRadSys( RadSysNum ).NumCircCalcMethod == CalculateFromLength ) {
					HydrRadSys( RadSysNum ).NumCircuits( SurfNum ) = ( HydrRadSys( RadSysNum ).SurfaceFlowFrac( SurfNum ) * HydrRadSys( RadSysNum ).TubeLength ) / HydrRadSys( RadSysNum ).CircLength;
					HydrRadSys( RadSysNum ).NumCircuits( SurfNum ) = max( HydrRadSys( RadSysNum ).NumCircuits( SurfNum ), 1.0 );
				} else {
					HydrRadSys( RadSysNum ).NumCircuits( SurfNum ) = 1.0;
				}
			}

			RegisterPlantCompDesignFlow( HydrRadSys( RadSysNum ).HotWaterInNode, HydrRadSys( RadSysNum ).WaterVolFlowMaxHeat );
			RegisterPlantCompDesignFlow( HydrRadSys( RadSysNum ).ColdWaterInNode, HydrRadSys( RadSysNum ).WaterVolFlowMaxCool );

		}

		if ( SystemType == ConstantFlowSystem ) {

			CompType = "ZoneHVAC:LowTemperatureRadiant:ConstantFlow";
			CompName = CFloRadSys( RadSysNum ).Name;

			// Check which operating system it is
			HeatNode = CFloRadSys( RadSysNum ).HotWaterInNode;
			CoolNode = CFloRadSys( RadSysNum ).ColdWaterInNode;
			if ( HeatNode > 0 && CoolNode > 0 ) {
				OpMode = ClgHtg;
			} else if ( HeatNode > 0 && CoolNode <= 0 ) {
				OpMode = HtgOnly;
			} else if ( CoolNode > 0 && HeatNode <= 0 ) {
				OpMode = ClgOnly;
			} else {
				OpMode = OFF; // It shouldn't happen here
			}

			if ( CFloRadSys( RadSysNum ).WaterVolFlowMax == AutoSize ) {
				IsAutoSize = true;
			}

			if ( CurZoneEqNum > 0 ) {
				if ( !IsAutoSize && !ZoneSizingRunDone ) { // simulation continue
					if ( CFloRadSys( RadSysNum ).WaterVolFlowMax > 0.0 ) {
						ReportSizingOutput( CompType, CFloRadSys( RadSysNum ).Name,
							"User-Specified Maximum Water Flow [m3/s]", CFloRadSys( RadSysNum ).WaterVolFlowMax );
					}
				} else { // Autosize or hard-size with sizing run
					CheckZoneSizing( CompType, CFloRadSys( RadSysNum ).Name );
					// Estimate hot water and chilled water flows
					// Index only if it provides heating to avoid severe error
					if ( OpMode == ClgHtg || OpMode == HtgOnly ) {
						PltSizHeatNum = MyPlantSizingIndex( CompType, CFloRadSys( RadSysNum ).Name,
											CFloRadSys( RadSysNum ).HotWaterInNode, CFloRadSys( RadSysNum ).HotWaterOutNode, ErrorsFound );
					}
					if ( PltSizHeatNum > 0 ) {
						if ( ( CalcFinalZoneSizing( CurZoneEqNum ).DesHeatLoad * CalcFinalZoneSizing( CurZoneEqNum ).HeatSizingFactor ) >= SmallLoad ) {
							rho = GetDensityGlycol( PlantLoop( CFloRadSys( RadSysNum ).HWLoopNum ).FluidName,
								60.0, PlantLoop( CFloRadSys( RadSysNum ).HWLoopNum ).FluidIndex, "SizeLowTempRadiantSystem" );
							Cp = GetSpecificHeatGlycol( PlantLoop( CFloRadSys( RadSysNum ).HWLoopNum ).FluidName,
								60.0, PlantLoop( CFloRadSys( RadSysNum ).HWLoopNum ).FluidIndex, "SizeLowTempRadiantSystem" );
							WaterVolFlowMaxHeatDes = CalcFinalZoneSizing( CurZoneEqNum ).DesHeatLoad * CalcFinalZoneSizing( CurZoneEqNum ).HeatSizingFactor /
                                                        ( PlantSizData( PltSizHeatNum ).DeltaT * Cp * rho );
						} else {
							WaterVolFlowMaxHeatDes = 0.0;
						}
					} else {
						if ( OpMode == ClgHtg || OpMode == HtgOnly ) {
							ShowSevereError( "Autosizing of water flow requires a heating loop Sizing:Plant object" );
							ShowContinueError( "Occurs in ZoneHVAC:LowTemperatureRadiant:ConstantFlow \nObject=" + CFloRadSys( RadSysNum ).Name );
							ErrorsFound = true;
						}
					}

					// Index only if it provides cooling system to avoid severe error
					if ( OpMode == ClgHtg || OpMode == ClgOnly ) {
						PltSizCoolNum = MyPlantSizingIndex( CompType, CFloRadSys( RadSysNum ).Name,
							CFloRadSys( RadSysNum ).ColdWaterInNode, CFloRadSys( RadSysNum ).ColdWaterOutNode, ErrorsFound );
					}
					if ( PltSizCoolNum > 0 ) {
						if ( ( CalcFinalZoneSizing( CurZoneEqNum ).DesCoolLoad * CalcFinalZoneSizing( CurZoneEqNum ).CoolSizingFactor ) >= SmallLoad ) {
							rho = GetDensityGlycol( PlantLoop( CFloRadSys( RadSysNum ).CWLoopNum ).FluidName, 5.0,
                             PlantLoop( CFloRadSys( RadSysNum ).CWLoopNum ).FluidIndex, "SizeLowTempRadiantSystem" );
							Cp = GetSpecificHeatGlycol( PlantLoop( CFloRadSys( RadSysNum ).CWLoopNum ).FluidName, 5.0,
								PlantLoop( CFloRadSys( RadSysNum ).CWLoopNum ).FluidIndex, "SizeLowTempRadiantSystem" );
							WaterVolFlowMaxCoolDes = CalcFinalZoneSizing( CurZoneEqNum ).DesCoolLoad * CalcFinalZoneSizing( CurZoneEqNum ).CoolSizingFactor /
								( PlantSizData(PltSizCoolNum).DeltaT * Cp * rho );
						} else {
							WaterVolFlowMaxCoolDes = 0.0;
						}
					} else {
						if ( OpMode == ClgHtg || OpMode == ClgOnly ) {
							ShowSevereError( "Autosizing of water flow requires a cooling loop Sizing:Plant object" );
							ShowContinueError( "Occurs in ZoneHVAC:LowTemperatureRadiant:ConstantFlow \n Object=" + CFloRadSys( RadSysNum ).Name );
							ErrorsFound = true;
						}
					}

					// Determine maximum water flow rate depending upon system type
					if ( OpMode == ClgHtg ) {
						WaterVolFlowMaxDes = std::max( WaterVolFlowMaxHeatDes, WaterVolFlowMaxCoolDes );
					} else if ( OpMode == ClgOnly ) {
						WaterVolFlowMaxDes = WaterVolFlowMaxCoolDes;
					} else if ( OpMode == HtgOnly ) {
						WaterVolFlowMaxDes = WaterVolFlowMaxHeatDes;
					} else {
						WaterVolFlowMaxDes = 0.0;
					}

					if ( IsAutoSize ) {
						CFloRadSys( RadSysNum ).WaterVolFlowMax = WaterVolFlowMaxDes;
						ReportSizingOutput( CompType, CFloRadSys( RadSysNum ).Name,
							"Design Size Maximum Water Flow [m3/s]", WaterVolFlowMaxDes );
					} else { // hard-size with sizing data
						if ( CFloRadSys( RadSysNum ).WaterVolFlowMax > 0.0 && WaterVolFlowMaxDes > 0.0 ) {
							WaterVolFlowMaxUser = CFloRadSys( RadSysNum ).WaterVolFlowMax;
							ReportSizingOutput( CompType, CFloRadSys( RadSysNum ).Name,
								"Design Size Maximum Water Flow [m3/s]", WaterVolFlowMaxDes, "User-Specified Maximum Water Flow [m3/s]", WaterVolFlowMaxUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( WaterVolFlowMaxDes - WaterVolFlowMaxUser ) / WaterVolFlowMaxUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeLowTempRadiantSystem: Potential issue with equipment sizing for \nZoneHVAC:LowTemperatureRadiant:ConstantFlow = \" " +
										CFloRadSys( RadSysNum ).Name + "\"." );
									ShowContinueError( "User-Specified Maximum Water Flow of " + RoundSigDigits( WaterVolFlowMaxUser, 5 ) + " [m3/s]" );
									ShowContinueError( "differs from Design Size Maximum Water Flow of " + RoundSigDigits(WaterVolFlowMaxDes, 5 ) + " [m3/s]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
					}
				}
			}

			IsAutoSize = false;
			if ( CFloRadSys( RadSysNum ).TubeLength == AutoSize ) {
				IsAutoSize = true;
			}

			if ( CurZoneEqNum > 0 ) {
				if ( !IsAutoSize && !ZoneSizingRunDone ) { // simulation continue
					if ( CFloRadSys( RadSysNum ).TubeLength > 0.0 ) {
						ReportSizingOutput( "ZoneHVAC:LowTemperatureRadiant:ConstantFlow", CFloRadSys( RadSysNum ).Name,
							"User-Specified Hydronic Tubing Length [m]", CFloRadSys( RadSysNum ).TubeLength );
					}
				} else {	// Autosize or hard-size with sizing run
					// assume tube spacing of 15 cm
					// CheckZoneSizing is not required here because the tube length calculation is not dependent on zone sizing calculation results
					TubeLengthDes = CFloRadSys( RadSysNum ).TotalSurfaceArea / 0.15;
					if (IsAutoSize ) {
						CFloRadSys( RadSysNum ).TubeLength = TubeLengthDes;
						ReportSizingOutput( "ZoneHVAC:LowTemperatureRadiant:ConstantFlow", CFloRadSys( RadSysNum ).Name,
							"Design Size Hydronic Tubing Length [m]", TubeLengthDes );
					} else { // hard-size with sizing data
						if ( CFloRadSys( RadSysNum ).TubeLength > 0.0 && TubeLengthDes > 0.0 ) {
							TubeLengthUser = CFloRadSys( RadSysNum ).TubeLength;
							ReportSizingOutput( "ZoneHVAC:LowTemperatureRadiant:ConstantFlow", CFloRadSys( RadSysNum ).Name,
								"Design Size Hydronic Tubing Length [m]", TubeLengthDes,
								"User-Specified Hydronic Tubing Length [m]", TubeLengthUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( TubeLengthDes - TubeLengthUser ) / TubeLengthUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeLowTempRadiantSystem: Potential issue with equipment sizing for \nZoneHVAC:LowTemperatureRadiant:ConstantFlow = \" " + CFloRadSys( RadSysNum ).Name + "\"." );
									ShowContinueError( "User-Specified Hydronic Tubing Length of " + RoundSigDigits( TubeLengthUser, 5 ) + " [m]" );
									ShowContinueError( "differs from Design Size Hydronic Tubing Length of " + RoundSigDigits( TubeLengthDes, 5 ) + " [m]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
					}
				}
			}

			for ( SurfNum = 1; SurfNum <= CFloRadSys( RadSysNum ).NumOfSurfaces; ++SurfNum ) {
				if ( CFloRadSys( RadSysNum ).NumCircCalcMethod == CalculateFromLength ) {
					CFloRadSys( RadSysNum ).NumCircuits( SurfNum ) = ( CFloRadSys( RadSysNum ).SurfaceFlowFrac( SurfNum ) * CFloRadSys( RadSysNum ).TubeLength ) / CFloRadSys( RadSysNum ).CircLength;
					CFloRadSys( RadSysNum ).NumCircuits( SurfNum ) = max( CFloRadSys( RadSysNum ).NumCircuits( SurfNum ), 1.0 );
				} else {
					CFloRadSys( RadSysNum ).NumCircuits( SurfNum ) = 1.0;
				}
			}
			if ( CFloRadSys( RadSysNum ).HotWaterInNode > 0 ) {
				RegisterPlantCompDesignFlow( CFloRadSys( RadSysNum ).HotWaterInNode, CFloRadSys( RadSysNum ).WaterVolFlowMax );
			}
			if ( CFloRadSys( RadSysNum ).ColdWaterInNode > 0 ) {
				RegisterPlantCompDesignFlow( CFloRadSys( RadSysNum ).ColdWaterInNode, CFloRadSys( RadSysNum ).WaterVolFlowMax );
			}
		}

		if ( ErrorsFound ) {
			ShowFatalError( "Preceding sizing errors cause program termination" );
		}

	}

	void
	CalcLowTempHydrRadiantSystem(
		int const RadSysNum, // name of the low temperature radiant system
		Real64 & LoadMet // load met by the radiant system, in Watts
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   November 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine does all of the stuff that is necessary to simulate
		// a low temperature hydronic radiant heating/cooling system.  Calls are
		// made to appropriate subroutines either in this module or outside of it.

		// METHODOLOGY EMPLOYED:
		// Follows the methods used by many other pieces of zone equipment.
		// Much like a water coil, a hydronic system will use the ControlCompOutput
		// routine to determine what fraction of capacity the unit should be
		// functioning at by controlling the flow rate of water to the element.

		// REFERENCES:
		// Other EnergyPlus modules
		// IBLAST-QTF research program, completed in January 1995 (unreleased)
		// Strand, R.K. 1995. "Heat Source Transfer Functions and Their Application to
		//   Low Temperature Radiant Heating Systems", Ph.D. dissertation, University
		//   of Illinois at Urbana-Champaign, Department of Mechanical and Industrial
		//   Engineering.
		// Seem, J.E. 1986. "Heat Transfer in Buildings", Ph.D. dissertation, University
		//   of Wisconsin-Madison.

		// Using/Aliasing
		using namespace DataZoneEnergyDemands;
		//  USE DataEnvironment,   ONLY : OutDryBulbTemp, OutWetBulbTemp
		using DataHeatBalance::MRT;
		using DataHeatBalance::Zone;
		using DataHeatBalance::ZoneData;
		using DataHeatBalFanSys::MAT;
		using DataHVACGlobals::SmallLoad;
		using ScheduleManager::GetCurrentScheduleValue;
		using PlantUtilities::SetComponentFlowRate;
		using DataBranchAirLoopPlant::MassFlowTolerance;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 ActWaterFlow; // actual water flow for heating or cooling [kg/sec]
		int ControlNode; // the hot water or cold water inlet node
		Real64 ControlTemp; // temperature of whatever is controlling the radiant system
		Real64 MassFlowFrac; // fraction of the maximum water flow rate as determined by the control algorithm
		Real64 MaxWaterFlow; // maximum water flow for heating or cooling [kg/sec]
		Real64 OffTempCool; // temperature at which the flow rate throttles back to zero for cooling
		Real64 OffTempHeat; // temperature at which the flow rate throttles back to zero for heating
		Real64 SetPointTemp; // temperature "goal" for the radiant system [Celsius]
		int SurfNum; // Surface number in the Surface derived type for a radiant system surface
		int SurfNum2; // Surface number in the Surface derived type for a radiant system surface
		int ZoneNum; // number of zone being served
		Real64 mdot; // local temporary for fluid mass flow rate
		bool SysRunning; // True when system is running

		// FLOW:
		// initialize local variables
		ControlNode = 0;
		MaxWaterFlow = 0.0;
		ActWaterFlow = 0.0;
		ZoneNum = HydrRadSys( RadSysNum ).ZonePtr;
		OperatingMode = NotOperating;
		SysRunning = true;

		if ( GetCurrentScheduleValue( HydrRadSys( RadSysNum ).SchedPtr ) <= 0 ) {

			// Unit is off or has no load upon it; set the flow rates to zero and then
			// simulate the components with the no flow conditions
			for ( SurfNum = 1; SurfNum <= HydrRadSys( RadSysNum ).NumOfSurfaces; ++SurfNum ) {
				SurfNum2 = HydrRadSys( RadSysNum ).SurfacePtr( SurfNum );
				QRadSysSource( SurfNum2 ) = 0.0;
				if ( Surface( SurfNum2 ).ExtBoundCond > 0 && Surface( SurfNum2 ).ExtBoundCond != SurfNum2 ) QRadSysSource( Surface( SurfNum2 ).ExtBoundCond ) = 0.0; // Also zero the other side of an interzone
			}
			if ( HydrRadSys( RadSysNum ).HeatingSystem ) {
				mdot = 0.0;
				SetComponentFlowRate( mdot, HydrRadSys( RadSysNum ).HotWaterInNode, HydrRadSys( RadSysNum ).HotWaterOutNode, HydrRadSys( RadSysNum ).HWLoopNum, HydrRadSys( RadSysNum ).HWLoopSide, HydrRadSys( RadSysNum ).HWBranchNum, HydrRadSys( RadSysNum ).HWCompNum );
			}
			if ( HydrRadSys( RadSysNum ).CoolingSystem ) {
				mdot = 0.0;
				SetComponentFlowRate( mdot, HydrRadSys( RadSysNum ).ColdWaterInNode, HydrRadSys( RadSysNum ).ColdWaterOutNode, HydrRadSys( RadSysNum ).CWLoopNum, HydrRadSys( RadSysNum ).CWLoopSide, HydrRadSys( RadSysNum ).CWBranchNum, HydrRadSys( RadSysNum ).CWCompNum );
			}
		} else { // Unit might be on-->this section is intended to control the water mass flow rate being
			// sent to the radiant system
			{ auto const SELECT_CASE_var( HydrRadSys( RadSysNum ).ControlType );
			if ( SELECT_CASE_var == MATControl ) {
				ControlTemp = MAT( ZoneNum );
			} else if ( SELECT_CASE_var == MRTControl ) {
				ControlTemp = MRT( ZoneNum );
			} else if ( SELECT_CASE_var == OperativeControl ) {
				ControlTemp = 0.5 * ( MAT( ZoneNum ) + MRT( ZoneNum ) );
			} else if ( SELECT_CASE_var == ODBControl ) {
				ControlTemp = Zone( ZoneNum ).OutDryBulbTemp;
			} else if ( SELECT_CASE_var == OWBControl ) {
				ControlTemp = Zone( ZoneNum ).OutWetBulbTemp;
			} else { // Should never get here
				ControlTemp = MAT( ZoneNum );
				ShowSevereError( "Illegal control type in low temperature radiant system: " + HydrRadSys( RadSysNum ).Name );
				ShowFatalError( "Preceding condition causes termination." );
			}}

			if ( HydrRadSys( RadSysNum ).HotSetptSchedPtr > 0 ) {
				SetPointTemp = GetCurrentScheduleValue( HydrRadSys( RadSysNum ).HotSetptSchedPtr );
				OffTempHeat = SetPointTemp + 0.5 * HydrRadSys( RadSysNum ).HotThrottlRange;
			} else { // This system is not capable of heating, set OffTempHeat to something really low
				OffTempHeat = LowTempHeating;
			}
			if ( HydrRadSys( RadSysNum ).ColdSetptSchedPtr > 0 ) {
				SetPointTemp = GetCurrentScheduleValue( HydrRadSys( RadSysNum ).ColdSetptSchedPtr );
				OffTempCool = SetPointTemp - 0.5 * HydrRadSys( RadSysNum ).ColdThrottlRange;
			} else { // This system is not capable of cooling, set OffTempCool to something really high
				OffTempCool = HighTempCooling;
			}

			// Check for an illogical condition where a user enters controls that could
			// potentially be heating or cooling at a particular control temperature
			if ( OffTempHeat > OffTempCool ) {
				MassFlowFrac = 0.0;
				ShowSevereError( "Overlapping heating and cooling control temps in radiant system: " + HydrRadSys( RadSysNum ).Name );
				ShowFatalError( "Preceding condition causes termination." );

			} else { // Temperatures for heating and cooling do not overlap--calculate the mass flow fraction

				if ( ControlTemp < OffTempHeat ) { // Heating mode
					OperatingMode = HeatingMode;
					ControlNode = HydrRadSys( RadSysNum ).HotWaterInNode;
					MaxWaterFlow = HydrRadSys( RadSysNum ).WaterFlowMaxHeat;
					MassFlowFrac = ( OffTempHeat - ControlTemp ) / HydrRadSys( RadSysNum ).HotThrottlRange;
				} else if ( ControlTemp > OffTempCool ) { // Cooling mode
					OperatingMode = CoolingMode;
					ControlNode = HydrRadSys( RadSysNum ).ColdWaterInNode;
					MaxWaterFlow = HydrRadSys( RadSysNum ).WaterFlowMaxCool;
					MassFlowFrac = ( ControlTemp - OffTempCool ) / HydrRadSys( RadSysNum ).ColdThrottlRange;
				} else { // ControlTemp is between OffTempHeat and OffTempCool--unit should not run
					MassFlowFrac = 0.0;
				}

			}

			// Calculate and limit the water flow rate
			ActWaterFlow = MassFlowFrac * MaxWaterFlow;
			if ( ActWaterFlow < MassFlowTolerance ) ActWaterFlow = 0.0;
			if ( HydrRadSys( RadSysNum ).EMSOverrideOnWaterMdot ) ActWaterFlow = HydrRadSys( RadSysNum ).EMSWaterMdotOverrideValue;

			if ( OperatingMode == HeatingMode ) {
				if ( HydrRadSys( RadSysNum ).HeatingSystem ) {
					SetComponentFlowRate( ActWaterFlow, HydrRadSys( RadSysNum ).HotWaterInNode, HydrRadSys( RadSysNum ).HotWaterOutNode, HydrRadSys( RadSysNum ).HWLoopNum, HydrRadSys( RadSysNum ).HWLoopSide, HydrRadSys( RadSysNum ).HWBranchNum, HydrRadSys( RadSysNum ).HWCompNum );
				} else { // not heating system
					SysRunning = false;
				}
			} else if ( OperatingMode == CoolingMode ) {
				if ( HydrRadSys( RadSysNum ).CoolingSystem ) {
					SetComponentFlowRate( ActWaterFlow, HydrRadSys( RadSysNum ).ColdWaterInNode, HydrRadSys( RadSysNum ).ColdWaterOutNode, HydrRadSys( RadSysNum ).CWLoopNum, HydrRadSys( RadSysNum ).CWLoopSide, HydrRadSys( RadSysNum ).CWBranchNum, HydrRadSys( RadSysNum ).CWCompNum );
				} else { // not cooling system
					SysRunning = false;
				}
			}

			// Now simulate the system...
			if ( ( ( OperatingMode == HeatingMode ) || ( OperatingMode == CoolingMode ) ) && SysRunning ) CalcLowTempHydrRadSysComps( RadSysNum, LoadMet );

		}

	}

	void
	CalcLowTempHydrRadSysComps(
		int const RadSysNum, // Index for the low temperature radiant system under consideration
		Real64 & LoadMet // Load met by the low temperature radiant system, in Watts
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   November 2000
		//       MODIFIED       Sep 2011 LKL/BG - resimulate only zones needing it for Radiant systems
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine solves the radiant system based on how much water is (and
		// the conditions of the water) supplied to the radiant system.

		// METHODOLOGY EMPLOYED:
		// Use heat exchanger formulas to obtain the heat source/sink for the radiant
		// system based on the inlet conditions and flow rate of water.  Once that is
		// determined, recalculate the surface heat balances to reflect this heat
		// addition/subtraction.  The load met by the system is determined by the
		// difference between the convection from all surfaces in the zone when
		// there was no radiant system output and with a source/sink added.

		// REFERENCES:
		// IBLAST-QTF research program, completed in January 1995 (unreleased)
		// Strand, R.K. 1995. "Heat Source Transfer Functions and Their Application to
		//   Low Temperature Radiant Heating Systems", Ph.D. dissertation, University
		//   of Illinois at Urbana-Champaign, Department of Mechanical and Industrial
		//   Engineering.

		// Using/Aliasing
		using DataEnvironment::OutBaroPress;
		using General::RoundSigDigits;
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
		using DataHeatBalSurface::TH;
		using DataLoopNode::Node;
		using DataSurfaces::Surface;
		using DataSurfaces::HeatTransferModel_CondFD;
		using DataSurfaces::HeatTransferModel_CTF;
		using PlantUtilities::SetComponentFlowRate;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int CondSurfNum; // Surface number (in radiant array) of
		int ConstrNum; // Index for construction number in Construct derived type
		Real64 DewPointTemp; // Dew-point temperature based on the zone air conditions
		Real64 EpsMdotCp; // Epsilon (heat exchanger terminology) times water mass flow rate times water specific heat
		Real64 FullWaterMassFlow; // Original water mass flow rate before reducing the flow for condensation concerns
		Real64 LowestRadSurfTemp; // Lowest surface temperature of a radiant system (when condensation is a concern)
		Real64 PredictedCondTemp; // Temperature at which condensation is predicted (includes user parameter)
		int RadSurfNum; // DO loop counter for the surfaces that comprise a particular radiant system
		int RadSurfNum2; // DO loop counter for the surfaces that comprise a particular radiant system
		int RadSurfNum3; // DO loop counter for the surfaces that comprise a particular radiant system
		Real64 ReductionFrac; // Fraction that the flow should be reduced to avoid condensation
		int SurfNum; // Index for radiant surface in Surface derived type
		int SurfNum2; // Index for radiant surface in Surface derived type
		Real64 SysWaterMassFlow; // System level water mass flow rate (includes effect of zone multiplier)
		Real64 WaterMassFlow; // Water mass flow rate in the radiant system, kg/s
		int WaterNodeIn; // Node number of the water entering the radiant system
		Real64 WaterTempIn; // Temperature of the water entering the radiant system, in C
		Real64 ZeroFlowSurfTemp; // Temperature of radiant surface when flow is zero
		int ZoneNum; // Zone pointer for this radiant system

		Real64 Ca; // Coefficients to relate the inlet water temperature to the heat source
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
		// For more info on Ca through Cl, see comments below

		// FLOW:
		// First, apply heat exchanger logic to find the heat source/sink to the system.
		// This involves finding out the heat transfer characteristics of the hydronic
		// loop and then applying the equations derived on pp. 113-118 of the dissertation.

		// Set the conditions on the water side inlet
		{ auto const SELECT_CASE_var( OperatingMode );
		if ( SELECT_CASE_var == HeatingMode ) {
			WaterNodeIn = HydrRadSys( RadSysNum ).HotWaterInNode;
		} else if ( SELECT_CASE_var == CoolingMode ) {
			WaterNodeIn = HydrRadSys( RadSysNum ).ColdWaterInNode;
		} else {
			WaterNodeIn = 0; // Suppress uninitialized warning
			ShowSevereError( "Illegal low temperature radiant system operating mode" );
			ShowContinueError( "Occurs in Radiant System=" + HydrRadSys( RadSysNum ).Name );
			ShowFatalError( "Preceding condition causes termination." );
		}}
		ZoneNum = HydrRadSys( RadSysNum ).ZonePtr;
		SysWaterMassFlow = Node( WaterNodeIn ).MassFlowRate;
		WaterMassFlow = Node( WaterNodeIn ).MassFlowRate / double( Zone( ZoneNum ).Multiplier * Zone( ZoneNum ).ListMultiplier );
		WaterTempIn = Node( WaterNodeIn ).Temp;

		if ( WaterMassFlow <= 0.0 ) {
			// No flow or below minimum allowed so there is no heat source/sink
			// This is possible with a mismatch between system and plant operation
			// or a slight mismatch between zone and system controls.  This is not
			// necessarily a "problem" so this exception is necessary in the code.
			for ( RadSurfNum = 1; RadSurfNum <= HydrRadSys( RadSysNum ).NumOfSurfaces; ++RadSurfNum ) {
				SurfNum = HydrRadSys( RadSysNum ).SurfacePtr( RadSurfNum );
				QRadSysSource( SurfNum ) = 0.0;
				if ( Surface( SurfNum ).ExtBoundCond > 0 && Surface( SurfNum ).ExtBoundCond != SurfNum ) QRadSysSource( Surface( SurfNum ).ExtBoundCond ) = 0.0; // Also zero the other side of an interzone
			}

		} else {

			for ( RadSurfNum = 1; RadSurfNum <= HydrRadSys( RadSysNum ).NumOfSurfaces; ++RadSurfNum ) {

				SurfNum = HydrRadSys( RadSysNum ).SurfacePtr( RadSurfNum );
				// Determine the heat exchanger "effectiveness" term
				EpsMdotCp = CalcRadSysHXEffectTerm( RadSysNum, HydronicSystem, WaterTempIn, WaterMassFlow, HydrRadSys( RadSysNum ).SurfaceFlowFrac( RadSurfNum ), HydrRadSys( RadSysNum ).NumCircuits( RadSurfNum ), HydrRadSys( RadSysNum ).TubeLength, HydrRadSys( RadSysNum ).TubeDiameter, HydrRadSys( RadSysNum ).GlycolIndex );

				// Obtain the heat balance coefficients and calculate the intermediate coefficients
				// linking the inlet water temperature to the heat source/sink to the radiant system.
				// The coefficients are based on the following development...
				// The heat balance equations at the outside and inside surfaces are of the form:
				//   Tinside  = Ca + Cb*Toutside + Cc*q"
				//   Toutside = Cd + Ce*Tinside  + Cf*q"
				//   Tsource  = Cg + Ch*q"       + Ci*Tinside + Cj*Toutside
				// where:
				//   Tinside is the temperature at the inside surface
				//   Toutside is the temperature at the outside surface
				//   Tsource is the temperature within the radiant system at the location of the source/sink
				//   Ca is all of the other terms in the inside heat balance (solar, LW exchange, conduction history terms, etc.)
				//   Cb is the current cross CTF term
				//   Cc is the QTF inside term for the current heat source/sink
				//   Cd is all of the other terms in the outside heat balance (solar, LW exchange, conduction history terms, etc.)
				//   Ce is the current cross CTF term (should be equal to Cb)
				//   Cf is the QTF outside term for the current heat source/sink
				//   Cg is the summation of all temperature and source history terms at the source/sink location
				//   Ch is the QTF term at the source/sink location for the current heat source/sink
				//   Ci is the CTF inside term for the current inside surface temperature
				//   Cj is the CTF outside term for the current outside surface temperature
				// Note that it is necessary to not use "slow conduction" assumptions because the
				// source/sink has an impact on BOTH the inside and outside surface heat balances.
				// Hence the more general formulation.
				// The first two T equations above can be solved to remove the other surface temperature.
				// This results in the following equations:
				//   Tinside  = Ca + Cb*(Cd + Ce*Tinside + Cf*q") + Cc*q"   or...
				//   Tinside  = (Ca + Cb*Cd + (Cc+Cb*Cf)*q") / (1 - Ce*Cb)
				//   Toutside = Cd + Ce*(Ca + Cb*Toutside + Cc*q") + Cf*q"  or...
				//   Toutside = (Cd + Ce*Ca + (Cf+Ce*Cc)*q") / (1 - Ce*Cb)
				// Substituting the new equations for Tinside and Toutside as a function of C and q"
				// into the equation for Tsource...
				//   Tsource  = Cg + Ch*q" + Ci*((Ca + Cb*Cd + (Cc+Cb*Cf)*q") / (1 - Ce*Cb)) &
				//                         + Cj*((Cd + Ce*Ca + (Cf+Ce*Cc)*q") / (1 - Ce*Cb))
				// Or rearranging this to get Tsource as a function of q", we get...
				//   Tsource  =  Cg + ((Ci*(Ca + Cb*Cd) + Cj*(Cd + Ce*Ca))/(1-Ce*Cb)) &
				//             +(Ch + ((Ci*(Cc + Cb*Cf) + Cj*(Cf + Ce*Cc))/(1-Ce*Cb)))*q"
				// Or in a slightly simpler form...
				//   Tsource  = Ck + Cl*q"
				// where:
				//   Ck = Cg + ((Ci*(Ca + Cb*Cd) + Cj*(Cd + Ce*Ca))/(1-Ce*Cb))
				//   Cl = Ch + ((Ci*(Cc + Cb*Cf) + Cj*(Cf + Ce*Cc))/(1-Ce*Cb))
				// Note also that from heat exchanger "algebra", we have:
				//   q = epsilon*qmax    and    qmax = Mdot*Cp*(Twaterin-Tsource)
				// So...
				//   q" = q/Area = (epsilon*Mdot*Cp/Area)*(Twaterin-Tsource)
				// Or rearranging this equation:
				//   Tsource = -(q"*A/(epsilon*Mdot*Cp)) + Twaterin
				// Setting this equation equal to the other equation for Tsource a couple lines up
				// and rearranging to solve for q"...
				//   q" = (Twaterin - Ck) / (Cl + (A/(epsilon*Mdot*Cp))
				// or
				//   q  = (Twaterin - Ck) / ((Cl/A) + (1/epsilon*Mdot*Cp))
				// or
				//   q  = epsilon*Mdot*Cp*(Twaterin - Ck) / (1+(epsilon*Mdot*Cp*Cl/A))
				// which is the desired result, that is the heat source or sink to the radiant
				// system as a function of the water inlet temperature (flow rate is also in there
				// as well as all of the heat balance terms "hidden" in Ck and Cl).
				ConstrNum = Surface( SurfNum ).Construction;

				if ( Surface( SurfNum ).HeatTransferAlgorithm == HeatTransferModel_CTF ) {

					Ca = RadSysTiHBConstCoef( SurfNum );
					Cb = RadSysTiHBToutCoef( SurfNum );
					Cc = RadSysTiHBQsrcCoef( SurfNum );

					Cd = RadSysToHBConstCoef( SurfNum );
					Ce = RadSysToHBTinCoef( SurfNum );
					Cf = RadSysToHBQsrcCoef( SurfNum );

					Cg = CTFTsrcConstPart( SurfNum );
					Ch = Construct( ConstrNum ).CTFTSourceQ( 0 );
					Ci = Construct( ConstrNum ).CTFTSourceIn( 0 );
					Cj = Construct( ConstrNum ).CTFTSourceOut( 0 );

					Ck = Cg + ( ( Ci * ( Ca + Cb * Cd ) + Cj * ( Cd + Ce * Ca ) ) / ( 1.0 - Ce * Cb ) );
					Cl = Ch + ( ( Ci * ( Cc + Cb * Cf ) + Cj * ( Cf + Ce * Cc ) ) / ( 1.0 - Ce * Cb ) );

					QRadSysSource( SurfNum ) = EpsMdotCp * ( WaterTempIn - Ck ) / ( 1.0 + ( EpsMdotCp * Cl / Surface( SurfNum ).Area ) );

				} else if ( Surface( SurfNum ).HeatTransferAlgorithm == HeatTransferModel_CondFD ) {

					QRadSysSource( SurfNum ) = EpsMdotCp * ( WaterTempIn - TCondFDSourceNode( SurfNum ) );

				}

				if ( Surface( SurfNum ).ExtBoundCond > 0 && Surface( SurfNum ).ExtBoundCond != SurfNum ) QRadSysSource( Surface( SurfNum ).ExtBoundCond ) = QRadSysSource( SurfNum ); // Also set the other side of an interzone

			}

			// "Temperature Comparison" Cut-off:
			for ( RadSurfNum = 1; RadSurfNum <= HydrRadSys( RadSysNum ).NumOfSurfaces; ++RadSurfNum ) {
				// Check to see whether or not the system should really be running.  If
				// QRadSysSource is negative when we are in heating mode or QRadSysSource
				// is positive when we are in cooling mode, then the radiant system will
				// be doing the opposite of its intention.  In this case, the flow rate
				// is set to zero to avoid heating in cooling mode or cooling in heating
				// mode.
				SurfNum = HydrRadSys( RadSysNum ).SurfacePtr( RadSurfNum );

				if ( ( ( OperatingMode == HeatingMode ) && ( QRadSysSource( SurfNum ) <= 0.0 ) ) || ( ( OperatingMode == CoolingMode ) && ( QRadSysSource( SurfNum ) >= 0.0 ) ) ) {
					WaterMassFlow = 0.0;
					if ( OperatingMode == HeatingMode ) {
						SetComponentFlowRate( WaterMassFlow, HydrRadSys( RadSysNum ).HotWaterInNode, HydrRadSys( RadSysNum ).HotWaterOutNode, HydrRadSys( RadSysNum ).HWLoopNum, HydrRadSys( RadSysNum ).HWLoopSide, HydrRadSys( RadSysNum ).HWBranchNum, HydrRadSys( RadSysNum ).HWCompNum );

					} else if ( OperatingMode == CoolingMode ) {
						SetComponentFlowRate( WaterMassFlow, HydrRadSys( RadSysNum ).ColdWaterInNode, HydrRadSys( RadSysNum ).ColdWaterOutNode, HydrRadSys( RadSysNum ).CWLoopNum, HydrRadSys( RadSysNum ).CWLoopSide, HydrRadSys( RadSysNum ).CWBranchNum, HydrRadSys( RadSysNum ).CWCompNum );
					}
					HydrRadSys( RadSysNum ).WaterMassFlowRate = WaterMassFlow;

					for ( RadSurfNum2 = 1; RadSurfNum2 <= HydrRadSys( RadSysNum ).NumOfSurfaces; ++RadSurfNum2 ) {
						SurfNum2 = HydrRadSys( RadSysNum ).SurfacePtr( RadSurfNum2 );
						QRadSysSource( SurfNum2 ) = 0.0;
						if ( Surface( SurfNum2 ).ExtBoundCond > 0 && Surface( SurfNum2 ).ExtBoundCond != SurfNum2 ) QRadSysSource( Surface( SurfNum2 ).ExtBoundCond ) = 0.0; // Also zero the other side of an interzone
					}
					break; // outer do loop
				}
			}

			// Condensation Cut-off:
			// Check to see whether there are any surface temperatures within the radiant system that have
			// dropped below the dew-point temperature.  If so, we need to shut off this radiant system.
			// A safety parameter is added (hardwired parameter) to avoid getting too close to condensation
			// conditions.
			HydrRadSys( RadSysNum ).CondCausedShutDown = false;
			DewPointTemp = PsyTdpFnWPb( ZoneAirHumRat( ZoneNum ), OutBaroPress );

			if ( ( OperatingMode == CoolingMode ) && ( HydrRadSys( RadSysNum ).CondCtrlType == CondCtrlSimpleOff ) ) {

				for ( RadSurfNum2 = 1; RadSurfNum2 <= HydrRadSys( RadSysNum ).NumOfSurfaces; ++RadSurfNum2 ) {
					if ( TH( 2, 1, HydrRadSys( RadSysNum ).SurfacePtr( RadSurfNum2 ) ) < ( DewPointTemp + HydrRadSys( RadSysNum ).CondDewPtDeltaT ) ) {
						// Condensation warning--must shut off radiant system
						HydrRadSys( RadSysNum ).CondCausedShutDown = true;
						WaterMassFlow = 0.0;
						SetComponentFlowRate( WaterMassFlow, HydrRadSys( RadSysNum ).ColdWaterInNode, HydrRadSys( RadSysNum ).ColdWaterOutNode, HydrRadSys( RadSysNum ).CWLoopNum, HydrRadSys( RadSysNum ).CWLoopSide, HydrRadSys( RadSysNum ).CWBranchNum, HydrRadSys( RadSysNum ).CWCompNum );
						HydrRadSys( RadSysNum ).WaterMassFlowRate = WaterMassFlow;
						for ( RadSurfNum3 = 1; RadSurfNum3 <= HydrRadSys( RadSysNum ).NumOfSurfaces; ++RadSurfNum3 ) {
							SurfNum2 = HydrRadSys( RadSysNum ).SurfacePtr( RadSurfNum3 );
							QRadSysSource( SurfNum2 ) = 0.0;
							if ( Surface( SurfNum2 ).ExtBoundCond > 0 && Surface( SurfNum2 ).ExtBoundCond != SurfNum2 ) QRadSysSource( Surface( SurfNum2 ).ExtBoundCond ) = 0.0; // Also zero the other side of an interzone
						}
						// Produce a warning message so that user knows the system was shut-off due to potential for condensation
						if ( ! WarmupFlag ) {
							if ( HydrRadSys( RadSysNum ).CondErrIndex == 0 ) { // allow errors up to number of radiant systems
								ShowWarningMessage( cHydronicSystem + " [" + HydrRadSys( RadSysNum ).Name + ']' );
								ShowContinueError( "Surface [" + Surface( HydrRadSys( RadSysNum ).SurfacePtr( RadSurfNum2 ) ).Name + "] temperature below dew-point temperature--potential for condensation exists" );
								ShowContinueError( "Flow to the radiant system will be shut-off to avoid condensation" );
								ShowContinueError( "Predicted radiant system surface temperature = " + RoundSigDigits( TH( 2, 1, HydrRadSys( RadSysNum ).SurfacePtr( RadSurfNum2 ) ), 2 ) );
								ShowContinueError( "Zone dew-point temperature + safety delta T= " + RoundSigDigits( DewPointTemp + HydrRadSys( RadSysNum ).CondDewPtDeltaT, 2 ) );
								ShowContinueErrorTimeStamp( "" );
								ShowContinueError( "Note that a " + RoundSigDigits( HydrRadSys( RadSysNum ).CondDewPtDeltaT, 4 ) + " C safety was chosen in the input for the shut-off criteria" );
								ShowContinueError( "Note also that this affects all surfaces that are part of this radiant system" );
							}
							ShowRecurringWarningErrorAtEnd( cHydronicSystem + " [" + HydrRadSys( RadSysNum ).Name + "] condensation shut-off occurrence continues.", HydrRadSys( RadSysNum ).CondErrIndex, DewPointTemp, DewPointTemp, _, "C", "C" );
						}
						break; // outer do loop
					}
				}

			} else if ( ( OperatingMode == CoolingMode ) && ( HydrRadSys( RadSysNum ).CondCtrlType == CondCtrlNone ) ) {

				for ( RadSurfNum2 = 1; RadSurfNum2 <= HydrRadSys( RadSysNum ).NumOfSurfaces; ++RadSurfNum2 ) {
					if ( TH( 2, 1, HydrRadSys( RadSysNum ).SurfacePtr( RadSurfNum2 ) ) < DewPointTemp ) {
						// Condensation occurring but user does not want to shut radiant system off ever
						HydrRadSys( RadSysNum ).CondCausedShutDown = true;
					}
				}

			} else if ( ( OperatingMode == CoolingMode ) && ( HydrRadSys( RadSysNum ).CondCtrlType == CondCtrlVariedOff ) ) {

				LowestRadSurfTemp = 999.9;
				CondSurfNum = 0;
				for ( RadSurfNum2 = 1; RadSurfNum2 <= HydrRadSys( RadSysNum ).NumOfSurfaces; ++RadSurfNum2 ) {
					if ( TH( 2, 1, HydrRadSys( RadSysNum ).SurfacePtr( RadSurfNum2 ) ) < ( DewPointTemp + HydrRadSys( RadSysNum ).CondDewPtDeltaT ) ) {
						if ( TH( 2, 1, HydrRadSys( RadSysNum ).SurfacePtr( RadSurfNum2 ) ) < LowestRadSurfTemp ) {
							LowestRadSurfTemp = TH( 2, 1, HydrRadSys( RadSysNum ).SurfacePtr( RadSurfNum2 ) );
							CondSurfNum = RadSurfNum2;
						}
					}
				}

				if ( CondSurfNum > 0 ) { // Condensation predicted so let's deal with it
					// Process here is: turn everything off and see what the resulting surface temperature is for
					// the surface that was causing the lowest temperature.  Then, interpolate to find the flow that
					// would still allow the system to operate without producing condensation.  Rerun the heat balance
					// and recheck for condensation.  If condensation still exists, shut everything down.  This avoids
					// excessive iteration and still makes an attempt to vary the flow rate.
					// First, shut everything off...
					FullWaterMassFlow = WaterMassFlow;
					WaterMassFlow = 0.0;
					SetComponentFlowRate( WaterMassFlow, HydrRadSys( RadSysNum ).ColdWaterInNode, HydrRadSys( RadSysNum ).ColdWaterOutNode, HydrRadSys( RadSysNum ).CWLoopNum, HydrRadSys( RadSysNum ).CWLoopSide, HydrRadSys( RadSysNum ).CWBranchNum, HydrRadSys( RadSysNum ).CWCompNum );
					HydrRadSys( RadSysNum ).WaterMassFlowRate = WaterMassFlow;
					for ( RadSurfNum3 = 1; RadSurfNum3 <= HydrRadSys( RadSysNum ).NumOfSurfaces; ++RadSurfNum3 ) {
						SurfNum2 = HydrRadSys( RadSysNum ).SurfacePtr( RadSurfNum3 );
						QRadSysSource( SurfNum2 ) = 0.0;
						if ( Surface( SurfNum2 ).ExtBoundCond > 0 && Surface( SurfNum2 ).ExtBoundCond != SurfNum2 ) QRadSysSource( Surface( SurfNum2 ).ExtBoundCond ) = 0.0; // Also zero the other side of an interzone
					}
					// Redo the heat balances since we have changed the heat source (set it to zero)
					HeatBalanceSurfaceManager::CalcHeatBalanceOutsideSurf( ZoneNum );
					HeatBalanceSurfaceManager::CalcHeatBalanceInsideSurf( ZoneNum );
					// Now check all of the surface temperatures.  If any potentially have condensation, leave the system off.
					for ( RadSurfNum2 = 1; RadSurfNum2 <= HydrRadSys( RadSysNum ).NumOfSurfaces; ++RadSurfNum2 ) {
						if ( TH( 2, 1, HydrRadSys( RadSysNum ).SurfacePtr( RadSurfNum2 ) ) < ( DewPointTemp + HydrRadSys( RadSysNum ).CondDewPtDeltaT ) ) {
							HydrRadSys( RadSysNum ).CondCausedShutDown = true;
						}
					}
					// If the system does not need to be shut down, then let's see if we can vary the flow based
					// on the lowest temperature surface from before.  This will use interpolation to try a new
					// flow rate.
					if ( ! HydrRadSys( RadSysNum ).CondCausedShutDown ) {
						PredictedCondTemp = DewPointTemp + HydrRadSys( RadSysNum ).CondDewPtDeltaT;
						ZeroFlowSurfTemp = TH( 2, 1, HydrRadSys( RadSysNum ).SurfacePtr( CondSurfNum ) );
						ReductionFrac = ( ZeroFlowSurfTemp - PredictedCondTemp ) / std::abs( ZeroFlowSurfTemp - LowestRadSurfTemp );
						if ( ReductionFrac < 0.0 ) ReductionFrac = 0.0; // Shouldn't happen as the above check should have screened this out
						if ( ReductionFrac > 1.0 ) ReductionFrac = 1.0; // Shouldn't happen either because condensation doesn't exist then
						WaterMassFlow = ReductionFrac * FullWaterMassFlow;
						SysWaterMassFlow = double( Zone( ZoneNum ).Multiplier * Zone( ZoneNum ).ListMultiplier ) * WaterMassFlow;
						// Got a new reduced flow rate that should work...reset loop variable and resimulate the system
						SetComponentFlowRate( SysWaterMassFlow, HydrRadSys( RadSysNum ).ColdWaterInNode, HydrRadSys( RadSysNum ).ColdWaterOutNode, HydrRadSys( RadSysNum ).CWLoopNum, HydrRadSys( RadSysNum ).CWLoopSide, HydrRadSys( RadSysNum ).CWBranchNum, HydrRadSys( RadSysNum ).CWCompNum );
						HydrRadSys( RadSysNum ).WaterMassFlowRate = SysWaterMassFlow;

						// Go through all of the surfaces again with the new flow rate...
						for ( RadSurfNum3 = 1; RadSurfNum3 <= HydrRadSys( RadSysNum ).NumOfSurfaces; ++RadSurfNum3 ) {
							SurfNum = HydrRadSys( RadSysNum ).SurfacePtr( RadSurfNum3 );
							// Determine the heat exchanger "effectiveness" term
							EpsMdotCp = CalcRadSysHXEffectTerm( RadSysNum, HydronicSystem, WaterTempIn, WaterMassFlow, HydrRadSys( RadSysNum ).SurfaceFlowFrac( RadSurfNum3 ), HydrRadSys( RadSysNum ).NumCircuits( RadSurfNum3 ), HydrRadSys( RadSysNum ).TubeLength, HydrRadSys( RadSysNum ).TubeDiameter, HydrRadSys( RadSysNum ).GlycolIndex );
							if ( Surface( SurfNum ).HeatTransferAlgorithm == HeatTransferModel_CTF ) {
								// For documentation on coefficients, see code earlier in this subroutine
								Ca = RadSysTiHBConstCoef( SurfNum );
								Cb = RadSysTiHBToutCoef( SurfNum );
								Cc = RadSysTiHBQsrcCoef( SurfNum );
								Cd = RadSysToHBConstCoef( SurfNum );
								Ce = RadSysToHBTinCoef( SurfNum );
								Cf = RadSysToHBQsrcCoef( SurfNum );
								Cg = CTFTsrcConstPart( SurfNum );
								Ch = Construct( ConstrNum ).CTFTSourceQ( 0 );
								Ci = Construct( ConstrNum ).CTFTSourceIn( 0 );
								Cj = Construct( ConstrNum ).CTFTSourceOut( 0 );
								Ck = Cg + ( ( Ci * ( Ca + Cb * Cd ) + Cj * ( Cd + Ce * Ca ) ) / ( 1.0 - Ce * Cb ) );
								Cl = Ch + ( ( Ci * ( Cc + Cb * Cf ) + Cj * ( Cf + Ce * Cc ) ) / ( 1.0 - Ce * Cb ) );
								QRadSysSource( SurfNum ) = EpsMdotCp * ( WaterTempIn - Ck ) / ( 1.0 + ( EpsMdotCp * Cl / Surface( SurfNum ).Area ) );
							} else if ( Surface( SurfNum ).HeatTransferAlgorithm == HeatTransferModel_CondFD ) {
								QRadSysSource( SurfNum ) = EpsMdotCp * ( WaterTempIn - TCondFDSourceNode( SurfNum ) );
							}
							if ( Surface( SurfNum ).ExtBoundCond > 0 && Surface( SurfNum ).ExtBoundCond != SurfNum ) QRadSysSource( Surface( SurfNum ).ExtBoundCond ) = QRadSysSource( SurfNum ); // Also set the other side of an interzone
						}

						// Redo the heat balances since we have changed the heat source
						HeatBalanceSurfaceManager::CalcHeatBalanceOutsideSurf( ZoneNum );
						HeatBalanceSurfaceManager::CalcHeatBalanceInsideSurf( ZoneNum );

						// Check for condensation one more time.  If no condensation, we are done.  If there is
						// condensation, shut things down and be done.
						for ( RadSurfNum2 = 1; RadSurfNum2 <= HydrRadSys( RadSysNum ).NumOfSurfaces; ++RadSurfNum2 ) {
							if ( HydrRadSys( RadSysNum ).CondCausedShutDown ) break;
							if ( TH( 2, 1, HydrRadSys( RadSysNum ).SurfacePtr( RadSurfNum2 ) ) < ( PredictedCondTemp ) ) {
								// Condensation still present--must shut off radiant system
								HydrRadSys( RadSysNum ).CondCausedShutDown = true;
								WaterMassFlow = 0.0;
								RadSurfNum = RadSurfNum2;
								SetComponentFlowRate( WaterMassFlow, HydrRadSys( RadSysNum ).ColdWaterInNode, HydrRadSys( RadSysNum ).ColdWaterOutNode, HydrRadSys( RadSysNum ).CWLoopNum, HydrRadSys( RadSysNum ).CWLoopSide, HydrRadSys( RadSysNum ).CWBranchNum, HydrRadSys( RadSysNum ).CWCompNum );
								HydrRadSys( RadSysNum ).WaterMassFlowRate = WaterMassFlow;
								for ( RadSurfNum3 = 1; RadSurfNum3 <= HydrRadSys( RadSysNum ).NumOfSurfaces; ++RadSurfNum3 ) {
									SurfNum2 = HydrRadSys( RadSysNum ).SurfacePtr( RadSurfNum3 );
									QRadSysSource( SurfNum2 ) = 0.0;
									if ( Surface( SurfNum2 ).ExtBoundCond > 0 && Surface( SurfNum2 ).ExtBoundCond != SurfNum2 ) QRadSysSource( Surface( SurfNum2 ).ExtBoundCond ) = 0.0; // Also zero the other side of an interzone
								}
							}
						}

					}

					if ( HydrRadSys( RadSysNum ).CondCausedShutDown ) {
						// Produce a warning message so that user knows the system was shut-off due to potential for condensation
						if ( ! WarmupFlag ) {
							if ( HydrRadSys( RadSysNum ).CondErrIndex == 0 ) { // allow errors up to number of radiant systems
								ShowWarningMessage( cHydronicSystem + " [" + HydrRadSys( RadSysNum ).Name + ']' );
								ShowContinueError( "Surface [" + Surface( HydrRadSys( RadSysNum ).SurfacePtr( CondSurfNum ) ).Name + "] temperature below dew-point temperature--potential for condensation exists" );
								ShowContinueError( "Flow to the radiant system will be shut-off to avoid condensation" );
								ShowContinueError( "Predicted radiant system surface temperature = " + RoundSigDigits( TH( 2, 1, HydrRadSys( RadSysNum ).SurfacePtr( CondSurfNum ) ), 2 ) );
								ShowContinueError( "Zone dew-point temperature + safety delta T= " + RoundSigDigits( DewPointTemp + HydrRadSys( RadSysNum ).CondDewPtDeltaT, 2 ) );
								ShowContinueErrorTimeStamp( "" );
								ShowContinueError( "Note that a " + RoundSigDigits( HydrRadSys( RadSysNum ).CondDewPtDeltaT, 4 ) + " C safety was chosen in the input for the shut-off criteria" );
								ShowContinueError( "Note also that this affects all surfaces that are part of this radiant system" );
							}
							ShowRecurringWarningErrorAtEnd( cHydronicSystem + " [" + HydrRadSys( RadSysNum ).Name + "] condensation shut-off occurrence continues.", HydrRadSys( RadSysNum ).CondErrIndex, DewPointTemp, DewPointTemp, _, "C", "C" );
						}
					}
				} // Condensation Predicted in Variable Shut-Off Control Type
			} // In cooling mode and one of the condensation control types
		} // There was a non-zero flow

		// Now that we have the source/sink term, we must redo the heat balances to obtain
		// the new SumHATsurf value for the zone.  Note that the difference between the new
		// SumHATsurf and the value originally calculated by the heat balance with a zero
		// source for all radiant systems in the zone is the load met by the system (approximately).
		HeatBalanceSurfaceManager::CalcHeatBalanceOutsideSurf( ZoneNum );
		HeatBalanceSurfaceManager::CalcHeatBalanceInsideSurf( ZoneNum );

		LoadMet = SumHATsurf( ZoneNum ) - ZeroSourceSumHATsurf( ZoneNum );

	}

	void
	CalcLowTempCFloRadiantSystem(
		int const RadSysNum, // name of the low temperature radiant system
		Real64 & LoadMet // load met by the radiant system, in Watts
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   August 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine does all of the stuff that is necessary to simulate
		// a constant flow low temperature hydronic radiant heating/cooling system.
		// Calls are made to appropriate subroutines either in this module or
		// outside of it.

		// METHODOLOGY EMPLOYED:
		// Similar in many aspects to the hydronic (variable flow) radiant system
		// except that flow rate through the radiant system is constant (based on
		// the user schedule) and the inlet temperature is varied by injecting
		// more or less fluid from the main loop to achieve the desired inlet
		// temperature.

		// REFERENCES:
		// Other EnergyPlus modules
		// IBLAST-QTF research program, completed in January 1995 (unreleased)
		// Strand, R.K. 1995. "Heat Source Transfer Functions and Their Application to
		//   Low Temperature Radiant Heating Systems", Ph.D. dissertation, University
		//   of Illinois at Urbana-Champaign, Department of Mechanical and Industrial
		//   Engineering.
		// Seem, J.E. 1986. "Heat Transfer in Buildings", Ph.D. dissertation, University
		//   of Wisconsin-Madison.

		// Using/Aliasing
		using namespace DataZoneEnergyDemands;
		using DataEnvironment::EnvironmentName;
		using DataEnvironment::CurMnDy;
		using DataHeatBalance::MRT;
		using DataHeatBalance::Zone;
		using DataHeatBalance::ZoneData;
		using DataHeatBalFanSys::MAT;
		using DataHVACGlobals::SmallLoad;
		using DataBranchAirLoopPlant::MassFlowTolerance;
		using DataLoopNode::Node;
		using FluidProperties::GetSpecificHeatGlycol;
		using ScheduleManager::GetCurrentScheduleValue;
		using General::TrimSigDigits;
		using PlantUtilities::SetComponentFlowRate;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const LowCpFluidValue( 100.0 ); // lowest allowed Cp fluid value (to avoid dividing by zero) [J/kg-K]
		static std::string const RoutineName( "CalcLowTempCFloRadiantSystem" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 CpFluid; // Specific heat of the fluid in the radiant system
		Real64 InjectFlowRate; // Calculated injection flow rate that will meet the inlet temperature requirement
		bool Iteration; // FALSE when a normal solution, TRUE when it is a solution where we must also find the inlet temp
		int LoopInNode; // Node on the loop that is the inlet to the constant flow radiant system
		Real64 OffTempCool; // temperature at which the cooling shuts down
		Real64 OffTempHeat; // temperature at which the heating shuts down
		Real64 PumpPartLoadRat; // Pump part load ratio (based on user schedule, or 1.0 for no schedule)
		Real64 PumpTempRise; // Temperature rise of the fluid as it passes through the pump
		Real64 RadInTemp; // "Desired" radiant system water inlet temperature [Celsius]
		Real64 SetPointTemp; // temperature that will be used to control the radiant system [Celsius]
		Real64 SetPointTempHi; // Current high point in setpoint temperature range
		Real64 SetPointTempLo; // Current low point in setpoint temperature range
		Real64 ShaftPower; // Amount of power expended at the pump shaft
		int SurfNum; // Surface number in the Surface derived type for a radiant system surface
		int SurfNum2; // Surface number in the Surface derived type for a radiant system surface
		bool SysRunning; // TRUE when the system is running
		Real64 SysWaterInTemp; // Fluid temperature supplied from the loop
		Real64 WaterTempHi; // Current high point in water temperature range
		Real64 WaterTempLo; // Current low point in water temperature range
		int ZoneNum; // number of zone being served
		Real64 mdot; // local temporary for water mass flow rate kg/s

		// FLOW:
		// initialize local variables
		ZoneNum = CFloRadSys( RadSysNum ).ZonePtr;
		SysRunning = true; // default to running and turn off only if not running
		VarOffCond = false;

		if ( GetCurrentScheduleValue( CFloRadSys( RadSysNum ).SchedPtr ) <= 0 ) SysRunning = false;

		if ( SysRunning ) { // Unit is probably on-->this section is intended to control the water
			// mass flow rate being sent to the radiant system

			// Set the current setpoint temperature (same procedure for either heating or cooling)
			{ auto const SELECT_CASE_var( CFloRadSys( RadSysNum ).ControlType );
			if ( SELECT_CASE_var == MATControl ) {
				SetPointTemp = MAT( ZoneNum );
			} else if ( SELECT_CASE_var == MRTControl ) {
				SetPointTemp = MRT( ZoneNum );
			} else if ( SELECT_CASE_var == OperativeControl ) {
				SetPointTemp = 0.5 * ( MAT( ZoneNum ) + MRT( ZoneNum ) );
			} else if ( SELECT_CASE_var == ODBControl ) {
				SetPointTemp = Zone( ZoneNum ).OutDryBulbTemp;
			} else if ( SELECT_CASE_var == OWBControl ) {
				SetPointTemp = Zone( ZoneNum ).OutWetBulbTemp;
			} else { // Should never get here
				SetPointTemp = 0.0; // Suppress uninitialized warning
				ShowSevereError( "Illegal control type in low temperature radiant system: " + CFloRadSys( RadSysNum ).Name );
				ShowFatalError( "Preceding condition causes termination." );
			}}

			// Avoid problems when there is no heating or cooling control because the system only cools or heats
			if ( CFloRadSys( RadSysNum ).HotCtrlHiTempSchedPtr > 0 ) {
				OffTempHeat = GetCurrentScheduleValue( CFloRadSys( RadSysNum ).HotCtrlHiTempSchedPtr );
			} else {
				OffTempHeat = LowTempHeating;
			}
			if ( CFloRadSys( RadSysNum ).ColdCtrlLoTempSchedPtr > 0 ) {
				OffTempCool = GetCurrentScheduleValue( CFloRadSys( RadSysNum ).ColdCtrlLoTempSchedPtr );
			} else {
				OffTempCool = HighTempCooling;
			}

			// Now actually decide what to do based on the setpoint temperature in relation to the control temperatures
			if ( SetPointTemp < OffTempHeat ) { // HEATING MODE

				OperatingMode = HeatingMode;

				if ( ! CFloRadSys( RadSysNum ).HeatingSystem ) {

					SysRunning = false; // Can't heat unless it's a heating system

				} else { // It is a heating system so set all of the values for controls

					SetPointTempHi = GetCurrentScheduleValue( CFloRadSys( RadSysNum ).HotCtrlHiTempSchedPtr );
					SetPointTempLo = GetCurrentScheduleValue( CFloRadSys( RadSysNum ).HotCtrlLoTempSchedPtr );
					if ( SetPointTempHi < SetPointTempLo ) {
						ShowSevereError( "Heating setpoint temperature mismatch in" + CFloRadSys( RadSysNum ).Name );
						ShowContinueError( "High setpoint temperature is less than low setpoint temperature--check your schedule input" );
						ShowFatalError( "Preceding condition causes termination." );
					}

					WaterTempHi = GetCurrentScheduleValue( CFloRadSys( RadSysNum ).HotWaterHiTempSchedPtr );
					WaterTempLo = GetCurrentScheduleValue( CFloRadSys( RadSysNum ).HotWaterLoTempSchedPtr );
					if ( WaterTempHi < WaterTempLo ) {
						ShowSevereError( "Heating water temperature mismatch in" + CFloRadSys( RadSysNum ).Name );
						ShowContinueError( "High water temperature is less than low water temperature--check your schedule input" );
						ShowFatalError( "Preceding condition causes termination." );
					}

					if ( SetPointTemp >= SetPointTempHi ) {
						// System is above high heating setpoint so we should be able to turn the system off
						RadInTemp = WaterTempLo;
						SysRunning = false;
					} else if ( SetPointTemp <= SetPointTempLo ) {
						// System is running with its highest inlet temperature
						RadInTemp = WaterTempHi;
					} else {
						// Interpolate to obtain the current radiant system inlet temperature
						RadInTemp = WaterTempHi - ( WaterTempHi - WaterTempLo ) * ( SetPointTemp - SetPointTempLo ) / ( SetPointTempHi - SetPointTempLo );
					}

				}

			} else if ( SetPointTemp > OffTempCool ) { // COOLING MODE

				OperatingMode = CoolingMode;

				if ( ! CFloRadSys( RadSysNum ).CoolingSystem ) {

					SysRunning = false; // Can't cool unless it's a cooling system

				} else { // It is a cooling system so set all of the values for controls

					SetPointTempHi = GetCurrentScheduleValue( CFloRadSys( RadSysNum ).ColdCtrlHiTempSchedPtr );
					SetPointTempLo = GetCurrentScheduleValue( CFloRadSys( RadSysNum ).ColdCtrlLoTempSchedPtr );
					if ( SetPointTempHi < SetPointTempLo ) {
						ShowSevereError( "Cooling setpoint temperature mismatch in" + CFloRadSys( RadSysNum ).Name );
						ShowContinueError( "High setpoint temperature is less than low setpoint temperature--check your schedule input" );
						ShowFatalError( "Preceding condition causes termination." );
					}

					WaterTempHi = GetCurrentScheduleValue( CFloRadSys( RadSysNum ).ColdWaterHiTempSchedPtr );
					WaterTempLo = GetCurrentScheduleValue( CFloRadSys( RadSysNum ).ColdWaterLoTempSchedPtr );
					if ( WaterTempHi < WaterTempLo ) {
						ShowSevereError( "Cooling water temperature mismatch in" + CFloRadSys( RadSysNum ).Name );
						ShowContinueError( "High water temperature is less than low water temperature--check your schedule input" );
						ShowFatalError( "Preceding condition causes termination." );
					}

					if ( SetPointTemp <= SetPointTempLo ) {
						// System is below low cooling setpoint so we should be able to turn the system off
						RadInTemp = WaterTempHi;
						SysRunning = false;
					} else if ( SetPointTemp >= SetPointTempHi ) {
						// System is running with its lowest inlet temperature
						RadInTemp = WaterTempLo;
					} else {
						// Interpolate to obtain the current radiant system inlet temperature
						RadInTemp = WaterTempHi - ( WaterTempHi - WaterTempLo ) * ( SetPointTemp - SetPointTempLo ) / ( SetPointTempHi - SetPointTempLo );
					}

				}

			} else { // System is not running because the setpoint temperature is in the "deadband"

				RadInTemp = SetPointTemp;
				SysRunning = false;

			}

		}

		if ( SysRunning ) {
			CpFluid = GetSpecificHeatGlycol( fluidNameWater, RadInTemp, CFloRadSys( RadSysNum ).GlycolIndex, RoutineName );
		}

		if ( ( ! SysRunning ) || ( CpFluid < LowCpFluidValue ) ) {
			// Unit is off or has no load upon it OR CpFluid value is "zero" so
			// set the flow rates to zero and then simulate the components with
			// the no flow conditions
			OperatingMode = NotOperating;
			CFloRadSys( RadSysNum ).WaterMassFlowRate = 0.0;
			CFloRadSys( RadSysNum ).WaterInjectionRate = 0.0;
			CFloRadSys( RadSysNum ).WaterRecircRate = 0.0;
			CFloRadSys( RadSysNum ).HeatPower = 0.0;
			CFloRadSys( RadSysNum ).CoolPower = 0.0;
			CFloRadSys( RadSysNum ).PumpPower = 0.0;
			CFloRadSys( RadSysNum ).PumpMassFlowRate = 0.0;
			CFloRadSys( RadSysNum ).PumpHeattoFluid = 0.0;

			for ( SurfNum = 1; SurfNum <= CFloRadSys( RadSysNum ).NumOfSurfaces; ++SurfNum ) {
				SurfNum2 = CFloRadSys( RadSysNum ).SurfacePtr( SurfNum );
				QRadSysSource( SurfNum2 ) = 0.0;
				if ( Surface( SurfNum2 ).ExtBoundCond > 0 && Surface( SurfNum2 ).ExtBoundCond != SurfNum2 ) QRadSysSource( Surface( SurfNum2 ).ExtBoundCond ) = 0.0; // Also zero the other side of an interzone
			}

			// turn off flow requests made during init because it is not actually running
			if ( CFloRadSys( RadSysNum ).CWLoopNum > 0 ) {
				mdot = 0.0;
				SetComponentFlowRate( mdot, CFloRadSys( RadSysNum ).ColdWaterInNode, CFloRadSys( RadSysNum ).ColdWaterOutNode, CFloRadSys( RadSysNum ).CWLoopNum, CFloRadSys( RadSysNum ).CWLoopSide, CFloRadSys( RadSysNum ).CWBranchNum, CFloRadSys( RadSysNum ).CWCompNum );
			}
			if ( CFloRadSys( RadSysNum ).HWLoopNum > 0 ) {
				mdot = 0.0;
				SetComponentFlowRate( mdot, CFloRadSys( RadSysNum ).HotWaterInNode, CFloRadSys( RadSysNum ).HotWaterOutNode, CFloRadSys( RadSysNum ).HWLoopNum, CFloRadSys( RadSysNum ).HWLoopSide, CFloRadSys( RadSysNum ).HWBranchNum, CFloRadSys( RadSysNum ).HWCompNum );
			}
		} else { // (SysRunning) so simulate the system...

			// Determine pump flow rate and pump heat addition
			CFloRadSys( RadSysNum ).PumpMassFlowRate = CFloRadSys( RadSysNum ).WaterMassFlowRate; // Set in InitLowTempRadiantSystem
			if ( CFloRadSys( RadSysNum ).VolFlowSchedPtr > 0 ) {
				PumpPartLoadRat = GetCurrentScheduleValue( CFloRadSys( RadSysNum ).VolFlowSchedPtr );
			} else {
				PumpPartLoadRat = 1.0;
			}
			CFloRadSys( RadSysNum ).PumpPower = PumpPartLoadRat * CFloRadSys( RadSysNum ).NomPowerUse;
			ShaftPower = CFloRadSys( RadSysNum ).PumpPower * CFloRadSys( RadSysNum ).MotorEffic;
			// This adds the pump heat based on User input for the pump (same as in Pump module)
			// We assume that all of the heat ends up in the fluid eventually since this is a closed loop.
			CFloRadSys( RadSysNum ).PumpHeattoFluid = ShaftPower + ( ( CFloRadSys( RadSysNum ).PumpPower - ShaftPower ) * CFloRadSys( RadSysNum ).FracMotorLossToFluid );
			if ( CFloRadSys( RadSysNum ).PumpMassFlowRate > 0.0 ) {
				PumpTempRise = CFloRadSys( RadSysNum ).PumpHeattoFluid / ( CFloRadSys( RadSysNum ).PumpMassFlowRate * CpFluid );
			} else {
				PumpTempRise = 0.0;
			}

			LoopReqTemp = RadInTemp - PumpTempRise; // Temperature required at the inlet of the pump to meet the temperature request

			if ( OperatingMode == HeatingMode ) {

				// in heating mode so shut down cold water flow request
				if ( CFloRadSys( RadSysNum ).CWLoopNum > 0 ) {
					mdot = 0.0;
					SetComponentFlowRate( mdot, CFloRadSys( RadSysNum ).ColdWaterInNode, CFloRadSys( RadSysNum ).ColdWaterOutNode, CFloRadSys( RadSysNum ).CWLoopNum, CFloRadSys( RadSysNum ).CWLoopSide, CFloRadSys( RadSysNum ).CWBranchNum, CFloRadSys( RadSysNum ).CWCompNum );
				}
				LoopInNode = CFloRadSys( RadSysNum ).HotWaterInNode;
				SysWaterInTemp = Node( LoopInNode ).Temp;
				Iteration = false;

				if ( ( SysWaterInTemp >= LoopReqTemp ) && ( Node( LoopInNode ).MassFlowRateMaxAvail >= CFloRadSys( RadSysNum ).WaterMassFlowRate ) ) {
					// Case 1: Adequate temperature and flow
					// Best condition--loop inlet temperature greater than requested and we have enough flow.
					// So, proceed assuming the RadInTemp requested by the controls and then figure out the
					// mixing after the outlet radiant temperature is calculated.
					CFloRadSys( RadSysNum ).WaterInletTemp = RadInTemp;
					CalcLowTempCFloRadSysComps( RadSysNum, LoopInNode, Iteration, LoadMet );

					// We now have inlet and outlet temperatures--we still need to set the flow rates
					if ( ( SysWaterInTemp - CFloRadSys( RadSysNum ).WaterOutletTemp ) != 0.0 ) { // protect divide by zero
						CFloRadSys( RadSysNum ).WaterInjectionRate = ( CFloRadSys( RadSysNum ).WaterMassFlowRate * ( CFloRadSys( RadSysNum ).WaterInletTemp - CFloRadSys( RadSysNum ).WaterOutletTemp ) / ( SysWaterInTemp - CFloRadSys( RadSysNum ).WaterOutletTemp ) ) - ( CFloRadSys( RadSysNum ).PumpHeattoFluid / ( CpFluid * ( SysWaterInTemp - CFloRadSys( RadSysNum ).WaterOutletTemp ) ) );
					}
					CFloRadSys( RadSysNum ).WaterRecircRate = CFloRadSys( RadSysNum ).WaterMassFlowRate - CFloRadSys( RadSysNum ).WaterInjectionRate;

				} else if ( ( SysWaterInTemp < LoopReqTemp ) && ( Node( LoopInNode ).MassFlowRateMaxAvail >= CFloRadSys( RadSysNum ).WaterMassFlowRate ) ) {
					// Case 2: Adequate flow but temperature too low
					// Only thing to do is to reset the inlet temperature and assume that the loop will supply
					// the entire flow to the component (no recirculation but potentially some bypass for the
					// overall loop).  There is no way we can meet the control temperature so don't even try.
					CFloRadSys( RadSysNum ).WaterInletTemp = SysWaterInTemp + PumpTempRise;
					CalcLowTempCFloRadSysComps( RadSysNum, LoopInNode, Iteration, LoadMet );

					// We now have inlet and outlet temperatures--we still need to set the flow rates
					if ( ( SysWaterInTemp - CFloRadSys( RadSysNum ).WaterOutletTemp ) != 0.0 ) { // protect divide by zero
						CFloRadSys( RadSysNum ).WaterInjectionRate = ( CFloRadSys( RadSysNum ).WaterMassFlowRate * ( CFloRadSys( RadSysNum ).WaterInletTemp - CFloRadSys( RadSysNum ).WaterOutletTemp ) / ( SysWaterInTemp - CFloRadSys( RadSysNum ).WaterOutletTemp ) ) - ( CFloRadSys( RadSysNum ).PumpHeattoFluid / ( CpFluid * ( SysWaterInTemp - CFloRadSys( RadSysNum ).WaterOutletTemp ) ) );
					} else {
						CFloRadSys( RadSysNum ).WaterInjectionRate = CFloRadSys( RadSysNum ).WaterMassFlowRate;
					}
					if ( CFloRadSys( RadSysNum ).WaterInjectionRate > CFloRadSys( RadSysNum ).WaterMassFlowRate ) CFloRadSys( RadSysNum ).WaterInjectionRate = CFloRadSys( RadSysNum ).WaterMassFlowRate;
					CFloRadSys( RadSysNum ).WaterRecircRate = 0.0; // by definition

				} else if ( ( SysWaterInTemp >= LoopReqTemp ) && ( Node( LoopInNode ).MassFlowRateMaxAvail < CFloRadSys( RadSysNum ).WaterMassFlowRate ) ) {
					// Case 3: Adequate temperature but loop flow is less than component flow
					// This case might work out, but there is no guarantee that there is enough loop flow to
					// mix with the recirculation flow and still provide a high enough temperature.  First
					// step is to try the inlet temperature and flow rate as in Case 1.  If we can obtain
					// the proper temperature inlet to the radiant system, then we are done.  If not, we
					// have to repeat the solution for an unknown inlet temperature and a known recirculation
					// rate.
					CFloRadSys( RadSysNum ).WaterInletTemp = RadInTemp;
					CalcLowTempCFloRadSysComps( RadSysNum, LoopInNode, Iteration, LoadMet );

					// Now see if we can really get that desired into temperature (RadInTemp) by solving
					// for the flow that is injected from the loop.  A heat balance for the mixer that relates
					// the important quantities is:
					//   Mdotradsys*Cp*Tradsysin = Mdotloop*Cp*Tloop + (Mdotradsys-Mdotloop)*Cp*Tradsysout + PumpHeat
					// or rearranging to get the injection flow (Mdotloop):
					//   Mdotloop = Mdotcomp*(Tradsysin-Tradsysout)/(Tloop-Tradsysout) - PumpHeat/(Cp*(Tloop-Tradsysout))
					// If Mdotloop from this equation is greater that the loop flow rate (Node%MassFlowRate),
					// then we cannot meet the inlet temperature and we have to "iterate" through the
					// alternate solution.
					InjectFlowRate = ( CFloRadSys( RadSysNum ).WaterMassFlowRate * ( CFloRadSys( RadSysNum ).WaterInletTemp - CFloRadSys( RadSysNum ).WaterOutletTemp ) / ( SysWaterInTemp - CFloRadSys( RadSysNum ).WaterOutletTemp ) ) - ( CFloRadSys( RadSysNum ).PumpHeattoFluid / ( CpFluid * ( SysWaterInTemp - CFloRadSys( RadSysNum ).WaterOutletTemp ) ) );
					if ( InjectFlowRate > Node( LoopInNode ).MassFlowRateMaxAvail ) {
						// We didn't have enough flow from the loop to meet our inlet temperature request.
						// So, set the injection rate to the loop flow and calculate the recirculation flow.
						// Then, resimulate the radiant system using these values (it will obtain the actual
						// inlet temperature that results from this).
						CFloRadSys( RadSysNum ).WaterInjectionRate = Node( LoopInNode ).MassFlowRateMaxAvail;
						CFloRadSys( RadSysNum ).WaterRecircRate = CFloRadSys( RadSysNum ).WaterMassFlowRate - CFloRadSys( RadSysNum ).WaterInjectionRate;
						CFloRadSys( RadSysNum ).WaterInletTemp = SysWaterInTemp + PumpTempRise;
						Iteration = true;
						CalcLowTempCFloRadSysComps( RadSysNum, LoopInNode, Iteration, LoadMet );
					} else {
						CFloRadSys( RadSysNum ).WaterInjectionRate = InjectFlowRate;
						CFloRadSys( RadSysNum ).WaterRecircRate = CFloRadSys( RadSysNum ).WaterMassFlowRate - CFloRadSys( RadSysNum ).WaterInjectionRate;

					}

				} else if ( ( SysWaterInTemp < LoopReqTemp ) && ( Node( LoopInNode ).MassFlowRateMaxAvail < CFloRadSys( RadSysNum ).WaterMassFlowRate ) ) {
					// Case 4: Temperature too low and loop flow is less than component flow
					// Worst condition--can't meet the temperature request at all.  Only thing to do is to
					// set the loop flow and recirculation rate (known) and solve for the inlet temperature
					// using the "iteration" solution scheme from "Case 3B" above
					CFloRadSys( RadSysNum ).WaterInjectionRate = Node( LoopInNode ).MassFlowRateMaxAvail;
					CFloRadSys( RadSysNum ).WaterRecircRate = CFloRadSys( RadSysNum ).WaterMassFlowRate - CFloRadSys( RadSysNum ).WaterInjectionRate;
					CFloRadSys( RadSysNum ).WaterInletTemp = SysWaterInTemp + PumpTempRise;
					Iteration = true;
					CalcLowTempCFloRadSysComps( RadSysNum, LoopInNode, Iteration, LoadMet );

				}

			} else if ( OperatingMode == CoolingMode ) {

				// in cooling mode so shut down heating water flow request
				if ( CFloRadSys( RadSysNum ).HWLoopNum > 0 ) {
					mdot = 0.0;
					SetComponentFlowRate( mdot, CFloRadSys( RadSysNum ).HotWaterInNode, CFloRadSys( RadSysNum ).HotWaterOutNode, CFloRadSys( RadSysNum ).HWLoopNum, CFloRadSys( RadSysNum ).HWLoopSide, CFloRadSys( RadSysNum ).HWBranchNum, CFloRadSys( RadSysNum ).HWCompNum );
				}
				LoopInNode = CFloRadSys( RadSysNum ).ColdWaterInNode;
				SysWaterInTemp = Node( LoopInNode ).Temp;
				CFloCondIterNum = 1;
				while ( ( CFloCondIterNum <= 1 ) || ( ( CFloCondIterNum <= 2 ) && ( CFloRadSys( RadSysNum ).CondCtrlType == CondCtrlVariedOff ) && ( VarOffCond ) ) ) {
					Iteration = false;

					if ( ( SysWaterInTemp <= LoopReqTemp ) && ( Node( LoopInNode ).MassFlowRateMaxAvail >= CFloRadSys( RadSysNum ).WaterMassFlowRate ) ) {
						// Case 1: Adequate temperature and flow
						// Best condition--loop inlet temperature lower than requested and we have enough flow.
						// So, proceed assuming the RadInTemp requested by the controls and then figure out the
						// mixing after the outlet radiant temperature is calculated.

						// This condition can also happen when LoopReqTemp has been reset  to dewpoint for condensation control
						if ( ! VarOffCond ) {
							CFloRadSys( RadSysNum ).WaterInletTemp = RadInTemp;
						} else {
							CFloRadSys( RadSysNum ).WaterInletTemp = LoopReqTemp;
						}
						CalcLowTempCFloRadSysComps( RadSysNum, LoopInNode, Iteration, LoadMet );

						// We now have inlet and outlet temperatures--we still need to set the flow rates
						CFloRadSys( RadSysNum ).WaterInjectionRate = ( CFloRadSys( RadSysNum ).WaterMassFlowRate * ( CFloRadSys( RadSysNum ).WaterInletTemp - CFloRadSys( RadSysNum ).WaterOutletTemp ) / ( SysWaterInTemp - CFloRadSys( RadSysNum ).WaterOutletTemp ) ) - ( CFloRadSys( RadSysNum ).PumpHeattoFluid / ( CpFluid * ( SysWaterInTemp - CFloRadSys( RadSysNum ).WaterOutletTemp ) ) );
						CFloRadSys( RadSysNum ).WaterRecircRate = CFloRadSys( RadSysNum ).WaterMassFlowRate - CFloRadSys( RadSysNum ).WaterInjectionRate;

					} else if ( ( SysWaterInTemp > LoopReqTemp ) && ( Node( LoopInNode ).MassFlowRateMaxAvail >= CFloRadSys( RadSysNum ).WaterMassFlowRate ) ) {
						// Case 2: Adequate flow but temperature too high
						// Only thing to do is to reset the inlet temperature and assume that the loop will supply
						// the entire flow to the component (no recirculation but potentially some bypass for the
						// overall loop).  There is no way we can meet the control temperature so don't even try.
						CFloRadSys( RadSysNum ).WaterInletTemp = SysWaterInTemp + PumpTempRise;
						CalcLowTempCFloRadSysComps( RadSysNum, LoopInNode, Iteration, LoadMet );

						// We now have inlet and outlet temperatures--we still need to set the flow rates
						if ( ( SysWaterInTemp - CFloRadSys( RadSysNum ).WaterOutletTemp ) != 0.0 ) { // protect div by zero
							CFloRadSys( RadSysNum ).WaterInjectionRate = ( CFloRadSys( RadSysNum ).WaterMassFlowRate * ( CFloRadSys( RadSysNum ).WaterInletTemp - CFloRadSys( RadSysNum ).WaterOutletTemp ) / ( SysWaterInTemp - CFloRadSys( RadSysNum ).WaterOutletTemp ) ) - ( CFloRadSys( RadSysNum ).PumpHeattoFluid / ( CpFluid * ( SysWaterInTemp - CFloRadSys( RadSysNum ).WaterOutletTemp ) ) );
						} else { // no temp change present, set injection rate to full flow
							CFloRadSys( RadSysNum ).WaterInjectionRate = CFloRadSys( RadSysNum ).WaterMassFlowRate;
						}
						if ( CFloRadSys( RadSysNum ).WaterInjectionRate > CFloRadSys( RadSysNum ).WaterMassFlowRate ) CFloRadSys( RadSysNum ).WaterInjectionRate = CFloRadSys( RadSysNum ).WaterMassFlowRate;
						CFloRadSys( RadSysNum ).WaterRecircRate = 0.0; // by definition

					} else if ( ( SysWaterInTemp <= LoopReqTemp ) && ( Node( LoopInNode ).MassFlowRateMaxAvail < CFloRadSys( RadSysNum ).WaterMassFlowRate ) ) {
						// Case 3: Adequate temperature but loop flow is less than component flow
						// This case might work out, but there is no guarantee that there is enough loop flow to
						// mix with the recirculation flow and still provide a high enough temperature.  First
						// step is to try the inlet temperature and flow rate as in Case 1.  If we can obtain
						// the proper temperature inlet to the radiant system, then we are done.  If not, we
						// have to repeat the solution for an unknown inlet temperature and a known recirculation
						// rate.
						// This condition might happen when LoopReqTemp has been reset  to dewpoint for condensation control
						if ( ! VarOffCond ) {
							CFloRadSys( RadSysNum ).WaterInletTemp = RadInTemp;
						} else {
							CFloRadSys( RadSysNum ).WaterInletTemp = LoopReqTemp;
						}
						CalcLowTempCFloRadSysComps( RadSysNum, LoopInNode, Iteration, LoadMet );

						// Now see if we can really get that desired into temperature (RadInTemp) by solving
						// for the flow that is injected from the loop.  A heat balance for the mixer that relates
						// the important quantities is:
						//   Mdotradsys*Cp*Tradsysin = Mdotloop*Cp*Tloop + (Mdotradsys-Mdotloop)*Cp*Tradsysout + PumpHeat
						// or rearranging to get the injection flow (Mdotloop):
						//   Mdotloop = Mdotcomp*(Tradsysin-Tradsysout)/(Tloop-Tradsysout) - PumpHeat/(Cp*(Tloop-Tradsysout))
						// If Mdotloop from this equation is greater that the loop flow rate (Node%MassFlowRate),
						// then we cannot meet the inlet temperature and we have to "iterate" through the
						// alternate solution.
						InjectFlowRate = ( CFloRadSys( RadSysNum ).WaterMassFlowRate * ( CFloRadSys( RadSysNum ).WaterInletTemp - CFloRadSys( RadSysNum ).WaterOutletTemp ) / ( SysWaterInTemp - CFloRadSys( RadSysNum ).WaterOutletTemp ) ) - ( CFloRadSys( RadSysNum ).PumpHeattoFluid / ( CpFluid * ( SysWaterInTemp - CFloRadSys( RadSysNum ).WaterOutletTemp ) ) );
						if ( InjectFlowRate > Node( LoopInNode ).MassFlowRateMaxAvail ) {
							// We didn't have enough flow from the loop to meet our inlet temperature request.
							// So, set the injection rate to the loop flow and calculate the recirculation flow.
							// Then, resimulate the radiant system using these values (it will obtain the actual
							// inlet temperature that results from this).
							CFloRadSys( RadSysNum ).WaterInjectionRate = Node( LoopInNode ).MassFlowRateMaxAvail;
							CFloRadSys( RadSysNum ).WaterRecircRate = CFloRadSys( RadSysNum ).WaterMassFlowRate - CFloRadSys( RadSysNum ).WaterInjectionRate;
							CFloRadSys( RadSysNum ).WaterInletTemp = SysWaterInTemp + PumpTempRise;
							Iteration = true;
							CalcLowTempCFloRadSysComps( RadSysNum, LoopInNode, Iteration, LoadMet );
						} else {
							CFloRadSys( RadSysNum ).WaterInjectionRate = InjectFlowRate;
							CFloRadSys( RadSysNum ).WaterRecircRate = CFloRadSys( RadSysNum ).WaterMassFlowRate - CFloRadSys( RadSysNum ).WaterInjectionRate;

						}

					} else if ( ( SysWaterInTemp > LoopReqTemp ) && ( Node( LoopInNode ).MassFlowRateMaxAvail < CFloRadSys( RadSysNum ).WaterMassFlowRate ) ) {
						// Case 4: Temperature too low and loop flow is less than component flow
						// Worst condition--can't meet the temperature request at all.  Only thing to do is to
						// set the loop flow and recirculation rate (known) and solve for the inlet temperature
						// using the "iteration" solution scheme from "Case 3B" above
						CFloRadSys( RadSysNum ).WaterInjectionRate = Node( LoopInNode ).MassFlowRateMaxAvail;
						CFloRadSys( RadSysNum ).WaterRecircRate = CFloRadSys( RadSysNum ).WaterMassFlowRate - CFloRadSys( RadSysNum ).WaterInjectionRate;
						CFloRadSys( RadSysNum ).WaterInletTemp = SysWaterInTemp + PumpTempRise;
						Iteration = true;
						CalcLowTempCFloRadSysComps( RadSysNum, LoopInNode, Iteration, LoadMet );

					}

					++CFloCondIterNum;

				}

			} // Operating mode (heating or cooling)

			// Case when system has been shut down because of condensation issues or other limitations:
			if ( CFloRadSys( RadSysNum ).WaterMassFlowRate < MassFlowTolerance ) {
				CFloRadSys( RadSysNum ).WaterMassFlowRate = 0.0;
				CFloRadSys( RadSysNum ).WaterInjectionRate = 0.0;
				CFloRadSys( RadSysNum ).WaterRecircRate = 0.0;
				CFloRadSys( RadSysNum ).PumpMassFlowRate = 0.0;
				OperatingMode = NotOperating;
			}

			// There are some cases when the pump heat is actually enough to provide all the heating that the system needs.
			// In this case, the water injection flow rate will come back as a slightly negative number.  Reset it to zero
			// and just recirculate all the flow through the local loop.
			if ( CFloRadSys( RadSysNum ).WaterInjectionRate < 0.0 ) {
				CFloRadSys( RadSysNum ).WaterInjectionRate = 0.0;
				CFloRadSys( RadSysNum ).WaterRecircRate = CFloRadSys( RadSysNum ).WaterMassFlowRate;
			}

			// Error check, just in case
			if ( CFloRadSys( RadSysNum ).WaterRecircRate < 0.0 ) {
				ShowWarningError( "Flow mismatch in radiant system--result will be an energy imbalance--should not get this error" );
				ShowContinueErrorTimeStamp( "WaterRecircRate=" + TrimSigDigits( CFloRadSys( RadSysNum ).WaterRecircRate, 2 ) + ", in Radiant System=" + CFloRadSys( RadSysNum ).Name + ',' );
				CFloRadSys( RadSysNum ).WaterRecircRate = 0.0;
				CFloRadSys( RadSysNum ).WaterInjectionRate = CFloRadSys( RadSysNum ).WaterMassFlowRate;
			}

		} // System running mode (yes or no)

	}

	void
	CalcLowTempCFloRadSysComps(
		int const RadSysNum, // Index for the low temperature radiant system under consideration
		int const MainLoopNodeIn, // Node number on main loop of the inlet node to the radiant system
		bool const Iteration, // FALSE for the regular solution, TRUE when we had to loop back
		Real64 & LoadMet // Load met by the low temperature radiant system, in Watts
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   August 2003
		//       MODIFIED       Sep 2011 LKL/BG - resimulate only zones needing it for Radiant systems
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine solves the radiant system based on how much water is (and
		// the conditions of the water) supplied to the radiant system.  The purpose
		// of this subroutine is similar to CalcLowTempHydrRadSysComps except that
		// it solves this for a constant flow hydronic radiant system.

		// METHODOLOGY EMPLOYED:
		// Use heat exchanger formulas to obtain the heat source/sink for the radiant
		// system based on the inlet conditions and flow rate of water.  Once that is
		// determined, recalculate the surface heat balances to reflect this heat
		// addition/subtraction.  The load met by the system is determined by the
		// difference between the convection from all surfaces in the zone when
		// there was no radiant system output and with a source/sink added.

		// REFERENCES:
		// IBLAST-QTF research program, completed in January 1995 (unreleased)
		// Strand, R.K. 1995. "Heat Source Transfer Functions and Their Application to
		//   Low Temperature Radiant Heating Systems", Ph.D. dissertation, University
		//   of Illinois at Urbana-Champaign, Department of Mechanical and Industrial
		//   Engineering.

		// Using/Aliasing
		using DataEnvironment::OutBaroPress;
		using General::RoundSigDigits;
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
		using DataHeatBalSurface::TH;
		using DataLoopNode::Node;
		using DataSurfaces::Surface;
		using DataSurfaces::HeatTransferModel_CondFD;
		using DataSurfaces::HeatTransferModel_CTF;
		using FluidProperties::GetSpecificHeatGlycol;
		using PlantUtilities::SetComponentFlowRate;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// through and figure out more information (i.e., did not know the
		// inlet temperature directly)

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const TempCheckLimit( 0.1 ); // Maximum allowed temperature difference between outlet temperature calculations
		Real64 const ZeroSystemResp( 0.1 ); // Response below which the system response is really zero
		static std::string const RoutineName( "CalcLowTempCFloRadSysComps" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ConstrNum; // Index for construction number in Construct derived type
		Real64 Cp; // Intermediate calculational variable for specific heat of water
		Real64 DewPointTemp; // Dew-point temperature based on the zone air conditions
		Real64 EpsMdotCp; // Epsilon (heat exchanger terminology) times water mass flow rate times water specific heat
		Real64 LoopTerm; // Intermeidate calculation variable for determining the water inlet temperature
		Real64 Mdot; // Intermediate calculation variable for mass flow rate in a surface within the radiant system
		int RadSurfNum; // DO loop counter for the surfaces that comprise a particular radiant system
		int RadSurfNum2; // DO loop counter for the surfaces that comprise a particular radiant system
		int RadSurfNum3; // DO loop counter for the surfaces that comprise a particular radiant system
		Real64 RecircTerm; // Intermeidate calculation variable for determining the water inlet temperature
		Real64 SumFlowFracCkCm; // Summation of surface flow fraction, Ck, and Cm product for each surface in the system
		Real64 SumFlowFracOneMinusCm; // Summation of surface flow fraction times (1-Cm) for each surface in the radiant system
		int SurfNum; // Index for radiant surface in Surface derived type
		int SurfNum2; // Index for radiant surface in Surface derived type
		Real64 TotalRadSysPower; // Total heat source/sink to radiant system
		Real64 TwiCoeff; // Intermeidate calculation variable for determining the water inlet temperature
		Real64 WaterMassFlow; // Water mass flow rate in the radiant system, kg/s
		int WaterNodeIn; // Node number of the water entering the radiant system
		Real64 WaterOutletTempCheck; // Radiant system water outlet temperature (calculated from mixing all outlet streams together)
		Real64 WaterTempIn; // Temperature of the water entering the radiant system, in C
		int ZoneNum; // number of zone being served
		Real64 ZoneMult; // Zone multiplier for this system

		Real64 Ca; // Coefficients to relate the inlet water temperature to the heat source
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
		// For more info on Ca through Cl, see comments below

		static Array1D< Real64 > Ckj; // Coefficients for individual surfaces within a radiant system
		static Array1D< Real64 > Cmj;
		static Array1D< Real64 > WaterTempOut; // Array of outlet water temperatures for
		// each surface in the radiant system

		static bool FirstTimeFlag( true ); // for setting size of Ckj, Cmj, WaterTempOut arrays

		// FLOW:
		// First, apply heat exchanger logic to find the heat source/sink to the system.
		// This involves finding out the heat transfer characteristics of the hydronic
		// loop and then applying the equations derived on pp. 113-118 of the dissertation.
		if ( FirstTimeFlag ) {
			Ckj.allocate( MaxCloNumOfSurfaces );
			Cmj.allocate( MaxCloNumOfSurfaces );
			WaterTempOut.allocate( MaxCloNumOfSurfaces );
			FirstTimeFlag = false;
		}

		Ckj = 0.0;
		Cmj = 0.0;
		WaterTempOut = CFloRadSys( RadSysNum ).WaterInletTemp;

		// Set the conditions on the water side inlet
		{ auto const SELECT_CASE_var( OperatingMode );
		if ( SELECT_CASE_var == HeatingMode ) {
			WaterNodeIn = CFloRadSys( RadSysNum ).HotWaterInNode;
		} else if ( SELECT_CASE_var == CoolingMode ) {
			WaterNodeIn = CFloRadSys( RadSysNum ).ColdWaterInNode;
		} else {
			ShowSevereError( "Illegal low temperature radiant system operating mode" );
			ShowContinueError( "Occurs in Radiant System=" + CFloRadSys( RadSysNum ).Name );
			ShowFatalError( "Preceding condition causes termination." );
		}}
		ZoneNum = CFloRadSys( RadSysNum ).ZonePtr;
		ZoneMult = double( Zone( ZoneNum ).Multiplier * Zone( ZoneNum ).ListMultiplier );
		WaterMassFlow = CFloRadSys( RadSysNum ).WaterMassFlowRate / ZoneMult;
		WaterTempIn = CFloRadSys( RadSysNum ).WaterInletTemp;

		if ( WaterMassFlow <= 0.0 ) {
			// No flow or below minimum allowed so there is no heat source/sink
			// This is possible with a mismatch between system and plant operation
			// or a slight mismatch between zone and system controls.  This is not
			// necessarily a "problem" so this exception is necessary in the code.
			for ( RadSurfNum = 1; RadSurfNum <= CFloRadSys( RadSysNum ).NumOfSurfaces; ++RadSurfNum ) {
				SurfNum = CFloRadSys( RadSysNum ).SurfacePtr( RadSurfNum );
				QRadSysSource( SurfNum ) = 0.0;
				if ( Surface( SurfNum ).ExtBoundCond > 0 && Surface( SurfNum ).ExtBoundCond != SurfNum ) QRadSysSource( Surface( SurfNum ).ExtBoundCond ) = 0.0; // Also zero the other side of an interzone
			}

			CFloRadSys( RadSysNum ).WaterOutletTemp = CFloRadSys( RadSysNum ).WaterInletTemp;

		} else {

			for ( RadSurfNum = 1; RadSurfNum <= CFloRadSys( RadSysNum ).NumOfSurfaces; ++RadSurfNum ) {
				SurfNum = CFloRadSys( RadSysNum ).SurfacePtr( RadSurfNum );
				// Determine the heat exchanger "effectiveness" term
				EpsMdotCp = CalcRadSysHXEffectTerm( RadSysNum, ConstantFlowSystem, WaterTempIn, WaterMassFlow, CFloRadSys( RadSysNum ).SurfaceFlowFrac( RadSurfNum ), CFloRadSys( RadSysNum ).NumCircuits( RadSurfNum ), CFloRadSys( RadSysNum ).TubeLength, CFloRadSys( RadSysNum ).TubeDiameter, CFloRadSys( RadSysNum ).GlycolIndex );

				// Obtain the heat balance coefficients and calculate the intermediate coefficients
				// linking the inlet water temperature to the heat source/sink to the radiant system.
				// The coefficients are based on the following development...
				// The heat balance equations at the outside and inside surfaces are of the form:
				//   Tinside  = Ca + Cb*Toutside + Cc*q"
				//   Toutside = Cd + Ce*Tinside  + Cf*q"
				//   Tsource  = Cg + Ch*q"       + Ci*Tinside + Cj*Toutside
				// where:
				//   Tinside is the temperature at the inside surface
				//   Toutside is the temperature at the outside surface
				//   Tsource is the temperature within the radiant system at the location of the source/sink
				//   Ca is all of the other terms in the inside heat balance (solar, LW exchange, conduction history terms, etc.)
				//   Cb is the current cross CTF term
				//   Cc is the QTF inside term for the current heat source/sink
				//   Cd is all of the other terms in the outside heat balance (solar, LW exchange, conduction history terms, etc.)
				//   Ce is the current cross CTF term (should be equal to Cb)
				//   Cf is the QTF outside term for the current heat source/sink
				//   Cg is the summation of all temperature and source history terms at the source/sink location
				//   Ch is the QTF term at the source/sink location for the current heat source/sink
				//   Ci is the CTF inside term for the current inside surface temperature
				//   Cj is the CTF outside term for the current outside surface temperature
				// Note that it is necessary to not use "slow conduction" assumptions because the
				// source/sink has an impact on BOTH the inside and outside surface heat balances.
				// Hence the more general formulation.
				// The first two T equations above can be solved to remove the other surface temperature.
				// This results in the following equations:
				//   Tinside  = Ca + Cb*(Cd + Ce*Tinside + Cf*q") + Cc*q"   or...
				//   Tinside  = (Ca + Cb*Cd + (Cc+Cb*Cf)*q") / (1 - Ce*Cb)
				//   Toutside = Cd + Ce*(Ca + Cb*Toutside + Cc*q") + Cf*q"  or...
				//   Toutside = (Cd + Ce*Ca + (Cf+Ce*Cc)*q") / (1 - Ce*Cb)
				// Substituting the new equations for Tinside and Toutside as a function of C and q"
				// into the equation for Tsource...
				//   Tsource  = Cg + Ch*q" + Ci*((Ca + Cb*Cd + (Cc+Cb*Cf)*q") / (1 - Ce*Cb)) &
				//                         + Cj*((Cd + Ce*Ca + (Cf+Ce*Cc)*q") / (1 - Ce*Cb))
				// Or rearranging this to get Tsource as a function of q", we get...
				//   Tsource  =  Cg + ((Ci*(Ca + Cb*Cd) + Cj*(Cd + Ce*Ca))/(1-Ce*Cb)) &
				//             +(Ch + ((Ci*(Cc + Cb*Cf) + Cj*(Cf + Ce*Cc))/(1-Ce*Cb)))*q"
				// Or in a slightly simpler form...
				//   Tsource  = Ck + Cl*q"
				// where:
				//   Ck = Cg + ((Ci*(Ca + Cb*Cd) + Cj*(Cd + Ce*Ca))/(1-Ce*Cb))
				//   Cl = Ch + ((Ci*(Cc + Cb*Cf) + Cj*(Cf + Ce*Cc))/(1-Ce*Cb))
				// Note also that from heat exchanger "algebra", we have:
				//   q = epsilon*qmax    and    qmax = Mdot*Cp*(Twaterin-Tsource)
				// So...
				//   q" = q/Area = (epsilon*Mdot*Cp/Area)*(Twaterin-Tsource)
				// Or rearranging this equation:
				//   Tsource = -(q"*A/(epsilon*Mdot*Cp)) + Twaterin
				// Setting this equation equal to the other equation for Tsource a couple lines up
				// and rearranging to solve for q"...
				//   q" = (Twaterin - Ck) / (Cl + (A/(epsilon*Mdot*Cp))
				// or
				//   q  = (Twaterin - Ck) / ((Cl/A) + (1/epsilon*Mdot*Cp))
				// or
				//   q  = epsilon*Mdot*Cp*(Twaterin - Ck) / (1+(epsilon*Mdot*Cp*Cl/A))
				// which is the desired result, that is the heat source or sink to the radiant
				// system as a function of the water inlet temperature (flow rate is also in there
				// as well as all of the heat balance terms "hidden" in Ck and Cl).

				ConstrNum = Surface( SurfNum ).Construction;

				Ca = RadSysTiHBConstCoef( SurfNum );
				Cb = RadSysTiHBToutCoef( SurfNum );
				Cc = RadSysTiHBQsrcCoef( SurfNum );

				Cd = RadSysToHBConstCoef( SurfNum );
				Ce = RadSysToHBTinCoef( SurfNum );
				Cf = RadSysToHBQsrcCoef( SurfNum );

				Cg = CTFTsrcConstPart( SurfNum );
				Ch = Construct( ConstrNum ).CTFTSourceQ( 0 );
				Ci = Construct( ConstrNum ).CTFTSourceIn( 0 );
				Cj = Construct( ConstrNum ).CTFTSourceOut( 0 );

				Ck = Cg + ( ( Ci * ( Ca + Cb * Cd ) + Cj * ( Cd + Ce * Ca ) ) / ( 1.0 - Ce * Cb ) );
				Cl = Ch + ( ( Ci * ( Cc + Cb * Cf ) + Cj * ( Cf + Ce * Cc ) ) / ( 1.0 - Ce * Cb ) );

				Mdot = WaterMassFlow * CFloRadSys( RadSysNum ).SurfaceFlowFrac( RadSurfNum );
				Cp = GetSpecificHeatGlycol( fluidNameWater, WaterTempIn, CFloRadSys( RadSysNum ).GlycolIndex, RoutineName );

				if ( ! Iteration ) {

					if ( Surface( SurfNum ).HeatTransferAlgorithm == HeatTransferModel_CTF ) QRadSysSource( SurfNum ) = EpsMdotCp * ( WaterTempIn - Ck ) / ( 1.0 + ( EpsMdotCp * Cl / Surface( SurfNum ).Area ) );

					if ( Surface( SurfNum ).HeatTransferAlgorithm == HeatTransferModel_CondFD ) QRadSysSource( SurfNum ) = EpsMdotCp * ( WaterTempIn - TCondFDSourceNode( SurfNum ) );

					if ( Surface( SurfNum ).ExtBoundCond > 0 && Surface( SurfNum ).ExtBoundCond != SurfNum ) QRadSysSource( Surface( SurfNum ).ExtBoundCond ) = QRadSysSource( SurfNum ); // Also set the other side of an interzone
					WaterTempOut( RadSurfNum ) = WaterTempIn - ( QRadSysSource( SurfNum ) / ( Mdot * Cp ) );
				} else { // (Iteration)
					// In this case, we did not know the inlet temperature directly and have
					// to figure it out as part of the solution.  Thus, we have to do a little
					// more algebra.
					// The last equation in the previous block was:
					//   q = epsilon*Mdot*Cp*(Twaterin - Ck) / (1+(epsilon*Mdot*Cp*Cl/A))
					// which combines with:
					//   q = Mdot*Cp*(Twaterin - Twaterout,j)
					// so that:
					//   (Twaterin - Twaterout.j) = epsilon*(Twaterin - Ck) / (1+(epsilon*Mdot*Cp*Cl/A))
					// Let:
					//   Cm = epsilonj / (1+(epsilonj*Mdot,j*Cp*Cl,j/A))
					// for each surface in the radiant system.  This results in:
					//   (Twaterin - Twaterout,j) = Cm,j*(Twaterin - Ck,j)
					// Or:
					//   Twaterout,j = (1 - Cm,j)*Twaterin + Cm,j*Ck,j
					// This holds for each surface that is part of the radiant system (j).  To get the
					// overall outlet temperature, we have to do a mixing calculation after all of the
					// surfaces have been simulated:
					//   Twaterout = SUM(Fractionj*Twaterout,j)
					// We also have to solve an energy balance at the mixing valve and add in pump heat.
					// The energy balance at the mixing valve relates the loop inlet temperature (Tloopin)
					// and the overall outlet temperature (Twaterout):
					//   Tpumpin = (Mdotloop/Mdotradsys)*Tloopin + (Mdotrecirc/Mdotradsys)*Twaterout
					// This can then be related to the inlet water temperature to the radiant system
					// after pump heat has been taken into account:
					//   Twaterin = (Mdotloop/Mdotradsys)*Tloopin + (Mdotrecirc/Mdotradsys)*Twaterout + PumpHeat/(Mdotradsys*Cp)
					// Pluggin in the definition of Twaterout (sum equation above) and then the definition
					// of each individual Twaterout,j equation (which is solely a function of Twaterin
					// and coefficients), we can obtain an equation for Twaterin that consists of all
					// known quantities.  This requires us to calculate Ck,j and Cm,j for all the radiant
					// surfaces in the system first and then coming up with a calculation for Twaterin.
					// After than, individual Twaterout,j can be calculated along with QRadSysSource.
					Ckj( RadSurfNum ) = Ck;
					Cmj( RadSurfNum ) = ( EpsMdotCp / ( Mdot * Cp ) ) / ( 1.0 + ( EpsMdotCp * Cl / Surface( SurfNum ).Area ) );

					if ( RadSurfNum == CFloRadSys( RadSysNum ).NumOfSurfaces ) { // Last one so we can now do the other calculations
						// Equation for Twaterin is:
						//   Twaterin = (LoopTerm + RecircTerm)/(TwiCoeff)
						// where:
						//   LoopTerm   = (Mdotloop/Mdotradsys)*Tloopin + PumpHeat/(Mdotradsys*Cp)
						//   RecircTerm = (Mdotrecirc/Mdotradsys)*SUM(FlowFracj*Ck,j*Cm,j)
						//   TwiCoeff   = 1 - (Mdotrecirc/Mdotradsys)*SUM(FlowFracj*(1 - Cm,j))
						SumFlowFracCkCm = 0.0;
						SumFlowFracOneMinusCm = 0.0;
						for ( RadSurfNum2 = 1; RadSurfNum2 <= CFloRadSys( RadSysNum ).NumOfSurfaces; ++RadSurfNum2 ) {
							SumFlowFracCkCm += ( CFloRadSys( RadSysNum ).SurfaceFlowFrac( RadSurfNum2 ) * Ckj( RadSurfNum ) * Cmj( RadSurfNum2 ) );
							SumFlowFracOneMinusCm += ( CFloRadSys( RadSysNum ).SurfaceFlowFrac( RadSurfNum2 ) * ( 1.0 - Cmj( RadSurfNum2 ) ) );
						}

						LoopTerm = ( CFloRadSys( RadSysNum ).WaterInjectionRate / CFloRadSys( RadSysNum ).WaterMassFlowRate ) * Node( MainLoopNodeIn ).Temp + ( CFloRadSys( RadSysNum ).PumpHeattoFluid / ( CFloRadSys( RadSysNum ).WaterMassFlowRate * Cp ) );

						RecircTerm = ( CFloRadSys( RadSysNum ).WaterRecircRate / CFloRadSys( RadSysNum ).WaterMassFlowRate ) * SumFlowFracCkCm;

						TwiCoeff = 1.0 - ( CFloRadSys( RadSysNum ).WaterRecircRate / CFloRadSys( RadSysNum ).WaterMassFlowRate ) * SumFlowFracOneMinusCm;

						WaterTempIn = ( LoopTerm + RecircTerm ) / ( TwiCoeff );

						CFloRadSys( RadSysNum ).WaterInletTemp = WaterTempIn;

						for ( RadSurfNum2 = 1; RadSurfNum2 <= CFloRadSys( RadSysNum ).NumOfSurfaces; ++RadSurfNum2 ) {
							WaterTempOut( RadSurfNum2 ) = WaterTempIn * ( 1.0 - Cmj( RadSurfNum2 ) ) + ( Ckj( RadSurfNum2 ) * Cmj( RadSurfNum2 ) );
							Mdot = WaterMassFlow * CFloRadSys( RadSysNum ).SurfaceFlowFrac( RadSurfNum2 );
							SurfNum = CFloRadSys( RadSysNum ).SurfacePtr( RadSurfNum2 );
							QRadSysSource( SurfNum ) = Mdot * Cp * ( WaterTempIn - WaterTempOut( RadSurfNum2 ) );
							if ( Surface( SurfNum ).ExtBoundCond > 0 && Surface( SurfNum ).ExtBoundCond != SurfNum ) QRadSysSource( Surface( SurfNum ).ExtBoundCond ) = QRadSysSource( SurfNum ); // Also set the other side of an interzone
						}

					}

				}

			}

			for ( RadSurfNum = 1; RadSurfNum <= CFloRadSys( RadSysNum ).NumOfSurfaces; ++RadSurfNum ) {
				SurfNum = CFloRadSys( RadSysNum ).SurfacePtr( RadSurfNum );
				// "Temperature Comparison" Cut-off:
				// Check to see whether or not the system should really be running.  If
				// QRadSysSource is negative when we are in heating mode or QRadSysSource
				// is positive when we are in cooling mode, then the radiant system will
				// be doing the opposite of its intention.  In this case, the flow rate
				// is set to zero to avoid heating in cooling mode or cooling in heating
				// mode.
				if ( ( ( OperatingMode == HeatingMode ) && ( QRadSysSource( SurfNum ) <= 0.0 ) ) || ( ( OperatingMode == CoolingMode ) && ( QRadSysSource( SurfNum ) >= 0.0 ) ) ) {
					WaterMassFlow = 0.0;
					if ( OperatingMode == HeatingMode ) {
						SetComponentFlowRate( WaterMassFlow, CFloRadSys( RadSysNum ).HotWaterInNode, CFloRadSys( RadSysNum ).HotWaterOutNode, CFloRadSys( RadSysNum ).HWLoopNum, CFloRadSys( RadSysNum ).HWLoopSide, CFloRadSys( RadSysNum ).HWBranchNum, CFloRadSys( RadSysNum ).HWCompNum );
					} else if ( OperatingMode == CoolingMode ) {
						SetComponentFlowRate( WaterMassFlow, CFloRadSys( RadSysNum ).ColdWaterInNode, CFloRadSys( RadSysNum ).ColdWaterOutNode, CFloRadSys( RadSysNum ).CWLoopNum, CFloRadSys( RadSysNum ).CWLoopSide, CFloRadSys( RadSysNum ).CWBranchNum, CFloRadSys( RadSysNum ).CWCompNum );
					}
					CFloRadSys( RadSysNum ).WaterMassFlowRate = WaterMassFlow;
					for ( RadSurfNum2 = 1; RadSurfNum2 <= CFloRadSys( RadSysNum ).NumOfSurfaces; ++RadSurfNum2 ) {
						SurfNum2 = CFloRadSys( RadSysNum ).SurfacePtr( RadSurfNum2 );
						QRadSysSource( SurfNum2 ) = 0.0;
						if ( Surface( SurfNum2 ).ExtBoundCond > 0 && Surface( SurfNum2 ).ExtBoundCond != SurfNum2 ) QRadSysSource( Surface( SurfNum2 ).ExtBoundCond ) = 0.0; // Also zero the other side of an interzone
					}
					break; // outer do loop
				}
			}
			// Condensation Cut-off:
			// Check to see whether there are any surface temperatures within the radiant system that have
			// dropped below the dew-point temperature.  If so, we need to shut off this radiant system.
			// A safety parameter is added (hardwired parameter) to avoid getting too close to condensation
			// conditions.
			CFloRadSys( RadSysNum ).CondCausedShutDown = false;
			DewPointTemp = PsyTdpFnWPb( ZoneAirHumRat( CFloRadSys( RadSysNum ).ZonePtr ), OutBaroPress );

			if ( ( OperatingMode == CoolingMode ) && ( CFloRadSys( RadSysNum ).CondCtrlType == CondCtrlSimpleOff ) ) {

				for ( RadSurfNum2 = 1; RadSurfNum2 <= CFloRadSys( RadSysNum ).NumOfSurfaces; ++RadSurfNum2 ) {
					if ( TH( 2, 1, CFloRadSys( RadSysNum ).SurfacePtr( RadSurfNum2 ) ) < ( DewPointTemp + CFloRadSys( RadSysNum ).CondDewPtDeltaT ) ) {
						// Condensation warning--must shut off radiant system
						CFloRadSys( RadSysNum ).CondCausedShutDown = true;
						WaterMassFlow = 0.0;
						SetComponentFlowRate( WaterMassFlow, CFloRadSys( RadSysNum ).ColdWaterInNode, CFloRadSys( RadSysNum ).ColdWaterOutNode, CFloRadSys( RadSysNum ).CWLoopNum, CFloRadSys( RadSysNum ).CWLoopSide, CFloRadSys( RadSysNum ).CWBranchNum, CFloRadSys( RadSysNum ).CWCompNum );
						CFloRadSys( RadSysNum ).WaterMassFlowRate = WaterMassFlow;
						for ( RadSurfNum3 = 1; RadSurfNum3 <= CFloRadSys( RadSysNum ).NumOfSurfaces; ++RadSurfNum3 ) {
							SurfNum2 = CFloRadSys( RadSysNum ).SurfacePtr( RadSurfNum3 );
							QRadSysSource( SurfNum2 ) = 0.0;
							if ( Surface( SurfNum2 ).ExtBoundCond > 0 && Surface( SurfNum2 ).ExtBoundCond != SurfNum2 ) QRadSysSource( Surface( SurfNum2 ).ExtBoundCond ) = 0.0; // Also zero the other side of an interzone
						}
						// Produce a warning message so that user knows the system was shut-off due to potential for condensation
						if ( ! WarmupFlag ) {
							if ( CFloRadSys( RadSysNum ).CondErrIndex == 0 ) { // allow errors up to number of radiant systems
								ShowWarningMessage( cConstantFlowSystem + " [" + CFloRadSys( RadSysNum ).Name + ']' );
								ShowContinueError( "Surface [" + Surface( CFloRadSys( RadSysNum ).SurfacePtr( RadSurfNum2 ) ).Name + "] temperature below dew-point temperature--potential for condensation exists" );
								ShowContinueError( "Flow to the radiant system will be shut-off to avoid condensation" );
								ShowContinueError( "Predicted radiant system surface temperature = " + RoundSigDigits( TH( 2, 1, CFloRadSys( RadSysNum ).SurfacePtr( RadSurfNum2 ) ), 2 ) );
								ShowContinueError( "Zone dew-point temperature + safety delta T= " + RoundSigDigits( DewPointTemp + CFloRadSys( RadSysNum ).CondDewPtDeltaT, 2 ) );
								ShowContinueErrorTimeStamp( "" );
								ShowContinueError( "Note that a " + RoundSigDigits( CFloRadSys( RadSysNum ).CondDewPtDeltaT, 4 ) + " C safety was chosen in the input for the shut-off criteria" );
								ShowContinueError( "Note also that this affects all surfaces that are part of this radiant system" );
							}
							ShowRecurringWarningErrorAtEnd( cConstantFlowSystem + " [" + CFloRadSys( RadSysNum ).Name + "] condensation shut-off occurrence continues.", CFloRadSys( RadSysNum ).CondErrIndex, DewPointTemp, DewPointTemp, _, "C", "C" );
						}
						break; // outer do loop
					}
				}

			} else if ( ( OperatingMode == CoolingMode ) && ( CFloRadSys( RadSysNum ).CondCtrlType == CondCtrlNone ) ) {

				for ( RadSurfNum2 = 1; RadSurfNum2 <= CFloRadSys( RadSysNum ).NumOfSurfaces; ++RadSurfNum2 ) {
					if ( TH( 2, 1, CFloRadSys( RadSysNum ).SurfacePtr( RadSurfNum2 ) ) < DewPointTemp ) {
						// Condensation occurring but user does not want to shut radiant system off ever
						CFloRadSys( RadSysNum ).CondCausedShutDown = true;
					}
				}

			} else if ( ( OperatingMode == CoolingMode ) && ( CFloRadSys( RadSysNum ).CondCtrlType == CondCtrlVariedOff ) ) {

				for ( RadSurfNum2 = 1; RadSurfNum2 <= CFloRadSys( RadSysNum ).NumOfSurfaces; ++RadSurfNum2 ) {
					if ( TH( 2, 1, CFloRadSys( RadSysNum ).SurfacePtr( RadSurfNum2 ) ) < ( DewPointTemp + CFloRadSys( RadSysNum ).CondDewPtDeltaT ) ) {
						VarOffCond = true;
						if ( CFloCondIterNum >= 2 ) {
							// We have already iterated once so now we must shut off radiant system
							CFloRadSys( RadSysNum ).CondCausedShutDown = true;
							WaterMassFlow = 0.0;
							SetComponentFlowRate( WaterMassFlow, CFloRadSys( RadSysNum ).ColdWaterInNode, CFloRadSys( RadSysNum ).ColdWaterOutNode, CFloRadSys( RadSysNum ).CWLoopNum, CFloRadSys( RadSysNum ).CWLoopSide, CFloRadSys( RadSysNum ).CWBranchNum, CFloRadSys( RadSysNum ).CWCompNum );
							CFloRadSys( RadSysNum ).WaterMassFlowRate = WaterMassFlow;
							for ( RadSurfNum3 = 1; RadSurfNum3 <= CFloRadSys( RadSysNum ).NumOfSurfaces; ++RadSurfNum3 ) {
								SurfNum2 = CFloRadSys( RadSysNum ).SurfacePtr( RadSurfNum3 );
								QRadSysSource( SurfNum2 ) = 0.0;
								if ( Surface( SurfNum2 ).ExtBoundCond > 0 && Surface( SurfNum2 ).ExtBoundCond != SurfNum2 ) QRadSysSource( Surface( SurfNum2 ).ExtBoundCond ) = 0.0; // Also zero the other side of an interzone
							}
							// Produce a warning message so that user knows the system was shut-off due to potential for condensation
							if ( ! WarmupFlag ) {
								if ( CFloRadSys( RadSysNum ).CondErrIndex == 0 ) { // allow errors up to number of radiant systems
									ShowWarningMessage( cConstantFlowSystem + " [" + CFloRadSys( RadSysNum ).Name + ']' );
									ShowContinueError( "Surface [" + Surface( CFloRadSys( RadSysNum ).SurfacePtr( RadSurfNum2 ) ).Name + "] temperature below dew-point temperature--potential for condensation exists" );
									ShowContinueError( "Flow to the radiant system will be shut-off to avoid condensation" );
									ShowContinueError( "Predicted radiant system surface temperature = " + RoundSigDigits( TH( 2, 1, CFloRadSys( RadSysNum ).SurfacePtr( RadSurfNum2 ) ), 2 ) );
									ShowContinueError( "Zone dew-point temperature + safety delta T= " + RoundSigDigits( DewPointTemp + CFloRadSys( RadSysNum ).CondDewPtDeltaT, 2 ) );
									ShowContinueErrorTimeStamp( "" );
									ShowContinueError( "Note that a " + RoundSigDigits( CFloRadSys( RadSysNum ).CondDewPtDeltaT, 4 ) + " C safety was chosen in the input for the shut-off criteria" );
									ShowContinueError( "Note also that this affects all surfaces that are part of this radiant system" );
								}
								ShowRecurringWarningErrorAtEnd( cConstantFlowSystem + " [" + CFloRadSys( RadSysNum ).Name + "] condensation shut-off occurrence continues.", CFloRadSys( RadSysNum ).CondErrIndex, DewPointTemp, DewPointTemp, _, "C", "C" );
							}
							break; // outer do loop
						} else { // (First iteration--reset loop required temperature and try again to avoid condensation)
							LoopReqTemp = DewPointTemp + CFloRadSys( RadSysNum ).CondDewPtDeltaT;
						}
					}
				}

			}

			// Determine radiant system outlet temperature (two ways to calculate--use as a check)
			WaterOutletTempCheck = 0.0;
			TotalRadSysPower = 0.0;
			for ( RadSurfNum = 1; RadSurfNum <= CFloRadSys( RadSysNum ).NumOfSurfaces; ++RadSurfNum ) {
				SurfNum = CFloRadSys( RadSysNum ).SurfacePtr( RadSurfNum );
				TotalRadSysPower += QRadSysSource( SurfNum );
				WaterOutletTempCheck += ( CFloRadSys( RadSysNum ).SurfaceFlowFrac( RadSurfNum ) * WaterTempOut( RadSurfNum ) );
			}
			TotalRadSysPower *= ZoneMult;

			if ( CFloRadSys( RadSysNum ).WaterMassFlowRate > 0.0 ) {
				Cp = GetSpecificHeatGlycol( fluidNameWater, WaterTempIn, CFloRadSys( RadSysNum ).GlycolIndex, RoutineName );
				CFloRadSys( RadSysNum ).WaterOutletTemp = CFloRadSys( RadSysNum ).WaterInletTemp - ( TotalRadSysPower / ( CFloRadSys( RadSysNum ).WaterMassFlowRate * Cp ) );
				if ( ( std::abs( CFloRadSys( RadSysNum ).WaterOutletTemp - WaterOutletTempCheck ) > TempCheckLimit ) && ( std::abs( TotalRadSysPower ) > ZeroSystemResp ) ) {
					// If the total system power is zero, that means we have shut down and the temperatures won't match because of that
					ShowWarningError( "Radiant system water outlet temperature calculation mismatch--this should not happen" );
				}
			} else {
				CFloRadSys( RadSysNum ).WaterOutletTemp = CFloRadSys( RadSysNum ).WaterInletTemp;
			}

		}

		// Now that we have the source/sink term(s), we must redo the heat balances to obtain
		// the new SumHATsurf value for the zone.  Note that the difference between the new
		// SumHATsurf and the value originally calculated by the heat balance with a zero
		// source for all radiant systems in the zone is the load met by the system (approximately).
		HeatBalanceSurfaceManager::CalcHeatBalanceOutsideSurf( ZoneNum );
		HeatBalanceSurfaceManager::CalcHeatBalanceInsideSurf( ZoneNum );

		LoadMet = SumHATsurf( CFloRadSys( RadSysNum ).ZonePtr ) - ZeroSourceSumHATsurf( CFloRadSys( RadSysNum ).ZonePtr );

		//  DEALLOCATE(Ckj)
		//  DEALLOCATE(Cmj)
		//  DEALLOCATE(WaterTempOut)

	}

	void
	CalcLowTempElecRadiantSystem(
		int const RadSysNum, // name of the low temperature radiant system
		Real64 & LoadMet // load met by the radiant system, in Watts
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   November 2000
		//       MODIFIED       Sep 2011 LKL/BG - resimulate only zones needing it for Radiant systems
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine does all of the stuff that is necessary to simulate
		// a low temperature electric radiant heating system.  Calls are made to
		// appropriate subroutines either in this module or outside of it.

		// METHODOLOGY EMPLOYED:
		// Follows the methods used by many other pieces of zone equipment except
		// that we are controlling the electrical input to the building element's
		// resistance heating wires.  Note that cooling is not allowed for such
		// a system.

		// REFERENCES:
		// Other EnergyPlus modules
		// IBLAST-QTF research program, completed in January 1995 (unreleased)
		// Strand, R.K. 1995. "Heat Source Transfer Functions and Their Application to
		//   Low Temperature Radiant Heating Systems", Ph.D. dissertation, University
		//   of Illinois at Urbana-Champaign, Department of Mechanical and Industrial
		//   Engineering.
		// Seem, J.E. 1986. "Heat Transfer in Buildings", Ph.D. dissertation, University
		//   of Wisconsin-Madison.

		// USE STATEMENTS:
		//  USE DataEnvironment,   ONLY : OutDryBulbTemp, OutWetBulbTemp
		// Using/Aliasing
		using DataHeatBalance::MRT;
		using DataHeatBalance::Zone;
		using DataHeatBalance::ZoneData;
		using DataHeatBalFanSys::MAT;
		using DataHVACGlobals::SmallLoad;
		using namespace DataZoneEnergyDemands;
		using ScheduleManager::GetCurrentScheduleValue;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 ControlTemp; // Temperature of the parameter that is controlling the radiant system
		Real64 HeatFrac; // fraction of maximum electrical heat input to radiant system [dimensionless]
		Real64 OffTemp; // Temperature above which the radiant system should be completely off [C]
		int RadSurfNum; // number of surface that is the radiant system
		Real64 SetPtTemp; // Setpoint temperature [C]
		int SurfNum; // intermediate variable for surface number in Surface derived type
		int ZoneNum; // number of zone being served

		// FLOW:
		// initialize local variables
		ZoneNum = ElecRadSys( RadSysNum ).ZonePtr;
		HeatFrac = 0.0;

		if ( GetCurrentScheduleValue( ElecRadSys( RadSysNum ).SchedPtr ) <= 0.0 ) {

			// Unit is off; set the heat source terms to zero
			for ( RadSurfNum = 1; RadSurfNum <= ElecRadSys( RadSysNum ).NumOfSurfaces; ++RadSurfNum ) {
				SurfNum = ElecRadSys( RadSysNum ).SurfacePtr( RadSurfNum );
				QRadSysSource( SurfNum ) = 0.0;
				if ( Surface( SurfNum ).ExtBoundCond > 0 && Surface( SurfNum ).ExtBoundCond != SurfNum ) QRadSysSource( Surface( SurfNum ).ExtBoundCond ) = 0.0; // Also zero the other side of an interzone
			}

		} else { // Unit might be on-->this section is intended to determine whether the controls say
			// that the unit should be on or not

			// Determine the current setpoint temperature and the temperature at which the unit should be completely off
			SetPtTemp = GetCurrentScheduleValue( ElecRadSys( RadSysNum ).SetptSchedPtr );
			OffTemp = SetPtTemp + 0.5 * ElecRadSys( RadSysNum ).ThrottlRange;

			// Determine the control temperature--what the setpoint/offtemp is being compared to for unit operation
			{ auto const SELECT_CASE_var( ElecRadSys( RadSysNum ).ControlType );
			if ( SELECT_CASE_var == MATControl ) {
				ControlTemp = MAT( ZoneNum );
			} else if ( SELECT_CASE_var == MRTControl ) {
				ControlTemp = MRT( ZoneNum );
			} else if ( SELECT_CASE_var == OperativeControl ) {
				ControlTemp = ( MAT( ZoneNum ) + MRT( ZoneNum ) ) / 2.0;
			} else if ( SELECT_CASE_var == ODBControl ) {
				ControlTemp = Zone( ZoneNum ).OutDryBulbTemp;
			} else if ( SELECT_CASE_var == OWBControl ) {
				ControlTemp = Zone( ZoneNum ).OutWetBulbTemp;
			} else { // Should never get here
				ControlTemp = MAT( ZoneNum );
				ShowSevereError( "Illegal control type in low temperature radiant system: " + ElecRadSys( RadSysNum ).Name );
				ShowFatalError( "Preceding condition causes termination." );
			}}

			if ( ControlTemp < OffTemp ) { // HEATING MODE

				OperatingMode = HeatingMode;

				HeatFrac = ( OffTemp - ControlTemp ) / ElecRadSys( RadSysNum ).ThrottlRange;
				if ( HeatFrac < 0.0 ) HeatFrac = 0.0;
				if ( HeatFrac > 1.0 ) HeatFrac = 1.0;

				// Set the heat source for the low temperature electric radiant system
				for ( RadSurfNum = 1; RadSurfNum <= ElecRadSys( RadSysNum ).NumOfSurfaces; ++RadSurfNum ) {
					SurfNum = ElecRadSys( RadSysNum ).SurfacePtr( RadSurfNum );
					QRadSysSource( SurfNum ) = HeatFrac * ElecRadSys( RadSysNum ).MaxElecPower * ElecRadSys( RadSysNum ).SurfacePowerFrac( RadSurfNum );
					if ( Surface( SurfNum ).ExtBoundCond > 0 && Surface( SurfNum ).ExtBoundCond != SurfNum ) QRadSysSource( Surface( SurfNum ).ExtBoundCond ) = QRadSysSource( SurfNum ); // Also set the other side of an interzone
				}

				// Now "simulate" the system by recalculating the heat balances
				HeatBalanceSurfaceManager::CalcHeatBalanceOutsideSurf( ZoneNum );
				HeatBalanceSurfaceManager::CalcHeatBalanceInsideSurf( ZoneNum );

				LoadMet = SumHATsurf( ZoneNum ) - ZeroSourceSumHATsurf( ZoneNum );

			} else { //  OFF or COOLING MODE (not allowed for an electric low temperature radiant system), turn it off

				for ( RadSurfNum = 1; RadSurfNum <= ElecRadSys( RadSysNum ).NumOfSurfaces; ++RadSurfNum ) {
					SurfNum = ElecRadSys( RadSysNum ).SurfacePtr( RadSurfNum );
					QRadSysSource( SurfNum ) = 0.0;
					if ( Surface( SurfNum ).ExtBoundCond > 0 && Surface( SurfNum ).ExtBoundCond != SurfNum ) QRadSysSource( Surface( SurfNum ).ExtBoundCond ) = 0.0; // Also zero the other side of an interzone
				}

			}

		}

	}

	void
	UpdateLowTempRadiantSystem(
		bool const EP_UNUSED( FirstHVACIteration ), // TRUE if 1st HVAC simulation of system timestep
		int const RadSysNum, // Index for the low temperature radiant system under consideration within the derived types
		int const SystemType // Type of radiant system: hydronic, constant flow, or electric
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
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
		using FluidProperties::GetSpecificHeatGlycol;
		using DataPlant::PlantLoop;
		using PlantUtilities::SafeCopyPlantNode;
		using PlantUtilities::SetComponentFlowRate;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "UpdateLowTempRadiantSystem" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 BypassMassFlow; // Local bypass for a constant flow radiant system (could have recirculation and/or bypass)
		Real64 CpWater; // Specific heat of water
		int RadSurfNum; // DO loop counter for radiant surfaces in the system
		int SurfNum( 0 ); // Surface index number for the current radiant system
		int WaterInletNode; // Node number for the water side inlet of the radiant system
		Real64 TotalHeatSource; // Total heat source or sink for a particular radiant system (sum of all surface source/sinks)
		int TotRadSurfaces( 0 ); // Total number of radiant surfaces in this system
		Real64 WaterMassFlow; // Flow rate of water in the radiant system
		int WaterOutletNode; // Node number for the water side outlet of the radiant system
		Real64 ZoneMult; // Zone multiplier
		int ZoneNum; // Zone for this radiant system

		// FLOW:
		{ auto const SELECT_CASE_var( SystemType );
		if ( SELECT_CASE_var == HydronicSystem ) {
			TotRadSurfaces = HydrRadSys( RadSysNum ).NumOfSurfaces;
		} else if ( SELECT_CASE_var == ConstantFlowSystem ) {
			TotRadSurfaces = CFloRadSys( RadSysNum ).NumOfSurfaces;
		} else if ( SELECT_CASE_var == ElectricSystem ) {
			TotRadSurfaces = ElecRadSys( RadSysNum ).NumOfSurfaces;
		} else {
			assert( false );
		}}

		for ( RadSurfNum = 1; RadSurfNum <= TotRadSurfaces; ++RadSurfNum ) {

			{ auto const SELECT_CASE_var( SystemType );
			if ( SELECT_CASE_var == HydronicSystem ) {
				SurfNum = HydrRadSys( RadSysNum ).SurfacePtr( RadSurfNum );
			} else if ( SELECT_CASE_var == ConstantFlowSystem ) {
				SurfNum = CFloRadSys( RadSysNum ).SurfacePtr( RadSurfNum );
			} else if ( SELECT_CASE_var == ElectricSystem ) {
				SurfNum = ElecRadSys( RadSysNum ).SurfacePtr( RadSurfNum );
			} else {
				assert( false );
			}}

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

		// For a hydronic system, calculate the water side outlet conditions and set the
		// appropriate conditions on the correct HVAC node.
		if ( SystemType == HydronicSystem ) {

			// First sum up all of the heat sources/sinks associated with this system
			TotalHeatSource = 0.0;
			for ( RadSurfNum = 1; RadSurfNum <= HydrRadSys( RadSysNum ).NumOfSurfaces; ++RadSurfNum ) {
				SurfNum = HydrRadSys( RadSysNum ).SurfacePtr( RadSurfNum );
				TotalHeatSource += QRadSysSource( SurfNum );
			}
			ZoneNum = HydrRadSys( RadSysNum ).ZonePtr;
			ZoneMult = double( Zone( ZoneNum ).Multiplier * Zone( ZoneNum ).ListMultiplier );
			TotalHeatSource *= ZoneMult;

			// Update the heating side of things
			if ( HydrRadSys( RadSysNum ).HeatingSystem ) {

				WaterInletNode = HydrRadSys( RadSysNum ).HotWaterInNode;
				WaterOutletNode = HydrRadSys( RadSysNum ).HotWaterOutNode;
				WaterMassFlow = Node( WaterInletNode ).MassFlowRate;

				CpWater = GetSpecificHeatGlycol( PlantLoop( HydrRadSys( RadSysNum ).HWLoopNum ).FluidName, Node( WaterInletNode ).Temp, PlantLoop( HydrRadSys( RadSysNum ).HWLoopNum ).FluidIndex, RoutineName );

				{ auto const SELECT_CASE_var( OperatingMode );

				if ( SELECT_CASE_var == HeatingMode ) {
					if ( ( CpWater > 0.0 ) && ( WaterMassFlow > 0.0 ) ) {
						SafeCopyPlantNode( WaterInletNode, WaterOutletNode );
						// Node(WaterOutletNode) = Node(WaterInletNode) ! bad practice, e.g. wipes out setpoints on outlet
						Node( WaterOutletNode ).Temp = Node( WaterInletNode ).Temp - TotalHeatSource / WaterMassFlow / CpWater;
					} else {
						SafeCopyPlantNode( WaterInletNode, WaterOutletNode );
					}

				} else { // CoolingMode or not on
					SafeCopyPlantNode( WaterInletNode, WaterOutletNode );
				}}
				CheckForOutOfRangeTempResult( SystemType, RadSysNum, Node( WaterOutletNode ).Temp, Node( WaterInletNode ).Temp, WaterMassFlow );

			}

			if ( HydrRadSys( RadSysNum ).CoolingSystem ) {

				WaterInletNode = HydrRadSys( RadSysNum ).ColdWaterInNode;
				WaterOutletNode = HydrRadSys( RadSysNum ).ColdWaterOutNode;
				WaterMassFlow = Node( WaterInletNode ).MassFlowRate;

				CpWater = GetSpecificHeatGlycol( PlantLoop( HydrRadSys( RadSysNum ).CWLoopNum ).FluidName, Node( WaterInletNode ).Temp, PlantLoop( HydrRadSys( RadSysNum ).CWLoopNum ).FluidIndex, RoutineName );

				{ auto const SELECT_CASE_var( OperatingMode );

				if ( SELECT_CASE_var == CoolingMode ) {
					if ( ( CpWater > 0.0 ) && ( WaterMassFlow > 0.0 ) ) {
						SafeCopyPlantNode( WaterInletNode, WaterOutletNode );
						Node( WaterOutletNode ).Temp = Node( WaterInletNode ).Temp - TotalHeatSource / WaterMassFlow / CpWater;
					} else {
						SafeCopyPlantNode( WaterInletNode, WaterOutletNode );
					}

				} else { // HeatingMode or not on
					SafeCopyPlantNode( WaterInletNode, WaterOutletNode );
				}}
				CheckForOutOfRangeTempResult( SystemType, RadSysNum, Node( WaterOutletNode ).Temp, Node( WaterInletNode ).Temp, WaterMassFlow );
			}

		} // ...end of Hydronic System block

		// For a constant flow system, calculate the water side outlet conditions
		// and set the appropriate conditions on the correct HVAC node.  This may
		// require mixing if the main system does not provide all of the flow that
		// the local radiant system circulates.
		if ( SystemType == ConstantFlowSystem ) {

			// Update the heating side of things
			if ( CFloRadSys( RadSysNum ).HeatingSystem ) {

				WaterInletNode = CFloRadSys( RadSysNum ).HotWaterInNode;
				WaterOutletNode = CFloRadSys( RadSysNum ).HotWaterOutNode;
				CpWater = GetSpecificHeatGlycol( PlantLoop( CFloRadSys( RadSysNum ).HWLoopNum ).FluidName, Node( WaterInletNode ).Temp, PlantLoop( CFloRadSys( RadSysNum ).HWLoopNum ).FluidIndex, RoutineName );
				SafeCopyPlantNode( WaterInletNode, WaterOutletNode );

				if ( OperatingMode == HeatingMode ) {

					// Leave the inlet and outlet flow alone (if high enough) and perform a bypass if more flow than needed
					if ( Node( WaterInletNode ).MassFlowRate <= CFloRadSys( RadSysNum ).WaterInjectionRate ) {
						// Note that the water injection rate has already been restricted to the maximum available flow
						Node( WaterOutletNode ).Temp = CFloRadSys( RadSysNum ).WaterOutletTemp;
					} else {
						// Loop is providing more flow than needed so perform a local bypass and
						// mix the flows to obtain the proper outlet temperature.  In this case,
						// the mass flow rates on the loop are left alone and the outlet temperature
						// is calculated from a simple steady-steady, steady-flow energy balance.
						BypassMassFlow = Node( WaterInletNode ).MassFlowRate - CFloRadSys( RadSysNum ).WaterInjectionRate;
						Node( WaterOutletNode ).Temp = ( ( BypassMassFlow * Node( WaterInletNode ).Temp ) + ( CFloRadSys( RadSysNum ).WaterInjectionRate * CFloRadSys( RadSysNum ).WaterOutletTemp ) ) / ( Node( WaterOutletNode ).MassFlowRate );
					}
				}
				CheckForOutOfRangeTempResult( SystemType, RadSysNum, Node( WaterOutletNode ).Temp, Node( WaterInletNode ).Temp, Node( WaterOutletNode ).MassFlowRate );
			}

			if ( CFloRadSys( RadSysNum ).CoolingSystem ) {

				WaterInletNode = CFloRadSys( RadSysNum ).ColdWaterInNode;
				WaterOutletNode = CFloRadSys( RadSysNum ).ColdWaterOutNode;
				CpWater = GetSpecificHeatGlycol( fluidNameWater, Node( WaterInletNode ).Temp, CFloRadSys( RadSysNum ).GlycolIndex, RoutineName );

				SafeCopyPlantNode( WaterInletNode, WaterOutletNode );

				if ( OperatingMode == CoolingMode ) {

					if ( Node( WaterInletNode ).MassFlowRate <= CFloRadSys( RadSysNum ).WaterInjectionRate ) {
						// Note that the water injection rate has already been restricted to the maximum available flow

						Node( WaterOutletNode ).Temp = CFloRadSys( RadSysNum ).WaterOutletTemp;
					} else {
						// Loop is providing more flow than needed so perform a local bypass and
						// mix the flows to obtain the proper outlet temperature.  In this case,
						// the mass flow rates on the loop are left alone and the outlet temperature
						// is calculated from a simple steady-steady, steady-flow energy balance.
						BypassMassFlow = Node( WaterInletNode ).MassFlowRate - CFloRadSys( RadSysNum ).WaterInjectionRate;
						Node( WaterOutletNode ).Temp = ( ( BypassMassFlow * Node( WaterInletNode ).Temp ) + ( CFloRadSys( RadSysNum ).WaterInjectionRate * CFloRadSys( RadSysNum ).WaterOutletTemp ) ) / ( Node( WaterOutletNode ).MassFlowRate );
					}

					CheckForOutOfRangeTempResult( SystemType, RadSysNum, Node( WaterOutletNode ).Temp, Node( WaterInletNode ).Temp, Node( WaterOutletNode ).MassFlowRate );

				}

			}

		} // ...end of Constant Flow System block

		// Electric systems just burn electrical current and do not need to update nodes.

	}

	void
	CheckForOutOfRangeTempResult(
		int const SystemType,
		int const RadSysNum,
		Real64 const outletTemp,
		Real64 const inletTemp,
		Real64 const EP_UNUSED( mdot )
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   March 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// check for crazy, out of range temperature results for fluid leaving radiant system

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const UpperRangeLimit( 500.0 ); // high error trigger limit for when model is not working
		Real64 const LowerRangeLimit( -300.0 ); // Low error trigger limit for when model is not working

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool WarnTooLow( false );
		static bool WarnTooHigh( false );

		WarnTooLow = false;
		WarnTooHigh = false;
		if ( outletTemp < LowerRangeLimit ) {
			WarnTooLow = true;
		}

		if ( outletTemp > UpperRangeLimit ) {
			WarnTooHigh = true;
		}

		if ( WarnTooLow || WarnTooHigh ) {

			{ auto const SELECT_CASE_var( SystemType );
			if ( SELECT_CASE_var == HydronicSystem ) {
				if ( WarnTooLow ) {
					if ( HydrRadSys( RadSysNum ).OutRangeLoErrorCount == 0 ) {
						ShowSevereMessage( "UpdateLowTempRadiantSystem: model result for fluid outlet temperature is not physical." );
						ShowContinueError( "Occurs for radiant system name = " + HydrRadSys( RadSysNum ).Name );
						ShowContinueError( "Calculated radiant system outlet temperature = " + RoundSigDigits( outletTemp, 3 ) + " [C]" );
						ShowContinueError( "Radiant system inlet temperature = " + RoundSigDigits( inletTemp, 3 ) + " [C]" );
						ShowContinueError( "A possible cause is that the materials used in the internal source construction are not compatible with the model." );
					}
					ShowRecurringSevereErrorAtEnd( "UpdateLowTempRadiantSystem: Detected low out of range outlet temperature result for radiant system name =" + HydrRadSys( RadSysNum ).Name, HydrRadSys( RadSysNum ).OutRangeLoErrorCount, outletTemp, outletTemp );
				}
				if ( WarnTooHigh ) {
					if ( HydrRadSys( RadSysNum ).OutRangeHiErrorCount == 0 ) {
						ShowSevereMessage( "UpdateLowTempRadiantSystem: model result for fluid outlet temperature is not physical." );
						ShowContinueError( "Occurs for radiant system name = " + HydrRadSys( RadSysNum ).Name );
						ShowContinueError( "Calculated radiant system outlet temperature = " + RoundSigDigits( outletTemp, 3 ) + " [C]" );
						ShowContinueError( "Radiant system inlet temperature = " + RoundSigDigits( inletTemp, 3 ) + " [C]" );
						ShowContinueError( "A possible cause is that the materials used in the internal source construction are not compatible with the model." );

					}
					ShowRecurringSevereErrorAtEnd( "UpdateLowTempRadiantSystem: Detected high out of range outlet temperature result radiant system name =" + HydrRadSys( RadSysNum ).Name, HydrRadSys( RadSysNum ).OutRangeHiErrorCount, outletTemp, outletTemp );

				}

			} else if ( SELECT_CASE_var == ConstantFlowSystem ) {
				if ( WarnTooLow ) {

					if ( CFloRadSys( RadSysNum ).OutRangeLoErrorCount == 0 ) {
						ShowSevereMessage( "UpdateLowTempRadiantSystem: model result for fluid outlet temperature is not physical." );
						ShowContinueError( "Occurs for radiant system name = " + CFloRadSys( RadSysNum ).Name );
						ShowContinueError( "Calculated radiant system outlet temperature = " + RoundSigDigits( outletTemp, 3 ) + " [C]" );
						ShowContinueError( "Radiant system inlet temperature = " + RoundSigDigits( inletTemp, 3 ) + " [C]" );
						ShowContinueError( "A possible cause is that the materials used in the internal source construction are not compatible with the model." );

					}
					ShowRecurringSevereErrorAtEnd( "UpdateLowTempRadiantSystem: Detected high out of range temperature result for radiant system name =" + CFloRadSys( RadSysNum ).Name, CFloRadSys( RadSysNum ).OutRangeLoErrorCount, outletTemp, outletTemp );

				}
				if ( WarnTooHigh ) {
					if ( CFloRadSys( RadSysNum ).OutRangeHiErrorCount == 0 ) {
						ShowSevereMessage( "UpdateLowTempRadiantSystem: model result for fluid outlet temperature is not physical." );
						ShowContinueError( "Occurs for radiant system name = " + CFloRadSys( RadSysNum ).Name );
						ShowContinueError( "Calculated radiant system outlet temperature = " + RoundSigDigits( outletTemp, 3 ) + " [C]" );
						ShowContinueError( "Radiant system inlet temperature = " + RoundSigDigits( inletTemp, 3 ) + " [C]" );
						ShowContinueError( "A possible cause is that the materials used in the internal source construction are not compatible with the model." );

					}
					ShowRecurringSevereErrorAtEnd( "UpdateLowTempRadiantSystem: Detected high out of range temperature result for radiant system name =" + CFloRadSys( RadSysNum ).Name, CFloRadSys( RadSysNum ).OutRangeHiErrorCount, outletTemp, outletTemp );

				}
			}}

		}

	}

	Real64
	CalcRadSysHXEffectTerm(
		int const RadSysNum, // Index number of radiant system under consideration !unused1208
		int const SystemType, // Type of radiant system: hydronic, constant flow, or electric
		Real64 const Temperature, // Temperature of water entering the radiant system, in C
		Real64 const WaterMassFlow, // Mass flow rate of water in the radiant system, in kg/s
		Real64 const FlowFraction, // Mass flow rate fraction for this surface in the radiant system
		Real64 const NumCircs, // Number of fluid circuits in this surface
		Real64 const TubeLength, // Length of tubing in the radiant system, in m
		Real64 const TubeDiameter, // Inside diameter of the tubing in the radiant system, in m
		int & EP_UNUSED( GlycolIndex ) // Index for the fluid used in this radiant system
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   December 2000
		//       MODIFIED       na
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
		// Property data for water shown below as parameters taken from
		//   Incropera and DeWitt, Introduction to Heat Transfer, Table A.6.
		// Heat exchanger information also from Incropera and DeWitt.
		// Code based loosely on code from IBLAST program (research version)

		// Using/Aliasing
		using DataGlobals::Pi;
		using FluidProperties::GetSpecificHeatGlycol;
		using DataPlant::PlantLoop;

		// Return value
		Real64 CalcRadSysHXEffectTerm;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const MaxLaminarRe( 2300.0 ); // Maximum Reynolds number for laminar flow
		int const NumOfPropDivisions( 13 );
		Real64 const MaxExpPower( 50.0 ); // Maximum power after which EXP argument would be zero for DP variables
		static Array1D< Real64 > const Temps( NumOfPropDivisions, { 1.85, 6.85, 11.85, 16.85, 21.85, 26.85, 31.85, 36.85, 41.85, 46.85, 51.85, 56.85, 61.85 } ); // Temperature, in C
		static Array1D< Real64 > const Mu( NumOfPropDivisions, { 0.001652, 0.001422, 0.001225, 0.00108, 0.000959, 0.000855, 0.000769, 0.000695, 0.000631, 0.000577, 0.000528, 0.000489, 0.000453 } ); // Viscosity, in Ns/m2
		static Array1D< Real64 > const Conductivity( NumOfPropDivisions, { 0.574, 0.582, 0.590, 0.598, 0.606, 0.613, 0.620, 0.628, 0.634, 0.640, 0.645, 0.650, 0.656 } ); // Conductivity, in W/mK
		static Array1D< Real64 > const Pr( NumOfPropDivisions, { 12.22, 10.26, 8.81, 7.56, 6.62, 5.83, 5.20, 4.62, 4.16, 3.77, 3.42, 3.15, 2.88 } ); // Prandtl number (dimensionless)
		static std::string const RoutineName( "CalcRadSysHXEffectTerm" );

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
		Real64 CpWater( 0.0 );
		Real64 Kactual;
		Real64 MUactual;
		Real64 PRactual;
		Real64 Eff; // HX effectiveness

		// FLOW:
		// First find out where we are in the range of temperatures
		Index = 1;
		while ( Index <= NumOfPropDivisions ) {
			if ( Temperature < Temps( Index ) ) break; // DO loop
			++Index;
		}

		// Initialize thermal properties of water
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
		{ auto const SELECT_CASE_var( SystemType );
		if ( SELECT_CASE_var == HydronicSystem ) {
			{ auto const SELECT_CASE_var1( OperatingMode );
			if ( SELECT_CASE_var1 == HeatingMode ) {
				CpWater = GetSpecificHeatGlycol( PlantLoop( HydrRadSys( RadSysNum ).HWLoopNum ).FluidName, 60.0, PlantLoop( HydrRadSys( RadSysNum ).HWLoopNum ).FluidIndex, RoutineName );
			} else if ( SELECT_CASE_var1 == CoolingMode ) {
				CpWater = GetSpecificHeatGlycol( PlantLoop( HydrRadSys( RadSysNum ).CWLoopNum ).FluidName, Temperature, PlantLoop( HydrRadSys( RadSysNum ).CWLoopNum ).FluidIndex, RoutineName );
			} else {
				assert( false );
			}}
		} else if ( SELECT_CASE_var == ConstantFlowSystem ) {
			{ auto const SELECT_CASE_var1( OperatingMode );

			if ( SELECT_CASE_var1 == HeatingMode ) {
				CpWater = GetSpecificHeatGlycol( PlantLoop( CFloRadSys( RadSysNum ).HWLoopNum ).FluidName, Temperature, PlantLoop( CFloRadSys( RadSysNum ).HWLoopNum ).FluidIndex, RoutineName );
			} else if ( SELECT_CASE_var1 == CoolingMode ) {
				CpWater = GetSpecificHeatGlycol( PlantLoop( CFloRadSys( RadSysNum ).CWLoopNum ).FluidName, InitConvTemp, PlantLoop( CFloRadSys( RadSysNum ).CWLoopNum ).FluidIndex, RoutineName );
			} else {
				assert( false );
			}}
		} else {
			assert( false );
		}}

		// Calculate the Reynold's number from RE=(4*Mdot)/(Pi*Mu*Diameter)
		ReD = 4.0 * WaterMassFlow * FlowFraction / ( Pi * MUactual * TubeDiameter * NumCircs );

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
		NTU = Pi * Kactual * NuD * TubeLength / ( WaterMassFlow * CpWater ); // FlowFraction cancels out here

		// Calculate Epsilon*MassFlowRate*Cp
		if ( NTU > MaxExpPower ) {
			Eff = 1.0;
			CalcRadSysHXEffectTerm = FlowFraction * WaterMassFlow * CpWater;
		} else {
			Eff = 1.0 - std::exp( -NTU );
			CalcRadSysHXEffectTerm = Eff * FlowFraction * WaterMassFlow * CpWater;
		}

		return CalcRadSysHXEffectTerm;

	}

	void
	UpdateRadSysSourceValAvg( bool & LowTempRadSysOn ) // .TRUE. if the radiant system has run this zone time step
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   November 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// To transfer the average value of the heat source/sink over the entire
		// zone time step back to the heat balance routines so that the heat
		// balance algorithms can simulate one last time with the average source
		// to maintain some reasonable amount of continuity and energy balance
		// in the temperature and flux histories.

		// METHODOLOGY EMPLOYED:
		// All of the record keeping for the average term is done in the Update
		// routine so the only other thing that this subroutine does is check to
		// see if the system was even on.  If any average term is non-zero, then
		// one or more of the radiant systems was running.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const CloseEnough( 0.01 ); // Some arbitrarily small value to avoid zeros and numbers that are almost the same

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int SurfNum; // DO loop counter for surface index

		// FLOW:
		LowTempRadSysOn = false;

		// If this was never allocated, then there are no radiant systems in this input file (just RETURN)
		if ( ! allocated( QRadSysSrcAvg ) ) return;

		// If it was allocated, then we have to check to see if this was running at all...
		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
			if ( QRadSysSrcAvg( SurfNum ) != 0.0 ) {
				LowTempRadSysOn = true;
				break; //DO loop
			}
		}

		QRadSysSource = QRadSysSrcAvg;

		// For interzone surfaces, QRadSysSrcAvg was only updated for the "active" side.  The active side
		// would have a non-zero value at this point.  If the numbers differ, then we have to manually update.
		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
			if ( Surface( SurfNum ).ExtBoundCond > 0 && Surface( SurfNum ).ExtBoundCond != SurfNum ) {
				if ( std::abs( QRadSysSource( SurfNum ) - QRadSysSource( Surface( SurfNum ).ExtBoundCond ) ) > CloseEnough ) { // numbers differ
					if ( std::abs( QRadSysSource( SurfNum ) ) > std::abs( QRadSysSource( Surface( SurfNum ).ExtBoundCond ) ) ) {
						QRadSysSource( Surface( SurfNum ).ExtBoundCond ) = QRadSysSource( SurfNum );
					} else {
						QRadSysSource( SurfNum ) = QRadSysSource( Surface( SurfNum ).ExtBoundCond );
					}
				}
			}
		}

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
	ReportLowTempRadiantSystem(
		int const RadSysNum, // Index for the low temperature radiant system under consideration within the derived types
		int const SystemType // Type of radiant system: hydronic, constant flow, or electric
	)
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
		using FluidProperties::GetSpecificHeatGlycol;
		using DataPlant::PlantLoop;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "ReportLowTempRadiantSystem" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 CpFluid; // Specific heat of the fluid in the radiant system
		int RadSurfNum; // DO loop counter for radiant surfaces in the system
		int SurfNum; // Surface number (index) in Surface derived type
		Real64 TotalRadSysPower; // Total source/sink power for the radiant system (sum of all surfaces of the system)
		Real64 ZoneMult; // Total zone multiplier to apply to the system level variables

		// FLOW:
		TotalRadSysPower = 0.0;
		ZoneMult = 1.0;

		{ auto const SELECT_CASE_var( SystemType );

		if ( SELECT_CASE_var == HydronicSystem ) {

			for ( RadSurfNum = 1; RadSurfNum <= HydrRadSys( RadSysNum ).NumOfSurfaces; ++RadSurfNum ) {
				SurfNum = HydrRadSys( RadSysNum ).SurfacePtr( RadSurfNum );
				TotalRadSysPower += QRadSysSource( SurfNum );
			}
			ZoneMult = double( Zone( HydrRadSys( RadSysNum ).ZonePtr ).Multiplier * Zone( HydrRadSys( RadSysNum ).ZonePtr ).ListMultiplier );
			TotalRadSysPower *= ZoneMult;

			HydrRadSys( RadSysNum ).HeatPower = 0.0;
			HydrRadSys( RadSysNum ).CoolPower = 0.0;

			{ auto const SELECT_CASE_var1( OperatingMode );

			if ( SELECT_CASE_var1 == HeatingMode ) {
				HydrRadSys( RadSysNum ).WaterInletTemp = Node( HydrRadSys( RadSysNum ).HotWaterInNode ).Temp;
				HydrRadSys( RadSysNum ).WaterOutletTemp = Node( HydrRadSys( RadSysNum ).HotWaterOutNode ).Temp;
				HydrRadSys( RadSysNum ).WaterMassFlowRate = Node( HydrRadSys( RadSysNum ).HotWaterInNode ).MassFlowRate;
				HydrRadSys( RadSysNum ).HeatPower = TotalRadSysPower;

			} else if ( SELECT_CASE_var1 == CoolingMode ) {
				HydrRadSys( RadSysNum ).WaterInletTemp = Node( HydrRadSys( RadSysNum ).ColdWaterInNode ).Temp;
				HydrRadSys( RadSysNum ).WaterOutletTemp = Node( HydrRadSys( RadSysNum ).ColdWaterOutNode ).Temp;
				HydrRadSys( RadSysNum ).WaterMassFlowRate = Node( HydrRadSys( RadSysNum ).ColdWaterInNode ).MassFlowRate;
				HydrRadSys( RadSysNum ).CoolPower = -TotalRadSysPower;

			} else { // Not Operating: Leave temperatures at previous values
				HydrRadSys( RadSysNum ).WaterMassFlowRate = 0.0;
				HydrRadSys( RadSysNum ).WaterOutletTemp = HydrRadSys( RadSysNum ).WaterInletTemp;

			}}

			HydrRadSys( RadSysNum ).HeatEnergy = HydrRadSys( RadSysNum ).HeatPower * TimeStepSys * SecInHour;
			HydrRadSys( RadSysNum ).CoolEnergy = HydrRadSys( RadSysNum ).CoolPower * TimeStepSys * SecInHour;

			if ( HydrRadSys( RadSysNum ).CondCausedShutDown ) {
				HydrRadSys( RadSysNum ).CondCausedTimeOff = TimeStepSys * SecInHour;
			} else {
				HydrRadSys( RadSysNum ).CondCausedTimeOff = 0.0;
			}

		} else if ( SELECT_CASE_var == ConstantFlowSystem ) {

			for ( RadSurfNum = 1; RadSurfNum <= CFloRadSys( RadSysNum ).NumOfSurfaces; ++RadSurfNum ) {
				SurfNum = CFloRadSys( RadSysNum ).SurfacePtr( RadSurfNum );
				TotalRadSysPower += QRadSysSource( SurfNum );
			}
			ZoneMult = double( Zone( CFloRadSys( RadSysNum ).ZonePtr ).Multiplier * Zone( CFloRadSys( RadSysNum ).ZonePtr ).ListMultiplier );
			TotalRadSysPower *= ZoneMult;

			CFloRadSys( RadSysNum ).HeatPower = 0.0;
			CFloRadSys( RadSysNum ).CoolPower = 0.0;

			{ auto const SELECT_CASE_var1( OperatingMode );
			// Note that temperatures have already been set as part of the simulation
			// step.  So, they do not need to be calculated here except for the pump
			// inlet temperature which was not calculated elsewhere.  If the system is
			// not operating, leave the temperatures with their previous values but
			// zero out the flow and power quantities (should have already been done
			// in another routine, but just in case...).

			if ( SELECT_CASE_var1 == HeatingMode ) {
				CpFluid = GetSpecificHeatGlycol( PlantLoop( CFloRadSys( RadSysNum ).HWLoopNum ).FluidName, Node( CFloRadSys( RadSysNum ).HotWaterInNode ).Temp, PlantLoop( CFloRadSys( RadSysNum ).HWLoopNum ).FluidIndex, RoutineName );

				CFloRadSys( RadSysNum ).HeatPower = TotalRadSysPower;
				if ( CFloRadSys( RadSysNum ).PumpMassFlowRate > 0.0 ) {
					CFloRadSys( RadSysNum ).PumpInletTemp = CFloRadSys( RadSysNum ).WaterInletTemp - ( CFloRadSys( RadSysNum ).PumpHeattoFluid / ( CFloRadSys( RadSysNum ).PumpMassFlowRate * CpFluid ) );
				} else {
					CFloRadSys( RadSysNum ).PumpInletTemp = CFloRadSys( RadSysNum ).WaterInletTemp;
				}

			} else if ( SELECT_CASE_var1 == CoolingMode ) {
				CpFluid = GetSpecificHeatGlycol( PlantLoop( CFloRadSys( RadSysNum ).CWLoopNum ).FluidName, Node( CFloRadSys( RadSysNum ).ColdWaterInNode ).Temp, PlantLoop( CFloRadSys( RadSysNum ).CWLoopNum ).FluidIndex, RoutineName );

				CFloRadSys( RadSysNum ).CoolPower = -TotalRadSysPower;
				CFloRadSys( RadSysNum ).PumpInletTemp = CFloRadSys( RadSysNum ).WaterInletTemp - ( CFloRadSys( RadSysNum ).PumpHeattoFluid / ( CFloRadSys( RadSysNum ).PumpMassFlowRate * CpFluid ) );

			} else { // Not Operating
				CFloRadSys( RadSysNum ).WaterOutletTemp = CFloRadSys( RadSysNum ).WaterInletTemp;
				CFloRadSys( RadSysNum ).PumpInletTemp = CFloRadSys( RadSysNum ).WaterInletTemp;
				CFloRadSys( RadSysNum ).WaterMassFlowRate = 0.0;
				CFloRadSys( RadSysNum ).WaterInjectionRate = 0.0;
				CFloRadSys( RadSysNum ).WaterRecircRate = 0.0;
				CFloRadSys( RadSysNum ).HeatPower = 0.0;
				CFloRadSys( RadSysNum ).CoolPower = 0.0;
				CFloRadSys( RadSysNum ).PumpPower = 0.0;
				CFloRadSys( RadSysNum ).PumpMassFlowRate = 0.0;
				CFloRadSys( RadSysNum ).PumpHeattoFluid = 0.0;

			}}

			CFloRadSys( RadSysNum ).HeatEnergy = CFloRadSys( RadSysNum ).HeatPower * TimeStepSys * SecInHour;
			CFloRadSys( RadSysNum ).CoolEnergy = CFloRadSys( RadSysNum ).CoolPower * TimeStepSys * SecInHour;
			CFloRadSys( RadSysNum ).PumpEnergy = CFloRadSys( RadSysNum ).PumpPower * TimeStepSys * SecInHour;
			CFloRadSys( RadSysNum ).PumpHeattoFluidEnergy = CFloRadSys( RadSysNum ).PumpHeattoFluid * TimeStepSys * SecInHour;

			if ( CFloRadSys( RadSysNum ).CondCausedShutDown ) {
				CFloRadSys( RadSysNum ).CondCausedTimeOff = TimeStepSys * SecInHour;
			} else {
				CFloRadSys( RadSysNum ).CondCausedTimeOff = 0.0;
			}

		} else if ( SELECT_CASE_var == ElectricSystem ) {

			for ( RadSurfNum = 1; RadSurfNum <= ElecRadSys( RadSysNum ).NumOfSurfaces; ++RadSurfNum ) {
				SurfNum = ElecRadSys( RadSysNum ).SurfacePtr( RadSurfNum );
				TotalRadSysPower += QRadSysSource( SurfNum );
			}
			ZoneMult = double( Zone( ElecRadSys( RadSysNum ).ZonePtr ).Multiplier * Zone( ElecRadSys( RadSysNum ).ZonePtr ).ListMultiplier );
			TotalRadSysPower *= ZoneMult;

			ElecRadSys( RadSysNum ).ElecPower = TotalRadSysPower;
			ElecRadSys( RadSysNum ).ElecEnergy = ElecRadSys( RadSysNum ).ElecPower * TimeStepSys * SecInHour;
			ElecRadSys( RadSysNum ).HeatPower = ElecRadSys( RadSysNum ).ElecPower;
			ElecRadSys( RadSysNum ).HeatEnergy = ElecRadSys( RadSysNum ).ElecEnergy;

		}}

	}

} // LowTempRadiantSystem

} // EnergyPlus
