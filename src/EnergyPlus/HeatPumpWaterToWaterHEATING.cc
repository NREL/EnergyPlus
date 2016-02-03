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
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <HeatPumpWaterToWaterHEATING.hh>
#include <BranchNodeConnections.hh>
#include <DataBranchAirLoopPlant.hh>
#include <DataEnvironment.hh>
#include <DataHVACGlobals.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <PlantLocation.hh>
#include <PlantUtilities.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace HeatPumpWaterToWaterHEATING {
	// Module containing the routines dealing with the Water to Water Heat Pump (Heating)

	// MODULE INFORMATION:
	//       AUTHOR         ARUN
	//       DATE WRITTEN   7/18/2000
	//       MODIFIED       ARUN: 6/27/2002: Cycle Time
	//                      L Lawrie: V1.1.1 (5/20/2003) add meters and energy to several reporting variables
	//                      L Lawrie: V1.1.1 (5/20/2003) restructure modules to comply with standard templates
	//                      B. Griffith, Sept 2010, plant upgrades, generalize fluid properties
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This module simulates a water to Water Heat Pump (Heating)

	// METHODOLOGY EMPLOYED:
	// This simulation is based on a set of selected parameters,
	// Which are obtained using Parameter Estimation technique.

	// REFERENCES: none

	// OTHER NOTES: none

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::BeginSimFlag;
	using DataGlobals::InitConvTemp;
	using DataGlobals::BeginEnvrnFlag;
	using DataGlobals::HourOfDay;
	using DataGlobals::KelvinConv;
	using DataGlobals::TimeStep;
	using DataGlobals::TimeStepZone;
	using DataGlobals::DayOfSim;
	using DataGlobals::WarmupFlag;
	using DataGlobals::SecInHour;
	using namespace DataLoopNode;

	// Use statements for access to subroutines in other modules

	// Data
	// MODULE PARAMETER DEFINITIONS
	std::string const ModuleCompName( "HeatPump:WaterToWater:ParameterEstimation:Heating" );
	std::string const ModuleCompNameUC( "HEATPUMP:WATERTOWATER:PARAMETERESTIMATION:HEATING" );
	bool GshpSpecs::GetInputFlag(true);

	// DERIVED TYPE DEFINITIONS

	// Type Description of Heat Pump

	// Output Variables Type definition

	// MODULE VARIABLE DECLARATIONS:

	std::string GSHPRefrigerant( "R22" ); // Refrigerent name and index
	int GSHPRefrigIndex( 0 );

	int NumGSHPs( 0 ); // number of Gshps specified in input
	Real64 LoadSideWaterMassFlowRate( 0.0 ); // Load Side mass flow rate, water side Kg/s
	Real64 SourceSideWaterMassFlowRate( 0.0 ); // Source Side mass flow rate, water side Kg/s
	Real64 Power( 0.0 ); // power consumption Watts Joules/sec
	Real64 QLoad( 0.0 ); // heat rejection from Load Side coil Joules
	Real64 QSource( 0.0 ); // cooling capacity Joules
	Real64 SourceSideWaterOutletTemp( 0.0 ); // Source Side outlet temperature °C
	Real64 SourceSideWaterInletTemp( 0.0 ); // Source Side outlet temperature °C
	Real64 LoadSideWaterOutletTemp( 0.0 ); // Source Side outlet temperature °C
	Real64 LoadSideWaterInletTemp( 0.0 ); // Source Side outlet temperature °C
	Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE

	// Name Public routines, optionally name Private routines within this module

	// Object Data
	Array1D< GshpSpecs > GSHP; // dimension to number of machines
	Array1D< ReportVars > GSHPReport;

	// MODULE SUBROUTINES:

	void
	GshpSpecs::InitGshp() // GSHP number
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Dan Fisher
		//       DATE WRITTEN:    July 2007

		// PURPOSE OF THIS SUBROUTINE:
		// initialization

		// METHODOLOGY EMPLOYED: na

		// REFERENCES: na

		// Using/Aliasing
		using DataPlant::PlantLoop;
		using FluidProperties::GetDensityGlycol;
		using PlantUtilities::InitComponentNodes;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "InitGshp" );

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool MyOneTimeFlag( true );
		Real64 rho; // local fluid density

		if ( MyOneTimeFlag ) {
			MyOneTimeFlag = false;
		}

		//For each new environment
		if ( BeginEnvrnFlag && this->MyEnvrnFlag ) {
			this->QLoad = 0.0;
			this->QSource = 0.0;
			this->Power = 0.0;
			this->QLoadEnergy = 0.0;
			this->QSourceEnergy = 0.0;
			this->Energy = 0.0;
			this->LoadSideWaterInletTemp = 0.0;
			this->SourceSideWaterInletTemp = 0.0;
			this->LoadSideWaterOutletTemp = 0.0;
			this->SourceSideWaterOutletTemp = 0.0;
			this->SourceSidemdot = 0.0;
			this->LoadSidemdot = 0.0;
			this->IsOn = false;
			this->MustRun = true;

			this->MyEnvrnFlag = false;

			rho = GetDensityGlycol( PlantLoop( this->LoadLoopNum ).FluidName, InitConvTemp, PlantLoop( this->LoadLoopNum ).FluidIndex, RoutineName );
			this->LoadSideDesignMassFlow = this->LoadSideVolFlowRate * rho;

			InitComponentNodes( 0.0, this->LoadSideDesignMassFlow, this->LoadSideInletNodeNum, this->LoadSideOutletNodeNum, this->LoadLoopNum, this->LoadLoopSideNum, this->LoadBranchNum, this->LoadCompNum );

			rho = GetDensityGlycol( PlantLoop( this->SourceLoopNum ).FluidName, InitConvTemp, PlantLoop( this->SourceLoopNum ).FluidIndex, RoutineName );
			this->SourceSideDesignMassFlow = this->SourceSideVolFlowRate * rho;

			InitComponentNodes( 0.0, this->SourceSideDesignMassFlow, this->SourceSideInletNodeNum, this->SourceSideOutletNodeNum, this->SourceLoopNum, this->SourceLoopSideNum, this->SourceBranchNum, this->SourceCompNum );

			if ( Node( this->SourceSideOutletNodeNum ).TempSetPoint == SensedNodeFlagValue ) Node( this->SourceSideOutletNodeNum ).TempSetPoint = 0.0;
			Node( this->SourceSideInletNodeNum ).Temp = Node( this->SourceSideOutletNodeNum ).TempSetPoint + 30;

		}

		if ( ! BeginEnvrnFlag ) this->MyEnvrnFlag = true;

		//On every call
		this->Running = 0;

		this->MustRun = true; // Reset MustRun Flag to TRUE

		LoadSideWaterMassFlowRate = 0.0; // Load Side mass flow rate, water side
		SourceSideWaterMassFlowRate = 0.0; // Source Side mass flow rate, water side
		Power = 0.0; // power consumption
		QLoad = 0.0; // heat rejection from Load Side coil
		QSource = 0.0;

	}

	void
	GshpSpecs::CalcGshpModel(
		Real64 & MyLoad, // Operating Load
		bool const EP_UNUSED( FirstHVACIteration )
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR
		//       DATE WRITTEN   Sept. 1998
		//       MODIFIED       April 1999
		//                      September 2002, SJR
		//       RE-ENGINEERED  Mar2000

		// PURPOSE OF THIS SUBROUTINE: This routine performs

		// METHODOLOGY EMPLOYED: under development

		// REFERENCES:

		// Using/Aliasing
		using DataHVACGlobals::SysTimeElapsed;
		using namespace FluidProperties;
		using General::TrimSigDigits;
		using DataPlant::PlantLoop;
		using DataBranchAirLoopPlant::MassFlowTolerance;
		using PlantUtilities::SetComponentFlowRate;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const gamma( 1.114 ); // Expnasion Coefficient
		Real64 const HeatBalTol( 0.0005 );
		Real64 const RelaxParam( 0.6 );
		Real64 const SmallNum( 1.0e-20 );
		int const IterationLimit( 500 );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 SourceSideEffect; // Source Side effectiveness
		Real64 LoadSideEffect; // Load Side effectiveness
		Real64 SourceSideTemp; // Source Side temperature °C
		Real64 LoadSideTemp; // Load Side temperature °C
		Real64 SourceSideUA; // Source Side heat transfer coefficient    w/k
		Real64 LoadSideUA; // Load Side heat transfer coefficient W/k
		Real64 SourceSidePressure; // Source Side pressure Pascals
		Real64 LoadSidePressure; // Load Side pressure Pascals
		Real64 SuctionPr; // Suction Pressure  pascals
		Real64 DischargePr; // Discharge Pressure pascals
		Real64 CompressInletTemp; // Compressor inlet temperature  °C
		Real64 PressureDrop; // Suction Pressure drop °C
		Real64 ClearanceFactor; // Clearance factor
		Real64 PistonDisp; // Compressor piston displacement  m3
		Real64 ShTemp; // Superheat temperature °C
		Real64 LosFac; // Loss factor used to define the electromechanical loss for compressor
		Real64 MassRef; // mass flow rate of refrigerant Kg/s
		Real64 SourceSideOutletEnth; // Enthalpy at Source Side pressure Joules
		Real64 LoadSideOutletEnth; // Enthalpy at Condensor Pressure  Joules
		Real64 initialQSource; // Guess Source Side Heat rate Joules
		Real64 initialQLoad; // Guess Load Side Heat rate Joules
		Real64 qual; // quality
		Real64 SuperHeatEnth;
		Real64 T110;
		Real64 T111;
		Real64 CompSuctionTemp;
		Real64 CompSuctionEnth;
		Real64 CompSuctionDensity;
		Real64 PowerLosses;
		Real64 CompSuctionSatTemp;
		Real64 HighPressCutoff;
		Real64 LowPressCutoff;
		std::string ErrString;
		Real64 DutyFactor;
		int IterationCount;

		static Real64 CurrentSimTime( 0.0 );
		static Real64 PrevSimTime( 0.0 );
		static bool OneTimeFlag( true );
		// Nodes
		int SourceSideInletNode; // Source Side inlet node number, water side
		int SourceSideOutletNode; // Source Side outlet node number, water side
		int LoadSideInletNode; // Load Side inlet node number, water side
		int LoadSideOutletNode; // Load Side outlet node number, water side
		int LoopNum;
		int LoopSideNum;
		Real64 CpSourceSide; // local temporary for fluid specific heat
		Real64 CpLoadSide; // local temporary for fluid specific heat

		//  LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
		PressureDrop = this->CompSucPressDrop;
		ClearanceFactor = this->CompClearanceFactor;
		PistonDisp = this->CompPistonDisp;
		ShTemp = this->SuperheatTemp;
		LosFac = this->LossFactor;
		SourceSideUA = this->SourceSideUACoeff;
		LoadSideUA = this->LoadSideUACoeff;
		PowerLosses = this->PowerLosses;
		HighPressCutoff = this->HighPressCutoff;
		LowPressCutoff = this->LowPressCutoff;
		// REPORT VAR
		this->Running = 0;

		// Init Module level Variables
		this->MustRun = true; // Reset MustRun Flag to TRUE
		LoadSideWaterMassFlowRate = 0.0; // Load Side mass flow rate, water side
		SourceSideWaterMassFlowRate = 0.0; // Source Side mass flow rate, water side
		Power = 0.0; // power consumption
		QLoad = 0.0; // heat rejection from Load Side coil
		QSource = 0.0;

		LoadSideInletNode = this->LoadSideInletNodeNum;
		LoadSideOutletNode = this->LoadSideOutletNodeNum;
		SourceSideInletNode = this->SourceSideInletNodeNum;
		SourceSideOutletNode = this->SourceSideOutletNodeNum;
		LoopNum = this->LoadLoopNum;
		LoopSideNum = this->LoadLoopSideNum;

		if ( PrevSimTime != CurrentSimTime ) {
			PrevSimTime = CurrentSimTime;
		}

		// CALCULATE THE SIMULATION TIME
		CurrentSimTime = ( DayOfSim - 1 ) * 24 + HourOfDay - 1 + ( TimeStep - 1 ) * TimeStepZone + SysTimeElapsed;

		// initialize event time array when the environment simulation begins
		if ( CurrentSimTime == 0.0 && OneTimeFlag ) {
			OneTimeFlag = false;
		}

		if ( CurrentSimTime > 0.0 ) OneTimeFlag = true;

		if ( MyLoad > 0.0 ) {
			this->MustRun = true;
			this->IsOn = true;
		} else {
			this->MustRun = false;
			this->IsOn = false;
		}

		//*******Set flow based on "run" flags**********
		// Set flows if the heat pump is not running
		if ( ! this->MustRun ) {
			LoadSideWaterMassFlowRate = 0.0;
			SetComponentFlowRate( LoadSideWaterMassFlowRate, LoadSideInletNode, LoadSideOutletNode, this->LoadLoopNum, this->LoadLoopSideNum, this->LoadBranchNum, this->LoadCompNum );
			SourceSideWaterMassFlowRate = 0.0;
			SetComponentFlowRate( SourceSideWaterMassFlowRate, SourceSideInletNode, SourceSideOutletNode, this->SourceLoopNum, this->SourceLoopSideNum, this->SourceBranchNum, this->SourceCompNum );
			PlantUtilities::PullCompInterconnectTrigger( this->LoadLoopNum, this->LoadLoopSideNum, this->LoadBranchNum, this->LoadCompNum , this->CondMassFlowIndex, this->SourceLoopNum, this->LoadLoopSideNum, DataPlant::CriteriaType_MassFlowRate, SourceSideWaterMassFlowRate );
			//now initialize simulation variables for "heat pump off"
			QLoad = 0.0;
			QSource = 0.0;
			Power = 0.0;
			LoadSideWaterInletTemp = Node( LoadSideInletNode ).Temp;
			LoadSideWaterOutletTemp = LoadSideWaterInletTemp;
			SourceSideWaterInletTemp = Node( SourceSideInletNode ).Temp;
			SourceSideWaterOutletTemp = SourceSideWaterInletTemp;
			return; //if heat pump is not running return without simulation

			// Set flows if the heat pump is running
		} else { // the heat pump must run, request design flow

			LoadSideWaterMassFlowRate = this->LoadSideDesignMassFlow;
			SetComponentFlowRate( LoadSideWaterMassFlowRate, LoadSideInletNode, LoadSideOutletNode, this->LoadLoopNum, this->LoadLoopSideNum, this->LoadBranchNum, this->LoadCompNum );

			SourceSideWaterMassFlowRate = this->SourceSideDesignMassFlow;
			SetComponentFlowRate( SourceSideWaterMassFlowRate, SourceSideInletNode, SourceSideOutletNode, this->SourceLoopNum, this->SourceLoopSideNum, this->SourceBranchNum, this->SourceCompNum );
			// get inlet temps
			LoadSideWaterInletTemp = Node( LoadSideInletNode ).Temp;
			SourceSideWaterInletTemp = Node( SourceSideInletNode ).Temp;
			//if there's no flow, turn the "heat pump off"
			if ( LoadSideWaterMassFlowRate < MassFlowTolerance || SourceSideWaterMassFlowRate < MassFlowTolerance ) {
				LoadSideWaterMassFlowRate = 0.0;
				SetComponentFlowRate( LoadSideWaterMassFlowRate, LoadSideInletNode, LoadSideOutletNode, this->LoadLoopNum, this->LoadLoopSideNum, this->LoadBranchNum, this->LoadCompNum );
				SourceSideWaterMassFlowRate = 0.0;
				SetComponentFlowRate( SourceSideWaterMassFlowRate, SourceSideInletNode, SourceSideOutletNode, this->SourceLoopNum, this->SourceLoopSideNum, this->SourceBranchNum, this->SourceCompNum );
				PlantUtilities::PullCompInterconnectTrigger( this->LoadLoopNum, this->LoadLoopSideNum, this->LoadBranchNum, this->LoadCompNum , this->CondMassFlowIndex, this->SourceLoopNum, this->LoadLoopSideNum, DataPlant::CriteriaType_MassFlowRate, SourceSideWaterMassFlowRate );
				QLoad = 0.0;
				QSource = 0.0;
				Power = 0.0;
				LoadSideWaterInletTemp = Node( LoadSideInletNode ).Temp;
				LoadSideWaterOutletTemp = LoadSideWaterInletTemp;
				SourceSideWaterInletTemp = Node( SourceSideInletNode ).Temp;
				SourceSideWaterOutletTemp = SourceSideWaterInletTemp;
				return;
			}
			PlantUtilities::PullCompInterconnectTrigger( this->LoadLoopNum, this->LoadLoopSideNum, this->LoadBranchNum, this->LoadCompNum , this->CondMassFlowIndex, this->SourceLoopNum, this->LoadLoopSideNum, DataPlant::CriteriaType_MassFlowRate, SourceSideWaterMassFlowRate );
		}

		//***********BEGIN CALCULATION****************
		// initialize the source and load side heat transfer rates for the simulation
		initialQSource = 0.0;
		initialQLoad = 0.0;
		IterationCount = 0;

		CpSourceSide = GetSpecificHeatGlycol( PlantLoop( this->SourceLoopNum ).FluidName, SourceSideWaterInletTemp, PlantLoop( this->SourceLoopNum ).FluidIndex, RoutineName );

		CpLoadSide = GetSpecificHeatGlycol( PlantLoop( this->LoadLoopNum ).FluidName, LoadSideWaterInletTemp, PlantLoop( this->LoadLoopNum ).FluidIndex, RoutineName );

		// Determine effectiveness of Source Side (the Evaporator in heating mode)
		SourceSideEffect = 1.0 - std::exp( -SourceSideUA / ( CpSourceSide * SourceSideWaterMassFlowRate ) );
		//Determine effectiveness of Load Side the condenser in heating mode
		LoadSideEffect = 1.0 - std::exp( -LoadSideUA / ( CpLoadSide * LoadSideWaterMassFlowRate ) );

		while ( true ) { // main loop to solve model equations
			++IterationCount;
			// Determine Source Side tempertaure
			SourceSideTemp = SourceSideWaterInletTemp - initialQSource / ( SourceSideEffect * CpSourceSide * SourceSideWaterMassFlowRate );

			// To determine Load Side temperature condenser
			LoadSideTemp = LoadSideWaterInletTemp + initialQLoad / ( LoadSideEffect * CpLoadSide * LoadSideWaterMassFlowRate );

			// Determine the evaporating and condensing pressures
			SourceSidePressure = GetSatPressureRefrig( GSHPRefrigerant, SourceSideTemp, GSHPRefrigIndex, RoutineNameSourceSideTemp );
			LoadSidePressure = GetSatPressureRefrig( GSHPRefrigerant, LoadSideTemp, GSHPRefrigIndex, RoutineNameLoadSideTemp );

			// check cutoff pressures
			if ( SourceSidePressure < LowPressCutoff ) {
				ShowSevereError( ModuleCompName + "=\"" + Name + "\" Heating Source Side Pressure Less than the Design Minimum" );
				ShowContinueError( "Source Side Pressure=" + TrimSigDigits( SourceSidePressure, 2 ) + " and user specified Design Minimum Pressure=" + TrimSigDigits( LowPressCutoff, 2 ) );
				ShowFatalError( "Preceding Conditions cause termination." );
			}
			if ( LoadSidePressure > HighPressCutoff ) {
				ShowSevereError( ModuleCompName + "=\"" + Name + "\" Heating Load Side Pressure greater than the Design Maximum" );
				ShowContinueError( "Load Side Pressure=" + TrimSigDigits( LoadSidePressure, 2 ) + " and user specified Design Maximum Pressure=" + TrimSigDigits( HighPressCutoff, 2 ) );
				ShowFatalError( "Preceding Conditions cause termination." );
			}

			// Determine Suction Pressure at compressor inlet
			SuctionPr = SourceSidePressure - PressureDrop;
			// Determine Discharge Pressure at compressor exit
			DischargePr = LoadSidePressure + PressureDrop;
			// check cutoff pressures
			if ( SuctionPr < LowPressCutoff ) {
				ShowSevereError( ModuleCompName + "=\"" + Name + "\" Heating Suction Pressure Less than the Design Minimum" );
				ShowContinueError( "Heating Suction Pressure=" + TrimSigDigits( SuctionPr, 2 ) + " and user specified Design Minimum Pressure=" + TrimSigDigits( LowPressCutoff, 2 ) );
				ShowFatalError( "Preceding Conditions cause termination." );
			}
			if ( DischargePr > HighPressCutoff ) {
				ShowSevereError( ModuleCompName + "=\"" + Name + "\" Heating Discharge Pressure greater than the Design Maximum" );
				ShowContinueError( "Heating Discharge Pressure=" + TrimSigDigits( DischargePr, 2 ) + " and user specified Design Maximum Pressure=" + TrimSigDigits( HighPressCutoff, 2 ) );
				ShowFatalError( "Preceding Conditions cause termination." );
			}

			// Determine the Source Side Outlet Enthalpy
			qual = 1.0;
			SourceSideOutletEnth = GetSatEnthalpyRefrig( GSHPRefrigerant, SourceSideTemp, qual, GSHPRefrigIndex, RoutineNameSourceSideTemp );

			// Determine Load Side Outlet Enthalpy
			qual = 0.0;
			LoadSideOutletEnth = GetSatEnthalpyRefrig( GSHPRefrigerant, LoadSideTemp, qual, GSHPRefrigIndex, RoutineNameLoadSideTemp );

			// Determine superheated temperature of the Source Side outlet/compressor inlet
			CompressInletTemp = SourceSideTemp + ShTemp;
			// Determine the enathalpy of the super heated fluid at Source Side outlet
			SuperHeatEnth = GetSupHeatEnthalpyRefrig( GSHPRefrigerant, CompressInletTemp, SourceSidePressure, GSHPRefrigIndex, RoutineNameCompressInletTemp );

			// Determining the suction state of the fluid from inlet state involves interation
			// Method employed...
			// Determine the saturated temp at suction pressure, shoot out into the superheated region find the enthalpy
			// check that with the inlet enthalpy ( as suction loss is isenthalpic). Iterate till desired accuracy is reached

			CompSuctionSatTemp = GetSatTemperatureRefrig( GSHPRefrigerant, SuctionPr, GSHPRefrigIndex, RoutineNameSuctionPr );

			T110 = CompSuctionSatTemp;
			//Shoot into the super heated region
			T111 = CompSuctionSatTemp + 80;

			// Iterate to find the Suction State - given suction pressure and superheat enthalpy
			while ( true ) {
				CompSuctionTemp = 0.5 * ( T110 + T111 );

				CompSuctionEnth = GetSupHeatEnthalpyRefrig( GSHPRefrigerant, CompSuctionTemp, SuctionPr, GSHPRefrigIndex, RoutineNameCompSuctionTemp );
				if ( std::abs( CompSuctionEnth - SuperHeatEnth ) / SuperHeatEnth < 0.0001 ) {
					goto LOOP_exit;
				}

				if ( CompSuctionEnth < SuperHeatEnth ) {
					T110 = CompSuctionTemp;
				} else {
					T111 = CompSuctionTemp;
				}
			}
			LOOP_exit: ;

			// Determine the Mass flow rate of refrigerant
			CompSuctionDensity = GetSupHeatDensityRefrig( GSHPRefrigerant, CompSuctionTemp, SuctionPr, GSHPRefrigIndex, RoutineNameCompSuctionTemp );
			MassRef = PistonDisp * CompSuctionDensity * ( 1.0 + ClearanceFactor - ClearanceFactor * std::pow( DischargePr / SuctionPr, 1.0 / gamma ) );

			// Find the  Source Side Heat Transfer
			QSource = MassRef * ( SourceSideOutletEnth - LoadSideOutletEnth );

			// Determine the theoretical power
			Power = PowerLosses + ( MassRef * gamma / ( gamma - 1 ) * SuctionPr / CompSuctionDensity / LosFac * ( std::pow( DischargePr / SuctionPr, ( gamma - 1 ) / gamma ) - 1 ) );

			// Determine the Loadside HeatRate (QLoad)
			QLoad = Power + QSource;

			// convergence and iteration limit check
			if ( std::abs( ( QLoad - initialQLoad ) / ( initialQLoad + SmallNum ) ) < HeatBalTol || IterationCount > IterationLimit ) {
				if ( IterationCount > IterationLimit ) {
					ShowWarningError( ModuleCompName + " did not converge" );
					ShowContinueErrorTimeStamp( "" );
					ShowContinueError( "Heatpump Name = " + this->Name );
					gio::write( ErrString, fmtLD ) << std::abs( 100.0 * ( QLoad - initialQLoad ) / ( initialQLoad + SmallNum ) );
					ShowContinueError( "Heat Inbalance (%)             = " + stripped( ErrString ) );
					gio::write( ErrString, fmtLD ) << QLoad;
					ShowContinueError( "Load-side heat transfer rate   = " + stripped( ErrString ) );
					gio::write( ErrString, fmtLD ) << QSource;
					ShowContinueError( "Source-side heat transfer rate = " + stripped( ErrString ) );
					gio::write( ErrString, fmtLD ) << SourceSideWaterMassFlowRate;
					ShowContinueError( "Source-side mass flow rate     = " + stripped( ErrString ) );
					gio::write( ErrString, fmtLD ) << LoadSideWaterMassFlowRate;
					ShowContinueError( "Load-side mass flow rate       = " + stripped( ErrString ) );
					gio::write( ErrString, fmtLD ) << SourceSideWaterInletTemp;
					ShowContinueError( "Source-side inlet temperature  = " + stripped( ErrString ) );
					gio::write( ErrString, fmtLD ) << LoadSideWaterInletTemp;
					ShowContinueError( "Load-side inlet temperature    = " + stripped( ErrString ) );
				}
				goto LOOPLoadEnth_exit;

			} else { // update load
				initialQLoad += RelaxParam * ( QLoad - initialQLoad );
				initialQSource += RelaxParam * ( QSource - initialQSource );
			}

		}
		LOOPLoadEnth_exit: ;

		//Control Strategy
		if ( std::abs( MyLoad ) < QLoad ) {
			DutyFactor = std::abs( MyLoad ) / QLoad;
			QLoad = std::abs( MyLoad );
			Power *= DutyFactor;
			QSource *= DutyFactor;

			// Determine the Exterior fluid temperature at the Load Side oulet and eveporator outlet...
			// Refrigerant = "Steam"
			LoadSideWaterOutletTemp = LoadSideWaterInletTemp + QLoad / ( LoadSideWaterMassFlowRate * CpLoadSide );
			SourceSideWaterOutletTemp = SourceSideWaterInletTemp - QSource / ( SourceSideWaterMassFlowRate * CpSourceSide );
			return;
		}

		LoadSideWaterOutletTemp = LoadSideWaterInletTemp + QLoad / ( LoadSideWaterMassFlowRate * CpLoadSide );
		SourceSideWaterOutletTemp = SourceSideWaterInletTemp - QSource / ( SourceSideWaterMassFlowRate * CpSourceSide );
		// REPORT VAR
		this->Running = 1;

	}

	void
	GshpSpecs::UpdateGSHPRecords() // GSHP number
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Arun
		//       DATE WRITTEN:    October 1998

		// PURPOSE OF THIS SUBROUTINE:
		// reporting

		// METHODOLOGY EMPLOYED: na

		// REFERENCES: na

		// Using/Aliasing
		using DataHVACGlobals::TimeStepSys;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int SourceSideInletNode; // Source Side inlet node number, water side
		int SourceSideOutletNode; // Source Side outlet node number, water side
		int LoadSideInletNode; // Load Side inlet node number, water side
		int LoadSideOutletNode; // Load Side outlet node number, water side
		Real64 ReportingConstant;

		LoadSideInletNode = this->LoadSideInletNodeNum;
		LoadSideOutletNode = this->LoadSideOutletNodeNum;
		SourceSideInletNode = this->SourceSideInletNodeNum;
		SourceSideOutletNode = this->SourceSideOutletNodeNum;

		if ( ! this->MustRun ) {
			//set node temperatures
			Node( SourceSideOutletNode ).Temp = Node( SourceSideInletNode ).Temp;
			Node( LoadSideOutletNode ).Temp = Node( LoadSideInletNode ).Temp;

			this->Power = 0.0;
			this->Energy = 0.0;
			this->QSource = 0.0;
			this->QSourceEnergy = 0.0;
			this->QLoad = 0.0;
			this->QLoadEnergy = 0.0;
			this->SourceSideWaterInletTemp = Node( SourceSideInletNode ).Temp;
			this->SourceSideWaterOutletTemp = Node( SourceSideOutletNode ).Temp;
			this->LoadSideWaterInletTemp = Node( LoadSideInletNode ).Temp;
			this->LoadSideWaterOutletTemp = Node( LoadSideOutletNode ).Temp;
			this->SourceSidemdot = SourceSideWaterMassFlowRate;
			this->LoadSidemdot = LoadSideWaterMassFlowRate;

		} else {
			//set node temperatures
			Node( LoadSideOutletNode ).Temp = LoadSideWaterOutletTemp;
			Node( SourceSideOutletNode ).Temp = SourceSideWaterOutletTemp;

			ReportingConstant = TimeStepSys * SecInHour;
			this->Energy = Power * ReportingConstant;
			this->QSourceEnergy = QSource * ReportingConstant;
			this->QLoadEnergy = QLoad * ReportingConstant;
			this->LoadSideWaterInletTemp = Node( LoadSideInletNode ).Temp;
			this->LoadSideWaterOutletTemp = Node( LoadSideOutletNode ).Temp;
			this->SourceSideWaterInletTemp = Node( SourceSideInletNode ).Temp;
			this->SourceSideWaterOutletTemp = Node( SourceSideOutletNode ).Temp;
			this->SourceSidemdot = SourceSideWaterMassFlowRate;
			this->LoadSidemdot = LoadSideWaterMassFlowRate;

		}
	}

	void
	GetGshpInput()
	{
		//       SUBROUTINE INFORMATION:
		//       AUTHOR:
		//       DATE WRITTEN:    April 1998

		// PURPOSE OF THIS SUBROUTINE:
		// This routine will get the input
		// required by the GSHP models.  As such
		// it will interact with the Input Scanner to retrieve
		// information from the input file, count the number of
		// GSHPs and begin to fill the
		// arrays associated with the type GSHP.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// Using/Aliasing
		using DataPlant::TypeOf_HPWaterPEHeating;
		using DataPlant::ScanPlantLoopsForObject;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::TestCompSet;
		using FluidProperties::FindRefrigerant;
		using PlantUtilities::RegisterPlantCompDesignFlow;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int GSHPNum; // Gshp counter
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		int IOStat; // IO Status when calling get input subroutine
		Array1D_string AlphArray( 5 ); // character string data
		Array1D< Real64 > NumArray( 23 ); // numeric data

		static bool ErrorsFound( false );
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name

		NumGSHPs = GetNumObjectsFound( ModuleCompName );

		if ( NumGSHPs <= 0 ) {
			ShowSevereError( ModuleCompName + ": No Equipment found" );
			ErrorsFound = true;
		}

		// Allocate Arrays
		GSHP.allocate( NumGSHPs );
		GSHPReport.allocate( NumGSHPs );
		CheckEquipName.dimension( NumGSHPs, true );

		for ( GSHPNum = 1; GSHPNum <= NumGSHPs; ++GSHPNum ) {
			GetObjectItem( ModuleCompNameUC, GSHPNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat );
			IsNotOK = false;
			IsBlank = true;
			VerifyName( AlphArray( 1 ), GSHP, GSHPNum - 1, IsNotOK, IsBlank, "GHSP Name" );

			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
			}
			GSHP( GSHPNum ).Name = AlphArray( 1 );

			GSHP( GSHPNum ).WWHPPlantTypeOfNum = TypeOf_HPWaterPEHeating;

			GSHP( GSHPNum ).COP = NumArray( 1 );
			if ( NumArray( 1 ) == 0.0 ) {
				ShowSevereError( ModuleCompName + ":COP = 0.0, Heatpump=" + AlphArray( 1 ) );
				ErrorsFound = true;
			}

			// zero values for NumArray 3 - 6 checked in input - idd
			GSHP( GSHPNum ).NomCap = NumArray( 2 );

			GSHP( GSHPNum ).MinPartLoadRat = NumArray( 3 );

			GSHP( GSHPNum ).MaxPartLoadRat = NumArray( 4 );

			GSHP( GSHPNum ).OptPartLoadRat = NumArray( 5 );

			GSHP( GSHPNum ).LoadSideVolFlowRate = NumArray( 6 );
			if ( NumArray( 6 ) == 0.0 ) {
				ShowSevereError( ModuleCompName + ":Load Side Flow Rate = 0.0, Heatpump=" + AlphArray( 1 ) );
				ErrorsFound = true;
			}

			GSHP( GSHPNum ).SourceSideVolFlowRate = NumArray( 7 );
			if ( NumArray( 7 ) == 0.0 ) {
				ShowSevereError( ModuleCompName + ":Source Side Flow Rate = 0.0, Heatpump=" + AlphArray( 1 ) );
				ErrorsFound = true;
			}

			GSHP( GSHPNum ).LoadSideUACoeff = NumArray( 8 );
			if ( NumArray( 8 ) == 0.0 ) {
				ShowSevereError( ModuleCompName + ":Load Side Heat Transfer Coeffcient = 0.0, Heatpump=" + AlphArray( 1 ) );
				ErrorsFound = true;
			}

			GSHP( GSHPNum ).SourceSideUACoeff = NumArray( 9 );
			if ( NumArray( 9 ) == 0.0 ) {
				ShowSevereError( ModuleCompName + ":Source Side Heat Transfer Coeffcient = 0.0, Heatpump=" + AlphArray( 1 ) );
				ErrorsFound = true;
			}

			GSHP( GSHPNum ).CompPistonDisp = NumArray( 10 );
			if ( NumArray( 10 ) == 0.0 ) {
				ShowSevereError( ModuleCompName + ":Compressor Piston displacement/Storke = 0.0, Heatpump=" + AlphArray( 1 ) );
				ErrorsFound = true;
			}

			GSHP( GSHPNum ).CompClearanceFactor = NumArray( 11 );
			if ( NumArray( 11 ) == 0.0 ) {
				ShowSevereError( ModuleCompName + ":Compressor Clearance Factor = 0.0, Heatpump=" + AlphArray( 1 ) );
				ErrorsFound = true;
			}

			GSHP( GSHPNum ).CompSucPressDrop = NumArray( 12 );
			if ( NumArray( 12 ) == 0.0 ) {
				ShowSevereError( ModuleCompName + ": Pressure Drop = 0.0, Heatpump=" + AlphArray( 1 ) );
				ErrorsFound = true;
			}

			GSHP( GSHPNum ).SuperheatTemp = NumArray( 13 );
			if ( NumArray( 13 ) == 0.0 ) {
				ShowSevereError( ModuleCompName + ":Source Side SuperHeat = 0.0, Heatpump=" + AlphArray( 1 ) );
				ErrorsFound = true;
			}

			GSHP( GSHPNum ).PowerLosses = NumArray( 14 );
			if ( NumArray( 14 ) == 0.0 ) {
				ShowSevereError( ModuleCompName + ":Compressor Power Loss = 0.0, Heatpump=" + AlphArray( 1 ) );
				ErrorsFound = true;
			}
			GSHP( GSHPNum ).LossFactor = NumArray( 15 );
			if ( NumArray( 15 ) == 0.0 ) {
				ShowSevereError( ModuleCompName + ":Efficiency = 0.0, Heatpump=" + AlphArray( 1 ) );
				ErrorsFound = true;
			}

			GSHP( GSHPNum ).HighPressCutoff = NumArray( 16 );
			if ( NumArray( 16 ) == 0.0 ) {
				GSHP( GSHPNum ).HighPressCutoff = 500000000.0;
				//CALL ShowWarningError(ModuleCompName//': High Pressure Cut Off= 0.0 Heat Pump'//TRIM(AlphArray(1)))
			}

			GSHP( GSHPNum ).LowPressCutoff = NumArray( 17 );
			if ( NumArray( 17 ) == 0.0 ) {
				GSHP( GSHPNum ).LowPressCutoff = 0.0;
				//CALL ShowWarningError(ModuleCompName//': Low Pressure Cut Off= 0.0 Heat Pump'//TRIM(AlphArray(1)))
			}

//Autodesk:Bug CycleTime was removed in 8.2 so this doesn't compile
//			GSHP( GSHPNum ).CycleTime = NumArray( 18 );
//			if ( NumArray( 18 ) == 0.0 ) {
//				GSHP( GSHPNum ).CycleTime = 0.10;
//				ShowWarningError( ModuleCompName + ": Unit Cycle Time= 0.0 Heat Pump" + trim( AlphArray( 1 ) ) );
//			}

			GSHP( GSHPNum ).SourceSideInletNodeNum = GetOnlySingleNode( AlphArray( 2 ), ErrorsFound, ModuleCompName, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

			GSHP( GSHPNum ).SourceSideOutletNodeNum = GetOnlySingleNode( AlphArray( 3 ), ErrorsFound, ModuleCompName, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			GSHP( GSHPNum ).LoadSideInletNodeNum = GetOnlySingleNode( AlphArray( 4 ), ErrorsFound, ModuleCompName, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Inlet, 2, ObjectIsNotParent );

			GSHP( GSHPNum ).LoadSideOutletNodeNum = GetOnlySingleNode( AlphArray( 5 ), ErrorsFound, ModuleCompName, AlphArray( 1 ), NodeType_Water, NodeConnectionType_Outlet, 2, ObjectIsNotParent );

			// Test node sets
			TestCompSet( ModuleCompNameUC, AlphArray( 1 ), AlphArray( 2 ), AlphArray( 3 ), "Condenser Water Nodes" );
			TestCompSet( ModuleCompNameUC, AlphArray( 1 ), AlphArray( 4 ), AlphArray( 5 ), "Hot Water Nodes" );

			// save the design source side flow rate for use by plant loop sizing algorithms
			RegisterPlantCompDesignFlow( GSHP( GSHPNum ).SourceSideInletNodeNum, 0.5 * GSHP( GSHPNum ).SourceSideVolFlowRate );

		}

		if ( ErrorsFound ) {
			ShowFatalError( "Errors Found in getting " + ModuleCompNameUC + " Input" );
		}

		GSHPRefrigIndex = FindRefrigerant( GSHPRefrigerant );
		if ( GSHPRefrigIndex == 0 ) {
			ShowFatalError( "Refrigerant for HeatPump:WaterToWater Heating not found, should have been=" + GSHPRefrigerant );
		}

		// CurrentModuleObject='HeatPump:WaterToWater:ParameterEstimation:Heating'
		for ( GSHPNum = 1; GSHPNum <= NumGSHPs; ++GSHPNum ) {
			SetupOutputVariable( "Water to Water Heat Pump Electric Power [W]", GSHPReport( GSHPNum ).Power, "System", "Average", GSHP( GSHPNum ).Name );
			SetupOutputVariable( "Water to Water Heat Pump Electric Energy [J]", GSHPReport( GSHPNum ).Energy, "System", "Sum", GSHP( GSHPNum ).Name, _, "Electricity", "Heating", _, "Plant" );

			SetupOutputVariable( "Water to Water Heat Pump Load Side Heat Transfer Rate [W]", GSHPReport( GSHPNum ).QLoad, "System", "Average", GSHP( GSHPNum ).Name );
			SetupOutputVariable( "Water to Water Heat Pump Load Side Heat Transfer Energy [J]", GSHPReport( GSHPNum ).QLoadEnergy, "System", "Sum", GSHP( GSHPNum ).Name );

			SetupOutputVariable( "Water to Water Heat Pump Source Side Heat Transfer Rate [W]", GSHPReport( GSHPNum ).QSource, "System", "Average", GSHP( GSHPNum ).Name );
			SetupOutputVariable( "Water to Water Heat Pump Source Side Heat Transfer Energy [J]", GSHPReport( GSHPNum ).QSourceEnergy, "System", "Sum", GSHP( GSHPNum ).Name );

			SetupOutputVariable( "Water to Water Heat Pump Load Side Outlet Temperature [C]", GSHPReport( GSHPNum ).LoadSideWaterOutletTemp, "System", "Average", GSHP( GSHPNum ).Name );
			SetupOutputVariable( "Water to Water Heat Pump Load Side Inlet Temperature [C]", GSHPReport( GSHPNum ).LoadSideWaterInletTemp, "System", "Average", GSHP( GSHPNum ).Name );
			SetupOutputVariable( "Water to Water Heat Pump Source Side Outlet Temperature [C]", GSHPReport( GSHPNum ).SourceSideWaterOutletTemp, "System", "Average", GSHP( GSHPNum ).Name );
			SetupOutputVariable( "Water to Water Heat Pump Source Side Inlet Temperature [C]", GSHPReport( GSHPNum ).SourceSideWaterInletTemp, "System", "Average", GSHP( GSHPNum ).Name );
			SetupOutputVariable( "Water to Water Heat Pump Load Side Mass Flow Rate [kg/s]", GSHPReport( GSHPNum ).LoadSidemdot, "System", "Average", GSHP( GSHPNum ).Name );
			SetupOutputVariable( "Water to Water Heat Pump Source Side Mass Flow Rate [kg/s]", GSHPReport( GSHPNum ).SourceSidemdot, "System", "Average", GSHP( GSHPNum ).Name );
		}

	}

	void GshpSpecs::simulate( const PlantLocation & calledFromLocation, bool const FirstHVACIteration, Real64 & CurLoad )
	{
		//       SUBROUTINE INFORMATION:
		//       AUTHOR    Arun
		//       DATE WRITTEN   Feb 2000
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE: This is the  water to water Heat Pump driver.
		// It gets the input for the models, initializes simulation variables, calls
		// the appropriate model and sets up reporting variables.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// Using/Aliasing
		using PlantUtilities::UpdateChillerComponentCondenserSide;
		using DataPlant::TypeOf_HPWaterEFHeating;
		using InputProcessor::FindItemInList;
		using namespace DataEnvironment;
		using General::TrimSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// Simulate the model for the Demand "MyLoad"

		if ( calledFromLocation.loopNum == this->LoadLoopNum ) { // chilled water loop
			InitGshp();
			CalcGshpModel( CurLoad, FirstHVACIteration );
			UpdateGSHPRecords();
		} else if ( calledFromLocation.loopNum == this->SourceLoopNum ) { // condenser loop
			UpdateChillerComponentCondenserSide( this->SourceLoopNum, this->SourceLoopSideNum, TypeOf_HPWaterEFHeating, this->SourceSideInletNodeNum, this->SourceSideOutletNodeNum, - this->QSource, this->SourceSideWaterInletTemp, this->SourceSideWaterOutletTemp, this->SourceSidemdot, FirstHVACIteration );
		} else {
			ShowFatalError( "SimHPWatertoWaterHEATING:: Invalid loop connection " + ModuleCompName + ", Requested Unit=" + this->Name );
		}
	}

	void GshpSpecs::getDesignCapacities( const PlantLocation & EP_UNUSED(calledFromLocation), Real64 & MaxLoad, Real64 & MinLoad, Real64 & OptLoad )
	{
		MinLoad = this->NomCap * this->MinPartLoadRat;
		MaxLoad = this->NomCap * this->MaxPartLoadRat;
		OptLoad = this->NomCap * this->OptPartLoadRat;
	}

	PlantComponent * GshpSpecs::factory( int objectType, std::string objectName ) {
		// Process the input data for pipes if it hasn't been done already
		if ( GetInputFlag ) {
			GetGshpInput();
			GetInputFlag = false;
		}
		// Now look for this particular pipe in the list
		for ( auto & t_gshp : GSHP ) {
			if ( t_gshp.WWHPPlantTypeOfNum == objectType && t_gshp.Name == objectName ) {
				return &t_gshp;
			}
		}
		// If we didn't find it, fatal
		ShowFatalError( "LocalPipeDataFactory: Error getting inputs for pipe named: " + objectName );
		// Shut up the compiler
		return nullptr;
	}

	void
	GshpSpecs::onInitLoopEquip( const PlantLocation & EP_UNUSED(calledFromLocation) )
	{
			//scan for loop connection data
			bool errFlag = false;
			DataPlant::ScanPlantLoopsForObject( this->Name, this->WWHPPlantTypeOfNum, this->SourceLoopNum, this->SourceLoopSideNum, this->SourceBranchNum, this->SourceCompNum, _, _, _, this->SourceSideInletNodeNum, _, errFlag );
			DataPlant::ScanPlantLoopsForObject( this->Name, this->WWHPPlantTypeOfNum, this->LoadLoopNum, this->LoadLoopSideNum, this->LoadBranchNum, this->LoadCompNum, _, _, _, this->LoadSideInletNodeNum, _, errFlag );

			if ( ! errFlag ) {
				PlantUtilities::InterConnectTwoPlantLoopSides( this->LoadLoopNum,  this->LoadLoopSideNum,  this->SourceLoopNum, this->SourceLoopSideNum, this->WWHPPlantTypeOfNum, true );
			}

			if ( errFlag ) {
				ShowFatalError( "GetWatertoWaterHPInput: Program terminated on scan for loop data" );
			}
	}


} // HeatPumpWaterToWaterHEATING

} // EnergyPlus
