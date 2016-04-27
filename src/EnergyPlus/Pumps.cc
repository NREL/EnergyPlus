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
#include <Pumps.hh>
#include <BranchNodeConnections.hh>
#include <CurveManager.hh>
#include <DataBranchAirLoopPlant.hh>
#include <DataConvergParams.hh>
#include <DataHeatBalance.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <EMSManager.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <HeatBalanceInternalHeatGains.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <OutputReportPredefined.hh>
#include <PlantPressureSystem.hh>
#include <PlantUtilities.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace Pumps {

	// MODULE INFORMATION:
	//       AUTHOR         Dan Fisher
	//       DATE WRITTEN   Sept 1998
	//       MODIFIED       July 2001, Richard Liesen
	//                      July 2001, Rick Strand (new "local" pump control method)
	//                      Feb 2005, Rahul Chillar(added condensate pump for steam systems)
	//                      Jan 2006, Sankaranarayanan (Added pump banks to the library of pumps)
	//                      May 2009, Brent Griffith (added support for EMS override of massflow)
	//                      Aug 2010, Edwin Lee (refactored code, significant clean-up)
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// Encapsulates the data and algorithms to simulate pumps.

	// REFERENCES:
	// HVAC 2 Toolkit:  A Toolkit for Secondary HVAC System
	// Energy Calculations, ASHRAE, 1993, pp2-10 to 2-15

	// Using/Aliasing
	using DataGlobals::InitConvTemp;
	using DataGlobals::AnyEnergyManagementSystemInModel;
	using DataGlobals::SecInHour;
	using DataGlobals::BeginEnvrnFlag;
	using DataHVACGlobals::SmallWaterVolFlow;
	using DataHVACGlobals::NumPlantLoops;
	using DataHVACGlobals::NumCondLoops;
	using DataHVACGlobals::ForceOff;
	using DataHVACGlobals::CycleOn;
	using DataHVACGlobals::TimeStepSys;
	using DataLoopNode::NodeID;
	using DataLoopNode::Node;
	using DataLoopNode::NodeType_Water;
	using DataLoopNode::NodeType_Steam;
	using DataLoopNode::NodeConnectionType_Inlet;
	using DataLoopNode::NodeConnectionType_Outlet;
	using DataLoopNode::ObjectIsNotParent;

	// Data
	//MODULE PARAMETER DEFINITIONS:
	int const Continuous( 1 ); // Pump control type (pump always running)
	int const Intermittent( 2 ); // Pump control type (pump runs only when there is a demand)

	int const VFDManual( 1 ); // VFD control type (Scheduled RPM)
	int const VFDAutomatic( 2 ); // VFD control type (Variable RPM according to flow request)

	int const OptimalScheme( 1 ); // Control sequencing for pump bank
	int const SequentialScheme( 2 ); // Control sequencing for pump bank
	int const UserDefined( 3 ); // Control sequencing for pump bank

	std::string const cPump_VarSpeed( "Pump:VariableSpeed" );
	int const Pump_VarSpeed( 101 );
	std::string const cPump_ConSpeed( "Pump:ConstantSpeed" );
	int const Pump_ConSpeed( 102 );
	std::string const cPump_Cond( "Pump:VariableSpeed:Condensate" );
	int const Pump_Cond( 103 );
	std::string const cPumpBank_VarSpeed( "HeaderedPumps:VariableSpeed" );
	int const PumpBank_VarSpeed( 104 );
	std::string const cPumpBank_ConSpeed( "HeaderedPumps:ConstantSpeed" );
	int const PumpBank_ConSpeed( 105 );
	Array1D_string const cPumpTypes( {101,105}, { cPump_VarSpeed, cPump_ConSpeed, cPump_Cond, cPumpBank_VarSpeed, cPumpBank_ConSpeed } );


	static std::string const fluidNameSteam( "STEAM" );
	static std::string const fluidNameWater( "WATER" );

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	int NumPumps( 0 ); // Num Pumps (used in pump bank)
	int NumPumpsRunning( 0 ); // Num of pumps ON (used in pump bank)
	int NumPumpsFullLoad( 0 ); // Num pumps running at full load (used in pump bank)
	bool GetInputFlag( true ); // Get input once and once only
	//  INTEGER       :: NumPumpsPartLoad                   = 0    !Num pumps running at part load (used in pump bank)

	//simulation and reporting variables
	//  REAL(r64)           :: OutletTemp                   = 0.0d0  ! pump outlet temperature
	Real64 PumpMassFlowRate( 0.0 ); // mass flow rate at pump inlet node
	//  REAL(r64)           :: PumpPress                    = 0.0d0  ! For Passing around the steam loops
	//  REAL(r64)           :: PumpQuality                  = 0.0d0  ! For Passing around the steam loops=0.0 here
	Real64 PumpHeattoFluid( 0.0 ); // Pump Power dissipated in fluid stream
	Real64 Power( 0.0 ); // Pump Electric power
	Real64 ShaftPower( 0.0 ); // Power passing through pump shaft

	// SUBROUTINE SPECIFICATIONS FOR MODULE PrimaryPlantLoops

	// Object Data
	Array1D< PumpSpecs > PumpEquip;
	Array1D< ReportVars > PumpEquipReport;

	//*************************************************************************!

	// Functions
	void
	clear_state()
	{
		NumPumps = 0;
		NumPumpsRunning = 0;
		NumPumpsFullLoad = 0;
		GetInputFlag = true;
		PumpMassFlowRate = 0.0 ;
		PumpHeattoFluid= 0.0 ;
		Power= 0.0 ;
		ShaftPower= 0.0 ;
		PumpEquip.deallocate();
		PumpEquipReport.deallocate();
	}

	void
	SimPumps(
		std::string const & PumpName, // Name of pump to be managed
		int const LoopNum, // Plant loop number
		Real64 const FlowRequest, // requested flow from adjacent demand side
		bool & PumpRunning, // .TRUE. if the loop pump is actually operating
		int & PumpIndex,
		Real64 & PumpHeat
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   July 2001
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine manages the pump operation based on the type of
		// pump and the pump controls (continuous, intermittent, etc.).  The
		// result of this subroutine is that the pump has been simulated for
		// the necessary loop and the PumpRunning has been correctly set.

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using General::TrimSigDigits;
		using DataPlant::PlantLoop;
		using DataPlant::FlowPumpQuery;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int PumpNum; // Pump index within PumpEquip derived type

		// Get input from IDF one time
		if ( GetInputFlag ) {
			GetPumpInput();
			GetInputFlag = false;
		}

		// Exit early if no pumps found
		if ( NumPumps == 0 ) {
			PumpHeat = 0.0;
			return;
		}

		// Setup pump component index if needed
		if ( PumpIndex == 0 ) {
			PumpNum = FindItemInList( PumpName, PumpEquip ); // Determine which pump to simulate
			if ( PumpNum == 0 ) {
				ShowFatalError( "ManagePumps: Pump requested not found =" + PumpName ); // Catch any bad names before crashing
			}
			PumpIndex = PumpNum;
		} else {
			PumpNum = PumpIndex;
			if ( PumpEquip( PumpNum ).CheckEquipName ) {
				if ( PumpNum > NumPumps || PumpNum < 1 ) {
					ShowFatalError( "ManagePumps: Invalid PumpIndex passed=" + TrimSigDigits( PumpNum ) + ", Number of Pumps=" + TrimSigDigits( NumPumps ) + ", Pump name=" + PumpName );
				}
				if ( PumpName != PumpEquip( PumpNum ).Name ) {
					ShowFatalError( "ManagePumps: Invalid PumpIndex passed=" + TrimSigDigits( PumpNum ) + ", Pump name=" + PumpName + ", stored Pump Name for that index=" + PumpEquip( PumpNum ).Name );
				}
				PumpEquip( PumpNum ).CheckEquipName = false;
			}
		}

		// Perform one-time and begin-environment initialization
		InitializePumps( PumpNum );

		// If all we need is to set outlet min/max avail, then just do it and get out.  Also, we only do min/max avail on flow query
		if ( PlantLoop( LoopNum ).LoopSide( PumpEquip( PumpNum ).LoopSideNum ).FlowLock == FlowPumpQuery ) {
			SetupPumpMinMaxFlows( LoopNum, PumpNum );
			return;
		}

		// Set pump flow rate and calculate power
		CalcPumps( PumpNum, FlowRequest, PumpRunning );

		// Update pump reporting data
		ReportPumps( PumpNum );

		// Send this up to the calling routine
		PumpHeat = PumpHeattoFluid;

	}

	//*************************************************************************!

	//*************************************************************************!

	void
	GetPumpInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Dan Fisher
		//       DATE WRITTEN:    April 1998
		//       MODIFIED:        July 2001, Rick Strand (addition of pump controls)
		//                        May 2009, Brent Griffith (added EMS calls)

		// PURPOSE OF THIS SUBROUTINE:
		// This routine will get the input
		// required by the pump simulation.

		// PUMP:VARIABLE SPEED,
		// This pump model is described in the ASHRAE secondary HVAC toolkit.

		// REFERENCES:
		// HVAC 2 Toolkit:  A Toolkit for Secondary HVAC System
		//  Energy Calculations, ASHRAE, 1993, pp2-10 to 2-15

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using InputProcessor::FindItemInList;
		using DataIPShortCuts::lAlphaFieldBlanks;
		using DataIPShortCuts::cAlphaFieldNames;
		using DataIPShortCuts::cNumericFieldNames;
		using DataIPShortCuts::lNumericFieldBlanks;
		using DataIPShortCuts::cCurrentModuleObject;
		using DataIPShortCuts::cAlphaArgs;
		using DataIPShortCuts::rNumericArgs;
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::TestCompSet;
		using FluidProperties::GetSatDensityRefrig;
		using FluidProperties::GetDensityGlycol;
		using DataSizing::AutoSize;
		using CurveManager::GetCurveType;
		using CurveManager::GetCurveIndex;
		using CurveManager::GetCurveMinMaxValues;
		using DataPlant::TypeOf_PumpVariableSpeed;
		using DataPlant::TypeOf_PumpConstantSpeed;
		using DataPlant::TypeOf_PumpCondensate;
		using DataPlant::TypeOf_PumpBankVariableSpeed;
		using DataPlant::TypeOf_PumpBankConstantSpeed;
		using ScheduleManager::GetScheduleIndex;
		using ScheduleManager::CheckScheduleValueMinMax;
		using DataHeatBalance::IntGainTypeOf_Pump_VarSpeed;
		using DataHeatBalance::IntGainTypeOf_Pump_ConSpeed;
		using DataHeatBalance::IntGainTypeOf_Pump_Cond;
		using DataHeatBalance::IntGainTypeOf_PumpBank_VarSpeed;
		using DataHeatBalance::IntGainTypeOf_PumpBank_ConSpeed;
		using DataHeatBalance::Zone;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const StartTemp( 100.0 ); // Standard Temperature across code to calculated Steam density
		static std::string const RoutineName( "GetPumpInput: " );
		static std::string const RoutineNameNoColon( "GetPumpInput" );

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int PumpNum;
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		int IOStat; // IO Status when calling get input subroutine
		bool ErrorsFound;
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		int TempCurveIndex;
		std::string TempCurveType;
		int NumVarSpeedPumps;
		int NumConstSpeedPumps;
		int NumCondensatePumps;
		int NumVarPump;
		int NumConstPump;
		int NumCondPump;
		int NumPumpBankSimpleVar;
		int NumPumpBankSimpleConst;
		int NumVarPumpBankSimple;
		int NumConstPumpBankSimple;
		Real64 SteamDensity;
		Real64 TempWaterDensity;
		static int DummyWaterIndex( 1 );

		ErrorsFound = false;

		//GET NUMBER OF ALL EQUIPMENT TYPES
		NumVarSpeedPumps = GetNumObjectsFound( cPump_VarSpeed );
		NumConstSpeedPumps = GetNumObjectsFound( cPump_ConSpeed );
		NumCondensatePumps = GetNumObjectsFound( cPump_Cond );
		NumPumpBankSimpleVar = GetNumObjectsFound( cPumpBank_VarSpeed );
		NumPumpBankSimpleConst = GetNumObjectsFound( cPumpBank_ConSpeed );
		NumPumps = NumVarSpeedPumps + NumConstSpeedPumps + NumCondensatePumps + NumPumpBankSimpleVar + NumPumpBankSimpleConst;

		if ( NumPumps <= 0 ) {
			ShowWarningError( "No Pumping Equipment Found" );
			return;
		}

		PumpEquip.allocate( NumPumps );
		PumpEquipReport.allocate( NumPumps );

		//LOAD ARRAYS WITH VARIABLE SPEED CURVE FIT PUMP DATA

		cCurrentModuleObject = cPump_VarSpeed;

		for ( NumVarPump = 1; NumVarPump <= NumVarSpeedPumps; ++NumVarPump ) {
			PumpNum = NumVarPump;
			GetObjectItem( cCurrentModuleObject, NumVarPump, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), PumpEquip, PumpNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			PumpEquip( PumpNum ).Name = cAlphaArgs( 1 );
			PumpEquip( PumpNum ).PumpType = Pump_VarSpeed; //'Pump:VariableSpeed'
			PumpEquip( PumpNum ).TypeOf_Num = TypeOf_PumpVariableSpeed;

			PumpEquip( PumpNum ).InletNodeNum = GetOnlySingleNode( cAlphaArgs( 2 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

			PumpEquip( PumpNum ).OutletNodeNum = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 2 ), cAlphaArgs( 3 ), "Water Nodes" );

			//    PumpEquip(PumpNum)%PumpControlType = cAlphaArgs(4)
			if ( SameString( cAlphaArgs( 4 ), "Continuous" ) ) {
				PumpEquip( PumpNum ).PumpControl = Continuous;
			} else if ( SameString( cAlphaArgs( 4 ), "Intermittent" ) ) {
				PumpEquip( PumpNum ).PumpControl = Intermittent;
			} else {
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + PumpEquip( PumpNum ).Name + "\", Invalid " + cAlphaFieldNames( 4 ) );
				ShowContinueError( "Entered Value=[" + cAlphaArgs( 4 ) + "]. " + cAlphaFieldNames( 4 ) + " has been set to Continuous for this pump." );
				PumpEquip( PumpNum ).PumpControl = Continuous;
			}

			// Input the optional schedule for the pump
			PumpEquip( PumpNum ).PumpSchedule = cAlphaArgs( 5 );
			PumpEquip( PumpNum ).PumpScheduleIndex = GetScheduleIndex( cAlphaArgs( 5 ) );
			if ( ! lAlphaFieldBlanks( 5 ) && ! ( PumpEquip( PumpNum ).PumpScheduleIndex > 0 ) ) {
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + PumpEquip( PumpNum ).Name + "\", Invalid " + cAlphaFieldNames( 5 ) );
				ShowContinueError( "Schedule named =[" + cAlphaArgs( 5 ) + "]. was not found and will not be used." );
			}

			PumpEquip( PumpNum ).NomVolFlowRate = rNumericArgs( 1 );
			if ( PumpEquip( PumpNum ).NomVolFlowRate ==  AutoSize ) {
				PumpEquip( PumpNum ).NomVolFlowRateWasAutoSized = true;
			}
			PumpEquip( PumpNum ).NomPumpHead = rNumericArgs( 2 );
			PumpEquip( PumpNum ).NomPowerUse = rNumericArgs( 3 );
			if ( PumpEquip( PumpNum ).NomPowerUse == AutoSize ) {
				PumpEquip( PumpNum ).NomPowerUseWasAutoSized = true;
			}
			PumpEquip( PumpNum ).MotorEffic = rNumericArgs( 4 );
			PumpEquip( PumpNum ).FracMotorLossToFluid = rNumericArgs( 5 );
			PumpEquip( PumpNum ).PartLoadCoef( 1 ) = rNumericArgs( 6 );
			PumpEquip( PumpNum ).PartLoadCoef( 2 ) = rNumericArgs( 7 );
			PumpEquip( PumpNum ).PartLoadCoef( 3 ) = rNumericArgs( 8 );
			PumpEquip( PumpNum ).PartLoadCoef( 4 ) = rNumericArgs( 9 );
			PumpEquip( PumpNum ).MinVolFlowRate    = rNumericArgs( 10 );
			if ( PumpEquip( PumpNum ).MinVolFlowRate == AutoSize  ) {
				PumpEquip( PumpNum ).minVolFlowRateWasAutosized = true;
			}
			//Probably the following two lines will be used if the team agrees on changing the F10 value from min flow rate to
			//minimum flow as a fraction of nominal flow.
			//    PumpEquip(PumpNum)%MinVolFlowRateFrac  = rNumericArgs(10)
			//    PumpEquip(PumpNum)%MinVolFlowRate      = PumpEquip(PumpNum)%NomVolFlowRate * PumpEquip(PumpNum)%MinVolFlowRateFrac

			// Input pressure related data such as pressure curve and impeller size/rotational speed
			PumpEquip( PumpNum ).PressureCurve_Name = cAlphaArgs( 6 );
			if ( PumpEquip( PumpNum ).PressureCurve_Name == "" ) {
				PumpEquip( PumpNum ).PressureCurve_Index = -1;
			} else {
				TempCurveIndex = GetCurveIndex( PumpEquip( PumpNum ).PressureCurve_Name );
				if ( TempCurveIndex == 0 ) {
					PumpEquip( PumpNum ).PressureCurve_Index = -1;
				} else {
					TempCurveType = GetCurveType( TempCurveIndex );
					{ auto const SELECT_CASE_var( TempCurveType );
					if ( ( SELECT_CASE_var == "LINEAR" ) || ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) || ( SELECT_CASE_var == "QUARTIC" ) ) {
						PumpEquip( PumpNum ).PressureCurve_Index = TempCurveIndex;
						GetCurveMinMaxValues( TempCurveIndex, PumpEquip( PumpNum ).MinPhiValue, PumpEquip( PumpNum ).MaxPhiValue );
					} else {
						ErrorsFound = true;
					}}
				}
			}

			//read in the rest of the pump pressure characteristics
			PumpEquip( PumpNum ).ImpellerDiameter = rNumericArgs( 11 );

			// Input VFD related data
			if ( lAlphaFieldBlanks( 7 ) ) {
				PumpEquip( PumpNum ).HasVFD = false;
			} else {
				PumpEquip( PumpNum ).HasVFD = true;
				if ( cAlphaArgs( 7 ) == "MANUALCONTROL" ) {
					PumpEquip( PumpNum ).VFD.VFDControlType = VFDManual;
					PumpEquip( PumpNum ).VFD.ManualRPMSchedName = cAlphaArgs( 8 );
					PumpEquip( PumpNum ).VFD.ManualRPMSchedIndex = GetScheduleIndex( cAlphaArgs( 8 ) );
					if ( PumpEquip( PumpNum ).VFD.ManualRPMSchedIndex <= 0 ) {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + PumpEquip( PumpNum ).Name + "\", At least one scheduled VFD schedule input was invalid." );
						ShowContinueError( "Verify that all of the pressure and rpm schedules referenced in the input fields actually exist." );
						ErrorsFound = true;
					} else if ( ! CheckScheduleValueMinMax( PumpEquip( PumpNum ).VFD.ManualRPMSchedIndex, ">", 0.0 ) || ! CheckScheduleValueMinMax( PumpEquip( PumpNum ).VFD.ManualRPMSchedIndex, ">", 0.0 ) ) {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + PumpEquip( PumpNum ).Name + "\", A pump rpm schedule had zero value.  Ensure all entries in the schedule are greater than zero." );
						ErrorsFound = true;
					}
				} else if ( cAlphaArgs( 7 ) == "PRESSURESETPOINTCONTROL" ) {
					PumpEquip( PumpNum ).VFD.VFDControlType = VFDAutomatic;
					PumpEquip( PumpNum ).VFD.LowerPsetSchedName = cAlphaArgs( 9 );
					PumpEquip( PumpNum ).VFD.LowerPsetSchedIndex = GetScheduleIndex( cAlphaArgs( 9 ) );
					PumpEquip( PumpNum ).VFD.UpperPsetSchedName = cAlphaArgs( 10 );
					PumpEquip( PumpNum ).VFD.UpperPsetSchedIndex = GetScheduleIndex( cAlphaArgs( 10 ) );
					PumpEquip( PumpNum ).VFD.MinRPMSchedName = cAlphaArgs( 11 );
					PumpEquip( PumpNum ).VFD.MinRPMSchedIndex = GetScheduleIndex( cAlphaArgs( 11 ) );
					PumpEquip( PumpNum ).VFD.MaxRPMSchedName = cAlphaArgs( 12 );
					PumpEquip( PumpNum ).VFD.MaxRPMSchedIndex = GetScheduleIndex( cAlphaArgs( 12 ) );
					if ( min( PumpEquip( PumpNum ).VFD.LowerPsetSchedIndex, PumpEquip( PumpNum ).VFD.UpperPsetSchedIndex, PumpEquip( PumpNum ).VFD.MinRPMSchedIndex, PumpEquip( PumpNum ).VFD.MaxRPMSchedIndex ) <= 0 ) {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + PumpEquip( PumpNum ).Name + "\", At least one scheduled VFD schedule input was invalid." );
						ShowContinueError( "Verify that all of the pressure and rpm schedules referenced in the input fields actually exist." );
						ErrorsFound = true;
					} else if ( ! CheckScheduleValueMinMax( PumpEquip( PumpNum ).VFD.MinRPMSchedIndex, ">", 0.0 ) || ! CheckScheduleValueMinMax( PumpEquip( PumpNum ).VFD.MaxRPMSchedIndex, ">", 0.0 ) ) {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + PumpEquip( PumpNum ).Name + "\", A pump rpm schedule had zero value.  Ensure all entries in the schedule are greater than zero." );
						ErrorsFound = true;
					}
				} else {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + PumpEquip( PumpNum ).Name + "\", VFD Control type entered is invalid.  Use one of the key choice entries." );
					ErrorsFound = true;
				}
			}

			if ( ! lAlphaFieldBlanks( 13 ) ) { // zone named for pump skin losses
				PumpEquip( PumpNum ).ZoneNum = FindItemInList( cAlphaArgs( 13 ), Zone );
				if ( PumpEquip( PumpNum ).ZoneNum > 0 ) {
					PumpEquip( PumpNum ).HeatLossesToZone = true;
					if ( ! lNumericFieldBlanks( 12 ) ) {
						PumpEquip( PumpNum ).SkinLossRadFraction = rNumericArgs( 12 );
					}
				} else {
					ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 13 ) + "=\"" + cAlphaArgs( 13 ) + "\" not found." );
					ErrorsFound = true;
				}
			}

			if ( ! lAlphaFieldBlanks( 14 ) ) {
				if ( cAlphaArgs( 14 ) == "POWERPERFLOW" ) {
					PumpEquip( PumpNum ).powerSizingMethod = sizePowerPerFlow;
				} else if ( cAlphaArgs( 14 ) == "POWERPERFLOWPERPRESSURE" ) {
					PumpEquip( PumpNum ).powerSizingMethod = sizePowerPerFlowPerPressure;
				} else {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + PumpEquip( PumpNum ).Name + "\", sizing method type entered is invalid.  Use one of the key choice entries." );
					ErrorsFound = true;
				}
			}

			if ( ! lNumericFieldBlanks( 13 ) ) {
				PumpEquip( PumpNum ).powerPerFlowScalingFactor = rNumericArgs( 13 );
			}

			if ( ! lNumericFieldBlanks( 14 ) ) {
				PumpEquip( PumpNum ).powerPerFlowPerPressureScalingFactor = rNumericArgs( 14 );
			}

			if ( ! lNumericFieldBlanks( 15 ) ) {
				PumpEquip( PumpNum ).MinVolFlowRateFrac = rNumericArgs( 15 );
			}

			// Is this really necessary for each pump GetInput loop?
			PumpEquip( PumpNum ).Energy = 0.0;
			PumpEquip( PumpNum ).Power = 0.0;
		}

		cCurrentModuleObject = cPump_ConSpeed;

		for ( NumConstPump = 1; NumConstPump <= NumConstSpeedPumps; ++NumConstPump ) {
			PumpNum = NumVarSpeedPumps + NumConstPump;
			GetObjectItem( cCurrentModuleObject, NumConstPump, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), PumpEquip, PumpNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			PumpEquip( PumpNum ).Name = cAlphaArgs( 1 );
			PumpEquip( PumpNum ).PumpType = Pump_ConSpeed; //'Pump:ConstantSpeed'
			PumpEquip( PumpNum ).TypeOf_Num = TypeOf_PumpConstantSpeed;

			PumpEquip( PumpNum ).InletNodeNum = GetOnlySingleNode( cAlphaArgs( 2 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

			PumpEquip( PumpNum ).OutletNodeNum = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 2 ), cAlphaArgs( 3 ), "Water Nodes" );

			PumpEquip( PumpNum ).NomVolFlowRate = rNumericArgs( 1 );
			if ( PumpEquip( PumpNum ).NomVolFlowRate ==  AutoSize ) {
				PumpEquip( PumpNum ).NomVolFlowRateWasAutoSized = true;
			}
			PumpEquip( PumpNum ).NomPumpHead = rNumericArgs( 2 );
			PumpEquip( PumpNum ).NomPowerUse = rNumericArgs( 3 );
			if ( PumpEquip( PumpNum ).NomPowerUse == AutoSize ) {
				PumpEquip( PumpNum ).NomPowerUseWasAutoSized = true;
			}
			PumpEquip( PumpNum ).MotorEffic = rNumericArgs( 4 );
			PumpEquip( PumpNum ).FracMotorLossToFluid = rNumericArgs( 5 );
			PumpEquip( PumpNum ).PartLoadCoef( 1 ) = 1.0;
			PumpEquip( PumpNum ).PartLoadCoef( 2 ) = 0.0;
			PumpEquip( PumpNum ).PartLoadCoef( 3 ) = 0.0;
			PumpEquip( PumpNum ).PartLoadCoef( 4 ) = 0.0;
			//DSU In a constant volume pump we previously set the minimum to the nominal capacity
			//DSU Now we model the pump as constant speed and set flow by riding the pump curve.
			//DSU PumpEquip(PumpNum)%MinVolFlowRate      = rNumericArgs(1)
			PumpEquip( PumpNum ).MinVolFlowRate = 0.0;
			PumpEquip( PumpNum ).Energy = 0.0;
			PumpEquip( PumpNum ).Power = 0.0;

			//    PumpEquip(PumpNum)%PumpControlType = cAlphaArgs(4)
			if ( SameString( cAlphaArgs( 4 ), "Continuous" ) ) {
				PumpEquip( PumpNum ).PumpControl = Continuous;
			} else if ( SameString( cAlphaArgs( 4 ), "Intermittent" ) ) {
				PumpEquip( PumpNum ).PumpControl = Intermittent;
			} else {
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + PumpEquip( PumpNum ).Name + "\", Invalid " + cAlphaFieldNames( 4 ) );
				ShowContinueError( "Entered Value=[" + cAlphaArgs( 4 ) + "]. " + cAlphaFieldNames( 4 ) + " has been set to Continuous for this pump." );
				PumpEquip( PumpNum ).PumpControl = Continuous;
			}

			// Input the optional schedule for the pump
			PumpEquip( PumpNum ).PumpSchedule = cAlphaArgs( 5 );
			PumpEquip( PumpNum ).PumpScheduleIndex = GetScheduleIndex( cAlphaArgs( 5 ) );
			if ( ! lAlphaFieldBlanks( 5 ) && ! ( PumpEquip( PumpNum ).PumpScheduleIndex > 0 ) ) {
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + PumpEquip( PumpNum ).Name + "\", Invalid " + cAlphaFieldNames( 5 ) );
				ShowContinueError( "Schedule named =[" + cAlphaArgs( 5 ) + "]. was not found and will not be used." );
			}

			// Input pressure related data such as pressure curve and impeller size/rotational speed
			PumpEquip( PumpNum ).PressureCurve_Name = cAlphaArgs( 6 );
			if ( PumpEquip( PumpNum ).PressureCurve_Name == "" ) {
				PumpEquip( PumpNum ).PressureCurve_Index = -1;
			} else {
				TempCurveIndex = GetCurveIndex( PumpEquip( PumpNum ).PressureCurve_Name );
				if ( TempCurveIndex == 0 ) {
					PumpEquip( PumpNum ).PressureCurve_Index = -1;
				} else {
					TempCurveType = GetCurveType( TempCurveIndex );
					{ auto const SELECT_CASE_var( TempCurveType );
					if ( ( SELECT_CASE_var == "LINEAR" ) || ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) || ( SELECT_CASE_var == "QUARTIC" ) ) {
						PumpEquip( PumpNum ).PressureCurve_Index = TempCurveIndex;
						GetCurveMinMaxValues( TempCurveIndex, PumpEquip( PumpNum ).MinPhiValue, PumpEquip( PumpNum ).MaxPhiValue );
					} else {
						ErrorsFound = true;
					}}
				}
			}

			//read in the rest of the pump pressure characteristics
			PumpEquip( PumpNum ).ImpellerDiameter = rNumericArgs( 6 );
			PumpEquip( PumpNum ).RotSpeed_RPM = rNumericArgs( 7 ); // retrieve the input rotational speed, in revs/min
			PumpEquip( PumpNum ).RotSpeed = PumpEquip( PumpNum ).RotSpeed_RPM / 60.0; //convert input[rpm] to calculation units[rps]

			if ( ! lAlphaFieldBlanks( 7 ) ) { // zone named for pump skin losses
				PumpEquip( PumpNum ).ZoneNum = FindItemInList( cAlphaArgs( 7 ), Zone );
				if ( PumpEquip( PumpNum ).ZoneNum > 0 ) {
					PumpEquip( PumpNum ).HeatLossesToZone = true;
					if ( ! lNumericFieldBlanks( 8 ) ) {
						PumpEquip( PumpNum ).SkinLossRadFraction = rNumericArgs( 8 );
					}
				} else {
					ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 7 ) + "=\"" + cAlphaArgs( 7 ) + "\" not found." );
					ErrorsFound = true;
				}
			}

			if ( ! lAlphaFieldBlanks( 8 ) ) {
				if ( cAlphaArgs( 8 ) == "POWERPERFLOW" ) {
					PumpEquip( PumpNum ).powerSizingMethod = sizePowerPerFlow;
				} else if ( cAlphaArgs( 8 ) == "POWERPERFLOWPERPRESSURE" ) {
					PumpEquip( PumpNum ).powerSizingMethod = sizePowerPerFlowPerPressure;
				} else {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + PumpEquip( PumpNum ).Name + "\", sizing method type entered is invalid.  Use one of the key choice entries." );
					ErrorsFound = true;
				}
			}

			if ( ! lNumericFieldBlanks( 9 ) ) {
				PumpEquip( PumpNum ).powerPerFlowScalingFactor = rNumericArgs( 9 );
			}

			if ( ! lNumericFieldBlanks( 10 ) ) {
				PumpEquip( PumpNum ).powerPerFlowPerPressureScalingFactor = rNumericArgs( 10 );
			}

		}

		// pumps for steam system pumping condensate
		cCurrentModuleObject = cPump_Cond;
		for ( NumCondPump = 1; NumCondPump <= NumCondensatePumps; ++NumCondPump ) {
			PumpNum = NumCondPump + NumVarSpeedPumps + NumConstSpeedPumps;
			GetObjectItem( cCurrentModuleObject, NumCondPump, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), PumpEquip, PumpNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + "  Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			PumpEquip( PumpNum ).Name = cAlphaArgs( 1 );
			PumpEquip( PumpNum ).PumpType = Pump_Cond; //'Pump:VariableSpeed:Condensate'
			PumpEquip( PumpNum ).TypeOf_Num = TypeOf_PumpCondensate;

			PumpEquip( PumpNum ).InletNodeNum = GetOnlySingleNode( cAlphaArgs( 2 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Steam, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

			PumpEquip( PumpNum ).OutletNodeNum = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Steam, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 2 ), cAlphaArgs( 3 ), "Water Nodes" );

			// PumpEquip(PumpNum)%PumpControlType == 'Intermittent'
			PumpEquip( PumpNum ).PumpControl = Intermittent;

			// Input the optional schedule for the pump
			PumpEquip( PumpNum ).PumpSchedule = cAlphaArgs( 4 );
			PumpEquip( PumpNum ).PumpScheduleIndex = GetScheduleIndex( cAlphaArgs( 4 ) );
			if ( ! lAlphaFieldBlanks( 4 ) && ! ( PumpEquip( PumpNum ).PumpScheduleIndex > 0 ) ) {
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + PumpEquip( PumpNum ).Name + "\", Invalid " + cAlphaFieldNames( 4 ) );
				ShowContinueError( "Schedule named =[" + cAlphaArgs( 4 ) + "]. was not found and will not be used." );
			}

			PumpEquip( PumpNum ).NomSteamVolFlowRate = rNumericArgs( 1 );
			if ( PumpEquip( PumpNum ).NomSteamVolFlowRate == AutoSize ) {
				PumpEquip( PumpNum ).NomSteamVolFlowRateWasAutoSized = true;
			}
			PumpEquip( PumpNum ).NomPumpHead = rNumericArgs( 2 );
			PumpEquip( PumpNum ).NomPowerUse = rNumericArgs( 3 );
			if ( PumpEquip( PumpNum ).NomPowerUse == AutoSize ) {
				PumpEquip( PumpNum ).NomPowerUseWasAutoSized = true;
			}
			PumpEquip( PumpNum ).MotorEffic = rNumericArgs( 4 );
			PumpEquip( PumpNum ).FracMotorLossToFluid = rNumericArgs( 5 );
			PumpEquip( PumpNum ).PartLoadCoef( 1 ) = rNumericArgs( 6 );
			PumpEquip( PumpNum ).PartLoadCoef( 2 ) = rNumericArgs( 7 );
			PumpEquip( PumpNum ).PartLoadCoef( 3 ) = rNumericArgs( 8 );
			PumpEquip( PumpNum ).PartLoadCoef( 4 ) = rNumericArgs( 9 );

			if ( ! lAlphaFieldBlanks( 5 ) ) { // zone named for pump skin losses
				PumpEquip( PumpNum ).ZoneNum = FindItemInList( cAlphaArgs( 5 ), Zone );
				if ( PumpEquip( PumpNum ).ZoneNum > 0 ) {
					PumpEquip( PumpNum ).HeatLossesToZone = true;
					if ( ! lNumericFieldBlanks( 10 ) ) {
						PumpEquip( PumpNum ).SkinLossRadFraction = rNumericArgs( 10 );
					}
				} else {
					ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 5 ) + "=\"" + cAlphaArgs( 5 ) + "\" not found." );
					ErrorsFound = true;
				}
			}

			PumpEquip( PumpNum ).MinVolFlowRate = 0.0;
			PumpEquip( PumpNum ).Energy = 0.0;
			PumpEquip( PumpNum ).Power = 0.0;

			if ( PumpEquip( PumpNum ).NomSteamVolFlowRateWasAutoSized ) {
				PumpEquip( PumpNum ).NomVolFlowRate = AutoSize;
				PumpEquip( PumpNum ).NomVolFlowRateWasAutoSized = true;
			} else {
				// Calc Condensate Pump Water Volume Flow Rate
				SteamDensity = GetSatDensityRefrig( fluidNameSteam, StartTemp, 1.0, PumpEquip( PumpNum ).FluidIndex, RoutineNameNoColon );
				TempWaterDensity = GetDensityGlycol( fluidNameWater, InitConvTemp, DummyWaterIndex, RoutineName );
				PumpEquip( PumpNum ).NomVolFlowRate = ( PumpEquip( PumpNum ).NomSteamVolFlowRate * SteamDensity ) / TempWaterDensity;
			}

			if ( ! lAlphaFieldBlanks( 6 ) ) {
				if ( cAlphaArgs( 6 ) == "POWERPERFLOW" ) {
					PumpEquip( PumpNum ).powerSizingMethod = sizePowerPerFlow;
				} else if ( cAlphaArgs( 6 ) == "POWERPERFLOWPERPRESSURE" ) {
					PumpEquip( PumpNum ).powerSizingMethod = sizePowerPerFlowPerPressure;
				} else {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + PumpEquip( PumpNum ).Name + "\", sizing method type entered is invalid.  Use one of the key choice entries." );
					ErrorsFound = true;
				}
			}

			if ( ! lNumericFieldBlanks( 11 ) ) {
				PumpEquip( PumpNum ).powerPerFlowScalingFactor = rNumericArgs( 11 );
			}

			if ( ! lNumericFieldBlanks( 12 ) ) {
				PumpEquip( PumpNum ).powerPerFlowPerPressureScalingFactor = rNumericArgs( 12 );
			}

		}

		//LOAD Variable Speed Pump Bank ARRAYS WITH VARIABLE SPEED CURVE FIT PUMP DATA
		cCurrentModuleObject = cPumpBank_VarSpeed;
		for ( NumVarPumpBankSimple = 1; NumVarPumpBankSimple <= NumPumpBankSimpleVar; ++NumVarPumpBankSimple ) {
			PumpNum = NumVarPumpBankSimple + NumVarSpeedPumps + NumConstSpeedPumps + NumCondensatePumps;
			GetObjectItem( cCurrentModuleObject, NumVarPumpBankSimple, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), PumpEquip, PumpNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			PumpEquip( PumpNum ).Name = cAlphaArgs( 1 );
			PumpEquip( PumpNum ).PumpType = PumpBank_VarSpeed; //'HeaderedPumps:VariableSpeed'
			PumpEquip( PumpNum ).TypeOf_Num = TypeOf_PumpBankVariableSpeed;

			PumpEquip( PumpNum ).InletNodeNum = GetOnlySingleNode( cAlphaArgs( 2 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

			PumpEquip( PumpNum ).OutletNodeNum = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 2 ), cAlphaArgs( 3 ), "Water Nodes" );

			//    PumpEquip(PumpNum)%PumpBankFlowSeqControl = cAlphaArgs(4)
			if ( SameString( cAlphaArgs( 4 ), "Optimal" ) ) {
				PumpEquip( PumpNum ).SequencingScheme = OptimalScheme;
			} else if ( SameString( cAlphaArgs( 4 ), "Sequential" ) ) {
				PumpEquip( PumpNum ).SequencingScheme = SequentialScheme;
			} else if ( SameString( cAlphaArgs( 4 ), "SupplyEquipmentAssigned" ) ) {
				PumpEquip( PumpNum ).SequencingScheme = UserDefined;
			} else {
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + PumpEquip( PumpNum ).Name + "\", Invalid " + cAlphaFieldNames( 4 ) );
				ShowContinueError( "Entered Value=[" + cAlphaArgs( 4 ) + "]. " + cAlphaFieldNames( 4 ) + " has been set to Sequential for this pump." );
				PumpEquip( PumpNum ).SequencingScheme = SequentialScheme;
			}

			//    PumpEquip(PumpNum)%PumpControlType = cAlphaArgs(5)
			if ( SameString( cAlphaArgs( 5 ), "Continuous" ) ) {
				PumpEquip( PumpNum ).PumpControl = Continuous;
			} else if ( SameString( cAlphaArgs( 5 ), "Intermittent" ) ) {
				PumpEquip( PumpNum ).PumpControl = Intermittent;
			} else {
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + PumpEquip( PumpNum ).Name + "\", Invalid " + cAlphaFieldNames( 5 ) );
				ShowContinueError( "Entered Value=[" + cAlphaArgs( 5 ) + "]. " + cAlphaFieldNames( 5 ) + " has been set to Continuous for this pump." );
				PumpEquip( PumpNum ).PumpControl = Continuous;
			}

			// Input the optional schedule for the pump
			PumpEquip( PumpNum ).PumpSchedule = cAlphaArgs( 6 );
			PumpEquip( PumpNum ).PumpScheduleIndex = GetScheduleIndex( cAlphaArgs( 6 ) );
			if ( ! lAlphaFieldBlanks( 6 ) && ! ( PumpEquip( PumpNum ).PumpScheduleIndex > 0 ) ) {
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + PumpEquip( PumpNum ).Name + "\", Invalid " + cAlphaFieldNames( 6 ) );
				ShowContinueError( "Schedule named =[" + cAlphaArgs( 6 ) + "]. was not found and will not be used." );
			}

			PumpEquip( PumpNum ).NomVolFlowRate = rNumericArgs( 1 );
			if ( PumpEquip( PumpNum ).NomVolFlowRate ==  AutoSize ) {
				PumpEquip( PumpNum ).NomVolFlowRateWasAutoSized = true;
			}
			PumpEquip( PumpNum ).NumPumpsInBank = rNumericArgs( 2 );
			PumpEquip( PumpNum ).NomPumpHead = rNumericArgs( 3 );
			PumpEquip( PumpNum ).NomPowerUse = rNumericArgs( 4 );
			if ( PumpEquip( PumpNum ).NomPowerUse == AutoSize ) {
				PumpEquip( PumpNum ).NomPowerUseWasAutoSized = true;
			}
			PumpEquip( PumpNum ).MotorEffic = rNumericArgs( 5 );
			PumpEquip( PumpNum ).FracMotorLossToFluid = rNumericArgs( 6 );
			PumpEquip( PumpNum ).PartLoadCoef( 1 ) = rNumericArgs( 7 );
			PumpEquip( PumpNum ).PartLoadCoef( 2 ) = rNumericArgs( 8 );
			PumpEquip( PumpNum ).PartLoadCoef( 3 ) = rNumericArgs( 9 );
			PumpEquip( PumpNum ).PartLoadCoef( 4 ) = rNumericArgs( 10 );
			PumpEquip( PumpNum ).MinVolFlowRateFrac = rNumericArgs( 11 );
			PumpEquip( PumpNum ).MinVolFlowRate = PumpEquip( PumpNum ).NomVolFlowRate * PumpEquip( PumpNum ).MinVolFlowRateFrac;

			if ( ! lAlphaFieldBlanks( 7 ) ) { // zone named for pump skin losses
				PumpEquip( PumpNum ).ZoneNum = FindItemInList( cAlphaArgs( 7 ), Zone );
				if ( PumpEquip( PumpNum ).ZoneNum > 0 ) {
					PumpEquip( PumpNum ).HeatLossesToZone = true;
					if ( ! lNumericFieldBlanks( 12 ) ) {
						PumpEquip( PumpNum ).SkinLossRadFraction = rNumericArgs( 12 );
					}
				} else {
					ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 7 ) + "=\"" + cAlphaArgs( 7 ) + "\" not found." );
					ErrorsFound = true;
				}
			}

			if ( ! lAlphaFieldBlanks( 8 ) ) {
				if ( cAlphaArgs( 8 ) == "POWERPERFLOW" ) {
					PumpEquip( PumpNum ).powerSizingMethod = sizePowerPerFlow;
				} else if ( cAlphaArgs( 8 ) == "POWERPERFLOWPERPRESSURE" ) {
					PumpEquip( PumpNum ).powerSizingMethod = sizePowerPerFlowPerPressure;
				} else {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + PumpEquip( PumpNum ).Name + "\", sizing method type entered is invalid.  Use one of the key choice entries." );
					ErrorsFound = true;
				}
			}

			if ( ! lNumericFieldBlanks( 13 ) ) {
				PumpEquip( PumpNum ).powerPerFlowScalingFactor = rNumericArgs( 13 );
			}

			if ( ! lNumericFieldBlanks( 14 ) ) {
				PumpEquip( PumpNum ).powerPerFlowPerPressureScalingFactor = rNumericArgs( 14 );
			}

			PumpEquip( PumpNum ).Energy = 0.0;
			PumpEquip( PumpNum ).Power = 0.0;
		}

		cCurrentModuleObject = cPumpBank_ConSpeed;
		for ( NumConstPumpBankSimple = 1; NumConstPumpBankSimple <= NumPumpBankSimpleConst; ++NumConstPumpBankSimple ) {
			PumpNum = NumConstPumpBankSimple + NumVarSpeedPumps + NumConstSpeedPumps + NumCondensatePumps + NumPumpBankSimpleVar;
			GetObjectItem( cCurrentModuleObject, NumConstPumpBankSimple, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), PumpEquip, PumpNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			PumpEquip( PumpNum ).Name = cAlphaArgs( 1 );
			PumpEquip( PumpNum ).PumpType = PumpBank_ConSpeed; //'HeaderedPumps:ConstantSpeed'
			PumpEquip( PumpNum ).TypeOf_Num = TypeOf_PumpBankConstantSpeed;

			PumpEquip( PumpNum ).InletNodeNum = GetOnlySingleNode( cAlphaArgs( 2 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

			PumpEquip( PumpNum ).OutletNodeNum = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 2 ), cAlphaArgs( 3 ), "Water Nodes" );

			//    PumpEquip(PumpNum)%PumpBankFlowSeqControl = cAlphaArgs(4)
			if ( SameString( cAlphaArgs( 4 ), "Optimal" ) ) {
				PumpEquip( PumpNum ).SequencingScheme = OptimalScheme;
			} else if ( SameString( cAlphaArgs( 4 ), "Sequential" ) ) {
				PumpEquip( PumpNum ).SequencingScheme = SequentialScheme;
			} else {
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + PumpEquip( PumpNum ).Name + "\", Invalid " + cAlphaFieldNames( 4 ) );
				ShowContinueError( "Entered Value=[" + cAlphaArgs( 4 ) + "]. " + cAlphaFieldNames( 4 ) + " has been set to Sequential for this pump." );
				PumpEquip( PumpNum ).SequencingScheme = SequentialScheme;
				//      PumpEquip(PumpNum)%PumpBankFlowSeqControl = 'Optimal'
			}

			//    PumpEquip(PumpNum)%PumpControlType = cAlphaArgs(5)
			if ( SameString( cAlphaArgs( 5 ), "Continuous" ) ) {
				PumpEquip( PumpNum ).PumpControl = Continuous;
			} else if ( SameString( cAlphaArgs( 5 ), "Intermittent" ) ) {
				PumpEquip( PumpNum ).PumpControl = Intermittent;
			} else {
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + PumpEquip( PumpNum ).Name + "\", Invalid " + cAlphaFieldNames( 5 ) );
				ShowContinueError( "Entered Value=[" + cAlphaArgs( 5 ) + "]. " + cAlphaFieldNames( 5 ) + " has been set to Continuous for this pump." );
				PumpEquip( PumpNum ).PumpControl = Continuous;
			}

			// Input the optional schedule for the pump
			PumpEquip( PumpNum ).PumpSchedule = cAlphaArgs( 6 );
			PumpEquip( PumpNum ).PumpScheduleIndex = GetScheduleIndex( cAlphaArgs( 6 ) );
			if ( ! lAlphaFieldBlanks( 6 ) && ! ( PumpEquip( PumpNum ).PumpScheduleIndex > 0 ) ) {
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + PumpEquip( PumpNum ).Name + "\", Invalid " + cAlphaFieldNames( 6 ) );
				ShowContinueError( "Schedule named =[" + cAlphaArgs( 6 ) + "]. was not found and will not be used." );
			}

			PumpEquip( PumpNum ).NomVolFlowRate = rNumericArgs( 1 );
			if ( PumpEquip( PumpNum ).NomVolFlowRate ==  AutoSize ) {
				PumpEquip( PumpNum ).NomVolFlowRateWasAutoSized = true;
			}
			PumpEquip( PumpNum ).NumPumpsInBank = rNumericArgs( 2 );
			PumpEquip( PumpNum ).NomPumpHead = rNumericArgs( 3 );
			PumpEquip( PumpNum ).NomPowerUse = rNumericArgs( 4 );
			if ( PumpEquip( PumpNum ).NomPowerUse == AutoSize ) {
				PumpEquip( PumpNum ).NomPowerUseWasAutoSized = true;
			}
			PumpEquip( PumpNum ).MotorEffic = rNumericArgs( 5 );
			PumpEquip( PumpNum ).FracMotorLossToFluid = rNumericArgs( 6 );
			PumpEquip( PumpNum ).PartLoadCoef( 1 ) = 1.0;
			PumpEquip( PumpNum ).PartLoadCoef( 2 ) = 0.0;
			PumpEquip( PumpNum ).PartLoadCoef( 3 ) = 0.0;
			PumpEquip( PumpNum ).PartLoadCoef( 4 ) = 0.0;
			//    PumpEquip(PumpNum)%MinVolFlowRateFrac  = rNumericArgs(11)
			//    PumpEquip(PumpNum)%MinVolFlowRate      = PumpEquip(PumpNum)%NomVolFlowRate * PumpEquip(PumpNum)%MinVolFlowRateFrac
			//DSU?  need a value set for %MinVolFlowRate ?? zero? NomVolFlowRate?

			if ( ! lAlphaFieldBlanks( 7 ) ) { // zone named for pump skin losses
				PumpEquip( PumpNum ).ZoneNum = FindItemInList( cAlphaArgs( 7 ), Zone );
				if ( PumpEquip( PumpNum ).ZoneNum > 0 ) {
					PumpEquip( PumpNum ).HeatLossesToZone = true;
					if ( ! lNumericFieldBlanks( 7 ) ) {
						PumpEquip( PumpNum ).SkinLossRadFraction = rNumericArgs( 7 );
					}
				} else {
					ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 7 ) + "=\"" + cAlphaArgs( 7 ) + "\" not found." );
					ErrorsFound = true;
				}
			}
			if ( ! lAlphaFieldBlanks( 8 ) ) {
				if ( cAlphaArgs( 8 ) == "POWERPERFLOW" ) {
					PumpEquip( PumpNum ).powerSizingMethod = sizePowerPerFlow;
				} else if ( cAlphaArgs( 8 ) == "POWERPERFLOWPERPRESSURE" ) {
					PumpEquip( PumpNum ).powerSizingMethod = sizePowerPerFlowPerPressure;
				} else {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + PumpEquip( PumpNum ).Name + "\", sizing method type entered is invalid.  Use one of the key choice entries." );
					ErrorsFound = true;
				}
			}

			if ( ! lNumericFieldBlanks( 8 ) ) {
				PumpEquip( PumpNum ).powerPerFlowScalingFactor = rNumericArgs( 8 );
			}

			if ( ! lNumericFieldBlanks( 9 ) ) {
				PumpEquip( PumpNum ).powerPerFlowPerPressureScalingFactor = rNumericArgs( 9 );
			}
			PumpEquip( PumpNum ).MinVolFlowRate = 0.0;
			PumpEquip( PumpNum ).Energy = 0.0;
			PumpEquip( PumpNum ).Power = 0.0;
		}

		if ( ErrorsFound ) {
			ShowFatalError( "Errors found in getting Pump input" );
		}

		for ( PumpNum = 1; PumpNum <= NumPumps; ++PumpNum ) { //CurrentModuleObject='Pumps'
			if ( PumpEquip( PumpNum ).PumpType == Pump_VarSpeed || PumpEquip( PumpNum ).PumpType == Pump_ConSpeed || PumpEquip( PumpNum ).PumpType == Pump_Cond ) {

				SetupOutputVariable( "Pump Electric Energy [J]", PumpEquip( PumpNum ).Energy, "System", "Sum", PumpEquip( PumpNum ).Name, _, "Electric", "Pumps", _, "Plant" );
				SetupOutputVariable( "Pump Electric Power [W]", PumpEquip( PumpNum ).Power, "System", "Average", PumpEquip( PumpNum ).Name );
				SetupOutputVariable( "Pump Shaft Power [W]", PumpEquipReport( PumpNum ).ShaftPower, "System", "Average", PumpEquip( PumpNum ).Name );
				SetupOutputVariable( "Pump Fluid Heat Gain Rate [W]", PumpEquipReport( PumpNum ).PumpHeattoFluid, "System", "Average", PumpEquip( PumpNum ).Name );
				SetupOutputVariable( "Pump Fluid Heat Gain Energy [J]", PumpEquipReport( PumpNum ).PumpHeattoFluidEnergy, "System", "Sum", PumpEquip( PumpNum ).Name );
				SetupOutputVariable( "Pump Outlet Temperature [C]", PumpEquipReport( PumpNum ).OutletTemp, "System", "Average", PumpEquip( PumpNum ).Name );
				SetupOutputVariable( "Pump Mass Flow Rate [kg/s]", PumpEquipReport( PumpNum ).PumpMassFlowRate, "System", "Average", PumpEquip( PumpNum ).Name );

			}
			if ( PumpEquip( PumpNum ).PumpType == PumpBank_VarSpeed || PumpEquip( PumpNum ).PumpType == PumpBank_ConSpeed ) { // CurrentModuleObject='HeaderedPumps'

				SetupOutputVariable( "Pump Electric Energy [J]", PumpEquip( PumpNum ).Energy, "System", "Sum", PumpEquip( PumpNum ).Name, _, "Electric", "Pumps", _, "Plant" );
				SetupOutputVariable( "Pump Electric Power [W]", PumpEquip( PumpNum ).Power, "System", "Average", PumpEquip( PumpNum ).Name );
				SetupOutputVariable( "Pump Shaft Power [W]", PumpEquipReport( PumpNum ).ShaftPower, "System", "Average", PumpEquip( PumpNum ).Name );
				SetupOutputVariable( "Pump Fluid Heat Gain Rate [W]", PumpEquipReport( PumpNum ).PumpHeattoFluid, "System", "Average", PumpEquip( PumpNum ).Name );
				SetupOutputVariable( "Pump Fluid Heat Gain Energy [J]", PumpEquipReport( PumpNum ).PumpHeattoFluidEnergy, "System", "Sum", PumpEquip( PumpNum ).Name );
				SetupOutputVariable( "Pump Outlet Temperature [C]", PumpEquipReport( PumpNum ).OutletTemp, "System", "Average", PumpEquip( PumpNum ).Name );
				SetupOutputVariable( "Pump Mass Flow Rate [kg/s]", PumpEquipReport( PumpNum ).PumpMassFlowRate, "System", "Average", PumpEquip( PumpNum ).Name );
				SetupOutputVariable( "Pump Operating Pumps Count []", PumpEquipReport( PumpNum ).NumPumpsOperating, "System", "Average", PumpEquip( PumpNum ).Name );
			}

			if ( AnyEnergyManagementSystemInModel ) {
				SetupEMSInternalVariable( "Pump Maximum Mass Flow Rate", PumpEquip( PumpNum ).Name, "[kg/s]", PumpEquip( PumpNum ).MassFlowRateMax );
				SetupEMSActuator( "Pump", PumpEquip( PumpNum ).Name, "Pump Mass Flow Rate", "[kg/s]", PumpEquip( PumpNum ).EMSMassFlowOverrideOn, PumpEquip( PumpNum ).EMSMassFlowValue );
				SetupEMSActuator( "Pump", PumpEquip( PumpNum ).Name, "Pump Pressure Rise", "[Pa]", PumpEquip( PumpNum ).EMSPressureOverrideOn, PumpEquip( PumpNum ).EMSPressureOverrideValue );
			}

			if ( PumpEquip( PumpNum ).HeatLossesToZone ) {
				// setup skin loss output vars
				SetupOutputVariable( "Pump Zone Total Heating Rate [W]", PumpEquipReport( PumpNum ).ZoneTotalGainRate, "System", "Average", PumpEquip( PumpNum ).Name );
				SetupOutputVariable( "Pump Zone Total Heating Energy [J]", PumpEquipReport( PumpNum ).ZoneTotalGainEnergy, "System", "Sum", PumpEquip( PumpNum ).Name );
				SetupOutputVariable( "Pump Zone Convective Heating Rate [W]", PumpEquipReport( PumpNum ).ZoneConvGainRate, "System", "Average", PumpEquip( PumpNum ).Name );
				SetupOutputVariable( "Pump Zone Radiative Heating Rate [W]", PumpEquipReport( PumpNum ).ZoneRadGainRate, "System", "Average", PumpEquip( PumpNum ).Name );

				// setup internal gains
				{ auto const SELECT_CASE_var( PumpEquip( PumpNum ).PumpType );
				if ( SELECT_CASE_var == Pump_VarSpeed ) {
					SetupZoneInternalGain( PumpEquip( PumpNum ).ZoneNum, "Pump:VariableSpeed", PumpEquip( PumpNum ).Name, IntGainTypeOf_Pump_VarSpeed, PumpEquipReport( PumpNum ).ZoneConvGainRate, _, PumpEquipReport( PumpNum ).ZoneRadGainRate );
				} else if ( SELECT_CASE_var == Pump_ConSpeed ) {
					SetupZoneInternalGain( PumpEquip( PumpNum ).ZoneNum, "Pump:ConstantSpeed", PumpEquip( PumpNum ).Name, IntGainTypeOf_Pump_ConSpeed, PumpEquipReport( PumpNum ).ZoneConvGainRate, _, PumpEquipReport( PumpNum ).ZoneRadGainRate );
				} else if ( SELECT_CASE_var == Pump_Cond ) {
					SetupZoneInternalGain( PumpEquip( PumpNum ).ZoneNum, "Pump:VariableSpeed:Condensate", PumpEquip( PumpNum ).Name, IntGainTypeOf_Pump_Cond, PumpEquipReport( PumpNum ).ZoneConvGainRate, _, PumpEquipReport( PumpNum ).ZoneRadGainRate );
				} else if ( SELECT_CASE_var == PumpBank_VarSpeed ) {
					SetupZoneInternalGain( PumpEquip( PumpNum ).ZoneNum, "HeaderedPumps:VariableSpeed", PumpEquip( PumpNum ).Name, IntGainTypeOf_PumpBank_VarSpeed, PumpEquipReport( PumpNum ).ZoneConvGainRate, _, PumpEquipReport( PumpNum ).ZoneRadGainRate );
				} else if ( SELECT_CASE_var == PumpBank_ConSpeed ) {
					SetupZoneInternalGain( PumpEquip( PumpNum ).ZoneNum, "HeaderedPumps:ConstantSpeed", PumpEquip( PumpNum ).Name, IntGainTypeOf_PumpBank_ConSpeed, PumpEquipReport( PumpNum ).ZoneConvGainRate, _, PumpEquipReport( PumpNum ).ZoneRadGainRate );
				}}

			}

		}

	}

	//*************************************************************************!

	//*************************************************************************!

	void
	InitializePumps( int const PumpNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR:        Edwin Lee
		//       DATE WRITTEN:  August 2010
		//       MODIFIED       Based on the INIT section of InitSimVars, credits here:
		//                        Author:
		//                          Oct 1998 Dan Fisher
		//                        Modifications:
		//                          Jul 2001 Richard Liesen
		//                          July 2001, Rick Strand (implemented new pump controls)
		//                          May 2009, Brent Griffith (added EMS override capability)
		//                          Nov 2010, Brent Griffith (call InitComponentNodes, generalize fluid props)
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine does one-time and begin-envrn inits for the pump

		// Using/Aliasing
		using General::RoundSigDigits;
		using DataPlant::ScanPlantLoopsForObject;
		using DataPlant::PlantLoop;
		using DataPlant::LoopFlowStatus_NeedyAndTurnsLoopOn;
		using DataPlant::PlantReSizingCompleted;
		using DataGlobals::RedoSizesHVACSimulation;
		using FluidProperties::GetSatDensityRefrig;
		using FluidProperties::GetDensityGlycol;
		using PlantUtilities::InitComponentNodes;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const StartTemp( 100.0 ); // Standard Temperature across code to calculated Steam density
		Real64 const ZeroPowerTol( 0.0000001 );
		static std::string const RoutineName( "PlantPumps::InitializePumps " );

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int InletNode; // pump inlet node number
		int OutletNode; // pump outlet node number
		Real64 TotalEffic;
		Real64 SteamDensity; // Density of working fluid
		static int DummyWaterIndex( 1 );
		Real64 TempWaterDensity;
		bool errFlag;
		Real64 mdotMax; // local fluid mass flow rate maximum
		Real64 mdotMin; // local fluid mass flow rate minimum
		int plloopnum;
		int lsnum;
		int brnum;
		int cpnum;

		// Set some variables for convenience
		InletNode = PumpEquip( PumpNum ).InletNodeNum;
		OutletNode = PumpEquip( PumpNum ).OutletNodeNum;

		// One time inits
		if ( PumpEquip( PumpNum ).PumpOneTimeFlag ) {

			errFlag = false;
			ScanPlantLoopsForObject( PumpEquip( PumpNum ).Name, PumpEquip( PumpNum ).TypeOf_Num, PumpEquip( PumpNum ).LoopNum, PumpEquip( PumpNum ).LoopSideNum, PumpEquip( PumpNum ).BranchNum, PumpEquip( PumpNum ).CompNum, _, _, _, _, _, errFlag );
			plloopnum = PumpEquip( PumpNum ).LoopNum;
			lsnum = PumpEquip( PumpNum ).LoopSideNum;
			brnum = PumpEquip( PumpNum ).BranchNum;
			cpnum = PumpEquip( PumpNum ).CompNum;
			if ( plloopnum > 0 && lsnum > 0 && brnum > 0 && cpnum > 0 ) {
				if ( PlantLoop( plloopnum ).LoopSide( lsnum ).Branch( brnum ).Comp( cpnum ).NodeNumIn != InletNode || PlantLoop( plloopnum ).LoopSide( lsnum ).Branch( brnum ).Comp( cpnum ).NodeNumOut != OutletNode ) {
					ShowSevereError( "InitializePumps: " + cPumpTypes( PumpEquip( PumpNum ).PumpType ) + "=\"" + PumpEquip( PumpNum ).Name + "\", non-matching nodes." );
					ShowContinueError( "...in Branch=\"" + PlantLoop( plloopnum ).LoopSide( lsnum ).Branch( brnum ).Name + "\", Component referenced with:" );
					ShowContinueError( "...Inlet Node=\"" + NodeID( PlantLoop( plloopnum ).LoopSide( lsnum ).Branch( brnum ).Comp( cpnum ).NodeNumIn ) );
					ShowContinueError( "...Outlet Node=\"" + NodeID( PlantLoop( plloopnum ).LoopSide( lsnum ).Branch( brnum ).Comp( cpnum ).NodeNumOut ) );
					ShowContinueError( "...Pump Inlet Node=\"" + NodeID( InletNode ) );
					ShowContinueError( "...Pump Outlet Node=\"" + NodeID( OutletNode ) );
					errFlag = true;
				}
			} else { // CR9292
				ShowSevereError( "InitializePumps: " + cPumpTypes( PumpEquip( PumpNum ).PumpType ) + "=\"" + PumpEquip( PumpNum ).Name + "\", component missing." );
				errFlag = true; // should have received warning/severe earlier, will reiterate
			}

			if ( errFlag ) {
				ShowFatalError( "InitializePumps: Program terminated due to previous condition(s)." );
			}
			PlantLoop( PumpEquip( PumpNum ).LoopNum ).LoopSide( PumpEquip( PumpNum ).LoopSideNum ).Branch( PumpEquip( PumpNum ).BranchNum ).Comp( PumpEquip( PumpNum ).CompNum ).CompNum = PumpNum;

			SizePump( PumpNum );

			// calculate the efficiency for each pump
			// by calculating the efficiency for each pump being simulated.  The calculation
			// is based on the PMPSIM code in the ASHRAE Secondary Toolkit
			if ( PumpEquip( PumpNum ).NomPowerUse > ZeroPowerTol && PumpEquip( PumpNum ).MotorEffic > ZeroPowerTol ) {
				TotalEffic = PumpEquip( PumpNum ).NomVolFlowRate * PumpEquip( PumpNum ).NomPumpHead / PumpEquip( PumpNum ).NomPowerUse;
				PumpEquip( PumpNum ).PumpEffic = TotalEffic / PumpEquip( PumpNum ).MotorEffic;
				if ( PumpEquip( PumpNum ).PumpEffic < 0.50 ) {
					ShowWarningError( "Check input. Calculated Pump Efficiency=" + RoundSigDigits( PumpEquip( PumpNum ).PumpEffic * 100.0, 2 ) + "% which is less than 50%, for pump=" + PumpEquip( PumpNum ).Name );
					ShowContinueError( "Calculated Pump_Efficiency % =Total_Efficiency % [" + RoundSigDigits( TotalEffic * 100.0, 1 ) + "] / Motor_Efficiency % [" + RoundSigDigits( PumpEquip( PumpNum ).MotorEffic * 100.0, 1 ) + ']' );
					ShowContinueError( "Total_Efficiency % =(Rated_Volume_Flow_Rate [" + RoundSigDigits( PumpEquip( PumpNum ).NomVolFlowRate, 1 ) + "] * Rated_Pump_Head [" + RoundSigDigits( PumpEquip( PumpNum ).NomPumpHead, 1 ) + "] / Rated_Power_Use [" + RoundSigDigits( PumpEquip( PumpNum ).NomPowerUse, 1 ) + "]) * 100." );
				} else if ( ( PumpEquip( PumpNum ).PumpEffic > 0.95 ) && ( PumpEquip( PumpNum ).PumpEffic <= 1.0 ) ) {
					ShowWarningError( "Check input.  Calculated Pump Efficiency=" + RoundSigDigits( PumpEquip( PumpNum ).PumpEffic * 100.0, 2 ) + "% is approaching 100%, for pump=" + PumpEquip( PumpNum ).Name );
					ShowContinueError( "Calculated Pump_Efficiency % =Total_Efficiency % [" + RoundSigDigits( TotalEffic * 100.0, 1 ) + "] / Motor_Efficiency % [" + RoundSigDigits( PumpEquip( PumpNum ).MotorEffic * 100.0, 1 ) + ']' );
					ShowContinueError( "Total_Efficiency % =(Rated_Volume_Flow_Rate [" + RoundSigDigits( PumpEquip( PumpNum ).NomVolFlowRate, 1 ) + "] * Rated_Pump_Head [" + RoundSigDigits( PumpEquip( PumpNum ).NomPumpHead, 1 ) + "] / Rated_Power_Use [" + RoundSigDigits( PumpEquip( PumpNum ).NomPowerUse, 1 ) + "]) * 100." );
				} else if ( PumpEquip( PumpNum ).PumpEffic > 1.0 ) {
					ShowSevereError( "Check input.  Calculated Pump Efficiency=" + RoundSigDigits( PumpEquip( PumpNum ).PumpEffic * 100.0, 3 ) + "% which is bigger than 100%, for pump=" + PumpEquip( PumpNum ).Name );
					ShowContinueError( "Calculated Pump_Efficiency % =Total_Efficiency % [" + RoundSigDigits( TotalEffic * 100.0, 1 ) + "] / Motor_Efficiency % [" + RoundSigDigits( PumpEquip( PumpNum ).MotorEffic * 100.0, 1 ) + ']' );
					ShowContinueError( "Total_Efficiency % =(Rated_Volume_Flow_Rate [" + RoundSigDigits( PumpEquip( PumpNum ).NomVolFlowRate, 1 ) + "] * Rated_Pump_Head [" + RoundSigDigits( PumpEquip( PumpNum ).NomPumpHead, 1 ) + "] / Rated_Power_Use [" + RoundSigDigits( PumpEquip( PumpNum ).NomPowerUse, 1 ) + "]) * 100." );
					ShowFatalError( "Errors found in Pump input" );
				}
			} else {
				ShowWarningError( "Check input. Pump nominal power or motor efficiency is set to 0, for pump=" + PumpEquip( PumpNum ).Name );
			}

			if ( PumpEquip( PumpNum ).NomVolFlowRate <= SmallWaterVolFlow ) {
				ShowWarningError( "Check input. Pump nominal flow rate is set or calculated = 0, for pump=" + PumpEquip( PumpNum ).Name );
			}

			if ( PumpEquip( PumpNum ).PumpControl == Continuous ) {
				// reset flow priority appropriately (default was for Intermittent)
				PlantLoop( PumpEquip( PumpNum ).LoopNum ).LoopSide( PumpEquip( PumpNum ).LoopSideNum ).Branch( PumpEquip( PumpNum ).BranchNum ).Comp( PumpEquip( PumpNum ).CompNum ).FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
			}

			PumpEquip( PumpNum ).PumpOneTimeFlag = false;
		}

		//HVAC Sizing Simulation resizing calls if needed
		if ( RedoSizesHVACSimulation && ! PlantReSizingCompleted ) {
			SizePump( PumpNum );
		}


		// Begin environment inits
		//DSU? Still need to clean this up and update condensate pump stuff -
		//     BG cleaned to call initComponentnodes, not sure what else may be needed if anything
		if ( PumpEquip( PumpNum ).PumpInitFlag && BeginEnvrnFlag ) {
			if ( PumpEquip( PumpNum ).PumpType == Pump_Cond ) {

				TempWaterDensity = GetDensityGlycol( fluidNameWater, InitConvTemp, DummyWaterIndex, RoutineName );
				SteamDensity = GetSatDensityRefrig( fluidNameSteam, StartTemp, 1.0, PumpEquip( PumpNum ).FluidIndex, RoutineName );
				PumpEquip( PumpNum ).NomVolFlowRate = ( PumpEquip( PumpNum ).NomSteamVolFlowRate * SteamDensity ) / TempWaterDensity;

				//set the maximum flow rate on the outlet node
				mdotMax = PumpEquip( PumpNum ).NomSteamVolFlowRate * SteamDensity;
				//mdotMin = PumpEquip(PumpNum)%MinVolFlowRate      * SteamDensity
				//On a pump the 'hardware min' (MassFlowRateMin) must be defined as zero and not
				//confused with the desired pump operating scheme or the user specified
				//'minimum flow rate'.  The user specified 'minimum flow rate' determines the minumum
				//flow rate under normal operating conditions.  For cases when 'MaxAvail' on the pump
				//inlet node actually less than the 'minimum flow rate' specified by the user, than a
				//loop shutdown must  be triggered.
				mdotMin = 0.0;
				InitComponentNodes( mdotMin, mdotMax, InletNode, OutletNode, PumpEquip( PumpNum ).LoopNum, PumpEquip( PumpNum ).LoopSideNum, PumpEquip( PumpNum ).BranchNum, PumpEquip( PumpNum ).CompNum );
				PumpEquip( PumpNum ).MassFlowRateMax = mdotMax;
				PumpEquip( PumpNum ).MassFlowRateMin = PumpEquip( PumpNum ).MinVolFlowRate * SteamDensity;

			} else {
				TempWaterDensity = GetDensityGlycol( PlantLoop( PumpEquip( PumpNum ).LoopNum ).FluidName, InitConvTemp, PlantLoop( PumpEquip( PumpNum ).LoopNum ).FluidIndex, RoutineName );
				mdotMax = PumpEquip( PumpNum ).NomVolFlowRate * TempWaterDensity;
				//mdotMin = PumpEquip(PumpNum)%MinVolFlowRate * TempWaterDensity
				//see note above
				mdotMin = 0.0;
				InitComponentNodes( mdotMin, mdotMax, InletNode, OutletNode, PumpEquip( PumpNum ).LoopNum, PumpEquip( PumpNum ).LoopSideNum, PumpEquip( PumpNum ).BranchNum, PumpEquip( PumpNum ).CompNum );
				PumpEquip( PumpNum ).MassFlowRateMax = mdotMax;
				PumpEquip( PumpNum ).MassFlowRateMin = PumpEquip( PumpNum ).MinVolFlowRate * TempWaterDensity;

			}
			//zero out report variables
			PumpEquip( PumpNum ).Energy = 0.0;
			PumpEquip( PumpNum ).Power = 0.0;
			PumpEquipReport( PumpNum ).ShaftPower = 0.0;
			PumpEquipReport( PumpNum ).PumpHeattoFluid = 0.0;
			PumpEquipReport( PumpNum ).PumpHeattoFluidEnergy = 0.0;
			PumpEquipReport( PumpNum ).OutletTemp = 0.0;
			PumpEquipReport( PumpNum ).PumpMassFlowRate = 0.0;
			PumpEquipReport( PumpNum ).NumPumpsOperating = 0;
			PumpEquipReport( PumpNum ).ZoneTotalGainRate = 0.0;
			PumpEquipReport( PumpNum ).ZoneTotalGainEnergy = 0.0;
			PumpEquipReport( PumpNum ).ZoneConvGainRate = 0.0;
			PumpEquipReport( PumpNum ).ZoneRadGainRate = 0.0;

			PumpEquip( PumpNum ).PumpInitFlag = false;

		}

		// Reset the local environment flag for the next environment
		if ( ! BeginEnvrnFlag ) PumpEquip( PumpNum ).PumpInitFlag = true;

		// zero out module level working variables
		PumpMassFlowRate = 0.0;
		PumpHeattoFluid = 0.0;
		Power = 0.0;
		ShaftPower = 0.0;

	}

	//*************************************************************************!

	//*************************************************************************!

	void
	SetupPumpMinMaxFlows(
		int const LoopNum,
		int const PumpNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR:        Edwin Lee
		//       DATE WRITTEN:  Aug 2010
		//       MODIFIED       Based on the Flow control portion of what was previously Pumps::InitSimVars, by:
		//                        Dan Fisher October 1998
		//                        Richard Liesen July 2001
		//                        July 2001, Rick Strand (implemented new pump controls)
		//                        May 2009, Brent Griffith (added EMS override capability)
		//                        B. Griffith, Nov 2011 Pump control: Intermittent vs Continuous
		//       RE-ENGINEERED

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine initializes the pump minAvail and maxAvail flow rates, and assigns them to the
		//  outlet min/max avail according to inlet min/max constraints and zero flow request
		// The loop solver then uses this information to set up the flow bounds for the loop side
		//  for the current iteration.

		// METHODOLOGY EMPLOYED:
		//  Design flow rate and user specified minimum flow rate is compared in the inlet node
		//  min/maxavail.  The pump output is appropriately constrained.
		//  Design flow is rated flow times schedule fraction
		//  Inlet node max will represent the rated flow rate according to pump init routines.
		//  These values are bounded by hardware min constraints on the inlet node, which is likely zero.
		//  These values are also bounded by EMS overridable limit of max flow rate.

		// Using/Aliasing
		using ScheduleManager::GetCurrentScheduleValue;
		using FluidProperties::GetDensityGlycol;
		using DataPlant::PlantLoop;
		using DataPlant::Press_FlowCorrection;
		using DataPlant::PlantAvailMgr;
		using PlantPressureSystem::ResolveLoopFlowVsPressure;
		using PlantUtilities::BoundValueToWithinTwoValues;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "PlantPumps:SetupPumpMinMaxFlows: " );

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int InletNode; // pump inlet node number
		int OutletNode; // pump outlet node number
		Real64 InletNodeMax;
		Real64 InletNodeMin;
		Real64 PumpMassFlowRateMax; // max allowable flow rate at the pump
		Real64 PumpMassFlowRateMin; // min allowable flow rate at the pump
		Real64 PumpSchedFraction;
		Real64 PumpOverridableMaxLimit;
		Real64 PumpMassFlowRateMinLimit;
		Real64 PumpSchedRPM; // Pump RPM Optional Input

		//Inlet/Outlet Node Numbers
		InletNode = PumpEquip( PumpNum ).InletNodeNum;
		OutletNode = PumpEquip( PumpNum ).OutletNodeNum;

		//Inlet node Min/MaxAvail
		InletNodeMax = Node( InletNode ).MassFlowRateMaxAvail;
		InletNodeMin = Node( InletNode ).MassFlowRateMinAvail;

		//Retrive the pump speed fraction from the pump schedule
		if ( PumpEquip( PumpNum ).PumpScheduleIndex != 0 ) {
			PumpSchedFraction = GetCurrentScheduleValue( PumpEquip( PumpNum ).PumpScheduleIndex );
			PumpSchedFraction = BoundValueToWithinTwoValues( PumpSchedFraction, 0.0, 1.0 );
		} else {
			PumpSchedFraction = 1.0;
		}

		//User specified min/max mass flow rates for pump
		PumpOverridableMaxLimit = PumpEquip( PumpNum ).MassFlowRateMax;

		// override the user specified min to allow pump to turn off when no flow is required.
		if ( PumpEquip( PumpNum ).LoopSolverOverwriteFlag ) {
			PumpMassFlowRateMinLimit = 0.0;
		} else {
			PumpMassFlowRateMinLimit = PumpEquip( PumpNum ).MassFlowRateMin;
		}


		//The pump outlet node Min/MaxAvail
		PumpMassFlowRateMin = max( InletNodeMin, PumpMassFlowRateMinLimit );
		PumpMassFlowRateMax = min( InletNodeMax, PumpOverridableMaxLimit * PumpSchedFraction );

		//Check for conflicts (MaxAvail < MinAvail)
		if ( PumpMassFlowRateMin > PumpMassFlowRateMax ) { //the demand side wants to operate outside of the pump range
			//shut the pump (and the loop) down
			PumpMassFlowRateMin = 0.0;
			PumpMassFlowRateMax = 0.0;
			//Let the user know that his input file is overconstrained
			//DSU? Call one-time warning...with a counter
		}

		//DSU? IF (EMS ACTIVE) THEN...
		//DSU?         PumpMassFlowRateMax = MIN(PumpMassFlowRateMax, PumpOverridableMaxLimit) !Allow override by EMS

		{ auto const SELECT_CASE_var( PumpEquip( PumpNum ).PumpType );

		if ( SELECT_CASE_var == Pump_VarSpeed ) {

			if ( PumpEquip( PumpNum ).HasVFD ) {
				{ auto const SELECT_CASE_var1( PumpEquip( PumpNum ).VFD.VFDControlType );
				if ( SELECT_CASE_var1 == VFDManual ) {

					//Evaluate the schedule if it exists and put the fraction into a local variable
					PumpSchedRPM = GetCurrentScheduleValue( PumpEquip( PumpNum ).VFD.ManualRPMSchedIndex );
					//Convert the RPM to rot/sec for calculation routine
					PumpEquip( PumpNum ).RotSpeed = PumpSchedRPM / 60.0;
					//Resolve the new mass flow rate based on current pressure characteristics
					if ( PlantLoop( PumpEquip( PumpNum ).LoopNum ).UsePressureForPumpCalcs && PlantLoop( PumpEquip( PumpNum ).LoopNum ).PressureSimType == Press_FlowCorrection && PlantLoop( PumpEquip( PumpNum ).LoopNum ).PressureDrop > 0.0 ) {

						PumpMassFlowRate = ResolveLoopFlowVsPressure( PumpEquip( PumpNum ).LoopNum, Node( PumpEquip( PumpNum ).InletNodeNum ).MassFlowRate, PumpEquip( PumpNum ).PressureCurve_Index, PumpEquip( PumpNum ).RotSpeed, PumpEquip( PumpNum ).ImpellerDiameter, PumpEquip( PumpNum ).MinPhiValue, PumpEquip( PumpNum ).MaxPhiValue );

						PumpMassFlowRateMax = PumpMassFlowRate;
						PumpMassFlowRateMin = PumpMassFlowRate;

					}

				} else if ( SELECT_CASE_var1 == VFDAutomatic ) {

					if ( PlantLoop( PumpEquip( PumpNum ).LoopNum ).UsePressureForPumpCalcs && PlantLoop( PumpEquip( PumpNum ).LoopNum ).PressureSimType == Press_FlowCorrection && PlantLoop( PumpEquip( PumpNum ).LoopNum ).PressureDrop > 0.0 ) {

						GetRequiredMassFlowRate( LoopNum, PumpNum, Node( PumpEquip( PumpNum ).InletNodeNum ).MassFlowRate, PumpMassFlowRate, PumpMassFlowRateMin, PumpMassFlowRateMax );

					}

				}} //VFDControlType

			}

			if ( PumpEquip( PumpNum ).PumpControl == Continuous ) {
				Node( InletNode ).MassFlowRateRequest = PumpMassFlowRateMin;
			}

		} else if ( SELECT_CASE_var == Pump_ConSpeed ) {

			if ( PumpEquip( PumpNum ).PumpControl == Continuous ) {
				PumpMassFlowRateMin = PumpMassFlowRateMax;
				Node( InletNode ).MassFlowRateRequest = PumpMassFlowRateMin;
			}

			// Override (lock down flow) for pressure drop if applicable
			if ( PumpEquip( PumpNum ).LoopNum > 0 ) {
				if ( PlantLoop( PumpEquip( PumpNum ).LoopNum ).UsePressureForPumpCalcs && PlantLoop( PumpEquip( PumpNum ).LoopNum ).PressureSimType == Press_FlowCorrection && PlantLoop( PumpEquip( PumpNum ).LoopNum ).PressureDrop > 0.0 ) {
					PumpMassFlowRate = ResolveLoopFlowVsPressure( PumpEquip( PumpNum ).LoopNum, Node( PumpEquip( PumpNum ).InletNodeNum ).MassFlowRate, PumpEquip( PumpNum ).PressureCurve_Index, PumpEquip( PumpNum ).RotSpeed, PumpEquip( PumpNum ).ImpellerDiameter, PumpEquip( PumpNum ).MinPhiValue, PumpEquip( PumpNum ).MaxPhiValue ); //DSU? Is this still valid?
					PumpMassFlowRateMax = PumpMassFlowRate;
					PumpMassFlowRateMin = PumpMassFlowRate;
				}
			}

		}}

		// Override pump operation based on System Availability Managers, should be done elsewhere?  I suppose this should be OK though
		if ( allocated( PlantAvailMgr ) ) {
			if ( PlantAvailMgr( LoopNum ).AvailStatus == ForceOff ) {
				PumpMassFlowRateMax = 0.0;
				PumpMassFlowRateMin = 0.0;
			}
		}

		// Check if EMS is overriding flow
		if ( PumpEquip( PumpNum ).EMSMassFlowOverrideOn ) {
			PumpMassFlowRateMax = PumpEquip( PumpNum ).EMSMassFlowValue;
			PumpMassFlowRateMin = PumpEquip( PumpNum ).EMSMassFlowValue;
		}

		// Update outlet node to allow loop solver to get data
		//DSU?  could avoid this by passing data in/out to avoid putting things on nodes
		Node( OutletNode ).MassFlowRateMinAvail = PumpMassFlowRateMin;
		Node( OutletNode ).MassFlowRateMaxAvail = PumpMassFlowRateMax;

	}

	//*************************************************************************!

	//*************************************************************************!

	void
	CalcPumps(
		int const PumpNum,
		Real64 const FlowRequest,
		bool & PumpRunning
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dan Fisher
		//       DATE WRITTEN   Sept. 1998
		//       MODIFIED       July 2001, Rick Strand
		//       RE-ENGINEERED  Sept 2010, Edwin Lee

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutines simulates a pump following
		// the methodology oulined in ASHRAE's secondary toolkit.

		// METHODOLOGY EMPLOYED:
		// Calculates power and updates other pump things.

		// REFERENCES:
		// HVAC 2 Toolkit:  A Toolkit for Secondary HVAC System
		// Energy Calculations, ASHRAE, 1993, pp2-10 to 2-15

		// Using/Aliasing
		using PlantUtilities::SetComponentFlowRate;
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSpecificHeatGlycol;
		using DataPlant::PlantLoop;
		using DataBranchAirLoopPlant::MassFlowTolerance;
		using General::RoundSigDigits;
		using ScheduleManager::GetCurrentScheduleValue;
		using DataBranchAirLoopPlant::ControlType_SeriesActive;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "PlantPumps:CalcPumps: " );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int InletNode;
		int OutletNode;
		Real64 LoopDensity;
		//  INTEGER   :: DummyWaterIndex = 1
		Real64 VolFlowRate;
		Real64 PartLoadRatio;
		Real64 FracFullLoadPower;
		Real64 FullLoadVolFlowRate;
		Real64 PartLoadVolFlowRate;
		Real64 FullLoadPower;
		Real64 FullLoadPowerRatio;
		Real64 TotalEffic;
		int PumpType;
		Real64 RotSpeed_Min;
		Real64 RotSpeed_Max;
		Real64 PumpActualRPMValueOne;
		Real64 PumpActualRPMValueTwo;

		InletNode = PumpEquip( PumpNum ).InletNodeNum;
		OutletNode = PumpEquip( PumpNum ).OutletNodeNum;
		PumpType = PumpEquip( PumpNum ).PumpType;

		//****************************!
		//** SETTING PUMP FLOW RATE **!
		//****************************!
		// So the loop solver always passes in the full loop side flow request to each pump called
		// The pump will try to use this value according to its inlet conditions via the SetComponentFlowRate routine.
		// If the loop solver is doing branch pumps, then individual parallel branch inlet nodes would have been previously
		// constrained, so even though we pass in a full flow request, each pump will "pull down" to the min/max avail.
		// Also, on flowlock == locked, we will just use the inlet node flow rate
		// The flow resolver can take care of argument resolution beyond that.
		// For a typical situation, the flow request should be within the values of min/max avail, so the pump will get this flow rate.
		if ( FlowRequest > MassFlowTolerance ) {
			PumpMassFlowRate = FlowRequest;
		} else {
			PumpMassFlowRate = 0.0;
		}

		// For variable speed branch pumps, with other components
		//  on the branch, we are not going to assign a request.
		// Other components on this branch will request flow for this branch

		//  ! If this is a variable speed pump
		if ( ( PumpEquip( PumpNum ).PumpType == Pump_VarSpeed ) || ( PumpEquip( PumpNum ).PumpType == PumpBank_VarSpeed ) || ( PumpEquip( PumpNum ).PumpType == Pump_Cond ) ) {

			if ( PlantLoop( PumpEquip( PumpNum ).LoopNum ).LoopSide( PumpEquip( PumpNum ).LoopSideNum ).Branch( PumpEquip( PumpNum ).BranchNum ).Comp( PumpEquip( PumpNum ).CompNum ).FlowCtrl == ControlType_SeriesActive ) {
				PumpMassFlowRate = 0.0;
			}

		}

		// bound flow request by pump max limit, the Flow Request is total loop flow and if this is a branch pump that is not appropriate
		PumpMassFlowRate = min( PumpEquip( PumpNum ).MassFlowRateMax, PumpMassFlowRate );
		PumpMassFlowRate = max( PumpEquip( PumpNum ).MassFlowRateMin, PumpMassFlowRate );

		SetComponentFlowRate( PumpMassFlowRate, InletNode, OutletNode, PumpEquip( PumpNum ).LoopNum, PumpEquip( PumpNum ).LoopSideNum, PumpEquip( PumpNum ).BranchNum, PumpEquip( PumpNum ).CompNum );

		//Get RPM value for reporting as output
		//RPM is calculated using pump affinity laws for rotation speed
		if ( PlantLoop( PumpEquip( PumpNum ).LoopNum ).UsePressureForPumpCalcs && PumpEquip( PumpNum ).HasVFD ) {
			RotSpeed_Min = GetCurrentScheduleValue( PumpEquip( PumpNum ).VFD.MinRPMSchedIndex );
			RotSpeed_Max = GetCurrentScheduleValue( PumpEquip( PumpNum ).VFD.MaxRPMSchedIndex );
			if ( PumpEquip( PumpNum ).PumpMassFlowRateMaxRPM < MassFlowTolerance || PumpEquip( PumpNum ).PumpMassFlowRateMinRPM < MassFlowTolerance ) {
				PumpEquip( PumpNum ).VFD.PumpActualRPM = 0.0;
			} else {
				PumpActualRPMValueOne = ( PumpMassFlowRate / PumpEquip( PumpNum ).PumpMassFlowRateMaxRPM ) * RotSpeed_Max;
				PumpActualRPMValueTwo = ( PumpMassFlowRate / PumpEquip( PumpNum ).PumpMassFlowRateMinRPM ) * RotSpeed_Min;
				PumpEquip( PumpNum ).VFD.PumpActualRPM = ( PumpActualRPMValueOne + PumpActualRPMValueTwo ) / 2;
			}
		}

		//****************************!
		//** DETERMINE IF PUMP IS ON *!
		//****************************!
		// Since we don't allow series pumping, if there is ANY flow rate for this pump, THIS PUMP is driving the flow!  Therefore...
		PumpRunning = ( PumpMassFlowRate > MassFlowTolerance );

		//****************************!
		//** UPDATE PUMP BANK USAGE **!
		//****************************!
		{ auto const SELECT_CASE_var( PumpEquip( PumpNum ).PumpType );
		if ( ( SELECT_CASE_var == PumpBank_VarSpeed ) || ( SELECT_CASE_var == PumpBank_ConSpeed ) ) {
			// previously, pumps did whatever they wanted
			// because of this a constant speed pump bank could adjust the flow rate as-desired
			//  even if it was not allowed
			// since pumps now must behave nicely like all other components, the calculation of number
			//  of running pumps in a pump bank is the same for both bank types
			// the pumps are loaded sequentially, and the last pump can have full or non-full part load
			//  status...this is just how it works now.  The pump cannot *bump* up the flow on the loop
			//  to make sure the last running pump is fully loaded anymore for constant speed pumps...sorry
			if ( PumpMassFlowRate >= PumpEquip( PumpNum ).MassFlowRateMax ) {
				//running full on
				NumPumpsRunning = PumpEquip( PumpNum ).NumPumpsInBank;
			} else {
				//running at some sort of part load
				NumPumpsRunning = CEILING( ( PumpMassFlowRate / ( PumpEquip( PumpNum ).MassFlowRateMax ) * PumpEquip( PumpNum ).NumPumpsInBank ) );
				NumPumpsRunning = min( NumPumpsRunning, PumpEquip( PumpNum ).NumPumpsInBank );
			}
		}}

		//****************************!
		//***** EXIT IF NO FLOW ******!
		//****************************!
		if ( PumpMassFlowRate <= MassFlowTolerance ) {
			Node( OutletNode ).Temp = Node( InletNode ).Temp;
			Node( OutletNode ).Press = Node( InletNode ).Press;
			Node( OutletNode ).Quality = Node( InletNode ).Quality;
			return;
		}

		// density used for volumetric flow calculations
		LoopDensity = GetDensityGlycol( PlantLoop( PumpEquip( PumpNum ).LoopNum ).FluidName, Node( InletNode ).Temp, PlantLoop( PumpEquip( PumpNum ).LoopNum ).FluidIndex, RoutineName );

		//****************************!
		//***** CALCULATE POWER (1) **!
		//****************************!
		if ( PumpType == Pump_ConSpeed || PumpType == Pump_VarSpeed || PumpType == Pump_Cond ) {

			VolFlowRate = PumpMassFlowRate / LoopDensity;
			PartLoadRatio = min( 1.0, ( VolFlowRate / PumpEquip( PumpNum ).NomVolFlowRate ) );
			FracFullLoadPower = PumpEquip( PumpNum ).PartLoadCoef( 1 ) + PumpEquip( PumpNum ).PartLoadCoef( 2 ) * PartLoadRatio + PumpEquip( PumpNum ).PartLoadCoef( 3 ) * pow_2( PartLoadRatio ) + PumpEquip( PumpNum ).PartLoadCoef( 4 ) * pow_3( PartLoadRatio );
			Power = FracFullLoadPower * PumpEquip( PumpNum ).NomPowerUse;

		} else if ( PumpType == PumpBank_ConSpeed || PumpType == PumpBank_VarSpeed ) {

			// now just assume the last one is (or is not) running at part load
			// if it is actually at full load, the calculations work out to PLR = 1
			// for the last pump, so all is OK
			NumPumpsFullLoad = NumPumpsRunning - 1;
			FullLoadVolFlowRate = PumpEquip( PumpNum ).NomVolFlowRate / PumpEquip( PumpNum ).NumPumpsInBank;
			PartLoadVolFlowRate = PumpMassFlowRate / LoopDensity - FullLoadVolFlowRate * NumPumpsFullLoad;
			FullLoadPower = PumpEquip( PumpNum ).NomPowerUse / PumpEquip( PumpNum ).NumPumpsInBank;
			FullLoadPowerRatio = PumpEquip( PumpNum ).PartLoadCoef( 1 ) + PumpEquip( PumpNum ).PartLoadCoef( 2 ) + PumpEquip( PumpNum ).PartLoadCoef( 3 ) + PumpEquip( PumpNum ).PartLoadCoef( 4 );
			PartLoadRatio = min( 1.0, ( PartLoadVolFlowRate / FullLoadVolFlowRate ) );
			FracFullLoadPower = PumpEquip( PumpNum ).PartLoadCoef( 1 ) + PumpEquip( PumpNum ).PartLoadCoef( 2 ) * PartLoadRatio + PumpEquip( PumpNum ).PartLoadCoef( 3 ) * pow_2( PartLoadRatio ) + PumpEquip( PumpNum ).PartLoadCoef( 4 ) * pow_3( PartLoadRatio );
			Power = ( FullLoadPowerRatio * NumPumpsFullLoad + FracFullLoadPower ) * FullLoadPower;

		}

		//****************************!
		//***** CALCULATE POWER (2) **!
		//****************************!
		if ( Power < 0.0 ) {
			if ( PumpEquip( PumpNum ).PowerErrIndex1 == 0 ) {
				ShowWarningMessage( RoutineName + " Calculated Pump Power < 0, Type=" + cPumpTypes( PumpType ) + ", Name=\"" + PumpEquip( PumpNum ).Name + "\"." );
				ShowContinueErrorTimeStamp( "" );
				ShowContinueError( "...PartLoadRatio=[" + RoundSigDigits( PartLoadRatio, 4 ) + "], Fraction Full Load Power=" + RoundSigDigits( FracFullLoadPower, 4 ) + ']' );
				ShowContinueError( "...Power is set to 0 for continuing the simulation." );
				ShowContinueError( "...Pump coefficients should be checked for producing this negative value." );
			}
			Power = 0.0;
			ShowRecurringWarningErrorAtEnd( RoutineName + " Calculated Pump Power < 0, " + cPumpTypes( PumpType ) + ", Name=\"" + PumpEquip( PumpNum ).Name + "\", PLR=", PumpEquip( PumpNum ).PowerErrIndex1, PartLoadRatio, PartLoadRatio );
			ShowRecurringContinueErrorAtEnd( "...Fraction Full Load Power=", PumpEquip( PumpNum ).PowerErrIndex2, FracFullLoadPower, FracFullLoadPower );
		}

		//****************************!
		//***** CALCULATE POWER (3) **!
		//****************************!
		//Now if we are doing pressure-based simulation, then we have a means to calculate power exactly based on current
		// simulation conditions (flow rate and pressure drop) along with knowledge about pump impeller and motor efficiencies
		//Thus we will override the power that was calculated based on nominal values with the corrected pressure-based power
		if ( PumpEquip( PumpNum ).LoopNum > 0 ) {
			if ( PlantLoop( PumpEquip( PumpNum ).LoopNum ).UsePressureForPumpCalcs ) {
				TotalEffic = PumpEquip( PumpNum ).PumpEffic * PumpEquip( PumpNum ).MotorEffic;
				//Efficiency errors are caught previously, but it doesn't hurt to add another catch before dividing by zero!!!
				if ( TotalEffic == 0.0 ) {
					ShowSevereError( RoutineName + " Plant pressure simulation encountered a pump with zero efficiency: " + PumpEquip( PumpNum ).Name );
					ShowContinueError( "Check efficiency inputs for this pump component." );
					ShowFatalError( "Errors in plant calculation would result in divide-by-zero cause program termination." );
				}
				Power = VolFlowRate * PlantLoop( PumpEquip( PumpNum ).LoopNum ).PressureDrop / TotalEffic;
			}
		}

		// if user has specified a pressure value, then use it, same as for pressure-based simulation
		if ( PumpEquip( PumpNum ).EMSPressureOverrideOn ) {
			TotalEffic = PumpEquip( PumpNum ).PumpEffic * PumpEquip( PumpNum ).MotorEffic;
			//Efficiency errors are caught previously, but it doesn't hurt to add another catch before dividing by zero!!!
			if ( TotalEffic == 0.0 ) {
				ShowSevereError( RoutineName + " Plant pump simulation encountered a pump with zero efficiency: " + PumpEquip( PumpNum ).Name );
				ShowContinueError( "Check efficiency inputs for this pump component." );
				ShowFatalError( "Errors in plant calculation would result in divide-by-zero cause program termination." );
			}
			Power = VolFlowRate * PumpEquip( PumpNum ).EMSPressureOverrideValue / TotalEffic;
		}

		//****************************!
		//***** CALCULATE POWER (4) **!
		//****************************!
		// This adds the pump heat based on User input for the pump
		// We assume that all of the heat ends up in the fluid eventually since this is a closed loop
		ShaftPower = Power * PumpEquip( PumpNum ).MotorEffic;
		PumpHeattoFluid = ShaftPower + ( Power - ShaftPower ) * PumpEquip( PumpNum ).FracMotorLossToFluid;

		//****************************!
		//***** UPDATE INFORMATION ***!
		//****************************!
		// Update data structure variables
		PumpEquip( PumpNum ).Power = Power;

		// Update outlet node conditions
		Node( OutletNode ).Temp = Node( InletNode ).Temp;
		Node( OutletNode ).Press = Node( InletNode ).Press;
		Node( OutletNode ).Quality = Node( InletNode ).Quality;

	}

	//*************************************************************************!

	//*************************************************************************!

	void
	SizePump( int const PumpNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   December 2001
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing Pump Components for which flow rates have not been
		// specified in the input.

		// METHODOLOGY EMPLOYED:
		// Obtains flow rates from the plant sizing array.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataSizing::AutoSize;
		using DataSizing::PlantSizData;
		using DataPlant::PlantLoop;
		using DataPlant::PlantFirstSizesOkayToReport;
		using DataPlant::PlantFinalSizesOkayToReport;
		using General::RoundSigDigits;
		using ReportSizingManager::ReportSizingOutput;
		using FluidProperties::GetSatDensityRefrig;
		using FluidProperties::GetDensityGlycol;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const StartTemp( 100.0 ); // Standard Temperature across code to calculated Steam density
		static std::string const RoutineName( "PlantPumps::InitSimVars " );
		static std::string const RoutineNameSizePumps( "SizePumps" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int PlantSizNum; // index of Plant Sizing array
		bool ErrorsFound;
		Real64 TotalEffic; // pump total efficiency
		int Side; // half loop index
		int BranchNum; // index of branch
		int CompNum; // index of component on branch
		Real64 PumpSizFac; // pump sizing factor
		Real64 SteamDensity;
		Real64 TempWaterDensity;
		static int DummyWaterIndex( 1 );
		Real64 DesVolFlowRatePerBranch; // local temporary for split of branch pumps

		// Calculate density at InitConvTemp once here, to remove RhoH2O calls littered throughout
		if ( PumpEquip( PumpNum ).LoopNum > 0 ) {
			TempWaterDensity = GetDensityGlycol( PlantLoop( PumpEquip( PumpNum ).LoopNum ).FluidName, InitConvTemp, PlantLoop( PumpEquip( PumpNum ).LoopNum ).FluidIndex, RoutineName );
		} else {
			TempWaterDensity = GetDensityGlycol( fluidNameWater, InitConvTemp, DummyWaterIndex, RoutineName );
		}

		// note: we assume pump impeller efficiency is 78% for autosizing
		//TotalEffic = 0.78 * PumpEquip( PumpNum ).MotorEffic;



		PlantSizNum = 0;
		PumpSizFac = 1.0;
		ErrorsFound = false;

		if ( PumpEquip( PumpNum ).LoopNum > 0 ) {
			PlantSizNum = PlantLoop( PumpEquip( PumpNum ).LoopNum ).PlantSizNum;
		}
		// use pump sizing factor stored in plant sizing data structure
		if ( PlantSizNum > 0 ) {
			PumpSizFac = PlantSizData( PlantSizNum ).PlantSizFac;
		} else {
			// might be able to remove this next block
			if ( PumpEquip( PumpNum ).LoopNum > 0 ) {
				for ( Side = 1; Side <= 2; ++Side ) {
					for ( BranchNum = 1; BranchNum <= PlantLoop( PumpEquip( PumpNum ).LoopNum ).LoopSide( Side ).TotalBranches; ++BranchNum ) {
						for ( CompNum = 1; CompNum <= PlantLoop( PumpEquip( PumpNum ).LoopNum ).LoopSide( Side ).Branch( BranchNum ).TotalComponents; ++CompNum ) {
							if ( PumpEquip( PumpNum ).InletNodeNum == PlantLoop( PumpEquip( PumpNum ).LoopNum ).LoopSide( Side ).Branch( BranchNum ).Comp( CompNum ).NodeNumIn && PumpEquip( PumpNum ).OutletNodeNum == PlantLoop( PumpEquip( PumpNum ).LoopNum ).LoopSide( Side ).Branch( BranchNum ).Comp( CompNum ).NodeNumOut ) {
								if ( PlantLoop( PumpEquip( PumpNum ).LoopNum ).LoopSide( Side ).Branch( BranchNum ).PumpSizFac > 0.0 ) {
									PumpSizFac = PlantLoop( PumpEquip( PumpNum ).LoopNum ).LoopSide( Side ).Branch( BranchNum ).PumpSizFac;
								} else {
									PumpSizFac = 1.0;
								}
								goto SideLoop_exit;
							}
						}
					}
				}
				SideLoop_exit: ;
			}
		}

		if ( PumpEquip( PumpNum ).NomVolFlowRateWasAutoSized ) {

			if ( PlantSizNum > 0 ) {
				if ( PlantSizData( PlantSizNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
					if ( ! PlantLoop( PumpEquip( PumpNum ).LoopNum ).LoopSide( PumpEquip( PumpNum ).LoopSideNum ).BranchPumpsExist ) {
						// size pump to full flow of plant loop
						if ( PumpEquip( PumpNum ).PumpType == Pump_Cond ) {
							TempWaterDensity = GetDensityGlycol( fluidNameWater, InitConvTemp, DummyWaterIndex, RoutineName );
							SteamDensity = GetSatDensityRefrig( fluidNameSteam, StartTemp, 1.0, PumpEquip( PumpNum ).FluidIndex, RoutineNameSizePumps );
							PumpEquip( PumpNum ).NomSteamVolFlowRate = PlantSizData( PlantSizNum ).DesVolFlowRate * PumpSizFac;
							PumpEquip( PumpNum ).NomVolFlowRate = PumpEquip( PumpNum ).NomSteamVolFlowRate * SteamDensity / TempWaterDensity;
						} else {
							PumpEquip( PumpNum ).NomVolFlowRate = PlantSizData( PlantSizNum ).DesVolFlowRate * PumpSizFac;
						}
					} else {
						// Distribute sizes evenly across all branch pumps
						DesVolFlowRatePerBranch = PlantSizData( PlantSizNum ).DesVolFlowRate / PlantLoop( PumpEquip( PumpNum ).LoopNum ).LoopSide( PumpEquip( PumpNum ).LoopSideNum ).TotalPumps;
						if ( PumpEquip( PumpNum ).PumpType == Pump_Cond ) {
							TempWaterDensity = GetDensityGlycol( fluidNameWater, InitConvTemp, DummyWaterIndex, RoutineName );
							SteamDensity = GetSatDensityRefrig( fluidNameSteam, StartTemp, 1.0, PumpEquip( PumpNum ).FluidIndex, RoutineNameSizePumps );
							PumpEquip( PumpNum ).NomSteamVolFlowRate = DesVolFlowRatePerBranch * PumpSizFac;
							PumpEquip( PumpNum ).NomVolFlowRate = PumpEquip( PumpNum ).NomSteamVolFlowRate * SteamDensity / TempWaterDensity;
						} else {
							PumpEquip( PumpNum ).NomVolFlowRate = DesVolFlowRatePerBranch * PumpSizFac;
						}
					}

				} else {
					if (PlantFinalSizesOkayToReport) {
						PumpEquip( PumpNum ).NomVolFlowRate = 0.0;
						ShowWarningError( "SizePump: Calculated Pump Nominal Volume Flow Rate=[" + RoundSigDigits( PlantSizData( PlantSizNum ).DesVolFlowRate, 2 ) + "] is too small. Set to 0.0" );
						ShowContinueError( "..occurs for Pump=" + PumpEquip( PumpNum ).Name );
					}
				}
				if (PlantFinalSizesOkayToReport) {
					ReportSizingOutput( cPumpTypes( PumpEquip( PumpNum ).PumpType ), PumpEquip( PumpNum ).Name,
						"Design Flow Rate [m3/s]", PumpEquip( PumpNum ).NomVolFlowRate );
				}
				if (PlantFirstSizesOkayToReport) {
					ReportSizingOutput( cPumpTypes( PumpEquip( PumpNum ).PumpType ), PumpEquip( PumpNum ).Name,
						"Initial Design Flow Rate [m3/s]", PumpEquip( PumpNum ).NomVolFlowRate );
				}
			} else {
				if (PlantFinalSizesOkayToReport) {
					ShowSevereError( "Autosizing of plant loop pump flow rate requires a loop Sizing:Plant object" );
					ShowContinueError( "Occurs in plant pump object=" + PumpEquip( PumpNum ).Name );
					ErrorsFound = true;
				}
			}

		}

		// Note that autocalculation of power is based on nominal volume flow, regardless of whether the flow was
		//  auto sized or manually sized.  Thus, this must go after the flow sizing block above.
		if ( PumpEquip( PumpNum ).NomPowerUseWasAutoSized ) {
			if ( PumpEquip( PumpNum ).NomVolFlowRate >= SmallWaterVolFlow ) {
				switch ( PumpEquip( PumpNum ).powerSizingMethod )
				{

				case sizePowerPerFlow: {
					TotalEffic = PumpEquip( PumpNum ).NomPumpHead / PumpEquip( PumpNum ).powerPerFlowScalingFactor;
					break;
					}

				case sizePowerPerFlowPerPressure: {
					TotalEffic = ( 1/PumpEquip( PumpNum ).powerPerFlowPerPressureScalingFactor ) * PumpEquip( PumpNum ).MotorEffic;
					break;
					}
				}

				PumpEquip( PumpNum ).NomPowerUse = ( PumpEquip( PumpNum ).NomPumpHead * PumpEquip( PumpNum ).NomVolFlowRate ) / TotalEffic;
			} else {
				PumpEquip( PumpNum ).NomPowerUse = 0.0;
			}
			if ( PlantFinalSizesOkayToReport ) {
				ReportSizingOutput( cPumpTypes( PumpEquip( PumpNum ).PumpType ), PumpEquip( PumpNum ).Name,
					"Design Power Consumption [W]", PumpEquip( PumpNum ).NomPowerUse );
			}
			if ( PlantFirstSizesOkayToReport ) {
				ReportSizingOutput( cPumpTypes( PumpEquip( PumpNum ).PumpType ), PumpEquip( PumpNum ).Name,
					"Initial Design Power Consumption [W]", PumpEquip( PumpNum ).NomPowerUse );
			}
		}

		if ( PumpEquip( PumpNum ).minVolFlowRateWasAutosized ) {
			PumpEquip( PumpNum ).MinVolFlowRate = PumpEquip( PumpNum ).NomVolFlowRate * PumpEquip( PumpNum ).MinVolFlowRateFrac;
			if (PlantFinalSizesOkayToReport) {
					ReportSizingOutput( cPumpTypes( PumpEquip( PumpNum ).PumpType ), PumpEquip( PumpNum ).Name,
						"Design Minimum Flow Rate [m3/s]", PumpEquip( PumpNum ).MinVolFlowRate );
				}
				if (PlantFirstSizesOkayToReport) {
					ReportSizingOutput( cPumpTypes( PumpEquip( PumpNum ).PumpType ), PumpEquip( PumpNum ).Name,
						"Initial Design Minimum Flow Rate [m3/s]", PumpEquip( PumpNum ).MinVolFlowRate );
				}
		}

		if (PlantFinalSizesOkayToReport) {
			PumpDataForTable( PumpNum );
		}

		if ( ErrorsFound ) {
			ShowFatalError( "Preceding sizing errors cause program termination" );
		}

	}

	//*************************************************************************!

	//*************************************************************************!

	void
	ReportPumps( int const PumpNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Dan Fisher
		//       DATE WRITTEN:    October 1998
		//       MODIFIED         July 2001, Rick Strand (revision of pump module)
		//       RE-ENGINEERED    na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine sets the pump reporting variables.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataBranchAirLoopPlant::MassFlowTolerance;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int InletNode; // pump inlet node number
		int OutletNode; // pump outlet node number
		int PumpType; // Current pump type

		// FLOW:
		PumpType = PumpEquip( PumpNum ).PumpType;
		InletNode = PumpEquip( PumpNum ).InletNodeNum;
		OutletNode = PumpEquip( PumpNum ).OutletNodeNum;

		if ( PumpMassFlowRate <= MassFlowTolerance ) {
			PumpEquipReport( PumpNum ).PumpMassFlowRate = 0.0;
			PumpEquipReport( PumpNum ).PumpHeattoFluid = 0.0;
			PumpEquipReport( PumpNum ).OutletTemp = Node( OutletNode ).Temp;
			PumpEquip( PumpNum ).Power = 0.0;
			PumpEquip( PumpNum ).Energy = 0.0;
			PumpEquipReport( PumpNum ).ShaftPower = 0.0;
			PumpEquipReport( PumpNum ).PumpHeattoFluidEnergy = 0.0;
			PumpEquipReport( PumpNum ).ZoneTotalGainRate = 0.0;
			PumpEquipReport( PumpNum ).ZoneTotalGainEnergy = 0.0;
			PumpEquipReport( PumpNum ).ZoneConvGainRate = 0.0;
			PumpEquipReport( PumpNum ).ZoneRadGainRate = 0.0;
			PumpEquipReport( PumpNum ).NumPumpsOperating = 0;
		} else {
			PumpEquipReport( PumpNum ).PumpMassFlowRate = PumpMassFlowRate;
			PumpEquipReport( PumpNum ).PumpHeattoFluid = PumpHeattoFluid;
			PumpEquipReport( PumpNum ).OutletTemp = Node( OutletNode ).Temp;
			PumpEquip( PumpNum ).Power = Power;
			PumpEquip( PumpNum ).Energy = PumpEquip( PumpNum ).Power * TimeStepSys * SecInHour;
			PumpEquipReport( PumpNum ).ShaftPower = ShaftPower;
			PumpEquipReport( PumpNum ).PumpHeattoFluidEnergy = PumpHeattoFluid * TimeStepSys * SecInHour;
			if ( PumpType == Pump_ConSpeed || PumpType == Pump_VarSpeed || PumpType == Pump_Cond ) {
				PumpEquipReport( PumpNum ).NumPumpsOperating = 1;
			} else if ( PumpType == PumpBank_ConSpeed || PumpType == PumpBank_VarSpeed ) {
				PumpEquipReport( PumpNum ).NumPumpsOperating = NumPumpsRunning;
			}
			PumpEquipReport( PumpNum ).ZoneTotalGainRate = Power - PumpHeattoFluid;
			PumpEquipReport( PumpNum ).ZoneTotalGainEnergy = PumpEquipReport( PumpNum ).ZoneTotalGainRate * TimeStepSys * SecInHour;
			PumpEquipReport( PumpNum ).ZoneConvGainRate = ( 1 - PumpEquip( PumpNum ).SkinLossRadFraction ) * PumpEquipReport( PumpNum ).ZoneTotalGainRate;
			PumpEquipReport( PumpNum ).ZoneRadGainRate = PumpEquip( PumpNum ).SkinLossRadFraction * PumpEquipReport( PumpNum ).ZoneTotalGainRate;
		}

	}

	//*************************************************************************!

	//*************************************************************************!

	void
	PumpDataForTable( int const NumPump )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Jason Glazer
		//       DATE WRITTEN:    September 2006
		//       MODIFIED         na
		//       RE-ENGINEERED    na

		// PURPOSE OF THIS SUBROUTINE:
		// Pull data together for predefined tables.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Using/Aliasing
		using namespace OutputReportPredefined;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		std::string equipName;

		equipName = PumpEquip( NumPump ).Name;
		PreDefTableEntry( pdchPumpType, equipName, cPumpTypes( PumpEquip( NumPump ).PumpType ) );
		if ( PumpEquip( NumPump ).PumpControl == Continuous ) {
			PreDefTableEntry( pdchPumpControl, equipName, "Continuous" );
		} else if ( PumpEquip( NumPump ).PumpControl == Intermittent ) {
			PreDefTableEntry( pdchPumpControl, equipName, "Intermittent" );
		} else {
			PreDefTableEntry( pdchPumpControl, equipName, "Unknown" );
		}
		PreDefTableEntry( pdchPumpHead, equipName, PumpEquip( NumPump ).NomPumpHead );
		PreDefTableEntry( pdchPumpFlow, equipName, PumpEquip( NumPump ).NomVolFlowRate, 6 );
		PreDefTableEntry( pdchPumpPower, equipName, PumpEquip( NumPump ).NomPowerUse );
		if ( PumpEquip( NumPump ).NomVolFlowRate != 0 ) {
			PreDefTableEntry( pdchPumpPwrPerFlow, equipName, PumpEquip( NumPump ).NomPowerUse / PumpEquip( NumPump ).NomVolFlowRate );
		} else {
			PreDefTableEntry( pdchPumpPwrPerFlow, equipName, "-" );
		}
		PreDefTableEntry( pdchMotEff, equipName, PumpEquip( NumPump ).MotorEffic );

	}

	//*************************************************************************!

	void
	GetRequiredMassFlowRate(
		int const LoopNum,
		int const PumpNum,
		Real64 const InletNodeMassFlowRate,
		Real64 & ActualFlowRate,
		Real64 & PumpMinMassFlowRateVFDRange,
		Real64 & PumpMaxMassFlowRateVFDRange
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR
		//       DATE WRITTEN
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS SUBROUTINE:

		// METHODOLOGY EMPLOYED:
		// General EnergyPlus Methodology

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// Using/Aliasing
		using PlantUtilities::SetComponentFlowRate;
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSpecificHeatGlycol;
		using DataPlant::Press_FlowCorrection;
		using DataPlant::PlantLoop;
		using General::RoundSigDigits;
		using ScheduleManager::GetCurrentScheduleValue;
		using PlantPressureSystem::ResolveLoopFlowVsPressure;
		using DataLoopNode::Node;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		static Real64 PumpMassFlowRateMaxPress( 0.0 ); // Maximum mass flow rate associated with maximum pressure limit
		static Real64 PumpMassFlowRateMinPress( 0.0 ); // Minimum mass flow rate associated with minimum pressure limit
		static Real64 RotSpeed_Max( 0.0 ); // Maximum rotaional speed in rps
		static Real64 RotSpeed_Min( 0.0 ); // Minimum rotaional speed in rps
		static Real64 MinPress( 0.0 ); // Minimum pressure
		static Real64 MaxPress( 0.0 ); // Maximum pressure

		RotSpeed_Min = GetCurrentScheduleValue( PumpEquip( PumpNum ).VFD.MinRPMSchedIndex );
		RotSpeed_Max = GetCurrentScheduleValue( PumpEquip( PumpNum ).VFD.MaxRPMSchedIndex );
		MinPress = GetCurrentScheduleValue( PumpEquip( PumpNum ).VFD.LowerPsetSchedIndex );
		MaxPress = GetCurrentScheduleValue( PumpEquip( PumpNum ).VFD.UpperPsetSchedIndex );

		//Calculate maximum and minimum mass flow rate associated with maximun and minimum RPM
		if ( PumpEquip( PumpNum ).LoopNum > 0 ) {
			if ( PlantLoop( PumpEquip( PumpNum ).LoopNum ).UsePressureForPumpCalcs && PlantLoop( PumpEquip( PumpNum ).LoopNum ).PressureSimType == Press_FlowCorrection && PlantLoop( PumpEquip( PumpNum ).LoopNum ).PressureDrop > 0.0 ) {
				PumpEquip( PumpNum ).PumpMassFlowRateMaxRPM = ResolveLoopFlowVsPressure( PumpEquip( PumpNum ).LoopNum, InletNodeMassFlowRate, PumpEquip( PumpNum ).PressureCurve_Index, RotSpeed_Max, PumpEquip( PumpNum ).ImpellerDiameter, PumpEquip( PumpNum ).MinPhiValue, PumpEquip( PumpNum ).MaxPhiValue ); //DSU? Is this still valid?
				PumpEquip( PumpNum ).PumpMassFlowRateMinRPM = ResolveLoopFlowVsPressure( PumpEquip( PumpNum ).LoopNum, InletNodeMassFlowRate, PumpEquip( PumpNum ).PressureCurve_Index, RotSpeed_Min, PumpEquip( PumpNum ).ImpellerDiameter, PumpEquip( PumpNum ).MinPhiValue, PumpEquip( PumpNum ).MaxPhiValue ); //DSU? Is this still valid?
			}
		}

		//Not correct necessarily, but values are coming out way wrong here, maxRPMmdot~3, minRPMmdot~62!
		if ( PumpEquip( PumpNum ).PumpMassFlowRateMaxRPM < PumpEquip( PumpNum ).PumpMassFlowRateMinRPM ) {
			PumpEquip( PumpNum ).PumpMassFlowRateMaxRPM = PumpEquip( PumpNum ).PumpMassFlowRateMinRPM;
		}

		//Calculate maximum and minimum mass flow rate associated with operating pressure range
		if ( PumpEquip( PumpNum ).LoopNum > 0 ) {
			if ( PlantLoop( LoopNum ).PressureEffectiveK > 0.0 ) {
				PumpMassFlowRateMaxPress = std::sqrt( MaxPress / PlantLoop( LoopNum ).PressureEffectiveK );
				PumpMassFlowRateMinPress = std::sqrt( MinPress / PlantLoop( LoopNum ).PressureEffectiveK );
			}
		}

		//Decide operating range for mass flow rate
		//Maximum mass flow rate value of the range
		if ( PumpEquip( PumpNum ).PumpMassFlowRateMaxRPM > PumpMassFlowRateMaxPress ) {
			//Maximum pressure value governs maximum VFD range value
			PumpMaxMassFlowRateVFDRange = PumpMassFlowRateMaxPress;
		} else {
			//Maximum RPM value governs maximum VFD range value
			PumpMaxMassFlowRateVFDRange = PumpEquip( PumpNum ).PumpMassFlowRateMaxRPM;
		}

		//Minimum mass flow rate value of the range
		if ( PumpEquip( PumpNum ).PumpMassFlowRateMinRPM > PumpMassFlowRateMinPress ) {
			//Minimum pressure value governs minimum VFD range value
			PumpMinMassFlowRateVFDRange = PumpEquip( PumpNum ).PumpMassFlowRateMinRPM;
		} else {
			//Minimum pressure range value governs minimum VFD range value
			PumpMinMassFlowRateVFDRange = PumpMassFlowRateMinPress;
		}

		//Set the mass flow rate within VFD operating range
		if ( InletNodeMassFlowRate > PumpMinMassFlowRateVFDRange ) {
			if ( InletNodeMassFlowRate < PumpMaxMassFlowRateVFDRange ) {
				//Flow request is within VFD operating range
				ActualFlowRate = InletNodeMassFlowRate;
			} else {
				//Flow request is outside VFD operating range
				//Flow is set to maximum VFD operating range
				ActualFlowRate = PumpMaxMassFlowRateVFDRange;
			}
		} else {
			//Flow request is outside VFD operating range
			//Flow is set to minimum VFD operating Range
			ActualFlowRate = PumpMinMassFlowRateVFDRange;
		}

	}

	//=================================================================================================!

} // Pumps

} // EnergyPlus
