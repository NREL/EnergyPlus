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
#include <WaterUse.hh>
#include <BranchNodeConnections.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <DataWater.hh>
#include <General.hh>
#include <HeatBalanceInternalHeatGains.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <PlantUtilities.hh>
#include <Psychrometrics.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>
#include <WaterManager.hh>

namespace EnergyPlus {

namespace WaterUse {

	// MODULE INFORMATION:
	//       AUTHOR         Peter Graham Ellis
	//       DATE WRITTEN   August 2006
	//       MODIFIED       Brent Griffith, plant upgrade
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:

	// METHODOLOGY EMPLOYED:

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::WarmupFlag;
	using DataGlobals::BeginEnvrnFlag;
	using DataGlobals::InitConvTemp;
	using DataGlobals::NumOfZones;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	int const HeatRecoveryHXIdeal( 1 );
	int const HeatRecoveryHXCounterFlow( 2 );
	int const HeatRecoveryHXCrossFlow( 3 );

	int const HeatRecoveryConfigPlant( 1 );
	int const HeatRecoveryConfigEquipment( 2 );
	int const HeatRecoveryConfigPlantAndEquip( 3 );

	static std::string const BlankString;

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE TYPE DECLARATIONS:

	// MODULE VARIABLE DECLARATIONS:
	int NumWaterEquipment( 0 );
	int NumWaterConnections( 0 );
	//INTEGER :: MaxIterationsErrorCount =0
	bool GetWaterUseInputFlag( true );

	Array1D_bool CheckEquipName;
	Array1D_bool CheckPlantLoop;

	// SUBROUTINE SPECIFICATIONS:

	// Object Data
	Array1D< WaterEquipmentType > WaterEquipment;
	Array1D< WaterConnectionsType > WaterConnections;

	// MODULE SUBROUTINES:

	// Functions

	void
	clear_state()
	{
		NumWaterEquipment = 0;
		NumWaterConnections = 0;
		GetWaterUseInputFlag = true;
		CheckEquipName.deallocate();
		CheckPlantLoop.deallocate();
		WaterEquipment.deallocate();
		WaterConnections.deallocate();
	}

	void
	SimulateWaterUse( bool const FirstHVACIteration )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   August 2006
		//       MODIFIED       Brent Griffith, March 2010, seperated plant connected to different sim routine
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine is called from non zone equipment manager and serves to call
		// water use and connections that are not connected to a full plant loop

		// METHODOLOGY EMPLOYED:

		// Using/Aliasing
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		int const MaxIterations( 100 );
		Real64 const Tolerance( 0.1 ); // Make input?

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int WaterEquipNum;
		int WaterConnNum;
		int NumIteration;
		static int MaxIterationsErrorCount;
		static bool MyEnvrnFlag( true );

		// FLOW:
		if ( GetWaterUseInputFlag ) {
			GetWaterUseInput();
			GetWaterUseInputFlag = false;
		}

		if ( BeginEnvrnFlag && MyEnvrnFlag ) {
			MaxIterationsErrorCount = 0;
			if ( NumWaterEquipment > 0 ) {
				for ( auto & e : WaterEquipment ) {
					e.SensibleRate = 0.0;
					e.SensibleEnergy = 0.0;
					e.LatentRate = 0.0;
					e.LatentEnergy = 0.0;
					e.MixedTemp = 0.0;
					e.TotalMassFlowRate = 0.0;
					e.DrainTemp = 0.0;
				}
			}

			if ( NumWaterConnections > 0 ) {
				for ( auto & e : WaterConnections ) e.TotalMassFlowRate = 0.0;
			}

			MyEnvrnFlag = false;
		}

		if ( ! BeginEnvrnFlag ) MyEnvrnFlag = true;

		// Simulate all unconnected WATER USE EQUIPMENT objects
		for ( WaterEquipNum = 1; WaterEquipNum <= NumWaterEquipment; ++WaterEquipNum ) {
			if ( WaterEquipment( WaterEquipNum ).Connections == 0 ) {
				CalcEquipmentFlowRates( WaterEquipNum );
				CalcEquipmentDrainTemp( WaterEquipNum );
			}
		} // WaterEquipNum

		ReportStandAloneWaterUse();

		// Simulate WATER USE CONNECTIONS objects and connected WATER USE EQUIPMENT objects
		for ( WaterConnNum = 1; WaterConnNum <= NumWaterConnections; ++WaterConnNum ) {

			if ( ! WaterConnections( WaterConnNum ).StandAlone ) continue; // only model non plant connections here

			InitConnections( WaterConnNum );

			NumIteration = 0;

			while ( true ) {
				++NumIteration;

				CalcConnectionsFlowRates( WaterConnNum, FirstHVACIteration );
				CalcConnectionsDrainTemp( WaterConnNum );
				CalcConnectionsHeatRecovery( WaterConnNum );

				if ( WaterConnections( WaterConnNum ).TempError < Tolerance ) {
					break;
				} else if ( NumIteration > MaxIterations ) {
					if ( ! WarmupFlag ) {
						if ( WaterConnections( WaterConnNum ).MaxIterationsErrorIndex == 0 ) {
							ShowWarningError( "WaterUse:Connections = " + WaterConnections( WaterConnNum ).Name + ":  Heat recovery temperature did not converge" );
							ShowContinueErrorTimeStamp( "" );
						}
						ShowRecurringWarningErrorAtEnd( "WaterUse:Connections = " + WaterConnections( WaterConnNum ).Name + ":  Heat recovery temperature did not converge", WaterConnections( WaterConnNum ).MaxIterationsErrorIndex );
					}
					break;
				}

			} // WHILE

			UpdateWaterConnections( WaterConnNum );

			ReportWaterUse( WaterConnNum );

		} // WaterConnNum

	}

	void
	SimulateWaterUseConnection(
		int const EP_UNUSED( EquipTypeNum ),
		std::string const & CompName,
		int & CompIndex,
		bool const InitLoopEquip,
		bool const FirstHVACIteration
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith March 2010, Demand Side Update
		//       DATE WRITTEN   August 2006
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Plant sim call for plant loop connected water use and connections
		// (based on SimulateWaterUse by P. Ellis)

		// METHODOLOGY EMPLOYED:

		// Using/Aliasing
		using General::RoundSigDigits;
		using General::TrimSigDigits;
		using InputProcessor::FindItemInList;

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		int const MaxIterations( 100 );
		Real64 const Tolerance( 0.1 ); // Make input?

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//  INTEGER :: WaterEquipNum
		int WaterConnNum;
		int NumIteration;
		static int MaxIterationsErrorCount;
		static bool MyEnvrnFlag( true );

		// FLOW:
		if ( GetWaterUseInputFlag ) {
			GetWaterUseInput();
			GetWaterUseInputFlag = false;
		}

		if ( CompIndex == 0 ) {
			WaterConnNum = FindItemInList( CompName, WaterConnections );
			if ( WaterConnNum == 0 ) {
				ShowFatalError( "SimulateWaterUseConnection: Unit not found=" + CompName );
			}
			CompIndex = WaterConnNum;
		} else {
			WaterConnNum = CompIndex;
			if ( WaterConnNum > NumWaterConnections || WaterConnNum < 1 ) {
				ShowFatalError( "SimulateWaterUseConnection: Invalid CompIndex passed=" + TrimSigDigits( WaterConnNum ) + ", Number of Units=" + TrimSigDigits( NumWaterConnections ) + ", Entered Unit name=" + CompName );
			}
			if ( CheckEquipName( WaterConnNum ) ) {
				if ( CompName != WaterConnections( WaterConnNum ).Name ) {
					ShowFatalError( "SimulateWaterUseConnection: Invalid CompIndex passed=" + TrimSigDigits( WaterConnNum ) + ", Unit name=" + CompName + ", stored Unit Name for that index=" + WaterConnections( WaterConnNum ).Name );
				}
				CheckEquipName( WaterConnNum ) = false;
			}
		}

		if ( InitLoopEquip ) {
			return;
		}

		if ( BeginEnvrnFlag && MyEnvrnFlag ) {
			MaxIterationsErrorCount = 0;
			if ( NumWaterEquipment > 0 ) {
				for ( int i = WaterEquipment.l(), e = WaterEquipment.u(); i <= e; ++i ) {
					WaterEquipment( i ).reset();
				}
			}

			if ( NumWaterConnections > 0 ) {
				for ( auto & e : WaterConnections ) e.TotalMassFlowRate = 0.0;
			}

			MyEnvrnFlag = false;
		}

		if ( ! BeginEnvrnFlag ) MyEnvrnFlag = true;

		InitConnections( WaterConnNum );

		NumIteration = 0;

		while ( true ) {
			++NumIteration;

			CalcConnectionsFlowRates( WaterConnNum, FirstHVACIteration );
			CalcConnectionsDrainTemp( WaterConnNum );
			CalcConnectionsHeatRecovery( WaterConnNum );

			if ( WaterConnections( WaterConnNum ).TempError < Tolerance ) {
				break;
			} else if ( NumIteration > MaxIterations ) {
				if ( ! WarmupFlag ) {
					if ( WaterConnections( WaterConnNum ).MaxIterationsErrorIndex == 0 ) {
						ShowWarningError( "WaterUse:Connections = " + WaterConnections( WaterConnNum ).Name + ":  Heat recovery temperature did not converge" );
						ShowContinueErrorTimeStamp( "" );
					}
					ShowRecurringWarningErrorAtEnd( "WaterUse:Connections = " + WaterConnections( WaterConnNum ).Name + ":  Heat recovery temperature did not converge", WaterConnections( WaterConnNum ).MaxIterationsErrorIndex );
				}
				break;
			}

		} // WHILE

		UpdateWaterConnections( WaterConnNum );

		ReportWaterUse( WaterConnNum );

	}

	void
	GetWaterUseInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   August 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// Using/Aliasing
		using InputProcessor::GetObjectDefMaxArgs;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::FindItemInList;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using namespace DataIPShortCuts; // Data for field names, blank numerics
		using ScheduleManager::GetScheduleIndex;
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::TestCompSet;
		using namespace DataLoopNode;
		using namespace DataHeatBalance;
		using WaterManager::SetupTankSupplyComponent;
		using WaterManager::SetupTankDemandComponent;
		using Psychrometrics::RhoH2O;
		using PlantUtilities::RegisterPlantCompDesignFlow;

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool ErrorsFound( false ); // Set to true if errors in input, fatal at end of routine
		int IOStatus; // Used in GetObjectItem
		bool IsBlank; // TRUE if the name is blank
		bool IsNotOK; // TRUE if there was a problem with a list name
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		//unused1208  INTEGER                        :: NumArgs
		int WaterEquipNum;
		int WaterConnNum;
		int AlphaNum;
		int thisWaterEquipNum;

		// FLOW:

		cCurrentModuleObject = "WaterUse:Equipment";
		NumWaterEquipment = GetNumObjectsFound( cCurrentModuleObject );

		if ( NumWaterEquipment > 0 ) {
			WaterEquipment.allocate( NumWaterEquipment );

			for ( WaterEquipNum = 1; WaterEquipNum <= NumWaterEquipment; ++WaterEquipNum ) {
				GetObjectItem( cCurrentModuleObject, WaterEquipNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

				IsNotOK = false;
				IsBlank = false;
				VerifyName( cAlphaArgs( 1 ), WaterEquipment, WaterEquipNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
				}
				WaterEquipment( WaterEquipNum ).Name = cAlphaArgs( 1 );

				WaterEquipment( WaterEquipNum ).EndUseSubcatName = cAlphaArgs( 2 );

				WaterEquipment( WaterEquipNum ).PeakVolFlowRate = rNumericArgs( 1 );

				if ( ( NumAlphas > 2 ) && ( ! lAlphaFieldBlanks( 3 ) ) ) {
					WaterEquipment( WaterEquipNum ).FlowRateFracSchedule = GetScheduleIndex( cAlphaArgs( 3 ) );
					// If no FlowRateFracSchedule, fraction defaults to 1.0

					if ( WaterEquipment( WaterEquipNum ).FlowRateFracSchedule == 0 ) {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 3 ) + '=' + cAlphaArgs( 3 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					}
				}

				if ( ( NumAlphas > 3 ) && ( ! lAlphaFieldBlanks( 4 ) ) ) {
					WaterEquipment( WaterEquipNum ).TargetTempSchedule = GetScheduleIndex( cAlphaArgs( 4 ) );

					if ( WaterEquipment( WaterEquipNum ).TargetTempSchedule == 0 ) {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 4 ) + '=' + cAlphaArgs( 4 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					}
				}

				if ( ( NumAlphas > 4 ) && ( ! lAlphaFieldBlanks( 5 ) ) ) {
					WaterEquipment( WaterEquipNum ).HotTempSchedule = GetScheduleIndex( cAlphaArgs( 5 ) );
					// If no HotTempSchedule, there is no hot water.
					// HotTempSchedule is ignored if connected to a plant loop via WATER USE CONNECTIONS

					if ( WaterEquipment( WaterEquipNum ).HotTempSchedule == 0 ) {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 5 ) + '=' + cAlphaArgs( 5 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					}
				}

				if ( ( NumAlphas > 5 ) && ( ! lAlphaFieldBlanks( 6 ) ) ) {
					WaterEquipment( WaterEquipNum ).ColdTempSchedule = GetScheduleIndex( cAlphaArgs( 6 ) );
					// If no ColdTempSchedule, temperatures will be calculated by WATER MAINS TEMPERATURES object

					if ( WaterEquipment( WaterEquipNum ).ColdTempSchedule == 0 ) {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 6 ) + '=' + cAlphaArgs( 6 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					}
				}

				if ( ( NumAlphas > 6 ) && ( ! lAlphaFieldBlanks( 7 ) ) ) {
					WaterEquipment( WaterEquipNum ).Zone = FindItemInList( cAlphaArgs( 7 ), Zone );

					if ( WaterEquipment( WaterEquipNum ).Zone == 0 ) {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 7 ) + '=' + cAlphaArgs( 7 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					}
				}

				if ( ( NumAlphas > 7 ) && ( ! lAlphaFieldBlanks( 8 ) ) ) {
					WaterEquipment( WaterEquipNum ).SensibleFracSchedule = GetScheduleIndex( cAlphaArgs( 8 ) );

					if ( WaterEquipment( WaterEquipNum ).SensibleFracSchedule == 0 ) {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 8 ) + '=' + cAlphaArgs( 8 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					}
				}

				if ( ( NumAlphas > 8 ) && ( ! lAlphaFieldBlanks( 9 ) ) ) {
					WaterEquipment( WaterEquipNum ).LatentFracSchedule = GetScheduleIndex( cAlphaArgs( 9 ) );

					if ( WaterEquipment( WaterEquipNum ).LatentFracSchedule == 0 ) {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 9 ) + '=' + cAlphaArgs( 9 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					}
				}

			} // WaterEquipNum

			if ( ErrorsFound ) ShowFatalError( "Errors found in processing input for " + cCurrentModuleObject );

		}

		cCurrentModuleObject = "WaterUse:Connections";
		NumWaterConnections = GetNumObjectsFound( cCurrentModuleObject );

		if ( NumWaterConnections > 0 ) {
			WaterConnections.allocate( NumWaterConnections );

			for ( WaterConnNum = 1; WaterConnNum <= NumWaterConnections; ++WaterConnNum ) {
				GetObjectItem( cCurrentModuleObject, WaterConnNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

				IsNotOK = false;
				IsBlank = false;
				VerifyName( cAlphaArgs( 1 ), WaterConnections, WaterConnNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
				}
				WaterConnections( WaterConnNum ).Name = cAlphaArgs( 1 );

				if ( ( ! lAlphaFieldBlanks( 2 ) ) || ( ! lAlphaFieldBlanks( 3 ) ) ) {
					WaterConnections( WaterConnNum ).InletNode = GetOnlySingleNode( cAlphaArgs( 2 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
					WaterConnections( WaterConnNum ).OutletNode = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

					// Check plant connections
					TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 2 ), cAlphaArgs( 3 ), "DHW Nodes" );
				} else {
					// If no plant nodes are connected, simulate in stand-alone mode.
					WaterConnections( WaterConnNum ).StandAlone = true;
				}

				if ( ! lAlphaFieldBlanks( 4 ) ) {
					SetupTankDemandComponent( WaterConnections( WaterConnNum ).Name, cCurrentModuleObject, cAlphaArgs( 4 ), ErrorsFound, WaterConnections( WaterConnNum ).SupplyTankNum, WaterConnections( WaterConnNum ).TankDemandID );
				}

				if ( ! lAlphaFieldBlanks( 5 ) ) {
					SetupTankSupplyComponent( WaterConnections( WaterConnNum ).Name, cCurrentModuleObject, cAlphaArgs( 5 ), ErrorsFound, WaterConnections( WaterConnNum ).RecoveryTankNum, WaterConnections( WaterConnNum ).TankSupplyID );
				}

				if ( ! lAlphaFieldBlanks( 6 ) ) {
					WaterConnections( WaterConnNum ).HotTempSchedule = GetScheduleIndex( cAlphaArgs( 6 ) );
					// If no HotTempSchedule, there is no hot water.
					// HotTempSchedule is ignored if connected to a plant loop via WATER USE CONNECTIONS

					if ( WaterConnections( WaterConnNum ).HotTempSchedule == 0 ) {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 6 ) + '=' + cAlphaArgs( 6 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					}
				}

				if ( ! lAlphaFieldBlanks( 7 ) ) {
					WaterConnections( WaterConnNum ).ColdTempSchedule = GetScheduleIndex( cAlphaArgs( 7 ) );
					// If no ColdTempSchedule, temperatures will be calculated by WATER MAINS TEMPERATURES object

					if ( WaterConnections( WaterConnNum ).ColdTempSchedule == 0 ) {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 7 ) + '=' + cAlphaArgs( 7 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					}
				}

				if ( ( ! lAlphaFieldBlanks( 8 ) ) && ( cAlphaArgs( 8 ) != "NONE" ) ) {
					WaterConnections( WaterConnNum ).HeatRecovery = true;

					{ auto const SELECT_CASE_var( cAlphaArgs( 8 ) );
					if ( SELECT_CASE_var == "IDEAL" ) {
						WaterConnections( WaterConnNum ).HeatRecoveryHX = HeatRecoveryHXIdeal;
					} else if ( SELECT_CASE_var == "COUNTERFLOW" ) {
						WaterConnections( WaterConnNum ).HeatRecoveryHX = HeatRecoveryHXCounterFlow;
					} else if ( SELECT_CASE_var == "CROSSFLOW" ) {
						WaterConnections( WaterConnNum ).HeatRecoveryHX = HeatRecoveryHXCrossFlow;
					} else {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 8 ) + '=' + cAlphaArgs( 8 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					}}

					{ auto const SELECT_CASE_var( cAlphaArgs( 9 ) );
					if ( SELECT_CASE_var == "PLANT" ) {
						WaterConnections( WaterConnNum ).HeatRecoveryConfig = HeatRecoveryConfigPlant;
					} else if ( SELECT_CASE_var == "EQUIPMENT" ) {
						WaterConnections( WaterConnNum ).HeatRecoveryConfig = HeatRecoveryConfigEquipment;
					} else if ( SELECT_CASE_var == "PLANTANDEQUIPMENT" ) {
						WaterConnections( WaterConnNum ).HeatRecoveryConfig = HeatRecoveryConfigPlantAndEquip;
					} else {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 9 ) + '=' + cAlphaArgs( 9 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					}}
				}

				WaterConnections( WaterConnNum ).HXUA = rNumericArgs( 1 );

				WaterConnections( WaterConnNum ).WaterEquipment.allocate( NumAlphas - 9 );

				for ( AlphaNum = 10; AlphaNum <= NumAlphas; ++AlphaNum ) {
					WaterEquipNum = FindItemInList( cAlphaArgs( AlphaNum ), WaterEquipment );

					if ( WaterEquipNum == 0 ) {
						ShowSevereError( "Invalid " + cAlphaFieldNames( AlphaNum ) + '=' + cAlphaArgs( AlphaNum ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					} else {
						if ( WaterEquipment( WaterEquipNum ).Connections > 0 ) {
							ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  WaterUse:Equipment = " + cAlphaArgs( AlphaNum ) + " is already referenced by another object." );
							ErrorsFound = true;
						} else {
							WaterEquipment( WaterEquipNum ).Connections = WaterConnNum;

							++WaterConnections( WaterConnNum ).NumWaterEquipment;
							WaterConnections( WaterConnNum ).WaterEquipment( WaterConnections( WaterConnNum ).NumWaterEquipment ) = WaterEquipNum;

							WaterConnections( WaterConnNum ).PeakVolFlowRate += WaterEquipment( WaterEquipNum ).PeakVolFlowRate; // this does not include possible multipliers
						}
					}
				}

			} // WaterConnNum

			if ( ErrorsFound ) ShowFatalError( "Errors found in processing input for " + cCurrentModuleObject );

			if ( NumWaterConnections > 0 ) {
				CheckEquipName.allocate( NumWaterConnections );
				CheckPlantLoop.allocate( NumWaterConnections );
				CheckEquipName = true;
				CheckPlantLoop = true;
			}

		}

		// determine connection's peak mass flow rates.
		if ( NumWaterConnections > 0 ) {
			for ( WaterConnNum = 1; WaterConnNum <= NumWaterConnections; ++WaterConnNum ) {
				WaterConnections( WaterConnNum ).PeakMassFlowRate = 0.0;
				for ( WaterEquipNum = 1; WaterEquipNum <= WaterConnections( WaterConnNum ).NumWaterEquipment; ++WaterEquipNum ) {
					thisWaterEquipNum = WaterConnections( WaterConnNum ).WaterEquipment( WaterEquipNum );
					if ( WaterEquipment( thisWaterEquipNum ).Zone > 0 ) {
						WaterConnections( WaterConnNum ).PeakMassFlowRate += WaterEquipment( thisWaterEquipNum ).PeakVolFlowRate * RhoH2O( InitConvTemp ) * Zone( WaterEquipment( thisWaterEquipNum ).Zone ).Multiplier * Zone( WaterEquipment( thisWaterEquipNum ).Zone ).ListMultiplier;
					} else { // can't have multipliers
						WaterConnections( WaterConnNum ).PeakMassFlowRate += WaterEquipment( thisWaterEquipNum ).PeakVolFlowRate * RhoH2O( InitConvTemp );
					}
				}
				RegisterPlantCompDesignFlow( WaterConnections( WaterConnNum ).InletNode, WaterConnections( WaterConnNum ).PeakMassFlowRate / RhoH2O( InitConvTemp ) );
			}
		}

		// Setup EQUIPMENT report variables (now that connections have been established)
		// CurrentModuleObject='WaterUse:Equipment'
		for ( WaterEquipNum = 1; WaterEquipNum <= NumWaterEquipment; ++WaterEquipNum ) {

			SetupOutputVariable( "Water Use Equipment Hot Water Mass Flow Rate [kg/s]", WaterEquipment( WaterEquipNum ).HotMassFlowRate, "System", "Average", WaterEquipment( WaterEquipNum ).Name );

			SetupOutputVariable( "Water Use Equipment Cold Water Mass Flow Rate [kg/s]", WaterEquipment( WaterEquipNum ).ColdMassFlowRate, "System", "Average", WaterEquipment( WaterEquipNum ).Name );

			SetupOutputVariable( "Water Use Equipment Total Mass Flow Rate [kg/s]", WaterEquipment( WaterEquipNum ).TotalMassFlowRate, "System", "Average", WaterEquipment( WaterEquipNum ).Name );

			SetupOutputVariable( "Water Use Equipment Hot Water Volume Flow Rate [m3/s]", WaterEquipment( WaterEquipNum ).HotVolFlowRate, "System", "Average", WaterEquipment( WaterEquipNum ).Name );

			SetupOutputVariable( "Water Use Equipment Cold Water Volume Flow Rate [m3/s]", WaterEquipment( WaterEquipNum ).ColdVolFlowRate, "System", "Average", WaterEquipment( WaterEquipNum ).Name );

			SetupOutputVariable( "Water Use Equipment Total Volume Flow Rate [m3/s]", WaterEquipment( WaterEquipNum ).TotalVolFlowRate, "System", "Average", WaterEquipment( WaterEquipNum ).Name );

			SetupOutputVariable( "Water Use Equipment Hot Water Volume [m3]", WaterEquipment( WaterEquipNum ).HotVolume, "System", "Sum", WaterEquipment( WaterEquipNum ).Name );

			SetupOutputVariable( "Water Use Equipment Cold Water Volume [m3]", WaterEquipment( WaterEquipNum ).ColdVolume, "System", "Sum", WaterEquipment( WaterEquipNum ).Name );

			SetupOutputVariable( "Water Use Equipment Total Volume [m3]", WaterEquipment( WaterEquipNum ).TotalVolume, "System", "Sum", WaterEquipment( WaterEquipNum ).Name, _, "Water", "WATERSYSTEMS", WaterEquipment( WaterEquipNum ).EndUseSubcatName, "Plant" );
			SetupOutputVariable( "Water Use Equipment Mains Water Volume [m3]", WaterEquipment( WaterEquipNum ).TotalVolume, "System", "Sum", WaterEquipment( WaterEquipNum ).Name, _, "MainsWater", "WATERSYSTEMS", WaterEquipment( WaterEquipNum ).EndUseSubcatName, "Plant" );

			SetupOutputVariable( "Water Use Equipment Hot Water Temperature [C]", WaterEquipment( WaterEquipNum ).HotTemp, "System", "Average", WaterEquipment( WaterEquipNum ).Name );

			SetupOutputVariable( "Water Use Equipment Cold Water Temperature [C]", WaterEquipment( WaterEquipNum ).ColdTemp, "System", "Average", WaterEquipment( WaterEquipNum ).Name );

			SetupOutputVariable( "Water Use Equipment Target Water Temperature [C]", WaterEquipment( WaterEquipNum ).TargetTemp, "System", "Average", WaterEquipment( WaterEquipNum ).Name );

			SetupOutputVariable( "Water Use Equipment Mixed Water Temperature [C]", WaterEquipment( WaterEquipNum ).MixedTemp, "System", "Average", WaterEquipment( WaterEquipNum ).Name );

			SetupOutputVariable( "Water Use Equipment Drain Water Temperature [C]", WaterEquipment( WaterEquipNum ).DrainTemp, "System", "Average", WaterEquipment( WaterEquipNum ).Name );

			SetupOutputVariable( "Water Use Equipment Heating Rate [W]", WaterEquipment( WaterEquipNum ).Power, "System", "Average", WaterEquipment( WaterEquipNum ).Name );

			if ( WaterEquipment( WaterEquipNum ).Connections == 0 ) {
				SetupOutputVariable( "Water Use Equipment Heating Energy [J]", WaterEquipment( WaterEquipNum ).Energy, "System", "Sum", WaterEquipment( WaterEquipNum ).Name, _, "DISTRICTHEATING", "WATERSYSTEMS", WaterEquipment( WaterEquipNum ).EndUseSubcatName, "Plant" );

			} else if ( WaterConnections( WaterEquipment( WaterEquipNum ).Connections ).StandAlone ) {
				SetupOutputVariable( "Water Use Equipment Heating Energy [J]", WaterEquipment( WaterEquipNum ).Energy, "System", "Sum", WaterEquipment( WaterEquipNum ).Name, _, "DISTRICTHEATING", "WATERSYSTEMS", WaterEquipment( WaterEquipNum ).EndUseSubcatName, "Plant" );

			} else { // The EQUIPMENT is coupled to a plant loop via a CONNECTIONS object
				SetupOutputVariable( "Water Use Equipment Heating Energy [J]", WaterEquipment( WaterEquipNum ).Energy, "System", "Sum", WaterEquipment( WaterEquipNum ).Name, _, "ENERGYTRANSFER", "WATERSYSTEMS", WaterEquipment( WaterEquipNum ).EndUseSubcatName, "Plant" );
			}

			if ( WaterEquipment( WaterEquipNum ).Zone > 0 ) {
				SetupOutputVariable( "Water Use Equipment Zone Sensible Heat Gain Rate [W]", WaterEquipment( WaterEquipNum ).SensibleRate, "System", "Average", WaterEquipment( WaterEquipNum ).Name );
				SetupOutputVariable( "Water Use Equipment Zone Sensible Heat Gain Energy [J]", WaterEquipment( WaterEquipNum ).SensibleEnergy, "System", "Sum", WaterEquipment( WaterEquipNum ).Name );

				SetupOutputVariable( "Water Use Equipment Zone Latent Gain Rate [W]", WaterEquipment( WaterEquipNum ).LatentRate, "System", "Average", WaterEquipment( WaterEquipNum ).Name );
				SetupOutputVariable( "Water Use Equipment Zone Latent Gain Energy [J]", WaterEquipment( WaterEquipNum ).LatentEnergy, "System", "Sum", WaterEquipment( WaterEquipNum ).Name );

				SetupOutputVariable( "Water Use Equipment Zone Moisture Gain Mass Flow Rate [kg/s]", WaterEquipment( WaterEquipNum ).MoistureRate, "System", "Average", WaterEquipment( WaterEquipNum ).Name );
				SetupOutputVariable( "Water Use Equipment Zone Moisture Gain Mass [kg]", WaterEquipment( WaterEquipNum ).MoistureMass, "System", "Sum", WaterEquipment( WaterEquipNum ).Name );

				SetupZoneInternalGain( WaterEquipment( WaterEquipNum ).Zone, "WaterUse:Equipment", WaterEquipment( WaterEquipNum ).Name, IntGainTypeOf_WaterUseEquipment, WaterEquipment( WaterEquipNum ).SensibleRateNoMultiplier, _, _, WaterEquipment( WaterEquipNum ).LatentRateNoMultiplier );

			}

		} // WaterEquipNum

		// Setup CONNECTIONS report variables (don't put any on meters; they are metered at WATER USE EQUIPMENT level)
		// CurrentModuleObject='WaterUse:Connections'
		for ( WaterConnNum = 1; WaterConnNum <= NumWaterConnections; ++WaterConnNum ) {

			SetupOutputVariable( "Water Use Connections Hot Water Mass Flow Rate [kg/s]", WaterConnections( WaterConnNum ).HotMassFlowRate, "System", "Average", WaterConnections( WaterConnNum ).Name );

			SetupOutputVariable( "Water Use Connections Cold Water Mass Flow Rate [kg/s]", WaterConnections( WaterConnNum ).ColdMassFlowRate, "System", "Average", WaterConnections( WaterConnNum ).Name );

			SetupOutputVariable( "Water Use Connections Total Mass Flow Rate [kg/s]", WaterConnections( WaterConnNum ).TotalMassFlowRate, "System", "Average", WaterConnections( WaterConnNum ).Name );

			SetupOutputVariable( "Water Use Connections Drain Water Mass Flow Rate [kg/s]", WaterConnections( WaterConnNum ).DrainMassFlowRate, "System", "Average", WaterConnections( WaterConnNum ).Name );

			SetupOutputVariable( "Water Use Connections Heat Recovery Mass Flow Rate [kg/s]", WaterConnections( WaterConnNum ).RecoveryMassFlowRate, "System", "Average", WaterConnections( WaterConnNum ).Name );

			SetupOutputVariable( "Water Use Connections Hot Water Volume Flow Rate [m3/s]", WaterConnections( WaterConnNum ).HotVolFlowRate, "System", "Average", WaterConnections( WaterConnNum ).Name );

			SetupOutputVariable( "Water Use Connections Cold Water Volume Flow Rate [m3/s]", WaterConnections( WaterConnNum ).ColdVolFlowRate, "System", "Average", WaterConnections( WaterConnNum ).Name );

			SetupOutputVariable( "Water Use Connections Total Volume Flow Rate [m3/s]", WaterConnections( WaterConnNum ).TotalVolFlowRate, "System", "Average", WaterConnections( WaterConnNum ).Name );

			SetupOutputVariable( "Water Use Connections Hot Water Volume [m3]", WaterConnections( WaterConnNum ).HotVolume, "System", "Sum", WaterConnections( WaterConnNum ).Name );

			SetupOutputVariable( "Water Use Connections Cold Water Volume [m3]", WaterConnections( WaterConnNum ).ColdVolume, "System", "Sum", WaterConnections( WaterConnNum ).Name );

			SetupOutputVariable( "Water Use Connections Total Volume [m3]", WaterConnections( WaterConnNum ).TotalVolume, "System", "Sum", WaterConnections( WaterConnNum ).Name ); //, &
			// ResourceTypeKey='Water', EndUseKey='DHW', EndUseSubKey=EndUseSubcategoryName, GroupKey='Plant')
			// tHIS WAS double counting

			SetupOutputVariable( "Water Use Connections Hot Water Temperature [C]", WaterConnections( WaterConnNum ).HotTemp, "System", "Average", WaterConnections( WaterConnNum ).Name );

			SetupOutputVariable( "Water Use Connections Cold Water Temperature [C]", WaterConnections( WaterConnNum ).ColdTemp, "System", "Average", WaterConnections( WaterConnNum ).Name );

			SetupOutputVariable( "Water Use Connections Drain Water Temperature [C]", WaterConnections( WaterConnNum ).DrainTemp, "System", "Average", WaterConnections( WaterConnNum ).Name );

			SetupOutputVariable( "Water Use Connections Return Water Temperature [C]", WaterConnections( WaterConnNum ).ReturnTemp, "System", "Average", WaterConnections( WaterConnNum ).Name );

			SetupOutputVariable( "Water Use Connections Waste Water Temperature [C]", WaterConnections( WaterConnNum ).WasteTemp, "System", "Average", WaterConnections( WaterConnNum ).Name );

			SetupOutputVariable( "Water Use Connections Heat Recovery Water Temperature [C]", WaterConnections( WaterConnNum ).RecoveryTemp, "System", "Average", WaterConnections( WaterConnNum ).Name );

			SetupOutputVariable( "Water Use Connections Heat Recovery Effectiveness []", WaterConnections( WaterConnNum ).Effectiveness, "System", "Average", WaterConnections( WaterConnNum ).Name );

			SetupOutputVariable( "Water Use Connections Heat Recovery Rate [W]", WaterConnections( WaterConnNum ).RecoveryRate, "System", "Average", WaterConnections( WaterConnNum ).Name );
			SetupOutputVariable( "Water Use Connections Heat Recovery Energy [J]", WaterConnections( WaterConnNum ).RecoveryEnergy, "System", "Sum", WaterConnections( WaterConnNum ).Name );
			// Does this go on a meter?

			// To do:  Add report variable for starved flow when tank can't deliver?

			if ( ! WaterConnections( WaterConnNum ).StandAlone ) {
				SetupOutputVariable( "Water Use Connections Plant Hot Water Energy [J]", WaterConnections( WaterConnNum ).Energy, "System", "Sum", WaterConnections( WaterConnNum ).Name, _, "PLANTLOOPHEATINGDEMAND", "WATERSYSTEMS", _, "Plant" );
			}

		} // WaterConnNum

	}

	void
	CalcEquipmentFlowRates( int const WaterEquipNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   August 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate desired hot and cold water flow rates

		// METHODOLOGY EMPLOYED:

		// Using/Aliasing
		using ScheduleManager::GetCurrentScheduleValue;
		using Psychrometrics::RhoH2O;
		using DataEnvironment::WaterMainsTemp;
		using DataHeatBalance::Zone;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int WaterConnNum;

		// FLOW:
		WaterConnNum = WaterEquipment( WaterEquipNum ).Connections;

		if ( WaterConnNum > 0 ) {
			// Get water temperature conditions from the CONNECTIONS object
			WaterEquipment( WaterEquipNum ).ColdTemp = WaterConnections( WaterConnNum ).ColdTemp;
			WaterEquipment( WaterEquipNum ).HotTemp = WaterConnections( WaterConnNum ).HotTemp;

		} else {
			// Get water temperature conditions from the WATER USE EQUIPMENT schedules
			if ( WaterEquipment( WaterEquipNum ).ColdTempSchedule > 0 ) {
				WaterEquipment( WaterEquipNum ).ColdTemp = GetCurrentScheduleValue( WaterEquipment( WaterEquipNum ).ColdTempSchedule );
			} else { // If no ColdTempSchedule, use the mains temperature
				WaterEquipment( WaterEquipNum ).ColdTemp = WaterMainsTemp;
			}

			if ( WaterEquipment( WaterEquipNum ).HotTempSchedule > 0 ) {
				WaterEquipment( WaterEquipNum ).HotTemp = GetCurrentScheduleValue( WaterEquipment( WaterEquipNum ).HotTempSchedule );
			} else { // If no HotTempSchedule, use all cold water
				WaterEquipment( WaterEquipNum ).HotTemp = WaterEquipment( WaterEquipNum ).ColdTemp;
			}
		}

		if ( WaterEquipment( WaterEquipNum ).TargetTempSchedule > 0 ) {
			WaterEquipment( WaterEquipNum ).TargetTemp = GetCurrentScheduleValue( WaterEquipment( WaterEquipNum ).TargetTempSchedule );
		} else { // If no TargetTempSchedule, use all hot water
			WaterEquipment( WaterEquipNum ).TargetTemp = WaterEquipment( WaterEquipNum ).HotTemp;
		}

		// Get the requested total flow rate
		// 11-17-2006 BG Added multipliers in next block
		if ( WaterEquipment( WaterEquipNum ).Zone > 0 ) {
			if ( WaterEquipment( WaterEquipNum ).FlowRateFracSchedule > 0 ) {
				WaterEquipment( WaterEquipNum ).TotalVolFlowRate = WaterEquipment( WaterEquipNum ).PeakVolFlowRate * GetCurrentScheduleValue( WaterEquipment( WaterEquipNum ).FlowRateFracSchedule ) * Zone( WaterEquipment( WaterEquipNum ).Zone ).Multiplier * Zone( WaterEquipment( WaterEquipNum ).Zone ).ListMultiplier;
			} else {
				WaterEquipment( WaterEquipNum ).TotalVolFlowRate = WaterEquipment( WaterEquipNum ).PeakVolFlowRate * Zone( WaterEquipment( WaterEquipNum ).Zone ).Multiplier * Zone( WaterEquipment( WaterEquipNum ).Zone ).ListMultiplier;
			}
		} else {
			if ( WaterEquipment( WaterEquipNum ).FlowRateFracSchedule > 0 ) {
				WaterEquipment( WaterEquipNum ).TotalVolFlowRate = WaterEquipment( WaterEquipNum ).PeakVolFlowRate * GetCurrentScheduleValue( WaterEquipment( WaterEquipNum ).FlowRateFracSchedule );
			} else {
				WaterEquipment( WaterEquipNum ).TotalVolFlowRate = WaterEquipment( WaterEquipNum ).PeakVolFlowRate;
			}
		}

		WaterEquipment( WaterEquipNum ).TotalMassFlowRate = WaterEquipment( WaterEquipNum ).TotalVolFlowRate * RhoH2O( InitConvTemp );

		// Calculate hot and cold water mixing at the tap
		if ( WaterEquipment( WaterEquipNum ).TotalMassFlowRate > 0.0 ) {
			// Calculate the flow rates needed to meet the target temperature
			if ( WaterEquipment( WaterEquipNum ).HotTemp == WaterEquipment( WaterEquipNum ).ColdTemp ) { // Avoid divide by zero
				// There is no hot water
				WaterEquipment( WaterEquipNum ).HotMassFlowRate = 0.0;

				// Need a special case for HotTemp < ColdTemp, due to bad user input  (but could happen in a plant loop accidentally)

			} else if ( WaterEquipment( WaterEquipNum ).TargetTemp > WaterEquipment( WaterEquipNum ).HotTemp ) {
				WaterEquipment( WaterEquipNum ).HotMassFlowRate = WaterEquipment( WaterEquipNum ).TotalMassFlowRate;

			} else {
				WaterEquipment( WaterEquipNum ).HotMassFlowRate = WaterEquipment( WaterEquipNum ).TotalMassFlowRate * ( WaterEquipment( WaterEquipNum ).TargetTemp - WaterEquipment( WaterEquipNum ).ColdTemp ) / ( WaterEquipment( WaterEquipNum ).HotTemp - WaterEquipment( WaterEquipNum ).ColdTemp );
			}

			if ( WaterEquipment( WaterEquipNum ).HotMassFlowRate < 0.0 ) {
				// Target temp is colder than the cold water temp; don't allow colder
				WaterEquipment( WaterEquipNum ).HotMassFlowRate = 0.0;
			}

			WaterEquipment( WaterEquipNum ).ColdMassFlowRate = WaterEquipment( WaterEquipNum ).TotalMassFlowRate - WaterEquipment( WaterEquipNum ).HotMassFlowRate;

			if ( WaterEquipment( WaterEquipNum ).ColdMassFlowRate < 0.0 ) WaterEquipment( WaterEquipNum ).ColdMassFlowRate = 0.0;

			WaterEquipment( WaterEquipNum ).MixedTemp = ( WaterEquipment( WaterEquipNum ).ColdMassFlowRate * WaterEquipment( WaterEquipNum ).ColdTemp + WaterEquipment( WaterEquipNum ).HotMassFlowRate * WaterEquipment( WaterEquipNum ).HotTemp ) / WaterEquipment( WaterEquipNum ).TotalMassFlowRate;
		} else {
			WaterEquipment( WaterEquipNum ).HotMassFlowRate = 0.0;
			WaterEquipment( WaterEquipNum ).ColdMassFlowRate = 0.0;
			WaterEquipment( WaterEquipNum ).MixedTemp = WaterEquipment( WaterEquipNum ).TargetTemp;
		}

	}

	void
	CalcEquipmentDrainTemp( int const WaterEquipNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   August 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate drainwater temperature and heat and moisture gains to zone.

		// METHODOLOGY EMPLOYED:

		// Using/Aliasing
		using ScheduleManager::GetCurrentScheduleValue;
		using Psychrometrics::CPHW;
		using Psychrometrics::PsyWFnTdbRhPb;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using Psychrometrics::PsyHfgAirFnWTdb;
		using DataHeatBalFanSys::MAT;
		using DataHeatBalFanSys::ZoneAirHumRat;
		using DataHeatBalance::Zone;
		using DataEnvironment::OutBaroPress;
		using DataGlobals::SecInHour;
		using DataHVACGlobals::TimeStepSys;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ZoneNum;
		Real64 ZoneMAT;
		Real64 ZoneHumRat;
		Real64 ZoneHumRatSat;
		Real64 RhoAirDry;
		Real64 ZoneMassMax;
		Real64 FlowMassMax;
		Real64 MoistureMassMax;

		static std::string const RoutineName( "CalcEquipmentDrainTemp" );

		// FLOW:

		WaterEquipment( WaterEquipNum ).SensibleRate = 0.0;
		WaterEquipment( WaterEquipNum ).SensibleEnergy = 0.0;
		WaterEquipment( WaterEquipNum ).LatentRate = 0.0;
		WaterEquipment( WaterEquipNum ).LatentEnergy = 0.0;

		if ( ( WaterEquipment( WaterEquipNum ).Zone == 0 ) || ( WaterEquipment( WaterEquipNum ).TotalMassFlowRate == 0.0 ) ) {
			WaterEquipment( WaterEquipNum ).DrainTemp = WaterEquipment( WaterEquipNum ).MixedTemp;
			WaterEquipment( WaterEquipNum ).DrainMassFlowRate = WaterEquipment( WaterEquipNum ).TotalMassFlowRate;

		} else {
			ZoneNum = WaterEquipment( WaterEquipNum ).Zone;
			ZoneMAT = MAT( ZoneNum );

			if ( WaterEquipment( WaterEquipNum ).SensibleFracSchedule == 0 ) {
				WaterEquipment( WaterEquipNum ).SensibleRate = 0.0;
				WaterEquipment( WaterEquipNum ).SensibleEnergy = 0.0;
			} else {
				WaterEquipment( WaterEquipNum ).SensibleRate = GetCurrentScheduleValue( WaterEquipment( WaterEquipNum ).SensibleFracSchedule ) * WaterEquipment( WaterEquipNum ).TotalMassFlowRate * CPHW( InitConvTemp ) * ( WaterEquipment( WaterEquipNum ).MixedTemp - ZoneMAT );
				WaterEquipment( WaterEquipNum ).SensibleEnergy = WaterEquipment( WaterEquipNum ).SensibleRate * TimeStepSys * SecInHour;
			}

			if ( WaterEquipment( WaterEquipNum ).LatentFracSchedule == 0 ) {
				WaterEquipment( WaterEquipNum ).LatentRate = 0.0;
				WaterEquipment( WaterEquipNum ).LatentEnergy = 0.0;
			} else {
				ZoneHumRat = ZoneAirHumRat( ZoneNum );
				ZoneHumRatSat = PsyWFnTdbRhPb( ZoneMAT, 1.0, OutBaroPress, RoutineName ); // Humidratio at 100% relative humidity
				RhoAirDry = PsyRhoAirFnPbTdbW( OutBaroPress, ZoneMAT, 0.0 );

				ZoneMassMax = ( ZoneHumRatSat - ZoneHumRat ) * RhoAirDry * Zone( ZoneNum ).Volume; // Max water that can be evaporated to zone
				FlowMassMax = WaterEquipment( WaterEquipNum ).TotalMassFlowRate * TimeStepSys * SecInHour; // Max water in flow
				MoistureMassMax = min( ZoneMassMax, FlowMassMax );

				WaterEquipment( WaterEquipNum ).MoistureMass = GetCurrentScheduleValue( WaterEquipment( WaterEquipNum ).LatentFracSchedule ) * MoistureMassMax;
				WaterEquipment( WaterEquipNum ).MoistureRate = WaterEquipment( WaterEquipNum ).MoistureMass / ( TimeStepSys * SecInHour );

				WaterEquipment( WaterEquipNum ).LatentRate = WaterEquipment( WaterEquipNum ).MoistureRate * PsyHfgAirFnWTdb( ZoneHumRat, ZoneMAT );
				WaterEquipment( WaterEquipNum ).LatentEnergy = WaterEquipment( WaterEquipNum ).LatentRate * TimeStepSys * SecInHour;
			}

			WaterEquipment( WaterEquipNum ).DrainMassFlowRate = WaterEquipment( WaterEquipNum ).TotalMassFlowRate - WaterEquipment( WaterEquipNum ).MoistureRate;

			if ( WaterEquipment( WaterEquipNum ).DrainMassFlowRate == 0.0 ) {
				WaterEquipment( WaterEquipNum ).DrainTemp = WaterEquipment( WaterEquipNum ).MixedTemp;
			} else {
				WaterEquipment( WaterEquipNum ).DrainTemp = ( WaterEquipment( WaterEquipNum ).TotalMassFlowRate * CPHW( InitConvTemp ) * WaterEquipment( WaterEquipNum ).MixedTemp - WaterEquipment( WaterEquipNum ).SensibleRate - WaterEquipment( WaterEquipNum ).LatentRate ) / ( WaterEquipment( WaterEquipNum ).DrainMassFlowRate * CPHW( InitConvTemp ) );
			}
		}

	}

	void
	InitConnections( int const WaterConnNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   August 2006
		//       MODIFIED       Brent Griffith 2010, demand side update
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:

		// METHODOLOGY EMPLOYED:

		// Using/Aliasing
		using DataGlobals::DoingSizing;
		using ScheduleManager::GetCurrentScheduleValue;
		using DataLoopNode::Node;
		using DataEnvironment::WaterMainsTemp;
		using DataWater::WaterStorage;
		using DataHeatBalance::Zone;
		using DataPlant::PlantLoop;
		using DataPlant::ScanPlantLoopsForObject;
		using DataPlant::TypeOf_WaterUseConnection;
		using PlantUtilities::InitComponentNodes;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int InletNode;
		int OutletNode;
		static bool MyOneTimeFlag( true ); // one time flag                    !DSU
		static Array1D_bool SetLoopIndexFlag; // get loop number flag             !DSU
		bool errFlag;

		if ( MyOneTimeFlag ) { //DSU
			SetLoopIndexFlag.dimension( NumWaterConnections, true ); //DSU
			MyOneTimeFlag = false; //DSU
		} //DSU

		if ( SetLoopIndexFlag( WaterConnNum ) ) { //DSU
			if ( allocated( PlantLoop ) && ! WaterConnections( WaterConnNum ).StandAlone ) { //DSU
				errFlag = false;
				ScanPlantLoopsForObject( WaterConnections( WaterConnNum ).Name, TypeOf_WaterUseConnection, WaterConnections( WaterConnNum ).PlantLoopNum, WaterConnections( WaterConnNum ).PlantLoopSide, WaterConnections( WaterConnNum ).PlantLoopBranchNum, WaterConnections( WaterConnNum ).PlantLoopCompNum, _, _, _, _, _, errFlag ); //DSU | DSU | DSU | DSU | DSU | DSU | DSU
				if ( errFlag ) { //DSU
					ShowFatalError( "InitConnections: Program terminated due to previous condition(s)." ); //DSU
				} //DSU
				SetLoopIndexFlag( WaterConnNum ) = false; //DSU
			} //DSU
			if ( WaterConnections( WaterConnNum ).StandAlone ) SetLoopIndexFlag( WaterConnNum ) = false;
		}

		// Set the cold water temperature
		if ( WaterConnections( WaterConnNum ).SupplyTankNum > 0 ) {
			WaterConnections( WaterConnNum ).ColdSupplyTemp = WaterStorage( WaterConnections( WaterConnNum ).SupplyTankNum ).Twater;

		} else if ( WaterConnections( WaterConnNum ).ColdTempSchedule > 0 ) {
			WaterConnections( WaterConnNum ).ColdSupplyTemp = GetCurrentScheduleValue( WaterConnections( WaterConnNum ).ColdTempSchedule );

		} else {
			WaterConnections( WaterConnNum ).ColdSupplyTemp = WaterMainsTemp;
		}

		// Initially set ColdTemp to the ColdSupplyTemp; with heat recovery, ColdTemp will change during iteration
		WaterConnections( WaterConnNum ).ColdTemp = WaterConnections( WaterConnNum ).ColdSupplyTemp;

		// Set the hot water temperature
		if ( WaterConnections( WaterConnNum ).StandAlone ) {
			if ( WaterConnections( WaterConnNum ).HotTempSchedule > 0 ) {
				WaterConnections( WaterConnNum ).HotTemp = GetCurrentScheduleValue( WaterConnections( WaterConnNum ).HotTempSchedule );
			} else {
				// If no HotTempSchedule, use all cold water
				WaterConnections( WaterConnNum ).HotTemp = WaterConnections( WaterConnNum ).ColdTemp;
			}

		} else {
			InletNode = WaterConnections( WaterConnNum ).InletNode;
			OutletNode = WaterConnections( WaterConnNum ).OutletNode;

			if ( BeginEnvrnFlag && WaterConnections( WaterConnNum ).Init ) {
				// Clear node initial conditions
				if ( InletNode > 0 && OutletNode > 0 ) {
					InitComponentNodes( 0.0, WaterConnections( WaterConnNum ).PeakMassFlowRate, InletNode, OutletNode, WaterConnections( WaterConnNum ).PlantLoopNum, WaterConnections( WaterConnNum ).PlantLoopSide, WaterConnections( WaterConnNum ).PlantLoopBranchNum, WaterConnections( WaterConnNum ).PlantLoopCompNum );

					WaterConnections( WaterConnNum ).ReturnTemp = Node( InletNode ).Temp;
				}

				WaterConnections( WaterConnNum ).Init = false;
			}

			if ( ! BeginEnvrnFlag ) WaterConnections( WaterConnNum ).Init = true;

			if ( InletNode > 0 ) {
				if ( ! DoingSizing ) {
					WaterConnections( WaterConnNum ).HotTemp = Node( InletNode ).Temp;
				} else {
					// plant loop will not be running so need a value here.
					// should change to use tank setpoint but water use connections don't have knowledge of the tank they are fed by
					WaterConnections( WaterConnNum ).HotTemp = 60.0;
				}
			}
		}

	}

	void
	CalcConnectionsFlowRates(
		int const WaterConnNum,
		bool const FirstHVACIteration
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   August 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate summed values for WATER USE CONNECTIONS (to prepare to request flow from plant, and for reporting).

		// METHODOLOGY EMPLOYED:

		// Using/Aliasing
		using DataLoopNode::Node;
		using Psychrometrics::RhoH2O;
		using DataWater::WaterStorage;
		using PlantUtilities::SetComponentFlowRate;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int WaterEquipNum;
		int Loop;
		int InletNode;
		int OutletNode;
		int LoopNum;
		int LoopSideNum;
		Real64 AvailableFraction;
		Real64 DesiredHotWaterMassFlow; // store original request

		// FLOW:
		WaterConnections( WaterConnNum ).ColdMassFlowRate = 0.0;
		WaterConnections( WaterConnNum ).HotMassFlowRate = 0.0;

		for ( Loop = 1; Loop <= WaterConnections( WaterConnNum ).NumWaterEquipment; ++Loop ) {
			WaterEquipNum = WaterConnections( WaterConnNum ).WaterEquipment( Loop );

			CalcEquipmentFlowRates( WaterEquipNum );

			WaterConnections( WaterConnNum ).ColdMassFlowRate += WaterEquipment( WaterEquipNum ).ColdMassFlowRate;
			WaterConnections( WaterConnNum ).HotMassFlowRate += WaterEquipment( WaterEquipNum ).HotMassFlowRate;
		} // Loop

		WaterConnections( WaterConnNum ).TotalMassFlowRate = WaterConnections( WaterConnNum ).ColdMassFlowRate + WaterConnections( WaterConnNum ).HotMassFlowRate;

		if ( ! WaterConnections( WaterConnNum ).StandAlone ) { // Interact with the plant loop
			InletNode = WaterConnections( WaterConnNum ).InletNode;
			OutletNode = WaterConnections( WaterConnNum ).OutletNode;
			LoopNum = WaterConnections( WaterConnNum ).PlantLoopNum;
			LoopSideNum = WaterConnections( WaterConnNum ).PlantLoopSide;
			if ( InletNode > 0 ) {
				if ( FirstHVACIteration ) {
					// Request the mass flow rate from the demand side manager
					//        Node(InletNode)%MassFlowRate = WaterConnections(WaterConnNum)%HotMassFlowRate
					//        Node(InletNode)%MassFlowRateMaxAvail = WaterConnections(WaterConnNum)%PeakMassFlowRate
					//        Node(InletNode)%MassFlowRateMinAvail = 0.0D0
					SetComponentFlowRate( WaterConnections( WaterConnNum ).HotMassFlowRate, InletNode, OutletNode, LoopNum, LoopSideNum, WaterConnections( WaterConnNum ).PlantLoopBranchNum, WaterConnections( WaterConnNum ).PlantLoopCompNum );

				} else {
					DesiredHotWaterMassFlow = WaterConnections( WaterConnNum ).HotMassFlowRate;
					SetComponentFlowRate( DesiredHotWaterMassFlow, InletNode, OutletNode, LoopNum, LoopSideNum, WaterConnections( WaterConnNum ).PlantLoopBranchNum, WaterConnections( WaterConnNum ).PlantLoopCompNum );
					//DSU3   Node(InletNode)%MassFlowRate = MIN(WaterConnections(WaterConnNum)%HotMassFlowRate, Node(InletNode)%MassFlowRateMaxAvail)
					//DSU3   Node(InletNode)%MassFlowRate = MAX(WaterConnections(WaterConnNum)%HotMassFlowRate, Node(InletNode)%MassFlowRateMinAvail)
					// readjust if more than actual available mass flow rate determined by the demand side manager
					if ( ( WaterConnections( WaterConnNum ).HotMassFlowRate != DesiredHotWaterMassFlow ) && ( WaterConnections( WaterConnNum ).HotMassFlowRate > 0.0 ) ) { // plant didn't give what was asked for

						//DSU3   Node(InletNode)%MassFlowRate = Node(InletNode)%MassFlowRateMaxAvail

						AvailableFraction = DesiredHotWaterMassFlow / WaterConnections( WaterConnNum ).HotMassFlowRate;

						//DSU3    WaterConnections(WaterConnNum)%HotMassFlowRate = Node(InletNode)%MassFlowRateMaxAvail
						WaterConnections( WaterConnNum ).ColdMassFlowRate = WaterConnections( WaterConnNum ).TotalMassFlowRate - WaterConnections( WaterConnNum ).HotMassFlowRate; // Preserve the total mass flow rate

						// Proportionally reduce hot water and increase cold water for all WATER USE EQUIPMENT
						for ( Loop = 1; Loop <= WaterConnections( WaterConnNum ).NumWaterEquipment; ++Loop ) {
							WaterEquipNum = WaterConnections( WaterConnNum ).WaterEquipment( Loop );

							// Recalculate flow rates for water equipment within connection
							WaterEquipment( WaterEquipNum ).HotMassFlowRate *= AvailableFraction;
							WaterEquipment( WaterEquipNum ).ColdMassFlowRate = WaterEquipment( WaterEquipNum ).TotalMassFlowRate - WaterEquipment( WaterEquipNum ).HotMassFlowRate;

							// Recalculate mixed water temperature
							if ( WaterEquipment( WaterEquipNum ).TotalMassFlowRate > 0.0 ) {
								WaterEquipment( WaterEquipNum ).MixedTemp = ( WaterEquipment( WaterEquipNum ).ColdMassFlowRate * WaterEquipment( WaterEquipNum ).ColdTemp + WaterEquipment( WaterEquipNum ).HotMassFlowRate * WaterEquipment( WaterEquipNum ).HotTemp ) / WaterEquipment( WaterEquipNum ).TotalMassFlowRate;
							} else {
								WaterEquipment( WaterEquipNum ).MixedTemp = WaterEquipment( WaterEquipNum ).TargetTemp;
							}
						} // Loop
					}
				}
			}
		}

		if ( WaterConnections( WaterConnNum ).SupplyTankNum > 0 ) {
			// Set the demand request for supply water from water storage tank
			WaterConnections( WaterConnNum ).ColdVolFlowRate = WaterConnections( WaterConnNum ).ColdMassFlowRate / RhoH2O( InitConvTemp );
			WaterStorage( WaterConnections( WaterConnNum ).SupplyTankNum ).VdotRequestDemand( WaterConnections( WaterConnNum ).TankDemandID ) = WaterConnections( WaterConnNum ).ColdVolFlowRate;

			// Check if cold flow rate should be starved by restricted flow from tank
			// Currently, the tank flow is not really starved--water continues to flow at the tank water temperature
			// But the user can see the error by comparing report variables for TankVolFlowRate < ColdVolFlowRate
			WaterConnections( WaterConnNum ).TankVolFlowRate = WaterStorage( WaterConnections( WaterConnNum ).SupplyTankNum ).VdotAvailDemand( WaterConnections( WaterConnNum ).TankDemandID );
			WaterConnections( WaterConnNum ).TankMassFlowRate = WaterConnections( WaterConnNum ).TankVolFlowRate * RhoH2O( InitConvTemp );
		}

	}

	void
	CalcConnectionsDrainTemp( int const WaterConnNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   August 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate

		// METHODOLOGY EMPLOYED:

		// Using/Aliasing
		using Psychrometrics::RhoH2O;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int WaterEquipNum;
		int Loop;
		Real64 MassFlowTempSum;

		// FLOW:
		WaterConnections( WaterConnNum ).DrainMassFlowRate = 0.0;
		MassFlowTempSum = 0.0;

		for ( Loop = 1; Loop <= WaterConnections( WaterConnNum ).NumWaterEquipment; ++Loop ) {
			WaterEquipNum = WaterConnections( WaterConnNum ).WaterEquipment( Loop );

			CalcEquipmentDrainTemp( WaterEquipNum );

			WaterConnections( WaterConnNum ).DrainMassFlowRate += WaterEquipment( WaterEquipNum ).DrainMassFlowRate;
			MassFlowTempSum += WaterEquipment( WaterEquipNum ).DrainMassFlowRate * WaterEquipment( WaterEquipNum ).DrainTemp;
		} // Loop

		if ( WaterConnections( WaterConnNum ).DrainMassFlowRate > 0.0 ) {
			WaterConnections( WaterConnNum ).DrainTemp = MassFlowTempSum / WaterConnections( WaterConnNum ).DrainMassFlowRate;
		} else {
			WaterConnections( WaterConnNum ).DrainTemp = WaterConnections( WaterConnNum ).HotTemp;
		}

		WaterConnections( WaterConnNum ).DrainVolFlowRate = WaterConnections( WaterConnNum ).DrainMassFlowRate * RhoH2O( InitConvTemp );

	}

	void
	CalcConnectionsHeatRecovery( int const WaterConnNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   August 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate drainwater heat recovery

		// METHODOLOGY EMPLOYED:

		// Using/Aliasing
		using Psychrometrics::CPHW;
		//unused0909  USE DataEnvironment, ONLY: WaterMainsTemp
		using DataWater::WaterStorage;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 CapacityRatio;
		Real64 NTU;
		Real64 ExpVal;
		Real64 HXCapacityRate;
		Real64 DrainCapacityRate;
		Real64 MinCapacityRate;

		// FLOW:
		if ( ! WaterConnections( WaterConnNum ).HeatRecovery ) {
			WaterConnections( WaterConnNum ).RecoveryTemp = WaterConnections( WaterConnNum ).ColdSupplyTemp;
			WaterConnections( WaterConnNum ).ReturnTemp = WaterConnections( WaterConnNum ).ColdSupplyTemp;
			WaterConnections( WaterConnNum ).WasteTemp = WaterConnections( WaterConnNum ).DrainTemp;

		} else if ( WaterConnections( WaterConnNum ).TotalMassFlowRate == 0.0 ) {
			WaterConnections( WaterConnNum ).Effectiveness = 0.0;
			WaterConnections( WaterConnNum ).RecoveryRate = 0.0;
			WaterConnections( WaterConnNum ).RecoveryTemp = WaterConnections( WaterConnNum ).ColdSupplyTemp;
			WaterConnections( WaterConnNum ).ReturnTemp = WaterConnections( WaterConnNum ).ColdSupplyTemp;
			WaterConnections( WaterConnNum ).WasteTemp = WaterConnections( WaterConnNum ).DrainTemp;

		} else { // WaterConnections(WaterConnNum)%TotalMassFlowRate > 0.0

			{ auto const SELECT_CASE_var( WaterConnections( WaterConnNum ).HeatRecoveryConfig );
			if ( SELECT_CASE_var == HeatRecoveryConfigPlant ) {
				WaterConnections( WaterConnNum ).RecoveryMassFlowRate = WaterConnections( WaterConnNum ).HotMassFlowRate;
			} else if ( SELECT_CASE_var == HeatRecoveryConfigEquipment ) {
				WaterConnections( WaterConnNum ).RecoveryMassFlowRate = WaterConnections( WaterConnNum ).ColdMassFlowRate;
			} else if ( SELECT_CASE_var == HeatRecoveryConfigPlantAndEquip ) {
				WaterConnections( WaterConnNum ).RecoveryMassFlowRate = WaterConnections( WaterConnNum ).TotalMassFlowRate;
			}}

			HXCapacityRate = CPHW( InitConvTemp ) * WaterConnections( WaterConnNum ).RecoveryMassFlowRate;
			DrainCapacityRate = CPHW( InitConvTemp ) * WaterConnections( WaterConnNum ).DrainMassFlowRate;
			MinCapacityRate = min( DrainCapacityRate, HXCapacityRate );

			{ auto const SELECT_CASE_var( WaterConnections( WaterConnNum ).HeatRecoveryHX );
			if ( SELECT_CASE_var == HeatRecoveryHXIdeal ) {
				WaterConnections( WaterConnNum ).Effectiveness = 1.0;

			} else if ( SELECT_CASE_var == HeatRecoveryHXCounterFlow ) { // Unmixed
				CapacityRatio = MinCapacityRate / max( DrainCapacityRate, HXCapacityRate );
				NTU = WaterConnections( WaterConnNum ).HXUA / MinCapacityRate;
				if ( CapacityRatio == 1.0 ) {
					WaterConnections( WaterConnNum ).Effectiveness = NTU / ( 1.0 + NTU );
				} else {
					ExpVal = std::exp( -NTU * ( 1.0 - CapacityRatio ) );
					WaterConnections( WaterConnNum ).Effectiveness = ( 1.0 - ExpVal ) / ( 1.0 - CapacityRatio * ExpVal );
				}

			} else if ( SELECT_CASE_var == HeatRecoveryHXCrossFlow ) { // Unmixed
				CapacityRatio = MinCapacityRate / max( DrainCapacityRate, HXCapacityRate );
				NTU = WaterConnections( WaterConnNum ).HXUA / MinCapacityRate;
				WaterConnections( WaterConnNum ).Effectiveness = 1.0 - std::exp( ( std::pow( NTU, 0.22 ) / CapacityRatio ) * ( std::exp( -CapacityRatio * std::pow( NTU, 0.78 ) ) - 1.0 ) );
			}}

			WaterConnections( WaterConnNum ).RecoveryRate = WaterConnections( WaterConnNum ).Effectiveness * MinCapacityRate * ( WaterConnections( WaterConnNum ).DrainTemp - WaterConnections( WaterConnNum ).ColdSupplyTemp );

			WaterConnections( WaterConnNum ).RecoveryTemp = WaterConnections( WaterConnNum ).ColdSupplyTemp + WaterConnections( WaterConnNum ).RecoveryRate / ( CPHW( InitConvTemp ) * WaterConnections( WaterConnNum ).TotalMassFlowRate );

			WaterConnections( WaterConnNum ).WasteTemp = WaterConnections( WaterConnNum ).DrainTemp - WaterConnections( WaterConnNum ).RecoveryRate / ( CPHW( InitConvTemp ) * WaterConnections( WaterConnNum ).TotalMassFlowRate );

			if ( WaterConnections( WaterConnNum ).RecoveryTankNum > 0 ) {
				WaterStorage( WaterConnections( WaterConnNum ).RecoveryTankNum ).VdotAvailSupply( WaterConnections( WaterConnNum ).TankSupplyID ) = WaterConnections( WaterConnNum ).DrainVolFlowRate;
				WaterStorage( WaterConnections( WaterConnNum ).RecoveryTankNum ).TwaterSupply( WaterConnections( WaterConnNum ).TankSupplyID ) = WaterConnections( WaterConnNum ).WasteTemp;
			}

			{ auto const SELECT_CASE_var( WaterConnections( WaterConnNum ).HeatRecoveryConfig );
			if ( SELECT_CASE_var == HeatRecoveryConfigPlant ) {
				WaterConnections( WaterConnNum ).TempError = 0.0; // No feedback back to the cold supply
				//WaterConnections(WaterConnNum)%ColdTemp = WaterConnections(WaterConnNum)%ColdSupplyTemp
				WaterConnections( WaterConnNum ).ReturnTemp = WaterConnections( WaterConnNum ).RecoveryTemp;

			} else if ( SELECT_CASE_var == HeatRecoveryConfigEquipment ) {
				WaterConnections( WaterConnNum ).TempError = std::abs( WaterConnections( WaterConnNum ).ColdTemp - WaterConnections( WaterConnNum ).RecoveryTemp );

				WaterConnections( WaterConnNum ).ColdTemp = WaterConnections( WaterConnNum ).RecoveryTemp;
				WaterConnections( WaterConnNum ).ReturnTemp = WaterConnections( WaterConnNum ).ColdSupplyTemp;

			} else if ( SELECT_CASE_var == HeatRecoveryConfigPlantAndEquip ) {
				WaterConnections( WaterConnNum ).TempError = std::abs( WaterConnections( WaterConnNum ).ColdTemp - WaterConnections( WaterConnNum ).RecoveryTemp );

				WaterConnections( WaterConnNum ).ColdTemp = WaterConnections( WaterConnNum ).RecoveryTemp;
				WaterConnections( WaterConnNum ).ReturnTemp = WaterConnections( WaterConnNum ).RecoveryTemp;
			}}
		}

	}

	void
	UpdateWaterConnections( int const WaterConnNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   August 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Updates the node variables with local variables.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// Using/Aliasing
		using DataLoopNode::Node;
		using PlantUtilities::SafeCopyPlantNode;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int InletNode;
		int OutletNode;
		int LoopNum;

		// FLOW:
		InletNode = WaterConnections( WaterConnNum ).InletNode;
		OutletNode = WaterConnections( WaterConnNum ).OutletNode;
		LoopNum = WaterConnections( WaterConnNum ).PlantLoopNum;

		if ( InletNode > 0 && OutletNode > 0 ) {
			// Pass all variables from inlet to outlet node
			SafeCopyPlantNode( InletNode, OutletNode, LoopNum );
			// DSU3 Node(OutletNode) = Node(InletNode)

			// Set outlet node variables that are possibly changed
			Node( OutletNode ).Temp = WaterConnections( WaterConnNum ).ReturnTemp;
			// should add enthalpy update to return?
		}

	}

	void
	ReportStandAloneWaterUse()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith, Peter Graham Ellis
		//       DATE WRITTEN   Nov. 2011
		//       MODIFIED       Brent Griffith, March 2010 added argument
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates report variables for stand alone water use

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// Using/Aliasing
		using DataGlobals::SecInHour;
		using DataHVACGlobals::TimeStepSys;
		using Psychrometrics::RhoH2O;
		using Psychrometrics::CPHW;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		int WaterEquipNum;

		// FLOW:
		for ( WaterEquipNum = 1; WaterEquipNum <= NumWaterEquipment; ++WaterEquipNum ) {
			WaterEquipment( WaterEquipNum ).ColdVolFlowRate = WaterEquipment( WaterEquipNum ).ColdMassFlowRate / RhoH2O( InitConvTemp );
			WaterEquipment( WaterEquipNum ).HotVolFlowRate = WaterEquipment( WaterEquipNum ).HotMassFlowRate / RhoH2O( InitConvTemp );
			WaterEquipment( WaterEquipNum ).TotalVolFlowRate = WaterEquipment( WaterEquipNum ).ColdVolFlowRate + WaterEquipment( WaterEquipNum ).HotVolFlowRate;

			WaterEquipment( WaterEquipNum ).ColdVolume = WaterEquipment( WaterEquipNum ).ColdVolFlowRate * TimeStepSys * SecInHour;
			WaterEquipment( WaterEquipNum ).HotVolume = WaterEquipment( WaterEquipNum ).HotVolFlowRate * TimeStepSys * SecInHour;
			WaterEquipment( WaterEquipNum ).TotalVolume = WaterEquipment( WaterEquipNum ).TotalVolFlowRate * TimeStepSys * SecInHour;

			if ( WaterEquipment( WaterEquipNum ).Connections == 0 ) {
				WaterEquipment( WaterEquipNum ).Power = WaterEquipment( WaterEquipNum ).HotMassFlowRate * CPHW( InitConvTemp ) * ( WaterEquipment( WaterEquipNum ).HotTemp - WaterEquipment( WaterEquipNum ).ColdTemp );
			} else {
				WaterEquipment( WaterEquipNum ).Power = WaterEquipment( WaterEquipNum ).HotMassFlowRate * CPHW( InitConvTemp ) * ( WaterEquipment( WaterEquipNum ).HotTemp - WaterConnections( WaterEquipment( WaterEquipNum ).Connections ).ReturnTemp );
			}

			WaterEquipment( WaterEquipNum ).Energy = WaterEquipment( WaterEquipNum ).Power * TimeStepSys * SecInHour;
		}

	}

	void
	ReportWaterUse( int const WaterConnNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   August 2006
		//       MODIFIED       Brent Griffith, March 2010 added argument
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates report variables.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// Using/Aliasing
		using DataGlobals::SecInHour;
		using DataHVACGlobals::TimeStepSys;
		using Psychrometrics::RhoH2O;
		using Psychrometrics::CPHW;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		int Loop;
		int WaterEquipNum;

		// FLOW:
		for ( Loop = 1; Loop <= WaterConnections( WaterConnNum ).NumWaterEquipment; ++Loop ) {
			WaterEquipNum = WaterConnections( WaterConnNum ).WaterEquipment( Loop );
			WaterEquipment( WaterEquipNum ).ColdVolFlowRate = WaterEquipment( WaterEquipNum ).ColdMassFlowRate / RhoH2O( InitConvTemp );
			WaterEquipment( WaterEquipNum ).HotVolFlowRate = WaterEquipment( WaterEquipNum ).HotMassFlowRate / RhoH2O( InitConvTemp );
			WaterEquipment( WaterEquipNum ).TotalVolFlowRate = WaterEquipment( WaterEquipNum ).ColdVolFlowRate + WaterEquipment( WaterEquipNum ).HotVolFlowRate;

			WaterEquipment( WaterEquipNum ).ColdVolume = WaterEquipment( WaterEquipNum ).ColdVolFlowRate * TimeStepSys * SecInHour;
			WaterEquipment( WaterEquipNum ).HotVolume = WaterEquipment( WaterEquipNum ).HotVolFlowRate * TimeStepSys * SecInHour;
			WaterEquipment( WaterEquipNum ).TotalVolume = WaterEquipment( WaterEquipNum ).TotalVolFlowRate * TimeStepSys * SecInHour;

			if ( WaterEquipment( WaterEquipNum ).Connections == 0 ) {
				WaterEquipment( WaterEquipNum ).Power = WaterEquipment( WaterEquipNum ).HotMassFlowRate * CPHW( InitConvTemp ) * ( WaterEquipment( WaterEquipNum ).HotTemp - WaterEquipment( WaterEquipNum ).ColdTemp );
			} else {
				WaterEquipment( WaterEquipNum ).Power = WaterEquipment( WaterEquipNum ).HotMassFlowRate * CPHW( InitConvTemp ) * ( WaterEquipment( WaterEquipNum ).HotTemp - WaterConnections( WaterEquipment( WaterEquipNum ).Connections ).ReturnTemp );
			}

			WaterEquipment( WaterEquipNum ).Energy = WaterEquipment( WaterEquipNum ).Power * TimeStepSys * SecInHour;
		}

		WaterConnections( WaterConnNum ).ColdVolFlowRate = WaterConnections( WaterConnNum ).ColdMassFlowRate / RhoH2O( InitConvTemp );
		WaterConnections( WaterConnNum ).HotVolFlowRate = WaterConnections( WaterConnNum ).HotMassFlowRate / RhoH2O( InitConvTemp );
		WaterConnections( WaterConnNum ).TotalVolFlowRate = WaterConnections( WaterConnNum ).ColdVolFlowRate + WaterConnections( WaterConnNum ).HotVolFlowRate;

		WaterConnections( WaterConnNum ).ColdVolume = WaterConnections( WaterConnNum ).ColdVolFlowRate * TimeStepSys * SecInHour;
		WaterConnections( WaterConnNum ).HotVolume = WaterConnections( WaterConnNum ).HotVolFlowRate * TimeStepSys * SecInHour;
		WaterConnections( WaterConnNum ).TotalVolume = WaterConnections( WaterConnNum ).TotalVolFlowRate * TimeStepSys * SecInHour;

		WaterConnections( WaterConnNum ).Power = WaterConnections( WaterConnNum ).HotMassFlowRate * CPHW( InitConvTemp ) * ( WaterConnections( WaterConnNum ).HotTemp - WaterConnections( WaterConnNum ).ReturnTemp );
		WaterConnections( WaterConnNum ).Energy = WaterConnections( WaterConnNum ).Power * TimeStepSys * SecInHour;

		WaterConnections( WaterConnNum ).RecoveryEnergy = WaterConnections( WaterConnNum ).RecoveryRate * TimeStepSys * SecInHour;

	}

	void
	CalcWaterUseZoneGains()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   August 2006
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the zone internal gains due to water use sensible and latent loads.

		// METHODOLOGY EMPLOYED:

		// Using/Aliasing
		using DataGlobals::BeginEnvrnFlag;
		using DataHeatBalance::Zone;

		// Locals
		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int WaterEquipNum;
		int ZoneNum;
		static bool MyEnvrnFlag( true );

		// FLOW:
		if ( NumWaterEquipment == 0 ) return;

		if ( BeginEnvrnFlag && MyEnvrnFlag ) {
			for ( auto & e : WaterEquipment ) {
				e.SensibleRate = 0.0;
				e.SensibleEnergy = 0.0;
				e.SensibleRateNoMultiplier = 0.0;
				e.LatentRate = 0.0;
				e.LatentEnergy = 0.0;
				e.LatentRateNoMultiplier = 0.0;
				e.MixedTemp = 0.0;
				e.TotalMassFlowRate = 0.0;
				e.DrainTemp = 0.0;
				e.ColdVolFlowRate = 0.0;
				e.HotVolFlowRate = 0.0;
				e.TotalVolFlowRate = 0.0;
				e.ColdMassFlowRate = 0.0;
				e.HotMassFlowRate = 0.0;
			}
			MyEnvrnFlag = false;
		}

		if ( ! BeginEnvrnFlag ) MyEnvrnFlag = true;

		for ( WaterEquipNum = 1; WaterEquipNum <= NumWaterEquipment; ++WaterEquipNum ) {
			if ( WaterEquipment( WaterEquipNum ).Zone == 0 ) continue;
			ZoneNum = WaterEquipment( WaterEquipNum ).Zone;
			WaterEquipment( WaterEquipNum ).SensibleRateNoMultiplier = WaterEquipment( WaterEquipNum ).SensibleRate / ( Zone( ZoneNum ).Multiplier * Zone( ZoneNum ).ListMultiplier ); // CR7401, back out multipliers
			WaterEquipment( WaterEquipNum ).LatentRateNoMultiplier = WaterEquipment( WaterEquipNum ).LatentRate / ( Zone( ZoneNum ).Multiplier * Zone( ZoneNum ).ListMultiplier ); // CR7401, back out multipliers
		}

		//  ! this routine needs to model approx zone gains for use during sizing
		//  IF(DoingSizing)THEN
		//    DO WaterEquipNum = 1, NumWaterEquipment
		//      WaterEquipment(WaterEquipNum)%SensibleRateNoMultiplier =
		//      WaterEquipment(WaterEquipNum)%LatentRateNoMultiplier   =
		//    END DO
		//  ENDIF

	}

} // WaterUse

} // EnergyPlus
