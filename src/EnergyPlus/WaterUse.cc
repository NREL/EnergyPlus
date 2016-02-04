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
				for ( WaterEquipmentType & waterEquip : WaterEquipment ) {
					waterEquip.SensibleRate = 0.0;
					waterEquip.SensibleEnergy = 0.0;
					waterEquip.LatentRate = 0.0;
					waterEquip.LatentEnergy = 0.0;
					waterEquip.MixedTemp = 0.0;
					waterEquip.TotalMassFlowRate = 0.0;
					waterEquip.DrainTemp = 0.0;
				}
			}

			if ( NumWaterConnections > 0 ) {
				for ( WaterConnectionsType & waterConn : WaterConnections ) waterConn.TotalMassFlowRate = 0.0;
			}

			MyEnvrnFlag = false;
		}

		if ( ! BeginEnvrnFlag ) MyEnvrnFlag = true;

		// Simulate all unconnected WATER USE EQUIPMENT objects
		for ( WaterEquipmentType & waterEquip : WaterEquipment ) {
			if ( waterEquip.Connections == 0 ) {
				waterEquip.CalcEquipmentFlowRates( );
				waterEquip.CalcEquipmentDrainTemp( );
			}
		} // WaterEquipNum

		ReportStandAloneWaterUse();

		// Simulate WATER USE CONNECTIONS objects and connected WATER USE EQUIPMENT objects
		for ( WaterConnectionsType & waterConn : WaterConnections ) {
			if ( ! waterConn.StandAlone ) continue; // only model non plant connections here

			waterConn.InitConnections( );

			int NumIteration = 0;

			while ( true ) {
				++NumIteration;

				waterConn.CalcConnectionsFlowRates( FirstHVACIteration );
				waterConn.CalcConnectionsDrainTemp( );
				waterConn.CalcConnectionsHeatRecovery( );

				if ( waterConn.TempError < Tolerance ) {
					break;
				} else if ( NumIteration > MaxIterations ) {
					if ( ! WarmupFlag ) {
						if ( waterConn.MaxIterationsErrorIndex == 0 ) {
							ShowWarningError( "WaterUse:Connections = " + waterConn.Name + ":  Heat recovery temperature did not converge" );
							ShowContinueErrorTimeStamp( "" );
						}
						ShowRecurringWarningErrorAtEnd( "WaterUse:Connections = " + waterConn.Name + ":  Heat recovery temperature did not converge", waterConn.MaxIterationsErrorIndex );
					}
					break;
				}

			} // while true

			waterConn.UpdateWaterConnections( );

			waterConn.ReportWaterUse( );

		} // for WaterConnNum

	} // SimulateWaterUse()

	PlantComponent *
	WaterConnectionsType::factory( const std::string objectName )
	{
		if ( GetWaterUseInputFlag ) {
			GetWaterUseInput();
			GetWaterUseInputFlag = false;
		}

		for ( WaterConnectionsType & waterConn : WaterConnections ) {
			if ( waterConn.Name == objectName )
				return &waterConn;
		}
		
		ShowFatalError( "SimulateWaterUseConnection: Unit not found=" + objectName );
		return nullptr;
	} // factory( )

	void
	WaterConnectionsType::onInitLoopEquip( const PlantLocation & EP_UNUSED( calledFromLocation ) )
	{
	} // ontInitLoopEquip()
	
	void
	WaterConnectionsType::simulate( const PlantLocation & EP_UNUSED( calledFromLocation ),
					bool const FirstHVACIteration,
					Real64 & EP_UNUSED( CurLoad ) )
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
		static int MaxIterationsErrorCount;
		static bool MyEnvrnFlag( true );

		// FLOW:

		if ( BeginEnvrnFlag && MyEnvrnFlag ) {
			MaxIterationsErrorCount = 0;
			if ( NumWaterEquipment > 0 ) {
				for ( WaterEquipmentType & waterEquip : WaterEquipment )
					waterEquip.reset();
			}

			if ( NumWaterConnections > 0 ) {
				for ( WaterConnectionsType & waterConn : WaterConnections ) waterConn.TotalMassFlowRate = 0.0;
			}

			MyEnvrnFlag = false;
		}

		if ( ! BeginEnvrnFlag ) MyEnvrnFlag = true;

		this->InitConnections( );

		int NumIteration = 0;

		while ( true ) {
			++NumIteration;

			this->CalcConnectionsFlowRates( FirstHVACIteration );
			this->CalcConnectionsDrainTemp( );
			this->CalcConnectionsHeatRecovery( );

			if ( this->TempError < Tolerance ) {
				break;
			} else if ( NumIteration > MaxIterations ) {
				if ( ! WarmupFlag ) {
					if ( this->MaxIterationsErrorIndex == 0 ) {
						ShowWarningError( "WaterUse:Connections = " + this->Name + ":  Heat recovery temperature did not converge" );
						ShowContinueErrorTimeStamp( "" );
					}
					ShowRecurringWarningErrorAtEnd( "WaterUse:Connections = " + this->Name + ":  Heat recovery temperature did not converge", this->MaxIterationsErrorIndex );
				}
				break;
			}

		} // WHILE

		this->UpdateWaterConnections( );

		this->ReportWaterUse( );

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
		int AlphaNum;

		// FLOW:

		cCurrentModuleObject = "WaterUse:Equipment";
		NumWaterEquipment = GetNumObjectsFound( cCurrentModuleObject );

		if ( NumWaterEquipment > 0 ) {
			WaterEquipment.allocate( NumWaterEquipment );

			for ( int WaterEquipNum = 1; WaterEquipNum <= NumWaterEquipment; ++WaterEquipNum ) {
				GetObjectItem( cCurrentModuleObject, WaterEquipNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				WaterEquipmentType & waterEquip = WaterEquipment( WaterEquipNum );

				IsNotOK = false;
				IsBlank = false;
				VerifyName( cAlphaArgs( 1 ), WaterEquipment, WaterEquipNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
				}
				waterEquip.Name = cAlphaArgs( 1 );

				waterEquip.EndUseSubcatName = cAlphaArgs( 2 );

				waterEquip.PeakVolFlowRate = rNumericArgs( 1 );

				if ( ( NumAlphas > 2 ) && ( ! lAlphaFieldBlanks( 3 ) ) ) {
					waterEquip.FlowRateFracSchedule = GetScheduleIndex( cAlphaArgs( 3 ) );
					// If no FlowRateFracSchedule, fraction defaults to 1.0

					if ( waterEquip.FlowRateFracSchedule == 0 ) {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 3 ) + '=' + cAlphaArgs( 3 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					}
				}

				if ( ( NumAlphas > 3 ) && ( ! lAlphaFieldBlanks( 4 ) ) ) {
					waterEquip.TargetTempSchedule = GetScheduleIndex( cAlphaArgs( 4 ) );

					if ( waterEquip.TargetTempSchedule == 0 ) {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 4 ) + '=' + cAlphaArgs( 4 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					}
				}

				if ( ( NumAlphas > 4 ) && ( ! lAlphaFieldBlanks( 5 ) ) ) {
					waterEquip.HotTempSchedule = GetScheduleIndex( cAlphaArgs( 5 ) );
					// If no HotTempSchedule, there is no hot water.
					// HotTempSchedule is ignored if connected to a plant loop via WATER USE CONNECTIONS

					if ( waterEquip.HotTempSchedule == 0 ) {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 5 ) + '=' + cAlphaArgs( 5 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					}
				}

				if ( ( NumAlphas > 5 ) && ( ! lAlphaFieldBlanks( 6 ) ) ) {
					waterEquip.ColdTempSchedule = GetScheduleIndex( cAlphaArgs( 6 ) );
					// If no ColdTempSchedule, temperatures will be calculated by WATER MAINS TEMPERATURES object

					if ( waterEquip.ColdTempSchedule == 0 ) {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 6 ) + '=' + cAlphaArgs( 6 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					}
				}

				if ( ( NumAlphas > 6 ) && ( ! lAlphaFieldBlanks( 7 ) ) ) {
					waterEquip.Zone = FindItemInList( cAlphaArgs( 7 ), Zone );

					if ( waterEquip.Zone == 0 ) {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 7 ) + '=' + cAlphaArgs( 7 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					}
				}

				if ( ( NumAlphas > 7 ) && ( ! lAlphaFieldBlanks( 8 ) ) ) {
					waterEquip.SensibleFracSchedule = GetScheduleIndex( cAlphaArgs( 8 ) );

					if ( waterEquip.SensibleFracSchedule == 0 ) {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 8 ) + '=' + cAlphaArgs( 8 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					}
				}

				if ( ( NumAlphas > 8 ) && ( ! lAlphaFieldBlanks( 9 ) ) ) {
					waterEquip.LatentFracSchedule = GetScheduleIndex( cAlphaArgs( 9 ) );

					if ( waterEquip.LatentFracSchedule == 0 ) {
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

			for ( int WaterConnNum = 1; WaterConnNum <= NumWaterConnections; ++WaterConnNum ) {
				GetObjectItem( cCurrentModuleObject, WaterConnNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				WaterConnectionsType & waterConn = WaterConnections( WaterConnNum );

				IsNotOK = false;
				IsBlank = false;
				VerifyName( cAlphaArgs( 1 ), WaterConnections, WaterConnNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
				}
				waterConn.Name = cAlphaArgs( 1 );

				if ( ( ! lAlphaFieldBlanks( 2 ) ) || ( ! lAlphaFieldBlanks( 3 ) ) ) {
					waterConn.InletNode = GetOnlySingleNode( cAlphaArgs( 2 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
					waterConn.OutletNode = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

					// Check plant connections
					TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 2 ), cAlphaArgs( 3 ), "DHW Nodes" );
				} else {
					// If no plant nodes are connected, simulate in stand-alone mode.
					waterConn.StandAlone = true;
				}

				if ( ! lAlphaFieldBlanks( 4 ) ) {
					SetupTankDemandComponent( waterConn.Name, cCurrentModuleObject, cAlphaArgs( 4 ), ErrorsFound, waterConn.SupplyTankNum, waterConn.TankDemandID );
				}

				if ( ! lAlphaFieldBlanks( 5 ) ) {
					SetupTankSupplyComponent( waterConn.Name, cCurrentModuleObject, cAlphaArgs( 5 ), ErrorsFound, waterConn.RecoveryTankNum, waterConn.TankSupplyID );
				}

				if ( ! lAlphaFieldBlanks( 6 ) ) {
					waterConn.HotTempSchedule = GetScheduleIndex( cAlphaArgs( 6 ) );
					// If no HotTempSchedule, there is no hot water.
					// HotTempSchedule is ignored if connected to a plant loop via WATER USE CONNECTIONS

					if ( waterConn.HotTempSchedule == 0 ) {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 6 ) + '=' + cAlphaArgs( 6 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					}
				}

				if ( ! lAlphaFieldBlanks( 7 ) ) {
					waterConn.ColdTempSchedule = GetScheduleIndex( cAlphaArgs( 7 ) );
					// If no ColdTempSchedule, temperatures will be calculated by WATER MAINS TEMPERATURES object

					if ( waterConn.ColdTempSchedule == 0 ) {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 7 ) + '=' + cAlphaArgs( 7 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					}
				}

				if ( ( ! lAlphaFieldBlanks( 8 ) ) && ( cAlphaArgs( 8 ) != "NONE" ) ) {
					waterConn.HeatRecovery = true;

					{ auto const SELECT_CASE_var( cAlphaArgs( 8 ) );
					if ( SELECT_CASE_var == "IDEAL" ) {
						waterConn.HeatRecoveryHX = HeatRecoveryHXIdeal;
					} else if ( SELECT_CASE_var == "COUNTERFLOW" ) {
						waterConn.HeatRecoveryHX = HeatRecoveryHXCounterFlow;
					} else if ( SELECT_CASE_var == "CROSSFLOW" ) {
						waterConn.HeatRecoveryHX = HeatRecoveryHXCrossFlow;
					} else {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 8 ) + '=' + cAlphaArgs( 8 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					}}

					{ auto const SELECT_CASE_var( cAlphaArgs( 9 ) );
					if ( SELECT_CASE_var == "PLANT" ) {
						waterConn.HeatRecoveryConfig = HeatRecoveryConfigPlant;
					} else if ( SELECT_CASE_var == "EQUIPMENT" ) {
						waterConn.HeatRecoveryConfig = HeatRecoveryConfigEquipment;
					} else if ( SELECT_CASE_var == "PLANTANDEQUIPMENT" ) {
						waterConn.HeatRecoveryConfig = HeatRecoveryConfigPlantAndEquip;
					} else {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 9 ) + '=' + cAlphaArgs( 9 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					}}
				}

				waterConn.HXUA = rNumericArgs( 1 );

				waterConn.localWaterEquipment.allocate( NumAlphas - 9 );

				for ( AlphaNum = 10; AlphaNum <= NumAlphas; ++AlphaNum ) {
					int WaterEquipNum = FindItemInList( cAlphaArgs( AlphaNum ), WaterEquipment );

					if ( WaterEquipNum == 0 ) {
						ShowSevereError( "Invalid " + cAlphaFieldNames( AlphaNum ) + '=' + cAlphaArgs( AlphaNum ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					} else {
						WaterEquipmentType & waterEquip = WaterEquipment( WaterEquipNum );
						if ( waterEquip.Connections > 0 ) {
							ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  WaterUse:Equipment = " + cAlphaArgs( AlphaNum ) + " is already referenced by another object." );
							ErrorsFound = true;
						} else {
							waterEquip.Connections = WaterConnNum;

							++waterConn.NumWaterEquipment;
							waterConn.localWaterEquipment( waterConn.NumWaterEquipment ) = WaterEquipNum;

							waterConn.PeakVolFlowRate += waterEquip.PeakVolFlowRate; // this does not include possible multipliers
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
			for ( WaterConnectionsType & waterConn : WaterConnections ) {
				waterConn.PeakMassFlowRate = 0.0;
				for ( int thisWaterEquipNum : waterConn.localWaterEquipment ) {
					WaterEquipmentType & thisWaterEquip = WaterEquipment( thisWaterEquipNum );
					if ( thisWaterEquip.Zone > 0 ) {
						waterConn.PeakMassFlowRate += thisWaterEquip.PeakVolFlowRate * RhoH2O( InitConvTemp ) * Zone( thisWaterEquip.Zone ).Multiplier * Zone( thisWaterEquip.Zone ).ListMultiplier;
					} else { // can't have multipliers
						waterConn.PeakMassFlowRate += thisWaterEquip.PeakVolFlowRate * RhoH2O( InitConvTemp );
					}
				}
				RegisterPlantCompDesignFlow( waterConn.InletNode, waterConn.PeakMassFlowRate / RhoH2O( InitConvTemp ) );
			}
		}

		// Setup EQUIPMENT report variables (now that connections have been established)
		// CurrentModuleObject='WaterUse:Equipment'
		for ( WaterEquipmentType & waterEquip : WaterEquipment ) {

			SetupOutputVariable( "Water Use Equipment Hot Water Mass Flow Rate [kg/s]", waterEquip.HotMassFlowRate, "System", "Average", waterEquip.Name );

			SetupOutputVariable( "Water Use Equipment Cold Water Mass Flow Rate [kg/s]", waterEquip.ColdMassFlowRate, "System", "Average", waterEquip.Name );

			SetupOutputVariable( "Water Use Equipment Total Mass Flow Rate [kg/s]", waterEquip.TotalMassFlowRate, "System", "Average", waterEquip.Name );

			SetupOutputVariable( "Water Use Equipment Hot Water Volume Flow Rate [m3/s]", waterEquip.HotVolFlowRate, "System", "Average", waterEquip.Name );

			SetupOutputVariable( "Water Use Equipment Cold Water Volume Flow Rate [m3/s]", waterEquip.ColdVolFlowRate, "System", "Average", waterEquip.Name );

			SetupOutputVariable( "Water Use Equipment Total Volume Flow Rate [m3/s]", waterEquip.TotalVolFlowRate, "System", "Average", waterEquip.Name );

			SetupOutputVariable( "Water Use Equipment Hot Water Volume [m3]", waterEquip.HotVolume, "System", "Sum", waterEquip.Name );

			SetupOutputVariable( "Water Use Equipment Cold Water Volume [m3]", waterEquip.ColdVolume, "System", "Sum", waterEquip.Name );

			SetupOutputVariable( "Water Use Equipment Total Volume [m3]", waterEquip.TotalVolume, "System", "Sum", waterEquip.Name, _, "Water", "WATERSYSTEMS", waterEquip.EndUseSubcatName, "Plant" );
			SetupOutputVariable( "Water Use Equipment Mains Water Volume [m3]", waterEquip.TotalVolume, "System", "Sum", waterEquip.Name, _, "MainsWater", "WATERSYSTEMS", waterEquip.EndUseSubcatName, "Plant" );

			SetupOutputVariable( "Water Use Equipment Hot Water Temperature [C]", waterEquip.HotTemp, "System", "Average", waterEquip.Name );

			SetupOutputVariable( "Water Use Equipment Cold Water Temperature [C]", waterEquip.ColdTemp, "System", "Average", waterEquip.Name );

			SetupOutputVariable( "Water Use Equipment Target Water Temperature [C]", waterEquip.TargetTemp, "System", "Average", waterEquip.Name );

			SetupOutputVariable( "Water Use Equipment Mixed Water Temperature [C]", waterEquip.MixedTemp, "System", "Average", waterEquip.Name );

			SetupOutputVariable( "Water Use Equipment Drain Water Temperature [C]", waterEquip.DrainTemp, "System", "Average", waterEquip.Name );

			SetupOutputVariable( "Water Use Equipment Heating Rate [W]", waterEquip.Power, "System", "Average", waterEquip.Name );

			if ( waterEquip.Connections == 0 ) {
				SetupOutputVariable( "Water Use Equipment Heating Energy [J]", waterEquip.Energy, "System", "Sum", waterEquip.Name, _, "DISTRICTHEATING", "WATERSYSTEMS", waterEquip.EndUseSubcatName, "Plant" );

			} else if ( WaterConnections( waterEquip.Connections ).StandAlone ) {
				SetupOutputVariable( "Water Use Equipment Heating Energy [J]", waterEquip.Energy, "System", "Sum", waterEquip.Name, _, "DISTRICTHEATING", "WATERSYSTEMS", waterEquip.EndUseSubcatName, "Plant" );

			} else { // The EQUIPMENT is coupled to a plant loop via a CONNECTIONS object
				SetupOutputVariable( "Water Use Equipment Heating Energy [J]", waterEquip.Energy, "System", "Sum", waterEquip.Name, _, "ENERGYTRANSFER", "WATERSYSTEMS", waterEquip.EndUseSubcatName, "Plant" );
			}

			if ( waterEquip.Zone > 0 ) {
				SetupOutputVariable( "Water Use Equipment Zone Sensible Heat Gain Rate [W]", waterEquip.SensibleRate, "System", "Average", waterEquip.Name );
				SetupOutputVariable( "Water Use Equipment Zone Sensible Heat Gain Energy [J]", waterEquip.SensibleEnergy, "System", "Sum", waterEquip.Name );

				SetupOutputVariable( "Water Use Equipment Zone Latent Gain Rate [W]", waterEquip.LatentRate, "System", "Average", waterEquip.Name );
				SetupOutputVariable( "Water Use Equipment Zone Latent Gain Energy [J]", waterEquip.LatentEnergy, "System", "Sum", waterEquip.Name );

				SetupOutputVariable( "Water Use Equipment Zone Moisture Gain Mass Flow Rate [kg/s]", waterEquip.MoistureRate, "System", "Average", waterEquip.Name );
				SetupOutputVariable( "Water Use Equipment Zone Moisture Gain Mass [kg]", waterEquip.MoistureMass, "System", "Sum", waterEquip.Name );

				SetupZoneInternalGain( waterEquip.Zone, "WaterUse:Equipment", waterEquip.Name, IntGainTypeOf_WaterUseEquipment, waterEquip.SensibleRateNoMultiplier, _, _, waterEquip.LatentRateNoMultiplier );

			}

		} // waterEquip

		// Setup CONNECTIONS report variables (don't put any on meters; they are metered at WATER USE EQUIPMENT level)
		// CurrentModuleObject='WaterUse:Connections'
		for ( WaterConnectionsType & waterConn : WaterConnections ) {

			SetupOutputVariable( "Water Use Connections Hot Water Mass Flow Rate [kg/s]", waterConn.HotMassFlowRate, "System", "Average", waterConn.Name );

			SetupOutputVariable( "Water Use Connections Cold Water Mass Flow Rate [kg/s]", waterConn.ColdMassFlowRate, "System", "Average", waterConn.Name );

			SetupOutputVariable( "Water Use Connections Total Mass Flow Rate [kg/s]", waterConn.TotalMassFlowRate, "System", "Average", waterConn.Name );

			SetupOutputVariable( "Water Use Connections Drain Water Mass Flow Rate [kg/s]", waterConn.DrainMassFlowRate, "System", "Average", waterConn.Name );

			SetupOutputVariable( "Water Use Connections Heat Recovery Mass Flow Rate [kg/s]", waterConn.RecoveryMassFlowRate, "System", "Average", waterConn.Name );

			SetupOutputVariable( "Water Use Connections Hot Water Volume Flow Rate [m3/s]", waterConn.HotVolFlowRate, "System", "Average", waterConn.Name );

			SetupOutputVariable( "Water Use Connections Cold Water Volume Flow Rate [m3/s]", waterConn.ColdVolFlowRate, "System", "Average", waterConn.Name );

			SetupOutputVariable( "Water Use Connections Total Volume Flow Rate [m3/s]", waterConn.TotalVolFlowRate, "System", "Average", waterConn.Name );

			SetupOutputVariable( "Water Use Connections Hot Water Volume [m3]", waterConn.HotVolume, "System", "Sum", waterConn.Name );

			SetupOutputVariable( "Water Use Connections Cold Water Volume [m3]", waterConn.ColdVolume, "System", "Sum", waterConn.Name );

			SetupOutputVariable( "Water Use Connections Total Volume [m3]", waterConn.TotalVolume, "System", "Sum", waterConn.Name ); //, &
			// ResourceTypeKey='Water', EndUseKey='DHW', EndUseSubKey=EndUseSubcategoryName, GroupKey='Plant')
			// tHIS WAS double counting

			SetupOutputVariable( "Water Use Connections Hot Water Temperature [C]", waterConn.HotTemp, "System", "Average", waterConn.Name );

			SetupOutputVariable( "Water Use Connections Cold Water Temperature [C]", waterConn.ColdTemp, "System", "Average", waterConn.Name );

			SetupOutputVariable( "Water Use Connections Drain Water Temperature [C]", waterConn.DrainTemp, "System", "Average", waterConn.Name );

			SetupOutputVariable( "Water Use Connections Return Water Temperature [C]", waterConn.ReturnTemp, "System", "Average", waterConn.Name );

			SetupOutputVariable( "Water Use Connections Waste Water Temperature [C]", waterConn.WasteTemp, "System", "Average", waterConn.Name );

			SetupOutputVariable( "Water Use Connections Heat Recovery Water Temperature [C]", waterConn.RecoveryTemp, "System", "Average", waterConn.Name );

			SetupOutputVariable( "Water Use Connections Heat Recovery Effectiveness []", waterConn.Effectiveness, "System", "Average", waterConn.Name );

			SetupOutputVariable( "Water Use Connections Heat Recovery Rate [W]", waterConn.RecoveryRate, "System", "Average", waterConn.Name );
			SetupOutputVariable( "Water Use Connections Heat Recovery Energy [J]", waterConn.RecoveryEnergy, "System", "Sum", waterConn.Name );
			// Does this go on a meter?

			// To do:  Add report variable for starved flow when tank can't deliver?

			if ( ! waterConn.StandAlone ) {
				SetupOutputVariable( "Water Use Connections Plant Hot Water Energy [J]", waterConn.Energy, "System", "Sum", waterConn.Name, _, "PLANTLOOPHEATINGDEMAND", "WATERSYSTEMS", _, "Plant" );
			}

		} // waterConn

	} // GetWaterUseInput()

	void
	WaterEquipmentType::CalcEquipmentFlowRates( )
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
		WaterConnNum = this->Connections;

		if ( WaterConnNum > 0 ) {
			// Get water temperature conditions from the CONNECTIONS object
			this->ColdTemp = WaterConnections( WaterConnNum ).ColdTemp;
			this->HotTemp = WaterConnections( WaterConnNum ).HotTemp;

		} else {
			// Get water temperature conditions from the WATER USE EQUIPMENT schedules
			if ( this->ColdTempSchedule > 0 ) {
				this->ColdTemp = GetCurrentScheduleValue( this->ColdTempSchedule );
			} else { // If no ColdTempSchedule, use the mains temperature
				this->ColdTemp = WaterMainsTemp;
			}

			if ( this->HotTempSchedule > 0 ) {
				this->HotTemp = GetCurrentScheduleValue( this->HotTempSchedule );
			} else { // If no HotTempSchedule, use all cold water
				this->HotTemp = this->ColdTemp;
			}
		}

		if ( this->TargetTempSchedule > 0 ) {
			this->TargetTemp = GetCurrentScheduleValue( this->TargetTempSchedule );
		} else { // If no TargetTempSchedule, use all hot water
			this->TargetTemp = this->HotTemp;
		}

		// Get the requested total flow rate
		// 11-17-2006 BG Added multipliers in next block
		if ( this->Zone > 0 ) {
			if ( this->FlowRateFracSchedule > 0 ) {
				this->TotalVolFlowRate = this->PeakVolFlowRate * GetCurrentScheduleValue( this->FlowRateFracSchedule ) * Zone( this->Zone ).Multiplier * Zone( this->Zone ).ListMultiplier;
			} else {
				this->TotalVolFlowRate = this->PeakVolFlowRate * Zone( this->Zone ).Multiplier * Zone( this->Zone ).ListMultiplier;
			}
		} else {
			if ( this->FlowRateFracSchedule > 0 ) {
				this->TotalVolFlowRate = this->PeakVolFlowRate * GetCurrentScheduleValue( this->FlowRateFracSchedule );
			} else {
				this->TotalVolFlowRate = this->PeakVolFlowRate;
			}
		}

		this->TotalMassFlowRate = this->TotalVolFlowRate * RhoH2O( InitConvTemp );

		// Calculate hot and cold water mixing at the tap
		if ( this->TotalMassFlowRate > 0.0 ) {
			// Calculate the flow rates needed to meet the target temperature
			if ( this->HotTemp == this->ColdTemp ) { // Avoid divide by zero
				// There is no hot water
				this->HotMassFlowRate = 0.0;

				// Need a special case for HotTemp < ColdTemp, due to bad user input  (but could happen in a plant loop accidentally)

			} else if ( this->TargetTemp > this->HotTemp ) {
				this->HotMassFlowRate = this->TotalMassFlowRate;

			} else {
				this->HotMassFlowRate = this->TotalMassFlowRate * ( this->TargetTemp - this->ColdTemp ) / ( this->HotTemp - this->ColdTemp );
			}

			if ( this->HotMassFlowRate < 0.0 ) {
				// Target temp is colder than the cold water temp; don't allow colder
				this->HotMassFlowRate = 0.0;
			}

			this->ColdMassFlowRate = this->TotalMassFlowRate - this->HotMassFlowRate;

			if ( this->ColdMassFlowRate < 0.0 ) this->ColdMassFlowRate = 0.0;

			this->MixedTemp = ( this->ColdMassFlowRate * this->ColdTemp + this->HotMassFlowRate * this->HotTemp ) / this->TotalMassFlowRate;
		} else {
			this->HotMassFlowRate = 0.0;
			this->ColdMassFlowRate = 0.0;
			this->MixedTemp = this->TargetTemp;
		}

	}

	void
	WaterEquipmentType::CalcEquipmentDrainTemp( )
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

		this->SensibleRate = 0.0;
		this->SensibleEnergy = 0.0;
		this->LatentRate = 0.0;
		this->LatentEnergy = 0.0;

		if ( ( this->Zone == 0 ) || ( this->TotalMassFlowRate == 0.0 ) ) {
			this->DrainTemp = this->MixedTemp;
			this->DrainMassFlowRate = this->TotalMassFlowRate;

		} else {
			ZoneNum = this->Zone;
			ZoneMAT = MAT( ZoneNum );

			if ( this->SensibleFracSchedule == 0 ) {
				this->SensibleRate = 0.0;
				this->SensibleEnergy = 0.0;
			} else {
				this->SensibleRate = GetCurrentScheduleValue( this->SensibleFracSchedule ) * this->TotalMassFlowRate * CPHW( InitConvTemp ) * ( this->MixedTemp - ZoneMAT );
				this->SensibleEnergy = this->SensibleRate * TimeStepSys * SecInHour;
			}

			if ( this->LatentFracSchedule == 0 ) {
				this->LatentRate = 0.0;
				this->LatentEnergy = 0.0;
			} else {
				ZoneHumRat = ZoneAirHumRat( ZoneNum );
				ZoneHumRatSat = PsyWFnTdbRhPb( ZoneMAT, 1.0, OutBaroPress, RoutineName ); // Humidratio at 100% relative humidity
				RhoAirDry = PsyRhoAirFnPbTdbW( OutBaroPress, ZoneMAT, 0.0 );

				ZoneMassMax = ( ZoneHumRatSat - ZoneHumRat ) * RhoAirDry * Zone( ZoneNum ).Volume; // Max water that can be evaporated to zone
				FlowMassMax = this->TotalMassFlowRate * TimeStepSys * SecInHour; // Max water in flow
				MoistureMassMax = min( ZoneMassMax, FlowMassMax );

				this->MoistureMass = GetCurrentScheduleValue( this->LatentFracSchedule ) * MoistureMassMax;
				this->MoistureRate = this->MoistureMass / ( TimeStepSys * SecInHour );

				this->LatentRate = this->MoistureRate * PsyHfgAirFnWTdb( ZoneHumRat, ZoneMAT );
				this->LatentEnergy = this->LatentRate * TimeStepSys * SecInHour;
			}

			this->DrainMassFlowRate = this->TotalMassFlowRate - this->MoistureRate;

			if ( this->DrainMassFlowRate == 0.0 ) {
				this->DrainTemp = this->MixedTemp;
			} else {
				this->DrainTemp = ( this->TotalMassFlowRate * CPHW( InitConvTemp ) * this->MixedTemp - this->SensibleRate - this->LatentRate ) / ( this->DrainMassFlowRate * CPHW( InitConvTemp ) );
			}
		}

	}

	void
	WaterConnectionsType::InitConnections( )
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
		bool errFlag;

		if ( this->SetLoopIndexFlag ) { // Set to true in WaterConnectionsType constructor
			if ( allocated( PlantLoop ) && ! this->StandAlone ) { //DSU
				errFlag = false;
				ScanPlantLoopsForObject( this->Name, TypeOf_WaterUseConnection, this->PlantLoopNum, this->PlantLoopSide, this->PlantLoopBranchNum, this->PlantLoopCompNum, _, _, _, _, _, errFlag ); //DSU | DSU | DSU | DSU | DSU | DSU | DSU
				if ( errFlag ) { //DSU
					ShowFatalError( "InitConnections: Program terminated due to previous condition(s)." ); //DSU
				} //DSU
				this->SetLoopIndexFlag = false; //DSU
			} //DSU
			if ( this->StandAlone ) this->SetLoopIndexFlag = false;
		}

		// Set the cold water temperature
		if ( this->SupplyTankNum > 0 ) {
			this->ColdSupplyTemp = WaterStorage( this->SupplyTankNum ).Twater;

		} else if ( this->ColdTempSchedule > 0 ) {
			this->ColdSupplyTemp = GetCurrentScheduleValue( this->ColdTempSchedule );

		} else {
			this->ColdSupplyTemp = WaterMainsTemp;
		}

		// Initially set ColdTemp to the ColdSupplyTemp; with heat recovery, ColdTemp will change during iteration
		this->ColdTemp = this->ColdSupplyTemp;

		// Set the hot water temperature
		if ( this->StandAlone ) {
			if ( this->HotTempSchedule > 0 ) {
				this->HotTemp = GetCurrentScheduleValue( this->HotTempSchedule );
			} else {
				// If no HotTempSchedule, use all cold water
				this->HotTemp = this->ColdTemp;
			}

		} else {
			if ( BeginEnvrnFlag && this->Init ) {
				// Clear node initial conditions
				if ( this->InletNode > 0 && this->OutletNode > 0 ) {
					InitComponentNodes( 0.0, this->PeakMassFlowRate, this->InletNode, this->OutletNode, this->PlantLoopNum, this->PlantLoopSide, this->PlantLoopBranchNum, this->PlantLoopCompNum );

					this->ReturnTemp = Node( this->InletNode ).Temp;
				}

				this->Init = false;
			}

			if ( ! BeginEnvrnFlag ) this->Init = true;

			if ( this->InletNode > 0 ) {
				if ( ! DoingSizing ) {
					this->HotTemp = Node( this->InletNode ).Temp;
				} else {
					// plant loop will not be running so need a value here.
					// should change to use tank setpoint but water use connections don't have knowledge of the tank they are fed by
					this->HotTemp = 60.0;
				}
			}
		}

	} // WaterConnectionsType::InitConnections()

	void
	WaterConnectionsType::CalcConnectionsFlowRates(
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

		// FLOW:
		this->ColdMassFlowRate = 0.0;
		this->HotMassFlowRate = 0.0;

		for ( int WaterEquipNum : this->localWaterEquipment ) {
			WaterEquipmentType & waterEquip = WaterEquipment( WaterEquipNum );
			waterEquip.CalcEquipmentFlowRates( );

			this->ColdMassFlowRate += waterEquip.ColdMassFlowRate;
			this->HotMassFlowRate += waterEquip.HotMassFlowRate;
		} // Loop

		this->TotalMassFlowRate = this->ColdMassFlowRate + this->HotMassFlowRate;

		if ( ! this->StandAlone ) { // Interact with the plant loop
			if ( this->InletNode > 0 ) {
				if ( FirstHVACIteration ) {
					// Request the mass flow rate from the demand side manager
					//        Node(InletNode)%MassFlowRate = WaterConnections(WaterConnNum)%HotMassFlowRate
					//        Node(InletNode)%MassFlowRateMaxAvail = WaterConnections(WaterConnNum)%PeakMassFlowRate
					//        Node(InletNode)%MassFlowRateMinAvail = 0.0D0
					SetComponentFlowRate( this->HotMassFlowRate, this->InletNode, this->OutletNode, this->PlantLoopNum, this->PlantLoopSide, this->PlantLoopBranchNum, this->PlantLoopCompNum );

				} else {
					Real64 DesiredHotWaterMassFlow = this->HotMassFlowRate;
					SetComponentFlowRate( DesiredHotWaterMassFlow, this->InletNode, this->OutletNode, this->PlantLoopNum, this->PlantLoopSide, this->PlantLoopBranchNum, this->PlantLoopCompNum );
					//DSU3   Node(InletNode)%MassFlowRate = MIN(WaterConnections(WaterConnNum)%HotMassFlowRate, Node(InletNode)%MassFlowRateMaxAvail)
					//DSU3   Node(InletNode)%MassFlowRate = MAX(WaterConnections(WaterConnNum)%HotMassFlowRate, Node(InletNode)%MassFlowRateMinAvail)
					// readjust if more than actual available mass flow rate determined by the demand side manager
					if ( ( this->HotMassFlowRate != DesiredHotWaterMassFlow ) && ( this->HotMassFlowRate > 0.0 ) ) { // plant didn't give what was asked for

						//DSU3   Node(InletNode)%MassFlowRate = Node(InletNode)%MassFlowRateMaxAvail

						Real64 AvailableFraction = DesiredHotWaterMassFlow / this->HotMassFlowRate;

						//DSU3    WaterConnections(WaterConnNum)%HotMassFlowRate = Node(InletNode)%MassFlowRateMaxAvail
						this->ColdMassFlowRate = this->TotalMassFlowRate - this->HotMassFlowRate; // Preserve the total mass flow rate

						// Proportionally reduce hot water and increase cold water for all WATER USE EQUIPMENT
						for ( int WaterEquipNum : this->localWaterEquipment ) {
							WaterEquipmentType & waterEquip = WaterEquipment( WaterEquipNum );
							// Recalculate flow rates for water equipment within connection
							waterEquip.HotMassFlowRate *= AvailableFraction;
							waterEquip.ColdMassFlowRate = waterEquip.TotalMassFlowRate - waterEquip.HotMassFlowRate;

							// Recalculate mixed water temperature
							if ( waterEquip.TotalMassFlowRate > 0.0 ) {
								waterEquip.MixedTemp = ( waterEquip.ColdMassFlowRate * waterEquip.ColdTemp + waterEquip.HotMassFlowRate * waterEquip.HotTemp ) / waterEquip.TotalMassFlowRate;
							} else {
								waterEquip.MixedTemp = waterEquip.TargetTemp;
							}
						} // Loop
					}
				}
			}
		}

		if ( this->SupplyTankNum > 0 ) {
			// Set the demand request for supply water from water storage tank
			this->ColdVolFlowRate = this->ColdMassFlowRate / RhoH2O( InitConvTemp );
			WaterStorage( this->SupplyTankNum ).VdotRequestDemand( this->TankDemandID ) = this->ColdVolFlowRate;

			// Check if cold flow rate should be starved by restricted flow from tank
			// Currently, the tank flow is not really starved--water continues to flow at the tank water temperature
			// But the user can see the error by comparing report variables for TankVolFlowRate < ColdVolFlowRate
			this->TankVolFlowRate = WaterStorage( this->SupplyTankNum ).VdotAvailDemand( this->TankDemandID );
			this->TankMassFlowRate = this->TankVolFlowRate * RhoH2O( InitConvTemp );
		}

	} // WaterConnectionsType::CalcConnectionsFlowRates()

	void
	WaterConnectionsType::CalcConnectionsDrainTemp( )
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

		// FLOW:
		this->DrainMassFlowRate = 0.0;
		Real64 MassFlowTempSum = 0.0;

		for ( int waterEquipNum : this->localWaterEquipment ) {
			WaterEquipmentType & waterEquip = WaterEquipment( waterEquipNum );
			waterEquip.CalcEquipmentDrainTemp( );

			this->DrainMassFlowRate += waterEquip.DrainMassFlowRate;
			MassFlowTempSum += waterEquip.DrainMassFlowRate * waterEquip.DrainTemp;
		} // Loop

		if ( this->DrainMassFlowRate > 0.0 ) {
			this->DrainTemp = MassFlowTempSum / this->DrainMassFlowRate;
		} else {
			this->DrainTemp = this->HotTemp;
		}

		this->DrainVolFlowRate = this->DrainMassFlowRate * RhoH2O( InitConvTemp );

	} // WaterConnectionsType::CalcConnectionsDrainTemp()

	void
	WaterConnectionsType::CalcConnectionsHeatRecovery( )
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

		// FLOW:
		if ( ! this->HeatRecovery ) {
			this->RecoveryTemp = this->ColdSupplyTemp;
			this->ReturnTemp = this->ColdSupplyTemp;
			this->WasteTemp = this->DrainTemp;

		} else if ( this->TotalMassFlowRate == 0.0 ) {
			this->Effectiveness = 0.0;
			this->RecoveryRate = 0.0;
			this->RecoveryTemp = this->ColdSupplyTemp;
			this->ReturnTemp = this->ColdSupplyTemp;
			this->WasteTemp = this->DrainTemp;

		} else { // WaterConnections(WaterConnNum)%TotalMassFlowRate > 0.0

			{ auto const SELECT_CASE_var( this->HeatRecoveryConfig );
			if ( SELECT_CASE_var == HeatRecoveryConfigPlant ) {
				this->RecoveryMassFlowRate = this->HotMassFlowRate;
			} else if ( SELECT_CASE_var == HeatRecoveryConfigEquipment ) {
				this->RecoveryMassFlowRate = this->ColdMassFlowRate;
			} else if ( SELECT_CASE_var == HeatRecoveryConfigPlantAndEquip ) {
				this->RecoveryMassFlowRate = this->TotalMassFlowRate;
			}}

			Real64 HXCapacityRate = CPHW( InitConvTemp ) * this->RecoveryMassFlowRate;
			Real64 DrainCapacityRate = CPHW( InitConvTemp ) * this->DrainMassFlowRate;
			Real64 MinCapacityRate = min( DrainCapacityRate, HXCapacityRate );

			{ auto const SELECT_CASE_var( this->HeatRecoveryHX );
			if ( SELECT_CASE_var == HeatRecoveryHXIdeal ) {
				this->Effectiveness = 1.0;

			} else if ( SELECT_CASE_var == HeatRecoveryHXCounterFlow ) { // Unmixed
				Real64 CapacityRatio = MinCapacityRate / max( DrainCapacityRate, HXCapacityRate );
				Real64 NTU = this->HXUA / MinCapacityRate;
				if ( CapacityRatio == 1.0 ) {
					this->Effectiveness = NTU / ( 1.0 + NTU );
				} else {
					Real64 ExpVal = std::exp( -NTU * ( 1.0 - CapacityRatio ) );
					this->Effectiveness = ( 1.0 - ExpVal ) / ( 1.0 - CapacityRatio * ExpVal );
				}

			} else if ( SELECT_CASE_var == HeatRecoveryHXCrossFlow ) { // Unmixed
				Real64 CapacityRatio = MinCapacityRate / max( DrainCapacityRate, HXCapacityRate );
				Real64 NTU = this->HXUA / MinCapacityRate;
				this->Effectiveness = 1.0 - std::exp( ( std::pow( NTU, 0.22 ) / CapacityRatio ) * ( std::exp( -CapacityRatio * std::pow( NTU, 0.78 ) ) - 1.0 ) );
			}}

			this->RecoveryRate = this->Effectiveness * MinCapacityRate * ( this->DrainTemp - this->ColdSupplyTemp );

			this->RecoveryTemp = this->ColdSupplyTemp + this->RecoveryRate / ( CPHW( InitConvTemp ) * this->TotalMassFlowRate );

			this->WasteTemp = this->DrainTemp - this->RecoveryRate / ( CPHW( InitConvTemp ) * this->TotalMassFlowRate );

			if ( this->RecoveryTankNum > 0 ) {
				WaterStorage( this->RecoveryTankNum ).VdotAvailSupply( this->TankSupplyID ) = this->DrainVolFlowRate;
				WaterStorage( this->RecoveryTankNum ).TwaterSupply( this->TankSupplyID ) = this->WasteTemp;
			}

			{ auto const SELECT_CASE_var( this->HeatRecoveryConfig );
			if ( SELECT_CASE_var == HeatRecoveryConfigPlant ) {
				this->TempError = 0.0; // No feedback back to the cold supply
				//WaterConnections(WaterConnNum)%ColdTemp = WaterConnections(WaterConnNum)%ColdSupplyTemp
				this->ReturnTemp = this->RecoveryTemp;

			} else if ( SELECT_CASE_var == HeatRecoveryConfigEquipment ) {
				this->TempError = std::abs( this->ColdTemp - this->RecoveryTemp );

				this->ColdTemp = this->RecoveryTemp;
				this->ReturnTemp = this->ColdSupplyTemp;

			} else if ( SELECT_CASE_var == HeatRecoveryConfigPlantAndEquip ) {
				this->TempError = std::abs( this->ColdTemp - this->RecoveryTemp );

				this->ColdTemp = this->RecoveryTemp;
				this->ReturnTemp = this->RecoveryTemp;
			}}
		}

	} // WaterConnectionsType::CalcConnectionsHeatRecovery()

	void
	WaterConnectionsType::UpdateWaterConnections( )
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

		// FLOW:

		if ( this->InletNode > 0 && this->OutletNode > 0 ) {
			// Pass all variables from inlet to outlet node
			SafeCopyPlantNode( this->InletNode, this->OutletNode, this->PlantLoopNum );
			// DSU3 Node(OutletNode) = Node(InletNode)

			// Set outlet node variables that are possibly changed
			Node( this->OutletNode ).Temp = this->ReturnTemp;
			// should add enthalpy update to return?
		}

	} // WaterConnectionsType::UpdateWaterConnections()

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

		// FLOW:
		for ( WaterEquipmentType & waterEquip : WaterEquipment ) {
			
			waterEquip.ColdVolFlowRate = waterEquip.ColdMassFlowRate / RhoH2O( InitConvTemp );
			waterEquip.HotVolFlowRate = waterEquip.HotMassFlowRate / RhoH2O( InitConvTemp );
			waterEquip.TotalVolFlowRate = waterEquip.ColdVolFlowRate + waterEquip.HotVolFlowRate;

			waterEquip.ColdVolume = waterEquip.ColdVolFlowRate * TimeStepSys * SecInHour;
			waterEquip.HotVolume = waterEquip.HotVolFlowRate * TimeStepSys * SecInHour;
			waterEquip.TotalVolume = waterEquip.TotalVolFlowRate * TimeStepSys * SecInHour;

			if ( waterEquip.Connections == 0 ) {
				waterEquip.Power = waterEquip.HotMassFlowRate * CPHW( InitConvTemp ) * ( waterEquip.HotTemp - waterEquip.ColdTemp );
			} else {
				waterEquip.Power = waterEquip.HotMassFlowRate * CPHW( InitConvTemp ) * ( waterEquip.HotTemp - WaterConnections( waterEquip.Connections ).ReturnTemp );
			}

			waterEquip.Energy = waterEquip.Power * TimeStepSys * SecInHour;
		}

	}

	void
	WaterConnectionsType::ReportWaterUse( )
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

		// FLOW:
		for ( int WaterEquipNum : this->localWaterEquipment ) {
			WaterEquipmentType & waterEquip = WaterEquipment( WaterEquipNum );
			waterEquip.ColdVolFlowRate = waterEquip.ColdMassFlowRate / RhoH2O( InitConvTemp );
			waterEquip.HotVolFlowRate = waterEquip.HotMassFlowRate / RhoH2O( InitConvTemp );
			waterEquip.TotalVolFlowRate = waterEquip.ColdVolFlowRate + waterEquip.HotVolFlowRate;

			waterEquip.ColdVolume = waterEquip.ColdVolFlowRate * TimeStepSys * SecInHour;
			waterEquip.HotVolume = waterEquip.HotVolFlowRate * TimeStepSys * SecInHour;
			waterEquip.TotalVolume = waterEquip.TotalVolFlowRate * TimeStepSys * SecInHour;

			if ( waterEquip.Connections == 0 ) {
				waterEquip.Power = waterEquip.HotMassFlowRate * CPHW( InitConvTemp ) * ( waterEquip.HotTemp - waterEquip.ColdTemp );
			} else {
				waterEquip.Power = waterEquip.HotMassFlowRate * CPHW( InitConvTemp ) * ( waterEquip.HotTemp - WaterConnections( waterEquip.Connections ).ReturnTemp );
			}

			waterEquip.Energy = waterEquip.Power * TimeStepSys * SecInHour;
		}

		this->ColdVolFlowRate = this->ColdMassFlowRate / RhoH2O( InitConvTemp );
		this->HotVolFlowRate = this->HotMassFlowRate / RhoH2O( InitConvTemp );
		this->TotalVolFlowRate = this->ColdVolFlowRate + this->HotVolFlowRate;

		this->ColdVolume = this->ColdVolFlowRate * TimeStepSys * SecInHour;
		this->HotVolume = this->HotVolFlowRate * TimeStepSys * SecInHour;
		this->TotalVolume = this->TotalVolFlowRate * TimeStepSys * SecInHour;

		this->Power = this->HotMassFlowRate * CPHW( InitConvTemp ) * ( this->HotTemp - this->ReturnTemp );
		this->Energy = this->Power * TimeStepSys * SecInHour;

		this->RecoveryEnergy = this->RecoveryRate * TimeStepSys * SecInHour;

	} // WaterConnectionsType::ReportWaterUse()

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
		using DataHeatBalance::ZoneData;
		using DataHeatBalance::Zone;

		// Locals
		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool MyEnvrnFlag( true );

		// FLOW:
		if ( NumWaterEquipment == 0 ) return;

		if ( BeginEnvrnFlag && MyEnvrnFlag ) {
			for ( WaterEquipmentType & waterEquip : WaterEquipment ) {
				waterEquip.SensibleRate = 0.0;
				waterEquip.SensibleEnergy = 0.0;
				waterEquip.SensibleRateNoMultiplier = 0.0;
				waterEquip.LatentRate = 0.0;
				waterEquip.LatentEnergy = 0.0;
				waterEquip.LatentRateNoMultiplier = 0.0;
				waterEquip.MixedTemp = 0.0;
				waterEquip.TotalMassFlowRate = 0.0;
				waterEquip.DrainTemp = 0.0;
				waterEquip.ColdVolFlowRate = 0.0;
				waterEquip.HotVolFlowRate = 0.0;
				waterEquip.TotalVolFlowRate = 0.0;
				waterEquip.ColdMassFlowRate = 0.0;
				waterEquip.HotMassFlowRate = 0.0;
			}
			MyEnvrnFlag = false;
		}

		if ( ! BeginEnvrnFlag ) MyEnvrnFlag = true;

		for ( WaterEquipmentType & waterEquip : WaterEquipment ) {
			if ( waterEquip.Zone == 0 ) continue;
			ZoneData & z = Zone( waterEquip.Zone );
			waterEquip.SensibleRateNoMultiplier = waterEquip.SensibleRate / ( z.Multiplier * z.ListMultiplier ); // CR7401, back out multipliers
			waterEquip.LatentRateNoMultiplier = waterEquip.LatentRate / ( z.Multiplier * z.ListMultiplier ); // CR7401, back out multipliers
		}

		//  ! this routine needs to model approx zone gains for use during sizing
		//  IF(DoingSizing)THEN
		//    DO WaterEquipNum = 1, NumWaterEquipment
		//      WaterEquipment(WaterEquipNum)%SensibleRateNoMultiplier =
		//      WaterEquipment(WaterEquipNum)%LatentRateNoMultiplier   =
		//    END DO
		//  ENDIF

	} // CalcWaterUseZoneGains()

} // WaterUse

} // EnergyPlus
