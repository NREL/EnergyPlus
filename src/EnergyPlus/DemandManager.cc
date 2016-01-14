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

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <DemandManager.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataPrecisionGlobals.hh>
#include <DataZoneControls.hh>
#include <MixedAir.hh>
#include <ExteriorEnergyUse.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <OutputProcessor.hh>
#include <ScheduleManager.hh>
#include <SimulationManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace DemandManager {

	// MODULE INFORMATION:
	//       AUTHOR         Peter Graham Ellis
	//       DATE WRITTEN   July 2005
	//       MODIFIED       Simon Vidanovic (March 2015) - Introduced DemandManager:Ventilation
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This module provides controls for demand limiting various loads.

	// METHODOLOGY EMPLOYED:
	// ManageDemand is called from within the ManageHVAC routine after the first pass through SimHVAC, but
	// _before_ any variables are reported or histories are updated.  If the metered demand is above the
	// limit action is taken using the various demand managers to reduce loads.  Exterior energy use, zone
	// heat balance, and HVAC system are then resimulated as necessary.  It is possible to iterate several
	// times through ManageDemand before the final demand managers are established and the timestep can be
	// completed.

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::NumOfTimeStepInHour;
	using DataGlobals::SecInHour;
	using DataGlobals::ScheduleAlwaysOn;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	int const ManagerTypeExtLights( 1 );
	int const ManagerTypeLights( 2 );
	int const ManagerTypeElecEquip( 3 );
	int const ManagerTypeThermostats( 4 );
	int const ManagerTypeVentilation( 5 );

	int const ManagerPrioritySequential( 1 );
	int const ManagerPriorityOptimal( 2 );
	int const ManagerPriorityAll( 3 );

	int const ManagerLimitOff( 1 );
	int const ManagerLimitFixed( 2 );
	int const ManagerLimitVariable( 3 );
	int const ManagerLimitReductionRatio( 4 );

	int const ManagerSelectionAll( 1 );
	int const ManagerSelectionMany( 2 );
	int const ManagerSelectionOne( 3 );

	int const CheckCanReduce( 1 );
	int const SetLimit( 2 );
	int const ClearLimit( 3 );

	static std::string const BlankString;

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE TYPE DECLARATIONS:

	// MODULE VARIABLE DECLARATIONS:
	int NumDemandManagerList( 0 );
	int NumDemandMgr( 0 );
	int DemandManagerExtIterations( 0 );
	int DemandManagerHBIterations( 0 );
	int DemandManagerHVACIterations( 0 );
	bool GetInput( true ); // Flag to prevent input from being read multiple times

	// SUBROUTINE SPECIFICATIONS:

	// Object Data
	Array1D< DemandManagerListData > DemandManagerList;
	Array1D< DemandManagerData > DemandMgr;

	// MODULE SUBROUTINES:

	// Functions

	// Clears the global data in DemandManager.
	// Needed for unit tests, should not be normally called.
	void
	clear_state()
	{
		NumDemandManagerList = 0;
		NumDemandMgr = 0;
		DemandManagerExtIterations = 0;
		DemandManagerHBIterations = 0;
		DemandManagerHVACIterations = 0;
		GetInput = true;
		DemandManagerList.deallocate();
		DemandMgr.deallocate();
	}

	void
	ManageDemand()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   July 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// Using/Aliasing
		using DataGlobals::WarmupFlag;
		using DataGlobals::DoingSizing;

		// Locals
		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ListNum;
		static bool firstTime; // Flag to allow Demand Manager List to simulate at least once
		static bool ResimExt; // Flag to resimulate the exterior energy use simulation
		static bool ResimHB; // Flag to resimulate the heat balance simulation (including HVAC)
		static bool ResimHVAC; // Flag to resimulate the HVAC simulation
		static bool BeginDemandSim; // TRUE in the first timestep after warmup of a new environment
		static bool ClearHistory; // TRUE in the first timestep during warmup of a new environment

		// FLOW:
		if ( GetInput && ! DoingSizing ) {
			GetDemandManagerInput();
			GetDemandManagerListInput();
			GetInput = false;
		}

		if ( NumDemandManagerList > 0 ) {

			if ( WarmupFlag ) {
				BeginDemandSim = true;
				if ( ClearHistory ) {
					// Clear historical variables
					for ( ListNum = 1; ListNum <= NumDemandManagerList; ++ListNum ) {
						DemandManagerList( ListNum ).History = 0.0;
						DemandManagerList( ListNum ).MeterDemand = 0.0;
						DemandManagerList( ListNum ).AverageDemand = 0.0;
						DemandManagerList( ListNum ).PeakDemand = 0.0;
						DemandManagerList( ListNum ).ScheduledLimit = 0.0;
						DemandManagerList( ListNum ).DemandLimit = 0.0;
						DemandManagerList( ListNum ).AvoidedDemand = 0.0;
						DemandManagerList( ListNum ).OverLimit = 0.0;
						DemandManagerList( ListNum ).OverLimitDuration = 0.0;
					} // ListNum

					// Clear demand manager variables
					for ( auto & e : DemandMgr ) {
						e.Active = false;
						e.ElapsedTime = 0;
						e.ElapsedRotationTime = 0;
						e.RotatedLoadNum = 0;
					}
				}
				ClearHistory = false;
			}

			if ( ! WarmupFlag && ! DoingSizing ) {

				if ( BeginDemandSim ) {
					BeginDemandSim = false;
					ClearHistory = true;
				}

				DemandManagerExtIterations = 0;
				DemandManagerHBIterations = 0;
				DemandManagerHVACIterations = 0;

				firstTime = true;
				ResimExt = false;
				ResimHB = false;
				ResimHVAC = false;

				while ( firstTime || ResimExt || ResimHB || ResimHVAC ) {
					firstTime = false;

					Resimulate( ResimExt, ResimHB, ResimHVAC );
					ResimExt = false;
					ResimHB = false;
					ResimHVAC = false;

					SurveyDemandManagers(); // Determines which Demand Managers can reduce demand

					for ( ListNum = 1; ListNum <= NumDemandManagerList; ++ListNum ) {
						SimulateDemandManagerList( ListNum, ResimExt, ResimHB, ResimHVAC );
					} // ListNum

					ActivateDemandManagers(); // Sets limits on loads

					if ( DemandManagerExtIterations + DemandManagerHBIterations + DemandManagerHVACIterations > 500 ) {
						// This error can only happen if there is a bug in the code
						ShowFatalError( "Too many DemandManager iterations. (>500)" );
						break;
					}
				}

				for ( ListNum = 1; ListNum <= NumDemandManagerList; ++ListNum ) {
					ReportDemandManagerList( ListNum );
				} // ListNum

			}

		}

	}

	void
	SimulateDemandManagerList(
		int const ListNum,
		bool & ResimExt, // Flag to resimulate the exterior energy use simulation
		bool & ResimHB, // Flag to resimulate the heat balance simulation (including HVAC)
		bool & ResimHVAC // Flag to resimulate the HVAC simulation
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   July 2005
		//       MODIFIED       Simon Vidanovic (March 2015) - Introduced DemandManager:Ventilation
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:

		// METHODOLOGY EMPLOYED:

		// Using/Aliasing
		using ScheduleManager::GetCurrentScheduleValue;
		using DataGlobals::TimeStepZoneSec;
		using DataGlobals::SecInHour;
		using DataHVACGlobals::TimeStepSys;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int MgrNum;
		int MgrPtr;
		Real64 AverageDemand;
		Real64 OverLimit;
		bool OnPeak;

		// FLOW:
		DemandManagerList( ListNum ).ScheduledLimit = GetCurrentScheduleValue( DemandManagerList( ListNum ).LimitSchedule );
		DemandManagerList( ListNum ).DemandLimit = DemandManagerList( ListNum ).ScheduledLimit * DemandManagerList( ListNum ).SafetyFraction;

		DemandManagerList( ListNum ).MeterDemand = GetInstantMeterValue( DemandManagerList( ListNum ).Meter, 1 ) / TimeStepZoneSec + GetInstantMeterValue( DemandManagerList( ListNum ).Meter, 2 ) / ( TimeStepSys * SecInHour );

		// Calculate average demand over the averaging window including the current timestep meter demand
		AverageDemand = DemandManagerList( ListNum ).AverageDemand + ( DemandManagerList( ListNum ).MeterDemand - DemandManagerList( ListNum ).History( 1 ) ) / DemandManagerList( ListNum ).AveragingWindow;

		if ( DemandManagerList( ListNum ).PeakSchedule == 0 ) {
			OnPeak = true;
		} else {
			if ( GetCurrentScheduleValue( DemandManagerList( ListNum ).PeakSchedule ) == 1 ) {
				OnPeak = true;
			} else {
				OnPeak = false;
			}
		}

		if ( OnPeak ) {
			OverLimit = AverageDemand - DemandManagerList( ListNum ).DemandLimit;

			if ( OverLimit > 0.0 ) {

				{ auto const SELECT_CASE_var( DemandManagerList( ListNum ).ManagerPriority );

				if ( SELECT_CASE_var == ManagerPrioritySequential ) { // Activate first Demand Manager that can reduce demand

					for ( MgrNum = 1; MgrNum <= DemandManagerList( ListNum ).NumOfManager; ++MgrNum ) {
						MgrPtr = DemandManagerList( ListNum ).Manager( MgrNum );

						if ( DemandMgr( MgrPtr ).CanReduceDemand ) {
							DemandMgr( MgrPtr ).Activate = true;

							{ auto const SELECT_CASE_var1( DemandMgr( MgrPtr ).Type );
							if ( SELECT_CASE_var1 == ManagerTypeExtLights ) {
								ResimExt = true;

							} else if ( ( SELECT_CASE_var1 == ManagerTypeLights ) || ( SELECT_CASE_var1 == ManagerTypeElecEquip ) ) {
								ResimHB = true;
								ResimHVAC = true;

							} else if ( ( SELECT_CASE_var1 == ManagerTypeThermostats ) || ( SELECT_CASE_var1 == ManagerTypeVentilation ) ) {
								ResimHVAC = true;

							}}

							break; // Leave the loop
						}
					} // MgrNum

				} else if ( SELECT_CASE_var == ManagerPriorityOptimal ) {
					// Not yet implemented

				} else if ( SELECT_CASE_var == ManagerPriorityAll ) { // Activate ALL Demand Managers that can reduce demand

					for ( MgrNum = 1; MgrNum <= DemandManagerList( ListNum ).NumOfManager; ++MgrNum ) {
						MgrPtr = DemandManagerList( ListNum ).Manager( MgrNum );

						if ( DemandMgr( MgrPtr ).CanReduceDemand ) {
							DemandMgr( MgrPtr ).Activate = true;

							{ auto const SELECT_CASE_var1( DemandMgr( MgrPtr ).Type );
							if ( SELECT_CASE_var1 == ManagerTypeExtLights ) {
								ResimExt = true;

							} else if ( ( SELECT_CASE_var1 == ManagerTypeLights ) || ( SELECT_CASE_var1 == ManagerTypeElecEquip ) ) {
								ResimHB = true;
								ResimHVAC = true;

							}
							else if ( ( SELECT_CASE_var1 == ManagerTypeThermostats ) || ( SELECT_CASE_var1 == ManagerTypeVentilation ) ) {
								ResimHVAC = true;

							}}

						}
					} // MgrNum

				}}

			}
		}

	}

	void
	GetDemandManagerListInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   July 2005
		//       MODIFIED       Simon Vidanovic (March 2015) - Introduced DemandManager:Ventilation
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Gets the DEMAND MANAGER LIST input from the input file.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// Using/Aliasing
		using DataGlobals::MinutesPerTimeStep;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::FindItemInList;
		using InputProcessor::GetObjectDefMaxArgs;
		using namespace DataIPShortCuts; // Data for field names, blank numerics
		using ScheduleManager::GetScheduleIndex;
		using OutputProcessor::EnergyMeters;

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ListNum;
		int MgrNum;
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		int IOStat; // IO Status when calling get input subroutine
		Array1D_string AlphArray; // Character string data
		Array1D< Real64 > NumArray; // Numeric data
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		std::string Units; // String for meter units
		static bool ErrorsFound( false );
		std::string CurrentModuleObject; // for ease in renaming.

		// FLOW:
		CurrentModuleObject = "DemandManagerAssignmentList";
		GetObjectDefMaxArgs( CurrentModuleObject, ListNum, NumAlphas, NumNums );

		NumDemandManagerList = GetNumObjectsFound( CurrentModuleObject );

		if ( NumDemandManagerList > 0 ) {
			AlphArray.dimension( NumAlphas, BlankString );
			NumArray.dimension( NumNums, 0.0 );

			DemandManagerList.allocate( NumDemandManagerList );

			for ( ListNum = 1; ListNum <= NumDemandManagerList; ++ListNum ) {

				GetObjectItem( CurrentModuleObject, ListNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

				IsNotOK = false;
				IsBlank = false;
				VerifyName( AlphArray( 1 ), DemandManagerList, ListNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
				}
				DemandManagerList( ListNum ).Name = AlphArray( 1 );

				DemandManagerList( ListNum ).Meter = GetMeterIndex( AlphArray( 2 ) );

				if ( DemandManagerList( ListNum ).Meter == 0 ) {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 2 ) + '=' + AlphArray( 2 ) );
					ShowContinueError( "Entered in " + CurrentModuleObject + '=' + AlphArray( 1 ) );
					ErrorsFound = true;

				} else {
					{ auto const SELECT_CASE_var( EnergyMeters( DemandManagerList( ListNum ).Meter ).ResourceType );
					if ( ( SELECT_CASE_var == "Electricity" ) || ( SELECT_CASE_var == "ElectricityNet" ) ) {
						Units = "[W]"; // For setup of report variables

					} else {
						ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid value " + cAlphaFieldNames( 2 ) + "=\"" + AlphArray( 2 ) + "\"." );
						ShowContinueError( "Only Electricity and ElectricityNet meters are currently allowed." );
						ErrorsFound = true;
					}}
				}

				// Further checking for conflicting DEMAND MANAGER LISTs

				if ( ! lAlphaFieldBlanks( 3 ) ) {
					DemandManagerList( ListNum ).LimitSchedule = GetScheduleIndex( AlphArray( 3 ) );

					if ( DemandManagerList( ListNum ).LimitSchedule == 0 ) {
						ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 3 ) + "=\"" + AlphArray( 3 ) + "\" not found." );
						ErrorsFound = true;
					}
				}

				DemandManagerList( ListNum ).SafetyFraction = NumArray( 1 );

				if ( ! lAlphaFieldBlanks( 4 ) ) {
					DemandManagerList( ListNum ).BillingSchedule = GetScheduleIndex( AlphArray( 4 ) );

					if ( DemandManagerList( ListNum ).BillingSchedule == 0 ) {
						ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 4 ) + "=\"" + AlphArray( 4 ) + "\" not found." );
						ErrorsFound = true;
					}
				}

				if ( ! lAlphaFieldBlanks( 5 ) ) {
					DemandManagerList( ListNum ).PeakSchedule = GetScheduleIndex( AlphArray( 5 ) );

					if ( DemandManagerList( ListNum ).PeakSchedule == 0 ) {
						ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 5 ) + "=\"" + AlphArray( 5 ) + "\" not found." );
						ErrorsFound = true;
					}
				}

				DemandManagerList( ListNum ).AveragingWindow = max( int( NumArray( 2 ) / MinutesPerTimeStep ), 1 );
				// Round to nearest timestep
				// Can make this fancier to include windows that do not fit the timesteps
				DemandManagerList( ListNum ).History.allocate( DemandManagerList( ListNum ).AveragingWindow );
				DemandManagerList( ListNum ).History = 0.0;

				// Validate Demand Manager Priority
				{ auto const SELECT_CASE_var( AlphArray( 6 ) );
				if ( SELECT_CASE_var == "SEQUENTIAL" ) {
					DemandManagerList( ListNum ).ManagerPriority = ManagerPrioritySequential;

				} else if ( SELECT_CASE_var == "OPTIMAL" ) {
					DemandManagerList( ListNum ).ManagerPriority = ManagerPriorityOptimal;

				} else if ( SELECT_CASE_var == "ALL" ) {
					DemandManagerList( ListNum ).ManagerPriority = ManagerPriorityAll;

				} else {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid value " + cAlphaFieldNames( 6 ) + "=\"" + AlphArray( 6 ) + "\" not found." );
					ErrorsFound = true;
				}}

				// Get DEMAND MANAGER Type and Name pairs
				DemandManagerList( ListNum ).NumOfManager = int( ( NumAlphas - 6 ) / 2.0 );

				if ( DemandManagerList( ListNum ).NumOfManager > 0 ) {
					DemandManagerList( ListNum ).Manager.allocate( DemandManagerList( ListNum ).NumOfManager );

					for ( MgrNum = 1; MgrNum <= DemandManagerList( ListNum ).NumOfManager; ++MgrNum ) {

						// Validate DEMAND MANAGER Type
						{ auto const SELECT_CASE_var( AlphArray( MgrNum * 2 + 5 ) );
						if ((SELECT_CASE_var == "DEMANDMANAGER:LIGHTS") || (SELECT_CASE_var == "DEMANDMANAGER:EXTERIORLIGHTS") || (SELECT_CASE_var == "DEMANDMANAGER:ELECTRICEQUIPMENT") || (SELECT_CASE_var == "DEMANDMANAGER:THERMOSTATS") || (SELECT_CASE_var == "DEMANDMANAGER:VENTILATION")) {

							DemandManagerList( ListNum ).Manager( MgrNum ) = FindItemInList( AlphArray( MgrNum * 2 + 6 ), DemandMgr );

							if ( DemandManagerList( ListNum ).Manager( MgrNum ) == 0 ) {
								ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( MgrNum * 2 + 6 ) + "=\"" + AlphArray( MgrNum * 2 + 6 ) + "\" not found." );
								ErrorsFound = true;
							}

						} else {
							ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid value " + cAlphaFieldNames( MgrNum * 2 + 5 ) + "=\"" + AlphArray( MgrNum * 2 + 5 ) + "\"." );
							ErrorsFound = true;
						}}

						// Check that each is not already referenced using %DemandManagerList field

					} // MgrNum
				}

				// Setup report variables
				SetupOutputVariable( "Demand Manager Meter Demand Power [W]", DemandManagerList( ListNum ).MeterDemand, "Zone", "Average", DemandManagerList( ListNum ).Name );

				SetupOutputVariable( "Demand Manager Average Demand Power [W]", DemandManagerList( ListNum ).AverageDemand, "Zone", "Average", DemandManagerList( ListNum ).Name );

				SetupOutputVariable( "Demand Manager Peak Demand Power [W]", DemandManagerList( ListNum ).PeakDemand, "Zone", "Average", DemandManagerList( ListNum ).Name );

				SetupOutputVariable( "Demand Manager Scheduled Limit Power [W]", DemandManagerList( ListNum ).ScheduledLimit, "Zone", "Average", DemandManagerList( ListNum ).Name );

				SetupOutputVariable( "Demand Manager Demand Limit Power [W]", DemandManagerList( ListNum ).DemandLimit, "Zone", "Average", DemandManagerList( ListNum ).Name );

				SetupOutputVariable( "Demand Manager Over Limit Power [W]", DemandManagerList( ListNum ).OverLimit, "Zone", "Average", DemandManagerList( ListNum ).Name );

				SetupOutputVariable( "Demand Manager Over Limit Time [hr]", DemandManagerList( ListNum ).OverLimitDuration, "Zone", "Sum", DemandManagerList( ListNum ).Name );

				if ( ErrorsFound ) {
					ShowFatalError( "Errors found in processing input for " + CurrentModuleObject );
				}

			} // ListNum

			AlphArray.deallocate();
			NumArray.deallocate();

			// Iteration diagnostic reporting for all DEMAND MANAGER LISTs
			SetupOutputVariable( "Demand Manager Exterior Energy Iteration Count []", DemandManagerExtIterations, "Zone", "Sum", "ManageDemand" );

			SetupOutputVariable( "Demand Manager Heat Balance Iteration Count []", DemandManagerHBIterations, "Zone", "Sum", "ManageDemand" );

			SetupOutputVariable( "Demand Manager HVAC Iteration Count []", DemandManagerHVACIterations, "Zone", "Sum", "ManageDemand" );

		}

	}

	void
	GetDemandManagerInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   July 2005
		//       MODIFIED       MODIFIED       Simon Vidanovic (March 2015) - Introduced DemandManager:Ventilation
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Gets the DEMAND MANAGER input from the input file.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// Using/Aliasing
		using DataGlobals::MinutesPerTimeStep;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::FindItemInList;
		using InputProcessor::GetObjectDefMaxArgs;
		using namespace DataIPShortCuts; // Data for field names, blank numerics
		using ScheduleManager::GetScheduleIndex;
		using DataHeatBalance::Lights;
		using DataHeatBalance::ZoneElectric;
		using DataHeatBalance::LightsObjects;
		using DataHeatBalance::ZoneElectricObjects;
		using ExteriorEnergyUse::ExteriorLights;
		using DataZoneControls::TempControlledZone;
		using DataZoneControls::TStatObjects;
		using General::RoundSigDigits;
		using MixedAir::GetOAController;

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumDemandMgrExtLights;
		int NumDemandMgrLights;
		int NumDemandMgrElecEquip;
		int NumDemandMgrThermostats;
		int NumDemandMgrVentilation;
		int MgrNum;
		int StartIndex;
		int EndIndex;
		int LoadNum;
		int LoadPtr;
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		int MaxAlphas; // Max number of elements in the alpha array
		int MaxNums; // Max number of elements in the numeric array
		int NumParams; // Number of arguments total in an ObjectDef
		int IOStat; // IO Status when calling get input subroutine
		Array1D_string AlphArray; // Character string data
		Array1D< Real64 > NumArray; // Numeric data
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		static bool ErrorsFound( false );
		std::string CurrentModuleObject; // for ease in renaming.
		int Item;
		int Item1;

		// FLOW:
		MaxAlphas = 0;
		MaxNums = 0;
		CurrentModuleObject = "DemandManager:ExteriorLights";
		NumDemandMgrExtLights = GetNumObjectsFound( CurrentModuleObject );
		if ( NumDemandMgrExtLights > 0 ) {
			GetObjectDefMaxArgs( CurrentModuleObject, NumParams, NumAlphas, NumNums );
			MaxAlphas = max( MaxAlphas, NumAlphas );
			MaxNums = max( MaxNums, NumNums );
		}
		CurrentModuleObject = "DemandManager:Lights";
		NumDemandMgrLights = GetNumObjectsFound( CurrentModuleObject );
		if ( NumDemandMgrLights > 0 ) {
			GetObjectDefMaxArgs( CurrentModuleObject, NumParams, NumAlphas, NumNums );
			MaxAlphas = max( MaxAlphas, NumAlphas );
			MaxNums = max( MaxNums, NumNums );
		}
		CurrentModuleObject = "DemandManager:ElectricEquipment";
		NumDemandMgrElecEquip = GetNumObjectsFound( CurrentModuleObject );
		if ( NumDemandMgrElecEquip > 0 ) {
			GetObjectDefMaxArgs( CurrentModuleObject, NumParams, NumAlphas, NumNums );
			MaxAlphas = max( MaxAlphas, NumAlphas );
			MaxNums = max( MaxNums, NumNums );
		}
		CurrentModuleObject = "DemandManager:Thermostats";
		NumDemandMgrThermostats = GetNumObjectsFound( CurrentModuleObject );
		if ( NumDemandMgrThermostats > 0 ) {
			GetObjectDefMaxArgs( CurrentModuleObject, NumParams, NumAlphas, NumNums );
			MaxAlphas = max( MaxAlphas, NumAlphas );
			MaxNums = max( MaxNums, NumNums );
		}
		CurrentModuleObject = "DemandManager:Ventilation";
		NumDemandMgrVentilation = GetNumObjectsFound( CurrentModuleObject );
		if ( NumDemandMgrVentilation > 0 ) {
			GetObjectDefMaxArgs( CurrentModuleObject, NumParams, NumAlphas, NumNums );
			MaxAlphas = max( MaxAlphas, NumAlphas );
			MaxNums = max( MaxNums, NumNums );
		}

		NumDemandMgr = NumDemandMgrExtLights + NumDemandMgrLights + NumDemandMgrElecEquip + NumDemandMgrThermostats + NumDemandMgrVentilation;

		if ( NumDemandMgr > 0 ) {
			AlphArray.dimension( MaxAlphas, BlankString );
			NumArray.dimension( MaxNums, 0.0 );

			DemandMgr.allocate( NumDemandMgr );

			// Get input for DemandManager:ExteriorLights
			StartIndex = 1;
			EndIndex = NumDemandMgrExtLights;

			CurrentModuleObject = "DemandManager:ExteriorLights";

			for ( MgrNum = StartIndex; MgrNum <= EndIndex; ++MgrNum ) {

				GetObjectItem( CurrentModuleObject, MgrNum - StartIndex + 1, AlphArray, NumAlphas, NumArray, NumNums, IOStat, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

				IsNotOK = false;
				IsBlank = false;
				VerifyName( AlphArray( 1 ), DemandMgr, MgrNum - StartIndex, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
				}
				DemandMgr( MgrNum ).Name = AlphArray( 1 );

				DemandMgr( MgrNum ).Type = ManagerTypeExtLights;

				if ( ! lAlphaFieldBlanks( 2 ) ) {
					DemandMgr( MgrNum ).AvailSchedule = GetScheduleIndex( AlphArray( 2 ) );

					if ( DemandMgr( MgrNum ).AvailSchedule == 0 ) {
						ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 2 ) + "=\"" + AlphArray( 2 ) + "\" not found." );
						ErrorsFound = true;
					}
				} else {
					DemandMgr( MgrNum ).AvailSchedule = ScheduleAlwaysOn;
				}

				// Validate Limiting Control
				{ auto const SELECT_CASE_var( AlphArray( 3 ) );
				if ( SELECT_CASE_var == "OFF" ) {
					DemandMgr( MgrNum ).LimitControl = ManagerLimitOff;

				} else if ( SELECT_CASE_var == "FIXED" ) {
					DemandMgr( MgrNum ).LimitControl = ManagerLimitFixed;

				} else if ( SELECT_CASE_var == "VARIABLE" ) {
					DemandMgr( MgrNum ).LimitControl = ManagerLimitVariable;

				} else {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid value" + cAlphaFieldNames( 3 ) + "=\"" + AlphArray( 3 ) + "\"." );
					ShowContinueError( "...value must be one of Off, Fixed, or Variable." );
					ErrorsFound = true;
				}}

				if ( NumArray( 1 ) == 0.0 )
					DemandMgr( MgrNum ).LimitDuration = MinutesPerTimeStep;
				else
					DemandMgr( MgrNum ).LimitDuration = NumArray( 1 );

				DemandMgr( MgrNum ).LowerLimit = NumArray( 2 );

				// Validate Selection Control
				{ auto const SELECT_CASE_var( AlphArray( 4 ) );
				if ( SELECT_CASE_var == "ALL" ) {
					DemandMgr( MgrNum ).SelectionControl = ManagerSelectionAll;

				} else if ( SELECT_CASE_var == "ROTATEONE" ) {
					DemandMgr( MgrNum ).SelectionControl = ManagerSelectionOne;

				} else if ( SELECT_CASE_var == "ROTATEMANY" ) {
					DemandMgr( MgrNum ).SelectionControl = ManagerSelectionMany;

				} else {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid value" + cAlphaFieldNames( 4 ) + "=\"" + AlphArray( 4 ) + "\"." );
					ShowContinueError( "...value must be one of All, RotateOne, or RotateMany." );
					ErrorsFound = true;
				}}

				if ( NumArray( 4 ) == 0.0 )
					DemandMgr( MgrNum ).RotationDuration = MinutesPerTimeStep;
				else
					DemandMgr( MgrNum ).RotationDuration = NumArray( 4 );

				DemandMgr( MgrNum ).NumOfLoads = NumAlphas - 4;

				if ( DemandMgr( MgrNum ).NumOfLoads > 0 ) {
					DemandMgr( MgrNum ).Load.allocate( DemandMgr( MgrNum ).NumOfLoads );

					for ( LoadNum = 1; LoadNum <= DemandMgr( MgrNum ).NumOfLoads; ++LoadNum ) {
						LoadPtr = FindItemInList( AlphArray( LoadNum + 4 ), ExteriorLights );

						if ( LoadPtr > 0 ) {
							DemandMgr( MgrNum ).Load( LoadNum ) = LoadPtr;

						} else {
							ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( LoadNum + 4 ) + "=\"" + AlphArray( LoadNum + 4 ) + "\" not found." );
							ErrorsFound = true;

						}
					} // LoadNum
				}
				else
				{
					ShowSevereError(CurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid value for number of loads.");
					ShowContinueError("Number of loads is calculated to be less than one. Demand manager must have at least one load assigned.");
					ErrorsFound = true;
				}

			} // MgrNum

			// Get input for DemandManager:Lights
			StartIndex = EndIndex + 1;
			EndIndex += NumDemandMgrLights;

			CurrentModuleObject = "DemandManager:Lights";

			for ( MgrNum = StartIndex; MgrNum <= EndIndex; ++MgrNum ) {

				GetObjectItem( CurrentModuleObject, MgrNum - StartIndex + 1, AlphArray, NumAlphas, NumArray, NumNums, IOStat, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

				IsNotOK = false;
				IsBlank = false;
				VerifyName( AlphArray( 1 ), DemandMgr, MgrNum - StartIndex, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
				}
				DemandMgr( MgrNum ).Name = AlphArray( 1 );

				DemandMgr( MgrNum ).Type = ManagerTypeLights;

				if ( ! lAlphaFieldBlanks( 2 ) ) {
					DemandMgr( MgrNum ).AvailSchedule = GetScheduleIndex( AlphArray( 2 ) );

					if ( DemandMgr( MgrNum ).AvailSchedule == 0 ) {
						ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 2 ) + "=\"" + AlphArray( 2 ) + "\" not found." );
						ErrorsFound = true;
					}
				} else {
					DemandMgr( MgrNum ).AvailSchedule = ScheduleAlwaysOn;
				}

				// Validate Limiting Control
				{ auto const SELECT_CASE_var( AlphArray( 3 ) );
				if ( SELECT_CASE_var == "OFF" ) {
					DemandMgr( MgrNum ).LimitControl = ManagerLimitOff;

				} else if ( SELECT_CASE_var == "FIXED" ) {
					DemandMgr( MgrNum ).LimitControl = ManagerLimitFixed;

				} else if ( SELECT_CASE_var == "VARIABLE" ) {
					DemandMgr( MgrNum ).LimitControl = ManagerLimitVariable;

				} else {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid value" + cAlphaFieldNames( 3 ) + "=\"" + AlphArray( 3 ) + "\"." );
					ShowContinueError( "...value must be one of Off, Fixed, or Variable." );
					ErrorsFound = true;
				}}

				if ( NumArray( 1 ) == 0.0 )
					DemandMgr( MgrNum ).LimitDuration = MinutesPerTimeStep;
				else
					DemandMgr( MgrNum ).LimitDuration = NumArray( 1 );

				DemandMgr( MgrNum ).LowerLimit = NumArray( 2 );

				// Validate Selection Control
				{ auto const SELECT_CASE_var( AlphArray( 4 ) );
				if ( SELECT_CASE_var == "ALL" ) {
					DemandMgr( MgrNum ).SelectionControl = ManagerSelectionAll;

				} else if ( SELECT_CASE_var == "ROTATEONE" ) {
					DemandMgr( MgrNum ).SelectionControl = ManagerSelectionOne;

				} else if ( SELECT_CASE_var == "ROTATEMANY" ) {
					DemandMgr( MgrNum ).SelectionControl = ManagerSelectionMany;

				} else {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid value" + cAlphaFieldNames( 4 ) + "=\"" + AlphArray( 4 ) + "\"." );
					ShowContinueError( "...value must be one of All, RotateOne, or RotateMany." );
					ErrorsFound = true;
				}}

				if ( NumArray( 4 ) == 0.0 )
					DemandMgr( MgrNum ).RotationDuration = MinutesPerTimeStep;
				else
					DemandMgr( MgrNum ).RotationDuration = NumArray( 4 );

				// Count actual pointers to controlled zones
				DemandMgr( MgrNum ).NumOfLoads = 0;
				for ( LoadNum = 1; LoadNum <= NumAlphas - 4; ++LoadNum ) {
					LoadPtr = FindItemInList( AlphArray( LoadNum + 4 ), LightsObjects );
					if ( LoadPtr > 0 ) {
						DemandMgr( MgrNum ).NumOfLoads += LightsObjects( LoadPtr ).NumOfZones;
					} else {
						LoadPtr = FindItemInList( AlphArray( LoadNum + 4 ), Lights );
						if ( LoadPtr > 0 ) {
							++DemandMgr( MgrNum ).NumOfLoads;
						} else {
							ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( LoadNum + 4 ) + "=\"" + AlphArray( LoadNum + 4 ) + "\" not found." );
							ErrorsFound = true;
						}
					}
				}

				//      DemandMgr(MgrNum)%NumOfLoads = NumAlphas - 4

				if ( DemandMgr( MgrNum ).NumOfLoads > 0 ) {
					DemandMgr( MgrNum ).Load.allocate( DemandMgr( MgrNum ).NumOfLoads );
					LoadNum = 0;
					for ( Item = 1; Item <= NumAlphas - 4; ++Item ) {
						LoadPtr = FindItemInList( AlphArray( Item + 4 ), LightsObjects );
						if ( LoadPtr > 0 ) {
							for ( Item1 = 1; Item1 <= LightsObjects( LoadPtr ).NumOfZones; ++Item1 ) {
								++LoadNum;
								DemandMgr( MgrNum ).Load( LoadNum ) = LightsObjects( LoadPtr ).StartPtr + Item1 - 1;
							}
						} else {
							LoadPtr = FindItemInList( AlphArray( Item + 4 ), Lights );
							if ( LoadPtr > 0 ) {
								++LoadNum;
								DemandMgr( MgrNum ).Load( LoadNum ) = LoadPtr;
							}
						}
					} // LoadNum
				}
				else
				{
					ShowSevereError(CurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid value for number of loads.");
					ShowContinueError("Number of loads is calculated to be less than one. Demand manager must have at least one load assigned.");
					ErrorsFound = true;
				}

			} // MgrNum

			// Get input for DemandManager:ElectricEquipment
			StartIndex = EndIndex + 1;
			EndIndex += NumDemandMgrElecEquip;

			CurrentModuleObject = "DemandManager:ElectricEquipment";

			for ( MgrNum = StartIndex; MgrNum <= EndIndex; ++MgrNum ) {

				GetObjectItem( CurrentModuleObject, MgrNum - StartIndex + 1, AlphArray, NumAlphas, NumArray, NumNums, IOStat, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

				IsNotOK = false;
				IsBlank = false;
				VerifyName( AlphArray( 1 ), DemandMgr, MgrNum - StartIndex, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
				}
				DemandMgr( MgrNum ).Name = AlphArray( 1 );

				DemandMgr( MgrNum ).Type = ManagerTypeElecEquip;

				if ( ! lAlphaFieldBlanks( 2 ) ) {
					DemandMgr( MgrNum ).AvailSchedule = GetScheduleIndex( AlphArray( 2 ) );

					if ( DemandMgr( MgrNum ).AvailSchedule == 0 ) {
						ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 2 ) + "=\"" + AlphArray( 2 ) + "\" not found." );
						ErrorsFound = true;
					}
				} else {
					DemandMgr( MgrNum ).AvailSchedule = ScheduleAlwaysOn;
				}

				// Validate Limiting Control
				{ auto const SELECT_CASE_var( AlphArray( 3 ) );
				if ( SELECT_CASE_var == "OFF" ) {
					DemandMgr( MgrNum ).LimitControl = ManagerLimitOff;

				} else if ( SELECT_CASE_var == "FIXED" ) {
					DemandMgr( MgrNum ).LimitControl = ManagerLimitFixed;

				} else if ( SELECT_CASE_var == "VARIABLE" ) {
					DemandMgr( MgrNum ).LimitControl = ManagerLimitVariable;

				} else {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid value" + cAlphaFieldNames( 3 ) + "=\"" + AlphArray( 3 ) + "\"." );
					ShowContinueError( "...value must be one of Off, Fixed, or Variable." );
					ErrorsFound = true;
				}}

				if ( NumArray( 1 ) == 0.0 )
					DemandMgr( MgrNum ).LimitDuration = MinutesPerTimeStep;
				else
					DemandMgr( MgrNum ).LimitDuration = NumArray( 1 );

				DemandMgr( MgrNum ).LowerLimit = NumArray( 2 );

				// Validate Selection Control
				{ auto const SELECT_CASE_var( AlphArray( 4 ) );
				if ( SELECT_CASE_var == "ALL" ) {
					DemandMgr( MgrNum ).SelectionControl = ManagerSelectionAll;

				} else if ( SELECT_CASE_var == "ROTATEONE" ) {
					DemandMgr( MgrNum ).SelectionControl = ManagerSelectionOne;

				} else if ( SELECT_CASE_var == "ROTATEMANY" ) {
					DemandMgr( MgrNum ).SelectionControl = ManagerSelectionMany;

				} else {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid value" + cAlphaFieldNames( 4 ) + "=\"" + AlphArray( 4 ) + "\"." );
					ShowContinueError( "...value must be one of All, RotateOne, or RotateMany." );
					ErrorsFound = true;
				}}

				if ( NumArray( 4 ) == 0.0 )
					DemandMgr( MgrNum ).RotationDuration = MinutesPerTimeStep;
				else
					DemandMgr( MgrNum ).RotationDuration = NumArray( 4 );

				// Count actual pointers to controlled zones
				DemandMgr( MgrNum ).NumOfLoads = 0;
				for ( LoadNum = 1; LoadNum <= NumAlphas - 4; ++LoadNum ) {
					LoadPtr = FindItemInList( AlphArray( LoadNum + 4 ), ZoneElectricObjects );
					if ( LoadPtr > 0 ) {
						DemandMgr( MgrNum ).NumOfLoads += ZoneElectricObjects( LoadPtr ).NumOfZones;
					} else {
						LoadPtr = FindItemInList( AlphArray( LoadNum + 4 ), ZoneElectric );
						if ( LoadPtr > 0 ) {
							++DemandMgr( MgrNum ).NumOfLoads;
						} else {
							ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( LoadNum + 4 ) + "=\"" + AlphArray( LoadNum + 4 ) + "\" not found." );
							ErrorsFound = true;
						}
					}
				}

				//      DemandMgr(MgrNum)%NumOfLoads = NumAlphas - 4

				if ( DemandMgr( MgrNum ).NumOfLoads > 0 ) {
					DemandMgr( MgrNum ).Load.allocate( DemandMgr( MgrNum ).NumOfLoads );
					LoadNum = 0;
					for ( Item = 1; Item <= NumAlphas - 4; ++Item ) {
						LoadPtr = FindItemInList( AlphArray( Item + 4 ), ZoneElectricObjects );
						if ( LoadPtr > 0 ) {
							for ( Item1 = 1; Item1 <= ZoneElectricObjects( LoadPtr ).NumOfZones; ++Item1 ) {
								++LoadNum;
								DemandMgr( MgrNum ).Load( LoadNum ) = ZoneElectricObjects( LoadPtr ).StartPtr + Item1 - 1;
							}
						} else {
							LoadPtr = FindItemInList( AlphArray( Item + 4 ), ZoneElectric );
							if ( LoadPtr > 0 ) {
								++LoadNum;
								DemandMgr( MgrNum ).Load( LoadNum ) = LoadPtr;
							}
						}
					} // LoadNum
				}
				else
				{
					ShowSevereError(CurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid value for number of loads.");
					ShowContinueError("Number of loads is calculated to be less than one. Demand manager must have at least one load assigned.");
					ErrorsFound = true;
				}

			} // MgrNum

			// Get input for DemandManager:Thermostats
			StartIndex = EndIndex + 1;
			EndIndex += NumDemandMgrThermostats;

			CurrentModuleObject = "DemandManager:Thermostats";

			for ( MgrNum = StartIndex; MgrNum <= EndIndex; ++MgrNum ) {

				GetObjectItem( CurrentModuleObject, MgrNum - StartIndex + 1, AlphArray, NumAlphas, NumArray, NumNums, IOStat, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

				IsNotOK = false;
				IsBlank = false;
				VerifyName( AlphArray( 1 ), DemandMgr, MgrNum - StartIndex, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
				}
				DemandMgr( MgrNum ).Name = AlphArray( 1 );

				DemandMgr( MgrNum ).Type = ManagerTypeThermostats;

				if ( ! lAlphaFieldBlanks( 2 ) ) {
					DemandMgr( MgrNum ).AvailSchedule = GetScheduleIndex( AlphArray( 2 ) );

					if ( DemandMgr( MgrNum ).AvailSchedule == 0 ) {
						ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 2 ) + "=\"" + AlphArray( 2 ) + "\" not found." );
						ErrorsFound = true;
					}
				} else {
					DemandMgr( MgrNum ).AvailSchedule = ScheduleAlwaysOn;
				}

				// Validate Limiting Control
				{ auto const SELECT_CASE_var( AlphArray( 3 ) );
				if ( SELECT_CASE_var == "OFF" ) {
					DemandMgr( MgrNum ).LimitControl = ManagerLimitOff;

				} else if ( SELECT_CASE_var == "FIXED" ) {
					DemandMgr( MgrNum ).LimitControl = ManagerLimitFixed;

				} else if ( SELECT_CASE_var == "VARIABLE" ) {
					DemandMgr( MgrNum ).LimitControl = ManagerLimitVariable;

				} else {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid value" + cAlphaFieldNames( 3 ) + "=\"" + AlphArray( 3 ) + "\"." );
					ShowContinueError( "...value must be one of Off, Fixed, or Variable." );
					ErrorsFound = true;
				}}

				if ( NumArray( 1 ) == 0.0 )
					DemandMgr( MgrNum ).LimitDuration = MinutesPerTimeStep;
				else
					DemandMgr( MgrNum ).LimitDuration = NumArray( 1 );

				DemandMgr( MgrNum ).LowerLimit = NumArray( 2 );
				DemandMgr( MgrNum ).UpperLimit = NumArray( 3 );

				if ( DemandMgr( MgrNum ).LowerLimit > DemandMgr( MgrNum ).UpperLimit ) {
					ShowSevereError( "Invalid input for " + CurrentModuleObject + " = " + AlphArray( 1 ) );
					ShowContinueError( cNumericFieldNames( 2 ) + " [" + RoundSigDigits( NumArray( 2 ), 2 ) + "] > " + cNumericFieldNames( 3 ) + " [" + RoundSigDigits( NumArray( 3 ), 2 ) + ']' );
					ShowContinueError( cNumericFieldNames( 2 ) + " cannot be greater than " + cNumericFieldNames( 3 ) );
					ErrorsFound = true;
				}

				// Validate Selection Control
				{ auto const SELECT_CASE_var( AlphArray( 4 ) );
				if ( SELECT_CASE_var == "ALL" ) {
					DemandMgr( MgrNum ).SelectionControl = ManagerSelectionAll;

				} else if ( SELECT_CASE_var == "ROTATEONE" ) {
					DemandMgr( MgrNum ).SelectionControl = ManagerSelectionOne;

				} else if ( SELECT_CASE_var == "ROTATEMANY" ) {
					DemandMgr( MgrNum ).SelectionControl = ManagerSelectionMany;

				} else {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid value" + cAlphaFieldNames( 4 ) + "=\"" + AlphArray( 4 ) + "\"." );
					ShowContinueError( "...value must be one of All, RotateOne, or RotateMany." );
					ErrorsFound = true;
				}}

				if ( NumArray( 5 ) == 0.0 )
					DemandMgr( MgrNum ).RotationDuration = MinutesPerTimeStep;
				else
					DemandMgr( MgrNum ).RotationDuration = NumArray( 5 );

				// Count actual pointers to controlled zones
				DemandMgr( MgrNum ).NumOfLoads = 0;
				for ( LoadNum = 1; LoadNum <= NumAlphas - 4; ++LoadNum ) {
					LoadPtr = FindItemInList( AlphArray( LoadNum + 4 ), TStatObjects );
					if ( LoadPtr > 0 ) {
						DemandMgr( MgrNum ).NumOfLoads += TStatObjects( LoadPtr ).NumOfZones;
					} else {
						LoadPtr = FindItemInList( AlphArray( LoadNum + 4 ), TempControlledZone );
						if ( LoadPtr > 0 ) {
							++DemandMgr( MgrNum ).NumOfLoads;
						} else {
							ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( LoadNum + 4 ) + "=\"" + AlphArray( LoadNum + 4 ) + "\" not found." );
							ErrorsFound = true;
						}
					}
				}

				if ( DemandMgr( MgrNum ).NumOfLoads > 0 ) {
					DemandMgr( MgrNum ).Load.allocate( DemandMgr( MgrNum ).NumOfLoads );
					LoadNum = 0;
					for ( Item = 1; Item <= NumAlphas - 4; ++Item ) {
						LoadPtr = FindItemInList( AlphArray( Item + 4 ), TStatObjects );
						if ( LoadPtr > 0 ) {
							for ( Item1 = 1; Item1 <= TStatObjects( LoadPtr ).NumOfZones; ++Item1 ) {
								++LoadNum;
								DemandMgr( MgrNum ).Load( LoadNum ) = TStatObjects( LoadPtr ).TempControlledZoneStartPtr + Item1 - 1;
							}
						} else {
							LoadPtr = FindItemInList( AlphArray( Item + 4 ), TempControlledZone );
							if ( LoadPtr > 0 ) {
								++LoadNum;
								DemandMgr( MgrNum ).Load( LoadNum ) = LoadPtr;
							}
						}
					} // LoadNum
				}
				else
				{
					ShowSevereError(CurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid value for number of loads.");
					ShowContinueError("Number of loads is calculated to be less than one. Demand manager must have at least one load assigned.");
					ErrorsFound = true;
				}

			} // MgrNum

			// Get input for DemandManager:Ventilation
			StartIndex = EndIndex + 1;
			EndIndex += NumDemandMgrVentilation;

			CurrentModuleObject = "DemandManager:Ventilation";

			for ( MgrNum = StartIndex; MgrNum <= EndIndex; ++MgrNum ) {

				GetObjectItem( CurrentModuleObject, MgrNum - StartIndex + 1, AlphArray, NumAlphas, NumArray, NumNums, IOStat, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

				IsNotOK = false;
				IsBlank = false;
				VerifyName( AlphArray( 1 ), DemandMgr, MgrNum - StartIndex, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
				}
				DemandMgr( MgrNum ).Name = AlphArray( 1 );

				DemandMgr( MgrNum ).Type = ManagerTypeVentilation;

				if ( !lAlphaFieldBlanks( 2 ) ) {
					DemandMgr( MgrNum ).AvailSchedule = GetScheduleIndex( AlphArray( 2 ) );

					if ( DemandMgr( MgrNum ).AvailSchedule == 0 ) {
						ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 2 ) + "=\"" + AlphArray( 2 ) + "\" not found." );
						ErrorsFound = true;
					}
				}
				else {
					DemandMgr( MgrNum ).AvailSchedule = ScheduleAlwaysOn;
				}

				// Validate Limiting Control
				{ auto const SELECT_CASE_var( AlphArray( 3 ) );
				if ( SELECT_CASE_var == "OFF" ) {
					DemandMgr( MgrNum ).LimitControl = ManagerLimitOff;

				}
				else if ( SELECT_CASE_var == "FIXEDRATE" ) {
					DemandMgr( MgrNum ).LimitControl = ManagerLimitFixed;

				}
				else if ( SELECT_CASE_var == "REDUCTIONRATIO" ) {
					DemandMgr( MgrNum ).LimitControl = ManagerLimitReductionRatio;

				}
				else {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid value" + cAlphaFieldNames( 3 ) + "=\"" + AlphArray( 3 ) + "\"." );
					ShowContinueError( "...value must be one of Off, FixedRate, or ReductionRatio." );
					ErrorsFound = true;
				}}

				if ( NumArray( 1 ) == 0.0 )
					DemandMgr( MgrNum ).LimitDuration = MinutesPerTimeStep;
				else
					DemandMgr( MgrNum ).LimitDuration = NumArray( 1 );

				if ( DemandMgr( MgrNum ).LimitControl == ManagerLimitFixed )
					DemandMgr( MgrNum ).FixedRate = NumArray( 2 );
				if ( DemandMgr( MgrNum ).LimitControl == ManagerLimitReductionRatio )
					DemandMgr( MgrNum ).ReductionRatio = NumArray( 3 );

				DemandMgr( MgrNum ).LowerLimit = NumArray( 4 );

				// Validate Selection Control
				{ auto const SELECT_CASE_var( AlphArray( 4 ) );
				if ( SELECT_CASE_var == "ALL" ) {
					DemandMgr( MgrNum ).SelectionControl = ManagerSelectionAll;

				}
				else if ( SELECT_CASE_var == "ROTATEONE" ) {
					DemandMgr( MgrNum ).SelectionControl = ManagerSelectionOne;

				}
				else if ( SELECT_CASE_var == "ROTATEMANY" ) {
					DemandMgr( MgrNum ).SelectionControl = ManagerSelectionMany;

				}
				else {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid value" + cAlphaFieldNames( 4 ) + "=\"" + AlphArray( 4 ) + "\"." );
					ShowContinueError( "...value must be one of All, RotateOne, or RotateMany." );
					ErrorsFound = true;
				}}

				if ( NumArray( 5 ) == 0.0 )
					DemandMgr( MgrNum ).RotationDuration = MinutesPerTimeStep;
				else
					DemandMgr( MgrNum ).RotationDuration = NumArray( 5 );

				// Count number of string fields for loading Controller:OutdoorAir names. This number must be increased in case if
				// new string field is added or decreased if string fields are removed.
				int AlphaShift = 4;

				// Count actual pointers to air controllers
				DemandMgr(MgrNum).NumOfLoads = 0;
				for ( LoadNum = 1; LoadNum <= NumAlphas - AlphaShift; ++LoadNum ) {
					LoadPtr = GetOAController( AlphArray( LoadNum + AlphaShift ) );
					if ( LoadPtr > 0 ) {
						++DemandMgr( MgrNum ).NumOfLoads;
					}
					else {
						ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( LoadNum + AlphaShift ) + "=\"" + AlphArray( LoadNum + AlphaShift ) + "\" not found." );
						ErrorsFound = true;
					}
				}

				if ( DemandMgr( MgrNum ).NumOfLoads > 0 ) {
					DemandMgr( MgrNum ).Load.allocate( DemandMgr( MgrNum ).NumOfLoads );
					for ( LoadNum = 1; LoadNum <= NumAlphas - AlphaShift; ++LoadNum ) {
						LoadPtr = GetOAController( AlphArray( LoadNum + AlphaShift ) );
						if ( LoadPtr > 0 ) {
							DemandMgr( MgrNum ).Load( LoadNum ) = LoadPtr;
						}
					}
				}
				else
				{
					ShowSevereError(CurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid value for number of loads.");
					ShowContinueError("Number of loads is calculated to be less than one. Demand manager must have at least one load assigned.");
					ErrorsFound = true;
				}
			} // MgrNum

			AlphArray.deallocate();
			NumArray.deallocate();

		}

		if ( ErrorsFound ) {
			ShowFatalError( "Errors found in processing input for demand managers. Preceding condition causes termination." );
		}

	}

	void
	SurveyDemandManagers()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   July 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Checks to see if any demand managers can reduce the load

		// METHODOLOGY EMPLOYED:

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int MgrNum;
		int LoadNum;
		int LoadPtr;
		bool CanReduceDemand;

		// FLOW:
		for ( MgrNum = 1; MgrNum <= NumDemandMgr; ++MgrNum ) {

			DemandMgr( MgrNum ).CanReduceDemand = false;

			if ( ! DemandMgr( MgrNum ).Available ) continue;
			if ( DemandMgr( MgrNum ).LimitControl == ManagerLimitOff ) continue;

			if ( DemandMgr( MgrNum ).Active ) continue; // This works for FIXED control action, but not VARIABLE
			// VARIABLE control could actually reduce demand farther, even if active already

			for ( LoadNum = 1; LoadNum <= DemandMgr( MgrNum ).NumOfLoads; ++LoadNum ) {
				LoadPtr = DemandMgr( MgrNum ).Load( LoadNum );

				// Check if this load can reduce demand
				// Assume FIXED control action for now, needs more sophisticated check for VARIABLE control
				LoadInterface( CheckCanReduce, MgrNum, LoadPtr, CanReduceDemand );

				if ( CanReduceDemand ) {
					DemandMgr( MgrNum ).CanReduceDemand = true;
					break; // If any one load can reduce demand, then the whole demand manager can reduce demand
				}

			} // LoadNum

		} // MgrNum

	}

	void
	ActivateDemandManagers()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   July 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:

		// METHODOLOGY EMPLOYED:

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int MgrNum;
		int LoadNum;
		int LoadPtr;
		int RotatedLoadNum;
		bool CanReduceDemand;

		// FLOW:
		for ( MgrNum = 1; MgrNum <= NumDemandMgr; ++MgrNum ) {

			if ( DemandMgr( MgrNum ).Activate ) {
				DemandMgr( MgrNum ).Activate = false;
				DemandMgr( MgrNum ).Active = true;

				{ auto const SELECT_CASE_var( DemandMgr( MgrNum ).SelectionControl );

				if ( SELECT_CASE_var == ManagerSelectionAll ) {
					// Turn ON limiting on all loads
					for ( LoadNum = 1; LoadNum <= DemandMgr( MgrNum ).NumOfLoads; ++LoadNum ) {
						LoadPtr = DemandMgr( MgrNum ).Load( LoadNum );
						LoadInterface( SetLimit, MgrNum, LoadPtr, CanReduceDemand );
					} // LoadNum

				} else if ( SELECT_CASE_var == ManagerSelectionMany ) { // All loads are limited except for one
					if ( DemandMgr( MgrNum ).NumOfLoads > 1 ) {

						// Turn ON limiting on all loads
						for ( LoadNum = 1; LoadNum <= DemandMgr( MgrNum ).NumOfLoads; ++LoadNum ) {
							LoadPtr = DemandMgr( MgrNum ).Load( LoadNum );
							LoadInterface( SetLimit, MgrNum, LoadPtr, CanReduceDemand );
						} // LoadNum

						// Set next rotated load (from last time it was active)
						RotatedLoadNum = DemandMgr( MgrNum ).RotatedLoadNum;
						++RotatedLoadNum;
						if ( RotatedLoadNum > DemandMgr( MgrNum ).NumOfLoads ) RotatedLoadNum = 1;
						DemandMgr( MgrNum ).RotatedLoadNum = RotatedLoadNum;

						// Turn OFF limiting for the new rotated load
						LoadPtr = DemandMgr( MgrNum ).Load( RotatedLoadNum );
						LoadInterface( ClearLimit, MgrNum, LoadPtr, CanReduceDemand );
					} else {
						// Turn ON limiting for the one and only load
						LoadPtr = DemandMgr( MgrNum ).Load( 1 );
						LoadInterface( SetLimit, MgrNum, LoadPtr, CanReduceDemand );
					}

				} else if ( SELECT_CASE_var == ManagerSelectionOne ) { // Only one load is limited
					if ( DemandMgr( MgrNum ).NumOfLoads > 1 ) {
						// Turn OFF limiting on all loads
						for ( LoadNum = 1; LoadNum <= DemandMgr( MgrNum ).NumOfLoads; ++LoadNum ) {
							LoadPtr = DemandMgr( MgrNum ).Load( LoadNum );
							LoadInterface( ClearLimit, MgrNum, LoadPtr, CanReduceDemand );
						} // LoadNum

						// Set next rotated load (from last time it was active)
						RotatedLoadNum = DemandMgr( MgrNum ).RotatedLoadNum;
						++RotatedLoadNum;
						if ( RotatedLoadNum > DemandMgr( MgrNum ).NumOfLoads ) RotatedLoadNum = 1;
						DemandMgr( MgrNum ).RotatedLoadNum = RotatedLoadNum;

						// Turn ON limiting for the new rotated load
						LoadPtr = DemandMgr( MgrNum ).Load( RotatedLoadNum );
						LoadInterface( SetLimit, MgrNum, LoadPtr, CanReduceDemand );
					} else {
						// Turn ON limiting for the one and only load
						LoadPtr = DemandMgr( MgrNum ).Load( 1 );
						LoadInterface( SetLimit, MgrNum, LoadPtr, CanReduceDemand );
					}

				}}

			}

		} // MgrNum

	}

	void
	UpdateDemandManagers()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   July 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Expires limits and rotates loads after specified time duration.
		// It updates availability flags, expires managers that ended in the last timestep, etc.

		// METHODOLOGY EMPLOYED:

		// Using/Aliasing
		using DataGlobals::MinutesPerTimeStep;
		using ExteriorEnergyUse::ExteriorLights;
		using DataHeatBalance::Lights;
		using DataHeatBalance::ZoneElectric;
		using DataZoneControls::TempControlledZone;
		using ScheduleManager::GetCurrentScheduleValue;

		// Locals
		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int MgrNum;
		int LoadNum;
		int LoadPtr;
		bool Available;
		bool CanReduceDemand;
		int RotatedLoadNum;

		// FLOW:
		for ( MgrNum = 1; MgrNum <= NumDemandMgr; ++MgrNum ) {

			// Check availability
			//    IF (DemandMgr(MgrNum)%AvailSchedule .EQ. 0) THEN
			//      Available = .TRUE.  ! No schedule defaults to available
			//    ELSE
			if ( GetCurrentScheduleValue( DemandMgr( MgrNum ).AvailSchedule ) > 0.0 ) {
				Available = true;
			} else {
				Available = false;
			}
			//    END IF

			DemandMgr( MgrNum ).Available = Available;

			// Update demand manager status
			if ( Available ) {

				if ( DemandMgr( MgrNum ).Active ) {

					DemandMgr( MgrNum ).ElapsedTime += MinutesPerTimeStep;

					// Check for expiring limit duration
					if ( DemandMgr( MgrNum ).ElapsedTime >= DemandMgr( MgrNum ).LimitDuration ) {
						DemandMgr( MgrNum ).ElapsedTime = 0;
						DemandMgr( MgrNum ).ElapsedRotationTime = 0;
						DemandMgr( MgrNum ).Active = false;

						// Demand Manager is not available, remove demand limits from all loads
						for ( LoadNum = 1; LoadNum <= DemandMgr( MgrNum ).NumOfLoads; ++LoadNum ) {
							LoadPtr = DemandMgr( MgrNum ).Load( LoadNum );
							LoadInterface( ClearLimit, MgrNum, LoadPtr, CanReduceDemand );
						} // LoadNum

					} else {

						{ auto const SELECT_CASE_var( DemandMgr( MgrNum ).SelectionControl );
						if ( SELECT_CASE_var == ManagerSelectionAll ) {
							// Do nothing; limits remain on all loads

						} else if ( SELECT_CASE_var == ManagerSelectionMany ) { // All loads are limited except for one
							DemandMgr( MgrNum ).ElapsedRotationTime += MinutesPerTimeStep;

							if ( DemandMgr( MgrNum ).ElapsedRotationTime >= DemandMgr( MgrNum ).RotationDuration ) {
								DemandMgr( MgrNum ).ElapsedRotationTime = 0;

								if ( DemandMgr( MgrNum ).NumOfLoads > 1 ) {
									// Turn ON limiting for the old rotated load
									RotatedLoadNum = DemandMgr( MgrNum ).RotatedLoadNum;
									LoadPtr = DemandMgr( MgrNum ).Load( RotatedLoadNum );
									LoadInterface( SetLimit, MgrNum, LoadPtr, CanReduceDemand );

									// Set next rotated load
									++RotatedLoadNum;
									if ( RotatedLoadNum > DemandMgr( MgrNum ).NumOfLoads ) RotatedLoadNum = 1;
									DemandMgr( MgrNum ).RotatedLoadNum = RotatedLoadNum;

									// Turn OFF limiting for the new rotated load
									LoadPtr = DemandMgr( MgrNum ).Load( RotatedLoadNum );
									LoadInterface( ClearLimit, MgrNum, LoadPtr, CanReduceDemand );
								}
							}

						} else if ( SELECT_CASE_var == ManagerSelectionOne ) { // Only one load is limited
							DemandMgr( MgrNum ).ElapsedRotationTime += MinutesPerTimeStep;

							if ( DemandMgr( MgrNum ).ElapsedRotationTime >= DemandMgr( MgrNum ).RotationDuration ) {
								DemandMgr( MgrNum ).ElapsedRotationTime = 0;

								if ( DemandMgr( MgrNum ).NumOfLoads > 1 ) {
									// Turn OFF limiting for the old rotated load
									RotatedLoadNum = DemandMgr( MgrNum ).RotatedLoadNum;
									LoadPtr = DemandMgr( MgrNum ).Load( RotatedLoadNum );
									LoadInterface( ClearLimit, MgrNum, LoadPtr, CanReduceDemand );

									// Set next rotated load
									++RotatedLoadNum;
									if ( RotatedLoadNum > DemandMgr( MgrNum ).NumOfLoads ) RotatedLoadNum = 1;
									DemandMgr( MgrNum ).RotatedLoadNum = RotatedLoadNum;

									// Turn ON limiting for the new rotated load
									LoadPtr = DemandMgr( MgrNum ).Load( RotatedLoadNum );
									LoadInterface( SetLimit, MgrNum, LoadPtr, CanReduceDemand );
								}
							}

						}}

					}
				}

			} else { // Demand Manager is not available
				DemandMgr( MgrNum ).Active = false;

				// Demand Manager is not available, remove demand limits from all loads
				for ( LoadNum = 1; LoadNum <= DemandMgr( MgrNum ).NumOfLoads; ++LoadNum ) {
					LoadPtr = DemandMgr( MgrNum ).Load( LoadNum );
					LoadInterface( ClearLimit, MgrNum, LoadPtr, CanReduceDemand );
				} // LoadNum

			}

		} // MgrNum

	}

	void
	ReportDemandManagerList( int const ListNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   July 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates report variables.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// Using/Aliasing
		using DataGlobals::MinutesPerTimeStep;
		using DataEnvironment::Month;
		using ScheduleManager::GetCurrentScheduleValue;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 BillingPeriod;
		int Item;
		int AveragingWindow;
		bool OnPeak;
		Real64 OverLimit;

		// FLOW:
		if ( DemandManagerList( ListNum ).BillingSchedule == 0 ) {
			BillingPeriod = Month;
		} else {
			BillingPeriod = GetCurrentScheduleValue( DemandManagerList( ListNum ).BillingSchedule );
		}

		if ( DemandManagerList( ListNum ).BillingPeriod != BillingPeriod ) {
			// Reset variables for new billing period
			//DemandManagerList(ListNum)%History = 0.0        ! Don't reset--continue from previous billing period
			//DemandManagerList(ListNum)%AverageDemand = 0.0  ! Don't reset--continue from previous billing period
			DemandManagerList( ListNum ).PeakDemand = 0.0;
			DemandManagerList( ListNum ).OverLimitDuration = 0.0;

			DemandManagerList( ListNum ).BillingPeriod = BillingPeriod;
		}

		// Add new timestep to demand history and subtract oldest timestep
		AveragingWindow = DemandManagerList( ListNum ).AveragingWindow;
		DemandManagerList( ListNum ).AverageDemand += ( DemandManagerList( ListNum ).MeterDemand - DemandManagerList( ListNum ).History( 1 ) ) / AveragingWindow;

		// Update demand history
		for ( Item = 1; Item <= AveragingWindow - 1; ++Item ) {
			DemandManagerList( ListNum ).History( Item ) = DemandManagerList( ListNum ).History( Item + 1 );
		}
		DemandManagerList( ListNum ).History( AveragingWindow ) = DemandManagerList( ListNum ).MeterDemand;

		if ( DemandManagerList( ListNum ).PeakSchedule == 0 ) {
			OnPeak = true;
		} else {
			if ( GetCurrentScheduleValue( DemandManagerList( ListNum ).PeakSchedule ) == 1 ) {
				OnPeak = true;
			} else {
				OnPeak = false;
			}
		}

		if ( OnPeak ) {
			DemandManagerList( ListNum ).PeakDemand = max( DemandManagerList( ListNum ).AverageDemand, DemandManagerList( ListNum ).PeakDemand );

			OverLimit = DemandManagerList( ListNum ).AverageDemand - DemandManagerList( ListNum ).ScheduledLimit;
			if ( OverLimit > 0.0 ) {
				DemandManagerList( ListNum ).OverLimit = OverLimit;
				DemandManagerList( ListNum ).OverLimitDuration += ( MinutesPerTimeStep / 60.0 );
			} else {
				DemandManagerList( ListNum ).OverLimit = 0.0;
			}

		} else {
			DemandManagerList( ListNum ).OverLimit = 0.0;
		}

	}

	void
	LoadInterface(
		int const Action,
		int const MgrNum,
		int const LoadPtr,
		bool & CanReduceDemand
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   August 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Provides a universal interface to handle all communication with the various load objects.
		// Demand managers for new types of loads can be easily added with a new CASE statement in this subroutine
		// and new GetInput code.

		// METHODOLOGY EMPLOYED:

		// Using/Aliasing
		using ExteriorEnergyUse::ExteriorLights;
		using DataHeatBalance::Lights;
		using DataHeatBalance::ZoneElectric;
		using DataZoneControls::TempControlledZone;
		using DataZoneControls::ComfortControlledZone;
		using DataZoneControls::NumComfortControlledZones;
		using DataHeatBalFanSys::ZoneThermostatSetPointHi;
		using DataHeatBalFanSys::ZoneThermostatSetPointLo;
		using DataHeatBalFanSys::ComfortControlType;
		using MixedAir::OAGetFlowRate;
		using MixedAir::OAGetMinFlowRate;
		using MixedAir::OASetDemandManagerVentilationState;
		using MixedAir::OASetDemandManagerVentilationFlow;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 LowestPower;

		// FLOW:
		CanReduceDemand = false;

		{ auto const SELECT_CASE_var( DemandMgr( MgrNum ).Type );

		if ( SELECT_CASE_var == ManagerTypeExtLights ) {
			LowestPower = ExteriorLights( LoadPtr ).DesignLevel * DemandMgr( MgrNum ).LowerLimit;
			if ( Action == CheckCanReduce ) {
				if ( ExteriorLights( LoadPtr ).Power > LowestPower ) CanReduceDemand = true;
			} else if ( Action == SetLimit ) {
				ExteriorLights( LoadPtr ).ManageDemand = true;
				ExteriorLights( LoadPtr ).DemandLimit = LowestPower;
			} else if ( Action == ClearLimit ) {
				ExteriorLights( LoadPtr ).ManageDemand = false;
			}

		} else if ( SELECT_CASE_var == ManagerTypeLights ) {
			LowestPower = Lights( LoadPtr ).DesignLevel * DemandMgr( MgrNum ).LowerLimit;
			if ( Action == CheckCanReduce ) {
				if ( Lights( LoadPtr ).Power > LowestPower ) CanReduceDemand = true;
			} else if ( Action == SetLimit ) {
				Lights( LoadPtr ).ManageDemand = true;
				Lights( LoadPtr ).DemandLimit = LowestPower;
			} else if ( Action == ClearLimit ) {
				Lights( LoadPtr ).ManageDemand = false;
			}

		} else if ( SELECT_CASE_var == ManagerTypeElecEquip ) {
			LowestPower = ZoneElectric( LoadPtr ).DesignLevel * DemandMgr( MgrNum ).LowerLimit;
			if ( Action == CheckCanReduce ) {
				if ( ZoneElectric( LoadPtr ).Power > LowestPower ) CanReduceDemand = true;
			} else if ( Action == SetLimit ) {
				ZoneElectric( LoadPtr ).ManageDemand = true;
				ZoneElectric( LoadPtr ).DemandLimit = LowestPower;
			} else if ( Action == ClearLimit ) {
				ZoneElectric( LoadPtr ).ManageDemand = false;
			}

		} else if ( SELECT_CASE_var == ManagerTypeThermostats ) {
			if ( Action == CheckCanReduce ) {
				if ( ZoneThermostatSetPointLo( TempControlledZone( LoadPtr ).ActualZoneNum ) > DemandMgr( MgrNum ).LowerLimit || ZoneThermostatSetPointHi( TempControlledZone( LoadPtr ).ActualZoneNum ) < DemandMgr( MgrNum ).UpperLimit ) CanReduceDemand = true; // Heating | Cooling
			} else if ( Action == SetLimit ) {
				TempControlledZone( LoadPtr ).ManageDemand = true;
				TempControlledZone( LoadPtr ).HeatingResetLimit = DemandMgr( MgrNum ).LowerLimit;
				TempControlledZone( LoadPtr ).CoolingResetLimit = DemandMgr( MgrNum ).UpperLimit;
			} else if ( Action == ClearLimit ) {
				TempControlledZone( LoadPtr ).ManageDemand = false;
			}
			if ( NumComfortControlledZones > 0 ) {
				if ( ComfortControlType( TempControlledZone( LoadPtr ).ActualZoneNum ) > 0 ) {
					if ( Action == CheckCanReduce ) {
						if ( ZoneThermostatSetPointLo( ComfortControlledZone( LoadPtr ).ActualZoneNum ) > DemandMgr( MgrNum ).LowerLimit || ZoneThermostatSetPointHi( ComfortControlledZone( LoadPtr ).ActualZoneNum ) < DemandMgr( MgrNum ).UpperLimit ) CanReduceDemand = true; //Heating
					} else if ( Action == SetLimit ) {
						ComfortControlledZone( LoadPtr ).ManageDemand = true;
						ComfortControlledZone( LoadPtr ).HeatingResetLimit = DemandMgr( MgrNum ).LowerLimit;
						ComfortControlledZone( LoadPtr ).CoolingResetLimit = DemandMgr( MgrNum ).UpperLimit;
					} else if ( Action == ClearLimit ) {
						ComfortControlledZone( LoadPtr ).ManageDemand = false;
					}
				}
			}

		} else if ( SELECT_CASE_var == ManagerTypeVentilation ) {
			Real64 FlowRate( 0 );
			FlowRate = OAGetFlowRate( LoadPtr );
			if ( Action == CheckCanReduce ) {
				CanReduceDemand = true;
			}
			else if ( Action == SetLimit ) {
				OASetDemandManagerVentilationState( LoadPtr, true );
				if ( DemandMgr(MgrNum).LimitControl == ManagerLimitFixed )
				{
					OASetDemandManagerVentilationFlow( LoadPtr, DemandMgr( MgrNum ).FixedRate );
				}
				else if ( DemandMgr( MgrNum ).LimitControl == ManagerLimitReductionRatio )
				{
					Real64 DemandRate( 0 );
					DemandRate = FlowRate * DemandMgr( MgrNum ).ReductionRatio;
					OASetDemandManagerVentilationFlow( LoadPtr, DemandRate );
				}
			}
			else if ( Action == ClearLimit )
			{
				OASetDemandManagerVentilationState( LoadPtr, false );
			}
		}


		}

	}

	void
	InitDemandManagers()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   September 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Provide external call to get Demand manager input after
		// appropriate initializations.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		if ( GetInput ) {
			GetDemandManagerInput();
			GetDemandManagerListInput();
			GetInput = false;
		}

	}

} // DemandManager

} // EnergyPlus
