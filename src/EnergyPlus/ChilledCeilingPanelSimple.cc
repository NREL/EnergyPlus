// EnergyPlus, Copyright (c) 1996-2017, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
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
//     similar designation, without the U.S. Department of Energy's prior written consent.
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

// C++ Headers
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <ChilledCeilingPanelSimple.hh>
#include <BranchNodeConnections.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHeatBalSurface.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataPlant.hh>
#include <DataSizing.hh>
#include <DataSurfaces.hh>
#include <DataZoneEnergyDemands.hh>
#include <DataZoneEquipment.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <HeatBalanceSurfaceManager.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <PlantUtilities.hh>
#include <Psychrometrics.hh>
#include <ScheduleManager.hh>
#include <ReportSizingManager.hh>

namespace EnergyPlus {

namespace CoolingPanelSimple {

	// Module -- (ref: Object: ZoneHVAC:CoolingPanel:RadiantConvective:Water)

	// Module containing the routines dealing with the simple (chilled ceiling) cooling panels

	// MODULE INFORMATION:
	//       AUTHOR         Rick Strand
	//       DATE WRITTEN   Aug 2014

	// PURPOSE OF THIS MODULE:
	// The purpose of this module is to simulate simple chilled ceiling panels.  It is similar to
    // hot water radiant/convective baseboard units and the code for this model used that model as
    // a starting point.

	// REFERENCES:
    // Existing code for hot water baseboard models (radiant-convective variety)
    
	// USE STATEMENTS:
	// Using/Aliasing
	using namespace DataGlobals;

	// Data
	//MODULE PARAMETER DEFINITIONS
	std::string const cCMO_CoolingPanel_Simple( "ZoneHVAC:CoolingPanel:RadiantConvective:Water" );
	// Control types:
	int const MATControl( 1 ); // Controls system using mean air temperature
	int const MRTControl( 2 ); // Controls system using mean radiant temperature
	int const OperativeControl( 3 ); // Controls system using operative temperature
	int const ODBControl( 4 ); // Controls system using outside air dry-bulb temperature
	int const OWBControl( 5 ); // Controls system using outside air wet-bulb temperature
	int const ZoneTotalLoadControl( 6 ); //controls system using zone total remaining load
	int const ZoneConvectiveLoadControl( 7 ); //controls system using zone convective remaining load
	// Condensation control types:
	int const CondCtrlNone( 0 ); // Condensation control--none, so system never shuts down
	int const CondCtrlSimpleOff( 1 ); // Condensation control--simple off, system shuts off when condensation predicted
	int const CondCtrlVariedOff( 2 ); // Condensation control--variable off, system modulates to keep running if possible

	//MODULE VARIABLE DECLARATIONS:
	int NumCoolingPanels( 0 );
	Array1D< Real64 > CoolingPanelSource; // Need to keep the last value in case we are still iterating
	Array1D< Real64 > CoolingPanelSrcAvg; // Need to keep the last value in case we are still iterating
	Array1D< Real64 > ZeroSourceSumHATsurf; // Equal to the SumHATsurf for all the walls in a zone with no source

	// Record keeping variables used to calculate CoolingPanelSrcAvg locally
	Array1D< Real64 > LastCoolingPanelSrc; // Need to keep the last value in case we are still iterating
	Array1D< Real64 > LastSysTimeElapsed; // Need to keep the last value in case we are still iterating
	Array1D< Real64 > LastTimeStepSys; // Need to keep the last value in case we are still iterating
	Array1D_bool CheckEquipName;
	Array1D_bool SetLoopIndexFlag; // get loop number flag

	// Autosizing variables
	Array1D_bool MySizeFlagCoolPanel;
	
	// Other variables
	static bool GetInputFlag( true ); // One time get input flag
	static bool MyOneTimeFlag( true );
	
	//SUBROUTINE SPECIFICATIONS FOR MODULE Simple Chilled Ceiling Panel
	// Object Data
	Array1D< CoolingPanelParams > CoolingPanel;
	Array1D< CoolingPanelSysNumericFieldData > CoolingPanelSysNumericFields;

	// Functions

	void
	clear_state()
	{
		GetInputFlag = true;
		MyOneTimeFlag = true;
		CoolingPanelSource.deallocate();
		CoolingPanelSrcAvg.deallocate();
		ZeroSourceSumHATsurf.deallocate();
		LastCoolingPanelSrc.deallocate();
		LastSysTimeElapsed.deallocate();
		LastTimeStepSys.deallocate();
		CheckEquipName.deallocate();
		SetLoopIndexFlag.deallocate();
		CoolingPanel.deallocate();
		CoolingPanelSysNumericFields.deallocate();
		MySizeFlagCoolPanel.deallocate();
	}
	
	void
	SimCoolingPanel(
		std::string const & EquipName,
		int const ActualZoneNum,
		int const ControlledZoneNum,
		bool const FirstHVACIteration,
		Real64 & PowerMet,
		int & CompIndex
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   Aug 2014

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine simulates the simple cooling (chilled ceiling) panel.  It borrows heavily
        // from the hot water radiant-convective baseboard model code.

		// REFERENCES:
		// Existing code for hot water baseboard models (radiant-convective variety)

		// Using/Aliasing
		using DataLoopNode::Node;
		using InputProcessor::FindItemInList;
		using General::TrimSigDigits;
		using DataZoneEnergyDemands::ZoneSysEnergyDemand;
		using DataPlant::TypeOf_CoolingPanel_Simple;

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int CoolingPanelNum; // Index of unit in baseboard array
		Real64 QZnReq; // Zone load not yet satisfied
		Real64 MaxWaterFlow;
		Real64 MinWaterFlow;

		if ( GetInputFlag ) {
			GetCoolingPanelInput();
			GetInputFlag = false;
		}

		// Find the correct Baseboard Equipment
		if ( CompIndex == 0 ) {
			CoolingPanelNum = FindItemInList( EquipName, CoolingPanel, &CoolingPanelParams::EquipID, NumCoolingPanels );
			if ( CoolingPanelNum == 0 ) {
				ShowFatalError( "SimCoolingPanelSimple: Unit not found=" + EquipName );
			}
			CompIndex = CoolingPanelNum;
		} else {
			CoolingPanelNum = CompIndex;
			if ( CoolingPanelNum > NumCoolingPanels || CoolingPanelNum < 1 ) {
				ShowFatalError( "SimCoolingPanelSimple:  Invalid CompIndex passed=" + TrimSigDigits( CoolingPanelNum ) + ", Number of Units=" + TrimSigDigits( NumCoolingPanels ) + ", Entered Unit name=" + EquipName );
			}
			if ( CheckEquipName( CoolingPanelNum ) ) {
				if ( EquipName != CoolingPanel( CoolingPanelNum ).EquipID ) {
					ShowFatalError( "SimCoolingPanelSimple: Invalid CompIndex passed=" + TrimSigDigits( CoolingPanelNum ) + ", Unit name=" + EquipName + ", stored Unit Name for that index=" + CoolingPanel( CoolingPanelNum ).EquipID );
				}
				CheckEquipName( CoolingPanelNum ) = false;
			}
		}

		if ( CompIndex > 0 ) {

			InitCoolingPanel( CoolingPanelNum, ControlledZoneNum, FirstHVACIteration );

			QZnReq = ZoneSysEnergyDemand( ActualZoneNum ).RemainingOutputReqToCoolSP;

			// On the first HVAC iteration the system values are given to the controller, but after that
			// the demand limits are in place and there needs to be feedback to the Zone Equipment
			if ( FirstHVACIteration ) {
				MaxWaterFlow = CoolingPanel( CoolingPanelNum ).WaterMassFlowRateMax;
				MinWaterFlow = 0.0;
			} else {
				MaxWaterFlow = Node( CoolingPanel( CoolingPanelNum ).WaterInletNode ).MassFlowRateMaxAvail;
				MinWaterFlow = Node( CoolingPanel( CoolingPanelNum ).WaterInletNode ).MassFlowRateMinAvail;
			}

			{ auto const SELECT_CASE_var( CoolingPanel( CoolingPanelNum ).EquipType );

			if ( SELECT_CASE_var == TypeOf_CoolingPanel_Simple ) { // 'ZoneHVAC:CoolingPanel:RadiantConvective:Water'
				CalcCoolingPanel( CoolingPanelNum );
			} else {
				ShowSevereError( "SimCoolingPanelSimple: Errors in CoolingPanel=" + CoolingPanel( CoolingPanelNum ).EquipID );
				ShowContinueError( "Invalid or unimplemented equipment type=" + TrimSigDigits( CoolingPanel( CoolingPanelNum ).EquipType ) );
				ShowFatalError( "Preceding condition causes termination." );

			}}

			PowerMet = CoolingPanel( CoolingPanelNum ).TotPower;

			UpdateCoolingPanel( CoolingPanelNum );

			ReportCoolingPanel( CoolingPanelNum );

		} else {
			ShowFatalError( "SimCoolingPanelSimple: Unit not found=" + EquipName );
		}

	}

	void
	GetCoolingPanelInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   Aug 2014

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine gets the input for the simple cooling panel units.

		// METHODOLOGY EMPLOYED:
		// Standard input processor calls--started from Daeho's radiant-convective water baseboard model.

		// Using/Aliasing
		using DataLoopNode::NodeType_Water;
		using DataLoopNode::NodeConnectionType_Inlet;
		using DataLoopNode::NodeConnectionType_Outlet;
		using DataLoopNode::ObjectIsNotParent;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using InputProcessor::SameString;
		using InputProcessor::VerifyName;
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::TestCompSet;
		using DataSurfaces::Surface;
		using ScheduleManager::GetScheduleIndex;
		using General::RoundSigDigits;
		using General::TrimSigDigits;
		using DataPlant::TypeOf_CoolingPanel_Simple;
		using namespace DataIPShortCuts;

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetCoolingPanelInput:" );
		Real64 const MaxFraction( 1.0 );
		Real64 const MinFraction( 0.0 );
		Real64 const MaxWaterTempAvg( 30.0 ); // Maximum limit of average water temperature in degree C
		Real64 const MinWaterTempAvg( 0.0 ); // Minimum limit of average water temperature in degree C
		Real64 const MaxWaterFlowRate( 10.0 ); // Maximum limit of water volume flow rate in m3/s
		Real64 const MinWaterFlowRate( 0.00001 ); // Minimum limit of water volume flow rate in m3/s
		Real64 const WaterMassFlowDefault( 0.063 ); // Default water mass flow rate in kg/s
		int const MinDistribSurfaces( 1 ); // Minimum number of surfaces that a baseboard heater can radiate to
		Real64 const MinThrottlingRange( 0.5 ); // Smallest throttling range allowed in degrees Celsius
		static std::string const MeanAirTemperature( "MeanAirTemperature" );
		static std::string const MeanRadiantTemperature( "MeanRadiantTemperature" );
		static std::string const OperativeTemperature( "OperativeTemperature" );
		static std::string const OutsideAirDryBulbTemperature( "OutdoorDryBulbTemperature" );
		static std::string const OutsideAirWetBulbTemperature( "OutdoorWetBulbTemperature" );
		static std::string const ZoneTotalLoad( "ZoneTotalLoad" );
		static std::string const ZoneConvectiveLoad( "ZoneConvectiveLoad" );
		static std::string const Off( "Off" );
		static std::string const SimpleOff( "SimpleOff" );
		static std::string const VariableOff( "VariableOff" );

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 AllFracsSummed; // Sum of the fractions radiant
		int CoolingPanelNum; // Cooling panel number
		int CoolPanelNumI; // For loop index
		int NumAlphas; // Number of Alphas for each GetobjectItem call
		int NumNumbers; // Number of Numbers for each GetobjectItem call
		int SurfNum; // Surface number Do loop counter
		int IOStat;
		static bool ErrorsFound( false ); // If errors detected in input
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name

		NumCoolingPanels = GetNumObjectsFound( cCMO_CoolingPanel_Simple );

		// Count total number of baseboard units

		CoolingPanel.allocate( NumCoolingPanels );
		CoolingPanelSysNumericFields.allocate( NumCoolingPanels );
		CheckEquipName.allocate( NumCoolingPanels );
		CheckEquipName = true;

		// Get the data from the user input related to cooling panels
		for ( CoolingPanelNum = 1; CoolingPanelNum <= NumCoolingPanels; ++CoolingPanelNum ) {

			GetObjectItem( cCMO_CoolingPanel_Simple, CoolingPanelNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			CoolingPanelSysNumericFields( CoolingPanelNum ).FieldNames.allocate( NumNumbers );
			CoolingPanelSysNumericFields( CoolingPanelNum ).FieldNames = "";
			CoolingPanelSysNumericFields( CoolingPanelNum ).FieldNames = cNumericFieldNames;
			
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), CoolingPanel, &CoolingPanelParams::EquipID, CoolingPanelNum, IsNotOK, IsBlank, cCMO_CoolingPanel_Simple + " Name" );

			if ( IsNotOK ) {
				ErrorsFound = true;
			}
			if ( CoolingPanelNum > 1 ) {
				for ( CoolPanelNumI = 2; CoolPanelNumI <= NumCoolingPanels; ++CoolPanelNumI ) {
					if ( cAlphaArgs(1) == CoolingPanel( CoolPanelNumI ).EquipID) {
						ErrorsFound = true;
						ShowSevereError( cAlphaArgs( 1 ) + " is used as a name for more than one simple COOLING PANEL." );
						ShowContinueError( "This is not allowed.");
					}
				}

			}
			CoolingPanel( CoolingPanelNum ).EquipID = cAlphaArgs( 1 ); // Name of this simple cooling panel
			CoolingPanel( CoolingPanelNum ).EquipType = TypeOf_CoolingPanel_Simple; //'ZoneHVAC:CoolingPanel:RadiantConvective:Water'

			// Get schedule
			CoolingPanel( CoolingPanelNum ).Schedule = cAlphaArgs( 2 );
			if ( lAlphaFieldBlanks( 2 ) ) {
				CoolingPanel( CoolingPanelNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				CoolingPanel( CoolingPanelNum ).SchedPtr = GetScheduleIndex( cAlphaArgs( 2 ) );
				if ( CoolingPanel( CoolingPanelNum ).SchedPtr == 0 ) {
					ShowSevereError( RoutineName + cCMO_CoolingPanel_Simple + "=\"" + cAlphaArgs( 1 ) + "\", " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\" not found." );
					ErrorsFound = true;
				}
			}

			// Get inlet node number
			CoolingPanel( CoolingPanelNum ).WaterInletNode = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCMO_CoolingPanel_Simple, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

			// Get outlet node number
			CoolingPanel( CoolingPanelNum ).WaterOutletNode = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCMO_CoolingPanel_Simple, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			TestCompSet( cCMO_CoolingPanel_Simple, cAlphaArgs( 1 ), cAlphaArgs( 3 ), cAlphaArgs( 4 ), "Chilled Water Nodes" );

			CoolingPanel( CoolingPanelNum ).RatedWaterTemp = rNumericArgs( 1 );
			if ( CoolingPanel( CoolingPanelNum ).RatedWaterTemp > MaxWaterTempAvg + 0.001 ) {
				ShowWarningError( RoutineName + cCMO_CoolingPanel_Simple + "=\"" + cAlphaArgs( 1 ) + "\", " + cNumericFieldNames( 1 ) + " was higher than the allowable maximum." );
				ShowContinueError( "...reset to maximum value=[" + RoundSigDigits( MaxWaterTempAvg, 2 ) + "]." );
				CoolingPanel( CoolingPanelNum ).RatedWaterTemp = MaxWaterTempAvg;
			} else if ( CoolingPanel( CoolingPanelNum ).RatedWaterTemp < MinWaterTempAvg - 0.001 ) {
				ShowWarningError( RoutineName + cCMO_CoolingPanel_Simple + "=\"" + cAlphaArgs( 1 ) + "\", " + cNumericFieldNames( 1 ) + " was lower than the allowable minimum." );
				ShowContinueError( "...reset to minimum value=[" + RoundSigDigits( MinWaterTempAvg, 2 ) + "]." );
				CoolingPanel( CoolingPanelNum ).RatedWaterTemp = MinWaterTempAvg;
			}

			CoolingPanel( CoolingPanelNum ).RatedZoneAirTemp = rNumericArgs( 2 );
			if ( CoolingPanel( CoolingPanelNum ).RatedZoneAirTemp > MaxWaterTempAvg + 0.001 ) {
				ShowWarningError( RoutineName + cCMO_CoolingPanel_Simple + "=\"" + cAlphaArgs( 1 ) + "\", " + cNumericFieldNames( 2 ) + " was higher than the allowable maximum." );
				ShowContinueError( "...reset to maximum value=[" + RoundSigDigits( MaxWaterTempAvg, 2 ) + "]." );
				CoolingPanel( CoolingPanelNum ).RatedZoneAirTemp = MaxWaterTempAvg;
			} else if ( CoolingPanel( CoolingPanelNum ).RatedZoneAirTemp < MinWaterTempAvg - 0.001 ) {
				ShowWarningError( RoutineName + cCMO_CoolingPanel_Simple + "=\"" + cAlphaArgs( 1 ) + "\", " + cNumericFieldNames( 2 ) + " was lower than the allowable minimum." );
				ShowContinueError( "...reset to minimum value=[" + RoundSigDigits( MinWaterTempAvg, 2 ) + "]." );
				CoolingPanel( CoolingPanelNum ).RatedZoneAirTemp = MinWaterTempAvg;
			}
			
			CoolingPanel( CoolingPanelNum ).RatedWaterFlowRate = rNumericArgs( 3 );
			if ( CoolingPanel( CoolingPanelNum ).RatedWaterFlowRate < 0.00001  || CoolingPanel( CoolingPanelNum ).RatedWaterFlowRate > 10.0 ) {
				ShowWarningError( RoutineName + cCMO_CoolingPanel_Simple + "=\"" + cAlphaArgs( 1 ) + "\", " + cNumericFieldNames( 2 ) + " is an invalid Standard Water mass flow rate." );
				ShowContinueError( "...reset to a default value=[" + RoundSigDigits( WaterMassFlowDefault, 1 ) + "]." );
				CoolingPanel( CoolingPanelNum ).RatedWaterFlowRate = WaterMassFlowDefault;
			}

			if ( SameString( cAlphaArgs( 5 ), "CoolingDesignCapacity" ) ) {
				CoolingPanel( CoolingPanelNum ).CoolingCapMethod = DataSizing::CoolingDesignCapacity;
				if ( ! lNumericFieldBlanks( 4 ) ) {
					CoolingPanel( CoolingPanelNum ).ScaledCoolingCapacity = rNumericArgs( 4 );
					if ( CoolingPanel( CoolingPanelNum ).ScaledCoolingCapacity < 0.0 && CoolingPanel( CoolingPanelNum ).ScaledCoolingCapacity != DataSizing::AutoSize ) {
						ShowSevereError( cCMO_CoolingPanel_Simple + " = " + CoolingPanel( CoolingPanelNum ).EquipID );
						ShowContinueError( "Illegal " + cNumericFieldNames( 4 ) + " = " + TrimSigDigits( rNumericArgs( 4 ), 7 ) );
						ErrorsFound = true;
					}
				} else {
					if( ( !lAlphaFieldBlanks( 6 ) ) || ( !lAlphaFieldBlanks( 7 ) ) ) {
						ShowSevereError( cCMO_CoolingPanel_Simple + " = " + CoolingPanel( CoolingPanelNum ).EquipID );
						ShowContinueError( "Input for " + cAlphaFieldNames( 5 ) + " = " + cAlphaArgs( 5 ) );
						ShowContinueError( "Blank field not allowed for " + cNumericFieldNames( 4 ) );
						ErrorsFound = true;
					}
				}
			} else if ( SameString( cAlphaArgs( 5 ), "CapacityPerFloorArea" ) ) {
				CoolingPanel( CoolingPanelNum ).CoolingCapMethod = DataSizing::CapacityPerFloorArea;
				if ( ! lNumericFieldBlanks( 5 ) ) {
					CoolingPanel( CoolingPanelNum ).ScaledCoolingCapacity = rNumericArgs( 5 );
					if ( CoolingPanel( CoolingPanelNum ).CoolingCapMethod <= 0.0) {
						ShowSevereError( cCMO_CoolingPanel_Simple + " = " + CoolingPanel( CoolingPanelNum ).EquipID );
						ShowContinueError( "Input for " + cAlphaFieldNames( 5 ) + " = " + cAlphaArgs( 5 ) );
						ShowContinueError( "Illegal " + cNumericFieldNames( 5 ) + " = " + TrimSigDigits( rNumericArgs( 5 ), 7 ) );
						ErrorsFound = true;
					} else if ( CoolingPanel( CoolingPanelNum ).ScaledCoolingCapacity == DataSizing::AutoSize ) {
						ShowSevereError( cCMO_CoolingPanel_Simple + " = " + CoolingPanel( CoolingPanelNum ).EquipID );
						ShowContinueError( "Input for " + cAlphaFieldNames( 5 ) + " = " + cAlphaArgs( 5 ) );
						ShowContinueError( "Illegal " + cNumericFieldNames( 5 ) + " = Autosize" );
						ErrorsFound = true;
					}
				} else {
					ShowSevereError( cCMO_CoolingPanel_Simple + " = " + CoolingPanel( CoolingPanelNum ).EquipID );
					ShowContinueError( "Input for " + cAlphaFieldNames( 5 ) + " = " + cAlphaArgs( 5 ) );
					ShowContinueError( "Blank field not allowed for " + cNumericFieldNames( 5 ) );
					ErrorsFound = true;
				}
			} else if (SameString( cAlphaArgs( 5 ), "FractionOfAutosizedCoolingCapacity" ) ) {
				CoolingPanel( CoolingPanelNum ).CoolingCapMethod = DataSizing::FractionOfAutosizedCoolingCapacity;
				if ( ! lNumericFieldBlanks( 6 ) ) {
					CoolingPanel( CoolingPanelNum ).ScaledCoolingCapacity = rNumericArgs( 6 );
					if ( CoolingPanel( CoolingPanelNum ).ScaledCoolingCapacity < 0.0 ) {
						ShowSevereError( cCMO_CoolingPanel_Simple + " = " + CoolingPanel( CoolingPanelNum ).EquipID );
						ShowContinueError( "Illegal " + cNumericFieldNames( 6 ) + " = " + TrimSigDigits( rNumericArgs( 6 ), 7 ) );
						ErrorsFound = true;
					}
				} else {
					ShowSevereError( cCMO_CoolingPanel_Simple + " = " + CoolingPanel( CoolingPanelNum ).EquipID );
					ShowContinueError( "Input for " + cAlphaFieldNames( 5 ) + " = " + cAlphaArgs( 5 ) );
					ShowContinueError( "Blank field not allowed for " + cNumericFieldNames( 6 ) );
					ErrorsFound = true;
				}
			} else {
				ShowSevereError( cCMO_CoolingPanel_Simple + " = " + CoolingPanel( CoolingPanelNum ).EquipID );
				ShowContinueError( "Illegal " + cAlphaFieldNames( 5 ) + " = " + cAlphaArgs( 5 ) );
				ErrorsFound = true;
			}
			
			CoolingPanel( CoolingPanelNum ).WaterVolFlowRateMax = rNumericArgs( 7 );
			if ( ( CoolingPanel( CoolingPanelNum ).WaterVolFlowRateMax <= MinWaterFlowRate ) && CoolingPanel( CoolingPanelNum ).WaterVolFlowRateMax != DataSizing::AutoSize ) {
				ShowWarningError( RoutineName + cCMO_CoolingPanel_Simple + "=\"" + cAlphaArgs( 1 ) + "\", " + cNumericFieldNames( 7 ) + " was less than the allowable minimum." );
				ShowContinueError( "...reset to minimum value=[" + RoundSigDigits( MinWaterFlowRate, 2 ) + "]." );
				CoolingPanel( CoolingPanelNum ).WaterVolFlowRateMax = MinWaterFlowRate;
			} else if ( CoolingPanel( CoolingPanelNum ).WaterVolFlowRateMax > MaxWaterFlowRate ) {
				ShowWarningError( RoutineName + cCMO_CoolingPanel_Simple + "=\"" + cAlphaArgs( 1 ) + "\", " + cNumericFieldNames( 7 ) + " was higher than the allowable maximum." );
				ShowContinueError( "...reset to maximum value=[" + RoundSigDigits( MaxWaterFlowRate, 2 ) + "]." );
				CoolingPanel( CoolingPanelNum ).WaterVolFlowRateMax = MaxWaterFlowRate;
			}

			// Process the temperature control type
			if ( SameString( cAlphaArgs( 6 ), MeanAirTemperature ) ) {
				CoolingPanel( CoolingPanelNum ).ControlType = MATControl;
			} else if ( SameString( cAlphaArgs( 6 ), MeanRadiantTemperature ) ) {
				CoolingPanel( CoolingPanelNum ).ControlType = MRTControl;
			} else if ( SameString( cAlphaArgs( 6 ), OperativeTemperature ) ) {
				CoolingPanel( CoolingPanelNum ).ControlType = OperativeControl;
			} else if ( SameString( cAlphaArgs( 6 ), OutsideAirDryBulbTemperature ) ) {
				CoolingPanel( CoolingPanelNum ).ControlType = ODBControl;
			} else if ( SameString( cAlphaArgs( 6 ), OutsideAirWetBulbTemperature ) ) {
				CoolingPanel( CoolingPanelNum ).ControlType = OWBControl;
			} else if ( SameString( cAlphaArgs( 6 ), ZoneTotalLoad ) ) {
				CoolingPanel( CoolingPanelNum ).ControlType = ZoneTotalLoadControl;
			} else if ( SameString( cAlphaArgs( 6 ), ZoneConvectiveLoad ) ) {
				CoolingPanel( CoolingPanelNum ).ControlType = ZoneConvectiveLoadControl;
			} else {
				ShowWarningError( "Invalid " + cAlphaFieldNames( 6 ) + " =" + cAlphaArgs( 6 ) );
				ShowContinueError( "Occurs in " + RoutineName + " = " + cAlphaArgs( 1 ) );
				ShowContinueError( "Control reset to MAT control for this Simple Cooling Panel." );
				CoolingPanel( CoolingPanelNum ).ControlType = MATControl;
			}

			CoolingPanel( CoolingPanelNum ).ColdThrottlRange = rNumericArgs( 8 );
			if ( CoolingPanel( CoolingPanelNum ).ColdThrottlRange < MinThrottlingRange ) {
				ShowWarningError( cCMO_CoolingPanel_Simple + "Cooling throttling range too small, reset to 0.5" );
				ShowContinueError( "Occurs in Cooling Panel=" + CoolingPanel( CoolingPanelNum ).EquipID );
				CoolingPanel( CoolingPanelNum ).ColdThrottlRange = MinThrottlingRange;
			}
			
			CoolingPanel( CoolingPanelNum ).ColdSetptSched = cAlphaArgs( 7 );
			CoolingPanel( CoolingPanelNum ).ColdSetptSchedPtr = GetScheduleIndex( cAlphaArgs( 7 ) );
			if ( ( CoolingPanel( CoolingPanelNum ).ColdSetptSchedPtr == 0 ) && ( ! lAlphaFieldBlanks( 7 ) ) ) {
				ShowSevereError( cAlphaFieldNames( 7 ) + " not found: " + cAlphaArgs( 7 ) );
				ShowContinueError( "Occurs in " + RoutineName + " = " + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}
			
			if ( SameString( cAlphaArgs( 8 ), Off ) ) {
				CoolingPanel( CoolingPanelNum ).CondCtrlType = CondCtrlNone;
			} else if ( SameString( cAlphaArgs( 8 ), SimpleOff ) ) {
				CoolingPanel( CoolingPanelNum ).CondCtrlType = CondCtrlSimpleOff;
			} else if ( SameString( cAlphaArgs( 8 ), VariableOff ) ) {
				CoolingPanel( CoolingPanelNum ).CondCtrlType = CondCtrlVariedOff;
			} else {
				CoolingPanel( CoolingPanelNum ).CondCtrlType = CondCtrlSimpleOff;
			}
			
			CoolingPanel( CoolingPanelNum ).CondDewPtDeltaT = rNumericArgs( 9 );
			
			CoolingPanel( CoolingPanelNum ).FracRadiant = rNumericArgs( 10 );
			if ( CoolingPanel( CoolingPanelNum ).FracRadiant < MinFraction ) {
				ShowWarningError( RoutineName + cCMO_CoolingPanel_Simple + "=\"" + cAlphaArgs( 1 ) + "\", " + cNumericFieldNames( 10 ) + " was lower than the allowable minimum." );
				ShowContinueError( "...reset to minimum value=[" + RoundSigDigits( MinFraction, 2 ) + "]." );
				CoolingPanel( CoolingPanelNum ).FracRadiant = MinFraction;
			}
			if ( CoolingPanel( CoolingPanelNum ).FracRadiant > MaxFraction ) {
				ShowWarningError( RoutineName + cCMO_CoolingPanel_Simple + "=\"" + cAlphaArgs( 1 ) + "\", " + cNumericFieldNames( 10 ) + " was higher than the allowable maximum." );
				ShowContinueError( "...reset to maximum value=[" + RoundSigDigits( MaxFraction, 2 ) + "]." );
				CoolingPanel( CoolingPanelNum ).FracRadiant = MaxFraction;
			}

			// Remaining fraction is added to the zone as convective heat transfer
			AllFracsSummed = CoolingPanel( CoolingPanelNum ).FracRadiant;
			if ( AllFracsSummed > MaxFraction ) {
				ShowWarningError( RoutineName + cCMO_CoolingPanel_Simple + "=\"" + cAlphaArgs( 1 ) + "\", Fraction Radiant was higher than the allowable maximum." );
				CoolingPanel( CoolingPanelNum ).FracRadiant = MaxFraction;
				CoolingPanel( CoolingPanelNum ).FracConvect = 0.0;
			} else {
				CoolingPanel( CoolingPanelNum ).FracConvect = 1.0 - AllFracsSummed;
			}

			CoolingPanel( CoolingPanelNum ).FracDistribPerson = rNumericArgs( 11 );
			if ( CoolingPanel( CoolingPanelNum ).FracDistribPerson < MinFraction ) {
				ShowWarningError( RoutineName + cCMO_CoolingPanel_Simple + "=\"" + cAlphaArgs( 1 ) + "\", " + cNumericFieldNames( 11 ) + " was lower than the allowable minimum." );
				ShowContinueError( "...reset to minimum value=[" + RoundSigDigits( MinFraction, 3 ) + "]." );
				CoolingPanel( CoolingPanelNum ).FracDistribPerson = MinFraction;
			}
			if ( CoolingPanel( CoolingPanelNum ).FracDistribPerson > MaxFraction ) {
				ShowWarningError( RoutineName + cCMO_CoolingPanel_Simple + "=\"" + cAlphaArgs( 1 ) + "\", " + cNumericFieldNames( 11 ) + " was higher than the allowable maximum." );
				ShowContinueError( "...reset to maximum value=[" + RoundSigDigits( MaxFraction, 3 ) + "]." );
				CoolingPanel( CoolingPanelNum ).FracDistribPerson = MaxFraction;
			}

			CoolingPanel( CoolingPanelNum ).TotSurfToDistrib = NumNumbers - 11;
			if ( ( CoolingPanel( CoolingPanelNum ).TotSurfToDistrib < MinDistribSurfaces ) && ( CoolingPanel( CoolingPanelNum ).FracRadiant > MinFraction ) ) {
				ShowSevereError( RoutineName + cCMO_CoolingPanel_Simple + "=\"" + cAlphaArgs( 1 ) + "\", the number of surface/radiant fraction groups entered was less than the allowable minimum." );
				ShowContinueError( "...the minimum that must be entered=[" + RoundSigDigits( MinDistribSurfaces ) + "]." );
				ErrorsFound = true;
				CoolingPanel( CoolingPanelNum ).TotSurfToDistrib = 0; // error
			}

			CoolingPanel( CoolingPanelNum ).SurfaceName.allocate( CoolingPanel( CoolingPanelNum ).TotSurfToDistrib );
			CoolingPanel( CoolingPanelNum ).SurfaceName = "";
			CoolingPanel( CoolingPanelNum ).SurfacePtr.allocate( CoolingPanel( CoolingPanelNum ).TotSurfToDistrib );
			CoolingPanel( CoolingPanelNum ).SurfacePtr = 0;
			CoolingPanel( CoolingPanelNum ).FracDistribToSurf.allocate( CoolingPanel( CoolingPanelNum ).TotSurfToDistrib );
			CoolingPanel( CoolingPanelNum ).FracDistribToSurf = 0.0;

			AllFracsSummed = CoolingPanel( CoolingPanelNum ).FracDistribPerson;
			for ( SurfNum = 1; SurfNum <= CoolingPanel( CoolingPanelNum ).TotSurfToDistrib; ++SurfNum ) {
				CoolingPanel( CoolingPanelNum ).SurfaceName( SurfNum ) = cAlphaArgs( SurfNum + 8 );
				CoolingPanel( CoolingPanelNum ).SurfacePtr( SurfNum ) = FindItemInList( cAlphaArgs( SurfNum + 8 ), Surface );
				CoolingPanel( CoolingPanelNum ).FracDistribToSurf( SurfNum ) = rNumericArgs( SurfNum + 11 );
				if ( CoolingPanel( CoolingPanelNum ).SurfacePtr( SurfNum ) == 0 ) {
					ShowSevereError( RoutineName + cCMO_CoolingPanel_Simple + "=\"" + cAlphaArgs( 1 ) + "\", " + cAlphaFieldNames( SurfNum + 8 ) + "=\"" + cAlphaArgs( SurfNum + 8 ) + "\" invalid - not found." );
					ErrorsFound = true;
				}
				if ( CoolingPanel( CoolingPanelNum ).FracDistribToSurf( SurfNum ) > MaxFraction ) {
					ShowWarningError( RoutineName + cCMO_CoolingPanel_Simple + "=\"" + cAlphaArgs( 1 ) + "\", " + cNumericFieldNames( SurfNum + 8 ) + "was greater than the allowable maximum." );
					ShowContinueError( "...reset to maximum value=[" + RoundSigDigits( MaxFraction, 2 ) + "]." );
					CoolingPanel( CoolingPanelNum ).TotSurfToDistrib = MaxFraction;
				}
				if ( CoolingPanel( CoolingPanelNum ).FracDistribToSurf( SurfNum ) < MinFraction ) {
					ShowWarningError( RoutineName + cCMO_CoolingPanel_Simple + "=\"" + cAlphaArgs( 1 ) + "\", " + cNumericFieldNames( SurfNum + 8 ) + "was less than the allowable minimum." );
					ShowContinueError( "...reset to maximum value=[" + RoundSigDigits( MinFraction, 2 ) + "]." );
					CoolingPanel( CoolingPanelNum ).TotSurfToDistrib = MinFraction;
				}
				if ( CoolingPanel( CoolingPanelNum ).SurfacePtr( SurfNum ) != 0 ) {
					Surface( CoolingPanel( CoolingPanelNum ).SurfacePtr( SurfNum ) ).IntConvSurfGetsRadiantHeat = true;
				}

				AllFracsSummed += CoolingPanel( CoolingPanelNum ).FracDistribToSurf( SurfNum );
			} // Surfaces

			if ( AllFracsSummed > ( MaxFraction + 0.01 ) ) {
				ShowSevereError( RoutineName + cCMO_CoolingPanel_Simple + "=\"" + cAlphaArgs( 1 ) + "\", Summed radiant fractions for people + surface groups > 1.0" );
				ErrorsFound = true;
			}
			if ( ( AllFracsSummed < ( MaxFraction - 0.01 ) ) && ( CoolingPanel( CoolingPanelNum ).FracRadiant > MinFraction ) ) { // User didn't distribute all of the | radiation warn that some will be lost
				ShowSevereError( RoutineName + cCMO_CoolingPanel_Simple + "=\"" + cAlphaArgs( 1 ) + "\", Summed radiant fractions for people + surface groups < 1.0" );
				ShowContinueError( "This would result in some of the radiant energy delivered by the high temp radiant heater being lost." );
				ShowContinueError( "The sum of all radiation fractions to surfaces = " + TrimSigDigits( ( AllFracsSummed - CoolingPanel( CoolingPanelNum ).FracDistribPerson ), 5) );
				ShowContinueError( "The radiant fraction to people = " + TrimSigDigits( CoolingPanel( CoolingPanelNum ).FracDistribPerson, 5) );
				ShowContinueError( "So, all radiant fractions including surfaces and people = " + TrimSigDigits( AllFracsSummed, 5) );
				ShowContinueError( "This means that the fraction of radiant energy that would be lost from the high temperature radiant heater would be = " +  TrimSigDigits( ( 1.0 - AllFracsSummed ), 5) );
				ShowContinueError( "Please check and correct this so that all radiant energy is accounted for in " + cCMO_CoolingPanel_Simple + " = " + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}
		}

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + cCMO_CoolingPanel_Simple + "Errors found getting input. Program terminates." );
		}

		// Setup Report variables for the Coils
		for ( CoolingPanelNum = 1; CoolingPanelNum <= NumCoolingPanels; ++CoolingPanelNum ) {
			// CurrentModuleObject='ZoneHVAC:CoolingPanel:RadiantConvective:Water'
			SetupOutputVariable( "Cooling Panel Total Cooling Rate", OutputProcessor::Unit::W, CoolingPanel( CoolingPanelNum ).Power, "System", "Average", CoolingPanel( CoolingPanelNum ).EquipID );
			SetupOutputVariable( "Cooling Panel Total System Cooling Rate", OutputProcessor::Unit::W, CoolingPanel( CoolingPanelNum ).TotPower, "System", "Average", CoolingPanel( CoolingPanelNum ).EquipID );
			SetupOutputVariable( "Cooling Panel Convective Cooling Rate", OutputProcessor::Unit::W, CoolingPanel( CoolingPanelNum ).ConvPower, "System", "Average", CoolingPanel( CoolingPanelNum ).EquipID );
			SetupOutputVariable( "Cooling Panel Radiant Cooling Rate", OutputProcessor::Unit::W, CoolingPanel( CoolingPanelNum ).RadPower, "System", "Average", CoolingPanel( CoolingPanelNum ).EquipID );

			SetupOutputVariable( "Cooling Panel Total Cooling Energy", OutputProcessor::Unit::J, CoolingPanel( CoolingPanelNum ).Energy, "System", "Sum", CoolingPanel( CoolingPanelNum ).EquipID, _, "ENERGYTRANSFER", "COOLINGPANEL", _, "System" );
			SetupOutputVariable( "Cooling Panel Total System Cooling Energy", OutputProcessor::Unit::J, CoolingPanel( CoolingPanelNum ).TotEnergy, "System", "Sum", CoolingPanel( CoolingPanelNum ).EquipID, _, "ENERGYTRANSFER", "COOLINGPANEL", _, "System" );
			SetupOutputVariable( "Cooling Panel Convective Cooling Energy", OutputProcessor::Unit::J, CoolingPanel( CoolingPanelNum ).ConvEnergy, "System", "Sum", CoolingPanel( CoolingPanelNum ).EquipID );
			SetupOutputVariable( "Cooling Panel Radiant Cooling Energy", OutputProcessor::Unit::J, CoolingPanel( CoolingPanelNum ).RadEnergy, "System", "Sum", CoolingPanel( CoolingPanelNum ).EquipID );

			SetupOutputVariable( "Cooling Panel Water Mass Flow Rate", OutputProcessor::Unit::kg_s, CoolingPanel( CoolingPanelNum ).WaterMassFlowRate, "System", "Average", CoolingPanel( CoolingPanelNum ).EquipID );
			SetupOutputVariable( "Cooling Panel Water Inlet Temperature", OutputProcessor::Unit::C, CoolingPanel( CoolingPanelNum ).WaterInletTemp, "System", "Average", CoolingPanel( CoolingPanelNum ).EquipID );
			SetupOutputVariable( "Cooling Panel Water Outlet Temperature", OutputProcessor::Unit::C, CoolingPanel( CoolingPanelNum ).WaterOutletTemp, "System", "Average", CoolingPanel( CoolingPanelNum ).EquipID );
		}

	}

	void
	InitCoolingPanel(
		int const CoolingPanelNum,
		int const ControlledZoneNumSub,
		bool const FirstHVACIteration
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   Sept 2014

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine initializes the cooling panel units, and determines the UA values during simulation.

		// METHODOLOGY EMPLOYED:
		// The initialization subrotines borrowed from other sources and heat exchanger formulation for cooling panel.

		// REFERENCES:
		// Incropera and DeWitt, Fundamentals of Heat and Mass Transfer

		// Using/Aliasing
		using DataGlobals::BeginEnvrnFlag;
		using DataLoopNode::Node;
		using PlantUtilities::InitComponentNodes;
		using DataPlant::ScanPlantLoopsForObject;
		using DataZoneEquipment::ZoneEquipInputsFilled;
		using DataZoneEquipment::CheckZoneEquipmentList;
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSpecificHeatGlycol;
		using DataZoneEquipment::ZoneEquipConfig;
		using DataPlant::PlantLoop;

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "ChilledCeilingPanelSimple:InitCoolingPanel" );

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool ZoneEquipmentListChecked( false );
		static Array1D_bool MyEnvrnFlag;
		int Loop;
		int WaterInletNode;
		int ZoneNode;
		int ZoneNum;
		Real64 rho; // local fluid density
		Real64 Cp; // local fluid specific heat
		bool errFlag;

		// Do the one time initializations
		if ( MyOneTimeFlag ) {

			// Initialize the environment and sizing flags
			MyEnvrnFlag.allocate( NumCoolingPanels );
			ZeroSourceSumHATsurf.allocate( NumOfZones );
			ZeroSourceSumHATsurf = 0.0;
			CoolingPanelSource.allocate( NumCoolingPanels );
			CoolingPanelSource = 0.0;
			CoolingPanelSrcAvg.allocate( NumCoolingPanels );
			CoolingPanelSrcAvg = 0.0;
			LastCoolingPanelSrc.allocate( NumCoolingPanels );
			LastCoolingPanelSrc = 0.0;
			LastSysTimeElapsed.allocate( NumCoolingPanels );
			LastSysTimeElapsed = 0.0;
			LastTimeStepSys.allocate( NumCoolingPanels );
			LastTimeStepSys = 0.0;
			SetLoopIndexFlag.allocate( NumCoolingPanels );
			MySizeFlagCoolPanel.allocate( NumCoolingPanels );
			MySizeFlagCoolPanel = true;
			MyEnvrnFlag = true;
			MyOneTimeFlag = false;
			SetLoopIndexFlag = true;
			
		}

		if ( CoolingPanel( CoolingPanelNum ).ZonePtr <= 0 ) CoolingPanel( CoolingPanelNum ).ZonePtr = ZoneEquipConfig( ControlledZoneNumSub ).ActualZoneNum;

		// Need to check all units to see if they are on ZoneHVAC:EquipmentList or issue warning
		if ( ! ZoneEquipmentListChecked && ZoneEquipInputsFilled ) {
			ZoneEquipmentListChecked = true;
			for ( Loop = 1; Loop <= NumCoolingPanels; ++Loop ) {
				if ( CheckZoneEquipmentList( cCMO_CoolingPanel_Simple, CoolingPanel( Loop ).EquipID ) ) continue;
				ShowSevereError( "InitCoolingPanel: Unit=[" + cCMO_CoolingPanel_Simple + ',' + CoolingPanel( Loop ).EquipID + "] is not on any ZoneHVAC:EquipmentList.  It will not be simulated." );
			}
		}

		if ( SetLoopIndexFlag( CoolingPanelNum ) ) {
			if ( allocated( PlantLoop ) ) {
				errFlag = false;
				ScanPlantLoopsForObject( CoolingPanel( CoolingPanelNum ).EquipID, CoolingPanel( CoolingPanelNum ).EquipType, CoolingPanel( CoolingPanelNum ).LoopNum, CoolingPanel( CoolingPanelNum ).LoopSideNum, CoolingPanel( CoolingPanelNum ).BranchNum, CoolingPanel( CoolingPanelNum ).CompNum, _, _, _, _, _, errFlag );
				if ( errFlag ) {
					ShowFatalError( "InitCoolingPanel: Program terminated for previous conditions." );
				}
				SetLoopIndexFlag( CoolingPanelNum ) = false;
			}
		}

		if ( ! SysSizingCalc ) {
			if ( MySizeFlagCoolPanel( CoolingPanelNum ) && ! SetLoopIndexFlag( CoolingPanelNum ) ) {
				// for each cooling panel do the sizing once.
				SizeCoolingPanel( CoolingPanelNum );
				MySizeFlagCoolPanel( CoolingPanelNum ) = false;
				
				//set design mass flow rates
				if ( CoolingPanel( CoolingPanelNum ).WaterInletNode > 0 ) {
					rho = GetDensityGlycol( PlantLoop( CoolingPanel( CoolingPanelNum ).LoopNum ).FluidName, DataGlobals::CWInitConvTemp, PlantLoop( CoolingPanel( CoolingPanelNum ).LoopNum ).FluidIndex, RoutineName );
					CoolingPanel( CoolingPanelNum ).WaterMassFlowRateMax = rho * CoolingPanel( CoolingPanelNum ).WaterVolFlowRateMax;
					InitComponentNodes( 0.0, CoolingPanel( CoolingPanelNum ).WaterMassFlowRateMax, CoolingPanel( CoolingPanelNum ).WaterInletNode, CoolingPanel( CoolingPanelNum ).WaterOutletNode, CoolingPanel( CoolingPanelNum ).LoopNum, CoolingPanel( CoolingPanelNum ).LoopSideNum, CoolingPanel( CoolingPanelNum ).BranchNum, CoolingPanel( CoolingPanelNum ).CompNum );
				}
			}
		}
		
		
		// Do the Begin Environment initializations
		if ( BeginEnvrnFlag && MyEnvrnFlag( CoolingPanelNum ) ) {
			// Initialize
			WaterInletNode = CoolingPanel( CoolingPanelNum ).WaterInletNode;

			rho = GetDensityGlycol( PlantLoop( CoolingPanel( CoolingPanelNum ).LoopNum ).FluidName, InitConvTemp, PlantLoop( CoolingPanel( CoolingPanelNum ).LoopNum ).FluidIndex, RoutineName );

			CoolingPanel( CoolingPanelNum ).WaterMassFlowRateMax = rho * CoolingPanel( CoolingPanelNum ).WaterVolFlowRateMax;

			InitComponentNodes( 0.0, CoolingPanel( CoolingPanelNum ).WaterMassFlowRateMax, CoolingPanel( CoolingPanelNum ).WaterInletNode, CoolingPanel( CoolingPanelNum ).WaterOutletNode, CoolingPanel( CoolingPanelNum ).LoopNum, CoolingPanel( CoolingPanelNum ).LoopSideNum, CoolingPanel( CoolingPanelNum ).BranchNum, CoolingPanel( CoolingPanelNum ).CompNum );

			Node( WaterInletNode ).Temp = 7.0;

			Cp = GetSpecificHeatGlycol( PlantLoop( CoolingPanel( CoolingPanelNum ).LoopNum ).FluidName, Node( WaterInletNode ).Temp, PlantLoop( CoolingPanel( CoolingPanelNum ).LoopNum ).FluidIndex, RoutineName );

			Node( WaterInletNode ).Enthalpy = Cp * Node( WaterInletNode ).Temp;
			Node( WaterInletNode ).Quality = 0.0;
			Node( WaterInletNode ).Press = 0.0;
			Node( WaterInletNode ).HumRat = 0.0;

			ZeroSourceSumHATsurf = 0.0;
			CoolingPanelSource = 0.0;
			CoolingPanelSrcAvg = 0.0;
			LastCoolingPanelSrc = 0.0;
			LastSysTimeElapsed = 0.0;
			LastTimeStepSys = 0.0;

			MyEnvrnFlag( CoolingPanelNum ) = false;
		}

		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( CoolingPanelNum ) = true;
		}

		if ( BeginTimeStepFlag && FirstHVACIteration ) {
			ZoneNum = CoolingPanel( CoolingPanelNum ).ZonePtr;
			ZeroSourceSumHATsurf( ZoneNum ) = SumHATsurf( ZoneNum );
			CoolingPanelSrcAvg( CoolingPanelNum ) = 0.0;
			LastCoolingPanelSrc( CoolingPanelNum ) = 0.0;
			LastSysTimeElapsed( CoolingPanelNum ) = 0.0;
			LastTimeStepSys( CoolingPanelNum ) = 0.0;
		}

		// Do the every time step initializations
		WaterInletNode = CoolingPanel( CoolingPanelNum ).WaterInletNode;
		ZoneNode = ZoneEquipConfig( ControlledZoneNumSub ).ZoneNode;
		CoolingPanel( CoolingPanelNum ).WaterMassFlowRate = Node( WaterInletNode ).MassFlowRate;
		CoolingPanel( CoolingPanelNum ).WaterInletTemp = Node( WaterInletNode ).Temp;
		CoolingPanel( CoolingPanelNum ).WaterInletEnthalpy = Node( WaterInletNode ).Enthalpy;
		CoolingPanel( CoolingPanelNum ).TotPower = 0.0;
		CoolingPanel( CoolingPanelNum ).Power = 0.0;
		CoolingPanel( CoolingPanelNum ).ConvPower = 0.0;
		CoolingPanel( CoolingPanelNum ).RadPower = 0.0;
		CoolingPanel( CoolingPanelNum ).TotEnergy = 0.0;
		CoolingPanel( CoolingPanelNum ).Energy = 0.0;
		CoolingPanel( CoolingPanelNum ).ConvEnergy = 0.0;
		CoolingPanel( CoolingPanelNum ).RadEnergy = 0.0;

	}

	void
	SizeCoolingPanel(
		int const CoolingPanelNum
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   Sept 2016
		
		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine sizes the simple chilled ceiling panel.  The process used here
		// was derived from the low temperature radiant system model and adapted for
		// cooling only.
		
		using DataSizing::DataScalableCapSizingON;
		using DataSizing::CurZoneEqNum;
		using DataSizing::ZoneEqSizing;
		using DataSizing::ZoneSizingRunDone;
		using DataSizing::AutoSize;
		using DataSizing::CoolingDesignCapacity;
		using DataSizing::CapacityPerFloorArea;
		using DataSizing::FractionOfAutosizedCoolingCapacity;
		using DataSizing::DataConstantUsedForSizing;
		using DataSizing::DataFractionUsedForSizing;
		using DataSizing::FinalZoneSizing;
		using DataSizing::PlantSizData;
		using DataSizing::AutoVsHardSizingThreshold;
		using DataHVACGlobals::CoolingCapacitySizing;
		using DataHVACGlobals::AutoCalculateSizing;
		using DataHVACGlobals::SmallLoad;
		using DataPlant::PlantLoop;

		using ReportSizingManager::RequestSizing;
		using ReportSizingManager::ReportSizingOutput;
		using DataHeatBalance::Zone;
		using DataPlant::MyPlantSizingIndex;
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSpecificHeatGlycol;
		using General::RoundSigDigits;
		using PlantUtilities::RegisterPlantCompDesignFlow;

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "SizeCoolingPanel" );
		
		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		bool ErrorsFound( false ); // If errors detected in input
		std::string CompName; // component name
		std::string CompType; // component type
		bool IsAutoSize( false ); // Indicator to autosize
		Real64 DesCoilLoad; // design autosized or user specified capacity
		int SizingMethod; // Integer representation of sizing method name (e.g. CoolingCapacitySizing, HeatingCapacitySizing)
		int FieldNum = 1; // IDD numeric field number where input field description is found
		bool PrintFlag; // TRUE when sizing information is reported in the eio file
		std::string SizingString; // input field sizing description (e.g., Nominal Capacity)
		Real64 TempSize; // autosized value of coil input field
		int CapSizingMethod( 0 ); // capacity sizing methods (HeatingDesignCapacity, CapacityPerFloorArea, FractionOfAutosizedCoolingCapacity, and FractionOfAutosizedHeatingCapacity )
		int PltSizCoolNum( 0 ); // index of plant sizing object for 1st cooling loop
		Real64 rho;
		Real64 Cp;
		Real64 WaterVolFlowMaxCoolDes( 0.0 ); // Design chilled water flow for reporting
		Real64 WaterVolFlowMaxCoolUser( 0.0 ); // User hard-sized chilled water flow for reporting
		
		DesCoilLoad = 0.0;
		DataScalableCapSizingON = false;
		
		CompType = "ZoneHVAC:CoolingPanel:RadiantConvective:Water";
		CompName = CoolingPanel( CoolingPanelNum ).EquipID;
		
		IsAutoSize = false;
		if ( CoolingPanel( CoolingPanelNum ).ScaledCoolingCapacity == AutoSize ) {
			IsAutoSize = true;
		}
		
		if ( CurZoneEqNum > 0 ) {
			
			SizingMethod = CoolingCapacitySizing;
			FieldNum = 4;
			PrintFlag = true;
			SizingString = CoolingPanelSysNumericFields( CoolingPanelNum ).FieldNames( FieldNum ) + " [W]";
			CapSizingMethod = CoolingPanel( CoolingPanelNum ).CoolingCapMethod;
			ZoneEqSizing( CurZoneEqNum ).SizingMethod( SizingMethod ) = CapSizingMethod;
			
			if ( !IsAutoSize && !ZoneSizingRunDone ) { // simulation continue
				if ( CapSizingMethod == CoolingDesignCapacity && CoolingPanel( CoolingPanelNum ).ScaledCoolingCapacity > 0.0 ) {
					TempSize = CoolingPanel( CoolingPanelNum ).ScaledCoolingCapacity;
					RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
					DesCoilLoad = TempSize;
				} else if ( CapSizingMethod == CapacityPerFloorArea ) {
					DataScalableCapSizingON = true;
					TempSize = CoolingPanel( CoolingPanelNum ).ScaledCoolingCapacity * Zone( CoolingPanel( CoolingPanelNum ).ZonePtr ).FloorArea;
					RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
					DesCoilLoad = TempSize;
					DataScalableCapSizingON = false;
				} else if ( CapSizingMethod == FractionOfAutosizedCoolingCapacity ) {
					if ( CoolingPanel( CoolingPanelNum ).WaterVolFlowRateMax == AutoSize ) {
						ShowSevereError( RoutineName + ": auto-sizing cannot be done for " + CompType + " = " + CoolingPanel( CoolingPanelNum ).EquipID + "\"." );
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
							DataConstantUsedForSizing = FinalZoneSizing( CurZoneEqNum ).NonAirSysDesCoolLoad;
							DataFractionUsedForSizing = 1.0;
						}
						if ( CoolingPanel( CoolingPanelNum ).ScaledCoolingCapacity == AutoSize ) {
							TempSize = AutoSize;
						} else {
							TempSize = CoolingPanel( CoolingPanelNum ).ScaledCoolingCapacity;
						}
					} else if ( CapSizingMethod == CapacityPerFloorArea ) {
						if ( ZoneSizingRunDone ) {
							CheckZoneSizing( CompType, CompName );
							ZoneEqSizing( CurZoneEqNum ).CoolingCapacity = true;
							ZoneEqSizing( CurZoneEqNum ).DesCoolingLoad = FinalZoneSizing( CurZoneEqNum ).NonAirSysDesCoolLoad;
						}
						TempSize = CoolingPanel( CoolingPanelNum ).ScaledCoolingCapacity * Zone( CoolingPanel( CoolingPanelNum ).ZonePtr ).FloorArea;
						DataScalableCapSizingON = true;
					} else if ( CapSizingMethod == FractionOfAutosizedCoolingCapacity ) {
						CheckZoneSizing( CompType, CompName );
						ZoneEqSizing( CurZoneEqNum ).CoolingCapacity = true;
						ZoneEqSizing( CurZoneEqNum ).DesCoolingLoad = FinalZoneSizing( CurZoneEqNum ).NonAirSysDesCoolLoad;
						TempSize = ZoneEqSizing( CurZoneEqNum ).DesCoolingLoad * CoolingPanel( CoolingPanelNum ).ScaledCoolingCapacity;
						DataScalableCapSizingON = true;
						
					} else {
						TempSize = CoolingPanel( CoolingPanelNum ).ScaledCoolingCapacity;
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
			CoolingPanel( CoolingPanelNum ).ScaledCoolingCapacity = DesCoilLoad;
		}
		
		IsAutoSize = false;
		if ( CoolingPanel( CoolingPanelNum ).WaterVolFlowRateMax == AutoSize ) {
			IsAutoSize = true;
		}
		if ( CurZoneEqNum > 0 ) {
			if ( ! IsAutoSize && ! ZoneSizingRunDone ) { // simulation continue
				if ( CoolingPanel( CoolingPanelNum ).WaterVolFlowRateMax > 0.0 ) {
					ReportSizingOutput( CompType, CoolingPanel( CoolingPanelNum ).EquipID, "User-Specified Maximum Cold Water Flow [m3/s]", CoolingPanel( CoolingPanelNum ).WaterVolFlowRateMax );
				}
			} else { // Autosize or hard-size with sizing run
				if ( CoolingPanel( CoolingPanelNum ).WaterInletNode > 0 && CoolingPanel( CoolingPanelNum ).WaterOutletNode > 0 ) {
					PltSizCoolNum = MyPlantSizingIndex( CompType, CoolingPanel( CoolingPanelNum ).EquipID, CoolingPanel( CoolingPanelNum ).WaterInletNode, CoolingPanel( CoolingPanelNum ).WaterOutletNode, ErrorsFound );
					if ( PltSizCoolNum > 0 ) {
						if ( DesCoilLoad >= SmallLoad ) {
							rho = GetDensityGlycol( PlantLoop( CoolingPanel( CoolingPanelNum ).LoopNum ).FluidName, 5., PlantLoop( CoolingPanel( CoolingPanelNum ).LoopNum ).FluidIndex, RoutineName );
							Cp = GetSpecificHeatGlycol( PlantLoop( CoolingPanel( CoolingPanelNum ).LoopNum ).FluidName, 5.0, PlantLoop( CoolingPanel( CoolingPanelNum ).LoopNum ).FluidIndex, RoutineName );
							WaterVolFlowMaxCoolDes = DesCoilLoad / ( PlantSizData( PltSizCoolNum ).DeltaT * Cp * rho );
						} else {
							WaterVolFlowMaxCoolDes = 0.0;
						}
					} else {
						ShowSevereError( "Autosizing of water flow requires a cooling loop Sizing:Plant object" );
						ShowContinueError( "Occurs in ZoneHVAC:CoolingPanel:RadiantConvective:Water Object=" + CoolingPanel( CoolingPanelNum ).EquipID );
						ErrorsFound = true;
					}
				}
				
				if ( IsAutoSize ) {
					CoolingPanel( CoolingPanelNum ).WaterVolFlowRateMax = WaterVolFlowMaxCoolDes;
					ReportSizingOutput( CompType, CoolingPanel( CoolingPanelNum ).EquipID, "Design Size Maximum Cold Water Flow [m3/s]", WaterVolFlowMaxCoolDes );
				} else { // hard-size with sizing data
					if ( CoolingPanel( CoolingPanelNum ).WaterVolFlowRateMax > 0.0 && WaterVolFlowMaxCoolDes > 0.0 ) {
						WaterVolFlowMaxCoolUser = CoolingPanel( CoolingPanelNum ).WaterVolFlowRateMax;
						ReportSizingOutput( CompType, CoolingPanel( CoolingPanelNum ).EquipID, "Design Size Maximum Cold Water Flow [m3/s]", WaterVolFlowMaxCoolDes, "User-Specified Maximum Cold Water Flow [m3/s]", WaterVolFlowMaxCoolUser );
						if ( DisplayExtraWarnings ) {
							if ( ( std::abs( WaterVolFlowMaxCoolDes - WaterVolFlowMaxCoolUser ) / WaterVolFlowMaxCoolUser ) > AutoVsHardSizingThreshold ) {
								ShowMessage( "SizeCoolingPanel: Potential issue with equipment sizing for ZoneHVAC:CoolingPanel:RadiantConvective:Water = \"" + CoolingPanel( CoolingPanelNum ).EquipID + "\"." );
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
		
		RegisterPlantCompDesignFlow( CoolingPanel( CoolingPanelNum ).WaterInletNode, CoolingPanel( CoolingPanelNum ).WaterVolFlowRateMax );

		bool SizeCoolingPanelUASuccess;
		SizeCoolingPanelUASuccess = SizeCoolingPanelUA( CoolingPanelNum );
		if ( ! SizeCoolingPanelUASuccess ) ShowFatalError( "SizeCoolingPanelUA: Program terminated for previous conditions." );

	}

	bool
	SizeCoolingPanelUA(
		int const CoolingPanelNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   June 2017
		
		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine sizes UA value for the simple chilled ceiling panel.

		// Return value
		bool SizeCoolingPanelUA;
		
		// These initializations are mainly the calculation of the UA value for the heat exchanger formulation of the simple cooling panel
		Real64 Cp;
		Real64 MDot;
		Real64 MDotXCp;
		Real64 Qrated;
		Real64 Tinletr;
		Real64 Tzoner;
		Real64 RatCapToTheoMax; // Ratio of unit capacity to theoretical maximum output based on rated parameters

		SizeCoolingPanelUA = true;
		Cp = 4120.0; // Just an approximation, don't need to get an exact number
		MDot = CoolingPanel( CoolingPanelNum ).RatedWaterFlowRate;
		MDotXCp = Cp * MDot;
		Qrated = CoolingPanel( CoolingPanelNum ).ScaledCoolingCapacity;
		Tinletr = CoolingPanel( CoolingPanelNum ).RatedWaterTemp;
		Tzoner = CoolingPanel( CoolingPanelNum ).RatedZoneAirTemp;
		if ( std::abs( Tinletr - Tzoner ) < 0.5 ) {
			RatCapToTheoMax = std::abs(Qrated) / ( MDotXCp * 0.5 ); // Avoid a divide by zero error
		} else {
			RatCapToTheoMax = std::abs(Qrated) / ( MDotXCp * std::abs( Tinletr - Tzoner ) );
		}
		if ( ( RatCapToTheoMax < 1.1 ) && ( RatCapToTheoMax > 0.9999 ) ) {
			// close to unity with some graciousness given in case the approximation of Cp causes a problem
			RatCapToTheoMax = 0.9999;
		} else if (RatCapToTheoMax >= 1.1 ) {
			ShowSevereError( "SizeCoolingPanelUA: Unit=[" + cCMO_CoolingPanel_Simple + ',' + CoolingPanel( CoolingPanelNum ).EquipID + "] has a cooling capacity that is greater than the maximum possible value." );
			ShowContinueError( "The result of this is that a UA value is impossible to calculate." );
			ShowContinueError( "Check the rated input for temperatures, flow, and capacity for this unit." );
			ShowContinueError( "The ratio of the capacity to the rated theoretical maximum must be less than unity." );
			ShowContinueError( "The most likely cause for this is probably either the capacity (whether autosized or hardwired) being too high, the rated flow being too low, rated temperatures being too close to each other, or all of those reasons." );
			ShowContinueError( "Compare the rated capacity in your input to the product of the rated mass flow rate, Cp of water, and the difference between the rated temperatures." );
			ShowContinueError( "If the rated capacity is higher than this product, then the cooling panel would violate the Second Law of Thermodynamics." );
			SizeCoolingPanelUA = false;
			CoolingPanel( CoolingPanelNum ).UA = 1.0;
		}
		if ( Tinletr >= Tzoner ) {
			ShowSevereError( "SizeCoolingPanelUA: Unit=[" + cCMO_CoolingPanel_Simple + ',' + CoolingPanel( CoolingPanelNum ).EquipID + "] has a rated water temperature that is higher than the rated zone temperature." );
			ShowContinueError( "Such a situation would not lead to cooling and thus the rated water or zone temperature or both should be adjusted." );
			SizeCoolingPanelUA = false;
			CoolingPanel( CoolingPanelNum ).UA = 1.0;
		} else {
			CoolingPanel( CoolingPanelNum ).UA = -MDotXCp * log( 1.0 - RatCapToTheoMax );
			if ( CoolingPanel( CoolingPanelNum ).UA <= 0.0 ) {
				ShowSevereError( "SizeCoolingPanelUA: Unit=[" + cCMO_CoolingPanel_Simple + ',' + CoolingPanel( CoolingPanelNum ).EquipID + "] has a zero or negative calculated UA value." );
				ShowContinueError( "This is not allowed.  Please check the rated input parameters for this device to ensure that the values are correct." );
				SizeCoolingPanelUA = false;
			}
		}

		return SizeCoolingPanelUA;

	}

	void
	CalcCoolingPanel(
		int & CoolingPanelNum
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   Sept 2014

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine calculates both the convective and radiant heat transfer rate
		// for the simple cooling panel.  The process used here was derived from the hot
		// water baseboard radiant/convective heater and adapted for cooling.

		// REFERENCES:
		// Existing code for hot water baseboard models (radiant-convective variety)
		// Incropera and DeWitt, Fundamentals of Heat and Mass Transfer

		// Using/Aliasing
		using DataLoopNode::Node;
		using ScheduleManager::GetCurrentScheduleValue;
		using DataZoneEnergyDemands::ZoneSysEnergyDemand;
		using DataZoneEnergyDemands::CurDeadBandOrSetback;
		using DataHeatBalance::MRT;
		using DataHeatBalFanSys::MAT;
		using PlantUtilities::SetComponentFlowRate;
		using DataHeatBalFanSys::ZoneAirHumRat;
		using DataEnvironment::OutBaroPress;
		using General::RoundSigDigits;
		using DataHVACGlobals::SmallLoad;
		using Psychrometrics::PsyTdpFnWPb;
		using DataPlant::PlantLoop;
		using FluidProperties::GetSpecificHeatGlycol;

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const MinFrac( 0.0005 ); // Minimum fraction that delivers radiant heats to surfaces
		int const Maxiter( 20 ); // Maximum number of iterations to achieve tolerance
		Real64 const IterTol( 0.005 ); // Tolerance of 0.5%
		static std::string const RoutineName( "CalcCoolingPanel" );

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ZoneNum;
		int iter;
		Real64 RadHeat;
		Real64 CoolingPanelCool;
		Real64 WaterInletTemp;
		Real64 WaterOutletTemp;
		Real64 WaterMassFlowRate;
		Real64 WaterMassFlowRateMax;
		Real64 CapacitanceWater;
		Real64 NTU;
		Real64 Effectiveness;
		Real64 QZnReq;
		Real64 Cp;
		Real64 Tzone;
		Real64 Xr;
		Real64 MCpEpsAct;
		Real64 MCpEpsLow;
		Real64 MCpEpsHigh;
		Real64 MdotLow;
		Real64 MdotHigh;
		Real64 FracGuess;
		Real64 MdotGuess;
		Real64 MCpEpsGuess;
		Real64 ControlTemp;
		Real64 SetPointTemp;
		Real64 OffTempCool;
		Real64 FullOnTempCool;
		Real64 MassFlowFrac;
		Real64 DewPointTemp;
		Real64 LoadMet;
		bool CoolingPanelOn;
		bool ModifiedWaterInletTemp;

		ModifiedWaterInletTemp = false;
		ZoneNum = CoolingPanel( CoolingPanelNum ).ZonePtr;
		QZnReq = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToCoolSP;
		WaterInletTemp = CoolingPanel( CoolingPanelNum ).WaterInletTemp;
		WaterOutletTemp = WaterInletTemp;
		WaterMassFlowRateMax = CoolingPanel( CoolingPanelNum ).WaterMassFlowRateMax;
		Xr = CoolingPanel( CoolingPanelNum ).FracRadiant;

		if ( GetCurrentScheduleValue( CoolingPanel( CoolingPanelNum ).SchedPtr ) > 0 ) {
			CoolingPanelOn = true;
		} else {
			CoolingPanelOn = false;
		}
		// Calculate the "zone" temperature for determining the output of the cooling panel
		Tzone = Xr * MRT( ZoneNum ) + ( (1.0 - Xr ) * MAT( ZoneNum ) );

		// Logical controls: if the WaterInletTemperature is higher than Tzone, do not run the panel
		if ( WaterInletTemp >= Tzone ) CoolingPanelOn = false;
		
		// Condensation Controls based on dewpoint temperature of the zone.
		// The assumption here is that condensation might take place if the inlet water temperature
		// is below the dewpoint temperature of the space.  This assumption is made because we are
		// probably dealing with a metal panel and the surface temperature of the panel will be very
		// close to the inlet water temperature in certain places.  Thus, if the water inlet temperature
		// is below the dewpoint temperature, then we might have condensation.  We need to deal with this
		// possibility based on the user selected method.  The good news here is that we don't have to
		// iterate like in the low temperature radiant systems because the inlet water condition is known
		// not calculated.  So, we can deal with this upfront rather than after calculation and then more
		// iteration.
		DewPointTemp = PsyTdpFnWPb( ZoneAirHumRat( ZoneNum ), OutBaroPress );
		
		if ( WaterInletTemp < ( DewPointTemp + CoolingPanel( CoolingPanelNum ).CondDewPtDeltaT ) && ( CoolingPanelOn ) ) {
			
			// Condensation is possible so invoke the three possible ways of handling this based on the user's choice...
			
			if ( CoolingPanel( CoolingPanelNum ).CondCtrlType == CondCtrlNone) {
				// Condensation control is "off" which means don't do anything, simply let it run and ignore condensation
			} else if ( CoolingPanel( CoolingPanelNum ).CondCtrlType == CondCtrlSimpleOff) {
				// For "simple off", simply turn the simple cooling panel off to avoid condensation
				WaterMassFlowRate = 0.0;
				CoolingPanelOn = false;
				// Produce a warning message so that user knows the system was shut-off due to potential for condensation
				if ( ! WarmupFlag ) {
					if ( CoolingPanel( CoolingPanelNum ).CondErrIndex == 0 ) { // allow errors up to number of radiant systems
						ShowWarningMessage( cCMO_CoolingPanel_Simple + " [" + CoolingPanel( CoolingPanelNum ).EquipID + "] inlet water temperature below dew-point temperature--potential for condensation exists" );
						ShowContinueError( "Flow to the simple cooling panel will be shut-off to avoid condensation" );
						ShowContinueError( "Water inlet temperature = " + RoundSigDigits( WaterInletTemp, 2 ) );
						ShowContinueError( "Zone dew-point temperature + safety delta T= " + RoundSigDigits( DewPointTemp + CoolingPanel( CoolingPanelNum ).CondDewPtDeltaT, 2 ) );
						ShowContinueErrorTimeStamp( "" );
						ShowContinueError( "Note that a " + RoundSigDigits( CoolingPanel( CoolingPanelNum ).CondDewPtDeltaT, 4 ) + " C safety was chosen in the input for the shut-off criteria" );
					}
					ShowRecurringWarningErrorAtEnd( cCMO_CoolingPanel_Simple + " [" + CoolingPanel( CoolingPanelNum ).EquipID + "] condensation shut-off occurrence continues.", CoolingPanel( CoolingPanelNum ).CondErrIndex, DewPointTemp, DewPointTemp, _, "C", "C" );
				}

			} else if ( CoolingPanel( CoolingPanelNum ).CondCtrlType == CondCtrlVariedOff) {
				// Varied off is the most complex because it tries to run by reducing the inlet temperature
				// As a result of this, there is some bypass/recirculation that has to take place.
				// We might not have enough flow rate to meet whatever load we have, but at least
				// the system is still running at some partial load and avoiding condensation.
				WaterInletTemp = DewPointTemp + CoolingPanel( CoolingPanelNum ).CondDewPtDeltaT;
				ModifiedWaterInletTemp = true;
			}
		}

		// The next IF block is to find the mass flow rate based on what type of control the user has requested.  Load based controls
		// vary the flow to meet the zone load calculated by the user-defined thermostat.  Temperature based controls vary the flow
		// based on a comparison between the control temperature and the setpoint schedule and throttling range.
		
		if ( ( CoolingPanel( CoolingPanelNum ).ControlType == ZoneTotalLoadControl ) || ( CoolingPanel( CoolingPanelNum ).ControlType == ZoneConvectiveLoadControl ) ) {
	
			if ( QZnReq < -SmallLoad && ! CurDeadBandOrSetback( ZoneNum ) && ( CoolingPanelOn ) ) {

				Cp = GetSpecificHeatGlycol( PlantLoop( CoolingPanel( CoolingPanelNum ).LoopNum ).FluidName, WaterInletTemp, PlantLoop( CoolingPanel( CoolingPanelNum ).LoopNum ).FluidIndex, RoutineName );

				// Find the actual load: this parameter modifies what the response of the system should be.  For total load control, the system tries to
				// meet the QZnReq.  For convective load control, the convective output of the device equals QZnReq which means that the load on the
				// panel is higher as is its output.  Total load control will miss the setpoint temperature but will likely get there with time.
				// Convective load control will hit the setpoint short term better but will result in overcooling in the long run probably.
				if ( CoolingPanel( CoolingPanelNum).ControlType == ZoneConvectiveLoadControl ) {
					QZnReq = QZnReq / CoolingPanel( CoolingPanelNum ).FracConvect;
				}
				
				// Now for a small amount of iteration.  Try to find the value of mass flow rate that will come the closest to giving
				// the proper value for MCpEpsAct.  Limit iterations to avoid too much time wasting.
				MCpEpsAct = QZnReq / ( WaterInletTemp - Tzone );
				MCpEpsLow = 0.0;
				MdotLow = 0.0;
				MCpEpsHigh = WaterMassFlowRateMax * Cp * ( 1.0 - exp( -CoolingPanel( CoolingPanelNum ).UA / ( WaterMassFlowRateMax * Cp ) ) );
				MdotHigh = WaterMassFlowRateMax;
				if ( MCpEpsAct <= MCpEpsLow ) {
					MCpEpsAct = MCpEpsLow;
					WaterMassFlowRate = 0.0;
					Node( CoolingPanel( CoolingPanelNum ).WaterInletNode ).MassFlowRate = 0.0;
					CoolingPanelOn = false;
				} else if (MCpEpsAct >= MCpEpsHigh ) {
					MCpEpsAct = MCpEpsHigh;
					WaterMassFlowRate = WaterMassFlowRateMax;
					Node( CoolingPanel( CoolingPanelNum ).WaterInletNode ).MassFlowRate = WaterMassFlowRateMax;
				} else {
					for ( iter = 1; iter <= Maxiter; ++iter ) {
						FracGuess = ( MCpEpsAct - MCpEpsLow ) / ( MCpEpsHigh - MCpEpsLow );
						MdotGuess = MdotHigh * FracGuess;
						MCpEpsGuess = MdotGuess * Cp * ( 1.0 - exp( -CoolingPanel( CoolingPanelNum ).UA / ( MdotGuess * Cp ) ) );
						if ( MCpEpsGuess <= MCpEpsAct ) {
							MCpEpsLow = MCpEpsGuess;
							MdotLow = MdotGuess;
						} else { // MCpEpsGuess > MCpEpsAct
							MCpEpsHigh = MCpEpsGuess;
							MdotHigh = MdotGuess;
						}
						if ( ( (MCpEpsAct - MCpEpsGuess) / MCpEpsAct ) <= IterTol ) {
							WaterMassFlowRate = MdotGuess;
							Node( CoolingPanel( CoolingPanelNum ).WaterInletNode ).MassFlowRate = WaterMassFlowRate;
							break;
						}
					}
				}
				
			} else {
				CoolingPanelOn = false;
			}

		} else { // temperature control rather than zone load control
			
			if ( CoolingPanelOn ) {
			
				SetCoolingPanelControlTemp( ControlTemp, CoolingPanelNum, ZoneNum );
				
				SetPointTemp = GetCurrentScheduleValue( CoolingPanel( CoolingPanelNum ).ColdSetptSchedPtr );
				OffTempCool = SetPointTemp - 0.5 * CoolingPanel( CoolingPanelNum ).ColdThrottlRange;
				FullOnTempCool = SetPointTemp + 0.5 * CoolingPanel( CoolingPanelNum ).ColdThrottlRange;

				if ( ControlTemp <= OffTempCool ) {
					MassFlowFrac = 0.0;
					CoolingPanelOn = false;
				} else if ( ControlTemp >= FullOnTempCool ) {
					MassFlowFrac = 1.0;
				} else {
					MassFlowFrac = ( ControlTemp - OffTempCool ) / CoolingPanel( CoolingPanelNum ).ColdThrottlRange;
					if ( MassFlowFrac < MinFrac ) MassFlowFrac = MinFrac;
				}

				WaterMassFlowRate = MassFlowFrac * WaterMassFlowRateMax;

			}
			
		}
		
		if ( CoolingPanelOn ) {
			SetComponentFlowRate( WaterMassFlowRate, CoolingPanel( CoolingPanelNum ).WaterInletNode, CoolingPanel( CoolingPanelNum ).WaterOutletNode, CoolingPanel( CoolingPanelNum ).LoopNum, CoolingPanel( CoolingPanelNum ).LoopSideNum, CoolingPanel( CoolingPanelNum ).BranchNum, CoolingPanel( CoolingPanelNum ).CompNum );
			if ( WaterMassFlowRate <= 0.0 ) CoolingPanelOn = false;
		}
		
		if ( CoolingPanelOn ) {
			// Now simulate the system...
			Cp = GetSpecificHeatGlycol( PlantLoop( CoolingPanel( CoolingPanelNum ).LoopNum ).FluidName, WaterInletTemp, PlantLoop( CoolingPanel( CoolingPanelNum ).LoopNum ).FluidIndex, RoutineName );
			Effectiveness = 1.0 - exp( -CoolingPanel( CoolingPanelNum ).UA / (WaterMassFlowRate * Cp) );
			if ( Effectiveness <= 0.0 ) {
				Effectiveness = 0.0;
			} else if ( Effectiveness >= 1.0 ) {
				Effectiveness = 1.0;
			}
			CoolingPanelCool = ( Effectiveness ) * WaterMassFlowRate * Cp * ( WaterInletTemp - Tzone );
			WaterOutletTemp = CoolingPanel( CoolingPanelNum ).WaterInletTemp - ( CoolingPanelCool / ( WaterMassFlowRate * Cp ) );
			RadHeat = CoolingPanelCool * CoolingPanel( CoolingPanelNum ).FracRadiant;
			CoolingPanelSource( CoolingPanelNum ) = RadHeat;
			
			if ( CoolingPanel( CoolingPanelNum ).FracRadiant <= MinFrac ) {
				LoadMet = CoolingPanelCool;
			} else {
				
				// Now, distribute the radiant energy of all systems to the appropriate surfaces, to people, and the air
				DistributeCoolingPanelRadGains();
				// Now "simulate" the system by recalculating the heat balances
				HeatBalanceSurfaceManager::CalcHeatBalanceOutsideSurf( ZoneNum );
				
				HeatBalanceSurfaceManager::CalcHeatBalanceInsideSurf( ZoneNum );
				
				// Here an assumption is made regarding radiant heat transfer to people.
				// While the radiant heat transfer to people array will be used by the thermal comfort
				// routines, the energy transfer to people would get lost from the perspective
				// of the heat balance.  So, to avoid this net loss of energy which clearly
				// gets added to the zones, we must account for it somehow.  This assumption
				// that all energy radiated to people is converted to convective energy is
				// not very precise, but at least it conserves energy. The system impact to heat balance
				// should include this.
				LoadMet = ( SumHATsurf( ZoneNum ) - ZeroSourceSumHATsurf( ZoneNum ) ) + ( CoolingPanelCool * CoolingPanel( CoolingPanelNum ).FracConvect ) + ( RadHeat * CoolingPanel( CoolingPanelNum ).FracDistribPerson );
			}
			CoolingPanel( CoolingPanelNum ).WaterOutletEnthalpy = CoolingPanel( CoolingPanelNum ).WaterInletEnthalpy - CoolingPanelCool / WaterMassFlowRate;
			
			
		} else { // cooling panel off
			CapacitanceWater = 0.0;
			NTU = 0.0;
			Effectiveness = 0.0;
			WaterOutletTemp = WaterInletTemp;
			CoolingPanelCool = 0.0;
			LoadMet = 0.0;
			RadHeat = 0.0;
			WaterMassFlowRate = 0.0;
			CoolingPanelSource( CoolingPanelNum ) = 0.0;
			CoolingPanel( CoolingPanelNum ).WaterOutletEnthalpy = CoolingPanel( CoolingPanelNum ).WaterInletEnthalpy;
		}
		
		CoolingPanel( CoolingPanelNum ).WaterOutletTemp = WaterOutletTemp;
		CoolingPanel( CoolingPanelNum ).WaterMassFlowRate = WaterMassFlowRate;
		CoolingPanel( CoolingPanelNum ).TotPower = LoadMet;
		CoolingPanel( CoolingPanelNum ).Power = CoolingPanelCool;
		CoolingPanel( CoolingPanelNum ).ConvPower = CoolingPanelCool - RadHeat;
		CoolingPanel( CoolingPanelNum ).RadPower = RadHeat;

	}

	void
	SetCoolingPanelControlTemp(
		Real64 & ControlTemp,
		int const CoolingPanelNum,
		int const ZoneNum
	)
	{
	
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   July 2016
		
		// METHODOLOGY EMPLOYED:
		// This subroutine sets the control temperature for the simple cooling panel.
		
		// Using/Aliasing
		using DataHeatBalance::MRT;
		using DataHeatBalance::Zone;
		using DataHeatBalFanSys::MAT;

		{ auto const SELECT_CASE_var( CoolingPanel( CoolingPanelNum ).ControlType );
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
				ShowSevereError( "Illegal control type in cooling panel system: " + CoolingPanel( CoolingPanelNum ).EquipID );
				ShowFatalError( "Preceding condition causes termination." );
			}
		}

		
	}
	
	void
	UpdateCoolingPanel( int const CoolingPanelNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   Sept 2014
		//                      February 2001
		//       MODIFIED       Aug 2007 Daeho Kang (Add the update of radiant source)

		// REFERENCES:
		// Existing code for hot water baseboard models (radiant-convective variety)

		// Using/Aliasing
		using DataLoopNode::Node;
		using DataGlobals::TimeStepZone;
		using DataHVACGlobals::TimeStepSys;
		using DataGlobals::BeginEnvrnFlag;
		using PlantUtilities::SafeCopyPlantNode;
		using DataHVACGlobals::SysTimeElapsed;

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int WaterInletNode;
		int WaterOutletNode;
		static int Iter( 0 );
		static bool MyEnvrnFlag( true );

		if ( BeginEnvrnFlag && MyEnvrnFlag ) {
			Iter = 0;
			MyEnvrnFlag = false;
		}
		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag = true;
		}

		// First, update the running average if necessary...
		if ( LastSysTimeElapsed( CoolingPanelNum ) == SysTimeElapsed ) {
			CoolingPanelSrcAvg( CoolingPanelNum ) -= LastCoolingPanelSrc( CoolingPanelNum ) * LastTimeStepSys( CoolingPanelNum ) / TimeStepZone;
		}
		// Update the running average and the "last" values with the current values of the appropriate variables
		CoolingPanelSrcAvg( CoolingPanelNum ) += CoolingPanelSource( CoolingPanelNum ) * TimeStepSys / TimeStepZone;

		LastCoolingPanelSrc( CoolingPanelNum ) = CoolingPanelSource( CoolingPanelNum );
		LastSysTimeElapsed( CoolingPanelNum ) = SysTimeElapsed;
		LastTimeStepSys( CoolingPanelNum ) = TimeStepSys;

		WaterInletNode = CoolingPanel( CoolingPanelNum ).WaterInletNode;
		WaterOutletNode = CoolingPanel( CoolingPanelNum ).WaterOutletNode;

		// Set the outlet water nodes for the panel
		SafeCopyPlantNode( WaterInletNode, WaterOutletNode );
		Node( WaterOutletNode ).Temp = CoolingPanel( CoolingPanelNum ).WaterOutletTemp;
		Node( WaterOutletNode ).Enthalpy = CoolingPanel( CoolingPanelNum ).WaterOutletEnthalpy;
		Node( WaterInletNode ).MassFlowRate = CoolingPanel( CoolingPanelNum ).WaterMassFlowRate;
		Node( WaterOutletNode ).MassFlowRate = CoolingPanel( CoolingPanelNum ).WaterMassFlowRate;
		Node( WaterInletNode ).MassFlowRateMax = CoolingPanel( CoolingPanelNum ).WaterMassFlowRateMax;
		Node( WaterOutletNode ).MassFlowRateMax = CoolingPanel( CoolingPanelNum ).WaterMassFlowRateMax;

	}

	void
	UpdateCoolingPanelSourceValAvg( bool & CoolingPanelSysOn ) // .TRUE. if the radiant system has run this zone time step
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   Sept 2014

		// PURPOSE OF THIS SUBROUTINE:
		// To transfer the average value of the heat source over the entire
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
		// Existing code for hot water baseboard models (radiant-convective variety)

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int CoolingPanelNum; // DO loop counter for surface index

		// FLOW:
		CoolingPanelSysOn = false;

		// If this was never allocated, then there are no radiant systems in this input file (just RETURN)
		if ( ! allocated( CoolingPanelSrcAvg ) ) return;

		// If it was allocated, then we have to check to see if this was running at all...
		for ( CoolingPanelNum = 1; CoolingPanelNum <= NumCoolingPanels; ++CoolingPanelNum ) {
			if ( CoolingPanelSrcAvg( CoolingPanelNum ) != 0.0 ) {
				CoolingPanelSysOn = true;
				break; //DO loop
			}
		}

		CoolingPanelSource = CoolingPanelSrcAvg;

		DistributeCoolingPanelRadGains(); // CoolingPanelRadSource has been modified so we need to redistribute gains

	}

	void
	DistributeCoolingPanelRadGains()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   Sept 2014

		// PURPOSE OF THIS SUBROUTINE:
		// To distribute the gains from the hot water basebaord heater
		// as specified in the user input file.  This includes distribution
		// of long wavelength radiant gains to surfaces and "people."

		// METHODOLOGY EMPLOYED:
		// We must cycle through all of the radiant systems because each
		// surface could feel the effect of more than one radiant system.
		// Note that the energy radiated to people is assumed to affect them
		// but them it is assumed to be convected to the air.

		// REFERENCES:
		// Existing code for hot water baseboard models (radiant-convective variety)
		
		// Using/Aliasing
		using General::RoundSigDigits;
		using DataHeatBalFanSys::QCoolingPanelToPerson;
		using DataHeatBalFanSys::QCoolingPanelSurf;
		using DataHeatBalFanSys::MaxRadHeatFlux;
		using DataSurfaces::Surface;

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const SmallestArea( 0.001 ); // Smallest area in meters squared (to avoid a divide by zero)

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int RadSurfNum; // Counter for surfaces receiving radiation from radiant heater
		int CoolingPanelNum; // Counter for the baseboard
		int SurfNum; // Pointer to the Surface derived type
		int ZoneNum; // Pointer to the Zone derived type
		Real64 ThisSurfIntensity; // temporary for W/m2 term for rad on a surface

		// FLOW:
		// Initialize arrays
		QCoolingPanelSurf = 0.0;
		QCoolingPanelToPerson = 0.0;

		for ( CoolingPanelNum = 1; CoolingPanelNum <= NumCoolingPanels; ++CoolingPanelNum ) {

			ZoneNum = CoolingPanel( CoolingPanelNum ).ZonePtr;
			if ( ZoneNum <= 0 ) continue;
			QCoolingPanelToPerson( ZoneNum ) += CoolingPanelSource( CoolingPanelNum ) * CoolingPanel( CoolingPanelNum ).FracDistribPerson;

			for ( RadSurfNum = 1; RadSurfNum <= CoolingPanel( CoolingPanelNum ).TotSurfToDistrib; ++RadSurfNum ) {
				SurfNum = CoolingPanel( CoolingPanelNum ).SurfacePtr( RadSurfNum );
				if ( Surface( SurfNum ).Area > SmallestArea ) {
					ThisSurfIntensity = ( CoolingPanelSource( CoolingPanelNum ) * CoolingPanel( CoolingPanelNum ).FracDistribToSurf( RadSurfNum ) / Surface( SurfNum ).Area );
					QCoolingPanelSurf( SurfNum ) += ThisSurfIntensity;
					// CR 8074, trap for excessive intensity (throws off surface balance )
					if ( ThisSurfIntensity > MaxRadHeatFlux ) {
						ShowSevereError( "DistributeCoolingPanelRadGains:  excessive thermal radiation heat flux intensity detected" );
						ShowContinueError( "Surface = " + Surface( SurfNum ).Name );
						ShowContinueError( "Surface area = " + RoundSigDigits( Surface( SurfNum ).Area, 3 ) + " [m2]" );
						ShowContinueError( "Occurs in " + cCMO_CoolingPanel_Simple + " = " + CoolingPanel( CoolingPanelNum ).EquipID );
						ShowContinueError( "Radiation intensity = " + RoundSigDigits( ThisSurfIntensity, 2 ) + " [W/m2]" );
						ShowContinueError( "Assign a larger surface area or more surfaces in " + cCMO_CoolingPanel_Simple );
						ShowFatalError( "DistributeCoolingPanelRadGains:  excessive thermal radiation heat flux intensity detected" );
					}
				} else {
					ShowSevereError( "DistributeCoolingPanelRadGains:  surface not large enough to receive thermal radiation heat flux" );
					ShowContinueError( "Surface = " + Surface( SurfNum ).Name );
					ShowContinueError( "Surface area = " + RoundSigDigits( Surface( SurfNum ).Area, 3 ) + " [m2]" );
					ShowContinueError( "Occurs in " + cCMO_CoolingPanel_Simple + " = " + CoolingPanel( CoolingPanelNum ).EquipID );
					ShowContinueError( "Assign a larger surface area or more surfaces in " + cCMO_CoolingPanel_Simple );
					ShowFatalError( "DistributeCoolingPanelRadGains:  surface not large enough to receive thermal radiation heat flux" );

				}
			}

		}

	}

	void
	ReportCoolingPanel( int const CoolingPanelNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   Aug 2014

		// REFERENCES:
		// Existing code for hot water baseboard models (radiant-convective variety)

		using DataHVACGlobals::TimeStepSys;
		
		CoolingPanel( CoolingPanelNum ).TotEnergy = CoolingPanel( CoolingPanelNum ).TotPower * TimeStepSys * SecInHour;
		CoolingPanel( CoolingPanelNum ).Energy = CoolingPanel( CoolingPanelNum ).Power * TimeStepSys * SecInHour;
		CoolingPanel( CoolingPanelNum ).ConvEnergy = CoolingPanel( CoolingPanelNum ).ConvPower * TimeStepSys * SecInHour;
		CoolingPanel( CoolingPanelNum ).RadEnergy = CoolingPanel( CoolingPanelNum ).RadPower * TimeStepSys * SecInHour;

	}

	Real64
	SumHATsurf( int const ZoneNum ) // Zone number
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   Aug 2014

		// PURPOSE OF THIS FUNCTION:
		// This function calculates the zone sum of Hc*Area*Tsurf.  It replaces the old SUMHAT.
		// The SumHATsurf code below is also in the CalcZoneSums subroutine in ZoneTempPredictorCorrector
		// and should be updated accordingly.

		// REFERENCES:
		// Existing code for hot water baseboard models (radiant-convective variety)
		
		// Using/Aliasing
		using DataSurfaces::Surface;
		using DataSurfaces::SurfaceWindow;
		using DataSurfaces::IntShadeOn;
		using DataSurfaces::IntBlindOn;
		using DataSurfaces::SurfaceClass_Window;
		using DataHeatBalance::Zone;
		using DataHeatBalance::HConvIn;
		using DataHeatBalSurface::TempSurfInTmp;

		// Return value
		Real64 SumHATsurf;

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
					// The area is the shade or blind area = the sum of the glazing area and the divider area (which is zero if no divider)
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

} // CoolingPanelSimple

} // EnergyPlus
