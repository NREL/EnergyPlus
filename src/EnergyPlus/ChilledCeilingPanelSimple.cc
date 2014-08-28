// C++ Headers
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/FArray.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <CoolingPanelRadiator.hh>
#include <BranchNodeConnections.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHeatBalSurface.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DataSurfaces.hh>
#include <DataZoneEnergyDemands.hh>
#include <DataZoneEquipment.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <GlobalNames.hh>
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

namespace CoolingPanelSimple {

	// Module -- (ref: Object: ZoneHVAC:CoolingPanel:RadiantConvective:Water)

	// Module containing the routines dealing with the simple (chilled ceiling) cooling panels

	// MODULE INFORMATION:
	//       AUTHOR         Rick Strand
	//       DATE WRITTEN   Aug 2014
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// The purpose of this module is to simulate simple chilled ceiling panels.  It is similar to
    // hot water radiant/convective baseboard units and the code for this model used that model as
    // a starting point.

	// METHODOLOGY EMPLOYED:

	// REFERENCES:
    // 1. Existing code for hot water baseboard models (radiant-convective variety)
    
	// OTHER NOTES:
	// na

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataGlobals;
	using DataPlant::PlantLoop;
	using DataPlant::TypeOf_CoolingPanel_Simple;
	using DataZoneEquipment::ZoneEquipInputsFilled;
	using DataZoneEquipment::CheckZoneEquipmentList;
	using DataZoneEquipment::ZoneEquipConfig;
	using DataHVACGlobals::SmallLoad;
	using DataHVACGlobals::TimeStepSys;
	using DataHVACGlobals::SysTimeElapsed;
	// Use statements for access to subroutines in other modules
	using Psychrometrics::PsyCpAirFnWTdb;
	using Psychrometrics::PsyRhoAirFnPbTdbW;
	using FluidProperties::GetDensityGlycol;
	using FluidProperties::GetSpecificHeatGlycol;
	using ReportSizingManager::ReportSizingOutput;

	// Data
	//MODULE PARAMETER DEFINITIONS

	std::string const cCMO_CoolingPanel_Simple( "ZoneHVAC:CoolingPanel:RadiantConvective:Water" );

	// DERIVED TYPE DEFINITIONS

	//MODULE VARIABLE DECLARATIONS:
	int NumCoolingPanels( 0 );
	FArray1D< Real64 > CoolingPanelSource; // Need to keep the last value in case we are still iterating
	FArray1D< Real64 > CoolingPanelSrcAvg; // Need to keep the last value in case we are still iterating
	FArray1D< Real64 > ZeroSourceSumHATsurf; // Equal to the SumHATsurf for all the walls in a zone with no source

	// Record keeping variables used to calculate CoolingPanelSrcAvg locally
	FArray1D< Real64 > LastCoolingPanelSrc; // Need to keep the last value in case we are still iterating
	FArray1D< Real64 > LastSysTimeElapsed; // Need to keep the last value in case we are still iterating
	FArray1D< Real64 > LastTimeStepSys; // Need to keep the last value in case we are still iterating
	FArray1D_bool CheckEquipName;
	FArray1D_bool SetLoopIndexFlag; // get loop number flag

	//SUBROUTINE SPECIFICATIONS FOR MODULE Simple Chilled Ceiling Panel

	// Object Data
	FArray1D< CoolingPanelParams > CoolingPanel;

	// Functions

	void
	SimCoolingPanelSimple(
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
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine simulates the simple cooling (chilled ceiling) panel.  It borrows heavily
        // from the hot water radiant-convective baseboard model code.

		// METHODOLOGY EMPLOYED:
		// See the hot water radiant-convective baseboard code.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataLoopNode::Node;
		using InputProcessor::FindItemInList;
		using General::TrimSigDigits;
		using ScheduleManager::GetCurrentScheduleValue;
		using DataZoneEnergyDemands::ZoneSysEnergyDemand;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const MaxIter( 30 );

		// INTERFACE BLOCK SPECIFICATIONS

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int CoolingPanelNum; // Index of unit in baseboard array
		static bool GetInputFlag( true ); // One time get input flag
		Real64 QZnReq; // Zone load not yet satisfied
		Real64 MaxWaterFlow;
		Real64 MinWaterFlow;

		if ( GetInputFlag ) {
			GetCoolingPanelInput();
			GetInputFlag = false;
		}

		// Find the correct Baseboard Equipment
		if ( CompIndex == 0 ) {
			CoolingPanelNum = FindItemInList( EquipName, CoolingPanel.EquipID(), NumCoolingPanels );
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

			QZnReq = ZoneSysEnergyDemand( ActualZoneNum ).RemainingOutputReqToHeatSP;

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
				ControlCompOutput( CoolingPanel( CoolingPanelNum ).EquipID, cCMO_CoolingPanel_Simple, CoolingPaneldNum, FirstHVACIteration, QZnReq, CoolingPanel( CoolingPanelNum ).WaterInletNode, MaxWaterFlow, MinWaterFlow, CoolingPanel( CoolingPanelNum ).Offset, CoolingPanel( CoolingPanelNum ).ControlCompTypeNum, CoolingPanel( CoolingPanelNum ).CompErrIndex, _, _, _, _, _, CoolingPanel( CoolingPanelNum ).LoopNum, CoolingPanel( CoolingPanelNum ).LoopSideNum, CoolingPanel( CoolingPanelNum ).BranchNum );
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
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine gets the input for the simple cooling panel units.

		// METHODOLOGY EMPLOYED:
		// Standard input processor calls--started from Daeho's radiant-convective water baseboard model.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataLoopNode::Node;
		using DataLoopNode::NodeType_Water;
		using DataLoopNode::NodeConnectionType_Inlet;
		using DataLoopNode::NodeConnectionType_Outlet;
		using DataLoopNode::ObjectIsNotParent;
		//unused0909    USE DataGlobals,           ONLY: NumOfZones
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using InputProcessor::SameString;
		using InputProcessor::VerifyName;
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::TestCompSet;
		using DataSurfaces::Surface;
		using DataSurfaces::TotSurfaces;
		using ScheduleManager::GetScheduleIndex;
		using ScheduleManager::GetCurrentScheduleValue;
		using GlobalNames::VerifyUniqueBaseboardName;
		using General::RoundSigDigits;
		using DataSizing::AutoSize;
		using DataSizing::FinalZoneSizing;
		using namespace DataIPShortCuts;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetCoolingPanelInput:" );
		Real64 const MaxFraction( 1.0 );
		Real64 const MinFraction( 0.0 );
		Real64 const MaxWaterTempAvg( 150.0 ); // Maximum limit of average water temperature in degree C
		Real64 const MinWaterTempAvg( 20.0 ); // Minimum limit of average water temperature in degree C
		Real64 const HighWaterMassFlowRate( 10.0 ); // Maximum limit of water mass flow rate in kg/s
		Real64 const LowWaterMassFlowRate( 0.00001 ); // Minimum limit of water mass flow rate in kg/s
		Real64 const MaxWaterFlowRate( 10.0 ); // Maximum limit of water volume flow rate in m3/s
		Real64 const MinWaterFlowRate( 0.00001 ); // Minimum limit of water volume flow rate in m3/s
		Real64 const WaterTempAvgDefault( 87.78 ); // Default water mass flow rate in kg/s
		Real64 const WaterMassFlowDefault( 0.063 ); // Default water mass flow rate in kg/s
		Real64 const AirInletTempStd( 18.0 ); // Standard air inlet temperature in degree C
		Real64 const CPAirStd( 1005.0 ); // Average specific heat of air at between 25C and 40C in J/kg-k
		//    INTEGER, PARAMETER   :: MaxDistribSurfaces    = 20         ! Maximum number of surfaces that a baseboard heater can radiate to
		int const MinDistribSurfaces( 1 ); // Minimum number of surfaces that a baseboard heater can radiate to

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 AllFracsSummed; // Sum of the fractions radiant
		int CoolingPanelNum; // Baseboard number
		int NumAlphas; // Number of Alphas for each GetobjectItem call
		int NumNumbers; // Number of Numbers for each GetobjectItem call
		int SurfNum; // Surface number Do loop counter
		int IOStat;
		static bool ErrorsFound( false ); // If errors detected in input
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		bool errFlag;

		NumCoolingPanels = GetNumObjectsFound( cCMO_CoolingPanel_Simple );

		// Count total number of baseboard units

		CoolingPanel.allocate( NumCoolingPanels );
		CheckEquipName.allocate( NumCoolingPanels );
		CheckEquipName = true;

		// Get the data from the user input related to baseboard heaters
		for ( CoolingPanelNum = 1; CoolingPanelNum <= NumCoolingPanels; ++CoolingPanelNum ) {

			GetObjectItem( cCMO_CoolingPanel_Simple, CoolingPanelNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), CoolingPanel.EquipID(), CoolingPanelNum, IsNotOK, IsBlank, cCMO_CoolingPanel_Simple + " Name" );

			if ( IsNotOK ) {
				ErrorsFound = true;
			}
			VerifyUniqueCoolingPanelName( cCMO_CoolingPanel_Simple, cAlphaArgs( 1 ), errFlag, cCMO_CoolingPanel_Simple  + " Name" );
			if ( errFlag ) {
				ErrorsFound = true;
			}

			CoolingPanel( CoolingPanelNum ).EquipID = cAlphaArgs( 1 ); // Name of this simple cooling panel
			CoolingPanel( CoolingPanelNum ).EquipType = TypeOf_ChilledCeilingPanel_Simple; //'ZoneHVAC:CoolingPanel:RadiantConvective:Water'

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

			CoolingPanel( CoolingPanelNum ).WaterTempAvg = rNumericArgs( 1 );
			if ( CoolingPanel( CoolingPanelNum ).WaterTempAvg > MaxWaterTempAvg + 0.001 ) {
				ShowWarningError( RoutineName + cCMO_CoolingPanel_Simple + "=\"" + cAlphaArgs( 1 ) + "\", " + cNumericFieldNames( 1 ) + " was higher than the allowable maximum." );
				ShowContinueError( "...reset to maximum value=[" + RoundSigDigits( MaxWaterTempAvg, 2 ) + "]." );
				CoolingPanel( CoolingPanelNum ).WaterTempAvg = MaxWaterTempAvg;
			} else if ( CoolingPanel( CoolingPanelNum ).WaterTempAvg < MinWaterTempAvg - 0.001 ) {
				ShowWarningError( RoutineName + cCMO_CoolingPanel_Simple + "=\"" + cAlphaArgs( 1 ) + "\", " + cNumericFieldNames( 1 ) + " was lower than the allowable minimum." );
				ShowContinueError( "...reset to minimum value=[" + RoundSigDigits( MinWaterTempAvg, 2 ) + "]." );
				CoolingPanel( CoolingPanelNum ).WaterTempAvg = MinWaterTempAvg;
			}

			CoolingPanel( CoolingPanelNum ).WaterMassFlowRateStd = rNumericArgs( 2 );
			if ( CoolingPanel( CoolingPanelNum ).WaterMassFlowRateStd < LowWaterMassFlowRate - 0.0001 || CoolingPanel( CoolingPanelNum ).WaterMassFlowRateStd > HighWaterMassFlowRate + 0.0001 ) {
				ShowWarningError( RoutineName + cCMO_CoolingPanel_Simple + "=\"" + cAlphaArgs( 1 ) + "\", " + cNumericFieldNames( 2 ) + " is an invalid Standard Water mass flow rate." );
				ShowContinueError( "...reset to a default value=[" + RoundSigDigits( WaterMassFlowDefault, 1 ) + "]." );
				CoolingPanel( CoolingPanelNum ).WaterMassFlowRateStd = WaterMassFlowDefault;
			}

			CoolingPanel( CoolingPanelNum ).RatedCapacity = rNumericArgs( 3 );

			CoolingPanel( CoolingPanelNum ).WaterVolFlowRateMax = rNumericArgs( 4 );
			if ( std::abs( CoolingPanel( CoolingPanelNum ).WaterVolFlowRateMax ) <= MinWaterFlowRate ) {
				ShowWarningError( RoutineName + cCMO_CoolingPanel_Simple + "=\"" + cAlphaArgs( 1 ) + "\", " + cNumericFieldNames( 4 ) + " was less than the allowable minimum." );
				ShowContinueError( "...reset to minimum value=[" + RoundSigDigits( MinWaterFlowRate, 2 ) + "]." );
				CoolingPanel( CoolingPanelNum ).WaterVolFlowRateMax = MinWaterFlowRate;
			} else if ( CoolingPanel( CoolingPanelNum ).WaterVolFlowRateMax > MaxWaterFlowRate ) {
				ShowWarningError( RoutineName + cCMO_CoolingPanel_Simple + "=\"" + cAlphaArgs( 1 ) + "\", " + cNumericFieldNames( 4 ) + " was higher than the allowable maximum." );
				ShowContinueError( "...reset to maximum value=[" + RoundSigDigits( MaxWaterFlowRate, 2 ) + "]." );
				CoolingPanel( CoolingPanelNum ).WaterVolFlowRateMax = MaxWaterFlowRate;
			}

			CoolingPanel( CoolingPanelNum ).Offset = rNumericArgs( 5 );
			// Set default convergence tolerance
			if ( CoolingPanel( CoolingPanelNum ).Offset <= 0.0 ) {
				ShowWarningError( RoutineName + cCMO_CoolingPanel_Simple + "=\"" + cAlphaArgs( 1 ) + "\", " + cNumericFieldNames( 5 ) + " was less than the allowable minimum." );
				ShowContinueError( "...reset to a default value=[" + RoundSigDigits( MaxFraction, 2 ) + "]." );
				CoolingPanel( CoolingPanelNum ).Offset = 0.001;
			}

			CoolingPanel( CoolingPanelNum ).FracRadiant = rNumericArgs( 6 );
			if ( CoolingPanel( CoolingPanelNum ).FracRadiant < MinFraction ) {
				ShowWarningError( RoutineName + cCMO_CoolingPanel_Simple + "=\"" + cAlphaArgs( 1 ) + "\", " + cNumericFieldNames( 6 ) + " was lower than the allowable minimum." );
				ShowContinueError( "...reset to minimum value=[" + RoundSigDigits( MinFraction, 2 ) + "]." );
				CoolingPanel( CoolingPanelNum ).FracRadiant = MinFraction;
			}
			if ( CoolingPanel( CoolingPanelNum ).FracRadiant > MaxFraction ) {
				ShowWarningError( RoutineName + cCMO_CoolingPanel_Simple + "=\"" + cAlphaArgs( 1 ) + "\", " + cNumericFieldNames( 6 ) + " was higher than the allowable maximum." );
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

			CoolingPanel( CoolingPanelNum ).FracDistribPerson = rNumericArgs( 7 );
			if ( CoolingPanel( CoolingPanelNum ).FracDistribPerson < MinFraction ) {
				ShowWarningError( RoutineName + cCMO_CoolingPanel_Simple + "=\"" + cAlphaArgs( 1 ) + "\", " + cNumericFieldNames( 7 ) + " was lower than the allowable minimum." );
				ShowContinueError( "...reset to minimum value=[" + RoundSigDigits( MinFraction, 3 ) + "]." );
				CoolingPanel( CoolingPanelNum ).FracDistribPerson = MinFraction;
			}
			if ( CoolingPanel( CoolingPanelNum ).FracDistribPerson > MaxFraction ) {
				ShowWarningError( RoutineName + cCMO_CoolingPanel_Simple + "=\"" + cAlphaArgs( 1 ) + "\", " + cNumericFieldNames( 7 ) + " was higher than the allowable maximum." );
				ShowContinueError( "...reset to maximum value=[" + RoundSigDigits( MaxFraction, 3 ) + "]." );
				CoolingPanel( CoolingPanelNum ).FracDistribPerson = MaxFraction;
			}

			CoolingPanel( CoolingPanelNum ).TotSurfToDistrib = NumNumbers - 7;
			//      IF (CoolingPanel(CoolingPanelNum)%TotSurfToDistrib > MaxDistribSurfaces) THEN
			//        CALL ShowWarningError(RoutineName//cCMO_CoolingPanel_Simple//'="'//TRIM(cAlphaArgs(1))// &
			//          '", the number of surface/radiant fraction groups entered was higher than the allowable maximum.')
			//        CALL ShowContinueError('...only the maximum value=['//TRIM(RoundSigDigits(MaxDistribSurfaces))// &
			//           '] will be processed.')
			//        CoolingPanel(CoolingPanelNum)%TotSurfToDistrib = MaxDistribSurfaces
			//      END IF
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
				CoolingPanel( CoolingPanelNum ).SurfaceName( SurfNum ) = cAlphaArgs( SurfNum + 4 );
				CoolingPanel( CoolingPanelNum ).SurfacePtr( SurfNum ) = FindItemInList( cAlphaArgs( SurfNum + 4 ), Surface.Name(), TotSurfaces );
				CoolingPanel( CoolingPanelNum ).FracDistribToSurf( SurfNum ) = rNumericArgs( SurfNum + 7 );
				if ( CoolingPanel( CoolingPanelNum ).SurfacePtr( SurfNum ) == 0 ) {
					ShowSevereError( RoutineName + cCMO_CoolingPanel_Simple + "=\"" + cAlphaArgs( 1 ) + "\", " + cAlphaFieldNames( SurfNum + 4 ) + "=\"" + cAlphaArgs( SurfNum + 4 ) + "\" invalid - not found." );
					ErrorsFound = true;
				}
				if ( CoolingPanel( CoolingPanelNum ).FracDistribToSurf( SurfNum ) > MaxFraction ) {
					ShowWarningError( RoutineName + cCMO_CoolingPanel_Simple + "=\"" + cAlphaArgs( 1 ) + "\", " + cNumericFieldNames( SurfNum + 7 ) + "was greater than the allowable maximum." );
					ShowContinueError( "...reset to maximum value=[" + RoundSigDigits( MaxFraction, 2 ) + "]." );
					CoolingPanel( CoolingPanelNum ).TotSurfToDistrib = MaxFraction;
				}
				if ( CoolingPanel( CoolingPanelNum ).FracDistribToSurf( SurfNum ) < MinFraction ) {
					ShowWarningError( RoutineName + cCMO_CoolingPanel_Simple + "=\"" + cAlphaArgs( 1 ) + "\", " + cNumericFieldNames( SurfNum + 7 ) + "was less than the allowable minimum." );
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
				ShowWarningError( RoutineName + cCMO_CoolingPanel_Simple + "=\"" + cAlphaArgs( 1 ) + "\", Summed radiant fractions for people + surface groups < 1.0" );
				ShowContinueError( "The rest of the radiant energy delivered by the baseboard heater will be lost" );
			}
		}

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + cCMO_CoolingPanel_Simple + "Errors found getting input. Program terminates." );
		}

		// Setup Report variables for the Coils
		for ( CoolingPanelNum = 1; CoolingPanelNum <= NumCoolingPanels; ++CoolingPanelNum ) {
			// CurrentModuleObject='ZoneHVAC:CoolingPanel:RadiantConvective:Water'
			SetupOutputVariable( "Cooling Panel Total Cooling Rate [W]", CoolingPanel( CoolingPanelNum ).TotPower, "System", "Average", CoolingPanel( CoolingPanelNum ).EquipID );

			SetupOutputVariable( "Cooling Panel Convective Cooling Rate [W]", CoolingPanel( CoolingPanelNum ).ConvPower, "System", "Average", CoolingPanel( CoolingPanelNum ).EquipID );
			SetupOutputVariable( "Cooling Panel Radiant Cooling Rate [W]", CoolingPanel( CoolingPanelNum ).RadPower, "System", "Average", CoolingPanel( CoolingPanelNum ).EquipID );
			SetupOutputVariable( "Cooling Panel Total Heating Energy [J]", CoolingPanel( CoolingPanelNum ).TotEnergy, "System", "Sum", CoolingPanel( CoolingPanelNum ).EquipID, _, "ENERGYTRANSFER", "COOLINGPANEL", _, "System" );

			SetupOutputVariable( "Cooling Panel Convective Cooling Energy [J]", CoolingPanel( CoolingPanelNum ).ConvEnergy, "System", "Sum", CoolingPanel( CoolingPanelNum ).EquipID );
			SetupOutputVariable( "Cooling Panel Radiant Cooling Energy [J]", CoolingPanel( CoolingPanelNum ).RadEnergy, "System", "Sum", CoolingPanel( CoolingPanelNum ).EquipID );
			SetupOutputVariable( "Cooling Panel Chilled Water Energy [J]", CoolingPanel( CoolingPanelNum ).Energy, "System", "Sum", CoolingPanel( CoolingPanelNum ).EquipID, _, "PLANTLOOPHEATINGDEMAND", "BASEBOARD", _, "System" );
			SetupOutputVariable( "Cooling Panel Chilled Water Mass Flow Rate [kg/s]", CoolingPanel( CoolingPanelNum ).WaterMassFlowRate, "System", "Average", CoolingPanel( CoolingPanelNum ).EquipID );
			SetupOutputVariable( "Cooling Panel Air Mass Flow Rate [kg/s]", CoolingPanel( CoolingPanelNum ).AirMassFlowRate, "System", "Average", CoolingPanel( CoolingPanelNum ).EquipID );
			SetupOutputVariable( "Cooling Panel Air Inlet Temperature [C]", CoolingPanel( CoolingPanelNum ).AirInletTemp, "System", "Average", CoolingPanel( CoolingPanelNum ).EquipID );
			SetupOutputVariable( "Cooling Panel Air Outlet Temperature [C]", CoolingPanel( CoolingPanelNum ).AirOutletTemp, "System", "Average", CoolingPanel( CoolingPanelNum ).EquipID );
			SetupOutputVariable( "Cooling Panel Water Inlet Temperature [C]", CoolingPanel( CoolingPanelNum ).WaterInletTemp, "System", "Average", CoolingPanel( CoolingPanelNum ).EquipID );
			SetupOutputVariable( "Cooling Panel Water Outlet Temperature [C]", CoolingPanel( CoolingPanelNum ).WaterOutletTemp, "System", "Average", CoolingPanel( CoolingPanelNum ).EquipID );
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
		//       AUTHOR         Russ Taylor
		//                      Rick Strand
		//       DATE WRITTEN   Nov 1997
		//                      Feb 2001
		//       MODIFIED       Aug 2007 Daeho Kang (Add radiant component)
		//                      Sept 2010 Brent Griffith (plant interactions)
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine initializes the cooling panel units, and determines the UA values during simulation.

		// METHODOLOGY EMPLOYED:
		// The initialization subrotines both in high temperature radiant radiator
		// and convective only baseboard radiator are combined and modified. In addition,
		// an UA value calculation by LMTD method is added.

		// REFERENCES:
		// 1. Incropera and DeWitt, Fundamentals of Heat and Mass Transfer
		// Chapter 11.3, p. 510, eq. 11.15 and 11.17

		// Using/Aliasing
		using DataGlobals::BeginEnvrnFlag;
		using DataGlobals::NumOfZones;
		using DataLoopNode::Node;
		using DataEnvironment::StdRhoAir;
		using PlantUtilities::InitComponentNodes;
		using DataPlant::ScanPlantLoopsForObject;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const Constant( 0.0062 ); // Constant of linear equation for air mass flow rate
		Real64 const Coeff( 0.0000275 ); // Correlation coefficient to capacity
		static std::string const RoutineName( "BaseboardRadiatorWater:InitCoolingPanel" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool MyOneTimeFlag( true );
		static bool ZoneEquipmentListChecked( false );
		static FArray1D_bool MyEnvrnFlag;
		int Loop;
		int WaterInletNode;
		int ZoneNode;
		int ZoneNum;
		Real64 RhoAirStdInit;
		Real64 rho; // local fluid density
		Real64 Cp; // local fluid specific heat
		bool errFlag;

		// Do the one time initializations
		if ( MyOneTimeFlag ) {

			// Initialize the environment and sizing flags
			MyEnvrnFlag.allocate( NumCoolingPanels );
			MySizeFlag.allocate( NumCoolingPanels );
			ZeroSourceSumHATsurf.allocate( NumOfZones );
			ZeroSourceSumHATsurf = 0.0;
			CoolingPanelRadSource.allocate( NumCoolingPanels );
			CoolingPanelRadSource = 0.0;
			CoolingPanelRadSrcAvg.allocate( NumCoolingPanels );
			CoolingPanelRadSrcAvg = 0.0;
			LastCoolingPanelRadSrc.allocate( NumCoolingPanels );
			LastCoolingPanelRadSrc = 0.0;
			LastSysTimeElapsed.allocate( NumCoolingPanels );
			LastSysTimeElapsed = 0.0;
			LastTimeStepSys.allocate( NumCoolingPanels );
			LastTimeStepSys = 0.0;
			SetLoopIndexFlag.allocate( NumCoolingPanels );
			MyEnvrnFlag = true;
			MySizeFlag = true;
			MyOneTimeFlag = false;
			SetLoopIndexFlag = true;
			for ( Loop = 1; Loop <= NumCoolingPanels; ++Loop ) {
				// Air mass flow rate is obtained from the following linear equation (reset if autosize is used)
				// m_dot = 0.0062 + 2.75e-05*q
				CoolingPanel( Loop ).AirMassFlowRateStd = Constant + Coeff * CoolingPanel( Loop ).RatedCapacity;
			}
		}

		if ( CoolingPanel( CoolingPanelNum ).ZonePtr <= 0 ) CoolingPanel( CoolingPanelNum ).ZonePtr = ZoneEquipConfig( ControlledZoneNumSub ).ActualZoneNum;

		// Need to check all units to see if they are on ZoneHVAC:EquipmentList or issue warning
		if ( ! ZoneEquipmentListChecked && ZoneEquipInputsFilled ) {
			ZoneEquipmentListChecked = true;
			for ( Loop = 1; Loop <= NumCoolingPanels; ++Loop ) {
				if ( CheckZoneEquipmentList( cCMO_CoolingPanel_Simple, CoolingPanel( Loop ).EquipID ) ) continue;
				ShowSevereError( "InitBaseboard: Unit=[" + cCMO_CoolingPanel_Simple + ',' + CoolingPanel( Loop ).EquipID + "] is not on any ZoneHVAC:EquipmentList.  It will not be simulated." );
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

		if ( ! SysSizingCalc && MySizeFlag( CoolingPanelNum ) && ! SetLoopIndexFlag( CoolingPanelNum ) ) {
			// For each coil, do the sizing once
			SizeCoolingPanel( CoolingPanelNum );
			MySizeFlag( CoolingPanelNum ) = false;
		}

		// Do the Begin Environment initializations
		if ( BeginEnvrnFlag && MyEnvrnFlag( CoolingPanelNum ) ) {
			// Initialize
			RhoAirStdInit = StdRhoAir;
			WaterInletNode = CoolingPanel( CoolingPanelNum ).WaterInletNode;

			rho = GetDensityGlycol( PlantLoop( CoolingPanel( CoolingPanelNum ).LoopNum ).FluidName, InitConvTemp, PlantLoop( CoolingPanel( CoolingPanelNum ).LoopNum ).FluidIndex, RoutineName );

			CoolingPanel( CoolingPanelNum ).WaterMassFlowRateMax = rho * CoolingPanel( CoolingPanelNum ).WaterVolFlowRateMax;

			InitComponentNodes( 0.0, CoolingPanel( CoolingPanelNum ).WaterMassFlowRateMax, CoolingPanel( CoolingPanelNum ).WaterInletNode, CoolingPanel( CoolingPanelNum ).WaterOutletNode, CoolingPanel( CoolingPanelNum ).LoopNum, CoolingPanel( CoolingPanelNum ).LoopSideNum, CoolingPanel( CoolingPanelNum ).BranchNum, CoolingPanel( CoolingPanelNum ).CompNum );

			Node( WaterInletNode ).Temp = 60.0;

			Cp = GetSpecificHeatGlycol( PlantLoop( CoolingPanel( CoolingPanelNum ).LoopNum ).FluidName, Node( WaterInletNode ).Temp, PlantLoop( CoolingPanel( CoolingPanelNum ).LoopNum ).FluidIndex, RoutineName );

			Node( WaterInletNode ).Enthalpy = Cp * Node( WaterInletNode ).Temp;
			Node( WaterInletNode ).Quality = 0.0;
			Node( WaterInletNode ).Press = 0.0;
			Node( WaterInletNode ).HumRat = 0.0;

			ZeroSourceSumHATsurf = 0.0;
			CoolingPanelRadSource = 0.0;
			CoolingPanelRadSrcAvg = 0.0;
			LastCoolingPanelRadSrc = 0.0;
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
			CoolingPanelRadSrcAvg( CoolingPanelNum ) = 0.0;
			LastCoolingPanelRadSrc( CoolingPanelNum ) = 0.0;
			LastSysTimeElapsed( CoolingPanelNum ) = 0.0;
			LastTimeStepSys( CoolingPanelNum ) = 0.0;
		}

		// Do the every time step initializations
		WaterInletNode = CoolingPanel( CoolingPanelNum ).WaterInletNode;
		ZoneNode = ZoneEquipConfig( ControlledZoneNumSub ).ZoneNode;
		CoolingPanel( CoolingPanelNum ).WaterMassFlowRate = Node( WaterInletNode ).MassFlowRate;
		CoolingPanel( CoolingPanelNum ).WaterInletTemp = Node( WaterInletNode ).Temp;
		CoolingPanel( CoolingPanelNum ).WaterInletEnthalpy = Node( WaterInletNode ).Enthalpy;
		CoolingPanel( CoolingPanelNum ).AirInletTemp = Node( ZoneNode ).Temp;
		CoolingPanel( CoolingPanelNum ).AirInletHumRat = Node( ZoneNode ).HumRat;

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
	SizeCoolingPanel( int const CoolingPanelNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   February 2002
		//       MODIFIED       August 2009 Daeho Kang (Add UA autosizing by LMTD)
		//                      Aug 2013 Daeho Kang, add component sizing table entries
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing hot water baseboard components

		// METHODOLOGY EMPLOYED:
		// Obtains flow rates from the zone sizing arrays and plant sizing data.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSizing;
		using DataLoopNode::Node;
		using PlantUtilities::RegisterPlantCompDesignFlow;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const AirInletTempStd( 18.0 ); // I=B=R rating document
		Real64 const CPAirStd( 1005.0 ); // Average specific heat of air at between 25C and 40C in J/kg-k
		Real64 const Constant( 0.0062 ); // Constant of linear equation for air mass flow rate
		Real64 const Coeff( 0.0000275 ); // Correlation coefficient to capacity
		static std::string const RoutineName( "SizeCoolingPanel" );
		static std::string const RoutineNameFull( "CoolingPanelWater:SizeCoolingPanel" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int PltSizNum; // do loop index for plant sizing
		int PltSizHeatNum; // index of plant sizing object for 1st heating loop
		Real64 DesCoilLoad;
		Real64 WaterInletTempStd;
		Real64 WaterOutletTempStd;
		Real64 AirOutletTempStd;
		Real64 DeltaT1;
		Real64 DeltaT2;
		Real64 LMTD;
		Real64 AirMassFlowRate;
		Real64 WaterMassFlowRateStd;
		Real64 rho; // local fluid density
		Real64 Cp; // local fluid specific heat
		bool ErrorsFound; // If errors detected in input
		bool FlowAutoSize; // Indicator to autosize for maximum water vloume flow
		bool CapAutoSize; // Indicator to autosize for capacity
		Real64 WaterVolFlowRateMaxDes; // Design maximum water volume flow for reproting
		Real64 WaterVolFlowRateMaxUser; // User hard-sized maximum water volume flow for reproting
		//REAL(r64)  :: RatedCapacityDes         ! Design rated capacity for reproting
		//REAL(r64)  :: RatedCapacityUser        ! User hard-sized rated capacity for reproting

		PltSizHeatNum = 0;
		PltSizNum = 0;
		DesCoilLoad = 0.0;
		ErrorsFound = false;
		FlowAutoSize = false;
		CapAutoSize = false;
		WaterVolFlowRateMaxDes = 0.0;
		WaterVolFlowRateMaxUser = 0.0;
		//RatedCapacityDes = 0.0d0
		//RatedCapacityUser = 0.0d0

		// find the appropriate heating Plant Sizing object
		PltSizHeatNum = PlantLoop( CoolingPanel( CoolingPanelNum ).LoopNum ).PlantSizNum;

		if ( PltSizHeatNum > 0 ) {
			if ( CurZoneEqNum > 0 ) {
				if ( CoolingPanel( CoolingPanelNum ).WaterVolFlowRateMax == AutoSize ) {
					FlowAutoSize = true;
				}
				if ( ! FlowAutoSize && ! ZoneSizingRunDone ) { // Simulation continue
					if ( CoolingPanel( CoolingPanelNum ).WaterVolFlowRateMax > 0.0 ) {
						ReportSizingOutput( cCMO_CoolingPanel_Simple, CoolingPanel( CoolingPanelNum ).EquipID, "User-Specified Maximum Water Flow Rate [m3/s]", CoolingPanel( CoolingPanelNum ).WaterVolFlowRateMax );
					}
				} else {
					CheckZoneSizing( cCMO_CoolingPanel_Simple, CoolingPanel( CoolingPanelNum ).EquipID );
					DesCoilLoad = CalcFinalZoneSizing( CurZoneEqNum ).DesHeatLoad * CalcFinalZoneSizing( CurZoneEqNum ).HeatSizingFactor;
					if ( DesCoilLoad >= SmallLoad ) {
						Cp = GetSpecificHeatGlycol( PlantLoop( CoolingPanel( CoolingPanelNum ).LoopNum ).FluidName, 60.0, PlantLoop( CoolingPanel( CoolingPanelNum ).LoopNum ).FluidIndex, RoutineName );
						rho = GetDensityGlycol( PlantLoop( CoolingPanel( CoolingPanelNum ).LoopNum ).FluidName, InitConvTemp, PlantLoop( CoolingPanel( CoolingPanelNum ).LoopNum ).FluidIndex, RoutineName );
						WaterVolFlowRateMaxDes = DesCoilLoad / ( PlantSizData( PltSizHeatNum ).DeltaT * Cp * rho );
					} else {
						WaterVolFlowRateMaxDes = 0.0;
					}

					if ( FlowAutoSize ) {
						CoolingPanel( CoolingPanelNum ).WaterVolFlowRateMax = WaterVolFlowRateMaxDes;
						ReportSizingOutput( cCMO_CoolingPanel_Simple, CoolingPanel( CoolingPanelNum ).EquipID, "Design Size Maximum Water Flow Rate [m3/s]", WaterVolFlowRateMaxDes );
					} else { // Hard-sized with sizing data
						if ( CoolingPanel( CoolingPanelNum ).WaterVolFlowRateMax > 0.0 && WaterVolFlowRateMaxDes > 0.0 ) {
							WaterVolFlowRateMaxUser = CoolingPanel( CoolingPanelNum ).WaterVolFlowRateMax;
							ReportSizingOutput( cCMO_CoolingPanel_Simple, CoolingPanel( CoolingPanelNum ).EquipID, "Design Size Maximum Water Flow Rate [m3/s]", WaterVolFlowRateMaxDes, "User-Specified Maximum Water Flow Rate [m3/s]", WaterVolFlowRateMaxUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( WaterVolFlowRateMaxDes - WaterVolFlowRateMaxUser ) / WaterVolFlowRateMaxUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeCoolingPanel: Potential issue with equipment sizing for " "ZoneHVAC:CoolingPanel:RadiantConvective:Water=\"" + CoolingPanel( CoolingPanelNum ).EquipID + "\"." );
									ShowContinueError( "User-Specified Maximum Water Flow Rate of " + RoundSigDigits( WaterVolFlowRateMaxUser, 5 ) + " [m3/s]" );
									ShowContinueError( "differs from Design Size Maximum Water Flow Rate of " + RoundSigDigits( WaterVolFlowRateMaxDes, 5 ) + " [m3/s]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
					}
				}
				if ( CoolingPanel( CoolingPanelNum ).WaterTempAvg > 0.0 && CoolingPanel( CoolingPanelNum ).WaterMassFlowRateStd > 0.0 && CoolingPanel( CoolingPanelNum ).RatedCapacity > 0.0 ) {
					DesCoilLoad = CoolingPanel( CoolingPanelNum ).RatedCapacity;
					WaterMassFlowRateStd = CoolingPanel( CoolingPanelNum ).WaterMassFlowRateStd;
				} else if ( CoolingPanel( CoolingPanelNum ).RatedCapacity == AutoSize || CoolingPanel( CoolingPanelNum ).RatedCapacity == 0.0 ) {
					DesCoilLoad = CalcFinalZoneSizing( CurZoneEqNum ).DesHeatLoad * CalcFinalZoneSizing( CurZoneEqNum ).HeatSizingFactor;
					rho = GetDensityGlycol( PlantLoop( CoolingPanel( CoolingPanelNum ).LoopNum ).FluidName, InitConvTemp, PlantLoop( CoolingPanel( CoolingPanelNum ).LoopNum ).FluidIndex, RoutineNameFull );
					WaterMassFlowRateStd = CoolingPanel( CoolingPanelNum ).WaterVolFlowRateMax * rho;
				}
				if ( DesCoilLoad >= SmallLoad ) {
					// Calculate UA value
					// Air mass flow rate is obtained from the following linear equation
					// m_dot = 0.0062 + 2.75e-05*q
					AirMassFlowRate = Constant + Coeff * DesCoilLoad;
					Cp = GetSpecificHeatGlycol( PlantLoop( CoolingPanel( CoolingPanelNum ).LoopNum ).FluidName, CoolingPanel( CoolingPanelNum ).WaterTempAvg, PlantLoop( CoolingPanel( CoolingPanelNum ).LoopNum ).FluidIndex, RoutineName );
					WaterInletTempStd = ( DesCoilLoad / ( 2.0 * WaterMassFlowRateStd * Cp ) ) + CoolingPanel( CoolingPanelNum ).WaterTempAvg;
					WaterOutletTempStd = std::abs( ( 2.0 * CoolingPanel( CoolingPanelNum ).WaterTempAvg ) - WaterInletTempStd );
					AirOutletTempStd = ( DesCoilLoad / ( AirMassFlowRate * CPAirStd ) ) + AirInletTempStd;
					CoolingPanel( CoolingPanelNum ).AirMassFlowRateStd = AirMassFlowRate;
					// Check Ta,out < Tw,in
					if ( AirOutletTempStd >= WaterInletTempStd ) {
						ShowSevereError( "SizeCoolingPanel: ZoneHVAC:CoolingPanel:RadiantConvective:Water=\"" + CoolingPanel( CoolingPanelNum ).EquipID + "\"." );
						ShowContinueError( "...Air Outlet temperature must be below the Water Inlet temperature" );
						ShowContinueError( "...Air Outlet Temperature=[" + RoundSigDigits( AirOutletTempStd, 2 ) + "], Water Inlet Temperature=[" + RoundSigDigits( WaterInletTempStd, 2 ) + "]." );
						AirOutletTempStd = WaterInletTempStd - 0.01;
						ShowContinueError( "...Air Outlet Temperature set to [" + RoundSigDigits( AirOutletTempStd, 2 ) + "]." );
					}
					// Check Tw,out < Ta,in
					if ( AirInletTempStd >= WaterOutletTempStd ) {
						ShowSevereError( "SizeCoolingPanel: ZoneHVAC:CoolingPanel:RadiantConvective:Water=\"" + CoolingPanel( CoolingPanelNum ).EquipID + "\"." );
						ShowContinueError( "...Water Outlet temperature must be below the Air Inlet temperature" );
						ShowContinueError( "...Air Inlet Temperature=[" + RoundSigDigits( AirInletTempStd, 2 ) + "], Water Outlet Temperature=[" + RoundSigDigits( WaterOutletTempStd, 2 ) + "]." );
						WaterOutletTempStd = AirInletTempStd + 0.01;
						ShowContinueError( "...Water Outlet Temperature set to [" + RoundSigDigits( WaterOutletTempStd, 2 ) + "]." );
					}
					// LMTD calculation
					DeltaT1 = WaterInletTempStd - AirOutletTempStd;
					DeltaT2 = WaterOutletTempStd - AirInletTempStd;
					LMTD = ( DeltaT1 - DeltaT2 ) / ( std::log( DeltaT1 / DeltaT2 ) );
					CoolingPanel( CoolingPanelNum ).UA = DesCoilLoad / LMTD;
				} else {
					CoolingPanel( CoolingPanelNum ).UA = 0.0;
				}
				// Report an UA value
				ReportSizingOutput( cCMO_CoolingPanel_Simple, CoolingPanel( CoolingPanelNum ).EquipID, "U-Factor times Area [W/C]", CoolingPanel( CoolingPanelNum ).UA );
			}
		} else {
			// if there is no heating Sizing:Plant object and autosizng was requested, issue an error message
			if ( CoolingPanel( CoolingPanelNum ).WaterVolFlowRateMax == AutoSize || CoolingPanel( CoolingPanelNum ).RatedCapacity == AutoSize || CoolingPanel( CoolingPanelNum ).RatedCapacity == 0.0 ) {
				ShowSevereError( "Autosizing of hot water baseboard requires a heating loop Sizing:Plant object" );
				ShowContinueError( "Occurs in Hot Water Baseboard Heater=" + CoolingPanel( CoolingPanelNum ).EquipID );
				ErrorsFound = true;
			}
			// calculate UA from rated capacities
			DesCoilLoad = CoolingPanel( CoolingPanelNum ).RatedCapacity;
			if ( DesCoilLoad >= SmallLoad ) {
				WaterMassFlowRateStd = CoolingPanel( CoolingPanelNum ).WaterMassFlowRateStd;
				// m_dot = 0.0062 + 2.75e-05*q
				AirMassFlowRate = Constant + Coeff * DesCoilLoad;
				Cp = GetSpecificHeatGlycol( PlantLoop( CoolingPanel( CoolingPanelNum ).LoopNum ).FluidName, CoolingPanel( CoolingPanelNum ).WaterTempAvg, PlantLoop( CoolingPanel( CoolingPanelNum ).LoopNum ).FluidIndex, RoutineName );
				WaterInletTempStd = ( DesCoilLoad / ( 2.0 * WaterMassFlowRateStd * Cp ) ) + CoolingPanel( CoolingPanelNum ).WaterTempAvg;
				WaterOutletTempStd = std::abs( ( 2.0 * CoolingPanel( CoolingPanelNum ).WaterTempAvg ) - WaterInletTempStd );
				AirOutletTempStd = ( DesCoilLoad / ( AirMassFlowRate * CPAirStd ) ) + AirInletTempStd;
				CoolingPanel( CoolingPanelNum ).AirMassFlowRateStd = AirMassFlowRate;

				// Check Ta,out < Tw,in
				if ( AirOutletTempStd >= WaterInletTempStd ) {
					ShowSevereError( "SizeCoolingPanel: ZoneHVAC:CoolingPanel:RadiantConvective:Water=\"" + CoolingPanel( CoolingPanelNum ).EquipID + "\"." );
					ShowContinueError( "...Air Outlet temperature must be below the Water Inlet temperature" );
					ShowContinueError( "...Air Outlet Temperature=[" + RoundSigDigits( AirOutletTempStd, 2 ) + "], Water Inlet Temperature=[" + RoundSigDigits( WaterInletTempStd, 2 ) + "]." );
					AirOutletTempStd = WaterInletTempStd - 0.01;
					ShowContinueError( "...Air Outlet Temperature set to [" + RoundSigDigits( AirOutletTempStd, 2 ) + "]." );
				}
				// Check Tw,out < Ta,in
				if ( AirInletTempStd >= WaterOutletTempStd ) {
					ShowSevereError( "SizeCoolingPanel: ZoneHVAC:CoolingPanel:RadiantConvective:Water=\"" + CoolingPanel( CoolingPanelNum ).EquipID + "\"." );
					ShowContinueError( "...Water Outlet temperature must be below the Air Inlet temperature" );
					ShowContinueError( "...Air Inlet Temperature=[" + RoundSigDigits( AirInletTempStd, 2 ) + "], Water Outlet Temperature=[" + RoundSigDigits( WaterOutletTempStd, 2 ) + "]." );
					WaterOutletTempStd = AirInletTempStd + 0.01;
					ShowContinueError( "...Water Outlet Temperature set to [" + RoundSigDigits( WaterOutletTempStd, 2 ) + "]." );
				}
				// LMTD calculation
				DeltaT1 = WaterInletTempStd - AirOutletTempStd;
				DeltaT2 = WaterOutletTempStd - AirInletTempStd;
				LMTD = ( DeltaT1 - DeltaT2 ) / ( std::log( DeltaT1 / DeltaT2 ) );
				CoolingPanel( CoolingPanelNum ).UA = DesCoilLoad / LMTD;
			} else {
				CoolingPanel( CoolingPanelNum ).UA = 0.0;
			}
			// Report an UA value
			ReportSizingOutput( cCMO_CoolingPanel_Simple, CoolingPanel( CoolingPanelNum ).EquipID, "U-Factor times Area [W/C]", CoolingPanel( CoolingPanelNum ).UA );

		}
		// save the design water flow rate for use by the water loop sizing algorithms
		RegisterPlantCompDesignFlow( CoolingPanel( CoolingPanelNum ).WaterInletNode, CoolingPanel( CoolingPanelNum ).WaterVolFlowRateMax );

		if ( ErrorsFound ) {
			ShowFatalError( "Preceding sizing errors cause program termination" );
		}

	}

	void
	CalcCoolingPanel(
		int & CoolingPanelNum,
		Real64 & LoadMet
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Russ Taylor
		//       DATE WRITTEN   Nov 1997
		//       MODIFIED       May 2000 Fred Buhl
		//                      Aug 2007 Daeho Kang (Add the calculation of radiant heat source)
		//                      Sep 2011 LKL/BG - resimulate only zones needing it for Radiant systems
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine calculates both the convective and radiant heat transfer rate
		// in a hot water baseboard heater.  The heater is assumed to be crossflowwith
		// both fluids unmixed.  The air flow is bouyancy driven and a constant airflow
		// and a constant airflow velocity of 0.5m/s is assumed.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// Incropera and DeWitt, Fundamentals of Heat and Mass Transfer
		// Chapter 11.4, p. 523, eq. 11.33

		// Using/Aliasing
		using namespace DataSizing;
		using DataLoopNode::Node;
		using ScheduleManager::GetCurrentScheduleValue;
		using DataZoneEnergyDemands::ZoneSysEnergyDemand;
		using DataZoneEnergyDemands::CurDeadBandOrSetback;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const Constant( 0.0062 ); // Constant of linear equation for air mass flow rate
		Real64 const Coeff( 0.0000275 ); // Correlation coefficient to capacity
		Real64 const MinFrac( 0.0005 ); // Minimum fraction that delivers radiant heats to surfaces
		static std::string const RoutineName( "CalcCoolingPanel" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ZoneNum;
		Real64 RadHeat;
		Real64 CoolingPanelHeat;
		Real64 AirInletTemp;
		Real64 AirOutletTemp;
		Real64 WaterInletTemp;
		Real64 WaterOutletTemp;
		Real64 WaterMassFlowRate;
		Real64 AirMassFlowRate;
		Real64 CapacitanceAir;
		Real64 CapacitanceWater;
		Real64 CapacitanceMax;
		Real64 CapacitanceMin;
		Real64 CapacityRatio;
		Real64 NTU;
		Real64 Effectiveness;
		Real64 AA;
		Real64 BB;
		Real64 CC;
		Real64 QZnReq;
		Real64 Cp;

		ZoneNum = CoolingPanel( CoolingPanelNum ).ZonePtr;
		QZnReq = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToHeatSP;
		AirInletTemp = CoolingPanel( CoolingPanelNum ).AirInletTemp;
		AirOutletTemp = AirInletTemp;
		WaterInletTemp = CoolingPanel( CoolingPanelNum ).WaterInletTemp;
		WaterOutletTemp = WaterInletTemp;
		WaterMassFlowRate = Node( CoolingPanel( CoolingPanelNum ).WaterInletNode ).MassFlowRate;

		if ( QZnReq > SmallLoad && ! CurDeadBandOrSetback( ZoneNum ) && ( GetCurrentScheduleValue( CoolingPanel( CoolingPanelNum ).SchedPtr ) > 0 ) && ( WaterMassFlowRate > 0.0 ) ) {
			// Assume the air mass flow rate is twice the water mass flow rate
			// Calculate air mass flow rate
			AirMassFlowRate = CoolingPanel( CoolingPanelNum ).AirMassFlowRateStd * ( WaterMassFlowRate / CoolingPanel( CoolingPanelNum ).WaterMassFlowRateStd );
			CapacitanceAir = PsyCpAirFnWTdb( CoolingPanel( CoolingPanelNum ).AirInletHumRat, AirInletTemp ) * AirMassFlowRate;
			Cp = GetSpecificHeatGlycol( PlantLoop( CoolingPanel( CoolingPanelNum ).LoopNum ).FluidName, WaterInletTemp, PlantLoop( CoolingPanel( CoolingPanelNum ).LoopNum ).FluidIndex, RoutineName );

			CapacitanceWater = Cp * WaterMassFlowRate;
			CapacitanceMax = max( CapacitanceAir, CapacitanceWater );
			CapacitanceMin = min( CapacitanceAir, CapacitanceWater );
			CapacityRatio = CapacitanceMin / CapacitanceMax;
			NTU = CoolingPanel( CoolingPanelNum ).UA / CapacitanceMin;

			// The effectiveness is given by the following formula:
			// Effectiveness = 1. - EXP((1./CapacityRatio)*(NTU)**0.22*(EXP(-CapacityRatio*(NTU)**0.78)-1.))
			// To prevent possible underflows (numbers smaller than the computer can handle) we must break
			// the calculation up into steps and check the size of the exponential arguments.
			AA = -CapacityRatio * std::pow( NTU, 0.78 );
			if ( AA < -20.0 ) {
				BB = 0.0;
			} else {
				BB = std::exp( AA );
			}
			CC = ( 1.0 / CapacityRatio ) * std::pow( NTU, 0.22 ) * ( BB - 1.0 );
			if ( CC < -20.0 ) {
				Effectiveness = 1.0;
			} else {
				Effectiveness = 1.0 - std::exp( CC );
			}

			AirOutletTemp = AirInletTemp + Effectiveness * CapacitanceMin * ( WaterInletTemp - AirInletTemp ) / CapacitanceAir;
			WaterOutletTemp = WaterInletTemp - CapacitanceAir * ( AirOutletTemp - AirInletTemp ) / CapacitanceWater;
			CoolingPanelHeat = CapacitanceWater * ( WaterInletTemp - WaterOutletTemp );
			RadHeat = CoolingPanelHeat * CoolingPanel( CoolingPanelNum ).FracRadiant;
			CoolingPanelRadSource( CoolingPanelNum ) = RadHeat;

			if ( CoolingPanel( CoolingPanelNum ).FracRadiant <= MinFrac ) {
				LoadMet = CoolingPanelHeat;
			} else {

				// Now, distribute the radiant energy of all systems to the appropriate surfaces, to people, and the air
				DistributeCoolingPanelRadGains();
				// Now "simulate" the system by recalculating the heat balances
				CalcHeatBalanceOutsideSurf( ZoneNum );

				CalcHeatBalanceInsideSurf( ZoneNum );

				// Here an assumption is made regarding radiant heat transfer to people.
				// While the radiant heat transfer to people array will be used by the thermal comfort
				// routines, the energy transfer to people would get lost from the perspective
				// of the heat balance.  So, to avoid this net loss of energy which clearly
				// gets added to the zones, we must account for it somehow.  This assumption
				// that all energy radiated to people is converted to convective energy is
				// not very precise, but at least it conserves energy. The system impact to heat balance
				// should include this.
				LoadMet = ( SumHATsurf( ZoneNum ) - ZeroSourceSumHATsurf( ZoneNum ) ) + ( CoolingPanelHeat * CoolingPanel( CoolingPanelNum ).FracConvect ) + ( RadHeat * CoolingPanel( CoolingPanelNum ).FracDistribPerson );
			}
			CoolingPanel( CoolingPanelNum ).WaterOutletEnthalpy = CoolingPanel( CoolingPanelNum ).WaterInletEnthalpy - CoolingPanelHeat / WaterMassFlowRate;
		} else {
			CapacitanceWater = 0.0;
			CapacitanceMax = 0.0;
			CapacitanceMin = 0.0;
			NTU = 0.0;
			Effectiveness = 0.0;
			AirOutletTemp = AirInletTemp;
			WaterOutletTemp = WaterInletTemp;
			CoolingPanelHeat = 0.0;
			LoadMet = 0.0;
			RadHeat = 0.0;
			WaterMassFlowRate = 0.0;
			AirMassFlowRate = 0.0;
			CoolingPanelRadSource( CoolingPanelNum ) = 0.0;
			CoolingPanel( CoolingPanelNum ).WaterOutletEnthalpy = CoolingPanel( CoolingPanelNum ).WaterInletEnthalpy;
		}

		CoolingPanel( CoolingPanelNum ).WaterOutletTemp = WaterOutletTemp;
		CoolingPanel( CoolingPanelNum ).AirOutletTemp = AirOutletTemp;
		CoolingPanel( CoolingPanelNum ).WaterMassFlowRate = WaterMassFlowRate;
		CoolingPanel( CoolingPanelNum ).AirMassFlowRate = AirMassFlowRate;
		CoolingPanel( CoolingPanelNum ).TotPower = LoadMet;
		CoolingPanel( CoolingPanelNum ).Power = CoolingPanelHeat;
		CoolingPanel( CoolingPanelNum ).ConvPower = CoolingPanelHeat - RadHeat;
		CoolingPanel( CoolingPanelNum ).RadPower = RadHeat;

	}

	void
	UpdateCoolingPanel( int const CoolingPanelNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Russ Taylor
		//                      Rick Strand
		//       DATE WRITTEN   Nov 1997
		//                      February 2001
		//       MODIFIED       Aug 2007 Daeho Kang (Add the update of radiant source)
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:

		// METHODOLOGY EMPLOYED:
		// The update subrotines both in high temperature radiant radiator
		// and convective only baseboard radiator are combined and modified.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataLoopNode::Node;
		using DataGlobals::TimeStepZone;
		using DataGlobals::BeginEnvrnFlag;
		using PlantUtilities::SafeCopyPlantNode;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

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
			CoolingPanelRadSrcAvg( CoolingPanelNum ) -= LastCoolingPanelRadSrc( CoolingPanelNum ) * LastTimeStepSys( CoolingPanelNum ) / TimeStepZone;
		}
		// Update the running average and the "last" values with the current values of the appropriate variables
		CoolingPanelRadSrcAvg( CoolingPanelNum ) += CoolingPanelRadSource( CoolingPanelNum ) * TimeStepSys / TimeStepZone;

		LastCoolingPanelRadSrc( CoolingPanelNum ) = CoolingPanelRadSource( CoolingPanelNum );
		LastSysTimeElapsed( CoolingPanelNum ) = SysTimeElapsed;
		LastTimeStepSys( CoolingPanelNum ) = TimeStepSys;

		WaterInletNode = CoolingPanel( CoolingPanelNum ).WaterInletNode;
		WaterOutletNode = CoolingPanel( CoolingPanelNum ).WaterOutletNode;

		// Set the outlet air nodes of the Baseboard
		// Set the outlet water nodes for the Coil
		SafeCopyPlantNode( WaterInletNode, WaterOutletNode );
		Node( WaterOutletNode ).Temp = CoolingPanel( CoolingPanelNum ).WaterOutletTemp;
		Node( WaterOutletNode ).Enthalpy = CoolingPanel( CoolingPanelNum ).WaterOutletEnthalpy;

	}

	void
	UpdateCoolingPanelRadSourceValAvg( bool & CoolingPanelSysOn ) // .TRUE. if the radiant system has run this zone time step
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   February 2001
		//       MODIFIED       Aug 2007 Daeho Kang (Modification only for baseboard)
		//       RE-ENGINEERED  na

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
		int CoolingPanelNum; // DO loop counter for surface index

		// FLOW:
		CoolingPanelSysOn = false;

		// If this was never allocated, then there are no radiant systems in this input file (just RETURN)
		if ( ! allocated( CoolingPanelRadSrcAvg ) ) return;

		// If it was allocated, then we have to check to see if this was running at all...
		for ( CoolingPanelNum = 1; CoolingPanelNum <= NumCoolingPanels; ++CoolingPanelNum ) {
			if ( CoolingPanelRadSrcAvg( CoolingPanelNum ) != 0.0 ) {
				CoolingPanelSysOn = true;
				break; //DO loop
			}
		}

		CoolingPanelRadSource = CoolingPanelRadSrcAvg;

		DistributeCoolingPanelRadGains(); // CoolingPanelRadSource has been modified so we need to redistribute gains

	}

	void
	DistributeCoolingPanelRadGains()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   February 2001
		//       MODIFIED       Aug. 2007 Daeho Kang (Modification only for baseboard)
		//                      April 2010 Brent Griffith, max limit to protect surface temperature calcs
		//       RE-ENGINEERED  na

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
		// na

		// Using/Aliasing
		using General::RoundSigDigits;
		using DataHeatBalance::Zone;
		using DataHeatBalFanSys::QCoolingPanelToPerson;
		using DataHeatBalFanSys::QCoolingPanelSurf;
		using DataHeatBalFanSys::MaxRadHeatFlux;
		using DataSurfaces::Surface;
		using DataSurfaces::TotSurfaces;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const SmallestArea( 0.001 ); // Smallest area in meters squared (to avoid a divide by zero)

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

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
			QCoolingPanelToPerson( ZoneNum ) += CoolingPanelRadSource( CoolingPanelNum ) * CoolingPanel( CoolingPanelNum ).FracDistribPerson;

			for ( RadSurfNum = 1; RadSurfNum <= CoolingPanel( CoolingPanelNum ).TotSurfToDistrib; ++RadSurfNum ) {
				SurfNum = CoolingPanel( CoolingPanelNum ).SurfacePtr( RadSurfNum );
				if ( Surface( SurfNum ).Area > SmallestArea ) {
					ThisSurfIntensity = ( CoolingPanelRadSource( CoolingPanelNum ) * CoolingPanel( CoolingPanelNum ).FracDistribToSurf( RadSurfNum ) / Surface( SurfNum ).Area );
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
		//       AUTHOR         Daeho Kang
		//       DATE WRITTEN   Aug 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE: This subroutine

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataLoopNode::Node;
		using DataSurfaces::Surface;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		CoolingPanel( CoolingPanelNum ).TotEnergy = CoolingPanel( CoolingPanelNum ).TotPower * TimeStepSys * SecInHour;
		CoolingPanel( CoolingPanelNum ).Energy = CoolingPanel( CoolingPanelNum ).Power * TimeStepSys * SecInHour;
		CoolingPanel( CoolingPanelNum ).ConvEnergy = CoolingPanel( CoolingPanelNum ).ConvPower * TimeStepSys * SecInHour;
		CoolingPanel( CoolingPanelNum ).RadEnergy = CoolingPanel( CoolingPanelNum ).RadPower * TimeStepSys * SecInHour;

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

	void
	UpdateCoolingPanelPlantConnection(
		int const BaseboardTypeNum, // type index
		std::string const & BaseboardName, // component name
		int const EquipFlowCtrl, // Flow control mode for the equipment
		int const LoopNum, // Plant loop index for where called from
		int const LoopSide, // Plant loop side index for where called from
		int & CompIndex, // Chiller number pointer
		bool const FirstHVACIteration,
		bool & InitLoopEquip // If not zero, calculate the max load for operating conditions
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Sept. 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// update sim routine called from plant

		// METHODOLOGY EMPLOYED:
		// check input, provide comp index, call utility routines

		// REFERENCES:
		// na

		// Using/Aliasing
		using PlantUtilities::PullCompInterconnectTrigger;
		using DataPlant::ccSimPlantEquipTypes;
		using DataPlant::TypeOf_ChilledCeilingPanel_Simple;
		using DataPlant::CriteriaType_MassFlowRate;
		using DataPlant::CriteriaType_Temperature;
		using DataPlant::CriteriaType_HeatTransferRate;
		using InputProcessor::FindItemInList;
		using General::TrimSigDigits;
		using DataGlobals::KickOffSimulation;

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

		int CoolingPanelNum;
		int InletNodeNum;
		int OutletNodeNum;

		// Find the correct baseboard
		if ( CompIndex == 0 ) {
			CoolingPanelNum = FindItemInList( BaseboardName, CoolingPanel.EquipID(), NumCoolingPanels );
			if ( CoolingPanelNum == 0 ) {
				ShowFatalError( "UpdateCoolingPanelPlantConnection: Specified baseboard not valid =" + BaseboardName );
			}
			CompIndex = CoolingPanelNum;
		} else {
			CoolingPanelNum = CompIndex;
			if ( CoolingPanelNum > NumCoolingPanels || CoolingPanelNum < 1 ) {
				ShowFatalError( "UpdateCoolingPanelPlantConnection:  Invalid CompIndex passed=" + TrimSigDigits( CoolingPanelNum ) + ", Number of baseboards=" + TrimSigDigits( NumCoolingPanels ) + ", Entered baseboard name=" + BaseboardName );
			}
			if ( KickOffSimulation ) {
				if ( CoolingPanelName != CoolingPanel( CoolingPanelNum ).EquipID ) {
					ShowFatalError( "UpdateCoolingPanelPlantConnection: Invalid CompIndex passed=" + TrimSigDigits( CoolingPanelNum ) + ", baseboard name=" + CoolingPanelName + ", stored baseboard Name for that index=" + CoolingPanel( CoolingPanelNum ).EquipID );
				}
				if ( CoolingPanelTypeNum != TypeOf_ChilledCeilingPanel_Simple ) {
					ShowFatalError( "UpdateCoolingPanelPlantConnection: Invalid CompIndex passed=" + TrimSigDigits( CoolingPanelNum ) + ", baseboard name=" + CoolingPanelName + ", stored baseboard Name for that index=" + ccSimPlantEquipTypes( CoolingPanelTypeNum ) );
				}
			}
		}

		if ( InitLoopEquip ) {
			return;
		}

		PullCompInterconnectTrigger( CoolingPanel( CoolingPanelNum ).LoopNum, CoolingPanel( CoolingPanelNum ).LoopSideNum, CoolingPanel( CoolingPanelNum ).BranchNum, CoolingPanel( CoolingPanelNum ).CompNum, CoolingPanel( CoolingPanelNum ).CoolingPanelLoadReSimIndex, CoolingPanel( CoolingPanelNum ).LoopNum, CoolingPanel( CoolingPanelNum ).LoopSideNum, CriteriaType_HeatTransferRate, CoolingPanel( CoolingPanelNum ).Power );

		PullCompInterconnectTrigger( CoolingPanel( CoolingPanelNum ).LoopNum, CoolingPanel( CoolingPanelNum ).LoopSideNum, CoolingPanel( CoolingPanelNum ).BranchNum, CoolingPanel( CoolingPanelNum ).CompNum, CoolingPanel( CoolingPanelNum ).CoolingPanelMassFlowReSimIndex, CoolingPanel( CoolingPanelNum ).LoopNum, CoolingPanel( CoolingPanelNum ).LoopSideNum, CriteriaType_MassFlowRate, CoolingPanel( CoolingPanelNum ).WaterMassFlowRate );

		PullCompInterconnectTrigger( CoolingPanel( CoolingPanelNum ).LoopNum, CoolingPanel( CoolingPanelNum ).LoopSideNum, CoolingPanel( CoolingPanelNum ).BranchNum, CoolingPanel( CoolingPanelNum ).CompNum, CoolingPanel( CoolingPanelNum ).CoolingPanelInletTempFlowReSimIndex, CoolingPanel( CoolingPanelNum ).LoopNum, CoolingPanel( CoolingPanelNum ).LoopSideNum, CriteriaType_Temperature, CoolingPanel( CoolingPanelNum ).WaterOutletTemp );

	}

	//*****************************************************************************************
	//     NOTICE

	//     Copyright � 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.

	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in EnergyPlus.f90.

	//     NOTICE: The U.S. Government is granted for itself and others acting on its
	//     behalf a paid-up, nonexclusive, irrevocable, worldwide license in this data to
	//     reproduce, prepare derivative works, and perform publicly and display publicly.
	//     Beginning five (5) years after permission to assert copyright is granted,
	//     subject to two possible five year renewals, the U.S. Government is granted for
	//     itself and others acting on its behalf a paid-up, non-exclusive, irrevocable
	//     worldwide license in this data to reproduce, prepare derivative works,
	//     distribute copies to the public, perform publicly and display publicly, and to
	//     permit others to do so.

	//     TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.

} // CoolingPanelSimple

} // EnergyPlus
