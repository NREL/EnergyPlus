// C++ Headers
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/FArray.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <SetPointManager.hh>
#include <CurveManager.hh>
#include <DataAirLoop.hh>
#include <DataAirSystems.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataHVACGlobals.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <DataZoneControls.hh>
#include <DataZoneEnergyDemands.hh>
#include <DataZoneEquipment.hh>
#include <EMSManager.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutAirNodeManager.hh>
#include <OutputProcessor.hh>
#include <Psychrometrics.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace SetPointManager {

	// Module containing the SetPoint Manager routines

	// MODULE INFORMATION:
	//       AUTHOR         Fred Buhl
	//       DATE WRITTEN   July 1998
	//       MODIFIED       Shirey/Raustad (FSEC), Jan 2004
	//                      Nov 2004 - Jan 2005 M. J. Witte, GARD Analytics, Inc.
	//                        Add new setpoint managers:
	//                          SET POINT MANAGER:SINGLE ZONE HEATING and
	//                          SET POINT MANAGER:SINGLE ZONE COOLING
	//                          SET POINT MANAGER:OUTSIDE AIR PRETREAT
	//                        Work supported by ASHRAE research project 1254-RP
	//                      Phil Haves Oct 2004
	//                      B. Griffith Aug. 2006.
	//                      R. Raustad - FSEC: added AllSetPtMgr used for node conflict checks
	//                      July 2010 B.A. Nigusse, FSEC/UCF
	//                        Added new setpoint managers:
	//                          SetpointManager:MultiZone:Heating:Average
	//                          SetpointManager:MultiZone:Cooling:Average
	//                          SetpointManager:MultiZone:MinimumHumidity:Average
	//                          SetpointManager:MultiZone:MaximumHumidity:Average
	//                       22Aug2010 Craig Wray - added Fan:ComponentModel
	//                      Aug 2010 B.A. Nigusse, FSEC/UCF
	//                        Added new setpoint managers:
	//                          SetpointManager:MultiZone:Humidity:Minimum
	//                          SetpointManager:MultiZone:Humidity:Maximum
	//                      July 2011 Chandan Sharma, FSEC/UCF
	//                        Added new setpoint managers:
	//                          SetpointManager:FollowOutdoorAirTemperature
	//                          SetpointManager:FollowSystemNodeTemperature
	//                          SetpointManager:FollowGroundTemperature
	//                      March 2012, Atefe Makhmalbaf and Heejin Cho, PNNL
	//                        Added new setpoint manager:
	//                          SetpointManager:CondenserEnteringReset
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To encapsulate the data and algorithms required to
	// determine all the controller setpoints in the problem.

	// METHODOLOGY EMPLOYED:
	// Previous time step node data will be used, in a set of fixed, precoded algorithms,
	// to determine the current time step's controller setpoints.

	// REFERENCES:

	// OTHER NOTES:

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataLoopNode;
	using namespace DataAirLoop;
	using DataGlobals::BeginEnvrnFlag;
	using DataGlobals::BeginDayFlag;
	using DataGlobals::NumOfZones;
	using DataGlobals::MetersHaveBeenInitialized;
	using DataGlobals::RunOptCondEntTemp;
	using DataEnvironment::OutDryBulbTemp;
	using DataEnvironment::OutWetBulbTemp;
	using DataEnvironment::OutBaroPress;
	using DataEnvironment::OutHumRat;
	using namespace ScheduleManager;
	using DataHVACGlobals::NumPrimaryAirSys;
	using namespace CurveManager;

	// USE STATEMENTS
	using Psychrometrics::PsyHFnTdbW;
	using Psychrometrics::PsyCpAirFnWTdb;

	// Data
	//MODULE PARAMETER DEFINITIONS:
	int const MaxTemp( 1 );
	int const MinTemp( 2 );
	int const TempFirst( 1 );
	int const FlowFirst( 2 );
	int const iRefTempType_WetBulb( 1 );
	int const iRefTempType_DryBulb( 2 );
	int const iRefGroundTempObjType_BuildingSurface( 1 );
	int const iRefGroundTempObjType_Shallow( 2 );
	int const iRefGroundTempObjType_Deep( 3 );
	int const iRefGroundTempObjType_FCfactorMethod( 4 );

	// following are used to reduce string comparisons related to CtrlVarType
	int const iCtrlVarType_Temp( 1 ); // control type 'Temperature'
	int const iCtrlVarType_MaxTemp( 2 ); // control type 'MaximumTemperature'
	int const iCtrlVarType_MinTemp( 3 ); // control type 'MinimumTemperature'
	int const iCtrlVarType_HumRat( 4 ); // control Type 'HumidityRatio'
	int const iCtrlVarType_MaxHumRat( 5 ); // control Type 'MaximumHumidityRatio'
	int const iCtrlVarType_MinHumRat( 6 ); // control Type 'MinimumHumidityRatio'
	int const iCtrlVarType_MassFlow( 7 ); // control type 'MassFlowRate'
	int const iCtrlVarType_MaxMassFlow( 8 ); // control Type 'MaximumMassFlowRate'
	int const iCtrlVarType_MinMassFlow( 9 ); // control Type 'MinimumMassFlowRate'

	int const NumValidCtrlTypes( 9 );
	FArray1D_string const cValidCtrlTypes( NumValidCtrlTypes, { "Temperature", "MaximumTemperature", "MinimumTemperature", "HumidityRatio", "MaximumHumidityRatio", "MinimumHumidityRatio", "MassFlowRate", "MaximumMassFlowRate", "MinimumMassFlowRate" } );

	// following are used to reduce string comparisons related to CtrlVarType
	int const iSPMType_Scheduled( 1 );
	int const iSPMType_ScheduledDual( 2 );
	int const iSPMType_OutsideAir( 3 );
	int const iSPMType_SZReheat( 4 );
	int const iSPMType_SZHeating( 5 );
	int const iSPMType_SZCooling( 6 );
	int const iSPMType_SZMinHum( 7 );
	int const iSPMType_SZMaxHum( 8 );
	int const iSPMType_MixedAir( 9 );
	int const iSPMType_OutsideAirPretreat( 10 );
	int const iSPMType_Warmest( 11 );
	int const iSPMType_Coldest( 12 );
	int const iSPMType_WarmestTempFlow( 13 );
	int const iSPMType_RAB( 14 );
	int const iSPMType_MZCoolingAverage( 15 );
	int const iSPMType_MZHeatingAverage( 16 );
	int const iSPMType_MZMinHumAverage( 17 );
	int const iSPMType_MZMaxHumAverage( 18 );
	int const iSPMType_MZMinHum( 19 );
	int const iSPMType_MZMaxHum( 20 );
	int const iSPMType_FollowOATemp( 21 );
	int const iSPMType_FollowSysNodeTemp( 22 );
	int const iSPMType_GroundTemp( 23 );
	int const iSPMType_CondEntReset( 24 );
	int const iSPMType_IdealCondEntReset( 25 );
	int const iSPMType_SZOneStageCooling( 26 );
	int const iSPMType_SZOneStageHeating( 27 );

	int const NumValidSPMTypes( 27 );
	FArray1D_string const cValidSPMTypes( NumValidSPMTypes, { "SetpointManager:Scheduled", "SetpointManager:Scheduled:DualSetpoint", "SetpointManager:OutdoorAirReset", "SetpointManager:SingleZone:Reheat", "SetpointManager:SingleZone:Heating", "SetpointManager:SingleZone:Cooling", "SetpointManager:SingleZone:Humidity:Minimum", "SetpointManager:SingleZone:Humidity:Maximum", "SetpointManager:MixedAir", "SetpointManager:OutdoorAirPretreat", "SetpointManager:Warmest", "SetpointManager:Coldest", "SetpointManager:WarmestTemperatureFlow", "SetpointManager:ReturnAirBypassFlow", "SetpointManager:MultiZone:Cooling:Average", "SetpointManager:MultiZone:Heating:Average", "SetpointManager:MultiZone:MinimumHumidity:Average", "SetpointManager:MultiZone:MaximumHumidity:Average", "SetpointManager:MultiZone:Humidity:Minimum", "SetpointManager:MultiZone:Humidity:Maximum", "SetpointManager:FollowOutdoorAirTemperature", "SetpointManager:FollowSystemNodeTemperature", "SetpointManager:FollowGroundTemperature", "SetpointManager:CondenserEnteringReset", "SetpointManager:CondenserEnteringReset:Ideal", "SetpointManager:SingleZone:OneStageCooling", "SetpointManager:SingleZone:OneStageHeating" } );

	//Type declarations in SetPointManager module

	// This one is used for conflicting node checks and is DEALLOCATED at the end of VerifySetPointManagers

	//MODULE VARIABLE DECLARATIONS:
	int NumAllSetPtMgrs( 0 ); // Number of all Setpoint Managers found in input
	int NumSchSetPtMgrs( 0 ); // Number of Scheduled Setpoint Managers found in input
	int NumDualSchSetPtMgrs( 0 ); // Number of Scheduled Dual Setpoint Managers found in input
	int NumOutAirSetPtMgrs( 0 ); // Number of Outside Air Setpoint Managers found in input
	int NumSZRhSetPtMgrs( 0 ); // number of single zone reheat setpoint managers
	int NumSZHtSetPtMgrs( 0 ); // number of single zone heating setpoint managers
	int NumSZClSetPtMgrs( 0 ); // number of single zone cooling setpoint managers
	int NumSZMinHumSetPtMgrs( 0 ); // number of Single Zone Minimum Humidity Setpoint Managers
	int NumSZMaxHumSetPtMgrs( 0 ); // number of Single Zone Maximum Humidity Setpoint Managers
	int NumMixedAirSetPtMgrs( 0 ); // number of mixed air setpoint managers
	int NumOAPretreatSetPtMgrs( 0 ); // number of outside air pretreat setpoint managers
	int NumWarmestSetPtMgrs( 0 ); // number of Warmest setpoint managers
	int NumColdestSetPtMgrs( 0 ); // number of Coldest setpoint managers
	int NumWarmestSetPtMgrsTempFlow( 0 ); // number of Warmest Temp Flow setpoint managers
	int NumRABFlowSetPtMgrs( 0 ); // number of return air bypass temperature-based flow setpoint manager
	int NumMZClgAverageSetPtMgrs( 0 ); // number of Multizone:Cooling:Average setpoint managers
	int NumMZHtgAverageSetPtMgrs( 0 ); // number of Multizone:Heating:Average setpoint managers
	int NumMZAverageMinHumSetPtMgrs( 0 ); // number of MultiZone:MinimumHumidity:Average setpoint managers
	int NumMZAverageMaxHumSetPtMgrs( 0 ); // number of MultiZone:MaximumHumidity:Average setpoint managers
	int NumMZMinHumSetPtMgrs( 0 ); // number of MultiZone:Humidity:Minimum setpoint managers
	int NumMZMaxHumSetPtMgrs( 0 ); // number of MultiZone:Humidity:Maximum setpoint managers
	int NumFollowOATempSetPtMgrs( 0 ); // number of SetpointManager:FollowOutdoorAirTemperature setpoint managers
	int NumFollowSysNodeTempSetPtMgrs( 0 ); // number of SetpointManager:FollowSystemNodeTemperature setpoint managers
	int NumGroundTempSetPtMgrs( 0 ); // number of SetpointManager:FollowGroundTemperature setpoint managers
	int NumCondEntSetPtMgrs( 0 ); // number of Condenser Entering Reset setpoint managers
	int NumIdealCondEntSetPtMgrs( 0 ); // number of Ideal Condenser Entering Temperature setpoint managers
	int NumSZOneStageCoolingSetPtMgrs( 0 ); // number of single zone one stage cooling setpoint managers
	int NumSZOneStageHeatingSetPtMgrs( 0 ); // number of singel zone one stage heating setpoint managers

	bool ManagerOn( false );
	bool GetInputFlag( true ); // First time, input is "gotten"

	// temperature-based flow control manager
	// Average Cooling Set Pt Mgr
	// Average Heating Set Pt Mgr
	// Average Minimum humidity ratio Set Pt Mgr
	// Average Maximum humidity ratio Set Pt Mgr

	// Temperature Setpoint Manager data
	// Node Temp Setpoint Manager data
	// Manager data

	//SUBROUTINE SPECIFICATIONS FOR MODULE SetPointManager

	// Object Data
	FArray1D< DataSetPointManager > AllSetPtMgr; // Array for all Setpoint Manager data(warnings)
	FArray1D< DefineScheduledSetPointManager > SchSetPtMgr; // Array for Scheduled Setpoint Manager data
	FArray1D< DefineSchedDualSetPointManager > DualSchSetPtMgr; // Dual Scheduled Setpoint Manager data
	FArray1D< DefineOutsideAirSetPointManager > OutAirSetPtMgr; // Array for Outside Air Setpoint Manager data
	FArray1D< DefineSZReheatSetPointManager > SingZoneRhSetPtMgr; // Array for SZRH Set Pt Mgr
	FArray1D< DefineSZHeatingSetPointManager > SingZoneHtSetPtMgr; // Array for SZ Heating Set Pt Mgr
	FArray1D< DefineSZCoolingSetPointManager > SingZoneClSetPtMgr; // Array for SZ Cooling Set Pt Mgr
	FArray1D< DefineSZMinHumSetPointManager > SZMinHumSetPtMgr; // Array for SZ Min Hum Set Pt Mgr
	FArray1D< DefineSZMaxHumSetPointManager > SZMaxHumSetPtMgr; // Array for SZ Max Hum Set Pt Mgr
	FArray1D< DefineMixedAirSetPointManager > MixedAirSetPtMgr; // Array for Mixed Air Set Pt Mgr
	FArray1D< DefineOAPretreatSetPointManager > OAPretreatSetPtMgr; // Array for OA Pretreat Set Pt Mgr
	FArray1D< DefineWarmestSetPointManager > WarmestSetPtMgr; // Array for Warmest Set Pt Mgr
	FArray1D< DefineColdestSetPointManager > ColdestSetPtMgr; // Array for Coldest Set Pt Mgr
	FArray1D< DefWarmestSetPtManagerTempFlow > WarmestSetPtMgrTempFlow; // Array for Warmest Set Pt Mgr
	FArray1D< DefRABFlowSetPointManager > RABFlowSetPtMgr; // Array for return air bypass
	FArray1D< DefMultiZoneAverageCoolingSetPointManager > MZAverageCoolingSetPtMgr; // Array for MultiZone
	FArray1D< DefMultiZoneAverageHeatingSetPointManager > MZAverageHeatingSetPtMgr; // Array for MultiZone
	FArray1D< DefMultiZoneAverageMinHumSetPointManager > MZAverageMinHumSetPtMgr; // Array for MultiZone
	FArray1D< DefMultiZoneAverageMaxHumSetPointManager > MZAverageMaxHumSetPtMgr; // Array for MultiZone
	FArray1D< DefMultiZoneMinHumSetPointManager > MZMinHumSetPtMgr; // Multizone min humidity rat Set Pt Mgr
	FArray1D< DefMultiZoneMaxHumSetPointManager > MZMaxHumSetPtMgr; // Multizone max humidity rat Set Pt Mgr
	FArray1D< DefineFollowOATempSetPointManager > FollowOATempSetPtMgr; // Array for Follow Outdoor Air
	FArray1D< DefineFollowSysNodeTempSetPointManager > FollowSysNodeTempSetPtMgr; // Array for Follow System
	FArray1D< DefineGroundTempSetPointManager > GroundTempSetPtMgr; // Array for Ground Temp Setpoint
	FArray1D< DefineCondEntSetPointManager > CondEntSetPtMgr; // Condenser Entering Water Set Pt Mgr
	FArray1D< DefineIdealCondEntSetPointManager > IdealCondEntSetPtMgr; // Ideal Condenser Entering Set Pt Mgr
	FArray1D< DefineSZOneStageCoolinggSetPointManager > SZOneStageCoolingSetPtMgr; // single zone 1 stage cool
	FArray1D< DefineSZOneStageHeatingSetPointManager > SZOneStageHeatingSetPtMgr; // single zone 1 stage heat

	// Functions

	void
	ManageSetPoints()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Russ Taylor, Rick Strand
		//       DATE WRITTEN   May 1998
		//       MODIFIED       Fred Buhl May 2000
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:

		// METHODOLOGY EMPLOYED:
		// Each flag is checked and the appropriate manager is then called.

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int SetPtMgrNum; // loop index

		// First time ManageSetPoints is called, get the input for all the setpoint managers
		if ( GetInputFlag ) {
			GetSetPointManagerInputs();
			GetInputFlag = false;
		}

		InitSetPointManagers();

		if ( ManagerOn ) {
			SimSetPointManagers();
			UpdateSetPointManagers();
			// The Mixed Air Setpoint Managers (since they depend on other setpoints, they must be calculated
			// and updated next to last).
			for ( SetPtMgrNum = 1; SetPtMgrNum <= NumMixedAirSetPtMgrs; ++SetPtMgrNum ) {
				CalcMixedAirSetPoint( SetPtMgrNum );
			}
			UpdateMixedAirSetPoints();
			// The Outside Air Pretreat Setpoint Managers (since they depend on other setpoints, they must be calculated
			// and updated last).
			for ( SetPtMgrNum = 1; SetPtMgrNum <= NumOAPretreatSetPtMgrs; ++SetPtMgrNum ) {
				CalcOAPretreatSetPoint( SetPtMgrNum );
			}
			UpdateOAPretreatSetPoints();
		}

	}

	void
	GetSetPointManagerInputs()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   July 1998
		//       MODIFIED       Shirey/Raustad (FSEC), Jan 2004
		//                      Nov 2004 - Jan 2005 M. J. Witte, GARD Analytics, Inc.
		//                        Add new setpoint managers:
		//                          SET POINT MANAGER:SINGLE ZONE HEATING and
		//                          SET POINT MANAGER:SINGLE ZONE COOLING
		//                          SET POINT MANAGER:OUTSIDE AIR PRETREAT
		//                        Work supported by ASHRAE research project 1254-RP
		//                      Haves October 2004
		//                      Witte (GARD), Sep 2006
		//                      July 2010 B.A. Nigusse, FSEC/UCF
		//                        Added new setpoint managers:
		//                          SetpointManager:MultiZone:Heating:Average
		//                          SetpointManager:MultiZone:Cooling:Average
		//                          SetpointManager:MultiZone:MinimumHumidity:Average
		//                          SetpointManager:MultiZone:MaximumHumidity:Average
		//                      Aug 2010 B.A. Nigusse, FSEC/UCF
		//                        Added new setpoint managers:
		//                          SetpointManager:MultiZone:Humidity:Minimum
		//                          SetpointManager:MultiZone:Humidity:Maximum

		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE
		// Input the SetPointManager data and store it in the SetPtMgrIn array.
		// Examine the Controllers in the input data and determine which ones
		// will have their setpoints set by a particular Setpoint Manager.

		// METHODOLOGY EMPLOYED:
		// Use the Get routines from the InputProcessor module.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using InputProcessor::GetObjectItemNum;
		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using InputProcessor::MakeUPPERCase;
		using InputProcessor::GetObjectDefMaxArgs;
		using NodeInputManager::GetOnlySingleNode;
		using NodeInputManager::GetNodeNums;
		using DataHeatBalance::Zone;
		using ScheduleManager::GetScheduleIndex;
		using ScheduleManager::CheckScheduleValueMinMax;
		//    USE DataIPShortCuts
		using General::RoundSigDigits;
		using General::FindNumberInList;
		using DataEnvironment::GroundTemp_DeepObjInput;
		using DataEnvironment::GroundTempObjInput;
		using DataEnvironment::GroundTemp_SurfaceObjInput;
		using DataEnvironment::FCGroundTemps;
		using DataEnvironment::GroundTemp_Deep;
		using DataEnvironment::GroundTemp;
		using DataEnvironment::GroundTemp_Surface;
		using DataEnvironment::GroundTempFC;
		using DataZoneEquipment::GetSystemNodeNumberForZone;
		using DataZoneControls::StageZoneLogic;

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetSetPointManagerInputs: " ); // include trailing blank space

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		FArray1D_string cAlphaFieldNames;
		FArray1D_string cNumericFieldNames;
		FArray1D_bool lNumericFieldBlanks;
		FArray1D_bool lAlphaFieldBlanks;
		FArray1D_string cAlphaArgs;
		FArray1D< Real64 > rNumericArgs;
		std::string cCurrentModuleObject;
		static int MaxNumAlphas( 0 ); // argument for call to GetObjectDefMaxArgs
		static int MaxNumNumbers( 0 ); // argument for call to GetObjectDefMaxArgs

		int NumNums; // Number of real numbers returned by GetObjectItem
		int NumAlphas; // Number of alphanumerics returned by GetObjectItem
		int NumParams;
		int SetPtMgrNum; // Setpoint Manager index
		int AllSetPtMgrNum; // Setpoint Manager index to ALL setpoint managers in single TYPE
		int IOStat; // Status flag from GetObjectItem
		int NumNodesCtrld; // number of controlled nodes in input node list
		int CtrldNodeNum; // index of the items in the controlled node node list
		int NumZones; // number of zone nodes in input node list
		int ZoneNum; // loop index for zone nodes
		int NumNodes;
		FArray1D_int NodeNums;
		static bool ErrorsFound( false );
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		static bool NodeListError( false );
		bool ErrInList;
		int Found;
		static bool NoSurfaceGroundTempObjWarning( true ); // This will cause a warning to be issued if no "surface" ground
		// temperature object was input.
		static bool NoShallowGroundTempObjWarning( true ); // This will cause a warning to be issued if no "shallow" ground
		// temperature object was input.
		static bool NoDeepGroundTempObjWarning( true ); // This will cause a warning to be issued if no "deep" ground
		// temperature object was input.
		static bool NoFCGroundTempObjWarning( true ); // This will cause a warning to be issued if no ground
		// temperature object was input for FC Factor method

		NumNodesCtrld = 0;
		CtrldNodeNum = 0;
		NumZones = 0;
		ZoneNum = 0;

		cCurrentModuleObject = "SetpointManager:Scheduled";
		NumSchSetPtMgrs = GetNumObjectsFound( cCurrentModuleObject ); // 'SetpointManager:Scheduled'
		GetObjectDefMaxArgs( cCurrentModuleObject, NumParams, NumAlphas, NumNums );
		MaxNumNumbers = NumNums;
		MaxNumAlphas = NumAlphas;

		cCurrentModuleObject = "SetpointManager:Scheduled:DualSetpoint";
		NumDualSchSetPtMgrs = GetNumObjectsFound( cCurrentModuleObject ); // 'SetpointManager:Scheduled:DualSetpoint'
		GetObjectDefMaxArgs( cCurrentModuleObject, NumParams, NumAlphas, NumNums );
		MaxNumNumbers = max( MaxNumNumbers, NumNums );
		MaxNumAlphas = max( MaxNumAlphas, NumAlphas );

		cCurrentModuleObject = "SetpointManager:OutdoorAirReset";
		NumOutAirSetPtMgrs = GetNumObjectsFound( cCurrentModuleObject ); // 'SetpointManager:OutdoorAirReset'
		GetObjectDefMaxArgs( cCurrentModuleObject, NumParams, NumAlphas, NumNums );
		MaxNumNumbers = max( MaxNumNumbers, NumNums );
		MaxNumAlphas = max( MaxNumAlphas, NumAlphas );

		cCurrentModuleObject = "SetpointManager:SingleZone:Reheat";
		NumSZRhSetPtMgrs = GetNumObjectsFound( cCurrentModuleObject ); // 'SetpointManager:SingleZone:Reheat'
		GetObjectDefMaxArgs( cCurrentModuleObject, NumParams, NumAlphas, NumNums );
		MaxNumNumbers = max( MaxNumNumbers, NumNums );
		MaxNumAlphas = max( MaxNumAlphas, NumAlphas );

		cCurrentModuleObject = "SetpointManager:SingleZone:Heating";
		NumSZHtSetPtMgrs = GetNumObjectsFound( cCurrentModuleObject ); // 'SetpointManager:SingleZone:Heating'
		GetObjectDefMaxArgs( cCurrentModuleObject, NumParams, NumAlphas, NumNums );
		MaxNumNumbers = max( MaxNumNumbers, NumNums );
		MaxNumAlphas = max( MaxNumAlphas, NumAlphas );

		cCurrentModuleObject = "SetpointManager:SingleZone:Cooling";
		NumSZClSetPtMgrs = GetNumObjectsFound( cCurrentModuleObject ); // 'SetpointManager:SingleZone:Cooling'
		GetObjectDefMaxArgs( cCurrentModuleObject, NumParams, NumAlphas, NumNums );
		MaxNumNumbers = max( MaxNumNumbers, NumNums );
		MaxNumAlphas = max( MaxNumAlphas, NumAlphas );

		cCurrentModuleObject = "SetpointManager:SingleZone:Humidity:Minimum";
		NumSZMinHumSetPtMgrs = GetNumObjectsFound( cCurrentModuleObject ); // 'SetpointManager:SingleZone:Humidity:Minimum'
		GetObjectDefMaxArgs( cCurrentModuleObject, NumParams, NumAlphas, NumNums );
		MaxNumNumbers = max( MaxNumNumbers, NumNums );
		MaxNumAlphas = max( MaxNumAlphas, NumAlphas );

		cCurrentModuleObject = "SetpointManager:SingleZone:Humidity:Maximum";
		NumSZMaxHumSetPtMgrs = GetNumObjectsFound( cCurrentModuleObject ); // 'SetpointManager:SingleZone:Humidity:Maximum'
		GetObjectDefMaxArgs( cCurrentModuleObject, NumParams, NumAlphas, NumNums );
		MaxNumNumbers = max( MaxNumNumbers, NumNums );
		MaxNumAlphas = max( MaxNumAlphas, NumAlphas );

		cCurrentModuleObject = "SetpointManager:MixedAir";
		NumMixedAirSetPtMgrs = GetNumObjectsFound( cCurrentModuleObject ); // 'SetpointManager:MixedAir'
		GetObjectDefMaxArgs( cCurrentModuleObject, NumParams, NumAlphas, NumNums );
		MaxNumNumbers = max( MaxNumNumbers, NumNums );
		MaxNumAlphas = max( MaxNumAlphas, NumAlphas );

		cCurrentModuleObject = "SetpointManager:OutdoorAirPretreat";
		NumOAPretreatSetPtMgrs = GetNumObjectsFound( cCurrentModuleObject ); // 'SetpointManager:OutdoorAirPretreat'
		GetObjectDefMaxArgs( cCurrentModuleObject, NumParams, NumAlphas, NumNums );
		MaxNumNumbers = max( MaxNumNumbers, NumNums );
		MaxNumAlphas = max( MaxNumAlphas, NumAlphas );

		cCurrentModuleObject = "SetpointManager:Warmest";
		NumWarmestSetPtMgrs = GetNumObjectsFound( cCurrentModuleObject ); // 'SetpointManager:Warmest'
		GetObjectDefMaxArgs( cCurrentModuleObject, NumParams, NumAlphas, NumNums );
		MaxNumNumbers = max( MaxNumNumbers, NumNums );
		MaxNumAlphas = max( MaxNumAlphas, NumAlphas );

		cCurrentModuleObject = "SetpointManager:Coldest";
		NumColdestSetPtMgrs = GetNumObjectsFound( cCurrentModuleObject ); // 'SetpointManager:Coldest'
		GetObjectDefMaxArgs( cCurrentModuleObject, NumParams, NumAlphas, NumNums );
		MaxNumNumbers = max( MaxNumNumbers, NumNums );
		MaxNumAlphas = max( MaxNumAlphas, NumAlphas );

		cCurrentModuleObject = "SetpointManager:WarmestTemperatureFlow";
		NumWarmestSetPtMgrsTempFlow = GetNumObjectsFound( cCurrentModuleObject ); // 'SetpointManager:WarmestTemperatureFlow'
		GetObjectDefMaxArgs( cCurrentModuleObject, NumParams, NumAlphas, NumNums );
		MaxNumNumbers = max( MaxNumNumbers, NumNums );
		MaxNumAlphas = max( MaxNumAlphas, NumAlphas );

		cCurrentModuleObject = "SetpointManager:ReturnAirBypassFlow";
		NumRABFlowSetPtMgrs = GetNumObjectsFound( cCurrentModuleObject ); // 'SetpointManager:ReturnAirBypassFlow'
		GetObjectDefMaxArgs( cCurrentModuleObject, NumParams, NumAlphas, NumNums );
		MaxNumNumbers = max( MaxNumNumbers, NumNums );
		MaxNumAlphas = max( MaxNumAlphas, NumAlphas );

		cCurrentModuleObject = "SetpointManager:MultiZone:Cooling:Average";
		NumMZClgAverageSetPtMgrs = GetNumObjectsFound( cCurrentModuleObject ); // 'SetpointManager:MultiZone:Cooling:Average'
		GetObjectDefMaxArgs( cCurrentModuleObject, NumParams, NumAlphas, NumNums );
		MaxNumNumbers = max( MaxNumNumbers, NumNums );
		MaxNumAlphas = max( MaxNumAlphas, NumAlphas );

		cCurrentModuleObject = "SetpointManager:MultiZone:Heating:Average";
		NumMZHtgAverageSetPtMgrs = GetNumObjectsFound( cCurrentModuleObject ); // 'SetpointManager:MultiZone:Heating:Average'
		GetObjectDefMaxArgs( cCurrentModuleObject, NumParams, NumAlphas, NumNums );
		MaxNumNumbers = max( MaxNumNumbers, NumNums );
		MaxNumAlphas = max( MaxNumAlphas, NumAlphas );

		cCurrentModuleObject = "SetpointManager:MultiZone:MinimumHumidity:Average";
		NumMZAverageMinHumSetPtMgrs = GetNumObjectsFound( cCurrentModuleObject ); // 'SetpointManager:MultiZone:MinimumHumidity:Average'
		GetObjectDefMaxArgs( cCurrentModuleObject, NumParams, NumAlphas, NumNums );
		MaxNumNumbers = max( MaxNumNumbers, NumNums );
		MaxNumAlphas = max( MaxNumAlphas, NumAlphas );

		cCurrentModuleObject = "SetpointManager:MultiZone:MaximumHumidity:Average";
		NumMZAverageMaxHumSetPtMgrs = GetNumObjectsFound( cCurrentModuleObject ); // 'SetpointManager:MultiZone:MaximumHumidity:Average'
		GetObjectDefMaxArgs( cCurrentModuleObject, NumParams, NumAlphas, NumNums );
		MaxNumNumbers = max( MaxNumNumbers, NumNums );
		MaxNumAlphas = max( MaxNumAlphas, NumAlphas );

		cCurrentModuleObject = "SetpointManager:MultiZone:Humidity:Minimum";
		NumMZMinHumSetPtMgrs = GetNumObjectsFound( cCurrentModuleObject ); // 'SetpointManager:MultiZone:Humidity:Minimum'
		GetObjectDefMaxArgs( cCurrentModuleObject, NumParams, NumAlphas, NumNums );
		MaxNumNumbers = max( MaxNumNumbers, NumNums );
		MaxNumAlphas = max( MaxNumAlphas, NumAlphas );

		cCurrentModuleObject = "SetpointManager:MultiZone:Humidity:Maximum";
		NumMZMaxHumSetPtMgrs = GetNumObjectsFound( cCurrentModuleObject ); // 'SetpointManager:MultiZone:Humidity:Maximum'
		GetObjectDefMaxArgs( cCurrentModuleObject, NumParams, NumAlphas, NumNums );
		MaxNumNumbers = max( MaxNumNumbers, NumNums );
		MaxNumAlphas = max( MaxNumAlphas, NumAlphas );

		cCurrentModuleObject = "SetpointManager:FollowOutdoorAirTemperature";
		NumFollowOATempSetPtMgrs = GetNumObjectsFound( cCurrentModuleObject ); // 'SetpointManager:FollowOutdoorAirTemperature'
		GetObjectDefMaxArgs( cCurrentModuleObject, NumParams, NumAlphas, NumNums );
		MaxNumNumbers = max( MaxNumNumbers, NumNums );
		MaxNumAlphas = max( MaxNumAlphas, NumAlphas );

		cCurrentModuleObject = "SetpointManager:FollowSystemNodeTemperature";
		NumFollowSysNodeTempSetPtMgrs = GetNumObjectsFound( cCurrentModuleObject ); // 'SetpointManager:FollowSystemNodeTemperature'
		GetObjectDefMaxArgs( cCurrentModuleObject, NumParams, NumAlphas, NumNums );
		MaxNumNumbers = max( MaxNumNumbers, NumNums );
		MaxNumAlphas = max( MaxNumAlphas, NumAlphas );

		cCurrentModuleObject = "SetpointManager:FollowGroundTemperature";
		NumGroundTempSetPtMgrs = GetNumObjectsFound( cCurrentModuleObject ); // 'SetpointManager:FollowGroundTemperature'
		GetObjectDefMaxArgs( cCurrentModuleObject, NumParams, NumAlphas, NumNums );
		MaxNumNumbers = max( MaxNumNumbers, NumNums );
		MaxNumAlphas = max( MaxNumAlphas, NumAlphas );

		cCurrentModuleObject = "SetpointManager:CondenserEnteringReset";
		NumCondEntSetPtMgrs = GetNumObjectsFound( cCurrentModuleObject ); // 'SetpointManager:CondenserEnteringReset'
		GetObjectDefMaxArgs( cCurrentModuleObject, NumParams, NumAlphas, NumNums );
		MaxNumNumbers = max( MaxNumNumbers, NumNums );
		MaxNumAlphas = max( MaxNumAlphas, NumAlphas );

		cCurrentModuleObject = "SetpointManager:CondenserEnteringReset:Ideal";
		NumIdealCondEntSetPtMgrs = GetNumObjectsFound( cCurrentModuleObject ); // 'SetpointManager:CondenserEnteringReset:Ideal'
		GetObjectDefMaxArgs( cCurrentModuleObject, NumParams, NumAlphas, NumNums );
		MaxNumNumbers = max( MaxNumNumbers, NumNums );
		MaxNumAlphas = max( MaxNumAlphas, NumAlphas );

		cCurrentModuleObject = "SetpointManager:SingleZone:OneStageCooling";
		NumSZOneStageCoolingSetPtMgrs = GetNumObjectsFound( cCurrentModuleObject );
		GetObjectDefMaxArgs( cCurrentModuleObject, NumParams, NumAlphas, NumNums );
		MaxNumNumbers = max( MaxNumNumbers, NumNums );
		MaxNumAlphas = max( MaxNumAlphas, NumAlphas );

		cCurrentModuleObject = "SetpointManager:SingleZone:OneStageHeating";
		NumSZOneStageHeatingSetPtMgrs = GetNumObjectsFound( cCurrentModuleObject );
		GetObjectDefMaxArgs( cCurrentModuleObject, NumParams, NumAlphas, NumNums );
		MaxNumNumbers = max( MaxNumNumbers, NumNums );
		MaxNumAlphas = max( MaxNumAlphas, NumAlphas );

		NumAllSetPtMgrs = NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs + NumSZRhSetPtMgrs + NumSZHtSetPtMgrs + NumSZClSetPtMgrs + NumSZMinHumSetPtMgrs + NumSZMaxHumSetPtMgrs + NumMixedAirSetPtMgrs + NumOAPretreatSetPtMgrs + NumWarmestSetPtMgrs + NumColdestSetPtMgrs + NumWarmestSetPtMgrsTempFlow + NumRABFlowSetPtMgrs + NumMZClgAverageSetPtMgrs + NumMZHtgAverageSetPtMgrs + NumMZAverageMinHumSetPtMgrs + NumMZAverageMaxHumSetPtMgrs + NumMZMinHumSetPtMgrs + NumMZMaxHumSetPtMgrs + NumFollowOATempSetPtMgrs + NumFollowSysNodeTempSetPtMgrs + NumGroundTempSetPtMgrs + NumCondEntSetPtMgrs + NumIdealCondEntSetPtMgrs + NumSZOneStageCoolingSetPtMgrs + NumSZOneStageHeatingSetPtMgrs;

		cAlphaFieldNames.allocate( MaxNumAlphas );
		cAlphaArgs.allocate( MaxNumAlphas );
		lAlphaFieldBlanks.dimension( MaxNumAlphas, false );
		cNumericFieldNames.allocate( MaxNumNumbers );
		rNumericArgs.dimension( MaxNumNumbers, 0.0 );
		lNumericFieldBlanks.dimension( MaxNumNumbers, false );

		GetObjectDefMaxArgs( "NodeList", NumParams, NumAlphas, NumNums );
		NodeNums.dimension( NumParams, 0 );

		if ( NumAllSetPtMgrs > 0 ) AllSetPtMgr.allocate( NumAllSetPtMgrs ); // Allocate the entire Setpoint Manager input data array

		// Input the Scheduled Setpoint Managers

		if ( NumSchSetPtMgrs > 0 ) SchSetPtMgr.allocate( NumSchSetPtMgrs ); // Allocate the Setpoint Manager input data array

		// Input the data for each Setpoint Manager

		cCurrentModuleObject = "SetpointManager:Scheduled";

		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumSchSetPtMgrs; ++SetPtMgrNum ) {
			GetObjectItem( cCurrentModuleObject, SetPtMgrNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), SchSetPtMgr.Name(), SetPtMgrNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			SchSetPtMgr( SetPtMgrNum ).Name = cAlphaArgs( 1 );
			SchSetPtMgr( SetPtMgrNum ).CtrlVarType = cAlphaArgs( 2 );
			// setup program flow control integers
			if ( SameString( SchSetPtMgr( SetPtMgrNum ).CtrlVarType, "Temperature" ) ) {
				SchSetPtMgr( SetPtMgrNum ).CtrlTypeMode = iCtrlVarType_Temp;
			} else if ( SameString( SchSetPtMgr( SetPtMgrNum ).CtrlVarType, "MaximumTemperature" ) ) {
				SchSetPtMgr( SetPtMgrNum ).CtrlTypeMode = iCtrlVarType_MaxTemp;
			} else if ( SameString( SchSetPtMgr( SetPtMgrNum ).CtrlVarType, "MinimumTemperature" ) ) {
				SchSetPtMgr( SetPtMgrNum ).CtrlTypeMode = iCtrlVarType_MinTemp;
			} else if ( SameString( SchSetPtMgr( SetPtMgrNum ).CtrlVarType, "HumidityRatio" ) ) {
				SchSetPtMgr( SetPtMgrNum ).CtrlTypeMode = iCtrlVarType_HumRat;
			} else if ( SameString( SchSetPtMgr( SetPtMgrNum ).CtrlVarType, "MaximumHumidityRatio" ) ) {
				SchSetPtMgr( SetPtMgrNum ).CtrlTypeMode = iCtrlVarType_MaxHumRat;
			} else if ( SameString( SchSetPtMgr( SetPtMgrNum ).CtrlVarType, "MinimumHumidityRatio" ) ) {
				SchSetPtMgr( SetPtMgrNum ).CtrlTypeMode = iCtrlVarType_MinHumRat;
			} else if ( SameString( SchSetPtMgr( SetPtMgrNum ).CtrlVarType, "MassFlowRate" ) ) {
				SchSetPtMgr( SetPtMgrNum ).CtrlTypeMode = iCtrlVarType_MassFlow;
			} else if ( SameString( SchSetPtMgr( SetPtMgrNum ).CtrlVarType, "MaximumMassFlowRate" ) ) {
				SchSetPtMgr( SetPtMgrNum ).CtrlTypeMode = iCtrlVarType_MaxMassFlow;
			} else if ( SameString( SchSetPtMgr( SetPtMgrNum ).CtrlVarType, "MinimumMassFlowRate" ) ) {
				SchSetPtMgr( SetPtMgrNum ).CtrlTypeMode = iCtrlVarType_MinMassFlow;
			} else {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid field." );
				ShowContinueError( "..invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\"." );
				ShowContinueError( "..Valid values are \"Temperature\",\"MaximumTemperature\",\"MinimumTemperature\"," );
				ShowContinueError( "     \"HumidityRatio\",\"MaximumHumidityRatio\",\"MinimumHumidityRatio\",\"MassFlowRate\"," );
				ShowContinueError( "     \"MaximumMassFlowRate\" or \"MinimumMassFlowRate\"" );
				ErrorsFound = true;
			}

			SchSetPtMgr( SetPtMgrNum ).Sched = cAlphaArgs( 3 );
			SchSetPtMgr( SetPtMgrNum ).SchedPtr = GetScheduleIndex( cAlphaArgs( 3 ) );
			if ( SchSetPtMgr( SetPtMgrNum ).SchedPtr == 0 ) {
				if ( lAlphaFieldBlanks( 3 ) ) {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", blank required field." );
					ShowContinueError( "..required field " + cAlphaFieldNames( 3 ) );
				} else {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid field." );
					ShowContinueError( "..invalid " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\"." );
				}
				ErrorsFound = true;
			}
			SchSetPtMgr( SetPtMgrNum ).CtrlNodeListName = cAlphaArgs( 4 );
			NodeListError = false;
			GetNodeNums( SchSetPtMgr( SetPtMgrNum ).CtrlNodeListName, NumNodes, NodeNums, NodeListError, NodeType_Unknown, cCurrentModuleObject, cAlphaArgs( 1 ), NodeConnectionType_SetPoint, 1, ObjectIsNotParent, _, cAlphaFieldNames( 4 ) );

			if ( ! NodeListError ) {
				NumNodesCtrld = NumNodes;
				SchSetPtMgr( SetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				SchSetPtMgr( SetPtMgrNum ).NumCtrlNodes = NumNodesCtrld;
				SchSetPtMgr( SetPtMgrNum ).SetPt = 0.0;

				for ( CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum ) {
					SchSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrldNodeNum ) = NodeNums( CtrldNodeNum );
				}
			} else {
				//    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
				//       '", invalid field.')
				//    Call ShowContinueError('..invalid '//TRIM(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))//'".')
				ErrorsFound = true;
			}

			AllSetPtMgrNum = SetPtMgrNum;

			if ( ! NodeListError ) {
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes = SchSetPtMgr( SetPtMgrNum ).CtrlNodes;
			}
			AllSetPtMgr( AllSetPtMgrNum ).Name = SchSetPtMgr( SetPtMgrNum ).Name;
			AllSetPtMgr( AllSetPtMgrNum ).SPMType = iSPMType_Scheduled;
			AllSetPtMgr( AllSetPtMgrNum ).CtrlTypeMode = SchSetPtMgr( SetPtMgrNum ).CtrlTypeMode;
			AllSetPtMgr( AllSetPtMgrNum ).NumCtrlNodes = SchSetPtMgr( SetPtMgrNum ).NumCtrlNodes;

		}

		// Input the Scheduled Setpoint Managers DUAL SETPOINT

		if ( NumDualSchSetPtMgrs > 0 ) DualSchSetPtMgr.allocate( NumDualSchSetPtMgrs ); // Allocate the Setpoint Manager input data array

		// Input the data for each Setpoint Manager
		cCurrentModuleObject = "SetpointManager:Scheduled:DualSetpoint";

		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumDualSchSetPtMgrs; ++SetPtMgrNum ) {
			GetObjectItem( cCurrentModuleObject, SetPtMgrNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), DualSchSetPtMgr.Name(), SetPtMgrNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			DualSchSetPtMgr( SetPtMgrNum ).Name = cAlphaArgs( 1 );
			DualSchSetPtMgr( SetPtMgrNum ).CtrlVarType = cAlphaArgs( 2 );
			if ( SameString( DualSchSetPtMgr( SetPtMgrNum ).CtrlVarType, "Temperature" ) ) {
				DualSchSetPtMgr( SetPtMgrNum ).CtrlTypeMode = iCtrlVarType_Temp;
			} else {
				// should not come here if idd type choice and key list is working
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid field." );
				ShowContinueError( "..invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\"." );
				ShowContinueError( "..Valid value is \"Temperature\"." );
				ErrorsFound = true;
			}
			DualSchSetPtMgr( SetPtMgrNum ).SchedHi = cAlphaArgs( 3 );
			DualSchSetPtMgr( SetPtMgrNum ).SchedPtrHi = GetScheduleIndex( cAlphaArgs( 3 ) );
			if ( DualSchSetPtMgr( SetPtMgrNum ).SchedPtrHi == 0 ) {
				if ( lAlphaFieldBlanks( 3 ) ) {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", blank required field." );
					ShowContinueError( "..required field " + cAlphaFieldNames( 3 ) );
				} else {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid field." );
					ShowContinueError( "..invalid " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\"." );
				}
				ErrorsFound = true;
			}
			DualSchSetPtMgr( SetPtMgrNum ).SchedLo = cAlphaArgs( 4 );
			DualSchSetPtMgr( SetPtMgrNum ).SchedPtrLo = GetScheduleIndex( cAlphaArgs( 4 ) );
			if ( DualSchSetPtMgr( SetPtMgrNum ).SchedPtrLo == 0 ) {
				if ( lAlphaFieldBlanks( 4 ) ) {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", blank required field." );
					ShowContinueError( "..required field " + cAlphaFieldNames( 4 ) );
				} else {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid field." );
					ShowContinueError( "..invalid " + cAlphaFieldNames( 4 ) + "=\"" + cAlphaArgs( 4 ) + "\"." );
				}
				ErrorsFound = true;
			}
			DualSchSetPtMgr( SetPtMgrNum ).CtrlNodeListName = cAlphaArgs( 5 );
			NodeListError = false;
			GetNodeNums( DualSchSetPtMgr( SetPtMgrNum ).CtrlNodeListName, NumNodes, NodeNums, NodeListError, NodeType_Unknown, cCurrentModuleObject, cAlphaArgs( 1 ), NodeConnectionType_SetPoint, 1, ObjectIsNotParent, _, cAlphaFieldNames( 5 ) );

			if ( ! NodeListError ) {
				NumNodesCtrld = NumNodes;
				DualSchSetPtMgr( SetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				DualSchSetPtMgr( SetPtMgrNum ).NumCtrlNodes = NumNodesCtrld;
				DualSchSetPtMgr( SetPtMgrNum ).SetPtHi = 0.0;
				DualSchSetPtMgr( SetPtMgrNum ).SetPtLo = 0.0;

				for ( CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum ) {
					DualSchSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrldNodeNum ) = NodeNums( CtrldNodeNum );
				}
			} else { // check getnodenums/nodelist
				//    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
				//       '", invalid field.')
				//    Call ShowContinueError('..invalid '//TRIM(cAlphaFieldNames(5))//'="'//TRIM(cAlphaArgs(5))//'".')
				ErrorsFound = true;
			}

			AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs;

			if ( ! NodeListError ) {
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes = DualSchSetPtMgr( SetPtMgrNum ).CtrlNodes;
			}
			AllSetPtMgr( AllSetPtMgrNum ).Name = DualSchSetPtMgr( SetPtMgrNum ).Name;
			AllSetPtMgr( AllSetPtMgrNum ).SPMType = iSPMType_ScheduledDual;
			AllSetPtMgr( AllSetPtMgrNum ).CtrlTypeMode = DualSchSetPtMgr( SetPtMgrNum ).CtrlTypeMode;
			AllSetPtMgr( AllSetPtMgrNum ).NumCtrlNodes = DualSchSetPtMgr( SetPtMgrNum ).NumCtrlNodes;

		}

		// Input the Outside Air Setpoint Managers

		if ( NumOutAirSetPtMgrs > 0 ) OutAirSetPtMgr.allocate( NumOutAirSetPtMgrs ); // Allocate the Setpoint Manager input data array

		// Input the data for each Setpoint Manager
		cCurrentModuleObject = "SetpointManager:OutdoorAirReset";

		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumOutAirSetPtMgrs; ++SetPtMgrNum ) {
			GetObjectItem( cCurrentModuleObject, SetPtMgrNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), OutAirSetPtMgr.Name(), SetPtMgrNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			OutAirSetPtMgr( SetPtMgrNum ).Name = cAlphaArgs( 1 );
			OutAirSetPtMgr( SetPtMgrNum ).CtrlVarType = cAlphaArgs( 2 );
			if ( SameString( OutAirSetPtMgr( SetPtMgrNum ).CtrlVarType, "Temperature" ) ) {
				OutAirSetPtMgr( SetPtMgrNum ).CtrlTypeMode = iCtrlVarType_Temp;
			} else {
				// should not come here if idd type choice and key list is working
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid field." );
				ShowContinueError( "..invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\"." );
				ShowContinueError( "..Valid value is \"Temperature\"." );
				ErrorsFound = true;
			}
			OutAirSetPtMgr( SetPtMgrNum ).OutLowSetPt1 = rNumericArgs( 1 );
			OutAirSetPtMgr( SetPtMgrNum ).OutLow1 = rNumericArgs( 2 );
			OutAirSetPtMgr( SetPtMgrNum ).OutHighSetPt1 = rNumericArgs( 3 );
			OutAirSetPtMgr( SetPtMgrNum ).OutHigh1 = rNumericArgs( 4 );
			OutAirSetPtMgr( SetPtMgrNum ).CtrlNodeListName = cAlphaArgs( 3 );
			if ( OutAirSetPtMgr( SetPtMgrNum ).OutHigh1 < OutAirSetPtMgr( SetPtMgrNum ).OutLow1 ) {
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid setpoints." );
				ShowContinueError( "..." + cNumericFieldNames( 4 ) + "=[" + RoundSigDigits( OutAirSetPtMgr( SetPtMgrNum ).OutHigh1, 1 ) + "] is less than " + cNumericFieldNames( 2 ) + "=[" + RoundSigDigits( OutAirSetPtMgr( SetPtMgrNum ).OutLow1, 1 ) + "]." );
			}
			// Get optional input: schedule and 2nd reset rule
			if ( NumAlphas == 4 && NumNums == 8 ) {
				OutAirSetPtMgr( SetPtMgrNum ).Sched = cAlphaArgs( 4 );
				OutAirSetPtMgr( SetPtMgrNum ).SchedPtr = GetScheduleIndex( cAlphaArgs( 4 ) );
				// Schedule is optional here, so no check on SchedPtr
				OutAirSetPtMgr( SetPtMgrNum ).OutLowSetPt2 = rNumericArgs( 5 );
				OutAirSetPtMgr( SetPtMgrNum ).OutLow2 = rNumericArgs( 6 );
				OutAirSetPtMgr( SetPtMgrNum ).OutHighSetPt2 = rNumericArgs( 7 );
				OutAirSetPtMgr( SetPtMgrNum ).OutHigh2 = rNumericArgs( 8 );
				if ( OutAirSetPtMgr( SetPtMgrNum ).OutHigh2 < OutAirSetPtMgr( SetPtMgrNum ).OutLow2 ) {
					ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid setpoints." );
					ShowContinueError( "..." + cNumericFieldNames( 8 ) + "=[" + RoundSigDigits( OutAirSetPtMgr( SetPtMgrNum ).OutHigh2, 1 ) + "] is less than " + cNumericFieldNames( 6 ) + "=[" + RoundSigDigits( OutAirSetPtMgr( SetPtMgrNum ).OutLow2, 1 ) + "]." );
				}
			} else {
				OutAirSetPtMgr( SetPtMgrNum ).Sched = "";
				OutAirSetPtMgr( SetPtMgrNum ).SchedPtr = 0;
				OutAirSetPtMgr( SetPtMgrNum ).OutLowSetPt2 = 0.0;
				OutAirSetPtMgr( SetPtMgrNum ).OutLow2 = 0.0;
				OutAirSetPtMgr( SetPtMgrNum ).OutHighSetPt2 = 0.0;
				OutAirSetPtMgr( SetPtMgrNum ).OutHigh2 = 0.0;
			}
			NodeListError = false;
			GetNodeNums( OutAirSetPtMgr( SetPtMgrNum ).CtrlNodeListName, NumNodes, NodeNums, NodeListError, NodeType_Unknown, cCurrentModuleObject, cAlphaArgs( 1 ), NodeConnectionType_SetPoint, 1, ObjectIsNotParent, _, cAlphaFieldNames( 3 ) );
			if ( ! NodeListError ) {
				NumNodesCtrld = NumNodes;
				OutAirSetPtMgr( SetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				OutAirSetPtMgr( SetPtMgrNum ).NumCtrlNodes = NumNodesCtrld;
				OutAirSetPtMgr( SetPtMgrNum ).SetPt = 0.0;

				for ( CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum ) {
					OutAirSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrldNodeNum ) = NodeNums( CtrldNodeNum );
				}
			} else {
				//    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
				//       '", invalid field.')
				//    Call ShowContinueError('..invalid '//TRIM(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'".')
				ErrorsFound = true;
			}

			AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs;

			if ( ! NodeListError ) {
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes = OutAirSetPtMgr( SetPtMgrNum ).CtrlNodes;
			}
			AllSetPtMgr( AllSetPtMgrNum ).Name = OutAirSetPtMgr( SetPtMgrNum ).Name;
			AllSetPtMgr( AllSetPtMgrNum ).SPMType = iSPMType_OutsideAir;
			AllSetPtMgr( AllSetPtMgrNum ).CtrlTypeMode = OutAirSetPtMgr( SetPtMgrNum ).CtrlTypeMode;
			AllSetPtMgr( AllSetPtMgrNum ).NumCtrlNodes = OutAirSetPtMgr( SetPtMgrNum ).NumCtrlNodes;

		}

		// Input the Single Zone Reheat Setpoint Managers

		if ( NumSZRhSetPtMgrs > 0 ) SingZoneRhSetPtMgr.allocate( NumSZRhSetPtMgrs ); // Allocate the Setpoint Manager input data array

		// Input the data for each Setpoint Manager
		cCurrentModuleObject = "SetpointManager:SingleZone:Reheat";

		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumSZRhSetPtMgrs; ++SetPtMgrNum ) {
			GetObjectItem( cCurrentModuleObject, SetPtMgrNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), SingZoneRhSetPtMgr.Name(), SetPtMgrNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			SingZoneRhSetPtMgr( SetPtMgrNum ).Name = cAlphaArgs( 1 );
			SingZoneRhSetPtMgr( SetPtMgrNum ).CtrlVarType = cAlphaArgs( 2 );
			if ( SameString( SingZoneRhSetPtMgr( SetPtMgrNum ).CtrlVarType, "Temperature" ) ) {
				SingZoneRhSetPtMgr( SetPtMgrNum ).CtrlTypeMode = iCtrlVarType_Temp;
			} else {
				// should not come here if idd type choice and key list is working
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid field." );
				ShowContinueError( "..invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\"." );
				ShowContinueError( "..Valid value is \"Temperature\"." );
				ErrorsFound = true;
			}
			SingZoneRhSetPtMgr( SetPtMgrNum ).ControlZoneName = cAlphaArgs( 3 );
			SingZoneRhSetPtMgr( SetPtMgrNum ).MinSetTemp = rNumericArgs( 1 );
			SingZoneRhSetPtMgr( SetPtMgrNum ).MaxSetTemp = rNumericArgs( 2 );
			if ( SingZoneRhSetPtMgr( SetPtMgrNum ).MaxSetTemp < SingZoneRhSetPtMgr( SetPtMgrNum ).MinSetTemp ) {
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"," );
				ShowContinueError( "..." + cNumericFieldNames( 2 ) + "=[" + RoundSigDigits( SingZoneRhSetPtMgr( SetPtMgrNum ).MaxSetTemp, 1 ) + "] is less than " + cNumericFieldNames( 1 ) + "=[" + RoundSigDigits( SingZoneRhSetPtMgr( SetPtMgrNum ).MinSetTemp, 1 ) + "]." );
			}
			SingZoneRhSetPtMgr( SetPtMgrNum ).ZoneNodeNum = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Sensor, 1, ObjectIsNotParent );
			SingZoneRhSetPtMgr( SetPtMgrNum ).ZoneInletNodeNum = GetOnlySingleNode( cAlphaArgs( 5 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Sensor, 1, ObjectIsNotParent );
			NodeListError = false;
			GetNodeNums( cAlphaArgs( 6 ), NumNodes, NodeNums, NodeListError, NodeType_Unknown, cCurrentModuleObject, cAlphaArgs( 1 ), NodeConnectionType_SetPoint, 1, ObjectIsNotParent, _, cAlphaFieldNames( 6 ) ); // setpoint nodes
			if ( ! NodeListError ) {
				NumNodesCtrld = NumNodes;
				SingZoneRhSetPtMgr( SetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				SingZoneRhSetPtMgr( SetPtMgrNum ).NumCtrlNodes = NumNodesCtrld;
				SingZoneRhSetPtMgr( SetPtMgrNum ).SetPt = 0.0;

				for ( CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum ) {
					SingZoneRhSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrldNodeNum ) = NodeNums( CtrldNodeNum );
				}
			} else {
				//    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
				//       '", invalid field.')
				//    Call ShowContinueError('..invalid '//TRIM(cAlphaFieldNames(6))//'="'//TRIM(cAlphaArgs(6))//'".')
				ErrorsFound = true;
			}

			// get the actual zone number of the control zone
			SingZoneRhSetPtMgr( SetPtMgrNum ).ControlZoneNum = FindItemInList( cAlphaArgs( 3 ), Zone.Name(), NumOfZones );
			if ( SingZoneRhSetPtMgr( SetPtMgrNum ).ControlZoneNum == 0 ) {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid field." );
				ShowContinueError( "..invalid " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\"." );
				ErrorsFound = true;
			}
			SingZoneRhSetPtMgr( SetPtMgrNum ).SetPt = 0.0;

			AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs;

			if ( ! NodeListError ) {
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes = SingZoneRhSetPtMgr( SetPtMgrNum ).CtrlNodes;
			}
			AllSetPtMgr( AllSetPtMgrNum ).Name = SingZoneRhSetPtMgr( SetPtMgrNum ).Name;
			AllSetPtMgr( AllSetPtMgrNum ).SPMType = iSPMType_SZReheat;
			AllSetPtMgr( AllSetPtMgrNum ).CtrlTypeMode = SingZoneRhSetPtMgr( SetPtMgrNum ).CtrlTypeMode;
			AllSetPtMgr( AllSetPtMgrNum ).NumCtrlNodes = SingZoneRhSetPtMgr( SetPtMgrNum ).NumCtrlNodes;

		}

		// Input the Single Zone Heating Setpoint Managers

		if ( NumSZHtSetPtMgrs > 0 ) SingZoneHtSetPtMgr.allocate( NumSZHtSetPtMgrs ); // Allocate the Setpoint Manager input data array

		// Input the data for each Setpoint Manager
		cCurrentModuleObject = "SetpointManager:SingleZone:Heating";

		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumSZHtSetPtMgrs; ++SetPtMgrNum ) {
			GetObjectItem( cCurrentModuleObject, SetPtMgrNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), SingZoneHtSetPtMgr.Name(), SetPtMgrNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			SingZoneHtSetPtMgr( SetPtMgrNum ).Name = cAlphaArgs( 1 );
			SingZoneHtSetPtMgr( SetPtMgrNum ).CtrlVarType = cAlphaArgs( 2 );
			if ( SameString( SingZoneHtSetPtMgr( SetPtMgrNum ).CtrlVarType, "Temperature" ) ) {
				SingZoneHtSetPtMgr( SetPtMgrNum ).CtrlTypeMode = iCtrlVarType_Temp;
			} else {
				// should not come here if idd type choice and key list is working
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid field." );
				ShowContinueError( "..invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\"." );
				ShowContinueError( "..Valid value is \"Temperature\"." );
				ErrorsFound = true;
			}
			SingZoneHtSetPtMgr( SetPtMgrNum ).ControlZoneName = cAlphaArgs( 3 );
			SingZoneHtSetPtMgr( SetPtMgrNum ).MinSetTemp = rNumericArgs( 1 );
			SingZoneHtSetPtMgr( SetPtMgrNum ).MaxSetTemp = rNumericArgs( 2 );
			if ( SingZoneHtSetPtMgr( SetPtMgrNum ).MaxSetTemp < SingZoneHtSetPtMgr( SetPtMgrNum ).MinSetTemp ) {
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"," );
				ShowContinueError( "..." + cNumericFieldNames( 2 ) + "=[" + RoundSigDigits( SingZoneHtSetPtMgr( SetPtMgrNum ).MaxSetTemp, 1 ) + "] is less than " + cNumericFieldNames( 1 ) + "=[" + RoundSigDigits( SingZoneHtSetPtMgr( SetPtMgrNum ).MinSetTemp, 1 ) + "]." );
			}
			SingZoneHtSetPtMgr( SetPtMgrNum ).ZoneNodeNum = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Sensor, 1, ObjectIsNotParent );
			SingZoneHtSetPtMgr( SetPtMgrNum ).ZoneInletNodeNum = GetOnlySingleNode( cAlphaArgs( 5 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Sensor, 1, ObjectIsNotParent );
			NodeListError = false;
			GetNodeNums( cAlphaArgs( 6 ), NumNodes, NodeNums, NodeListError, NodeType_Unknown, cCurrentModuleObject, cAlphaArgs( 1 ), NodeConnectionType_SetPoint, 1, ObjectIsNotParent, _, cAlphaFieldNames( 6 ) ); // setpoint nodes
			if ( ! NodeListError ) {
				NumNodesCtrld = NumNodes;
				SingZoneHtSetPtMgr( SetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				SingZoneHtSetPtMgr( SetPtMgrNum ).NumCtrlNodes = NumNodesCtrld;
				SingZoneHtSetPtMgr( SetPtMgrNum ).SetPt = 0.0;

				for ( CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum ) {
					SingZoneHtSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrldNodeNum ) = NodeNums( CtrldNodeNum );
				}
			} else {
				//    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
				//       '", invalid field.')
				//    Call ShowContinueError('..invalid '//TRIM(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))//'".')
				ErrorsFound = true;
			}

			// get the actual zone number of the control zone
			SingZoneHtSetPtMgr( SetPtMgrNum ).ControlZoneNum = FindItemInList( cAlphaArgs( 3 ), Zone.Name(), NumOfZones );
			if ( SingZoneHtSetPtMgr( SetPtMgrNum ).ControlZoneNum == 0 ) {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid field." );
				ShowContinueError( "..invalid " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\"." );
				ErrorsFound = true;
			}
			SingZoneHtSetPtMgr( SetPtMgrNum ).SetPt = 0.0;

			AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs + NumSZRhSetPtMgrs;

			if ( ! NodeListError ) {
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes = SingZoneHtSetPtMgr( SetPtMgrNum ).CtrlNodes;
			}
			AllSetPtMgr( AllSetPtMgrNum ).Name = SingZoneHtSetPtMgr( SetPtMgrNum ).Name;
			AllSetPtMgr( AllSetPtMgrNum ).SPMType = iSPMType_SZHeating;
			AllSetPtMgr( AllSetPtMgrNum ).CtrlTypeMode = SingZoneHtSetPtMgr( SetPtMgrNum ).CtrlTypeMode;
			AllSetPtMgr( AllSetPtMgrNum ).NumCtrlNodes = SingZoneHtSetPtMgr( SetPtMgrNum ).NumCtrlNodes;

		}

		// Input the Single Zone Cooling Setpoint Managers

		if ( NumSZClSetPtMgrs > 0 ) SingZoneClSetPtMgr.allocate( NumSZClSetPtMgrs ); // Allocate the Setpoint Manager input data array

		// Input the data for each Setpoint Manager
		cCurrentModuleObject = "SetpointManager:SingleZone:Cooling";
		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumSZClSetPtMgrs; ++SetPtMgrNum ) {
			GetObjectItem( cCurrentModuleObject, SetPtMgrNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), SingZoneClSetPtMgr.Name(), SetPtMgrNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			SingZoneClSetPtMgr( SetPtMgrNum ).Name = cAlphaArgs( 1 );
			SingZoneClSetPtMgr( SetPtMgrNum ).CtrlVarType = cAlphaArgs( 2 );
			if ( SameString( SingZoneClSetPtMgr( SetPtMgrNum ).CtrlVarType, "Temperature" ) ) {
				SingZoneClSetPtMgr( SetPtMgrNum ).CtrlTypeMode = iCtrlVarType_Temp;
			} else {
				// should not come here if idd type choice and key list is working
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid field." );
				ShowContinueError( "..invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\"." );
				ShowContinueError( "..Valid value is \"Temperature\"." );
				ErrorsFound = true;
			}
			SingZoneClSetPtMgr( SetPtMgrNum ).ControlZoneName = cAlphaArgs( 3 );
			SingZoneClSetPtMgr( SetPtMgrNum ).MinSetTemp = rNumericArgs( 1 );
			SingZoneClSetPtMgr( SetPtMgrNum ).MaxSetTemp = rNumericArgs( 2 );
			if ( SingZoneClSetPtMgr( SetPtMgrNum ).MaxSetTemp < SingZoneClSetPtMgr( SetPtMgrNum ).MinSetTemp ) {
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"," );
				ShowContinueError( "..." + cNumericFieldNames( 2 ) + "=[" + RoundSigDigits( SingZoneClSetPtMgr( SetPtMgrNum ).MaxSetTemp, 1 ) + "] is less than " + cNumericFieldNames( 1 ) + "=[" + RoundSigDigits( SingZoneClSetPtMgr( SetPtMgrNum ).MinSetTemp, 1 ) + "]." );
			}
			SingZoneClSetPtMgr( SetPtMgrNum ).ZoneNodeNum = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Sensor, 1, ObjectIsNotParent );
			SingZoneClSetPtMgr( SetPtMgrNum ).ZoneInletNodeNum = GetOnlySingleNode( cAlphaArgs( 5 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Sensor, 1, ObjectIsNotParent );
			NodeListError = false;
			GetNodeNums( cAlphaArgs( 6 ), NumNodes, NodeNums, NodeListError, NodeType_Unknown, cCurrentModuleObject, cAlphaArgs( 1 ), NodeConnectionType_SetPoint, 1, ObjectIsNotParent, _, cAlphaFieldNames( 6 ) ); // setpoint nodes
			if ( ! NodeListError ) {
				NumNodesCtrld = NumNodes;
				SingZoneClSetPtMgr( SetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				SingZoneClSetPtMgr( SetPtMgrNum ).NumCtrlNodes = NumNodesCtrld;
				SingZoneClSetPtMgr( SetPtMgrNum ).SetPt = 0.0;

				for ( CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum ) {
					SingZoneClSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrldNodeNum ) = NodeNums( CtrldNodeNum );
				}
			} else {
				//    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
				//       '", invalid field.')
				//    Call ShowContinueError('..invalid '//TRIM(cAlphaFieldNames(6))//'="'//TRIM(cAlphaArgs(6))//'".')
				ErrorsFound = true;
			}

			// get the actual zone number of the control zone
			SingZoneClSetPtMgr( SetPtMgrNum ).ControlZoneNum = FindItemInList( cAlphaArgs( 3 ), Zone.Name(), NumOfZones );
			if ( SingZoneClSetPtMgr( SetPtMgrNum ).ControlZoneNum == 0 ) {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid field." );
				ShowContinueError( "..invalid " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\"." );
				ErrorsFound = true;
			}
			SingZoneClSetPtMgr( SetPtMgrNum ).SetPt = 0.0;

			AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs + NumSZRhSetPtMgrs + NumSZHtSetPtMgrs;

			if ( ! NodeListError ) {
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes = SingZoneClSetPtMgr( SetPtMgrNum ).CtrlNodes;
			}
			AllSetPtMgr( AllSetPtMgrNum ).Name = SingZoneClSetPtMgr( SetPtMgrNum ).Name;
			AllSetPtMgr( AllSetPtMgrNum ).SPMType = iSPMType_SZCooling;
			AllSetPtMgr( AllSetPtMgrNum ).CtrlTypeMode = SingZoneClSetPtMgr( SetPtMgrNum ).CtrlTypeMode;
			AllSetPtMgr( AllSetPtMgrNum ).NumCtrlNodes = SingZoneClSetPtMgr( SetPtMgrNum ).NumCtrlNodes;

		}

		// Input the Single Zone Minimum Humidity Setpoint Managers

		if ( NumSZMinHumSetPtMgrs > 0 ) SZMinHumSetPtMgr.allocate( NumSZMinHumSetPtMgrs );

		// Input the data for each Setpoint Manager
		cCurrentModuleObject = "SetpointManager:SingleZone:Humidity:Minimum";
		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumSZMinHumSetPtMgrs; ++SetPtMgrNum ) {
			GetObjectItem( cCurrentModuleObject, SetPtMgrNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), SZMinHumSetPtMgr.Name(), SetPtMgrNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			SZMinHumSetPtMgr( SetPtMgrNum ).Name = cAlphaArgs( 1 );
			SZMinHumSetPtMgr( SetPtMgrNum ).CtrlVarType = "MinimumHumidityRatio";
			SZMinHumSetPtMgr( SetPtMgrNum ).CtrlTypeMode = iCtrlVarType_MinHumRat;

			if ( cAlphaArgs( 2 ) != "" ) {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid field." );
				ShowContinueError( "..invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\"." );
				ShowContinueError( "Deprecated Field in Object.  Please leave blank." );
				ShowContinueError( "Please note that this field in this object will be deleted in future versions." );
			}
			if ( cAlphaArgs( 3 ) != "" ) {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid field." );
				ShowContinueError( "..invalid " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\"." );
				ShowContinueError( "Deprecated Field in Object.  Please leave blank." );
				ShowContinueError( "Please note that this field in this object will be deleted in future versions." );
			}

			NodeListError = false;
			GetNodeNums( cAlphaArgs( 4 ), NumNodes, NodeNums, NodeListError, NodeType_Air, cCurrentModuleObject, cAlphaArgs( 1 ), NodeConnectionType_SetPoint, 1, ObjectIsNotParent, _, cAlphaFieldNames( 4 ) ); // nodes whose min humidity ratio will be set
			if ( ! NodeListError ) {
				NumNodesCtrld = NumNodes;
				SZMinHumSetPtMgr( SetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				SZMinHumSetPtMgr( SetPtMgrNum ).NumCtrlNodes = NumNodesCtrld;
				SZMinHumSetPtMgr( SetPtMgrNum ).SetPt = 0.0;

				for ( CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum ) {
					SZMinHumSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrldNodeNum ) = NodeNums( CtrldNodeNum );
				}
			} else {
				//    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
				//       '", invalid field.')
				//    Call ShowContinueError('..invalid '//TRIM(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))//'".')
				ErrorsFound = true;
			}

			ErrInList = false;
			GetNodeNums( cAlphaArgs( 5 ), NumNodes, NodeNums, ErrInList, NodeType_Air, cCurrentModuleObject, cAlphaArgs( 1 ), NodeConnectionType_Sensor, 1, ObjectIsNotParent, _, cAlphaFieldNames( 5 ) ); // nodes of zones whose humidity is being controlled
			if ( ErrInList ) {
				//    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
				//       '", invalid field.')
				//    Call ShowContinueError('..invalid '//TRIM(cAlphaFieldNames(5))//'="'//TRIM(cAlphaArgs(5))//'".')
				ErrorsFound = true;
			}
			NumZones = NumNodes;
			SZMinHumSetPtMgr( SetPtMgrNum ).NumZones = NumZones;
			// only allow one control zone for now
			if ( NumNodes > 1 ) {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", entered nodelist." );
				ShowContinueError( "..invalid " + cAlphaFieldNames( 5 ) + "=\"" + cAlphaArgs( 5 ) + "\"." );
				ShowContinueError( "..only one control zone is allowed." );
				ErrorsFound = true;
			}
			SZMinHumSetPtMgr( SetPtMgrNum ).ZoneNodes.allocate( NumZones );
			SZMinHumSetPtMgr( SetPtMgrNum ).ZoneNum.allocate( NumZones );
			SZMinHumSetPtMgr( SetPtMgrNum ).CtrlZoneNum.allocate( NumZones );

			for ( ZoneNum = 1; ZoneNum <= NumZones; ++ZoneNum ) {
				SZMinHumSetPtMgr( SetPtMgrNum ).ZoneNodes( ZoneNum ) = NodeNums( ZoneNum );
				SZMinHumSetPtMgr( SetPtMgrNum ).ZoneNum( ZoneNum ) = 0;
				SZMinHumSetPtMgr( SetPtMgrNum ).CtrlZoneNum( ZoneNum ) = 0;
			}

			AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs + NumSZRhSetPtMgrs + NumSZHtSetPtMgrs + NumSZClSetPtMgrs;

			if ( ! NodeListError ) {
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes = SZMinHumSetPtMgr( SetPtMgrNum ).CtrlNodes;
			}
			AllSetPtMgr( AllSetPtMgrNum ).Name = SZMinHumSetPtMgr( SetPtMgrNum ).Name;
			AllSetPtMgr( AllSetPtMgrNum ).SPMType = iSPMType_SZMinHum;
			AllSetPtMgr( AllSetPtMgrNum ).CtrlTypeMode = SZMinHumSetPtMgr( SetPtMgrNum ).CtrlTypeMode;
			AllSetPtMgr( AllSetPtMgrNum ).NumCtrlNodes = SZMinHumSetPtMgr( SetPtMgrNum ).NumCtrlNodes;

		}

		// Input the Single Zone Maximum Humidity Setpoint Managers

		if ( NumSZMaxHumSetPtMgrs > 0 ) SZMaxHumSetPtMgr.allocate( NumSZMaxHumSetPtMgrs );

		// Input the data for each Setpoint Manager
		cCurrentModuleObject = "SetpointManager:SingleZone:Humidity:Maximum";
		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumSZMaxHumSetPtMgrs; ++SetPtMgrNum ) {
			GetObjectItem( cCurrentModuleObject, SetPtMgrNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), SZMaxHumSetPtMgr.Name(), SetPtMgrNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			SZMaxHumSetPtMgr( SetPtMgrNum ).Name = cAlphaArgs( 1 );
			SZMaxHumSetPtMgr( SetPtMgrNum ).CtrlVarType = "MaximumHumidityRatio";
			SZMaxHumSetPtMgr( SetPtMgrNum ).CtrlTypeMode = iCtrlVarType_MaxHumRat;

			if ( cAlphaArgs( 2 ) != "" ) {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid field." );
				ShowContinueError( "..invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\"." );
				ShowContinueError( "Deprecated Field in Object.  Please leave blank." );
				ShowContinueError( "Please note that this field in this object will be deleted in future versions." );
			}
			if ( cAlphaArgs( 3 ) != "" ) {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid field." );
				ShowContinueError( "..invalid " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\"." );
				ShowContinueError( "Deprecated Field in Object.  Please leave blank." );
				ShowContinueError( "Please note that this field in this object will be deleted in future versions." );
			}

			NodeListError = false;
			GetNodeNums( cAlphaArgs( 4 ), NumNodes, NodeNums, NodeListError, NodeType_Air, cCurrentModuleObject, cAlphaArgs( 1 ), NodeConnectionType_SetPoint, 1, ObjectIsNotParent, _, cAlphaFieldNames( 4 ) ); // nodes whose max humidity ratio will be set
			if ( ! NodeListError ) {
				NumNodesCtrld = NumNodes;
				SZMaxHumSetPtMgr( SetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				SZMaxHumSetPtMgr( SetPtMgrNum ).NumCtrlNodes = NumNodesCtrld;
				SZMaxHumSetPtMgr( SetPtMgrNum ).SetPt = 0.0;

				for ( CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum ) {
					SZMaxHumSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrldNodeNum ) = NodeNums( CtrldNodeNum );
				}
			} else {
				//    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
				//       '", invalid field.')
				//    Call ShowContinueError('..invalid '//TRIM(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))//'".')
				ErrorsFound = true;
			}

			ErrInList = false;
			GetNodeNums( cAlphaArgs( 5 ), NumNodes, NodeNums, ErrInList, NodeType_Air, cCurrentModuleObject, cAlphaArgs( 1 ), NodeConnectionType_Sensor, 1, ObjectIsNotParent, _, cAlphaFieldNames( 5 ) ); // nodes of zones whose humidity is being controlled
			if ( ErrInList ) {
				//    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
				//       '", invalid field.')
				//    Call ShowContinueError('..invalid '//TRIM(cAlphaFieldNames(5))//'="'//TRIM(cAlphaArgs(5))//'".')
				ErrorsFound = true;
			}
			NumZones = NumNodes;
			SZMaxHumSetPtMgr( SetPtMgrNum ).NumZones = NumZones;
			// only allow one control zone for now
			if ( NumNodes > 1 ) {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", entered nodelist." );
				ShowContinueError( "..invalid " + cAlphaFieldNames( 5 ) + "=\"" + cAlphaArgs( 5 ) + "\"." );
				ShowContinueError( "..only one control zone is allowed." );
				ErrorsFound = true;
			}
			SZMaxHumSetPtMgr( SetPtMgrNum ).ZoneNodes.allocate( NumZones );
			SZMaxHumSetPtMgr( SetPtMgrNum ).ZoneNum.allocate( NumZones );
			SZMaxHumSetPtMgr( SetPtMgrNum ).CtrlZoneNum.allocate( NumZones );

			for ( ZoneNum = 1; ZoneNum <= NumZones; ++ZoneNum ) {
				SZMaxHumSetPtMgr( SetPtMgrNum ).ZoneNodes( ZoneNum ) = NodeNums( ZoneNum );
				//   Actual zone node and controlled zone numbers set in Init subroutine
				SZMaxHumSetPtMgr( SetPtMgrNum ).ZoneNum( ZoneNum ) = 0;
				SZMaxHumSetPtMgr( SetPtMgrNum ).CtrlZoneNum( ZoneNum ) = 0;
			}

			AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs + NumSZRhSetPtMgrs + NumSZHtSetPtMgrs + NumSZClSetPtMgrs + NumSZMinHumSetPtMgrs;

			if ( ! NodeListError ) {
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes = SZMaxHumSetPtMgr( SetPtMgrNum ).CtrlNodes;
			}
			AllSetPtMgr( AllSetPtMgrNum ).Name = SZMaxHumSetPtMgr( SetPtMgrNum ).Name;
			AllSetPtMgr( AllSetPtMgrNum ).SPMType = iSPMType_SZMaxHum;
			AllSetPtMgr( AllSetPtMgrNum ).CtrlTypeMode = SZMaxHumSetPtMgr( SetPtMgrNum ).CtrlTypeMode;
			AllSetPtMgr( AllSetPtMgrNum ).NumCtrlNodes = SZMaxHumSetPtMgr( SetPtMgrNum ).NumCtrlNodes;

		}

		// Input the Mixed Air Setpoint Managers

		if ( NumMixedAirSetPtMgrs > 0 ) MixedAirSetPtMgr.allocate( NumMixedAirSetPtMgrs );

		// Input the data for each Setpoint Manager
		cCurrentModuleObject = "SetpointManager:MixedAir";
		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumMixedAirSetPtMgrs; ++SetPtMgrNum ) {
			GetObjectItem( cCurrentModuleObject, SetPtMgrNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), MixedAirSetPtMgr.Name(), SetPtMgrNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}

			MixedAirSetPtMgr( SetPtMgrNum ).Name = cAlphaArgs( 1 );
			MixedAirSetPtMgr( SetPtMgrNum ).CtrlVarType = cAlphaArgs( 2 );
			if ( SameString( MixedAirSetPtMgr( SetPtMgrNum ).CtrlVarType, "Temperature" ) ) {
				MixedAirSetPtMgr( SetPtMgrNum ).CtrlTypeMode = iCtrlVarType_Temp;
			} else {
				// should not come here if idd type choice and key list is working
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid field." );
				ShowContinueError( "..invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\"." );
				ShowContinueError( "..Valid value is \"Temperature\"." );
				ErrorsFound = true;
			}
			MixedAirSetPtMgr( SetPtMgrNum ).RefNode = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Sensor, 1, ObjectIsNotParent );
			MixedAirSetPtMgr( SetPtMgrNum ).FanInNode = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Sensor, 1, ObjectIsNotParent );
			MixedAirSetPtMgr( SetPtMgrNum ).FanOutNode = GetOnlySingleNode( cAlphaArgs( 5 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Sensor, 1, ObjectIsNotParent );
			NodeListError = false;
			GetNodeNums( cAlphaArgs( 6 ), NumNodes, NodeNums, NodeListError, NodeType_Air, cCurrentModuleObject, cAlphaArgs( 1 ), NodeConnectionType_SetPoint, 1, ObjectIsNotParent, _, cAlphaFieldNames( 6 ) ); // setpoint nodes
			if ( ! NodeListError ) {
				NumNodesCtrld = NumNodes;
				MixedAirSetPtMgr( SetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				MixedAirSetPtMgr( SetPtMgrNum ).NumCtrlNodes = NumNodesCtrld;
				MixedAirSetPtMgr( SetPtMgrNum ).SetPt = 0.0;

				for ( CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum ) {
					MixedAirSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrldNodeNum ) = NodeNums( CtrldNodeNum );
				}
			} else {
				//    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
				//       '", invalid field.')
				//    Call ShowContinueError('..invalid '//TRIM(cAlphaFieldNames(6))//'="'//TRIM(cAlphaArgs(6))//'".')
				ErrorsFound = true;
			}

			Found = FindNumberInList( MixedAirSetPtMgr( SetPtMgrNum ).RefNode, MixedAirSetPtMgr( SetPtMgrNum ).CtrlNodes, MixedAirSetPtMgr( SetPtMgrNum ).NumCtrlNodes );
			if ( Found > 0 ) {
				if ( MixedAirSetPtMgr( SetPtMgrNum ).NumCtrlNodes > 1 ) {
					ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", reference node." );
					ShowContinueError( "..Reference Node is the same as one of the nodes in SetPoint NodeList" );
				} else {
					ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", reference node." );
					ShowContinueError( "..Reference Node is the same as the SetPoint Node" );
				}
				ShowContinueError( "Reference Node Name=\"" + NodeID( MixedAirSetPtMgr( SetPtMgrNum ).RefNode ) + "\"." );
			}

			AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs + NumSZRhSetPtMgrs + NumSZHtSetPtMgrs + NumSZClSetPtMgrs + NumSZMinHumSetPtMgrs + NumSZMaxHumSetPtMgrs;

			if ( ! NodeListError ) {
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes = MixedAirSetPtMgr( SetPtMgrNum ).CtrlNodes;
			}
			AllSetPtMgr( AllSetPtMgrNum ).Name = MixedAirSetPtMgr( SetPtMgrNum ).Name;
			AllSetPtMgr( AllSetPtMgrNum ).SPMType = iSPMType_MixedAir;
			AllSetPtMgr( AllSetPtMgrNum ).CtrlTypeMode = MixedAirSetPtMgr( SetPtMgrNum ).CtrlTypeMode;
			AllSetPtMgr( AllSetPtMgrNum ).NumCtrlNodes = MixedAirSetPtMgr( SetPtMgrNum ).NumCtrlNodes;

		}

		// Input the Outside Air Pretreat Setpoint Managers

		if ( NumOAPretreatSetPtMgrs > 0 ) OAPretreatSetPtMgr.allocate( NumOAPretreatSetPtMgrs );

		// Input the data for each Setpoint Manager
		cCurrentModuleObject = "SetpointManager:OutdoorAirPretreat";
		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumOAPretreatSetPtMgrs; ++SetPtMgrNum ) {
			GetObjectItem( cCurrentModuleObject, SetPtMgrNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), OAPretreatSetPtMgr.Name(), SetPtMgrNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}

			OAPretreatSetPtMgr( SetPtMgrNum ).Name = cAlphaArgs( 1 );
			OAPretreatSetPtMgr( SetPtMgrNum ).CtrlVarType = cAlphaArgs( 2 );
			// setup program flow control integers.
			{ auto const SELECT_CASE_var( MakeUPPERCase( OAPretreatSetPtMgr( SetPtMgrNum ).CtrlVarType ) );

			if ( SELECT_CASE_var == "TEMPERATURE" ) {
				OAPretreatSetPtMgr( SetPtMgrNum ).CtrlTypeMode = iCtrlVarType_Temp;
			} else if ( SELECT_CASE_var == "HUMIDITYRATIO" ) {
				OAPretreatSetPtMgr( SetPtMgrNum ).CtrlTypeMode = iCtrlVarType_HumRat;
			} else if ( SELECT_CASE_var == "MAXIMUMHUMIDITYRATIO" ) {
				OAPretreatSetPtMgr( SetPtMgrNum ).CtrlTypeMode = iCtrlVarType_MaxHumRat;
			} else if ( SELECT_CASE_var == "MINIMUMHUMIDITYRATIO" ) {
				OAPretreatSetPtMgr( SetPtMgrNum ).CtrlTypeMode = iCtrlVarType_MinHumRat;
			} else {
				// should not come here if idd type choice and key list is working
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid field." );
				ShowContinueError( "..invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\"." );
				ShowContinueError( "..Valid values are \"Temperature\",\"HumidityRatio\",\"MaximumHumidityRatio\" or \"MinimumHumidityRatio\"." );
				ErrorsFound = true;
			}}

			OAPretreatSetPtMgr( SetPtMgrNum ).MinSetTemp = rNumericArgs( 1 );
			OAPretreatSetPtMgr( SetPtMgrNum ).MaxSetTemp = rNumericArgs( 2 );
			if ( OAPretreatSetPtMgr( SetPtMgrNum ).MaxSetTemp < OAPretreatSetPtMgr( SetPtMgrNum ).MinSetTemp ) {
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"," );
				ShowContinueError( "..." + cNumericFieldNames( 2 ) + "=[" + RoundSigDigits( OAPretreatSetPtMgr( SetPtMgrNum ).MaxSetTemp, 1 ) + "] is less than " + cNumericFieldNames( 1 ) + "=[" + RoundSigDigits( OAPretreatSetPtMgr( SetPtMgrNum ).MinSetTemp, 1 ) + "]." );
			}
			OAPretreatSetPtMgr( SetPtMgrNum ).MinSetHumRat = rNumericArgs( 3 );
			OAPretreatSetPtMgr( SetPtMgrNum ).MaxSetHumRat = rNumericArgs( 4 );
			if ( OAPretreatSetPtMgr( SetPtMgrNum ).MaxSetHumRat < OAPretreatSetPtMgr( SetPtMgrNum ).MinSetHumRat ) {
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"," );
				ShowContinueError( "..." + cNumericFieldNames( 4 ) + "=[" + RoundSigDigits( OAPretreatSetPtMgr( SetPtMgrNum ).MaxSetHumRat, 1 ) + "] is less than " + cNumericFieldNames( 3 ) + "=[" + RoundSigDigits( OAPretreatSetPtMgr( SetPtMgrNum ).MinSetHumRat, 1 ) + "]." );
			}

			// Because a zero humidity ratio setpoint is a special value indicating "off" or "no load"
			// must not allow MinSetHumRat or MaxSetHumRat to be <=0.0
			if ( OAPretreatSetPtMgr( SetPtMgrNum ).MinSetHumRat <= 0.0 ) {
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid value." );
				ShowContinueError( "Minimum setpoint humidity ratio <=0.0, resetting to 0.00001" );
				OAPretreatSetPtMgr( SetPtMgrNum ).MinSetHumRat = 0.00001;
			}
			if ( OAPretreatSetPtMgr( SetPtMgrNum ).MaxSetHumRat <= 0.0 ) {
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid value." );
				ShowContinueError( "Maximum setpoint humidity ratio <=0.0, resetting to 0.00001" );
				OAPretreatSetPtMgr( SetPtMgrNum ).MaxSetHumRat = 0.00001;
			}

			OAPretreatSetPtMgr( SetPtMgrNum ).RefNode = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Sensor, 1, ObjectIsNotParent );
			OAPretreatSetPtMgr( SetPtMgrNum ).MixedOutNode = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Sensor, 1, ObjectIsNotParent );
			OAPretreatSetPtMgr( SetPtMgrNum ).OAInNode = GetOnlySingleNode( cAlphaArgs( 5 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Sensor, 1, ObjectIsNotParent );
			OAPretreatSetPtMgr( SetPtMgrNum ).ReturnInNode = GetOnlySingleNode( cAlphaArgs( 6 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Sensor, 1, ObjectIsNotParent );
			NodeListError = false;
			GetNodeNums( cAlphaArgs( 7 ), NumNodes, NodeNums, NodeListError, NodeType_Air, cCurrentModuleObject, cAlphaArgs( 1 ), NodeConnectionType_SetPoint, 1, ObjectIsNotParent, _, cAlphaFieldNames( 7 ) ); // setpoint nodes
			if ( ! NodeListError ) {
				NumNodesCtrld = NumNodes;
				OAPretreatSetPtMgr( SetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				OAPretreatSetPtMgr( SetPtMgrNum ).NumCtrlNodes = NumNodesCtrld;
				OAPretreatSetPtMgr( SetPtMgrNum ).SetPt = 0.0;

				for ( CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum ) {
					OAPretreatSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrldNodeNum ) = NodeNums( CtrldNodeNum );
				}
			} else {
				//    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
				//       '", invalid field.')
				//    Call ShowContinueError('..invalid '//TRIM(cAlphaFieldNames(7))//'="'//TRIM(cAlphaArgs(7))//'".')
				ErrorsFound = true;
			}

			Found = FindNumberInList( OAPretreatSetPtMgr( SetPtMgrNum ).RefNode, OAPretreatSetPtMgr( SetPtMgrNum ).CtrlNodes, OAPretreatSetPtMgr( SetPtMgrNum ).NumCtrlNodes );
			if ( Found > 0 ) {
				if ( OAPretreatSetPtMgr( SetPtMgrNum ).NumCtrlNodes > 1 ) {
					ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", reference node." );
					ShowContinueError( "..Reference Node is the same as one of the nodes in SetPoint NodeList" );
				} else {
					ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", reference node." );
					ShowContinueError( "..Reference Node is the same as the SetPoint Node" );
				}
				ShowContinueError( "Reference Node Name=\"" + NodeID( OAPretreatSetPtMgr( SetPtMgrNum ).RefNode ) + "\"." );
			}

			AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs + NumSZRhSetPtMgrs + NumSZHtSetPtMgrs + NumSZClSetPtMgrs + NumSZMinHumSetPtMgrs + NumSZMaxHumSetPtMgrs + NumMixedAirSetPtMgrs;

			if ( ! NodeListError ) {
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes = OAPretreatSetPtMgr( SetPtMgrNum ).CtrlNodes;
			}
			AllSetPtMgr( AllSetPtMgrNum ).Name = OAPretreatSetPtMgr( SetPtMgrNum ).Name;
			AllSetPtMgr( AllSetPtMgrNum ).SPMType = iSPMType_OutsideAirPretreat;
			AllSetPtMgr( AllSetPtMgrNum ).CtrlTypeMode = OAPretreatSetPtMgr( SetPtMgrNum ).CtrlTypeMode;
			AllSetPtMgr( AllSetPtMgrNum ).NumCtrlNodes = OAPretreatSetPtMgr( SetPtMgrNum ).NumCtrlNodes;

		}

		// Input the Warmest Setpoint Managers

		if ( NumWarmestSetPtMgrs > 0 ) WarmestSetPtMgr.allocate( NumWarmestSetPtMgrs );

		// Input the data for each Setpoint Manager
		cCurrentModuleObject = "SetpointManager:Warmest";
		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumWarmestSetPtMgrs; ++SetPtMgrNum ) {
			GetObjectItem( cCurrentModuleObject, SetPtMgrNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), WarmestSetPtMgr.Name(), SetPtMgrNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			WarmestSetPtMgr( SetPtMgrNum ).Name = cAlphaArgs( 1 );
			WarmestSetPtMgr( SetPtMgrNum ).CtrlVarType = cAlphaArgs( 2 );
			if ( SameString( WarmestSetPtMgr( SetPtMgrNum ).CtrlVarType, "Temperature" ) ) {
				WarmestSetPtMgr( SetPtMgrNum ).CtrlTypeMode = iCtrlVarType_Temp;
			} else {
				// should not come here if idd type choice and key list is working
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid field." );
				ShowContinueError( "..invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\"." );
				ShowContinueError( "..Valid value is \"Temperature\"." );
				ErrorsFound = true;
			}
			WarmestSetPtMgr( SetPtMgrNum ).AirLoopName = cAlphaArgs( 3 );
			WarmestSetPtMgr( SetPtMgrNum ).AirLoopNum = 0;
			WarmestSetPtMgr( SetPtMgrNum ).MinSetTemp = rNumericArgs( 1 );
			WarmestSetPtMgr( SetPtMgrNum ).MaxSetTemp = rNumericArgs( 2 );
			if ( WarmestSetPtMgr( SetPtMgrNum ).MaxSetTemp < WarmestSetPtMgr( SetPtMgrNum ).MinSetTemp ) {
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"," );
				ShowContinueError( "..." + cNumericFieldNames( 2 ) + "=[" + RoundSigDigits( WarmestSetPtMgr( SetPtMgrNum ).MaxSetTemp, 1 ) + "] is less than " + cNumericFieldNames( 1 ) + "=[" + RoundSigDigits( WarmestSetPtMgr( SetPtMgrNum ).MinSetTemp, 1 ) + "]." );
			}

			{ auto const SELECT_CASE_var( MakeUPPERCase( cAlphaArgs( 4 ) ) );
			if ( SELECT_CASE_var == "MAXIMUMTEMPERATURE" ) {
				WarmestSetPtMgr( SetPtMgrNum ).Strategy = MaxTemp;
			} else {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid field." );
				ShowContinueError( "..invalid " + cAlphaFieldNames( 4 ) + "=\"" + cAlphaArgs( 4 ) + "\"." );
				ShowContinueError( "..Valid value is \"MaximumTemperature\"." );
				ErrorsFound = true;
			}}

			NodeListError = false;
			GetNodeNums( cAlphaArgs( 5 ), NumNodes, NodeNums, NodeListError, NodeType_Air, cCurrentModuleObject, cAlphaArgs( 1 ), NodeConnectionType_SetPoint, 1, ObjectIsNotParent, _, cAlphaFieldNames( 5 ) ); // setpoint nodes
			if ( ! NodeListError ) {
				NumNodesCtrld = NumNodes;
				WarmestSetPtMgr( SetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				WarmestSetPtMgr( SetPtMgrNum ).NumCtrlNodes = NumNodesCtrld;
				WarmestSetPtMgr( SetPtMgrNum ).SetPt = 0.0;

				for ( CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum ) {
					WarmestSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrldNodeNum ) = NodeNums( CtrldNodeNum );
				}
			} else {
				//    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
				//       '", invalid field.')
				//    Call ShowContinueError('..invalid '//TRIM(cAlphaFieldNames(5))//'="'//TRIM(cAlphaArgs(5))//'".')
				ErrorsFound = true;
			}

			AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs + NumSZRhSetPtMgrs + NumSZHtSetPtMgrs + NumSZClSetPtMgrs + NumSZMinHumSetPtMgrs + NumSZMaxHumSetPtMgrs + NumMixedAirSetPtMgrs + NumOAPretreatSetPtMgrs;

			if ( ! NodeListError ) {
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes = WarmestSetPtMgr( SetPtMgrNum ).CtrlNodes;
			}
			AllSetPtMgr( AllSetPtMgrNum ).Name = WarmestSetPtMgr( SetPtMgrNum ).Name;
			AllSetPtMgr( AllSetPtMgrNum ).SPMType = iSPMType_Warmest;
			AllSetPtMgr( AllSetPtMgrNum ).CtrlTypeMode = WarmestSetPtMgr( SetPtMgrNum ).CtrlTypeMode;
			AllSetPtMgr( AllSetPtMgrNum ).NumCtrlNodes = WarmestSetPtMgr( SetPtMgrNum ).NumCtrlNodes;

		}

		// Input the Coldest Setpoint Managers

		if ( NumColdestSetPtMgrs > 0 ) ColdestSetPtMgr.allocate( NumColdestSetPtMgrs );

		// Input the data for each Setpoint Manager
		cCurrentModuleObject = "SetpointManager:Coldest";
		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumColdestSetPtMgrs; ++SetPtMgrNum ) {
			GetObjectItem( cCurrentModuleObject, SetPtMgrNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), ColdestSetPtMgr.Name(), SetPtMgrNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			ColdestSetPtMgr( SetPtMgrNum ).Name = cAlphaArgs( 1 );
			ColdestSetPtMgr( SetPtMgrNum ).CtrlVarType = cAlphaArgs( 2 );
			if ( SameString( ColdestSetPtMgr( SetPtMgrNum ).CtrlVarType, "Temperature" ) ) {
				ColdestSetPtMgr( SetPtMgrNum ).CtrlTypeMode = iCtrlVarType_Temp;
			} else {
				// should not come here if idd type choice and key list is working
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid field." );
				ShowContinueError( "..invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\"." );
				ShowContinueError( "..Valid value is \"Temperature\"." );
				ErrorsFound = true;
			}
			ColdestSetPtMgr( SetPtMgrNum ).AirLoopName = cAlphaArgs( 3 );
			ColdestSetPtMgr( SetPtMgrNum ).AirLoopNum = 0;
			ColdestSetPtMgr( SetPtMgrNum ).MinSetTemp = rNumericArgs( 1 );
			ColdestSetPtMgr( SetPtMgrNum ).MaxSetTemp = rNumericArgs( 2 );
			if ( ColdestSetPtMgr( SetPtMgrNum ).MaxSetTemp < ColdestSetPtMgr( SetPtMgrNum ).MinSetTemp ) {
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"," );
				ShowContinueError( "..." + cNumericFieldNames( 2 ) + "=[" + RoundSigDigits( ColdestSetPtMgr( SetPtMgrNum ).MaxSetTemp, 1 ) + "] is less than " + cNumericFieldNames( 1 ) + "=[" + RoundSigDigits( ColdestSetPtMgr( SetPtMgrNum ).MinSetTemp, 1 ) + "]." );
			}

			{ auto const SELECT_CASE_var( MakeUPPERCase( cAlphaArgs( 4 ) ) );
			if ( SELECT_CASE_var == "MINIMUMTEMPERATURE" ) {
				ColdestSetPtMgr( SetPtMgrNum ).Strategy = MinTemp;
			} else {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid field." );
				ShowContinueError( "..invalid " + cAlphaFieldNames( 4 ) + "=\"" + cAlphaArgs( 4 ) + "\"." );
				ShowContinueError( "..Valid value is \"MinimumTemperature\"." );
				ErrorsFound = true;
			}}

			NodeListError = false;
			GetNodeNums( cAlphaArgs( 5 ), NumNodes, NodeNums, NodeListError, NodeType_Air, cCurrentModuleObject, cAlphaArgs( 1 ), NodeConnectionType_SetPoint, 1, ObjectIsNotParent, _, cAlphaFieldNames( 5 ) ); // setpoint nodes
			if ( ! NodeListError ) {
				NumNodesCtrld = NumNodes;
				ColdestSetPtMgr( SetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				ColdestSetPtMgr( SetPtMgrNum ).NumCtrlNodes = NumNodesCtrld;
				ColdestSetPtMgr( SetPtMgrNum ).SetPt = 0.0;

				for ( CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum ) {
					ColdestSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrldNodeNum ) = NodeNums( CtrldNodeNum );
				}
			} else {
				//    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
				//       '", invalid field.')
				//    Call ShowContinueError('..invalid '//TRIM(cAlphaFieldNames(5))//'="'//TRIM(cAlphaArgs(5))//'".')
				ErrorsFound = true;
			}

			AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs + NumSZRhSetPtMgrs + NumSZHtSetPtMgrs + NumSZClSetPtMgrs + NumSZMinHumSetPtMgrs + NumSZMaxHumSetPtMgrs + NumMixedAirSetPtMgrs + NumOAPretreatSetPtMgrs + NumWarmestSetPtMgrs;

			if ( ! NodeListError ) {
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes = ColdestSetPtMgr( SetPtMgrNum ).CtrlNodes;
			}
			AllSetPtMgr( AllSetPtMgrNum ).Name = ColdestSetPtMgr( SetPtMgrNum ).Name;
			AllSetPtMgr( AllSetPtMgrNum ).SPMType = iSPMType_Coldest;
			AllSetPtMgr( AllSetPtMgrNum ).CtrlTypeMode = ColdestSetPtMgr( SetPtMgrNum ).CtrlTypeMode;
			AllSetPtMgr( AllSetPtMgrNum ).NumCtrlNodes = ColdestSetPtMgr( SetPtMgrNum ).NumCtrlNodes;

		}

		// Input the Warmest Temp Flow Setpoint Managers

		if ( NumWarmestSetPtMgrsTempFlow > 0 ) WarmestSetPtMgrTempFlow.allocate( NumWarmestSetPtMgrsTempFlow );

		// Input the data for each Setpoint Manager
		cCurrentModuleObject = "SetpointManager:WarmestTemperatureFlow";
		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumWarmestSetPtMgrsTempFlow; ++SetPtMgrNum ) {
			GetObjectItem( cCurrentModuleObject, SetPtMgrNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), WarmestSetPtMgrTempFlow.Name(), SetPtMgrNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			WarmestSetPtMgrTempFlow( SetPtMgrNum ).Name = cAlphaArgs( 1 );
			WarmestSetPtMgrTempFlow( SetPtMgrNum ).CtrlVarType = cAlphaArgs( 2 );
			if ( SameString( WarmestSetPtMgrTempFlow( SetPtMgrNum ).CtrlVarType, "Temperature" ) ) {
				WarmestSetPtMgrTempFlow( SetPtMgrNum ).CtrlTypeMode = iCtrlVarType_Temp;
			} else {
				// should not come here if idd type choice and key list is working
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid field." );
				ShowContinueError( "..invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\"." );
				ShowContinueError( "..Valid value is \"Temperature\"." );
				ErrorsFound = true;
			}
			WarmestSetPtMgrTempFlow( SetPtMgrNum ).AirLoopName = cAlphaArgs( 3 );
			WarmestSetPtMgrTempFlow( SetPtMgrNum ).AirLoopNum = 0;
			WarmestSetPtMgrTempFlow( SetPtMgrNum ).MinSetTemp = rNumericArgs( 1 );
			WarmestSetPtMgrTempFlow( SetPtMgrNum ).MaxSetTemp = rNumericArgs( 2 );
			if ( WarmestSetPtMgrTempFlow( SetPtMgrNum ).MaxSetTemp < WarmestSetPtMgrTempFlow( SetPtMgrNum ).MinSetTemp ) {
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"," );
				ShowContinueError( "..." + cNumericFieldNames( 2 ) + "=[" + RoundSigDigits( WarmestSetPtMgrTempFlow( SetPtMgrNum ).MaxSetTemp, 1 ) + "] is less than " + cNumericFieldNames( 1 ) + "=[" + RoundSigDigits( WarmestSetPtMgrTempFlow( SetPtMgrNum ).MinSetTemp, 1 ) + "]." );
			}
			WarmestSetPtMgrTempFlow( SetPtMgrNum ).MinTurndown = rNumericArgs( 3 );
			if ( WarmestSetPtMgrTempFlow( SetPtMgrNum ).MinTurndown >= 0.8 ) {
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"," );
				ShowContinueError( "..." + cNumericFieldNames( 3 ) + "=[" + RoundSigDigits( WarmestSetPtMgrTempFlow( SetPtMgrNum ).MinTurndown, 2 ) + "] is greater than 0.8;" );
				ShowContinueError( "...typical values for " + cNumericFieldNames( 3 ) + " are less than 0.8." );
			}
			{ auto const SELECT_CASE_var( MakeUPPERCase( cAlphaArgs( 4 ) ) );
			if ( SELECT_CASE_var == "TEMPERATUREFIRST" ) {
				WarmestSetPtMgrTempFlow( SetPtMgrNum ).Strategy = TempFirst;
			} else if ( SELECT_CASE_var == "FLOWFIRST" ) {
				WarmestSetPtMgrTempFlow( SetPtMgrNum ).Strategy = FlowFirst;
			} else {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid field." );
				ShowContinueError( "..invalid " + cAlphaFieldNames( 4 ) + "=\"" + cAlphaArgs( 4 ) + "\"." );
				ShowContinueError( "..Valid values are \"TemperatureFirst\" or \"FlowFirst\"." );
				ErrorsFound = true;
			}}

			NodeListError = false;
			GetNodeNums( cAlphaArgs( 5 ), NumNodes, NodeNums, NodeListError, NodeType_Air, cCurrentModuleObject, cAlphaArgs( 1 ), NodeConnectionType_SetPoint, 1, ObjectIsNotParent, _, cAlphaFieldNames( 5 ) ); // setpoint nodes
			if ( ! NodeListError ) {
				NumNodesCtrld = NumNodes;
				WarmestSetPtMgrTempFlow( SetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				WarmestSetPtMgrTempFlow( SetPtMgrNum ).NumCtrlNodes = NumNodesCtrld;
				WarmestSetPtMgrTempFlow( SetPtMgrNum ).SetPt = 0.0;

				for ( CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum ) {
					WarmestSetPtMgrTempFlow( SetPtMgrNum ).CtrlNodes( CtrldNodeNum ) = NodeNums( CtrldNodeNum );
				}
			} else {
				//    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
				//       '", invalid field.')
				//    Call ShowContinueError('..invalid '//TRIM(cAlphaFieldNames(5))//'="'//TRIM(cAlphaArgs(5))//'".')
				ErrorsFound = true;
			}

			AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs + NumSZRhSetPtMgrs + NumSZHtSetPtMgrs + NumSZClSetPtMgrs + NumSZMinHumSetPtMgrs + NumSZMaxHumSetPtMgrs + NumMixedAirSetPtMgrs + NumOAPretreatSetPtMgrs + NumWarmestSetPtMgrs + NumColdestSetPtMgrs;

			if ( ! NodeListError ) {
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes = WarmestSetPtMgrTempFlow( SetPtMgrNum ).CtrlNodes;
			}
			AllSetPtMgr( AllSetPtMgrNum ).Name = WarmestSetPtMgrTempFlow( SetPtMgrNum ).Name;
			AllSetPtMgr( AllSetPtMgrNum ).SPMType = iSPMType_WarmestTempFlow;
			AllSetPtMgr( AllSetPtMgrNum ).CtrlTypeMode = WarmestSetPtMgrTempFlow( SetPtMgrNum ).CtrlTypeMode;
			AllSetPtMgr( AllSetPtMgrNum ).NumCtrlNodes = WarmestSetPtMgrTempFlow( SetPtMgrNum ).NumCtrlNodes;

		}

		// Input the Return Air Bypass Flow Setpoint Managers

		if ( NumRABFlowSetPtMgrs > 0 ) RABFlowSetPtMgr.allocate( NumRABFlowSetPtMgrs );

		// Input the data for each Setpoint Manager
		cCurrentModuleObject = "SetpointManager:ReturnAirBypassFlow";
		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumRABFlowSetPtMgrs; ++SetPtMgrNum ) {
			GetObjectItem( cCurrentModuleObject, SetPtMgrNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), RABFlowSetPtMgr.Name(), SetPtMgrNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			RABFlowSetPtMgr( SetPtMgrNum ).Name = cAlphaArgs( 1 );
			RABFlowSetPtMgr( SetPtMgrNum ).CtrlVarType = cAlphaArgs( 2 );
			RABFlowSetPtMgr( SetPtMgrNum ).NumCtrlNodes = 1;
			NumNodesCtrld = 1;

			if ( SameString( RABFlowSetPtMgr( SetPtMgrNum ).CtrlVarType, "Flow" ) ) {
				RABFlowSetPtMgr( SetPtMgrNum ).CtrlTypeMode = iCtrlVarType_MassFlow;
			} else {
				// should not come here if idd type choice and key list is working
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid field." );
				ShowContinueError( "..invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\"." );
				ShowContinueError( "..Valid value is \"Temperature\"." );
				ErrorsFound = true;
			}
			RABFlowSetPtMgr( SetPtMgrNum ).AirLoopName = cAlphaArgs( 3 );
			RABFlowSetPtMgr( SetPtMgrNum ).AirLoopNum = 0;
			RABFlowSetPtMgr( SetPtMgrNum ).Sched = cAlphaArgs( 4 );
			RABFlowSetPtMgr( SetPtMgrNum ).SchedPtr = GetScheduleIndex( cAlphaArgs( 4 ) );
			if ( RABFlowSetPtMgr( SetPtMgrNum ).SchedPtr == 0 ) {
				if ( lAlphaFieldBlanks( 4 ) ) {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", blank required field." );
					ShowContinueError( "..required field " + cAlphaFieldNames( 4 ) );
				} else {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid field." );
					ShowContinueError( "..invalid " + cAlphaFieldNames( 4 ) + "=\"" + cAlphaArgs( 4 ) + "\"." );
				}
				ErrorsFound = true;
			}

			AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs + NumSZRhSetPtMgrs + NumSZHtSetPtMgrs + NumSZClSetPtMgrs + NumSZMinHumSetPtMgrs + NumSZMaxHumSetPtMgrs + NumMixedAirSetPtMgrs + NumOAPretreatSetPtMgrs + NumWarmestSetPtMgrs + NumColdestSetPtMgrs + NumWarmestSetPtMgrsTempFlow;

			RABFlowSetPtMgr( SetPtMgrNum ).AllSetPtMgrIndex = AllSetPtMgrNum;
			RABFlowSetPtMgr( SetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
			RABFlowSetPtMgr( SetPtMgrNum ).CtrlNodes = 0;
			AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
			// need to reset this to the control node (RABSplitOutNode) in Init, will be 0 here
			AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes = RABFlowSetPtMgr( SetPtMgrNum ).CtrlNodes;
			AllSetPtMgr( AllSetPtMgrNum ).Name = RABFlowSetPtMgr( SetPtMgrNum ).Name;
			AllSetPtMgr( AllSetPtMgrNum ).SPMType = iSPMType_RAB;
			AllSetPtMgr( AllSetPtMgrNum ).CtrlTypeMode = RABFlowSetPtMgr( SetPtMgrNum ).CtrlTypeMode;
			AllSetPtMgr( AllSetPtMgrNum ).NumCtrlNodes = RABFlowSetPtMgr( SetPtMgrNum ).NumCtrlNodes;

		}

		// Input the MultiZone Average Cooling Setpoint Managers
		if ( NumMZClgAverageSetPtMgrs > 0 ) MZAverageCoolingSetPtMgr.allocate( NumMZClgAverageSetPtMgrs );

		// Input the data for each setpoint manager
		cCurrentModuleObject = "SetpointManager:MultiZone:Cooling:Average";
		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumMZClgAverageSetPtMgrs; ++SetPtMgrNum ) {
			GetObjectItem( cCurrentModuleObject, SetPtMgrNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), MZAverageCoolingSetPtMgr.Name(), SetPtMgrNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			MZAverageCoolingSetPtMgr( SetPtMgrNum ).Name = cAlphaArgs( 1 );
			MZAverageCoolingSetPtMgr( SetPtMgrNum ).AirLoopName = cAlphaArgs( 2 );
			MZAverageCoolingSetPtMgr( SetPtMgrNum ).AirLoopNum = 0;
			MZAverageCoolingSetPtMgr( SetPtMgrNum ).MinSetTemp = rNumericArgs( 1 );
			MZAverageCoolingSetPtMgr( SetPtMgrNum ).MaxSetTemp = rNumericArgs( 2 );
			MZAverageCoolingSetPtMgr( SetPtMgrNum ).CtrlVarType = "Temperature";
			MZAverageCoolingSetPtMgr( SetPtMgrNum ).CtrlTypeMode = iCtrlVarType_Temp;

			if ( MZAverageCoolingSetPtMgr( SetPtMgrNum ).MaxSetTemp < MZAverageCoolingSetPtMgr( SetPtMgrNum ).MinSetTemp ) {
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"," );
				ShowContinueError( "..." + cNumericFieldNames( 2 ) + "=[" + RoundSigDigits( MZAverageCoolingSetPtMgr( SetPtMgrNum ).MaxSetTemp, 1 ) + "] is less than " + cNumericFieldNames( 1 ) + "=[" + RoundSigDigits( MZAverageCoolingSetPtMgr( SetPtMgrNum ).MinSetTemp, 1 ) + "]." );
			}

			NodeListError = false;
			GetNodeNums( cAlphaArgs( 3 ), NumNodes, NodeNums, NodeListError, NodeType_Air, cCurrentModuleObject, cAlphaArgs( 1 ), NodeConnectionType_SetPoint, 1, ObjectIsNotParent, _, cAlphaFieldNames( 3 ) ); // setpoint nodes
			if ( ! NodeListError ) {
				NumNodesCtrld = NumNodes;
				MZAverageCoolingSetPtMgr( SetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				MZAverageCoolingSetPtMgr( SetPtMgrNum ).NumCtrlNodes = NumNodesCtrld;
				MZAverageCoolingSetPtMgr( SetPtMgrNum ).SetPt = 0.0;

				for ( CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum ) {
					MZAverageCoolingSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrldNodeNum ) = NodeNums( CtrldNodeNum );
				}
			} else {
				//    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
				//       '", invalid field.')
				//    Call ShowContinueError('..invalid '//TRIM(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'".')
				ErrorsFound = true;
			}

			AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs + NumSZRhSetPtMgrs + NumSZHtSetPtMgrs + NumSZClSetPtMgrs + NumSZMinHumSetPtMgrs + NumSZMaxHumSetPtMgrs + NumMixedAirSetPtMgrs + NumOAPretreatSetPtMgrs + NumWarmestSetPtMgrs + NumColdestSetPtMgrs + NumWarmestSetPtMgrsTempFlow + NumRABFlowSetPtMgrs;

			if ( ! NodeListError ) {
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes = MZAverageCoolingSetPtMgr( SetPtMgrNum ).CtrlNodes;
			}
			AllSetPtMgr( AllSetPtMgrNum ).Name = MZAverageCoolingSetPtMgr( SetPtMgrNum ).Name;
			AllSetPtMgr( AllSetPtMgrNum ).SPMType = iSPMType_MZCoolingAverage;
			AllSetPtMgr( AllSetPtMgrNum ).CtrlTypeMode = MZAverageCoolingSetPtMgr( SetPtMgrNum ).CtrlTypeMode;
			AllSetPtMgr( AllSetPtMgrNum ).NumCtrlNodes = MZAverageCoolingSetPtMgr( SetPtMgrNum ).NumCtrlNodes;

		}

		// Input the MultiZone Average Heating Setpoint Managers
		if ( NumMZHtgAverageSetPtMgrs > 0 ) MZAverageHeatingSetPtMgr.allocate( NumMZHtgAverageSetPtMgrs );

		// Input the data for each setpoint manager
		cCurrentModuleObject = "SetpointManager:MultiZone:Heating:Average";
		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumMZHtgAverageSetPtMgrs; ++SetPtMgrNum ) {
			GetObjectItem( cCurrentModuleObject, SetPtMgrNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), MZAverageHeatingSetPtMgr.Name(), SetPtMgrNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			MZAverageHeatingSetPtMgr( SetPtMgrNum ).Name = cAlphaArgs( 1 );
			MZAverageHeatingSetPtMgr( SetPtMgrNum ).AirLoopName = cAlphaArgs( 2 );
			MZAverageHeatingSetPtMgr( SetPtMgrNum ).AirLoopNum = 0;
			MZAverageHeatingSetPtMgr( SetPtMgrNum ).MinSetTemp = rNumericArgs( 1 );
			MZAverageHeatingSetPtMgr( SetPtMgrNum ).MaxSetTemp = rNumericArgs( 2 );
			MZAverageHeatingSetPtMgr( SetPtMgrNum ).CtrlVarType = "Temperature";
			MZAverageHeatingSetPtMgr( SetPtMgrNum ).CtrlTypeMode = iCtrlVarType_Temp;

			if ( MZAverageHeatingSetPtMgr( SetPtMgrNum ).MaxSetTemp < MZAverageHeatingSetPtMgr( SetPtMgrNum ).MinSetTemp ) {
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"," );
				ShowContinueError( "..." + cNumericFieldNames( 2 ) + "=[" + RoundSigDigits( MZAverageHeatingSetPtMgr( SetPtMgrNum ).MaxSetTemp, 1 ) + "] is less than " + cNumericFieldNames( 1 ) + "=[" + RoundSigDigits( MZAverageHeatingSetPtMgr( SetPtMgrNum ).MinSetTemp, 1 ) + "]." );
			}

			NodeListError = false;
			GetNodeNums( cAlphaArgs( 3 ), NumNodes, NodeNums, NodeListError, NodeType_Air, cCurrentModuleObject, cAlphaArgs( 1 ), NodeConnectionType_SetPoint, 1, ObjectIsNotParent, _, cAlphaFieldNames( 3 ) ); // setpoint nodes
			if ( ! NodeListError ) {
				NumNodesCtrld = NumNodes;
				MZAverageHeatingSetPtMgr( SetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				MZAverageHeatingSetPtMgr( SetPtMgrNum ).NumCtrlNodes = NumNodesCtrld;
				MZAverageHeatingSetPtMgr( SetPtMgrNum ).SetPt = 0.0;

				for ( CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum ) {
					MZAverageHeatingSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrldNodeNum ) = NodeNums( CtrldNodeNum );
				}
			} else {
				//    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
				//       '", invalid field.')
				//    Call ShowContinueError('..invalid '//TRIM(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'".')
				ErrorsFound = true;
			}

			AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs + NumSZRhSetPtMgrs + NumSZHtSetPtMgrs + NumSZClSetPtMgrs + NumSZMinHumSetPtMgrs + NumSZMaxHumSetPtMgrs + NumMixedAirSetPtMgrs + NumOAPretreatSetPtMgrs + NumWarmestSetPtMgrs + NumColdestSetPtMgrs + NumWarmestSetPtMgrsTempFlow + NumRABFlowSetPtMgrs + NumMZClgAverageSetPtMgrs;

			if ( ! NodeListError ) {
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes = MZAverageHeatingSetPtMgr( SetPtMgrNum ).CtrlNodes;
			}
			AllSetPtMgr( AllSetPtMgrNum ).Name = MZAverageHeatingSetPtMgr( SetPtMgrNum ).Name;
			AllSetPtMgr( AllSetPtMgrNum ).SPMType = iSPMType_MZHeatingAverage;
			AllSetPtMgr( AllSetPtMgrNum ).CtrlTypeMode = MZAverageHeatingSetPtMgr( SetPtMgrNum ).CtrlTypeMode;
			AllSetPtMgr( AllSetPtMgrNum ).NumCtrlNodes = MZAverageHeatingSetPtMgr( SetPtMgrNum ).NumCtrlNodes;

		}

		// Input the MultiZone Average Minimum Humidity Setpoint Managers
		if ( NumMZAverageMinHumSetPtMgrs > 0 ) MZAverageMinHumSetPtMgr.allocate( NumMZAverageMinHumSetPtMgrs );

		// Input the data for each setpoint manager
		cCurrentModuleObject = "SetpointManager:MultiZone:MinimumHumidity:Average";
		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumMZAverageMinHumSetPtMgrs; ++SetPtMgrNum ) {
			GetObjectItem( cCurrentModuleObject, SetPtMgrNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), MZAverageMinHumSetPtMgr.Name(), SetPtMgrNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			MZAverageMinHumSetPtMgr( SetPtMgrNum ).Name = cAlphaArgs( 1 );
			MZAverageMinHumSetPtMgr( SetPtMgrNum ).AirLoopName = cAlphaArgs( 2 );
			MZAverageMinHumSetPtMgr( SetPtMgrNum ).AirLoopNum = 0;
			MZAverageMinHumSetPtMgr( SetPtMgrNum ).MinSetHum = rNumericArgs( 1 );
			MZAverageMinHumSetPtMgr( SetPtMgrNum ).MaxSetHum = rNumericArgs( 2 );
			MZAverageMinHumSetPtMgr( SetPtMgrNum ).CtrlVarType = "MinimumHumidityRatio";
			MZAverageMinHumSetPtMgr( SetPtMgrNum ).CtrlTypeMode = iCtrlVarType_MinHumRat;

			if ( MZAverageMinHumSetPtMgr( SetPtMgrNum ).MaxSetHum < MZAverageMinHumSetPtMgr( SetPtMgrNum ).MinSetHum ) {
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"," );
				ShowContinueError( "..." + cNumericFieldNames( 2 ) + "=[" + RoundSigDigits( MZAverageMinHumSetPtMgr( SetPtMgrNum ).MaxSetHum, 3 ) + "] is less than " + cNumericFieldNames( 1 ) + "=[" + RoundSigDigits( MZAverageMinHumSetPtMgr( SetPtMgrNum ).MinSetHum, 3 ) + "]." );
			}

			NodeListError = false;
			GetNodeNums( cAlphaArgs( 3 ), NumNodes, NodeNums, NodeListError, NodeType_Air, cCurrentModuleObject, cAlphaArgs( 1 ), NodeConnectionType_SetPoint, 1, ObjectIsNotParent, _, cAlphaFieldNames( 3 ) ); // setpoint nodes
			if ( ! NodeListError ) {
				NumNodesCtrld = NumNodes;
				MZAverageMinHumSetPtMgr( SetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				MZAverageMinHumSetPtMgr( SetPtMgrNum ).NumCtrlNodes = NumNodesCtrld;
				MZAverageMinHumSetPtMgr( SetPtMgrNum ).SetPt = 0.0;

				for ( CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum ) {
					MZAverageMinHumSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrldNodeNum ) = NodeNums( CtrldNodeNum );
				}
			} else {
				//    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
				//       '", invalid field.')
				//    Call ShowContinueError('..invalid '//TRIM(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'".')
				ErrorsFound = true;
			}

			AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs + NumSZRhSetPtMgrs + NumSZHtSetPtMgrs + NumSZClSetPtMgrs + NumSZMinHumSetPtMgrs + NumSZMaxHumSetPtMgrs + NumMixedAirSetPtMgrs + NumOAPretreatSetPtMgrs + NumWarmestSetPtMgrs + NumColdestSetPtMgrs + NumWarmestSetPtMgrsTempFlow + NumRABFlowSetPtMgrs + NumMZClgAverageSetPtMgrs + NumMZHtgAverageSetPtMgrs;

			if ( ! NodeListError ) {
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes = MZAverageMinHumSetPtMgr( SetPtMgrNum ).CtrlNodes;
			}
			AllSetPtMgr( AllSetPtMgrNum ).Name = MZAverageMinHumSetPtMgr( SetPtMgrNum ).Name;
			AllSetPtMgr( AllSetPtMgrNum ).SPMType = iSPMType_MZMinHumAverage;
			AllSetPtMgr( AllSetPtMgrNum ).CtrlTypeMode = MZAverageMinHumSetPtMgr( SetPtMgrNum ).CtrlTypeMode;
			AllSetPtMgr( AllSetPtMgrNum ).NumCtrlNodes = MZAverageMinHumSetPtMgr( SetPtMgrNum ).NumCtrlNodes;

		}

		// Input the MultiZone Average Maximum Humidity SetPoint Managers
		if ( NumMZAverageMaxHumSetPtMgrs > 0 ) MZAverageMaxHumSetPtMgr.allocate( NumMZAverageMaxHumSetPtMgrs );

		// Input the data for each setpoint manager
		cCurrentModuleObject = "SetpointManager:MultiZone:MaximumHumidity:Average";
		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumMZAverageMaxHumSetPtMgrs; ++SetPtMgrNum ) {
			GetObjectItem( cCurrentModuleObject, SetPtMgrNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), MZAverageMaxHumSetPtMgr.Name(), SetPtMgrNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			MZAverageMaxHumSetPtMgr( SetPtMgrNum ).Name = cAlphaArgs( 1 );
			MZAverageMaxHumSetPtMgr( SetPtMgrNum ).AirLoopName = cAlphaArgs( 2 );
			MZAverageMaxHumSetPtMgr( SetPtMgrNum ).AirLoopNum = 0;
			MZAverageMaxHumSetPtMgr( SetPtMgrNum ).MinSetHum = rNumericArgs( 1 );
			MZAverageMaxHumSetPtMgr( SetPtMgrNum ).MaxSetHum = rNumericArgs( 2 );
			MZAverageMaxHumSetPtMgr( SetPtMgrNum ).CtrlVarType = "MaximumHumidityRatio";
			MZAverageMaxHumSetPtMgr( SetPtMgrNum ).CtrlTypeMode = iCtrlVarType_MaxHumRat;

			if ( MZAverageMaxHumSetPtMgr( SetPtMgrNum ).MaxSetHum < MZAverageMaxHumSetPtMgr( SetPtMgrNum ).MinSetHum ) {
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"," );
				ShowContinueError( "..." + cNumericFieldNames( 2 ) + "=[" + RoundSigDigits( MZAverageMaxHumSetPtMgr( SetPtMgrNum ).MaxSetHum, 3 ) + "] is less than " + cNumericFieldNames( 1 ) + "=[" + RoundSigDigits( MZAverageMaxHumSetPtMgr( SetPtMgrNum ).MinSetHum, 3 ) + "]." );
			}

			NodeListError = false;
			GetNodeNums( cAlphaArgs( 3 ), NumNodes, NodeNums, NodeListError, NodeType_Air, cCurrentModuleObject, cAlphaArgs( 1 ), NodeConnectionType_SetPoint, 1, ObjectIsNotParent, _, cAlphaFieldNames( 3 ) ); // setpoint nodes
			if ( ! NodeListError ) {
				NumNodesCtrld = NumNodes;
				MZAverageMaxHumSetPtMgr( SetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				MZAverageMaxHumSetPtMgr( SetPtMgrNum ).NumCtrlNodes = NumNodesCtrld;
				MZAverageMaxHumSetPtMgr( SetPtMgrNum ).SetPt = 0.0;

				for ( CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum ) {
					MZAverageMaxHumSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrldNodeNum ) = NodeNums( CtrldNodeNum );
				}
			} else {
				//    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
				//       '", invalid field.')
				//    Call ShowContinueError('..invalid '//TRIM(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'".')
				ErrorsFound = true;
			}

			AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs + NumSZRhSetPtMgrs + NumSZHtSetPtMgrs + NumSZClSetPtMgrs + NumSZMinHumSetPtMgrs + NumSZMaxHumSetPtMgrs + NumMixedAirSetPtMgrs + NumOAPretreatSetPtMgrs + NumWarmestSetPtMgrs + NumColdestSetPtMgrs + NumWarmestSetPtMgrsTempFlow + NumRABFlowSetPtMgrs + NumMZClgAverageSetPtMgrs + NumMZHtgAverageSetPtMgrs + NumMZAverageMinHumSetPtMgrs;

			if ( ! NodeListError ) {
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes = MZAverageMaxHumSetPtMgr( SetPtMgrNum ).CtrlNodes;
			}
			AllSetPtMgr( AllSetPtMgrNum ).Name = MZAverageMaxHumSetPtMgr( SetPtMgrNum ).Name;
			AllSetPtMgr( AllSetPtMgrNum ).SPMType = iSPMType_MZMaxHumAverage;
			AllSetPtMgr( AllSetPtMgrNum ).CtrlTypeMode = MZAverageMaxHumSetPtMgr( SetPtMgrNum ).CtrlTypeMode;
			AllSetPtMgr( AllSetPtMgrNum ).NumCtrlNodes = MZAverageMaxHumSetPtMgr( SetPtMgrNum ).NumCtrlNodes;

		}

		// Input the Multizone Minimum Humidity Ratio SetPoint Managers
		if ( NumMZMinHumSetPtMgrs > 0 ) MZMinHumSetPtMgr.allocate( NumMZMinHumSetPtMgrs );

		// Input the data for each setpoint manager
		cCurrentModuleObject = "SetpointManager:MultiZone:Humidity:Minimum";
		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumMZMinHumSetPtMgrs; ++SetPtMgrNum ) {
			GetObjectItem( cCurrentModuleObject, SetPtMgrNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), MZMinHumSetPtMgr.Name(), SetPtMgrNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			MZMinHumSetPtMgr( SetPtMgrNum ).Name = cAlphaArgs( 1 );
			MZMinHumSetPtMgr( SetPtMgrNum ).AirLoopName = cAlphaArgs( 2 );
			MZMinHumSetPtMgr( SetPtMgrNum ).AirLoopNum = 0;
			MZMinHumSetPtMgr( SetPtMgrNum ).MinSetHum = rNumericArgs( 1 );
			MZMinHumSetPtMgr( SetPtMgrNum ).MaxSetHum = rNumericArgs( 2 );
			MZMinHumSetPtMgr( SetPtMgrNum ).CtrlVarType = "MinimumHumidityRatio";
			MZMinHumSetPtMgr( SetPtMgrNum ).CtrlTypeMode = iCtrlVarType_MinHumRat;

			if ( MZMinHumSetPtMgr( SetPtMgrNum ).MaxSetHum < MZMinHumSetPtMgr( SetPtMgrNum ).MinSetHum ) {
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"," );
				ShowContinueError( "..." + cNumericFieldNames( 2 ) + "=[" + RoundSigDigits( MZMinHumSetPtMgr( SetPtMgrNum ).MaxSetHum, 3 ) + "] is less than " + cNumericFieldNames( 1 ) + "=[" + RoundSigDigits( MZMinHumSetPtMgr( SetPtMgrNum ).MinSetHum, 3 ) + "]." );
			}

			NodeListError = false;
			GetNodeNums( cAlphaArgs( 3 ), NumNodes, NodeNums, NodeListError, NodeType_Air, cCurrentModuleObject, cAlphaArgs( 1 ), NodeConnectionType_SetPoint, 1, ObjectIsNotParent, _, cAlphaFieldNames( 3 ) ); // setpoint nodes
			if ( ! NodeListError ) {
				NumNodesCtrld = NumNodes;
				MZMinHumSetPtMgr( SetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				MZMinHumSetPtMgr( SetPtMgrNum ).NumCtrlNodes = NumNodesCtrld;
				MZMinHumSetPtMgr( SetPtMgrNum ).SetPt = 0.0;

				for ( CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum ) {
					MZMinHumSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrldNodeNum ) = NodeNums( CtrldNodeNum );
				}
			} else {
				//    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
				//       '", invalid field.')
				//    Call ShowContinueError('..invalid '//TRIM(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'".')
				ErrorsFound = true;
			}

			AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs + NumSZRhSetPtMgrs + NumSZHtSetPtMgrs + NumSZClSetPtMgrs + NumSZMinHumSetPtMgrs + NumSZMaxHumSetPtMgrs + NumMixedAirSetPtMgrs + NumOAPretreatSetPtMgrs + NumWarmestSetPtMgrs + NumColdestSetPtMgrs + NumWarmestSetPtMgrsTempFlow + NumRABFlowSetPtMgrs + NumMZClgAverageSetPtMgrs + NumMZHtgAverageSetPtMgrs + NumMZAverageMinHumSetPtMgrs + NumMZAverageMaxHumSetPtMgrs;

			if ( ! NodeListError ) {
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes = MZMinHumSetPtMgr( SetPtMgrNum ).CtrlNodes;
			}
			AllSetPtMgr( AllSetPtMgrNum ).Name = MZMinHumSetPtMgr( SetPtMgrNum ).Name;
			AllSetPtMgr( AllSetPtMgrNum ).SPMType = iSPMType_MZMinHum;
			AllSetPtMgr( AllSetPtMgrNum ).CtrlTypeMode = MZMinHumSetPtMgr( SetPtMgrNum ).CtrlTypeMode;
			AllSetPtMgr( AllSetPtMgrNum ).NumCtrlNodes = MZMinHumSetPtMgr( SetPtMgrNum ).NumCtrlNodes;

		}

		// Input the Multizone Maximum Humidity Ratio SetPoint Managers
		if ( NumMZMaxHumSetPtMgrs > 0 ) MZMaxHumSetPtMgr.allocate( NumMZMaxHumSetPtMgrs );

		// Input the data for each setpoint manager
		cCurrentModuleObject = "SetpointManager:MultiZone:Humidity:Maximum";
		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumMZMaxHumSetPtMgrs; ++SetPtMgrNum ) {
			GetObjectItem( cCurrentModuleObject, SetPtMgrNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), MZMaxHumSetPtMgr.Name(), SetPtMgrNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			MZMaxHumSetPtMgr( SetPtMgrNum ).Name = cAlphaArgs( 1 );
			MZMaxHumSetPtMgr( SetPtMgrNum ).AirLoopName = cAlphaArgs( 2 );
			MZMaxHumSetPtMgr( SetPtMgrNum ).AirLoopNum = 0;
			MZMaxHumSetPtMgr( SetPtMgrNum ).MinSetHum = rNumericArgs( 1 );
			MZMaxHumSetPtMgr( SetPtMgrNum ).MaxSetHum = rNumericArgs( 2 );
			MZMaxHumSetPtMgr( SetPtMgrNum ).CtrlVarType = "MaximumHumidityRatio";
			MZMaxHumSetPtMgr( SetPtMgrNum ).CtrlTypeMode = iCtrlVarType_MaxHumRat;

			if ( MZMaxHumSetPtMgr( SetPtMgrNum ).MaxSetHum < MZMaxHumSetPtMgr( SetPtMgrNum ).MinSetHum ) {
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"," );
				ShowContinueError( "..." + cNumericFieldNames( 2 ) + "=[" + RoundSigDigits( MZMaxHumSetPtMgr( SetPtMgrNum ).MaxSetHum, 3 ) + "] is less than " + cNumericFieldNames( 1 ) + "=[" + RoundSigDigits( MZMaxHumSetPtMgr( SetPtMgrNum ).MinSetHum, 3 ) + "]." );
			}

			NodeListError = false;
			GetNodeNums( cAlphaArgs( 3 ), NumNodes, NodeNums, NodeListError, NodeType_Air, cCurrentModuleObject, cAlphaArgs( 1 ), NodeConnectionType_SetPoint, 1, ObjectIsNotParent, _, cAlphaFieldNames( 3 ) ); // setpoint nodes
			if ( ! NodeListError ) {
				NumNodesCtrld = NumNodes;
				MZMaxHumSetPtMgr( SetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				MZMaxHumSetPtMgr( SetPtMgrNum ).NumCtrlNodes = NumNodesCtrld;
				MZMaxHumSetPtMgr( SetPtMgrNum ).SetPt = 0.0;

				for ( CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum ) {
					MZMaxHumSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrldNodeNum ) = NodeNums( CtrldNodeNum );
				}
			} else {
				//    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
				//       '", invalid field.')
				//    Call ShowContinueError('..invalid '//TRIM(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'".')
				ErrorsFound = true;
			}

			AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs + NumSZRhSetPtMgrs + NumSZHtSetPtMgrs + NumSZClSetPtMgrs + NumSZMinHumSetPtMgrs + NumSZMaxHumSetPtMgrs + NumMixedAirSetPtMgrs + NumOAPretreatSetPtMgrs + NumWarmestSetPtMgrs + NumColdestSetPtMgrs + NumWarmestSetPtMgrsTempFlow + NumRABFlowSetPtMgrs + NumMZClgAverageSetPtMgrs + NumMZHtgAverageSetPtMgrs + NumMZAverageMinHumSetPtMgrs + NumMZAverageMaxHumSetPtMgrs + NumMZMinHumSetPtMgrs;

			if ( ! NodeListError ) {
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes = MZMaxHumSetPtMgr( SetPtMgrNum ).CtrlNodes;
			}
			AllSetPtMgr( AllSetPtMgrNum ).Name = MZMaxHumSetPtMgr( SetPtMgrNum ).Name;
			AllSetPtMgr( AllSetPtMgrNum ).SPMType = iSPMType_MZMaxHum;
			AllSetPtMgr( AllSetPtMgrNum ).CtrlTypeMode = MZMaxHumSetPtMgr( SetPtMgrNum ).CtrlTypeMode;
			AllSetPtMgr( AllSetPtMgrNum ).NumCtrlNodes = MZMaxHumSetPtMgr( SetPtMgrNum ).NumCtrlNodes;

		}

		// Input the Follow Outdoor Air Temperature Setpoint Managers

		if ( NumFollowOATempSetPtMgrs > 0 ) FollowOATempSetPtMgr.allocate( NumFollowOATempSetPtMgrs );

		// Input the data for each Setpoint Manager
		cCurrentModuleObject = "SetpointManager:FollowOutdoorAirTemperature";
		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumFollowOATempSetPtMgrs; ++SetPtMgrNum ) {
			GetObjectItem( cCurrentModuleObject, SetPtMgrNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), FollowOATempSetPtMgr.Name(), SetPtMgrNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			FollowOATempSetPtMgr( SetPtMgrNum ).Name = cAlphaArgs( 1 );
			FollowOATempSetPtMgr( SetPtMgrNum ).CtrlVarType = cAlphaArgs( 2 );
			if ( SameString( FollowOATempSetPtMgr( SetPtMgrNum ).CtrlVarType, "Temperature" ) ) {
				FollowOATempSetPtMgr( SetPtMgrNum ).CtrlTypeMode = iCtrlVarType_Temp;
			} else if ( SameString( FollowOATempSetPtMgr( SetPtMgrNum ).CtrlVarType, "MaximumTemperature" ) ) {
				FollowOATempSetPtMgr( SetPtMgrNum ).CtrlTypeMode = iCtrlVarType_MaxTemp;
			} else if ( SameString( FollowOATempSetPtMgr( SetPtMgrNum ).CtrlVarType, "MinimumTemperature" ) ) {
				FollowOATempSetPtMgr( SetPtMgrNum ).CtrlTypeMode = iCtrlVarType_MinTemp;
			} else {
				// should not come here if idd type choice and key list is working
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid field." );
				ShowContinueError( "..invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\"." );
				ShowContinueError( "..Valid values are \"Temperature\",\"MaximumTemperature\" or \"MinimumTemperature\"." );
				ErrorsFound = true;
			}
			FollowOATempSetPtMgr( SetPtMgrNum ).RefTempType = cAlphaArgs( 3 );
			if ( SameString( FollowOATempSetPtMgr( SetPtMgrNum ).RefTempType, "OutdoorAirWetBulb" ) ) {
				FollowOATempSetPtMgr( SetPtMgrNum ).RefTypeMode = iRefTempType_WetBulb;
			} else if ( SameString( FollowOATempSetPtMgr( SetPtMgrNum ).RefTempType, "OutdoorAirDryBulb" ) ) {
				FollowOATempSetPtMgr( SetPtMgrNum ).RefTypeMode = iRefTempType_DryBulb;
			} else {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid field." );
				ShowContinueError( "..invalid " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\"." );
				ShowContinueError( "..Valid values are \"OutdoorAirWetBulb\" or \"OutdoorAirDryBulb\"." );
				ErrorsFound = true;
			}
			FollowOATempSetPtMgr( SetPtMgrNum ).Offset = rNumericArgs( 1 );
			FollowOATempSetPtMgr( SetPtMgrNum ).MaxSetTemp = rNumericArgs( 2 );
			FollowOATempSetPtMgr( SetPtMgrNum ).MinSetTemp = rNumericArgs( 3 );
			if ( FollowOATempSetPtMgr( SetPtMgrNum ).MaxSetTemp < FollowOATempSetPtMgr( SetPtMgrNum ).MinSetTemp ) {
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"," );
				ShowContinueError( "..." + cNumericFieldNames( 2 ) + "=[" + RoundSigDigits( FollowOATempSetPtMgr( SetPtMgrNum ).MaxSetTemp, 1 ) + "] is less than " + cNumericFieldNames( 3 ) + "=[" + RoundSigDigits( FollowOATempSetPtMgr( SetPtMgrNum ).MinSetTemp, 1 ) + "]." );
			}

			NodeListError = false;
			GetNodeNums( cAlphaArgs( 4 ), NumNodes, NodeNums, NodeListError, NodeType_Unknown, cCurrentModuleObject, cAlphaArgs( 1 ), NodeConnectionType_SetPoint, 1, ObjectIsNotParent, _, cAlphaFieldNames( 4 ) ); // setpoint nodes
			if ( ! NodeListError ) {
				NumNodesCtrld = NumNodes;
				FollowOATempSetPtMgr( SetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				FollowOATempSetPtMgr( SetPtMgrNum ).NumCtrlNodes = NumNodesCtrld;
				FollowOATempSetPtMgr( SetPtMgrNum ).SetPt = 0.0;

				for ( CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum ) {
					FollowOATempSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrldNodeNum ) = NodeNums( CtrldNodeNum );
				}
			} else {
				//    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
				//       '", invalid field.')
				//    Call ShowContinueError('..invalid '//TRIM(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))//'".')
				ErrorsFound = true;
			}

			AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs + NumSZRhSetPtMgrs + NumSZHtSetPtMgrs + NumSZClSetPtMgrs + NumSZMinHumSetPtMgrs + NumSZMaxHumSetPtMgrs + NumMixedAirSetPtMgrs + NumOAPretreatSetPtMgrs + NumWarmestSetPtMgrs + NumColdestSetPtMgrs + NumWarmestSetPtMgrsTempFlow + NumRABFlowSetPtMgrs + NumMZClgAverageSetPtMgrs + NumMZHtgAverageSetPtMgrs + NumMZAverageMinHumSetPtMgrs + NumMZAverageMaxHumSetPtMgrs + NumMZMinHumSetPtMgrs + NumMZMaxHumSetPtMgrs;

			if ( ! NodeListError ) {
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes = FollowOATempSetPtMgr( SetPtMgrNum ).CtrlNodes;
			}
			AllSetPtMgr( AllSetPtMgrNum ).Name = FollowOATempSetPtMgr( SetPtMgrNum ).Name;
			AllSetPtMgr( AllSetPtMgrNum ).SPMType = iSPMType_FollowOATemp;
			AllSetPtMgr( AllSetPtMgrNum ).CtrlTypeMode = FollowOATempSetPtMgr( SetPtMgrNum ).CtrlTypeMode;
			AllSetPtMgr( AllSetPtMgrNum ).NumCtrlNodes = FollowOATempSetPtMgr( SetPtMgrNum ).NumCtrlNodes;

		}

		// Input the Follow System Node Temperature Setpoint Managers

		if ( NumFollowSysNodeTempSetPtMgrs > 0 ) FollowSysNodeTempSetPtMgr.allocate( NumFollowSysNodeTempSetPtMgrs );

		// Input the data for each Setpoint Manager
		cCurrentModuleObject = "SetpointManager:FollowSystemNodeTemperature";
		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumFollowSysNodeTempSetPtMgrs; ++SetPtMgrNum ) {
			GetObjectItem( cCurrentModuleObject, SetPtMgrNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), FollowSysNodeTempSetPtMgr.Name(), SetPtMgrNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			FollowSysNodeTempSetPtMgr( SetPtMgrNum ).Name = cAlphaArgs( 1 );
			FollowSysNodeTempSetPtMgr( SetPtMgrNum ).CtrlVarType = cAlphaArgs( 2 );
			if ( SameString( FollowSysNodeTempSetPtMgr( SetPtMgrNum ).CtrlVarType, "Temperature" ) ) {
				FollowSysNodeTempSetPtMgr( SetPtMgrNum ).CtrlTypeMode = iCtrlVarType_Temp;
			} else if ( SameString( FollowSysNodeTempSetPtMgr( SetPtMgrNum ).CtrlVarType, "MaximumTemperature" ) ) {
				FollowSysNodeTempSetPtMgr( SetPtMgrNum ).CtrlTypeMode = iCtrlVarType_MaxTemp;
			} else if ( SameString( FollowSysNodeTempSetPtMgr( SetPtMgrNum ).CtrlVarType, "MinimumTemperature" ) ) {
				FollowSysNodeTempSetPtMgr( SetPtMgrNum ).CtrlTypeMode = iCtrlVarType_MinTemp;
			} else {
				// should not come here if idd type choice and key list is working
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid field." );
				ShowContinueError( "..invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\"." );
				ShowContinueError( "..Valid values are \"Temperature\",\"MaximumTemperature\" or \"MinimumTemperature\"." );
				ErrorsFound = true;
			}
			FollowSysNodeTempSetPtMgr( SetPtMgrNum ).RefNodeNum = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Unknown, NodeConnectionType_Sensor, 1, ObjectIsNotParent );
			FollowSysNodeTempSetPtMgr( SetPtMgrNum ).RefTempType = cAlphaArgs( 4 );
			if ( SameString( FollowSysNodeTempSetPtMgr( SetPtMgrNum ).RefTempType, "NodeWetBulb" ) ) {
				FollowSysNodeTempSetPtMgr( SetPtMgrNum ).RefTypeMode = iRefTempType_WetBulb;
			} else if ( SameString( FollowSysNodeTempSetPtMgr( SetPtMgrNum ).RefTempType, "NodeDryBulb" ) ) {
				FollowSysNodeTempSetPtMgr( SetPtMgrNum ).RefTypeMode = iRefTempType_DryBulb;
			} else {
				// should not come here if idd type choice and key list is working
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid field." );
				ShowContinueError( "..invalid " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\"." );
				ShowContinueError( "..Valid values are \"NodeWetBulb\" or \"NodeDryBulb\"." );
				ErrorsFound = true;
			}
			FollowSysNodeTempSetPtMgr( SetPtMgrNum ).Offset = rNumericArgs( 1 );
			FollowSysNodeTempSetPtMgr( SetPtMgrNum ).MaxSetTemp = rNumericArgs( 2 );
			FollowSysNodeTempSetPtMgr( SetPtMgrNum ).MinSetTemp = rNumericArgs( 3 );
			if ( FollowSysNodeTempSetPtMgr( SetPtMgrNum ).MaxSetTemp < FollowSysNodeTempSetPtMgr( SetPtMgrNum ).MinSetTemp ) {
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"," );
				ShowContinueError( "..." + cNumericFieldNames( 2 ) + "=[" + RoundSigDigits( FollowSysNodeTempSetPtMgr( SetPtMgrNum ).MaxSetTemp, 1 ) + "] is less than " + cNumericFieldNames( 3 ) + "=[" + RoundSigDigits( FollowSysNodeTempSetPtMgr( SetPtMgrNum ).MinSetTemp, 1 ) + "]." );
			}

			NodeListError = false;
			GetNodeNums( cAlphaArgs( 5 ), NumNodes, NodeNums, NodeListError, NodeType_Unknown, cCurrentModuleObject, cAlphaArgs( 1 ), NodeConnectionType_SetPoint, 1, ObjectIsNotParent, _, cAlphaFieldNames( 5 ) ); // setpoint nodes
			if ( ! NodeListError ) {
				NumNodesCtrld = NumNodes;
				FollowSysNodeTempSetPtMgr( SetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				FollowSysNodeTempSetPtMgr( SetPtMgrNum ).NumCtrlNodes = NumNodesCtrld;
				FollowSysNodeTempSetPtMgr( SetPtMgrNum ).SetPt = 0.0;

				for ( CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum ) {
					FollowSysNodeTempSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrldNodeNum ) = NodeNums( CtrldNodeNum );
				}
			} else {
				//    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
				//       '", invalid field.')
				//    Call ShowContinueError('..invalid '//TRIM(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))//'".')
				ErrorsFound = true;
			}

			AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs + NumSZRhSetPtMgrs + NumSZHtSetPtMgrs + NumSZClSetPtMgrs + NumSZMinHumSetPtMgrs + NumSZMaxHumSetPtMgrs + NumMixedAirSetPtMgrs + NumOAPretreatSetPtMgrs + NumWarmestSetPtMgrs + NumColdestSetPtMgrs + NumWarmestSetPtMgrsTempFlow + NumRABFlowSetPtMgrs + NumMZClgAverageSetPtMgrs + NumMZHtgAverageSetPtMgrs + NumMZAverageMinHumSetPtMgrs + NumMZAverageMaxHumSetPtMgrs + NumMZMinHumSetPtMgrs + NumMZMaxHumSetPtMgrs + NumFollowOATempSetPtMgrs;

			if ( ! NodeListError ) {
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes = FollowSysNodeTempSetPtMgr( SetPtMgrNum ).CtrlNodes;
			}
			AllSetPtMgr( AllSetPtMgrNum ).Name = FollowSysNodeTempSetPtMgr( SetPtMgrNum ).Name;
			AllSetPtMgr( AllSetPtMgrNum ).SPMType = iSPMType_FollowSysNodeTemp;
			AllSetPtMgr( AllSetPtMgrNum ).CtrlTypeMode = FollowSysNodeTempSetPtMgr( SetPtMgrNum ).CtrlTypeMode;
			AllSetPtMgr( AllSetPtMgrNum ).NumCtrlNodes = FollowSysNodeTempSetPtMgr( SetPtMgrNum ).NumCtrlNodes;

		}

		// Input the Ground Temperature Setpoint Managers

		if ( NumGroundTempSetPtMgrs > 0 ) GroundTempSetPtMgr.allocate( NumGroundTempSetPtMgrs );

		// Input the data for each Setpoint Manager
		cCurrentModuleObject = "SetpointManager:FollowGroundTemperature";
		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumGroundTempSetPtMgrs; ++SetPtMgrNum ) {
			GetObjectItem( cCurrentModuleObject, SetPtMgrNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), GroundTempSetPtMgr.Name(), SetPtMgrNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			GroundTempSetPtMgr( SetPtMgrNum ).Name = cAlphaArgs( 1 );
			GroundTempSetPtMgr( SetPtMgrNum ).CtrlVarType = cAlphaArgs( 2 );
			if ( SameString( GroundTempSetPtMgr( SetPtMgrNum ).CtrlVarType, "Temperature" ) ) {
				GroundTempSetPtMgr( SetPtMgrNum ).CtrlTypeMode = iCtrlVarType_Temp;
			} else if ( SameString( GroundTempSetPtMgr( SetPtMgrNum ).CtrlVarType, "MaximumTemperature" ) ) {
				GroundTempSetPtMgr( SetPtMgrNum ).CtrlTypeMode = iCtrlVarType_MaxTemp;
			} else if ( SameString( GroundTempSetPtMgr( SetPtMgrNum ).CtrlVarType, "MinimumTemperature" ) ) {
				GroundTempSetPtMgr( SetPtMgrNum ).CtrlTypeMode = iCtrlVarType_MinTemp;
			} else {
				// should not come here if idd type choice and key list is working
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid field." );
				ShowContinueError( "..invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\"." );
				ShowContinueError( "..Valid values are \"Temperature\",\"MaximumTemperature\" or \"MinimumTemperature\"." );
				ErrorsFound = true;
			}
			GroundTempSetPtMgr( SetPtMgrNum ).RefGroundTempObjType = cAlphaArgs( 3 );
			if ( SameString( GroundTempSetPtMgr( SetPtMgrNum ).RefGroundTempObjType, "Site:GroundTemperature:BuildingSurface" ) ) {
				GroundTempSetPtMgr( SetPtMgrNum ).RefTypeMode = iRefGroundTempObjType_BuildingSurface;
				if ( NoSurfaceGroundTempObjWarning ) {
					if ( ! GroundTempObjInput ) {
						ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" requires " "\"Site:GroundTemperature:BuildingSurface\" in the input." );
						ShowContinueError( "Defaults, constant throughout the year of (" + RoundSigDigits( GroundTemp, 1 ) + ") will be used." );
					}
					NoSurfaceGroundTempObjWarning = false;
				}
			} else if ( SameString( GroundTempSetPtMgr( SetPtMgrNum ).RefGroundTempObjType, "Site:GroundTemperature:Shallow" ) ) {
				GroundTempSetPtMgr( SetPtMgrNum ).RefTypeMode = iRefGroundTempObjType_Shallow;
				if ( NoShallowGroundTempObjWarning ) {
					if ( ! GroundTemp_SurfaceObjInput ) {
						ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" requires " "\"Site:GroundTemperature:Shallow\" in the input." );
						ShowContinueError( "Defaults, constant throughout the year of (" + RoundSigDigits( GroundTemp_Surface, 1 ) + ") will be used." );
					}
					NoShallowGroundTempObjWarning = false;
				}
			} else if ( SameString( GroundTempSetPtMgr( SetPtMgrNum ).RefGroundTempObjType, "Site:GroundTemperature:Deep" ) ) {
				GroundTempSetPtMgr( SetPtMgrNum ).RefTypeMode = iRefGroundTempObjType_Deep;
				if ( NoDeepGroundTempObjWarning ) {
					if ( ! GroundTemp_DeepObjInput ) {
						ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" requires " "\"Site:GroundTemperature:Deep\" in the input." );
						ShowContinueError( "Defaults, constant throughout the year of (" + RoundSigDigits( GroundTemp_Deep, 1 ) + ") will be used." );
					}
					NoDeepGroundTempObjWarning = false;
				}
			} else if ( SameString( GroundTempSetPtMgr( SetPtMgrNum ).RefGroundTempObjType, "Site:GroundTemperature:FCfactorMethod" ) ) {
				GroundTempSetPtMgr( SetPtMgrNum ).RefTypeMode = iRefGroundTempObjType_FCfactorMethod;
				if ( NoFCGroundTempObjWarning ) {
					if ( ! FCGroundTemps ) {
						ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" requires " "\"Site:GroundTemperature:FCfactorMethod\" in the input." );
						ShowContinueError( "Defaults, constant throughout the year of (" + RoundSigDigits( GroundTempFC, 1 ) + ") will be used." );
					}
					NoFCGroundTempObjWarning = false;
				}
			} else {
				// should not come here if idd type choice and key list is working
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid field." );
				ShowContinueError( "..invalid " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\"." );
				ShowContinueError( "..Valid values are \"Site:GroundTemperature:BuildingSurface\", \"Site:GroundTemperature:Shallow\"," );
				ShowContinueError( "     \"Site:GroundTemperature:Deep\" or \"Site:GroundTemperature:FCfactorMethod\"." );
				ErrorsFound = true;
			}
			GroundTempSetPtMgr( SetPtMgrNum ).Offset = rNumericArgs( 1 );
			GroundTempSetPtMgr( SetPtMgrNum ).MaxSetTemp = rNumericArgs( 2 );
			GroundTempSetPtMgr( SetPtMgrNum ).MinSetTemp = rNumericArgs( 3 );
			if ( GroundTempSetPtMgr( SetPtMgrNum ).MaxSetTemp < GroundTempSetPtMgr( SetPtMgrNum ).MinSetTemp ) {
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"," );
				ShowContinueError( "..." + cNumericFieldNames( 2 ) + "=[" + RoundSigDigits( GroundTempSetPtMgr( SetPtMgrNum ).MaxSetTemp, 1 ) + "] is less than " + cNumericFieldNames( 3 ) + "=[" + RoundSigDigits( GroundTempSetPtMgr( SetPtMgrNum ).MinSetTemp, 1 ) + "]." );
			}

			NodeListError = false;
			GetNodeNums( cAlphaArgs( 4 ), NumNodes, NodeNums, NodeListError, NodeType_Unknown, cCurrentModuleObject, cAlphaArgs( 1 ), NodeConnectionType_SetPoint, 1, ObjectIsNotParent, _, cAlphaFieldNames( 4 ) ); // setpoint nodes
			if ( ! NodeListError ) {
				NumNodesCtrld = NumNodes;
				GroundTempSetPtMgr( SetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				GroundTempSetPtMgr( SetPtMgrNum ).NumCtrlNodes = NumNodesCtrld;
				GroundTempSetPtMgr( SetPtMgrNum ).SetPt = 0.0;

				for ( CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum ) {
					GroundTempSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrldNodeNum ) = NodeNums( CtrldNodeNum );
				}
			} else {
				//    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
				//       '", invalid field.')
				//    Call ShowContinueError('..invalid '//TRIM(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))//'".')
				ErrorsFound = true;
			}

			AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs + NumSZRhSetPtMgrs + NumSZHtSetPtMgrs + NumSZClSetPtMgrs + NumSZMinHumSetPtMgrs + NumSZMaxHumSetPtMgrs + NumMixedAirSetPtMgrs + NumOAPretreatSetPtMgrs + NumWarmestSetPtMgrs + NumColdestSetPtMgrs + NumWarmestSetPtMgrsTempFlow + NumRABFlowSetPtMgrs + NumMZClgAverageSetPtMgrs + NumMZHtgAverageSetPtMgrs + NumMZAverageMinHumSetPtMgrs + NumMZAverageMaxHumSetPtMgrs + NumMZMinHumSetPtMgrs + NumMZMaxHumSetPtMgrs + NumFollowOATempSetPtMgrs + NumFollowSysNodeTempSetPtMgrs;

			if ( ! NodeListError ) {
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes = GroundTempSetPtMgr( SetPtMgrNum ).CtrlNodes;
			}
			AllSetPtMgr( AllSetPtMgrNum ).Name = GroundTempSetPtMgr( SetPtMgrNum ).Name;
			AllSetPtMgr( AllSetPtMgrNum ).SPMType = iSPMType_GroundTemp;
			AllSetPtMgr( AllSetPtMgrNum ).CtrlTypeMode = GroundTempSetPtMgr( SetPtMgrNum ).CtrlTypeMode;
			AllSetPtMgr( AllSetPtMgrNum ).NumCtrlNodes = GroundTempSetPtMgr( SetPtMgrNum ).NumCtrlNodes;

		}

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in input.  Program terminates." );
		}

		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumWarmestSetPtMgrsTempFlow; ++SetPtMgrNum ) {
			SetupOutputVariable( "Setpoint Manager Warmest Temperature Critical Zone Number []", WarmestSetPtMgrTempFlow( SetPtMgrNum ).CritZoneNum, "System", "Average", WarmestSetPtMgrTempFlow( SetPtMgrNum ).Name );
			SetupOutputVariable( "Setpoint Manager Warmest Temperature Turndown Flow Fraction []", WarmestSetPtMgrTempFlow( SetPtMgrNum ).Turndown, "System", "Average", WarmestSetPtMgrTempFlow( SetPtMgrNum ).Name );
		}

		// Input the Condenser Entering Set Point Managers

		if ( NumCondEntSetPtMgrs > 0 ) CondEntSetPtMgr.allocate( NumCondEntSetPtMgrs ); // Allocate the Set Point Manager input data array

		// Input the data for each Set Point Manager
		cCurrentModuleObject = "SetpointManager:CondenserEnteringReset";

		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumCondEntSetPtMgrs; ++SetPtMgrNum ) {
			GetObjectItem( cCurrentModuleObject, SetPtMgrNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), CondEntSetPtMgr.Name(), SetPtMgrNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			CondEntSetPtMgr( SetPtMgrNum ).Name = cAlphaArgs( 1 );
			CondEntSetPtMgr( SetPtMgrNum ).CtrlVarType = cAlphaArgs( 2 );
			if ( SameString( CondEntSetPtMgr( SetPtMgrNum ).CtrlVarType, "Temperature" ) ) {
				CondEntSetPtMgr( SetPtMgrNum ).CtrlTypeMode = iCtrlVarType_Temp;
			} else {
				// should not come here if idd type choice and key list is working
				ShowSevereError( " found invalid control type of " + cAlphaArgs( 2 ) + " in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}
			CondEntSetPtMgr( SetPtMgrNum ).CondEntTempSched = cAlphaArgs( 3 );
			CondEntSetPtMgr( SetPtMgrNum ).CondEntTempSchedPtr = GetScheduleIndex( cAlphaArgs( 3 ) );
			CondEntSetPtMgr( SetPtMgrNum ).MinTwrWbCurve = GetCurveIndex( cAlphaArgs( 4 ) );
			CondEntSetPtMgr( SetPtMgrNum ).MinOaWbCurve = GetCurveIndex( cAlphaArgs( 5 ) );
			CondEntSetPtMgr( SetPtMgrNum ).OptCondEntCurve = GetCurveIndex( cAlphaArgs( 6 ) );
			CondEntSetPtMgr( SetPtMgrNum ).MinimumLiftTD = rNumericArgs( 1 );
			CondEntSetPtMgr( SetPtMgrNum ).MaxCondEntTemp = rNumericArgs( 2 );
			CondEntSetPtMgr( SetPtMgrNum ).TowerDsnInletAirWetBulb = rNumericArgs( 3 );
			CondEntSetPtMgr( SetPtMgrNum ).CtrlNodeListName = cAlphaArgs( 7 );
			if ( CondEntSetPtMgr( SetPtMgrNum ).MaxCondEntTemp < CondEntSetPtMgr( SetPtMgrNum ).TowerDsnInletAirWetBulb ) {
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"," );
				ShowContinueError( "..." + cNumericFieldNames( 2 ) + "=[" + RoundSigDigits( CondEntSetPtMgr( SetPtMgrNum ).MaxCondEntTemp, 1 ) + "] is less than " + cNumericFieldNames( 1 ) + "=[" + RoundSigDigits( CondEntSetPtMgr( SetPtMgrNum ).TowerDsnInletAirWetBulb, 1 ) + "]." );
			}

			NodeListError = false;
			GetNodeNums( CondEntSetPtMgr( SetPtMgrNum ).CtrlNodeListName, NumNodes, NodeNums, NodeListError, NodeType_Unknown, cCurrentModuleObject, cAlphaArgs( 1 ), NodeConnectionType_SetPoint, 1, ObjectIsNotParent, _, cAlphaFieldNames( 7 ) );
			if ( ! NodeListError ) {
				NumNodesCtrld = NumNodes;
				CondEntSetPtMgr( SetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				CondEntSetPtMgr( SetPtMgrNum ).NumCtrlNodes = NumNodesCtrld;
				CondEntSetPtMgr( SetPtMgrNum ).SetPt = 0.0;

				for ( CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum ) {
					CondEntSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrldNodeNum ) = NodeNums( CtrldNodeNum );
				}
			} else {
				//    CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(3))//' in '//TRIM(cCurrentModuleObject)//' = '// &
				//                           TRIM(CondEntSetPtMgr(SetPtMgrNum)%Name))
				ErrorsFound = true;
			}

			AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs + NumSZRhSetPtMgrs + NumSZHtSetPtMgrs + NumSZClSetPtMgrs + NumSZMinHumSetPtMgrs + NumSZMaxHumSetPtMgrs + NumMixedAirSetPtMgrs + NumOAPretreatSetPtMgrs + NumWarmestSetPtMgrs + NumColdestSetPtMgrs + NumWarmestSetPtMgrsTempFlow + NumRABFlowSetPtMgrs + NumMZClgAverageSetPtMgrs + NumMZHtgAverageSetPtMgrs + NumMZAverageMinHumSetPtMgrs + NumMZAverageMaxHumSetPtMgrs + NumMZMinHumSetPtMgrs + NumMZMaxHumSetPtMgrs + NumFollowOATempSetPtMgrs + NumFollowSysNodeTempSetPtMgrs + NumGroundTempSetPtMgrs;

			if ( ! NodeListError ) {
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes = CondEntSetPtMgr( SetPtMgrNum ).CtrlNodes;
			}
			AllSetPtMgr( AllSetPtMgrNum ).Name = CondEntSetPtMgr( SetPtMgrNum ).Name;
			AllSetPtMgr( AllSetPtMgrNum ).SPMType = iSPMType_CondEntReset;
			AllSetPtMgr( AllSetPtMgrNum ).CtrlTypeMode = CondEntSetPtMgr( SetPtMgrNum ).CtrlTypeMode;
			AllSetPtMgr( AllSetPtMgrNum ).NumCtrlNodes = CondEntSetPtMgr( SetPtMgrNum ).NumCtrlNodes;

		}

		// Input the Ideal Condenser Entering Set Point Managers

		// Allocate the Set Point Manager input data array
		if ( NumIdealCondEntSetPtMgrs > 0 ) IdealCondEntSetPtMgr.allocate( NumIdealCondEntSetPtMgrs );

		// Input the data for each Set Point Manager
		cCurrentModuleObject = "SetpointManager:CondenserEnteringReset:Ideal";

		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumIdealCondEntSetPtMgrs; ++SetPtMgrNum ) {
			GetObjectItem( cCurrentModuleObject, SetPtMgrNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), IdealCondEntSetPtMgr.Name(), SetPtMgrNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			IdealCondEntSetPtMgr( SetPtMgrNum ).Name = cAlphaArgs( 1 );
			IdealCondEntSetPtMgr( SetPtMgrNum ).CtrlVarType = cAlphaArgs( 2 );
			if ( SameString( IdealCondEntSetPtMgr( SetPtMgrNum ).CtrlVarType, "Temperature" ) ) {
				IdealCondEntSetPtMgr( SetPtMgrNum ).CtrlTypeMode = iCtrlVarType_Temp;
			} else {
				ShowSevereError( " found invalid control type of " + cAlphaArgs( 2 ) + " in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}
			IdealCondEntSetPtMgr( SetPtMgrNum ).MinimumLiftTD = rNumericArgs( 1 );
			IdealCondEntSetPtMgr( SetPtMgrNum ).MaxCondEntTemp = rNumericArgs( 2 );
			IdealCondEntSetPtMgr( SetPtMgrNum ).CtrlNodeListName = cAlphaArgs( 3 );

			NodeListError = false;
			GetNodeNums( IdealCondEntSetPtMgr( SetPtMgrNum ).CtrlNodeListName, NumNodes, NodeNums, NodeListError, NodeType_Unknown, cCurrentModuleObject, cAlphaArgs( 1 ), NodeConnectionType_SetPoint, 1, ObjectIsNotParent, _, cAlphaFieldNames( 3 ) );
			if ( ! NodeListError ) {
				NumNodesCtrld = NumNodes;
				IdealCondEntSetPtMgr( SetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				IdealCondEntSetPtMgr( SetPtMgrNum ).NumCtrlNodes = NumNodesCtrld;
				IdealCondEntSetPtMgr( SetPtMgrNum ).SetPt = 0.0;

				for ( CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum ) {
					IdealCondEntSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrldNodeNum ) = NodeNums( CtrldNodeNum );
				}
			} else {
				//    CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(3))//' in '//TRIM(cCurrentModuleObject)//' = '// &
				//                           TRIM(IdealCondEntSetPtMgr(SetPtMgrNum)%Name))
				ErrorsFound = true;
			}

			AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs + NumSZRhSetPtMgrs + NumSZHtSetPtMgrs + NumSZClSetPtMgrs + NumSZMinHumSetPtMgrs + NumSZMaxHumSetPtMgrs + NumMixedAirSetPtMgrs + NumOAPretreatSetPtMgrs + NumWarmestSetPtMgrs + NumColdestSetPtMgrs + NumWarmestSetPtMgrsTempFlow + NumRABFlowSetPtMgrs + NumMZClgAverageSetPtMgrs + NumMZHtgAverageSetPtMgrs + NumMZAverageMinHumSetPtMgrs + NumMZAverageMaxHumSetPtMgrs + NumMZMinHumSetPtMgrs + NumMZMaxHumSetPtMgrs + NumFollowOATempSetPtMgrs + NumFollowSysNodeTempSetPtMgrs + NumGroundTempSetPtMgrs + NumCondEntSetPtMgrs;

			if ( ! NodeListError ) {
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes = IdealCondEntSetPtMgr( SetPtMgrNum ).CtrlNodes;
			}
			AllSetPtMgr( AllSetPtMgrNum ).Name = IdealCondEntSetPtMgr( SetPtMgrNum ).Name;
			AllSetPtMgr( AllSetPtMgrNum ).SPMType = iSPMType_IdealCondEntReset;
			AllSetPtMgr( AllSetPtMgrNum ).CtrlTypeMode = IdealCondEntSetPtMgr( SetPtMgrNum ).CtrlTypeMode;
			AllSetPtMgr( AllSetPtMgrNum ).NumCtrlNodes = IdealCondEntSetPtMgr( SetPtMgrNum ).NumCtrlNodes;

		}

		if ( NumSZOneStageCoolingSetPtMgrs > 0 ) SZOneStageCoolingSetPtMgr.allocate( NumSZOneStageCoolingSetPtMgrs );

		cCurrentModuleObject = "SetpointManager:SingleZone:OneStageCooling";
		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumSZOneStageCoolingSetPtMgrs; ++SetPtMgrNum ) {
			GetObjectItem( cCurrentModuleObject, SetPtMgrNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), SZOneStageCoolingSetPtMgr.Name(), SetPtMgrNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			SZOneStageCoolingSetPtMgr( SetPtMgrNum ).Name = cAlphaArgs( 1 );
			SZOneStageCoolingSetPtMgr( SetPtMgrNum ).CtrlVarType = "Temperature";
			SZOneStageCoolingSetPtMgr( SetPtMgrNum ).CtrlTypeMode = iCtrlVarType_Temp;
			SZOneStageCoolingSetPtMgr( SetPtMgrNum ).CoolingOnTemp = rNumericArgs( 1 );
			SZOneStageCoolingSetPtMgr( SetPtMgrNum ).CoolingOffTemp = rNumericArgs( 2 );

			if ( SZOneStageCoolingSetPtMgr( SetPtMgrNum ).CoolingOffTemp < SZOneStageCoolingSetPtMgr( SetPtMgrNum ).CoolingOnTemp ) {
				// throw warning, off must be warmer than on
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"," );
				ShowContinueError( "..." + cNumericFieldNames( 2 ) + "=[" + RoundSigDigits( SZOneStageCoolingSetPtMgr( SetPtMgrNum ).CoolingOffTemp, 1 ) + "] is less than " + cNumericFieldNames( 1 ) + "=[" + RoundSigDigits( SZOneStageCoolingSetPtMgr( SetPtMgrNum ).CoolingOnTemp, 1 ) + "]." );
			}

			SZOneStageCoolingSetPtMgr( SetPtMgrNum ).ControlZoneName = cAlphaArgs( 2 );
			SZOneStageCoolingSetPtMgr( SetPtMgrNum ).ZoneNodeNum = GetSystemNodeNumberForZone( cAlphaArgs( 2 ) );
			// get the actual zone number of the control zone
			SZOneStageCoolingSetPtMgr( SetPtMgrNum ).ControlZoneNum = FindItemInList( cAlphaArgs( 2 ), Zone.Name(), NumOfZones );
			if ( SZOneStageCoolingSetPtMgr( SetPtMgrNum ).ControlZoneNum == 0 ) {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid field." );
				ShowContinueError( "..invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\"." );
				ErrorsFound = true;
			} else {
				if ( allocated( StageZoneLogic ) ) {
					if ( ! StageZoneLogic( SZOneStageCoolingSetPtMgr( SetPtMgrNum ).ControlZoneNum ) ) {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid field." );
						ShowContinueError( "..invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\"." );
						ShowContinueError( "Zone thermostat must use ZoneControl:Thermostat:StagedDualSetpoint." );
						ErrorsFound = true;
					}
				}

			}

			NodeListError = false;
			GetNodeNums( cAlphaArgs( 3 ), NumNodes, NodeNums, NodeListError, NodeType_Unknown, cCurrentModuleObject, cAlphaArgs( 1 ), NodeConnectionType_SetPoint, 1, ObjectIsNotParent, _, cAlphaFieldNames( 3 ) ); // setpoint nodes
			if ( ! NodeListError ) {
				NumNodesCtrld = NumNodes;
				SZOneStageCoolingSetPtMgr( SetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				SZOneStageCoolingSetPtMgr( SetPtMgrNum ).NumCtrlNodes = NumNodesCtrld;
				SZOneStageCoolingSetPtMgr( SetPtMgrNum ).SetPt = 0.0;

				for ( CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum ) {
					SZOneStageCoolingSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrldNodeNum ) = NodeNums( CtrldNodeNum );
				}
			} else {
				ErrorsFound = true;
			}

			AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs + NumSZRhSetPtMgrs + NumSZHtSetPtMgrs + NumSZClSetPtMgrs + NumSZMinHumSetPtMgrs + NumSZMaxHumSetPtMgrs + NumMixedAirSetPtMgrs + NumOAPretreatSetPtMgrs + NumWarmestSetPtMgrs + NumColdestSetPtMgrs + NumWarmestSetPtMgrsTempFlow + NumRABFlowSetPtMgrs + NumMZClgAverageSetPtMgrs + NumMZHtgAverageSetPtMgrs + NumMZAverageMinHumSetPtMgrs + NumMZAverageMaxHumSetPtMgrs + NumMZMinHumSetPtMgrs + NumMZMaxHumSetPtMgrs + NumFollowOATempSetPtMgrs + NumFollowSysNodeTempSetPtMgrs + NumGroundTempSetPtMgrs + NumCondEntSetPtMgrs + NumIdealCondEntSetPtMgrs;
			if ( ! NodeListError ) {
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes = SZOneStageCoolingSetPtMgr( SetPtMgrNum ).CtrlNodes;
			}
			AllSetPtMgr( AllSetPtMgrNum ).Name = SZOneStageCoolingSetPtMgr( SetPtMgrNum ).Name;
			AllSetPtMgr( AllSetPtMgrNum ).SPMType = iSPMType_SZOneStageCooling;
			AllSetPtMgr( AllSetPtMgrNum ).CtrlTypeMode = SZOneStageCoolingSetPtMgr( SetPtMgrNum ).CtrlTypeMode;
			AllSetPtMgr( AllSetPtMgrNum ).NumCtrlNodes = SZOneStageCoolingSetPtMgr( SetPtMgrNum ).NumCtrlNodes;

		}

		if ( NumSZOneStageHeatingSetPtMgrs > 0 ) SZOneStageHeatingSetPtMgr.allocate( NumSZOneStageHeatingSetPtMgrs );

		cCurrentModuleObject = "SetpointManager:SingleZone:OneStageHeating";
		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumSZOneStageHeatingSetPtMgrs; ++SetPtMgrNum ) {
			GetObjectItem( cCurrentModuleObject, SetPtMgrNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), SZOneStageHeatingSetPtMgr.Name(), SetPtMgrNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			SZOneStageHeatingSetPtMgr( SetPtMgrNum ).Name = cAlphaArgs( 1 );
			SZOneStageHeatingSetPtMgr( SetPtMgrNum ).CtrlVarType = "Temperature";
			SZOneStageHeatingSetPtMgr( SetPtMgrNum ).CtrlTypeMode = iCtrlVarType_Temp;
			SZOneStageHeatingSetPtMgr( SetPtMgrNum ).HeatingOnTemp = rNumericArgs( 1 );
			SZOneStageHeatingSetPtMgr( SetPtMgrNum ).HeatingOffTemp = rNumericArgs( 2 );

			if ( SZOneStageHeatingSetPtMgr( SetPtMgrNum ).HeatingOffTemp > SZOneStageHeatingSetPtMgr( SetPtMgrNum ).HeatingOnTemp ) {
				// throw warning, off must be cooler than on
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"," );
				ShowContinueError( "..." + cNumericFieldNames( 2 ) + "=[" + RoundSigDigits( SZOneStageHeatingSetPtMgr( SetPtMgrNum ).HeatingOnTemp, 1 ) + "] is less than " + cNumericFieldNames( 1 ) + "=[" + RoundSigDigits( SZOneStageHeatingSetPtMgr( SetPtMgrNum ).HeatingOffTemp, 1 ) + "]." );
			}

			SZOneStageHeatingSetPtMgr( SetPtMgrNum ).ControlZoneName = cAlphaArgs( 2 );
			SZOneStageHeatingSetPtMgr( SetPtMgrNum ).ZoneNodeNum = GetSystemNodeNumberForZone( cAlphaArgs( 2 ) );
			// get the actual zone number of the control zone
			SZOneStageHeatingSetPtMgr( SetPtMgrNum ).ControlZoneNum = FindItemInList( cAlphaArgs( 2 ), Zone.Name(), NumOfZones );
			if ( SZOneStageHeatingSetPtMgr( SetPtMgrNum ).ControlZoneNum == 0 ) {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid field." );
				ShowContinueError( "..invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\"." );
				ErrorsFound = true;
			} else {
				if ( allocated( StageZoneLogic ) ) {
					if ( ! StageZoneLogic( SZOneStageHeatingSetPtMgr( SetPtMgrNum ).ControlZoneNum ) ) {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid field." );
						ShowContinueError( "..invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\"." );
						ShowContinueError( "Zone thermostat must use ZoneControl:Thermostat:StagedDualSetpoint." );
						ErrorsFound = true;
					}
				}
			}

			NodeListError = false;
			GetNodeNums( cAlphaArgs( 3 ), NumNodes, NodeNums, NodeListError, NodeType_Unknown, cCurrentModuleObject, cAlphaArgs( 1 ), NodeConnectionType_SetPoint, 1, ObjectIsNotParent, _, cAlphaFieldNames( 3 ) ); // setpoint nodes
			if ( ! NodeListError ) {
				NumNodesCtrld = NumNodes;
				SZOneStageHeatingSetPtMgr( SetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				SZOneStageHeatingSetPtMgr( SetPtMgrNum ).NumCtrlNodes = NumNodesCtrld;
				SZOneStageHeatingSetPtMgr( SetPtMgrNum ).SetPt = 0.0;

				for ( CtrldNodeNum = 1; CtrldNodeNum <= NumNodesCtrld; ++CtrldNodeNum ) {
					SZOneStageHeatingSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrldNodeNum ) = NodeNums( CtrldNodeNum );
				}
			} else {
				ErrorsFound = true;
			}

			AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs + NumSZRhSetPtMgrs + NumSZHtSetPtMgrs + NumSZClSetPtMgrs + NumSZMinHumSetPtMgrs + NumSZMaxHumSetPtMgrs + NumMixedAirSetPtMgrs + NumOAPretreatSetPtMgrs + NumWarmestSetPtMgrs + NumColdestSetPtMgrs + NumWarmestSetPtMgrsTempFlow + NumRABFlowSetPtMgrs + NumMZClgAverageSetPtMgrs + NumMZHtgAverageSetPtMgrs + NumMZAverageMinHumSetPtMgrs + NumMZAverageMaxHumSetPtMgrs + NumMZMinHumSetPtMgrs + NumMZMaxHumSetPtMgrs + NumFollowOATempSetPtMgrs + NumFollowSysNodeTempSetPtMgrs + NumGroundTempSetPtMgrs + NumCondEntSetPtMgrs + NumIdealCondEntSetPtMgrs + NumSZOneStageCoolingSetPtMgrs;
			if ( ! NodeListError ) {
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes.allocate( NumNodesCtrld );
				AllSetPtMgr( AllSetPtMgrNum ).CtrlNodes = SZOneStageHeatingSetPtMgr( SetPtMgrNum ).CtrlNodes;
			}
			AllSetPtMgr( AllSetPtMgrNum ).Name = SZOneStageHeatingSetPtMgr( SetPtMgrNum ).Name;
			AllSetPtMgr( AllSetPtMgrNum ).SPMType = iSPMType_SZOneStageHeating;
			AllSetPtMgr( AllSetPtMgrNum ).CtrlTypeMode = SZOneStageHeatingSetPtMgr( SetPtMgrNum ).CtrlTypeMode;
			AllSetPtMgr( AllSetPtMgrNum ).NumCtrlNodes = SZOneStageHeatingSetPtMgr( SetPtMgrNum ).NumCtrlNodes;

		}

		cAlphaFieldNames.deallocate();
		cAlphaArgs.deallocate();
		lAlphaFieldBlanks.deallocate();
		cNumericFieldNames.deallocate();
		rNumericArgs.deallocate();
		lNumericFieldBlanks.deallocate();

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in input.  Program terminates." );
		}

	}

	void
	VerifySetPointManagers( bool & ErrorsFound ) // flag to denote node conflicts in input. !unused1208
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   July 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE
		// Check the SetPointManager data to eliminate conflicts.

		// METHODOLOGY EMPLOYED:
		// 1) Check for duplicate names in individual setpoint managers.
		// Control nodes = A B C D
		// Check A with B, C, and D
		// Check B with C and D
		// Check C with D
		// 2) Check for duplicate names in all other setpoint managers
		//    Verify setpoint managers use same control type (e.g. TEMP) and then check for duplicate nodes
		// SPM 1 - Control nodes A - D, SPM 2 - Control nodes E - H, SPM 3 - Control nodes I - L
		// If SPM 1 has same control type as SPM 2 and SPM 3 (e.g. all use SPM%CtrlTypeMode = iCtrlVarType_Temp) then:
		// Check A with E-H and I-L
		// Check B with E-H and I-L
		// Check C with E-H and I-L
		// Check D with E-H and I-L
		// Then check SPM 2 nodes with SPM 3. Check E with I-L, F with I-L, etc.
		// 3) For SET POINT MANAGER:RETURN AIR BYPASS FLOW
		//    check for duplicate air loop names.
		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::SameString;

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int SetPtMgrNum; // Setpoint Manager index
		int TempSetPtMgrNum; // Setpoint Manager index for warning messages
		int CtrldNodeNum; // index of the items in the controlled node node list
		int TempCtrldNodeNum; // index of the items in the controlled node node list, used for warning messages

		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumAllSetPtMgrs; ++SetPtMgrNum ) {

			// check for duplicate nodes in each setpoint managers control node list (node lists of size 1 do not need verification)
			// issue warning only since duplicate node names within a setpoint manager does not cause a conflict (i.e., same
			// value written to node) but may indicate an error in the node name.
			for ( CtrldNodeNum = 1; CtrldNodeNum <= AllSetPtMgr( SetPtMgrNum ).NumCtrlNodes - 1; ++CtrldNodeNum ) {
				for ( TempCtrldNodeNum = CtrldNodeNum + 1; TempCtrldNodeNum <= AllSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++TempCtrldNodeNum ) {
					if ( AllSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrldNodeNum ) != AllSetPtMgr( SetPtMgrNum ).CtrlNodes( TempCtrldNodeNum ) ) continue;
					ShowWarningError( cValidSPMTypes( AllSetPtMgr( SetPtMgrNum ).SPMType ) + "=\"" + AllSetPtMgr( SetPtMgrNum ).Name + "\"" );
					ShowContinueError( "...duplicate node specified = " + NodeID( AllSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrldNodeNum ) ) );
					ShowContinueError( "...control type variable    = " + cValidCtrlTypes( AllSetPtMgr( SetPtMgrNum ).CtrlTypeMode ) );
				}
			}

			// check for node conflicts in all other setpoint managers
			for ( TempSetPtMgrNum = SetPtMgrNum + 1; TempSetPtMgrNum <= NumAllSetPtMgrs; ++TempSetPtMgrNum ) {

				//   check the air loop name in addition to the node names for these SP manager types
				//    IF((AllSetPtMgr(SetPtMgrNum)%SPMType == iSPMType_WarmestTempFlow .AND. &
				//        AllSetPtMgr(TempSetPtMgrNum)%SPMType == iSPMType_WarmestTempFlow) .OR. &
				//       (AllSetPtMgr(SetPtMgrNum)%SPMType == iSPMType_RAB .AND. &
				//        AllSetPtMgr(TempSetPtMgrNum)%SPMType == iSPMType_RAB) .OR. &
				//       (AllSetPtMgr(SetPtMgrNum)%SPMType == iSPMType_Coldest .AND. &
				//        AllSetPtMgr(TempSetPtMgrNum)%SPMType == iSPMType_Coldest) .OR. &
				//       (AllSetPtMgr(SetPtMgrNum)%SPMType == iSPMType_Warmest .AND. &
				//        AllSetPtMgr(TempSetPtMgrNum)%SPMType == iSPMType_Warmest))THEN
				if ( ( AllSetPtMgr( SetPtMgrNum ).SPMType == iSPMType_RAB && AllSetPtMgr( TempSetPtMgrNum ).SPMType == iSPMType_RAB ) ) {

					//     check the air loop name for duplicates in this SP manager type
					if ( AllSetPtMgr( SetPtMgrNum ).AirLoopNum == AllSetPtMgr( TempSetPtMgrNum ).AirLoopNum ) {
						ShowWarningError( cValidSPMTypes( AllSetPtMgr( SetPtMgrNum ).SPMType ) + "=\"" + AllSetPtMgr( SetPtMgrNum ).Name + "\"" );
						ShowContinueError( "...air loop name conflicts with another setpoint manager." );
						ShowContinueError( "...conflicting setpoint manager = " + cValidSPMTypes( AllSetPtMgr( TempSetPtMgrNum ).SPMType ) + " \"" + AllSetPtMgr( TempSetPtMgrNum ).Name + "\"" );
						ShowContinueError( "...conflicting air loop name = " + AllSetPtMgr( SetPtMgrNum ).AirLoopName );
						//        ErrorsFound=.TRUE.
					}

					//     check for duplicate control nodes
					if ( AllSetPtMgr( SetPtMgrNum ).CtrlTypeMode != AllSetPtMgr( TempSetPtMgrNum ).CtrlTypeMode ) continue;

					for ( CtrldNodeNum = 1; CtrldNodeNum <= AllSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrldNodeNum ) {
						for ( TempCtrldNodeNum = 1; TempCtrldNodeNum <= AllSetPtMgr( TempSetPtMgrNum ).NumCtrlNodes; ++TempCtrldNodeNum ) {
							if ( ( AllSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrldNodeNum ) == AllSetPtMgr( TempSetPtMgrNum ).CtrlNodes( TempCtrldNodeNum ) ) && AllSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrldNodeNum ) != 0 ) {
								ShowWarningError( cValidSPMTypes( AllSetPtMgr( SetPtMgrNum ).SPMType ) + "=\"" + AllSetPtMgr( SetPtMgrNum ).Name + "\"" );
								ShowContinueError( "...setpoint node conflicts with another setpoint manager." );
								ShowContinueError( "...conflicting setpoint manager = " + cValidSPMTypes( AllSetPtMgr( TempSetPtMgrNum ).SPMType ) + " \"" + AllSetPtMgr( TempSetPtMgrNum ).Name + "\"" );
								ShowContinueError( "...conflicting node name = " + NodeID( AllSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrldNodeNum ) ) );
								ShowContinueError( "...control type variable = " + cValidCtrlTypes( AllSetPtMgr( SetPtMgrNum ).CtrlTypeMode ) );
								//            ErrorsFound=.TRUE.
							}
						}
					}

				} else { // not a RAB setpoint manager

					//     check just the control nodes for other types of SP managers
					if ( AllSetPtMgr( SetPtMgrNum ).CtrlTypeMode != AllSetPtMgr( TempSetPtMgrNum ).CtrlTypeMode ) continue;

					for ( CtrldNodeNum = 1; CtrldNodeNum <= AllSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrldNodeNum ) {
						for ( TempCtrldNodeNum = 1; TempCtrldNodeNum <= AllSetPtMgr( TempSetPtMgrNum ).NumCtrlNodes; ++TempCtrldNodeNum ) {

							if ( AllSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrldNodeNum ) != AllSetPtMgr( TempSetPtMgrNum ).CtrlNodes( TempCtrldNodeNum ) ) continue;

							//         only warn if scheduled setpoint manager is setting mass flow rate on the same node used by RAB
							if ( AllSetPtMgr( SetPtMgrNum ).SPMType == iSPMType_RAB || AllSetPtMgr( TempSetPtMgrNum ).SPMType == iSPMType_RAB ) {
								ShowWarningError( cValidSPMTypes( AllSetPtMgr( SetPtMgrNum ).SPMType ) + "=\"" + AllSetPtMgr( SetPtMgrNum ).Name + "\"" );
								ShowContinueError( "...setpoint node conflicts with another setpoint manager." );
								ShowContinueError( "...conflicting setpoint manager =" + cValidSPMTypes( AllSetPtMgr( TempSetPtMgrNum ).SPMType ) + ":\"" + AllSetPtMgr( TempSetPtMgrNum ).Name + "\"" );
								ShowContinueError( "...conflicting node name = " + NodeID( AllSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrldNodeNum ) ) );
								ShowContinueError( "...control type variable = " + cValidCtrlTypes( AllSetPtMgr( SetPtMgrNum ).CtrlTypeMode ) );
								ShowContinueError( "...return air bypass flow setpoint manager will have priority setting mass flow rate" " on this node." );
							} else { // severe error for other SP manager types
								ShowWarningError( cValidSPMTypes( AllSetPtMgr( SetPtMgrNum ).SPMType ) + "=\"" + AllSetPtMgr( SetPtMgrNum ).Name + "\"" );
								ShowContinueError( "...setpoint node conflicts with another setpoint manager." );
								ShowContinueError( "...conflicting setpoint manager = " + cValidSPMTypes( AllSetPtMgr( TempSetPtMgrNum ).SPMType ) + ":\"" + AllSetPtMgr( TempSetPtMgrNum ).Name + "\"" );
								ShowContinueError( "...conflicting node name = " + NodeID( AllSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrldNodeNum ) ) );
								ShowContinueError( "...control type variable = " + cValidCtrlTypes( AllSetPtMgr( SetPtMgrNum ).CtrlTypeMode ) );
								//            ErrorsFound=.TRUE.
							}
						}
					}
				}

			} // DO TempSetPtMgrNum = SetPtMgrNum+1, AllSetPtMgrs

		} // DO SetPtMgrNum = 1, AllSetPtMgrs

		if ( allocated( AllSetPtMgr ) ) AllSetPtMgr.deallocate();

	}

	void
	InitSetPointManagers()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   October 2000
		//       MODIFIED       Shirey/Raustad (FSEC), Jan 2004
		//                      Nov 2004 - Jan 2005 M. J. Witte, GARD Analytics, Inc.
		//                        Add new setpoint managers:
		//                          SET POINT MANAGER:SINGLE ZONE HEATING and
		//                          SET POINT MANAGER:SINGLE ZONE COOLING
		//                          SET POINT MANAGER:OUTSIDE AIR PRETREAT
		//                        Work supported by ASHRAE research project 1254-RP
		//                      Haves Oct 2004
		//                      July 2010 B.A. Nigusse, FSEC/UCF
		//                        Added new setpoint managers:
		//                          SetpointManager:MultiZone:Heating:Average
		//                          SetpointManager:MultiZone:Cooling:Average
		//                          SetpointManager:MultiZone:MinimumHumidity:Average
		//                          SetpointManager:MultiZone:MaximumHumidity:Average
		//                      Aug 2010 B.A. Nigusse, FSEC/UCF
		//                        Added new setpoint managers:
		//                          SetpointManager:MultiZone:Humidity:Minimum
		//                          SetpointManager:MultiZone:Humidity:Maximum
		//                      Sep 2010 B.A. Nigusse, FSEC/UCF
		//                         Added control varibles for SetpointManage:Scheduled
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for initializations of the Setpoint Manager objects.

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger initializations.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataZoneEquipment::ZoneEquipConfig;
		using DataZoneEquipment::ZoneEquipInputsFilled;
		using DataZoneControls::HumidityControlZone;
		using DataZoneControls::NumHumidityControlZones;
		using InputProcessor::FindItemInList;
		using DataAirSystems::PrimaryAirSystem;
		using DataHeatBalance::Zone;
		using DataHVACGlobals::NumPlantLoops;
		using DataHVACGlobals::NumCondLoops;
		using namespace DataPlant;
		using InputProcessor::SameString;
		using DataEnvironment::GroundTemp_Deep;
		using DataEnvironment::GroundTemp;
		using DataEnvironment::GroundTemp_Surface;
		using DataEnvironment::GroundTempFC;
		using OutAirNodeManager::CheckOutAirNodeNumber;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// NA

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool MyOneTimeFlag( true );
		static bool MyEnvrnFlag( true ); // flag for init once at start of environment
		static bool MyOneTimeFlag2( true );

		int SetZoneNum;
		int ControlledZoneNum;
		int ZoneNode;
		int ZoneInletNode;
		int SetPtMgrNum;
		int ZoneIndex;
		int CtrlNodeIndex;
		int NodeNum;
		int AirLoopNum;
		int LoopNum;
		int LoopNum2;
		static bool ErrorsFound( false );
		int ConZoneNum;
		int MixedAirNode;
		int BranchNum;
		int BranchNum2;
		int InletBranchNum;
		int CompNum;
		int CompNum2;
		static bool LookForFan( false );
		std::string CompType;
		std::string cSetPointManagerType;
		int FanNodeIn;
		int FanNodeOut;
		int LoopInNode;
		int HStatZoneNum;
		bool HstatZoneFound;
		int ZonesCooledIndex; // Cooled zones index in an air loop
		int TotalBranches;
		int TotalComponents;
		int BranchNumPlantSide;
		int CompNumPlantSide;
		int VarNum;
		//INTEGER  :: ChillerIndexPlantSide    = 0
		//INTEGER  :: ChillerIndexDemandSide   = 0
		//INTEGER  :: BranchIndexPlantSide     = 0
		//INTEGER  :: BranchIndexDemandSide    = 0
		//INTEGER  :: LoopIndexPlantSide       = 0
		//INTEGER  :: LoopIndexDemandSide      = 0
		static int TypeNum( 0 );
		static int TowerNum( 0 );
		static int CondLoopNum( 0 );
		static int CondBranchNum( 0 );
		static int NumChiller( 0 );
		static int NumCT( 0 );
		static int TypeOf_Num( 0 );

		ManagerOn = true;

		// One time initializations

		if ( ZoneEquipInputsFilled && AirLoopInputsFilled ) { // check that the zone equipment and air loop data has been read in

			if ( MyOneTimeFlag ) {

				// Minimum humidity setpoint managers
				cSetPointManagerType = cValidSPMTypes( iSPMType_SZMinHum );
				for ( SetPtMgrNum = 1; SetPtMgrNum <= NumSZMinHumSetPtMgrs; ++SetPtMgrNum ) {
					for ( SetZoneNum = 1; SetZoneNum <= SZMinHumSetPtMgr( SetPtMgrNum ).NumZones; ++SetZoneNum ) {
						// set the actual and controlled zone numbers
						for ( ControlledZoneNum = 1; ControlledZoneNum <= NumOfZones; ++ControlledZoneNum ) {
							if ( ZoneEquipConfig( ControlledZoneNum ).ZoneNode == SZMinHumSetPtMgr( SetPtMgrNum ).ZoneNodes( SetZoneNum ) ) {
								SZMinHumSetPtMgr( SetPtMgrNum ).CtrlZoneNum( SetZoneNum ) = ControlledZoneNum;
								SZMinHumSetPtMgr( SetPtMgrNum ).ZoneNum( SetZoneNum ) = ZoneEquipConfig( ControlledZoneNum ).ActualZoneNum;
								break;
							}
						}
						// still need to validate...
						if ( SZMinHumSetPtMgr( SetPtMgrNum ).CtrlZoneNum( SetZoneNum ) == 0 ) { // didn't find
							ShowSevereError( cSetPointManagerType + "=\"" + SZMinHumSetPtMgr( SetPtMgrNum ).Name + "\", invalid zone" );
							ShowContinueError( "could not find Controlled Zone=" + Zone( SZMinHumSetPtMgr( SetPtMgrNum ).ZoneNum( SetZoneNum ) ).Name );
							ErrorsFound = true;
						} else {
							// make sure humidity controlled zone
							HstatZoneFound = false;
							for ( HStatZoneNum = 1; HStatZoneNum <= NumHumidityControlZones; ++HStatZoneNum ) {
								if ( HumidityControlZone( HStatZoneNum ).ActualZoneNum != SZMinHumSetPtMgr( SetPtMgrNum ).ZoneNum( SetZoneNum ) ) continue;
								HstatZoneFound = true;
								break;
							}
							if ( ! HstatZoneFound ) {
								ShowSevereError( cSetPointManagerType + "=\"" + SZMinHumSetPtMgr( SetPtMgrNum ).Name + "\", invalid humidistat specification" );
								ShowContinueError( "could not locate Humidistat in Zone=" + Zone( SZMinHumSetPtMgr( SetPtMgrNum ).ZoneNum( SetZoneNum ) ).Name );
								ErrorsFound = true;
							}
						}
					}
				}

				// Maximum humidity setpoint managers
				cSetPointManagerType = cValidSPMTypes( iSPMType_SZMaxHum );
				for ( SetPtMgrNum = 1; SetPtMgrNum <= NumSZMaxHumSetPtMgrs; ++SetPtMgrNum ) {
					for ( SetZoneNum = 1; SetZoneNum <= SZMaxHumSetPtMgr( SetPtMgrNum ).NumZones; ++SetZoneNum ) {
						// set the actual and controlled zone numbers
						for ( ControlledZoneNum = 1; ControlledZoneNum <= NumOfZones; ++ControlledZoneNum ) {
							if ( ZoneEquipConfig( ControlledZoneNum ).ZoneNode == SZMaxHumSetPtMgr( SetPtMgrNum ).ZoneNodes( SetZoneNum ) ) {
								SZMaxHumSetPtMgr( SetPtMgrNum ).CtrlZoneNum( SetZoneNum ) = ControlledZoneNum;
								SZMaxHumSetPtMgr( SetPtMgrNum ).ZoneNum( SetZoneNum ) = ZoneEquipConfig( ControlledZoneNum ).ActualZoneNum;
								break;
							}
						}
						// still need to validate...
						if ( SZMaxHumSetPtMgr( SetPtMgrNum ).CtrlZoneNum( SetZoneNum ) == 0 ) { // didn't find
							ShowSevereError( cSetPointManagerType + "=\"" + SZMaxHumSetPtMgr( SetPtMgrNum ).Name + "\", invalid zone" );
							ShowContinueError( "could not find Controlled Zone=" + Zone( SZMaxHumSetPtMgr( SetPtMgrNum ).ZoneNum( SetZoneNum ) ).Name );
							ErrorsFound = true;
						} else {
							// make sure humidity controlled zone
							HstatZoneFound = false;
							for ( HStatZoneNum = 1; HStatZoneNum <= NumHumidityControlZones; ++HStatZoneNum ) {
								if ( HumidityControlZone( HStatZoneNum ).ActualZoneNum != SZMaxHumSetPtMgr( SetPtMgrNum ).ZoneNum( SetZoneNum ) ) continue;
								HstatZoneFound = true;
								break;
							}
							if ( ! HstatZoneFound ) {
								ShowSevereError( cSetPointManagerType + "=\"" + SZMaxHumSetPtMgr( SetPtMgrNum ).Name + "\", invalid humidistat specification" );
								ShowContinueError( "could not locate Humidistat in Zone=" + Zone( SZMaxHumSetPtMgr( SetPtMgrNum ).ZoneNum( SetZoneNum ) ).Name );
								ErrorsFound = true;
							}
						}
					}
				}

				// single zone reheat setpoint manager
				cSetPointManagerType = cValidSPMTypes( iSPMType_SZReheat );
				for ( SetPtMgrNum = 1; SetPtMgrNum <= NumSZRhSetPtMgrs; ++SetPtMgrNum ) {
					FanNodeIn = 0;
					FanNodeOut = 0;
					MixedAirNode = 0;
					AirLoopNum = 0;
					InletBranchNum = 0;
					LoopInNode = 0;
					LookForFan = false;
					ZoneInletNode = SingZoneRhSetPtMgr( SetPtMgrNum ).ZoneInletNodeNum;
					ZoneNode = SingZoneRhSetPtMgr( SetPtMgrNum ).ZoneNodeNum;
					// find the index in the ZoneEquipConfig array of the control zone (the one with the main or only thermostat)
					ConZoneNum = 0;
					for ( ControlledZoneNum = 1; ControlledZoneNum <= NumOfZones; ++ControlledZoneNum ) {
						if ( ZoneEquipConfig( ControlledZoneNum ).ZoneNode == ZoneNode ) {
							ConZoneNum = ControlledZoneNum;
						}
					}
					if ( ConZoneNum == 0 ) {
						ShowSevereError( cSetPointManagerType + "=\"" + SingZoneRhSetPtMgr( SetPtMgrNum ).Name + "\", Zone Node not found:" );
						ShowContinueError( "Node=\"" + NodeID( SingZoneRhSetPtMgr( SetPtMgrNum ).ZoneNodeNum ) + "\", not found in any controlled Zone" );
						ErrorsFound = true;
					} else {
						AirLoopNum = ZoneEquipConfig( ConZoneNum ).AirLoopNum;
						if ( AirLoopNum == 0 ) {
							ShowSevereError( cSetPointManagerType + "=\"" + SingZoneRhSetPtMgr( SetPtMgrNum ).Name + "\", Zone not on air loop:" );
							ShowContinueError( "Controlled Zone not on air loop, Zone=" + ZoneEquipConfig( ConZoneNum ).ZoneName );
							ErrorsFound = true;
							continue;
						}
						MixedAirNode = PrimaryAirSystem( AirLoopNum ).OASysOutletNodeNum;
						InletBranchNum = PrimaryAirSystem( AirLoopNum ).InletBranchNum( 1 );
						LoopInNode = PrimaryAirSystem( AirLoopNum ).Branch( InletBranchNum ).NodeNumIn;
						// get the supply fan inlet and outlet nodes
						if ( MixedAirNode > 0 ) {
							for ( BranchNum = 1; BranchNum <= PrimaryAirSystem( AirLoopNum ).NumBranches; ++BranchNum ) {
								for ( CompNum = 1; CompNum <= PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).TotalComponents; ++CompNum ) {
									CompType = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).TypeOf;
									if ( MixedAirNode == PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).NodeNumIn ) {
										LookForFan = true;
									}
									if ( LookForFan ) {
										//cpw22Aug2010 Add Fan:ComponentModel (new)
										if ( SameString( CompType, "Fan:ConstantVolume" ) || SameString( CompType, "Fan:VariableVolume" ) || SameString( CompType, "Fan:OnOff" ) || SameString( CompType, "Fan:ComponentModel" ) ) {
											FanNodeIn = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).NodeNumIn;
											FanNodeOut = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).NodeNumOut;
											break;
										}
									}
								}
							}
						} else {
							for ( BranchNum = 1; BranchNum <= PrimaryAirSystem( AirLoopNum ).NumBranches; ++BranchNum ) {
								for ( CompNum = 1; CompNum <= PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).TotalComponents; ++CompNum ) {
									CompType = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).TypeOf;
									//cpw22Aug2010 Add Fan:ComponentModel (new)
									if ( SameString( CompType, "Fan:ConstantVolume" ) || SameString( CompType, "Fan:VariableVolume" ) || SameString( CompType, "Fan:OnOff" ) || SameString( CompType, "Fan:ComponentModel" ) ) {
										FanNodeIn = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).NodeNumIn;
										FanNodeOut = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).NodeNumOut;
									}
								}
							}
						}
						SingZoneRhSetPtMgr( SetPtMgrNum ).FanNodeIn = FanNodeIn;
						SingZoneRhSetPtMgr( SetPtMgrNum ).FanNodeOut = FanNodeOut;
						SingZoneRhSetPtMgr( SetPtMgrNum ).MixedAirNode = MixedAirNode;
						SingZoneRhSetPtMgr( SetPtMgrNum ).AirLoopNum = AirLoopNum;
						SingZoneRhSetPtMgr( SetPtMgrNum ).OAInNode = PrimaryAirSystem( AirLoopNum ).OAMixOAInNodeNum;
						SingZoneRhSetPtMgr( SetPtMgrNum ).RetNode = PrimaryAirSystem( AirLoopNum ).OASysInletNodeNum;
						SingZoneRhSetPtMgr( SetPtMgrNum ).OAInNode = PrimaryAirSystem( AirLoopNum ).OAMixOAInNodeNum;
						SingZoneRhSetPtMgr( SetPtMgrNum ).LoopInNode = LoopInNode;
					}
				}

				// Warmest Setpoint Managers
				cSetPointManagerType = cValidSPMTypes( iSPMType_Warmest );
				for ( SetPtMgrNum = 1; SetPtMgrNum <= NumWarmestSetPtMgrs; ++SetPtMgrNum ) {
					if ( NumPrimaryAirSys > 0 ) {
						AirLoopNum = FindItemInList( WarmestSetPtMgr( SetPtMgrNum ).AirLoopName, AirToZoneNodeInfo.AirLoopName(), NumPrimaryAirSys );
						if ( AirLoopNum == 0 ) {
							ShowSevereError( cSetPointManagerType + "=\"" + WarmestSetPtMgr( SetPtMgrNum ).Name + "\", invalid Air Loop specified:" );
							ShowContinueError( "Air Loop not found =\"" + WarmestSetPtMgr( SetPtMgrNum ).AirLoopName + "\"." );
							ErrorsFound = true;
						} else {
							WarmestSetPtMgr( SetPtMgrNum ).AirLoopNum = AirLoopNum;
						}
						if ( AirToZoneNodeInfo( AirLoopNum ).NumZonesCooled == 0 ) {
							ShowSevereError( cSetPointManagerType + "=\"" + WarmestSetPtMgr( SetPtMgrNum ).Name + "\", no zones with cooling found:" );
							ShowContinueError( "Air Loop provides no cooling, Air Loop=\"" + WarmestSetPtMgr( SetPtMgrNum ).AirLoopName + "\"." );
							ErrorsFound = true;
						}
					} else {
						ShowSevereError( cSetPointManagerType + "=\"" + WarmestSetPtMgr( SetPtMgrNum ).Name + "\", no AirLoopHVAC objects found:" );
						ShowContinueError( "Setpoint Manager needs an AirLoopHVAC to operate." );
						ErrorsFound = true;
					}
				}

				// Coldest Setpoint Managers
				cSetPointManagerType = cValidSPMTypes( iSPMType_Coldest );
				for ( SetPtMgrNum = 1; SetPtMgrNum <= NumColdestSetPtMgrs; ++SetPtMgrNum ) {
					if ( NumPrimaryAirSys > 0 ) {
						AirLoopNum = FindItemInList( ColdestSetPtMgr( SetPtMgrNum ).AirLoopName, AirToZoneNodeInfo.AirLoopName(), NumPrimaryAirSys );
						if ( AirLoopNum == 0 ) {
							ShowSevereError( cSetPointManagerType + "=\"" + ColdestSetPtMgr( SetPtMgrNum ).Name + "\", invalid Air Loop specified:" );
							ShowContinueError( "Air Loop not found =\"" + ColdestSetPtMgr( SetPtMgrNum ).AirLoopName + "\"." );
							ErrorsFound = true;
						} else {
							ColdestSetPtMgr( SetPtMgrNum ).AirLoopNum = AirLoopNum;
						}
						if ( AirToZoneNodeInfo( AirLoopNum ).NumZonesHeated == 0 ) {
							ShowSevereError( cSetPointManagerType + "=\"" + ColdestSetPtMgr( SetPtMgrNum ).Name + "\", no zones with heating found:" );
							ShowContinueError( "Air Loop provides no heating, Air Loop=\"" + ColdestSetPtMgr( SetPtMgrNum ).AirLoopName + "\"." );
							ErrorsFound = true;
						}
					} else {
						ShowSevereError( cSetPointManagerType + "=\"" + ColdestSetPtMgr( SetPtMgrNum ).Name + "\", no AirLoopHVAC objects found:" );
						ShowContinueError( "Setpoint Manager needs an AirLoopHVAC to operate." );
						ErrorsFound = true;
					}
				}

				// Warmest Temp Flow Setpoint Managers
				cSetPointManagerType = cValidSPMTypes( iSPMType_WarmestTempFlow );
				for ( SetPtMgrNum = 1; SetPtMgrNum <= NumWarmestSetPtMgrsTempFlow; ++SetPtMgrNum ) {
					if ( NumPrimaryAirSys > 0 ) {
						AirLoopNum = FindItemInList( WarmestSetPtMgrTempFlow( SetPtMgrNum ).AirLoopName, AirToZoneNodeInfo.AirLoopName(), NumPrimaryAirSys );
						if ( AirLoopNum == 0 ) {
							ShowSevereError( cSetPointManagerType + "=\"" + WarmestSetPtMgrTempFlow( SetPtMgrNum ).Name + "\", invalid Air Loop specified:" );
							ShowContinueError( "Air Loop not found =\"" + WarmestSetPtMgrTempFlow( SetPtMgrNum ).AirLoopName + "\"." );
							ErrorsFound = true;
						} else {
							WarmestSetPtMgrTempFlow( SetPtMgrNum ).AirLoopNum = AirLoopNum;
							WarmestSetPtMgrTempFlow( SetPtMgrNum ).SimReady = true;
						}
						if ( AirToZoneNodeInfo( AirLoopNum ).NumZonesCooled == 0 ) {
							ShowSevereError( cSetPointManagerType + "=\"" + WarmestSetPtMgrTempFlow( SetPtMgrNum ).Name + "\", no zones with cooling found:" );
							ShowContinueError( "Air Loop provides no cooling, Air Loop=\"" + WarmestSetPtMgrTempFlow( SetPtMgrNum ).AirLoopName + "\"." );
							ErrorsFound = true;
						}
					} else {
						ShowSevereError( cSetPointManagerType + "=\"" + WarmestSetPtMgrTempFlow( SetPtMgrNum ).Name + "\", no AirLoopHVAC objects found:" );
						ShowContinueError( "Setpoint Manager needs an AirLoopHVAC to operate." );
						ErrorsFound = true;
					}
				}

				// return air bypass flow set manager
				cSetPointManagerType = cValidSPMTypes( iSPMType_RAB );
				for ( SetPtMgrNum = 1; SetPtMgrNum <= NumRABFlowSetPtMgrs; ++SetPtMgrNum ) {
					if ( NumPrimaryAirSys > 0 ) {
						AirLoopNum = FindItemInList( RABFlowSetPtMgr( SetPtMgrNum ).AirLoopName, AirToZoneNodeInfo.AirLoopName(), NumPrimaryAirSys );
						AllSetPtMgr( RABFlowSetPtMgr( SetPtMgrNum ).AllSetPtMgrIndex ).AirLoopNum = AirLoopNum;
						AllSetPtMgr( RABFlowSetPtMgr( SetPtMgrNum ).AllSetPtMgrIndex ).AirLoopName = RABFlowSetPtMgr( SetPtMgrNum ).AirLoopName;
						if ( AirLoopNum == 0 ) {
							ShowSevereError( cSetPointManagerType + "=\"" + RABFlowSetPtMgr( SetPtMgrNum ).Name + "\", invalid Air Loop specified:" );
							ShowContinueError( "Air Loop not found =\"" + RABFlowSetPtMgr( SetPtMgrNum ).AirLoopName + "\"." );
							ErrorsFound = true;
						} else {
							RABFlowSetPtMgr( SetPtMgrNum ).AirLoopNum = AirLoopNum;
							if ( PrimaryAirSystem( AirLoopNum ).RABExists ) {
								RABFlowSetPtMgr( SetPtMgrNum ).RABMixInNode = PrimaryAirSystem( AirLoopNum ).RABMixInNode;
								RABFlowSetPtMgr( SetPtMgrNum ).SupMixInNode = PrimaryAirSystem( AirLoopNum ).SupMixInNode;
								RABFlowSetPtMgr( SetPtMgrNum ).MixOutNode = PrimaryAirSystem( AirLoopNum ).MixOutNode;
								RABFlowSetPtMgr( SetPtMgrNum ).RABSplitOutNode = PrimaryAirSystem( AirLoopNum ).RABSplitOutNode;
								RABFlowSetPtMgr( SetPtMgrNum ).SysOutNode = AirToZoneNodeInfo( AirLoopNum ).AirLoopSupplyNodeNum( 1 );
								RABFlowSetPtMgr( SetPtMgrNum ).CtrlNodes( 1 ) = RABFlowSetPtMgr( SetPtMgrNum ).RABSplitOutNode;
								AllSetPtMgr( RABFlowSetPtMgr( SetPtMgrNum ).AllSetPtMgrIndex ).CtrlNodes( 1 ) = RABFlowSetPtMgr( SetPtMgrNum ).RABSplitOutNode;
							} else {
								ShowSevereError( cSetPointManagerType + "=\"" + RABFlowSetPtMgr( SetPtMgrNum ).Name + "\", no RAB in air loop found:" );
								ShowContinueError( "Air Loop=\"" + RABFlowSetPtMgr( SetPtMgrNum ).AirLoopName + "\"." );
								ErrorsFound = true;
							}
						}
					} else {
						ShowSevereError( cSetPointManagerType + "=\"" + RABFlowSetPtMgr( SetPtMgrNum ).Name + "\", no AirLoopHVAC objects found:" );
						ShowContinueError( "Setpoint Manager needs an AirLoopHVAC to operate." );
						ErrorsFound = true;
					}
				}

				// MultiZone Average Cooling Setpoint Managers
				cSetPointManagerType = cValidSPMTypes( iSPMType_MZCoolingAverage );
				for ( SetPtMgrNum = 1; SetPtMgrNum <= NumMZClgAverageSetPtMgrs; ++SetPtMgrNum ) {
					if ( NumPrimaryAirSys > 0 ) {
						AirLoopNum = FindItemInList( MZAverageCoolingSetPtMgr( SetPtMgrNum ).AirLoopName, AirToZoneNodeInfo.AirLoopName(), NumPrimaryAirSys );
						if ( AirLoopNum == 0 ) {
							ShowSevereError( cSetPointManagerType + "=\"" + MZAverageCoolingSetPtMgr( SetPtMgrNum ).Name + "\", invalid Air Loop specified:" );
							ShowContinueError( "Air Loop not found =\"" + MZAverageCoolingSetPtMgr( SetPtMgrNum ).AirLoopName + "\"." );
							ErrorsFound = true;
						} else {
							MZAverageCoolingSetPtMgr( SetPtMgrNum ).AirLoopNum = AirLoopNum;
						}
						if ( AirToZoneNodeInfo( AirLoopNum ).NumZonesCooled == 0 ) {
							ShowSevereError( cSetPointManagerType + "=\"" + MZAverageCoolingSetPtMgr( SetPtMgrNum ).Name + "\", no zones with cooling found:" );
							ShowContinueError( "Air Loop provides no cooling, Air Loop=\"" + MZAverageCoolingSetPtMgr( SetPtMgrNum ).AirLoopName + "\"." );
							ErrorsFound = true;
						}
					} else {
						ShowSevereError( cSetPointManagerType + "=\"" + MZAverageCoolingSetPtMgr( SetPtMgrNum ).Name + "\", no AirLoopHVAC objects found:" );
						ShowContinueError( "Setpoint Manager needs an AirLoopHVAC to operate." );
						ErrorsFound = true;
					}
				}

				// MultiZone Average Heating Setpoint Managers
				cSetPointManagerType = cValidSPMTypes( iSPMType_MZHeatingAverage );
				for ( SetPtMgrNum = 1; SetPtMgrNum <= NumMZHtgAverageSetPtMgrs; ++SetPtMgrNum ) {
					if ( NumPrimaryAirSys > 0 ) {
						AirLoopNum = FindItemInList( MZAverageHeatingSetPtMgr( SetPtMgrNum ).AirLoopName, AirToZoneNodeInfo.AirLoopName(), NumPrimaryAirSys );
						if ( AirLoopNum == 0 ) {
							ShowSevereError( cSetPointManagerType + "=\"" + MZAverageHeatingSetPtMgr( SetPtMgrNum ).Name + "\", invalid Air Loop specified:" );
							ShowContinueError( "Air Loop not found =\"" + MZAverageHeatingSetPtMgr( SetPtMgrNum ).AirLoopName + "\"." );
							ErrorsFound = true;
						} else {
							MZAverageHeatingSetPtMgr( SetPtMgrNum ).AirLoopNum = AirLoopNum;
						}
						// Commented out as we are using %NumZonesCooled instead of %NumZonesHeated for all systems for now
						//IF (AirToZoneNodeInfo(AirLoopNum)%NumZonesHeated == 0) THEN
						//  CALL ShowSevereError(TRIM(cSetPointManagerType)//': Air Loop provides no heating ' // &
						//                       TRIM(MZAverageHeatingSetPtMgr(SetPtMgrNum)%Name))
						//  CALL ShowContinueError('Occurs in Setpoint Manager='//TRIM(MZAverageHeatingSetPtMgr(SetPtMgrNum)%Name))
						//  ErrorsFound = .TRUE.
						//END IF
					} else {
						ShowSevereError( cSetPointManagerType + "=\"" + MZAverageHeatingSetPtMgr( SetPtMgrNum ).Name + "\", no AirLoopHVAC objects found:" );
						ShowContinueError( "Setpoint Manager needs an AirLoopHVAC to operate." );
						ErrorsFound = true;
					}
				}

				// MultiZone Average Minimum Humidity Setpoint Managers
				cSetPointManagerType = cValidSPMTypes( iSPMType_MZMinHumAverage );
				for ( SetPtMgrNum = 1; SetPtMgrNum <= NumMZAverageMinHumSetPtMgrs; ++SetPtMgrNum ) {
					if ( NumPrimaryAirSys > 0 ) {
						AirLoopNum = FindItemInList( MZAverageMinHumSetPtMgr( SetPtMgrNum ).AirLoopName, AirToZoneNodeInfo.AirLoopName(), NumPrimaryAirSys );
						if ( AirLoopNum == 0 ) {
							ShowSevereError( cSetPointManagerType + "=\"" + MZAverageMinHumSetPtMgr( SetPtMgrNum ).Name + "\", invalid Air Loop specified:" );
							ShowContinueError( "Air Loop not found =\"" + MZAverageMinHumSetPtMgr( SetPtMgrNum ).AirLoopName + "\"." );
							ErrorsFound = true;
						} else {
							MZAverageMinHumSetPtMgr( SetPtMgrNum ).AirLoopNum = AirLoopNum;
							// make sure humidity controlled zone
							HstatZoneFound = false;
							for ( HStatZoneNum = 1; HStatZoneNum <= NumHumidityControlZones; ++HStatZoneNum ) {
								for ( ZonesCooledIndex = 1; ZonesCooledIndex <= AirToZoneNodeInfo( MZAverageMinHumSetPtMgr( SetPtMgrNum ).AirLoopNum ).NumZonesCooled; ++ZonesCooledIndex ) {
									if ( HumidityControlZone( HStatZoneNum ).ActualZoneNum != AirToZoneNodeInfo( MZAverageMinHumSetPtMgr( SetPtMgrNum ).AirLoopNum ).CoolCtrlZoneNums( ZonesCooledIndex ) ) continue;
									HstatZoneFound = true;
									break;
								}
							}
							if ( ! HstatZoneFound ) {
								ShowSevereError( cSetPointManagerType + "=\"" + MZAverageMinHumSetPtMgr( SetPtMgrNum ).Name + "\", invalid humidistat specification" );
								ShowContinueError( "could not locate Humidistat in any of the zones" " served by the Air loop=" + PrimaryAirSystem( AirLoopNum ).Name );
								ErrorsFound = true;
							}
						}
					} else {
						ShowSevereError( cSetPointManagerType + "=\"" + MZAverageMinHumSetPtMgr( SetPtMgrNum ).Name + "\", no AirLoopHVAC objects found:" );
						ShowContinueError( "Setpoint Manager needs an AirLoopHVAC to operate." );
						ErrorsFound = true;
					}
				}

				// MultiZone Average Maximum Humidity Setpoint Managers
				cSetPointManagerType = cValidSPMTypes( iSPMType_MZMaxHumAverage );
				for ( SetPtMgrNum = 1; SetPtMgrNum <= NumMZAverageMaxHumSetPtMgrs; ++SetPtMgrNum ) {
					if ( NumPrimaryAirSys > 0 ) {
						AirLoopNum = FindItemInList( MZAverageMaxHumSetPtMgr( SetPtMgrNum ).AirLoopName, AirToZoneNodeInfo.AirLoopName(), NumPrimaryAirSys );
						if ( AirLoopNum == 0 ) {
							ShowSevereError( cSetPointManagerType + "=\"" + MZAverageMaxHumSetPtMgr( SetPtMgrNum ).Name + "\", invalid Air Loop specified:" );
							ShowContinueError( "Air Loop not found =\"" + MZAverageMaxHumSetPtMgr( SetPtMgrNum ).AirLoopName + "\"." );
							ErrorsFound = true;
						} else {
							MZAverageMaxHumSetPtMgr( SetPtMgrNum ).AirLoopNum = AirLoopNum;
							// make sure humidity controlled zone
							HstatZoneFound = false;
							for ( HStatZoneNum = 1; HStatZoneNum <= NumHumidityControlZones; ++HStatZoneNum ) {
								for ( ZonesCooledIndex = 1; ZonesCooledIndex <= AirToZoneNodeInfo( MZAverageMaxHumSetPtMgr( SetPtMgrNum ).AirLoopNum ).NumZonesCooled; ++ZonesCooledIndex ) {
									if ( HumidityControlZone( HStatZoneNum ).ActualZoneNum != AirToZoneNodeInfo( MZAverageMaxHumSetPtMgr( SetPtMgrNum ).AirLoopNum ).CoolCtrlZoneNums( ZonesCooledIndex ) ) continue;
									HstatZoneFound = true;
									break;
								}
							}
							if ( ! HstatZoneFound ) {
								ShowSevereError( cSetPointManagerType + "=\"" + MZAverageMaxHumSetPtMgr( SetPtMgrNum ).Name + "\", invalid humidistat specification" );
								ShowContinueError( "could not locate Humidistat in any of the zones" " served by the Air loop=" + PrimaryAirSystem( AirLoopNum ).Name );
								ErrorsFound = true;
							}
						}
					} else {
						ShowSevereError( cSetPointManagerType + "=\"" + MZAverageMaxHumSetPtMgr( SetPtMgrNum ).Name + "\", no AirLoopHVAC objects found:" );
						ShowContinueError( "Setpoint Manager needs an AirLoopHVAC to operate." );
						ErrorsFound = true;
					}
				}

				// Multizone Minimum Humidity Ratio Setpoint Managers
				cSetPointManagerType = cValidSPMTypes( iSPMType_MZMinHum );
				for ( SetPtMgrNum = 1; SetPtMgrNum <= NumMZMinHumSetPtMgrs; ++SetPtMgrNum ) {
					if ( NumPrimaryAirSys > 0 ) {
						AirLoopNum = FindItemInList( MZMinHumSetPtMgr( SetPtMgrNum ).AirLoopName, AirToZoneNodeInfo.AirLoopName(), NumPrimaryAirSys );
						if ( AirLoopNum == 0 ) {
							ShowSevereError( cSetPointManagerType + "=\"" + MZMinHumSetPtMgr( SetPtMgrNum ).Name + "\", invalid Air Loop specified:" );
							ShowContinueError( "Air Loop not found =\"" + MZMinHumSetPtMgr( SetPtMgrNum ).AirLoopName + "\"." );
							ErrorsFound = true;
						} else {
							MZMinHumSetPtMgr( SetPtMgrNum ).AirLoopNum = AirLoopNum;
							// make sure humidity controlled zone
							HstatZoneFound = false;
							for ( HStatZoneNum = 1; HStatZoneNum <= NumHumidityControlZones; ++HStatZoneNum ) {
								for ( ZonesCooledIndex = 1; ZonesCooledIndex <= AirToZoneNodeInfo( MZMinHumSetPtMgr( SetPtMgrNum ).AirLoopNum ).NumZonesCooled; ++ZonesCooledIndex ) {
									if ( HumidityControlZone( HStatZoneNum ).ActualZoneNum != AirToZoneNodeInfo( MZMinHumSetPtMgr( SetPtMgrNum ).AirLoopNum ).CoolCtrlZoneNums( ZonesCooledIndex ) ) continue;
									HstatZoneFound = true;
									break;
								}
							}
							if ( ! HstatZoneFound ) {
								ShowSevereError( cSetPointManagerType + "=\"" + MZMinHumSetPtMgr( SetPtMgrNum ).Name + "\", invalid humidistat specification" );
								ShowContinueError( "could not locate Humidistat in any of the zones" " served by the Air loop=" + PrimaryAirSystem( AirLoopNum ).Name );
								ErrorsFound = true;
							}
						}
					} else {
						ShowSevereError( cSetPointManagerType + "=\"" + MZMinHumSetPtMgr( SetPtMgrNum ).Name + "\", no AirLoopHVAC objects found:" );
						ShowContinueError( "Setpoint Manager needs an AirLoopHVAC to operate." );
						ErrorsFound = true;
					}
				}

				// Multizone Maximum Humidity Ratio Setpoint Managers
				cSetPointManagerType = cValidSPMTypes( iSPMType_MZMaxHum );
				for ( SetPtMgrNum = 1; SetPtMgrNum <= NumMZMaxHumSetPtMgrs; ++SetPtMgrNum ) {
					if ( NumPrimaryAirSys > 0 ) {
						AirLoopNum = FindItemInList( MZMaxHumSetPtMgr( SetPtMgrNum ).AirLoopName, AirToZoneNodeInfo.AirLoopName(), NumPrimaryAirSys );
						if ( AirLoopNum == 0 ) {
							ShowSevereError( cSetPointManagerType + "=\"" + MZMaxHumSetPtMgr( SetPtMgrNum ).Name + "\", invalid Air Loop specified:" );
							ShowContinueError( "Air Loop not found =\"" + MZMaxHumSetPtMgr( SetPtMgrNum ).AirLoopName + "\"." );
							ErrorsFound = true;
						} else {
							MZMaxHumSetPtMgr( SetPtMgrNum ).AirLoopNum = AirLoopNum;
							// make sure humidity controlled zone
							HstatZoneFound = false;
							for ( HStatZoneNum = 1; HStatZoneNum <= NumHumidityControlZones; ++HStatZoneNum ) {
								for ( ZonesCooledIndex = 1; ZonesCooledIndex <= AirToZoneNodeInfo( MZMaxHumSetPtMgr( SetPtMgrNum ).AirLoopNum ).NumZonesCooled; ++ZonesCooledIndex ) {
									if ( HumidityControlZone( HStatZoneNum ).ActualZoneNum != AirToZoneNodeInfo( MZMaxHumSetPtMgr( SetPtMgrNum ).AirLoopNum ).CoolCtrlZoneNums( ZonesCooledIndex ) ) continue;
									HstatZoneFound = true;
									break;
								}
							}
							if ( ! HstatZoneFound ) {
								ShowSevereError( cSetPointManagerType + "=\"" + MZMaxHumSetPtMgr( SetPtMgrNum ).Name + "\", invalid humidistat specification" );
								ShowContinueError( "could not locate Humidistat in any of the zones" " served by the Air loop=" + PrimaryAirSystem( AirLoopNum ).Name );
								ErrorsFound = true;
							}
						}
					} else {
						ShowSevereError( cSetPointManagerType + "=\"" + MZMaxHumSetPtMgr( SetPtMgrNum ).Name + "\", no AirLoopHVAC objects found:" );
						ShowContinueError( "Setpoint Manager needs an AirLoopHVAC to operate." );
						ErrorsFound = true;
					}
				}

				// condenser entering water temperature reset setpoint manager
				NumCT = 0;
				cSetPointManagerType = cValidSPMTypes( iSPMType_CondEntReset );
				for ( SetPtMgrNum = 1; SetPtMgrNum <= NumCondEntSetPtMgrs; ++SetPtMgrNum ) {
					// Scan loops and find the loop index that includes the condenser cooling tower node used as setpoint
					for ( LoopNum = 1; LoopNum <= NumCondLoops + NumPlantLoops; ++LoopNum ) { // Begin demand side loops ... When condenser is added becomes NumLoops
						for ( CtrlNodeIndex = 1; CtrlNodeIndex <= CondEntSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) {
							if ( PlantLoop( LoopNum ).TempSetPointNodeNum == CondEntSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ) ) {
								for ( BranchNum = 1; BranchNum <= PlantLoop( LoopNum ).LoopSide( SupplySide ).TotalBranches; ++BranchNum ) {
									for ( CompNum = 1; CompNum <= PlantLoop( LoopNum ).LoopSide( SupplySide ).Branch( BranchNum ).TotalComponents; ++CompNum ) {
										// Check if cooling tower is single speed and generate and error
										TypeOf_Num = PlantLoop( LoopNum ).LoopSide( SupplySide ).Branch( BranchNum ).Comp( CompNum ).TypeOf_Num;
										if ( TypeOf_Num == TypeOf_CoolingTower_SingleSpd ) {
											ShowSevereError( cSetPointManagerType + "=\"" + CondEntSetPtMgr( SetPtMgrNum ).Name + "\", invalid tower found" );
											ShowContinueError( "Found SingleSpeed Cooling Tower, Cooling Tower=" + PlantLoop( LoopNum ).LoopSide( SupplySide ).Branch( BranchNum ).Comp( CompNum ).Name );
											ShowContinueError( "SingleSpeed cooling towers cannot be used with this setpoint manager on each loop" );
											ErrorsFound = true;
										}
										// Check if there are more than 1 cooling tower on the plant and generate error
										if ( TypeOf_Num == TypeOf_CoolingTower_TwoSpd || TypeOf_Num == TypeOf_CoolingTower_VarSpdMerkel || TypeOf_Num == TypeOf_CoolingTower_VarSpd ) {
											++NumCT;
											if ( NumCT > 1 ) {
												ShowSevereError( cSetPointManagerType + "=\"" + CondEntSetPtMgr( SetPtMgrNum ).Name + "\", too many towers found" );
												ShowContinueError( "only one cooling tower can be used with this setpoint manager on each loop" );
												ShowContinueError( "Found more than one cooling tower, Cooling Tower=" + PlantLoop( LoopNum ).LoopSide( SupplySide ).Branch( BranchNum ).Comp( CompNum ).Name );
												ErrorsFound = true;
											}
										}
									}
								}
								NumCT = 0;
								// Scan all attached chillers in the condenser loop index found to find the chiller index
								for ( BranchNum = 1; BranchNum <= PlantLoop( LoopNum ).LoopSide( DemandSide ).TotalBranches; ++BranchNum ) {
									for ( CompNum = 1; CompNum <= PlantLoop( LoopNum ).LoopSide( DemandSide ).Branch( BranchNum ).TotalComponents; ++CompNum ) {
										TypeOf_Num = PlantLoop( LoopNum ).LoopSide( DemandSide ).Branch( BranchNum ).Comp( CompNum ).TypeOf_Num;
										if ( TypeOf_Num == TypeOf_Chiller_Absorption || TypeOf_Num == TypeOf_Chiller_Indirect_Absorption || TypeOf_Num == TypeOf_Chiller_CombTurbine || TypeOf_Num == TypeOf_Chiller_ConstCOP || TypeOf_Num == TypeOf_Chiller_Electric || TypeOf_Num == TypeOf_Chiller_ElectricEIR || TypeOf_Num == TypeOf_Chiller_DFAbsorption || TypeOf_Num == TypeOf_Chiller_ElectricReformEIR || TypeOf_Num == TypeOf_Chiller_EngineDriven ) {

											// Scan the supply side to find the chiller index and branch index on plantloop
											TypeNum = PlantLoop( LoopNum ).LoopSide( DemandSide ).Branch( BranchNum ).Comp( CompNum ).TypeOf_Num;
											for ( LoopNum2 = 1; LoopNum2 <= NumCondLoops + NumPlantLoops; ++LoopNum2 ) {
												for ( BranchNumPlantSide = 1; BranchNumPlantSide <= PlantLoop( LoopNum2 ).LoopSide( SupplySide ).TotalBranches; ++BranchNumPlantSide ) {
													for ( CompNumPlantSide = 1; CompNumPlantSide <= PlantLoop( LoopNum2 ).LoopSide( SupplySide ).Branch( BranchNumPlantSide ).TotalComponents; ++CompNumPlantSide ) {
														if ( PlantLoop( LoopNum2 ).LoopSide( SupplySide ).Branch( BranchNumPlantSide ).Comp( CompNumPlantSide ).TypeOf_Num == TypeNum ) {
															CondEntSetPtMgr( SetPtMgrNum ).LoopIndexPlantSide = LoopNum2;
															CondEntSetPtMgr( SetPtMgrNum ).ChillerIndexPlantSide = CompNumPlantSide;
															CondEntSetPtMgr( SetPtMgrNum ).BranchIndexPlantSide = BranchNumPlantSide;
														}
													}
												}
											}
											CondEntSetPtMgr( SetPtMgrNum ).TypeNum = TypeNum;
											CondEntSetPtMgr( SetPtMgrNum ).LoopIndexDemandSide = LoopNum;
											CondEntSetPtMgr( SetPtMgrNum ).ChillerIndexDemandSide = CompNum;
											CondEntSetPtMgr( SetPtMgrNum ).BranchIndexDemandSide = BranchNum;
										}
									}
								}
							}
						}
					}
				}

				// Ideal condenser entering water temperature reset setpoint manager
				cSetPointManagerType = cValidSPMTypes( iSPMType_IdealCondEntReset );
				NumCT = 0;
				NumChiller = 0;
				for ( SetPtMgrNum = 1; SetPtMgrNum <= NumIdealCondEntSetPtMgrs; ++SetPtMgrNum ) {
					// Scan loops and find the loop index that includes the condenser cooling tower node used as setpoint
					for ( LoopNum = 1; LoopNum <= NumCondLoops + NumPlantLoops; ++LoopNum ) { // Begin demand side loops ... When condenser is added becomes NumLoops
						for ( CtrlNodeIndex = 1; CtrlNodeIndex <= IdealCondEntSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) {
							if ( PlantLoop( LoopNum ).TempSetPointNodeNum == IdealCondEntSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ) ) {
								for ( BranchNum = 1; BranchNum <= PlantLoop( LoopNum ).LoopSide( SupplySide ).TotalBranches; ++BranchNum ) {
									for ( CompNum = 1; CompNum <= PlantLoop( LoopNum ).LoopSide( SupplySide ).Branch( BranchNum ).TotalComponents; ++CompNum ) {
										// Check if cooling tower is single speed and generate and error
										TypeOf_Num = PlantLoop( LoopNum ).LoopSide( SupplySide ).Branch( BranchNum ).Comp( CompNum ).TypeOf_Num;
										if ( TypeOf_Num == TypeOf_CoolingTower_SingleSpd ) {
											ShowSevereError( cSetPointManagerType + "=\"" + IdealCondEntSetPtMgr( SetPtMgrNum ).Name + "\", invalid cooling tower found" );
											ShowContinueError( "Found Single Speed Cooling Tower, Cooling Tower=" + PlantLoop( LoopNum ).LoopSide( SupplySide ).Branch( BranchNum ).Comp( CompNum ).Name );
											ShowContinueError( "SingleSpeed cooling towers cannot be used with this setpoint manager on each loop" );
											ErrorsFound = true;
										}
										// Check if there are more than 1 cooling tower on the plant and generate error
										if ( TypeOf_Num == TypeOf_CoolingTower_TwoSpd || TypeOf_Num == TypeOf_CoolingTower_VarSpdMerkel || TypeOf_Num == TypeOf_CoolingTower_VarSpd ) {
											++NumCT;
											if ( NumCT > 1 ) {
												ShowSevereError( cSetPointManagerType + "=\"" + IdealCondEntSetPtMgr( SetPtMgrNum ).Name + "\", too many cooling towers found" );
												ShowContinueError( "only one cooling tower can be used with this setpoint manager on each loop" );
												ShowContinueError( "Found more than one cooling tower, Cooling Tower=" + PlantLoop( LoopNum ).LoopSide( SupplySide ).Branch( BranchNum ).Comp( CompNum ).Name );
												ErrorsFound = true;
											}
										}
										// Scan the pump on the condenser water loop
										if ( TypeOf_Num == TypeOf_PumpVariableSpeed || TypeOf_Num == TypeOf_PumpConstantSpeed ) {
											IdealCondEntSetPtMgr( SetPtMgrNum ).CondPumpNum = CompNum;
											IdealCondEntSetPtMgr( SetPtMgrNum ).CondPumpBranchNum = BranchNum;
										}
									}
								}
								NumCT = 0;
								// Scan all attached chillers in the condenser loop index found to find the chiller index
								for ( BranchNum = 1; BranchNum <= PlantLoop( LoopNum ).LoopSide( DemandSide ).TotalBranches; ++BranchNum ) {
									for ( CompNum = 1; CompNum <= PlantLoop( LoopNum ).LoopSide( DemandSide ).Branch( BranchNum ).TotalComponents; ++CompNum ) {
										TypeOf_Num = PlantLoop( LoopNum ).LoopSide( DemandSide ).Branch( BranchNum ).Comp( CompNum ).TypeOf_Num;
										if ( TypeOf_Num == TypeOf_Chiller_Absorption || TypeOf_Num == TypeOf_Chiller_Indirect_Absorption || TypeOf_Num == TypeOf_Chiller_CombTurbine || TypeOf_Num == TypeOf_Chiller_ConstCOP || TypeOf_Num == TypeOf_Chiller_Electric || TypeOf_Num == TypeOf_Chiller_ElectricEIR || TypeOf_Num == TypeOf_Chiller_DFAbsorption || TypeOf_Num == TypeOf_Chiller_ElectricReformEIR || TypeOf_Num == TypeOf_Chiller_EngineDriven ) {

											// Scan the supply side to find the chiller index and branch index on plantloop
											TypeNum = PlantLoop( LoopNum ).LoopSide( DemandSide ).Branch( BranchNum ).Comp( CompNum ).TypeOf_Num;
											for ( LoopNum2 = 1; LoopNum2 <= NumCondLoops + NumPlantLoops; ++LoopNum2 ) {
												for ( BranchNumPlantSide = 1; BranchNumPlantSide <= PlantLoop( LoopNum2 ).LoopSide( SupplySide ).TotalBranches; ++BranchNumPlantSide ) {
													for ( CompNumPlantSide = 1; CompNumPlantSide <= PlantLoop( LoopNum2 ).LoopSide( SupplySide ).Branch( BranchNumPlantSide ).TotalComponents; ++CompNumPlantSide ) {
														TypeOf_Num = PlantLoop( LoopNum2 ).LoopSide( SupplySide ).Branch( BranchNumPlantSide ).Comp( CompNumPlantSide ).TypeOf_Num;
														if ( TypeOf_Num == TypeNum ) {
															++NumChiller;
															IdealCondEntSetPtMgr( SetPtMgrNum ).LoopIndexPlantSide = LoopNum2;
															IdealCondEntSetPtMgr( SetPtMgrNum ).ChillerIndexPlantSide = CompNumPlantSide;
															IdealCondEntSetPtMgr( SetPtMgrNum ).BranchIndexPlantSide = BranchNumPlantSide;
															// Scan the pump on the chilled water loop
															for ( BranchNum2 = 1; BranchNum2 <= PlantLoop( LoopNum2 ).LoopSide( SupplySide ).TotalBranches; ++BranchNum2 ) {
																for ( CompNum2 = 1; CompNum2 <= PlantLoop( LoopNum2 ).LoopSide( SupplySide ).Branch( BranchNum2 ).TotalComponents; ++CompNum2 ) {
																	TypeOf_Num = PlantLoop( LoopNum2 ).LoopSide( SupplySide ).Branch( BranchNum2 ).Comp( CompNum2 ).TypeOf_Num;
																	if ( TypeOf_Num == TypeOf_PumpVariableSpeed || TypeOf_Num == TypeOf_PumpConstantSpeed ) {
																		IdealCondEntSetPtMgr( SetPtMgrNum ).ChilledPumpNum = CompNum2;
																		IdealCondEntSetPtMgr( SetPtMgrNum ).ChilledPumpBranchNum = BranchNum2;
																	}
																}
															}
														}
													}
												}
											}
											if ( NumChiller > 1 ) {
												ShowSevereError( cSetPointManagerType + "=\"" + IdealCondEntSetPtMgr( SetPtMgrNum ).Name + "\", too many chillers found" );
												ShowContinueError( "only one chiller can be used with this setpoint manager on each loop" );
												ShowContinueError( "Found more than one chiller, chiller =" + PlantLoop( LoopNum ).LoopSide( DemandSide ).Branch( BranchNum ).Comp( CompNum ).Name );
												ErrorsFound = true;
											}
											IdealCondEntSetPtMgr( SetPtMgrNum ).TypeNum = TypeNum;
											IdealCondEntSetPtMgr( SetPtMgrNum ).CondLoopNum = LoopNum;
											IdealCondEntSetPtMgr( SetPtMgrNum ).TowerNum = CompNum;
											IdealCondEntSetPtMgr( SetPtMgrNum ).CondBranchNum = BranchNum;
										}
									}
								}
								NumChiller = 0;
							}
						}
					}
				}

				VerifySetPointManagers( ErrorsFound );

			}

			MyOneTimeFlag = false;

			if ( ErrorsFound ) {
				ShowFatalError( "InitSetPointManagers: Errors found in getting SetPointManager input." );
			}

		}

		if ( ( BeginEnvrnFlag && MyEnvrnFlag ) || MyOneTimeFlag2 ) {

			ManagerOn = false;

			for ( SetPtMgrNum = 1; SetPtMgrNum <= NumSchSetPtMgrs; ++SetPtMgrNum ) {
				for ( CtrlNodeIndex = 1; CtrlNodeIndex <= SchSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) {
					NodeNum = SchSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number
					// Initialize scheduled setpoints
					{ auto const SELECT_CASE_var( SchSetPtMgr( SetPtMgrNum ).CtrlTypeMode );
					if ( SELECT_CASE_var == iCtrlVarType_Temp ) {
						Node( NodeNum ).TempSetPoint = GetCurrentScheduleValue( SchSetPtMgr( SetPtMgrNum ).SchedPtr );
					} else if ( SELECT_CASE_var == iCtrlVarType_MaxTemp ) {
						Node( NodeNum ).TempSetPointHi = GetCurrentScheduleValue( SchSetPtMgr( SetPtMgrNum ).SchedPtr );
					} else if ( SELECT_CASE_var == iCtrlVarType_MinTemp ) {
						Node( NodeNum ).TempSetPointLo = GetCurrentScheduleValue( SchSetPtMgr( SetPtMgrNum ).SchedPtr );
					} else if ( SELECT_CASE_var == iCtrlVarType_HumRat ) {
						Node( NodeNum ).HumRatSetPoint = GetCurrentScheduleValue( SchSetPtMgr( SetPtMgrNum ).SchedPtr );
					} else if ( SELECT_CASE_var == iCtrlVarType_MaxHumRat ) {
						Node( NodeNum ).HumRatMax = GetCurrentScheduleValue( SchSetPtMgr( SetPtMgrNum ).SchedPtr );
					} else if ( SELECT_CASE_var == iCtrlVarType_MinHumRat ) {
						Node( NodeNum ).HumRatMin = GetCurrentScheduleValue( SchSetPtMgr( SetPtMgrNum ).SchedPtr );
					} else if ( SELECT_CASE_var == iCtrlVarType_MassFlow ) {
						Node( NodeNum ).MassFlowRateSetPoint = GetCurrentScheduleValue( SchSetPtMgr( SetPtMgrNum ).SchedPtr );
					} else if ( SELECT_CASE_var == iCtrlVarType_MaxMassFlow ) {
						Node( NodeNum ).MassFlowRateMax = GetCurrentScheduleValue( SchSetPtMgr( SetPtMgrNum ).SchedPtr );
					} else if ( SELECT_CASE_var == iCtrlVarType_MinMassFlow ) {
						Node( NodeNum ).MassFlowRateMin = GetCurrentScheduleValue( SchSetPtMgr( SetPtMgrNum ).SchedPtr );
					}}
				}
			}

			for ( SetPtMgrNum = 1; SetPtMgrNum <= NumDualSchSetPtMgrs; ++SetPtMgrNum ) {
				for ( CtrlNodeIndex = 1; CtrlNodeIndex <= DualSchSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) {
					NodeNum = DualSchSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number
					if ( DualSchSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_Temp ) {
						Node( NodeNum ).TempSetPointHi = GetCurrentScheduleValue( DualSchSetPtMgr( SetPtMgrNum ).SchedPtrHi );
						Node( NodeNum ).TempSetPointLo = GetCurrentScheduleValue( DualSchSetPtMgr( SetPtMgrNum ).SchedPtrLo );
						Node( NodeNum ).TempSetPoint = ( Node( NodeNum ).TempSetPointHi + Node( NodeNum ).TempSetPointLo ) / 2.0;
					}
				}
			}

			for ( SetPtMgrNum = 1; SetPtMgrNum <= NumOutAirSetPtMgrs; ++SetPtMgrNum ) {
				for ( CtrlNodeIndex = 1; CtrlNodeIndex <= OutAirSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) {
					NodeNum = OutAirSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number
					if ( OutAirSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_Temp ) {
						// Call the CALC routine, with an optional argument to only set
						// the initialization NODE(:)% setpoint, and not the OutAirSetPtMgr(:)%SetPt
						CalcOutsideAirSetPoint( SetPtMgrNum, NodeNum, true );
					}
				}
			}

			for ( SetPtMgrNum = 1; SetPtMgrNum <= NumSZMinHumSetPtMgrs; ++SetPtMgrNum ) { // Minimum humidity setpoint managers
				for ( ZoneIndex = 1; ZoneIndex <= SZMinHumSetPtMgr( SetPtMgrNum ).NumZones; ++ZoneIndex ) {
					ZoneNode = SZMinHumSetPtMgr( SetPtMgrNum ).ZoneNodes( ZoneIndex );
					Node( ZoneNode ).MassFlowRate = 0.0;
				}
				for ( CtrlNodeIndex = 1; CtrlNodeIndex <= SZMinHumSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) {
					NodeNum = SZMinHumSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number
					Node( NodeNum ).HumRatMin = 0.007; // Set the setpoint
				}
			}

			for ( SetPtMgrNum = 1; SetPtMgrNum <= NumSZMaxHumSetPtMgrs; ++SetPtMgrNum ) { // Maximum humidity setpoint managers
				for ( ZoneIndex = 1; ZoneIndex <= SZMaxHumSetPtMgr( SetPtMgrNum ).NumZones; ++ZoneIndex ) {
					ZoneNode = SZMaxHumSetPtMgr( SetPtMgrNum ).ZoneNodes( ZoneIndex );
					Node( ZoneNode ).MassFlowRate = 0.0;
				}
				for ( CtrlNodeIndex = 1; CtrlNodeIndex <= SZMaxHumSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) {
					NodeNum = SZMaxHumSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number
					Node( NodeNum ).HumRatMax = 0.011; // Set the setpoint
				}
			}

			for ( SetPtMgrNum = 1; SetPtMgrNum <= NumSZRhSetPtMgrs; ++SetPtMgrNum ) { // single zone reheat setpoint managers
				ZoneInletNode = SingZoneRhSetPtMgr( SetPtMgrNum ).ZoneInletNodeNum;
				ZoneNode = SingZoneRhSetPtMgr( SetPtMgrNum ).ZoneNodeNum;
				Node( ZoneInletNode ).MassFlowRate = 0.0;
				Node( ZoneNode ).MassFlowRate = 0.0;
				for ( CtrlNodeIndex = 1; CtrlNodeIndex <= SingZoneRhSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) {
					NodeNum = SingZoneRhSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number
					if ( SingZoneRhSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_Temp ) {
						Node( NodeNum ).TempSetPoint = 20.0; // Set the setpoint
					}
				}
			}

			for ( SetPtMgrNum = 1; SetPtMgrNum <= NumSZHtSetPtMgrs; ++SetPtMgrNum ) { // single zone heating setpoint managers
				ZoneInletNode = SingZoneHtSetPtMgr( SetPtMgrNum ).ZoneInletNodeNum;
				ZoneNode = SingZoneHtSetPtMgr( SetPtMgrNum ).ZoneNodeNum;
				Node( ZoneInletNode ).MassFlowRate = 0.0;
				Node( ZoneNode ).MassFlowRate = 0.0;
				for ( CtrlNodeIndex = 1; CtrlNodeIndex <= SingZoneHtSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) {
					NodeNum = SingZoneHtSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number
					if ( SingZoneHtSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_Temp ) {
						Node( NodeNum ).TempSetPoint = 20.0; // Set the setpoint
					}
				}
			}

			for ( SetPtMgrNum = 1; SetPtMgrNum <= NumSZClSetPtMgrs; ++SetPtMgrNum ) { // single zone cooling setpoint managers
				ZoneInletNode = SingZoneClSetPtMgr( SetPtMgrNum ).ZoneInletNodeNum;
				ZoneNode = SingZoneClSetPtMgr( SetPtMgrNum ).ZoneNodeNum;
				Node( ZoneInletNode ).MassFlowRate = 0.0;
				Node( ZoneNode ).MassFlowRate = 0.0;
				for ( CtrlNodeIndex = 1; CtrlNodeIndex <= SingZoneClSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) {
					NodeNum = SingZoneClSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number
					if ( SingZoneClSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_Temp ) {
						Node( NodeNum ).TempSetPoint = 20.0; // Set the setpoint
					}
				}
			}

			for ( SetPtMgrNum = 1; SetPtMgrNum <= NumMixedAirSetPtMgrs; ++SetPtMgrNum ) { // mixed air setpoint managers

				Node( MixedAirSetPtMgr( SetPtMgrNum ).RefNode ).MassFlowRate = 0.0;
				Node( MixedAirSetPtMgr( SetPtMgrNum ).FanInNode ).MassFlowRate = 0.0;
				Node( MixedAirSetPtMgr( SetPtMgrNum ).FanOutNode ).MassFlowRate = 0.0;
				Node( MixedAirSetPtMgr( SetPtMgrNum ).RefNode ).Temp = 20.0;
				Node( MixedAirSetPtMgr( SetPtMgrNum ).FanInNode ).Temp = 20.0;
				Node( MixedAirSetPtMgr( SetPtMgrNum ).FanOutNode ).Temp = 20.0;
				Node( MixedAirSetPtMgr( SetPtMgrNum ).RefNode ).HumRat = OutHumRat;
				Node( MixedAirSetPtMgr( SetPtMgrNum ).FanInNode ).HumRat = OutHumRat;
				Node( MixedAirSetPtMgr( SetPtMgrNum ).FanOutNode ).HumRat = OutHumRat;
				Node( MixedAirSetPtMgr( SetPtMgrNum ).RefNode ).Quality = 1.0;
				Node( MixedAirSetPtMgr( SetPtMgrNum ).FanInNode ).Quality = 1.0;
				Node( MixedAirSetPtMgr( SetPtMgrNum ).FanOutNode ).Quality = 1.0;
				Node( MixedAirSetPtMgr( SetPtMgrNum ).RefNode ).Press = OutBaroPress;
				Node( MixedAirSetPtMgr( SetPtMgrNum ).FanInNode ).Press = OutBaroPress;
				Node( MixedAirSetPtMgr( SetPtMgrNum ).FanOutNode ).Press = OutBaroPress;
				Node( MixedAirSetPtMgr( SetPtMgrNum ).RefNode ).Enthalpy = PsyHFnTdbW( constant_twenty, OutHumRat );
				Node( MixedAirSetPtMgr( SetPtMgrNum ).FanInNode ).Enthalpy = PsyHFnTdbW( constant_twenty, OutHumRat );
				Node( MixedAirSetPtMgr( SetPtMgrNum ).FanOutNode ).Enthalpy = PsyHFnTdbW( constant_twenty, OutHumRat );
				for ( CtrlNodeIndex = 1; CtrlNodeIndex <= MixedAirSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) {
					NodeNum = MixedAirSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number
					if ( MixedAirSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_Temp ) {
						Node( NodeNum ).TempSetPoint = 20.0; // Set the setpoint
					}
				}
			}

			for ( SetPtMgrNum = 1; SetPtMgrNum <= NumOAPretreatSetPtMgrs; ++SetPtMgrNum ) { // Outside Air Pretreat setpoint managers

				Node( OAPretreatSetPtMgr( SetPtMgrNum ).RefNode ).MassFlowRate = 0.0;
				Node( OAPretreatSetPtMgr( SetPtMgrNum ).MixedOutNode ).MassFlowRate = 0.0;
				Node( OAPretreatSetPtMgr( SetPtMgrNum ).OAInNode ).MassFlowRate = 0.0;
				Node( OAPretreatSetPtMgr( SetPtMgrNum ).ReturnInNode ).MassFlowRate = 0.0;
				Node( OAPretreatSetPtMgr( SetPtMgrNum ).RefNode ).Temp = 20.0;
				Node( OAPretreatSetPtMgr( SetPtMgrNum ).MixedOutNode ).Temp = 20.0;
				Node( OAPretreatSetPtMgr( SetPtMgrNum ).OAInNode ).Temp = 20.0;
				Node( OAPretreatSetPtMgr( SetPtMgrNum ).ReturnInNode ).Temp = 20.0;
				Node( OAPretreatSetPtMgr( SetPtMgrNum ).RefNode ).HumRat = OutHumRat;
				Node( OAPretreatSetPtMgr( SetPtMgrNum ).MixedOutNode ).HumRat = OutHumRat;
				Node( OAPretreatSetPtMgr( SetPtMgrNum ).OAInNode ).HumRat = OutHumRat;
				Node( OAPretreatSetPtMgr( SetPtMgrNum ).ReturnInNode ).HumRat = OutHumRat;
				Node( OAPretreatSetPtMgr( SetPtMgrNum ).RefNode ).Quality = 1.0;
				Node( OAPretreatSetPtMgr( SetPtMgrNum ).MixedOutNode ).Quality = 1.0;
				Node( OAPretreatSetPtMgr( SetPtMgrNum ).OAInNode ).Quality = 1.0;
				Node( OAPretreatSetPtMgr( SetPtMgrNum ).ReturnInNode ).Quality = 1.0;
				Node( OAPretreatSetPtMgr( SetPtMgrNum ).RefNode ).Press = OutBaroPress;
				Node( OAPretreatSetPtMgr( SetPtMgrNum ).MixedOutNode ).Press = OutBaroPress;
				Node( OAPretreatSetPtMgr( SetPtMgrNum ).OAInNode ).Press = OutBaroPress;
				Node( OAPretreatSetPtMgr( SetPtMgrNum ).ReturnInNode ).Press = OutBaroPress;
				Node( OAPretreatSetPtMgr( SetPtMgrNum ).RefNode ).Enthalpy = PsyHFnTdbW( constant_twenty, OutHumRat );
				Node( OAPretreatSetPtMgr( SetPtMgrNum ).MixedOutNode ).Enthalpy = PsyHFnTdbW( constant_twenty, OutHumRat );
				Node( OAPretreatSetPtMgr( SetPtMgrNum ).OAInNode ).Enthalpy = PsyHFnTdbW( constant_twenty, OutHumRat );
				Node( OAPretreatSetPtMgr( SetPtMgrNum ).ReturnInNode ).Enthalpy = PsyHFnTdbW( constant_twenty, OutHumRat );
				for ( CtrlNodeIndex = 1; CtrlNodeIndex <= OAPretreatSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) {
					NodeNum = OAPretreatSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number
					if ( OAPretreatSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_Temp ) {
						Node( NodeNum ).TempSetPoint = 20.0; // Set the setpoint
					}
					if ( OAPretreatSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_MaxHumRat ) {
						Node( NodeNum ).HumRatMax = OutHumRat; // Set the setpoint
					}
					if ( OAPretreatSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_MinHumRat ) {
						Node( NodeNum ).HumRatMin = OutHumRat; // Set the setpoint
					}
					if ( OAPretreatSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_HumRat ) {
						Node( NodeNum ).HumRatSetPoint = OutHumRat; // Set the setpoint
					}
				}
			}

			for ( SetPtMgrNum = 1; SetPtMgrNum <= NumWarmestSetPtMgrs; ++SetPtMgrNum ) {
				for ( CtrlNodeIndex = 1; CtrlNodeIndex <= WarmestSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) {
					NodeNum = WarmestSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number
					if ( WarmestSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_Temp ) {
						Node( NodeNum ).TempSetPoint = 20.0; // Set the setpoint
					}
				}
			}

			for ( SetPtMgrNum = 1; SetPtMgrNum <= NumColdestSetPtMgrs; ++SetPtMgrNum ) {
				for ( CtrlNodeIndex = 1; CtrlNodeIndex <= ColdestSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) {
					NodeNum = ColdestSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number
					if ( ColdestSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_Temp ) {
						Node( NodeNum ).TempSetPoint = 20.0; // Set the setpoint
					}
				}
			}

			for ( SetPtMgrNum = 1; SetPtMgrNum <= NumWarmestSetPtMgrsTempFlow; ++SetPtMgrNum ) {
				for ( CtrlNodeIndex = 1; CtrlNodeIndex <= WarmestSetPtMgrTempFlow( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) {
					NodeNum = WarmestSetPtMgrTempFlow( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number
					if ( WarmestSetPtMgrTempFlow( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_Temp ) {
						Node( NodeNum ).TempSetPoint = 20.0; // Set the temperature setpoint
						if ( WarmestSetPtMgrTempFlow( SetPtMgrNum ).AirLoopNum != 0 ) {
							AirLoopFlow( WarmestSetPtMgrTempFlow( SetPtMgrNum ).AirLoopNum ).ReqSupplyFrac = 1.0; // PH 10/09/04 Set the flow
							AirLoopControlInfo( WarmestSetPtMgrTempFlow( SetPtMgrNum ).AirLoopNum ).LoopFlowRateSet = true; // PH 10/09/04 Set the flag
						}
					}
				}
			}

			if ( ZoneEquipInputsFilled && AirLoopInputsFilled ) {
				for ( SetPtMgrNum = 1; SetPtMgrNum <= NumRABFlowSetPtMgrs; ++SetPtMgrNum ) {
					NodeNum = RABFlowSetPtMgr( SetPtMgrNum ).RABSplitOutNode;
					if ( RABFlowSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_MassFlow ) {
						Node( NodeNum ).MassFlowRateSetPoint = 0.0;
					}
				}
			}

			for ( SetPtMgrNum = 1; SetPtMgrNum <= NumMZClgAverageSetPtMgrs; ++SetPtMgrNum ) {
				for ( CtrlNodeIndex = 1; CtrlNodeIndex <= MZAverageCoolingSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) {
					NodeNum = MZAverageCoolingSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number
					if ( MZAverageCoolingSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_Temp ) {
						Node( NodeNum ).TempSetPoint = 20.0; // Set the setpoint
					}
				}
			}

			for ( SetPtMgrNum = 1; SetPtMgrNum <= NumMZHtgAverageSetPtMgrs; ++SetPtMgrNum ) {
				for ( CtrlNodeIndex = 1; CtrlNodeIndex <= MZAverageHeatingSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) {
					NodeNum = MZAverageHeatingSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number
					if ( MZAverageHeatingSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_Temp ) {
						Node( NodeNum ).TempSetPoint = 20.0; // Set the setpoint
					}
				}
			}

			for ( SetPtMgrNum = 1; SetPtMgrNum <= NumMZAverageMinHumSetPtMgrs; ++SetPtMgrNum ) {
				for ( CtrlNodeIndex = 1; CtrlNodeIndex <= MZAverageMinHumSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) {
					NodeNum = MZAverageMinHumSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number
					Node( NodeNum ).HumRatMin = 0.007; // Set the setpoint
				}
			}

			for ( SetPtMgrNum = 1; SetPtMgrNum <= NumMZAverageMaxHumSetPtMgrs; ++SetPtMgrNum ) {
				for ( CtrlNodeIndex = 1; CtrlNodeIndex <= MZAverageMaxHumSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) {
					NodeNum = MZAverageMaxHumSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number
					Node( NodeNum ).HumRatMax = 0.011; // Set the setpoint
				}
			}

			for ( SetPtMgrNum = 1; SetPtMgrNum <= NumMZMinHumSetPtMgrs; ++SetPtMgrNum ) {
				for ( CtrlNodeIndex = 1; CtrlNodeIndex <= MZMinHumSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) {
					NodeNum = MZMinHumSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number
					Node( NodeNum ).HumRatMin = 0.007; // Set the setpoint
				}
			}

			for ( SetPtMgrNum = 1; SetPtMgrNum <= NumMZMaxHumSetPtMgrs; ++SetPtMgrNum ) {
				for ( CtrlNodeIndex = 1; CtrlNodeIndex <= MZMaxHumSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) {
					NodeNum = MZMaxHumSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number
					Node( NodeNum ).HumRatMax = 0.011; // Set the setpoint
				}
			}

			for ( SetPtMgrNum = 1; SetPtMgrNum <= NumFollowOATempSetPtMgrs; ++SetPtMgrNum ) {
				for ( CtrlNodeIndex = 1; CtrlNodeIndex <= FollowOATempSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) {
					NodeNum = FollowOATempSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number
					if ( FollowOATempSetPtMgr( SetPtMgrNum ).RefTypeMode == iRefTempType_WetBulb ) {
						if ( FollowOATempSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_Temp ) {
							Node( NodeNum ).TempSetPoint = OutWetBulbTemp; // Set the setpoint
						} else if ( FollowOATempSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_MaxTemp ) {
							Node( NodeNum ).TempSetPointHi = OutWetBulbTemp; // Set the setpoint
						} else if ( FollowOATempSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_MinTemp ) {
							Node( NodeNum ).TempSetPointLo = OutWetBulbTemp; // Set the setpoint
						}
					} else if ( FollowOATempSetPtMgr( SetPtMgrNum ).RefTypeMode == iRefTempType_DryBulb ) {
						if ( FollowOATempSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_Temp ) {
							Node( NodeNum ).TempSetPoint = OutDryBulbTemp; // Set the setpoint
						} else if ( FollowOATempSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_MaxTemp ) {
							Node( NodeNum ).TempSetPointHi = OutDryBulbTemp; // Set the setpoint
						} else if ( FollowOATempSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_MinTemp ) {
							Node( NodeNum ).TempSetPointLo = OutDryBulbTemp; // Set the setpoint
						}
					}
				}
			}

			for ( SetPtMgrNum = 1; SetPtMgrNum <= NumFollowSysNodeTempSetPtMgrs; ++SetPtMgrNum ) {
				for ( CtrlNodeIndex = 1; CtrlNodeIndex <= FollowSysNodeTempSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) {
					NodeNum = FollowSysNodeTempSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number
					if ( CheckOutAirNodeNumber( FollowSysNodeTempSetPtMgr( SetPtMgrNum ).RefNodeNum ) ) {
						if ( FollowSysNodeTempSetPtMgr( SetPtMgrNum ).RefTypeMode == iRefTempType_WetBulb ) {
							Node( FollowSysNodeTempSetPtMgr( SetPtMgrNum ).RefNodeNum ).SPMNodeWetBulbRepReq = true;
							if ( FollowSysNodeTempSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_Temp ) {
								Node( NodeNum ).TempSetPoint = OutWetBulbTemp; // Set the setpoint
							} else if ( FollowSysNodeTempSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_MaxTemp ) {
								Node( NodeNum ).TempSetPointHi = OutWetBulbTemp; // Set the setpoint
							} else if ( FollowSysNodeTempSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_MinTemp ) {
								Node( NodeNum ).TempSetPointLo = OutWetBulbTemp; // Set the setpoint
							}
						} else if ( FollowSysNodeTempSetPtMgr( SetPtMgrNum ).RefTypeMode == iRefTempType_DryBulb ) {
							if ( FollowSysNodeTempSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_Temp ) {
								Node( NodeNum ).TempSetPoint = OutDryBulbTemp; // Set the setpoint
							} else if ( FollowSysNodeTempSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_MaxTemp ) {
								Node( NodeNum ).TempSetPointHi = OutDryBulbTemp; // Set the setpoint
							} else if ( FollowSysNodeTempSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_MinTemp ) {
								Node( NodeNum ).TempSetPointLo = OutDryBulbTemp; // Set the setpoint
							}
						}
					} else {
						// If reference node is a water node, then set RefTypeMode to NodeDryBulb
						if ( Node( FollowSysNodeTempSetPtMgr( SetPtMgrNum ).RefNodeNum ).FluidType == NodeType_Water ) {
							FollowSysNodeTempSetPtMgr( SetPtMgrNum ).RefTypeMode = iRefTempType_DryBulb;
						} else if ( Node( FollowSysNodeTempSetPtMgr( SetPtMgrNum ).RefNodeNum ).FluidType == NodeType_Air ) {
							if ( FollowSysNodeTempSetPtMgr( SetPtMgrNum ).RefTypeMode == iRefTempType_WetBulb ) {
								Node( FollowSysNodeTempSetPtMgr( SetPtMgrNum ).RefNodeNum ).SPMNodeWetBulbRepReq = true;
							}
						}
						if ( FollowSysNodeTempSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_Temp ) {
							Node( NodeNum ).TempSetPoint = 20.0; // Set the setpoint
						} else if ( FollowSysNodeTempSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_MaxTemp ) {
							Node( NodeNum ).TempSetPointHi = 20.0; // Set the setpoint
						} else if ( FollowSysNodeTempSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_MinTemp ) {
							Node( NodeNum ).TempSetPointLo = 20.0; // Set the setpoint
						}
					}
				}
			}

			for ( SetPtMgrNum = 1; SetPtMgrNum <= NumGroundTempSetPtMgrs; ++SetPtMgrNum ) {
				for ( CtrlNodeIndex = 1; CtrlNodeIndex <= GroundTempSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) {
					NodeNum = GroundTempSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number
					if ( GroundTempSetPtMgr( SetPtMgrNum ).RefTypeMode == iRefGroundTempObjType_BuildingSurface ) {
						if ( GroundTempSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_Temp ) {
							Node( NodeNum ).TempSetPoint = GroundTemp; // Set the setpoint
						} else if ( GroundTempSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_MaxTemp ) {
							Node( NodeNum ).TempSetPointHi = GroundTemp; // Set the setpoint
						} else if ( GroundTempSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_MinTemp ) {
							Node( NodeNum ).TempSetPointLo = GroundTemp; // Set the setpoint
						}
					} else if ( GroundTempSetPtMgr( SetPtMgrNum ).RefTypeMode == iRefGroundTempObjType_Shallow ) {
						if ( GroundTempSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_Temp ) {
							Node( NodeNum ).TempSetPoint = GroundTemp_Surface; // Set the setpoint
						} else if ( GroundTempSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_MaxTemp ) {
							Node( NodeNum ).TempSetPointHi = GroundTemp_Surface; // Set the setpoint
						} else if ( GroundTempSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_MinTemp ) {
							Node( NodeNum ).TempSetPointLo = GroundTemp_Surface; // Set the setpoint
						}
					} else if ( GroundTempSetPtMgr( SetPtMgrNum ).RefTypeMode == iRefGroundTempObjType_Deep ) {
						if ( GroundTempSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_Temp ) {
							Node( NodeNum ).TempSetPoint = GroundTemp_Deep; // Set the setpoint
						} else if ( GroundTempSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_MaxTemp ) {
							Node( NodeNum ).TempSetPointHi = GroundTemp_Deep; // Set the setpoint
						} else if ( GroundTempSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_MinTemp ) {
							Node( NodeNum ).TempSetPointLo = GroundTemp_Deep; // Set the setpoint
						}
					} else if ( GroundTempSetPtMgr( SetPtMgrNum ).RefTypeMode == iRefGroundTempObjType_FCfactorMethod ) {
						if ( GroundTempSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_Temp ) {
							Node( NodeNum ).TempSetPoint = GroundTempFC; // Set the setpoint
						} else if ( GroundTempSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_MaxTemp ) {
							Node( NodeNum ).TempSetPointHi = GroundTempFC; // Set the setpoint
						} else if ( GroundTempSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_MinTemp ) {
							Node( NodeNum ).TempSetPointLo = GroundTempFC; // Set the setpoint
						}
					}
				}
			}

			for ( SetPtMgrNum = 1; SetPtMgrNum <= NumCondEntSetPtMgrs; ++SetPtMgrNum ) { // Condenser entering water Set point managers
				for ( CtrlNodeIndex = 1; CtrlNodeIndex <= CondEntSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) {
					NodeNum = CondEntSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number
					if ( CondEntSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_Temp ) {
						Node( NodeNum ).TempSetPoint = GetCurrentScheduleValue( CondEntSetPtMgr( SetPtMgrNum ).CondEntTempSchedPtr );
					}
				}
			}

			for ( SetPtMgrNum = 1; SetPtMgrNum <= NumIdealCondEntSetPtMgrs; ++SetPtMgrNum ) { // Ideal Condenser entering water Set point managers
				for ( CtrlNodeIndex = 1; CtrlNodeIndex <= IdealCondEntSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) {
					NodeNum = IdealCondEntSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number
					if ( IdealCondEntSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_Temp ) {
						Node( NodeNum ).TempSetPoint = IdealCondEntSetPtMgr( SetPtMgrNum ).MaxCondEntTemp;
					}
				}
			}

			for ( SetPtMgrNum = 1; SetPtMgrNum <= NumSZOneStageCoolingSetPtMgrs; ++SetPtMgrNum ) {
				for ( CtrlNodeIndex = 1; CtrlNodeIndex <= SZOneStageCoolingSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) {
					NodeNum = SZOneStageCoolingSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number
					if ( SZOneStageCoolingSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_Temp ) {
						Node( NodeNum ).TempSetPoint = SZOneStageCoolingSetPtMgr( SetPtMgrNum ).CoolingOffTemp;
					}
				}
			}

			for ( SetPtMgrNum = 1; SetPtMgrNum <= NumSZOneStageHeatingSetPtMgrs; ++SetPtMgrNum ) {
				for ( CtrlNodeIndex = 1; CtrlNodeIndex <= SZOneStageHeatingSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) {
					NodeNum = SZOneStageHeatingSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number
					if ( SZOneStageHeatingSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_Temp ) {
						Node( NodeNum ).TempSetPoint = SZOneStageHeatingSetPtMgr( SetPtMgrNum ).HeatingOffTemp;
					}
				}
			}
			MyEnvrnFlag = false;
			if ( ! MyOneTimeFlag ) MyOneTimeFlag2 = false;

			if ( ErrorsFound ) {
				ShowFatalError( "InitSetPointManagers: Errors found. Program Terminates." );
			}

		} // end begin environment inits
		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag = true;
		}

	}

	void
	SimSetPointManagers()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   July 1998
		//       MODIFIED       Shirey/Raustad (FSEC), Jan 2004
		//                      Nov 2004 M. J. Witte, GARD Analytics, Inc.
		//                        Add new setpoint managers:
		//                          SET POINT MANAGER:SINGLE ZONE HEATING and
		//                          SET POINT MANAGER:SINGLE ZONE COOLING
		//                        Work supported by ASHRAE research project 1254-RP
		//                      Haves Oct 2004
		//                      July 2010 B.A. Nigusse, FSEC/UCF
		//                        Added new setpoint managers
		//                          SetpointManager:MultiZone:Heating:Average
		//                          SetpointManager:MultiZone:Cooling:Average
		//                          SetpointManager:MultiZone:MinimumHumidity:Average
		//                          SetpointManager:MultiZone:MaximumHumidity:Average
		//                      Aug 2010 B.A. Nigusse, FSEC/UCF
		//                        Added new setpoint managers:
		//                          SetpointManager:MultiZone:Humidity:Minimum
		//                          SetpointManager:MultiZone:Humidity:Maximum
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE
		// Loop over all the Setpoint Managers and invoke the correct
		// Setpoint Manager algorithm.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int SetPtMgrNum;

		// Execute all the Setpoint Managers

		// The Scheduled Setpoint Managers

		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumSchSetPtMgrs; ++SetPtMgrNum ) {

			CalcScheduledSetPoint( SetPtMgrNum );

		}

		// The Scheduled Dual Setpoint Managers

		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumDualSchSetPtMgrs; ++SetPtMgrNum ) {

			CalcScheduledDualSetPoint( SetPtMgrNum );

		}

		// The Outside Air Setpoint Managers

		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumOutAirSetPtMgrs; ++SetPtMgrNum ) {

			CalcOutsideAirSetPoint( SetPtMgrNum );

		}

		// The Single Zone Reheat Setpoint Managers

		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumSZRhSetPtMgrs; ++SetPtMgrNum ) {

			CalcSingZoneRhSetPoint( SetPtMgrNum );

		}

		// The Single Zone Heating Setpoint Managers

		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumSZHtSetPtMgrs; ++SetPtMgrNum ) {

			CalcSingZoneHtSetPoint( SetPtMgrNum );

		}

		// The Single Zone Cooling Setpoint Managers

		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumSZClSetPtMgrs; ++SetPtMgrNum ) {

			CalcSingZoneClSetPoint( SetPtMgrNum );

		}

		// The Single Zone Minimum Humidity Setpoint Managers

		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumSZMinHumSetPtMgrs; ++SetPtMgrNum ) {

			CalcSingZoneMinHumSetPoint( SetPtMgrNum );

		}

		// The Single Zone Maximum Humidity Setpoint Managers

		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumSZMaxHumSetPtMgrs; ++SetPtMgrNum ) {

			CalcSingZoneMaxHumSetPoint( SetPtMgrNum );

		}

		// The Warmest Setpoint Managers

		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumWarmestSetPtMgrs; ++SetPtMgrNum ) {

			CalcWarmestSetPoint( SetPtMgrNum );

		}

		// The Coldest Setpoint Managers

		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumColdestSetPtMgrs; ++SetPtMgrNum ) {

			CalcColdestSetPoint( SetPtMgrNum );

		}

		// The Warmest Temp Flow Setpoint Managers

		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumWarmestSetPtMgrsTempFlow; ++SetPtMgrNum ) {

			CalcWarmestSetPointTempFlow( SetPtMgrNum );

		}

		// The RAB Temp Flow Setpoint Managers

		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumRABFlowSetPtMgrs; ++SetPtMgrNum ) {

			CalcRABFlowSetPoint( SetPtMgrNum );

		}

		// The Multizone Average Cooling Setpoint Managers

		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumMZClgAverageSetPtMgrs; ++SetPtMgrNum ) {

			CalcMultiZoneAverageCoolingSetPoint( SetPtMgrNum );

		}

		// The Multizone Average Heating Setpoint Managers

		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumMZHtgAverageSetPtMgrs; ++SetPtMgrNum ) {

			CalcMultiZoneAverageHeatingSetPoint( SetPtMgrNum );

		}

		// The Multizone Average Minimum Humidity Setpoint Managers

		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumMZAverageMinHumSetPtMgrs; ++SetPtMgrNum ) {

			CalcMultiZoneAverageMinHumSetPoint( SetPtMgrNum );

		}

		// The Multizone Average Maximum Humidity Setpoint Managers

		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumMZAverageMaxHumSetPtMgrs; ++SetPtMgrNum ) {

			CalcMultiZoneAverageMaxHumSetPoint( SetPtMgrNum );

		}

		// The Multizone Minimum Humidity Ratio Setpoint Managers
		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumMZMinHumSetPtMgrs; ++SetPtMgrNum ) {

			CalcMultiZoneMinHumSetPoint( SetPtMgrNum );

		}

		// The Multizone Maximum Humidity Ratio Setpoint Managers
		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumMZMaxHumSetPtMgrs; ++SetPtMgrNum ) {

			CalcMultiZoneMaxHumSetPoint( SetPtMgrNum );

		}

		// The Follow Outdoor Air  Temperature Setpoint Managers
		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumFollowOATempSetPtMgrs; ++SetPtMgrNum ) {

			CalcFollowOATempSetPoint( SetPtMgrNum );

		}

		// The Follow System Node Temp Setpoint Managers
		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumFollowSysNodeTempSetPtMgrs; ++SetPtMgrNum ) {

			CalcFollowSysNodeTempSetPoint( SetPtMgrNum );

		}

		// The Ground Temp Setpoint Managers
		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumGroundTempSetPtMgrs; ++SetPtMgrNum ) {

			CalcGroundTempSetPoint( SetPtMgrNum );

		}

		// The Condenser Entering Water Temperature Set Point Managers
		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumCondEntSetPtMgrs; ++SetPtMgrNum ) {

			CalcCondEntSetPoint( SetPtMgrNum );

		}

		// The Ideal Condenser Entering Water Temperature Set Point Managers
		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumIdealCondEntSetPtMgrs; ++SetPtMgrNum ) {

			CalcIdealCondEntSetPoint( SetPtMgrNum );

		}

		// the single zone cooling on/off staged control setpoint managers
		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumSZOneStageCoolingSetPtMgrs; ++SetPtMgrNum ) {
			CalcSZOneStageCoolingSetPt( SetPtMgrNum );
		}

		// the single zone heating on/off staged control setpoint managers
		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumSZOneStageHeatingSetPtMgrs; ++SetPtMgrNum ) {
			CalcSZOneStageHeatingSetPt( SetPtMgrNum );
		}

	}

	void
	CalcScheduledSetPoint( int & SetPtMgrNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   July 1998
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Set the setpoint using a simple schedule.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENTS:

		SchSetPtMgr( SetPtMgrNum ).SetPt = GetCurrentScheduleValue( SchSetPtMgr( SetPtMgrNum ).SchedPtr );

	}

	void
	CalcScheduledDualSetPoint( int & SetPtMgrNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   May 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Set the both setpoint using a simple schedule.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENTS:

		DualSchSetPtMgr( SetPtMgrNum ).SetPtHi = GetCurrentScheduleValue( DualSchSetPtMgr( SetPtMgrNum ).SchedPtrHi );
		DualSchSetPtMgr( SetPtMgrNum ).SetPtLo = GetCurrentScheduleValue( DualSchSetPtMgr( SetPtMgrNum ).SchedPtrLo );

	}

	void
	CalcOutsideAirSetPoint(
		int & SetPtMgrNum,
		Optional_int_const NodeNum, // When Init Calls this routine, it passes the cur node number
		Optional_bool_const InitFlag // When Init Calls this routine, it passes True
	)
	{

		// SUBROUTINE ARGUMENTS:

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 SchedVal;
		Real64 OutLowTemp;
		Real64 OutHighTemp;
		Real64 SetTempAtOutLow;
		Real64 SetTempAtOutHigh;
		int SchedPtr;
		Real64 SetPt;

		SchedPtr = OutAirSetPtMgr( SetPtMgrNum ).SchedPtr;

		if ( SchedPtr > 0 ) {
			SchedVal = GetCurrentScheduleValue( SchedPtr );
		} else {
			SchedVal = 0.0;
		}

		if ( SchedVal == 2.0 ) {
			OutLowTemp = OutAirSetPtMgr( SetPtMgrNum ).OutLow2;
			OutHighTemp = OutAirSetPtMgr( SetPtMgrNum ).OutHigh2;
			SetTempAtOutLow = OutAirSetPtMgr( SetPtMgrNum ).OutLowSetPt2;
			SetTempAtOutHigh = OutAirSetPtMgr( SetPtMgrNum ).OutHighSetPt2;
		} else {
			OutLowTemp = OutAirSetPtMgr( SetPtMgrNum ).OutLow1;
			OutHighTemp = OutAirSetPtMgr( SetPtMgrNum ).OutHigh1;
			SetTempAtOutLow = OutAirSetPtMgr( SetPtMgrNum ).OutLowSetPt1;
			SetTempAtOutHigh = OutAirSetPtMgr( SetPtMgrNum ).OutHighSetPt1;
		}

		if ( OutLowTemp < OutHighTemp && SetTempAtOutLow > SetTempAtOutHigh ) {

			if ( OutDryBulbTemp <= OutLowTemp ) {
				SetPt = SetTempAtOutLow;
			} else if ( OutDryBulbTemp >= OutHighTemp ) {
				SetPt = SetTempAtOutHigh;
			} else {
				SetPt = SetTempAtOutLow - ( ( OutDryBulbTemp - OutLowTemp ) / ( OutHighTemp - OutLowTemp ) ) * ( SetTempAtOutLow - SetTempAtOutHigh );
			}

		} else {
			SetPt = 0.5 * ( SetTempAtOutLow + SetTempAtOutHigh );
		}

		if ( present( InitFlag ) ) {
			Node( NodeNum ).TempSetPoint = SetPt; //Setpoint for Initial Routine
		} else {
			OutAirSetPtMgr( SetPtMgrNum ).SetPt = SetPt; //Setpoint for Calc Routine
		}

	}

	void
	CalcSingZoneRhSetPoint( int const SetPtMgrNum ) // number of the current setpoint manager being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   May 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// From the heating or cooling load of the control zone, calculate the supply air setpoint
		// needed to meet that zone load

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataZoneEnergyDemands;
		using DataHVACGlobals::SmallMassFlow;
		using DataHVACGlobals::SmallLoad;
		using Psychrometrics::PsyTdbFnHW;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 ZoneLoad; // required zone load [W]
		Real64 ZoneMassFlow; // zone inlet mass flow rate [kg/s]
		Real64 CpAir; // inlet air specific heat [J/kg-C]
		int ZoneInletNode;
		int ZoneNode;
		int ZoneNum;
		Real64 ZoneTemp;
		Real64 ZoneLoadToCoolSetPt;
		Real64 ZoneLoadToHeatSetPt;
		Real64 TSetPt;
		Real64 TSetPt1;
		Real64 TSetPt2;
		bool DeadBand;
		int FanNodeIn;
		int FanNodeOut;
		int RetNode;
		int OAMixOAInNode;
		Real64 FanDeltaT;
		static Real64 TSupNoHC( 0.0 ); // supply temperature with no heating or cooling
		Real64 TMixAtMinOA;
		Real64 EnthMixAtMinOA;
		Real64 HumRatMixAtMinOA;
		int AirLoopNum;
		Real64 MinOAFrac;
		int LoopInNode;
		static Real64 ExtrRateNoHC( 0.0 ); // the heating (>0) or cooling (<0) that can be done by supply air at TSupNoHC [W]

		ZoneInletNode = SingZoneRhSetPtMgr( SetPtMgrNum ).ZoneInletNodeNum;
		ZoneNum = SingZoneRhSetPtMgr( SetPtMgrNum ).ControlZoneNum;
		ZoneNode = SingZoneRhSetPtMgr( SetPtMgrNum ).ZoneNodeNum;
		FanNodeIn = SingZoneRhSetPtMgr( SetPtMgrNum ).FanNodeIn;
		FanNodeOut = SingZoneRhSetPtMgr( SetPtMgrNum ).FanNodeOut;
		RetNode = SingZoneRhSetPtMgr( SetPtMgrNum ).RetNode;
		OAMixOAInNode = SingZoneRhSetPtMgr( SetPtMgrNum ).OAInNode;
		AirLoopNum = SingZoneRhSetPtMgr( SetPtMgrNum ).AirLoopNum;
		MinOAFrac = AirLoopFlow( AirLoopNum ).OAMinFrac;
		ZoneMassFlow = Node( ZoneInletNode ).MassFlowRate;
		ZoneLoad = ZoneSysEnergyDemand( ZoneNum ).TotalOutputRequired;
		ZoneLoadToCoolSetPt = ZoneSysEnergyDemand( ZoneNum ).OutputRequiredToCoolingSP;
		ZoneLoadToHeatSetPt = ZoneSysEnergyDemand( ZoneNum ).OutputRequiredToHeatingSP;
		DeadBand = DeadBandOrSetback( ZoneNum );
		ZoneTemp = Node( ZoneNode ).Temp;
		LoopInNode = SingZoneRhSetPtMgr( SetPtMgrNum ).LoopInNode;
		if ( OAMixOAInNode > 0 ) {
			HumRatMixAtMinOA = ( 1.0 - MinOAFrac ) * Node( RetNode ).HumRat + MinOAFrac * Node( OAMixOAInNode ).HumRat;
			EnthMixAtMinOA = ( 1.0 - MinOAFrac ) * Node( RetNode ).Enthalpy + MinOAFrac * Node( OAMixOAInNode ).Enthalpy;
			TMixAtMinOA = PsyTdbFnHW( EnthMixAtMinOA, HumRatMixAtMinOA );
		} else {
			TMixAtMinOA = Node( LoopInNode ).Temp;
		}
		if ( FanNodeOut > 0 && FanNodeIn > 0 ) {
			FanDeltaT = Node( FanNodeOut ).Temp - Node( FanNodeIn ).Temp;
		} else {
			FanDeltaT = 0.0;
		}
		TSupNoHC = TMixAtMinOA + FanDeltaT;
		CpAir = PsyCpAirFnWTdb( Node( ZoneInletNode ).HumRat, Node( ZoneInletNode ).Temp );
		ExtrRateNoHC = CpAir * ZoneMassFlow * ( TSupNoHC - ZoneTemp );
		if ( ZoneMassFlow <= SmallMassFlow ) {
			TSetPt = TSupNoHC;
		} else if ( DeadBand || std::abs( ZoneLoad ) < SmallLoad ) {
			// if air with no active heating or cooling provides cooling
			if ( ExtrRateNoHC < 0.0 ) {
				// if still in deadband, do no active heating or cooling;
				// if below heating setpoint, set a supply temp that will cool to the heating setpoint
				if ( ExtrRateNoHC >= ZoneLoadToHeatSetPt ) {
					TSetPt = TSupNoHC;
				} else {
					TSetPt = ZoneTemp + ZoneLoadToHeatSetPt / ( CpAir * ZoneMassFlow );
				}
				// if air with no active heating or cooling provides heating
			} else if ( ExtrRateNoHC > 0.0 ) {
				// if still in deadband, do no active heating or cooling;
				// if above cooling setpoint, set a supply temp that will heat to the cooling setpoint
				if ( ExtrRateNoHC <= ZoneLoadToCoolSetPt ) {
					TSetPt = TSupNoHC;
				} else {
					TSetPt = ZoneTemp + ZoneLoadToCoolSetPt / ( CpAir * ZoneMassFlow );
				}
			} else {
				TSetPt = TSupNoHC;
			}
		} else if ( ZoneLoad < ( -1.0 * SmallLoad ) ) {
			TSetPt1 = ZoneTemp + ZoneLoad / ( CpAir * ZoneMassFlow );
			TSetPt2 = ZoneTemp + ZoneLoadToHeatSetPt / ( CpAir * ZoneMassFlow );
			if ( TSetPt1 > TSupNoHC ) {
				if ( TSetPt2 > TSupNoHC ) {
					TSetPt = TSetPt2;
				} else {
					TSetPt = TSupNoHC;
				}
			} else {
				TSetPt = TSetPt1;
			}
		} else if ( ZoneLoad > SmallLoad ) {
			TSetPt1 = ZoneTemp + ZoneLoad / ( CpAir * ZoneMassFlow );
			TSetPt2 = ZoneTemp + ZoneLoadToCoolSetPt / ( CpAir * ZoneMassFlow );
			if ( TSetPt1 < TSupNoHC ) {
				if ( TSetPt2 < TSupNoHC ) {
					TSetPt = TSetPt2;
				} else {
					TSetPt = TSupNoHC;
				}
			} else {
				TSetPt = TSetPt1;
			}
		} else {
			TSetPt = TSupNoHC;
		}

		TSetPt = max( min( TSetPt, SingZoneRhSetPtMgr( SetPtMgrNum ).MaxSetTemp ), SingZoneRhSetPtMgr( SetPtMgrNum ).MinSetTemp );
		SingZoneRhSetPtMgr( SetPtMgrNum ).SetPt = TSetPt;

	}

	void
	CalcSingZoneHtSetPoint( int const SetPtMgrNum ) // number of the current setpoint manager being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         M. J. Witte based on CalcSingZoneRhSetPoint by Fred Buhl,
		//                        Work supported by ASHRAE research project 1254-RP
		//       DATE WRITTEN   November 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// From the heating load of the control zone, calculate the supply air setpoint
		// needed to meet that zone load (based on CalcSingZoneRhSetPoint)

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataZoneEnergyDemands::ZoneSysEnergyDemand;
		using DataHVACGlobals::SmallMassFlow;
		using DataHVACGlobals::SmallLoad;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 ZoneLoadtoHeatSP; // required zone load to zone heating setpoint [W]
		Real64 ZoneMassFlow; // zone inlet mass flow rate [kg/s]
		Real64 CpAir; // inlet air specific heat [J/kg-C]
		int ZoneInletNode;
		int ZoneNode;
		int ZoneNum;
		Real64 ZoneTemp;

		ZoneInletNode = SingZoneHtSetPtMgr( SetPtMgrNum ).ZoneInletNodeNum;
		ZoneNum = SingZoneHtSetPtMgr( SetPtMgrNum ).ControlZoneNum;
		ZoneNode = SingZoneHtSetPtMgr( SetPtMgrNum ).ZoneNodeNum;
		ZoneMassFlow = Node( ZoneInletNode ).MassFlowRate;
		ZoneLoadtoHeatSP = ZoneSysEnergyDemand( ZoneNum ).OutputRequiredToHeatingSP;
		ZoneTemp = Node( ZoneNode ).Temp;
		//CR7654 IF (ZoneLoadtoHeatSP.GT.0.0) THEN
		if ( ZoneMassFlow <= SmallMassFlow ) {
			SingZoneHtSetPtMgr( SetPtMgrNum ).SetPt = SingZoneHtSetPtMgr( SetPtMgrNum ).MaxSetTemp;
		} else {
			CpAir = PsyCpAirFnWTdb( Node( ZoneInletNode ).HumRat, Node( ZoneInletNode ).Temp );
			SingZoneHtSetPtMgr( SetPtMgrNum ).SetPt = ZoneTemp + ZoneLoadtoHeatSP / ( CpAir * ZoneMassFlow );
			SingZoneHtSetPtMgr( SetPtMgrNum ).SetPt = max( SingZoneHtSetPtMgr( SetPtMgrNum ).SetPt, SingZoneHtSetPtMgr( SetPtMgrNum ).MinSetTemp );
			SingZoneHtSetPtMgr( SetPtMgrNum ).SetPt = min( SingZoneHtSetPtMgr( SetPtMgrNum ).SetPt, SingZoneHtSetPtMgr( SetPtMgrNum ).MaxSetTemp );
		}
		//CR7654 ELSE
		//CR7654   SingZoneHtSetPtMgr(SetPtMgrNum)%SetPt = SingZoneHtSetPtMgr(SetPtMgrNum)%MinSetTemp
		//CR7654 END IF

	}

	void
	CalcSingZoneClSetPoint( int const SetPtMgrNum ) // number of the current setpoint manager being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         M. J. Witte based on CalcSingZoneRhSetPoint by Fred Buhl,
		//                        Work supported by ASHRAE research project 1254-RP
		//       DATE WRITTEN   November 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// From the Cooling load of the control zone, calculate the supply air setpoint
		// needed to meet that zone load (based on CalcSingZoneRhSetPoint)

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataZoneEnergyDemands::ZoneSysEnergyDemand;
		using DataHVACGlobals::SmallMassFlow;
		using DataHVACGlobals::SmallLoad;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 ZoneLoadtoCoolSP; // required zone load to zone Cooling setpoint [W]
		Real64 ZoneMassFlow; // zone inlet mass flow rate [kg/s]
		Real64 CpAir; // inlet air specific Cool [J/kg-C]
		int ZoneInletNode;
		int ZoneNode;
		int ZoneNum;
		Real64 ZoneTemp;

		ZoneInletNode = SingZoneClSetPtMgr( SetPtMgrNum ).ZoneInletNodeNum;
		ZoneNum = SingZoneClSetPtMgr( SetPtMgrNum ).ControlZoneNum;
		ZoneNode = SingZoneClSetPtMgr( SetPtMgrNum ).ZoneNodeNum;
		ZoneMassFlow = Node( ZoneInletNode ).MassFlowRate;
		ZoneLoadtoCoolSP = ZoneSysEnergyDemand( ZoneNum ).OutputRequiredToCoolingSP;
		ZoneTemp = Node( ZoneNode ).Temp;
		//CR7654 IF (ZoneLoadtoCoolSP.LT.0.0) THEN
		if ( ZoneMassFlow <= SmallMassFlow ) {
			SingZoneClSetPtMgr( SetPtMgrNum ).SetPt = SingZoneClSetPtMgr( SetPtMgrNum ).MinSetTemp;
		} else {
			CpAir = PsyCpAirFnWTdb( Node( ZoneInletNode ).HumRat, Node( ZoneInletNode ).Temp );
			SingZoneClSetPtMgr( SetPtMgrNum ).SetPt = ZoneTemp + ZoneLoadtoCoolSP / ( CpAir * ZoneMassFlow );
			SingZoneClSetPtMgr( SetPtMgrNum ).SetPt = max( SingZoneClSetPtMgr( SetPtMgrNum ).SetPt, SingZoneClSetPtMgr( SetPtMgrNum ).MinSetTemp );
			SingZoneClSetPtMgr( SetPtMgrNum ).SetPt = min( SingZoneClSetPtMgr( SetPtMgrNum ).SetPt, SingZoneClSetPtMgr( SetPtMgrNum ).MaxSetTemp );
		}
		//CR7654 ELSE
		//CR7654   SingZoneClSetPtMgr(SetPtMgrNum)%SetPt = SingZoneClSetPtMgr(SetPtMgrNum)%MaxSetTemp
		//CR7654 END IF

	}

	void
	CalcSZOneStageCoolingSetPt( int const SetPtMgrNum ) // number of the current setpoint manager being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   August 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// calculate the setpoint for staged on/off cooling

		// METHODOLOGY EMPLOYED:
		// Evaluate stage in zone energy demand structure and choose setpoint accordingly

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataZoneEnergyDemands::ZoneSysEnergyDemand;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		if ( ZoneSysEnergyDemand( SZOneStageCoolingSetPtMgr( SetPtMgrNum ).ControlZoneNum ).StageNum >= 0 ) {
			SZOneStageCoolingSetPtMgr( SetPtMgrNum ).SetPt = SZOneStageCoolingSetPtMgr( SetPtMgrNum ).CoolingOffTemp;
		} else { // negative so a cooling stage is set
			SZOneStageCoolingSetPtMgr( SetPtMgrNum ).SetPt = SZOneStageCoolingSetPtMgr( SetPtMgrNum ).CoolingOnTemp;
		}

	}

	void
	CalcSZOneStageHeatingSetPt( int const SetPtMgrNum ) // number of the current setpoint manager being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   August 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// calculate the setpoint for staged on/off control

		// METHODOLOGY EMPLOYED:
		// Evaluate stage in zone energy demand structure and choose setpoint accordingly

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataZoneEnergyDemands::ZoneSysEnergyDemand;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na
		if ( ZoneSysEnergyDemand( SZOneStageHeatingSetPtMgr( SetPtMgrNum ).ControlZoneNum ).StageNum <= 0 ) {
			SZOneStageHeatingSetPtMgr( SetPtMgrNum ).SetPt = SZOneStageHeatingSetPtMgr( SetPtMgrNum ).HeatingOffTemp;
		} else { // positive so a heating stage is set
			SZOneStageHeatingSetPtMgr( SetPtMgrNum ).SetPt = SZOneStageHeatingSetPtMgr( SetPtMgrNum ).HeatingOnTemp;
		}

	}

	void
	CalcSingZoneMinHumSetPoint( int const SetPtMgrNum ) // number of the current setpoint manager being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   October 2000
		//       MODIFIED       Shirey/Raustad Jan 2002
		//                      Gu, Dec 2007
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// From humidity load of the control zone, calculate the supply air humidity
		// needed to meet the minimum humidity setpoint

		// METHODOLOGY EMPLOYED:
		// Zone moisture load from ZoneTempPredictorCorrector (via DataZoneEnergyDemands)
		// is used to calculate the minimum supply air humidity ratio
		// needed to meet minimum zone relative humidity requirement

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHVACGlobals::SmallMassFlow;
		using DataZoneEnergyDemands::ZoneSysMoistureDemand;
		using Psychrometrics::PsyWFnTdbRhPb;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int ZoneNode;
		Real64 ZoneMassFlow;
		//REAL(r64)     :: RelHumSet
		//REAL(r64)     :: ZoneHumRatSet
		int ZoneNum;
		Real64 MoistureLoad; // Zone moisture load (kg moisture/second) required to meet the relative humidity setpoint
		// Value obtained from ZoneTempPredictorCorrector (via ZoneSysMoistureDemand in DataZoneEnergyDemands)
		Real64 SupplyAirHumRat; // Desired air humidity ratio

		SZMinHumSetPtMgr( SetPtMgrNum ).SetPt = 0.0;
		// Only use one zone for now
		ZoneNode = SZMinHumSetPtMgr( SetPtMgrNum ).ZoneNodes( 1 );
		ZoneMassFlow = Node( ZoneNode ).MassFlowRate;
		ZoneNum = SZMinHumSetPtMgr( SetPtMgrNum ).ZoneNum( 1 );

		if ( ZoneMassFlow > SmallMassFlow ) {

			MoistureLoad = ZoneSysMoistureDemand( SZMinHumSetPtMgr( SetPtMgrNum ).ZoneNum( 1 ) ).OutputRequiredToHumidifyingSP;

			SupplyAirHumRat = max( 0.0, Node( ZoneNode ).HumRat + MoistureLoad / ZoneMassFlow );

			// Positive Humidity Ratio MoistureLoad means a humidification load and only humidifying can raise up to a minimum
			//  IF(MoistureLoad .GT. 0.0) SZMinHumSetPtMgr(SetPtMgrNum)%SetPt = SupplyAirHumRat
			SZMinHumSetPtMgr( SetPtMgrNum ).SetPt = SupplyAirHumRat;

		}

	}

	void
	CalcSingZoneMaxHumSetPoint( int const SetPtMgrNum ) // number of the current setpoint manager being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Raustad/Shirey, FSEC
		//       DATE WRITTEN   January 2004
		//       MODIFIED       Gu, Dec. 2007
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// From humidity load of the control zone, calculate the supply air humidity
		// needed to meet the maximum humidity setpoint

		// METHODOLOGY EMPLOYED:
		// Zone moisture load from ZoneTempPredictorCorrector (via DataZoneEnergyDemands)
		// is used to calculate the maximum supply air humidity ratio
		// needed to meet maximum zone relative humidity requirement

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHVACGlobals::SmallMassFlow;
		using DataZoneEnergyDemands::ZoneSysMoistureDemand;
		using Psychrometrics::PsyWFnTdbRhPb;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int ZoneNode; // Control zone air node number
		Real64 ZoneMassFlow; // Zone air mass flow rate (kg/s)
		//REAL(r64)     :: RelHumSet       ! Zone air relative humidity setpoint for this time step (fraction)
		//REAL(r64)     :: ZoneHumRatSet   ! Zone air humidity ratio setpoint for this time step (kg/kg)
		Real64 MoistureLoad; // Zone moisture load (kg moisture/sec) required to meet the relative humidity setpoint
		// Value obtained from ZoneTempPredictorCorrector (via ZoneSysMoistureDemand in DataZoneEnergyDemands)
		Real64 SupplyAirHumRat; // Desired air humidity ratio
		Real64 SystemMassFlow;

		SZMaxHumSetPtMgr( SetPtMgrNum ).SetPt = 0.0;
		// Only use one zone for now
		ZoneNode = SZMaxHumSetPtMgr( SetPtMgrNum ).ZoneNodes( 1 );
		ZoneMassFlow = Node( ZoneNode ).MassFlowRate;

		if ( ZoneMassFlow > SmallMassFlow ) {

			MoistureLoad = ZoneSysMoistureDemand( SZMaxHumSetPtMgr( SetPtMgrNum ).ZoneNum( 1 ) ).OutputRequiredToDehumidifyingSP;

			SystemMassFlow = Node( SZMaxHumSetPtMgr( SetPtMgrNum ).CtrlNodes( 1 ) ).MassFlowRate;

			// MoistureLoad (negative for dehumidification) may be so large that a negative humrat results, cap at 0.00001
			SupplyAirHumRat = max( 0.00001, Node( ZoneNode ).HumRat + MoistureLoad / ZoneMassFlow );

			// This hum rat is currently used in Controller:Simple, control variable "TEMPandHUMRAT" (Jan 2004)
			// Negative MoistureLoad means a dehumidification load
			if ( MoistureLoad < 0.0 ) SZMaxHumSetPtMgr( SetPtMgrNum ).SetPt = SupplyAirHumRat;

		}

	}

	void
	CalcMixedAirSetPoint( int const SetPtMgrNum ) // number of the current setpoint manager being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   May 2001
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Starting with the setpoint at the reference node, subtract the supply fan
		// temperature rise and set the resulting temperature at the mixed air node.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::SysSizingCalc;
		using DataGlobals::AnyEnergyManagementSystemInModel;
		using DataHVACGlobals::SetPointErrorFlag;
		using EMSManager::iTemperatureSetPoint;
		using EMSManager::CheckIfNodeSetPointManagedByEMS;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int FanInNode; // supply fan inlet node number
		int FanOutNode; // supply fan outlet node number
		int RefNode; // setpoint reference node number

		FanInNode = MixedAirSetPtMgr( SetPtMgrNum ).FanInNode;
		FanOutNode = MixedAirSetPtMgr( SetPtMgrNum ).FanOutNode;
		RefNode = MixedAirSetPtMgr( SetPtMgrNum ).RefNode;

		if ( ! SysSizingCalc && MixedAirSetPtMgr( SetPtMgrNum ).MySetPointCheckFlag ) {

			RefNode = MixedAirSetPtMgr( SetPtMgrNum ).RefNode;
			if ( Node( RefNode ).TempSetPoint == SensedNodeFlagValue ) {
				if ( ! AnyEnergyManagementSystemInModel ) {
					ShowSevereError( "CalcMixedAirSetPoint: Missing reference temperature setpoint for Mixed Air Setpoint Manager " + MixedAirSetPtMgr( SetPtMgrNum ).Name );
					ShowContinueError( "Node Referenced =" + NodeID( RefNode ) );
					ShowContinueError( "  use an additional Setpoint Manager with Control Variable = \"Temperature\" to establish a " "setpoint at this node." );
					SetPointErrorFlag = true;
				} else {
					// need call to check if this is the target of an EnergyManagementSystem:Actuator object
					CheckIfNodeSetPointManagedByEMS( RefNode, iTemperatureSetPoint, SetPointErrorFlag );
					if ( SetPointErrorFlag ) {
						ShowSevereError( "CalcMixedAirSetPoint: Missing reference temperature setpoint for Mixed Air Setpoint Manager " + MixedAirSetPtMgr( SetPtMgrNum ).Name );
						ShowContinueError( "Node Referenced =" + NodeID( RefNode ) );
						ShowContinueError( "  use an additional Setpoint Manager with Control Variable = \"Temperature\" to establish a " "setpoint at this node." );
						ShowContinueError( "Or add EMS Actuator to provide temperature setpoint at this node" );
					}
				}
			}

			MixedAirSetPtMgr( SetPtMgrNum ).MySetPointCheckFlag = false;
		}

		MixedAirSetPtMgr( SetPtMgrNum ).SetPt = Node( RefNode ).TempSetPoint - ( Node( FanOutNode ).Temp - Node( FanInNode ).Temp );

	}

	void
	CalcOAPretreatSetPoint( int const SetPtMgrNum ) // number of the current setpoint manager being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         M. J. Witte based on CalcMixedAirSetPoint by Fred Buhl,
		//                        Work supported by ASHRAE research project 1254-RP
		//       DATE WRITTEN   January 2005
		//       MODIFIED       Witte (GARD), Sep 2006
		//                      Griffith( NREL), May 2009, added EMS setpoint checks
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Starting with the setpoint at the reference node, determine the required
		// outside air inlet conditions which when mixed with return air result in
		// the reference setpoint at the mixed air node.
		// (based on CalcMixedAirSetPoint)

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::SysSizingCalc;
		using DataGlobals::AnyEnergyManagementSystemInModel;
		using EMSManager::iTemperatureSetPoint;
		using EMSManager::CheckIfNodeSetPointManagedByEMS;
		using EMSManager::iHumidityRatioSetPoint;
		using EMSManager::iHumidityRatioMinSetPoint;
		using EMSManager::iHumidityRatioMaxSetPoint;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int RefNode; // setpoint reference node number
		int MixedOutNode; // mixed air outlet node number
		int OAInNode; // outside air inlet node number
		int ReturnInNode; // return air inlet node number
		Real64 OAFraction; // outside air fraction of mixed flow rate
		Real64 ReturnInValue; // return air inlet node mass flow rate
		Real64 RefNodeSetPoint; // setpoint at reference node
		Real64 MinSetPoint; // minimum allowed setpoint
		Real64 MaxSetPoint; // maximum allowed setpoint
		bool HumiditySetPoint; // logical to indicate if this is a humidity setpoint
		static bool LocalSetPointCheckFailed( false );

		RefNode = OAPretreatSetPtMgr( SetPtMgrNum ).RefNode;
		MixedOutNode = OAPretreatSetPtMgr( SetPtMgrNum ).MixedOutNode;
		OAInNode = OAPretreatSetPtMgr( SetPtMgrNum ).OAInNode;
		ReturnInNode = OAPretreatSetPtMgr( SetPtMgrNum ).ReturnInNode;
		HumiditySetPoint = false;

		{ auto const SELECT_CASE_var( OAPretreatSetPtMgr( SetPtMgrNum ).CtrlTypeMode );
		if ( SELECT_CASE_var == iCtrlVarType_Temp ) { // 'Temperature'
			RefNodeSetPoint = Node( RefNode ).TempSetPoint;
			ReturnInValue = Node( ReturnInNode ).Temp;
			MinSetPoint = OAPretreatSetPtMgr( SetPtMgrNum ).MinSetTemp;
			MaxSetPoint = OAPretreatSetPtMgr( SetPtMgrNum ).MaxSetTemp;
		} else if ( SELECT_CASE_var == iCtrlVarType_MaxHumRat ) { // 'HUMRATMAX'
			RefNodeSetPoint = Node( RefNode ).HumRatMax;
			ReturnInValue = Node( ReturnInNode ).HumRat;
			MinSetPoint = OAPretreatSetPtMgr( SetPtMgrNum ).MinSetHumRat;
			MaxSetPoint = OAPretreatSetPtMgr( SetPtMgrNum ).MaxSetHumRat;
			HumiditySetPoint = true;
		} else if ( SELECT_CASE_var == iCtrlVarType_MinHumRat ) { // 'HUMRATMIN'
			RefNodeSetPoint = Node( RefNode ).HumRatMin;
			ReturnInValue = Node( ReturnInNode ).HumRat;
			MinSetPoint = OAPretreatSetPtMgr( SetPtMgrNum ).MinSetHumRat;
			MaxSetPoint = OAPretreatSetPtMgr( SetPtMgrNum ).MaxSetHumRat;
			HumiditySetPoint = true;
		} else if ( SELECT_CASE_var == iCtrlVarType_HumRat ) { // 'HumidityRatio'
			RefNodeSetPoint = Node( RefNode ).HumRatSetPoint;
			ReturnInValue = Node( ReturnInNode ).HumRat;
			MinSetPoint = OAPretreatSetPtMgr( SetPtMgrNum ).MinSetHumRat;
			MaxSetPoint = OAPretreatSetPtMgr( SetPtMgrNum ).MaxSetHumRat;
			HumiditySetPoint = true;
		}}

		if ( ! SysSizingCalc && OAPretreatSetPtMgr( SetPtMgrNum ).MySetPointCheckFlag ) {
			OAPretreatSetPtMgr( SetPtMgrNum ).MySetPointCheckFlag = false;
			if ( RefNodeSetPoint == SensedNodeFlagValue ) {
				if ( ! AnyEnergyManagementSystemInModel ) {
					ShowSevereError( "CalcOAPretreatSetPoint: Missing reference setpoint for Outdoor Air Pretreat Setpoint Manager " + OAPretreatSetPtMgr( SetPtMgrNum ).Name );
					ShowContinueError( "Node Referenced =" + NodeID( RefNode ) );
					ShowContinueError( "use a Setpoint Manager to establish a setpoint at this node." );
					ShowFatalError( "Missing reference setpoint." );
				} else {
					LocalSetPointCheckFailed = false;
					{ auto const SELECT_CASE_var( OAPretreatSetPtMgr( SetPtMgrNum ).CtrlTypeMode );
					if ( SELECT_CASE_var == iCtrlVarType_Temp ) { // 'Temperature'
						CheckIfNodeSetPointManagedByEMS( RefNode, iTemperatureSetPoint, LocalSetPointCheckFailed );
					} else if ( SELECT_CASE_var == iCtrlVarType_MaxHumRat ) { // 'HUMRATMAX'
						CheckIfNodeSetPointManagedByEMS( RefNode, iHumidityRatioMaxSetPoint, LocalSetPointCheckFailed );
					} else if ( SELECT_CASE_var == iCtrlVarType_MinHumRat ) { // 'HUMRATMIN'
						CheckIfNodeSetPointManagedByEMS( RefNode, iHumidityRatioMinSetPoint, LocalSetPointCheckFailed );
					} else if ( SELECT_CASE_var == iCtrlVarType_HumRat ) { // 'HumidityRatio'
						CheckIfNodeSetPointManagedByEMS( RefNode, iHumidityRatioSetPoint, LocalSetPointCheckFailed );
					}}
					if ( LocalSetPointCheckFailed ) {
						ShowSevereError( "CalcOAPretreatSetPoint: Missing reference setpoint for Outdoor Air Pretreat Setpoint Manager " + OAPretreatSetPtMgr( SetPtMgrNum ).Name );
						ShowContinueError( "Node Referenced =" + NodeID( RefNode ) );
						ShowContinueError( "use a Setpoint Manager to establish a setpoint at this node." );
						ShowContinueError( "Or use an EMS actuator to control a setpoint at this node." );
						ShowFatalError( "Missing reference setpoint." );
					}
				}
			}
		}
		if ( ( Node( MixedOutNode ).MassFlowRate <= 0.0 ) || ( Node( OAInNode ).MassFlowRate <= 0.0 ) ) {
			OAPretreatSetPtMgr( SetPtMgrNum ).SetPt = RefNodeSetPoint;
		} else if ( HumiditySetPoint && ( RefNodeSetPoint == 0.0 ) ) {
			// For humidity setpoints, zero is special meaning "off" or "no load"
			// so pass through zero setpoints without enforcing the max/min setpoint limits
			OAPretreatSetPtMgr( SetPtMgrNum ).SetPt = 0.0;
		} else {
			OAFraction = Node( OAInNode ).MassFlowRate / Node( MixedOutNode ).MassFlowRate;
			OAPretreatSetPtMgr( SetPtMgrNum ).SetPt = ReturnInValue + ( RefNodeSetPoint - ReturnInValue ) / OAFraction;
			// Apply maximum and minimum values
			OAPretreatSetPtMgr( SetPtMgrNum ).SetPt = max( OAPretreatSetPtMgr( SetPtMgrNum ).SetPt, MinSetPoint );
			OAPretreatSetPtMgr( SetPtMgrNum ).SetPt = min( OAPretreatSetPtMgr( SetPtMgrNum ).SetPt, MaxSetPoint );
		}

	}

	void
	CalcWarmestSetPoint( int const SetPtMgrNum ) // number of the current setpoint manager being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   May 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate the "warmest" supply air setpoint temperature that will satisfy the cooling
		// requirements of all the zones served by a central air system.

		// METHODOLOGY EMPLOYED:
		// Zone sensible heat balance

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataZoneEquipment::ZoneEquipConfig;
		using DataZoneEnergyDemands::ZoneSysEnergyDemand;
		using DataHVACGlobals::SmallMassFlow;
		using DataHVACGlobals::SmallLoad;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 ZoneLoad; // required zone load [W]
		Real64 ZoneMassFlowMax; // zone inlet maximum mass flow rate [kg/s]
		Real64 CpAir; // inlet air specific heat [J/kg-C]
		int AirLoopNum; // the index of the air loop served by this setpoint manager
		Real64 TotCoolLoad; // sum of the zone cooling loads for this air loop [W]
		int ZonesCooledIndex; // DO loop index for zones cooled by the air loop
		int CtrlZoneNum; // the controlled zone index
		int ZoneInletNode; // the zone inlet node number
		Real64 ZoneTemp; // zone temperature [C]
		Real64 ZoneSetPointTemp; // zone supply air temperature [C]
		Real64 SetPointTemp; // the system setpoint temperature [C]
		int ZoneNode; // the zone node number of the current zone
		int ZoneNum; // the actual zone number

		AirLoopNum = WarmestSetPtMgr( SetPtMgrNum ).AirLoopNum;
		TotCoolLoad = 0.0;
		SetPointTemp = WarmestSetPtMgr( SetPtMgrNum ).MaxSetTemp;

		for ( ZonesCooledIndex = 1; ZonesCooledIndex <= AirToZoneNodeInfo( AirLoopNum ).NumZonesCooled; ++ZonesCooledIndex ) {
			CtrlZoneNum = AirToZoneNodeInfo( AirLoopNum ).CoolCtrlZoneNums( ZonesCooledIndex );
			ZoneInletNode = AirToZoneNodeInfo( AirLoopNum ).CoolZoneInletNodes( ZonesCooledIndex );
			ZoneNode = ZoneEquipConfig( CtrlZoneNum ).ZoneNode;
			ZoneNum = ZoneEquipConfig( CtrlZoneNum ).ActualZoneNum;
			ZoneMassFlowMax = Node( ZoneInletNode ).MassFlowRateMax;
			ZoneLoad = ZoneSysEnergyDemand( ZoneNum ).TotalOutputRequired;
			ZoneTemp = Node( ZoneNode ).Temp;
			ZoneSetPointTemp = WarmestSetPtMgr( SetPtMgrNum ).MaxSetTemp;
			if ( ZoneLoad < 0.0 ) {
				TotCoolLoad += std::abs( ZoneLoad );
				CpAir = PsyCpAirFnWTdb( Node( ZoneInletNode ).HumRat, Node( ZoneInletNode ).Temp );
				if ( ZoneMassFlowMax > SmallMassFlow ) {
					ZoneSetPointTemp = ZoneTemp + ZoneLoad / ( CpAir * ZoneMassFlowMax );
				}
			}
			SetPointTemp = min( SetPointTemp, ZoneSetPointTemp );
		}

		SetPointTemp = max( WarmestSetPtMgr( SetPtMgrNum ).MinSetTemp, min( SetPointTemp, WarmestSetPtMgr( SetPtMgrNum ).MaxSetTemp ) );
		if ( TotCoolLoad < SmallLoad ) {
			SetPointTemp = WarmestSetPtMgr( SetPtMgrNum ).MaxSetTemp;
		}

		WarmestSetPtMgr( SetPtMgrNum ).SetPt = SetPointTemp;

	}

	void
	CalcColdestSetPoint( int const SetPtMgrNum ) // number of the current setpoint manager being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   May 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate the "coldest" supply air setpoint temperature that will satisfy the heating
		// requirements of all the zones served by a central air system.

		// METHODOLOGY EMPLOYED:
		// Zone sensible heat balance

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataZoneEquipment::ZoneEquipConfig;
		using DataZoneEnergyDemands::ZoneSysEnergyDemand;
		using DataHVACGlobals::SmallMassFlow;
		using DataHVACGlobals::SmallLoad;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 ZoneLoad; // required zone load [W]
		Real64 ZoneMassFlowMax; // zone inlet maximum mass flow rate [kg/s]
		Real64 CpAir; // inlet air specific heat [J/kg-C]
		int AirLoopNum; // the index of the air loop served by this setpoint manager
		Real64 TotHeatLoad; // sum of the zone heating loads for this air loop [W]
		int ZonesHeatedIndex; // DO loop index for zones heated by the air loop
		int CtrlZoneNum; // the controlled zone index
		int ZoneInletNode; // the zone inlet node number
		Real64 ZoneTemp; // zone temperature [C]
		Real64 ZoneSetPointTemp; // zone supply air temperature [C]
		Real64 SetPointTemp; // the system setpoint temperature [C]
		int ZoneNode; // the zone node number of the current zone
		int ZoneNum; // the actual zone number

		AirLoopNum = ColdestSetPtMgr( SetPtMgrNum ).AirLoopNum;
		TotHeatLoad = 0.0;
		SetPointTemp = ColdestSetPtMgr( SetPtMgrNum ).MinSetTemp;

		for ( ZonesHeatedIndex = 1; ZonesHeatedIndex <= AirToZoneNodeInfo( AirLoopNum ).NumZonesHeated; ++ZonesHeatedIndex ) {
			CtrlZoneNum = AirToZoneNodeInfo( AirLoopNum ).HeatCtrlZoneNums( ZonesHeatedIndex );
			ZoneInletNode = AirToZoneNodeInfo( AirLoopNum ).HeatZoneInletNodes( ZonesHeatedIndex );
			ZoneNode = ZoneEquipConfig( CtrlZoneNum ).ZoneNode;
			ZoneNum = ZoneEquipConfig( CtrlZoneNum ).ActualZoneNum;
			ZoneMassFlowMax = Node( ZoneInletNode ).MassFlowRateMax;
			ZoneLoad = ZoneSysEnergyDemand( ZoneNum ).TotalOutputRequired;
			ZoneTemp = Node( ZoneNode ).Temp;
			ZoneSetPointTemp = ColdestSetPtMgr( SetPtMgrNum ).MinSetTemp;
			if ( ZoneLoad > 0.0 ) {
				TotHeatLoad += ZoneLoad;
				CpAir = PsyCpAirFnWTdb( Node( ZoneInletNode ).HumRat, Node( ZoneInletNode ).Temp );
				if ( ZoneMassFlowMax > SmallMassFlow ) {
					ZoneSetPointTemp = ZoneTemp + ZoneLoad / ( CpAir * ZoneMassFlowMax );
				}
			}
			SetPointTemp = max( SetPointTemp, ZoneSetPointTemp );
		}

		SetPointTemp = min( ColdestSetPtMgr( SetPtMgrNum ).MaxSetTemp, max( SetPointTemp, ColdestSetPtMgr( SetPtMgrNum ).MinSetTemp ) );
		if ( TotHeatLoad < SmallLoad ) {
			SetPointTemp = ColdestSetPtMgr( SetPtMgrNum ).MinSetTemp;
		}

		ColdestSetPtMgr( SetPtMgrNum ).SetPt = SetPointTemp;

	}

	void
	CalcWarmestSetPointTempFlow( int const SetPtMgrNum ) // number of the current setpoint manager being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   May 2002
		//       MODIFIED       Haves, Oct 2004
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate the "warmest" supply air setpoint temperature that will satisfy the cooling
		// requirements of all the zones served by a central air system.

		// METHODOLOGY EMPLOYED:
		// Zone sensible heat balance

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataZoneEquipment::ZoneEquipConfig;
		using DataZoneEnergyDemands::ZoneSysEnergyDemand;
		using DataHVACGlobals::SmallMassFlow;
		using DataHVACGlobals::SmallLoad;
		using DataAirLoop::AirLoopControlInfo;
		using DataAirLoop::AirLoopFlow;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 ZoneLoad; // required zone load [W]
		Real64 ZoneMassFlowMax; // zone inlet maximum mass flow rate [kg/s]
		Real64 CpAir; // inlet air specific heat [J/kg-C]
		int AirLoopNum; // the index of the air loop served by this setpoint manager
		Real64 TotCoolLoad; // sum of the zone cooling loads for this air loop [W]
		int ZonesCooledIndex; // DO loop index for zones cooled by the air loop
		int CtrlZoneNum; // the controlled zone index
		int ZoneInletNode; // the zone inlet node number
		Real64 ZoneTemp; // zone temperature [C]
		Real64 ZoneSetPointTemp; // zone supply air temperature [C]
		Real64 SetPointTemp; // the system setpoint temperature [C]
		int ZoneNode; // the zone node number of the current zone
		int ZoneNum; // the actual zone number
		Real64 MinFracFlow;
		Real64 ZoneFracFlow;
		Real64 FracFlow;
		Real64 MaxSetPointTemp;
		Real64 MinSetPointTemp;
		int CritZoneNumTemp;
		int CritZoneNumFlow;
		int ControlStrategy;

		if ( ! WarmestSetPtMgrTempFlow( SetPtMgrNum ).SimReady ) return;
		AirLoopNum = WarmestSetPtMgrTempFlow( SetPtMgrNum ).AirLoopNum;
		TotCoolLoad = 0.0;
		MaxSetPointTemp = WarmestSetPtMgrTempFlow( SetPtMgrNum ).MaxSetTemp;
		SetPointTemp = MaxSetPointTemp;
		MinSetPointTemp = WarmestSetPtMgrTempFlow( SetPtMgrNum ).MinSetTemp;
		MinFracFlow = WarmestSetPtMgrTempFlow( SetPtMgrNum ).MinTurndown;
		FracFlow = MinFracFlow;
		CritZoneNumTemp = 0;
		CritZoneNumFlow = 0;
		ControlStrategy = WarmestSetPtMgrTempFlow( SetPtMgrNum ).Strategy;

		for ( ZonesCooledIndex = 1; ZonesCooledIndex <= AirToZoneNodeInfo( AirLoopNum ).NumZonesCooled; ++ZonesCooledIndex ) {
			CtrlZoneNum = AirToZoneNodeInfo( AirLoopNum ).CoolCtrlZoneNums( ZonesCooledIndex );
			ZoneInletNode = AirToZoneNodeInfo( AirLoopNum ).CoolZoneInletNodes( ZonesCooledIndex );
			ZoneNode = ZoneEquipConfig( CtrlZoneNum ).ZoneNode;
			ZoneNum = ZoneEquipConfig( CtrlZoneNum ).ActualZoneNum;
			ZoneMassFlowMax = Node( ZoneInletNode ).MassFlowRateMax;
			ZoneLoad = ZoneSysEnergyDemand( ZoneNum ).TotalOutputRequired;
			ZoneTemp = Node( ZoneNode ).Temp;
			ZoneSetPointTemp = MaxSetPointTemp;
			ZoneFracFlow = MinFracFlow;
			if ( ZoneLoad < 0.0 ) {
				TotCoolLoad += std::abs( ZoneLoad );
				CpAir = PsyCpAirFnWTdb( Node( ZoneInletNode ).HumRat, Node( ZoneInletNode ).Temp );
				if ( ZoneMassFlowMax > SmallMassFlow ) {
					if ( ControlStrategy == TempFirst ) {
						// First find supply air temperature required to meet the load at minimum flow. If this is
						// below the minimum supply air temperature, calculate the fractional flow rate required to meet the
						// load at the minimum supply air temperature.
						ZoneSetPointTemp = ZoneTemp + ZoneLoad / ( CpAir * ZoneMassFlowMax * MinFracFlow );
						if ( ZoneSetPointTemp < MinSetPointTemp ) {
							ZoneFracFlow = ( ZoneLoad / ( CpAir * ( MinSetPointTemp - ZoneTemp ) ) ) / ZoneMassFlowMax;
						} else {
							ZoneFracFlow = MinFracFlow;
						}
					} else { // ControlStrategy = FlowFirst
						// First find supply air flow rate required to meet the load at maximum supply air temperature. If this
						// is above the maximum supply air flow rate, calculate the supply air temperature required to meet the
						// load at the maximum flow.
						ZoneFracFlow = ( ZoneLoad / ( CpAir * ( MaxSetPointTemp - ZoneTemp ) ) ) / ZoneMassFlowMax;
						if ( ZoneFracFlow > 1.0 || ZoneFracFlow < 0.0 ) {
							ZoneSetPointTemp = ZoneTemp + ZoneLoad / ( CpAir * ZoneMassFlowMax );
						} else {
							ZoneSetPointTemp = MaxSetPointTemp;
						}
					}
				}
			}
			if ( ZoneSetPointTemp < SetPointTemp ) {
				SetPointTemp = ZoneSetPointTemp;
				CritZoneNumTemp = ZoneNum;
			}
			if ( ZoneFracFlow > FracFlow ) {
				FracFlow = ZoneFracFlow;
				CritZoneNumFlow = ZoneNum;
			}
		}

		SetPointTemp = max( MinSetPointTemp, min( SetPointTemp, MaxSetPointTemp ) );
		FracFlow = max( MinFracFlow, min( FracFlow, 1.0 ) );
		if ( TotCoolLoad < SmallLoad ) {
			SetPointTemp = MaxSetPointTemp;
			FracFlow = MinFracFlow;
		}

		WarmestSetPtMgrTempFlow( SetPtMgrNum ).SetPt = SetPointTemp;
		WarmestSetPtMgrTempFlow( SetPtMgrNum ).Turndown = FracFlow;
		if ( ControlStrategy == TempFirst ) {
			if ( CritZoneNumFlow != 0 ) {
				WarmestSetPtMgrTempFlow( SetPtMgrNum ).CritZoneNum = CritZoneNumFlow;
			} else {
				WarmestSetPtMgrTempFlow( SetPtMgrNum ).CritZoneNum = CritZoneNumTemp;
			}
		} else { // ControlStrategy = FlowFirst
			if ( CritZoneNumTemp != 0 ) {
				WarmestSetPtMgrTempFlow( SetPtMgrNum ).CritZoneNum = CritZoneNumTemp;
			} else {
				WarmestSetPtMgrTempFlow( SetPtMgrNum ).CritZoneNum = CritZoneNumFlow;
			}
		}

	}

	void
	CalcRABFlowSetPoint( int const SetPtMgrNum ) // number of the current setpoint manager being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   July 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Given the desired setpoint temperature, calulate the flow rate through the
		// return asir branch that will deliver the desired temperature at the loop outlet
		// node.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int MixerRABInNode; // Mixer RAB inlet node number
		int MixerSupInNode; // Mixer supply inlet node number
		int MixerOutNode; // Mixer outlet node number
		int LoopOutNode; // loop outlet node number
		Real64 TempSetPt; // the setpoint temperature (from schedule) [C]
		Real64 TempSetPtMod; // the setpoint temperature modified for fan heat gain [C]
		Real64 SupFlow; // supply flow rate before mixing [kg/s]
		Real64 RABFlow; // Return Air Bypass flow rate [kg/s]
		Real64 TotSupFlow; // supply air flow after mixing [kg/s]
		Real64 TempSup; // temperature of supply air before mixing [kg/s]
		Real64 TempRAB; // temperature of return bypass air

		MixerRABInNode = RABFlowSetPtMgr( SetPtMgrNum ).RABMixInNode;
		MixerSupInNode = RABFlowSetPtMgr( SetPtMgrNum ).SupMixInNode;
		MixerOutNode = RABFlowSetPtMgr( SetPtMgrNum ).MixOutNode;
		LoopOutNode = RABFlowSetPtMgr( SetPtMgrNum ).SysOutNode;
		TempSetPt = GetCurrentScheduleValue( RABFlowSetPtMgr( SetPtMgrNum ).SchedPtr );
		TempSetPtMod = TempSetPt - ( Node( LoopOutNode ).Temp - Node( MixerOutNode ).Temp );
		SupFlow = Node( MixerSupInNode ).MassFlowRate;
		TempSup = Node( MixerSupInNode ).Temp;
		TotSupFlow = Node( MixerOutNode ).MassFlowRate;
		TempRAB = Node( MixerRABInNode ).Temp;
		RABFlow = ( TotSupFlow * TempSetPtMod - SupFlow * TempSup ) / max( TempRAB, 1.0 );
		RABFlow = max( 0.0, RABFlow );
		RABFlowSetPtMgr( SetPtMgrNum ).FlowSetPt = RABFlow;

	}

	void
	CalcMultiZoneAverageHeatingSetPoint( int const SetPtMgrNum ) // number of the current setpoint manager being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Bereket Nigusse, FSEC
		//       DATE WRITTEN   July 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the "Average" supply air setpoint temperature that will satisfy the heating
		// requirements of multizones served by a central air system.

		// METHODOLOGY EMPLOYED:
		// Zone sensible (heating load) heat balance around the zones served by a central air system

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataZoneEquipment::ZoneEquipConfig;
		using DataZoneEnergyDemands::ZoneSysEnergyDemand;
		using DataHVACGlobals::SmallMassFlow;
		using DataHVACGlobals::SmallLoad;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 ZoneLoad; // zone load predicted to the setpoint [W]
		Real64 ZoneMassFlowRate; // zone inlet node actual mass flow rate lagged by system one time step[kg/s]
		Real64 CpAir; // inlet air specific heat [J/kg-C]
		int AirLoopNum; // the index of the air loop served by this setpoint manager
		Real64 SumHeatLoad; // sum of the zone's predicted heating loads for this air loop [W]
		Real64 SumProductMdotCpTZoneTot; // sum of the product of zone inlet node actual mass flow rate,
		// Cp of air at zone air node and zone air node temperature for
		// all zones in the air loop [W]
		Real64 SumProductMdotCp; // sum of the product of zone inlet node actual mass flow rate, and
		// Cp of air at zone inlet node for all heated zones in the airloop [W/C]
		Real64 SumProductMdotCpTot; // sum of the product of zone inlet node actual mass flow rate, and
		// Cp of air at zone air node for all zones in the airloop [W/C]
		Real64 ZoneAverageTemp; // multizone average zone air node temperature [C]
		int ZonesHeatedIndex; // DO loop index for zones cooled by the air loop
		int CtrlZoneNum; // the controlled zone index
		int ZoneInletNode; // the zone inlet node number
		Real64 ZoneTemp; // zone air node temperature [C]
		Real64 SetPointTemp; // the system setpoint temperature [C]
		int ZoneNode; // the zone node number of the current zone

		SumHeatLoad = 0.0;
		ZoneAverageTemp = 0.0;
		SumProductMdotCp = 0.0;
		SumProductMdotCpTot = 0.0;
		SumProductMdotCpTZoneTot = 0.0;
		AirLoopNum = MZAverageHeatingSetPtMgr( SetPtMgrNum ).AirLoopNum;
		SetPointTemp = MZAverageHeatingSetPtMgr( SetPtMgrNum ).MinSetTemp;

		for ( ZonesHeatedIndex = 1; ZonesHeatedIndex <= AirToZoneNodeInfo( AirLoopNum ).NumZonesCooled; ++ZonesHeatedIndex ) {
			// DO ZonesHeatedIndex=1,AirToZoneNodeInfo(AirLoopNum)%NumZonesHeated
			// Using AirToZoneNodeInfo(AirLoopNum)%Cool* structure variables since they include heating and cooling.

			// The data for number of zones heated is included in the data structure of the variable
			// "AirToZoneNodeInfo(AirLoopNum)%NumZonesCooled" for all systems.  The data structure
			// "AirToZoneNodeInfo(AirLoopNum)%NumZonesHeated" applies to Dual Duct System only and
			// if used will limit the application of this setpoint manager to other systems.  Thus,
			// the "AirToZoneNodeInfo(AirLoopNum)%NumZonesCooled" data is used instead.

			CtrlZoneNum = AirToZoneNodeInfo( AirLoopNum ).CoolCtrlZoneNums( ZonesHeatedIndex );
			ZoneInletNode = AirToZoneNodeInfo( AirLoopNum ).CoolZoneInletNodes( ZonesHeatedIndex );
			ZoneNode = ZoneEquipConfig( CtrlZoneNum ).ZoneNode;
			ZoneMassFlowRate = Node( ZoneInletNode ).MassFlowRate;
			ZoneLoad = ZoneSysEnergyDemand( CtrlZoneNum ).TotalOutputRequired;
			ZoneTemp = Node( ZoneNode ).Temp;
			CpAir = PsyCpAirFnWTdb( Node( ZoneNode ).HumRat, ZoneTemp );
			SumProductMdotCpTot += ZoneMassFlowRate * CpAir;
			SumProductMdotCpTZoneTot += ZoneMassFlowRate * CpAir * ZoneTemp;
			if ( ZoneLoad > 0.0 ) {
				CpAir = PsyCpAirFnWTdb( Node( ZoneInletNode ).HumRat, Node( ZoneInletNode ).Temp );
				SumHeatLoad += ZoneLoad;
				SumProductMdotCp += ZoneMassFlowRate * CpAir;
			}
		}
		if ( SumProductMdotCpTot > 0.0 ) ZoneAverageTemp = SumProductMdotCpTZoneTot / SumProductMdotCpTot;
		if ( SumProductMdotCp > 0.0 ) SetPointTemp = ZoneAverageTemp + SumHeatLoad / SumProductMdotCp;

		SetPointTemp = min( MZAverageHeatingSetPtMgr( SetPtMgrNum ).MaxSetTemp, max( SetPointTemp, MZAverageHeatingSetPtMgr( SetPtMgrNum ).MinSetTemp ) );
		if ( SumHeatLoad < SmallLoad ) {
			SetPointTemp = MZAverageHeatingSetPtMgr( SetPtMgrNum ).MinSetTemp;
		}
		MZAverageHeatingSetPtMgr( SetPtMgrNum ).SetPt = SetPointTemp;

	}

	void
	CalcMultiZoneAverageCoolingSetPoint( int const SetPtMgrNum ) // number of the current setpoint manager being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Bereket Nigusse, FSEC
		//       DATE WRITTEN   July 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate the "Average" supply air setpoint temperature that will satisfy the cooling
		// requirements of all the zones served by a central air system.

		// METHODOLOGY EMPLOYED:
		// Zone sensible (cooling load) heat balance around the zones served by a central air system

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataZoneEquipment::ZoneEquipConfig;
		using DataZoneEnergyDemands::ZoneSysEnergyDemand;
		using DataHVACGlobals::SmallMassFlow;
		using DataHVACGlobals::SmallLoad;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 ZoneLoad; // zone load predicted to the setpoint [W]
		Real64 ZoneMassFlowRate; // zone inlet node actual mass flow rate lagged by system one time step[kg/s]
		Real64 CpAir; // inlet air specific heat [J/kg-C]
		int AirLoopNum; // the index of the air loop served by this setpoint manager
		Real64 SumCoolLoad; // sum of the zone cooling loads for this air loop [W]
		Real64 SumProductMdotCpTZoneTot; // sum of the product of zone inlet node actual mass flow rate,
		// Cp of air at zone air node and zone air node temperature for
		// all zones in the air loop [W]
		Real64 SumProductMdotCp; // sum of the product of zone inlet node actual mass flow rate, and
		// Cp of air at zone inlet node for cooled zones in the airloop [W/C]
		Real64 SumProductMdotCpTot; // sum of the product of zone inlet node actual mass flow rate, and
		// Cp of air at zone air node for all zones in the airloop [W/C]
		Real64 ZoneAverageTemp; // multizone average zone Air node temperature [C]
		int ZonesCooledIndex; // DO loop index for zones cooled by the air loop
		int CtrlZoneNum; // the controlled zone index
		int ZoneInletNode; // the zone inlet node number
		Real64 ZoneTemp; // zone air node temperature [C]
		Real64 SetPointTemp; // the system setpoint temperature [C]
		int ZoneNode; // the zone node number of the current zone

		SumCoolLoad = 0.0;
		ZoneAverageTemp = 0.0;
		SumProductMdotCp = 0.0;
		SumProductMdotCpTot = 0.0;
		SumProductMdotCpTZoneTot = 0.0;
		AirLoopNum = MZAverageCoolingSetPtMgr( SetPtMgrNum ).AirLoopNum;
		SetPointTemp = MZAverageCoolingSetPtMgr( SetPtMgrNum ).MaxSetTemp;

		for ( ZonesCooledIndex = 1; ZonesCooledIndex <= AirToZoneNodeInfo( AirLoopNum ).NumZonesCooled; ++ZonesCooledIndex ) {
			CtrlZoneNum = AirToZoneNodeInfo( AirLoopNum ).CoolCtrlZoneNums( ZonesCooledIndex );
			ZoneInletNode = AirToZoneNodeInfo( AirLoopNum ).CoolZoneInletNodes( ZonesCooledIndex );
			ZoneNode = ZoneEquipConfig( CtrlZoneNum ).ZoneNode;
			ZoneMassFlowRate = Node( ZoneInletNode ).MassFlowRate;
			ZoneLoad = ZoneSysEnergyDemand( CtrlZoneNum ).TotalOutputRequired;
			ZoneTemp = Node( ZoneNode ).Temp;
			CpAir = PsyCpAirFnWTdb( Node( ZoneNode ).HumRat, ZoneTemp );
			SumProductMdotCpTot += ZoneMassFlowRate * CpAir;
			SumProductMdotCpTZoneTot += ZoneMassFlowRate * CpAir * ZoneTemp;
			if ( ZoneLoad < 0.0 ) {
				CpAir = PsyCpAirFnWTdb( Node( ZoneInletNode ).HumRat, Node( ZoneInletNode ).Temp );
				SumCoolLoad += ZoneLoad;
				SumProductMdotCp += ZoneMassFlowRate * CpAir;
			}
		}
		if ( SumProductMdotCpTot > 0.0 ) ZoneAverageTemp = SumProductMdotCpTZoneTot / SumProductMdotCpTot;
		if ( SumProductMdotCp > 0.0 ) SetPointTemp = ZoneAverageTemp + SumCoolLoad / SumProductMdotCp;

		SetPointTemp = max( MZAverageCoolingSetPtMgr( SetPtMgrNum ).MinSetTemp, min( SetPointTemp, MZAverageCoolingSetPtMgr( SetPtMgrNum ).MaxSetTemp ) );

		if ( std::abs( SumCoolLoad ) < SmallLoad ) {
			SetPointTemp = MZAverageCoolingSetPtMgr( SetPtMgrNum ).MaxSetTemp;
		}

		MZAverageCoolingSetPtMgr( SetPtMgrNum ).SetPt = SetPointTemp;

	}

	void
	CalcMultiZoneAverageMinHumSetPoint( int const SetPtMgrNum ) // number of the current setpoint manager being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Bereket Nigusse, FSEC
		//       DATE WRITTEN   July 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate the "Average" supply air minimum humidity setpoint that will satisfy the minimum
		// humidity ratio requirements of multiple zones served by a central air system.

		// METHODOLOGY EMPLOYED:
		// Zone latent load balance around the zones served by a central air system

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataZoneEquipment::ZoneEquipConfig;
		using DataZoneEnergyDemands::ZoneSysMoistureDemand;
		using DataHVACGlobals::SmallMassFlow;
		using DataHVACGlobals::SmallLoad;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 MoistureLoad; // zone's moisture load predicted to the setpoint [kgH20/s]
		Real64 ZoneMassFlowRate; // zone inlet node actual mass flow rate lagged by system one time step[kg/s]
		int AirLoopNum; // the index of the air loop served by this setpoint manager
		Real64 SumMoistureLoad; // sum of the zone moisture loads for this air loop [W]
		Real64 SumMdot; // sum of the actual mass flow rate for controlled zones in the air loop [kg/s]
		Real64 SumMdotTot; // sum of the actual mass flow rate for this air loop [kg/s]
		Real64 SumProductMdotHumTot; // sum of product of actual mass flow rate at the zone inlet node,
		// and humidity ratio at zones air node for all zones in the airloop [kgH20/s]
		Real64 AverageZoneHum; // multizone average zone air node humidity ratio of all zones in the air loop [kg/kg]
		int ZonesCooledIndex; // DO loop index for zones cooled by the air loop
		int CtrlZoneNum; // the controlled zone index
		int ZoneInletNode; // the zone inlet node number
		Real64 ZoneHum; // zone air node humidity ratio [kg/kg]
		Real64 SetPointHum; // system setpoint humidity ratio [kg/kg]
		int ZoneNode; // the zone node number of the current zone

		SumMdot = 0.0;
		SumMdotTot = 0.0;
		AverageZoneHum = 0.0;
		SumMoistureLoad = 0.0;
		SumProductMdotHumTot = 0.0;
		AirLoopNum = MZAverageMinHumSetPtMgr( SetPtMgrNum ).AirLoopNum;
		SetPointHum = MZAverageMinHumSetPtMgr( SetPtMgrNum ).MinSetHum;

		for ( ZonesCooledIndex = 1; ZonesCooledIndex <= AirToZoneNodeInfo( AirLoopNum ).NumZonesCooled; ++ZonesCooledIndex ) {
			CtrlZoneNum = AirToZoneNodeInfo( AirLoopNum ).CoolCtrlZoneNums( ZonesCooledIndex );
			ZoneInletNode = AirToZoneNodeInfo( AirLoopNum ).CoolZoneInletNodes( ZonesCooledIndex );
			ZoneNode = ZoneEquipConfig( CtrlZoneNum ).ZoneNode;
			ZoneMassFlowRate = Node( ZoneInletNode ).MassFlowRate;
			MoistureLoad = ZoneSysMoistureDemand( CtrlZoneNum ).OutputRequiredToHumidifyingSP;
			ZoneHum = Node( ZoneNode ).HumRat;
			SumMdotTot += ZoneMassFlowRate;
			SumProductMdotHumTot += ZoneMassFlowRate * ZoneHum;
			// For humidification the mositure load is positive
			if ( MoistureLoad > 0.0 ) {
				SumMdot += ZoneMassFlowRate;
				SumMoistureLoad += MoistureLoad;
			}
		}
		if ( SumMdotTot > SmallMassFlow ) AverageZoneHum = SumProductMdotHumTot / SumMdotTot;
		if ( SumMdot > SmallMassFlow ) SetPointHum = max( 0.0, AverageZoneHum + SumMoistureLoad / SumMdot );

		SetPointHum = min( MZAverageMinHumSetPtMgr( SetPtMgrNum ).MaxSetHum, max( SetPointHum, MZAverageMinHumSetPtMgr( SetPtMgrNum ).MinSetHum ) );

		MZAverageMinHumSetPtMgr( SetPtMgrNum ).SetPt = SetPointHum;

	}

	void
	CalcMultiZoneAverageMaxHumSetPoint( int const SetPtMgrNum ) // number of the current setpoint manager being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Bereket Nigusse, FSEC
		//       DATE WRITTEN   July 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate the "Average" supply air maximum humidity setpoint that will satisfy the maximum
		// himudity ratio requirements of multiple zones served by a central air system.

		// METHODOLOGY EMPLOYED:
		// Zone latent load balance around the zones served by a central air system

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataZoneEquipment::ZoneEquipConfig;
		using DataZoneEnergyDemands::ZoneSysMoistureDemand;
		using DataHVACGlobals::SmallMassFlow;
		using DataHVACGlobals::SmallLoad;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 MoistureLoad; // zone's moisture load predicted to the setpoint [kgH20/s]
		Real64 ZoneMassFlowRate; // zone inlet node actual mass flow rate lagged by system one time step[kg/s]
		int AirLoopNum; // the index of the air loop served by this setpoint manager
		Real64 SumMoistureLoad; // sum of the zone moisture loads for this air loop [W]
		Real64 SumMdot; // sum of the actual mass flow rate for controlled zones in the air loop [kg/s]
		Real64 SumMdotTot; // sum of the actual mass flow rate for this air loop [kg/s]
		Real64 SumProductMdotHumTot; // sum of product of actual mass flow rate at the zone inlet node,
		// and humidity ratio at zones air node for all zones in the airloop [kgH20/s]
		Real64 AverageZoneHum; // multizone average zone air node humidity ratio of all zones in the air loop [kg/kg]
		int ZonesCooledIndex; // DO loop index for zones cooled by the air loop
		int CtrlZoneNum; // the controlled zone index
		int ZoneInletNode; // the zone inlet node number
		Real64 ZoneHum; // zone air node humidity ratio [kg/kg]
		//  REAL(r64)      :: AverageSetPointHum   ! Supply air humidity ratio [kg/kg]
		Real64 SetPointHum; // system setpoint humidity ratio [kg/kg]
		int ZoneNode; // the zone node number of the current zone

		SumMdot = 0.0;
		SumMdotTot = 0.0;
		AverageZoneHum = 0.0;
		SumMoistureLoad = 0.0;
		SumProductMdotHumTot = 0.0;
		AirLoopNum = MZAverageMaxHumSetPtMgr( SetPtMgrNum ).AirLoopNum;
		SetPointHum = MZAverageMaxHumSetPtMgr( SetPtMgrNum ).MaxSetHum;

		for ( ZonesCooledIndex = 1; ZonesCooledIndex <= AirToZoneNodeInfo( AirLoopNum ).NumZonesCooled; ++ZonesCooledIndex ) {
			CtrlZoneNum = AirToZoneNodeInfo( AirLoopNum ).CoolCtrlZoneNums( ZonesCooledIndex );
			ZoneInletNode = AirToZoneNodeInfo( AirLoopNum ).CoolZoneInletNodes( ZonesCooledIndex );
			ZoneNode = ZoneEquipConfig( CtrlZoneNum ).ZoneNode;
			ZoneMassFlowRate = Node( ZoneInletNode ).MassFlowRate;
			MoistureLoad = ZoneSysMoistureDemand( CtrlZoneNum ).OutputRequiredToDehumidifyingSP;
			ZoneHum = Node( ZoneNode ).HumRat;
			SumMdotTot += ZoneMassFlowRate;
			SumProductMdotHumTot += ZoneMassFlowRate * ZoneHum;
			// For dehumidification the mositure load is negative
			if ( MoistureLoad < 0.0 ) {
				SumMdot += ZoneMassFlowRate;
				SumMoistureLoad += MoistureLoad;
			}
		}
		if ( SumMdotTot > SmallMassFlow ) AverageZoneHum = SumProductMdotHumTot / SumMdotTot;
		if ( SumMdot > SmallMassFlow ) SetPointHum = max( 0.0, AverageZoneHum + SumMoistureLoad / SumMdot );

		SetPointHum = max( MZAverageMaxHumSetPtMgr( SetPtMgrNum ).MinSetHum, min( SetPointHum, MZAverageMaxHumSetPtMgr( SetPtMgrNum ).MaxSetHum ) );
		MZAverageMaxHumSetPtMgr( SetPtMgrNum ).SetPt = SetPointHum;

	}

	void
	CalcMultiZoneMinHumSetPoint( int const SetPtMgrNum ) // number of the current setpoint manager being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Bereket Nigusse, FSEC/UCF
		//       DATE WRITTEN   Aug 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the minimum supply air humidity ratio based on humidification requirements of
		// a controlled zone with critical humidification need (i.e., a zone with the highest
		// humidity ratio setpoint) in an air loop served by a central air-conditioner.
		// METHODOLOGY EMPLOYED:
		// Uses moisture mass balance to calculate the humidity ratio setpoint. The algorithm loops
		// over all the zones that a central air system can humidify and calculates the setpoint based
		// on a zone with the highest humidity ratio setpoint requirement:

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataZoneEquipment::ZoneEquipConfig;
		using DataZoneEnergyDemands::ZoneSysMoistureDemand;
		using DataHVACGlobals::SmallMassFlow;
		using DataHVACGlobals::SmallLoad;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const SmallMoistureLoad( 0.00001 ); // small moisture load [kgH2O/s]

		// INTERFACE BLOCK SPECIFICATIONS

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int AirLoopNum; // the index of the air loop served by this setpoint manager
		int ZonesCooledIndex; // DO loop index for zones cooled by the air loop
		int CtrlZoneNum; // the controlled zone index
		int ZoneInletNode; // the zone inlet node number
		int ZoneNode; // the zone node number of the current zone
		Real64 ZoneHum; // zone air node humidity ratio [kg/kg]
		Real64 SetPointHum; // system setpoint humidity ratio [kg/kg]
		Real64 ZoneSetPointHum; // Zone setpoint humidity ratio [kg/kg]
		Real64 MoistureLoad; // zone's moisture load predicted to the setpoint [kgH20/s]
		Real64 ZoneMassFlowRate; // zone inlet node actual supply air mass flow rate [kg/s]
		Real64 SumMoistureLoad( 0.0 ); // sum of the zone moisture loads for this air loop [W]

		AirLoopNum = MZMinHumSetPtMgr( SetPtMgrNum ).AirLoopNum;
		SetPointHum = MZMinHumSetPtMgr( SetPtMgrNum ).MinSetHum;

		for ( ZonesCooledIndex = 1; ZonesCooledIndex <= AirToZoneNodeInfo( AirLoopNum ).NumZonesCooled; ++ZonesCooledIndex ) {
			CtrlZoneNum = AirToZoneNodeInfo( AirLoopNum ).CoolCtrlZoneNums( ZonesCooledIndex );
			ZoneInletNode = AirToZoneNodeInfo( AirLoopNum ).CoolZoneInletNodes( ZonesCooledIndex );
			ZoneNode = ZoneEquipConfig( CtrlZoneNum ).ZoneNode;
			ZoneMassFlowRate = Node( ZoneInletNode ).MassFlowRate;
			MoistureLoad = ZoneSysMoistureDemand( CtrlZoneNum ).OutputRequiredToHumidifyingSP;
			ZoneHum = Node( ZoneNode ).HumRat;
			ZoneSetPointHum = MZMinHumSetPtMgr( SetPtMgrNum ).MinSetHum;
			// For humidification the mositure load is positive
			if ( MoistureLoad > 0.0 ) {
				SumMoistureLoad += MoistureLoad;
				if ( ZoneMassFlowRate > SmallMassFlow ) {
					ZoneSetPointHum = max( 0.0, ZoneHum + MoistureLoad / ZoneMassFlowRate );
				}
			}
			SetPointHum = max( SetPointHum, ZoneSetPointHum );
		}
		SetPointHum = min( MZMinHumSetPtMgr( SetPtMgrNum ).MaxSetHum, max( SetPointHum, MZMinHumSetPtMgr( SetPtMgrNum ).MinSetHum ) );
		if ( SumMoistureLoad < SmallMoistureLoad ) {
			SetPointHum = MZMinHumSetPtMgr( SetPtMgrNum ).MinSetHum;
		}
		MZMinHumSetPtMgr( SetPtMgrNum ).SetPt = SetPointHum;

	}

	void
	CalcMultiZoneMaxHumSetPoint( int const SetPtMgrNum ) // number of the current setpoint manager being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Bereket Nigusse, FSEC/UCF
		//       DATE WRITTEN   August 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the maximum supply air humidity ratio based on dehumidification requirements of
		// a controlled zone with critical dehumidification need (i.e., a zone with the lowest
		// humidity ratio setpoint) in an air loop served by a central air-conditioner.

		// METHODOLOGY EMPLOYED:
		// Uses moisture mass balance to calculate the humidity ratio setpoint. The algorithm loops
		// over all the zones that a central air system can dehumidify and calculates the setpoint
		// based on a zone with the lowest humidity ratio setpoint requirement:

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataZoneEquipment::ZoneEquipConfig;
		using DataZoneEnergyDemands::ZoneSysMoistureDemand;
		using DataHVACGlobals::SmallMassFlow;
		using DataHVACGlobals::SmallLoad;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const SmallMoistureLoad( 0.00001 ); // small moisture load [kgH2O/s]

		// INTERFACE BLOCK SPECIFICATIONS

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int AirLoopNum; // the index of the air loop served by this setpoint manager
		int ZonesCooledIndex; // DO loop index for zones cooled by the air loop
		int CtrlZoneNum; // the controlled zone index
		int ZoneInletNode; // the zone inlet node number
		int ZoneNode; // the zone node number of the current zone
		Real64 ZoneHum; // zone air node humidity ratio [kg/kg]
		Real64 SetPointHum; // system setpoint humidity ratio [kg/kg]
		Real64 ZoneSetPointHum; // Zone setpoint humidity ratio [kg/kg]
		Real64 MoistureLoad; // zone's moisture load predicted to the setpoint [kgH20/s]
		Real64 ZoneMassFlowRate; // zone inlet node actual supply air mass flow rate [kg/s]
		Real64 SumMoistureLoad( 0.0 ); // sum of the zone moisture loads for this air loop [W]

		AirLoopNum = MZMaxHumSetPtMgr( SetPtMgrNum ).AirLoopNum;
		SetPointHum = MZMaxHumSetPtMgr( SetPtMgrNum ).MaxSetHum;

		for ( ZonesCooledIndex = 1; ZonesCooledIndex <= AirToZoneNodeInfo( AirLoopNum ).NumZonesCooled; ++ZonesCooledIndex ) {
			CtrlZoneNum = AirToZoneNodeInfo( AirLoopNum ).CoolCtrlZoneNums( ZonesCooledIndex );
			ZoneInletNode = AirToZoneNodeInfo( AirLoopNum ).CoolZoneInletNodes( ZonesCooledIndex );
			ZoneNode = ZoneEquipConfig( CtrlZoneNum ).ZoneNode;
			ZoneMassFlowRate = Node( ZoneInletNode ).MassFlowRate;
			MoistureLoad = ZoneSysMoistureDemand( CtrlZoneNum ).OutputRequiredToDehumidifyingSP;
			ZoneHum = Node( ZoneNode ).HumRat;
			ZoneSetPointHum = MZMaxHumSetPtMgr( SetPtMgrNum ).MaxSetHum;

			// For dehumidification the mositure load is negative
			if ( MoistureLoad < 0.0 ) {
				SumMoistureLoad += MoistureLoad;
				if ( ZoneMassFlowRate > SmallMassFlow ) {
					ZoneSetPointHum = max( 0.0, ZoneHum + MoistureLoad / ZoneMassFlowRate );
				}
			}
			SetPointHum = min( SetPointHum, ZoneSetPointHum );
		}
		SetPointHum = max( MZMaxHumSetPtMgr( SetPtMgrNum ).MinSetHum, min( SetPointHum, MZMaxHumSetPtMgr( SetPtMgrNum ).MaxSetHum ) );

		if ( std::abs( SumMoistureLoad ) < SmallMoistureLoad ) {
			SetPointHum = MZMaxHumSetPtMgr( SetPtMgrNum ).MaxSetHum;
		}

		MZMaxHumSetPtMgr( SetPtMgrNum ).SetPt = SetPointHum;

	}

	void
	CalcFollowOATempSetPoint( int const SetPtMgrNum ) // number of the current setpoint manager being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Chandan Sharma, FSEC
		//       DATE WRITTEN   July 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Set the setpoint based on outdoor air dry-bulb/wet-bulb temperature

		// METHODOLOGY EMPLOYED:
		// Based on reference temperature type specifed in the setpoint manager,
		// the setpoint is calculated as OutWetBulbTemp(Or OutDryBulbTemp) + Offset.
		// The sign convention is that a positive Offset will increase the resulting setpoint.
		// Final value of the setpoint is limited by the Max and Min limit specified in the setpoint manager.

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//  INTEGER      :: CtrldNodeNum    ! index of the items in the controlled node list
		Real64 MinSetPoint; // minimum allowed setpoint
		Real64 MaxSetPoint; // maximum allowed setpoint

		MaxSetPoint = FollowOATempSetPtMgr( SetPtMgrNum ).MaxSetTemp;
		MinSetPoint = FollowOATempSetPtMgr( SetPtMgrNum ).MinSetTemp;

		{ auto const SELECT_CASE_var( FollowOATempSetPtMgr( SetPtMgrNum ).RefTypeMode );
		if ( SELECT_CASE_var == iRefTempType_WetBulb ) {
			FollowOATempSetPtMgr( SetPtMgrNum ).SetPt = OutWetBulbTemp + FollowOATempSetPtMgr( SetPtMgrNum ).Offset;
		} else if ( SELECT_CASE_var == iRefTempType_DryBulb ) {
			FollowOATempSetPtMgr( SetPtMgrNum ).SetPt = OutDryBulbTemp + FollowOATempSetPtMgr( SetPtMgrNum ).Offset;
		}}

		// Apply maximum and minimum values
		FollowOATempSetPtMgr( SetPtMgrNum ).SetPt = max( FollowOATempSetPtMgr( SetPtMgrNum ).SetPt, MinSetPoint );
		FollowOATempSetPtMgr( SetPtMgrNum ).SetPt = min( FollowOATempSetPtMgr( SetPtMgrNum ).SetPt, MaxSetPoint );

	}

	void
	CalcFollowSysNodeTempSetPoint( int const SetPtMgrNum ) // number of the current setpoint manager being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Chandan Sharma, FSEC
		//       DATE WRITTEN   July 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Set the setpoint based on current temperatures at a separate system node.

		// METHODOLOGY EMPLOYED:
		// The current value of the temperature at a reference node are obtained and used
		// to generate setpoint on a second system node.  If the reference node is also designated
		// to be an outdoor air (intake) node, then this setpoint manager can be used to follow
		// outdoor air conditions that are adjusted for altitude.
		// Also, based on reference temperature type specifed in the setpoint manager, the out door air wet-bulb
		// or dry-bulb temperature at the reference node could be used.
		// A temperature offset will be applied to the value obtained from the reference system node.
		// If this value is zero, and the limits are met, then the resulting setpoint will be exactly the same
		// as the reference system node temperature.  The sign convention is that a positive offset will increase
		// the resulting setpoint.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int RefNode; // setpoint reference node number
		Real64 RefNodeTemp; // setpoint at reference node
		Real64 MinSetPoint; // minimum allowed setpoint
		Real64 MaxSetPoint; // maximum allowed setpoint

		RefNodeTemp = 0.0;

		MaxSetPoint = FollowSysNodeTempSetPtMgr( SetPtMgrNum ).MaxSetTemp;
		MinSetPoint = FollowSysNodeTempSetPtMgr( SetPtMgrNum ).MinSetTemp;

		RefNode = FollowSysNodeTempSetPtMgr( SetPtMgrNum ).RefNodeNum;

		{ auto const SELECT_CASE_var( FollowSysNodeTempSetPtMgr( SetPtMgrNum ).RefTypeMode );
		if ( SELECT_CASE_var == iRefTempType_WetBulb ) {
			if ( allocated( MoreNodeInfo ) ) {
				RefNodeTemp = MoreNodeInfo( RefNode ).WetBulbTemp;
			}
		} else if ( SELECT_CASE_var == iRefTempType_DryBulb ) {
			RefNodeTemp = Node( RefNode ).Temp;
		}}

		FollowSysNodeTempSetPtMgr( SetPtMgrNum ).SetPt = RefNodeTemp + FollowSysNodeTempSetPtMgr( SetPtMgrNum ).Offset;

		// Apply maximum and minimum values
		FollowSysNodeTempSetPtMgr( SetPtMgrNum ).SetPt = max( FollowSysNodeTempSetPtMgr( SetPtMgrNum ).SetPt, MinSetPoint );
		FollowSysNodeTempSetPtMgr( SetPtMgrNum ).SetPt = min( FollowSysNodeTempSetPtMgr( SetPtMgrNum ).SetPt, MaxSetPoint );

	}

	void
	CalcGroundTempSetPoint( int const SetPtMgrNum ) // number of the current setpoint manager being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Chandan Sharma, FSEC
		//       DATE WRITTEN   July 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Set the setpoint based on current ground temperature

		// METHODOLOGY EMPLOYED:
		// Based on reference ground temperature object type specifed in the setpoint manager,
		// the setpoint is calculated as GroundTemperature + Offset.
		// The sign convention is that a positive Offset will increase the resulting setpoint.
		// Final value of the setpoint is limited by the Max and Min limit specified in the setpoint manager.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataEnvironment::GroundTemp_Deep;
		using DataEnvironment::GroundTemp;
		using DataEnvironment::GroundTemp_Surface;
		using DataEnvironment::GroundTempFC;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//  INTEGER      :: CtrldNodeNum    ! index of the items in the controlled node list
		Real64 MinSetPoint; // minimum allowed setpoint
		Real64 MaxSetPoint; // maximum allowed setpoint

		MaxSetPoint = GroundTempSetPtMgr( SetPtMgrNum ).MaxSetTemp;
		MinSetPoint = GroundTempSetPtMgr( SetPtMgrNum ).MinSetTemp;

		{ auto const SELECT_CASE_var( GroundTempSetPtMgr( SetPtMgrNum ).RefTypeMode );
		if ( SELECT_CASE_var == iRefGroundTempObjType_BuildingSurface ) {
			GroundTempSetPtMgr( SetPtMgrNum ).SetPt = GroundTemp + GroundTempSetPtMgr( SetPtMgrNum ).Offset;
		} else if ( SELECT_CASE_var == iRefGroundTempObjType_Shallow ) {
			GroundTempSetPtMgr( SetPtMgrNum ).SetPt = GroundTemp_Surface + GroundTempSetPtMgr( SetPtMgrNum ).Offset;
		} else if ( SELECT_CASE_var == iRefGroundTempObjType_Deep ) {
			GroundTempSetPtMgr( SetPtMgrNum ).SetPt = GroundTemp_Deep + GroundTempSetPtMgr( SetPtMgrNum ).Offset;
		} else if ( SELECT_CASE_var == iRefGroundTempObjType_FCfactorMethod ) {
			GroundTempSetPtMgr( SetPtMgrNum ).SetPt = GroundTempFC + GroundTempSetPtMgr( SetPtMgrNum ).Offset;
		}}

		// Apply maximum and minimum values
		GroundTempSetPtMgr( SetPtMgrNum ).SetPt = max( GroundTempSetPtMgr( SetPtMgrNum ).SetPt, MinSetPoint );
		GroundTempSetPtMgr( SetPtMgrNum ).SetPt = min( GroundTempSetPtMgr( SetPtMgrNum ).SetPt, MaxSetPoint );

	}

	void
	CalcCondEntSetPoint( int const SetPtMgrNum ) // number of the current set point manager being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Atefe Makhmalbaf and Heejin Cho, PNNL
		//       DATE WRITTEN   March 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate the optimal condenser water temperature set point for a chiller plant
		// with one or more chillers.  The condenser water leaving the tower should be at this temperature
		// for optimal operation of the chiller plant.

		// METHODOLOGY EMPLOYED:
		// using one curve to determine the optimum condenser entering water temperature for a given timestep
		// and two other curves to place boundary conditions on the optimal setpoint value.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataEnvironment::CurMnDy;
		using DataEnvironment::OutDryBulbTemp;
		using DataEnvironment::OutWetBulbTemp;
		using CurveManager::CurveValue;
		using ScheduleManager::GetCurrentScheduleValue;
		using namespace DataPlant;
		using DataLoopNode::Node;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS

		// DERIVED TYPE DEFINITIONS
		// na
		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		static Real64 NormDsnCondFlow( 0.0 ); // Normalized design condenser flow for cooling towers, m3/s per watt
		static Real64 Twr_DesignWB( 0.0 ); // The cooling tower design inlet air wet bulb temperature, C
		static Real64 Dsn_EntCondTemp( 0.0 ); // The chiller design entering condenser temp, C; e.g. 29.44C {85F}
		static Real64 Dsn_CondMinThisChiller( 0.0 ); // Design Minimum Condenser Entering for current chillers this timestep
		static Real64 Dsn_MinCondSetpt( 0.0 ); // The design minimum condenser water temp, C; e.g. 18.33C {65 F}
		static Real64 Cur_MinLiftTD( 0.0 ); // Minimum lift (TCond entering - Tevap leaving) TD this timestep
		static Real64 temp_MinLiftTD( 0.0 ); // Intermeidate variable associated with lift (TCond entering - Tevap leaving) TD
		static Real64 Des_Load( 0.0 ); // array of chiller design loads
		static Real64 Act_Load( 0.0 ); // array of chiller actual loads
		static Real64 ALW( 0.0 ); // Actual load weighting of each chiller, W
		static Real64 DLW( 0.0 ); // Design capacity of each chiller, W
		static Real64 Design_Load_Sum( 0.0 ); // the design load of the chillers, W
		static Real64 Actual_Load_Sum( 0.0 ); // the actual load of the chillers, W
		static Real64 Weighted_Actual_Load_Sum( 0.0 ); // Intermediate weighted value of actual load on plant, W
		static Real64 Weighted_Design_Load_Sum( 0.0 ); // Intermediate weighted value of design load on plant, W
		static Real64 Weighted_Ratio( 0.0 ); // Weighted part load ratio of chillers
		static Real64 Min_DesignWB( 0.0 ); // Minimum design twr wet bulb allowed, C
		static Real64 Min_ActualWb( 0.0 ); // Minimum actual oa wet bulb allowed, C
		static Real64 SetPoint( 0.0 ); // Condenser entering water temperature setpoint this timestep, C
		static Real64 Opt_CondEntTemp( 0.0 ); // Optimized Condenser entering water temperature setpoint this timestep, C
		static Real64 CondWaterSetPoint( 0.0 ); // Condenser entering water temperature setpoint this timestep, C
		static Real64 DesignClgCapacity_Watts( 0.0 );
		static Real64 CurrentLoad_Watts( 0.0 );
		static Real64 CondInletTemp( 0.0 ); // Condenser water inlet temperature (C)
		static Real64 TempDesCondIn( 0.0 ); // Design condenser inlet temp. C , or 25.d0
		static Real64 EvapOutletTemp( 0.0 ); // Evaporator water outlet temperature (C)
		static Real64 TempEvapOutDesign( 0.0 ); // design evaporator outlet temperature, water side
		static Real64 CurLoad( 0.0 );
		static int ChillerIndexPlantSide( 0 );
		static int ChillerIndexDemandSide( 0 );
		static int BranchIndexPlantSide( 0 );
		static int BranchIndexDemandSide( 0 );
		static int LoopIndexPlantSide( 0 );
		static int LoopIndexDemandSide( 0 );
		static int TypeNum( 0 );

		// Get from tower design values
		NormDsnCondFlow = 5.38e-8; //m3/s per watt (typically 3 gpm/ton)=(Volume of condenser fluid)/(ton of heat rejection)

		// Grab tower design inlet air wet bulb from setpoint manager
		Twr_DesignWB = CondEntSetPtMgr( SetPtMgrNum ).TowerDsnInletAirWetBulb;

		// Current timestep's condenser water entering setpoint
		CondWaterSetPoint = GetCurrentScheduleValue( CondEntSetPtMgr( SetPtMgrNum ).CondEntTempSchedPtr );
		LoopIndexPlantSide = CondEntSetPtMgr( SetPtMgrNum ).LoopIndexPlantSide;
		ChillerIndexPlantSide = CondEntSetPtMgr( SetPtMgrNum ).ChillerIndexPlantSide;
		BranchIndexPlantSide = CondEntSetPtMgr( SetPtMgrNum ).BranchIndexPlantSide;
		TypeNum = CondEntSetPtMgr( SetPtMgrNum ).TypeNum;
		LoopIndexDemandSide = CondEntSetPtMgr( SetPtMgrNum ).LoopIndexDemandSide;
		ChillerIndexDemandSide = CondEntSetPtMgr( SetPtMgrNum ).ChillerIndexDemandSide;
		BranchIndexDemandSide = CondEntSetPtMgr( SetPtMgrNum ).BranchIndexDemandSide;

		// If chiller is on
		CurLoad = std::abs( PlantLoop( LoopIndexPlantSide ).LoopSide( SupplySide ).Branch( BranchIndexPlantSide ).Comp( ChillerIndexPlantSide ).MyLoad );
		if ( CurLoad > 0 ) {
			if ( TypeNum == TypeOf_Chiller_Absorption || TypeNum == TypeOf_Chiller_CombTurbine || TypeNum == TypeOf_Chiller_Electric || TypeNum == TypeOf_Chiller_ElectricReformEIR || TypeNum == TypeOf_Chiller_EngineDriven ) {
				TempDesCondIn = PlantLoop( LoopIndexPlantSide ).LoopSide( SupplySide ).Branch( BranchIndexPlantSide ).Comp( ChillerIndexPlantSide ).TempDesCondIn;
				CondInletTemp = Node( PlantLoop( LoopIndexDemandSide ).LoopSide( DemandSide ).Branch( BranchIndexDemandSide ).Comp( ChillerIndexDemandSide ).NodeNumIn ).Temp;
				EvapOutletTemp = Node( PlantLoop( LoopIndexPlantSide ).LoopSide( SupplySide ).Branch( BranchIndexPlantSide ).Comp( ChillerIndexPlantSide ).NodeNumOut ).Temp;
				TempEvapOutDesign = PlantLoop( LoopIndexPlantSide ).LoopSide( SupplySide ).Branch( BranchIndexPlantSide ).Comp( ChillerIndexPlantSide ).TempDesEvapOut;
				DesignClgCapacity_Watts = PlantLoop( LoopIndexPlantSide ).LoopSide( SupplySide ).Branch( BranchIndexPlantSide ).Comp( ChillerIndexPlantSide ).MaxLoad;
				CurrentLoad_Watts = PlantReport( LoopIndexPlantSide ).CoolingDemand;
			} else if ( TypeNum == TypeOf_Chiller_Indirect_Absorption || TypeNum == TypeOf_Chiller_DFAbsorption ) {
				TempDesCondIn = PlantLoop( LoopIndexPlantSide ).LoopSide( SupplySide ).Branch( BranchIndexPlantSide ).Comp( ChillerIndexPlantSide ).TempDesCondIn;
				TempEvapOutDesign = 6.666;
			} else {
				TempDesCondIn = 25.0;
				TempEvapOutDesign = 6.666;
			}

			// for attached chillers (that are running this timestep) find their Dsn_MinCondSetpt and Dsn_EntCondTemp
			Dsn_MinCondSetpt = 999.0;
			Dsn_EntCondTemp = 0.0;

			// Design Minimum Condenser Entering as a function of the minimum lift and TEvapLvg
			// for chillers operating on current cond loop this timestep
			Dsn_CondMinThisChiller = TempEvapOutDesign + ( CondEntSetPtMgr( SetPtMgrNum ).MinimumLiftTD );
			Dsn_MinCondSetpt = min( Dsn_MinCondSetpt, Dsn_CondMinThisChiller );

			// Design entering condenser water temperature for chillers operating
			// on current cond loop this timestep
			Dsn_EntCondTemp = max( Dsn_EntCondTemp, TempDesCondIn );

			// Load this array with the design capacity and actual load of each chiller this timestep
			Des_Load = 0.0;
			Act_Load = 0.0;
			Des_Load = DesignClgCapacity_Watts;
			Act_Load = CurrentLoad_Watts;

			// ***** Load Calculations *****
			// In this section the sum of the actual load (watts) and design load (watts)
			// of the chillers that are on is calculated.
			Actual_Load_Sum += Act_Load;
			Design_Load_Sum += Des_Load;

			// Exit if the chillers are all off this hour
			if ( Actual_Load_Sum <= 0 ) {
				CondWaterSetPoint = Dsn_EntCondTemp;
				return;
			}

			// ***** Weighted Ratio Calculation *****
			// This section first calculates the actual (ALW) and design (DLW) individual
			// weights. Then the weighted actual and design loads are computed. Finally
			// the Weighted Ratio is found.
			if ( Actual_Load_Sum != 0 && Design_Load_Sum != 0 ) {
				ALW = ( ( Act_Load / Actual_Load_Sum ) * Act_Load );
				DLW = ( ( Des_Load / Design_Load_Sum ) * Des_Load );
			} else {
				ALW = 0.0;
				DLW = 0.0;
			}
			Weighted_Actual_Load_Sum += ALW;
			Weighted_Design_Load_Sum += DLW;
			Weighted_Ratio = Weighted_Actual_Load_Sum / Weighted_Design_Load_Sum;

			// ***** Optimal Temperature Calculation *****
			// In this section the optimal temperature is computed along with the minimum
			// design wet bulb temp and the mimimum actual wet bulb temp.
			// Min_DesignWB = ACoef1 + ACoef2*OaWb + ACoef3*WPLR + ACoef4*TwrDsnWB + ACoef5*NF
			Min_DesignWB = CurveValue( CondEntSetPtMgr( SetPtMgrNum ).MinTwrWbCurve, OutWetBulbTemp, Weighted_Ratio, Twr_DesignWB, NormDsnCondFlow );

			// Min_ActualWb = BCoef1 + BCoef2*MinDsnWB + BCoef3*WPLR + BCoef4*TwrDsnWB + BCoef5*NF
			Min_ActualWb = CurveValue( CondEntSetPtMgr( SetPtMgrNum ).MinOaWbCurve, Min_DesignWB, Weighted_Ratio, Twr_DesignWB, NormDsnCondFlow );

			// Opt_CondEntTemp = CCoef1 + CCoef2*OaWb + CCoef3*WPLR + CCoef4*TwrDsnWB + CCoef5*NF
			Opt_CondEntTemp = CurveValue( CondEntSetPtMgr( SetPtMgrNum ).OptCondEntCurve, OutWetBulbTemp, Weighted_Ratio, Twr_DesignWB, NormDsnCondFlow );

			// ***** Calculate (Cond ent - Evap lvg) Section *****
			// In this section we find the worst case of (Cond ent - Evap lvg) for the
			// chillers that are running.
			Cur_MinLiftTD = 9999.0;
			temp_MinLiftTD = 20.0 / 1.8;
			temp_MinLiftTD = CondInletTemp - EvapOutletTemp;
			Cur_MinLiftTD = min( Cur_MinLiftTD, temp_MinLiftTD );
		}

		// ***** Limit conditions Section *****
		// Check for limit conditions and control to the proper value.
		if ( ( Weighted_Ratio >= 0.90 ) && ( Opt_CondEntTemp >= ( Dsn_EntCondTemp + 1.0 ) ) ) {
			// Optimized value exceeds the design condenser entering condition or chillers
			// near full load condition; reset condenser entering setpoint to its design value
			SetPoint = Dsn_EntCondTemp + 1.0;
		} else {
			if ( ( OutWetBulbTemp >= Min_ActualWb ) && ( Twr_DesignWB >= Min_DesignWB ) && ( Cur_MinLiftTD > CondEntSetPtMgr( SetPtMgrNum ).MinimumLiftTD ) ) {
				// Boundaries are satified; use optimized condenser entering water temp
				SetPoint = Opt_CondEntTemp;
			} else {
				//Boundaries violated; Reset to scheduled value of condenser water entering setpoint
				SetPoint = CondWaterSetPoint;
			}
		}
		// Do not allow new setpoint to be less than the design condenser minimum entering condition,
		// i.e., TCondWaterEnt not allowed to be less than DsnEvapWaterLvg + MinimumLiftTD
		CondWaterSetPoint = max( SetPoint, Dsn_MinCondSetpt );
		CondEntSetPtMgr( SetPtMgrNum ).SetPt = CondWaterSetPoint;

	}

	void
	CalcIdealCondEntSetPoint( int const SetPtMgrNum ) // number of the current set point manager being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Heejin Cho, PNNL
		//       DATE WRITTEN   March 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate the optimal condenser water entering temperature set point for a chiller plant.

		// METHODOLOGY EMPLOYED:
		// The �ideal� chiller-tower optimization scheme uses a search algorithm to find the ideal optimal setpoint
		// at a given timestep. This requires resimulating HVAC systems at each timestep until finding
		// an �optimal� condenser water entering setpoint (OptSetpoint) which gives the minimum total chiller,
		// cooling tower, chilled water pump and condenser water pump power consumption.
		// The OptSetpoint falls between realistic minimum and maximum boundaries, which are set by the user.
		// The minimum boundary is determined based on the minimum lift (user input)
		// and evaporator leaving water temperature. The maximum boundary is specified by the user.
		// It is assumed that a single minimum point exists between these boundaries.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataPlant;
		using DataLoopNode::Node;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static Real64 CondWaterSetPoint( 0.0 ); // Condenser entering water temperature setpoint this timestep, C
		static Real64 InitCondWaterSetPoint( 0.0 ); // Initial condenser entering water temperature setpoint this timestep, C
		static Real64 EvapOutletTemp( 0.0 ); // Evaporator water outlet temperature (C)
		static Real64 CondTempLimit( 0.0 ); // Condenser entering water temperature setpoint lower limit
		static Real64 CurLoad( 0.0 ); // Current cooling load, W
		static Real64 MinLiftTD( 0.0 ); // Minimum lift (Tcond entering - Tevap leaving) TD this timestep
		static int ChillerTypeNum( 0 ); // Chiller type number
		static int ChillerLoopNum( 0 ); // Chiller loop number
		static int ChillerBranchNum( 0 ); // Chiller branch number
		static int ChillerNum( 0 ); // Chiller number
		static int TowerLoopNum( 0 ); // Tower loop number
		static int CondLoopNum( 0 ); // Condenser loop number
		static int TowerBranchNum( 0 ); // Tower branch number
		static int TowerNum( 0 ); // Tower number
		static int ChilledPumpBranchNum( 0 ); // Chilled water pump branch number
		static int ChilledPumpNum( 0 ); // Chilled water pump number
		static int CondPumpBranchNum( 0 ); // Condenser water pump branch number
		static int CondPumpNum( 0 ); // Condenser pump number
		//static int VarNum( 0 ); // Metered variable number
		//static int VarType( 0 ); // Metered variable type number
		//static int VarIndex( 0 ); // Metered variable index
		static Real64 DeltaTotEnergy( 0.0 ); // Difference between total energy consumptions at this time step
		// and at the previous time step
		static Real64 ChillerEnergy( 0.0 ); // Chiller energy consumption
		static Real64 ChilledPumpEnergy( 0.0 ); // Chilled water pump energy consumption
		static Real64 TowerFanEnergy( 0.0 ); // Colling tower fan energy consumption
		static Real64 CondPumpEnergy( 0.0 ); // Condenser water pump energy consumption
		static Real64 TotEnergy( 0.0 ); // Totoal energy consumptions at this time step
		static Real64 TotEnergyPre( 0.0 ); // Totoal energy consumptions at the previous time step
		static bool RunSubOptCondEntTemp( false );
		static bool RunFinalOptCondEntTemp( false );
		static bool firstTime( true );
		static FArray1D_bool SetupIdealCondEntSetPtVars;

		//! Current timestep's condenser water entering setpoint
		if ( firstTime ) {
			SetupIdealCondEntSetPtVars.dimension( NumIdealCondEntSetPtMgrs, true );
			firstTime = false;
		}

		InitCondWaterSetPoint = IdealCondEntSetPtMgr( SetPtMgrNum ).MaxCondEntTemp;
		MinLiftTD = IdealCondEntSetPtMgr( SetPtMgrNum ).MinimumLiftTD;
		ChillerTypeNum = IdealCondEntSetPtMgr( SetPtMgrNum ).TypeNum;
		ChillerLoopNum = IdealCondEntSetPtMgr( SetPtMgrNum ).LoopIndexPlantSide;
		ChillerBranchNum = IdealCondEntSetPtMgr( SetPtMgrNum ).BranchIndexPlantSide;
		ChillerNum = IdealCondEntSetPtMgr( SetPtMgrNum ).ChillerIndexPlantSide;
		TowerLoopNum = IdealCondEntSetPtMgr( SetPtMgrNum ).CondLoopNum;
		CondLoopNum = IdealCondEntSetPtMgr( SetPtMgrNum ).CondLoopNum;
		TowerBranchNum = IdealCondEntSetPtMgr( SetPtMgrNum ).CondBranchNum;
		TowerNum = IdealCondEntSetPtMgr( SetPtMgrNum ).TowerNum;
		ChilledPumpBranchNum = IdealCondEntSetPtMgr( SetPtMgrNum ).ChilledPumpBranchNum;
		ChilledPumpNum = IdealCondEntSetPtMgr( SetPtMgrNum ).ChilledPumpNum;
		CondPumpBranchNum = IdealCondEntSetPtMgr( SetPtMgrNum ).CondPumpBranchNum;
		CondPumpNum = IdealCondEntSetPtMgr( SetPtMgrNum ).CondPumpNum;

		if ( MetersHaveBeenInitialized ) {
			// Setup meter vars
			if ( SetupIdealCondEntSetPtVars( SetPtMgrNum ) ) {
				SetupMeteredVarsForSetPt( SetPtMgrNum );
				SetupIdealCondEntSetPtVars( SetPtMgrNum ) = false;
			}
		}

		if ( MetersHaveBeenInitialized && RunOptCondEntTemp ) {

			// If chiller is on
			CurLoad = std::abs( PlantLoop( ChillerLoopNum ).LoopSide( SupplySide ).Branch( ChillerBranchNum ).Comp( ChillerNum ).MyLoad );

			if ( CurLoad > 0 ) {
				// Calculate the minimum condenser inlet temperature boundry for set point
				if ( ChillerTypeNum == TypeOf_Chiller_Absorption || ChillerTypeNum == TypeOf_Chiller_CombTurbine || ChillerTypeNum == TypeOf_Chiller_Electric || ChillerTypeNum == TypeOf_Chiller_ElectricReformEIR || ChillerTypeNum == TypeOf_Chiller_EngineDriven ) {
					EvapOutletTemp = Node( PlantLoop( ChillerLoopNum ).LoopSide( SupplySide ).Branch( ChillerBranchNum ).Comp( ChillerNum ).NodeNumOut ).Temp;
				} else {
					EvapOutletTemp = 6.666;
				}
				CondTempLimit = MinLiftTD + EvapOutletTemp;

				// Energy consumption metered variable number = 1

				// Get the chiller energy consumption
				ChillerEnergy = GetInternalVariableValue( IdealCondEntSetPtMgr( SetPtMgrNum ).ChllrVarType, IdealCondEntSetPtMgr( SetPtMgrNum ).ChllrVarIndex );

				// Get the chilled water pump energy consumption
				ChilledPumpEnergy = GetInternalVariableValue( IdealCondEntSetPtMgr( SetPtMgrNum ).ChlPumpVarType, IdealCondEntSetPtMgr( SetPtMgrNum ).ChlPumpVarIndex );

				// Get the cooling tower fan energy consumption
				TowerFanEnergy = GetInternalVariableValue( IdealCondEntSetPtMgr( SetPtMgrNum ).ClTowerVarType, IdealCondEntSetPtMgr( SetPtMgrNum ).ClTowerVarIndex );

				// Get the condenser pump energy consumption
				CondPumpEnergy = GetInternalVariableValue( IdealCondEntSetPtMgr( SetPtMgrNum ).CndPumpVarType, IdealCondEntSetPtMgr( SetPtMgrNum ).CndPumpVarIndex );

				// Calculate the total energy consumption
				TotEnergy = ChillerEnergy + ChilledPumpEnergy + TowerFanEnergy + CondPumpEnergy;

				if ( TotEnergyPre != 0.0 ) {
					DeltaTotEnergy = 0.0;
					// Calculate the total energy consumption difference
					DeltaTotEnergy = TotEnergyPre - TotEnergy;
					// Search for the minimum total energy consumption
					if ( ( DeltaTotEnergy > 0 ) && ( CondWaterSetPoint >= CondTempLimit ) && ( ! RunFinalOptCondEntTemp ) ) {
						if ( ! RunSubOptCondEntTemp ) {
							--CondWaterSetPoint;
							RunOptCondEntTemp = true;
						} else {
							CondWaterSetPoint -= 0.2;
							RunOptCondEntTemp = true;
						}
						TotEnergyPre = TotEnergy;
						// Set smaller set point (0.2 degC) decrease
					} else if ( ( DeltaTotEnergy < 0 ) && ( ! RunSubOptCondEntTemp ) && ( CondWaterSetPoint > CondTempLimit ) && ( ! RunFinalOptCondEntTemp ) ) {
						CondWaterSetPoint += 0.8;
						RunOptCondEntTemp = true;
						RunSubOptCondEntTemp = true;
					} else {
						if ( ! RunFinalOptCondEntTemp ) {
							CondWaterSetPoint += 0.2;
							RunOptCondEntTemp = true;
							RunSubOptCondEntTemp = false;
							RunFinalOptCondEntTemp = true;
						} else {
							//CondWaterSetPoint = CondWaterSetPoint; // Self-assignment commented out
							TotEnergyPre = 0.0;
							RunOptCondEntTemp = false;
							RunSubOptCondEntTemp = false;
							RunFinalOptCondEntTemp = false;
						}
					}
				} else {
					CondWaterSetPoint = InitCondWaterSetPoint - 1.0;
					TotEnergyPre = TotEnergy;
					RunOptCondEntTemp = true;
					RunSubOptCondEntTemp = false;
				}
			} else {
				CondWaterSetPoint = InitCondWaterSetPoint;
				TotEnergyPre = 0.0;
				RunOptCondEntTemp = false;
				RunSubOptCondEntTemp = false;
			}
		} else {
			CondWaterSetPoint = InitCondWaterSetPoint;
			RunOptCondEntTemp = false;
			RunSubOptCondEntTemp = false;
		}

		IdealCondEntSetPtMgr( SetPtMgrNum ).SetPt = CondWaterSetPoint;

	}

	void
	SetupMeteredVarsForSetPt( int const SetPtMgrNum ) // number of this setpoint manager (only Ideal Cond Reset)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   Sep 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// For the Ideal Cond reset setpoint manager, this sets up the
		// report variables used during the calculation.

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataPlant;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		std::string TypeOfComp;
		std::string NameOfComp;

		FArray1D_int VarIndexes; // Variable Numbers
		FArray1D_int VarTypes; // Variable Types (1=integer, 2=real, 3=meter)
		FArray1D_int IndexTypes; // Variable Index Types (1=Zone,2=HVAC)
		FArray1D_string UnitsStrings; // UnitsStrings for each variable
		FArray1D_int ResourceTypes; // ResourceTypes for each variable
		FArray1D_string EndUses; // EndUses for each variable
		FArray1D_string Groups; // Groups for each variable
		FArray1D_string Names; // Variable Names for each variable
		int NumVariables;
		int NumFound;

		static int ChillerTypeNum( 0 ); // Chiller type number
		static int ChillerLoopNum( 0 ); // Chiller loop number
		static int ChillerBranchNum( 0 ); // Chiller branch number
		static int ChillerNum( 0 ); // Chiller number
		static int TowerLoopNum( 0 ); // Tower loop number
		static int TowerBranchNum( 0 ); // Tower branch number
		static int CondLoopNum( 0 ); // Condenser loop number
		static int TowerNum( 0 ); // Tower number
		static int ChilledPumpBranchNum( 0 ); // Chilled water pump branch number
		static int ChilledPumpNum( 0 ); // Chilled water pump number
		static int CondPumpBranchNum( 0 ); // Condenser water pump branch number
		static int CondPumpNum( 0 ); // Condenser pump number

		ChillerTypeNum = IdealCondEntSetPtMgr( SetPtMgrNum ).TypeNum;
		ChillerLoopNum = IdealCondEntSetPtMgr( SetPtMgrNum ).LoopIndexPlantSide;
		ChillerBranchNum = IdealCondEntSetPtMgr( SetPtMgrNum ).BranchIndexPlantSide;
		ChillerNum = IdealCondEntSetPtMgr( SetPtMgrNum ).ChillerIndexPlantSide;
		TowerLoopNum = IdealCondEntSetPtMgr( SetPtMgrNum ).CondLoopNum;
		CondLoopNum = IdealCondEntSetPtMgr( SetPtMgrNum ).CondLoopNum;
		TowerBranchNum = IdealCondEntSetPtMgr( SetPtMgrNum ).CondBranchNum;
		TowerNum = IdealCondEntSetPtMgr( SetPtMgrNum ).TowerNum;
		ChilledPumpBranchNum = IdealCondEntSetPtMgr( SetPtMgrNum ).ChilledPumpBranchNum;
		ChilledPumpNum = IdealCondEntSetPtMgr( SetPtMgrNum ).ChilledPumpNum;
		CondPumpBranchNum = IdealCondEntSetPtMgr( SetPtMgrNum ).CondPumpBranchNum;
		CondPumpNum = IdealCondEntSetPtMgr( SetPtMgrNum ).CondPumpNum;

		TypeOfComp = PlantLoop( ChillerLoopNum ).LoopSide( SupplySide ).Branch( ChillerBranchNum ).Comp( ChillerNum ).TypeOf;
		NameOfComp = PlantLoop( ChillerLoopNum ).LoopSide( SupplySide ).Branch( ChillerBranchNum ).Comp( ChillerNum ).Name;
		NumVariables = GetNumMeteredVariables( TypeOfComp, NameOfComp );
		VarIndexes.allocate( NumVariables );
		VarTypes.allocate( NumVariables );
		IndexTypes.allocate( NumVariables );
		UnitsStrings.allocate( NumVariables );
		ResourceTypes.allocate( NumVariables );
		EndUses.allocate( NumVariables );
		Groups.allocate( NumVariables );
		Names.allocate( NumVariables );

		GetMeteredVariables( TypeOfComp, NameOfComp, VarIndexes, VarTypes, IndexTypes, UnitsStrings, ResourceTypes, EndUses, Groups, Names, NumFound );
		IdealCondEntSetPtMgr( SetPtMgrNum ).ChllrVarType = VarTypes( 1 );
		IdealCondEntSetPtMgr( SetPtMgrNum ).ChllrVarIndex = VarIndexes( 1 );

		VarIndexes.deallocate();
		VarTypes.deallocate();
		IndexTypes.deallocate();
		UnitsStrings.deallocate();
		ResourceTypes.deallocate();
		EndUses.deallocate();
		Groups.deallocate();
		Names.deallocate();

		TypeOfComp = PlantLoop( ChillerLoopNum ).LoopSide( SupplySide ).Branch( ChilledPumpBranchNum ).Comp( ChilledPumpNum ).TypeOf;
		NameOfComp = PlantLoop( ChillerLoopNum ).LoopSide( SupplySide ).Branch( ChilledPumpBranchNum ).Comp( ChilledPumpNum ).Name;
		NumVariables = GetNumMeteredVariables( TypeOfComp, NameOfComp );
		VarIndexes.allocate( NumVariables );
		VarTypes.allocate( NumVariables );
		IndexTypes.allocate( NumVariables );
		UnitsStrings.allocate( NumVariables );
		ResourceTypes.allocate( NumVariables );
		EndUses.allocate( NumVariables );
		Groups.allocate( NumVariables );
		Names.allocate( NumVariables );

		GetMeteredVariables( TypeOfComp, NameOfComp, VarIndexes, VarTypes, IndexTypes, UnitsStrings, ResourceTypes, EndUses, Groups, Names, NumFound );
		IdealCondEntSetPtMgr( SetPtMgrNum ).ChlPumpVarType = VarTypes( 1 );
		IdealCondEntSetPtMgr( SetPtMgrNum ).ChlPumpVarIndex = VarIndexes( 1 );

		VarIndexes.deallocate();
		VarTypes.deallocate();
		IndexTypes.deallocate();
		UnitsStrings.deallocate();
		ResourceTypes.deallocate();
		EndUses.deallocate();
		Groups.deallocate();
		Names.deallocate();

		TypeOfComp = PlantLoop( TowerLoopNum ).LoopSide( SupplySide ).Branch( TowerBranchNum ).Comp( TowerNum ).TypeOf;
		NameOfComp = PlantLoop( TowerLoopNum ).LoopSide( SupplySide ).Branch( TowerBranchNum ).Comp( TowerNum ).Name;
		NumVariables = GetNumMeteredVariables( TypeOfComp, NameOfComp );
		VarIndexes.allocate( NumVariables );
		VarTypes.allocate( NumVariables );
		IndexTypes.allocate( NumVariables );
		UnitsStrings.allocate( NumVariables );
		ResourceTypes.allocate( NumVariables );
		EndUses.allocate( NumVariables );
		Groups.allocate( NumVariables );
		Names.allocate( NumVariables );

		GetMeteredVariables( TypeOfComp, NameOfComp, VarIndexes, VarTypes, IndexTypes, UnitsStrings, ResourceTypes, EndUses, Groups, Names, NumFound );
		IdealCondEntSetPtMgr( SetPtMgrNum ).ClTowerVarType = VarTypes( 1 );
		IdealCondEntSetPtMgr( SetPtMgrNum ).ClTowerVarIndex = VarIndexes( 1 );

		VarIndexes.deallocate();
		VarTypes.deallocate();
		IndexTypes.deallocate();
		UnitsStrings.deallocate();
		ResourceTypes.deallocate();
		EndUses.deallocate();
		Groups.deallocate();
		Names.deallocate();

		TypeOfComp = PlantLoop( CondLoopNum ).LoopSide( SupplySide ).Branch( CondPumpBranchNum ).Comp( CondPumpNum ).TypeOf;
		NameOfComp = PlantLoop( CondLoopNum ).LoopSide( SupplySide ).Branch( CondPumpBranchNum ).Comp( CondPumpNum ).Name;
		NumVariables = GetNumMeteredVariables( TypeOfComp, NameOfComp );
		VarIndexes.allocate( NumVariables );
		VarTypes.allocate( NumVariables );
		IndexTypes.allocate( NumVariables );
		UnitsStrings.allocate( NumVariables );
		ResourceTypes.allocate( NumVariables );
		EndUses.allocate( NumVariables );
		Groups.allocate( NumVariables );
		Names.allocate( NumVariables );

		GetMeteredVariables( TypeOfComp, NameOfComp, VarIndexes, VarTypes, IndexTypes, UnitsStrings, ResourceTypes, EndUses, Groups, Names, NumFound );
		IdealCondEntSetPtMgr( SetPtMgrNum ).CndPumpVarType = VarTypes( 1 );
		IdealCondEntSetPtMgr( SetPtMgrNum ).CndPumpVarIndex = VarIndexes( 1 );

		VarIndexes.deallocate();
		VarTypes.deallocate();
		IndexTypes.deallocate();
		UnitsStrings.deallocate();
		ResourceTypes.deallocate();
		EndUses.deallocate();
		Groups.deallocate();
		Names.deallocate();

	}

	void
	UpdateSetPointManagers()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   July 1998
		//       MODIFIED       Shirey/Raustad (FSEC), Jan 2004
		//                      P. Haves Oct 2004
		//                        Add new setpoint managers:
		//                          SET POINT MANAGER:WARMEST TEMP FLOW and
		//                          SET POINT MANAGER:COLDEST TEMP FLOW
		//                      Nov 2004 M. J. Witte, GARD Analytics, Inc.
		//                        Add new setpoint managers:
		//                          SET POINT MANAGER:SINGLE ZONE HEATING and
		//                          SET POINT MANAGER:SINGLE ZONE COOLING
		//                        Work supported by ASHRAE research project 1254-RP
		//                      B. Griffith Aug. 2006.  Allow HUMRAT for scheduled setpoint manager
		//                      P. Haves Aug 2007
		//                        SET POINT MANAGER:WARMEST TEMP FLOW:
		//                          Set AirLoopControlInfo()%LoopFlowRateSet every call not just on
		//                          initialization (flag now reset in SUBROUTINE ResetHVACControl)
		//                        Removed SET POINT MANAGER:COLDEST TEMP FLOW
		//                      July 2010 B.A. Nigusse, FSEC/UCF
		//                        Added new setpoint managers
		//                          SetpointManager:MultiZone:Heating:Average
		//                          SetpointManager:MultiZone:Cooling:Average
		//                          SetpointManager:MultiZone:MinimumHumidity:Average
		//                          SetpointManager:MultiZone:MaximumHumidity:Average
		//                      Aug 2010 B.A. Nigusse, FSEC/UCF
		//                        Added new setpoint managers:
		//                          SetpointManager:MultiZone:Humidity:Minimum
		//                          SetpointManager:MultiZone:Humidity:Maximum

		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE
		// Loop over all the Setpoint Managers and use their output arrays
		// to set the node setpoints.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::SysSizingCalc;
		using DataGlobals::AnyEnergyManagementSystemInModel;
		using DataHVACGlobals::DoSetPointTest;
		using DataHVACGlobals::SetPointErrorFlag;
		using EMSManager::iTemperatureSetPoint;
		using EMSManager::CheckIfNodeSetPointManagedByEMS;

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int SetPtMgrNum;
		int CtrlNodeIndex;
		int NodeNum;

		// Loop over all the Scheduled Setpoint Managers

		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumSchSetPtMgrs; ++SetPtMgrNum ) {

			for ( CtrlNodeIndex = 1; CtrlNodeIndex <= SchSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) { // Loop over the list of nodes wanting
				// setpoints from this setpoint manager
				NodeNum = SchSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number

				{ auto const SELECT_CASE_var( SchSetPtMgr( SetPtMgrNum ).CtrlTypeMode );
				// set the setpoint depending on the type of variable being controlled
				if ( SELECT_CASE_var == iCtrlVarType_Temp ) {
					Node( NodeNum ).TempSetPoint = SchSetPtMgr( SetPtMgrNum ).SetPt;
				} else if ( SELECT_CASE_var == iCtrlVarType_MaxTemp ) {
					Node( NodeNum ).TempSetPointHi = SchSetPtMgr( SetPtMgrNum ).SetPt;
				} else if ( SELECT_CASE_var == iCtrlVarType_MinTemp ) {
					Node( NodeNum ).TempSetPointLo = SchSetPtMgr( SetPtMgrNum ).SetPt;
				} else if ( SELECT_CASE_var == iCtrlVarType_HumRat ) {
					Node( NodeNum ).HumRatSetPoint = SchSetPtMgr( SetPtMgrNum ).SetPt;
				} else if ( SELECT_CASE_var == iCtrlVarType_MaxHumRat ) {
					Node( NodeNum ).HumRatMax = SchSetPtMgr( SetPtMgrNum ).SetPt;
				} else if ( SELECT_CASE_var == iCtrlVarType_MinHumRat ) {
					Node( NodeNum ).HumRatMin = SchSetPtMgr( SetPtMgrNum ).SetPt;
				} else if ( SELECT_CASE_var == iCtrlVarType_MassFlow ) {
					Node( NodeNum ).MassFlowRateSetPoint = SchSetPtMgr( SetPtMgrNum ).SetPt;
				} else if ( SELECT_CASE_var == iCtrlVarType_MaxMassFlow ) {
					Node( NodeNum ).MassFlowRateMax = SchSetPtMgr( SetPtMgrNum ).SetPt;
				} else if ( SELECT_CASE_var == iCtrlVarType_MinMassFlow ) {
					Node( NodeNum ).MassFlowRateMin = SchSetPtMgr( SetPtMgrNum ).SetPt;
				}}

			} //nodes in list

		} // setpoint manger:scheduled

		// Loop over all the Scheduled Dual Setpoint Managers

		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumDualSchSetPtMgrs; ++SetPtMgrNum ) {

			for ( CtrlNodeIndex = 1; CtrlNodeIndex <= DualSchSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) { // Loop over the list of nodes wanting
				// setpoints from this setpoint manager
				NodeNum = DualSchSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number

				if ( DualSchSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_Temp ) {
					Node( NodeNum ).TempSetPointHi = DualSchSetPtMgr( SetPtMgrNum ).SetPtHi; // Set the setpoint High
					Node( NodeNum ).TempSetPointLo = DualSchSetPtMgr( SetPtMgrNum ).SetPtLo; // Set the setpoint Low
					Node( NodeNum ).TempSetPoint = ( Node( NodeNum ).TempSetPointHi + Node( NodeNum ).TempSetPointLo ) / 2.0; // average of the high and low
				}

			}

		}

		// Loop over all the Outside Air Setpoint Managers

		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumOutAirSetPtMgrs; ++SetPtMgrNum ) {

			for ( CtrlNodeIndex = 1; CtrlNodeIndex <= OutAirSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) { // Loop over the list of nodes wanting
				// setpoints from this setpoint manager
				NodeNum = OutAirSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number

				if ( OutAirSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_Temp ) {
					Node( NodeNum ).TempSetPoint = OutAirSetPtMgr( SetPtMgrNum ).SetPt; // Set the setpoint
				}

			}

		}

		// Loop over all the Single Zone Reheat Setpoint Managers

		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumSZRhSetPtMgrs; ++SetPtMgrNum ) {

			for ( CtrlNodeIndex = 1; CtrlNodeIndex <= SingZoneRhSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) { // Loop over the list of nodes wanting
				// setpoints from this setpoint manager
				NodeNum = SingZoneRhSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number

				if ( SingZoneRhSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_Temp ) {
					Node( NodeNum ).TempSetPoint = SingZoneRhSetPtMgr( SetPtMgrNum ).SetPt; // Set the setpoint
				}

			}

		}

		// Loop over all the Single Zone Heating Setpoint Managers

		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumSZHtSetPtMgrs; ++SetPtMgrNum ) {

			for ( CtrlNodeIndex = 1; CtrlNodeIndex <= SingZoneHtSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) { // Loop over the list of nodes wanting
				// setpoints from this setpoint manager
				NodeNum = SingZoneHtSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number

				if ( SingZoneHtSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_Temp ) {
					Node( NodeNum ).TempSetPoint = SingZoneHtSetPtMgr( SetPtMgrNum ).SetPt; // Set the setpoint
				}

			}

		}

		// Loop over all the Single Zone Cooling Setpoint Managers

		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumSZClSetPtMgrs; ++SetPtMgrNum ) {

			for ( CtrlNodeIndex = 1; CtrlNodeIndex <= SingZoneClSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) { // Loop over the list of nodes wanting
				// setpoints from this setpoint manager
				NodeNum = SingZoneClSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number

				if ( SingZoneClSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_Temp ) {
					Node( NodeNum ).TempSetPoint = SingZoneClSetPtMgr( SetPtMgrNum ).SetPt; // Set the setpoint
				}

			}

		}

		// Loop over all the Single Zone Minimum Humidity Setpoint Managers

		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumSZMinHumSetPtMgrs; ++SetPtMgrNum ) {

			for ( CtrlNodeIndex = 1; CtrlNodeIndex <= SZMinHumSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) { // Loop over the list of nodes wanting
				// setpoints from this setpoint manager
				NodeNum = SZMinHumSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number

				Node( NodeNum ).HumRatMin = SZMinHumSetPtMgr( SetPtMgrNum ).SetPt; // Set the setpoint

			}

		}

		// Loop over all the Single Zone Maximum Humidity Setpoint Managers

		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumSZMaxHumSetPtMgrs; ++SetPtMgrNum ) {

			for ( CtrlNodeIndex = 1; CtrlNodeIndex <= SZMaxHumSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) { // Loop over the list of nodes wanting
				// setpoints from this setpoint manager
				NodeNum = SZMaxHumSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number

				Node( NodeNum ).HumRatMax = SZMaxHumSetPtMgr( SetPtMgrNum ).SetPt; // Set the setpoint

			}

		}

		// Loop over all the Warmest Setpoint Managers

		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumWarmestSetPtMgrs; ++SetPtMgrNum ) {

			for ( CtrlNodeIndex = 1; CtrlNodeIndex <= WarmestSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) { // Loop over the list of nodes wanting
				// setpoints from this setpoint manager
				NodeNum = WarmestSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number

				if ( WarmestSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_Temp ) {
					Node( NodeNum ).TempSetPoint = WarmestSetPtMgr( SetPtMgrNum ).SetPt; // Set the setpoint
				}

			}

		}

		// Loop over all the Coldest Setpoint Managers

		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumColdestSetPtMgrs; ++SetPtMgrNum ) {

			for ( CtrlNodeIndex = 1; CtrlNodeIndex <= ColdestSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) { // Loop over the list of nodes wanting
				// setpoints from this setpoint manager
				NodeNum = ColdestSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number

				if ( ColdestSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_Temp ) {
					Node( NodeNum ).TempSetPoint = ColdestSetPtMgr( SetPtMgrNum ).SetPt; // Set the setpoint
				}

			}

		}

		// Loop over all the Warmest Temp Flow Setpoint Managers

		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumWarmestSetPtMgrsTempFlow; ++SetPtMgrNum ) {

			for ( CtrlNodeIndex = 1; CtrlNodeIndex <= WarmestSetPtMgrTempFlow( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) { // Loop over the list of nodes wanting
				// setpoints from this setpoint manager
				NodeNum = WarmestSetPtMgrTempFlow( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number

				if ( WarmestSetPtMgrTempFlow( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_Temp ) {
					Node( NodeNum ).TempSetPoint = WarmestSetPtMgrTempFlow( SetPtMgrNum ).SetPt; // Set the supply air temperature setpoint
					AirLoopFlow( WarmestSetPtMgrTempFlow( SetPtMgrNum ).AirLoopNum ).ReqSupplyFrac = WarmestSetPtMgrTempFlow( SetPtMgrNum ).Turndown; // Set the supply air flow rate
					AirLoopControlInfo( WarmestSetPtMgrTempFlow( SetPtMgrNum ).AirLoopNum ).LoopFlowRateSet = true; // PH 8/17/07
				}

			}

		}

		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumRABFlowSetPtMgrs; ++SetPtMgrNum ) {

			NodeNum = RABFlowSetPtMgr( SetPtMgrNum ).RABSplitOutNode; // Get the node number

			if ( RABFlowSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_MassFlow ) {
				Node( NodeNum ).MassFlowRateSetPoint = RABFlowSetPtMgr( SetPtMgrNum ).FlowSetPt; // Set the flow setpoint
			}

		}

		// Loop over all the MultiZone Average Cooling Setpoint Managers
		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumMZClgAverageSetPtMgrs; ++SetPtMgrNum ) {
			for ( CtrlNodeIndex = 1; CtrlNodeIndex <= MZAverageCoolingSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) { // Loop over the list of nodes wanting
				//  setpoints from this setpoint manager
				NodeNum = MZAverageCoolingSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number
				if ( MZAverageCoolingSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_Temp ) {
					Node( NodeNum ).TempSetPoint = MZAverageCoolingSetPtMgr( SetPtMgrNum ).SetPt; // Set the temperature setpoint
				}
			}
		}

		// Loop over all the MultiZone Average Heating Setpoint Managers
		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumMZHtgAverageSetPtMgrs; ++SetPtMgrNum ) {
			for ( CtrlNodeIndex = 1; CtrlNodeIndex <= MZAverageHeatingSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) { // Loop over the list of nodes wanting
				//  setpoints from this setpoint manager
				NodeNum = MZAverageHeatingSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number
				if ( MZAverageHeatingSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_Temp ) {
					Node( NodeNum ).TempSetPoint = MZAverageHeatingSetPtMgr( SetPtMgrNum ).SetPt; // Set the temperature setpoint
				}
			}
		}

		// Loop over all the MultiZone Average Minimum Humidity Setpoint Managers
		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumMZAverageMinHumSetPtMgrs; ++SetPtMgrNum ) {
			for ( CtrlNodeIndex = 1; CtrlNodeIndex <= MZAverageMinHumSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) { // Loop over the list of nodes wanting
				//  setpoints from this setpoint manager
				NodeNum = MZAverageMinHumSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number
				if ( MZAverageMinHumSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_MinHumRat ) {
					Node( NodeNum ).HumRatMin = MZAverageMinHumSetPtMgr( SetPtMgrNum ).SetPt; // Set the humidity ratio setpoint
				}
			}
		}

		// Loop over all the MultiZone Average Maxiumum Humidity Setpoint Managers
		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumMZAverageMaxHumSetPtMgrs; ++SetPtMgrNum ) {
			for ( CtrlNodeIndex = 1; CtrlNodeIndex <= MZAverageMaxHumSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) { // Loop over the list of nodes wanting
				//  setpoints from this setpoint manager
				NodeNum = MZAverageMaxHumSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number
				if ( MZAverageMaxHumSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_MaxHumRat ) {
					Node( NodeNum ).HumRatMax = MZAverageMaxHumSetPtMgr( SetPtMgrNum ).SetPt; // Set the humidity ratio setpoint
				}
			}
		}

		// Loop over all the Multizone Minimum Humidity Ratio Setpoint Managers
		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumMZMinHumSetPtMgrs; ++SetPtMgrNum ) {
			for ( CtrlNodeIndex = 1; CtrlNodeIndex <= MZMinHumSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) { // Loop over the list of nodes wanting
				//  setpoints from this setpoint manager
				NodeNum = MZMinHumSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number
				if ( MZMinHumSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_MinHumRat ) {
					Node( NodeNum ).HumRatMin = MZMinHumSetPtMgr( SetPtMgrNum ).SetPt; // Set the humidity ratio setpoint
				}
			}
		}

		// Loop over all the Multizone Maximum Humidity Ratio Setpoint Managers
		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumMZMaxHumSetPtMgrs; ++SetPtMgrNum ) {
			for ( CtrlNodeIndex = 1; CtrlNodeIndex <= MZMaxHumSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) { // Loop over the list of nodes wanting
				//  setpoints from this setpoint manager
				NodeNum = MZMaxHumSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number
				if ( MZMaxHumSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_MaxHumRat ) {
					Node( NodeNum ).HumRatMax = MZMaxHumSetPtMgr( SetPtMgrNum ).SetPt; // Set the humidity ratio setpoint
				}
			}
		}

		// Loop over all the Follow Outdoor Air Setpoint Managers
		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumFollowOATempSetPtMgrs; ++SetPtMgrNum ) {
			for ( CtrlNodeIndex = 1; CtrlNodeIndex <= FollowOATempSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) { // Loop over the list of nodes wanting
				//  setpoints from this setpoint manager
				NodeNum = FollowOATempSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number
				if ( FollowOATempSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_Temp ) {
					Node( NodeNum ).TempSetPoint = FollowOATempSetPtMgr( SetPtMgrNum ).SetPt; // Set the temperature setpoint
				} else if ( FollowOATempSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_MaxTemp ) {
					Node( NodeNum ).TempSetPointHi = FollowOATempSetPtMgr( SetPtMgrNum ).SetPt; // Set the temperature setpoint
				} else if ( FollowOATempSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_MinTemp ) {
					Node( NodeNum ).TempSetPointLo = FollowOATempSetPtMgr( SetPtMgrNum ).SetPt; // Set the temperature setpoint
				}
			}
		}

		// Loop over all the Follow System Node Temperature Setpoint Managers
		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumFollowSysNodeTempSetPtMgrs; ++SetPtMgrNum ) {
			for ( CtrlNodeIndex = 1; CtrlNodeIndex <= FollowSysNodeTempSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) { // Loop over the list of nodes wanting
				//  setpoints from this setpoint manager
				NodeNum = FollowSysNodeTempSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number
				if ( FollowSysNodeTempSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_Temp ) {
					Node( NodeNum ).TempSetPoint = FollowSysNodeTempSetPtMgr( SetPtMgrNum ).SetPt; // Set the temperature setpoint
				} else if ( FollowSysNodeTempSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_MaxTemp ) {
					Node( NodeNum ).TempSetPointHi = FollowSysNodeTempSetPtMgr( SetPtMgrNum ).SetPt; // Set the temperature setpoint
				} else if ( FollowSysNodeTempSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_MinTemp ) {
					Node( NodeNum ).TempSetPointLo = FollowSysNodeTempSetPtMgr( SetPtMgrNum ).SetPt; // Set the temperature setpoint
				}
			}
		}

		// Loop over all the Ground Tempearture Setpoint Managers
		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumGroundTempSetPtMgrs; ++SetPtMgrNum ) {
			for ( CtrlNodeIndex = 1; CtrlNodeIndex <= GroundTempSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) { // Loop over the list of nodes wanting
				//  setpoints from this setpoint manager
				NodeNum = GroundTempSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number
				if ( GroundTempSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_Temp ) {
					Node( NodeNum ).TempSetPoint = GroundTempSetPtMgr( SetPtMgrNum ).SetPt; // Set the temperature setpoint
				} else if ( GroundTempSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_MaxTemp ) {
					Node( NodeNum ).TempSetPointHi = GroundTempSetPtMgr( SetPtMgrNum ).SetPt; // Set the temperature setpoint
				} else if ( GroundTempSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_MinTemp ) {
					Node( NodeNum ).TempSetPointLo = GroundTempSetPtMgr( SetPtMgrNum ).SetPt; // Set the temperature setpoint
				}
			}
		}

		// Loop over all Condenser Entering Set Point Managers

		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumCondEntSetPtMgrs; ++SetPtMgrNum ) {
			for ( CtrlNodeIndex = 1; CtrlNodeIndex <= CondEntSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) { // Loop over the list of nodes wanting
				//  set points from this set point manager
				NodeNum = CondEntSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number
				if ( CondEntSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_Temp ) {
					Node( NodeNum ).TempSetPoint = CondEntSetPtMgr( SetPtMgrNum ).SetPt; // Set the temperature setpoint
				}
			}
		}

		// Loop over all Ideal Condenser Entering Set Point Managers

		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumIdealCondEntSetPtMgrs; ++SetPtMgrNum ) {
			for ( CtrlNodeIndex = 1; CtrlNodeIndex <= IdealCondEntSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) { // Loop over the list of nodes wanting
				// set points from this set point manager
				NodeNum = IdealCondEntSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number
				if ( IdealCondEntSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_Temp ) {
					Node( NodeNum ).TempSetPoint = IdealCondEntSetPtMgr( SetPtMgrNum ).SetPt; // Set the temperature setpoint
				}
			}
		}

		//loop over all single zone on/off cooling setpoint managers
		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumSZOneStageCoolingSetPtMgrs; ++SetPtMgrNum ) {
			for ( CtrlNodeIndex = 1; CtrlNodeIndex <= SZOneStageCoolingSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) { // Loop over the list of nodes wanting
				// set points from this set point manager
				NodeNum = SZOneStageCoolingSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number
				if ( SZOneStageCoolingSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_Temp ) {
					Node( NodeNum ).TempSetPoint = SZOneStageCoolingSetPtMgr( SetPtMgrNum ).SetPt; // Set the temperature setpoint
				}
			}
		}

		//loop over all single zone on/off heating setpoint managers
		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumSZOneStageHeatingSetPtMgrs; ++SetPtMgrNum ) {
			for ( CtrlNodeIndex = 1; CtrlNodeIndex <= SZOneStageHeatingSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) { // Loop over the list of nodes wanting
				// set points from this set point manager
				NodeNum = SZOneStageHeatingSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number
				if ( SZOneStageHeatingSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_Temp ) {
					Node( NodeNum ).TempSetPoint = SZOneStageHeatingSetPtMgr( SetPtMgrNum ).SetPt; // Set the temperature setpoint
				}
			}
		}

	}

	void
	UpdateMixedAirSetPoints()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   May 2001
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE
		// Loop over all the Mixed Air Managers and use their output arrays
		// to set the node setpoints.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int SetPtMgrNum;
		int CtrlNodeIndex;
		int NodeNum;

		// Loop over all the Mixed Air Setpoint Managers

		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumMixedAirSetPtMgrs; ++SetPtMgrNum ) {

			for ( CtrlNodeIndex = 1; CtrlNodeIndex <= MixedAirSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) { // Loop over the list of nodes wanting
				// setpoints from this setpoint manager
				NodeNum = MixedAirSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number

				if ( MixedAirSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType_Temp ) {
					Node( NodeNum ).TempSetPoint = MixedAirSetPtMgr( SetPtMgrNum ).SetPt; // Set the setpoint
				}

			}

		}

	}

	void
	UpdateOAPretreatSetPoints()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         M. J. Witte based on UpdateMixedAirSetPoints by Fred Buhl,
		//                        Work supported by ASHRAE research project 1254-RP
		//       DATE WRITTEN   January 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE
		// Loop over all the Outside Air Pretreat Managers and use their output arrays
		// to set the node setpoints.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int SetPtMgrNum;
		int CtrlNodeIndex;
		int NodeNum;

		// Loop over all the Mixed Air Setpoint Managers

		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumOAPretreatSetPtMgrs; ++SetPtMgrNum ) {

			for ( CtrlNodeIndex = 1; CtrlNodeIndex <= OAPretreatSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) { // Loop over the list of nodes wanting
				// setpoints from this setpoint manager
				NodeNum = OAPretreatSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex ); // Get the node number

				{ auto const SELECT_CASE_var( OAPretreatSetPtMgr( SetPtMgrNum ).CtrlTypeMode );
				if ( SELECT_CASE_var == iCtrlVarType_Temp ) { // 'Temperature'
					Node( NodeNum ).TempSetPoint = OAPretreatSetPtMgr( SetPtMgrNum ).SetPt; // Set the setpoint
				} else if ( SELECT_CASE_var == iCtrlVarType_MaxHumRat ) { // 'MaximumHumidityRatio'
					Node( NodeNum ).HumRatMax = OAPretreatSetPtMgr( SetPtMgrNum ).SetPt; // Set the setpoint
				} else if ( SELECT_CASE_var == iCtrlVarType_MinHumRat ) { // 'MinimumHumidityRatio'
					Node( NodeNum ).HumRatMin = OAPretreatSetPtMgr( SetPtMgrNum ).SetPt; // Set the setpoint
				} else if ( SELECT_CASE_var == iCtrlVarType_HumRat ) { // 'HumidityRatio'
					Node( NodeNum ).HumRatSetPoint = OAPretreatSetPtMgr( SetPtMgrNum ).SetPt; // Set the setpoint
				}}

			}

		}

	}

	bool
	IsNodeOnSetPtManager(
		int const NodeNum,
		int const SetPtType
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Sankaranarayanan K P
		//       DATE WRITTEN   January 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Determines if a particular node is acted upon by a specific setpoint manager

		// METHODOLOGY EMPLOYED:
		// Cycle through all setpoint managers and find if the node passed in has a setpoint manager of passed
		// in type associated to it.

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Return value
		bool IsNodeOnSetPtManager;

		// Locals
		int SetPtMgrNum;
		int NumNode;

		// First time called, get the input for all the setpoint managers
		if ( GetInputFlag ) {
			GetSetPointManagerInputs();
			GetInputFlag = false;
		}

		IsNodeOnSetPtManager = false;

		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumSchSetPtMgrs; ++SetPtMgrNum ) {
			if ( SetPtType == SchSetPtMgr( SetPtMgrNum ).CtrlTypeMode ) {
				for ( NumNode = 1; NumNode <= SchSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++NumNode ) {
					if ( NodeNum == SchSetPtMgr( SetPtMgrNum ).CtrlNodes( NumNode ) ) {
						IsNodeOnSetPtManager = true;
						break;
					}
				}
			}
		}

		return IsNodeOnSetPtManager;

	}

	bool
	NodeHasSPMCtrlVarType(
		int const NodeNum,
		int const iCtrlVarType
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Chandan Sharma
		//       DATE WRITTEN   March 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Determines if a particular node is acted upon by a specific setpoint manager

		// METHODOLOGY EMPLOYED:
		// Cycle through all setpoint managers and find if the node has a specific control type

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// Return value
		bool NodeHasSPMCtrlVarType;

		// Locals
		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int SetPtMgrNum; // loop counter for each set point manager
		int NumNode; // loop counter for each node and specific control type

		// FLOW:

		// First time called, get the input for all the setpoint managers
		if ( GetInputFlag ) {
			GetSetPointManagerInputs();
			GetInputFlag = false;
		}

		// Initialize to false that node is not controlled by set point manager
		NodeHasSPMCtrlVarType = false;

		SPMLoop: for ( SetPtMgrNum = 1; SetPtMgrNum <= NumAllSetPtMgrs; ++SetPtMgrNum ) {
			for ( NumNode = 1; NumNode <= AllSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++NumNode ) {
				if ( NodeNum == AllSetPtMgr( SetPtMgrNum ).CtrlNodes( NumNode ) ) {
					if ( AllSetPtMgr( SetPtMgrNum ).CtrlTypeMode == iCtrlVarType ) {
						//       If specific control type is found, it doesn't matter if there are other of same type.
						NodeHasSPMCtrlVarType = true;
						goto SPMLoop_exit;
					}
				}
			}
			SPMLoop_loop: ;
		}
		SPMLoop_exit: ;

		return NodeHasSPMCtrlVarType;

	}

	void
	CheckIfAnyIdealCondEntSetPoint()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Heejin Cho, PNNL
		//       DATE WRITTEN   March 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Determine if ideal condenser entering set point manager is used in model and set flag

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using DataGlobals::AnyIdealCondEntSetPointInModel;

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

		int write_stat;
		std::string cCurrentModuleObject;

		cCurrentModuleObject = "SetpointManager:CondenserEnteringReset:Ideal";
		NumIdealCondEntSetPtMgrs = GetNumObjectsFound( cCurrentModuleObject );

		if ( NumIdealCondEntSetPtMgrs > 0 ) {
			AnyIdealCondEntSetPointInModel = true;
		} else {
			AnyIdealCondEntSetPointInModel = false;
		}

	}

	int
	GetHumidityRatioVariableType( int const CntrlNodeNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. A. Nigusse
		//       DATE WRITTEN   December 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE
		// Loop over all the humidity setpoint Managers to determine the
		// humidity ratio setpoint type

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Return value
		int HumRatCntrlType;

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int SetPtMgrNum;
		int CtrlNodeIndex;
		int NodeNum;

		if ( GetInputFlag ) {
			GetSetPointManagerInputs();
			GetInputFlag = false;
		}

		HumRatCntrlType = iCtrlVarType_HumRat;

		// Loop over the single zone maximum humidity setpoint Managers
		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumSZMaxHumSetPtMgrs; ++SetPtMgrNum ) {
			for ( CtrlNodeIndex = 1; CtrlNodeIndex <= SZMaxHumSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) {
				NodeNum = SZMaxHumSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex );
				if ( CntrlNodeNum == NodeNum ) {
					HumRatCntrlType = iCtrlVarType_MaxHumRat;
					return HumRatCntrlType;
				}
			}
		}
		// Loop over the MultiZone maximum humidity setpoint Managers
		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumMZMaxHumSetPtMgrs; ++SetPtMgrNum ) {
			for ( CtrlNodeIndex = 1; CtrlNodeIndex <= MZMaxHumSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) {
				NodeNum = MZMaxHumSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex );
				if ( CntrlNodeNum == NodeNum ) {
					HumRatCntrlType = iCtrlVarType_MaxHumRat;
					return HumRatCntrlType;
				}
			}
		}
		// Loop over the MultiZone Average Maxiumum Humidity Setpoint Managers
		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumMZAverageMaxHumSetPtMgrs; ++SetPtMgrNum ) {
			for ( CtrlNodeIndex = 1; CtrlNodeIndex <= MZAverageMaxHumSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) {
				NodeNum = MZAverageMaxHumSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex );
				if ( CntrlNodeNum == NodeNum ) {
					HumRatCntrlType = iCtrlVarType_MaxHumRat;
					return HumRatCntrlType;
				}
			}
		}
		// Loop over the single zone minimum humidity setpoint Managers
		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumSZMinHumSetPtMgrs; ++SetPtMgrNum ) {
			for ( CtrlNodeIndex = 1; CtrlNodeIndex <= SZMinHumSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) {
				NodeNum = SZMinHumSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex );
				if ( CntrlNodeNum == NodeNum ) {
					HumRatCntrlType = iCtrlVarType_MinHumRat;
					return HumRatCntrlType;
				}
			}
		}
		// Loop over the MultiZone minimum humidity setpoint Managers
		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumMZMinHumSetPtMgrs; ++SetPtMgrNum ) {
			for ( CtrlNodeIndex = 1; CtrlNodeIndex <= MZMinHumSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) {
				NodeNum = MZMinHumSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex );
				if ( CntrlNodeNum == NodeNum ) {
					HumRatCntrlType = iCtrlVarType_MinHumRat;
					return HumRatCntrlType;
				}
			}
		}
		// Loop over the MultiZone Average Minimum Humidity Setpoint Managers
		for ( SetPtMgrNum = 1; SetPtMgrNum <= NumMZAverageMinHumSetPtMgrs; ++SetPtMgrNum ) {
			for ( CtrlNodeIndex = 1; CtrlNodeIndex <= MZAverageMinHumSetPtMgr( SetPtMgrNum ).NumCtrlNodes; ++CtrlNodeIndex ) {
				NodeNum = MZAverageMinHumSetPtMgr( SetPtMgrNum ).CtrlNodes( CtrlNodeIndex );
				if ( CntrlNodeNum == NodeNum ) {
					HumRatCntrlType = iCtrlVarType_MinHumRat;
					return HumRatCntrlType;
				}
			}
		}

		return HumRatCntrlType;
	}

	//     NOTICE

	//     Copyright � 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.

	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in main.cc.

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

} // SetPointManager

} // EnergyPlus
