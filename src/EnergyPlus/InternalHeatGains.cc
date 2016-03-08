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
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <InternalHeatGains.hh>
#include <DataContaminantBalance.hh>
#include <DataDaylighting.hh>
#include <DataEnvironment.hh>
#include <DataGlobalConstants.hh>
#include <DataGlobals.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataIPShortCuts.hh>
#include <DataPrecisionGlobals.hh>
#include <DataRoomAirModel.hh>
#include <DataSizing.hh>
#include <DataSurfaces.hh>
#include <DataZoneEquipment.hh>
#include <DaylightingDevices.hh>
#include <EMSManager.hh>
#include <FuelCellElectricGenerator.hh>
#include <General.hh>
#include <HeatBalanceInternalHeatGains.hh>
#include <InputProcessor.hh>
#include <MicroCHPElectricGenerator.hh>
#include <OutputProcessor.hh>
#include <OutputReportPredefined.hh>
#include <OutputReportTabular.hh>
#include <PipeHeatTransfer.hh>
#include <Psychrometrics.hh>
#include <RefrigeratedCase.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>
#include <WaterThermalTanks.hh>
#include <WaterUse.hh>
#include <ZonePlenum.hh>
#include <DataLoopNode.hh>
#include <NodeInputManager.hh>
#include <CurveManager.hh>
#include <DataHVACGlobals.hh>
#include <ElectricPowerServiceManager.hh>

namespace EnergyPlus {

namespace InternalHeatGains {
	// Module containing the routines dealing with the internal heat gains

	// MODULE INFORMATION:
	//       AUTHOR         Rick Strand
	//       DATE WRITTEN   August 2000
	//       MODIFIED       Aug 2005, PGE (Added object names and report variables)
	//                      Feb 2006, PGE (Added end-use subcategories)
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// Part of the heat balance modularization/re-engineering.  Purpose of this
	// module is to contain the internal heat gain routines in a single location.

	// METHODOLOGY EMPLOYED:
	// Routines are called as subroutines to supply the data-only module structures
	// with the proper values.

	// REFERENCES:
	// Legacy BLAST code

	// OTHER NOTES: none

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataGlobals;
	using namespace DataEnvironment;
	using namespace DataHeatBalance;
	using namespace DataSurfaces;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	int const ITEClassNone( 0 );
	int const ITEClassA1( 1 );
	int const ITEClassA2( 2 );
	int const ITEClassA3( 3 );
	int const ITEClassA4( 4 );
	int const ITEClassB( 5 );
	int const ITEClassC( 6 );
	int const ITEInletAdjustedSupply( 0 );
	int const ITEInletZoneAirNode( 1 );
	int const ITEInletRoomAirModel( 2 );

	bool GetInternalHeatGainsInputFlag( true ); // Controls the GET routine calling (limited to first time)

	static std::string const BlankString;

	// SUBROUTINE SPECIFICATIONS FOR MODULE InternalHeatGains
	//PUBLIC  SumInternalConvectionGainsByIndices
	//PUBLIC SumReturnAirConvectionGainsByIndices
	//PUBLIC  SumInternalRadiationGainsByIndices
	//PUBLIC  SumInternalLatentGainsByIndices
	//PUBLIC
	//PUBLIC  SumInternalCO2GainsByIndices
	//PUBLIC  GetInternalGainDeviceIndex

	// Functions
	void
	clear_state()
	{
		GetInternalHeatGainsInputFlag = true;
	}

	void
	ManageInternalHeatGains( Optional_bool_const InitOnly ) // when true, just calls the get input, if appropriate and returns.
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   May 2000
		//       MODIFIED       Mar 2004, FCW: move call to DayltgElecLightingControl from InitSurfaceHeatBalance
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This is the main driver subroutine for the internal heat gains.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//unused  INTEGER       :: ZoneNum              ! Zone number

		// FLOW:
		if ( GetInternalHeatGainsInputFlag ) {
			GetInternalHeatGainsInput();
			GetInternalHeatGainsInputFlag = false;
		}

		if ( present( InitOnly ) ) {
			if ( InitOnly ) return;
		}

		InitInternalHeatGains();

		ReportInternalHeatGains();

		//for the load component report, gather the load components for each timestep but not when doing pulse
		if ( ZoneSizingCalc ) GatherComponentLoadsIntGain();

	}

	void
	GetInternalHeatGainsInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   September 1997
		//       MODIFIED       September 1998, FW
		//                      May 2009, BG: added calls to setup for possible EMS override
		//       RE-ENGINEERED  August 2000, RKS

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine gets the Internal Heat Gain Data for the Zones.
		// Sets up the various data that will be used later with the
		// schedulemanager to determine the actual values.

		// METHODOLOGY EMPLOYED:
		// The GetObjectItem routines are employed to retrieve the data.

		// REFERENCES:
		// IDD Objects:
		// People
		// Lights
		// ElectricEquipment
		// GasEquipment
		// SteamEquipment
		// HotWaterEquipment
		// OtherEquipment
		// ElectricEquipment:ITE:AirCooled
		// ZoneBaseboard:OutdoorTemperatureControlled

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using namespace InputProcessor;
		using namespace ScheduleManager;
		using General::RoundSigDigits;
		using General::CheckCreatedZoneItemName;
		using namespace OutputReportPredefined;
		using namespace DataLoopNode;
		using CurveManager::GetCurveIndex;
		using NodeInputManager::GetOnlySingleNode;
		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static gio::Fmt fmtA( "(A)" );
		static std::string const RoutineName( "GetInternalHeatGains: " );
		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Array1D_string AlphaName;
		static bool ErrorsFound( false ); // If errors found in input
		bool IsNotOK; // Flag to verify name
		Array1D< Real64 > IHGNumbers;
		int IOStat;
		bool IsBlank;
		int Loop;
		bool MustInpSch;
		int NumAlpha;
		int NumNumber;
		int MaxAlpha;
		int MaxNumber;
		int OptionNum( 0 ); //Autodesk:Init Initialization added to elim poss use uninitialized
		int lastOption;
		Array1D_bool RepVarSet;
		//   Variables for reporting nominal internal gains
		Real64 LightTot; // Total Lights for calculating lights per square meter
		Real64 ElecTot; // Total Electric Load for calculating electric per square meter
		Real64 GasTot; // Total Gas load for calculating gas per square meter
		Real64 OthTot; // Total Other load for calculating other load per square meter
		Real64 HWETot; // Total Hot Water Equipment for calculating HWE per square meter
		Real64 StmTot; // Total Steam for calculating Steam per square meter
		std::string BBHeatInd; // Yes if BBHeat in zone, no if not.
		int Loop1;
		std::string StringOut;
		Real64 SchMin;
		Real64 SchMax;
		static bool UsingThermalComfort( false );
		std::string liteName;
		int zonePt;
		Real64 mult;
		static Real64 sumArea( 0.0 );
		static Real64 sumPower( 0.0 );
		int ZoneNum;
		Real64 maxOccupLoad;
		std::string CurrentModuleObject;
		bool errFlag;
		int Item;
		int ZLItem;
		int Item1;

		// Formats
		static gio::Fmt Format_720( "(' Zone Internal Gains, ',A,',',A,',',A,',')" );
		static gio::Fmt Format_721( "('! <Zone Internal Gains/Equipment Information - Nominal>,Zone Name, Floor Area {m2},# Occupants,','Area per Occupant {m2/person},Occupant per Area {person/m2},Interior Lighting {W/m2},','Electric Load {W/m2},Gas Load {W/m2},Other Load {W/m2},Hot Water Eq {W/m2},','Steam Equipment {W/m2},Sum Loads per Area {W/m2},Outdoor Controlled Baseboard Heat')" );
		static gio::Fmt Format_722( "(' ',A,' Internal Gains, ',A,',',A,',',A,',',A,',',A,',')" );
		static gio::Fmt Format_723( "('! <',A,' Internal Gains - Nominal>,Name,Schedule Name,Zone Name,Zone Floor Area {m2},# Zone Occupants,',A)" );
		static gio::Fmt Format_724( "(' ',A,', ',A)" );

		// FLOW:
		ZoneIntGain.allocate( NumOfZones );
		ZnRpt.allocate( NumOfZones );
		ZoneIntEEuse.allocate( NumOfZones );
		RefrigCaseCredit.allocate( NumOfZones );

		RepVarSet.dimension( NumOfZones, true );

		// Determine argument length of objects gotten by this routine
		MaxAlpha = -100;
		MaxNumber = -100;
		CurrentModuleObject = "People";
		GetObjectDefMaxArgs( CurrentModuleObject, Loop, NumAlpha, NumNumber );
		MaxAlpha = max( MaxAlpha, NumAlpha );
		MaxNumber = max( MaxNumber, NumNumber );
		CurrentModuleObject = "Lights";
		GetObjectDefMaxArgs( CurrentModuleObject, Loop, NumAlpha, NumNumber );
		MaxAlpha = max( MaxAlpha, NumAlpha );
		MaxNumber = max( MaxNumber, NumNumber );
		CurrentModuleObject = "ElectricEquipment";
		GetObjectDefMaxArgs( CurrentModuleObject, Loop, NumAlpha, NumNumber );
		MaxAlpha = max( MaxAlpha, NumAlpha );
		MaxNumber = max( MaxNumber, NumNumber );
		CurrentModuleObject = "GasEquipment";
		GetObjectDefMaxArgs( CurrentModuleObject, Loop, NumAlpha, NumNumber );
		MaxAlpha = max( MaxAlpha, NumAlpha );
		MaxNumber = max( MaxNumber, NumNumber );
		CurrentModuleObject = "HotWaterEquipment";
		GetObjectDefMaxArgs( CurrentModuleObject, Loop, NumAlpha, NumNumber );
		MaxAlpha = max( MaxAlpha, NumAlpha );
		MaxNumber = max( MaxNumber, NumNumber );
		CurrentModuleObject = "SteamEquipment";
		GetObjectDefMaxArgs( CurrentModuleObject, Loop, NumAlpha, NumNumber );
		MaxAlpha = max( MaxAlpha, NumAlpha );
		MaxNumber = max( MaxNumber, NumNumber );
		CurrentModuleObject = "OtherEquipment";
		GetObjectDefMaxArgs( CurrentModuleObject, Loop, NumAlpha, NumNumber );
		MaxAlpha = max( MaxAlpha, NumAlpha );
		MaxNumber = max( MaxNumber, NumNumber );
		CurrentModuleObject = "ElectricEquipment:ITE:AirCooled";
		GetObjectDefMaxArgs( CurrentModuleObject, Loop, NumAlpha, NumNumber );
		MaxAlpha = max( MaxAlpha, NumAlpha );
		MaxNumber = max( MaxNumber, NumNumber );
		CurrentModuleObject = "ZoneBaseboard:OutdoorTemperatureControlled";
		GetObjectDefMaxArgs( CurrentModuleObject, Loop, NumAlpha, NumNumber );
		MaxAlpha = max( MaxAlpha, NumAlpha );
		MaxNumber = max( MaxNumber, NumNumber );
		CurrentModuleObject = "ZoneContaminantSourceAndSink:CarbonDioxide";
		GetObjectDefMaxArgs( CurrentModuleObject, Loop, NumAlpha, NumNumber );
		MaxAlpha = max( MaxAlpha, NumAlpha );
		MaxNumber = max( MaxNumber, NumNumber );

		IHGNumbers.allocate( MaxNumber );
		AlphaName.allocate( MaxAlpha );
		IHGNumbers = 0.0;
		AlphaName = "";

		//CurrentModuleObject='Zone'
		for ( Loop = 1; Loop <= NumOfZones; ++Loop ) {
			// Overall Zone Variables
			SetupOutputVariable( "Zone Total Internal Radiant Heating Energy [J]", ZnRpt( Loop ).TotRadiantGain, "Zone", "Sum", Zone( Loop ).Name );
			SetupOutputVariable( "Zone Total Internal Radiant Heating Rate [W]", ZnRpt( Loop ).TotRadiantGainRate, "Zone", "Average", Zone( Loop ).Name );
			SetupOutputVariable( "Zone Total Internal Visible Radiation Heating Energy [J]", ZnRpt( Loop ).TotVisHeatGain, "Zone", "Sum", Zone( Loop ).Name );
			SetupOutputVariable( "Zone Total Internal Visible Radiation Heating Rate [W]", ZnRpt( Loop ).TotVisHeatGainRate, "Zone", "Average", Zone( Loop ).Name );
			SetupOutputVariable( "Zone Total Internal Convective Heating Energy [J]", ZnRpt( Loop ).TotConvectiveGain, "Zone", "Sum", Zone( Loop ).Name );
			SetupOutputVariable( "Zone Total Internal Convective Heating Rate [W]", ZnRpt( Loop ).TotConvectiveGainRate, "Zone", "Average", Zone( Loop ).Name );
			SetupOutputVariable( "Zone Total Internal Latent Gain Energy [J]", ZnRpt( Loop ).TotLatentGain, "Zone", "Sum", Zone( Loop ).Name );
			SetupOutputVariable( "Zone Total Internal Latent Gain Rate [W]", ZnRpt( Loop ).TotLatentGainRate, "Zone", "Average", Zone( Loop ).Name );
			SetupOutputVariable( "Zone Total Internal Total Heating Energy [J]", ZnRpt( Loop ).TotTotalHeatGain, "Zone", "Sum", Zone( Loop ).Name );
			SetupOutputVariable( "Zone Total Internal Total Heating Rate [W]", ZnRpt( Loop ).TotTotalHeatGainRate, "Zone", "Average", Zone( Loop ).Name );
		}

		// PEOPLE: Includes both information related to the heat balance and thermal comfort
		// First, allocate and initialize the People derived type
		CurrentModuleObject = "People";
		NumPeopleStatements = GetNumObjectsFound( CurrentModuleObject );
		PeopleObjects.allocate( NumPeopleStatements );

		TotPeople = 0;
		errFlag = false;
		for ( Item = 1; Item <= NumPeopleStatements; ++Item ) {
			GetObjectItem( CurrentModuleObject, Item, AlphaName, NumAlpha, IHGNumbers, NumNumber, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( AlphaName( 1 ), PeopleObjects, Item - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				errFlag = true;
				if ( IsBlank ) AlphaName( 1 ) = "xxxxx";
			}
			PeopleObjects( Item ).Name = AlphaName( 1 );

			Item1 = FindItemInList( AlphaName( 2 ), Zone );
			ZLItem = 0;
			if ( Item1 == 0 && NumOfZoneLists > 0 ) ZLItem = FindItemInList( AlphaName( 2 ), ZoneList );
			if ( Item1 > 0 ) {
				PeopleObjects( Item ).StartPtr = TotPeople + 1;
				++TotPeople;
				PeopleObjects( Item ).NumOfZones = 1;
				PeopleObjects( Item ).ZoneListActive = false;
				PeopleObjects( Item ).ZoneOrZoneListPtr = Item1;
			} else if ( ZLItem > 0 ) {
				PeopleObjects( Item ).StartPtr = TotPeople + 1;
				TotPeople += ZoneList( ZLItem ).NumOfZones;
				PeopleObjects( Item ).NumOfZones = ZoneList( ZLItem ).NumOfZones;
				PeopleObjects( Item ).ZoneListActive = true;
				PeopleObjects( Item ).ZoneOrZoneListPtr = ZLItem;
			} else {
				ShowSevereError( CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\" invalid " + cAlphaFieldNames( 2 ) + "=\"" + AlphaName( 2 ) + "\" not found." );
				ErrorsFound = true;
				errFlag = true;
			}
		}

		if ( errFlag ) {
			ShowSevereError( RoutineName + "Errors with invalid names in " + CurrentModuleObject + " objects." );
			ShowContinueError( "...These will not be read in.  Other errors may occur." );
			TotPeople = 0;
		}

		People.allocate( TotPeople );

		if ( TotPeople > 0 ) {
			Loop = 0;
			for ( Item = 1; Item <= NumPeopleStatements; ++Item ) {
				AlphaName = BlankString;
				IHGNumbers = 0.0;

				GetObjectItem( CurrentModuleObject, Item, AlphaName, NumAlpha, IHGNumbers, NumNumber, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

				for ( Item1 = 1; Item1 <= PeopleObjects( Item ).NumOfZones; ++Item1 ) {
					++Loop;
					if ( ! PeopleObjects( Item ).ZoneListActive ) {
						People( Loop ).Name = AlphaName( 1 );
						People( Loop ).ZonePtr = PeopleObjects( Item ).ZoneOrZoneListPtr;
					} else {
						CheckCreatedZoneItemName( RoutineName, CurrentModuleObject, Zone( ZoneList( PeopleObjects( Item ).ZoneOrZoneListPtr ).Zone( Item1 ) ).Name, ZoneList( PeopleObjects( Item ).ZoneOrZoneListPtr ).MaxZoneNameLength, PeopleObjects( Item ).Name, People, Loop - 1, People( Loop ).Name, errFlag );
						People( Loop ).ZonePtr = ZoneList( PeopleObjects( Item ).ZoneOrZoneListPtr ).Zone( Item1 );
						if ( errFlag ) ErrorsFound = true;
					}

					People( Loop ).NumberOfPeoplePtr = GetScheduleIndex( AlphaName( 3 ) );
					SchMin = 0.0;
					SchMax = 0.0;
					if ( People( Loop ).NumberOfPeoplePtr == 0 ) {
						if ( Item1 == 1 ) { // only show error on first one
							if ( lAlphaFieldBlanks( 3 ) ) {
								ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", " + cAlphaFieldNames( 3 ) + " is required." );
							} else {
								ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", invalid " + cAlphaFieldNames( 3 ) + " entered=" + AlphaName( 3 ) );
							}
							ErrorsFound = true;
						}
					} else { // check min/max on schedule
						SchMin = GetScheduleMinValue( People( Loop ).NumberOfPeoplePtr );
						SchMax = GetScheduleMaxValue( People( Loop ).NumberOfPeoplePtr );
						if ( SchMin < 0.0 || SchMax < 0.0 ) {
							if ( Item1 == 1 ) {
								if ( SchMin < 0.0 ) {
									ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", " + cAlphaFieldNames( 3 ) + ", minimum is < 0.0" );
									ShowContinueError( "Schedule=\"" + AlphaName( 3 ) + "\". Minimum is [" + RoundSigDigits( SchMin, 1 ) + "]. Values must be >= 0.0." );
									ErrorsFound = true;
								}
							}
							if ( Item1 == 1 ) {
								if ( SchMax < 0.0 ) {
									ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", " + cAlphaFieldNames( 3 ) + ", maximum is < 0.0" );
									ShowContinueError( "Schedule=\"" + AlphaName( 3 ) + "\". Maximum is [" + RoundSigDigits( SchMax, 1 ) + "]. Values must be >= 0.0." );
									ErrorsFound = true;
								}
							}
						}
					}

					// Number of people calculation method.
					{ auto const peopleMethod( AlphaName( 4 ) );
					if ( peopleMethod == "PEOPLE" ) {
						People( Loop ).NumberOfPeople = IHGNumbers( 1 );
						if ( lNumericFieldBlanks( 1 ) ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + People( Loop ).Name + "\", specifies " + cNumericFieldNames( 1 ) + ", but that field is blank.  0 People will result." );
						}

					} else if ( peopleMethod == "PEOPLE/AREA" ) {
						if ( People( Loop ).ZonePtr != 0 ) {
							if ( IHGNumbers( 2 ) >= 0.0 ) {
								People( Loop ).NumberOfPeople = IHGNumbers( 2 ) * Zone( People( Loop ).ZonePtr ).FloorArea;
								if ( Zone( People( Loop ).ZonePtr ).FloorArea <= 0.0 ) {
									ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + People( Loop ).Name + "\", specifies " + cNumericFieldNames( 2 ) + ", but Zone Floor Area = 0.  0 People will result." );
								}
							} else {
								ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + People( Loop ).Name + "\", invalid " + cNumericFieldNames( 2 ) + ", value  [<0.0]=" + RoundSigDigits( IHGNumbers( 2 ), 3 ) );
								ErrorsFound = true;
							}
						}
						if ( lNumericFieldBlanks( 2 ) ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + People( Loop ).Name + "\", specifies " + cNumericFieldNames( 2 ) + ", but that field is blank.  0 People will result." );
						}

					} else if ( peopleMethod == "AREA/PERSON" ) {
						if ( People( Loop ).ZonePtr != 0 ) {
							if ( IHGNumbers( 3 ) > 0.0 ) {
								People( Loop ).NumberOfPeople = Zone( People( Loop ).ZonePtr ).FloorArea / IHGNumbers( 3 );
								if ( Zone( People( Loop ).ZonePtr ).FloorArea <= 0.0 ) {
									ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + People( Loop ).Name + "\", specifies " + cNumericFieldNames( 2 ) + ", but Zone Floor Area = 0.  0 People will result." );
								}
							} else {
								ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + People( Loop ).Name + "\", invalid " + cNumericFieldNames( 3 ) + ", value  [<0.0]=" + RoundSigDigits( IHGNumbers( 3 ), 3 ) );
								ErrorsFound = true;
							}
						}
						if ( lNumericFieldBlanks( 3 ) ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + People( Loop ).Name + "\", specifies " + cNumericFieldNames( 3 ) + ", but that field is blank.  0 People will result." );
						}

					} else {
						if ( Item1 == 1 ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", invalid " + cAlphaFieldNames( 4 ) + ", value  =" + AlphaName( 4 ) );
							ShowContinueError( "...Valid values are \"People\", \"People/Area\", \"Area/Person\"." );
							ErrorsFound = true;
						}
					}}

					// Calculate nominal min/max people
					People( Loop ).NomMinNumberPeople = People( Loop ).NumberOfPeople * SchMin;
					People( Loop ).NomMaxNumberPeople = People( Loop ).NumberOfPeople * SchMax;

					if ( People( Loop ).ZonePtr > 0 ) {
						Zone( People( Loop ).ZonePtr ).TotOccupants += People( Loop ).NumberOfPeople;
					}

					People( Loop ).FractionRadiant = IHGNumbers( 4 );
					People( Loop ).FractionConvected = 1.0 - People( Loop ).FractionRadiant;
					if ( Item1 == 1 ) {
						if ( People( Loop ).FractionConvected < 0.0 ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", " + cNumericFieldNames( 4 ) + " < 0.0, value =" + RoundSigDigits( IHGNumbers( 4 ), 2 ) );
							ErrorsFound = true;
						}
					}

					if ( NumNumber >= 5 && ! lNumericFieldBlanks( 5 ) ) {
						People( Loop ).UserSpecSensFrac = IHGNumbers( 5 );
					} else {
						People( Loop ).UserSpecSensFrac = AutoCalculate;
					}

					if ( NumNumber == 6 && ! lNumericFieldBlanks( 6 ) ) {
						People( Loop ).CO2RateFactor = IHGNumbers( 6 );
					} else {
						People( Loop ).CO2RateFactor = 3.82e-8; // m3/s-W
					}
					if ( People( Loop ).CO2RateFactor < 0.0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", " + cNumericFieldNames( 6 ) + " < 0.0, value =" + RoundSigDigits( IHGNumbers( 6 ), 2 ) );
						ErrorsFound = true;
					}

					People( Loop ).ActivityLevelPtr = GetScheduleIndex( AlphaName( 5 ) );
					if ( People( Loop ).ActivityLevelPtr == 0 ) {
						if ( Item1 == 1 ) {
							if ( lAlphaFieldBlanks( 5 ) ) {
								ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", " + cAlphaFieldNames( 5 ) + " is required." );
							} else {
								ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", invalid " + cAlphaFieldNames( 5 ) + " entered=" + AlphaName( 5 ) );
							}
							ErrorsFound = true;
						}
					} else { // Check values in Schedule
						SchMin = GetScheduleMinValue( People( Loop ).ActivityLevelPtr );
						SchMax = GetScheduleMaxValue( People( Loop ).ActivityLevelPtr );
						if ( SchMin < 0.0 || SchMax < 0.0 ) {
							if ( Item1 == 1 ) {
								if ( SchMin < 0.0 ) {
									ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", " + cAlphaFieldNames( 5 ) + " minimum is < 0.0" );
									ShowContinueError( "Schedule=\"" + AlphaName( 5 ) + "\". Minimum is [" + RoundSigDigits( SchMin, 1 ) + "]. Values must be >= 0.0." );
									ErrorsFound = true;
								}
							}
							if ( Item1 == 1 ) {
								if ( SchMax < 0.0 ) {
									ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", " + cAlphaFieldNames( 5 ) + " maximum is < 0.0" );
									ShowContinueError( "Schedule=\"" + AlphaName( 5 ) + "\". Maximum is [" + RoundSigDigits( SchMax, 1 ) + "]. Values must be >= 0.0." );
									ErrorsFound = true;
								}
							}
						} else if ( SchMin < 70.0 || SchMax > 1000.0 ) {
							if ( Item1 == 1 ) {
								ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", " + cAlphaFieldNames( 5 ) + " values" );
								ShowContinueError( "fall outside typical range [70,1000] W/person for Thermal Comfort Reporting." );
								ShowContinueError( "Odd comfort values may result; Schedule=\"" + AlphaName( 5 ) + "\"." );
								ShowContinueError( "Entered min/max range=[" + RoundSigDigits( SchMin, 1 ) + ',' + RoundSigDigits( SchMax, 1 ) + "] W/person." );
							}
						}
					}

					// Following is an optional parameter (ASHRAE 55 warnings
					if ( NumAlpha >= 6 ) {
						if ( SameString( AlphaName( 6 ), "Yes" ) ) {
							People( Loop ).Show55Warning = true;
						} else if ( ! SameString( AlphaName( 6 ), "No" ) && ! lAlphaFieldBlanks( 6 ) ) {
							if ( Item1 == 1 ) {
								ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", " + cAlphaFieldNames( 6 ) + " field should be Yes or No" );
								ShowContinueError( "...Field value=\"" + AlphaName( 6 ) + "\" is invalid." );
								ErrorsFound = true;
							}
						}
					}

					if ( NumAlpha > 6 ) { // Optional parameters present--thermal comfort data follows...
						MustInpSch = false;
						UsingThermalComfort = false;
						lastOption = NumAlpha;

						for ( OptionNum = 14; OptionNum <= lastOption; ++OptionNum ) {

							{ auto const thermalComfortType( AlphaName( OptionNum ) );

							if ( thermalComfortType == "FANGER" ) {
								People( Loop ).Fanger = true;
								MustInpSch = true;
								UsingThermalComfort = true;

							} else if ( thermalComfortType == "PIERCE" ) {
								People( Loop ).Pierce = true;
								MustInpSch = true;
								UsingThermalComfort = true;

							} else if ( thermalComfortType == "KSU" ) {
								People( Loop ).KSU = true;
								MustInpSch = true;
								UsingThermalComfort = true;

							} else if ( thermalComfortType == "ADAPTIVEASH55" ) {
								People( Loop ).AdaptiveASH55 = true;
								AdaptiveComfortRequested_ASH55 = true;
								MustInpSch = true;
								UsingThermalComfort = true;

							} else if ( thermalComfortType == "ADAPTIVECEN15251" ) {
								People( Loop ).AdaptiveCEN15251 = true;
								AdaptiveComfortRequested_CEN15251 = true;
								MustInpSch = true;
								UsingThermalComfort = true;

							} else if ( thermalComfortType == "" ) { // Blank input field--just ignore this

							} else { // An invalid keyword was entered--warn but ignore
								if ( Item1 == 1 ) {
									ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", invalid " + cAlphaFieldNames( OptionNum ) + " Option=" + AlphaName( OptionNum ) );
									ShowContinueError( "Valid Values are \"Fanger\", \"Pierce\", \"KSU\", \"AdaptiveASH55\", \"AdaptiveCEN15251\"" );
								}
							}}

						}

						if ( UsingThermalComfort ) {

							// Set the default value of MRTCalcType as 'ZoneAveraged'
							People( Loop ).MRTCalcType = ZoneAveraged;

							// MRT Calculation Type and Surface Name
							{ auto const mrtType( AlphaName( 7 ) );

							if ( mrtType == "ZONEAVERAGED" ) {
								People( Loop ).MRTCalcType = ZoneAveraged;

							} else if ( mrtType == "SURFACEWEIGHTED" ) {
								People( Loop ).MRTCalcType = SurfaceWeighted;
								People( Loop ).SurfacePtr = FindItemInList( AlphaName( 8 ), Surface );
								if ( People( Loop ).SurfacePtr == 0 ) {
									if ( Item1 == 1 ) {
										ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", " + cAlphaFieldNames( 7 ) + '=' + AlphaName( 7 ) + " invalid Surface Name=" + AlphaName( 8 ) );
										ErrorsFound = true;
									}
								} else if ( Surface( People( Loop ).SurfacePtr ).Zone != People( Loop ).ZonePtr ) {
									ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", Surface referenced in " + cAlphaFieldNames( 7 ) + '=' + AlphaName( 8 ) + " in different zone." );
									ShowContinueError( "Surface is in Zone=" + Zone( Surface( People( Loop ).SurfacePtr ).Zone ).Name + " and " + CurrentModuleObject + " is in Zone=" + AlphaName( 2 ) );
									ErrorsFound = true;
								}

							} else if ( mrtType == "ANGLEFACTOR" ) {
								People( Loop ).MRTCalcType = AngleFactor;
								People( Loop ).AngleFactorListName = AlphaName( 8 );

							} else if ( mrtType == "" ) { // Blank input field--just ignore this
								if ( MustInpSch && Item1 == 1 ) ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", blank " + cAlphaFieldNames( 7 ) );

							} else { // An invalid keyword was entered--warn but ignore
								if ( MustInpSch && Item1 == 1 ) {
									ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", invalid " + cAlphaFieldNames( 7 ) + '=' + AlphaName( 7 ) );
									ShowContinueError( "...Valid values are \"ZoneAveraged\", \"SurfaceWeighted\", \"AngleFactor\"." );
								}
							}}

							if ( ! lAlphaFieldBlanks( 9 ) ) {
								People( Loop ).WorkEffPtr = GetScheduleIndex( AlphaName( 9 ) );
								if ( People( Loop ).WorkEffPtr == 0 ) {
									if ( Item1 == 1 ) {
										ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", invalid " + cAlphaFieldNames( 9 ) + " entered=" + AlphaName( 9 ) );
										ErrorsFound = true;
									}
								} else { // check min/max on schedule
									SchMin = GetScheduleMinValue( People( Loop ).WorkEffPtr );
									SchMax = GetScheduleMaxValue( People( Loop ).WorkEffPtr );
									if ( SchMin < 0.0 || SchMax < 0.0 ) {
										if ( SchMin < 0.0 ) {
											if ( Item1 == 1 ) {
												ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", " + cAlphaFieldNames( 9 ) + ", minimum is < 0.0" );
												ShowContinueError( "Schedule=\"" + AlphaName( 9 ) + "\". Minimum is [" + RoundSigDigits( SchMin, 1 ) + "]. Values must be >= 0.0." );
												ErrorsFound = true;
											}
										}
										if ( SchMax < 0.0 ) {
											if ( Item1 == 1 ) {
												ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", " + cAlphaFieldNames( 9 ) + ", maximum is < 0.0" );
												ShowContinueError( "Schedule=\"" + AlphaName( 9 ) + "\". Maximum is [" + RoundSigDigits( SchMax, 1 ) + "]. Values must be >= 0.0." );
												ErrorsFound = true;
											}
										}
									}
									if ( SchMax > 1.0 ) {
										if ( Item1 == 1 ) {
											ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", " + cAlphaFieldNames( 9 ) + ", maximum is > 1.0" );
											ShowContinueError( "Schedule=\"" + AlphaName( 9 ) + "\"; Entered min/max range=[" + RoundSigDigits( SchMin, 1 ) + ',' + RoundSigDigits( SchMax, 1 ) + "] Work Efficiency." );
										}
									}
								}
							} else if ( MustInpSch ) {
								if ( Item1 == 1 ) {
									ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", blank " + cAlphaFieldNames( 9 ) + " is required for this item." );
									ErrorsFound = true;
								}
							}

							if ( ! lAlphaFieldBlanks( 10 ) || AlphaName( 10 ) != "" ) {
								{ auto const clothingType( AlphaName( 10 ) );
								if ( clothingType == "CLOTHINGINSULATIONSCHEDULE" ) {
									People( Loop ).ClothingType = 1;
									People( Loop ).ClothingPtr = GetScheduleIndex( AlphaName( 12 ) );
									if ( People( Loop ).ClothingPtr == 0 ) {
										if ( Item1 == 1 ) {
											ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", invalid " + cAlphaFieldNames( 12 ) + " entered=" + AlphaName( 12 ) );
											ErrorsFound = true;
										}
									} else { // check min/max on schedule
										SchMin = GetScheduleMinValue( People( Loop ).ClothingPtr );
										SchMax = GetScheduleMaxValue( People( Loop ).ClothingPtr );
										if ( SchMin < 0.0 || SchMax < 0.0 ) {
											if ( SchMin < 0.0 ) {
												if ( Item1 == 1 ) {
													ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", " + cAlphaFieldNames( 12 ) + ", minimum is < 0.0" );
													ShowContinueError( "Schedule=\"" + AlphaName( 12 ) + "\". Minimum is [" + RoundSigDigits( SchMin, 1 ) + "]. Values must be >= 0.0." );
													ErrorsFound = true;
												}
											}
											if ( SchMax < 0.0 ) {
												if ( Item1 == 1 ) {
													ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", " + cAlphaFieldNames( 12 ) + ", maximum is < 0.0" );
													ShowContinueError( "Schedule=\"" + AlphaName( 12 ) + "\". Maximum is [" + RoundSigDigits( SchMax, 1 ) + "]. Values must be >= 0.0." );
													ErrorsFound = true;
												}
											}
										}
										if ( SchMax > 2.0 ) {
											if ( Item1 == 1 ) {
												ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", " + cAlphaFieldNames( 12 ) + ", maximum is > 2.0" );
												ShowContinueError( "Schedule=\"" + AlphaName( 12 ) + "\"; Entered min/max range=[" + RoundSigDigits( SchMin, 1 ) + ',' + RoundSigDigits( SchMax, 1 ) + "] Clothing." );
											}
										}
									}

								} else if ( clothingType == "DYNAMICCLOTHINGMODELASHRAE55" ) {
									People( Loop ).ClothingType = 2;

								} else if ( clothingType == "CALCULATIONMETHODSCHEDULE" ) {
									People( Loop ).ClothingType = 3;
									People( Loop ).ClothingMethodPtr = GetScheduleIndex( AlphaName( 11 ) );
									if ( People( Loop ).ClothingMethodPtr == 0 ) {
										if ( Item1 == 1 ) {
											ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", invalid " + cAlphaFieldNames( 11 ) + " entered=" + AlphaName( 11 ) );
											ErrorsFound = true;
										}
									}
									if ( CheckScheduleValue( People( Loop ).ClothingMethodPtr, 1 ) ) {
										People( Loop ).ClothingPtr = GetScheduleIndex( AlphaName( 12 ) );
										if ( People( Loop ).ClothingPtr == 0 ) {
											if ( Item1 == 1 ) {
												ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", invalid " + cAlphaFieldNames( 12 ) + " entered=" + AlphaName( 12 ) );
												ErrorsFound = true;
											}
										}
									}

								} else {
									ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + People( Loop ).Name + "\", invalid " + cAlphaFieldNames( 10 ) + ", value  =" + AlphaName( 10 ) );
									ShowContinueError( "...Valid values are \"ClothingInsulationSchedule\",\"DynamicClothingModelASHRAE55a\", \"CalculationMethodSchedule\"." );
									ErrorsFound = true;
								}}
							}

							if ( ! lAlphaFieldBlanks( 13 ) ) {
								People( Loop ).AirVelocityPtr = GetScheduleIndex( AlphaName( 13 ) );
								if ( People( Loop ).AirVelocityPtr == 0 ) {
									if ( Item1 == 1 ) {
										ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", invalid " + cAlphaFieldNames( 13 ) + " entered=" + AlphaName( 13 ) );
										ErrorsFound = true;
									}
								} else { // check min/max on schedule
									SchMin = GetScheduleMinValue( People( Loop ).AirVelocityPtr );
									SchMax = GetScheduleMaxValue( People( Loop ).AirVelocityPtr );
									if ( SchMin < 0.0 || SchMax < 0.0 ) {
										if ( SchMin < 0.0 ) {
											if ( Item1 == 1 ) {
												ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", " + cAlphaFieldNames( 13 ) + ", minimum is < 0.0" );
												ShowContinueError( "Schedule=\"" + AlphaName( 13 ) + "\". Minimum is [" + RoundSigDigits( SchMin, 1 ) + "]. Values must be >= 0.0." );
												ErrorsFound = true;
											}
										}
										if ( SchMax < 0.0 ) {
											if ( Item1 == 1 ) {
												ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", " + cAlphaFieldNames( 13 ) + ", maximum is < 0.0" );
												ShowContinueError( "Schedule=\"" + AlphaName( 13 ) + "\". Maximum is [" + RoundSigDigits( SchMax, 1 ) + "]. Values must be >= 0.0." );
												ErrorsFound = true;
											}
										}
									}
								}
							} else if ( MustInpSch ) {
								if ( Item1 == 1 ) {
									ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", blank " + cAlphaFieldNames( 13 ) + " is required for this item." );
									ErrorsFound = true;
								}
							}

						} // usingthermalcomfort block

					} // ...end of thermal comfort data IF-THEN block  (NumAlphas > 6)

					if ( People( Loop ).ZonePtr <= 0 ) continue; // Error, will be caught and terminated later

					// Object report variables
					SetupOutputVariable( "People Occupant Count []", People( Loop ).NumOcc, "Zone", "Average", People( Loop ).Name );
					SetupOutputVariable( "People Radiant Heating Energy [J]", People( Loop ).RadGainEnergy, "Zone", "Sum", People( Loop ).Name );
					SetupOutputVariable( "People Radiant Heating Rate [W]", People( Loop ).RadGainRate, "Zone", "Average", People( Loop ).Name );
					SetupOutputVariable( "People Convective Heating Energy [J]", People( Loop ).ConGainEnergy, "Zone", "Sum", People( Loop ).Name );
					SetupOutputVariable( "People Convective Heating Rate [W]", People( Loop ).ConGainRate, "Zone", "Average", People( Loop ).Name );
					SetupOutputVariable( "People Sensible Heating Energy [J]", People( Loop ).SenGainEnergy, "Zone", "Sum", People( Loop ).Name );
					SetupOutputVariable( "People Sensible Heating Rate [W]", People( Loop ).SenGainRate, "Zone", "Average", People( Loop ).Name );
					SetupOutputVariable( "People Latent Gain Energy [J]", People( Loop ).LatGainEnergy, "Zone", "Sum", People( Loop ).Name );
					SetupOutputVariable( "People Latent Gain Rate [W]", People( Loop ).LatGainRate, "Zone", "Average", People( Loop ).Name );
					SetupOutputVariable( "People Total Heating Energy [J]", People( Loop ).TotGainEnergy, "Zone", "Sum", People( Loop ).Name );
					SetupOutputVariable( "People Total Heating Rate [W]", People( Loop ).TotGainRate, "Zone", "Average", People( Loop ).Name );
					SetupOutputVariable( "People Air Temperature [C]", People( Loop ).TemperatureInZone, "Zone", "Average", People( Loop ).Name );
					SetupOutputVariable( "People Air Relative Humidity [%]", People( Loop ).RelativeHumidityInZone, "Zone", "Average", People( Loop ).Name );

					// Zone total report variables
					if ( RepVarSet( People( Loop ).ZonePtr ) ) {
						RepVarSet( People( Loop ).ZonePtr ) = false;
						SetupOutputVariable( "Zone People Occupant Count []", ZnRpt( People( Loop ).ZonePtr ).PeopleNumOcc, "Zone", "Average", Zone( People( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone People Radiant Heating Energy [J]", ZnRpt( People( Loop ).ZonePtr ).PeopleRadGain, "Zone", "Sum", Zone( People( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone People Radiant Heating Rate [W]", ZnRpt( People( Loop ).ZonePtr ).PeopleRadGainRate, "Zone", "Average", Zone( People( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone People Convective Heating Energy [J]", ZnRpt( People( Loop ).ZonePtr ).PeopleConGain, "Zone", "Sum", Zone( People( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone People Convective Heating Rate [W]", ZnRpt( People( Loop ).ZonePtr ).PeopleConGainRate, "Zone", "Average", Zone( People( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone People Sensible Heating Energy [J]", ZnRpt( People( Loop ).ZonePtr ).PeopleSenGain, "Zone", "Sum", Zone( People( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone People Sensible Heating Rate [W]", ZnRpt( People( Loop ).ZonePtr ).PeopleSenGainRate, "Zone", "Average", Zone( People( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone People Latent Gain Energy [J]", ZnRpt( People( Loop ).ZonePtr ).PeopleLatGain, "Zone", "Sum", Zone( People( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone People Latent Gain Rate [W]", ZnRpt( People( Loop ).ZonePtr ).PeopleLatGainRate, "Zone", "Average", Zone( People( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone People Total Heating Energy [J]", ZnRpt( People( Loop ).ZonePtr ).PeopleTotGain, "Zone", "Sum", Zone( People( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone People Total Heating Rate [W]", ZnRpt( People( Loop ).ZonePtr ).PeopleTotGainRate, "Zone", "Average", Zone( People( Loop ).ZonePtr ).Name );
					}

					if ( AnyEnergyManagementSystemInModel ) {
						SetupEMSActuator( "People", People( Loop ).Name, "Number of People", "[each]", People( Loop ).EMSPeopleOn, People( Loop ).EMSNumberOfPeople );
						SetupEMSInternalVariable( "People Count Design Level", People( Loop ).Name, "[each]", People( Loop ).NumberOfPeople );
					}

					//setup internal gains
					if ( ! ErrorsFound ) SetupZoneInternalGain( People( Loop ).ZonePtr, "People", People( Loop ).Name, IntGainTypeOf_People, People( Loop ).ConGainRate, _, People( Loop ).RadGainRate, People( Loop ).LatGainRate, _, People( Loop ).CO2GainRate );

				} // Item1 - number of zones
			} // Item - number of people statements
		} // TotPeople > 0

		//transfer the nominal number of people in a zone to the tabular reporting
		for ( Loop = 1; Loop <= NumOfZones; ++Loop ) {
			if ( Zone( Loop ).TotOccupants > 0.0 ) {
				if ( Zone( Loop ).FloorArea > 0.0 && Zone( Loop ).FloorArea / Zone( Loop ).TotOccupants < 0.1 ) {
					ShowWarningError( RoutineName + "Zone=\"" + Zone( Loop ).Name + "\" occupant density is extremely high." );
					if ( Zone( Loop ).FloorArea > 0.0 ) {
						ShowContinueError( "Occupant Density=[" + RoundSigDigits( Zone( Loop ).TotOccupants / Zone( Loop ).FloorArea, 0 ) + "] person/m2." );
					}
					ShowContinueError( "Occupant Density=[" + RoundSigDigits( Zone( Loop ).FloorArea / Zone( Loop ).TotOccupants, 3 ) + "] m2/person. Problems in Temperature Out of Bounds may result." );
				}
				maxOccupLoad = 0.0;
				for ( Loop1 = 1; Loop1 <= TotPeople; ++Loop1 ) {
					if ( People( Loop1 ).ZonePtr != Loop ) continue;
					if ( maxOccupLoad < GetScheduleMaxValue( People( Loop1 ).NumberOfPeoplePtr ) * People( Loop1 ).NumberOfPeople ) {
						maxOccupLoad = GetScheduleMaxValue( People( Loop1 ).NumberOfPeoplePtr ) * People( Loop1 ).NumberOfPeople;
						MaxNumber = People( Loop1 ).NumberOfPeoplePtr;
						OptionNum = Loop1;
					}
				}
				if ( maxOccupLoad > Zone( Loop ).TotOccupants ) {
					if ( Zone( Loop ).FloorArea > 0.0 && Zone( Loop ).FloorArea / maxOccupLoad < 0.1 ) {
						ShowWarningError( RoutineName + "Zone=\"" + Zone( Loop ).Name + "\" occupant density at a maximum schedule value is extremely high." );
						if ( Zone( Loop ).FloorArea > 0.0 ) {
							ShowContinueError( "Occupant Density=[" + RoundSigDigits( maxOccupLoad / Zone( Loop ).FloorArea, 0 ) + "] person/m2." );
						}
						ShowContinueError( "Occupant Density=[" + RoundSigDigits( Zone( Loop ).FloorArea / maxOccupLoad, 3 ) + "] m2/person. Problems in Temperature Out of Bounds may result." );
						ShowContinueError( "Check values in People=" + People( OptionNum ).Name + ", Number of People Schedule=" + GetScheduleName( MaxNumber ) );
					}
				}
			}

			if ( Zone( Loop ).isNominalControlled ) { //conditioned zones only
				if ( Zone( Loop ).TotOccupants > 0.0 ) {
					Zone( Loop ).isNominalOccupied = true;
					PreDefTableEntry( pdchOaoNomNumOcc1, Zone( Loop ).Name, Zone( Loop ).TotOccupants );
					PreDefTableEntry( pdchOaoNomNumOcc2, Zone( Loop ).Name, Zone( Loop ).TotOccupants );
				}
			}
		}

		RepVarSet = true;
		CurrentModuleObject = "Lights";
		NumLightsStatements = GetNumObjectsFound( CurrentModuleObject );
		LightsObjects.allocate( NumLightsStatements );

		TotLights = 0;
		errFlag = false;
		for ( Item = 1; Item <= NumLightsStatements; ++Item ) {
			GetObjectItem( CurrentModuleObject, Item, AlphaName, NumAlpha, IHGNumbers, NumNumber, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( AlphaName( 1 ), LightsObjects, Item - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				errFlag = true;
				if ( IsBlank ) AlphaName( 1 ) = "xxxxx";
			}
			LightsObjects( Item ).Name = AlphaName( 1 );

			Item1 = FindItemInList( AlphaName( 2 ), Zone );
			ZLItem = 0;
			if ( Item1 == 0 && NumOfZoneLists > 0 ) ZLItem = FindItemInList( AlphaName( 2 ), ZoneList );
			if ( Item1 > 0 ) {
				LightsObjects( Item ).StartPtr = TotLights + 1;
				++TotLights;
				LightsObjects( Item ).NumOfZones = 1;
				LightsObjects( Item ).ZoneListActive = false;
				LightsObjects( Item ).ZoneOrZoneListPtr = Item1;
			} else if ( ZLItem > 0 ) {
				LightsObjects( Item ).StartPtr = TotLights + 1;
				TotLights += ZoneList( ZLItem ).NumOfZones;
				LightsObjects( Item ).NumOfZones = ZoneList( ZLItem ).NumOfZones;
				LightsObjects( Item ).ZoneListActive = true;
				LightsObjects( Item ).ZoneOrZoneListPtr = ZLItem;
			} else {
				ShowSevereError( CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\" invalid " + cAlphaFieldNames( 2 ) + "=\"" + AlphaName( 2 ) + "\" not found." );
				ErrorsFound = true;
				errFlag = true;
			}
		}

		if ( errFlag ) {
			ShowSevereError( RoutineName + "Errors with invalid names in " + CurrentModuleObject + " objects." );
			ShowContinueError( "...These will not be read in.  Other errors may occur." );
			TotLights = 0;
		}

		Lights.allocate( TotLights );

		if ( TotLights > 0 ) {
			Loop = 0;
			for ( Item = 1; Item <= NumLightsStatements; ++Item ) {
				AlphaName = BlankString;
				IHGNumbers = 0.0;

				GetObjectItem( CurrentModuleObject, Item, AlphaName, NumAlpha, IHGNumbers, NumNumber, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

				for ( Item1 = 1; Item1 <= LightsObjects( Item ).NumOfZones; ++Item1 ) {
					++Loop;
					if ( ! LightsObjects( Item ).ZoneListActive ) {
						Lights( Loop ).Name = AlphaName( 1 );
						Lights( Loop ).ZonePtr = LightsObjects( Item ).ZoneOrZoneListPtr;
					} else {
						CheckCreatedZoneItemName( RoutineName, CurrentModuleObject, Zone( ZoneList( LightsObjects( Item ).ZoneOrZoneListPtr ).Zone( Item1 ) ).Name, ZoneList( LightsObjects( Item ).ZoneOrZoneListPtr ).MaxZoneNameLength, LightsObjects( Item ).Name, Lights, Loop - 1, Lights( Loop ).Name, errFlag );
						Lights( Loop ).ZonePtr = ZoneList( LightsObjects( Item ).ZoneOrZoneListPtr ).Zone( Item1 );
						if ( errFlag ) ErrorsFound = true;
					}

					Lights( Loop ).SchedPtr = GetScheduleIndex( AlphaName( 3 ) );
					SchMin = 0.0;
					SchMax = 0.0;
					if ( Lights( Loop ).SchedPtr == 0 ) {
						if ( Item1 == 1 ) {
							if ( lAlphaFieldBlanks( 3 ) ) {
								ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", " + cAlphaFieldNames( 3 ) + " is required." );
							} else {
								ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", invalid " + cAlphaFieldNames( 3 ) + " entered=" + AlphaName( 3 ) );
							}
							ErrorsFound = true;
						}
					} else { // check min/max on schedule
						SchMin = GetScheduleMinValue( Lights( Loop ).SchedPtr );
						SchMax = GetScheduleMaxValue( Lights( Loop ).SchedPtr );
						if ( SchMin < 0.0 || SchMax < 0.0 ) {
							if ( Item1 == 1 ) {
								if ( SchMin < 0.0 ) {
									ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", " + cAlphaFieldNames( 3 ) + ", minimum is < 0.0" );
									ShowContinueError( "Schedule=\"" + AlphaName( 3 ) + "\". Minimum is [" + RoundSigDigits( SchMin, 1 ) + "]. Values must be >= 0.0." );
									ErrorsFound = true;
								}
							}
							if ( Item1 == 1 ) {
								if ( SchMax < 0.0 ) {
									ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", " + cAlphaFieldNames( 3 ) + ", maximum is < 0.0" );
									ShowContinueError( "Schedule=\"" + AlphaName( 3 ) + "\". Maximum is [" + RoundSigDigits( SchMax, 1 ) + "]. Values must be >= 0.0." );
									ErrorsFound = true;
								}
							}
						}
					}

					// Lights Design Level calculation method.
					{ auto const lightingLevel( AlphaName( 4 ) );
					if ( lightingLevel == "LIGHTINGLEVEL" ) {
						Lights( Loop ).DesignLevel = IHGNumbers( 1 );
						if ( lNumericFieldBlanks( 1 ) ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Lights( Loop ).Name + "\", specifies " + cNumericFieldNames( 1 ) + ", but that field is blank.  0 Lights will result." );
						}

					} else if ( lightingLevel == "WATTS/AREA" ) {
						if ( Lights( Loop ).ZonePtr != 0 ) {
							if ( IHGNumbers( 2 ) >= 0.0 ) {
								Lights( Loop ).DesignLevel = IHGNumbers( 2 ) * Zone( Lights( Loop ).ZonePtr ).FloorArea;
								if ( Zone( Lights( Loop ).ZonePtr ).FloorArea <= 0.0 ) {
									ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Lights( Loop ).Name + "\", specifies " + cNumericFieldNames( 2 ) + ", but Zone Floor Area = 0.  0 Lights will result." );
								}
							} else {
								ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Lights( Loop ).Name + "\", invalid " + cNumericFieldNames( 2 ) + ", value  [<0.0]=" + RoundSigDigits( IHGNumbers( 2 ), 3 ) );
								ErrorsFound = true;
							}
						}
						if ( lNumericFieldBlanks( 2 ) ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Lights( Loop ).Name + "\", specifies " + cNumericFieldNames( 2 ) + ", but that field is blank.  0 Lights will result." );
						}

					} else if ( lightingLevel == "WATTS/PERSON" ) {
						if ( Lights( Loop ).ZonePtr != 0 ) {
							if ( IHGNumbers( 3 ) >= 0.0 ) {
								Lights( Loop ).DesignLevel = IHGNumbers( 3 ) * Zone( Lights( Loop ).ZonePtr ).TotOccupants;
								if ( Zone( Lights( Loop ).ZonePtr ).TotOccupants <= 0.0 ) {
									ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Lights( Loop ).Name + "\", specifies " + cNumericFieldNames( 2 ) + ", but Total Occupants = 0.  0 Lights will result." );
								}
							} else {
								ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Lights( Loop ).Name + "\", invalid " + cNumericFieldNames( 3 ) + ", value  [<0.0]=" + RoundSigDigits( IHGNumbers( 3 ), 3 ) );
								ErrorsFound = true;
							}
						}
						if ( lNumericFieldBlanks( 3 ) ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Lights( Loop ).Name + "\", specifies " + cNumericFieldNames( 3 ) + ", but that field is blank.  0 Lights will result." );
						}

					} else {
						if ( Item1 == 1 ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", invalid " + cAlphaFieldNames( 4 ) + ", value  =" + AlphaName( 4 ) );
							ShowContinueError( "...Valid values are \"LightingLevel\", \"Watts/Area\", \"Watts/Person\"." );
							ErrorsFound = true;
						}
					}}

					// Calculate nominal min/max lighting level
					Lights( Loop ).NomMinDesignLevel = Lights( Loop ).DesignLevel * SchMin;
					Lights( Loop ).NomMaxDesignLevel = Lights( Loop ).DesignLevel * SchMax;

					Lights( Loop ).FractionReturnAir = IHGNumbers( 4 );
					Lights( Loop ).FractionRadiant = IHGNumbers( 5 );
					Lights( Loop ).FractionShortWave = IHGNumbers( 6 );
					Lights( Loop ).FractionReplaceable = IHGNumbers( 7 );
					Lights( Loop ).FractionReturnAirPlenTempCoeff1 = IHGNumbers( 8 );
					Lights( Loop ).FractionReturnAirPlenTempCoeff2 = IHGNumbers( 9 );

					Lights( Loop ).FractionConvected = 1.0 - ( Lights( Loop ).FractionReturnAir + Lights( Loop ).FractionRadiant + Lights( Loop ).FractionShortWave );
					if ( std::abs( Lights( Loop ).FractionConvected ) <= 0.001 ) Lights( Loop ).FractionConvected = 0.0;
					if ( Lights( Loop ).FractionConvected < 0.0 ) {
						if ( Item1 == 1 ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", Sum of Fractions > 1.0" );
							ErrorsFound = true;
						}
					}

					// Note: if FractionReturnAirIsCalculated = Yes and there is a return-air plenum:
					// (1) The input values of FractionReturnAir, FractionRadiant and FractionShortWave, and the
					// value of FractionConvected calculated from these are used in the zone sizing calculations;
					// (2) in the regular calculation, FractionReturnAir is calculated each time step in
					// Subr. InitInternalHeatGains as a function of the zone's return plenum air temperature
					// using FractionReturnAirPlenTempCoeff1 and FractionReturnAirPlenTempCoeff2; then
					// FractionRadiant and FractionConvected are adjusted from their input values such that
					// FractionReturnAir + FractionRadiant + FractionShortWave + FractionConvected = 1.0, assuming
					// FractionShortWave is constant and equal to its input value.

					if ( NumAlpha > 4 ) {
						Lights( Loop ).EndUseSubcategory = AlphaName( 5 );
					} else {
						Lights( Loop ).EndUseSubcategory = "General";
					}

					if ( lAlphaFieldBlanks( 6 ) ) {
						Lights( Loop ).FractionReturnAirIsCalculated = false;
					} else if ( AlphaName( 6 ) != "YES" && AlphaName( 6 ) != "NO" ) {
						if ( Item1 == 1 ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", invalid " + cAlphaFieldNames( 6 ) + ", value  =" + AlphaName( 6 ) );
							ShowContinueError( ".. Return Air Fraction from Plenum will NOT be calculated." );
						}
						Lights( Loop ).FractionReturnAirIsCalculated = false;
					} else {
						Lights( Loop ).FractionReturnAirIsCalculated = ( AlphaName( 6 ) == "YES" );
					}

					if ( Lights( Loop ).ZonePtr <= 0 ) continue; // Error, will be caught and terminated later

					// Object report variables
					SetupOutputVariable( "Lights Electric Power [W]", Lights( Loop ).Power, "Zone", "Average", Lights( Loop ).Name );

					SetupOutputVariable( "Lights Radiant Heating Energy [J]", Lights( Loop ).RadGainEnergy, "Zone", "Sum", Lights( Loop ).Name );
					SetupOutputVariable( "Lights Radiant Heating Rate [W]", Lights( Loop ).RadGainRate, "Zone", "Average", Lights( Loop ).Name );
					SetupOutputVariable( "Lights Visible Radiation Heating Energy [J]", Lights( Loop ).VisGainEnergy, "Zone", "Sum", Lights( Loop ).Name );
					SetupOutputVariable( "Lights Visible Radiation Heating Rate [W]", Lights( Loop ).VisGainRate, "Zone", "Average", Lights( Loop ).Name );
					SetupOutputVariable( "Lights Convective Heating Energy [J]", Lights( Loop ).ConGainEnergy, "Zone", "Sum", Lights( Loop ).Name );
					SetupOutputVariable( "Lights Convective Heating Rate [W]", Lights( Loop ).ConGainRate, "Zone", "Average", Lights( Loop ).Name );
					SetupOutputVariable( "Lights Return Air Heating Energy [J]", Lights( Loop ).RetAirGainEnergy, "Zone", "Sum", Lights( Loop ).Name );
					SetupOutputVariable( "Lights Return Air Heating Rate [W]", Lights( Loop ).RetAirGainRate, "Zone", "Average", Lights( Loop ).Name );
					SetupOutputVariable( "Lights Total Heating Energy [J]", Lights( Loop ).TotGainEnergy, "Zone", "Sum", Lights( Loop ).Name );
					SetupOutputVariable( "Lights Total Heating Rate [W]", Lights( Loop ).TotGainRate, "Zone", "Average", Lights( Loop ).Name );
					SetupOutputVariable( "Lights Electric Energy [J]", Lights( Loop ).Consumption, "Zone", "Sum", Lights( Loop ).Name, _, "Electricity", "InteriorLights", Lights( Loop ).EndUseSubcategory, "Building", Zone( Lights( Loop ).ZonePtr ).Name, Zone( Lights( Loop ).ZonePtr ).Multiplier, Zone( Lights( Loop ).ZonePtr ).ListMultiplier );

					// Zone total report variables
					if ( RepVarSet( Lights( Loop ).ZonePtr ) ) {
						RepVarSet( Lights( Loop ).ZonePtr ) = false;
						SetupOutputVariable( "Zone Lights Electric Power [W]", ZnRpt( Lights( Loop ).ZonePtr ).LtsPower, "Zone", "Average", Zone( Lights( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Lights Electric Energy [J]", ZnRpt( Lights( Loop ).ZonePtr ).LtsElecConsump, "Zone", "Sum", Zone( Lights( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Lights Radiant Heating Energy [J]", ZnRpt( Lights( Loop ).ZonePtr ).LtsRadGain, "Zone", "Sum", Zone( Lights( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Lights Radiant Heating Rate [W]", ZnRpt( Lights( Loop ).ZonePtr ).LtsRadGainRate, "Zone", "Average", Zone( Lights( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Lights Visible Radiation Heating Energy [J]", ZnRpt( Lights( Loop ).ZonePtr ).LtsVisGain, "Zone", "Sum", Zone( Lights( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Lights Visible Radiation Heating Rate [W]", ZnRpt( Lights( Loop ).ZonePtr ).LtsVisGainRate, "Zone", "Average", Zone( Lights( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Lights Convective Heating Energy [J]", ZnRpt( Lights( Loop ).ZonePtr ).LtsConGain, "Zone", "Sum", Zone( Lights( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Lights Convective Heating Rate [W]", ZnRpt( Lights( Loop ).ZonePtr ).LtsConGainRate, "Zone", "Average", Zone( Lights( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Lights Return Air Heating Energy [J]", ZnRpt( Lights( Loop ).ZonePtr ).LtsRetAirGain, "Zone", "Sum", Zone( Lights( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Lights Return Air Heating Rate [W]", ZnRpt( Lights( Loop ).ZonePtr ).LtsRetAirGainRate, "Zone", "Average", Zone( Lights( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Lights Total Heating Energy [J]", ZnRpt( Lights( Loop ).ZonePtr ).LtsTotGain, "Zone", "Sum", Zone( Lights( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Lights Total Heating Rate [W]", ZnRpt( Lights( Loop ).ZonePtr ).LtsTotGainRate, "Zone", "Average", Zone( Lights( Loop ).ZonePtr ).Name );
					}

					if ( AnyEnergyManagementSystemInModel ) {
						SetupEMSActuator( "Lights", Lights( Loop ).Name, "Electric Power Level", "[W]", Lights( Loop ).EMSLightsOn, Lights( Loop ).EMSLightingPower );
						SetupEMSInternalVariable( "Lighting Power Design Level", Lights( Loop ).Name, "[W]", Lights( Loop ).DesignLevel );
					} // EMS
					//setup internal gains
					if ( ! ErrorsFound ) SetupZoneInternalGain( Lights( Loop ).ZonePtr, "Lights", Lights( Loop ).Name, IntGainTypeOf_Lights, Lights( Loop ).ConGainRate, Lights( Loop ).RetAirGainRate, Lights( Loop ).RadGainRate );

					// send values to predefined lighting summary report
					liteName = Lights( Loop ).Name;
					zonePt = Lights( Loop ).ZonePtr;
					mult = Zone( zonePt ).Multiplier * Zone( zonePt ).ListMultiplier;
					sumArea += Zone( zonePt ).FloorArea * mult;
					sumPower += Lights( Loop ).DesignLevel * mult;
					PreDefTableEntry( pdchInLtZone, liteName, Zone( zonePt ).Name );
					if ( Zone( zonePt ).FloorArea > 0.0 ) {
						PreDefTableEntry( pdchInLtDens, liteName, Lights( Loop ).DesignLevel / Zone( zonePt ).FloorArea, 4 );
					} else {
						PreDefTableEntry( pdchInLtDens, liteName, constant_zero, 4 );
					}
					PreDefTableEntry( pdchInLtArea, liteName, Zone( zonePt ).FloorArea * mult );
					PreDefTableEntry( pdchInLtPower, liteName, Lights( Loop ).DesignLevel * mult );
					PreDefTableEntry( pdchInLtEndUse, liteName, Lights( Loop ).EndUseSubcategory );
					PreDefTableEntry( pdchInLtSchd, liteName, GetScheduleName( Lights( Loop ).SchedPtr ) );
					PreDefTableEntry( pdchInLtRetAir, liteName, Lights( Loop ).FractionReturnAir, 4 );
				} // Item1 - zones
			} // Item = Number of Lights Objects
		} // TotLights > 0 check
		// add total line to lighting summary table
		if ( sumArea > 0.0 ) {
			PreDefTableEntry( pdchInLtDens, "Interior Lighting Total", sumPower / sumArea, 4 ); //** line 792
		} else {
			PreDefTableEntry( pdchInLtDens, "Interior Lighting Total", constant_zero, 4 );
		}
		PreDefTableEntry( pdchInLtArea, "Interior Lighting Total", sumArea );
		PreDefTableEntry( pdchInLtPower, "Interior Lighting Total", sumPower );

		RepVarSet = true;
		CurrentModuleObject = "ElectricEquipment";
		NumZoneElectricStatements = GetNumObjectsFound( CurrentModuleObject );
		ZoneElectricObjects.allocate( NumZoneElectricStatements );

		TotElecEquip = 0;
		errFlag = false;
		for ( Item = 1; Item <= NumZoneElectricStatements; ++Item ) {
			GetObjectItem( CurrentModuleObject, Item, AlphaName, NumAlpha, IHGNumbers, NumNumber, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( AlphaName( 1 ), ZoneElectricObjects, Item - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				errFlag = true;
				if ( IsBlank ) AlphaName( 1 ) = "xxxxx";
			}
			ZoneElectricObjects( Item ).Name = AlphaName( 1 );

			Item1 = FindItemInList( AlphaName( 2 ), Zone );
			ZLItem = 0;
			if ( Item1 == 0 && NumOfZoneLists > 0 ) ZLItem = FindItemInList( AlphaName( 2 ), ZoneList );
			if ( Item1 > 0 ) {
				ZoneElectricObjects( Item ).StartPtr = TotElecEquip + 1;
				++TotElecEquip;
				ZoneElectricObjects( Item ).NumOfZones = 1;
				ZoneElectricObjects( Item ).ZoneListActive = false;
				ZoneElectricObjects( Item ).ZoneOrZoneListPtr = Item1;
			} else if ( ZLItem > 0 ) {
				ZoneElectricObjects( Item ).StartPtr = TotElecEquip + 1;
				TotElecEquip += ZoneList( ZLItem ).NumOfZones;
				ZoneElectricObjects( Item ).NumOfZones = ZoneList( ZLItem ).NumOfZones;
				ZoneElectricObjects( Item ).ZoneListActive = true;
				ZoneElectricObjects( Item ).ZoneOrZoneListPtr = ZLItem;
			} else {
				ShowSevereError( CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\" invalid " + cAlphaFieldNames( 2 ) + "=\"" + AlphaName( 2 ) + "\" not found." );
				ErrorsFound = true;
				errFlag = true;
			}
		}

		if ( errFlag ) {
			ShowSevereError( RoutineName + "Errors with invalid names in " + CurrentModuleObject + " objects." );
			ShowContinueError( "...These will not be read in.  Other errors may occur." );
			TotElecEquip = 0;
		}

		ZoneElectric.allocate( TotElecEquip );

		if ( TotElecEquip > 0 ) {
			Loop = 0;
			for ( Item = 1; Item <= NumZoneElectricStatements; ++Item ) {
				AlphaName = BlankString;
				IHGNumbers = 0.0;

				GetObjectItem( CurrentModuleObject, Item, AlphaName, NumAlpha, IHGNumbers, NumNumber, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

				for ( Item1 = 1; Item1 <= ZoneElectricObjects( Item ).NumOfZones; ++Item1 ) {
					++Loop;
					if ( ! ZoneElectricObjects( Item ).ZoneListActive ) {
						ZoneElectric( Loop ).Name = AlphaName( 1 );
						ZoneElectric( Loop ).ZonePtr = ZoneElectricObjects( Item ).ZoneOrZoneListPtr;
					} else {
						CheckCreatedZoneItemName( RoutineName, CurrentModuleObject, Zone( ZoneList( ZoneElectricObjects( Item ).ZoneOrZoneListPtr ).Zone( Item1 ) ).Name, ZoneList( ZoneElectricObjects( Item ).ZoneOrZoneListPtr ).MaxZoneNameLength, ZoneElectricObjects( Item ).Name, ZoneElectric, Loop - 1, ZoneElectric( Loop ).Name, errFlag );
						ZoneElectric( Loop ).ZonePtr = ZoneList( ZoneElectricObjects( Item ).ZoneOrZoneListPtr ).Zone( Item1 );
						if ( errFlag ) ErrorsFound = true;
					}

					ZoneElectric( Loop ).SchedPtr = GetScheduleIndex( AlphaName( 3 ) );
					SchMin = 0.0;
					SchMax = 0.0;
					if ( ZoneElectric( Loop ).SchedPtr == 0 ) {
						if ( lAlphaFieldBlanks( 3 ) ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", " + cAlphaFieldNames( 3 ) + " is required." );
						} else {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", invalid " + cAlphaFieldNames( 3 ) + " entered=" + AlphaName( 3 ) );
						}
						ErrorsFound = true;
					} else { // check min/max on schedule
						SchMin = GetScheduleMinValue( ZoneElectric( Loop ).SchedPtr );
						SchMax = GetScheduleMaxValue( ZoneElectric( Loop ).SchedPtr );
						if ( SchMin < 0.0 || SchMax < 0.0 ) {
							if ( SchMin < 0.0 ) {
								ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", " + cAlphaFieldNames( 3 ) + ", minimum is < 0.0" );
								ShowContinueError( "Schedule=\"" + AlphaName( 3 ) + "\". Minimum is [" + RoundSigDigits( SchMin, 1 ) + "]. Values must be >= 0.0." );
								ErrorsFound = true;
							}
							if ( SchMax < 0.0 ) {
								ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", " + cAlphaFieldNames( 3 ) + ", maximum is < 0.0" );
								ShowContinueError( "Schedule=\"" + AlphaName( 3 ) + "\". Maximum is [" + RoundSigDigits( SchMax, 1 ) + "]. Values must be >= 0.0." );
								ErrorsFound = true;
							}
						}
					}

					// Electric equipment design level calculation method.
					{ auto const equipmentLevel( AlphaName( 4 ) );
					if ( equipmentLevel == "EQUIPMENTLEVEL" ) {
						ZoneElectric( Loop ).DesignLevel = IHGNumbers( 1 );
						if ( lNumericFieldBlanks( 1 ) ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", specifies " + cNumericFieldNames( 1 ) + ", but that field is blank.  0 Electric Equipment will result." );
						}

					} else if ( equipmentLevel == "WATTS/AREA" ) {
						if ( ZoneElectric( Loop ).ZonePtr != 0 ) {
							if ( IHGNumbers( 2 ) >= 0.0 ) {
								ZoneElectric( Loop ).DesignLevel = IHGNumbers( 2 ) * Zone( ZoneElectric( Loop ).ZonePtr ).FloorArea;
								if ( Zone( ZoneElectric( Loop ).ZonePtr ).FloorArea <= 0.0 ) {
									ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", specifies " + cNumericFieldNames( 2 ) + ", but Zone Floor Area = 0.  0 Electric Equipment will result." );
								}
							} else {
								ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", invalid " + cNumericFieldNames( 2 ) + ", value  [<0.0]=" + RoundSigDigits( IHGNumbers( 2 ), 3 ) );
								ErrorsFound = true;
							}
						}
						if ( lNumericFieldBlanks( 2 ) ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", specifies " + cNumericFieldNames( 2 ) + ", but that field is blank.  0 Electric Equipment will result." );
						}

					} else if ( equipmentLevel == "WATTS/PERSON" ) {
						if ( ZoneElectric( Loop ).ZonePtr != 0 ) {
							if ( IHGNumbers( 3 ) >= 0.0 ) {
								ZoneElectric( Loop ).DesignLevel = IHGNumbers( 3 ) * Zone( ZoneElectric( Loop ).ZonePtr ).TotOccupants;
								if ( Zone( ZoneElectric( Loop ).ZonePtr ).TotOccupants <= 0.0 ) {
									ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", specifies " + cNumericFieldNames( 2 ) + ", but Total Occupants = 0.  0 Electric Equipment will result." );
								}
							} else {
								ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", invalid " + cNumericFieldNames( 3 ) + ", value  [<0.0]=" + RoundSigDigits( IHGNumbers( 3 ), 3 ) );
								ErrorsFound = true;
							}
						}
						if ( lNumericFieldBlanks( 3 ) ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", specifies " + cNumericFieldNames( 3 ) + ", but that field is blank.  0 Electric Equipment will result." );
						}

					} else {
						if ( Item1 == 1 ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", invalid " + cAlphaFieldNames( 4 ) + ", value  =" + AlphaName( 4 ) );
							ShowContinueError( "...Valid values are \"EquipmentLevel\", \"Watts/Area\", \"Watts/Person\"." );
							ErrorsFound = true;
						}
					}}

					// Calculate nominal min/max equipment level
					ZoneElectric( Loop ).NomMinDesignLevel = ZoneElectric( Loop ).DesignLevel * SchMin;
					ZoneElectric( Loop ).NomMaxDesignLevel = ZoneElectric( Loop ).DesignLevel * SchMax;

					ZoneElectric( Loop ).FractionLatent = IHGNumbers( 4 );
					ZoneElectric( Loop ).FractionRadiant = IHGNumbers( 5 );
					ZoneElectric( Loop ).FractionLost = IHGNumbers( 6 );
					// FractionConvected is a calculated field
					ZoneElectric( Loop ).FractionConvected = 1.0 - ( ZoneElectric( Loop ).FractionLatent + ZoneElectric( Loop ).FractionRadiant + ZoneElectric( Loop ).FractionLost );
					if ( std::abs( ZoneElectric( Loop ).FractionConvected ) <= 0.001 ) ZoneElectric( Loop ).FractionConvected = 0.0;
					if ( ZoneElectric( Loop ).FractionConvected < 0.0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", Sum of Fractions > 1.0" );
						ErrorsFound = true;
					}

					if ( NumAlpha > 4 ) {
						ZoneElectric( Loop ).EndUseSubcategory = AlphaName( 5 );
					} else {
						ZoneElectric( Loop ).EndUseSubcategory = "General";
					}

					if ( ZoneElectric( Loop ).ZonePtr <= 0 ) continue; // Error, will be caught and terminated later

					// Object report variables
					SetupOutputVariable( "Electric Equipment Electric Power [W]", ZoneElectric( Loop ).Power, "Zone", "Average", ZoneElectric( Loop ).Name );
					SetupOutputVariable( "Electric Equipment Electric Energy [J]", ZoneElectric( Loop ).Consumption, "Zone", "Sum", ZoneElectric( Loop ).Name, _, "Electricity", "InteriorEquipment", ZoneElectric( Loop ).EndUseSubcategory, "Building", Zone( ZoneElectric( Loop ).ZonePtr ).Name, Zone( ZoneElectric( Loop ).ZonePtr ).Multiplier, Zone( ZoneElectric( Loop ).ZonePtr ).ListMultiplier );

					SetupOutputVariable( "Electric Equipment Radiant Heating Energy [J]", ZoneElectric( Loop ).RadGainEnergy, "Zone", "Sum", ZoneElectric( Loop ).Name );
					SetupOutputVariable( "Electric Equipment Radiant Heating Rate [W]", ZoneElectric( Loop ).RadGainRate, "Zone", "Average", ZoneElectric( Loop ).Name );
					SetupOutputVariable( "Electric Equipment Convective Heating Energy [J]", ZoneElectric( Loop ).ConGainEnergy, "Zone", "Sum", ZoneElectric( Loop ).Name );
					SetupOutputVariable( "Electric Equipment Convective Heating Rate [W]", ZoneElectric( Loop ).ConGainRate, "Zone", "Average", ZoneElectric( Loop ).Name );
					SetupOutputVariable( "Electric Equipment Latent Gain Energy [J]", ZoneElectric( Loop ).LatGainEnergy, "Zone", "Sum", ZoneElectric( Loop ).Name );
					SetupOutputVariable( "Electric Equipment Latent Gain Rate [W]", ZoneElectric( Loop ).LatGainRate, "Zone", "Average", ZoneElectric( Loop ).Name );
					SetupOutputVariable( "Electric Equipment Lost Heat Energy [J]", ZoneElectric( Loop ).LostEnergy, "Zone", "Sum", ZoneElectric( Loop ).Name );
					SetupOutputVariable( "Electric Equipment Lost Heat Rate [W]", ZoneElectric( Loop ).LostRate, "Zone", "Average", ZoneElectric( Loop ).Name );
					SetupOutputVariable( "Electric Equipment Total Heating Energy [J]", ZoneElectric( Loop ).TotGainEnergy, "Zone", "Sum", ZoneElectric( Loop ).Name );
					SetupOutputVariable( "Electric Equipment Total Heating Rate [W]", ZoneElectric( Loop ).TotGainRate, "Zone", "Average", ZoneElectric( Loop ).Name );

					// Zone total report variables
					if ( RepVarSet( ZoneElectric( Loop ).ZonePtr ) ) {
						RepVarSet( ZoneElectric( Loop ).ZonePtr ) = false;
						SetupOutputVariable( "Zone Electric Equipment Electric Power [W]", ZnRpt( ZoneElectric( Loop ).ZonePtr ).ElecPower, "Zone", "Average", Zone( ZoneElectric( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Electric Equipment Electric Energy [J]", ZnRpt( ZoneElectric( Loop ).ZonePtr ).ElecConsump, "Zone", "Sum", Zone( ZoneElectric( Loop ).ZonePtr ).Name );

						SetupOutputVariable( "Zone Electric Equipment Radiant Heating Energy [J]", ZnRpt( ZoneElectric( Loop ).ZonePtr ).ElecRadGain, "Zone", "Sum", Zone( ZoneElectric( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Electric Equipment Radiant Heating Rate [W]", ZnRpt( ZoneElectric( Loop ).ZonePtr ).ElecRadGainRate, "Zone", "Average", Zone( ZoneElectric( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Electric Equipment Convective Heating Energy [J]", ZnRpt( ZoneElectric( Loop ).ZonePtr ).ElecConGain, "Zone", "Sum", Zone( ZoneElectric( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Electric Equipment Convective Heating Rate [W]", ZnRpt( ZoneElectric( Loop ).ZonePtr ).ElecConGainRate, "Zone", "Average", Zone( ZoneElectric( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Electric Equipment Latent Gain Energy [J]", ZnRpt( ZoneElectric( Loop ).ZonePtr ).ElecLatGain, "Zone", "Sum", Zone( ZoneElectric( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Electric Equipment Latent Gain Rate [W]", ZnRpt( ZoneElectric( Loop ).ZonePtr ).ElecLatGainRate, "Zone", "Average", Zone( ZoneElectric( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Electric Equipment Lost Heat Energy [J]", ZnRpt( ZoneElectric( Loop ).ZonePtr ).ElecLost, "Zone", "Sum", Zone( ZoneElectric( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Electric Equipment Lost Heat Rate [W]", ZnRpt( ZoneElectric( Loop ).ZonePtr ).ElecLostRate, "Zone", "Average", Zone( ZoneElectric( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Electric Equipment Total Heating Energy [J]", ZnRpt( ZoneElectric( Loop ).ZonePtr ).ElecTotGain, "Zone", "Sum", Zone( ZoneElectric( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Electric Equipment Total Heating Rate [W]", ZnRpt( ZoneElectric( Loop ).ZonePtr ).ElecTotGainRate, "Zone", "Average", Zone( ZoneElectric( Loop ).ZonePtr ).Name );
					}

					if ( AnyEnergyManagementSystemInModel ) {
						SetupEMSActuator( "ElectricEquipment", ZoneElectric( Loop ).Name, "Electric Power Level", "[W]", ZoneElectric( Loop ).EMSZoneEquipOverrideOn, ZoneElectric( Loop ).EMSEquipPower );
						SetupEMSInternalVariable( "Plug and Process Power Design Level", ZoneElectric( Loop ).Name, "[W]", ZoneElectric( Loop ).DesignLevel );
					} // EMS

					if ( ! ErrorsFound ) SetupZoneInternalGain( ZoneElectric( Loop ).ZonePtr, "ElectricEquipment", ZoneElectric( Loop ).Name, IntGainTypeOf_ElectricEquipment, ZoneElectric( Loop ).ConGainRate, _, ZoneElectric( Loop ).RadGainRate, ZoneElectric( Loop ).LatGainRate );

				} // Item1
			} // Item - Number of ZoneElectric objects
		} // Check on number of ZoneElectric

		RepVarSet = true;
		CurrentModuleObject = "GasEquipment";
		NumZoneGasStatements = GetNumObjectsFound( CurrentModuleObject );
		ZoneGasObjects.allocate( NumZoneGasStatements );

		TotGasEquip = 0;
		errFlag = false;
		for ( Item = 1; Item <= NumZoneGasStatements; ++Item ) {
			GetObjectItem( CurrentModuleObject, Item, AlphaName, NumAlpha, IHGNumbers, NumNumber, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( AlphaName( 1 ), ZoneGasObjects, Item - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				errFlag = true;
				if ( IsBlank ) AlphaName( 1 ) = "xxxxx";
			}
			ZoneGasObjects( Item ).Name = AlphaName( 1 );

			Item1 = FindItemInList( AlphaName( 2 ), Zone );
			ZLItem = 0;
			if ( Item1 == 0 && NumOfZoneLists > 0 ) ZLItem = FindItemInList( AlphaName( 2 ), ZoneList );
			if ( Item1 > 0 ) {
				ZoneGasObjects( Item ).StartPtr = TotGasEquip + 1;
				++TotGasEquip;
				ZoneGasObjects( Item ).NumOfZones = 1;
				ZoneGasObjects( Item ).ZoneListActive = false;
				ZoneGasObjects( Item ).ZoneOrZoneListPtr = Item1;
			} else if ( ZLItem > 0 ) {
				ZoneGasObjects( Item ).StartPtr = TotGasEquip + 1;
				TotGasEquip += ZoneList( ZLItem ).NumOfZones;
				ZoneGasObjects( Item ).NumOfZones = ZoneList( ZLItem ).NumOfZones;
				ZoneGasObjects( Item ).ZoneListActive = true;
				ZoneGasObjects( Item ).ZoneOrZoneListPtr = ZLItem;
			} else {
				ShowSevereError( CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\" invalid " + cAlphaFieldNames( 2 ) + "=\"" + AlphaName( 2 ) + "\" not found." );
				ErrorsFound = true;
				errFlag = true;
			}
		}

		if ( errFlag ) {
			ShowSevereError( RoutineName + "Errors with invalid names in " + CurrentModuleObject + " objects." );
			ShowContinueError( "...These will not be read in.  Other errors may occur." );
			TotGasEquip = 0;
		}

		ZoneGas.allocate( TotGasEquip );

		if ( TotGasEquip > 0 ) {
			Loop = 0;
			for ( Item = 1; Item <= NumZoneGasStatements; ++Item ) {
				AlphaName = BlankString;
				IHGNumbers = 0.0;

				GetObjectItem( CurrentModuleObject, Item, AlphaName, NumAlpha, IHGNumbers, NumNumber, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

				for ( Item1 = 1; Item1 <= ZoneGasObjects( Item ).NumOfZones; ++Item1 ) {
					++Loop;
					if ( ! ZoneGasObjects( Item ).ZoneListActive ) {
						ZoneGas( Loop ).Name = AlphaName( 1 );
						ZoneGas( Loop ).ZonePtr = ZoneGasObjects( Item ).ZoneOrZoneListPtr;
					} else {
						CheckCreatedZoneItemName( RoutineName, CurrentModuleObject, Zone( ZoneList( ZoneGasObjects( Item ).ZoneOrZoneListPtr ).Zone( Item1 ) ).Name, ZoneList( ZoneGasObjects( Item ).ZoneOrZoneListPtr ).MaxZoneNameLength, ZoneGasObjects( Item ).Name, ZoneGas, Loop - 1, ZoneGas( Loop ).Name, errFlag );
						ZoneGas( Loop ).ZonePtr = ZoneList( ZoneGasObjects( Item ).ZoneOrZoneListPtr ).Zone( Item1 );
						if ( errFlag ) ErrorsFound = true;
					}

					ZoneGas( Loop ).SchedPtr = GetScheduleIndex( AlphaName( 3 ) );
					SchMin = 0.0;
					SchMax = 0.0;
					if ( ZoneGas( Loop ).SchedPtr == 0 ) {
						if ( Item1 == 1 ) {
							if ( lAlphaFieldBlanks( 3 ) ) {
								ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", " + cAlphaFieldNames( 3 ) + " is required." );
							} else {
								ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", invalid " + cAlphaFieldNames( 3 ) + " entered=" + AlphaName( 3 ) );
							}
							ErrorsFound = true;
						}
					} else { // check min/max on schedule
						SchMin = GetScheduleMinValue( ZoneGas( Loop ).SchedPtr );
						SchMax = GetScheduleMaxValue( ZoneGas( Loop ).SchedPtr );
						if ( SchMin < 0.0 || SchMax < 0.0 ) {
							if ( Item1 == 1 ) {
								if ( SchMin < 0.0 ) {
									ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", " + cAlphaFieldNames( 3 ) + ", minimum is < 0.0" );
									ShowContinueError( "Schedule=\"" + AlphaName( 3 ) + "\". Minimum is [" + RoundSigDigits( SchMin, 1 ) + "]. Values must be >= 0.0." );
									ErrorsFound = true;
								}
							}
							if ( Item1 == 1 ) {
								if ( SchMax < 0.0 ) {
									ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", " + cAlphaFieldNames( 3 ) + ", maximum is < 0.0" );
									ShowContinueError( "Schedule=\"" + AlphaName( 3 ) + "\". Maximum is [" + RoundSigDigits( SchMax, 1 ) + "]. Values must be >= 0.0." );
									ErrorsFound = true;
								}
							}
						}
					}

					// equipment design level calculation method.
					{ auto const equipmentLevel( AlphaName( 4 ) );
					if ( equipmentLevel == "EQUIPMENTLEVEL" ) {
						ZoneGas( Loop ).DesignLevel = IHGNumbers( 1 );
						if ( lNumericFieldBlanks( 1 ) ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + ZoneGas( Loop ).Name + "\", specifies " + cNumericFieldNames( 1 ) + ", but that field is blank.  0 Gas Equipment will result." );
						}

					} else if ( equipmentLevel == "WATTS/AREA" || equipmentLevel == "POWER/AREA" ) {
						if ( ZoneGas( Loop ).ZonePtr != 0 ) {
							if ( IHGNumbers( 2 ) >= 0.0 ) {
								ZoneGas( Loop ).DesignLevel = IHGNumbers( 2 ) * Zone( ZoneGas( Loop ).ZonePtr ).FloorArea;
								if ( Zone( ZoneGas( Loop ).ZonePtr ).FloorArea <= 0.0 ) {
									ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + ZoneGas( Loop ).Name + "\", specifies " + cNumericFieldNames( 2 ) + ", but Zone Floor Area = 0.  0 Gas Equipment will result." );
								}
							} else {
								ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + ZoneGas( Loop ).Name + "\", invalid " + cNumericFieldNames( 2 ) + ", value  [<0.0]=" + RoundSigDigits( IHGNumbers( 2 ), 3 ) );
								ErrorsFound = true;
							}
						}
						if ( lNumericFieldBlanks( 2 ) ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + ZoneGas( Loop ).Name + "\", specifies " + cNumericFieldNames( 2 ) + ", but that field is blank.  0 Gas Equipment will result." );
						}

					} else if ( equipmentLevel == "WATTS/PERSON" || equipmentLevel == "POWER/PERSON" ) {
						if ( ZoneGas( Loop ).ZonePtr != 0 ) {
							if ( IHGNumbers( 3 ) >= 0.0 ) {
								ZoneGas( Loop ).DesignLevel = IHGNumbers( 3 ) * Zone( ZoneGas( Loop ).ZonePtr ).TotOccupants;
								if ( Zone( ZoneGas( Loop ).ZonePtr ).TotOccupants <= 0.0 ) {
									ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + ZoneGas( Loop ).Name + "\", specifies " + cNumericFieldNames( 2 ) + ", but Total Occupants = 0.  0 Gas Equipment will result." );
								}
							} else {
								ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + ZoneGas( Loop ).Name + "\", invalid " + cNumericFieldNames( 3 ) + ", value  [<0.0]=" + RoundSigDigits( IHGNumbers( 3 ), 3 ) );
								ErrorsFound = true;
							}
						}
						if ( lNumericFieldBlanks( 3 ) ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + ZoneGas( Loop ).Name + "\", specifies " + cNumericFieldNames( 3 ) + ", but that field is blank.  0 Gas Equipment will result." );
						}

					} else {
						if ( Item1 == 1 ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", invalid " + cAlphaFieldNames( 4 ) + ", value  =" + AlphaName( 4 ) );
							ShowContinueError( "...Valid values are \"EquipmentLevel\", \"Watts/Area\", \"Watts/Person\"." );
							ErrorsFound = true;
						}
					}}

					// Calculate nominal min/max equipment level
					ZoneGas( Loop ).NomMinDesignLevel = ZoneGas( Loop ).DesignLevel * SchMin;
					ZoneGas( Loop ).NomMaxDesignLevel = ZoneGas( Loop ).DesignLevel * SchMax;

					ZoneGas( Loop ).FractionLatent = IHGNumbers( 4 );
					ZoneGas( Loop ).FractionRadiant = IHGNumbers( 5 );
					ZoneGas( Loop ).FractionLost = IHGNumbers( 6 );

					if ( ( NumNumber == 7 ) || ( ! lNumericFieldBlanks( 7 ) ) ) {
						ZoneGas( Loop ).CO2RateFactor = IHGNumbers( 7 );
					}
					if ( ZoneGas( Loop ).CO2RateFactor < 0.0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", " + cNumericFieldNames( 7 ) + " < 0.0, value =" + RoundSigDigits( IHGNumbers( 7 ), 2 ) );
						ErrorsFound = true;
					}
					if ( ZoneGas( Loop ).CO2RateFactor > 4.0e-7 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", " + cNumericFieldNames( 7 ) + " > 4.0E-7, value =" + RoundSigDigits( IHGNumbers( 7 ), 2 ) );
						ErrorsFound = true;
					}
					// FractionConvected is a calculated field
					ZoneGas( Loop ).FractionConvected = 1.0 - ( ZoneGas( Loop ).FractionLatent + ZoneGas( Loop ).FractionRadiant + ZoneGas( Loop ).FractionLost );
					if ( std::abs( ZoneGas( Loop ).FractionConvected ) <= 0.001 ) ZoneGas( Loop ).FractionConvected = 0.0;
					if ( ZoneGas( Loop ).FractionConvected < 0.0 ) {
						if ( Item1 == 1 ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", Sum of Fractions > 1.0" );
							ErrorsFound = true;
						}
					}

					if ( NumAlpha > 4 ) {
						ZoneGas( Loop ).EndUseSubcategory = AlphaName( 5 );
					} else {
						ZoneGas( Loop ).EndUseSubcategory = "General";
					}

					if ( ZoneGas( Loop ).ZonePtr <= 0 ) continue; // Error, will be caught and terminated later

					// Object report variables
					SetupOutputVariable( "Gas Equipment Gas Rate [W]", ZoneGas( Loop ).Power, "Zone", "Average", ZoneGas( Loop ).Name );
					SetupOutputVariable( "Gas Equipment Gas Energy [J]", ZoneGas( Loop ).Consumption, "Zone", "Sum", ZoneGas( Loop ).Name, _, "Gas", "InteriorEquipment", ZoneGas( Loop ).EndUseSubcategory, "Building", Zone( ZoneGas( Loop ).ZonePtr ).Name, Zone( ZoneGas( Loop ).ZonePtr ).Multiplier, Zone( ZoneGas( Loop ).ZonePtr ).ListMultiplier );

					SetupOutputVariable( "Gas Equipment Radiant Heating Energy [J]", ZoneGas( Loop ).RadGainEnergy, "Zone", "Sum", ZoneGas( Loop ).Name );
					SetupOutputVariable( "Gas Equipment Convective Heating Energy [J]", ZoneGas( Loop ).ConGainEnergy, "Zone", "Sum", ZoneGas( Loop ).Name );
					SetupOutputVariable( "Gas Equipment Latent Gain Energy [J]", ZoneGas( Loop ).LatGainEnergy, "Zone", "Sum", ZoneGas( Loop ).Name );
					SetupOutputVariable( "Gas Equipment Lost Heat Energy [J]", ZoneGas( Loop ).LostEnergy, "Zone", "Sum", ZoneGas( Loop ).Name );
					SetupOutputVariable( "Gas Equipment Total Heating Energy [J]", ZoneGas( Loop ).TotGainEnergy, "Zone", "Sum", ZoneGas( Loop ).Name );
					SetupOutputVariable( "Gas Equipment Radiant Heating Rate [W]", ZoneGas( Loop ).RadGainRate, "Zone", "Average", ZoneGas( Loop ).Name );
					SetupOutputVariable( "Gas Equipment Convective Heating Rate [W]", ZoneGas( Loop ).ConGainRate, "Zone", "Average", ZoneGas( Loop ).Name );
					SetupOutputVariable( "Gas Equipment Latent Gain Rate [W]", ZoneGas( Loop ).LatGainRate, "Zone", "Average", ZoneGas( Loop ).Name );
					SetupOutputVariable( "Gas Equipment Lost Heat Rate [W]", ZoneGas( Loop ).LostRate, "Zone", "Average", ZoneGas( Loop ).Name );
					SetupOutputVariable( "Gas Equipment Total Heating Rate [W]", ZoneGas( Loop ).TotGainRate, "Zone", "Average", ZoneGas( Loop ).Name );

					// Zone total report variables
					if ( RepVarSet( ZoneGas( Loop ).ZonePtr ) ) {
						RepVarSet( ZoneGas( Loop ).ZonePtr ) = false;

						SetupOutputVariable( "Zone Gas Equipment Gas Rate [W]", ZnRpt( ZoneGas( Loop ).ZonePtr ).GasPower, "Zone", "Average", Zone( ZoneGas( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Gas Equipment Gas Energy [J]", ZnRpt( ZoneGas( Loop ).ZonePtr ).GasConsump, "Zone", "Sum", Zone( ZoneGas( Loop ).ZonePtr ).Name );

						SetupOutputVariable( "Zone Gas Equipment Radiant Heating Energy [J]", ZnRpt( ZoneGas( Loop ).ZonePtr ).GasRadGain, "Zone", "Sum", Zone( ZoneGas( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Gas Equipment Radiant Heating Rate [W]", ZnRpt( ZoneGas( Loop ).ZonePtr ).GasRadGainRate, "Zone", "Average", Zone( ZoneGas( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Gas Equipment Convective Heating Energy [J]", ZnRpt( ZoneGas( Loop ).ZonePtr ).GasConGain, "Zone", "Sum", Zone( ZoneGas( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Gas Equipment Convective Heating Rate [W]", ZnRpt( ZoneGas( Loop ).ZonePtr ).GasConGainRate, "Zone", "Average", Zone( ZoneGas( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Gas Equipment Latent Gain Energy [J]", ZnRpt( ZoneGas( Loop ).ZonePtr ).GasLatGain, "Zone", "Sum", Zone( ZoneGas( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Gas Equipment Latent Gain Rate [W]", ZnRpt( ZoneGas( Loop ).ZonePtr ).GasLatGainRate, "Zone", "Average", Zone( ZoneGas( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Gas Equipment Lost Heat Energy [J]", ZnRpt( ZoneGas( Loop ).ZonePtr ).GasLost, "Zone", "Sum", Zone( ZoneGas( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Gas Equipment Lost Heat Rate [W]", ZnRpt( ZoneGas( Loop ).ZonePtr ).GasLostRate, "Zone", "Average", Zone( ZoneGas( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Gas Equipment Total Heating Energy [J]", ZnRpt( ZoneGas( Loop ).ZonePtr ).GasTotGain, "Zone", "Sum", Zone( ZoneGas( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Gas Equipment Total Heating Rate [W]", ZnRpt( ZoneGas( Loop ).ZonePtr ).GasTotGainRate, "Zone", "Average", Zone( ZoneGas( Loop ).ZonePtr ).Name );
					}

					if ( AnyEnergyManagementSystemInModel ) {
						SetupEMSActuator( "GasEquipment", ZoneGas( Loop ).Name, "Gas Power Level", "[W]", ZoneGas( Loop ).EMSZoneEquipOverrideOn, ZoneGas( Loop ).EMSEquipPower );
						SetupEMSInternalVariable( "Gas Process Power Design Level", ZoneGas( Loop ).Name, "[W]", ZoneGas( Loop ).DesignLevel );
					} // EMS

					if ( ! ErrorsFound ) SetupZoneInternalGain( ZoneGas( Loop ).ZonePtr, "GasEquipment", ZoneGas( Loop ).Name, IntGainTypeOf_GasEquipment, ZoneGas( Loop ).ConGainRate, _, ZoneGas( Loop ).RadGainRate, ZoneGas( Loop ).LatGainRate, _, ZoneGas( Loop ).CO2GainRate );

				} // Item1
			} // Item - number of gas statements
		} // check for number of gas statements

		RepVarSet = true;
		CurrentModuleObject = "HotWaterEquipment";
		NumHotWaterEqStatements = GetNumObjectsFound( CurrentModuleObject );
		HotWaterEqObjects.allocate( NumHotWaterEqStatements );

		TotHWEquip = 0;
		errFlag = false;
		for ( Item = 1; Item <= NumHotWaterEqStatements; ++Item ) {
			GetObjectItem( CurrentModuleObject, Item, AlphaName, NumAlpha, IHGNumbers, NumNumber, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( AlphaName( 1 ), HotWaterEqObjects, Item - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				errFlag = true;
				if ( IsBlank ) AlphaName( 1 ) = "xxxxx";
			}
			HotWaterEqObjects( Item ).Name = AlphaName( 1 );

			Item1 = FindItemInList( AlphaName( 2 ), Zone );
			ZLItem = 0;
			if ( Item1 == 0 && NumOfZoneLists > 0 ) ZLItem = FindItemInList( AlphaName( 2 ), ZoneList );
			if ( Item1 > 0 ) {
				HotWaterEqObjects( Item ).StartPtr = TotHWEquip + 1;
				++TotHWEquip;
				HotWaterEqObjects( Item ).NumOfZones = 1;
				HotWaterEqObjects( Item ).ZoneListActive = false;
				HotWaterEqObjects( Item ).ZoneOrZoneListPtr = Item1;
			} else if ( ZLItem > 0 ) {
				HotWaterEqObjects( Item ).StartPtr = TotHWEquip + 1;
				TotHWEquip += ZoneList( ZLItem ).NumOfZones;
				HotWaterEqObjects( Item ).NumOfZones = ZoneList( ZLItem ).NumOfZones;
				HotWaterEqObjects( Item ).ZoneListActive = true;
				HotWaterEqObjects( Item ).ZoneOrZoneListPtr = ZLItem;
			} else {
				ShowSevereError( CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\" invalid " + cAlphaFieldNames( 2 ) + "=\"" + AlphaName( 2 ) + "\" not found." );
				ErrorsFound = true;
				errFlag = true;
			}
		}

		if ( errFlag ) {
			ShowSevereError( RoutineName + "Errors with invalid names in " + CurrentModuleObject + " objects." );
			ShowContinueError( "...These will not be read in.  Other errors may occur." );
			TotHWEquip = 0;
		}

		ZoneHWEq.allocate( TotHWEquip );

		if ( TotHWEquip > 0 ) {
			Loop = 0;
			for ( Item = 1; Item <= NumHotWaterEqStatements; ++Item ) {
				AlphaName = BlankString;
				IHGNumbers = 0.0;

				GetObjectItem( CurrentModuleObject, Item, AlphaName, NumAlpha, IHGNumbers, NumNumber, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

				for ( Item1 = 1; Item1 <= HotWaterEqObjects( Item ).NumOfZones; ++Item1 ) {
					++Loop;
					if ( ! HotWaterEqObjects( Item ).ZoneListActive ) {
						ZoneHWEq( Loop ).Name = AlphaName( 1 );
						ZoneHWEq( Loop ).ZonePtr = HotWaterEqObjects( Item ).ZoneOrZoneListPtr;
					} else {
						CheckCreatedZoneItemName( RoutineName, CurrentModuleObject, Zone( ZoneList( HotWaterEqObjects( Item ).ZoneOrZoneListPtr ).Zone( Item1 ) ).Name, ZoneList( HotWaterEqObjects( Item ).ZoneOrZoneListPtr ).MaxZoneNameLength, HotWaterEqObjects( Item ).Name, ZoneHWEq, Loop - 1, ZoneHWEq( Loop ).Name, errFlag );
						ZoneHWEq( Loop ).ZonePtr = ZoneList( HotWaterEqObjects( Item ).ZoneOrZoneListPtr ).Zone( Item1 );
						if ( errFlag ) ErrorsFound = true;
					}

					ZoneHWEq( Loop ).SchedPtr = GetScheduleIndex( AlphaName( 3 ) );
					SchMin = 0.0;
					SchMax = 0.0;
					if ( ZoneHWEq( Loop ).SchedPtr == 0 ) {
						if ( lAlphaFieldBlanks( 3 ) ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", " + cAlphaFieldNames( 3 ) + " is required." );
						} else {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", invalid " + cAlphaFieldNames( 3 ) + " entered=" + AlphaName( 3 ) );
						}
						ErrorsFound = true;
					} else { // check min/max on schedule
						SchMin = GetScheduleMinValue( ZoneHWEq( Loop ).SchedPtr );
						SchMax = GetScheduleMaxValue( ZoneHWEq( Loop ).SchedPtr );
						if ( SchMin < 0.0 || SchMax < 0.0 ) {
							if ( SchMin < 0.0 ) {
								ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", " + cAlphaFieldNames( 3 ) + ", minimum is < 0.0" );
								ShowContinueError( "Schedule=\"" + AlphaName( 3 ) + "\". Minimum is [" + RoundSigDigits( SchMin, 1 ) + "]. Values must be >= 0.0." );
								ErrorsFound = true;
							}
							if ( SchMax < 0.0 ) {
								ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", " + cAlphaFieldNames( 3 ) + ", maximum is < 0.0" );
								ShowContinueError( "Schedule=\"" + AlphaName( 3 ) + "\". Maximum is [" + RoundSigDigits( SchMax, 1 ) + "]. Values must be >= 0.0." );
								ErrorsFound = true;
							}
						}
					}

					// Hot Water equipment design level calculation method.
					{ auto const equipmentLevel( AlphaName( 4 ) );
					if ( equipmentLevel == "EQUIPMENTLEVEL" ) {
						ZoneHWEq( Loop ).DesignLevel = IHGNumbers( 1 );
						if ( lNumericFieldBlanks( 1 ) ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", specifies " + cNumericFieldNames( 1 ) + ", but that field is blank.  0 Hot Water Equipment will result." );
						}

					} else if ( equipmentLevel == "WATTS/AREA" || equipmentLevel == "POWER/AREA" ) {
						if ( ZoneHWEq( Loop ).ZonePtr != 0 ) {
							if ( IHGNumbers( 2 ) >= 0.0 ) {
								ZoneHWEq( Loop ).DesignLevel = IHGNumbers( 2 ) * Zone( ZoneHWEq( Loop ).ZonePtr ).FloorArea;
								if ( Zone( ZoneHWEq( Loop ).ZonePtr ).FloorArea <= 0.0 ) {
									ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", specifies " + cNumericFieldNames( 2 ) + ", but Zone Floor Area = 0.  0 Hot Water Equipment will result." );
								}
							} else {
								ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", invalid " + cNumericFieldNames( 2 ) + ", value  [<0.0]=" + RoundSigDigits( IHGNumbers( 2 ), 3 ) );
								ErrorsFound = true;
							}
						}
						if ( lNumericFieldBlanks( 2 ) ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", specifies " + cNumericFieldNames( 2 ) + ", but that field is blank.  0 Hot Water Equipment will result." );
						}

					} else if ( equipmentLevel == "WATTS/PERSON" || equipmentLevel == "POWER/PERSON" ) {
						if ( ZoneHWEq( Loop ).ZonePtr != 0 ) {
							if ( IHGNumbers( 3 ) >= 0.0 ) {
								ZoneHWEq( Loop ).DesignLevel = IHGNumbers( 3 ) * Zone( ZoneHWEq( Loop ).ZonePtr ).TotOccupants;
								if ( Zone( ZoneHWEq( Loop ).ZonePtr ).TotOccupants <= 0.0 ) {
									ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", specifies " + cNumericFieldNames( 2 ) + ", but Total Occupants = 0.  0 Hot Water Equipment will result." );
								}
							} else {
								ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", invalid " + cNumericFieldNames( 3 ) + ", value  [<0.0]=" + RoundSigDigits( IHGNumbers( 3 ), 3 ) );
								ErrorsFound = true;
							}
						}
						if ( lNumericFieldBlanks( 3 ) ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", specifies " + cNumericFieldNames( 3 ) + ", but that field is blank.  0 Hot Water Equipment will result." );
						}

					} else {
						if ( Item1 == 1 ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", invalid " + cAlphaFieldNames( 4 ) + ", value  =" + AlphaName( 4 ) );
							ShowContinueError( "...Valid values are \"EquipmentLevel\", \"Watts/Area\", \"Watts/Person\"." );
							ErrorsFound = true;
						}
					}}

					// Calculate nominal min/max equipment level
					ZoneHWEq( Loop ).NomMinDesignLevel = ZoneHWEq( Loop ).DesignLevel * SchMin;
					ZoneHWEq( Loop ).NomMaxDesignLevel = ZoneHWEq( Loop ).DesignLevel * SchMax;

					ZoneHWEq( Loop ).FractionLatent = IHGNumbers( 4 );
					ZoneHWEq( Loop ).FractionRadiant = IHGNumbers( 5 );
					ZoneHWEq( Loop ).FractionLost = IHGNumbers( 6 );
					// FractionConvected is a calculated field
					ZoneHWEq( Loop ).FractionConvected = 1.0 - ( ZoneHWEq( Loop ).FractionLatent + ZoneHWEq( Loop ).FractionRadiant + ZoneHWEq( Loop ).FractionLost );
					if ( std::abs( ZoneHWEq( Loop ).FractionConvected ) <= 0.001 ) ZoneHWEq( Loop ).FractionConvected = 0.0;
					if ( ZoneHWEq( Loop ).FractionConvected < 0.0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", Sum of Fractions > 1.0" );
						ErrorsFound = true;
					}

					if ( NumAlpha > 4 ) {
						ZoneHWEq( Loop ).EndUseSubcategory = AlphaName( 5 );
					} else {
						ZoneHWEq( Loop ).EndUseSubcategory = "General";
					}

					if ( ZoneHWEq( Loop ).ZonePtr <= 0 ) continue; // Error, will be caught and terminated later

					// Object report variables
					SetupOutputVariable( "Hot Water Equipment District Heating Rate [W]", ZoneHWEq( Loop ).Power, "Zone", "Average", ZoneHWEq( Loop ).Name );
					SetupOutputVariable( "Hot Water Equipment District Heating Energy [J]", ZoneHWEq( Loop ).Consumption, "Zone", "Sum", ZoneHWEq( Loop ).Name, _, "DistrictHeating", "InteriorEquipment", ZoneHWEq( Loop ).EndUseSubcategory, "Building", Zone( ZoneHWEq( Loop ).ZonePtr ).Name, Zone( ZoneHWEq( Loop ).ZonePtr ).Multiplier, Zone( ZoneHWEq( Loop ).ZonePtr ).ListMultiplier );

					SetupOutputVariable( "Hot Water Equipment Radiant Heating Energy [J]", ZoneHWEq( Loop ).RadGainEnergy, "Zone", "Sum", ZoneHWEq( Loop ).Name );
					SetupOutputVariable( "Hot Water Equipment Radiant Heating Rate [W]", ZoneHWEq( Loop ).RadGainRate, "Zone", "Average", ZoneHWEq( Loop ).Name );
					SetupOutputVariable( "Hot Water Equipment Convective Heating Energy [J]", ZoneHWEq( Loop ).ConGainEnergy, "Zone", "Sum", ZoneHWEq( Loop ).Name );
					SetupOutputVariable( "Hot Water Equipment Convective Heating Rate [W]", ZoneHWEq( Loop ).ConGainRate, "Zone", "Average", ZoneHWEq( Loop ).Name );
					SetupOutputVariable( "Hot Water Equipment Latent Gain Energy [J]", ZoneHWEq( Loop ).LatGainEnergy, "Zone", "Sum", ZoneHWEq( Loop ).Name );
					SetupOutputVariable( "Hot Water Equipment Latent Gain Rate [W]", ZoneHWEq( Loop ).LatGainRate, "Zone", "Average", ZoneHWEq( Loop ).Name );
					SetupOutputVariable( "Hot Water Equipment Lost Heat Energy [J]", ZoneHWEq( Loop ).LostEnergy, "Zone", "Sum", ZoneHWEq( Loop ).Name );
					SetupOutputVariable( "Hot Water Equipment Lost Heat Rate [W]", ZoneHWEq( Loop ).LostRate, "Zone", "Average", ZoneHWEq( Loop ).Name );
					SetupOutputVariable( "Hot Water Equipment Total Heating Energy [J]", ZoneHWEq( Loop ).TotGainEnergy, "Zone", "Sum", ZoneHWEq( Loop ).Name );
					SetupOutputVariable( "Hot Water Equipment Total Heating Rate [W]", ZoneHWEq( Loop ).TotGainRate, "Zone", "Average", ZoneHWEq( Loop ).Name );

					// Zone total report variables
					if ( RepVarSet( ZoneHWEq( Loop ).ZonePtr ) ) {
						RepVarSet( ZoneHWEq( Loop ).ZonePtr ) = false;
						SetupOutputVariable( "Zone Hot Water Equipment District Heating Rate [W]", ZnRpt( ZoneHWEq( Loop ).ZonePtr ).HWPower, "Zone", "Average", Zone( ZoneHWEq( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Hot Water Equipment District Heating Energy [J]", ZnRpt( ZoneHWEq( Loop ).ZonePtr ).HWConsump, "Zone", "Sum", Zone( ZoneHWEq( Loop ).ZonePtr ).Name );

						SetupOutputVariable( "Zone Hot Water Equipment Radiant Heating Energy [J]", ZnRpt( ZoneHWEq( Loop ).ZonePtr ).HWRadGain, "Zone", "Sum", Zone( ZoneHWEq( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Hot Water Equipment Radiant Heating Rate [W]", ZnRpt( ZoneHWEq( Loop ).ZonePtr ).HWRadGainRate, "Zone", "Average", Zone( ZoneHWEq( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Hot Water Equipment Convective Heating Energy [J]", ZnRpt( ZoneHWEq( Loop ).ZonePtr ).HWConGain, "Zone", "Sum", Zone( ZoneHWEq( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Hot Water Equipment Convective Heating Rate [W]", ZnRpt( ZoneHWEq( Loop ).ZonePtr ).HWConGainRate, "Zone", "Average", Zone( ZoneHWEq( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Hot Water Equipment Latent Gain Energy [J]", ZnRpt( ZoneHWEq( Loop ).ZonePtr ).HWLatGain, "Zone", "Sum", Zone( ZoneHWEq( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Hot Water Equipment Latent Gain Rate [W]", ZnRpt( ZoneHWEq( Loop ).ZonePtr ).HWLatGainRate, "Zone", "Average", Zone( ZoneHWEq( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Hot Water Equipment Lost Heat Energy [J]", ZnRpt( ZoneHWEq( Loop ).ZonePtr ).HWLost, "Zone", "Sum", Zone( ZoneHWEq( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Hot Water Equipment Lost Heat Rate [W]", ZnRpt( ZoneHWEq( Loop ).ZonePtr ).HWLostRate, "Zone", "Average", Zone( ZoneHWEq( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Hot Water Equipment Total Heating Energy [J]", ZnRpt( ZoneHWEq( Loop ).ZonePtr ).HWTotGain, "Zone", "Sum", Zone( ZoneHWEq( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Hot Water Equipment Total Heating Rate [W]", ZnRpt( ZoneHWEq( Loop ).ZonePtr ).HWTotGainRate, "Zone", "Average", Zone( ZoneHWEq( Loop ).ZonePtr ).Name );
					}

					if ( AnyEnergyManagementSystemInModel ) {
						SetupEMSActuator( "HotWaterEquipment", ZoneHWEq( Loop ).Name, "District Heating Power Level", "[W]", ZoneHWEq( Loop ).EMSZoneEquipOverrideOn, ZoneHWEq( Loop ).EMSEquipPower );
						SetupEMSInternalVariable( "Process District Heat Design Level", ZoneHWEq( Loop ).Name, "[W]", ZoneHWEq( Loop ).DesignLevel );
					} // EMS

					if ( ! ErrorsFound ) SetupZoneInternalGain( ZoneHWEq( Loop ).ZonePtr, "HotWaterEquipment", ZoneHWEq( Loop ).Name, IntGainTypeOf_HotWaterEquipment, ZoneHWEq( Loop ).ConGainRate, _, ZoneHWEq( Loop ).RadGainRate, ZoneHWEq( Loop ).LatGainRate );

				} // Item1
			} // Item - number of hot water statements
		}

		RepVarSet = true;
		CurrentModuleObject = "SteamEquipment";
		NumSteamEqStatements = GetNumObjectsFound( CurrentModuleObject );
		SteamEqObjects.allocate( NumSteamEqStatements );

		TotStmEquip = 0;
		errFlag = false;
		for ( Item = 1; Item <= NumSteamEqStatements; ++Item ) {
			GetObjectItem( CurrentModuleObject, Item, AlphaName, NumAlpha, IHGNumbers, NumNumber, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( AlphaName( 1 ), SteamEqObjects, Item - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				errFlag = true;
				if ( IsBlank ) AlphaName( 1 ) = "xxxxx";
			}
			SteamEqObjects( Item ).Name = AlphaName( 1 );

			Item1 = FindItemInList( AlphaName( 2 ), Zone );
			ZLItem = 0;
			if ( Item1 == 0 && NumOfZoneLists > 0 ) ZLItem = FindItemInList( AlphaName( 2 ), ZoneList );
			if ( Item1 > 0 ) {
				SteamEqObjects( Item ).StartPtr = TotStmEquip + 1;
				++TotStmEquip;
				SteamEqObjects( Item ).NumOfZones = 1;
				SteamEqObjects( Item ).ZoneListActive = false;
				SteamEqObjects( Item ).ZoneOrZoneListPtr = Item1;
			} else if ( ZLItem > 0 ) {
				SteamEqObjects( Item ).StartPtr = TotStmEquip + 1;
				TotStmEquip += ZoneList( ZLItem ).NumOfZones;
				SteamEqObjects( Item ).NumOfZones = ZoneList( ZLItem ).NumOfZones;
				SteamEqObjects( Item ).ZoneListActive = true;
				SteamEqObjects( Item ).ZoneOrZoneListPtr = ZLItem;
			} else {
				ShowSevereError( CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\" invalid " + cAlphaFieldNames( 2 ) + "=\"" + AlphaName( 2 ) + "\" not found." );
				ErrorsFound = true;
				errFlag = true;
			}
		}

		if ( errFlag ) {
			ShowSevereError( RoutineName + "Errors with invalid names in " + CurrentModuleObject + " objects." );
			ShowContinueError( "...These will not be read in.  Other errors may occur." );
			TotStmEquip = 0;
		}

		ZoneSteamEq.allocate( TotStmEquip );

		if ( TotStmEquip > 0 ) {
			Loop = 0;
			for ( Item = 1; Item <= NumSteamEqStatements; ++Item ) {
				AlphaName = BlankString;
				IHGNumbers = 0.0;

				GetObjectItem( CurrentModuleObject, Item, AlphaName, NumAlpha, IHGNumbers, NumNumber, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

				for ( Item1 = 1; Item1 <= SteamEqObjects( Item ).NumOfZones; ++Item1 ) {
					++Loop;
					if ( ! SteamEqObjects( Item ).ZoneListActive ) {
						ZoneSteamEq( Loop ).Name = AlphaName( 1 );
						ZoneSteamEq( Loop ).ZonePtr = SteamEqObjects( Item ).ZoneOrZoneListPtr;
					} else {
						CheckCreatedZoneItemName( RoutineName, CurrentModuleObject, Zone( ZoneList( SteamEqObjects( Item ).ZoneOrZoneListPtr ).Zone( Item1 ) ).Name, ZoneList( SteamEqObjects( Item ).ZoneOrZoneListPtr ).MaxZoneNameLength, SteamEqObjects( Item ).Name, ZoneSteamEq, Loop - 1, ZoneSteamEq( Loop ).Name, errFlag );
						ZoneSteamEq( Loop ).ZonePtr = ZoneList( SteamEqObjects( Item ).ZoneOrZoneListPtr ).Zone( Item1 );
						if ( errFlag ) ErrorsFound = true;
					}

					ZoneSteamEq( Loop ).SchedPtr = GetScheduleIndex( AlphaName( 3 ) );
					SchMin = 0.0;
					SchMax = 0.0;
					if ( ZoneSteamEq( Loop ).SchedPtr == 0 ) {
						if ( lAlphaFieldBlanks( 3 ) ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", " + cAlphaFieldNames( 3 ) + " is required." );
						} else {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", invalid " + cAlphaFieldNames( 3 ) + " entered=" + AlphaName( 3 ) );
						}
						ErrorsFound = true;
					} else { // check min/max on schedule
						SchMin = GetScheduleMinValue( ZoneSteamEq( Loop ).SchedPtr );
						SchMax = GetScheduleMaxValue( ZoneSteamEq( Loop ).SchedPtr );
						if ( SchMin < 0.0 || SchMax < 0.0 ) {
							if ( SchMin < 0.0 ) {
								ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", " + cAlphaFieldNames( 3 ) + ", minimum is < 0.0" );
								ShowContinueError( "Schedule=\"" + AlphaName( 3 ) + "\". Minimum is [" + RoundSigDigits( SchMin, 1 ) + "]. Values must be >= 0.0." );
								ErrorsFound = true;
							}
							if ( SchMax < 0.0 ) {
								ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", " + cAlphaFieldNames( 3 ) + ", maximum is < 0.0" );
								ShowContinueError( "Schedule=\"" + AlphaName( 3 ) + "\". Maximum is [" + RoundSigDigits( SchMax, 1 ) + "]. Values must be >= 0.0." );
								ErrorsFound = true;
							}
						}
					}

					// Hot Water equipment design level calculation method.
					{ auto const equipmentLevel( AlphaName( 4 ) );
					if ( equipmentLevel == "EQUIPMENTLEVEL" ) {
						ZoneSteamEq( Loop ).DesignLevel = IHGNumbers( 1 );
						if ( lNumericFieldBlanks( 1 ) ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", specifies " + cNumericFieldNames( 1 ) + ", but that field is blank.  0 Hot Water Equipment will result." );
						}

					} else if ( equipmentLevel == "WATTS/AREA" || equipmentLevel == "POWER/AREA" ) {
						if ( ZoneSteamEq( Loop ).ZonePtr != 0 ) {
							if ( IHGNumbers( 2 ) >= 0.0 ) {
								ZoneSteamEq( Loop ).DesignLevel = IHGNumbers( 2 ) * Zone( ZoneSteamEq( Loop ).ZonePtr ).FloorArea;
								if ( Zone( ZoneSteamEq( Loop ).ZonePtr ).FloorArea <= 0.0 ) {
									ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", specifies " + cNumericFieldNames( 2 ) + ", but Zone Floor Area = 0.  0 Hot Water Equipment will result." );
								}
							} else {
								ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", invalid " + cNumericFieldNames( 2 ) + ", value  [<0.0]=" + RoundSigDigits( IHGNumbers( 2 ), 3 ) );
								ErrorsFound = true;
							}
						}
						if ( lNumericFieldBlanks( 2 ) ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", specifies " + cNumericFieldNames( 2 ) + ", but that field is blank.  0 Hot Water Equipment will result." );
						}

					} else if ( equipmentLevel == "WATTS/PERSON" || equipmentLevel == "POWER/PERSON" ) {
						if ( ZoneSteamEq( Loop ).ZonePtr != 0 ) {
							if ( IHGNumbers( 3 ) >= 0.0 ) {
								ZoneSteamEq( Loop ).DesignLevel = IHGNumbers( 3 ) * Zone( ZoneSteamEq( Loop ).ZonePtr ).TotOccupants;
								if ( Zone( ZoneSteamEq( Loop ).ZonePtr ).TotOccupants <= 0.0 ) {
									ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", specifies " + cNumericFieldNames( 2 ) + ", but Total Occupants = 0.  0 Hot Water Equipment will result." );
								}
							} else {
								ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", invalid " + cNumericFieldNames( 3 ) + ", value  [<0.0]=" + RoundSigDigits( IHGNumbers( 3 ), 3 ) );
								ErrorsFound = true;
							}
						}
						if ( lNumericFieldBlanks( 3 ) ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", specifies " + cNumericFieldNames( 3 ) + ", but that field is blank.  0 Hot Water Equipment will result." );
						}

					} else {
						if ( Item1 == 1 ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", invalid " + cAlphaFieldNames( 4 ) + ", value  =" + AlphaName( 4 ) );
							ShowContinueError( "...Valid values are \"EquipmentLevel\", \"Watts/Area\", \"Watts/Person\"." );
							ErrorsFound = true;
						}
					}}

					// Calculate nominal min/max equipment level
					ZoneSteamEq( Loop ).NomMinDesignLevel = ZoneSteamEq( Loop ).DesignLevel * SchMin;
					ZoneSteamEq( Loop ).NomMaxDesignLevel = ZoneSteamEq( Loop ).DesignLevel * SchMax;

					ZoneSteamEq( Loop ).FractionLatent = IHGNumbers( 4 );
					ZoneSteamEq( Loop ).FractionRadiant = IHGNumbers( 5 );
					ZoneSteamEq( Loop ).FractionLost = IHGNumbers( 6 );
					// FractionConvected is a calculated field
					ZoneSteamEq( Loop ).FractionConvected = 1.0 - ( ZoneSteamEq( Loop ).FractionLatent + ZoneSteamEq( Loop ).FractionRadiant + ZoneSteamEq( Loop ).FractionLost );
					if ( std::abs( ZoneSteamEq( Loop ).FractionConvected ) <= 0.001 ) ZoneSteamEq( Loop ).FractionConvected = 0.0;
					if ( ZoneSteamEq( Loop ).FractionConvected < 0.0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", Sum of Fractions > 1.0" );
						ErrorsFound = true;
					}

					if ( NumAlpha > 4 ) {
						ZoneSteamEq( Loop ).EndUseSubcategory = AlphaName( 5 );
					} else {
						ZoneSteamEq( Loop ).EndUseSubcategory = "General";
					}

					if ( ZoneSteamEq( Loop ).ZonePtr <= 0 ) continue; // Error, will be caught and terminated later

					// Object report variables
					SetupOutputVariable( "Steam Equipment District Heating Rate [W]", ZoneSteamEq( Loop ).Power, "Zone", "Average", ZoneSteamEq( Loop ).Name );
					SetupOutputVariable( "Steam Equipment District Heating Energy [J]", ZoneSteamEq( Loop ).Consumption, "Zone", "Sum", ZoneSteamEq( Loop ).Name, _, "DistrictHeating", "InteriorEquipment", ZoneSteamEq( Loop ).EndUseSubcategory, "Building", Zone( ZoneSteamEq( Loop ).ZonePtr ).Name, Zone( ZoneSteamEq( Loop ).ZonePtr ).Multiplier, Zone( ZoneSteamEq( Loop ).ZonePtr ).ListMultiplier );

					SetupOutputVariable( "Steam Equipment Radiant Heating Energy [J]", ZoneSteamEq( Loop ).RadGainEnergy, "Zone", "Sum", ZoneSteamEq( Loop ).Name );
					SetupOutputVariable( "Steam Equipment Radiant Heating Rate [W]", ZoneSteamEq( Loop ).RadGainRate, "Zone", "Average", ZoneSteamEq( Loop ).Name );
					SetupOutputVariable( "Steam Equipment Convective Heating Energy [J]", ZoneSteamEq( Loop ).ConGainEnergy, "Zone", "Sum", ZoneSteamEq( Loop ).Name );
					SetupOutputVariable( "Steam Equipment Convective Heating Rate [W]", ZoneSteamEq( Loop ).ConGainRate, "Zone", "Average", ZoneSteamEq( Loop ).Name );
					SetupOutputVariable( "Steam Equipment Latent Gain Energy [J]", ZoneSteamEq( Loop ).LatGainEnergy, "Zone", "Sum", ZoneSteamEq( Loop ).Name );
					SetupOutputVariable( "Steam Equipment Latent Gain Rate [W]", ZoneSteamEq( Loop ).LatGainRate, "Zone", "Average", ZoneSteamEq( Loop ).Name );
					SetupOutputVariable( "Steam Equipment Lost Heat Energy [J]", ZoneSteamEq( Loop ).LostEnergy, "Zone", "Sum", ZoneSteamEq( Loop ).Name );
					SetupOutputVariable( "Steam Equipment Lost Heat Rate [W]", ZoneSteamEq( Loop ).LostRate, "Zone", "Average", ZoneSteamEq( Loop ).Name );
					SetupOutputVariable( "Steam Equipment Total Heating Energy [J]", ZoneSteamEq( Loop ).TotGainEnergy, "Zone", "Sum", ZoneSteamEq( Loop ).Name );
					SetupOutputVariable( "Steam Equipment Total Heating Rate [W]", ZoneSteamEq( Loop ).TotGainRate, "Zone", "Average", ZoneSteamEq( Loop ).Name );

					// Zone total report variables
					if ( RepVarSet( ZoneSteamEq( Loop ).ZonePtr ) ) {
						RepVarSet( ZoneSteamEq( Loop ).ZonePtr ) = false;
						SetupOutputVariable( "Zone Steam Equipment District Heating Rate [W]", ZnRpt( ZoneSteamEq( Loop ).ZonePtr ).SteamPower, "Zone", "Average", Zone( ZoneSteamEq( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Steam Equipment District Heating Energy [J]", ZnRpt( ZoneSteamEq( Loop ).ZonePtr ).SteamConsump, "Zone", "Sum", Zone( ZoneSteamEq( Loop ).ZonePtr ).Name );

						SetupOutputVariable( "Zone Steam Equipment Radiant Heating Energy [J]", ZnRpt( ZoneSteamEq( Loop ).ZonePtr ).SteamRadGain, "Zone", "Sum", Zone( ZoneSteamEq( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Steam Equipment Radiant Heating Rate [W]", ZnRpt( ZoneSteamEq( Loop ).ZonePtr ).SteamRadGainRate, "Zone", "Average", Zone( ZoneSteamEq( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Steam Equipment Convective Heating Energy [J]", ZnRpt( ZoneSteamEq( Loop ).ZonePtr ).SteamConGain, "Zone", "Sum", Zone( ZoneSteamEq( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Steam Equipment Convective Heating Rate [W]", ZnRpt( ZoneSteamEq( Loop ).ZonePtr ).SteamConGainRate, "Zone", "Average", Zone( ZoneSteamEq( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Steam Equipment Latent Gain Energy [J]", ZnRpt( ZoneSteamEq( Loop ).ZonePtr ).SteamLatGain, "Zone", "Sum", Zone( ZoneSteamEq( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Steam Equipment Latent Gain Rate [W]", ZnRpt( ZoneSteamEq( Loop ).ZonePtr ).SteamLatGainRate, "Zone", "Average", Zone( ZoneSteamEq( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Steam Equipment Lost Heat Energy [J]", ZnRpt( ZoneSteamEq( Loop ).ZonePtr ).SteamLost, "Zone", "Sum", Zone( ZoneSteamEq( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Steam Equipment Lost Heat Rate [W]", ZnRpt( ZoneSteamEq( Loop ).ZonePtr ).SteamLostRate, "Zone", "Average", Zone( ZoneSteamEq( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Steam Equipment Total Heating Energy [J]", ZnRpt( ZoneSteamEq( Loop ).ZonePtr ).SteamTotGain, "Zone", "Sum", Zone( ZoneSteamEq( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Steam Equipment Total Heating Rate [W]", ZnRpt( ZoneSteamEq( Loop ).ZonePtr ).SteamTotGainRate, "Zone", "Average", Zone( ZoneSteamEq( Loop ).ZonePtr ).Name );
					}

					if ( AnyEnergyManagementSystemInModel ) {
						SetupEMSActuator( "SteamEquipment", ZoneSteamEq( Loop ).Name, "District Heating Power Level", "[W]", ZoneSteamEq( Loop ).EMSZoneEquipOverrideOn, ZoneSteamEq( Loop ).EMSEquipPower );
						SetupEMSInternalVariable( "Process Steam District Heat Design Level", ZoneSteamEq( Loop ).Name, "[W]", ZoneSteamEq( Loop ).DesignLevel );
					} // EMS

					if ( ! ErrorsFound ) SetupZoneInternalGain( ZoneSteamEq( Loop ).ZonePtr, "SteamEquipment", ZoneSteamEq( Loop ).Name, IntGainTypeOf_SteamEquipment, ZoneSteamEq( Loop ).ConGainRate, _, ZoneSteamEq( Loop ).RadGainRate, ZoneSteamEq( Loop ).LatGainRate );

				} // Item1
			} // Item - number of hot water statements
		}

		RepVarSet = true;
		CurrentModuleObject = "OtherEquipment";
		NumOtherEqStatements = GetNumObjectsFound( CurrentModuleObject );
		OtherEqObjects.allocate( NumOtherEqStatements );

		TotOthEquip = 0;
		errFlag = false;
		for ( Item = 1; Item <= NumOtherEqStatements; ++Item ) {
			GetObjectItem( CurrentModuleObject, Item, AlphaName, NumAlpha, IHGNumbers, NumNumber, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( AlphaName( 1 ), OtherEqObjects, Item - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				errFlag = true;
				if ( IsBlank ) AlphaName( 1 ) = "xxxxx";
			}
			OtherEqObjects( Item ).Name = AlphaName( 1 );

			Item1 = FindItemInList( AlphaName( 2 ), Zone );
			ZLItem = 0;
			if ( Item1 == 0 && NumOfZoneLists > 0 ) ZLItem = FindItemInList( AlphaName( 2 ), ZoneList );
			if ( Item1 > 0 ) {
				OtherEqObjects( Item ).StartPtr = TotOthEquip + 1;
				++TotOthEquip;
				OtherEqObjects( Item ).NumOfZones = 1;
				OtherEqObjects( Item ).ZoneListActive = false;
				OtherEqObjects( Item ).ZoneOrZoneListPtr = Item1;
			} else if ( ZLItem > 0 ) {
				OtherEqObjects( Item ).StartPtr = TotOthEquip + 1;
				TotOthEquip += ZoneList( ZLItem ).NumOfZones;
				OtherEqObjects( Item ).NumOfZones = ZoneList( ZLItem ).NumOfZones;
				OtherEqObjects( Item ).ZoneListActive = true;
				OtherEqObjects( Item ).ZoneOrZoneListPtr = ZLItem;
			} else {
				ShowSevereError( CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\" invalid " + cAlphaFieldNames( 2 ) + "=\"" + AlphaName( 2 ) + "\" not found." );
				ErrorsFound = true;
				errFlag = true;
			}
		}

		if ( errFlag ) {
			ShowSevereError( RoutineName + "Errors with invalid names in " + CurrentModuleObject + " objects." );
			ShowContinueError( "...These will not be read in.  Other errors may occur." );
			TotOthEquip = 0;
		}

		ZoneOtherEq.allocate( TotOthEquip );

		if ( TotOthEquip > 0 ) {
			Loop = 0;
			for ( Item = 1; Item <= NumOtherEqStatements; ++Item ) {
				AlphaName = BlankString;
				IHGNumbers = 0.0;

				GetObjectItem( CurrentModuleObject, Item, AlphaName, NumAlpha, IHGNumbers, NumNumber, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

				for ( Item1 = 1; Item1 <= OtherEqObjects( Item ).NumOfZones; ++Item1 ) {
					++Loop;
					if ( ! OtherEqObjects( Item ).ZoneListActive ) {
						ZoneOtherEq( Loop ).Name = AlphaName( 1 );
						ZoneOtherEq( Loop ).ZonePtr = OtherEqObjects( Item ).ZoneOrZoneListPtr;
					} else {
						CheckCreatedZoneItemName( RoutineName, CurrentModuleObject, Zone( ZoneList( OtherEqObjects( Item ).ZoneOrZoneListPtr ).Zone( Item1 ) ).Name, ZoneList( OtherEqObjects( Item ).ZoneOrZoneListPtr ).MaxZoneNameLength, OtherEqObjects( Item ).Name, ZoneOtherEq, Loop - 1, ZoneOtherEq( Loop ).Name, errFlag );
						ZoneOtherEq( Loop ).ZonePtr = ZoneList( OtherEqObjects( Item ).ZoneOrZoneListPtr ).Zone( Item1 );
						if ( errFlag ) ErrorsFound = true;
					}

					ZoneOtherEq( Loop ).SchedPtr = GetScheduleIndex( AlphaName( 3 ) );
					SchMin = 0.0;
					SchMax = 0.0;
					if ( ZoneOtherEq( Loop ).SchedPtr == 0 ) {
						if ( lAlphaFieldBlanks( 3 ) ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", " + cAlphaFieldNames( 3 ) + " is required." );
						} else {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", invalid " + cAlphaFieldNames( 3 ) + " entered=" + AlphaName( 3 ) );
						}
						ErrorsFound = true;
					} else { // check min/max on schedule
						SchMin = GetScheduleMinValue( ZoneOtherEq( Loop ).SchedPtr );
						SchMax = GetScheduleMaxValue( ZoneOtherEq( Loop ).SchedPtr );
					}

					// Hot Water equipment design level calculation method.
					{ auto const equipmentLevel( AlphaName( 4 ) );
					if ( equipmentLevel == "EQUIPMENTLEVEL" ) {
						ZoneOtherEq( Loop ).DesignLevel = IHGNumbers( 1 );
						if ( lNumericFieldBlanks( 1 ) ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", specifies " + cNumericFieldNames( 1 ) + ", but that field is blank.  0 Hot Water Equipment will result." );
						}

					} else if ( equipmentLevel == "WATTS/AREA" || equipmentLevel == "POWER/AREA" ) {
						if ( ZoneOtherEq( Loop ).ZonePtr != 0 ) {
							ZoneOtherEq( Loop ).DesignLevel = IHGNumbers( 2 ) * Zone( ZoneOtherEq( Loop ).ZonePtr ).FloorArea;
							if ( Zone( ZoneOtherEq( Loop ).ZonePtr ).FloorArea <= 0.0 ) {
								ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", specifies " + cNumericFieldNames( 2 ) + ", but Zone Floor Area = 0.  0 Hot Water Equipment will result." );
							}
						}
						if ( lNumericFieldBlanks( 2 ) ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", specifies " + cNumericFieldNames( 2 ) + ", but that field is blank.  0 Hot Water Equipment will result." );
						}

					} else if ( equipmentLevel == "WATTS/PERSON" || equipmentLevel == "POWER/PERSON" ) {
						if ( ZoneOtherEq( Loop ).ZonePtr != 0 ) {
							ZoneOtherEq( Loop ).DesignLevel = IHGNumbers( 3 ) * Zone( ZoneOtherEq( Loop ).ZonePtr ).TotOccupants;
							if ( Zone( ZoneOtherEq( Loop ).ZonePtr ).TotOccupants <= 0.0 ) {
								ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", specifies " + cNumericFieldNames( 2 ) + ", but Total Occupants = 0.  0 Hot Water Equipment will result." );
							}
						}
						if ( lNumericFieldBlanks( 3 ) ) {
							ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", specifies " + cNumericFieldNames( 3 ) + ", but that field is blank.  0 Hot Water Equipment will result." );
						}

					} else {
						if ( Item1 == 1 ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", invalid " + cAlphaFieldNames( 4 ) + ", value  =" + AlphaName( 4 ) );
							ShowContinueError( "...Valid values are \"EquipmentLevel\", \"Watts/Area\", \"Watts/Person\"." );
							ErrorsFound = true;
						}
					}}

					// Calculate nominal min/max equipment level
					ZoneOtherEq( Loop ).NomMinDesignLevel = ZoneOtherEq( Loop ).DesignLevel * SchMin;
					ZoneOtherEq( Loop ).NomMaxDesignLevel = ZoneOtherEq( Loop ).DesignLevel * SchMax;

					ZoneOtherEq( Loop ).FractionLatent = IHGNumbers( 4 );
					ZoneOtherEq( Loop ).FractionRadiant = IHGNumbers( 5 );
					ZoneOtherEq( Loop ).FractionLost = IHGNumbers( 6 );
					// FractionConvected is a calculated field
					ZoneOtherEq( Loop ).FractionConvected = 1.0 - ( ZoneOtherEq( Loop ).FractionLatent + ZoneOtherEq( Loop ).FractionRadiant + ZoneOtherEq( Loop ).FractionLost );
					if ( std::abs( ZoneOtherEq( Loop ).FractionConvected ) <= 0.001 ) ZoneOtherEq( Loop ).FractionConvected = 0.0;
					if ( ZoneOtherEq( Loop ).FractionConvected < 0.0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", Sum of Fractions > 1.0" );
						ErrorsFound = true;
					}

					if ( NumAlpha > 4 ) {
						ZoneOtherEq( Loop ).EndUseSubcategory = AlphaName( 5 );
					} else {
						ZoneOtherEq( Loop ).EndUseSubcategory = "General";
					}

					if ( ZoneOtherEq( Loop ).ZonePtr <= 0 ) continue; // Error, will be caught and terminated later

					// Object report variables
					SetupOutputVariable( "Other Equipment Radiant Heating Energy [J]", ZoneOtherEq( Loop ).RadGainEnergy, "Zone", "Sum", ZoneOtherEq( Loop ).Name );
					SetupOutputVariable( "Other Equipment Radiant Heating Rate [W]", ZoneOtherEq( Loop ).RadGainRate, "Zone", "Average", ZoneOtherEq( Loop ).Name );
					SetupOutputVariable( "Other Equipment Convective Heating Energy [J]", ZoneOtherEq( Loop ).ConGainEnergy, "Zone", "Sum", ZoneOtherEq( Loop ).Name );
					SetupOutputVariable( "Other Equipment Convective Heating Rate [W]", ZoneOtherEq( Loop ).ConGainRate, "Zone", "Average", ZoneOtherEq( Loop ).Name );
					SetupOutputVariable( "Other Equipment Latent Gain Energy [J]", ZoneOtherEq( Loop ).LatGainEnergy, "Zone", "Sum", ZoneOtherEq( Loop ).Name );
					SetupOutputVariable( "Other Equipment Latent Gain Rate [W]", ZoneOtherEq( Loop ).LatGainRate, "Zone", "Average", ZoneOtherEq( Loop ).Name );
					SetupOutputVariable( "Other Equipment Lost Heat Energy [J]", ZoneOtherEq( Loop ).LostEnergy, "Zone", "Sum", ZoneOtherEq( Loop ).Name );
					SetupOutputVariable( "Other Equipment Lost Heat Rate [W]", ZoneOtherEq( Loop ).LostRate, "Zone", "Average", ZoneOtherEq( Loop ).Name );
					SetupOutputVariable( "Other Equipment Total Heating Energy [J]", ZoneOtherEq( Loop ).TotGainEnergy, "Zone", "Sum", ZoneOtherEq( Loop ).Name );
					SetupOutputVariable( "Other Equipment Total Heating Rate [W]", ZoneOtherEq( Loop ).TotGainRate, "Zone", "Average", ZoneOtherEq( Loop ).Name );

					// Zone total report variables
					if ( RepVarSet( ZoneOtherEq( Loop ).ZonePtr ) ) {
						RepVarSet( ZoneOtherEq( Loop ).ZonePtr ) = false;
						SetupOutputVariable( "Zone Other Equipment Radiant Heating Energy [J]", ZnRpt( ZoneOtherEq( Loop ).ZonePtr ).OtherRadGain, "Zone", "Sum", Zone( ZoneOtherEq( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Other Equipment Radiant Heating Rate [W]", ZnRpt( ZoneOtherEq( Loop ).ZonePtr ).OtherRadGainRate, "Zone", "Average", Zone( ZoneOtherEq( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Other Equipment Convective Heating Energy [J]", ZnRpt( ZoneOtherEq( Loop ).ZonePtr ).OtherConGain, "Zone", "Sum", Zone( ZoneOtherEq( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Other Equipment Convective Heating Rate [W]", ZnRpt( ZoneOtherEq( Loop ).ZonePtr ).OtherConGainRate, "Zone", "Average", Zone( ZoneOtherEq( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Other Equipment Latent Gain Energy [J]", ZnRpt( ZoneOtherEq( Loop ).ZonePtr ).OtherLatGain, "Zone", "Sum", Zone( ZoneOtherEq( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Other Equipment Latent Gain Rate [W]", ZnRpt( ZoneOtherEq( Loop ).ZonePtr ).OtherLatGainRate, "Zone", "Average", Zone( ZoneOtherEq( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Other Equipment Lost Heat Energy [J]", ZnRpt( ZoneOtherEq( Loop ).ZonePtr ).OtherLost, "Zone", "Sum", Zone( ZoneOtherEq( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Other Equipment Lost Heat Rate [W]", ZnRpt( ZoneOtherEq( Loop ).ZonePtr ).OtherLostRate, "Zone", "Average", Zone( ZoneOtherEq( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Other Equipment Total Heating Energy [J]", ZnRpt( ZoneOtherEq( Loop ).ZonePtr ).OtherTotGain, "Zone", "Sum", Zone( ZoneOtherEq( Loop ).ZonePtr ).Name );
						SetupOutputVariable( "Zone Other Equipment Total Heating Rate [W]", ZnRpt( ZoneOtherEq( Loop ).ZonePtr ).OtherTotGainRate, "Zone", "Average", Zone( ZoneOtherEq( Loop ).ZonePtr ).Name );
					}
					if ( AnyEnergyManagementSystemInModel ) {
						SetupEMSActuator( "OtherEquipment", ZoneOtherEq( Loop ).Name, "Power Level", "[W]", ZoneOtherEq( Loop ).EMSZoneEquipOverrideOn, ZoneOtherEq( Loop ).EMSEquipPower );
						SetupEMSInternalVariable( "Other Equipment Design Level", ZoneOtherEq( Loop ).Name, "[W]", ZoneOtherEq( Loop ).DesignLevel );
					} // EMS

					if ( ! ErrorsFound ) SetupZoneInternalGain( ZoneOtherEq( Loop ).ZonePtr, "OtherEquipment", ZoneOtherEq( Loop ).Name, IntGainTypeOf_OtherEquipment, ZoneOtherEq( Loop ).ConGainRate, _, ZoneOtherEq( Loop ).RadGainRate, ZoneOtherEq( Loop ).LatGainRate );

				} // Item1
			} // Item - number of hot water statements
		}

		RepVarSet = true;
		CurrentModuleObject = "ElectricEquipment:ITE:AirCooled";
		NumZoneITEqStatements = GetNumObjectsFound( CurrentModuleObject );
		errFlag = false;

		// Note that this object type does not support ZoneList due to node names in input fields
		ZoneITEq.allocate( NumZoneITEqStatements );

		if ( NumZoneITEqStatements > 0 ) {
			Loop = 0;
			for ( Loop = 1; Loop <= NumZoneITEqStatements; ++Loop ) {
				AlphaName = BlankString;
				IHGNumbers = 0.0;

				GetObjectItem( CurrentModuleObject, Loop, AlphaName, NumAlpha, IHGNumbers, NumNumber, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

				ZoneITEq( Loop ).Name = AlphaName( 1 );
				ZoneITEq( Loop ).ZonePtr = FindItemInList( AlphaName( 2 ), Zone );

				// IT equipment design level calculation method.
				{ auto const equipmentLevel( AlphaName( 3 ) );
				if ( equipmentLevel == "WATTS/UNIT" ) {
					ZoneITEq( Loop ).DesignTotalPower = IHGNumbers( 1 ) * IHGNumbers( 2 );
					if ( lNumericFieldBlanks( 1 ) ) {
						ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", specifies " + cNumericFieldNames( 1 ) + ", but that field is blank.  0 IT Equipment will result." );
					}
					if ( lNumericFieldBlanks( 2 ) ) {
						ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", specifies " + cNumericFieldNames( 2 ) + ", but that field is blank.  0 IT Equipment will result." );
					}

				} else if ( equipmentLevel == "WATTS/AREA" ) {
					if ( ZoneITEq( Loop ).ZonePtr != 0 ) {
						if ( IHGNumbers( 3 ) >= 0.0 ) {
							ZoneITEq( Loop ).DesignTotalPower = IHGNumbers( 3 ) * Zone( ZoneITEq( Loop ).ZonePtr ).FloorArea;
							if ( Zone( ZoneITEq( Loop ).ZonePtr ).FloorArea <= 0.0 ) {
								ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", specifies " + cNumericFieldNames( 3 ) + ", but Zone Floor Area = 0.  0 IT Equipment will result." );
							}
						} else {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", invalid " + cNumericFieldNames( 3 ) + ", value  [<0.0]=" + RoundSigDigits( IHGNumbers( 3 ), 3 ) );
							ErrorsFound = true;
						}
					}
					if ( lNumericFieldBlanks( 3 ) ) {
						ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", specifies " + cNumericFieldNames( 3 ) + ", but that field is blank.  0 IT Equipment will result." );
					}

				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", invalid " + cAlphaFieldNames( 3 ) + ", value  =" + AlphaName( 3 ) );
					ShowContinueError( "...Valid values are \"Watts/Unit\" or \"Watts/Area\"." );
					ErrorsFound = true;
				}}

				if ( lAlphaFieldBlanks( 4 ) ) {
					ZoneITEq( Loop ).OperSchedPtr = ScheduleAlwaysOn;
				} else {
					ZoneITEq( Loop ).OperSchedPtr = GetScheduleIndex( AlphaName( 4 ) );
				}
				SchMin = 0.0;
				SchMax = 0.0;
				if ( ZoneITEq( Loop ).OperSchedPtr == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", invalid " + cAlphaFieldNames( 4 ) + " entered=" + AlphaName( 4 ) );
					ErrorsFound = true;
				} else { // check min/max on schedule
					SchMin = GetScheduleMinValue( ZoneITEq( Loop ).OperSchedPtr );
					SchMax = GetScheduleMaxValue( ZoneITEq( Loop ).OperSchedPtr );
					if ( SchMin < 0.0 || SchMax < 0.0 ) {
						if ( SchMin < 0.0 ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", " + cAlphaFieldNames( 4 ) + ", minimum is < 0.0" );
							ShowContinueError( "Schedule=\"" + AlphaName( 4 ) + "\". Minimum is [" + RoundSigDigits( SchMin, 1 ) + "]. Values must be >= 0.0." );
							ErrorsFound = true;
						}
						if ( SchMax < 0.0 ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", " + cAlphaFieldNames( 4 ) + ", maximum is < 0.0" );
							ShowContinueError( "Schedule=\"" + AlphaName( 4 ) + "\". Maximum is [" + RoundSigDigits( SchMax, 1 ) + "]. Values must be >= 0.0." );
							ErrorsFound = true;
						}
					}
				}

				if ( lAlphaFieldBlanks( 5 ) ) {
					ZoneITEq( Loop ).CPULoadSchedPtr = ScheduleAlwaysOn;
				} else {
					ZoneITEq( Loop ).CPULoadSchedPtr = GetScheduleIndex( AlphaName( 5 ) );
				}
				SchMin = 0.0;
				SchMax = 0.0;
				if ( ZoneITEq( Loop ).CPULoadSchedPtr == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", invalid " + cAlphaFieldNames( 5 ) + " entered=" + AlphaName( 5 ) );
					ErrorsFound = true;
				} else { // check min/max on schedule
					SchMin = GetScheduleMinValue( ZoneITEq( Loop ).CPULoadSchedPtr );
					SchMax = GetScheduleMaxValue( ZoneITEq( Loop ).CPULoadSchedPtr );
					if ( SchMin < 0.0 || SchMax < 0.0 ) {
						if ( SchMin < 0.0 ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", " + cAlphaFieldNames( 5 ) + ", minimum is < 0.0" );
							ShowContinueError( "Schedule=\"" + AlphaName( 5 ) + "\". Minimum is [" + RoundSigDigits( SchMin, 1 ) + "]. Values must be >= 0.0." );
							ErrorsFound = true;
						}
						if ( SchMax < 0.0 ) {
							ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", " + cAlphaFieldNames( 5 ) + ", maximum is < 0.0" );
							ShowContinueError( "Schedule=\"" + AlphaName( 5 ) + "\". Maximum is [" + RoundSigDigits( SchMax, 1 ) + "]. Values must be >= 0.0." );
							ErrorsFound = true;
						}
					}
				}

				// Calculate nominal min/max equipment level
				ZoneITEq( Loop ).NomMinDesignLevel = ZoneITEq( Loop ).DesignTotalPower * SchMin;
				ZoneITEq( Loop ).NomMaxDesignLevel = ZoneITEq( Loop ).DesignTotalPower * SchMax;

				ZoneITEq( Loop ).DesignFanPowerFrac = IHGNumbers( 4 );
				ZoneITEq( Loop ).DesignFanPower = ZoneITEq( Loop ).DesignFanPowerFrac * ZoneITEq( Loop ).DesignTotalPower;
				ZoneITEq( Loop ).DesignCPUPower = ( 1.0 - ZoneITEq( Loop ).DesignFanPowerFrac ) * ZoneITEq( Loop ).DesignTotalPower;
				ZoneITEq( Loop ).DesignAirVolFlowRate = IHGNumbers( 5 ) * ZoneITEq( Loop ).DesignTotalPower;
				ZoneITEq( Loop ).DesignTAirIn = IHGNumbers( 6 );
				ZoneITEq( Loop ).DesignRecircFrac = IHGNumbers( 7 );
				ZoneITEq( Loop ).DesignUPSEfficiency = IHGNumbers( 8 );
				ZoneITEq( Loop ).UPSLossToZoneFrac = IHGNumbers( 9 );

				// Performance curves
				ZoneITEq( Loop ).CPUPowerFLTCurve = GetCurveIndex( AlphaName( 6 ) );
				if ( ZoneITEq( Loop ).CPUPowerFLTCurve == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + " \"" + AlphaName( 1 ) + "\"" );
					ShowContinueError( "Invalid " + cAlphaFieldNames( 6 ) + '=' + AlphaName( 6 ) );
					ErrorsFound = true;
				}

				ZoneITEq( Loop ).AirFlowFLTCurve = GetCurveIndex( AlphaName( 7 ) );
				if ( ZoneITEq( Loop ).AirFlowFLTCurve == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + " \"" + AlphaName( 1 ) + "\"" );
					ShowContinueError( "Invalid " + cAlphaFieldNames( 7 ) + '=' + AlphaName( 7 ) );
					ErrorsFound = true;
				}

				ZoneITEq( Loop ).FanPowerFFCurve = GetCurveIndex( AlphaName( 8 ) );
				if ( ZoneITEq( Loop ).FanPowerFFCurve == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + " \"" + AlphaName( 1 ) + "\"" );
					ShowContinueError( "Invalid " + cAlphaFieldNames( 8 ) + '=' + AlphaName( 8 ) );
					ErrorsFound = true;
				}

				ZoneITEq( Loop ).RecircFLTCurve = GetCurveIndex( AlphaName( 14 ) );
				if ( ZoneITEq( Loop ).RecircFLTCurve == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + " \"" + AlphaName( 1 ) + "\"" );
					ShowContinueError( "Invalid " + cAlphaFieldNames( 14 ) + '=' + AlphaName( 14 ) );
					ErrorsFound = true;
				}

				ZoneITEq( Loop ).UPSEfficFPLRCurve = GetCurveIndex( AlphaName( 15 ) );
				if ( ZoneITEq( Loop ).UPSEfficFPLRCurve == 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + " \"" + AlphaName( 1 ) + "\"" );
					ShowContinueError( "Invalid " + cAlphaFieldNames( 15 ) + '=' + AlphaName( 15 ) );
					ErrorsFound = true;
				}

				// Environmental class
				if ( SameString( AlphaName( 9 ), "None" ) ) {
					ZoneITEq( Loop ).Class = ITEClassNone;
				} else if ( SameString( AlphaName( 9 ), "A1" ) ) {
					ZoneITEq( Loop ).Class = ITEClassA1;
				} else if ( SameString( AlphaName( 9 ), "A2" ) ) {
					ZoneITEq( Loop ).Class = ITEClassA2;
				} else if ( SameString( AlphaName( 9 ), "A3" ) ) {
					ZoneITEq( Loop ).Class = ITEClassA3;
				} else if ( SameString( AlphaName( 9 ), "A4" ) ) {
					ZoneITEq( Loop ).Class = ITEClassA4;
				} else if ( SameString( AlphaName( 9 ), "B" ) ) {
					ZoneITEq( Loop ).Class = ITEClassB;
				} else if ( SameString( AlphaName( 9 ), "C" ) ) {
					ZoneITEq( Loop ).Class = ITEClassC;
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + ": " + AlphaName( 1 ) );
					ShowContinueError( "Invalid " + cAlphaFieldNames( 9 ) + '=' + AlphaName( 9 ) );
					ShowContinueError( "Valid entries are None, A1, A2, A3, A4, B or C." );
					ErrorsFound = true;
				}

				// Air and supply inlet connections
				if ( SameString( AlphaName( 10 ), "AdjustedSupply" ) ) {
					ZoneITEq( Loop ).AirConnectionType = ITEInletAdjustedSupply;
				} else if ( SameString( AlphaName( 10 ), "ZoneAirNode" ) ) {
					ZoneITEq( Loop ).AirConnectionType = ITEInletZoneAirNode;
				} else if ( SameString( AlphaName( 10 ), "RoomAirModel" ) ) {
					// ZoneITEq( Loop ).AirConnectionType = ITEInletRoomAirModel;
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "Air Inlet Connection Type = RoomAirModel is not implemented yet, using ZoneAirNode" );
					ZoneITEq( Loop ).AirConnectionType = ITEInletZoneAirNode;
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + ": " + AlphaName( 1 ) );
					ShowContinueError( "Invalid " + cAlphaFieldNames( 10 ) + '=' + AlphaName( 10 ) );
					ShowContinueError( "Valid entries are AdjustedSupply, ZoneAirNode, or RoomAirModel." );
					ErrorsFound = true;
				}
				if ( lAlphaFieldBlanks( 13 ) ) {
					if ( ZoneITEq( Loop ).AirConnectionType == ITEInletAdjustedSupply ) {
						ShowSevereError( RoutineName + CurrentModuleObject + ": " + AlphaName( 1 ) );
						ShowContinueError( "For " + cAlphaFieldNames( 10 ) + "= AdjustedSupply, " + cAlphaFieldNames( 13 ) + " is required, but this field is blank." );
						ErrorsFound = true;
					} else {
						ZoneITEq( Loop ).SupplyAirNodeNum = 0;
					}
				} else {
					ZoneITEq( Loop ).SupplyAirNodeNum = GetOnlySingleNode( AlphaName( 13 ), ErrorsFound, CurrentModuleObject, AlphaName( 1 ), NodeType_Air, NodeConnectionType_Sensor, 1, ObjectIsNotParent );
				}

				// End-Use subcategories
				if ( NumAlpha > 15 ) {
					ZoneITEq( Loop ).EndUseSubcategoryCPU = AlphaName( 16 );
				} else {
					ZoneITEq( Loop ).EndUseSubcategoryCPU = "ITE-CPU";
				}

				if ( NumAlpha > 16 ) {
					ZoneITEq( Loop ).EndUseSubcategoryFan = AlphaName( 17 );
				} else {
					ZoneITEq( Loop ).EndUseSubcategoryFan = "ITE-Fans";
				}
				if ( ZoneITEq( Loop ).ZonePtr <= 0 ) continue; // Error, will be caught and terminated later

				if ( NumAlpha > 17 ) {
					ZoneITEq( Loop ).EndUseSubcategoryUPS = AlphaName( 18 );
				} else {
					ZoneITEq( Loop ).EndUseSubcategoryUPS = "ITE-UPS";
				}

				// Object report variables
				SetupOutputVariable( "ITE CPU Electric Power [W]", ZoneITEq( Loop ).CPUPower, "Zone", "Average", ZoneITEq( Loop ).Name );
				SetupOutputVariable( "ITE Fan Electric Power [W]", ZoneITEq( Loop ).FanPower, "Zone", "Average", ZoneITEq( Loop ).Name );
				SetupOutputVariable( "ITE UPS Electric Power [W]", ZoneITEq( Loop ).UPSPower, "Zone", "Average", ZoneITEq( Loop ).Name );
				SetupOutputVariable( "ITE CPU Electric Power at Design Inlet Conditions [W]", ZoneITEq( Loop ).CPUPowerAtDesign, "Zone", "Average", ZoneITEq( Loop ).Name );
				SetupOutputVariable( "ITE Fan Electric Power at Design Inlet Conditions [W]", ZoneITEq( Loop ).FanPowerAtDesign, "Zone", "Average", ZoneITEq( Loop ).Name );
				SetupOutputVariable( "ITE UPS Heat Gain to Zone Rate [W]", ZoneITEq( Loop ).UPSGainRateToZone, "Zone", "Average", ZoneITEq( Loop ).Name );
				SetupOutputVariable( "ITE Total Heat Gain to Zone Rate [W]", ZoneITEq( Loop ).ConGainRateToZone, "Zone", "Average", ZoneITEq( Loop ).Name );

				SetupOutputVariable( "ITE CPU Electric Energy [J]", ZoneITEq( Loop ).CPUConsumption, "Zone", "Sum", ZoneITEq( Loop ).Name, _, "Electricity", "InteriorEquipment", ZoneITEq( Loop ).EndUseSubcategoryCPU, "Building", Zone( ZoneITEq( Loop ).ZonePtr ).Name, Zone( ZoneITEq( Loop ).ZonePtr ).Multiplier, Zone( ZoneITEq( Loop ).ZonePtr ).ListMultiplier );
				SetupOutputVariable( "ITE Fan Electric Energy [J]", ZoneITEq( Loop ).FanConsumption, "Zone", "Sum", ZoneITEq( Loop ).Name, _, "Electricity", "InteriorEquipment", ZoneITEq( Loop ).EndUseSubcategoryFan, "Building", Zone( ZoneITEq( Loop ).ZonePtr ).Name, Zone( ZoneITEq( Loop ).ZonePtr ).Multiplier, Zone( ZoneITEq( Loop ).ZonePtr ).ListMultiplier );
				SetupOutputVariable( "ITE UPS Electric Energy [J]", ZoneITEq( Loop ).UPSConsumption, "Zone", "Sum", ZoneITEq( Loop ).Name, _, "Electricity", "InteriorEquipment", ZoneITEq( Loop ).EndUseSubcategoryUPS, "Building", Zone( ZoneITEq( Loop ).ZonePtr ).Name, Zone( ZoneITEq( Loop ).ZonePtr ).Multiplier, Zone( ZoneITEq( Loop ).ZonePtr ).ListMultiplier );
				SetupOutputVariable( "ITE CPU Electric Energy at Design Inlet Conditions [J]", ZoneITEq( Loop ).CPUEnergyAtDesign, "Zone", "Sum", ZoneITEq( Loop ).Name );
				SetupOutputVariable( "ITE Fan Electric Energy at Design Inlet Conditions [J]", ZoneITEq( Loop ).FanEnergyAtDesign, "Zone", "Sum", ZoneITEq( Loop ).Name );
				SetupOutputVariable( "ITE UPS Heat Gain to Zone Energy [J]", ZoneITEq( Loop ).UPSGainEnergyToZone, "Zone", "Sum", ZoneITEq( Loop ).Name );
				SetupOutputVariable( "ITE Total Heat Gain to Zone Energy [J]", ZoneITEq( Loop ).ConGainEnergyToZone, "Zone", "Sum", ZoneITEq( Loop ).Name );

				SetupOutputVariable( "ITE Standard Density Air Volume Flow Rate [m3/s]", ZoneITEq( Loop ).AirVolFlowStdDensity, "Zone", "Average", ZoneITEq( Loop ).Name );
				SetupOutputVariable( "ITE Current Density Air Volume Flow Rate [m3/s]", ZoneITEq( Loop ).AirVolFlowCurDensity, "Zone", "Average", ZoneITEq( Loop ).Name );
				SetupOutputVariable( "ITE Air Mass Flow Rate [kg/s]", ZoneITEq( Loop ).AirMassFlow, "Zone", "Average", ZoneITEq( Loop ).Name );
				SetupOutputVariable( "ITE Air Inlet Dry-Bulb Temperature [C]", ZoneITEq( Loop ).AirInletDryBulbT, "Zone", "Average", ZoneITEq( Loop ).Name );
				SetupOutputVariable( "ITE Air Inlet Dewpoint Temperature [C]", ZoneITEq( Loop ).AirInletDewpointT, "Zone", "Average", ZoneITEq( Loop ).Name );
				SetupOutputVariable( "ITE Air Inlet Relative Humidity [%]", ZoneITEq( Loop ).AirInletRelHum, "Zone", "Average", ZoneITEq( Loop ).Name );
				SetupOutputVariable( "ITE Air Outlet Dry-Bulb Temperature [C]", ZoneITEq( Loop ).AirOutletDryBulbT, "Zone", "Average", ZoneITEq( Loop ).Name );
				if ( ZoneITEq( Loop ).SupplyAirNodeNum != 0 ) SetupOutputVariable( "ITE Supply Heat Index []", ZoneITEq( Loop ).SHI, "Zone", "Average", ZoneITEq( Loop ).Name );
				SetupOutputVariable( "ITE Air Inlet Operating Range Exceeded Time [hr]", ZoneITEq( Loop ).TimeOutOfOperRange, "Zone", "Sum", ZoneITEq( Loop ).Name );
				SetupOutputVariable( "ITE Air Inlet Dry-Bulb Temperature Above Operating Range Time [hr]", ZoneITEq( Loop ).TimeAboveDryBulbT, "Zone", "Sum", ZoneITEq( Loop ).Name );
				SetupOutputVariable( "ITE Air Inlet Dry-Bulb Temperature Below Operating Range Time [hr]", ZoneITEq( Loop ).TimeBelowDryBulbT, "Zone", "Sum", ZoneITEq( Loop ).Name );
				SetupOutputVariable( "ITE Air Inlet Dewpoint Temperature Above Operating Range Time [hr]", ZoneITEq( Loop ).TimeAboveDewpointT, "Zone", "Sum", ZoneITEq( Loop ).Name );
				SetupOutputVariable( "ITE Air Inlet Dewpoint Temperature Below Operating Range Time [hr]", ZoneITEq( Loop ).TimeBelowDewpointT, "Zone", "Sum", ZoneITEq( Loop ).Name );
				SetupOutputVariable( "ITE Air Inlet Relative Humidity Above Operating Range Time [hr]", ZoneITEq( Loop ).TimeAboveRH, "Zone", "Sum", ZoneITEq( Loop ).Name );
				SetupOutputVariable( "ITE Air Inlet Relative Humidity Below Operating Range Time [hr]", ZoneITEq( Loop ).TimeBelowRH, "Zone", "Sum", ZoneITEq( Loop ).Name );
				SetupOutputVariable( "ITE Air Inlet Dry-Bulb Temperature Difference Above Operating Range [deltaC]", ZoneITEq( Loop ).DryBulbTAboveDeltaT, "Zone", "Average", ZoneITEq( Loop ).Name );
				SetupOutputVariable( "ITE Air Inlet Dry-Bulb Temperature Difference Below Operating Range [deltaC]", ZoneITEq( Loop ).DryBulbTBelowDeltaT, "Zone", "Average", ZoneITEq( Loop ).Name );
				SetupOutputVariable( "ITE Air Inlet Dewpoint Temperature Difference Above Operating Range [deltaC]", ZoneITEq( Loop ).DewpointTAboveDeltaT, "Zone", "Average", ZoneITEq( Loop ).Name );
				SetupOutputVariable( "ITE Air Inlet Dewpoint Temperature Difference Below Operating Range [deltaC]", ZoneITEq( Loop ).DewpointTBelowDeltaT, "Zone", "Average", ZoneITEq( Loop ).Name );
				SetupOutputVariable( "ITE Air Inlet Relative Humidity Difference Above Operating Range [%]", ZoneITEq( Loop ).RHAboveDeltaRH, "Zone", "Average", ZoneITEq( Loop ).Name );
				SetupOutputVariable( "ITE Air Inlet Relative Humidity Difference Below Operating Range [%]", ZoneITEq( Loop ).RHBelowDeltaRH, "Zone", "Average", ZoneITEq( Loop ).Name );

				// Zone total report variables
				if ( RepVarSet( ZoneITEq( Loop ).ZonePtr ) ) {
					RepVarSet( ZoneITEq( Loop ).ZonePtr ) = false;
					SetupOutputVariable( "Zone ITE CPU Electric Power [W]", ZnRpt( ZoneITEq( Loop ).ZonePtr ).ITEqCPUPower, "Zone", "Average", Zone( ZoneITEq( Loop ).ZonePtr ).Name );
					SetupOutputVariable( "Zone ITE Fan Electric Power [W]", ZnRpt( ZoneITEq( Loop ).ZonePtr ).ITEqFanPower, "Zone", "Average", Zone( ZoneITEq( Loop ).ZonePtr ).Name );
					SetupOutputVariable( "Zone ITE UPS Electric Power [W]", ZnRpt( ZoneITEq( Loop ).ZonePtr ).ITEqUPSPower, "Zone", "Average", Zone( ZoneITEq( Loop ).ZonePtr ).Name );
					SetupOutputVariable( "Zone ITE CPU Electric Power at Design Inlet Conditions[W]", ZnRpt( ZoneITEq( Loop ).ZonePtr ).ITEqCPUPowerAtDesign, "Zone", "Average", Zone( ZoneITEq( Loop ).ZonePtr ).Name );
					SetupOutputVariable( "Zone ITE Fan Electric Power at Design Inlet Conditions[W]", ZnRpt( ZoneITEq( Loop ).ZonePtr ).ITEqFanPowerAtDesign, "Zone", "Average", Zone( ZoneITEq( Loop ).ZonePtr ).Name );
					SetupOutputVariable( "Zone ITE UPS Heat Gain to Zone Rate [W]", ZnRpt( ZoneITEq( Loop ).ZonePtr ).ITEqUPSGainRateToZone, "Zone", "Average", Zone( ZoneITEq( Loop ).ZonePtr ).Name );
					SetupOutputVariable( "Zone ITE Total Heat Gain to Zone Rate [W]", ZnRpt( ZoneITEq( Loop ).ZonePtr ).ITEqConGainRateToZone, "Zone", "Average", Zone( ZoneITEq( Loop ).ZonePtr ).Name );

					SetupOutputVariable( "Zone ITE CPU Electric Energy [J]", ZnRpt( ZoneITEq( Loop ).ZonePtr ).ITEqCPUConsumption, "Zone", "Sum", Zone( ZoneITEq( Loop ).ZonePtr ).Name );
					SetupOutputVariable( "Zone ITE Fan Electric Energy [J]", ZnRpt( ZoneITEq( Loop ).ZonePtr ).ITEqFanConsumption, "Zone", "Sum", Zone( ZoneITEq( Loop ).ZonePtr ).Name );
					SetupOutputVariable( "Zone ITE UPS Electric Energy [J]", ZnRpt( ZoneITEq( Loop ).ZonePtr ).ITEqUPSConsumption, "Zone", "Sum", Zone( ZoneITEq( Loop ).ZonePtr ).Name );
					SetupOutputVariable( "Zone ITE CPU Electric Energy at Design Inlet Conditions [J]", ZnRpt( ZoneITEq( Loop ).ZonePtr ).ITEqCPUEnergyAtDesign, "Zone", "Sum", Zone( ZoneITEq( Loop ).ZonePtr ).Name );
					SetupOutputVariable( "Zone ITE Fan Electric Energy at Design Inlet Conditions [J]", ZnRpt( ZoneITEq( Loop ).ZonePtr ).ITEqFanEnergyAtDesign, "Zone", "Sum", Zone( ZoneITEq( Loop ).ZonePtr ).Name );
					SetupOutputVariable( "Zone ITE UPS Heat Gain to Zone Energy [J]", ZnRpt( ZoneITEq( Loop ).ZonePtr ).ITEqUPSGainEnergyToZone, "Zone", "Sum", Zone( ZoneITEq( Loop ).ZonePtr ).Name );
					SetupOutputVariable( "Zone ITE Total Heat Gain to Zone Energy [J]", ZnRpt( ZoneITEq( Loop ).ZonePtr ).ITEqConGainEnergyToZone, "Zone", "Sum", Zone( ZoneITEq( Loop ).ZonePtr ).Name );

					SetupOutputVariable( "Zone ITE Standard Density Air Volume Flow Rate [m3/s]", ZnRpt( ZoneITEq( Loop ).ZonePtr ).ITEqAirVolFlowStdDensity, "Zone", "Average", Zone( ZoneITEq( Loop ).ZonePtr ).Name );
					SetupOutputVariable( "Zone ITE Air Mass Flow Rate [kg/s]", ZnRpt( ZoneITEq( Loop ).ZonePtr ).ITEqAirMassFlow, "Zone", "Average", Zone( ZoneITEq( Loop ).ZonePtr ).Name );
					SetupOutputVariable( "Zone ITE Average Supply Heat Index []", ZnRpt( ZoneITEq( Loop ).ZonePtr ).ITEqSHI, "Zone", "Average", Zone( ZoneITEq( Loop ).ZonePtr ).Name );
					SetupOutputVariable( "Zone ITE Any Air Inlet Operating Range Exceeded Time [hr]", ZnRpt( ZoneITEq( Loop ).ZonePtr ).ITEqTimeOutOfOperRange, "Zone", "Sum", Zone( ZoneITEq( Loop ).ZonePtr ).Name );
					SetupOutputVariable( "Zone ITE Any Air Inlet Dry-Bulb Temperature Above Operating Range Time [hr]", ZnRpt( ZoneITEq( Loop ).ZonePtr ).ITEqTimeAboveDryBulbT, "Zone", "Sum", Zone( ZoneITEq( Loop ).ZonePtr ).Name );
					SetupOutputVariable( "Zone ITE Any Air Inlet Dry-Bulb Temperature Below Operating Range Time [hr]", ZnRpt( ZoneITEq( Loop ).ZonePtr ).ITEqTimeBelowDryBulbT, "Zone", "Sum", Zone( ZoneITEq( Loop ).ZonePtr ).Name );
					SetupOutputVariable( "Zone ITE Any Air Inlet Dewpoint Temperature Above Operating Range Time [hr]", ZnRpt( ZoneITEq( Loop ).ZonePtr ).ITEqTimeAboveDewpointT, "Zone", "Sum", Zone( ZoneITEq( Loop ).ZonePtr ).Name );
					SetupOutputVariable( "Zone ITE Any Air Inlet Dewpoint Temperature Below Operating Range Time [hr]", ZnRpt( ZoneITEq( Loop ).ZonePtr ).ITEqTimeBelowDewpointT, "Zone", "Sum", Zone( ZoneITEq( Loop ).ZonePtr ).Name );
					SetupOutputVariable( "Zone ITE Any Air Inlet Relative Humidity Above Operating Range Time [hr]", ZnRpt( ZoneITEq( Loop ).ZonePtr ).ITEqTimeAboveRH, "Zone", "Sum", Zone( ZoneITEq( Loop ).ZonePtr ).Name );
					SetupOutputVariable( "Zone ITE Any Air Inlet Relative Humidity Below Operating Range Time [hr]", ZnRpt( ZoneITEq( Loop ).ZonePtr ).ITEqTimeBelowRH, "Zone", "Sum", Zone( ZoneITEq( Loop ).ZonePtr ).Name );
				}

				// MJW - EMS Not in place yet
				// if ( AnyEnergyManagementSystemInModel ) {
				// SetupEMSActuator( "ElectricEquipment", ZoneITEq( Loop ).Name, "Electric Power Level", "[W]", ZoneITEq( Loop ).EMSZoneEquipOverrideOn, ZoneITEq( Loop ).EMSEquipPower );
				// SetupEMSInternalVariable( "Plug and Process Power Design Level", ZoneITEq( Loop ).Name, "[W]", ZoneITEq( Loop ).DesignTotalPower );
				// } // EMS

				if ( !ErrorsFound ) SetupZoneInternalGain( ZoneITEq( Loop ).ZonePtr, "ElectricEquipment:ITE:AirCooled", ZoneITEq( Loop ).Name, IntGainTypeOf_ElectricEquipmentITEAirCooled, ZoneITEq( Loop ).ConGainRateToZone );

			} // Item - Number of ZoneITEq objects
		} // Check on number of ZoneITEq

		RepVarSet = true;
		CurrentModuleObject = "ZoneBaseboard:OutdoorTemperatureControlled";
		TotBBHeat = GetNumObjectsFound( CurrentModuleObject );
		ZoneBBHeat.allocate( TotBBHeat );

		for ( Loop = 1; Loop <= TotBBHeat; ++Loop ) {
			AlphaName = "";
			IHGNumbers = 0.0;
			GetObjectItem( CurrentModuleObject, Loop, AlphaName, NumAlpha, IHGNumbers, NumNumber, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( AlphaName( 1 ), ZoneBBHeat, Loop - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) AlphaName( 1 ) = "xxxxx";
			}
			ZoneBBHeat( Loop ).Name = AlphaName( 1 );

			ZoneBBHeat( Loop ).ZonePtr = FindItemInList( AlphaName( 2 ), Zone );
			if ( ZoneBBHeat( Loop ).ZonePtr == 0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", invalid " + cAlphaFieldNames( 2 ) + " entered=" + AlphaName( 2 ) );
				ErrorsFound = true;
			}

			ZoneBBHeat( Loop ).SchedPtr = GetScheduleIndex( AlphaName( 3 ) );
			if ( ZoneBBHeat( Loop ).SchedPtr == 0 ) {
				if ( lAlphaFieldBlanks( 3 ) ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", " + cAlphaFieldNames( 3 ) + " is required." );
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", invalid " + cAlphaFieldNames( 3 ) + " entered=" + AlphaName( 3 ) );
				}
				ErrorsFound = true;
			} else { // check min/max on schedule
				SchMin = GetScheduleMinValue( ZoneBBHeat( Loop ).SchedPtr );
				SchMax = GetScheduleMaxValue( ZoneBBHeat( Loop ).SchedPtr );
				if ( SchMin < 0.0 || SchMax < 0.0 ) {
					if ( SchMin < 0.0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", " + cAlphaFieldNames( 3 ) + ", minimum is < 0.0" );
						ShowContinueError( "Schedule=\"" + AlphaName( 3 ) + "\". Minimum is [" + RoundSigDigits( SchMin, 1 ) + "]. Values must be >= 0.0." );
						ErrorsFound = true;
					}
					if ( SchMax < 0.0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", " + cAlphaFieldNames( 3 ) + ", maximum is < 0.0" );
						ShowContinueError( "Schedule=\"" + AlphaName( 3 ) + "\". Maximum is [" + RoundSigDigits( SchMax, 1 ) + "]. Values must be >= 0.0." );
						ErrorsFound = true;
					}
				}
			}

			if ( NumAlpha > 3 ) {
				ZoneBBHeat( Loop ).EndUseSubcategory = AlphaName( 4 );
			} else {
				ZoneBBHeat( Loop ).EndUseSubcategory = "General";
			}

			ZoneBBHeat( Loop ).CapatLowTemperature = IHGNumbers( 1 );
			ZoneBBHeat( Loop ).LowTemperature = IHGNumbers( 2 );
			ZoneBBHeat( Loop ).CapatHighTemperature = IHGNumbers( 3 );
			ZoneBBHeat( Loop ).HighTemperature = IHGNumbers( 4 );
			ZoneBBHeat( Loop ).FractionRadiant = IHGNumbers( 5 );
			ZoneBBHeat( Loop ).FractionConvected = 1.0 - ZoneBBHeat( Loop ).FractionRadiant;
			if ( ZoneBBHeat( Loop ).FractionConvected < 0.0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", Sum of Fractions > 1.0" );
				ErrorsFound = true;
			}

			if ( ZoneBBHeat( Loop ).ZonePtr <= 0 ) continue; // Error, will be caught and terminated later

			// Object report variables
			SetupOutputVariable( "Baseboard Electric Power [W]", ZoneBBHeat( Loop ).Power, "Zone", "Average", ZoneBBHeat( Loop ).Name );
			SetupOutputVariable( "Baseboard Electric Energy [J]", ZoneBBHeat( Loop ).Consumption, "Zone", "Sum", ZoneBBHeat( Loop ).Name, _, "Electricity", "InteriorEquipment", ZoneBBHeat( Loop ).EndUseSubcategory, "Building", Zone( ZoneBBHeat( Loop ).ZonePtr ).Name, Zone( ZoneBBHeat( Loop ).ZonePtr ).Multiplier, Zone( ZoneBBHeat( Loop ).ZonePtr ).ListMultiplier );

			SetupOutputVariable( "Baseboard Radiant Heating Energy [J]", ZoneBBHeat( Loop ).RadGainEnergy, "Zone", "Sum", ZoneBBHeat( Loop ).Name );
			SetupOutputVariable( "Baseboard Radiant Heating Rate [W]", ZoneBBHeat( Loop ).RadGainRate, "Zone", "Average", ZoneBBHeat( Loop ).Name );
			SetupOutputVariable( "Baseboard Convective Heating Energy [J]", ZoneBBHeat( Loop ).ConGainEnergy, "Zone", "Sum", ZoneBBHeat( Loop ).Name );
			SetupOutputVariable( "Baseboard Convective Heating Rate [W]", ZoneBBHeat( Loop ).ConGainRate, "Zone", "Average", ZoneBBHeat( Loop ).Name );
			SetupOutputVariable( "Baseboard Total Heating Energy [J]", ZoneBBHeat( Loop ).TotGainEnergy, "Zone", "Sum", ZoneBBHeat( Loop ).Name );
			SetupOutputVariable( "Baseboard Total Heating Rate [W]", ZoneBBHeat( Loop ).TotGainRate, "Zone", "Average", ZoneBBHeat( Loop ).Name );

			// Zone total report variables
			if ( RepVarSet( ZoneBBHeat( Loop ).ZonePtr ) ) {
				RepVarSet( ZoneBBHeat( Loop ).ZonePtr ) = false;
				SetupOutputVariable( "Zone Baseboard Electric Power [W]", ZnRpt( ZoneBBHeat( Loop ).ZonePtr ).BaseHeatPower, "Zone", "Average", Zone( ZoneBBHeat( Loop ).ZonePtr ).Name );
				SetupOutputVariable( "Zone Baseboard Electric Energy [J]", ZnRpt( ZoneBBHeat( Loop ).ZonePtr ).BaseHeatElecCons, "Zone", "Sum", Zone( ZoneBBHeat( Loop ).ZonePtr ).Name );

				SetupOutputVariable( "Zone Baseboard Radiant Heating Energy [J]", ZnRpt( ZoneBBHeat( Loop ).ZonePtr ).BaseHeatRadGain, "Zone", "Sum", Zone( ZoneBBHeat( Loop ).ZonePtr ).Name );
				SetupOutputVariable( "Zone Baseboard Radiant Heating Rate [W]", ZnRpt( ZoneBBHeat( Loop ).ZonePtr ).BaseHeatRadGainRate, "Zone", "Average", Zone( ZoneBBHeat( Loop ).ZonePtr ).Name );
				SetupOutputVariable( "Zone Baseboard Convective Heating Energy [J]", ZnRpt( ZoneBBHeat( Loop ).ZonePtr ).BaseHeatConGain, "Zone", "Sum", Zone( ZoneBBHeat( Loop ).ZonePtr ).Name );
				SetupOutputVariable( "Zone Baseboard Convective Heating Rate [W]", ZnRpt( ZoneBBHeat( Loop ).ZonePtr ).BaseHeatConGainRate, "Zone", "Average", Zone( ZoneBBHeat( Loop ).ZonePtr ).Name );
				SetupOutputVariable( "Zone Baseboard Total Heating Energy [J]", ZnRpt( ZoneBBHeat( Loop ).ZonePtr ).BaseHeatTotGain, "Zone", "Sum", Zone( ZoneBBHeat( Loop ).ZonePtr ).Name );
				SetupOutputVariable( "Zone Baseboard Total Heating Rate [W]", ZnRpt( ZoneBBHeat( Loop ).ZonePtr ).BaseHeatTotGainRate, "Zone", "Average", Zone( ZoneBBHeat( Loop ).ZonePtr ).Name );
			}

			if ( AnyEnergyManagementSystemInModel ) {
				SetupEMSActuator( "ZoneBaseboard:OutdoorTemperatureControlled", ZoneBBHeat( Loop ).Name, "Power Level", "[W]", ZoneBBHeat( Loop ).EMSZoneBaseboardOverrideOn, ZoneBBHeat( Loop ).EMSZoneBaseboardPower );
				SetupEMSInternalVariable( "Simple Zone Baseboard Capacity At Low Temperature", ZoneBBHeat( Loop ).Name, "[W]", ZoneBBHeat( Loop ).CapatLowTemperature );
				SetupEMSInternalVariable( "Simple Zone Baseboard Capacity At High Temperature", ZoneBBHeat( Loop ).Name, "[W]", ZoneBBHeat( Loop ).CapatHighTemperature );
			} // EMS

			SetupZoneInternalGain( ZoneBBHeat( Loop ).ZonePtr, "ZoneBaseboard:OutdoorTemperatureControlled", ZoneBBHeat( Loop ).Name, IntGainTypeOf_ZoneBaseboardOutdoorTemperatureControlled, ZoneBBHeat( Loop ).ConGainRate, _, ZoneBBHeat( Loop ).RadGainRate );

		}

		RepVarSet = true;
		CurrentModuleObject = "ZoneContaminantSourceAndSink:CarbonDioxide";
		TotCO2Gen = GetNumObjectsFound( CurrentModuleObject );
		ZoneCO2Gen.allocate( TotCO2Gen );

		for ( Loop = 1; Loop <= TotCO2Gen; ++Loop ) {
			AlphaName = "";
			IHGNumbers = 0.0;
			GetObjectItem( CurrentModuleObject, Loop, AlphaName, NumAlpha, IHGNumbers, NumNumber, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( AlphaName( 1 ), ZoneCO2Gen, Loop - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) AlphaName( 1 ) = "xxxxx";
			}
			ZoneCO2Gen( Loop ).Name = AlphaName( 1 );

			ZoneCO2Gen( Loop ).ZonePtr = FindItemInList( AlphaName( 2 ), Zone );
			if ( ZoneCO2Gen( Loop ).ZonePtr == 0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", invalid " + cAlphaFieldNames( 2 ) + " entered=" + AlphaName( 2 ) );
				ErrorsFound = true;
			}

			ZoneCO2Gen( Loop ).SchedPtr = GetScheduleIndex( AlphaName( 3 ) );
			if ( ZoneCO2Gen( Loop ).SchedPtr == 0 ) {
				if ( lAlphaFieldBlanks( 3 ) ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", " + cAlphaFieldNames( 3 ) + " is required." );
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", invalid " + cAlphaFieldNames( 3 ) + " entered=" + AlphaName( 3 ) );
				}
				ErrorsFound = true;
			} else { // check min/max on schedule
				SchMin = GetScheduleMinValue( ZoneCO2Gen( Loop ).SchedPtr );
				SchMax = GetScheduleMaxValue( ZoneCO2Gen( Loop ).SchedPtr );
				if ( SchMin < 0.0 || SchMax < 0.0 ) {
					if ( SchMin < 0.0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", " + cAlphaFieldNames( 3 ) + ", minimum is < 0.0" );
						ShowContinueError( "Schedule=\"" + AlphaName( 3 ) + "\". Minimum is [" + RoundSigDigits( SchMin, 1 ) + "]. Values must be >= 0.0." );
						ErrorsFound = true;
					}
					if ( SchMax < 0.0 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphaName( 1 ) + "\", " + cAlphaFieldNames( 3 ) + ", maximum is < 0.0" );
						ShowContinueError( "Schedule=\"" + AlphaName( 3 ) + "\". Maximum is [" + RoundSigDigits( SchMax, 1 ) + "]. Values must be >= 0.0." );
						ErrorsFound = true;
					}
				}
			}

			ZoneCO2Gen( Loop ).CO2DesignRate = IHGNumbers( 1 );

			if ( ZoneCO2Gen( Loop ).ZonePtr <= 0 ) continue; // Error, will be caught and terminated later

			// Object report variables
			SetupOutputVariable( "Contaminant Source or Sink CO2 Gain Volume Flow Rate [m3/s]", ZoneCO2Gen( Loop ).CO2GainRate, "Zone", "Average", ZoneCO2Gen( Loop ).Name );

			// Zone total report variables
			if ( RepVarSet( ZoneCO2Gen( Loop ).ZonePtr ) ) {
				RepVarSet( ZoneCO2Gen( Loop ).ZonePtr ) = false;

				SetupOutputVariable( "Zone Contaminant Source or Sink CO2 Gain Volume Flow Rate [m3/s]", ZnRpt( ZoneCO2Gen( Loop ).ZonePtr ).CO2Rate, "Zone", "Average", Zone( ZoneCO2Gen( Loop ).ZonePtr ).Name );

			}

			SetupZoneInternalGain( ZoneCO2Gen( Loop ).ZonePtr, "ZoneContaminantSourceAndSink:CarbonDioxide", ZoneCO2Gen( Loop ).Name, IntGainTypeOf_ZoneContaminantSourceAndSinkCarbonDioxide, _, _, _, _, _, ZoneCO2Gen( Loop ).CO2GainRate );

		}

		RepVarSet.deallocate();
		IHGNumbers.deallocate();
		AlphaName.deallocate();

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in Getting Internal Gains Input, Program Stopped" );
		}

		gio::write( OutputFileInits, Format_721 );
		for ( Loop = 1; Loop <= NumOfZones; ++Loop ) {
			LightTot = 0.0;
			ElecTot = 0.0;
			GasTot = 0.0;
			OthTot = 0.0;
			HWETot = 0.0;
			StmTot = 0.0;
			BBHeatInd = "No";
			for ( Loop1 = 1; Loop1 <= TotLights; ++Loop1 ) {
				if ( Lights( Loop1 ).ZonePtr != Loop ) continue;
				LightTot += Lights( Loop1 ).DesignLevel;
			}
			for ( Loop1 = 1; Loop1 <= TotElecEquip; ++Loop1 ) {
				if ( ZoneElectric( Loop1 ).ZonePtr != Loop ) continue;
				ElecTot += ZoneElectric( Loop1 ).DesignLevel;
			}
			for ( Loop1 = 1; Loop1 <= NumZoneITEqStatements; ++Loop1 ) {
				if ( ZoneITEq( Loop1 ).ZonePtr != Loop ) continue;
				ElecTot += ZoneITEq( Loop1 ).DesignTotalPower;
			}
			for ( Loop1 = 1; Loop1 <= TotGasEquip; ++Loop1 ) {
				if ( ZoneGas( Loop1 ).ZonePtr != Loop ) continue;
				GasTot += ZoneGas( Loop1 ).DesignLevel;
			}
			for ( Loop1 = 1; Loop1 <= TotOthEquip; ++Loop1 ) {
				if ( ZoneOtherEq( Loop1 ).ZonePtr != Loop ) continue;
				OthTot += ZoneOtherEq( Loop1 ).DesignLevel;
			}
			for ( Loop1 = 1; Loop1 <= TotStmEquip; ++Loop1 ) {
				if ( ZoneSteamEq( Loop1 ).ZonePtr != Loop ) continue;
				StmTot += ZoneSteamEq( Loop1 ).DesignLevel;
			}
			for ( Loop1 = 1; Loop1 <= TotHWEquip; ++Loop1 ) {
				if ( ZoneHWEq( Loop1 ).ZonePtr != Loop ) continue;
				HWETot += ZoneHWEq( Loop1 ).DesignLevel;
			}
			for ( Loop1 = 1; Loop1 <= TotBBHeat; ++Loop1 ) {
				if ( ZoneBBHeat( Loop1 ).ZonePtr != Loop ) continue;
				BBHeatInd = "Yes";
			}
			Zone( Loop ).InternalHeatGains = LightTot + ElecTot + GasTot + OthTot + HWETot + StmTot;
			if ( Zone( Loop ).FloorArea > 0.0 ) {
				{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, Format_720, flags ) << Zone( Loop ).Name << RoundSigDigits( Zone( Loop ).FloorArea, 2 ) << RoundSigDigits( Zone( Loop ).TotOccupants, 1 ); }
				if ( Zone( Loop ).TotOccupants > 0.0 ) {
					StringOut = RoundSigDigits( Zone( Loop ).FloorArea / Zone( Loop ).TotOccupants, 3 );
				} else {
					StringOut = "N/A";
				}
				{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
				StringOut = RoundSigDigits( Zone( Loop ).TotOccupants / Zone( Loop ).FloorArea, 3 );
				{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
				StringOut = RoundSigDigits( LightTot / Zone( Loop ).FloorArea, 3 );
				{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
				StringOut = RoundSigDigits( ElecTot / Zone( Loop ).FloorArea, 3 );
				{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
				StringOut = RoundSigDigits( GasTot / Zone( Loop ).FloorArea, 3 );
				{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
				StringOut = RoundSigDigits( OthTot / Zone( Loop ).FloorArea, 3 );
				{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
				StringOut = RoundSigDigits( HWETot / Zone( Loop ).FloorArea, 3 );
				{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
				StringOut = RoundSigDigits( StmTot / Zone( Loop ).FloorArea, 3 );
				{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
				StringOut = RoundSigDigits( Zone( Loop ).InternalHeatGains / Zone( Loop ).FloorArea, 3 );
				gio::write( OutputFileInits, fmtA ) << StringOut + ',' + BBHeatInd;
			} else {
				{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, Format_720, flags ) << Zone( Loop ).Name << RoundSigDigits( Zone( Loop ).FloorArea, 2 ) << RoundSigDigits( Zone( Loop ).TotOccupants, 1 ); }
				gio::write( OutputFileInits, fmtA ) << "0.0,N/A,N/A,N/A,N/A,N/A,N/A,N/A,N/A" + BBHeatInd;
			}
		}
		for ( Loop = 1; Loop <= TotPeople; ++Loop ) {
			if ( Loop == 1 ) { IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, Format_723, flags ) << "People" << "Number of People {},People/Floor Area {person/m2},Floor Area per person {m2/person},Fraction Radiant,Fraction Convected,Sensible Fraction Calculation,Activity level,ASHRAE 55 Warnings,Carbon Dioxide Generation Rate,Nominal Minimum Number of People,Nominal Maximum Number of People"; };
			if ( Loop == 1 ) {
				if ( People( Loop ).Fanger || People( Loop ).Pierce || People( Loop ).KSU ) {
					gio::write( OutputFileInits, fmtA ) << ",MRT Calculation Type,Work Efficiency, Clothing Insulation Calculation Method,Clothing Insulation Calculation Method Schedule,Clothing,Air Velocity,Fanger Calculation,Pierce Calculation,KSU Calculation";
				} else {
					gio::write( OutputFileInits );
				}
			}

			ZoneNum = People( Loop ).ZonePtr;

			if ( ZoneNum == 0 ) {
				gio::write( OutputFileInits, Format_724 ) << "People-Illegal Zone specified" << People( Loop ).Name;
				continue;
			}

			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, Format_722, flags ) << "People" << People( Loop ).Name << GetScheduleName( People( Loop ).NumberOfPeoplePtr ) << Zone( ZoneNum ).Name << RoundSigDigits( Zone( ZoneNum ).FloorArea, 2 ) << RoundSigDigits( Zone( ZoneNum ).TotOccupants, 1 ); }
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << RoundSigDigits( People( Loop ).NumberOfPeople, 1 ) + ','; }
			if ( Zone( ZoneNum ).FloorArea > 0.0 ) {
				StringOut = RoundSigDigits( People( Loop ).NumberOfPeople / Zone( ZoneNum ).FloorArea, 3 );
			} else {
				StringOut = "N/A";
			}
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			if ( People( Loop ).NumberOfPeople > 0.0 ) {
				if ( Zone( ZoneNum ).FloorArea > 0.0 ) {
					StringOut = RoundSigDigits( Zone( ZoneNum ).FloorArea / People( Loop ).NumberOfPeople, 3 );
				} else {
					StringOut = "N/A";
				}
			} else {
				StringOut = "N/A";
			}
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits( People( Loop ).FractionRadiant, 3 );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits( People( Loop ).FractionConvected, 3 );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			if ( People( Loop ).UserSpecSensFrac == AutoCalculate ) {
				StringOut = "AutoCalculate";
			} else {
				StringOut = RoundSigDigits( People( Loop ).UserSpecSensFrac, 3 );
			}
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = GetScheduleName( People( Loop ).ActivityLevelPtr );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			if ( People( Loop ).Show55Warning ) {
				StringOut = "Yes";
			} else {
				StringOut = "No";
			}
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits( People( Loop ).CO2RateFactor, 4 );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits( People( Loop ).NomMinNumberPeople, 0 );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits( People( Loop ).NomMaxNumberPeople, 0 );
			if ( People( Loop ).Fanger || People( Loop ).Pierce || People( Loop ).KSU ) {
				{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
				if ( People( Loop ).MRTCalcType == ZoneAveraged ) {
					StringOut = "Zone Averaged";
				} else if ( People( Loop ).MRTCalcType == SurfaceWeighted ) {
					StringOut = "Surface Weighted";
				} else if ( People( Loop ).MRTCalcType == AngleFactor ) {
					StringOut = "Angle Factor";
				} else {
					StringOut = "N/A";
				}
				{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
				{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << GetScheduleName( People( Loop ).WorkEffPtr ) + ','; }

				if ( People( Loop ).ClothingType == 1 ) {
					StringOut = "Clothing Insulation Schedule";
				} else if ( People( Loop ).ClothingType == 2 ) {
					StringOut = "Dynamic Clothing Model ASHRAE55";
				} else if ( People( Loop ).ClothingType == 3 ) {
					StringOut = "Calculation Method Schedule";
				} else {
					StringOut = "N/A";
				}
				{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }

				if ( People( Loop ).ClothingType == 3 ) {
					StringOut = GetScheduleName( People( Loop ).ClothingMethodPtr );
				} else {
					StringOut = "N/A";
				}
				{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }

				{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << GetScheduleName( People( Loop ).ClothingPtr ) + ','; }
				{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << GetScheduleName( People( Loop ).AirVelocityPtr ) + ','; }
				if ( People( Loop ).Fanger ) {
					StringOut = "Yes";
				} else {
					StringOut = "No";
				}
				{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
				if ( People( Loop ).Pierce ) {
					StringOut = "Yes";
				} else {
					StringOut = "No";
				}
				{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
				if ( People( Loop ).KSU ) {
					StringOut = "Yes";
				} else {
					StringOut = "No";
				}
				gio::write( OutputFileInits, fmtA ) << StringOut;
			} else {
				gio::write( OutputFileInits, fmtA ) << StringOut;
			}
		}
		for ( Loop = 1; Loop <= TotLights; ++Loop ) {
			if ( Loop == 1 ) gio::write( OutputFileInits, Format_723 ) << "Lights" << "Lighting Level {W},Lights/Floor Area {W/m2},Lights per person {W/person},Fraction Return Air,Fraction Radiant,Fraction Short Wave,Fraction Convected,Fraction Replaceable,End-Use Category,Nominal Minimum Lighting Level {W},Nominal Maximum Lighting Level {W}";

			ZoneNum = Lights( Loop ).ZonePtr;

			if ( ZoneNum == 0 ) {
				gio::write( OutputFileInits, Format_724 ) << "Lights-Illegal Zone specified" << Lights( Loop ).Name;
				continue;
			}

			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, Format_722, flags ) << "Lights" << Lights( Loop ).Name << GetScheduleName( Lights( Loop ).SchedPtr ) << Zone( ZoneNum ).Name << RoundSigDigits( Zone( ZoneNum ).FloorArea, 2 ) << RoundSigDigits( Zone( ZoneNum ).TotOccupants, 1 ); }

			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << RoundSigDigits( Lights( Loop ).DesignLevel, 3 ) + ','; }
			if ( Zone( ZoneNum ).FloorArea > 0.0 ) {
				StringOut = RoundSigDigits( Lights( Loop ).DesignLevel / Zone( ZoneNum ).FloorArea, 3 );
			} else {
				StringOut = "N/A";
			}
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			if ( Zone( ZoneNum ).TotOccupants > 0.0 ) {
				StringOut = RoundSigDigits( Lights( Loop ).DesignLevel / Zone( ZoneNum ).TotOccupants, 3 );
			} else {
				StringOut = "N/A";
			}
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits( Lights( Loop ).FractionReturnAir, 3 );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits( Lights( Loop ).FractionRadiant, 3 );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits( Lights( Loop ).FractionShortWave, 3 );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits( Lights( Loop ).FractionConvected, 3 );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits( Lights( Loop ).FractionReplaceable, 3 );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << Lights( Loop ).EndUseSubcategory + ','; }
			StringOut = RoundSigDigits( Lights( Loop ).NomMinDesignLevel, 3 );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits( Lights( Loop ).NomMaxDesignLevel, 3 );
			gio::write( OutputFileInits, fmtA ) << StringOut;
		}
		for ( Loop = 1; Loop <= TotElecEquip; ++Loop ) {
			if ( Loop == 1 ) gio::write( OutputFileInits, Format_723 ) << "ElectricEquipment" << "Equipment Level {W},Equipment/Floor Area {W/m2},Equipment per person {W/person},Fraction Latent,Fraction Radiant,Fraction Lost,Fraction Convected,End-Use SubCategory,Nominal Minimum Equipment Level {W},Nominal Maximum Equipment Level {W}";

			ZoneNum = ZoneElectric( Loop ).ZonePtr;

			if ( ZoneNum == 0 ) {
				gio::write( OutputFileInits, Format_724 ) << "Electric Equipment-Illegal Zone specified" << ZoneElectric( Loop ).Name;
				continue;
			}

			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, Format_722, flags ) << "ElectricEquipment" << ZoneElectric( Loop ).Name << GetScheduleName( ZoneElectric( Loop ).SchedPtr ) << Zone( ZoneNum ).Name << RoundSigDigits( Zone( ZoneNum ).FloorArea, 2 ) << RoundSigDigits( Zone( ZoneNum ).TotOccupants, 1 ); }

			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << RoundSigDigits( ZoneElectric( Loop ).DesignLevel, 3 ) + ','; }
			if ( Zone( ZoneNum ).FloorArea > 0.0 ) {
				StringOut = RoundSigDigits( ZoneElectric( Loop ).DesignLevel / Zone( ZoneNum ).FloorArea, 3 );
			} else {
				StringOut = "N/A";
			}
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			if ( Zone( ZoneNum ).TotOccupants > 0.0 ) {
				StringOut = RoundSigDigits( ZoneElectric( Loop ).DesignLevel / Zone( ZoneNum ).TotOccupants, 3 );
			} else {
				StringOut = "N/A";
			}
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits( ZoneElectric( Loop ).FractionLatent, 3 );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits( ZoneElectric( Loop ).FractionRadiant, 3 );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits( ZoneElectric( Loop ).FractionLost, 3 );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits( ZoneElectric( Loop ).FractionConvected, 3 );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << ZoneElectric( Loop ).EndUseSubcategory + ','; }
			StringOut = RoundSigDigits( ZoneElectric( Loop ).NomMinDesignLevel, 3 );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits( ZoneElectric( Loop ).NomMaxDesignLevel, 3 );
			gio::write( OutputFileInits, fmtA ) << StringOut;
		}
		for ( Loop = 1; Loop <= TotGasEquip; ++Loop ) {
			if ( Loop == 1 ) gio::write( OutputFileInits, Format_723 ) << "GasEquipment" << "Equipment Level {W},Equipment/Floor Area {W/m2},Equipment per person {W/person},Fraction Latent,Fraction Radiant,Fraction Lost,Fraction Convected,End-Use SubCategory,Nominal Minimum Equipment Level {W},Nominal Maximum Equipment Level {W}";

			ZoneNum = ZoneGas( Loop ).ZonePtr;

			if ( ZoneNum == 0 ) {
				gio::write( OutputFileInits, Format_724 ) << "Gas Equipment-Illegal Zone specified" << ZoneGas( Loop ).Name;
				continue;
			}

			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, Format_722, flags ) << "GasEquipment" << ZoneGas( Loop ).Name << GetScheduleName( ZoneGas( Loop ).SchedPtr ) << Zone( ZoneNum ).Name << RoundSigDigits( Zone( ZoneNum ).FloorArea, 2 ) << RoundSigDigits( Zone( ZoneNum ).TotOccupants, 1 ); }

			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << RoundSigDigits( ZoneGas( Loop ).DesignLevel, 3 ) + ','; }

			if ( Zone( ZoneNum ).FloorArea > 0.0 ) {
				StringOut = RoundSigDigits( ZoneGas( Loop ).DesignLevel / Zone( ZoneNum ).FloorArea, 3 );
			} else {
				StringOut = "N/A";
			}
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			if ( Zone( ZoneNum ).TotOccupants > 0.0 ) {
				StringOut = RoundSigDigits( ZoneGas( Loop ).DesignLevel / Zone( ZoneNum ).TotOccupants, 3 );
			} else {
				StringOut = "N/A";
			}
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits( ZoneGas( Loop ).FractionLatent, 3 );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits( ZoneGas( Loop ).FractionRadiant, 3 );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits( ZoneGas( Loop ).FractionLost, 3 );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits( ZoneGas( Loop ).FractionConvected, 3 );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << ZoneGas( Loop ).EndUseSubcategory + ','; }
			StringOut = RoundSigDigits( ZoneGas( Loop ).NomMinDesignLevel, 3 );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits( ZoneGas( Loop ).NomMaxDesignLevel, 3 );
			gio::write( OutputFileInits, fmtA ) << StringOut;
		}

		for ( Loop = 1; Loop <= TotHWEquip; ++Loop ) {
			if ( Loop == 1 ) gio::write( OutputFileInits, Format_723 ) << "HotWaterEquipment" << "Equipment Level {W},Equipment/Floor Area {W/m2},Equipment per person {W/person},Fraction Latent,Fraction Radiant,Fraction Lost,Fraction Convected,End-Use SubCategory,Nominal Minimum Equipment Level {W},Nominal Maximum Equipment Level {W}";

			ZoneNum = ZoneHWEq( Loop ).ZonePtr;

			if ( ZoneNum == 0 ) {
				gio::write( OutputFileInits, Format_724 ) << "Hot Water Equipment-Illegal Zone specified" << ZoneHWEq( Loop ).Name;
				continue;
			}

			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, Format_722, flags ) << "HotWaterEquipment" << ZoneHWEq( Loop ).Name << GetScheduleName( ZoneHWEq( Loop ).SchedPtr ) << Zone( ZoneNum ).Name << RoundSigDigits( Zone( ZoneNum ).FloorArea, 2 ) << RoundSigDigits( Zone( ZoneNum ).TotOccupants, 1 ); }

			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << RoundSigDigits( ZoneHWEq( Loop ).DesignLevel, 3 ) + ','; }

			if ( Zone( ZoneNum ).FloorArea > 0.0 ) {
				StringOut = RoundSigDigits( ZoneHWEq( Loop ).DesignLevel / Zone( ZoneNum ).FloorArea, 3 );
			} else {
				StringOut = "N/A";
			}
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			if ( Zone( ZoneNum ).TotOccupants > 0.0 ) {
				StringOut = RoundSigDigits( ZoneHWEq( Loop ).DesignLevel / Zone( ZoneNum ).TotOccupants, 3 );
			} else {
				StringOut = "N/A";
			}
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits( ZoneHWEq( Loop ).FractionLatent, 3 );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits( ZoneHWEq( Loop ).FractionRadiant, 3 );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits( ZoneHWEq( Loop ).FractionLost, 3 );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits( ZoneHWEq( Loop ).FractionConvected, 3 );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << ZoneHWEq( Loop ).EndUseSubcategory + ','; }
			StringOut = RoundSigDigits( ZoneHWEq( Loop ).NomMinDesignLevel, 3 );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits( ZoneHWEq( Loop ).NomMaxDesignLevel, 3 );
			gio::write( OutputFileInits, fmtA ) << StringOut;
		}

		for ( Loop = 1; Loop <= TotStmEquip; ++Loop ) {
			if ( Loop == 1 ) gio::write( OutputFileInits, Format_723 ) << "SteamEquipment" << "Equipment Level {W},Equipment/Floor Area {W/m2},Equipment per person {W/person},Fraction Latent,Fraction Radiant,Fraction Lost,Fraction Convected,End-Use SubCategory,Nominal Minimum Equipment Level {W},Nominal Maximum Equipment Level {W}";

			ZoneNum = ZoneSteamEq( Loop ).ZonePtr;

			if ( ZoneNum == 0 ) {
				gio::write( OutputFileInits, Format_724 ) << "Steam Equipment-Illegal Zone specified" << ZoneSteamEq( Loop ).Name;
				continue;
			}

			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, Format_722, flags ) << "SteamEquipment" << ZoneSteamEq( Loop ).Name << GetScheduleName( ZoneSteamEq( Loop ).SchedPtr ) << Zone( ZoneNum ).Name << RoundSigDigits( Zone( ZoneNum ).FloorArea, 2 ) << RoundSigDigits( Zone( ZoneNum ).TotOccupants, 1 ); }

			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << RoundSigDigits( ZoneSteamEq( Loop ).DesignLevel, 3 ) + ','; }

			if ( Zone( ZoneNum ).FloorArea > 0.0 ) {
				StringOut = RoundSigDigits( ZoneSteamEq( Loop ).DesignLevel / Zone( ZoneNum ).FloorArea, 3 );
			} else {
				StringOut = "N/A";
			}
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			if ( Zone( ZoneNum ).TotOccupants > 0.0 ) {
				StringOut = RoundSigDigits( ZoneSteamEq( Loop ).DesignLevel / Zone( ZoneNum ).TotOccupants, 3 );
			} else {
				StringOut = "N/A";
			}
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits( ZoneSteamEq( Loop ).FractionLatent, 3 );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits( ZoneSteamEq( Loop ).FractionRadiant, 3 );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits( ZoneSteamEq( Loop ).FractionLost, 3 );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits( ZoneSteamEq( Loop ).FractionConvected, 3 );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << ZoneSteamEq( Loop ).EndUseSubcategory + ','; }
			StringOut = RoundSigDigits( ZoneSteamEq( Loop ).NomMinDesignLevel, 3 );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits( ZoneSteamEq( Loop ).NomMaxDesignLevel, 3 );
			gio::write( OutputFileInits, fmtA ) << StringOut;
		}

		for ( Loop = 1; Loop <= TotOthEquip; ++Loop ) {
			if ( Loop == 1 ) gio::write( OutputFileInits, Format_723 ) << "OtherEquipment" << "Equipment Level {W},Equipment/Floor Area {W/m2},Equipment per person {W/person},Fraction Latent,Fraction Radiant,Fraction Lost,Fraction Convected,Nominal Minimum Equipment Level {W},Nominal Maximum Equipment Level {W}";
			ZoneNum = ZoneOtherEq( Loop ).ZonePtr;

			if ( ZoneNum == 0 ) {
				gio::write( OutputFileInits, Format_724 ) << "Other Equipment-Illegal Zone specified" << ZoneOtherEq( Loop ).Name;
				continue;
			}

			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, Format_722, flags ) << "OtherEquipment" << ZoneOtherEq( Loop ).Name << GetScheduleName( ZoneOtherEq( Loop ).SchedPtr ) << Zone( ZoneNum ).Name << RoundSigDigits( Zone( ZoneNum ).FloorArea, 2 ) << RoundSigDigits( Zone( ZoneNum ).TotOccupants, 1 ); }

			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << RoundSigDigits( ZoneOtherEq( Loop ).DesignLevel, 3 ) + ','; }

			if ( Zone( ZoneNum ).FloorArea > 0.0 ) {
				StringOut = RoundSigDigits( ZoneOtherEq( Loop ).DesignLevel / Zone( ZoneNum ).FloorArea, 3 );
			} else {
				StringOut = "N/A";
			}
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			if ( Zone( ZoneNum ).TotOccupants > 0.0 ) {
				StringOut = RoundSigDigits( ZoneOtherEq( Loop ).DesignLevel / Zone( ZoneNum ).TotOccupants, 3 );
			} else {
				StringOut = "N/A";
			}
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits( ZoneOtherEq( Loop ).FractionLatent, 3 );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits( ZoneOtherEq( Loop ).FractionRadiant, 3 );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits( ZoneOtherEq( Loop ).FractionLost, 3 );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits( ZoneOtherEq( Loop ).FractionConvected, 3 );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits( ZoneOtherEq( Loop ).NomMinDesignLevel, 3 );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits( ZoneOtherEq( Loop ).NomMaxDesignLevel, 3 );
			gio::write( OutputFileInits, fmtA ) << StringOut;
		}

		for ( Loop = 1; Loop <= NumZoneITEqStatements; ++Loop ) {
			if ( Loop == 1 ) gio::write( OutputFileInits, Format_723 ) << "ElectricEquipment:ITE:AirCooled" << "Equipment Level {W}," "Equipment/Floor Area {W/m2},Equipment per person {W/person}," "Fraction Convected,CPU End-Use SubCategory,Fan End-Use SubCategory,UPS End-Use SubCategory," "Nominal Minimum Equipment Level {W},Nominal Maximum Equipment Level {W}, Design Air Volume Flow Rate {m3/s}";

			ZoneNum = ZoneITEq( Loop ).ZonePtr;

			if ( ZoneNum == 0 ) {
				gio::write( OutputFileInits, Format_724 ) << "ElectricEquipment:ITE:AirCooled-Illegal Zone specified" << ZoneITEq( Loop ).Name;
				continue;
			}

			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, Format_722, flags ) << "ElectricEquipment:ITE:AirCooled" << ZoneITEq( Loop ).Name << GetScheduleName( ZoneITEq( Loop ).OperSchedPtr ) << Zone( ZoneNum ).Name << RoundSigDigits( Zone( ZoneNum ).FloorArea, 2 ) << RoundSigDigits( Zone( ZoneNum ).TotOccupants, 1 ); }

			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << RoundSigDigits( ZoneITEq( Loop ).DesignTotalPower, 3 ) + ','; }
			if ( Zone( ZoneNum ).FloorArea > 0.0 ) {
				StringOut = RoundSigDigits( ZoneITEq( Loop ).DesignTotalPower / Zone( ZoneNum ).FloorArea, 3 );
			} else {
				StringOut = "N/A";
			}
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			if ( Zone( ZoneNum ).TotOccupants > 0.0 ) {
				StringOut = RoundSigDigits( ZoneITEq( Loop ).DesignTotalPower / Zone( ZoneNum ).TotOccupants, 3 );
			} else {
				StringOut = "N/A";
			}
			StringOut = "1.0"; // ElectricEquipment:ITE:AirCooled is 100% convective
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << ZoneITEq( Loop ).EndUseSubcategoryCPU + ','; }
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << ZoneITEq( Loop ).EndUseSubcategoryFan + ','; }
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << ZoneITEq( Loop ).EndUseSubcategoryUPS + ','; }
			StringOut = RoundSigDigits( ZoneITEq( Loop ).NomMinDesignLevel, 3 );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits( ZoneITEq( Loop ).NomMaxDesignLevel, 3 );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits( ZoneITEq( Loop ).DesignAirVolFlowRate , 10 );
			gio::write( OutputFileInits, fmtA ) << StringOut;
		}

		for ( Loop = 1; Loop <= TotBBHeat; ++Loop ) {
			if ( Loop == 1 ) gio::write( OutputFileInits, Format_723 ) << "Outdoor Controlled Baseboard Heat" << "Capacity at Low Temperature {W},Low Temperature {C},Capacity at High Temperature {W},High Temperature {C},Fraction Radiant,Fraction Convected,End-Use Subcategory";

			ZoneNum = ZoneBBHeat( Loop ).ZonePtr;

			if ( ZoneNum == 0 ) {
				gio::write( OutputFileInits, Format_724 ) << "Outdoor Controlled Baseboard Heat-Illegal Zone specified" << ZoneBBHeat( Loop ).Name;
				continue;
			}

			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, Format_722, flags ) << "Outdoor Controlled Baseboard Heat" << ZoneBBHeat( Loop ).Name << GetScheduleName( ZoneBBHeat( Loop ).SchedPtr ) << Zone( ZoneNum ).Name << RoundSigDigits( Zone( ZoneNum ).FloorArea, 2 ) << RoundSigDigits( Zone( ZoneNum ).TotOccupants, 1 ); }

			StringOut = RoundSigDigits( ZoneBBHeat( Loop ).CapatLowTemperature, 3 );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits( ZoneBBHeat( Loop ).LowTemperature, 3 );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits( ZoneBBHeat( Loop ).CapatHighTemperature, 3 );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits( ZoneBBHeat( Loop ).HighTemperature, 3 );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits( ZoneBBHeat( Loop ).FractionRadiant, 3 );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			StringOut = RoundSigDigits( ZoneBBHeat( Loop ).FractionConvected, 3 );
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileInits, fmtA, flags ) << StringOut + ','; }
			gio::write( OutputFileInits, fmtA ) << ZoneBBHeat( Loop ).EndUseSubcategory;
		}

	}

	void
	InitInternalHeatGains()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   September 1997
		//       MODIFIED       November 1998, FW: add adjustment to elec lights for dayltg controls
		//                      August 2003, FCW: add optional calculation of light-to-return fraction
		//                       as a function of return plenum air temperature.
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine sets up the zone internal heat gains
		// that are independent of the zone air temperature.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace ScheduleManager;
		using DataHeatBalFanSys::MAT;
		using DataHeatBalFanSys::SumConvHTRadSys;
		using DataHeatBalFanSys::ZoneLatentGain;
		using namespace DataDaylighting;
		using DataZoneEquipment::ZoneEquipConfig;
		using ZonePlenum::ZoneRetPlenCond;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using DataRoomAirModel::IsZoneDV;
		using DataRoomAirModel::TCMF;
		using DataRoomAirModel::IsZoneUI;
		using WaterThermalTanks::CalcWaterThermalTankZoneGains;
		using WaterUse::CalcWaterUseZoneGains;
		using FuelCellElectricGenerator::FigureFuelCellZoneGains;
		using MicroCHPElectricGenerator::FigureMicroCHPZoneGains;
		using DaylightingDevices::FigureTDDZoneGains;
		using RefrigeratedCase::FigureRefrigerationZoneGains;
		using OutputReportTabular::radiantPulseUsed;
		using OutputReportTabular::radiantPulseTimestep;
		using OutputReportTabular::radiantPulseReceived;
		using DataGlobals::CompLoadReportIsReq;
		using OutputReportTabular::AllocateLoadComponentArrays;
		using DataSizing::CurOverallSimDay;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static Array1D< Real64 > const C( 9, { 6.4611027, 0.946892, 0.0000255737, 7.139322, -0.0627909, 0.0000589271, -0.198550, 0.000940018, -0.00000149532 } );
		static ZoneCatEUseData const zeroZoneCatEUse; // For initialization

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 ActivityLevel_WperPerson; // Units on Activity Level (Schedule)
		Real64 NumberOccupants; // Number of occupants
		int SurfNum; // DO loop counter for surfaces
		int Loop;
		int NZ;
		Real64 Q; // , QR
		Real64 TotalPeopleGain; // Total heat gain from people (intermediate calculational variable)
		Real64 SensiblePeopleGain; // Sensible heat gain from people (intermediate calculational variable)
		Real64 FractionConvected; // For general lighting, fraction of heat from lights convected to zone air
		Real64 FractionReturnAir; // For general lighting, fraction of heat from lights convected to zone's return air
		Real64 FractionRadiant; // For general lighting, fraction of heat from lights to zone that is long wave
		int ReturnZonePlenumCondNum; // Number of ZoneRetPlenCond for a zone's return air plenum, if it exists
		Real64 ReturnPlenumTemp; // Air temperature of a zone's return air plenum (C)
		Real64 pulseMultipler; // use to create a pulse for the load component report computations
		static Real64 curQL( 0.0 ); // radiant value prior to adjustment for pulse for load component report
		static Real64 adjQL( 0.0 ); // radiant value including adjustment for pulse for load component report

		//  REAL(r64), ALLOCATABLE, SAVE, DIMENSION(:) :: QSA

		//  IF (.NOT. ALLOCATED(QSA)) ALLOCATE(QSA(NumOfZones))

		//  Zero out time step variables
		for ( auto & e : ZoneIntGain ) {
			e.NOFOCC = 0.0;
			e.QOCTOT = 0.0;
			e.QOCSEN = 0.0;
			e.QOCLAT = 0.0;
			e.QOCRAD = 0.0;
			e.QOCCON = 0.0;
			e.QLTSW = 0.0;
			e.QLTCRA = 0.0;
			e.QLTRAD = 0.0;
			e.QLTCON = 0.0;
			e.QLTTOT = 0.0;

			e.QEELAT = 0.0;
			e.QEERAD = 0.0;
			e.QEECON = 0.0;
			e.QEELost = 0.0;
			e.QGELAT = 0.0;
			e.QGERAD = 0.0;
			e.QGECON = 0.0;
			e.QGELost = 0.0;
			e.QBBRAD = 0.0;
			e.QBBCON = 0.0;
			e.QOELAT = 0.0;
			e.QOERAD = 0.0;
			e.QOECON = 0.0;
			e.QOELost = 0.0;
			e.QHWLAT = 0.0;
			e.QHWRAD = 0.0;
			e.QHWCON = 0.0;
			e.QHWLost = 0.0;
			e.QSELAT = 0.0;
			e.QSERAD = 0.0;
			e.QSECON = 0.0;
			e.QSELost = 0.0;
		}

		ZoneIntEEuse = zeroZoneCatEUse; // Set all member arrays to zeros

		for ( auto & e : ZnRpt ) {
			e.LtsPower = 0.0;
			e.ElecPower = 0.0;
			e.GasPower = 0.0;
			e.HWPower = 0.0;
			e.SteamPower = 0.0;
			e.BaseHeatPower = 0.0;
		}

		//  QSA = 0.0

		// Process Internal Heat Gains, People done below
		// Occupant Stuff
		//   METHOD:
		//       The function is based on a curve fit to data presented in
		//       Table 48 'Heat Gain From People' of Chapter 1 of the 'Carrier
		//       Handbook of Air Conditioning System Design', 1965.  Values of
		//       Sensible gain were obtained from the table at average adjusted
		//       metabolic rates 350, 400, 450, 500, 750, 850, 1000, and
		//       1450 Btu/hr each at temperatures 82, 80, 78, 75, and 70F.
		//       Sensible gains of 0.0 at 96F and equal to the metabolic rate
		//       at 30F were assumed in order to give reasonable values beyond
		//       The reported temperature range.
		for ( Loop = 1; Loop <= TotPeople; ++Loop ) {
			NZ = People( Loop ).ZonePtr;
			NumberOccupants = People( Loop ).NumberOfPeople * GetCurrentScheduleValue( People( Loop ).NumberOfPeoplePtr );
			if ( People( Loop ).EMSPeopleOn ) NumberOccupants = People( Loop ).EMSNumberOfPeople;

			TotalPeopleGain = 0.0;
			SensiblePeopleGain = 0.0;

			if ( NumberOccupants > 0.0 ) {
				ActivityLevel_WperPerson = GetCurrentScheduleValue( People( Loop ).ActivityLevelPtr );
				TotalPeopleGain = NumberOccupants * ActivityLevel_WperPerson;
				// if the user did not specify a sensible fraction, calculate the sensible heat gain
				if ( People( Loop ).UserSpecSensFrac == AutoCalculate ) {
					if ( ! ( IsZoneDV( NZ ) || IsZoneUI( NZ ) ) ) {
						SensiblePeopleGain = NumberOccupants * ( C( 1 ) + ActivityLevel_WperPerson * ( C( 2 ) + ActivityLevel_WperPerson * C( 3 ) ) + MAT( NZ ) * ( ( C( 4 ) + ActivityLevel_WperPerson * ( C( 5 ) + ActivityLevel_WperPerson * C( 6 ) ) ) + MAT( NZ ) * ( C( 7 ) + ActivityLevel_WperPerson * ( C( 8 ) + ActivityLevel_WperPerson * C( 9 ) ) ) ) );
					} else { // UCSD - DV or UI
						SensiblePeopleGain = NumberOccupants * ( C( 1 ) + ActivityLevel_WperPerson * ( C( 2 ) + ActivityLevel_WperPerson * C( 3 ) ) + TCMF( NZ ) * ( ( C( 4 ) + ActivityLevel_WperPerson * ( C( 5 ) + ActivityLevel_WperPerson * C( 6 ) ) ) + TCMF( NZ ) * ( C( 7 ) + ActivityLevel_WperPerson * ( C( 8 ) + ActivityLevel_WperPerson * C( 9 ) ) ) ) );
					}
				} else { // if the user did specify a sensible fraction, use it
					SensiblePeopleGain = TotalPeopleGain * People( Loop ).UserSpecSensFrac;
				}

				if ( SensiblePeopleGain > TotalPeopleGain ) SensiblePeopleGain = TotalPeopleGain;
				if ( SensiblePeopleGain < 0.0 ) SensiblePeopleGain = 0.0;

				//For predefined tabular reports related to outside air ventilation
				ZonePreDefRep( NZ ).isOccupied = true; //set flag to occupied to be used in tabular reporting for ventilation
				ZonePreDefRep( NZ ).NumOccAccum += NumberOccupants * TimeStepZone;
				ZonePreDefRep( NZ ).NumOccAccumTime += TimeStepZone;
			} else {
				ZonePreDefRep( NZ ).isOccupied = false; //set flag to occupied to be used in tabular reporting for ventilation
			}

			People( Loop ).NumOcc = NumberOccupants;
			People( Loop ).RadGainRate = SensiblePeopleGain * People( Loop ).FractionRadiant;
			People( Loop ).ConGainRate = SensiblePeopleGain * People( Loop ).FractionConvected;
			People( Loop ).SenGainRate = SensiblePeopleGain;
			People( Loop ).LatGainRate = TotalPeopleGain - SensiblePeopleGain;
			People( Loop ).TotGainRate = TotalPeopleGain;
			People( Loop ).CO2GainRate = TotalPeopleGain * People( Loop ).CO2RateFactor;

			ZoneIntGain( NZ ).NOFOCC += People( Loop ).NumOcc;
			ZoneIntGain( NZ ).QOCRAD += People( Loop ).RadGainRate;
			ZoneIntGain( NZ ).QOCCON += People( Loop ).ConGainRate;
			ZoneIntGain( NZ ).QOCSEN += People( Loop ).SenGainRate;
			ZoneIntGain( NZ ).QOCLAT += People( Loop ).LatGainRate;
			ZoneIntGain( NZ ).QOCTOT += People( Loop ).TotGainRate;
		}

		for ( Loop = 1; Loop <= TotLights; ++Loop ) {
			NZ = Lights( Loop ).ZonePtr;
			Q = Lights( Loop ).DesignLevel * GetCurrentScheduleValue( Lights( Loop ).SchedPtr );

			if ( ZoneDaylight( NZ ).DaylightType == DetailedDaylighting || ZoneDaylight( NZ ).DaylightType == DElightDaylighting ) {

				if ( Lights( Loop ).FractionReplaceable > 0.0 ) { // FractionReplaceable can only be 0 or 1 for these models
					Q *= ZoneDaylight( NZ ).ZonePowerReductionFactor;
				}
			}

			// Reduce lighting power due to demand limiting
			if ( Lights( Loop ).ManageDemand && ( Q > Lights( Loop ).DemandLimit ) ) Q = Lights( Loop ).DemandLimit;

			// Set Q to EMS override if being called for by EMs
			if ( Lights( Loop ).EMSLightsOn ) Q = Lights( Loop ).EMSLightingPower;

			FractionConvected = Lights( Loop ).FractionConvected;
			FractionReturnAir = Lights( Loop ).FractionReturnAir;
			FractionRadiant = Lights( Loop ).FractionRadiant;
			if ( Lights( Loop ).FractionReturnAirIsCalculated && ! ZoneSizingCalc && SimTimeSteps > 1 ) {
				// Calculate FractionReturnAir based on conditions in the zone's return air plenum, if there is one.
				if ( Zone( NZ ).IsControlled ) {
					ReturnZonePlenumCondNum = ZoneEquipConfig( NZ ).ReturnZonePlenumCondNum;
					if ( ReturnZonePlenumCondNum > 0 ) {
						ReturnPlenumTemp = ZoneRetPlenCond( ReturnZonePlenumCondNum ).ZoneTemp;
						FractionReturnAir = Lights( Loop ).FractionReturnAirPlenTempCoeff1 - Lights( Loop ).FractionReturnAirPlenTempCoeff2 * ReturnPlenumTemp;
						FractionReturnAir = max( 0.0, min( 1.0, FractionReturnAir ) );
						if ( FractionReturnAir >= ( 1.0 - Lights( Loop ).FractionShortWave ) ) {
							FractionReturnAir = 1.0 - Lights( Loop ).FractionShortWave;
							FractionRadiant = 0.0;
							FractionConvected = 0.0;
						} else {
							FractionRadiant = ( ( 1.0 - FractionReturnAir - Lights( Loop ).FractionShortWave ) / ( Lights( Loop ).FractionRadiant + Lights( Loop ).FractionConvected ) ) * Lights( Loop ).FractionRadiant;
							FractionConvected = 1.0 - ( FractionReturnAir + FractionRadiant + Lights( Loop ).FractionShortWave );
						}
					}
				}
			}

			Lights( Loop ).Power = Q;
			Lights( Loop ).RadGainRate = Q * FractionRadiant;
			Lights( Loop ).VisGainRate = Q * Lights( Loop ).FractionShortWave;
			Lights( Loop ).ConGainRate = Q * FractionConvected;
			Lights( Loop ).RetAirGainRate = Q * FractionReturnAir;
			Lights( Loop ).TotGainRate = Q;

			ZnRpt( NZ ).LtsPower += Lights( Loop ).Power;
			ZoneIntGain( NZ ).QLTRAD += Lights( Loop ).RadGainRate;
			ZoneIntGain( NZ ).QLTSW += Lights( Loop ).VisGainRate;
			ZoneIntGain( NZ ).QLTCON += Lights( Loop ).ConGainRate;
			ZoneIntGain( NZ ).QLTCRA += Lights( Loop ).RetAirGainRate;
			ZoneIntGain( NZ ).QLTTOT += Lights( Loop ).TotGainRate;
		}

		for ( Loop = 1; Loop <= TotElecEquip; ++Loop ) {
			Q = ZoneElectric( Loop ).DesignLevel * GetCurrentScheduleValue( ZoneElectric( Loop ).SchedPtr );

			// Reduce equipment power due to demand limiting
			if ( ZoneElectric( Loop ).ManageDemand && ( Q > ZoneElectric( Loop ).DemandLimit ) ) Q = ZoneElectric( Loop ).DemandLimit;

			// Set Q to EMS override if being called for by EMs
			if ( ZoneElectric( Loop ).EMSZoneEquipOverrideOn ) Q = ZoneElectric( Loop ).EMSEquipPower;

			ZoneElectric( Loop ).Power = Q;
			ZoneElectric( Loop ).RadGainRate = Q * ZoneElectric( Loop ).FractionRadiant;
			ZoneElectric( Loop ).ConGainRate = Q * ZoneElectric( Loop ).FractionConvected;
			ZoneElectric( Loop ).LatGainRate = Q * ZoneElectric( Loop ).FractionLatent;
			ZoneElectric( Loop ).LostRate = Q * ZoneElectric( Loop ).FractionLost;
			ZoneElectric( Loop ).TotGainRate = Q - ZoneElectric( Loop ).LostRate;

			NZ = ZoneElectric( Loop ).ZonePtr;
			ZnRpt( NZ ).ElecPower += ZoneElectric( Loop ).Power;
			ZoneIntGain( NZ ).QEERAD += ZoneElectric( Loop ).RadGainRate;
			ZoneIntGain( NZ ).QEECON += ZoneElectric( Loop ).ConGainRate;
			ZoneIntGain( NZ ).QEELAT += ZoneElectric( Loop ).LatGainRate;
			ZoneIntGain( NZ ).QEELost += ZoneElectric( Loop ).LostRate;
		}

		for ( Loop = 1; Loop <= TotGasEquip; ++Loop ) {
			Q = ZoneGas( Loop ).DesignLevel * GetCurrentScheduleValue( ZoneGas( Loop ).SchedPtr );

			// Set Q to EMS override if being called for by EMs
			if ( ZoneGas( Loop ).EMSZoneEquipOverrideOn ) Q = ZoneGas( Loop ).EMSEquipPower;

			ZoneGas( Loop ).Power = Q;
			ZoneGas( Loop ).RadGainRate = Q * ZoneGas( Loop ).FractionRadiant;
			ZoneGas( Loop ).ConGainRate = Q * ZoneGas( Loop ).FractionConvected;
			ZoneGas( Loop ).LatGainRate = Q * ZoneGas( Loop ).FractionLatent;
			ZoneGas( Loop ).LostRate = Q * ZoneGas( Loop ).FractionLost;
			ZoneGas( Loop ).TotGainRate = Q - ZoneGas( Loop ).LostRate;
			ZoneGas( Loop ).CO2GainRate = Q * ZoneGas( Loop ).CO2RateFactor;

			NZ = ZoneGas( Loop ).ZonePtr;
			ZnRpt( NZ ).GasPower += ZoneGas( Loop ).Power;
			ZoneIntGain( NZ ).QGERAD += ZoneGas( Loop ).RadGainRate;
			ZoneIntGain( NZ ).QGECON += ZoneGas( Loop ).ConGainRate;
			ZoneIntGain( NZ ).QGELAT += ZoneGas( Loop ).LatGainRate;
			ZoneIntGain( NZ ).QGELost += ZoneGas( Loop ).LostRate;
		}

		for ( Loop = 1; Loop <= TotOthEquip; ++Loop ) {
			Q = ZoneOtherEq( Loop ).DesignLevel * GetCurrentScheduleValue( ZoneOtherEq( Loop ).SchedPtr );

			// Set Q to EMS override if being called for by EMs
			if ( ZoneOtherEq( Loop ).EMSZoneEquipOverrideOn ) Q = ZoneOtherEq( Loop ).EMSEquipPower;

			ZoneOtherEq( Loop ).Power = Q;
			ZoneOtherEq( Loop ).RadGainRate = Q * ZoneOtherEq( Loop ).FractionRadiant;
			ZoneOtherEq( Loop ).ConGainRate = Q * ZoneOtherEq( Loop ).FractionConvected;
			ZoneOtherEq( Loop ).LatGainRate = Q * ZoneOtherEq( Loop ).FractionLatent;
			ZoneOtherEq( Loop ).LostRate = Q * ZoneOtherEq( Loop ).FractionLost;
			ZoneOtherEq( Loop ).TotGainRate = Q - ZoneOtherEq( Loop ).LostRate;

			NZ = ZoneOtherEq( Loop ).ZonePtr;
			ZoneIntGain( NZ ).QOERAD += ZoneOtherEq( Loop ).RadGainRate;
			ZoneIntGain( NZ ).QOECON += ZoneOtherEq( Loop ).ConGainRate;
			ZoneIntGain( NZ ).QOELAT += ZoneOtherEq( Loop ).LatGainRate;
			ZoneIntGain( NZ ).QOELost += ZoneOtherEq( Loop ).LostRate;
		}

		for ( Loop = 1; Loop <= TotHWEquip; ++Loop ) {
			Q = ZoneHWEq( Loop ).DesignLevel * GetCurrentScheduleValue( ZoneHWEq( Loop ).SchedPtr );

			// Set Q to EMS override if being called for by EMs
			if ( ZoneHWEq( Loop ).EMSZoneEquipOverrideOn ) Q = ZoneHWEq( Loop ).EMSEquipPower;

			ZoneHWEq( Loop ).Power = Q;
			ZoneHWEq( Loop ).RadGainRate = Q * ZoneHWEq( Loop ).FractionRadiant;
			ZoneHWEq( Loop ).ConGainRate = Q * ZoneHWEq( Loop ).FractionConvected;
			ZoneHWEq( Loop ).LatGainRate = Q * ZoneHWEq( Loop ).FractionLatent;
			ZoneHWEq( Loop ).LostRate = Q * ZoneHWEq( Loop ).FractionLost;
			ZoneHWEq( Loop ).TotGainRate = Q - ZoneHWEq( Loop ).LostRate;

			NZ = ZoneHWEq( Loop ).ZonePtr;
			ZnRpt( NZ ).HWPower += ZoneHWEq( Loop ).Power;
			ZoneIntGain( NZ ).QHWRAD += ZoneHWEq( Loop ).RadGainRate;
			ZoneIntGain( NZ ).QHWCON += ZoneHWEq( Loop ).ConGainRate;
			ZoneIntGain( NZ ).QHWLAT += ZoneHWEq( Loop ).LatGainRate;
			ZoneIntGain( NZ ).QHWLost += ZoneHWEq( Loop ).LostRate;
		}

		for ( Loop = 1; Loop <= TotStmEquip; ++Loop ) {
			Q = ZoneSteamEq( Loop ).DesignLevel * GetCurrentScheduleValue( ZoneSteamEq( Loop ).SchedPtr );

			// Set Q to EMS override if being called for by EMs
			if ( ZoneSteamEq( Loop ).EMSZoneEquipOverrideOn ) Q = ZoneSteamEq( Loop ).EMSEquipPower;

			ZoneSteamEq( Loop ).Power = Q;
			ZoneSteamEq( Loop ).RadGainRate = Q * ZoneSteamEq( Loop ).FractionRadiant;
			ZoneSteamEq( Loop ).ConGainRate = Q * ZoneSteamEq( Loop ).FractionConvected;
			ZoneSteamEq( Loop ).LatGainRate = Q * ZoneSteamEq( Loop ).FractionLatent;
			ZoneSteamEq( Loop ).LostRate = Q * ZoneSteamEq( Loop ).FractionLost;
			ZoneSteamEq( Loop ).TotGainRate = Q - ZoneSteamEq( Loop ).LostRate;

			NZ = ZoneSteamEq( Loop ).ZonePtr;
			ZnRpt( NZ ).SteamPower += ZoneSteamEq( Loop ).Power;
			ZoneIntGain( NZ ).QSERAD += ZoneSteamEq( Loop ).RadGainRate;
			ZoneIntGain( NZ ).QSECON += ZoneSteamEq( Loop ).ConGainRate;
			ZoneIntGain( NZ ).QSELAT += ZoneSteamEq( Loop ).LatGainRate;
			ZoneIntGain( NZ ).QSELost += ZoneSteamEq( Loop ).LostRate;
		}

		for ( Loop = 1; Loop <= TotBBHeat; ++Loop ) {
			NZ = ZoneBBHeat( Loop ).ZonePtr;
			if ( Zone( NZ ).OutDryBulbTemp >= ZoneBBHeat( Loop ).HighTemperature ) {
				Q = 0.0;
			} else if ( Zone( NZ ).OutDryBulbTemp > ZoneBBHeat( Loop ).LowTemperature ) {
				Q = ( Zone( NZ ).OutDryBulbTemp - ZoneBBHeat( Loop ).LowTemperature ) * ( ZoneBBHeat( Loop ).CapatHighTemperature - ZoneBBHeat( Loop ).CapatLowTemperature ) / ( ZoneBBHeat( Loop ).HighTemperature - ZoneBBHeat( Loop ).LowTemperature ) + ZoneBBHeat( Loop ).CapatLowTemperature;
			} else {
				Q = ZoneBBHeat( Loop ).CapatLowTemperature;
			}
			Q *= GetCurrentScheduleValue( ZoneBBHeat( Loop ).SchedPtr );

			// set with EMS value if being called for.
			if ( ZoneBBHeat( Loop ).EMSZoneBaseboardOverrideOn ) Q = ZoneBBHeat( Loop ).EMSZoneBaseboardPower;

			ZoneBBHeat( Loop ).Power = Q;
			ZoneBBHeat( Loop ).RadGainRate = Q * ZoneBBHeat( Loop ).FractionRadiant;
			ZoneBBHeat( Loop ).ConGainRate = Q * ZoneBBHeat( Loop ).FractionConvected;
			ZoneBBHeat( Loop ).TotGainRate = Q;

			NZ = ZoneBBHeat( Loop ).ZonePtr;
			ZnRpt( NZ ).BaseHeatPower += ZoneBBHeat( Loop ).Power;
			ZoneIntGain( NZ ).QBBRAD += ZoneBBHeat( Loop ).RadGainRate;
			ZoneIntGain( NZ ).QBBCON += ZoneBBHeat( Loop ).ConGainRate;
		}

		for ( Loop = 1; Loop <= TotCO2Gen; ++Loop ) {
			NZ = ZoneCO2Gen( Loop ).ZonePtr;
			ZoneCO2Gen( Loop ).CO2GainRate = ZoneCO2Gen( Loop ).CO2DesignRate * GetCurrentScheduleValue( ZoneCO2Gen( Loop ).SchedPtr );
			ZnRpt( NZ ).CO2Rate += ZoneCO2Gen( Loop ).CO2GainRate;
		}

		if ( NumZoneITEqStatements > 0 ) CalcZoneITEq();

		CalcWaterThermalTankZoneGains();
		PipeHeatTransfer::PipeHTData::CalcZonePipesHeatGain();
		CalcWaterUseZoneGains();
		FigureFuelCellZoneGains();
		FigureMicroCHPZoneGains();
		initializeElectricPowerServiceZoneGains();
		FigureTDDZoneGains();
		FigureRefrigerationZoneGains();

		// store pointer values to hold generic internal gain values constant for entire timestep
		UpdateInternalGainValues();

		for ( NZ = 1; NZ <= NumOfZones; ++NZ ) {
			SumAllInternalRadiationGains( NZ, QL( NZ ) );

			SumAllInternalLatentGains( NZ, ZoneLatentGain( NZ ) );

		}

		SumConvHTRadSys = 0.0;

		pulseMultipler = 0.01; // the W/sqft pulse for the zone
		if ( CompLoadReportIsReq ) {
			AllocateLoadComponentArrays();
		}
		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
			NZ = Surface( SurfNum ).Zone;
			if ( ! Surface( SurfNum ).HeatTransSurf || NZ == 0 ) continue; // Skip non-heat transfer surfaces
			if ( ! doLoadComponentPulseNow ) {
				QRadThermInAbs( SurfNum ) = QL( NZ ) * TMULT( NZ ) * ITABSF( SurfNum );
			} else {
				curQL = QL( NZ );
				// for the loads component report during the special sizing run increase the radiant portion
				// a small amount to create a "pulse" of heat that is used for the
				adjQL = curQL + Zone( NZ ).FloorArea * pulseMultipler;
				// ITABSF is the Inside Thermal Absorptance
				// TMULT is a mulipliter for each zone
				// QRadThermInAbs is the thermal radiation absorbed on inside surfaces
				QRadThermInAbs( SurfNum ) = adjQL * TMULT( NZ ) * ITABSF( SurfNum );
				// store the magnitude and time of the pulse
				radiantPulseUsed( CurOverallSimDay, NZ ) = adjQL - curQL;
				radiantPulseTimestep( CurOverallSimDay, NZ ) = ( HourOfDay - 1 ) * NumOfTimeStepInHour + TimeStep;
				radiantPulseReceived( CurOverallSimDay, SurfNum ) = ( adjQL - curQL ) * TMULT( NZ ) * ITABSF( SurfNum ) * Surface( SurfNum ).Area;
			}
		}

	}

	void
	CalcZoneITEq()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         M.J. Witte
		//       DATE WRITTEN   October 2014
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine calculates the gains and other results for ElectricEquipment:ITE:AirCooled.
		// This broken into a separate subroutine, because the calculations are more detailed than the other
		// types of internal gains.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using ScheduleManager::GetCurrentScheduleValue;
		using DataHeatBalFanSys::MAT;
		using DataHeatBalFanSys::ZoneAirHumRat;
		using DataZoneEquipment::ZoneEquipConfig;
		using namespace Psychrometrics;
		using DataRoomAirModel::IsZoneDV;
		using DataRoomAirModel::TCMF;
		using DataRoomAirModel::IsZoneUI;
		using DataLoopNode::Node;
		using CurveManager::CurveValue;
		using DataHVACGlobals::SmallAirVolFlow;
		using DataHVACGlobals::SmallTempDiff;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// Operating Limits for environmental class: None, A1, A2, A3, A4, B, C
		// From ASHRAE 2011 Thermal Guidelines environmental classes for Air-Cooled ITE
		static Array1D< Real64 > const DBMin( 7, { -99.0, 15.0, 10.0, 5.0, 5.0, 5.0, 5.0 } ); // Minimum dry-bulb temperature [C]
		static Array1D< Real64 > const DBMax( 7, { 99.0, 32.0, 35.0, 40.0, 45.0, 35.0, 40.0 } ); // Maximum dry-bulb temperature [C]
		static Array1D< Real64 > const DPMax( 7, { 99.0, 17.0, 21.0, 24.0, 24.0, 28.0, 28.0 } ); // Maximum dewpoint temperature [C]
		static Array1D< Real64 > const DPMin( 7, { -99.0, -99.0, -99.0, -12.0, -12.0, -99.0, -99.0 } ); // Minimum dewpoint temperature [C]
		static Array1D< Real64 > const RHMin( 7, { 0.0, 20.0, 20.0, 8.0, 8.0, 8.0, 8.0 } ); // Minimum relative humidity [%]
		static Array1D< Real64 > const RHMax( 7, { 99.0, 80.0, 80.0, 85.0, 90.0, 80.0, 80.0 } );  // Minimum relative humidity [%]

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static std::string const RoutineName( "CalcZoneITEq" );
		int Loop;
		int NZ;
		int SupplyNodeNum;			// Supply air node number (if zero, then not specified)
		Real64 OperSchedFrac;		// Operating schedule fraction
		Real64 CPULoadSchedFrac;	// CPU loading schedule fraction
		Real64 AirConnection;		// Air connection type
		Real64 TSupply;				// Supply air temperature [C]
		Real64 WSupply;				// Supply air humidity ratio [kgH2O/kgdryair]
		Real64 RecircFrac;			// Recirulation fraction - current
		Real64 TRecirc;				// Recirulation air temperature [C]
		Real64 WRecirc;				// Recirulation air humidity ratio [kgH2O/kgdryair]
		Real64 TAirIn;				// Entering air dry-bulb temperature [C]
		Real64 TAirInDesign;		// Design entering air dry-bulb temperature [C]
		Real64 WAirIn;				// Entering air humidity ratio [kgH2O/kgdryair]
		Real64 TDPAirIn;			// Entering air dewpoint temperature [C]
		Real64 RHAirIn;				// Entering air relative humidity [%]
		Real64 SupplyHeatIndex;		// Supply heat index
		Real64 TAirOut;				// Leaving air temperature [C]
		Real64 AirVolFlowFrac;		// Air volume flow fraction
		Real64 AirVolFlowFracDesignT; // Air volume flow fraction at design entering air temperature
		Real64 AirVolFlowRate;		// Air volume flow rate at current density [m3/s]
		Real64 AirMassFlowRate;		// Air mass flow rate [kg/s]
		Real64 CPUPower;			// CPU power input [W]
		Real64 FanPower;			// Fan power input [W]
		Real64 UPSPower;			// UPS new power input (losses) [W]
		Real64 UPSPartLoadRatio;	// UPS part load ratio (current total power input / design total power input)
		Real64 UPSHeatGain;			// UPS convective heat gain to zone [W]
		int EnvClass;				// Index for environmental class (None=0, A1=1, A2=2, A3=3, A4=4, B=5, C=6)
		Array1D< Real64 > ZoneSumTinMinusTSup( NumOfZones ); // Numerator for zone-level sensible heat index (SHI)
		Array1D< Real64 > ZoneSumToutMinusTSup( NumOfZones ); // Denominator for zone-level sensible heat index (SHI)


		//  Zero out time step variables
		// Object report variables
		for ( Loop = 1; Loop <= NumZoneITEqStatements; ++Loop ) {
			ZoneITEq( Loop ).CPUPower = 0.0;
			ZoneITEq( Loop ).FanPower = 0.0;
			ZoneITEq( Loop ).UPSPower = 0.0;
			ZoneITEq( Loop ).CPUPowerAtDesign = 0.0;
			ZoneITEq( Loop ).FanPowerAtDesign = 0.0;
			ZoneITEq( Loop ).UPSGainRateToZone = 0.0;
			ZoneITEq( Loop ).ConGainRateToZone = 0.0;

			ZoneITEq( Loop ).CPUConsumption = 0.0;
			ZoneITEq( Loop ).FanConsumption = 0.0;
			ZoneITEq( Loop ).UPSConsumption = 0.0;
			ZoneITEq( Loop ).CPUEnergyAtDesign = 0.0;
			ZoneITEq( Loop ).FanEnergyAtDesign = 0.0;
			ZoneITEq( Loop ).UPSGainEnergyToZone = 0.0;
			ZoneITEq( Loop ).ConGainEnergyToZone = 0.0;

			ZoneITEq( Loop ).AirVolFlowStdDensity = 0.0;
			ZoneITEq( Loop ).AirVolFlowCurDensity = 0.0;
			ZoneITEq( Loop ).AirMassFlow = 0.0;
			ZoneITEq( Loop ).AirInletDryBulbT = 0.0;
			ZoneITEq( Loop ).AirInletDewpointT = 0.0;
			ZoneITEq( Loop ).AirInletRelHum = 0.0;
			ZoneITEq( Loop ).AirOutletDryBulbT = 0.0;
			ZoneITEq( Loop ).SHI = 0.0;
			ZoneITEq( Loop ).TimeOutOfOperRange = 0.0;
			ZoneITEq( Loop ).TimeAboveDryBulbT = 0.0;
			ZoneITEq( Loop ).TimeBelowDryBulbT = 0.0;
			ZoneITEq( Loop ).TimeAboveDewpointT = 0.0;
			ZoneITEq( Loop ).TimeBelowDewpointT = 0.0;
			ZoneITEq( Loop ).TimeAboveRH = 0.0;
			ZoneITEq( Loop ).TimeBelowRH = 0.0;
			ZoneITEq( Loop ).DryBulbTAboveDeltaT = 0.0;
			ZoneITEq( Loop ).DryBulbTBelowDeltaT = 0.0;
			ZoneITEq( Loop ).DewpointTAboveDeltaT = 0.0;
			ZoneITEq( Loop ).DewpointTBelowDeltaT = 0.0;
			ZoneITEq( Loop ).RHAboveDeltaRH = 0.0;
			ZoneITEq( Loop ).RHBelowDeltaRH = 0.0;
		} // ZoneITEq init loop

		// Zone total report variables
		for ( Loop = 1; Loop <= NumOfZones; ++Loop ) {
			ZnRpt( Loop ).ITEqCPUPower = 0.0;
			ZnRpt( Loop ).ITEqFanPower = 0.0;
			ZnRpt( Loop ).ITEqUPSPower = 0.0;
			ZnRpt( Loop ).ITEqCPUPowerAtDesign = 0.0;
			ZnRpt( Loop ).ITEqFanPowerAtDesign = 0.0;
			ZnRpt( Loop ).ITEqUPSGainRateToZone = 0.0;
			ZnRpt( Loop ).ITEqConGainRateToZone = 0.0;

			ZnRpt( Loop ).ITEqCPUConsumption = 0.0;
			ZnRpt( Loop ).ITEqFanConsumption = 0.0;
			ZnRpt( Loop ).ITEqUPSConsumption = 0.0;
			ZnRpt( Loop ).ITEqCPUEnergyAtDesign = 0.0;
			ZnRpt( Loop ).ITEqFanEnergyAtDesign = 0.0;
			ZnRpt( Loop ).ITEqUPSGainEnergyToZone = 0.0;
			ZnRpt( Loop ).ITEqConGainEnergyToZone = 0.0;

			ZnRpt( Loop ).ITEqAirVolFlowStdDensity = 0.0;
			ZnRpt( Loop ).ITEqAirMassFlow = 0.0;
			ZnRpt( Loop ).ITEqSHI = 0.0;
			ZnRpt( Loop ).ITEqTimeOutOfOperRange = 0.0;
			ZnRpt( Loop ).ITEqTimeAboveDryBulbT = 0.0;
			ZnRpt( Loop ).ITEqTimeBelowDryBulbT = 0.0;
			ZnRpt( Loop ).ITEqTimeAboveDewpointT = 0.0;
			ZnRpt( Loop ).ITEqTimeBelowDewpointT = 0.0;
			ZnRpt( Loop ).ITEqTimeAboveRH = 0.0;
			ZnRpt( Loop ).ITEqTimeBelowRH = 0.0;

			ZoneSumTinMinusTSup( Loop ) = 0.0;
			ZoneSumToutMinusTSup( Loop ) = 0.0;
		} // Zone init loop

		for ( Loop = 1; Loop <= NumZoneITEqStatements; ++Loop ) {
			// Get schedules
			NZ = ZoneITEq( Loop ).ZonePtr;
			OperSchedFrac = GetCurrentScheduleValue( ZoneITEq( Loop ).OperSchedPtr );
			CPULoadSchedFrac = GetCurrentScheduleValue( ZoneITEq( Loop ).CPULoadSchedPtr );

			// Determine inlet air temperature and humidity
			AirConnection = ZoneITEq( Loop ).AirConnectionType;
			RecircFrac = 0.0;
			SupplyNodeNum = ZoneITEq( Loop ).SupplyAirNodeNum;
			if ( AirConnection == ITEInletAdjustedSupply ) {
				if ( SupplyNodeNum != 0 ) {
					TSupply = Node( SupplyNodeNum ).Temp;
					WSupply = Node( SupplyNodeNum ).HumRat;
				} else {
					ShowFatalError( RoutineName + ": ElectricEquipment:ITE:AirCooled " + ZoneITEq( Loop ).Name );
					ShowContinueError( "Air Inlet Connection Type = AdjustedSupply but no Supply Air Node is specified." );
				}
				if ( ZoneITEq( Loop ).RecircFLTCurve != 0 ) {
					RecircFrac = ZoneITEq( Loop ).DesignRecircFrac * CurveValue( ZoneITEq( Loop ).RecircFLTCurve, CPULoadSchedFrac, TSupply );
				} else {
					RecircFrac = ZoneITEq( Loop ).DesignRecircFrac;
				}
				TRecirc = MAT( NZ );
				WRecirc = ZoneAirHumRat( NZ );
				TAirIn = TRecirc * RecircFrac + TSupply * ( 1.0 - RecircFrac );
				WAirIn = WRecirc * RecircFrac + WSupply * ( 1.0 - RecircFrac );
			} else if ( AirConnection == ITEInletRoomAirModel ) {
				// Room air model option not implemented yet
				TAirIn = MAT( NZ );
				WAirIn = ZoneAirHumRat( NZ );
			} else { // Default to ITEInletZoneAirNode
				TAirIn = MAT( NZ );
				WAirIn = ZoneAirHumRat( NZ );
			}
			TDPAirIn = PsyTdpFnWPb( WAirIn, StdBaroPress, RoutineName );
			RHAirIn = PsyRhFnTdbWPb( TAirIn, WAirIn, StdBaroPress, RoutineName );

			// Calculate power input and airflow
			TAirInDesign = ZoneITEq( Loop ).DesignTAirIn;

			CPUPower = max( ZoneITEq( Loop ).DesignCPUPower * OperSchedFrac * CurveValue( ZoneITEq( Loop ).CPUPowerFLTCurve, CPULoadSchedFrac, TAirIn ) , 0.0 );
			ZoneITEq( Loop ).CPUPowerAtDesign = max( ZoneITEq( Loop ).DesignCPUPower * OperSchedFrac * CurveValue( ZoneITEq( Loop ).CPUPowerFLTCurve, CPULoadSchedFrac, TAirInDesign ) , 0.0 );

			AirVolFlowFrac = max( CurveValue( ZoneITEq( Loop ).AirFlowFLTCurve, CPULoadSchedFrac, TAirIn ) , 0.0 );
			AirVolFlowRate = ZoneITEq( Loop ).DesignAirVolFlowRate * OperSchedFrac * AirVolFlowFrac;
			if ( AirVolFlowRate < SmallAirVolFlow ) {
				AirVolFlowRate = 0.0;
			}
			AirVolFlowFracDesignT = max( CurveValue( ZoneITEq( Loop ).AirFlowFLTCurve, CPULoadSchedFrac, TAirInDesign ) , 0.0 );

			FanPower = max( ZoneITEq( Loop ).DesignFanPower * OperSchedFrac * CurveValue( ZoneITEq( Loop ).FanPowerFFCurve, AirVolFlowFrac ) , 0.0 );
			ZoneITEq( Loop ).FanPowerAtDesign = max( ZoneITEq( Loop ).DesignFanPower * OperSchedFrac * CurveValue( ZoneITEq( Loop ).FanPowerFFCurve, AirVolFlowFracDesignT ) , 0.0);

			//Calcaulate UPS net power input (power in less power to ITEquip) and UPS heat gain to zone
			if ( ZoneITEq( Loop ).DesignTotalPower > 0.0 ) {
				UPSPartLoadRatio = ( CPUPower + FanPower ) / ZoneITEq( Loop ).DesignTotalPower;
			} else {
				UPSPartLoadRatio = 0.0;
			}
			UPSPower = ( CPUPower + FanPower ) * max(( 1.0 - ZoneITEq( Loop ).DesignUPSEfficiency * CurveValue( ZoneITEq( Loop ).UPSEfficFPLRCurve, UPSPartLoadRatio ) ) , 0.0 );
			UPSHeatGain = UPSPower * ZoneITEq( Loop ).UPSLossToZoneFrac;

			// Calculate air outlet conditions and convective heat gain to zone

			AirMassFlowRate = AirVolFlowRate * PsyRhoAirFnPbTdbW( StdBaroPress, TAirIn, WAirIn, RoutineName );
			if ( AirMassFlowRate > 0.0 ) {
				TAirOut = TAirIn + ( CPUPower + FanPower ) / AirMassFlowRate / PsyCpAirFnWTdb( WAirIn, TAirIn );
			} else {
				TAirOut = TAirIn;
			}

			if ( abs( TAirOut - TSupply ) < SmallTempDiff ) {
				TAirOut = TSupply;
			}

			if ( ( SupplyNodeNum != 0 ) && ( TAirOut != TSupply ) ) {
				SupplyHeatIndex = ( TAirIn - TSupply ) / ( TAirOut - TSupply );
			} else {
				SupplyHeatIndex = 0.0;
			}

			if ( AirConnection == ITEInletAdjustedSupply || AirConnection == ITEInletZoneAirNode ) {
				// If not a room air model, then all ITEquip power input is a convective heat gain to the zone heat balance, plus UPS heat gain
				ZoneITEq( Loop ).ConGainRateToZone = CPUPower + FanPower + UPSHeatGain;
			} else if ( AirConnection == ITEInletRoomAirModel ) {
				// Room air model option not implemented yet - set room air model outlet node conditions here
				// If a room air model, then the only convective heat gain to the zone heat balance is the UPS heat gain
				ZoneITEq( Loop ).ConGainRateToZone = UPSHeatGain;
			}

			// Object report variables
			ZoneITEq( Loop ).CPUPower = CPUPower;
			ZoneITEq( Loop ).FanPower = FanPower;
			ZoneITEq( Loop ).UPSPower = UPSPower;
			// ZoneITEq( Loop ).CPUPowerAtDesign = set above
			// ZoneITEq( Loop ).FanPowerAtDesign = set above
			ZoneITEq( Loop ).UPSGainRateToZone = UPSHeatGain;
			// ZoneITEq( Loop ).ConGainRateToZone = set above

			ZnRpt( NZ ).ITEqCPUPower += ZoneITEq( Loop ).CPUPower;
			ZnRpt( NZ ).ITEqFanPower += ZoneITEq( Loop ).FanPower;
			ZnRpt( NZ ).ITEqUPSPower += ZoneITEq( Loop ).UPSPower;
			ZnRpt( NZ ).ITEqCPUPowerAtDesign += ZoneITEq( Loop ).CPUPowerAtDesign;
			ZnRpt( NZ ).ITEqFanPowerAtDesign += ZoneITEq( Loop ).FanPowerAtDesign;
			ZnRpt( NZ ).ITEqUPSGainRateToZone += ZoneITEq( Loop ).UPSGainRateToZone;
			ZnRpt( NZ ).ITEqConGainRateToZone += ZoneITEq( Loop ).ConGainRateToZone;

			ZoneITEq( Loop ).CPUConsumption = CPUPower * TimeStepZoneSec;
			ZoneITEq( Loop ).FanConsumption = FanPower * TimeStepZoneSec;
			ZoneITEq( Loop ).UPSConsumption = UPSPower * TimeStepZoneSec;
			ZoneITEq( Loop ).CPUEnergyAtDesign = ZoneITEq( Loop ).CPUPowerAtDesign * TimeStepZoneSec;
			ZoneITEq( Loop ).FanEnergyAtDesign = ZoneITEq( Loop ).FanPowerAtDesign * TimeStepZoneSec;
			ZoneITEq( Loop ).UPSGainEnergyToZone = UPSHeatGain * TimeStepZoneSec;
			ZoneITEq( Loop ).ConGainEnergyToZone = ZoneITEq( Loop ).ConGainRateToZone * TimeStepZoneSec;

			ZnRpt( NZ ).ITEqCPUConsumption += ZoneITEq( Loop ).CPUConsumption;
			ZnRpt( NZ ).ITEqFanConsumption += ZoneITEq( Loop ).FanConsumption;
			ZnRpt( NZ ).ITEqUPSConsumption += ZoneITEq( Loop ).UPSConsumption;
			ZnRpt( NZ ).ITEqCPUEnergyAtDesign += ZoneITEq( Loop ).CPUEnergyAtDesign;
			ZnRpt( NZ ).ITEqFanEnergyAtDesign += ZoneITEq( Loop ).FanEnergyAtDesign;
			ZnRpt( NZ ).ITEqUPSGainEnergyToZone += ZoneITEq( Loop ).UPSGainEnergyToZone;
			ZnRpt( NZ ).ITEqConGainEnergyToZone += ZoneITEq( Loop ).ConGainEnergyToZone;

			ZoneITEq( Loop ).AirVolFlowStdDensity = AirMassFlowRate * StdRhoAir;
			ZoneITEq( Loop ).AirVolFlowCurDensity = AirVolFlowRate;
			ZoneITEq( Loop ).AirMassFlow = AirMassFlowRate;
			ZoneITEq( Loop ).AirInletDryBulbT = TAirIn;
			ZoneITEq( Loop ).AirInletDewpointT = TDPAirIn;
			ZoneITEq( Loop ).AirInletRelHum = RHAirIn;
			ZoneITEq( Loop ).AirOutletDryBulbT = TAirOut;
			ZoneITEq( Loop ).SHI = SupplyHeatIndex;

			ZnRpt( NZ ).ITEqAirVolFlowStdDensity += ZoneITEq( Loop ).AirVolFlowStdDensity;
			ZnRpt( NZ ).ITEqAirMassFlow += ZoneITEq( Loop ).AirMassFlow;
			ZoneSumTinMinusTSup( NZ ) += ( TAirIn - TSupply ) * AirVolFlowRate;
			ZoneSumToutMinusTSup( NZ ) += ( TAirOut - TSupply ) * AirVolFlowRate;

			// Check environmental class operating range limits (defined as parameters in this subroutine)
			EnvClass = ZoneITEq( Loop ).Class;
			if ( EnvClass > 0 ) {
				if ( TAirIn > DBMax( EnvClass )) {
					ZoneITEq( Loop ).TimeAboveDryBulbT = TimeStepZone;
					ZoneITEq( Loop ).TimeOutOfOperRange = TimeStepZone;
					ZoneITEq( Loop ).DryBulbTAboveDeltaT = TAirIn - DBMax( EnvClass );
					ZnRpt( NZ ).ITEqTimeAboveDryBulbT = TimeStepZone;
					ZnRpt( NZ ).ITEqTimeOutOfOperRange = TimeStepZone;
				}
				if ( TAirIn < DBMin( EnvClass ) ) {
					ZoneITEq( Loop ).TimeBelowDryBulbT = TimeStepZone;
					ZoneITEq( Loop ).TimeOutOfOperRange = TimeStepZone;
					ZoneITEq( Loop ).DryBulbTBelowDeltaT = TAirIn - DBMin( EnvClass );
					ZnRpt( NZ ).ITEqTimeBelowDryBulbT = TimeStepZone;
					ZnRpt( NZ ).ITEqTimeOutOfOperRange = TimeStepZone;
				}
				if ( TDPAirIn > DPMax( EnvClass ) ) {
					ZoneITEq( Loop ).TimeAboveDewpointT = TimeStepZone;
					ZoneITEq( Loop ).TimeOutOfOperRange = TimeStepZone;
					ZoneITEq( Loop ).DewpointTAboveDeltaT = TDPAirIn - DPMax( EnvClass );
					ZnRpt( NZ ).ITEqTimeAboveDewpointT = TimeStepZone;
					ZnRpt( NZ ).ITEqTimeOutOfOperRange = TimeStepZone;
				}
				if ( TDPAirIn < DPMin( EnvClass ) ) {
					ZoneITEq( Loop ).TimeBelowDewpointT = TimeStepZone;
					ZoneITEq( Loop ).TimeOutOfOperRange = TimeStepZone;
					ZoneITEq( Loop ).DewpointTBelowDeltaT = TDPAirIn - DPMin( EnvClass );
					ZnRpt( NZ ).ITEqTimeBelowDewpointT = TimeStepZone;
					ZnRpt( NZ ).ITEqTimeOutOfOperRange = TimeStepZone;
				}
				if ( RHAirIn > RHMax( EnvClass ) ) {
					ZoneITEq( Loop ).TimeAboveRH = TimeStepZone;
					ZoneITEq( Loop ).TimeOutOfOperRange = TimeStepZone;
					ZoneITEq( Loop ).RHAboveDeltaRH = RHAirIn - RHMax( EnvClass );
					ZnRpt( NZ ).ITEqTimeAboveRH = TimeStepZone;
					ZnRpt( NZ ).ITEqTimeOutOfOperRange = TimeStepZone;
				}
				if ( RHAirIn < RHMin( EnvClass ) ) {
					ZoneITEq( Loop ).TimeBelowRH = TimeStepZone;
					ZoneITEq( Loop ).TimeOutOfOperRange = TimeStepZone;
					ZoneITEq( Loop ).RHBelowDeltaRH = RHAirIn - RHMin( EnvClass );
					ZnRpt( NZ ).ITEqTimeBelowRH = TimeStepZone;
					ZnRpt( NZ ).ITEqTimeOutOfOperRange = TimeStepZone;
				}
			}

		} // ZoneITEq calc loop

		// Zone-level sensible heat index
		for ( Loop = 1; Loop <= NumOfZones; ++Loop ) {
			if ( ZoneSumToutMinusTSup( Loop ) != 0.0 ) {
				ZnRpt( Loop ).ITEqSHI = ZoneSumTinMinusTSup( Loop ) / ZoneSumToutMinusTSup( Loop );
			}
		}

	} // End CalcZoneITEq

	void
	ReportInternalHeatGains()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   June 1997
		//       MODIFIED       July 1997 RKS
		//       RE-ENGINEERED  December 1998 LKL

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine currently creates the values for standard "zone loads" reporting
		// from the heat balance module.

		// METHODOLOGY EMPLOYED:
		// The reporting methodology is described in the OutputDataStructure.doc
		// as the "modified modular" format.

		// REFERENCES:
		// OutputDataStructure.doc (EnergyPlus documentation)

		// Using/Aliasing
		using OutputReportTabular::WriteTabularFiles;

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
		int Loop;
		int ZoneLoop; // Counter for the # of zones (nz)
		static Array1D_int TradIntGainTypes( 7, { IntGainTypeOf_People, IntGainTypeOf_Lights, IntGainTypeOf_ElectricEquipment, IntGainTypeOf_GasEquipment, IntGainTypeOf_HotWaterEquipment, IntGainTypeOf_SteamEquipment, IntGainTypeOf_OtherEquipment } );

		// FLOW:
		for ( Loop = 1; Loop <= TotPeople; ++Loop ) {
			People( Loop ).RadGainEnergy = People( Loop ).RadGainRate * TimeStepZoneSec;
			People( Loop ).ConGainEnergy = People( Loop ).ConGainRate * TimeStepZoneSec;
			People( Loop ).SenGainEnergy = People( Loop ).SenGainRate * TimeStepZoneSec;
			People( Loop ).LatGainEnergy = People( Loop ).LatGainRate * TimeStepZoneSec;
			People( Loop ).TotGainEnergy = People( Loop ).TotGainRate * TimeStepZoneSec;
		}

		for ( Loop = 1; Loop <= TotLights; ++Loop ) {
			Lights( Loop ).Consumption = Lights( Loop ).Power * TimeStepZoneSec;
			Lights( Loop ).RadGainEnergy = Lights( Loop ).RadGainRate * TimeStepZoneSec;
			Lights( Loop ).VisGainEnergy = Lights( Loop ).VisGainRate * TimeStepZoneSec;
			Lights( Loop ).ConGainEnergy = Lights( Loop ).ConGainRate * TimeStepZoneSec;
			Lights( Loop ).RetAirGainEnergy = Lights( Loop ).RetAirGainRate * TimeStepZoneSec;
			Lights( Loop ).TotGainEnergy = Lights( Loop ).TotGainRate * TimeStepZoneSec;
			if ( ! WarmupFlag ) {
				if ( DoOutputReporting && WriteTabularFiles && ( KindOfSim == ksRunPeriodWeather ) ) { //for weather simulations only
					//for tabular report, accumlate the total electricity used for each Light object
					Lights( Loop ).SumConsumption += Lights( Loop ).Consumption;
					//for tabular report, accumulate the time when each Light has consumption (using a very small threshold instead of zero)
					if ( Lights( Loop ).Power > 0.01 * Lights( Loop ).DesignLevel ) {
						Lights( Loop ).SumTimeNotZeroCons += TimeStepZone;
					}
				}
			}
		}

		for ( Loop = 1; Loop <= TotElecEquip; ++Loop ) {
			ZoneElectric( Loop ).Consumption = ZoneElectric( Loop ).Power * TimeStepZoneSec;
			ZoneElectric( Loop ).RadGainEnergy = ZoneElectric( Loop ).RadGainRate * TimeStepZoneSec;
			ZoneElectric( Loop ).ConGainEnergy = ZoneElectric( Loop ).ConGainRate * TimeStepZoneSec;
			ZoneElectric( Loop ).LatGainEnergy = ZoneElectric( Loop ).LatGainRate * TimeStepZoneSec;
			ZoneElectric( Loop ).LostEnergy = ZoneElectric( Loop ).LostRate * TimeStepZoneSec;
			ZoneElectric( Loop ).TotGainEnergy = ZoneElectric( Loop ).TotGainRate * TimeStepZoneSec;
		}

		for ( Loop = 1; Loop <= TotGasEquip; ++Loop ) {
			ZoneGas( Loop ).Consumption = ZoneGas( Loop ).Power * TimeStepZoneSec;
			ZoneGas( Loop ).RadGainEnergy = ZoneGas( Loop ).RadGainRate * TimeStepZoneSec;
			ZoneGas( Loop ).ConGainEnergy = ZoneGas( Loop ).ConGainRate * TimeStepZoneSec;
			ZoneGas( Loop ).LatGainEnergy = ZoneGas( Loop ).LatGainRate * TimeStepZoneSec;
			ZoneGas( Loop ).LostEnergy = ZoneGas( Loop ).LostRate * TimeStepZoneSec;
			ZoneGas( Loop ).TotGainEnergy = ZoneGas( Loop ).TotGainRate * TimeStepZoneSec;
		}

		for ( Loop = 1; Loop <= TotOthEquip; ++Loop ) {
			ZoneOtherEq( Loop ).Consumption = ZoneOtherEq( Loop ).Power * TimeStepZoneSec;
			ZoneOtherEq( Loop ).RadGainEnergy = ZoneOtherEq( Loop ).RadGainRate * TimeStepZoneSec;
			ZoneOtherEq( Loop ).ConGainEnergy = ZoneOtherEq( Loop ).ConGainRate * TimeStepZoneSec;
			ZoneOtherEq( Loop ).LatGainEnergy = ZoneOtherEq( Loop ).LatGainRate * TimeStepZoneSec;
			ZoneOtherEq( Loop ).LostEnergy = ZoneOtherEq( Loop ).LostRate * TimeStepZoneSec;
			ZoneOtherEq( Loop ).TotGainEnergy = ZoneOtherEq( Loop ).TotGainRate * TimeStepZoneSec;
		}

		for ( Loop = 1; Loop <= TotHWEquip; ++Loop ) {
			ZoneHWEq( Loop ).Consumption = ZoneHWEq( Loop ).Power * TimeStepZoneSec;
			ZoneHWEq( Loop ).RadGainEnergy = ZoneHWEq( Loop ).RadGainRate * TimeStepZoneSec;
			ZoneHWEq( Loop ).ConGainEnergy = ZoneHWEq( Loop ).ConGainRate * TimeStepZoneSec;
			ZoneHWEq( Loop ).LatGainEnergy = ZoneHWEq( Loop ).LatGainRate * TimeStepZoneSec;
			ZoneHWEq( Loop ).LostEnergy = ZoneHWEq( Loop ).LostRate * TimeStepZoneSec;
			ZoneHWEq( Loop ).TotGainEnergy = ZoneHWEq( Loop ).TotGainRate * TimeStepZoneSec;
		}

		for ( Loop = 1; Loop <= TotStmEquip; ++Loop ) {
			ZoneSteamEq( Loop ).Consumption = ZoneSteamEq( Loop ).Power * TimeStepZoneSec;
			ZoneSteamEq( Loop ).RadGainEnergy = ZoneSteamEq( Loop ).RadGainRate * TimeStepZoneSec;
			ZoneSteamEq( Loop ).ConGainEnergy = ZoneSteamEq( Loop ).ConGainRate * TimeStepZoneSec;
			ZoneSteamEq( Loop ).LatGainEnergy = ZoneSteamEq( Loop ).LatGainRate * TimeStepZoneSec;
			ZoneSteamEq( Loop ).LostEnergy = ZoneSteamEq( Loop ).LostRate * TimeStepZoneSec;
			ZoneSteamEq( Loop ).TotGainEnergy = ZoneSteamEq( Loop ).TotGainRate * TimeStepZoneSec;
		}

		for ( Loop = 1; Loop <= TotBBHeat; ++Loop ) {
			ZoneBBHeat( Loop ).Consumption = ZoneBBHeat( Loop ).Power * TimeStepZoneSec;
			ZoneBBHeat( Loop ).RadGainEnergy = ZoneBBHeat( Loop ).RadGainRate * TimeStepZoneSec;
			ZoneBBHeat( Loop ).ConGainEnergy = ZoneBBHeat( Loop ).ConGainRate * TimeStepZoneSec;
			ZoneBBHeat( Loop ).TotGainEnergy = ZoneBBHeat( Loop ).TotGainRate * TimeStepZoneSec;
		}

		for ( ZoneLoop = 1; ZoneLoop <= NumOfZones; ++ZoneLoop ) {
			// People
			ZnRpt( ZoneLoop ).PeopleNumOcc = ZoneIntGain( ZoneLoop ).NOFOCC;
			ZnRpt( ZoneLoop ).PeopleRadGain = ZoneIntGain( ZoneLoop ).QOCRAD * TimeStepZoneSec;
			ZnRpt( ZoneLoop ).PeopleConGain = ZoneIntGain( ZoneLoop ).QOCCON * TimeStepZoneSec;
			ZnRpt( ZoneLoop ).PeopleSenGain = ZoneIntGain( ZoneLoop ).QOCSEN * TimeStepZoneSec;
			ZnRpt( ZoneLoop ).PeopleLatGain = ZoneIntGain( ZoneLoop ).QOCLAT * TimeStepZoneSec;
			ZnRpt( ZoneLoop ).PeopleTotGain = ZoneIntGain( ZoneLoop ).QOCTOT * TimeStepZoneSec;
			ZnRpt( ZoneLoop ).PeopleRadGainRate = ZoneIntGain( ZoneLoop ).QOCRAD;
			ZnRpt( ZoneLoop ).PeopleConGainRate = ZoneIntGain( ZoneLoop ).QOCCON;
			ZnRpt( ZoneLoop ).PeopleSenGainRate = ZoneIntGain( ZoneLoop ).QOCSEN;
			ZnRpt( ZoneLoop ).PeopleLatGainRate = ZoneIntGain( ZoneLoop ).QOCLAT;
			ZnRpt( ZoneLoop ).PeopleTotGainRate = ZoneIntGain( ZoneLoop ).QOCTOT;

			// General Lights
			ZnRpt( ZoneLoop ).LtsRetAirGain = ZoneIntGain( ZoneLoop ).QLTCRA * TimeStepZoneSec;
			ZnRpt( ZoneLoop ).LtsRadGain = ZoneIntGain( ZoneLoop ).QLTRAD * TimeStepZoneSec;
			ZnRpt( ZoneLoop ).LtsTotGain = ZoneIntGain( ZoneLoop ).QLTTOT * TimeStepZoneSec;
			ZnRpt( ZoneLoop ).LtsConGain = ZoneIntGain( ZoneLoop ).QLTCON * TimeStepZoneSec;
			ZnRpt( ZoneLoop ).LtsVisGain = ZoneIntGain( ZoneLoop ).QLTSW * TimeStepZoneSec;
			ZnRpt( ZoneLoop ).LtsRetAirGainRate = ZoneIntGain( ZoneLoop ).QLTCRA;
			ZnRpt( ZoneLoop ).LtsRadGainRate = ZoneIntGain( ZoneLoop ).QLTRAD;
			ZnRpt( ZoneLoop ).LtsTotGainRate = ZoneIntGain( ZoneLoop ).QLTTOT;
			ZnRpt( ZoneLoop ).LtsConGainRate = ZoneIntGain( ZoneLoop ).QLTCON;
			ZnRpt( ZoneLoop ).LtsVisGainRate = ZoneIntGain( ZoneLoop ).QLTSW;
			ZnRpt( ZoneLoop ).LtsElecConsump = ZnRpt( ZoneLoop ).LtsTotGain;

			// Electric Equipment
			ZnRpt( ZoneLoop ).ElecConGain = ZoneIntGain( ZoneLoop ).QEECON * TimeStepZoneSec;
			ZnRpt( ZoneLoop ).ElecRadGain = ZoneIntGain( ZoneLoop ).QEERAD * TimeStepZoneSec;
			ZnRpt( ZoneLoop ).ElecLatGain = ZoneIntGain( ZoneLoop ).QEELAT * TimeStepZoneSec;
			ZnRpt( ZoneLoop ).ElecLost = ZoneIntGain( ZoneLoop ).QEELost * TimeStepZoneSec;
			ZnRpt( ZoneLoop ).ElecConGainRate = ZoneIntGain( ZoneLoop ).QEECON;
			ZnRpt( ZoneLoop ).ElecRadGainRate = ZoneIntGain( ZoneLoop ).QEERAD;
			ZnRpt( ZoneLoop ).ElecLatGainRate = ZoneIntGain( ZoneLoop ).QEELAT;
			ZnRpt( ZoneLoop ).ElecLostRate = ZoneIntGain( ZoneLoop ).QEELost;
			ZnRpt( ZoneLoop ).ElecConsump = ZnRpt( ZoneLoop ).ElecConGain + ZnRpt( ZoneLoop ).ElecRadGain + ZnRpt( ZoneLoop ).ElecLatGain + ZnRpt( ZoneLoop ).ElecLost;
			ZnRpt( ZoneLoop ).ElecTotGain = ZnRpt( ZoneLoop ).ElecConGain + ZnRpt( ZoneLoop ).ElecRadGain + ZnRpt( ZoneLoop ).ElecLatGain;
			ZnRpt( ZoneLoop ).ElecTotGainRate = ZnRpt( ZoneLoop ).ElecConGainRate + ZnRpt( ZoneLoop ).ElecRadGainRate + ZnRpt( ZoneLoop ).ElecLatGainRate;

			// Gas Equipment
			ZnRpt( ZoneLoop ).GasConGain = ZoneIntGain( ZoneLoop ).QGECON * TimeStepZoneSec;
			ZnRpt( ZoneLoop ).GasRadGain = ZoneIntGain( ZoneLoop ).QGERAD * TimeStepZoneSec;
			ZnRpt( ZoneLoop ).GasLatGain = ZoneIntGain( ZoneLoop ).QGELAT * TimeStepZoneSec;
			ZnRpt( ZoneLoop ).GasLost = ZoneIntGain( ZoneLoop ).QGELost * TimeStepZoneSec;
			ZnRpt( ZoneLoop ).GasConGainRate = ZoneIntGain( ZoneLoop ).QGECON;
			ZnRpt( ZoneLoop ).GasRadGainRate = ZoneIntGain( ZoneLoop ).QGERAD;
			ZnRpt( ZoneLoop ).GasLatGainRate = ZoneIntGain( ZoneLoop ).QGELAT;
			ZnRpt( ZoneLoop ).GasLostRate = ZoneIntGain( ZoneLoop ).QGELost;
			ZnRpt( ZoneLoop ).GasConsump = ZnRpt( ZoneLoop ).GasConGain + ZnRpt( ZoneLoop ).GasRadGain + ZnRpt( ZoneLoop ).GasLatGain + ZnRpt( ZoneLoop ).GasLost;
			ZnRpt( ZoneLoop ).GasTotGain = ZnRpt( ZoneLoop ).GasConGain + ZnRpt( ZoneLoop ).GasRadGain + ZnRpt( ZoneLoop ).GasLatGain;
			ZnRpt( ZoneLoop ).GasTotGainRate = ZnRpt( ZoneLoop ).GasConGainRate + ZnRpt( ZoneLoop ).GasRadGainRate + ZnRpt( ZoneLoop ).GasLatGainRate;

			// Hot Water Equipment
			ZnRpt( ZoneLoop ).HWConGain = ZoneIntGain( ZoneLoop ).QHWCON * TimeStepZoneSec;
			ZnRpt( ZoneLoop ).HWRadGain = ZoneIntGain( ZoneLoop ).QHWRAD * TimeStepZoneSec;
			ZnRpt( ZoneLoop ).HWLatGain = ZoneIntGain( ZoneLoop ).QHWLAT * TimeStepZoneSec;
			ZnRpt( ZoneLoop ).HWLost = ZoneIntGain( ZoneLoop ).QHWLost * TimeStepZoneSec;
			ZnRpt( ZoneLoop ).HWConGainRate = ZoneIntGain( ZoneLoop ).QHWCON;
			ZnRpt( ZoneLoop ).HWRadGainRate = ZoneIntGain( ZoneLoop ).QHWRAD;
			ZnRpt( ZoneLoop ).HWLatGainRate = ZoneIntGain( ZoneLoop ).QHWLAT;
			ZnRpt( ZoneLoop ).HWLostRate = ZoneIntGain( ZoneLoop ).QHWLost;
			ZnRpt( ZoneLoop ).HWConsump = ZnRpt( ZoneLoop ).HWConGain + ZnRpt( ZoneLoop ).HWRadGain + ZnRpt( ZoneLoop ).HWLatGain + ZnRpt( ZoneLoop ).HWLost;
			ZnRpt( ZoneLoop ).HWTotGain = ZnRpt( ZoneLoop ).HWConGain + ZnRpt( ZoneLoop ).HWRadGain + ZnRpt( ZoneLoop ).HWLatGain;
			ZnRpt( ZoneLoop ).HWTotGainRate = ZnRpt( ZoneLoop ).HWConGainRate + ZnRpt( ZoneLoop ).HWRadGainRate + ZnRpt( ZoneLoop ).HWLatGainRate;

			// Steam Equipment
			ZnRpt( ZoneLoop ).SteamConGain = ZoneIntGain( ZoneLoop ).QSECON * TimeStepZoneSec;
			ZnRpt( ZoneLoop ).SteamRadGain = ZoneIntGain( ZoneLoop ).QSERAD * TimeStepZoneSec;
			ZnRpt( ZoneLoop ).SteamLatGain = ZoneIntGain( ZoneLoop ).QSELAT * TimeStepZoneSec;
			ZnRpt( ZoneLoop ).SteamLost = ZoneIntGain( ZoneLoop ).QSELost * TimeStepZoneSec;
			ZnRpt( ZoneLoop ).SteamConGainRate = ZoneIntGain( ZoneLoop ).QSECON;
			ZnRpt( ZoneLoop ).SteamRadGainRate = ZoneIntGain( ZoneLoop ).QSERAD;
			ZnRpt( ZoneLoop ).SteamLatGainRate = ZoneIntGain( ZoneLoop ).QSELAT;
			ZnRpt( ZoneLoop ).SteamLostRate = ZoneIntGain( ZoneLoop ).QSELost;
			ZnRpt( ZoneLoop ).SteamConsump = ZnRpt( ZoneLoop ).SteamConGain + ZnRpt( ZoneLoop ).SteamRadGain + ZnRpt( ZoneLoop ).SteamLatGain + ZnRpt( ZoneLoop ).SteamLost;
			ZnRpt( ZoneLoop ).SteamTotGain = ZnRpt( ZoneLoop ).SteamConGain + ZnRpt( ZoneLoop ).SteamRadGain + ZnRpt( ZoneLoop ).SteamLatGain;
			ZnRpt( ZoneLoop ).SteamTotGainRate = ZnRpt( ZoneLoop ).SteamConGainRate + ZnRpt( ZoneLoop ).SteamRadGainRate + ZnRpt( ZoneLoop ).SteamLatGainRate;

			// Other Equipment
			ZnRpt( ZoneLoop ).OtherConGain = ZoneIntGain( ZoneLoop ).QOECON * TimeStepZoneSec;
			ZnRpt( ZoneLoop ).OtherRadGain = ZoneIntGain( ZoneLoop ).QOERAD * TimeStepZoneSec;
			ZnRpt( ZoneLoop ).OtherLatGain = ZoneIntGain( ZoneLoop ).QOELAT * TimeStepZoneSec;
			ZnRpt( ZoneLoop ).OtherLost = ZoneIntGain( ZoneLoop ).QOELost * TimeStepZoneSec;
			ZnRpt( ZoneLoop ).OtherConGainRate = ZoneIntGain( ZoneLoop ).QOECON;
			ZnRpt( ZoneLoop ).OtherRadGainRate = ZoneIntGain( ZoneLoop ).QOERAD;
			ZnRpt( ZoneLoop ).OtherLatGainRate = ZoneIntGain( ZoneLoop ).QOELAT;
			ZnRpt( ZoneLoop ).OtherLostRate = ZoneIntGain( ZoneLoop ).QOELost;
			ZnRpt( ZoneLoop ).OtherTotGain = ZnRpt( ZoneLoop ).OtherConGain + ZnRpt( ZoneLoop ).OtherRadGain + ZnRpt( ZoneLoop ).OtherLatGain;
			ZnRpt( ZoneLoop ).OtherTotGainRate = ZnRpt( ZoneLoop ).OtherConGainRate + ZnRpt( ZoneLoop ).OtherRadGainRate + ZnRpt( ZoneLoop ).OtherLatGainRate;

			// Baseboard Heat
			ZnRpt( ZoneLoop ).BaseHeatConGain = ZoneIntGain( ZoneLoop ).QBBCON * TimeStepZoneSec;
			ZnRpt( ZoneLoop ).BaseHeatRadGain = ZoneIntGain( ZoneLoop ).QBBRAD * TimeStepZoneSec;
			ZnRpt( ZoneLoop ).BaseHeatConGainRate = ZoneIntGain( ZoneLoop ).QBBCON;
			ZnRpt( ZoneLoop ).BaseHeatRadGainRate = ZoneIntGain( ZoneLoop ).QBBRAD;
			ZnRpt( ZoneLoop ).BaseHeatTotGain = ZnRpt( ZoneLoop ).BaseHeatConGain + ZnRpt( ZoneLoop ).BaseHeatRadGain;
			ZnRpt( ZoneLoop ).BaseHeatTotGainRate = ZnRpt( ZoneLoop ).BaseHeatConGainRate + ZnRpt( ZoneLoop ).BaseHeatRadGainRate;
			ZnRpt( ZoneLoop ).BaseHeatElecCons = ZnRpt( ZoneLoop ).BaseHeatTotGain;

			// Overall Zone Variables

			// these overalls include component gains from devices like water heater, water use, and generators
			//   working vars QFCConv QGenConv QFCRad QGenRad  WaterUseLatentGain WaterThermalTankGain WaterUseSensibleGain

			ZnRpt( ZoneLoop ).TotVisHeatGain = ZnRpt( ZoneLoop ).LtsVisGain;
			ZnRpt( ZoneLoop ).TotVisHeatGainRate = ZnRpt( ZoneLoop ).LtsVisGainRate;

			SumInternalRadiationGainsByTypes( ZoneLoop, TradIntGainTypes, ZnRpt( ZoneLoop ).TotRadiantGainRate );
			ZnRpt( ZoneLoop ).TotRadiantGain = ZnRpt( ZoneLoop ).TotRadiantGainRate * TimeStepZoneSec;

			SumInternalConvectionGainsByTypes( ZoneLoop, TradIntGainTypes, ZnRpt( ZoneLoop ).TotConvectiveGainRate );
			ZnRpt( ZoneLoop ).TotConvectiveGain = ZnRpt( ZoneLoop ).TotConvectiveGainRate * TimeStepZoneSec;

			SumInternalLatentGainsByTypes( ZoneLoop, TradIntGainTypes, ZnRpt( ZoneLoop ).TotLatentGainRate );
			ZnRpt( ZoneLoop ).TotLatentGain = ZnRpt( ZoneLoop ).TotLatentGainRate * TimeStepZoneSec;

			ZnRpt( ZoneLoop ).TotTotalHeatGainRate = ZnRpt( ZoneLoop ).TotLatentGainRate + ZnRpt( ZoneLoop ).TotRadiantGainRate + ZnRpt( ZoneLoop ).TotConvectiveGainRate + ZnRpt( ZoneLoop ).TotVisHeatGainRate;
			ZnRpt( ZoneLoop ).TotTotalHeatGain = ZnRpt( ZoneLoop ).TotTotalHeatGainRate * TimeStepZoneSec;
		}

	}

	Real64
	GetDesignLightingLevelForZone( int const WhichZone ) // name of zone
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   April 2007; January 2008 - moved to InternalGains
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This routine sums the Lighting Level for a zone.
		// Will issue a severe error for illegal zone.
		// Must be called after InternalHeatGains get input.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataHeatBalance;
		using namespace DataGlobals;

		// Return value
		Real64 DesignLightingLevelSum; // Sum of design lighting level for this zone

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int Loop;

		if ( GetInternalHeatGainsInputFlag ) {
			ShowFatalError( "GetDesignLightingLevelForZone: Function called prior to Getting Lights Input." );
		}

		DesignLightingLevelSum = 0.0;

		for ( Loop = 1; Loop <= TotLights; ++Loop ) {
			if ( Lights( Loop ).ZonePtr == WhichZone ) {
				DesignLightingLevelSum += Lights( Loop ).DesignLevel;
			}
		}

		return DesignLightingLevelSum;

	}

	void
	CheckLightsReplaceableMinMaxForZone( int const WhichZone ) // Zone Number
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   April 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Daylighting is not available unless Lights (replaceable) is 0.0 or 1.0.  No dimming will be done
		// unless the lights replaceable fraction is 1.0.  This is documented in the InputOutputReference but
		// not warned about.  Also, this will sum the Zone Design Lighting level, in case the calling routine
		// would like to have an error if the lights is zero and daylighting is requested.

		// METHODOLOGY EMPLOYED:
		// Traverse the LIGHTS structure and get fraction replaceable - min/max as well as lighting
		// level for a zone.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataDaylighting;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Loop;
		Real64 LightsRepMin; // Minimum Lighting replacement fraction for any lights statement for this zone
		Real64 LightsRepMax; // Maximum Lighting replacement fraction for any lights statement for this zone
		int NumLights; // Number of Lights statement for that zone.

		if ( GetInternalHeatGainsInputFlag ) {
			ShowFatalError( "CheckLightsReplaceableMinMaxForZone: Function called prior to Getting Lights Input." );
		}

		LightsRepMin = 99999.0;
		LightsRepMax = -99999.0;
		NumLights = 0;

		for ( Loop = 1; Loop <= TotLights; ++Loop ) {
			if ( Lights( Loop ).ZonePtr != WhichZone ) continue;
			LightsRepMin = min( LightsRepMin, Lights( Loop ).FractionReplaceable );
			LightsRepMax = max( LightsRepMax, Lights( Loop ).FractionReplaceable );
			++NumLights;
			if ( ( ZoneDaylight( Lights( Loop ).ZonePtr ).DaylightType == DetailedDaylighting || ZoneDaylight( Lights( Loop ).ZonePtr ).DaylightType == DElightDaylighting ) && ( Lights( Loop ).FractionReplaceable > 0.0 && Lights( Loop ).FractionReplaceable < 1.0 ) ) {
				ShowWarningError( "CheckLightsReplaceableMinMaxForZone: Fraction Replaceable must be 0.0 or 1.0 if used with daylighting." );
				ShowContinueError( "..Lights=\"" + Lights( Loop ).Name + "\", Fraction Replaceable will be reset to 1.0 to allow dimming controls" );
				ShowContinueError( "..in Zone=" + Zone( WhichZone ).Name );
				Lights( Loop ).FractionReplaceable = 1.0;
			}
		}

		if ( ZoneDaylight( WhichZone ).DaylightType == DetailedDaylighting ) {
			if ( LightsRepMax == 0.0 ) {
				ShowWarningError( "CheckLightsReplaceable: Zone \"" + Zone( WhichZone ).Name + "\" has Daylighting:Controls." );
				ShowContinueError( "but all of the LIGHTS object in that zone have zero Fraction Replaceable." );
				ShowContinueError( "The daylighting controls will have no effect." );
			}
			if ( NumLights == 0 ) {
				ShowWarningError( "CheckLightsReplaceable: Zone \"" + Zone( WhichZone ).Name + "\" has Daylighting:Controls." );
				ShowContinueError( "but there are no LIGHTS objects in that zone." );
				ShowContinueError( "The daylighting controls will have no effect." );
			}
		} else if ( ZoneDaylight( WhichZone ).DaylightType == DElightDaylighting ) {
			if ( LightsRepMax == 0.0 ) {
				ShowWarningError( "CheckLightsReplaceable: Zone \"" + Zone( WhichZone ).Name + "\" has Daylighting:Controls." );
				ShowContinueError( "but all of the LIGHTS object in that zone have zero Fraction Replaceable." );
				ShowContinueError( "The daylighting controls will have no effect." );
			}
			if ( NumLights == 0 ) {
				ShowWarningError( "CheckLightsReplaceable: Zone \"" + Zone( WhichZone ).Name + "\" has Daylighting:Controls." );
				ShowContinueError( "but there are no LIGHTS objects in that zone." );
				ShowContinueError( "The daylighting controls will have no effect." );
			}
		}

	}

	void
	UpdateInternalGainValues(
		Optional_bool_const SuppressRadiationUpdate,
		Optional_bool_const SumLatentGains
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   Dec. 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHeatBalFanSys::ZoneLatentGain;
		using DataContaminantBalance::Contaminant;
		using DataContaminantBalance::ZoneGCGain;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Loop;
		int NZ;
		bool DoRadiationUpdate;
		bool ReSumLatentGains;

		DoRadiationUpdate = true;
		ReSumLatentGains = false;

		if ( present( SuppressRadiationUpdate ) ) {
			if ( SuppressRadiationUpdate ) DoRadiationUpdate = false;
		}

		if ( present( SumLatentGains ) ) {
			if ( SumLatentGains ) ReSumLatentGains = true;
		}

		// store pointer values to hold generic internal gain values constant for entire timestep
		for ( NZ = 1; NZ <= NumOfZones; ++NZ ) {
			for ( Loop = 1; Loop <= ZoneIntGain( NZ ).NumberOfDevices; ++Loop ) {
				ZoneIntGain( NZ ).Device( Loop ).ConvectGainRate = ZoneIntGain( NZ ).Device( Loop ).PtrConvectGainRate;
				ZoneIntGain( NZ ).Device( Loop ).ReturnAirConvGainRate = ZoneIntGain( NZ ).Device( Loop ).PtrReturnAirConvGainRate;
				if ( DoRadiationUpdate ) ZoneIntGain( NZ ).Device( Loop ).RadiantGainRate = ZoneIntGain( NZ ).Device( Loop ).PtrRadiantGainRate;
				ZoneIntGain( NZ ).Device( Loop ).LatentGainRate = ZoneIntGain( NZ ).Device( Loop ).PtrLatentGainRate;
				ZoneIntGain( NZ ).Device( Loop ).ReturnAirLatentGainRate = ZoneIntGain( NZ ).Device( Loop ).PtrReturnAirLatentGainRate;
				ZoneIntGain( NZ ).Device( Loop ).CarbonDioxideGainRate = ZoneIntGain( NZ ).Device( Loop ).PtrCarbonDioxideGainRate;
				ZoneIntGain( NZ ).Device( Loop ).GenericContamGainRate = ZoneIntGain( NZ ).Device( Loop ).PtrGenericContamGainRate;
			}
			if ( ReSumLatentGains ) {
				SumAllInternalLatentGains( NZ, ZoneLatentGain( NZ ) );
			}
		}

		if ( Contaminant.GenericContamSimulation && allocated( ZoneGCGain ) ) {
			for ( NZ = 1; NZ <= NumOfZones; ++NZ ) {
				SumAllInternalGenericContamGains( NZ, ZoneGCGain( NZ ) );
				ZnRpt( NZ ).GCRate = ZoneGCGain( NZ );
			}
		}

	}

	void
	SumAllInternalConvectionGains(
		int const ZoneNum, // zone index pointer for which zone to sum gains for
		Real64 & SumConvGainRate
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   Nov. 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// worker routine for summing all the internal gain types

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 tmpSumConvGainRate;
		int DeviceNum;

		tmpSumConvGainRate = 0.0;

		if ( ZoneIntGain( ZoneNum ).NumberOfDevices == 0 ) {
			SumConvGainRate = 0.0;
			return;
		}

		for ( DeviceNum = 1; DeviceNum <= ZoneIntGain( ZoneNum ).NumberOfDevices; ++DeviceNum ) {
			tmpSumConvGainRate += ZoneIntGain( ZoneNum ).Device( DeviceNum ).ConvectGainRate;
		}

		SumConvGainRate = tmpSumConvGainRate;

	}

	void
	SumInternalConvectionGainsByTypes(
		int const ZoneNum, // zone index pointer for which zone to sum gains for
		Array1S_int const GainTypeARR, // variable length 1-d array of integer valued gain types
		Real64 & SumConvGainRate
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   Nov. 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// worker routine for summing a subset of the internal gain types

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumberOfTypes;
		Real64 tmpSumConvGainRate;
		int DeviceNum;
		int TypeNum;

		NumberOfTypes = size( GainTypeARR );
		tmpSumConvGainRate = 0.0;

		if ( ZoneIntGain( ZoneNum ).NumberOfDevices == 0 ) {
			SumConvGainRate = 0.0;
			return;
		}

		for ( DeviceNum = 1; DeviceNum <= ZoneIntGain( ZoneNum ).NumberOfDevices; ++DeviceNum ) {
			for ( TypeNum = 1; TypeNum <= NumberOfTypes; ++TypeNum ) {

				if ( ZoneIntGain( ZoneNum ).Device( DeviceNum ).CompTypeOfNum == GainTypeARR( TypeNum ) ) {
					tmpSumConvGainRate += ZoneIntGain( ZoneNum ).Device( DeviceNum ).ConvectGainRate;
				}

			}
		}

		SumConvGainRate = tmpSumConvGainRate;

	}

	void
	SumAllReturnAirConvectionGains(
		int const ZoneNum, // zone index pointer for which zone to sum gains for
		Real64 & SumReturnAirGainRate
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   Dec. 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// worker routine for summing all the internal gain types

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 tmpSumRetAirGainRate;
		int DeviceNum;

		tmpSumRetAirGainRate = 0.0;

		if ( ZoneIntGain( ZoneNum ).NumberOfDevices == 0 ) {
			SumReturnAirGainRate = 0.0;
			return;
		}

		for ( DeviceNum = 1; DeviceNum <= ZoneIntGain( ZoneNum ).NumberOfDevices; ++DeviceNum ) {
			tmpSumRetAirGainRate += ZoneIntGain( ZoneNum ).Device( DeviceNum ).ReturnAirConvGainRate;
		}

		SumReturnAirGainRate = tmpSumRetAirGainRate;

	}

	void
	SumReturnAirConvectionGainsByTypes(
		int const ZoneNum, // zone index pointer for which zone to sum gains for
		Array1S_int const GainTypeARR, // variable length 1-d array of integer valued gain types
		Real64 & SumReturnAirGainRate
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   Nov. 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// worker routine for summing a subset of the internal gain types

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumberOfTypes;
		Real64 tmpSumRetAirConvGainRate;
		int DeviceNum;
		int TypeNum;

		NumberOfTypes = size( GainTypeARR );
		tmpSumRetAirConvGainRate = 0.0;

		if ( ZoneIntGain( ZoneNum ).NumberOfDevices == 0 ) {
			SumReturnAirGainRate = 0.0;
			return;
		}

		for ( DeviceNum = 1; DeviceNum <= ZoneIntGain( ZoneNum ).NumberOfDevices; ++DeviceNum ) {
			for ( TypeNum = 1; TypeNum <= NumberOfTypes; ++TypeNum ) {

				if ( ZoneIntGain( ZoneNum ).Device( DeviceNum ).CompTypeOfNum == GainTypeARR( TypeNum ) ) {
					tmpSumRetAirConvGainRate += ZoneIntGain( ZoneNum ).Device( DeviceNum ).ReturnAirConvGainRate;
				}

			}
		}

		SumReturnAirGainRate = tmpSumRetAirConvGainRate;

	}

	void
	SumAllInternalRadiationGains(
		int const ZoneNum, // zone index pointer for which zone to sum gains for
		Real64 & SumRadGainRate
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   Nov. 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// worker routine for summing all the internal gain types

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 tmpSumRadGainRate;
		int DeviceNum;

		tmpSumRadGainRate = 0.0;

		if ( ZoneIntGain( ZoneNum ).NumberOfDevices == 0 ) {
			SumRadGainRate = 0.0;
			return;
		}

		for ( DeviceNum = 1; DeviceNum <= ZoneIntGain( ZoneNum ).NumberOfDevices; ++DeviceNum ) {
			tmpSumRadGainRate += ZoneIntGain( ZoneNum ).Device( DeviceNum ).RadiantGainRate;
		}

		SumRadGainRate = tmpSumRadGainRate;

	}

	void
	SumInternalRadiationGainsByTypes(
		int const ZoneNum, // zone index pointer for which zone to sum gains for
		Array1S_int const GainTypeARR, // variable length 1-d array of integer valued gain types
		Real64 & SumRadiationGainRate
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   Dec. 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// worker routine for summing a subset of the internal gain types

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumberOfTypes;
		Real64 tmpSumRadiationGainRate;
		int DeviceNum;
		int TypeNum;

		NumberOfTypes = size( GainTypeARR );
		tmpSumRadiationGainRate = 0.0;

		if ( ZoneIntGain( ZoneNum ).NumberOfDevices == 0 ) {
			SumRadiationGainRate = 0.0;
			return;
		}

		for ( DeviceNum = 1; DeviceNum <= ZoneIntGain( ZoneNum ).NumberOfDevices; ++DeviceNum ) {
			for ( TypeNum = 1; TypeNum <= NumberOfTypes; ++TypeNum ) {

				if ( ZoneIntGain( ZoneNum ).Device( DeviceNum ).CompTypeOfNum == GainTypeARR( TypeNum ) ) {
					tmpSumRadiationGainRate += ZoneIntGain( ZoneNum ).Device( DeviceNum ).RadiantGainRate;
				}

			}
		}

		SumRadiationGainRate = tmpSumRadiationGainRate;

	}

	void
	SumAllInternalLatentGains(
		int const ZoneNum, // zone index pointer for which zone to sum gains for
		Real64 & SumLatentGainRate
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   Nov. 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// worker routine for summing all the internal gain types

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 tmpSumLatentGainRate;
		int DeviceNum;

		tmpSumLatentGainRate = 0.0;

		if ( ZoneIntGain( ZoneNum ).NumberOfDevices == 0 ) {
			SumLatentGainRate = 0.0;
			return;
		}

		for ( DeviceNum = 1; DeviceNum <= ZoneIntGain( ZoneNum ).NumberOfDevices; ++DeviceNum ) {
			tmpSumLatentGainRate += ZoneIntGain( ZoneNum ).Device( DeviceNum ).LatentGainRate;
		}

		SumLatentGainRate = tmpSumLatentGainRate;

	}

	void
	SumInternalLatentGainsByTypes(
		int const ZoneNum, // zone index pointer for which zone to sum gains for
		Array1S_int const GainTypeARR, // variable length 1-d array of integer valued gain types
		Real64 & SumLatentGainRate
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   Dec. 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// worker routine for summing a subset of the internal gain types

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumberOfTypes;
		Real64 tmpSumLatentGainRate;
		int DeviceNum;
		int TypeNum;

		NumberOfTypes = size( GainTypeARR );
		tmpSumLatentGainRate = 0.0;

		if ( ZoneIntGain( ZoneNum ).NumberOfDevices == 0 ) {
			SumLatentGainRate = 0.0;
			return;
		}

		for ( DeviceNum = 1; DeviceNum <= ZoneIntGain( ZoneNum ).NumberOfDevices; ++DeviceNum ) {
			for ( TypeNum = 1; TypeNum <= NumberOfTypes; ++TypeNum ) {

				if ( ZoneIntGain( ZoneNum ).Device( DeviceNum ).CompTypeOfNum == GainTypeARR( TypeNum ) ) {
					tmpSumLatentGainRate += ZoneIntGain( ZoneNum ).Device( DeviceNum ).LatentGainRate;
				}

			}
		}

		SumLatentGainRate = tmpSumLatentGainRate;

	}

	void
	SumAllReturnAirLatentGains(
		int const ZoneNum, // zone index pointer for which zone to sum gains for
		Real64 & SumRetAirLatentGainRate
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   Nov. 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// worker routine for summing all the internal gain types

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 tmpSumLatentGainRate;
		int DeviceNum;

		tmpSumLatentGainRate = 0.0;

		if ( ZoneIntGain( ZoneNum ).NumberOfDevices == 0 ) {
			SumRetAirLatentGainRate = 0.0;
			return;
		}

		for ( DeviceNum = 1; DeviceNum <= ZoneIntGain( ZoneNum ).NumberOfDevices; ++DeviceNum ) {
			tmpSumLatentGainRate += ZoneIntGain( ZoneNum ).Device( DeviceNum ).ReturnAirLatentGainRate;
		}

		SumRetAirLatentGainRate = tmpSumLatentGainRate;

	}

	void
	SumAllInternalCO2Gains(
		int const ZoneNum, // zone index pointer for which zone to sum gains for
		Real64 & SumCO2GainRate
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   Dec. 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// worker routine for summing all the internal gain types

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 tmpSumCO2GainRate;
		int DeviceNum;

		tmpSumCO2GainRate = 0.0;

		if ( ZoneIntGain( ZoneNum ).NumberOfDevices == 0 ) {
			SumCO2GainRate = 0.0;
			return;
		}

		for ( DeviceNum = 1; DeviceNum <= ZoneIntGain( ZoneNum ).NumberOfDevices; ++DeviceNum ) {
			tmpSumCO2GainRate += ZoneIntGain( ZoneNum ).Device( DeviceNum ).CarbonDioxideGainRate;
		}

		SumCO2GainRate = tmpSumCO2GainRate;

	}

	void
	SumInternalCO2GainsByTypes(
		int const ZoneNum, // zone index pointer for which zone to sum gains for
		Array1S_int const GainTypeARR, // variable length 1-d array of integer valued gain types
		Real64 & SumCO2GainRate
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   Dec. 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// worker routine for summing a subset of the internal gain types

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumberOfTypes;
		Real64 tmpSumCO2GainRate;
		int DeviceNum;
		int TypeNum;

		NumberOfTypes = size( GainTypeARR );
		tmpSumCO2GainRate = 0.0;

		if ( ZoneIntGain( ZoneNum ).NumberOfDevices == 0 ) {
			SumCO2GainRate = 0.0;
			return;
		}

		for ( DeviceNum = 1; DeviceNum <= ZoneIntGain( ZoneNum ).NumberOfDevices; ++DeviceNum ) {
			for ( TypeNum = 1; TypeNum <= NumberOfTypes; ++TypeNum ) {

				if ( ZoneIntGain( ZoneNum ).Device( DeviceNum ).CompTypeOfNum == GainTypeARR( TypeNum ) ) {
					tmpSumCO2GainRate += ZoneIntGain( ZoneNum ).Device( DeviceNum ).CarbonDioxideGainRate;
				}

			}
		}

		SumCO2GainRate = tmpSumCO2GainRate;

	}

	void
	SumAllInternalGenericContamGains(
		int const ZoneNum, // zone index pointer for which zone to sum gains for
		Real64 & SumGCGainRate
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         L. Gu
		//       DATE WRITTEN   Feb. 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// worker routine for summing all the internal gain types based on the existing subrotine SumAllInternalCO2Gains

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 tmpSumGCGainRate;
		int DeviceNum;

		tmpSumGCGainRate = 0.0;

		if ( ZoneIntGain( ZoneNum ).NumberOfDevices == 0 ) {
			SumGCGainRate = 0.0;
			return;
		}

		for ( DeviceNum = 1; DeviceNum <= ZoneIntGain( ZoneNum ).NumberOfDevices; ++DeviceNum ) {
			tmpSumGCGainRate += ZoneIntGain( ZoneNum ).Device( DeviceNum ).GenericContamGainRate;
		}

		SumGCGainRate = tmpSumGCGainRate;

	}

	void
	GatherComponentLoadsIntGain()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   September 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   Gather values during sizing used for loads component report.

		// METHODOLOGY EMPLOYED:
		//   Save sequence of values for report during sizing.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na
		// Using/Aliasing
		using namespace DataHeatBalance;
		using DataGlobals::NumOfTimeStepInHour;
		using DataGlobals::CompLoadReportIsReq;
		using DataGlobals::isPulseZoneSizing;
		using DataSizing::CurOverallSimDay;
		using OutputReportTabular::peopleInstantSeq;
		using OutputReportTabular::peopleLatentSeq;
		using OutputReportTabular::peopleRadSeq;
		using OutputReportTabular::lightInstantSeq;
		using OutputReportTabular::lightRetAirSeq;
		using OutputReportTabular::lightLWRadSeq;
		using OutputReportTabular::equipInstantSeq;
		using OutputReportTabular::equipLatentSeq;
		using OutputReportTabular::equipRadSeq;
		using OutputReportTabular::refrigInstantSeq;
		using OutputReportTabular::refrigRetAirSeq;
		using OutputReportTabular::refrigLatentSeq;
		using OutputReportTabular::waterUseInstantSeq;
		using OutputReportTabular::waterUseLatentSeq;
		using OutputReportTabular::hvacLossInstantSeq;
		using OutputReportTabular::hvacLossRadSeq;
		using OutputReportTabular::powerGenInstantSeq;
		using OutputReportTabular::powerGenRadSeq;

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
		static int iZone( 0 );
		static int TimeStepInDay( 0 );
		static Array1D_int IntGainTypesPeople( 1, { IntGainTypeOf_People } );
		static Array1D_int IntGainTypesLight( 1, { IntGainTypeOf_Lights } );
		static Array1D_int IntGainTypesEquip( 5, { IntGainTypeOf_ElectricEquipment, IntGainTypeOf_GasEquipment, IntGainTypeOf_HotWaterEquipment, IntGainTypeOf_SteamEquipment, IntGainTypeOf_OtherEquipment } );
		static Array1D_int IntGainTypesRefrig( 7, { IntGainTypeOf_RefrigerationCase, IntGainTypeOf_RefrigerationCompressorRack, IntGainTypeOf_RefrigerationSystemAirCooledCondenser, IntGainTypeOf_RefrigerationSystemSuctionPipe, IntGainTypeOf_RefrigerationSecondaryReceiver, IntGainTypeOf_RefrigerationSecondaryPipe, IntGainTypeOf_RefrigerationWalkIn } );
		static Array1D_int IntGainTypesWaterUse( 3, { IntGainTypeOf_WaterUseEquipment, IntGainTypeOf_WaterHeaterMixed, IntGainTypeOf_WaterHeaterStratified } );
		static Array1D_int IntGainTypesHvacLoss( 13, { IntGainTypeOf_ZoneBaseboardOutdoorTemperatureControlled, IntGainTypeOf_ThermalStorageChilledWaterMixed, IntGainTypeOf_ThermalStorageChilledWaterStratified, IntGainTypeOf_PipeIndoor, IntGainTypeOf_Pump_VarSpeed, IntGainTypeOf_Pump_ConSpeed, IntGainTypeOf_Pump_Cond, IntGainTypeOf_PumpBank_VarSpeed, IntGainTypeOf_PumpBank_ConSpeed, IntGainTypeOf_PlantComponentUserDefined, IntGainTypeOf_CoilUserDefined, IntGainTypeOf_ZoneHVACForcedAirUserDefined, IntGainTypeOf_AirTerminalUserDefined } );
		static Array1D_int IntGainTypesPowerGen( 8, { IntGainTypeOf_GeneratorFuelCell, IntGainTypeOf_GeneratorMicroCHP, IntGainTypeOf_ElectricLoadCenterTransformer, IntGainTypeOf_ElectricLoadCenterInverterSimple, IntGainTypeOf_ElectricLoadCenterInverterFunctionOfPower, IntGainTypeOf_ElectricLoadCenterInverterLookUpTable, IntGainTypeOf_ElectricLoadCenterStorageBattery, IntGainTypeOf_ElectricLoadCenterStorageSimple } );

		if ( CompLoadReportIsReq && ! isPulseZoneSizing ) {
			TimeStepInDay = ( HourOfDay - 1 ) * NumOfTimeStepInHour + TimeStep;
			for ( iZone = 1; iZone <= NumOfZones; ++iZone ) {
				SumInternalConvectionGainsByTypes( iZone, IntGainTypesPeople, peopleInstantSeq( CurOverallSimDay, TimeStepInDay, iZone ) );
				SumInternalLatentGainsByTypes( iZone, IntGainTypesPeople, peopleLatentSeq( CurOverallSimDay, TimeStepInDay, iZone ) );
				SumInternalRadiationGainsByTypes( iZone, IntGainTypesPeople, peopleRadSeq( CurOverallSimDay, TimeStepInDay, iZone ) );

				SumInternalConvectionGainsByTypes( iZone, IntGainTypesLight, lightInstantSeq( CurOverallSimDay, TimeStepInDay, iZone ) );
				SumReturnAirConvectionGainsByTypes( iZone, IntGainTypesLight, lightRetAirSeq( CurOverallSimDay, TimeStepInDay, iZone ) );
				SumInternalRadiationGainsByTypes( iZone, IntGainTypesLight, lightLWRadSeq( CurOverallSimDay, TimeStepInDay, iZone ) );

				SumInternalConvectionGainsByTypes( iZone, IntGainTypesEquip, equipInstantSeq( CurOverallSimDay, TimeStepInDay, iZone ) );
				SumInternalLatentGainsByTypes( iZone, IntGainTypesEquip, equipLatentSeq( CurOverallSimDay, TimeStepInDay, iZone ) );
				SumInternalRadiationGainsByTypes( iZone, IntGainTypesEquip, equipRadSeq( CurOverallSimDay, TimeStepInDay, iZone ) );

				SumInternalConvectionGainsByTypes( iZone, IntGainTypesRefrig, refrigInstantSeq( CurOverallSimDay, TimeStepInDay, iZone ) );
				SumReturnAirConvectionGainsByTypes( iZone, IntGainTypesRefrig, refrigRetAirSeq( CurOverallSimDay, TimeStepInDay, iZone ) );
				SumInternalLatentGainsByTypes( iZone, IntGainTypesRefrig, refrigLatentSeq( CurOverallSimDay, TimeStepInDay, iZone ) );

				SumInternalConvectionGainsByTypes( iZone, IntGainTypesWaterUse, waterUseInstantSeq( CurOverallSimDay, TimeStepInDay, iZone ) );
				SumInternalLatentGainsByTypes( iZone, IntGainTypesWaterUse, waterUseLatentSeq( CurOverallSimDay, TimeStepInDay, iZone ) );

				SumInternalConvectionGainsByTypes( iZone, IntGainTypesHvacLoss, hvacLossInstantSeq( CurOverallSimDay, TimeStepInDay, iZone ) );
				SumInternalRadiationGainsByTypes( iZone, IntGainTypesHvacLoss, hvacLossRadSeq( CurOverallSimDay, TimeStepInDay, iZone ) );

				SumInternalConvectionGainsByTypes( iZone, IntGainTypesPowerGen, powerGenInstantSeq( CurOverallSimDay, TimeStepInDay, iZone ) );
				SumInternalRadiationGainsByTypes( iZone, IntGainTypesPowerGen, powerGenRadSeq( CurOverallSimDay, TimeStepInDay, iZone ) );
			}
		}
	}

	void
	GetInternalGainDeviceIndex(
		int const ZoneNum, // zone index pointer for which zone to sum gains for
		int const IntGainTypeOfNum, // zone internal gain type number
		std::string const & IntGainName, // Internal gain name
		int & DeviceIndex, // Device index
		bool & ErrorFound
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   June 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// utility to retrieve index pointer to a specific internal gain

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		using InputProcessor::SameString;
		// na

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		bool Found;
		int DeviceNum;

		Found = false;

		if ( ZoneIntGain( ZoneNum ).NumberOfDevices == 0 ) {
			DeviceIndex = -1;
			ErrorFound = true;
			return;
		}

		for ( DeviceNum = 1; DeviceNum <= ZoneIntGain( ZoneNum ).NumberOfDevices; ++DeviceNum ) {
			if ( SameString( ZoneIntGain( ZoneNum ).Device( DeviceNum ).CompObjectName, IntGainName ) ) {
				if ( ZoneIntGain( ZoneNum ).Device( DeviceNum ).CompTypeOfNum != IntGainTypeOfNum ) {
					ErrorFound = true;
				}
				else {
					ErrorFound = false;
				}
				Found = true;
				DeviceIndex = DeviceNum;
				break;
			}
		}

	}

	void
	SumInternalConvectionGainsByIndices(
		int const ZoneNum, // zone index pointer for which zone to sum gains for
		Array1S_int const DeviceIndexARR, // variable length 1-d array of integer device index pointers to include in summation
		Array1A< Real64 > const FractionARR, // array of fractional multipliers to apply to devices
		Real64 & SumConvGainRate
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   June 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// worker routine for summing a subset of the internal gains by index

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumberOfIndices;
		int NumberOfFractions;
		Real64 tmpSumConvGainRate;
		int loop;
		int DeviceNum;
		Real64 DeviceFraction;

		NumberOfIndices = isize( DeviceIndexARR );
		NumberOfFractions = isize( FractionARR );
		tmpSumConvGainRate = 0.0;

		//remove this next safety check after testing code
		if ( NumberOfIndices != NumberOfFractions ) { //throw error
			ShowSevereError( "SumInternalConvectionGainsByIndices: bad arguments, sizes do not match" );
		}

		if ( ZoneIntGain( ZoneNum ).NumberOfDevices == 0 ) {
			SumConvGainRate = 0.0;
			return;
		}

		for ( loop = 1; loop <= NumberOfIndices; ++loop ) {
			DeviceNum = DeviceIndexARR( loop );
			DeviceFraction = FractionARR( loop );
			tmpSumConvGainRate = tmpSumConvGainRate + ZoneIntGain( ZoneNum ).Device( DeviceNum ).ConvectGainRate * DeviceFraction;
		}
		SumConvGainRate = tmpSumConvGainRate;

	}

	void
	SumInternalLatentGainsByIndices(
		int const ZoneNum, // zone index pointer for which zone to sum gains for
		Array1S_int const DeviceIndexARR, // variable length 1-d array of integer device index pointers to include in summation
		Array1A< Real64 > const FractionARR, // array of fractional multipliers to apply to devices
		Real64 & SumLatentGainRate
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   June 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// worker routine for summing a subset of the internal gains by index

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumberOfIndices;
		int NumberOfFractions;
		Real64 tmpSumLatentGainRate;
		int loop;
		int DeviceNum;
		Real64 DeviceFraction;

		NumberOfIndices = isize( DeviceIndexARR );
		NumberOfFractions = isize( FractionARR );
		tmpSumLatentGainRate = 0.0;

		//remove this next safety check after testing code
		if ( NumberOfIndices != NumberOfFractions ) { //throw error
			ShowSevereError( "SumInternalLatentGainsByIndices: bad arguments, sizes do not match" );
		}

		if ( ZoneIntGain( ZoneNum ).NumberOfDevices == 0 ) {
			SumLatentGainRate = 0.0;
			return;
		}

		for ( loop = 1; loop <= NumberOfIndices; ++loop ) {
			DeviceNum = DeviceIndexARR( loop );
			DeviceFraction = FractionARR( loop );
			tmpSumLatentGainRate = tmpSumLatentGainRate + ZoneIntGain( ZoneNum ).Device( DeviceNum ).LatentGainRate * DeviceFraction;
		}
		SumLatentGainRate = tmpSumLatentGainRate;

	}

	void
	SumReturnAirConvectionGainsByIndices(
		int const ZoneNum, // zone index pointer for which zone to sum gains for
		Array1S_int const DeviceIndexARR, // variable length 1-d array of integer device index pointers to include in summation
		Array1A< Real64 > const FractionARR, // array of fractional multipliers to apply to devices
		Real64 & SumReturnAirGainRate
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   June 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// worker routine for summing a subset of the internal gains by index

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumberOfIndices;
		int NumberOfFractions;
		Real64 tmpSumReturnAirGainRate;
		int loop;
		int DeviceNum;
		Real64 DeviceFraction;

		NumberOfIndices = isize( DeviceIndexARR );
		NumberOfFractions = isize( FractionARR );
		tmpSumReturnAirGainRate = 0.0;

		//remove this next safety check after testing code
		if ( NumberOfIndices != NumberOfFractions ) { //throw error
			ShowSevereError( "SumReturnAirConvectionGainsByIndice: bad arguments, sizes do not match" );
		}

		if ( ZoneIntGain( ZoneNum ).NumberOfDevices == 0 ) {
			SumReturnAirGainRate = 0.0;
			return;
		}

		for ( loop = 1; loop <= NumberOfIndices; ++loop ) {
			DeviceNum = DeviceIndexARR( loop );
			DeviceFraction = FractionARR( loop );
			tmpSumReturnAirGainRate = tmpSumReturnAirGainRate + ZoneIntGain( ZoneNum ).Device( DeviceNum ).ReturnAirConvGainRate * DeviceFraction;
		}
		SumReturnAirGainRate = tmpSumReturnAirGainRate;

	}

} // InternalHeatGains

} // EnergyPlus
