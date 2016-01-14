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

// EnergyPlus Headers
#include <BaseboardElectric.hh>
#include <DataHeatBalance.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataSizing.hh>
#include <DataZoneEnergyDemands.hh>
#include <DataZoneEquipment.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <GlobalNames.hh>
#include <InputProcessor.hh>
#include <OutputProcessor.hh>
#include <Psychrometrics.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace BaseboardElectric {
	// Module containing the routines dealing with the BASEBOARD Electric HEATER
	// component(s).

	// MODULE INFORMATION:  Richard Liesen
	//       DATE WRITTEN   Nov 2001
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// Needs description

	// METHODOLOGY EMPLOYED:
	// Needs description, as appropriate

	// REFERENCES: none

	// OTHER NOTES: none

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataGlobals;

	// Use statements for access to subroutines in other modules
	using namespace ScheduleManager;

	// Data
	//MODULE PARAMETER DEFINITIONS
	std::string const cCMO_BBRadiator_Electric( "ZoneHVAC:Baseboard:Convective:Electric" );
	Real64 const SimpConvAirFlowSpeed( 0.5 ); // m/s

	// DERIVED TYPE DEFINITIONS

	//MODULE VARIABLE DECLARATIONS:
	int NumBaseboards( 0 );
	Array1D_bool MySizeFlag;
	Array1D_bool CheckEquipName;

	//SUBROUTINE SPECIFICATIONS FOR MODULE BaseboardRadiator

	// Object Data
	Array1D< BaseboardParams > Baseboard;
	Array1D< BaseboardNumericFieldData > BaseboardNumericFields;

	// Functions

	void
	clear_state()
	{
		NumBaseboards = 0;
		MySizeFlag.deallocate();
		CheckEquipName.deallocate();
		Baseboard.deallocate();
		BaseboardNumericFields.deallocate();
	}

	void
	SimElectricBaseboard(
		std::string const & EquipName,
		int const ActualZoneNum,
		int const ControlledZoneNum,
		Real64 & PowerMet,
		int & CompIndex
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   Nov 2001
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine simulates the Electric Baseboard units.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using DataZoneEnergyDemands::ZoneSysEnergyDemand;
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

		int BaseboardNum; // index of unit in baseboard array
		static bool GetInputFlag( true ); // one time get input flag
		Real64 QZnReq; // zone load not yet satisfied

		if ( GetInputFlag ) {
			GetBaseboardInput();
			GetInputFlag = false;
		}

		// Find the correct Baseboard Equipment
		if ( CompIndex == 0 ) {
			BaseboardNum = FindItemInList( EquipName, Baseboard, &BaseboardParams::EquipName );
			if ( BaseboardNum == 0 ) {
				ShowFatalError( "SimElectricBaseboard: Unit not found=" + EquipName );
			}
			CompIndex = BaseboardNum;
		} else {
			BaseboardNum = CompIndex;
			if ( BaseboardNum > NumBaseboards || BaseboardNum < 1 ) {
				ShowFatalError( "SimElectricBaseboard:  Invalid CompIndex passed=" + TrimSigDigits( BaseboardNum ) + ", Number of Units=" + TrimSigDigits( NumBaseboards ) + ", Entered Unit name=" + EquipName );
			}
			if ( CheckEquipName( BaseboardNum ) ) {
				if ( EquipName != Baseboard( BaseboardNum ).EquipName ) {
					ShowFatalError( "SimElectricBaseboard: Invalid CompIndex passed=" + TrimSigDigits( BaseboardNum ) + ", Unit name=" + EquipName + ", stored Unit Name for that index=" + Baseboard( BaseboardNum ).EquipName );
				}
				CheckEquipName( BaseboardNum ) = false;
			}
		}

		InitBaseboard( BaseboardNum, ControlledZoneNum );

		QZnReq = ZoneSysEnergyDemand( ActualZoneNum ).RemainingOutputReqToHeatSP;

		// Simulate baseboard
		SimElectricConvective( BaseboardNum, QZnReq );

		PowerMet = Baseboard( BaseboardNum ).Power;

		ReportBaseboard( BaseboardNum );

	}

	void
	GetBaseboardInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   Nov 2001
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine gets the input for the Baseboard units.

		// METHODOLOGY EMPLOYED:
		// Standard input processor calls.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::MakeUPPERCase;
		using InputProcessor::SameString;
		using GlobalNames::VerifyUniqueBaseboardName;
		using namespace DataIPShortCuts;
		using General::TrimSigDigits;
		using DataSizing::HeatingDesignCapacity;
		using DataSizing::CapacityPerFloorArea;
		using DataSizing::FractionOfAutosizedHeatingCapacity;
		using DataSizing::AutoSize;
		using DataGlobals::NumOfZones;
		using DataZoneEquipment::ZoneEquipList;
		using DataZoneEquipment::BBElectricConvective_Num;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetBaseboardInput: " ); // include trailing blank space
		int const iHeatCAPMAlphaNum( 3 ); // get input index to baseboard heating capacity sizing method
		int const iHeatDesignCapacityNumericNum( 1 ); // get input index to baseboard heating capacity
		int const iHeatCapacityPerFloorAreaNumericNum( 2 ); // get input index to baseboard heating capacity per floor area sizing
		int const iHeatFracOfAutosizedCapacityNumericNum( 3 ); //  get input index to baseboard heating capacity sizing as fraction of autozized heating capacity

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int BaseboardNum;
		int NumConvElecBaseboards;
		int ConvElecBBNum;
		int NumAlphas;
		int NumNums;
		int IOStat;
		static bool ErrorsFound( false ); // If errors detected in input
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		bool errFlag;

		int CtrlZone;   // index to constrolled zone number
		int ZoneEquipTypeNum; // index to zone equipment in a zone equipment list

		cCurrentModuleObject = cCMO_BBRadiator_Electric;

		NumConvElecBaseboards = GetNumObjectsFound( cCurrentModuleObject );

		// Calculate total number of baseboard units
		NumBaseboards = NumConvElecBaseboards;

		Baseboard.allocate( NumBaseboards );
		CheckEquipName.allocate( NumBaseboards );
		BaseboardNumericFields.allocate( NumBaseboards );
		CheckEquipName = true;

		if ( NumConvElecBaseboards > 0 ) { //Get the data for cooling schemes
			BaseboardNum = 0;
			for ( ConvElecBBNum = 1; ConvElecBBNum <= NumConvElecBaseboards; ++ConvElecBBNum ) {

				GetObjectItem( cCurrentModuleObject, ConvElecBBNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

				BaseboardNumericFields( ConvElecBBNum ).FieldNames.allocate( NumNums);
				BaseboardNumericFields( ConvElecBBNum ).FieldNames = "";
				BaseboardNumericFields( ConvElecBBNum ).FieldNames = cNumericFieldNames;

				IsNotOK = false;
				IsBlank = false;
				VerifyName( cAlphaArgs( 1 ), Baseboard, &BaseboardParams::EquipName, BaseboardNum, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					continue;
				}
				VerifyUniqueBaseboardName( cCurrentModuleObject, cAlphaArgs( 1 ), errFlag, cCurrentModuleObject + " Name" );
				if ( errFlag ) {
					ErrorsFound = true;
				}
				++BaseboardNum;
				Baseboard( BaseboardNum ).EquipName = cAlphaArgs( 1 ); // name of this baseboard
				Baseboard( BaseboardNum ).EquipType = MakeUPPERCase( cCurrentModuleObject ); // the type of baseboard-rename change
				Baseboard( BaseboardNum ).Schedule = cAlphaArgs( 2 );
				if ( lAlphaFieldBlanks( 2 ) ) {
					Baseboard( BaseboardNum ).SchedPtr = ScheduleAlwaysOn;
				} else {
					Baseboard( BaseboardNum ).SchedPtr = GetScheduleIndex( cAlphaArgs( 2 ) );
					if ( Baseboard( BaseboardNum ).SchedPtr == 0 ) {
						ShowSevereError( RoutineName + cCurrentModuleObject + ": invalid " + cAlphaFieldNames( 2 ) + " entered =" + cAlphaArgs( 2 ) + " for " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					}
				}
				// get inlet node number
				Baseboard( BaseboardNum ).BaseboardEfficiency = rNumericArgs( 4 );

				// Determine baseboard electric heating design capacity sizing method
				if ( SameString( cAlphaArgs( iHeatCAPMAlphaNum ), "HeatingDesignCapacity" ) ) {
					Baseboard( BaseboardNum ).HeatingCapMethod = HeatingDesignCapacity;
					if ( !lNumericFieldBlanks( iHeatDesignCapacityNumericNum ) ) {
						Baseboard( BaseboardNum ).ScaledHeatingCapacity = rNumericArgs( iHeatDesignCapacityNumericNum );
						if ( Baseboard( BaseboardNum ).ScaledHeatingCapacity < 0.0 && Baseboard( BaseboardNum ).ScaledHeatingCapacity != AutoSize ) {
							ShowSevereError( cCurrentModuleObject + " = " + Baseboard( BaseboardNum ).EquipName );
							ShowContinueError( "Illegal " + cNumericFieldNames( iHeatDesignCapacityNumericNum ) + " = " + TrimSigDigits( rNumericArgs( iHeatDesignCapacityNumericNum ), 7 ) );
							ErrorsFound = true;
						}
					} else {
						ShowSevereError( cCurrentModuleObject + " = " + Baseboard( BaseboardNum ).EquipName );
						ShowContinueError( "Input for " + cAlphaFieldNames( iHeatCAPMAlphaNum ) + " = " + cAlphaArgs( iHeatCAPMAlphaNum ) );
						ShowContinueError( "Blank field not allowed for " + cNumericFieldNames( iHeatDesignCapacityNumericNum ) );
						ErrorsFound = true;
					}
				} else if ( SameString( cAlphaArgs( iHeatCAPMAlphaNum ), "CapacityPerFloorArea" ) ) {
					Baseboard( BaseboardNum ).HeatingCapMethod = CapacityPerFloorArea;
					if ( !lNumericFieldBlanks( iHeatCapacityPerFloorAreaNumericNum ) ) {
						Baseboard( BaseboardNum ).ScaledHeatingCapacity = rNumericArgs( iHeatCapacityPerFloorAreaNumericNum );
						if ( Baseboard( BaseboardNum ).ScaledHeatingCapacity <= 0.0 ) {
							ShowSevereError( cCurrentModuleObject + " = " + Baseboard( BaseboardNum ).EquipName );
							ShowContinueError( "Input for " + cAlphaFieldNames( iHeatCAPMAlphaNum ) + " = " + cAlphaArgs( iHeatCAPMAlphaNum ) );
							ShowContinueError( "Illegal " + cNumericFieldNames( iHeatCapacityPerFloorAreaNumericNum ) + " = " + TrimSigDigits( rNumericArgs( iHeatCapacityPerFloorAreaNumericNum ), 7 ) );
							ErrorsFound = true;
						} else if ( Baseboard( BaseboardNum ).ScaledHeatingCapacity == AutoSize ) {
							ShowSevereError( cCurrentModuleObject + " = " + Baseboard( BaseboardNum ).EquipName );
							ShowContinueError( "Input for " + cAlphaFieldNames( iHeatCAPMAlphaNum ) + " = " + cAlphaArgs( iHeatCAPMAlphaNum ) );
							ShowContinueError( "Illegal " + cNumericFieldNames( iHeatCapacityPerFloorAreaNumericNum ) + " = Autosize" );
							ErrorsFound = true;
						}
					} else {
						ShowSevereError( cCurrentModuleObject + " = " + Baseboard( BaseboardNum ).EquipName );
						ShowContinueError( "Input for " + cAlphaFieldNames( iHeatCAPMAlphaNum ) + " = " + cAlphaArgs( iHeatCAPMAlphaNum ) );
						ShowContinueError( "Blank field not allowed for " + cNumericFieldNames( iHeatCapacityPerFloorAreaNumericNum ) );
						ErrorsFound = true;
					}
				} else if ( SameString( cAlphaArgs( iHeatCAPMAlphaNum ), "FractionOfAutosizedHeatingCapacity" ) ) {
					Baseboard( BaseboardNum ).HeatingCapMethod = FractionOfAutosizedHeatingCapacity;
					if ( !lNumericFieldBlanks( iHeatFracOfAutosizedCapacityNumericNum ) ) {
						Baseboard( BaseboardNum ).ScaledHeatingCapacity = rNumericArgs( iHeatFracOfAutosizedCapacityNumericNum );
						if ( Baseboard( BaseboardNum ).ScaledHeatingCapacity < 0.0 ) {
							ShowSevereError( cCurrentModuleObject + " = " + Baseboard( BaseboardNum ).EquipName );
							ShowContinueError( "Illegal " + cNumericFieldNames( iHeatFracOfAutosizedCapacityNumericNum ) + " = " + TrimSigDigits( rNumericArgs( iHeatFracOfAutosizedCapacityNumericNum ), 7 ) );
							ErrorsFound = true;
						}
					} else {
						ShowSevereError( cCurrentModuleObject + " = " + Baseboard( BaseboardNum ).EquipName );
						ShowContinueError( "Input for " + cAlphaFieldNames( iHeatCAPMAlphaNum ) + " = " + cAlphaArgs( iHeatCAPMAlphaNum ) );
						ShowContinueError( "Blank field not allowed for " + cNumericFieldNames( iHeatFracOfAutosizedCapacityNumericNum ) );
						ErrorsFound = true;
					}
				} else {
					ShowSevereError( cCurrentModuleObject + " = " + Baseboard( BaseboardNum ).EquipName );
					ShowContinueError( "Illegal " + cAlphaFieldNames( iHeatCAPMAlphaNum ) + " = " + cAlphaArgs( iHeatCAPMAlphaNum ) );
					ErrorsFound = true;
				}

				for ( CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone ) {
					for ( ZoneEquipTypeNum = 1; ZoneEquipTypeNum <= ZoneEquipList( CtrlZone ).NumOfEquipTypes; ++ZoneEquipTypeNum ) {
						if ( ZoneEquipList( CtrlZone ).EquipType_Num( ZoneEquipTypeNum ) == BBElectricConvective_Num && ZoneEquipList( CtrlZone ).EquipName( ZoneEquipTypeNum ) == Baseboard( BaseboardNum ).EquipName ) {
							Baseboard( BaseboardNum ).ZonePtr = CtrlZone;
						}
					}
				}
			}

			if ( ErrorsFound ) {
				ShowFatalError( RoutineName + "Errors found in getting input.  Preceding condition(s) cause termination." );
			}
		}

		for ( BaseboardNum = 1; BaseboardNum <= NumBaseboards; ++BaseboardNum ) {

			// Setup Report variables for the Electric Baseboards
			// CurrentModuleObject='ZoneHVAC:Baseboard:Convective:Electric'
			SetupOutputVariable( "Baseboard Total Heating Energy [J]", Baseboard( BaseboardNum ).Energy, "System", "Sum", Baseboard( BaseboardNum ).EquipName, _, "ENERGYTRANSFER", "BASEBOARD", _, "System" );

			SetupOutputVariable( "Baseboard Total Heating Rate [W]", Baseboard( BaseboardNum ).Power, "System", "Average", Baseboard( BaseboardNum ).EquipName );

			SetupOutputVariable( "Baseboard Electric Energy [J]", Baseboard( BaseboardNum ).ElecUseLoad, "System", "Sum", Baseboard( BaseboardNum ).EquipName, _, "Electric", "HEATING", _, "System" );

			SetupOutputVariable( "Baseboard Electric Power [W]", Baseboard( BaseboardNum ).ElecUseRate, "System", "Average", Baseboard( BaseboardNum ).EquipName );

		}

	}

	void
	InitBaseboard(
		int const BaseboardNum,
		int const ControlledZoneNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   Nov 2001
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine initializes the Baseboard units during simulation.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataLoopNode::Node;
		using DataZoneEquipment::ZoneEquipInputsFilled;
		using DataZoneEquipment::CheckZoneEquipmentList;
		using DataZoneEquipment::ZoneEquipConfig;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ZoneNode;
		static bool MyOneTimeFlag( true );
		static bool ZoneEquipmentListChecked( false ); // True after the Zone Equipment List has been checked for items
		int Loop;
		static Array1D_bool MyEnvrnFlag;

		// Do the one time initializations
		if ( MyOneTimeFlag ) {
			// initialize the environment and sizing flags
			MyEnvrnFlag.allocate( NumBaseboards );
			MySizeFlag.allocate( NumBaseboards );
			MyEnvrnFlag = true;
			MySizeFlag = true;

			MyOneTimeFlag = false;

		}

		// need to check all units to see if they are on ZoneHVAC:EquipmentList or issue warning
		if ( ! ZoneEquipmentListChecked && ZoneEquipInputsFilled ) {
			ZoneEquipmentListChecked = true;
			for ( Loop = 1; Loop <= NumBaseboards; ++Loop ) {
				if ( CheckZoneEquipmentList( Baseboard( Loop ).EquipType, Baseboard( Loop ).EquipName ) ) continue;
				ShowSevereError( "InitBaseboard: Unit=[" + Baseboard( Loop ).EquipType + ',' + Baseboard( Loop ).EquipName + "] is not on any ZoneHVAC:EquipmentList.  It will not be simulated." );
			}
		}

		if ( ! SysSizingCalc && MySizeFlag( BaseboardNum ) ) {
			// for each coil, do the sizing once.
			SizeElectricBaseboard( BaseboardNum );

			MySizeFlag( BaseboardNum ) = false;
		}

		// Set the reporting variables to zero at each timestep.
		Baseboard( BaseboardNum ).Energy = 0.0;
		Baseboard( BaseboardNum ).Power = 0.0;
		Baseboard( BaseboardNum ).ElecUseLoad = 0.0;
		Baseboard( BaseboardNum ).ElecUseRate = 0.0;

		// Do the every time step initializations
		ZoneNode = ZoneEquipConfig( ControlledZoneNum ).ZoneNode;
		Baseboard( BaseboardNum ).AirInletTemp = Node( ZoneNode ).Temp;
		Baseboard( BaseboardNum ).AirInletHumRat = Node( ZoneNode ).HumRat;

	}

	void
	SizeElectricBaseboard( int const BaseboardNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   February 2002
		//       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
		//                      July 2014, B. Nigusse, added scalable sizing
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing electric baseboard components for which nominal capacities have not been
		// specified in the input.

		// METHODOLOGY EMPLOYED:
		// Obtains flow rates from the zone sizing arrays and plant sizing data. UAs are
		// calculated by numerically inverting the baseboard calculation routine.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSizing;
		using ReportSizingManager::RequestSizing;
		using ReportSizingManager::ReportSizingOutput;
		using General::RoundSigDigits;
		using DataHVACGlobals::HeatingCapacitySizing;
		using DataHeatBalance::Zone;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName("SizeElectricBaseboard");

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		bool IsAutoSize; // Indicator to autosizing nominal capacity
		Real64 NominalCapacityDes; // Design nominal capacity for reporting
		Real64 NominalCapacityUser; // User hard-sized nominal capacity for reporting

		std::string CompName; // component name
		std::string CompType; // component type
		std::string SizingString; // input field sizing description (e.g., Nominal Capacity)
		Real64 TempSize; // autosized value of coil input field
		int FieldNum = 1; // IDD numeric field number where input field description is found
		int SizingMethod; // Integer representation of sizing method name (e.g., CoolingAirflowSizing, HeatingAirflowSizing, CoolingCapacitySizing, HeatingCapacitySizing, etc.)
		bool PrintFlag; // TRUE when sizing information is reported in the eio file
		int CapSizingMethod( 0 ); // capacity sizing methods (HeatingDesignCapacity, CapacityPerFloorArea, FractionOfAutosizedCoolingCapacity, and FractionOfAutosizedHeatingCapacity )

		IsAutoSize = false;
		NominalCapacityDes = 0.0;
		NominalCapacityUser = 0.0;
		DataScalableCapSizingON = false;

		if ( CurZoneEqNum > 0 ) {

			CompType = Baseboard( BaseboardNum ).EquipType;
			CompName = Baseboard( BaseboardNum ).EquipName;
			DataFracOfAutosizedHeatingCapacity = 1.0;
			DataZoneNumber = Baseboard( BaseboardNum ).ZonePtr;
			SizingMethod = HeatingCapacitySizing;
			FieldNum = 1;
			PrintFlag = true;
			SizingString = BaseboardNumericFields(BaseboardNum).FieldNames(FieldNum) + " [W]";
			CapSizingMethod = Baseboard( BaseboardNum ).HeatingCapMethod;
			ZoneEqSizing(CurZoneEqNum).SizingMethod(SizingMethod) = CapSizingMethod;
			if ( CapSizingMethod == HeatingDesignCapacity || CapSizingMethod == CapacityPerFloorArea || CapSizingMethod == FractionOfAutosizedHeatingCapacity ) {
				if ( CapSizingMethod == HeatingDesignCapacity ) {
					if ( Baseboard( BaseboardNum ).ScaledHeatingCapacity == AutoSize ) {
						CheckZoneSizing(CompType, CompName);
						ZoneEqSizing( CurZoneEqNum ).HeatingCapacity = true;
						ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = CalcFinalZoneSizing( CurZoneEqNum ).DesHeatLoad * CalcFinalZoneSizing( CurZoneEqNum ).HeatSizingFactor;
					}
					TempSize = Baseboard( BaseboardNum ).ScaledHeatingCapacity;
				} else if ( CapSizingMethod == CapacityPerFloorArea ) {
					ZoneEqSizing( CurZoneEqNum ).HeatingCapacity = true;
					ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = Baseboard( BaseboardNum ).ScaledHeatingCapacity * Zone( DataZoneNumber ).FloorArea;
					TempSize = ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad;
					DataScalableCapSizingON = true;
				} else if ( CapSizingMethod == FractionOfAutosizedHeatingCapacity ) {
					CheckZoneSizing( CompType, CompName );
					ZoneEqSizing( CurZoneEqNum ).HeatingCapacity = true;
					DataFracOfAutosizedHeatingCapacity = Baseboard( BaseboardNum ).ScaledHeatingCapacity;
					ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = CalcFinalZoneSizing( CurZoneEqNum ).DesHeatLoad * CalcFinalZoneSizing(CurZoneEqNum).HeatSizingFactor;
					TempSize = AutoSize;
					DataScalableCapSizingON = true;
				} else {
					TempSize = Baseboard( BaseboardNum ).ScaledHeatingCapacity;
				}
				RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
				Baseboard( BaseboardNum ).NominalCapacity = TempSize;
				DataScalableCapSizingON = false;
			}
		}
	}

	void
	SimElectricConvective(
		int const BaseboardNum,
		Real64 const LoadMet
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   Nov 2001
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE: This subroutine calculates the heat exchange rate
		// in a pure Electricconvective baseboard heater.

		// METHODOLOGY EMPLOYED:
		// Currently this is primarily modified from HW Convective baseboard which has connections to
		//  a water loop and was necessary to calculate temps, flow rates and other things.  This
		//  model might be made more sophisticated and might use some of those data structures in the future
		//  so they are left in place even though this model does not utilize them.

		// REFERENCES:

		// USE STATEMENTS:
		//unused0909    USE DataEnvironment, ONLY: OutBaroPress
		// Using/Aliasing
		using DataLoopNode::Node;
		using Psychrometrics::PsyCpAirFnWTdb;
		using DataHVACGlobals::SmallLoad;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 AirInletTemp;
		Real64 CpAir;
		Real64 AirMassFlowRate;
		Real64 CapacitanceAir;
		Real64 Effic;
		Real64 AirOutletTemp;
		Real64 QBBCap;

		AirInletTemp = Baseboard( BaseboardNum ).AirInletTemp;
		AirOutletTemp = AirInletTemp;
		CpAir = PsyCpAirFnWTdb( Baseboard( BaseboardNum ).AirInletHumRat, AirInletTemp );
		AirMassFlowRate = SimpConvAirFlowSpeed;
		CapacitanceAir = CpAir * AirMassFlowRate;
		// currently only the efficiency is used to calculate the electric consumption.  There could be some
		//  thermal loss that could be accounted for with this efficiency input.
		Effic = Baseboard( BaseboardNum ).BaseboardEfficiency;

		if ( GetCurrentScheduleValue( Baseboard( BaseboardNum ).SchedPtr ) > 0.0 && LoadMet >= SmallLoad ) {

			// if the load exceeds the capacity than the capacity is set to the BB limit.
			if ( LoadMet > Baseboard( BaseboardNum ).NominalCapacity ) {
				QBBCap = Baseboard( BaseboardNum ).NominalCapacity;
			} else {
				QBBCap = LoadMet;
			}

			// this could be utilized somehow or even reported so the data structures are left in place
			AirOutletTemp = AirInletTemp + QBBCap / CapacitanceAir;

			//The Baseboard electric Load is calculated using the efficiency
			Baseboard( BaseboardNum ).ElecUseRate = QBBCap / Effic;

		} else {
			//if there is an off condition the BB does nothing.
			AirOutletTemp = AirInletTemp;
			QBBCap = 0.0;
			Baseboard( BaseboardNum ).ElecUseRate = 0.0;
		}

		Baseboard( BaseboardNum ).AirOutletTemp = AirOutletTemp;
		Baseboard( BaseboardNum ).Power = QBBCap;

	}

	void
	ReportBaseboard( int const BaseboardNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   Nov 2001
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE: This subroutine

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHVACGlobals::TimeStepSys;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		Baseboard( BaseboardNum ).Energy = Baseboard( BaseboardNum ).Power * TimeStepSys * SecInHour;
		Baseboard( BaseboardNum ).ElecUseLoad = Baseboard( BaseboardNum ).ElecUseRate * TimeStepSys * SecInHour;

	}

} // BaseboardElectric

} // EnergyPlus
