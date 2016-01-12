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
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <ZoneDehumidifier.hh>
#include <CurveManager.hh>
#include <DataContaminantBalance.hh>
#include <DataEnvironment.hh>
#include <DataHVACGlobals.hh>
#include <DataLoopNode.hh>
#include <DataPrecisionGlobals.hh>
#include <DataWater.hh>
#include <DataZoneEnergyDemands.hh>
#include <DataZoneEquipment.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <Psychrometrics.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>
#include <WaterManager.hh>

namespace EnergyPlus {

namespace ZoneDehumidifier {

	// Module containing the routines dealing with the ZoneDehumidifier

	// MODULE INFORMATION:
	//       AUTHOR         Don Shirey, FSEC
	//       DATE WRITTEN   July/Aug 2009
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// Calculate the performance of zone (room) air dehumidifiers.  Meant to model
	// conventional direct expansion (DX) cooling-based room air dehumidifiers
	// (reject 100% of condenser heat to the zone air), but the approach
	// might be able to be used to model other room air dehumidifier types.

	// METHODOLOGY EMPLOYED:
	// Model as a piece of zone equipment, with inputs for water removal and
	// energy factor at rated conditions (26.7C, 60% RH). Then provide curve objects
	// to describe performance at off-rated conditions. A part-load cycling curve
	// input is also provided. It is assumed that this equipment dehumidifies but
	// heats the air. If used in tandem with another system that cools and dehumidifies,
	// then the zone dehumidifier should be specified as the lowest cooling priority
	// in the ZoneHVAC:EquipmentList object. The cooling and dehumidification system
	// operates first to meet the temperature setpoint (and possibly the high humidity
	// setpoint as well). If additional dehumidification is needed, then the zone
	// dehumidifier operates. The excess sensible heat generated by the dehumidifier
	// is carried over to the next HVAC time step.

	// REFERENCES:
	// na

	// OTHER NOTES:
	// Example manufacturer's data at:
	//   http://www.thermastor.com/HI-E-DRY-100/HI-E-DRY-100-Spec.pdf
	//   http://www.thermastor.com/HI-E-DRY-195/HI-E-DRY-195-Spec.pdf

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataLoopNode;
	using DataGlobals::BeginEnvrnFlag;
	using DataGlobals::SecInHour;
	using DataGlobals::ScheduleAlwaysOn;
	using DataEnvironment::OutBaroPress;
	using DataEnvironment::StdBaroPress;
	using General::TrimSigDigits;
	using namespace ScheduleManager;

	// Data
	// MODULE PARAMETER DEFINITIONS:

	// Unit type index
	int const ZoneDehumidUnit( 1 ); // 1 is the index for ZoneHVAC:Dehumidifier:DX

	// Water Systems
	int const CondensateDiscarded( 1001 ); // Default mode where water is "lost"
	int const CondensateToTank( 1002 ); // Collect coil condensate from air and store in water storage tank

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:

	int NumDehumidifiers( 0 ); // Number of zone dehumidifier objects in the input file

	bool GetInputFlag( true ); // Set to FALSE after first time input is "gotten"
	Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE:
	// Driver/Manager Routines

	// Get Input routines for module

	// Initialization routines for module

	// Algorithms/Calculation routines for the module

	// Update routine to update node information

	// Reporting routines for module

	// Get either inlet or outlet node number

	// Object Data
	Array1D< ZoneDehumidifierData > ZoneDehumid;

	// Functions

	void
	clear_state()
	{
		NumDehumidifiers = 0;
		GetInputFlag = true;
		CheckEquipName.deallocate();
		ZoneDehumid.deallocate();
	}

	void
	SimZoneDehumidifier(
		std::string const & CompName, // Name of the zone dehumidifier
		int const ZoneNum, // Number of zone being served
		bool const EP_UNUSED( FirstHVACIteration ), // TRUE if 1st HVAC simulation of system timestep
		Real64 & QSensOut, // Sensible capacity delivered to zone (W)
		Real64 & QLatOut, // Latent capacity delivered to zone (kg/s), dehumidify = negative
		int & CompIndex // Index to the zone dehumidifier
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Don Shirey, FSEC
		//       DATE WRITTEN   July/Aug 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Simulate a zone dehumidifier.

		// METHODOLOGY EMPLOYED:
		// Call appropriate subroutines to get input values, initialize variables, model performanc
		// update node information, report model outputs.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using DataZoneEnergyDemands::ZoneSysMoistureDemand;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ZoneDehumidNum; // Index of zone dehumidifier being simulated
		Real64 QZnDehumidReq; // Zone dehumidification load required (kg moisture/sec)

		if ( GetInputFlag ) {
			GetZoneDehumidifierInput();
			GetInputFlag = false;
		}

		// Find the correct zone dehumidifier
		if ( CompIndex == 0 ) {
			ZoneDehumidNum = FindItemInList( CompName, ZoneDehumid );
			if ( ZoneDehumidNum == 0 ) {
				ShowFatalError( "SimZoneDehumidifier: Unit not found= " + CompName );
			}
			CompIndex = ZoneDehumidNum;
		} else {
			ZoneDehumidNum = CompIndex;
			if ( ZoneDehumidNum > NumDehumidifiers || ZoneDehumidNum < 1 ) {
				ShowFatalError( "SimZoneDehumidifier:  Invalid CompIndex passed= " + TrimSigDigits( ZoneDehumidNum ) + ", Number of Units= " + TrimSigDigits( NumDehumidifiers ) + ", Entered Unit name= " + CompName );
			}
			if ( CheckEquipName( ZoneDehumidNum ) ) {
				if ( CompName != ZoneDehumid( ZoneDehumidNum ).Name ) {
					ShowFatalError( "SimZoneDehumidifier: Invalid CompIndex passed=" + TrimSigDigits( ZoneDehumidNum ) + ", Unit name= " + CompName + ", stored Unit Name for that index= " + ZoneDehumid( ZoneDehumidNum ).Name );
				}
				CheckEquipName( ZoneDehumidNum ) = false;
			}
		}

		QZnDehumidReq = ZoneSysMoistureDemand( ZoneNum ).RemainingOutputReqToDehumidSP; // Negative means dehumidify

		InitZoneDehumidifier( ZoneDehumidNum );

		CalcZoneDehumidifier( ZoneDehumidNum, QZnDehumidReq, QSensOut, QLatOut );

		UpdateZoneDehumidifier( ZoneDehumidNum );

		ReportZoneDehumidifier( ZoneDehumidNum );

	}

	void
	GetZoneDehumidifierInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Don Shirey, FSEC
		//       DATE WRITTEN   July/Aug 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Retrieve the inputs from the input data file (idf) being simulated.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology using available utility routines where appropriate.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::GetObjectDefMaxArgs;
		using NodeInputManager::GetOnlySingleNode;
		using CurveManager::GetCurveIndex;
		using CurveManager::GetCurveType;
		using CurveManager::CurveValue;
		using WaterManager::SetupTankSupplyComponent;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetZoneDehumidifierInput" );
		static std::string const CurrentModuleObject( "ZoneHVAC:Dehumidifier:DX" );
		// Curve Types
		int const Quadratic( 1 );
		int const BiQuadratic( 2 );
		int const Cubic( 3 );
		Real64 const RatedInletAirTemp( 26.7 );
		Real64 const RatedInletAirRH( 60.0 );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ZoneDehumidIndex; // Loop index
		static int NumAlphas( 0 ); // Number of Alphas to allocate arrays, then used for each GetObjectItem call
		static int NumNumbers( 0 ); // Number of Numbers to allocate arrays, then used for each GetObjectItem call
		int IOStatus; // Used in GetObjectItem
		static bool ErrorsFound( false ); // Set to true if errors in input, fatal at end of routine
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		Array1D_string Alphas; // Alpha input items for object
		Array1D_string cAlphaFields; // Alpha field names
		Array1D_string cNumericFields; // Numeric field names
		Array1D< Real64 > Numbers; // Numeric input items for object
		Array1D_bool lAlphaBlanks; // Logical array, alpha field input BLANK = .TRUE.
		Array1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.
		static int TotalArgs( 0 ); // Total number of alpha and numeric arguments (max)
		Real64 CurveVal; // Output from curve object (water removal or energy factor curves)

		NumDehumidifiers = GetNumObjectsFound( CurrentModuleObject );

		ZoneDehumid.allocate( NumDehumidifiers );
		CheckEquipName.dimension( NumDehumidifiers, true );

		GetObjectDefMaxArgs( CurrentModuleObject, TotalArgs, NumAlphas, NumNumbers );

		Alphas.allocate( NumAlphas );
		cAlphaFields.allocate( NumAlphas );
		cNumericFields.allocate( NumNumbers );
		Numbers.dimension( NumNumbers, 0.0 );
		lAlphaBlanks.dimension( NumAlphas, true );
		lNumericBlanks.dimension( NumNumbers, true );

		for ( ZoneDehumidIndex = 1; ZoneDehumidIndex <= NumDehumidifiers; ++ZoneDehumidIndex ) {

			GetObjectItem( CurrentModuleObject, ZoneDehumidIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), ZoneDehumid, ZoneDehumidIndex - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}

			// A1,  \field Name
			ZoneDehumid( ZoneDehumidIndex ).Name = Alphas( 1 );
			ZoneDehumid( ZoneDehumidIndex ).UnitType = CurrentModuleObject; // 'ZoneHVAC:Dehumidifier:DX'
			ZoneDehumid( ZoneDehumidIndex ).UnitType_Num = ZoneDehumidUnit; // 'ZoneHVAC:Dehumidifier:DX' = 1

			// A2,  \field Availability Schedule Name
			if ( lAlphaBlanks( 2 ) ) {
				ZoneDehumid( ZoneDehumidIndex ).SchedPtr = ScheduleAlwaysOn;
			} else {
				ZoneDehumid( ZoneDehumidIndex ).SchedPtr = GetScheduleIndex( Alphas( 2 ) ); // Convert schedule name to pointer
				if ( ZoneDehumid( ZoneDehumidIndex ).SchedPtr == 0 ) {
					ShowSevereError( cAlphaFields( 2 ) + " not found = " + Alphas( 2 ) );
					ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + ZoneDehumid( ZoneDehumidIndex ).Name );
					ErrorsFound = true;
				}
			}

			// A3 , \field Air Inlet Node Name
			ZoneDehumid( ZoneDehumidIndex ).AirInletNodeNum = GetOnlySingleNode( Alphas( 3 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

			// A4 , \field Air Outlet Node Name
			ZoneDehumid( ZoneDehumidIndex ).AirOutletNodeNum = GetOnlySingleNode( Alphas( 4 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			// N1,  \field Rated Water Removal
			ZoneDehumid( ZoneDehumidIndex ).RatedWaterRemoval = Numbers( 1 );
			if ( ZoneDehumid( ZoneDehumidIndex ).RatedWaterRemoval <= 0.0 ) {
				ShowSevereError( cNumericFields( 1 ) + " must be greater than zero." );
				ShowContinueError( "Value specified = " + TrimSigDigits( Numbers( 1 ), 5 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + ZoneDehumid( ZoneDehumidIndex ).Name );
				ErrorsFound = true;
			}

			// N2,  \field Rated Energy Factor
			ZoneDehumid( ZoneDehumidIndex ).RatedEnergyFactor = Numbers( 2 );
			if ( ZoneDehumid( ZoneDehumidIndex ).RatedEnergyFactor <= 0.0 ) {
				ShowSevereError( cNumericFields( 2 ) + " must be greater than zero." );
				ShowContinueError( "Value specified = " + TrimSigDigits( Numbers( 2 ), 5 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + ZoneDehumid( ZoneDehumidIndex ).Name );
				ErrorsFound = true;
			}

			// N3,  \field Rated Air Flow Rate
			ZoneDehumid( ZoneDehumidIndex ).RatedAirVolFlow = Numbers( 3 );
			if ( ZoneDehumid( ZoneDehumidIndex ).RatedAirVolFlow <= 0.0 ) {
				ShowSevereError( cNumericFields( 3 ) + " must be greater than zero." );
				ShowContinueError( "Value specified = " + TrimSigDigits( Numbers( 3 ), 5 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + ZoneDehumid( ZoneDehumidIndex ).Name );
				ErrorsFound = true;
			}

			// A5,  \field Water Removal Curve Name
			ZoneDehumid( ZoneDehumidIndex ).WaterRemovalCurveIndex = GetCurveIndex( Alphas( 5 ) ); // Convert curve name to index number
			if ( ZoneDehumid( ZoneDehumidIndex ).WaterRemovalCurveIndex == 0 ) {
				if ( lAlphaBlanks( 5 ) ) {
					ShowSevereError( RoutineName + ':' + CurrentModuleObject + "=\"" + cAlphaFields( 5 ) + "\" is required, missing for " + cAlphaFields( 1 ) + " = " + ZoneDehumid( ZoneDehumidIndex ).Name );
				} else {
					ShowSevereError( cAlphaFields( 5 ) + " not found = " + Alphas( 5 ) );
					ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + ZoneDehumid( ZoneDehumidIndex ).Name );
				}
				ErrorsFound = true;
			} else {
				// Verify Curve object, only legal type is BiQuadratic
				{ auto const SELECT_CASE_var( GetCurveType( ZoneDehumid( ZoneDehumidIndex ).WaterRemovalCurveIndex ) );
				if ( SELECT_CASE_var == "BIQUADRATIC" ) {
					ZoneDehumid( ZoneDehumidIndex ).WaterRemovalCurveType = BiQuadratic;
					CurveVal = CurveValue( ZoneDehumid( ZoneDehumidIndex ).WaterRemovalCurveIndex, RatedInletAirTemp, RatedInletAirRH );
					if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
						ShowWarningError( cAlphaFields( 5 ) + " output is not equal to 1.0" );
						ShowContinueError( "(+ or -10%) at rated conditions for " + CurrentModuleObject + " = " + Alphas( 1 ) );
						ShowContinueError( "Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
					}

				} else {
					ShowSevereError( RoutineName + ':' + CurrentModuleObject + "=\"" + ZoneDehumid( ZoneDehumidIndex ).Name + "\" illegal " + cAlphaFields( 5 ) + " type for this object = " + GetCurveType( ZoneDehumid( ZoneDehumidIndex ).WaterRemovalCurveIndex ) );
					ShowContinueError( "Curve type must be BiQuadratic." );
					ErrorsFound = true;
				}}
			}

			// A6,  \field Energy Factor Curve Name
			ZoneDehumid( ZoneDehumidIndex ).EnergyFactorCurveIndex = GetCurveIndex( Alphas( 6 ) ); // convert curve name to number
			if ( ZoneDehumid( ZoneDehumidIndex ).EnergyFactorCurveIndex == 0 ) {
				if ( lAlphaBlanks( 6 ) ) {
					ShowSevereError( RoutineName + ':' + CurrentModuleObject + "=\"" + cAlphaFields( 6 ) + "\" is required, missing for " + cAlphaFields( 1 ) + " = " + ZoneDehumid( ZoneDehumidIndex ).Name );
				} else {
					ShowSevereError( cAlphaFields( 6 ) + " not found = " + Alphas( 6 ) );
					ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + ZoneDehumid( ZoneDehumidIndex ).Name );
				}
				ErrorsFound = true;
			} else {
				// Verify Curve Object, only legal type is BiQuadratic
				{ auto const SELECT_CASE_var( GetCurveType( ZoneDehumid( ZoneDehumidIndex ).EnergyFactorCurveIndex ) );
				if ( SELECT_CASE_var == "BIQUADRATIC" ) {
					ZoneDehumid( ZoneDehumidIndex ).EnergyFactorCurveType = BiQuadratic;
					CurveVal = CurveValue( ZoneDehumid( ZoneDehumidIndex ).EnergyFactorCurveIndex, RatedInletAirTemp, RatedInletAirRH );
					if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
						ShowWarningError( cAlphaFields( 6 ) + " output is not equal to 1.0" );
						ShowContinueError( "(+ or -10%) at rated conditions for " + CurrentModuleObject + " = " + Alphas( 1 ) );
						ShowContinueError( "Curve output at rated conditions = " + TrimSigDigits( CurveVal, 3 ) );
					}

				} else {
					ShowSevereError( RoutineName + ':' + CurrentModuleObject + "=\"" + ZoneDehumid( ZoneDehumidIndex ).Name + "\" illegal " + cAlphaFields( 6 ) + " type for this object = " + GetCurveType( ZoneDehumid( ZoneDehumidIndex ).EnergyFactorCurveIndex ) );
					ShowContinueError( "Curve type must be BiQuadratic." );
					ErrorsFound = true;
				}}
			}

			// A7,  \field Part Load Fraction Correlation Curve Name
			ZoneDehumid( ZoneDehumidIndex ).PartLoadCurveIndex = GetCurveIndex( Alphas( 7 ) ); // convert curve name to number
			if ( ZoneDehumid( ZoneDehumidIndex ).PartLoadCurveIndex == 0 ) {
				if ( lAlphaBlanks( 7 ) ) {
					ShowSevereError( RoutineName + ':' + CurrentModuleObject + "=\"" + cAlphaFields( 7 ) + "\" is required, missing for " + cAlphaFields( 1 ) + " = " + ZoneDehumid( ZoneDehumidIndex ).Name );
				} else {
					ShowSevereError( cAlphaFields( 7 ) + " not found = " + Alphas( 7 ) );
					ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + ZoneDehumid( ZoneDehumidIndex ).Name );
				}
				ErrorsFound = true;
			} else {
				// Verify Curve Object, legal types are Quadratic and Cubic
				{ auto const SELECT_CASE_var( GetCurveType( ZoneDehumid( ZoneDehumidIndex ).PartLoadCurveIndex ) );
				if ( SELECT_CASE_var == "QUADRATIC" ) {
					ZoneDehumid( ZoneDehumidIndex ).PartLoadCurveType = Quadratic;

				} else if ( SELECT_CASE_var == "CUBIC" ) {
					ZoneDehumid( ZoneDehumidIndex ).PartLoadCurveType = Cubic;

				} else {
					ShowSevereError( RoutineName + ':' + CurrentModuleObject + "=\"" + ZoneDehumid( ZoneDehumidIndex ).Name + "\" illegal " + cAlphaFields( 7 ) + " type for this object = " + GetCurveType( ZoneDehumid( ZoneDehumidIndex ).PartLoadCurveIndex ) );
					ShowContinueError( "Curve type must be Quadratic or Cubic." );
					ErrorsFound = true;
				}}
			}

			// N4,  \field Minimum Dry-Bulb Temperature for Dehumidifier Operation
			// N5,  \field Maximum Dry-Bulb Temperature for Dehumidifier Operation
			ZoneDehumid( ZoneDehumidIndex ).MinInletAirTemp = Numbers( 4 );
			ZoneDehumid( ZoneDehumidIndex ).MaxInletAirTemp = Numbers( 5 );

			if ( ZoneDehumid( ZoneDehumidIndex ).MinInletAirTemp >= ZoneDehumid( ZoneDehumidIndex ).MaxInletAirTemp ) {
				ShowSevereError( cNumericFields( 5 ) + " must be greater than " + cNumericFields( 4 ) );
				ShowContinueError( cNumericFields( 5 ) + " specified = " + TrimSigDigits( Numbers( 5 ), 1 ) );
				ShowContinueError( cNumericFields( 4 ) + " specified = " + TrimSigDigits( Numbers( 4 ), 1 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + ZoneDehumid( ZoneDehumidIndex ).Name );
				ErrorsFound = true;
			}

			// N6,  \field Off Cycle Parasitic Electric Load
			ZoneDehumid( ZoneDehumidIndex ).OffCycleParasiticLoad = Numbers( 6 ); // Off Cycle Parasitic Load [W]

			if ( ZoneDehumid( ZoneDehumidIndex ).OffCycleParasiticLoad < 0.0 ) {
				ShowSevereError( cNumericFields( 6 ) + " must be >= zero." );
				ShowContinueError( "Value specified = " + TrimSigDigits( Numbers( 6 ), 2 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + ZoneDehumid( ZoneDehumidIndex ).Name );
				ErrorsFound = true;
			}

			// A8;  \field Condensate Collection Water Storage Tank Name
			ZoneDehumid( ZoneDehumidIndex ).CondensateCollectName = Alphas( 8 );
			if ( lAlphaBlanks( 8 ) ) {
				ZoneDehumid( ZoneDehumidIndex ).CondensateCollectMode = CondensateDiscarded;
			} else {
				ZoneDehumid( ZoneDehumidIndex ).CondensateCollectMode = CondensateToTank;
				SetupTankSupplyComponent( ZoneDehumid( ZoneDehumidIndex ).Name, CurrentModuleObject, ZoneDehumid( ZoneDehumidIndex ).CondensateCollectName, ErrorsFound, ZoneDehumid( ZoneDehumidIndex ).CondensateTankID, ZoneDehumid( ZoneDehumidIndex ).CondensateTankSupplyARRID );
			}

		} //   DO ZoneDehumidIndex=1,NumDehumidifiers

		Alphas.deallocate();
		cAlphaFields.deallocate();
		cNumericFields.deallocate();
		Numbers.deallocate();
		lAlphaBlanks.deallocate();
		lNumericBlanks.deallocate();

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + ':' + CurrentModuleObject + ": Errors found in input." );
		}

		for ( ZoneDehumidIndex = 1; ZoneDehumidIndex <= NumDehumidifiers; ++ZoneDehumidIndex ) {
			// Set up report variables for the dehumidifiers
			SetupOutputVariable( "Zone Dehumidifier Sensible Heating Rate [W]", ZoneDehumid( ZoneDehumidIndex ).SensHeatingRate, "System", "Average", ZoneDehumid( ZoneDehumidIndex ).Name );
			SetupOutputVariable( "Zone Dehumidifier Sensible Heating Energy [J]", ZoneDehumid( ZoneDehumidIndex ).SensHeatingEnergy, "System", "Sum", ZoneDehumid( ZoneDehumidIndex ).Name );
			SetupOutputVariable( "Zone Dehumidifier Removed Water Mass Flow Rate [kg/s]", ZoneDehumid( ZoneDehumidIndex ).WaterRemovalRate, "System", "Average", ZoneDehumid( ZoneDehumidIndex ).Name );
			SetupOutputVariable( "Zone Dehumidifier Removed Water Mass [kg]", ZoneDehumid( ZoneDehumidIndex ).WaterRemoved, "System", "Sum", ZoneDehumid( ZoneDehumidIndex ).Name );
			SetupOutputVariable( "Zone Dehumidifier Electric Power [W]", ZoneDehumid( ZoneDehumidIndex ).ElecPower, "System", "Average", ZoneDehumid( ZoneDehumidIndex ).Name );
			SetupOutputVariable( "Zone Dehumidifier Electric Energy [J]", ZoneDehumid( ZoneDehumidIndex ).ElecConsumption, "System", "Sum", ZoneDehumid( ZoneDehumidIndex ).Name, _, "Electric", "COOLING", _, "System" );
			SetupOutputVariable( "Zone Dehumidifier Off Cycle Parasitic Electric Power [W]", ZoneDehumid( ZoneDehumidIndex ).OffCycleParasiticElecPower, "System", "Average", ZoneDehumid( ZoneDehumidIndex ).Name );
			SetupOutputVariable( "Zone Dehumidifier Off Cycle Parasitic Electric Energy [J]", ZoneDehumid( ZoneDehumidIndex ).OffCycleParasiticElecCons, "System", "Sum", ZoneDehumid( ZoneDehumidIndex ).Name );
			SetupOutputVariable( "Zone Dehumidifier Part Load Ratio []", ZoneDehumid( ZoneDehumidIndex ).DehumidPLR, "System", "Average", ZoneDehumid( ZoneDehumidIndex ).Name );
			SetupOutputVariable( "Zone Dehumidifier Runtime Fraction []", ZoneDehumid( ZoneDehumidIndex ).DehumidRTF, "System", "Average", ZoneDehumid( ZoneDehumidIndex ).Name );
			SetupOutputVariable( "Zone Dehumidifier Outlet Air Temperature [C]", ZoneDehumid( ZoneDehumidIndex ).OutletAirTemp, "System", "Average", ZoneDehumid( ZoneDehumidIndex ).Name );

			if ( ZoneDehumid( ZoneDehumidIndex ).CondensateCollectMode == CondensateToTank ) {
				SetupOutputVariable( "Zone Dehumidifier Condensate Volume Flow Rate [m3/s]", ZoneDehumid( ZoneDehumidIndex ).DehumidCondVolFlowRate, "System", "Average", ZoneDehumid( ZoneDehumidIndex ).Name );
				SetupOutputVariable( "Zone Dehumidifier Condensate Volume [m3]", ZoneDehumid( ZoneDehumidIndex ).DehumidCondVol, "System", "Sum", ZoneDehumid( ZoneDehumidIndex ).Name, _, "OnSiteWater", "Condensate", _, "System" );
			}

		}

	}

	void
	InitZoneDehumidifier( int const ZoneDehumNum ) // Number of the current zone dehumidifier being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Don Shirey, FSEC
		//       DATE WRITTEN   July/Aug 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine initializes information for the zone dehumidifier model

		// METHODOLOGY EMPLOYED:
		// Use status flags to trigger various initializations

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataZoneEquipment::ZoneEquipInputsFilled;
		using DataZoneEquipment::CheckZoneEquipmentList;
		using Psychrometrics::PsyWFnTdbRhPb;
		using Psychrometrics::PsyRhoAirFnPbTdbW;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "InitZoneDehumidifier" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static Array1D_bool MyEnvrnFlag; // Used for initializations each begin environment flag
		//  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MySizeFlag  ! Used for sizing zone dehumidifier inputs one time
		static bool MyOneTimeFlag( true ); // initialization flag
		static bool ZoneEquipmentListChecked( false ); // True after the Zone Equipment List has been checked for items
		int LoopIndex; // DO loop index
		int AirInletNode; // Inlet air node number
		Real64 RatedAirHumrat; // Humidity ratio (kg/kg) at rated inlet air conditions of 26.6667C, 60% RH
		Real64 RatedAirDBTemp; // Dry-bulb air temperature at rated conditions 26.6667C
		Real64 RatedAirRH; // Relative humidity of air (0.6 --> 60%) at rated conditions

		// Do the one time initializations
		if ( MyOneTimeFlag ) {
			MyEnvrnFlag.allocate( NumDehumidifiers );
			//    ALLOCATE(MySizeFlag(NumDehumidifiers))
			MyEnvrnFlag = true;
			//    MySizeFlag = .TRUE.
			MyOneTimeFlag = false;
		}

		// Need to check all dehumidifiers to see if they are on Zone Equipment List or issue warning
		if ( ! ZoneEquipmentListChecked && ZoneEquipInputsFilled ) {
			ZoneEquipmentListChecked = true;
			for ( LoopIndex = 1; LoopIndex <= NumDehumidifiers; ++LoopIndex ) {
				if ( CheckZoneEquipmentList( ZoneDehumid( LoopIndex ).UnitType, ZoneDehumid( LoopIndex ).Name ) ) continue;
				ShowSevereError( "InitZoneDehumidifier: Zone Dehumidifier=\"" + ZoneDehumid( LoopIndex ).UnitType + ',' + ZoneDehumid( LoopIndex ).Name + "\" is not on any ZoneHVAC:EquipmentList.  It will not be simulated." );
			}
		}

		AirInletNode = ZoneDehumid( ZoneDehumNum ).AirInletNodeNum;
		// Do the Begin Environment initializations
		if ( BeginEnvrnFlag && MyEnvrnFlag( ZoneDehumNum ) ) {

			// Set the mass flow rates from the input volume flow rates, at rated conditions of 26.6667C, 60% RH
			// Might default back to STP later after discussion with M. Witte, use StdRhoAir instead of calc'd RhoAir at rated conditions
			RatedAirDBTemp = 26.6667; // 26.6667 C, 80F
			RatedAirRH = 0.6; // 60% RH
			RatedAirHumrat = PsyWFnTdbRhPb( RatedAirDBTemp, RatedAirRH, StdBaroPress, RoutineName );
			ZoneDehumid( ZoneDehumNum ).RatedAirMassFlow = PsyRhoAirFnPbTdbW( StdBaroPress, RatedAirDBTemp, RatedAirHumrat, RoutineName ) * ZoneDehumid( ZoneDehumNum ).RatedAirVolFlow;

			// Set the node max and min mass flow rates on inlet node... outlet node gets updated in UPDATE subroutine
			Node( AirInletNode ).MassFlowRateMax = ZoneDehumid( ZoneDehumNum ).RatedAirMassFlow;
			Node( AirInletNode ).MassFlowRateMaxAvail = ZoneDehumid( ZoneDehumNum ).RatedAirMassFlow;
			Node( AirInletNode ).MassFlowRateMinAvail = 0.0;
			Node( AirInletNode ).MassFlowRateMin = 0.0;

			MyEnvrnFlag( ZoneDehumNum ) = false;
		} // End one time inits

		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( ZoneDehumNum ) = true;
		}

		// These initializations are done every iteration
		Node( AirInletNode ).MassFlowRate = ZoneDehumid( ZoneDehumNum ).RatedAirMassFlow;

		// Zero out the report variables
		ZoneDehumid( ZoneDehumNum ).SensHeatingRate = 0.0; // Zone Dehumidifier Sensible Heating Rate [W]
		ZoneDehumid( ZoneDehumNum ).SensHeatingEnergy = 0.0; // Zone Dehumidifier Sensible Heating Energy [J]
		ZoneDehumid( ZoneDehumNum ).WaterRemovalRate = 0.0; // Zone Dehumidifier Water Removal Rate [kg/s]
		ZoneDehumid( ZoneDehumNum ).WaterRemoved = 0.0; // Zone Dehumidifier Water Removed [kg]
		ZoneDehumid( ZoneDehumNum ).ElecPower = 0.0; // Zone Dehumidifier Electric Power [W]
		ZoneDehumid( ZoneDehumNum ).ElecConsumption = 0.0; // Zone Dehumidifier Electric Consumption [J]
		ZoneDehumid( ZoneDehumNum ).DehumidPLR = 0.0; // Zone Dehumidifier Part-Load Ratio [-]
		ZoneDehumid( ZoneDehumNum ).DehumidRTF = 0.0; // Zone Dehumidifier Runtime Fraction [-]
		ZoneDehumid( ZoneDehumNum ).OffCycleParasiticElecPower = 0.0; // Zone Dehumidifier Off-Cycle Parasitic Electric Power [W]
		ZoneDehumid( ZoneDehumNum ).OffCycleParasiticElecCons = 0.0; // Zone Dehumidifier Off-Cycle Parasitic Electric Consumption [J]
		ZoneDehumid( ZoneDehumNum ).DehumidCondVolFlowRate = 0.0; // Zone Dehumidifier Condensate Volumetric Flow Rate [m3/s]
		ZoneDehumid( ZoneDehumNum ).DehumidCondVol = 0.0; // Zone Dehumidifier Condensate Volume [m3]
		ZoneDehumid( ZoneDehumNum ).OutletAirTemp = Node( AirInletNode ).Temp; // Zone Dehumidifier Outlet Air Temperature [C]

	}

	void
	SizeZoneDehumidifier()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Don Shirey, FSEC
		//       DATE WRITTEN   July 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// No automatic sizing for this model (yet).  Left in place for later (autosize based on latent requirements)

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

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

	}

	void
	CalcZoneDehumidifier(
		int const ZoneDehumNum, // Index number of the current zone dehumidifier being simulated
		Real64 const QZnDehumidReq, // Dehumidification load to be met (kg/s), negative value means dehumidification load
		Real64 & SensibleOutput, // Sensible (heating) output (W), sent to load predictor for next simulation time step
		Real64 & LatentOutput // Latent (dehumidification) output provided (kg/s)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Don Shirey, FSEC
		//       DATE WRITTEN   July/Aug 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate the delivered capacity, electric energy consumption and water/condensate
		// removal rates for the zone dehumidifier.

		// METHODOLOGY EMPLOYED:
		// Cycle the dehumidifier as needed to meet the remaining zone dehumidification load.
		// Send excess sensible heat to zone energy balance (via SensibleOutput) for next HVAC time step,
		// so set the dehumidifier outlet air temp = inlet air temp to avoid double counting excess sensible.

		// REFERENCES:
		// na

		// Using/Aliasing
		using CurveManager::CurveValue;
		using Psychrometrics::RhoH2O;
		using Psychrometrics::PsyRhFnTdbWPb;
		using Psychrometrics::PsyHfgAirFnWTdb;
		using Psychrometrics::PsyCpAirFnWTdb;
		using Psychrometrics::PsyHFnTdbW;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcZoneDehumidifier" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 WaterRemovalRateFactor; // Adjustment to  Rate Water Removal as a function of inlet air T and RH
		Real64 WaterRemovalVolRate; // Actual water removal rate at current inlet air conditions (L/day)
		Real64 WaterRemovalMassRate; // Actual water removal rate at current inlet air conditions (kg/s)
		Real64 EnergyFactorAdjFactor; // Adjustment to Rate Energy Factor as a function of inlet air T and RH
		Real64 EnergyFactor; // Actual Energy Factor as a function of inlet air T and RH
		Real64 InletAirTemp; // Dry-bulb temperature of air entering the dehumidifier (C)
		Real64 InletAirHumRat; // Humidity ratio of the air entering the dehumidifier (kg/kg)
		Real64 InletAirRH; // Relative humidity of air entering the dehumidifier (%)
		Real64 OutletAirTemp; // Dry-bulb temperature of air leaving the dehumidifier (C)
		Real64 OutletAirHumRat; // Humidity ratio of air leaving the dehumidifier (kg/kg)
		Real64 PLR; // Part-load ratio = (dehumid load to be met)/(dehumid capacity of the dehumidifier)
		Real64 PLF; // Part-load fraction (-), RuntimeFraction = PLR/PLF
		Real64 RunTimeFraction; // Dehumidifier runtime fraction (-)
		Real64 ElectricPowerOnCycle; // Electric power when dehumidifier is operating (W)
		Real64 ElectricPowerAvg; // Average electric power for this dehumidifier (W)
		Real64 hfg; // Enthalpy of evaporation of inlet air (J/kg)
		Real64 AirMassFlowRate; // Air mass flow rate through this dehumidifier (kg/s)
		Real64 Cp; // Heat capacity of inlet air (J/kg-C)
		static int AirInletNodeNum( 0 ); // Node number for the inlet air to the dehumidifier
		static int AirOutletNodeNum( 0 ); // Node number for the outlet air from the dehumidifier

		SensibleOutput = 0.0;
		LatentOutput = 0.0;
		WaterRemovalRateFactor = 0.0;
		AirMassFlowRate = 0.0;
		PLR = 0.0;
		PLF = 0.0;
		EnergyFactorAdjFactor = 0.0;
		RunTimeFraction = 0.0;
		ElectricPowerAvg = 0.0;
		ElectricPowerOnCycle = 0.0;

		AirInletNodeNum = ZoneDehumid( ZoneDehumNum ).AirInletNodeNum;
		AirOutletNodeNum = ZoneDehumid( ZoneDehumNum ).AirOutletNodeNum;

		InletAirTemp = Node( AirInletNodeNum ).Temp;
		InletAirHumRat = Node( AirInletNodeNum ).HumRat;
		InletAirRH = 100.0 * PsyRhFnTdbWPb( InletAirTemp, InletAirHumRat, OutBaroPress, RoutineName ); // RH in percent (%)

		if ( QZnDehumidReq < 0.0 && GetCurrentScheduleValue( ZoneDehumid( ZoneDehumNum ).SchedPtr ) > 0.0 && InletAirTemp >= ZoneDehumid( ZoneDehumNum ).MinInletAirTemp && InletAirTemp <= ZoneDehumid( ZoneDehumNum ).MaxInletAirTemp ) {
			// A dehumidification load is being requested and dehumidifier is available (schedule value > 0)
			//  and the inlet air temperature is within the min/max values specified by user input

			WaterRemovalRateFactor = CurveValue( ZoneDehumid( ZoneDehumNum ).WaterRemovalCurveIndex, InletAirTemp, InletAirRH );
			// Warn user if curve output goes negative
			if ( WaterRemovalRateFactor <= 0.0 ) {
				if ( ZoneDehumid( ZoneDehumNum ).WaterRemovalCurveErrorCount < 1 ) {
					++ZoneDehumid( ZoneDehumNum ).WaterRemovalCurveErrorCount;
					ShowWarningError( ZoneDehumid( ZoneDehumNum ).UnitType + " \"" + ZoneDehumid( ZoneDehumNum ).Name + "\":" );
					ShowContinueError( " Water Removal Rate Curve output is <= 0.0 (" + TrimSigDigits( WaterRemovalRateFactor, 5 ) + ")." );
					ShowContinueError( " Negative value occurs using an inlet air dry-bulb temperature of " + TrimSigDigits( InletAirTemp, 2 ) + " and an inlet air relative humidity of " + TrimSigDigits( InletAirRH, 1 ) + '.' );
					ShowContinueErrorTimeStamp( " Dehumidifier turned off for this time step but simulation continues." );
				} else {
					ShowRecurringWarningErrorAtEnd( ZoneDehumid( ZoneDehumNum ).UnitType + " \"" + ZoneDehumid( ZoneDehumNum ).Name + "\": Water Removal Rate Curve output is <= 0.0 warning continues...", ZoneDehumid( ZoneDehumNum ).WaterRemovalCurveErrorIndex, WaterRemovalRateFactor, WaterRemovalRateFactor );
				}
				WaterRemovalRateFactor = 0.0;
			}

			WaterRemovalVolRate = WaterRemovalRateFactor * ZoneDehumid( ZoneDehumNum ).RatedWaterRemoval;

			WaterRemovalMassRate = WaterRemovalVolRate / ( 24.0 * SecInHour * 1000.0 ) * RhoH2O( max( ( InletAirTemp - 11.0 ), 1.0 ) ); //(L/d)/(24 hr/day *3600 sec/hr * 1000 L/m3) | Density of water, minimum temp = 1.0C

			if ( WaterRemovalMassRate > 0.0 ) {
				PLR = max( 0.0, min( 1.0, - QZnDehumidReq / WaterRemovalMassRate ) );
			} else {
				PLR = 0.0;
				RunTimeFraction = 0.0;
			}

			EnergyFactorAdjFactor = CurveValue( ZoneDehumid( ZoneDehumNum ).EnergyFactorCurveIndex, InletAirTemp, InletAirRH );

			// Warn user if curve output goes negative
			if ( EnergyFactorAdjFactor <= 0.0 ) {
				if ( ZoneDehumid( ZoneDehumNum ).EnergyFactorCurveErrorCount < 1 ) {
					++ZoneDehumid( ZoneDehumNum ).EnergyFactorCurveErrorCount;
					ShowWarningError( ZoneDehumid( ZoneDehumNum ).UnitType + " \"" + ZoneDehumid( ZoneDehumNum ).Name + "\":" );
					ShowContinueError( " Energy Factor Curve output is <= 0.0 (" + TrimSigDigits( EnergyFactorAdjFactor, 5 ) + ")." );
					ShowContinueError( " Negative value occurs using an inlet air dry-bulb temperature of " + TrimSigDigits( InletAirTemp, 2 ) + " and an inlet air relative humidity of " + TrimSigDigits( InletAirRH, 1 ) + '.' );
					ShowContinueErrorTimeStamp( " Dehumidifier turned off for this time step but simulation continues." );
				} else {
					ShowRecurringWarningErrorAtEnd( ZoneDehumid( ZoneDehumNum ).UnitType + " \"" + ZoneDehumid( ZoneDehumNum ).Name + "\": Energy Factor Curve output is <= 0.0 warning continues...", ZoneDehumid( ZoneDehumNum ).EnergyFactorCurveErrorIndex, EnergyFactorAdjFactor, EnergyFactorAdjFactor );
				}
				ElectricPowerAvg = 0.0;
				PLR = 0.0;
				RunTimeFraction = 0.0;
			} else {
				// EnergyFactorAdjFactor is not negative, so proceed with calculations
				EnergyFactor = EnergyFactorAdjFactor * ZoneDehumid( ZoneDehumNum ).RatedEnergyFactor;

				if ( ZoneDehumid( ZoneDehumNum ).PartLoadCurveIndex > 0 ) {
					PLF = CurveValue( ZoneDehumid( ZoneDehumNum ).PartLoadCurveIndex, PLR ); // Calculate part load fraction
				} else {
					PLF = 1.0;
				}

				if ( PLF < 0.7 ) {
					if ( ZoneDehumid( ZoneDehumNum ).LowPLFErrorCount < 1 ) {
						++ZoneDehumid( ZoneDehumNum ).LowPLFErrorCount;
						ShowWarningError( ZoneDehumid( ZoneDehumNum ).UnitType + " \"" + ZoneDehumid( ZoneDehumNum ).Name + "\":" );
						ShowContinueError( " The Part Load Fraction Correlation Curve output is (" + TrimSigDigits( PLF, 2 ) + ") at a part-load ratio =" + TrimSigDigits( PLR, 3 ) );
						ShowContinueErrorTimeStamp( " PLF curve values must be >= 0.7.  PLF has been reset to 0.7 and simulation is continuing." );
					} else {
						ShowRecurringWarningErrorAtEnd( ZoneDehumid( ZoneDehumNum ).UnitType + " \"" + ZoneDehumid( ZoneDehumNum ).Name + "\": Part Load Fraction Correlation Curve output < 0.7 warning continues...", ZoneDehumid( ZoneDehumNum ).LowPLFErrorIndex, PLF, PLF );
					}
					PLF = 0.7;
				}

				if ( PLF > 1.0 ) {
					if ( ZoneDehumid( ZoneDehumNum ).HighPLFErrorCount < 1 ) {
						++ZoneDehumid( ZoneDehumNum ).HighPLFErrorCount;
						ShowWarningError( ZoneDehumid( ZoneDehumNum ).UnitType + " \"" + ZoneDehumid( ZoneDehumNum ).Name + "\":" );
						ShowContinueError( " The Part Load Fraction Correlation Curve output is (" + TrimSigDigits( PLF, 2 ) + ") at a part-load ratio =" + TrimSigDigits( PLR, 3 ) );
						ShowContinueErrorTimeStamp( " PLF curve values must be < 1.0.  PLF has been reset to 1.0 and simulation is continuing." );
					} else {
						ShowRecurringWarningErrorAtEnd( ZoneDehumid( ZoneDehumNum ).UnitType + " \"" + ZoneDehumid( ZoneDehumNum ).Name + "\": Part Load Fraction Correlation Curve output > 1.0 warning continues...", ZoneDehumid( ZoneDehumNum ).HighPLFErrorIndex, PLF, PLF );
					}
					PLF = 1.0;
				}

				if ( PLF > 0.0 && PLF >= PLR ) {
					RunTimeFraction = PLR / PLF; // Calculate dehumidifier runtime fraction
				} else {
					if ( ZoneDehumid( ZoneDehumNum ).PLFPLRErrorCount < 1 ) {
						++ZoneDehumid( ZoneDehumNum ).PLFPLRErrorCount;
						ShowWarningError( ZoneDehumid( ZoneDehumNum ).UnitType + " \"" + ZoneDehumid( ZoneDehumNum ).Name + "\":" );
						ShowContinueError( "The part load fraction was less than the part load ratio calculated for this time step [PLR=" + TrimSigDigits( PLR, 4 ) + ", PLF=" + TrimSigDigits( PLF, 4 ) + "]." );
						ShowContinueError( "Runtime fraction reset to 1 and the simulation will continue." );
						ShowContinueErrorTimeStamp( "" );
					} else {
						ShowRecurringWarningErrorAtEnd( ZoneDehumid( ZoneDehumNum ).UnitType + " \"" + ZoneDehumid( ZoneDehumNum ).Name + "\": Part load fraction less than part load ratio warning continues...", ZoneDehumid( ZoneDehumNum ).PLFPLRErrorIndex );
					}
					RunTimeFraction = 1.0;
				}

				if ( RunTimeFraction > 1.0 && std::abs( RunTimeFraction - 1.0 ) > 0.001 ) {
					if ( ZoneDehumid( ZoneDehumNum ).HighRTFErrorCount < 1 ) {
						++ZoneDehumid( ZoneDehumNum ).HighRTFErrorCount;
						ShowWarningError( ZoneDehumid( ZoneDehumNum ).UnitType + " \"" + ZoneDehumid( ZoneDehumNum ).Name + "\":" );
						ShowContinueError( "The runtime fraction for this zone dehumidifier exceeded 1.0 [" + TrimSigDigits( RunTimeFraction, 4 ) + "]." );
						ShowContinueError( "Runtime fraction reset to 1 and the simulation will continue." );
						ShowContinueErrorTimeStamp( "" );
					} else {
						ShowRecurringWarningErrorAtEnd( ZoneDehumid( ZoneDehumNum ).UnitType + " \"" + ZoneDehumid( ZoneDehumNum ).Name + "\": Runtime fraction for zone dehumidifier exceeded 1.0 warning continues...", ZoneDehumid( ZoneDehumNum ).HighRTFErrorIndex, RunTimeFraction, RunTimeFraction );
					}
					RunTimeFraction = 1.0;
				}

				// ElectricPowerOnCycle = Water removal volumetric rate (L/day) / (Energy Factor(L/kWh) * 24 hrs/day ) * 1000 Wh/kWh
				ElectricPowerOnCycle = WaterRemovalVolRate / ( EnergyFactor * 24.0 ) * 1000.0; // Watts
				// ElectricPowerAvg     = ElectricPowerOnCycle * RTF + (1-RTF)*OffCycleParsiticLoad
				ElectricPowerAvg = ElectricPowerOnCycle * RunTimeFraction + ( 1.0 - RunTimeFraction ) * ZoneDehumid( ZoneDehumNum ).OffCycleParasiticLoad; // average Watts
			}

			LatentOutput = WaterRemovalMassRate * PLR; // Average moisture removal rate, kg/s, for this timestep
			hfg = PsyHfgAirFnWTdb( InletAirHumRat, InletAirTemp );
			SensibleOutput = ( LatentOutput * hfg ) + ElectricPowerAvg; // Average sensible output, Watts
			// Send SensibleOutput to zone air heat balance via SysDepZoneLoads in ZoneEquipmentManager

			Node( AirInletNodeNum ).MassFlowRate = ZoneDehumid( ZoneDehumNum ).RatedAirMassFlow * PLR;
			AirMassFlowRate = Node( AirInletNodeNum ).MassFlowRate; // Average air mass flow for this timestep
			Cp = PsyCpAirFnWTdb( InletAirHumRat, InletAirTemp ); // Heat capacity of air
			if ( AirMassFlowRate > 0.0 && Cp > 0.0 ) {
				OutletAirTemp = InletAirTemp + ( ElectricPowerOnCycle + ( WaterRemovalMassRate * hfg ) ) / ( ZoneDehumid( ZoneDehumNum ).RatedAirMassFlow * Cp );
				OutletAirHumRat = InletAirHumRat - LatentOutput / AirMassFlowRate;
			} else {
				OutletAirTemp = InletAirTemp;
				OutletAirHumRat = InletAirHumRat;
			}

		} else {

			// No load or not available or inlet air temps beyond min/max limits, then set outlet conditions
			// equal to inlet conditions and PLR = RTF = 0.0
			OutletAirTemp = InletAirTemp;
			OutletAirHumRat = InletAirHumRat;
			PLR = 0.0;
			RunTimeFraction = 0.0;
			Node( AirInletNodeNum ).MassFlowRate = 0.0;
			// If available but didn't operate, then set electric power = off cycle parasitic load.
			// Else, electric power = 0.0
			if ( GetCurrentScheduleValue( ZoneDehumid( ZoneDehumNum ).SchedPtr ) > 0.0 ) {
				ElectricPowerAvg = ZoneDehumid( ZoneDehumNum ).OffCycleParasiticLoad; // off cycle parasitic is on entire timestep
			} else {
				ElectricPowerAvg = 0.0;
			}

		}

		ZoneDehumid( ZoneDehumNum ).OutletAirTemp = OutletAirTemp; // Update report variable here. Node outlet Temp set equal
		//   to Node inlet Temp in Update subroutine
		ZoneDehumid( ZoneDehumNum ).OutletAirHumRat = OutletAirHumRat; // Store in structure, updated outlet node in Update subroutine

		// Use inlet air temperature in outlet air enthalpy calculation... since the sensible heat output
		// from the dehumidifier is being sent directly to the zone air heat balance for next hvac simulation time step
		ZoneDehumid( ZoneDehumNum ).OutletAirEnthalpy = PsyHFnTdbW( InletAirTemp, OutletAirHumRat );

		ZoneDehumid( ZoneDehumNum ).SensHeatingRate = SensibleOutput; // Report variable update, W,  avg sens output when unit is 'on'
		ZoneDehumid( ZoneDehumNum ).WaterRemovalRate = LatentOutput; // Report variable update, kg/s
		LatentOutput = -LatentOutput; // change sign... negative is dehumidification in zone air balance

		ZoneDehumid( ZoneDehumNum ).OffCycleParasiticElecPower = ( 1.0 - RunTimeFraction ) * ZoneDehumid( ZoneDehumNum ).OffCycleParasiticLoad;
		ZoneDehumid( ZoneDehumNum ).ElecPower = ElectricPowerAvg;
		ZoneDehumid( ZoneDehumNum ).DehumidPLR = PLR;
		ZoneDehumid( ZoneDehumNum ).DehumidRTF = RunTimeFraction;

	}

	void
	UpdateZoneDehumidifier( int const ZoneDehumNum ) // Number of the current zone dehumidifier being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Don Shirey, FSEC
		//       DATE WRITTEN   August 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for passing results to the outlet air node.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataContaminantBalance::Contaminant;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int AirInletNodeNum; // Node number corresponding to the air entering dehumidifier
		int AirOutletNodeNum; // Node number corresponding to the air leaving dehumidifier

		AirInletNodeNum = ZoneDehumid( ZoneDehumNum ).AirInletNodeNum;
		AirOutletNodeNum = ZoneDehumid( ZoneDehumNum ).AirOutletNodeNum;

		// Changed outlet node properties
		Node( AirOutletNodeNum ).Enthalpy = ZoneDehumid( ZoneDehumNum ).OutletAirEnthalpy;
		Node( AirOutletNodeNum ).HumRat = ZoneDehumid( ZoneDehumNum ).OutletAirHumRat;
		// Set outlet temp = inlet temp; send excess sensible heat directly to air heat balance
		// (via SensibleOutput and QSensOut) for the next hvac simulation time step.
		Node( AirOutletNodeNum ).Temp = Node( AirInletNodeNum ).Temp;

		// Pass through output node properties
		Node( AirOutletNodeNum ).Quality = Node( AirInletNodeNum ).Quality;
		Node( AirOutletNodeNum ).Press = Node( AirInletNodeNum ).Press;
		Node( AirOutletNodeNum ).MassFlowRate = Node( AirInletNodeNum ).MassFlowRate;
		Node( AirOutletNodeNum ).MassFlowRateMin = Node( AirInletNodeNum ).MassFlowRateMin;
		Node( AirOutletNodeNum ).MassFlowRateMax = Node( AirInletNodeNum ).MassFlowRateMax;
		Node( AirOutletNodeNum ).MassFlowRateMinAvail = Node( AirInletNodeNum ).MassFlowRateMinAvail;
		Node( AirOutletNodeNum ).MassFlowRateMaxAvail = Node( AirInletNodeNum ).MassFlowRateMaxAvail;

		if ( Contaminant.CO2Simulation ) {
			Node( AirOutletNodeNum ).CO2 = Node( AirInletNodeNum ).CO2;
		}
		if ( Contaminant.GenericContamSimulation ) {
			Node( AirOutletNodeNum ).GenContam = Node( AirInletNodeNum ).GenContam;
		}

	}

	void
	ReportZoneDehumidifier( int const DehumidNum ) // Index of the current zone dehumidifier being simulated
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Don Shirey, FSEC
		//       DATE WRITTEN   August 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Fills some of the report variables for the zone dehumidifiers

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHVACGlobals::TimeStepSys;
		using DataWater::WaterStorage;
		using Psychrometrics::RhoH2O;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 ReportingConstant; // Number of seconds per HVAC system time step, to convert from W (J/s) to J
		Real64 RhoWater; // Density of condensate (water) being removed (kg/m3)
		Real64 InletAirTemp; // Dry-bulb temperature of air entering the dehumidifier (C)
		Real64 OutletAirTemp; // Dry-bulb temperature of air leaving the dehumidifier (C)
		int AirInletNodeNum; // Node number corresponding to the air entering dehumidifier

		ReportingConstant = TimeStepSys * SecInHour;

		ZoneDehumid( DehumidNum ).SensHeatingEnergy = ZoneDehumid( DehumidNum ).SensHeatingRate * ReportingConstant;
		ZoneDehumid( DehumidNum ).WaterRemoved = ZoneDehumid( DehumidNum ).WaterRemovalRate * ReportingConstant;
		ZoneDehumid( DehumidNum ).ElecConsumption = ZoneDehumid( DehumidNum ).ElecPower * ReportingConstant;
		ZoneDehumid( DehumidNum ).OffCycleParasiticElecCons = ZoneDehumid( DehumidNum ).OffCycleParasiticElecPower * ReportingConstant;

		// Dehumidifier water collection to water storage tank (if needed)
		if ( ZoneDehumid( DehumidNum ).CondensateCollectMode == CondensateToTank ) {
			// Calculate and report condensation rate (how much water extracted from the air stream)
			// Volumetric flow of water in m3/s for water system interactions

			AirInletNodeNum = ZoneDehumid( DehumidNum ).AirInletNodeNum;
			InletAirTemp = Node( AirInletNodeNum ).Temp;
			OutletAirTemp = max( ( InletAirTemp - 11.0 ), 1.0 ); // Assume coil outlet air is 11C (20F) lower than inlet air temp
			RhoWater = RhoH2O( OutletAirTemp ); // Density of water, minimum temp = 1.0 C

			if ( RhoWater > 0.0 ) {
				ZoneDehumid( DehumidNum ).DehumidCondVolFlowRate = ZoneDehumid( DehumidNum ).WaterRemovalRate / RhoWater;
			}

			ZoneDehumid( DehumidNum ).DehumidCondVol = ZoneDehumid( DehumidNum ).DehumidCondVolFlowRate * ReportingConstant;

			WaterStorage( ZoneDehumid( DehumidNum ).CondensateTankID ).VdotAvailSupply( ZoneDehumid( DehumidNum ).CondensateTankSupplyARRID ) = ZoneDehumid( DehumidNum ).DehumidCondVolFlowRate;
			// Assume water outlet temp = air outlet temp.... same assumption in other places in code (e.g., water coil component)
			WaterStorage( ZoneDehumid( DehumidNum ).CondensateTankID ).TwaterSupply( ZoneDehumid( DehumidNum ).CondensateTankSupplyARRID ) = OutletAirTemp;

		}

	}

	bool
	GetZoneDehumidifierNodeNumber( int const NodeNumber ) // Node being tested
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Lixing Gu
		//       DATE WRITTEN   August 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// After making sure get input is done, the node number of indicated
		// zone dehumidifier is returned.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		bool FindZoneDehumidifierNodeNumber; // Zone Dehumidifier Node Number Check

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:
		int ZoneDehumidIndex; // Loop index

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		if ( GetInputFlag ) {
			GetZoneDehumidifierInput();
			GetInputFlag = false;
		}

		FindZoneDehumidifierNodeNumber = false;
		for ( ZoneDehumidIndex = 1; ZoneDehumidIndex <= NumDehumidifiers; ++ZoneDehumidIndex ) {
			if ( NodeNumber == ZoneDehumid( ZoneDehumidIndex ).AirInletNodeNum ) {
				FindZoneDehumidifierNodeNumber = true;
				break;
			}
			if ( NodeNumber == ZoneDehumid( ZoneDehumidIndex ).AirOutletNodeNum ) {
				FindZoneDehumidifierNodeNumber = true;
				break;
			}
		}

		return FindZoneDehumidifierNodeNumber;

	}

} // ZoneDehumidifier

} // EnergyPlus
