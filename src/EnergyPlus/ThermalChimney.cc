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
#include <ThermalChimney.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHeatBalSurface.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataSurfaces.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <OutputProcessor.hh>
#include <Psychrometrics.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace ThermalChimney {
	// Module containing the data for Thermal Chimney system

	// MODULE INFORMATION:
	//       AUTHOR         Kwang Ho Lee
	//       DATE WRITTEN   April 2008
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To encapsulate the data and algorithyms required to manage the ThermalChimney System Component

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// 1. N. K. Bansal, R. Mathur and M. S. Bhandari, "Solar Chimney for Enhanced Stack Ventilation",
	// Building and Environment, 28, pp. 373-377, 1993
	// 2. K. S. Ong, "A Mathematical Model of a Solar Chimney", Renewable Energy, 28, pp. 1047-1060, 2003
	// 3. J. Marti-Herrero and M. R. Heras-Celemin, "Dynamic Physical Model for a Solar Chimney",
	// Solar Energy, 81, pp. 614-622, 2007

	// OTHER NOTES: none

	// Using/Aliasing
	using namespace DataGlobals;
	using namespace DataEnvironment;
	using namespace DataHeatBalFanSys;
	using namespace DataHeatBalance;
	using namespace DataSurfaces;
	using namespace DataHeatBalSurface;

	// Use statements for access to subroutines in other modules
	using namespace Psychrometrics;

	// Data
	// DERIVED TYPE DEFINITIONS

	int TotThermalChimney( 0 ); // Total ThermalChimney Statements in input

	static std::string const BlankString;

	// Subroutine Specifications for the Heat Balance Module
	// Driver Routines
	// Get Input routines for module
	// Algorithms for the module
	// Reporting routines for module
	// Utility routines for module

	// Object Data
	Array1D< ThermalChimneyData > ThermalChimneySys;
	Array1D< ThermChimZnReportVars > ZnRptThermChim;
	Array1D< ThermChimReportVars > ThermalChimneyReport;

	// MODULE SUBROUTINES:
	//*************************************************************************

	// Functions

	void
	ManageThermalChimney()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Kwang Ho Lee
		//       DATE WRITTEN   April 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine manages the simulation of ThermalChimney unit.
		// This driver manages the calls to all of
		// the other drivers and simulation algorithms.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool GetInputFlag( true );
		static bool ErrorsFound( false );

		// Obtains and Allocates heat balance related parameters from input file
		if ( GetInputFlag ) {
			GetThermalChimney( ErrorsFound );
			GetInputFlag = false;
		}

		if ( TotThermalChimney == 0 ) return;

		CalcThermalChimney();

		ReportThermalChimney();

	}

	void
	GetThermalChimney( bool & ErrorsFound ) // If errors found in input
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Kwang Ho Lee
		//       DATE WRITTEN   April 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine obtains input data for ThermalChimney units and
		// stores it in the ThermalChimney data structure.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using InputProcessor::VerifyName;
		using ScheduleManager::GetScheduleIndex;
		using General::RoundSigDigits;
		using namespace DataIPShortCuts;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const FlowFractionTolerance( 0.0001 ); // Smallest deviation from unity for the sum of all fractions

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//    CHARACTER(len=MaxNameLength), DIMENSION(23) :: AlphaName
		//    REAL(r64) , DIMENSION(63)              :: IHGNumbers
		int NumAlpha;
		int NumNumber;
		Real64 AllRatiosSummed;
		int TCZoneNum; // Thermal chimney zone counter
		int TCZoneNum1; // Thermal chimney zone counter
		int IOStat;
		int Loop;
		int Loop1;
		bool IsNotOK;
		bool IsBlank;

		//  ALLOCATE(MCPTThermChim(NumOfZones))
		//  MCPTThermChim=0.0
		//  ALLOCATE(MCPThermChim(NumOfZones))
		//  MCPThermChim=0.0
		//  ALLOCATE(ThermChimAMFL(NumOfZones))
		//  ThermChimAMFL=0.0

		// Following used for reporting
		ZnRptThermChim.allocate( NumOfZones );

		cCurrentModuleObject = "ZoneThermalChimney";
		TotThermalChimney = GetNumObjectsFound( cCurrentModuleObject );

		ThermalChimneySys.allocate( TotThermalChimney );
		ThermalChimneyReport.allocate( TotThermalChimney );

		for ( Loop = 1; Loop <= TotThermalChimney; ++Loop ) {

			GetObjectItem( cCurrentModuleObject, Loop, cAlphaArgs, NumAlpha, rNumericArgs, NumNumber, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			// First Alpha is Thermal Chimney Name
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), ThermalChimneySys, Loop, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) {
					continue;
				} else {
					cAlphaArgs( 1 ) = cAlphaArgs( 1 ) + "--dup";
				}
			}
			ThermalChimneySys( Loop ).Name = cAlphaArgs( 1 );

			// Second Alpha is Zone Name
			ThermalChimneySys( Loop ).RealZonePtr = FindItemInList( cAlphaArgs( 2 ), Zone );
			if ( ThermalChimneySys( Loop ).RealZonePtr == 0 ) {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid Zone" );
				ShowContinueError( "invalid - not found " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\"." );
				ErrorsFound = true;
			} else if ( ! Zone( ThermalChimneySys( Loop ).RealZonePtr ).HasWindow ) {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid Zone" );
				ShowContinueError( "...invalid - no window(s) in " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\"." );
				ShowContinueError( "...thermal chimney zones must have window(s)." );
				ErrorsFound = true;
			}
			ThermalChimneySys( Loop ).RealZoneName = cAlphaArgs( 2 );

			ThermalChimneySys( Loop ).SchedName = cAlphaArgs( 3 );
			if ( lAlphaFieldBlanks( 3 ) ) {
				ThermalChimneySys( Loop ).SchedPtr = ScheduleAlwaysOn;
			} else {
				ThermalChimneySys( Loop ).SchedPtr = GetScheduleIndex( cAlphaArgs( 3 ) );
				if ( ThermalChimneySys( Loop ).SchedPtr == 0 ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid data" );
					ShowContinueError( "Invalid-not found " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\"." );
					ErrorsFound = true;
				}
			}

			ThermalChimneySys( Loop ).AbsorberWallWidth = rNumericArgs( 1 );
			if ( ThermalChimneySys( Loop ).AbsorberWallWidth < 0.0 ) {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid " + cNumericFieldNames( 1 ) + " must be >= 0, entered value=[" + RoundSigDigits( rNumericArgs( 1 ), 2 ) + "]." );
				ErrorsFound = true;
			}

			ThermalChimneySys( Loop ).AirOutletCrossArea = rNumericArgs( 2 );
			if ( ThermalChimneySys( Loop ).AirOutletCrossArea < 0.0 ) {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid " + cNumericFieldNames( 2 ) + " must be >= 0, entered value=[" + RoundSigDigits( rNumericArgs( 2 ), 2 ) + "]." );
				ErrorsFound = true;
			}

			ThermalChimneySys( Loop ).DischargeCoeff = rNumericArgs( 3 );
			if ( ( ThermalChimneySys( Loop ).DischargeCoeff <= 0.0 ) || ( ThermalChimneySys( Loop ).DischargeCoeff > 1.0 ) ) {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid " + cNumericFieldNames( 3 ) + " must be > 0 and <=1.0, entered value=[" + RoundSigDigits( rNumericArgs( 3 ), 2 ) + "]." );
				ErrorsFound = true;
			}

			ThermalChimneySys( Loop ).TotZoneToDistrib = NumAlpha - 3;
			ThermalChimneySys( Loop ).ZonePtr.allocate( ThermalChimneySys( Loop ).TotZoneToDistrib );
			ThermalChimneySys( Loop ).ZoneName.allocate( ThermalChimneySys( Loop ).TotZoneToDistrib );
			ThermalChimneySys( Loop ).DistanceThermChimInlet.allocate( ThermalChimneySys( Loop ).TotZoneToDistrib );
			ThermalChimneySys( Loop ).RatioThermChimAirFlow.allocate( ThermalChimneySys( Loop ).TotZoneToDistrib );
			ThermalChimneySys( Loop ).EachAirInletCrossArea.allocate( ThermalChimneySys( Loop ).TotZoneToDistrib );

			AllRatiosSummed = 0.0;
			for ( TCZoneNum = 1; TCZoneNum <= ThermalChimneySys( Loop ).TotZoneToDistrib; ++TCZoneNum ) {
				ThermalChimneySys( Loop ).ZoneName( TCZoneNum ) = cAlphaArgs( TCZoneNum + 3 );
				ThermalChimneySys( Loop ).ZonePtr( TCZoneNum ) = FindItemInList( cAlphaArgs( TCZoneNum + 3 ), Zone );
				ThermalChimneySys( Loop ).DistanceThermChimInlet( TCZoneNum ) = rNumericArgs( 3 * TCZoneNum + 1 );
				ThermalChimneySys( Loop ).RatioThermChimAirFlow( TCZoneNum ) = rNumericArgs( 3 * TCZoneNum + 2 );
				if ( lNumericFieldBlanks( 3 * TCZoneNum + 2 ) ) ThermalChimneySys( Loop ).RatioThermChimAirFlow( TCZoneNum ) = 1.0;
				ThermalChimneySys( Loop ).EachAirInletCrossArea( TCZoneNum ) = rNumericArgs( 3 * TCZoneNum + 3 );

				//!! Error trap for zones that do not exist or zones not in the zone the thermal chimney is in
				if ( ThermalChimneySys( Loop ).ZonePtr( TCZoneNum ) == 0 ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid " + cAlphaFieldNames( TCZoneNum + 3 ) + "=\"" + cAlphaArgs( TCZoneNum + 3 ) + "\" not found." );
					ErrorsFound = true;
				} else if ( ThermalChimneySys( Loop ).ZonePtr( TCZoneNum ) == ThermalChimneySys( Loop ).RealZonePtr ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid reference " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) );
					ShowContinueError( "...must not have same zone as reference= " + cAlphaFieldNames( TCZoneNum + 3 ) + "=\"" + cAlphaArgs( TCZoneNum + 3 ) + "\"." );
					ErrorsFound = true;
				}

				if ( ThermalChimneySys( Loop ).DistanceThermChimInlet( TCZoneNum ) < 0.0 ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid " + cNumericFieldNames( 3 * TCZoneNum + 1 ) + " must be >= 0, entered value=[" + RoundSigDigits( rNumericArgs( 3 * TCZoneNum + 1 ), 2 ) + "]." );
					ErrorsFound = true;
				}

				if ( ( ThermalChimneySys( Loop ).RatioThermChimAirFlow( TCZoneNum ) <= 0.0 ) || ( ThermalChimneySys( Loop ).RatioThermChimAirFlow( TCZoneNum ) > 1.0 ) ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid " + cNumericFieldNames( 3 * TCZoneNum + 2 ) + " must be > 0 and <=1.0, entered value=[" + RoundSigDigits( rNumericArgs( 3 * TCZoneNum + 2 ), 2 ) + "]." );
					ErrorsFound = true;
				}

				if ( ThermalChimneySys( Loop ).EachAirInletCrossArea( TCZoneNum ) < 0.0 ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid " + cNumericFieldNames( 3 * TCZoneNum + 3 ) + " must be >= 0, entered value=[" + RoundSigDigits( rNumericArgs( 3 * TCZoneNum + 3 ), 2 ) + "]." );
					ErrorsFound = true;
				}

				AllRatiosSummed += ThermalChimneySys( Loop ).RatioThermChimAirFlow( TCZoneNum );

			} // DO TCZoneNum = 1, ThermalChimneySys(Loop)%TotZoneToDistrib

			// Error trap if the sum of fractions is not equal to 1.0
			if ( std::abs( AllRatiosSummed - 1.0 ) > FlowFractionTolerance ) {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid sum of fractions, must be =1.0, entered value (summed from entries)=[" + RoundSigDigits( AllRatiosSummed, 4 ) + "]." );
				ErrorsFound = true;
			}

		} // DO Loop=1, TotThermalChimney

		// Set up the output variables for thermal chimneys
		for ( Loop = 1; Loop <= TotThermalChimney; ++Loop ) {
			SetupOutputVariable( "Zone Thermal Chimney Current Density Air Volume Flow Rate [m3/s]", ThermalChimneyReport( Loop ).OverallTCVolumeFlow, "System", "Average", ThermalChimneySys( Loop ).Name );
			SetupOutputVariable( "Zone Thermal Chimney Standard Density Air Volume Flow Rate [m3/s]", ThermalChimneyReport( Loop ).OverallTCVolumeFlowStd, "System", "Average", ThermalChimneySys( Loop ).Name );
			SetupOutputVariable( "Zone Thermal Chimney Mass Flow Rate [kg/s]", ThermalChimneyReport( Loop ).OverallTCMassFlow, "System", "Average", ThermalChimneySys( Loop ).Name );
			SetupOutputVariable( "Zone Thermal Chimney Outlet Temperature [C]", ThermalChimneyReport( Loop ).OutletAirTempThermalChim, "System", "Average", ThermalChimneySys( Loop ).Name );

			for ( TCZoneNum = 1; TCZoneNum <= ThermalChimneySys( Loop ).TotZoneToDistrib; ++TCZoneNum ) {
				SetupOutputVariable( "Zone Thermal Chimney Heat Loss Energy [J]", ZnRptThermChim( ThermalChimneySys( Loop ).ZonePtr( TCZoneNum ) ).ThermalChimneyHeatLoss, "System", "Sum", Zone( ThermalChimneySys( Loop ).ZonePtr( TCZoneNum ) ).Name );
				SetupOutputVariable( "Zone Thermal Chimney Heat Gain Energy [J]", ZnRptThermChim( ThermalChimneySys( Loop ).ZonePtr( TCZoneNum ) ).ThermalChimneyHeatGain, "System", "Sum", Zone( ThermalChimneySys( Loop ).ZonePtr( TCZoneNum ) ).Name );
				SetupOutputVariable( "Zone Thermal Chimney Volume [m3]", ZnRptThermChim( ThermalChimneySys( Loop ).ZonePtr( TCZoneNum ) ).ThermalChimneyVolume, "System", "Sum", Zone( ThermalChimneySys( Loop ).ZonePtr( TCZoneNum ) ).Name );
				SetupOutputVariable( "Zone Thermal Chimney Mass [kg]", ZnRptThermChim( ThermalChimneySys( Loop ).ZonePtr( TCZoneNum ) ).ThermalChimneyMass, "System", "Sum", Zone( ThermalChimneySys( Loop ).ZonePtr( TCZoneNum ) ).Name );
			} // DO TCZoneNum = 1, ThermalChimneySys(Loop)%TotZoneToDistrib
		} // DO Loop=1, TotThermalChimney

		//! LKL-more renaming effort and code review might be possible here
		// Check to make sure there is only one thermal chimney statement per zone
		for ( Loop = 1; Loop <= TotThermalChimney; ++Loop ) {
			if ( ThermalChimneySys( Loop ).TotZoneToDistrib > 1 ) {
				for ( TCZoneNum = 1; TCZoneNum <= ThermalChimneySys( Loop ).TotZoneToDistrib; ++TCZoneNum ) {

					if ( ThermalChimneySys( Loop ).TotZoneToDistrib >= ( TCZoneNum + 1 ) ) {
						for ( TCZoneNum1 = TCZoneNum + 1; TCZoneNum1 <= ThermalChimneySys( Loop ).TotZoneToDistrib; ++TCZoneNum1 ) {
							if ( ThermalChimneySys( Loop ).ZonePtr( TCZoneNum ) == ThermalChimneySys( Loop ).ZonePtr( TCZoneNum1 ) ) {
								ShowSevereError( "Only one ZoneThermalChimney object allowed per zone but zone " + ThermalChimneySys( Loop ).ZoneName( TCZoneNum ) + " has two ZoneThermalChimney objects associated with it" );
								ErrorsFound = true;
							}
						}
						for ( TCZoneNum1 = 1; TCZoneNum1 <= TCZoneNum - 1; ++TCZoneNum1 ) {
							if ( ThermalChimneySys( Loop ).ZonePtr( TCZoneNum ) == ThermalChimneySys( Loop ).ZonePtr( TCZoneNum1 ) ) {
								ShowSevereError( "Only one ZoneThermalChimney object allowed per zone but zone " + ThermalChimneySys( Loop ).ZoneName( TCZoneNum ) + " has two ZoneThermalChimney objects associated with it" );
								ErrorsFound = true;
							}
						}
					} else { // IF ( ThermalChimneySys(Loop)%TotZoneToDistrib >= (TCZoneNum+1) ) THEN
						for ( TCZoneNum1 = 1; TCZoneNum1 <= TCZoneNum - 1; ++TCZoneNum1 ) {
							if ( ThermalChimneySys( Loop ).ZonePtr( TCZoneNum ) == ThermalChimneySys( Loop ).ZonePtr( TCZoneNum1 ) ) {
								ShowSevereError( "Only one ZoneThermalChimney object allowed per zone but zone " + ThermalChimneySys( Loop ).ZoneName( TCZoneNum ) + " has two ZoneThermalChimney objects associated with it" );
								ErrorsFound = true;
							}
						}
					} // IF ( ThermalChimneySys(Loop)%TotZoneToDistrib >= (TCZoneNum+1) ) THEN

				} // DO TCZoneNum = 1, ThermalChimneySys(Loop)%TotZoneToDistrib
			} // IF (ThermalChimneySys(Loop)%TotZoneToDistrib > 1) THEN
		} // DO Loop = 1, TotThermalChimney

		// Check to make sure there is only one thermal chimney statement per zone
		if ( TotThermalChimney > 1 ) {
			for ( Loop = 1; Loop <= TotThermalChimney; ++Loop ) {

				if ( TotThermalChimney >= ( Loop + 1 ) ) {
					for ( Loop1 = Loop + 1; Loop1 <= TotThermalChimney; ++Loop1 ) {
						for ( TCZoneNum = 1; TCZoneNum <= ThermalChimneySys( Loop ).TotZoneToDistrib; ++TCZoneNum ) {
							for ( TCZoneNum1 = 1; TCZoneNum1 <= ThermalChimneySys( Loop1 ).TotZoneToDistrib; ++TCZoneNum1 ) {
								if ( ThermalChimneySys( Loop ).ZonePtr( TCZoneNum ) == ThermalChimneySys( Loop1 ).ZonePtr( TCZoneNum1 ) ) {
									ShowSevereError( "Only one ZoneThermalChimney object allowed per zone but zone " + ThermalChimneySys( Loop ).ZoneName( TCZoneNum ) + " has two ZoneThermalChimney objects associated with it" );
									ErrorsFound = true;
								}
							}
						}
					}
					for ( Loop1 = 1; Loop1 <= Loop - 1; ++Loop1 ) {
						for ( TCZoneNum = 1; TCZoneNum <= ThermalChimneySys( Loop ).TotZoneToDistrib; ++TCZoneNum ) {
							for ( TCZoneNum1 = 1; TCZoneNum1 <= ThermalChimneySys( Loop1 ).TotZoneToDistrib; ++TCZoneNum1 ) {
								if ( ThermalChimneySys( Loop ).ZonePtr( TCZoneNum ) == ThermalChimneySys( Loop1 ).ZonePtr( TCZoneNum1 ) ) {
									ShowSevereError( "Only one ZoneThermalChimney object allowed per zone but zone " + ThermalChimneySys( Loop ).ZoneName( TCZoneNum ) + " has two ZoneThermalChimney objects associated with it" );
									ErrorsFound = true;
								}
							}
						}
					}
				} else { // IF ( TotThermalChimney >= (Loop+1) ) THEN
					for ( Loop1 = 1; Loop1 <= Loop - 1; ++Loop1 ) {
						for ( TCZoneNum = 1; TCZoneNum <= ThermalChimneySys( Loop ).TotZoneToDistrib; ++TCZoneNum ) {
							for ( TCZoneNum1 = 1; TCZoneNum1 <= ThermalChimneySys( Loop1 ).TotZoneToDistrib; ++TCZoneNum1 ) {
								if ( ThermalChimneySys( Loop ).ZonePtr( TCZoneNum ) == ThermalChimneySys( Loop1 ).ZonePtr( TCZoneNum1 ) ) {
									ShowSevereError( "Only one ZoneThermalChimney object allowed per zone but zone " + ThermalChimneySys( Loop ).ZoneName( TCZoneNum ) + " has two ZoneThermalChimney objects associated with it" );
									ErrorsFound = true;
								}
							}
						}
					}
				} // IF ( TotThermalChimney >= (Loop+1) ) THEN

			} // DO Loop = 1, TotThermalChimney
		} // IF (TotThermalChimney > 1) THEN

		if ( ErrorsFound ) {
			ShowFatalError( cCurrentModuleObject + " Errors found in input.  Preceding condition(s) cause termination." );
		}

	}

	void
	CalcThermalChimney()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Kwang Ho Lee
		//       DATE WRITTEN   April 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine simulates the components making up the ThermalChimney.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using ScheduleManager::GetCurrentScheduleValue;
		using DataEnvironment::StdRhoAir;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const NTC( 15 ); // Number of subregions in thermal chimney air channel for FINITE DIFFERENCE

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// To be obtained from other modules and subroutines
		Real64 SurfTempAbsorberWall; // Absorber wall surface temperature (K)
		Real64 SurfTempGlassCover; // Glass cover surface temperature (K)
		Real64 ConvTransCoeffWallFluid; // Absorber wall convection trasnfer coefficient
		Real64 ConvTransCoeffGlassFluid; // Glass cover convection trasnfer coefficient

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// Real local vaiables
		int Loop; // DO loop counter
		int SurfNum; // DO loop counter for surfaces
		int ZoneNum; // DO loop counter for zones
		int TCZoneNumCounter;
		int TCZoneNum;
		Real64 minorW; // width of enclosure (narrow dimension)
		Real64 majorW; // width of major surface
		Real64 TempmajorW;

		Real64 RoomAirTemp;
		Real64 AirSpecHeatThermalChim; // (J/kg*C) or (J/kg*K)
		Real64 AbsorberWallWidthTC;
		Real64 TCVolumeAirFlowRate; // (m^3/s)
		Real64 TCMassAirFlowRate; // (kg/s)
		Real64 DischargeCoeffTC;
		Real64 AirOutletCrossAreaTC;
		Real64 AirInletCrossArea;
		Real64 AirRelativeCrossArea;
		// REAL(r64)                    :: OutletAirTempThermalChim
		Real64 OverallThermalChimLength;
		Real64 ThermChimTolerance;
		Array1D< Real64 > TempTCMassAirFlowRate( 10 ); // Temporary Value of Thermal Chimney Mass Flow Rate ()
		Array1D< Real64 > TempTCVolumeAirFlowRate( 10 ); // Temporary Value of Thermal Chimney Volume Flow Rate ()
		int IterationLoop;
		Real64 Process1; // Temporary Variable Used in the Middle of the Calculation
		Real64 Process2; // Temporary Variable Used in the Middle of the Calculation
		Real64 Process3; // Temporary Variable Used in the Middle of the Calculation
		//unused1208  REAL(r64)   :: Process4                            ! Temporary Variable Used in the Middle of the Calculation
		Real64 AirDensityThermalChim; // (kg/m^3)
		Real64 AirDensity; // (kg/m^3)
		Real64 CpAir;
		Real64 TemporaryWallSurfTemp;

		Real64 DeltaL; // OverallThermalChimLength / NTC
		int ThermChimLoop1;
		int ThermChimLoop2;
		Array2D< Real64 > EquaCoef( NTC, NTC ); // Coefficients in Linear Algebraic Euqation for FINITE DIFFERENCE
		Array1D< Real64 > EquaConst( NTC ); // Constants in Linear Algebraic Equation for FINITE DIFFERENCE
		Array1D< Real64 > ThermChimSubTemp( NTC ); // Air temperature of each thermal chimney air channel subregion

		for ( Loop = 1; Loop <= TotThermalChimney; ++Loop ) {

			ZoneNum = ThermalChimneySys( Loop ).RealZonePtr;
			// start off with first surface in zone widths
			majorW = Surface( Zone( ZoneNum ).SurfaceFirst ).Width;
			minorW = majorW;
			TempmajorW = 0.0;
			TemporaryWallSurfTemp = -10000.0;

			// determine major width and minor width
			for ( SurfNum = Zone( ZoneNum ).SurfaceFirst + 1; SurfNum <= Zone( ZoneNum ).SurfaceLast; ++SurfNum ) {
				if ( Surface( SurfNum ).Class != SurfaceClass_Wall ) continue;

				if ( Surface( SurfNum ).Width > majorW ) {
					majorW = Surface( SurfNum ).Width;
				}

				if ( Surface( SurfNum ).Width < minorW ) {
					minorW = Surface( SurfNum ).Width;
				}
			}

			for ( SurfNum = Zone( ZoneNum ).SurfaceFirst; SurfNum <= Zone( ZoneNum ).SurfaceLast; ++SurfNum ) {
				if ( Surface( SurfNum ).Width == majorW ) {
					if ( TempSurfIn( SurfNum ) > TemporaryWallSurfTemp ) {
						TemporaryWallSurfTemp = TempSurfIn( SurfNum );
						ConvTransCoeffWallFluid = HConvIn( SurfNum );
						SurfTempAbsorberWall = TempSurfIn( SurfNum ) + KelvinConv;
					}
				}
			}

			for ( SurfNum = Zone( ZoneNum ).SurfaceFirst; SurfNum <= Zone( ZoneNum ).SurfaceLast; ++SurfNum ) {

				if ( ! Surface( SurfNum ).HeatTransSurf ) continue; // Skip non-heat transfer surfaces

				if ( Surface( SurfNum ).Class == SurfaceClass_Window ) {

					if ( Surface( SurfNum ).Width > TempmajorW ) {
						TempmajorW = Surface( SurfNum ).Width;
						ConvTransCoeffGlassFluid = HConvIn( SurfNum );
						SurfTempGlassCover = TempSurfIn( SurfNum ) + KelvinConv;
					}

				}

			}

			AbsorberWallWidthTC = majorW;
			if ( ThermalChimneySys( Loop ).AbsorberWallWidth != majorW ) {
				AbsorberWallWidthTC = ThermalChimneySys( Loop ).AbsorberWallWidth;
			}

			AirDensityThermalChim = PsyRhoAirFnPbTdbW( OutBaroPress, MAT( ZoneNum ), ZoneAirHumRat( ZoneNum ) );
			AirSpecHeatThermalChim = PsyCpAirFnWTdb( ZoneAirHumRat( ZoneNum ), MAT( ZoneNum ) );
			AirOutletCrossAreaTC = ThermalChimneySys( Loop ).AirOutletCrossArea;
			DischargeCoeffTC = ThermalChimneySys( Loop ).DischargeCoeff;

			AirInletCrossArea = 0.0;
			for ( TCZoneNum = 1; TCZoneNum <= ThermalChimneySys( Loop ).TotZoneToDistrib; ++TCZoneNum ) {
				AirInletCrossArea += ThermalChimneySys( Loop ).EachAirInletCrossArea( TCZoneNum );
			}

			RoomAirTemp = 0.0;
			for ( TCZoneNum = 1; TCZoneNum <= ThermalChimneySys( Loop ).TotZoneToDistrib; ++TCZoneNum ) {
				TCZoneNumCounter = ThermalChimneySys( Loop ).ZonePtr( TCZoneNum );
				RoomAirTemp += ThermalChimneySys( Loop ).RatioThermChimAirFlow( TCZoneNum ) * MAT( TCZoneNumCounter );
			}
			RoomAirTemp += KelvinConv;

			Process1 = 0.0;
			Process2 = 0.0;
			for ( TCZoneNum = 1; TCZoneNum <= ThermalChimneySys( Loop ).TotZoneToDistrib; ++TCZoneNum ) {
				TCZoneNumCounter = ThermalChimneySys( Loop ).ZonePtr( TCZoneNum );
				Process1 += PsyHFnTdbW( MAT( TCZoneNumCounter ), ZoneAirHumRat( TCZoneNumCounter ) ) * ThermalChimneySys( Loop ).DistanceThermChimInlet( TCZoneNum ) * ThermalChimneySys( Loop ).RatioThermChimAirFlow( TCZoneNum );
				Process2 += ThermalChimneySys( Loop ).RatioThermChimAirFlow( TCZoneNum ) * PsyHFnTdbW( MAT( TCZoneNumCounter ), ZoneAirHumRat( TCZoneNumCounter ) );
			}
			OverallThermalChimLength = Process1 / Process2;

			DeltaL = OverallThermalChimLength / NTC;

			// Starting the iteration for mass and volumetric flow rate calculation
			ThermChimTolerance = 10000000.0; // An impossibly big tolerance
			for ( IterationLoop = 1; IterationLoop <= 10; ++IterationLoop ) {

				if ( IterationLoop == 1 ) {
					TempTCMassAirFlowRate( IterationLoop ) = 0.05; // Inital Guess

				} else {
					TempTCMassAirFlowRate( IterationLoop ) = TempTCVolumeAirFlowRate( IterationLoop - 1 ) * AirDensityThermalChim;

					if ( std::abs( TempTCMassAirFlowRate( IterationLoop ) - TempTCMassAirFlowRate( IterationLoop - 1 ) ) < ThermChimTolerance ) {
						ThermChimTolerance = std::abs( TempTCMassAirFlowRate( IterationLoop ) - TempTCMassAirFlowRate( IterationLoop - 1 ) );
						TCMassAirFlowRate = TempTCMassAirFlowRate( IterationLoop );
						TCVolumeAirFlowRate = TempTCVolumeAirFlowRate( IterationLoop );
					}

				} // IF (IterationLoop == 1) THEN

				// Calculation of Thermal Chimney Discharge Air Temperature
				Process1 = AbsorberWallWidthTC * DeltaL * ConvTransCoeffGlassFluid + AbsorberWallWidthTC * DeltaL * ConvTransCoeffWallFluid - 2.0 * TempTCMassAirFlowRate( IterationLoop ) * AirSpecHeatThermalChim;
				Process2 = AbsorberWallWidthTC * DeltaL * ConvTransCoeffGlassFluid + AbsorberWallWidthTC * DeltaL * ConvTransCoeffWallFluid + 2.0 * TempTCMassAirFlowRate( IterationLoop ) * AirSpecHeatThermalChim;
				Process3 = 2.0 * AbsorberWallWidthTC * DeltaL * ConvTransCoeffGlassFluid * SurfTempGlassCover + 2.0 * AbsorberWallWidthTC * DeltaL * ConvTransCoeffWallFluid * SurfTempAbsorberWall;

				for ( ThermChimLoop1 = 1; ThermChimLoop1 <= NTC; ++ThermChimLoop1 ) {
					for ( ThermChimLoop2 = 1; ThermChimLoop2 <= NTC; ++ThermChimLoop2 ) {
						EquaCoef( ThermChimLoop2, ThermChimLoop1 ) = 0.0;
					}
				}

				EquaCoef( 1, 1 ) = Process2;
				EquaConst( 1 ) = Process3 - Process1 * RoomAirTemp;
				for ( ThermChimLoop1 = 2; ThermChimLoop1 <= NTC; ++ThermChimLoop1 ) {
					EquaCoef( ( ThermChimLoop1 - 1 ), ThermChimLoop1 ) = Process1;
					EquaCoef( ThermChimLoop1, ThermChimLoop1 ) = Process2;
					EquaConst( ThermChimLoop1 ) = Process3;
				}

				GaussElimination( EquaCoef, EquaConst, ThermChimSubTemp, NTC );

				AirRelativeCrossArea = AirOutletCrossAreaTC / AirInletCrossArea;
				if ( ThermChimSubTemp( NTC ) <= RoomAirTemp ) {
					TempTCVolumeAirFlowRate( IterationLoop ) = 0.0;
				} else {
					TempTCVolumeAirFlowRate( IterationLoop ) = DischargeCoeffTC * AirOutletCrossAreaTC * std::sqrt( 2.0 * ( ( ThermChimSubTemp( NTC ) - RoomAirTemp ) / RoomAirTemp ) * 9.8 * OverallThermalChimLength / pow_2( 1.0 + AirRelativeCrossArea ) );
				}

			} // DO IterationLoop = 1,10

			// Calculation of Thermal Chimney Discharge Temperature
			Process1 = AbsorberWallWidthTC * DeltaL * ConvTransCoeffGlassFluid + AbsorberWallWidthTC * DeltaL * ConvTransCoeffWallFluid - 2.0 * TCMassAirFlowRate * AirSpecHeatThermalChim;
			Process2 = AbsorberWallWidthTC * DeltaL * ConvTransCoeffGlassFluid + AbsorberWallWidthTC * DeltaL * ConvTransCoeffWallFluid + 2.0 * TCMassAirFlowRate * AirSpecHeatThermalChim;
			Process3 = 2.0 * AbsorberWallWidthTC * DeltaL * ConvTransCoeffGlassFluid * SurfTempGlassCover + 2.0 * AbsorberWallWidthTC * DeltaL * ConvTransCoeffWallFluid * SurfTempAbsorberWall;

			for ( ThermChimLoop1 = 1; ThermChimLoop1 <= NTC; ++ThermChimLoop1 ) {
				for ( ThermChimLoop2 = 1; ThermChimLoop2 <= NTC; ++ThermChimLoop2 ) {
					EquaCoef( ThermChimLoop2, ThermChimLoop1 ) = 0.0;
				}
			}

			EquaCoef( 1, 1 ) = Process2;
			EquaConst( 1 ) = Process3 - Process1 * RoomAirTemp;
			for ( ThermChimLoop1 = 2; ThermChimLoop1 <= NTC; ++ThermChimLoop1 ) {
				EquaCoef( ( ThermChimLoop1 - 1 ), ThermChimLoop1 ) = Process1;
				EquaCoef( ThermChimLoop1, ThermChimLoop1 ) = Process2;
				EquaConst( ThermChimLoop1 ) = Process3;
			}

			GaussElimination( EquaCoef, EquaConst, ThermChimSubTemp, NTC );

			AirRelativeCrossArea = AirOutletCrossAreaTC / AirInletCrossArea;
			if ( ThermChimSubTemp( NTC ) <= RoomAirTemp ) {
				TCVolumeAirFlowRate = 0.0;
			} else {
				TCVolumeAirFlowRate = DischargeCoeffTC * AirOutletCrossAreaTC * std::sqrt( 2.0 * ( ( ThermChimSubTemp( NTC ) - RoomAirTemp ) / RoomAirTemp ) * 9.8 * OverallThermalChimLength / pow_2( 1.0 + AirRelativeCrossArea ) );
			}

			// Now assignment of the overall mass flow rate into each zone
			for ( TCZoneNum = 1; TCZoneNum <= ThermalChimneySys( Loop ).TotZoneToDistrib; ++TCZoneNum ) {
				TCZoneNumCounter = ThermalChimneySys( Loop ).ZonePtr( TCZoneNum );
				AirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, MAT( TCZoneNumCounter ), ZoneAirHumRat( TCZoneNumCounter ) );
				CpAir = PsyCpAirFnWTdb( ZoneAirHumRat( TCZoneNumCounter ), MAT( TCZoneNumCounter ) );
				MCPThermChim( TCZoneNumCounter ) = TCVolumeAirFlowRate * AirDensity * CpAir * ThermalChimneySys( Loop ).RatioThermChimAirFlow( TCZoneNum );
				if ( MCPThermChim( TCZoneNumCounter ) <= 0.0 ) {
					MCPThermChim( TCZoneNumCounter ) = 0.0;
				}
				ThermChimAMFL( TCZoneNumCounter ) = MCPThermChim( TCZoneNumCounter ) / CpAir;
				MCPTThermChim( TCZoneNumCounter ) = MCPThermChim( TCZoneNumCounter ) * Zone( TCZoneNumCounter ).OutDryBulbTemp;
			}

			MCPThermChim( ZoneNum ) = TCVolumeAirFlowRate * AirDensity * CpAir;
			if ( MCPThermChim( ZoneNum ) <= 0.0 ) {
				MCPThermChim( ZoneNum ) = 0.0;
			}
			ThermChimAMFL( ZoneNum ) = MCPThermChim( ZoneNum ) / CpAir;
			MCPTThermChim( ZoneNum ) = MCPThermChim( ZoneNum ) * Zone( ZoneNum ).OutDryBulbTemp;

			ThermalChimneyReport( Loop ).OverallTCVolumeFlow = TCVolumeAirFlowRate;
			ThermalChimneyReport( Loop ).OverallTCMassFlow = TCMassAirFlowRate;
			ThermalChimneyReport( Loop ).OverallTCVolumeFlowStd = TCMassAirFlowRate / StdRhoAir;
			if ( ThermalChimneyReport( Loop ).OverallTCMassFlow != ( TCVolumeAirFlowRate * AirDensityThermalChim ) ) {
				ThermalChimneyReport( Loop ).OverallTCMassFlow = ThermalChimneyReport( Loop ).OverallTCVolumeFlow * AirDensityThermalChim;
			}
			ThermalChimneyReport( Loop ).OutletAirTempThermalChim = ThermChimSubTemp( NTC ) - KelvinConv;

			if ( GetCurrentScheduleValue( ThermalChimneySys( Loop ).SchedPtr ) <= 0.0 ) {
				for ( TCZoneNum = 1; TCZoneNum <= ThermalChimneySys( Loop ).TotZoneToDistrib; ++TCZoneNum ) {
					TCZoneNumCounter = ThermalChimneySys( Loop ).ZonePtr( TCZoneNum );
					MCPThermChim( TCZoneNumCounter ) = 0.0;
					ThermChimAMFL( TCZoneNumCounter ) = 0.0;
					MCPTThermChim( TCZoneNumCounter ) = 0.0;
				}
				MCPThermChim( ZoneNum ) = 0.0;
				ThermChimAMFL( ZoneNum ) = 0.0;
				MCPTThermChim( ZoneNum ) = 0.0;
				ThermalChimneyReport( Loop ).OverallTCVolumeFlow = 0.0;
				ThermalChimneyReport( Loop ).OverallTCVolumeFlowStd = 0.0;
				ThermalChimneyReport( Loop ).OverallTCMassFlow = 0.0;
				ThermalChimneyReport( Loop ).OutletAirTempThermalChim = MAT( ZoneNum );
			}

		} // DO Loop=1, TotThermalChimney

	}

	void
	ReportThermalChimney()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Kwang Ho Lee
		//       DATE WRITTEN   April 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine fills remaining report variables.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHVACGlobals::TimeStepSys;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ZoneLoop; // Counter for the # of zones (nz)
		Real64 AirDensity;
		Real64 CpAir;
		Real64 TSMult;

		TSMult = TimeStepSys * SecInHour;

		for ( ZoneLoop = 1; ZoneLoop <= NumOfZones; ++ZoneLoop ) { // Start of zone loads report variable update loop ...

			// Break the infiltration load into heat gain and loss components.
			AirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, MAT( ZoneLoop ), ZoneAirHumRat( ZoneLoop ) );
			CpAir = PsyCpAirFnWTdb( ZoneAirHumRat( ZoneLoop ), MAT( ZoneLoop ) );
			ZnRptThermChim( ZoneLoop ).ThermalChimneyVolume = ( MCPThermChim( ZoneLoop ) / CpAir / AirDensity ) * TSMult;
			ZnRptThermChim( ZoneLoop ).ThermalChimneyMass = ( MCPThermChim( ZoneLoop ) / CpAir ) * TSMult;

			ZnRptThermChim( ZoneLoop ).ThermalChimneyHeatLoss = 0.0;
			ZnRptThermChim( ZoneLoop ).ThermalChimneyHeatGain = 0.0;

			if ( ZT( ZoneLoop ) > Zone( ZoneLoop ).OutDryBulbTemp ) {

				ZnRptThermChim( ZoneLoop ).ThermalChimneyHeatLoss = MCPThermChim( ZoneLoop ) * ( ZT( ZoneLoop ) - Zone( ZoneLoop ).OutDryBulbTemp ) * TSMult;
				ZnRptThermChim( ZoneLoop ).ThermalChimneyHeatGain = 0.0;

			} else if ( ZT( ZoneLoop ) <= Zone( ZoneLoop ).OutDryBulbTemp ) {

				ZnRptThermChim( ZoneLoop ).ThermalChimneyHeatGain = MCPThermChim( ZoneLoop ) * ( Zone( ZoneLoop ).OutDryBulbTemp - ZT( ZoneLoop ) ) * TSMult;
				ZnRptThermChim( ZoneLoop ).ThermalChimneyHeatLoss = 0.0;

			}

		} // ... end of zone loads report variable update loop.

	}

	void
	GaussElimination(
		Array2A< Real64 > EquaCoef,
		Array1A< Real64 > EquaConst,
		Array1A< Real64 > ThermChimSubTemp,
		int const NTC
	)
	{
		// SUBROUTINE INFORMATION:

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine sovles linear algebraic equations using Gauss Elimination Method.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Argument array dimensioning
		EquaCoef.dim( NTC, NTC );
		EquaConst.dim( NTC );
		ThermChimSubTemp.dim( NTC );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		Array1D< Real64 > tempor( NTC );
		Real64 tempb;
		Real64 TCvalue;
		Real64 TCcoefficient;
		int pivot;
		Real64 ThermalChimSum;
		int ThermChimLoop1;
		int ThermChimLoop2;
		int ThermChimLoop3;

		for ( ThermChimLoop1 = 1; ThermChimLoop1 <= NTC; ++ThermChimLoop1 ) {

			TCvalue = std::abs( EquaCoef( ThermChimLoop1, ThermChimLoop1 ) );
			pivot = ThermChimLoop1;
			for ( ThermChimLoop2 = ThermChimLoop1 + 1; ThermChimLoop2 <= NTC; ++ThermChimLoop2 ) {
				if ( std::abs( EquaCoef( ThermChimLoop1, ThermChimLoop2 ) ) > TCvalue ) {
					TCvalue = std::abs( EquaCoef( ThermChimLoop1, ThermChimLoop2 ) );
					pivot = ThermChimLoop2;
				}
			}

			if ( pivot != ThermChimLoop1 ) {
				tempor( {ThermChimLoop1,NTC} ) = EquaCoef( {ThermChimLoop1,NTC}, ThermChimLoop1 );
				tempb = EquaConst( ThermChimLoop1 );
				EquaCoef( {ThermChimLoop1,NTC}, ThermChimLoop1 ) = EquaCoef( {ThermChimLoop1,NTC}, pivot );
				EquaConst( ThermChimLoop1 ) = EquaConst( pivot );
				EquaCoef( {ThermChimLoop1,NTC}, pivot ) = tempor( {ThermChimLoop1,NTC} );
				EquaConst( pivot ) = tempb;
			}

			for ( ThermChimLoop2 = ThermChimLoop1 + 1; ThermChimLoop2 <= NTC; ++ThermChimLoop2 ) {
				TCcoefficient = -EquaCoef( ThermChimLoop1, ThermChimLoop2 ) / EquaCoef( ThermChimLoop1, ThermChimLoop1 );
				EquaCoef( {ThermChimLoop1,NTC}, ThermChimLoop2 ) += TCcoefficient * EquaCoef( {ThermChimLoop1,NTC}, ThermChimLoop1 );
				EquaConst( ThermChimLoop2 ) += TCcoefficient * EquaConst( ThermChimLoop1 );
			}

		}

		ThermChimSubTemp( NTC ) = EquaConst( NTC ) / EquaCoef( NTC, NTC );
		for ( ThermChimLoop2 = NTC - 1; ThermChimLoop2 >= 1; --ThermChimLoop2 ) {
			ThermalChimSum = 0.0;
			for ( ThermChimLoop3 = ThermChimLoop2 + 1; ThermChimLoop3 <= NTC; ++ThermChimLoop3 ) {
				ThermalChimSum += EquaCoef( ThermChimLoop3, ThermChimLoop2 ) * ThermChimSubTemp( ThermChimLoop3 );
			}
			ThermChimSubTemp( ThermChimLoop2 ) = ( EquaConst( ThermChimLoop2 ) - ThermalChimSum ) / EquaCoef( ThermChimLoop2, ThermChimLoop2 );
		}

	}

	//        End of Module Subroutines for ThermalChimney

	//*****************************************************************************************

} // ThermalChimney

} // EnergyPlus
