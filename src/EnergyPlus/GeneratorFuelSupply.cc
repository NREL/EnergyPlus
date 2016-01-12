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
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <GeneratorFuelSupply.hh>
#include <CurveManager.hh>
#include <DataGenerators.hh>
#include <DataGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPrecisionGlobals.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace GeneratorFuelSupply {

	//_______________________________________________
	// Utility modules used by other generators.
	//
	// GeneratorFuelSupply
	//   reused among some generators to define gaseous fuel chemistry, optional compressor)

	// Module containing the routines dealing with the fuel supply for some generators
	// different generator modules can reuse the same fuel supply code, hence a seperate module

	// MODULE INFORMATION:
	//       AUTHOR         B Griffith
	//       DATE WRITTEN   July 2006
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// <description>

	// METHODOLOGY EMPLOYED:
	// data defined in DataGenerators.cc
	// this module only provides input and subroutines for other component simulations
	//  no specific energyplus component is modeled here.  it is used by other generators

	// REFERENCES:
	// Annex 42 documentation

	// OTHER NOTES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataGenerators;
	using DataGlobals::OutputFileInits;
	using DataGlobals::HoursInDay;

	// <use statements for access to subroutines in other modules>

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// na

	// DERIVED TYPE DEFINITIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:
	// na

	// SUBROUTINE SPECIFICATIONS FOR MODULE

	// <name Public routines, optionally name Private routines within this module>

	// Functions

	void
	GetGeneratorFuelSupplyInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B Griffith
		//       DATE WRITTEN   July 2006,
		//       MODIFIED       na
		//       RE-ENGINEERED  this module extracted from older SOFC module for
		//                      reuse with both Annex 42 models,

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound; // might also use FindItemInList
		using InputProcessor::VerifyName;
		using InputProcessor::GetObjectItem;
		using InputProcessor::SameString;
		using namespace DataIPShortCuts;
		using NodeInputManager::GetOnlySingleNode;
		using CurveManager::GetCurveIndex;
		using ScheduleManager::GetScheduleIndex;
		using DataLoopNode::NodeConnectionType_Sensor;
		using DataLoopNode::NodeType_Air;
		using DataLoopNode::ObjectIsNotParent;
		using General::RoundSigDigits;

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
		//  INTEGER                     :: GeneratorNum !Generator counter
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		int IOStat; // IO Status when calling get input subroutine
		Array1D_string AlphArray( 25 ); // character string data
		Array1D< Real64 > NumArray( 200 ); // numeric data TODO deal with allocatable for extensible
		static bool ErrorsFound( false ); // error flag
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		int FuelSupNum;
		static bool MyOneTimeFlag( true );
		std::string ObjMSGName;
		int ConstitNum;

		if ( MyOneTimeFlag ) {
			cCurrentModuleObject = "Generator:FuelSupply";
			NumGeneratorFuelSups = GetNumObjectsFound( cCurrentModuleObject );

			if ( NumGeneratorFuelSups <= 0 ) {
				ShowSevereError( "No " + cCurrentModuleObject + " equipment specified in input file" );
				ErrorsFound = true;
			}

			FuelSupply.allocate( NumGeneratorFuelSups );

			for ( FuelSupNum = 1; FuelSupNum <= NumGeneratorFuelSups; ++FuelSupNum ) {
				GetObjectItem( cCurrentModuleObject, FuelSupNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat, _, _, cAlphaFieldNames, cNumericFieldNames );

				IsNotOK = false;
				IsBlank = false;
				VerifyName( AlphArray( 1 ), FuelSupply, FuelSupNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
				}

				FuelSupply( FuelSupNum ).Name = AlphArray( 1 );
				ObjMSGName = cCurrentModuleObject + " Named " + AlphArray( 1 );
				if ( SameString( "TemperatureFromAirNode", AlphArray( 2 ) ) ) {
					FuelSupply( FuelSupNum ).FuelTempMode = FuelInTempFromNode;
				} else if ( SameString( "Scheduled", AlphArray( 2 ) ) ) {
					FuelSupply( FuelSupNum ).FuelTempMode = FuelInTempSchedule;
				} else {
					ShowSevereError( "Invalid, " + cAlphaFieldNames( 2 ) + " = " + AlphArray( 2 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
					ErrorsFound = true;
				}

				FuelSupply( FuelSupNum ).NodeName = AlphArray( 3 );
				FuelSupply( FuelSupNum ).NodeNum = GetOnlySingleNode( AlphArray( 3 ), ErrorsFound, cCurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Sensor, 1, ObjectIsNotParent );

				FuelSupply( FuelSupNum ).SchedNum = GetScheduleIndex( AlphArray( 4 ) );
				if ( ( FuelSupply( FuelSupNum ).SchedNum == 0 ) && ( FuelSupply( FuelSupNum ).FuelTempMode == FuelInTempSchedule ) ) {
					ShowSevereError( "Invalid, " + cAlphaFieldNames( 4 ) + " = " + AlphArray( 4 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
					ShowContinueError( "Schedule named was not found" );
					ErrorsFound = true;
				}

				FuelSupply( FuelSupNum ).CompPowerCurveID = GetCurveIndex( AlphArray( 5 ) );
				if ( FuelSupply( FuelSupNum ).CompPowerCurveID == 0 ) {
					ShowSevereError( "Invalid, " + cAlphaFieldNames( 5 ) + " = " + AlphArray( 5 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
					ShowContinueError( "Curve named was not found " );
					ErrorsFound = true;
				}

				for ( auto & e : FuelSupply ) e.CompPowerLossFactor = NumArray( 1 );

				if ( SameString( AlphArray( 6 ), "GaseousConstituents" ) ) {
					FuelSupply( FuelSupNum ).FuelTypeMode = fuelModeGaseousConstituents;
				} else if ( SameString( AlphArray( 6 ), "LiquidGeneric" ) ) {
					FuelSupply( FuelSupNum ).FuelTypeMode = fuelModeGenericLiquid;
				} else {
					ShowSevereError( "Invalid, " + cAlphaFieldNames( 6 ) + " = " + AlphArray( 6 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + AlphArray( 1 ) );
					ErrorsFound = true;
				}

				FuelSupply( FuelSupNum ).LHVliquid = NumArray( 2 ) * 1000.0; //generic liquid LHV  (kJ/kG input converted to J/kG )
				FuelSupply( FuelSupNum ).HHV = NumArray( 3 ) * 1000.0; //generic liquid HHV (kJ/kG input converted to J/kG )
				FuelSupply( FuelSupNum ).MW = NumArray( 4 );
				FuelSupply( FuelSupNum ).eCO2 = NumArray( 5 );

				if ( FuelSupply( FuelSupNum ).FuelTypeMode == fuelModeGaseousConstituents ) {
					NumFuelConstit = NumArray( 6 );
					FuelSupply( FuelSupNum ).NumConstituents = NumFuelConstit;

					if ( NumFuelConstit > 12 ) {
						ShowSevereError( cCurrentModuleObject + " model not set up for more than 12 fuel constituents" );
						ErrorsFound = true;
					}
					if ( NumFuelConstit < 1 ) {
						ShowSevereError( cCurrentModuleObject + " model needs at least one fuel constituent" );
						ErrorsFound = true;
					}

					for ( ConstitNum = 1; ConstitNum <= NumFuelConstit; ++ConstitNum ) {
						FuelSupply( FuelSupNum ).ConstitName( ConstitNum ) = AlphArray( ConstitNum + 6 );
						FuelSupply( FuelSupNum ).ConstitMolalFract( ConstitNum ) = NumArray( ConstitNum + 6 );

					}

					// check for molar fractions summing to 1.0.
					if ( std::abs( sum( FuelSupply( FuelSupNum ).ConstitMolalFract ) - 1.0 ) > 0.0001 ) {
						ShowSevereError( cCurrentModuleObject + " molar fractions do not sum to 1.0" );
						ShowContinueError( "Sum was=" + RoundSigDigits( sum( FuelSupply( FuelSupNum ).ConstitMolalFract ), 5 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + AlphArray( 1 ) );
						ErrorsFound = true;
					}
				}

			}

			//now make calls to Setup

			for ( FuelSupNum = 1; FuelSupNum <= NumGeneratorFuelSups; ++FuelSupNum ) {
				SetupFuelConstituentData( FuelSupNum, ErrorsFound );
			}

			if ( ErrorsFound ) {
				ShowFatalError( "Problem found processing input for " + cCurrentModuleObject );
			}

			MyOneTimeFlag = false;
		} // MyOneTimeFlag

	}

	//******************************************************************************

	void
	SetupFuelConstituentData(
		int const FuelSupplyNum,
		bool & ErrorsFound
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B Griffith
		//       DATE WRITTEN   Aug 2005,
		//       MODIFIED       na
		//       RE-ENGINEERED  July/Aug 2006, extracted to own module. added liquid fuel option

		// PURPOSE OF THIS SUBROUTINE:
		// Fill data structure for gas phase thermochemistry

		// METHODOLOGY EMPLOYED:
		// Hardcoded data from NIST is filled into data structure one time only

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na
		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using InputProcessor::FindItem;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumHardCodedConstituents; // number of gases included in data
		Real64 LHVfuel; // lower heating value of fuel, working var
		Real64 HHVfuel; // higher heating value of fuel, working var
		Real64 O2Stoic; // stochiometric oxygen coef in chemical equation (15)
		Real64 CO2ProdStoic; // product gases carbon dioxide coeff
		Real64 H20ProdStoic; // product gases water coeff
		int i; // loop index
		std::string thisName; // working string var
		int thisGasID; // working index in Gas phase data structure
		int CO2dataID; // hard wired to CO2 index in gas data struct
		int WaterDataID; // hard wired to Water index in gas data struct
		Real64 LHVi; // working var for lower heating value calc
		Real64 HHVi; // working var for higher heating value calc
		//  INTEGER   :: thisConstituent
		Real64 MWfuel;
		//unused  REAL(r64) :: DelfHfuel
		//unused  REAL(r64) :: h_i
		//unused  REAL(r64) :: LHV

		// Formats
		static gio::Fmt fmtA( "(A)" );
		static gio::Fmt Format_501( "(' Fuel Supply, ',A,',',G13.6E2,',',G13.6E2,',',G13.6E2,',',G13.6E2)" );

		NumHardCodedConstituents = 14;

		if ( ! allocated( GasPhaseThermoChemistryData ) ) {
			GasPhaseThermoChemistryData.allocate( NumHardCodedConstituents );

		}
		// Carbon Dioxide (CO2) Temp K 298-1200 (Chase 1998)
		GasPhaseThermoChemistryData( 1 ).ConstituentName = "CarbonDioxide";
		GasPhaseThermoChemistryData( 1 ).ConstituentFormula = "CO2";
		GasPhaseThermoChemistryData( 1 ).StdRefMolarEnthOfForm = -393.5224; //KJ/mol
		GasPhaseThermoChemistryData( 1 ).ThermoMode = NISTShomate;
		GasPhaseThermoChemistryData( 1 ).ShomateA = 24.99735;
		GasPhaseThermoChemistryData( 1 ).ShomateB = 55.18696;
		GasPhaseThermoChemistryData( 1 ).ShomateC = -33.69137;
		GasPhaseThermoChemistryData( 1 ).ShomateD = 7.948387;
		GasPhaseThermoChemistryData( 1 ).ShomateE = -0.136638;
		GasPhaseThermoChemistryData( 1 ).ShomateF = -403.6075;
		GasPhaseThermoChemistryData( 1 ).ShomateG = 228.2431;
		GasPhaseThermoChemistryData( 1 ).ShomateH = -393.5224;
		GasPhaseThermoChemistryData( 1 ).NumCarbons = 1.0;
		GasPhaseThermoChemistryData( 1 ).NumHydrogens = 0.0;
		GasPhaseThermoChemistryData( 1 ).NumOxygens = 2.0;
		GasPhaseThermoChemistryData( 1 ).MolecularWeight = 44.01;

		// Nitrogen (N2) Temp (K) 298-6000
		GasPhaseThermoChemistryData( 2 ).ConstituentName = "Nitrogen";
		GasPhaseThermoChemistryData( 2 ).ConstituentFormula = "N2";
		GasPhaseThermoChemistryData( 2 ).StdRefMolarEnthOfForm = 0.0;
		GasPhaseThermoChemistryData( 2 ).ThermoMode = NISTShomate;
		GasPhaseThermoChemistryData( 2 ).ShomateA = 26.092;
		GasPhaseThermoChemistryData( 2 ).ShomateB = 8.218801;
		GasPhaseThermoChemistryData( 2 ).ShomateC = -1.976141;
		GasPhaseThermoChemistryData( 2 ).ShomateD = 0.159274;
		GasPhaseThermoChemistryData( 2 ).ShomateE = 0.044434;
		GasPhaseThermoChemistryData( 2 ).ShomateF = -7.98923;
		GasPhaseThermoChemistryData( 2 ).ShomateG = 221.02;
		GasPhaseThermoChemistryData( 2 ).ShomateH = 0.000;
		GasPhaseThermoChemistryData( 2 ).NumCarbons = 0.0;
		GasPhaseThermoChemistryData( 2 ).NumHydrogens = 0.0;
		GasPhaseThermoChemistryData( 2 ).NumOxygens = 0.0;
		GasPhaseThermoChemistryData( 2 ).MolecularWeight = 28.01;

		// Oxygen (O2) Temp (K) 298-6000
		GasPhaseThermoChemistryData( 3 ).ConstituentName = "Oxygen";
		GasPhaseThermoChemistryData( 3 ).ConstituentFormula = "O2";
		GasPhaseThermoChemistryData( 3 ).StdRefMolarEnthOfForm = 0.0;
		GasPhaseThermoChemistryData( 3 ).ThermoMode = NISTShomate;
		GasPhaseThermoChemistryData( 3 ).ShomateA = 29.659;
		GasPhaseThermoChemistryData( 3 ).ShomateB = 6.137261;
		GasPhaseThermoChemistryData( 3 ).ShomateC = -1.186521;
		GasPhaseThermoChemistryData( 3 ).ShomateD = 0.095780;
		GasPhaseThermoChemistryData( 3 ).ShomateE = -0.219663;
		GasPhaseThermoChemistryData( 3 ).ShomateF = -9.861391;
		GasPhaseThermoChemistryData( 3 ).ShomateG = 237.948;
		GasPhaseThermoChemistryData( 3 ).ShomateH = 0.0;
		GasPhaseThermoChemistryData( 3 ).NumCarbons = 0.0;
		GasPhaseThermoChemistryData( 3 ).NumHydrogens = 0.0;
		GasPhaseThermoChemistryData( 3 ).NumOxygens = 2.0;
		GasPhaseThermoChemistryData( 3 ).MolecularWeight = 32.00;

		// Water (H2O) Temp K 300-1700
		// need lower temperature range for Shomate coef for Water Vapor..
		GasPhaseThermoChemistryData( 4 ).ConstituentName = "Water";
		GasPhaseThermoChemistryData( 4 ).ConstituentFormula = "H2O";
		GasPhaseThermoChemistryData( 4 ).StdRefMolarEnthOfForm = -241.8264; //KJ/mol
		GasPhaseThermoChemistryData( 4 ).ThermoMode = NISTShomate;
		GasPhaseThermoChemistryData( 4 ).ShomateA = 29.0373;
		GasPhaseThermoChemistryData( 4 ).ShomateB = 10.2573;
		GasPhaseThermoChemistryData( 4 ).ShomateC = 2.81048;
		GasPhaseThermoChemistryData( 4 ).ShomateD = -0.95914;
		GasPhaseThermoChemistryData( 4 ).ShomateE = 0.11725;
		GasPhaseThermoChemistryData( 4 ).ShomateF = -250.569;
		GasPhaseThermoChemistryData( 4 ).ShomateG = 223.3967;
		GasPhaseThermoChemistryData( 4 ).ShomateH = -241.8264;
		GasPhaseThermoChemistryData( 4 ).NumCarbons = 0.0;
		GasPhaseThermoChemistryData( 4 ).NumHydrogens = 2.0;
		GasPhaseThermoChemistryData( 4 ).NumOxygens = 1.0;
		GasPhaseThermoChemistryData( 4 ).MolecularWeight = 18.02;

		// Argon (Ar)  Temp K 298-600

		GasPhaseThermoChemistryData( 5 ).ConstituentName = "Argon";
		GasPhaseThermoChemistryData( 5 ).ConstituentFormula = "Ar";
		GasPhaseThermoChemistryData( 5 ).StdRefMolarEnthOfForm = 0.0;
		GasPhaseThermoChemistryData( 5 ).ThermoMode = NISTShomate;
		GasPhaseThermoChemistryData( 5 ).ShomateA = 20.786;
		GasPhaseThermoChemistryData( 5 ).ShomateB = 2.825911e-07;
		GasPhaseThermoChemistryData( 5 ).ShomateC = -1.464191e-07;
		GasPhaseThermoChemistryData( 5 ).ShomateD = 1.092131e-08;
		GasPhaseThermoChemistryData( 5 ).ShomateE = -3.661371e-08;
		GasPhaseThermoChemistryData( 5 ).ShomateF = -6.19735;
		GasPhaseThermoChemistryData( 5 ).ShomateG = 179.999;
		GasPhaseThermoChemistryData( 5 ).ShomateH = 0.0;
		GasPhaseThermoChemistryData( 5 ).NumCarbons = 0.0;
		GasPhaseThermoChemistryData( 5 ).NumHydrogens = 0.0;
		GasPhaseThermoChemistryData( 5 ).NumOxygens = 0.0;
		GasPhaseThermoChemistryData( 5 ).MolecularWeight = 39.95;

		// Hydrogen (H2) Temp K 298-1000
		GasPhaseThermoChemistryData( 6 ).ConstituentName = "Hydrogen";
		GasPhaseThermoChemistryData( 6 ).ConstituentFormula = "H2";
		GasPhaseThermoChemistryData( 6 ).StdRefMolarEnthOfForm = 0.0;
		GasPhaseThermoChemistryData( 6 ).ThermoMode = NISTShomate;
		GasPhaseThermoChemistryData( 6 ).ShomateA = 33.066178;
		GasPhaseThermoChemistryData( 6 ).ShomateB = -11.363417;
		GasPhaseThermoChemistryData( 6 ).ShomateC = 11.432816;
		GasPhaseThermoChemistryData( 6 ).ShomateD = -2.772874;
		GasPhaseThermoChemistryData( 6 ).ShomateE = -0.158558;
		GasPhaseThermoChemistryData( 6 ).ShomateF = -9.980797;
		GasPhaseThermoChemistryData( 6 ).ShomateG = 172.707974;
		GasPhaseThermoChemistryData( 6 ).ShomateH = 0.0;
		GasPhaseThermoChemistryData( 6 ).NumCarbons = 0.0;
		GasPhaseThermoChemistryData( 6 ).NumHydrogens = 2.0;
		GasPhaseThermoChemistryData( 6 ).NumOxygens = 0.0;
		GasPhaseThermoChemistryData( 6 ).MolecularWeight = 2.02;

		// Methane (CH4) Temp K 298-1300
		GasPhaseThermoChemistryData( 7 ).ConstituentName = "Methane";
		GasPhaseThermoChemistryData( 7 ).ConstituentFormula = "CH4";
		GasPhaseThermoChemistryData( 7 ).StdRefMolarEnthOfForm = -74.8731; //KJ/mol (Chase 1998)
		GasPhaseThermoChemistryData( 7 ).ThermoMode = NISTShomate;
		GasPhaseThermoChemistryData( 7 ).ShomateA = -0.703029;
		GasPhaseThermoChemistryData( 7 ).ShomateB = 108.4773;
		GasPhaseThermoChemistryData( 7 ).ShomateC = -42.52157;
		GasPhaseThermoChemistryData( 7 ).ShomateD = 5.862788;
		GasPhaseThermoChemistryData( 7 ).ShomateE = 0.678565;
		GasPhaseThermoChemistryData( 7 ).ShomateF = -76.84376;
		GasPhaseThermoChemistryData( 7 ).ShomateG = 158.7163;
		GasPhaseThermoChemistryData( 7 ).ShomateH = -74.87310;
		GasPhaseThermoChemistryData( 7 ).NumCarbons = 1.0;
		GasPhaseThermoChemistryData( 7 ).NumHydrogens = 4.0;
		GasPhaseThermoChemistryData( 7 ).NumOxygens = 0.0;
		GasPhaseThermoChemistryData( 7 ).MolecularWeight = 16.04;

		// Ethane (C2H6)
		GasPhaseThermoChemistryData( 8 ).ConstituentName = "Ethane";
		GasPhaseThermoChemistryData( 8 ).ConstituentFormula = "C2H6";
		GasPhaseThermoChemistryData( 8 ).StdRefMolarEnthOfForm = -83.8605; // -83.8 !KJ/mol (Pittam and Pilcher 1972)
		GasPhaseThermoChemistryData( 8 ).ThermoMode = NISTShomate;
		GasPhaseThermoChemistryData( 8 ).ShomateA = -3.03849;
		GasPhaseThermoChemistryData( 8 ).ShomateB = 199.202;
		GasPhaseThermoChemistryData( 8 ).ShomateC = -84.9812;
		GasPhaseThermoChemistryData( 8 ).ShomateD = 11.0348;
		GasPhaseThermoChemistryData( 8 ).ShomateE = 0.30348;
		GasPhaseThermoChemistryData( 8 ).ShomateF = -90.0633;
		GasPhaseThermoChemistryData( 8 ).ShomateG = -999.0;
		GasPhaseThermoChemistryData( 8 ).ShomateH = -83.8605;
		GasPhaseThermoChemistryData( 8 ).NumCarbons = 2.0;
		GasPhaseThermoChemistryData( 8 ).NumHydrogens = 6.0;
		GasPhaseThermoChemistryData( 8 ).NumOxygens = 0.0;
		GasPhaseThermoChemistryData( 8 ).MolecularWeight = 30.07;
		GasPhaseThermoChemistryData( 8 ).NASA_A1 = 0.14625388e+01;
		GasPhaseThermoChemistryData( 8 ).NASA_A2 = 0.15494667e-01;
		GasPhaseThermoChemistryData( 8 ).NASA_A3 = 0.05780507e-04;
		GasPhaseThermoChemistryData( 8 ).NASA_A4 = -0.12578319e-07;
		GasPhaseThermoChemistryData( 8 ).NASA_A5 = 0.04586267e-10;
		GasPhaseThermoChemistryData( 8 ).NASA_A6 = -0.11239176e+05;
		GasPhaseThermoChemistryData( 8 ).NASA_A7 = 0.14432295e+02;

		// Propane (C3H8)
		GasPhaseThermoChemistryData( 9 ).ConstituentName = "Propane";
		GasPhaseThermoChemistryData( 9 ).ConstituentFormula = "C3H8";
		GasPhaseThermoChemistryData( 9 ).StdRefMolarEnthOfForm = -103.855; //  -104.7 !kJ/mol  (Pittam and Pilcher 1972)
		GasPhaseThermoChemistryData( 9 ).ThermoMode = NISTShomate;
		GasPhaseThermoChemistryData( 9 ).ShomateA = -23.1747;
		GasPhaseThermoChemistryData( 9 ).ShomateB = 363.742;
		GasPhaseThermoChemistryData( 9 ).ShomateC = -222.981;
		GasPhaseThermoChemistryData( 9 ).ShomateD = 56.253;
		GasPhaseThermoChemistryData( 9 ).ShomateE = 0.61164;
		GasPhaseThermoChemistryData( 9 ).ShomateF = -109.206;
		GasPhaseThermoChemistryData( 9 ).ShomateG = -999.0;
		GasPhaseThermoChemistryData( 9 ).ShomateH = -103.855;
		GasPhaseThermoChemistryData( 9 ).NumCarbons = 3.0;
		GasPhaseThermoChemistryData( 9 ).NumHydrogens = 8.0;
		GasPhaseThermoChemistryData( 9 ).NumOxygens = 0.0;
		GasPhaseThermoChemistryData( 9 ).MolecularWeight = 44.10;
		GasPhaseThermoChemistryData( 9 ).NASA_A1 = 0.08969208e+01;
		GasPhaseThermoChemistryData( 9 ).NASA_A2 = 0.02668986e+00;
		GasPhaseThermoChemistryData( 9 ).NASA_A3 = 0.05431425e-04;
		GasPhaseThermoChemistryData( 9 ).NASA_A4 = -0.02126000e-06;
		GasPhaseThermoChemistryData( 9 ).NASA_A5 = 0.09243330e-10;
		GasPhaseThermoChemistryData( 9 ).NASA_A6 = -0.13954918e+05;
		GasPhaseThermoChemistryData( 9 ).NASA_A7 = 0.01935533e+03;

		// Butane (C4H10)
		GasPhaseThermoChemistryData( 10 ).ConstituentName = "Butane";
		GasPhaseThermoChemistryData( 10 ).ConstituentFormula = "C4H10";
		GasPhaseThermoChemistryData( 10 ).StdRefMolarEnthOfForm = -133.218; // -125.6 !kJ/mol  (Pittam and Pilcher 1972)
		GasPhaseThermoChemistryData( 10 ).ThermoMode = NISTShomate;
		GasPhaseThermoChemistryData( 10 ).ShomateA = -5.24343;
		GasPhaseThermoChemistryData( 10 ).ShomateB = 426.442;
		GasPhaseThermoChemistryData( 10 ).ShomateC = -257.955;
		GasPhaseThermoChemistryData( 10 ).ShomateD = 66.535;
		GasPhaseThermoChemistryData( 10 ).ShomateE = -0.26994;
		GasPhaseThermoChemistryData( 10 ).ShomateF = -149.365;
		GasPhaseThermoChemistryData( 10 ).ShomateG = -999.0;
		GasPhaseThermoChemistryData( 10 ).ShomateH = -133.218;
		GasPhaseThermoChemistryData( 10 ).NumCarbons = 4.0;
		GasPhaseThermoChemistryData( 10 ).NumHydrogens = 10.0;
		GasPhaseThermoChemistryData( 10 ).NumOxygens = 0.0;
		GasPhaseThermoChemistryData( 10 ).MolecularWeight = 58.12;
		GasPhaseThermoChemistryData( 10 ).NASA_A1 = -0.02256618e+02;
		GasPhaseThermoChemistryData( 10 ).NASA_A2 = 0.05881732e+00;
		GasPhaseThermoChemistryData( 10 ).NASA_A3 = -0.04525782e-03;
		GasPhaseThermoChemistryData( 10 ).NASA_A4 = 0.02037115e-06;
		GasPhaseThermoChemistryData( 10 ).NASA_A5 = -0.04079458e-10;
		GasPhaseThermoChemistryData( 10 ).NASA_A6 = -0.01760233e+06;
		GasPhaseThermoChemistryData( 10 ).NASA_A7 = 0.03329595e+03;

		// Pentane (C5H12)
		GasPhaseThermoChemistryData( 11 ).ConstituentName = "Pentane";
		GasPhaseThermoChemistryData( 11 ).ConstituentFormula = "C5H12";
		GasPhaseThermoChemistryData( 11 ).StdRefMolarEnthOfForm = -146.348; // -146.8 !kJ/mol (Good 1970)
		GasPhaseThermoChemistryData( 11 ).ThermoMode = NISTShomate;
		GasPhaseThermoChemistryData( 11 ).ShomateA = -34.9431;
		GasPhaseThermoChemistryData( 11 ).ShomateB = 576.777;
		GasPhaseThermoChemistryData( 11 ).ShomateC = -338.353;
		GasPhaseThermoChemistryData( 11 ).ShomateD = 76.8232;
		GasPhaseThermoChemistryData( 11 ).ShomateE = 1.00948;
		GasPhaseThermoChemistryData( 11 ).ShomateF = -155.348;
		GasPhaseThermoChemistryData( 11 ).ShomateG = -999.0;
		GasPhaseThermoChemistryData( 11 ).ShomateH = -146.348;
		GasPhaseThermoChemistryData( 11 ).NumCarbons = 5.0;
		GasPhaseThermoChemistryData( 11 ).NumHydrogens = 12.0;
		GasPhaseThermoChemistryData( 11 ).NumOxygens = 0.0;
		GasPhaseThermoChemistryData( 11 ).MolecularWeight = 72.15;
		GasPhaseThermoChemistryData( 11 ).NASA_A1 = 0.01877907e+02;
		GasPhaseThermoChemistryData( 11 ).NASA_A2 = 0.04121645e+00;
		GasPhaseThermoChemistryData( 11 ).NASA_A3 = 0.12532337e-04;
		GasPhaseThermoChemistryData( 11 ).NASA_A4 = -0.03701536e-06;
		GasPhaseThermoChemistryData( 11 ).NASA_A5 = 0.15255685e-10;
		GasPhaseThermoChemistryData( 11 ).NASA_A6 = -0.02003815e+06;
		GasPhaseThermoChemistryData( 11 ).NASA_A7 = 0.01877256e+03;

		// Hexane  (C6H14)
		GasPhaseThermoChemistryData( 12 ).ConstituentName = "Hexane";
		GasPhaseThermoChemistryData( 12 ).ConstituentFormula = "C6H14";
		GasPhaseThermoChemistryData( 12 ).StdRefMolarEnthOfForm = -166.966; // -167.2 !kJ/mol (Prosen and Rossini 1945)
		GasPhaseThermoChemistryData( 12 ).ThermoMode = NISTShomate;
		GasPhaseThermoChemistryData( 12 ).ShomateA = -46.7786;
		GasPhaseThermoChemistryData( 12 ).ShomateB = 711.187;
		GasPhaseThermoChemistryData( 12 ).ShomateC = -438.39;
		GasPhaseThermoChemistryData( 12 ).ShomateD = 103.784;
		GasPhaseThermoChemistryData( 12 ).ShomateE = 1.23887;
		GasPhaseThermoChemistryData( 12 ).ShomateF = -176.813;
		GasPhaseThermoChemistryData( 12 ).ShomateG = -999.0;
		GasPhaseThermoChemistryData( 12 ).ShomateH = -166.966;
		GasPhaseThermoChemistryData( 12 ).NumCarbons = 6.0;
		GasPhaseThermoChemistryData( 12 ).NumHydrogens = 14.0;
		GasPhaseThermoChemistryData( 12 ).NumOxygens = 0.0;
		GasPhaseThermoChemistryData( 12 ).MolecularWeight = 86.18;
		GasPhaseThermoChemistryData( 12 ).NASA_A1 = 0.01836174e+02;
		GasPhaseThermoChemistryData( 12 ).NASA_A2 = 0.05098461e+00;
		GasPhaseThermoChemistryData( 12 ).NASA_A3 = 0.12595857e-04;
		GasPhaseThermoChemistryData( 12 ).NASA_A4 = -0.04428362e-06;
		GasPhaseThermoChemistryData( 12 ).NASA_A5 = 0.01872237e-09;
		GasPhaseThermoChemistryData( 12 ).NASA_A6 = -0.02292749e+06;
		GasPhaseThermoChemistryData( 12 ).NASA_A7 = 0.02088145e+03;

		// Methanol (CH3OH)
		// No Shomate coefficients???
		GasPhaseThermoChemistryData( 13 ).ConstituentName = "Methanol";
		GasPhaseThermoChemistryData( 13 ).ConstituentFormula = "CH3OH";
		GasPhaseThermoChemistryData( 13 ).StdRefMolarEnthOfForm = -201.102; // -201.0 !kJ/mol (Hine and Arata 1976)
		GasPhaseThermoChemistryData( 13 ).ThermoMode = NISTShomate;
		GasPhaseThermoChemistryData( 13 ).ShomateA = 14.1952;
		GasPhaseThermoChemistryData( 13 ).ShomateB = 97.7218;
		GasPhaseThermoChemistryData( 13 ).ShomateC = -9.73279;
		GasPhaseThermoChemistryData( 13 ).ShomateD = -12.8461;
		GasPhaseThermoChemistryData( 13 ).ShomateE = 0.15819;
		GasPhaseThermoChemistryData( 13 ).ShomateF = -209.037;
		GasPhaseThermoChemistryData( 13 ).ShomateG = -999.0;
		GasPhaseThermoChemistryData( 13 ).ShomateH = -201.102;
		GasPhaseThermoChemistryData( 13 ).NumCarbons = 1.0;
		GasPhaseThermoChemistryData( 13 ).NumHydrogens = 4.0;
		GasPhaseThermoChemistryData( 13 ).NumOxygens = 1.0;
		GasPhaseThermoChemistryData( 13 ).MolecularWeight = 32.04;
		GasPhaseThermoChemistryData( 13 ).NASA_A1 = 0.02660115e+02;
		GasPhaseThermoChemistryData( 13 ).NASA_A2 = 0.07341508e-01;
		GasPhaseThermoChemistryData( 13 ).NASA_A3 = 0.07170050e-04;
		GasPhaseThermoChemistryData( 13 ).NASA_A4 = -0.08793194e-07;
		GasPhaseThermoChemistryData( 13 ).NASA_A5 = 0.02390570e-10;
		GasPhaseThermoChemistryData( 13 ).NASA_A6 = -0.02535348e+06;
		GasPhaseThermoChemistryData( 13 ).NASA_A7 = 0.11232631e+02;

		// Ethanol (C2H5OH)
		// No Shomate coefficients???
		GasPhaseThermoChemistryData( 14 ).ConstituentName = "Ethanol";
		GasPhaseThermoChemistryData( 14 ).ConstituentFormula = "C2H5OH";
		GasPhaseThermoChemistryData( 14 ).StdRefMolarEnthOfForm = -234.441; //  -235.3 !kJ/mol (Green 1960)
		GasPhaseThermoChemistryData( 14 ).ThermoMode = NISTShomate;
		GasPhaseThermoChemistryData( 14 ).ShomateA = -8.87256;
		GasPhaseThermoChemistryData( 14 ).ShomateB = 282.389;
		GasPhaseThermoChemistryData( 14 ).ShomateC = -178.85;
		GasPhaseThermoChemistryData( 14 ).ShomateD = 46.3528;
		GasPhaseThermoChemistryData( 14 ).ShomateE = 0.48364;
		GasPhaseThermoChemistryData( 14 ).ShomateF = -241.239;
		GasPhaseThermoChemistryData( 14 ).ShomateG = -999.0;
		GasPhaseThermoChemistryData( 14 ).ShomateH = -234.441;
		GasPhaseThermoChemistryData( 14 ).NumCarbons = 2.0;
		GasPhaseThermoChemistryData( 14 ).NumHydrogens = 6.0;
		GasPhaseThermoChemistryData( 14 ).NumOxygens = 1.0;
		GasPhaseThermoChemistryData( 14 ).MolecularWeight = 46.07;
		GasPhaseThermoChemistryData( 14 ).NASA_A1 = 0.18461027e+01;
		GasPhaseThermoChemistryData( 14 ).NASA_A2 = 0.20475008e-01;
		GasPhaseThermoChemistryData( 14 ).NASA_A3 = 0.39904089e-05;
		GasPhaseThermoChemistryData( 14 ).NASA_A4 = -0.16585986e-07;
		GasPhaseThermoChemistryData( 14 ).NASA_A5 = 0.73090440e-11;
		GasPhaseThermoChemistryData( 14 ).NASA_A6 = -0.29663086e+05;
		GasPhaseThermoChemistryData( 14 ).NASA_A7 = 0.17289993e+02;

		if ( FuelSupply( FuelSupplyNum ).FuelTypeMode == fuelModeGaseousConstituents ) {
			// now calculate LHV of fuel for entire simulation

			// sum over each constituent
			O2Stoic = 0.0;
			CO2ProdStoic = 0.0;
			H20ProdStoic = 0.0;
			CO2dataID = 1; //hard-coded above
			WaterDataID = 4; //hard-coded above
			// Loop over fuel constituents and do one-time setup
			for ( i = 1; i <= FuelSupply( FuelSupplyNum ).NumConstituents; ++i ) {

				thisName = FuelSupply( FuelSupplyNum ).ConstitName( i );
				thisGasID = FindItem( thisName, GasPhaseThermoChemistryData, &GasPropertyDataStruct::ConstituentName );
				FuelSupply( FuelSupplyNum ).GasLibID( i ) = thisGasID;

				if ( thisGasID == 0 ) {
					ShowSevereError( "Fuel constituent not found in thermochemistry data: " + thisName );
					ErrorsFound = true;
				}

				//for this fuel mixture, figure stoichiometric oxygen requirement
				O2Stoic += FuelSupply( FuelSupplyNum ).ConstitMolalFract( i ) * ( GasPhaseThermoChemistryData( thisGasID ).NumCarbons + GasPhaseThermoChemistryData( thisGasID ).NumHydrogens / 4.0 - GasPhaseThermoChemistryData( thisGasID ).NumOxygens / 2.0 );
				// for this fuel mixture, figure stoichiometric Carbon Dioxide in Product Gases

				CO2ProdStoic += FuelSupply( FuelSupplyNum ).ConstitMolalFract( i ) * GasPhaseThermoChemistryData( thisGasID ).NumCarbons;

				H20ProdStoic += FuelSupply( FuelSupplyNum ).ConstitMolalFract( i ) * GasPhaseThermoChemistryData( thisGasID ).NumHydrogens / 2.0;
			}

			FuelSupply( FuelSupplyNum ).StoicOxygenRate = O2Stoic;
			FuelSupply( FuelSupplyNum ).CO2ProductGasCoef = CO2ProdStoic;
			FuelSupply( FuelSupplyNum ).H20ProductGasCoef = H20ProdStoic;

			//Calculate LHV for an NdotFuel of 1.0
			LHVfuel = 0.0;
			for ( i = 1; i <= FuelSupply( FuelSupplyNum ).NumConstituents; ++i ) {
				thisGasID = FuelSupply( FuelSupplyNum ).GasLibID( i );
				if ( GasPhaseThermoChemistryData( thisGasID ).NumHydrogens == 0.0 ) {
					LHVi = 0.0;
				} else {
					LHVi = GasPhaseThermoChemistryData( thisGasID ).StdRefMolarEnthOfForm - GasPhaseThermoChemistryData( thisGasID ).NumCarbons * GasPhaseThermoChemistryData( CO2dataID ).StdRefMolarEnthOfForm - ( GasPhaseThermoChemistryData( thisGasID ).NumHydrogens / 2.0 ) * GasPhaseThermoChemistryData( WaterDataID ).StdRefMolarEnthOfForm;
				}
				LHVfuel += LHVi * FuelSupply( FuelSupplyNum ).ConstitMolalFract( i );
			}
			FuelSupply( FuelSupplyNum ).LHV = LHVfuel;

			//Calculate HHV for an NdotFuel of 1.0
			HHVfuel = 0.0;
			for ( i = 1; i <= FuelSupply( FuelSupplyNum ).NumConstituents; ++i ) {
				thisGasID = FuelSupply( FuelSupplyNum ).GasLibID( i );
				if ( GasPhaseThermoChemistryData( thisGasID ).NumHydrogens == 0.0 ) {
					HHVi = 0.0;
				} else {
					HHVi = GasPhaseThermoChemistryData( thisGasID ).StdRefMolarEnthOfForm - GasPhaseThermoChemistryData( thisGasID ).NumCarbons * GasPhaseThermoChemistryData( CO2dataID ).StdRefMolarEnthOfForm - ( GasPhaseThermoChemistryData( thisGasID ).NumHydrogens / 2.0 ) * GasPhaseThermoChemistryData( WaterDataID ).StdRefMolarEnthOfForm + ( GasPhaseThermoChemistryData( thisGasID ).NumHydrogens / 2.0 ) * ( GasPhaseThermoChemistryData( WaterDataID ).StdRefMolarEnthOfForm + 285.8304 );
				}
				HHVfuel += HHVi * FuelSupply( FuelSupplyNum ).ConstitMolalFract( i );
			}

			//Calculate Molecular Weight for this fuel
			MWfuel = 0.0;
			for ( i = 1; i <= FuelSupply( FuelSupplyNum ).NumConstituents; ++i ) {
				thisGasID = FuelSupply( FuelSupplyNum ).GasLibID( i );
				MWfuel += FuelSupply( FuelSupplyNum ).ConstitMolalFract( i ) * GasPhaseThermoChemistryData( thisGasID ).MolecularWeight;
			}
			FuelSupply( FuelSupplyNum ).MW = MWfuel;
			FuelSupply( FuelSupplyNum ).KmolPerSecToKgPerSec = MWfuel; //TODO check this, guessing on conversion...
			FuelSupply( FuelSupplyNum ).HHV = 1000000.0 * HHVfuel / MWfuel; // (1000/k) (1000/k) (kJ/mol)/(g/mol) = J/kg
			FuelSupply( FuelSupplyNum ).LHVJperkg = FuelSupply( FuelSupplyNum ).LHV * 1000000.0 / FuelSupply( FuelSupplyNum ).MW;

		} else if ( FuelSupply( FuelSupplyNum ).FuelTypeMode == fuelModeGenericLiquid ) {
			FuelSupply( FuelSupplyNum ).LHV = FuelSupply( FuelSupplyNum ).LHVliquid * FuelSupply( FuelSupplyNum ).MW / 1000000.0; // J/kg * g/mol (k/1000) (k/10000)

		} else {

		}

		// report Heating Values in EIO.
		gio::write( OutputFileInits, fmtA ) << "! <Fuel Supply>, Fuel Supply Name, Lower Heating Value [J/kmol], Lower Heating Value [kJ/kg], Higher Heating Value [KJ/kg],  Molecular Weight [g/mol] ";
		gio::write( OutputFileInits, Format_501 ) << FuelSupply( FuelSupplyNum ).Name << FuelSupply( FuelSupplyNum ).LHV * 1000000.0 << FuelSupply( FuelSupplyNum ).LHVJperkg / 1000.0 << FuelSupply( FuelSupplyNum ).HHV / 1000.0 << FuelSupply( FuelSupplyNum ).MW;

	}

} // GeneratorFuelSupply

} // EnergyPlus
