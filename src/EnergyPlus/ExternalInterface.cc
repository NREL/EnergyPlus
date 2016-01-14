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

// FMI-Related Headers
extern "C" {
#include <FMI/main.h>
#include <BCVTB/utilSocket.h>
#include <BCVTB/utilXml.h>
}


// C++ Headers
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <ExternalInterface.hh>
#include <DataEnvironment.hh>
#include <DataIPShortCuts.hh>
#include <DataPrecisionGlobals.hh>
#include <DataStringGlobals.hh>
#include <DataSystemVariables.hh>
#include <EMSManager.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <OutputProcessor.hh>
#include <RuntimeLanguageProcessor.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>
#include <DisplayRoutines.hh>


namespace EnergyPlus {

namespace ExternalInterface {

	// Module containing the routines dealing with the BCVTB interface

	// MODULE INFORMATION:
	//       AUTHOR         Michael Wetter
	//       DATE WRITTEN   2Dec2007
	//       MODIFIED       Rui Zhang July 2009
	//       MODIFIED       Thierry S. Nouidui 2011
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To encapsulate the data and routines required to interface
	// the Building Controls Virtual Test Bed (BCVTB) and FunctionalMockupUnits (FMU)

	// REFERENCES:
	// http://simulationresearch.lbl.gov/bcvtb
	// http://www.modelisar.com

	// Data
	Real64 tComm( 0.0 ); // Communication time step
	Real64 tStop( 3600.0 ); // Stop time used during the warmup period
	Real64 tStart( 0.0 ); // Start time used during the warmup period
	Real64 hStep( 15.0 ); // Communication step size
	bool FlagReIni( false ); // Flag for reinitialization of states in GetSetAndDoStep
	std::string FMURootWorkingFolder; // FMU root working folder

	// MODULE PARAMETER DEFINITIONS:
	int const maxVar( 1024 ); // Maximum number of variables to be exchanged
	int const maxErrMsgLength( 10000 ); // Maximum error message length from xml schema validation
	int const indexSchedule( 1 ); // Index for schedule in inpVarTypes
	int const indexVariable( 2 ); // Index for variable in inpVarTypes
	int const indexActuator( 3 ); // Index for actuator in inpVarTypes
	int nInKeys( 3 ); // Number of input variables available in ExternalInterface (=highest index* number)
	int const fmiOK( 0 ); // fmiOK
	int const fmiWarning( 1 ); // fmiWarning
	int const fmiDiscard( 2 ); // fmiDiscard
	int const fmiError( 3 ); // fmiError
	int const fmiFatal( 4 ); // fmiPending
	int const fmiPending( 5 ); // fmiPending
	std::string const socCfgFilNam( "socket.cfg" ); // socket configuration file
	std::string const BlankString;

	// MODULE VARIABLE DECLARATIONS:

	int NumExternalInterfaces( 0 ); // Number of ExternalInterface objects
	int NumExternalInterfacesBCVTB( 0 ); // Number of BCVTB ExternalInterface objects
	int NumExternalInterfacesFMUImport( 0 ); // Number of FMU ExternalInterface objects
	int NumExternalInterfacesFMUExport( 0 ); // Number of FMU ExternalInterface objects
	int NumFMUObjects( 0 ); // Number of FMU objects
	int FMUExportActivate( 0 ); // FMU Export flag
	bool haveExternalInterfaceBCVTB( false ); // Flag for BCVTB interface
	bool haveExternalInterfaceFMUImport( false ); // Flag for FMU-Import interface
	bool haveExternalInterfaceFMUExport( false ); // Flag for FMU-Export interface
	int simulationStatus( 1 ); // Status flag. Used to report during
	// which phase an error occured.
	// (1=initialization, 2=time stepping)

	Array1D_int keyVarIndexes; // Array index for specific key name
	Array1D_int varTypes; // Types of variables in keyVarIndexes
	Array1D_int varInd; // Index of ErlVariables for ExternalInterface
	int socketFD( -1 ); // socket file descriptor
	bool ErrorsFound( false ); // Set to true if errors are found
	bool noMoreValues( false ); // Flag, true if no more values
	// will be sent by the server

	Array1D_string varKeys; // Keys of report variables used for data exchange
	Array1D_string varNames; // Names of report variables used for data exchange
	Array1D_int inpVarTypes; // Names of report variables used for data exchange
	Array1D_string inpVarNames; // Names of report variables used for data exchange

	bool configuredControlPoints( false ); // True if control points have been configured
	bool useEMS( false ); // Will be set to true if ExternalInterface writes to EMS variables or actuators

	// SUBROUTINE SPECIFICATIONS FOR MODULE ExternalInterface:

	// Object Data
	Array1D< FMUType > FMU; // Variable Types structure
	Array1D< FMUType > FMUTemp; // Variable Types structure
	Array1D< checkFMUInstanceNameType > checkInstanceName; // Variable Types structure for checking instance names

	// Functions

	void
	ExternalInterfaceExchangeVariables()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Michael Wetter
		//       DATE WRITTEN   2Dec2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Exchanges variables between EnergyPlus and the BCVTB socket.

		// Using/Aliasing
		using DataGlobals::WarmupFlag;
		using DataGlobals::KindOfSim;
		using DataGlobals::ksRunPeriodWeather;

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool GetInputFlag( true ); // First time, input is "gotten"
		std::string errorMessage; // Error message
		int retValErrMsg;

		if ( GetInputFlag ) {
			GetExternalInterfaceInput();
			GetInputFlag = false;
		}

		if ( haveExternalInterfaceBCVTB || haveExternalInterfaceFMUExport ) {
			InitExternalInterface();
			// Exchange data only after sizing and after warm-up.
			// Note that checking for ZoneSizingCalc SysSizingCalc does not work here, hence we
			// use the KindOfSim flag
			if ( !WarmupFlag && ( KindOfSim == ksRunPeriodWeather ) ) {
				CalcExternalInterface();
			}
		}

		if ( haveExternalInterfaceFMUImport ) {
			char * errorMessagePtr( &errorMessage[0] );
			retValErrMsg = checkOperatingSystem( errorMessagePtr );
			if ( retValErrMsg != 0 ) {
				ShowSevereError( "ExternalInterface/ExternalInterfaceExchangeVariables:" + std::string( errorMessagePtr ) );
				ErrorsFound = true;
				StopExternalInterfaceIfError();
			}
			// initialize the FunctionalMockupUnitImport interface
			InitExternalInterfaceFMUImport();
			// No Data exchange during design days
			// Data Exchange data during warmup and after warmup
			CalcExternalInterfaceFMUImport();
		}

	}

	void
	GetExternalInterfaceInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Michael Wetter
		//       DATE WRITTEN   2Dec2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Obtains input data for ExternalInterface

		// METHODOLOGY EMPLOYED:
		// Uses InputProcessor "Get" routines to obtain data.

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using DataIPShortCuts::cCurrentModuleObject;
		using DataIPShortCuts::cAlphaArgs;
		using DataIPShortCuts::rNumericArgs;
		using DataIPShortCuts::cAlphaFieldNames;
		using DataIPShortCuts::cNumericFieldNames;

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		int IOStatus; // Used in GetObjectItem
		int Loop; // Loop counter

		cCurrentModuleObject = "ExternalInterface";
		NumExternalInterfaces = GetNumObjectsFound( cCurrentModuleObject );

		for ( Loop = 1; Loop <= NumExternalInterfaces; ++Loop ) { // This loop determines whether the external interface is for FMU or BCVTB
			GetObjectItem( cCurrentModuleObject, Loop, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, _, _, cAlphaFieldNames, cNumericFieldNames );
			if ( SameString( cAlphaArgs( 1 ), "PtolemyServer" ) ) { // The BCVTB interface is activated.
				++NumExternalInterfacesBCVTB;
			} else if ( SameString( cAlphaArgs( 1 ), "FunctionalMockupUnitImport" ) ) { // The functional mock up unit import interface is activated.
				++NumExternalInterfacesFMUImport;
			} else if ( SameString( cAlphaArgs( 1 ), "FunctionalMockupUnitExport" ) ) { // The functional mock up unit export interface is activated.
				++NumExternalInterfacesFMUExport;
			}
		}

		// Check if objects are used although BCVTB interface object is not defined
		if ( NumExternalInterfacesBCVTB == 0 ) {
			WarnIfExternalInterfaceObjectsAreUsed( "ExternalInterface:Schedule" );
			WarnIfExternalInterfaceObjectsAreUsed( "ExternalInterface:Variable" );
			WarnIfExternalInterfaceObjectsAreUsed( "ExternalInterface:Actuator" );
		}

		// Check if objects are used although FMUExport interface is not defined
		if ( NumExternalInterfacesFMUExport == 0 ) {
			WarnIfExternalInterfaceObjectsAreUsed( "ExternalInterface:FunctionalMockupUnitExport:To:Schedule" );
			WarnIfExternalInterfaceObjectsAreUsed( "ExternalInterface:FunctionalMockupUnitExport:To:Variable" );
			WarnIfExternalInterfaceObjectsAreUsed( "ExternalInterface:FunctionalMockupUnitExport:To:Actuator" );
		}

		// Check if objects are used although FMU Import interface is not defined
		if ( NumExternalInterfacesFMUImport == 0 ) {
			WarnIfExternalInterfaceObjectsAreUsed( "ExternalInterface:FunctionalMockupUnitImport:To:Schedule" );
			WarnIfExternalInterfaceObjectsAreUsed( "ExternalInterface:FunctionalMockupUnitImport:To:Variable" );
			WarnIfExternalInterfaceObjectsAreUsed( "ExternalInterface:FunctionalMockupUnitImport:To:Actuator" );
		}

		if ( ( NumExternalInterfacesBCVTB == 1 ) && ( NumExternalInterfacesFMUExport == 0 ) ) {
			haveExternalInterfaceBCVTB = true;
			DisplayString( "Instantiating Building Controls Virtual Test Bed" );
			varKeys.allocate( maxVar ); // Keys of report variables used for data exchange
			varNames.allocate( maxVar ); // Names of report variables used for data exchange
			inpVarTypes.dimension( maxVar, 0 ); // Names of report variables used for data exchange
			inpVarNames.allocate( maxVar ); // Names of report variables used for data exchange
			VerifyExternalInterfaceObject();
		} else if ( ( NumExternalInterfacesBCVTB == 0 ) && ( NumExternalInterfacesFMUExport == 1 ) ) {
			haveExternalInterfaceFMUExport = true;
			FMUExportActivate = 1;
			DisplayString( "Instantiating FunctionalMockupUnitExport interface" );
			varKeys.allocate( maxVar ); // Keys of report variables used for data exchange
			varNames.allocate( maxVar ); // Names of report variables used for data exchange
			inpVarTypes.dimension( maxVar, 0 ); // Names of report variables used for data exchange
			inpVarNames.allocate( maxVar ); // Names of report variables used for data exchange
			VerifyExternalInterfaceObject();
		} else if ( ( NumExternalInterfacesBCVTB == 1 ) && ( NumExternalInterfacesFMUExport != 0 ) ) {
			ShowSevereError( "GetExternalInterfaceInput: Cannot have Ptolemy and FMU-Export interface simultaneously." );
			ErrorsFound = true;
		}

		if ( ( NumExternalInterfacesFMUImport == 1 ) && ( NumExternalInterfacesFMUExport == 0 ) ) {
			haveExternalInterfaceFMUImport = true;
			DisplayString( "Instantiating FunctionalMockupUnitImport interface" );
			cCurrentModuleObject = "ExternalInterface:FunctionalMockupUnitImport";
			NumFMUObjects = GetNumObjectsFound( cCurrentModuleObject );
			VerifyExternalInterfaceObject();
		} else if ( ( NumExternalInterfacesFMUImport == 1 ) && ( NumExternalInterfacesFMUExport != 0 ) ) {
			ShowSevereError( "GetExternalInterfaceInput: Cannot have FMU-Import and FMU-Export interface simultaneously." );
			ErrorsFound = true;
		}

		if ( NumExternalInterfacesBCVTB > 1 ) {
			ShowSevereError( "GetExternalInterfaceInput: Cannot have more than one Ptolemy interface." );
			ShowContinueError( "GetExternalInterfaceInput: Errors found in input." );
			ErrorsFound = true;
		}

		if ( NumExternalInterfacesFMUExport > 1 ) {
			ShowSevereError( "GetExternalInterfaceInput: Cannot have more than one FMU-Export interface." );
			ShowContinueError( "Errors found in input." );
			ErrorsFound = true;
		}

		if ( NumExternalInterfacesFMUImport > 1 ) {
			ShowSevereError( "GetExternalInterfaceInput: Cannot have more than one FMU-Import interface." );
			ShowContinueError( "Errors found in input." );
			ErrorsFound = true;
		}

		if ( ErrorsFound ) {
			ShowFatalError( "GetExternalInterfaceInput: preceding conditions cause termination." );
		}

		StopExternalInterfaceIfError();

	}

	void
	StopExternalInterfaceIfError()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Michael Wetter
		//       DATE WRITTEN   9Jan2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine gracefully stops the ExternalInterface if an error has been found.
		// It sends an appropriate message to the ExternalInterface
		// and then calls a fatal error to stop EnergyPlus.

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int retVal; // Return value, needed to catch return value of function call
		int const flag1( -10 );
		int const flag2( -20 );

		if ( ( NumExternalInterfacesBCVTB != 0 ) || ( NumExternalInterfacesFMUExport != 0 ) ) {
			if ( ErrorsFound ) {
				// Check if the socket is open
				if ( socketFD >= 0 ) {
					// Socket is open
					if ( simulationStatus == 1 ) {
						retVal = sendclientmessage( &socketFD, &flag1 );
					} else {
						retVal = sendclientmessage( &socketFD, &flag2 );
					}
				}
				ShowFatalError( "Error in ExternalInterface: Check EnergyPlus *.err file." );
			}
		}
		if ( NumExternalInterfacesFMUImport != 0 ) {
			if ( ErrorsFound ) {
				ShowFatalError( "ExternalInterface/StopExternalInterfaceIfError: Error in ExternalInterface: Check EnergyPlus *.err file." );
			}
		}
	}

	void
	CloseSocket( int const FlagToWriteToSocket )
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Michael Wetter
		//       DATE WRITTEN   December 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine tries to write the optional error code to the
		// socket and then closes the socket

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// +1: E+ reached final time
		// -1: E+ had some error

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int retVal; // Return value, needed to catch return value of function call
		bool fileExist; // Set to true if file exists

		// Try to establish socket connection. This is needed if Ptolemy started E+,
		//  but E+ had an error before the call to InitExternalInterface.

		{ IOFlags flags; gio::inquire( socCfgFilNam, flags ); fileExist = flags.exists(); }

		if ( ( socketFD == -1 ) && fileExist ) {
			socketFD = establishclientsocket( socCfgFilNam.c_str() );
		}

		if ( socketFD >= 0 ) {
			retVal = sendclientmessage( &socketFD, &FlagToWriteToSocket );
			// Don't close socket as this may give sometimes an IOException in Windows
			// This problem seems to affect only Windows but not Mac
			//     close(socketFD)
		}


	}

	void
	ParseString(
		std::string const & str, // The string, with all elements separated by ';'
		Array1S_string ele, // The elements
		int const nEle // The number of elements
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Michael Wetter
		//       DATE WRITTEN   8Jan2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine parses the semicolon separated string xmlStr
		// and assigns each element to ele

		// Using/Aliasing
		using InputProcessor::MakeUPPERCase;

		// SUBROUTINE VARIABLE DEFINITIONS:
		int i; // Counter
		std::string::size_type iSta; // Start of substring
		std::string::size_type iEnd; // End of substring
		std::string::size_type iCol; // Index of ;
		std::string::size_type lenStr; // Length of string

		lenStr = len( str );
		iEnd = 0;
		for ( i = 1; i <= nEle; ++i ) {
			iSta = iEnd; // add one to skip ';'
			iCol = str.find( ';', iSta );
			if ( iCol != std::string::npos ) {
				iEnd = iCol + 1;
			} else { // Use rest of string
				iEnd = lenStr;
			}
			ele( i ) = MakeUPPERCase( str.substr( iSta, iEnd - iSta - 1 ) );
		}

	}

	void
	InitExternalInterface()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Michael Wetter
		//       DATE WRITTEN   2Dec2007
		//       MODIFIED       Rui Zhang Aug 2009
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for initializations of the ExternalInterface

		// Using/Aliasing
		using ScheduleManager::GetDayScheduleIndex;
		using RuntimeLanguageProcessor::isExternalInterfaceErlVariable;
		using RuntimeLanguageProcessor::FindEMSVariable;
		using General::TrimSigDigits;

		// SUBROUTINE PARAMETER DEFINITIONS:

		static bool firstCall( true ); // First time, input has been read
		std::string const simCfgFilNam("variables.cfg"); // Configuration file
		std::string const xmlStrInKey("schedule,variable,actuator\0"); // xml values in string, separated by ','

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int i; // loop counters
		std::string xmlStrOut; // xml values in string, separated by ';'
		std::string xmlStrOutTyp; // xml values in string, separated by ';'
		std::string xmlStrIn; // xml values in string, separated by ';'
		static int nOutVal; // Number of output values (E+ -> ExternalInterface)
		static int nInpVar; // Number of input values (ExternalInterface -> E+)
		int retVal; // Return value of function call, used for error handling
		int mainVersion; // The version number
		bool socFileExist; // Set to true if socket configuration
		// file exists
		bool simFileExist; // Set to true if simulation configuration
		// file exists

		if ( firstCall ) {
			DisplayString( "ExternalInterface initializes." );
			// do one time initializations

			if ( haveExternalInterfaceBCVTB ) {
				// Check version number
				mainVersion = getmainversionnumber();
				if ( mainVersion < 0 ) {
					ShowSevereError( "ExternalInterface: BCVTB is not installed in this version." );
					ErrorsFound = true;
					StopExternalInterfaceIfError();
				}
			}

			// Get port number
			{ IOFlags flags; gio::inquire( socCfgFilNam, flags ); socFileExist = flags.exists(); }
			if ( socFileExist ) {
				socketFD = establishclientsocket( socCfgFilNam.c_str() );
				if ( socketFD < 0 ) {
					ShowSevereError( "ExternalInterface: Could not open socket. File descriptor = " + TrimSigDigits( socketFD ) + '.' );
					ErrorsFound = true;
				}
			} else {
				ShowSevereError( "ExternalInterface: Did not find file \"" + socCfgFilNam + "\"." );
				ShowContinueError( "This file needs to be in same directory as in.idf." );
				ShowContinueError( "Check the documentation for the ExternalInterface." );
				ErrorsFound = true;
			}

			// Make sure that idf file specified a run period other than
			// design day and system sizing.
			ValidateRunControl();

			StopExternalInterfaceIfError();

			// make a single length here for all strings to be passed to getepvariables
			int lenXmlStr( maxVar * DataGlobals::MaxNameLength ); // Length of strings being passed to getepvariables

			// initialize all the strings to this length with blanks
			xmlStrOut = std::string(lenXmlStr, ' ');
			xmlStrOutTyp = std::string(lenXmlStr, ' ');
			xmlStrIn = std::string(lenXmlStr, ' ');

			// Get input and output variables for EnergyPlus in sequence
			// Check if simCfgFilNam exists.
			{ IOFlags flags; gio::inquire( simCfgFilNam, flags ); simFileExist = flags.exists(); }
			if ( simFileExist ) {

				// preprocess the strings into char vectors before making the library call
				auto xmlStrOutTypArr( getCharArrayFromString( xmlStrOutTyp ) );
				auto xmlStrOutArr( getCharArrayFromString( xmlStrOut ) );
				auto xmlStrInArr( getCharArrayFromString( xmlStrIn ) );

				// now make the library call
				if ( haveExternalInterfaceBCVTB ) {
					retVal = getepvariables(simCfgFilNam.c_str(), &xmlStrOutTypArr[0], &xmlStrOutArr[0], &nOutVal, xmlStrInKey.c_str(), &nInKeys, &xmlStrInArr[0], &nInpVar, inpVarTypes.data_, &lenXmlStr);
				} else if ( haveExternalInterfaceFMUExport ) {
					retVal = getepvariablesFMU(simCfgFilNam.c_str(), &xmlStrOutTypArr[0], &xmlStrOutArr[0], &nOutVal, xmlStrInKey.c_str(), &nInKeys, &xmlStrInArr[0], &nInpVar, inpVarTypes.data_, &lenXmlStr);
				} else {
					//there should be no else condition at this point, however we'll still assign the error value for completeness
					retVal = -1;
				}

				// then postprocess the char vectors in case they are used after the fact
				xmlStrOutTyp = getStringFromCharArray( xmlStrOutTypArr );
				xmlStrOut = getStringFromCharArray( xmlStrOutArr );
				xmlStrIn = getStringFromCharArray( xmlStrInArr );

				xmlStrOutTypArr.clear();
				xmlStrOutArr.clear();
				xmlStrInArr.clear();

				// handle errors when reading variables.cfg file
				if ( retVal < 0 ) {
					ShowSevereError( "ExternalInterface: Error when getting input and output variables for EnergyPlus," );
					ShowContinueError( "check simulation.log for error message." );
					ErrorsFound = true;
				}

			} else {

				ShowSevereError( "ExternalInterface: Did not find file \"" + simCfgFilNam + "\"." );
				ShowContinueError( "This file needs to be in same directory as in.idf." );
				ShowContinueError( "Check the documentation for the ExternalInterface." );
				ErrorsFound = true;

			}
			StopExternalInterfaceIfError();

			if ( nOutVal + nInpVar > maxVar ) {
				ShowSevereError( "ExternalInterface: Too many variables to be exchanged." );
				ShowContinueError( "Attempted to exchange " + TrimSigDigits( nOutVal ) + " outputs" );
				ShowContinueError( "plus " + TrimSigDigits( nOutVal ) + " inputs." );
				ShowContinueError( "Maximum allowed is sum is " + TrimSigDigits( maxVar ) + '.' );
				ShowContinueError( "To fix, increase maxVar in ExternalInterface.cc" );
				ErrorsFound = true;
			}
			StopExternalInterfaceIfError();

			if ( nOutVal < 0 ) {
				ShowSevereError( "ExternalInterface: Error when getting number of xml values for outputs." );
				ErrorsFound = true;
			} else {
				ParseString( xmlStrOut, varNames, nOutVal );
				ParseString( xmlStrOutTyp, varKeys, nOutVal );
			}
			StopExternalInterfaceIfError();

			if ( nInpVar < 0 ) {
				ShowSevereError( "ExternalInterface: Error when getting number of xml values for inputs." );
				ErrorsFound = true;
			} else {
				ParseString( xmlStrIn, inpVarNames, nInpVar );
			}
			StopExternalInterfaceIfError();

			DisplayString( "Number of outputs in ExternalInterface = " + TrimSigDigits( nOutVal ) );
			DisplayString( "Number of inputs  in ExternalInterface = " + TrimSigDigits( nInpVar ) );

			firstCall = false;

		} else if ( ! configuredControlPoints ) {
			keyVarIndexes.allocate( nOutVal );
			varTypes.allocate( nOutVal );
			GetReportVariableKey( varKeys, nOutVal, varNames, keyVarIndexes, varTypes );
			varInd.allocate( nInpVar );
			for ( i = 1; i <= nInpVar; ++i ) {
				if ( inpVarTypes( i ) == indexSchedule ) {
					varInd( i ) = GetDayScheduleIndex( inpVarNames( i ) );
				} else if ( inpVarTypes( i ) == indexVariable ) {
					varInd( i ) = FindEMSVariable( inpVarNames( i ), 0 );
				} else if ( inpVarTypes( i ) == indexActuator ) {
					varInd( i ) = FindEMSVariable( inpVarNames( i ), 0 );
				}
				if ( varInd( i ) <= 0 ) {
					ShowSevereError( "ExternalInterface: Error, xml file \"" + simCfgFilNam + "\" declares variable \"" + inpVarNames( i ) + "\"," );
					ShowContinueError( "but variable was not found in idf file." );
					ErrorsFound = true;
				}
			}
			StopExternalInterfaceIfError();
			// Configure Erl variables
			for ( i = 1; i <= nInpVar; ++i ) {
				if ( inpVarTypes( i ) == indexVariable ) { // ems-globalvariable
					useEMS = true;
					if ( ! isExternalInterfaceErlVariable( varInd( i ) ) ) {
						ShowSevereError( "ExternalInterface: Error, xml file \"" + simCfgFilNam + "\" declares variable \"" + inpVarNames( i ) + "\"," );
						ShowContinueError( "But this variable is an ordinary Erl variable, not an ExternalInterface variable." );
						ShowContinueError( "You must specify a variable of type \"ExternalInterface:Variable\"." );
						ErrorsFound = true;
					}
				} else if ( inpVarTypes( i ) == indexActuator ) { // ems-actuator
					useEMS = true;
					if ( ! isExternalInterfaceErlVariable( varInd( i ) ) ) {
						ShowSevereError( "ExternalInterface: Error, xml file \"" + simCfgFilNam + "\" declares variable \"" + inpVarNames( i ) + "\"," );
						ShowContinueError( "But this variable is an ordinary Erl actuator, not an ExternalInterface actuator." );
						ShowContinueError( "You must specify a variable of type \"ExternalInterface:Actuator\"." );
						ErrorsFound = true;
					}
				}
			}
			configuredControlPoints = true;
		}
		StopExternalInterfaceIfError();

	}

	void
	GetSetVariablesAndDoStepFMUImport()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Thierry S. Nouidui, Michael Wetter, Wangda Zuo
		//       DATE WRITTEN   08Aug2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine gets, sets and does the time integration in FMUs.

		// Using/Aliasing
		using RuntimeLanguageProcessor::isExternalInterfaceErlVariable;
		using RuntimeLanguageProcessor::FindEMSVariable;
		using RuntimeLanguageProcessor::ExternalInterfaceSetErlVariable;
		using ScheduleManager::ExternalInterfaceSetSchedule;
		using EMSManager::ManageEMS;
		using DataGlobals::WarmupFlag;
		using DataGlobals::KindOfSim;
		using DataGlobals::ksRunPeriodWeather;
		using DataGlobals::emsCallFromExternalInterface;
		using General::TrimSigDigits;

		// SUBROUTINE PARAMETER DEFINITIONS:
		static bool FirstCallGetSetDoStep( true ); // Flag to check when External Interface is called first time

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int i, j, k; // Loop counters

		for ( i = 1; i <= NumFMUObjects; ++i ) {
			for ( j = 1; j <= FMU( i ).NumInstances; ++j ) {
				if ( FlagReIni ) {
					// Get from FMUs, values that will be set in EnergyPlus (Schedule)
					for ( k = 1; k <= FMUTemp( i ).Instance( j ).NumOutputVariablesSchedule; ++k ) {
						FMU( i ).Instance( j ).fmuOutputVariableSchedule( k ).RealVarValue = FMUTemp( i ).Instance( j ).fmuOutputVariableSchedule( k ).RealVarValue;
					}

					// Get from FMUs, values that will be set in EnergyPlus (Variable)
					for ( k = 1; k <= FMUTemp( i ).Instance( j ).NumOutputVariablesVariable; ++k ) {
						FMU( i ).Instance( j ).fmuOutputVariableVariable( k ).RealVarValue = FMUTemp( i ).Instance( j ).fmuOutputVariableVariable( k ).RealVarValue;
					}

					// Get from FMUs, values that will be set in EnergyPlus (Actuator)
					for ( k = 1; k <= FMUTemp( i ).Instance( j ).NumOutputVariablesActuator; ++k ) {
						FMU( i ).Instance( j ).fmuOutputVariableActuator( k ).RealVarValue = FMUTemp( i ).Instance( j ).fmuOutputVariableActuator( k ).RealVarValue;
					}
				} else {
					// Get from FMUs, values that will be set in EnergyPlus (Schedule)

					if ( size( FMU(i).Instance(j).fmuOutputVariableSchedule ) > 0 ) {

						// generate vectors here first
						std::vector<unsigned int> valueReferenceVec;
						std::vector<Real64> realVarValueVec;
						for ( unsigned long x = 1; x <= size( FMU( i ).Instance( j ).fmuOutputVariableSchedule ); ++x ) {
							valueReferenceVec.push_back( FMU( i ).Instance( j ).fmuOutputVariableSchedule( x ).ValueReference );
							realVarValueVec.push_back( FMU( i ).Instance( j ).fmuOutputVariableSchedule( x ).RealVarValue );
						}

						// pass in the vectors as pointers to the first member of the vector
						FMU( i ).Instance( j ).fmistatus = fmiEPlusGetReal ( &FMU( i ).Instance( j ).fmicomponent, &valueReferenceVec[0], &realVarValueVec[0], &FMU( i ).Instance( j ).NumOutputVariablesSchedule, &FMU( i ).Instance( j ).Index );

						for ( unsigned long x = 1; x <= size( FMU( i ).Instance( j ).fmuOutputVariableSchedule ); ++x ) {
							FMU( i ).Instance( j ).fmuOutputVariableSchedule( x ).ValueReference = valueReferenceVec[x-1];
							FMU( i ).Instance( j ).fmuOutputVariableSchedule( x ).RealVarValue = realVarValueVec[x-1];
						}

						if ( FMU( i ).Instance( j ).fmistatus != fmiOK ) {
							ShowSevereError( "ExternalInterface/GetSetVariablesAndDoStepFMUImport: Error when trying to get outputs" );
							ShowContinueError( "in instance \"" + FMU( i ).Instance( j ).Name + "\" of FMU \"" + FMU( i ).Name + "\"" );
							ShowContinueError( "Error Code = \"" + TrimSigDigits( FMU( i ).Instance( j ).fmistatus ) + "\"" );
							ErrorsFound = true;
							StopExternalInterfaceIfError();
						}

					}

					// generate vectors here first
					if ( size(FMU(i).Instance(j).fmuOutputVariableVariable) > 0 ) {

						std::vector<unsigned int> valueReferenceVec2;
						std::vector<Real64> realVarValueVec2;
						for ( unsigned long x = 1; x <= size( FMU( i ).Instance( j ).fmuOutputVariableVariable ); ++x ) {
							valueReferenceVec2.push_back( FMU( i ).Instance( j ).fmuOutputVariableVariable( x ).ValueReference );
							realVarValueVec2.push_back( FMU( i ).Instance( j ).fmuOutputVariableVariable( x ).RealVarValue );
						}

						// pass in the vectors as pointers to the first member of the vector
						FMU( i ).Instance( j ).fmistatus = fmiEPlusGetReal ( &FMU( i ).Instance( j ).fmicomponent, &valueReferenceVec2[0], &realVarValueVec2[0], &FMU( i ).Instance( j ).NumOutputVariablesVariable, &FMU( i ).Instance( j ).Index );

						for ( unsigned long x = 1; x <= size( FMU( i ).Instance( j ).fmuOutputVariableVariable ); ++x ) {
							FMU( i ).Instance( j ).fmuOutputVariableVariable( x ).ValueReference = valueReferenceVec2[x-1];
							FMU( i ).Instance( j ).fmuOutputVariableVariable( x ).RealVarValue = realVarValueVec2[x-1];
						}

						if ( FMU( i ).Instance( j ).fmistatus != fmiOK ) {
							ShowSevereError( "ExternalInterface/GetSetVariablesAndDoStepFMUImport: Error when trying to get outputs" );
							ShowContinueError( "in instance \"" + FMU( i ).Instance( j ).Name + "\" of FMU \"" + FMU( i ).Name + "\"" );
							ShowContinueError( "Error Code = \"" + TrimSigDigits( FMU( i ).Instance( j ).fmistatus ) + "\"" );
							ErrorsFound = true;
							StopExternalInterfaceIfError();
						}

					}

					if ( size( FMU( i ).Instance( j ).fmuOutputVariableActuator ) > 0 ) {

						// generate vectors here first
						std::vector<unsigned int> valueReferenceVec3;
						std::vector<Real64> realVarValueVec3;
						for ( unsigned long x = 1; x <= size( FMU( i ).Instance( j ).fmuOutputVariableActuator ); ++x ) {
							valueReferenceVec3.push_back( FMU( i ).Instance( j ).fmuOutputVariableActuator( x ).ValueReference );
							realVarValueVec3.push_back( FMU( i ).Instance( j ).fmuOutputVariableActuator( x ).RealVarValue );
						}

						// pass in the vectors as pointers to the first member of the vector
						FMU( i ).Instance( j ).fmistatus = fmiEPlusGetReal ( &FMU( i ).Instance( j ).fmicomponent, &valueReferenceVec3[0], &realVarValueVec3[0], &FMU( i ).Instance( j ).NumOutputVariablesActuator, &FMU( i ).Instance( j ).Index );

						for ( unsigned long x = 1; x <= size( FMU( i ).Instance( j ).fmuOutputVariableActuator ); ++x ) {
							FMU( i ).Instance( j ).fmuOutputVariableActuator( x ).ValueReference = valueReferenceVec3[x-1];
							FMU( i ).Instance( j ).fmuOutputVariableActuator( x ).RealVarValue = realVarValueVec3[x-1];
						}

						if ( FMU( i ).Instance( j ).fmistatus != fmiOK ) {
							ShowSevereError( "ExternalInterface/GetSetVariablesAndDoStepFMUImport: Error when trying to get outputs" );
							ShowContinueError( "in instance \"" + FMU( i ).Instance( j ).Name + "\" of FMU \"" + FMU( i ).Name + "\"" );
							ShowContinueError( "Error Code = \"" + TrimSigDigits( FMU( i ).Instance( j ).fmistatus ) + "\"" );
							ErrorsFound = true;
							StopExternalInterfaceIfError();
						}
					}
				}

				// Set in EnergyPlus the values of the schedules
				for ( k = 1; k <= FMU( i ).Instance( j ).NumOutputVariablesSchedule; ++k ) {
					ExternalInterfaceSetSchedule( FMU( i ).Instance( j ).eplusInputVariableSchedule( k ).VarIndex, FMU( i ).Instance( j ).fmuOutputVariableSchedule( k ).RealVarValue );
				}

				// Set in EnergyPlus the values of the variables
				for ( k = 1; k <= FMU( i ).Instance( j ).NumOutputVariablesVariable; ++k ) {
					ExternalInterfaceSetErlVariable( FMU( i ).Instance( j ).eplusInputVariableVariable( k ).VarIndex, FMU( i ).Instance( j ).fmuOutputVariableVariable( k ).RealVarValue );
				}

				// Set in EnergyPlus the values of the actuators
				for ( k = 1; k <= FMU( i ).Instance( j ).NumOutputVariablesActuator; ++k ) {
					ExternalInterfaceSetErlVariable( FMU( i ).Instance( j ).eplusInputVariableActuator( k ).VarIndex, FMU( i ).Instance( j ).fmuOutputVariableActuator( k ).RealVarValue );
				}

				if ( FirstCallGetSetDoStep ) {
					// Get from EnergyPlus, values that will be set in fmus
					for ( k = 1; k <= FMU( i ).Instance( j ).NumInputVariablesInIDF; ++k ) {
						//This make sure that the variables are updated at the Zone Time Step
						FMU( i ).Instance( j ).eplusOutputVariable( k ).RTSValue = GetInternalVariableValue( FMU( i ).Instance( j ).eplusOutputVariable( k ).VarType, FMU( i ).Instance( j ).eplusOutputVariable( k ).VarIndex );
					}
				} else {
					// Get from EnergyPlus, values that will be set in fmus
					for ( k = 1; k <= FMU( i ).Instance( j ).NumInputVariablesInIDF; ++k ) {
						//This make sure that the variables are updated at the Zone Time Step
						FMU( i ).Instance( j ).eplusOutputVariable( k ).RTSValue = GetInternalVariableValueExternalInterface( FMU( i ).Instance( j ).eplusOutputVariable( k ).VarType, FMU( i ).Instance( j ).eplusOutputVariable( k ).VarIndex );
					}
				}

				if ( ! FlagReIni ) {

					// generate vectors here first
					std::vector<unsigned int> valueReferenceVec4;
					for ( unsigned long x = 1; x <= size( FMU( i ).Instance( j ).fmuInputVariable ); ++x ) {
						valueReferenceVec4.push_back( FMU( i ).Instance( j ).fmuInputVariable( x ).ValueReference );
					}

					std::vector<Real64> rtsValueVec4;
					for ( unsigned long x = 1; x <= size( FMU( i ).Instance( j ).eplusOutputVariable ); ++x ) {
						rtsValueVec4.push_back( FMU( i ).Instance( j ).eplusOutputVariable( x ).RTSValue );
					}

					FMU( i ).Instance( j ).fmistatus = fmiEPlusSetReal( &FMU( i ).Instance( j ).fmicomponent, &valueReferenceVec4[0], &rtsValueVec4[0], &FMU( i ).Instance( j ).NumInputVariablesInIDF, &FMU( i ).Instance( j ).Index );

					if ( FMU( i ).Instance( j ).fmistatus != fmiOK ) {
						ShowSevereError( "ExternalInterface/GetSetVariablesAndDoStepFMUImport: Error when trying to set inputs" );
						ShowContinueError( "in instance \"" + FMU( i ).Instance( j ).Name + "\" of FMU \"" + FMU( i ).Name + "\"" );
						ShowContinueError( "Error Code = \"" + TrimSigDigits( FMU( i ).Instance( j ).fmistatus ) + "\"" );
						ErrorsFound = true;
						StopExternalInterfaceIfError();
					}
				}
				int localfmitrue( fmiTrue );
				// Call and simulate the FMUs to get values at the corresponding timestep.
				FMU( i ).Instance( j ).fmistatus = fmiEPlusDoStep( &FMU( i ).Instance( j ).fmicomponent, &tComm, &hStep, &localfmitrue, &FMU( i ).Instance( j ).Index );
				if ( FMU( i ).Instance( j ).fmistatus != fmiOK ) {
					ShowSevereError( "ExternalInterface/GetSetVariablesAndDoStepFMUImport: Error when trying to" );
					ShowContinueError( "do the coSimulation with instance \"" + FMU( i ).Instance( j ).Name + "\"" );
					ShowContinueError( "of FMU \"" + FMU( i ).Name + "\"" );
					ShowContinueError( "Error Code = \"" + TrimSigDigits( FMU( i ).Instance( j ).fmistatus ) + "\"" );
					ErrorsFound = true;
					StopExternalInterfaceIfError();
				}
			}
		}

		// If we have Erl variables, we need to call ManageEMS so that they get updated in the Erl data structure
		if ( useEMS ) {
			ManageEMS( emsCallFromExternalInterface );
		}

		FirstCallGetSetDoStep = false;
	}

	void
	InstantiateInitializeFMUImport()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Thierry S. Nouidui, Michael Wetter, Wangda Zuo
		//       DATE WRITTEN   08Aug2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine instantiates and initializes FMUs.

		// Using/Aliasing
		using General::TrimSigDigits;

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int i, j; // Loop counters

		// Instantiate FMUs
		for ( i = 1; i <= NumFMUObjects; ++i ) {
			for ( j = 1; j <= FMU( i ).NumInstances; ++j ) {
				FMU(i).Instance(j).fmicomponent = fmiEPlusInstantiateSlave( 
					(char*)FMU(i).Instance(j).WorkingFolder.c_str(), &FMU(i).Instance(j).LenWorkingFolder, 
					&FMU(i).TimeOut, &FMU(i).Visible, &FMU(i).Interactive, &FMU(i).LoggingOn, &FMU(i).Instance(j).Index);
				// TODO: This is doing a null pointer check; OK?
				if ( ! FMU( i ).Instance( j ).fmicomponent ) {
					ShowSevereError( "ExternalInterface/CalcExternalInterfaceFMUImport: Error when trying to instantiate" );
					ShowContinueError( "instance \"" + FMU( i ).Instance( j ).Name + "\" of FMU \"" + FMU( i ).Name + "\"" );
					ErrorsFound = true;
					StopExternalInterfaceIfError();
				}
			}
		}

		// Initialize FMUs
		int localfmiTrue( fmiTrue );
		for ( i = 1; i <= NumFMUObjects; ++i ) {
			for ( j = 1; j <= FMU( i ).NumInstances; ++j ) {
				FMU( i ).Instance( j ).fmistatus = fmiEPlusInitializeSlave( &FMU( i ).Instance( j ).fmicomponent, 
					&tStart, &localfmiTrue, &tStop, &FMU( i ).Instance( j ).Index );
				if ( FMU( i ).Instance( j ).fmistatus != fmiOK ) {
					ShowSevereError( "ExternalInterface/CalcExternalInterfaceFMUImport: Error when trying to initialize" );
					ShowContinueError( "instance \"" + FMU( i ).Instance( j ).Name + "\" of FMU \"" + FMU( i ).Name + "\"" );
					ShowContinueError( "Error Code = \"" + TrimSigDigits( FMU( i ).Instance( j ).fmistatus ) + "\"" );
					ErrorsFound = true;
					StopExternalInterfaceIfError();
				}
			}
		}
	}

	void
	InitializeFMU()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Thierry S. Nouidui, Michael Wetter, Wangda Zuo
		//       DATE WRITTEN   08Aug2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine reinitializes FMUs.

		// Using/Aliasing
		using General::TrimSigDigits;

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int i, j; // Loop counters
		int localfmiTrue( fmiTrue );

		// Initialize FMUs
		for ( i = 1; i <= NumFMUObjects; ++i ) {
			for ( j = 1; j <= FMU( i ).NumInstances; ++j ) {
				FMU( i ).Instance( j ).fmistatus = fmiEPlusInitializeSlave( &FMU( i ).Instance( j ).fmicomponent, 
					&tStart, &localfmiTrue, &tStop, &FMU( i ).Instance( j ).Index );
				if ( FMU( i ).Instance( j ).fmistatus != fmiOK ) {
					ShowSevereError( "ExternalInterface/CalcExternalInterfaceFMUImport: Error when trying to initialize" );
					ShowContinueError( "instance \"" + FMU( i ).Instance( j ).Name + "\" of FMU \"" + FMU( i ).Name + "\"" );
					ShowContinueError( "Error Code = \"" + TrimSigDigits( FMU( i ).Instance( j ).fmistatus ) + "\"" );
					ErrorsFound = true;
					StopExternalInterfaceIfError();
				}
			}
		}
	}

	void
	TerminateResetFreeFMUImport(int fmiEndSimulation)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Thierry S. Nouidui, Michael Wetter, Wangda Zuo
		//       DATE WRITTEN   08Aug2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine terminates the FMUs instances

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int i, j; // Loop counter

		//----Needs to have function that allows to terminates FMU. Was not defined in version 1.0 -- fixme
		for ( i = 1; i <= NumFMUObjects; ++i ) {
			for ( j = 1; j <= FMU( i ).NumInstances; ++j ) {
				if ( FMU( i ).Instance( j ).fmistatus != fmiFatal ) {
					// Cleanup slaves
					FMU( i ).Instance( j ).fmistatus = fmiEPlusFreeSlave( &FMU( i ).Instance( j ).fmicomponent, &FMU( i ).Instance( j ).Index, &fmiEndSimulation );
				}
				// check if fmiComponent has been freed
				if ( ! FMU( i ).Instance( j ).fmicomponent ) {
					ShowSevereError( "ExternalInterface/TerminateResetFreeFMUImport: Error when trying to terminate" );
					ShowContinueError( "instance \"" + FMU( i ).Instance( j ).Name + "\" of FMU \"" + FMU( i ).Name + "\"" );
					ErrorsFound = true;
					StopExternalInterfaceIfError();
				}
			}
		}
	}

	void
	InitExternalInterfaceFMUImport()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Thierry S. Nouidui, Michael Wetter, Wangda Zuo
		//       DATE WRITTEN   08Aug2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine initializes the input and outputs variables used for the co-simulation with FMUs.

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using InputProcessor::FindItem;
		using ScheduleManager::GetDayScheduleIndex;
		using RuntimeLanguageProcessor::isExternalInterfaceErlVariable;
		using RuntimeLanguageProcessor::FindEMSVariable;
		using DataIPShortCuts::cCurrentModuleObject;
		using DataIPShortCuts::cAlphaArgs;
		using DataIPShortCuts::rNumericArgs;
		using DataIPShortCuts::cAlphaFieldNames;
		using DataIPShortCuts::cNumericFieldNames;
		using DataSystemVariables::CheckForActualFileName;
		using General::TrimSigDigits;
		using DataStringGlobals::pathChar;
		using DataStringGlobals::altpathChar;
		using DataStringGlobals::CurrentWorkingFolder;

		// Locals
		int i, j, k, l, Loop; // Loop counters
		int retVal; // Return value of function call, used for error handling
		int NumAlphas( 0 ); // Number of Alphas for each GetObjectItem call
		int NumNumbers( 0 ); // Number of Numbers for each GetObjectItem call
		int IOStatus( 0 ); // Used in GetObjectItem
		int NumFMUInputVariables( 0 ); // Number of FMU input variables
		std::string Name_NEW; // Units sting, may be blank
		std::string Name_OLD; // Units sting, may be blank

		Array1D_int keyIndexes( 1 ); // Array index for
		Array1D_int varTypes( 1 ); // Array index for
		Array1D_string NamesOfKeys( 1 ); // Specific key name
		int retValfmiVersion;
		int retValfmiPathLib;
		Array1D_string NameListInstances( 5 );
		bool IsNotOK;
		bool IsBlank;
		static bool FirstCallIni( true ); // First time, input has been read
		bool fileExist;
		std::string tempFullFileName;
		Array1D_string strippedFileName; // remove path from entered file name
		Array1D_string fullFileName; // entered file name/found
		std::string::size_type pos;
		int FOUND;

		if ( FirstCallIni ) {
			DisplayString( "Initializing FunctionalMockupUnitImport interface" );
			// do one time initializations
			ValidateRunControl();
			FMU.allocate( NumFMUObjects );

			// there used to be code in here to apply the root working folder to create an absolute path
			// however, this wasn't working, as the root working folder was coming back empty
			// in any case, the relative paths work fine here

			// post process as needed in case these are used later
			FMURootWorkingFolder = "tmp-fmus";
			FMURootWorkingFolder += pathChar; //getStringFromCharArray( FMUWorkingFolderCharArr );

			// Get and store the names of all FMUs in EnergyPlus data structure
			strippedFileName.allocate( NumFMUObjects );
			fullFileName.allocate( NumFMUObjects );
			cCurrentModuleObject = "ExternalInterface:FunctionalMockupUnitImport";
			for ( Loop = 1; Loop <= NumFMUObjects; ++Loop ) {
				GetObjectItem( cCurrentModuleObject, Loop, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, _, _, cAlphaFieldNames, cNumericFieldNames );
				// Get the FMU name
				FMU( Loop ).Name = cAlphaArgs( 1 );
				CheckForActualFileName( cAlphaArgs( 1 ), fileExist, tempFullFileName );
				if ( fileExist ) {
					pos = index( FMU( Loop ).Name, pathChar, true ); // look backwards
					if ( pos != std::string::npos ) {
						strippedFileName( Loop ) = FMU( Loop ).Name.substr( pos + 1 );
					} else { // pos == 0, look for alt path char
						pos = index( FMU( Loop ).Name, altpathChar, true ); // look backwards
						if ( pos != std::string::npos ) {
							strippedFileName( Loop ) = FMU( Loop ).Name.substr( pos + 1 );
						} else {
							strippedFileName( Loop ) = FMU( Loop ).Name;
						}
					}
					fullFileName( Loop ) = tempFullFileName;
				} else {
					ShowSevereError( "ExternalInterface/InitExternalInterfaceFMUImport:" );
					ShowContinueError( "file not located = \"" + cAlphaArgs( 1 ) + "\"." );
					ErrorsFound = true;
				}
				// Get fmu time out
				FMU( Loop ).TimeOut = rNumericArgs( 1 );
				// Get fmu logging on
				FMU( Loop ).LoggingOn = rNumericArgs( 2 );
			}

			// check for dups that aren't the same file
			// this is windows code...
			for ( j = 1; j <= NumFMUObjects; ++j ) {
				for ( k = 2; k <= NumFMUObjects; ++k ) {
					if ( ! SameString( strippedFileName( j ), strippedFileName( k ) ) ) continue;
					// base file names are the same
					if ( SameString( fullFileName( j ), fullFileName( k ) ) ) continue;
					ShowSevereError( "ExternalInterface/InitExternalInterfaceFMUImport:" );
					ShowContinueError( "duplicate file names (but not same file) entered." );
					ShowContinueError( "...entered file name=\"" + FMU( j ).Name + "\"" );
					ShowContinueError( "...   full file name=\"" + fullFileName( j ) + "\"" );
					ShowContinueError( "...entered file name=\"" + FMU( k ).Name + "\"" );
					ShowContinueError( "...   full file name=\"" + fullFileName( k ) + "\"" );
					ShowContinueError( "...name collision but not same file name." );
					ErrorsFound = true;
				}
			}
			if ( ErrorsFound ) {
				strippedFileName.deallocate();
				fullFileName.deallocate();
				StopExternalInterfaceIfError();
			}

			// get the names of the input variables each fmu(and the names of the
			// corresponding output variables in EnergyPlus --).
			cCurrentModuleObject = "ExternalInterface:FunctionalMockupUnitImport:From:Variable";
			NumFMUInputVariables = GetNumObjectsFound( cCurrentModuleObject );
			// Determine the number of instances for each FMUs
			for ( i = 1; i <= NumFMUObjects; ++i ) {
				Name_NEW = "";
				Name_OLD = "";
				j = 1;
				k = 1;
				FMU( i ).Instance.allocate( NumFMUInputVariables );
				checkInstanceName.allocate( NumFMUInputVariables );
				for ( l = 1; l <= NumFMUInputVariables; ++l ) {
					GetObjectItem( cCurrentModuleObject, l, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, _, _, cAlphaFieldNames, cNumericFieldNames );
					if ( SameString( cAlphaArgs( 3 ), FMU( i ).Name ) ) {
						Name_NEW = cAlphaArgs( 4 );
						if ( ! SameString( Name_OLD, Name_NEW ) ) {
							FOUND = FindItem( Name_NEW, checkInstanceName );
							if ( FOUND == 0 ) {
								checkInstanceName( l ).Name = Name_NEW;
								FMU( i ).NumInstances = j;
								FMU( i ).Instance( j ).Name = Name_NEW;
								++j;
								Name_OLD = Name_NEW;
							}
						}
						FMU( i ).TotNumInputVariablesInIDF = k;
						++k;
					}
				}
				checkInstanceName.deallocate();
			}

			for ( i = 1; i <= NumFMUObjects; ++i ) {
				if ( FMU( i ).NumInstances == 0 ) {
					ShowSevereError( "ExternalInterface/InitExternalInterfaceFMUImport: The FMU \"" + FMU( i ).Name + "\" does" );
					ShowContinueError( "not have any instances or any input variable. An FMU should have at least one instance" );
					ShowContinueError( "or one input variable defined in input file. Check FMU object in the input file." );
					ErrorsFound = true;
					StopExternalInterfaceIfError();
				}
				if ( NumFMUInputVariables > 0 && FMU( i ).TotNumInputVariablesInIDF == 0 ) {
					ShowWarningError( "InitExternalInterfaceFMUImport: The FMU \"" + FMU( i ).Name + "\"" );
					ShowContinueError( "is defined but has no input variables." );
					ShowContinueError( "Check the input field of the corresponding object" );
					ShowContinueError( "ExternalInterface:FunctionalMockupUnitImport:From:Variable." );
				}
			}

			// write output folder where FMUs will be unpacked later on.
			for ( i = 1; i <= NumFMUObjects; ++i ) {
				for ( j = 1; j <= FMU( i ).NumInstances; ++j ) {
					FMU( i ).Instance( j ).WorkingFolder = FMURootWorkingFolder + strippedFileName( i ) + '_' + FMU( i ).Instance( j ).Name;
				}
			}

			// parse the fmu defined in the idf using the fmuUnpack.
			for ( i = 1; i <= NumFMUObjects; ++i ) {
				for ( j = 1; j <= FMU( i ).NumInstances; ++j ) {
					// get the length of working folder trimmed
					FMU( i ).Instance( j ).LenWorkingFolder = FMU( i ).Instance( j ).WorkingFolder.length();
					// unpack fmus
					// preprocess arguments for library call
					{
						auto fullFileNameArr( getCharArrayFromString( fullFileName( i ) ) );
						auto workingFolderArr( getCharArrayFromString( FMU( i ).Instance( j ).WorkingFolder ) );
						int lenFileName( len( fullFileName( i ) ) );

						// make the library call
						retVal = fmiEPlusUnpack( &fullFileNameArr[0], &workingFolderArr[0], &lenFileName, &FMU( i ).Instance( j ).LenWorkingFolder );

						if ( retVal != 0 ) {
							ShowSevereError( "ExternalInterface/InitExternalInterfaceFMUImport: Error when trying to" );
							ShowContinueError( "unpack the FMU \"" + FMU( i ).Name + "\"." );
							ShowContinueError( "Check if the FMU exists. Also check if the FMU folder is not write protected." );
							ErrorsFound = true;
							StopExternalInterfaceIfError();
						}

					}

					{
						// determine modelID and modelGUID of all FMU instances
						// preprocess arguments for library call
						auto workingFolderArr( getCharArrayFromString( FMU( i ).Instance( j ).WorkingFolder ) );

						// make the library call
						FMU(i).Instance(j).Index = model_ID_GUID((char*)FMU(i).Instance(j).Name.c_str(), &workingFolderArr[0], &FMU(i).Instance(j).LenWorkingFolder, &FMU(i).Instance(j).NumInputVariablesInFMU, &FMU(i).Instance(j).NumOutputVariablesInFMU);

						if ( FMU( i ).Instance( j ).Index < 0 ) {
							ShowSevereError( "ExternalInterface/InitExternalInterfaceFMUImport: Error when trying to" );
							ShowContinueError( "get the model ID and model GUID" );
							ShowContinueError( "of instance \"" + FMU( i ).Instance( j ).Name + "\" of FMU \"" + FMU( i ).Name + "\"." );
							ShowContinueError( "Check if modelDescription.xml exists in the folder where the FMU has been unpacked." );
							ErrorsFound = true;
							StopExternalInterfaceIfError();
						}
					}

					{
						// get the path to the binaries
						// preprocess args for library call
						auto workingFolderArr( getCharArrayFromString( FMU( i ).Instance( j ).WorkingFolder ) );
						FMU(i).Instance(j).WorkingFolder_wLib = FMU(i).Instance(j).WorkingFolder + "                                                                                           ";
						auto workingFolderWithLibArr( getCharArrayFromString( FMU( i ).Instance( j ).WorkingFolder_wLib ) );

						// make the library call
						retValfmiPathLib = addLibPathCurrentWorkingFolder( &workingFolderWithLibArr[0], &workingFolderArr[0], &FMU( i ).Instance( j ).LenWorkingFolder, &FMU( i ).Instance( j ).Index );

						// post process args in case they are used later
						FMU( i ).Instance( j ).WorkingFolder_wLib = trim(getStringFromCharArray( workingFolderWithLibArr ));

						if ( retValfmiPathLib != 0 ) {
							ShowSevereError( "ExternalInterface/InitExternalInterfaceFMUImport: Error when trying to" );
							ShowContinueError( "get the path to the binaries of instance" );
							ShowContinueError( "\"" + FMU( i ).Instance( j ).Name + "\" of FMU \"" + FMU( i ).Name + "\"." );
							ShowContinueError( "Check if binaries folder exists where the FMU has been unpacked." );
							ErrorsFound = true;
							StopExternalInterfaceIfError();
						}

						// get the length of the working folder with libraries
						FMU( i ).Instance( j ).LenWorkingFolder_wLib = FMU( i ).Instance( j ).WorkingFolder_wLib.length();
					}

					{
						// determine the FMI version
						// preprocess args for library call
						auto workingFolderWithLibArr( getCharArrayFromString( FMU( i ).Instance( j ).WorkingFolder_wLib ) );
						auto VersionNumArr( getCharArrayFromString( "    " ) ); // the version should only be 3 characters long, since for now we only handle "1.0"

						// make the library call
						retValfmiVersion = getfmiEPlusVersion(&workingFolderWithLibArr[0], &FMU(i).Instance(j).LenWorkingFolder_wLib, &VersionNumArr[0], &FMU(i).Instance(j).Index);

						// post process in case args are used later
						FMU( i ).Instance( j ).fmiVersionNumber = getStringFromCharArray( VersionNumArr );

						if ( retValfmiVersion != 0 ) {
							ShowSevereError( "ExternalInterface/InitExternalInterfaceFMUImport: Error when trying to" );
							ShowContinueError( "load FMI functions library of instance" );
							ShowContinueError( "\"" + FMU( i ).Instance( j ).Name + "\" of FMU \"" + FMU( i ).Name + "\"." );
							ShowContinueError( "\"" + FMU( i ).Instance( j ).fmiVersionNumber + "\"." );
							ErrorsFound = true;
							StopExternalInterfaceIfError();
						}

						if ( FMU( i ).Instance( j ).fmiVersionNumber.substr( 0, 3 ) != "1.0" ) {
							ShowSevereError( "ExternalInterface/InitExternalInterfaceFMUImport: Error when getting version" );
							ShowContinueError( "number of instance \"" + FMU( i ).Instance( j ).Name + "\"" );
							ShowContinueError( "of FMU \"" + FMU( i ).Name + "\"." );
							ShowContinueError( "The version number found (\"" + FMU( i ).Instance( j ).fmiVersionNumber.substr( 0, 3 ) + "\")" );
							ShowContinueError( "differs from version 1.0 which is currently supported." );
							ErrorsFound = true;
							StopExternalInterfaceIfError();
						}
					}

				}
			}

			strippedFileName.deallocate();
			fullFileName.deallocate();

			for ( i = 1; i <= NumFMUObjects; ++i ) {
				for ( j = 1; j <= FMU( i ).NumInstances; ++j ) {
					FMU( i ).Instance( j ).fmuInputVariable.allocate( NumFMUInputVariables );
					FMU( i ).Instance( j ).checkfmuInputVariable.allocate( NumFMUInputVariables );
					FMU( i ).Instance( j ).eplusOutputVariable.allocate( NumFMUInputVariables );
					k = 1;
					for ( l = 1; l <= NumFMUInputVariables; ++l ) {
						GetObjectItem( cCurrentModuleObject, l, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, _, _, cAlphaFieldNames, cNumericFieldNames );
						if ( SameString( cAlphaArgs( 3 ), FMU( i ).Name ) && SameString( cAlphaArgs( 4 ), FMU( i ).Instance( j ).Name ) ) {
							FMU( i ).Instance( j ).fmuInputVariable( k ).Name = cAlphaArgs( 5 );
							FMU( i ).Instance( j ).eplusOutputVariable( k ).VarKey = cAlphaArgs( 1 );
							FMU( i ).Instance( j ).eplusOutputVariable( k ).Name = cAlphaArgs( 2 );
							// verify whether we have duplicate FMU input variables in the idf
							VerifyName( FMU( i ).Instance( j ).fmuInputVariable( k ).Name, FMU( i ).Instance( j ).checkfmuInputVariable, NumFMUInputVariables, IsNotOK, IsBlank, "The FMU input variable \"" + FMU( i ).Instance( j ).fmuInputVariable( k ).Name + "\" of instance \"" + FMU( i ).Instance( j ).Name + "\" of FMU \"" + FMU( i ).Name + "\" has duplicates. Please check the input file again and delete duplicated entries." );
							if ( IsNotOK ) {
								ErrorsFound = true;
								StopExternalInterfaceIfError();
							} else {
								FMU( i ).Instance( j ).checkfmuInputVariable( k ).Name = FMU( i ).Instance( j ).fmuInputVariable( k ).Name;
							}

							// preprocess args for library call
							auto inputVarNameArr( getCharArrayFromString( FMU( i ).Instance( j ).fmuInputVariable( k ).Name ) );
							int inputVarNameLen( len( FMU( i ).Instance( j ).fmuInputVariable( k ).Name ) );

							// make the library call
							FMU(i).Instance(j).fmuInputVariable(k).ValueReference = getValueReferenceByNameFMUInputVariables(&inputVarNameArr[0], &inputVarNameLen, &FMU(i).Instance(j).Index);

							// postprocess args in case they are used later
							FMU( i ).Instance( j ).fmuInputVariable( k ).Name = getStringFromCharArray( inputVarNameArr );

							if ( FMU( i ).Instance( j ).fmuInputVariable( k ).ValueReference == -999 ) {
								ShowSevereError( "ExternalInterface/InitExternalInterfaceFMUImport: Error when trying to" );
								ShowContinueError( "get the value reference of FMU input variable" );
								ShowContinueError( "\"" + FMU( i ).Instance( j ).fmuInputVariable( k ).Name + "\" of instance \"" + FMU( i ).Instance( j ).Name + "\" of FMU" );
								ShowContinueError( "of FMU \"" + FMU( i ).Name + "\". Please check the name of input variable" );
								ShowContinueError( "in the input file and in the modelDescription file." );
								ErrorsFound = true;
								StopExternalInterfaceIfError();
							}

							if ( FMU( i ).Instance( j ).fmuInputVariable( k ).ValueReference == -1 ) {
								ShowSevereError( "ExternalInterface/InitExternalInterfaceFMUImport: Error when trying to" );
								ShowContinueError( "get the value reference of FMU input variable" );
								ShowContinueError( "\"" + FMU( i ).Instance( j ).fmuInputVariable( k ).Name + "\" of instance \"" + FMU( i ).Instance( j ).Name + "\" of FMU" );
								ShowContinueError( "\"" + FMU( i ).Name + "\". This variable is not an FMU input variable." );
								ShowContinueError( "Please check the causality of the variable in the modelDescription file." );
								ErrorsFound = true;
								StopExternalInterfaceIfError();
							}

							// The next call expects an array, but a single item is passed
							// Therefore create a single item array here first
							Array1D_string tempSingleStringA( 1, FMU( i ).Instance( j ).eplusOutputVariable( k ).VarKey );
							Array1D_string tempSingleStringB( 1, FMU( i ).Instance( j ).eplusOutputVariable( k ).Name );

							// Make the call with arrays
							GetReportVariableKey( tempSingleStringA, 1, tempSingleStringB, keyIndexes, varTypes );

							// Then postprocess the array items back in case they changed
							FMU( i ).Instance( j ).eplusOutputVariable( k ).VarKey = tempSingleStringA( 1 );
							FMU( i ).Instance( j ).eplusOutputVariable( k ).Name = tempSingleStringB( 1 );

							FMU( i ).Instance( j ).eplusOutputVariable( k ).VarIndex = keyIndexes( 1 );
							FMU( i ).Instance( j ).eplusOutputVariable( k ).VarType = varTypes( 1 );
							FMU( i ).Instance( j ).NumInputVariablesInIDF = k;
							++k;
						}
					}

					if ( NumFMUInputVariables > 0 && FMU( i ).Instance( j ).NumInputVariablesInIDF == 0 ) {
						ShowWarningError( "InitExternalInterfaceFMUImport: The instance \"" + FMU( i ).Instance( j ).Name + "\" of FMU \"" + FMU( i ).Name + "\"" );
						ShowContinueError( "is defined but has no input variables. Check the input field of the" );
						ShowContinueError( "corresponding object: ExternalInterface:FunctionalMockupUnitImport:From:Variable." );
					}
				}
			}

			for ( i = 1; i <= NumFMUObjects; ++i ) {
				for ( j = 1; j <= FMU( i ).NumInstances; ++j ) {
					// check whether the number of input variables in fmu is bigger than in the idf
					if ( FMU( i ).Instance( j ).NumInputVariablesInFMU > FMU( i ).Instance( j ).NumInputVariablesInIDF ) {
						ShowWarningError( "InitExternalInterfaceFMUImport: The number of input variables defined in input file (" + TrimSigDigits( FMU( i ).Instance( j ).NumInputVariablesInIDF ) + ')' );
						ShowContinueError( "of instance \"" + FMU( i ).Instance( j ).Name + "\" of FMU \"" + FMU( i ).Name + "\" is less than the number of input variables" );
						ShowContinueError( "in the modelDescription file (" + TrimSigDigits( FMU( i ).Instance( j ).NumInputVariablesInFMU ) + ")." );
						ShowContinueError( "Check the input file and the modelDescription file again." );
					}
					// check whether the number of input variables in fmu is less than in the idf
					if ( FMU( i ).Instance( j ).NumInputVariablesInFMU < FMU( i ).Instance( j ).NumInputVariablesInIDF ) {
						ShowWarningError( "InitExternalInterfaceFMUImport: The number of input variables defined in input file (" + TrimSigDigits( FMU( i ).Instance( j ).NumInputVariablesInIDF ) + ')' );
						ShowContinueError( "of instance \"" + FMU( i ).Instance( j ).Name + "\" of FMU \"" + FMU( i ).Name + "\" is bigger than the number of input variables" );
						ShowContinueError( "in the modelDescription file (" + TrimSigDigits( FMU( i ).Instance( j ).NumInputVariablesInFMU ) + ")." );
						ShowContinueError( "Check the input file and the modelDescription file again." );
					}
				}
			}

			// get the names of the output variables each fmu (and the names of the
			// corresponding input variables in EnergyPlus -- schedule).
			cCurrentModuleObject = "ExternalInterface:FunctionalMockupUnitImport:To:Schedule";
			NumFMUInputVariables = GetNumObjectsFound( cCurrentModuleObject );

			for ( i = 1; i <= NumFMUObjects; ++i ) {
				j = 1;
				for ( k = 1; k <= NumFMUInputVariables; ++k ) {
					GetObjectItem( cCurrentModuleObject, k, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, _, _, cAlphaFieldNames, cNumericFieldNames );
					if ( SameString( cAlphaArgs( 3 ), FMU( i ).Name ) ) {
						FMU( i ).TotNumOutputVariablesSchedule = j;
						++j;
					}
				}
			}

			for ( i = 1; i <= NumFMUObjects; ++i ) {
				for ( j = 1; j <= FMU( i ).NumInstances; ++j ) {
					FMU( i ).Instance( j ).fmuOutputVariableSchedule.allocate( NumFMUInputVariables );
					FMU( i ).Instance( j ).eplusInputVariableSchedule.allocate( NumFMUInputVariables );
					k = 1;
					for ( l = 1; l <= NumFMUInputVariables; ++l ) {
						GetObjectItem( cCurrentModuleObject, l, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, _, _, cAlphaFieldNames, cNumericFieldNames );
						if ( SameString( cAlphaArgs( 3 ), FMU( i ).Name ) && SameString( cAlphaArgs( 4 ), FMU( i ).Instance( j ).Name ) ) {
							FMU( i ).Instance( j ).fmuOutputVariableSchedule( k ).Name = cAlphaArgs( 5 );
							FMU( i ).Instance( j ).eplusInputVariableSchedule( k ).Name = cAlphaArgs( 1 );
							FMU( i ).Instance( j ).eplusInputVariableSchedule( k ).InitialValue = rNumericArgs( 1 );

							// get the value reference by using the FMU name and the variable name.

							// preprocess the arguments before the following library call
							auto NameCharArr( getCharArrayFromString( FMU( i ).Instance( j ).fmuOutputVariableSchedule( k ).Name ) );
							int lengthVar( len( FMU( i ).Instance( j ).fmuOutputVariableSchedule( k ).Name ) );

							// make the library call
							FMU(i).Instance(j).fmuOutputVariableSchedule(k).ValueReference = getValueReferenceByNameFMUOutputVariables(&NameCharArr[0], &lengthVar, &FMU(i).Instance(j).Index);

							// postprocess the arguments after the library call in case they are changed and used later
							FMU( i ).Instance( j ).fmuOutputVariableSchedule( k ).Name = getStringFromCharArray( NameCharArr );

							if ( FMU( i ).Instance( j ).fmuOutputVariableSchedule( k ).ValueReference == -999 ) {
								ShowSevereError( "ExternalInterface/InitExternalInterfaceFMUImport: Error when trying to get the value reference of the FMU output variable" );
								ShowContinueError( "\"" + FMU( i ).Instance( j ).fmuOutputVariableSchedule( k ).Name + "\" of instance \"" + FMU( i ).Instance( j ).Name + "\"" );
								ShowContinueError( "of FMU \"" + FMU( i ).Name + "\" that will be mapped to a schedule." );
								ShowContinueError( "Please check the name of output variables in the input file and" );
								ShowContinueError( "in the modelDescription file." );
								ErrorsFound = true;
								StopExternalInterfaceIfError();
							}

							if ( FMU( i ).Instance( j ).fmuOutputVariableSchedule( k ).ValueReference == -1 ) {
								ShowSevereError( "ExternalInterface/InitExternalInterfaceFMUImport: Error when trying to get the value reference of the FMU output variable" );
								ShowContinueError( "\"" + FMU( i ).Instance( j ).fmuOutputVariableSchedule( k ).Name + "\" of instance \"" + FMU( i ).Instance( j ).Name + "\"" );
								ShowContinueError( "of FMU \"" + FMU( i ).Name + "\" that will be mapped to a schedule." );
								ShowContinueError( "This variable is not an FMU output variable." );
								ShowContinueError( "Please check the causality of the variable in the modelDescription file." );
								ErrorsFound = true;
								StopExternalInterfaceIfError();
							}

							FMU( i ).Instance( j ).eplusInputVariableSchedule( k ).VarIndex = GetDayScheduleIndex( FMU( i ).Instance( j ).eplusInputVariableSchedule( k ).Name );
							FMU( i ).Instance( j ).NumOutputVariablesSchedule = k;
							if ( FMU( i ).Instance( j ).eplusInputVariableSchedule( k ).VarIndex <= 0 ) {
								ShowSevereError( "ExternalInterface/InitExternalInterfaceFMUImport:declares variable \"" + FMU( i ).Instance( j ).eplusInputVariableSchedule( k ).Name + "\"," );
								ShowContinueError( "but variable is not a schedule variable." );
								ErrorsFound = true;
								StopExternalInterfaceIfError();
							}
							++k;
						}
					}
				}
			}

			// get the names of the output variables each fmu (and the names of the
			// corresponding input variables in EnergyPlus -- variable).
			cCurrentModuleObject = "ExternalInterface:FunctionalMockupUnitImport:To:Variable";
			NumFMUInputVariables = GetNumObjectsFound( cCurrentModuleObject );

			for ( i = 1; i <= NumFMUObjects; ++i ) {
				j = 1;
				for ( k = 1; k <= NumFMUInputVariables; ++k ) {
					GetObjectItem( cCurrentModuleObject, k, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, _, _, cAlphaFieldNames, cNumericFieldNames );
					if ( SameString( cAlphaArgs( 2 ), FMU( i ).Name ) ) {
						FMU( i ).TotNumOutputVariablesVariable = j;
						++j;
					}
				}
			}

			for ( i = 1; i <= NumFMUObjects; ++i ) {
				for ( j = 1; j <= FMU( i ).NumInstances; ++j ) {
					FMU( i ).Instance( j ).fmuOutputVariableVariable.allocate( NumFMUInputVariables );
					FMU( i ).Instance( j ).eplusInputVariableVariable.allocate( NumFMUInputVariables );
					k = 1;
					for ( l = 1; l <= NumFMUInputVariables; ++l ) {
						GetObjectItem( cCurrentModuleObject, l, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, _, _, cAlphaFieldNames, cNumericFieldNames );
						if ( SameString( cAlphaArgs( 2 ), FMU( i ).Name ) && SameString( cAlphaArgs( 3 ), FMU( i ).Instance( j ).Name ) ) {
							FMU( i ).Instance( j ).fmuOutputVariableVariable( k ).Name = cAlphaArgs( 4 );
							FMU( i ).Instance( j ).eplusInputVariableVariable( k ).Name = cAlphaArgs( 1 );

							// get the value reference by using the FMU name and the variable name.
							auto NameCharArr( getCharArrayFromString( FMU( i ).Instance( j ).fmuOutputVariableVariable( k ).Name ) );
							int tempLength( len( FMU( i ).Instance( j ).fmuOutputVariableVariable( k ).Name ) );
							FMU(i).Instance(j).fmuOutputVariableVariable(k).ValueReference = getValueReferenceByNameFMUOutputVariables(&NameCharArr[0], &tempLength, &FMU(i).Instance(j).Index);
							//FMU( i ).Instance( j ).fmuOutputVariableVariable( k ).Name = getStringFromCharArray( NameCharArr );

							if ( FMU( i ).Instance( j ).fmuOutputVariableVariable( k ).ValueReference == -999 ) {
								ShowSevereError( "ExternalInterface/InitExternalInterfaceFMUImport: Error when trying to get the value reference of the FMU output variable" );
								ShowContinueError( "\"" + FMU( i ).Instance( j ).fmuOutputVariableVariable( k ).Name + "\" of instance \"" + FMU( i ).Instance( j ).Name + "\"" );
								ShowContinueError( "of FMU \"" + FMU( i ).Name + "\" that will be mapped to a variable." );
								ShowContinueError( "Please check the name of output variables in the input file and in the modelDescription file." );
								ErrorsFound = true;
								StopExternalInterfaceIfError();
							}

							if ( FMU( i ).Instance( j ).fmuOutputVariableVariable( k ).ValueReference == -1 ) {
								ShowSevereError( "ExternalInterface/InitExternalInterfaceFMUImport: Error when trying to get the value reference of the FMU output variable" );
								ShowContinueError( "\"" + FMU( i ).Instance( j ).fmuOutputVariableVariable( k ).Name + "\" of instance \"" + FMU( i ).Instance( j ).Name + "\"" );
								ShowContinueError( "of FMU \"" + FMU( i ).Name + "\" that will be mapped to a variable." );
								ShowContinueError( "This variable is not an FMU output variable. Please check the causality of the variable in the modelDescription file." );
								ErrorsFound = true;
								StopExternalInterfaceIfError();
							}

							FMU( i ).Instance( j ).eplusInputVariableVariable( k ).VarIndex = FindEMSVariable( FMU( i ).Instance( j ).eplusInputVariableVariable( k ).Name, 0 );
							FMU( i ).Instance( j ).NumOutputVariablesVariable = k;
							if ( FMU( i ).Instance( j ).eplusInputVariableVariable( k ).VarIndex <= 0 ) {
								ShowSevereError( "ExternalInterface/InitExternalInterfaceFMUImport:declares variable \"" + FMU( i ).Instance( j ).eplusInputVariableVariable( k ).Name + "\"," );
								ShowContinueError( "but variable is not an EMS variable." );
								ErrorsFound = true;
								StopExternalInterfaceIfError();
							}
							++k;
						}
					}
					if ( FMU( i ).Instance( j ).NumOutputVariablesVariable >= 1 ) {
						useEMS = true;
					}
				}
			}

			// get the names of the output variables each fmu (and the names of the
			// corresponding input variables in EnergyPlus -- actuator).
			cCurrentModuleObject = "ExternalInterface:FunctionalMockupUnitImport:To:Actuator";
			NumFMUInputVariables = GetNumObjectsFound( cCurrentModuleObject );

			for ( i = 1; i <= NumFMUObjects; ++i ) {
				j = 1;
				for ( k = 1; k <= NumFMUInputVariables; ++k ) {
					GetObjectItem( cCurrentModuleObject, k, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, _, _, cAlphaFieldNames, cNumericFieldNames );
					if ( SameString( cAlphaArgs( 5 ), FMU( i ).Name ) ) {
						FMU( i ).TotNumOutputVariablesActuator = j;
						++j;
					}
				}
			}

			for ( i = 1; i <= NumFMUObjects; ++i ) {
				for ( j = 1; j <= FMU( i ).NumInstances; ++j ) {
					FMU( i ).Instance( j ).fmuOutputVariableActuator.allocate( NumFMUInputVariables );
					FMU( i ).Instance( j ).eplusInputVariableActuator.allocate( NumFMUInputVariables );
					k = 1;
					for ( l = 1; l <= NumFMUInputVariables; ++l ) {
						GetObjectItem( cCurrentModuleObject, l, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, _, _, cAlphaFieldNames, cNumericFieldNames );
						if ( SameString( cAlphaArgs( 5 ), FMU( i ).Name ) && SameString( cAlphaArgs( 6 ), FMU( i ).Instance( j ).Name ) ) {
							FMU( i ).Instance( j ).fmuOutputVariableActuator( k ).Name = cAlphaArgs( 7 );
							FMU( i ).Instance( j ).eplusInputVariableActuator( k ).Name = cAlphaArgs( 1 );

							// get the value reference by using the FMU name and the variable name.
							auto tempNameArr( getCharArrayFromString( FMU( i ).Instance( j ).fmuOutputVariableActuator( k ).Name ) );
							int tempLength( len( FMU( i ).Instance( j ).fmuOutputVariableActuator( k ).Name ) );
							FMU(i).Instance(j).fmuOutputVariableActuator(k).ValueReference = getValueReferenceByNameFMUOutputVariables(&tempNameArr[0], &tempLength, &FMU(i).Instance(j).Index);
							//FMU( i ).Instance( j ).fmuOutputVariableActuator( k ).Name = getStringFromCharArray( tempNameArr );

							if ( FMU( i ).Instance( j ).fmuOutputVariableActuator( k ).ValueReference == -999 ) {
								ShowSevereError( "ExternalInterface/InitExternalInterfaceFMUImport: Error when trying to get the value reference of the FMU output variable" );
								ShowContinueError( "\"" + FMU( i ).Instance( j ).fmuOutputVariableActuator( k ).Name + "\" of instance \"" + FMU( i ).Instance( j ).Name + "\"" );
								ShowContinueError( "of FMU \"" + FMU( i ).Name + "\" that will be mapped to an actuator." );
								ShowContinueError( "Please check the name of output variables in the input file and in the modelDescription file." );
								ErrorsFound = true;
								StopExternalInterfaceIfError();
							}

							if ( FMU( i ).Instance( j ).fmuOutputVariableActuator( k ).ValueReference == -1 ) {
								ShowSevereError( "ExternalInterface/InitExternalInterfaceFMUImport: Error when trying to get the value reference of the FMU output variable" );
								ShowContinueError( "\"" + FMU( i ).Instance( j ).fmuOutputVariableActuator( k ).Name + "\" of instance \"" + FMU( i ).Instance( j ).Name + "\"" );
								ShowContinueError( "of FMU \"" + FMU( i ).Name + "\" that will be mapped to an actuator." );
								ShowContinueError( "This variable is not an FMU output variable. Please check the causality of the variable in the modelDescription file." );
								ErrorsFound = true;
								StopExternalInterfaceIfError();
							}

							FMU( i ).Instance( j ).eplusInputVariableActuator( k ).VarIndex = FindEMSVariable( FMU( i ).Instance( j ).eplusInputVariableActuator( k ).Name, 0 );
							FMU( i ).Instance( j ).NumOutputVariablesActuator = k;
							if ( FMU( i ).Instance( j ).eplusInputVariableActuator( k ).VarIndex <= 0 ) {
								ShowSevereError( "ExternalInterface/InitExternalInterfaceFMUImport:declares variable \"" + FMU( i ).Instance( j ).eplusInputVariableActuator( k ).Name + "\"," );
								ShowContinueError( "but variable is not an EMS variable." );
								ErrorsFound = true;
								StopExternalInterfaceIfError();
							}
							++k;
						}
					}
					// set the flag useEMs to true. This will be used then to update the erl variables in erl data structure
					if ( FMU( i ).Instance( j ).NumOutputVariablesActuator >= 1 ) {
						useEMS = true;
					}
				}
			}

			// parse the fmu defined in the idf using the fmuUnpack with the flag --unpack.
			for ( i = 1; i <= NumFMUObjects; ++i ) {
				for ( j = 1; j <= FMU( i ).NumInstances; ++j ) {
					FMU( i ).Instance( j ).NumOutputVariablesInIDF = FMU( i ).Instance( j ).NumOutputVariablesSchedule + FMU( i ).Instance( j ).NumOutputVariablesVariable + FMU( i ).Instance( j ).NumOutputVariablesActuator;
					// check whether the number of output variables in fmu is bigger than in the idf
					if ( FMU( i ).Instance( j ).NumOutputVariablesInFMU > FMU( i ).Instance( j ).NumOutputVariablesInIDF ) {
						ShowWarningError( "InitExternalInterfaceFMUImport: The number of output variables defined in input file (" + TrimSigDigits( FMU( i ).Instance( j ).NumOutputVariablesInIDF ) + ')' );
						ShowContinueError( "of instance \"" + FMU( i ).Instance( j ).Name + "\" of FMU \"" + FMU( i ).Name + "\" is less than the number of output variables" );
						ShowContinueError( "in the modelDescription file (" + TrimSigDigits( FMU( i ).Instance( j ).NumOutputVariablesInFMU ) + ")." );
						ShowContinueError( "Check the input file and the modelDescription file again." );
					}
					// check whether the number of output variables in fmu is less than in the idf
					if ( FMU( i ).Instance( j ).NumOutputVariablesInFMU < FMU( i ).Instance( j ).NumOutputVariablesInIDF ) {
						ShowWarningError("InitExternalInterfaceFMUImport: The number of output variables defined in input file (" + TrimSigDigits(FMU(i).Instance(j).NumOutputVariablesInIDF) + ')');
						ShowContinueError( "of instance \"" + FMU( i ).Instance( j ).Name + "\" of FMU \"" + FMU( i ).Name + "\" is bigger than the number of output variables" );
						ShowContinueError( "in the modelDescription file (" + TrimSigDigits( FMU( i ).Instance( j ).NumOutputVariablesInFMU ) + ")." );
						ShowContinueError( "Check the input file and the modelDescription file again." );
					}

					DisplayString( "Number of inputs in instance \"" + FMU( i ).Instance( j ).Name + "\" of FMU \"" + FMU( i ).Name + "\" = \"" + TrimSigDigits( FMU( i ).Instance( j ).NumInputVariablesInIDF ) + "\"." );
					DisplayString( "Number of outputs in instance \"" + FMU( i ).Instance( j ).Name + "\" of FMU \"" + FMU( i ).Name + "\" = \"" + TrimSigDigits( FMU( i ).Instance( j ).NumOutputVariablesInIDF ) + "\"." );
				}
			}
			StopExternalInterfaceIfError();
			FirstCallIni = false;
		}
	}

	std::string trim(std::string const& str)
	{
		std::size_t first = str.find_first_not_of(' ');
		std::size_t last = str.find_last_not_of(' ');
		return str.substr(first, last - first + 1);
	}

	Real64
	GetCurSimStartTimeSeconds()
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Thierry S. Nouidui, Michael Wetter, Wangda Zuo
		//       DATE WRITTEN   August 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		//  Get the current month and day in the runperiod and convert
		//  it into seconds.

		// Using/Aliasing
		using DataEnvironment::Month;
		using DataEnvironment::DayOfMonth;
		using DataEnvironment::CurrentYearIsLeapYear;
		using DataGlobals::HourOfDay;

		// Locals
		Real64 simtime;

		if ( ! CurrentYearIsLeapYear ) {
			switch ( Month ) {
				case  1: simtime =   0; break;
				case  2: simtime =  31; break;
				case  3: simtime =  59; break;
				case  4: simtime =  90; break;
				case  5: simtime = 120; break;
				case  6: simtime = 151; break;
				case  7: simtime = 181; break;
				case  8: simtime = 212; break;
				case  9: simtime = 243; break;
				case 10: simtime = 273; break;
				case 11: simtime = 304; break;
				case 12: simtime = 334; break;
				default: simtime = 0;
			}
		} else {
			switch ( Month ) {
				case  1: simtime =   0; break;
				case  2: simtime =  31; break;
				case  3: simtime =  59+1; break;
				case  4: simtime =  90+1; break;
				case  5: simtime = 120+1; break;
				case  6: simtime = 151+1; break;
				case  7: simtime = 181+1; break;
				case  8: simtime = 212+1; break;
				case  9: simtime = 243+1; break;
				case 10: simtime = 273+1; break;
				case 11: simtime = 304+1; break;
				case 12: simtime = 334+1; break;
				default: simtime = 0;
			}
		}

		simtime = 24 * ( simtime + ( DayOfMonth - 1 ) ); // day of month does not need to be substracted??
		simtime = 60 * ( simtime + ( HourOfDay - 1 ) ); // hours to minutes
		simtime = 60 * ( simtime ); // minutes to seconds

		return simtime;
	}

	void
	CalcExternalInterfaceFMUImport()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Thierry S. Nouidui, Michael Wetter, Wangda Zuo
		//       DATE WRITTEN   08Aug2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine organizes the data exchange between FMU and EnergyPlus.

		// Using/Aliasing
		using DataEnvironment::TotalOverallSimDays;
		using DataEnvironment::TotDesDays;
		using ScheduleManager::GetDayScheduleIndex;
		using ScheduleManager::ExternalInterfaceSetSchedule;
		using RuntimeLanguageProcessor::isExternalInterfaceErlVariable;
		using RuntimeLanguageProcessor::FindEMSVariable;
		using RuntimeLanguageProcessor::ExternalInterfaceSetErlVariable;
		using EMSManager::ManageEMS;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using DataGlobals::WarmupFlag;
		using DataGlobals::KindOfSim;
		using DataGlobals::ksRunPeriodWeather;
		using DataGlobals::TimeStepZone;
		using DataGlobals::emsCallFromExternalInterface;
		using DataSystemVariables::UpdateDataDuringWarmupExternalInterface;
		using General::TrimSigDigits;

		// SUBROUTINE PARAMETER DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int i, j, k; // Loop counter

		static bool FirstCallDesignDays( true ); // Flag fo first call during warmup
		static bool FirstCallWUp( true ); // Flag fo first call during warmup
		static bool FirstCallTStep( true ); // Flag for first call during time stepping
		static int fmiEndSimulation(0); // Flag to indicate end of simulation

		Array1D_string Alphas( 5 );
		Array1D_int keyIndexes( 1 ); // Array index for
		Array1D_string NamesOfKeys( 1 ); // Specific key name

		if ( WarmupFlag && ( KindOfSim != ksRunPeriodWeather ) ) { // No data exchange during design days
			if ( FirstCallDesignDays ) {
				ShowWarningError( "ExternalInterface/CalcExternalInterfaceFMUImport: ExternalInterface does not exchange data during design days." );
			}
			FirstCallDesignDays = false;
		}
		if ( WarmupFlag && ( KindOfSim == ksRunPeriodWeather ) ) { // Data exchange after design days
			if ( FirstCallWUp ) {
				// set the report during warmup to true so that variables are also updated during the warmup
				UpdateDataDuringWarmupExternalInterface = true;
				hStep = ( 60.0 * TimeStepZone ) * 60.0;
				tStart = GetCurSimStartTimeSeconds();
				tStop = tStart + 24.0 * 3600.0;
				tComm = tStart;

				// instantiate and initialize the unpack fmus
				InstantiateInitializeFMUImport();

				// allocate memory for a temporary FMU that will be used at the end of the warmup
				FMUTemp.allocate( NumFMUObjects );
				for ( i = 1; i <= NumFMUObjects; ++i ) {
					FMUTemp( i ).Instance.allocate( FMU( i ).NumInstances );
				}
				for ( i = 1; i <= NumFMUObjects; ++i ) {
					for ( j = 1; j <= FMU( i ).NumInstances; ++j ) {
						FMUTemp( i ).Instance( j ).fmuInputVariable.allocate( FMU( i ).Instance( j ).NumInputVariablesInIDF );
						FMUTemp( i ).Instance( j ).eplusOutputVariable.allocate( FMU( i ).Instance( j ).NumInputVariablesInIDF );
						FMUTemp( i ).Instance( j ).fmuOutputVariableSchedule.allocate( FMU( i ).Instance( j ).NumOutputVariablesSchedule );
						FMUTemp( i ).Instance( j ).fmuOutputVariableVariable.allocate( FMU( i ).Instance( j ).NumOutputVariablesVariable );
						FMUTemp( i ).Instance( j ).fmuOutputVariableActuator.allocate( FMU( i ).Instance( j ).NumOutputVariablesActuator );
					}
				}

				GetSetVariablesAndDoStepFMUImport();
				tComm += hStep;
				FirstCallWUp = false;

			} else {
				if ( tComm < tStop ) {
					GetSetVariablesAndDoStepFMUImport();
					// Advance the communication time step
					tComm += hStep;
				} else {
					for ( i = 1; i <= NumFMUObjects; ++i ) {
						for ( j = 1; j <= FMU( i ).NumInstances; ++j ) {

							FMUTemp( i ).Instance( j ).NumInputVariablesInIDF = FMU( i ).Instance( j ).NumInputVariablesInIDF;
							for ( k = 1; k <= FMU( i ).Instance( j ).NumInputVariablesInIDF; ++k ) {
								FMUTemp( i ).Instance( j ).fmuInputVariable( k ).ValueReference = FMU( i ).Instance( j ).fmuInputVariable( k ).ValueReference;
								FMUTemp( i ).Instance( j ).eplusOutputVariable( k ).RTSValue = FMU( i ).Instance( j ).eplusOutputVariable( k ).RTSValue;
								FMUTemp( i ).Instance( j ).eplusOutputVariable( k ).ITSValue = FMU( i ).Instance( j ).eplusOutputVariable( k ).ITSValue;
								FMUTemp( i ).Instance( j ).eplusOutputVariable( k ).VarType = FMU( i ).Instance( j ).eplusOutputVariable( k ).VarType;
							}

							// save values that will be set in EnergyPlus (Schedule)
							FMUTemp( i ).Instance( j ).NumOutputVariablesSchedule = FMU( i ).Instance( j ).NumOutputVariablesSchedule;
							for ( k = 1; k <= FMU( i ).Instance( j ).NumOutputVariablesSchedule; ++k ) {
								FMUTemp( i ).Instance( j ).fmuOutputVariableSchedule( k ).RealVarValue = FMU( i ).Instance( j ).fmuOutputVariableSchedule( k ).RealVarValue;
							}

							// save values that will be set in EnergyPlus (Variable)
							FMUTemp( i ).Instance( j ).NumOutputVariablesVariable = FMU( i ).Instance( j ).NumOutputVariablesVariable;
							for ( k = 1; k <= FMU( i ).Instance( j ).NumOutputVariablesVariable; ++k ) {
								FMUTemp( i ).Instance( j ).fmuOutputVariableVariable( k ).RealVarValue = FMU( i ).Instance( j ).fmuOutputVariableVariable( k ).RealVarValue;
							}

							// save values that will be set in EnergyPlus (Actuator)
							FMUTemp( i ).Instance( j ).NumOutputVariablesActuator = FMU( i ).Instance( j ).NumOutputVariablesActuator;
							for ( k = 1; k <= FMU( i ).Instance( j ).NumOutputVariablesActuator; ++k ) {
								FMUTemp( i ).Instance( j ).fmuOutputVariableActuator( k ).RealVarValue = FMU( i ).Instance( j ).fmuOutputVariableActuator( k ).RealVarValue;
							}
						}
					}

					StopExternalInterfaceIfError();

					// Terminate all FMUs
					TerminateResetFreeFMUImport(fmiEndSimulation);

					// Reset the communication time step
					tComm = tStart;

					// Reinstantiate and reinitialize the FMUs
					InstantiateInitializeFMUImport();

					// Set the values that have been saved in the FMUs-- saveFMUStateVariables ()
					for ( i = 1; i <= NumFMUObjects; ++i ) {
						for ( j = 1; j <= FMU( i ).NumInstances; ++j ) {

							std::vector< unsigned int > valRefVec;
							for ( unsigned long x = 1; x <= size( FMU( i ).Instance( j ).fmuInputVariable ); ++x ) {
								valRefVec.push_back( FMU( i ).Instance( j ).fmuInputVariable( x ).ValueReference );
							}

							std::vector< Real64 > rtsValVec;
							for ( unsigned long x = 1; x <= size( FMU( i ).Instance( j ).eplusOutputVariable ); ++x ) {
								rtsValVec.push_back( FMU( i ).Instance( j ).eplusOutputVariable( x ).RTSValue );
							}

							// make the library call
							FMU( i ).Instance( j ).fmistatus = fmiEPlusSetReal( &FMU( i ).Instance( j ).fmicomponent, &valRefVec[0], &rtsValVec[0], &FMUTemp( i ).Instance( j ).NumInputVariablesInIDF, &FMU( i ).Instance( j ).Index );

							if ( FMU( i ).Instance( j ).fmistatus != fmiOK ) {
								ShowSevereError( "ExternalInterface/CalcExternalInterfaceFMUImport: Error when trying to set an input value in instance \"" + FMU( i ).Instance( j ).Name + "\"" );
								ShowContinueError( "of FMU \"" + FMU( i ).Name + "\"; Error Code = \"" + TrimSigDigits( FMU( i ).Instance( j ).fmistatus ) + "\"" );
								ErrorsFound = true;
								StopExternalInterfaceIfError();
							}
						}
					}
					// set the flag to reinitialize states to be true
					FlagReIni = true;
					GetSetVariablesAndDoStepFMUImport();
					FlagReIni = false;
					// advance one time step ahead for the next calculation
					tComm += hStep;
				}
			}
		}
		// BeginSimulation
		if ( !WarmupFlag && ( KindOfSim == ksRunPeriodWeather ) ) {

			if ( FirstCallTStep ) {
				// reset the UpdateDataDuringWarmupExternalInterface to be false.
				UpdateDataDuringWarmupExternalInterface = false;
				// The time is computed in seconds for FMU
				tStart = GetCurSimStartTimeSeconds();
				tStop = tStart + ( TotalOverallSimDays - TotDesDays ) * 24.0 * 3600.0;
				tComm = tStart;

				// Terminate all FMUs
				TerminateResetFreeFMUImport(fmiEndSimulation);

				// Reinstantiate and reinitialize the FMUs
				InstantiateInitializeFMUImport();

				// Set the values that have been saved in the FMUs-- saveFMUStateVariables ()
				for ( i = 1; i <= NumFMUObjects; ++i ) {
					for ( j = 1; j <= FMU( i ).NumInstances; ++j ) {

						// make vectors first
						std::vector< unsigned int > valRefVec;
						for ( unsigned long x = 1; x <= size( FMUTemp( i ).Instance( j ).fmuInputVariable ); ++x ) {
							valRefVec.push_back( FMUTemp( i ).Instance( j ).fmuInputVariable( x ).ValueReference );
						}
						std::vector< Real64 > rtsValVec;
						for ( unsigned long x = 1; x <= size( FMUTemp( i ).Instance( j ).eplusOutputVariable ); ++x ) {
							rtsValVec.push_back( FMUTemp( i ).Instance( j ).eplusOutputVariable( x ).RTSValue );
						}

						// make the library call
						FMU( i ).Instance( j ).fmistatus = fmiEPlusSetReal( &FMU( i ).Instance( j ).fmicomponent, &valRefVec[0], &rtsValVec[0], &FMUTemp( i ).Instance( j ).NumInputVariablesInIDF, &FMU( i ).Instance( j ).Index );

						if ( FMU( i ).Instance( j ).fmistatus != fmiOK ) {
							ShowSevereError( "ExternalInterface/CalcExternalInterfaceFMUImport: " );
							ShowContinueError( "Error when trying to set inputs in instance" );
							ShowContinueError( "\"" + FMU( i ).Instance( j ).Name + "\" of FMU \"" + FMU( i ).Name + "\"" );
							ShowContinueError( "Error Code = \"" + TrimSigDigits( FMU( i ).Instance( j ).fmistatus ) + "\"" );
							ErrorsFound = true;
							StopExternalInterfaceIfError();
						}
					}
				}
				// set the flag to reinitialize states to be true
				FlagReIni = true;
				GetSetVariablesAndDoStepFMUImport();
				FlagReIni = false;
				// advance one time step ahead for the next calculation
				tComm += hStep;
				FirstCallTStep = false;
			} else {
				if ( tComm != tStop ) {
					GetSetVariablesAndDoStepFMUImport();
					tComm += hStep;
				} else {
					// Terminate reset and free Slaves
					fmiEndSimulation = 1;
					TerminateResetFreeFMUImport(fmiEndSimulation);
					for ( i = 1; i <= NumFMUObjects; ++i ) {
						for ( j = 1; j <= FMU( i ).NumInstances; ++j ) {
							// Deallocate used objects
							FMUTemp( i ).Instance( j ).fmuInputVariable.deallocate();
							FMUTemp( i ).Instance( j ).eplusOutputVariable.deallocate();
							FMUTemp( i ).Instance( j ).fmuOutputVariableSchedule.deallocate();
							FMUTemp( i ).Instance( j ).fmuOutputVariableVariable.deallocate();
							FMUTemp( i ).Instance( j ).fmuOutputVariableActuator.deallocate();
						}
					}

					for ( i = 1; i <= NumFMUObjects; ++i ) {
						FMUTemp( i ).Instance.deallocate();
					}

					FMUTemp.deallocate();

					for ( i = 1; i <= NumFMUObjects; ++i ) {
						for ( j = 1; j <= FMU( i ).NumInstances; ++j ) {
							FMU( i ).Instance( j ).eplusInputVariableSchedule.deallocate();
							FMU( i ).Instance( j ).fmuOutputVariableSchedule.deallocate();
							FMU( i ).Instance( j ).eplusInputVariableVariable.deallocate();
							FMU( i ).Instance( j ).fmuOutputVariableVariable.deallocate();
							FMU( i ).Instance( j ).eplusInputVariableActuator.deallocate();
							FMU( i ).Instance( j ).fmuOutputVariableActuator.deallocate();
							FMU( i ).Instance( j ).fmuInputVariable.deallocate();
							FMU( i ).Instance( j ).checkfmuInputVariable.deallocate();
						}
					}

					for ( i = 1; i <= NumFMUObjects; ++i ) {
						FMU( i ).Instance.deallocate();
					}
					FMU.deallocate();
				}
			}
		}

	}

	void
	ValidateRunControl()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Michael Wetter
		//       DATE WRITTEN   December 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine ensures that the RunControl object is valid.

		// METHODOLOGY EMPLOYED:
		// Use GetObjectItem from the Input Processor

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using DataIPShortCuts::cCurrentModuleObject;
		using DataIPShortCuts::cAlphaArgs;
		using DataIPShortCuts::rNumericArgs;
		using DataIPShortCuts::cAlphaFieldNames;
		using DataIPShortCuts::cNumericFieldNames;

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumAlphas( 0 ); // Number of Alphas for each GetObjectItem call
		int NumNumbers( 0 ); // Number of Numbers for each GetObjectItem call
		int IOStatus( 0 ); // Used in GetObjectItem

		cCurrentModuleObject = "SimulationControl";
		int const NumRunControl = GetNumObjectsFound( cCurrentModuleObject );
		if ( NumRunControl > 0 ) {
			GetObjectItem( cCurrentModuleObject, 1, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, _, _, cAlphaFieldNames, cNumericFieldNames );
			if ( cAlphaArgs( 5 ) == "NO" ) { // This run does not have a weather file simulation.
				ShowSevereError( "ExternalInterface: Error in idf file, section SimulationControl:" );
				ShowContinueError( "When using the ExternalInterface, a run period from the weather file must be specified" );
				ShowContinueError( "in the idf file, because the ExternalInterface interface is not active during" );
				ShowContinueError( "warm-up and during sizing." );
				ErrorsFound = true;
			}
		}
	}

	void
	CalcExternalInterface()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Michael Wetter
		//       DATE WRITTEN   2Dec2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// Using/Aliasing
		using DataGlobals::SimTimeSteps;
		using DataGlobals::MinutesPerTimeStep;
		using DataGlobals::emsCallFromExternalInterface;
		using ScheduleManager::ExternalInterfaceSetSchedule;
		using RuntimeLanguageProcessor::ExternalInterfaceSetErlVariable;
		using EMSManager::ManageEMS;
		using General::TrimSigDigits;
		//using DataPrecisionGlobals;

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const nDblMax( 1024 ); // Maximum number of doubles

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int i; // Loop counter
		int retVal; // Return value from socket

		int flaWri; // flag to write to the socket
		int flaRea; // flag read from the socket
		int nDblWri; // number of doubles to write to socket
		int nDblRea; // number of doubles to read from socket
		Real64 curSimTim; // current simulation time
		Real64 preSimTim; // previous time step's simulation time

		Array1D< Real64 > dblValWri( nDblMax );
		Array1D< Real64 > dblValRea( nDblMax );
		std::string retValCha;
		bool continueSimulation; // Flag, true if simulation should continue
		static bool firstCall( true );
		static bool showContinuationWithoutUpdate( true );

		// Formats
		static gio::Fmt Format_1000( "(I2)" );

		if ( firstCall ) {
			DisplayString( "ExternalInterface starts first data exchange." );
			simulationStatus = 2;
			preSimTim = 0; // In the first call, E+ did not reset SimTimeSteps to zero
		} else {
			preSimTim = SimTimeSteps * MinutesPerTimeStep * 60.0;
		}

		// Socket asked to terminate simulation, but simulation continues
		if ( noMoreValues && showContinuationWithoutUpdate ) {
			if ( haveExternalInterfaceBCVTB ) {
				ShowWarningError( "ExternalInterface: Continue simulation without updated values from server at t =" + TrimSigDigits( preSimTim/3600.0, 2 ) + " hours" );
			}
			showContinuationWithoutUpdate = false;
		}

		// Usual branch, control is configured and simulation should continue
		if ( configuredControlPoints && ( ! noMoreValues ) ) {
			// Data to be exchanged
			nDblWri = size( varTypes );
			nDblRea = 0;
			flaWri = 0;

			// Get EnergyPlus variables
			if ( firstCall ) { // bug fix causing external interface to send zero at the beginning of sim, Thierry Nouidui
				for ( i = 1; i <= nDblWri; ++i ) {
					dblValWri( i ) = GetInternalVariableValue( varTypes( i ), keyVarIndexes( i ) );
				}
			} else {
				for ( i = 1; i <= nDblWri; ++i ) {
					dblValWri( i ) = GetInternalVariableValueExternalInterface( varTypes( i ), keyVarIndexes( i ) );
				}
			}

			// Exchange data with socket
			retVal = 0;
			flaRea = 0;
			if ( haveExternalInterfaceBCVTB ) {
				retVal = exchangedoubleswithsocket( &socketFD, &flaWri, &flaRea, &nDblWri, &nDblRea, &preSimTim, dblValWri.data_, &curSimTim, dblValRea.data_ );
			} else if ( haveExternalInterfaceFMUExport ) {
				retVal = exchangedoubleswithsocketFMU( &socketFD, &flaWri, &flaRea, &nDblWri, &nDblRea, &preSimTim, dblValWri.data_, &curSimTim, dblValRea.data_, &FMUExportActivate );
			}
			continueSimulation = true;

			// Check for errors, in which case we terminate the simulation loop
			// Added a check since the FMUExport is terminated with the flaRea set to 1.
			if ( haveExternalInterfaceBCVTB || ( haveExternalInterfaceFMUExport && ( flaRea == 0 ) ) ) {
				if ( retVal != 0 ) {
					continueSimulation = false;
					gio::write( retValCha, Format_1000 ) << retVal;
					ShowSevereError( "ExternalInterface: Socket communication received error value \"" + retValCha + "\" at time = " + TrimSigDigits( preSimTim / 3600, 2 ) + " hours." );
					gio::write( retValCha, Format_1000 ) << flaRea;
					ShowContinueError( "ExternalInterface: Flag from server \"" + retValCha + "\"." );
					ErrorsFound = true;
					StopExternalInterfaceIfError();
				}
			}

			// Check communication flag
			if ( flaRea != 0 ) {
				// No more values will be received in future steps
				// Added a check since the FMUExport  is terminated with the flaRea set to 1.
				noMoreValues = true;
				gio::write( retValCha, Format_1000 ) << flaRea;
				if ( haveExternalInterfaceBCVTB ) {
					ShowSevereError( "ExternalInterface: Received end of simulation flag at time = " + TrimSigDigits( preSimTim / 3600, 2 ) + " hours." );
					StopExternalInterfaceIfError();
				}
			}

			// Make sure we get the right number of double values, unless retVal != 0
			if ( ( flaRea == 0 ) && ( ! ErrorsFound ) && continueSimulation && ( nDblRea != isize( varInd ) ) ) {
				ShowSevereError( "ExternalInterface: Received \"" + TrimSigDigits( nDblRea ) + "\" double values, expected \"" + TrimSigDigits( size( varInd ) ) + "\"." );
				ErrorsFound = true;
				StopExternalInterfaceIfError();
			}

			// No errors found. Assign exchanged variables
			if ( ( flaRea == 0 ) && continueSimulation ) {
				for ( i = 1; i <= isize( varInd ); ++i ) {
					if ( inpVarTypes( i ) == indexSchedule ) {
						ExternalInterfaceSetSchedule( varInd( i ), dblValRea( i ) );
					} else if ( ( inpVarTypes( i ) == indexVariable ) || ( inpVarTypes( i ) == indexActuator ) ) {
						ExternalInterfaceSetErlVariable( varInd( i ), dblValRea( i ) );
					} else {
						ShowContinueError( "ExternalInterface: Error in finding the type of the input variable for EnergyPlus" );
						ShowContinueError( "variable index: " + std::to_string( i ) + ". Variable will not be updated." );
					}
				}
			}
		}

		// If we have Erl variables, we need to call ManageEMS so that they get updated in the Erl data structure
		if ( useEMS ) {
			ManageEMS( emsCallFromExternalInterface );
		}

		firstCall = false; // bug fix causing external interface to send zero at the beginning of sim, Thierry Nouidui

	}

	void
	GetReportVariableKey(
		Array1S_string const varKeys, // Standard variable name
		int const numberOfKeys, // Number of keys=size(varKeys)
		Array1S_string const varNames, // Standard variable name
		Array1S_int keyVarIndexes, // Array index
		Array1S_int varTypes // Types of variables in keyVarIndexes
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Michael Wetter
		//       DATE WRITTEN   2Dec2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Gets the sensor key index and type for the specified variable key and name

		// Using/Aliasing
		using InputProcessor::SameString;

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int varType( 0 ); // 0=not found, 1=integer, 2=real, 3=meter
		int numKeys( 0 ); // Number of keys found
		int varAvgSum( 0 ); // Variable  is Averaged=1 or Summed=2
		int varStepType( 0 ); // Variable time step is Zone=1 or HVAC=2
		std::string varUnits; // Units sting, may be blank
		Array1D_int keyIndexes; // Array index for
		Array1D_string NamesOfKeys; // Specific key name
		int Loop, iKey; // Loop counters

		// Get pointers for variables to be sent to Ptolemy
		for ( Loop = 1; Loop <= numberOfKeys; ++Loop ) {
			GetVariableKeyCountandType( varNames( Loop ), numKeys, varType, varAvgSum, varStepType, varUnits );
			if ( varType != 0 ) {
				NamesOfKeys.allocate( numKeys );
				keyIndexes.allocate( numKeys );
				GetVariableKeys( varNames( Loop ), varType, NamesOfKeys, keyIndexes );
				// Find key index whose keyName is equal to keyNames(Loop)
				int max( NamesOfKeys.size() );
				for ( iKey = 1; iKey <= max; ++iKey ) {
					if ( NamesOfKeys( iKey ) == varKeys( Loop ) ) {
						keyVarIndexes( Loop ) = keyIndexes( iKey );
						varTypes( Loop ) = varType;
						break;
					}
				}
				keyIndexes.deallocate();
				NamesOfKeys.deallocate();
			}
			if ( ( varType == 0 ) || ( iKey > numKeys ) ) {
				ShowSevereError( "ExternalInterface: Simulation model has no variable \"" + varNames( Loop ) + "\" with key \"" + varKeys( Loop ) + "\"." );
				ErrorsFound = true;
			}
		}
	}

	void
	WarnIfExternalInterfaceObjectsAreUsed( std::string const & ObjectWord )
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Michael Wetter
		//       DATE WRITTEN   December 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine writes a warning if ExternalInterface objects are used in the
		// idf file, but the ExternalInterface link is not specified.

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;

		int const NumObjects = GetNumObjectsFound( ObjectWord );
		if ( NumObjects > 0 ) {
			ShowWarningError( "IDF file contains object \"" + ObjectWord + "\"," );
			ShowContinueError( "but object \"ExternalInterface\" with appropriate key entry is not specified. Values will not be updated." );
		}
	}

	void
	VerifyExternalInterfaceObject()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Michael Wetter
		//       DATE WRITTEN   12Dec2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine verifies the correctness of the fields of
		// the ExternalInterface object in the idf file

		// Using/Aliasing
		using InputProcessor::GetObjectItem;
		using InputProcessor::SameString;
		using DataIPShortCuts::cCurrentModuleObject;
		using DataIPShortCuts::cAlphaArgs;
		using DataIPShortCuts::rNumericArgs;
		using DataIPShortCuts::cAlphaFieldNames;
		using DataIPShortCuts::cNumericFieldNames;

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumAlphas( 0 ); // Number of Alphas for each GetObjectItem call
		int NumNumbers( 0 ); // Number of Numbers for each GetObjectItem call
		int IOStatus( 0 ); // Used in GetObjectItem

		cCurrentModuleObject = "ExternalInterface";
		GetObjectItem( cCurrentModuleObject, 1, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, _, _, cAlphaFieldNames, cNumericFieldNames );
		if ( ( ! SameString( cAlphaArgs( 1 ), "PtolemyServer" ) ) && ( ! SameString( cAlphaArgs( 1 ), "FunctionalMockupUnitImport" ) ) && ( ! SameString( cAlphaArgs( 1 ), "FunctionalMockupUnitExport" ) ) ) {
			ShowSevereError( "VerifyExternalInterfaceObject: " + cCurrentModuleObject + ", invalid " + cAlphaFieldNames( 1 ) + "=\"" + cAlphaArgs( 1 ) + "\"." );
			ShowContinueError( "only \"PtolemyServer or FunctionalMockupUnitImport or FunctionalMockupUnitExport\" allowed." );
			ErrorsFound = true;
		}

	}

	std::vector< char >
	getCharArrayFromString( std::string const & originalString )
	{
		// c_str returns null terminated, so we don't need a +1?
		return std::vector< char >( originalString.c_str(), originalString.c_str() + originalString.size() );
	}

	std::string
	getStringFromCharArray( std::vector< char > originalCharArray )
	{
		originalCharArray.push_back('\0');
		return std::string( &originalCharArray.front() );
	}

} // ExternalInterface

} // EnergyPlus
