// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>
#include <DataPrecisionGlobals.hh>
#include <DataStringGlobals.hh>
#include <ExternalInterface.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <UtilityRoutines.hh>
#include <ScheduleManager.hh>
#include <RuntimeLanguageProcessor.hh>
#include <DisplayRoutines.hh>
#include <DataIPShortCuts.hh>

// C++ Standard Library Headers
#include <string>

// Objexx Headers
#include <ObjexxFCL/FArray1D.hh>
#include <ObjexxFCL/Inquire.hh>
#include <ObjexxFCL/IOFlags.hh>

// FMI-Related Headers
extern "C" {
#include <FMI/main.h>
#include <BCVTB/utilSocket.h>
#include <BCVTB/utilXml.h>
}

namespace EnergyPlus {

namespace ExternalInterface {

	// Module containing the routines dealing with the BCVTB interface

	// MODULE INFORMATION:
	//       AUTHOR         Michael Wetter
	//       DATE WRITTEN   5Jan2010
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// Do nothing except show fatal error when an instance is made of an ExternalInterface
	// object.
	// This module replaces ExternalInterface.f90 when building EnergyPlus without the bcvtb LIB and DLL.
	// This should only be done during development. The official EnergyPlus release needs to be
	// compiled with ExternalInterface.f90, and linked to a dummy bcvtb LIB and DLL.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// http://simulationresearch.lbl.gov/bcvtb

	// OTHER NOTES:
	// na

	// USE STATEMENTS:
	// na

	// Data
	// DERIVED TYPE DEFINITIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:
	Real64 tComm( 0.0 ); // Communication time step
	Real64 tStop( 3600.0 ); // Stop time used during the warmup period
	Real64 tStart( 0.0 ); // Start time used during the warmup period
	Real64 hStep( 15.0 ); // Communication step size
	bool FlagReIni( false ); // Flag for reinitialization of states in GetSetAndDoStep
	std::string FMURootWorkingFolder( " " ); // FMU root working folder
	int LEN_FMU_ROOT_DIR;

    // MODULE PARAMETER DEFINITIONS:
    int const maxVar( 1024 );             // Maximum number of variables to be exchanged
    int const maxErrMsgLength( 10000 );   // Maximum error message length from xml schema validation
    int const indexSchedule( 1 );  // Index for schedule in inpVarTypes
    int const indexVariable( 2 );  // Index for variable in inpVarTypes
    int const indexActuator( 3 );  // Index for actuator in inpVarTypes
    int const nInKeys( 3 );  // Number of input variables available in ExternalInterface (=highest index* number)
    int const fmiOK( 0 );          // fmiOK
    int const fmiWarning( 1 );     // fmiWarning
    int const fmiDiscard( 2 );     // fmiDiscard
    int const fmiError( 3 );       // fmiError
    int const fmiFatal( 4 );       // fmiPending
    int const fmiPending( 5 );     // fmiPending
	std::string const socCfgFilNam( "socket.cfg" ); // socket configuration file
    std::string const BlankString( " " );

	FArray1D< FMUType > FMU; // Variable Types structure
    FArray1D< checkFMUInstanceNameType > checkInstanceName; // Variable Types structure for checking instance names
    int NumExternalInterfaces( 0 ); //Number of ExternalInterface objects
    int NumExternalInterfacesBCVTB( 0 ); //Number of BCVTB ExternalInterface objects
    int NumExternalInterfacesFMUImport( 0 ); //Number of FMU ExternalInterface objects
    int NumExternalInterfacesFMUExport( 0 ); //Number of FMU ExternalInterface objects
    int NumFMUObjects( 0 ); //Number of FMU objects
    int FMUExportActivate( 0 ); //FMU Export flag
    bool haveExternalInterfaceBCVTB( false ); //Flag for BCVTB interface
    bool haveExternalInterfaceFMUImport( false ); //Flag for FMU-Import interface
    bool haveExternalInterfaceFMUExport( false ); //Flag for FMU-Export interface
    int simulationStatus( 1 ); // Status flag. Used to report during
    // which phase an error occured.
    // (1=initialization, 2=time stepping)

	FArray1D< int > keyVarIndexes; // Array index for specific key name
    FArray1D< int > varTypes; // Types of variables in keyVarIndexes
    FArray1D< int > varInd; // Index of ErlVariables for ExternalInterface
    int socketFD( -1 ); // socket file descriptor
    bool ErrorsFound( false ); // Set to true if errors are found
    bool noMoreValues( false ); //Flag, true if no more values
    // will be sent by the server

    FArray1D< std::string > varKeys; // Keys of report variables used for data exchange
    FArray1D< std::string > varNames; // Names of report variables used for data exchange
    FArray1D< int > inpVarTypes; // Names of report variables used for data exchange
    FArray1D< std::string > inpVarNames; // Names of report variables used for data exchange

    bool configuredControlPoints( false ); // True if control points have been configured
    bool useEMS( false ); // Will be set to true if ExternalInterface writes to EMS variables or actuators

	// SUBROUTINE SPECIFICATIONS FOR MODULE ExternalInterface:
	
	// Functions

	void
	ExternalInterfaceExchangeVariables()
	{

		//SUBROUTINE INFORMATION:
		//       AUTHOR         Michael Wetter
		//       DATE WRITTEN   2Dec2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Exchanges variables between EnergyPlus and the BCVTB socket.

		// USE STATEMENTS:
		using DataGlobals::WarmupFlag;
		using DataGlobals::KindOfSim;
		using DataGlobals::ksRunPeriodWeather;
		using DataGlobals::ZoneTSReporting;

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		bool static GetInputFlag( true ); // First time, input is "gotten"
		std::string errorMessage( BlankString ); // Error message
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
			retValErrMsg = checkOperatingSystem( (char*)errorMessage.c_str() );
			if ( retValErrMsg != 0 ) {
				ShowSevereError( "ExternalInterface/ExternalInterfaceExchangeVariables:" + errorMessage );
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

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int retVal; // Return value, needed to catch return value of function call
		bool fileExist; // Set to true if file exists

		// Try to establish socket connection. This is needed if Ptolemy started E+,
		//  but E+ had an error before the call to InitExternalInterface.

		IOFlags flags;
		Inquire( socCfgFilNam, flags );

		if ( ( socketFD == -1 ) && ( flags.exists() ) ) {
			socketFD = establishclientsocket( (char*)socCfgFilNam.c_str() );
		}

		if ( socketFD >= 0 ) {
			retVal = sendclientmessage( &socketFD, &FlagToWriteToSocket );
			// Don't close socket as this may give sometimes an IOException in Windows
			// This problem seems to affect only Windows but not Mac
			//     close(socketFD)
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

		// USE STATEMENTS:
		using ScheduleManager::GetDayScheduleIndex;
		using RuntimeLanguageProcessor::isExternalInterfaceErlVariable;
		using RuntimeLanguageProcessor::FindEMSVariable;
		using DataGlobals::WeathSimReq;
		using General::TrimSigDigits;

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		bool static FirstCall( true ); // First time, input has been read
		std::string const simCfgFilNam( "variables.cfg" );
		int i, j; // loop counters
		std::string xmlStrOut; // xml values in string, separated by ';'
		std::string xmlStrOutTyp; // xml values in string, separated by ';'
		std::string xmlStrInKey; // xml values in string, separated by ';'
		std::string xmlStrIn; // xml values in string, separated by ';'
		std::string xmlStrInTyp; // xml values in string, separated by ';'
		int static nOutVal; // Number of output values (E+ -> ExternalInterface)
		int static nInpVar; // Number of input values (ExternalInterface -> E+)
		int retVal; // Return value of function call, used for error handling
		int counter( 0 ); // Counter for ErlVariables
		int mainVersion; // The version number
		FArray1D< std::string > curVals; // Names of schedules (i.e., schedule names)
		int curNumInpVal; // current number of input values for the InputValType
		std::string validateErrMsg; // error returned when xml Schema validate failed
		int errMsgLen; // the length of the error message
		bool socFileExist; // Set to true if socket configuration
		// file exists
		bool simFileExist; // Set to true if simulation configuration
		// file exists
		
		if ( FirstCall ) {
			DisplayString( "ExternalInterface initializes." );
			// do one time initializations
			
			if ( haveExternalInterfaceBCVTB ) {
				// Check version number
				mainVersion = getmainversionnumber();
				if ( mainVersion < 0.0 ) {
					ShowSevereError( "ExternalInterface: BCVTB is not installed in this version." );
					ErrorsFound = true;
					StopExternalInterfaceIfError();
				}
			}

			// Get port number
			IOFlags flags;
			Inquire( socCfgFilNam, flags );
			if ( flags.exists() ) {
				socketFD = establishclientsocket( (char*)socCfgFilNam.c_str() );
				if ( socketFD < 0 ) {
					ShowSevereError( "ExternalInterface: Could not open socket. File descriptor = " + TrimSigDigits(socketFD) );
					ErrorsFound = true;
				}
			} else {
				ShowSevereError("ExternalInterface: Did not find file " + socCfgFilNam );
				ShowContinueError("This file needs to be in same directory as in.idf.");
				ShowContinueError("Check the documentation for the ExternalInterface.");
				ErrorsFound = true;
			}
			
			// Make sure that idf file specified a run period other than
			//  design day and system sizing.
			ValidateRunControl();

			StopExternalInterfaceIfError();

			int const lenXmlStr( 1000 ); // TODO: Fix string length here
			xmlStrOut = "";
			xmlStrOutTyp = "";
			xmlStrInKey = "";
			xmlStrIn = "";
			
			// Get input and output variables for EnergyPlus in sequence
			xmlStrInKey = "schedule,variable,actuator\0";
			// Check if simCfgFilNam exists.
			IOFlags flags2;
			Inquire( simCfgFilNam, flags2 );
			if ( flags2.exists() ) {
				if ( haveExternalInterfaceBCVTB ) {
					retVal = getepvariables( (char*)simCfgFilNam.c_str(), (char*)xmlStrOutTyp.c_str(), (char*)xmlStrOut.c_str(), (int*)nOutVal, (char*)xmlStrInKey.c_str(), (int*)nInKeys, (char*)xmlStrIn.c_str(), (int*)nInpVar, inpVarTypes.data_, (int*)lenXmlStr );
				} else if ( haveExternalInterfaceFMUExport ) {
					retVal = getepvariablesFMU( (char*)simCfgFilNam.c_str(), (char*)xmlStrOutTyp.c_str(), (char*)xmlStrOut.c_str(), (int*)nOutVal, (char*)xmlStrInKey.c_str(), (int*)nInKeys, (char*)xmlStrIn.c_str(), (int*)nInpVar, inpVarTypes.data_, (int*)lenXmlStr );
				}
				// handle errors when reading variables.cfg file
				if ( retVal < 0 ) {
					ShowSevereError( "ExternalInterface: Error when getting input and output variables for EnergyPlus," );
					ShowContinueError( "check simulation.log for error message." );
					ErrorsFound = true;
				}
			} else {
				ShowSevereError( "ExternalInterface: Did not find file " + simCfgFilNam );
				ShowContinueError( "This file needs to be in same directory as in.idf." );
				ShowContinueError( "Check the documentation for the ExternalInterface." );
				ErrorsFound = true;
			}
			StopExternalInterfaceIfError();

			if ( (nOutVal + nInpVar) > maxVar ) {
				ShowSevereError("ExternalInterface: Too many variables to be exchanged.");
				ShowContinueError("Attempted to exchange " + TrimSigDigits(nOutVal) + " outputs");
				ShowContinueError("plus " + TrimSigDigits(nOutVal) + " inputs.");
				ShowContinueError("Maximum allowed is sum is " + TrimSigDigits(maxVar) );
				ShowContinueError("To fix, increase maxVar in ExternalInterface.cc");
				ErrorsFound = true;
			}
			StopExternalInterfaceIfError();
			
			if ( nOutVal < 0 ) {
				ShowSevereError("ExternalInterface: Error when getting number of xml values for outputs.");
				ErrorsFound = true;
			} else {
				ParseString( xmlStrOut, varNames, nOutVal );
				ParseString( xmlStrOutTyp, varKeys, nOutVal );
			}
			StopExternalInterfaceIfError();

			if ( nInpVar < 0 ) {
				ShowSevereError("ExternalInterface: Error when getting number of xml values for inputs.");
				ErrorsFound = true;
			} else {
				ParseString( xmlStrIn, inpVarNames, nInpVar );
			}
			StopExternalInterfaceIfError();

			DisplayString( "Number of outputs in ExternalInterface = " + TrimSigDigits( nOutVal ) );
			DisplayString( "Number of inputs  in ExternalInterface = " + TrimSigDigits( nInpVar ) );

			FirstCall = false;

		} else if ( ! configuredControlPoints ) {
			keyVarIndexes.allocate( nOutVal );
			varTypes.allocate( nOutVal );
			GetReportVariableKey( varKeys, nOutVal, varNames, keyVarIndexes, varTypes );
			varInd.allocate( nInpVar );
			for ( i = 1; i <= nInpVar; i++ ) {
				if ( inpVarTypes( i ) == indexSchedule ) {
					varInd( i ) = GetDayScheduleIndex( inpVarNames( i ) );
				} else if ( inpVarTypes(i) == indexVariable ) {
					varInd( i ) = FindEMSVariable( inpVarNames( i ), 0 );
				} else if ( inpVarTypes(i) == indexActuator ) {
					varInd( i ) = FindEMSVariable( inpVarNames( i ), 0 );
				}
				if ( varInd( i ) <= 0 ) {
					ShowSevereError( "ExternalInterface: Error, xml file \"" + simCfgFilNam + "\" declares variable \"" + inpVarNames( i ) );
					ShowContinueError( "but variable was not found in idf file." );
					ErrorsFound = true;
				}
			}
			StopExternalInterfaceIfError();
			// Configure Erl variables
			for ( i = 1; i <= nInpVar; i++ ) {
				if ( inpVarTypes( i ) == indexVariable ) { // ems-globalvariable
					useEMS = true;
					if ( ! isExternalInterfaceErlVariable( varInd( i ) ) ) {
						ShowSevereError( "ExternalInterface: Error, xml file \"" + simCfgFilNam + "\" declares variable \"" + inpVarNames( i ) + "\"" );
						ShowContinueError( "But this variable is an ordinary Erl variable, not an ExternalInterface variable." );
						ShowContinueError( "You must specify a variable of type \"ExternalInterface:Variable\"" );
						ErrorsFound = true;
					}
				} else if ( inpVarTypes( i ) == indexActuator ) { // ems-actuator
					useEMS = true;
					if ( ! isExternalInterfaceErlVariable( varInd( i ) ) ) {
						ShowSevereError( "ExternalInterface: Error, xml file \"" + simCfgFilNam + "\" declares variable \"" + inpVarNames( i ) + "\"" );
						ShowContinueError( "But this variable is an ordinary Erl actuator, not an ExternalInterface actuator." );
						ShowContinueError( "You must specify a variable of type \"ExternalInterface:Actuator\"" );
						ErrorsFound = true;
					}
				}
			}
			configuredControlPoints = true;
		}
		StopExternalInterfaceIfError();
		
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

		// USE STATEMENTS:
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
		//FArray1D< std::string > Alphas;  // Alpha items for object
		//FArray< Real64 > Numbers; // Numeric items for object
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		int IOStatus; //Used in GetObjectItem
		int Loop; // Loop counter
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name

		cCurrentModuleObject = "ExternalInterface";
		NumExternalInterfaces = GetNumObjectsFound( cCurrentModuleObject );

		for ( Loop = 1; Loop <= NumExternalInterfaces; Loop++ ) { // This loop determines whether the external interface is for FMU or BCVTB
			GetObjectItem( cCurrentModuleObject, Loop, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, _, _, cAlphaFieldNames, cNumericFieldNames );
			if ( SameString( cAlphaArgs(1), "PtolemyServer" ) ) { 
				// The BCVTB interface is activated.
				NumExternalInterfacesBCVTB++;
			} else if ( SameString( cAlphaArgs(1), "FunctionalMockupUnitImport" ) ) {
				// The functional mock up unit import interface is activated.
				NumExternalInterfacesFMUImport++;
			} else if ( SameString( cAlphaArgs(1), "FunctionalMockupUnitExport" ) ) {
				// The functional mock up unit export interface is activated.
				NumExternalInterfacesFMUExport++;
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
			varKeys.allocate(maxVar); // Keys of report variables used for data exchange
			//varKeys = ' ';
			varNames.allocate(maxVar); // Names of report variables used for data exchange
			//varNames = ' ';
			inpVarTypes.allocate(maxVar); // Names of report variables used for data exchange
			//inpVarTypes = 0;
			inpVarNames.allocate(maxVar); // Names of report variables used for data exchange
			//inpVarNames = ' ';
			VerifyExternalInterfaceObject();
		} else if ( ( NumExternalInterfacesBCVTB == 0 ) && ( NumExternalInterfacesFMUExport == 1 ) ) {
			haveExternalInterfaceFMUExport = true;
			FMUExportActivate = 1;
			DisplayString( "Instantiating FunctionalMockupUnitExport interface" );
			varKeys.allocate(maxVar); // Keys of report variables used for data exchange
			//varKeys = ' ';
			varNames.allocate(maxVar); // Names of report variables used for data exchange
			//varNames = ' ';
			inpVarTypes.allocate(maxVar); // Names of report variables used for data exchange
			//inpVarTypes = 0;
			inpVarNames.allocate(maxVar); // Names of report variables used for data exchange
			//inpVarNames = ' ';
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
	CalcExternalInterface()
	{}
    
    void
	ParseString(
		std::string const str,
		FArray1D< std::string > & ele,
		int const nEle
	)
	{}
    
    void
	GetReportVariableKey(
		FArray1D< std::string > varKeys,
		int const numberOfKeys,
		FArray1D< std::string > varNames,
		FArray1D< int > & keyVarIndexes,
		FArray1D< int > & varTypes
	)
	{}
    
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
		if ( ( NumExternalInterfacesBCVTB != 0 ) || ( NumExternalInterfacesFMUExport != 0 ) ) {
			if ( ErrorsFound ) {
				// Check if the socket is open
				if ( socketFD >= 0 ) {
					// Socket is open
					if ( simulationStatus == 1 ) {
						retVal = sendclientmessage( socketFD, -10 );
					} else {
						retVal = sendclientmessage( socketFD, -20 );
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
	ValidateRunControl()
	{}
    
    void
	WarnIfExternalInterfaceObjectsAreUsed( std::string ObjectWord )
	{}
    
    void
	CalcExternalInterfaceFMUImport()
	{}
    
    void
	InitExternalInterfaceFMUImport()
	{}
    
    void
	InstantiateInitializeFMUImport()
	{}
    
    void
	TerminateResetFreeFMUImport()
	{}
    
    void
	GetSetVariablesAndDoStepFMUImport()
	{}

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

		// USE STATEMENTS:
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
		if ( ( ! SameString( cAlphaArgs( 1 ), "PtolemyServer" ) ) &&
		     ( ! SameString( cAlphaArgs( 1 ), "FunctionalMockupUnitImport" ) ) &&
		     ( ! SameString( cAlphaArgs( 1 ), "FunctionalMockupUnitExport" ) ) ) {
			ShowSevereError( "VerifyExternalInterfaceObject: " + cCurrentModuleObject + ", invalid " + cAlphaFieldNames( 1 ) + "=\"" + cAlphaArgs(1) + "\"" );
			ShowContinueError( "only \"PtolemyServer or FunctionalMockupUnitImport or FunctionalMockupUnitExport\" allowed." );
			ErrorsFound = true;
		}

	}


	//     NOTICE

	//     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
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

} // ExternalInterface

} // EnergyPlus
