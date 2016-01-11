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

#ifndef ExternalInterface_hh_INCLUDED
#define ExternalInterface_hh_INCLUDED

// FMI-Related Headers
extern "C" {
#include <FMI/main.h>
}


// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>
#include <DataPrecisionGlobals.hh>
#include <DataStringGlobals.hh>
#include <ExternalInterface.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <UtilityRoutines.hh>

// C++ Standard Library Headers
#include <string>

// Objexx Headers
#include <ObjexxFCL/Array1D.hh>


namespace EnergyPlus {

namespace ExternalInterface {

	// MODULE VARIABLE DECLARATIONS:
	extern Real64 tComm;
	extern Real64 tStop;
	extern Real64 tStart;
	extern Real64 hStep;
	extern bool FlagReIni;
	extern std::string FMURootWorkingFolder;
	extern int LEN_FMU_ROOT_DIR;

	// MODULE PARAMETER DEFINITIONS:
	extern int const maxVar;             // Maximum number of variables to be exchanged
	extern int const maxErrMsgLength;   // Maximum error message length from xml schema validation
	extern int const indexSchedule;  // Index for schedule in inpVarTypes
	extern int const indexVariable;  // Index for variable in inpVarTypes
	extern int const indexActuator;  // Index for actuator in inpVarTypes
	extern int nInKeys;  // Number of input variables available in ExternalInterface (=highest index* number)
	extern int const fmiOK;          // fmiOK
	extern int const fmiWarning;     // fmiWarning
	extern int const fmiDiscard;     // fmiDiscard
	extern int const fmiError;       // fmiError
	extern int const fmiFatal;       // fmiPending
	extern int const fmiPending;     // fmiPending
	extern std::string const socCfgFilNam; // socket configuration file
	extern std::string const BlankString;

	struct fmuInputVariableType {

		std::string Name; // Name of FMU input variable
		int ValueReference; // = fmiValueReference specific to FMU variable

		// Default Constructor
		fmuInputVariableType() :
			Name( BlankString ),
			ValueReference( 0 )
		{}

	};

	struct checkFMUInstanceNameType {

		std::string Name; // Name of fmu instance

		// Default Constructor
		checkFMUInstanceNameType() :
			Name( BlankString )
		{}

	};

	struct eplusOutputVariableType {

		std::string Name; // Variable name in EnergyPlus
		std::string VarKey; // Key value in EnergyPlus
		Real64 RTSValue;    // Real value of variable at the Zone Time Step
		int ITSValue; // Integer value of variable at the Zone Time Step
		int VarIndex; // Index Value of variable
		int VarType; // Type of variable at the Zone Time Step
		std::string VarUnits; // Units string, may be blank

		// Default Constructor
		eplusOutputVariableType() :
			Name( BlankString ),
			VarKey( BlankString ),
			RTSValue( 0.0 ),
			ITSValue( 0 ),
			VarIndex( 0 ),
			VarType( 0 ),
			VarUnits( BlankString )
		{}

	};

	struct  fmuOutputVariableScheduleType {

		std::string Name; // Name of fmu output variable --> schedule in energyplus
		Real64 RealVarValue; // = Real value at the Zone Time Step
		int ValueReference; // = fmiValueReference specific to FMU variable

		// Default Constructor
		fmuOutputVariableScheduleType() :
			Name( BlankString ),
			RealVarValue( 0.0 ),
			ValueReference( 0 )
		{}

	};

	struct  fmuOutputVariableVariableType {

		std::string Name; // Name of fmu output variable --> variable in energyplus
		Real64 RealVarValue; // = Real value at the Zone Time Step
		int ValueReference; // = fmiValueReference specific to FMU variable

		// Default Constructor
		fmuOutputVariableVariableType() :
			Name( BlankString ),
			RealVarValue( 0.0 ),
			ValueReference( 0 )
		{}

	};

	struct  fmuOutputVariableActuatorType {

		std::string Name; // Name of fmu output variable --> actuator in energyplus
		Real64 RealVarValue; // = Real value at the Zone Time Step
		int ValueReference; // = fmiValueReference specific to FMU variable

		// Default Constructor
		fmuOutputVariableActuatorType() :
			Name( BlankString ),
			RealVarValue( 0.0 ),
			ValueReference( 0 )
		{}

	};

	struct  eplusInputVariableScheduleType {

		std::string Name; // Name of energyplus input variable from Type schedule
		int VarIndex; // Index Value of this variable
		int InitialValue; // Initial value used during the warmup

		// Default Constructor
		eplusInputVariableScheduleType() :
			Name( BlankString ),
			VarIndex( 0 )
		{
			// InitialValue not initialized in default constructor
		}

	};

	struct  eplusInputVariableVariableType {

		std::string Name; // Name of energyplus input variable from Type variable
		int VarIndex; // Index Value of this variable

		// Default Constructor
		eplusInputVariableVariableType() :
			Name( BlankString ),
			VarIndex( 0 )
		{}

	};

	struct  eplusInputVariableActuatorType {

		std::string Name; // Name of energyplus input variable from Type actuator
		int VarIndex; // Index Value of this variable

		// Default Constructor
		eplusInputVariableActuatorType() :
			Name( BlankString ),
			VarIndex( 0 )
		{}

	};

	struct InstanceType {

		std::string Name; // FMU Filename
		std::string modelID; // FMU modelID
		std::string modelGUID; // FMU modelGUID
		std::string WorkingFolder; // Path to the FMU wokring folder
		std::string WorkingFolder_wLib; // Path to the binaries
		std::string fmiVersionNumber; // Version number of FMI used
		int NumInputVariablesInFMU; // Number of input variables in fmu
		int NumInputVariablesInIDF; // Number of fmus input variables in idf
		int NumOutputVariablesInFMU; // Number of output variables in fmu
		int NumOutputVariablesInIDF; // Number of output variables in idf
		int NumOutputVariablesSchedule; // Number of output variables from type schedule
		int NumOutputVariablesVariable; // Number of output variables from type variable
		int NumOutputVariablesActuator; // Number of output variables from type actuator
		int LenModelID; // Length of modelID trimmed
		int LenModelGUID; // Length of modelGUID trimmed
		int LenWorkingFolder; // Length of working folder trimmed
		int LenWorkingFolder_wLib; // Length of working folder with libraries trimmed
		fmiComponent fmicomponent; // FMU instance
		fmiStatus fmistatus; // Status of fmi
		int Index; // Index of FMU
		// Variable Types structure for fmu input variables
		Array1D< fmuInputVariableType > fmuInputVariable;
		// Variable Types structure for checking duplicates fmu input variables
		Array1D< fmuInputVariableType > checkfmuInputVariable;
		// Variable Types structure for energyplus output variables
		Array1D< eplusOutputVariableType > eplusOutputVariable;
		// Variable Types structure for fmu output variables from type schedule
		Array1D< fmuOutputVariableScheduleType > fmuOutputVariableSchedule;
		// Variable Types structure for energyplus input variables from type schedule
		Array1D< eplusInputVariableScheduleType > eplusInputVariableSchedule;
		// Variable Types structure for fmu output variables from type variable
		Array1D< fmuOutputVariableVariableType > fmuOutputVariableVariable;
		// Variable Types structure for energyplus input variables from type variable
		Array1D< eplusInputVariableVariableType > eplusInputVariableVariable;
		// Variable Types structure for fmu output variables from type actuator
		Array1D< fmuOutputVariableActuatorType > fmuOutputVariableActuator;
		// Variable Types structure for energyplus input variables from type actuator
		Array1D< eplusInputVariableActuatorType > eplusInputVariableActuator;

		// Default Constructor
		InstanceType() :
			Name( BlankString ),
			modelID( BlankString ),
			modelGUID( BlankString ),
			WorkingFolder( BlankString ),
			WorkingFolder_wLib( BlankString ),
			fmiVersionNumber( BlankString ),
			NumInputVariablesInFMU( 0 ),
			NumInputVariablesInIDF( 0 ),
			NumOutputVariablesInFMU( 0 ),
			NumOutputVariablesInIDF( 0 ),
			NumOutputVariablesSchedule( 0 ),
			NumOutputVariablesVariable( 0 ),
			NumOutputVariablesActuator( 0 ),
			LenModelID( 0 ),
			LenModelGUID( 0 ),
			LenWorkingFolder( 0 ),
			LenWorkingFolder_wLib( 0 )
		{
			//fmiStatus, Index, and arrays not initialized in default constructor
		}

	};

	struct FMUType {

		std::string Name; // FMU Filename
		Real64 TimeOut; // Default TimeOut value
		int Visible; // Default Visible value
		int Interactive; // Default Interactive value
		int LoggingOn; // Default LoggingOn value
		int NumInstances; // Number of Instances
		int TotNumInputVariablesInIDF; // Number of input variables
		int TotNumOutputVariablesSchedule; // Number of output variables from type schedule
		int TotNumOutputVariablesVariable; // Number of output variables from type variable
		int TotNumOutputVariablesActuator; // Number of output variables from type actuator
		Array1D< InstanceType > Instance; // Variable Types structure for energyplus input variables from type actuator

		// Default Constructor
		FMUType() :
			Name( BlankString ),
			TimeOut( 0.0 ),
			Visible( 0 ),
			Interactive( 0 ),
			LoggingOn( 0 ),
			NumInstances( 0 ),
			TotNumInputVariablesInIDF( 0 ),
			TotNumOutputVariablesSchedule( 0 ),
			TotNumOutputVariablesVariable( 0 ),
			TotNumOutputVariablesActuator( 0 )
		{
			//Instance not instantiated in default constructor
		}

	};

	extern Array1D< FMUType > FMU; // Variable Types structure
	extern Array1D< FMUType > FMUTemp; // Variable Types structure
	extern Array1D< checkFMUInstanceNameType > checkInstanceName; // Variable Types structure for checking instance names
	extern int NumExternalInterfaces; //Number of ExternalInterface objects
	extern int NumExternalInterfacesBCVTB; //Number of BCVTB ExternalInterface objects
	extern int NumExternalInterfacesFMUImport; //Number of FMU ExternalInterface objects
	extern int NumExternalInterfacesFMUExport; //Number of FMU ExternalInterface objects
	extern int NumFMUObjects; //Number of FMU objects
	extern int FMUExportActivate; //FMU Export flag
	extern bool haveExternalInterfaceBCVTB; //Flag for BCVTB interface
	extern bool haveExternalInterfaceFMUImport; //Flag for FMU-Import interface
	extern bool haveExternalInterfaceFMUExport; //Flag for FMU-Export interface
	extern int simulationStatus; // Status flag. Used to report during
	// which phase an error occured.
	// (1=initialization, 2=time stepping)

	extern Array1D< int > keyVarIndexes; // Array index for specific key name
	extern Array1D< int > varTypes; // Types of variables in keyVarIndexes
	extern Array1D< int > varInd; // Index of ErlVariables for ExternalInterface
	extern int socketFD; // socket file descriptor
	extern bool ErrorsFound; // Set to true if errors are found
	extern bool noMoreValues; //Flag, true if no more values
	// will be sent by the server

	extern Array1D< std::string > varKeys; // Keys of report variables used for data exchange
	extern Array1D< std::string > varNames; // Names of report variables used for data exchange
	extern Array1D< int > inpVarTypes; // Names of report variables used for data exchange
	extern Array1D< std::string > inpVarNames; // Names of report variables used for data exchange

	extern bool configuredControlPoints; // True if control points have been configured
	extern bool useEMS; // Will be set to true if ExternalInterface writes to EMS variables or actuators

	// Functions

	void
	ExternalInterfaceExchangeVariables();

	void
	CloseSocket( int const FlagToWriteToSocket );

	void
	InitExternalInterface();

	void
	GetExternalInterfaceInput();

	void
	CalcExternalInterface();

	void
	ParseString(
		std::string const & str,
		Array1S_string ele,
		int const nEle
	);

	void
	GetReportVariableKey(
		Array1S_string const varKeys,
		int const numberOfKeys,
		Array1S_string const varNames,
		Array1S_int keyVarIndexes,
		Array1S_int varTypes
	);

	std::vector< char >
	getCharArrayFromString( std::string const & originalString );

	std::string
	getStringFromCharArray( std::vector< char > originalCharArray );

	void
	StopExternalInterfaceIfError();

	void
	ValidateRunControl();

	void
	WarnIfExternalInterfaceObjectsAreUsed( std::string const & ObjectWord );

	void
	CalcExternalInterfaceFMUImport();

	void
	InitExternalInterfaceFMUImport();

	void
	InstantiateInitializeFMUImport();

	void
	TerminateResetFreeFMUImport(int fmiEndSimulation);

	void
	GetSetVariablesAndDoStepFMUImport();

	void
	VerifyExternalInterfaceObject();

	Real64
	GetCurSimStartTimeSeconds();

	std::string
	trim(std::string const& str);

} // ExternalInterface

} // EnergyPlus

#endif
