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

// CLI Headers
#include <ezOptionParser.hpp>

// ObjexxFCL Headers
#include <ObjexxFCL/gio.hh>

// Project headers
#include <CommandLineInterface.hh>
#include <DataGlobals.hh>
#include <DataStringGlobals.hh>
#include <DataSystemVariables.hh>
#include <DisplayRoutines.hh>
#include <EnergyPlus.hh>
#include <FileSystem.hh>
#include <InputProcessor.hh>
#include <OutputProcessor.hh>
#include <OutputReportTabular.hh>
#include <OutputReports.hh>
#include <SimulationManager.hh>
#include <SolarShading.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus{

namespace CommandLineInterface{

using namespace DataGlobals;
using namespace DataStringGlobals;
using namespace DataSystemVariables;
using namespace FileSystem;
using namespace InputProcessor;
using namespace SimulationManager;
using namespace OutputReportTabular;
using namespace OutputProcessor;
using namespace SolarShading;
using namespace ez;

int
ProcessArgs(int argc, const char * argv[])
{
	typedef  std::string::size_type  size_type;

	// Expand long-name options using "=" sign into two arguments
	// and expand multiple short options into separate arguments
	std::vector< std::string > arguments;

	for ( int i = 0; i < argc; ++i ) {

		std::string inputArg( argv[ i ] );

		std::string const dash( "-" );
		size_type const doubleDashPosition = inputArg.find("--");
		size_type const equalsPosition = inputArg.find("=");

		if ( doubleDashPosition == 0 && equalsPosition != std::string::npos ) { // --option=value
			arguments.push_back(inputArg.substr(0,equalsPosition));
			arguments.push_back(inputArg.substr(equalsPosition + 1,inputArg.size() - 1));
		} else if ( ( inputArg.size() > 2 ) && ( inputArg[ 0 ] == '-' ) && ( inputArg[ 1 ] != '-' ) ) { // -abc style
			for ( size_type c = 1; c < inputArg.size(); ++c ) {
				arguments.push_back( dash + inputArg[ c ] );
			}
		} else { // ?
			arguments.push_back(inputArg);
		}
	}

//Fix This is problematic for a few reasons:
//  Using ezOptionParser with a raw C-string interface is asking for trouble: Find something taking std::string if possible
//  Passing out pointers returned by c_str() is bad form:
//   They are pointers to internally-managed memory in std::string
//   They are invalid as soon as the string goes out of scope or is modified
//   In this case the strings may be in scope and unmodified until parse is done but this is red flag usage
	// convert to vector of C strings for option parser
	std::vector< const char * > cStrArgs;
	cStrArgs.reserve(arguments.size());
	for ( size_type i = 0; i < arguments.size(); ++i ) {
		cStrArgs.push_back(arguments[i].c_str());
	}

	size_type const argCount = cStrArgs.size();

	bool const legacyMode = (argCount == 1);

	// Define options
	ezOptionParser opt;

	opt.overview = VerString;

	opt.syntax = "energyplus [options] [input-file]";

	opt.add("", 0, 0, 0, "Force annual simulation", "-a", "--annual");

	opt.add("", 0, 1, 0, "Output directory path (default: current directory)", "-d", "--output-directory");

	opt.add("", 0, 0, 0, "Force design-day-only simulation", "-D", "--design-day");

	opt.add("", 0, 0, 0, "Display help information", "-h", "--help");

	opt.add("Energy+.idd", 0, 1, 0, "Input data dictionary path (default: Energy+.idd in executable directory)", "-i", "--idd");

	opt.add("", 0, 0, 0, "Run EPMacro prior to simulation", "-m", "--epmacro");

	opt.add("", 0, 1, 0, "Prefix for output file names (default: eplus)", "-p", "--output-prefix");

	opt.add("", 0, 0, 0, "Run ReadVarsESO after simulation", "-r", "--readvars");

	opt.add("L", 0, 1, 0, "Suffix style for output file names (default: L)\n   L: Legacy (e.g., eplustbl.csv)\n   C: Capital (e.g., eplusTable.csv)\n   D: Dash (e.g., eplus-table.csv)", "-s", "--output-suffix");

	opt.add("", 0, 0, 0, "Display version information", "-v", "--version");

	opt.add("in.epw", 0, 1, 0, "Weather file path (default: in.epw in current directory)", "-w", "--weather");

	opt.add("", 0, 0, 0, "Run ExpandObjects prior to simulation", "-x", "--expandobjects");

	opt.example = "energyplus -w weather.epw -r input.idf";

	std::string errorFollowUp = "Type 'energyplus --help' for usage.";

	// Parse arguments
	opt.parse( argCount, &cStrArgs[0] );

	// print arguments parsed (useful for debugging)
	/*std::string pretty;
	opt.prettyPrint(pretty);
	std::cout << pretty << std::endl;*/

	std::string usage;
	opt.getUsage(usage);

	// Set path of EnergyPlus program path
	exeDirectory = getParentDirectoryPath(getAbsolutePath(getProgramPath()));

	opt.get("-w")->getString(inputWeatherFileName);

	opt.get("-i")->getString(inputIddFileName);

	if (!opt.isSet("-i") && !legacyMode)
		inputIddFileName = exeDirectory + inputIddFileName;

	std::string dirPathName;

	opt.get("-d")->getString(dirPathName);

	runReadVars = opt.isSet("-r");

	DDOnlySimulation = opt.isSet("-D");

	AnnualSimulation = opt.isSet("-a");

	// Process standard arguments
	if (opt.isSet("-h")) {
		DisplayString(usage);
		exit(EXIT_SUCCESS);
	}

	if (opt.isSet("-v")) {
		DisplayString(VerString);
		exit(EXIT_SUCCESS);
	}

	if (opt.lastArgs.size() == 1) {
		for ( size_type i = 0; i < opt.lastArgs.size(); ++i ) {
			std::string const & arg( *opt.lastArgs[i] );
			inputIdfFileName = arg;
		}
	}
	if (opt.lastArgs.size() == 0) inputIdfFileName = "in.idf";

	// Convert all paths to native paths
	makeNativePath(inputIdfFileName);
	makeNativePath(inputWeatherFileName);
	makeNativePath(inputIddFileName);
	makeNativePath(dirPathName);

	std::vector<std::string> badOptions;
	if (opt.lastArgs.size() > 1u) {
		bool invalidOptionFound = false;
		for ( size_type i = 0; i < opt.lastArgs.size(); ++i ) {
			std::string const & arg( *opt.lastArgs[i] );
			if (arg.substr(0,1) == "-") {
				invalidOptionFound = true;
				DisplayString("ERROR: Invalid option: " + arg);
			}
		}
		if (invalidOptionFound) {
			DisplayString(errorFollowUp);
			exit(EXIT_FAILURE);
		} else {
			DisplayString("ERROR: Multiple input files specified:");
			for ( size_type i = 0; i < opt.lastArgs.size(); ++i ) {
				std::string const & arg( *opt.lastArgs[i] );
				DisplayString("  Input file #" + std::to_string(i+1) +  ": " + arg);
			}
			DisplayString(errorFollowUp);
			exit(EXIT_FAILURE);
		}
	}

	idfFileNameOnly = removeFileExtension(getFileName(inputIdfFileName));
	idfDirPathName = getParentDirectoryPath(inputIdfFileName);

	std::string weatherFilePathWithoutExtension = removeFileExtension(inputWeatherFileName);

	bool runExpandObjects(false);
	bool runEPMacro(false);

	runExpandObjects = opt.isSet("-x");

	runEPMacro = opt.isSet("-m");

	if (opt.isSet("-d") ) {
		// Add the trailing path character if necessary
		if (dirPathName[dirPathName.size()-1]!=pathChar) {
			dirPathName+=pathChar;
		}

		// Create directory if it doesn't already exist
		makeDirectory(dirPathName);
	}

	// File naming scheme
	std::string outputFilePrefix;
	if (opt.isSet("-p")) {
		std::string prefixOutName;
		opt.get("-p")->getString(prefixOutName);
		makeNativePath(prefixOutName);
		outputFilePrefix = dirPathName + prefixOutName;
	} else {
		outputFilePrefix = dirPathName + "eplus";
	}

	std::string suffixType;
	opt.get("-s")->getString(suffixType);


	std::string outputEpmdetFileName;
	std::string outputEpmidfFileName;

	std::string outputExpidfFileName;
	std::string outputExperrFileName;

	std::string normalSuffix;
	std::string tableSuffix;
	std::string mapSuffix;
	std::string zszSuffix;
	std::string sszSuffix;
	std::string meterSuffix;
	std::string sqliteSuffix;
	std::string adsSuffix;
	std::string screenSuffix;

	if (suffixType == "L" || suffixType == "l")	{

		normalSuffix = "out";
		tableSuffix = "tbl";
		mapSuffix = "map";
		zszSuffix = "zsz";
		sszSuffix = "ssz";
		meterSuffix = "mtr";
		sqliteSuffix = "sqlite";
		adsSuffix = "ADS";
		screenSuffix = "screen";

	} else if (suffixType == "D" || suffixType == "d") {

		normalSuffix = "";
		tableSuffix = "-table";
		mapSuffix = "-map";
		zszSuffix = "-zsz";
		sszSuffix = "-ssz";
		meterSuffix = "-meter";
		sqliteSuffix = "-sqlite";
		adsSuffix = "-ads";
		screenSuffix = "-screen";

	} else if (suffixType == "C" || suffixType == "c") {

		normalSuffix = "";
		tableSuffix = "Table";
		mapSuffix = "Map";
		zszSuffix = "Zsz";
		sszSuffix = "Ssz";
		meterSuffix = "Meter";
		sqliteSuffix = "Sqlite";
		adsSuffix = "Ads";
		screenSuffix = "Screen";

	} else {
		DisplayString("ERROR: Unrecognized argument for output suffix style: " + suffixType);
		DisplayString(errorFollowUp);
		exit(EXIT_FAILURE);
	}

	// EnergyPlus files
	outputAuditFileName = outputFilePrefix + normalSuffix + ".audit";
	outputBndFileName = outputFilePrefix + normalSuffix + ".bnd";
	outputDxfFileName = outputFilePrefix + normalSuffix + ".dxf";
	outputEioFileName = outputFilePrefix + normalSuffix + ".eio";
	outputEndFileName = outputFilePrefix + normalSuffix + ".end";
	outputErrFileName = outputFilePrefix + normalSuffix + ".err";
	outputEsoFileName = outputFilePrefix + normalSuffix + ".eso";
	outputMtdFileName = outputFilePrefix + normalSuffix + ".mtd";
	outputMddFileName = outputFilePrefix + normalSuffix + ".mdd";
	outputMtrFileName = outputFilePrefix + normalSuffix + ".mtr";
	outputRddFileName = outputFilePrefix + normalSuffix + ".rdd";
	outputShdFileName = outputFilePrefix + normalSuffix + ".shd";
	outputDfsFileName = outputFilePrefix + normalSuffix + ".dfs";
	outputEddFileName = outputFilePrefix + normalSuffix + ".edd";
	outputIperrFileName = outputFilePrefix + normalSuffix + ".iperr";
	outputSlnFileName = outputFilePrefix + normalSuffix + ".sln";
	outputSciFileName = outputFilePrefix + normalSuffix + ".sci";
	outputWrlFileName = outputFilePrefix + normalSuffix + ".wrl";
	outputSqlFileName = outputFilePrefix + normalSuffix + ".sql";
	outputDbgFileName = outputFilePrefix + normalSuffix + ".dbg";
	outputTblCsvFileName = outputFilePrefix + tableSuffix + ".csv";
	outputTblHtmFileName = outputFilePrefix + tableSuffix + ".htm";
	outputTblTabFileName = outputFilePrefix + tableSuffix + ".tab";
	outputTblTxtFileName = outputFilePrefix + tableSuffix + ".txt";
	outputTblXmlFileName = outputFilePrefix + tableSuffix + ".xml";
	outputMapTabFileName = outputFilePrefix + mapSuffix + ".tab";
	outputMapCsvFileName = outputFilePrefix + mapSuffix + ".csv";
	outputMapTxtFileName = outputFilePrefix + mapSuffix + ".txt";
	outputZszCsvFileName = outputFilePrefix + zszSuffix + ".csv";
	outputZszTabFileName = outputFilePrefix + zszSuffix + ".tab";
	outputZszTxtFileName = outputFilePrefix + zszSuffix + ".txt";
	outputSszCsvFileName = outputFilePrefix + sszSuffix + ".csv";
	outputSszTabFileName = outputFilePrefix + sszSuffix + ".tab";
	outputSszTxtFileName = outputFilePrefix + sszSuffix + ".txt";
	outputAdsFileName = outputFilePrefix + adsSuffix + ".out";
	if (suffixType == "L" || suffixType == "l") {
		outputSqliteErrFileName = dirPathName + sqliteSuffix + ".err";
	}
	else {
		outputSqliteErrFileName = outputFilePrefix + sqliteSuffix + ".err";
	}
	outputScreenCsvFileName = outputFilePrefix + screenSuffix + ".csv";
	outputDelightInFileName = "eplusout.delightin";
	outputDelightOutFileName = "eplusout.delightout";
	outputDelightEldmpFileName = "eplusout.delighteldmp";
	outputDelightDfdmpFileName = "eplusout.delightdfdmp";
	EnergyPlusIniFileName = "Energy+.ini";
	inStatFileName = weatherFilePathWithoutExtension + ".stat";
	TarcogIterationsFileName = "TarcogIterations.dbg";
	eplusADSFileName = idfDirPathName+"eplusADS.inp";

	// Readvars files
	outputCsvFileName = outputFilePrefix + normalSuffix + ".csv";
	outputMtrCsvFileName = outputFilePrefix + meterSuffix + ".csv";
	outputRvauditFileName = outputFilePrefix + normalSuffix + ".rvaudit";

	// EPMacro files
	outputEpmdetFileName = outputFilePrefix + normalSuffix + ".epmdet";
	outputEpmidfFileName = outputFilePrefix + normalSuffix + ".epmidf";

	// ExpandObjects files
	outputExpidfFileName = outputFilePrefix + normalSuffix + ".expidf";
	outputExperrFileName = outputFilePrefix + normalSuffix + ".experr";


	// Handle bad options
	if (!opt.gotExpected(badOptions)) {
		for ( size_type i = 0; i < badOptions.size(); ++i ) {
			DisplayString("ERROR: Unexpected number of arguments for option " + badOptions[i]);
		}
		DisplayString(errorFollowUp);
		exit(EXIT_FAILURE);
	}

	// This is a place holder in case there are required options in the future
	if ( !opt.gotRequired(badOptions) ) {
		for ( size_type i = 0; i < badOptions.size(); ++i ) {
			DisplayString("ERROR: Missing required option " + badOptions[i]);
		}
		DisplayString(errorFollowUp);
		exit(EXIT_FAILURE);
	}

	if ( opt.firstArgs.size() > 1 || opt.unknownArgs.size() > 0 ) {
		for ( size_type i = 1; i < opt.firstArgs.size(); ++i ) {
			std::string const & arg( *opt.firstArgs[i] );
			DisplayString("ERROR: Invalid option: " + arg);
		}
		for ( size_type i = 0; i < opt.unknownArgs.size(); ++i ) {
			std::string const & arg( *opt.unknownArgs[i] );
			DisplayString("ERROR: Invalid option: " + arg);
		}
		DisplayString(errorFollowUp);
		exit(EXIT_FAILURE);
	}

	// Error for cases where both design-day and annual simulation switches are set
	if (DDOnlySimulation && AnnualSimulation) {
		DisplayString("ERROR: Cannot force both design-day and annual simulations. Set either '-D' or '-a', but not both.");
		DisplayString(errorFollowUp);
		exit(EXIT_FAILURE);
	}

	// Read path from INI file if it exists
	bool EPlusINI;
	int LFN; // Unit Number for reads
	std::string::size_type TempIndx;
	int iostatus;
	bool FileExists;

	// Check for IDD and IDF files
	{ IOFlags flags; gio::inquire( EnergyPlusIniFileName, flags ); EPlusINI = flags.exists(); }
	if ( EPlusINI ) {
		LFN = GetNewUnitNumber();
		{ IOFlags flags; flags.ACTION( "read" ); gio::open( LFN, EnergyPlusIniFileName, flags ); iostatus = flags.ios(); }
		if ( iostatus != 0 ) {
			DisplayString( "ERROR: Could not open file " + EnergyPlusIniFileName + " for input (read)." );
			exit(EXIT_FAILURE);
		}
		{ IOFlags flags; gio::inquire( LFN, flags ); CurrentWorkingFolder = flags.name(); }
		// Relying on compiler to supply full path name here
		TempIndx = index( CurrentWorkingFolder, pathChar, true );
		if ( TempIndx == std::string::npos ) {
			CurrentWorkingFolder = "";
		} else {
			CurrentWorkingFolder.erase( TempIndx + 1 );
		}
		//       Get directories from ini file
		ReadINIFile( LFN, "program", "dir", ProgramPath );

		gio::close( LFN );

		inputIddFileName = ProgramPath + "Energy+.idd";
	}

	// Check if specified files exist
	{ IOFlags flags; gio::inquire( inputIddFileName, flags ); FileExists = flags.exists(); }
	if ( ! FileExists ) {
		DisplayString("ERROR: Could not find input data dictionary: " + getAbsolutePath(inputIddFileName) + "." );
		DisplayString(errorFollowUp);
		exit(EXIT_FAILURE);
	}

	{ IOFlags flags; gio::inquire( inputIdfFileName, flags ); FileExists = flags.exists(); }
	if ( ! FileExists ) {
		DisplayString("ERROR: Could not find input data file: " + getAbsolutePath(inputIdfFileName) + "." );
		DisplayString(errorFollowUp);
		exit(EXIT_FAILURE);
	}

	if (opt.isSet("-w") && !DDOnlySimulation) {
		{ IOFlags flags; gio::inquire( inputWeatherFileName, flags ); FileExists = flags.exists(); }
		if ( ! FileExists ) {
			DisplayString("ERROR: Could not find weather file: " + getAbsolutePath(inputWeatherFileName) + "." );
			DisplayString(errorFollowUp);
			exit(EXIT_FAILURE);
		}
	}

	OutputFileDebug = GetNewUnitNumber();
	{ IOFlags flags; flags.ACTION( "write" ); gio::open( OutputFileDebug, outputDbgFileName, flags ); iostatus = flags.ios(); }
	if ( iostatus != 0 ) {
		DisplayString( "ERROR: Could not open output debug file: " + outputDbgFileName + "." );
		exit(EXIT_FAILURE);
	}

	// Preprocessors (These will likely move to a new file)
	if (runEPMacro) {
		std::string epMacroPath = exeDirectory + "EPMacro" + exeExtension;
		{ IOFlags flags; gio::inquire( epMacroPath, flags ); FileExists = flags.exists(); }
		if (!FileExists) {
			DisplayString("ERROR: Could not find EPMacro executable: " + getAbsolutePath(epMacroPath) + "." );
			exit(EXIT_FAILURE);
		}
		std::string epMacroCommand = "\"" + epMacroPath + "\"";
		bool inputFileNamedIn =
				(getAbsolutePath(inputIdfFileName) == getAbsolutePath("in.imf"));

		if (!inputFileNamedIn) linkFile(inputIdfFileName.c_str(), "in.imf");
		DisplayString("Running EPMacro...");
		systemCall(epMacroCommand);
		if (!inputFileNamedIn) removeFile("in.imf");
		moveFile("audit.out",outputEpmdetFileName);
		moveFile("out.idf",outputEpmidfFileName);
	   inputIdfFileName = outputEpmidfFileName;
	}

	if (runExpandObjects) {
		std::string expandObjectsPath = exeDirectory + "ExpandObjects" + exeExtension;
		{ IOFlags flags; gio::inquire( expandObjectsPath, flags ); FileExists = flags.exists(); }
		if (!FileExists) {
			DisplayString("ERROR: Could not find ExpandObjects executable: " + getAbsolutePath(expandObjectsPath) + "." );
			exit(EXIT_FAILURE);
		}
		std::string expandObjectsCommand = "\"" + expandObjectsPath + "\"";
		bool inputFileNamedIn =
				(getAbsolutePath(inputIdfFileName) == getAbsolutePath("in.idf"));

		bool iddFileNamedEnergy =
				(getAbsolutePath(inputIddFileName) == getAbsolutePath("Energy+.idd"));

		if (!inputFileNamedIn)
			linkFile(inputIdfFileName.c_str(), "in.idf");
		if (!iddFileNamedEnergy)
			linkFile(inputIddFileName,"Energy+.idd");
		systemCall(expandObjectsCommand);
		if (!inputFileNamedIn)
			removeFile("in.idf");
		if (!iddFileNamedEnergy)
			removeFile("Energy+.idd");
		moveFile("expandedidf.err", outputExperrFileName);
		{ IOFlags flags; gio::inquire( "expanded.idf", flags ); FileExists = flags.exists(); }
		if (FileExists) {
			moveFile("expanded.idf", outputExpidfFileName);
		    inputIdfFileName = outputExpidfFileName;
		}
	}


	return 0;
}

//Fix This is Fortranic code that needs to be brought up to C++ style
//     All the index and len and strip should be eliminated and replaced by string calls only where needed
//     I/o with std::string should not be pulling in trailing blanks so stripping should not be needed, etc.
//     Rewinding is a big performance hit and should be avoided if possible
//     Case-insensitive comparison is much faster than converting strings to upper or lower case
//     Each strip and case conversion is a heap hit and should be avoided if possible
void
ReadINIFile(
	int const UnitNumber, // Unit number of the opened INI file
	std::string const & Heading, // Heading for the parameters ('[heading]')
	std::string const & KindofParameter, // Kind of parameter to be found (String)
	std::string & DataOut // Output from the retrieval
)
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Linda K. Lawrie
	//       DATE WRITTEN   September 1997
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// This routine reads the .ini file and retrieves
	// the path names for the files from it.

	// METHODOLOGY EMPLOYED:
	// Duplicate the kind of reading the Windows "GetINISetting" would
	// do.

	// REFERENCES:
	// na

	// Using/Aliasing
	using namespace EnergyPlus;
	using namespace DataStringGlobals;
	using namespace DataSystemVariables;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	static std::string LINE;
	static std::string LINEOut;
	std::string Param;
	std::string::size_type ILB;
	std::string::size_type IRB;
	std::string::size_type IEQ;
	std::string::size_type IPAR;
	std::string::size_type IPOS;
	std::string::size_type ILEN;
	int ReadStat;
	bool EndofFile;
	bool Found;
	bool NewHeading;

	// Formats
	static gio::Fmt Format_700( "(A)" );

	DataOut.clear();

	// I tried ADJUSTL(TRIM(KindofParameter)) and got an internal compiler error

	Param = KindofParameter;
	strip( Param );
	ILEN = len( Param );
	gio::rewind( UnitNumber ); //Performance Ouch!
	EndofFile = false;
	Found = false;
	NewHeading = false;

	while ( ! EndofFile && ! Found ) {
		{ IOFlags flags; gio::read( UnitNumber, Format_700, flags ) >> LINE; ReadStat = flags.ios(); }
		if ( ReadStat < GoodIOStatValue ) {
			EndofFile = true;
			break;
		}

		if ( len( LINE ) == 0 ) continue; // Ignore Blank Lines

		ConvertCaseToLower( LINE, LINEOut ); // Turn line into lower case
		//        LINE=LINEOut

		if ( ! has( LINEOut, Heading ) ) continue;

		//                                  See if [ and ] are on line
		ILB = index( LINEOut, '[' );
		IRB = index( LINEOut, ']' );
		if ( ILB == std::string::npos && IRB == std::string::npos ) continue;
		if ( ! has( LINEOut, '[' + Heading + ']' ) ) continue; // Must be really correct heading line

		//                                  Heading line found, now looking for Kind
		while ( ! EndofFile && ! NewHeading ) {
			{ IOFlags flags; gio::read( UnitNumber, Format_700, flags ) >> LINE; ReadStat = flags.ios(); }
			if ( ReadStat < GoodIOStatValue ) {
				EndofFile = true;
				break;
			}
			strip( LINE );

			if ( len( LINE ) == 0 ) continue; // Ignore Blank Lines

			ConvertCaseToLower( LINE, LINEOut ); // Turn line into lower case
			//         LINE=LINEOut

			ILB = index( LINEOut, '[' );
			IRB = index( LINEOut, ']' );
			NewHeading = ( ILB != std::string::npos && IRB != std::string::npos );

			//                                  Should be a parameter line
			//                                  KindofParameter = string
			IEQ = index( LINEOut, '=' );
			IPAR = index( LINEOut, Param );
			if ( IEQ == std::string::npos ) continue;
			if ( IPAR == std::string::npos ) continue;
			if ( IPAR != 0 ) continue;
			if ( ! has( LINEOut, Param + '=' ) ) continue; // needs to be param=

			//                                  = found and parameter found.
			if ( IPAR > IEQ ) continue;

			//                                  parameter = found
			//                                  Set output string to start with non-blank character

			DataOut = stripped( LINE.substr( IEQ + 1 ) );
			Found = true;
			break;

		}

	}

	if ( Param == "dir" ) {
		IPOS = len( DataOut );
		if ( IPOS != 0 ) {
			// Non-blank make sure last position is valid path character
			//  (Set in DataStringGlobals)

			if ( DataOut[ IPOS - 1 ] != pathChar ) {
				DataOut += pathChar;
			}

		}
	}

}

} // CommandLineInterface namespace
} // EnergyPlus namespace
