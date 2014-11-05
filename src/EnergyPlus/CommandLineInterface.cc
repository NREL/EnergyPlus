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

std::string outputAuditFileName;
std::string outputBndFileName;
std::string outputDxfFileName;
std::string outputEioFileName;
std::string outputEndFileName;
std::string outputErrFileName;
std::string outputEsoFileName;
std::string outputMtdFileName;
std::string outputMddFileName;
std::string outputMtrFileName;
std::string outputRddFileName;
std::string outputShdFileName;
std::string outputTblCsvFileName;
std::string outputTblHtmFileName;
std::string outputTblTabFileName;
std::string outputTblTxtFileName;
std::string outputTblXmlFileName;
std::string inputIdfFileName;
std::string inputIddFileName;
std::string inputWeatherFileName;
std::string outputAdsFileName;
std::string outputDfsFileName;
std::string outputDelightInFileName;
std::string outputDelightOutFileName;
std::string outputDelightEldmpFileName;
std::string outputDelightDfdmpFileName;
std::string outputMapTabFileName;
std::string outputMapCsvFileName;
std::string outputMapTxtFileName;
std::string outputEddFileName;
std::string outputIperrFileName;
std::string outputDbgFileName;
std::string outputSlnFileName;
std::string outputSciFileName;
std::string outputWrlFileName;
std::string outputZszCsvFileName;
std::string outputZszTabFileName;
std::string outputZszTxtFileName;
std::string outputSszCsvFileName;
std::string outputSszTabFileName;
std::string outputSszTxtFileName;
std::string outputScreenCsvFileName;
std::string outputSqlFileName;
std::string outputSqliteErrFileName;
std::string EnergyPlusIniFileName;
std::string inStatFileName;
std::string TarcogIterationsFileName;
std::string eplusADSFileName;
std::string outputCsvFileName;
std::string outputMtrCsvFileName;
std::string outputRvauditFileName;

std::string idfFileNameOnly;
std::string idfDirPathName;
std::string exeDirectory;

bool runReadVars(false);
bool DDOnlySimulation(false);
bool AnnualSimulation(false);

int
ProcessArgs(int argc, const char * argv[])
{
	bool annSimulation;
	bool ddSimulation;

	// Expand long-name options using "=" sign into two arguments
	// and expand multiple short options into separate arguments
	std::vector<std::string> arguments;

	for ( int i = 0; i < argc; ++i ) {

		std::string inputArg( argv[ i ] );

		int doubleDashPosition = inputArg.find("--");
		int equalsPosition = inputArg.find("=");

		if (doubleDashPosition == 0 && equalsPosition != std::string::npos){
			arguments.push_back(inputArg.substr(0,equalsPosition));
			arguments.push_back(inputArg.substr(equalsPosition + 1,inputArg.size() - 1));
		}
		else if (inputArg.substr(0,1) == '-' && inputArg.substr(1,1) != '-' && inputArg.size() > 2){
			for (int c = 1; c < inputArg.size(); ++c){
				arguments.push_back("-" + inputArg.substr(c,1));
			}
		}
		else
			arguments.push_back(inputArg);
	}

	// convert to vector of C strings for option parser
	std::vector<const char *> cStrArgs;
	cStrArgs.reserve(arguments.size());
	for (int i = 0; i < arguments.size(); ++i) {
		cStrArgs.push_back(arguments[i].c_str());
	}

	int argCount = cStrArgs.size();

	bool legacyMode = (argCount == 1);

	// Define options
	ezOptionParser opt;

	opt.overview = VerString;

	opt.syntax = "energyplus [options] [input-file]";

	opt.add("", 0, 0, 0, "Force annual simulation", "-a", "--annual");

	opt.add("", 0, 1, 0, "Output directory path (default: INPUTFILE-output)", "-d", "--output-directory");

	opt.add("", 0, 0, 0, "Force design-day-only simulation", "-D", "--design-day");

	opt.add("", 0, 0, 0, "Display help information", "-h", "--help");

	opt.add("Energy+.idd", 0, 1, 0, "Input data dictionary path (default: Energy+.idd in executable directory)", "-i", "--idd");

	opt.add("", 0, 0, 0, "Run EPMacro", "-m", "--epmacro");

	opt.add("", 0, 1, 0, "Prefix for output file names (default: ep)", "-p", "--output-prefix");

	opt.add("", 0, 0, 0, "Run ReadVarsESO", "-r", "--readvars");

	opt.add("", 0, 0, 0, "Display version information", "-v", "--version");

	opt.add("in.epw", 0, 1, 0, "Weather file path (default: in.epw)", "-w", "--weather");

	opt.add("", 0, 0, 0, "Run ExpandObjects", "-x", "--expandobjects");

	opt.example = "energyplus -w weather.epw -r input.idf";

	std::string errorFollowUp = "Type 'energyplus --help' for usage.";

	// Parse arguments
	opt.parse(argCount, &cStrArgs[0]);

	// print arguments parsed (useful for debugging)
	/*std::string pretty;
	opt.prettyPrint(pretty);
	std::cout << pretty << std::endl;*/

	std::string usage;
	opt.getUsage(usage);

	// Set path of EnergyPlus program path
	exeDirectory = getDirectoryPath(getAbsolutePath(getProgramPath()));

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

	if(opt.lastArgs.size() == 1){
		for(int i=0; i < opt.lastArgs.size(); ++i) {
			std::string arg(opt.lastArgs[i]->c_str());
			inputIdfFileName = arg;
		}
	}
	if(opt.lastArgs.size() == 0)
		inputIdfFileName = "in.idf";

	std::vector<std::string> badOptions;
	if(opt.lastArgs.size() > 1){
		bool invalidOptionFound = false;
		for(int i=0; i < opt.lastArgs.size(); ++i) {
			std::string arg(opt.lastArgs[i]->c_str());
			if (arg.substr(0,1) == "-"){
				invalidOptionFound = true;
				DisplayString("ERROR: Invalid option: " + arg);
			}
		}
		if (invalidOptionFound){
			DisplayString(errorFollowUp);
			exit(EXIT_FAILURE);
		}
		else {
			DisplayString("ERROR: Multiple input files specified:");
			for(int i=0; i < opt.lastArgs.size(); ++i) {
				std::string arg(opt.lastArgs[i]->c_str());
				DisplayString("  Input file #" + std::to_string(i+1) +  ": " + arg);
			}
			DisplayString(errorFollowUp);
			exit(EXIT_FAILURE);
		}
	}

	idfFileNameOnly = removeFileExtension(getFileName(inputIdfFileName));
	idfDirPathName = getDirectoryPath(inputIdfFileName);

	std::string weatherFilePathWithoutExtension = removeFileExtension(inputWeatherFileName);

	bool runExpandObjects(false);
	bool runEPMacro(false);

	runExpandObjects = opt.isSet("-x");

	runEPMacro = opt.isSet("-m");

	if (opt.isSet("-d") ){
		// Add the trailing path character if necessary
		if(dirPathName[dirPathName.size()-1]!=pathChar){
			dirPathName+=pathChar;
		}

		// Create directory if it doesn't already exist
		makeDirectory(dirPathName);
	}
	else if (!legacyMode)
	{
		dirPathName = idfFileNameOnly + "-output" + pathChar;
		// Create directory if it doesn't already exist
		makeDirectory(dirPathName);
	}

	// File naming scheme
	std::string outputFilePrefix;
	if(opt.isSet("-p")) {
		std::string prefixOutName;
		opt.get("-p")->getString(prefixOutName);
		outputFilePrefix = dirPathName + prefixOutName;
	}
	else if (!legacyMode)
		outputFilePrefix = dirPathName + "ep";
	else
		outputFilePrefix = dirPathName + "eplus";


	std::string outputEpmdetFileName;
	std::string outputEpmidfFileName;

	std::string outputExpidfFileName;
	std::string outputExperrFileName;

	if (legacyMode)	{
		// EnergyPlus files
		outputAuditFileName = outputFilePrefix + "out.audit";
		outputBndFileName = outputFilePrefix + "out.bnd";
		outputDxfFileName = outputFilePrefix + "out.dxf";
		outputEioFileName = outputFilePrefix + "out.eio";
		outputEndFileName = outputFilePrefix + "out.end";
		outputErrFileName = outputFilePrefix + "out.err";
		outputEsoFileName = outputFilePrefix + "out.eso";
		outputMtdFileName = outputFilePrefix + "out.mtd";
		outputMddFileName = outputFilePrefix + "out.mdd";
		outputMtrFileName = outputFilePrefix + "out.mtr";
		outputRddFileName = outputFilePrefix + "out.rdd";
		outputShdFileName = outputFilePrefix + "out.shd";
		outputTblCsvFileName = outputFilePrefix + "tbl.csv";
		outputTblHtmFileName = outputFilePrefix + "tbl.htm";
		outputTblTabFileName = outputFilePrefix + "tbl.tab";
		outputTblTxtFileName = outputFilePrefix + "tbl.txt";
		outputTblXmlFileName = outputFilePrefix + "tbl.xml";
		outputAdsFileName = outputFilePrefix + "ADS.out";
		outputDfsFileName = outputFilePrefix + "out.dfs";
		outputDelightInFileName = "eplusout.delightin";
		outputDelightOutFileName = "eplusout.delightout";
		outputDelightEldmpFileName = "eplusout.delighteldmp";
		outputDelightDfdmpFileName = "eplusout.delightdfdmp";
		outputMapTabFileName = outputFilePrefix + "map.tab";
		outputMapCsvFileName = outputFilePrefix + "map.csv";
		outputMapTxtFileName = outputFilePrefix + "map.txt";
		outputEddFileName = outputFilePrefix + "out.edd";
		outputIperrFileName = outputFilePrefix + "out.iperr";
		outputSlnFileName = outputFilePrefix + "out.sln";
		outputSciFileName = outputFilePrefix + "out.sci";
		outputWrlFileName = outputFilePrefix + "out.wrl";
		outputZszCsvFileName = outputFilePrefix + "zsz.csv";
		outputZszTabFileName = outputFilePrefix + "zsz.tab";
		outputZszTxtFileName = outputFilePrefix + "zsz.txt";
		outputSszCsvFileName = outputFilePrefix + "ssz.csv";
		outputSszTabFileName = outputFilePrefix + "ssz.tab";
		outputSszTxtFileName = outputFilePrefix + "ssz.txt";
		outputScreenCsvFileName = outputFilePrefix + "screen.csv";
		outputSqlFileName = outputFilePrefix + "out.sql";
		outputSqliteErrFileName = dirPathName + "sqlite.err";
		outputDbgFileName = outputFilePrefix + "out.dbg";
		EnergyPlusIniFileName = "Energy+.ini";
		inStatFileName = weatherFilePathWithoutExtension + ".stat";
		TarcogIterationsFileName = "TarcogIterations.dbg";
		eplusADSFileName = idfDirPathName+"eplusADS.inp";

		// Readvars files
		outputCsvFileName = outputFilePrefix + "out.csv";
		outputMtrCsvFileName = outputFilePrefix + "mtr.csv";
		outputRvauditFileName = outputFilePrefix + "out.rvaudit";

		// EPMacro files
		outputEpmdetFileName = outputFilePrefix + "out.epmdet";
		outputEpmidfFileName = outputFilePrefix + "out.epmidf";

		// ExpandObjects files
		outputExpidfFileName = outputFilePrefix + "out.expidf";
		outputExperrFileName = outputFilePrefix + "out.experr";
	}
	else {
		// EnergyPlus files
		outputAuditFileName = outputFilePrefix + ".audit";
		outputBndFileName = outputFilePrefix + ".bnd";
		outputDxfFileName = outputFilePrefix + ".dxf";
		outputEioFileName = outputFilePrefix + ".eio";
		outputEndFileName = outputFilePrefix + ".end";
		outputErrFileName = outputFilePrefix + ".err";
		outputEsoFileName = outputFilePrefix + ".eso";
		outputMtdFileName = outputFilePrefix + ".mtd";
		outputMddFileName = outputFilePrefix + ".mdd";
		outputMtrFileName = outputFilePrefix + ".mtr";
		outputRddFileName = outputFilePrefix + ".rdd";
		outputShdFileName = outputFilePrefix + ".shd";
		outputTblCsvFileName = outputFilePrefix + "-table.csv";
		outputTblHtmFileName = outputFilePrefix + "-table.htm";
		outputTblTabFileName = outputFilePrefix + "-table.tab";
		outputTblTxtFileName = outputFilePrefix + "-table.txt";
		outputTblXmlFileName = outputFilePrefix + "-table.xml";
		outputAdsFileName = outputFilePrefix + "-ads.out";
		outputDfsFileName = outputFilePrefix + ".dfs";
		outputDelightInFileName = "eplusout.delightin";
		outputDelightOutFileName = "eplusout.delightout";
		outputDelightEldmpFileName = "eplusout.delighteldmp";
		outputDelightDfdmpFileName = "eplusout.delightdfdmp";
		outputMapTabFileName = outputFilePrefix + "-map.tab";
		outputMapCsvFileName = outputFilePrefix + "-map.csv";
		outputMapTxtFileName = outputFilePrefix + "-map.txt";
		outputEddFileName = outputFilePrefix + ".edd";
		outputIperrFileName = outputFilePrefix + ".iperr";
		outputSlnFileName = outputFilePrefix + ".sln";
		outputSciFileName = outputFilePrefix + ".sci";
		outputWrlFileName = outputFilePrefix + ".wrl";
		outputZszCsvFileName = outputFilePrefix + "-zsz.csv";
		outputZszTabFileName = outputFilePrefix + "-zsz.tab";
		outputZszTxtFileName = outputFilePrefix + "-zsz.txt";
		outputSszCsvFileName = outputFilePrefix + "-ssz.csv";
		outputSszTabFileName = outputFilePrefix + "-ssz.tab";
		outputSszTxtFileName = outputFilePrefix + "-ssz.txt";
		outputScreenCsvFileName = outputFilePrefix + "-screen.csv";
		outputSqlFileName = outputFilePrefix + ".sql";
		outputSqliteErrFileName = outputFilePrefix + "-sqlite.err";
		outputDbgFileName = outputFilePrefix + ".dbg";
		EnergyPlusIniFileName = "Energy+.ini";
		inStatFileName = weatherFilePathWithoutExtension + ".stat";
		TarcogIterationsFileName = "TarcogIterations.dbg";
		eplusADSFileName = idfDirPathName+"eplusADS.inp";

		// Readvars files
		outputCsvFileName = outputFilePrefix + ".csv";
		outputMtrCsvFileName = outputFilePrefix + "Meter.csv";
		outputRvauditFileName = outputFilePrefix + ".rvaudit";

		// EPMacro files
		outputEpmdetFileName = outputFilePrefix + ".epmdet";
		outputEpmidfFileName = outputFilePrefix + ".epmidf";

		// ExpandObjects files
		outputExpidfFileName = outputFilePrefix + ".expidf";
		outputExperrFileName = outputFilePrefix + ".experr";
	}

	// Handle bad options
	if(!opt.gotExpected(badOptions)) {
		for(int i=0; i < badOptions.size(); ++i) {
			DisplayString("ERROR: Unexpected number of arguments for option " + badOptions[i]);
		}
		DisplayString(errorFollowUp);
		exit(EXIT_FAILURE);
	}

	// This is a place holder in case there are required options in the future
	if(!opt.gotRequired(badOptions)) {
		for(int i=0; i < badOptions.size(); ++i) {
			DisplayString("ERROR: Missing required option " + badOptions[i]);
		}
		DisplayString(errorFollowUp);
		exit(EXIT_FAILURE);
	}

	if(opt.firstArgs.size() > 1 || opt.unknownArgs.size() > 0){
		for(int i=1; i < opt.firstArgs.size(); ++i) {
			std::string arg(opt.firstArgs[i]->c_str());
			DisplayString("ERROR: Invalid option: " + arg);
		}
		for(int i=0; i < opt.unknownArgs.size(); ++i) {
			std::string arg(opt.unknownArgs[i]->c_str());
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
			ShowFatalError( "EnergyPlus: Could not open file "+EnergyPlusIniFileName+" for input (read)." );
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
		ShowFatalError( "EnergyPlus: Could not open output debug file: " + outputDbgFileName + "." );
	}

	// Preprocessors (These will likely move to a new file)
	if(runEPMacro){
		std::string epMacroPath = exeDirectory + "EPMacro";
		{ IOFlags flags; gio::inquire( epMacroPath, flags ); FileExists = flags.exists(); }
		if (!FileExists){
			DisplayString("ERROR: Could not find EPMacro executable: " + getAbsolutePath(epMacroPath) + "." );
			exit(EXIT_FAILURE);
		}
		std::string epMacroCommand = "\"" + epMacroPath + "\"";
		bool inputFileNamedIn =
				(getAbsolutePath(inputIdfFileName) == getAbsolutePath("in.imf"));

		if (!inputFileNamedIn)
			linkFile(inputIdfFileName.c_str(), "in.imf");
		DisplayString("Running EPMacro...");
		systemCall(epMacroCommand);
		if (!inputFileNamedIn)
			removeFile("in.imf");
		moveFile("audit.out",outputEpmdetFileName);
		moveFile("out.idf",outputEpmidfFileName);
	    inputIdfFileName = outputEpmidfFileName;
	}

	if(runExpandObjects) {
		std::string expandObjectsPath = exeDirectory + "ExpandObjects";
		{ IOFlags flags; gio::inquire( expandObjectsPath, flags ); FileExists = flags.exists(); }
		if (!FileExists){
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
		if (FileExists){
			moveFile("expanded.idf", outputExpidfFileName);
		    inputIdfFileName = outputExpidfFileName;
		}
	}


	return 0;
}

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
	static gio::Fmt const Format_700( "(A)" );

	DataOut = "           ";

	// I tried ADJUSTL(TRIM(KindofParameter)) and got an internal compiler error

	Param = KindofParameter;
	strip( Param );
	ILEN = len( Param );
	gio::rewind( UnitNumber );
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

} //CommandLineInterface namespace
} //EnergyPlus namespace


