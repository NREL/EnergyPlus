//Standard C++ library
#include <sys/types.h>
#include <sys/stat.h>
#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <mach-o/dyld.h>

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

bool
fileExist(const std::string& filename)
{
	std::ifstream infile(filename);
	return infile.good();
}

std::string
returnFileName( std::string const& filepath )
{
	int pathCharPosition = filepath.find_last_of(pathChar);
	return filepath.substr(pathCharPosition + 1, filepath.size() - 1);
}

std::string
returnDirPath( std::string const& filepath )
{
	int pathCharPosition = filepath.find_last_of(pathChar);
	return filepath.substr(0, pathCharPosition + 1);
}

std::string
returnAbsolutePath( std::string const& filepath )
{
	char absolutePath[1024];
	realpath(filepath.c_str(), absolutePath);
	return std::string(absolutePath);
}

std::string
returnProgramPath()
{
	char executableRelativePath[1024];
	uint32_t pathSize = sizeof(executableRelativePath);
	_NSGetExecutablePath(executableRelativePath, &pathSize);

	return std::string(executableRelativePath);
}


// Not currently used
std::string
returnFileExtension(const std::string& filename){
	int extensionPosition = filename.find_last_of(".");
	return filename.substr(extensionPosition + 1, filename.size() - 1);
}

std::string
removeFileExtension(const std::string& filename){
	int extensionPosition = filename.find_last_of(".");
	return filename.substr(0, extensionPosition);
}

int
mkpath(std::string s,mode_t mode)
{
	size_t pre=0,pos;
	std::string dir;
	int mdret;

	if(s[s.size()-1]!=pathChar){
		s+=pathChar;
	}

	while((pos=s.find_first_of(pathChar,pre))!=std::string::npos){
		dir=s.substr(0,pos++);
		pre=pos;
		if(dir.size()==0) continue; // if leading / first time is 0 length
		if((mdret=mkdir(dir.c_str(),mode)) && errno!=EEXIST){
			return mdret;
		}
	}
	return mdret;
}

std::string EqualsToSpace(std::string text)
{
	std::replace(text.begin(), text.end(), '=', ' ');
	return text;
}

int
ProcessArgs(int argc, const char * argv[])
{
	bool annSimulation;
	bool ddSimulation;

	// Expand long-name options using "=" sign into two arguments
	std::vector<std::string> arguments;
	int wildCardPosition = 2;

	for ( int i = 0; i < argc; ++i ) {

		std::string inputArg( argv[ i ] );

		int beginningPosition = inputArg.find("--");
		int endingPosition = inputArg.find("=");

		if (beginningPosition == 0 && endingPosition != std::string::npos){
			arguments.push_back(inputArg.substr(0,endingPosition));
			arguments.push_back(inputArg.substr(endingPosition + 1,inputArg.size() - 1));
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

	opt.syntax = "energyplus [options] [input file]";

	opt.add("", 0, 0, 0, "Force annual simulation", "-a", "--annual");

	opt.add("", 0, 1, 0, "Output directory path (default: current working directory)", "-d", "--output-directory");

	opt.add("", 0, 0, 0, "Force design-day-only simulation", "-D", "--design-day");

	opt.add("", 0, 0, 0, "Display help information", "-h", "--help");

	opt.add("Energy+.idd", 0, 1, 0, "Input data dictionary path (default: Energy+.idd in executable directory)", "-i", "--idd");

	opt.add("", 0, 0, 0, "Run EPMacro", "-m", "--epmacro");

	opt.add("", 0, 1, 0, "Prefix for output file names (default: same as input file name)", "-p", "--output-prefix");

	opt.add("", 0, 0, 0, "Run ReadVarsESO", "-r", "--readvars");

	opt.add("", 0, 0, 0, "Display version information", "-v", "--version");

	opt.add("in.epw", 0, 1, 0, "Weather file path (default: in.epw)", "-w", "--weather");

	opt.add("", 0, 0, 0, "Run ExpandObjects", "-x", "--expandobjects");

	opt.example = "energyplus -w weather.epw -d output -r input.idf";

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
	exeDirectory = returnDirPath(returnAbsolutePath(returnProgramPath()));

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
	if(inputIdfFileName.empty())
		inputIdfFileName = "in.idf";

	if(opt.lastArgs.size() > 1){
		DisplayString("ERROR: Multiple input files specified:");
		for(int i=0; i < opt.lastArgs.size(); ++i) {
			std::string arg(opt.lastArgs[i]->c_str());
			DisplayString("  Input file #" + std::to_string(i+1) +  ": " + arg);
		}
		DisplayString(errorFollowUp);
		exit(EXIT_FAILURE);
	}

	if(opt.lastArgs.size() == 0 && !legacyMode){
		DisplayString("ERROR: No input file provided.");
		DisplayString(errorFollowUp);
		exit(EXIT_FAILURE);
	}
	idfFileNameOnly = removeFileExtension(returnFileName(inputIdfFileName));
	idfDirPathName = returnDirPath(inputIdfFileName);

	std::string weatherFilePathWithoutExtension = removeFileExtension(inputWeatherFileName);

	bool runExpandObjects(false);
	bool runEPMacro(false);

	runExpandObjects = opt.isSet("-x");

	runEPMacro = opt.isSet("-m");

	if (opt.isSet("-d") ){
		struct stat sb = {0};

		if (stat(dirPathName.c_str(), &sb) == -1) {
			int mkdirretval;
			mkdirretval=mkpath(dirPathName,0755);
		}

		if(dirPathName[dirPathName.size()-1]!=pathChar){
			dirPathName+=pathChar;
		}
	}

	std::string outputFilePrefix;
	if(opt.isSet("-p")) {
		std::string prefixOutName;
		opt.get("-p")->getString(prefixOutName);
		outputFilePrefix = dirPathName + prefixOutName;
	}
	else if (!legacyMode)
		outputFilePrefix = dirPathName + idfFileNameOnly;
	else
		outputFilePrefix = dirPathName + "eplus";

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
		outputTblCsvFileName = outputFilePrefix + "Table.csv";
		outputTblHtmFileName = outputFilePrefix + "Table.htm";
		outputTblTabFileName = outputFilePrefix + "Table.tab";
		outputTblTxtFileName = outputFilePrefix + "Table.txt";
		outputTblXmlFileName = outputFilePrefix + "Table.xml";
		outputAdsFileName = outputFilePrefix + "ADS.out";
		outputDfsFileName = outputFilePrefix + ".dfs";
		outputDelightInFileName = "eplusout.delightin";
		outputDelightOutFileName = "eplusout.delightout";
		outputDelightEldmpFileName = "eplusout.delighteldmp";
		outputDelightDfdmpFileName = "eplusout.delightdfdmp";
		outputMapTabFileName = outputFilePrefix + "Map.tab";
		outputMapCsvFileName = outputFilePrefix + "Map.csv";
		outputMapTxtFileName = outputFilePrefix + "Map.txt";
		outputEddFileName = outputFilePrefix + ".edd";
		outputIperrFileName = outputFilePrefix + ".iperr";
		outputSlnFileName = outputFilePrefix + ".sln";
		outputSciFileName = outputFilePrefix + ".sci";
		outputWrlFileName = outputFilePrefix + ".wrl";
		outputZszCsvFileName = outputFilePrefix + "Zsz.csv";
		outputZszTabFileName = outputFilePrefix + "Zsz.tab";
		outputZszTxtFileName = outputFilePrefix + "Zsz.txt";
		outputSszCsvFileName = outputFilePrefix + "Ssz.csv";
		outputSszTabFileName = outputFilePrefix + "Ssz.tab";
		outputSszTxtFileName = outputFilePrefix + "Ssz.txt";
		outputScreenCsvFileName = outputFilePrefix + "Screen.csv";
		outputSqlFileName = outputFilePrefix + ".sql";
		outputSqliteErrFileName = outputFilePrefix + "SQLite.err";
		outputDbgFileName = outputFilePrefix + ".dbg";
		EnergyPlusIniFileName = "Energy+.ini";
		inStatFileName = weatherFilePathWithoutExtension + ".stat";
		TarcogIterationsFileName = "TarcogIterations.dbg";
		eplusADSFileName = idfDirPathName+"eplusADS.inp";

		// Readvars files
		outputCsvFileName = outputFilePrefix + ".csv";
		outputMtrCsvFileName = outputFilePrefix + "Meter.csv";
		outputRvauditFileName = outputFilePrefix + ".rvaudit";
	}

	// Handle bad options
	std::vector<std::string> badOptions;
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
		ShowFatalError( "EnergyPlus: Could not find input data dictionary: " + returnAbsolutePath(inputIddFileName) + "." );
	}

	{ IOFlags flags; gio::inquire( inputIdfFileName, flags ); FileExists = flags.exists(); }
	if ( ! FileExists ) {
		ShowFatalError( "EnergyPlus: Could not find input data file: " + returnAbsolutePath(inputIdfFileName) + "." );
	}

	{ IOFlags flags; gio::inquire( inputWeatherFileName, flags ); FileExists = flags.exists(); }
	if ( ! FileExists ) {
		ShowFatalError( "EnergyPlus: Could not find weather file: " + returnAbsolutePath(inputWeatherFileName) + "." );
	}

	OutputFileDebug = GetNewUnitNumber();
	{ IOFlags flags; flags.ACTION( "write" ); gio::open( OutputFileDebug, outputDbgFileName, flags ); iostatus = flags.ios(); }
	if ( iostatus != 0 ) {
		ShowFatalError( "EnergyPlus: Could not open output debug file: " + outputDbgFileName + "." );
	}

	// Preprocessors (These will likely move to a new file)
	bool inputFileNamedIn = (idfFileNameOnly == "in");

	if(runEPMacro){
		if (!inputFileNamedIn)
			symlink(inputIdfFileName.c_str(), "in.imf");
		std::string runEPMacro = exeDirectory + "EPMacro";
		DisplayString("Running EPMacro...");
		system(runEPMacro.c_str());
		if (!inputFileNamedIn)
			remove("in.imf");
		std::string outputEpmdetFileName = outputFilePrefix + "out.epmdet";
		rename("audit.out",outputEpmdetFileName.c_str());
		std::string outputEpmidfFileName = outputFilePrefix + "out.epmidf";
		rename("out.idf",outputEpmidfFileName.c_str());
	    inputIdfFileName = outputEpmidfFileName;
	}

	if(runExpandObjects) {
		std::string runExpandObjects = exeDirectory + "ExpandObjects";
		if (!inputFileNamedIn)
			symlink(inputIdfFileName.c_str(), "in.idf");
		system(runExpandObjects.c_str());
		if (!inputFileNamedIn)
			remove("in.idf");
		remove("expandedidf.err");
		std::string outputExpidfFileName = outputFilePrefix + "out.expidf";
	    rename("expanded.idf", outputExpidfFileName.c_str());
	    inputIdfFileName = outputExpidfFileName;
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


