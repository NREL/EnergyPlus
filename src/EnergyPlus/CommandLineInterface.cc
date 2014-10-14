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

// Project headers
#include <CommandLineInterface.hh>
#include <DisplayRoutines.hh>
#include <DataStringGlobals.hh>
#include <DataSystemVariables.hh>
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
std::string outputDelightFileName;
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
std::string EnergyPlusIniFileName;
std::string inStatFileName;
std::string TarcogIterationsFileName;
std::string eplusADSFileName;
std::string outputCsvFileName;
std::string outputMtrCsvFileName;
std::string outputRvauditFileName;

std::string idfFileNameOnly;
std::string exePathName;

bool runReadVars(false);
bool DDOnlySimulation(false);
bool AnnualSimulation(false);
bool iddArgSet(false);

bool fileExist(const std::string& filename)
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

	ezOptionParser opt;

	opt.overview = VerString;
	opt.example = "energyplus -i custom.idd -w weather.epw -d output/ -p prefix -r input.idf\n";

	opt.syntax = "energyplus [options] [input file]";

	opt.add("", 0, 0, 0, "Display this message", "-h", "--help");

	opt.add("", 0, 0, 0, "Display version information", "-v", "--version");

	opt.add("in.epw", 0, 1, 0, "Weather file path (default in.epw)", "-w", "--weather");

	opt.add("Energy+.idd", 0, 1, 0, "Input data dictionary path (default Energy+.idd in executable directory)", "-i", "--idd");

	opt.add("", 0, 1, 0, "Prefix for output files (default same as input file name)", "-p", "--prefix");

	opt.add("", 0, 0, 0, "Run ReadVarsESO to generate time-series CSV", "-r", "--readvars");

	opt.add("", 0, 0, 0, "Run ExpandObjects", "-x", "--expand");

	opt.add("", 0, 0, 0, "Force design-day-only simulation", "-D", "--designday");

	opt.add("", 0, 0, 0, "Force annual simulation", "-a", "--annual");

	opt.add("", 0, 0, 0, "Run EPMacro", "-m", "--epmacro");

	opt.add("", 0, 1, 0, "Output directory (default current working directory)", "-d", "--dir");

	// Parse arguments
	opt.parse(cStrArgs.size(), &cStrArgs[0]);

	// print arguments parsed (useful for debugging)
	//std::string pretty;
	//opt.prettyPrint(pretty);
	//std::cout << pretty << std::endl;

	std::string usage, idfDirPathName;
	opt.getUsage(usage);

	//To check the path of EnergyPlus
	char executableAbsolutePath[1024];
	char executableRelativePath[1024];
	uint32_t pathSize = sizeof(executableRelativePath);
	_NSGetExecutablePath(executableRelativePath, &pathSize);
	realpath(executableRelativePath, executableAbsolutePath);
	exePathName = returnDirPath(std::string(executableAbsolutePath));

	opt.get("-w")->getString(inputWeatherFileName);

	opt.get("-i")->getString(inputIddFileName);

	if (opt.isSet("-i"))
		iddArgSet = true;
	else
		inputIddFileName = exePathName + inputIddFileName;

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

			struct stat s;
			if( stat(arg.c_str(),&s) != 0 )	{
				char resolved_path[100];
				realpath(arg.c_str(), resolved_path);
				std::string idfPath = std::string(resolved_path);
				DisplayString("ERROR: "+ idfPath +" is not a valid path.\n" );
				exit(EXIT_FAILURE);
			}
		}
	}

	if(opt.lastArgs.size() > 1){
		DisplayString("ERROR: Multiple input files specified:");
		for(int i=0; i < opt.lastArgs.size(); ++i) {
			std::string arg(opt.lastArgs[i]->c_str());
			DisplayString("  Input file #" + std::to_string(i+1) +  ": " + arg);
		}
		DisplayString(usage);
		exit(EXIT_FAILURE);
	}

	if(inputIdfFileName.empty())
		inputIdfFileName = "in.idf";

	idfFileNameOnly = removeFileExtension(returnFileName(inputIdfFileName));
	idfDirPathName = returnDirPath(inputIdfFileName);

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
		outputFilePrefix = dirPathName + prefixOutName + "-";
	}
	else if (argc > 1)
		outputFilePrefix = dirPathName + idfFileNameOnly + "-";
	else
		outputFilePrefix = dirPathName + "eplus";

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
	outputDelightFileName = outputFilePrefix + "out.delightdfdmp";
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
	outputDbgFileName = outputFilePrefix + ".dbg";
	EnergyPlusIniFileName = "Energy+.ini";
	inStatFileName = "in.stat";
	TarcogIterationsFileName = "TarcogIterations.dbg";
	eplusADSFileName = idfDirPathName+"eplusADS.inp";

	// Readvars files
	outputCsvFileName = outputFilePrefix + "out.csv";
	outputMtrCsvFileName = outputFilePrefix + "mtr.csv";
	outputRvauditFileName = outputFilePrefix + "out.rvaudit";

	// Handle bad options
	std::vector<std::string> badOptions;
	if(!opt.gotExpected(badOptions)) {
		for(int i=0; i < badOptions.size(); ++i) {
			DisplayString("\nERROR: Unexpected number of arguments for option " + badOptions[i] + "\n");
			DisplayString(usage);
			ShowFatalError("\nERROR: Unexpected number of arguments for option " + badOptions[i] + "\n");
			exit(EXIT_FAILURE);
		}
	}

	if(!opt.gotRequired(badOptions)) {
		for(int i=0; i < badOptions.size(); ++i) {
			DisplayString("\nERROR: Missing required option " + badOptions[i] + "\n");
			DisplayString(usage);
			ShowFatalError("\nERROR: Missing required option " + badOptions[i] + "\n");
			exit(EXIT_FAILURE);
		}
	}

	if(opt.firstArgs.size() > 1 || opt.unknownArgs.size() > 0){
		for(int i=1; i < opt.firstArgs.size(); ++i) {
			std::string arg(opt.firstArgs[i]->c_str());
			DisplayString("\nERROR: Invalid option: " + arg + "\n");
			DisplayString(usage);
			ShowFatalError("\nERROR: Invalid option: " + arg + "\n");
		}
		for(int i=0; i < opt.unknownArgs.size(); ++i) {
			std::string arg(opt.unknownArgs[i]->c_str());
			DisplayString("ERROR: Invalid option: " + arg + "\n");
			DisplayString(usage);
			ShowFatalError("ERROR: Invalid option: " + arg + "\n");
		}
		exit(EXIT_FAILURE);
	}

	// Error for cases where both design-day and annual simulation switches are set
	if (DDOnlySimulation && AnnualSimulation) {
		ShowFatalError("ERROR: Cannot force both design-day and annual simulations. Set either '-D' or '-a', but not both.\n");
	}

	bool inputFileNamedIn = (idfFileNameOnly == "in");

	if(runEPMacro){
		if (!inputFileNamedIn)
			symlink(inputIdfFileName.c_str(), "in.imf");
		std::string runEPMacro = exePathName + "EPMacro";
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
		std::string runExpandObjects = exePathName + "ExpandObjects";
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
} //CommandLineInterface namespace
} //EnergyPlus namespace


