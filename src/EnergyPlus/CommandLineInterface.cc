//Standard C++ library
#include <sys/types.h>
#include <sys/stat.h>
#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

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
std::string outputFilePrefix;
std::string dirPathName;
std::string idfFileNameOnly;
std::string exePathName;
std::string prefixOutName;
std::string inputIMFFileName;

bool readVarsValue(false);
bool prefixValue(false);
bool expandObjValue(false);
bool EPMacroValue(false);
bool DDOnlySimulation(false);
bool AnnualSimulation(false);

bool fileExist(const std::string& filename)
{
	std::ifstream infile(filename);
	return infile.good();
}

std::string
returnFileName( std::string const& filename )
{
	return {std::find_if(filename.rbegin(), filename.rend(),
			[](char c) { return c == pathChar; }).base(),
			filename.end()};
}

std::string
returnDirPathName( std::string const& filename )
{
	std::string::const_reverse_iterator pivot = std::find( filename.rbegin(), filename.rend(), pathChar );
	return pivot == filename.rend()
			? filename
					: std::string( filename.begin(), pivot.base() - 1 );
}

std::string
returnFileExtension(const std::string& filename){
	std::string ext = "";

	for(int i=0; i<filename.length(); i++){
		if(filename[i] == '.' || filename[i] == '=' || filename[i] == ' '){
			for(int j = i+1; j<filename.length(); j++){
				ext += filename[j];
			}
			return ext;
		}
	}
	return ext;
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
	std::ifstream inputW, inputIDD;
	std::istream* instreamW;
	std::istream* instreamIDD;
	bool annSimulation;
	bool ddSimulation;

	for ( int i = 1; i < argc; ++i ) {
		std::string inputArg( argv[ i ] );
		if(inputArg.substr(0,10) == "--weather="){
			std::string weatherLongOption = EqualsToSpace(inputArg.substr(0,10));
			argv[i] = weatherLongOption.c_str();
			inputWeatherFileName = returnFileExtension(inputArg);
		}

		if(inputArg.substr(0,6) == "--idd="){
			std::string iddLongOption = EqualsToSpace(inputArg.substr(0,6));
			argv[i] = iddLongOption.c_str();
			inputIddFileName = returnFileExtension(inputArg);
		}

		if(inputArg == "-a")
			annSimulation = true;

		if(inputArg == "-dd")
			ddSimulation = true;
	}

	ezOptionParser opt;

	opt.overview = VerString;
	opt.example = "energyplus -i custom.idd -w weather.epw -d output/ -p prefix -r input.idf";

	opt.syntax = "energyplus [options] [input file]";

	opt.add("", 0, 0, 0, "Display this message", "-h", "--help");

	opt.add("", 0, 0, 0, "Display version information", "-v", "--version");

	opt.add("in.epw", 0, 1, 0, "Weather file path (default in.epw)", "-w", "--weather");

	opt.add("", 0, 0, 0, "Weather file path", "--weather ");

	opt.add("Energy+.idd", 0, 1, 0, "Input data dictionary path (default Energy+.idd in executable directory)", "-i", "--idd");

	opt.add("", 0, 0, 0, "Input data dictionary path", "--idd ");

	opt.add("", 0, 1, 0, "Prefix for output files (default same as input file name)", "-p", "--prefix");

	opt.add("", 0, 0, 0, "Run ReadVarsESO to generate time-series CSV", "-r", "--readvars");

	opt.add("", 0, 0, 0, "Run ExpandObjects", "-e", "--expandObj");

	opt.add("", 0, 0, 0, "Force design-day-only simulation", "-dd", "--designday");

	opt.add("", 0, 0, 0, "Force design-day-only simulation", "-a", "--annual");

	opt.add("", 0, 1, 0, "Run EPMacro", "-ep", "--epMacro");

	opt.add("", 0, 1, 0, "Output directory (default current working directory)", "-d", "--dir");

	// Parse arguments
	opt.parse(argc, argv);

	// print arguments parsed (useful for debugging)
	//std::string pretty;
	//opt.prettyPrint(pretty);
	//std::cout << pretty << std::endl;

	std::string usage, idfFileNameWextn, idfDirPathName;
	std::string weatherFileNameWextn, weatherDirPathName;
	opt.getUsage(usage);

	//To check the path of EnergyPlus
	char executable_path[100];
	realpath(argv[0], executable_path);
	std::string exePath = std::string(executable_path);
	exePathName = returnDirPathName(exePath);

	if(inputWeatherFileName.empty())
		inputWeatherFileName = "in.epw";

	instreamW = &inputW;
	inputW.open( inputWeatherFileName.c_str() );

	if(inputIddFileName.empty())
		inputIddFileName = "Energy+.idd";

	instreamIDD = &inputIDD;
	inputIDD.open( inputIddFileName.c_str() );

	opt.get("-d")->getString(dirPathName);

	opt.get("-ep")->getString(inputIMFFileName);

	if (opt.isSet("-r"))
		readVarsValue = true;


	if (opt.isSet("-dd"))
			DDOnlySimulation = true;

	if(!(annSimulation && ddSimulation)){
		if (opt.isSet("-a"))
			AnnualSimulation = true;
	}

	// Process standard arguments
	if (opt.isSet("-h")) {
		DisplayString(usage);
		exit(EXIT_SUCCESS);
	}

	if (opt.isSet("-v")) {
		DisplayString(VerString);
		exit(EXIT_SUCCESS);
	}

	if(opt.lastArgs.size() > 0 && opt.lastArgs.size() < 2){
		for(int i=0; i < opt.lastArgs.size(); ++i) {
			std::string arg(opt.lastArgs[i]->c_str());
			inputIdfFileName = arg;

			struct stat s;
			if( stat(arg.c_str(),&s) != 0 )	{
				char resolved_path[100];
				realpath(arg.c_str(), resolved_path);
				std::string idfPath = std::string(resolved_path);
				DisplayString("ERROR: "+ idfPath +" is not a valid path. \n" );
				exit(EXIT_FAILURE);
			}
		}
	}

	if(opt.lastArgs.size() > 1){
		for(int i=1; i < opt.lastArgs.size(); ++i) {
			std::string arg(opt.lastArgs[i]->c_str());
			DisplayString("ERROR: Invalid option last arg: " + arg + "\n");
			DisplayString(usage);
		}
		exit(EXIT_FAILURE);
	}

	if(inputIdfFileName.empty())
		inputIdfFileName = "in.idf";

	idfFileNameWextn = returnFileName(inputIdfFileName);

	idfFileNameOnly = idfFileNameWextn.substr(0,idfFileNameWextn.size()-4);
	idfDirPathName = returnDirPathName(inputIdfFileName);

	opt.get("-p")->getString(prefixOutName);

	if(opt.isSet("-p"))
		prefixValue = true;

	if(opt.isSet("-e"))
		expandObjValue = true;

	if(opt.isSet("-ep"))
		EPMacroValue = true;

	if (opt.isSet("-d") ){
		struct stat sb = {0};

		if (stat(dirPathName.c_str(), &sb) == -1) {
			int mkdirretval;
			mkdirretval=mkpath(dirPathName,0755);
		}

		if(dirPathName[dirPathName.size()-1]!=pathChar){
			dirPathName+=pathChar;
		}

		if(prefixValue)
			outputFilePrefix = dirPathName + prefixOutName + "_";
		else
			outputFilePrefix = dirPathName + idfFileNameOnly + "_";

	}
	else
		outputFilePrefix = "eplus";

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
			DisplayString("\nERROR: Invalid option first arg: " + arg + "\n");
			DisplayString(usage);
			ShowFatalError("\nERROR: Invalid option first arg: " + arg + "\n");
		}
		for(int i=0; i < opt.unknownArgs.size(); ++i) {
			std::string arg(opt.unknownArgs[i]->c_str());
			DisplayString("ERROR: Invalid option unknown arg: " + arg + "\n");
			DisplayString(usage);
			ShowFatalError("ERROR: Invalid option unknown arg: " + arg + "\n");
		}
		exit(EXIT_FAILURE);
	}

	if(expandObjValue) {
		std::string runExpandObjects = "./ExpandObjects";
		system(runExpandObjects.c_str());

		std::string expandIdfFile = "expanded.idf";
		bool expandIdfFileExist = fileExist(expandIdfFile);
		if(expandIdfFileExist) {
			std::string expidfFileName = outputFilePrefix + ".expidf";
			std::ifstream  src(expandIdfFile.c_str());
			std::ofstream  dst(expidfFileName.c_str());
			dst << src.rdbuf();
			remove(inputIdfFileName.c_str());
			symlink(expidfFileName.c_str(), inputIdfFileName.c_str());
		}
		//else
			// DisplayString("ExpandObjects file does not exist. \n");
	}

	if(EPMacroValue){
		bool imfFileExist = fileExist(inputIMFFileName);
		std::string defaultIMFFileName = "in.imf";

		if(imfFileExist){
			symlink(inputIMFFileName.c_str(), defaultIMFFileName.c_str());

			std::string outIDFFileName = "out.idf";
			remove(outIDFFileName.c_str());
			std::string outAuditFileName = "audit.out";
			remove(outAuditFileName.c_str());
			std::string epmdetFileName = outputFilePrefix+".epmdet";
			remove(epmdetFileName.c_str());
			std::string epmidfFileName = outputFilePrefix+".epmidf";
			remove(epmidfFileName.c_str());

			std::string runEPMacro = "./EPMacro";

			system(runEPMacro.c_str());
			std::cout<<"Running EPMacro...\n";

			bool AuditFileExist = fileExist(outAuditFileName);
			if(AuditFileExist){
				std::string epmdetFileName = outputFilePrefix + ".epmdet";
				std::ifstream  src(outAuditFileName.c_str());
				std::ofstream  dst(epmdetFileName.c_str());
				dst << src.rdbuf();
			}

			bool outFileExist = fileExist(outIDFFileName);
			if(outFileExist){
				std::string epmidfFileName = outputFilePrefix + ".epmidf";
				std::ifstream  src(outIDFFileName.c_str());
				std::ofstream  dst(epmidfFileName.c_str());
				dst << src.rdbuf();
				symlink(epmidfFileName.c_str(), inputIdfFileName.c_str());
				DisplayString("Input file: " + inputIMFFileName + "\n");
			}
			else{
				ShowFatalError("EPMacro did not produce "+ outIDFFileName + "with "+ inputIMFFileName +"\n");
				exit(EXIT_FAILURE);
			}
		}
	}
	return 0;
}
} //CommandLineInterface namespace
} //EnergyPlus namespace


