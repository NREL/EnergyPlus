//Standard C++ library
#include <iostream>
#include <stdio.h>
#include <stdlib.h>

// CLI Headers
#include <ezOptionParser.hpp>

// Project headers
#include <CommandLineInterface.hh>
#include <DisplayRoutines.hh>
#include <DataStringGlobals.hh>
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
	using namespace InputProcessor;
	using namespace SimulationManager;
	using namespace OutputReportTabular;
	using namespace OutputProcessor;
	using namespace SolarShading;
	using namespace ez;

	std::string outputAuditFile;
	std::string outputBndFile;
	std::string outputDxfFile;
	std::string outputEioFile;
	std::string outputEndFile;
	std::string outputErrFile;
	std::string outputEsoFile;
	std::string outputMtdFile;
	std::string outputMddFile;
	std::string outputMtrFile;
	std::string outputRddFile;
	std::string outputShdFile;
	std::string outputCsvFile;
	std::string outputHtmFile;
	std::string outputTabFile;
	std::string outputTxtFile;
	std::string outputXmlFile;
	std::string inputFileName;
	std::string inputEnergyFile;
	std::string inputWeatherFile;

	int
	ProcessArgs(int argc, const char * argv[])
	{
		ezOptionParser opt;

		opt.overview = VerString;
		opt.example = "EnergyPlus -i InputFile.idf -e Energy+.idd -w WeatherFile.epw -o";

		opt.syntax = "EnergyPlus [options]";

		opt.add("", 0, 0, 0, "Display this message", "-h", "--help");

		opt.add("", 0, 0, 0, "Print version invormation", "-v", "--version");

		opt.add("in.idf", 0, 1, 0, "Input Definition File (IDF) file path (default \".\\in.idf\")", "-i", "--idf");

		opt.add("in.epw", 0, 1, 0, "EnergyPlus Weather (EPW) file path (default \".\\in.epw\")", "-w", "--weather");

		opt.add("Energy+.idd", 0, 1, 0, "Input Data Dictionary (IDD) file path (default \".\\Energy+.idd\")", "-e", "--idd");

		opt.add("", 0, 0, 0, "Rename output files to using the IDF and EPW file names", "-o", "--output");

		// Parse arguments
		opt.parse(argc, argv);

		// print arguments parsed (useful for debugging)
		/*
		std::string pretty;

		opt.prettyPrint(pretty);

		std::cout << pretty << std::endl;
		*/

		std::string usage;
		opt.getUsage(usage);

		// Handle bad options
		std::vector<std::string> badOptions;

		if(!opt.gotExpected(badOptions)) {
			for(int i=0; i < badOptions.size(); ++i) {
				DisplayString("ERROR: Unexpected number of arguments for option " + badOptions[i] + "\n");
			}
			DisplayString(usage);
			exit(EXIT_FAILURE);
		}

		if(opt.firstArgs.size() > 1 || opt.unknownArgs.size() > 0 || opt.lastArgs.size() > 0){
			for(int i=1; i < opt.firstArgs.size(); ++i) {
				std::string arg(opt.firstArgs[i]->c_str());
				DisplayString("ERROR: Invalid option: " + arg + "\n");
			}
			for(int i=0; i < opt.unknownArgs.size(); ++i) {
				std::string arg(opt.unknownArgs[i]->c_str());
				DisplayString("ERROR: Invalid option: " + arg + "\n");
			}
			for(int i=0; i < opt.lastArgs.size(); ++i) {
				std::string arg(opt.lastArgs[i]->c_str());
				DisplayString("ERROR: Invalid option: " + arg + "\n");
			}
			DisplayString(usage);
			exit(EXIT_FAILURE);
		}

		if(!opt.gotRequired(badOptions)) {
			for(int i=0; i < badOptions.size(); ++i) {
				DisplayString("ERROR: Missing required option " + badOptions[i] + "\n");
			}
			DisplayString(usage);
			exit(EXIT_FAILURE);
		}

		// Process arguments
		if (opt.isSet("-h")) {
			DisplayString(usage);
			exit(EXIT_SUCCESS);
		}

		if (opt.isSet("-v")) {
			DisplayString(VerString);
			exit(EXIT_SUCCESS);
		}

		opt.get("-i")->getString(inputFileName);

		opt.get("-w")->getString(inputWeatherFile);

		opt.get("-e")->getString(inputEnergyFile);

		std::string outputFilePrefix;

		if (opt.isSet("-o")) {
			outputFilePrefix =
					inputFileName.substr(0, inputFileName.size()-4) + "_" +
					inputWeatherFile.substr(0, inputWeatherFile.size()-4) + "_";
		}
		else {
			outputFilePrefix = "eplus";
		}

		outputAuditFile = outputFilePrefix + "out.audit";
		outputBndFile = outputFilePrefix + "out.bnd";
		outputDxfFile = outputFilePrefix + "out.dxf";
		outputEioFile = outputFilePrefix + "out.eio";
		outputEndFile = outputFilePrefix + "out.end";
		outputErrFile = outputFilePrefix + "out.err";
		outputEsoFile = outputFilePrefix + "out.eso";
		outputMtdFile = outputFilePrefix + "out.mtd";
		outputMddFile = outputFilePrefix + "out.mdd";
		outputMtrFile = outputFilePrefix + "out.mtr";
		outputRddFile = outputFilePrefix + "out.rdd";
		outputShdFile = outputFilePrefix + "out.shd";
		outputCsvFile = outputFilePrefix + "tbl.csv";
		outputHtmFile = outputFilePrefix + "tbl.htm";
		outputTabFile = outputFilePrefix + "tbl.tab";
		outputTxtFile = outputFilePrefix + "tbl.txt";
		outputXmlFile = outputFilePrefix + "tbl.xml";

		return 0;
	}
} //namespace options
} //EnergyPlus namespace
