
// CLI Headers
#include <ezOptionParser.hpp>
#include <stdio.h>

// Project headers
#include <CommandLineInterface.hh>
#include <DisplayRoutines.hh>
#include <DataStringGlobals.hh>
#include <EnergyPlus.hh>
#include <InputProcessor.hh>
#include <OutputProcessor.hh>
#include <OutputReportTabular.hh>
#include <SimulationManager.hh>
#include <UtilityRoutines.hh>
using namespace ez;

namespace EnergyPlus{

namespace options{

using namespace DataStringGlobals;
using namespace InputProcessor;
using namespace SimulationManager;
using namespace OutputReportTabular;


void Usage(ezOptionParser& opt) {
	std::string usage;
	opt.getUsage(usage);
	DisplayString(usage);
};

int
ProcessArgs(int argc, const char * argv[])
{
	 ezOptionParser opt;

	    opt.overview =  "*******************************************\n";
	    opt.overview += "Copyright (C) 1996-2014 \n"; // Add owners of copyright?
	    opt.overview += "Web: www.energyplus.gov\n";
	    opt.overview += VerString;
	    opt.overview += "\n*******************************************\n";
	    opt.syntax = "./EnergyPlus -i InputFile.idf -e Energy+.idd -w WeatherFile.epw -o OutFile.csv";

	    opt.add(
	            "", // Default.
	            0, // Required?
	            1, // Number of args expected.
	            0, // Delimiter if expecting multiple args.
	            "Usage information displayed on top", // Help description.
	            "-h",     // Flag token.
	            "--help"  // Flag token.
	            );

	    opt.add(
	            "",
	            0,
	            1,
	            0,
	            "Options to get version control",
	            "-v",
	            "--version"
	            );

	    opt.add(
	            "in.idf",
	            0,
	            1,
	            0,
	            "Input file to process",
	            "-i",
	            "--input"
	            );

	    opt.add(
	            "in.epw",
	            0,
	            1,
	            0,
	            "Input weather file to process",
	            "-w",
	            "--weather"
	            );

	    opt.add(
	            "Energy+.idd",
	            0,
	            1,
	            0,
	            "Input energy file to process",
	            "-e",
	            "--energy"
	            );

	    opt.add(
	            "eplustbl.csv",
	            0,
	            1,
	            0,
	            "Output file",
	            "-o",
	            "--output"
	            );


	    opt.parse(argc, argv);

	    std::string usage;

		if (opt.isSet("-h")) {
			opt.getUsage(usage);
	        DisplayString(usage);
			return 1;
		}

	    if (opt.isSet("-v")) {
			DisplayString(VerString);
			return 1;
	    }

	    std::vector<std::string> badOptions;
	    if(!opt.gotExpected(badOptions)) {
			for(int i=0; i < badOptions.size(); ++i)
				DisplayString("ERROR: Got unexpected number of arguments for option " + badOptions[i] + ".\n\n");

			opt.getUsage(usage);
			DisplayString(usage);
			return 1;
		}

	    Usage(opt); //Ugly at the moment

	        std::istream* instream;
	        std::string input_filename;
	        std::string input_weatherFile;
	        std::string input_energyFile;
	        std::string output_File;
	        std::ifstream input;
	        std::ifstream input1;
	        std::ifstream input2;


	        if (opt.isSet("-i")) {
	    		opt.get("-i")->getString(input_filename);
	            input.open( input_filename.c_str() );
	            instream = &input;
	            DisplayString("\n\n====================================================================== \n");
	            DisplayString("-- Input file request by the user = " + input_filename);
	            if (!input) {
	                DisplayString("\n -X- Error opening input file = " + input_filename + " -X- \n");
	                DisplayString(" -X- Exiting -X- \n") ;
	                return -1;
	            }
	        }

	        if (opt.isSet("-w")) {
	    		opt.get("-w")->getString(input_weatherFile);
	            input1.open( input_weatherFile.c_str() );
	            instream = &input1;
	            DisplayString("====================================================================== \n");
	            DisplayString("-- Weather file requested by the user = " + input_weatherFile + "\n");
	            if (!input1) {
	                DisplayString("\n -X- Error opening weather file =  " + input_weatherFile + " -X- \n");
	                DisplayString(" -X- Exiting -X- \n");
	                return -1;
	            }
	        }

	        if (opt.isSet("-e")) {
	    		opt.get("-e")->getString(input_energyFile);
	            input2.open( input_energyFile.c_str() );
	            instream = &input2;
	            DisplayString("====================================================================== \n");
	            DisplayString("-- Energy file requested by the user = " + input_energyFile + "\n");
	            DisplayString("====================================================================== ");
	            if (!input2) {
	                DisplayString("\n -X- Error opening energy file =  " + input_energyFile + " -X- \n");
	                DisplayString(" -X- Exiting -X- \n");
	                return -1;
	            }
	        }

	        if (opt.isSet("-o")) {
	    		opt.get("-o")->getString(output_File);
	            std::ofstream output;
	            output.open( output_File.c_str() );
	            DisplayString("====================================================================== \n");
	            DisplayString("-- Output to be stored in file = " + output_File + "\n");
	            DisplayString("====================================================================== ");
	            if (!output) {
	                DisplayString("\n -X- Error opening output file =  " + output_File + " -X- \n");
	                DisplayString(" -X- Exiting -X- \n");
	                return -1;
	            }
	        }

	           if(input_filename.empty())
	               input_filename = "in.idf";

	           inputFileName = assign(input_filename);

	           if(input_weatherFile.empty())
	               input_weatherFile = "in.epw";

	           inputWeatherFile = assignWFile(input_weatherFile);

	           if(input_energyFile.empty())
	               input_energyFile = "Energy+.idd";

	           inputEnergyFile = assignEFile(input_energyFile);

	           if(output_File.empty())
	               output_File = "eplustbl.csv";

	           outputFile = assignOFile(output_File);
 return 0;
	        /////////////////////////////////////////////////////
}
} //namespace options
} //EnergyPlus namespace
