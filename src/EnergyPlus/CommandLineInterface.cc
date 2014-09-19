
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
#include <OutputReports.hh>
#include <SimulationManager.hh>
#include <SolarShading.hh>
#include <UtilityRoutines.hh>
using namespace ez;

namespace EnergyPlus{

namespace CommandLineInterface{

using namespace DataStringGlobals;
using namespace InputProcessor;
using namespace SimulationManager;
using namespace OutputReportTabular;
using namespace OutputProcessor;
using namespace SolarShading;

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
std::string outputHtmFile;
std::string outputTabFile;
std::string outputTxtFile;
std::string outputXmlFile;

void Usage(ezOptionParser& opt) {
	std::string usage;
	opt.getUsage(usage);
	DisplayString(usage);
};

int
ProcessArgs(int argc, const char * argv[])
{
	 std::istream* instream;
     std::string input_filename;
     std::string input_weatherFile;
     std::string input_energyFile;

     std::ifstream input;
     std::ifstream input1;
     std::ifstream input2;


	 ezOptionParser opt;

	    opt.overview =  "*******************************************\n";
	    opt.overview += "Copyright (C) 1996-2014 \n"; // Add owners of copyright?
	    opt.overview += "Web: www.energyplus.gov\n";
	    opt.overview += VerString;
	    opt.overview += "\n*******************************************\n";
	    opt.syntax = "./EnergyPlus -i InputFile.idf -e Energy+.idd -w WeatherFile.epw -o OutFile.csv";

	    opt.add("", 0, 1, 0, "Usage information displayed on top", "-h", "--help");

	    opt.add("", 0, 1, 0, "Options to get version control", "-v", "--version");

	    opt.add("in.idf", 0, 1, 0, "Input file to process", "-i", "--input");

	    opt.add("in.epw", 0, 1, 0, "Input weather file to process", "-w", "--weather");

	    opt.add("Energy+.idd", 0, 1, 0, "Input energy file to process", "-e", "--energy");

	    opt.add("eplustbl.csv", 0, 0, 0, "Output file", "-o", "--output");

	    std::string usage;
	    opt.parse(argc, argv);

		if (opt.isSet("-h")) {
			opt.getUsage(usage);
	        DisplayString(usage);
			return 1;
		}

	    if (opt.isSet("-v")) {
			DisplayString(VerString);
			return 1;
	    }

	    Usage(opt); //Ugly at the moment

	    std::vector<std::string> badOptions;
	    	   	 if(!opt.gotExpected(badOptions)) {
	    	   		for(int i=0; i < badOptions.size(); ++i)
	    				DisplayString("ERROR: Got unexpected number of arguments for option " + badOptions[i] + ".\n\n");
	    	   		    DisplayString("Usage information displayed above. \n");
	    	   			return 1;
	    	   		}

	    	   	if(!opt.isSet("-h") && !opt.isSet("-v") && !opt.isSet("-i") && !opt.isSet("-o") && !opt.isSet("-w") && !opt.isSet("-e")){
	    	   		std::cout<<"Invalid option used. Exiting...\n";
	    	   		return 1;
	    	   	}

	    	   	if(!opt.gotRequired(badOptions)) {
	    	   			for(int i=0; i < badOptions.size(); ++i)
	    	   				std::cerr << "ERROR: Missing required option " << badOptions[i] << ".\n\n";
	    	   			Usage(opt);
	    	   			return 1;
	    	   		}


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

	           if(input_filename.empty())
	               input_filename = "in.idf";

	           inputFileName = assign(input_filename);

	           if(input_weatherFile.empty())
	               input_weatherFile = "in.epw";

	           inputWeatherFile = assignWFile(input_weatherFile);

	           if(input_energyFile.empty())
	               input_energyFile = "Energy+.idd";

	           inputEnergyFile = assignEFile(input_energyFile);

	           std::string output_File = input_filename+"_"+input_weatherFile+".csv";
	           std::string output_AuditFile = input_filename+"_"+input_weatherFile+".audit";
	           std::string output_BndFile = input_filename+"_"+input_weatherFile+".bnd";
	           std::string output_DxfFile = input_filename+"_"+input_weatherFile+".dxf";
	           std::string output_EioFile = input_filename+"_"+input_weatherFile+".eio";
	           std::string output_EndFile = input_filename+"_"+input_weatherFile+".end";
	           std::string output_ErrFile = input_filename+"_"+input_weatherFile+".err";
	           std::string output_EsoFile = input_filename+"_"+input_weatherFile+".eso";
	           std::string output_MtdFile = input_filename+"_"+input_weatherFile+".mtd";
	           std::string output_MddFile = input_filename+"_"+input_weatherFile+".mdd";
	           std::string output_MtrFile = input_filename+"_"+input_weatherFile+".mtr";
	           std::string output_RddFile = input_filename+"_"+input_weatherFile+".rdd";
	           std::string output_ShdFile = input_filename+"_"+input_weatherFile+".shd";
	           std::string output_HtmFile = input_filename+"_"+input_weatherFile+".htm";
	           std::string output_TabFile = input_filename+"_"+input_weatherFile+".tab";
	           std::string output_TxtFile = input_filename+"_"+input_weatherFile+".txt";
	           std::string output_XmlFile = input_filename+"_"+input_weatherFile+".xml";

	           if (opt.isSet("-o")) {
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

	           if(output_File.empty())
	               	   	   	   output_File = "eplustbl.csv";

	           outputFile = assignOFile(output_File);

	           if(output_AuditFile.empty())
	        	   	   	   	   output_AuditFile = "eplusout.audit";

	           outputAuditFile = assignAuditFile(output_AuditFile);

	           if(output_BndFile.empty())
	        	   	   	   	   output_BndFile = "eplusout.bnd";

	           outputBndFile = assignBndFile(output_BndFile);

	           if(output_DxfFile.empty())
	           	        	   output_DxfFile = "eplusout.dxf";

	           outputDxfFile = assignDxfFile(output_DxfFile);

	           if(output_EioFile.empty())
	           	           	   output_EioFile = "eplusout.eio";

	           outputEioFile = assignEioFile(output_EioFile);

	           if(output_EndFile.empty())
	           	           	    output_EndFile = "eplusout.end";

	           outputEndFile = assignEndFile(output_EndFile);

	           if(output_ErrFile.empty())
	           	           	    output_ErrFile = "eplusout.err";

	           outputErrFile = assignErrFile(output_ErrFile);

	           if(output_EsoFile.empty())
	           	           	    output_EsoFile = "eplusout.eso";

	           outputEsoFile = assignEsoFile(output_EsoFile);

	           if(output_MtdFile.empty())
	           	           	    output_MtdFile = "eplusout.mtd";

	           outputMtdFile = assignMtdFile(output_MtdFile);

	           if(output_MddFile.empty())
	           	           	    output_MddFile = "eplusout.mdd";

	           outputMddFile = assignMddFile(output_MddFile);

	           if(output_MtrFile.empty())
	           	           	    output_MtrFile = "eplusout.mtr";

	           outputMtrFile = assignMtrFile(output_MtrFile);

	           if(output_RddFile.empty())
	           	           	    output_RddFile = "eplusout.rdd";

	           outputRddFile = assignRddFile(output_RddFile);

	           if(output_ShdFile.empty())
	           	           	    output_ShdFile = "eplusout.shd";

	           outputShdFile = assignShdFile(output_ShdFile);

	           if(output_HtmFile.empty())
	           	           	    output_HtmFile = "eplustbl.htm";

	           outputHtmFile = assignHtmFile(output_HtmFile);

	           if(output_TabFile.empty())
	           	           	    output_TabFile = "eplustbl.tab";

	           outputTabFile = assignTabFile(output_TabFile);

	           if(output_TxtFile.empty())
	           	           	    output_TxtFile = "eplustbl.txt";

	           outputTxtFile = assignTxtFile(output_TxtFile);

	           if(output_XmlFile.empty())
	           	           	    output_XmlFile = "eplustbl.xml";

	           outputXmlFile = assignXmlFile(output_XmlFile);

 return 0;
	        /////////////////////////////////////////////////////
}
} //namespace options
} //EnergyPlus namespace
