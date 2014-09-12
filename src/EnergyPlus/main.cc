// C++ Headers
#ifndef NDEBUG
#ifdef __unix__
#include <cfenv>
#endif
#endif

// ObjexxFCL Headers
#include <ObjexxFCL/environment.hh>
#include <ObjexxFCL/FArray1D.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>
#include <ObjexxFCL/Time_Date.hh>

// CLI Headers
#include <../../CLI/ezOptionParser.hpp> // Added by Monika Sharma for CLI
#include <stdio.h> // Added by Monika Sharma for CLI

// EnergyPlus Headers
#include <main.hh>
#include <DataEnvironment.hh>
#include <DataGlobals.hh>
#include <DataPrecisionGlobals.hh>
#include <DataStringGlobals.hh>
#include <DataSystemVariables.hh>
#include <DataTimings.hh>
#include <DisplayRoutines.hh>
#include <FluidProperties.hh>
#include <InputProcessor.hh>
#include <OutputProcessor.hh>
#include <OutputReportTabular.hh>
#include <Psychrometrics.hh>
#include <ScheduleManager.hh>
#include <SimulationManager.hh>
#include <UtilityRoutines.hh>

using namespace ez;
using namespace std;
//using namespace vif;

void Usage(ezOptionParser& opt) {
	string usage;
	opt.getUsage(usage);
	cout << usage;
};


int
main(int argc, const char * argv[])
{
	// Using/Aliasing
	using namespace EnergyPlus;
  	//      NOTICE

	//      Copyright © 1996-2014 The Board of Trustees of the University of Illinois and The Regents of the
	//      University of California through Ernest Orlando Lawrence Berkeley National Laboratory.  All rights
	//      reserved.

	//      Portions of the EnergyPlus(tm) software package have been developed and copyrighted by other
	//      individuals, companies and institutions.  These portions have been incorporated into the EnergyPlus
	//      software package under license.

	//      In addition to the primary authorship of the LBNL Simulation Research Group (
	//      http://simulationresearch.lbl.gov/) and the UIUC Building Systems Laboratory (
	//      http://www.bso.uiuc.edu/), the following have contributed to EnergyPlus V1.0:

	//      Portions of the EnergyPlus weather processor were developed by US Department of Energy, Building
	//      Technologies Program, www.energyplus.gov

	//      Portions of the input processing, output processing, weather processor, BLAST Translator were
	//      developed by US Army Corps of Engineers, Construction Engineering Research Laboratories, 2902 Newmark
	//      Drive, Champaign IL  61821. www.cecer.army.mil

	//      Portions of this software package were developed by Linda Lawrie of DHL Consulting LLC.

	//      Portions of this software package were developed by C.O. Pedersen Associates.

	//      Portions of the EnergyPlus utility software (EP-Launch, IDFEditor, DOE2Translator, HVAC-Diagram,
	//      ExpandObjects, CSVProc, System Templates, and convertESOMTR) were developed by GARD Analytics, Inc.
	//      1028 Busse Highway, Park Ridge, Illinois 60068-1802, USA (847) 698-5690, www.gard.com.  GARD Analytics
	//      performed independent verification and validation testing of the software after developing the testing
	//      strategy and plan.  GARD Analytics was also responsible for gas absorption chiller, desiccant
	//      dehumidifier, ice storage (simple), table reports and economics.

	//      Portions of flow resolver, chiller models (absorption, electric, const cop, engine-driven, gas-
	//      turbine), generator models (diesel electric, gas turbine), furnace models, heat recovery loop, plant
	//      loop, plant condenser loop, air-change dependent inside film coefficients were developed by Oklahoma
	//      State University, 110 Engineering North, Stillwater, OK 74078.

	//      Portions of EnergyPlus related to the models for EMPD moisture calculations, DX coils, furnace/unitary
	//      systems, cooling towers, air-to-air heat pumps, air distribution systems, refrigerated cases, electric
	//      EIR chillers, packaged terminal heat pumps, desuperheater air and water heating coils, and heat pump
	//      water heaters were developed by University of Central Florida, Florida Solar Energy Center (FSEC),
	//      1679 Clearlake Road, Cocoa, FL  32922, www.fsec.ucf.edu/.

	//      Portions of EnergyPlus were developed by the National Renewable Energy Laboratory (NREL), 1617 Cole
	//      Blvd, Golden, CO 80401.

	//      EnergyPlus v1.0.1, v1.0.2, v1.0.3, v1.1, v1.1.1 (Wintel platform) includes a link to TRNSYS (The Transient
	//      Energy System Simulation Tool) for photovoltaic calculations developed by Thermal Energy System Specialists,
	//      2916 Marketplace Drive, Suite 104, Madison, WI 53719; Tel: (608) 274-2577. EnergyPlus v1.2 and later
	//      includes Photovoltaic calculations implemented in EnergyPlus by Thermal Energy System Specialists.
	//      This model was originally developed by Oystein Ulleberg, Institute for Energy Technology, Norway -- based on
	//      the Duffie and Beckman equivalent one-diode model.

	//      Portions of this software package that convert certain stand-alone heat transfer models for slab-on-
	//      grade and basement foundations were developed by William Bahnfleth, Cynthia Cogil, and Edward
	//      Clements, Department of Architectural Engineering, Pennsylvania State University, 224 Engineering Unit
	//      A, University Park, Pennsylvania  16802-1416, (814) 863-2076.

	//      The concept and initial implementation for the EnergyPlus COM/DLL version (Wintel platform) was made
	//      possible through cooperation with DesignBuilder Software, Ltd, Andy Tindale - an EnergyPlus
	//      collaborative developer.

	//      The thickness, conductivity, density and specific heat values of the material layers for the
	//      constructions in the Composite Wall Construction reference data set have been taken from the ASHRAE
	//      report "Modeling Two- and Three-Dimensional Heat Transfer through Composite Wall and Roof Assemblies
	//      in Hourly  Energy Simulation Programs (1145-TRP)," by Enermodal Engineering Limited, Oak Ridge
	//      National Laboratory, and the Polish Academy of Sciences, January 2001.

	//      EnergyPlus v1.2 contains DELight2 (wintel platform), a simulation engine for daylighting and electric
	//      lighting system analysis developed at Ernest Orlando Lawrence Berkeley National Laboratory.

	//      Portions of the EnergyPlus v1.2 air distribution system calculations were written by George Walton of
	//      the National Institute for Standards and Technology (NIST), 100 Bureau Drive, Gaithersburg, MD 20899,
	//      (301) 975-6478.  The EnergyPlus AirflowNetwork model also includes portions of an early version of COMIS
	//      (Conjunction Of Multizone Infiltration Specialists) developed by a multinational, multi-institutional
	//      effort under the auspices of the International Energy Agency's Buildings and Community Systems Agreement
	//      working group focusing on multizone air flow modeling (Annex 23) and now administered by the Swiss Federal
	//      Laboratories for Materials Testing and Research (EMPA), Division 175, Überlandstrasse 129, CH-8600 Dübendorf,
	//      Switzerland.

	//      The EnergyPlus v1.2 model for displacement ventilation and cross-ventilation was developed
	//      by Guilherme Carrilho da Graça and Paul Linden of the Department of Mechanical and Aerospace
	//      Engineering, University of California, San Diego.

	//      The EnergyPlus models for UFAD served zones were developed by Anna Liu and Paul Linden at the Department
	//      of Mechanical and Aerospace Engineering, University of California, San Diego.

	//      ASHRAE research project 1254-RP supported the development of the following features first added in
	//      EnergyPlus v1.2.2:
	//         DXSystem:AirLoop enhancements (valid as OA system equipment, new humidity control options);
	//         New set point managers: SET POINT MANAGER:SINGLE ZONE HEATING, SET POINT MANAGER:SINGLE ZONE COOLING,
	//                 and SET POINT MANAGER:OUTSIDE AIR PRETREAT;
	//         New 2-stage DX coil with enhanced dehumidification option (COIL:DX:MultiMode:CoolingEmpirical);
	//         Additional DESICCANT DEHUMIDIFIER:SOLID setpoint control option;
	//      American Society of Heating Refrigerating and Air-Conditioning Engineers, Inc,,
	//      1791 Tullie Circle, N.E., Atlanta, GA 30329. www.ashrae.org
	//      Work performed by GARD Analytics, Inc., 1028 Busse Highway, Park Ridge, Illinois 60068-1802, USA.
	//      www.gard.com, November 2004.

	//      EnergyPlus v1.2.2 and later versions (wintel platform) contains links to SPARK, a simulation engine for
	//      detailed system modeling developed at Ernest Orlando Lawrence Berkeley National Laboratory in
	//      conjunction with Ayres Sowell Associates, Inc.  SPARK was removed in V3.1 - April 2009 release.

	//      The Ecoroof (Green Roof) model, first introduced in EnergyPlus v2.0, was developed at Portland State University,
	//      by David Sailor and his students. It is based on the FASST vegetation models developed by Frankenstein and
	//      Koenig for the US Army Corps of Engineers.

	//      The HAMT (Heat And Moisture Transfer) model, first introduced in EnergyPlus v3.0.0 was developed by Phillip Biddulph,
	//      Complex Built Environment Systems, The Bartlett School of Graduate Studies, University College London, Gower Street,
	//      London WC1E 6BT, United Kingdom. http://www.cbes.ucl.ac.uk/.

	//      The SQLite output module, first introduced in EnergyPlus v3.0.0, was developed by Gregory B. Stark, P.E.,
	//      Building Synergies, LLC, 1860 Washington Street, Suite 208, Denver, Colorado 80203, United States.
	//      http://www.buildingsynergies.com/

	//      Refrigeration compressor performance data and refrigeration practices were provided by CDH Energy, Cazenovia, NY 12035.

	//      NOTICE: The U.S. Government is granted for itself and others acting on its behalf a paid-up,
	//      nonexclusive, irrevocable, worldwide license in this data to reproduce, prepare derivative works, and
	//      perform publicly and display publicly.  Beginning five (5) years after permission to assert copyright
	//      is granted, subject to two possible five year renewals, the U.S. Government is granted for itself and
	//      others acting on its behalf a paid-up, non-exclusive, irrevocable worldwide license in this data to
	//      reproduce, prepare derivative works, distribute copies to the public, perform publicly and display
	//      publicly, and to permit others to do so.

	//      TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.

	//      Other Acknowledgments

	//      This work was supported by the Assistant Secretary for Energy Efficiency and Renewable Energy, Office
	//      of Building Technologies Program of the US Department of Energy.

	//      Additional support was provided by the Gas Technology Institute and the California Energy Commission.

	//      The ice thermal storage module development was supported by the U.S. Department of Energy Office of
	//      Electricity Delivery and Energy Reliability.

	//      The HAMT (Heat And Moisture Transfer) model was supported by the Engineering and Physical Sciences Research Council (EPSRC),
	//      the UK government agency for funding research and training in engineering and the physical sciences.

	//      The SQLite output module was funded by Building Synergies, LLC and was made possible by inclusion of software code
	//      from the SQLite project (http://www.sqlite.org/).

	// PROGRAM INFORMATION:
	//       AUTHOR         Linda K. Lawrie, et al
	//       DATE WRITTEN   January 1997.....
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS PROGRAM:
	// This program implements the calls for EnergyPlus (originally configured
	// as the merger of BLAST/IBLAST and DOE-2 energy analysis programs).

	// METHODOLOGY EMPLOYED:
	// The method used in EnergyPlus is to simplify the main program as much
	// as possible and contain all "simulation" code in other modules and files.

	// REFERENCES:
	// na

	// USE STATEMENTS:
	// data only modules
	using namespace DataPrecisionGlobals;
	using namespace DataStringGlobals;
	using namespace DataGlobals;
	using namespace DataSystemVariables;
	using namespace DataTimings;
	using DataEnvironment::IgnoreSolarRadiation;
	using DataEnvironment::IgnoreBeamRadiation;
	using DataEnvironment::IgnoreDiffuseRadiation;
	// routine modules
	using namespace InputProcessor;
	using namespace OutputProcessor;
	using namespace SimulationManager;
    using namespace OutputReportTabular;
	using ScheduleManager::ReportOrphanSchedules;
	using FluidProperties::ReportOrphanFluids;
	using Psychrometrics::ShowPsychrometricSummary;
    
    ///////// Added for CLI /////////////////////////////
    ezOptionParser opt;
    
    opt.overview = "\n*******************************************\nCopyright (C) 2012-2014 Big Ladder Software\nWeb: www.bigladdersoftware.com\n";
    opt.overview += VerString;
    opt.overview += "\n*******************************************\n";
    opt.syntax = "./EnergyPlus -df InputFile.idf -i Energy+.idd -w WeatherFile.epw -o OutFile.csv";
    
    opt.add(
            "", // Default.
            0, // Required?
            1, // Number of args expected.
            0, // Delimiter if expecting multiple args.
            "Usage information displayed on top", // Help description.
            "-h",     // Flag token.
            "-help",  // Flag token.
            "--help", // Flag token.
            "--usage" // Flag token.
            );
    
    opt.add(
            "",
            0,
            1,
            0,
            "Options to get version control",
            "-v",
            "--v",
            "-version",
            "--version"
            );
    
    opt.add(
            "in.idf",
            0,
            1,
            0,
            "Input file to process",
            "-df",
            "--df"
            //"--in",
            //"--input"
            );
    
    opt.add(
            "in.epw",
            0,
            1,
            0,
            "Input weather file to process",
            "-w",
            "-wea",
            "--w",
            "--wea"
            );
    
    opt.add(
            "Energy+.idd",
            0,
            1,
            0,
            "Input energy file to process",
            "-i",
            "-in",
            "--i",
            "--in"
            );
    
    opt.add(
            "eplustbl.csv",
            0,
            1,
            0,
            "Output file",
            "-o",
            "-out",
            "--o",
            "--out"
            );
    

    opt.parse(argc, argv);
    if(argv[1]==0) cout<<"No arguments provided"<<endl;
    
    std::string usage;
    
	if (opt.isSet("-h")) {
		opt.getUsage(usage);
        std::cout << usage;
		return 1;
	}
    
    if (opt.isSet("-v")) {
		cout << VerString;
		return 1;
    }
    
    std::vector<std::string> badOptions;
    if(!opt.gotExpected(badOptions)) {
		for(int i=0; i < badOptions.size(); ++i)
			cerr << "ERROR: Got unexpected number of arguments for option " << badOptions[i] << ".\n\n";
        
		opt.getUsage(usage);
		cout << usage;
		return -1;
	}
    
/*	if(!opt.gotRequired(badOptions)) {
		for(int i=0; i < badOptions.size(); ++i)
			std::cerr << "ERROR: Missing required option " << badOptions[i] << ".\n\n";
	//	Usage(opt);
        opt.getUsage(usage);
		return 1;
	}
    
    std::vector<std::string> badArgs;
    if(!opt.gotValid(badOptions, badArgs)) {
        for(int i=0; i < badOptions.size(); ++i)
            std::cerr << "ERROR: Got invalid argument \"" << badArgs[i] << "\" for option " << badOptions[i] << ".\n\n";
        
        //Usage(opt);
        return 1;
    }
    
    if (opt.lastArgs.size() < 2) {
        cout << "Zeroth argument = " << argv[0] << '\t'<< argv[1] << '\t'<< argv[2] << endl;
		cerr << "\n\n-------------------------------------------\n";
        cerr << "** EnergyPlus in default configuration **\n";
        cerr << "-------------------------------------------\n";
		Usage(opt);
	}
 */ // gotRequired, gotValid and lastArgs.size() options aren't working at the moment.
    
    Usage(opt); //Ugly at the moment
    
    int status = 0;
    std::istream* instream;
    std::string input_filename;
    std::string input_weatherFile;
    std::string input_energyFile;
    std::string output_File;
    std::ifstream input;
    std::ifstream input1;
    std::ifstream input2;
    
    if (opt.isSet("-df")) {
		opt.get("-df")->getString(input_filename);
        input.open( input_filename.c_str() );
        instream = &input;
        std::cout << "\n\n====================================================================== \n";
        std::cout << "-- Input file request by the user = " << input_filename << std::endl;
        if (!input) {
            status = -1;
            std::cerr << "\n -X- Error opening input file = " << input_filename << " -X- \n";
            std::cerr << " -X- Exiting -X- \n" ;
            return -1;
        }
    }
    
    if (opt.isSet("-w")) {
		opt.get("-w")->getString(input_weatherFile);
        input1.open( input_weatherFile.c_str() );
        instream = &input1;
        std::cout << "====================================================================== \n";
        std::cout << "-- Weather file requested by the user = " << input_weatherFile << std::endl;
        if (!input1) {
            status = -1;
            std::cerr << "\n -X- Error opening weather file =  " << input_weatherFile << " -X- \n";
            std::cerr << " -X- Exiting -X- \n";
            return -1;
        }
    }
    
    if (opt.isSet("-i")) {
		opt.get("-i")->getString(input_energyFile);
        input2.open( input_energyFile.c_str() );
        instream = &input2;
        std::cout << "====================================================================== \n";
        std::cout << "-- Energy file requested by the user = " << input_energyFile << std::endl;
        std::cout << "====================================================================== ";
        if (!input2) {
            status = -1;
            std::cerr << "\n -X- Error opening energy file =  " << input_energyFile << " -X- \n";
            std::cerr << " -X- Exiting -X- \n";
            return -1;
        }
    }
    
    if (opt.isSet("-o")) {
		opt.get("-o")->getString(output_File);
        std::ofstream output;
        output.open( output_File.c_str() );
        std::cout << "====================================================================== \n";
        std::cout << "-- Output to be stored in file = " << output_File << std::endl;
        std::cout << "====================================================================== ";
        if (!output) {
            status = -1;
            std::cerr << "\n -X- Error opening output file =  " << output_File << " -X- \n";
            std::cerr << " -X- Exiting -X- \n";
            return -1;
        }
    }
    
    /////////////////////////////////////////////////////

// Enable floating point exceptions
#ifndef NDEBUG
#ifdef __unix__
	feenableexcept( FE_DIVBYZERO | FE_INVALID | FE_OVERFLOW );
#endif
#endif

	// Locals
	// PROGRAM PARAMETER DEFINITIONS:
	// Note: General Parameters for the entire EnergyPlus program are contained
	// in "DataGlobals.f90"
	gio::Fmt const EPlusiniFormat( "(/,'[',A,']',/,'dir=',A)" );
	string const BlankString;

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// DERIVED TYPE DEFINITIONS
	// na

	// PROGRAM LOCAL VARIABLE DECLARATIONS:
	int LFN; // Unit Number for reads
	bool EPlusINI;
	string::size_type TempIndx;
	static string cEnvValue;
	int iostatus;
	bool FileExists;

	//                           INITIALIZE VARIABLES
	Time_Start = epElapsedTime();
#ifdef EP_Detailed_Timings
	epStartTime( "EntireRun=" );
#endif
	CreateCurrentDateTimeString( CurrentDateTime );
	VerString += "," + CurrentDateTime;
	get_environment_variable( DDOnlyEnvVar, cEnvValue );
	DDOnly = env_var_on( cEnvValue ); // Yes or True

	get_environment_variable( ReverseDDEnvVar, cEnvValue );
	ReverseDD = env_var_on( cEnvValue ); // Yes or True

	get_environment_variable( FullAnnualSimulation, cEnvValue );
	FullAnnualRun = env_var_on( cEnvValue ); // Yes or True

	get_environment_variable( cDisplayAllWarnings, cEnvValue );
	DisplayAllWarnings = env_var_on( cEnvValue ); // Yes or True
	if ( DisplayAllWarnings ) {
		DisplayAllWarnings = true;
		DisplayExtraWarnings = true;
		DisplayUnusedSchedules = true;
		DisplayUnusedObjects = true;
	}

	get_environment_variable( cDisplayExtraWarnings, cEnvValue );
	if ( ! cEnvValue.empty() ) DisplayExtraWarnings = env_var_on( cEnvValue ); // Yes or True

	get_environment_variable( cDisplayUnusedObjects, cEnvValue );
	if ( ! cEnvValue.empty() ) DisplayUnusedObjects = env_var_on( cEnvValue ); // Yes or True

	get_environment_variable( cDisplayUnusedSchedules, cEnvValue );
	if ( ! cEnvValue.empty() ) DisplayUnusedSchedules = env_var_on( cEnvValue ); // Yes or True

	get_environment_variable( cDisplayZoneAirHeatBalanceOffBalance, cEnvValue );
	if ( ! cEnvValue.empty() ) DisplayZoneAirHeatBalanceOffBalance = env_var_on( cEnvValue ); // Yes or True

	get_environment_variable( cDisplayAdvancedReportVariables, cEnvValue );
	if ( ! cEnvValue.empty() ) DisplayAdvancedReportVariables = env_var_on( cEnvValue ); // Yes or True

	get_environment_variable( cReportDuringWarmup, cEnvValue );
	if ( ! cEnvValue.empty() ) ReportDuringWarmup = env_var_on( cEnvValue ); // Yes or True

	get_environment_variable( cIgnoreSolarRadiation, cEnvValue );
	if ( ! cEnvValue.empty() ) IgnoreSolarRadiation = env_var_on( cEnvValue ); // Yes or True

	get_environment_variable( cMinimalSurfaceVariables, cEnvValue );
	if ( ! cEnvValue.empty() ) CreateMinimalSurfaceVariables = env_var_on( cEnvValue ); // Yes or True

	get_environment_variable( cSortIDD, cEnvValue );
	if ( ! cEnvValue.empty() ) SortedIDD = env_var_on( cEnvValue ); // Yes or True

	get_environment_variable( MinReportFrequencyEnvVar, cEnvValue );
	if ( ! cEnvValue.empty() ) cMinReportFrequency = cEnvValue; // turned into value later

	get_environment_variable( cDeveloperFlag, cEnvValue );
	if ( ! cEnvValue.empty() ) DeveloperFlag = env_var_on( cEnvValue ); // Yes or True

	get_environment_variable( cIgnoreBeamRadiation, cEnvValue );
	if ( ! cEnvValue.empty() ) IgnoreBeamRadiation = env_var_on( cEnvValue ); // Yes or True

	get_environment_variable( cIgnoreDiffuseRadiation, cEnvValue );
	if ( ! cEnvValue.empty() ) IgnoreDiffuseRadiation = env_var_on( cEnvValue ); // Yes or True

	get_environment_variable( cSutherlandHodgman, cEnvValue );
	if ( ! cEnvValue.empty() ) SutherlandHodgman = env_var_on( cEnvValue ); // Yes or True

	get_environment_variable( cMinimalShadowing, cEnvValue );
	if ( ! cEnvValue.empty() ) lMinimalShadowing = env_var_on( cEnvValue ); // Yes or True

	get_environment_variable( cTimingFlag, cEnvValue );
	if ( ! cEnvValue.empty() ) TimingFlag = env_var_on( cEnvValue ); // Yes or True

	// Initialize env flags for air loop simulation debugging
	get_environment_variable( TrackAirLoopEnvVar, cEnvValue );
	if ( ! cEnvValue.empty() ) TrackAirLoopEnvFlag = env_var_on( cEnvValue ); // Yes or True

	get_environment_variable( TraceAirLoopEnvVar, cEnvValue );
	if ( ! cEnvValue.empty() ) TraceAirLoopEnvFlag = env_var_on( cEnvValue ); // Yes or True

	get_environment_variable( TraceHVACControllerEnvVar, cEnvValue );
	if ( ! cEnvValue.empty() ) TraceHVACControllerEnvFlag = env_var_on( cEnvValue ); // Yes or True

	{ IOFlags flags; gio::inquire( "eplusout.end", flags ); FileExists = flags.exists(); }
	if ( FileExists ) {
		LFN = GetNewUnitNumber();
		{ IOFlags flags; flags.ACTION( "read" ); gio::open( LFN, "eplusout.end", flags ); iostatus = flags.ios(); }
		if ( iostatus != 0 ) {
			ShowFatalError( "EnergyPlus: Could not open file \"eplusout.end\" for input (read)." );
		}
		{ IOFlags flags; flags.DISPOSE( "delete" ); gio::close( LFN, flags ); }
	}

	{ IOFlags flags; gio::inquire( "Energy+.ini", flags ); EPlusINI = flags.exists(); }
	if ( EPlusINI ) {
		LFN = GetNewUnitNumber();
		{ IOFlags flags; flags.ACTION( "read" ); gio::open( LFN, "Energy+.ini", flags ); iostatus = flags.ios(); }
		if ( iostatus != 0 ) {
			ShowFatalError( "EnergyPlus: Could not open file \"Energy+.ini\" for input (read)." );
		}
		{ IOFlags flags; gio::inquire( LFN, flags ); CurrentWorkingFolder = flags.name(); }
		// Relying on compiler to supply full path name here
		TempIndx = index( CurrentWorkingFolder, pathChar, true );
		if ( TempIndx == string::npos ) {
			CurrentWorkingFolder = "";
		} else {
			CurrentWorkingFolder.erase( TempIndx + 1 );
		}
		//       Get directories from ini file
		ReadINIFile( LFN, "program", "dir", ProgramPath );

		gio::close( LFN );
	} else {
		DisplayString( "Missing Energy+.ini" );
		ProgramPath = "";
		LFN = GetNewUnitNumber();
		{ IOFlags flags; flags.ACTION( "write" ); gio::open( LFN, "Energy+.ini", flags ); iostatus = flags.ios(); }
		if ( iostatus != 0 ) {
			ShowFatalError( "EnergyPlus: Could not open file \"Energy+.ini\" for output (write)." );
		}
		// Relying on compiler to supply full path name here
		{ IOFlags flags; gio::inquire( LFN, flags ); CurrentWorkingFolder = flags.name(); }
		TempIndx = index( CurrentWorkingFolder, pathChar, true );
		if ( TempIndx == string::npos ) {
			CurrentWorkingFolder = "";
		} else {
			CurrentWorkingFolder.erase( TempIndx + 1 );
		}
		gio::write( LFN, EPlusiniFormat ) << "program" << ProgramPath;
		gio::close( LFN );
	}
	TestAllPaths = true;

    DisplayString( "\n" );
    DisplayString( "\n" );
    
	OutputFileDebug = GetNewUnitNumber();
	{ IOFlags flags; flags.ACTION( "write" ); gio::open( OutputFileDebug, "eplusout.dbg", flags ); iostatus = flags.ios(); }
	if ( iostatus != 0 ) {
		ShowFatalError( "EnergyPlus: Could not open file \"eplusout.dbg\" for output (write)." );
	}

	//Call ProcessInput to produce the IDF file which is read by all of the
	// Get input routines in the rest of the simulation

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
    
	ProcessInput();

	ManageSimulation();

	ShowMessage( "Simulation Error Summary *************" );

	GenOutputVariablesAuditReport();

	ShowPsychrometricSummary();

	ReportOrphanRecordObjects();
	ReportOrphanFluids();
	ReportOrphanSchedules();

	EndEnergyPlus();
    
    return 0;
}

void
CreateCurrentDateTimeString( string & CurrentDateTimeString )
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Linda Lawrie
	//       DATE WRITTEN   October 2010
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// Be able to supply a current date/time string from intrinsic calls so
	// that one is always available.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// na

	// USE STATEMENTS:
	// na

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	gio::Fmt const fmtDate( "(1X,'YMD=',I4,'.',I2.2,'.',I2.2,1X,I2.2,':',I2.2)" );

	// INTERFACE BLOCK SPECIFICATIONS:
	// na

	// DERIVED TYPE DEFINITIONS:
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	FArray1D_int value( 8 );
	//value(1)   Current year
	//value(2)   Current month
	//value(3)   Current day
	//value(4)   Time difference with respect to UTC in minutes (0-59)
	//value(5)   Hour of the day (0-23)
	//value(6)   Minutes (0-59)
	//value(7)   Seconds (0-59)
	//value(8)   Milliseconds (0-999)
	string datestring; // supposedly returns blank when no date available.

	date_and_time_string( datestring, _, _, value );
	if ( ! datestring.empty() ) {
		gio::write( CurrentDateTimeString, fmtDate ) << value( 1 ) << value( 2 ) << value( 3 ) << value( 5 ) << value( 6 );
	} else {
		CurrentDateTimeString = " unknown date/time";
	}

}

void
ReadINIFile(
	int const UnitNumber, // Unit number of the opened INI file
	string const & Heading, // Heading for the parameters ('[heading]')
	string const & KindofParameter, // Kind of parameter to be found (String)
	string & DataOut // Output from the retrieval
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
	static string LINE;
	static string LINEOut;
	string Param;
	string::size_type ILB;
	string::size_type IRB;
	string::size_type IEQ;
	string::size_type IPAR;
	string::size_type IPOS;
	string::size_type ILEN;
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
		if ( ILB == string::npos && IRB == string::npos ) continue;
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
			NewHeading = ( ILB != string::npos && IRB != string::npos );

			//                                  Should be a parameter line
			//                                  KindofParameter = string
			IEQ = index( LINEOut, '=' );
			IPAR = index( LINEOut, Param );
			if ( IEQ == string::npos ) continue;
			if ( IPAR == string::npos ) continue;
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
