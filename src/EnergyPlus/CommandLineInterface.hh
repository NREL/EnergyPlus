#ifndef CommandLineInterface_hh_INCLUDED
#define CommandLineInterface_hh_INCLUDED

#include <string>
#include <EnergyPlusAPI.hh>
namespace EnergyPlus{

namespace CommandLineInterface {

 extern std::string outputAuditFileName;
 extern std::string outputBndFileName;
 extern std::string outputDxfFileName;
 extern std::string outputEioFileName;
 extern std::string outputEndFileName;
 extern std::string outputErrFileName;
 extern std::string outputEsoFileName;
 extern std::string outputMtdFileName;
 extern std::string outputMddFileName;
 extern std::string outputMtrFileName;
 extern std::string outputRddFileName;
 extern std::string outputShdFileName;
 extern std::string outputTblCsvFileName;
 extern std::string outputTblHtmFileName;
 extern std::string outputTblTabFileName;
 extern std::string outputTblTxtFileName;
 extern std::string outputTblXmlFileName;
 extern std::string inputIdfFileName;
 extern	std::string inputIddFileName;
 extern	std::string inputWeatherFileName;
 extern std::string outputAdsFileName;
 extern std::string outputDfsFileName;
 extern std::string outputDelightInFileName;
 extern std::string outputDelightOutFileName;
 extern std::string outputDelightEldmpFileName;
 extern std::string outputDelightDfdmpFileName;
 extern std::string outputMapTabFileName;
 extern std::string outputMapCsvFileName;
 extern std::string outputMapTxtFileName;
 extern std::string outputEddFileName;
 extern std::string outputIperrFileName;
 extern std::string outputDbgFileName;
 extern std::string outputSlnFileName;
 extern std::string outputSciFileName;
 extern std::string outputWrlFileName;
 extern std::string outputZszCsvFileName;
 extern std::string outputZszTabFileName;
 extern std::string outputZszTxtFileName;
 extern std::string outputSszCsvFileName;
 extern std::string outputSszTabFileName;
 extern std::string outputSszTxtFileName;
 extern std::string outputScreenCsvFileName;
 extern std::string outputSqlFileName;
 extern std::string outputSqliteErrFileName;
 extern std::string EnergyPlusIniFileName;
 extern std::string inStatFileName;
 extern std::string TarcogIterationsFileName;
 extern std::string eplusADSFileName;
 extern std::string outputCsvFileName;
 extern std::string outputMtrCsvFileName;
 extern std::string outputRvauditFileName;

 extern std::string weatherFileNameOnly;
 extern std::string idfDirPathName;
 extern std::string idfFileNameOnly;
 extern std::string exeDirectory;

 extern bool runReadVars;
 extern bool DDOnlySimulation;
 extern bool AnnualSimulation;

 // Process command line arguments
 int
 ENERGYPLUSLIB_API ProcessArgs( int argc, const char * argv[] );

 void
 ReadINIFile(
 	int const UnitNumber, // Unit number of the opened INI file
 	std::string const & Heading, // Heading for the parameters ('[heading]')
 	std::string const & KindofParameter, // Kind of parameter to be found (String)
 	std::string & DataOut // Output from the retrieval
 );

 }

}
#endif
