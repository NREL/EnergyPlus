#ifndef CommandLineInterface_hh_INCLUDED
#define CommandLineInterface_hh_INCLUDED

#include <string>
namespace EnergyPlus{

namespace CommandLineInterface {

 extern std::string outputAuditFile;
 extern std::string outputBndFile;
 extern std::string outputDxfFile;
 extern std::string outputEioFile;
 extern std::string outputEndFile;
 extern std::string outputErrFile;
 extern std::string outputEsoFile;
 extern std::string outputMtdFile;
 extern std::string outputMddFile;
 extern std::string outputMtrFile;
 extern std::string outputRddFile;
 extern std::string outputShdFile;
 extern std::string outputCsvFile;
 extern std::string outputHtmFile;
 extern std::string outputTabFile;
 extern std::string outputTxtFile;
 extern std::string outputXmlFile;
 extern std::string inputFileName;
 extern	std::string inputEnergyFile;
 extern	std::string inputWeatherFile;

 // Process command line arguments
 int
 ProcessArgs( int argc, const char * argv[] );
 }
}
#endif
