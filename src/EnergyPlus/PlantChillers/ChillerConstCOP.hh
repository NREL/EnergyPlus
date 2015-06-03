#ifndef CHILLERCONSTCOP_HH_INCLUDED
#define CHILLERCONSTCOP_HH_INCLUDED

#include <memory>

#include <DataPrecisionGlobals.hh>
#include <PlantComponent.hh>
#include <PlantChillers/ChillerBase.hh>
#include <PlantLocation.hh>

#include <ObjexxFCL/Array1D.hh>

namespace EnergyPlus {

namespace PlantChillers {

class ChillerConstCOP : public ChillerBase {

	// report variables
	ChillerBaseReportVars reports;
	Real64 ActualCOP = 0.0;

	// methods
	public:
		static std::shared_ptr< PlantComponent >
		constCOPChillerFactory(
			int objectType,
			std::string objectName
		);

	private:
		int performEveryTimeInit();
		int performOneTimeInit();
		int performBeginEnvrnInit();
		int performFirstHVACInit();
		int simulate( const PlantLocation & calledFromLocation );
		int sizeChiller();
		int calcChiller();
		int updateChiller();

};

extern Array1D< std::shared_ptr< ChillerConstCOP > > ConstCOPChiller; // dimension to number of machines

}

}

#endif
