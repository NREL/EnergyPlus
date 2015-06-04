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

	public:
		class ReportVars : public ChillerBaseReportVars {
			public:
			Real64 Qcond;
			ReportVars() :
				Qcond( 0.0 )
			{}
		};

		// additional variables over the base class
		ReportVars report;
		Real64 ActualCOP = 0.0;
		Real64 curLoad = 0.0;
		bool runFlag = false;

		// methods
		ChillerConstCOP();
		
		static std::shared_ptr< PlantComponent >
		constCOPChillerFactory(
			int objectType,
			std::string objectName
		);

	private:
		int performEveryTimeInit( const PlantLocation & calledFromLocation );
		int performOneTimeInit( const PlantLocation & calledFromLocation );
		int performBeginEnvrnInit( const PlantLocation & calledFromLocation );
		int performFirstHVACInit( const PlantLocation & calledFromLocation );
		int simulate( const PlantLocation & calledFromLocation, bool const & FirstHVACIteration );
		int sizeChiller();
		int calcChiller();
		int updateChiller();

};

extern Array1D< std::shared_ptr< ChillerConstCOP > > ConstCOPChiller; // dimension to number of machines

}

}

#endif
