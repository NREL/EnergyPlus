#ifndef SiteFCFactorMethodTemperatures_hh_INCLUDED
#define SiteFCFactorMethodTemperatures_hh_INCLUDED

// C++ Headers
#include <memory>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <GroundTemperatureModeling/BaseGroundTemperatureModel.hh>

namespace EnergyPlus {

	// Derived class for Site:GroundTemperature:FCFactorMethod
	class SiteFCFactorMethodGroundTemps : public BaseGroundTempsModel
	{
		public:
			int timeOfSimInMonths;
			Array1D< Real64 > fcFactorGroundTemps;

		// Default Constructor
		SiteFCFactorMethodGroundTemps():
			timeOfSimInMonths( 0 ),
			fcFactorGroundTemps( 12, 13.0 )

			{}

		static std::shared_ptr< SiteFCFactorMethodGroundTemps > FCFactorGTMFactory( 
			int objectType, 
			std::string objectName
		);

		Real64
		getGroundTemp();

		Real64
		getGroundTempAtTimeInSeconds(
			Real64 const depth,
			Real64 const timeInSecondsOfSim
		);

		Real64
		getGroundTempAtTimeInMonths(
			Real64 const depth,
			int const monthOfSim
		);

	};

}

#endif
