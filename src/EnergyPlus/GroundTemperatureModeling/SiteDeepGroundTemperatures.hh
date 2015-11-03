#ifndef SiteDeepGroundTemperatures_hh_INCLUDED
#define SiteDeepGroundTemperatures_hh_INCLUDED

// C++ Headers
#include <memory>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <GroundTemperatureModeling/BaseGroundTemperatureModel.hh>

namespace EnergyPlus {

	// Derived class for Site:GroundTemperature:Deep
	class SiteDeepGroundTemps : public BaseGroundTempsModel
	{
		public:
			int timeOfSimInMonths;
			Array1D< Real64 > deepGroundTemps;

		// Default Constructor
		SiteDeepGroundTemps():
			timeOfSimInMonths( 0 ),
			deepGroundTemps( 12, 13.0 )

			{}

		static std::shared_ptr< SiteDeepGroundTemps > DeepGTMFactory( 
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
