#ifndef SiteShallowGroundTemperatures_hh_INCLUDED
#define SiteShallowGroundTemperatures_hh_INCLUDED

// C++ Headers
#include <memory>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <GroundTemperatureModeling/BaseGroundTemperatureModel.hh>

namespace EnergyPlus {

	// Derived class for Site:GroundTemperature:Shallow
	class SiteShallowGroundTemps : public BaseGroundTempsModel
	{
		public:
			int timeOfSimInMonths;
			Array1D< Real64 > surfaceGroundTemps;

		// Default Constructor
		SiteShallowGroundTemps():
			timeOfSimInMonths( 0 ),
			surfaceGroundTemps( 12, 13.0 )

			{}

		static std::shared_ptr< SiteShallowGroundTemps > ShallowGTMFactory( 
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
