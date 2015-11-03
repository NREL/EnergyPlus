#ifndef SiteBuildingSurfaceGroundTemperatures_hh_INCLUDED
#define SiteBuildingSurfaceGroundTemperatures_hh_INCLUDED

// C++ Headers
#include <memory>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <GroundTemperatureModeling/BaseGroundTemperatureModel.hh>

namespace EnergyPlus {

	// Derived class for Site:GroundTemperature:BuildingSurface
	class SiteBuildingSurfaceGroundTemps : public BaseGroundTempsModel
	{
		public:
			int timeOfSimInMonths;
			Array1D< Real64 > buildingSurfaceGroundTemps;

		// Default Constructor
		SiteBuildingSurfaceGroundTemps():
			timeOfSimInMonths( 0 ),
			buildingSurfaceGroundTemps( 12, 13.0 )

			{}

		static std::shared_ptr< SiteBuildingSurfaceGroundTemps > BuildingSurfaceGTMFactory( 
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
