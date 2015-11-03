#ifndef XingGroundTemperatureManager_hh_INCLUDED
#define XingGroundTemperatureManager_hh_INCLUDED

// C++ Headers
#include <memory>

// EnergyPlus Headers
#include <GroundTemperatureModeling/BaseGroundTemperatureModel.hh>
#include <EnergyPlus.hh>

namespace EnergyPlus {

	class XingGroundTempsModel : public BaseGroundTempsModel
	{
		public:
			Real64 depth;
			Real64 groundThermalDiffisivity;
			Real64 simTimeInDays;
			Real64 aveGroundTemp;
			Real64 surfTempAmplitude_1;
			Real64 phaseShift_1;
			Real64 surfTempAmplitude_2;
			Real64 phaseShift_2;

		static std::shared_ptr< XingGroundTempsModel > 
		XingGTMFactory( 
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
