#ifndef KusudaAchenbachGroundTemperatureModel_hh_INCLUDED
#define KusudaAchenbachGroundTemperatureModel_hh_INCLUDED

// C++ Headers
#include <memory>

// EnergyPlus Headers
#include <DataGlobals.hh>
#include <EnergyPlus.hh>
#include <GroundTemperatureModeling/BaseGroundTemperatureModel.hh>

namespace EnergyPlus{

	// Derived class for Kusuda-Achenbach model
	class KusudaGroundTempsModel : public BaseGroundTempsModel
	{
		public:
			// Public Members
			Real64 depth;
			Real64 groundThermalDiffisivity;
			Real64 simTimeInSeconds;
			Real64 aveGroundTemp;
			Real64 aveGroundTempAmplitude;
			Real64 phaseShiftInSecs;

		static std::shared_ptr< KusudaGroundTempsModel > 
		KusudaGTMFactory( 
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
