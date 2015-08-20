// C++ Headers
#include <memory>


// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

	class XingGroundTemps : public BaseGroundTempsModel
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

		static std::shared_ptr< XingGroundTemps > XingGTMFactory();

	};
}
