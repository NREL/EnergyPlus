#ifndef GroundTemperatureModelManager_hh_INCLUDED
#define GroundTemperatureModelManager_hh_INCLUDED

// C++ Headers
#include <memory>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <GroundTemperatureModeling/BaseGroundTemperatureModel.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace GroundTemperatureManager {

	extern int const objectType_KusudaGroundTemp;
	extern int const objectType_FiniteDiffGroundTemp;
	extern int const objectType_SiteBuildingSurfaceGroundTemp;
	extern int const objectType_SiteShallowGroundTemp;
	extern int const objectType_SiteDeepGroundTemp;
	extern int const objectType_SiteFCFactorMethodGroundTemp;
	extern int const objectType_XingGroundTemp;

	extern Array1D_string const CurrentModuleObjects;

	extern std::vector < std::shared_ptr < BaseGroundTempsModel > > groundTempModels;

	std::shared_ptr< BaseGroundTempsModel >
	GetGroundTempModelAndInit(
		std::string const type,
		std::string const name
	);

	void
	clear_state();

}	// GroundTemperatureManager

}	// EnergyPlus

#endif
