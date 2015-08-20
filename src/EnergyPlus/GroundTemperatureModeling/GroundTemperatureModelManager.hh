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

	static std::vector < std::shared_ptr < BaseGroundTempsModel > > groundTempModels;

	std::shared_ptr< BaseGroundTempsModel >
	GetGroundTempModelAndInit(
		std::string const type,
		std::string const name,
		Real64 const groundThermalDiffusivity
	);

	void
	clear_state();

	//     NOTICE

	//     Copyright (c) 1996-2015 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.

	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in main.cc.

	//     NOTICE: The U.S. Government is granted for itself and others acting on its
	//     behalf a paid-up, nonexclusive, irrevocable, worldwide license in this data to
	//     reproduce, prepare derivative works, and perform publicly and display publicly.
	//     Beginning five (5) years after permission to assert copyright is granted,
	//     subject to two possible five year renewals, the U.S. Government is granted for
	//     itself and others acting on its behalf a paid-up, non-exclusive, irrevocable
	//     worldwide license in this data to reproduce, prepare derivative works,
	//     distribute copies to the public, perform publicly and display publicly, and to
	//     permit others to do so.

	//     TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.

}	// GroundTemperatureManager

}	// EnergyPlus

#endif
