// C++ Headers
#include<memory>
#include<vector>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <GroundTemperatureModeling/BaseGroundTemperatureModel.hh>
#include <GroundTemperatureModeling/FiniteDifferenceGroundTemperatureModel.hh>
#include <GroundTemperatureModeling/GroundTemperatureModelManager.hh>
#include <GroundTemperatureModeling/KusudaAchenbachGroundTemperatureModel.hh>
#include <GroundTemperatureModeling/SiteBuildingSurfaceGroundTemperatures.hh>
#include <GroundTemperatureModeling/SiteDeepGroundTemperatures.hh>
#include <GroundTemperatureModeling/SiteFCFactorMethodGroundTemperatures.hh>
#include <GroundTemperatureModeling/SiteShallowGroundTemperatures.hh>
#include <GroundTemperatureModeling/XingGroundTemperatureModel.hh>
#include <InputProcessor.hh>

namespace EnergyPlus {

namespace GroundTemperatureManager {

	int const objectType_KusudaGroundTemp( 1 );
	int const objectType_FiniteDiffGroundTemp( 2 );
	int const objectType_SiteBuildingSurfaceGroundTemp( 3 );
	int const objectType_SiteShallowGroundTemp( 4 );
	int const objectType_SiteDeepGroundTemp( 5 );
	int const objectType_SiteFCFactorMethodGroundTemp( 6 );
	int const objectType_XingGroundTemp( 7 );

	Array1D_string const CurrentModuleObjects( 7, { "Site:Groundtemperature:Undisturbed:KusudaAchenbach", "Site:GroundTemperature:Undisturbed:FiniteDifference", "Site:GroundTemperature:BuildingSurface", "Site:GroundTemperature:Shallow", "Site:GroundTemperature:Deep", "Site:GroundTemperature:FCfactorMethod", "Site:GroundTemperature:Undisturbed:Xing"} );

	std::vector < std::shared_ptr < BaseGroundTempsModel > > groundTempModels;

	//******************************************************************************

	std::shared_ptr< BaseGroundTempsModel >
	GetGroundTempModelAndInit(
		std::string const objectType_str,
		std::string const objectName
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Matt Mitchell
		//       DATE WRITTEN   Summer 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Called by objects requireing ground temperature models. Determines type and calls appropriate factory method.

		// USE STATEMENTS:
		using InputProcessor::MakeUPPERCase;

		// Locals
		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int objectType( 0 );

		std::string objectType_str_UPPERCase = MakeUPPERCase( objectType_str );
	
		// Set object type
		if ( objectType_str_UPPERCase == MakeUPPERCase( CurrentModuleObjects( objectType_KusudaGroundTemp ) ) ) {
			objectType = objectType_KusudaGroundTemp;
		} else if ( objectType_str_UPPERCase == MakeUPPERCase( CurrentModuleObjects( objectType_FiniteDiffGroundTemp ) ) ) {
			objectType = objectType_FiniteDiffGroundTemp;
		} else if ( objectType_str_UPPERCase == MakeUPPERCase( CurrentModuleObjects( objectType_SiteBuildingSurfaceGroundTemp ) ) ) {
			objectType = objectType_SiteBuildingSurfaceGroundTemp;
		} else if ( objectType_str_UPPERCase == MakeUPPERCase( CurrentModuleObjects( objectType_SiteShallowGroundTemp ) ) ){
			objectType = objectType_SiteShallowGroundTemp;
		} else if ( objectType_str_UPPERCase == MakeUPPERCase( CurrentModuleObjects( objectType_SiteDeepGroundTemp ) ) ) {
			objectType = objectType_SiteDeepGroundTemp;
		} else if ( objectType_str_UPPERCase == MakeUPPERCase( CurrentModuleObjects( objectType_SiteFCFactorMethodGroundTemp ) ) ) {
			objectType = objectType_SiteFCFactorMethodGroundTemp;
		} else if (objectType_str_UPPERCase == MakeUPPERCase( CurrentModuleObjects( objectType_XingGroundTemp ) ) ) {
			objectType = objectType_XingGroundTemp;
		} else {
			// Error out if no ground temperature object types recognized
			ShowFatalError( "GetGroundTempsModelAndInit: Ground temperature object " + objectType_str + " not recognized." );
		}

		int numGTMs = groundTempModels.size();

		// Check if this instance of this model has already been retrieved
		for ( int i = 0; i < numGTMs; ++i ) {
			auto currentModel( groundTempModels[i] );
			// Check if the type and name match
			if ( objectType == currentModel->objectType && objectName == currentModel->objectName) {
				return groundTempModels[i];
			}
		}

		// If not found, create new instance of the model
		if ( objectType == objectType_KusudaGroundTemp ) {
			return KusudaGroundTempsModel::KusudaGTMFactory( objectType, objectName );
		} else if ( objectType == objectType_FiniteDiffGroundTemp ) {
			return FiniteDiffGroundTempsModel::FiniteDiffGTMFactory( objectType, objectName );
		} else if ( objectType == objectType_SiteBuildingSurfaceGroundTemp ) {
			return SiteBuildingSurfaceGroundTemps::BuildingSurfaceGTMFactory( objectType, objectName );
		} else if ( objectType == objectType_SiteShallowGroundTemp ) {
			return SiteShallowGroundTemps::ShallowGTMFactory( objectType, objectName );
		} else if ( objectType == objectType_SiteDeepGroundTemp ) {
			return SiteDeepGroundTemps::DeepGTMFactory( objectType, objectName );
		} else if ( objectType == objectType_SiteFCFactorMethodGroundTemp ) {
			return SiteFCFactorMethodGroundTemps::FCFactorGTMFactory( objectType, objectName );
		} else if ( objectType == objectType_XingGroundTemp ) {
			return XingGroundTempsModel::XingGTMFactory( objectType, objectName );
		} else {
			// Error
			return nullptr; 
		}
	}

	//******************************************************************************

	void
	clear_state() {
		groundTempModels.clear();
	}

	//******************************************************************************

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
