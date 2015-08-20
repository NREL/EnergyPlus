#ifndef SiteGroundTemperatures_hh_INCLUDED
#define SiteGroundTemperatures_hh_INCLUDED

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
	class ShallowGroundTemps : public BaseGroundTempsModel
	{
		public:
			int timeOfSimInMonths;
			Array1D< Real64 > surfaceGroundTemps;

		// Default Constructor
		ShallowGroundTemps():
			timeOfSimInMonths( 0 ),
			surfaceGroundTemps( 12, 13.0 )

			{}

		static std::shared_ptr< ShallowGroundTemps > ShallowGTMFactory();

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

	//******************************************************************************

	// Derived class for Site:GroundTemperature:BuildingSurface
	class BuildingSurfaceGroundTemps : public BaseGroundTempsModel
	{
		public:
			int timeOfSimInMonths;
			Array1D< Real64 > buildingSurfaceGroundTemps;

		// Default Constructor
		BuildingSurfaceGroundTemps():
			timeOfSimInMonths( 0 ),
			buildingSurfaceGroundTemps( 12, 13.0 )

			{}

		static std::shared_ptr< BuildingSurfaceGroundTemps > BuildingSurfaceGTMFactory();

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

	//******************************************************************************

	// Derived class for Site:GroundTemperature:FCFactorMethod
	class FCFactorGroundTemps : public BaseGroundTempsModel
	{
		public:
			int timeOfSimInMonths;
			Array1D< Real64 > fcFactorGroundTemps;

		// Default Constructor
		FCFactorGroundTemps():
			timeOfSimInMonths( 0 ),
			fcFactorGroundTemps( 12, 13.0 )

			{}

		static std::shared_ptr< FCFactorGroundTemps > FCFactorGTMFactory();

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
	
	//******************************************************************************

	// Derived class for Site:GroundTemperature:Deep
	class DeepGroundTemps : public BaseGroundTempsModel
	{
		public:
			int timeOfSimInMonths;
			Array1D< Real64 > deepGroundTemps;

		// Default Constructor
		DeepGroundTemps():
			timeOfSimInMonths( 0 ),
			deepGroundTemps( 12, 13.0 )

			{}

		static std::shared_ptr< DeepGroundTemps > DeepGTMFactory();

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

}

#endif
