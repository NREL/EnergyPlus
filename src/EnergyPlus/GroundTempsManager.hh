#ifndef GroundTempManager_hh_INCLUDED
#define GroundTempManager_hh_INCLUDED

// C++ Headers
#include <memory>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace GroundTemps {

	// Base class
	class BaseGroundTempsModel
	{
		public:
			// Public Members
			int objectType;
			std::string objectName;
			bool errorsFound;

			// Default Constructor
		BaseGroundTempsModel() :
			objectType( 0 ),
			errorsFound( false )
			{}
		
		// Virtual method for retrieving the ground temp
		virtual Real64
		getGroundTemp()=0;

		virtual Real64
		getGroundTempAtTimeInSeconds(
			Real64 const,
			Real64 const
		)=0;

		virtual Real64
		getGroundTempAtTimeInMonths(
			Real64 const,
			int const
		)=0;

	};

	//******************************************************************************

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

		static std::shared_ptr< KusudaGroundTempsModel > KusudaGTMFactory( int objectType, std::string objectName );

		static std::shared_ptr< KusudaGroundTempsModel > ShallowGTMFactory( int objectType, std::string objectName ); 

	};

	//******************************************************************************

	// Derived class for Finite-Difference Model
	class FiniteDiffGroundTempsModel : public BaseGroundTempsModel {
		
		public:
			// Public Members

		struct Cell {
			Real64 Density;
			Real64 SpecificHeat;
			Real64 Conductivity;
			Real64 Thickness;
			Real64 Depth;
			Real64 Temp;

			void EvaluateCellTemp();

			void UpdateCellProps();

			void CalcCellResistance();
		};

		struct Canopy {
			int CanopyType;
			Real64 VegitationDensity;

			void CalcCanopyHT();
		};

		struct WeatherData {
			struct SingleWeatherDataPoint {
				Real64 DryBulbTemp;
				Real64 WetBulbTemp;
				Real64 WindSpeed;
			};

			Array1D< Real64 > SingleWeatherDataPoint;

			void GetWeatherData();
	
		};

		struct GroundSurface {
			Real64 Qnet;
			Real64 SurfaceArea;
			bool SnowCover;

			void EvaluateGroundSurfHT();
		};

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

		static std::shared_ptr< FiniteDiffGroundTempsModel > FiniteDiffGTMFactory( int objectType, std::string objectName);
	};

	//******************************************************************************
	
	// Derived class for Site:GroundTemperature:Shallow
	class ShallowGroundTemps : public BaseGroundTempsModel
	{
		public:
			// Public Members
			//Real64 aveGroundTemp;
			//Real64 aveGroundTempAmplitude;
			//Real64 phaseShiftInSecs;
			int timeOfSimInMonths;
			Array1D< Real64 > surfaceGroundTemps;


		// Default Constructor
		ShallowGroundTemps():
			//aveGroundTemp( 15 ),
			//aveGroundTempAmplitude( 12 ),
			//phaseShiftInSecs( 0 ),
			timeOfSimInMonths( 0 ),
			surfaceGroundTemps( 12, 13.0 )

			{}

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

		static std::shared_ptr< ShallowGroundTemps > ShallowGTMFactory( int objectType, std::string objectName ); 

	};

	//******************************************************************************

	std::shared_ptr< BaseGroundTempsModel >
	GetGroundTempModelAndInit(
		std::string const type,
		std::string const name,
		Real64 const groundThermalDiffusivity
	);

	//******************************************************************************

	//     NOTICE

	//     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
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

}	// GroundTemps

}	// EnergyPlus

#endif
