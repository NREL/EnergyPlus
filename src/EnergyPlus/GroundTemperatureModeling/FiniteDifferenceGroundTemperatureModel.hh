#ifndef FiniteDifferenceGroundTemperatureModel_hh_INCLUDED
#define FiniteDifferenceGroundTemperatureModel_hh_INCLUDED

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

	// Derived class for Finite-Difference Model
	class FiniteDiffGroundTempsModel : public BaseGroundTempsModel {
		
		public:
			Real64 baseConductivity;
			Real64 baseDensity;
			Real64 baseSpecificHeat;
			int totalNumCells;
			Real64 timeStepInSeconds;
			Real64 evapotransCoeff;
			Real64 saturatedWaterContent; 
			Real64 waterContent;
			Real64 annualAveAirTemp;
			Real64 minDailyAirTemp; // Set hi. Will be reset later
			Real64 maxDailyAirTemp; // Set low. Will be reset later
			Real64 dayOfMinDailyAirTemp;
			Real64 depth;
			Real64 simTimeInDays;

		// Default constructor
		FiniteDiffGroundTempsModel() :
			minDailyAirTemp( 100.0 ),
			maxDailyAirTemp( -100.0 ),
			dayOfMinDailyAirTemp( 1 )

		{}

		struct instanceOfCellData {

			struct properties
			{
				Real64 conductivity;
				Real64 density;
				Real64 specificHeat;
				Real64 diffusivity;
				Real64 rhoCp;
			};

			properties props;
		
			int index;
			Real64 thickness;
			Real64 minZValue;
			Real64 maxZValue;
			Real64 temperature;
			Real64 temperature_prevIteration;
			Real64 temperature_prevTimeStep;
			Real64 temperature_finalConvergence;
			Real64 beta;
			Real64 volume;
			Real64 conductionArea = 1.0; // Assumes 1 m2 
		
			};

		Array1D< instanceOfCellData > cellArray;

		struct instanceOfWeatherData
			{
				Real64 dryBulbTemp;
				Real64 relativeHumidity;
				Real64 windSpeed;
				Real64 horizontalRadiation;
				Real64 airDensity;
			};

		Array1D< instanceOfWeatherData > weatherDataArray;

		static std::shared_ptr< FiniteDiffGroundTempsModel > 
		FiniteDiffGTMFactory(
			int objectType,
			std::string objectName
		);

		void
		getWeatherData();

		void
		initAndSim();

		void
		developMesh();

		void
		performSimulation();

		void
		updateSurfaceCellTemperature();

		void
		updateGeneralDomainCellTemperature(
			int const cell
		);

		void
		updateBottomCellTemperature();

		void
		initDomain();

		bool
		checkFinalTemperatureConvergence();

		bool
		checkIterationTemperatureConvergence();

		void
		updateIterationTemperatures();

		void
		updateTimeStepTemperatures();

		void
		doStartOfTimeStepInits();

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

		void
		evaluateSoilRhoCp(
			Optional< int const > cell = _,
			Optional_bool_const InitOnly = _
		);

		Real64
		interpolate(
			Real64 const x,
			Real64 const x_hi,
			Real64 const x_low,
			Real64 const y_hi,
			Real64 const y_low
		);

		Array2D< Real64 > groundTemps;

		Array1D< Real64 > cellDepths;

		enum surfaceTypes {
			surfaceCoverType_bareSoil = 1,
			surfaceCoverType_shortGrass = 2,
			surfaceCoverType_longGrass = 3
		};

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
