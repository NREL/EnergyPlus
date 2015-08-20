#ifndef GroundTempManager_hh_INCLUDED
#define GroundTempManager_hh_INCLUDED

// C++ Headers
#include <memory>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace GroundTemps {

	//******************************************************************************

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
			Real64 baseConductivity;
			Real64 baseDensity;
			Real64 baseSpecificHeat;
			int totalNumCells;
			Real64 timeStepInSeconds;
			Real64 evapotransCoeff;
			Real64 saturatedWaterContent; 
			Real64 waterContent;
			Real64 annualAveAirTemp;
			Real64 depth;
			Real64 simTimeInDays;

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
				Real64 dewPointTemp;
				Real64 barometricPressure;
				Real64 airDensity;
				Real64 relativeHumidity;
				Real64 windSpeed;
				Real64 horizontalRadiation;
			};

		Array1D< instanceOfWeatherData > weatherDataArray;

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

		static std::shared_ptr< FiniteDiffGroundTempsModel > FiniteDiffGTMFactory( int objectType, std::string objectName);

		enum surfaceTypes {
			surfaceCoverType_bareSoil = 1,
			surfaceCoverType_shortGrass = 2,
			surfaceCoverType_longGrass = 3
		};

	};

	//******************************************************************************
	
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

		static std::shared_ptr< ShallowGroundTemps > ShallowGTMFactory(); 

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

		static std::shared_ptr< BuildingSurfaceGroundTemps > BuildingSurfaceGTMFactory(); 

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

		static std::shared_ptr< FCFactorGroundTemps > FCFactorGTMFactory(); 

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

		static std::shared_ptr< DeepGroundTemps > DeepGTMFactory(); 

	};

	//******************************************************************************

	void
	clear_state();

	//******************************************************************************

	std::shared_ptr< BaseGroundTempsModel >
	GetGroundTempModelAndInit(
		std::string const type,
		std::string const name,
		Real64 const groundThermalDiffusivity
	);

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

}	// GroundTemps

}	// EnergyPlus

#endif
