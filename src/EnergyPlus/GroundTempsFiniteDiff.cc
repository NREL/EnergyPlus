// C++ Headers
#include <memory>
#include <fstream> 

// ObjexxFCL Headers

// EnergyPlus Headers
#include <DataGlobals.hh>
#include <DataReportingFlags.hh>
#include <GroundTempsManager.hh>
#include <WeatherManager.hh>

namespace EnergyPlus {

namespace GroundTemps {

	void
	FiniteDiffGroundTempsModel::initModel()
	{

		FiniteDiffGroundTempsModel::getWeatherData();

		FiniteDiffGroundTempsModel::developMesh();

		FiniteDiffGroundTempsModel::performSimulation();
	
	}

	//******************************************************************************

	void
	FiniteDiffGroundTempsModel::getWeatherData()
	{
	
		using WeatherManager::AddDesignSetToEnvironmentStruct;
		using WeatherManager::GetNextEnvironment;
		using WeatherManager::ManageWeather;
		using WeatherManager::ResetEnvironmentCounter;

		using namespace DataGlobals;
		using namespace DataReportingFlags;

		bool Available; // an environment is available to process
		bool ErrorsFound;

		ResetEnvironmentCounter();

		AddDesignSetToEnvironmentStruct( 1 );

		WarmupFlag = false;
		Available = true;
		//for ( int i = 1; i <= NumOfEnvrn; ++i ) { // loop over environments

		GetNextEnvironment( Available, ErrorsFound );

		//if (ErrorsFound) break;
		//if (!Available) continue;

		// need to handle above two cases

		//hvacSizingSimulationManager->sizingLogger.SetupSizingLogsNewEnvironment();

		//	if (!DoDesDaySim) continue; // not sure about this, may need to force users to set this on input for this method, but maybe not
		//	if ( KindOfSim == ksRunPeriodWeather ) continue;
		//	if ( KindOfSim == ksDesignDay ) continue;
		//	if ( KindOfSim == ksRunPeriodDesign ) continue;

		//	if ( Environment(Envrn).HVACSizingIterationNum != HVACSizingIterCount ) continue;

		//	if ( ReportDuringHVACSizingSimulation ) {
		//		if ( sqlite ) {
		//			sqlite->sqliteBegin();
		//			sqlite->createSQLiteEnvironmentPeriodRecord( DataEnvironment::CurEnvirNum, DataEnvironment::EnvironmentName, DataGlobals::KindOfSim );
		//			sqlite->sqliteCommit();
		//		}
		//	}
		//	ExitDuringSimulations = true;

		//	DisplayString("Initializing New Environment Parameters, HVAC Sizing Simulation");

		BeginEnvrnFlag = true;
		EndEnvrnFlag = false;
		//EndMonthFlag = false;
		WarmupFlag = false;
		DayOfSim = 0;
		DayOfSimChr = "0";
		NumOfWarmupDays = 0;

		//	ManageEMS(emsCallFromBeginNewEvironment); // calling point

		while ( ( DayOfSim < NumOfDayInEnvrn ) || ( WarmupFlag ) ) { // Begin day loop ...

		//		if ( ReportDuringHVACSizingSimulation ) {
		//			if ( sqlite ) sqlite->sqliteBegin(); // setup for one transaction per day
		//		}

			++DayOfSim;

		//		gio::write( DayOfSimChr, fmtLD ) << DayOfSim;
		//		strip(DayOfSimChr);
		//		if ( !WarmupFlag ) {
		//			++CurrentOverallSimDay;
		//			DisplaySimDaysProgress(CurrentOverallSimDay, TotalOverallSimDays);
		//		} else {
		//			DayOfSimChr = "0";
		//		}

			BeginDayFlag = true;
			EndDayFlag = false;

		//		if ( WarmupFlag ) {
		//			++NumOfWarmupDays;
		//			cWarmupDay = TrimSigDigits(NumOfWarmupDays);
		//			DisplayString("Warming up {" + cWarmupDay + '}');
		//		} else if (DayOfSim == 1) {
		//			DisplayString("Starting HVAC Sizing Simulation at " + CurMnDy + " for " + EnvironmentName);
		//			gio::write(OutputFileInits, Format_700) << NumOfWarmupDays;
		//		} else if (DisplayPerfSimulationFlag) {
		//			DisplayString("Continuing Simulation at " + CurMnDy + " for " + EnvironmentName);
		//			DisplayPerfSimulationFlag = false;
		//		}

			for ( HourOfDay = 1; HourOfDay <= 24; ++HourOfDay ) { // Begin hour loop ...

		//			BeginHourFlag = true;
		//			EndHourFlag = false;

				for ( TimeStep = 1; TimeStep <= NumOfTimeStepInHour; ++TimeStep ) {
		//				if ( AnySlabsInModel || AnyBasementsInModel ) {
		//					InitAndSimGroundDomains();
		//				}

					BeginTimeStepFlag = true;


		//				 Set the End__Flag variables to true if necessary.  Note that
		//				 each flag builds on the previous level.  EndDayFlag cannot be
		//				 .TRUE. unless EndHourFlag is also .TRUE., etc.  Note that the
		//				 EndEnvrnFlag and the EndSimFlag cannot be set during warmup.
		//				 Note also that BeginTimeStepFlag, EndTimeStepFlag, and the
		//				 SubTimeStepFlags can/will be set/reset in the HVAC Manager.

					if ( TimeStep == NumOfTimeStepInHour ) {
						EndHourFlag = true;
						if ( HourOfDay == 24 ) {
							EndDayFlag = true;
							if ( !WarmupFlag && (DayOfSim == NumOfDayInEnvrn) ) {
								EndEnvrnFlag = true;
							}
						}
					}

					ManageWeather();

					BeginHourFlag = false;
					BeginDayFlag = false;
					BeginEnvrnFlag = false;
					BeginSimFlag = false;
					BeginFullSimFlag = false;

				} // TimeStep loop

				PreviousHour = HourOfDay;

			} // ... End hour loop.

		//		if ( ReportDuringHVACSizingSimulation ) {
		//			if ( sqlite ) sqlite->sqliteCommit(); // one transaction per day
		//		}

		} // ... End day loop.

		//} // ... End environment loop.
	}

	//******************************************************************************

	void
	FiniteDiffGroundTempsModel::developMesh()
	{

		std::ofstream static outFile( "MeshData.csv", std::ofstream::out );
		
		// Surface layer parameters
		Real64 surfaceLayerThickness = 2.0;
		Real64 surfaceLayerCellThickness = 0.015;
		int surfaceLayerNumCells = surfaceLayerThickness / surfaceLayerCellThickness;
		
		// Center layer parameters
		Real64 centerLayerThickness = 17.8;
		Real64 centerLayerExpansionCoeff = 1.10879;
		int centerLayerNumCells = 80; 
		
		// Deep layer parameters
		Real64 deepLayerThickness = 0.2;
		Real64 deepLayerCellThickness = surfaceLayerCellThickness;
		int deepLayerNumCells = deepLayerThickness / deepLayerCellThickness;

		// Other
		Real64 currentCellDepth = 0.0;
		Real64 cellThickness = surfaceLayerCellThickness;

		totalNumCells = surfaceLayerNumCells + centerLayerNumCells + deepLayerNumCells;

		cellArray.allocate( totalNumCells );

		// Setup cells surface layer cells
		for ( int i = 1; i <= totalNumCells; ++i ) {

			// Reference to thisCell
			auto & thisCell( cellArray( i ) );

			// Set the index 
			thisCell.index = i;

			// Give thickness to the cell
			if ( i <= surfaceLayerNumCells ) {				

				// Constant thickness mesh here
				thisCell.thickness = surfaceLayerCellThickness;

			} else if ( i > surfaceLayerNumCells && i <= ( centerLayerNumCells + surfaceLayerNumCells ) ) {

				int numCenterCell = i - surfaceLayerNumCells;

				if ( numCenterCell <= ( centerLayerNumCells / 2 ) ) {
					thisCell.thickness = surfaceLayerCellThickness * std::pow( centerLayerExpansionCoeff, numCenterCell );
				} else {
					thisCell.thickness = cellArray( ( surfaceLayerNumCells + ( centerLayerNumCells / 2 ) ) - ( numCenterCell - ( centerLayerNumCells / 2 ) ) ).thickness;
				}

			} else if ( i > ( centerLayerNumCells + surfaceLayerNumCells ) ) {
				
				// Constant thickness mesh here
				thisCell.thickness = deepLayerCellThickness;
			}

			// Set minimum z value
			thisCell.minZValue = currentCellDepth;

			// Update local counter
			currentCellDepth += thisCell.thickness;

			// Set maximum z value
			thisCell.maxZValue = currentCellDepth;

			// Set base properties
			thisCell.props.conductivity = baseConductivity;
			thisCell.props.density = baseDensity;
			thisCell.props.specificHeat = baseSpecificHeat;
			thisCell.props.diffusivity = baseConductivity / ( baseDensity * baseSpecificHeat );

			outFile << thisCell.index << "," << thisCell.thickness << "," << thisCell.minZValue << "," << thisCell.maxZValue << std::endl;
		}
	}

	//******************************************************************************

	void
	FiniteDiffGroundTempsModel::performSimulation()
	{
	
		int daysInYear = 365;
		bool convergedFinal = false;

		initDomainTemperatures();
		
		do {
		
			// loop over all days
			for ( int day = 1; day <= daysInYear; ++day ) {

				bool iterationConverged = false;

				doStartOfTimeStepInits();

				// Loop until iteration temperature converges
				do {
			
					// For all cells
					for ( int cell = 1; cell <= totalNumCells; ++cell ) {

						if ( cell == 1 ) {
							updateSurfaceCellTemperature();
						} else if ( cell > 1 && cell < totalNumCells ) {
							updateGeneralDomainCellTemperature( cell );
						} else if ( cell == totalNumCells ) {
							updateBottomCellTemperature();
						}
					}

					// Shift temperatures for next iteration
					updateIterationTemperatures();

					// Check iteration temperature convergence
					iterationConverged = checkIterationTemperatureConvergence();

				} while ( !iterationConverged );

				// Shift temperatures for next timestep
				updateTimeStepTemperatures();
			}

			// Check final temperature convergence
			convergedFinal = checkFinalTemperatureConvergence();

		} while ( !convergedFinal );
	}

	//******************************************************************************

	void
	FiniteDiffGroundTempsModel::updateSurfaceCellTemperature()
	{
		cellArray( 1 ).temperature = 20.0; // Just for testing
	}

	//******************************************************************************

	void
	FiniteDiffGroundTempsModel::updateGeneralDomainCellTemperature( 
		Real64 const cell
	)
	{

		// Set up once-per-cell items
		Real64 numerator = 0.0;
		Real64 denominator = 0.0;

		auto & thisCell( cellArray( cell ) );

		// add effect from cell history
		numerator += thisCell.temperature_prevTimeStep;
		++denominator;

		// loop across each direction in the simulation
		for ( int curDirection = 1; curDirection <= 2; ++curDirection ) {

			Real64 neighborTemp = 0.0;
			Real64 resistance = 0.0;

			//'evaluate the transient expression terms
			evaluateNeighborResistance( thisCell, curDirection, neighborTemp, resistance );
			numerator += ( thisCell.beta / resistance ) * neighborTemp;
			denominator += thisCell.beta / resistance;
		}

		//'now that we have passed all directions, update the temperature
		thisCell.temperature = numerator / denominator;

	}

	//******************************************************************************

	void
	FiniteDiffGroundTempsModel::updateBottomCellTemperature()
	{
		cellArray( totalNumCells ).temperature = 15.0; // Just for testing
	}

	//******************************************************************************

	bool
	FiniteDiffGroundTempsModel::checkFinalTemperatureConvergence()
	{
		return true;
	}

	//******************************************************************************

	bool
	FiniteDiffGroundTempsModel::checkIterationTemperatureConvergence()
	{
		return true;
	}

	//******************************************************************************

	void
	FiniteDiffGroundTempsModel::initDomainTemperatures()
	{
		
		using DataGlobals::SecsInDay;

		std::string objectName = "KAModelForFDModel";
		int objectType = 1;
		Real64 avgGroundTemp = 15.5;
		Real64 aveGroundTempAmplitiude = 12.0;
		int phaseShiftDay = 21;
		Real64 groundThemalDiffusivity = 4.0e-7;
		
		std::unique_ptr< KusudaGroundTempsModel > tempModel( new KusudaGroundTempsModel() );

		std::ofstream initTempsFile( "InitTemps.csv", std::ofstream::out );

		tempModel->objectName = objectName;

		tempModel->objectType = objectType;

		tempModel->aveGroundTemp = avgGroundTemp;

		tempModel->aveGroundTempAmplitude = aveGroundTempAmplitiude;

		tempModel->phaseShiftInSecs = phaseShiftDay * SecsInDay;

		tempModel->groundThermalDiffisivity = groundThemalDiffusivity;

		for ( int cell = 1; cell <= totalNumCells; ++cell ) {
			auto & thisCell( cellArray( cell ) );

			Real64 depth = ( thisCell.maxZValue + thisCell.minZValue ) / 2.0;

			thisCell.temperature = tempModel->getGroundTempAtTimeInSeconds( depth, 0.0 );  // Initialized at first day of year

			initTempsFile << thisCell.temperature << std::endl;
		}
	}

	//******************************************************************************

	void
	FiniteDiffGroundTempsModel::updateIterationTemperatures()
	{
		for ( int cell = 1; cell <= totalNumCells; ++cell ) {
			cellArray( cell ).temperature_prevIteration = cellArray( cell ).temperature;
		}
	}

	//******************************************************************************

	void
	FiniteDiffGroundTempsModel::updateTimeStepTemperatures()
	{
		for ( int cell = 1; cell <= totalNumCells; ++cell ) {
			cellArray( cell ).temperature_prevTimeStep = cellArray( cell ).temperature;
		}
	}

	//******************************************************************************

	void
	FiniteDiffGroundTempsModel::doStartOfTimeStepInits()
	{
		for ( int cell = 1; cell <= totalNumCells; ++cell ) {
			
			//inits here

		}
	}

	//******************************************************************************

	void
	FiniteDiffGroundTempsModel::evaluateNeighborResistance(
		cell curCell,
		int const currDirection,
		Real64 neighborTemp,
		Real64 resistance
	)
	{
	
	}

	//******************************************************************************

	Real64
	FiniteDiffGroundTempsModel::getGroundTemp()
	{
		return 0;
	}

	//******************************************************************************

	Real64
	FiniteDiffGroundTempsModel::getGroundTempAtTimeInSeconds(
		Real64 const depth,
		Real64 const simTimeInSeconds
	)
	{
		return 0;
	}

	//******************************************************************************

	Real64
	FiniteDiffGroundTempsModel::getGroundTempAtTimeInMonths(
		Real64 const depth,
		int const simTimeInMonths
	)
	{
		return 0;
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

}	// GroundTemps

}	// EnergyPlus
