// C++ Headers
#include <memory>
#include <fstream> 

// ObjexxFCL Headers
#include <ObjexxFCL\Optional.hh>

// EnergyPlus Headers
#include <DataGlobals.hh>
#include <DataReportingFlags.hh>
#include <GroundTempsManager.hh>
#include <WeatherManager.hh>

namespace EnergyPlus {

namespace GroundTemps {
	int daysInYear = 365;
	int secInDay = 86400;
	int timeStepInDays = 0;
	Real64 finalTempConvergenceCriteria = 0.01;
	Real64 iterationTempConvergenceCriteria = 0.00001;

	void
	FiniteDiffGroundTempsModel::simulate()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Matt Mitchell
		//       DATE WRITTEN   Summer 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		FiniteDiffGroundTempsModel::getWeatherData();

		FiniteDiffGroundTempsModel::developMesh();

		FiniteDiffGroundTempsModel::performSimulation();
	
	}

	//******************************************************************************

	void
	FiniteDiffGroundTempsModel::getWeatherData()
	{
	
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Matt Mitchell
		//       DATE WRITTEN   Summer 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

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

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Matt Mitchell
		//       DATE WRITTEN   Summer 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

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

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Matt Mitchell
		//       DATE WRITTEN   Summer 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		std::ofstream static outFile( "FinalTemps.csv", std::ofstream::out );
		std::ofstream static outFile_timeStep( "TimeStepTemps.csv", std::ofstream::out );
		bool convergedFinal = false;

		initDomain();
		
		do {
		
			// loop over all days
			for ( timeStepInDays = 1; timeStepInDays <= daysInYear; ++timeStepInDays ) {

				timeStepInSeconds = 86400; // Seconds in day

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

						// Check iteration temperature convergence
						iterationConverged = checkIterationTemperatureConvergence();

						if ( !iterationConverged ) {
							// Shift temperatures for next iteration
							updateIterationTemperatures();
						}
					
					} while ( !iterationConverged );

					// Shift temperatures for next timestep
					updateTimeStepTemperatures();

				//// Output timestep temps for testing
				//for ( int cell = 1; cell <= totalNumCells; ++cell ) {
				//	if ( cell == totalNumCells ) {
				//		outFile_timeStep << groundTemps( timeStepInDays, cell ) << std::endl;
				//	} else {
				//		outFile_timeStep << groundTemps( timeStepInDays, cell ) << "," ;
				//	}

				//}

			}

			// Check final temperature convergence
			convergedFinal = checkFinalTemperatureConvergence();

		} while ( !convergedFinal );

		//// Output final annual temps for testing
		//for ( int cell = 1; cell <= totalNumCells; ++cell ) {

		//	outFile << cellArray( cell ).minZValue;

		//	for ( int day = 1; day <= daysInYear; ++ day ) {
		//		if ( day < daysInYear ) {
		//			outFile << "," << groundTemps( day, cell );
		//		} else {
		//			outFile << "," << groundTemps( day, cell ) << "," << std::endl;
		//		}
		//	}
		//}
	}

	//******************************************************************************

	void
	FiniteDiffGroundTempsModel::updateSurfaceCellTemperature()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Matt Mitchell
		//       DATE WRITTEN   Summer 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		cellArray( 1 ).temperature = 20.0; // Just for testing
	}

	//******************************************************************************

	void
	FiniteDiffGroundTempsModel::updateGeneralDomainCellTemperature( 
		int const cell
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Matt Mitchell
		//       DATE WRITTEN   Summer 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// Set up once-per-cell items
		Real64 numerator = 0.0;
		Real64 denominator = 0.0;
		Real64 neighborTemp = 0.0;
		Real64 resistance = 0.0;

		auto & thisCell( cellArray( cell ) );
		auto & cellAbove_thisCell( cellArray( cell - 1 ) );
		auto & cellBelow_thisCell( cellArray( cell + 1 ) );

		// add effect from cell history
		numerator += thisCell.temperature_prevTimeStep;
		++denominator;			

		// Conduction resistance between this cell and above cell
		resistance = ( ( thisCell.thickness / 2.0 ) / ( thisCell.conductionArea * thisCell.props.conductivity ) ) 
					+ ( ( cellAbove_thisCell.thickness / 2.0 ) / ( cellAbove_thisCell.conductionArea * cellAbove_thisCell.props.conductivity ) );

		numerator += ( thisCell.beta / resistance ) * cellAbove_thisCell.temperature;
		denominator += thisCell.beta / resistance;

		// Conduction resitance between this cell and below cell
		resistance = ( ( thisCell.thickness / 2.0 ) / ( thisCell.conductionArea * thisCell.props.conductivity ) ) 
					+ ( ( cellBelow_thisCell.thickness / 2.0 ) / ( cellBelow_thisCell.conductionArea * cellBelow_thisCell.props.conductivity ) );

		numerator += ( thisCell.beta / resistance ) * cellBelow_thisCell.temperature;
		denominator += thisCell.beta / resistance;

		//'now that we have passed all directions, update the temperature
		thisCell.temperature = numerator / denominator;

	}

	//******************************************************************************

	void
	FiniteDiffGroundTempsModel::updateBottomCellTemperature()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Matt Mitchell
		//       DATE WRITTEN   Summer 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		cellArray( totalNumCells ).temperature = 15.0; // Just for testing
	}

	//******************************************************************************

	bool
	FiniteDiffGroundTempsModel::checkFinalTemperatureConvergence()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Matt Mitchell
		//       DATE WRITTEN   Summer 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		bool converged = true;

		for ( int cell = 1; cell <= totalNumCells; ++ cell ) {
		
			auto & thisCell( cellArray( cell ) );

			if ( std::abs( thisCell.temperature - thisCell.temperature_finalConvergence ) >= finalTempConvergenceCriteria ) {
				converged = false;
			}

			thisCell.temperature_finalConvergence = thisCell.temperature;
		}

		return converged;
	}

	//******************************************************************************

	bool
	FiniteDiffGroundTempsModel::checkIterationTemperatureConvergence()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Matt Mitchell
		//       DATE WRITTEN   Summer 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		bool converged = true;

		for ( int cell = 1; cell <= totalNumCells; ++ cell ) {
		
			if ( std::abs( cellArray( cell ).temperature - cellArray( cell ).temperature_prevIteration ) >= iterationTempConvergenceCriteria ) {
				converged = false;
				break;
			}
		}

		return converged;
	}

	//******************************************************************************

	void
	FiniteDiffGroundTempsModel::initDomain()
	{
		
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Matt Mitchell
		//       DATE WRITTEN   Summer 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

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

		// Intialize temperatures and volume
		for ( int cell = 1; cell <= totalNumCells; ++cell ) {
			auto & thisCell( cellArray( cell ) );
			
			Real64 depth = ( thisCell.maxZValue + thisCell.minZValue ) / 2.0;
			
			// Initialize temperatures
			thisCell.temperature = 15.0; //tempModel->getGroundTempAtTimeInSeconds( depth, 0.0 );  // Initialized at first day of year
			thisCell.temperature_finalConvergence = thisCell.temperature;
			thisCell.temperature_prevIteration = thisCell.temperature;
			thisCell.temperature_prevTimeStep = thisCell.temperature;

			// Set cell volume
			thisCell.volume = thisCell.thickness * thisCell.conductionArea;

			// Delete me
			initTempsFile << thisCell.temperature << std::endl;
		}

		// Initialize freezing calculation variables
		evaluateSoilRhoCp( _, true );

		// Initialize the groundTemps array
		groundTemps.dimension( { 0, daysInYear }, { 0, totalNumCells }, 0.0 );

	}

	//******************************************************************************

	void
	FiniteDiffGroundTempsModel::updateIterationTemperatures()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Matt Mitchell
		//       DATE WRITTEN   Summer 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		for ( int cell = 1; cell <= totalNumCells; ++cell ) {
			cellArray( cell ).temperature_prevIteration = cellArray( cell ).temperature;
		}
	}

	//******************************************************************************

	void
	FiniteDiffGroundTempsModel::updateTimeStepTemperatures()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Matt Mitchell
		//       DATE WRITTEN   Summer 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		for ( int cell = 1; cell <= totalNumCells; ++cell ) {

			auto & thisCell( cellArray( cell ) );

			thisCell.temperature_prevTimeStep = thisCell.temperature;

			// Log temps for later use
			groundTemps( timeStepInDays, cell ) = thisCell.temperature;
		}
	}

	//******************************************************************************

	void
	FiniteDiffGroundTempsModel::doStartOfTimeStepInits()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Matt Mitchell
		//       DATE WRITTEN   Summer 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		for ( int cell = 1; cell <= totalNumCells; ++cell ) {
			
			auto & thisCell( cellArray( cell ) );

			evaluateSoilRhoCp( cell );

			thisCell.beta = ( timeStepInSeconds / ( thisCell.props.rhoCp * thisCell.volume ) );

		}
	}

	//******************************************************************************

	Real64
	FiniteDiffGroundTempsModel::getGroundTemp()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Matt Mitchell
		//       DATE WRITTEN   Summer 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		return 0;
	}

	//******************************************************************************

	Real64
	FiniteDiffGroundTempsModel::getGroundTempAtTimeInSeconds(
		Real64 const depth,
		Real64 const simTimeInSeconds
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Matt Mitchell
		//       DATE WRITTEN   Summer 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		return 0;
	}

	//******************************************************************************

	Real64
	FiniteDiffGroundTempsModel::getGroundTempAtTimeInMonths(
		Real64 const depth,
		int const simTimeInMonths
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Matt Mitchell
		//       DATE WRITTEN   Summer 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		return 0;
	}

	//******************************************************************************
	
	void
	FiniteDiffGroundTempsModel::evaluateSoilRhoCp(
		Optional< int const > cell,
		Optional_bool_const InitOnly
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       Matt Mitchell, Summer 2015
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//'static variables only calculated once per simulation run
		static Real64 Theta_ice;
		static Real64 Theta_liq;
		static Real64 Theta_sat;
		static Real64 rho_ice;
		static Real64 rho_liq;
		static Real64 rhoCp_soil_liq_1;
		static Real64 CP_liq;
		static Real64 CP_ice;
		static Real64 Lat_fus;
		static Real64 Cp_transient;
		static Real64 rhoCP_soil_liq;
		static Real64 rhoCP_soil_transient;
		static Real64 rhoCP_soil_ice;
		// other variables
		Real64 frzAllIce;
		Real64 frzIceTrans;
		Real64 frzLiqTrans;
		Real64 frzAllLiq;
		Real64 rhoCP_soil;

		// These vary by domain now, so we must be careful to retrieve them every time
		Theta_liq = baseMoistureContent;
		Theta_sat = baseMoistureContentAtSaturation;

		// Assumption
		Theta_ice = Theta_liq;

		if ( present( InitOnly ) ) {
			//'Cp (freezing) calculations
			rho_ice = 917.0; //'Kg / m3
			rho_liq = 1000.0; //'kg / m3
			rhoCp_soil_liq_1 = 1225000.0 / ( 1.0 - Theta_sat ); //'J/m3K
			//'from( " An improved model for predicting soil thermal conductivity from water content at room temperature, Fig 4" )
			CP_liq = 4180.0; //'J / KgK
			CP_ice = 2066.0; //'J / KgK
			Lat_fus = 334000.0; //'J / Kg
			Cp_transient = Lat_fus / 0.4 + ( 0.5 * CP_ice - ( CP_liq + CP_ice ) / 2.0 * 0.1 ) / 0.4;
			//'from( " Numerical and experimental investigation of melting and freezing processes in phase change material storage" )
			rhoCP_soil_liq = rhoCp_soil_liq_1 * ( 1.0 - Theta_sat ) + rho_liq * CP_liq * Theta_liq;
			rhoCP_soil_transient = rhoCp_soil_liq_1 * ( 1.0 - Theta_sat ) + ( ( rho_liq + rho_ice ) / 2.0 ) * Cp_transient * Theta_ice;
			rhoCP_soil_ice = rhoCp_soil_liq_1 * ( 1.0 - Theta_sat ) + rho_ice * CP_ice * Theta_ice; //'!J / m3K
			return;
		}

		auto & thisCell( cellArray( cell ) );

		//'set some temperatures here for generalization -- these could be set in the input file
		frzAllIce = -0.5;
		frzIceTrans = -0.4;
		frzLiqTrans = -0.1;
		frzAllLiq = 0.0;

		//'calculate this cell's new Cp value based on the cell temperature
		if ( thisCell.temperature >= frzAllLiq ) {
			rhoCP_soil = rhoCp_soil_liq_1;
		} else if ( thisCell.temperature <= frzAllIce ) {
			rhoCP_soil = rhoCP_soil_ice;
		} else if ( ( thisCell.temperature < frzAllLiq ) && ( thisCell.temperature > frzLiqTrans ) ) {
			rhoCP_soil = rhoCp_soil_liq_1 + ( rhoCP_soil_transient - rhoCP_soil_liq ) / ( frzAllLiq - frzLiqTrans ) * ( frzAllLiq - thisCell.temperature );
		} else if ( ( thisCell.temperature <= frzLiqTrans ) && ( thisCell.temperature >= frzIceTrans ) ) {
			rhoCP_soil = rhoCP_soil_transient;
		} else if ( ( thisCell.temperature < frzIceTrans ) && ( thisCell.temperature > frzAllIce ) ) {
			rhoCP_soil = rhoCP_soil_ice + ( rhoCP_soil_transient - rhoCP_soil_ice ) / ( frzIceTrans - frzAllIce ) * ( thisCell.temperature - frzAllIce );
		} else { // Debugging -- Delete me
			thisCell.temperature = thisCell.temperature;
		}
		
		thisCell.props.rhoCp = baseDensity * baseSpecificHeat; //rhoCP_soil;

		thisCell.props.specificHeat = thisCell.props.rhoCp / thisCell.props.density;

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
