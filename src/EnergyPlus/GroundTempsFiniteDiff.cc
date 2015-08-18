// C++ Headers
#include <memory>
#include <fstream> 

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <DataEnvironment.hh>
#include <DataGlobals.hh>
#include <DataReportingFlags.hh>
#include <GroundTempsManager.hh>
#include <WeatherManager.hh>

namespace EnergyPlus {

namespace GroundTemps {
	
	using DataGlobals::SecsInDay;	
	
	int daysInYear = 365;
	int simDay = 0;
	Real64 finalTempConvergenceCriteria = 0.01;
	Real64 iterationTempConvergenceCriteria = 0.00001;

	void
	FiniteDiffGroundTempsModel::initAndSim()
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

		//using WeatherManager::AddDesignSetToEnvironmentStruct;
		using WeatherManager::GetNextEnvironment;
		using WeatherManager::ManageWeather;
		using WeatherManager::ResetEnvironmentCounter;

		using namespace DataEnvironment;
		using namespace DataGlobals;
		using namespace DataReportingFlags;
		using namespace WeatherManager;

		bool Available; // an environment is available to process
		bool ErrorsFound;

		Real64 outDryBulbTemp_num;
		Real64 relHum_num;
		Real64 windSpeed_num;
		Real64 horizSolarRad_num;
		Real64 airDensity_num;
		Real64 annualAveAirTemp_num;
		int denominator;

		ResetEnvironmentCounter();

		std::ofstream outFile( "WeatherFileData.csv", std::ofstream::out );

		WarmupFlag = false;
		Available = true;
		ErrorsFound = false;

		for ( int i = 1; i <= NumOfEnvrn; ++i ) { // loop over environments

			GetNextEnvironment( Available, ErrorsFound );

			if ( KindOfSim != ksReadAllWeatherData ) continue;

			weatherDataArray.dimension( daysInYear );

			BeginEnvrnFlag = true;
			EndEnvrnFlag = false;
			EndMonthFlag = false;
			WarmupFlag = false;
			DayOfSim = 0;
			DayOfSimChr = "0";
			NumOfWarmupDays = 0;

			annualAveAirTemp_num = 0.0;

			while ( ( DayOfSim < NumOfDayInEnvrn ) || ( WarmupFlag ) ) { // Begin day loop ...

				++DayOfSim;

				// Reset daily values
				outDryBulbTemp_num = 0.0;
				relHum_num = 0.0;
				windSpeed_num = 0.0;
				horizSolarRad_num = 0.0;
				airDensity_num = 0.0;
				denominator = 0;

				auto & tdwd = weatherDataArray( DayOfSim ); // "This day weather data"

				BeginDayFlag = true;
				EndDayFlag = false;

				for ( HourOfDay = 1; HourOfDay <= 24; ++HourOfDay ) { // Begin hour loop ...

					BeginHourFlag = true;
					EndHourFlag = false;

					for ( TimeStep = 1; TimeStep <= NumOfTimeStepInHour; ++TimeStep ) {

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

						outDryBulbTemp_num += OutDryBulbTemp;
						airDensity_num += OutAirDensity;
						relHum_num += OutRelHumValue;
						windSpeed_num += WindSpeed;
						horizSolarRad_num += BeamSolarRad + DifSolarRad;

						BeginHourFlag = false;
						BeginDayFlag = false;
						BeginEnvrnFlag = false;
						BeginSimFlag = false;
						BeginFullSimFlag = false;

						++denominator;

					} // TimeStep loop

					PreviousHour = HourOfDay;

				} // ... End hour loop.

				tdwd.dryBulbTemp = outDryBulbTemp_num / denominator;
				tdwd.relativeHumidity = relHum_num / denominator;
				tdwd.windSpeed = windSpeed_num / denominator;
				tdwd.horizontalRadiation = horizSolarRad_num / denominator;
				tdwd.airDensity = airDensity_num / denominator;
				annualAveAirTemp_num += tdwd.dryBulbTemp;

				outFile << tdwd.dryBulbTemp << "," << tdwd.relativeHumidity << "," << tdwd.windSpeed << "," << tdwd.horizontalRadiation << "," << tdwd.airDensity << std::endl;

			} // ... End day loop.

		} // ... End environment loop.

		annualAveAirTemp = annualAveAirTemp_num / 365;
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
			auto & thisCell = cellArray( i );

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
		std::ofstream static outFile_Jan1( "Jan1.csv", std::ofstream::out );
		std::ofstream static outFile_June1( "June1.csv", std::ofstream::out );

		timeStepInSeconds = SecsInDay; 
		bool convergedFinal = false;
		int yearCounter = 0;

		initDomain();

		for (int i = 1; i <= totalNumCells; ++i ) {
				outFile_Jan1 << "," << cellArray(i).minZValue;
			}
		
		outFile_Jan1 << std::endl;

		outFile_Jan1 << "0";

		for (int i = 1; i <= totalNumCells; ++i ) {
				outFile_Jan1 << "," << cellArray(i).temperature;
			}

		outFile_Jan1 << std::endl;

		do {
		
			// loop over all days
			for ( simDay = 1; simDay <= daysInYear; ++simDay ) {

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

			}

			// Check final temperature convergence
			convergedFinal = checkFinalTemperatureConvergence();
			
			++yearCounter;
			
			outFile_Jan1 << yearCounter;
			outFile_June1 << yearCounter;

			for (int i = 1; i <= totalNumCells; ++i ) {
				outFile_Jan1 << "," << groundTemps( 1, i );
				outFile_June1 << "," << groundTemps( 152, i );
			}

			outFile_Jan1 << std::endl;
			outFile_June1 << std::endl;

		} while ( !convergedFinal );

		// Output final annual temps for testing
		for ( int cell = 1; cell <= totalNumCells; ++cell ) {

			outFile << cellArray( cell ).minZValue;

			for ( int day = 1; day <= daysInYear; ++ day ) {
				if ( day < daysInYear ) {
					outFile << "," << groundTemps( day, cell );
				} else {
					outFile << "," << groundTemps( day, cell ) << "," << std::endl;
				}
			}
		}
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

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// declare some variables
		Real64 numerator;
		Real64 denominator;
		Real64 resistance;
		Real64 incidentHeatGain;
		Real64 incidentSolar_MJhrmin;
		Real64 evapotransHeatLoss_Wm2;
		Real64 absorbedIncidentSolar_MJhrmin;
		Real64 vaporPressureSaturated_kPa;
		Real64 vaporPressureActual_kPa;
		Real64 currAirTempK;
		Real64 QRAD_A;
		Real64 QRAD_SO;
		Real64 QRAD_NL;
		Real64 ratio_SO;
		Real64 netIncidentRadiation_MJhr;
		Real64 netIncidentRadiation_Wm2;
		Real64 slope_S;
		Real64 CN;
		Real64 G_hr;
		Real64 Cd;
		Real64 pressure;
		Real64 psychrometricConstant;
		Real64 evapotransFluidLoss_mmhr;
		Real64 evapotransFluidLoss_mhr;
		Real64 latentHeatVaporization;
		Real64 evapotransHeatLoss_MJhrmin;

		Real64 const rho_water( 998.0 ); // [kg/m3]
		Real64 const airSpecificHeat( 1003 ); // '[J/kg-K]
		// evapotranspiration parameters
		Real64 const meanSolarConstant( 0.08196 ); // 1367 [W/m2], entered in [MJ/m2-minute]
		Real64 const A_s( 0.25 );
		Real64 const B_s( 0.5 ); 
		Real64 const absor_Corrected( 0.77 );
		Real64 const convert_Wm2_To_MJhrmin( 3600.0 / 1000000.0 );
		Real64 const convert_MJhrmin_To_Wm2( 1.0 / convert_Wm2_To_MJhrmin );

		bool static surfConstantsSet;

		using DataEnvironment::Elevation;

		// initialize values
		numerator = 0.0;
		denominator = 0.0;
		resistance = 0.0;
		surfConstantsSet = false;

		auto & thisCell = cellArray( 1 );
		auto & cellBelow_thisCell = cellArray( 2 );
		auto & cwd = weatherDataArray( simDay ); // "Current Weather Day"

		// Add effect from previous time step
		numerator += thisCell.temperature_prevTimeStep;
		++denominator;

		// Conduction to lower cell
		resistance = ( thisCell.thickness / 2.0 ) / ( thisCell.props.conductivity * thisCell.conductionArea ) 
						+ ( cellBelow_thisCell.thickness / 2.0 )/( cellBelow_thisCell.props.conductivity * cellBelow_thisCell.conductionArea );
		numerator += ( thisCell.beta / resistance ) * cellBelow_thisCell.temperature;
		denominator += ( thisCell.beta / resistance );

		// Convection to atmosphere
		if ( cwd.windSpeed > 0.1 ) {
			resistance = 208.0 / ( cwd.airDensity * airSpecificHeat * cwd.windSpeed * thisCell.conductionArea );
		} else {
			// Future development should include additional natural convection effects here
		}
		numerator += ( thisCell.beta / resistance ) * cwd.dryBulbTemp;
		denominator += ( thisCell.beta / resistance );

		// Initialize, this variable is used for both evapotranspiration and non-ET cases, [W]
		incidentHeatGain = 0.0;

		// For convenience convert to Kelvin once
		currAirTempK = cwd.dryBulbTemp + 273.15;

		// Convert input solar radiation [w/m2] into units for ET model, [MJ/hr-min]
		// Diffuse + Direct Beam Radation
		incidentSolar_MJhrmin = cwd.horizontalRadiation * convert_Wm2_To_MJhrmin;

		// Absorbed solar radiation, [MJ/hr-min]
		absorbedIncidentSolar_MJhrmin = absor_Corrected * incidentSolar_MJhrmin;

		// Calculate saturated vapor pressure, [kPa]
		vaporPressureSaturated_kPa = 0.6108 * std::exp( 17.27 * cwd.dryBulbTemp / ( cwd.dryBulbTemp + 237.3 ) );

		// Calculate actual vapor pressure, [kPa]
		vaporPressureActual_kPa = vaporPressureSaturated_kPa * cwd.relativeHumidity;

		// Calculate another Q term, [MJ/m2-hr]
		QRAD_NL = 2.042E-10 * pow_4( currAirTempK ) * ( 0.34 - 0.14 * std::sqrt( vaporPressureActual_kPa ) );

		// Calculate another Q term, [MJ/hr]
		netIncidentRadiation_MJhr = absorbedIncidentSolar_MJhrmin - QRAD_NL;

		// constant
		CN = 37.0;

		// Check whether there was sun
		if ( netIncidentRadiation_MJhr < 0.0 ) {
			G_hr = 0.5 * netIncidentRadiation_MJhr;
			Cd = 0.96;
		} else {
			G_hr = 0.1 * netIncidentRadiation_MJhr;
			Cd = 0.24;
		}

		slope_S = 2503.0 * std::exp( 17.27 * cwd.dryBulbTemp / ( cwd.dryBulbTemp + 237.3 ) ) / pow_2( cwd.dryBulbTemp + 237.3 );
		pressure = 98.0;
		psychrometricConstant = 0.665e-3 * pressure;

		// Evapotranspiration constant, [mm/hr]
		evapotransFluidLoss_mmhr = ( evapotransCoeff * slope_S * ( netIncidentRadiation_MJhr - G_hr ) + psychrometricConstant * ( CN / currAirTempK ) * cwd.windSpeed * ( vaporPressureSaturated_kPa - vaporPressureActual_kPa ) ) / ( slope_S + psychrometricConstant * ( 1 + Cd * cwd.windSpeed ) );

		// Convert units, [m/hr]
		evapotransFluidLoss_mhr = evapotransFluidLoss_mmhr / 1000.0;

		// Calculate latent heat, [MJ/kg]
		// Full formulation is cubic: L(T) = -0.0000614342 * T**3 + 0.00158927 * T**2 - 2.36418 * T + 2500.79[5]
		// In: Cubic fit to Table 2.1,p.16, Textbook: R.R.Rogers & M.K. Yau, A Short Course in Cloud Physics, 3e,(1989), Pergamon press
		// But a linear relation should suffice;
		// note-for now using the previous time step temperature as an approximation to help ensure stability
		latentHeatVaporization = 2.501 - 2.361e-3 * thisCell.temperature_prevTimeStep;

		// Calculate evapotranspiration heat loss, [MJ/m2-hr]
		evapotransHeatLoss_MJhrmin = rho_water * evapotransFluidLoss_mhr * latentHeatVaporization;

		// Convert net incident solar units, [W/m2]
		netIncidentRadiation_Wm2 = netIncidentRadiation_MJhr * convert_MJhrmin_To_Wm2;

		// Convert evapotranspiration units, [W/m2]
		evapotransHeatLoss_Wm2 = evapotransHeatLoss_MJhrmin * convert_MJhrmin_To_Wm2;

		// Calculate overall net heat ?gain? into the cell, [W]
		incidentHeatGain = ( netIncidentRadiation_Wm2 - evapotransHeatLoss_Wm2 ) * thisCell.conductionArea;

		// Add any solar/evapotranspiration heat gain here
		numerator += thisCell.beta * incidentHeatGain;

		// Calculate the return temperature and leave
		cellArray( 1 ).temperature = numerator / denominator;

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

		auto & thisCell = cellArray( cell );
		auto & cellAbove_thisCell = cellArray( cell - 1 );
		auto & cellBelow_thisCell = cellArray( cell + 1 );

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
		// Fridleifsson, I.B., R. Bertani, E.Huenges, J.W. Lund, A. Ragnarsson, L. Rybach. 2008
		//	'The possible role and contribution of geothermal energy to the mitigation of climate change.'
		//	IPCC scoping meeting on renewable energy sources: 59-80.

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		Real64 numerator;
		Real64 denominator;
		Real64 resistance;
		Real64 HTBottom;
		Real64 geothermalGradient;

		auto & thisCell = cellArray( totalNumCells );
		auto & cellAbove_thisCell = cellArray( totalNumCells - 1 );

		numerator = 0.0;
		denominator = 0.0;
		resistance = 0.0;
		geothermalGradient = 0.025; // C/m

		// Initialize
		numerator += thisCell.temperature_prevTimeStep;
		++denominator;

		// Conduction resistance between this cell and above cell
		resistance = ( ( thisCell.thickness / 2.0 ) / ( thisCell.conductionArea * thisCell.props.conductivity ) ) 
					+ ( ( cellAbove_thisCell.thickness / 2.0 ) / ( cellAbove_thisCell.conductionArea * cellAbove_thisCell.props.conductivity ) );

		numerator += ( thisCell.beta / resistance ) * cellAbove_thisCell.temperature;
		denominator += thisCell.beta / resistance;

		// Geothermal gradient heat transfer
		HTBottom = geothermalGradient * thisCell.props.conductivity * thisCell.conductionArea;

		numerator += thisCell.beta * HTBottom;

		cellArray( totalNumCells ).temperature = numerator / denominator;

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
		
			auto & thisCell = cellArray( cell );

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
		Real64 avgGroundTemp = annualAveAirTemp;
		Real64 aveGroundTempAmplitiude = 12.0;
		int phaseShiftDay = 21;
		Real64 groundThemalDiffusivity = baseConductivity / ( baseDensity * baseSpecificHeat );
		
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
			auto & thisCell = cellArray( cell );
			
			Real64 depth = ( thisCell.maxZValue + thisCell.minZValue ) / 2.0;
			
			// Initialize temperatures
			thisCell.temperature = tempModel->getGroundTempAtTimeInSeconds( depth, 0.0 );  // Initialized at first day of year
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

			auto & thisCell = cellArray( cell );

			thisCell.temperature_prevTimeStep = thisCell.temperature;

			// Log temps for later use
			groundTemps( simDay, cell ) = thisCell.temperature;
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
			
			auto & thisCell = cellArray( cell );

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
		Theta_liq = waterContent;
		Theta_sat = saturatedWaterContent;

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

		auto & thisCell = cellArray( cell );

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
		} else {
			assert( false ); // Shouldn't get here
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
