// EnergyPlus, Copyright (c) 1996-2016, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
//
// If you have questions about your rights to use or distribute this software, please contact
// Berkeley Lab's Innovation & Partnerships Office at IPO@lbl.gov.
//
// NOTICE: This Software was developed under funding from the U.S. Department of Energy and the
// U.S. Government consequently retains certain rights. As such, the U.S. Government has been
// granted for itself and others acting on its behalf a paid-up, nonexclusive, irrevocable,
// worldwide license in the Software to reproduce, distribute copies to the public, prepare
// derivative works, and perform publicly and display publicly, and to permit others to do so.
//
// Redistribution and use in source and binary forms, with or without modification, are permitted
// provided that the following conditions are met:
//
// (1) Redistributions of source code must retain the above copyright notice, this list of
//     conditions and the following disclaimer.
//
// (2) Redistributions in binary form must reproduce the above copyright notice, this list of
//     conditions and the following disclaimer in the documentation and/or other materials
//     provided with the distribution.
//
// (3) Neither the name of the University of California, Lawrence Berkeley National Laboratory,
//     the University of Illinois, U.S. Dept. of Energy nor the names of its contributors may be
//     used to endorse or promote products derived from this software without specific prior
//     written permission.
//
// (4) Use of EnergyPlus(TM) Name. If Licensee (i) distributes the software in stand-alone form
//     without changes from the version obtained under this License, or (ii) Licensee makes a
//     reference solely to the software portion of its product, Licensee must refer to the
//     software as "EnergyPlus version X" software, where "X" is the version number Licensee
//     obtained under this License and may not use a different name for the software. Except as
//     specifically required in this Section (4), Licensee shall not use in a company name, a
//     product name, in advertising, publicity, or other promotional activities any name, trade
//     name, trademark, logo, or other designation of "EnergyPlus", "E+", "e+" or confusingly
//     similar designation, without Lawrence Berkeley National Laboratory's prior written consent.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
//
// You are under no obligation whatsoever to provide any bug fixes, patches, or upgrades to the
// features, functionality or performance of the source code ("Enhancements") to anyone; however,
// if you choose to make your Enhancements available either publicly, or directly to Lawrence
// Berkeley National Laboratory, without imposing a separate written license agreement for such
// Enhancements, then you hereby grant the following license: a non-exclusive, royalty-free
// perpetual license to install, use, modify, prepare derivative works, incorporate into other
// computer software, distribute, and sublicense such enhancements or derivative works thereof,
// in binary and source code form.

// C++ Headers
#include <algorithm>
#include <memory>

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <DataEnvironment.hh>
#include <General.hh>
#include <DataGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataReportingFlags.hh>
#include <GroundTemperatureModeling/FiniteDifferenceGroundTemperatureModel.hh>
#include <GroundTemperatureModeling/KusudaAchenbachGroundTemperatureModel.hh>
#include <GroundTemperatureModeling/GroundTemperatureModelManager.hh>
#include <InputProcessor.hh>
#include <WeatherManager.hh>

namespace EnergyPlus {

	using DataGlobals::SecsInDay;
	using WeatherManager::NumDaysInYear;
	int simDay = 0;
	int numIterYears = 0;
	int const maxYearsToIterate = 10;
	Real64 finalTempConvergenceCriteria = 0.05;
	Real64 iterationTempConvergenceCriteria = 0.00001;

	//******************************************************************************

	// Finite difference model factory
	std::shared_ptr< FiniteDiffGroundTempsModel >
	FiniteDiffGroundTempsModel::FiniteDiffGTMFactory(
		int objectType,
		std::string objectName
		)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Matt Mitchell
		//       DATE WRITTEN   Summer 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Read input and creates instance of finite difference ground temp model

		// USE STATEMENTS:
		using namespace DataIPShortCuts;
		using namespace GroundTemperatureManager;

		// Locals
		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		bool found = false;
		int NumNums;
		int NumAlphas;
		int IOStat;
		bool ErrorsFound = false;

		// New shared pointer for this model object
		std::shared_ptr< FiniteDiffGroundTempsModel > thisModel( new FiniteDiffGroundTempsModel() );

		// Search through finite diff models here
		std::string const cCurrentModuleObject = CurrentModuleObjects( objectType_FiniteDiffGroundTemp );
		int numCurrModels = InputProcessor::GetNumObjectsFound( cCurrentModuleObject );

		for ( int modelNum = 1; modelNum <= numCurrModels; ++modelNum ) {

			InputProcessor::GetObjectItem( cCurrentModuleObject, modelNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat );

			if ( objectName == cAlphaArgs( 1 ) ) {
				// Read input into object here

				thisModel->objectType = objectType;
				thisModel->objectName = cAlphaArgs( 1 );
				thisModel->baseConductivity = rNumericArgs( 1 );
				thisModel->baseDensity = rNumericArgs( 2 );
				thisModel->baseSpecificHeat = rNumericArgs( 3 );
				thisModel->waterContent = rNumericArgs( 4 ) / 100.0;
				thisModel->saturatedWaterContent = rNumericArgs( 5 ) / 100.0;
				thisModel->evapotransCoeff = rNumericArgs( 6 );

				found = true;
				break;
			}
		}

		if ( found && !ErrorsFound ) {
			groundTempModels.push_back( thisModel );

			// Simulate
			thisModel->initAndSim();

			// Return the pointer
			return thisModel;
		} else {
			ShowFatalError( "Site:GroundTemperature:Undisturbed:FiniteDifference--Errors getting input for ground temperature model" );
			return nullptr;
		}
	}

	//******************************************************************************

	void
	FiniteDiffGroundTempsModel::initAndSim()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Matt Mitchell
		//       DATE WRITTEN   Summer 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Initalizes and simulated finite difference ground temps model

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
		// Finds correct envrionment for reading all weather data. Loops over all weather data in weather file
		//	and data structure containing daily average of required weather data.

		// USE STATEMENTS:
		using General::JulianDay;
		using WeatherManager::GetNextEnvironment;
		using WeatherManager::ManageWeather;
		using WeatherManager::ResetEnvironmentCounter;
		using WeatherManager::RPReadAllWeatherData;
		using namespace DataEnvironment;
		using namespace DataGlobals;
		using namespace DataReportingFlags;
		using namespace WeatherManager;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		bool Available; // an environment is available to process
		bool ErrorsFound;
		Real64 outDryBulbTemp_num;
		Real64 relHum_num;
		Real64 windSpeed_num;
		Real64 horizSolarRad_num;
		Real64 airDensity_num;
		Real64 annualAveAirTemp_num;
		int denominator;

		// Save current environment so we can revert back when done
		int Envrn_reset = Envrn;
		int KindOfSim_reset = KindOfSim;
		int TimeStep_reset = TimeStep;
		int HourOfDay_reset = HourOfDay;
		bool BeginEnvrnFlag_reset = BeginEnvrnFlag;
		bool EndEnvrnFlag_reset = EndEnvrnFlag;
		bool EndMonthFlag_reset = EndMonthFlag;
		bool WarmupFlag_reset = WarmupFlag;
		int DayOfSim_reset = DayOfSim;
		std::string DayOfSimChr_reset = DayOfSimChr;
		int NumOfWarmupDays_reset = NumOfWarmupDays;
		bool BeginDayFlag_reset = BeginDayFlag;
		bool EndDayFlag_reset = EndDayFlag;
		bool BeginHourFlag_reset = BeginHourFlag;
		bool EndHourFlag_reset = EndHourFlag;

		if ( !WeatherFileExists ){
			ShowContinueError( "Site:GroundTemperature:Undisturbed:FiniteDifference -- using this model requires specification of a weather file." );
			ShowContinueError( "Either place in.epw in the working directory or specify a weather file on the command line using -w /path/to/weather.epw");
			ShowFatalError( "Simulation halted due to input error in ground temperaure model." );
		}

		++NumOfEnvrn;
		++TotRunPers;
		Environment.redimension( NumOfEnvrn );
		RunPeriodInput.redimension( TotRunPers );
		Environment( NumOfEnvrn ).KindOfEnvrn = ksReadAllWeatherData;
		RPReadAllWeatherData = true;
		WeathSimReq = true;
		RunPeriodInput( TotRunPers ).StartDate = JulianDay( 1, 1, 0 );
		RunPeriodInput( TotRunPers ).EndDate = JulianDay( 12, 31, 0 );
		RunPeriodInput( TotRunPers ).MonWeekDay = 0;

		SetupEnvironmentTypes();

		ResetEnvironmentCounter();

		Available = true;
		ErrorsFound = false;

		for ( int i = 1; i <= NumOfEnvrn; ++i ) { // loop over environments

			GetNextEnvironment( Available, ErrorsFound );

			if ( KindOfSim != ksReadAllWeatherData ) continue;

			weatherDataArray.dimension( NumDaysInYear );

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

				// Log data for domain initialization using KA model
				annualAveAirTemp_num += tdwd.dryBulbTemp;

				if (tdwd.dryBulbTemp < minDailyAirTemp ) {
					minDailyAirTemp = tdwd.dryBulbTemp;
					dayOfMinDailyAirTemp = DayOfSim;
				}

				if (tdwd.dryBulbTemp > maxDailyAirTemp ) {
					maxDailyAirTemp = tdwd.dryBulbTemp;
				}

			} // ... End day loop.

		} // ... End environment loop.

		annualAveAirTemp = annualAveAirTemp_num / NumDaysInYear; // Used for initalizing domain

		// Reset Envrionment when done reading data
		--NumOfEnvrn; // May need better way of eliminating the extra envrionment that was added to read the data
		--TotRunPers;
		KindOfSim = KindOfSim_reset;
		RPReadAllWeatherData = false;
		Environment.redimension( NumOfEnvrn );
		RunPeriodInput.redimension( TotRunPers );
		Envrn = Envrn_reset;
		TimeStep = TimeStep_reset;
		HourOfDay = HourOfDay_reset;
		BeginEnvrnFlag = BeginEnvrnFlag_reset;
		EndEnvrnFlag = EndEnvrnFlag_reset;
		EndMonthFlag = EndMonthFlag_reset;
		WarmupFlag = WarmupFlag_reset;
		DayOfSim = DayOfSim_reset;
		DayOfSimChr = DayOfSimChr_reset;
		NumOfWarmupDays = NumOfWarmupDays_reset;
		BeginDayFlag = BeginDayFlag_reset;
		EndDayFlag = EndDayFlag_reset;
		BeginHourFlag = BeginHourFlag_reset;
		EndHourFlag = EndHourFlag_reset;
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
		// Creates static mesh used for model

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// Surface layer parameters
		Real64 surfaceLayerThickness = 2.0;
		Real64 surfaceLayerCellThickness = 0.015;
		int surfaceLayerNumCells = surfaceLayerThickness / surfaceLayerCellThickness;

		// Center layer parameters
		Real64 centerLayerExpansionCoeff = 1.10879;
		int centerLayerNumCells = 80;

		// Deep layer parameters
		Real64 deepLayerThickness = 0.2;
		Real64 deepLayerCellThickness = surfaceLayerCellThickness;
		int deepLayerNumCells = deepLayerThickness / deepLayerCellThickness;

		// Other
		Real64 currentCellDepth = 0.0;

		totalNumCells = surfaceLayerNumCells + centerLayerNumCells + deepLayerNumCells;

		// Allocate arrays
		cellArray.allocate( totalNumCells );
		cellDepths.allocate( totalNumCells );

		for ( int i = 1; i <= totalNumCells; ++i ) {

			// Reference to thisCell
			auto & thisCell = cellArray( i );

			// Set the index
			thisCell.index = i;

			// Give thickness to the cells
			if ( i <= surfaceLayerNumCells ) {
				// Constant thickness mesh here
				thisCell.thickness = surfaceLayerCellThickness;

			} else if ( i > surfaceLayerNumCells && i <= ( centerLayerNumCells + surfaceLayerNumCells ) ) {
				// Geometric expansion/contraction here
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

			// Populate depth array for use later when looking up temperatures
			cellDepths( i ) = currentCellDepth + thisCell.thickness / 2.0 ;

			// Update local counter
			currentCellDepth += thisCell.thickness;

			// Set maximum z value
			thisCell.maxZValue = currentCellDepth;

			// Set base properties
			thisCell.props.conductivity = baseConductivity;
			thisCell.props.density = baseDensity;
			thisCell.props.specificHeat = baseSpecificHeat;
			thisCell.props.diffusivity = baseConductivity / ( baseDensity * baseSpecificHeat );

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
		// Simulates model, repeating years, until steady-periodic temperatures are determined.

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		timeStepInSeconds = SecsInDay;
		bool convergedFinal = false;

		initDomain();

		// Loop until converged
		do {

			// loop over all days
			for ( simDay = 1; simDay <= NumDaysInYear; ++simDay ) {

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


		} while ( !convergedFinal );
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
		// Determines heat transfer to surface. Updates surface cell temperature.

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 numerator( 0.0 );
		Real64 denominator( 0.0 );
		Real64 resistance( 0.0 );
		Real64 incidentHeatGain;
		Real64 incidentSolar_MJhrmin;
		Real64 evapotransHeatLoss_Wm2;
		Real64 absorbedIncidentSolar_MJhrmin;
		Real64 vaporPressureSaturated_kPa;
		Real64 vaporPressureActual_kPa;
		Real64 currAirTempK;
		Real64 QRAD_NL;
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
		Real64 const absor_Corrected( 0.77 );
		Real64 const convert_Wm2_To_MJhrmin( 3600.0 / 1000000.0 );
		Real64 const convert_MJhrmin_To_Wm2( 1.0 / convert_Wm2_To_MJhrmin );

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
		// Update cell temperature based on HT from cells above and below

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 numerator( 0.0 );
		Real64 denominator( 0.0 );
		Real64 resistance( 0.0 );

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
		// Updates bottom cell temperature based on earth heat flux HT from cell above

		// REFERENCES:
		// Fridleifsson, I.B., R. Bertani, E.Huenges, J.W. Lund, A. Ragnarsson, L. Rybach. 2008
		//	'The possible role and contribution of geothermal energy to the mitigation of climate change.'
		//	IPCC scoping meeting on renewable energy sources: 59-80.

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 numerator( 0.0 );
		Real64 denominator( 0.0 );
		Real64 resistance( 0.0 );
		Real64 HTBottom;
		Real64 geothermalGradient( 0.025 ); // C/m

		auto & thisCell = cellArray( totalNumCells );
		auto & cellAbove_thisCell = cellArray( totalNumCells - 1 );

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
		// Checks final temperature convergence

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		bool converged = true;

		if ( numIterYears == maxYearsToIterate ) return converged;

		for ( int cell = 1; cell <= totalNumCells; ++ cell ) {

			auto & thisCell = cellArray( cell );

			if ( std::abs( thisCell.temperature - thisCell.temperature_finalConvergence ) >= finalTempConvergenceCriteria ) {
				converged = false;
			}

			thisCell.temperature_finalConvergence = thisCell.temperature;
		}

		++ numIterYears;

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
		// Checks iteration temperature convergence

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
		// Initalizes model using Kusuda-Achenbach model.
		// Average ground temp initialized to average annual air temperature

		// USE STATEMENTS:
		using DataGlobals::SecsInDay;
		using namespace GroundTemperatureManager;

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// Temporary KA model for initialization
		std::unique_ptr< KusudaGroundTempsModel > tempModel( new KusudaGroundTempsModel() );

		tempModel->objectName = "KAModelForFDModel";
		tempModel->objectType = objectType_KusudaGroundTemp;
		tempModel->aveGroundTemp = annualAveAirTemp;
		tempModel->aveGroundTempAmplitude = ( maxDailyAirTemp - minDailyAirTemp ) / 4.0; // Rough estimate here. Ground temps will not swing as far as the air temp.
		tempModel->phaseShiftInSecs = dayOfMinDailyAirTemp * SecsInDay;
		tempModel->groundThermalDiffisivity = baseConductivity / ( baseDensity * baseSpecificHeat );

		// Intialize temperatures and volume
		for ( int cell = 1; cell <= totalNumCells; ++cell ) {
			auto & thisCell = cellArray( cell );

			Real64 depth = ( thisCell.maxZValue + thisCell.minZValue ) / 2.0;

			// Initialize temperatures
			if ( tempModel ) {
				thisCell.temperature = tempModel->getGroundTempAtTimeInSeconds( depth, 0.0 );  // Initialized at first day of year
			}
			thisCell.temperature_finalConvergence = thisCell.temperature;
			thisCell.temperature_prevIteration = thisCell.temperature;
			thisCell.temperature_prevTimeStep = thisCell.temperature;

			// Set cell volume
			thisCell.volume = thisCell.thickness * thisCell.conductionArea;

		}

		// Initialize freezing calculation variables
		evaluateSoilRhoCp( _, true );

		// Initialize the groundTemps array
		groundTemps.dimension( { 1, NumDaysInYear }, { 1, totalNumCells }, 0.0 );

		tempModel.reset();
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
		// Updates iteration temperatures for convergence checks

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
		// Updates timestep temperatures for convergence checks.

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
		// Updates cell properties for each timestep

		for ( int cell = 1; cell <= totalNumCells; ++cell ) {

			auto & thisCell = cellArray( cell );

			evaluateSoilRhoCp( cell );

			thisCell.beta = ( timeStepInSeconds / ( thisCell.props.rhoCp * thisCell.volume ) );

		}
	}

	//******************************************************************************

	Real64
	FiniteDiffGroundTempsModel::interpolate(
		Real64 const x,
		Real64 const x_hi,
		Real64 const x_low,
		Real64 const y_hi,
		Real64 const y_low
	)
	{
		return ( x - x_low ) / ( x_hi - x_low ) * ( y_hi - y_low ) + y_low;
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
		// Interpolates between days and depths to find correct ground temperature

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// Interpolation variables
		int i0;				// First day
		int i1;				// Next day
		int j0;				// Cell index with depth less than y-depth
		int j1;				// Next cell index (with depth greater than y-depth
		Real64 T_i0_j0;		// Temp at int( x-day ); cell lower_bound( y-depth )
		Real64 T_i1_j0;		// Temp at int( x-day ) + 1; cell lower_bound( y-depth )
		Real64 T_i0_j1;		// Temp at int( x-day ); cell lower_bound( y-depth ) + 1
		Real64 T_i1_j1;		// Temp at int( x-day ) + 1; cell lower_bound( y-depth ) + 1
		Real64 T_ix_j0;		// Temp at x-day; cell lower_bound( y-depth )
		Real64 T_ix_j1;		// Temp at x-day; cell lower_bound( y-depth ) + 1
		Real64 T_ix_jy;		// Final Temperature--Temp at x-day; y-depth
		Real64 dayFrac;		// Fraction of day

		if ( depth < 0.0 ) {
			depth = 0.0;
		}

		// Get index of nearest cell with depth less than depth
		auto it = std::lower_bound( cellDepths.begin(), cellDepths.end(), depth );
		j0 = std::distance( cellDepths.begin(), it );

		// Compensate for 1-based array
		++j0;

		// Fraction of day
		dayFrac = simTimeInDays - int( simTimeInDays );

		if ( j0 < totalNumCells - 1 ) {
			// All depths within domain
			j1 = j0 + 1;

			if ( simTimeInDays <= 1 || simTimeInDays >= NumDaysInYear) {
				// First day of year, last day of year, and leap day
				// Interpolate between first and last day
				i0 = NumDaysInYear;
				i1 = 1;

				// Lookup ground temps
				T_i0_j0 = groundTemps( i0, j0 );
				T_i0_j1 = groundTemps( i0, j1 );
				T_i1_j0 = groundTemps( i1, j0 );
				T_i1_j1 = groundTemps( i1, j1 );

				// Interpolate between days holding depth constant
				T_ix_j0 = interpolate( dayFrac, 1, 0, T_i1_j0, T_i0_j0 );
				T_ix_j1 = interpolate( dayFrac, 1, 0, T_i1_j1, T_i0_j1 );

				// Interpolate to correct depth now that we're at the right time
				T_ix_jy = interpolate( depth, cellDepths( j1 ), cellDepths( j0 ), T_ix_j1, T_ix_j0 );

			} else {
				// All other days
				i0 = int( simTimeInDays );
				i1 = i0 + 1;

				// Lookup ground temps
				T_i0_j0 = groundTemps( i0, j0 );
				T_i0_j1 = groundTemps( i0, j1 );
				T_i1_j0 = groundTemps( i1, j0 );
				T_i1_j1 = groundTemps( i1, j1 );

				// Interpolate between days holding depth constant
				T_ix_j0 = interpolate( dayFrac, 1, 0, T_i1_j0, T_i0_j0 );
				T_ix_j1 = interpolate( dayFrac, 1, 0, T_i1_j1, T_i0_j1 );

				// Interpolate to correct depth now that we're at the right time
				T_ix_jy = interpolate( depth, cellDepths( j1 ), cellDepths( j0 ), T_ix_j1, T_ix_j0 );
			}

		} else {
			// Requesting a temperature deeper than domain. Pass deepest point in domain.
			j0 = totalNumCells;
			j1 = j0;

			if ( simTimeInDays <= 1 || simTimeInDays >= NumDaysInYear) {
				// First day of year, last day of year, and leap day
				// Interpolate between first and last day
				i0 = NumDaysInYear;
				i1 = 1;

				// Lookup ground temps
				T_i0_j1 = groundTemps( i0, j1 );
				T_i1_j1 = groundTemps( i1, j1 );

				// Interpolate between days holding depth constant
				T_ix_jy = interpolate( dayFrac, 1, 0, T_i1_j1, T_i0_j1 );

			} else {
				// All other days
				i0 = int( simTimeInDays );
				i1 = i0 + 1;

				// Lookup ground temps
				T_i0_j1 = groundTemps( i0, j1 );
				T_i1_j1 = groundTemps( i1, j1 );

				// Interpolate between days holding depth constant
				T_ix_jy = interpolate( dayFrac, 1, 0, T_i1_j1, T_i0_j1 );
			}
		}

		return T_ix_jy;
	}

	//******************************************************************************

	Real64
	FiniteDiffGroundTempsModel::getGroundTempAtTimeInSeconds(
		Real64 const _depth,
		Real64 const seconds
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Matt Mitchell
		//       DATE WRITTEN   Summer 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Retrieves ground tempeature when input time is in seconds

		//Using
		using DataGlobals::SecsInDay;

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		depth = _depth;

		simTimeInDays = seconds / SecsInDay;

		if ( simTimeInDays > NumDaysInYear ) {
			simTimeInDays = remainder( simTimeInDays, NumDaysInYear );
		}

		return getGroundTemp();
	}

	//******************************************************************************

	Real64
	FiniteDiffGroundTempsModel::getGroundTempAtTimeInMonths(
		Real64 const _depth,
		int const month
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Matt Mitchell
		//       DATE WRITTEN   Summer 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Returns ground temperature when input time is in months

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 const aveDaysInMonth = NumDaysInYear / 12;

		depth = _depth;

		// Convert months to days. Puts time in middle of specified month
		simTimeInDays = aveDaysInMonth * ( ( month - 1 ) + 0.5 );

		if ( simTimeInDays > NumDaysInYear ) {
			simTimeInDays = remainder( simTimeInDays, NumDaysInYear );
		}

		// Get and return ground temperature
		return getGroundTemp();
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
		// Evaluates the soil properties

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
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

}	// EnergyPlus
