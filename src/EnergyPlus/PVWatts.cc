// EnergyPlus, Copyright (c) 1996-2017, The Board of Trustees of the University of Illinois,
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy), Oak Ridge
// National Laboratory, managed by UT-Battelle, Alliance for Sustainable Energy, LLC, and other
// contributors. All rights reserved.
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
//     similar designation, without the U.S. Department of Energy's prior written consent.
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

// C++ Headers
#include <math.h>

// ObjexxFCL Headers

// EnergyPlus Headers

#include <PVWatts.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <OutputProcessor.hh>
#include <DataHVACGlobals.hh>
#include <DataGlobals.hh>
#include <WeatherManager.hh>
#include <DataEnvironment.hh>

namespace EnergyPlus {

namespace PVWatts {

	std::map<int, PVWattsGenerator> PVWattsGenerators;

	PVWattsGenerator::PVWattsGenerator(const std::string &name, const Real64 dcSystemCapacity, ModuleType moduleType, ArrayType arrayType, Real64 systemLosses, GeometryType geometryType, Real64 tilt, Real64 azimuth, size_t surfaceNum, Real64 groundCoverageRatio) : m_lastCellTemperature(20.0), m_lastPlaneOfArrayIrradiance(0.0), m_cellTemperature(20.0), m_planeOfArrayIrradiance(0.0)
	{
		using General::RoundSigDigits;
		bool errorsFound(false);

		if (name.empty()) {
			ShowSevereError("PVWatts: name cannot be blank.");
			errorsFound = true;
		}
		m_name = name;

		if (dcSystemCapacity <= 0) {
			ShowSevereError("PVWatts: DC system capacity must be greater than zero.");
			errorsFound = true;
		}
		m_dcSystemCapacity = dcSystemCapacity;

		m_moduleType = moduleType;
		switch( m_moduleType ) {
			case ModuleType::STANDARD:
				m_gamma = -0.0047;
				m_useARGlass = false;
				break;
			case ModuleType::PREMIUM:
				m_gamma = -0.0035;
				m_useARGlass = true;
				break;
			case ModuleType::THIN_FILM:
				m_gamma = -0.0020;
				m_useARGlass = false;
				break;
		}

		m_arrayType = arrayType;
		switch( m_arrayType ) {
			case ArrayType::FIXED_OPEN_RACK:
				m_trackMode = 0;
				m_inoct = 45;
				m_shadeMode1x = 0;
				break;
			case ArrayType::FIXED_ROOF_MOUNTED:
				m_trackMode = 0;
				m_inoct = 49;
				m_shadeMode1x = 0;
				break;
			case ArrayType::ONE_AXIS:
				m_trackMode = 1;
				m_inoct = 45;
				m_shadeMode1x = 0;
				break;
			case ArrayType::ONE_AXIS_BACKTRACKING:
				m_trackMode = 1;
				m_inoct = 45;
				m_shadeMode1x = 1;
				break;
			case ArrayType::TWO_AXIS:
				m_trackMode = 2;
				m_inoct = 45;
				m_shadeMode1x = 0;
				break;
		}

		if (systemLosses > 1.0 || systemLosses < 0.0) {
			ShowSevereError("PVWatts: Invalid system loss value " + RoundSigDigits(systemLosses, 2));
			errorsFound = true;
		}
		m_systemLosses = systemLosses;

		m_geometryType = geometryType;

		if (m_geometryType == GeometryType::TILT_AZIMUTH) {
			if ( tilt < 0 || tilt > 90) {
				ShowSevereError("PVWatts: Invalid tilt: " + RoundSigDigits(tilt, 2));
					errorsFound = true;
			}
			m_tilt = tilt;
			if ( azimuth < 0 || azimuth >= 360) {
				ShowSevereError("PVWatts: Invalid azimuth: " + RoundSigDigits(azimuth, 2));
			}
			m_azimuth = azimuth;
		} else if (m_geometryType == GeometryType::SURFACE) {
			if (surfaceNum == 0 || surfaceNum > DataSurfaces::Surface.size()) {
				ShowSevereError("PVWatts: SurfaceNum not in Surfaces: " + std::to_string(surfaceNum));
				errorsFound = true;
			} else {
				m_surfaceNum = surfaceNum;
				m_tilt = getSurface().Tilt;
				m_azimuth = getSurface().Azimuth;
				// TODO: Do some bounds checking on Tilt and Azimuth.
			}
		} else {
			assert(false);
		}

		if (groundCoverageRatio > 1.0 || groundCoverageRatio < 0.0) {
			ShowSevereError("PVWatts: Invalid ground coverage ratio: " + RoundSigDigits(groundCoverageRatio, 2));
			errorsFound = true;
		}
		m_groundCoverageRatio = groundCoverageRatio;

		if (errorsFound) {
			ShowFatalError("Errors found in getting PVWatts input");
		}

		// Set up the pvwatts cell temperature member
		const Real64 pvwatts_height = 5.0;
		m_tccalc = std::unique_ptr< pvwatts_celltemp >( new pvwatts_celltemp( m_inoct + 273.15, pvwatts_height, DataGlobals::TimeStepZone ) );

	}

	void PVWattsGenerator::setupOutputVariables()
	{
		// Set up output variables
		SetupOutputVariable("Generator Produced DC Electric Power", OutputProcessor::Unit::W, m_outputDCPower, "System", "Average", m_name);
		SetupOutputVariable( "Generator Produced DC Electric Energy", OutputProcessor::Unit::J, m_outputDCEnergy, "System", "Sum", m_name, _, "ElectricityProduced", "Photovoltaics", _, "Plant" );
		SetupOutputVariable( "Generator PV Cell Temperature", OutputProcessor::Unit::C, m_cellTemperature, "System", "Average", m_name );
	}

	PVWattsGenerator PVWattsGenerator::createFromIdfObj(int objNum)
	{
		using InputProcessor::GetObjectItem;
		using InputProcessor::SameString;
		using InputProcessor::FindItemInList;

		Array1D_string cAlphaFieldNames;
		Array1D_string cNumericFieldNames;
		Array1D_bool lNumericFieldBlanks;
		Array1D_bool lAlphaFieldBlanks;
		Array1D_string cAlphaArgs;
		Array1D< Real64 > rNumericArgs;
		const int maxAlphas = 6; // from idd
		const int maxNumeric = 5; // from idd
		cAlphaFieldNames.allocate(maxAlphas);
		cNumericFieldNames.allocate(maxNumeric);
		lNumericFieldBlanks.allocate(maxNumeric);
		lAlphaFieldBlanks.allocate(maxAlphas);
		cAlphaArgs.allocate(maxAlphas);
		rNumericArgs.allocate(maxNumeric);
		int NumAlphas;
		int NumNums;
		int IOStat;

		GetObjectItem("Generator:PVWatts", objNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames);

		const std::string name(cAlphaArgs(AlphaFields::NAME));
		const Real64 dcSystemCapacity(rNumericArgs(NumFields::DC_SYSTEM_CAPACITY));
		const std::map<std::string, ModuleType> moduleTypeMap = { {"STANDARD", ModuleType::STANDARD}, {"PREMIUM", ModuleType::PREMIUM}, {"ThinFilm", ModuleType::THIN_FILM} };
		const ModuleType moduleType(moduleTypeMap.at(cAlphaArgs(AlphaFields::MODULE_TYPE)));
		const std::map<std::string, ArrayType> arrayTypeMap = { {"FIXEDOPENRACK", ArrayType::FIXED_OPEN_RACK}, {"FIXEDROOFMOUNTED", ArrayType::FIXED_ROOF_MOUNTED}, {"ONEAXIS", ArrayType::ONE_AXIS}, {"ONEAXISBACKTRACKING", ArrayType::ONE_AXIS_BACKTRACKING}, {"TWOAXIS", ArrayType::TWO_AXIS} };
		const ArrayType arrayType(arrayTypeMap.at(cAlphaArgs(AlphaFields::ARRAY_TYPE)));
		const Real64 systemLosses(rNumericArgs(NumFields::SYSTEM_LOSSES));
		const std::map<std::string, GeometryType> geometryTypeMap { {"TILTAZIMUTH", GeometryType::TILT_AZIMUTH}, {"SURFACE", GeometryType::SURFACE} };
		const GeometryType geometryType(geometryTypeMap.at(cAlphaArgs(AlphaFields::GEOMETRY_TYPE)));
		const Real64 tilt(rNumericArgs(NumFields::TILT_ANGLE));
		const Real64 azimuth(rNumericArgs(NumFields::AZIMUTH_ANGLE));
		int surfaceNum;
		if (lAlphaFieldBlanks(AlphaFields::SURFACE_NAME)) {
				surfaceNum = 0;
		} else {
			surfaceNum = FindItemInList(cAlphaArgs(AlphaFields::SURFACE_NAME), DataSurfaces::Surface);
		}
		if ( NumNums < NumFields::GROUND_COVERAGE_RATIO ) {
			return PVWattsGenerator(name, dcSystemCapacity, moduleType, arrayType, systemLosses, geometryType, tilt, azimuth, surfaceNum);
		}
		const Real64 groundCoverageRatio(rNumericArgs(NumFields::GROUND_COVERAGE_RATIO));

		PVWattsGenerator pvwattsGenerator(name, dcSystemCapacity, moduleType, arrayType, systemLosses, geometryType, tilt, azimuth, surfaceNum, groundCoverageRatio);
		pvwattsGenerator.setupOutputVariables();
		return pvwattsGenerator;
	}

	Real64 PVWattsGenerator::getDCSystemCapacity()
	{
		return m_dcSystemCapacity;
	}

	ModuleType PVWattsGenerator::getModuleType()
	{
		return m_moduleType;
	}

	ArrayType PVWattsGenerator::getArrayType()
	{
		return m_arrayType;
	}

	Real64 PVWattsGenerator::getSystemLosses()
	{
		return m_systemLosses;
	}

	GeometryType PVWattsGenerator::getGeometryType()
	{
		return m_geometryType;
	}

	Real64 PVWattsGenerator::getTilt()
	{
		return m_tilt;
	}

	Real64 PVWattsGenerator::getAzimuth()
	{
		return m_azimuth;
	}

	DataSurfaces::SurfaceData& PVWattsGenerator::getSurface()
	{
		return DataSurfaces::Surface(m_surfaceNum);
	}

	Real64 PVWattsGenerator::getGroundCoverageRatio()
	{
		return m_groundCoverageRatio;
	}

	Real64 PVWattsGenerator::getCellTempearture()
	{
		return m_cellTemperature;
	}

	void PVWattsGenerator::setCellTemperature(Real64 cellTemp) {
		m_cellTemperature = cellTemp;
	}

	Real64 PVWattsGenerator::getPlaneOfArrayIrradiance() {
		return m_planeOfArrayIrradiance;
	}

	void PVWattsGenerator::setPlaneOfArrayIrradiance(Real64 poa)
	{
		m_planeOfArrayIrradiance = poa;
	}

	void PVWattsGenerator::calc()
	{
		using DataGlobals::TimeStep;
		using DataHVACGlobals::TimeStepSys;
		using DataGlobals::HourOfDay;
		using DataGlobals::TimeStepZone;
		using DataGlobals::SecInHour;

		// We only run this once for each zone time step.
		if ( !DataGlobals::BeginTimeStepFlag ) {
			m_outputDCEnergy = m_outputDCPower * TimeStepSys * SecInHour;
			return;
		}

		m_lastCellTemperature = m_cellTemperature;
		m_lastPlaneOfArrayIrradiance = m_planeOfArrayIrradiance;

		// initialize_cell_temp
		m_tccalc->set_last_values(m_lastCellTemperature, m_lastPlaneOfArrayIrradiance);

		Real64 albedo = WeatherManager::TodayAlbedo(TimeStep, HourOfDay);
		if (not ( std::isfinite(albedo) && albedo > 0.0 && albedo < 1 )) {
			albedo = 0.2;
		}

		// process_irradiance
		IrradianceOutput irr_st = processIrradiance(DataEnvironment::Year, DataEnvironment::Month, DataEnvironment::DayOfMonth, HourOfDay - 1, (TimeStep - 0.5) * DataGlobals::MinutesPerTimeStep, TimeStepZone, WeatherManager::WeatherFileLatitude, WeatherManager::WeatherFileLongitude, WeatherManager::WeatherFileTimeZone, DataEnvironment::BeamSolarRad, DataEnvironment::DifSolarRad, albedo);

		// powerout
		// TODO: Change shad_beam to account for shading of other surfaces.
		Real64 shad_beam = 1.0;
		DCPowerOutput pwr_st = powerout(shad_beam, 1.0, DataEnvironment::BeamSolarRad, albedo, DataEnvironment::WindSpeed, DataEnvironment::OutDryBulbTemp, irr_st);

		// Report out
		m_cellTemperature = pwr_st.pvt;
		m_planeOfArrayIrradiance = pwr_st.poa;
		m_outputDCPower = pwr_st.dc;
		m_outputDCEnergy = m_outputDCPower * TimeStepSys * SecInHour;
	}

	void PVWattsGenerator::getResults(Real64 &GeneratorPower, Real64 &GeneratorEnergy, Real64 &ThermalPower, Real64 &ThermalEnergy)
	{
		GeneratorPower = m_outputDCPower;
		GeneratorEnergy = m_outputDCEnergy;
		ThermalPower = 0.0;
		ThermalEnergy = 0.0;
	}

	IrradianceOutput PVWattsGenerator::processIrradiance(int year, int month, int day, int hour, Real64 minute, Real64 ts_hour, Real64 lat, Real64 lon, Real64 tz, Real64 dn, Real64 df, Real64 alb) {
		IrradianceOutput out;

		using DataGlobals::TimeStep;
		using DataGlobals::HourOfDay;

		irrad irr;
		irr.set_time(  year, month, day, hour, minute, ts_hour );
		irr.set_location( lat, lon, tz );
		irr.set_sky_model( 2, alb );
		irr.set_beam_diffuse( dn, df );
		irr.set_surface( m_trackMode, m_tilt, m_azimuth, 45.0, m_shadeMode1x == 1, m_groundCoverageRatio );

		int irrRetCode = irr.calc();

		if ( irrRetCode != 0 ) {
			ShowFatalError("PVWatts: Failed to calculate plane of array irradiance with given input parameters.");
		}

		irr.get_sun( &out.solazi, &out.solzen, &out.solalt, 0, 0, 0, &out.sunup, 0, 0, 0 );
		irr.get_angles( &out.aoi, &out.stilt, &out.sazi, &out.rot, &out.btd );
		irr.get_poa( &out.ibeam, &out.iskydiff, &out.ignddiff, 0, 0, 0 );

		return out;
	}

	DCPowerOutput PVWattsGenerator::powerout(Real64 &shad_beam, Real64 shad_diff, Real64 dni, Real64 alb, Real64 wspd, Real64 tdry, IrradianceOutput& irr_st) {

		using DataGlobals::DegToRadians;
		using DataGlobals::RadToDeg;
		using General::RoundSigDigits;

		const Real64 &gcr = m_groundCoverageRatio;

		Real64 poa, tpoa, pvt, dc;


		if ( irr_st.sunup > 0 ) {
			if ( m_trackMode == 1 && m_shadeMode1x == 0 ) {
				Real64 shad1xf = shade_fraction_1x( irr_st.solazi, irr_st.solzen, m_tilt, m_azimuth, m_groundCoverageRatio, irr_st.rot);
				shad_beam *= 1 - shad1xf;

				if ( irr_st.iskydiff > 0 ) {
					Real64 reduced_skydiff = irr_st.iskydiff;
					Real64 Fskydiff = 1.0;
					Real64 reduced_gnddiff = irr_st.ignddiff;
					Real64 Fgnddiff = 1.0;

					// worst-case mask angle using calculated surface tilt
					Real64 phi0 = RadToDeg * std::atan2( std::sin(irr_st.stilt * DegToRadians), 1.0 / m_groundCoverageRatio - std::cos(irr_st.stilt * DegToRadians) );

					// calculate sky and gnd diffuse derate factors
					// based on view factor reductions from self-shading
					diffuse_reduce( irr_st.solzen, irr_st.stilt,
								   dni, irr_st.iskydiff+irr_st.ignddiff,
								   gcr, phi0, alb, 1000,

								   // outputs (pass by reference)
								   reduced_skydiff, Fskydiff,
								   reduced_gnddiff, Fgnddiff );

					if ( Fskydiff >= 0 && Fskydiff <= 1 ) irr_st.iskydiff *= Fskydiff;
					else ShowWarningError("PVWatts: sky diffuse reduction factor invalid: fskydiff=" + RoundSigDigits(Fskydiff, 7) + ", stilt=" + RoundSigDigits(irr_st.stilt, 7));

					if ( Fgnddiff >= 0 && Fgnddiff <= 1 ) irr_st.ignddiff *= Fgnddiff;
					else ShowWarningError("PVWatts: gnd diffuse reduction factor invalid: fgnddiff=" + RoundSigDigits(Fgnddiff, 7) + ", stilt=" + RoundSigDigits(irr_st.stilt, 7));
				}
			}

			// apply hourly shading factors to beam (if none enabled, factors are 1.0)
			irr_st.ibeam *= shad_beam;

			// apply sky diffuse shading factor (specified as constant, nominally 1.0 if disabled in UI)
			irr_st.iskydiff *= shad_diff;

			poa = irr_st.ibeam + irr_st.iskydiff + irr_st.ignddiff;

			Real64 wspd_corr = wspd < 0 ? 0 : wspd;

			// module cover
			tpoa = poa;
			if ( irr_st.aoi > 0.5 && irr_st.aoi < 89.5 )
			{
				double mod = iam( irr_st.aoi, m_useARGlass );
				tpoa = poa - ( 1.0 - mod )*dni*cosd(irr_st.aoi);
				if( tpoa < 0.0 ) tpoa = 0.0;
				if( tpoa > poa ) tpoa = poa;
			}

			// cell temperature
			pvt = (*m_tccalc)( poa, wspd_corr, tdry );

			// dc power output (Watts)
			dc = m_dcSystemCapacity * ( 1.0 + m_gamma * ( pvt - 25.0 ) ) * tpoa / 1000.0;

			// dc losses
			dc *= 1.0 - m_systemLosses;

		} else {
			poa = 0.0;
			tpoa = 0.0;
			pvt = tdry;
			dc = 0.0;
		}

		DCPowerOutput pwrOutput = { poa, tpoa, pvt, dc };

		return pwrOutput;
	}

	PVWattsGenerator& GetOrCreatePVWattsGenerator(std::string const & GeneratorName) {
		// Find the generator, and create a new one if it hasn't been loaded yet.
		int ObjNum = InputProcessor::GetObjectItemNum("Generator:PVWatts", InputProcessor::MakeUPPERCase(GeneratorName));
		assert(ObjNum >= 0);
		if (ObjNum == 0) {
			ShowFatalError("Cannot find Generator:PVWatts " + GeneratorName);
		}
		auto it = PVWattsGenerators.find(ObjNum);
		if ( it == PVWattsGenerators.end() ) {
			// It's not in the map, add it.
			PVWattsGenerators.insert(std::make_pair(ObjNum, PVWattsGenerator::createFromIdfObj(ObjNum)));
			return PVWattsGenerators.find(ObjNum)->second;
		} else {
			return it->second;
		}
	}

	static const int __nday[12] = {31,28,31,30,31,30,31,31,30,31,30,31};

	static int julian(int yr,int month,int day)    /* Calculates julian day of year */
	{
		int i=1,jday=0,k;

		if( yr%4 == 0 )                      /* For leap years */
			k = 1;
		else
			k = 0;

		while( i < month )
		{
			jday = jday + __nday[i-1];
			i++;
		}
		if( month > 2 )
			jday = jday + k + day;
		else
			jday = jday + day;
		return(jday);
	}

	static int day_of_year( int month, int day_of_month ) /* returns 1-365 */
	{
		int i=1,iday=0;

		while ( i < month )
			iday += __nday[i++ - 1];

		return iday + day_of_month;
	}

	static double vec_dot(double a[3], double b[3])
	{
		return a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
	}

	static void vec_cross(double a[3], double b[3], double result[3])
	{
		result[0] = a[1] * b[2] - a[2] * b[1];
		result[1] = a[2] * b[0] - a[0] * b[2];
		result[2] = a[0] * b[1] - a[1] * b[0];
	}

	static void vec_diff( double a[3], double b[3], double result[3] )
	{
		result[0] = a[0] - b[0];
		result[1] = a[1] - b[1];
		result[2] = a[2] - b[2];
	}

	static void sun_unit( double sazm, double szen, double sun[3] )
	{
		//Get unit vector in direction of sun
		double solalt = 90 - szen;

		if ( sazm >= 0 && sazm <= 90 )
		{
			sun[0] = cosd(solalt)*sind(sazm);
			sun[1] = cosd(solalt)*cosd(sazm);
		}
		else if ( sazm > 90 && sazm <= 180 )
		{
			sun[0] = cosd(solalt)*sind(180-sazm);
			sun[1] = -cosd(solalt)*cosd(180-sazm);
		}
		else if ( sazm > 180 && sazm <= 270 )
		{
			sun[0] = -cosd(solalt)*sind(sazm-180);
			sun[1] = -cosd(solalt)*cosd(sazm-180);
		}
		else
		{
			sun[0] = -cosd(solalt)*sind(360-sazm);
			sun[1] = cosd(solalt)*cosd(360-sazm);
		}

		sun[2] = sind(solalt);

		//normalize
		double magnitude = sqrt(sun[0]*sun[0] + sun[1]*sun[1] + sun[2]*sun[2]);
		sun[0] = sun[0] / magnitude;
		sun[1] = sun[1] / magnitude;
		sun[2] = sun[2] / magnitude;
	}

	static void get_vertices( double axis_tilt, double axis_azimuth, double gcr, double vertices[3][4][3], double rotation)
	{
		//Get panel vertices for flat panels, no tilt or azimuth,
		//ordered ccw starting from x+
		//vertices[0] is panel 0
		//vertices[0][1] is corner 1 on panel 0
		//vertices[0][1][2] is coordinate 2(z) for corner 1 on panel 0.
		//All are 0-indexed.  0=x, 1=y, 2=z.

		double width = 1.0;
		double row_spacing = 1.0/gcr - 1.0;
		double length = 10.0;

		for (int i=0; i<3; i++)
		{
			vertices[i][0][0] = width/2 + i*(row_spacing + width);
			vertices[i][0][1] = 0;
			vertices[i][0][2] = 0;

			vertices[i][1][0] = width/2 + i*(row_spacing + width);
			vertices[i][1][1] = length;
			vertices[i][1][2] = 0;

			vertices[i][2][0] = -width/2 + i*(row_spacing + width);
			vertices[i][2][1] = length;
			vertices[i][2][2] = 0;

			vertices[i][3][0] = -width/2 + i*(row_spacing + width);
			vertices[i][3][1] = 0;
			vertices[i][3][2] = 0;
		}
		//We now have vertices for flat panels spaced evenly along the x+ axis.

		//Rotate each panel by rotation angle
		for (int i=0; i<3; i++)
		{
			//Move so that we rotate about y-axis.
			//Perform rotation, then move back.
			double offset = i*(row_spacing + width);

			vertices[i][0][0] = vertices[i][0][0] - offset;
			vertices[i][1][0] = vertices[i][1][0] - offset;
			vertices[i][2][0] = vertices[i][2][0] - offset;
			vertices[i][3][0] = vertices[i][3][0] - offset;

			//Rotation matrix T is
			//  cos 0 sin
			//   0  1  0
			// -sin 0 cos
			// for each vertex v in vector form, set v=Tv. (using matrix multiplication)
			for(int j=0; j<4; j++)
			{
				//When we do calculations for new coords, they all depend on old coords.
				double oldVertX = vertices[i][j][0]; //Z coord depends on original y coord.
				double oldVertZ = vertices[i][j][2];
				vertices[i][j][0] = oldVertX * cosd(rotation) + oldVertZ * sind(rotation);
				vertices[i][j][2] = oldVertX * -sind(rotation) + oldVertZ * cosd(rotation);
			}

			//Translate back to original location after rotation is complete.
			vertices[i][0][0] = vertices[i][0][0] + offset;
			vertices[i][1][0] = vertices[i][1][0] + offset;
			vertices[i][2][0] = vertices[i][2][0] + offset;
			vertices[i][3][0] = vertices[i][3][0] + offset;
		}


		//Now globally rotate all coords by axis tilt
		for (int i=0; i<3; i++)
		{
			//Move to rotate about x axis.
			//Perform rotation, then move back.
			double offset = length;

			vertices[i][0][1] = vertices[i][0][1] - offset;
			vertices[i][1][1] = vertices[i][1][1] - offset;
			vertices[i][2][1] = vertices[i][2][1] - offset;
			vertices[i][3][1] = vertices[i][3][1] - offset;

			//Rotation matrix T is
			// 1   0   0
			// 0  cos sin
			// 0 -sin cos
			// for each vertex v in vector form, set v=Tv. (using matrix multiplication)
			for (int j=0; j<4; j++)
			{
				//When we do calculations for new coords, they all depend on old coords.
				double oldVertY = vertices[i][j][1]; //Z coord depends on original y coord.
				double oldVertZ = vertices[i][j][2];
				vertices[i][j][1] = oldVertY * cosd(axis_tilt) + oldVertZ * sind(axis_tilt);
				vertices[i][j][2] = oldVertY * -sind(axis_tilt) + oldVertZ * cosd(axis_tilt);
			}

			vertices[i][0][1] = vertices[i][0][1] + offset;
			vertices[i][1][1] = vertices[i][1][1] + offset;
			vertices[i][2][1] = vertices[i][2][1] + offset;
			vertices[i][3][1] = vertices[i][3][1] + offset;
		}


		//Now globally rotate all coords by axis azimuth
		for (int i=0; i<3; i++)
		{
			//We are rotating about the Z axis, so we don't need to translate.

			//Rotation matrix T is
			//  cos sin 0
			// -sin cos 0
			//   0   0  1
			// for each vertex v in vector form, set v=Tv. (using matrix multiplication)
			for (int j=0; j<4; j++)
			{
				//When we do calculations for new coords, they all depend on old coords.
				double oldVertX = vertices[i][j][0]; //Z coord depends on original y coord.
				double oldVertY = vertices[i][j][1];
				vertices[i][j][0] = oldVertX * cosd(axis_azimuth) + oldVertY * sind(axis_azimuth);
				vertices[i][j][1] = oldVertX * -sind(axis_azimuth) + oldVertY * cosd(axis_azimuth);
			}
		}
	}

	pvwatts_celltemp::pvwatts_celltemp( double _inoct, double _height, const double& _dTimeHrs) : dtime(_dTimeHrs)
	{
		/* constants */
		boltz = 0.00000005669;
		cap = 0;
		capo = 11000.0;
		convrt = 0;
		absorb=0.83;
		emmis=0.84;
		tgrat=0;
		tgrnd=0;
		xlen=0.5;

		/* configuration parameters */
		inoct = _inoct;
		height = _height;

		/* initial values */
		suno=0.0;
		tmodo=293.15;

		/* convective coefficient at noct */
		windmd=1.0;
		tave=(inoct+293.15)/2.0;
		denair=0.003484*101325.0/tave;
		visair=0.24237e-6*pow(tave,0.76)/denair;
		conair=2.1695e-4*pow(tave,0.84);
		reynld=windmd*xlen/visair;
		hforce=0.8600/pow(reynld,0.5)*denair*windmd*1007.0/pow(0.71,0.67);
		grashf=9.8/tave*(inoct-293.15)*pow(xlen,3.0)/pow(visair,2.0)*0.5;
		hfree=0.21*pow(grashf*0.71,0.32)*conair/xlen;
		hconv=pow(pow(hfree,3.0)+pow(hforce,3.0),1.0/3.0);

		/* Determine the ground temperature ratio and the ratio of
		 the total convection to the top side convection */
		hgrnd=emmis*boltz*(pow(inoct,2.0)+pow(293.15,2.0))*(inoct+293.15);
		backrt=( absorb*800.0-emmis*boltz*(pow(inoct,4.0)-pow(282.21,4.0))
				-hconv*(inoct-293.15) )/((hgrnd+hconv)*(inoct-293.15));
		tgrnd=pow(pow(inoct,4.0)-backrt*(pow(inoct,4.0)-pow(293.15,4.0)),0.25);
		if( tgrnd > inoct)
			tgrnd=inoct;
		if( tgrnd < 293.15)
			tgrnd=293.15;
		tgrat=(tgrnd-293.15)/(inoct-293.15);
		convrt=(absorb*800.0-emmis*boltz*(2.0*pow(inoct,4.0)-pow(282.21,4.0)
										  -pow(tgrnd,4.0)))/(hconv*(inoct-293.15));

		/* Adjust the capacitance of the module based on the inoct */
		cap=capo;
		if( inoct > 321.15)
			cap=cap*(1.0+(inoct-321.15)/12.0);

	}

	double pvwatts_celltemp::operator() ( double poa2, double ws2, double ambt2, double fhconv )
	{
		double celltemp = ambt2;

		/* If poa is gt 0 then compute cell temp, else set to 999 */
		if( poa2 > 0.0 )
		{        /* Initialize local variables for insolation and temp */
			tamb=ambt2+273.15;
			suun=poa2*absorb;
			tsky=0.68*(0.0552*pow(tamb,1.5))+0.32*tamb;  /* Estimate sky temperature */

			/*  Estimate wind speed at module height - use technique developed by
			 menicucci and hall (sand84-2530) */
			windmd=ws2*pow(height/9.144,0.2) + 0.0001;
			/* Find overall convective coefficient */
			tmod=tmodo;
			for(j=0;j<=9;j++)
			{
				tave=(tmod+tamb)/2.0;
				denair=0.003484*101325.0/tave;
				visair=0.24237e-6*pow(tave,0.76)/denair;
				conair=2.1695e-4*pow(tave,0.84);
				reynld=windmd*xlen/visair;
				hforce=0.8600/pow(reynld,0.5)*denair*windmd*1007.0/pow(0.71,0.67);
				if(reynld > 1.2e5)
					hforce=0.0282/pow(reynld,0.2)*denair*windmd*1007.0/pow(0.71,0.4);
				grashf=9.8/tave*fabs(tmod-tamb)*pow(xlen,3.0)/pow(visair,2.0)*0.5;
				hfree=0.21*pow(grashf*0.71,0.32)*conair/xlen;
				hconv=fhconv*convrt*pow(pow(hfree,3.0)+pow(hforce,3.0),1.0/3.0);
				/* Solve the heat transfer equation */
				hsky=emmis*boltz*(pow(tmod,2.0)+pow(tsky,2.0))*(tmod+tsky);
				tgrnd=tamb+tgrat*(tmod-tamb);
				hgrnd=emmis*boltz*(tmod*tmod+tgrnd*tgrnd)*(tmod+tgrnd);
				eigen=-(hconv+hsky+hgrnd)/cap*dtime*3600.0;
				ex=0.0;
				if(eigen > -10.0)
					ex=exp(eigen);
				tmod=tmodo*ex+((1.0-ex)*(hconv*tamb+hsky*tsky+hgrnd*tgrnd
										 +suno+(suun-suno)/eigen)+suun-suno)/(hconv+hsky+hgrnd);
			}

			tmodo=tmod;  /* Save the new values as initial values for the next hour */
			suno=suun;

			celltemp = tmod-273.15;  /* PV module temperature in degrees C */
		}
		else
		{
			/* sun down, save module temp = ambient, poa = 0  (apd 2/24/2012) */
			tmodo = ambt2+273.15;
			suno = 0;
		}

		return celltemp;
	}

	void pvwatts_celltemp::set_last_values( double Tc, double poa )
	{
		tmodo = Tc+273.15;
		suno = poa*absorb;
	}

	irrad::irrad()
	{
		year=month=day=hour = -999;
		minute=delt=lat=lon=tz=-999;
		radmode=skymodel=track = -1;
		gh=dn=df=alb=tilt=sazm=rlim=-999;

		for (int i=0;i<9;i++) sun[i] = std::numeric_limits<double>::quiet_NaN();
		angle[0]=angle[1]=angle[2]=angle[3]=angle[4]= std::numeric_limits<double>::quiet_NaN();
		poa[0]=poa[1]=poa[2]=diffc[0]=diffc[1]=diffc[2] = std::numeric_limits<double>::quiet_NaN();
		tms[0]=tms[1]=tms[2] = -999;
		gcr=std::numeric_limits<double>::quiet_NaN();
		en_backtrack = false;
		ghi = std::numeric_limits<double>::quiet_NaN();
	}

	int irrad::check()
	{
		if (year < 0 || month < 0 || day < 0 || hour < 0 || minute < 0 || delt > 1) return -1;
		if ( lat < -90 || lat > 90 || lon < -180 || lon > 180 || tz < -15 || tz > 15 ) return -2;
		if ( radmode < DN_DF || radmode > POA_P || skymodel < 0 || skymodel > 2 ) return -3;
		if ( track < 0 || track > 4 ) return -4;
		if ( radmode == DN_DF && (dn < 0 || dn > 1500 || df < 0 || df > 1500)) return -5;
		if ( radmode == DN_GH && (gh < 0 || gh > 1500 || dn < 0 || dn > 1500)) return -6;
		if ( alb < 0 || alb > 1 ) return -7;
		if ( tilt < 0 || tilt > 90 ) return -8;
		if ( sazm < 0 || sazm >= 360 ) return -9;
		if ( rlim < -90 || rlim > 90 ) return -10;
		if ( radmode == GH_DF && (gh < 0 || gh > 1500 || df < 0 || df > 1500)) return -11;
		return 0;
	}

	double irrad::get_ghi()
	{
		return ghi;
	}

	double irrad::get_sunpos_calc_hour()
	{
		return ((double)tms[0]) + ((double)tms[1])/60.0;
	}

	void irrad::get_sun( double *solazi,
						double *solzen,
						double *solelv,
						double *soldec,
						double *sunrise,
						double *sunset,
						int *sunup,
						double *eccfac,
						double *tst,
						double *hextra )
	{
		if ( solazi != 0 ) *solazi = sun[0] * (180/DataGlobals::Pi);
		if ( solzen != 0 ) *solzen = sun[1] * (180/DataGlobals::Pi);
		if ( solelv != 0 ) *solelv = sun[2] * (180/DataGlobals::Pi);
		if ( soldec != 0 ) *soldec = sun[3] * (180/DataGlobals::Pi);
		if ( sunrise != 0 ) *sunrise = sun[4];
		if ( sunset != 0 ) *sunset = sun[5];
		if ( sunup != 0 ) *sunup = tms[2];
		if ( eccfac != 0 ) *eccfac = sun[6];
		if ( tst != 0 ) *tst = sun[7];
		if ( hextra != 0 ) *hextra = sun[8];
	}

	void irrad::get_angles( double *aoi,
						   double *surftilt,
						   double *surfazi,
						   double *axisrot,
						   double *btdiff )
	{
		if ( aoi != 0 ) *aoi = angle[0] * (180/DataGlobals::Pi);
		if ( surftilt != 0 ) *surftilt = angle[1] * (180/DataGlobals::Pi);
		if ( surfazi != 0 ) *surfazi = angle[2] * (180/DataGlobals::Pi);
		if ( axisrot != 0 ) *axisrot = angle[3] * (180/DataGlobals::Pi);
		if ( btdiff != 0 ) *btdiff = angle[4] * (180/DataGlobals::Pi);
	}

	void irrad::get_poa( double *beam, double *skydiff, double *gnddiff,
						double *isotrop, double *circum, double *horizon )
	{
		if ( beam != 0 ) *beam = poa[0];
		if ( skydiff != 0 ) *skydiff = poa[1];
		if ( gnddiff != 0 ) *gnddiff = poa[2];
		if ( isotrop != 0 ) *isotrop = diffc[0];
		if ( circum != 0 ) *circum = diffc[1];
		if ( horizon != 0 ) *horizon = diffc[2];
	}

	void irrad::get_irrad (double *ghi, double *dni, double *dhi){
		*ghi = gh;
		*dni = dn;
		*dhi = df;
	}

	void irrad::set_time( int year, int month, int day, int hour, double minute, double delt_hr )
	{
		this->year = year;
		this->month = month;
		this->day = day;
		this->hour = hour;
		this->minute = minute;
		this->delt = delt_hr;
	}

	void irrad::set_location( double lat, double lon, double tz )
	{
		this->lat = lat;
		this->lon = lon;
		this->tz = tz;
	}

	void irrad::set_sky_model( int skymodel, double albedo )
	{
		this->skymodel = skymodel;
		this->alb = albedo;
	}

	void irrad::set_surface( int tracking, double tilt_deg, double azimuth_deg, double rotlim_deg, bool en_backtrack, double gcr )
	{
		this->track = tracking;
		if (tracking == 4)
			this->track = 0; //treat timeseries tilt as fixed tilt
		this->tilt = tilt_deg;
		this->sazm = azimuth_deg;
		this->rlim = rotlim_deg;
		this->en_backtrack = en_backtrack;
		this->gcr = gcr;
	}

	void irrad::set_beam_diffuse( double beam, double diffuse )
	{
		this->dn = beam;
		this->df = diffuse;
		this->radmode = DN_DF;
	}

	void irrad::set_global_beam( double global, double beam )
	{
		this->gh = global;
		this->dn = beam;
		this->radmode = DN_GH;
	}

	void irrad::set_global_diffuse(double global, double diffuse)
	{
		this->gh = global;
		this->df = diffuse;
		this->radmode = GH_DF;
	}

	void irrad::set_poa_reference( double poa, poaDecompReq* pA){
		this->wfpoa = poa;
		this->radmode = POA_R;
		this->poaAll = pA;
	}
	void irrad::set_poa_pyranometer( double poa, poaDecompReq* pA ){
		this->wfpoa = poa;
		this->radmode = POA_P;
		this->poaAll = pA;
	}


	int irrad::calc()
	{
		int code = check();
		if ( code < 0 )
			return -100+code;
		/*
		 calculates effective sun position at current timestep, with delt specified in hours

		 sun: results from solarpos
		 tms: [0]  effective hour of day used for sun position
		 [1]  effective minute of hour used for sun position
		 [2]  is sun up?  (0=no, 1=midday, 2=sunup, 3=sundown)
		 angle: result from incidence
		 poa: result from sky model
		 diff: broken out diffuse components from sky model

		 lat, lon, tilt, sazm, rlim: angles in degrees
		 */
		double t_cur = hour + minute/60.0;

		// calculate sunrise and sunset hours in local standard time for the current day
		solarpos( year, month, day, 12, 0.0, lat, lon, tz, sun );

		double t_sunrise = sun[4];
		double t_sunset = sun[5];

		// recall: if delt <= 0.0, do not interpolate sunrise and sunset hours, just use specified time stamp
		if ( delt > 0
			&& t_cur >= t_sunrise - delt/2.0
			&& t_cur < t_sunrise + delt/2.0 )
		{
			// time step encompasses the sunrise
			double t_calc = (t_sunrise + (t_cur+delt/2.0))/2.0; // midpoint of sunrise and end of timestep
			int hr_calc = (int)t_calc;
			double min_calc = (t_calc-hr_calc)*60.0;

			tms[0] = hr_calc;
			tms[1] = (int)min_calc;

			solarpos( year, month, day, hr_calc, min_calc, lat, lon, tz, sun );

			tms[2] = 2;
		}
		else if ( delt > 0
				 && t_cur > t_sunset - delt/2.0
				 && t_cur <= t_sunset + delt/2.0 )
		{
			// timestep encompasses the sunset
			double t_calc = ( (t_cur-delt/2.0) + t_sunset )/2.0; // midpoint of beginning of timestep and sunset
			int hr_calc = (int)t_calc;
			double min_calc = (t_calc-hr_calc)*60.0;

			tms[0] = hr_calc;
			tms[1] = (int)min_calc;

			solarpos( year, month, day, hr_calc, min_calc, lat, lon, tz, sun );

			tms[2] = 3;
		}
		else if (t_cur >= t_sunrise && t_cur <= t_sunset)
		{
			// timestep is not sunrise nor sunset, but sun is up  (calculate position at provided t_cur)
			tms[0] = hour;
			tms[1] = (int)minute;
			solarpos( year, month, day, hour, minute, lat, lon, tz, sun );
			tms[2] = 1;
		}
		else
		{
			// sun is down, assign sundown values
			sun[0] = -999*DTOR; //avoid returning a junk azimuth angle (return in radians)
			sun[1] = -999*DTOR; //avoid returning a junk zenith angle (return in radians)
			sun[2] = -999*DTOR; //avoid returning a junk elevation angle (return in radians)
			tms[0] = 0;
			tms[1] = 0;
			tms[2] = 0;
		}


		poa[0]=poa[1]=poa[2] = 0;
		diffc[0]=diffc[1]=diffc[2] = 0;
		angle[0]=angle[1]=angle[2]=angle[3]=angle[4] = 0;

		ghi = 0;

		// do irradiance calculations if sun is up

		if (tms[2] > 0)
		{
			// compute incidence angles onto fixed or tracking surface
			incidence( track, tilt, sazm, rlim, sun[1], sun[0], en_backtrack, gcr, angle );

			if(radmode < POA_R){  // Sev 2015-09-11 - Run this code if no POA decomposition is required
				double hextra = sun[8];
				double hbeam = dn*cos( sun[1] ); // calculated beam on horizontal surface: sun[1]=zenith

				// check beam irradiance against extraterrestrial irradiance
				if ( hbeam > hextra )
				{
					//beam irradiance on horizontal W/m2 exceeded calculated extraterrestrial irradiance
					return -1;
				}

				// compute beam and diffuse inputs based on irradiance inputs mode
				//ibeam and idiff in this calculation are DNI and DHI, they are NOT in the plane of array! those are poa[0-2]!!!
				double ibeam = dn;
				double idiff = 0.0;
				if (radmode == DN_DF)  // Beam+Diffuse
				{
					idiff = df;
					ibeam = dn;
				}
				else if (radmode == DN_GH) // Total+Beam
				{
					idiff = gh - hbeam;
					if (idiff < 0) idiff = 0; //error checking added 12/18/15 jmf to prevent negative dh values if input data is bad
					ibeam = dn;
				}
				else if (radmode == GH_DF) //Total+Diffuse
				{
					idiff = df;
					ibeam = (gh - df) / cos(sun[1]); //compute beam from total, diffuse, and zenith angle
					if (ibeam > 1500) ibeam = 1500; //error checking on computation
					if (ibeam < 0) ibeam = 0; //error checking on computation
				}
				else
					return -2; // just in case of a weird error


				// compute incident irradiance on tilted surface
				switch( skymodel )
				{
					case 0:
						isotropic( hextra, ibeam, idiff, alb, angle[0], angle[1], sun[1], poa, diffc );
						break;
					case 1:
						hdkr( hextra, ibeam, idiff, alb, angle[0], angle[1], sun[1], poa, diffc );
						break;
					default:
						perez( hextra, ibeam, idiff, alb, angle[0], angle[1], sun[1], poa, diffc );
						break;
				}

				ghi = idiff;
			}
			else { // Sev 2015/09/11 - perform a POA decomp.
				poaDecomp( wfpoa, angle, sun, alb, poaAll, dn, df, gh, poa, diffc);
			}
		} else { gh=0; dn=0; df=0;}

		return 0;

	}

	//Pass a PV system, sun zenith, sun azimuth
	//Return fraction shaded [0...1] if system is shaded
	//False otherwise
	double shade_fraction_1x( double solazi, double solzen,
							 double axis_tilt, double axis_azimuth,
							 double gcr, double rotation )
	{
		//Get unit vector in direction of sun

		double sun[3];
		sun_unit( solazi, solzen, sun );

		//For now, assume array has at least 3 rows.
		//This way we can use index 1 and it has a panel on both sides.

		//Get our vertices for our array.
		double verts[3][4][3]; //To allocate
		get_vertices( axis_tilt, axis_azimuth, gcr, verts, rotation );

		//Find which panel is in the direction of the sun by using dot product.
		//toPrev is a vector from panel 1 to panel 0.
		//toNext is a vector from panel 1 to panel 2.
		//The sun is in the direction of the panel whose vector has a larger positive
		//dot product with the sun direction vector.
		//Store the panel in the direction of the sun from panel 1 in the variable iPanel.
		int iPanel = 0;
		double toPrev[3];
		toPrev[0] = verts[0][0][0] - verts[1][0][0];
		toPrev[1] = verts[0][0][1] - verts[1][0][1];
		toPrev[2] = verts[0][0][2] - verts[1][0][2];
		double toNext[3];
		toNext[0] = verts[2][0][0] - verts[1][0][0];
		toNext[1] = verts[2][0][1] - verts[1][0][1];
		toNext[2] = verts[2][0][2] - verts[1][0][2];
		if (vec_dot(toPrev, sun) < vec_dot(toNext, sun)) iPanel = 2;


		//Get midpoint of edge of panel 1 on the sun side.
		//This edge is on the same side of panel 1 as iPanel.
		//Store midpoint in midP.
		double midP[3];
		for (int i=0; i<3; i++)
		{
			if (iPanel == 0) midP[i] = (verts[1][2][i] + verts[1][3][i]) / 2;
			else midP[i] = (verts[1][0][i] + verts[1][1][i]) / 2;
		}

		//Get normal vector to plane for iPanel.
		//This is easy - just get two vectors in the plane of iPanel and cross them.
		//Use the vectors along the edges out of vertex 0.  That is, edge01 and edge03.
		//Store the result in a vector called normal.
		double normal[3],a1[3],a2[3];
		vec_diff(verts[iPanel][1], verts[iPanel][0], a1);
		vec_diff(verts[iPanel][3], verts[iPanel][0], a2);
		vec_cross(a1, a2, normal);


		//We want to find the point of intersection of the ray that starts at midP
		//and goes in the direction of the sun.
		//Assume sun is at infinity in direction 'sun'.
		//First make sure that this ray has a unique intersection point.
		double sunDot = vec_dot(normal, sun);
		if (fabs(sunDot) < 0.001) return 0; //sun vector lies in plane

		//Now, vector pDir goes from midP to any point in the plane of iPanel.  Vertex 0, say.
		//Project pDir onto the normal to iPanel, and project the sun vector onto the normal vector.
		//The ratio of these projections tells us how fat to move along the sun vector, starting
		//from midP, to get a point in the plane of iPanel.
		double pDir[3];
		vec_diff(verts[iPanel][0], midP, pDir);
		double t = vec_dot(normal, pDir) / vec_dot(normal, sun);
		if (t < 0) return 0; // sun has set (is behind array).

		double intersectP[3];
		for (int i=0; i<3; i++)
		{
			intersectP[i] = midP[i] + t * sun[i];
		}
		//intersectP is along the ray from midP to the sun, and lies in the (infinite) plane of iPanel.

		//Figure out if intersectP is inside the bounds of the iPanel.
		//This is simple.  If intersectP is on the same side of the edge as the panel for both edges,
		//then it is in the panel.
		//Find a vector from edge to intersectP and take dot product with vector from edge through panel.
		//If dot product is positive, intersectP is on the panel side of that edge.
		//Reuse a1 and a2 from above (as temporary vectors).
		vec_diff(verts[iPanel][3], verts[iPanel][0], a1);
		vec_diff(intersectP, verts[iPanel][0], a2);
		if (vec_dot(a1, a2) < 0) return 0; //intersect is outside panel bounds.

		vec_diff(verts[iPanel][0], verts[iPanel][3], a1);
		vec_diff(intersectP, verts[iPanel][3], a2);
		if (vec_dot(a1, a2) < 0) return 0; //intersect is outside panel bounds.

		// Now we know the panel is shaded, so compute geometric shade fraction
		double mu[3] = { 0, 0, 0 }; // upper edge midpoint on adjacent panel
		double ml[3] = { 0, 0, 0 }; // lower edge midpoint on adjacent panel
		for (int i=0; i<3; i++)
		{
			if (iPanel == 2) mu[i] = (verts[iPanel][2][i] + verts[iPanel][3][i]) / 2;
			else             mu[i] = (verts[iPanel][0][i] + verts[iPanel][1][i]) / 2;

			if (iPanel == 2) ml[i] = (verts[iPanel][0][i] + verts[iPanel][1][i]) / 2;
			else             ml[i] = (verts[iPanel][2][i] + verts[iPanel][3][i]) / 2;

		}

		vec_diff(intersectP, mu, a1);
		vec_diff(ml, mu, a2);
		double maga2 = vec_dot(a2,a2);
		double Ab = vec_dot(a1, a2)/maga2;  // geometric shading fraction [0..1]

		return Ab;
	}

	void diffuse_reduce(// inputs (angles in degrees)
						double solzen,
						double stilt,
						double Gb_nor,
						double Gd_poa,
						double gcr,
						double phi0, // mask angle
						double alb,
						double nrows,

						// outputs
						double &reduced_skydiff,
						double &Fskydiff,  // derate factor on sky diffuse
						double &reduced_gnddiff,
						double &Fgnddiff) // derate factor on ground diffuse
	{
		if (Gd_poa < 0.1)
		{
			Fskydiff = Fgnddiff = 1.0;
			return;
		}

		// view factor calculations assume isotropic sky
		double Gd = Gd_poa; // total plane-of-array diffuse
		double Gdh = Gd * 2 / (1 + cosd(stilt)); // total
		double Gbh = Gb_nor * cosd(solzen); // beam irradiance on horizontal surface

		// sky diffuse reduction
		reduced_skydiff = Gd - Gdh*(1 - pow(cosd(phi0 / 2), 2))*(nrows - 1.0) / nrows;
		Fskydiff = reduced_skydiff / Gd;

		double B = 1.0;
		double R = B / gcr;

		double solalt = 90 - solzen;

		// ground reflected reduction
		double F1 = alb * pow(sind(stilt / 2.0), 2);
		double Y1 = R - B * sind(180.0 - solalt - stilt) / sind(solalt);
		Y1 = max(0.00001, Y1); // constraint per Chris 4/23/12
		double F2 = 0.5 * alb * (1.0 + Y1 / B - sqrt(pow(Y1, 2) / pow(B, 2) - 2 * Y1 / B * cosd(180 - stilt) + 1.0));
		double F3 = 0.5 * alb * (1.0 + R / B - sqrt(pow(R, 2) / pow(B, 2) - 2 * R / B * cosd(180 - stilt) + 1.0));

		double Gr1 = F1 * (Gbh + Gdh);
		reduced_gnddiff = ((F1 + (nrows - 1)*F2) / nrows) * Gbh
		+ ((F1 + (nrows - 1) * F3) / nrows) * Gdh;

		Fgnddiff = 1.0;
		if (Gr1 > 0)
			Fgnddiff = reduced_gnddiff / Gr1;
	}

	double iam( double theta, bool ar_glass )
	{
		if ( theta < AOI_MIN ) theta = AOI_MIN;
		if ( theta > AOI_MAX ) theta = AOI_MAX;

		double normal = iam_nonorm( 1, ar_glass );
		double actual = iam_nonorm( theta, ar_glass );
		return actual/normal;
	}

	void solarpos(int year,int month,int day,int hour,double minute,double lat,double lng,double tz,double sunn[9])
	{
		/* This function is based on a paper by Michalsky published in Solar Energy
		 Vol. 40, No. 3, pp. 227-235, 1988. It calculates solar position for the
		 time and location passed to the function based on the Astronomical
		 Almanac's Algorithm for the period 1950-2050. For data averaged over an
		 interval, the appropriate time passed is the midpoint of the interval.
		 (Example: For hourly data averaged from 10 to 11, the time passed to the
		 function should be 10 hours and 30 minutes). The exception is when the time
		 interval includes a sunrise or sunset. For these intervals, the appropriate
		 time should be the midpoint of the portion of the interval when the sun is
		 above the horizon. (Example: For hourly data averaged from 7 to 8 with a
		 sunrise time of 7:30, the time passed to the function should be 7 hours and
		 and 45 minutes).

		 Revised 5/15/98. Replaced algorithm for solar azimuth with one by Iqbal
		 so latitudes below the equator are correctly handled. Also put in checks
		 to allow an elevation of 90 degrees without crashing the program and prevented
		 elevation from exceeding 90 degrees after refraction correction.

		 This function calls the function julian to get the julian day of year.

		 List of Parameters Passed to Function:
		 year   = year (e.g. 1986)
		 month  = month of year (e.g. 1=Jan)
		 day    = day of month
		 hour   = hour of day, local standard time, (1-24, or 0-23)
		 minute = minutes past the hour, local standard time
		 lat    = latitude in degrees, north positive
		 lng    = longitude in degrees, east positive
		 tz     = time zone, west longitudes negative

		 sunn[]  = array of elements to return sun parameters to calling function
		 sunn[0] = azm = sun azimuth in radians, measured east from north, 0 to 2*pi
		 sunn[1] = 0.5*pi - elv = sun zenith in radians, 0 to pi
		 sunn[2] = elv = sun elevation in radians, -pi/2 to pi/2
		 sunn[3] = dec = sun declination in radians
		 sunn[4] = sunrise in local standard time (hrs), not corrected for refraction
		 sunn[5] = sunset in local standard time (hrs), not corrected for refraction
		 sunn[6] = Eo = eccentricity correction factor
		 sunn[7] = tst = true solar time (hrs)
		 sunn[8] = hextra = extraterrestrial solar irradiance on horizontal at particular time (W/m2)  */

		int jday,delta,leap;                           /* Local variables */
		double zulu,jd,time,mnlong,mnanom,
		eclong,oblqec,num,den,ra,dec,gmst,lmst,ha,elv,azm,refrac,
		E,ws,sunrise,sunset,Eo,tst;
		double arg,hextra,Gon,zen;

		jday = julian(year,month,day);       /* Get julian day of year */
		zulu = hour + minute/60.0 - tz;      /* Convert local time to zulu time */
		if( zulu < 0.0 )                     /* Force time between 0-24 hrs */
		{                                 /* Adjust julian day if needed */
			zulu = zulu + 24.0;
			jday = jday - 1;
		}
		else if( zulu > 24.0 )
		{
			zulu = zulu - 24.0;
			jday = jday + 1;
		}
		delta = year - 1949;
		leap = delta/4;
		jd = 32916.5 + delta*365 + leap + jday + zulu/24.0;
		time = jd - 51545.0;     /* Time in days referenced from noon 1 Jan 2000 */

		mnlong = 280.46 + 0.9856474*time;
		mnlong = fmod((double)mnlong,360.0);         /* Finds doubleing point remainder */
		if( mnlong < 0.0 )
			mnlong = mnlong + 360.0;          /* Mean longitude between 0-360 deg */

		mnanom = 357.528 + 0.9856003*time;
		mnanom = fmod((double)mnanom,360.0);
		if( mnanom < 0.0 )
			mnanom = mnanom + 360.0;
		mnanom = mnanom*DTOR;             /* Mean anomaly between 0-2pi radians */

		eclong = mnlong + 1.915*sin(mnanom) + 0.020*sin(2.0*mnanom);
		eclong = fmod((double)eclong,360.0);
		if( eclong < 0.0 )
			eclong = eclong + 360.0;
		eclong = eclong*DTOR;       /* Ecliptic longitude between 0-2pi radians */

		oblqec = ( 23.439 - 0.0000004*time )*DTOR;   /* Obliquity of ecliptic in radians */
		num = cos(oblqec)*sin(eclong);
		den = cos(eclong);
		ra  = atan(num/den);                         /* Right ascension in radians */
		if( den < 0.0 )
			ra = ra + DataGlobals::Pi;
		else if( num < 0.0 )
			ra = ra + 2.0*DataGlobals::Pi;

		dec = asin( sin(oblqec)*sin(eclong) );       /* Declination in radians */

		gmst = 6.697375 + 0.0657098242*time + zulu;
		gmst = fmod((double)gmst,24.0);
		if( gmst < 0.0 )
			gmst = gmst + 24.0;         /* Greenwich mean sidereal time in hours */

		lmst = gmst + lng/15.0;
		lmst = fmod((double)lmst,24.0);
		if( lmst < 0.0 )
			lmst = lmst + 24.0;
		lmst = lmst*15.0*DTOR;         /* Local mean sidereal time in radians */

		ha = lmst - ra;
		if( ha < -DataGlobals::Pi )
			ha = ha + 2*DataGlobals::Pi;
		else if( ha > DataGlobals::Pi )
			ha = ha - 2*DataGlobals::Pi;             /* Hour angle in radians between -pi and pi */

		lat = lat*DTOR;                /* Change latitude to radians */

		arg = sin(dec)*sin(lat) + cos(dec)*cos(lat)*cos(ha);  /* For elevation in radians */
		if( arg > 1.0 )
			elv = DataGlobals::Pi/2.0;
		else if( arg < -1.0 )
			elv = -DataGlobals::Pi/2.0;
		else
			elv = asin(arg);

		if( cos(elv) == 0.0 )
		{
			azm = DataGlobals::Pi;         /* Assign azimuth = 180 deg if elv = 90 or -90 */
		}
		else
		{                 /* For solar azimuth in radians per Iqbal */
			arg = ((sin(elv)*sin(lat)-sin(dec))/(cos(elv)*cos(lat))); /* for azimuth */
			if( arg > 1.0 )
				azm = 0.0;              /* Azimuth(radians)*/
			else if( arg < -1.0 )
				azm = DataGlobals::Pi;
			else
				azm = acos(arg);

			if( ( ha <= 0.0 && ha >= -DataGlobals::Pi) || ha >= DataGlobals::Pi )
				azm = DataGlobals::Pi - azm;
			else
				azm = DataGlobals::Pi + azm;
		}

		elv = elv/DTOR;          /* Change to degrees for atmospheric correction */
		if( elv > -0.56 )
			refrac = 3.51561*( 0.1594 + 0.0196*elv + 0.00002*elv*elv )/( 1.0 + 0.505*elv + 0.0845*elv*elv );
		else
			refrac = 0.56;
		if( elv + refrac > 90.0 )
			elv = 90.0*DTOR;
		else
			elv = ( elv + refrac )*DTOR ; /* Atmospheric corrected elevation(radians) */

		E = ( mnlong - ra/DTOR )/15.0;       /* Equation of time in hours */
		if( E < - 0.33 )   /* Adjust for error occuring if mnlong and ra are in quadrants I and IV */
			E = E + 24.0;
		else if( E > 0.33 )
			E = E - 24.0;

		arg = -tan(lat)*tan(dec);
		if( arg >= 1.0 )
			ws = 0.0;                         /* No sunrise, continuous nights */
		else if( arg <= -1.0 )
			ws = DataGlobals::Pi;                          /* No sunset, continuous days */
		else
			ws = acos(arg);                   /* Sunrise hour angle in radians */

		/* Sunrise and sunset in local standard time */
		sunrise = 12.0 - (ws/DTOR)/15.0 - (lng/15.0 - tz) - E;
		sunset  = 12.0 + (ws/DTOR)/15.0 - (lng/15.0 - tz) - E;

		Eo = 1.00014 - 0.01671*cos(mnanom) - 0.00014*cos(2.0*mnanom);  /* Earth-sun distance (AU) */
		Eo = 1.0/(Eo*Eo);                    /* Eccentricity correction factor */

		tst = hour + minute/60.0 + (lng/15.0 - tz) + E;  /* True solar time (hr) */

		/* 25aug2011 apd: addition of calculation of horizontal extraterrestrial irradiance */
		zen = 0.5*DataGlobals::Pi - elv;
		Gon = 1367*(1+0.033*cos( 360.0/365.0*day_of_year(month,day)*DataGlobals::Pi/180 )); /* D&B eq 1.4.1a, using solar constant=1367 W/m2 */
		if (zen > 0 && zen < DataGlobals::Pi/2) /* if sun is up */
			hextra = Gon*cos(zen); /* elevation is incidence angle (zen=90-elv) with horizontal */
		else if (zen == 0)
			hextra = Gon;
		else
			hextra = 0.0;

		sunn[0] = azm;                        /* Variables returned in array sunn[] */
		sunn[1] = zen;               /*  Zenith */
		sunn[2] = elv;
		sunn[3] = dec;
		sunn[4] = sunrise;
		sunn[5] = sunset;
		sunn[6] = Eo;
		sunn[7] = tst;
		sunn[8] = hextra;
	}

	void incidence(int mode,double tilt,double sazm,double rlim,double zen,double azm, bool en_backtrack, double gcr, double angle[5])
	{
		/* This function calculates the incident angle of direct beam radiation to a
		 surface for a given sun position, latitude, and surface orientation. The
		 modes available are fixed tilt, 1-axis tracking, and 2-axis tracking.
		 Azimuth angles are for N=0 or 2pi, E=pi/2, S=pi, and W=3pi/2.  8/13/98

		 List of Parameters Passed to Function:
		 mode         = 0 for fixed-tilt, 1 for 1-axis tracking, 2 for 2-axis tracking, 3 for azimuth-axis tracking, 4 for timeseries tilt tracking (in "set surface" function, this is set as mode 0)
		 tilt         = tilt angle of surface from horizontal in degrees (mode 0),
		 or tilt angle of tracker axis from horizontal in degrees (mode 1),
		 MUST BE FROM 0 to 90 degrees.
		 sazm         = surface azimuth in degrees of collector (mode 0), or surface
		 azimuth of tracker axis (mode 1) with axis azimuth directed from
		 raised to lowered end of axis if axis tilted.
		 rlim         = plus or minus rotation in degrees permitted by physical constraints
		 of tracker, range is 0 to 180 degrees.
		 zen          = sun zenith in radians, MUST BE LESS THAN PI/2
		 azm          = sun azimuth in radians, measured east from north
		 en_backtrack = enable backtracking, using Ground coverage ratio ( below )
		 gcr          = ground coverage ratio ( used for backtracking )

		 Parameters Returned:
		 angle[]  = array of elements to return angles to calling function
		 angle[0] = inc  = incident angle in radians
		 angle[1] = tilt = tilt angle of surface from horizontal in radians
		 angle[2] = sazm = surface azimuth in radians, measured east from north
		 angle[3] = rot = tracking axis rotation angle in radians, measured from surface normal of unrotating axis (only for 1 axis trackers)
		 angle[4] = btdiff = (rot - ideal_rot) will be zero except in case of backtracking for 1 axis tracking
		 */
		/* Local variables: rot is the angle that the collector is rotated about the
		 axis when viewed from the raised end of the 1-axis tracker. If rotated
		 counter clockwise the angle is negative. Range is -180 to +180 degrees.
		 When xsazm = azm : rot = 0, tilt = xtilt, and sazm = xsazm = azm  */

		double arg,inc=0,xsazm,xtilt,rot=0,btdiff=0;

		if (mode == 4)
			mode = 0; //treat timeseries tilt as fixed tilt for each timestep

		switch ( mode )
		{
			case 0:              /* Fixed-Tilt, */
			case 3:              /* or Azimuth Axis*/
				tilt = tilt*DTOR;    /* Change tilt and surface azimuth to radians */
				sazm = (mode==0) ? sazm*DTOR : azm; /* either fixed surface azimuth or solar azimuth */
				arg = sin(zen)*cos(azm-sazm)*sin(tilt) + cos(zen)*cos(tilt);
				rot = 0;
				if( arg < -1.0 )
					inc = DataGlobals::Pi;
				else if( arg > 1.0  )
					inc = 0.0;
				else
					inc = acos(arg);
				break;
			case 1:                 /* One-Axis Tracking */
				xtilt = tilt*DTOR;   /* Change axis tilt, surface azimuth, and rotation limit to radians */
				xsazm = sazm*DTOR;
				rlim  = rlim*DTOR;
				/* Find rotation angle of axis for peak tracking */
				if( fabs( cos(xtilt) ) < 0.001745 )    /* 89.9 to 90.1 degrees */
				{          /* For vertical axis only */
					if( xsazm <= DataGlobals::Pi )
					{
						if( azm <= xsazm + DataGlobals::Pi )
							rot = azm - xsazm;
						else
							rot = azm - xsazm - 2.0*DataGlobals::Pi;
					}
					else        /* For xsazm > pi */
					{
						if( azm >= xsazm - DataGlobals::Pi )
							rot = azm - xsazm;
						else
							rot = azm - xsazm + 2.0*DataGlobals::Pi;
					}
				}
				else          /* For other than vertical axis */
				{
					arg = sin(zen)*sin(azm-xsazm)/
					( sin(zen)*cos(azm-xsazm)*sin(xtilt) + cos(zen)*cos(xtilt) );
					if( arg < -99999.9 )
						rot = -DataGlobals::Pi/2.0;
					else if( arg > 99999.9 )
						rot = DataGlobals::Pi/2.0;
					else
						rot = atan(arg);
					/* Put rot in II or III quadrant if needed */
					if( xsazm <= DataGlobals::Pi )
					{
						if( azm > xsazm && azm <= xsazm + DataGlobals::Pi )
						{     /* Ensure positive rotation */
							if( rot < 0.0 )
								rot = DataGlobals::Pi + rot;   /* Put in II quadrant: 90 to 180 deg */
						}
						else
						{     /* Ensure negative rotation  */
							if( rot > 0.0 )
								rot = rot - DataGlobals::Pi;   /* Put in III quadrant: -90 to -180 deg */
						}
					}
					else        /* For xsazm > pi */
					{
						if( azm < xsazm && azm >= xsazm - DataGlobals::Pi )
						{     /* Ensure negative rotation  */
							if( rot > 0.0 )
								rot = rot - DataGlobals::Pi;   /* Put in III quadrant: -90 to -180 deg */
						}
						else
						{     /* Ensure positive rotation */
							if( rot < 0.0 )
								rot = DataGlobals::Pi + rot;   /* Put in II quadrant: 90 to 180 deg */
						}
					}
				}
				/*    printf("rot=%6.1f azm=%6.1f xsazm=%6.1f xtilt=%6.1f zen=%6.1f\n",rot/DTOR,azm/DTOR,xsazm/DTOR,xtilt/DTOR,zen/DTOR);  */

				if( rot < -rlim ) /* Do not let rotation exceed physical constraints */
					rot = -rlim;
				else if( rot > rlim )
					rot = rlim;

				// apd: added 21jan2012 to enable backtracking for 1 axis arrays using 3D iterative method
				// coded originally by intern M.Kasberg summer 2011
				if ( en_backtrack )
				{
					// find backtracking rotation angle
					double backrot = backtrack( azm*180/DataGlobals::Pi, zen*180/DataGlobals::Pi, // solar azimuth, zenith (deg)
											   tilt, sazm, // axis tilt, axis azimuth (deg)
											   rlim*180/DataGlobals::Pi, gcr, // rotation limit, GCR
											   rot*180/DataGlobals::Pi ); // ideal rotation angle

					btdiff = backrot - rot*180/DataGlobals::Pi; // log the difference (degrees)
					btdiff *= DataGlobals::Pi/180; // convert output to radians
					rot = backrot * DataGlobals::Pi/180; // convert backtracked rotation angle to radians
				}


				/* Find tilt angle for the tracking surface */
				arg = cos(xtilt)*cos(rot);
				if( arg < -1.0 )
					tilt = DataGlobals::Pi;
				else if( arg > 1.0  )
					tilt = 0.0;
				else
					tilt = acos(arg);
				/* Find surface azimuth for the tracking surface */
				if( tilt == 0.0 )
					sazm = DataGlobals::Pi;     /* Assign any value if tilt is zero */
				else
				{
					arg = sin(rot)/sin(tilt);
					if( arg < -1.0 )
						sazm = 1.5*DataGlobals::Pi + xsazm;
					else if( arg > 1.0  )
						sazm = 0.5*DataGlobals::Pi + xsazm;
					else if( rot < -0.5*DataGlobals::Pi )
						sazm = xsazm - DataGlobals::Pi - asin(arg);
					else if( rot > 0.5*DataGlobals::Pi )
						sazm = xsazm + DataGlobals::Pi - asin(arg);
					else
						sazm = asin(arg) + xsazm;
					if( sazm > 2.0*DataGlobals::Pi )       /* Keep between 0 and 2pi */
						sazm = sazm - 2.0*DataGlobals::Pi;
					else if( sazm < 0.0 )
						sazm = sazm + 2.0*DataGlobals::Pi;
				}
				/* printf("zen=%6.1f azm-sazm=%6.1f tilt=%6.1f arg=%7.4f\n",zen/DTOR,(azm-sazm)/DTOR,tilt/DTOR,arg); */
				/* Find incident angle */
				arg = sin(zen)*cos(azm-sazm)*sin(tilt) + cos(zen)*cos(tilt);
				if( arg < -1.0 )
					inc = DataGlobals::Pi;
				else if( arg > 1.0  )
					inc = 0.0;
				else
					inc = acos(arg);
				break;
			case 2:                 /* Two-Axis Tracking */
				tilt = zen;
				sazm = azm;
				inc = 0.0;
				rot = 0.0;
				break;
		}
		angle[0] = inc;           /* Variables returned in array angle[] */
		angle[1] = tilt;
		angle[2] = sazm;
		angle[3] = rot;
		angle[4] = btdiff;
	}

	void isotropic( double , double dn, double df, double alb, double inc, double tilt, double zen, double poa[3], double diffc[3] )
	{
		/* added aug2011 by aron dobos. Defines isotropic sky model for diffuse irradiance on a tilted surface

		 List of Parameters Passed to Function:
		 hextra = extraterrestrial irradiance on horizontal surface (W/m2) (unused for isotropic sky)
		 dn     = direct normal radiation (W/m2)
		 df     = diffuse horizontal radiation (W/m2)
		 alb    = surface albedo (decimal fraction)
		 inc    = incident angle of direct beam radiation to surface in radians
		 tilt   = surface tilt angle from horizontal in radians
		 zen    = sun zenith angle in radians

		 Variable Returned
		 poa    = plane-of-array irradiances (W/m2)
		 poa[0]: incident beam
		 poa[1]: incident sky diffuse
		 poa[2]: incident ground diffuse

		 diffc   = diffuse components, if an array is provided
		 diffc[0] = isotro	angle[]  = array of elements to return angles to calling function
		 angle[0] = inc  = incident angle in radians
		 angle[1] = tilt = tilt angle of surface from horizontal in radians
		 angle[2] = sazm = surface azimuth in radians, measured east from north
		 angle[3] = rot = tracking axis rotation angle in radians, measured from surface normal of unrotating axis (only for 1 axis trackers)
		 angle[4] = btdiff = (rot - ideal_rot) will be zero except in case of backtracking for 1 axis trackingpic
		 diffc[1] = circumsolar
		 diffc[2] = horizon brightening
		 */

		poa[0] = dn*cos(inc);
		poa[1] = df*(1.0+cos(tilt))/2.0;
		poa[2] = (dn*cos(zen)+df)*alb*(1.0-cos(tilt))/2.0;

		//prevent from returning negative poa values, added by jmf 7/28/14
		if (poa[0] < 0) poa[0] = 0;
		if (poa[1] < 0) poa[1] = 0;
		if (poa[2] < 0) poa[2] = 0;

		if (diffc != 0)
		{
			diffc[0] = poa[1];
			diffc[1] = 0; // no circumsolar
			diffc[2] = 0; // no horizon brightening
		}
	}

	void perez( double , double dn, double df, double alb, double inc, double tilt, double zen, double poa[3], double diffc[3] )
	{
		/* Modified aug2011 by aron dobos to split out beam, diffuse, ground for output.
		 Total POA is poa[0]+poa[1]+poa[2]

		 Defines the Perez function for calculating values of diffuse + direct
		 solar radiation + ground reflected radiation for a tilted surface
		 and returns the total plane-of-array irradiance(poa).  Function does
		 not check all input for valid entries; consequently, this should be
		 done before calling the function.  (Reference: Perez et al, Solar
		 Energy Vol. 44, No.5, pp.271-289,1990.) Based on original FORTRAN
		 program by Howard Bisner.

		 Modified 6/10/98 so that for zenith angles between 87.5 and 90.0 degrees,
		 the diffuse radiation is treated as isotropic instead of 0.0.

		 List of Parameters Passed to Function:
		 hextra = extraterrestrial irradiance on horizontal surface (W/m2) (unused in perez model)
		 dn     = direct normal radiation (W/m2)
		 df     = diffuse horizontal radiation (W/m2)
		 alb    = surface albedo (decimal fraction)
		 inc    = incident angle of direct beam radiation to surface in radians
		 tilt   = surface tilt angle from horizontal in radians
		 zen    = sun zenith angle in radians

		 Variable Returned
		 poa    = plane-of-array irradiances (W/m2)
		 poa[0]: incident beam
		 poa[1]: incident sky diffuse
		 poa[2]: incident ground diffuse

		 diffc   = diffuse components, if an array is provided
		 diffc[0] = isotropic
		 diffc[1] = circumsolar
		 diffc[2] = horizon brightening

		 */

		/* Local variables */
		double F11R[8] = { -0.0083117, 0.1299457, 0.3296958, 0.5682053,
			0.8730280, 1.1326077, 1.0601591, 0.6777470 };
		double F12R[8] = {  0.5877285, 0.6825954, 0.4868735, 0.1874525,
			-0.3920403, -1.2367284, -1.5999137, -0.3272588 };
		double F13R[8] = { -0.0620636, -0.1513752, -0.2210958, -0.2951290,
			-0.3616149, -0.4118494, -0.3589221, -0.2504286 };
		double F21R[8] = { -0.0596012, -0.0189325, 0.0554140, 0.1088631,
			0.2255647, 0.2877813, 0.2642124, 0.1561313 };
		double F22R[8] = {  0.0721249, 0.0659650, -0.0639588, -0.1519229,
			-0.4620442, -0.8230357, -1.1272340, -1.3765031 };
		double F23R[8] = { -0.0220216, -0.0288748, -0.0260542, -0.0139754,
			0.0012448, 0.0558651, 0.1310694, 0.2506212 };
		double EPSBINS[7] = { 1.065, 1.23, 1.5, 1.95, 2.8, 4.5, 6.2 };
		double B2=0.000005534,
		EPS,T,D,DELTA,A,B,C,ZH,F1,F2,COSINC,x;
		double CZ,ZC,ZENITH,AIRMASS;

		int i;

		if ( diffc != 0 )
			diffc[0] = diffc[1] = diffc[2] = 0.0;

		if ( dn < 0.0 )           /* Negative values may be measured if cloudy */
			dn = 0.0;

		if ( zen < 0.0 || zen > 1.5271631 ) /* Zen not between 0 and 87.5 deg */
		{
			if( df < 0.0 )
				df = 0.0;
			if ( cos(inc) > 0.0 && zen < 1.5707963 )  /* Zen between 87.5 and 90 */
			{                                      /* and incident < 90 deg   */
				poa[0] = dn * cos(inc);
				poa[1] = df*( 1.0 + cos(tilt) )/2.0;
				poa[2] = 0.0;

				if (diffc != 0) diffc[0] = poa[1]; /* isotropic only */
				return;
			}
			else
			{
				poa[0] = 0;
				poa[1] = df*( 1.0 + cos(tilt) )/2.0;   /* Isotropic diffuse only */
				poa[2] = 0.0;

				if (diffc != 0) diffc[0] = poa[1]; /* isotropic only */
				return;
			}
		}
		else                      /* Zen between 0 and 87.5 deg */
		{
			CZ = cos(zen);
			ZH = ( CZ > 0.0871557 ) ? CZ:0.0871557;    /* Maximum of 85 deg */
			D = df;                /* Horizontal diffuse radiation */
			if ( D <= 0.0 )        /* Diffuse is zero or less      */
			{
				if ( cos(inc) > 0.0 )    /* Incident < 90 deg */
				{
					poa[0] = dn*cos(inc);
					poa[1] = 0.0;
					poa[2] = 0.0;
					return;
				}
				else
				{
					poa[0] = 0;
					poa[1] = 0;
					poa[2] = 0;
					return;
				}
			}
			else                   /* Diffuse is greater than zero */
			{
				ZENITH = zen/DTOR;
				AIRMASS = 1.0 / (CZ + 0.15 * pow(93.9 - ZENITH, -1.253) );
				DELTA = D * AIRMASS / 1367.0;
				T = pow(ZENITH,3.0);
				EPS = (dn + D) / D;
				EPS = (EPS + T*B2) / (1.0 + T*B2);
				i=0;
				while ( i < 7 && EPS > EPSBINS[i] )
					i++;
				x = F11R[i] + F12R[i]*DELTA + F13R[i]*zen;
				F1 = ( 0.0 > x ) ? 0.0:x;
				F2 = F21R[i] + F22R[i]*DELTA + F23R[i]*zen;
				COSINC = cos(inc);
				if( COSINC < 0.0 )
					ZC = 0.0;
				else
					ZC = COSINC;

				// apd 7oct2011: reorganized from original pvwatts code
				// see duffie&beckman 2006, eqn 2.16.14
				A = D*(1-F1)*( 1.0 + cos(tilt) )/2.0; // isotropic diffuse
				B = D*F1*ZC/ZH; // circumsolar diffuse
				C = D*F2*sin(tilt); // horizon brightness term

				if (diffc != 0)
				{
					diffc[0] = A;
					diffc[1] = B;
					diffc[2] = C;
				}

				// original PVWatts: poa = A + F1*B + F2*C + alb*(dn*CZ+D)*(1.0 - cos(tilt) )/2.0 + dn*ZC;
				poa[0] = dn*ZC; // beam
				poa[1] = A + B + C; // total sky diffuse
				poa[2] = alb*(dn*CZ+D)*(1.0 - cos(tilt) )/2.0; // ground diffuse
				return;
			}
		}
	}

	void hdkr( double hextra, double dn, double df, double alb, double inc, double tilt, double zen, double poa[3], double diffc[3] /* can be null */ )
	{
		/* added aug2011 by aron dobos. Defines Hay, Davies, Klutcher, Reindl model for diffuse irradiance on a tilted surface

		 List of Parameters Passed to Function:
		 hextra = extraterrestrial irradiance on horizontal surface (W/m2)
		 dn     = direct normal radiation (W/m2)
		 df     = diffuse horizontal radiation (W/m2)
		 alb    = surface albedo (decimal fraction)
		 inc    = incident angle of direct beam radiation to surface in radians
		 tilt   = surface tilt angle from horizontal in radians
		 zen    = sun zenith angle in radians

		 Variable Returned
		 poa    = plane-of-array irradiances (W/m2)
		 poa[0]: incident beam
		 poa[1]: incident sky diffuse
		 poa[2]: incident ground diffuse

		 diffc   = diffuse components, if an array is provided
		 diffc[0] = isotropic
		 diffc[1] = circumsolar
		 diffc[2] = horizon brightening*/

		double hb = dn*cos(zen); /* beam irradiance on horizontal */
		double ht = hb+df; /* total irradiance on horizontal */
		if (ht < SMALL) ht = SMALL;
		if (hextra < SMALL) hextra = SMALL;

		double Rb = cos(inc)/cos(zen); /* ratio of beam on surface to beam on horizontal (D&B eq 1.8.1) */
		double Ai = hb/hextra; /* anisotropy index, term for forward scattering circumsolar diffuse (D&B eq 2.16.3) */
		double f = sqrt(hb/ht); /* modulating factor for horizontal brightening correction */
		double s3 = pow( sin( tilt*0.5 ), 3 ); /* horizontal brightening correction */

		/* see ESTIMATING DIFFUSE RADIATION ON HORIZONTAL SURFACES AND TOTAL RADIATION ON TILTED SURFACES
		 Master's Thesis, Douglas T Reindl, 1988, U.Wisc-Madison, Solar Energy Laboratory, http://sel.me.wisc.edu/publications/theses/reindl88.zip */

		double cir = df*Ai*Rb;
		double iso = df*(1-Ai)*0.5*(1+cos(tilt));
		double isohor = df*(1.0-Ai)*0.5*(1.0+cos(tilt))*(1.0+f*s3);

		poa[0] = dn*cos(inc);
		poa[1] = isohor+cir;
		poa[2] = (hb+df)*alb*(1.0-cos(tilt))/2.0;

		//prevent from returning negative poa values, added by jmf 7/28/14
		if (poa[0] < 0) poa[0] = 0;
		if (poa[1] < 0) poa[1] = 0;
		if (poa[2] < 0) poa[2] = 0;

		if (diffc != 0)
		{
			diffc[0] = iso;
			diffc[1] = cir;
			diffc[2] = isohor-iso;
		}
	}

	void poaDecomp( double , double angle[], double sun[], double alb, poaDecompReq *pA, double &dn, double &df, double &gh, double poa[3], double diffc[3]){
		/* added by Severin Ryberg. Decomposes POA into direct normal and diffuse irradiances

		 List of Parameters Passed to Function:
		 wfPOA	  = Plane of array irradiance measured from weatherfile (W/m2)
		 angle[]   = array of elements to return angles to calling function
		 angle[0] = inc  = incident angle in radians
		 angle[1] = tilt = tilt angle of surface from horizontal in radians
		 angle[2] = sazm = surface azimuth in radians, measured east from north
		 angle[3] = rot = tracking axis rotation angle in radians, measured from surface normal of unrotating axis (only for 1 axis trackers)
		 angle[4] = btdiff = (rot - ideal_rot) will be zero except in case of backtracking for 1 axis tracking
		 sun		  = array of elements to return sun position angles to calling function
		 sun[1]   = zeninth angle
		 sun[8]   = extraterrestrial radiation
		 alb       = albedo


		 Variable Returned
		 dn     = Direct Normal Irradiance (W/m2)
		 df     = Diffuse Horizontal Irradiance (W/m2)
		 gh     = Global Horizontal Irradiance (W/m2)
		 poa    = plane-of-array irradiances (W/m2)
		 poa[0]: incident beam
		 poa[1]: incident sky diffuse
		 poa[2]: incident ground diffuse

		 diffc   = diffuse components, if an array is provided
		 diffc[0] = isotropic
		 diffc[1] = circumsolar
		 diffc[2] = horizon brightening
		 */
		double r90(DataGlobals::Pi/2), r80( 80.0/180*DataGlobals::Pi ), r65(65.0/180*DataGlobals::Pi);
		/*
		 if ( (angle[0] != pA->inc[pA->i]) || (wfPOA != pA->POA[ pA->i ])) {
		 std::cout << "Error wih POA decomp" << std::endl;
		 exit(1);
		 }
		 */
		if ( angle[0] < r90 ){  // Check if incident angle if greater than 90 degrees

			double gti[] = {pA->POA[ pA->i-1 ], pA->POA[ pA->i ], pA->POA[ pA->i+1 ]};
			double inc[] = {pA->inc[ pA->i-1 ], pA->inc[ pA->i ], pA->inc[ pA->i+1 ]};

			GTI_DIRINT( gti, inc, sun[1], angle[1], sun[8], alb, pA->doy, pA->tDew, pA->elev, dn, df, gh, poa );

		} else {

			size_t stepsInDay = 24;
			if( pA->stepScale == 'm'){
				stepsInDay *= 60 / (unsigned int)pA->stepSize;
			}

			size_t noon = pA->dayStart + stepsInDay/2;
			size_t start, stop;
			// Check for a morning value or evening, set looping bounds accordingly
			if( pA->i < noon ){ // Calculate morning value
				start = pA->dayStart;
				stop = noon;
			} else {
				start = noon;
				stop = pA->dayStart + stepsInDay;
			}


			// Determine an average Kt prime value
			int count = 0;
			double avgKtp = 0;

			for( size_t j = start; j < stop; j++ ){


				if( (pA->inc[j] < r80) && (pA->inc[j] > r65) ){
					count++;
					double gti[] = {pA->POA[ j-1 ], pA->POA[ j ], pA->POA[ j+1 ]};
					double inc[] = {pA->inc[ j-1 ], pA->inc[ j ], pA->inc[ j+1 ]};

					double dnTmp, dfTmp, ghTmp, poaTmp[3];
					avgKtp += GTI_DIRINT( gti, inc, pA->zen[j], pA->tilt[j], pA->exTer[j], alb, pA->doy, pA->tDew, pA->elev, dnTmp, dfTmp, ghTmp, poaTmp );
				}
			}

			avgKtp /= count;

			//Calculate Kt
			double am = Min(15.25, 1.0 / (cos(sun[1]) + 0.15 * (pow(93.9 - sun[1]*180/DataGlobals::Pi, -1.253)))); // air mass
			double ktpam = am * exp(-0.0001184 * pA->elev);
			double Kt = avgKtp *( 1.031 * exp( -1.4/ (0.9 + 9.4/ktpam) ) + 0.1);

			//Calculate DNI using DIRINT
			double Kt_[3]  = {-999, Kt,               -999 };
			double Ktp_[3] = {-999, avgKtp,           -999 };
			double gti[3]  = {-999, pA->POA[ pA->i ], -999 };
			double zen[3]  = {-999, sun[1]          , -999 }; // Might need to be Zenith angle instead of inciden

			ModifiedDISC( Kt_, Ktp_, gti, zen, pA->tDew, pA->elev, pA->doy, dn);

			// Calculate DHI and GHI
			double ct = cos(angle[1]);
			df = (2*pA->POA[pA->i] - dn*cos(sun[1])*alb*(1-ct)) / (1 + ct + alb*(1-ct)) ;
			gh = dn * cos( angle[0] ) + df;

			//Check for bad values
			if(dn<0) dn = 0;
			if(df<0) df = 0;
			if(gh<0) gh = 0;

			// Get component poa from Perez
			perez( sun[8], dn, df, alb, angle[0], angle[1], sun[1], poa, diffc );

		}
	}

	double iam_nonorm( double theta, bool ar_glass )
	{
		double n_air = 1.0;

		double n_g = 1.526;
		double k_g = 4;
		double l_g = 0.002;

		double n_arc = 1.3;
		double k_arc = 4;
		double l_arc = l_g*0.01;  // assume 1/100th thickness of glass for AR coating

		if ( theta < AOI_MIN ) theta = AOI_MIN;
		if ( theta > AOI_MAX ) theta = AOI_MAX;

		if ( ar_glass )
		{
			double theta2 = 1;
			double tau_coating = transmittance( theta, n_arc, n_air, k_arc, l_arc, &theta2 );
			double tau_glass = transmittance( theta2, n_g, n_arc, k_g, l_g );
			return tau_coating*tau_glass;
		}
		else
		{
			return transmittance(theta, n_g, n_air, k_g, l_g );
		}
	}

	// Begin modified DISC code

	double cm[6][6][7][5] =
	{{{{ 0.385230, 0.385230, 0.385230, 0.462880, 0.317440 },
		{ 0.338390, 0.338390, 0.221270, 0.316730, 0.503650 },
		{ 0.235680, 0.235680, 0.241280, 0.157830, 0.269440 },
		{ 0.830130, 0.830130, 0.171970, 0.841070, 0.457370 },
		{ 0.548010, 0.548010, 0.478000, 0.966880, 1.036370 },
		{ 0.548010, 0.548010, 1.000000, 3.012370, 1.976540 },
		{ 0.582690, 0.582690, 0.229720, 0.892710, 0.569950 }},

		{{ 0.131280, 0.131280, 0.385460, 0.511070, 0.127940 },
			{ 0.223710, 0.223710, 0.193560, 0.304560, 0.193940 },
			{ 0.229970, 0.229970, 0.275020, 0.312730, 0.244610 },
			{ 0.090100, 0.184580, 0.260500, 0.687480, 0.579440 },
			{ 0.131530, 0.131530, 0.370190, 1.380350, 1.052270 },
			{ 1.116250, 1.116250, 0.928030, 3.525490, 2.316920 },
			{ 0.090100, 0.237000, 0.300040, 0.812470, 0.664970 }},

		{{ 0.587510, 0.130000, 0.400000, 0.537210, 0.832490 },
			{ 0.306210, 0.129830, 0.204460, 0.500000, 0.681640 },
			{ 0.224020, 0.260620, 0.334080, 0.501040, 0.350470 },
			{ 0.421540, 0.753970, 0.750660, 3.706840, 0.983790 },
			{ 0.706680, 0.373530, 1.245670, 0.864860, 1.992630 },
			{ 4.864400, 0.117390, 0.265180, 0.359180, 3.310820 },
			{ 0.392080, 0.493290, 0.651560, 1.932780, 0.898730 }},

		{{ 0.126970, 0.126970, 0.126970, 0.126970, 0.126970 },
			{ 0.810820, 0.810820, 0.810820, 0.810820, 0.810820 },
			{ 3.241680, 2.500000, 2.291440, 2.291440, 2.291440 },
			{ 4.000000, 3.000000, 2.000000, 0.975430, 1.965570 },
			{ 12.494170, 12.494170, 8.000000, 5.083520, 8.792390 },
			{ 21.744240, 21.744240, 21.744240, 21.744240, 21.744240 },
			{ 3.241680, 12.494170, 1.620760, 1.375250, 2.331620 }},

		{{ 0.126970, 0.126970, 0.126970, 0.126970, 0.126970 },
			{ 0.810820, 0.810820, 0.810820, 0.810820, 0.810820 },
			{ 3.241680, 2.500000, 2.291440, 2.291440, 2.291440 },
			{ 4.000000, 3.000000, 2.000000, 0.975430, 1.965570 },
			{ 12.494170, 12.494170, 8.000000, 5.083520, 8.792390 },
			{ 21.744240, 21.744240, 21.744240, 21.744240, 21.744240 },
			{ 3.241680, 12.494170, 1.620760, 1.375250, 2.331620 }},

		{{ 0.126970, 0.126970, 0.126970, 0.126970, 0.126970 },
			{ 0.810820, 0.810820, 0.810820, 0.810820, 0.810820 },
			{ 3.241680, 2.500000, 2.291440, 2.291440, 2.291440 },
			{ 4.000000, 3.000000, 2.000000, 0.975430, 1.965570 },
			{ 12.494170, 12.494170, 8.000000, 5.083520, 8.792390 },
			{ 21.744240, 21.744240, 21.744240, 21.744240, 21.744240 },
			{ 3.241680, 12.494170, 1.620760, 1.375250, 2.331620 }}},

		{{{ 0.337440, 0.337440, 0.969110, 1.097190, 1.116080 },
			{ 0.337440, 0.337440, 0.969110, 1.116030, 0.623900 },
			{ 0.337440, 0.337440, 1.530590, 1.024420, 0.908480 },
			{ 0.584040, 0.584040, 0.847250, 0.914940, 1.289300 },
			{ 0.337440, 0.337440, 0.310240, 1.435020, 1.852830 },
			{ 0.337440, 0.337440, 1.015010, 1.097190, 2.117230 },
			{ 0.337440, 0.337440, 0.969110, 1.145730, 1.476400 }},

			{{ 0.300000, 0.300000, 0.700000, 1.100000, 0.796940 },
				{ 0.219870, 0.219870, 0.526530, 0.809610, 0.649300 },
				{ 0.386650, 0.386650, 0.119320, 0.576120, 0.685460 },
				{ 0.746730, 0.399830, 0.470970, 0.986530, 0.785370 },
				{ 0.575420, 0.936700, 1.649200, 1.495840, 1.335590 },
				{ 1.319670, 4.002570, 1.276390, 2.644550, 2.518670 },
				{ 0.665190, 0.678910, 1.012360, 1.199940, 0.986580 }},

			{{ 0.378870, 0.974060, 0.500000, 0.491880, 0.665290 },
				{ 0.105210, 0.263470, 0.407040, 0.553460, 0.582590 },
				{ 0.312900, 0.345240, 1.144180, 0.854790, 0.612280 },
				{ 0.119070, 0.365120, 0.560520, 0.793720, 0.802600 },
				{ 0.781610, 0.837390, 1.270420, 1.537980, 1.292950 },
				{ 1.152290, 1.152290, 1.492080, 1.245370, 2.177100 },
				{ 0.424660, 0.529550, 0.966910, 1.033460, 0.958730 }},

			{{ 0.310590, 0.714410, 0.252450, 0.500000, 0.607600 },
				{ 0.975190, 0.363420, 0.500000, 0.400000, 0.502800 },
				{ 0.175580, 0.196250, 0.476360, 1.072470, 0.490510 },
				{ 0.719280, 0.698620, 0.657770, 1.190840, 0.681110 },
				{ 0.426240, 1.464840, 0.678550, 1.157730, 0.978430 },
				{ 2.501120, 1.789130, 1.387090, 2.394180, 2.394180 },
				{ 0.491640, 0.677610, 0.685610, 1.082400, 0.735410 }},

			{{ 0.597000, 0.500000, 0.300000, 0.310050, 0.413510 },
				{ 0.314790, 0.336310, 0.400000, 0.400000, 0.442460 },
				{ 0.166510, 0.460440, 0.552570, 1.000000, 0.461610 },
				{ 0.401020, 0.559110, 0.403630, 1.016710, 0.671490 },
				{ 0.400360, 0.750830, 0.842640, 1.802600, 1.023830 },
				{ 3.315300, 1.510380, 2.443650, 1.638820, 2.133990 },
				{ 0.530790, 0.745850, 0.693050, 1.458040, 0.804500 }},

			{{ 0.597000, 0.500000, 0.300000, 0.310050, 0.800920 },
				{ 0.314790, 0.336310, 0.400000, 0.400000, 0.237040 },
				{ 0.166510, 0.460440, 0.552570, 1.000000, 0.581990 },
				{ 0.401020, 0.559110, 0.403630, 1.016710, 0.898570 },
				{ 0.400360, 0.750830, 0.842640, 1.802600, 3.400390 },
				{ 3.315300, 1.510380, 2.443650, 1.638820, 2.508780 },
				{ 0.204340, 1.157740, 2.003080, 2.622080, 1.409380 }}},

		{{{ 1.242210, 1.242210, 1.242210, 1.242210, 1.242210 },
			{ 0.056980, 0.056980, 0.656990, 0.656990, 0.925160 },
			{ 0.089090, 0.089090, 1.040430, 1.232480, 1.205300 },
			{ 1.053850, 1.053850, 1.399690, 1.084640, 1.233340 },
			{ 1.151540, 1.151540, 1.118290, 1.531640, 1.411840 },
			{ 1.494980, 1.494980, 1.700000, 1.800810, 1.671600 },
			{ 1.018450, 1.018450, 1.153600, 1.321890, 1.294670 }},

			{{ 0.700000, 0.700000, 1.023460, 0.700000, 0.945830 },
				{ 0.886300, 0.886300, 1.333620, 0.800000, 1.066620 },
				{ 0.902180, 0.902180, 0.954330, 1.126690, 1.097310 },
				{ 1.095300, 1.075060, 1.176490, 1.139470, 1.096110 },
				{ 1.201660, 1.201660, 1.438200, 1.256280, 1.198060 },
				{ 1.525850, 1.525850, 1.869160, 1.985410, 1.911590 },
				{ 1.288220, 1.082810, 1.286370, 1.166170, 1.119330 }},

			{{ 0.600000, 1.029910, 0.859890, 0.550000, 0.813600 },
				{ 0.604450, 1.029910, 0.859890, 0.656700, 0.928840 },
				{ 0.455850, 0.750580, 0.804930, 0.823000, 0.911000 },
				{ 0.526580, 0.932310, 0.908620, 0.983520, 0.988090 },
				{ 1.036110, 1.100690, 0.848380, 1.035270, 1.042380 },
				{ 1.048440, 1.652720, 0.900000, 2.350410, 1.082950 },
				{ 0.817410, 0.976160, 0.861300, 0.974780, 1.004580 }},

			{{ 0.782110, 0.564280, 0.600000, 0.600000, 0.665740 },
				{ 0.894480, 0.680730, 0.541990, 0.800000, 0.669140 },
				{ 0.487460, 0.818950, 0.841830, 0.872540, 0.709040 },
				{ 0.709310, 0.872780, 0.908480, 0.953290, 0.844350 },
				{ 0.863920, 0.947770, 0.876220, 1.078750, 0.936910 },
				{ 1.280350, 0.866720, 0.769790, 1.078750, 0.975130 },
				{ 0.725420, 0.869970, 0.868810, 0.951190, 0.829220 }},

			{{ 0.791750, 0.654040, 0.483170, 0.409000, 0.597180 },
				{ 0.566140, 0.948990, 0.971820, 0.653570, 0.718550 },
				{ 0.648710, 0.637730, 0.870510, 0.860600, 0.694300 },
				{ 0.637630, 0.767610, 0.925670, 0.990310, 0.847670 },
				{ 0.736380, 0.946060, 1.117590, 1.029340, 0.947020 },
				{ 1.180970, 0.850000, 1.050000, 0.950000, 0.888580 },
				{ 0.700560, 0.801440, 0.961970, 0.906140, 0.823880 }},

			{{ 0.500000, 0.500000, 0.586770, 0.470550, 0.629790 },
				{ 0.500000, 0.500000, 1.056220, 1.260140, 0.658140 },
				{ 0.500000, 0.500000, 0.631830, 0.842620, 0.582780 },
				{ 0.554710, 0.734730, 0.985820, 0.915640, 0.898260 },
				{ 0.712510, 1.205990, 0.909510, 1.078260, 0.885610 },
				{ 1.899260, 1.559710, 1.000000, 1.150000, 1.120390 },
				{ 0.653880, 0.793120, 0.903320, 0.944070, 0.796130 }}},

		{{{ 1.000000, 1.000000, 1.050000, 1.170380, 1.178090 },
			{ 0.960580, 0.960580, 1.059530, 1.179030, 1.131690 },
			{ 0.871470, 0.871470, 0.995860, 1.141910, 1.114600 },
			{ 1.201590, 1.201590, 0.993610, 1.109380, 1.126320 },
			{ 1.065010, 1.065010, 0.828660, 0.939970, 1.017930 },
			{ 1.065010, 1.065010, 0.623690, 1.119620, 1.132260 },
			{ 1.071570, 1.071570, 0.958070, 1.114130, 1.127110 }},

			{{ 0.950000, 0.973390, 0.852520, 1.092200, 1.096590 },
				{ 0.804120, 0.913870, 0.980990, 1.094580, 1.042420 },
				{ 0.737540, 0.935970, 0.999940, 1.056490, 1.050060 },
				{ 1.032980, 1.034540, 0.968460, 1.032080, 1.015780 },
				{ 0.900000, 0.977210, 0.945960, 1.008840, 0.969960 },
				{ 0.600000, 0.750000, 0.750000, 0.844710, 0.899100 },
				{ 0.926800, 0.965030, 0.968520, 1.044910, 1.032310 }},

			{{ 0.850000, 1.029710, 0.961100, 1.055670, 1.009700 },
				{ 0.818530, 0.960010, 0.996450, 1.081970, 1.036470 },
				{ 0.765380, 0.953500, 0.948260, 1.052110, 1.000140 },
				{ 0.775610, 0.909610, 0.927800, 0.987800, 0.952100 },
				{ 1.000990, 0.881880, 0.875950, 0.949100, 0.893690 },
				{ 0.902370, 0.875960, 0.807990, 0.942410, 0.917920 },
				{ 0.856580, 0.928270, 0.946820, 1.032260, 0.972990 }},

			{{ 0.750000, 0.857930, 0.983800, 1.056540, 0.980240 },
				{ 0.750000, 0.987010, 1.013730, 1.133780, 1.038250 },
				{ 0.800000, 0.947380, 1.012380, 1.091270, 0.999840 },
				{ 0.800000, 0.914550, 0.908570, 0.999190, 0.915230 },
				{ 0.778540, 0.800590, 0.799070, 0.902180, 0.851560 },
				{ 0.680190, 0.317410, 0.507680, 0.388910, 0.646710 },
				{ 0.794920, 0.912780, 0.960830, 1.057110, 0.947950 }},

			{{ 0.750000, 0.833890, 0.867530, 1.059890, 0.932840 },
				{ 0.979700, 0.971470, 0.995510, 1.068490, 1.030150 },
				{ 0.858850, 0.987920, 1.043220, 1.108700, 1.044900 },
				{ 0.802400, 0.955110, 0.911660, 1.045070, 0.944470 },
				{ 0.884890, 0.766210, 0.885390, 0.859070, 0.818190 },
				{ 0.615680, 0.700000, 0.850000, 0.624620, 0.669300 },
				{ 0.835570, 0.946150, 0.977090, 1.049350, 0.979970 }},

			{{ 0.689220, 0.809600, 0.900000, 0.789500, 0.853990 },
				{ 0.854660, 0.852840, 0.938200, 0.923110, 0.955010 },
				{ 0.938600, 0.932980, 1.010390, 1.043950, 1.041640 },
				{ 0.843620, 0.981300, 0.951590, 0.946100, 0.966330 },
				{ 0.694740, 0.814690, 0.572650, 0.400000, 0.726830 },
				{ 0.211370, 0.671780, 0.416340, 0.297290, 0.498050 },
				{ 0.843540, 0.882330, 0.911760, 0.898420, 0.960210 }}},

		{{{ 1.054880, 1.075210, 1.068460, 1.153370, 1.069220 },
			{ 1.000000, 1.062220, 1.013470, 1.088170, 1.046200 },
			{ 0.885090, 0.993530, 0.942590, 1.054990, 1.012740 },
			{ 0.920000, 0.950000, 0.978720, 1.020280, 0.984440 },
			{ 0.850000, 0.908500, 0.839940, 0.985570, 0.962180 },
			{ 0.800000, 0.800000, 0.810080, 0.950000, 0.961550 },
			{ 1.038590, 1.063200, 1.034440, 1.112780, 1.037800 }},

			{{ 1.017610, 1.028360, 1.058960, 1.133180, 1.045620 },
				{ 0.920000, 0.998970, 1.033590, 1.089030, 1.022060 },
				{ 0.912370, 0.949930, 0.979770, 1.020420, 0.981770 },
				{ 0.847160, 0.935300, 0.930540, 0.955050, 0.946560 },
				{ 0.880260, 0.867110, 0.874130, 0.972650, 0.883420 },
				{ 0.627150, 0.627150, 0.700000, 0.774070, 0.845130 },
				{ 0.973700, 1.006240, 1.026190, 1.071960, 1.017240 }},

			{{ 1.028710, 1.017570, 1.025900, 1.081790, 1.024240 },
				{ 0.924980, 0.985500, 1.014100, 1.092210, 0.999610 },
				{ 0.828570, 0.934920, 0.994950, 1.024590, 0.949710 },
				{ 0.900810, 0.901330, 0.928830, 0.979570, 0.913100 },
				{ 0.761030, 0.845150, 0.805360, 0.936790, 0.853460 },
				{ 0.626400, 0.546750, 0.730500, 0.850000, 0.689050 },
				{ 0.957630, 0.985480, 0.991790, 1.050220, 0.987900 }},

			{{ 0.992730, 0.993880, 1.017150, 1.059120, 1.017450 },
				{ 0.975610, 0.987160, 1.026820, 1.075440, 1.007250 },
				{ 0.871090, 0.933190, 0.974690, 0.979840, 0.952730 },
				{ 0.828750, 0.868090, 0.834920, 0.905510, 0.871530 },
				{ 0.781540, 0.782470, 0.767910, 0.764140, 0.795890 },
				{ 0.743460, 0.693390, 0.514870, 0.630150, 0.715660 },
				{ 0.934760, 0.957870, 0.959640, 0.972510, 0.981640 }},

			{{ 0.965840, 0.941240, 0.987100, 1.022540, 1.011160 },
				{ 0.988630, 0.994770, 0.976590, 0.950000, 1.034840 },
				{ 0.958200, 1.018080, 0.974480, 0.920000, 0.989870 },
				{ 0.811720, 0.869090, 0.812020, 0.850000, 0.821050 },
				{ 0.682030, 0.679480, 0.632450, 0.746580, 0.738550 },
				{ 0.668290, 0.445860, 0.500000, 0.678920, 0.696510 },
				{ 0.926940, 0.953350, 0.959050, 0.876210, 0.991490 }},

			{{ 0.948940, 0.997760, 0.850000, 0.826520, 0.998470 },
				{ 1.017860, 0.970000, 0.850000, 0.700000, 0.988560 },
				{ 1.000000, 0.950000, 0.850000, 0.606240, 0.947260 },
				{ 1.000000, 0.746140, 0.751740, 0.598390, 0.725230 },
				{ 0.922210, 0.500000, 0.376800, 0.517110, 0.548630 },
				{ 0.500000, 0.450000, 0.429970, 0.404490, 0.539940 },
				{ 0.960430, 0.881630, 0.775640, 0.596350, 0.937680 }}},

		{{{ 1.030000, 1.040000, 1.000000, 1.000000, 1.049510 },
			{ 1.050000, 0.990000, 0.990000, 0.950000, 0.996530 },
			{ 1.050000, 0.990000, 0.990000, 0.820000, 0.971940 },
			{ 1.050000, 0.790000, 0.880000, 0.820000, 0.951840 },
			{ 1.000000, 0.530000, 0.440000, 0.710000, 0.928730 },
			{ 0.540000, 0.470000, 0.500000, 0.550000, 0.773950 },
			{ 1.038270, 0.920180, 0.910930, 0.821140, 1.034560 }},

			{{ 1.041020, 0.997520, 0.961600, 1.000000, 1.035780 },
				{ 0.948030, 0.980000, 0.900000, 0.950360, 0.977460 },
				{ 0.950000, 0.977250, 0.869270, 0.800000, 0.951680 },
				{ 0.951870, 0.850000, 0.748770, 0.700000, 0.883850 },
				{ 0.900000, 0.823190, 0.727450, 0.600000, 0.839870 },
				{ 0.850000, 0.805020, 0.692310, 0.500000, 0.788410 },
				{ 1.010090, 0.895270, 0.773030, 0.816280, 1.011680 }},

			{{ 1.022450, 1.004600, 0.983650, 1.000000, 1.032940 },
				{ 0.943960, 0.999240, 0.983920, 0.905990, 0.978150 },
				{ 0.936240, 0.946480, 0.850000, 0.850000, 0.930320 },
				{ 0.816420, 0.885000, 0.644950, 0.817650, 0.865310 },
				{ 0.742960, 0.765690, 0.561520, 0.700000, 0.827140 },
				{ 0.643870, 0.596710, 0.474460, 0.600000, 0.651200 },
				{ 0.971740, 0.940560, 0.714880, 0.864380, 1.001650 }},

			{{ 0.995260, 0.977010, 1.000000, 1.000000, 1.035250 },
				{ 0.939810, 0.975250, 0.939980, 0.950000, 0.982550 },
				{ 0.876870, 0.879440, 0.850000, 0.900000, 0.917810 },
				{ 0.873480, 0.873450, 0.751470, 0.850000, 0.863040 },
				{ 0.761470, 0.702360, 0.638770, 0.750000, 0.783120 },
				{ 0.734080, 0.650000, 0.600000, 0.650000, 0.715660 },
				{ 0.942160, 0.919100, 0.770340, 0.731170, 0.995180 }},

			{{ 0.952560, 0.916780, 0.920000, 0.900000, 1.005880 },
				{ 0.928620, 0.994420, 0.900000, 0.900000, 0.983720 },
				{ 0.913070, 0.850000, 0.850000, 0.800000, 0.924280 },
				{ 0.868090, 0.807170, 0.823550, 0.600000, 0.844520 },
				{ 0.769570, 0.719870, 0.650000, 0.550000, 0.733500 },
				{ 0.580250, 0.650000, 0.600000, 0.500000, 0.628850 },
				{ 0.904770, 0.852650, 0.708370, 0.493730, 0.949030 }},

			{{ 0.911970, 0.800000, 0.800000, 0.800000, 0.956320 },
				{ 0.912620, 0.682610, 0.750000, 0.700000, 0.950110 },
				{ 0.653450, 0.659330, 0.700000, 0.600000, 0.856110 },
				{ 0.648440, 0.600000, 0.641120, 0.500000, 0.695780 },
				{ 0.570000, 0.550000, 0.598800, 0.400000, 0.560150 },
				{ 0.475230, 0.500000, 0.518640, 0.339970, 0.520230 },
				{ 0.743440, 0.592190, 0.603060, 0.316930, 0.794390 }}}};

	double ModifiedDISC(const double g[3], const double z[3], double td, double alt, int doy, double &dn) // aka DIRINT model
	{
		// Calculates direct normal (beam) radiation from global horizontal radiation.
		// Arguments passed to function:
		// g[3] - global irradiance array (watts / sq. meter)
		// z[3] - solar zenith angle array (radians)
		// td - dew point temperature (degrees c)
		// doy - julian day of year
		// alt - altitude of site (meters)
		// Returns:      dn - beam irradiance (watts / sq. meter)  (returned through the input dn)
		//				 Ktp - Kt prime
		// Notes:  This function uses a disc beam model to calculate the beam irradiance returned.
		// The argument g is an array of 3 values. The values are the global irradiance of the
		// previous reading, the current reading ,and the next reading in that order. The argument
		// z uses the same format, except the values are the respective solar zenith angles. If any
		// of the g or z values are not available or the previous or next readings did not occur
		// within 1.5 hours of the current reading then the appropriate value or values should be
		// replaced with a -999.0. If the argument td is missing then the value -999.0 should be
		// used in place of the missing argument. The current global irradiance (g[1]) must have a
		// value. If the dew point temperature (td) is missing then td is not used to find an index
		// into the correction matrix (cm), instead a special column in the matrix is used. If the
		// previous global irradiance (g[0]) or solar zenith angle (z[0]) and the next global
		// irradiance (g[2]) or solar zenith angle (z[2]) are missing then delta kt' (dkt1) is not
		// used to find an index into the correction matrix (cm), instead a special column in the
		// matrix is used.
		//
		// Modification history:
		// 25/10/2015 Converted to C++ for use in SAM by David Severin Ryberg
		// 4/14/2015 Corrected error in incrementing i,j, and k array indices
		// 7/16/13. Converted by Bill Marion to C#. The 7/5/91 version provided by Daryl that this
		//  is based on was significantly different than the 6/26/91 I had got from Martin years ago in that
		//  the section on bin interpolating for clear stable cases had been removed.
		// 6/28/2013. Converted to C# from Howard Bisner FORTRAN77 code
		// 5/24/91. Richard's code to do linear interpolation between highest kt' bins added by RS.
		//  RS fixed some typos in Richard's untested code.
		// 6/10/91. Corrected bin interpolation near label 141 [divide by 0.007 and zbin2 not zbin]
		// 6/26/91: Richard perez: Modification of DKT1 calculation
		//  to avoid very low sun distorsion caused by questionable
		//  cosine response of pyranometers
		// 7/5/91:  RS: lines extending beyond col 72 fixed.
		//  Questionable use of x**-y changed to x**(-y)
		//  Made reference to intrinsic dmax1 agree with type

		double cz[3], zenith[3], kt[3], am[3], ktpam[3], kt1[3];

		double ktbin[5] = { 0.24, 0.4, 0.56, 0.7, 0.8 };
		double zbin[5] = { 25.0, 40.0, 55.0, 70.0, 80.0 };
		double dktbin[5] = { 0.015, 0.035, 0.07, 0.15, 0.3 };
		double wbin[3] = { 1.0, 2.0, 3.0 };
		double rtod = 57.295779513082316;
		double a, b, c, w, knc, bmax, dkt1, io;

		//double dn = 0.0;
		if (g[1] >= 1.0 && cos(z[1]) > 0.0)
		{   // Model only if present global >= 1 and present zenith < 90 deg
			io = 1367.0 * (1.0 + 0.033 * cos(0.0172142 * doy));    // Extraterrestrial dn
			int j = 0, k = 2, i = 0, l = 0;
			if (g[0] < -998.0 || z[0] < -998.0)
			{   // Prehour global and zenith were passed missing -999.0
				j = 1;
				kt1[0] = -999.0;
			}
			if (g[2] < -998.0 || z[2] < -998.0)
			{   // Posthour global and zenith were passed missing -999.0
				k = 1;
				kt1[2] = -999.0;
			}
			for (i = j; i <= k; i++)
			{   // For each of the 3 hours that have data, find kt prime
				cz[i] = cos(z[i]); // Cosine of zenith angle
				if (cz[i] < 0.0)
					kt1[i] = -999.0;
				else
				{
					zenith[i] = z[i] * rtod;
					kt[i] = g[i] / (io * Max(0.065, cz[i]));   // Kt
					am[i] = Min(15.25, 1.0 / (cz[i] + 0.15 * (pow(93.9 - zenith[i], -1.253))));
					ktpam[i] = am[i] * exp(-0.0001184 * alt);
					kt1[i] = kt[i] / (1.031 * exp(-1.4 / (0.9 + 9.4 / ktpam[i])) + 0.1);   // Kt prime
				}
			}
			if (kt[1] <= 0.6)
			{
				a = 0.512 - 1.56 * kt[1] + 2.286 * pow(kt[1], 2.0) - 2.22 * pow(kt[1], 3.0);
				b = 0.37 + 0.962 * kt[1];
				c = -0.28 + 0.932 * kt[1] - 2.048 * pow(kt[1], 2.0);
			}
			else
			{
				a = -5.743 + 21.77 * kt[1] - 27.49 * pow(kt[1], 2.0) + 11.56 * pow(kt[1], 3.0);
				b = 41.40 - 118.5 * kt[1] + 66.05 * pow(kt[1], 2.0) + 31.9 * pow(kt[1], 3.0);
				c = -47.01 + 184.2 * kt[1] - 222.0 * pow(kt[1], 2.0) + 73.81 * pow(kt[1], 3.0);
			}
			knc = 0.866 - 0.122 * am[1] + 0.0121 * pow(am[1], 2.0) - 0.000653 * pow(am[1], 3.0) + 0.000014 * pow(am[1], 4.0);
			bmax = io * (knc - (a + b * exp(c * am[1])));
			if (kt1[0] < -998.0 && kt1[2] < -998.0)
				k = 6;
			else
			{
				if (kt1[0] < -998.0 || zenith[0] >= 85.0)
					dkt1 = fabs(kt1[2] - kt1[1]);
				else if (kt1[2] < -998.0 || zenith[2] >= 85.0)
					dkt1 = fabs(kt1[1] - kt1[0]);
				else
					dkt1 = 0.5 * (fabs(kt1[1] - kt1[0]) + fabs(kt1[2] - kt1[1]));

				k = 0;
				//while (k < 4 && dkt1 >= dktbin[k])
				while (k < 5 && dkt1 >= dktbin[k])      // Error fix 4/14/2015
					k++;
			}
			i = 0;
			//while (i < 4 && kt1[1] >= ktbin[i])
			while (i < 5 && kt1[1] >= ktbin[i])         // Error fix 4/14/2015
				i++;
			j = 0;
			//while (j < 4 && zenith[1] >= zbin[j])
			while (j < 5 && zenith[1] >= zbin[j])       // Error fix 4/14/2015
				j++;
			if (td < -998.0)
				l = 4;  // l = letter "l'
			else
			{
				w = exp(-0.075 + 0.07 * td);
				l = 0;
				while (l < 3 && w >= wbin[l])
					l++;
			}
			dn = bmax * cm[i][j][k][l];
			dn = Max(0.0, dn);
		}   // End of if present global >= 1

		return kt1[1];
	}   // End of ModifiedDISC

	void ModifiedDISC(const double kt[3], const double kt1[3], const double g[3], const double z[3], double td, double alt, int doy, double &dn) // aka DIRINT model
	{
		// Calculates direct normal (beam) radiation from global horizontal radiation.
		// Arguments passed to function:
		// g[3] - global irradiance array (watts / sq. meter)
		// z[3] - solar zenith angle array (radians)
		// td - dew point temperature (degrees c)
		// doy - julian day of year
		// alt - altitude of site (meters)
		// Returns:      dn - beam irradiance (watts / sq. meter)  (returned through the input dn)
		//				 Ktp - Kt prime
		// Notes: This is a modification to the orininally provided Modified-DISC model which takes
		// as input the four bin variables and GHI and returns the resulting DNI

		double cz[3], zenith[3], am[3];
		double ktbin[5] = { 0.24, 0.4, 0.56, 0.7, 0.8 };
		double zbin[5] = { 25.0, 40.0, 55.0, 70.0, 80.0 };
		double dktbin[5] = { 0.015, 0.035, 0.07, 0.15, 0.3 };
		double wbin[3] = { 1.0, 2.0, 3.0 };
		double rtod = 57.295779513082316;
		double a, b, c, w, knc, bmax, dkt1, io;

		//double dn = 0.0;
		if (g[1] >= 1.0 && cos(z[1]) > 0.0)
		{   // Model only if present global >= 1 and present zenith < 90 deg

			//std::cout << "yes!\n";

			io = 1367.0 * (1.0 + 0.033 * cos(0.0172142 * doy));    // Extraterrestrial dn
			int j = 0, k = 2, i = 0, l = 0;

			for (i = j; i <= k; i++)
			{   // For each of the 3 hours that have data, find kt prime
				cz[i] = cos(z[i]); // Cosine of zenith angle
				zenith[i] = z[i] * rtod;
				am[i] = Min(15.25, 1.0 / (cz[i] + 0.15 * (pow(93.9 - zenith[i], -1.253))));
			}
			if (kt[1] <= 0.6)
			{
				a = 0.512 - 1.56 * kt[1] + 2.286 * pow(kt[1], 2.0) - 2.22 * pow(kt[1], 3.0);
				b = 0.37 + 0.962 * kt[1];
				c = -0.28 + 0.932 * kt[1] - 2.048 * pow(kt[1], 2.0);
			}
			else
			{
				a = -5.743 + 21.77 * kt[1] - 27.49 * pow(kt[1], 2.0) + 11.56 * pow(kt[1], 3.0);
				b = 41.40 - 118.5 * kt[1] + 66.05 * pow(kt[1], 2.0) + 31.9 * pow(kt[1], 3.0);
				c = -47.01 + 184.2 * kt[1] - 222.0 * pow(kt[1], 2.0) + 73.81 * pow(kt[1], 3.0);
			}
			knc = 0.866 - 0.122 * am[1] + 0.0121 * pow(am[1], 2.0) - 0.000653 * pow(am[1], 3.0) + 0.000014 * pow(am[1], 4.0);
			bmax = io * (knc - (a + b * exp(c * am[1])));
			//std::cout << "Kt: " << kt[1] << std::endl;
			//std::cout << io << " " << knc << " " << a << " " << b << " " << c << " " << am[1] << std::endl;


			if (kt1[0] < -998.0 && kt1[2] < -998.0)
				k = 6;
			else
			{
				if (kt1[0] < -998.0 || zenith[0] >= 85.0)
					dkt1 = fabs(kt1[2] - kt1[1]);
				else if (kt1[2] < -998.0 || zenith[2] >= 85.0)
					dkt1 = fabs(kt1[1] - kt1[0]);
				else
					dkt1 = 0.5 * (fabs(kt1[1] - kt1[0]) + fabs(kt1[2] - kt1[1]));

				k = 0;
				//while (k < 4 && dkt1 >= dktbin[k])
				while (k < 5 && dkt1 >= dktbin[k])      // Error fix 4/14/2015
					k++;
			}
			i = 0;
			//while (i < 4 && kt1[1] >= ktbin[i])
			while (i < 5 && kt1[1] >= ktbin[i])         // Error fix 4/14/2015
				i++;
			j = 0;
			//while (j < 4 && zenith[1] >= zbin[j])
			while (j < 5 && zenith[1] >= zbin[j])       // Error fix 4/14/2015
				j++;
			if (td < -998.0)
				l = 4;  // l = letter "l'
			else
			{
				w = exp(-0.075 + 0.07 * td);
				l = 0;
				while (l < 3 && w >= wbin[l])
					l++;
			}


			dn = Max( 0.0, bmax * cm[i][j][k][l]);
			//std::cout << dn << " " << bmax << " " << cm[i][j][k][l] << std::endl;
		}   // End of if present global >= 1
		else
			dn=0;
		return;
	}   // End of ModifiedDISC

	double backtrack( double solazi, double solzen,
					 double axis_tilt, double axis_azimuth,
					 double rotlim, double gcr, double rotation )
	{
		//Now do backtracking.
		//This is very straightforward - decrease the rotation as long as we are in shade.
		int iter = 0;
		while( shade_fraction_1x( solazi, solzen, axis_tilt, axis_azimuth, gcr, rotation) > 0 && ++iter < 100)
		{
			//Move closer to flat.
			if (rotation > 0)
			{
				if ( fabs(rotation-1) > fabs(rotlim) )
					break;
				rotation = rotation - 1;
			}
			else
			{
				if ( fabs(rotation+1) > fabs(rotlim) )
					break;
				rotation = rotation + 1;
			}
		}
		return rotation;
	}

	double GTI_DIRINT( const double poa[3], const double inc[3], double zen, double tilt, double ext, double alb, int doy, double tDew, double elev, double& dnOut, double& dfOut, double& ghOut, double poaCompOut[3])
	{

		double diff = 1E6;
		double bestDiff = 1E6;
		double Ktp=0;
		double GTI[] = { poa[0], poa[1], poa[2] };

		double Ci[30] = {1., 1., 1., 0.5, 0.5,
			0.5, 0.5, 0.5, 0.5, 0.5,
			0.25, 0.25, 0.25, 0.25, 0.25,
			0.25, 0.25, 0.25, 0.25, 0.25,
			0.125, 0.125, 0.125, 0.125, 0.125,
			0.125, 0.125, 0.125, 0.125, 0.125};

		double poa_tmp[3], diffc_tmp[3], poaBest[3] = {0, 0, 0};

		// Begin iterative solution for Kt
		//	double Io = 1367.0 * (1.0 + 0.033 * cos(0.0172142 * doy));    // Extraterrestrial dn (Taken from DIRINT Model)
		double cz = cos(zen);
		int i = 0;

		while (fabs(diff) > 1.0 && i++ < 30 ){

			// Calculate Kt using GTI and Eq. 2
			//		double Kt_inc = GTI[1] / (Io * Max(0.065, cos(inc[1])));

			//Calculate GNI using Kt and DIRINT Eq.s
			double dn_tmp;
			double Ktp_tmp = ModifiedDISC( GTI, inc, tDew, elev, doy, dn_tmp);

			//Calculate DHI using Eq. 3
			double df_tmp = GTI[1] * Max( 0.065, cz) / Max(0.065, cos(inc[1])) - dn_tmp*cz;

			//Check for bad values
			if( dn_tmp < 0 ) dn_tmp = 0;
			if( df_tmp < 0 ) df_tmp = 0;

			//Model POA using Perez model and find diff from GTI (Model - GTI)
			perez( ext, dn_tmp, df_tmp, alb, inc[1], tilt, zen, poa_tmp, diffc_tmp );

			//Compare modeled POA to measured POA
			diff = ( poa_tmp[0] + poa_tmp[1] + poa_tmp[2]) - poa[1];

			//Check for best Difference. If found, save results
			if ( fabs(diff) < fabs(bestDiff)){
				bestDiff = diff;
				Ktp = Ktp_tmp;
				dnOut = dn_tmp;
				dfOut = df_tmp;
				poaBest[0] = poa_tmp[0];
				poaBest[1] = poa_tmp[1];
				poaBest[2] = poa_tmp[2];
			}

			// Adjust GTI using Eq. 4
			// Apply the same change to previous/ subsequent GTI's as well (based on Bill's email)
			GTI[0] = Max( 1.0, GTI[0] - Ci[i] * diff);
			GTI[1] = Max( 1.0, GTI[1] - Ci[i] * diff);
			GTI[2] = Max( 1.0, GTI[2] - Ci[i] * diff);

		}

		poaCompOut[0] = poaBest[0];
		poaCompOut[1] = poaBest[1];
		poaCompOut[2] = poaBest[2];

		ghOut = dnOut * cos(inc[1]) + dfOut;

		return Ktp;
	}

	double Min( double v1, double v2)
	{

		// Check if both are NAN

		if( v1 != v1 && v2!=v2 ) return NAN;

		if( v1 <= v2) return v1;
		else return v2;
	}

	double Max( double v1, double v2)
	{

		// Check if both are NAN

		if( v1 != v1 && v2!=v2 ) return NAN;

		if( v1 >= v2) return v1;
		else return v2;
	}

	double transmittance( double theta1_deg, /* incidence angle of incoming radiation (deg) */
						 double n_cover,  /* refractive index of cover material, n_glass = 1.586 */
						 double n_incoming, /* refractive index of incoming material, typically n_air = 1.0 */
						 double k,        /* proportionality constant assumed to be 4 (1/m) for derivation of Bouguer's law (set to zero to skip bougeur's law */
						 double l_thick,  /* material thickness (set to zero to skip Bouguer's law */
						 double *_theta2_deg ) /* thickness of cover material (m), usually 2 mm for typical module */
	{
		// reference: duffie & beckman, Ch 5.3

		double theta1 = theta1_deg * DataGlobals::Pi/180.0;
		double theta2 = asin( n_incoming / n_cover * sin(theta1 ) ); // snell's law, assuming n_air = 1.0
		// fresnel's equation for non-reflected unpolarized radiation as an average of perpendicular and parallel components
		double tr = 1 - 0.5 *
		( pow( sin(theta2-theta1), 2 )/pow( sin(theta2+theta1), 2)
		 + pow( tan(theta2-theta1), 2 )/pow( tan(theta2+theta1), 2 ) );

		if ( _theta2_deg ) *_theta2_deg = theta2 * 180/DataGlobals::Pi;

		return tr * exp( -k * l_thick / cos(theta2) );
	}

	void clear_state()
	{
		PVWattsGenerators.clear();
	}

}

}
