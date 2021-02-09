/**
BSD-3-Clause
Copyright 2019 Alliance for Sustainable Energy, LLC
Redistribution and use in source and binary forms, with or without modification, are permitted provided 
that the following conditions are met :
1.	Redistributions of source code must retain the above copyright notice, this list of conditions 
and the following disclaimer.
2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions 
and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse 
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES 
DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, 
OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

// TFF notes on 25-Jan-2012
// Differences from last version (SAM 2011-21-2) of code that will cause different answers for the same inputs
// 1 - "CalculateNewTemperature" is called with current elapsed time, instead of one month ahead (similar to Jun 2011 version & GETEM spreadsheet)
// 2 - DAYS_PER_YEAR changed to 365 from 365.25
// 3 - "CGeothermalAnalyzer::EGSFractureLength" uses the input "mo_geo_in.md_DistanceBetweenProductionInjectionWellsM" instead of
//     having the distance between wells be hardwired to 1000m regardless of what user enters.
// 4 - Because this code stores values in constants rather than in member variables (that were never changed), the results changed slightly (at about the 4th significant digit)
//     E.G., geothermal::EXCESS_PRESSURE_BAR returns 3.50000000000, but mpGBI->mdExcessPressureBar returned 3.4997786965077915, although it was set to 3.5 and never changed.
// March 2013
// 5 - pump efficiencies were used incorrectly in the old version (fix committed to svn March 1, 2012)
// 6 - pump power calculations in the old version did not match GETEM

#include "lib_physics.h"
#include "lib_geothermal.h"

#ifdef _MSC_VER
#pragma warning(disable: 4127)  // ignore warning: 'warning C4127: conditional expression is constant'
#endif


#ifndef MAX
#define MAX(a,b)  ((a>b)?a:b)
#endif

namespace geothermal
{

	const double MAX_TEMP_RATIO = 1.134324;  // max valid value for (resource temp)/(plant design temp) where both are measured in Kelvin
	const double DEFAULT_AMBIENT_TEMPC_BINARY = 10.0;			// degrees C
	//const double WET_BULB_TEMPERATURE_FOR_FLASH_CALCS = 15.0;	// degrees C, used in Flash calcs brine effectiveness calcs an flash injection temperature calcs
	const bool ADDITIONAL_PRESSURE_REQUIRED = true;
	//const double EFFICIENCY_PUMP_GF = 0.6;
	const double EGS_THERMAL_CONDUCTIVITY = 3 * 3600 * 24;				// J/m-day-C
	//const double PRESSURE_CHANGE_ACROSS_SURFACE_EQUIPMENT_PSI = 25;	// 25 psi [2B.Resource&Well Input].D146, H146
	const double TEMPERATURE_EGS_INJECTIONC = 76.1;					// degrees C, [7C.EGS Subsrfce HX].D11 [should be a function of plant design temperature]
	const double TEMPERATURE_EGS_AMBIENT_C = 15.0;					// Note in GETEM spreadsheet says that this is only used in calculating resource temp or depth.  However, if EGS calculations are based on depth, then resource temp is based on this number, so all power calcs are based on it as well
	const double CONST_CT = 0.0009;									// these are both inputs that are shaded out in GETEM
	const double CONST_CP = 0.000000000464;							//	"		"			"			"			"
	//const double EXCESS_PRESSURE_BAR = 3.5;						// default 3.5 bar, [2B.Resource&Well Input].D205
	//const double PRESSURE_AMBIENT_PSI = 14.7; // default
	const double WATER_LOSS_PERCENT = 0.02;							// 2%
	const double EGS_TIME_INPUT = 3.076;							// years, not really explained - user is supposed to vary input until a calculated value equals plant design temp [7C.EGS Subsrfce HX].D42 (fTimeStar)
	const double FRACTURE_LENGTH_ADJUSTMENT = 2;					// used for one instance of where the EGS fracture length is used.  All others use the original fracture length
	const double DELTA_PRESSURE_HP_FLASH_PSI = 1.0;					//Was 2.2 -> now changed to 1.0 (as seen in GETEM on 10/09/18)
	const double DELTA_PRESSURE_LP_FLASH_PSI = 1.0;
	const double DELTA_TEMPERATURE_CWF = 25.0;						// (degrees F) Was 30.0 -> now changed to 25.0 (as seen in GETEM on 10/09/18)
	const double TEMPERATURE_PINCH_PT_CONDENSER_F = 7.5;			//Was 10.0 -> now changed to 7.5 (as seen in GETEM on 10/09/18)
	const double TEMPERATURE_PINCH_PT_COOLING_TOWER_F = 5;			//Was 15 -> now changed to 5.0 (as seen in GETEM on 10/09/18)
	const double PRESSURE_CONDENSER_NCG_PARTIAL_INHG = 0.32;			// (inches of Mercury) was 0.5 -> now changed to 0.32 (as seen in GETEM on 10/09/18)
	const double GEOTHERMAL_FLUID_FOR_FLASH = 1000;					// D67 in "5C.Flash-Steam Plant Perf" [was an integer, not sure why]
	const double EFFICIENCY_TURBINE = 0.80;							//Was 0.825 -> now changed to 0.80 (as seen in GETEM on 10/09/18)
	const double EFFICIENCY_GENERATOR = 0.98;
	const double EFFICIENCY_PUMP_FLASH = 0.7;
	const ncgRemovalTypes NCG_REMOVAL_TYPE = HYBRID;					//Always type JET??
	const int NUMBER_OF_COOLING_STAGES = 3;							// 1,2, or 3
	const double NCG_LEVEL_PPM = 2000;								//Was 100 -> now changed to 2000 (as seen in GETEM on 10/09/18)
	const double MOLE_WEIGHT_NCG = 44.0;
	const double MOLE_WEIGHT_H2O = 18.0;
	const double BASE_CW_PUMP_HEAD_FT = 65.0;						//Was 60.0 -> now changed to 65.0 (as seen in GETEM on 10/09/18)		
	const condenserTypes CONDENSER_TYPE = SURFACE;
	const double INJECTION_PUMPING_CYCLES = 5.0;					//Was  6.0 -> now changed to 5.0 (as seen in GETEM on 10/09/18) - cell #D133
	const double ADDITIONAL_CW_PUMP_HEAD_SURFACE = 10 * 144 / physics::WATER_DENSITY;
	//const double MAX_TEMPERATURE_DECLINE_C = 30;
	const double FINAL_YEARS_WITH_NO_REPLACEMENT = 5;


	/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// GETEM Physics and general equations
	/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	const bool IMITATE_GETEM = false;
	const double GETEM_FT_IN_METER = (IMITATE_GETEM) ? 3.28083 : physics::FT_PER_METER; // feet per meter - largest source of discrepancy
	const double GETEM_PSI_PER_BAR = (IMITATE_GETEM) ? 14.50377 : physics::PSI_PER_BAR; // psi per bar
	const double GETEM_PSI_PER_INHG = (IMITATE_GETEM) ? 0.49115 : physics::PSI_PER_INHG; // psi per inch of mercury
	const double GETEM_KGM3_PER_LBF3 = (IMITATE_GETEM) ? (35.3146 / 2.20462) : physics::KGM3_PER_LBF3; // lbs/ft^3 per kg/m^3 
	const double GETEM_LB_PER_KG = (IMITATE_GETEM) ? 2.20462 : physics::LB_PER_KG; // pounds per kilogram
	const double GETEM_KW_PER_HP = (IMITATE_GETEM) ? 0.7457 : physics::KW_PER_HP; // kilowatts per unit of horsepower
	const double GRAVITY_MS2 = (IMITATE_GETEM) ? 9.807 : physics::GRAVITY_MS2; // meters per second^2; this varies between 9.78 and 9.82 depending on latitude
	const double DAYS_PER_YEAR = 365.25;

	double MetersToFeet(const double &m) { return m * GETEM_FT_IN_METER; }
	double FeetToMeters(const double &ft) { return ft / GETEM_FT_IN_METER; }
	double M2ToFeet2(const double &mSquared) { return (IMITATE_GETEM) ? mSquared * 10.76391 : mSquared * pow(GETEM_FT_IN_METER, 2); }

	double InHgToPsi(const double &inHg) { return inHg * GETEM_PSI_PER_INHG; }
	double PsiToInHg(const double &psi) { return psi / GETEM_PSI_PER_INHG; }
	double BarToPsi(const double &bar) { return bar * GETEM_PSI_PER_BAR; }

	double KgPerM3ToLbPerCf(const double &kgPerM3) { return kgPerM3 / GETEM_KGM3_PER_LBF3; }
	double LbPerCfToKgPerM3(const double &lbPerCf) { return lbPerCf * GETEM_KGM3_PER_LBF3; }
	double LbPerCfToKgPerM3_B(const double &lbPerCf) { return (IMITATE_GETEM) ? lbPerCf * 16.01846 : lbPerCf * GETEM_KGM3_PER_LBF3; }

	double KgToLb(const double &kg) { return kg * GETEM_LB_PER_KG; }
	double LbToKg(const double &lb) { return lb / GETEM_LB_PER_KG; }

	double HPtoKW(const double &hp) { return hp * GETEM_KW_PER_HP; }
	double KWtoHP(const double &kw) { return kw / GETEM_KW_PER_HP; }

	double PSItoFTB(const double &psi) { return (IMITATE_GETEM) ? psi * 144 / 62 : physics::PSItoFT(psi); }  // convert PSI to pump 'head' in feet.  assumes water density ~ 62 lb/ft^3 if imitating GETEM

	double pumpSizeInHP(const double &flow_LbPerHr, const double &head_Ft, const double &eff, std::string sErr)
	{
		if (eff <= 0) {
			sErr = ("Pump efficiency <= 0 in 'pumpSizeInHP'.");
			return 0;
		}
		return (flow_LbPerHr * head_Ft) / (60 * 33000 * eff);
	}

	double pumpWorkInWattHr(const double &flow_LbPerHr, const double &head_Ft, const double &eff, std::string sErr)
	{
		return HPtoKW(1000 * pumpSizeInHP(flow_LbPerHr, head_Ft, eff, sErr));
	}

	double calcEGSTemperatureConstant(double tempC, double maxSecondLawEff)
	{	// not explained.  a constant used to calculate the 'average water temp' for EGS resource
		// it's used in [7C.EGS Subsrfce HX].D127, and to do the makeup calculations in [6Bb.Makeup-EGS HX]
		double c1 = (-0.0006 * tempC) - 0.0681;
		double c2 = (-0.0004 * tempC) + 1.0166;
		double c3 = (maxSecondLawEff * c1) + c2;
		double c4 = (-0.0002 * tempC) + 0.9117;
		double c5 = (-0.001 * tempC) + 0.55;
		return (tempC < 150) ? c3 : ((maxSecondLawEff < c5) ? c3 : c4);
	}

	double calcEGSAverageWaterTemperatureC(double temp1C, double temp2C, double maxEff)
	{
		return physics::KelvinToCelcius(physics::CelciusToKelvin(temp1C) * calcEGSTemperatureConstant(temp2C, maxEff));
	}


	double gauss_error_function(const double &x)
	{
		// Based VBA code in Xnumbers.xla v 5.6
		// by Foxes Team, 2007
		// E -mail: leovlp@libero.it
		// Web:    http://digilander.libero.it/foxes
		// 10.11.2006


		//if (is) wxMessageBox(wxString::Format("Reached max loops in ERFC calculation (u <= 2). x = %2.10f\n", x));


		// returns the integral of Gauss' standard error function and complimentary error function
		// first 6 digits match MS Excel erfc function, but they are different after that
		int i;
		double u, a0, a1, a2, b0, B1, b2, g, t, p, s, f1, f2 = 0, d;
		double y, yc; // y = err function, yc = complimentary error function
		const int maxloop = 2000;
		const double tiny = 10e-15;
		u = fabs(x);   //10.11.06 fix bug for x<<0. Thanks to Michael Hautus
		if (u <= 2)
		{
			t = 2 * u * u; p = 1; s = 1;
			for (i = 3; i <= maxloop; i = i + 2)
			{
				p = p * t / i;
				s = s + p;
				if (p < tiny) break;
			}
			//if (i >= maxloop - 1) wxMessageBox(("Reached max loops in ERFC calculation (u<=2)"));
			//if (i >= maxloop - 1)
			//	wxMessageBox(wxString::Format("Reached max loops in ERFC calculation (u <= 2). x = %2.10f\n", x));
			y = 2 * s * u * exp(-u * u) / sqrt(physics::PI);
			if (x < 0) y = -y;
			yc = 1 - y;
		}
		else
		{
			a0 = 1; b0 = 0; a1 = 0; B1 = 1; f1 = 0;
			for (i = 1; i <= maxloop; i++)
			{
				g = 2 - fmod(i, 2.0);
				a2 = g * u * a1 + i * a0;
				b2 = g * u * B1 + i * b0;
				f2 = a2 / b2;
				d = fabs(f2 - f1);
				if (d < tiny) break;
				a0 = a1 / b2;
				b0 = B1 / b2;
				a1 = a2 / b2;
				B1 = 1;
				f1 = f2;
			}
			//if (i >= maxloop - 1) wxMessageBox(("Reached max loops in ERFC calculation (u > 2)"));
	//		if (i >= maxloop - 1)
	//			wxMessageBox(wxString::Format("Reached max loops in ERFC calculation (u > 2). x = %2.10f\n", x));
			yc = 2 * exp(-u * u) / (2 * u + f2) / sqrt(physics::PI);
			y = 1 - yc;
			if (x < 0) { y = -y; yc = 2 - yc; }
		}
		return yc;  // y = err function, yc = complimentary error function
	}

	double evaluatePolynomial(const double &x, const double &c0, const double &c1, const double &c2, const double &c3, const double &c4, const double &c5, const double &c6)
	{
		return (c0 + (c1 * x) + (c2 * pow(x, 2)) + (c3 * pow(x, 3)) + (c4 * pow(x, 4)) + (c5 * pow(x, 5)) + (c6 * pow(x, 6)));
	}

	// Convert foot-lbs per hour to watt-hr/lb and include pump efficiency
	double FrictionFactor(double Re) { return pow((0.79 * log(Re) - 1.640), -2); }

	/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// Create CPolynomial class and objects to use throughout code
	/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	class CPolynomial
	{
	public:
		CPolynomial(void) { md1 = 0.0; md2 = 0.0; md3 = 0.0; md4 = 0.0; md5 = 0.0; md6 = 0.0; md7 = 0.0; }
		CPolynomial(const double &c1) { md1 = c1; md2 = 0.0; md3 = 0.0; md4 = 0.0; md5 = 0.0; md6 = 0.0; md7 = 0.0; }
		CPolynomial(const double &c1, const double &c2) { md1 = c1; md2 = c2; md3 = 0.0; md4 = 0.0; md5 = 0.0; md6 = 0.0; md7 = 0.0; }
		CPolynomial(const double &c1, const double &c2, const double &c3) { md1 = c1; md2 = c2; md3 = c3; md4 = 0.0; md5 = 0.0; md6 = 0.0; md7 = 0.0; }
		CPolynomial(const double &c1, const double &c2, const double &c3, const double &c4) { md1 = c1; md2 = c2; md3 = c3; md4 = c4; md5 = 0.0; md6 = 0.0; md7 = 0.0; }
		CPolynomial(const double &c1, const double &c2, const double &c3, const double &c4, const double &c5) { md1 = c1; md2 = c2; md3 = c3; md4 = c4; md5 = c5; md6 = 0.0; md7 = 0.0; }
		CPolynomial(const double &c1, const double &c2, const double &c3, const double &c4, const double &c5, const double &c6) { md1 = c1; md2 = c2; md3 = c3; md4 = c4; md5 = c5; md6 = c6; md7 = 0.0; }
		CPolynomial(const double &c1, const double &c2, const double &c3, const double &c4, const double &c5, const double &c6, const double &c7) { md1 = c1; md2 = c2; md3 = c3; md4 = c4; md5 = c5; md6 = c6; md7 = c7; }
		virtual ~CPolynomial(void) {}

		//void init(const double &c1, const double &c2, const double &c3, const double &c4, const double &c5, const double &c6, const double &c7) { md1=c1; md2=c2; md3=c3; md4=c4; md5=c5; md6=c6; md7=c7; }
		double evaluate(double val) { return evaluatePolynomial(val, md1, md2, md3, md4, md5, md6, md7); }

	private:
		double md1, md2, md3, md4, md5, md6, md7;

	};

	// Enthalpy and Entropy Constants
	CPolynomial oAmbientEnthalpyConstants(-31.76958886, 0.997066497, 0.00001087);
	CPolynomial oAmbientEntropyConstants(-0.067875028480951, 0.002201824618666, -0.000002665154152, 0.000000004390426, -0.000000000004355);
	CPolynomial oBinaryEnthalpyConstants(-24.113934502, 0.83827719984, 0.0013462856545, -5.9760546933E-6, 1.4924845946E-8, -1.8805783302E-11, 1.0122595469E-14);
	CPolynomial oBinaryEntropyConstants(-0.060089552413, 0.0020324314656, -1.2026247967E-6, -1.8419111147E-09, 8.8430105661E-12, -1.2945213491E-14, 7.3991541798E-18);
	CPolynomial oFlashEnthalpyConstants(-32.232886, 1.0112508, -0.00013079803, 0.00000050269721, -0.00000000050170088, 1.5041709E-13, 7.0459062E-16);
	CPolynomial oFlashEntropyConstants(-0.067756238, 0.0021979159, -0.0000026352004, 0.0000000045293969, -6.5394475E-12, 6.2185729E-15, -2.2525163E-18);

	// specific volume calculation constants
	CPolynomial oSVC(0.017070951786, -0.000023968043944, 0.00000022418007508, -9.1528222658E-10, 2.1775771856E-12, -2.6995711458E-15, 1.4068205291E-18);

	// pressure calculation constants
	CPolynomial oPC(8.0894106754, -0.19788525656, 0.0019695373372, -0.0000091909636468, 0.000000024121846658, -2.5517506351E-12);
	CPolynomial oPressureAmbientConstants(0.320593729630411, -0.0156410175570826, 0.0003545452343917, -0.0000027120923771, 0.0000000136666056);
	CPolynomial oDensityConstants(62.329, 0.0072343, -0.00012456, 0.00000020215, -0.00000000017845);
	CPolynomial oFlashTempConstants(113.186, -2.48032, 0.0209139, -0.0000557641, 0.0000000542893);

	// used in calculating flash brine effectiveness
	CPolynomial oFlashConstants1(-1.306483, 0.2198881, -0.003125628, 0.0000173028, -0.00000003258986);
	CPolynomial oFlashConstants2(0.01897203, -0.0002054368, 0.000002824477, -0.00000001427949, 0.00000000002405238);
	CPolynomial oPSatConstants(0.0588213, -0.0018299913, 0.00010459209, -0.00000084085735, 0.0000000086940123);

	// EGS
	CPolynomial oEGSDensity(0.001003773308, -0.00000043857183, 0.00000001365689, -0.00000000006419, 0.00000000000013);
	CPolynomial oEGSSpecificHeat(4.301651536642, -0.011554722573, 0.00020328187235, -0.0000011433197, 0.00000000217642);

	// Min geothermal fluid outlet temperatures to prevent Si precipitation
	// If fluid temp >= 356 degrees F (180 C), use quartz curve
	CPolynomial oMinimumTemperatureQuartz(-159.597976, 0.69792956, 0.00035129);
	// If fluid temp < 356 degrees F (180 C), use chalcedony curve
	CPolynomial oMinimumTemperatureChalcedony(-127.71, 0.8229);

	// The constants for the following 19 objects were only used within the CGETEMGlobals class
	CPolynomial oDHaUnder150(60.251233, -0.28682223, 0.0049745244, -0.000050841601, 0.00000026431087, -0.00000000054076309);
	CPolynomial oDHa150To1500(53.67656, -0.02861559, 0.0000469389, -0.000000047788062, 0.000000000024733176, -5.0493347E-15);
	CPolynomial oDHaOver1500(123.86562, -0.18362579, 0.00016780015, -0.000000077555328, 0.000000000017815452, -1.6323827E-15);
	CPolynomial oDHbUnder150(-2.1991099, 1.4133748, -0.019163136, 0.0001766481, -0.00000087079731, 0.0000000017257066);
	CPolynomial oDHb150To1500(33.304544, 0.27192791, -0.00045591346, 0.000000443209, -0.00000000022501399, 4.5323448E-14);
	CPolynomial oDHbOver1500(740.43412, -1.5040745, 0.0014334909, -0.00000067364263, 0.00000000015600207, -1.4371477E-14);

	// Getting enthalpy from temperature
	CPolynomial oFlashEnthalpyFUnder125(-32.479184, 1.0234315, -0.00034115062, 0.0000020320904, -0.000000004480902);
	CPolynomial oFlashEnthalpyF125To325(-31.760088, 0.9998551, -0.000027703224, 0.000000073480055, 0.00000000025563678);
	CPolynomial oFlashEnthalpyF325To675(-1137.0718729, 13.426933583, -0.055373746094, 0.00012227602697, -0.00000013378773724, 5.8634263518E-11);
	CPolynomial oFlashEnthalpyFOver675(-5658291651.7, 41194401.715, -119960.00955, 174.6587566, -0.12714518982, 0.000037021613128);

	CPolynomial oFlashEnthalpyGUnder125(1061.0996074, 0.44148580795, -0.000030268712038, -0.00000015844186585, -7.2150559138E-10);
	CPolynomial oFlashEnthalpyG125To325(1061.9537518, 0.42367961566, 0.000099006018886, -0.00000051596852593, -0.0000000005035389718);
	CPolynomial oFlashEnthalpyG325To675(-3413.791688, 60.38391862, -0.33157805684, 0.00096963380389, -0.0000015842735401, 0.0000000013698021251, -4.9118123157E-13);
	CPolynomial oFlashEnthalpyGOver675(7355226428.1, -53551582.984, 155953.29919, -227.07686319, 0.16531315908, -0.000048138033984);

	// Getting temperature from pressure
	CPolynomial oFlashTemperatureUnder2(14.788238833, 255.85632577, -403.56297354, 400.57269432, -222.30982965, 63.304761377, -7.1864066799);
	CPolynomial oFlashTemperature2To20(78.871966537, 31.491049082, -4.8016701723, 0.49468791547, -0.029734376328, 0.00094358038872, -0.000012178121702);
	CPolynomial oFlashTemperature20To200(161.40853789, 4.3688747745, -0.062604066919, 0.00061292292067, -0.0000034988475881, 0.00000001053096688, -1.2878309875E-11);
	CPolynomial oFlashTemperature200To1000(256.29706201, 0.93056131917, -0.0020724712921, 0.0000034048164769, -0.0000000034275245432, 1.8867165569E-12, -4.3371351471E-16);
	CPolynomial oFlashTemperatureOver1000(342.90613285, 0.33345911089, -0.00020256473758, 0.000000094407417758, -2.7823504188E-11, 4.589696886E-15, -3.2288675486E-19);

	// Second law equations, used in FractionOfMaxEfficiency
	CPolynomial oSecondLawConstantsBinary(130.8952, -426.5406, 462.9957, -166.3503); // ("6Ab. Makeup-Annl%").Range("R24:R27")
	CPolynomial oSecondLawConstantsSingleFlash(-3637.06, 25.7411, -0.0684072, 0.0000808782, -0.0000000359423);	// ("6Ef.Flash Makeup").Range("R20:V20")
	CPolynomial oSecondLawConstantsDualFlashNoTempConstraint(-2762.4048, 18.637876, -0.047198813, 0.000053163057, -0.000000022497296); // ("6Ef.Flash Makeup").Range("R22:V22")
	CPolynomial oSecondLawConstantsDualFlashWithTempConstraint(-4424.6599, 31.149268, -0.082103498, 0.000096016499, -0.00000004211223);	// ("6Ef.Flash Makeup").Range("R21:V21")


	//Specific Volume Coefficients (Used in Flass Vessels Cost Calculation):
	CPolynomial specVolUnder125(11678.605, -464.41472, 8.9931223, -0.1033793, 0.00071596466, -0.0000027557218, 0.0000000045215227);
	CPolynomial specVol125to325(3890.919, -83.834081, 0.78482148, -0.0040132715, 0.000011692082, -0.000000018270648, 0.000000000011909478);
	CPolynomial specVol325to675(268.32894, -2.7389634, 0.011958041, -0.000028277928, 0.000000037948334, -0.000000000027284644, 8.187709e-15);
	CPolynomial specVolOver675(1786.8983, 10.645163, -0.023769687, 0.000023582903, -0.0000000087731388);

	double EGSWaterDensity(double tempC) { return 1 / oEGSDensity.evaluate(tempC); }			// kg/m^3
	double EGSSpecificHeat(double tempC) { return oEGSSpecificHeat.evaluate(tempC) * 1000; }	// J/kg-C




	double GetDHa(double pressurePSI)
	{
		if (pressurePSI > 1500)
			return oDHaOver1500.evaluate(pressurePSI);
		else if (pressurePSI > 150)
			return oDHa150To1500.evaluate(pressurePSI);
		else
			return oDHaUnder150.evaluate(pressurePSI);
	}

	double GetDHb(double pressurePSI)
	{
		if (pressurePSI > 1500)
			return oDHbOver1500.evaluate(pressurePSI);
		else if (pressurePSI > 150)
			return oDHb150To1500.evaluate(pressurePSI);
		else
			return oDHbUnder150.evaluate(pressurePSI);
	}

	double GetFlashEnthalpyF(double temperatureF)
	{
		if (temperatureF > 675)
			return  oFlashEnthalpyFOver675.evaluate(temperatureF);
		else if (temperatureF > 325)
			return  oFlashEnthalpyF325To675.evaluate(temperatureF);
		else if (temperatureF > 125)
			return  oFlashEnthalpyF125To325.evaluate(temperatureF);
		else
			return  oFlashEnthalpyFUnder125.evaluate(temperatureF);
	}

	double GetFlashEnthalpyG(double temperatureF)
	{
		if (temperatureF > 675)
			return  oFlashEnthalpyGOver675.evaluate(temperatureF);
		else if (temperatureF > 325)
			return  oFlashEnthalpyG325To675.evaluate(temperatureF);
		else if (temperatureF > 125)
			return  oFlashEnthalpyG125To325.evaluate(temperatureF);
		else
			return  oFlashEnthalpyGUnder125.evaluate(temperatureF);
	}

	double GetFlashTemperature(double pressurePSI)
	{
		if (pressurePSI > 1000)
			return  oFlashTemperatureOver1000.evaluate(pressurePSI);
		else if (pressurePSI > 200)
			return  oFlashTemperature200To1000.evaluate(pressurePSI);
		else if (pressurePSI > 20)
			return  oFlashTemperature20To200.evaluate(pressurePSI);
		else if (pressurePSI > 2)
			return  oFlashTemperature2To20.evaluate(pressurePSI);
		else
			return  oFlashTemperatureUnder2.evaluate(pressurePSI);
	}

	double getSpecVol(double flashTempF)
	{
		if (flashTempF > 675)
			return specVolOver675.evaluate(flashTempF);
		else if (flashTempF > 325)
			return specVol325to675.evaluate(flashTempF);
		else if (flashTempF > 125)
			return specVol125to325.evaluate(flashTempF);
		else
			return specVolUnder125.evaluate(flashTempF);

	}

	double GetSiPrecipitationTemperatureF(double geoFluidTempF)
	{
		return (geoFluidTempF >= 356) ? oMinimumTemperatureQuartz.evaluate(geoFluidTempF) : oMinimumTemperatureChalcedony.evaluate(geoFluidTempF);
	}



	/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// Declaration of CGeoFluidContainer2 
	/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	class CGeoFluidContainer2
	{
	public:
		double GetAEForBinaryWattHr(double tempF, double ambientTempF) { return physics::toWattHr(GetAEForBinaryBTU(tempF, ambientTempF)); }
		double GetAEForFlashWattHr(double tempF, double ambientTempF) { return physics::toWattHr(GetAEForFlashBTU(tempF, ambientTempF)); }

		double GetAEForBinaryWattHrUsingC(double tempC, double ambientTempC) { return GetAEForBinaryWattHr(physics::CelciusToFarenheit(tempC), physics::CelciusToFarenheit(ambientTempC)); }
		double GetAEForFlashWattHrUsingC(double tempC, double ambientTempC) { return GetAEForFlashWattHr(physics::CelciusToFarenheit(tempC), physics::CelciusToFarenheit(ambientTempC)); }

	private:
		double GetAEForBinaryBTU(double tempHighF, double tempLowF)
		{
			return (oBinaryEnthalpyConstants.evaluate(tempHighF) - oAmbientEnthalpyConstants.evaluate(tempLowF)) - ((tempLowF + 460) * (oBinaryEntropyConstants.evaluate(tempHighF) - oAmbientEntropyConstants.evaluate(tempLowF)));
		}

		double GetAEForFlashBTU(double tempHighF, double tempLowF)
		{
			return (oFlashEnthalpyConstants.evaluate(tempHighF) - oAmbientEnthalpyConstants.evaluate(tempLowF)) - ((tempLowF + 460) * (oFlashEntropyConstants.evaluate(tempHighF) - oAmbientEntropyConstants.evaluate(tempLowF)));
		}

	};
	CGeoFluidContainer2 oGFC;

};// namespace geotheraml




//******************************************************************************************************************************************************************************
//******************************************************************************************************************************************************************************
// Implementation of CGeoHourlyAnalysis
//******************************************************************************************************************************************************************************
//******************************************************************************************************************************************************************************
CGeothermalAnalyzer::CGeothermalAnalyzer(const SGeothermal_Inputs& gti, SGeothermal_Outputs& gto)
	: mp_geo_out(&gto), mo_geo_in(gti)
{
	init();
}

CGeothermalAnalyzer::CGeothermalAnalyzer(const SPowerBlockParameters& pbp, SPowerBlockInputs& pbi, const SGeothermal_Inputs& gti, SGeothermal_Outputs& gto)
	: mp_geo_out(&gto), mo_geo_in(gti), mo_pb_p(pbp), mo_pb_in(pbi)
{
	init();
}

CGeothermalAnalyzer::~CGeothermalAnalyzer(void)
{
	// delete anything?
}

void CGeothermalAnalyzer::init()
{	// code common to constructors
	ms_ErrorString = "";
	mf_LastIntervalDone = 0.0f;
	mb_WeatherFileOpen = false;
	ml_ReadCount = 0;
	ml_HourCount = 0;
	me_makeup = NO_MAKEUP_ALGORITHM;
	mi_ReservoirReplacements = 0;
	md_WorkingTemperatureC = 0.0;
	md_LastProductionTemperatureC = 0.0;
	md_TimeOfLastReservoirReplacement = 0.0;
}

bool CGeothermalAnalyzer::IsHourly() { return (mo_geo_in.mi_MakeupCalculationsPerYear == 8760) ? true : false; }

double CGeothermalAnalyzer::PlantGrossPowerkW(void)
{
	double dPlantBrineEfficiency = 0;  // plant Brine Efficiency as a function of temperature
	switch (me_makeup)
	{
	case MA_BINARY:
		dPlantBrineEfficiency = MaxSecondLawEfficiency() * mo_geo_in.md_PlantEfficiency * ((geothermal::IMITATE_GETEM) ? GetAEBinary() : GetAE());				//MaxSecondLawEfficiency() * FractionOfMaxEfficiency() * GetAEBinaryAtTemp(md_WorkingTemperatureC);
		break;

	case MA_FLASH:
		dPlantBrineEfficiency = MaxSecondLawEfficiency() * FractionOfMaxEfficiency() * GetAEFlashAtTemp(md_WorkingTemperatureC);
		break;

	case MA_EGS:
		dPlantBrineEfficiency = MaxSecondLawEfficiency() * FractionOfMaxEfficiency() * GetAEBinaryAtTemp(md_WorkingTemperatureC);
		break;

	default: ms_ErrorString = ("Invalid make up technology in CGeothermalAnalyzer::PlantGrossPowerkW"); return 0;
	}

	return dPlantBrineEfficiency * flowRateTotal() / 1000.0;
}

double CGeothermalAnalyzer::MaxSecondLawEfficiency()
{
	// the available energy, in GETEM (dGetemAEForSecondLaw), is actually based on plant design temp, although resource temp is being used to calculate the output
	// this only matters for EGS resources, where resource temp and plant design temp are different
	// this leads to Plant brine effectiveness higher than input values
	// which leads to actual plant output(after pumping losses) > design output (before pump losses) ??
	// which leads to relative revenue > 1 ??
	double dGetemAEForSecondLaw = (geothermal::IMITATE_GETEM) ? GetAEBinary() : GetAE(); // GETEM uses the correct ambient temperature, but it always uses Binary constants, even if flash is chosen as the conversion technology
	mp_geo_out->eff_secondlaw = GetPlantBrineEffectiveness() / dGetemAEForSecondLaw;	//2nd law efficiency used in direct plant cost calculations. This is NOT the same as the MAX 2nd law efficiency.
	if (me_makeup == MA_BINARY)
		return (mp_geo_out->max_secondlaw);
	else
		return (GetPlantBrineEffectiveness() / dGetemAEForSecondLaw);
}


double CGeothermalAnalyzer::FractionOfMaxEfficiency()
{
	double dTemperatureRatio = 0.0;
	if (me_makeup == MA_EGS)
		dTemperatureRatio = physics::CelciusToKelvin(md_LastProductionTemperatureC) / physics::CelciusToKelvin(GetTemperaturePlantDesignC());
	else
		dTemperatureRatio = physics::CelciusToKelvin(md_WorkingTemperatureC) / physics::CelciusToKelvin(GetTemperaturePlantDesignC());

	if (me_makeup == MA_FLASH)
	{
		switch (mo_geo_in.me_ft)
		{
		case SINGLE_FLASH_NO_TEMP_CONSTRAINT:
		case SINGLE_FLASH_WITH_TEMP_CONSTRAINT:
			return (1.1 - (0.1 * pow(dTemperatureRatio, geothermal::oSecondLawConstantsSingleFlash.evaluate(physics::CelciusToKelvin(GetResourceTemperatureC())))));

		case DUAL_FLASH_NO_TEMP_CONSTRAINT:
			return (1.1 - (0.1 * pow(dTemperatureRatio, geothermal::oSecondLawConstantsDualFlashNoTempConstraint.evaluate(physics::CelciusToKelvin(GetResourceTemperatureC())))));

		case DUAL_FLASH_WITH_TEMP_CONSTRAINT:
			return (1.1 - (0.1 * pow(dTemperatureRatio, geothermal::oSecondLawConstantsDualFlashWithTempConstraint.evaluate(physics::CelciusToKelvin(GetResourceTemperatureC())))));

		default: ms_ErrorString = ("Invalid flash technology in CGeothermalAnalyzer::FractionOfMaxEfficiency"); return 0;
		}

	}
	else // Binary and EGS
		return(dTemperatureRatio > 0.98) ? geothermal::oSecondLawConstantsBinary.evaluate(dTemperatureRatio) : 1.0177 * pow(dTemperatureRatio, 2.6237);
}

bool CGeothermalAnalyzer::CanReplaceReservoir(double dTimePassedInYears)
{
	return ((mi_ReservoirReplacements < NumberOfReservoirs()) && (dTimePassedInYears + geothermal::FINAL_YEARS_WITH_NO_REPLACEMENT <= mo_geo_in.mi_ProjectLifeYears)) ? true : false;
}

void CGeothermalAnalyzer::CalculateNewTemperature(double dElapsedTimeInYears)
{
	if (me_makeup != MA_EGS)
		md_WorkingTemperatureC = md_WorkingTemperatureC * (1 - (mo_geo_in.md_TemperatureDeclineRate / 12));
	else
	{
		// The EGS temperature drop depends on the amount of fluid being produced (makes intuitive sense).
		md_LastProductionTemperatureC = md_WorkingTemperatureC;

		double dAverageReservoirTempC = geothermal::calcEGSAverageWaterTemperatureC(md_LastProductionTemperatureC, md_LastProductionTemperatureC, MaxSecondLawEfficiency());
		//double dDaysSinceLastReDrill = (md_YearsAtNextTimeStep - md_TimeOfLastReservoirReplacement) * geothermal::DAYS_PER_YEAR;
		double dDaysSinceLastReDrill = (dElapsedTimeInYears - md_TimeOfLastReservoirReplacement) * geothermal::DAYS_PER_YEAR;
		double dFunctionOfRockProperties = EGSReservoirConstant(dAverageReservoirTempC, dDaysSinceLastReDrill); //[6Bb.Makeup-EGS HX] column AG

		double tempBrineEfficiencyC = physics::KelvinToCelcius(exp((-0.42 * log(md_LastProductionTemperatureC) + 1.4745) * MaxSecondLawEfficiency() * FractionOfMaxEfficiency()) * physics::CelciusToKelvin(md_LastProductionTemperatureC));
		double tempSILimitC = physics::FarenheitToCelcius(geothermal::GetSiPrecipitationTemperatureF(physics::CelciusToFarenheit(md_LastProductionTemperatureC)));
		double dNewInjectionTemperatureC = MAX(tempBrineEfficiencyC, tempSILimitC);
		double dNewEGSProductionTemperatureC = GetResourceTemperatureC() + ((dNewInjectionTemperatureC - GetResourceTemperatureC()) * dFunctionOfRockProperties);

		md_WorkingTemperatureC = dNewEGSProductionTemperatureC;
	}
}


double CGeothermalAnalyzer::GetPumpWorkKW(void)
{
	return (mo_geo_in.mb_CalculatePumpWork) ? GetPumpWorkWattHrPerLb() * flowRateTotal() / 1000.0 : mo_geo_in.md_UserSpecifiedPumpWorkKW;
}

double CGeothermalAnalyzer::NumberOfReservoirs(void)
{
	double d1 = GetAEBinary();
	if ((d1 == 0) && (geothermal::IMITATE_GETEM))
	{
		ms_ErrorString = ("GetAEBinary returned zero in CGeothermalAnalyzer::NumberOfReservoirs. Could not calculate the number of reservoirs.");
		return 0;
	}

	double dFactor = (geothermal::IMITATE_GETEM) ? GetAE() / d1 : 1;
	double dPlantOutputKW = dFactor * flowRateTotal() * GetPlantBrineEffectiveness() / 1000.0; // KW = (watt-hr/lb)*(lbs/hr) / 1000
	if (dPlantOutputKW == 0)
	{
		ms_ErrorString = ("The Plant Output was zero in CGeothermalAnalyzer::NumberOfReservoirs. Could not calculate the number of reservoirs.");
		return 0;
	}
	return floor(mo_geo_in.md_PotentialResourceMW * 1000 / dPlantOutputKW);
}


double CGeothermalAnalyzer::CalculatePumpWorkInKW(double dFlowLbPerHr, double dPumpHeadFt)
{
	//	double test = geothermal::pumpWorkInWattHr(dFlowLbPerHr, dPumpHeadFt, geothermal::EFFICIENCY_PUMP_FLASH, ms_ErrorString);

	return geothermal::HPtoKW((dFlowLbPerHr * dPumpHeadFt) / (60 * 33000 * geothermal::EFFICIENCY_PUMP_FLASH));
}

double CGeothermalAnalyzer::GetPumpWorkWattHrPerLb(void)
{	// Enter 1 for flow to Get power per lb of flow
	//double dProductionPumpPower = geothermal::pumpWorkInWattHr(1, pumpHeadFt(), geothermal::EFFICIENCY_PUMP_GF, ms_ErrorString);
	double dProductionPumpPower = geothermal::pumpWorkInWattHr(1, pumpHeadFt(), mo_geo_in.md_GFPumpEfficiency, ms_ErrorString);
	if (!ms_ErrorString.empty()) return 0;

	double dInjectionPumpPower = 0;
	if (geothermal::ADDITIONAL_PRESSURE_REQUIRED)
	{
		double dWaterLoss = (1 / (1 - geothermal::WATER_LOSS_PERCENT)); // G130 - lb/hr

		double dFractionOfInletGFInjected = 1.0;
		if (mo_geo_in.me_rt == EGS)
			dFractionOfInletGFInjected = (1 + geothermal::WATER_LOSS_PERCENT);
		else if (mo_geo_in.me_ct == FLASH)
		{
			calculateFlashPressures();
			double dWaterLossFractionOfGF = waterLoss() / geothermal::GEOTHERMAL_FLUID_FOR_FLASH;
			return (1 - dWaterLossFractionOfGF);
		}

		// Calculate injection pump items, on [7A.GF Pumps] unless otherwise noted
		double dInjectionPressure = mo_geo_in.md_AdditionalPressure + geothermal::BarToPsi(mo_geo_in.md_ExcessPressureBar) + GetPressureChangeAcrossReservoir();
		if (mo_geo_in.md_AdditionalPressure < 0)
		{
			//			double injectionPressurePSI = 150 - (pressureInjectionWellBottomHolePSI() - pressureHydrostaticPSI() );
			//			double dInjectionPressure = (injectionPressurePSI < 0) ? 0 : injectionPressurePSI; // G40,  If it's less than zero, use zero.
		}
		double dInjectionPumpHeadFt = dInjectionPressure * 144 / InjectionDensity(); // G129

		//dInjectionPumpPower = geothermal::pumpWorkInWattHr(dWaterLoss, dInjectionPumpHeadFt, geothermal::EFFICIENCY_PUMP_GF, ms_ErrorString) * dFractionOfInletGFInjected; // ft-lbs/hr
		dInjectionPumpPower = geothermal::pumpWorkInWattHr(dWaterLoss, dInjectionPumpHeadFt, mo_geo_in.md_GFPumpEfficiency, ms_ErrorString) * dFractionOfInletGFInjected; // ft-lbs/hr

	}
	double retVal = dProductionPumpPower + dInjectionPumpPower; // watt-hr per lb of flow

	if (retVal < 0)
	{
		ms_ErrorString = ("CGeothermalAnalyzer::GetPumpWorkWattHrPerLb calculated a value < 0");
		return 0;
	}

	return retVal;
}

double CGeothermalAnalyzer::GetCalculatedPumpDepthInFeet(void)
{	// Calculate the pumpSetDepth

	// mp_geo_out->md_BottomHolePressure; // [7B.Reservoir Hydraulics].G75
	double dInectionPumpHeadUsed = 0; // [2B.Resource&Well Input].D162

	if (mo_geo_in.me_rt == EGS)
		mp_geo_out->md_BottomHolePressure = (pressureInjectionWellBottomHolePSI() + dInectionPumpHeadUsed) - GetPressureChangeAcrossReservoir();
	else
		mp_geo_out->md_BottomHolePressure = pressureHydrostaticPSI() - GetPressureChangeAcrossReservoir();

	double pressureDiff = mp_geo_out->md_BottomHolePressure - pressureWellHeadPSI();
	double dDiameterProductionWellFt = mo_geo_in.md_DiameterProductionWellInches / 12;

	double areaWell = physics::areaCircle(dDiameterProductionWellFt / 2); // ft^2
	double velocityWell = productionFlowRate() / areaWell; // [7A.GF Pumps].G70
	double ReWell = dDiameterProductionWellFt * velocityWell * productionDensity() / productionViscosity();
	double frictionHeadLossWell = (geothermal::FrictionFactor(ReWell) / dDiameterProductionWellFt)* pow(velocityWell, 2) / (2 * physics::GRAVITY_FTS2);

	double pumpSetting = ((pressureDiff * 144) / productionDensity())*(1 - frictionHeadLossWell);   // [7A.GF Pumps].D89
	return (geothermal::MetersToFeet(GetResourceDepthM()) - pumpSetting < 0) ? 0 : geothermal::MetersToFeet(GetResourceDepthM()) - pumpSetting; // feet - [7A.GF Pumps].D90
}

double CGeothermalAnalyzer::pumpHeadFt() // ft
{	// calculate the friction head loss of the casing
	double dDiameterPumpCasingFt = mo_geo_in.md_DiameterPumpCasingInches / 12;
	double areaCasing = physics::areaCircle(dDiameterPumpCasingFt / 2); // ft^2
	double velocityCasing = productionFlowRate() / areaCasing;

	double dReCasing = dDiameterPumpCasingFt * velocityCasing * productionDensity() / productionViscosity();
	double frictionHeadLossCasing = (geothermal::FrictionFactor(dReCasing) * GetCalculatedPumpDepthInFeet() / dDiameterPumpCasingFt)* pow(velocityCasing, 2) / (2 * physics::GRAVITY_FTS2); //feet

	// Add (friction head loss) and (pump Set depth) to Get total pump head.

	return frictionHeadLossCasing + GetCalculatedPumpDepthInFeet();
}



void CGeothermalAnalyzer::ReplaceReservoir(double dElapsedTimeInYears)
{
	mi_ReservoirReplacements++;
	md_WorkingTemperatureC = GetResourceTemperatureC();

	if (me_makeup == MA_EGS)
	{	// have to keep track of the last temperature of the working fluid, and the last time the reservoir was "replaced" (re-drilled)
		md_LastProductionTemperatureC = md_WorkingTemperatureC;
		double dYearsAtNextTimeStep = dElapsedTimeInYears + (1.0 / 12.0);
		if (dElapsedTimeInYears > 0) md_TimeOfLastReservoirReplacement = dYearsAtNextTimeStep - (EGSTimeStar(EGSAverageWaterTemperatureC2()) / geothermal::DAYS_PER_YEAR);
	}
}

double CGeothermalAnalyzer::GetTemperatureGradient(void) // degrees C per km
{	// Conversation with Chad on August 30th 2010, 10am MT - just use the average gradient, even if it's changing at that point according to the depth/temp graph.
	if (mo_geo_in.me_rt == HYDROTHERMAL) { return ((mo_geo_in.md_TemperatureResourceC - GetAmbientTemperatureC(BINARY)) / mo_geo_in.md_ResourceDepthM) * 1000; }
	return ((mo_geo_in.md_TemperatureResourceC - mo_geo_in.md_TemperatureEGSAmbientC) / mo_geo_in.md_ResourceDepthM) * 1000;
}

double CGeothermalAnalyzer::GetResourceTemperatureC(void) // degrees C
{
	if ((mo_geo_in.me_rt == EGS) && (mo_geo_in.me_dc == DEPTH)) return ((mo_geo_in.md_ResourceDepthM / 1000) * GetTemperatureGradient()) + mo_geo_in.md_TemperatureEGSAmbientC;
	return mo_geo_in.md_TemperatureResourceC;
}

double CGeothermalAnalyzer::GetTemperaturePlantDesignC(void) { return (mo_geo_in.me_rt == EGS) ? mo_geo_in.md_TemperaturePlantDesignC : GetResourceTemperatureC(); }

double CGeothermalAnalyzer::GetResourceDepthM(void) // meters
{
	if ((mo_geo_in.me_rt == EGS) && (mo_geo_in.me_dc == TEMPERATURE)) return 1000 * (mo_geo_in.md_TemperatureResourceC - mo_geo_in.md_TemperatureEGSAmbientC) / GetTemperatureGradient();
	return mo_geo_in.md_ResourceDepthM;
}

double CGeothermalAnalyzer::GetAmbientTemperatureC(conversionTypes ct)
{
	if (ct == NO_CONVERSION_TYPE) ct = mo_geo_in.me_ct;
	//return (ct == BINARY) ? geothermal::DEFAULT_AMBIENT_TEMPC_BINARY : (1.3842 * geothermal::WET_BULB_TEMPERATURE_FOR_FLASH_CALCS) + 5.1772 ;
	return (ct == BINARY) ? geothermal::DEFAULT_AMBIENT_TEMPC_BINARY : (1.3842 * mo_geo_in.md_TemperatureWetBulbC) + 5.1772;
}

double CGeothermalAnalyzer::InjectionTemperatureC() // calculate injection temperature in degrees C
{	// Plant design temp AND resource temp have to be Set correctly!!!
	// These are the calculations done at the bottom of [10B.GeoFluid] with the result in D89

	// this is used in pump Power calculations, and in EGS energy produciton calculations
	if ((GetTemperaturePlantDesignC() != GetResourceTemperatureC()) && ((me_makeup == MA_BINARY) || (me_makeup == MA_FLASH)))
	{
		ms_ErrorString = ("Resource temperature was not equal to plant design temp in non-EGS analysis in CGeoHourlyBaseInputs::InjectionTemperatureC");
		return 0;
	}

	double a = (-0.000655 * GetTemperaturePlantDesignC()) + 1.01964;
	double b = (-0.00244 * GetTemperaturePlantDesignC()) - 0.0567;
	double dPlantBrineEffectiveness = (geothermal::IMITATE_GETEM) ? 10.35 : GetPlantBrineEffectiveness();
	double eff = dPlantBrineEffectiveness / GetAEBinary(); //available energy based on resource temp


	double tr = a * exp(b*eff);
	double t1 = physics::KelvinToCelcius(physics::CelciusToKelvin(GetTemperaturePlantDesignC()) * tr);
	double t2 = GetAmbientTemperatureC() + 27;

	double x = geothermal::evaluatePolynomial(GetTemperaturePlantDesignC(), 4.205944351495, 0.3672417729236, -0.0036294799613, 0.0000706584462, -0.0000001334837, 0, 0);
	double x1 = geothermal::evaluatePolynomial(x, -0.294394, 0.307616, -0.000119669, -0.00000000425191, 0.0000000000249634, 0, 0);

	double t3 = physics::FarenheitToCelcius(physics::CelciusToFarenheit(x1) + 1);
	double y = (t1 > t2) ? t1 : t2;

	return ((t3 > y) ? t3 : y);
}





double CGeothermalAnalyzer::InjectionTemperatureF(void)
{
	double dInjectionTempForResource = (mo_geo_in.me_rt == EGS) ? geothermal::TEMPERATURE_EGS_INJECTIONC : InjectionTemperatureC();	// D15 - degrees C

	return physics::CelciusToFarenheit(dInjectionTempForResource);	// G15 - degrees F
}

double CGeothermalAnalyzer::InjectionDensity(void) { return (1 / geothermal::oSVC.evaluate(InjectionTemperatureF())); }											// G19,G44, G128 - lb/ft^3


double CGeothermalAnalyzer::GetAEAtTemp(double tempC) { return (mo_geo_in.me_ct == BINARY) ? GetAEBinaryAtTemp(tempC) : GetAEFlashAtTemp(tempC); }
double CGeothermalAnalyzer::GetAEBinaryAtTemp(double tempC) { return geothermal::oGFC.GetAEForBinaryWattHrUsingC(tempC, GetAmbientTemperatureC()); }	// watt-hr/lb - Calculate available energy using binary constants and plant design temp (short cut)
double CGeothermalAnalyzer::GetAEFlashAtTemp(double tempC) { return geothermal::oGFC.GetAEForFlashWattHrUsingC(tempC, GetAmbientTemperatureC()); }	// watt-hr/lb - Calculate available energy using flash constants and plant design temp (short cut)
double CGeothermalAnalyzer::GetAE(void) { return GetAEAtTemp(GetTemperaturePlantDesignC()); }
double CGeothermalAnalyzer::GetAEBinary(void) { return GetAEBinaryAtTemp(GetTemperaturePlantDesignC()); }// watt-hr/lb - Calculate available energy using binary constants and plant design temp (short cut)
double CGeothermalAnalyzer::GetAEFlash(void) { return GetAEFlashAtTemp(GetTemperaturePlantDesignC()); }

double CGeothermalAnalyzer::EGSTimeStar(double tempC)
{
	double dEGSFractureSurfaceArea = mo_geo_in.md_EGSFractureWidthM * EGSFractureLength();
	double d2 = 27 * geothermal::EGSWaterDensity(tempC) * geothermal::EGSSpecificHeat(tempC) * EGSFlowPerFracture(tempC);
	return (pow(EGSThermalConductivity() * dEGSFractureSurfaceArea / (d2), 2) / EGSAlpha()) + EGSLengthOverVelocity(tempC);
}

double CGeothermalAnalyzer::EGSAverageWaterTemperatureC2()
{	// degrees C (used in [6Bb.Makeup-EGS HX ].X35 to calc time*
	return (InjectionTemperatureC() + GetResourceTemperatureC()) / 2;
}

double CGeothermalAnalyzer::EGSThermalConductivity()
{	// convert to J/m-hr-C for hourly analysis
	//return (IsHourly()) ? geothermal::EGS_THERMAL_CONDUCTIVITY/24 : geothermal::EGS_THERMAL_CONDUCTIVITY;
	return geothermal::EGS_THERMAL_CONDUCTIVITY;
}

double CGeothermalAnalyzer::EGSFractureLength()
{	// fEffectiveLength, meters used in pump power calcs
	// Pre Jan-2012 version: 
	return 1000 / cos(mo_geo_in.md_EGSFractureAngle * physics::PI / 180); // Distance was hardwired to 1000m in GETEM
	//return mo_geo_in.md_DistanceBetweenProductionInjectionWellsM / cos(mo_geo_in.md_EGSFractureAngle * physics::PI / 180);
}

double CGeothermalAnalyzer::EGSFlowPerFracture(double tempC)
{	// m^3 per day
	//double dFlowInTimePeriod = (IsHourly()) ? 60*60  : 60*60*24 ; // hourly analysis uses hourly flow, monthly analysis uses daily flow
	double dFlowInTimePeriod = 60 * 60 * 24; // hourly analysis and monthly analyses use daily flow
	return ((mo_geo_in.md_ProductionFlowRateKgPerS / geothermal::EGSWaterDensity(tempC)) / mo_geo_in.md_EGSNumberOfFractures) * dFlowInTimePeriod;
}

double CGeothermalAnalyzer::EGSAlpha(void)
{	// fAlpha (m^2 per day) or (m^2 per hr)
	return EGSThermalConductivity() / (mo_geo_in.md_EGSSpecificHeatConstant * mo_geo_in.md_EGSRockDensity);
}

double CGeothermalAnalyzer::EGSLengthOverVelocity(double tempC)
{
	double dEGSFractureCrossSectionArea = mo_geo_in.md_EGSFractureWidthM * mo_geo_in.md_EGSFractureAperature; // Cross Sectional Area, m^2
	double dEGSVelocity = EGSFlowPerFracture(tempC) / dEGSFractureCrossSectionArea;		// m^3 per day / m^2 = m/day (or hour)
	return EGSFractureLength() / dEGSVelocity; 	// (m / m per day) = days (or hours)
}

double CGeothermalAnalyzer::EGSAvailableEnergy()
{	// watt-hr/lb - not sure why the flash constants are used to calc EGS available energy
	return geothermal::oGFC.GetAEForFlashWattHrUsingC(mo_geo_in.md_TemperaturePlantDesignC, geothermal::TEMPERATURE_EGS_AMBIENT_C);
}

double CGeothermalAnalyzer::EGSReservoirConstant(double avgWaterTempC, double dDays)
{	// all this is from [7C.EGS Subsrfce HX], also from the calculations over time on 6Bb.Makeup-EGS HX, AF62-AF422
	double lv = EGSLengthOverVelocity(avgWaterTempC);	// days (or hours)
	if (dDays <= lv) return 0;

	double cp = geothermal::EGSSpecificHeat(avgWaterTempC);	// J/kg-C
	double rho = geothermal::EGSWaterDensity(avgWaterTempC);	// kg/m^3
	double flow = EGSFlowPerFracture(avgWaterTempC);	// m^3 per day (or per hour)
	double dEGSFractureSurfaceArea = mo_geo_in.md_EGSFractureWidthM * EGSFractureLength();//fFractureSurfaceArea, m^2

	double x = (EGSThermalConductivity() * dEGSFractureSurfaceArea) / (cp * rho * flow * sqrt(EGSAlpha()*(dDays - lv)));
	return geothermal::gauss_error_function(x);
}

double CGeothermalAnalyzer::GetPressureChangeAcrossReservoir()
{	//  Only used in GetCalculatedPumpDepthInFeet

	// [7B.Reservoir Hydraulics].G70
	if (mo_geo_in.me_pc == ENTER_PC) return mo_geo_in.md_ReservoirDeltaPressure * flowRatePerWell() / 1000.0;
	double md_PressureChangeAcrossReservoir = 0.0;

	// if user didn't input the pressure change, we have to calculate it.  start with these

	// Why does GETEM calculate the average water temperature for EGS two different ways?  Is one better?  Method 2 is certainly simpler.
	double dEGSAverageWaterTemperatureC1 = geothermal::calcEGSAverageWaterTemperatureC(GetResourceTemperatureC(), mo_geo_in.md_TemperaturePlantDesignC, GetPlantBrineEffectiveness() / EGSAvailableEnergy()); // degrees C (used in EGS makeup, and on [7C.EGS Subsrfce HX]
	//double EGSAverageWaterTemperatureC1 = KelvinToCelcius(CelciusToKelvin(GetResourceTemperatureC()) * calcEGSTemperatureConstant( (GetPlantBrineEffectiveness() / EGSAvailableEnergy()), md_TemperaturePlantDesignC)); // other equation used in GETEM	

	// all this is from [7C.EGS Subsrfce HX]
	double waterTempC = (geothermal::IMITATE_GETEM) ? dEGSAverageWaterTemperatureC1 : EGSAverageWaterTemperatureC2(); // degrees C
	double days = geothermal::EGS_TIME_INPUT * geothermal::DAYS_PER_YEAR;
	double tempEGSProductionC = GetResourceTemperatureC() + (geothermal::TEMPERATURE_EGS_INJECTIONC - GetResourceTemperatureC()) * EGSReservoirConstant(waterTempC, days);
	double dEGSAverageReservoirTemperatureF = physics::CelciusToFarenheit((geothermal::TEMPERATURE_EGS_INJECTIONC + tempEGSProductionC) / 2);  //[7C.EGS Subsrfce HX].D52, [7B.Reservoir Hydraulics].D24

	mp_geo_out->md_AverageReservoirTemperatureF = (mo_geo_in.me_rt == EGS) ? dEGSAverageReservoirTemperatureF : physics::CelciusToFarenheit(GetResourceTemperatureC());	// G54 on [7B.Reservoir Hydraulics]

	double density = geothermal::oDensityConstants.evaluate(mp_geo_out->md_AverageReservoirTemperatureF); // lbs per ft^3
	double volumetricFlow = (flowRatePerWell() / density) / 3600; // ft^3 per second
	double viscosity = 0.115631 * pow(mp_geo_out->md_AverageReservoirTemperatureF, -1.199532); // lb per ft-second

	if ((mo_geo_in.me_rt == EGS) && (mo_geo_in.me_pc == SIMPLE_FRACTURE))
	{	// only a valid option for EGS resources
		// calculate the pressure change across the reservoir using simple fracture flow
		double dEGSFractureLengthUserAdjusted = EGSFractureLength() * geothermal::FRACTURE_LENGTH_ADJUSTMENT;
		double effectiveLengthFt = geothermal::MetersToFeet(dEGSFractureLengthUserAdjusted);
		double fractureFlowArea = geothermal::MetersToFeet(mo_geo_in.md_EGSFractureAperature) * geothermal::MetersToFeet(mo_geo_in.md_EGSFractureWidthM);  // ft^2
		double hydraulicDiameter = (2 * fractureFlowArea) / (geothermal::MetersToFeet(mo_geo_in.md_EGSFractureAperature) + geothermal::MetersToFeet(mo_geo_in.md_EGSFractureWidthM));  // ft
		double flowPerFracture = volumetricFlow / mo_geo_in.md_EGSNumberOfFractures; // ft^3 per second
		double velocity = flowPerFracture / fractureFlowArea; // ft per second
		double Re = density * velocity * hydraulicDiameter / viscosity;
		double frictionFactor = 64 / Re;
		double headLoss = frictionFactor * (effectiveLengthFt / hydraulicDiameter) * pow(velocity, 2) / (2 * physics::GRAVITY_FTS2); // ft
		md_PressureChangeAcrossReservoir = headLoss * density / 144; // psi
	}
	else {
		// calculate the change in pressure across reservoir using K*A (from [7B.Reservoir Hydraulics].G70)
		double dReservoirAreaSqFt = geothermal::MetersToFeet(mo_geo_in.md_ReservoirHeightM) * geothermal::MetersToFeet(mo_geo_in.md_ReservoirWidthM);
		double G53 = geothermal::M2ToFeet2(mo_geo_in.md_ReservoirPermeability * dReservoirAreaSqFt *  0.000000000000986923); //ft^4
		double G61 = volumetricFlow * viscosity * geothermal::MetersToFeet(mo_geo_in.md_DistanceBetweenProductionInjectionWellsM) / G53; // lbs per second^2-ft
		md_PressureChangeAcrossReservoir = G61 / physics::GRAVITY_FTS2 / 144; // change in pressure (psi)
	}
	return md_PressureChangeAcrossReservoir;
}

double CGeothermalAnalyzer::pressureInjectionWellBottomHolePSI() // [7B.Reservoir Hydraulics].G72, [7A.GF Pumps].G50
{
	//double injectionWellSurfacePressurePSI = (mo_geo_in.me_ct == FLASH) ? 0 : (pressureWellHeadPSI() - geothermal::PRESSURE_CHANGE_ACROSS_SURFACE_EQUIPMENT_PSI); // [2B.Resource&Well Input].D149
	double injectionWellSurfacePressurePSI = (mo_geo_in.me_ct == FLASH) ? 0 : (pressureWellHeadPSI() - mo_geo_in.md_PressureChangeAcrossSurfaceEquipmentPSI); // [2B.Resource&Well Input].D149
	// this used to incorrectly convert PSI to bar - does this still work for Binary?????????????????????
	double pMax = (pZero() > injectionWellSurfacePressurePSI) ? pZero() : injectionWellSurfacePressurePSI; //G18
	double depthFt = geothermal::MetersToFeet(GetResourceDepthM()); //G22
	double G23 = pMax + InjectionDensity() * depthFt / 144; // psi

	double flowRate = mo_geo_in.md_ProductionFlowRateKgPerS / mo_geo_in.md_RatioInjectionToProduction / (1 - geothermal::WATER_LOSS_PERCENT); // kg per second
	flowRate = geothermal::KgToLb(flowRate) / InjectionDensity();  // cf per second
	double dDiameterInjectionWellFt = mo_geo_in.md_DiameterInjectionWellInches / 12;
	double areaInjectionWell = physics::areaCircle(dDiameterInjectionWellFt / 2); // ft^2
	double velocityInjectionWell = flowRate / areaInjectionWell;

	double viscosity = 0.0925 * pow(InjectionTemperatureF(), -1.159);
	double ReInjectionWell = dDiameterInjectionWellFt * velocityInjectionWell * InjectionDensity() / viscosity;

	double frictionHeadLossInjectionWell = (geothermal::FrictionFactor(ReInjectionWell) * depthFt / dDiameterInjectionWellFt)* pow(velocityInjectionWell, 2) / (2 * physics::GRAVITY_FTS2); //feet
	double G36 = frictionHeadLossInjectionWell * InjectionDensity() / 144; // conversion to psi

	return G23 - G36; // pressureBHInjection, psi
}

double CGeothermalAnalyzer::pressureWellHeadPSI()
{
	double tempF = physics::CelciusToFarenheit(GetTemperaturePlantDesignC());
	double pressureSaturation = geothermal::oPC.evaluate(tempF); // valid above boiling, I guess.
	//double pressureExcessPSI = geothermal::BarToPsi(geothermal::EXCESS_PRESSURE_BAR); // bar to psi
	double pressureExcessPSI = geothermal::BarToPsi(mo_geo_in.md_ExcessPressureBar); // bar to psi
	//return (GetTemperaturePlantDesignC() > 100) ? pressureSaturation + pressureExcessPSI : geothermal::PRESSURE_AMBIENT_PSI + pressureExcessPSI;
	return (GetTemperaturePlantDesignC() > 100) ? pressureSaturation + pressureExcessPSI : mo_geo_in.md_PressureAmbientPSI + pressureExcessPSI;
}

double CGeothermalAnalyzer::pressureHydrostaticPSI()
{	// calculate the hydrostatic pressure (at the bottom of the well)
	double tempAmbientF = (geothermal::IMITATE_GETEM) ? physics::CelciusToFarenheit(geothermal::TEMPERATURE_EGS_AMBIENT_C) : physics::CelciusToFarenheit(GetAmbientTemperatureC());
	double pressureAmbientBar = physics::PsiToBar(geothermal::oPressureAmbientConstants.evaluate(tempAmbientF));

	double tempF = (geothermal::IMITATE_GETEM) ? physics::CelciusToFarenheit(geothermal::TEMPERATURE_EGS_AMBIENT_C) : physics::CelciusToFarenheit(GetAmbientTemperatureC());
	double densityAmbient = geothermal::LbPerCfToKgPerM3_B(geothermal::oDensityConstants.evaluate(tempF));

	double tempAmbientC = (geothermal::IMITATE_GETEM) ? 10 : GetAmbientTemperatureC(); // GETEM assumes 10 deg C ambient temperature here. Above, the assumption is 15 deg C ambient.
	double tempGradient = (mo_geo_in.me_rt == EGS) ? GetTemperatureGradient() / 1000 : (GetResourceTemperatureC() - tempAmbientC) / GetResourceDepthM();

	// hydrostatic pressure at production well depth (GetResourceDepthFt) in bar
	double d1 = densityAmbient * geothermal::GRAVITY_MS2 * geothermal::CONST_CP;
	double d2 = (exp(d1 * (GetResourceDepthM() - (0.5 * geothermal::CONST_CT * tempGradient * pow(GetResourceDepthM(), 2)))) - 1);
	double pressureHydrostaticBar = pressureAmbientBar + (1 / geothermal::CONST_CP) * (d2) / 100000;

	return geothermal::BarToPsi(pressureHydrostaticBar);
}

double CGeothermalAnalyzer::pZero(void) { return geothermal::oPC.evaluate(InjectionTemperatureF()); }	// G16 - psi


// wells
double CGeothermalAnalyzer::productionTempF(void) { return physics::CelciusToFarenheit(GetTemperaturePlantDesignC()); }
double CGeothermalAnalyzer::productionDensity(void) { return 1 / geothermal::oSVC.evaluate(productionTempF()); } // [7A.GF Pumps].G81; specific volume, f^3 per lb
double CGeothermalAnalyzer::productionFlowRate(void) { return (flowRatePerWell() / productionDensity()) / 3600; } // [7A.GF Pumps].G69 // lbs per hr / lbs per cf = cf/hr divided by 3600 = cfs
double CGeothermalAnalyzer::productionViscosity(void) { return 0.115631 * pow(productionTempF(), -1.199532); } // seems like this is resource temp in spreadsheet!
double CGeothermalAnalyzer::flowRatePerWell(void) { return (60 * 60 * geothermal::KgToLb(mo_geo_in.md_ProductionFlowRateKgPerS)); } // lbs per hour, one well
double CGeothermalAnalyzer::flowRateTotal(void) {
	mp_geo_out->GF_flowrate = (flowRatePerWell() * GetNumberOfWells());
	return (flowRatePerWell() * GetNumberOfWells());
}								// lbs per hour, all wells

double CGeothermalAnalyzer::GetNumberOfWells(void)
{
	if (mo_geo_in.me_cb == NUMBER_OF_WELLS)
		mp_geo_out->md_NumberOfWells = mo_geo_in.md_NumberOfWells;
	else
	{
		double netBrineEffectiveness = GetPlantBrineEffectiveness() - GetPumpWorkWattHrPerLb();
		double netCapacityPerWell = flowRatePerWell() * netBrineEffectiveness / 1000.0;			// after pumping losses

		if (netCapacityPerWell == 0)
		{
			ms_ErrorString = "The well capacity was calculated to be zero.  Could not continue analysis.";
			mp_geo_out->md_NumberOfWells = 0;
		}
		mp_geo_out->md_NumberOfWells = mo_geo_in.md_DesiredSalesCapacityKW / netCapacityPerWell;
	}

	return mp_geo_out->md_NumberOfWells;
}

double CGeothermalAnalyzer::GetPlantBrineEffectiveness(void)
{
	/*
	double dTemperaturePlantDesignF = physics::CelciusToFarenheit(GetTemperaturePlantDesignC());
	double exitTempLowF = (0.8229 * dTemperaturePlantDesignF ) - 127.71;
	double exitTempHighF = (0.00035129 * pow(dTemperaturePlantDesignF,2)) + (0.69792956 * dTemperaturePlantDesignF) - 159.598;
	double dTemperatureGFExitF = 109.31;// (GetTemperaturePlantDesignC() < 180) ? exitTempLowF : exitTempHighF;  // degrees farenheit - exit temperature for geothermal fluid
	double dTemperatureGFExitC = physics::FarenheitToCelcius(dTemperatureGFExitF);	//physics::FarenheitToCelcius();
	double dAE_At_Exit = GetAEAtTemp(dTemperatureGFExitC); // watt-hr/lb - Calculate available energy using binary constants and plant design temp (short cut)
	*/
	double TSiO2 = -(0.0000001334837*pow(GetTemperaturePlantDesignC(), 4)) + (0.0000706584462*pow(GetTemperaturePlantDesignC(), 3)) - (0.0036294799613*pow(GetTemperaturePlantDesignC(), 2)) + (0.3672417729236*GetTemperaturePlantDesignC()) + 4.205944351495;
	double TamphSiO2 = (0.0000000000249634* pow(TSiO2, 4)) - (0.00000000425191 * pow(TSiO2, 3)) - (0.000119669*pow(TSiO2, 2)) + (0.307616*TSiO2) - 0.294394;



	//	double dTemperatureGFExitF = physics::CelciusToFarenheit(TamphSiO2); //109.31

	double dAE_At_Exit = GetAEAtTemp(TamphSiO2);


	// GETEM's "optimizer" seems to pick the max possible brine effectiveness for the default binary plant, so use this as a proxy for now



//	double dAEMaxPossible = (geothermal::IMITATE_GETEM) ? GetAEBinary() -  GetAEBinaryAtTemp(TamphSiO2) : GetAE() - dAE_At_Exit; // watt-hr/lb - [10B.GeoFluid].H54 "maximum possible available energy accounting for the available energy lost due to a silica constraint on outlet temperature"

	mp_geo_out->max_secondlaw = (1 - ((geothermal::IMITATE_GETEM) ? GetAEBinaryAtTemp(TamphSiO2) / GetAEBinary() : dAE_At_Exit / GetAE()) - 0.375);
	double dMaxBinaryBrineEffectiveness = ((geothermal::IMITATE_GETEM) ? GetAEBinary() : GetAE()) * ((GetTemperaturePlantDesignC() < 150) ? 0.14425 * exp(0.008806 * GetTemperaturePlantDesignC()) : mp_geo_out->max_secondlaw);

	return (mo_geo_in.me_ct == FLASH) ? FlashBrineEffectiveness() : dMaxBinaryBrineEffectiveness * mo_geo_in.md_PlantEfficiency;
}

double CGeothermalAnalyzer::calculateX(double enthalpyIn, double temperatureF)
{
	double enthalpyF = geothermal::GetFlashEnthalpyF(temperatureF);
	double enthalpyG = geothermal::GetFlashEnthalpyG(temperatureF);
	//mp_geo_out -> getX = (enthalpyIn - enthalpyF) / (enthalpyG - enthalpyF);
	return (enthalpyIn - enthalpyF) / (enthalpyG - enthalpyF);
}

double CGeothermalAnalyzer::enthalpyChangeTurbine(double dEnthalpyDeltaInitial, double dEnthalpyTurbineG)
{	// I65-I80, I87-I102
	double xPrime, effTurb, dEnthapyDelta, hEx;

	dEnthapyDelta = dEnthalpyDeltaInitial;
	for (int i = 0; i < 4; i++) {
		hEx = dEnthalpyTurbineG - dEnthapyDelta;
		xPrime = calculateX(hEx, temperatureCondF());
		xPrime = (xPrime > 0.95) ? 0 : 0.95 - xPrime;
		effTurb = geothermal::EFFICIENCY_TURBINE - (0.5 * xPrime);
		dEnthapyDelta = dEnthalpyDeltaInitial * effTurb;
	}
	return dEnthapyDelta;
}


// Turbine 1 - high pressure
double CGeothermalAnalyzer::turbine1dHInitial() { return calculateDH(mp_geo_out->md_PressureHPFlashPSI - geothermal::DELTA_PRESSURE_HP_FLASH_PSI); } // I65
double CGeothermalAnalyzer::turbine1TemperatureF() {
	mp_geo_out->flash_temperature = geothermal::GetFlashTemperature(mp_geo_out->md_PressureHPFlashPSI);		//Storing HP Flash Temperature (for cmod_geothermal_costs)
	return geothermal::GetFlashTemperature(mp_geo_out->md_PressureHPFlashPSI);
} // D80

//Getting HP and LP specific volume values
double CGeothermalAnalyzer::GetSpecVol(double flash_temp) { return geothermal::getSpecVol(flash_temp); }


double CGeothermalAnalyzer::turbine1EnthalpyF() { return  geothermal::GetFlashEnthalpyF(turbine1TemperatureF()); }	// D81
double CGeothermalAnalyzer::turbine1EnthalpyG() { return  geothermal::GetFlashEnthalpyG(turbine1TemperatureF()); }	// D82
double CGeothermalAnalyzer::turbine1DH() { return enthalpyChangeTurbine(turbine1dHInitial(), turbine1EnthalpyG()); } // I80 - btu/lb
double CGeothermalAnalyzer::turbine1HEx() { return turbine1EnthalpyG() - turbine1DH(); } // I81 - btu/lb
double CGeothermalAnalyzer::turbine1X()
{	// D83 - %
	mp_geo_out->spec_vol = GetSpecVol(mp_geo_out->flash_temperature);
	double enthalpyPlantDesignTemp = geothermal::GetFlashEnthalpyF(physics::CelciusToFarenheit(GetTemperaturePlantDesignC()));// D69
	mp_geo_out->getX_hp = calculateX(enthalpyPlantDesignTemp, turbine1TemperatureF());
	return calculateX(enthalpyPlantDesignTemp, turbine1TemperatureF());
}
double CGeothermalAnalyzer::turbine1Steam() { return geothermal::GEOTHERMAL_FLUID_FOR_FLASH * turbine1X(); }																										// D85 - lb/hr
double CGeothermalAnalyzer::turbine1NetSteam()
{	// I82 lb/hr
	double dForNCGRemoval = 0.0;
	if (geothermal::NCG_REMOVAL_TYPE != VAC_PUMP)
	{
		double dSteamFlow = steamFlow(1);
		if (geothermal::NUMBER_OF_COOLING_STAGES > 1) { dSteamFlow += steamFlow(2); }
		if (geothermal::NUMBER_OF_COOLING_STAGES > 2) { dSteamFlow += steamFlow(3); }
		dForNCGRemoval = dSteamFlow;
	}
	return turbine1Steam() - dForNCGRemoval;
}
double CGeothermalAnalyzer::turbine1OutputKWh() { return turbine1DH() * turbine1NetSteam() / 3413; }																				// I83 - kW/hr = (btu/lb) * (lb/hr) / (btu/kW)

// Flash Turbine 2 - low pressure
double CGeothermalAnalyzer::turbine2dHInitial(void) { return calculateDH(mp_geo_out->md_PressureLPFlashPSI - geothermal::DELTA_PRESSURE_LP_FLASH_PSI); }														// I87
double CGeothermalAnalyzer::turbine2TemperatureF(void) {
	mp_geo_out->flash_temperature_lp = geothermal::GetFlashTemperature(mp_geo_out->md_PressureLPFlashPSI);	//Getting LP Flash Temperature value for calculations in cmod_geothermal_costs
	mp_geo_out->spec_vol_lp = GetSpecVol(mp_geo_out->flash_temperature_lp);
	return geothermal::GetFlashTemperature(mp_geo_out->md_PressureLPFlashPSI);
}														// D88
double CGeothermalAnalyzer::turbine2EnthalpyF(void) { return geothermal::GetFlashEnthalpyF(turbine2TemperatureF()); }															// D89
double CGeothermalAnalyzer::turbine2EnthalpyG(void) { return geothermal::GetFlashEnthalpyG(turbine2TemperatureF()); }															// D90
double CGeothermalAnalyzer::turbine2DH(void) { return enthalpyChangeTurbine(turbine2dHInitial(), turbine2EnthalpyG()); }																// I102 - btu/lb
double CGeothermalAnalyzer::turbine2HEx(void) { return turbine2EnthalpyG() - turbine2DH(); }																							// I103 - btu/lb
double CGeothermalAnalyzer::turbine2X(void) {
	mp_geo_out->getX_lp = calculateX(turbine1EnthalpyF(), turbine2TemperatureF());
	return calculateX(turbine1EnthalpyF(), turbine2TemperatureF());
}																		// D91 %
double CGeothermalAnalyzer::turbine2Steam(void) { return (FlashCount() == 2) ? geothermal::GEOTHERMAL_FLUID_FOR_FLASH * turbine2X() * (1 - turbine1X()) : 0; }																						// I104, D93 - lb/hr
double CGeothermalAnalyzer::turbine2OutputKWh(void) { return turbine2DH() * turbine2Steam() / 3413; }																				// I105 - kW/hr


// NCG Removal
double CGeothermalAnalyzer::pInter(int stage)
{	// D156, D205, D253 - psi
	switch (stage)
	{
	case 0: return pTotal();
	case 1: return pTotal() * pRatio();
		//case 2: return (geothermal::NUMBER_OF_COOLING_STAGES > 2) ? pTotal() * pRatio() * pRatio()  : geothermal::PRESSURE_AMBIENT_PSI;
	case 2: return (geothermal::NUMBER_OF_COOLING_STAGES > 2) ? pTotal() * pRatio() * pRatio() : mo_geo_in.md_PressureAmbientPSI;
		//case 3: return geothermal::PRESSURE_AMBIENT_PSI;
	case 3: return mo_geo_in.md_PressureAmbientPSI;
	default: { ms_ErrorString = ("Invalid stage in CGeothermalAnalyzer::pInter"); return 0; }
	}
}
double CGeothermalAnalyzer::pTotal(void) { return (geothermal::IMITATE_GETEM) ? pressureSaturation() + (geothermal::PRESSURE_CONDENSER_NCG_PARTIAL_INHG * 0.49) : pressureCondenser(); } // calculated separately in spreadsheet, but mathematically equivalent to pressureCondenser					   D150,D74 - psi
//double CGeothermalAnalyzer::pRatio(void) { return exp(log(geothermal::PRESSURE_AMBIENT_PSI / (pTotal()))/geothermal::NUMBER_OF_COOLING_STAGES); }
double CGeothermalAnalyzer::pRatio(void) { return exp(log(mo_geo_in.md_PressureAmbientPSI / (pTotal())) / geothermal::NUMBER_OF_COOLING_STAGES); }
double CGeothermalAnalyzer::ncgFlowLbsPerHour(void) { return geothermal::GEOTHERMAL_FLUID_FOR_FLASH * geothermal::NCG_LEVEL_PPM / 1000000; }
double CGeothermalAnalyzer::ncgFlowMolesPerHour(void) { return ncgFlowLbsPerHour() / geothermal::MOLE_WEIGHT_NCG; }
double CGeothermalAnalyzer::pSuction(int stage) { return pTotal() * pow(pRatio(), stage - 1); }
double CGeothermalAnalyzer::prJet(int stage) { return pInter(stage) / pInter(stage - 1); }
double CGeothermalAnalyzer::h2oMolesPerHour(int st) { return ncgFlowMolesPerHour() / ((pSuction(st) / pressureSaturation()) - 1); }
double CGeothermalAnalyzer::totalVentFlow(int st) { return ncgFlowLbsPerHour() + (h2oMolesPerHour(st) * geothermal::MOLE_WEIGHT_H2O); }
double CGeothermalAnalyzer::moleWeightVent(int st) { return totalVentFlow(st) / (ncgFlowMolesPerHour() + h2oMolesPerHour(st)); }
double CGeothermalAnalyzer::suctionSteamRatio(int st) {
	mp_geo_out->pressure_ratio_1 = pSuction(1) / mp_geo_out->md_PressureHPFlashPSI;
	//mp_geo_out-> pressure_ratio_2 = pSuction(2) / mp_geo_out->md_PressureHPFlashPSI;
	//mp_geo_out-> pressure_ratio_3 = pSuction(3) / mp_geo_out->md_PressureHPFlashPSI;
	return pSuction(st) / mp_geo_out->md_PressureHPFlashPSI;
}
double CGeothermalAnalyzer::AR(int stage) { return ((3.5879 * pow(prJet(stage), -2.1168)) + 0.1) * pow(suctionSteamRatio(stage), (-1.155 * pow(prJet(stage), -0.0453))); }
double CGeothermalAnalyzer::ERd(int stage) { return (1.0035 * AR(stage) + 8.9374)* pow(suctionSteamRatio(stage), (2.9594* pow(AR(stage), -0.8458) + 0.99)); }
double CGeothermalAnalyzer::ER(int st) { return ERd(st) * pow((((460 + geothermal::GetFlashTemperature(mp_geo_out->md_PressureHPFlashPSI)) * moleWeightVent(st)) / ((temperatureCondF() + 460) * geothermal::MOLE_WEIGHT_H2O)), 0.5); }
double CGeothermalAnalyzer::steamFlow(int st) { return (st >= 3 && (geothermal::NCG_REMOVAL_TYPE != JET || geothermal::NUMBER_OF_COOLING_STAGES < 3)) ? 0 : totalVentFlow(st) / ER(st); }

int CGeothermalAnalyzer::FlashCount(void) {
	mp_geo_out->flash_count = (mo_geo_in.me_ft >= DUAL_FLASH_NO_TEMP_CONSTRAINT) ? 2 : 1;
	return (mo_geo_in.me_ft >= DUAL_FLASH_NO_TEMP_CONSTRAINT) ? 2 : 1;
}

double CGeothermalAnalyzer::calculateDH(double pressureIn)
{
	double a = geothermal::GetDHa(pressureIn);
	double b = geothermal::GetDHb(pressureIn);
	double x = pressureIn / (pressureCondenser());
	return a * log(x) + b;
}

double CGeothermalAnalyzer::TemperatureWetBulbF(void) { return physics::CelciusToFarenheit(mo_geo_in.md_TemperatureWetBulbC); }
double CGeothermalAnalyzer::temperatureCondF(void)
{	// D71 - deg F
	return (TemperatureWetBulbF() + geothermal::DELTA_TEMPERATURE_CWF + geothermal::TEMPERATURE_PINCH_PT_CONDENSER_F + geothermal::TEMPERATURE_PINCH_PT_COOLING_TOWER_F);
}
double CGeothermalAnalyzer::pressureSaturation(void) { return geothermal::oPSatConstants.evaluate(temperatureCondF()); }// D72 - psi
double CGeothermalAnalyzer::pressureCondenser(void)
{// D74 - psi
	return pressureSaturation() + geothermal::InHgToPsi(geothermal::PRESSURE_CONDENSER_NCG_PARTIAL_INHG);
}

double CGeothermalAnalyzer::FlashBrineEffectiveness(void)
{
	if (!mp_geo_out->mb_BrineEffectivenessCalculated) {
		calculateFlashPressures();

		double dGrossOutput = turbine1OutputKWh();
		if (FlashCount() == 2) dGrossOutput += turbine2OutputKWh();
		double dGrossPower = dGrossOutput * geothermal::EFFICIENCY_GENERATOR;
		//double deltaPressureCondenserFt = (geothermal::CONDENSER_TYPE == SURFACE) ? geothermal::ADDITIONAL_CW_PUMP_HEAD_SURFACE : physics::PSItoFT(geothermal::PRESSURE_AMBIENT_PSI + 1 - (pressureCondenser())); // O102
		double deltaPressureCondenserFt = (geothermal::CONDENSER_TYPE == SURFACE) ? geothermal::ADDITIONAL_CW_PUMP_HEAD_SURFACE : physics::PSItoFT(mo_geo_in.md_PressureAmbientPSI + 1 - (pressureCondenser())); // O102
		double cwPumpHead = geothermal::BASE_CW_PUMP_HEAD_FT + deltaPressureCondenserFt; // D110 - ft
		mp_geo_out->cw_pump_head = cwPumpHead;


		double mainCWPumpPowerKW = CalculatePumpWorkInKW(cwFlow(), cwPumpHead);	// part of I105
		double dTotalPumpingKW = mainCWPumpPowerKW + cwPumpWorkKW();


		double dParasiticPower = dTotalPumpingKW + condensatePumpingKW() + fanPowerKW() + vacuumPumpingKW() + condenserInjectionPumpingKW();
		mp_geo_out->md_FlashBrineEffectiveness = dGrossPower - dParasiticPower;
		mp_geo_out->mb_BrineEffectivenessCalculated = true;
	}
	return mp_geo_out->md_FlashBrineEffectiveness;
}

void CGeothermalAnalyzer::calculateFlashPressures(void)
{	// This function Sets some values that will be used throughout the calculations of the flash brine effectiveness
	// These cannot be Set during initialization since some of the public properties may have been changed.  These
	// need to be calculated right when the brine effectiveness is calculated.

	if (mp_geo_out->mb_FlashPressuresCalculated) return;

	// if single flash - add flash pressure to delta pressure and quit
	if (FlashCount() == 1)
	{
		mp_geo_out->md_PressureHPFlashPSI = pressureSingle() + geothermal::DELTA_PRESSURE_HP_FLASH_PSI;
		return;
	}

	// dual flash, have to calculate both
	// high pressure flash
//i think this might be using the wrong temperature - resource instead of plant design - for EGS
	mp_geo_out->md_PressureHPFlashPSI = pressureDualHigh() + geothermal::DELTA_PRESSURE_HP_FLASH_PSI;


	// low pressure flash
	mp_geo_out->md_PressureLPFlashPSI = pressureDualLow() + geothermal::DELTA_PRESSURE_LP_FLASH_PSI;
	mp_geo_out->mb_FlashPressuresCalculated = true;
}

// Pump Power
double CGeothermalAnalyzer::overAllSteam(void) { return (this->FlashCount() == 2) ? turbine1NetSteam() + turbine2Steam() : turbine1NetSteam(); } // D96
double CGeothermalAnalyzer::qCondenser(void) {
	mp_geo_out->condenser_q = overAllSteam() * (overAllHEx() - geothermal::GetFlashEnthalpyF(temperatureCondF()));
	return overAllSteam() * (overAllHEx() - geothermal::GetFlashEnthalpyF(temperatureCondF()));
} // D99
double CGeothermalAnalyzer::cwFlow(void) {
	mp_geo_out->cwflow = qCondenser() / geothermal::DELTA_TEMPERATURE_CWF;
	return qCondenser() / geothermal::DELTA_TEMPERATURE_CWF;
} // D115
double CGeothermalAnalyzer::overAllHEx() //I107
{
	return (FlashCount() == 2) ? ((turbine2HEx() * turbine2Steam()) + (turbine1HEx() * turbine1NetSteam())) / (turbine1NetSteam() + turbine2Steam()) : turbine1HEx();
}

// CW Pump Power
double CGeothermalAnalyzer::h2oVentFlow(int stage) { return h2oMolesPerHour(stage) * geothermal::MOLE_WEIGHT_H2O; } // D160 - lb/hr
double CGeothermalAnalyzer::moleRatio(int st) { return (pInter(st) / pressureSaturation()) - 1; } // D184,
double CGeothermalAnalyzer::flowSteamMolesPerHr(int st) { return ncgFlowMolesPerHour() / moleRatio(st); } // D186,
double CGeothermalAnalyzer::flowSteamLbPerHr(int st) { return flowSteamMolesPerHr(st) * geothermal::MOLE_WEIGHT_H2O; } // D187,  - lb/hr
double CGeothermalAnalyzer::condensedSteamLbPerHour(int stage) { return steamFlow(stage) + h2oVentFlow(stage) - flowSteamLbPerHr(stage); } // D188 = D171+D160-D187 = stage1CondensedSteam (lb/hr)
double CGeothermalAnalyzer::pumpWorkFromSteamFlow(double flow)
{// D189-D198,
	double enthalpyCondF = geothermal::GetFlashEnthalpyF(temperatureCondF());
	double enthalpyCondG = geothermal::GetFlashEnthalpyG(temperatureCondF());

	double qReject = flow * (enthalpyCondG - enthalpyCondF);
	double cwFlow = qReject / geothermal::DELTA_TEMPERATURE_CWF;
	double pumpHead = geothermal::BASE_CW_PUMP_HEAD_FT + geothermal::ADDITIONAL_CW_PUMP_HEAD_SURFACE;
	return CalculatePumpWorkInKW(cwFlow, pumpHead);
}
double CGeothermalAnalyzer::cwPumpWorkKWByStage(int st) { return pumpWorkFromSteamFlow(condensedSteamLbPerHour(st)); } // D203- kW
double CGeothermalAnalyzer::cwPumpWorkKW(void) {
	mp_geo_out->cw_pump_work = cwPumpWorkKWByStage(1) + cwPumpWorkKWByStage(2) + cwPumpWorkKWByStage(3);
	return cwPumpWorkKWByStage(1) + cwPumpWorkKWByStage(2) + cwPumpWorkKWByStage(3);
} // D305 - kW, part of L105


// Condensate Pump Power
//double CGeothermalAnalyzer::condensatePumpHead(void) { return geothermal::PSItoFTB(geothermal::PRESSURE_AMBIENT_PSI + 1 - pressureCondenser()) + geothermal::BASE_CW_PUMP_HEAD_FT; } // D121
double CGeothermalAnalyzer::condensatePumpHead(void) { return geothermal::PSItoFTB(mo_geo_in.md_PressureAmbientPSI + 1 - pressureCondenser()) + geothermal::BASE_CW_PUMP_HEAD_FT; } // D121
double CGeothermalAnalyzer::condensatePumpPowerKW(void) {
	mp_geo_out->condensate_pump_power = CalculatePumpWorkInKW(overAllSteam(), condensatePumpHead());
	return CalculatePumpWorkInKW(overAllSteam(), condensatePumpHead());
} // D126->kw, part of I106
//double CGeothermalAnalyzer::condensatePumpHeadByStage(int st) { return geothermal::PSItoFTB(geothermal::PRESSURE_AMBIENT_PSI + 1 - pInter(st)); } // D201, D249, D297
double CGeothermalAnalyzer::condensatePumpHeadByStage(int st) { return geothermal::PSItoFTB(mo_geo_in.md_PressureAmbientPSI + 1 - pInter(st)); } // D201, D249, D297
double CGeothermalAnalyzer::condensatePumpWorkByStage(int st) { return CalculatePumpWorkInKW(condensedSteamLbPerHour(st), condensatePumpHeadByStage(st)); } // D207, ... kW
double CGeothermalAnalyzer::totalCondensatePumpWorkKW(void) {
	mp_geo_out->ncg_condensate_pump = condensatePumpWorkByStage(1) + condensatePumpWorkByStage(2) + condensatePumpWorkByStage(3); //D310
	return condensatePumpWorkByStage(1) + condensatePumpWorkByStage(2) + condensatePumpWorkByStage(3);
} // D310 - kW
double CGeothermalAnalyzer::condensatePumpingKW(void) { return condensatePumpPowerKW() + totalCondensatePumpWorkKW(); } // I106 - kW

// Fan Power
double CGeothermalAnalyzer::qRejectByStage(int stage) { return condensedSteamLbPerHour(stage) * (geothermal::GetFlashEnthalpyG(temperatureCondF()) - geothermal::GetFlashEnthalpyF(temperatureCondF())); } // D190
double CGeothermalAnalyzer::qRejectTotal(void) {
	mp_geo_out->qRejectByStage_1 = qRejectByStage(1);
	mp_geo_out->qRejectByStage_2 = qRejectByStage(2);
	mp_geo_out->qRejectByStage_3 = qRejectByStage(3);

	//mp_geo_out->qRejectedTotal = qRejectByStage(1) + qRejectByStage(2) + qRejectByStage(3);
	return qRejectByStage(1) + qRejectByStage(2) + qRejectByStage(3);
} // D303
double CGeothermalAnalyzer::qRejectedTower(void) {
	mp_geo_out->qRejectedTotal = qCondenser() + qRejectTotal();
	return qCondenser() + qRejectTotal();
} // D102
double CGeothermalAnalyzer::fanPowerCoeffA(void) { return -2.0814 * log(geothermal::DELTA_TEMPERATURE_CWF) + 10.6013; } // O95
double CGeothermalAnalyzer::fanPowerCoeffB(void) { return -0.0188 * pow(geothermal::DELTA_TEMPERATURE_CWF, 0.0232); } // P95
double CGeothermalAnalyzer::fanPower(void) { return fanPowerCoeffA() * exp(this->TemperatureWetBulbF() * fanPowerCoeffB()); } // D103 - hp per MMBtu/hr
double CGeothermalAnalyzer::fanPowerKW(void) { return geothermal::HPtoKW(fanPower() * qRejectedTower() / 1000000); } // D105, I118


double CGeothermalAnalyzer::deltaPressureByStage(int st) { return pInter(st) - pSuction(st); } // D173, D222, D270 - psi
double CGeothermalAnalyzer::densityForVacuumPump(int st) { return pSuction(st) * moleWeightVent(st) / ((temperatureCondF() + 460)*10.7316); }// D166, D215, D263 - lb/ft^3
double CGeothermalAnalyzer::vaccumPumpHead(int st) { return deltaPressureByStage(st) * 144 / densityForVacuumPump(st); }	// D175, D224, D272 - ft
double CGeothermalAnalyzer::vacuumPumpWorkByStage(int st)
{ // D182, D231, D279 - kW
	return (geothermal::NCG_REMOVAL_TYPE == VAC_PUMP || (st == 3 && geothermal::NCG_REMOVAL_TYPE == HYBRID)) ? CalculatePumpWorkInKW(totalVentFlow(st), vaccumPumpHead(st)) : 0;
}
double CGeothermalAnalyzer::vacuumPumpingKW(void) {
	mp_geo_out->v_stage_1 = vacuumPumpWorkByStage(1);
	mp_geo_out->v_stage_2 = vacuumPumpWorkByStage(2);
	mp_geo_out->v_stage_3 = vacuumPumpWorkByStage(3);
	mp_geo_out->pressure_ratio_2 = pInter(1) / mp_geo_out->md_PressureHPFlashPSI;				//pressure ratios used in ncg ejector cost calculation
	mp_geo_out->pressure_ratio_3 = pInter(2) / mp_geo_out->md_PressureHPFlashPSI;
	return vacuumPumpWorkByStage(1) + vacuumPumpWorkByStage(2) + vacuumPumpWorkByStage(3);
}	// D311, I108

// Condenser Injection Pump Power
double CGeothermalAnalyzer::injectionDeltaP(void)
{	// D127 - psi (condensate injection delta pressure)
	//return (FlashCount() == 1) ? mp_geo_out->md_PressureHPFlashPSI - geothermal::PRESSURE_AMBIENT_PSI : mp_geo_out->md_PressureLPFlashPSI - geothermal::PRESSURE_AMBIENT_PSI; 
	return (FlashCount() == 1) ? mp_geo_out->md_PressureHPFlashPSI - mo_geo_in.md_PressureAmbientPSI : mp_geo_out->md_PressureLPFlashPSI - mo_geo_in.md_PressureAmbientPSI;
}
double CGeothermalAnalyzer::injectionPumpHead(void) { return physics::PSItoFT(injectionDeltaP()); } // D128 - ft
double CGeothermalAnalyzer::injCoeffA(void) { return -0.0001769 * log(geothermal::DELTA_TEMPERATURE_CWF) + 0.0011083; }// R95
double CGeothermalAnalyzer::injCoeffB(void) { return  0.0657628 * log(geothermal::DELTA_TEMPERATURE_CWF) - 0.4091309; }	// S95
double CGeothermalAnalyzer::injCoeffC(void) { return -6.7041142 * log(geothermal::DELTA_TEMPERATURE_CWF) + 44.3438937; }	// T95
double CGeothermalAnalyzer::injCoeffD(void) { return -0.0325112 * pow(geothermal::DELTA_TEMPERATURE_CWF, 2) + (6.831236 * geothermal::DELTA_TEMPERATURE_CWF) - 64.6250943; }	// U95

double CGeothermalAnalyzer::evaporativeWaterLoss(void) { return ((injCoeffA() * pow(TemperatureWetBulbF(), 3)) + (injCoeffB() * pow(TemperatureWetBulbF(), 2)) + (injCoeffC() * TemperatureWetBulbF()) + injCoeffD()) * qRejectedTower() / 1000000; } // D129 - lb/hr (evaporative water loss)
double CGeothermalAnalyzer::drift(void) { return 0.0005 * cwFlow(); }																												// D130
double CGeothermalAnalyzer::blowDown(void) { return evaporativeWaterLoss() / (geothermal::INJECTION_PUMPING_CYCLES - 1) - drift(); }																	// D132
double CGeothermalAnalyzer::waterLoss(void) { return evaporativeWaterLoss() + drift() + blowDown(); }																				// D133
double CGeothermalAnalyzer::steamCondensate(void) { return (turbine1Steam() + turbine2Steam()) - waterLoss(); }																		// D135
double CGeothermalAnalyzer::steamCondensateInjected(void) { return (steamCondensate() < 0) ? 0 : steamCondensate(); }																// D136 - lb/hr
double CGeothermalAnalyzer::condenserInjectionPumpingKW() { return CalculatePumpWorkInKW(steamCondensateInjected(), injectionPumpHead()); }													// D138, I120 - kW


// Flash Pressures 
bool CGeothermalAnalyzer::TempConstraint(void)
{
	return ((mo_geo_in.me_ft == DUAL_FLASH_WITH_TEMP_CONSTRAINT) || (mo_geo_in.me_ft == SINGLE_FLASH_WITH_TEMP_CONSTRAINT));
}
double CGeothermalAnalyzer::tempFlashLimitF(void)
{	// D26 - deg F
	return physics::CelciusToFarenheit(geothermal::oFlashTempConstants.evaluate(GetResourceTemperatureC()));
}
double CGeothermalAnalyzer::pressureFlashAmorphousSilica(void) { return geothermal::oPC.evaluate(tempFlashLimitF()); } // D27 - psi
double CGeothermalAnalyzer::pressureSingleNoConstraint()
{ // Q64
	return (0.0207 * temperatureCondF() - 0.8416) * exp(0.0334*pow(temperatureCondF(), -0.1732) * GetTemperaturePlantDesignC());
}
double CGeothermalAnalyzer::pressureSingleWithConstraint()
{ // S64
	return (pressureSingleNoConstraint() < pressureFlashAmorphousSilica()) ? pressureFlashAmorphousSilica() : pressureSingleNoConstraint();
}
double CGeothermalAnalyzer::pressureSingleToTest(void)
{	// Q64 or S64
	return (TempConstraint()) ? pressureSingleWithConstraint() : pressureSingleNoConstraint();
}
double CGeothermalAnalyzer::pressureSingle(void)
{	// O64
	//return (pressureSingleToTest() < geothermal::PRESSURE_AMBIENT_PSI) ? geothermal::PRESSURE_AMBIENT_PSI : pressureSingleToTest(); 
	return (pressureSingleToTest() < mo_geo_in.md_PressureAmbientPSI) ? mo_geo_in.md_PressureAmbientPSI : pressureSingleToTest();
}
double CGeothermalAnalyzer::pressureDualHighNoConstraint()
{	// R64
	return geothermal::oFlashConstants1.evaluate(temperatureCondF()) * exp(geothermal::oFlashConstants2.evaluate(temperatureCondF()) * GetTemperaturePlantDesignC());
}
double CGeothermalAnalyzer::pressureDualHighWithConstraint()
{
	double a = (temperatureCondF() > 125) ? 1.59 + (0.0015547 * exp(0.0354727*temperatureCondF())) : 1.59 + (0.098693 * exp(0.0025283*temperatureCondF()));
	double b = (temperatureCondF() > 125) ? 0.01916 - (0.000005307 * exp(0.031279921*temperatureCondF())) : 0.01916 - (0.000167123 * exp(0.00400728*temperatureCondF()));
	return a * exp(b * GetTemperaturePlantDesignC());
}
double CGeothermalAnalyzer::pressureDualHigh(void)
{	// P64
	return (TempConstraint()) ? pressureDualHighWithConstraint() : pressureDualHighNoConstraint();
}
double CGeothermalAnalyzer::pressureDualLowUnconstrained()
{ // R65
	return (0.12632*exp(0.01918 * temperatureCondF())) * exp((0.0146 * exp(-0.00205*temperatureCondF()) * GetTemperaturePlantDesignC()));
}
double CGeothermalAnalyzer::pressureDualLowConstrained()
{ // T65
	return (pressureDualLowUnconstrained() < pressureFlashAmorphousSilica()) ? pressureFlashAmorphousSilica() : pressureDualLowUnconstrained();
}
double CGeothermalAnalyzer::pressureDualLowToTest(void)
{	// R65 or T65
	return (TempConstraint()) ? pressureDualLowConstrained() : pressureDualLowUnconstrained();
}
double CGeothermalAnalyzer::pressureDualLow(void)
{	// P65
	//return  (pressureDualLowToTest() < geothermal::PRESSURE_AMBIENT_PSI) ? geothermal::PRESSURE_AMBIENT_PSI : pressureDualLowToTest(); 
	return  (pressureDualLowToTest() < mo_geo_in.md_PressureAmbientPSI) ? mo_geo_in.md_PressureAmbientPSI : pressureDualLowToTest();
}



//---------------------------------------------------------------------------------------------------------------------------------------------------
bool CGeothermalAnalyzer::OpenWeatherFile(const char * fn)
{
	mb_WeatherFileOpen = false;
	ml_ReadCount = 0;
	if (!m_wFile.open(fn))
		ms_ErrorString = "Could not open the weather file: " + std::string(fn);
	else
		mb_WeatherFileOpen = true;

	return mb_WeatherFileOpen;
}

bool CGeothermalAnalyzer::ReadWeatherForTimeStep(bool bHourly, unsigned int timeStep)
// Read one line in weather file for hourly analysis, or calculate the average values for a month for monthly analysis
{
	// if this is an hourly analysis, just ignore the time step and get the data from the next line in the weather file
	if (bHourly) return ReadNextLineInWeatherFile();

	// Not an hourly analysis, so calculate the monthly weather info
	int month = (timeStep % 12) + 1;
	size_t hours = util::hours_in_month(month);
	if (hours == 0)
	{
		ms_ErrorString = "util::hours_in_month returned zero for month =  " + util::to_string(month) + ".";
		return false;
	}

	double pressure = 0, wetbulb = 0, drybulb = 0, rel_humidity = 0;
	for (size_t i = 0; i < hours; i++)
	{
		ReadNextLineInWeatherFile();
		pressure += m_wf.pres;
		wetbulb += m_wf.twet;
		drybulb += m_wf.tdry;
		rel_humidity += m_wf.rhum;
	}
	m_wf.pres = pressure / hours;
	m_wf.twet = wetbulb / hours;
	m_wf.tdry = drybulb / hours;
	m_wf.rhum = rel_humidity / hours;
	return true;
}


bool CGeothermalAnalyzer::ReadNextLineInWeatherFile(void)
// Private function, called from ReadWeatherForTimeStep(timeStep)
// Read the next line from weather file; rewind file if passed the end; assumes 8760 hour weather file;
{
	if (ml_ReadCount >= 8760)
	{
		m_wFile.rewind();
		ml_ReadCount = 0;
	}

	if (!m_wFile.read(&m_wf))
	{
		ms_ErrorString = "Could not read  line " + util::to_string((int)ml_ReadCount + 1) + " in the weather file.";
		return false;
	}
	ml_ReadCount++;
	ml_HourCount++;

	return true;
}


bool CGeothermalAnalyzer::determineMakeupAlgorithm()
{   // This is the logic to determine which makeup algorithm GETEM uses: Binary, Flash, or EGS
	// Just because the user chooses "EGS" from the drop-down box on the "2A.Scenario Input" sheet,
	// does NOT mean that the model will use the results from the EGS makeup sheet.
	me_makeup = NO_MAKEUP_ALGORITHM;

	if ((mo_geo_in.me_rt != HYDROTHERMAL) && (mo_geo_in.me_rt != EGS)) ms_ErrorString = "Reource type not recognized in CGeoHourlyBaseInputs::determineMakeupAlgorithm.";
	if ((mo_geo_in.me_ct != BINARY) && (mo_geo_in.me_ct != FLASH))   ms_ErrorString = "Conversion system not recognized in CGeoHourlyBaseInputs::determineMakeupAlgorithm.";
	if (ms_ErrorString != "") return false;

	if (mo_geo_in.me_tdm == ENTER_RATE)
	{ // if user has chosen to enter the temperature decline rate, then the makeup is calculated either with the binary or flash method.
		if (mo_geo_in.me_ct == BINARY)
			me_makeup = MA_BINARY;
		else if (mo_geo_in.me_rt == EGS)
			ms_ErrorString = ("GETEM algorithms are not meant to handle flash plants with EGS resources.");
		else
		{
			if ((mo_geo_in.me_ft > NO_FLASH_SUBTYPE) && (mo_geo_in.me_ft <= DUAL_FLASH_WITH_TEMP_CONSTRAINT))
				me_makeup = MA_FLASH;
			else
				ms_ErrorString = ("Conversion system Set to 'flash', but the type of flash system was not recognized in CGeoHourlyBaseInputs::determineMakeupAlgorithm");
		}
	}
	else if (mo_geo_in.me_tdm == CALCULATE_RATE)
	{	// this temperature decline can only be calculated for Binary conversion systems with EGS resources
		if ((mo_geo_in.me_rt == EGS) && (mo_geo_in.me_ct == BINARY))
			me_makeup = MA_EGS;
		else
			ms_ErrorString = ("Fluid temperature decline rate can only be calculated for an EGS resource using a binary plant");
	}
	else
		ms_ErrorString = ("Fluid temperature decline method not recognized in CGeoHourlyBaseInputs::determineMakeupAlgorithm.");

	return (me_makeup != NO_MAKEUP_ALGORITHM);
}

bool CGeothermalAnalyzer::inputErrorsForUICalculations(void)
{
	if (!ms_ErrorString.empty()) return true;
	if (GetTemperaturePlantDesignC() > GetResourceTemperatureC()) { ms_ErrorString = ("Plant design temperature cannot be greater than the resource temperature."); return true; }

	if ((mo_geo_in.me_rt != EGS) && (mo_geo_in.me_pc == SIMPLE_FRACTURE)) { ms_ErrorString = ("Reservoir pressure change based on simple fracture flow can only be calculated for EGS resources."); return true; }

	if ((mo_geo_in.me_rt != EGS) && (mo_geo_in.me_tdm == CALCULATE_RATE)) { ms_ErrorString = ("Temperature decline can only be calculated for EGS resources."); return true; }

	if ((mo_geo_in.me_tdm == ENTER_RATE) && (mo_geo_in.md_TemperatureDeclineRate < 0))
	{
		ms_ErrorString = ("Fluid temperature decline method chosen was 'enter rate', but the rate is < 0"); return true;
	}

	double dTemperatureRatio = physics::CelciusToKelvin(GetResourceTemperatureC()) / physics::CelciusToKelvin(GetTemperaturePlantDesignC()); // max valid value is MAX_TEMP_RATIO
	if ((dTemperatureRatio > geothermal::MAX_TEMP_RATIO) && (mo_geo_in.mi_ModelChoice == 0))
	{
		ms_ErrorString = ("Plant design temperature is too low for resource temperature.  GETEM equations will return invalid results."); return true;
	}

	//if ( this->netBrineEffectiveness() == 0 ) // this will cause a division by zero error
	//	{ ms_ErrorString = ("Inputs led to a divide by zero error.  Pump work = Plant output, so the net efficiency is zero."); return true; }

	//if ( this->netBrineEffectiveness() < 0 )
	//	{ ms_ErrorString = ("Inputs lead to required pump energy being greater than the plant output."); return true; }


	if (GetAEBinary() == 0)
	{
		ms_ErrorString = ("Inputs lead to available energy = zero, which will cause a division by zero error."); return true;
	}

	if (!determineMakeupAlgorithm()) return true; // determineMakeupAlgorithm sets member enum "me_makeup"

	return false;
}

bool CGeothermalAnalyzer::inputErrorsForAnalysis(void)
{	// check for errors in mo_geo_in
	if (inputErrorsForUICalculations()) return true;

	if (mo_geo_in.mi_ProjectLifeYears == 0) { ms_ErrorString = ("Project life was zero."); return true; }
	if (mo_geo_in.mi_ModelChoice < 0) { ms_ErrorString = ("The model choice was not set."); return true; }

	if (!(NumberOfReservoirs() > 0)) { ms_ErrorString = ("Resource potential must be greater than the gross plant output."); return true; }
	if (mo_pb_p.P_ref == 0) { ms_ErrorString = ("The power block parameters were not initialized."); return true; }

	if (!ms_ErrorString.empty()) return true;
	return false;
}


bool CGeothermalAnalyzer::ReadyToAnalyze()
{
	if (inputErrorsForAnalysis()) return false;

	if (!OpenWeatherFile(mo_geo_in.mc_WeatherFileName)) return false;

	if (!mp_geo_out->maf_ReplacementsByYear || !mp_geo_out->maf_monthly_resource_temp || !mp_geo_out->maf_monthly_power || !mp_geo_out->maf_monthly_energy || !mp_geo_out->maf_timestep_resource_temp ||
		!mp_geo_out->maf_timestep_power || !mp_geo_out->maf_timestep_test_values || !mp_geo_out->maf_timestep_pressure || !mp_geo_out->maf_timestep_dry_bulb || !mp_geo_out->maf_timestep_wet_bulb)

	{
		ms_ErrorString = "One of the output arrays was not initialized in the geothermal hourly model.";
		return false;
	}

	return true;
}

bool CGeothermalAnalyzer::RunAnalysis(bool(*update_function)(float, void*), void *user_data)
{
	if (!ReadyToAnalyze()) return false;   // open weather file m_wf

	if ((mo_geo_in.mi_ModelChoice != 0) && (!mo_PowerBlock.InitializeForParameters(mo_pb_p)))
	{
		ms_ErrorString = "There was an error initializing the power block with the input parameters: " + mo_PowerBlock.GetLastError();
		return false;
	}

	// ReSet all calculated values to zero
	float fPercentDone = 0.0;
	bool bWantToReplaceReservoir = false;
	double dElapsedTimeInYears = 0.0;

	// Initialize
	ReplaceReservoir(dElapsedTimeInYears);
	mp_geo_out->md_PumpWorkKW = GetPumpWorkKW();

	// Go through time step (hours or months) one by one
//    bool bReDrill = false;
	unsigned int iElapsedMonths = 0, iElapsedTimeSteps = 0, iEvaluationsInMonth = 0, iElapsedHours = 0;
	float fMonthlyPowerTotal;
	for (unsigned int year = 0; year < mo_geo_in.mi_ProjectLifeYears; year++)
	{
		mp_geo_out->maf_ReplacementsByYear[year] = 0;
		for (unsigned int month = 1; month < 13; month++)
		{
			fPercentDone = (float)iElapsedMonths / (float)(12 * mo_geo_in.mi_ProjectLifeYears) * 100.0f;

			if ((update_function != 0) && (TimeToUpdateInterface(fPercentDone, 2.0f)))
			{
				if (!(*update_function)(fPercentDone, user_data))
				{
					ms_ErrorString = "Aborted by user.";
					return false;
				}
			}

			fMonthlyPowerTotal = 0;
			for (unsigned int hour = 0; hour < (unsigned int)util::hours_in_month(month); hour++)
			{
				if (IsHourly() || (hour == 0))
				{
					// Error check
					if (iElapsedTimeSteps >= mo_geo_in.mi_TotalMakeupCalculations)
					{
						ms_ErrorString = "Time step exceded the array size in CGeoHourlyAnalysis::RunAnalysis().";
						return false;
					}

					// Read weather file info (function is smart enough to average for month if tis is a monthly analysis)
					// The call to ReadWeatherForTimeStep increments the hour counter (over whole life), and file read counter [0 to 8760(=# lines in weather file]
					if (!ReadWeatherForTimeStep(IsHourly(), iElapsedTimeSteps)) return false;

					// Set inputs that change for each timestep, weather data into power block inputs
					mo_pb_in.T_htf_hot = md_WorkingTemperatureC;
					mo_pb_in.T_wb = m_wf.twet;
					mo_pb_in.T_db = m_wf.tdry;
					mo_pb_in.P_amb = physics::mBarToAtm(m_wf.pres);
					mo_pb_in.TOU = mo_geo_in.mia_tou[ml_ReadCount - 1];

					// record current temperature (temperature changes monthly, but this is an hourly record of it)
					mp_geo_out->maf_timestep_resource_temp[iElapsedTimeSteps] = (float)md_WorkingTemperatureC; // NOTE: If EGS temp drop is being calculated, then PlantGrossPowerkW must be called.  No production = no temp change
					mp_geo_out->maf_timestep_pressure[iElapsedTimeSteps] = (float)mo_pb_in.P_amb;
					mp_geo_out->maf_timestep_dry_bulb[iElapsedTimeSteps] = (float)mo_pb_in.T_db;
					mp_geo_out->maf_timestep_wet_bulb[iElapsedTimeSteps] = (float)mo_pb_in.T_wb;

					// record outputs based on current inputs
					if (mo_geo_in.mi_ModelChoice == 0) // model choice 0 = GETEM
						mp_geo_out->maf_timestep_power[iElapsedTimeSteps] = (float)MAX(PlantGrossPowerkW() - mp_geo_out->md_PumpWorkKW, 0);
					else
					{	// run power block model
						if (!mo_PowerBlock.Execute((ml_HourCount - 1) * 3600, mo_pb_in))
							ms_ErrorString = "There was an error running the power block model: " + mo_PowerBlock.GetLastError();
						mp_geo_out->maf_timestep_power[iElapsedTimeSteps] = (float)MAX(mo_PowerBlock.GetOutputkW() - mp_geo_out->md_PumpWorkKW, 0);
						//fJunk = (float)moMA->PlantGrossPowerkW(); // kinda works, but not quite the same
					}

					mp_geo_out->maf_timestep_test_values[iElapsedTimeSteps] = (float)(year + 1) * 1000 + (month);//+(hour); // puts number formatted "year,month,hour_of_month" number into test value

					fMonthlyPowerTotal += mp_geo_out->maf_timestep_power[iElapsedTimeSteps];

					// record hourly power which = hourly energy
					mp_geo_out->maf_hourly_power[iElapsedHours] = mp_geo_out->maf_timestep_power[iElapsedTimeSteps];

					//md_ElapsedTimeInYears = year + util::percent_of_year(month,hour);
					if (!ms_ErrorString.empty()) { return false; }
					iElapsedTimeSteps++;
					dElapsedTimeInYears = iElapsedTimeSteps * (1.0 / mo_geo_in.mi_MakeupCalculationsPerYear);  //moved to be after iElapsedTimeSteps++;
				}
				else
					mp_geo_out->maf_hourly_power[iElapsedHours] = fMonthlyPowerTotal;

				iElapsedHours++;
			}//hours

			mp_geo_out->maf_monthly_resource_temp[iElapsedMonths] = (float)md_WorkingTemperatureC;	// resource temperature for this month
			iEvaluationsInMonth = (IsHourly()) ? (unsigned int)util::hours_in_month(month) : 1;
			mp_geo_out->maf_monthly_power[iElapsedMonths] = fMonthlyPowerTotal / iEvaluationsInMonth;		// avg monthly power
			mp_geo_out->maf_monthly_energy[iElapsedMonths] = fMonthlyPowerTotal * util::hours_in_month(month) / iEvaluationsInMonth;		// energy output in month (kWh)

			// Is it possible and do we want to replace the reservoir in the next time step?
			//bWantToReplaceReservoir = ( md_WorkingTemperatureC < (GetResourceTemperatureC() - geothermal::MAX_TEMPERATURE_DECLINE_C) ) ? true : false;
			bWantToReplaceReservoir = (md_WorkingTemperatureC < (GetResourceTemperatureC() - mo_geo_in.md_MaxTempDeclineC)) ? true : false;
			if (bWantToReplaceReservoir && CanReplaceReservoir(dElapsedTimeInYears + (1.0 / 12)))
			{
				ReplaceReservoir(dElapsedTimeInYears); // this will 'reset' temperature back to original resource temp
				mp_geo_out->maf_ReplacementsByYear[year] = mp_geo_out->maf_ReplacementsByYear[year] + 1;
			}
			else
				//CalculateNewTemperature( dElapsedTimeInYears ); // once per month -> reduce temperature from last temp
				CalculateNewTemperature(dElapsedTimeInYears + (1.0 / 12)); // once per month -> reduce temperature from last temp

			iElapsedMonths++;  //for recording values into arrays, not used in calculations
		}//months
	}//years

	if (!ms_ErrorString.empty()) return false;
	return true;
}


bool CGeothermalAnalyzer::TimeToUpdateInterface(float fPercentDone, float fNotificationIntervalInPercent)
{	// Needs to be called with fPercentDone = zero at beginning of each run to reset the static var

	if (fPercentDone == 0)
	{
		mf_LastIntervalDone = 0;
		return true;
	}

	if (fPercentDone >= (mf_LastIntervalDone + fNotificationIntervalInPercent))
	{
		mf_LastIntervalDone += fNotificationIntervalInPercent;
		return true;
	}

	return false;
}

bool CGeothermalAnalyzer::InterfaceOutputsFilled(void)
{
	if (inputErrorsForUICalculations()) return false;

	// This is not very efficient because it will call much of the code several times un-necessarily. Probably
	// doesn't matter, since this is called from the user interface and not repetatively.
	GetNumberOfWells();
	mp_geo_out->md_PlantBrineEffectiveness = GetPlantBrineEffectiveness();
	ReplaceReservoir(0.0); // set the working temp so the further calculations are correct
	mp_geo_out->md_GrossPlantOutputMW = PlantGrossPowerkW() / 1000;

	mp_geo_out->md_PumpWorkKW = GetPumpWorkKW();
	mp_geo_out->md_PumpDepthFt = GetCalculatedPumpDepthInFeet();
	// mp_geo_out->md_BottomHolePressure  is calculated in GetCalculatedPumpDepthInFeet()
	//mp_geo_out->md_PumpHorsePower = (flowRatePerWell() * pumpHeadFt())/(60 * 33000 * geothermal::EFFICIENCY_PUMP_GF);
	mp_geo_out->md_PumpHorsePower = (flowRatePerWell() * pumpHeadFt()) / (60 * 33000 * mo_geo_in.md_GFPumpEfficiency);

	mp_geo_out->md_AverageReservoirTemperatureF = physics::CelciusToFarenheit(GetResourceTemperatureC());	// Set a default value, it might be recalculated in "GetPressureChangeAcrossReservoir()" if necessary
	mp_geo_out->md_PressureChangeAcrossReservoir = GetPressureChangeAcrossReservoir();

	if ((mp_geo_out->md_NumberOfWells > 0) && (error().empty()))
		return true;
	else
		return false;
}




/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// RunGeothermalAnalysis
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
int RunGeothermalAnalysis(bool(*update_function)(float, void*), void*user_data, std::string &err_msg,
	const SPowerBlockParameters &pbp, SPowerBlockInputs &pbInputs,
	const SGeothermal_Inputs &geo_inputs, SGeothermal_Outputs &geo_outputs)
{
	// return value 0 = clean run; 1 = error with message; 2 = unknown error, no message
	CGeothermalAnalyzer geo_analyzer(pbp, pbInputs, geo_inputs, geo_outputs);
	if (geo_analyzer.RunAnalysis(update_function, user_data))  // 
		return 0;
	else
		if (geo_analyzer.error() != "")
		{
			err_msg = geo_analyzer.error();
			return 1; // error that was flagged
		}
		else
		{
			err_msg = "Unknown error during run"; return 2;
		}
}


/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Get interum values for user interface
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
int FillOutputsForUI(std::string &err_msg, const SGeothermal_Inputs &geo_inputs, SGeothermal_Outputs &geo_outputs)
{
	// return value 0 = clean run; 1 = error with message; 2 = unknown error, no message
	CGeothermalAnalyzer geo_analyzer(geo_inputs, geo_outputs);
	if (geo_analyzer.InterfaceOutputsFilled())
		return 0;
	else
		if (geo_analyzer.error() != "")
		{
			err_msg = geo_analyzer.error();
			return 1; // error that was flagged
		}
		else
		{
			err_msg = "Unknown error during run"; return 2;
		}
}