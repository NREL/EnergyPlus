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

#include <cstring>

#include "lib_windwatts.h"
#include "lib_physics.h"

#include <iostream>
#include <math.h>
#include "lib_util.h"
#include "lib_windwakemodel.h"


#ifdef _MSC_VER
#pragma warning(disable: 4127)  // ignore warning: 'warning C4127: conditional expression is constant'
#endif

bool windPowerCalculator::InitializeModel(std::shared_ptr<wakeModelBase>selectedWakeModel)
{
	if (selectedWakeModel){
		wakeModel = selectedWakeModel;
		return true;
	}
	return false;
}

std::string windPowerCalculator::GetWakeModelName()
{
	if (wakeModel) return wakeModel->getModelName();
	return "NA";
}

double windPowerCalculator::gammaln(double x)
{
	// Based on VBA code in Xnumbers.xla v 5.6
	// by Foxes Team, 2007
	// E -mail: leovlp@libero.it
	// Web:    http://digilander.libero.it/foxes
	// 10.11.2006

	double z, w, s, p, mantissa, expo;
	std::vector<double> cf(15);
	const double DOUBLEPI = 2 * physics::PI;
	const double G_ = 607.0 / 128.0; //= 4.7421875

	z = x - 1;
	cf[0] = 0.999999999999997;
	cf[1] = 57.1562356658629;
	cf[2] = -59.5979603554755;
	cf[3] = 14.1360979747417;
	cf[4] = -0.49191381609762;
	cf[5] = 3.39946499848119E-05;
	cf[6] = 4.65236289270486E-05;
	cf[7] = -9.83744753048796E-05;
	cf[8] = 1.58088703224912E-04;
	cf[9] = -2.10264441724105E-04;
	cf[10] = 2.17439618115213E-04;
	cf[11] = -1.64318106536764E-04;
	cf[12] = 8.44182239838528E-05;
	cf[13] = -2.61908384015814E-05;
	cf[14] = 3.68991826595316E-06;

	w = exp(G_) / sqrt(DOUBLEPI);
	s = cf[0];

	for (int i = 1; i<15; i++){
		s += cf[i] / (z + i);
	}
	s = s / w;
	p = log((z + G_ + 0.5) / exp(1.0)) * (z + 0.5) / log(10.0);

	//split in mantissa and exponent to avoid overflow
	expo = floor(p);
	p = p - floor(p);
	mantissa = pow(10, p) * s;

	//rescaling
	p = floor(log(mantissa) / log(10.0));  // 'int' replaced with '' since VBA 'int' rounds negative numbers down
	mantissa = mantissa * pow(10.0, -p);
	expo = expo + p;

	return log(mantissa) + expo * log(10.0);
}

void windPowerCalculator::coordtrans(double metersNorth, double metersEast, double windDirDegrees, double *metersDownwind, double *metersCrosswind)
{
	// rotate wind direction to match unit circle (where zero is East, not North)
	windDirDegrees += 90;

	// convert degrees to radians
	double fWind_dir_radians = windDirDegrees*physics::PI / 180.0;

	// create downwind and crosswind coordinates
	*metersDownwind = metersEast*cos(fWind_dir_radians) - (metersNorth * sin(fWind_dir_radians)); //!northerly = FROM North"
	*metersCrosswind = metersEast*sin(fWind_dir_radians) + (metersNorth * cos(fWind_dir_radians));
}

int
windPowerCalculator::windPowerUsingResource(double windSpeed, double windDirDeg, double airPressureAtm, double TdryC,
                                            double *farmPower,
                                            double *farmPowerGross, double power[], double thrust[], double eff[],
                                            double adWindSpeed[],
                                            double TI[], double distanceDownwind[], double distanceCrosswind[])
{
    if (!wakeModel)
    {
        errDetails = "Wake model not initialized.";
        return 0;
    }

    if ((nTurbines > MAX_WIND_TURBINES) || (nTurbines < 1))
	{
		errDetails = "The number of wind turbines was greater than the maximum allowed in the wake model.";
		return 0;
	}

	size_t i, j;
	//unsigned char wt_id[MAX_WIND_TURBINES], wid; // unsigned char has 256 limit
	size_t wt_id[MAX_WIND_TURBINES], wid;

	for (i = 0; i<nTurbines; i++)
		wt_id[i] = i;

	// convert barometric pressure in ATM to air density
	double fAirDensity = (airPressureAtm * physics::Pa_PER_Atm) / (physics::R_GAS_DRY_AIR * physics::CelciusToKelvin(TdryC));   //!Air Density, kg/m^3

	// calculate output power of a turbine
	double fTurbine_output(0.0), fThrust_coeff(0.0), fTurbine_gross(0.0);
    windTurb->turbinePower(windSpeed, fAirDensity, &fTurbine_output, &fTurbine_gross, &fThrust_coeff);
	if (windTurb->errDetails.length() > 0){
		errDetails = windTurb->errDetails;
		return 0;
	}
	*farmPowerGross = fTurbine_gross * nTurbines;

	// initialize values before possible exit from the function
	for (i = 0; i<nTurbines; i++)
	{
		power[i] = 0.0;
		thrust[i] = 0.0;
		eff[i] = 0.0;
		adWindSpeed[i] = windSpeed;
		TI[i] = turbulenceIntensity;
	}

	// if there is only one turbine, we're done
	if (nTurbines < 2)
	{
		*farmPower = fTurbine_output;
		return 1;
	}

	// if power output of first turbine is zero, then it will be for the rest: we're done
	if (fTurbine_output <= 0.0)
	{
		*farmPower = 0.0;
		return (int)nTurbines;
	}

	// if constant loss wake model, simply apply and exit
	if (std::strcmp(wakeModel->getModelName().c_str(), "Constant") == 0)
	{
        wakeModel->wakeCalculations(fAirDensity, &distanceDownwind[0], &distanceCrosswind[0], power, eff, thrust, adWindSpeed, TI);
        *farmPower = power[0] * nTurbines;
        return (int)nTurbines;
	}

	// ok, let's calculate the farm output
	//!Convert to d (downwind - axial), c (crosswind - radial) coordinates
	double d(0.0), c(0.0);
	//std::vector<double> aDistanceDownwind(m_iNumberOfTurbinesInFarm);	// downwind coordinate of each WT
	//std::vector<double> aDistanceCrosswind(m_iNumberOfTurbinesInFarm);	// crosswind coordinate of each WT
	for (i = 0; i<nTurbines; i++)
	{
		coordtrans(YCoords[i], XCoords[i], windDirDeg, &d, &c);
		distanceDownwind[i] = d;
		distanceCrosswind[i] = c;
	}

	// Remove negative numbers from downwind, crosswind coordinates 	
	double Dmin = distanceDownwind[0];
	double Cmin = distanceCrosswind[0];

	for (j = 1; j<nTurbines; j++)
	{
		Dmin = min_of(distanceDownwind[j], Dmin);
		Cmin = min_of(distanceCrosswind[j], Cmin);
	}
	for (j = 0; j<nTurbines; j++)
	{
		distanceDownwind[j] = distanceDownwind[j] - Dmin; // Final downwind coordinates, meters
		distanceCrosswind[j] = distanceCrosswind[j] - Cmin; // Final crosswind coordinates, meters
	}

	// Convert downwind, crosswind measurements from meters into wind turbine radii
	for (i = 0; i<nTurbines; i++)
	{
		distanceDownwind[i] = 2.0*distanceDownwind[i] / windTurb->rotorDiameter;
		distanceCrosswind[i] = 2.0*distanceCrosswind[i] / windTurb->rotorDiameter;
	}

	// Record the output for the most upwind turbine (already calculated above)
	power[0] = fTurbine_output;
	thrust[0] = fThrust_coeff;
	eff[0] = (fTurbine_output < 1.0) ? 0.0 : 100.0;


	// Sort aDistanceDownwind, aDistanceCrosswind arrays by downwind distance, aDistanceDownwind[0] is smallest downwind distance, presumably zero
	for (j = 1; j<nTurbines; j++)
	{
		d = distanceDownwind[j]; // pick out each element
		c = distanceCrosswind[j];
		wid = wt_id[j];

		i = j;
		while (i > 0 && distanceDownwind[i - 1] > d) // look for place to insert item
		{
			distanceDownwind[i] = distanceDownwind[i - 1];
			distanceCrosswind[i] = distanceCrosswind[i - 1];
			wt_id[i] = wt_id[i - 1];
			i--;
		}

		distanceDownwind[i] = d; // insert it
		distanceCrosswind[i] = c;
		wt_id[i] = wid;
	}

	// calculate the power output of downwind turbines using wake model
	wakeModel->wakeCalculations(fAirDensity, &distanceDownwind[0], &distanceCrosswind[0], power, eff, thrust, adWindSpeed, TI);
	if (wakeModel->errDetails.length() > 0){
		errDetails = wakeModel->errDetails;
		return 0;
	}

	// calculate total farm power
	*farmPower = 0;
	for (i = 0; i<nTurbines; i++)
		*farmPower += power[i];

	// Update down/cross wind distance units for turbine zero (convert from radii to meters)
	distanceDownwind[0] *= windTurb->rotorDiameter / 2.;
	distanceCrosswind[0] *= windTurb->rotorDiameter / 2.;

	// Re-sort output arrays by wind turbine ID (0..nwt-1)
	// for consistent reporting
	double p(0.0), t(0.0), e(0.0), w(0.0), b(0.0), dd(0.0), dc(0.0);
	for (j = 1; j<nTurbines; j++)
	{
		p = power[j];// pick out each element
		t = thrust[j];
		e = eff[j];
		w = adWindSpeed[j];
		b = TI[j];
		dd = distanceDownwind[j] * windTurb->rotorDiameter / 2.; // convert back to meters from radii
		dc = distanceCrosswind[j] * windTurb->rotorDiameter / 2.;
		wid = wt_id[j];

		i = j;
		while (i > 0 && wt_id[i - 1] > wid) // look for place to insert item
		{
			power[i] = power[i - 1];
			thrust[i] = thrust[i - 1];
			eff[i] = eff[i - 1];
			adWindSpeed[i] = adWindSpeed[i - 1];
			TI[i] = TI[i - 1];
			distanceDownwind[i] = distanceDownwind[i - 1];
			distanceCrosswind[i] = distanceCrosswind[i - 1];
			wt_id[i] = wt_id[i - 1];
			i--;
		}

		power[i] = p;
		thrust[i] = t;
		eff[i] = e;
		adWindSpeed[i] = w;
		TI[i] = b;
		distanceDownwind[i] = dd;
		distanceCrosswind[i] = dc;
		wt_id[i] = wid;
	}

	return (int)nTurbines;
}


double windPowerCalculator::windPowerUsingWeibull(double weibull_k, double avg_speed, double ref_height, double energy_turbine[])
{	// returns same units as 'power_curve'

	double hub_ht_windspeed = pow((windTurb->hubHeight / ref_height), windTurb->shearExponent) * avg_speed;
	double denom = exp(gammaln(1 + (1 / weibull_k))); //fixed jmf 2/18/15- weibull_k was accidentally replaced with hub_ht_windspeed previously

	double lambda = hub_ht_windspeed / denom;
	//double air_density = physics::Pa_PER_Atm * pow( (1-((0.0065*elevation)/288.0)), (physics::GRAVITY_MS2/(0.0065*287.15)) ) / (287.15*(288.0-0.0065*elevation));

	// 'RUN' MODEL ****************************************************************************************
	double total_energy_turbine = 0;//, total_energy_generic=0;
	std::vector<double> weibull_cummulative(windTurb->powerCurveArrayLength, 0);
	std::vector<double> weibull_bin(windTurb->powerCurveArrayLength, 0);
	//std::vector<double> weibull_probability(m_iLengthOfTurbinePowerCurveArray, 0);
	//std::vector<double> energy_turbine(m_iLengthOfTurbinePowerCurveArray, 0);	// energy from turbine chosen from library

	// weibull_k = 2.10; // used for testing: this is off in the 5th significant digit when passed into SSC from samwx

	// CHANGE IN METHODOLOGY JMF 3/17/15
	/* The cost and scaling model calculates the POINT weibull probability and multiplies it by the width of the bin (0.25 m/s)- implemented by dividing by 4 in "Energy Capture" result.
	This is effectively a midpoint integration. We were effectively calculating a right-hand integration by assuming that the cumulative weibull_bin (more accurate)
	should be paired with the power curve at the upper end of the bin. To fix this, we will calculate the weibull probabilities shifted up by half of the bin width
	(0.5 * 0.25 m/s = 0.125 m/s), so that the WS lies at the midpoint of the probability bin.
	*/
	weibull_cummulative[0] = 1.0 - exp(-pow((0.125) / lambda, weibull_k)); //first bin is probability from 0 to 0.125 m/s
	weibull_bin[0] = weibull_cummulative[0]; //first bin is equal to first cumulative calculation
	energy_turbine[0] = 0.0;
	for (size_t i = 1; i<windTurb->powerCurveArrayLength; i++)
	{
		// calculate Weibull likelihood of the wind blowing in the range from windspeed[i - 0.5*bin_width] to windspeed[i + 0.5*bin_width]
		weibull_cummulative[i] = 1.0 - exp(-pow((windTurb->getPowerCurveWS()[i] + 0.125) / lambda, weibull_k)); //the 0.125 shifts the probability bin so that the WS lies at the midpoint of the bin
		weibull_bin[i] = weibull_cummulative[i] - weibull_cummulative[i - 1];

		// calculate annual energy from turbine at this wind speed = (hours per year at this wind speed) X (turbine output at wind speed)
		energy_turbine[i] = (8760.0 * weibull_bin[i]) * windTurb->getPowerCurveKW()[i];

		// keep track of cummulative output
		total_energy_turbine += energy_turbine[i];
	}

	// calculate output accounting for losses
	return total_energy_turbine;
}

bool windPowerCalculator::windPowerUsingDistribution(std::vector<std::vector<double>> &&wind_dist, double *farmPower,
                                                     double *farmPowerGross)
{
    if (!wakeModel)
    {
        errDetails = "Wake model not initialized.";
        return false;
    }

    if ((nTurbines > MAX_WIND_TURBINES) || (nTurbines < 1))
    {
        errDetails = "The number of wind turbines was greater than the maximum allowed in the wake model.";
        return false;
    }

    size_t i, j;
    //unsigned char wt_id[MAX_WIND_TURBINES], wid; // unsigned char has 256 limit
    size_t wt_id[MAX_WIND_TURBINES], wid;

    for (i = 0; i<nTurbines; i++)
        wt_id[i] = i;


    double freq_total = 0.0, farmpower = 0.0, farmgross = 0.0;
    for (auto& row : wind_dist){
        double& windSpeed = row[0];
        double& windDirDeg = row[1];
        freq_total += row[2];

        // calculate output power of a turbine
        double fTurbine_output(0.0), fThrust_coeff(0.0), fTurbine_gross(0.0);
        windTurb->turbinePower(windSpeed, physics::AIR_DENSITY_SEA_LEVEL, &fTurbine_output, &fTurbine_gross, &fThrust_coeff);
        if (windTurb->errDetails.length() > 0){
            errDetails = windTurb->errDetails;
            return false;
        }

        // if there is only one turbine, we're done
        if (nTurbines < 2)
        {
            farmpower += fTurbine_output;
            continue;
        }

        // if power output of first turbine is zero, then it will be for the rest: we're done
        if (fTurbine_output <= 0.0)
        {
            continue;
        }

        // calculate the farm output
        //!Convert to d (downwind - axial), c (crosswind - radial) coordinates
        double d(0.0), c(0.0);
        std::vector<double> distanceDownwind(nTurbines);	// downwind coordinate of each WT
        std::vector<double> distanceCrosswind(nTurbines);	// crosswind coordinate of each WT
        for (i = 0; i<nTurbines; i++)
        {
            coordtrans(YCoords[i], XCoords[i], windDirDeg, &d, &c);
            distanceDownwind[i] = d;
            distanceCrosswind[i] = c;
        }

        // Remove negative numbers from downwind, crosswind coordinates
        double Dmin = distanceDownwind[0];
        double Cmin = distanceCrosswind[0];

        for (j = 1; j<nTurbines; j++)
        {
            Dmin = min_of(distanceDownwind[j], Dmin);
            Cmin = min_of(distanceCrosswind[j], Cmin);
        }
        for (j = 0; j<nTurbines; j++)
        {
            distanceDownwind[j] = distanceDownwind[j] - Dmin; // Final downwind coordinates, meters
            distanceCrosswind[j] = distanceCrosswind[j] - Cmin; // Final crosswind coordinates, meters
        }

        // Convert downwind, crosswind measurements from meters into wind turbine radii
        for (i = 0; i<nTurbines; i++)
        {
            distanceDownwind[i] = 2.0*distanceDownwind[i] / windTurb->rotorDiameter;
            distanceCrosswind[i] = 2.0*distanceCrosswind[i] / windTurb->rotorDiameter;
        }

        // Sort aDistanceDownwind, aDistanceCrosswind arrays by downwind distance, aDistanceDownwind[0] is smallest downwind distance, presumably zero
        for (j = 1; j<nTurbines; j++)
        {
            d = distanceDownwind[j]; // pick out each element
            c = distanceCrosswind[j];
            wid = wt_id[j];

            i = j;
            while (i > 0 && distanceDownwind[i - 1] > d) // look for place to insert item
            {
                distanceDownwind[i] = distanceDownwind[i - 1];
                distanceCrosswind[i] = distanceCrosswind[i - 1];
                wt_id[i] = wt_id[i - 1];
                i--;
            }

            distanceDownwind[i] = d; // insert it
            distanceCrosswind[i] = c;
            wt_id[i] = wid;
        }

        // calculate the power output of downwind turbines using wake model
        std::vector<double> power(nTurbines, fTurbine_output), eff(nTurbines, 0.), thrust(nTurbines, 0.),
                adWindSpeed(nTurbines, windSpeed), TI(nTurbines, turbulenceIntensity);
        wakeModel->wakeCalculations(physics::AIR_DENSITY_SEA_LEVEL, &distanceDownwind[0], &distanceCrosswind[0], &power[0],
                                    &eff[0], &thrust[0], &adWindSpeed[0], &TI[0]);
        if (wakeModel->errDetails.length() > 0){
            errDetails = wakeModel->errDetails;
            return false;
        }

        // calculate total farm power
        double freq = 8760.0 * row[2];
        for (i = 0; i<nTurbines; i++){
            farmpower += freq * power[i];
        }
        farmgross += freq * fTurbine_gross * nTurbines;
    }


    if (fabs(freq_total - 1.0) > 0.01){
        errDetails = "Sum of wind resource distribution frequencies must be 1.";
        return false;
    }
    *farmPower = farmpower;
    *farmPowerGross = farmgross;

    return true;
}
