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

#ifndef __lib_windwatts_h
#define __lib_windwatts_h

#include <memory>
#include <vector>
#include "lib_util.h"
#include "lib_windwakemodel.h"

static inline double max_of(double a, double b)
{	return (a > b) ? a : b;	}

static inline double min_of(double a, double b)
{	return (a < b) ? a : b; }

/**
 * Wind power calculator calculates the power output of a wind turbine farm. Requires an initialized windTurbine and wakeModel, as well as the following variables:
 * nTurbines, turbulenceIntensity, YCoords, XCoords. The windPowerUsingResource and windPowerUsingWeibull require allocated vectors for inputs and outputs.
 */

class windPowerCalculator
{
private:
	std::shared_ptr<wakeModelBase> wakeModel;
	std::string errDetails;

	/// Transforms the east, north coordinate system to a downwind, crosswind orientation orthogonal to current wind direction
	void coordtrans(double metersNorth, double metersEast, double fWind_dir_degrees, double *fMetersDownWind, double *metersCrosswind);
	double gammaln(double x);

public:
	windTurbine* windTurb;
	size_t nTurbines;
	double turbulenceIntensity;	
	windPowerCalculator() {
		//m_dShearExponent = 1.0/7.0;
		// check classes are initialized
		nTurbines = 0;
		turbulenceIntensity = 0.0;
		errDetails="";
	}
	
	static const int MAX_WIND_TURBINES = 300;	// Max turbines in the farm
	static const int MIN_DIAM_EV = 2;			// Minimum number of rotor diameters between turbines for EV wake modeling to work
	static const int EV_SCALE = 1;				// Uo or 1.0 depending on how you read Ainslie 1988

	//int m_iWakeModelChoice;			// 0=Original Model (based on Pat Quinlan's thesis), 1=Park Model, 2=Eddy viscosity model
	//double m_dCutInSpeed;			// wind speed Cut in (m/s)
	//double m_dRatedSpeed;			// rated wind speed
	//double m_dRatedPower;			// rated power (kw)

	std::vector<double> XCoords, YCoords;

	size_t GetMaxTurbines() {return MAX_WIND_TURBINES;}
	bool InitializeModel(std::shared_ptr<wakeModelBase>selectedWakeModel);
	std::string GetWakeModelName();
	std::string GetErrorDetails() { return errDetails; }

	int
    windPowerUsingResource(double windSpeed, double windDirDeg, double airPressureAtm, double TdryC, double *farmPower,
                           double *farmPowerGross, double power[], double thrust[], double eff[], double adWindSpeed[],
                           double TI[], double distanceDownwind[], double distanceCrosswind[]);

	/// returns energy output of a turbine
	double windPowerUsingWeibull(
		double weibull_k, 
		double avg_speed, 
		double ref_height,
		double energy_turbine[]
	);

	/// returns energy output of wind farm
    bool windPowerUsingDistribution(std::vector<std::vector<double>> &&wind_dist, double *farmPower,
                                    double *farmPowerGross);
	double windPowerUsingDistribution(std::vector<std::vector<double>>& wind_dist, double *farmPower,
                                      double *farmPowerGross){
	    return windPowerUsingDistribution(std::move(wind_dist),farmPower, farmPowerGross);
	}
};

#endif
