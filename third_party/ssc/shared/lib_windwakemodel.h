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

#ifndef __lib_windwake
#define __lib_windwake

#include <vector>
#include "lib_util.h"
#include "lib_physics.h"

/**
 * windTurbine class stores characteristics of turbine used in simulation and the power curve arrays,
 * which map from wind speed to turbine's output. The following public variables must be set before use:
 * shearExponent, measurementHeight, hubHeight, rotorDiameter, lossesAbsolute, lossesPercent; and the
 * powerCurve must be filled using setPowerCurve();
 */

class windTurbine
{
private:
	std::vector<double> powerCurveWS,			// windspeed: x-axis on turbine power curve
						powerCurveKW,			// power output: y-axis
						densityCorrectedWS,
						powerCurveRPM;
	double cutInSpeed;
	double previousAirDensity;
public:

	std::vector<double> getPowerCurveWS(){ return powerCurveWS; }
	std::vector<double> getPowerCurveKW(){ return powerCurveKW; }

	size_t powerCurveArrayLength;
	double rotorDiameter, hubHeight, measurementHeight, shearExponent;
	std::string errDetails;

	windTurbine(){ 
		shearExponent = -999;
		measurementHeight = -999;
		hubHeight = -999;
		rotorDiameter = -999;
        previousAirDensity = physics::AIR_DENSITY_SEA_LEVEL;
	}
	bool setPowerCurve(std::vector<double> windSpeeds, std::vector<double> powerOutput);
	
	double tipSpeedRatio(double windSpeed);

	bool isInitialized(){
		if (shearExponent != -999 && measurementHeight != -999 && hubHeight != -999 && rotorDiameter != -999){
			if (powerCurveArrayLength > 0) return true;
		}
		return false;
	}
	void turbinePower(double windVelocity, double airDensity, double *turbineOutput, double *turbineGross,
                      double *thrustCoefficient);
	double calculateEff(double reducedPower, double originalPower) {
		double Eff = 0.0;
		if (originalPower < 0.0)
			Eff = 0.0;
		else
			Eff = 100.0*(reducedPower + 0.0001) / (originalPower + 0.0001);
		return Eff;
	}
};

/**
 * Wake models are used to calculate the wind velocity deficit at a turbine and the following changes to power, efficient, thrust and
 * turbulence intensity. The class requires an turbine with initialized values to run. Error messages can be propagated via errDetails.
 */

class wakeModelBase
{
protected:
	size_t nTurbines;
	windTurbine* wTurbine;
public:
	wakeModelBase(){}
	virtual ~wakeModelBase() {};
	virtual std::string getModelName(){ return ""; };
	std::string errDetails;
	virtual int test(int a){ return a + 10; }
	virtual void wakeCalculations(
		const double airDensity, const double distanceDownwind[], const double distanceCrosswind[],
		double power[], double eff[], double thrust[], double windSpeed[], double turbulenceIntensity[]) = 0;
};

/**
* simpleWakeModel uses Pat Quinlan's model: calculate the wind speed deficit factor to estimate the reduction in wind speed
* at a downwind turbine due to the wake of an upwind turbine. The model returns the modified wind speed for each turbine.
*/

class simpleWakeModel : public wakeModelBase{
private:	
	double velDeltaPQ(double radiiCrosswind, double axialDistInRadii, double thrustCoeff, double *newTurbulenceIntensity);

public:
	simpleWakeModel(){ nTurbines = 0; }
	simpleWakeModel(size_t numberOfTurbinesInFarm, windTurbine* wt){ nTurbines = numberOfTurbinesInFarm; wTurbine = wt; }
	virtual ~simpleWakeModel() {};
	std::string getModelName() override { return "PQ"; }

	void wakeCalculations(
		/*INPUTS*/
		const double airDensity,					// not used in this model
		const double distanceDownwind[],			// downwind coordinate of each WT
		const double distanceCrosswind[],			// crosswind coordinate of each WT

		/*OUTPUTS*/
		double power[],
		double eff[],
		double thrust[],					// thrust coefficient at each WT
		double windSpeed[],					// wind speed at each WT
		double turbulenceIntensity[]		// turbulence intensity at each WT
	) override;
};

/**
* The Park / WAsP model assumes the turbine wake width expands linearly downstream, dependent on wake-decay
* constant k = 0.07, empirically determined. The wind speed deficit of a downstream turbine depends on how
* much its swept area overlaps with the upstream wake. 
*/

class parkWakeModel : public wakeModelBase{
private:
	double rotorDiameter;
	double wakeDecayCoefficient = 0.07,
		   minThrustCoeff = 0.02;
	double delta_V_Park(double dVelFreeStream, double dVelUpwind, double dDistCrossWind, double dDistDownWind, double dRadiusUpstream, double dRadiusDownstream, double dThrustCoeff);
	double circle_overlap(double dist_center_to_center, double rad1, double rad2);

public:
	parkWakeModel(){ nTurbines = 0; }
	parkWakeModel(size_t numberOfTurbinesInFarm, windTurbine* wt){ nTurbines = numberOfTurbinesInFarm; wTurbine = wt; }
	virtual ~parkWakeModel() {};
	std::string getModelName() override { return "Park"; }
	void setRotorDiameter(double d){ rotorDiameter = d; }
	void wakeCalculations(
		/*INPUTS*/
		const double airDensity,					// not used in this model
		const double distanceDownwind[],			// downwind coordinate of each WT
		const double distanceCrosswind[],			// crosswind coordinate of each WT

		/*OUTPUTS*/
		double power[],
		double eff[],
		double thrust[],					// thrust coefficient at each WT
		double windSpeed[],					// wind speed at each WT
		double turbulenceIntensity[]		// turbulence intensity at each WT
	) override;
};

/**
* Eddy viscosity wake Model requires a turbulence coefficient and an initialized windTurbine to operate on variables:
* thrust, power, eff, windspeed, turbulence intensity. Note: turbulence intensity as percent is used, whereas default is as ratio.
*/

class eddyViscosityWakeModel : public wakeModelBase{
private:
	double rotorDiameter, turbulenceCoeff;
	double axialResolution, minThrustCoeff, nBlades;
	double minDeficit;
	int MIN_DIAM_EV, EV_SCALE, MAX_WIND_TURBINES;
	bool useFilterFx;
	// EV wake matrices: each turbine is row, each col is wake data for that turbine at dist
	util::matrix_t<double> matEVWakeDeficits;	// wind velocity deficit behind each turbine, indexed by axial distance downwind
	util::matrix_t<double> matEVWakeWidths;		// width of wake (in diameters) for each turbine, indexed by axial distance downwind

	struct VMLN
	{
		VMLN(){}
		virtual ~VMLN(){}

		double m;
		double Ro;
		double Xh;
		double Xn;
		double Rh;
		double Rn;
		double Xf;
		double Rf;
		double dUc_Uinf_Xn;
		double diam;
	};

	/// get the velocity deficit at an axial distance behind a given upwind turbine using the wake deficit matrix
	double getVelocityDeficit(int upwindTurbine, double axialDistanceInDiameters);

	/// get the wake width at a radial distance from the centerline, using the given upwind turbine's wake widths matrix
	double getWakeWidth(int upwindTurbine, double axialDistanceInDiameters);

	/// get deficit by modeling wake beyond near wake region as axisymmetric profile with Gaussian cross-section
	double wakeDeficit(int upwindTurbine, double distCrosswind, double distDownwind);

	/// Use the Pat Quinlan method to get added TI
	double addedTurbulenceIntensity(double Ct, double deltaX);
	
	double totalTurbulenceIntensity(double ambientTI, double additionalTI, double Uo, double Uw, double partial);

	bool fillWakeArrays(int turbineIndex, double ambientVelocity, double velocityAtTurbine, double power, double thrustCoeff, double turbulenceIntensity, double maxX);

	/// Using Ii, ambient turbulence intensity, and thrust coeff, calculates the length of the near wake region
	void nearWakeRegionLength(double U, double Ii, double Ct, double airDensity, VMLN& vmln);

	/// Returns the intersection area between the downwind turbine swept area and the wake
	double simpleIntersect(double distToCenter, double radiusTurbine, double radiusWake);

public:
	eddyViscosityWakeModel(){ nTurbines = 0; }
	eddyViscosityWakeModel(size_t numberOfTurbinesInFarm, windTurbine* wt, double turbCoeff){ 
		wTurbine = wt;
		rotorDiameter = wt->rotorDiameter;
		if (turbCoeff >= 0 && turbCoeff <= 1) turbulenceCoeff = turbCoeff ;
		else turbulenceCoeff = .10;

		nTurbines = numberOfTurbinesInFarm; 
		axialResolution = 0.5;
		minThrustCoeff = 0.02;
		nBlades = 3;
		minDeficit = 0.0002;
		MIN_DIAM_EV = 2;
		EV_SCALE = 1;
		MAX_WIND_TURBINES = 300;
		axialResolution = 0.5; // in rotor diameters, default in openWind=0.5
		//double radialResolution = 0.2; // in rotor diameters, default in openWind=0.2
		double maxRotorDiameters = 50; // in rotor diameters, default in openWind=50
		useFilterFx = true;
		matEVWakeDeficits.resize_fill(nTurbines, (int)(maxRotorDiameters / axialResolution) + 1, 0.0); // each turbine is row, each col is wake deficit for that turbine at dist
		matEVWakeWidths.resize_fill(nTurbines, (int)(maxRotorDiameters / axialResolution) + 1, 0.0); // each turbine is row, each col is wake deficit for that turbine at dist
	}
	virtual ~eddyViscosityWakeModel() {};
	std::string getModelName() override { return "FastEV"; }

	void wakeCalculations(
		/*INPUTS*/
		const double airDensity,					// not used in this model
		const double distanceDownwind[],			// downwind coordinate of each WT
		const double distanceCrosswind[],			// crosswind coordinate of each WT

		/*OUTPUTS*/
		double power[],
		double eff[],
		double thrust[],					// thrust coefficient at each WT
		double windSpeed[],					// wind speed at each WT
		double turbulenceIntensity[]		// turbulence intensity at each WT
		) override;
};

/**
* constantWakeModel derates the wind power output by a constant percentage.
*/

class constantWakeModel : public wakeModelBase
{
    double derate;
public:
    constantWakeModel(size_t nTurbs, windTurbine* wt, double derate_multiplier){
        nTurbines = nTurbs;
        wTurbine = wt;
        derate = derate_multiplier;
    }
    std::string getModelName() override { return "Constant"; }
    void wakeCalculations (
            /*INPUTS*/
            const double airDensity,					// not used in this model
            const double distanceDownwind[],			// downwind coordinate of each WT
            const double distanceCrosswind[],			// crosswind coordinate of each WT

            /*OUTPUTS*/
            double power[],
            double eff[],
            double thrust[],					// thrust coefficient at each WT
            double windSpeed[],					// wind speed at each WT
            double turbulenceIntensity[]		// turbulence intensity at each WT
            ) override;
};

#endif