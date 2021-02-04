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

#include <cmath>
#include "lib_physics.h"
#include "lib_util.h"
#include "lib_windwatts.h"
#include "lib_windwakemodel.h"

bool windTurbine::setPowerCurve(std::vector<double> windSpeeds, std::vector<double> powerOutput){
	if (windSpeeds.size() == powerOutput.size()) powerCurveArrayLength = windSpeeds.size();
	else{
		errDetails = "Turbine power curve array sizes are unequal.";
		return 0;
	}
	powerCurveWS = windSpeeds;
	powerCurveKW = powerOutput;
	densityCorrectedWS = powerCurveWS;
	powerCurveRPM.resize(powerCurveArrayLength, -1);
	return 1;
}

double windTurbine::tipSpeedRatio(double windSpeed)
{
	if (powerCurveRPM[0] == -1) return 7.0;
	double rpm = 0.0;
	if ((windSpeed > powerCurveWS[0]) && (windSpeed < powerCurveWS[powerCurveArrayLength - 1]))
	{
		int j = 1;
		while (powerCurveWS[j] <= windSpeed)
			j++; // find first m_adPowerCurveRPM > fWindSpeedAtHubHeight

		rpm = util::interpolate(powerCurveWS[j - 1], powerCurveRPM[j - 1], powerCurveWS[j], powerCurveRPM[j], windSpeed);
	}
	else if (windSpeed == powerCurveWS[powerCurveArrayLength - 1])
		rpm = powerCurveRPM[powerCurveArrayLength - 1]; // rpm -> zero if wind speed greater than maximum in the array

	// if rpm>0, calculate the tip speed ratio from it, otherwise, return a reasonable value
	return (rpm>0) ? rpm * rotorDiameter * physics::PI / (windSpeed*60.0) : 7.0;
}

void windTurbine::turbinePower(double windVelocity, double airDensity, double *turbineOutput, double *turbineGross,
                               double *thrustCoefficient) {
	if (!isInitialized()){
		errDetails = "windTurbine not initialized with necessary data";
		return;
	}

	*thrustCoefficient = 0.0;
	*turbineOutput = 0.0;

	//correct wind speeds in power curve for site air density if necessary, using method 2 described in https://www.scribd.com/document/38818683/PO310-EWEC2010-Presentation
	if (fabs(airDensity - previousAirDensity) > 0.001 ) {
        double correction = pow((physics::AIR_DENSITY_SEA_LEVEL / airDensity), (1.0 / 3.0));
        for (size_t i = 0; i < densityCorrectedWS.size(); i++) {
            densityCorrectedWS[i] = powerCurveWS[i] * correction;
        }
        previousAirDensity = airDensity;
	}

	int i = 0;
	while (powerCurveKW[i] == 0)
		i++; //find the index of the first non-zero power output in the power curve
	//the cut-in speed is defined where the turbine FIRST STARTS TO TURN, not where it first generates electricity! Therefore, assume that the cut-in speed is actually 1 speed BELOW where power is generated.
	//this is consistent with the NREL Cost & Scaling model- if you specify a cut-in speed of 4 m/s, the power curve value at 4 m/s is 0, and it starts producing power at 4.25.
	//HOWEVER, if you specify the cut-in speed BETWEEN the wind speed bins, then this method would improperly assume that the cut-in speed is lower than it actually is. But given the 0.25 m/s size of the bins, that type of
	//specification would be false accuracy anyways, so we'll ignore it for now.
	cutInSpeed = densityCorrectedWS[i - 1];

	/*	//We will continue not to check cut-out speed because currently the model will interpolate between the last non-zero power point and zero, and we don't have a better definition of where the power cutoff should be.
	i = m_adPowerCurveKW.size() - 1; //last index in the array
	while (m_adPowerCurveKW[i] == 0)
	i--; //find the index of the last non-zero power output in the power curve
	m_dCutOutSpeed = m_adPowerCurveWS[i]; //unlike cut in speed, we want power to hard cut AFTER this wind speed value*/

	// Find power from turbine power curve
	double out_pwr = 0.0;
	if ((windVelocity > densityCorrectedWS[0]) && (windVelocity < densityCorrectedWS[powerCurveArrayLength - 1]))
	{
		int j = 1;
		while (densityCorrectedWS[j] <= windVelocity)
			j++; // find first m_adPowerCurveWS > windVelocity

		out_pwr = util::interpolate(densityCorrectedWS[j - 1], powerCurveKW[j - 1], densityCorrectedWS[j], powerCurveKW[j], windVelocity);
	}
	else if (windVelocity == densityCorrectedWS[powerCurveArrayLength - 1])
		out_pwr = powerCurveKW[powerCurveArrayLength - 1];

	// Check against turbine cut-in speed
	if (windVelocity < cutInSpeed) out_pwr = 0.0; //this is effectively redundant, because the power at the cut-in speed is defined to be 0, above, so anything below that will also be 0, but leave in for completeness


	//if (out_pwr > (m_dRatedPower * 0.001)) // if calculated power is > 0.1% of rating, set outputs
	if (out_pwr > 0)
	{
	    if (turbineGross)
	        *turbineGross = out_pwr;
		double pden = 0.5*airDensity*pow(windVelocity, 3.0);
		double area = physics::PI / 4.0*rotorDiameter*rotorDiameter;
		double fPowerCoefficient = max_of(0.0, 1000.0*out_pwr / (pden*area));

		// set outputs to something other than zero
		*turbineOutput = out_pwr;
		if (fPowerCoefficient >= 0.0)
			*thrustCoefficient = max_of(0.0, -1.453989e-2 + 1.473506*fPowerCoefficient - 2.330823*pow(fPowerCoefficient, 2) + 3.885123*pow(fPowerCoefficient, 3));
	} // out_pwr > (rated power * 0.001)

	return;
}


/// Calculates the velocity deficit (% reduction in wind speed) and the turbulence intensity (TI) due to an upwind turbine.
double simpleWakeModel::velDeltaPQ(double radiiCrosswind, double axialDistInRadii, double thrustCoeff, double *newTurbulenceIntensity)
{
	if (radiiCrosswind > 20.0 || *newTurbulenceIntensity <= 0.0 || axialDistInRadii <= 0.0 || thrustCoeff <= 0.0)
		return 0.0;

	double fAddedTurbulence = (thrustCoeff / 7.0)*(1.0 - (2.0 / 5.0)*log(2.0*axialDistInRadii));
	*newTurbulenceIntensity = sqrt(pow(fAddedTurbulence, 2.0) + pow(*newTurbulenceIntensity, 2.0));

	double AA = pow(*newTurbulenceIntensity, 2.0) * pow(axialDistInRadii, 2.0);
	double fExp = max_of(-99.0, (-pow(radiiCrosswind, 2.0) / (2.0*AA)));
	double dVelocityDeficit = (thrustCoeff / (4.0*AA))*exp(fExp);
	return max_of(min_of(dVelocityDeficit, 1.0), 0.0); // limit result from zero to one
}

void simpleWakeModel::wakeCalculations(const double airDensity, const double distanceDownwind[], const double distanceCrosswind[],
	double power[], double eff[], double thrust[], double windSpeed[], double turbulenceIntensity[])
{
	for (size_t i = 1; i < nTurbines; i++) // loop through all turbines, starting with most upwind turbine. i=0 has already been done
	{
		double dDeficit = 1;
		for (size_t j = 0; j < i; j++) // loop through all turbines upwind of turbine[i]
		{
			// distance downwind (axial distance) = distance from turbine j to turbine i along axis of wind direction (units of wind turbine blade radii)
			double fDistanceDownwind = fabs(distanceDownwind[j] - distanceDownwind[i]);

			// separation crosswind (radial distance) between turbine j and turbine i (units of wind turbine blade radii)
			double fDistanceCrosswind = fabs(distanceCrosswind[j] - distanceCrosswind[i]);

			// Calculate the wind speed reduction and turbulence at turbine i, due to turbine j
			// Both the velocity deficit (vdef) and the turbulence intensity (TI) are accumulated over the j loop
			// vdef is accumulated in the code below, using dDeficit
			// TI is accumulated in vel_delta_PQ (it will either add to current TI, or return the same value)
			double vdef = velDeltaPQ(fDistanceCrosswind, fDistanceDownwind, thrust[j], &turbulenceIntensity[i]);
			dDeficit *= (1.0 - vdef);
		}
		windSpeed[i] = windSpeed[i] * dDeficit;
        wTurbine->turbinePower(windSpeed[i], airDensity, &power[i], nullptr, &thrust[i]);
		if (wTurbine->errDetails.length() > 0){
			errDetails = wTurbine->errDetails;
			return;
		}
		eff[i] = wTurbine->calculateEff(power[i], power[0]);
	}
	eff[0] = 100.;
}


/// Returns the area of overlap, NOT a fraction
double parkWakeModel::circle_overlap(double dist_center_to_center, double rad1, double rad2)
{	// Source: http://mathworld.wolfram.com/Circle-CircleIntersection.html, equation 14
	if (dist_center_to_center<0 || rad1<0 || rad2<0)
		return 0;

	if (dist_center_to_center >= rad1 + rad2)
		return 0;

	if (rad1 >= dist_center_to_center + rad2)
		return physics::PI * pow(rad2, 2); // overlap = area of circle 2

	if (rad2 >= dist_center_to_center + rad1)
		return physics::PI * pow(rad1, 2); // overlap = area of circle 1 ( if rad1 is turbine, it's completely inside wake)

	double t1 = pow(rad1, 2) * acos((pow(dist_center_to_center, 2) + pow(rad1, 2) - pow(rad2, 2)) / (2 * dist_center_to_center*rad1));
	double t2 = pow(rad2, 2) * acos((pow(dist_center_to_center, 2) + pow(rad2, 2) - pow(rad1, 2)) / (2 * dist_center_to_center*rad2));
	double t3 = 0.5 * sqrt((-dist_center_to_center + rad1 + rad2) * (dist_center_to_center + rad1 - rad2) * (dist_center_to_center - rad1 + rad2) * (dist_center_to_center + rad1 + rad2));

	return t1 + t2 - t3;
}

/// Calculate the change in wind speed due to wake effects of upwind turbine
double parkWakeModel::delta_V_Park(double Uo, double Ui, double distCrosswind, double distDownwind, double dRadiusUpstream, double dRadiusDownstream, double dThrustCoeff)
{
	// bound the coeff of thrust
	double Ct = max_of(min_of(0.999, dThrustCoeff), minThrustCoeff);

	double k = wakeDecayCoefficient;

	double dRadiusOfWake = dRadiusUpstream + (k * distDownwind); // radius of circle formed by wake from upwind rotor
	double dAreaOverlap = circle_overlap(distCrosswind, dRadiusDownstream, dRadiusOfWake);

	// if there is no overlap, no impact on wind speed
	if (dAreaOverlap <= 0.0) return Uo;

	double dDef = (1 - sqrt(1 - Ct)) * pow(dRadiusUpstream / dRadiusOfWake, 2) * (dAreaOverlap / (physics::PI*dRadiusDownstream*dRadiusDownstream));

	return Ui * (1.0 - dDef);
}

/// Deficit at a downwind turbine is the minimum speed found from all the upwind turbine impacts
void parkWakeModel::wakeCalculations(const double airDensity, const double distanceDownwind[], const double distanceCrosswind[],
	double power[], double eff[], double thrust[], double windSpeed[], double [])
{
	double turbineRadius = wTurbine->rotorDiameter / 2;

	for (size_t i = 1; i < nTurbines; i++) // downwind turbines, i=0 has already been done
	{
		double newSpeed = windSpeed[0];
		for (size_t j = 0; j < i; j++) // upwind turbines
		{
			double distanceDownwindMeters = turbineRadius*fabs(distanceDownwind[i] - distanceDownwind[j]);
			double distanceCrosswindMeters = turbineRadius*fabs(distanceCrosswind[i] - distanceCrosswind[j]);

			// Calculate the wind speed reduction at turbine i, due turbine [j]
			// keep this new speed if it's less than any other calculated speed
			newSpeed = min_of(newSpeed, delta_V_Park(windSpeed[0], windSpeed[j], distanceCrosswindMeters, distanceDownwindMeters, turbineRadius, turbineRadius, thrust[j]));
		}
		windSpeed[i] = newSpeed;
        wTurbine->turbinePower(windSpeed[i], airDensity, &power[i], nullptr, &thrust[i]);
		if (wTurbine->errDetails.length() > 0){
			errDetails = wTurbine->errDetails;
			return;
		}
		eff[i] = wTurbine->calculateEff(power[i], power[0]);
	}
	eff[0] = 100;
}


double eddyViscosityWakeModel::getVelocityDeficit(int upwindTurbine, double axialDistanceInDiameters)
{	
	// if we're too close, it's just the initial deficit (simplification, but model isn't valid closer than MIN_DIAM_EV to upwind turbine)
	double dDistPastMin = axialDistanceInDiameters - MIN_DIAM_EV; // in diameters
	if (dDistPastMin < 0.0)
		return rotorDiameter * matEVWakeDeficits.at(upwindTurbine, 0);

	double dDistInResolutionUnits = dDistPastMin / axialResolution;
	size_t iLowerIndex = (size_t)dDistInResolutionUnits;
	size_t iUpperIndex = iLowerIndex + 1;

	if (iUpperIndex >= matEVWakeDeficits.ncols())
		return 0.0;

	dDistInResolutionUnits -= iLowerIndex;

	return (matEVWakeDeficits.at(upwindTurbine, iLowerIndex) * (1.0 - dDistInResolutionUnits)) + (matEVWakeDeficits.at(upwindTurbine, iUpperIndex) * dDistInResolutionUnits);	// in meters
}

double eddyViscosityWakeModel::wakeDeficit(int upwindTurbine, double distCrosswind, double distDownwind)
{
	double dDef = getVelocityDeficit(upwindTurbine, distDownwind);
	if (dDef <= 0.0)
		return 0.0;

	double dSteps = 25.0;
	double dCrossWindDistanceInMeters = distCrosswind * rotorDiameter;
	double dWidth = getWakeWidth(upwindTurbine, distDownwind);
	double dRadius = rotorDiameter / 2.0;
	double dStep = rotorDiameter / dSteps;

	double dTotal = 0.0;
	for (double y = dCrossWindDistanceInMeters - dRadius; y <= dCrossWindDistanceInMeters + dRadius; y += dStep)
	{
		dTotal += dDef * exp(-3.56*(((y*y)) / (dWidth*dWidth)));  // exp term ranges from >zero to one
	}

	dTotal /= (dSteps + 1.0); // average of all terms above will be zero to dDef

	return dTotal;
}

double eddyViscosityWakeModel::getWakeWidth(int upwindTurbine, double axialDistanceInDiameters)
{	
	// if we're too close, it's just the initial wake width
	double dDistPastMin = axialDistanceInDiameters - MIN_DIAM_EV; // in diameters
	if (dDistPastMin < 0.0)
		return rotorDiameter * matEVWakeWidths.at(upwindTurbine, 0);

	double dDistInResolutionUnits = dDistPastMin / axialResolution;
	int iLowerIndex = (int)dDistInResolutionUnits;
	size_t iUpperIndex = iLowerIndex + 1;
	dDistInResolutionUnits -= iLowerIndex;

	if (iUpperIndex >= matEVWakeWidths.ncols())
		return 0.0;

	return rotorDiameter * max_of(1.0, (matEVWakeWidths.at(upwindTurbine, iLowerIndex) * (1.0 - dDistInResolutionUnits) + matEVWakeWidths.at(upwindTurbine, iUpperIndex) * dDistInResolutionUnits));	// in meters
}

double eddyViscosityWakeModel::addedTurbulenceIntensity(double Ct, double deltaX)
{
	if (deltaX == 0) return 0.0; 
	return max_of(0.0, (Ct / 7.0)*(1.0 - (2.0 / 5.0)*log(deltaX / rotorDiameter)));
}

void eddyViscosityWakeModel::nearWakeRegionLength(double U, double Ii, double Ct, double, VMLN& vmln)
{
	// Ii is incident TI in percent at upstream turbine
	Ct = max_of(min_of(0.999, Ct), minThrustCoeff);

	// these formulae can be found in Wind Energy Handbook by Bossanyi, pages 36 and 37
	// although there are errors in that book so it has been supplemented from the original work  
	// by Vermeulen, P.E.J.  TNO - report
	double dr_dx;

	double m = 1.0 / sqrt(1.0 - Ct);

	double r0 = 0.5*rotorDiameter*sqrt((m + 1.0) / 2.0);

	double t1 = sqrt(0.214 + 0.144*m);
	double t2 = sqrt(0.134 + 0.124*m);

	double n = (t1*(1.0 - t2)) / ((1.0 - t1)*t2);

	double dr_dx_A = Ii < 2.0 ? 0.05*Ii : 0.025*Ii + 0.05; // from original TNO report

	double dr_dx_M = ((1.0 - m)*sqrt(1.49 + m)) / ((1.0 + m)*9.76);

	double dr_dx_L = 0.012*(double)nBlades * wTurbine->tipSpeedRatio(U);

	dr_dx = sqrt(dr_dx_A*dr_dx_A + dr_dx_M*dr_dx_M + dr_dx_L*dr_dx_L);		// wake growth rate

	/////////////////////////////////////////////////////////

	vmln.m = m;

	vmln.diam = rotorDiameter;

	vmln.Xh = r0 / (dr_dx); // end of region 1

	vmln.Xn = n*vmln.Xh;	// end of region 2

	return;

	//	this part not fully used just now but its coded and it could be used in future enhancements
	//vmln.Xf = 5.0*vmln.Xn; // end of region 3
	//vmln.Ro = r0;
	//double c1 = 0.416 + 0.134*m;
	//double c2 = 0.021*(1.0+0.8*m-0.45*m*m);
	//vmln.Rh = r0*((-c1+sqrt(c1*c1+4.0*c2))/(2.0*c2)); // A
	//vmln.Rn = r0 + n*(vmln.Rh-r0);
	//vmln.dUc_Uinf_Xn = ((m-1.0)/m)*((-0.258*m + sqrt(0.066564*m*m + 0.536*(1.0-m)*(vmln.Ro/vmln.Rn)*(vmln.Ro/vmln.Rn)))/(0.268*(1.0-m))); // A-16
	//vmln.Rf = m_dRotorDiameter*(sqrt(m*m-1.0)/0.882*m) * (1.0/sqrt(0.353*vmln.dUc_Uinf_Xn - 0.0245*vmln.dUc_Uinf_Xn*vmln.dUc_Uinf_Xn));
}

double eddyViscosityWakeModel::simpleIntersect(double distToCenter, double radiusTurbine, double radiusWake)
{	// returns the fraction of overlap, NOT an area
	if (distToCenter<0 || radiusTurbine<0 || radiusWake<0)
		return 0;

	if (distToCenter > radiusTurbine + radiusWake)
		return 0;

	if (radiusWake >= distToCenter + radiusTurbine)
		return 1; // turbine completely inside wake

	return min_of(1.0, max_of(0.0, (radiusTurbine + radiusWake - distToCenter) / (2 * radiusTurbine)));
}

double eddyViscosityWakeModel::totalTurbulenceIntensity(double ambientTI, double additionalTI, double Uo, double Uw, double partial)
{
	if (Uw <= 0.0)
		return ambientTI;

	double f = max_of(0.0, ambientTI*ambientTI + additionalTI*additionalTI);
	f = sqrt(f)*Uo / Uw;
	return (1.0 - partial)*ambientTI + partial*f;
	//	return f;
}

bool eddyViscosityWakeModel::fillWakeArrays(int turbineIndex, double ambientVelocity, double velocityAtTurbine, double power, double thrustCoeff, double turbulenceIntensity, double metersToFurthestDownwindTurbine) {
	if (power <= 0.0)
		return true; // no wake effect - wind speed is below cut-in, or above cut-out

	if (thrustCoeff <= 0.0)
		return true; // i.e. there is no wake (both arrays were initialized with zeros, so they just stay that way)

	thrustCoeff = max_of(min_of(0.999, thrustCoeff), minThrustCoeff);

	turbulenceIntensity = min_of(turbulenceIntensity, 50.0); // to avoid turbines with high TIs having no wake

	double Dm, Dmi;

	// Von Karman constant
	const double K = 0.4; 										// Ainslee 1988 (notation)

																// dimensionless constant K1
	const double K1 = 0.015;									// Ainslee 1988 (page 217: input parameters)

	double F, x = MIN_DIAM_EV; // actual distance in rotor diameters

							   // Filter function F
	if (x >= 5.5 || !useFilterFx)
		F = 1.0;
	else
		x < 4.5 ? F = 0.65 - pow(-(x - 4.5) / 23.32, 1.0 / 3.0) : F = 0.65 + pow((x - 4.5) / 23.32, 1.0 / 3.0);

	// calculate the ambient eddy viscocity term
	double Km = F*K*K*turbulenceIntensity / 100.0;  // also known as the ambient eddy viscosity???

													 // calculate the initial centreline velocity deficit at 2 rotor diameters downstream
	Dm = Dmi = max_of(0.0, thrustCoeff - 0.05 - ((16.0*thrustCoeff - 0.5)*turbulenceIntensity / 1000.0));		// Ainslee 1988 (5)

	if (Dmi <= 0.0)
		return true;

	double Uc = velocityAtTurbine - Dmi*velocityAtTurbine; // assuming Uc is the initial centreline velocity at 2 diameters downstream

															 // now make Dmi relative to the freestream
	Dm = Dmi = (ambientVelocity - Uc) / ambientVelocity;

	// calculate the initial (2D) wake width (1.89 x the half-width of the guassian profile
	double Bw = sqrt(3.56*thrustCoeff / (8.0*Dmi*(1.0 - 0.5*Dmi)));			// Ainslee 1988 (6)
																				// Dmi must be as a fraction of dAmbientVelocity or the above line would cause an error sqrt(-ve)
																				// Bw must be in rotor diameters.

																				// the eddy viscosity is then
	double E = F*K1*Bw*Dm*EV_SCALE + Km;

	// Start major departure from Eddy-Viscosity solution using Crank-Nicolson
	std::vector<double> m_d2U(matEVWakeDeficits.ncols());
	m_d2U[0] = EV_SCALE*(1.0 - Dmi);

	matEVWakeDeficits.at(turbineIndex, 0) = Dmi;
	matEVWakeWidths.at(turbineIndex, 0) = Bw;

	// j = 0 is initial conditions, j = 1 is the first step into the unknown
	//	int iterations = 5;
	for (size_t j = 0; j<matEVWakeDeficits.ncols() - 1; j++)
	{
		x = MIN_DIAM_EV + (double)(j)* axialResolution;

		// deficit = Dm at the beginning of each timestep

		if (x >= 5.5 || !useFilterFx)
			F = 1.0;
		else
			x < 4.5 ? F = 0.65 - pow(-(x - 4.5) / 23.32, 1.0 / 3.0) : F = 0.65 + pow((x - 4.5) / 23.32, 1.0 / 3.0); // for some reason pow() does not deal with -ve numbers even though excel does

		Km = F*K*K*turbulenceIntensity / 100.0;

		// first calculate the eddy viscosity
		E = F*K1*Bw*(Dm*EV_SCALE) + Km;

		// calculate the change in velocity at distance x downstream
		double dUdX = 16.0*(pow(m_d2U[j], 3.0) - pow(m_d2U[j], 2.0) - m_d2U[j] + 1.0)*E / (m_d2U[j] * thrustCoeff);
		m_d2U[j + 1] = m_d2U[j] + dUdX*axialResolution;

		// calculate Dm at distance X downstream....
		Dm = (EV_SCALE - m_d2U[j + 1]) / EV_SCALE;

		// now calculate wake width using Dm
		Bw = sqrt(3.56*thrustCoeff / (8.0*Dm*(1.0 - 0.5*Dm)));

		// ok now store the answers for later use	
		matEVWakeDeficits.at(turbineIndex, j + 1) = Dm; // fractional deficit
		matEVWakeWidths.at(turbineIndex, j + 1) = Bw; // diameters

														// if the deficit is below min (a setting), or distance x is past the furthest downstream turbine, or we're out of room to store answers, we're done
		if (Dm <= minDeficit || x > metersToFurthestDownwindTurbine + axialResolution || j >= matEVWakeDeficits.ncols() - 2)
			break;
	}
	return true;
}


/// Simplified Eddy-Viscosity model as per "Simplified Solution To The Eddy Viscosity Wake Model" - 2009 by Dr Mike Anderson of RES
void eddyViscosityWakeModel::wakeCalculations(/*INPUTS */ const double air_density, const double aDistanceDownwind[], const double aDistanceCrosswind[],
	/*OUTPUTS*/ double power[], double eff[], double Thrust[], double adWindSpeed[], double aTurbulence_intensity[])
{
	double dTurbineRadius = rotorDiameter / 2;
	matEVWakeDeficits.fill(0.0);
	matEVWakeWidths.fill(0.0);
	std::vector<VMLN> vmln(nTurbines);
	std::vector<double> Iamb(nTurbines, turbulenceCoeff);

	// Note that this 'i' loop starts with i=0, which is necessary to initialize stuff for turbine[0]
	for (size_t i = 0; i<nTurbines; i++) // downwind turbines, but starting with most upwind and working downwind
	{
		double dDeficit = 0, Iadd = 0, dTotalTI = aTurbulence_intensity[i];
		//		double dTOut=0, dThrustCoeff=0;
		for (size_t j = 0; j<i; j++) // upwind turbines - turbines upwind of turbine[i]
		{
			// distance downwind = distance from turbine i to turbine j along axis of wind direction
			double dDistAxialInDiameters = fabs(aDistanceDownwind[i] - aDistanceDownwind[j]) / 2.0;
			if (fabs(dDistAxialInDiameters) <= 0.0001)
				continue; // if this turbine isn't really upwind, move on to the next

			// separation crosswind between turbine i and turbine j
			double dDistRadialInDiameters = fabs(aDistanceCrosswind[i] - aDistanceCrosswind[j]) / 2.0;

			double dWakeRadiusMeters = getWakeWidth((int)j, dDistAxialInDiameters);  // the radius of the wake
			if (dWakeRadiusMeters <= 0.0)
				continue;

			// calculate the wake deficit
			double dDef = wakeDeficit((int)j, dDistRadialInDiameters, dDistAxialInDiameters);
			double dWindSpeedWaked = adWindSpeed[0] * (1 - dDef); // wind speed = free stream * (1-deficit)

			// keep it if it's bigger
			dDeficit = max_of(dDeficit, dDef);

			Iadd = addedTurbulenceIntensity( Thrust[j], dDistAxialInDiameters*rotorDiameter );

			double dFractionOfOverlap = simpleIntersect(dDistRadialInDiameters*rotorDiameter, dTurbineRadius, dWakeRadiusMeters);
			dTotalTI = max_of(dTotalTI, totalTurbulenceIntensity(aTurbulence_intensity[i], Iadd, adWindSpeed[0], dWindSpeedWaked, dFractionOfOverlap));
		}
		// use the max deficit found to calculate the turbine output
		adWindSpeed[i] = adWindSpeed[0] * (1 - dDeficit);
		aTurbulence_intensity[i] = dTotalTI;
        wTurbine->turbinePower(adWindSpeed[i], air_density, &power[i], nullptr, &Thrust[i]);
		if (wTurbine->errDetails.length() > 0){
			errDetails = wTurbine->errDetails;
			return;
		}
		eff[i] = wTurbine->calculateEff(power[i], power[0]);

		// now that turbine[i] wind speed, output, thrust, etc. have been calculated, calculate wake characteristics for it, because downwind turbines will need the info
		if (!fillWakeArrays((int)i, adWindSpeed[0], adWindSpeed[i], power[i], Thrust[i], aTurbulence_intensity[i], fabs(aDistanceDownwind[nTurbines - 1] - aDistanceDownwind[i])*dTurbineRadius))
		{
			if (errDetails.length() == 0) errDetails = "Could not calculate the turbine wake arrays in the Eddy-Viscosity model.";
		}
		nearWakeRegionLength(adWindSpeed[i], Iamb[i], Thrust[i], air_density, vmln[i]);
	}
}

void constantWakeModel::wakeCalculations(const double airDensity, const double [], const double [],
                                       double power[], double eff[], double thrust[], double windSpeed[], double [])
{
    double turbPower = 0., turbThrust = 0.;
    wTurbine->turbinePower(windSpeed[0], airDensity, &turbPower, nullptr, &turbThrust);
    if (wTurbine->errDetails.length() > 0){
        errDetails = wTurbine->errDetails;
        return;
    }
    turbPower *= derate;
    for (size_t i = 0; i < nTurbines; i++) // loop through all turbines, starting with most upwind turbine. i=0 has already been done
    {
        power[i] = turbPower;
        thrust[i] = turbThrust;
        eff[i] = 100.;
    }
}
