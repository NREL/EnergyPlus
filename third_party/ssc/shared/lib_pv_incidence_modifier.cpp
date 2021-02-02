#include <math.h>

#include "lib_pv_incidence_modifier.h"

///Supporting function for IAM functions to calculate transmissance of a module cover at a specific angle- reference: duffie & beckman, Ch 5.3
double transmittance(double theta1_deg, /* incidence angle of incoming radiation (deg) */
	double n_cover,  /* refractive index of cover material, n_glass = 1.586 */
	double n_incoming, /* refractive index of incoming material, typically n_air = 1.0 */
	double k,        /* proportionality constant assumed to be 4 (1/m) for derivation of Bouguer's law (set to zero to skip bougeur's law */
	double l_thick,  /* material thickness (set to zero to skip Bouguer's law */
	double *_theta2_deg) /* thickness of cover material (m), usually 2 mm for typical module */
{
	double theta1 = theta1_deg * M_PI / 180.0;
	double theta2 = asin(n_incoming / n_cover * sin(theta1)); // snell's law, assuming n_air = 1.0
															  // fresnel's equation for non-reflected unpolarized radiation as an average of perpendicular and parallel components
	double tr = 1 - 0.5 *
		(pow(sin(theta2 - theta1), 2) / pow(sin(theta2 + theta1), 2)
			+ pow(tan(theta2 - theta1), 2) / pow(tan(theta2 + theta1), 2));

	if (_theta2_deg) *_theta2_deg = theta2 * 180 / M_PI;

	return tr * exp(-k * l_thick / cos(theta2));
}

///Incidence angle modifier not normalized relative to normal incidence (used as a supporting function to normalized IAM function)
double iam_nonorm(double theta, bool ar_glass)
{
	if (theta < AOI_MIN) theta = AOI_MIN;
	if (theta > AOI_MAX) theta = AOI_MAX;

	if (ar_glass)
	{
		double theta2 = 1;
		double tau_coating = transmittance(theta, n_arc, n_air, k_arc, l_arc, &theta2);
		double tau_glass = transmittance(theta2, n_glass, n_arc, k_glass, l_glass);
		return tau_coating * tau_glass;
	}
	else
	{
		return transmittance(theta, n_glass, n_air, k_glass, l_glass);
	}
}

///Incidence angle modifier normalized relative to normal incidence- used by 61853 model and PVWatts
double iam(double theta, bool ar_glass) //jmf- we should rename this to something more descriptive
{
	if (theta < AOI_MIN) theta = AOI_MIN;
	if (theta > AOI_MAX) theta = AOI_MAX;

	double normal = iam_nonorm(1, ar_glass);
	double actual = iam_nonorm(theta, ar_glass);
	return actual / normal;
}

///Only used in a test to compare against bifacial model
double iamSjerpsKoomen(double n2, double incidenceAngleRadians)
{
	//  Only calculates valid value for 0 <= inc <= 90 degrees
	double cor = -9999.0;   

		// Reflectance at normal incidence, Beckman p217
		double r0 = pow((n2 - 1.0) / (n2 + 1), 2);	

		if (incidenceAngleRadians == 0) {	
			cor = 1.0;		
		}
		else if (incidenceAngleRadians > 0.0 && incidenceAngleRadians <= M_PI / 2.0) {

			double refrAng = asin(sin(incidenceAngleRadians) / n2); 
			double r1 = (pow(sin(refrAng - incidenceAngleRadians), 2.0) / pow(sin(refrAng + incidenceAngleRadians), 2.0));
			double r2 = (pow(tan(refrAng - incidenceAngleRadians), 2.0) / pow(tan(refrAng + incidenceAngleRadians), 2.0));
			cor = 1.0 - 0.5 * (r1 + r2);
			cor /= 1.0 - r0;	
		}
		return cor;
}

///DeSoto IAM model used by CEC model
double calculateIrradianceThroughCoverDeSoto(double theta, double theta_z, double tilt, double G_beam, double G_sky, double G_gnd, bool antiReflectiveGlass)
{
	// establish limits on incidence angle and zenith angle
	if (theta < 1) theta = 1;
	if (theta > 89) theta = 89;

	if (theta_z > 86.0) theta_z = 86.0; // !Zenith angle must be < 90 (?? why 86?)
	if (theta_z < 0) theta_z = 0; 

	// transmittance at angle normal to surface (0 deg), use 1 (deg) to avoid numerical probs.
	double tau_norm = transmittance(1.0, n_glass, 1.0, k_glass, l_glass);

	// transmittance of beam radiation, at incidence angle
	double theta_after_coating = theta;
	double tau_beam = 1.0;
	if (antiReflectiveGlass)
	{
		double tau_coating = transmittance(theta, n_arc, 1.0, k_arc, l_arc, &theta_after_coating);
		tau_beam *= tau_coating;
	}
	tau_beam *= transmittance(theta_after_coating, n_glass, (antiReflectiveGlass ? n_arc : 1.0), k_glass, l_glass);

	// transmittance of sky diffuse, at modified angle by (D&B Eqn 5.4.2)
	double theta_sky = 59.7 - 0.1388*tilt + 0.001497*tilt*tilt;
	double tau_sky = transmittance(theta_sky, n_glass, 1.0, k_glass, l_glass);

	// transmittance of ground diffuse, at modified angle by (D&B Eqn 5.4.1)
	double theta_gnd = 90.0 - 0.5788*tilt + 0.002693*tilt*tilt;
	double tau_gnd = transmittance(theta_gnd, n_glass, 1.0, k_glass, l_glass);

	// calculate component incidence angle modifiers, D&B Chap. 5 eqn 5.12.1, DeSoto'04
	double Kta_beam = tau_beam / tau_norm;
	double Kta_sky = tau_sky / tau_norm;
	double Kta_gnd = tau_gnd / tau_norm;

	// total effective irradiance absorbed by solar cell
	double Geff_total = G_beam * Kta_beam + G_sky * Kta_sky + G_gnd * Kta_gnd;

	if (Geff_total < 0) Geff_total = 0;

	return Geff_total;
}
