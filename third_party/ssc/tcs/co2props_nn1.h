#ifndef __co2_h
#define __co2_h

struct _property_info
{
	double T; /* temperature (K) */
	double Q; /* quality [0..1] */
	double P; /* pressure (kPa) */
	double V; /* specific volume (m3/kg) */
	double U; /* internal energy (kJ/kg) */
	double H; /* enthalpy (kJ/kg) */
	double S; /* entropy (kJ/kg-K) */
	double dens; /* density (kg/m3) */
	double Cv; /* specific heat at const. volume (kJ/kg-K), only available at a quality of 0 or 1 */
	double Cp; /* specific heat at const. pressure (kJ/kg-K), only available at a quality of 0 or 1 */
	double cond; /* thermal conductivity (W/m-K), only available at a quality of 0 or 1 */
	double visc; /* viscosity (Pa-s or kg/m-s)*10-6, only available at a quality of 0 or 1 */
	double ssnd; /* speed of sound in fluid (m/s), only available at a quality of 0 or 1 */
};

typedef struct _property_info property_info;

/* property functions return 0 on success, non-zero values are error codes */

int co2_TD( double T, double D, property_info *data );
int co2_TP( double T, double P, property_info *data );
int co2_PH( double P, double H, property_info *data );
int co2_PS( double P, double S, property_info *data );
int co2_HS( double H, double S, property_info *data );

#endif

