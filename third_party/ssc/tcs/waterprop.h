#ifndef __waterprop_h
#define __waterprop_h

#if defined(WIN32)&&defined(MAKE_DLL)
#define WPEXPORT __declspec(dllexport)
#else
#define WPEXPORT
#endif


struct _property_info
{
	double T; /* temperature ('C) */
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
	double visc; /* viscosity (Pa-s or kg/ms-s), only available at a quality of 0 or 1 */
	double ssnd; /* speed of sound in fluid (m/s), only available at a quality of 0 or 1 */
};

typedef struct _property_info property_info;

/// property functions return 0 on success, non-zero values are error codes 
WPEXPORT int water_TQ( double T, double Q, property_info *data );
WPEXPORT int water_PQ( double P, double Q, property_info *data );
WPEXPORT int water_TP( double T, double P, property_info *data );
WPEXPORT int water_PH( double P, double H, property_info *data );
WPEXPORT int water_PS( double P, double S, property_info *data );

/// index notes:
/// VaporGap::firstCoefIndex = water_vapor_entr_index_vector[pIndex] - pIndex;
/// LiquidGap::lastCoefIndex = water_liquid_entr_index_vector[pIndex+1] - pIndex -2;



#endif

