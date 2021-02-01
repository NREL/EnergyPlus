#include "waterprop.h"

#include <vector>
#include <cmath>
#include <algorithm>
#include <complex>
#include <assert.h>

#include "waterprop_critical.dat"
#include "waterprop_saturated.dat"

///  common operations
static int VaporNonGap(int pIndex, double pFraction, property_info *data, int edgeInd, double indVar );
static void LiquidNonGap(int pIndex, double pFraction, property_info *data, int edgeInd, double indVar );
static void VaporGap(int firstCoefIndex, double TSat, double sFraction,property_info *data);
static void LiquidGap(int lastCoefIndex, double TSat, double sFraction,property_info *data);

/// common helper functions
static double cubic_solution(double coef_1, double coef_2, double coef_3, double coef_4, double value);
static double t_sat(double P);
static void sat_find(double T, int *TSatIndex, double *dT);
static void pres_find(double P, int *pSatIndex, double *pFraction);

/// internal helpers
static double vaporGrid( int j, int k, int firstCoefIndex, double pFraction);
static double vaporGrid( int k, int lastCoefIndex, double pFraction, double sFraction = 1.);
static double liquidGrid( int j, int k, int firstCoefIndex, double pFraction);
static double liquidGrid(int k,int lastCoefIndex, double pFraction, double sFraction=1.);
static double propDome( int j, int k, int TSatIndex, double dT);
static double VHSDome(int i, int j, int TSatIndex, double dT);
static int maxloc(int firstIndex, int lastIndex, double indexVector[], double S );
static int maxloc( std::vector<double> &valueVector, double S);


/// gap functions
/// Returns the temperature in the vapor gap given the entropy fractional position
inline double vapor_gap_temp(double sFraction, double pFraction, double TSat, int firstCoefIndex)
{
	/// linear indices j = 0, k = 0 
	double grid = vaporGrid(0,0,firstCoefIndex, pFraction);
	return( TSat + (grid-TSat)*sFraction);
}
/// Returns the density in the vapor gap given the entropy fractional position 
inline double vapor_gap_dens(double sFraction, double pFraction, double dT, int TSatIndex, int firstCoefIndex)
{

	/// linear indices i = 0, j = 1
	double dome = 1.0/(VHSDome(0, 1, TSatIndex, dT));

	/// linear indices j = 0, k = 1 
	double grid = vaporGrid(0,1,firstCoefIndex, pFraction);

	return (dome + (grid-dome)*sFraction);
}
/// Returns the enthalpy in the vapor gap given the entropy fractional position
inline double vapor_gap_enth(double sFraction, double pFraction, double dT, int TSatIndex, int firstCoefIndex)
{
	/// linear indices i = 1, j = 1 
	double dome = VHSDome(1, 1, TSatIndex, dT);

	/// linear indices j = 0, k = 2 
	double grid = vaporGrid(0,2,firstCoefIndex, pFraction);

	return (dome + (grid-dome)*sFraction);
}
/// Returns Cv in the vapor gap given the entropy fractional position
inline double vapor_gap_cv(double sFraction, double pFraction, double dT, int TSatIndex, int firstCoefIndex)
{
	/// linear indices using j = 0, k = 1
	double dome = propDome( 0, 1, TSatIndex, dT);

	/// linear indices j = 0, k = 3 
	double grid = vaporGrid(0,3,firstCoefIndex, pFraction);

	return (dome + (grid-dome)*sFraction);
}
/// Returns Cp in the vapor gap given the entropy fractional position
inline double vapor_gap_cp(double sFraction, double pFraction, double dT, int TSatIndex, int firstCoefIndex)
{
	/// linear indices using j = 1, k = 1
	double dome = propDome( 1, 1, TSatIndex, dT);

	/// linear indices j = 0, k = 4 
	double grid = vaporGrid(0,4,firstCoefIndex, pFraction);

	return (dome + (grid-dome)*sFraction);
}
/// Returns ssnd in the vapor gap given the entropy fractional position
inline double vapor_gap_ssnd(double sFraction, double pFraction, double dT, int TSatIndex, int firstCoefIndex)
{
	/// linear indices using j = 2, k = 1
	double dome = propDome(2, 1, TSatIndex, dT);

	/// linear indices j = 0, k = 5 
	double grid = vaporGrid(0,5,firstCoefIndex, pFraction);

	return (dome + (grid-dome)*sFraction);
}
/// Returns cond in the vapor gap given the entropy fractional position
inline double vapor_gap_cond(double sFraction, double pFraction, double dT, int TSatIndex, int firstCoefIndex)
{
	/// linear indices using j = 3, k = 1
	double dome = propDome( 3, 1, TSatIndex, dT);

	/// return linear indices j = 0, k = 6 (R = 4, C = 4, D = 8)
	double grid = vaporGrid(0,6,firstCoefIndex, pFraction);

	return (dome + (grid-dome)*sFraction);
}
/// Returns visc in the vapor gap given the entropy fractional position
inline double vapor_gap_visc(double sFraction, double pFraction, double dT, int TSatIndex, int firstCoefIndex)
{
	/// linear indices j = 4, k = 1
	double dome = propDome(4, 1, TSatIndex, dT);

	/// linear indices j = 0, k = 7 
	double grid = vaporGrid(0,7,firstCoefIndex, pFraction);

	return (dome + (grid-dome)*sFraction);
}
/// Returns entropy in the vapor gap given the entropy fractional position
inline double vapor_gap_entr(double sFraction, double dT, int TSatIndex, int pIndex)
{
	/// linear indices i = 2, j = 1 
	double dome = VHSDome(2, 1, TSatIndex, dT);

	return (dome + (water_vapor_entr_values[water_vapor_entr_index_vector[pIndex]] - dome )*sFraction);
}
/// Returns temp in the liquid gap given the entropy fractional position
inline double liquid_gap_temp(double sFraction, double pFraction, double TSat, int lastCoefIndex)
{
	/// linear index k = 0 (R = 4, C = 4, D = 8)
	double grid = liquidGrid(0,lastCoefIndex,pFraction,1.);
	return (grid + (TSat - grid)*sFraction);
}
/// Returns density in the liquid gap given the entropy fractional position
inline double liquid_gap_dens(double sFraction, double pFraction, double dT, int TSatIndex, int lastCoefIndex)
{
	/// linear indices i = 0, j = 0 
	double dome = 1.0/(VHSDome(0, 0, TSatIndex, dT));

	/// linear index k = 1 (R = 4, C = 4, D = 8)
	double grid = liquidGrid(1,lastCoefIndex,pFraction,1.);

	return (grid + (dome-grid)*sFraction);
}
/// Returns enthalpy in the liquid gap given the entropy fractional position
inline double liquid_gap_enth(double sFraction, double pFraction, double dT, int TSatIndex, int lastCoefIndex)
{
	/// linear indices i = 1, j = 0 
	double dome = VHSDome(1, 0, TSatIndex, dT);

	/// linear index k = 2 (R = 4, C = 4, D = 8)
	double grid = liquidGrid(2,lastCoefIndex,pFraction,1.);

	return (grid + (dome - grid)*sFraction);
}
/// Returns Cv in the liquid gap given the entropy fractional position
inline double liquid_gap_cv(double sFraction, double pFraction, double dT, int TSatIndex, int lastCoefIndex)
{
	/// dome indices j = 0, k = 0
	double dome = propDome( 0, 0, TSatIndex, dT);

	/// linear index k = 3 (R = 4, C = 4, D = 8)
	double grid = liquidGrid(3,lastCoefIndex,pFraction,1.);

	return (grid + (dome - grid)*sFraction);
}
/// Returns Cp in the liquid gap given the entropy fractional position
inline double liquid_gap_cp(double sFraction, double pFraction, double dT, int TSatIndex, int lastCoefIndex)
{
	/// dome indices j = 1, k = 0 
	double dome = propDome( 1, 0, TSatIndex, dT);

	/// linear index k = 4 (R = 4, C = 4, D = 8)
	double grid = liquidGrid(4,lastCoefIndex,pFraction,1.);

	return (grid + (dome - grid)*sFraction);
}
/// Returns ssnd in the liquid gap given the entropy fractional position
inline double liquid_gap_ssnd(double sFraction, double pFraction, double dT, int TSatIndex, int lastCoefIndex)
{
	/// dome indices j = 2, k = 0
	double dome = propDome( 2, 0, TSatIndex, dT);

	/// linear index k = 5 (R = 4, C = 4, D = 8)
	double grid = liquidGrid(5,lastCoefIndex,pFraction,1.);

	return (grid + (dome - grid)*sFraction);
}
/// Returns cond in the liquid gap given the entropy fractional position
inline double liquid_gap_cond(double sFraction, double pFraction, double dT, int TSatIndex, int lastCoefIndex)
{
	/// dome indices j = 3, k = 0
	double dome = propDome( 3, 0, TSatIndex, dT);

	/// linear index k = 6 (R = 4, C = 4, D = 8)
	double grid = liquidGrid(6,lastCoefIndex,pFraction,1.);

	return (grid + (dome - grid)*sFraction);
}
/// Returns viscosity in the liquid gap given the entropy fractional position
inline double liquid_gap_visc(double sFraction, double pFraction, double dT, int TSatIndex, int lastCoefIndex)
{
	/// dome indices j = 4, k = 0
	double dome = propDome( 4, 0, TSatIndex, dT);

	/// linear index k = 7 (R = 4, C = 4, D = 8)
	double grid = liquidGrid(7,lastCoefIndex,pFraction,1.);

	return (grid + (dome - grid)*sFraction);
}
/// Returns entropy in the liquid gap given the entropy fractional position
inline double liquid_gap_entr(double sFraction, double dT, int TSatIndex, int pIndex)
{
	/// dome indices i = 2, j = 0 
	double dome = VHSDome(2, 0, TSatIndex,  dT);

	int lastIndex = water_liquid_entr_index_vector[pIndex+1]-1;

	return (water_liquid_entr_values[lastIndex] + (dome - water_liquid_entr_values[lastIndex])*sFraction);

}



/// indexing/**********************************************************************//**
/* Purpose is to return an index to a 1D array for multidimensional inputs 
* Note: Assumes COLUMN based arrays !!!
* Notation:
* i,j,k,l = indices in the 1st, 2nd, 3rd, 4th dimension
* R = number of elements in first dimension (rows)
* C = number of elements in second dimension (columns)
* D = number of elements in third dimension (depth)
**************************************************************************/

/// 2D indexing
static inline int Index2D(int i, int j, int R)
{
	return( i + R*j);
}
/// 4D indexing
static inline int Index4D(int i, int j, int k, int l, int R, int C, int D)
{
	return ( R*C*D*l +  (R*C*k + (i+R*j) ) );
}


/// water_TQ
WPEXPORT int water_TQ( double T, double Q, property_info *data )
{
	/************************************************************
	/// INPUTS
	/// T - temperature (C)
	/// Q - Quality (mass basis)
	///		: Q < 0 (subcooled liquid) Q > 1 (superheated vapor)
	///		: Q = 0 (saturated liquid) Q = 1 (saturated vapor)
	///     : Q >= 999 (supercritical state)
	/// OUTPUTS
	/// error_code 
	///		: 1 (quality not in saturated region)
	///		: 2 (temperature outside saturation bounds)
	///  data - the property_info structure
	*************************************************************/

	/// initialize
	(*data).T = T;
	(*data).Q = Q;

	/// error code
	int error_code = 0;
	
	/// quality check
	if (Q > 1.0 || Q <0.0) 
	{
		error_code = 1;
		return error_code;
	}

	/// temperature check
	if (T > water_max_sat_temp || T < water_min_sat_temp )
	{
		error_code = 2;
		return error_code;
	}

	/// locate interval containing T and compute dT
	int TSatIndex;
	double dT;
	sat_find(T,&TSatIndex,&dT);

	/// calculate sat liq and vapor vals for volume, enthalphy, entropy
	double sat_vhs_values[3][2]; 
	for (int i = 0; i < 3; i++)
	{
		for (int j = 0; j < 2; j++)
		{
			sat_vhs_values[i][j] = VHSDome(i, j, TSatIndex, dT);
		}
	}

	/// compute volume enthalphy entropy
	double vhs_values[3];
	for (int i = 0; i < 3; i++)
	{
		vhs_values[i] = Q*(sat_vhs_values[i][1] - sat_vhs_values[i][0]) + sat_vhs_values[i][0];
	}

	/// return requested properties valid in two-phase region
	int ind[4]; 
	ind[0] = Index2D(0,TSatIndex,4); ind[1] = Index2D(1,TSatIndex,4);
	ind[2] = Index2D(2,TSatIndex,4); ind[3] = Index2D(3,TSatIndex,4);

	(*data).P = ((water_sat_pres_coef_array[ind[0]]*dT + 
				  water_sat_pres_coef_array[ind[1]])*dT + 
				  water_sat_pres_coef_array[ind[2]])*dT + 
				  water_sat_pres_coef_array[ind[3]];

	(*data).dens = 1./vhs_values[0];
	(*data).V = vhs_values[0];
	(*data).H = vhs_values[1];
	(*data).S = vhs_values[2];
	(*data).U = (*data).H - (*data).P*(*data).V;
	
	/// property check
	double propValues[5] = {0.,0.,0.,0.,0.};
	if ( Q == 0.0 )
	{
		for (int i = 0; i < 5; i++) propValues[i] = propDome(i,0,TSatIndex,dT);
	}
	else if ( Q == 1.0 )
	{
		for (int i = 0; i < 5; i++) propValues[i] = propDome(i,1,TSatIndex,dT);	
	}
	(*data).Cv = propValues[0];
	(*data).Cp = propValues[1];
	(*data).ssnd = propValues[2];
	(*data).cond = propValues[3];
	(*data).visc = propValues[4];

	/// ensure independent data hasn't changed
	(*data).T = T;
	(*data).Q = Q;

	return error_code;
}

/// water_PQ
WPEXPORT int water_PQ( double P, double Q, property_info *data )
{
	/************************************************************
	/// INPUTS
	/// P - Pressure (kPa)
	/// Q - Quality (mass basis)
	///		: Q < 0 (subcooled liquid) Q > 1 (superheated vapor)
	///		: Q = 0 (saturated liquid) Q = 1 (saturated vapor)
	///     : Q >= 999 (supercritical state)
	/// OUTPUTS
	/// error_code 
	///		: 1 (quality not in saturated region)
	///		: 3 (pressure outside saturation bounds)
	///  data - the property_info structure
	*************************************************************/

	/// initialize
	int error_code = 0;
	(*data).P = P;
	(*data).Q = Q;


	/// check quality input
	if (Q > 1.0 || Q < 0.0) 
	{
		error_code = 1;
		return error_code;
	}

	/// check pressure input
	if (P > water_max_sat_pres || P < water_min_sat_pres)
	{
		error_code = 3;
		return error_code;
	}

	/// calculate TSat at given pressure
	double TSat = t_sat(P);

	/// Find interval containing TSat and compute dT
	int TSatIndex; double dT;
	sat_find(TSat, &TSatIndex, &dT);

	/// calculate sat liquid and vapor vals for volume, enthalphy and entropy
	double sat_vhs_values[3][2]; 
	for (int i = 0; i < 3; i++)
	{
		for (int j = 0; j < 2; j++)
		{
			sat_vhs_values[i][j] = VHSDome(i, j, TSatIndex, dT);
		}
	}

	/// compute volume enthalphy entropy
	double vhs_values[3];
	for (int i = 0; i < 3; i++)
	{
		vhs_values[i] = Q*(sat_vhs_values[i][1] - sat_vhs_values[i][0]) + sat_vhs_values[i][0];
	}

	/// Requested properties valid in two-phase region
	(*data).T = TSat;
	(*data).dens = 1./vhs_values[0];
	(*data).V = vhs_values[0];
	(*data).H = vhs_values[1];
	(*data).S = vhs_values[2];
	(*data).U = vhs_values[1] - P*vhs_values[0]; // U = H - PV

	/// Check to see if properties are requested that are only valid if Q = 0 or 1
	double propValues[5] = {0,0,0,0,0};
	if (Q == 0.0)
	{
		for (int i = 0; i < 5; i++) propValues[i] = propDome(i,0,TSatIndex,dT);
	}
	else if ( Q == 1.0)
	{
		for (int i = 0; i < 5; i++) propValues[i] = propDome(i,1,TSatIndex,dT );
	}

	(*data).Cv = propValues[0];
	(*data).Cp = propValues[1];
	(*data).ssnd = propValues[2];
	(*data).cond = propValues[3];
	(*data).visc = propValues[4];

	/// ensure independent data haven't changed
	(*data).P = P;
	(*data).Q = Q;

	return error_code;
}


WPEXPORT int water_TP( double T, double P, property_info *data )
{
	/************************************************************
	/// INPUTS
	/// T - temperature (C)
	/// P - pressure (kPa)
	/// OUTPUTS
	/// error_code 
	///		: 4 (pressure outside saturation limits)
	///		: 7 (temperature outside saturation bounds)
	///		: 8 (indeterminable state)
	///		: 9 (entropy fraction not between 0 and 1)
	///     :99 (subcritical)
	///  data - the property_info structure
	*************************************************************/

	int error_code = 0;

	/// initialize independent variables
	(*data).P = P;
	(*data).T = T;

	/// check pressure input
	if (P > water_max_sat_pres || P < water_min_sat_pres)
	{
		error_code = 4;
		return error_code;
	}

	/// check temperature input
	if (T < water_min_temp || T > water_max_temp)
	{
		error_code = 7;
		return error_code;
	}

	/// check if saturated or above vapor dome
	if (P <= water_max_sat_pres)
	{
		double TSat = t_sat(P);
		/// check for indeterminable state
		if (T == TSat)
		{
			error_code = 8;
			return error_code;
		}

		/// find pressure_index and fractional position
		int pIndex; double pFraction;
		pres_find(P, &pIndex, &pFraction);

		/// determine state of provided P and S
		if (T > TSat)
		{
			/// ---------------------VAPOR -------------------------------------
			int firstCoefIndex = water_vapor_entr_index_vector[pIndex] - pIndex;

			/// check if T is in gaps between vapor dome and left-most element
			double Tgrid = vaporGrid(0,0,firstCoefIndex,pFraction);
			if (T < Tgrid)
			{
				double sFraction = (T-TSat)/(Tgrid - TSat);
				VaporGap(firstCoefIndex,TSat,sFraction,data);
			}
			else error_code = VaporNonGap(pIndex,pFraction,data, 0, T);
		}
		else
		{
			/// --------------------- LIQUID ----------------------------------------
			int lastCoefIndex = water_liquid_entr_index_vector[pIndex+1] - pIndex - 2; 
			double TGrid = liquidGrid(0,lastCoefIndex,pFraction, 1.);

			/// check if is gap between vapor dome and right-most element
			if (T > TGrid)
			{
				double sFraction = (T - TGrid)/(TSat - TGrid);
				LiquidGap(lastCoefIndex, TSat,sFraction,data);
			}
			else LiquidNonGap(pIndex,pFraction,data,0,T);
		}
	}
	/// subcritical
	else error_code = 99; 

	/// ensure independent variables are the same
	(*data).T = T;
	(*data).P = P;

	return error_code;
}
/// water_PH
WPEXPORT int water_PH( double P, double H, property_info *data )
{
	/************************************************************
	/// INPUTS
	/// P - pressure (kPa)
	/// H - enthalpy (kJ/kg)
	/// OUTPUTS
	/// error_code 
	///		: 4 (pressure outside saturation limits)
	///		: 9 (entropy fraction not between 0 and 1)
	///     :99 (subcritical)
	///  data - the property_info structure
	*************************************************************/
	/// initialize independent variables
	(*data).P = P;
	(*data).H = H;

	int error_code = 0;

	/// check pressure 
	if (P > water_max_sat_pres || P < water_min_sat_pres)
	{
		error_code = 4;
		return error_code;
	}

	if (P <= water_max_sat_pres)
	{
		/// find saturated liquid and vapor enthalpy at the given pressure
		int TSatIndex; double dT;
		double TSat = t_sat(P);
		sat_find(TSat,&TSatIndex,&dT);

		/// find pressure index row which contains P and fractional position
		int pIndex; double pFraction;
		pres_find(P,&pIndex,&pFraction);

		/// Saturation dome
		double HSat[2];
		HSat[0] = VHSDome(1,0,TSatIndex,dT);
		HSat[1] = VHSDome(1,1,TSatIndex,dT);

		/// Determine state of provide P and H
		if (H > HSat[1])
		{
			/// ------------------ VAPOR ---------------------------------------
			int firstCoefIndex = water_vapor_entr_index_vector[pIndex] - pIndex;

			/// check if temperature lies in gap between vapor dome and left-most element
			double HGrid = vaporGrid(0,2,firstCoefIndex,pFraction);
			if (H < HGrid) 
			{
				/// in gap
				double sFraction = (H - HSat[1])/(HGrid-HSat[1]);
				VaporGap(firstCoefIndex,TSat,sFraction,data);
			}
			else error_code = VaporNonGap(pIndex,pFraction,data,2,H);
		}
		else if ( H >= HSat[0])
		{
			/// ---------------TWO-PHASE -------------------

			/// Quality
			(*data).Q = (H - HSat[0])/(HSat[1] - HSat[0]);

			/// Calculate sat liquid and vopor vals
			double sat_vhs_values[3][2];
			double vhs_values[3];

			for (int i = 0; i < 3; i++)
			{
				for (int j = 0; j < 2; j++)
				{
					sat_vhs_values[i][j] = VHSDome(i,j,TSatIndex,dT);
				}
				vhs_values[i] = (*data).Q*(sat_vhs_values[i][1] - sat_vhs_values[i][0]) + sat_vhs_values[i][0];
			}

			/// Compute requested properties
			(*data).T = TSat;
			(*data).V = vhs_values[0];
			(*data).dens = 1./(*data).V;
			(*data).S = vhs_values[2];
			(*data).U = H - (*data).P*(*data).V;

			/// check to see if properties are requested that are only valid if Q = 0 or 1
			double prop_values[5] = {0., 0. , 0. , 0., 0.};
			double Q = (*data).Q;
			if (Q == 0.0)
			{
				for (int i = 0; i < 5; i++)
				{
					prop_values[i] = propDome(i,0,TSatIndex,dT);
				}
			}
			else if (Q == 1.)
			{
				for (int i = 0; i < 5; i++)
				{
					prop_values[i] = propDome(i,1,TSatIndex,dT);
				}
			}
			(*data).Cv = prop_values[0];
			(*data).Cp = prop_values[1];
			(*data).ssnd = prop_values[2];
			(*data).cond = prop_values[3];
			(*data).visc = prop_values[4];
		}
		else
		{
			/// ----------------- LIQUID ------------------------------------------
			int lastCoefIndex = water_liquid_entr_index_vector[pIndex+1] - pIndex -2;
			double HGrid = liquidGrid(2,lastCoefIndex,pFraction,1.);

			/// check if is gap between liquid dome and right-most element
			if (H > HGrid)
			{
				double sFraction = (H - HGrid)/(HSat[0] - HGrid);
				LiquidGap(lastCoefIndex,TSat,sFraction,data);
			}
			else LiquidNonGap(pIndex,pFraction,data,2,H);
		}
	}
	/// supercritical
	else error_code = 99; 

	/// ensure independent variables haven't changed
	(*data).P = P;
	(*data).H = H;

	return error_code;
}
/// water_PS
WPEXPORT int water_PS( double P, double S, property_info *data )
{
	/************************************************************
	/// INPUTS
	/// P - pressure (kPa)
	/// S - entropy (kJ/kg-K)
	/// OUTPUTS
	/// error_code 
	///		: 4 (pressure outside saturation limits)
	///     : 5 (entropy out of bounds on upper limit)
	///		: 6 (entropy out of bounds on lower limit)
	///     :99 (subcritical)
	///  data - the property_info structure
	*************************************************************/

	/// initialize independent variables
	(*data).P = P;
	(*data).S = S;

	int error_code = 0;

	/// Check Pressure Input
	if (P > water_max_sat_pres || P < water_min_sat_pres) 
	{
		error_code = 4;
		return error_code;
	}

	/// Use provide pressure to determine whether saturated or above vapor dome
	if (P <= water_max_sat_pres)
	{ 
		/// determine saturated liquid and vapor entropy at given P
		double TSat = t_sat(P);
		int TSatIndex; double dT;
		sat_find(TSat, &TSatIndex, &dT);
		double SSat[2];

		SSat[0] = VHSDome(2,0,TSatIndex,dT);
		SSat[1] = VHSDome(2,1,TSatIndex,dT);

		/// find pressure index row which contains P and fractional position
		int pIndex; double pFraction;
		pres_find(P,&pIndex,&pFraction);

		if (S > SSat[1])
		{
			/// ---------------- VAPOR --------------------------
			int firstIndex = water_vapor_entr_index_vector[pIndex];

			/// check if temperature lies in gap between vapor dome and left-most element
			if (S < water_vapor_entr_values[firstIndex] )
			{
				/// in gap
				double sFraction = (S - SSat[1])/(water_vapor_entr_values[firstIndex] - SSat[1]);
				int firstCoefIndex = water_vapor_entr_index_vector[pIndex] - pIndex;
				VaporGap(firstCoefIndex,TSat,sFraction,data);
			}
			else
			{
				/// fully vapor
				int lastIndex = water_vapor_entr_index_vector[pIndex+1] - 1;
				int rowInd = maxloc(firstIndex, lastIndex, water_vapor_entr_values,S);
				int sIndex = firstIndex + rowInd;
				if (sIndex >= lastIndex)
				{
					if (S == water_vapor_entr_values[lastIndex])
					{
						--rowInd;
						--sIndex;
					}
					else
					{
						error_code = 5;
						return error_code;
					}
				}

				/// calculate sFraction and coefficient index for element
				double sFraction = (S - water_vapor_entr_values[sIndex])/(water_vapor_entr_values[sIndex+1] - water_vapor_entr_values[sIndex]);
				int coefIndex = firstIndex - pIndex + rowInd;

				/// dependent properties
				(*data).T = vaporGrid(0, coefIndex,pFraction,sFraction);
				(*data).dens = vaporGrid(1,coefIndex,pFraction,sFraction);
				(*data).V = 1./(*data).dens;
				(*data).H = vaporGrid(2,coefIndex,pFraction,sFraction);
				(*data).Cv = vaporGrid(3,coefIndex,pFraction,sFraction);
				(*data).Cp = vaporGrid(4,coefIndex,pFraction,sFraction);
				(*data).ssnd = vaporGrid(5,coefIndex,pFraction,sFraction);
				(*data).cond = vaporGrid(6,coefIndex,pFraction,sFraction);
				(*data).visc = vaporGrid(7,coefIndex,pFraction,sFraction);
				(*data).U = (*data).H - (*data).P*(*data).V;
				(*data).Q = 10;
			}
		}
		else if (S >= SSat[0] )
		{
			/// ------------- TWO-PHASE -----------------
			double Q = (S - SSat[0])/(SSat[1] - SSat[0]);

			/// calculate saturated liquid and vapor values
			double sat_vhs_values[2][2];
			double vhs_values[2];

			for (int i = 0; i < 2; i++)
			{
				for (int j = 0; j < 2; j++)
				{
					sat_vhs_values[i][j] = VHSDome(i,j,TSatIndex,dT);
				}
				vhs_values[i] = Q*(sat_vhs_values[i][1] - sat_vhs_values[i][0]) + sat_vhs_values[i][0];
			}
			(*data).T = TSat;
			(*data).V = vhs_values[0];
			(*data).dens = 1./(*data).V;
			(*data).H = vhs_values[1];
			(*data).U =  (*data).H - (*data).P*(*data).V;
			(*data).Q = Q;

			double prop_values[5] = {0.,0.,0.,0.,0.};
			if (Q == 0.0)
			{
				for (int i = 0; i < 5; i++)
				{
					prop_values[i] = propDome(i,0,TSatIndex,dT);
				}
			}
			else if (Q == 1.0)
			{
				for (int i = 0; i < 5; i++)
				{
					prop_values[i] = propDome(i,1,TSatIndex,dT);
				}
			}
			(*data).Cv = prop_values[0];
			(*data).Cp = prop_values[1];
			(*data).ssnd = prop_values[2];
			(*data).cond = prop_values[3];
			(*data).visc = prop_values[4];

		}
		else
		{
			/// --------------LIQUID---------------------------
			int lastIndex = water_liquid_entr_index_vector[pIndex+1]-1;

			/// check if S lies in gap between vapor dome and right most element
			if (S > water_liquid_entr_values[lastIndex])
			{
				double sFraction = (S-water_liquid_entr_values[lastIndex])/(SSat[0] - water_liquid_entr_values[lastIndex]);
				int lastCoefIndex = water_liquid_entr_index_vector[pIndex+1] - pIndex - 2  ;
				LiquidGap(lastCoefIndex,TSat, sFraction,data);
			}
			else
			{
				/// fully in liquid region
				int firstIndex = water_liquid_entr_index_vector[pIndex];
				int rowInd = maxloc(firstIndex,lastIndex,water_liquid_entr_values,S);
				int sIndex = firstIndex + rowInd ; 
				if (sIndex == lastIndex && S == water_liquid_entr_values[lastIndex])
				{
					--rowInd;
					--sIndex;
				}
				if (S < water_liquid_entr_values[firstIndex])
				{
					error_code = 6;
					return (error_code);
				}

				double sFraction = (S - water_liquid_entr_values[sIndex])/(water_liquid_entr_values[sIndex+1] - water_liquid_entr_values[sIndex]);
				int coefIndex = water_liquid_entr_index_vector[pIndex] - pIndex + rowInd;

				/// dependent properties
				(*data).T = liquidGrid(0,coefIndex,pFraction,sFraction);
				(*data).dens = liquidGrid(1,coefIndex,pFraction,sFraction);
				(*data).V = 1./(*data).dens;
				(*data).H = liquidGrid(2,coefIndex,pFraction,sFraction);
				(*data).Cv = liquidGrid(3,coefIndex,pFraction,sFraction);
				(*data).Cp = liquidGrid(4,coefIndex,pFraction,sFraction);
				(*data).ssnd = liquidGrid(5,coefIndex,pFraction,sFraction);
				(*data).cond = liquidGrid(6,coefIndex,pFraction,sFraction);
				(*data).visc = liquidGrid(7,coefIndex,pFraction,sFraction);
				(*data).U = (*data).H - (*data).P*(*data).V;
				(*data).Q = -10;
			}
		}
	}
	/// supercritical
	else error_code = 99;

	/// ensure data haven't changed
	(*data).P = P;
	(*data).S = S;

	return error_code;

}
/// Generic function for computing non-gap vapor properties
int VaporNonGap(int pIndex, double pFraction, property_info *data, int edgeInd, double indVar )
{
	int error_code = 0;
	
	/// fortranOffset 
	int fortranOffset = - 1;
	int firstCoefIndex = water_vapor_entr_index_vector[pIndex] - pIndex + 1 + fortranOffset;
	int lastCoefIndex = water_vapor_entr_index_vector[pIndex+1] - pIndex- 1 + fortranOffset;

	std::vector<double> edge;
	edge.reserve( lastCoefIndex - firstCoefIndex + 1 );

	for (int i = firstCoefIndex; i <= lastCoefIndex; i++) edge.push_back(vaporGrid(0,edgeInd,i,pFraction));
	int rowInd = maxloc(edge, indVar);
	int coefIndex = firstCoefIndex + rowInd; 

	double pAdjustedCoefs[4];
	for (int i = 0; i < 4; i++) pAdjustedCoefs[i] = vaporGrid(i,edgeInd,coefIndex,pFraction);

	/// solve cubic for sFraction at given T
	double sFraction = cubic_solution(pAdjustedCoefs[0],pAdjustedCoefs[1], pAdjustedCoefs[2], pAdjustedCoefs[3],indVar);
	if (fabs(sFraction) > 1.) return error_code = 9;

	/// calculate S
	int sIndex = water_vapor_entr_index_vector[pIndex] +rowInd;
	(*data).S = water_vapor_entr_values[sIndex] + sFraction*(water_vapor_entr_values[sIndex+1] -water_vapor_entr_values[sIndex]);

	/// dependent properties
	(*data).T = vaporGrid(0,coefIndex,pFraction,sFraction);
	(*data).dens = vaporGrid(1,coefIndex,pFraction,sFraction);
	(*data).V = 1./(*data).dens;
	(*data).H = vaporGrid(2,coefIndex,pFraction,sFraction);
	(*data).Cv = vaporGrid(3,coefIndex,pFraction,sFraction);
	(*data).Cp = vaporGrid(4,coefIndex,pFraction,sFraction);
	(*data).ssnd = vaporGrid(5,coefIndex,pFraction,sFraction);
	(*data).cond = vaporGrid(6,coefIndex,pFraction,sFraction);
	(*data).visc = vaporGrid(7,coefIndex,pFraction,sFraction);
	(*data).U = (*data).H - (*data).P*(*data).V;
	(*data).Q = 10 ;

	return error_code;
}
/// Generic function for computing non-gap liquid properties
void LiquidNonGap(int pIndex, double pFraction, property_info *data, int edgeInd, double indVar )
{
	/// fortranOffset 
	int fortranOffset = - 1;

	/// appear correct for liquid
	int firstCoefIndex = water_liquid_entr_index_vector[pIndex] - pIndex + 1 + fortranOffset;
	int lastCoefIndex = water_liquid_entr_index_vector[pIndex+1] - pIndex- 1 + fortranOffset;

	std::vector<double> edge;
	edge.reserve( lastCoefIndex - firstCoefIndex + 1 );

	for (int i = firstCoefIndex; i <= lastCoefIndex; i++) edge.push_back(liquidGrid(0,edgeInd,i,pFraction));
	int rowInd = maxloc(edge,indVar);
	int coefIndex = firstCoefIndex + rowInd; 

	double pAdjustedCoefs[4];
	for (int i = 0; i < 4; i++) pAdjustedCoefs[i] = liquidGrid(i,edgeInd,coefIndex,pFraction);

	/// solve cubic for sFraction at given T
	double sFraction = cubic_solution(pAdjustedCoefs[0],pAdjustedCoefs[1], pAdjustedCoefs[2], pAdjustedCoefs[3], indVar);

	/// calculate S
	int sIndex = water_liquid_entr_index_vector[pIndex] +rowInd;
	(*data).S = water_liquid_entr_values[sIndex] + sFraction*(water_liquid_entr_values[sIndex+1] -water_liquid_entr_values[sIndex]);

	/// dependent properties
	(*data).T = liquidGrid(0,coefIndex,pFraction,sFraction);
	(*data).dens = liquidGrid(1,coefIndex,pFraction,sFraction);
	(*data).V = 1./(*data).dens;
	(*data).H = liquidGrid(2,coefIndex,pFraction,sFraction);
	(*data).Cv = liquidGrid(3,coefIndex,pFraction,sFraction);
	(*data).Cp = liquidGrid(4,coefIndex,pFraction,sFraction);
	(*data).ssnd = liquidGrid(5,coefIndex,pFraction,sFraction);
	(*data).cond = liquidGrid(6,coefIndex,pFraction,sFraction);
	(*data).visc = liquidGrid(7,coefIndex,pFraction,sFraction);
	(*data).U = (*data).H - (*data).P*(*data).V;
	(*data).Q = -10 ;
}
/// Generic function for computing vapor gap properties
void VaporGap(int firstCoefIndex, double TSat, double sFraction,property_info *data)
{
	/// find temperature index and fractional position
	int TSatIndex; double dT;
	sat_find(TSat, &TSatIndex, &dT);

	/// find pressure_index and fractional position
	int pIndex; double pFraction;
	pres_find((*data).P, &pIndex, &pFraction);

	/// call vapor gap functions
	(*data).T = vapor_gap_temp(sFraction,pFraction,TSat,firstCoefIndex);
	(*data).dens = vapor_gap_dens(sFraction,pFraction,dT,TSatIndex,firstCoefIndex);
	(*data).V = 1./(*data).dens;
	(*data).H = vapor_gap_enth(sFraction,pFraction,dT,TSatIndex,firstCoefIndex);
	(*data).S = vapor_gap_entr(sFraction,dT,TSatIndex,pIndex);
	(*data).U = (*data).H - (*data).P*(*data).V;
	(*data).Cv = vapor_gap_cv(sFraction,pFraction,dT,TSatIndex,firstCoefIndex);
	(*data).Cp = vapor_gap_cp(sFraction,pFraction,dT,TSatIndex,firstCoefIndex);
	(*data).ssnd = vapor_gap_ssnd(sFraction,pFraction,dT,TSatIndex,firstCoefIndex);
	(*data).cond = vapor_gap_cond(sFraction,pFraction,dT,TSatIndex,firstCoefIndex);
	(*data).visc = vapor_gap_visc(sFraction,pFraction,dT,TSatIndex,firstCoefIndex);
	(*data).Q = 10.;
}
/// Generic function for computing liquid gap properties
void LiquidGap(int lastCoefIndex, double TSat, double sFraction,property_info *data)
{
	/// find temperature index and fractional position
	int TSatIndex; double dT;
	sat_find(TSat, &TSatIndex, &dT);

	/// find pressure_index and fractional position
	int pIndex; double pFraction;
	pres_find((*data).P, &pIndex, &pFraction);

	/// call liquid gap functions
	(*data).T = liquid_gap_temp(sFraction,pFraction,TSat,lastCoefIndex);
	(*data).dens = liquid_gap_dens(sFraction,pFraction,dT,TSatIndex,lastCoefIndex);
	(*data).V = 1./(*data).dens;
	(*data).H = liquid_gap_enth(sFraction,pFraction,dT,TSatIndex,lastCoefIndex);
	(*data).S = liquid_gap_entr(sFraction,dT,TSatIndex,pIndex);
	(*data).U = (*data).H - (*data).P*(*data).V;
	(*data).Cv = liquid_gap_cv(sFraction,pFraction,dT,TSatIndex,lastCoefIndex);
	(*data).Cp = liquid_gap_cp(sFraction,pFraction,dT,TSatIndex,lastCoefIndex);
	(*data).ssnd = liquid_gap_ssnd(sFraction,pFraction,dT,TSatIndex,lastCoefIndex);
	(*data).cond = liquid_gap_cond(sFraction,pFraction,dT,TSatIndex,lastCoefIndex);
	(*data).visc = liquid_gap_visc(sFraction,pFraction,dT,TSatIndex,lastCoefIndex);
	(*data).Q = -10.;
}



/// Calculate the root (that should lie between 0 and 1) of a cubic equation. 
double cubic_solution(double coef_1, double coef_2, double coef_3, double coef_4, double value)
{
/// Adapted from Numerical Recipes: 3rd Edition (C++), pg. 228
///   Required Inputs: 
///	   coef_1 = the first coefficient
///	   coef_2 = the second coefficient
///	   coef_3 = the third coefficient
///	   coef_4 = the fourth coefficient
///	   value = the cubic equation will be solved for this value (y)
///    Required Outputs
///	   root = the root of the cubic that ideally lies between 0 and 1
///	Problem of form:
///	   input:  (coef_4)*x^3+(coef_3)*x^2+(coef_2)*x+(coef_1) = value
///	   to solve: x^3 + ax^2 + bx + c = 0 */

	double PI =  3.1415926535897932384626433;
	double root = -999999999999.;
	double one_third = (1./3.);
	double a = coef_3 / coef_4;
	double b = coef_2 / coef_4;
	double c = (coef_1 - value) / coef_4;
	double q = ( (a*a) - 3*b) * (1./9.);
	double r = (2.*(a*a*a) - 9.*(a*b) + 27.*c)*(1./54.);
	double r2 = r*r;
	double q3= q*q*q;

	/// Case where 3 real roots exist
	if (r2 < q3)
	{
		double theta = acos(r/ sqrt(q3) );
		double root_basis = -2*sqrt(q);
		double root_1 = root_basis * cos(theta/3.) - a/3.;

		if ( (root_1 >= 0.0) && (root_1 <= 1.0) ) 
		{
			root = root_1;
			return root;
		}
		else
		{
			double root_2 = root_basis * cos((theta + 2.*PI)/3.) - a/3.;
			if ((root_2 >= 0.0) && (root_2 <= 1.0) ) 
			{
				root = root_2;
				return root;
			}
			else
			{
				double root_3 = root_basis*cos((theta-2.*PI)/3.) - a/3.;
				if ((root_3 >= 0.0) && (root_3 <= 1.0) )
				{
					root = root_3;
					return root;
				}
				else
				{
					double froot_1 = fabs(root_1);
					double froot_2 = fabs(root_2);
					double froot_3 = fabs(root_3);

					if ((froot_1 < froot_2) && (froot_1 < froot_3)) root = root_1;
					else if ((froot_2 < froot_1) && (froot_2 < froot_3)) root = root_2;
					else root = root_3;

					return root;
				}
			}
		}
	}
	else 
	{
		int sign = 1;
		double t1 = sqrt(r2 - q3);
		double A,B;

		if (r < 0) sign = -1;
		A = -sign*pow((fabs(r) + t1),one_third);
		fabs(A) < 1e-9 ? B = 0: B = q/A;

		root = (A+B) - a*one_third;
		return root;
	}
}

/// Returns the saturation temperature at the given pressure.
double t_sat(double P)
{
/// No bounds checking is performed and the function will not adjust the highest index - 
///	it assumes the pressure vector extends beyond the maximum pressure that will be given.
///	Required Inputs:
///	    pres = pressure (kPa)
///	 Returns:
///	    temp = saturation temperature (C) 

	/// locate interval
	int index = -1;
	if (P >= water_sat_pres_vector[index+64]) index = index+64;
	if (P >= water_sat_pres_vector[index+32]) index = index+32;
	if (P >= water_sat_pres_vector[index+16]) index = index+16;
	if (P >= water_sat_pres_vector[index+8]) index = index+8;
	if (P >= water_sat_pres_vector[index+4]) index = index+4;
	if (P >= water_sat_pres_vector[index+2]) index = index+2;
	if (P >= water_sat_pres_vector[index+1]) index = index+1;
	double dP = P-water_sat_pres_vector[index];

	/// Extract Saturation properties (R = 4, C = 126)
	int ind[4];
	ind[0] = Index2D(0,index,4);
	ind[1] = Index2D(1,index,4);
	ind[2] = Index2D(2,index,4);
	ind[3] = Index2D(3,index,4);

	/// calculate TSat using nested multiplication
	double TSat = ((water_sat_temp_coef_array[ind[0]] *dP + 
				    water_sat_temp_coef_array[ind[1]])*dP +
				    water_sat_temp_coef_array[ind[2]])*dP + 
					water_sat_temp_coef_array[ind[3]];
	return TSat;
}

/// Looks for the index of the element containing the provided temperature.
void sat_find(double T, int *TSatIndex, double *dT)
{

/// Note: will return 0 if the temperature is less than the lowest value, and if the temperature is exactly equal to the highest value then it will return one less than expected 
///	Required Inputs:
///		T = saturation temperature
///	 Required Outputs:
///	    TSatIndex = the element containing the value, referencing the smaller index value (left side of element)
///	    dT = the temperature difference between the given temperature and the smaller element bound */


	/// locate interval containing given T
	int index = -1;
	if (T >= water_sat_temp_vector[index+64]) index = index+64;
	if (T >= water_sat_temp_vector[index+32]) index = index+32;
	if (T >= water_sat_temp_vector[index+16]) index = index+16;
	if (T >= water_sat_temp_vector[index+8]) index = index+8;
	if (T >= water_sat_temp_vector[index+4]) index = index+4;
	if (T >= water_sat_temp_vector[index+2]) index = index+2;
	if (T >= water_sat_temp_vector[index+1]) index = index+1;
	if (index == 126 && T == water_sat_temp_vector[126]) index = 125;

	/// calculate difference between left temp and given temperature
	*TSatIndex = index;
	*dT = T - water_sat_temp_vector[index];
}
/// Looks for the index of the element containing the provided pressure
void pres_find(double P, int *PSatIndex, double *pFraction)
{

	/// Locate the interval containing the given temperature.
	int index = -1;
	if (P >= water_pres_vector[index+64]) index = index+64;
	if (P >= water_pres_vector[index+32]) index = index+32;
	if (P >= water_pres_vector[index+16]) index = index+16;
	if (P >= water_pres_vector[index+8]) index = index+8;
	if (P >= water_pres_vector[index+4]) index = index+4;
	if (P >= water_pres_vector[index+2]) index = index+2;
	if (P >= water_pres_vector[index+1]) index = index+1;
	if (index == 126 && P == water_pres_vector[126]) index = 125;

	/// Calculate the fractional position within the element.
	*pFraction = (P - water_pres_vector[index])/(water_pres_vector[index+1]-water_pres_vector[index]);
	*PSatIndex = index;

}

/// Two index model of vapor grid 
double vaporGrid( int j, int k, int firstCoefIndex, double pFraction)
{
	/// return linear indices (R = 4, C = 4, D = 8)
	int ind[4];
	ind[0] = Index4D(3,j,k,firstCoefIndex,4,4,8);
	ind[1] = Index4D(2,j,k,firstCoefIndex,4,4,8);
	ind[2] = Index4D(1,j,k,firstCoefIndex,4,4,8);
	ind[3] = Index4D(0,j,k,firstCoefIndex,4,4,8);

	/// uses linear interpolation
	double grid = ((water_vapor_coef_array[ind[0]]*pFraction + 
					water_vapor_coef_array[ind[1]])*pFraction +
					water_vapor_coef_array[ind[2]])*pFraction + 
					water_vapor_coef_array[ind[3]];
	return grid;
}
/// One index model of vapor grid (overloaded), sFraction input optional
double vaporGrid(int k,int lastCoefIndex, double pFraction, double sFraction)
{

	int ind[16];
	/// linear index (R = 4, C = 4, D = 8)
	ind[0] = Index4D(3,3,k,lastCoefIndex,4,4,8);
	ind[1] = Index4D(2,3,k,lastCoefIndex,4,4,8);
	ind[2] = Index4D(1,3,k,lastCoefIndex,4,4,8);
	ind[3] = Index4D(0,3,k,lastCoefIndex,4,4,8);
	ind[4] = Index4D(3,2,k,lastCoefIndex,4,4,8);
	ind[5] = Index4D(2,2,k,lastCoefIndex,4,4,8);
	ind[6] = Index4D(1,2,k,lastCoefIndex,4,4,8);
	ind[7] = Index4D(0,2,k,lastCoefIndex,4,4,8);
	ind[8] = Index4D(3,1,k,lastCoefIndex,4,4,8);
	ind[9] = Index4D(2,1,k,lastCoefIndex,4,4,8);
	ind[10] = Index4D(1,1,k,lastCoefIndex,4,4,8);
	ind[11] = Index4D(0,1,k,lastCoefIndex,4,4,8);
	ind[12] = Index4D(3,0,k,lastCoefIndex,4,4,8);
	ind[13] = Index4D(2,0,k,lastCoefIndex,4,4,8);
	ind[14] = Index4D(1,0,k,lastCoefIndex,4,4,8);
	ind[15] = Index4D(0,0,k,lastCoefIndex,4,4,8);


	double grid = (((((water_vapor_coef_array[ind[0]]*pFraction + 
		water_vapor_coef_array[ind[1]])*pFraction +
		water_vapor_coef_array[ind[2]])*pFraction + 
		water_vapor_coef_array[ind[3]]*sFraction) +
		((water_vapor_coef_array[ind[4]]*pFraction + 
		water_vapor_coef_array[ind[5]])*pFraction +
		water_vapor_coef_array[ind[6]])*pFraction + 
		water_vapor_coef_array[ind[7]])*sFraction +
		((water_vapor_coef_array[ind[8]]*pFraction + 
		water_vapor_coef_array[ind[9]])*pFraction +
		water_vapor_coef_array[ind[10]])*pFraction + 
		water_vapor_coef_array[ind[11]])*sFraction +
		((water_vapor_coef_array[ind[12]]*pFraction + 
		water_vapor_coef_array[ind[13]])*pFraction +
		water_vapor_coef_array[ind[14]])*pFraction + 
		water_vapor_coef_array[ind[15]];

	return grid;
}
/// Two index version of liquidGrid
double liquidGrid( int j, int k, int firstCoefIndex, double pFraction)
{
	/// return linear indices (R = 4, C = 4, D = 8)
	int ind1 = Index4D(3,j,k,firstCoefIndex,4,4,8);
	int ind2 = Index4D(2,j,k,firstCoefIndex,4,4,8);
	int ind3 = Index4D(1,j,k,firstCoefIndex,4,4,8);
	int ind4 = Index4D(0,j,k,firstCoefIndex,4,4,8);

	/// uses linear interpolation
	double grid = ((water_liquid_coef_array[ind1]*pFraction + 
				    water_liquid_coef_array[ind2])*pFraction + 
					water_liquid_coef_array[ind3])*pFraction + 
					water_liquid_coef_array[ind4];
	return grid;
}
/// One parameter model of liquidGrid (overloaded). sFraction option input
double liquidGrid(int k,int lastCoefIndex, double pFraction, double sFraction)
{

	int ind[16];
	/// linear index (R = 4, C = 4, D = 8)
	ind[0] = Index4D(3,3,k,lastCoefIndex,4,4,8);
	ind[1] = Index4D(2,3,k,lastCoefIndex,4,4,8);
	ind[2] = Index4D(1,3,k,lastCoefIndex,4,4,8);
	ind[3] = Index4D(0,3,k,lastCoefIndex,4,4,8);
	ind[4] = Index4D(3,2,k,lastCoefIndex,4,4,8);
	ind[5] = Index4D(2,2,k,lastCoefIndex,4,4,8);
	ind[6] = Index4D(1,2,k,lastCoefIndex,4,4,8);
	ind[7] = Index4D(0,2,k,lastCoefIndex,4,4,8);
	ind[8] = Index4D(3,1,k,lastCoefIndex,4,4,8);
	ind[9] = Index4D(2,1,k,lastCoefIndex,4,4,8);
	ind[10] = Index4D(1,1,k,lastCoefIndex,4,4,8);
	ind[11] = Index4D(0,1,k,lastCoefIndex,4,4,8);
	ind[12] = Index4D(3,0,k,lastCoefIndex,4,4,8);
	ind[13] = Index4D(2,0,k,lastCoefIndex,4,4,8);
	ind[14] = Index4D(1,0,k,lastCoefIndex,4,4,8);
	ind[15] = Index4D(0,0,k,lastCoefIndex,4,4,8);


	double grid = (((((water_liquid_coef_array[ind[0]]*pFraction + 
		water_liquid_coef_array[ind[1]])*pFraction +
		water_liquid_coef_array[ind[2]])*pFraction + 
		water_liquid_coef_array[ind[3]]*sFraction )+
		((water_liquid_coef_array[ind[4]]*pFraction + 
		water_liquid_coef_array[ind[5]])*pFraction +
		water_liquid_coef_array[ind[6]])*pFraction + 
		water_liquid_coef_array[ind[7]])*sFraction +
		((water_liquid_coef_array[ind[8]]*pFraction + 
		water_liquid_coef_array[ind[9]])*pFraction +
		water_liquid_coef_array[ind[10]])*pFraction + 
		water_liquid_coef_array[ind[11]])*sFraction +
		((water_liquid_coef_array[ind[12]]*pFraction + 
		water_liquid_coef_array[ind[13]])*pFraction +
		water_liquid_coef_array[ind[14]])*pFraction + 
		water_liquid_coef_array[ind[15]];

	return grid;
}
/// Dome from saturation properties
double propDome( int j, int k, int TSatIndex, double dT)
{

	int ind[4];
	/// dome indices (R = 4, C = 5, D = 2) 
	ind[0] = Index4D(0,j,k,TSatIndex,4,5,2); 
	ind[1] = Index4D(1,j,k,TSatIndex, 4,5,2);
	ind[2] = Index4D(2,j,k,TSatIndex,4,5,2); 
	ind[3] = Index4D(3,j,k,TSatIndex, 4,5,2);

	/// linear interpolation
	double dome = ((water_sat_prop_coef_array[ind[0]]*dT + 
					water_sat_prop_coef_array[ind[1]])*dT +
					water_sat_prop_coef_array[ind[2]])*dT + 
					water_sat_prop_coef_array[ind[3]];
	return dome;
}
/// Dome from VHS properties
double VHSDome(int i, int j, int TSatIndex, double dT)
{
	int ind[4];
	/// dome indices (R = 3, C = 2, D = 4) 
	ind[0] = Index4D(i,j,0,TSatIndex,3,2,4); 
	ind[1] = Index4D(i,j,1,TSatIndex, 3,2,4);
	ind[2] = Index4D(i,j,2,TSatIndex,3,2,4); 
	ind[3] = Index4D(i,j,3,TSatIndex, 3,2,4);

	/// linear interpolation
	double dome = ((water_sat_vhs_coef_array[ind[0]]*dT + 
		water_sat_vhs_coef_array[ind[1]])*dT +
		water_sat_vhs_coef_array[ind[2]])*dT + 
		water_sat_vhs_coef_array[ind[3]];
	return dome;
}
/// Equivalent of Fortran maxloc
int maxloc( int firstIndex, int lastIndex, double valueVector[], double S)
{
	double maxS = 0.;
	int rowInd = -1;

	for (int i = firstIndex; i != lastIndex; i++)
	{
		if (valueVector[i] < S)
		{
			if (valueVector[i] > maxS)
			{
				++rowInd;
				maxS = valueVector[i];
			}
		}
	}
	return rowInd;

}
/// Overloaded Fortran maxloc
int maxloc( std::vector<double> &valueVector, double S)
{
	/// meant for inputting user defined vector, not one included in header file
	double maxS = 0.;
	int rowInd = -1;

	for (size_t i = 0; i != valueVector.size(); i++)
	{
		if (valueVector[i] < S)
		{
			if (valueVector[i] > maxS)
			{
				++rowInd;
				maxS = valueVector[i];
			}
		}
		else return rowInd;
	}

	/// if row index is -1, then the wrong range was given.  
	return rowInd;

}


