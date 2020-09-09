#include "co2props_nn.h"
#include "CO2_properties.h"
	
int co2_TD(double T, double D, property_info *data)
{
	CO2_state state;
	int val = CO2_TD(T, D, &state);

	data->T = state.temp;
	data->Q = state.qual;
	data->P = state.pres;
	data->V = 1.0 / state.dens;
	data->U = state.inte;
	data->H = state.enth;
	data->S = state.entr;
	data->dens = state.dens;
	data->Cv = state.cv;
	data->Cp = state.cp;
	data->cond = CO2_cond(state.dens, state.temp);
	data->visc = CO2_visc(state.dens, state.temp);
	data->ssnd = state.ssnd;
	return val;
}

int co2_TP(double T, double P, property_info *data)
{
	CO2_state state;
	int val = CO2_TP(T, P, &state);

	data->T = state.temp;
	data->Q = state.qual;
	data->P = state.pres;
	data->V = 1.0 / state.dens;
	data->U = state.inte;
	data->H = state.enth;
	data->S = state.entr;
	data->dens = state.dens;
	data->Cv = state.cv;
	data->Cp = state.cp;
	data->cond = CO2_cond(state.dens, state.temp);
	data->visc = CO2_visc(state.dens, state.temp);
	data->ssnd = state.ssnd;
	return val;
}

int co2_PH(double P, double H, property_info *data)
{
	CO2_state state;
	int val = CO2_PH(P, H, &state);

	data->T = state.temp;
	data->Q = state.qual;
	data->P = state.pres;
	data->V = 1.0 / state.dens;
	data->U = state.inte;
	data->H = state.enth;
	data->S = state.entr;
	data->dens = state.dens;
	data->Cv = state.cv;
	data->Cp = state.cp;
	data->cond = CO2_cond(state.dens, state.temp);
	data->visc = CO2_visc(state.dens, state.temp);
	data->ssnd = state.ssnd;
	return val;
}

int co2_PS(double P, double S, property_info *data)
{
	CO2_state state;
	int val = CO2_PS(P, S, &state);

	data->T = state.temp;
	data->Q = state.qual;
	data->P = state.pres;
	data->V = 1.0 / state.dens;
	data->U = state.inte;
	data->H = state.enth;
	data->S = state.entr;
	data->dens = state.dens;
	data->Cv = state.cv;
	data->Cp = state.cp;
	data->cond = CO2_cond(state.dens, state.temp);
	data->visc = CO2_visc(state.dens, state.temp);
	data->ssnd = state.ssnd;
	return val;
}

int co2_HS(double H, double S, property_info *data)
{
	CO2_state state;
	int val = CO2_HS(H, S, &state);

	data->T = state.temp;
	data->Q = state.qual;
	data->P = state.pres;
	data->V = 1.0 / state.dens;
	data->U = state.inte;
	data->H = state.enth;
	data->S = state.entr;
	data->dens = state.dens;
	data->Cv = state.cv;
	data->Cp = state.cp;
	data->cond = CO2_cond(state.dens, state.temp);
	data->visc = CO2_visc(state.dens, state.temp);
	data->ssnd = state.ssnd;
	return val;
}

