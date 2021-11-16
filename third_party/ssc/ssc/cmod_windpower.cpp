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

#include "core.h"
#include "lib_windfile.h"
#include "lib_windwatts.h"
// for adjustment factors
#include "common.h"
#include "lib_util.h"
#include "cmod_windpower.h"

static var_info _cm_vtab_windpower[] = {
	// VARTYPE     DATATYPE     NAME                                    LABEL                                        UNITS     META    GROUP                                REQUIRED_IF                       CONSTRAINTS                                          UI_HINTS
	{ SSC_INPUT  , SSC_NUMBER , "wind_resource_model_choice"         , "Hourly, Weibull or Distribution model"    , "0/1/2"   ,""                                    , "Resource"                             , "*"                                               , "INTEGER"                                         , "" } ,
	{ SSC_INPUT  , SSC_STRING , "wind_resource_filename"             , "Local wind data file path"                , ""        ,""                                    , "Resource"                             , "?"                                               , "LOCAL_FILE"                                      , "" } ,
	{ SSC_INPUT  , SSC_TABLE  , "wind_resource_data"                 , "Wind resouce data in memory"              , ""        ,""                                    , "Resource"                             , "?"                                               , ""                                                , "" } ,
	{ SSC_INPUT  , SSC_MATRIX , "wind_resource_distribution"         , "Wind Speed x Dir Distribution as 2-D PDF" , "m/s,deg" ,""                                    , "Resource"                             , "wind_resource_model_choice=2"                    , ""                                                , "" } ,
	{ SSC_INPUT  , SSC_NUMBER , "weibull_reference_height"           , "Reference height for Weibull wind speed"  , "m"       ,""                                    , "Resource"                             , "?=50"                                            , "MIN=0"                                           , "" } ,
	{ SSC_INPUT  , SSC_NUMBER , "weibull_k_factor"                   , "Weibull K factor for wind resource"       , ""        ,""                                    , "Resource"                             , "wind_resource_model_choice=1"                    , ""                                                , "" } ,
	{ SSC_INPUT  , SSC_NUMBER , "weibull_wind_speed"                 , "Average wind speed for Weibull model"     , ""        ,""                                    , "Resource"                             , "wind_resource_model_choice=1"                    , "MIN=0"                                           , "" } ,

	{ SSC_INPUT  , SSC_NUMBER , "wind_resource_shear"                , "Shear exponent"                           , ""        ,""                                    , "Turbine"                              , "*"                                               , "MIN=0"                                           , "" } ,
	{ SSC_INPUT  , SSC_NUMBER , "wind_turbine_rotor_diameter"        , "Rotor diameter"                           , "m"       ,""                                    , "Turbine"                              , "*"                                               , "POSITIVE"                                        , "" } ,
	{ SSC_INOUT  , SSC_ARRAY  , "wind_turbine_powercurve_windspeeds" , "Power curve wind speed array"             , "m/s"     ,""                                    , "Turbine"                              , "*"                                               , ""                                                , "GROUP=WTPCD" } ,
	{ SSC_INOUT  , SSC_ARRAY  , "wind_turbine_powercurve_powerout"   , "Power curve turbine output array"         , "kW"      ,""                                    , "Turbine"                              , "*"                                               , "LENGTH_EQUAL=wind_turbine_powercurve_windspeeds" , "GROUP=WTPCD" } ,
	{ SSC_INPUT  , SSC_NUMBER , "wind_turbine_hub_ht"                , "Hub height"                               , "m"       ,""                                    , "Turbine"                              , "*"                                               , "POSITIVE"                                        , "" } ,
	{ SSC_INPUT  , SSC_NUMBER , "wind_turbine_max_cp"                , "Max Coefficient of Power"                 , ""        ,""                                    , "Turbine"                              , "wind_resource_model_choice=1"                    , "MIN=0"                                           , "" } ,

	{ SSC_INPUT  , SSC_NUMBER , "wind_farm_wake_model"               , "Wake Model [Simple, Park, EV, Constant]"  , "0/1/2/3" ,""                                    , "Farm"                                 , "*"                                               , "INTEGER"                                         , "" } ,
	{ SSC_INPUT  , SSC_NUMBER , "wind_resource_turbulence_coeff"     , "Turbulence coefficient"                   , "%"       ,""                                    , "Farm"                                 , "*"                                               , "MIN=0"                                           , "" } ,
	{ SSC_INPUT  , SSC_NUMBER , "system_capacity"                    , "Nameplate capacity"                       , "kW"      ,""                                    , "Farm"                                 , "*"                                               , "MIN=0"                                           , "" } ,
	{ SSC_INPUT  , SSC_ARRAY  , "wind_farm_xCoordinates"             , "Turbine X coordinates"                    , "m"       ,""                                    , "Farm"                                 , "*"                                               , ""                                                , "" } ,
	{ SSC_INPUT  , SSC_ARRAY  , "wind_farm_yCoordinates"             , "Turbine Y coordinates"                    , "m"       ,""                                    , "Farm"                                 , "*"                                               , "LENGTH_EQUAL=wind_farm_xCoordinates"             , "" } ,

	{ SSC_INPUT  , SSC_NUMBER , "en_low_temp_cutoff"                 , "Enable Low Temperature Cutoff"            , "0/1"     ,""                                    , "Losses"                               , "?=0"                                             , "INTEGER"                                         , "" } ,
	{ SSC_INPUT  , SSC_NUMBER , "low_temp_cutoff"                    , "Low Temperature Cutoff"                   , "C"       ,""                                    , "Losses"                               , "en_low_temp_cutoff=1"                            , ""                                                , "" } ,
	{ SSC_INPUT  , SSC_NUMBER , "en_icing_cutoff"                    , "Enable Icing Cutoff"                      , "0/1"     ,""                                    , "Losses"                               , "?=0"                                             , "INTEGER"                                         , "" } ,
	{ SSC_INPUT  , SSC_NUMBER , "icing_cutoff_temp"                  , "Icing Cutoff Temperature"                 , "C"       ,""                                    , "Losses"                               , "en_icing_cutoff=1"                               , ""                                                , "" } ,
	{ SSC_INPUT  , SSC_NUMBER , "icing_cutoff_rh"                    , "Icing Cutoff Relative Humidity"           , "%"       ,"'rh' required in wind_resource_data" , "Losses"                               , "en_icing_cutoff=1"                               , "MIN=0"                                           , "" } ,
    { SSC_INPUT  , SSC_NUMBER , "wake_int_loss"                      , "Constant Wake Model, internal wake loss"  , "%"       ,""                                    , "Losses"                               , "wind_farm_wake_model=3"                          , "MIN=0,MAX=100"                                   , "" } ,
    { SSC_INPUT  , SSC_NUMBER , "wake_ext_loss"                      , "External Wake loss"                       , "%"       ,""                                    , "Losses"                               , "?=0"                                             , "MIN=0,MAX=100"                                   , "" } ,
    { SSC_INPUT  , SSC_NUMBER , "wake_future_loss"                   , "Future Wake loss"                         , "%"       ,""                                    , "Losses"                               , "?=0"                                             , "MIN=0,MAX=100"                                   , "" } ,
	{ SSC_INPUT  , SSC_NUMBER , "avail_bop_loss"                     , "Balance-of-plant availability loss"       , "%"       ,""                                    , "Losses"                               , "?=0"                                             , "MIN=0,MAX=100"                                   , "" } ,
	{ SSC_INPUT  , SSC_NUMBER , "avail_grid_loss"                    , "Grid availability loss"                   , "%"       ,""                                    , "Losses"                               , "?=0"                                             , "MIN=0,MAX=100"                                   , "" } ,
	{ SSC_INPUT  , SSC_NUMBER , "avail_turb_loss"                    , "Turbine availabaility loss"               , "%"       ,""                                    , "Losses"                               , "?=0"                                             , "MIN=0,MAX=100"                                   , "" } ,
	{ SSC_INPUT  , SSC_NUMBER , "elec_eff_loss"                      , "Electrical efficiency loss"               , "%"       ,""                                    , "Losses"                               , "?=0"                                             , "MIN=0,MAX=100"                                   , "" } ,
	{ SSC_INPUT  , SSC_NUMBER , "elec_parasitic_loss"                , "Electrical parasitic consumption loss"    , "%"       ,""                                    , "Losses"                               , "?=0"                                             , "MIN=0,MAX=100"                                   , "" } ,
	{ SSC_INPUT  , SSC_NUMBER , "env_degrad_loss"                    , "Environmental Degradation loss"           , "%"       ,""                                    , "Losses"                               , "?=0"                                             , "MIN=0,MAX=100"                                   , "" } ,
	{ SSC_INPUT  , SSC_NUMBER , "env_exposure_loss"                  , "Environmental Exposure loss"              , "%"       ,""                                    , "Losses"                               , "?=0"                                             , "MIN=0,MAX=100"                                   , "" } ,
	{ SSC_INPUT  , SSC_NUMBER , "env_env_loss"                       , "Environmental External Conditions loss"   , "%"       ,""                                    , "Losses"                               , "?=0"                                             , "MIN=0,MAX=100"                                   , "" } ,
	{ SSC_INPUT  , SSC_NUMBER , "env_icing_loss"                     , "Environmental Icing loss"                 , "%"       ,""                                    , "Losses"                               , "?=0"                                             , "MIN=0,MAX=100"                                   , "" } ,
	{ SSC_INPUT  , SSC_NUMBER , "ops_env_loss"                       , "Environmental/Permit Curtailment loss"    , "%"       ,""                                    , "Losses"                               , "?=0"                                             , "MIN=0,MAX=100"                                   , "" } ,
	{ SSC_INPUT  , SSC_NUMBER , "ops_grid_loss"                      , "Grid curtailment loss"                    , "%"       ,""                                    , "Losses"                               , "?=0"                                             , "MIN=0,MAX=100"                                   , "" } ,
	{ SSC_INPUT  , SSC_NUMBER , "ops_load_loss"                      , "Load curtailment loss"                    , "%"       ,""                                    , "Losses"                               , "?=0"                                             , "MIN=0,MAX=100"                                   , "" } ,
	{ SSC_INPUT  , SSC_NUMBER , "ops_strategies_loss"                , "Operational strategies loss"              , "%"       ,""                                    , "Losses"                               , "?=0"                                             , "MIN=0,MAX=100"                                   , "" } ,
	{ SSC_INPUT  , SSC_NUMBER , "turb_generic_loss"                  , "Turbine Generic Powercurve loss"          , "%"       ,""                                    , "Losses"                               , "?=0"                                             , "MIN=0,MAX=100"                                   , "" } ,
	{ SSC_INPUT  , SSC_NUMBER , "turb_hysteresis_loss"               , "Turbine High Wind Hysteresis loss"        , "%"       ,""                                    , "Losses"                               , "?=0"                                             , "MIN=0,MAX=100"                                   , "" } ,
	{ SSC_INPUT  , SSC_NUMBER , "turb_perf_loss"                     , "Turbine Sub-optimal performance loss"     , "%"       ,""                                    , "Losses"                               , "?=0"                                             , "MIN=0,MAX=100"                                   , "" } ,
	{ SSC_INPUT  , SSC_NUMBER , "turb_specific_loss"                 , "Turbine Site-specific Powercurve loss"    , "%"       ,""                                    , "Losses"                               , "?=0"                                             , "MIN=0,MAX=100"                                   , "" } ,



        // OUTPUTS ----------------------------------------------------------------------------annual_energy
	{ SSC_OUTPUT , SSC_ARRAY  , "turbine_output_by_windspeed_bin"    , "Turbine output by wind speed bin"         , "kW"      ,""                                    , "Power Curve"                      ,"" , "LENGTH_EQUAL=wind_turbine_powercurve_windspeeds" , "" } ,
	{ SSC_OUTPUT , SSC_ARRAY  , "wind_direction"                     , "Wind direction"                           , "deg"     ,""                                    , "Time Series"                          , "wind_resource_model_choice=0"                    , ""                                                , "" } ,
    { SSC_OUTPUT , SSC_ARRAY  , "wind_speed"                         , "Wind speed"                               , "m/s"     ,""                                    , "Time Series"                          , "wind_resource_model_choice=0"                    , ""                                                , "" } ,
	{ SSC_OUTPUT , SSC_ARRAY  , "temp"                               , "Air temperature"                          , "'C"      ,""                                    , "Time Series"                          , "wind_resource_model_choice=0"                    , ""                                                , "" } ,
	{ SSC_OUTPUT , SSC_ARRAY  , "pressure"                           , "Pressure"                                 , "atm"     ,""                                    , "Time Series"                          , "wind_resource_model_choice=0"                    , ""                                                , "" } ,

	{ SSC_OUTPUT , SSC_ARRAY  , "monthly_energy"                     , "Monthly Energy"                           , "kWh"     ,""                                    , "Monthly"                              , "*"                                               , "LENGTH=12"                                       , "" } ,
	{ SSC_OUTPUT , SSC_NUMBER , "annual_energy"                      , "Annual Energy"                            , "kWh"     ,""                                    , "Annual"                               , "*"                                               , ""                                                , "" } ,
	{ SSC_OUTPUT , SSC_NUMBER , "annual_gross_energy"                , "Annual Gross Energy"                      , "kWh"     ,""                                    , "Annual"                               , "*"                                               , ""                                                , "" } ,
	{ SSC_OUTPUT , SSC_NUMBER , "capacity_factor"                    , "Capacity factor"                          , "%"       ,""                                    , "Annual"                               , "*"                                               , ""                                                , "" } ,
	{ SSC_OUTPUT , SSC_NUMBER , "kwh_per_kw"                         , "First year kWh/kW"                        , "kWh/kW"  ,""                                    , "Annual"                               , "*"                                               , ""                                                , "" } ,
    { SSC_OUTPUT , SSC_NUMBER , "wind_speed_average"                 , "Average Wind speed"                       , "m/s"     ,""                                    , "Annual"                               , ""                                                , ""                                                , "" } ,

    { SSC_OUTPUT , SSC_NUMBER , "avail_losses"                       , "Availability losses"                      , "%"       ,""                                    , "Annual"                           ,"" , ""                                                , "" } ,
    { SSC_OUTPUT , SSC_NUMBER , "elec_losses"                        , "Electrical losses"                        , "%"       ,""                                    , "Annual"                           ,"" , ""                                                , "" } ,
    { SSC_OUTPUT , SSC_NUMBER , "env_losses"                         , "Environmental losses"                     , "%"       ,""                                    , "Annual"                           ,"" , ""                                                , "" } ,
    { SSC_OUTPUT , SSC_NUMBER , "ops_losses"                         , "Operational losses"                       , "%"       ,""                                    , "Annual"                           ,"" , ""                                                , "" } ,
    { SSC_OUTPUT , SSC_NUMBER , "turb_losses"                        , "Turbine losses"                           , "%"       ,""                                    , "Annual"                           ,"" , ""                                                , "" } ,
    { SSC_OUTPUT , SSC_NUMBER , "wake_losses"                        , "Wake losses"                              , "%"       ,""                                    , "Annual"                           ,"" , ""                                                , "" } ,

    { SSC_OUTPUT , SSC_NUMBER , "cutoff_losses"                      , "Low temp and Icing Cutoff losses"         , "%"       ,""                                    , "Annual"                           ,"" , ""                                                , "" } ,
	var_info_invalid };

winddata::winddata(var_data *data_table)
{
	irecord = 0;

	stdErrorMsg = "wind data must be an SSC table variable with fields: "
                           "(number): lat, lon, elev, year, "
                           "(array): heights, fields [dim: 4, temp=1,pres=2,speed=3,dir=4], rh (dim: nstep, optional)"
                           "(matrix): data (dim: 4 x Nheights x nstep)";

	if (data_table->type != SSC_TABLE)
	{
		m_errorMsg = stdErrorMsg;
		return;
	}

	lat = get_number(data_table, "lat");
	lon = get_number(data_table, "lon");
	elev = get_number(data_table, "elev");
	year = (int)get_number(data_table, "year");

	size_t len = 0;
	ssc_number_t *p = get_vector(data_table, "heights", &len);
	for (size_t i = 0; i < len; i++)
		m_heights.push_back((double)p[i]);

	p = get_vector(data_table, "fields", &len);
	for (size_t i = 0; i < len; i++)
		m_dataid.push_back((int)p[i]);

	if (m_dataid.size() != m_heights.size() || m_heights.size() == 0){
		m_errorMsg = util::format("'fields' and 'heights' must have same length");
		return;
	}

	if (var_data *D = data_table->table.lookup("data"))
		if (D->type == SSC_MATRIX)
			data = D->num;

	if (data.ncols() != m_heights.size()){
		m_errorMsg = util::format("number of columns in 'data' must be same as length of 'fields' and 'heights'");
		return;
	}

	double* rh = get_vector(data_table, "rh", &len);
	if (rh != nullptr && len == data.nrows() )
		m_relativeHumidity = std::vector<double>(rh, rh+(int)len);
	else if (rh != nullptr){
        m_errorMsg = stdErrorMsg;
        return;
	}
}

size_t winddata::nrecords()
{
	return data.nrows();
}

ssc_number_t winddata::get_number(var_data *v, const char *name)
{
	if (var_data *value = v->table.lookup(name))
	{
		if (value->type == SSC_NUMBER)
			return value->num;
	}

	return std::numeric_limits<ssc_number_t>::quiet_NaN();
}

ssc_number_t *winddata::get_vector(var_data *v, const char *name, size_t *len)
{
	ssc_number_t *p = 0;
	*len = 0;
	if (var_data *value = v->table.lookup(name))
	{
		if (value->type == SSC_ARRAY)
		{
			*len = value->num.length();
			p = value->num.data();
		}
	}
	return p;
}

bool winddata::read_line(std::vector<double> &values)
{
	if (irecord >= data.nrows()
		|| data.ncols() == 0
		|| data.nrows() == 0) return false;

	values.resize(data.ncols(), 0.0);
	for (size_t j = 0; j < data.ncols(); j++)
		values[j] = (double)data(irecord, j);

	irecord++;
	return true;
}


cm_windpower::cm_windpower(){
	add_var_info(_cm_vtab_windpower);
	// performance adjustment factors
	add_var_info(vtab_adjustment_factors);
	add_var_info(vtab_technology_outputs);
	// wind PRUF
	add_var_info(vtab_p50p90);
}

// wind PRUF loss framework. Can replace numerical loss percentages by calculated losses in future model
void calculate_losses(compute_module *cm, double wake_int_loss_percent) {
    double avail_loss_percent = 1. - ( 100. - cm->as_double("avail_bop_loss"))/100. * (100. - cm->as_double("avail_grid_loss"))/100.
            * ( 100. - cm->as_double("avail_turb_loss"))/100.;
    double elec_loss_percent = 1. - ( 100. - cm->as_double("elec_eff_loss"))/100. * ( 100. - cm->as_double("elec_parasitic_loss"))/100.;
    // for instance, how will icing and low temp cut off affect total env loss?
    double env_loss_percent = 1. - ( 100. - cm->as_double("env_degrad_loss"))/100. * ( 100. - cm->as_double("env_exposure_loss"))/100.
                              * ( 100. - cm->as_double("env_env_loss"))/100. * ( 100. - cm->as_double("env_icing_loss"))/100.;
    double ops_loss_percent = 1. - ( 100. - cm->as_double("ops_env_loss"))/100. * ( 100. - cm->as_double("ops_grid_loss"))/100.
                              * ( 100. - cm->as_double("ops_load_loss"))/100. * ( 100. - cm->as_double("ops_strategies_loss"))/100.;
    double turb_loss_percent = 1. - ( 100. - cm->as_double("turb_generic_loss"))/100. * ( 100. - cm->as_double("turb_hysteresis_loss"))/100.
                               * ( 100. - cm->as_double("turb_perf_loss"))/100. * ( 100. - cm->as_double("turb_specific_loss"))/100.;
    double wake_loss_percent = 1. - ( 100. - cm->as_double("wake_ext_loss"))/100. * ( 100. - cm->as_double("wake_future_loss"))/100.
                                * (100. - wake_int_loss_percent) / 100.;
    cm->assign("avail_losses", avail_loss_percent * 100.);
    cm->assign("elec_losses", elec_loss_percent * 100.);
    cm->assign("env_losses", env_loss_percent * 100.);
    cm->assign("ops_losses", ops_loss_percent * 100.);
    cm->assign("turb_losses", turb_loss_percent * 100.);
    cm->assign("wake_losses", wake_loss_percent * 100.);
}

double get_fixed_losses(compute_module* cm){
    double lossMultiplier = 1.;
    std::vector<std::string> loss_names = { "avail_bop_loss", "avail_grid_loss", "avail_turb_loss", "elec_eff_loss",
                                            "elec_parasitic_loss", "env_degrad_loss", "env_exposure_loss", "env_env_loss",
                                            "env_icing_loss", "ops_env_loss", "ops_grid_loss", "ops_load_loss",
                                            "ops_strategies_loss", "turb_generic_loss", "turb_hysteresis_loss",
                                            "turb_perf_loss", "turb_specific_loss", "wake_ext_loss", "wake_future_loss"};
    for (auto& loss : loss_names){
        if (cm->is_assigned(loss))
            lossMultiplier *= (1. - cm->as_double(loss)/100.);
    }
    return lossMultiplier;
}

void cm_windpower::exec()
{
	// create windTurbine's powerCurve
	windTurbine wt;
	wt.shearExponent = as_double("wind_resource_shear");
	wt.hubHeight = as_double("wind_turbine_hub_ht");
	wt.measurementHeight = wt.hubHeight;
	wt.rotorDiameter = as_double("wind_turbine_rotor_diameter");
	ssc_number_t *pc_w = as_array("wind_turbine_powercurve_windspeeds", &wt.powerCurveArrayLength);
	ssc_number_t *pc_p = as_array("wind_turbine_powercurve_powerout", NULL);
	std::vector<double> windSpeeds(wt.powerCurveArrayLength), powerOutput(wt.powerCurveArrayLength);
	for (size_t i = 0; i < wt.powerCurveArrayLength; i++){
		windSpeeds[i] = pc_w[i];
		powerOutput[i] = pc_p[i];
	}
	wt.setPowerCurve(windSpeeds, powerOutput);

	// create windPowerCalculator using windTurbine
	windPowerCalculator wpc;
	wpc.windTurb = &wt;
	wpc.turbulenceIntensity = as_double("wind_resource_turbulence_coeff");
	ssc_number_t *wind_farm_xCoordinates = as_array("wind_farm_xCoordinates", &wpc.nTurbines);
	ssc_number_t *wind_farm_yCoordinates = as_array("wind_farm_yCoordinates", NULL);
	wpc.XCoords.resize(wpc.nTurbines);
	wpc.YCoords.resize(wpc.nTurbines);
	for (size_t i = 0; i < wpc.nTurbines; i++)
	{
		wpc.XCoords[i] = (double)wind_farm_xCoordinates[i];
		wpc.YCoords[i] = (double)wind_farm_yCoordinates[i];
	}
	if (!wt.isInitialized())
		throw exec_error("windpower", util::format("wind turbine class not properly initialized"));
	if (wpc.nTurbines < 1)
		throw exec_error("windpower", util::format("the number of wind turbines was zero."));
	if (wpc.nTurbines > wpc.GetMaxTurbines())
		throw exec_error("windpower", util::format("the wind model is only configured to handle up to %d turbines.", wpc.GetMaxTurbines()));

	// create adjustment factors and losses
	adjustment_factors haf(this, "adjust");
	if (!haf.setup())
		throw exec_error("windpower", "failed to setup adjustment factors: " + haf.error());

    // add up all the percent losses, except for wind_int_loss, which will be applied by the turbine
    double lossMultiplier = get_fixed_losses(this);
    if (lossMultiplier > 1 || lossMultiplier < 0){
        throw exec_error("windpower", "Total percent losses must be between 0 and 100.");
    }
    double wake_int_loss_percent = 0.;

	bool lowTempCutoff = as_boolean("en_low_temp_cutoff");
	double lowTempCutoffValue = lowTempCutoff ? as_double("low_temp_cutoff") : -1;
	bool icingCutoff = as_boolean("en_icing_cutoff");
    double icingTempCutoffValue = icingCutoff ? as_double("icing_cutoff_temp") : -1;
    double icingRHCutoffValue = icingCutoff ? as_double("icing_cutoff_rh") : -1;

	// Run Weibull Statistical model (single outputs) if selected
	if (as_integer("wind_resource_model_choice") == 1 ){
		ssc_number_t *turbine_output = allocate("turbine_output_by_windspeed_bin", wt.powerCurveArrayLength);
		std::vector<double> turbine_outkW(wt.powerCurveArrayLength);
		double weibull_k = as_double("weibull_k_factor");
		double avg_speed = as_double("weibull_wind_speed");
		double ref_height = as_double("weibull_reference_height");
		//double max_cp = as_double("wind_turbine_max_cp");
		//double elevation = as_double("elevation");
		//ssc_number_t *hub_efficiency = as_array( "hub_efficiency", NULL );
		//std::vector<double> dp_hub_eff(wt.powerCurveArrayLength);
		//for (i=0;i<wt.powerCurveArrayLength;i++)
		//	dp_hub_eff[i] = (double)hub_efficiency[i];


		double turbine_kw = wpc.windPowerUsingWeibull(weibull_k, avg_speed, ref_height, &turbine_outkW[0]);
		ssc_number_t gross_energy = turbine_kw * wpc.nTurbines;

        wake_int_loss_percent = is_assigned("wake_int_loss") ? as_double("wake_int_loss") : 0.;
		turbine_kw = turbine_kw * lossMultiplier * (1. - wake_int_loss_percent/100.);

		int nstep = 8760;
		ssc_number_t farm_kw = (ssc_number_t)turbine_kw * wpc.nTurbines / (ssc_number_t)nstep;
		ssc_number_t *farmpwr = allocate("gen", nstep);
		for (int i = 0; i < nstep; i++) //nstep is always 8760 for Weibull
		{
			farmpwr[i] = farm_kw; // fill "gen"
			farmpwr[i] *= haf(i); //apply adjustment factor/availability and curtailment losses
		}

		for (size_t i = 0; i < wpc.nTurbines; i++)
			turbine_output[i] = (ssc_number_t)turbine_outkW[i];

		accumulate_monthly("gen", "monthly_energy");
		accumulate_annual("gen", "annual_energy");

		// metric outputs moved to technology
		double kWhperkW = 0.0;
		double nameplate = as_double("system_capacity");
		double annual_energy = as_double("annual_energy");
		if (nameplate > 0) kWhperkW = annual_energy / nameplate;
		assign("capacity_factor", var_data((ssc_number_t)(kWhperkW / 87.6)));
		assign("kwh_per_kw", var_data((ssc_number_t)kWhperkW));
		assign("annual_gross_energy", gross_energy);
		assign("wind_speed", avg_speed);
        calculate_p50p90(this);
        calculate_losses(this, wake_int_loss_percent);
		return;
	}

    // create wakeModel
    std::shared_ptr<wakeModelBase> wakeModel(nullptr);
    int wakeModelChoice = as_integer("wind_farm_wake_model");
    if (wakeModelChoice == 0)
        wakeModel = std::make_shared<simpleWakeModel>(simpleWakeModel(wpc.nTurbines, &wt));
    else if (wakeModelChoice == 1)
        wakeModel = std::make_shared<parkWakeModel>(parkWakeModel(wpc.nTurbines, &wt));
    else if (wakeModelChoice == 2)
    {
        wpc.turbulenceIntensity *= 100;
        wakeModel = std::make_shared<eddyViscosityWakeModel>(eddyViscosityWakeModel(wpc.nTurbines, &wt, as_double("wind_resource_turbulence_coeff")));
    }
    else if (wakeModelChoice == 3)
    {
        wake_int_loss_percent = as_double("wake_int_loss");
        wakeModel = std::make_shared<constantWakeModel>(constantWakeModel(wpc.nTurbines, &wt, (100. - wake_int_loss_percent)/100.));
    }
    else{
        throw exec_error("windpower", util::format("wind_farm_wake_model must be 0, 1, 2 or 3."));
    }
    if (!wpc.InitializeModel(wakeModel))
        throw exec_error("windpower", util::format("Error initializing wake model."));

    // Run Wind Speed x Direction Distribution model if selected
    if (as_integer("wind_resource_model_choice") == 2 ){
        double farmPower = 0., farmPowerGross = 0.;
        auto wind_dist = lookup("wind_resource_distribution")->matrix_vector();
        if (!wpc.windPowerUsingDistribution(wind_dist, &farmPower, &farmPowerGross)){
            throw exec_error("windpower", wpc.GetErrorDetails());
        }

        if (wakeModelChoice != 3)
            wake_int_loss_percent = (1. - farmPower / farmPowerGross) * 100.;

        int nstep = 8760;
        ssc_number_t farm_kw = farmPower / (ssc_number_t)nstep;
        ssc_number_t *farmpwr = allocate("gen", nstep);
        for (int i = 0; i < nstep; i++)
        {
            farmpwr[i] = farm_kw; // fill "gen"
            farmpwr[i] *= haf(i); //apply adjustment factor/availability and curtailment losses
            farmpwr[i] *= lossMultiplier;
        }

        accumulate_monthly("gen", "monthly_energy");
        accumulate_annual("gen", "annual_energy");

        // average wind speed
        double avg_speed = 0.;
        for (auto& row : wind_dist){
            avg_speed += row[0] * row[2];
        }

        // metric outputs moved to technology
        double kWhperkW = 0.0;
        double nameplate = as_double("system_capacity");
        double annual_energy = as_double("annual_energy");
        if (nameplate > 0) kWhperkW = annual_energy / nameplate;
        assign("capacity_factor", var_data((ssc_number_t)(kWhperkW / 87.6)));
        assign("kwh_per_kw", var_data((ssc_number_t)kWhperkW));
        assign("annual_gross_energy", farmPowerGross);
        assign("wind_speed_average", avg_speed);
        calculate_p50p90(this);
        calculate_losses(this, wake_int_loss_percent);
        return;
    }

	// Run time-step farm model (hourly or subhourly array outputs)

	////double meas_ht = as_double("meas_ht");
	////wpc.m_dCutInSpeed = as_double("wind_turbine_cutin");
	////ssc_number_t *pc_rpm = as_array( "pc_rpm", NULL );

	// create winddata_provider
	size_t nstep = 8760;
	smart_ptr<winddata_provider>::ptr wdprov;
	if (is_assigned("wind_resource_filename"))
	{
		// read the wind data file
		const char *file = as_string("wind_resource_filename");
		windfile *wp = new windfile(file);
		nstep = wp->nrecords();
		wdprov = smart_ptr<winddata_provider>::ptr(wp);
		if (!wp->ok() || (nstep == 0))
			throw exec_error("windpower", "failed to read local weather file: " + std::string(file) + " " + wp->error());
	}
	else if (is_assigned("wind_resource_data"))
	{
	  	wdprov = smart_ptr<winddata_provider>::ptr(new winddata(lookup("wind_resource_data")));
        if (!wdprov->error().empty()){
            throw exec_error("windpower", wdprov->error());
        }
        nstep = wdprov->nrecords();
        if (icingCutoff) {
            if (wdprov->relativeHumidity().empty()){
                std::string err = dynamic_cast<winddata*>(wdprov.get())->get_stdErrorMsg();
                throw exec_error( "windpower", err);
            }
            if (wdprov->relativeHumidity().size() != nstep)
                throw exec_error("windpower", "Length of rh (relative humidity) data must be equal to length of other fields.");
        }
	}
	else
		throw exec_error("windpower", "no wind resource data supplied");


	// check for leap day
	bool contains_leap_day = false;
	if (std::fmod((double)nstep, 8784) == 0)
	{
		contains_leap_day = true;
		int leap_steps_per_hr = (int)nstep / 8784;
		log("This weather file appears to contain a leap day. Feb 29th will be skipped. If this is not the case, please check your wind resource file.", SSC_NOTICE);
		nstep = leap_steps_per_hr * 8760;
	}

	// check for subhourly data
	size_t steps_per_hour = nstep / 8760;
	if (steps_per_hour * 8760 != nstep  && !contains_leap_day)
		throw exec_error("windpower", util::format("invalid number of data records (%d): must be an integer multiple of 8760", (int)nstep));

	// allocate output data
	ssc_number_t *farmpwr = allocate("gen", nstep);
	ssc_number_t *wspd = allocate("wind_speed", nstep);
	ssc_number_t *wdir = allocate("wind_direction", nstep);
	ssc_number_t *air_temp = allocate("temp", nstep);
	ssc_number_t *air_pres = allocate("pressure", nstep);

	std::vector<double> Power(wpc.nTurbines, 0.), Thrust(wpc.nTurbines, 0.),
		Eff(wpc.nTurbines, 0.), Wind(wpc.nTurbines, 0.), Turb(wpc.nTurbines, 0.),
		DistDown(wpc.nTurbines, 0.), DistCross(wpc.nTurbines, 0.);

	ssc_number_t *monthly = allocate("monthly_energy", 12);
	for (int i = 0; i < 12; i++)
		monthly[i] = 0.0f;
	double annual = 0.0;
	double annual_gross = 0.0;
	double withoutCutOffLosses = 0.0;
	double annual_after_wake_loss = 0.0;

	// compute power output at i-th timestep
	int i = 0;
	for (size_t hr = 0; hr < 8760; hr++)
	{
		int imonth = util::month_of((double)hr) - 1;

		for (size_t istep = 0; istep < steps_per_hour; istep++)
		{
			if (i % (nstep / 20) == 0)
				update("", 100.0f * ((float)i) / ((float)nstep), (float)i); //update percentage complete in UI

			double wind, dir, temp, pres, closest_dir_meas_ht;

			//skip leap day if applicable
			if (contains_leap_day)
			{
				if (hr == 1416) //(31 days in Jan  + 28 days in Feb) * 24 hours a day, +1 to be the start of Feb 29, -1 because of 0 indexing
					for (size_t j = 0; j < 24 * steps_per_hour; j++) //trash 24 hours' worth of lines in the weather file to skip the entire day of Feb 29
					{
						if (!wdprov->read(wt.hubHeight, &wind, &dir, &temp, &pres, &wt.measurementHeight, &closest_dir_meas_ht, true))
							throw exec_error("windpower", util::format("error reading wind resource file at %d: ", i) + wdprov->error());
					}
			} //now continue with the normal process, none of the counters have been incremented so everything else should be ok

			// if wf.read is set to interpolate (last input), and it's able to do so, then it will set wpc.measurementHeight equal to hub_ht
			// direction will not be interpolated, pressure and temperature will be if possible
			if (!wdprov->read(wt.hubHeight, &wind, &dir, &temp, &pres, &wt.measurementHeight, &closest_dir_meas_ht, true))
				throw exec_error("windpower", util::format("error reading wind resource file at %d: ", i) + wdprov->error());

			if (fabs(wt.measurementHeight - wt.hubHeight) > 35.0)
				throw exec_error("windpower", util::format("the closest wind speed measurement height (%lg m) found is more than 35 m from the hub height specified (%lg m)", wt.measurementHeight, wt.hubHeight));

			if (fabs(closest_dir_meas_ht - wt.measurementHeight) > 10.0)
			{
				if (i > 0) // if this isn't the first hour, then it's probably because of interpolation
				{
					// probably interpolated wind speed, but could not interpolate wind direction because the directions were too far apart.
					// first, verify:
					if ((wt.measurementHeight == wt.hubHeight) && (closest_dir_meas_ht != wt.hubHeight))
						// now, alert the user of this discrepancy
						throw exec_error("windpower", util::format("on hour %d, SAM interpolated the wind speed to an %lgm measurement height, but could not interpolate the wind direction from the two closest measurements because the directions encountered were too disparate", i + 1, wt.measurementHeight));
					else
						throw exec_error("windpower", util::format("SAM encountered an error at hour %d: hub height = %lg, closest wind speed meas height = %lg, closest wind direction meas height = %lg ", i + 1, wt.hubHeight, wt.measurementHeight, closest_dir_meas_ht));
				}
				else
					throw exec_error("windpower", util::format("the closest wind speed measurement height (%lg m) and direction measurement height (%lg m) were more than 10m apart", wt.measurementHeight, closest_dir_meas_ht));
			}

			// If the wind speed measurement height still differs from the turbine hub height (ie it wasn't corrected above, maybe because file only has one measurement height), use the shear to correct it.
			if (fabs(wt.measurementHeight - wt.hubHeight) > 1) {
				if (wt.shearExponent > 1.0) wt.shearExponent = 1.0 / 7.0;
				wind = wind * pow(wt.hubHeight / wt.measurementHeight, wt.shearExponent);
				wt.measurementHeight = wt.hubHeight;
			}

			double farmp = 0., gross_farmp = 0.;

			if ((int)wpc.nTurbines != wpc.windPowerUsingResource(
                    /* inputs */
                    wind,    /* m/s */
                    dir,    /* degrees */
                    pres,    /* Atm */
                    temp,    /* deg C */

                    /* outputs */
                    &farmp,
                    &gross_farmp,
                    &Power[0],
                    &Thrust[0],
                    &Eff[0],
                    &Wind[0],
                    &Turb[0],
                    &DistDown[0],
                    &DistCross[0]))
				throw exec_error("windpower", util::format("error in wind calculation at time %d, details: %s", i, wpc.GetErrorDetails().c_str()));

			annual_gross += gross_farmp;
			annual_after_wake_loss += farmp;
			farmp *= lossMultiplier;
			// apply and track cutoff losses
			withoutCutOffLosses += farmp * haf(hr);
			if (lowTempCutoff){
				if (temp < lowTempCutoffValue) farmp = 0.0;
			}
			if (icingCutoff){
				if (temp < icingTempCutoffValue && wdprov->relativeHumidity()[i] > icingRHCutoffValue)
					farmp = 0.0;
			}

			farmpwr[i] = (ssc_number_t)farmp*haf(hr); //adjustment factors are constrained to be hourly, not sub-hourly, so it's correct for this to be indexed on the hour
			wspd[i] = (ssc_number_t)wind;
			wdir[i] = (ssc_number_t)dir;
			air_temp[i] = (ssc_number_t)temp;
			air_pres[i] = (ssc_number_t)pres;

			// accumulate monthly and annual energy
			monthly[imonth] += farmpwr[i] / (ssc_number_t)steps_per_hour;
			annual += farmpwr[i] / (ssc_number_t)steps_per_hour;

			i++;
		} // end steps_per_hour loop
	} // end 1->8760 loop

	// assign outputs
	assign("annual_energy", var_data((ssc_number_t)annual));
	double kWhperkW = 0.0;
	double nameplate = as_double("system_capacity");
	if (nameplate > 0) kWhperkW = annual / nameplate;
	assign("capacity_factor", var_data((ssc_number_t)(kWhperkW / 87.6)));
	assign("kwh_per_kw", var_data((ssc_number_t)kWhperkW));
	assign("cutoff_losses", var_data((ssc_number_t)((withoutCutOffLosses - annual) / withoutCutOffLosses)));
	assign("annual_gross_energy", annual_gross);

    double wsp_avg = 0.;
    for (size_t n = 0; n < nstep; n++)
        wsp_avg += wspd[n];
    wsp_avg /= nstep;
    assign("wind_speed_average", wsp_avg);

	// internal wake loss is calculated during simulation rather than provided
	if (wakeModelChoice != 3){
        wake_int_loss_percent = (1. - annual_after_wake_loss/annual_gross) * 100.;
	}

	calculate_p50p90(this);
    calculate_losses(this, wake_int_loss_percent);
} // exec

DEFINE_MODULE_ENTRY(windpower, "Utility scale wind farm model (adapted from TRNSYS code by P.Quinlan and openWind software by AWS Truepower)", 2);
