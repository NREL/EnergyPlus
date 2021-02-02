#include <string>
#include <vector>
#include <cstdio>
#include <math.h>

#include "vartab.h"
#include "../shared/lib_util.h"

#include "cmod_windpower_eqns.h"

#pragma warning(disable: 4297)  // ignore warning: 'function assumed not to throw an exception but does'

void Turbine_calculate_powercurve(ssc_data_t data)
{
    auto vt = static_cast<var_table*>(data);
    if (!vt){
        throw std::runtime_error("ssc_data_t data invalid");
    }
	 
    double turbine_size, rotor_diameter, elevation, max_cp, max_tip_speed, max_tip_sp_ratio, cut_in,
            cut_out;
    int drive_train;

    vt_get_number(vt, "turbine_size", &turbine_size);
    vt_get_number(vt, "wind_turbine_rotor_diameter", &rotor_diameter);     // ssc input
    vt_get_number(vt, "elevation", &elevation);
    vt_get_number(vt, "wind_turbine_max_cp", &max_cp);                     // ssc input
    vt_get_number(vt, "max_tip_speed", &max_tip_speed);
    vt_get_number(vt, "max_tip_sp_ratio", &max_tip_sp_ratio);
    vt_get_number(vt, "cut_in", &cut_in);
    vt_get_number(vt, "cut_out", &cut_out);
    vt_get_int(vt, "drive_train", &drive_train);

    util::matrix_t<ssc_number_t> powercurve_windspeeds;
    util::matrix_t<ssc_number_t> powercurve_powerout;
    util::matrix_t<ssc_number_t> powercurve_hub_efficiency;

	char errmsg[250];

	double region2_slope = 5;
	double a, b, c;
	int drive_train_type = (int)drive_train + 1;

    if ( drive_train_type == 1 ) {
	    a = 0.012894;
	    b = 0.085095;
		c = 0.000000;
	
	}
	else if ( drive_train_type == 2 ) {
		a = 0.013307;
		b = 0.036547;
		c = 0.061067;
	
	}
	else if ( drive_train_type == 3 ) {
		a = 0.015474;
		b = 0.044631;
		c = 0.057898;
	}
	else if ( drive_train_type == 4 ) {
		a = 0.010072;
		b = 0.019995;
		c = 0.068990;
	}
	else{
        throw std::runtime_error("drive_train must be between 0 and 3");
	}

	double eff = 1.0 - (a + b + c);
	double rated_hub_power = turbine_size / eff;

    //air_density = 101300.0 * pow( (1-((0.0065*elevation)/288.0)), (9.8/(0.0065*287.15)) ) / (287.15*(288.0-0.0065*elevation)); // used through March 4, 2013
    // From Widipedia:
    // sea level standard atmospheric pressure = 101.325 kPa (changes
    // sea level standard temperature = 288.15 K
    // Earth-surface gravitational acceleration = 9.80665 m/s2. (varies from 9.78 to 9.82 depending on where on earth it's measured)
    // temperature lapse rate L = 0.0065 K/m
	double air_density = 101325.0 * pow( 1 - (0.0065 * elevation/288.15), (9.80665/(0.0065*287.15)) )
	        / (287.15 * (288.15 - 0.0065 * elevation));
    double omega_m = max_tip_speed / rotor_diameter * 2.;
	double omega_0 = omega_m / (1. + region2_slope / 100.);
	double t_sub_m = rated_hub_power * 1000. / omega_m;
	double k = air_density * M_PI * pow( rotor_diameter, 5 ) * max_cp / (64. * pow( max_tip_sp_ratio, 3));
	double omegaT_a = k;
	double omegaT_b = -t_sub_m / (omega_m - omega_0);
    double omegaT_c = t_sub_m * omega_0 / (omega_m - omega_0);
	double omegaT = -(omegaT_b / (2.0 * omegaT_a)) - (sqrt( pow(omegaT_b, 2) - (4.0 * omegaT_a * omegaT_c) ) / (2.0 * omegaT_a));
	double wind_at_omegaT = omegaT * rotor_diameter / 2. / max_tip_sp_ratio;
	double power_at_omegaT = k * pow( omegaT, 3. ) / 1000.;
	double rated_wind_speed = 0.33 * pow(2.0 * rated_hub_power * 1000.0 / (air_density * M_PI * pow(rotor_diameter, 2)
	        /4.0 * max_cp), (1.0/3.0)) + 0.67 * (((1.0 / (1.5 * air_density * M_PI * pow(rotor_diameter, 2) * 0.25
	                * max_cp * pow(wind_at_omegaT, 2))) * 1000.0 * (rated_hub_power-power_at_omegaT)) + wind_at_omegaT);

	if ( omegaT > omega_m ) {
		sprintf( errmsg, "Turbine inputs are not valid, please adjust the inputs. omegaT: %f, omegaM: %f", omegaT, omega_m );
        vt->assign( "error", std::string(errmsg ));
    }

	double step = 0.25;
	size_t array_size = 1 + size_t(40 / step);

    powercurve_windspeeds.resize(array_size);
    powercurve_powerout.resize(array_size);
    powercurve_hub_efficiency.resize( array_size );

	for ( size_t i = 0; i < array_size; i += 1 ){
	    double ws = i * step;
	    double hub_power;
		if ( ws <= cut_in || ws >= cut_out ) {
			hub_power = 0.;
		}
		else if ( ws < wind_at_omegaT ) {
            hub_power = k * pow(ws * max_tip_sp_ratio / rotor_diameter * 2. , 3) / 1000.0; // 'region 2' hub power
		}
		else if ( ws <= rated_wind_speed ) {
            hub_power = (rated_hub_power - power_at_omegaT) / (rated_wind_speed-wind_at_omegaT) * (ws - wind_at_omegaT)
                    + power_at_omegaT; // 'region 2.5' hub power
		}
		else {
            hub_power = rated_hub_power;
        }

		if ( hub_power > rated_hub_power ) {
            sprintf( errmsg, "Turbine power curve calculation calculated power > rated output at windspeed %f", ws );
            vt->assign( "error", std::string(errmsg ));
            hub_power = rated_hub_power;
		}
		if ( hub_power == 0. ) {
            powercurve_hub_efficiency[i] = 0;
		}
		else {
            powercurve_hub_efficiency[i] = ((hub_power / rated_hub_power) - (a + b * (hub_power / rated_hub_power)
                    + c * pow(hub_power/rated_hub_power, 2))) / (hub_power / rated_hub_power);

        }
        powercurve_powerout[i] = hub_power * powercurve_hub_efficiency[i];
        powercurve_windspeeds[i] = ws;

    }

	var_data windspeeds = var_data(powercurve_windspeeds.data(), powercurve_windspeeds.ncols());
	var_data powerout = var_data(powercurve_powerout.data(), powercurve_powerout.ncols());
	var_data hub_eff = var_data(powercurve_hub_efficiency.data(), powercurve_hub_efficiency.ncols());

	vt->assign( "wind_turbine_powercurve_windspeeds", windspeeds);
    vt->assign( "wind_turbine_powercurve_powerout", powerout);
    vt->assign( "rated_wind_speed", rated_wind_speed );
    vt->assign( "hub_efficiency", hub_eff );
}
