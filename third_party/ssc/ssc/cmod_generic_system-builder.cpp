#include <string>
#include <vector>

#include "vartab.h"

#include "cmod_generic_system-builder.h"

SSCEXPORT float GenericSystem_conv_eff_eval(ssc_data_t ptr)
{
	auto vt = static_cast<var_table*>(ptr);

	auto vd = vt->lookup("heat_rate");

	if (!vd)
	    throw std::runtime_error("Could not calculate conv_eff for GenericSystem: heat_rate not set");

	// inputs
	double heat_rate = vd->num;

	// outputs
	double conv_eff;

	if ( heat_rate == 0.000000 ) {
		conv_eff = 0.000000;
	}
	conv_eff = 100.000000 / heat_rate * 0.293100;

	return (float)conv_eff;

}



