#ifndef __CO2_TEST_
#define __CO2_TEST_

#include "co2props.h"

void blah()
{
	property_info co2_props;

	double err = co2_TP(35.0 + 273.15, 10000, &co2_props);

	double c_h0 = co2_props.H;

	double err2 = co2_TP(55.0 + 273.15, 10000, &co2_props);

	double c_h1 = co2_props.H;		//[kJ/kg] 

	double delta_h = c_h1 - c_h0;

	double bllaahhh = 1.23;

}

#endif