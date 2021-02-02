#ifndef _TCSFRESNEL_MOLTEN_SALT_CASES_H_
#define _TCSFRESNEL_MOLTEN_SALT_CASES_H_

#include <map>
#include "../input_cases/code_generator_utilities.h"
#include "tcsfresnel_molten_salt_common_data.h"

/**
*   Data for high-level integration tests that verifies whether results for a liner Fresnel molten
*   salt plant in Tucson, AZ matches expected results.
*   Data generated from code-generator (Shift+F5) within SAM UI.
*   Test uses SSCAPI interfaces (similiar to SDK usage) to pass and receive data to tcsfresnel_molten_salt
*/


// Linear Fresnel molten salt default congifuration
int tcsfresnel_molten_salt_tucson_default(ssc_data_t &data)
{
	tcsfresnel_molten_salt_default(data);

	int status = run_module(data, "tcsmslf");

	//single_owner_default(data);
	//status += run_module(data, "singleowner");

	return status;
}

// Linear Fresnel molten salt with alternative defocusing strategy
// Defocusing strategy: Sequenced
// Rest default configurations with respect to the No Finanical model
int tcsfresnel_molten_salt_tucson_defocusing_strategy(ssc_data_t &data)
{
	tcsfresnel_molten_salt_default(data);

	ssc_data_set_number( data, "fthrctrl", 1 );

	int status = run_module(data, "tcsmslf");

	return status;
}

// Linear Fresnel molten salt with alternative Field HTF
// Field HTF: Therminol VP-1
// Rest default configurations with respect to the No Finanical model
int tcsfresnel_molten_salt_tucson_field_HTF(ssc_data_t &data)
{
	tcsfresnel_molten_salt_default(data);

	ssc_data_set_number(data, "Fluid", 21);
	ssc_data_set_number(data, "field_fluid", 21);
	ssc_data_set_number(data, "is_hx", 1);
	ssc_data_set_number(data, "V_tank_hot_ini", 1290.5642);
	ssc_data_set_number(data, "vol_tank", 6452.821);

	int status = run_module(data, "tcsmslf");

	return status;
}

// Linear Fresnel molten salt with alternative optical characterization method
// Optical characterization method: Solar position 
// Rest default configurations with respect to the No Finanical model
int tcsfresnel_molten_salt_tucson_optical_char_solar(ssc_data_t &data)
{
	tcsfresnel_molten_salt_default(data);

	ssc_data_set_number(data, "opt_model", 1);

	int status = run_module(data, "tcsmslf");

	return status;
}

// Linear Fresnel molten salt with alternative receiver model type
// Receiver model type: Polynomial heat loss model
// Rest default configurations with respect to the No Finanical model
//int tcsfresnel_molten_salt_tucson_polynomial_heat_loss_model(ssc_data_t &data)
//{
//	tcsfresnel_molten_salt_default(data);
//
//	ssc_data_set_number(data, "nLoops", 148);
//	ssc_data_set_number(data, "rec_model", 1);
//
//	int status = run_module(data, "tcsmslf");
//
//	return status;
//}

// Linear Fresnel molten salt with alternative condenser type
// Condenser type: Evaporative
// Rest default configurations with respect to the No Finanical model
//int tcsfresnel_molten_salt_tucson_evap_condenser(ssc_data_t &data)
//{
//	tcsfresnel_molten_salt_default(data);
//
//	ssc_data_set_number(data, "CT", 1);
//
//	int status = run_module(data, "tcsmslf");
//
//	return status;
//}

// Linear Fresnel molten salt with alternative condenser type
// Condenser type: Hybrid
// Rest default configurations with respect to the No Finanical model
//int tcsfresnel_molten_salt_tucson_hybrid_condenser(ssc_data_t &data)
//{
//	tcsfresnel_molten_salt_default(data);
//
//	ssc_data_set_number(data, "CT", 3);
//
//	int status = run_module(data, "tcsmslf");
//
//	return status;
//}

// Linear Fresnel molten salt with alternative turbine inlet pressure control
// Turbine inlet pressure control: Sliding pressure
// Rest default configurations with respect to the No Finanical model
//int tcsfresnel_molten_salt_tucson_sliding_p(ssc_data_t &data)
//{
//	tcsfresnel_molten_salt_default(data);
//
//	ssc_data_set_number(data, "tech_type", 3);
//
//	int status = run_module(data, "tcsmslf");
//
//	return status;
//}

// Linear Fresnel molten salt with alternative HTF freeze protection mode
// HTF freeze protection mode: Electric heating
// Rest default configurations with respect to the No Finanical model
//int tcsfresnel_molten_salt_tucson_HTF_freeze_protection(ssc_data_t &data)
//{
//	tcsfresnel_molten_salt_default(data);
//
//	ssc_data_set_number(data, "fp_mode", 1);
//
//	int status = run_module(data, "tcsmslf");
//
//	return status;
//}

// Linear Fresnel molten salt with alternative storage HTF
// Storage HTF: Therminol VP-1
// Rest default configurations with respect to the No Finanical model
//int tcsfresnel_molten_salt_tucson_storage_HTF(ssc_data_t &data)
//{
//	tcsfresnel_molten_salt_default(data);
//
//	ssc_data_set_number(data, "store_fluid", 21);
//	ssc_data_set_number(data, "is_hx", 1);
//	ssc_data_set_number(data, "V_tank_hot_ini", 1963.66443);
//	ssc_data_set_number(data, "vol_tank", 9818.3223);
//
//	int status = run_module(data, "tcsmslf");
//
//	return status;
//}

// Linear Fresnel molten salt with alternative Power Cycle
// Power Cycle: User Defined 
// Rest default configurations with respect to the No Finanical model
//int tcsfresnel_molten_salt_tucson_userdefined_default(ssc_data_t &data)
//{
//	tcsfresnel_molten_salt_default(data);
//
//	ssc_data_set_number(data, "pc_config", 1);
//
//	int status = run_module(data, "tcsmslf");
//
//	return status;
//}

#endif
