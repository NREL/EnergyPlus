#ifndef _TCSTROUGH_EMPIRICAL_DEFAULTS_H
#define _TCSTROUGH_EMPIRICAL_DEFAULTS_H

#include <stdio.h>
#include "../input_cases/code_generator_utilities.h"

/**
*  Default data for tcstrough_empirical technology model
*/
ssc_data_t tcstrough_empirical_defaults()
{
    ssc_data_t data = ssc_data_create();

	char solar_resource_path[512];
	int n1 = sprintf(solar_resource_path, "%s/test/input_cases/trough_empirical_data/tucson_az_32.116521_-110.933042_psmv3_60_tmy.csv", std::getenv("SSCDIR"));

	ssc_data_set_string(data, "file_name", solar_resource_path);
    ssc_data_set_number(data, "track_mode", 1);
    ssc_data_set_number(data, "tilt", 0);
    ssc_data_set_number(data, "azimuth", 0);
    ssc_data_set_number(data, "system_capacity", 99900);
    ssc_number_t p_weekday_schedule[288] = { 6, 6, 6, 6, 6, 6, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 6, 6, 6, 6, 6, 6, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 6, 6, 6, 6, 6, 6, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 6, 6, 6, 6, 6, 6, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 6, 6, 6, 6, 6, 6, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 6, 6, 6, 6, 6, 6, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 6, 6, 6, 6, 6, 6, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 6, 6, 6, 6, 6, 6, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5 };
    ssc_data_set_matrix(data, "weekday_schedule", p_weekday_schedule, 12, 24);
    ssc_number_t p_weekend_schedule[288] = { 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5 };
    ssc_data_set_matrix(data, "weekend_schedule", p_weekend_schedule, 12, 24);
    ssc_data_set_number(data, "i_SfTi", -999);
    ssc_data_set_number(data, "SfPipeHl300", 10);
    ssc_data_set_number(data, "SfPipeHl1", 0.0016930000000000001);
    ssc_data_set_number(data, "SfPipeHl2", -1.683e-05);
    ssc_data_set_number(data, "SfPipeHl3", 6.7799999999999998e-08);
    ssc_data_set_number(data, "Stow_Angle", 170);
    ssc_data_set_number(data, "DepAngle", 10);
    ssc_data_set_number(data, "Distance_SCA", 1);
    ssc_data_set_number(data, "Row_Distance", 15);
    ssc_data_set_number(data, "NumScas", 4);
    ssc_data_set_number(data, "Solar_Field_Area", 877579.8125);
    ssc_data_set_number(data, "Solar_Field_Mult", 2);
    ssc_data_set_number(data, "SfInTempD", 293);
    ssc_data_set_number(data, "SfOutTempD", 391);
    ssc_data_set_number(data, "MinHtfTemp", 50);
    ssc_data_set_number(data, "HtfGalArea", 0.61399999999999999);
    ssc_data_set_number(data, "SFTempInit", 100);
    ssc_data_set_number(data, "HTFFluid", 21);
    ssc_data_set_number(data, "IamF0", 1);
    ssc_data_set_number(data, "IamF1", 0.050599999999999999);
    ssc_data_set_number(data, "IamF2", -0.17630000000000001);
    ssc_data_set_number(data, "Ave_Focal_Length", 1.8);
    ssc_data_set_number(data, "ScaLen", 100);
    ssc_data_set_number(data, "SCA_aper", 5);
    ssc_data_set_number(data, "SfAvail", 0.98999999999999999);
    ssc_data_set_number(data, "TrkTwstErr", 0.99399999999999999);
    ssc_data_set_number(data, "GeoAcc", 0.97999999999999998);
    ssc_data_set_number(data, "MirRef", 0.93500000000000005);
    ssc_data_set_number(data, "MirCln", 0.94999999999999996);
    ssc_data_set_number(data, "ConcFac", 1);
    ssc_data_set_number(data, "NumHCETypes", 4);
    ssc_number_t p_HCEtype[4] = { 1, 1, 1, 1 };
    ssc_data_set_array(data, "HCEtype", p_HCEtype, 4);
    ssc_number_t p_HCEFrac[4] = { 0.98499999999999999, 0.01, 0.0050000000000000001, 0 };
    ssc_data_set_array(data, "HCEFrac", p_HCEFrac, 4);
    ssc_number_t p_HCEdust[4] = { 0.97999999999999998, 0.97999999999999998, 0.97999999999999998, 0.97999999999999998 };
    ssc_data_set_array(data, "HCEdust", p_HCEdust, 4);
    ssc_number_t p_HCEBelShad[4] = { 0.96299999999999997, 0.96299999999999997, 0.96299999999999997, 0 };
    ssc_data_set_array(data, "HCEBelShad", p_HCEBelShad, 4);
    ssc_number_t p_HCEEnvTrans[4] = { 0.96299999999999997, 0.96299999999999997, 1, 0 };
    ssc_data_set_array(data, "HCEEnvTrans", p_HCEEnvTrans, 4);
    ssc_number_t p_HCEabs[4] = { 0.95999999999999996, 0.95999999999999996, 0.80000000000000004, 0 };
    ssc_data_set_array(data, "HCEabs", p_HCEabs, 4);
    ssc_number_t p_HCEmisc[4] = { 1, 1, 1, 0 };
    ssc_data_set_array(data, "HCEmisc", p_HCEmisc, 4);
    ssc_number_t p_PerfFac[4] = { 1, 1, 1, 0 };
    ssc_data_set_array(data, "PerfFac", p_PerfFac, 4);
    ssc_number_t p_RefMirrAper[4] = { 5, 5, 5, 5 };
    ssc_data_set_array(data, "RefMirrAper", p_RefMirrAper, 4);
    ssc_number_t p_HCE_A0[4] = { 4.0499999999999998, 50.799999999999997, -9.9499999999999993, 0 };
    ssc_data_set_array(data, "HCE_A0", p_HCE_A0, 4);
    ssc_number_t p_HCE_A1[4] = { 0.247, 0.90400000000000003, 0.46500000000000002, 0 };
    ssc_data_set_array(data, "HCE_A1", p_HCE_A1, 4);
    ssc_number_t p_HCE_A2[4] = { -0.0014599999999999999, 0.00057899999999999998, -0.00085400000000000005, 0 };
    ssc_data_set_array(data, "HCE_A2", p_HCE_A2, 4);
    ssc_number_t p_HCE_A3[4] = { 5.6500000000000001e-06, 1.13e-05, 1.8499999999999999e-05, 0 };
    ssc_data_set_array(data, "HCE_A3", p_HCE_A3, 4);
    ssc_number_t p_HCE_A4[4] = { 7.6199999999999994e-08, 1.73e-07, 6.8899999999999999e-07, 0 };
    ssc_data_set_array(data, "HCE_A4", p_HCE_A4, 4);
    ssc_number_t p_HCE_A5[4] = { -1.7, -43.200000000000003, 24.699999999999999, 0 };
    ssc_data_set_array(data, "HCE_A5", p_HCE_A5, 4);
    ssc_number_t p_HCE_A6[4] = { 0.012500000000000001, 0.52400000000000002, 3.3700000000000001, 0 };
    ssc_data_set_array(data, "HCE_A6", p_HCE_A6, 4);
    ssc_data_set_number(data, "TurbOutG", 111);
    ssc_data_set_number(data, "TurbEffG", 0.37740000000000001);
    ssc_data_set_number(data, "PTTMAX", 1.1499999761581421);
    ssc_data_set_number(data, "PTTMIN", 0.25);
    ssc_data_set_number(data, "MaxGrOut", 1.1499999999999999);
    ssc_data_set_number(data, "MinGrOut", 0.25);
    ssc_data_set_number(data, "TurSUE", 0.20000000000000001);
    ssc_data_set_number(data, "T2EPLF0", -0.037726000000000003);
    ssc_data_set_number(data, "T2EPLF1", 1.0062);
    ssc_data_set_number(data, "T2EPLF2", 0.076315999999999995);
    ssc_data_set_number(data, "T2EPLF3", -0.044775000000000002);
    ssc_data_set_number(data, "T2EPLF4", 0);
    ssc_data_set_number(data, "E2TPLF0", 0.03737);
    ssc_data_set_number(data, "E2TPLF1", 0.98823000000000005);
    ssc_data_set_number(data, "E2TPLF2", -0.064990999999999993);
    ssc_data_set_number(data, "E2TPLF3", 0.039387999999999999);
    ssc_data_set_number(data, "E2TPLF4", 0);
    ssc_data_set_number(data, "TempCorrF", 1);
    ssc_data_set_number(data, "TempCorr0", 1.0087299999999999);
    ssc_data_set_number(data, "TempCorr1", 0.00435842);
    ssc_data_set_number(data, "TempCorr2", -0.00025102300000000001);
    ssc_data_set_number(data, "TempCorr3", -9.02e-07);
    ssc_data_set_number(data, "TempCorr4", 4.8200000000000001e-08);
    ssc_data_set_number(data, "LHVBoilEff", 0.90000000000000002);
    ssc_data_set_number(data, "TurTesEffAdj", 0.98499999999999999);
    ssc_data_set_number(data, "TurTesOutAdj", 0.998);
    ssc_data_set_number(data, "TnkHL", 0.96999999999999997);
    ssc_data_set_number(data, "PTSmax", 294.11764526367188);
    ssc_data_set_number(data, "PFSmax", 297.9993896484375);
    ssc_data_set_number(data, "TSHOURS", 6);
    ssc_data_set_number(data, "NUMTOU", 9);
    ssc_number_t p_TSLogic[36] = { 1, 0.10000000000000001, 0.10000000000000001, 1.05, 2, 0.10000000000000001, 0.10000000000000001, 1, 3, 0.10000000000000001, 0.10000000000000001, 1, 4, 0.10000000000000001, 0.10000000000000001, 1, 5, 0.10000000000000001, 0.10000000000000001, 1, 6, 0.10000000000000001, 0.10000000000000001, 1, 7, 0.10000000000000001, 0.10000000000000001, 1, 8, 0.10000000000000001, 0.10000000000000001, 1, 9, 0.10000000000000001, 0.10000000000000001, 1 };
    ssc_data_set_matrix(data, "TSLogic", p_TSLogic, 9, 4);
    ssc_number_t p_FossilFill[9] = { 0, 0, 0, 0, 0, 0, 0, 0, 0 };
    ssc_data_set_array(data, "FossilFill", p_FossilFill, 9);
    ssc_data_set_number(data, "E_tes_ini", 0);
    ssc_data_set_number(data, "SfPar", 0.2334362268447876);
    ssc_data_set_number(data, "SfParPF", 1);
    ssc_data_set_number(data, "ChtfPar", 9.2321395874023438);
    ssc_data_set_number(data, "ChtfParPF", 1);
    ssc_data_set_number(data, "CHTFParF0", -0.035999999999999997);
    ssc_data_set_number(data, "CHTFParF1", 0.24199999999999999);
    ssc_data_set_number(data, "CHTFParF2", 0.79400000000000004);
    ssc_data_set_number(data, "AntiFrPar", 0.92321395874023438);
    ssc_data_set_number(data, "BOPPar", 2.7383699417114258);
    ssc_data_set_number(data, "BOPParPF", 1);
    ssc_data_set_number(data, "BOPParF0", 0.48299999999999998);
    ssc_data_set_number(data, "BOPParF1", 0.51700000000000002);
    ssc_data_set_number(data, "BOPParF2", 0);
    ssc_data_set_number(data, "CtOpF", 1);
    ssc_data_set_number(data, "CtPar", 1.8919949531555176);
    ssc_data_set_number(data, "CtParPF", 1);
    ssc_data_set_number(data, "CtParF0", -0.035999999999999997);
    ssc_data_set_number(data, "CtParF1", 0.24199999999999999);
    ssc_data_set_number(data, "CtParF2", 0.79400000000000004);
    ssc_data_set_number(data, "HtrPar", 2.5230300426483154);
    ssc_data_set_number(data, "HtrParPF", 1);
    ssc_data_set_number(data, "HtrParF0", 0.48299999999999998);
    ssc_data_set_number(data, "HtrParF1", 0.51700000000000002);
    ssc_data_set_number(data, "HtrParF2", 0);
    ssc_data_set_number(data, "HhtfPar", 2.2200000286102295);
    ssc_data_set_number(data, "HhtfParPF", 1);
    ssc_data_set_number(data, "HhtfParF0", -0.035999999999999997);
    ssc_data_set_number(data, "HhtfParF1", 0.24199999999999999);
    ssc_data_set_number(data, "HhtfParF2", 0.79400000000000004);
    ssc_data_set_number(data, "PbFixPar", 0.61049997806549072);
    ssc_data_set_number(data, "adjust:constant", 4);

    return data;
}

#endif
