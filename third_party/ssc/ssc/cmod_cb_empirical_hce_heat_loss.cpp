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

static var_info _cm_vtab_cb_empirical_hce_heat_loss[] = {

    { SSC_INPUT,   SSC_ARRAY,   "HCEFrac",           "Fraction of field that is this type of HCE",  "",     "",   "hce",   "*",     "",           "" }, 
    { SSC_INPUT,   SSC_ARRAY,   "PerfFac",           "label",                                       "",     "",   "hce",   "*",     "",           "" }, 
    { SSC_INPUT,   SSC_ARRAY,   "RefMirrAper",       "label",                                       "",     "",   "hce",   "*",     "",           "" }, 
    { SSC_INPUT,   SSC_ARRAY,   "HCE_A0",            "label",                                       "",     "",   "hce",   "*",     "",           "" }, 
    { SSC_INPUT,   SSC_ARRAY,   "HCE_A1",            "label",                                       "",     "",   "hce",   "*",     "",           "" }, 
    { SSC_INPUT,   SSC_ARRAY,   "HCE_A2",            "label",                                       "",     "",   "hce",   "*",     "",           "" }, 
    { SSC_INPUT,   SSC_ARRAY,   "HCE_A3",            "label",                                       "",     "",   "hce",   "*",     "",           "" }, 
    { SSC_INPUT,   SSC_ARRAY,   "HCE_A4",            "label",                                       "",     "",   "hce",   "*",     "",           "" }, 
    { SSC_INPUT,   SSC_ARRAY,   "HCE_A5",            "label",                                       "",     "",   "hce",   "*",     "",           "" }, 
    { SSC_INPUT,   SSC_ARRAY,   "HCE_A6",            "label",                                       "",     "",   "hce",   "*",     "",           "" }, 

	{ SSC_INPUT,   SSC_NUMBER,  "ui_reference_wind_speed",              "Wind speed for design heat loss",     "m/s",  "",   "hce",   "*",     "",           "" },
	{ SSC_INPUT,   SSC_NUMBER,  "SfOutTempD",                           "Solar Field Outlet Temp at design",   "C",    "",   "hce",   "*",     "",           "" },
	{ SSC_INPUT,   SSC_NUMBER,  "SfInTempD",                            "Solar Field Inlet Temp at design",    "C",    "",   "hce",   "*",     "",           "" },
	{ SSC_INPUT,   SSC_NUMBER,  "ui_reference_ambient_temperature",     "Ambient temp at design heat loss",    "C",    "",   "hce",   "*",     "",           "" },
	{ SSC_INPUT,   SSC_NUMBER,  "ui_reference_direct_normal_irradiance","DNI at design",                       "W/m2", "",   "hce",   "*",     "",           "" },

	{ SSC_OUTPUT,  SSC_ARRAY,   "HL",                "HCE Heat Losses",                             "W/m",  "",   "hce",   "*",     "",           "" }, 
	{ SSC_OUTPUT,  SSC_NUMBER,  "HL_weighted",       "Weighted HCE Heat Loss",                 	    "W/m",  "",   "hce",   "*",     "",           "" }, 
	{ SSC_OUTPUT,  SSC_NUMBER,  "HL_weighted_m2",    "Weighted HCE Heat Loss per Aperture Area",    "W/m2", "",   "hce",   "*",     "",           "" }, 

	var_info_invalid };

class cm_cb_empirical_hce_heat_loss : public compute_module
{
public:

	cm_cb_empirical_hce_heat_loss()
	{
		add_var_info(_cm_vtab_cb_empirical_hce_heat_loss);
	}

	void exec() override
	{
		std::vector<double> PerfFac;
		size_t n_PerfFac = 0;
		ssc_number_t *p_PerfFac = as_array("PerfFac", &n_PerfFac);
		
		std::vector<double> HCE_A0;
		size_t n_HCE_A0 = 0;
		ssc_number_t *p_HCE_A0 = as_array("HCE_A0", &n_HCE_A0);

		std::vector<double> HCE_A1;
		size_t n_HCE_A1 = 0;
		ssc_number_t *p_HCE_A1 = as_array("HCE_A1", &n_HCE_A1);

		std::vector<double> HCE_A2;
		size_t n_HCE_A2 = 0;
		ssc_number_t *p_HCE_A2 = as_array("HCE_A2", &n_HCE_A2);

		std::vector<double> HCE_A3;
		size_t n_HCE_A3 = 0;
		ssc_number_t *p_HCE_A3 = as_array("HCE_A3", &n_HCE_A3);

		std::vector<double> HCE_A4;
		size_t n_HCE_A4 = 0;
		ssc_number_t *p_HCE_A4 = as_array("HCE_A4", &n_HCE_A4);

		std::vector<double> HCE_A5;
		size_t n_HCE_A5 = 0;
		ssc_number_t *p_HCE_A5 = as_array("HCE_A5", &n_HCE_A5);

		std::vector<double> HCE_A6;
		size_t n_HCE_A6 = 0;
		ssc_number_t *p_HCE_A6 = as_array("HCE_A6", &n_HCE_A6);

		std::vector<double> HCEFrac;
		size_t n_HCEFrac = 0;
		ssc_number_t *p_HCEFrac = as_array("HCEFrac", &n_HCEFrac);

		std::vector<double> RefMirrAper;
		size_t n_RefMirrAper = 0;
		ssc_number_t *p_RefMirrAper = as_array("RefMirrAper", &n_RefMirrAper);

		// Check that all arrays are the same length
		if( n_PerfFac != n_HCE_A0 || n_PerfFac != n_HCE_A1 || n_PerfFac != n_HCE_A2 || n_PerfFac != n_HCE_A3
			|| n_PerfFac != n_HCE_A4 || n_PerfFac != n_HCE_A5 || n_PerfFac != n_HCE_A6 || n_PerfFac != n_HCEFrac
			|| n_PerfFac != n_RefMirrAper )
		{
			throw exec_error("Empirical trough HCE heat loss", "Not all HCE input arrays are the same length");
		}

		PerfFac.resize(n_PerfFac);
		HCE_A0.resize(n_PerfFac);
		HCE_A1.resize(n_PerfFac);
		HCE_A2.resize(n_PerfFac);
		HCE_A3.resize(n_PerfFac);
		HCE_A4.resize(n_PerfFac);
		HCE_A5.resize(n_PerfFac);
		HCE_A6.resize(n_PerfFac);
		HCEFrac.resize(n_PerfFac);
		RefMirrAper.resize(n_PerfFac);

		for(size_t i = 0; i < n_HCE_A0; i++)
		{
			PerfFac[i] = (double)p_PerfFac[i];
			HCE_A0[i] = (double)p_HCE_A0[i];
			HCE_A1[i] = (double)p_HCE_A1[i];
			HCE_A2[i] = (double)p_HCE_A2[i];
			HCE_A3[i] = (double)p_HCE_A3[i];
			HCE_A4[i] = (double)p_HCE_A4[i];
			HCE_A5[i] = (double)p_HCE_A5[i];
			HCE_A6[i] = (double)p_HCE_A6[i];
			HCEFrac[i] = (double)p_HCEFrac[i];
			RefMirrAper[i] = (double)p_RefMirrAper[i];
		}
	

		double HLWind = as_double("ui_reference_wind_speed");
		double T_amb = as_double("ui_reference_ambient_temperature");
		double I_bn = as_double("ui_reference_direct_normal_irradiance");

		double SfTo = as_double("SfOutTempD");
		double SfTi = as_double("SfInTempD");
		
		std::vector<double> HL(n_HCE_A0);

		double Rec_HL = 0.0;		//[W/m]
		double Rec_HL_m2 = 0.0;		//[W/m2]

		for(size_t i = 0; i < n_HCE_A0; i++)
		{
			if(SfTi >= SfTo)
				SfTo = SfTi + 0.1;		//HP: Keeps HL curve fits from blowing up
		
			double HLTerm1 = (HCE_A0[i] + HCE_A5[i]*sqrt(HLWind))*(SfTo - SfTi);
			double HLTerm2 = (HCE_A1[i] + HCE_A6[i]*sqrt(HLWind))*((pow(SfTo,2)-pow(SfTi,2))/2.0 - T_amb*(SfTo-SfTi));
			double HLTerm3 = (HCE_A2[i] + HCE_A4[i]*I_bn)/3.0*(pow(SfTo,3)-pow(SfTi,3));
			double HLTerm4 = HCE_A3[i]/4.0*(pow(SfTo,4)-pow(SfTi,4));

			HL[i] = (HLTerm1 + HLTerm2 + HLTerm3 + HLTerm4)/(SfTo - SfTi);	//[W/m]

			Rec_HL += PerfFac[i] * HCEFrac[i] * HL[i];		//[W/m]

			Rec_HL_m2 += PerfFac[i] * HCEFrac[i] * HL[i] / RefMirrAper[i];	//[W/m2]
		}

		ssc_number_t *p_HL = allocate("HL", n_HCE_A0);

		for(size_t i = 0; i < n_HCE_A0; i++)
		{
			p_HL[i] = (ssc_number_t)HL[i];
		}

		assign("HL_weighted", (ssc_number_t)Rec_HL);
		assign("HL_weighted_m2", (ssc_number_t)Rec_HL_m2);
	}

};

DEFINE_MODULE_ENTRY(cb_empirical_hce_heat_loss, "Empirical HCE Heat Loss", 0)