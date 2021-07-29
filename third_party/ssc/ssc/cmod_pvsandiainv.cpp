/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (�Alliance�) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
*  The Government retains for itself and others acting on its behalf a nonexclusive, paid-up,
*  irrevocable worldwide license in the software to reproduce, prepare derivative works, distribute
*  copies to the public, perform publicly and display publicly, and to permit others to do so.
*
*  Redistribution and use in source and binary forms, with or without modification, are permitted
*  provided that the following conditions are met:
*
*  1. Redistributions of source code must retain the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer.
*
*  2. Redistributions in binary form must reproduce the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer in the documentation and/or
*  other materials provided with the distribution.
*
*  3. The entire corresponding source code of any redistribution, with or without modification, by a
*  research entity, including but not limited to any contracting manager/operator of a United States
*  National Laboratory, any institution of higher learning, and any non-profit organization, must be
*  made publicly available under this license for as long as the redistribution is made available by
*  the research entity.
*
*  4. Redistribution of this software, without modification, must refer to the software by the same
*  designation. Redistribution of a modified version of this software (i) may not refer to the modified
*  version by the same designation, or by any confusingly similar designation, and (ii) must refer to
*  the underlying software originally provided by Alliance as �System Advisor Model� or �SAM�. Except
*  to comply with the foregoing, the terms �System Advisor Model�, �SAM�, or any confusingly similar
*  designation may not be used to refer to any modified version of this software or any modified
*  version of the underlying software originally provided by Alliance without the prior written consent
*  of Alliance.
*
*  5. The name of the copyright holder, contributors, the United States Government, the United States
*  Department of Energy, or any of their employees may not be used to endorse or promote products
*  derived from this software without specific prior written permission.
*
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
*  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
*  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER,
*  CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR
*  EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
*  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
*  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
*  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
*  THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*******************************************************************************************************/

#include <math.h>

#ifndef M_PI
#define M_PI 3.141592653589793238462643
#endif

#include "core.h"
#include "lib_sandia.h"

static var_info _cm_vtab_pvsandiainv[] = {

/*   VARTYPE           DATATYPE         NAME                         LABEL                              UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
		
	{ SSC_INPUT,        SSC_ARRAY,       "dc",                      "DC power input to inverter",     "Watt",   "",                   "Sandia Inverter Model",      "*",                          "",                          "" },
	{ SSC_INPUT,        SSC_ARRAY,       "dc_voltage",              "DC voltage input to inverter",   "Volt",   "",                   "Sandia Inverter Model",      "*",                          "LENGTH_EQUAL=dc",                          "" },

	{ SSC_INPUT,        SSC_NUMBER,      "paco",                    "Max AC power rating",            "Wac",      "",                     "Sandia Inverter Model",      "*",                       "",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pdco",                    "DC power level at which Paco is achieved",    "Wdc",       "",                     "Sandia Inverter Model",      "*",                       "",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "vdco",                    "DV voltage level at which Paco is achieved",  "Volt",       "",                     "Sandia Inverter Model",      "*",                       "",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pso",                     "DC power level required to start inversion",  "Wdc",       "",                     "Sandia Inverter Model",      "*",                       "",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pntare",                  "Parasitic AC consumption",        "Wac",       "",                     "Sandia Inverter Model",      "*",                       "",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "c0",                      "C0: Defines parabolic curvature of relationship between ac power and dc power at reference conditions",    "1/W",    "",                     "Sandia Inverter Model",      "*",                       "",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "c1",                      "C1: Parameter allowing Pdco to vary linearly with dc voltage input",  "1/V",    "",                     "Sandia Inverter Model",      "*",                       "",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "c2",                      "C2: Parameter allowing Pso to vary linearly with dc voltage input ",  "1/V",    "",                     "Sandia Inverter Model",      "*",                       "",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "c3",                      "C3: Parameter allowing C0 to vary linearly with dc voltage input",    "1/V",    "",                     "Sandia Inverter Model",      "*",                       "",              "" },

	{ SSC_OUTPUT,       SSC_ARRAY,       "ac",                      "AC power output",                "Wac",    "",                      "Sandia Inverter Model",      "*",                       "LENGTH_EQUAL=dc",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "acpar",                   "AC parasitic power",             "Wac",    "",                      "Sandia Inverter Model",      "*",                       "LENGTH_EQUAL=dc",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "plr",                     "Part load ratio",                "0..1",   "",                      "Sandia Inverter Model",      "*",                       "LENGTH_EQUAL=dc",                          "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "eff_inv",                 "Conversion efficiency",          "0..1",   "",                      "Sandia Inverter Model",      "*",                       "LENGTH_EQUAL=dc",                          "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "cliploss",                 "Power loss due to clipping (Wac)", "Wac", "",                      "Sandia Inverter Model",      "*",                       "LENGTH_EQUAL=dc",                          "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "soloss",                  "Power loss due to operating power consumption (Wac)", "Wac",   "",  "Sandia Inverter Model",      "*",                       "LENGTH_EQUAL=dc",                          "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "ntloss",                  "Power loss due to night time tare loss (Wac)", "Wac",   "",         "Sandia Inverter Model",      "*",                       "LENGTH_EQUAL=dc",                          "" },

	var_info_invalid };

class cm_pvsandiainv : public compute_module
{
private:
public:
	cm_pvsandiainv()
	{
		add_var_info( _cm_vtab_pvsandiainv );
	}

	void exec( )
	{
		size_t arr_len;
		ssc_number_t *p_dcp = as_array( "dc", &arr_len );
		ssc_number_t *p_dcv = as_array( "dc_voltage", &arr_len );

		sandia_inverter_t inv;
	
		inv.Paco = as_double("paco");
		inv.Pdco = as_double("pdco");
		inv.Vdco = as_double("vdco");
		inv.Pso = as_double("pso");
		inv.Pntare = as_double("pntare");
		inv.C0 = as_double("c0");
		inv.C1 = as_double("c1");
		inv.C2 = as_double("c2");
		inv.C3 = as_double("c3");
		
		ssc_number_t *p_ac = allocate("ac", arr_len);
		ssc_number_t *p_acpar = allocate("acpar", arr_len);
		ssc_number_t *p_plr = allocate("plr", arr_len);
		ssc_number_t *p_eff = allocate("eff_inv", arr_len);
		ssc_number_t *p_cliploss = allocate("cliploss", arr_len);
		ssc_number_t *p_soloss = allocate("soloss", arr_len);
		ssc_number_t *p_ntloss = allocate("ntloss", arr_len);

		for (size_t i = 0; i < arr_len; i++ )
		{
			double pac, ppar, plr, eta, pcliploss, psoloss, pntloss;
			if ( !inv.acpower( p_dcp[i], p_dcv[i], &pac, &ppar, &plr, &eta, &pcliploss, &psoloss, &pntloss ) ) throw general_error("sandia inverter model calculation error with given inputs", (float) i);

			p_ac[i] = (ssc_number_t) pac;
			p_acpar[i] = (ssc_number_t) ppar;
			p_plr[i] = (ssc_number_t) plr;
			p_eff[i] = (ssc_number_t) eta;
			p_cliploss[i] = (ssc_number_t) pcliploss;
			p_soloss[i] = (ssc_number_t) psoloss;
			p_ntloss[i] = (ssc_number_t) pntloss;
		}
	}
};

DEFINE_MODULE_ENTRY( pvsandiainv, "Sandia PV inverter performance calculator.", 1 )
