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

#ifndef _CMOD_PVAMV1_H_
#define _CMOD_PVAMV1_H_

#include <string>
#include <cmath>
#include <limits>
#include <vector>
#include <memory>

#include "core.h"
#include "common.h"
#include "cmod_battery.h"
#include "lib_power_electronics.h"
#include "lib_weatherfile.h"
#include "lib_irradproc.h"
#include "lib_cec6par.h"
#include "lib_sandia.h"
#include "lib_mlmodel.h"
#include "lib_ondinv.h"
#include "lib_pvinv.h"
#include "6par_jacobian.h"
#include "6par_lu.h"
#include "6par_search.h"
#include "6par_newton.h"
#include "6par_gamma.h"
#include "6par_solve.h"
#include "lib_pvshade.h"
#include "lib_snowmodel.h"
#include "lib_iec61853.h"
#include "lib_util.h"
#include "lib_pv_shade_loss_mpp.h"

// comment following define if do not want shading database validation outputs
//#define SHADE_DB_OUTPUTS

/**
* Detailed photovoltaic model in SAM, version 1
* Contains calculations to process a weather file, parse the irradiance, and evaluate PV subarray power production with AC or DC connected batteries
*/
class cm_pvsamv1 : public compute_module
{
public:
	
	//! PV model class constructor
	cm_pvsamv1();

	//! Setup the Nominal Operating Cell Temperature (NOCT) model
	void setup_noct_model(const std::string &prefix, noct_celltemp_t &noct_tc);
	
	//! Run the PV model
	void exec();
	
	//! Return the module efficiency
	double module_eff(int mod_type);

	//! Check the max inverter DC voltage
	void inverter_vdcmax_check();

	//! Check the inverter size and the associated clipping
	void inverter_size_check();
};

#endif // !_CMOD_PVAMV1_H_
