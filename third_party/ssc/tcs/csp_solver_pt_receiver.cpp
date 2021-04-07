/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (“Alliance”) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
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
*  the underlying software originally provided by Alliance as “System Advisor Model” or “SAM”. Except
*  to comply with the foregoing, the terms “System Advisor Model”, “SAM”, or any confusingly similar
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

#include "csp_solver_pt_receiver.h"
#include "csp_solver_core.h"

#include "Ambient.h"
#include "definitions.h"

C_pt_receiver::C_pt_receiver()
{
	m_h_tower = std::numeric_limits<double>::quiet_NaN();
    m_epsilon = std::numeric_limits<double>::quiet_NaN();
	m_T_htf_hot_des = std::numeric_limits<double>::quiet_NaN();
	m_T_htf_cold_des = std::numeric_limits<double>::quiet_NaN();
	m_f_rec_min = std::numeric_limits<double>::quiet_NaN();
	m_q_rec_des = std::numeric_limits<double>::quiet_NaN();
	m_rec_su_delay = std::numeric_limits<double>::quiet_NaN();
	m_rec_qf_delay = std::numeric_limits<double>::quiet_NaN();
	m_m_dot_htf_max_frac = std::numeric_limits<double>::quiet_NaN();

    m_eta_pump = std::numeric_limits<double>::quiet_NaN();
	m_night_recirc = -1;

	error_msg = "";
	m_m_dot_htf_des = std::numeric_limits<double>::quiet_NaN();
    m_mode = C_csp_collector_receiver::E_csp_cr_modes::OFF;
    m_mode_prev = C_csp_collector_receiver::E_csp_cr_modes::OFF;

	m_clearsky_model = -1;
	m_clearsky_data.resize(0);
}

int C_pt_receiver::get_operating_state()
{
    return m_mode_prev;
}

HTFProperties *C_pt_receiver::get_htf_property_object()
{
    return &field_htfProps;
}

double C_pt_receiver::get_startup_time()
{
    return m_rec_su_delay * 3600.; // sec
}

double C_pt_receiver::get_startup_energy()
{
    return m_rec_qf_delay * m_q_rec_des * 1.e-6;  // MWh
}

double C_pt_receiver::get_clearsky(const C_csp_weatherreader::S_outputs &weather, double hour)
{
	if (m_clearsky_model == -1 || weather.m_solzen >= 90.0)
		return 0.0;

	double clearsky;
	if (m_clearsky_model == 0)  // Use user-defined array
	{
		int nsteps = (int)m_clearsky_data.size();
		double baseline_step = 8760. / double(nsteps);  // Weather file time step size (hr)
		int step = (int)((hour - 1.e-6) / baseline_step);
		step = std::min(step, nsteps - 1);
		clearsky = m_clearsky_data.at(step);
	}
	else  // use methods in SolarPILOT
	{ 
		std::vector<int> monthlen{ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
		int doy = weather.m_day;
		int m = weather.m_month - 1;
		for (int j = 0; j < m; j++)
			doy += monthlen[j];

		double pres = weather.m_pres;
		if (pres < 20. && pres > 1.0)				// Some weather files seem to have inconsistent pressure units... make sure that value is of correct order of magnitude
			pres = weather.m_pres * 100.;			// convert to mbar
		double dpres = pres * 1.e-3 * 0.986923;		// Ambient pressure in atm
		double del_h2o = exp(0.058 * weather.m_tdew + 2.413);  // Correlation for precipitable water in mm H20 (from Choudhoury INTERNATIONAL JOURNAL OF CLIMATOLOGY, VOL. 16, 663-475 (1996))

		// Methods taken from SolarPilot Ambient class
		double S0 = 1.353 * (1. + .0335 * cos(2. * PI * (doy + 10.) / 365.));
		double zenith = weather.m_solzen * 3.14159 / 180.;
		double azimuth = weather.m_solazi * 3.14159 / 180.;
		double szen = sin(zenith);
		double czen = cos(zenith);
		double save2 = 90. - atan2(szen, czen) * R2D;
		double save = 1.0 / czen;
		if (save2 <= 30.)
			save = save - 41.972213 * pow(save2, (-2.0936381 - 0.04117341 * save2 + 0.000849854 * pow(save2, 2)));

		double alt = weather.m_elev / 1000.;
		double csky = 0.0;
		if (m_clearsky_model == 1)  // Meinel
			csky = (1. - .14 * alt) * exp(-.357 / pow(czen, .678)) + .14 * alt;
		else if (m_clearsky_model == 2)  // Hottel
			csky = 0.4237 - 0.00821 * pow(6. - alt, 2) + (0.5055 + 0.00595 * pow(6.5 - alt, 2)) * exp(-(0.2711 + 0.01858 * pow(2.5 - alt, 2)) / (czen + .00001));
		else if (m_clearsky_model == 3)  // Allen
			csky = 1.0 - 0.263 * ((del_h2o + 2.72) / (del_h2o + 5.0)) * pow((save * dpres), (0.367 * ((del_h2o + 11.53) / (del_h2o + 7.88))));
		else if (m_clearsky_model == 4)  // Moon
			csky = 0.183 * exp(-save * dpres / 0.48) + 0.715 * exp(-save * dpres / 4.15) + .102;

		clearsky = std::fmax(0.0, csky * S0 * 1000.);
	}

	return clearsky;
}
