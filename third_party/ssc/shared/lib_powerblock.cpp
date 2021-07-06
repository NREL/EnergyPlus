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

#include "lib_powerblock.h"
#include <cmath>
#include <limits>
#include <math.h>

#include "lib_util.h"

// responsibility of developers of this file to fix these warnings
//#pragma warning (disable : 4458 ) // https://docs.microsoft.com/en-us/cpp/error-messages/compiler-warnings/compiler-warning-level-4-c4458?view=vs-2017

CPowerBlock_Type224::CPowerBlock_Type224()
{	// inputs
	m_pbi.demand_var = 0;
	m_pbi.m_dot_htf = 0;
	m_pbi.mode = 0;
	m_pbi.P_amb = 0;
	m_pbi.standby_control = 0;
	m_pbi.T_db = 0;
	m_pbi.T_htf_hot = 0;
	m_pbi.T_wb = 0;
	m_pbi.TOU = 0;

	// outputs
	m_pbo.P_cycle = 0.0;
	m_pbo.eta = 0.0;
	m_pbo.T_htf_cold = 0;
	m_pbo.m_dot_demand = 0.0;
	m_pbo.m_dot_makeup = 0.0;
	m_pbo.W_cool_par = 0.0;
	m_pbo.f_hrsys = 0.0;
	m_pbo.P_cond = 0.0;
	m_pbo.m_dot_htf = 0;
	m_pbo.m_dot_htf_ref = 0;
	m_pbo.P_ref = 0;

	m_bInitialized = false;
	m_bFirstCall = true;

	m_dDeltaEnthalpySteam = 0;
	m_iLastStandbyControl = 0;
	m_dLastPCycle = 0;
	m_dStartupEnergy = 0;
	m_dStartupERemain = 0;
	m_dStartupRemain = 0;
	m_lCurrentSecondsFromStart = 0;					// hours
	m_F_wcMax = 0;
	m_F_wcMin = 0;
	m_strLastError = "";
	m_strWarningMsg = "";

	m_sv.dLastP_Cycle=0;
	m_sv.dStartupEnergyRemaining=0;
	m_sv.dStartupTimeRemaining=0;
	m_sv.iLastStandbyControl=1;
}

CPowerBlock_Type224::~CPowerBlock_Type224()
{

}

//************************************************************************************************************
//************************************************************************************************************
bool CPowerBlock_Type224::InitializeForParameters(const SPowerBlockParameters& pbp)
{
	m_bInitialized = false;
	m_pbp = pbp;

	if (m_pbp.tech_type == 1)
	{	//	Power tower applications
		double dTemp[18][20] =
		{
			{0.20000, 0.25263, 0.30526, 0.35789, 0.41053, 0.46316, 0.51579, 0.56842, 0.62105, 0.67368, 0.72632, 0.77895, 0.83158, 0.88421, 0.93684, 0.98947, 1.04211, 1.09474, 1.14737, 1.20000},
			{0.16759, 0.21750, 0.26932, 0.32275, 0.37743, 0.43300, 0.48910, 0.54545, 0.60181, 0.65815, 0.71431, 0.77018, 0.82541, 0.88019, 0.93444, 0.98886, 1.04378, 1.09890, 1.15425, 1.20982},
			{0.19656, 0.24969, 0.30325, 0.35710, 0.41106, 0.46497, 0.51869, 0.57215, 0.62529, 0.67822, 0.73091, 0.78333, 0.83526, 0.88694, 0.93838, 0.98960, 1.04065, 1.09154, 1.14230, 1.19294},
			{3000.00, 4263.16, 5526.32, 6789.47, 8052.63, 9315.79, 10578.95, 11842.11, 13105.26, 14368.42, 15631.58, 16894.74, 18157.89, 19421.05, 20684.21, 21947.37, 23210.53, 24473.68, 25736.84, 27000.00},
			{1.07401, 1.04917, 1.03025, 1.01488, 1.00201, 0.99072, 0.98072, 0.97174, 0.96357, 0.95607, 0.94914, 0.94269, 0.93666, 0.93098, 0.92563, 0.92056, 0.91573, 0.91114, 0.90675, 0.90255},
			{1.00880, 1.00583, 1.00355, 1.00168, 1.00010, 0.99870, 0.99746, 0.99635, 0.99532, 0.99438, 0.99351, 0.99269, 0.99193, 0.99121, 0.99052, 0.98988, 0.98926, 0.98867, 0.98810, 0.98756},
			{0.10000, 0.17368, 0.24737, 0.32105, 0.39474, 0.46842, 0.54211, 0.61579, 0.68947, 0.76316, 0.83684, 0.91053, 0.98421, 1.05789, 1.13158, 1.20526, 1.27895, 1.35263, 1.42632, 1.50000},
			{0.09403, 0.16542, 0.23861, 0.31328, 0.38901, 0.46540, 0.54203, 0.61849, 0.69437, 0.76928, 0.84282, 0.91458, 0.98470, 1.05517, 1.12536, 1.19531, 1.26502, 1.33450, 1.40376, 1.47282},
			{0.10659, 0.18303, 0.25848, 0.33316, 0.40722, 0.48075, 0.55381, 0.62646, 0.69873, 0.77066, 0.84228, 0.91360, 0.98464, 1.05542, 1.12596, 1.19627, 1.26637, 1.33625, 1.40593, 1.47542},
			{0.20000, 0.25263, 0.30526, 0.35789, 0.41053, 0.46316, 0.51579, 0.56842, 0.62105, 0.67368, 0.72632, 0.77895, 0.83158, 0.88421, 0.93684, 0.98947, 1.04211, 1.09474, 1.14737, 1.20000},
			{1.03323, 1.04058, 1.04456, 1.04544, 1.04357, 1.03926, 1.03282, 1.02446, 1.01554, 1.00944, 1.00487, 1.00169, 0.99986, 0.99926, 0.99980, 1.00027, 1.00021, 1.00015, 1.00006, 0.99995},
			{0.98344, 0.98630, 0.98876, 0.99081, 0.99247, 0.99379, 0.99486, 0.99574, 0.99649, 0.99716, 0.99774, 0.99826, 0.99877, 0.99926, 0.99972, 1.00017, 1.00060, 1.00103, 1.00143, 1.00182},
			{3000.00, 4263.16, 5526.32, 6789.47, 8052.63, 9315.79, 10578.95, 11842.11, 13105.26, 14368.42, 15631.58, 16894.74, 18157.89, 19421.05, 20684.21, 21947.37, 23210.53, 24473.68, 25736.84, 27000.00},
			{0.99269, 0.99520, 0.99718, 0.99882, 1.00024, 1.00150, 1.00264, 1.00368, 1.00464, 1.00554, 1.00637, 1.00716, 1.00790, 1.00840, 1.00905, 1.00965, 1.01022, 1.01075, 1.01126, 1.01173},
			{0.99768, 0.99861, 0.99933, 0.99992, 1.00043, 1.00087, 1.00127, 1.00164, 1.00197, 1.00227, 1.00255, 1.00282, 1.00307, 1.00331, 1.00353, 1.00375, 1.00395, 1.00415, 1.00433, 1.00451},
			{0.10000, 0.17368, 0.24737, 0.32105, 0.39474, 0.46842, 0.54211, 0.61579, 0.68947, 0.76316, 0.83684, 0.91053, 0.98421, 1.05789, 1.13158, 1.20526, 1.27895, 1.35263, 1.42632, 1.50000},
			{1.00812, 1.00513, 1.00294, 1.00128, 0.99980, 0.99901, 0.99855, 0.99836, 0.99846, 0.99883, 0.99944, 1.00033, 1.00042, 1.00056, 1.00069, 1.00081, 1.00093, 1.00104, 1.00115, 1.00125},
			{1.09816, 1.07859, 1.06487, 1.05438, 1.04550, 1.03816, 1.03159, 1.02579, 1.02061, 1.01587, 1.01157, 1.00751, 1.00380, 1.00033, 0.99705, 0.99400, 0.99104, 0.98832, 0.98565, 0.98316}
		};
		m_db.assign(dTemp[0],18,20);
	}

	else if (m_pbp.tech_type == 2)
	{	//  Low temperature parabolic trough applications
		double dTemp[18][20] =
		{
			{0.10000, 0.16842, 0.23684, 0.30526, 0.37368, 0.44211, 0.51053, 0.57895, 0.64737, 0.71579, 0.78421, 0.85263, 0.92105, 0.98947, 1.05789, 1.12632, 1.19474, 1.26316, 1.33158, 1.40000},
			{0.08547, 0.14823, 0.21378, 0.28166, 0.35143, 0.42264, 0.49482, 0.56747, 0.64012, 0.71236, 0.78378, 0.85406, 0.92284, 0.98989, 1.05685, 1.12369, 1.19018, 1.25624, 1.32197, 1.38744},
			{0.10051, 0.16934, 0.23822, 0.30718, 0.37623, 0.44534, 0.51443, 0.58338, 0.65209, 0.72048, 0.78848, 0.85606, 0.92317, 0.98983, 1.05604, 1.12182, 1.18718, 1.25200, 1.31641, 1.38047},
			{3000.00, 4263.16, 5526.32, 6789.47, 8052.63, 9315.79, 10578.95, 11842.11, 13105.26, 14368.42, 15631.58, 16894.74, 18157.89, 19421.05, 20684.21, 21947.37, 23210.53, 24473.68, 25736.84, 27000.00},
			{1.08827, 1.06020, 1.03882, 1.02145, 1.00692, 0.99416, 0.98288, 0.97273, 0.96350, 0.95504, 0.94721, 0.93996, 0.93314, 0.92673, 0.92069, 0.91496, 0.90952, 0.90433, 0.89938, 0.89464},
			{1.01276, 1.00877, 1.00570, 1.00318, 1.00106, 0.99918, 0.99751, 0.99601, 0.99463, 0.99335, 0.99218, 0.99107, 0.99004, 0.98907, 0.98814, 0.98727, 0.98643, 0.98563, 0.98487, 0.98413},
			{0.10000, 0.17368, 0.24737, 0.32105, 0.39474, 0.46842, 0.54211, 0.61579, 0.68947, 0.76316, 0.83684, 0.91053, 0.98421, 1.05789, 1.13158, 1.20526, 1.27895, 1.35263, 1.42632, 1.50000},
			{0.09307, 0.16421, 0.23730, 0.31194, 0.38772, 0.46420, 0.54098, 0.61763, 0.69374, 0.76896, 0.84287, 0.91511, 0.98530, 1.05512, 1.12494, 1.19447, 1.26373, 1.33273, 1.40148, 1.46999},
			{0.10741, 0.18443, 0.26031, 0.33528, 0.40950, 0.48308, 0.55610, 0.62861, 0.70066, 0.77229, 0.84354, 0.91443, 0.98497, 1.05520, 1.12514, 1.19478, 1.26416, 1.33329, 1.40217, 1.47081},
			{0.10000, 0.16842, 0.23684, 0.30526, 0.37368, 0.44211, 0.51053, 0.57895, 0.64737, 0.71579, 0.78421, 0.85263, 0.92105, 0.98947, 1.05789, 1.12632, 1.19474, 1.26316, 1.33158, 1.40000},
			{1.01749, 1.03327, 1.04339, 1.04900, 1.05051, 1.04825, 1.04249, 1.03343, 1.02126, 1.01162, 1.00500, 1.00084, 0.99912, 0.99966, 0.99972, 0.99942, 0.99920, 0.99911, 0.99885, 0.99861},
			{0.99137, 0.99297, 0.99431, 0.99564, 0.99681, 0.99778, 0.99855, 0.99910, 0.99948, 0.99971, 0.99984, 0.99989, 0.99993, 0.99993, 0.99992, 0.99992, 0.99992, 1.00009, 1.00010, 1.00012},
			{3000.00, 4263.16, 5526.32, 6789.47, 8052.63, 9315.79, 10578.95, 11842.11, 13105.26, 14368.42, 15631.58, 16894.74, 18157.89, 19421.05, 20684.21, 21947.37, 23210.53, 24473.68, 25736.84, 27000.00},
			{0.99653, 0.99756, 0.99839, 0.99906, 0.99965, 1.00017, 1.00063, 1.00106, 1.00146, 1.00183, 1.00218, 1.00246, 1.00277, 1.00306, 1.00334, 1.00361, 1.00387, 1.00411, 1.00435, 1.00458},
			{0.99760, 0.99831, 0.99888, 0.99934, 0.99973, 1.00008, 1.00039, 1.00067, 1.00093, 1.00118, 1.00140, 1.00161, 1.00180, 1.00199, 1.00217, 1.00234, 1.00250, 1.00265, 1.00280, 1.00294},
			{0.10000, 0.17368, 0.24737, 0.32105, 0.39474, 0.46842, 0.54211, 0.61579, 0.68947, 0.76316, 0.83684, 0.91053, 0.98421, 1.05789, 1.13158, 1.20526, 1.27895, 1.35263, 1.42632, 1.50000},
			{1.01994, 1.01645, 1.01350, 1.01073, 1.00801, 1.00553, 1.00354, 1.00192, 1.00077, 0.99995, 0.99956, 0.99957, 1.00000, 0.99964, 0.99955, 0.99945, 0.99937, 0.99928, 0.99919, 0.99918},
			{1.02055, 1.01864, 1.01869, 1.01783, 1.01508, 1.01265, 1.01031, 1.00832, 1.00637, 1.00454, 1.00301, 1.00141, 1.00008, 0.99851, 0.99715, 0.99586, 0.99464, 0.99347, 0.99227, 0.99177}	
		};
		m_db.assign(dTemp[0],18,20);
	}

	else if (m_pbp.tech_type == 3)
	{	//  Sliding pressure power cycle formulation

		double dTemp[18][10] =
		{
			{0.10000, 0.21111, 0.32222, 0.43333, 0.54444, 0.65556, 0.76667, 0.87778, 0.98889, 1.10000},
			{0.89280, 0.90760, 0.92160, 0.93510, 0.94820, 0.96110, 0.97370, 0.98620, 0.99860, 1.01100},
			{0.93030, 0.94020, 0.94950, 0.95830, 0.96690, 0.97520, 0.98330, 0.99130, 0.99910, 1.00700},
			{4000.00, 6556.00, 9111.00, 11677.0, 14222.0, 16778.0, 19333.0, 21889.0, 24444.0, 27000.0},
			{1.04800, 1.01400, 0.99020, 0.97140, 0.95580, 0.94240, 0.93070, 0.92020, 0.91060, 0.90190},
			{0.99880, 0.99960, 1.00000, 1.00100, 1.00100, 1.00100, 1.00100, 1.00200, 1.00200, 1.00200},
			{0.20000, 0.31667, 0.43333, 0.55000, 0.66667, 0.78333, 0.90000, 1.01667, 1.13333, 1.25000},
			{0.16030, 0.27430, 0.39630, 0.52310, 0.65140, 0.77820, 0.90060, 1.01600, 1.12100, 1.21400},
			{0.22410, 0.34700, 0.46640, 0.58270, 0.69570, 0.80550, 0.91180, 1.01400, 1.11300, 1.20700},
			{0.10000, 0.21111, 0.32222, 0.43333, 0.54444, 0.65556, 0.76667, 0.87778, 0.98889, 1.10000},
			{1.05802, 1.05127, 1.04709, 1.03940, 1.03297, 1.02480, 1.01758, 1.00833, 1.00180, 0.99307},
			{1.03671, 1.03314, 1.02894, 1.02370, 1.01912, 1.01549, 1.01002, 1.00486, 1.00034, 0.99554},
			{4000.00, 6556.00, 9111.00, 11677.0, 14222.0, 16778.0, 19333.0, 21889.0, 24444.0, 27000.0},
			{1.00825, 0.98849, 0.99742, 1.02080, 1.02831, 1.03415, 1.03926, 1.04808, 1.05554, 1.05862},
			{1.01838, 1.02970, 0.99785, 0.99663, 0.99542, 0.99183, 0.98897, 0.99299, 0.99013, 0.98798}, // tweaked entry #4 to be the average of 3 and 5. it was an outlier in the simulation. mjw 3.31.11
			{0.20000, 0.31667, 0.43333, 0.55000, 0.66667, 0.78333, 0.90000, 1.01667, 1.13333, 1.25000},
			{1.43311, 1.27347, 1.19090, 1.13367, 1.09073, 1.05602, 1.02693, 1.00103, 0.97899, 0.95912},
			{0.48342, 0.64841, 0.64322, 0.74366, 0.76661, 0.82764, 0.97792, 1.15056, 1.23117, 1.31179}  // tweaked entry #9 to be the average of 8 and 10. it was an outlier in the simulation mjw 3.31.11
		};
		m_db.assign(dTemp[0],18,10);
	}

	else if (m_pbp.tech_type == 4)
	{	//	Geothermal applications - Isopentane Rankine cycle
		double dTemp[18][20] =
		{
			{0.50000, 0.53158, 0.56316, 0.59474, 0.62632, 0.65789, 0.68947, 0.72105, 0.75263, 0.78421, 0.81579, 0.84737, 0.87895, 0.91053, 0.94211, 0.97368, 1.00526, 1.03684, 1.06842, 1.10000},
			{0.55720, 0.58320, 0.60960, 0.63630, 0.66330, 0.69070, 0.71840, 0.74630, 0.77440, 0.80270, 0.83130, 0.85990, 0.88870, 0.91760, 0.94670, 0.97570, 1.00500, 1.03400, 1.06300, 1.09200},
			{0.67620, 0.69590, 0.71570, 0.73570, 0.75580, 0.77600, 0.79630, 0.81670, 0.83720, 0.85780, 0.87840, 0.89910, 0.91990, 0.94070, 0.96150, 0.98230, 1.00300, 1.02400, 1.04500, 1.06600},
			{35000.00, 46315.79, 57631.58, 68947.37, 80263.16, 91578.95, 102894.74, 114210.53, 125526.32, 136842.11, 148157.89, 159473.68, 170789.47, 182105.26, 193421.05, 204736.84, 216052.63, 227368.42, 238684.21, 250000.00},
			{1.94000, 1.77900, 1.65200, 1.54600, 1.45600, 1.37800, 1.30800, 1.24600, 1.18900, 1.13700, 1.08800, 1.04400, 1.00200, 0.96290, 0.92620, 0.89150, 0.85860, 0.82740, 0.79770, 0.76940},
			{1.22400, 1.19100, 1.16400, 1.14000, 1.11900, 1.10000, 1.08300, 1.06700, 1.05200, 1.03800, 1.02500, 1.01200, 1.00000, 0.98880, 0.97780, 0.96720, 0.95710, 0.94720, 0.93770, 0.92850},
			{0.80000, 0.81316, 0.82632, 0.83947, 0.85263, 0.86579, 0.87895, 0.89211, 0.90526, 0.91842, 0.93158, 0.94474, 0.95789, 0.97105, 0.98421, 0.99737, 1.01053, 1.02368, 1.03684, 1.05000},
			{0.84760, 0.85880, 0.86970, 0.88050, 0.89120, 0.90160, 0.91200, 0.92210, 0.93220, 0.94200, 0.95180, 0.96130, 0.97080, 0.98010, 0.98920, 0.99820, 1.00700, 1.01600, 1.02400, 1.03300},
			{0.89590, 0.90350, 0.91100, 0.91840, 0.92570, 0.93290, 0.93990, 0.94680, 0.95370, 0.96040, 0.96700, 0.97350, 0.97990, 0.98620, 0.99240, 0.99850, 1.00400, 1.01000, 1.01500, 1.02100},
			{0.50000, 0.53158, 0.56316, 0.59474, 0.62632, 0.65789, 0.68947, 0.72105, 0.75263, 0.78421, 0.81579, 0.84737, 0.87895, 0.91053, 0.94211, 0.97368, 1.00526, 1.03684, 1.06842, 1.10000},
			{0.79042, 0.80556, 0.82439, 0.84177, 0.85786, 0.87485, 0.88898, 0.90182, 0.91783, 0.93019, 0.93955, 0.95105, 0.96233, 0.97150, 0.98059, 0.98237, 0.99829, 1.00271, 1.02084, 1.02413},
			{0.67400, 0.69477, 0.71830, 0.73778, 0.75991, 0.78079, 0.80052, 0.82622, 0.88152, 0.92737, 0.93608, 0.94800, 0.95774, 0.96653, 0.97792, 0.99852, 0.99701, 1.01295, 1.02825, 1.04294},
			{35000.00, 46315.79, 57631.58, 68947.37, 80263.16, 91578.95, 102894.74, 114210.53, 125526.32, 136842.11, 148157.89, 159473.68, 170789.47, 182105.26, 193421.05, 204736.84, 216052.63, 227368.42, 238684.21, 250000.00},
			{0.80313, 0.82344, 0.83980, 0.86140, 0.87652, 0.89274, 0.91079, 0.92325, 0.93832, 0.95229, 0.97004, 0.98211, 1.00399, 1.01514, 1.03494, 1.04962, 1.06646, 1.08374, 1.10088, 1.11789},
			{0.93426, 0.94458, 0.94618, 0.95878, 0.96352, 0.96738, 0.97058, 0.98007, 0.98185, 0.99048, 0.99144, 0.99914, 1.00696, 1.00849, 1.01573, 1.01973, 1.01982, 1.02577, 1.02850, 1.03585},
			{0.80000, 0.81316, 0.82632, 0.83947, 0.85263, 0.86579, 0.87895, 0.89211, 0.90526, 0.91842, 0.93158, 0.94474, 0.95789, 0.97105, 0.98421, 0.99737, 1.01053, 1.02368, 1.03684, 1.05000},
			{1.06790, 1.06247, 1.05688, 1.05185, 1.04687, 1.04230, 1.03748, 1.03281, 1.02871, 1.02473, 1.02050, 1.01639, 1.01204, 1.00863, 1.00461, 1.00051, 0.99710, 0.99352, 0.98974, 0.98692},
			{1.02335, 1.02130, 1.02041, 1.01912, 1.01655, 1.01601, 1.01379, 1.01431, 1.01321, 1.01207, 1.01129, 1.00784, 1.00548, 1.00348, 1.00183, 0.99982, 0.99698, 0.99457, 0.99124, 0.99016}
		};
		m_db.assign(dTemp[0],18,20);
	}
	else
	{
        m_strLastError = "Power block (Type 224) encountered an unkown technology type when trying to initialize.";
		return false;
	}

    // READ IN THE VALUES OF THE PARAMETERS IN SEQUENTIAL ORDER
    m_pbp.P_ref = m_pbp.P_ref*1000;  //P_ref = PAR(1)*1000. !Convert from MW to kW
    //eta_ref = PAR(2)
    //T_htf_hot_ref = PAR(3)
    //T_htf_cold_ref = PAR(4)
    //dT_cw_ref = PAR(5)
    //T_amb_des = PAR(6)
    //HTF=PAR(7)
    //q_sby_frac=PAR(8)
    //P_boil=PAR(9)
    //CT=PAR(10) !Cooling type
    //startup_time=PAR(11)
    //startup_frac=PAR(12)
    //tech_type=PAR(13)
    //T_approach=PAR(14)
    //T_ITD_des=PAR(15)
    //P_cond_ratio=PAR(16)
    //pb_bd_frac=PAR(17)
    //LU_pb = int(PAR(18))  !This sets the value in the global_props module
	m_pbp.P_cond_min = physics::InHgToPa(m_pbp.P_cond_min);  //P_cond_min = PAR(19)*3386. !Convert inHg to Pa
    //n_pl_inc = PAR(20)

    // find min and max hybrid cooling dispatch fractions
    for (int i=0; i<9; i++)
	{
		m_F_wcMax = dmax1(m_F_wcMax, m_pbp.F_wc[i]);
		m_F_wcMin = dmin1(m_F_wcMin, m_pbp.F_wc[i]);
	}

	if ( (m_F_wcMax > 1.0) || (m_F_wcMin < 0.0) )
	{
        m_strLastError = "Hybrid dispatch values must be between zero and one.";
		return false;
	}

    // Calculate the power block side steam enthalpy rise for blowdown calculations
    // Steam properties are as follows:
    // =======================================================================
    //  | T(C) | P(MPa) | h(kJ/kg) | s(kJ/kg-K) | x(-) | v(m3/kg) | U(kJ/kg) |
    // =======================================================================
    // Limit the boiler pressure to below the supercritical point.  If a supercritical pressure is used,
    // notify the user that the pressure value is being switched.
    if(m_pbp.P_boil > 220.0)
	{
        m_pbp.P_boil = 220.0;  // Set to 220 bar, 22 MPa
        m_strWarningMsg = "Boiler pressure provided by the user requires a supercritical system. The pressure value has been reset to 220 bar.";
	}

	double h_st_hot, h_st_cold;
    // hot steam
    //prop = (/(T_htf_hot_ref - GetFieldToTurbineTemperatureDropC()),(P_boil*0.1d0),1.d0,1.d0,1.d0,1.d0,1.d0/)
    //call steam_props("SI",prop,12,ierr)
    //h_st_hot = prop(3) // Use the hot steam enthalpy
	if(!physics::EnthalpyFromTempAndPressure((m_pbp.T_htf_hot_ref - GetFieldToTurbineTemperatureDropC())+273.15, m_pbp.P_boil, h_st_hot))
	{
        m_strLastError = "Could not calculate the enthalpy for the given temperature and pressure.";
		return false;
	}
    //prop = (/1.d0,(P_boil*0.1d0),1.d0,1.d0,0.d0,1.d0,1.d0/)
    //call steam_props("SI",prop,25,ierr)
    // Use the cold steam enthalpy at x=0, subtract subcooled enthalpy with specific heat. Cp is based
    // on an integral of specific heat for water in the slightly subcooled range of ~100degC
    //h_st_cold = prop(3) - 4.91*100.
	if(!physics::EnthalpyFromTempAndPressure(274+273.15, m_pbp.P_boil, h_st_cold))
	{
        m_strLastError = "Could not calculate the enthalpy for the given temperature and pressure.";
		return false;
	}
    h_st_cold = h_st_cold - 4.91*100.0;
	m_dDeltaEnthalpySteam = (h_st_hot - h_st_cold);

    // 8.30.2010 :: Calculate the startup energy needed
    m_dStartupEnergy = m_pbp.startup_frac * m_pbp.P_ref / m_pbp.eta_ref; // [kWt]

	m_bInitialized = true;
	return true;
}

//************************************************************************************************************
//************************************************************************************************************
bool CPowerBlock_Type224::SetNewTime(const long lTimeInSeconds)
{
	if (lTimeInSeconds < m_lCurrentSecondsFromStart)
	{
        m_strLastError = "New time was earlier than the last time.";
		return false;
	}

	if (lTimeInSeconds > m_lCurrentSecondsFromStart) Step(lTimeInSeconds);
	return true;
}
//************************************************************************************************************
//************************************************************************************************************
void CPowerBlock_Type224::Step(const long lNewSecondsFromStart)
{
	// moving to next time step, power block code used hours, so calculate it
	m_dHoursSinceLastStep = (lNewSecondsFromStart - m_lCurrentSecondsFromStart)/3600;
	m_lCurrentSecondsFromStart = lNewSecondsFromStart;

	// Execute may get called several times for the same time step, these only get saved once we've started the next time step
	m_sv.iLastStandbyControl = m_pbi.standby_control;									// STORED(1)=standby_control
	m_sv.dStartupTimeRemaining = dmax1(m_dStartupRemain - m_dHoursSinceLastStep, 0.0);	// STORED(2)=dmax1(startup_remain-tstep,0.d0)
	// not used in Fortran code, nothing to do here										// STORED(3)=P_cycle
	m_sv.dStartupEnergyRemaining = m_dStartupERemain;									// STORED(4)= startup_e_remain
	// handled with m_bFirstCall, nothing to do here									// STORED(5)=fcall

	return;
}

//************************************************************************************************************
//************************************************************************************************************
bool CPowerBlock_Type224::Execute(const long lSecondsFromStart, const SPowerBlockInputs& pbi)
{
	if (!m_bInitialized) return false;
	if (!SetNewTime(lSecondsFromStart)) return false;
	if ( (pbi.TOU < 0) || (pbi.TOU > 8) )
	{
        m_strLastError = "The power block inputs contained an invalid time-of-use period. The value encountered was " + util::to_string(pbi.TOU) + " and it should be >=0 and <=8.";
		return false;
	}
	m_pbi = pbi;

	//real(8):: P_cycle, eta, T_htf_cold, m_dot_demand,m_dot_htf_ref, q_ND_tot, c_p_w, &
	//		  q_sby_needed, m_dot_sby, c_htf, q_tot, specheat, T_cw_in, last_standby_control, tstep, startup_remain ,&
	//		  tech_type, W_cool_par, m_dot_makeup, F_wcmin, F_wcmax, P_cond, startup_energy, startup_e_remain, &
	//		  startup_e_used, Q_cycle, f_st, f_restart, rh
	//real(8):: xin, out, time, par, stored, T, dTdt, prop(7), pb_bd_frac, h_st_hot, h_st_cold, dh_steam, m_dot_st_bd, f_hrsys, fcall
	//integer(4)::INFO(15), np, ni, nout, nd, npar, nin, nder, iunit, itype, icntrl, nstored, ierr, TOU, i

	// RETRIEVE THE VALUES IN THE STORAGE ARRAY FOR THIS ITERATION
	// CALL getStorageVars(STORED,NSTORED,INFO)
	m_iLastStandbyControl = m_sv.iLastStandbyControl;			//last_standby_control=STORED(1)
	m_dStartupRemain = m_sv.dStartupTimeRemaining;				//startup_remain=STORED(2)
	//last_P_cycle=STORED(3)
	m_dStartupERemain = m_sv.dStartupEnergyRemaining;			//startup_e_remain=STORED(4)
	//fcall = STORED(5)

	// Get inputs
	//mode = XIN(1)
	//T_htf_hot = XIN(2)
	//m_dot_htf = XIN(3)
	m_pbi.T_wb = physics::CelciusToKelvin(m_pbi.T_wb); // XIN(4) + 273.15      // Convert to K
	//demand_var = XIN(5)
	//standby_control = XIN(6)
	m_pbi.T_db = physics::CelciusToKelvin(m_pbi.T_db); // XIN(7) + 273.15      // Convert to K
	m_pbi.P_amb = physics::AtmToPa(m_pbi.P_amb); // = xin(8)*101300. // [atm] -> [Pa]
	//TOU = int(xin(9))
	//rh = xin(10)  !relative humidity
	//f_restart = xin(11)  !fraction of the hour that the turbine can operate during restart

    //IUNIT=INFO(1)
    //ITYPE=INFO(2)
    if(m_pbi.mode == 1) m_pbi.demand_var = m_pbi.demand_var * 1000.0;  // If the mode is to operate in power demand, convert from MW to kW
	
	//if(info(7)>10) goto 900 // MJW 12.10.2010 Don't recalculate

	// Specific heat liquid of water
	//c_p_w = physics::SPECIFIC_HEAT_LIQUID_WATER; // 4.183  // [kJ/kg-C]
	double m_dot_st_bd = 0.0;

	switch (m_pbi.standby_control)
	{
		case 1:  // The cycle is in normal operation
			RankineCycle(m_pbp.P_ref, m_pbp.eta_ref, m_pbp.T_htf_hot_ref, m_pbp.T_htf_cold_ref, m_pbi.T_db, m_pbi.T_wb, m_pbi.P_amb, m_pbp.dT_cw_ref, physics::SPECIFIC_HEAT_LIQUID_WATER, 
						 m_pbi.T_htf_hot, m_pbi.m_dot_htf, m_pbi.mode, m_pbi.demand_var, m_pbp.P_boil, m_pbp.T_amb_des, m_pbp.T_approach, m_pbp.F_wc[m_pbi.TOU],
						 m_F_wcMin, m_F_wcMax, m_pbp.T_ITD_des, m_pbp.P_cond_ratio, m_pbp.P_cond_min,
						 m_pbo.P_cycle, m_pbo.eta, m_pbo.T_htf_cold, m_pbo.m_dot_demand, m_pbo.m_dot_htf_ref, m_pbo.m_dot_makeup, m_pbo.W_cool_par, m_pbo.f_hrsys, m_pbo.P_cond);

			// Check the output to make sure it's reasonable. If not, return zeros.
			if ( ((m_pbo.eta > 1.0) || (m_pbo.eta < 0.0)) || ((m_pbo.T_htf_cold > m_pbi.T_htf_hot) || (m_pbo.T_htf_cold < m_pbp.T_htf_cold_ref - 50.0)) )
			{
				m_pbo.P_cycle = 0.0;
				m_pbo.eta = 0.0;
				m_pbo.T_htf_cold = m_pbp.T_htf_cold_ref;
			}

			// -----Calculate the blowdown fraction-----
			if (m_pbp.tech_type != 4)
				m_dot_st_bd = m_pbo.P_cycle/dmax1((m_pbo.eta * m_dDeltaEnthalpySteam), 1.e-6) * m_pbp.pb_bd_frac;
			else
				m_dot_st_bd = 0; // Added Aug 3, 2011 for Isopentane Rankine cycle
			break;

		case 2: {  // The cycle is in standby operation
			double c_htf = specheat(m_pbp.HTF, physics::CelciusToKelvin((m_pbi.T_htf_hot + m_pbp.T_htf_cold_ref)/2.0), 1.0);
		    double q_tot = m_pbp.P_ref / m_pbp.eta_ref;

			// Calculate the actual q_sby_needed from the reference flows
			double q_sby_needed = q_tot * m_pbp.q_sby_frac;

			// now calculate the mass flow rate knowing the inlet temperature of the salt,
			// ..and holding the outlet temperature at the reference outlet temperature
			double m_dot_sby = q_sby_needed/(c_htf * (m_pbi.T_htf_hot - m_pbp.T_htf_cold_ref))*3600.0;


			// Set other output values
			m_pbo.P_cycle = 0.0;
			m_pbo.eta = 0.0;
			m_pbo.T_htf_cold = m_pbp.T_htf_cold_ref;
			m_pbo.m_dot_demand = m_dot_sby;
			m_pbo.m_dot_makeup = 0.0;
			m_pbo.W_cool_par = 0.0;
			m_pbo.f_hrsys = 0.0;
			m_pbo.P_cond = 0.0;
			break;}
			
		case 3:  // The cycle has been completely shut down
			m_pbo.P_cycle = 0.0;
			m_pbo.eta = 0.0;
			m_pbo.T_htf_cold = m_pbp.T_htf_cold_ref;  // Changed from m_pbi.T_htf_hot 12/18/2009 was causing problems with T250
			m_pbo.m_dot_demand = 0.0;
			m_pbo.m_dot_makeup = 0.0;
			m_pbo.W_cool_par = 0.0;
			m_pbo.f_hrsys = 0.0;
			m_pbo.P_cond = 0.0;
			break;
	}


	// If the cycle is going from completely shut down to starting up, set the remaining startup
	// time to be equal to the designated startup time
	if((m_iLastStandbyControl == 3) && (m_pbi.standby_control == 1))
	{
		m_dStartupRemain = m_pbp.startup_time;
		m_dStartupERemain = m_dStartupEnergy;
	}

	// If the cycle is starting up beginning in this time period, or it is continuing to start
	// up from the last time period, then subtract the appropriate fraction of electric power
	// from the output.  Note that during the startup time, not only is the cycle not producing power,
	// but it is also consuming thermal energy
	if(m_pbo.P_cycle > 0.0)
	{
		if ( ((m_iLastStandbyControl == 3) && (m_pbi.standby_control == 1)) || ((m_dStartupRemain + m_dStartupERemain) > 0.0))
		{

			// Adjust the power cycle output. Both the energy and time requirement must be met before power is produced,
			// so subtract the maximum of these two values
			double Q_cycle = m_pbo.P_cycle/m_pbo.eta;
			double startup_e_used = dmin1(Q_cycle * m_dHoursSinceLastStep, m_dStartupERemain);       // The used startup energy is the less of the energy to the power block and the remaining startup requirement

			double f_st = 1.0 - dmax1(dmin1(1.0, m_dStartupRemain/m_dHoursSinceLastStep), startup_e_used/Q_cycle);
			m_pbo.P_cycle = m_pbo.P_cycle*f_st;

			// Fraction of the timestep running at full capacity
			// The power cycle still requires mass flow to satisfy the energy demand requirement, so only subtract demand mass flow
			// for the case when startup time exceeds startup energy.
			m_pbo.m_dot_demand = m_pbo.m_dot_demand * (1.0 - dmax1(dmin1(1.0, m_dStartupRemain/m_dHoursSinceLastStep) - startup_e_used/Q_cycle, 0.0));

			m_pbo.eta = m_pbp.eta_ref;
			m_pbo.T_htf_cold = m_pbp.T_htf_cold_ref;
			// m_dot_htf_ref = m_dot_htf_ref // TFF - huh? Is this a typo?

			m_dStartupRemain = dmax1(m_dStartupRemain - m_dHoursSinceLastStep, 0.0);
			m_dStartupERemain = dmax1(m_dStartupERemain - startup_e_used, 0.0);
		}	
	}

	// SET THE OUTPUTS FROM THIS MODEL
	// 900 continue // MJW 12.10.2010
	// Cycle power output
	// OUT(1)=P_cycle/1000.  // Convert from kW to MW
	m_pbo.P_cycle = m_pbo.P_cycle/1000.0;

	// Cycle thermal efficiency
	// OUT(2)=eta
	//m_pbo.eta = eta;

	// Heat transfer fluid outlet temp
	// OUT(3)=T_htf_cold
	//m_pbo.T_htf_cold = T_htf_cold;

	// Wet cooling makeup water flow rate
	// OUT(4)=(m_dot_makeup + m_dot_st_bd)*3600.
	m_pbo.m_dot_makeup = (m_pbo.m_dot_makeup + m_dot_st_bd)*3600.0;

	// Heat transfer fluid demand flow rate
	// OUT(5)=m_dot_demand
	//m_pbo.m_dot_demand = m_dot_demand;

	// Heat transfer fluid flow rate
	// OUT(6)=m_pbi.m_dot_htf
	//m_pbo.m_dot_htf = m_pbi.m_dot_htf;

	// Calculated reference htf flow rate
	// OUT(7)=m_dot_htf_ref
	//m_pbo.m_dot_htf_ref = m_dot_htf_ref;

	// Cooling tower parasitic load [MW]
	// OUT(8)=W_cool_par
	//m_pbo.W_cool_par = W_cool_par;

	// Reference power level output
	// OUT(9)=P_ref/1000.   // Convert from kW to MW
	m_pbo.P_ref = m_pbo.P_ref/1000.0;

	// Fraction of cooling system in operation
	// OUT(10)=f_hrsys
	//m_pbo.f_hrsys = f_hrsys;

	// Condenser pressure (Pa)
	// OUT(11)=P_cond
	//m_pbo.P_cond = P_cond;

	return (m_strLastError=="") ? true : false;
}

//************************************************************************************************************
//************************************************************************************************************
double CPowerBlock_Type224::f_Tsat_p(double P)
{
	// Calculates the saturation temperature[C] of steam given a certain pressure[Pa]
	double Pg = 0, T = 9999.9, err = 999.9, Tg = 0;
	if(P-Pg > 1) Tg = 25.0;

	for (int i=0; i<30; i++)
	{	// iterative loop to solve for Pg = P and return T. T cannot be expressed in terms of P.
		Pg = f_psat_T(Tg);
		err = (P-Pg)/P;
		T = Tg;
		if( (fabs(err) < 1.0E-6) ) break;
		Tg = T + (err * 25.0);
	}
	return T;
 }

//************************************************************************************************************
//************************************************************************************************************
double CPowerBlock_Type224::specheat(int fnum, double T, double )
{
	//use global_props
	// This function accepts as inputs temperature [K] and pressure [Pa]
	// This function outputs in units of [kJ/kg-K]
	//double precision,intent(in)::T,P,fnumd
	double  Td; //xlo, xhi,;
	//double precision,dimension(size(fprop(1,:)))::dxx,dyy // Create dummy arrays
	//int lb,ub,dum,t_warn;
	//specheat=1.0
	Td = T - 273.15;

	switch(fnum)
	{
		case 1: return 1.03749 - 0.000305497*T + 7.49335E-07*T*T - 3.39363E-10*T*T*T; break;	//	1.) Air
		case 2: return 0.368455 + 0.000399548*T - 1.70558E-07*T*T; break;	// EES					2.) Stainless_AISI316
		case 3: return 4.181; break;  //															3.) Water (liquid)
		case 4: return 1; break;  //																4.) Steam
		case 5: return 1; break;  //																5.) CO2
		case 6:  return 1.156; break; //															6.) Salt (68% KCl, 32% MgCl2)
		case 7:  return 1.507; break; //															7.) Salt (8% NaF, 92% NaBF4)
		case 8:  return 1.306; break; //															8.) Salt (25% KF, 75% KBF4)
		case 9:  return 9.127; break; //															9.) Salt (31% RbF, 69% RbBF4)
		case 10: return 2.010; break; //															10.) Salt (46.5% LiF, 11.5%NaF, 42%KF)
		case 11: return 1.239; break; //															11.) Salt (49% LiF, 29% NaF, 29% ZrF4)
		case 12: return 1.051; break; //															12.) Salt (58% KF, 42% ZrF4)
		case 13: return 8.918; break; //															13.) Salt (58% LiCl, 42% RbCl)
		case 14: return 1.080; break; //															14.) Salt (58% NaCl, 42% MgCl2)
		case 15: return 1.202; break; //															15.) Salt (59.5% LiCl, 40.5% KCl)
		case 16: return 1.172; break; //															16.) Salt (59.5% NaF, 40.5% ZrF4)
		case 17: return -1E-10*T*T*T + 2E-07*T*T + 5E-06*T + 1.4387; break; //						17.) Salt (60% NaNO3, 40% KNO3)
		case 18: return (1443. + 0.172 * (T-273.15))/1000.0; break;									// Heat Capacity of Nitrate Salt, [J/kg/K]
		case 19: return (3.88 * (T-273.15) + 1606.0)/1000.0; break;									// Specific Heat of Caloria HT 43 [J/kgC]
		case 20: return dmax1(1536 - 0.2624 * Td - 0.0001139 * Td * Td, 1000.0)/1000.0; break;		// Heat Capacity of HITEC XL Nitrate Salt, [J/kg/K]
		case 21: return (1.509 + 0.002496 * Td + 0.0000007888 * Td*Td); break;		 				// Specific Heat of Therminol Oil, J/kg/K
		case 22: return (1560 - 0.0 * Td)/1000.0; break;											// Heat Capacity of HITEC Salt, [J/kg/K]
		case 23: return (-0.00053943*Td*Td + 3.2028*Td + 1589.2)/1000.0; break;						// Specific Heat of Dowtherm Q, J/kg/K (Russ 10-2-03)
		case 24: return (-0.0000031915*Td*Td + 2.977*Td + 1560.8)/1000.0; break;					// Specific Heat of Dowtherm RP, J/kg/K (Russ 10-2-03)
		case 25: return dmax1(1536 - 0.2624 * Td - 0.0001139 * Td * Td,1000.0)/1000.0; break;		// Heat Capacity of HITEC XL Nitrate Salt, [J/kg/K]
		case 26: return 0.5203; break;																// Argon (Cp only, Cv is different)
		case 27: 
			return dmin1(dmax1(-45.4022 + 0.690156*T - 0.00327354*T*T + 0.00000817326*T*T*T - 1.13234E-08*pow(T,4) + 8.24995E-12*pow(T,5) - 2.46804E-15*pow(T,6),11.30),14.7);   //  Hydrogen
			break;
		default: return 1; break;
		/*
			case 28:35:
			continue
			case 36:) // Any integer greater than 35
			// Call the user-defined property table
			lb=fl_bounds(fnum-35)
			ub=fl_bounds(fnum-35+1)-1
			if(ub.lt.lb) ub=size(fprop(1,:))
			dxx(:)=fprop(1,lb:ub)
			dyy(:)=fprop(2,lb:ub)
			call interp(Td,size(dxx),dxx,dyy,Gjsav,specheat)
					if((Gjsav.eq.ub).or.(Gjsav.eq.lb)) dum=t_warn(Td,dxx(lb),dxx(ub),"User-specified fluid")
		*/
	}
}


//************************************************************************************************************
//************************************************************************************************************
void CPowerBlock_Type224::RankineCycle(/*double time,*/double P_ref, double eta_ref, double T_htf_hot_ref, double T_htf_cold_ref, double T_db, double T_wb, 
				  double P_amb, double dT_cw_ref, /*double HTF,*/ double , double T_htf_hot, double m_dot_htf, int /*double*/ mode, 
				  double demand_var, double P_boil, /*double tech_type,*/ double T_amb_des, double T_approach, double F_wc, double F_wcmin, 
				  double F_wcmax, double T_ITD_des, double P_cond_ratio, /*double CT,*/ double P_cond_min, /*double n_pl_inc,*/
				  /*double& fcall, */ double& P_cycle, double& eta, double& T_htf_cold, double& m_dot_demand, double& m_dot_htf_ref, 
				  double& m_dot_makeup, double& W_cool_par, double& f_hrsys, double& P_cond)
{
	// Note: the old "TT" is now m_pbp.tech_type

	// The user provides a reference efficiency, ambient temperature, and cooling system parameters. Using
	// this information, we have to adjust the provided reference efficiency to match the normalized efficiency
	// that is part of the power block regression coefficients. I.e. if the user provides a ref. ambient temperature
	// of 25degC, but the power block coefficients indicate that the normalized efficiency equals 1.0 at an ambient
	// temp of 20degC, we have to adjust the user's efficiency value back to the coefficient set.
	if (m_bFirstCall)
	{
		double Psat_ref = 0;
		switch(m_pbp.CT)
		{
			case 1: // Wet cooled case
				if (m_pbp.tech_type != 4)
					Psat_ref = f_psat_T(dT_cw_ref + 3.0 + T_approach + T_amb_des); // Steam
				else
					Psat_ref = P_sat4(dT_cw_ref + 3.0 + T_approach + T_amb_des); // Isopentane
				break;
			
			case 2:
			case 3:// Dry cooled case and Hybrid case
				if (m_pbp.tech_type != 4)
					Psat_ref = f_psat_T(T_ITD_des + T_amb_des); // Steam
				else
					Psat_ref = P_sat4(T_ITD_des + T_amb_des); // Isopentane
				break;
		}
		eta_adj = eta_ref/(Interpolate(12,2,Psat_ref)/Interpolate(22,2,Psat_ref));
		m_bFirstCall = false;
	}

	// Calculate the specific heat before converting to Kelvin
	double c_htf_ref = specheat(m_pbp.HTF, physics::CelciusToKelvin((T_htf_hot_ref+T_htf_cold_ref)/2.0), 1.0);
	double c_htf = specheat(m_pbp.HTF, physics::CelciusToKelvin((T_htf_hot+T_htf_cold_ref)/2.0), 1.0);

	// Convert units
	// **Temperatures from Celcius to Kelvin
	T_htf_hot = physics::CelciusToKelvin(T_htf_hot);
	T_htf_hot_ref = physics::CelciusToKelvin(T_htf_hot_ref);
	T_htf_cold_ref = physics::CelciusToKelvin(T_htf_cold_ref);
	// Mass flow rates from kg/hr to kg/s
	m_dot_htf = m_dot_htf/3600.0; // [kg/s]

	// ****Calculate the reference values
	double q_dot_ref = P_ref/eta_adj;   // The reference heat flow
	m_dot_htf_ref = q_dot_ref/(c_htf_ref*(T_htf_hot_ref - T_htf_cold_ref));  // The HTF mass flow rate [kg/s]

	double T_ref = 0; // The saturation temp at the boiler
	if (m_pbp.tech_type == 4)
		T_ref = T_sat4(P_boil); // Sat temp for isopentane
	else
		T_ref = T_sat(P_boil);  // Sat temp for water

	// Calculate the htf hot temperature, in non-dimensional form
	if (T_ref>=T_htf_hot)
	{	// boiler pressure is unrealistic -> it could not be achieved with this resource temp
		m_strLastError = "The input boiler pressure could not be achieved with the resource temperature entered.";
		P_cycle = 0.0;
	}
	double T_htf_hot_ND = (T_htf_hot - T_ref)/(T_htf_hot_ref - T_ref);

	// Calculate the htf mass flow rate in non-dimensional form
	double m_dot_htf_ND = m_dot_htf/m_dot_htf_ref;

	// Do an initial cooling tower call to estimate the turbine back pressure.
	double q_reject_est = q_dot_ref*1000.0*(1.0-eta_adj)*m_dot_htf_ND*T_htf_hot_ND;

	double T_cond=0, m_dot_air=0, W_cool_parhac=0, W_cool_parhwc=0;
	switch(m_pbp.CT)  // Cooling technology type {1=evaporative cooling, 2=air cooling, 3=hybrid cooling}
	{
		case 1:
			// For a wet-cooled system
			evap_tower(P_cond_min, m_pbp.n_pl_inc, dT_cw_ref, T_approach, (P_ref*1000.), eta_adj, T_db, T_wb, P_amb, q_reject_est, m_dot_makeup, W_cool_par, P_cond, T_cond, f_hrsys);
			break;
		case 2:
			// For a dry-cooled system
			ACC(P_cond_min, m_pbp.n_pl_inc, T_ITD_des, P_cond_ratio, (P_ref*1000.), eta_adj, T_db, P_amb, q_reject_est, m_dot_air, W_cool_par, P_cond, T_cond, f_hrsys);
			m_dot_makeup = 0.0;
			break;
		case 3:
			// for a hybrid cooled system
			HybridHR(/*fcall,*/ P_cond_min, m_pbp.n_pl_inc, F_wc, F_wcmax, F_wcmin, T_ITD_des, T_approach, dT_cw_ref, P_cond_ratio, (P_ref*1000.), eta_adj, T_db, T_wb,
							P_amb, q_reject_est, m_dot_makeup, W_cool_parhac, W_cool_parhwc, W_cool_par, P_cond, T_cond, f_hrsys);
			break;
	}

	//   Set initial values
	double ADJ = 1.0, err = 1.0; /*qq=0;*/

	// Do a quick check to see if there is actually a mass flow being supplied
	//   to the cycle. If not, go to the end.
	if(fabs(m_dot_htf_ND) < 1.0E-3)
	{
		P_cycle = 0.0;
		eta = 0.0;
		T_htf_cold = T_htf_hot_ref;
		m_dot_demand = m_dot_htf_ref;
		W_cool_par = 0.0;
		m_dot_makeup = 0.0;
		// Set the error to zero, since we don't want to iterate
		err=0.0;
	}

	double P_dem_ND, P_AB, P_CA, P_BC, Q_AB, Q_CA, Q_BC, P_ND_tot, Q_ND_tot, q_reject;
	double P_ND[3], Q_ND[3];
	// Begin iterations
	//do while ((err.gt.1.e-6).and.(qq.lt.100))
	for (int qq = 1; qq<100; qq++)
	{
		if (err <= 1.0E-6) break;
		/*qq=qq+1*/

		// Now use the constrained variable to calculate the demand mass flow rate
		if(mode == 1)
		{
			P_dem_ND = demand_var/P_ref;
			if(qq == 1) m_dot_htf_ND = P_dem_ND;   // An initial guess (function of power)
			// if(qq.gt.1) m_dot_htf_ND = m_dot_htf_ND*ADJ
		}
		/*
		elseif(mode == 2.) then
			continue     //  do nothing
		endif*/

		// ++++++++++++++Correlations++++++++++++++++++
		// Calculate the correlations
		// ++++++++++++++++++++++++++++++++++++++++++++
		// POWER
		// Main effects
		P_ND[0] = Interpolate(11,1,T_htf_hot_ND)-1.0;
		P_ND[1] = Interpolate(12,2,P_cond)-1.0;
		P_ND[2] = Interpolate(13,3,m_dot_htf_ND)-1.0;

		// Interactions
		P_CA = Interpolate(113,13,T_htf_hot_ND);
		P_AB = Interpolate(112,12,P_cond);
		P_BC = Interpolate(123,23,m_dot_htf_ND);

		P_ND[0] = P_ND[0]*P_AB;
		P_ND[1] = P_ND[1]*P_BC;
		P_ND[2] = P_ND[2]*P_CA;

		// HEAT
		// Main effects
		Q_ND[0] = Interpolate(21,1,T_htf_hot_ND)-1.0;
		Q_ND[1] = Interpolate(22,2,P_cond)-1.0;
		Q_ND[2] = Interpolate(23,3,m_dot_htf_ND)-1.0;

		// Interactions
		Q_CA = Interpolate(213,13,T_htf_hot_ND);
		Q_AB = Interpolate(212,12,P_cond);
		Q_BC = Interpolate(223,23,m_dot_htf_ND);

		Q_ND[0] = Q_ND[0]*Q_AB;
		Q_ND[1] = Q_ND[1]*Q_BC;
		Q_ND[2] = Q_ND[2]*Q_CA;

		// Calculate the cumulative values
		P_ND_tot = 1.0;
		Q_ND_tot = 1.0;

		// Increment main effects. MJW 8.11.2010 :: For this system, the effects are multiplicative.
		for (int i=0; i<3; i++)
		{
			P_ND_tot = P_ND_tot * (1.0+ P_ND[i]);
			Q_ND_tot = Q_ND_tot * (1.0+ Q_ND[i]);
		}

		// Calculate the output values:
		P_cycle = P_ND_tot*P_ref;
		T_htf_cold = T_htf_hot-Q_ND_tot*q_dot_ref/(m_dot_htf*c_htf);
		eta = P_cycle/(Q_ND_tot*q_dot_ref);
		m_dot_demand = dmax1(m_dot_htf_ND*m_dot_htf_ref, 0.00001);   // [kg/s]

		// Call the cooling tower model to update the condenser pressure
		q_reject = (1.0 - eta)*q_dot_ref*Q_ND_tot*1000.0;
		if (qq < 10) // MJW 10.31.2010
		{
			switch(m_pbp.CT)  // Cooling technology type {1=evaporative cooling, 2=air cooling, 3=hybrid cooling}
			{
				case 1:
					evap_tower(P_cond_min, m_pbp.n_pl_inc, dT_cw_ref, T_approach, (P_ref*1000.), eta_adj, T_db, T_wb, P_amb, q_reject, m_dot_makeup, W_cool_par, P_cond, T_cond, f_hrsys);
					break;
				case 2:
					ACC(P_cond_min, m_pbp.n_pl_inc, T_ITD_des, P_cond_ratio, (P_ref*1000.), eta_adj, T_db, P_amb, q_reject, m_dot_air, W_cool_par, P_cond, T_cond, f_hrsys);
					break;
				case 3:
					HybridHR(/*fcall, */P_cond_min, m_pbp.n_pl_inc, F_wc, F_wcmax, F_wcmin, T_ITD_des, T_approach, dT_cw_ref, P_cond_ratio, (P_ref*1000.), eta_adj, T_db, T_wb,
							  P_amb, q_reject, m_dot_makeup, W_cool_parhac, W_cool_parhwc, W_cool_par, P_cond, T_cond, f_hrsys);
					break;
			}
		}

		// Check to see if the calculated and demand values match
		// If they don't match, calculate the "ADJ" factor
		if (mode == 1)
		{
			// err = (P_cycle - demand_var)/demand_var
			// ADJ = 1.+(demand_var-P_cycle)/(3.*demand_var)
			ADJ = (demand_var-P_cycle)/demand_var;		// MJW 10.31.2010: Adjustment factor
			err = fabs(ADJ);								// MJW 10.31.2010: Take the absolute value of the error..
			m_dot_htf_ND = m_dot_htf_ND + ADJ*0.75;		// MJW 10.31.2010: Iterate the mass flow rate. Take a step smaller than the calculated adjustment
		
		}
		else if(mode== 2)
			err = 0.0;

		if(qq == 99)
		{
			m_strLastError = "Power cycle model did not converge after 100 iterations";
			P_cycle = 0.0;
			eta = 0.0;
			T_htf_cold = T_htf_hot_ref;
			m_dot_demand = m_dot_htf_ref;
			// TFF - should this be here too? m_bFirstCall = false;
			/*if(errorfound())*/ return;
		}
		// If this is not true, the cycle has not yet converged, and we should return
		//  to continue in the iterations
	}

	// Finally, convert the values back to their original units
	T_htf_cold = T_htf_cold - 273.15;			// [K]-->[C]
	T_htf_cold_ref = T_htf_cold_ref - 273.15;	// [K]->[C]
	T_htf_hot_ref = T_htf_hot_ref - 273.15;		// [K]->[C]
	m_dot_demand = m_dot_demand*3600.0;			// [kg/s]->[kg/hr]
	m_dot_htf = m_dot_htf*3600.0;				// [kg/s]->[kg/hr]
	m_dot_htf_ref = m_dot_htf_ref*3600.0;		// [kg/s]->[kg/hr]

	// Set the "been called" variable
	//fcall = 0.0;
}


//************************************************************************************************************
//************************************************************************************************************
double CPowerBlock_Type224::Interpolate(int YT, int XT, double X)
{
	double ind;//, temp(200);
	int XI=0, YI=0, lbi=0, ubi=0;
	//This function interpolates the data of one data list based on the value provided to a corresponding list of the same length.
	//YT: The name of the dependent Y variable being interpolated
	//XT: The name of the independent X variable which the Y variable is a function of
	//X: The value of the X variable
	//Y: is returned

/*
if(TT==2)then !Low temperature parabolic trough applications
    ! On first call, allocate the array and define coefficients
    allocate(db(18,20))

	db(1,:)=(/ &
		0.10000, 0.16842, 0.23684, 0.30526, 0.37368, 0.44211,  &
		0.51053, 0.57895, 0.64737, 0.71579, 0.78421, 0.85263,  &
		0.92105, 0.98947, 1.05789, 1.12632, 1.19474, 1.26316,  &
		1.33158, 1.40000 /)
	db(2,:)=(/ &
		0.08547, 0.14823, 0.21378, 0.28166, 0.35143, 0.42264,  &
		0.49482, 0.56747, 0.64012, 0.71236, 0.78378, 0.85406,  &
		0.92284, 0.98989, 1.05685, 1.12369, 1.19018, 1.25624,  &
		1.32197, 1.38744 /)
	db(3,:)=(/ &
		0.10051, 0.16934, 0.23822, 0.30718, 0.37623, 0.44534,  &
		0.51443, 0.58338, 0.65209, 0.72048, 0.78848, 0.85606,  &
		0.92317, 0.98983, 1.05604, 1.12182, 1.18718, 1.25200,  &
		1.31641, 1.38047 /)
	db(4,:)=(/ &
		3000.00, 4263.16, 5526.32, 6789.47, 8052.63, 9315.79,  &
		10578.95, 11842.11, 13105.26, 14368.42, 15631.58, 16894.74,  &
		18157.89, 19421.05, 20684.21, 21947.37, 23210.53, 24473.68,  &
		25736.84, 27000.00 /)
	db(5,:)=(/ &
		1.08827, 1.06020, 1.03882, 1.02145, 1.00692, 0.99416,  &
		0.98288, 0.97273, 0.96350, 0.95504, 0.94721, 0.93996,  &
		0.93314, 0.92673, 0.92069, 0.91496, 0.90952, 0.90433,  &
		0.89938, 0.89464 /)
	db(6,:)=(/ &
		1.01276, 1.00877, 1.00570, 1.00318, 1.00106, 0.99918,  &
		0.99751, 0.99601, 0.99463, 0.99335, 0.99218, 0.99107,  &
		0.99004, 0.98907, 0.98814, 0.98727, 0.98643, 0.98563,  &
		0.98487, 0.98413 /)
	db(7,:)=(/ &
		0.10000, 0.17368, 0.24737, 0.32105, 0.39474, 0.46842,  &
		0.54211, 0.61579, 0.68947, 0.76316, 0.83684, 0.91053,  &
		0.98421, 1.05789, 1.13158, 1.20526, 1.27895, 1.35263,  &
		1.42632, 1.50000 /)
	db(8,:)=(/ &
		0.09307, 0.16421, 0.23730, 0.31194, 0.38772, 0.46420,  &
		0.54098, 0.61763, 0.69374, 0.76896, 0.84287, 0.91511,  &
		0.98530, 1.05512, 1.12494, 1.19447, 1.26373, 1.33273,  &
		1.40148, 1.46999 /)
	db(9,:)=(/ &
		0.10741, 0.18443, 0.26031, 0.33528, 0.40950, 0.48308,  &
		0.55610, 0.62861, 0.70066, 0.77229, 0.84354, 0.91443,  &
		0.98497, 1.05520, 1.12514, 1.19478, 1.26416, 1.33329,  &
		1.40217, 1.47081 /)
	db(10,:)=(/ &
		0.10000, 0.16842, 0.23684, 0.30526, 0.37368, 0.44211,  &
		0.51053, 0.57895, 0.64737, 0.71579, 0.78421, 0.85263,  &
		0.92105, 0.98947, 1.05789, 1.12632, 1.19474, 1.26316,  &
		1.33158, 1.40000 /)
	db(11,:)=(/ &
		1.01749, 1.03327, 1.04339, 1.04900, 1.05051, 1.04825,  &
		1.04249, 1.03343, 1.02126, 1.01162, 1.00500, 1.00084,  &
		0.99912, 0.99966, 0.99972, 0.99942, 0.99920, 0.99911,  &
		0.99885, 0.99861 /)
	db(12,:)=(/ &
		0.99137, 0.99297, 0.99431, 0.99564, 0.99681, 0.99778,  &
		0.99855, 0.99910, 0.99948, 0.99971, 0.99984, 0.99989,  &
		0.99993, 0.99993, 0.99992, 0.99992, 0.99992, 1.00009,  &
		1.00010, 1.00012 /)
	db(13,:)=(/ &
		3000.00, 4263.16, 5526.32, 6789.47, 8052.63, 9315.79,  &
		10578.95, 11842.11, 13105.26, 14368.42, 15631.58, 16894.74,  &
		18157.89, 19421.05, 20684.21, 21947.37, 23210.53, 24473.68,  &
		25736.84, 27000.00 /)
	db(14,:)=(/ &
		0.99653, 0.99756, 0.99839, 0.99906, 0.99965, 1.00017,  &
		1.00063, 1.00106, 1.00146, 1.00183, 1.00218, 1.00246,  &
		1.00277, 1.00306, 1.00334, 1.00361, 1.00387, 1.00411,  &
		1.00435, 1.00458 /)
	db(15,:)=(/ &
		0.99760, 0.99831, 0.99888, 0.99934, 0.99973, 1.00008,  &
		1.00039, 1.00067, 1.00093, 1.00118, 1.00140, 1.00161,  &
		1.00180, 1.00199, 1.00217, 1.00234, 1.00250, 1.00265,  &
		1.00280, 1.00294 /)
	db(16,:)=(/ &
		0.10000, 0.17368, 0.24737, 0.32105, 0.39474, 0.46842,  &
		0.54211, 0.61579, 0.68947, 0.76316, 0.83684, 0.91053,  &
		0.98421, 1.05789, 1.13158, 1.20526, 1.27895, 1.35263,  &
		1.42632, 1.50000 /)
	db(17,:)=(/ &
		1.01994, 1.01645, 1.01350, 1.01073, 1.00801, 1.00553,  &
		1.00354, 1.00192, 1.00077, 0.99995, 0.99956, 0.99957,  &
		1.00000, 0.99964, 0.99955, 0.99945, 0.99937, 0.99928,  &
		0.99919, 0.99918 /)
	db(18,:)=(/ &
		1.02055, 1.01864, 1.01869, 1.01783, 1.01508, 1.01265,  &
		1.01031, 1.00832, 1.00637, 1.00454, 1.00301, 1.00141,  &
		1.00008, 0.99851, 0.99715, 0.99586, 0.99464, 0.99347,  &
		0.99227, 0.99177 /)

elseif (TT==1) then  !Power tower applications
    ! On first call, allocate the array and define coefficients
    allocate(db(18,20))
		db(1,:)=(/ &
		0.20000, 0.25263, 0.30526, 0.35789, 0.41053, 0.46316,  &
		0.51579, 0.56842, 0.62105, 0.67368, 0.72632, 0.77895,  &
		0.83158, 0.88421, 0.93684, 0.98947, 1.04211, 1.09474,  &
		1.14737, 1.20000 /)
	db(2,:)=(/ &
		0.16759, 0.21750, 0.26932, 0.32275, 0.37743, 0.43300,  &
		0.48910, 0.54545, 0.60181, 0.65815, 0.71431, 0.77018,  &
		0.82541, 0.88019, 0.93444, 0.98886, 1.04378, 1.09890,  &
		1.15425, 1.20982 /)
	db(3,:)=(/ &
		0.19656, 0.24969, 0.30325, 0.35710, 0.41106, 0.46497,  &
		0.51869, 0.57215, 0.62529, 0.67822, 0.73091, 0.78333,  &
		0.83526, 0.88694, 0.93838, 0.98960, 1.04065, 1.09154,  &
		1.14230, 1.19294 /)
	db(4,:)=(/ &
		3000.00, 4263.16, 5526.32, 6789.47, 8052.63, 9315.79,  &
		10578.95, 11842.11, 13105.26, 14368.42, 15631.58, 16894.74,  &
		18157.89, 19421.05, 20684.21, 21947.37, 23210.53, 24473.68,  &
		25736.84, 27000.00 /)
	db(5,:)=(/ &
		1.07401, 1.04917, 1.03025, 1.01488, 1.00201, 0.99072,  &
		0.98072, 0.97174, 0.96357, 0.95607, 0.94914, 0.94269,  &
		0.93666, 0.93098, 0.92563, 0.92056, 0.91573, 0.91114,  &
		0.90675, 0.90255 /)
	db(6,:)=(/ &
		1.00880, 1.00583, 1.00355, 1.00168, 1.00010, 0.99870,  &
		0.99746, 0.99635, 0.99532, 0.99438, 0.99351, 0.99269,  &
		0.99193, 0.99121, 0.99052, 0.98988, 0.98926, 0.98867,  &
		0.98810, 0.98756 /)
	db(7,:)=(/ &
		0.10000, 0.17368, 0.24737, 0.32105, 0.39474, 0.46842,  &
		0.54211, 0.61579, 0.68947, 0.76316, 0.83684, 0.91053,  &
		0.98421, 1.05789, 1.13158, 1.20526, 1.27895, 1.35263,  &
		1.42632, 1.50000 /)
	db(8,:)=(/ &
		0.09403, 0.16542, 0.23861, 0.31328, 0.38901, 0.46540,  &
		0.54203, 0.61849, 0.69437, 0.76928, 0.84282, 0.91458,  &
		0.98470, 1.05517, 1.12536, 1.19531, 1.26502, 1.33450,  &
		1.40376, 1.47282 /)
	db(9,:)=(/ &
		0.10659, 0.18303, 0.25848, 0.33316, 0.40722, 0.48075,  &
		0.55381, 0.62646, 0.69873, 0.77066, 0.84228, 0.91360,  &
		0.98464, 1.05542, 1.12596, 1.19627, 1.26637, 1.33625,  &
		1.40593, 1.47542 /)
	db(10,:)=(/ &
		0.20000, 0.25263, 0.30526, 0.35789, 0.41053, 0.46316,  &
		0.51579, 0.56842, 0.62105, 0.67368, 0.72632, 0.77895,  &
		0.83158, 0.88421, 0.93684, 0.98947, 1.04211, 1.09474,  &
		1.14737, 1.20000 /)
	db(11,:)=(/ &
		1.03323, 1.04058, 1.04456, 1.04544, 1.04357, 1.03926,  &
		1.03282, 1.02446, 1.01554, 1.00944, 1.00487, 1.00169,  &
		0.99986, 0.99926, 0.99980, 1.00027, 1.00021, 1.00015,  &
		1.00006, 0.99995 /)
	db(12,:)=(/ &
		0.98344, 0.98630, 0.98876, 0.99081, 0.99247, 0.99379,  &
		0.99486, 0.99574, 0.99649, 0.99716, 0.99774, 0.99826,  &
		0.99877, 0.99926, 0.99972, 1.00017, 1.00060, 1.00103,  &
		1.00143, 1.00182 /)
	db(13,:)=(/ &
		3000.00, 4263.16, 5526.32, 6789.47, 8052.63, 9315.79,  &
		10578.95, 11842.11, 13105.26, 14368.42, 15631.58, 16894.74,  &
		18157.89, 19421.05, 20684.21, 21947.37, 23210.53, 24473.68,  &
		25736.84, 27000.00 /)
	db(14,:)=(/ &
		0.99269, 0.99520, 0.99718, 0.99882, 1.00024, 1.00150,  &
		1.00264, 1.00368, 1.00464, 1.00554, 1.00637, 1.00716,  &
		1.00790, 1.00840, 1.00905, 1.00965, 1.01022, 1.01075,  &
		1.01126, 1.01173 /)
	db(15,:)=(/ &
		0.99768, 0.99861, 0.99933, 0.99992, 1.00043, 1.00087,  &
		1.00127, 1.00164, 1.00197, 1.00227, 1.00255, 1.00282,  &
		1.00307, 1.00331, 1.00353, 1.00375, 1.00395, 1.00415,  &
		1.00433, 1.00451 /)
	db(16,:)=(/ &
		0.10000, 0.17368, 0.24737, 0.32105, 0.39474, 0.46842,  &
		0.54211, 0.61579, 0.68947, 0.76316, 0.83684, 0.91053,  &
		0.98421, 1.05789, 1.13158, 1.20526, 1.27895, 1.35263,  &
		1.42632, 1.50000 /)
	db(17,:)=(/ &
		1.00812, 1.00513, 1.00294, 1.00128, 0.99980, 0.99901,  &
		0.99855, 0.99836, 0.99846, 0.99883, 0.99944, 1.00033,  &
		1.00042, 1.00056, 1.00069, 1.00081, 1.00093, 1.00104,  &
		1.00115, 1.00125 /)
	db(18,:)=(/ &
		1.09816, 1.07859, 1.06487, 1.05438, 1.04550, 1.03816,  &
		1.03159, 1.02579, 1.02061, 1.01587, 1.01157, 1.00751,  &
		1.00380, 1.00033, 0.99705, 0.99400, 0.99104, 0.98832,  &
		0.98565, 0.98316 /)

elseif (TT==3) then !Sliding pressure power cycle formulation mjw 3.31.11
    allocate(db(18,10))
	db(1,:)=(/ &
		0.10000, 0.21111, 0.32222, 0.43333, 0.54444, 0.65556,  &
		0.76667, 0.87778, 0.98889, 1.10000 /)
	db(2,:)=(/ &
		0.89280, 0.90760, 0.92160, 0.93510, 0.94820, 0.96110,  &
		0.97370, 0.98620, 0.99860, 1.01100 /)
	db(3,:)=(/ &
		0.93030, 0.94020, 0.94950, 0.95830, 0.96690, 0.97520,  &
		0.98330, 0.99130, 0.99910, 1.00700 /)
	db(4,:)=(/ &
		0.04000, 0.06556, 0.09111, 0.11667, 0.14222, 0.16778,  &
		0.19333, 0.21889, 0.24444, 0.27000 /)
	db(5,:)=(/ &
		1.04800, 1.01400, 0.99020, 0.97140, 0.95580, 0.94240,  &
		0.93070, 0.92020, 0.91060, 0.90190 /)
	db(6,:)=(/ &
		0.99880, 0.99960, 1.00000, 1.00100, 1.00100, 1.00100,  &
		1.00100, 1.00200, 1.00200, 1.00200 /)
	db(7,:)=(/ &
		0.20000, 0.31667, 0.43333, 0.55000, 0.66667, 0.78333,  &
		0.90000, 1.01667, 1.13333, 1.25000 /)
	db(8,:)=(/ &
		0.16030, 0.27430, 0.39630, 0.52310, 0.65140, 0.77820,  &
		0.90060, 1.01600, 1.12100, 1.21400 /)
	db(9,:)=(/ &
		0.22410, 0.34700, 0.46640, 0.58270, 0.69570, 0.80550,  &
		0.91180, 1.01400, 1.11300, 1.20700 /)
	db(10,:)=(/ &
		0.10000, 0.21111, 0.32222, 0.43333, 0.54444, 0.65556,  &
		0.76667, 0.87778, 0.98889, 1.10000 /)
	db(11,:)=(/ &
		1.05802, 1.05127, 1.04709, 1.03940, 1.03297, 1.02480,  &
		1.01758, 1.00833, 1.00180, 0.99307 /)
	db(12,:)=(/ &
		1.03671, 1.03314, 1.02894, 1.02370, 1.01912, 1.01549,  &
		1.01002, 1.00486, 1.00034, 0.99554 /)
	db(13,:)=(/ &
		0.04000, 0.06556, 0.09111, 0.11667, 0.14222, 0.16778,  &
		0.19333, 0.21889, 0.24444, 0.27000 /)
	db(14,:)=(/ &
		1.00825, 0.98849, 0.99742, 1.02080, 1.02831, 1.03415,  &
		1.03926, 1.04808, 1.05554, 1.05862 /)
	db(15,:)=(/ &
		!tweaked entry #4 to be the average of 3 and 5. it was an outlier in the simulation. mjw 3.31.11
		1.01838, 1.02970, 0.99785, 0.99663, 0.99542, 0.99183,  &  
		0.98897, 0.99299, 0.99013, 0.98798 /)
	db(16,:)=(/ &
		0.20000, 0.31667, 0.43333, 0.55000, 0.66667, 0.78333,  &
		0.90000, 1.01667, 1.13333, 1.25000 /)
	db(17,:)=(/ &
		1.43311, 1.27347, 1.19090, 1.13367, 1.09073, 1.05602,  &
		1.02693, 1.00103, 0.97899, 0.95912 /)
	db(18,:)=(/ &
	    !tweaked entry #9 to be the average of 8 and 10. it was an outlier in the simulation mjw 3.31.11
		0.48342, 0.64841, 0.64322, 0.74366, 0.76661, 0.82764,  &
		0.97792, 1.15056, 1.23117, 1.31179 /)
    
else
    !Read the coefficients from a user-file
    !Do a test read to make sure the file is available
    read(LU_pb,fmt="(A)",advance="NO",err=200,eor=200)
    rewind(LU_pb)
    goto 205

    200 continue !There was a problem
        call messages(-1,"Power block coefficient file was not found","FATAL",224,0)
        stop
    205 continue

    !Continue reading the file
    !how long is the string containing values? We need to determine the number of entries
    line=''
    read(LU_pb,fmt='(A)') line

    !SAM is expecting each entry to be 8 long, with 1 column of header info
    n=(len(trim(line))+1)/8-1

    !Now allocate the db array
    if(allocated(db)) deallocate(db)
    allocate(db(18,n))

    !Rewind and read in the file. There are 18 effects, 1st column is label text
    rewind(LU_pb)
    do i=1,18
        read(LU_pb,fmt="(8X)",advance="NO")
        do j=1,n
            read(LU_pb,fmt="(F8.5)",advance="NO") db(i,j)
        enddo
        read(LU_pb,fmt="(X)",advance="YES")
    enddo

    close(LU_pb)

endif
*/

	//Now select which to interpolate
//n=size(db(1,:))  !All of the sub arrays should be of the same length

	//Allocate data arrays


//if(.not.allocated(datx)) 
//	allocate(datx(n),daty(n))
//endif


	switch(XT)
	{
		case 1: XI=1; break;    // A
		case 2: XI=4; break;    // B
		case 3: XI=7; break;    // C
		case 12: XI=13; break;  // AB
		case 13: XI=10; break;  // AC
		case 23: XI=16; break;  // BC
	}

	switch(YT)
	{
		case 11:  YI=2; break;      // PA
		case 12:  YI=5; break;      // PB
		case 13:  YI=8; break;      // PC
		case 112: YI=14; break;    // PAB
		case 113: YI=11; break;    // PAC
		case 123: YI=17; break;    // PBC
		case 21:  YI=3; break;      // QA
		case 22:  YI=6; break;      // QB
		case 23:  YI=9; break;      // QC
		case 212: YI=15; break;    // QAB
		case 213: YI=12; break;    // QAC
		case 223: YI=18; break;    // QBC
	}

	//Set the data to be interpolated
	//datx(1:n)=db(XI,1:n)
	//daty(1:n)=db(YI,1:n)

	if ( (XI==0) || (YI==0) ) return 0.0;

	XI--; YI--; // C++ arrays start index at 0 instead of 1, like Fortran arrays

	//Use brute force interpolation.. it is faster in this case than bisection or hunting methods used in the user-specified HTF case
	
	size_t iLastIndex = m_db.ncols()-1;
	for (size_t i=0; i < m_db.ncols(); i++)
	{
		// if we got to the last one, then set bounds and end loop
		if(i == iLastIndex)
		{
			lbi = (int)iLastIndex;
			ubi = (int)iLastIndex;
			break;
		}

		// if the x variable is outside the table range, set the bounds and get out
		if(i == 0) {
			if(m_db.at(XI,1) > m_db.at(XI,0))
			{ // The table is in ascending order
				if(X <= m_db.at(XI,0))
				{
					lbi=0; ubi=0; break;
				}
				if(X >= m_db.at(XI,iLastIndex))
				{
					lbi = (int)iLastIndex; ubi = (int)iLastIndex; break;
				}
			}
			else
			{// the table is in descending order
				if(X >= m_db.at(XI,0))
				{
					lbi=0; ubi=0; break;
				}
				if(X <= m_db.at(XI,iLastIndex))
				{
					lbi = (int)iLastIndex; ubi = (int)iLastIndex; break;
				}
			}
		}
		
		// if i = iLastIndex, the code above will catch it and break out of the loop before getting here.
		// so the reference [i+1], where i = iLastIndex, will never happen
		if( ( (X >= m_db.at(XI,i)) && (X < m_db.at(XI,i+1)) ) || ( (X <= m_db.at(XI,i)) && (X > m_db.at(XI,i+1)) ) )
		{
			lbi = (int)i;
			ubi = (int)i + 1;
			break;
		}
	}

	if(m_db.at(XI,ubi) == m_db.at(XI,lbi))
		ind = 0.0;
	else
		ind = (X-m_db.at(XI,lbi)) / (m_db.at(XI,ubi) - m_db.at(XI,lbi));

	return m_db.at(YI,lbi) + ind * (m_db.at(YI,ubi)- m_db.at(YI,lbi));
} // Interpolate





//************************************************************************************************************
//************************************************************************************************************
void CPowerBlock_Type224::evap_tower(double P_cond_min, int n_pl_inc, double DeltaT_cw_des, double T_approach, double P_cycle, 
							 double eta_ref, double T_db, double T_wb, double P_amb, double q_reject, double &m_dot_water, 
							 double &W_dot_tot, double &P_cond, double &T_cond, double &f_hrsys)
{
	double c_air, c_cooling, deltah_evap, deltat_cw, dp_evap, dl_frac, dt_out, eta_fan, eta_fan_s,
          eta_pump_s, eta_pump_t, h_fan_in, h_fan_out, h_fan_out_s, h_pcw_in, h_pcw_out,
          h_pcw_out_s, m_dot_air, m_dot_blowdown, m_dot_cw, m_dot_cw_design, m_dot_drift, blowdwn_frac,
          m_dot_evap, mass_ratio_fan, p_ratio_fan, q_reject_des, R, rho_cw, t_fan_in,
		  t_fan_in_k, t_fan_out, t_fan_out_k, w_dot_cw_pump, w_dot_fan;
	/*
	!------------------------------------------------------------------------------------------------------------
	!--Inputs
	!   * P_cond_min    [Pa]    Minimum allowable condenser pressure
	!   * n_pl_inc      [-]     Number of part load heat rejection levels
	!   * DeltaT_cw_des [K]     Cooling water temperature rise across condenser
	!   * T_approach    [K]     Cooling tower approach temperature, difference between cw out and wet bulb temp
	!   * P_cycle       [W]     Rated power block capacity
	!   * eta_ref       [-]     Rated gross conversion efficiency
	!   * T_db          [K]     Dry bulb temperature (converted to C)
	!   * P_amb         [Pa]    Atmospheric pressure
	!------------------------------------------------------------------------------------------------------------
	!--Output
	!   * m_dot_water   [kg/s]  Total cooling tower water usage
	!   * W_dot_tot     [MW]    Total parasitic power for cooling tower model
	!   * P_cond        [Pa]    Condenser steam pressure
	!   * T_cond        [K]     Condenser steam temperature
	!   * f_hrsys       [-]     Fraction of the cooling system operating
	!------------------------------------------------------------------------------------------------------------
	*/

	// Unit conversions
	T_db = T_db - 273.15;    //[C] Converted dry bulb temp
	T_wb = T_wb - 273.15;    //[C] Converted wet bulb temp

	// Values that can be estimated
	dt_out = 3.0;				// Temperature difference at hot side of the condenser
	dl_frac = 0.001;            // Drift loss fraction
	blowdwn_frac = 0.003;       // Blowdown fraction
	dp_evap = 0.37*1.0e5;       // [Pa] Pressure drop across the condenser and cooling tower
	eta_pump_t = 0.75;          // Total pump efficiency
	eta_pump_s = 0.8;           // Isentropic cooling water pump efficiency
	eta_fan = 0.75;             // Fan mechanical efficiency
	eta_fan_s = 0.8;            // Fan isentropic efficiency
	p_ratio_fan = 1.0025;       // Fan pressure ratio
	mass_ratio_fan = 1.01;      // Ratio of air flow to water flow in the cooling tower

	// Cooling water specific heat
	c_cooling = f_c_psat(P_amb);

	// **** Calculations for design conditions
	q_reject_des = P_cycle*(1./eta_ref-1.0);    	        // Heat rejection from the cycle
	m_dot_cw_design = q_reject_des/(c_cooling*DeltaT_cw_des);	// Mass flow rate of cooling water required to absorb the rejected heat
	f_hrsys = 1.0;   // Initial fraction of cooling system operating

	// **** Calculations for performance
	// Calculate the cooling water temp. rise associated with normal cooling system operation
	m_dot_cw = m_dot_cw_design;
	deltat_cw = q_reject/(m_dot_cw*c_cooling);

	// Condenser saturation temperature
	T_cond = T_wb + deltat_cw + dt_out + T_approach; // celcius

	// Condenser back pressure
	if (m_pbp.tech_type != 4)
		P_cond = f_psat_T(T_cond); // steam
	else
		P_cond = P_sat4(T_cond); // isopentane


	// MJW 7.19.2010 :: Cooling system part-load strategy uses the number of part-load increments to determine how the coolign system is
	// partially shut down during under design operation. The condenser pressure is reduced with the cooling system running
	// at full load until it reaches the minimum condenser pressure. The cooling system then incrementally shuts off bays until
	// the condenser temperature/pressure rise above their minimum level. Default cond. pressure is 1.25 inHg (4233 Pa).
	if ( (P_cond < P_cond_min) && (m_pbp.tech_type != 4) ) // Aug 3, 2011: No lower limit on Isopentane
	{
		for (int i=2; i <=n_pl_inc; i++)
		{
			f_hrsys = (1.0 - (float)((i-1.0)/n_pl_inc));
			m_dot_cw = m_dot_cw_design*f_hrsys;
			deltat_cw = q_reject/(m_dot_cw*c_cooling);
			T_cond = T_wb + deltat_cw + dt_out + T_approach;
			P_cond = f_psat_T(T_cond);
			if(P_cond > P_cond_min) break;
		}
		if(P_cond <= P_cond_min)
		{
			// Still below min. fix to min condenser pressure and recalc. temp.
			P_cond = P_cond_min;
			T_cond = f_Tsat_p(P_cond);
			deltat_cw = T_cond - (T_wb + dt_out + T_approach);
			m_dot_cw = q_reject/(deltat_cw * c_cooling);
		}
	}

	//100 continue

	// Circulating water pump power
	h_pcw_in = f_hw_psat(P_amb);											// [J/kg] cw pump inlet enthalpy
	// s_pcw_in = f_s_hw_psat(P_amb);										// [J/kg-K] cw pump inlet entropy
	rho_cw = f_rho_P(P_amb);												// [kg/m3] cooling water density in the pump
	h_pcw_out_s = (dp_evap/rho_cw) + h_pcw_in;								// [J/kg] isentropic outlet enthalpy.. incompressible fluid
	h_pcw_out = h_pcw_in + ((h_pcw_out_s - h_pcw_in)/eta_pump_s);			// [J/kg] Outlet enthalpy accounting for irreversibility
	w_dot_cw_pump = (h_pcw_out - h_pcw_in) * m_dot_cw/eta_pump_t * 1.0E-6;	// [MW] Cooling water circulating pump power


	// Fan power
	m_dot_air = m_dot_cw*mass_ratio_fan;
	t_fan_in = (T_db + T_wb + T_approach)/2.0;
	h_fan_in = f_h_air_T(t_fan_in);

	c_air = 1003.0;		// [J/kg-K] specific heat of air (This is relatively constant)
	R = 8314./28.97;	// [J/kmol-K]/[kg/kmol] Gas constant over the molar mass of air

	t_fan_in_k = t_fan_in + 273.15;										// Fan inlet temp, in K
	t_fan_out_k = t_fan_in_k * pow(p_ratio_fan,(R/c_air));				// [K] isentropic temperature rise
	t_fan_out = t_fan_out_k - 273.15;									// [C] Convert isentropic temperature rise to deg C
	h_fan_out_s = f_h_air_T(t_fan_out);									// [J/kg] Calculate isentropic enthalpy at fan outlet
	h_fan_out = h_fan_in + (h_fan_out_s - h_fan_in)/eta_fan_s;			// [J/kg] Actual enthalpy, accounting for irreversibility

	w_dot_fan = (h_fan_out - h_fan_in)*m_dot_air/eta_fan*1.0E-6;  // [MW] Fan parasitic power



	// Total cooling tower parasitic power
	W_dot_tot = w_dot_cw_pump + w_dot_fan;   // [MW]

	// Enthalpy of evaporation
	deltah_evap = f_dh_evap(P_amb);

	// Evaporative water loss
	m_dot_evap = q_reject/deltah_evap;

	// Other water losses
	m_dot_drift = dl_frac * m_dot_cw;			        // Drift loss fraction, based on cooling water mass flow rate
	m_dot_blowdown = blowdwn_frac * m_dot_cw;			// Blow down fraction

	// Total power block water usage
	m_dot_water = m_dot_evap + m_dot_drift + m_dot_blowdown;

	// Unit conversions
	T_db = T_db + 273.15;		// [C] Converted dry bulb temp (TFF - I think this is irrelevant, since it's not passed back out)
	T_wb = T_wb + 273.15;		// [C] Converted wet bulb temp (TFF - I think this is irrelevant, since it's not passed back out)
	T_cond = T_cond + 273.15;	// [K] Convert to K for output

}


//************************************************************************************************************
//************************************************************************************************************
void CPowerBlock_Type224::ACC(double P_cond_min, int n_pl_inc, double T_ITD_des, double P_cond_ratio, double P_cycle, double eta_ref, 
		 double T_db, double , double q_reject, double& m_dot_air, double& W_dot_fan, double& P_cond, double& T_cond, 
		 double f_hrsys)
{
	//use cooling property functions
	//use CSP_cooling_functions
	//implicit none

	double eta_fan, eta_fan_s, h_fan_in, h_fan_out, h_fan_out_s, mm, R, T_fan_in_K, T_fan_out, T_fan_out_K,
			T_hot_delta, T_ITD, c_air, Q_reject_design, m_dot_air_des, dT_air;
	/*
	!------------------------------------------------------------------------------------------------------------
	!--Inputs
	!   * P_cond_min    [Pa]    Minimum allowable condenser pressure
	!   * n_pl_inc      [-]     Number of part load heat rejection levels
	!   * T_ITD         [K]     ACC initial temperature difference, difference between dry bulb and steam inlet temp
	!   * P_cond_ratio  [-]     Condenser air inlet/outlet pressure ratio
	!   * P_cycle       [W]     Rated power block capacity
	!   * eta_ref       [-]     Rated gross conversion efficiency
	!   * T_db          [K]     Dry bulb temperature (converted to C)
	!   * P_amb         [Pa]    Atmospheric pressure
	!------------------------------------------------------------------------------------------------------------
	!--Output
	!   * m_dot_air     [kg/s]  Total ACC air mass flow rate
	!   * W_dot_fan     [MW]    Total parasitic power for ACC model
	!   * P_cond        [Pa]    Condenser steam pressure
	!   * T_cond        [K]     Condenser steam temperature
	!------------------------------------------------------------------------------------------------------------
	*/

	// Unit conversions
	T_db = T_db - 273.15;		// [C] Converted dry bulb temp

	// Values that can be estimated
	T_hot_delta = 3.0;          // [C] Temperature difference between saturation steam and condenser outlet air temp
	eta_fan_s = 0.8;            // [-] Fan isentropic efficiency
	eta_fan = pow(0.98,3.0);	// [-] Fan mechanical efficiency
	c_air = 1005.0;				// [J/kg-K] Specific heat of air, relatively constant over dry bulb range

	// **** Calculations for design conditions
	Q_reject_design = P_cycle*(1.0/eta_ref-1.0);							// Heat rejection from the cycle
	m_dot_air_des = Q_reject_design/(c_air*(T_ITD_des - T_hot_delta));
	f_hrsys = 1.0;

	// Fan power
	dT_air = q_reject/(m_dot_air_des*c_air);
	T_ITD = T_hot_delta + dT_air;	// [C] Calculate the actual ITD during off-design operation

	// Calculated output
	T_cond = T_db + T_ITD;		// Condensation temperature

	// Turbine back pressure
	if (m_pbp.tech_type != 4)
		P_cond = f_psat_T(T_cond); // steam
	else
		P_cond = P_sat4(T_cond); // isopentane


	// MJW 7.19.2010 :: Cooling system part-load strategy uses the number of part-load increments to determine how the coolign system is
	// partially shut down during under design operation. The condenser pressure is reduced with the cooling system running
	// at full load until it reaches the minimum condenser pressure. The cooling system then incrementally shuts off bays until
	// the condenser temperature/pressure rise above their minimum level. Default cond. pressure is 2.0 inHg (6772 Pa).
	if ( (P_cond < P_cond_min) && (m_pbp.tech_type != 4) ) // Aug 3, 2011: No lower limit on Isopentane
	{
		for (int i=2; i<=n_pl_inc; i++)
		{
			f_hrsys = (1.0 - (float)((i-1.0)/n_pl_inc));
			m_dot_air = m_dot_air_des*f_hrsys;
			dT_air = q_reject/(m_dot_air*c_air);
			T_cond = T_db + T_hot_delta + dT_air;
			P_cond = f_psat_T(T_cond);
			if(P_cond > P_cond_min) break;
		}
		if (P_cond <= P_cond_min)
		{
			// Still below min. fix to min condenser pressure and recalc. temp.
			P_cond = P_cond_min;
			T_cond = f_Tsat_p(P_cond);
			dT_air = T_cond - (T_db + T_hot_delta);
			m_dot_air = q_reject/(dT_air*c_air);
		}
	}

	//100 continue
	h_fan_in = f_h_air_T(T_db);		// [J/kg] Fan inlet enthalpy

	mm = 28.97;						// [kg/kmol] molar mass of air
	R = 8314.0/mm;					// [J/kg-K] Gas constant for air

	// These temperature calculations are for the isentropic expansion across the fan, not accounting for heat gain in the ACC
	T_fan_in_K = T_db + 273.15;									// [K] Fan inlet temperature
	T_fan_out_K = T_fan_in_K * pow(P_cond_ratio,(R/c_air));
	T_fan_out = T_fan_out_K - 273.15;							// [C] Fan outlet temperature
	// dT_fan = T_fan_out - T_db;									// [C] Difference in temperature including irreversibilities in fan

	h_fan_out_s = f_h_air_T(T_fan_out);							// [J/kg] Isentropic fan outlet temperature
	h_fan_out = h_fan_in + (h_fan_out_s - h_fan_in)/eta_fan_s;	// [J/kg] Actual fan outlet temperature
	// Total ACC parasitic power
	W_dot_fan = (h_fan_out - h_fan_in)*m_dot_air/eta_fan*1.0e-6;// [MW] Fan power

	// Unit conversions
	T_db = T_db + 273.15;		// [C] Converted dry bulb temp (TFF - I think this is irrelevant, since it's not passed back out)
	T_cond = T_cond + 273.15;    // [K] Convert to K for output
}




//************************************************************************************************************
//************************************************************************************************************
void CPowerBlock_Type224::HybridHR(/*double fcall,*/ double P_cond_min, int n_pl_inc, /*double time,*/ double F_wc, double F_wcmax, double F_wcmin, 
				  double T_ITD_des, double T_approach, double dT_cw_ref, double P_cond_ratio, double P_cycle, double eta_ref, 
				  double T_db, double T_wb, double P_amb, double q_reject, double& m_dot_water, double& W_dot_acfan, 
				  double& W_dot_wctot, double& W_dot_tot, double& P_cond, double& T_cond, double f_hrsys)
{
	/*
	!------------------------------------------------------------------------------------------------------------
	!This subroutine models a hybrid wet/dry cooling heat rejection system. In this system, a dry-cooled condenser
	!is responsible for rejecting the thermal load, except a supplemental wet-cooled system is placed in parallel
	!to aid in heat rejection during the hottest hours of the day. The wet cooled system can reject heat based
	!on the wetbulb temperature, and thus will have much lower parasitics in rejecting a fraction of the heat than
	!the dry cooled system will, and the dry cooled system running at normal power will result in a lower
	!condenser temperature and pressure.
	!
	!Several assumptions are made in the control of this system. The user can specify a cooling distribution factor
	!on the thermal storage page with the other TOU factors. The fraction indicates what the distribution of
	!the heat rejection load will be. If the fraction is 0.2 for example, then the wet cooling tower will reject
	!20% of the load.
	!
	!The wet-cooling system is a forced-draft tower, and is sized based on the largest TOU fraction supplied in the
	!control array.
	!
	!--Inputs----------------------------------------------------------------------------------------------------
	!   * P_cond_min    [Pa]    Minimum allowable condenser pressure
	!   * n_pl_inc      [-]     Number of part load heat rejection levels
	!   * time          [-]     hour of the year
	!   * F_wc          [-]     Wet cooling fraction
	!   * F_wcmax       [-]     Maximum annual wet cooling fraction
	!   * F_wcmin       [-]     Minimum annual wet cooling fraction
	!   * T_ITD_des     [K]     ACC initial temperature difference, difference between dry bulb and steam inlet temp
	!   * T_approach    [K]     Wet cooling tower approach temperature, difference between cw out and wet bulb temp
	!   * P_cond_ratio  [-]     Condenser air inlet/outlet pressure ratio
	!   * P_cycle       [W]     Rated power block capacity
	!   * eta_ref       [-]     Rated gross conversion efficiency
	!   * T_db          [K]     Dry bulb temperature (converted to C)
	!   * T_wb          [K]     Wet bulb temperature (converted to C)
	!   * P_amb         [Pa]    Atmospheric pressure
	!   * q_reject      [W]     Total required heat rejection load
	!------------------------------------------------------------------------------------------------------------
	!--Output
	!   * m_dot_water   [kg/s]  Total cooling tower water usage
	!   * W_dot_acfan   [MW]    Total parasitic power for ACC fan
	!   * W_dot_wctot   [MW]    Total parasitic power for cooling tower
	!   * W_dot_tot     [MW]    Total overall parasitic power
	!   * P_cond        [Pa]    Condenser steam pressure
	!   * T_cond        [K]     Condenser steam temperature
	!------------------------------------------------------------------------------------------------------------
	*/

	//!use cooling property functions
	//use CSP_cooling_functions

	//implicit none
	
	// seem unused
	//double , p_amb, p_cond, p_cond_ratio, p_cycle, rh, t_cond, t_db, w_dot_wctot, t_itd_des, etap_pcw_s, t_wb, w_dot_acfan, , , , , time, 

	// input parameters - defined in function parameter list
	//double F_wc, F_wcmax, F_wcmin, eta_ref, q_reject, m_dot_water, W_dot_tot, dT_cw_ref, P_cond_min, n_pl_inc, f_hrsys, fcall;


	// local vars
	double m_dot_acair = 0, h_acfan_in, T_acfan_in_K, T_acfan_out_K, T_acfan_out, h_acfan_out_s,  
	q_ac_rej, q_wc_rej, h_pcw_in, rho_cw, h_pcw_out_s, h_pcw_out, m_dot_wcair, m_dot_cw=0,
	h_wcfan_in, T_wcfan_in_K, T_wcfan_out, h_wcfan_out_s, h_wcfan_out, W_dot_wcfan, m_dot_evap, m_dot_drift, m_dot_blowdown, h_acfan_out, 
    f_hrsysair, f_hrsyswc, T_condwc, T_condair, dT_air, R, T_ITD, DeltaT_cw,
	T_wcfan_in, W_dot_cw_pump, T_wcfan_out_K, deltaH_evap;

	int i,j;


	// these are static - are they 'reset' in the code and preserved over calls to this function?
	//save::T_hot_diff, eta_acfan_s, eta_acfan, C_air, drift_loss_frac, blowdown_frac, dP_evap, eta_pump, eta_pcw_s, eta_wcfan, &
	//      eta_wcfan_s, P_ratio_wcfan, mass_ratio_wcfan, Q_reject_des, q_ac_des, m_dot_acair_des, q_wc_des, c_cw, m_dot_cw_des
	
	// Only call the parameter equations at the beginning of the simulation. Once they're established, they don't need to be reset each time.
	//if(fcall == 1.0)
	//if(m_bFirstCall)
//	if(true)
	{
		// Values that can be estimated--------
		//-dry
		T_hot_diff = 3.0;				//[C] Temperature difference between saturation steam and condenser outlet air temp
		eta_acfan_s = 0.8;				//[-] Fan isentropic efficiency
		eta_acfan = pow(0.98,3);		//[-] Fan mechanical efficiency
		C_air = 1005.0;					//[J/kg-K] specific heat of air (This is relatively constant)
		R = 286.986538;					//[J/kg-K] Gas constant for air = 8314./28.97

		//-wet
		drift_loss_frac = 0.001;		//Drift loss fraction
		blowdown_frac = 0.003;			//Blowdown fraction
		dP_evap = 0.37*1.e5;			//[Pa] Pressure drop across the condenser and cooling tower
		eta_pump = 0.75;				//Total pump efficiency
		eta_pcw_s = 0.8;				//Isentropic cooling water pump efficiency
		eta_wcfan = 0.75;				//Fan mechanical efficiency
		eta_wcfan_s = 0.8;				//Fan isentropic efficiency
		P_ratio_wcfan = 1.0025;			//Fan pressure ratio
		mass_ratio_wcfan = 1.01;		//Ratio of air flow to water flow in the cooling tower
	}

	//**** Calculations for design conditions
	Q_reject_des = P_cycle*(1.0/eta_ref - 1.0);    	    //Heat rejection from the cycle
	//-dry
	q_ac_des = Q_reject_des*(1.0 - F_wcmin);    //Size the ACC to always be able to handle the load that isn't going to the wet cooler
	m_dot_acair_des = q_ac_des/(C_air*(T_ITD_des - T_hot_diff));
	//-wet
	q_wc_des = Q_reject_des*F_wcmax;			//Size the wet cooler to handle the maximum fraction in the control array
	//c_cw = f_c_psat(P_amb);						//Cooling water specific heat

	//Unit conversions
	T_db = T_db - 273.15;        //[C] Converted dry bulb temp
	T_wb = T_wb - 273.15;
	c_cw = f_c_psat(P_amb);      //Cooling water specific heat (TFF, this is also calculated above.)
	m_dot_cw_des = q_wc_des/(c_cw*dT_cw_ref);	//Mass flow rate of cooling water required to absorb the rejected heat 

	//Calculate the cooling loads
	q_ac_rej = q_reject*(1.0 - F_wc);
	q_wc_rej = q_reject*F_wc;
	f_hrsyswc = 1.0;
	f_hrsysair = 1.0;

	//-ACC
	dT_air = q_ac_rej/(m_dot_acair_des * C_air);
	T_ITD = T_hot_diff + dT_air;  //[C] Calculate the actual ITD during off-design operation
	//-WC
	DeltaT_cw = q_wc_rej/(m_dot_cw_des * c_cw);

	//***Calculated output
	//Condensation temperature is the maximum of either the wet or dry system cooling stream outlet temperature (plus hot side dT)
	T_condwc = T_wb + DeltaT_cw + T_hot_diff + T_approach;
	T_condair = T_db + T_ITD;
	if (F_wc > 0.0) //MJW 7.23.2010
		T_cond = dmax1(T_condwc, T_condair);
	else
		T_cond = T_condair;

	if (m_pbp.tech_type != 4)
		P_cond =  f_psat_T(T_cond); // steam
	else
		P_cond = P_sat4(T_cond); // isopentane

	// MJW 7.19.2010 :: Cooling system part-load strategy uses the number of part-load increments to determine how the coolign system is
	// partially shut down during under-design operation. The condenser pressure is reduced with the cooling system running
	// at full load until it reaches the minimum condenser pressure. The cooling system then incrementally shuts off bays until
	// the condenser temperature/pressure rise above their minimum level. Default cond. pressure is 2.0 inHg (6772 Pa).
	i=1; j=1;
	if ( (P_cond < P_cond_min) && (m_pbp.tech_type != 4) ) // Aug 3, 2011: No lower limit on Isopentane
	{
		do
		{
			if(T_condwc > T_condair)
			{
				i++;
				//Reduce just wet cooled
				f_hrsyswc = (1.0 - (float)((i-1.0)/n_pl_inc));
				m_dot_cw = m_dot_cw_des*f_hrsyswc;
				DeltaT_cw = q_wc_rej/(m_dot_cw*c_cw);
				T_condwc = T_wb + DeltaT_cw + T_hot_diff + T_approach;
			}
			else
			{
				i++;
				j++;
				//Reduce both wet and dry cooled
				f_hrsysair = (1.0 - (float)((j-1.0)/n_pl_inc));
				m_dot_acair = m_dot_acair_des*f_hrsysair;
				dT_air = q_ac_rej/(m_dot_acair*C_air);
				T_condair = T_db + dT_air + T_hot_diff;
				//--
				f_hrsyswc = (1.0 - (float)((i-1.0)/n_pl_inc));
				m_dot_cw = m_dot_cw_des*f_hrsyswc;
				DeltaT_cw = q_wc_rej/(m_dot_cw*c_cw);
				T_condwc = T_wb + DeltaT_cw + T_hot_diff + T_approach;
			}

			if(F_wc > 0.0) //MJW 7.23.2010
				T_cond = dmax1(T_condwc, T_condair);
			else
				T_cond = T_condair;
			P_cond = f_psat_T(T_cond);

			//if(P_cond > P_cond_min) goto 100
			if((i >= n_pl_inc) || (j >= n_pl_inc) ) break;

		} while (P_cond < P_cond_min);

		if (P_cond <= P_cond_min)
		{
			//Still below min. fix to min condenser pressure and recalc. temp.
			P_cond = P_cond_min;
			T_cond = f_Tsat_p(P_cond);
			if(T_condwc > T_condair)
			{
				DeltaT_cw = T_cond - (T_wb + T_hot_diff + T_approach);
				m_dot_cw = q_reject/(DeltaT_cw*c_cw);
			}
			else
			{
				dT_air = T_cond - (T_db + T_hot_diff);
				m_dot_acair = q_reject/(dT_air*C_air);
			}
		}
	}

//100	f_hrsys = (f_hrsyswc + f_hrsysair)/2;
	f_hrsys = (f_hrsyswc + f_hrsysair)/2.0;

	//-----ACC Fan power---------
	h_acfan_in = f_h_air_T(T_db);  //[J/kg] Fan inlet enthalpy

	//These temperature calculations are for the isentropic expansion across the fan, not accounting for heat gain in the ACC
	T_acfan_in_K = T_db + 273.15;  //[K] Fan inlet temperature
	T_acfan_out_K = T_acfan_in_K * pow(P_cond_ratio,(R/C_air));
	T_acfan_out = T_acfan_out_K - 273.15;    //[C] Fan outlet temperature
	// dT_acfan = T_acfan_out - T_db;   //[C] Difference in temperature including irreversibilities in fan

	h_acfan_out_s = f_h_air_T(T_acfan_out);	//[J/kg] Isentropic fan outlet temperature
	h_acfan_out = h_acfan_in + (h_acfan_out_s - h_acfan_in)/eta_acfan_s;   //[J/kg] Actual fan outlet temperature
	//Total ACC parasitic power
	W_dot_acfan = (h_acfan_out - h_acfan_in) * m_dot_acair/eta_acfan*1.e-6;  //[MW] Fan power


	//-----Wet cooling parasitics --------
	if(q_wc_rej > 0.001)
	{
		//Circulating water pump power
		h_pcw_in = f_hw_psat(P_amb);     //[J/kg] cw pump inlet enthalpy
		// s_pcw_in = f_s_hw_psat(P_amb);     //[J/kg-K] cw pump inlet entropy
		rho_cw = f_rho_P(P_amb);         //[kg/m3] cooling water density in the pump
		h_pcw_out_s = dP_evap/rho_cw + h_pcw_in;                         //[J/kg] isentropic outlet enthalpy.. incompressible fluid
		h_pcw_out = h_pcw_in + (h_pcw_out_s - h_pcw_in)/eta_pcw_s;       //[J/kg] Outlet enthalpy accounting for irreversibility
		W_dot_cw_pump = (h_pcw_out - h_pcw_in)*m_dot_cw/eta_pump*1.e-6;  //[MW] Cooling water circulating pump power

		//Fan power
		m_dot_wcair = m_dot_cw*mass_ratio_wcfan;
		T_wcfan_in = (T_db + T_wb + T_approach)/2.0;
		h_wcfan_in = f_h_air_T(T_wcfan_in);

		T_wcfan_in_K = T_wcfan_in + 273.15;  //Fan inlet temp, in K
		T_wcfan_out_K = T_wcfan_in_K * pow(P_ratio_wcfan,(R/C_air));    //[K] isentropic temperature rise
		T_wcfan_out = T_wcfan_out_K - 273.15;    //[C] Convert isentropic temperature rise to deg C
		h_wcfan_out_s = f_h_air_T(T_wcfan_out);  //[J/kg] Calculate isentropic enthalpy at fan outlet
		h_wcfan_out = h_wcfan_in + (h_wcfan_out_s - h_wcfan_in)/eta_wcfan_s;   //[J/kg] Actual enthalpy, accounting for irreversibility

		W_dot_wcfan = (h_wcfan_out - h_wcfan_in)*m_dot_wcair/eta_wcfan*1.0E-6;  //[MW] Fan parasitic power

		//Total wet cooling tower parasitic power
		W_dot_wctot = W_dot_cw_pump + W_dot_wcfan;   //[MW]

		//Enthalpy of evaporation
		deltaH_evap = f_dh_evap(P_amb);

		//Evaporative water loss
		m_dot_evap = q_wc_rej/deltaH_evap;

		//Other water losses
		m_dot_drift = drift_loss_frac*m_dot_cw;	//Drift loss fraction, based on cooling water mass flow rate
		m_dot_blowdown = blowdown_frac*m_dot_cw;	//Blow down fraction

		//Total power block water usage
		m_dot_water = m_dot_evap + m_dot_drift + m_dot_blowdown;
		}
	else
	{
		//Otherwise set the wet-cooling outputs to zero
		m_dot_water = 0.0;
		W_dot_wctot = 0.0;
	}

	W_dot_tot = W_dot_wctot + W_dot_acfan;

	//Unit conversions
	T_db = T_db + 273.15;    //[C] Converted dry bulb temp
	T_wb = T_wb + 273.15;    //[C] Converted wet bulb temp
	T_cond = T_cond + 273.15;    //[K] Convert to K for output
}
