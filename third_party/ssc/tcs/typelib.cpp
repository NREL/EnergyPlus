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

#include "tcstype.h"

TCS_DEFINE_TYPE( weatherreader )
TCS_DEFINE_TYPE( trnsys_weatherreader )
TCS_DEFINE_TYPE( solarcollector )
TCS_DEFINE_TYPE( pump )
TCS_DEFINE_TYPE( datatest )
TCS_DEFINE_TYPE( sumprod )
TCS_DEFINE_TYPE( sco2_test_type401 )
TCS_DEFINE_TYPE( sam_sco2_recomp_type424 )
TCS_DEFINE_TYPE( tc_test_type402 )
TCS_DEFINE_TYPE( sam_trough_model_type805 )
TCS_DEFINE_TYPE( sam_trough_plant_type807 )
TCS_DEFINE_TYPE( sam_trough_storage_type806 )
TCS_DEFINE_TYPE( sam_mw_pt_type224 )
TCS_DEFINE_TYPE( sam_mw_trough_type250 )
TCS_DEFINE_TYPE( sam_type250_input_generator )
TCS_DEFINE_TYPE( sam_mw_trough_type251 )
TCS_DEFINE_TYPE( sam_mw_csp_SumCalcs )
TCS_DEFINE_TYPE( sam_mw_pt_heliostatfield )
TCS_DEFINE_TYPE( sam_lf_st_pt_type232 )
TCS_DEFINE_TYPE( sam_mw_pt_type222 )
TCS_DEFINE_TYPE( sam_dsg_controller_type265 )
TCS_DEFINE_TYPE( sam_mw_gen_type260 )
TCS_DEFINE_TYPE( sam_mw_lf_type262 )
TCS_DEFINE_TYPE( sam_mw_type234 )
TCS_DEFINE_TYPE( sam_mw_pt_type228 )
TCS_DEFINE_TYPE( sam_pf_dish_collector_type295 )
TCS_DEFINE_TYPE( sam_pf_dish_receiver_type296 )
TCS_DEFINE_TYPE( sam_pf_dish_engine_type297 )
TCS_DEFINE_TYPE( sam_pf_dish_parasitics_type298 )
TCS_DEFINE_TYPE( sam_mw_lf_type261_steam )
TCS_DEFINE_TYPE( sam_iscc_powerblock )
TCS_DEFINE_TYPE( sam_iscc_parasitics )
TCS_DEFINE_TYPE( sam_mw_lf_type261_Wnet )
TCS_DEFINE_TYPE( tou_translator )
TCS_DEFINE_TYPE( atmospheric_aod )
TCS_DEFINE_TYPE( Heliostat3DInterp )
//TCS_DEFINE_TYPE( sam_geothermal_hybrid_pb )

TCS_BEGIN_EXPORT()
	TCS_EXPORT_TYPE( weatherreader )
	TCS_EXPORT_TYPE( trnsys_weatherreader )
	TCS_EXPORT_TYPE( solarcollector )
	TCS_EXPORT_TYPE( pump )
	TCS_EXPORT_TYPE( datatest )
	TCS_EXPORT_TYPE( sumprod )
	TCS_EXPORT_TYPE( sco2_test_type401 )
	TCS_EXPORT_TYPE( sam_sco2_recomp_type424 )
	TCS_EXPORT_TYPE( tc_test_type402 )
	TCS_EXPORT_TYPE( sam_trough_model_type805 )
	TCS_EXPORT_TYPE( sam_trough_plant_type807 )
	TCS_EXPORT_TYPE( sam_trough_storage_type806 )
	TCS_EXPORT_TYPE( sam_mw_pt_type224 )
	TCS_EXPORT_TYPE( sam_mw_trough_type250 )
	TCS_EXPORT_TYPE( sam_type250_input_generator )
	TCS_EXPORT_TYPE( sam_mw_trough_type251 )
	TCS_EXPORT_TYPE( sam_mw_csp_SumCalcs )
	TCS_EXPORT_TYPE( sam_mw_pt_heliostatfield )
	TCS_EXPORT_TYPE( sam_lf_st_pt_type232 )
	TCS_EXPORT_TYPE( sam_mw_pt_type222 )
	TCS_EXPORT_TYPE( sam_mw_gen_type260 )
	TCS_EXPORT_TYPE( sam_dsg_controller_type265 )
	TCS_EXPORT_TYPE( sam_mw_lf_type262 )
	TCS_EXPORT_TYPE( sam_mw_type234 )
	TCS_EXPORT_TYPE( sam_mw_pt_type228 )
	TCS_EXPORT_TYPE( sam_pf_dish_collector_type295 )
	TCS_EXPORT_TYPE( sam_pf_dish_receiver_type296 )
	TCS_EXPORT_TYPE( sam_pf_dish_engine_type297 )
	TCS_EXPORT_TYPE( sam_pf_dish_parasitics_type298 )
	TCS_EXPORT_TYPE( sam_mw_lf_type261_steam )
	TCS_EXPORT_TYPE( sam_iscc_powerblock )
	TCS_EXPORT_TYPE( sam_iscc_parasitics )
	TCS_EXPORT_TYPE( sam_mw_lf_type261_Wnet )
	TCS_EXPORT_TYPE( tou_translator )
	TCS_EXPORT_TYPE( atmospheric_aod )
	TCS_EXPORT_TYPE( Heliostat3DInterp )
	//TCS_EXPORT_TYPE( sam_geothermal_hybrid_pb )
TCS_END_EXPORT()




