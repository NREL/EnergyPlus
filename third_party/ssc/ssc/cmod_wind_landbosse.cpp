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
#include <fstream>
#include <future>

#ifdef __WINDOWS__
#include <Windows.h>
#include <stdio.h>
#include <tchar.h>
#pragma warning(disable: 4191)
#include "AtlBase.h"
#include "AtlConv.h"
#endif

#include <json/json.h>

#include "sscapi.h"
#include "core.h"
#include "cmod_wind_landbosse.h"

static var_info _cm_vtab_wind_landbosse[] = {
     /*   VARTYPE           DATATYPE         NAME                              LABEL                                                      UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
        // Inputs
        { SSC_INPUT,        SSC_NUMBER, "en_landbosse",                          "Enable landbosse (1 for enabled)",                        "",         "",                      "LandBOSSE",      "*",                       "",                         "" },
        { SSC_INPUT,        SSC_STRING, "wind_resource_filename",                "Local hourly wind data file path",                        "",         "",                      "LandBOSSE",      "*",                       "",                         "" },
        { SSC_INPUT,        SSC_NUMBER, "distance_to_interconnect_mi",           "Distance to Interconnect",                                "miles",    "",                      "LandBOSSE",      "*",                       "",                         "" },
        { SSC_INPUT,        SSC_NUMBER, "interconnect_voltage_kV",               "Interconnect Voltage",                                    "kV",       "",                      "LandBOSSE",      "*",                       "",                         "" },
        { SSC_INPUT,        SSC_NUMBER, "depth",                                 "Foundation Depth",                                        "m",        "",                      "LandBOSSE",      "*",                       "",                         "" },
        { SSC_INPUT,        SSC_NUMBER, "rated_thrust_N",                        "Rated Thrust",                                            "N",        "",                      "LandBOSSE",      "*",                       "",                         "" },
        { SSC_INPUT,        SSC_NUMBER, "labor_cost_multiplier",                 "Labor Cost Multiplier",                                   "",         "",                      "LandBOSSE",      "*",                       "",                         "" },
        { SSC_INPUT,        SSC_NUMBER, "gust_velocity_m_per_s",                 "50 year Gust Velocity",                                   "m/s",      "",                      "LandBOSSE",      "*",                       "",                         "" },
        { SSC_INPUT,        SSC_NUMBER, "wind_resource_shear",                   "Wind Shear Exponent",                                     "",         "",                      "LandBOSSE",      "*",                       "",                         "" },

        { SSC_INPUT,        SSC_NUMBER, "num_turbines",                          "Number of Turbines",                                      "",         "",                      "LandBOSSE",      "*",                       "INTEGER,",                 "" },
        { SSC_INPUT,        SSC_NUMBER, "turbine_spacing_rotor_diameters",       "Turbine Spacing",                                         "diameters","",                      "LandBOSSE",      "*",                       "",                         "" },
        { SSC_INPUT,        SSC_NUMBER, "row_spacing_rotor_diameters",           "Row Spacing",                                             "diameters","",                      "LandBOSSE",      "*",                       "",                         "" },
        { SSC_INPUT,        SSC_NUMBER, "turbine_rating_MW",                     "Turbine Rating",                                          "kW",       "",                      "LandBOSSE",      "*",                       "",                         "" },
        { SSC_INPUT,        SSC_NUMBER, "wind_turbine_hub_ht",                   "Hub Height",                                              "m",        "",                      "LandBOSSE",      "*",                       "",                         "" },
        { SSC_INPUT,        SSC_NUMBER, "wind_turbine_rotor_diameter",           "Rotor Diameter",                                          "m",        "",                      "LandBOSSE",      "*",                       "",                         "" },

        { SSC_OUTPUT, 		SSC_STRING, "errors",	            		        "BOS - Error message",			                                 "",  "", "LandBOSSE", "en_landbosse=1", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "bonding_usd",			                "BOS - Management - Bonding Cost",		                       	 "$", "", "LandBOSSE", "en_landbosse=1", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "collection_equipment_rental_usd",       "BOS - Collection - Equipment Rental Cost",		            	 "$", "", "LandBOSSE", "en_landbosse=1", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "collection_labor_usd",			        "BOS - Collection - Labor Cost",			                     "$", "", "LandBOSSE", "en_landbosse=1", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "collection_material_usd",	            "BOS - Collection - Materials Cost",		                 	 "$", "", "LandBOSSE", "en_landbosse=1", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "collection_mobilization_usd",	        "BOS - Collection - Mobilization Cost",		                	 "$", "", "LandBOSSE", "en_landbosse=1", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "construction_permitting_usd",	        "BOS - Management - Construction Permitting Cost",	    		 "$", "", "LandBOSSE", "en_landbosse=1", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "development_labor_usd",			        "BOS - Development - Labor Cost",			                     "$", "", "LandBOSSE", "en_landbosse=1", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "development_material_usd",		        "BOS - Development - Material Cost",			                 "$", "", "LandBOSSE", "en_landbosse=1", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "development_mobilization_usd",	        "BOS - Development - Mobilization Cost",		            	 "$", "", "LandBOSSE", "en_landbosse=1", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "engineering_usd",			            "BOS - Management - Engineering Cost",			                 "$", "", "LandBOSSE", "en_landbosse=1", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "erection_equipment_rental_usd",	        "BOS - Erection - Equipment Rental Cost",		            	 "$", "", "LandBOSSE", "en_landbosse=1", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "erection_fuel_usd",			            "BOS - Erection - Fuel Cost",			                         "$", "", "LandBOSSE", "en_landbosse=1", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "erection_labor_usd",		      	    "BOS - Erection - Labor Cost",			                         "$", "", "LandBOSSE", "en_landbosse=1", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "erection_material_usd",		            "BOS - Erection - Material Cost",			                     "$", "", "LandBOSSE", "en_landbosse=1", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "erection_mobilization_usd",		        "BOS - Erection - Mobilization Cost",			                 "$", "", "LandBOSSE", "en_landbosse=1", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "erection_other_usd",			        "BOS - Erection - Other Cost",			                         "$", "", "LandBOSSE", "en_landbosse=1", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "foundation_equipment_rental_usd",       "BOS - Foundation - Equipment Rental Cost",		            	 "$", "", "LandBOSSE", "en_landbosse=1", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "foundation_labor_usd",			        "BOS - Foundation - Labor Cost",			                     "$", "", "LandBOSSE", "en_landbosse=1", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "foundation_material_usd",		        "BOS - Foundation - Material Cost",			                     "$", "", "LandBOSSE", "en_landbosse=1", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "foundation_mobilization_usd",      	    "BOS - Foundation - Mobilization Cost",			                 "$", "", "LandBOSSE", "en_landbosse=1", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "insurance_usd",			                "BOS - Management - Insurance Cost",			                 "$", "", "LandBOSSE", "en_landbosse=1", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "markup_contingency_usd",	   	        "BOS - Management - Markup Contingency",			             "$", "", "LandBOSSE", "en_landbosse=1", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "project_management_usd",		        "BOS - Management - Project Management Cost",			         "$", "", "LandBOSSE", "en_landbosse=1", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "site_facility_usd",			            "BOS - Management - Site Facility Cost",			             "$", "", "LandBOSSE", "en_landbosse=1", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "sitepreparation_equipment_rental_usd",  "BOS - Site Preparation - Equipment Rental Cost",		    	 "$", "", "LandBOSSE", "en_landbosse=1", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "sitepreparation_labor_usd",			    "BOS - Site Preparation - Labor Cost",			                 "$", "", "LandBOSSE", "en_landbosse=1", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "sitepreparation_material_usd",			"BOS - Site Preparation - Material Cost",			             "$", "", "LandBOSSE", "en_landbosse=1", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "sitepreparation_mobilization_usd",	    "BOS - Site Preparation - Mobilization Cost",			         "$", "", "LandBOSSE", "en_landbosse=1", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "total_collection_cost",			        "BOS - Total Collection Cost",			                         "$", "", "LandBOSSE", "en_landbosse=1", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "total_development_cost",		        "BOS - Total Development Cost",			                         "$", "", "LandBOSSE", "en_landbosse=1", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "total_erection_cost",			        "BOS - Total Erection Cost",			                         "$", "", "LandBOSSE", "en_landbosse=1", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "total_foundation_cost",			        "BOS - Total Foundation Cost",			                         "$", "", "LandBOSSE", "en_landbosse=1", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "total_gridconnection_cost",		   	    "BOS - Total Grid Connection Cost",			                     "$", "", "LandBOSSE", "en_landbosse=1", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "total_management_cost",			        "BOS - Total Management Cost",			                         "$", "", "LandBOSSE", "en_landbosse=1", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "total_sitepreparation_cost",			"BOS - Total Site Preparation Cost",			                 "$", "", "LandBOSSE", "en_landbosse=1", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "total_substation_cost",			        "BOS - Total Substation Cost",			                         "$", "", "LandBOSSE", "en_landbosse=1", "", "", } ,
        { SSC_OUTPUT, 		SSC_NUMBER, "total_bos_cost",			            "BOS - Total BOS Cost",			                             "$", "", "LandBOSSE", "en_landbosse=1", "", "", } ,

        var_info_invalid
};

cm_wind_landbosse::cm_wind_landbosse() {
    add_var_info(_cm_vtab_wind_landbosse);
}

void cm_wind_landbosse::load_config(){
    std::string python_config_path = get_python_path();

    if (python_config_path.empty())
        throw exec_error("wind_landbosse", "Path to SAM python configuration directory not set. "
                                           "Use 'set_python_path' function in sscapi.h to point to the correct folder.");

    // load python configuration
    Json::Value python_config_root;
    std::ifstream python_config_doc(python_config_path + "/python_config.json");
    if (python_config_doc.fail())
        throw exec_error("wind_landbosse", "Could not open 'python_config.json'. "
                                           "Use 'set_python_path' function in sscapi.h to point to the folder containing the file.");

#ifdef __WINDOWS__
    // check for byte-order mark indicating UTF-8 and skip if it exists since it's not JSON-compatible
    char a,b,c;
    a = (char)python_config_doc.get();
    b = (char)python_config_doc.get();
    c = (char)python_config_doc.get();
    if (a != (char)0xEF || b != (char)0xBB || c != (char)0xBF) {
        python_config_doc.seekg(0);
    }
#endif
    python_config_doc >> python_config_root;

    if (!python_config_root.isMember("exec_path"))
        throw exec_error("wind_landbosse", "Missing key 'exec_path' in 'python_config.json'.");
    if (!python_config_root.isMember("python_version"))
        throw exec_error("wind_landbosse", "Missing key 'python_version' in 'python_config.json'.");


    python_exec_path = python_config_root["exec_path"].asString();
    auto python_version = python_config_root["python_version"].asString();

    // load landbosse configuration
    Json::Value landbosse_config_root;
    std::ifstream landbosse_config_doc(python_config_path + "/landbosse.json");
    if (landbosse_config_doc.fail())
        throw exec_error("wind_landbosse", "Could not open 'landbosse.json'. "
                                           "Use 'set_python_path' function in sscapi.h to point to the folder containing the file.");

    landbosse_config_doc >> landbosse_config_root;

    if (!landbosse_config_root.isMember("run_cmd"))
        throw exec_error("wind_landbosse", "Missing key 'run_cmd' in 'landbosse.json'.");
    if (!landbosse_config_root.isMember("min_python_version"))
        throw exec_error("wind_landbosse", "Missing key 'min_python_version' in 'landbosse.json'.");

    python_run_cmd = landbosse_config_root["run_cmd"].asString();
    auto min_python_version = landbosse_config_root["min_python_version"].asString();

    // check version works out
    std::stringstream min_ver(min_python_version);
    std::stringstream py_ver(python_version);
    std::string min_ver_token, py_ver_token;

    while (std::getline(min_ver, min_ver_token, '.')) {
        if (!std::getline(py_ver, py_ver_token, '.'))
            return;
        if (std::stoi(min_ver_token) > std::stoi(py_ver_token))
            throw exec_error("wind_landbosse", "'min_python_version' requirement not met.");
    }
}

const size_t BUFSIZE = 4096;

std::string cm_wind_landbosse::call_python_module(const std::string& input_dict_as_text){
    std::promise<std::string> python_result;
    std::future<std::string> f_completes = python_result.get_future();
    std::thread([&]
                {
                    std::string cmd = std::string(get_python_path()) + "/" + python_exec_path + " -c \"" + python_run_cmd + "\"";
                    size_t pos = cmd.find("<input>");
                    cmd.replace(pos, 7, input_dict_as_text);

                    FILE *file_pipe = popen(cmd.c_str(), "r");
                    if (!file_pipe){
                        python_result.set_value("wind_landbosse error. Could not call python with cmd:\n" + cmd);
                        return;
                    }

                    std::string mod_response;
                    char buffer[BUFSIZE];
                    while (fgets(buffer, sizeof(buffer), file_pipe)){
                        mod_response += buffer;
                    }
                    pclose(file_pipe);
                    if (mod_response.empty())
                        python_result.set_value("LandBOSSE error. Function did not return a response.");
                    else
                        python_result.set_value(mod_response);
                }
    ).detach();

    std::chrono::system_clock::time_point time_passed
            = std::chrono::system_clock::now() + std::chrono::seconds(60 * 5);

    if(std::future_status::ready == f_completes.wait_until(time_passed))
        return f_completes.get();
    else
        throw exec_error("wind_landbosse", "python handler error. Python process timed out.");
}

#ifdef __WINDOWS__
std::string cm_wind_landbosse::call_python_module_windows(const std::string& input_dict_as_text) {
	STARTUPINFO si;
	SECURITY_ATTRIBUTES sa;
	PROCESS_INFORMATION pi;
    HANDLE stdin_rd = NULL;
    HANDLE stdout_wr = NULL;
    HANDLE stdout_rd = NULL;
    HANDLE stdin_wr = NULL;
    HANDLE stderr_rd = NULL;
    HANDLE stderr_wr = NULL;  //pipe handles
	char buf[BUFSIZE];           //i/o buffer
	memset(buf, 0, sizeof(buf));

	std::string pythonpath = std::string(get_python_path()) + "\\" + python_exec_path;
	CA2T programpath( pythonpath.c_str());
	std::string pythonarg = " -c \"" + python_run_cmd + "\"";
	size_t pos = pythonarg.find("<input>");
	pythonarg.replace(pos, 7, input_dict_as_text);
	CA2T programargs(pythonarg.c_str());

	sa.nLength = sizeof(SECURITY_ATTRIBUTES);
	sa.bInheritHandle = TRUE;
	sa.lpSecurityDescriptor = NULL;

    if (!CreatePipe(&stdin_rd, &stdin_wr, &sa, 0)) {
        goto done;
    }
	if (!SetHandleInformation(stdin_wr, HANDLE_FLAG_INHERIT, 0)) {
		goto done;
	}
    if (!CreatePipe(&stdout_rd, &stdout_wr, &sa, 0)) {
        goto done;
    }
	if (!SetHandleInformation(stdout_rd, HANDLE_FLAG_INHERIT, 0)) {
		goto done;
	}
    if (!CreatePipe(&stderr_rd, &stderr_wr, &sa, 0)) {
        goto done;
    }
	if (!SetHandleInformation(stderr_rd, HANDLE_FLAG_INHERIT, 0)) {
		goto done;
	}

    //set startupinfo for the spawned process
    /*The dwFlags member tells CreateProcess how to make the process.
    STARTF_USESTDHANDLES: validates the hStd* members.
    STARTF_USESHOWWINDOW: validates the wShowWindow member*/
    GetStartupInfo(&si);

    si.dwFlags = STARTF_USESTDHANDLES | STARTF_USESHOWWINDOW;
    si.wShowWindow = SW_HIDE;
    //set the new handles for the child process
    si.hStdOutput = stdout_wr;
    si.hStdError = stderr_wr;
    si.hStdInput = stdin_rd;

    //spawn the child process
    if (CreateProcess(programpath, programargs, NULL, NULL, TRUE, CREATE_NO_WINDOW,
        NULL, NULL, &si, &pi)) {
        unsigned long bread;   //bytes read
        unsigned long bread_last = 0;
        unsigned long avail;   //bytes available

        for (;;) {
            PeekNamedPipe(stdout_rd, buf, BUFSIZE - 1, &bread, &avail, NULL);
            //check to see if there is any data to read from stdout
            if (bread != 0) {
                if (ReadFile(stdout_rd, buf, BUFSIZE - 1, &bread, NULL)) {
                    bread_last = bread;
                }
            }
            else if (bread_last > 0)
            {
                break;
            }
        }

        CloseHandle(pi.hThread);
        CloseHandle(pi.hProcess);
    }
    done:
    std::vector<HANDLE> handles = {stdin_rd, stdin_wr, stdout_rd, stdout_wr, stderr_rd, stderr_wr};
    for (HANDLE handle : handles) {
        if (handle && handle != INVALID_HANDLE_VALUE) {
            CloseHandle(handle);
        }
    }
    if (buf[0] == '\0')
        throw exec_error("wind_landbosse", "LandBOSSE error. Function did not return a response.");
	return buf;
}
#endif

void cm_wind_landbosse::cleanOutputString(std::string& output_json) {
	size_t pos = output_json.find("{");
	if (pos != std::string::npos)
		output_json = output_json.substr(pos);
	std::replace(output_json.begin(), output_json.end(), '\'', '\"');
}

void cm_wind_landbosse::exec() {
    if (!m_vartab->lookup("en_landbosse")->num[0])
        return;

    // limit the input json through the process pip to only landbosse-required inputs
    var_table input_data;
    input_data.assign_match_case("weather_file_path", *m_vartab->lookup("wind_resource_filename"));
    input_data.assign_match_case("distance_to_interconnect_mi", *m_vartab->lookup("distance_to_interconnect_mi"));
    input_data.assign_match_case("interconnect_voltage_kV", *m_vartab->lookup("interconnect_voltage_kv"));
    input_data.assign_match_case("depth", *m_vartab->lookup("depth"));
    input_data.assign_match_case("rated_thrust_N", *m_vartab->lookup("rated_thrust_n"));
    input_data.assign_match_case("labor_cost_multiplier", *m_vartab->lookup("labor_cost_multiplier"));
    input_data.assign_match_case("gust_velocity_m_per_s", *m_vartab->lookup("gust_velocity_m_per_s"));
    input_data.assign_match_case("wind_shear_exponent", *m_vartab->lookup("wind_resource_shear"));
    input_data.assign_match_case("num_turbines", *m_vartab->lookup("num_turbines"));
    input_data.assign_match_case("turbine_spacing_rotor_diameters", *m_vartab->lookup("turbine_spacing_rotor_diameters"));
    input_data.assign_match_case("row_spacing_rotor_diameters", *m_vartab->lookup("row_spacing_rotor_diameters"));
    input_data.assign_match_case("turbine_rating_MW", *m_vartab->lookup("turbine_rating_mw"));
    input_data.assign_match_case("hub_height_meters", *m_vartab->lookup("wind_turbine_hub_ht"));
    input_data.assign_match_case("rotor_diameter_m", *m_vartab->lookup("wind_turbine_rotor_diameter"));

    std::string input_json = ssc_data_to_json(&input_data);
	std::string input_dict_as_text = input_json;
	std::replace(input_dict_as_text.begin(), input_dict_as_text.end(), '\"', '\'');

    load_config();
#ifdef __WINDOWS__
	std::string output_json = call_python_module_windows(input_dict_as_text);
#else
    std::string output_json = call_python_module(input_dict_as_text);
#endif

	cleanOutputString(output_json);
    auto output_data = static_cast<var_table*>(json_to_ssc_data(output_json.c_str()));
    if (output_data->is_assigned("error")){
        m_vartab->assign("errors", output_json);
        return;
    }

    m_vartab->merge(*output_data, false);

    auto error_vd = m_vartab->lookup("errors");
    if (error_vd && error_vd->type == SSC_ARRAY)
        m_vartab->assign("errors", std::to_string(int(0)));
    if (error_vd && error_vd->type == SSC_DATARR)
        m_vartab->assign("errors", error_vd->vec[0].str);
}

DEFINE_MODULE_ENTRY( wind_landbosse, "Land-based Balance-of-System Systems Engineering (LandBOSSE) cost model", 1 )
