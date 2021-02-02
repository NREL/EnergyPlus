#ifndef SYSTEM_ADVISOR_MODEL_CMOD_WIND_LANDBOSSE_H
#define SYSTEM_ADVISOR_MODEL_CMOD_WIND_LANDBOSSE_H


#include <future>
#include <chrono>
#include <stdio.h>
#include <iostream>
#include <string>

#include "sscapi.h"
#include "vartab.h"
#include "core.h"

#ifdef _MSC_VER
#define popen _popen
#define pclose _pclose
#endif

class cm_wind_landbosse : public compute_module {
private:
    std::string python_module_name;
    std::string python_exec_path;
    std::string python_run_cmd;

    void load_config();

public:
    cm_wind_landbosse();

    std::string call_python_module(const std::string& input_json);

#ifdef __WINDOWS__
	std::string call_python_module_windows(const std::string& input_json);
#endif

	void cleanOutputString(std::string& output_json);

    void exec() override;
};

#endif //SYSTEM_ADVISOR_MODEL_CMOD_WIND_LANDBOSSE_H
