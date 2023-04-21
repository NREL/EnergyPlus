#ifndef _CODE_GENERATOR_INPUTS_H_
#define _CODE_GENERATOR_INPUTS_H_

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "../ssc/sscapi.h"
#include "../ssc/core.h"
#include <map>
#include <string>
#include <type_traits>

namespace {
	static const char * SSCDIR = std::getenv("SSCDIR");
}

static ssc_bool_t my_handler(ssc_module_t p_mod, ssc_handler_t p_handler, int action,
	float f0, float f1, const char *s0, const char *s1, void *user_data)
{
	if (action == SSC_LOG)
	{
		// print log message to console
		switch ((int)f0)
		{
		case SSC_NOTICE: printf("Notice: %s", s0); break;
		case SSC_WARNING: printf("Warning: %s", s0); break;
		case SSC_ERROR: printf("Error: %s", s0); break;
		}
		return 1;
	}
	else if (action == SSC_UPDATE)
	{
		// print status update to console
		printf("(%.2f %%) %s", f0, s0);
		return 1; // return 0 to abort simulation as needed.
	}
	else
		return 0;
}

static int set_array(ssc_data_t p_data, const char *name, const char* fn, int len)
{
	char buffer[1024];
	char *record, *line;
	int i = 0;
	ssc_number_t *ary;
	FILE *fp = fopen(fn, "r");
	if (fp == NULL)
	{
		printf("file opening failed ");
		return 0;
	}
	ary = (ssc_number_t *)malloc(len * sizeof(ssc_number_t));
	while ((line = fgets(buffer, sizeof(buffer), fp)) != NULL)
	{
		record = strtok(line, ",");
		while ((record != NULL) && (i < len))
		{
			ary[i] = (ssc_number_t)atof(record);
			record = strtok(NULL, ",");
			i++;
		}
	}
	fclose(fp);
	ssc_data_set_array(p_data, name, ary, len);
	free(ary);
	return 1;
}

static int set_matrix(ssc_data_t p_data, const char *name, const char* fn, int nr, int nc)
{
	char buffer[1024];
	char *record, *line;
	ssc_number_t *ary;
	int i = 0, len = nr*nc;
	FILE *fp = fopen(fn, "r");
	if (fp == NULL)
	{
		printf("file opening failed ");
		return 0;
	}
	ary = (ssc_number_t *)malloc(len * sizeof(ssc_number_t));
	while ((line = fgets(buffer, sizeof(buffer), fp)) != NULL)
	{
		record = strtok(line, ",");
		while ((record != NULL) && (i < len))
		{
			ary[i] = (ssc_number_t)atof(record);
			record = strtok(NULL, ",");
			i++;
		}
	}
	fclose(fp);
	ssc_data_set_matrix(p_data, name, ary, nr, nc);
	free(ary);
	return 1;
}

static int run_module(ssc_data_t & data, std::string module_name, bool printErrors=true)
{
	ssc_module_exec_set_print(0);
	if (data == NULL)
	{
		if (printErrors){ printf("error: out of memory.");}
		return -1;
	}
	ssc_module_t module;
	module = ssc_module_create(const_cast<char*>(module_name.c_str()));
	if (NULL == module)
	{
		if (printErrors){ printf("error: could not create module.");}
		return -1;
	}

	// C++ exception handling
	try {

		if (ssc_module_exec(module, data) == 0)
		{
			if (printErrors) {printf("error during simulation.");}
			int i = 0;
			compute_module *cm = static_cast<compute_module*>(module);
			while (cm->log(i) != nullptr) {
				if (printErrors) {printf("%s\n", cm->log(i)->text.c_str());}
				i++;
			}
			ssc_module_free(module);
			return -1;
		}
	}
	catch (std::exception& e) {
		if (printErrors) {printf("Exception: %s", e.what());}
		return -1;
	}
	
	ssc_module_free(module);
	module = nullptr;
	return 0;
}

/**
*   Data for high-level integration test that modifies data with a set of key value pairs
*/
static int modify_ssc_data_and_run_module(ssc_data_t &data, std::string module_name, std::map<std::string, double> pairs)
{
	for (std::map<std::string, double>::iterator it = pairs.begin(); it != pairs.end(); it++)
	{
		std::string name = std::string(it->first);
		ssc_data_set_number(data, const_cast<char *>(name.c_str()), static_cast<ssc_number_t>(it->second));
	}
	return run_module(data, module_name);
}

static int modify_ssc_data_and_run_module(ssc_data_t &data, std::string module_name, std::map<std::string, std::string> pairs)
{
	for (std::map<std::string, std::string>::iterator it = pairs.begin(); it != pairs.end(); it++)
	{
		std::string name = std::string(it->first);
		std::string value = it->second;
		ssc_data_set_string(data, const_cast<char *>(name.c_str()), const_cast<char *>(value.c_str()));
	}
	return run_module(data, module_name);
}


#endif
