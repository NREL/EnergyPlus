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

#ifndef _CM_WINDPOWER_
#define _CM_WINDPOWER_

#include "core.h"
#include "lib_windfile.h"
#include "lib_windwatts.h"
// for adjustment factors
#include "common.h"
#include "lib_util.h"

/**
 * Winddata uses var_data instead of a file. Format: For each measurement height, four fields should be provided
 * (pres, temp, spd, dir). Data matrix has a time-step per row, and one field per column, four field columns per height.
 */

class winddata : public winddata_provider
{
	size_t irecord;
	util::matrix_t<double> data;
	std::string stdErrorMsg;
public:
	explicit winddata(var_data *data_table);

	size_t nrecords() override;

	ssc_number_t get_number(var_data *v, const char *name);

	ssc_number_t *get_vector(var_data *v, const char *name, size_t *len);

	bool read_line(std::vector<double> &values) override;

	std::string get_stdErrorMsg(){ return stdErrorMsg; };
};

class cm_windpower : public compute_module
{
private:
public:

	cm_windpower();

	void exec() override;
};

#endif