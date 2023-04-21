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

#ifndef _FINANCIAL_H_
#define _FINANCIAL_H_ 1
#include "math.h"
#include <vector>
#include "mod_base.h"

class SolarField;

class Financial : public mod_base
 {
     double _tower_cost;
     double _rec_cost;
     double _site_cost;
     double _heliostat_cost;
     double _wiring_cost;
     double _contingency_cost;
     double _total_direct_cost;
     double _total_indirect_cost;
     double _land_cost;
     double _sales_tax_cost;
     double _total_installed_cost;

     std::vector< double > _pricing_array;
     std::vector< int > _schedule_array;

     var_financial *_var_fin;

public:
    void Create(var_map &V);
    void updateCalculatedParameters(var_map &V);

    std::vector< double >* getPricingArray();
    std::vector< int >* getScheduleArray();

    void CreateHourlyTODSchedule(var_map &V);
	void calcPlantCapitalCost(var_map &V);

 } ;

#endif