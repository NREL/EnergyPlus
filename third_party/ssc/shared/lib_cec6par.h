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

#ifndef cec6par_h
#define cec6par_h

#include "lib_pvmodel.h"



/*
   Implementation of CEC 5 (6) parameter model as presented by
   DeSoto, Klein, and Beckman, Solar Energy Journal 2005   
   http://minds.wisconsin.edu/handle/1793/7602
*/

class cec6par_module_t : public pvmodule_t
{
public:	
	double Area;
	double Vmp;
	double Imp;
	double Voc;
	double Isc;
	double alpha_isc;
	double beta_voc;
	double a;
	double Il;
	double Io;
	double Rs;
	double Rsh;
	double Adj;

	cec6par_module_t();

	virtual double AreaRef() { return Area; }
	virtual double VmpRef() { return Vmp; }
	virtual double ImpRef() { return Imp; }
	virtual double VocRef() { return Voc; }
	virtual double IscRef() { return Isc; }

	virtual bool operator() ( pvinput_t &input, double TcellC, double opvoltage, pvoutput_t &output );

	virtual ~cec6par_module_t() {};
};


class noct_celltemp_t : public pvcelltemp_t
{
public:
	double standoff_tnoct_adj;
	double ffv_wind;
	double Tnoct;

	virtual bool operator() ( pvinput_t &input, pvmodule_t &module, double opvoltage, double &Tcell );

	virtual ~noct_celltemp_t(){}
};


class mcsp_celltemp_t : public pvcelltemp_t
{
public:
	double DcDerate; // DC derate factor (0..1)
	int MC; // Mounting configuration (1=rack,2=flush,3=integrated,4=gap)
	int HTD; // Heat transfer dimension (1=Module,2=Array)
	int MSO; // Mounting structure orientation (1=does not impede flow beneath, 2=vertical supports, 3=horizontal supports)
	size_t Nrows, Ncols; // number of modules in rows and columns, when using array heat transfer dimensions
	double Length; // module length, along horizontal dimension, (m)
	double Width; // module width, along vertical dimension, (m)
	double Wgap;  // gap width spacing (m)
	double TbackInteg;  // back surface temperature for integrated modules ('C)
	
	virtual bool operator() ( pvinput_t &input, pvmodule_t &module, double opvoltage, double &Tcell );

	virtual ~mcsp_celltemp_t() {};
};

#endif

