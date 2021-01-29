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

#ifndef _iec61853_h
#define _iec61853_h

#include "lib_util.h"
#include "lib_pvmodel.h"

class Imessage_api {
public: virtual ~Imessage_api() { }
		virtual void Printf(const char *, ...) = 0;
		virtual void Outln(const char *) = 0;
};

class iec61853_module_t : public pvmodule_t
{
public:
	enum { monoSi, multiSi, CdTe, CIS, CIGS, Amorphous, _maxTypeNames };
	static const char *module_type_names[_maxTypeNames];

	iec61853_module_t();
	void set_fs267_from_matlab();

	// model parameters
	double alphaIsc;
	double n;
	double Il;
	double Io;
	double C1;
	double C2;
	double C3;
	double D1;
	double D2;
	double D3;
	double Egref;

	// additional tempco
	double betaVoc;
	double gammaPmp;
	
	// STC parameters
	double Vmp0, Imp0, Voc0, Isc0;
	
	// physical data
	int NcellSer;
	double Area;
	bool GlassAR;
	double AMA[5];

	Imessage_api *_imsg;


	#define ROW_MAX 30
	enum { COL_IRR, COL_TC, COL_PMP, COL_VMP, COL_VOC, COL_ISC, COL_MAX };
	static const char *col_names[COL_MAX];
	enum { IL, IO, RS, RSH, A, PARMAX };
	static const char *par_names[PARMAX];
	
	bool calculate( util::matrix_t<double> &input, int nseries, int type, 
		util::matrix_t<double> &par, bool verbose );
	
	bool solve( double Voc, double Isc, double Vmp, double Imp, double a,
			double *p_Il, double *p_Io, double *p_Rs, double *p_Rsh );
	bool tcoeff( util::matrix_t<double> &input, size_t icol, double irr, 
		double *tempc, bool verbose );

	virtual double AreaRef() { return Area; }
	virtual double VmpRef() { return Vmp0; }
	virtual double ImpRef() { return Imp0; }
	virtual double VocRef() { return Voc0; }
	virtual double IscRef() { return Isc0; }
	virtual bool operator() ( pvinput_t &input, double TcellC, double opvoltage, pvoutput_t &output );
};






#endif

