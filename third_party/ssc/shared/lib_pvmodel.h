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

#ifndef __pvmodulemodel_h
#define __pvmodulemodel_h

#include <string>

class pvcelltemp_t;
class pvpower_t;

class pvinput_t
{
public:
	pvinput_t();
	pvinput_t( double ib, double id, double ig, double irear, double ip, 
		double ta, double td, double ws, double wd, double patm,
		double zen, double inc, 
		double elv, double tlt, double azi,
		double hrday, int rmode , bool up);

	double Ibeam; // beam irradiance, W/m2
	double Idiff; // sky diffuse irradiance, W/m2
	double Ignd; // ground reflected irradiance, W/m2
	double Irear; // rear side irradiance, W/m2 **needs to be the amount actually usable by panel, so must be multiplied by bifaciality
	double poaIrr; // plane of array irradiance, W/m2
	double Tdry; // dry bulb temp, C
	double Tdew; // dew point temp, C
	double Wspd; // wind speed, m/s
	double Wdir; // wind direction, deg +from north
	double Patm; // atmospheric pressure, millibar
	double Zenith; // zenith angle, deg
	double IncAng; // incidence angle on surface, deg
	double Elev; // site elevation, m
	double Tilt; // surface tilt angle, deg +from horizontal
	double Azimuth; // surface azimuth angle, deg +from north (E=90,S=180)
	double HourOfDay; // hour of the day 0=12am, 23=11pm
	int radmode; //radiation mode
	bool usePOAFromWF; // use poa directly flag
};

class pvoutput_t
{
public:
	pvoutput_t();
	pvoutput_t( double p, double v,
		double c, double e, 
		double voc, double isc, double t, double aoi_modifier);

	double Power; // output power, Watts
	double Voltage; // operating voltage, V
	double Current; // operating current, A
	double Efficiency; // operating efficiency, fraction (0..1)
	double Voc_oper; // open circuit voltage at operating condition, V
	double Isc_oper; // short circuit current at operating condition, A
	double CellTemp; // cell temperature, 'C
	double AOIModifier; // angle-of-incidence modifier for total poa irradiance on front side of module (0-1)
};

class pvmodule_t; // forward decl

class pvcelltemp_t
{
protected:
	std::string m_err;
public:
	
	virtual bool operator() ( pvinput_t &input, pvmodule_t &module, double opvoltage, double &Tcell ) = 0;
	std::string error();

	virtual ~pvcelltemp_t() {};
};

class pvmodule_t
{
protected:
	std::string m_err;
public:

	virtual double AreaRef() = 0;
	virtual double VmpRef() = 0;
	virtual double ImpRef() = 0;
	virtual double VocRef() = 0;
	virtual double IscRef() = 0;


	virtual bool operator() ( pvinput_t &input, double TcellC, double opvoltage, pvoutput_t &output ) = 0;
	std::string error();

	virtual ~pvmodule_t() {};
};



class spe_module_t : public pvmodule_t
{
public:
	double VmpNominal;
	double VocNominal;
	double Area; // m2
	double Gamma; // temp coefficient %/'C
	int Reference; // specification of reference condition.  valid values: 0..4
	double fd; // diffuse fraction
	double Eff[5]; // as fractions
	double Rad[5]; // W/m2

	spe_module_t( );	
	static double eff_interpolate( double irrad, double rad[5], double eff[5] );
	
	double WattsStc() { return Eff[Reference] * Rad[Reference] * Area; }

	virtual double AreaRef() { return Area; }
	virtual double VmpRef() { return VmpNominal; }
	virtual double ImpRef() { return WattsStc()/VmpRef(); }
	virtual double VocRef() { return VocNominal; }
	virtual double IscRef() { return ImpRef()*1.3; }
	virtual bool operator() ( pvinput_t &input, double TcellC, double opvoltage, pvoutput_t &output);

	virtual ~spe_module_t() {};
};

#define AOI_MIN 0.5
#define AOI_MAX 89.5

double current_5par( double V, double IMR, double A, double IL, double IO, double RS, double RSH );
double current_5par_rec(double V, double IMR, double A, double IL, double IO, double RS, double RSH, double D2MuTau, double Vbi);
double openvoltage_5par( double Voc0, double a, double IL, double IO, double Rsh );
double openvoltage_5par_rec(double Voc0, double a, double IL, double IO, double Rsh, double D2MuTau, double Vbi);
double maxpower_5par( double Voc_ubound, double a, double Il, double Io, double Rs, double Rsh, double *Vmp=0, double *Imp=0);
double maxpower_5par_rec(double Voc_ubound, double a, double Il, double Io, double Rs, double Rsh, double D2MuTau, double Vbi, double *__Vmp=0, double *__Imp=0);
double air_mass_modifier( double Zenith_deg, double Elev_m, double a[5] );



#endif
