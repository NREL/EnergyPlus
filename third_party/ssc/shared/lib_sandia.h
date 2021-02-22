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

#ifndef __lib_sandia_h
#define __lib_sandia_h

#include "lib_pvmodel.h"
#include <vector>


/*
	Implementation of report SAND2004-3535
	Photovoltaic Array Performance Model
	King, Boyson, Kratochvil

	http://photovoltaics.sandia.gov/docs/PDF/King%20SAND.pdf
*/

class sandia_module_t : public pvmodule_t
{
public:
	double A0, A1, A2, A3, A4;
	double B0, B1, B2, B3, B4, B5;
	double C0, C1, C2, C3, C4, C5, C6, C7;
	double Isc0, aIsc;
	double Imp0, aImp;
	double Voc0, BVoc0, mBVoc;
	double Vmp0, BVmp0, mBVmp;
	double Ix0, Ixx0;
	double fd, DiodeFactor, NcellSer;
	double Area;

	sandia_module_t( );	
	
	virtual double AreaRef() { return Area; }
	virtual double VmpRef() { return Vmp0; }
	virtual double ImpRef() { return Imp0; }
	virtual double VocRef() { return Voc0; }
	virtual double IscRef() { return Isc0; }
	virtual bool operator() ( pvinput_t &input, double TcellC, double opvoltage, pvoutput_t &output);
};



class sandia_celltemp_t : public pvcelltemp_t
{
public:
	double a, b, DT0, fd;	
	virtual bool operator() ( pvinput_t &input, pvmodule_t &module, double opvoltage, double &Tcell );
		
	static double sandia_tcell_from_tmodule( double Tm, double poaIrr, double fd, double DT0);
	static double sandia_module_temperature( double poaIrr, double Ws, double Ta, double fd, double a, double b );
};




/*
	Implementation of report SAND2007-5036
	Performance Model for Grid-Connected Photovoltaic Inverters
	King, Gonzalez, Galbraith, Boyson

	http://photovoltaics.sandia.gov/Pubs_2010/old/075036.pdf
*/

class sandia_inverter_t
{
public:
	sandia_inverter_t( );

	double Paco;    /* Maximum AC power rating, upper limit value  (Wac) */
	double Pdco;    /* DC power level at which Paco is achieved (Wdc) */
	double Vdco;    /* DC voltage level at which Paco is achieved (Vdc) */
	double Pso;     /* DC power require to start inversion process, or self-consumption by inverter (Wdc) */
	double Pntare;  /* AC power consumed by inverter at night as parasitic load (Wac) */
	double C0;      /* (1/W, empirical, default 0) Defines parabolic curvature of relationship between ac power and dc power at reference conditions */
	double C1;      /* (1/V, empirical, default 0) Parameter allowing Pdco to vary linearly with dc voltage input */
	double C2;      /* (1/V, empirical, default 0) Parameter allowing Pso to vary linearly with dc voltage input */
	double C3;      /* (1/V, empirical, default 0) Parameter allowing C0 to vary linearly with dc voltage input */

	//function that calculates AC power and inverter losses for a single inverter with one MPPT input
	bool acpower(	
		/* inputs */
		double Pdc,     /* Input power to a SINGLE inverter (Wdc). Note that with several inverters, this is the power to ONE inverter.*/
		double Vdc,     /* Voltage input to inverter (Vdc) */

		/* outputs */
		double *Pac,    /* AC output power (Wac) */
		double *Ppar,   /* AC parasitic power consumption (Wac) */
		double *Plr,    /* Part load ratio (Pdc_in/Pdc_rated, 0..1) */
		double *Eff,	    /* Conversion efficiency (0..1) */
		double *Pcliploss, /* Power loss due to clipping loss (Wac) */
		double *Psoloss, /* Power loss due to operating power consumption (Wdc) */
		double *Pntloss /* Power loss due to night time tare loss (Wac) */
		);

	//function that calculates AC power and inverter losses for a single inverter with multiple MPPT inputs
	bool acpower(
		/* inputs */
		std::vector<double> Pdc,     /* Vector of Input power to inverter (Wdc), one per MPPT input on the inverter. Note that with several inverters, this is the power to ONE inverter.*/
		std::vector<double> Vdc,     /* Vector of voltage inputs to inverter (Vdc), one per MPPT input on the inverter */

		/* outputs */
		double *Pac,    /* AC output power (Wac) */
		double *Ppar,   /* AC parasitic power consumption (Wac) */
		double *Plr,    /* Part load ratio (Pdc_in/Pdc_rated, 0..1) */
		double *Eff,	    /* Conversion efficiency (0..1) */
		double *Pcliploss, /* Power loss due to clipping loss (Wac) */
		double *Psoloss, /* Power loss due to operating power consumption (Wdc) */
		double *Pntloss /* Power loss due to night time tare loss (Wac) */
	);

} ;

#endif
