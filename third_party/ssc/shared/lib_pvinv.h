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

#ifndef __lib_pvinv_h
#define __lib_pvinv_h

#include <vector>
/*
	Implementation of inverter partload curve with linear interpolation
*/

class partload_inverter_t
{
public:
	partload_inverter_t( );

	double Vdco;    /* Nominal DC voltage inptu (Vdc) */
	double Paco;    /* Maximum AC power rating, upper limit value  (Wac) */
	double Pdco;    /* DC power level at which Paco is achieved (Wdc) */
	double Pntare;  /* AC power consumed by inverter at night as parasitic load (Wac) */
	std::vector<double> Partload; /* Array of partload values (Pdc/Paco) for linear interpolation */
	std::vector<double> Efficiency; /* Array of efficiencies corresponding to partload values */

	//function that calculates AC power and inverter losses for a single inverter with one MPPT input
	bool acpower(
		/* inputs */
		double Pdc,     /* Input power to inverter (Wdc), one per MPPT input on the inverter. Note that with several inverters, this is the power to ONE inverter.*/

									 /* outputs */
		double *Pac,    /* AC output power (Wac) */
		double *Plr,    /* Part load ratio (Pdc_in/Pdc_rated, 0..1) */
		double *Ppar,   /* AC parasitic power consumption (Wac) */
		double *Eff,	    /* Conversion efficiency (0..1) */
		double *Pcliploss, /* Power loss due to clipping loss (Wac) */
		double *Pntloss /* Power loss due to night time tare loss (Wac) */
	);

	//function that calculates AC power and inverter losses for a single inverter with multiple MPPT inputs
	bool acpower(	
		/* inputs */
		std::vector<double> Pdc,     /* Vector of Input power to inverter (Wdc), one per MPPT input on the inverter. Note that with several inverters, this is the power to ONE inverter.*/

		/* outputs */
		double *Pac,    /* AC output power (Wac) */
		double *Plr,    /* Part load ratio (Pdc_in/Pdc_rated, 0..1) */
		double *Ppar,   /* AC parasitic power consumption (Wac) */
		double *Eff,	    /* Conversion efficiency (0..1) */
		double *Pcliploss, /* Power loss due to clipping loss (Wac) */
		double *Pntloss /* Power loss due to night time tare loss (Wac) */
		);

} ;

#endif
