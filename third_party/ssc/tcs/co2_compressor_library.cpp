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

#include "co2_compressor_library.h"

double compressor_psi_polynomial_fit(int comp_type, double phi)
{
	switch( comp_type )
	{
	case 1:				// Sandia compressor from John Dyreby's work
		return (((-498626.0*phi + 53224.0)*phi - 2505.0)*phi + 54.6)*phi + 0.04049;		// from performance map curve fit

	default:
		return (((-498626.0*phi + 53224.0)*phi - 2505.0)*phi + 54.6)*phi + 0.04049;		// from performance map curve fit

	}
}

double compressor_eta_polynomial_fit(int comp_type, double phi)
{
	switch( comp_type )
	{
	case 1:
		return (((-1.638E6*phi + 182725.0)*phi - 8089.0)*phi + 168.6)*phi - 0.7069;		// from performance map curve fit

	default:
		return (((-1.638E6*phi + 182725.0)*phi - 8089.0)*phi + 168.6)*phi + 0.7069;		// from performance map curve fit
	}
}

void get_compressor_parameters(int comp_type, double & phi_design, double & phi_min, double & phi_max)
{
	switch( comp_type )
	{
	case 1:				// Sandia compressor from John Dyreby's work
		phi_design = 0.0297;
		phi_min = 0.021;
		phi_max = 0.05;
		return;

	default:
		phi_design = 0.0297;
		phi_min = 0.021;
		phi_max = 0.05;
		return;
	}
}