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

#ifndef __6_PAR_JACOBIAN_H__
#define __6_PAR_JACOBIAN_H__

template< typename Real, typename F, int n, int m >
void jacobian( Real x[n], Real f[m], Real J[n][m], F &func, Real epsrel )
{
	// create copy of x input vector
	Real x1[n];
	for ( int j=0; j < n; j++ )
		x1[j] = x[j];
	
	// storage for result of calculating function at x1
	Real f1[m];
	
	for ( int j=0 ; j < n; j++ )
	{
		Real xj = x[j];
		Real dx = epsrel * fabs( xj );
		
		if (dx == 0)
			dx = epsrel;
		
		// overwrite j position with forward difference value
		x1[j] = xj + dx;
		dx = x1[j] - xj; // trick from NR to reduce finite precision error, esp with float or double
		
		func( x1, f1 );
		
		// replace original xj value
		x1[j] = xj;
		
		// compute forward difference derivate for each row at current column
		// i.e. Jij = dFi / dxj
		for ( int i=0; i < m; i++ )
			J[i][j] = ( f1[i] - f[i] ) / dx;
	}
}
#endif