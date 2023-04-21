/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (�Alliance�) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
*  The Government retains for itself and others acting on its behalf a nonexclusive, paid-up,
*  irrevocable worldwide license in the software to reproduce, prepare derivative works, distribute
*  copies to the public, perform publicly and display publicly, and to permit others to do so.
*
*  Redistribution and use in source and binary forms, with or without modification, are permitted
*  provided that the following conditions are met:
*
*  1. Redistributions of source code must retain the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer.
*
*  2. Redistributions in binary form must reproduce the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer in the documentation and/or
*  other materials provided with the distribution.
*
*  3. The entire corresponding source code of any redistribution, with or without modification, by a
*  research entity, including but not limited to any contracting manager/operator of a United States
*  National Laboratory, any institution of higher learning, and any non-profit organization, must be
*  made publicly available under this license for as long as the redistribution is made available by
*  the research entity.
*
*  4. Redistribution of this software, without modification, must refer to the software by the same
*  designation. Redistribution of a modified version of this software (i) may not refer to the modified
*  version by the same designation, or by any confusingly similar designation, and (ii) must refer to
*  the underlying software originally provided by Alliance as �System Advisor Model� or �SAM�. Except
*  to comply with the foregoing, the terms �System Advisor Model�, �SAM�, or any confusingly similar
*  designation may not be used to refer to any modified version of this software or any modified
*  version of the underlying software originally provided by Alliance without the prior written consent
*  of Alliance.
*
*  5. The name of the copyright holder, contributors, the United States Government, the United States
*  Department of Energy, or any of their employees may not be used to endorse or promote products
*  derived from this software without specific prior written permission.
*
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
*  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
*  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER,
*  CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR
*  EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
*  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
*  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
*  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
*  THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*******************************************************************************************************/
#ifndef _LIB_6PAR_NEWTON_H_
#define _LIB_6PAR_NEWTON_H_

#include <cmath>

#include "6par_jacobian.h"
#include "6par_lu.h"
#include "6par_search.h"

#include <limits>

/// Newton Method using Line Search and Backtracking
/// from Globally Convergent Methods for Nonlinear Systems of Equations, "Numerical Recipes in C"
template< typename Real, typename F, int n >
int newton( Real x[n], Real residual[n], bool &check, F &func, 
	int MAXITER, const Real TOLF, const Real TOLMIN, const Real STPMX,
	bool (*notify)(int iter, Real x[], Real resid[], const int, void *) = 0,
	void *notify_data = 0)
{
	const Real TOLX = std::numeric_limits<Real>::epsilon();
	
	int i,j,its;
	Real den,f,fold,stpmax,sum,temp,test;
	Real g[n],p[n],xold[n];
	Real fjac[n][n];
	
	Real lu[n][n];
	int permute[n];
		
	f = fminsum<Real, F, n>(x, residual, func);
	test=0.0;
	for (i=0;i<n;i++)
		if (fabs(residual[i]) > test) 
			test=fabs(residual[i]);
		
	if (test < 0.01*TOLF)
	{
		check = false;
		return 0;
	}
	
	sum=0.0;
	for (i=0;i<n;i++)
		sum += x[i]*x[i];
		
	stpmax = STPMX*mymax(sqrt(sum), (Real)n);
	for (its=0;its<MAXITER;its++)
	{
		if ( notify != 0 )
		{
			bool ok = (*notify)(its, x, residual, n, notify_data);
			if (!ok)
				return -3;
		}

		jacobian<Real, F, n, n>( x, residual, fjac, func, 1e-8 );
		
		for (i=0;i<n;i++) 
		{
			sum=0.0;
			for (j=0;j<n;j++) sum += fjac[j][i]*residual[j];
			g[i]=sum;
		}
		
		for (i=0;i<n;i++)
			xold[i]=x[i];
			
		fold=f;
		
		for (i=0;i<n;i++)
			p[i] = -residual[i];
		
				
		if (!lu_decomp<Real, n>( fjac, lu, permute )) return false;			
		lu_solve<Real, n>( lu, permute, p, p );
		
		if (!search<Real, F, n>(xold, fold, g, p, x, f, stpmax, check, func, residual))
			return -2;
		
		test=0.0;
		for (i=0;i<n;i++)
			if (fabs(residual[i]) > test)
				test=fabs(residual[i]);
				
		if (test < TOLF)
		{
			check=false;
			return its+1;
		}
		
		if (check) {
			test=0.0;
			den=mymax(f,0.5*n);
			for (i=0;i<n;i++) {
				temp=fabs(g[i])*mymax(fabs(x[i]),1.0)/den;
				if (temp > test) test=temp;
			}
			check=(test < TOLMIN) ? true : false;
			return its+1;
		}
		
		test=0.0;
		for (i=0;i<n;i++) {
			temp=(fabs(x[i]-xold[i]))/mymax(fabs(x[i]),1.0);
			if (temp > test) test=temp;
		}
		
		if (test < TOLX)
			return its+1;
	}
	
	return -1;
}
#endif
