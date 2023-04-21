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

#ifndef __6_PAR_SEARCH_H__
#define __6_PAR_SEARCH_H__

#include <limits>

template< typename Real > Real mymax(Real a, Real b) { return a > b ? a : b; }

template< typename Real, typename F, int n >
Real fminsum ( Real x[n], Real f[n], F &func )
{
	Real sum=0;
	func(x, f);
	for (int i=0;i<n;i++)
		sum += f[i]*f[i];
	return 0.5*sum;
}
	
template< typename Real, typename F, int n >
bool search( Real xold[n], const Real fold, Real g[n], Real p[n],
			Real x[n], Real &f, const Real stpmax, bool &check, F &func, Real fvec[n])
{
	const Real ALF=1.0e-4, TOLX = std::numeric_limits<Real>::epsilon();
	Real a,alam,alam2=0.0,alamin,b,disc,f2=0.0;
	Real rhs1,rhs2,slope=0.0,sum=0.0,temp,test,tmplam;
	int i;
	check = false;
	
	for (i=0;i<n;i++)
		sum += p[i]*p[i];
		
	sum=sqrt(sum);
	if (sum > stpmax)
		for (i=0;i<n;i++)
			p[i] *= stpmax/sum;
	
	for (i=0;i<n;i++)
		slope += g[i]*p[i];
	
	if (slope >= 0.0)
		return false;
		
	test=0.0;
	for (i=0;i<n;i++)
	{
		temp=fabs(p[i])/mymax(fabs(xold[i]),1.0);
		if (temp > test) test=temp;
	}
	
	alamin=TOLX/test;
	alam=1.0;
	int niter = 0;
	int niter_max = 5000;
	while (niter++ < niter_max)
	{
		for (i=0;i<n;i++)
			x[i]=xold[i]+alam*p[i];
			
		f = fminsum<Real, F, n>( x, fvec, func );
		if ( (f) != (f) )
			return false;
			
		if (alam < alamin)
		{
			for (i=0;i<n;i++) x[i]=xold[i];
			check=true;
			return true;
		}
		else if (f <= fold+ALF*alam*slope)
		{
			return true;
		}
		else
		{
			if (alam == 1.0)
			{
				tmplam = -slope/(2.0*(f-fold-slope));
			}
			else
			{
				rhs1=f-fold-alam*slope;
				rhs2=f2-fold-alam2*slope;
				a=(rhs1/(alam*alam)-rhs2/(alam2*alam2))/(alam-alam2);
				b=(-alam2*rhs1/(alam*alam)+alam*rhs2/(alam2*alam2))/(alam-alam2);
				if (a == 0.0)
				{
					tmplam = -slope/(2.0*b);
				}
				else
				{
					disc=b*b-3.0*a*slope;
					if (disc < 0.0) tmplam=0.5*alam;
					else if (b <= 0.0) tmplam=(-b+sqrt(disc))/(3.0*a);
					else tmplam=-slope/(b+sqrt(disc));
				}

				if (tmplam>0.5*alam)
					tmplam=0.5*alam;
			}
		}

		alam2=alam;
		f2 = f;
		alam=mymax(tmplam,0.1*alam);
	}

	if (niter == niter_max) return false;

	return true;
}

#endif
