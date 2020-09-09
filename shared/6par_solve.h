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

#ifndef __6_PAR_SOLVE_H__
#define __6_PAR_SOLVE_H__

#include "6par_gamma.h"
#include "6par_newton.h"
#include "lib_util.h"

class notification_interface
{
public:
	virtual bool notify( int iter, double x[], double resid[], const int n ) = 0;
};

template<typename Real>
bool solve6par_callback( int iter, Real x[], Real resid[], const int n, void *data)
{
	if ( n != 6 ) return false;

	if ( notification_interface *nif = static_cast<notification_interface*>(data) )
	{
		double xd[6];
		double rd[6];

		for (int i=0;i<6;i++)
		{
			xd[i] = (double)( x[i] );
			rd[i] = (double)( resid[i] );
		}

		return nif->notify( iter, xd, rd, 6 );	
	}
	else
		return true;
}

template< typename Real >
class __Module6ParNonlinear
{
private:
	notification_interface *m_notifyInterface;
	Real Vmp, Imp, Voc, Isc, bVoc, aIsc, gPmp, Egref, Tref;

public:
	
	__Module6ParNonlinear( notification_interface *nif,
		double _Vmp, double _Imp, double _Voc, double _Isc,
		double _bVoc, double _aIsc, double _gPmp, double _Egref, double _Tref )
		: m_notifyInterface(nif),
			Vmp(_Vmp), Imp(_Imp), 
			Voc(_Voc), Isc(_Isc),
			bVoc(_bVoc), aIsc(_aIsc), gPmp(_gPmp),
			Egref(_Egref), Tref(_Tref)
	{
	}

	void operator() ( const Real x[6], Real f[6] )
	{
		Real a = x[0];
		Real Il = x[1];
		Real Io = x[2];
		Real Rs = x[3];
		Real Rsh = x[4];
		Real Adj = x[5];
		
		f[0] = Il - Io*( exp( Isc*Rs / a ) - 1 ) - Isc*Rs/Rsh - Isc;
		f[1] = Io*( exp( Voc/a ) - 1 ) + Voc/Rsh -Il;
		f[2] = Il - Io*( exp( (Vmp + Imp*Rs) / a ) - 1 ) - (Vmp + Imp*Rs)/Rsh - Imp;

		f[3] = Imp - Vmp*(
			( Io/a*exp( (Vmp + Imp*Rs)/a ) + 1/Rsh )
		 /( 1 + Io*Rs/a*exp( (Vmp + Imp*Rs)/a ) + Rs/Rsh ) ); 

		const Real dT = 5;

		const Real aT = a*(Tref+dT)/Tref;
		const Real VocT = bVoc*(1+Adj/100.0)*dT + Voc;
		const Real Eg = (1-0.0002677*dT)*Egref;
		const Real IoT = Io*pow( (Tref+dT)/Tref, 3)*exp( 11600 * (Egref/Tref - Eg/(Tref+dT)));

		f[4] = Il+aIsc*(1-Adj/100)*dT - IoT*(exp( VocT/aT ) - 1 ) - VocT/Rsh;
		
		
		Real gamma = 0;
		mod6par_gamma_approx<Real>( &gamma, Io, Il, a, aIsc, Adj, Vmp, Imp, Rs, Rsh, Egref, Tref );
		
		f[5] = gamma - gPmp;
	}

	bool exec(Real &_a, Real &_Il, Real &_Io, Real &_Rs, Real &_Rsh, Real &_Adj, 
		int max_iter, double tol, notification_interface *nif )
	{	
		Real x[6], resid[6];
		
		x[0] = _a;
		x[1] = _Il;
		x[2] = _Io;
		x[3] = _Rs;
		x[4] = _Rsh;
		x[5] = _Adj;
		
		bool check = false;
		int niter = newton<Real, __Module6ParNonlinear, 6>( x, resid, check, *this, 
			max_iter, Real(tol), Real(tol), 0.7,
			solve6par_callback<Real>, nif );
		
		if ( niter < 0 || check ) return false;
		
		_a = x[0];
		_Il = x[1];
		_Io = x[2];
		_Rs = x[3];
		_Rsh = x[4];
		_Adj = x[5];
		
		return true;
	}
};

class module6par
{
public:
	enum { monoSi, multiSi, CdTe, CIS, CIGS, Amorphous };
	
	module6par() 
		: Type(monoSi), Vmp(0), Imp(0), Voc(0), Isc(0), bVoc(0), aIsc(0), gPmp(0), Nser(0), Tref(0),
			a(0.0), Il(0.0), Io(0.0), Rs(0.0), Rsh(0.0), Adj(0.0) {  }

	module6par( int _type, double _vmp, double _imp, double _voc, double _isc, double _bvoc, double _aisc, double _gpmp, int _nser, double _Tref )
		: Type(_type), Vmp(_vmp), Imp(_imp), Voc(_voc), Isc(_isc), bVoc(_bvoc), aIsc(_aisc), gPmp(_gpmp), Nser(_nser), Tref(_Tref),
			a(0.0), Il(0.0), Io(0.0), Rs(0.0), Rsh(0.0), Adj(0.0) {  }

	std::string Name;
	std::string Tech;

	int Type;
	double Vmp, Imp;
	double Voc, Isc;
	double bVoc, aIsc, gPmp;
	int Nser;

	double Tref;
	
	double a, Il, Io, Rs, Rsh, Adj;

	double bandgap()
	{
		return 1.121; // use as reference value for all cell types

		/*
		switch( Type )
		{
		
		case Amorphous: // http://en.wikipedia.org/wiki/Solar_cell
			return 1.7;

		case CdTe: // http://en.wikipedia.org/wiki/Band_gap#Photovoltaic_cells
			return 1.49;

		case CIS:
			return 1.05; // Numerical Model of the Copper-Indium-Selenium(CIS) based Solar cell Performance by AMPS-1D (Amin, Tang, Sopian), IEEE

		case CIGS:
			return 1.4; // Ref. as above, and higher than CIS, b/c of addition of gallium http://solar.calfinder.com/library/solar-electricity/cells/cell-materials/cis

			
		case monoSi:
		case multiSi:
		default:
			return 1.121;
		}*/
	}

	void guess()
	{
		// guess initial conditions (take into account module technology -based on heuristics)
		
		/*
		// -------- original regressions - based on CEC database -----------
		switch( Type )
		{
		case Amorphous: a = 0.0353 * Nser + 0.5222; break;
		case CdTe: a = 0.0109 * Nser + 1.4112; break;
		case CIGS: a = 0.019 * Nser + 0.3088; break;
		case CIS: a = 0.0224 * Nser - 0.11; break;
		case monoSi: a = 0.0272 * Nser - 0.0142; break;
		case multiSi:
		default:
			a = 0.0252 * Nser;
		}		
		*/

		
		// -------- regressions based on bootstrapping iteration 1 -----------
		switch( Type )
		{
		case Amorphous: a = 0.029 * Nser + 0.5264; break;
		case CdTe: a = 0.012 * Nser + 1.3565; break;
		case CIGS: a = 0.018 * Nser + 0.3277; break;
		case CIS: a = 0.021 * Nser + 0.0897; break;
		case monoSi: a = 0.027 * Nser - 0.0172; break;
		case multiSi:
		default:
			a = 0.0263 * Nser + 0.0212;
		}
				
		if (a < 0.1) a = 0.1;
		if (a > 10) a = 10;
			
		Il = Isc;
			
		Io = Isc*exp( -Voc/a );

		// put a "reasonable" limit on the initial guess value of Io
		if (Io > 1e-9) Io = 1e-9;
		if (Io < 1e-15) Io = 1e-15;
				
		//if ( Type == monoSi || Type == multiSi )
			//Rs = ( a * log( (Isc-Imp)/Io ) - Vmp ) / Imp;
		//else
		//  Rs = 0.3*(Voc - Vmp)/Imp;
		
		double Rs_scale, Rsh_scale;
		switch( Type )
		{
		case Amorphous:
			Rs_scale = 0.59;
			Rsh_scale = 0.922;
			break;
		case CdTe:
			Rs_scale = 0.46;
			Rsh_scale = 1.11;
			break;
		case CIGS:
			Rs_scale = 0.55;
			Rsh_scale = 1.22;
			break;
		case CIS:
			Rs_scale = 0.61;
			Rsh_scale = 1.07;
			break;
		case monoSi:
			Rs_scale = 0.32;
			Rsh_scale = 4.92;
			break;
		case multiSi:
		default:
			Rs_scale = 0.34;
			Rsh_scale = 5.36;
			break;
		}

		
		//if ( Type == monoSi || Type == multiSi )
		//	Rsh = 100;// * Rs;
		//else
		//  Rsh = Voc/(0.3*(Isc-Imp));

		//Rs_scale = 0.3;
		//Rsh_scale = 1/0.3;

		Rs = Rs_scale * (Voc - Vmp)/Imp;

		if (Rs < 0.02) Rs = 0.02;
		if (Rs > 60) Rs = 60;
		
		Rsh = Rsh_scale * Voc/(Isc-Imp);

		Adj = 0.0;


		
		
	}

	void guess_ees()
	{
		// initial conditions specified in EES solver
		a = 2 * Nser * 0.025;
		Io = Isc * exp(-Voc/a);
		Il = Isc;
		Rs = ( a * log( (Isc-Imp)/Io ) - Vmp ) / Imp;
		Rsh = 100;
	}

	int sanity()
	{
		// ensure values are in "reasonable" ranges
		if ( a < 0.05 || a > 15.0 ) return -1;
		if ( Il < 0.5 || Il > 15.0 ) return -2;
		if ( Io < 1e-16 || Io > 1e-7 ) return -3;
		if ( Rs < 0.001 || Rs > 75.0 ) return -4;
		if ( Rsh < 1.0 || Rsh > 100001.0 ) return -5;
		if ( Adj < -100.0 || Adj > 100.0 ) return -6;
		if ( Imp >= Isc ) return -7;
				
		double V, I, P;

		// make sure Pmp on curve is within 1 % of specified Pmp
		V = Vmp;
		I = module6par::current(V, Il, Io, Rs, a, Rsh, Imp );
		P = V * I;
		double Pmp = Vmp * Imp;
		if ( fabs( (P-Pmp)/Pmp ) > 0.015 )
			return -33;
				
		// make sure I @ open circut is basically zero (less than 1% of Imp)
		V = Voc;
		I = module6par::current(V, Il, Io, Rs, a, Rsh, Imp );
		if ( fabs(I) > 0.015 * Imp )
			return -44;

		// derivative of I/V curve should always be negative (because of numerical issues, could be 0)
		double slope = max_slope( 0.015 * Voc, 0.98 * Voc );
		if ( slope > 0 ) return -55;

				
		return 0;
	}

	double max_slope( double Vstart, double Vend )
	{
		if (Vend <= Vstart) Vend *= 1.01;

		double count = 0;
		const int numPoints = 100;
		double V, I;
		
		double deriv = 0;

		double Ilast = module6par::current(Vstart, Il, Io, Rs, a, Rsh, Imp);

		double dV = (Vend-Vstart) / numPoints;

		V = Vstart;
		while (V <= Vend)
		{
			I = module6par::current(V, Il, Io, Rs, a, Rsh, Imp );
			if (V > Voc) I = 0;
			double d = ( I - Ilast ) / dV;
			if (d > deriv) deriv = d;
			Ilast = I;
			V += dV;
			count++;
		}

		return deriv ;
	}
			
	static double current(double Vmodule, double IL_ref, 
								  double IO_ref, double RS, 
								  double A_ref, double RSH_ref, 
								  double I_mp_ref)
	{
			/*
			 5/21/08 - based on email from Nate 4/3/08

			  V,        !load voltage [volts]
			IO_REF,   !reverse saturation current at reference conditions [A]
			A_REF,    !modified ideality factor at reference conditions
			IL_REF,   !photocurrent at reference conditions [A]
			IO_REF =  reverse saturation current at reference conditions [A]
			RS =        !series resistance (constant)
			RSH_REF =  !shunt resistance at reference conditions
			IMR =   reference current at max. power [A]

				   FUNCTION CURRENT5194(V,IMR,NSER,NPRL)
			C     Iterative solution for current as a function of voltage using
			C     equations from the five-parameter model.  Newton's method is used
			C     to converge on a value.  Max power at reference conditions is initial
			C     guess.
				  IMPLICIT NONE
				  IOLD = 0.0
				  V_MODULE    = V/NSer

			C**** first guess is max.power point current

				  INEW = IMR
				  DO WHILE (DABS(INEW-IOLD).GT.1.E-4)
					IOLD   = INEW
					F      = IL_REF-IOLD-IO_REF*(DEXP((V_MODULE+IOLD*RS)/A_REF)-1.0)-
				 &               (V_MODULE+IOLD*RS)/RSH_REF
					FPRIME = -1.-IO_REF*(RS/A)*DEXP((V_MODULE+IOLD*RS)/A_REF)-(RS/RSH_REF)
					INEW   = MAX(0.d0,(IOLD-(F/FPRIME)))
				  END DO
				  I           = INEW               !CURRENT FOR ONE MODULE

				  END FUNCTION CURRENT5194

			*/
	  double F=0, Fprime=0;
	  double Iold = 0.0;
	  double Inew = I_mp_ref;
	  int iter = 0;

	  while (fabs(Inew - Iold) > 1.0e-5)
	  {
		Iold = Inew;

		F =  IL_ref - Iold - IO_ref *
			  (exp((Vmodule + Iold * RS) / A_ref) - 1.0) -
			  (Vmodule + Iold * RS) / RSH_ref;

		Fprime = -1.0 - IO_ref * (RS / A_ref) *
				  exp((Vmodule + Iold * RS) / A_ref) -
				  (RS / RSH_ref);

		Inew = MAX( double(0.0),(Iold-(F/Fprime)));

		if (++iter > 500) break;
	  }
	  return Inew;
	}

	template< typename Real >
	int solve(	int max_iter, double tol,
		notification_interface *nif = 0 )
	{

		__Module6ParNonlinear<Real> solver( nif,
			Vmp, Imp, Voc, Isc,
			bVoc, aIsc, gPmp, bandgap(), Tref );		

		Real _a = Real(a);
		Real _Il = Real(Il);
		Real _Io = Real(Io);
		Real _Rs = Real(Rs);
		Real _Rsh = Real(Rsh);
		Real _Adj = Real(Adj);

		bool ok = solver.exec( _a, _Il, _Io, _Rs, _Rsh, _Adj, max_iter, tol, nif );
		
		a = (double)(_a);
		Il = (double)(_Il);
		Io = (double)(_Io);
		Rs = (double)(_Rs);
		Rsh = (double)(_Rsh);
		Adj = (double)(_Adj);

		int err = sanity();

		if ( err == 0 && !ok ) return -99;
		else return err;

	}

	template< typename Real >
	int solve_with_sanity_and_heuristics( int max_iter, double tol,
		notification_interface *nif = 0 )
	{
		guess();

		int err = solve<Real>( max_iter, tol, nif );
		
		
		if ( err < 0 && (Type == Amorphous || Type == CdTe) )
		{
			// attempt decreasing 'a' and solving
			int downattempt = 0;
			while( err < 0 && ++downattempt <= 6 )
			{
				guess();
				a /= (1 + downattempt*0.2); // divide down by 1.2, 1.4, 1.6, 1.8, 2.0, 2.2
				if( downattempt > 4 ) Io /= 100;
				err = solve<Real>( max_iter, tol, nif );
			}

			// attempt increasing 'a' and solving
			int upattempt = 0;
			while( err < 0 && ++upattempt <= 6 )
			{
				guess();
				a *= (1 + upattempt*0.2); // multiply up by 1.2, 1.4, 1.6, 1.8, 2.0, 2.2
				if( upattempt > 4 ) Io /= 100;
				err = solve<Real>( max_iter, tol, nif );
			}
		}
		else if ( err < 0 && Type == multiSi )
		{
			guess();
			Io /= 100;
			Rsh /= 2;
			err = solve<Real>( max_iter, tol, nif );
		}
		
		double Isc_save = Isc;
		int nattempt = 0;
		while ( err < 0 && nattempt < 5 )
		{
				Isc *= 1.01;
				guess();
				err = solve<Real>( max_iter, tol, nif );
				nattempt++;
		}
		
		Isc = Isc_save;
		
		
		return err;
	}

	
};

#endif
