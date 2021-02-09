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

#ifndef _FLUX_H_
#define _FLUX_H_ 1

#include <vector>
#include <time.h>


#include <random>
#include "Toolbox.h"
#include "definitions.h"

/*Notes:
* For Hermite polynomials, refer to DELSOL code, line 1471
* The flux distribution is calculated to include the moments of sunshape, heliostat error, 
and heliostat shape using the MOMENT subroutine, line 8244

*/
class FluxSurface;
class Heliostat;
class Receiver;
class SolarField;
class Ambient;

typedef std::vector<Heliostat*> Hvector;

class Random
{
	int rmax;
public:
	Random();
	double uniform();
	double triangular();
	double normal(double stddev=1.0);
	double sign();
	int integer(int min=0, int max=RAND_MAX);
};

class Flux
 {
	matrix_t<double> 
		_hermitePoly,		//Contains a vector of values of the evaluated hermite polynomial coefficients
		_fact_odds,			//Contains the factorial values for the odd terms in the hermite expansion
		_fact_d,			//Contains factorial values
		_binomials,			//Contains the binomial coefficients
		_binomials_hxn,		//Contains binomial coefficients for the HXN array
		_mu_SN;		//Normalized moments of sunshape
	
	block_t<double>
		_mu_GN;		//Normalized moments of the error distribution
		
	int _n_order;	//Order of the Hermite expansion
	int _n_terms;	//Number of terms (n_order + 1), to include constant term
	
	int *_jmin;
	int *_jmax;

	double pi,Pi;
	
	Random *_random;

	//coefficient weighting arrays for hermite integral
	double _ci[4];
	double _ag[16];
	double _xg[16];

 public:


	 Flux();
	~Flux();
	
	//Copy constructor
	Flux(Flux &f);

	void Setup();

	//Methods defined here
	Random *getRandomObject();

	//-------------DELSOL3 methods------------------------
	void factOdds(); //Method to calculate factorial component of Hermite expansion terms
	
	//Frequently reused bounds calculations
	int JMN(int i);
	int JMX(int i);
	int IMN(int i);

	void Binomials();	//Method to calculate binomial coefficients

	void Binomials_hxn();

	matrix_t<double> hermitePoly( double x );	//This will return a vector<double> of hermite coefficients

	//moments of sunshape distribution. If user-defined, also requires specification of the _user_sun vector
	void hermiteSunCoefs(var_map &V, matrix_t<double> &mSun);

	//moments of the error distribution
	void hermiteErrDistCoefs(block_t<double> &errDM);

	//moments of the Mirror shape distribution
	void hermiteMirrorCoefs(Heliostat &H, double tht);

	//Evaluate the hermite coefficients in the array _mu_F
	double hermiteIntEval(Heliostat &H, Receiver *Rec);

	//The Hermite integral - determine the flux intercept
	void hermiteIntegral(double G[5], double F[5], double X[2], double A[2], double TA[2], double WT, matrix_t<double> &hspill);

	//Detailed intercept calculation
	void hermiteIntegralSetup(double A[2], Heliostat &H, matrix_t<double> &hspill, Receiver *Rec);

	//  >>>> This is the main method for intercept calculation, this calls other methods <<<<<<
	//Returns the total image intercept.
	//This method to calculate and convolve all moments of error distribution in the image plane
	double imagePlaneIntercept(var_map &V, Heliostat &H, Receiver *Rec, Vect *Sun);

	//An algorithm to initialize the polynomial coefficients
	void initHermiteCoefs(var_map &V);

	//A method to calculate the flux density given a map of values and a solar field
	void fluxDensity(simulation_info *siminfo, FluxSurface &flux_surface, Hvector &helios, bool clear_grid = true, bool norm_grid = true, bool show_progress=false);

	double hermiteFluxEval(Heliostat *H, double xs, double ys);

	//-------------End DELSOL3 methods--------------------

	void calcBestReceiverTarget(Heliostat *H, std::vector<Receiver*> *Recs, double tht, int &rec_index, Vect *rtoh=0);

	void simpleAimPoint(sp_point *Aim, sp_point *AimF, Heliostat &H, SolarField &SF);
	void simpleAimPoint(Heliostat &H, SolarField &SF);	//Method for quick calculation of the aim point to maximize intercept

	void sigmaAimPoint(Heliostat &H, SolarField &SF, double args[]);

	void probabilityShiftAimPoint(Heliostat &H, SolarField &SF, double args[]);

	void imageSizeAimPoint(Heliostat &H, SolarField &SF, double args[], bool islast);

    void frozenAimPoint(Heliostat &H, double tht, double args[]);

    void keepExistingAimPoint(Heliostat &H, SolarField &SF, double args[]);

    void zenithAimPoint(Heliostat &H, Vect &Sun);
 } ;

#endif
