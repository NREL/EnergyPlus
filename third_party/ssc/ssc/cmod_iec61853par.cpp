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

#include "core.h"
#include "lib_iec61853.h"

static var_info vtab_iec61853[] = 
{	
/*   VARTYPE            DATATYPE         NAME                        LABEL                       UNITS     META                                             GROUP          REQUIRED_IF    CONSTRAINTS UI_HINTS*/
	{ SSC_INPUT,        SSC_MATRIX,      "input",                  "IEC-61853 matrix test data", "various",  "[IRR,TC,PMP,VMP,VOC,ISC]",                      "IEC61853",    "*",           "",         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "nser",                   "Number of cells in series",  "",         "",                                              "IEC61853",    "*",           "",         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "type",                   "Cell technology type",       "0..5",     "monoSi,multiSi/polySi,cdte,cis,cigs,amorphous", "IEC61853",    "*",           "",         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "verbose",                "Output solver messages",     "0/1",      "",                                              "IEC61853",    "*",           "",         "" },
																								 											                			   
	{ SSC_OUTPUT,       SSC_NUMBER,      "alphaIsc",               "SC temp coefficient @ STC",  "A/C",      "",                                              "IEC61853",    "*",           "",         "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "betaVoc",                "OC temp coefficient @ STC",  "V/C",      "",                                              "IEC61853",    "*",           "",         "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "gammaPmp",               "MP temp coefficient @ STC",  "%/C",      "",                                              "IEC61853",    "*",           "",         "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "n",                      "Diode factor",               "",         "",                                              "IEC61853",    "*",           "",         "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "Il",                     "Light current",              "A",        "",                                              "IEC61853",    "*",           "",         "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "Io",                     "Saturation current",         "A",        "",                                              "IEC61853",    "*",           "",         "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "C1",                     "Rsh fitting C1",             "",         "",                                              "IEC61853",    "*",           "",         "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "C2",                     "Rsh fitting C2",             "",         "",                                              "IEC61853",    "*",           "",         "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "C3",                     "Rsh fitting C3",             "",         "",                                              "IEC61853",    "*",           "",         "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "D1",                     "Rs fitting D1",              "",         "",                                              "IEC61853",    "*",           "",         "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "D2",                     "Rs fitting D2",              "",         "",                                              "IEC61853",    "*",           "",         "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "D3",                     "Rs fitting D3",              "",         "",                                              "IEC61853",    "*",           "",         "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "Egref",                  "Bandgap voltage",            "eV",       "",                                              "IEC61853",    "*",           "",         "" },

var_info_invalid };

class cm_iec61853par : public compute_module
{
private:
public:
	cm_iec61853par()
	{
		add_var_info( vtab_iec61853 );
	}

	class msg_handler : public Imessage_api
	{
		compute_module &cm;
	public:
		msg_handler(compute_module &_c) :cm(_c) { }
		
		virtual void Printf(const char *fmt, ...) {
			char buf[1024];
			va_list ap;
			va_start(ap, fmt);
#ifdef _MSC_VER
			_vsnprintf(buf, 1024, fmt, ap);
#else
			vsnprintf(buf,1024,fmt,ap);
#endif
			va_end(ap);
			cm.log( buf, SSC_NOTICE );
		}
		virtual void Outln(const char *msg ) {
			cm.log( msg, SSC_NOTICE );
		}
	};

	void exec( )
	{
		iec61853_module_t solver;
		msg_handler msgs( *this );
		solver._imsg = &msgs;

		util::matrix_t<double> input = as_matrix("input"), par;
		if ( input.ncols() != iec61853_module_t::COL_MAX )
			throw exec_error( "iec61853", "six data columns required for input matrix: IRR,TC,PMP,VMP,VOC,ISC");

		if (!solver.calculate( input, as_integer("nser"), as_integer("type"), par, as_boolean("verbose") ))
			throw exec_error( "iec61853", "failed to solve for parameters");

		assign("n", var_data((ssc_number_t)solver.n));
		assign("alphaIsc", var_data((ssc_number_t)solver.alphaIsc));
		assign("betaVoc", var_data((ssc_number_t)solver.betaVoc));
		assign( "gammaPmp", var_data((ssc_number_t)solver.gammaPmp) );
		assign( "Il", var_data((ssc_number_t)solver.Il) );
		assign( "Io", var_data((ssc_number_t)solver.Io) );
		assign( "C1", var_data((ssc_number_t)solver.C1) );
		assign( "C2", var_data((ssc_number_t)solver.C2) );
		assign( "C3", var_data((ssc_number_t)solver.C3) );
		assign( "D1", var_data((ssc_number_t)solver.D1) );
		assign( "D2", var_data((ssc_number_t)solver.D2) );
		assign( "D3", var_data((ssc_number_t)solver.D3) );
		assign( "Egref", var_data((ssc_number_t)solver.Egref) );

		ssc_number_t *output = allocate( "output", par.nrows(), par.ncols() );
		size_t c = 0;
		for( size_t i=0;i<par.nrows();i++ )
			for( size_t j=0;j<par.ncols();j++ )
				output[c++] = (ssc_number_t)par(i, j);
	}
};

DEFINE_MODULE_ENTRY( iec61853par, "Calculate 11-parameter single diode model parameters from IEC-61853 PV module test data.", 1 )


#include "../solarpilot/Toolbox.h"
#include "../tcs/interpolation_routines.h"

static var_info vtab_iec61853interp[] = 
{	
/*   VARTYPE            DATATYPE         NAME                        LABEL                       UNITS     META                                             GROUP          REQUIRED_IF    CONSTRAINTS UI_HINTS*/
	{ SSC_INPUT,        SSC_MATRIX,      "input",                  "IEC-61853 matrix test data", "various",  "[IRR,TC,PMP,VMP,VOC,ISC]",                      "IEC61853",    "*",           "",         "" },
	{ SSC_INPUT,        SSC_MATRIX,      "param",                  "Parameter solution matrix",  "",         "[IL,IO,RS,RSH,A]",                                              "IEC61853",    "*",           "",         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "I",                       "Irradiance",                    "W/m2",      "",                    "Single Diode Model",      "*",                       "",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "T",                       "Temperature",                   "C",         "",                    "Single Diode Model",      "*",                       "",              "" },
																								 											                			   
	{ SSC_OUTPUT,       SSC_NUMBER,      "a",                       "Modified nonideality factor",    "1/V",    "",                      "Single Diode Model",      "*",                        "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "Il",                      "Light current",                  "A",      "",                      "Single Diode Model",      "*",                        "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "Io",                      "Saturation current",             "A",      "",                      "Single Diode Model",      "*",                        "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "Rs",                      "Series resistance",              "ohm",    "",                      "Single Diode Model",      "*",                        "",                              "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "Rsh",                     "Shunt resistance",               "ohm",    "",                      "Single Diode Model",      "*",                        "",                              "" },

var_info_invalid };

enum { IRR, TC, PMP, VMP, VOC, ISC, DATACOLS };
enum { IL, IO, RS, RSH, A, PARCOLS };
static const char *parnames[] = { "IL", "IO", "RS", "RSH", "A" };

class cm_iec61853interp : public compute_module
{
private:
public:
	cm_iec61853interp()
	{
		add_var_info( vtab_iec61853interp );
	}


	double interpolate( util::matrix_t<double> &data,
		util::matrix_t<double>  &par,
		double I, double T, int idx, bool quiet )
	{
		MatDoub tempirr;
		std::vector<double> parvals;
		std::vector<sp_point> pts, hull;

		double maxz = -1e99;
		double tmin = 1e99;
		double tmax = -1e99;
		double imin = 1e99;
		double imax = -1e99;
		double dist = 1e99;
		int idist = -1;
		for( size_t i=0;i<data.nrows();i++ )
		{
			double z = par(i,idx);
			if ( !std::isfinite( z ) )
				continue;

			double temp = data(i,TC);//x value
			double irr = data(i,IRR);//y value

			if ( temp < tmin ) tmin = temp;
			if ( temp > tmax ) tmax = temp;
			if ( irr < imin ) imin = irr;
			if ( irr > imax ) imax = irr;

			double d = sqrt( (irr-I)*(irr-I) + (temp-T)*(temp-T) );
			if ( d < dist )
			{
				dist = d;
				idist = (int)i;
			}
			
			std::vector<double> it(2,0.0);
			it[0] = temp; it[1] = irr;
			tempirr.push_back( it );

			parvals.push_back( z );

			if ( z > maxz ) maxz = z;

			pts.push_back( sp_point( temp, irr, z ) );
		}

		Toolbox::convex_hull( pts, hull );
		if ( Toolbox::pointInPolygon( hull, sp_point(T, I, 0.0) ) )
		{
			// scale values based on max - helps GM interp routine
			for( size_t i=0;i<parvals.size();i++)
				parvals[i] /= maxz;

			Powvargram vgram( tempirr, parvals, 1.75, 0. );
			GaussMarkov gm( tempirr, parvals, vgram );

			// test the fit against the data
			double err_fit = 0.;
			for( size_t i=0;i<parvals.size();i++ )
			{
				double zref = parvals[i];
				double zfit = gm.interp( tempirr[i] );
				double dz = zref - zfit;
				err_fit += dz*dz;
			}
			err_fit = sqrt(err_fit);
			if ( err_fit > 0.01 )
			{
				log(	util::format("interpolation function for iec61853 parameter '%s' at I=%lg T=%lg is poor: %lg RMS",
							parnames[idx], I, T, err_fit ),

					SSC_WARNING );
			}

			std::vector<double> q(2,0.0);
				q[0] = T;
				q[1] = I;
			
			// now interpolate and return the value
			return gm.interp( q ) * maxz;
		}
		else
		{
			// if we're pretty close, return the nearest known value
			if ( dist < 30. )
			{
				
				if ( !quiet )
					log( util::format("query point (%lg, %lg) is outside convex hull of data but close... returning nearest value from data table at (%lg, %lg)=%lg",
						T, I, data(idist,TC), data(idist,IRR), par(idist,idx) ),

						SSC_WARNING );

				return par(idist,idx);
			}

				

			// fall back to the 5 parameter model's auxiliary equations 
			// to estimate the parameter values outside the convex hull

			int idx_stc = -1;
			for( size_t i=0;i<data.nrows();i++)
				if ( data(i,IRR) == 1000.0
					&& data(i,TC) == 25.0 )
					idx_stc = (int)i;

			if ( idx_stc < 0 )
				throw general_error("STC conditions required to be supplied in the temperature/irradiance data");



			double value = par(idist,idx);;
			if ( idx == A )
			{
				double a_nearest = par( idist, A );
				double T_nearest = data( idist, TC );
				double a_est = a_nearest * T/T_nearest;
				value = a_est;
			}
			else if ( idx == IL )
			{
				double IL_nearest = par( idist, IL );
				double I_nearest = data(idist, IRR );
				double IL_est = IL_nearest * I/I_nearest;
				value = IL_est;
			}/*
			else if ( idx == IO )
			{
#define Tc_ref 298.15
#define Eg_ref 1.12
#define KB 8.618e-5

				double IO_stc = par(idx_stc,IO);
				double TK = T+273.15;
				double EG = Eg_ref * (1-0.0002677*(TK-Tc_ref));
				double IO_oper =  IO_stc * pow(TK/Tc_ref, 3) * exp( 1/KB*(Eg_ref/Tc_ref - EG/TK) );
				value = IO_oper;	
			}*/
			else if ( idx == RSH )
			{
				double RSH_nearest = par( idist, RSH );
				double I_nearest = data(idist, IRR );
				double RSH_est = RSH_nearest * I_nearest/I;
				value = RSH_est;
			}
			
			if ( !quiet )
				log( util::format("query point (%lg, %lg) is too far out of convex hull of data (dist=%lg)... estimating value from 5 parameter modele at (%lg, %lg)=%lg",
					T, I, dist, data(idist,TC), data(idist,IRR), value ),

					SSC_WARNING );

			return value;
		}
	}

	void exec()
	{
		double I = as_double("I");
		double T = as_double("T");
		util::matrix_t<double> data = as_matrix("input");
		util::matrix_t<double> par = as_matrix("param");

		if ( data.ncols() != DATACOLS )
			throw general_error( util::format("input matrix must have 6 columns (Irr, Tc, Pmp, Vmp, Voc, Isc), but is %d x %d", 
				(int)data.nrows(), (int)data.ncols() ) );

		if ( par.ncols() != PARCOLS )
			throw general_error( util::format("parameter matrix must have 5 columns (Il, Io, Rs, Rsh, a), but is %d x %d",
				(int)par.nrows(), (int)par.ncols() ) );

		if ( par.nrows() != data.nrows() || data.nrows() < 3 )
			throw general_error( "input and parameter matrices must have same number of rows, and at least 3" );

		bool quiet = false;
		if ( is_assigned( "quiet" ) )
			quiet = true;

		assign( "a", var_data((ssc_number_t) interpolate( data, par, I, T, A, quiet ) ) );
		assign("Il", var_data((ssc_number_t)interpolate(data, par, I, T, IL, quiet)));
		assign("Io", var_data((ssc_number_t)interpolate(data, par, I, T, IO, quiet)));
		assign("Rs", var_data((ssc_number_t)interpolate(data, par, I, T, RS, quiet)));
		assign("Rsh", var_data((ssc_number_t)interpolate(data, par, I, T, RSH, quiet)));

	}
};
DEFINE_MODULE_ENTRY( iec61853interp, "Determine single diode model parameters from IEC 61853 solution matrix at a given temperature and irradiance.", 1 )
