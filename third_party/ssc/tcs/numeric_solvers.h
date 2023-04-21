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

#ifndef __NUMERIC_SOLVERS_
#define __NUMERIC_SOLVERS_

#include <vector>
#include <limits>

class C_monotonic_equation
{
public:
	C_monotonic_equation()
	{
	}
	~C_monotonic_equation()
	{
	}

	virtual int operator()(double x, double *y) = 0;
};

class C_import_mono_eq : public C_monotonic_equation
{

private:
	int(*mf_monotonic_function)(double x, double *y);

public:

	
	//C_import_mono_eq()
	C_import_mono_eq(int(*f)(double x, double *y))
	{
		mf_monotonic_function = f;
	}

	~C_import_mono_eq(){}

	virtual int operator()(double x, double *y);
};


class C_monotonic_eq_solver
{
public:
	struct S_eq_chars
	{
		double x;		//[...] Input value
		double y;		//[...] Calculated output value
		int err_code;	//[-] Integer error message

		S_eq_chars()
		{
			x = y = std::numeric_limits<double>::quiet_NaN();

			err_code = 0;
		}
	};

	struct S_xy_pair
	{
		double x;	//[...] Independent variable
		double y;	//[...] Dependent variable

		S_xy_pair()
		{
			x = y = std::numeric_limits<double>::quiet_NaN();
		}
	};

private:

	C_monotonic_equation &mf_mono_eq;

	// Values set in solver
	bool m_is_pos_bound;
	bool m_is_neg_bound;
	bool m_is_pos_error;
	bool m_is_neg_error;
	bool m_is_pos_error_prev;
	bool m_is_neg_error_prev;

	double m_x_guess;
	double m_x_neg_err;
	double m_x_pos_err;
	double m_y_err_pos;
	double m_y_err_neg;

	double m_y_err;
	int m_iter;

    double m_E_slope;

	double check_against_limits(double x);

	double calc_x_intercept(double x1, double y1, double x2, double y2);

	int solver_core(double x_guess_1, double y1, double x_guess_2, double y2, double y_target,
		double &x_solved, double &tol_solved, int &iter_solved);

	// Save x, y, and int_return of for each mono_eq call
	std::vector<S_eq_chars> ms_eq_call_tracker;

	S_eq_chars ms_eq_tracker_temp;

protected:
	double m_func_x_lower;		// Lower limit of independent variable
	double m_func_x_upper;		// Upper limit of independent variable

	double m_tol;			// Convergence error
	int m_iter_max;			// Maximum number of iterations allowed

	bool m_is_err_rel;		// Should error be calculated relative (true) or absolute (false)?

public:

	enum solver_exit_modes
	{
		REL_TOL_WITH_0_TARGET,
		EQUAL_GUESS_VALUES,
		NO_SOLUTION,

		CONVERGED,

		SLOPE_POS_NO_NEG_ERR,
		SLOPE_NEG_NO_NEG_ERR,

		SLOPE_POS_NO_POS_ERR,
		SLOPE_NEG_NO_POS_ERR,

		SLOPE_POS_BOTH_ERRS,
		SLOPE_NEG_BOTH_ERRS,

		MAX_ITER_SLOPE_POS_NO_NEG_ERR,
		MAX_ITER_SLOPE_NEG_NO_NEG_ERR,

		MAX_ITER_SLOPE_POS_NO_POS_ERR,
		MAX_ITER_SLOPE_NEG_NO_POS_ERR,

		MAX_ITER_SLOPE_POS_BOTH_ERRS,
		MAX_ITER_SLOPE_NEG_BOTH_ERRS,
	};
	
	C_monotonic_eq_solver(C_monotonic_equation & f);

	~C_monotonic_eq_solver(){}

	virtual void settings(double tol, int iter_limit, double x_lower, double x_upper, bool is_err_rel);

	int solve(double x_guess_1, double x_guess_2, double y_target,
		double &x_solved, double &tol_solved, int &iter_solved);
		
    int solve(S_xy_pair solved_pair_1, double x_guess_2, double y_target,
        double &x_solved, double &tol_solved, int &iter_solved);

    int solve(S_xy_pair solved_pair_1, S_xy_pair solved_pair_2, double y_target,
		double &x_solved, double &tol_solved, int &iter_solved);

    int solve(std::vector<double> x_solved_vector, std::vector<double> y_solved_vector, double y_target,
        double &x_solved, double &tol_solved, int &iter_solved);

    bool is_last_x_best(double & x_at_lowest, double y_target);

    bool get_min_abs_diff_no_err(C_monotonic_eq_solver::S_eq_chars & s_eq_chars_min_abs_diff,
        double y_target);

	int call_mono_eq(double x, double *y);

    double call_mono_eq_calc_y_err(double x, double y_target);

	bool did_solver_find_positive_error(int solver_exit_mode);

	bool did_solver_find_negative_error(int solver_exit_mode);

    double get_E_slope();

	const S_eq_chars get_last_mono_eq_call();

	const std::vector<S_eq_chars> *get_solver_call_history()
	{
		return &ms_eq_call_tracker;
	}

	int test_member_function(double x, double *y);
};


//class monotonic_solver
//{
//public:
//
//	// f(x) = 0
//	static int solve(monotonic_equation &f,
//		double *result, double xmin, double xmax)
//	{
//		double x = xmin;
//		while( x <= xmax )
//		{
//			double y;
//			int err = f(x, &y);
//			if( err != 0 )
//				return err;
//
//			if( y > 0 )
//			{
//				*result = x;
//				return true;
//			}
//			x++;
//		}
//		return false;
//	}
//};
//
//class HX_object// : public monotonic_equation
//{
//	double call_equation(double x)
//	{
//		return x;
//	}
//
//public:
//	// ty's CSP code
//	class yequalsZxM1 : public monotonic_equation
//	{
//	private:
//		HX_object *m_z;
//	public:
//		yequalsZxM1(HX_object *z)
//		{
//			m_z = z;
//		}
//		virtual ~yequalsZxM1() { }
//		virtual int operator()(double x, double *y)
//		{
//			*y = m_z->call_equation(x);
//			return 0;
//		}
//	};
//
//	//virtual int operator()(double x, double *y) const {
//	//	*y = x*103894;
//	//	return 0;
//	//}
//
//
//	// some code ...
//	int myfunction(double &result)
//	{
//		HX_object::yequalsZxM1 eqn1(this);
//		
//		return monotonic_solver::solve(eqn1, &result, -10, 10);
//		//if( !monotonic_solver::solve(eqn1, &result, -10, 10) )
//		//{
//		//	printf("error!");
//		//}
//
//		// monotonic_solver::solve( *this, &result, -10, 10 );
//	}
//
//};



#endif