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

#include <numeric>
#include <limits>
#include <math.h>

#include "numeric_solvers.h"
#include "csp_solver_util.h"

#include <algorithm>
#include <cmath>
#include <limits>

int C_import_mono_eq::operator()(double x, double *y)
{
	return mf_monotonic_function(x, y);
}

C_monotonic_eq_solver::C_monotonic_eq_solver(C_monotonic_equation & f): mf_mono_eq(f)
{
	m_x_guess = m_x_neg_err = m_x_pos_err = m_y_err_pos = m_y_err_neg =
		m_y_err = m_E_slope =
		m_func_x_lower = m_func_x_upper = std::numeric_limits<double>::quiet_NaN();

	m_is_pos_bound = m_is_neg_bound =
		m_is_pos_error = m_is_neg_error =
		m_is_pos_error_prev = m_is_neg_error_prev =
		false;

	m_iter = -1;

	// Set default settings:
	m_tol = 0.001;
	m_is_err_rel = true;
	m_iter_max = 50;
}

void C_monotonic_eq_solver::settings( double tol, int iter_limit, double x_lower, double x_upper, bool is_err_rel)
{
	m_tol = tol;
	m_func_x_lower = x_lower;
	m_func_x_upper = x_upper;

	m_is_err_rel = is_err_rel;

	m_iter_max = std::max(1, iter_limit);
}

double C_monotonic_eq_solver::check_against_limits(double x)
{
	if( !std::isfinite(m_func_x_lower) && !std::isfinite(m_func_x_upper) )
	{
		return x;
	}
	else if ( !std::isfinite(m_func_x_lower) )
	{
		return std::min(m_func_x_upper, x);
	}
	else if ( !std::isfinite(m_func_x_upper) )
	{
		return std::max(m_func_x_lower, x);
	}
	else
	{
		return std::min(m_func_x_upper, std::max(x, m_func_x_lower));
	}
}

double C_monotonic_eq_solver::calc_x_intercept(double x1, double y1, double x2, double y2)
{
	return (x2 - x1) / (y2 - y1)*(-y1) + x1;
}

int C_monotonic_eq_solver::solve(double x_guess_1, double x_guess_2, double y_target,
	double &x_solved, double &tol_solved, int &iter_solved)
{
	// Set / reset vector that tracks calls to equation
	ms_eq_call_tracker.resize(0);
	ms_eq_call_tracker.reserve(m_iter_max);

	// Check that x guesses fall with bounds (set during initialization)
	x_guess_1 = check_against_limits(x_guess_1);
	x_guess_2 = check_against_limits(x_guess_2);

	// Call function with x guesses
	double y1, y2;
	if( call_mono_eq(x_guess_1, &y1) != 0 )
	{
		y1 = std::numeric_limits<double>::quiet_NaN();
	}
	// Check that guesses are different
	if (x_guess_1 != x_guess_2)
	{
		if (call_mono_eq(x_guess_2, &y2) != 0)
		{
			y2 = std::numeric_limits<double>::quiet_NaN();
		}
		//x_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
		//iter_solved = 0;
		//return EQUAL_GUESS_VALUES;
	}
	else
	{
		y2 = y1;
	}	
	
	return solver_core(x_guess_1, y1, x_guess_2, y2, y_target, x_solved, tol_solved, iter_solved);
}

int C_monotonic_eq_solver::solve(S_xy_pair solved_pair_1, double x_guess_2, double y_target,
    double &x_solved, double &tol_solved, int &iter_solved)
{
    // Set / reset vector that tracks calls to equation
    ms_eq_call_tracker.resize(0);
    ms_eq_call_tracker.reserve(m_iter_max);

    double x_guess_1 = solved_pair_1.x;
    double y1 = solved_pair_1.y;

	ms_eq_tracker_temp.x = x_guess_1;
	ms_eq_tracker_temp.y = y1;
	ms_eq_tracker_temp.err_code = 0;
	ms_eq_call_tracker.push_back(ms_eq_tracker_temp);

    // Check that x guesses fall with bounds (set during initialization)
    x_guess_2 = check_against_limits(x_guess_2);

    double y2 = std::numeric_limits<double>::quiet_NaN();
    call_mono_eq(x_guess_2, &y2);

    return solver_core(x_guess_1, y1, x_guess_2, y2, y_target, x_solved, tol_solved, iter_solved);
}

int C_monotonic_eq_solver::solve(S_xy_pair solved_pair_1, S_xy_pair solved_pair_2, double y_target,
	double &x_solved, double &tol_solved, int &iter_solved)
{
	// We could also call the equation before solving for an x value, so this solve method
	//    allows us to pass in exactly 2 x-y pairs
	// .... could improve this in future to accept a variable number of x-y pairs

	// Set / reset vector that tracks calls to equation
	ms_eq_call_tracker.resize(0);
	ms_eq_call_tracker.reserve(m_iter_max);

	// Get x & y values from solved_pairs
	double x_guess_1 = solved_pair_1.x;
	double x_guess_2 = solved_pair_2.x;

	double y1 = solved_pair_1.y;
	double y2 = solved_pair_2.y;

	ms_eq_tracker_temp.x = x_guess_1;
	ms_eq_tracker_temp.y = y1;
	ms_eq_tracker_temp.err_code = 0;
	ms_eq_call_tracker.push_back(ms_eq_tracker_temp);

	ms_eq_tracker_temp.x = x_guess_2;
	ms_eq_tracker_temp.y = y2;
	ms_eq_tracker_temp.err_code = 0;
	ms_eq_call_tracker.push_back(ms_eq_tracker_temp);

	return solver_core(x_guess_1, y1, x_guess_2, y2, y_target, x_solved, tol_solved, iter_solved);
}

int C_monotonic_eq_solver::solve(std::vector<double> x_solved_vector, std::vector<double> y_solved_vector, double y_target,
    double &x_solved, double &tol_solved, int &iter_solved)
{
    size_t x_len = x_solved_vector.size();
    size_t y_len = y_solved_vector.size();

    if (x_len != y_len)
    {
        return NO_SOLUTION;
    }

    int i_low = -1;
    double y_low = std::numeric_limits<double>::quiet_NaN();
    int i_high = -1;
    double y_high = std::numeric_limits<double>::quiet_NaN();

    for (int i = 0; i < y_len; i++)
    {
        if (std::isfinite(y_solved_vector[i]) && y_solved_vector[i] <= y_target)
        {
            if ((i_low > -1 && y_solved_vector[i] > y_low) || i_low == -1)
            {
                i_low = i;
                y_low = y_solved_vector[i];
            }
        }
        else if(std::isfinite(y_solved_vector[i]))
        {
            if ((i_high > -1 && y_solved_vector[i] < y_high) || i_high == -1)
            {
                i_high = i;
                y_high = y_solved_vector[i];
            }
        }
    }

    if (i_low == -1 && i_high == -1)
    {
        return NO_SOLUTION;
    }
    else if (i_low == -1)
    {
        S_xy_pair xy_pair;
        xy_pair.x = x_solved_vector[i_high];
        xy_pair.y = y_solved_vector[i_high];

		ms_eq_tracker_temp.x = xy_pair.x;
		ms_eq_tracker_temp.y = xy_pair.y;
		ms_eq_tracker_temp.err_code = 0;
		ms_eq_call_tracker.push_back(ms_eq_tracker_temp);

        return solve(xy_pair, xy_pair.x*0.9, y_target, x_solved, tol_solved, iter_solved);
    }
    else if (i_high == -1)
    {
        S_xy_pair xy_pair;
        xy_pair.x = x_solved_vector[i_low];
        xy_pair.y = y_solved_vector[i_low];

		ms_eq_tracker_temp.x = xy_pair.x;
		ms_eq_tracker_temp.y = xy_pair.y;
		ms_eq_tracker_temp.err_code = 0;
		ms_eq_call_tracker.push_back(ms_eq_tracker_temp);

        return solve(xy_pair, xy_pair.x*0.9, y_target, x_solved, tol_solved, iter_solved);
    }
    else
    {
        S_xy_pair xy_pair1;
        xy_pair1.x = x_solved_vector[i_high];
        xy_pair1.y = y_solved_vector[i_high];

		ms_eq_tracker_temp.x = xy_pair1.x;
		ms_eq_tracker_temp.y = xy_pair1.y;
		ms_eq_tracker_temp.err_code = 0;
		ms_eq_call_tracker.push_back(ms_eq_tracker_temp);

        S_xy_pair xy_pair2;
        xy_pair2.x = x_solved_vector[i_low];
        xy_pair2.y = y_solved_vector[i_low];

		ms_eq_tracker_temp.x = xy_pair2.x;
		ms_eq_tracker_temp.y = xy_pair2.y;
		ms_eq_tracker_temp.err_code = 0;
		ms_eq_call_tracker.push_back(ms_eq_tracker_temp);

        return solve(xy_pair1, xy_pair2, y_target, x_solved, tol_solved, iter_solved);
    }
}

int C_monotonic_eq_solver::solver_core(double x_guess_1, double y1, double x_guess_2, double y2, double y_target,
	double &x_solved, double &tol_solved, int &iter_solved)
{
	// At this point, upstream 'solve' methods should have:
	// 1) Set/reset tracking vector
	// 2) Checked X values
	// 3) Found (but not checked) y values corresponding to each X value	
	// *****************************************************************

	// Check whether function returned real results
	if ( !std::isfinite(y1) && !std::isfinite(y2) )
	{
		// Neither x guess produced a real result. Get out.
		x_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
		iter_solved = 0;
		return NO_SOLUTION;
	}
	else if( !std::isfinite(y1) )
	{
		// y2 produced a real result, but y1 did not
		// So try guessing a new x1 on other side of x2
		x_guess_1 = x_guess_2 + (x_guess_2 - x_guess_1);
		// Check against bounds
		x_guess_1 = check_against_limits(x_guess_1);
		// Check that guesses are different
		if( x_guess_1 == x_guess_2 )
		{
			x_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
			iter_solved = 0;
			return NO_SOLUTION;
		}

		if( call_mono_eq(x_guess_1, &y1) != 0 )
		{
			y1 = std::numeric_limits<double>::quiet_NaN();
		}

		// Check if this worked...
		if( !std::isfinite(y1) )
		{
			x_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
			iter_solved = 0;
			return NO_SOLUTION;
		}
	}
	else if( !std::isfinite(y2) )
	{
		// y1 produced a real result, but y2 did not
		// So try guessing a new x2 on the other side of x1
		x_guess_2 = x_guess_1 + (x_guess_1 - x_guess_2);
		// Check against bounds
		x_guess_2 = check_against_limits(x_guess_2);
		// Check that guesses are different
		if( x_guess_1 == x_guess_2 )
		{
			x_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
			iter_solved = 0;
			return NO_SOLUTION;
		}
		
		if( call_mono_eq(x_guess_2, &y2) != 0 )
		{
			y2 = std::numeric_limits<double>::quiet_NaN();
		}

		// Check if this worked...
		if( !std::isfinite(y2) )
		{
			x_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
			iter_solved = 0;
			return NO_SOLUTION;
		}
	}

	// Ok, at this point, we should have two x guesses and two real y results
	// Now, need to calculate the slope of the error w/r/t x
	// So calculate errors
	double E1 = y1 - y_target;
	double E2 = y2 - y_target;

	// If tolerance corresponds to relative error, then calculate it
	if( m_is_err_rel )
	{
		if( y_target == 0 )
			return REL_TOL_WITH_0_TARGET;
		E1 = E1 / fabs(y_target);
		E2 = E2 / fabs(y_target);
	}

	// x guesses might be lucky, check against tolerance
	if( fabs(E1) < m_tol )
	{
		// Last call to equation was with x_guess_2, so call again here...
		// ... if equation is setting member data, this is required to have outputs matching calculated solution
		call_mono_eq(x_guess_1, &y1);
		x_solved = x_guess_1;
		tol_solved = E1;
		iter_solved = 0;
		return CONVERGED;
	}
	if( fabs(E2) < m_tol )
	{
		double last_x_tried = get_last_mono_eq_call().x;
		if(last_x_tried != x_guess_2 || !std::isfinite(last_x_tried))
			call_mono_eq(x_guess_2, &y2);
		x_solved = x_guess_2;
		tol_solved = E2;
		iter_solved = 0;
		return CONVERGED;
	}
	// ***************************************************

	// Calculate slope of error vs x
	m_E_slope = (E2 - E1) / (x_guess_2 - x_guess_1);

	if (m_E_slope == 0.0 || x_guess_1 == x_guess_2)
	{
		x_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
		iter_solved = 0;
		return EQUAL_GUESS_VALUES;
	}

	// Now can set some upper and lower bound and error information...
	if( E1 > 0.0 && E2 > 0.0 )
	{
		// Error/x slope is positive, and errors are positive, so we know upper bound and error
		m_is_pos_bound = true;
		m_is_pos_error = true;
		m_is_pos_error_prev = true;
		m_is_neg_bound = false;
		m_is_neg_error = false;
		m_is_neg_error_prev = false;
		double x_pos_err_prev = std::numeric_limits<double>::quiet_NaN();
		double y_err_pos_prev = std::numeric_limits<double>::quiet_NaN();
		if( E1 < E2 )
		{
			m_x_pos_err = x_guess_1;
			m_y_err_pos = E1;
			x_pos_err_prev = x_guess_2;
			y_err_pos_prev = E2;
		}
		else
		{
			m_x_pos_err = x_guess_2;
			m_y_err_pos = E2;
			x_pos_err_prev = x_guess_1;
			y_err_pos_prev = E1;
		}

		m_x_guess = calc_x_intercept(m_x_pos_err, m_y_err_pos, x_pos_err_prev, y_err_pos_prev);
		m_x_guess = check_against_limits(m_x_guess);
	}
	else if( E2 < 0.0 && E1 < 0.0 )
	{
		// Error/x slope is positive, and x values are both negative, so we know
		m_is_neg_bound = true;
		m_is_neg_error = true;
		m_is_neg_error_prev = true;
		m_is_pos_bound = false;
		m_is_pos_error = false;
		m_is_pos_error_prev = false;
		double x_neg_err_prev = std::numeric_limits<double>::quiet_NaN();
		double y_err_neg_prev = std::numeric_limits<double>::quiet_NaN();
		if( E1 < E2 )
		{
			m_x_neg_err = x_guess_2;
			m_y_err_neg = E2;
			x_neg_err_prev = x_guess_1;
			y_err_neg_prev = E1;
		}
		else
		{
			m_x_neg_err = x_guess_1;
			m_y_err_neg = E1;
			x_neg_err_prev = x_guess_2;
			y_err_neg_prev = E2;
		}

		m_x_guess = calc_x_intercept(m_x_neg_err, m_y_err_neg, x_neg_err_prev, y_err_neg_prev);
		m_x_guess = check_against_limits(m_x_guess);
	}
	else if( E1 > 0.0 )		// So, per above logic, x_guess_2 < 0.0
	{
		// Error/x slope is positive, and have both a negative and positive error
		m_is_pos_bound = true;
		m_is_pos_error = true;
		m_is_pos_error_prev = false;
		m_is_neg_bound = true;
		m_is_neg_error = true;
		m_is_neg_error_prev = false;

		m_x_pos_err = x_guess_1;
		m_y_err_pos = E1;

		m_x_neg_err = x_guess_2;
		m_y_err_neg = E2;

		m_x_guess = calc_x_intercept(m_x_pos_err, m_y_err_pos, m_x_neg_err, m_y_err_neg);
	}
	else		// E1 < 0.0   && E2 > 0.0
	{
		// Error/x slope is positive, and have both a negative and positive error
		m_is_pos_bound = true;
		m_is_pos_error = true;
		m_is_pos_error_prev = false;
		m_is_neg_bound = true;
		m_is_neg_error = true;
		m_is_neg_error_prev = false;

		m_x_pos_err = x_guess_2;
		m_y_err_pos = E2;

		m_x_neg_err = x_guess_1;
		m_y_err_neg = E1;

		m_x_guess = calc_x_intercept(m_x_pos_err, m_y_err_pos, m_x_neg_err, m_y_err_neg);
	}

	// ******************************
	// Need to send a x_guess into the iteration loop!!!!!
	// ******************************

	// Set error such that iteration loop is entered
	m_y_err = 999.9*m_tol;
	m_iter = 0;		// Counter is first line inside loop, so first iteration = 1

	// Start iteration loop
	while( fabs(m_y_err) > m_tol || !std::isfinite(m_y_err) )
	{
		m_iter++;		// First iteration = 1

		// Check if distance between bounds is too small
		double diff_x_bounds = std::numeric_limits<double>::quiet_NaN();
		if( m_E_slope > 0.0 )
		{
			if ( !std::isfinite(m_x_pos_err) )
			{	// Haven't set x bound on positive error. Because slope > 0, then check neg err x against Upper bound
				diff_x_bounds = m_func_x_upper - m_x_neg_err;
			}
			else if ( !std::isfinite(m_x_neg_err) )
			{	// Haven't set x bound on negative error. Because slope > 0, then check pos err x against Lower bound
				diff_x_bounds = m_x_pos_err - m_func_x_lower;
			}
			else
			{	// Both bounds are set, so compare them
				diff_x_bounds = m_x_pos_err - m_x_neg_err;
			}
		}
		else
		{
			if ( !std::isfinite(m_x_pos_err) )
			{	// Haven't set x bound on positive error. Because slope < 0, then check neg err x against Lower bound
				diff_x_bounds = m_x_neg_err - m_func_x_lower;
			}
			else if ( !std::isfinite(m_x_neg_err) )
			{	// Haven't set x bound on negative error. Because slope < 0, then check pos err x against Upper bound
				diff_x_bounds = m_func_x_upper - m_x_pos_err;
			}
			else
			{	// Both bounds are set, so compare them
				diff_x_bounds = m_x_neg_err - m_x_pos_err;
			}
		}
		if (m_is_err_rel)
		{
			// Not sure that it necessarily makes sense to tie normalization of diff_x_bounds to relative y error...
			// But for now, at least need to make sure that diff_x_bounds doesn't end up nan
			if (std::isfinite(m_x_neg_err) && std::isfinite(m_x_pos_err))
			{
				diff_x_bounds = diff_x_bounds / std::max(m_x_neg_err, m_x_pos_err);
			}
			else if (std::isfinite(m_x_neg_err))
			{
				diff_x_bounds = diff_x_bounds / m_x_neg_err;
			}
			else if (std::isfinite(m_x_pos_err))
			{
				diff_x_bounds = diff_x_bounds / m_x_pos_err;
			}

			// diff_x_bounds = diff_x_bounds / std::max(m_x_neg_err, m_x_pos_err);
		}

		// If it is early in the iteration and it was not set with finite bounds, then diff_x_bounds might be nan
		if (fabs(diff_x_bounds) < m_tol / 10.0 && m_iter > 1)
		{	// Assumes if x values are too close, then *something* is preventing convergence

			// 1) Solver can't find a negative error
			if (!m_is_neg_error && m_is_pos_error)
			{
				if (std::isfinite(m_y_err))
				{
					x_solved = m_x_guess;
					tol_solved = m_y_err;
					iter_solved = m_iter - 1;
				}
				else
				{
					x_solved = m_x_pos_err;
					tol_solved = m_y_err_pos;
					iter_solved = m_iter;
				}

                double x_at_lowest = std::numeric_limits<double>::quiet_NaN();
                if (is_last_x_best(x_at_lowest, y_target))
                {
                    m_x_guess = x_at_lowest;
                    m_y_err = call_mono_eq_calc_y_err(m_x_guess, y_target);
                    x_solved = m_x_guess;
                    tol_solved = m_y_err;
                }

				if (m_E_slope > 0.0)
					return SLOPE_POS_NO_NEG_ERR;
				else
					return SLOPE_NEG_NO_NEG_ERR;
			}
			// 2) Solver can't find a positive error
			else if (m_is_neg_error && !m_is_pos_error)
			{
				if (std::isfinite(m_y_err))
				{
					x_solved = m_x_guess;
					tol_solved = m_y_err;
					iter_solved = m_iter - 1;
				}
				else
				{
					x_solved = m_x_neg_err;
					tol_solved = m_y_err_neg;
					iter_solved = m_iter;
				}

                double x_at_lowest = std::numeric_limits<double>::quiet_NaN();
                if (is_last_x_best(x_at_lowest, y_target))
                {
                    m_x_guess = x_at_lowest;
                    m_y_err = call_mono_eq_calc_y_err(m_x_guess, y_target);
                    x_solved = m_x_guess;
                    tol_solved = m_y_err;
                }

				if (m_E_slope > 0.0)
					return SLOPE_POS_NO_POS_ERR;
				else
					return SLOPE_NEG_NO_POS_ERR;
			}
			// 3) Solver has bound both negative and positive errors
			else
			{	// Function is returning a value, but solver hasn't converged within tolerance
				if (std::isfinite(m_y_err))
				{
					x_solved = m_x_guess;
					tol_solved = m_y_err;
					iter_solved = m_iter - 1;
				}
				else
				{
					x_solved = m_x_guess;
					tol_solved = m_y_err;
					iter_solved = m_iter;
                }

                double x_at_lowest = std::numeric_limits<double>::quiet_NaN();
                if (is_last_x_best(x_at_lowest, y_target))
                {
                    m_x_guess = x_at_lowest;
                    m_y_err = call_mono_eq_calc_y_err(m_x_guess, y_target);
                    x_solved = m_x_guess;
                    tol_solved = m_y_err;
                }

				if (m_E_slope > 0.0)
					return SLOPE_POS_BOTH_ERRS;
				else
					return SLOPE_NEG_BOTH_ERRS;
			}
		}
		// Also check against iteration limit
		if (m_iter > m_iter_max)
		{
			// 1) Solver can't find a negative error
			if (!m_is_neg_error && m_is_pos_error)
			{
				if (std::isfinite(m_y_err))
				{
					x_solved = m_x_guess;
					tol_solved = m_y_err;
					iter_solved = m_iter - 1;
				}
				else
				{
					x_solved = m_x_pos_err;
					tol_solved = m_y_err_pos;
					iter_solved = m_iter;
				}

                double x_at_lowest = std::numeric_limits<double>::quiet_NaN();
                if (is_last_x_best(x_at_lowest, y_target))
                {
                    m_x_guess = x_at_lowest;
                    m_y_err = call_mono_eq_calc_y_err(m_x_guess, y_target);
                    x_solved = m_x_guess;
                    tol_solved = m_y_err;
                }

				if (m_E_slope > 0.0)
					return MAX_ITER_SLOPE_POS_NO_NEG_ERR;
				else
					return MAX_ITER_SLOPE_NEG_NO_NEG_ERR;
			}
			// 2) Solver can't find a positive error
			else if (m_is_neg_error && !m_is_pos_error)
			{
				if (std::isfinite(m_y_err))
				{
					x_solved = m_x_guess;
					tol_solved = m_y_err;
					iter_solved = m_iter - 1;
				}
				else
				{
					x_solved = m_x_neg_err;
					tol_solved = m_y_err_neg;
					iter_solved = m_iter;
				}

                double x_at_lowest = std::numeric_limits<double>::quiet_NaN();
                if (is_last_x_best(x_at_lowest, y_target))
                {
                    m_x_guess = x_at_lowest;
                    m_y_err = call_mono_eq_calc_y_err(m_x_guess, y_target);
                    x_solved = m_x_guess;
                    tol_solved = m_y_err;
                }

				if (m_E_slope > 0.0)
					return MAX_ITER_SLOPE_POS_NO_POS_ERR;
				else
					return MAX_ITER_SLOPE_NEG_NO_POS_ERR;
			}
			// 3) Solver has bound both negative and positive errors
			else
			{	// Function is returning a value, but solver hasn't converged within tolerance
				if (std::isfinite(m_y_err))
				{
					x_solved = m_x_guess;
					tol_solved = m_y_err;
					iter_solved = m_iter - 1;
				}
				else
				{
					x_solved = m_x_guess;
					tol_solved = m_y_err;
					iter_solved = m_iter;
				}

                double x_at_lowest = std::numeric_limits<double>::quiet_NaN();
                if (is_last_x_best(x_at_lowest, y_target))
                {
                    m_x_guess = x_at_lowest;
                    m_y_err = call_mono_eq_calc_y_err(m_x_guess, y_target);
                    x_solved = m_x_guess;
                    tol_solved = m_y_err;
                }

				if (m_E_slope > 0.0)
					return MAX_ITER_SLOPE_POS_BOTH_ERRS;
				else
					return MAX_ITER_SLOPE_NEG_BOTH_ERRS;
			}
		}

		// **************************************************************
		// **************************************************************

		// Subsequent iterations need to re-calculate x
		if( m_iter > 1 )
		{
			if ( !std::isfinite(m_y_err) )
			{	// Function isn't returning a real value with which to calculate an error
				if( !m_is_neg_bound && !m_is_pos_bound )
				{	// This shouldn't occur, as we need at least one bound to find the error slope, but let's keep it...
					x_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
					iter_solved = m_iter;
					return NO_SOLUTION;
				}
				else if( m_is_neg_bound && !m_is_pos_bound )
				{	// know that we have a negative error bound, so assume that x that caused NaN is pos err bound
					m_x_pos_err = m_x_guess;
					m_is_pos_bound = true;
					m_is_pos_error = false;
					m_is_pos_error_prev = false;
					m_x_guess = 0.5*(m_x_pos_err + m_x_neg_err);
					// Should need to check against bounds if both 'is_bounds' = true
				}
				else if( !m_is_neg_bound && m_is_pos_bound )
				{	// know that we have an upper bound, so assume that x that caused NaN is lower bound
					m_x_neg_err = m_x_guess;
					m_is_neg_bound = true;
					m_is_neg_error = false;
					m_is_neg_error_prev = false;
					m_x_guess = 0.5*(m_x_pos_err + m_x_neg_err);
					// Should need to check against bounds if both 'is_bounds' = true
				}
				else if( m_is_neg_error && !m_is_pos_error )
				{	// Know both bounds, but don't have positive error, so assume that x that caused NaN is pos error bound
					// so keep using bisection and hopefully get a real number out of function
					m_x_pos_err = m_x_guess;
					m_x_guess = 0.5*(m_x_pos_err + m_x_neg_err);
				}
				else if( !m_is_neg_error && m_is_pos_error )
				{	// Know both bounds, but don't have negative error, so assume that x that caused NaN is neg error bound
					// so keep using bisection and hopefully get a real number out of function
					m_x_neg_err = m_x_guess;
					m_x_guess = 0.5*(m_x_pos_err + m_x_neg_err);
				}
				else
				{	// This shouldn't happen, so let's throw an exception
					// throw(C_csp_exception("Numerical solver iteration with a NaN error found an unexpected case"));

					// Have both positive and negative errors, but x value in between is causing an error
					// Not expected behavior for the function, but try to get around the problem
					double x_min_abs_err = m_x_pos_err;
					if (fabs(m_y_err_neg) < fabs(m_y_err_pos))
						x_min_abs_err = m_x_neg_err;

					m_x_guess = 0.5*(m_x_guess + x_min_abs_err);
				}
			}
			else if( m_y_err > 0.0 )
			{
				double x_pos_err_prev = std::numeric_limits<double>::quiet_NaN();
				double y_err_pos_prev = std::numeric_limits<double>::quiet_NaN();
				if( m_is_pos_error )
				{
					if (m_y_err > m_y_err_pos)
					{	// Equation not behaving monotonically, so disregard previous smaller error and use current larger
						m_is_pos_error_prev = false;
					}
					else
					{
						m_is_pos_error_prev = true;
						x_pos_err_prev = m_x_pos_err;
						y_err_pos_prev = m_y_err_pos;
					}					
				}
				else
				{
					m_is_pos_error_prev = false;
				}
				m_x_pos_err = m_x_guess;
				m_y_err_pos = m_y_err;
				m_is_pos_bound = true;
				m_is_pos_error = true;
				if( !m_is_neg_bound )
				{	// Only have positive error, and getting another positive error, so...
					if( m_is_pos_error_prev )
					{	// If we have two positive errors, then linearly interpolate using them
						m_x_guess = calc_x_intercept(m_x_pos_err, m_y_err_pos, x_pos_err_prev, y_err_pos_prev);
					}
					else if(m_E_slope > 0.0 )
					{
						m_x_guess = m_x_pos_err - 0.5*std::max(x_guess_1, x_guess_2);
					}
					else
					{

						m_x_guess = m_x_pos_err + 0.5*std::max(fabs(x_guess_1), fabs(x_guess_2));
					}
					m_x_guess = check_against_limits(m_x_guess);
				}
				else if( !m_is_neg_error )           //( (m_is_neg_bound && !m_is_pos_bound) || (m_is_neg_bound && m_is_pos_bound) )
				{	// Have a negative error bound, but not error
					m_x_guess = 0.5*(m_x_pos_err + m_x_neg_err);
				}
				else
				{
					m_x_guess = calc_x_intercept(m_x_neg_err, m_y_err_neg, m_x_pos_err, m_y_err_pos);
				}
			}
			else		// (m_y_err < 0.0)
			{
				double x_neg_err_prev = std::numeric_limits<double>::quiet_NaN();
				double y_err_neg_prev = std::numeric_limits<double>::quiet_NaN();
				if( m_is_neg_error )
				{
					if (m_y_err < m_y_err_neg)
					{	// Equation not behaving monotonically, so disregard previous smaller error and use current larger
						m_is_neg_error_prev = false;
					}
					else
					{
						m_is_neg_error_prev = true;
						x_neg_err_prev = m_x_neg_err;
						y_err_neg_prev = m_y_err_neg;
					}					
				}
				else
				{
					m_is_pos_error_prev = false;
				}
				m_x_neg_err = m_x_guess;
				m_y_err_neg = m_y_err;
				m_is_neg_bound = true;
				m_is_neg_error = true;
				if( !m_is_pos_bound )
				{	// Only have negative error, and getting another negative error, so use bisection
					if( m_is_neg_error_prev )
					{
						m_x_guess = calc_x_intercept(m_x_neg_err, m_y_err_neg, x_neg_err_prev, y_err_neg_prev);
					}
					else if( m_E_slope > 0.0 )
					{
						m_x_guess = m_x_neg_err + 0.5*std::max(fabs(x_guess_1), fabs(x_guess_2));
					}
					else
					{
						m_x_guess = m_x_neg_err - 0.5*std::max(x_guess_1, x_guess_2);
					}
					m_x_guess = check_against_limits(m_x_guess);
				}
				else if( !m_is_pos_error )		//( (!m_is_neg_bound && m_is_pos_bound) || (m_is_neg_bound && m_is_pos_bound) )
				{	// Have a positive error bound, but not error
					m_x_guess = 0.5*(m_x_pos_err + m_x_neg_err);
				}
				else
				{
					m_x_guess = calc_x_intercept(m_x_neg_err, m_y_err_neg, m_x_pos_err, m_y_err_pos);
				}
			}
		}

		// Call function with new x_guess
        m_y_err = call_mono_eq_calc_y_err(m_x_guess, y_target);

	}	// End of iteration on x

	x_solved = m_x_guess;
	tol_solved = m_y_err;
	iter_solved = m_iter;

	return CONVERGED;
}

double C_monotonic_eq_solver::call_mono_eq_calc_y_err(double x, double y_target)
{
    // Call function with new x_guess
    double y_calc;
    if (call_mono_eq(x, &y_calc) != 0)
    {
        y_calc = std::numeric_limits<double>::quiet_NaN();
    }

    double y_err = y_calc - y_target;

    if (m_is_err_rel)
        y_err = y_err / fabs(y_target);

    return y_err;
}

int C_monotonic_eq_solver::call_mono_eq(double x, double *y)
{
    try
    {
        ms_eq_tracker_temp.err_code = mf_mono_eq(x, y);
    }
    catch (...)
    {
        *y = std::numeric_limits<double>::quiet_NaN();
        ms_eq_tracker_temp.err_code = -99;
    }

	ms_eq_tracker_temp.x = x;
	ms_eq_tracker_temp.y = *y;
	
	ms_eq_call_tracker.push_back(ms_eq_tracker_temp);

	return ms_eq_tracker_temp.err_code;
}

bool C_monotonic_eq_solver::is_last_x_best(double & x_at_lowest, double y_target)
{
    C_monotonic_eq_solver::S_eq_chars s_eq_chars_min_abs_diff;

    bool is_use_last_x = false;
    x_at_lowest = std::numeric_limits<double>::quiet_NaN();

    if (get_min_abs_diff_no_err(s_eq_chars_min_abs_diff, y_target))
    {
        double y_err = s_eq_chars_min_abs_diff.y - y_target;
        if (m_is_err_rel)
            y_err = y_err / fabs(y_target);

        double min_abs_diff = fabs(y_err);
        if (min_abs_diff < std::fabs(m_y_err) || !std::isfinite(m_y_err))
        {
            x_at_lowest = s_eq_chars_min_abs_diff.x;

            is_use_last_x = true;
        }
    }

    return is_use_last_x;
}

 bool C_monotonic_eq_solver::get_min_abs_diff_no_err(C_monotonic_eq_solver::S_eq_chars & s_eq_chars_min_abs_diff,
                                            double y_target)
{
    int len = ms_eq_call_tracker.size();
    if (len == 0)
    {
        return false;
    }
    
    bool is_found_min = false;
    double min_abs_diff = std::numeric_limits<double>::quiet_NaN();
    double y_err = std::numeric_limits<double>::quiet_NaN();

    for (int i = 0; i < len; i++)
    {
        C_monotonic_eq_solver::S_eq_chars i_ms_eq = ms_eq_call_tracker[i];
        if (i_ms_eq.err_code == 0 && std::isfinite(i_ms_eq.y))
        {
            y_err = fabs(i_ms_eq.y - y_target);
            if (m_is_err_rel)
            {
                y_err = y_err / fabs(y_target);
            }

            if (is_found_min && y_err < min_abs_diff)
            {
                min_abs_diff = y_err;
                s_eq_chars_min_abs_diff = i_ms_eq;
            }
            else if (!is_found_min)
            {
                min_abs_diff = y_err;
                is_found_min = true;
                s_eq_chars_min_abs_diff = i_ms_eq;
            }
        }
    }

    return is_found_min;
}

const C_monotonic_eq_solver::S_eq_chars C_monotonic_eq_solver::get_last_mono_eq_call()
{
	// Get length of saved equation calls
	int len = ms_eq_call_tracker.size();

	if( len == 0 )
	{
		C_monotonic_eq_solver::S_eq_chars s_null;
		return s_null;
	}
	else
	{
		return ms_eq_call_tracker[len - 1];
	}
}

int C_monotonic_eq_solver::test_member_function(double x, double *y)
{
	return mf_mono_eq(x,y);
}

bool C_monotonic_eq_solver::did_solver_find_negative_error(int solver_exit_mode)
{
	if (solver_exit_mode == SLOPE_POS_NO_NEG_ERR
		|| solver_exit_mode == SLOPE_NEG_NO_NEG_ERR
		|| solver_exit_mode == MAX_ITER_SLOPE_NEG_NO_NEG_ERR
		|| solver_exit_mode == MAX_ITER_SLOPE_POS_NO_NEG_ERR)
	{
		return false;
	}

	return true;
}

bool C_monotonic_eq_solver::did_solver_find_positive_error(int solver_exit_mode)
{
	if (solver_exit_mode == SLOPE_POS_NO_POS_ERR
		|| solver_exit_mode == SLOPE_NEG_NO_POS_ERR
		|| solver_exit_mode == MAX_ITER_SLOPE_NEG_NO_POS_ERR
		|| solver_exit_mode == MAX_ITER_SLOPE_POS_NO_POS_ERR)
	{
		return false;
	}

	return true;
}

double C_monotonic_eq_solver::get_E_slope()
{
    return m_E_slope;
}
