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

#include "csp_solver_core.h"
#include "numeric_solvers.h"
#include <cmath>
#include <math.h>
#include <algorithm>

#include "lib_util.h"

void C_csp_solver::reset_time(double step /*s*/)
{
    mc_kernel.mc_sim_info.ms_ts.m_step = step;						//[s]
    mc_kernel.mc_sim_info.ms_ts.m_time =
        mc_kernel.mc_sim_info.ms_ts.m_time_start +
        mc_kernel.mc_sim_info.ms_ts.m_step;		//[s]
}

int C_csp_solver::solve_operating_mode(int cr_mode, C_csp_power_cycle::E_csp_power_cycle_modes pc_mode,
    C_MEQ__m_dot_tes::E_m_dot_solver_modes solver_mode, C_MEQ__timestep::E_timestep_target_modes step_target_mode,
    double q_dot_pc_target /*MWt*/, bool is_defocus,
    std::string op_mode_str, double & defocus_solved)
{
    double t_ts_initial = mc_kernel.mc_sim_info.ms_ts.m_step;   //[s]

    C_MEQ__defocus c_mdot_eq(solver_mode, C_MEQ__defocus::E_M_DOT_BAL, step_target_mode, this,
        q_dot_pc_target,
        pc_mode, cr_mode,
        t_ts_initial); //, step_tolerance);
    C_monotonic_eq_solver c_mdot_solver(c_mdot_eq);

    double df_full = 1.0;
    double m_dot_bal = std::numeric_limits<double>::quiet_NaN();
    int m_dot_bal_code = c_mdot_solver.test_member_function(df_full, &m_dot_bal);

    if (m_dot_bal_code != 0)
    {
        reset_time(t_ts_initial);
        return -1;
    }
    
    defocus_solved = df_full;      //[-]
    bool is_m_dot_bal_converged = false;
    if (is_defocus)
    {
        if (m_dot_bal > 0.0)
        {
            // At no defocus, PC mass flow rate is exceeding limits
            // So, need to find defocus that results in mass flow <= max
            C_monotonic_eq_solver::S_xy_pair xy1;
            xy1.x = defocus_solved;
            xy1.y = m_dot_bal;

            // Guess another value
            C_monotonic_eq_solver::S_xy_pair xy2;
            double m_dot_bal2 = std::numeric_limits<double>::quiet_NaN();
            double x1 = xy1.x;
            do
            {
                xy2.x = x1 * (1.0 / (1.0 + m_dot_bal));

                m_dot_bal_code = c_mdot_solver.test_member_function(xy2.x, &m_dot_bal2);
                if (m_dot_bal_code != 0)
                {
                    reset_time(t_ts_initial);
                    return -2;
                }
                x1 = xy2.x;
            } while (fabs(m_dot_bal2 - m_dot_bal) < 0.02);

            xy2.y = m_dot_bal2;

            c_mdot_solver.settings(1.E-3, 50, 0.0, 1.0, false);

            // Now solve for the required defocus
            double tol_solved;
            tol_solved = std::numeric_limits<double>::quiet_NaN();
            int iter_solved = -1;

            try
            {
                m_dot_bal_code = c_mdot_solver.solve(xy1, xy2, -1.E-3, defocus_solved, tol_solved, iter_solved);
            }
            catch (C_csp_exception)
            {
                reset_time(t_ts_initial);
                return -3;
            }
            if (m_dot_bal_code != C_monotonic_eq_solver::CONVERGED)
            {
                if (m_dot_bal_code > C_monotonic_eq_solver::CONVERGED && fabs(tol_solved) < 0.1)
                {
                    std::string msg = util::format("At time = %lg %s "
                        "iteration to find a defocus resulting in the maximum power cycle mass flow rate only reached a convergence "
                        "= %lg. Check that results at this timestep are not unreasonably biasing total simulation results",
                        mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, op_mode_str.c_str(), tol_solved);
                    mc_csp_messages.add_message(C_csp_messages::NOTICE, msg);
                }
                else
                {
                    // Weird that controller chose Defocus operating mode, so report message and shut down CR and PC
                    error_msg = util::format("At time = %lg the controller chose %s operating mode, but the code"
                        " failed to converge.",
                        mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, op_mode_str.c_str());
                    mc_csp_messages.add_message(C_csp_messages::NOTICE, error_msg);

                    reset_time(t_ts_initial);
                    return -4;
                }
            }
            is_m_dot_bal_converged = true;
        }

        if ((mc_pc_out_solver.m_q_dot_htf - m_q_dot_pc_max) / m_q_dot_pc_max > 1.E-3)
        {
            // Have defocused such that balancing mass flow rates should not result in
            //    a cycle mass flow rate greater than the max
            double defocus_guess = defocus_solved;

            C_MEQ__defocus c_q_dot_eq(C_MEQ__m_dot_tes::E__CR_OUT__CR_OUT_LESS_TES_FULL, C_MEQ__defocus::E_Q_DOT_PC, step_target_mode, 
                this,
                q_dot_pc_target,
                pc_mode, cr_mode,
                t_ts_initial);
            C_monotonic_eq_solver c_q_dot_solver(c_q_dot_eq);

            // Set up solver
            c_q_dot_solver.settings(1.E-3, 50, 0.0, defocus_guess, true);

            double q_dot_pc_1 = std::numeric_limits<double>::quiet_NaN();
            int q_dot_df_code = -1;
            double defocus_set = defocus_guess;
            while (q_dot_df_code != 0)
            {
                defocus_guess = defocus_set;

                q_dot_df_code = c_q_dot_solver.test_member_function(defocus_guess, &q_dot_pc_1);
                if (q_dot_df_code != 0 && defocus_guess < 0.1)
                {
                    // Weird that controller chose Defocus operating mode, so report message and shut down CR and PC
                    error_msg = util::format("At time = %lg the controller chose %s operating mode, but the code"
                        " failed to converge.",
                        mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, op_mode_str.c_str());
                    mc_csp_messages.add_message(C_csp_messages::NOTICE, error_msg);

                    reset_time(t_ts_initial);
                    return -5;
                }

                defocus_set *= 0.8;
            }

            C_monotonic_eq_solver::S_xy_pair xy_q_dot_1;
            xy_q_dot_1.x = defocus_guess;
            xy_q_dot_1.y = q_dot_pc_1;

            // Solve for defocus
            double defocus_guess_q_dot = (std::max)(0.7*defocus_guess, (std::min)(0.99*defocus_guess, defocus_guess * (m_q_dot_pc_max / mc_pc_out_solver.m_q_dot_htf)));

            double defocus_solved, tol_solved;
            defocus_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
            int iter_solved = -1;

            int solver_code = 0;
            try
            {
                solver_code = c_q_dot_solver.solve(xy_q_dot_1, defocus_guess_q_dot, m_q_dot_pc_max, defocus_solved, tol_solved, iter_solved);
            }
            catch (C_csp_exception)
            {
                reset_time(t_ts_initial);
                return -6;
            }

            if (solver_code != C_monotonic_eq_solver::CONVERGED)
            {
                if (solver_code > C_monotonic_eq_solver::CONVERGED && fabs(tol_solved) < 0.1)
                {
                    std::string msg = util::format("At time = %lg %s "
                        "iteration to find a defocus resulting in the maximum power cycle heat input only reached a convergence "
                        "= %lg. Check that results at this timestep are not unreasonably biasing total simulation results",
                        mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, op_mode_str.c_str(), tol_solved);
                    mc_csp_messages.add_message(C_csp_messages::NOTICE, msg);
                }
                else
                {
                    // Weird that controller chose Defocus operating mode, so report message and shut down CR and PC
                    error_msg = util::format("At time = %lg the controller chose %s operating mode, but the code"
                        " failed to solve. Controller will shut-down CR and PC",
                        mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, op_mode_str.c_str());
                    mc_csp_messages.add_message(C_csp_messages::NOTICE, error_msg);

                    reset_time(t_ts_initial);
                    return -7;
                }
            }
        }
        else if (defocus_solved == 1.0)
        {
            C_MEQ__m_dot_tes::E_m_dot_solver_modes solver_mode_df1 = C_MEQ__m_dot_tes::E__CR_OUT__CR_OUT_LESS_TES_FULL;

            if (pc_mode == C_csp_power_cycle::STARTUP_CONTROLLED)
            {
                solver_mode_df1 = C_MEQ__m_dot_tes::E__CR_OUT__ITER_M_DOT_SU_CH_ONLY;
            }

            // Need to make sure mass flow isn't unbalanced by sending too much to the power cycle
            C_MEQ__defocus c_bal_eq(solver_mode_df1, C_MEQ__defocus::E_Q_DOT_PC, step_target_mode, this,
                //m_dot_tes, 
                q_dot_pc_target,
                pc_mode, cr_mode,
                t_ts_initial);  // , step_tolerance);
            C_monotonic_eq_solver c_bal_solver(c_bal_eq);

            // Set up solver
            c_bal_solver.settings(1.E-3, 50, 0.0, defocus_solved, true);

            double q_dot_pc_bal = std::numeric_limits<double>::quiet_NaN();
            int q_dot_bal_code = c_bal_solver.test_member_function(defocus_solved, &q_dot_pc_bal);
            if (q_dot_bal_code != 0)
            {
                // Weird that controller chose Defocus operating mode, so report message and shut down CR and PC
                /*error_msg = util::format("At time = %lg the controller chose %s operating mode, but the code"
                    " failed to solve. Controller will shut-down CR and PC",
                    mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, op_mode_str.c_str());
                mc_csp_messages.add_message(C_csp_messages::NOTICE, error_msg);*/

                reset_time(t_ts_initial);
                return -8;
            }
        }
    }

    return 0;
}

double C_csp_solver::C_MEQ__defocus::calc_meq_target()
{
    if (m_df_target_mode == C_MEQ__defocus::E_M_DOT_BAL)
    {
        // Calculate and report mass flow rate balance
        double m_dot_rec = mpc_csp_solver->mc_cr_out_solver.m_m_dot_salt_tot;	//[kg/hr]
        double m_dot_pc = mpc_csp_solver->mc_pc_out_solver.m_m_dot_htf;			//[kg/hr]
        double m_dot_ch = std::max(0.0, mpc_csp_solver->mc_tes_outputs.m_m_dot_cr_to_tes_hot -
            mpc_csp_solver->mc_tes_outputs.m_m_dot_tes_hot_out)*3600.0;    // mpc_csp_solver->mc_tes_ch_htf_state.m_m_dot;          //[kg/hr]
        double m_dot_dc = std::max(0.0, mpc_csp_solver->mc_tes_outputs.m_m_dot_pc_to_tes_cold -
            mpc_csp_solver->mc_tes_outputs.m_m_dot_tes_cold_out)*3600.0;  // mpc_csp_solver->mc_tes_dc_htf_state.m_m_dot;          //[kg/hr]

        return (m_dot_rec + m_dot_dc - m_dot_pc - m_dot_ch) / mpc_csp_solver->m_m_dot_pc_des; //[-]
    }
    else if (m_df_target_mode == C_MEQ__defocus::E_Q_DOT_PC)
    {
        return mpc_csp_solver->mc_pc_out_solver.m_q_dot_htf;	//[MWt]
    }
}

int C_csp_solver::C_MEQ__defocus::operator()(double defocus /*-*/, double *target /*-*/)
{
    C_MEQ__timestep c_T_cold_eq(m_solver_mode, m_ts_target_mode, mpc_csp_solver,
        m_q_dot_pc_target,
        m_pc_mode, m_cr_mode, defocus);
    C_monotonic_eq_solver c_T_cold_solver(c_T_cold_eq);

    double t_ts_solved = std::numeric_limits<double>::quiet_NaN();
    mpc_csp_solver->mc_kernel.mc_sim_info.ms_ts.m_step = m_t_ts_initial;    //[s]

    double t_ts_max_local = m_t_ts_initial;     //[s]

    if (m_ts_target_mode == C_MEQ__timestep::E_STEP_Q_DOT_PC)
    {
        double t_ts_guess = t_ts_max_local;     //[s]
        double q_dot_pc_calc = std::numeric_limits<double>::quiet_NaN();
        int test_code = c_T_cold_solver.test_member_function(t_ts_guess, &q_dot_pc_calc);

        if (test_code != 0 || (q_dot_pc_calc - m_q_dot_pc_target) / m_q_dot_pc_target < 1.E-3)
        {
            C_monotonic_eq_solver::S_xy_pair xy1;
            xy1.x = t_ts_guess;     //[s]
            xy1.y = q_dot_pc_calc;  //[MWt]
            double t_ts_prev = t_ts_guess;

            while (test_code != 0)
            {
                t_ts_prev = xy1.x;
                xy1.x *= 0.8;       //[s]
                xy1.y = std::numeric_limits<double>::quiet_NaN();

                test_code = c_T_cold_solver.test_member_function(xy1.x, &xy1.y);

                if (xy1.x < mpc_csp_solver->m_step_tolerance)
                {
                    mpc_csp_solver->reset_time(m_t_ts_initial);
                    return -6;
                }
            }

            t_ts_guess = 0.5*xy1.x;

            if (xy1.y - m_q_dot_pc_target > 0.0)
            {
                t_ts_guess = 1.05*xy1.x;
            }

            c_T_cold_solver.settings(1.E-3, 50, 0.1, t_ts_prev, true);

            double tol_solved = std::numeric_limits<double>::quiet_NaN();  //[s]
            int iter_solved = -1;
            int t_ts_code = 0;
            try
            {
                // Want solver to miss such that solved startup time is greater than components require
                // If components require slightly more, then they won't completely finish startup and next time period will require
                //       a very short timestep of startup
                t_ts_code = c_T_cold_solver.solve(xy1, t_ts_guess, m_q_dot_pc_target, t_ts_solved, tol_solved, iter_solved);
            }
            catch (C_csp_exception)
            {
                *target = std::numeric_limits<double>::quiet_NaN();

                mpc_csp_solver->reset_time(m_t_ts_initial);
                
                return -4;
            }
            if (t_ts_code != C_monotonic_eq_solver::CONVERGED)
            {
                if (t_ts_code > C_monotonic_eq_solver::CONVERGED && fabs(tol_solved) < 0.1)
                {
                    std::string msg = util::format("At time = %lg power cycle startup time iteration "
                        " only reached a convergence"
                        "= %lg [s]. Check that results at this timestep are not unreasonably biasing total simulation results",
                        mpc_csp_solver->mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, tol_solved);
                    mpc_csp_solver->mc_csp_messages.add_message(C_csp_messages::NOTICE, msg);
                }
                else
                {
                    *target = std::numeric_limits<double>::quiet_NaN();

                    mpc_csp_solver->reset_time(m_t_ts_initial);
                    
                    return -7;
                }
            }
        }
        else
        {
            t_ts_solved = t_ts_guess;       //[s]
        }

        if (m_cr_mode == C_csp_collector_receiver::STARTUP)
        {
            double t_ts_cr_su = mpc_csp_solver->mc_cr_out_solver.m_time_required_su;       //[s]
            if (t_ts_cr_su < t_ts_solved)
            {
                m_ts_target_mode = C_MEQ__timestep::E_STEP_FROM_COMPONENT;
                t_ts_max_local = t_ts_solved;
                t_ts_solved = std::numeric_limits<double>::quiet_NaN();
            }
        }
    }

    if (m_ts_target_mode == C_MEQ__timestep::E_STEP_FROM_COMPONENT
        || m_ts_target_mode == C_MEQ__timestep::E_STEP_FIXED)
    {
        double diff_t_ts_calculated = std::numeric_limits<double>::quiet_NaN();
        int test_code = c_T_cold_solver.test_member_function(t_ts_max_local, &diff_t_ts_calculated);

        if (m_ts_target_mode == C_MEQ__timestep::E_STEP_FIXED)
        {
            if (test_code != 0)
            {
                *target = std::numeric_limits<double>::quiet_NaN();

                mpc_csp_solver->reset_time(m_t_ts_initial);

                return -1;
            }
            
            *target = calc_meq_target();

            mpc_csp_solver->reset_time(m_t_ts_initial);

            return 0;
        }

        double t_ts_guess = t_ts_max_local;     //[s]
        double t_ts_prev = t_ts_guess;

        while (test_code != 0)
        {
            t_ts_prev = t_ts_guess;
            t_ts_guess *= 0.8;       //[s]
            diff_t_ts_calculated = std::numeric_limits<double>::quiet_NaN();

            test_code = c_T_cold_solver.test_member_function(t_ts_guess, &diff_t_ts_calculated);

            if (t_ts_guess < mpc_csp_solver->m_step_tolerance)
            {
                mpc_csp_solver->reset_time(m_t_ts_initial);
                return -6;
            }
        }

        t_ts_max_local = t_ts_prev;

        if (m_ts_target_mode == C_MEQ__timestep::E_STEP_FROM_COMPONENT && test_code == 0)
        {
            double t_ts_calculated = diff_t_ts_calculated + t_ts_guess;   //[s]
            if (t_ts_calculated <= 0.0)
            {
                *target = std::numeric_limits<double>::quiet_NaN();
                
                mpc_csp_solver->reset_time(m_t_ts_initial);
                
                return -2;
            }

            if (t_ts_calculated < t_ts_guess - mpc_csp_solver->m_step_tolerance)
            {
                C_monotonic_eq_solver::S_xy_pair xy1;
                xy1.x = t_ts_guess;   //[s]
                xy1.y = diff_t_ts_calculated;

                t_ts_solved = t_ts_calculated + 0.05;   //[s]

                test_code = c_T_cold_solver.test_member_function(t_ts_solved, &diff_t_ts_calculated);
                t_ts_calculated = diff_t_ts_calculated + t_ts_solved;   //[s]
                if (test_code != 0 || t_ts_calculated <= 0.0)
                {
                    *target = std::numeric_limits<double>::quiet_NaN();
                    
                    mpc_csp_solver->reset_time(m_t_ts_initial);
                    
                    return -3;
                }

                if (diff_t_ts_calculated < -0.1 || diff_t_ts_calculated > 0.0)
                {
                    // Set up solver
                    double tol_t_ts_target = 0.1;   //[s]
                    c_T_cold_solver.settings(tol_t_ts_target /*s*/, 50, 0.0, t_ts_max_local, false);

                    C_monotonic_eq_solver::S_xy_pair xy2;
                    xy2.x = t_ts_solved;   //[s]
                    xy2.y = diff_t_ts_calculated;

                    double tol_solved = std::numeric_limits<double>::quiet_NaN();  //[s]
                    int iter_solved = -1;
                    int t_ts_code = 0;
                    try
                    {
                        // Want solver to miss such that solved startup time is greater than components require
                        // If components require slightly more, then they won't completely finish startup and next time period will require
                        //       a very short timestep of startup
                        t_ts_code = c_T_cold_solver.solve(xy1, xy2, -tol_t_ts_target, t_ts_solved, tol_solved, iter_solved);
                    }
                    catch (C_csp_exception)
                    {
                        *target = std::numeric_limits<double>::quiet_NaN();

                        mpc_csp_solver->reset_time(m_t_ts_initial);

                        return -4;
                    }
                    if (t_ts_code != C_monotonic_eq_solver::CONVERGED)
                    {
                        if (t_ts_code > C_monotonic_eq_solver::CONVERGED && fabs(tol_solved) < 0.1*m_t_ts_initial)
                        {
                            std::string msg = util::format("At time = %lg power cycle startup time iteration "
                                " only reached a convergence"
                                "= %lg [s]. Check that results at this timestep are not unreasonably biasing total simulation results",
                                mpc_csp_solver->mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, tol_solved);
                            mpc_csp_solver->mc_csp_messages.add_message(C_csp_messages::NOTICE, msg);
                        }
                        else
                        {
                            *target = std::numeric_limits<double>::quiet_NaN();
                            
                            mpc_csp_solver->reset_time(m_t_ts_initial);

                            return -5;
                        }
                    }
                }
            }
            else
            {
                t_ts_solved = t_ts_max_local;
            }
        }
        else
        {
            return -123;
        }
    }


    // Have been mucking with mc_kernel.mc_sim_info.ms_ts.m_time, so need to reset these
    mpc_csp_solver->mc_kernel.mc_sim_info.ms_ts.m_step = t_ts_solved;						//[s]
    mpc_csp_solver->mc_kernel.mc_sim_info.ms_ts.m_time = mpc_csp_solver->mc_kernel.mc_sim_info.ms_ts.m_time_start + t_ts_solved;		//[s]

    *target = calc_meq_target();

    return 0;
}

int C_csp_solver::C_MEQ__timestep::operator()(double t_ts_guess /*s*/, double *target /*varying*/)
{
    C_MEQ__T_field_cold c_eq(m_solver_mode, mpc_csp_solver, 
        m_q_dot_pc_target,
        m_pc_mode, m_cr_mode,
        m_defocus, t_ts_guess, 
        mpc_csp_solver->m_P_cold_des, mpc_csp_solver->m_x_cold_des);
    C_monotonic_eq_solver c_solver(c_eq);
    
    // Solve for cold temperature
    double T_field_cold_guess_1 = mpc_csp_solver->m_T_htf_cold_des - 273.15;    //[C], convert from [K]

    double diff_T_field_cold = std::numeric_limits<double>::quiet_NaN();
    int T_field_cold_code = -1;

    try
    {
        T_field_cold_code = c_solver.test_member_function(T_field_cold_guess_1, &diff_T_field_cold);
    }
    catch (C_csp_exception)
    {
        return -2;
    }

    if (T_field_cold_code != 0)
    {
        return -3;
    }

    // Check if iteration is required
    if (fabs(diff_T_field_cold) > 1.E-3)
    {
        // Set up solver
        c_solver.settings(1.E-3, 50, mpc_csp_solver->m_T_field_cold_limit, mpc_csp_solver->m_T_field_in_hot_limit, false);

        C_monotonic_eq_solver::S_xy_pair xy1;
        xy1.x = T_field_cold_guess_1;        //[C]
        xy1.y = diff_T_field_cold;           //[-]

        double T_field_cold_guess_2 = std::numeric_limits<double>::quiet_NaN();
        if (diff_T_field_cold > 0.0)
        {
            T_field_cold_guess_2 = T_field_cold_guess_1 + 10.0;		//[C]
        }
        else
        {
            T_field_cold_guess_2 = T_field_cold_guess_1 - 10.0;        //[C]
        }

        double T_field_cold_solved, tol_solved;
        T_field_cold_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
        int iter_solved = -1;

        T_field_cold_code = 0;
        try
        {
            T_field_cold_code = c_solver.solve(xy1, T_field_cold_guess_2, 0.0, T_field_cold_solved, tol_solved, iter_solved);
        }
        catch (C_csp_exception)
        {
            throw(C_csp_exception(util::format("At time = %lg, C_MEQ__timestep failed", mpc_csp_solver->mc_kernel.mc_sim_info.ms_ts.m_time), ""));
        }

        if (T_field_cold_code != C_monotonic_eq_solver::CONVERGED)
        {
            if (T_field_cold_code > C_monotonic_eq_solver::CONVERGED && fabs(tol_solved) < 0.1)
            {
                double abc = 1.23;
                //std::string msg = util::format("At time = %lg C_csp_solver:::solver_pc_fixed__tes_dc failed "
                //    "iteration to find the cold HTF temperature to balance energy between the TES and PC only reached a convergence "
                //    "= %lg. Check that results at this timestep are not unreasonably biasing total simulation results",
                //    mpc_csp_solver->mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, tol_solved);
                //mpc_csp_solver->mc_csp_messages.add_message(C_csp_messages::NOTICE, msg);
            }
            else
            {
                *target = std::numeric_limits<double>::quiet_NaN();
                return -1;
            }
        }
    }

    if (m_step_target_mode == E_STEP_FROM_COMPONENT)
    {
        *target = c_eq.m_t_ts_calc - t_ts_guess;   //[s]
    }
    else if (m_step_target_mode == E_STEP_Q_DOT_PC)
    {
        *target = mpc_csp_solver->mc_pc_out_solver.m_q_dot_htf; //[MWt]
    }
    else if (m_step_target_mode == E_STEP_FIXED)
    {
        *target = 0.0;
    }
    else
    {
        *target = std::numeric_limits<double>::quiet_NaN();
    }

    return 0;
}

void C_csp_solver::C_MEQ__m_dot_tes::init_calc_member_vars()
{
    m_T_field_cold_calc = std::numeric_limits<double>::quiet_NaN();
    m_t_ts_calc = std::numeric_limits<double>::quiet_NaN();
    m_m_dot_pc_in = std::numeric_limits<double>::quiet_NaN();
}

int C_csp_solver::C_MEQ__m_dot_tes::operator()(double f_m_dot_tes /*-*/, double *diff_target /*-*/)
{
    init_calc_member_vars();

    // Set timestep
    mpc_csp_solver->mc_kernel.mc_sim_info.ms_ts.m_step = m_t_ts_in;    //[s]
    mpc_csp_solver->mc_kernel.mc_sim_info.ms_ts.m_time = mpc_csp_solver->mc_kernel.mc_sim_info.ms_ts.m_time_start + m_t_ts_in;		//[s]

    // Solve collector-receiver
    mpc_csp_solver->mc_cr_htf_state_in.m_temp = m_T_field_cold_guess;       //[C]
    mpc_csp_solver->mc_cr_htf_state_in.m_pres = m_P_field_in;	//[kPa]
    mpc_csp_solver->mc_cr_htf_state_in.m_qual = m_x_field_in;	//[-]

    double m_dot_field_out = std::numeric_limits<double>::quiet_NaN();     //[kg/hr]
    double t_ts_cr_su = m_t_ts_in;
    if (m_cr_mode == C_csp_collector_receiver::ON)
    {
        mpc_csp_solver->mc_collector_receiver.on(mpc_csp_solver->mc_weather.ms_outputs,
            mpc_csp_solver->mc_cr_htf_state_in,
            m_defocus,
            mpc_csp_solver->mc_cr_out_solver,
            mpc_csp_solver->mc_kernel.mc_sim_info);

        if (mpc_csp_solver->mc_cr_out_solver.m_m_dot_salt_tot == 0.0 || mpc_csp_solver->mc_cr_out_solver.m_q_thermal == 0.0)
        {
            *diff_target = std::numeric_limits<double>::quiet_NaN();
            return -1;
        }

        m_dot_field_out = mpc_csp_solver->mc_cr_out_solver.m_m_dot_salt_tot;     //[kg/hr]
    }
    else if (m_cr_mode == C_csp_collector_receiver::STARTUP)
    {
        mpc_csp_solver->mc_collector_receiver.startup(mpc_csp_solver->mc_weather.ms_outputs,
            mpc_csp_solver->mc_cr_htf_state_in,
            mpc_csp_solver->mc_cr_out_solver,
            mpc_csp_solver->mc_kernel.mc_sim_info);

        if (mpc_csp_solver->mc_cr_out_solver.m_q_startup == 0.0)
        {
            *diff_target = std::numeric_limits<double>::quiet_NaN();
            return -1;
        }

        if (mpc_csp_solver->m_is_cr_config_recirc)
        {
            m_dot_field_out = 0.0;  //[kg/hr]
        }

        t_ts_cr_su = mpc_csp_solver->mc_cr_out_solver.m_time_required_su;       //[s]
    }
    else if (m_cr_mode == C_csp_collector_receiver::OFF)
    {
        mpc_csp_solver->mc_collector_receiver.off(mpc_csp_solver->mc_weather.ms_outputs,
            mpc_csp_solver->mc_cr_htf_state_in,
            mpc_csp_solver->mc_cr_out_solver,
            mpc_csp_solver->mc_kernel.mc_sim_info);

        if (mpc_csp_solver->m_is_cr_config_recirc)
        {
            m_dot_field_out = 0.0;  //[kg/hr]
        }
    }

    // For now, check the CR return pressure against the assumed constant system interface pressure
    if (fabs((mpc_csp_solver->mc_cr_out_solver.m_P_htf_hot - m_P_field_in) / m_P_field_in) > 0.001 && !mpc_csp_solver->mc_collector_receiver.m_is_sensible_htf)
    {
        std::string msg = util::format("C_csp_solver::solver_cr_to_pc_to_cr(...) The pressure returned from the CR model, %lg [bar],"
            " is different than the assumed constant pressure, %lg [bar]",
            mpc_csp_solver->mc_cr_out_solver.m_P_htf_hot / 100.0, m_P_field_in / 100.0);
        mpc_csp_solver->mc_csp_messages.add_message(C_csp_messages::NOTICE, msg);
    }

    // Choose max pc flow rate based on cycle operating mode
    double m_dot_pc_max = mpc_csp_solver->m_m_dot_pc_max;     //[kg/hr]
    if (m_pc_mode == C_csp_power_cycle::E_csp_power_cycle_modes::STARTUP_CONTROLLED) {
        m_dot_pc_max = mpc_csp_solver->m_m_dot_pc_max_startup;          //[kg/hr]
    }

    // Calculate mass flow to power cycle
    // and actual hot mass flow TES
    // based on solver strategy
    double m_dot_hot_to_tes = std::numeric_limits<double>::quiet_NaN();
    if ( m_solver_mode == E__CR_OUT__CR_OUT_PLUS_TES_EMPTY
        || m_solver_mode == E__CR_OUT__0
        || m_solver_mode == E__CR_OUT__ITER_M_DOT_SU_CH_ONLY || m_solver_mode == E__CR_OUT__ITER_M_DOT_SU_DC_ONLY
        || m_solver_mode == E__CR_OUT__ITER_Q_DOT_TARGET_DC_ONLY || m_solver_mode == E__CR_OUT__ITER_Q_DOT_TARGET_CH_ONLY
        || m_solver_mode == E__CR_OUT__CR_OUT || m_solver_mode == E__CR_OUT__CR_OUT_LESS_TES_FULL)
    {
        if (m_solver_mode == E__CR_OUT__CR_OUT_PLUS_TES_EMPTY)
        {
            double q_dot_dc_est, m_dot_tes_dc, T_tes_dc_est;
            q_dot_dc_est = m_dot_tes_dc = T_tes_dc_est = std::numeric_limits<double>::quiet_NaN();
            mpc_csp_solver->mc_tes.discharge_avail_est(m_T_field_cold_guess, mpc_csp_solver->mc_kernel.mc_sim_info.ms_ts.m_step, q_dot_dc_est, m_dot_tes_dc, T_tes_dc_est);
            m_dot_tes_dc *= 3600.0;     //[kg/hr] convert from kg/s
            m_m_dot_pc_in = fmin(m_dot_pc_max, m_dot_field_out + m_dot_tes_dc);
        }
        else if (m_solver_mode == E__CR_OUT__0)
        {
            m_m_dot_pc_in = 0.0;
        }
        else if (m_solver_mode == C_MEQ__m_dot_tes::E__CR_OUT__ITER_M_DOT_SU_DC_ONLY || m_solver_mode == E__CR_OUT__ITER_Q_DOT_TARGET_DC_ONLY)
        {
            double q_dot_dc_est, m_dot_tes_dc, T_tes_dc_est;
            q_dot_dc_est = m_dot_tes_dc = T_tes_dc_est = std::numeric_limits<double>::quiet_NaN();
            mpc_csp_solver->mc_tes.discharge_avail_est(m_T_field_cold_guess, mpc_csp_solver->mc_kernel.mc_sim_info.ms_ts.m_step, q_dot_dc_est, m_dot_tes_dc, T_tes_dc_est);
            m_dot_tes_dc *= 3600.0;     //[kg/hr] convert from kg/s

            // max: not allowing TES CH, so all field m_dot must go to pc
            // min: can't send more to pc than field + dc
            double m_dot_to_pc_max = fmin(m_dot_pc_max, m_dot_tes_dc + m_dot_field_out);
            m_m_dot_pc_in = m_dot_field_out + fmin(0.99999,f_m_dot_tes)*fmax(0.0, m_dot_to_pc_max - m_dot_field_out);
        }
        else if (m_solver_mode == E__CR_OUT__ITER_M_DOT_SU_CH_ONLY || m_solver_mode == E__CR_OUT__ITER_Q_DOT_TARGET_CH_ONLY)
        {
            double q_dot_ch_est, m_dot_hot_to_tes_est, T_cold_field_est;
            q_dot_ch_est = m_dot_hot_to_tes_est = T_cold_field_est = std::numeric_limits<double>::quiet_NaN();
            mpc_csp_solver->mc_tes.charge_avail_est(mpc_csp_solver->mc_cr_out_solver.m_T_salt_hot + 273.15, mpc_csp_solver->mc_kernel.mc_sim_info.ms_ts.m_step,
                q_dot_ch_est, m_dot_hot_to_tes_est, T_cold_field_est);
            m_dot_hot_to_tes_est *= 3600;       //[kg/hr] convert from kg/s

            // min: not allowing TES DC, so max m_dot to pc is field m_dot
            // max: need to send enough mass flow to pc so TES doesn't overcharge
            double m_dot_to_tes_max = fmin(m_dot_field_out, m_dot_hot_to_tes_est);
            double m_dot_to_tes_min = fmax(m_dot_field_out - m_dot_pc_max, 0.0);
            double m_dot_to_tes = m_dot_to_tes_max - fmin(0.99999,f_m_dot_tes)* fmax(0.0, m_dot_to_tes_max - m_dot_to_tes_min);

            m_m_dot_pc_in = m_dot_field_out - m_dot_to_tes;
        }
        else if (m_solver_mode == E__CR_OUT__CR_OUT)
        {
            m_m_dot_pc_in = m_dot_field_out;   //[kg/hr]
        }
        else if (m_solver_mode == E__CR_OUT__CR_OUT_LESS_TES_FULL)
        {
            double q_dot_ch_est, m_dot_hot_to_tes_est, T_cold_field_est;
            q_dot_ch_est = m_dot_hot_to_tes_est = T_cold_field_est = std::numeric_limits<double>::quiet_NaN();
            mpc_csp_solver->mc_tes.charge_avail_est(mpc_csp_solver->mc_cr_out_solver.m_T_salt_hot + 273.15, mpc_csp_solver->mc_kernel.mc_sim_info.ms_ts.m_step,
                q_dot_ch_est, m_dot_hot_to_tes_est, T_cold_field_est);

            m_dot_hot_to_tes_est *= 3600;       //[kg/hr] convert from kg/s

            m_m_dot_pc_in = std::max(0.0, m_dot_field_out - m_dot_hot_to_tes_est);    //[kg/hr]
        }

        if (m_m_dot_pc_in < 0.0)
        {
            *diff_target = std::numeric_limits<double>::quiet_NaN();
            return -2;
        }
        if (m_m_dot_pc_in > m_dot_pc_max)
        {
            return -11;
        }

        m_dot_hot_to_tes = m_dot_field_out;     //[kg/hr]
    }
    // Mass does NOT balance for these modes, and is balanced upstream by defocus or timestep iteration
    else if(m_solver_mode == E__PC_MAX_PLUS_TES_FULL__PC_MAX || m_solver_mode == E__TO_PC_PLUS_TES_FULL__ITER_M_DOT_SU)
    {
        double q_dot_ch_est, m_dot_hot_to_tes_est, T_cold_field_est;
        q_dot_ch_est = m_dot_hot_to_tes_est = T_cold_field_est = std::numeric_limits<double>::quiet_NaN();
        mpc_csp_solver->mc_tes.charge_avail_est(mpc_csp_solver->mc_cr_out_solver.m_T_salt_hot + 273.15, mpc_csp_solver->mc_kernel.mc_sim_info.ms_ts.m_step,
            q_dot_ch_est, m_dot_hot_to_tes_est, T_cold_field_est); 
        
        m_dot_hot_to_tes_est *= 3600;       //[kg/hr] convert from kg/s

        if (m_solver_mode == E__PC_MAX_PLUS_TES_FULL__PC_MAX)
        {
            m_m_dot_pc_in = m_dot_pc_max;  //[kg/hr]
        }
        else if (m_solver_mode == E__TO_PC_PLUS_TES_FULL__ITER_M_DOT_SU)
        {
            m_m_dot_pc_in = fmin(0.99999, f_m_dot_tes) * m_dot_pc_max;
        }

        if (m_m_dot_pc_in > m_dot_pc_max)
        {
            return -12;
        }

        m_dot_hot_to_tes = m_m_dot_pc_in + m_dot_hot_to_tes_est;     //[kg/s]
    }
    else if (m_solver_mode == E__TO_PC__PC_MAX || m_solver_mode == E__TO_PC__ITER_M_DOT_SU)
    {
        if (m_solver_mode == E__TO_PC__PC_MAX)
        {
            m_m_dot_pc_in = m_dot_pc_max;   //[kg/hr]
        }
        else if (m_solver_mode == E__TO_PC__ITER_M_DOT_SU)
        {
            m_m_dot_pc_in = fmin(0.99999, f_m_dot_tes) * m_dot_pc_max;
        }

        if (m_m_dot_pc_in > m_dot_pc_max)
        {
            return -12;
        }

        m_dot_hot_to_tes = m_m_dot_pc_in;
    }
    else if (m_solver_mode == E__TES_FULL__0)
    {
        double q_dot_ch_est, m_dot_hot_to_tes_est, T_cold_field_est;
        q_dot_ch_est = m_dot_hot_to_tes_est = T_cold_field_est = std::numeric_limits<double>::quiet_NaN();
        mpc_csp_solver->mc_tes.charge_avail_est(mpc_csp_solver->mc_cr_out_solver.m_T_salt_hot + 273.15, mpc_csp_solver->mc_kernel.mc_sim_info.ms_ts.m_step,
            q_dot_ch_est, m_dot_hot_to_tes_est, T_cold_field_est);

        m_dot_hot_to_tes_est *= 3600;       //[kg/hr] convert from kg/s

        m_dot_hot_to_tes = m_dot_hot_to_tes_est;    //[kg/hr]
        m_m_dot_pc_in = 0.0;
    }

    double T_cycle_hot = std::numeric_limits<double>::quiet_NaN();          //[K]
    double T_field_cold_calc = std::numeric_limits<double>::quiet_NaN();    //[K]
    if (mpc_csp_solver->m_is_tes)
    {
        int tes_code = mpc_csp_solver->mc_tes.solve_tes_off_design(mpc_csp_solver->mc_kernel.mc_sim_info.ms_ts.m_step,
            mpc_csp_solver->mc_weather.ms_outputs.m_tdry + 273.15,
            m_dot_hot_to_tes / 3600.0, m_m_dot_pc_in / 3600.0,
            mpc_csp_solver->mc_cr_out_solver.m_T_salt_hot + 273.15, m_T_field_cold_guess + 273.15,
            T_cycle_hot, T_field_cold_calc,
            mpc_csp_solver->mc_tes_outputs);

        if (tes_code != 0)
        {
            *diff_target = std::numeric_limits<double>::quiet_NaN();
            return -3;
        }
    }
    else
    {
        T_cycle_hot = mpc_csp_solver->mc_cr_out_solver.m_T_salt_hot + 273.15;   //[K]
    }

    // Power Cycle
    mpc_csp_solver->mc_pc_htf_state_in.m_temp = T_cycle_hot - 273.15;		//[C], convert from K
    mpc_csp_solver->mc_pc_htf_state_in.m_pres = mpc_csp_solver->mc_cr_out_solver.m_P_htf_hot;	//[kPa]
    mpc_csp_solver->mc_pc_htf_state_in.m_qual = mpc_csp_solver->mc_cr_out_solver.m_xb_htf_hot;	//[-]

    mpc_csp_solver->mc_pc_inputs.m_m_dot = m_m_dot_pc_in;		//[kg/hr] no mass flow rate to power cycle
        // Inputs
    mpc_csp_solver->mc_pc_inputs.m_standby_control = m_pc_mode;
    // Performance Call
    mpc_csp_solver->mc_power_cycle.call(mpc_csp_solver->mc_weather.ms_outputs,
        mpc_csp_solver->mc_pc_htf_state_in,
        mpc_csp_solver->mc_pc_inputs,
        mpc_csp_solver->mc_pc_out_solver,
        mpc_csp_solver->mc_kernel.mc_sim_info);

    // Check that power cycle is producing power and solving without errors
    if (!mpc_csp_solver->mc_pc_out_solver.m_was_method_successful && mpc_csp_solver->mc_pc_inputs.m_standby_control == C_csp_power_cycle::ON)
    {
        *diff_target = std::numeric_limits<double>::quiet_NaN();
        return -2;
    }

    double t_ts_pc_su = m_t_ts_in;      //[s]
    if (m_pc_mode == C_csp_power_cycle::STARTUP || m_pc_mode == C_csp_power_cycle::STARTUP_CONTROLLED)
    {
        t_ts_pc_su = mpc_csp_solver->mc_pc_out_solver.m_time_required_max;  //[s]
    }

    if (mpc_csp_solver->m_is_tes)
    {
        int tes_code = mpc_csp_solver->mc_tes.solve_tes_off_design(mpc_csp_solver->mc_kernel.mc_sim_info.ms_ts.m_step,
            mpc_csp_solver->mc_weather.ms_outputs.m_tdry + 273.15,
            m_dot_hot_to_tes / 3600.0, m_m_dot_pc_in / 3600.0,
            mpc_csp_solver->mc_cr_out_solver.m_T_salt_hot + 273.15, mpc_csp_solver->mc_pc_out_solver.m_T_htf_cold + 273.15,
            T_cycle_hot, T_field_cold_calc,
            mpc_csp_solver->mc_tes_outputs);

        if (tes_code != 0)
        {
            *diff_target = std::numeric_limits<double>::quiet_NaN();
            return -3;
        }
    }
    else
    {
        T_field_cold_calc = mpc_csp_solver->mc_pc_out_solver.m_T_htf_cold + 273.15; //[K]
    }

    m_T_field_cold_calc = T_field_cold_calc - 273.15;   //[C]

    if (m_cr_mode == C_csp_collector_receiver::STARTUP &&
        (m_pc_mode == C_csp_power_cycle::STARTUP || m_pc_mode == C_csp_power_cycle::STARTUP_CONTROLLED))
    {
        m_t_ts_calc = (std::min)(t_ts_pc_su, t_ts_cr_su);   //[s]
    }
    else if (m_cr_mode == C_csp_collector_receiver::STARTUP)
    {
        m_t_ts_calc = t_ts_cr_su;       //[s]
    }
    else if (m_pc_mode == C_csp_power_cycle::STARTUP || m_pc_mode == C_csp_power_cycle::STARTUP_CONTROLLED)
    {
        m_t_ts_calc = t_ts_pc_su;       //[s]
    }
    
    if (m_solver_mode == E__TO_PC_PLUS_TES_FULL__ITER_M_DOT_SU
        || m_solver_mode == E__CR_OUT__ITER_M_DOT_SU_CH_ONLY || m_solver_mode == E__CR_OUT__ITER_M_DOT_SU_DC_ONLY
        || m_solver_mode == E__TO_PC__ITER_M_DOT_SU)
    {
        *diff_target = (m_m_dot_pc_in - mpc_csp_solver->mc_pc_out_solver.m_m_dot_htf) / mpc_csp_solver->mc_pc_out_solver.m_m_dot_htf;
    }
    else if (m_solver_mode == E__CR_OUT__ITER_Q_DOT_TARGET_DC_ONLY || m_solver_mode == E__CR_OUT__ITER_Q_DOT_TARGET_CH_ONLY)
    {
        *diff_target = (mpc_csp_solver->mc_pc_out_solver.m_q_dot_htf - m_q_dot_pc_target) / m_q_dot_pc_target;
    }

    return 0;
}

void C_csp_solver::C_MEQ__T_field_cold::init_calc_member_vars()
{
    m_t_ts_calc = std::numeric_limits<double>::quiet_NaN();
}

int C_csp_solver::C_MEQ__T_field_cold::operator()(double T_field_cold /*C*/, double *diff_T_field_cold /*-*/)
{
    init_calc_member_vars();

    C_MEQ__m_dot_tes c_eq(m_solver_mode, mpc_csp_solver, 
        m_pc_mode, m_cr_mode,
        m_q_dot_pc_target,
        m_defocus, m_t_ts_in,
        m_P_field_in, m_x_field_in, T_field_cold);

    C_monotonic_eq_solver c_solver(c_eq);

    if (m_solver_mode == C_MEQ__m_dot_tes::E__CR_OUT__ITER_M_DOT_SU_DC_ONLY ||
        m_solver_mode == C_MEQ__m_dot_tes::E__CR_OUT__ITER_Q_DOT_TARGET_DC_ONLY ||
        m_solver_mode == C_MEQ__m_dot_tes::E__CR_OUT__ITER_M_DOT_SU_CH_ONLY ||
        m_solver_mode == C_MEQ__m_dot_tes::E__CR_OUT__ITER_Q_DOT_TARGET_CH_ONLY ||
        m_solver_mode == C_MEQ__m_dot_tes::E__TO_PC_PLUS_TES_FULL__ITER_M_DOT_SU ||
        m_solver_mode == C_MEQ__m_dot_tes::E__TO_PC__ITER_M_DOT_SU)
    {
        double diff_m_dot = std::numeric_limits<double>::quiet_NaN();

        // Fraction = 0 means no interaction with storage
        // Fraction = 1 means 'maximum' interaction with storage
        double f_m_dot_guess_1 = 1.0;
        int f_m_dot_code = c_solver.test_member_function(f_m_dot_guess_1, &diff_m_dot);
        if (f_m_dot_code != 0)
        {
            return -1;
        }

        // Can't hit target thermal power with max mass flow rate
        // But mode 'E_PC_OUT_TARGET__TES_CONTINUOUS' should balance mass and energy
        if ((m_solver_mode == C_MEQ__m_dot_tes::E__CR_OUT__ITER_Q_DOT_TARGET_CH_ONLY || m_solver_mode == C_MEQ__m_dot_tes::E__CR_OUT__ITER_Q_DOT_TARGET_DC_ONLY)
            && diff_m_dot < 0.0)
        {
            m_t_ts_calc = c_eq.m_t_ts_calc;

            double T_field_cold_calc = c_eq.m_T_field_cold_calc;        //[C]
            *diff_T_field_cold = (T_field_cold_calc - T_field_cold) / T_field_cold; //[-]

            return 0;
        }
        else if(diff_m_dot < -1.E-3)
        {
            return -4;
        }

        if (fabs(diff_m_dot) > 1.E-3)
        {
            C_monotonic_eq_solver::S_xy_pair xy1;
            xy1.x = f_m_dot_guess_1;        //[-]
            xy1.y = diff_m_dot;             //[-]

            // Use difference from guess 1 to generate a new guess
            double f_m_dot_guess_2 = 1.0 / (1.0 + diff_m_dot);      //[-]

            c_solver.settings(1.E-3, 50, 0.0, 1.0, false);

            double f_m_dot_solved, tol_solved;
            f_m_dot_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
            int iter_solved = -1;
            f_m_dot_code = -1;

            try
            {
                f_m_dot_code = c_solver.solve(xy1, f_m_dot_guess_2, 0.0, f_m_dot_solved, tol_solved, iter_solved);
            }
            catch (C_csp_exception)
            {
                return -2;
            }

            if (f_m_dot_code != C_monotonic_eq_solver::CONVERGED)
            {
                if (f_m_dot_code > C_monotonic_eq_solver::CONVERGED && fabs(tol_solved) < 0.1)
                {
                    std::string msg = util::format("At time = %lg power cycle mass flow for startup "
                        "iteration to find a defocus resulting in the maximum power cycle mass flow rate only reached a convergence "
                        "= %lg. Check that results at this timestep are not unreasonably biasing total simulation results",
                        mpc_csp_solver->mc_kernel.mc_sim_info.ms_ts.m_time / 3600.0, tol_solved);
                    mpc_csp_solver->mc_csp_messages.add_message(C_csp_messages::NOTICE, msg);
                }
                else
                {
                    return -3;
                }
            }
        }
    }
    else
    {
        double m_dot_tes = std::numeric_limits<double>::quiet_NaN();
        double y_diff_target = std::numeric_limits<double>::quiet_NaN();

        int m_dot_err = c_solver.test_member_function(m_dot_tes, &y_diff_target);
        if (m_dot_err != 0)
        {
            *diff_T_field_cold = std::numeric_limits<double>::quiet_NaN();
            return m_dot_err;
        }        
    }

    m_t_ts_calc = c_eq.m_t_ts_calc;

    double T_field_cold_calc = c_eq.m_T_field_cold_calc;        //[C]
    *diff_T_field_cold = (T_field_cold_calc - T_field_cold) / T_field_cold; //[-]

    return 0;
}
