// -*- lsst-c++ -*-

//
// Created by jackcook on 7/11/20.
//

#ifndef CPPGFUNCTION_GFUNCTION_H
#define CPPGFUNCTION_GFUNCTION_H

#include <cpgfunction/boreholes.h>
#include <cpgfunction/segments.h>
#include <iostream>
#include <thread>
#include <vector>

/** The functions will be listed here in the order which they appear in the
 * associated cpp file **/

namespace gt::gfunction {
    /**
     * Uniform borehole wall temperature (UBWHT) g-function calculation method
     *
     * This function superimposes the finite line source (FLS) solution to
     * estimate the g-function of a geothermal bore field. Each borehole is
     * modeled as a series of finite line source segments, as proposed
     * in [CITE: CimminoBernier2014].
     *
     * @param gfunction
     * @param boreholes
     * @param time
     * @param alpha
     * @param nSegments
     * @param use_similarities
     * @param disp
     */
    std::vector<double> uniform_borehole_wall_temperature(
            std::vector<gt::boreholes::Borehole> &boreField,
            std::vector<double> &time, double alpha, int nSegments=12,
            bool use_similarities=true,
            int n_Threads=1,
            bool display=false);

    void _borehole_segments(std::vector<gt::boreholes::Borehole>& boreSegments,
                            std::vector<gt::boreholes::Borehole>& boreholes,
                            int nSegments);
    void load_history_reconstruction(std::vector<double>& q_reconstructed,
                                     std::vector<double>& time,
                                     std::vector<double>& _time,
                                     std::vector<std::vector<double> >& Q,
                                     std::vector<double>& dt, int p);
    void _temporal_superposition(std::vector<double>& Tb_0,
                                 gt::segments::SegmentResponse &SegRes,
                                 std::vector<double> &h_ij,
                                 std::vector<double> &q_reconstructed, int p,
                                 int &nSources, int numThreads);
    void _solve_eqn(std::vector<double>& x, std::vector<std::vector<double>>& A,
                    std::vector<double>& b);

}  // namespace gt::gfunction

#endif //CPPGFUNCTION_GFUNCTION_H
