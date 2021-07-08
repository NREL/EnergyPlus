// -*- lsst-c++ -*-

//
// Created by jackcook on 7/11/20.
//

#include <iostream>
#include <vector>
#include <cpgfunction/boreholes.h>
#include <cpgfunction/heat_transfer.h>

using namespace std;

#ifndef CPPGFUNCTION_GFUNCTION_H
#define CPPGFUNCTION_GFUNCTION_H

/** The functions will be listed here in the order which they appear in the associated cpp file **/

namespace gt {
namespace gfunction {
    /**
     * Uniform borehole wall temperature (UBWHT) g-function calculation method
     *
     * This function superimposes the finite line source (FLS) solution to estimate the g-function of a
     * geothermal bore field. Each borehole is modeled as a series of finite line source segments, as proposed
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
    vector<double> uniform_borehole_wall_temperature(
            vector<gt::boreholes::Borehole> &boreField,
            vector<double> &time, double alpha, int nSegments=12,
            bool use_similarities=true, bool adaptive=true, int n_Threads=1,
            bool multi_thread=true, bool display=false);

    void _borehole_segments(vector<gt::boreholes::Borehole>& boreSegments,
                            vector<gt::boreholes::Borehole>& boreholes, int nSegments);
    void load_history_reconstruction(vector<double>& q_reconstructed, vector<double>& time,
                                     vector<double>& _time, vector<vector<double> >& Q,
                                     vector<double>& dt, const int p);
    void _temporal_superposition(vector<double>& Tb_0, gt::heat_transfer::SegmentResponse &SegRes,
                                 vector<double> &h_ij, vector<double> &q_reconstructed, int p, int &nSources);
    void _solve_eqn(vector<double>& x, vector<vector<double>>& A, vector<double>& b);

}  // namespace gfunction
}  // namespace gt

#endif //CPPGFUNCTION_GFUNCTION_H
