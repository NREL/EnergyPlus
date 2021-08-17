// -*- lsst-c++ -*-

//
// Created by jackcook on 7/11/20.
//

#include <thread>
#include <chrono>
#include <LU-Decomposition/lu.h>
#include <blas/blas.h>
#include <cpgfunction/gfunction.h>
#include <cpgfunction/interpolation.h>
#include <cpgfunction/segments.h>
#include <cpgfunction/heat_transfer.h>


using namespace std;  // lots of vectors, only namespace to be used

namespace gt::gfunction {
    // The uniform borehole wall temperature (UBWHT) g-function calculation.
    // Originally presented in Cimmino and Bernier (2014) and a later paper on
    // speed improvements by Cimmino (2018)
    vector<double> uniform_borehole_wall_temperature(
            vector<gt::boreholes::Borehole> &boreField, vector<double> &time,
            double alpha, int nSegments, bool use_similarities, int n_Threads,
            bool display){
        vector<double> gFunction(time.size());

        if (display) {
            std::cout << "---------------------------------------------------"
                         "---------" << std::endl;
            std::cout << "Calculating g-function for uniform borehole wall "
                         "temperature" << std::endl;
            std::cout << "---------------------------------------------------"
                         "---------" << std::endl;
        }
        auto startall = std::chrono::steady_clock::now();

        if (display) {
            cout << "\tMaking use of " << n_Threads << " threads." << endl;
        }

        // Number of boreholes
        int nbh = boreField.size();
        // Total number of line sources
        int nSources = nSegments * nbh;
        // Number of time values
        int nt = time.size();

        auto sum_to_n = [](const int n) {
            return n * (n + 1) / 2;
        };
        int nSum = sum_to_n(nSources);

        // Segment Response struct
        gt::segments::SegmentResponse SegRes(nSources, nSum, nt);

        // Split boreholes into segments
        _borehole_segments(SegRes.boreSegments, boreField, nSegments);

        // Calculate segment to segment thermal response factors
        auto start = std::chrono::steady_clock::now();
        gt::heat_transfer::thermal_response_factors(SegRes,
                                                    time,
                                                    alpha,
                                                    use_similarities,
                                                    display);
        auto end = std::chrono::steady_clock::now();

        if (display) {
            std::cout << "Building and solving system of equations ..."
                << std::endl;
        }
        // ---------------------------------------------------------------------
        // Build a system of equation [A]*[X] = [B] for the evaluation of the
        // g-function. [A] is a coefficient matrix, [X] = [Qb,Tb] is a state
        // space vector of the borehole heat extraction rates and borehole wall
        // temperature (equal for all segments), [B] is a coefficient vector.
        // ---------------------------------------------------------------------

        // -------- timings for debug
        double milli = 0;
        double segment_length_time = 0;
        double time_vector_time = 0;
        double segment_h_values_time = 0;
        double fill_A_time = 0;
        double load_history_reconstruction_time = 0;
        double temporal_superposition_time = 0;
        double fill_gsl_matrices_time = 0;
        double LU_decomposition_time = 0;

        auto start2 = std::chrono::steady_clock::now();

        // ------ Segment lengths -------
        start = std::chrono::steady_clock::now();
        std::vector<float> Hb(nSources);
        # pragma omp parallel for num_threads(n_Threads)
        for (int b=0; b<nSources; b++) {
            Hb[b] = SegRes.boreSegments[b].H;
        } // next b

        end = std::chrono::steady_clock::now();
        milli = chrono::duration_cast<chrono::milliseconds>
                (end - start).count();
        segment_length_time += milli;

        // ------ time vectors ---------
        start = std::chrono::steady_clock::now();
        // create new time vector that starts at 0
        std::vector<double> _time_untouched(time.size()+1);
        std::vector<double> _time(time.size()+1);
        std::vector<double> dt(time.size()+1);

//        auto _fill_time = [&_time, &time, &dt, &_time_untouched]() {
//            for (int i=0; i<_time.size(); i++) {
//                if (i==0) {
//                    _time[0] = 0;
//                    _time_untouched[0] = 0;
//                    dt[i] = time[i];
//                } else {
//                    _time[i] = time[i-1];
//                    _time_untouched[i] = time[i-1];
//                    dt[i] = time[i] - time[i-1];
//                } // fi
//            } // next i
//        }; // auto _fill_time
//        if (multi_thread) {
//            boost::asio::post(pool, [&_fill_time]{ _fill_time() ;});
//        } else {
//            _fill_time();
//        }  // if (multi_thread);
        # pragma omp parallel for num_threads(n_Threads)
        for (int i=0; i<_time.size(); i++) {
            if (i==0) {
                _time[0] = 0;
                _time_untouched[0] = 0;
                dt[i] = time[i];
            } else {
                _time[i] = time[i-1];
                _time_untouched[i] = time[i-1];
                if (i < time.size()) {
                    dt[i] = time[i] - time[i-1];
                } else if (i == time.size()) {
                    dt[i] = time[i-1] - time[i-2];
                }
            } // fi
        } // next i

        end = std::chrono::steady_clock::now();
        milli = chrono::duration_cast<chrono::milliseconds>
                (end - start).count();
        time_vector_time += milli;

        // ---------- segment h values -------------
        /** Starting up pool2 here **/
        // Launch the pool with n threads.
        auto tic = std::chrono::steady_clock::now();
        auto toc = std::chrono::steady_clock::now();
        if (display) {
            double milli = chrono::duration_cast<chrono::milliseconds>
                    (tic - toc).count();
            double seconds = milli;
            std::cout << "Time to open a pool : "
                      << seconds
                      << " sec" << std::endl;
        }

        start = std::chrono::steady_clock::now();

        end = chrono::steady_clock::now();
        milli = chrono::duration_cast<chrono::milliseconds>
                (end - start).count();
        segment_h_values_time += milli;

        // after interpolation scheme, get rid of h_ij first
        // Initialize segment heat extraction rates
        vector<vector<double> > Q(nSources, vector<double> (nt));

        // Define A and b for utitilizing Ax=b
        /**
         * A = [    [   ],
         *          [hb[0:len(hb), 0]
         *     ]
         * b = [    [  ],
         *          [sum(hb)]
         *     ]
         * **/

        int SIZE = nSources + 1;
        vector<vector<double> > A(SIZE, vector<double> (SIZE, 0));
        vector<double> B (SIZE, 0);

        // Fill A
        int n = SIZE - 1;
        double Hb_sum=0;
        for (auto & _hb : Hb) {
            Hb_sum += _hb;
        }

        // Build and solve the system of equations at all times

        // the loop p=n depends on what occured at p=n-1, so this will be be in
        // series however threading will be interspersed throughout to make use
        // of as many threads as possible
        std::vector<double> Tb_0 (nSources);
        // Restructured load history
        // create interpolation object for accumulated heat extraction
        std::vector<std::vector<double>>
            q_reconstructed (nSources, std::vector<double> (nt));
        std::vector<double> q_r(nSources * nt, 0);

        int gauss_sum = nSources * (nSources + 1) / 2;
        std::vector<double> H_ij(gauss_sum * nt, 0);  // 1D nSources x nt
        int idx;
        for (int i=0; i<nt; i++) {
            for (int j=0; j<gauss_sum; j++) {
                idx = (i * gauss_sum) + j;
                H_ij[idx] = SegRes.h_ij[j][i];
            }  // next j
        }  // next i

        for (int p=0; p<nt; p++) {
            // ------------- fill A ------------
            start = std::chrono::steady_clock::now();
            auto _fillA = [&Hb, &dt, &time, &SegRes, &A]
                    (int i, int p, int SIZE) {
                double xp;
                double yp;
                int n = SIZE - 1;
                for (int j=0; j<SIZE; j++) {
                    if (i == n) { // then we are referring to Hb
                        if (j==n) {
                            A[i][n] = 0;
                        } else {
                            A[i][j] = Hb[j];
                        } // fi
                    } else {
                        if (j==SIZE-1) {
                            A[i][j] = -1;
                        } else {
                            xp = dt[p];
                            jcc::interpolation::interp1d(xp, yp,
                                                         time, SegRes,
                                                         i, j, p);
                            A[i][j] = yp;
                        } // fi
                    } // fi
                } // next k
            };
            // A needs filled each loop because the _gsl partial pivot
            // decomposition modifies the matrix
            // TODO: Look into reducing the number of times A is built given that Eigen is now being used
            # pragma omp parallel for num_threads(n_Threads)
            for (int i=0; i<SIZE; i++) {
                _fillA(i, p, SIZE);
            }  // next i
            end = std::chrono::steady_clock::now();  // _fill_A
            milli = chrono::duration_cast<chrono::milliseconds>
                    (end - start).count();
            fill_A_time += milli;

            // ----- load history reconstruction -------
            start = std::chrono::steady_clock::now();
            load_history_reconstruction(q_r,time, _time, Q, dt, p);
            end = std::chrono::steady_clock::now();
            milli = chrono::duration_cast<chrono::milliseconds>
                    (end - start).count();
            load_history_reconstruction_time += milli;

            // ----- temporal superposition
            start = chrono::steady_clock::now();
            _temporal_superposition(Tb_0,
                                    SegRes,
                                    H_ij,
                                    q_r,
                                    p,
                                    nSources);
            // fill b with -Tb
            B[SIZE-1] = Hb_sum;
            for (int i=0; i<Tb_0.size(); i++) {
                B[i] = -Tb_0[i];
            }
            end = chrono::steady_clock::now();
            milli = chrono::duration_cast<chrono::milliseconds>
                    (end - start).count();
            temporal_superposition_time += milli;

            int m = SIZE;
            int n = SIZE;
            vector<double> x(SIZE);
//            _solve_eqn(x, A, b);
            /** was _solve_eqn **/

            // ---- fill gsl matrix A and b -----
            start = std::chrono::steady_clock::now();

            end = std::chrono::steady_clock::now();
            milli = chrono::duration_cast<chrono::milliseconds>
                    (end - start).count();
            fill_gsl_matrices_time += milli;

            // ----- LU decomposition -----
            start = std::chrono::steady_clock::now();
            vector<int> indx(SIZE, 0);
            double d;
            jcc::decomposition(A, SIZE, indx, d);
            jcc::back_substitution(A, SIZE, indx, B);

//            for (int i=0; i<SIZE; i++) {
//                x[i] = X(i, 0);
//            } // next i
            end = chrono::steady_clock::now();
            milli = chrono::duration_cast<chrono::milliseconds>
                    (end - start).count();
            LU_decomposition_time += milli;

            // ---- Save Q's for next p ---
            for (int j=0; j<Q.size(); j++) {
                Q[j][p] = B[j];
            } // next j
            // the borehole wall temperatures are equal for all segments
            double Tb = B[x.size()-1];
            gFunction[p] = Tb;
        } // next p
        segment_length_time /= 1000;
        time_vector_time /= 1000;
        segment_h_values_time /= 1000;
        segment_length_time /= 1000;
        fill_A_time /= 1000;
        load_history_reconstruction_time /= 1000;
        temporal_superposition_time /= 1000;
        fill_gsl_matrices_time /= 1000;
        LU_decomposition_time /= 1000;

        if (display) {
            cout << "------ timings report -------" << endl;
            cout << " t\t " << " t/p\t" << "name" << endl;
            cout << segment_length_time << "\t" << segment_length_time << "\t" << "segment length time" << endl;
            cout << time_vector_time << "\t" << time_vector_time << "\t" << "time vector time" << endl;
            cout << segment_h_values_time << "\t" << segment_h_values_time << "\t" << "segment h values time" << endl;
            cout << fill_A_time << "\t" << fill_A_time / double(nt) << "\t" << "time to fill vector A" << endl;
            cout << load_history_reconstruction_time << "\t" << load_history_reconstruction_time / double(nt)
                 << "\t" << "load hist reconstruction" << endl;
            cout << temporal_superposition_time << "\t" << temporal_superposition_time / double(nt)
                 << "\t" << "temporal superposition time:" << endl;
            cout << fill_gsl_matrices_time << "\t" << fill_gsl_matrices_time / double(nt)
                 << "\t" << "gsl fill matrices time" << endl;
            cout << LU_decomposition_time << "\t" << LU_decomposition_time/double(nt)
                 << "\t" << "LU decomp time" << endl;
        }

        auto end2 = std::chrono::steady_clock::now();
        if (display) {
            double milli1 = chrono::duration_cast<chrono::milliseconds>
                    (end2 - start2).count();
            double seconds1 = milli1 / 1000;
            double milli2 = chrono::duration_cast<chrono::milliseconds>
                    (end2 - startall).count();
            double seconds2 = milli2 / 1000;
            std::cout << "Elapsed time in seconds : "
                      << seconds1
                      << " sec" << std::endl;
            std::cout << "Total time for g-function evaluation : "
                      << seconds2
                      << " sec" << std::endl;
        }

        return gFunction;
    }  // uniform_borehole_wall_temperature();

    void _borehole_segments(std::vector<gt::boreholes::Borehole>& boreSegments,
            std::vector<gt::boreholes::Borehole>& boreholes, const int nSegments) {
        double H;
        double D;
        int count = 0;
        // Split boreholes into segments
        for(auto& b : boreholes) {
            // TODO: maybe thread this later on
            for (int i=0; i<nSegments; i++) {
                H = b.H / double(nSegments);
                D = b.D + double(i) * b.H / double(nSegments);
                boreSegments[count] = gt::boreholes::Borehole(H, D, b.r_b, b.x, b.y);
                count++;
            }  // end for
        } // end for
    } // void _borehole_segments

    void load_history_reconstruction(std::vector<double>& q_reconstructed,
            vector<double>& time, vector<double>& _time,
            vector<vector<double> >& Q, vector<double>& dt, const int p) {
        // for s in range p+1
        int nSources = Q.size();

        // Inverted time steps
        std::vector<double> dt_reconstructed (p+1);
        for (int i=p; i>=0; i--) {
            dt_reconstructed[p-i] = dt[i];  // reverse the dt
        }
        // t_restructured is [0, cumsum(dt_reversed)]
        std::vector<double> t_reconstructed(p+2);  // will start at 0
        for (int i=1; i<=p+1; i++) {
            t_reconstructed[i] = dt_reconstructed[i-1];
        }
        for (int i=1; i<=p+1; i++) {
            t_reconstructed[i] = t_reconstructed[i] + t_reconstructed[i-1];
        }
         // local time vector
        std::vector<double> t(p+3);
        for (int i=0; i<t.size(); i++) {
            if (i==t.size()-1) {
                t[i] = _time[i-1] + _time[1];
            } else {
                t[i] = _time[i];
            }
        }
        int _tsize = t.size();
        // Q*dt
        vector<vector <double>> Q_dt (nSources,
                                      std::vector<double> (t.size()));
        auto _Q_dot_dt = [&Q_dt, &Q, &dt, &p, &_tsize, &t](const int i) {
            for (int j = 1; j<_tsize; j++) {
                if (j>=p+1) {
                    Q_dt[i][j] = Q_dt[i][j-1];
                } else {
                    Q_dt[i][j] = Q[i][j-1] * dt[j-1] + Q_dt[i][j-1];
                }  // fi
            } // next j
        };
        for (int i=0; i<nSources; i++) {
            _Q_dot_dt(i);  // could be threaded here, if timings ever prove necessary
        } // next i

        auto _interpolate = [&Q_dt, &q_reconstructed, &t, &t_reconstructed,
                             &dt_reconstructed, &p, &nSources](const int i) {
            int n = t.size();
            std::vector<double> y(n);
            for (int j=0; j<n; j++) {
                y[j] = Q_dt[i][j];
            }
            int n2 = t_reconstructed.size();
            std::vector<double> yp(n2);
            jcc::interpolation::interp1d(t_reconstructed, yp, t, y);

            int idx;
            for (int j=0; j<p+1; j++) {
                double c = yp[j];
                double d = yp[j+1];
                double e = dt_reconstructed[j];
                idx = (j * nSources) + i;
                q_reconstructed[idx] = (d - c) / e;
            }
        }; // _interpolate
        for (int i=0; i<nSources; i++) {
            _interpolate(i); // could be threaded here, but this function doesn't take long at all
        }
    } // load_history_reconstruction

    void _temporal_superposition(vector<double>& Tb_0,
                                 gt::segments::SegmentResponse &SegRes,
                                 vector<double> &h_ij,
                                 vector<double> &q_reconstructed,
                                 const int p, int &nSources)
            {
        // This function performs equation (37) of Cimmino (2017)
        std::fill(Tb_0.begin(), Tb_0.end(), 0);
        // Number of time steps
        int nt = p + 1;

        const auto processor_count = thread::hardware_concurrency();
        int n_threads = int(processor_count);

        int gauss_sum = nSources * (nSources + 1) / 2;  // Number of positions in packed symmetric matrix
        // Storage of h_ij differences
        std::vector<double> dh_ij(gauss_sum, 0);
        int begin_1;  // integer declarations for where the linear algebra will begin
        int begin_2;
        int begin_q;  // time for q_reconstructed to begin

        double alpha = 1;
        double alpha_n = -1;

        std::vector<double>::iterator begin_it_1;
        std::vector<double>::iterator end_it_1;

        for (int k = 0; k < nt; k++) {
            begin_1 = k * gauss_sum;
            begin_it_1 = h_ij.begin() + begin_1;
            end_it_1 = h_ij.begin() + begin_1 + gauss_sum;
            if (k==0){
                // dh_ij = h(k)
                std::copy(begin_it_1, end_it_1, dh_ij.begin());
            } else {
                begin_2 = (k-1) * gauss_sum;
                // h_1 -> dh_ij
                std::copy(begin_it_1, end_it_1, dh_ij.begin());
                // dh_ij = -1 * h(k) + h(k-1)
                jcc::blas::axpy(gauss_sum, alpha_n, h_ij, dh_ij, begin_2,
                                n_threads);
            }
            // q_reconstructed(t_k - t_k')
            begin_q = (nt - k - 1) * nSources;
            // dh_ij is a lower triangular packed matrix
            // Tb_0 = 1 * dh_ij * q(t_k-t_k') + 1 * Tb_0
            jcc::blas::spmv(nSources, alpha, dh_ij, q_reconstructed, alpha,
                            Tb_0, begin_q, n_threads);
        }  // next k
    }  // _temporal_superposition();
} // namespace gt::gfunction
