//
// Created by jackcook on 7/11/20.
//

#include <cpgfunction/heat_transfer.h>
#include <stdexcept>
#include <thread>
#include <cpgfunction/boreholes.h>
#include <cmath>
#include <qdt.h>

using namespace gt;
using namespace std;

namespace gt::heat_transfer {

    double finite_line_source(const double time_, const double alpha,
                              boreholes::Borehole &b1, boreholes::Borehole &b2,
                              bool reaSource, bool imgSource) {

        auto _Ils = [&b1, &b2, reaSource, imgSource](const double s) {
            auto _erfint = [](const double x) {
                return x * std::erf(x) - (1 / sqrt(M_PI)) * (1 - exp(-pow(x, 2)));
            };
            double r = b1.distance(b2);
            double func = 0.;
            // function to integrate
            if (reaSource) {
                // Real part of the FLS solution
                func += _erfint(double(b2.D - b1.D + b2.H) * s);
                func += -_erfint(double(b2.D - b1.D) * s);
                func += _erfint(double(b2.D - b1.D - b1.H) * s);
                func += -_erfint(double(b2.D - b1.D + b2.H - b1.H) * s);
            } // fi reaSource
            if (imgSource) {
                // Image part of the FLS solution
                func += _erfint(double(b2.D + b1.D + b2.H) * s);
                func += -_erfint(double(b2.D + b1.D) * s);
                func += _erfint(double(b2.D + b1.D + b1.H) * s);
                func += -_erfint(double(b2.D + b1.D + b2.H + b1.H) * s);
            } // fi imgSource
            double a = 0.5 / (b2.H * pow(s, 2)) * func * exp(-pow(r, 2) * pow(s, 2));
            return a;
        }; // auto _Ils

        // lower bound of integration
        double a = double(1.) / sqrt(double(4.) * alpha * time_);
        // Evaluate the integral using Gauss-Kronrod
        double result;
        auto method = qdt::adaptive(qdt::gauss_kronrod());
        result = method.integrate(_Ils, a, qdt::INF);

        return result;
    } // void finite_line_source

    void thermal_response_factors(gt::segments::SegmentResponse &SegRes,
                                  vector<double> &time, const double alpha,
                                  bool use_similaries, bool disp) {
        // total number of line sources
        int nSources = SegRes.boreSegments.size();
        // number of time values
        int nt = time.size();

        // Open up processes here
        // Create a vector of threads
        //may return 0 when not able to detect
        const auto processor_count = thread::hardware_concurrency();
        if (disp) {
            cout << "\tDetected " << processor_count
            << " as the number of available threads" << endl;
        }

        gt::boreholes::SimilaritiesType SimReal; // positive
        gt::boreholes::SimilaritiesType SimImage; // negative

        auto sum_to_n = [](const int n) {
            return n * (n + 1) / 2;
        };
        if (use_similaries) {
            auto start = std::chrono::steady_clock::now();
            // Calculations with similarities
            if (disp) {
                cout << "Identifying similarities..." << endl;
            }
            bool splitRealAndImage = true;
            double disTol = 0.1;
            double tol = 1.0e-6;
            gt::boreholes::Similarity sim;
            sim.similarities(SimReal, SimImage, SegRes.boreSegments,
                             splitRealAndImage, disTol, tol);

            // ---
            // Adaptive hashing scheme if statement
            // Determine the Segment Response storing mode here
            int Ntot = sum_to_n(nSources);

            // lambda function for calculating h at each time step
            auto _calculate_h = [&SegRes, &splitRealAndImage, &time, &alpha,
                                 &nt](boreholes::SimilaritiesType &SimReal,
                                         int s, bool reaSource,
                                         bool imgSource) {
                // begin function
                int n1;
                int n2;
                gt::boreholes::Borehole b1;
                gt::boreholes::Borehole b2;
                // begin thread
                n1 = get<0>(SimReal.Sim[s][0]);
                n2 = get<1>(SimReal.Sim[s][0]);
                b1 = SegRes.boreSegments[n1];
                b2 = SegRes.boreSegments[n2];
                vector<double> hPos(nt);
                if (splitRealAndImage) {
                    for (int k=0; k<nt; k++) {
                        hPos[k] = finite_line_source(time[k], alpha, b1,
                                                     b2, reaSource, imgSource);
                    }  // next k
                    int i;
                    int j;
                    if (SegRes.storage_mode==1) {
                        // will loop through every (i, j), will combine real+image
                        int index;
                        for (std::size_t k=0; k<SimReal.Sim[s].size(); k++) {
                            i = get<0>(SimReal.Sim[s][k]);
                            j = get<1>(SimReal.Sim[s][k]);
                            for (std::size_t t=0; t<time.size(); t++){
                                // must consider real and image source separate
                                // when combining
                                if (i <= j) {
                                    // we want to store n2, n1
                                    SegRes.get_index_value(index, i, j);
                                    // non-critical race condition
                                    SegRes.h_ij[index][t] += b2.H / b1.H * hPos[t];
                                } else {
                                    SegRes.get_index_value(index, j, i);
                                    // non-critical race condition
                                    SegRes.h_ij[index][t] += hPos[t];
                                }  // else ()
                            }  // next t
                        }  // next k
                    }  // else if(SegRes.storage_mode==1)
                } else {
                    throw std::invalid_argument( "Currently, the only "
                                                 "Segment Response storage"
                                                 "mode available is 1. This"
                                                 "corresponds to a packed "
                                                 "matrix.");
                }
            };
            auto end = std::chrono::steady_clock::now();
            if (disp) {
                auto milli = chrono::duration_cast<
                        chrono::milliseconds>(end - start).count();
                double seconds = double(milli) / 1000;
                std::cout << "Elapsed time in seconds : " << seconds
                << " sec" << std::endl;
                std::cout << "Calculating segment to segment response "
                             "factors ..." << std::endl;
            } // end if

            // inputs
            bool reaSource;
            bool imgSource;
            # pragma omp parallel for num_threads(processor_count)
            for (int s=0; s<SimReal.nSim; s++) {
                reaSource = true;
                imgSource = false;
                _calculate_h(SimReal, s, reaSource, imgSource);
            } // next s
            if (splitRealAndImage) {
                reaSource = false;
                imgSource = true;
                # pragma omp parallel for num_threads(processor_count)
                for (int s=0; s<SimImage.nSim; s++) {
                    _calculate_h(SimImage, s, reaSource, imgSource);
                }
            }

            auto end2 = std::chrono::steady_clock::now();
            if (disp) {
                auto milli = std::chrono::duration_cast<std::chrono::milliseconds>(end2 - end).count();
                double seconds = double(milli) / 1000;
                std::cout << "Elapsed time in seconds : "
                          << seconds
                          << " sec" << std::endl;
            }
        } else {
            if (disp) {
                std::cout << "Calculating segment to segment response factors ..." << std::endl;
            } // end if
            auto start = std::chrono::steady_clock::now();
            bool sameSegment;
            bool otherSegment;

            auto _fill_line = [&SegRes, &time](const int i, const int j,
                    const double alpha, bool sameSegment, bool otherSegment) {
                double h;
                int index;
                gt::boreholes::Borehole b1;
                gt::boreholes::Borehole b2;
                b2 = SegRes.boreSegments[i];
                for (std::size_t k = 0; k < time.size(); k++) {
                    double t = time[k];
                    if (!otherSegment){
                        if (sameSegment) {
                            b1 = SegRes.boreSegments[i];
                            h = finite_line_source(t, alpha, b2, b2);
                        }
                    } else if (otherSegment && !sameSegment) {
                        b1 = SegRes.boreSegments[j];
                        h = finite_line_source(t, alpha, b1, b2);
                    } else {
                        throw std::invalid_argument( "sameSegment and otherSegment cannot both be true" );
                    } // end if
                    SegRes.get_index_value(index, i, j);
                    SegRes.h_ij[index][k] = h;
                }; // end for
            }; // auto _fill_line
            #pragma omp parallel for num_threads(processor_count)
            for (int i = 0; i < nSources; i++) {
                // Segment to same-segment thermal response factor
                // FLS solution for combined real and image sources
                sameSegment = true;
                otherSegment = false;
                _fill_line(i, i, alpha, sameSegment, otherSegment);
                // Segment to other segment thermal response factor
                for (int j = i + 1; j<nSources; j++) {
                    sameSegment = false;
                    otherSegment = true;
                    _fill_line(i, j, alpha, sameSegment, otherSegment);
                } // end for
            } // fi (end if)

            auto end = std::chrono::steady_clock::now();
            if (disp) {
                auto milli = std::chrono::duration_cast<std::chrono::milliseconds>(end - start).count();
                double seconds = double(milli) / 1000;
                std::cout << "Elapsed time in seconds : "
                          << seconds
                          << " sec" << std::endl;
            }
            // Iterate over the thread vector
        } // fi similarity
    } // void thermal_response_factors

} // namespace gt::heat_transfer