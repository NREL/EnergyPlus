//
// Created by jackcook on 7/11/20.
//

#include <stdexcept>
#include <thread>
#include <cpgfunction/boreholes.h>
#include <cpgfunction/heat_transfer.h>

std::vector<double> gt::heat_transfer::FLSApproximation::construct_dm(
        gt::boreholes::Borehole &segment_i, gt::boreholes::Borehole &segment_j) {
    // Fill d_m vector (equation 13)
    std::vector<double> d_(8, 0);

    // Real source portion (equation 13)
    d_[0] = segment_i.D - segment_j.D + segment_i.H;
    d_[1] = segment_i.D - segment_j.D;
    d_[2] = segment_i.D - segment_j.D - segment_j.H;
    d_[3] = segment_i.D - segment_j.D + segment_i.H - segment_j.H;
    // Mirror (Image) source portion (equation 13)
    d_[4] = segment_i.D + segment_j.D + segment_i.H;
    d_[5] = segment_i.D + segment_j.D;
    d_[6] = segment_i.D + segment_j.D + segment_j.H;
    d_[7] = segment_i.D + segment_j.D + segment_i.H + segment_j.H;

    return d_;
}

double gt::heat_transfer::FLSApproximation::finite_line_source(
        double &time, double &alpha, gt::boreholes::Borehole &segment_i,
        gt::boreholes::Borehole &segment_j, std::vector<double> &d_,
        bool reaSource, bool imgSource) {

    int m_start;
    int m_stop;
    if (reaSource && !imgSource) {
        m_start = 0;
        m_stop = 4;
    } else if (!reaSource && imgSource) {
        m_start = 4;
        m_stop = 8;
    } else if (reaSource && imgSource) {
        m_start = 0;
        m_stop = 8;
    } else {
        throw std::invalid_argument("Real and Mirror sources should not both"
                                    "be set to false for "
                                    "gt::heat_transfer::FLSApproximation::finite_line_source.");
    }


    double r_ij = segment_i.distance(segment_j);
    double G1;
    double G3;

    int n, m;

    double c_m, d_m, num, ratio;
    double den = 4. * alpha * time;
    double r_ij2 = std::pow(r_ij, 2);
    double sqPI = std::sqrt(M_PI);

    double h_ij = 0.;
    double summation;
    for (n=0; n<=N;n++) {
        summation = 0.;
        for (m=m_start; m<m_stop; m++) {
            c_m = std::pow(-1., m);
            d_m = d_[m];
            num = r_ij2 + b_erf[n] * std::pow(d_m, 2);
            ratio = num / den;
            G1 = 0.5 * gt::heat_transfer::E1(ratio);
            summation += c_m * std::fabs(d_m) * G1;
        } // next m
        h_ij += a_erf[n] * summation;
    } // next n
    h_ij *= 1. / (2. * segment_i.H);

    summation = 0;
    double val;
    for (m=m_start; m<m_stop; m++) {
        val = r_ij2 + std::pow(d_[m], 2);

        G3 = std::sqrt(den) * std::exp(- val / den) - std::sqrt(val) * sqPI
                                                      * std::erfc(std::sqrt(val / den));

        summation += std::pow(-1., m) * G3;
    }
    h_ij += - 1. / (2. * std::sqrt(M_PI) * segment_i.H) * summation;

    return -h_ij;
}

void gt::heat_transfer::thermal_response_factors(gt::segments::SegmentResponse &SegRes,
                                                 std::vector<double> &time, double &alpha,
                                                 bool use_similarities, bool disp, int n_Threads) {
    // total number of line sources
    int nSources = SegRes.boreSegments.size();
    // number of time values
    int nt = time.size();

    gt::boreholes::SimilaritiesType SimReal; // positive
    gt::boreholes::SimilaritiesType SimImage; // negative

    auto sum_to_n = [](const int n) {
        return n * (n + 1) / 2;
    };
    if (use_similarities) {
        auto start = std::chrono::steady_clock::now();
        // Calculations with similarities
        if (disp) {
            std::cout << "Identifying similarities..." << std::endl;
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

        gt::heat_transfer::FLSApproximation FLSApprox =
                gt::heat_transfer::FLSApproximation(10);

        // lambda function for calculating h at each time step
        auto _calculate_h = [&SegRes, &splitRealAndImage, &time, &alpha, &nt, &FLSApprox]
                (boreholes::SimilaritiesType &SimReal, int s, bool reaSource,
                 bool imgSource) {
            // begin function
            int n1;
            int n2;
            gt::boreholes::Borehole b1;
            gt::boreholes::Borehole b2;
            // begin thread
            n1 = std::get<0>(SimReal.Sim[s][0]);
            n2 = std::get<1>(SimReal.Sim[s][0]);
            b1 = SegRes.boreSegments[n1];
            b2 = SegRes.boreSegments[n2];
            std::vector<double> d_ = FLSApprox.construct_dm(b1, b2);
            std::vector<double> hPos(nt);
            if (splitRealAndImage) {
                for (int k=0; k<nt; k++) {
                    hPos[k] = FLSApprox.finite_line_source(time[k],
                                                           const_cast<double &>(alpha), b1,
                                                           b2, d_, reaSource, imgSource);
                }  // next k
                int i;
                int j;
                if (SegRes.storage_mode==1) {
                    // will loop through every (i, j), will combine real+image
                    int index;
                    for (std::size_t k=0; k<SimReal.Sim[s].size(); k++) {
                        i = std::get<0>(SimReal.Sim[s][k]);
                        j = std::get<1>(SimReal.Sim[s][k]);
                        for (std::size_t t=0; t<time.size(); t++){
                            // must consider real and image source separate
                            // when combining
                            if (i <= j) {
                                // we want to store n2, n1
                                SegRes.get_index_value(index, i, j);
                                // non-critical race condition
                                SegRes.h_ij[t][index] += b2.H / b1.H * hPos[t];
                            } else {
                                SegRes.get_index_value(index, j, i);
                                // non-critical race condition
                                SegRes.h_ij[t][index] += hPos[t];
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
            auto milli = std::chrono::duration_cast<
                    std::chrono::milliseconds>(end - start).count();
            double seconds = double(milli) / 1000;
            std::cout << "Elapsed time in seconds : " << seconds
                      << " sec" << std::endl;
            std::cout << "Calculating segment to segment response "
                         "factors ..." << std::endl;
        } // end if

        // inputs
        bool reaSource;
        bool imgSource;
# pragma omp parallel for default(none) num_threads(n_Threads) shared(SimReal, _calculate_h, reaSource, imgSource)
        for (int s=0; s<SimReal.nSim; s++) {
            reaSource = true;
            imgSource = false;
            _calculate_h(SimReal, s, reaSource, imgSource);
        } // next s
        if (splitRealAndImage) {
            reaSource = false;
            imgSource = true;
# pragma omp parallel for default(none) num_threads(n_Threads) shared(SimImage, _calculate_h, reaSource, imgSource)
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

        gt::heat_transfer::FLSApproximation FLSApprox =
                gt::heat_transfer::FLSApproximation(10);

        auto _fill_line = [&SegRes, &time, &FLSApprox](const int i, const int j,
                                           const double alpha, bool sameSegment, bool otherSegment) {
            double h;
            int index;
            gt::boreholes::Borehole b1;
            gt::boreholes::Borehole b2;
            std::vector<double> d_ = FLSApprox.construct_dm(b1, b2);
            b2 = SegRes.boreSegments[i];
            for (std::size_t k = 0; k < time.size(); k++) {
                double t = time[k];
                if (!otherSegment){
                    if (sameSegment) {
                        b1 = SegRes.boreSegments[i];
                        h = FLSApprox.finite_line_source(
                                t,const_cast<double &>(alpha), b1,
                                b2, d_, true, true);
                    }
                } else if (otherSegment && !sameSegment) {
                    b1 = SegRes.boreSegments[j];
                    h = FLSApprox.finite_line_source(
                            t,const_cast<double &>(alpha), b1,
                            b2, d_, true, true);
                } else {
                    throw std::invalid_argument( "sameSegment and otherSegment cannot both be true" );
                } // end if
                SegRes.get_index_value(index, i, j);
                SegRes.h_ij[index][k] = h;
            }; // end for
        }; // auto _fill_line
#pragma omp parallel for default(none) num_threads(n_Threads) shared(nSources, _fill_line, alpha) private(sameSegment, otherSegment)
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


double gt::heat_transfer::E1(double &z, const int max_iter){
    // Exponential integral (see quotient difference algorithm)
    //
    // Note: This is only valid for z>= 0, if z > 0, then Ei needs to be
    // computed from the relation -E1(x) = Ei(-x)
    //
    // References:
    //  (1) ISO C++ 14882 TR1:5.2 Special functions by Edward Smith-Rowland
    //  (2) Numerical Recipes in C++ 2nd Ed. by Press et al. (2002).
    //  (3) Exponential Integral Wikipedia
    //      https://en.wikipedia.org/wiki/Exponential_integral
    //  (4) Digital Library of Mathematical Functions, 6.12
    //      Asymptotic Expansions. https://dlmf.nist.gov/6.12

    double old_val=99999.;
    double expint;
    // Convergence criteria
    const double criteria = 1.0e-06;
    double error = 999999.;
    double factorial = 1.;
    // Euler-Mascheroni
    // (https://en.wikipedia.org/wiki/Euler%E2%80%93Mascheroni_constant)
    double gamma = 0.577215664901532860606512090082;

    int m=1;
    double summation;
    // Small value that is not zero
    const double near_0 = std::numeric_limits<double>::min();

    if (z == 0.0) expint = 1.;
    else if (z <= 1) {
        // Series representation
        summation = 0.;
        while (error >= criteria && m < max_iter) {
            factorial *= m;
            summation += std::pow(-z, m) / (m * factorial);
            // Obtain current error
            error = std::abs(old_val - summation) / std::abs(old_val);
            // Store value for old next iteration
            old_val = summation;
            // increment iteration
            m++;
        }
        expint = gamma + std::log(z) + summation;
    } else if (z <= 100) {
        // Continued fraction equation by modified Lentz's method
        unsigned int n = 1;  // E1 rather than En
        double bj = z + n;
        double aj;
        // Start Lentz's algorithm smarter by propagating through one time
        // to help reduce precision errors
        // set f0 = b0 if b0=0 set f0 near 0
        // set c0 = f0
        // set D0 = 0
        // Now propagate through algorithm
        // Dj = bj + a_jD_{j-1}, therefore, D = 1 / b now
        double D = 1. / bj;
        // Cj = bj + aj/C_{j-1} -> infinity
        double C = 1. / near_0;
        // f = f * delta_j = f * C * D = 1 / infty * infty * 1 / bj = D
        double f = D;
        double delta_j = 99999;
        //       b       a
        // 1    z+n      1
        // 2   b1+2     -1*n
        // 3   b2+2    -2*(n+1)
        // 4   b3+2    -4*(n+1)
        // Therefore,
        // a = - (m * ((n-1) + m))
        // b0 = z + n and bj = b_{j-1} + 2
        // For j  = 1,2,...
        while ( std::abs(delta_j - 1.) > criteria && m < max_iter ) {
            bj += 2.;
            aj = - (m * ((n-1.) + m));
            // set Dj = bj + ajD_j-1
            D = bj + aj * D;
            // if Dj =0, set Dj=near 0
            if (D == 0.0) D = near_0;
            // set Cj = bj + aj/C_{j-1}
            C = bj + (aj / C);
            if (C == 0.0) C = near_0;
            // Set Dj = 1/Dj
            D = 1 / D;
            // Set delta_j = CjDj
            delta_j = C * D;
            // Set fj = f_{j-1} * delta_j
            f = f * delta_j;
            m++;
        }
        expint = -f * std::exp(-z);
    } else if (z > 100) {
        // Asymptotic expansion
        factorial = 1.;
        summation = 1.;
        while (error >= criteria && m < max_iter) {
            factorial *= m;
            summation += std::pow(-1, m) * factorial / std::pow(z, m);
            // Obtain current error
            error = std::abs(old_val - summation) / std::abs(old_val);
            // Store value for old next iteration
            old_val = summation;
            // increment iteration
            m++;
        }
        expint = std::exp(-z) / z * summation;
    } else {
        throw std::invalid_argument("This exponential integral function E1 "
                                    "only handles values greater than 0. "
                                    "The Ei function needs implemented to "
                                    "handle values less than 0.");
    }

    return expint;

};