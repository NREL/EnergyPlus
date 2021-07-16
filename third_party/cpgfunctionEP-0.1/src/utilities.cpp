//
// Created by jackcook on 7/11/20.
//

#include <cpgfunction/utilities.h>

namespace gt {
    namespace utilities {
        std::vector<double> time_geometric(double dt, double tmax, int Nt) {
            std::vector<double> time(Nt);  // create a time vector of size Nt

            double value;
            double tmax_calc = double(Nt) * double(dt);
            if (tmax > tmax_calc) {
                double dr = 1.0e99;
                double r = 2.;
                while (std::abs(dr) > 1.0e-10) {
                    dr = std::pow(1+tmax/double(dt)*(r-1), 1/double(Nt)) - r;
                    r += dr;
                } // end while
                for (int j=0; j < time.size(); j++) {
                    value = 1 - std::pow(r, double(j+1));
                    value = value * double(dt) / (1 - r);
                    time[j] = value;
                } // end for
            } else {
                for (int j=0; j < time.size(); j++) {
                    value = double(dt) * double(j + 1);
                    time[j] = value;
                } // end for
            } // end if
            return time;
        } // vector<double> time_geometric

        std::vector<double> Eskilson_original_points() {
            // Eskilsons original 27 time steps
            std::vector<double> logtime = {-8.5, -7.8, -7.2, -6.5, -5.9, -5.2, -4.5, -3.963, -3.27, -2.864,-2.577,
                                           -2.171, -1.884, -1.191, -0.497, -0.274, -0.051, 0.196, 0.419, 0.642, 0.873,
                                           1.112, 1.335, 1.679, 2.028, 2.275, 3.003};
            return logtime;

        }

        std::vector<double> time_Eskilson(const double &H, const double &alpha){
            std::vector<double> logtime = Eskilson_original_points();
            std::vector<double> time = convert_time(logtime, H, alpha);

            return time;
        }  // time_Eskilson();

        std::vector<double> convert_time(std::vector<double> &logtime, const double &H, const double &alpha) {
            int nt = logtime.size();
            std::vector<double> time(nt);

            double ts = pow(H, 2) / (9 * alpha);
            for (int i=0; i<nt; i++) {
                time[i] = exp(logtime[i]) * ts;
            }

            return time;
        } // convert_time();

        void cook_spitler_time (std::vector<double> &logtime){
            int np = 31; // 31 total points
            if (logtime.size() != np) {
                logtime.resize(np);
            }
            for (int i=1; i<np+1; i++) {
                logtime[i-1] = 0.0005*pow(i, 3) - 0.0303 * pow(i, 2) + 0.8491*i - 9.4028;
            }
        } // cook_spitler_time

        void convert_time(std::vector<double> &logtime, std::vector<double> &time, const double H, const double alpha) {
            int nt = logtime.size();
            if (time.size() != nt) {
                time.resize(nt);
            }
            double ts = pow(H, 2) / (9 * alpha);
            for (int i=0; i<nt; i++) {
                time[i] = exp(logtime[i]) * ts;
            }
        } // convert_time

    } // namespace utilities
} // namespace gt
