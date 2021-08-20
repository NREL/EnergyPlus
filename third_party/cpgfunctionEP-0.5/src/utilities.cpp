//
// Created by jackcook on 7/11/20.
//

#include <algorithm>
#include <cmath>
#include <cpgfunction/utilities.h>

using namespace std;


namespace gt::utilities {

    double time_scale(const double& H, const double& alpha){
        return pow(H, 2) / (9 * alpha);
    }

    double hour_to_sec(double& x){
        return x * 3600.;
    }

    double day_to_sec(double& x){
        double hours = x * 24;
        return hour_to_sec(hours);
    }

    double month_to_sec(double& x){
        double days = x * 30.4167;
        return day_to_sec(days);
    }

    double year_to_sec(double& x) {
        double days = x * 365.;
        return day_to_sec(days);
    }

    double time_to_seconds(double& duration, const string& units) {
        // define acceptable inputs
        vector<string> acceptable_arguments{"sec", "hour" "month", "year"};
        // check if the input string is acceptable
        bool acceptable = (find(acceptable_arguments.begin(),
                                acceptable_arguments.end(), units) !=
                           acceptable_arguments.end());

        if (!acceptable) {
            throw std::invalid_argument("The unit described (" + units
                                        + ") is not an available input for "
                                          "gt::utilities::time_vector().");
        }
        // declare a variable for time in seconds
        double time_in_seconds = 0;
        // convert the time to seconds if it is not already in seconds
        if (units == "sec") {
            time_in_seconds = duration;
        } else if (units == "hour") {
            time_in_seconds = hour_to_sec(duration);
        } else if (units == "month") {
            time_in_seconds = month_to_sec(duration);
        } else if (units == "year") {
            time_in_seconds = year_to_sec(duration);
        }
        return time_in_seconds;
    }

    vector<double> time_geometric(double dt, double tmax, int Nt) {
        vector<double> time(Nt);  // create a time vector of size Nt

        double value;
        double tmax_calc = double(Nt) * double(dt);
        if (tmax > tmax_calc) {
            double dr = 1.0e99;
            double r = 2.;
            while (abs(dr) > 1.0e-10) {
                dr = pow(1+tmax/double(dt)*(r-1), 1/double(Nt)) - r;
                r += dr;
            } // end while
            for (int j=0; j < time.size(); j++) {
                value = 1 - pow(r, double(j+1));
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

    vector<double> time_geometric_auto(double duration, const string& units) {
        double time_in_seconds = time_to_seconds(duration, units);

        double dt = 100. * 3600.;

        int Nt = ceil(time_in_seconds / dt);
        if (Nt < 5) {
            Nt = 5;
        } else if (Nt > 30) {
            Nt = 30;
        }

        vector<double> time = time_geometric(dt, time_in_seconds, Nt);
        return time;
    }

    vector<double> Eskilson_original_points() {
        // Eskilsons original 27 time steps
        vector<double> logtime = {-8.5, -7.8, -7.2, -6.5, -5.9, -5.2, -4.5,
                                  -3.963, -3.27, -2.864,-2.577, -2.171, -1.884,
                                  -1.191, -0.497, -0.274, -0.051, 0.196, 0.419,
                                  0.642, 0.873, 1.112, 1.335, 1.679, 2.028,
                                  2.275, 3.003};
        return logtime;

    }

    vector<double> time_Eskilson(const double &H, const double &alpha){
        vector<double> logtime = Eskilson_original_points();
        vector<double> time = convert_time(logtime, H, alpha);

        return time;
    }  // time_Eskilson();

    vector<double> convert_time(vector<double> &logtime,
                                     const double &H, const double &alpha) {
        int nt = logtime.size();
        vector<double> time(nt);

        double ts = time_scale(H, alpha);
        for (int i=0; i<nt; i++) {
            time[i] = exp(logtime[i]) * ts;
        }

        return time;
    } // convert_time();

    vector<double> cook_spitler_time (){
        int np = 31; // 31 total points
        vector<double> logtime(np, 0);
        if (logtime.size() != np) {
            logtime.resize(np);
        }
        for (int i=1; i<np+1; i++) {
            logtime[i-1] = 0.0005*pow(i, 3) - 0.0303 * pow(i, 2) + 0.8491*i - 9.4028;
        }
        return logtime;
    } // cook_spitler_time

    void convert_time(vector<double> &logtime, vector<double> &time,
                      const double H, const double alpha) {
        int nt = logtime.size();
        if (time.size() != nt) {
            time.resize(nt);
        }
        double ts = pow(H, 2) / (9 * alpha);
        for (int i=0; i<nt; i++) {
            time[i] = exp(logtime[i]) * ts;
        }
    } // convert_time

    vector<double> time_vector_Eskilson(double& H, double& alpha, double& duration,
                               const string& units="sec"){
        // get Eskilson's 27 points in seconds
        vector<double> time = time_Eskilson(H, alpha);

        double time_in_seconds = time_to_seconds(duration, units);

        // loop through points until the time in vector is greater than
        // the duration
        int i = 0;
        bool enough = false;
        while (i < time.size() && !enough) {
            i += 1;
            if (time_in_seconds < time[i]) {
                enough = true;
            }
        }  // wend
        // resize the vector if need be
        if (enough) {
            time.resize(i+1);
        }
        // Note: the maximum value described by Eskilson's highest value of
        // ln(t/ts) = 3.003 is the maximum time available from this function.
        // Any request for a longer simulation than that will just simulate to
        // ln(t/ts) = 3.003.
        return time;
    }  // time_vector();

    vector<double> time_vector_constant_expansion(
            double& H, double& alpha, double& duration,
            const string& units, const double expansion_constant) {
        double time_in_seconds = time_to_seconds(duration, units);
        // find the number of points necessary
        double log_time_begin = -8.5;
        double ts = time_scale(H, alpha);
        double log_time_end = log(time_in_seconds / ts);
        int n_points = ceil((log_time_end - log_time_begin) /
                expansion_constant) + 1;
        vector<double> log_time(n_points, 0);
        log_time[0] = log_time_begin;
        for (int i=1; i<n_points;i++) {
            log_time[i] = log_time_begin + i * expansion_constant;
        }  // next i
        vector<double> time = convert_time(log_time, H, alpha);
        return time;
    }

} // namespace gt::utilities
