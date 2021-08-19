//
// Created by jackcook on 7/11/20.
//

#include <iostream>
#include <vector>
#include <cmath>

using namespace std;

#ifndef CPPGFUNCTION_UTILITIES_H
#define CPPGFUNCTION_UTILITIES_H

namespace gt::utilities {

    double time_scale(const double& H, const double& alpha);

    double hour_to_sec(double& x);
    double day_to_sec(double& x);
    double month_to_sec(double& x);
    double year_to_sec(double& x);
    double time_to_seconds(double& duration, const string& units);

    vector<double> time_geometric(double dt, double tmax, int Nt);
    vector<double> time_geometric_auto(double duration, const string& units);
    vector<double> Eskilson_original_points();
    vector<double> time_Eskilson(const double &H, const double &alpha);
    vector<double> convert_time(vector<double> &logtime, const double &H,
                                const double &alpha);
    vector<double> cook_spitler_time();
    void convert_time(vector<double> &logtime, vector<double> &time,
                      double H, double alpha);
    vector<double> time_vector_Eskilson(
            double& H, double& alpha, double& duration, const string& units);
    vector<double> time_vector_constant_expansion(
            double& H, double& alpha, double& duration,
            const string& units="sec", const double expansion_constant=0.35);

} // namespace gt::utilities

#endif //CPPGFUNCTION_UTILITIES_H
