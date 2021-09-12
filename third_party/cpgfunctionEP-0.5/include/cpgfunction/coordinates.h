//
// Created by jackcook on 5/6/21.
//

#include <utility>
#include <vector>
#include <tuple>
#include <iostream>

using namespace std;

#ifndef CPGFUNCTION_COORDINATES_H
#define CPGFUNCTION_COORDINATES_H

namespace gt::coordinates {

    vector<tuple<double, double>> configuration(const string& shape, int Nx,
                                                int Ny, double Bx, double By);
//    vector<tuple<double, double>> configuration(const string& shape,
//                                                const string& input_path);

    vector<tuple<double, double>> rectangle(int Nx, int Ny, double Bx,
                                            double By);
    vector<tuple<double, double>> Open_rectangle(int Nx, int Ny, double Bx,
                                                 double By);
    vector<tuple<double, double>> U_shape(int Nx, int Ny, double Bx, double By);
    vector<tuple<double, double>> L_shape(int Nx, int Ny, double Bx, double By);

//    vector<tuple<double, double>> import_coordinates_from_file(const string& input_path);
//    void export_coordinates_to_file(const vector<tuple<double, double>> &coordinates,
//                                    const string& output_path);

}

#endif //CPGFUNCTION_COORDINATES_H
