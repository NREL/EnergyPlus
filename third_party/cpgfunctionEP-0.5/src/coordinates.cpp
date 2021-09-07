//
// Created by jackcook on 5/6/21.
//

#include <algorithm>
#include <cpgfunction/coordinates.h>
#include <stdexcept>
#include <string>
#include <tuple>

using namespace std;

namespace gt::coordinates {

    vector<tuple<double, double>> configuration(const string& shape, int Nx,
                                                int Ny, double Bx, double By){
        // define acceptable inputs
        vector<string> acceptable_arguments{"Rectangle",
                                            "OpenRectangle",
                                            "U",
                                            "L"};
        // check if the input string is acceptable
        bool acceptable = (find(acceptable_arguments.begin(),
                                acceptable_arguments.end(),
                                shape) != acceptable_arguments.end());

        if (!acceptable) {
            throw std::invalid_argument("The shape described (" +
            shape + ") is not an available input for "
                    "gt::coordinates::configuration().");
        }

        std::vector<std::tuple<double, double>> coordinates;

        if (shape == "Rectangle") {
            coordinates = rectangle(Nx, Ny, Bx, By);
        } else if (shape == "OpenRectangle") {
            coordinates = Open_rectangle(Nx, Ny, Bx, By);
        } else if (shape == "U") {
            coordinates = U_shape(Nx, Ny, Bx, By);
        } else if (shape == "L") {
            coordinates = L_shape(Nx, Ny, Bx, By);
        }

        return coordinates;
    }  // configuration();

//    vector<tuple<double, double>> configuration(const string& shape,
//                                                const string& input_path){
//        // define acceptable inputs
//        std::vector<std::string> acceptable_arguments{"custom"};
//        // check if the input string is acceptable
//        bool acceptable = (std::find(acceptable_arguments.begin(),
//                                     acceptable_arguments.end(), shape) !=
//                                             acceptable_arguments.end());
//
//        if (!acceptable) {
//            throw invalid_argument("The shape described (" +
//            shape + ") is not an available input for "
//                    "gt::coordinates::configuration().");
//        }
//
//        vector<tuple<double, double>> custom =
//                import_coordinates_from_file(input_path);
//
//        return custom;
//    }  // configuration();

    vector<tuple<double, double>> rectangle(int Nx, int Ny, double Bx,
                                            double By) {
        std::vector<std::tuple<double, double>> r;  // rectangular coordinates

        int nbh = Nx * Ny;
        r.reserve(nbh);
        for (int i = 0; i < Nx; i++) {
            for (int j = 0; j < Ny; j++) {
                r.emplace_back(i * Bx, j * By);
            }  // next j
        }  // next i

        return r;
    }  // rectangle();

    vector<tuple<double, double>> Open_rectangle(int Nx, int Ny, double Bx,
                                                 double By){
        vector<tuple<double, double>> open_r;  // open rectangle coordinates

        int nbh;
        if (Nx > 2 && Ny > 2) {
            nbh = Ny * 2 + (Nx - 2) * 2;
            open_r.reserve(nbh);
            for (int i = 0; i < Nx; i++) {
                open_r.emplace_back(i * Bx, 0.);
            }  // next i
            for (int j = 1; j < Ny - 1; j++) {
                open_r.emplace_back(0., j * By);
                open_r.emplace_back((Nx-1) * Bx, j * By);
            } // next j
            for (int i = 0; i < Nx; i++) {
                open_r.emplace_back(i * Bx, (Ny-1)*By);
            }  // next i
        } // if (Nx > 2 && Ny > 2)
        else {
            nbh = Nx * Ny;
            open_r.reserve(nbh);
            open_r = rectangle(Nx, Ny, Bx, By);
        }  // else()

        return open_r;
    }  // Open_rectangle();

    vector<tuple<double, double>> U_shape(int Nx, int Ny, double Bx, double By){
        std::vector<std::tuple<double, double>> U;  // U-shape coordinates

        int nbh;
        if (Nx > 2 && Ny > 1) {
            nbh = 2 * Ny + (Nx - 2);
            U.reserve(nbh);
            for (int i = 0; i < Nx; i++){
                U.emplace_back(i * Bx, 0.);
            } // next i
            for (int j = 1; j < Ny; j++) {
                U.emplace_back(0., j * By);
                U.emplace_back((Nx-1) * Bx, j * By);
            }  // next j
        }  // if (Nx > 2 && Ny > 1)
        else {
            nbh = Nx * Ny;
            U.reserve(nbh);
            U = rectangle(Nx, Ny, Bx, By);
        }  // else()

        return U;
    }  // U_shape();

    vector<tuple<double, double>> L_shape(int Nx, int Ny, double Bx, double By){
        std::vector<std::tuple<double, double>> L;  // L-shape coordinates

        int nbh = Nx + Ny - 1;
        L.reserve(nbh);  // reserve for "emplace_back"
        for (int i = 0; i < Nx; i++){
            L.emplace_back(i * Bx, 0.);
        }  // next i
        for (int j = 1; j < Ny; j++){
            L.emplace_back(0., j * By);
        }  // next j

        return L;
    }  // L_shape();

//    vector<tuple<double, double>> import_coordinates_from_file(
//            const string& input_path){
//        vector<tuple<double, double>> custom;  // custom-shape from .json file
//
//        // nlohmann json input
//        std::ifstream in(input_path);
//        nlohmann::json js;
//        in >> js;
//
//        std::vector<double> x = js["x"];
//        std::vector<double> y = js["y"];
//
//        custom.reserve(x.size());
//        for (int i = 0; i < x.size(); i++){
//            custom.emplace_back(x[i], y[i]);
//        }
//
//        return custom;
//    }  // import_coordinates_from_file();
//
//    void export_coordinates_to_file(
//            const vector<tuple<double, double>> &coordinates,
//            const std::string& output_path){
//        // Use nlohmann json to export the coordinates to a path
//
//        std::vector<double> x_values(coordinates.size());
//        std::vector<double> y_values(coordinates.size());
//
//        for (int i=0; i<x_values.size(); i++) {
//            x_values[i] = std::get<0>(coordinates[i]);
//            y_values[i] = std::get<1>(coordinates[i]);
//        }
//
//        std::ofstream o(output_path);
//
//        nlohmann::json j;
//
//        j["x"] = x_values;
//        j["y"] = y_values;
//
//        o << std::setw(4) << j << std::endl;
//    }  // export_coordinates_to_file();
}  // namespace gt::coordinates