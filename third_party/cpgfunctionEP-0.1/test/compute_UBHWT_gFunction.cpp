//
// Created by jackcook on 5/7/21.
//

#include <cpgfunction/coordinates.h>
#include <cpgfunction/boreholes.h>
#include <cpgfunction/utilities.h>
#include <cpgfunction/gfunction.h>
#include <nlohmann/json.hpp>
#include <fstream>
#include <iomanip>
#include <tuple>
#include <cpgfunction/statistics.h>
#include <stdexcept>
#include <chrono>


void export_gFunction(std::string output_path, std::vector<double> logtime,
                      std::vector<double> gFunction) {
    std::ofstream o(output_path);

    nlohmann::json j;

    j["logtime"] = logtime;
    j["g"] = gFunction;

    o << std::setw(4) << j << std::endl;
}  // export_gFunction();


std::vector<std::tuple<double, double>> import_gFunction(std::string input_path) {
    std::vector<std::tuple<double, double>> gFunction;  // custom-shape from .json file

    // nlohmann json input
    std::ifstream in(input_path);
    nlohmann::json js;
    in >> js;

    std::vector<double> x = js["logtime"];
    std::vector<double> y = js["g"];

    gFunction.reserve(x.size());
    for (int i = 0; i < x.size(); i++){
        gFunction.emplace_back(x[i], y[i]);
    }

    return gFunction;
}


int main(){
    // Toggle whether or not files are being created
    bool creation = false;

    // -- Definitions --
    // Coordinate geometry
    int Nx = 10;
    int Ny = 10;
    double Bx = 6.;
    double By = 4.5;

    // -- Borehole geometry --
    double H = 100;  // height of the borehole (in meters)
    double D = 4;  // burial depth (in meters)
    double r_b = 0.075;  // borehole radius (in meters)

    // Ground properties
    double alpha = 1.0e-06;  // ground thermal diffusivity

    // -- Time definition --
    // Eskilson's original 27 time steps (in seconds)
    std::vector<double> time = gt::utilities::time_Eskilson(H, alpha);
    // Eskilson's original 27 time steps (logarithmic)
    std::vector<double> logtime = gt::utilities::Eskilson_original_points();

    // -- Configurations --
    std::vector<std::string> shapes{"Rectangle", "OpenRectangle", "U", "L", "custom"};
    std::string custom_path = "Poisson_Disk_120_30_101.json";

    for (int i = 0; i < shapes.size(); i++) {
        // Field shape
        std::string shape = shapes[i];
        std::vector<std::tuple<double, double>> coordinates;
        // Define a path
        std::string path = shape + ".json";
        if (shape != "custom") {
            // Get x,y coordinates
            coordinates = gt::coordinates::configuration(shape, Nx, Ny, Bx, By);
        } else {
            coordinates = gt::coordinates::configuration(shape, custom_path);
        }

        // Define borehole field
        std::vector<gt::boreholes::Borehole> boreField = gt::boreholes::boreField(coordinates, r_b, H, D);
        std::cout << "Compute g-function for borefield of shape: " + shape << std::endl;
        auto start = std::chrono::steady_clock::now();
        // Compute uniform borehole wall temperature g-function
        vector<double> gFunction =
                gt::gfunction::uniform_borehole_wall_temperature(boreField,
                                                                 time, alpha,
                                                                 12,
                                                                 true,
                                                                 true);
        auto end = std::chrono::steady_clock::now();
        auto milli = std::chrono::duration_cast<std::chrono::milliseconds>(end - start).count();
        double seconds = double(milli) / 1000;
        std::cout << "\ttime: ";
        std::cout << seconds;
        std::cout << " seconds" << std::endl;

        if (creation) {
            export_gFunction(path, logtime, gFunction);
        } else {
            std::vector<std::tuple<double, double>> gFunctionValues = import_gFunction(path);
            std::vector<double> gFunctionReference(gFunctionValues.size());
            // pull out the g-function value from the tuple
            for (int j = 0; j < gFunctionValues.size(); j++) {
                gFunctionReference[j] = std::get<1>(gFunctionValues[j]);
            }  // next i
            double rmse = gt::statistics::root_mean_square_error(gFunctionReference, gFunction);
            rmse *= 100;

            if (rmse > 1.0E-3) {
                throw std::invalid_argument("The root mean square error is more than 0.");
            }

            std::cout << "\trmse (%): ";
            std::cout.precision(17);
            std::cout << rmse;
            std::cout << std::endl;
            std::cout.precision(5);
        }
    }

    // A test for a custom configuration

    return 0;
}
