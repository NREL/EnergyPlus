//
// Created by jackcook on 5/7/21.
//

#include <cpgfunction/utilities.h>


int main() {
    // -- Time definitions --
    // Geometrically expanding time vector
    double dt = 100 * 3600.;  // time step
    int duration = 36;  // duration in months
    double tmax = double(duration) / 3 * 8760. * 3600.;
    int Nt = 10;  // number of time steps
    std::vector<double> time = gt::utilities::time_geometric(dt, tmax, Nt);

    // Eskilson's original 27 time steps
    double H = 100;  // height of the boreholes
    double alpha = 1.0e-06;  // thermal diffusivity
    time = gt::utilities::time_Eskilson(H, alpha);

    return 0;
}