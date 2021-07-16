//
// Created by jackcook on 5/7/21.
//

#include <cpgfunction/coordinates.h>
#include <cpgfunction/boreholes.h>


int main(){
    // -- Definitions --
    // Coordinate geometry
    int Nx = 3;
    int Ny = 3;
    double Bx = 6.;
    double By = 4.5;

    // -- Configurations --
    // Get x,y coordinates for a rectangle
    std::string shape = "Rectangle";
    std::vector<std::tuple<double, double>> coordinates =
            gt::coordinates::configuration(shape, Nx, Ny, Bx, By);

    // -- Borehole geometry --
    double H = 100;  // height of the borehole (in meters)
    double D = 4;  // burial depth (in meters)
    double r_b = 0.075;  // borehole radius (in meters)

    // -- boreField --
    std::vector<gt::boreholes::Borehole> boreField = gt::boreholes::boreField(coordinates, r_b, H, D);

    return 0;
}
