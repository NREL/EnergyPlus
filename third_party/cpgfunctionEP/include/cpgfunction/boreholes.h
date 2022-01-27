//
// Created by jackcook on 7/11/20.
//

#ifndef CPPGFUNCTION_BOREHOLES_H
#define CPPGFUNCTION_BOREHOLES_H

#include <cmath>
#include <iostream>
#include <string>
#include <tuple>
#include <vector>

namespace gt {

    double Distance_Formula(double x1, double y1, double x2, double y2);

    namespace boreholes {
        struct Borehole {
            // Destructor
            virtual ~Borehole() {
            }

            double H;    // height or length of the borehole (meters)
            double D;    // borehole burial depth (meters)
            double r_b;  // borehole radius (meters)
            double x;    // position (meters) of the center of the borehole along the x-axis
            double y;    // position (meters) of the center of the borehole along the y-axis

            Borehole(double H=0.0,
                     double D=0.0,
                     double r_b=0.0,
                     double x=0.0,
                     double y=0.0) : H(H), D(D), r_b(r_b), x(x), y(y) {
            }

            double distance(Borehole target);
            std::tuple<double, double> position();
        };

        std::vector<Borehole>boreField(
                const std::vector<std::tuple<double, double> > &coordinates,
                const double &r_b, const double &H, const double &D);

        struct SimilaritiesType {
            ~SimilaritiesType() {} // destructor

            int nSim = 0;
            std::vector< std::vector <std::tuple<int, int> > > Sim;
            std::vector<std::tuple<double, double> > HSim;
            std::vector<std::tuple<double, double> > DSim;
            std::vector<double> disSim;

            SimilaritiesType() {} // constructor
        };

        struct Similarity {
            ~Similarity() {} // destructor

            // delcare variables
            // positive similarities
            std::vector<int> nSimPos;  // number of positive similarities
            std::vector<std::vector<std::tuple<int, int> > > simPos;  // number of sim positions
            std::vector<double> disSimPos;  // the distances between each position
            std::vector<std::tuple<int, int> > HSimPos;  // the heights of each borehole
            std::vector<std::tuple<int, int> > DSimPos;  // the burial depth of each borehole
            // negative similarities
            std::vector<int> nSimNeg;
            std::vector<std::vector<std::tuple<int, int> > > simNeg;
            std::vector<double> disSimNeg;
            std::vector<std::tuple<int, int> > HSimNeg;
            std::vector<std::tuple<int, int> > DSimNeg;

            Similarity() {} // constructor


            void similarities(SimilaritiesType &SimReal, SimilaritiesType &SimImage,
                              std::vector<gt::boreholes::Borehole> &boreSegments,
                              bool splitRealAndImage = true, double disTol = 0.1, double tol = 1.0e-6);

            void _similarities_group_by_distance(std::vector<gt::boreholes::Borehole> &boreSegments,
                                                 std::vector<std::vector<std::tuple<int, int> > > &Pairs,
                                                 std::vector<int> &nPairs, std::vector<double> &disPairs, int &nDis,
                                                 double disTol = 0.1);

            void _similarities_one_distance(SimilaritiesType &SimT, std::vector<std::tuple<int, int> > &pairs,
                                            std::vector<gt::boreholes::Borehole> &boreSegments, const std::string &kind,
                                            double tol = 1.0e-6);
        };

    } // boreholes name space

} // gt namespace

#endif //CPPGFUNCTION_BOREHOLES_H
