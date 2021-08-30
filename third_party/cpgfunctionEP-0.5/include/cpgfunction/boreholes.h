//
// Created by jackcook on 7/11/20.
//

#include <iostream>
#include <cmath>
#include <tuple>
#include <vector>

using namespace std;

#ifndef CPPGFUNCTION_BOREHOLES_H
#define CPPGFUNCTION_BOREHOLES_H

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
            tuple<double, double> position();
        };

        vector<Borehole>boreField(
                const vector<tuple<double, double> > &coordinates,
                const double &r_b, const double &H, const double &D);

        struct SimilaritiesType {
            ~SimilaritiesType() {} // destructor

            int nSim = 0;
            vector< vector <tuple<int, int> > > Sim;
            vector<tuple<double, double> > HSim;
            vector<tuple<double, double> > DSim;
            vector<double> disSim;

            SimilaritiesType() {} // constructor
        };

        struct Similarity {
            ~Similarity() {} // destructor

            // delcare variables
            // positive similarities
            vector<int> nSimPos;  // number of positive similarities
            vector<vector<tuple<int, int> > > simPos;  // number of sim positions
            vector<double> disSimPos;  // the distances between each position
            vector<tuple<int, int> > HSimPos;  // the heights of each borehole
            vector<tuple<int, int> > DSimPos;  // the burial depth of each borehole
            // negative similarities
            vector<int> nSimNeg;
            vector<vector<tuple<int, int> > > simNeg;
            vector<double> disSimNeg;
            vector<tuple<int, int> > HSimNeg;
            vector<tuple<int, int> > DSimNeg;

            Similarity() {} // constructor


            void similarities(SimilaritiesType &SimReal, SimilaritiesType &SimImage,
                              vector<gt::boreholes::Borehole> &boreSegments,
                              bool splitRealAndImage = true, double disTol = 0.1, double tol = 1.0e-6);

            void _similarities_group_by_distance(vector<gt::boreholes::Borehole> &boreSegments,
                                                 vector<vector<tuple<int, int> > > &Pairs,
                                                 vector<int> &nPairs, vector<double> &disPairs, int &nDis,
                                                 double disTol = 0.1);

            void _similarities_one_distance(SimilaritiesType &SimT, vector<tuple<int, int> > &pairs,
                                            vector<gt::boreholes::Borehole> &boreSegments, const string &kind,
                                            double tol = 1.0e-6);
        };

    } // boreholes name space

} // gt namespace

#endif //CPPGFUNCTION_BOREHOLES_H
