//
// Created by jackcook on 7/15/20.
//

#include <cpgfunction/interpolation.h>
#include <stdexcept>

using namespace std;

namespace jcc::interpolation {

    double linterp(double xp, double x0, double y0, double x1, double y1) {
        double yp;
        yp = y0 + ((y1-y0) / (x1-x0)) * (xp-x0);
        return yp;
    } // linterp

    void interp1d(vector<double>& xp, vector<double>& yp, vector<double>& x,
                  vector<double>& y) {
        int counter = 0;
        for (int i=0; i<yp.size(); i++) {
            if (xp[i] < x[0] || xp[i] > x[x.size()-1]) {
                throw invalid_argument("Need to add extrapolation");
            }
            for (int j = counter; j<y.size();j++) {
                if (xp[i] - x[j] < 10) {
                    yp[i] = y[j];
                    break;
                } else if (xp[i] >= x[j] && xp[i] <= x[j+1]) {
                    yp[i] = linterp(xp[i], x[j], y[j], x[j + 1], y[j + 1]);
                    break;
                } else {
                    counter++;
                } // fi
            } // next j
        } // next i
    } // interp1d

    double interp1d(double &xp, vector<double>& x, vector<double>& y) {
        // this function takes in an x point, an x-vector and a y-vector
        // a linear interpolation occurs over the y-vector
        int counter = 0;
        double yp = 0;

        if ( xp < x[0] ) {
            yp = y[0];  // return the first member of the list
            return yp;
        } else if ( xp > x[x.size()-1] ) {
            yp = y[x.size()-1];  // return the last member of the list
            return yp;
        }

        for (int j = counter; j<y.size();j++) {
            if (xp - x[j] < 1e-07) {
                yp = y[j];
                return yp;
            } else if (xp >= x[j] && xp <= x[j+1]) {
                yp = linterp(xp, x[j], y[j], x[j + 1], y[j + 1]);
                return yp;
            } else {
                counter++;
            } // fi
        } // next j
        return yp;  // this function should never reach this point
    } // interp1d

    void interp1d(double &xp, double &yp, vector<double> &time,
                  gt::segments::SegmentResponse &SegRes,
                  int &i, int &j, int &k) {
        // if the x point is out of bounds, then tell the user that
        // extrapolation is not possible
        if (xp < 0 || xp > time[time.size()-1]) {
            throw invalid_argument("Need to add extrapolation");
        }
        // if the time value falls in between 0 and the first time value,
        // then interpolate 0 to t1
        if (0 < xp && xp < time[0]) {
            double h;
            SegRes.get_h_value(h, i, j, 0);  // this must pull k == 0
            yp = linterp(xp, 0., 0., time[0], h);
            return;
        }
        // loop until the value for interpolation is found
        int counter=0;
        double h1;
        SegRes.get_h_value(h1, i, j, 0);
        double h2;
        SegRes.get_h_value(h2, i, j, 1);
        for (int k=counter; k<time.size()-1; k++) {
            if (xp>=time[k] && xp <=time[k+1]) {
                yp = linterp(xp, time[k], h1, time[k+1], h2);
                return;
            } else {
                counter++;
                h1 = h2;
                SegRes.get_h_value(h2, i, j, counter+1);
            }  // else()
        }  // next k
    }  // interp1d();

} // jcc::interpolation


